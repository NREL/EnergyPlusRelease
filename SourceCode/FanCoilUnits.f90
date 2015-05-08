MODULE FanCoilUnits

  ! Module containing the routines dealing with 2 and 4 pipe fan coil units

  ! MODULE INFORMATION:
  !       AUTHOR         Fred Buhl
  !       DATE WRITTEN   March 2000
  !       MODIFIED       October 2003 (FSEC added cooling coil type)
  !                      June 2010    Arnaud Flament LBNL added 3-speed and variables-speed fan capacity control;
  !                                   outside air schedule; and removed coil water inlet node inputs
  !                      Sept 2010    Brent Griffith, plant upgrades for water coils, fluid properties
  !       RE-ENGINEERED  na

  ! PURPOSE OF THIS MODULE:
  ! To encapsulate the data and algorithms needed to simulate 2 and 4 pipe
  ! fan coil units.

  ! METHODOLOGY EMPLOYED:
  ! Units are modeled as a collection of components: outside air mixer,
  ! fan, heating coil and/or cooling coil plus an integrated control
  ! algorithm that adjusts the hot or cold water flow to meet the zone
  ! load. Or varies the air flow rate to meet the zone load. Or both.

  ! REFERENCES: none

  ! OTHER NOTES: none

  ! USE STATEMENTS:
  ! Use statements for data only modules
USE DataPrecisionGlobals
USE DataLoopNode
USE DataSizing
USE DataGlobals,     ONLY: BeginEnvrnFlag, BeginDayFlag, MaxNameLength, SecInHour, &
                           InitConvTemp, SysSizingCalc, DisplayExtraWarnings
USE DataInterfaces
Use DataEnvironment, ONLY: OutBaroPress, OutDryBulbTemp, OutRelHum, StdBaroPress, StdRhoAir
USE DataHVACGlobals, ONLY: SmallMassFlow, SmallLoad, FanElecPower, SmallAirVolFlow, SingleCoolingSetPoint, &
                           SingleHeatingSetPoint,cFanTypes, ContFanCycCoil, ATMixer_InletSide, &
                           ATMixer_SupplySide, cATMixerTypes, ATMixerExists

  ! Use statements for access to subroutines in other modules
USE ScheduleManager

IMPLICIT NONE         ! Enforce explicit typing of all variables

PRIVATE ! Everything private unless explicitly made public

  ! MODULE PARAMETER DEFINITIONS

CHARACTER(len=*), PARAMETER :: cMO_FanCoil='ZoneHVAC:FourPipeFanCoil'

! coil operation
INTEGER, PARAMETER :: On =  1              ! normal coil operation
INTEGER, PARAMETER :: Off = 0              ! signal coil shouldn't run

! coil type units supported in this module
INTEGER, PARAMETER :: FanCoilUnit_4Pipe = 1

INTEGER, PARAMETER :: CCoil_Water=1
INTEGER, PARAMETER :: CCoil_Detailed=2
INTEGER, PARAMETER :: CCoil_HXAssist=3

INTEGER, PARAMETER :: HCoil_Water=1

!capacity control method supported in this module
INTEGER, PARAMETER :: CCM_ConsFanVarFlow=1
INTEGER, PARAMETER :: CCM_CycFan=2
INTEGER, PARAMETER :: CCM_VarFanVarFlow=3
INTEGER, PARAMETER :: CCM_VarFanConsFlow=4

  ! DERIVED TYPE DEFINITIONS
TYPE FanCoilData
  ! Input data
  CHARACTER(len=MaxNameLength) :: Name         =' '  ! name of unit
  CHARACTER(len=MaxNameLength) :: UnitType     =' '  ! type of unit
  INTEGER                      :: UnitType_Num = 0
  CHARACTER(len=MaxNameLength) :: Sched        =' '  ! availability schedule
  INTEGER                      :: SchedPtr     =0    ! index to schedule
  CHARACTER(len=MaxNameLength) :: SchedOutAir  =' '  ! outside air schedule, multipliy maximum outdoor air flow rate
  INTEGER                      :: SchedOutAirPtr= 0  ! index to outside air schedule
  INTEGER                      :: FanType_Num  =0    ! index to fan type
  CHARACTER(len=MaxNameLength) :: CapCtrlMeth  =' '  ! type of capacity control method
                                                     ! 'ConstantFanVariableFlow' or
                                                     ! 'CyclingFan' or
                                                     ! 'VariableFanVariableFlow'
  INTEGER                      :: SpeedFanSel = 0    ! Speed fan selected
  INTEGER                      :: CapCtrlMeth_Num = 0
  REAL (r64)                   :: PLR = 0.0d0            ! Part Load Ratio, fraction of time step fancoil is on
  INTEGER                      :: MaxIterIndexH = 0  ! Maximum iterations exceeded for heating
  INTEGER                      :: MaxIterIndexC = 0  ! Maximum iterations exceeded for cooling
  REAL(r64)                    :: FanAirVolFlow=0.0d0  ! m3/s
  REAL(r64)                    :: MaxAirVolFlow=0.0d0  ! m3/s
  REAL(r64)                    :: MaxAirMassFlow=0.0d0 ! kg/s
  REAL(r64)                    :: LowSpeedRatio=0.0d0  ! Low speed fan supply air flow ratio
  REAL(r64)                    :: MedSpeedRatio=0.0d0  ! Medium speed fan supply air flow ratio
  REAL (r64)                   :: SpeedFanRatSel=0.0d0 ! Speed fan ratio determined by fan speed selection at each timestep
  REAL(r64)                    :: OutAirVolFlow =0.0d0 ! m3/s
  REAL(r64)                    :: OutAirMassFlow=0.0d0 ! kg/s
  INTEGER                      :: AirInNode     =0   ! inlet air node number
  INTEGER                      :: AirOutNode    =0   ! outlet air node number
  INTEGER                      :: OutsideAirNode=0   ! outside air node number
  INTEGER                      :: AirReliefNode =0   ! relief air node number
  INTEGER                      :: MixedAirNode  =0   ! Mixed Air Node number
  INTEGER                      :: ColdControlNode=0  ! chilled water control node
  INTEGER                      :: ColdPlantOutletNode=0 ! chilled water coil outlet plant node
  INTEGER                      :: HotControlNode =0  ! hot water control node
  INTEGER                      :: HotPlantOutletNode=0 ! hot water coil outlet plant node
  CHARACTER(len=MaxNameLength) :: OAMixName    =' '  ! name of outside air mixer
  CHARACTER(len=MaxNameLength) :: OAMixType    =' '  ! type of outside air mixer
  INTEGER                      :: OAMixIndex   = 0
  CHARACTER(len=MaxNameLength) :: FanName      =' '  ! name of fan
  CHARACTER(len=MaxNameLength) :: FanType      =' '  ! type of fan
  INTEGER                      :: FanIndex     =0    ! index for fan
  CHARACTER(len=MaxNameLength) :: CCoilName    =' '  ! name of cooling coil
  INTEGER                      :: CCoilName_Index= 0 ! Index for this Cooling Coil in SimWaterComp
  CHARACTER(len=MaxNameLength) :: CCoilType    =' '  ! type of cooling coil:
                                                     ! 'Coil:Cooling:Water' or
                                                     ! 'Coil:Cooling:Water:DetailedGeometry' or
                                                     ! 'CoilSystem:Cooling:Water:HeatExchangerAssisted'
  INTEGER                      :: CCoilType_Num = 0  ! Numeric equivalent for type of cooling coil
  CHARACTER(len=MaxNameLength) :: CCoilPlantName =' '  ! name of cooling coil (child<=CoilSystem:Cooling:Water:HeatExchangerAssisted)
  CHARACTER(len=MaxNameLength) :: CCoilPlantType =' '  ! type of cooling coil (child<=CoilSystem:Cooling:Water:HeatExchangerAssisted)
  INTEGER                      :: CCoilPlantTypeOfNum = 0
  INTEGER                      :: CWLoopNum           =0   ! index for plant loop with chilled water coil
  INTEGER                      :: CWLoopSide          =0   ! index for plant loop side for chilled water coil
  INTEGER                      :: CWBranchNum         =0   ! index for plant branch for chilled water coil
  INTEGER                      :: CWCompNum           =0   ! index for plant component for chilled water coil
  INTEGER                      :: ControlCompTypeNum  = 0
  INTEGER                      :: CompErrIndex        = 0
  REAL(r64)                    :: MaxColdWaterVolFlow =0.0d0 ! m3/s
  REAL(r64)                    :: MaxColdWaterFlow    =0.0d0 ! kg/s
  REAL(r64)                    :: MinColdWaterVolFlow =0.0d0 ! m3/s
  REAL(r64)                    :: MinColdWaterFlow    =0.0d0 ! kg/s
  REAL(r64)                    :: ColdControlOffset   =0.0d0 ! control tolerance
  CHARACTER(len=MaxNameLength) :: HCoilName           =' ' ! name of heating coil
  INTEGER                      :: HCoilName_Index = 0
  CHARACTER(len=MaxNameLength) :: HCoilType    =' '  ! type of heating coil:
                                                     ! 'Coil:Heating:Water' or
  INTEGER                      :: HCoilType_Num = 0  ! Numeric equivalent for type of cooling coil
  INTEGER                      :: HCoilPlantTypeOfNum =0
  INTEGER                      :: HWLoopNum           =0   ! index for plant loop with hot water coil
  INTEGER                      :: HWLoopSide          =0   ! index for plant loop side for hot water coil
  INTEGER                      :: HWBranchNum         =0   ! index for plant branch for hot water coil
  INTEGER                      :: HWCompNum           =0   ! index for plant component for hot water coil
  REAL(r64)                    :: MaxHotWaterVolFlow  =0.0d0 ! m3/s
  REAL(r64)                    :: MaxHotWaterFlow     =0.0d0 ! kg/s
  REAL(r64)                    :: MinHotWaterVolFlow  =0.0d0 ! m3/s
  REAL(r64)                    :: MinHotWaterFlow     =0.0d0 ! kg/s
  REAL(r64)                    :: HotControlOffset    =0.0d0 ! control tolerance
  INTEGER                      :: AvailStatus         =0
  CHARACTER(len=MaxNameLength) :: AvailManagerListName = ' ' ! Name of an availability manager list object
  ! addition for OA to Zone Units
  LOGICAL                      :: ATMixerExists       = .FALSE. ! True if there is an ATMixer
  CHARACTER(len=MaxNameLength) :: ATMixerName         =' ' ! name of air terminal mixer
  INTEGER                      :: ATMixerIndex        =0   ! index to the air terminal mixer
  INTEGER                      :: ATMixerType         =0   ! 1 = inlet side mixer, 2 = supply side mixer
  INTEGER                      :: ATMixerPriNode      =0   ! primary inlet air node number for the air terminal mixer
  INTEGER                      :: ATMixerSecNode      =0   ! secondary air inlet node number for the air terminal mixer
  INTEGER                      :: ATMixerOutNode      =0   ! outlet air node number for the air terminal mixer
  ! Report data
  REAL(r64)                    :: HeatPower           =0.0d0 ! unit heating output in watts
  REAL(r64)                    :: HeatEnergy          =0.0d0 ! unit heating output in J
  REAL(r64)                    :: TotCoolPower        =0.0d0 ! unit total cooling power output in watts
  REAL(r64)                    :: TotCoolEnergy       =0.0d0 ! unit total cooling energy output in joules
  REAL(r64)                    :: SensCoolPower       =0.0d0 ! unit sensible cooling power output in watts
  REAL(r64)                    :: SensCoolEnergy      =0.0d0 ! unit sensible cooling energy output in joules
  REAL(r64)                    :: ElecPower           =0.0d0 ! unit electric power consumption in watts
  REAL(r64)                    :: ElecEnergy          =0.0d0 ! unit electiric energy consumption in joules
  REAL(r64)                    :: DesCoolingLoad      =0.0d0 ! used for reporting in watts
  REAL(r64)                    :: DesHeatingLoad      =0.0d0 ! used for reporting in watts
END TYPE FanCoilData

  ! MODULE VARIABLE DECLARATIONS:
TYPE (FanCoilData), ALLOCATABLE, DIMENSION(:)         :: FanCoil

INTEGER                                         :: NumFanCoils=0
INTEGER                                         :: Num4PipeFanCoils=0
LOGICAL, ALLOCATABLE, DIMENSION(:) :: MySizeFlag
LOGICAL, ALLOCATABLE, DIMENSION(:) :: CheckEquipName
LOGICAL,SAVE                       :: GetFanCoilInputFlag = .true.  ! First time, input is "gotten"

  ! SUBROUTINE SPECIFICATIONS FOR MODULE

PUBLIC  SimFanCoilUnit
PRIVATE GetFanCoilUnits
PRIVATE InitFanCoilUnits
PRIVATE SizeFanCoilUnit
PRIVATE Sim4PipeFanCoil
PUBLIC  Calc4PipeFanCoil
PRIVATE ReportFanCoilUnit
! look up functions for node numbers
PUBLIC  GetFanCoilOutAirNode
PUBLIC  GetFanCoilReturnAirNode
PUBLIC  GetFanCoilMixedAirNode
PUBLIC  GetFanCoilZoneInletAirNode
PUBLIC  GetFanCoilInletAirNode
PUBLIC  GetFanCoilIndex

CONTAINS

SUBROUTINE SimFanCoilUnit(CompName,ZoneNum,ControlledZoneNum,FirstHVACIteration,PowerMet,LatOutputProvided,CompIndex)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   March 2000
          !       MODIFIED       Don Shirey, Aug 2009 (LatOutputProvided)
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Manages the simulation of a fan coil unit. Called from SimZone Equipment

          ! METHODOLOGY EMPLOYED:
          ! NA

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor,    ONLY: FindItemInList
  USE General,           ONLY: TrimSigDigits

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT (IN)  :: CompName            ! name of the fan coil unit
  INTEGER,          INTENT (IN)  :: ZoneNum             ! number of zone being served
  INTEGER,          INTENT (IN)  :: ControlledZoneNum   ! index into ZoneEquipConfig array; may not be equal to ZoneNum
  LOGICAL,          INTENT (IN)  :: FirstHVACIteration  ! TRUE if 1st HVAC simulation of system timestep
  REAL(r64),        INTENT (OUT) :: PowerMet            ! Sensible power supplied (W)
  REAL(r64),        INTENT (OUT) :: LatOutputProvided   ! Latent add/removal supplied by window AC (kg/s), dehumid = negative
  INTEGER,          INTENT(INOUT):: CompIndex

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER      :: FanCoilNum             ! index of fan coil unit being simulated

          ! FLOW

! First time SimFanCoilUnit is called, get the input for all the fan coil units
IF (GetFanCoilInputFlag) THEN
  CALL GetFanCoilUnits
  GetFanCoilInputFlag = .FALSE.
END IF

! Find the correct Fan Coil Equipment
IF (CompIndex == 0) THEN
  FanCoilNum = FindItemInList(CompName,FanCoil%Name,NumFanCoils)
  IF (FanCoilNum == 0) THEN
    CALL ShowFatalError('SimFanCoil: Unit not found='//TRIM(CompName))
  ENDIF
  CompIndex=FanCoilNum
ELSE
  FanCoilNum=CompIndex
  IF (FanCoilNum > NumFanCoils .or. FanCoilNum < 1) THEN
    CALL ShowFatalError('SimFanCoil:  Invalid CompIndex passed='//  &
                        TRIM(TrimSigDigits(FanCoilNum))// &
                        ', Number of Units='//TRIM(TrimSigDigits(NumFanCoils))//  &
                        ', Entered Unit name='//TRIM(CompName))
  ENDIF
  IF (CheckEquipName(FanCoilNum)) THEN
    IF (CompName /= FanCoil(FanCoilNum)%Name) THEN
      CALL ShowFatalError('SimFanCoil: Invalid CompIndex passed='//  &
                          TRIM(TrimSigDigits(FanCoilNum))// &
                          ', Unit name='//TRIM(CompName)//', stored Unit Name for that index='//  &
                          TRIM(FanCoil(FanCoilNum)%Name))
    ENDIF
    CheckEquipName(FanCoilNum)=.false.
  ENDIF
ENDIF

ZoneEqFanCoil = .TRUE.

! Initialize the fan coil unit
CALL InitFanCoilUnits(FanCoilNum,ZoneNum)

! Select the correct unit type
SELECT CASE(FanCoil(FanCoilNum)%UnitType_Num)

  CASE (FanCoilUnit_4Pipe)

    CALL Sim4PipeFanCoil(FanCoilNum,ZoneNum,ControlledZoneNum,FirstHVACIteration,PowerMet,LatOutputProvided)

END SELECT

! Report the result of the simulation
CALL ReportFanCoilUnit(FanCoilNum)

ZoneEqFanCoil = .FALSE.

RETURN
END SUBROUTINE SimFanCoilUnit

SUBROUTINE GetFanCoilUnits

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   March 2000
          !       MODIFIED       Bereket Nigusse, FSEC, April 2011: eliminated input node names
          !                                                         added OA Mixer object type
          !                                                         and fan object type
          !                      Chandan Sharma, FSEC, July 2012: Added zone sys avail managers
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Obtains input data for fan coil units and stores it in fan coil data structures

          ! METHODOLOGY EMPLOYED:
          ! Uses "Get" routines to read in data.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor,        ONLY: GetNumObjectsFound, GetObjectItem, VerifyName, SameString, GetObjectDefMaxArgs, FindItemInList
  USE NodeInputManager,      ONLY: GetOnlySingleNode
  USE BranchNodeConnections, ONLY: TestCompSet, SetUpCompSets
  USE Fans,                  ONLY: GetFanDesignVolumeFlowRate, GetFanType
  USE General,               ONLY: TrimSigDigits
  USE DataIPShortCuts
  USE WaterCoils,            ONLY: GetCoilWaterInletNode
  USE HVACHXAssistedCoolingCoil, ONLY: GetHXCoilWaterInletNode=>GetCoilWaterInletNode,GetHXCoilTypeAndName
  USE DataHvacGlobals,       ONLY: FanType_SimpleConstVolume, FanType_SimpleVAV, FanType_SimpleOnOff, ZoneComp
  USE DataPlant,             ONLY: TypeOf_CoilWaterSimpleHeating, TypeOf_CoilWaterDetailedFlatCooling, &
                                   TypeOf_CoilWaterCooling
  USE MixedAir,              ONLY: GetOAMixerIndex, GetOAMixerNodeNumbers
  USE DataZoneEquipment,     ONLY: ZoneEquipConfig, FanCoil4Pipe_Num
  USE DataGlobals,           ONLY: NumOfZones, ScheduleAlwaysOn
  USE SingleDuct,            ONLY: GetATMixer

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), PARAMETER     :: RoutineName='GetFanCoilUnits: ' ! include trailing blank space

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER                         :: FanCoilIndex ! loop index
  INTEGER                         :: FanCoilNum   ! current fan coil number
  INTEGER                         :: NumAlphas    ! Number of Alphas for each GetObjectItem call
  INTEGER                         :: NumNumbers   ! Number of Numbers for each GetObjectItem call
  INTEGER, DIMENSION(4)           :: OANodeNums   ! Node numbers of Outdoor air mixer (OA, EA, RA, MA)
  INTEGER                         :: IOStatus     ! Used in GetObjectItem
  LOGICAL                         :: ErrorsFound=.false.  ! Set to true if errors in input, fatal at end of routine
  LOGICAL                         :: ErrFlag=.false. ! Local error flag for GetOAMixerNodeNums
  LOGICAL                         :: IsNotOK      ! Flag to verify name
  LOGICAL                         :: IsBlank      ! Flag for blank name
  CHARACTER(len=MaxNameLength) :: CurrentModuleObject      ! Object type for getting and error messages
  CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: Alphas         ! Alpha input items for object
  CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: cAlphaFields   ! Alpha field names
  CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: cNumericFields ! Numeric field names
  REAL(r64), ALLOCATABLE, DIMENSION(:) :: Numbers         ! Numeric input items for object
  LOGICAL, ALLOCATABLE, DIMENSION(:)   :: lAlphaBlanks    ! Logical array, alpha field input BLANK = .true.
  LOGICAL, ALLOCATABLE, DIMENSION(:)   :: lNumericBlanks  ! Logical array, numeric field input BLANK = .true.
  INTEGER                              :: TotalArgs=0     ! Total number of alpha and numeric arguments (max) for a
  INTEGER                              :: CtrlZone    ! index to loop counter
  INTEGER                              :: NodeNum     ! index to loop counter
  LOGICAL                              :: ZoneExNodeNotFound = .FALSE. ! used in error checking
  LOGICAL                              :: ZoneInNodeNotFound = .FALSE. ! used in error checking
  INTEGER                              :: ATMixerNum=0         ! index of air terminal mixer in the air terminal mixer data array
  INTEGER                              :: ATMixerType=0        ! type of air terminal mixer (1=inlet side; 2=supply side)
  INTEGER                              :: ATMixerPriNode=0     ! node number of the air terminal mixer primary air inlet
  INTEGER                              :: ATMixerSecNode=0     ! node number of the air terminal mixer secondary air inlet
  INTEGER                              :: ATMixerOutNode=0     ! node number of the air terminal mixer secondary air inlet
  CHARACTER(len=MaxNameLength)         :: ATMixerName


          ! FLOW

! find the number of each type of fan coil unit

CurrentModuleObject = cMO_FanCoil
Num4PipeFanCoils = GetNumObjectsFound(CurrentModuleObject)
NumFanCoils = Num4PipeFanCoils
! allocate the data structures
ALLOCATE(FanCoil(NumFanCoils))
ALLOCATE(CheckEquipName(NumFanCoils))
CheckEquipName=.true.

CALL GetObjectDefMaxArgs(CurrentModuleObject,TotalArgs,NumAlphas,NumNumbers)
ALLOCATE(Alphas(NumAlphas))
Alphas=' '
ALLOCATE(cAlphaFields(NumAlphas))
cAlphaFields=' '
ALLOCATE(cNumericFields(NumNumbers))
cNumericFields=' '
ALLOCATE(Numbers(NumNumbers))
Numbers=0.0d0
ALLOCATE(lAlphaBlanks(NumAlphas))
lAlphaBlanks=.TRUE.
ALLOCATE(lNumericBlanks(NumNumbers))
lNumericBlanks=.TRUE.


! loop over 4 pipe fan coil units; get and load the input data
DO FanCoilIndex = 1,Num4PipeFanCoils

  CALL GetObjectItem(CurrentModuleObject,FanCoilIndex,Alphas,NumAlphas,Numbers,NumNumbers,IOStatus, &
                     NumBlank=lNumericBlanks,AlphaBlank=lAlphaBlanks, &
                     AlphaFieldNames=cAlphaFields,NumericFieldNames=cNumericFields)

  FanCoilNum = FanCoilIndex
  IsNotOK=.FALSE.
  IsBlank=.FALSE.
  CALL VerifyName(Alphas(1),FanCoil%Name,FanCoilNum-1,IsNotOK,IsBlank,TRIM(CurrentModuleObject)//' Name')
  IF (IsNotOK) THEN
    ErrorsFound=.true.
    IF (IsBlank) Alphas(1)='xxxxx'
  ENDIF
  FanCoil(FanCoilNum)%Name = Alphas(1)
  FanCoil(FanCoilNum)%UnitType = TRIM(CurrentModuleObject)
  FanCoil(FanCoilNum)%UnitType_Num = FanCoilUnit_4Pipe
  FanCoil(FanCoilNum)%Sched = Alphas(2)
  IF (lAlphaBlanks(2)) THEN
    FanCoil(FanCoilNum)%SchedPtr = ScheduleAlwaysOn
  ELSE
    FanCoil(FanCoilNum)%SchedPtr = GetScheduleIndex(Alphas(2))  ! convert schedule name to pointer
    IF (FanCoil(FanCoilNum)%SchedPtr .EQ. 0) THEN
      CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//trim(Alphas(1))//'", invalid')
      CALL ShowContinueError('invalid-not found: '//TRIM(cAlphaFields(2))//'="'//TRIM(Alphas(2))//'".')
      ErrorsFound=.TRUE.
    END IF
  END IF


  IF (SameString(Alphas(3),'ConstantFanVariableFlow') .OR. &
      SameString(Alphas(3),'CyclingFan') .OR. &
      SameString(Alphas(3),'VariableFanVariableFlow') .OR. &
      SameString(Alphas(3),'VariableFanConstantFlow') ) THEN
    FanCoil(FanCoilNum)%CapCtrlMeth= Alphas(3)
    IF (SameString(Alphas(3),'ConstantFanVariableFlow'))   &
       FanCoil(FanCoilNum)%CapCtrlMeth_Num=CCM_ConsFanVarFlow
    IF (SameString(Alphas(3),'CyclingFan'))   &
       FanCoil(FanCoilNum)%CapCtrlMeth_Num=CCM_CycFan
    IF (SameString(Alphas(3),'VariableFanVariableFlow'))   &
        FanCoil(FanCoilNum)%CapCtrlMeth_Num=CCM_VarFanVarFlow
    IF (SameString(Alphas(3),'VariableFanConstantFlow'))   &
        FanCoil(FanCoilNum)%CapCtrlMeth_Num=CCM_VarFanConsFlow
  ELSE
    CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//trim(FanCoil(FanCoilNum)%Name)//'", invalid')
    CALL ShowContinueError('illegal value: '//TRIM(cAlphaFields(3))//'="'//TRIM(Alphas(3))//'".')
    ErrorsFound=.TRUE.
  END IF

  FanCoil(FanCoilNum)%SchedOutAir = Alphas(4)
  FanCoil(FanCoilNum)%SchedOutAirPtr = GetScheduleIndex(Alphas(4))  ! convert schedule name to pointer
  IF (FanCoil(FanCoilNum)%SchedOutAirPtr .EQ. 0 .AND. (.NOT. lAlphaBlanks(4))) THEN
    CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//trim(FanCoil(FanCoilNum)%Name)//'", invalid')
    CALL ShowContinueError('illegal value: '//TRIM(cAlphaFields(4))//'="'//TRIM(Alphas(4))//'".')
    ErrorsFound=.TRUE.
  END IF
  FanCoil(FanCoilNum)%MaxAirVolFlow = Numbers(1)
  FanCoil(FanCoilNum)%LowSpeedRatio = Numbers(2)
  FanCoil(FanCoilNum)%MedSpeedRatio = Numbers(3)
  ! check if low speed ratio < medium speed ratio, if not : warning & set to default values
  IF(FanCoil(FanCoilNum)%LowSpeedRatio .GT. FanCoil(FanCoilNum)%MedSpeedRatio)THEN
    CALL ShowWarningError(RoutineName//trim(CurrentModuleObject)//'="'//trim(FanCoil(FanCoilNum)%Name)//'",')
    CALL ShowContinueError('... '//TRIM(cNumericFields(2))//' is greater than the medium speed supply air flow ratio.')
    CALL ShowContinueError('... Fan Coil Unit low speed supply air flow ratio = '//  &
         TRIM(TrimSigDigits(FanCoil(FanCoilNum)%LowSpeedRatio,5))//' ')
    CALL ShowContinueError('... Fan Coit Unit medium speed supply air flow ratio = '//  &
         TRIM(TrimSigDigits(FanCoil(FanCoilNum)%MedSpeedRatio,5))//' ')
    CALL ShowContinueError('... Fan Coil Unit low speed supply air flow ratio and medium speed '//  &
         'supply air flow ratio set to default values')
    FanCoil(FanCoilNum)%LowSpeedRatio = 1.d0/3.d0
    FanCoil(FanCoilNum)%MedSpeedRatio = 2.d0/3.d0
  END IF

  FanCoil(FanCoilNum)%OutAirVolFlow = Numbers(4)

  FanCoil(FanCoilNum)%AirInNode = &    ! air input node
               GetOnlySingleNode(Alphas(5),ErrorsFound,FanCoil(FanCoilNum)%UnitType,Alphas(1),NodeType_Air, &
                                 NodeConnectionType_Inlet, 1, ObjectIsParent)

  FanCoil(FanCoilNum)%AirOutNode = &   ! air outlet node
               GetOnlySingleNode(Alphas(6),ErrorsFound,FanCoil(FanCoilNum)%UnitType,Alphas(1),NodeType_Air, &
                                 NodeConnectionType_Outlet, 1, ObjectIsParent)

  FanCoil(FanCoilNum)%OAMixType = Alphas(7)
  FanCoil(FanCoilNum)%OAMixName = Alphas(8)
  ! check to see if local OA mixer specified
  IF (.NOT. lAlphaBlanks(8)) THEN
    ErrFlag = .false.
    CALL ValidateComponent(FanCoil(FanCoilNum)%OAMixType,FanCoil(FanCoilNum)%OAMixName,ErrFlag,TRIM(CurrentModuleObject))
    IF (ErrFlag) THEN
       CALL ShowContinueError('specified in '//TRIM(CurrentModuleObject)//' = "'//TRIM(FanCoil(FanCoilNum)%Name)//'".')
       ErrorsFound = .TRUE.
    ELSE
      ! Get outdoor air mixer node numbers
      OANodeNums = GetOAMixerNodeNumbers(FanCoil(FanCoilNum)%OAMixName, ErrFlag)
      IF (ErrFlag) THEN
         CALL ShowContinueError('that was specified in '//TRIM(CurrentModuleObject)//' = '//TRIM(FanCoil(FanCoilNum)%Name))
         CALL ShowContinueError('..OutdoorAir:Mixer is required. Enter an OutdoorAir:Mixer object with this name.')
         ErrorsFound=.true.
      ELSE
         FanCoil(FanCoilNum)%OutsideAirNode = OANodeNums(1)
         FanCoil(FanCoilNum)%AirReliefNode = OANodeNums(2)
         FanCoil(FanCoilNum)%MixedAirNode = OANodeNums(4)
      ENDIF
    ENDIF
  ENDIF

  FanCoil(FanCoilNum)%CCoilName = Alphas(12)
  FanCoil(FanCoilNum)%MaxColdWaterVolFlow = Numbers(5)
  FanCoil(FanCoilNum)%MinColdWaterVolFlow = Numbers(6)
  FanCoil(FanCoilNum)%ColdControlOffset = Numbers(7)
  FanCoil(FanCoilNum)%HCoilName = Alphas(14)
  FanCoil(FanCoilNum)%HCoilType = Alphas(13)
  FanCoil(FanCoilNum)%MaxHotWaterVolFlow = Numbers(8)
  FanCoil(FanCoilNum)%MinHotWaterVolFlow = Numbers(9)
  FanCoil(FanCoilNum)%HotControlOffset = Numbers(10)

  IF (SameString(Alphas(11),'Coil:Cooling:Water') .OR. &
      SameString(Alphas(11),'Coil:Cooling:Water:DetailedGeometry') .OR. &
      SameString(Alphas(11),'CoilSystem:Cooling:Water:HeatExchangerAssisted') ) THEN
    FanCoil(FanCoilNum)%CCoilType = Alphas(11)
    IF (SameString(Alphas(11),'Coil:Cooling:Water')) THEN
      FanCoil(FanCoilNum)%CCoilType_Num=CCoil_Water
      FanCoil(FanCoilNum)%CCoilPlantName=FanCoil(FanCoilNum)%CCoilName
      FanCoil(FanCoilNum)%CCoilPlantTypeOfNum=TypeOf_CoilWaterCooling
    ENDIF
    IF (SameString(Alphas(11),'Coil:Cooling:Water:DetailedGeometry')) THEN
      FanCoil(FanCoilNum)%CCoilType_Num=CCoil_Detailed
      FanCoil(FanCoilNum)%CCoilPlantName=FanCoil(FanCoilNum)%CCoilName
      FanCoil(FanCoilNum)%CCoilPlantTypeOfNum=TypeOf_CoilWaterDetailedFlatCooling
    ENDIF
    IF (SameString(Alphas(11),'CoilSystem:Cooling:Water:HeatExchangerAssisted')) THEN
      FanCoil(FanCoilNum)%CCoilType_Num=CCoil_HXAssist
      CALL GetHXCoilTypeAndName(FanCoil(FanCoilNum)%CCoilType,FanCoil(FanCoilNum)%CCoilName,ErrorsFound,  &
         FanCoil(FanCoilNum)%CCoilPlantType,FanCoil(FanCoilNum)%CCoilPlantName)
      IF (SameString(FanCoil(FanCoilNum)%CCoilPlantType,'Coil:Cooling:Water')) THEN
        FanCoil(FanCoilNum)%CCoilPlantTypeOfNum=TypeOf_CoilWaterCooling
      ELSEIF (SameString(FanCoil(FanCoilNum)%CCoilPlantType,'Coil:Cooling:Water:DetailedGeometry')) THEN
        FanCoil(FanCoilNum)%CCoilPlantTypeOfNum=TypeOf_CoilWaterDetailedFlatCooling
      ELSE
        CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//trim(FanCoil(FanCoilNum)%Name)//'", invalid')
        CALL ShowContinueError('For: '//TRIM(cAlphaFields(11))//'="'//TRIM(Alphas(11))//'".')
        CALL ShowContinueError('Invalid Coil Type='//trim(FanCoil(FanCoilNum)%CCoilPlantType)//  &
           ', Name='//trim(FanCoil(FanCoilNum)%CCoilPlantName))
        CALL ShowContinueError('must be "Coil:Cooling:Water" or "Coil:Cooling:Water:DetailedGeometry"')
        ErrorsFound=.true.
      ENDIF
    ENDIF
    IsNotOK=.FALSE.
    CALL ValidateComponent(FanCoil(FanCoilNum)%CCoilType,FanCoil(FanCoilNum)%CCoilName,IsNotOK,FanCoil(FanCoilNum)%UnitType)
    IF (IsNotOK) THEN
      CALL ShowContinueError('...specified in '//TRIM(CurrentModuleObject)//'="'// &
                             TRIM(FanCoil(FanCoilNum)%Name)//'".')
      ErrorsFound=.true.
    ELSE
      IF (FanCoil(FanCoilNum)%CCoilType_Num /= CCoil_HXAssist) THEN
        ! mine the cold water node from the coil object
        FanCoil(FanCoilNum)%ColdControlNode = GetCoilWaterInletNode(FanCoil(FanCoilNum)%CCoilType,  &
           FanCoil(FanCoilNum)%CCoilName,IsNotOK)
      ELSE
        FanCoil(FanCoilNum)%ColdControlNode = GetHXCoilWaterInletNode(FanCoil(FanCoilNum)%CCoilType,  &
           FanCoil(FanCoilNum)%CCoilName,IsNotOK)
      ENDIF
      ! Other error checks should trap before it gets to this point in the code, but including just in case.
      IF (IsNotOK) THEN
        CALL ShowContinueError('...specified in '//TRIM(CurrentModuleObject)//'="'//TRIM(FanCoil(FanCoilNum)%Name)//'".')
        ErrorsFound=.true.
      ENDIF
    ENDIF
  ELSE
    CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//trim(FanCoil(FanCoilNum)%Name)//'", invalid')
    CALL ShowContinueError('illegal value: '//TRIM(cAlphaFields(11))//'="'//TRIM(Alphas(11))//'".')
    ErrorsFound=.TRUE.
  END IF

  IF (SameString(Alphas(13),'Coil:Heating:Water')) THEN
    FanCoil(FanCoilNum)%HCoilType_Num=HCoil_Water
    FanCoil(FanCoilNum)%HCoilPlantTypeOfNum=TypeOf_CoilWaterSimpleHeating
    IsNotOK=.FALSE.
    CALL ValidateComponent(FanCoil(FanCoilNum)%HCoilType,FanCoil(FanCoilNum)%HCoilName,IsNotOK,TRIM(CurrentModuleObject))
    IF (IsNotOK) THEN
      CALL ShowContinueError('...specified in '//TRIM(CurrentModuleObject)//'="'//TRIM(FanCoil(FanCoilNum)%Name)//'".')
      ErrorsFound = .TRUE.
    ELSE
     ! mine the hot water node from the coil object
       FanCoil(FanCoilNum)%HotControlNode = GetCoilWaterInletNode(FanCoil(FanCoilNum)%HCoilType, &
                                            FanCoil(FanCoilNum)%HCoilName,IsNotOK)
       IF (IsNotOK) THEN
          CALL ShowContinueError('...specified in '//TRIM(CurrentModuleObject)//'="'// &
                                 TRIM(FanCoil(FanCoilNum)%Name)//'".')
          ErrorsFound=.true.
       ENDIF
    ENDIF
  ELSE
    CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//trim(FanCoil(FanCoilNum)%Name)//'", invalid')
    CALL ShowContinueError('illegal value: '//TRIM(cAlphaFields(13))//'="'//TRIM(Alphas(13))//'".')
    ErrorsFound=.TRUE.
  END IF

  FanCoil(FanCoilNum)%FanType = Alphas(9)
  FanCoil(FanCoilNum)%FanName = Alphas(10)

  IF (.NOT. lAlphaBlanks(15)) THEN
    FanCoil(FanCoilNum)%AvailManagerListName = Alphas(15)
    ZoneComp(FanCoil4Pipe_Num)%ZoneCompAvailMgrs(FanCoilNum)%AvailManagerListName  = Alphas(15)
  ENDIF

  ErrFlag = .FALSE.
  CALL ValidateComponent(FanCoil(FanCoilNum)%FanType,FanCoil(FanCoilNum)%FanName,ErrFlag,TRIM(CurrentModuleObject))
  IF (ErrFlag) THEN
    CALL ShowContinueError('specified in '//TRIM(CurrentModuleObject)//' = "'//TRIM(FanCoil(FanCoilNum)%Name)//'".')
    ErrorsFound=.TRUE.
  ELSE
    CALL GetFanType(FanCoil(FanCoilNum)%FanName,FanCoil(FanCoilNum)%FanType_Num, &
                    ErrFlag,CurrentModuleObject,FanCoil(FanCoilNum)%Name)
    SELECT CASE (FanCoil(FanCoilNum)%FanType_Num)
      CASE (FanType_SimpleConstVolume, FanType_SimpleVAV, FanType_SimpleOnOff)
          ! Get fan air volume flow rate
        FanCoil(FanCoilNum)%FanAirVolFlow = GetFanDesignVolumeFlowRate(FanCoil(FanCoilNum)%FanType, &
                                                                       FanCoil(FanCoilNum)%FanName,IsNotOK)
        ! Check that the fan volumetric flow rate is greater than or equal to the FCU volumetric flow rate
        IF(FanCoil(FanCoilNum)%MaxAirVolFlow .GT. FanCoil(FanCoilNum)%FanAirVolFlow .AND. &
           FanCoil(FanCoilNum)%FanAirVolFlow .NE. AutoSize)THEN
          CALL ShowWarningError(RoutineName//TRIM(FanCoil(FanCoilNum)%UnitType)//': '//TRIM(FanCoil(FanCoilNum)%Name))
          CALL ShowContinueError('... '//TRIM(cNumericFields(1))//' is greater than the maximum fan flow rate.')
          CALL ShowContinueError('... Fan Coil Unit flow = '//TRIM(TrimSigDigits(FanCoil(FanCoilNum)%MaxAirVolFlow,5))//' m3/s.')
          CALL ShowContinueError('... Fan = '//TRIM(cFanTypes(FanCoil(FanCoilNum)%FanType_Num))//': '//  &
             TRIM(FanCoil(FanCoilNum)%FanName))
          CALL ShowContinueError('... Fan flow = '//TRIM(TrimSigDigits(FanCoil(FanCoilNum)%FanAirVolFlow,5))//' m3/s.')
          CALL ShowContinueError('... Fan Coil Unit flow rate reduced to match the fan flow rate and the simulation continues.')
          FanCoil(FanCoilNum)%MaxAirVolFlow = FanCoil(FanCoilNum)%FanAirVolFlow
        END IF

        ! Check that the fan type match with the capacity control method selected
        IF((FanCoil(FanCoilNum)%CapCtrlMeth_Num == CCM_ConsFanVarFlow                                &
             .and. (FanCoil(FanCoilNum)%FanType_Num==FanType_SimpleVAV))                             &
            .OR.(FanCoil(FanCoilNum)%CapCtrlMeth_Num == CCM_CycFan                                   &
                 .and. FanCoil(FanCoilNum)%FanType_Num.ne.FanType_SimpleOnOff)                       &
            .OR. (FanCoil(FanCoilNum)%CapCtrlMeth_Num == CCM_VarFanVarFlow                           &
                  .and. FanCoil(FanCoilNum)%FanType_Num.ne.FanType_SimpleVAV)                        &
            .OR. (FanCoil(FanCoilNum)%CapCtrlMeth_Num == CCM_VarFanConsFlow                          &
                   .and. FanCoil(FanCoilNum)%FanType_Num.ne.FanType_SimpleVAV)) THEN
          CALL ShowSevereError(RoutineName//TRIM(FanCoil(FanCoilNum)%UnitType)//': '//TRIM(FanCoil(FanCoilNum)%Name))
          CALL ShowContinueError('...the fan type of the object : '//TRIM(FanCoil(FanCoilNum)%FanName)//  &
              ' does not match with the capacity control method selected : '//  &
              TRIM(FanCoil(FanCoilNum)%CapCtrlMeth)//' please see I/O reference')
          CALL ShowContinueError('...for ConstantFanVariableFlow a Fan:OnOff or Fan:ConstantVolume is valid.')
          CALL ShowContinueError('...for CyclingFan a Fan:OnOff is valid.')
          CALL ShowContinueError('...for VariableFanVariableFlow or VariableFanConstantFlow a Fan:VariableVolume is valid.')
          ErrorsFound=.TRUE.
        END IF

      CASE DEFAULT
        CALL ShowSevereError(TRIM(CurrentModuleObject)//' = "'//TRIM(Alphas(1))//'"')
        CALL ShowContinueError('Fan Type must be Fan:OnOff, Fan:ConstantVolume or Fan:VariableVolume.')
        ErrorsFound=.TRUE.
    END SELECT
  ENDIF
  ! Set defaults for convergence tolerance
  IF (FanCoil(FanCoilNum)%ColdControlOffset .LE. 0.0d0) THEN
    FanCoil(FanCoilNum)%ColdControlOffset = 0.001d0
  END IF
  IF (FanCoil(FanCoilNum)%HotControlOffset .LE. 0.0d0) THEN
    FanCoil(FanCoilNum)%HotControlOffset = 0.001d0
  END IF

  !check for inlet side air mixer
  CALL GetATMixer(FanCoil(FanCoilNum)%Name,ATMixerName,ATMixerNum,ATMixerType,ATMixerPriNode,AtmixerSecNode,ATMixerOutNode)
  IF (ATMixerType == ATMixer_InletSide) THEN
    ! save the air terminal mixer data in the fan coil data array
    FanCoil(FanCoilNum)%ATMixerExists = .TRUE.
    FanCoil(FanCoilNum)%ATMixerIndex = ATMixerNum
    FanCoil(FanCoilNum)%ATMixerName = ATMixerName
    FanCoil(FanCoilNum)%ATMixerType = ATMixer_InletSide
    FanCoil(FanCoilNum)%ATMixerPriNode = ATMixerPriNode
    FanCoil(FanCoilNum)%ATMixerSecNode = ATMixerSecNode
    FanCoil(FanCoilNum)%ATMixerOutNode = ATMixerOutNode
    ! check that fan coil doesn' have local outside air
    IF (.NOT. lAlphaBlanks(8)) THEN
      CALL ShowSevereError(TRIM(CurrentModuleObject)//' = "'//TRIM(FanCoil(FanCoilNum)%Name)//'".'// &
                           ' Fan coil unit has local as well as central outdoor air specified')
    END IF
    ! check that the air teminal mixer out node is the fan coil inlet node
    IF (FanCoil(FanCoilNum)%AirInNode .NE. ATMixerOutNode) THEN
      CALL ShowSevereError(TRIM(CurrentModuleObject)//' = "'//TRIM(FanCoil(FanCoilNum)%Name)//'".'// &
                    ' Fan coil unit air inlet node name must be the same as an air terminal mixer outlet node name.')
      CALL ShowContinueError('..Air terminal mixer outlet node name is specified in AirTerminal:SingleDuct:InletSideMixer object.')
      CALL ShowContinueError('..Fan coil unit air inlet node name = '//TRIM(NodeID(FanCoil(FanCoilNum)%AirInNode)))
      ErrorsFound=.TRUE.
    END IF
  ! check for supply side air terminal mixer
  ELSE IF (ATMixerType == ATMixer_SupplySide) THEN
    ! save the air terminal mixer data in the fan coil data array
    FanCoil(FanCoilNum)%ATMixerExists = .TRUE.
    FanCoil(FanCoilNum)%ATMixerIndex = ATMixerNum
    FanCoil(FanCoilNum)%ATMixerName = ATMixerName
    FanCoil(FanCoilNum)%ATMixerType = ATMixer_SupplySide
    FanCoil(FanCoilNum)%ATMixerPriNode = ATMixerPriNode
    FanCoil(FanCoilNum)%ATMixerSecNode = ATMixerSecNode
    FanCoil(FanCoilNum)%ATMixerOutNode = ATMixerOutNode
    ! check that fan coil doesn' have local outside air
    IF (.NOT. lAlphaBlanks(8)) THEN
      CALL ShowSevereError(TRIM(CurrentModuleObject)//' = "'//TRIM(FanCoil(FanCoilNum)%Name)//'".'// &
                           ' Fan coil unit has local as well as central outdoor air specified')
    END IF
    ! check that the air teminal mixer secondary air inlet node is the fan coil outlet node
    IF (FanCoil(FanCoilNum)%AirOutNode .NE. ATMixerSecNode) THEN
      CALL ShowSevereError(TRIM(CurrentModuleObject)//' = "'//TRIM(FanCoil(FanCoilNum)%Name)//'".'// &
                   ' Fan coil unit air outlet node name must be the same as the air terminal mixer secondary air inlet node name.')
      CALL ShowContinueError('..Air terminal mixer secondary inlet node name is specified in '// &
        'AirTerminal:SingleDuct:SupplySideMixer object.')
      CALL ShowContinueError('..Fan coil unit air outlet node name = '//TRIM(NodeID(FanCoil(FanCoilNum)%AirOutNode)))
      ErrorsFound = .TRUE.
    END IF
  ! no air terminal mixer; do the normal connectivity checks
  ELSE
    ! check that the fan coil inlet node is the same as one of the zone exhaust nodes
    ZoneExNodeNotFound = .TRUE.
    DO CtrlZone = 1,NumOfZones
      IF (.not. ZoneEquipConfig(CtrlZone)%IsControlled) CYCLE
      DO NodeNum = 1,ZoneEquipConfig(CtrlZone)%NumExhaustNodes
        IF (FanCoil(FanCoilNum)%AirInNode .EQ. ZoneEquipConfig(CtrlZone)%ExhaustNode(NodeNum)) THEN
          ZoneExNodeNotFound = .FALSE.
        END IF
      END DO
    END DO
    IF (ZoneExNodeNotFound) THEN
      CALL ShowSevereError(TRIM(CurrentModuleObject)//' = "'//TRIM(FanCoil(FanCoilNum)%Name)//'".'// &
                         ' Fan coil unit air inlet node name must be the same as a zone exhaust node name.')
      CALL ShowContinueError('..Zone exhaust node name is specified in ZoneHVAC:EquipmentConnections object.')
      CALL ShowContinueError('..Fan coil unit air inlet node name = '//TRIM(NodeID(FanCoil(FanCoilNum)%AirInNode)))
      ErrorsFound=.TRUE.
    END IF
    ! check that the fan coil outlet node is the same as one of the zone inlet nodes
    ZoneInNodeNotFound = .TRUE.
    DO CtrlZone = 1,NumOfZones
      IF (.not. ZoneEquipConfig(CtrlZone)%IsControlled) CYCLE
      DO NodeNum = 1,ZoneEquipConfig(CtrlZone)%NumInletNodes
        IF (FanCoil(FanCoilNum)%AirOutNode .EQ. ZoneEquipConfig(CtrlZone)%InletNode(NodeNum)) THEN
          ZoneInNodeNotFound = .FALSE.
        END IF
      END DO
    END DO
    IF (ZoneInNodeNotFound) THEN
      CALL ShowSevereError(TRIM(CurrentModuleObject)//' = "'//TRIM(FanCoil(FanCoilNum)%Name)//'".'// &
                           ' Fan coil unit air outlet node name must be the same as a zone inlet node name.')
      CALL ShowContinueError('..Zone inlet node name is specified in ZoneHVAC:EquipmentConnections object.')
      CALL ShowContinueError('..Fan coil unit air outlet node name = '//TRIM(NodeID(FanCoil(FanCoilNum)%AirOutNode)))

      ErrorsFound=.TRUE.
    END IF
  END IF

  ! Set up component set for supply fan
  IF (FanCoil(FanCoilNum)%OutsideAirNode > 0) THEN
    CALL SetUpCompSets(FanCoil(FanCoilNum)%UnitType, FanCoil(FanCoilNum)%Name, &
                       FanCoil(FanCoilNum)%FanType,FanCoil(FanCoilNum)%FanName, &
                       NodeID(FanCoil(FanCoilNum)%MixedAirNode),'UNDEFINED')
  ELSE
    CALL SetUpCompSets(FanCoil(FanCoilNum)%UnitType, FanCoil(FanCoilNum)%Name, &
                     FanCoil(FanCoilNum)%FanType,FanCoil(FanCoilNum)%FanName, &
                     NodeID(FanCoil(FanCoilNum)%AirInNode),'UNDEFINED')
  END IF
   ! Set up component set for cooling coil
  CALL SetUpCompSets(FanCoil(FanCoilNum)%UnitType, FanCoil(FanCoilNum)%Name, &
                     FanCoil(FanCoilNum)%CCoilType,FanCoil(FanCoilNum)%CCoilName,'UNDEFINED','UNDEFINED')

   ! Set up component set for heating coil
  CALL SetUpCompSets(FanCoil(FanCoilNum)%UnitType, FanCoil(FanCoilNum)%Name, &
                     FanCoil(FanCoilNum)%HCoilType,FanCoil(FanCoilNum)%HCoilName, &
                     'UNDEFINED',NodeID(FanCoil(FanCoilNum)%AirOutNode))

   ! Set up component set for OA mixer - use OA node and Mixed air node
  IF (FanCoil(FanCoilNum)%OutsideAirNode > 0) THEN
    CALL SetUpCompSets(FanCoil(FanCoilNum)%UnitType, FanCoil(FanCoilNum)%Name, &
                       FanCoil(FanCoilNum)%OAMixType, FanCoil(FanCoilNum)%OAMixName, &
                       NodeID(FanCoil(FanCoilNum)%OutsideAirNode),NodeID(FanCoil(FanCoilNum)%MixedAirNode))
  END IF
END DO

DEALLOCATE(Alphas)
DEALLOCATE(cAlphaFields)
DEALLOCATE(cNumericFields)
DEALLOCATE(Numbers)
DEALLOCATE(lAlphaBlanks)
DEALLOCATE(lNumericBlanks)

IF (ErrorsFound) THEN
  CALL ShowFatalError(RoutineName//'Errors found in input. Preceding condition(s) cause termination.')
END IF

Do FanCoilNum=1,NumFanCoils
  ! Setup Report variables for the Fan Coils
  ! CurrentModuleObject='ZoneHVAC:FourPipeFanCoil'
  CALL SetupOutputVariable('Fan Coil Heating Rate [W]',FanCoil(FanCoilNum)%HeatPower,'System','Average',&
                           FanCoil(FanCoilNum)%Name)
  CALL SetupOutputVariable('Fan Coil Heating Energy [J]',FanCoil(FanCoilNum)%HeatEnergy,'System','Sum',&
                           FanCoil(FanCoilNum)%Name)
  CALL SetupOutputVariable('Fan Coil Total Cooling Rate [W]',FanCoil(FanCoilNum)%TotCoolPower,'System','Average',&
                           FanCoil(FanCoilNum)%Name)
  CALL SetupOutputVariable('Fan Coil Total Cooling Energy [J]',FanCoil(FanCoilNum)%TotCoolEnergy,'System','Sum',&
                           FanCoil(FanCoilNum)%Name)
  CALL SetupOutputVariable('Fan Coil Sensible Cooling Rate [W]',FanCoil(FanCoilNum)%SensCoolPower,'System','Average',&
                           FanCoil(FanCoilNum)%Name)
  CALL SetupOutputVariable('Fan Coil Sensible Cooling Energy [J]',FanCoil(FanCoilNum)%SensCoolEnergy,'System','Sum',&
                           FanCoil(FanCoilNum)%Name)
  CALL SetupOutputVariable('Fan Coil Fan Electric Power [W]',FanCoil(FanCoilNum)%ElecPower,'System','Average',&
                           FanCoil(FanCoilNum)%Name)
  CALL SetupOutputVariable('Fan Coil Fan Electric Energy [J]',FanCoil(FanCoilNum)%ElecEnergy,'System','Sum',&
                           FanCoil(FanCoilNum)%Name)
  IF (FanCoil(FanCoilNum)%CapCtrlMeth_Num == CCM_CycFan) THEN
    CALL SetupOutputVariable('Fan Coil Runtime Fraction []',FanCoil(FanCoilNum)%PLR,'System','Average',&
                              FanCoil(FanCoilNum)%Name)
    CALL SetupOutputVariable('Fan Coil Fan Speed Level []',FanCoil(FanCoilNum)%SpeedFanSel,'System','Average',&
                           FanCoil(FanCoilNum)%Name)
  END IF
  IF (FanCoil(FanCoilNum)%CapCtrlMeth_Num == CCM_VarFanVarFlow .or. FanCoil(FanCoilNum)%CapCtrlMeth_Num == CCM_VarFanConsFlow) THEN
    CALL SetupOutputVariable('Fan Coil Part Load Ratio []',FanCoil(FanCoilNum)%PLR,'System','Average',&
                              FanCoil(FanCoilNum)%Name)
  END IF
  CALL SetupOutputVariable('Fan Coil Availability Status []',FanCoil(FanCoilNum)%AvailStatus,&
                             'System','Average',FanCoil(FanCoilNum)%Name)
END DO

RETURN
END SUBROUTINE GetFanCoilUnits

SUBROUTINE InitFanCoilUnits(FanCoilNum, ZoneNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   March 2000
          !       MODIFIED       July 2012, Chandan Sharma - FSEC: Added zone sys avail managers
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is for initializations of the Fan Coil Components.

          ! METHODOLOGY EMPLOYED:
          ! Uses the status flags to trigger initializations.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE Psychrometrics,     ONLY: PsyRhoAirFnPbTdbW
  USE DataZoneEquipment,  ONLY: ZoneEquipInputsFilled,CheckZoneEquipmentList, FanCoil4Pipe_Num
  USE DataPlant,          ONLY: PlantLoop, ScanPlantLoopsForObject, TypeOf_CoilWaterCooling, &
                                TypeOf_CoilWaterDetailedFlatCooling
  USE FluidProperties,    ONLY: GetDensityGlycol
  USE PlantUtilities,     ONLY: InitComponentNodes
  USE DataHVACGlobals,    ONLY: ZoneComp

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN) :: FanCoilNum ! number of the current fan coil unit being simulated
  INTEGER, INTENT (IN) :: ZoneNum    ! number of zone being served

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER             :: InNode ! inlet node number in fan coil loop
  INTEGER             :: OutNode ! outlet node number in fan coil loop
  INTEGER             :: InletNode ! inlet node number for fan coil FanCoilNum
  INTEGER             :: HotConNode ! hot water control node number in fan coil loop
  INTEGER             :: ColdConNode ! hot water control node number in fan coil loop
  INTEGER             :: OutsideAirNode ! outside air node number in fan coil loop
  INTEGER             :: AirRelNode ! relief air node number in fan coil loop
  REAL(r64)           :: RhoAir ! air density at InNode
  LOGICAL,SAVE        :: MyOneTimeFlag = .true.
  LOGICAL,SAVE        :: ZoneEquipmentListChecked = .false.  ! True after the Zone Equipment List has been checked for items
  Integer             :: Loop
  LOGICAL, ALLOCATABLE,Save, DIMENSION(:) :: MyEnvrnFlag
  LOGICAL, ALLOCATABLE, SAVE, DIMENSION(:) :: MyPlantScanFlag
  REAL(r64)           :: rho
  LOGICAL             :: errFlag

          ! FLOW:

! Do the one time initializations
IF (MyOneTimeFlag) THEN

  ALLOCATE(MyEnvrnFlag(NumFanCoils))
  ALLOCATE(MySizeFlag(NumFanCoils))
  ALLOCATE(MyPlantScanFlag(NumFanCoils))
  MyEnvrnFlag = .TRUE.
  MySizeFlag = .TRUE.
  MyPlantScanFlag = .TRUE.
  MyOneTimeFlag = .false.

END IF

IF (ALLOCATED(ZoneComp)) THEN
  ZoneComp(FanCoil4Pipe_Num)%ZoneCompAvailMgrs(FanCoilNum)%ZoneNum = ZoneNum
  FanCoil(FanCoilNum)%AvailStatus = ZoneComp(FanCoil4Pipe_Num)%ZoneCompAvailMgrs(FanCoilNum)%AvailStatus
ENDIF

IF (MyPlantScanFlag(FanCoilNum) .AND. ALLOCATED(PlantLoop)) THEN
  errFlag=.false.
  CALL ScanPlantLoopsForObject( FanCoil(FanCoilNum)%HCoilName, &
                                FanCoil(FanCoilNum)%HCoilPlantTypeOfNum, &
                                FanCoil(FanCoilNum)%HWLoopNum,   &
                                FanCoil(FanCoilNum)%HWLoopSide,  &
                                FanCoil(FanCoilNum)%HWBranchNum, &
                                FanCoil(FanCoilNum)%HWCompNum,     &
                                errFlag=errFlag)

  IF (errFlag) THEN
    CALL ShowContinueError('Reference Unit="'//trim(FanCoil(FanCoilNum)%Name)//'", type='//trim(FanCoil(FanCoilNum)%UnitType))
    CALL ShowFatalError('InitFanCoilUnits: Program terminated for previous conditions.')
  ENDIF

  FanCoil(FanCoilNum)%HotPlantOutletNode = &
                      PlantLoop(FanCoil(FanCoilNum)%HWLoopNum)%LoopSide(FanCoil(FanCoilNum)%HWLoopSide) &
                         %Branch(FanCoil(FanCoilNum)%HWBranchNum)%Comp(FanCoil(FanCoilNum)%HWCompNum)%NodeNumOut

  IF ( (FanCoil(FanCoilNum)%CCoilPlantTypeOfNum == TypeOf_CoilWaterCooling) .OR. &
       (FanCoil(FanCoilNum)%CCoilPlantTypeOfNum == TypeOf_CoilWaterDetailedFlatCooling ) ) THEN
    CALL ScanPlantLoopsForObject( FanCoil(FanCoilNum)%CCoilPlantName, &
                                  FanCoil(FanCoilNum)%CCoilPlantTypeOfNum, &
                                  FanCoil(FanCoilNum)%CWLoopNum,   &
                                  FanCoil(FanCoilNum)%CWLoopSide,  &
                                  FanCoil(FanCoilNum)%CWBranchNum, &
                                  FanCoil(FanCoilNum)%CWCompNum,     &
                                  errFlag=errFlag)
    IF (errFlag) THEN
      CALL ShowContinueError('Reference Unit="'//trim(FanCoil(FanCoilNum)%Name)//'", type='//trim(FanCoil(FanCoilNum)%UnitType))
      CALL ShowFatalError('InitFanCoilUnits: Program terminated for previous conditions.')
    ENDIF
    FanCoil(FanCoilNum)%ColdPlantOutletNode = &
                      PlantLoop(FanCoil(FanCoilNum)%CWLoopNum)%LoopSide(FanCoil(FanCoilNum)%CWLoopSide) &
                         %Branch(FanCoil(FanCoilNum)%CWBranchNum)%Comp(FanCoil(FanCoilNum)%CWCompNum)%NodeNumOut
  ELSE
    CALL ShowFatalError('InitFanCoilUnits: FanCoil='//trim(FanCoil(FanCoilNum)%Name)//  &
          ', invalid cooling coil type. Program terminated.')
  ENDIF

  MyPlantScanFlag(FanCoilNum) = .FALSE.
ENDIF


IF (.not. ZoneEquipmentListChecked .and. ZoneEquipInputsFilled) THEN
  ZoneEquipmentListChecked=.true.
  DO Loop=1,NumFanCoils
    IF (CheckZoneEquipmentList(FanCoil(Loop)%UnitType,FanCoil(Loop)%Name)) CYCLE
    CALL ShowSevereError('InitFanCoil: FanCoil Unit=['//TRIM(FanCoil(Loop)%UnitType)//','//  &
       TRIM(FanCoil(Loop)%Name)//  &
         '] is not on any ZoneHVAC:EquipmentList.  It will not be simulated.')
  ENDDO
ENDIF

IF ( .NOT. SysSizingCalc .AND. MySizeFlag(FanCoilNum) .AND. .NOT. MyPlantScanFlag(FanCoilNum) ) THEN

  CALL SizeFanCoilUnit(FanCoilNum)

  MySizeFlag(FanCoilNum) = .FALSE.
END IF

! Do the Begin Environment initializations
IF (BeginEnvrnFlag .AND. MyEnvrnFlag(FanCoilNum) .AND. .NOT. MyPlantScanFlag(FanCoilNum)) THEN
  InNode = FanCoil(FanCoilNum)%AirInNode
  OutNode = FanCoil(FanCoilNum)%AirOutNode
  OutsideAirNode = FanCoil(FanCoilNum)%OutsideAirNode
  RhoAir = StdRhoAir
  HotConNode = FanCoil(FanCoilNum)%HotControlNode
  ColdConNode = FanCoil(FanCoilNum)%ColdControlNode
  ! set the mass flow rates from the input volume flow rates
  FanCoil(FanCoilNum)%MaxAirMassFlow = RhoAir*FanCoil(FanCoilNum)%MaxAirVolFlow
  FanCoil(FanCoilNum)%OutAirMassFlow = RhoAir*FanCoil(FanCoilNum)%OutAirVolFlow
  rho = GetDensityGlycol(PlantLoop(FanCoil(FanCoilNum)%HWLoopNum)%FluidName, &
                         60.d0, &
                         PlantLoop(FanCoil(FanCoilNum)%HWLoopNum)%FluidIndex, &
                         'InitFanCoilUnits')

  FanCoil(FanCoilNum)%MaxHotWaterFlow = rho * FanCoil(FanCoilNum)%MaxHotWaterVolFlow
  FanCoil(FanCoilNum)%MinHotWaterFlow = rho * FanCoil(FanCoilNum)%MinHotWaterVolFlow

  rho = GetDensityGlycol(PlantLoop(FanCoil(FanCoilNum)%CWLoopNum)%FluidName, &
                         InitConvTemp, &
                         PlantLoop(FanCoil(FanCoilNum)%CWLoopNum)%FluidIndex, &
                         'InitFanCoilUnits')
  FanCoil(FanCoilNum)%MaxColdWaterFlow = rho * FanCoil(FanCoilNum)%MaxColdWaterVolFlow
  FanCoil(FanCoilNum)%MinColdWaterFlow = rho * FanCoil(FanCoilNum)%MinColdWaterVolFlow

  ! set the node max and min mass flow rates
  Call InitComponentNodes(FanCoil(FanCoilNum)%MinHotWaterFlow, &
                          FanCoil(FanCoilNum)%MaxHotWaterFlow, &
                          FanCoil(FanCoilNum)%HotControlNode, &
                          FanCoil(FanCoilNum)%HotPlantOutletNode, &
                          FanCoil(FanCoilNum)%HWLoopNum, &
                          FanCoil(FanCoilNum)%HWLoopSide, &
                          FanCoil(FanCoilNum)%HWBranchNum, &
                          FanCoil(FanCoilNum)%HWCompNum)

  Call InitComponentNodes(FanCoil(FanCoilNum)%MinColdWaterFlow, &
                          FanCoil(FanCoilNum)%MaxColdWaterFlow, &
                          FanCoil(FanCoilNum)%ColdControlNode, &
                          FanCoil(FanCoilNum)%ColdPlantOutletNode, &
                          FanCoil(FanCoilNum)%CWLoopNum, &
                          FanCoil(FanCoilNum)%CWLoopSide, &
                          FanCoil(FanCoilNum)%CWBranchNum, &
                          FanCoil(FanCoilNum)%CWCompNum)
!  Node(HotConNode)%MassFlowRateMax = FanCoil(FanCoilNum)%MaxHotWaterFlow
!  Node(HotConNode)%MassFlowRateMin = FanCoil(FanCoilNum)%MinHotWaterFlow
!  Node(ColdConNode)%MassFlowRateMax = FanCoil(FanCoilNum)%MaxColdWaterFlow
!  Node(ColdConNode)%MassFlowRateMin = FanCoil(FanCoilNum)%MinColdWaterFlow

  IF (FanCoil(FanCoilNum)%OutsideAirNode > 0) THEN
    Node(OutsideAirNode)%MassFlowRateMax = FanCoil(FanCoilNum)%OutAirMassFlow
    Node(OutsideAirNode)%MassFlowRateMin = 0.0d0
  END IF
  Node(OutNode)%MassFlowRateMax = FanCoil(FanCoilNum)%MaxAirMassFlow
  Node(OutNode)%MassFlowRateMin = 0.0d0
  Node(InNode)%MassFlowRateMax = FanCoil(FanCoilNum)%MaxAirMassFlow
  Node(InNode)%MassFlowRateMin = 0.0d0
  MyEnvrnFlag(FanCoilNum) = .FALSE.
END IF ! end one time inits

IF (.not. BeginEnvrnFlag) THEN
  MyEnvrnFlag(FanCoilNum)=.true.
ENDIF

! These initializations are done every iteration
InletNode = FanCoil(FanCoilNum)%AirInNode
OutsideAirNode = FanCoil(FanCoilNum)%OutsideAirNode
AirRelNode = FanCoil(FanCoilNum)%AirReliefNode
! Set the inlet node mass flow rate
IF (GetCurrentScheduleValue(FanCoil(FanCoilNum)%SchedPtr) .gt. 0.0d0) THEN
  Node(InletNode)%MassFlowRate = FanCoil(FanCoilNum)%MaxAirMassFlow
  Node(InletNode)%MassFlowRateMaxAvail = Node(InletNode)%MassFlowRate
  Node(InletNode)%MassFlowRateMinAvail = Node(InletNode)%MassFlowRate

  IF (OutsideAirNode > 0) THEN
    Node(OutsideAirNode)%MassFlowRate = FanCoil(FanCoilNum)%OutAirMassFlow
    Node(OutsideAirNode)%MassFlowRateMaxAvail = FanCoil(FanCoilNum)%OutAirMassFlow
    Node(OutsideAirNode)%MassFlowRateMinAvail = FanCoil(FanCoilNum)%OutAirMassFlow
    Node(AirRelNode)%MassFlowRate = FanCoil(FanCoilNum)%OutAirMassFlow
    Node(AirRelNode)%MassFlowRateMaxAvail = FanCoil(FanCoilNum)%OutAirMassFlow
    Node(AirRelNode)%MassFlowRateMinAvail = FanCoil(FanCoilNum)%OutAirMassFlow
  ENDIF

ELSE
  Node(InletNode)%MassFlowRate = 0.0d0
  Node(InletNode)%MassFlowRateMaxAvail = 0.0d0
  Node(InletNode)%MassFlowRateMinAvail = 0.0d0
  IF (OutsideAirNode > 0) THEN
    Node(OutsideAirNode)%MassFlowRate = 0.0d0
    Node(OutsideAirNode)%MassFlowRateMaxAvail = 0.0d0
    Node(OutsideAirNode)%MassFlowRateMinAvail = 0.0d0
    Node(AirRelNode)%MassFlowRate = 0.0d0
    Node(AirRelNode)%MassFlowRateMaxAvail = 0.0d0
    Node(AirRelNode)%MassFlowRateMinAvail = 0.0d0
  END IF
END IF

RETURN
END SUBROUTINE InitFanCoilUnits

SUBROUTINE SizeFanCoilUnit(FanCoilNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   January 2002
          !       MODIFIED       August 2013 Daeho Kang, add component sizing table entries
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is for sizing Fan Coil Unit components for which flow rates have not been
          ! specified in the input.

          ! METHODOLOGY EMPLOYED:
          ! Obtains flow rates from the zone or system sizing arrays and plant sizing data.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor
  USE Psychrometrics, ONLY: PsyCpAirFnWTdb, PsyHFnTdbW
  USE Fans,           ONLY: SimulateFanComponents, GetFanDesignVolumeFlowRate
  USE General,        ONLY: TrimSigDigits
  USE WaterCoils,     ONLY: SetCoilDesFlow, GetCoilWaterInletNode, GetCoilWaterOutletNode
  USE HVACHXAssistedCoolingCoil, ONLY: GetHXDXCoilName, GetHXCoilType
  USE DataPlant,      ONLY: PlantLoop, MyPlantSizingIndex
  USE FluidProperties, ONLY: GetDensityGlycol, GetSpecificHeatGlycol
  USE ReportSizingManager, ONLY: ReportSizingOutput
  USE General,             ONLY: RoundSigDigits

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: FanCoilNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER     :: RoutineName='SizeFanCoilUnit: ' ! include trailing blank space

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER             :: PltSizHeatNum ! index of plant sizing object for 1st heating loop
  INTEGER             :: PltSizCoolNum ! index of plant sizing object for 1st cooling loop
  REAL(r64)           :: CoilInTemp    ! design inlet air temperature for coil [C]
  REAL(r64)           :: CoilOutTemp   ! design outlet air temperature for coil [C]
  REAL(r64)           :: CoilOutHumRat ! design inlet air humidity ratio for coil [kg/kg]
  REAL(r64)           :: CoilInHumRat  ! design outlet air humidity ratio for coil [kg/kg]
  LOGICAL             :: ErrorsFound   ! TRUE if errors foind during sizing
  REAL(r64)           :: DesCoilLoad   ! coil load used for sizing [W]
  REAL(r64)           :: FCOAFrac      ! design outside air fraction for the fan coil unit
  INTEGER             :: CoilWaterInletNode=0
  INTEGER             :: CoilWaterOutletNode=0
  CHARACTER(len=MaxNameLength) :: CoolingCoilName
  CHARACTER(len=MaxNameLength) :: CoolingCoilType
  REAL(r64)   :: rho
  REAL(r64)   :: Cp
  LOGICAL             :: IsAutoSize               ! Indicator to autosize for reporting
  REAL(r64)           :: MaxAirVolFlowDes         ! Autosized max air flow for reporting
  REAL(r64)           :: MaxAirVolFlowUser        ! Hardsized max air flow for reporting
  REAL(r64)           :: OutAirVolFlowDes         ! Autosized outdoor air flow for reporting
  REAL(r64)           :: OutAirVolFlowUser        ! Hardsized outdoor air flow for reporting
  REAL(r64)           :: MaxHotWaterVolFlowDes    ! Autosized hot water flow for reporting
  REAL(r64)           :: MaxHotWaterVolFlowUser   ! Hardsized hot water flow for reporting
  REAL(r64)           :: MaxColdWaterVolFlowDes   ! Autosized cold water flow for reporting
  REAL(r64)           :: MaxColdWaterVolFlowUser  ! Hardsized cold water flow for reporting

  PltSizCoolNum = 0
  PltSizHeatNum = 0
  ErrorsFound = .FALSE.
  IsAutosize = .FALSE.
  MaxAirVolFlowDes = 0.0d0
  MaxAirVolFlowUser = 0.0d0
  OutAirVolFlowDes = 0.0d0
  OutAirVolFlowUser = 0.0d0
  MaxHotWaterVolFlowDes = 0.0d0
  MaxHotWaterVolFlowUser = 0.0d0
  MaxColdWaterVolFlowDes = 0.0d0
  MaxColdWaterVolFlowUser = 0.0d0

  IF (FanCoil(FanCoilNum)%MaxAirVolFlow == AutoSize) THEN
    IsAutosize = .TRUE.
  END IF

  IF (CurZoneEqNum > 0) THEN
    IF (.NOT. IsAutosize .AND. .NOT. ZoneSizingRunDone) THEN
      IF (FanCoil(FanCoilNum)%MaxAirVolFlow > 0.0d0) THEN
        CALL ReportSizingOutput(FanCoil(FanCoilNum)%UnitType, FanCoil(FanCoilNum)%Name, &
                          'User-Specified Supply Air Maximum Flow Rate [m3/s]', FanCoil(FanCoilNum)%MaxAirVolFlow)
      END IF
    ELSE
      CALL CheckZoneSizing(FanCoil(FanCoilNum)%UnitType, FanCoil(FanCoilNum)%Name)
      MaxAirVolFlowDes = MAX(FinalZoneSizing(CurZoneEqNum)%DesCoolVolFlow, &
                                              FinalZoneSizing(CurZoneEqNum)%DesHeatVolFlow)
      IF (MaxAirVolFlowDes < SmallAirVolFlow) THEN
        MaxAirVolFlowDes = 0.0d0
      END IF

!     If fan is autosized, get fan volumetric flow rate
      IF(FanCoil(FanCoilNum)%FanAirVolFlow .EQ. AutoSize)THEN
        CALL SimulateFanComponents(FanCoil(FanCoilNum)%FanName,.TRUE.,FanCoil(FanCoilNum)%FanIndex)
        FanCoil(FanCoilNum)%FanAirVolFlow = GetFanDesignVolumeFlowRate(TRIM(cFanTypes(FanCoil(FanCoilNum)%FanType_Num)), &
                                                                       TRIM(FanCoil(FanCoilNum)%FanName),ErrorsFound)
      END IF
!     Check that the fan volumetric flow rate is greater than or equal to the FCU voluetric flow rate
      IF(MaxAirVolFlowDes .GT. FanCoil(FanCoilNum)%FanAirVolFlow )THEN
        CALL ShowWarningError(RoutineName//TRIM(FanCoil(FanCoilNum)%UnitType)//': '//TRIM(FanCoil(FanCoilNum)%Name))
        CALL ShowContinueError('... Maximum supply air flow rate is greater than the maximum fan flow rate.')
        CALL ShowContinueError('... Fan Coil Unit flow = '//TRIM(TrimSigDigits(MaxAirVolFlowDes,5))//' [m3/s].')
        CALL ShowContinueError('... Fan = '//TRIM(cFanTypes(FanCoil(FanCoilNum)%FanType_Num))//': '//  &
           TRIM(FanCoil(FanCoilNum)%FanName))
        CALL ShowContinueError('... Fan flow = '//TRIM(TrimSigDigits(FanCoil(FanCoilNum)%FanAirVolFlow,5))//' [m3/s].')
        CALL ShowContinueError('... Fan Coil Unit flow rate reduced to match the fan flow rate and the simulation continues.')
        MaxAirVolFlowDes = FanCoil(FanCoilNum)%FanAirVolFlow
      END IF
      IF (IsAutosize) THEN
        FanCoil(FanCoilNum)%MaxAirVolFlow = MaxAirVolFlowDes
        CALL ReportSizingOutput(FanCoil(FanCoilNum)%UnitType, FanCoil(FanCoilNum)%Name, &
                              'Design Size Supply Air Maximum Flow Rate [m3/s]', MaxAirVolFlowDes)
      ELSE ! Hard size with sizing data
        IF (FanCoil(FanCoilNum)%MaxAirVolFlow > 0.0d0 .AND. MaxAirVolFlowDes > 0.0d0) THEN
          MaxAirVolFlowUser = FanCoil(FanCoilNum)%MaxAirVolFlow
          CALL ReportSizingOutput(FanCoil(FanCoilNum)%UnitType, FanCoil(FanCoilNum)%Name, &
                              'Design Size Supply Air Maximum Flow Rate [m3/s]', MaxAirVolFlowDes, &
                              'User-Specified Supply Air Maximum Flow Rate [m3/s]', MaxAirVolFlowUser)
          IF (DisplayExtraWarnings) THEN
            IF ((ABS(MaxAirVolFlowDes - MaxAirVolFlowUser)/MaxAirVolFlowUser) > AutoVsHardSizingThreshold) THEN
              CALL ShowMessage('SizeFanCoilUnit: Potential issue with equipment sizing for ' &
                                     //TRIM(FanCoil(FanCoilNum)%UnitType)//' '//TRIM(FanCoil(FanCoilNum)%Name))
              CALL ShowContinueError('User-Specified Supply Air Maximum Flow Rate of '// &
                                      TRIM(RoundSigDigits(MaxAirVolFlowUser,5))// ' [m3/s]')
              CALL ShowContinueError('differs from Design Size Supply Air Maximum Flow Rate of ' // &
                                      TRIM(RoundSigDigits(MaxAirVolFlowDes,5))// ' [m3/s]')
              CALL ShowContinueError('This may, or may not, indicate mismatched component sizes.')
              CALL ShowContinueError('Verify that the value entered is intended and is consistent with other components.')
            END IF
          ENDIF
        END IF
      END IF
    END IF

  ELSE IF(FanCoil(FanCoilNum)%FanAirVolFlow .EQ. AutoSize)THEN
    CALL SimulateFanComponents(FanCoil(FanCoilNum)%FanName,.TRUE.,FanCoil(FanCoilNum)%FanIndex)
    FanCoil(FanCoilNum)%FanAirVolFlow = GetFanDesignVolumeFlowRate(TRIM(cFanTypes(FanCoil(FanCoilNum)%FanType_Num)), &
                                                                   TRIM(FanCoil(FanCoilNum)%FanName),ErrorsFound)
!   Check that the fan volumetric flow rate is greater than or equal to the FCU volumetric flow rate
    IF(FanCoil(FanCoilNum)%MaxAirVolFlow .GT. FanCoil(FanCoilNum)%FanAirVolFlow)THEN
      CALL ShowWarningError(RoutineName//TRIM(FanCoil(FanCoilNum)%UnitType)//': '//TRIM(FanCoil(FanCoilNum)%Name))
      CALL ShowContinueError('... Maximum supply air flow rate is greater than the maximum fan flow rate.')
      CALL ShowContinueError('... Fan Coil Unit flow = '//TRIM(TrimSigDigits(FanCoil(FanCoilNum)%MaxAirVolFlow,5))//' m3/s.')
      CALL ShowContinueError('... Fan = '//TRIM(cFanTypes(FanCoil(FanCoilNum)%FanType_Num))//': '//  &
         TRIM(FanCoil(FanCoilNum)%FanName))
      CALL ShowContinueError('... Fan flow = '//TRIM(TrimSigDigits(FanCoil(FanCoilNum)%FanAirVolFlow,5))//' m3/s.')
      CALL ShowContinueError('... Fan Coil Unit flow rate reduced to match the fan flow rate and the simulation continues.')
      FanCoil(FanCoilNum)%MaxAirVolFlow = FanCoil(FanCoilNum)%FanAirVolFlow
    END IF
  END IF

  IsAutosize = .FALSE.
  IF (FanCoil(FanCoilNum)%OutAirVolFlow == AutoSize) THEN
    IsAutosize = .TRUE.
  END IF

  IF (CurZoneEqNum > 0) THEN
    IF (.NOT. IsAutosize .AND. .NOT. ZoneSizingRunDone) THEN
      IF (FanCoil(FanCoilNum)%OutAirVolFlow > 0.0d0) THEN
        CALL ReportSizingOutput(FanCoil(FanCoilNum)%UnitType, FanCoil(FanCoilNum)%Name, &
                              'User-Specified Maximum Outdoor Air Flow Rate [m3/s]',FanCoil(FanCoilNum)%OutAirVolFlow)
      END IF
    ELSE
      CALL CheckZoneSizing(FanCoil(FanCoilNum)%UnitType, FanCoil(FanCoilNum)%Name)
      OutAirVolFlowDes = MIN(FinalZoneSizing(CurZoneEqNum)%MinOA,FanCoil(FanCoilNum)%MaxAirVolFlow)
      IF (OutAirVolFlowDes < SmallAirVolFlow) THEN
        OutAirVolFlowDes = 0.0d0
      END IF
      IF (IsAutosize) THEN
        FanCoil(FanCoilNum)%OutAirVolFlow = OutAirVolFlowDes
        CALL ReportSizingOutput(FanCoil(FanCoilNum)%UnitType, FanCoil(FanCoilNum)%Name, &
                              'Design Size Maximum Outdoor Air Flow Rate [m3/s]',OutAirVolFlowDes)
      ELSE
        IF (FanCoil(FanCoilNum)%OutAirVolFlow > 0.0d0 .AND. OutAirVolFlowDes > 0.0d0) THEN
          OutAirVolFlowUser = FanCoil(FanCoilNum)%OutAirVolFlow
          CALL ReportSizingOutput(FanCoil(FanCoilNum)%UnitType, FanCoil(FanCoilNum)%Name, &
                              'Design Size Maximum Outdoor Air Flow Rate [m3/s]',OutAirVolFlowDes, &
                              'User-Specified Maximum Outdoor Air Flow Rate [m3/s]',OutAirVolFlowUser)
          IF (DisplayExtraWarnings) THEN
            IF ((ABS(OutAirVolFlowDes - OutAirVolFlowUser)/OutAirVolFlowUser) > AutoVsHardSizingThreshold) THEN
              CALL ShowMessage('SizeFanCoilUnit: Potential issue with equipment sizing for ' &
                                     //TRIM(FanCoil(FanCoilNum)%UnitType)//' '//TRIM(FanCoil(FanCoilNum)%Name))
              CALL ShowContinueError('User-Specified Maximum Outdoor Air Flow Rate of '// &
                                      TRIM(RoundSigDigits(OutAirVolFlowUser,5))// ' [m3/s]')
              CALL ShowContinueError('differs from Design Size Maximum Outdoor Air Flow Rate of ' // &
                                      TRIM(RoundSigDigits(OutAirVolFlowDes,5))// ' [m3/s]')
              CALL ShowContinueError('This may, or may not, indicate mismatched component sizes.')
              CALL ShowContinueError('Verify that the value entered is intended and is consistent with other components.')
            END IF
          ENDIF
        END IF
      END IF
    END IF
  END IF

  IsAutosize = .FALSE.
  IF (FanCoil(FanCoilNum)%MaxHotWaterVolFlow == AutoSize) THEN
    IsAutosize = .TRUE.
  END IF

  IF (CurZoneEqNum > 0) THEN
    IF (.NOT. IsAutosize .AND. .NOT. ZoneSizingRunDone) THEN
      IF (FanCoil(FanCoilNum)%MaxHotWaterVolFlow > 0.0d0) THEN
        CALL ReportSizingOutput(FanCoil(FanCoilNum)%UnitType, FanCoil(FanCoilNum)%Name, &
                                'User-Specified Maximum Hot Water Flow [m3/s]', FanCoil(FanCoilNum)%MaxHotWaterVolFlow)
      END IF
    ELSE
      CoilWaterInletNode = GetCoilWaterInletNode('Coil:Heating:Water',FanCoil(FanCoilNum)%HCoilName,ErrorsFound)
      CoilWaterOutletNode = GetCoilWaterOutletNode('Coil:Heating:Water',FanCoil(FanCoilNum)%HCoilName,ErrorsFound)
      IF (IsAutosize) THEN
        PltSizHeatNum = MyPlantSizingIndex('Coil:Heating:Water', FanCoil(FanCoilNum)%HCoilName, CoilWaterInletNode, &
                                         CoilWaterOutletNode, ErrorsFound)
        IF (PltSizHeatNum > 0) THEN
          CALL CheckZoneSizing(FanCoil(FanCoilNum)%UnitType, FanCoil(FanCoilNum)%Name)
          IF (FinalZoneSizing(CurZoneEqNum)%DesHeatMassFlow > 0.0d0) THEN
            FCOAFrac = MIN(FanCoil(FanCoilNum)%OutAirVolFlow / FinalZoneSizing(CurZoneEqNum)%DesHeatMassFlow, 1.0d0)
          ELSE
            FCOAFrac = 0.0d0
          END IF
          CoilInTemp = FCOAFrac*FinalZoneSizing(CurZoneEqNum)%OutTempAtHeatPeak + &
                     (1.0d0-FCOAFrac)*FinalZoneSizing(CurZoneEqNum)%ZoneTempAtHeatPeak
          CoilOutTemp = FinalZoneSizing(CurZoneEqNum)%HeatDesTemp
          CoilOutHumRat =FinalZoneSizing(CurZoneEqNum)%HeatDesHumRat
          DesCoilLoad = PsyCpAirFnWTdb(CoilOutHumRat, 0.5d0*(CoilInTemp+CoilOutTemp)) &
                          * FinalZoneSizing(CurZoneEqNum)%DesHeatMassFlow &
                          * (CoilOutTemp-CoilInTemp)
          FanCoil(FanCoilNum)%DesHeatingLoad = DesCoilLoad
          IF (DesCoilLoad >= SmallLoad) THEN
            rho = GetDensityGlycol( PlantLoop(FanCoil(FanCoilNum)%HWLoopNum)%FluidName, &
                         60.d0, &
                         PlantLoop(FanCoil(FanCoilNum)%HWLoopNum)%FluidIndex, &
                         'SizeFanCoilUnit')


            Cp  = GetSpecificHeatGlycol(PlantLoop(FanCoil(FanCoilNum)%HWLoopNum)%FluidName, &
                         60.d0, &
                         PlantLoop(FanCoil(FanCoilNum)%HWLoopNum)%FluidIndex, &
                         'SizeFanCoilUnit')


            MaxHotWaterVolFlowDes = DesCoilLoad / &
                                  ( PlantSizData(PltSizHeatNum)%DeltaT * &
                                  Cp * rho )
          ELSE
            MaxHotWaterVolFlowDes = 0.0d0
          END IF
        ELSE
          CALL ShowSevereError('Autosizing of water flow requires a heating loop Sizing:Plant object')
          CALL ShowContinueError('Occurs in ' // TRIM(FanCoil(FanCoilNum)%UnitType) // ' Object=' &
                               //TRIM(FanCoil(FanCoilNum)%Name))
          ErrorsFound = .TRUE.
        END IF
      END IF
      IF (IsAutosize) THEN
        FanCoil(FanCoilNum)%MaxHotWaterVolFlow = MaxHotWaterVolFlowDes
        CALL ReportSizingOutput(FanCoil(FanCoilNum)%UnitType, FanCoil(FanCoilNum)%Name, &
                                'Design Size Maximum Hot Water Flow [m3/s]', MaxHotWaterVolFlowDes)
      ELSE ! Hard size with sizing data
        IF (FanCoil(FanCoilNum)%MaxHotWaterVolFlow > 0.0d0 .AND. MaxHotWaterVolFlowDes > 0.0d0) THEN
          MaxHotWaterVolFlowDes = FanCoil(FanCoilNum)%MaxHotWaterVolFlow
          CALL ReportSizingOutput(FanCoil(FanCoilNum)%UnitType, FanCoil(FanCoilNum)%Name, &
                                'Design Size Maximum Hot Water Flow [m3/s]', MaxHotWaterVolFlowDes, &
                                'User-Specified Maximum Hot Water Flow [m3/s]', MaxHotWaterVolFlowUser)
          IF (DisplayExtraWarnings) THEN
            IF ((ABS(MaxHotWaterVolFlowDes - MaxHotWaterVolFlowUser)/ MaxHotWaterVolFlowUser) > AutoVsHardSizingThreshold) THEN
              CALL ShowMessage('SizeFanCoilUnit: Potential issue with equipment sizing for ' &
                                    //TRIM(FanCoil(FanCoilNum)%UnitType)//' '//TRIM(FanCoil(FanCoilNum)%Name))
              CALL ShowContinueError('User-Specified Maximum Hot Water Flow of '// &
                                      TRIM(RoundSigDigits(MaxHotWaterVolFlowUser,5))// ' [m3/s]')
              CALL ShowContinueError('differs from Design Size Maximum Hot Water Flow of ' // &
                                      TRIM(RoundSigDigits(MaxHotWaterVolFlowDes,5))// ' [m3/s]')
              CALL ShowContinueError('This may, or may not, indicate mismatched component sizes.')
              CALL ShowContinueError('Verify that the value entered is intended and is consistent with other components.')
            END IF
          ENDIf
        END IF
      END IF
    END IF
  END IF

  IsAutosize = .FALSE.
  IF (FanCoil(FanCoilNum)%MaxColdWaterVolFlow == AutoSize) THEN
    IsAutosize = .TRUE.
  END IF

  IF (CurZoneEqNum > 0) THEN
    IF (.NOT. IsAutosize .AND. .NOT. ZoneSizingRunDone) THEN
      IF (FanCoil(FanCoilNum)%MaxColdWaterVolFlow > 0.0d0) THEN
        CALL ReportSizingOutput(FanCoil(FanCoilNum)%UnitType, FanCoil(FanCoilNum)%Name, &
                     'User-Specified Maximum Cold Water Flow [m3/s]', FanCoil(FanCoilNum)%MaxColdWaterVolFlow)
      END IF
    ELSE
      IF (SameString(FanCoil(FanCoilNum)%CCoilType,'CoilSystem:Cooling:Water:HeatExchangerAssisted')) THEN
        CoolingCoilName = GetHXDXCoilName(FanCoil(FanCoilNum)%CCoilType,FanCoil(FanCoilNum)%CCoilName,ErrorsFound)
        CoolingCoilType = GetHXCoilType(FanCoil(FanCoilNum)%CCoilType,FanCoil(FanCoilNum)%CCoilName,ErrorsFound)
      ELSE
        CoolingCoilName = FanCoil(FanCoilNum)%CCoilName
        CoolingCoilType = FanCoil(FanCoilNum)%CCoilType
      END IF
      CoilWaterInletNode = GetCoilWaterInletNode(CoolingCoilType,CoolingCoilName,ErrorsFound)
      CoilWaterOutletNode = GetCoilWaterOutletNode(CoolingCoilType,CoolingCoilName,ErrorsFound)
      IF (IsAutosize) THEN
        PltSizCoolNum = MyPlantSizingIndex(CoolingCoilType, CoolingCoilName, CoilWaterInletNode, &
                                         CoilWaterOutletNode, ErrorsFound)
        IF (PltSizCoolNum > 0) THEN
          CALL CheckZoneSizing(FanCoil(FanCoilNum)%UnitType, FanCoil(FanCoilNum)%Name)
          IF (FinalZoneSizing(CurZoneEqNum)%DesCoolMassFlow > 0.0d0) THEN
            FCOAFrac = MIN(FanCoil(FanCoilNum)%OutAirVolFlow / FinalZoneSizing(CurZoneEqNum)%DesCoolMassFlow, 1.0d0)
          ELSE
            FCOAFrac = 0.0d0
          END IF
          CoilInTemp = FCOAFrac*FinalZoneSizing(CurZoneEqNum)%OutTempAtCoolPeak + &
                       (1.0d0-FCOAFrac)*FinalZoneSizing(CurZoneEqNum)%ZoneTempAtCoolPeak
          CoilInHumRat = FCOAFrac*FinalZoneSizing(CurZoneEqNum)%OutHumRatAtCoolPeak + &
                        (1.0d0-FCOAFrac)*FinalZoneSizing(CurZoneEqNum)%ZoneHumRatAtCoolPeak
          CoilOutTemp = FinalZoneSizing(CurZoneEqNum)%CoolDesTemp
          CoilOutHumRat = FinalZoneSizing(CurZoneEqNum)%CoolDesHumRat
          IF (CoilOutHumRat > CoilInHumRat) THEN
            IF (CoilInHumRat > 0.016d0) THEN
              CoilOutHumRat = 0.5d0*CoilInHumRat
            ELSE
              CoilOutHumRat = CoilInHumRat
            END IF
          END IF
          DesCoilLoad = FinalZoneSizing(CurZoneEqNum)%DesCoolMassFlow &
                      * (PsyHFnTdbW(CoilInTemp, CoilInHumRat)-PsyHFnTdbW(CoilOutTemp, CoilOutHumRat))
          FanCoil(FanCoilNum)%DesCoolingLoad = DesCoilLoad
          IF (DesCoilLoad >= SmallLoad) THEN

            rho = GetDensityGlycol( PlantLoop(FanCoil(FanCoilNum)%CWLoopNum)%FluidName, &
                         5.d0, &
                         PlantLoop(FanCoil(FanCoilNum)%CWLoopNum)%FluidIndex, &
                         'SizeFanCoilUnit')

            Cp  = GetSpecificHeatGlycol(PlantLoop(FanCoil(FanCoilNum)%CWLoopNum)%FluidName, &
                         5.d0, &
                         PlantLoop(FanCoil(FanCoilNum)%CWLoopNum)%FluidIndex, &
                         'SizeFanCoilUnit')

            MaxColdWaterVolFlowDes = DesCoilLoad / &
                                   ( PlantSizData(PltSizCoolNum)%DeltaT * &
                                   Cp * rho )
          ELSE
            MaxColdWaterVolFlowDes = 0.0d0
          END IF
        ELSE
          CALL ShowSevereError('Autosizing of water flow requires a cooling loop Sizing:Plant object')
          CALL ShowContinueError('Occurs in ' // TRIM(FanCoil(FanCoilNum)%UnitType) // ' Object=' &
                               //TRIM(FanCoil(FanCoilNum)%Name))
          ErrorsFound = .TRUE.
        END IF
      END IF
      IF (IsAutosize) THEN
        FanCoil(FanCoilNum)%MaxColdWaterVolFlow = MaxColdWaterVolFlowDes
        CALL ReportSizingOutput(FanCoil(FanCoilNum)%UnitType, FanCoil(FanCoilNum)%Name, &
                                'Design Size Maximum Cold Water Flow [m3/s]', MaxColdWaterVolFlowDes)
      ELSE ! Hard size with sizing data
        IF (FanCoil(FanCoilNum)%MaxColdWaterVolFlow > 0.0d0 .AND. MaxColdWaterVolFlowDes > 0.0d0) THEN
          MaxColdWaterVolFlowUser = FanCoil(FanCoilNum)%MaxColdWaterVolFlow
          CALL ReportSizingOutput(FanCoil(FanCoilNum)%UnitType, FanCoil(FanCoilNum)%Name, &
                                'Design Size Maximum Cold Water Flow [m3/s]', MaxColdWaterVolFlowDes, &
                                'User-Specified Maximum Cold Water Flow [m3/s]', MaxColdWaterVolFlowUser)
          IF (DisplayExtraWarnings) THEN
            IF ((ABS(MaxColdWaterVolFlowDes - MaxColdWaterVolFlowUser)/MaxColdWaterVolFlowUser) > AutoVsHardSizingThreshold) THEN
              CALL ShowMessage('SizeFanCoilUnit: Potential issue with equipment sizing for ' &
                                    //TRIM(FanCoil(FanCoilNum)%UnitType)//' '//TRIM(FanCoil(FanCoilNum)%Name))
              CALL ShowContinueError('User-Specified Maximum Cold Water Flow of '// &
                                      TRIM(RoundSigDigits(MaxColdWaterVolFlowUser,5))// '[m3/s]')
              CALL ShowContinueError('differs from Design Size Maximum Cold Water Flow of ' // &
                                      TRIM(RoundSigDigits(MaxColdWaterVolFlowDes,5))// '[m3/s]')
              CALL ShowContinueError('This may, or may not, indicate mismatched component sizes.')
              CALL ShowContinueError('Verify that the value entered is intended and is consistent with other components.')
            END IF
          ENDIF
        END IF
      END IF
    END IF
  END IF
  ! set the design air flow rates for the heating and cooling coils
  IF (SameString(FanCoil(FanCoilNum)%CCoilType,'CoilSystem:Cooling:Water:HeatExchangerAssisted')) THEN
    CoolingCoilName = GetHXDXCoilName(FanCoil(FanCoilNum)%CCoilType,FanCoil(FanCoilNum)%CCoilName,ErrorsFound)
    CoolingCoilType = GetHXCoilType(FanCoil(FanCoilNum)%CCoilType,FanCoil(FanCoilNum)%CCoilName,ErrorsFound)
  ELSE
    CoolingCoilName = FanCoil(FanCoilNum)%CCoilName
    CoolingCoilType = FanCoil(FanCoilNum)%CCoilType
  END IF
  IF (ZoneSizingRunDone) THEN
    CALL SetCoilDesFlow(CoolingCoilType,CoolingCoilName,FinalZoneSizing(CurZoneEqNum)%DesCoolVolFlow,&
                        ErrorsFound)
    CALL SetCoilDesFlow(FanCoil(FanCoilNum)%HCoilType,FanCoil(FanCoilNum)%HCoilName,FinalZoneSizing(CurZoneEqNum)%DesHeatVolFlow,&
                        ErrorsFound)
  ELSE
    CALL SetCoilDesFlow(CoolingCoilType,CoolingCoilName,FanCoil(FanCoilNum)%MaxAirVolFlow,&
                        ErrorsFound)
    CALL SetCoilDesFlow(FanCoil(FanCoilNum)%HCoilType,FanCoil(FanCoilNum)%HCoilName,FanCoil(FanCoilNum)%MaxAirVolFlow,&
                        ErrorsFound)
  END IF
  IF (CurZoneEqNum > 0) THEN
    ZoneEqSizing(CurZoneEqNum)%MaxHWVolFlow = FanCoil(FanCoilNum)%MaxHotWaterVolFlow
    ZoneEqSizing(CurZoneEqNum)%MaxCWVolFlow = FanCoil(FanCoilNum)%MaxColdWaterVolFlow
    ZoneEqSizing(CurZoneEqNum)%OAVolFlow = FanCoil(FanCoilNum)%OutAirVolFlow
    ZoneEqSizing(CurZoneEqNum)%AirVolFlow = FanCoil(FanCoilNum)%MaxAirVolFlow
    ZoneEqSizing(CurZoneEqNum)%DesCoolingLoad = FanCoil(FanCoilNum)%DesCoolingLoad
    ZoneEqSizing(CurZoneEqNum)%DesHeatingLoad = FanCoil(FanCoilNum)%DesHeatingLoad
  END IF

  IF (ErrorsFound) THEN
    CALL ShowFatalError('Preceding sizing errors cause program termination')
  END IF

  RETURN

END SUBROUTINE SizeFanCoilUnit

SUBROUTINE Sim4PipeFanCoil(FanCoilNum,ZoneNum,ControlledZoneNum,FirstHVACIteration,PowerMet,LatOutputProvided)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   March 2000
          !       MODIFIED       Don Shirey, Aug 2009 (LatOutputProvided)
          !       MODIFIED       Arnaud Flament June 2010 (added airflow capacity control methods)
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Simulate a 4 pipe fan coil unit; adjust its output to match the
          ! remaining zone load.

          ! METHODOLOGY EMPLOYED:
          ! If unit is on, calls ControlCompOutput to obtain the desired unit output

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataZoneEnergyDemands
  USE Psychrometrics, ONLY:PsyHFnTdbW
  USE DataHeatBalFanSys, ONLY:TempControlType
  USE DataInterfaces, ONLY: ControlCompOutput
  USE General, ONLY: TrimSigDigits
  USE PlantUtilities, ONLY: SetComponentFlowRate


  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  LOGICAL, INTENT (IN)     :: FirstHVACIteration ! TRUE if 1st HVAC simulation of system timestep
  INTEGER, INTENT (INOUT)  :: FanCoilNum         ! number of the current fan coil unit being simulated
  INTEGER, INTENT (IN)     :: ZoneNum            ! number of zone being served
  INTEGER, INTENT (IN)     :: ControlledZoneNum  ! index into ZoneEqupConfig
  REAL(r64), INTENT (OUT)  :: PowerMet           ! Sensible power supplied (W)
  REAL(r64), INTENT (OUT)  :: LatOutputProvided  ! Latent power supplied (kg/s), negative = dehumidification

          ! SUBROUTINE PARAMETER DEFINITIONS:
  INTEGER, PARAMETER :: MaxIter = 25 ! maximum number of iterations for controlling output
  INTEGER, PARAMETER ::    iReverseAction      =1
  INTEGER, PARAMETER ::    iNormalAction       =2
  INTEGER, PARAMETER :: MaxIterCycl = 100


          ! INTERFACE BLOCK SPECIFICATIONS
          ! see use DataInterfaces

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
REAL(r64)    :: QZnReq           ! heating or cooling needed by zone [watts]
REAL(r64)    :: QUnitOut         ! heating or sens. cooling provided by fan coil unit [watts]
REAL(r64)    :: QUnitOutMax      ! heating or sens. cooling provided by fan coil unit (running during an entire timestep)
REAL(r64)    :: PLR              ! Part Load Ratio, fraction of time step fancoil is on
LOGICAL      :: UnitOn           ! TRUE if unit is on
INTEGER      :: ControlNode      ! the hot water or cold water inlet node
REAL(r64)    :: ControlOffset    ! tolerance for output control
REAL(r64)    :: MaxWaterFlow     ! maximum water flow for heating or cooling [kg/sec]
REAL(r64)    :: MinWaterFlow     ! minimum water flow for heating or cooling [kg/sec]
INTEGER      :: OutletNode       ! unit air outlet node
INTEGER      :: InletNode        ! unit air inlet node
REAL(r64)    :: QTotUnitOut      ! total unit output [watts]
REAL(r64)    :: AirMassFlow      ! air mass flow rate [kg/sec]
REAL(r64)    :: QUnitOutNoHC     ! unit output with no active heating or cooling [W]
REAL(r64)    :: QUnitOutMaxHC    ! unit output with full active heating or cooling [W]
REAL(r64)    :: QCoilHeatSP      ! coil load to the heating setpoint [W]
REAL(r64)    :: QCoilCoolSP      ! coil load to the cooling setpoint [W]
REAL(r64)    :: LatentOutput     ! Latent (moisture) add/removal rate, negative is dehumidification [kg/s]
REAL(r64)    :: SpecHumOut       ! Specific humidity ratio of outlet air (kg moisture / kg moist air)
REAL(r64)    :: SpecHumIn        ! Specific humidity ratio of inlet air (kg moisture / kg moist air)
REAL (r64)   :: Error            ! Error between QZnReq and QUnitOut
REAL(r64)    :: AbsError         ! Absolute error between QZnReq and QUnitOut [W]   !FB
INTEGER      :: Iter             ! iteration counter
REAL (r64)   :: Relax
REAL (r64)   :: DelPLR
REAL(r64)    :: mdot

          ! FLOW
FanElecPower = 0.0d0
! initialize local variables
UnitOn = .TRUE.
ControlNode = 0
QUnitOut = 0.0d0
QUnitOutMax = 0.0d0
PLR = 0.0d0
LatentOutput = 0.0d0
QUnitOutNoHC = 0.0d0
QCoilHeatSP = 0.0d0
QCoilCoolSP = 0.0d0
QZnReq = 0.0d0
ControlOffset = 0.0d0
MaxWaterFlow = 0.0d0
MinWaterFlow = 0.0d0
OutletNode = FanCoil(FanCoilNum)%AirOutNode
InletNode = FanCoil(FanCoilNum)%AirInNode
AirMassFlow = Node(InletNode)%MassFlowRate
Error = 1.0d0
AbsError = 2.0d0 * SmallLoad
Iter = 0
Relax = 1.0d0

! select capacity control method
SELECT CASE (FanCoil(FanCoilNum)%CapCtrlMeth_Num)

  ! constant fan variable flow
  CASE(CCM_ConsFanVarFlow)

    IF (AirMassFlow.LT.SmallMassFlow) UnitOn = .FALSE.
    ! zero the hot & cold water flows
!    Node(FanCoil(FanCoilNum)%ColdControlNode)%MassFlowRate = 0.0
!    Node(FanCoil(FanCoilNum)%HotControlNode)%MassFlowRate = 0.0
    mdot = 0.d0
    CALL SetComponentFlowRate(mdot , &
                                FanCoil(FanCoilNum)%ColdControlNode, &
                                FanCoil(FanCoilNum)%ColdPlantOutletNode, &
                                FanCoil(FanCoilNum)%CWLoopNum, &
                                FanCoil(FanCoilNum)%CWLoopSide, &
                                FanCoil(FanCoilNum)%CWBranchNum, &
                                FanCoil(FanCoilNum)%CWCompNum)
    mdot = 0.d0
    CALL SetComponentFlowRate(mdot , &
                                FanCoil(FanCoilNum)%HotControlNode, &
                                FanCoil(FanCoilNum)%HotPlantOutletNode, &
                                FanCoil(FanCoilNum)%HWLoopNum, &
                                FanCoil(FanCoilNum)%HWLoopSide, &
                                FanCoil(FanCoilNum)%HWBranchNum, &
                                FanCoil(FanCoilNum)%HWCompNum)

    ! obtain unit output with no active heating/cooling
    CALL Calc4PipeFanCoil(FanCoilNum,ControlledZoneNum,FirstHVACIteration,QUnitOutNoHC)
    ! get the loads at the coils
    QCoilHeatSP = ZoneSysEnergyDemand(ZoneNum)%RemainingOutputReqToHeatSP - QUnitOutNoHC
    QCoilCoolSP = ZoneSysEnergyDemand(ZoneNum)%RemainingOutputReqToCoolSP - QUnitOutNoHC
    IF (UnitOn .and. QCoilCoolSP < (-1.d0*SmallLoad) .and. TempControlType(ZoneNum) .NE. SingleHeatingSetPoint) THEN
      ! get full load result
      mdot = FanCoil(FanCoilNum)%MaxColdWaterFlow
      CALL SetComponentFlowRate(mdot , &
                                FanCoil(FanCoilNum)%ColdControlNode, &
                                FanCoil(FanCoilNum)%ColdPlantOutletNode, &
                                FanCoil(FanCoilNum)%CWLoopNum, &
                                FanCoil(FanCoilNum)%CWLoopSide, &
                                FanCoil(FanCoilNum)%CWBranchNum, &
                                FanCoil(FanCoilNum)%CWCompNum)
      CALL Calc4PipeFanCoil(FanCoilNum,ControlledZoneNum,FirstHVACIteration,QUnitOutMaxHC)
      IF(QUnitOutMaxHC .LT. QCoilCoolSP)THEN
        ! more cooling than required, find reduced water flow rate to meet the load
        ControlNode = FanCoil(FanCoilNum)%ColdControlNode
        ControlOffset = FanCoil(FanCoilNum)%ColdControlOffset
        MaxWaterFlow = FanCoil(FanCoilNum)%MaxColdWaterFlow
        MinWaterFlow = FanCoil(FanCoilNum)%MinColdWaterFlow
        !On the first HVAC iteration the system values are given to the controller, but after that
        ! the demand limits are in place and there needs to be feedback to the Zone Equipment
        If(.not. FirstHVACIteration) Then
          MaxWaterFlow = Node(ControlNode)%MassFlowRateMaxAvail
          MinWaterFlow = Node(ControlNode)%MassFlowRateMinAvail
        End If
        QZnReq = ZoneSysEnergyDemand(ZoneNum)%RemainingOutputReqToCoolSP
        CALL ControlCompOutput(CompType=cMO_FanCoil,CompNum=FanCoilNum, &
                             FirstHVACIteration=FirstHVACIteration,QZnReq=QZnReq, &
                             ActuatedNode=ControlNode,MaxFlow=MaxWaterFlow, &
                             MinFlow=MinWaterFlow,ControlOffSet=ControlOffset,Action=iReverseAction, &
                             CompName = FanCoil(FanCoilNum)%Name, &
                             ControlCompTypeNum=FanCoil(FanCoilNum)%ControlCompTypeNum, &
                             CompErrIndex=FanCoil(FanCoilNum)%CompErrIndex, &
                             LoopNum     = FanCoil(FanCoilNum)%CWLoopNum, &
                             LoopSide    = FanCoil(FanCoilNum)%CWLoopSide, &
                             BranchIndex = FanCoil(FanCoilNum)%CWBranchNum, &
                             ControlledZoneIndex = ControlledZoneNum)
      END IF
      QUnitOut = AirMassFlow * (PsyHFnTdbW(Node(OutletNode)%Temp,Node(InletNode)%HumRat)  &
                   - PsyHFnTdbW(Node(InletNode)%Temp,Node(InletNode)%HumRat))
    ELSE IF (UnitOn .and. QCoilHeatSP > SmallLoad .and. TempControlType(ZoneNum) .NE. SingleCoolingSetPoint) THEN
      ! get full load result
      mdot = FanCoil(FanCoilNum)%MaxHotWaterFlow
      CALL SetComponentFlowRate(mdot , &
                                FanCoil(FanCoilNum)%HotControlNode, &
                                FanCoil(FanCoilNum)%HotPlantOutletNode, &
                                FanCoil(FanCoilNum)%HWLoopNum, &
                                FanCoil(FanCoilNum)%HWLoopSide, &
                                FanCoil(FanCoilNum)%HWBranchNum, &
                                FanCoil(FanCoilNum)%HWCompNum)
      CALL Calc4PipeFanCoil(FanCoilNum,ControlledZoneNum,FirstHVACIteration,QUnitOutMaxHC)
      IF(QUnitOutMaxHC .GT. QCoilHeatSP)THEN
        ! more heating than required, find reduced water flow rate to meet the load
        ControlNode = FanCoil(FanCoilNum)%HotControlNode
        ControlOffset = FanCoil(FanCoilNum)%HotControlOffset
        MaxWaterFlow = FanCoil(FanCoilNum)%MaxHotWaterFlow
        MinWaterFlow = FanCoil(FanCoilNum)%MinHotWaterFlow
        !On the first HVAC iteration the system values are given to the controller, but after that
        ! the demand limits are in place and there needs to be feedback to the Zone Equipment
        If(.not. FirstHVACIteration) Then
          MaxWaterFlow = Node(ControlNode)%MassFlowRateMaxAvail
          MinWaterFlow = Node(ControlNode)%MassFlowRateMinAvail
        End If
        QZnReq = ZoneSysEnergyDemand(ZoneNum)%RemainingOutputReqToHeatSP
        CALL ControlCompOutput(CompType=cMO_FanCoil,CompNum=FanCoilNum, &
                             FirstHVACIteration=FirstHVACIteration,QZnReq=QZnReq, &
                             ActuatedNode=ControlNode,MaxFlow=MaxWaterFlow, &
                             MinFlow=MinWaterFlow,ControlOffSet=ControlOffset,Action=iNormalAction, &
                             CompName = FanCoil(FanCoilNum)%Name, &
                             ControlCompTypeNum=FanCoil(FanCoilNum)%ControlCompTypeNum, &
                             CompErrIndex=FanCoil(FanCoilNum)%CompErrIndex, &
                             LoopNum     = FanCoil(FanCoilNum)%HWLoopNum, &
                             LoopSide    = FanCoil(FanCoilNum)%HWLoopSide, &
                             BranchIndex = FanCoil(FanCoilNum)%HWBranchNum, &
                             ControlledZoneIndex = ControlledZoneNum)
      ENDIF
      QUnitOut = AirMassFlow * (PsyHFnTdbW(Node(OutletNode)%Temp,Node(InletNode)%HumRat)  &
                   - PsyHFnTdbW(Node(InletNode)%Temp,Node(InletNode)%HumRat))
    ELSE
      ! no action
      QUnitOut = QUnitOutNoHC
    END IF

    ! CR9155 Remove specific humidity calculations
    SpecHumOut = Node(OutletNode)%HumRat
    SpecHumIn  = Node(InletNode)%HumRat
    LatentOutput = AirMassFlow * (SpecHumOut - SpecHumIn) ! Latent rate (kg/s), dehumid = negative
    QTotUnitOut = AirMassFlow * (Node(OutletNode)%Enthalpy - Node(InletNode)%Enthalpy)
    ! report variables
    FanCoil(FanCoilNum)%HeatPower = MAX(0.0d0,QUnitOut)
    FanCoil(FanCoilNum)%SensCoolPower = ABS(MIN(constant_zero,QUnitOut))
    FanCoil(FanCoilNum)%TotCoolPower = ABS(MIN(constant_zero,QTotUnitOut))
    FanCoil(FanCoilNum)%ElecPower = FanElecPower
    PowerMet = QUnitOut
    LatOutputProvided = LatentOutput

  ! cycling fan constant water flow AND VarFanVarFlow
  CASE (CCM_CycFan,CCM_VarFanVarFlow)

    IF (CurDeadbandOrSetback(ZoneNum) .OR. AirMassFlow < SmallMassFlow) UnitOn = .FALSE.

    ! speed fan selection only for multispeed cycling fan
    IF (UnitOn .and. (FanCoil(FanCoilNum)%CapCtrlMeth_Num == CCM_CycFan)) THEN
      QZnReq = ZoneSysEnergyDemand(ZoneNum)%RemainingOutputRequired
      Node(InletNode)%MassFlowRateMax = FanCoil(FanCoilNum)%LowSpeedRatio * FanCoil(FanCoilNum)%MaxAirMassFlow
      CALL Calc4PipeFanCoil (FanCoilNum,ControlledZoneNum,FirstHVACIteration,QUnitOutMax)
      FanCoil(FanCoilNum)%SpeedFanSel = 1
      FanCoil(FanCoilNum)%SpeedFanRatSel = FanCoil(FanCoilNum)%LowSpeedRatio
      IF (ABS(QUnitOutMax) .lt. ABS(QZnReq)) THEN
        Node(InletNode)%MassFlowRateMax = FanCoil(fanCoilNum)%MedSpeedRatio * FanCoil(FanCoilNum)%MaxAirMassFlow
        CALL Calc4PipeFanCoil (FanCoilNum,ControlledZoneNum,FirstHVACIteration,QUnitOutMax)
        FanCoil(FanCoilNum)%SpeedFanSel = 2
        FanCoil(FanCoilNum)%SpeedFanRatSel = FanCoil(FanCoilNum)%MedSpeedRatio
      END IF
      IF (ABS(QUnitOutMax) .lt. ABS(QZnReq)) THEN
        FanCoil(FanCoilNum)%SpeedFanSel = 3
        FanCoil(FanCoilNum)%SpeedFanRatSel = 1.0d0
        Node(InletNode)%MassFlowRateMax = FanCoil(FanCoilNum)%MaxAirMassFlow
      END IF
      ELSE
        FanCoil(FanCoilNum)%SpeedFanSel = 0
      END IF

      !  zero the hot & cold water flows
!      Node(FanCoil(FanCoilNum)%ColdControlNode)%MassFlowRate = 0.0
!      Node(FanCoil(FanCoilNum)%HotControlNode)%MassFlowRate = 0.0
      mdot = 0.d0
      CALL SetComponentFlowRate(mdot , &
                                FanCoil(FanCoilNum)%ColdControlNode, &
                                FanCoil(FanCoilNum)%ColdPlantOutletNode, &
                                FanCoil(FanCoilNum)%CWLoopNum, &
                                FanCoil(FanCoilNum)%CWLoopSide, &
                                FanCoil(FanCoilNum)%CWBranchNum, &
                                FanCoil(FanCoilNum)%CWCompNum)
      mdot = 0.d0
      CALL SetComponentFlowRate(mdot , &
                                FanCoil(FanCoilNum)%HotControlNode, &
                                FanCoil(FanCoilNum)%HotPlantOutletNode, &
                                FanCoil(FanCoilNum)%HWLoopNum, &
                                FanCoil(FanCoilNum)%HWLoopSide, &
                                FanCoil(FanCoilNum)%HWBranchNum, &
                                FanCoil(FanCoilNum)%HWCompNum)

      CALL Calc4PipeFanCoil (FanCoilNum,ControlledZoneNum,FirstHVACIteration,QUnitOutNOHC)

      IF (UnitOn .and. ZoneSysEnergyDemand(ZoneNum)%RemainingOutputReqToCoolSP < (-1.d0*SmallLoad) .and. &
          TempControlType(ZoneNum) .NE. SingleHeatingSetPoint) THEN
        ! cooling coil action, maximum cold water flow
        mdot = FanCoil(FanCoilNum)%MaxColdWaterFlow
        CALL SetComponentFlowRate(mdot , &
                                FanCoil(FanCoilNum)%ColdControlNode, &
                                FanCoil(FanCoilNum)%ColdPlantOutletNode, &
                                FanCoil(FanCoilNum)%CWLoopNum, &
                                FanCoil(FanCoilNum)%CWLoopSide, &
                                FanCoil(FanCoilNum)%CWBranchNum, &
                                FanCoil(FanCoilNum)%CWCompNum)

        QZnReq = ZoneSysEnergyDemand(ZoneNum)%RemainingOutputReqToCoolSP
        ControlOffset = FanCoil(FanCoilNum)%ColdControlOffset

        ! get the maximum output of the fcu
        CALL Calc4PipeFanCoil (FanCoilNum,ControlledZoneNum,FirstHVACIteration,QUnitOutMax)
        ! calculate the PLR, if load greater than output, PLR = 1 (output = max)
        If(QUnitOutMax .Ne. 0.0d0) PLR = ABS(QZnReq/QUnitOutMax)
        if (PLR .gt. 1.0d0) PLR = 1.0d0

        ! adjust the PLR to meet the cooling load calling Calc4PipeFanCoil repeatedly with the PLR adjusted
        do while (ABS(Error) > ControlOffset .and. ABS(AbsError) > SmallLoad .and. Iter < MaxIterCycl .and. PLR.ne.1.0d0 )
          ! the water flow rate is at the maximum flow rate time the PLR
      !    Node(FanCoil(FanCoilNum)%ColdControlNode)%MassFlowRate = PLR * FanCoil(FanCoilNum)%MaxColdWaterFlow
          mdot = PLR * FanCoil(FanCoilNum)%MaxColdWaterFlow
          CALL SetComponentFlowRate(mdot , &
                                FanCoil(FanCoilNum)%ColdControlNode, &
                                FanCoil(FanCoilNum)%ColdPlantOutletNode, &
                                FanCoil(FanCoilNum)%CWLoopNum, &
                                FanCoil(FanCoilNum)%CWLoopSide, &
                                FanCoil(FanCoilNum)%CWBranchNum, &
                                FanCoil(FanCoilNum)%CWCompNum)
          CALL Calc4PipeFanCoil (FanCoilNum,ControlledZoneNum,FirstHVACIteration,QUnitOut, PLR)
          Error = (QZnReq - QUnitOut)/QZnReq
          AbsError = QZnReq - QUnitOut
          DelPLR = (QZnReq-QUnitOut)/QUnitOutMax
          PLR = PLR + Relax * DelPLR
          PLR = MAX(0.0d0,MIN(1.0d0,PLR))
          Iter = Iter + 1
          IF (Iter == 32) Relax = 0.5d0
          IF (Iter == 65) Relax = 0.25d0
        END DO

        ! warning if not converged
        IF (Iter .GT. (MaxIterCycl-1)) THEN
          IF (FanCoil(FanCoilNum)%MaxIterIndexC == 0) THEN
            CALL ShowWarningMessage('ZoneHVAC:FourPipeFanCoil="'//TRIM(FanCoil(FanCoilNum)%Name)//  &
                                    '" -- Exceeded max iterations while adjusting cycling fan'// &
                                    ' sensible runtime to meet the zone load within the cooling convergence tolerance.')
            CALL ShowContinueErrorTimeStamp('Iterations='//TRIM(TrimSigDigits(MaxIterCycl)))
          ENDIF
          CALL ShowRecurringWarningErrorAtEnd('ZoneHVAC:FourPipeFanCoil="'//TRIM(FanCoil(FanCoilNum)%Name)//  &
                     '"  -- Exceeded max iterations error (sensible runtime) continues...',FanCoil(FanCoilNum)%MaxIterIndexC)
        END IF

        ! at the end calculate output with adjusted PLR
        CALL Calc4PipeFanCoil (FanCoilNum,ControlledZoneNum,FirstHVACIteration,QUnitOut, PLR)

      ELSE IF (UnitOn .and. ZoneSysEnergyDemand(ZoneNum)%RemainingOutputReqToHeatSP > SmallLoad .and. &
               TempControlType(ZoneNum) .NE. SingleCoolingSetPoint) THEN
        ! heating coil action, maximun hot water flow
    !    Node(FanCoil(FanCoilNum)%HotControlNode)%MassFlowRate = FanCoil(FanCoilNum)%MaxHotWaterFlow

        mdot = FanCoil(FanCoilNum)%MaxHotWaterFlow
        CALL SetComponentFlowRate(mdot , &
                                FanCoil(FanCoilNum)%HotControlNode, &
                                FanCoil(FanCoilNum)%HotPlantOutletNode, &
                                FanCoil(FanCoilNum)%HWLoopNum, &
                                FanCoil(FanCoilNum)%HWLoopSide, &
                                FanCoil(FanCoilNum)%HWBranchNum, &
                                FanCoil(FanCoilNum)%HWCompNum)

        QZnReq = ZoneSysEnergyDemand(ZoneNum)%RemainingOutputReqToHeatSP
        ControlOffset = FanCoil(FanCoilNum)%HotControlOffset

        ! get the maximum output of the fcu
        CALL Calc4PipeFanCoil (FanCoilNum,ControlledZoneNum,FirstHVACIteration,QUnitOutMax)
        ! calculate the PLR, if load greater than output, PLR = 1 (output = max)
        If(QUnitOutMax .Ne. 0.0d0) THEN
          PLR = ABS (QZnReq/QUnitOutMax)
        ELSE
          PLR = 1.0d0
        ENDIF
        if (PLR .gt. 1.0d0) PLR = 1.0d0

        ! adjust the PLR to meet the heating load calling Calc4PipeFanCoil repeatedly with the PLR adjusted
        do while (ABS(Error) > ControlOffset .and. ABS(AbsError) > SmallLoad .and. Iter < MaxIterCycl .and. PLR.ne.1.0d0 )
          ! the water flow rate is at the maximum flow rate time the PLR
      !    Node(FanCoil(FanCoilNum)%HotControlNode)%MassFlowRate = PLR * FanCoil(FanCoilNum)%MaxHotWaterFlow

          mdot = PLR * FanCoil(FanCoilNum)%MaxHotWaterFlow
          CALL SetComponentFlowRate(mdot , &
                                FanCoil(FanCoilNum)%HotControlNode, &
                                FanCoil(FanCoilNum)%HotPlantOutletNode, &
                                FanCoil(FanCoilNum)%HWLoopNum, &
                                FanCoil(FanCoilNum)%HWLoopSide, &
                                FanCoil(FanCoilNum)%HWBranchNum, &
                                FanCoil(FanCoilNum)%HWCompNum)

          CALL Calc4PipeFanCoil (FanCoilNum,ControlledZoneNum,FirstHVACIteration,QUnitOut, PLR)
          Error = (QZnReq - QUnitOut)/QZnReq
          AbsError = QZnReq - QUnitOut
          DelPLR = (QZnReq-QUnitOut)/QUnitOutMax
          PLR = PLR + Relax * DelPLR
          PLR = MAX(0.0d0,MIN(1.0d0,PLR))
          Iter = Iter + 1
          IF (Iter == 32) Relax = 0.5d0
          IF (Iter == 65) Relax = 0.25d0
        END DO

        ! warning if not converged
        IF (Iter .GT. (MaxIterCycl - 1)) THEN
          IF (FanCoil(FanCoilNum)%MaxIterIndexH == 0) THEN
            CALL ShowWarningMessage('ZoneHVAC:FourPipeFanCoil="'//TRIM(FanCoil(FanCoilNum)%Name)//  &
                                    '" -- Exceeded max iterations while adjusting cycling fan'// &
                                    ' sensible runtime to meet the zone load within the heating convergence tolerance.')
            CALL ShowContinueErrorTimeStamp('Iterations='//TRIM(TrimSigDigits(MaxIterCycl)))
          ENDIF
          CALL ShowRecurringWarningErrorAtEnd('ZoneHVAC:FourPipeFanCoil="'//TRIM(FanCoil(FanCoilNum)%Name)//  &
                      '"  -- Exceeded max iterations error (sensible runtime) continues...',FanCoil(FanCoilNum)%MaxIterIndexH)
        END IF

        ! at the end calculate output with adjusted PLR
        CALL Calc4PipeFanCoil (FanCoilNum,ControlledZoneNum,FirstHVACIteration,QUnitOut, PLR)

        !this part of the code is just if we want ventilation in the deadband zone
        !ELSE IF (AirMassFlow .gt. 0.0d0) THEN
        ! if fan scheduled available : just ventilation, PLR = 1
        !QUnitOut = QUnitOutNOHC
        !PLR = 1.

      ELSE
        ! no action, zero the air flow rate, the unit is off
        Node(InletNode)%MassFlowRate = 0.0d0
        Node(OutletNode)%MassFlowRate = 0.0d0
        FanCoil(FanCoilNum)%SpeedFanSel = 0
        PLR = 0.0d0
        CALL Calc4PipeFanCoil (FanCoilNum,ControlledZoneNum,FirstHVACIteration,QUnitOut,PLR)
      END IF

      AirMassFlow = Node(InletNode)%MassFlowRate
      ! CR9155 Remove specific humidity calculations
      SpecHumOut = Node(OutletNode)%HumRat
      SpecHumIn  = Node(InletNode)%HumRat
      LatentOutput = AirMassFlow * (SpecHumOut - SpecHumIn) ! Latent rate (kg/s), dehumid = negative
      QTotUnitOut = AirMassFlow * (Node(OutletNode)%Enthalpy - Node(InletNode)%Enthalpy)
      ! report variables
      FanCoil(FanCoilNum)%HeatPower = MAX(0.0d0,QUnitOut)
      FanCoil(FanCoilNum)%SensCoolPower = ABS(MIN(constant_zero,QUnitOut))
      FanCoil(FanCoilNum)%TotCoolPower = ABS(MIN(constant_zero,QTotUnitOut))
      FanCoil(FanCoilNum)%ElecPower = FanElecPower
      FanCoil(FanCoilNum)%PLR = PLR
      PowerMet = QUnitOut
      LatOutputProvided = LatentOutput

  ! cycling fan constant water flow AND VarFanVarFlow
  CASE (CCM_VarFanConsFlow)

    IF (CurDeadbandOrSetback(ZoneNum) .OR. AirMassFlow < SmallMassFlow) UnitOn = .FALSE.

    !  zero the hot & cold water flows
!    Node(FanCoil(FanCoilNum)%ColdControlNode)%MassFlowRate = 0.0
!    Node(FanCoil(FanCoilNum)%HotControlNode)%MassFlowRate = 0.0
    mdot = 0.d0
    CALL SetComponentFlowRate(mdot , &
                                FanCoil(FanCoilNum)%ColdControlNode, &
                                FanCoil(FanCoilNum)%ColdPlantOutletNode, &
                                FanCoil(FanCoilNum)%CWLoopNum, &
                                FanCoil(FanCoilNum)%CWLoopSide, &
                                FanCoil(FanCoilNum)%CWBranchNum, &
                                FanCoil(FanCoilNum)%CWCompNum)
    mdot = 0.d0
    CALL SetComponentFlowRate(mdot , &
                                FanCoil(FanCoilNum)%HotControlNode, &
                                FanCoil(FanCoilNum)%HotPlantOutletNode, &
                                FanCoil(FanCoilNum)%HWLoopNum, &
                                FanCoil(FanCoilNum)%HWLoopSide, &
                                FanCoil(FanCoilNum)%HWBranchNum, &
                                FanCoil(FanCoilNum)%HWCompNum)

    CALL Calc4PipeFanCoil (FanCoilNum,ControlledZoneNum,FirstHVACIteration,QUnitOutNOHC)

    IF (UnitOn .and. ZoneSysEnergyDemand(ZoneNum)%RemainingOutputReqToCoolSP < (-1.d0*SmallLoad) .and. &
        TempControlType(ZoneNum) .NE. SingleHeatingSetPoint) THEN
      ! cooling coil action, maximum cold water flow
      mdot = FanCoil(FanCoilNum)%MaxColdWaterFlow
      CALL SetComponentFlowRate(mdot , &
                                FanCoil(FanCoilNum)%ColdControlNode, &
                                FanCoil(FanCoilNum)%ColdPlantOutletNode, &
                                FanCoil(FanCoilNum)%CWLoopNum, &
                                FanCoil(FanCoilNum)%CWLoopSide, &
                                FanCoil(FanCoilNum)%CWBranchNum, &
                                FanCoil(FanCoilNum)%CWCompNum)
      QZnReq = ZoneSysEnergyDemand(ZoneNum)%RemainingOutputReqToCoolSP
      ControlOffset = FanCoil(FanCoilNum)%ColdControlOffset

      ! get the maximum output of the fcu
      CALL Calc4PipeFanCoil (FanCoilNum,ControlledZoneNum,FirstHVACIteration,QUnitOutMax)
      ! calculate the PLR, if load greater than output, PLR = 1 (output = max)
      If(QUnitOutMax .Ne. 0.0d0) PLR = ABS(QZnReq/QUnitOutMax)
      if (PLR .gt. 1.0d0) PLR = 1.0d0

      ! adjust the PLR to meet the cooling load calling Calc4PipeFanCoil repeatedly with the PLR adjusted
      do while (ABS(Error) > ControlOffset .and. ABS(AbsError) > SmallLoad .and. Iter < MaxIterCycl .and. PLR.ne.1.0d0 )
        CALL Calc4PipeFanCoil (FanCoilNum,ControlledZoneNum,FirstHVACIteration,QUnitOut, PLR)
        Error = (QZnReq - QUnitOut)/QZnReq
        AbsError = QZnReq - QUnitOut
        DelPLR = (QZnReq-QUnitOut)/QUnitOutMax
        PLR = PLR + Relax * DelPLR
        PLR = MAX(0.0d0,MIN(1.0d0,PLR))
        Iter = Iter + 1
        IF (Iter == 32) Relax = 0.5d0
        IF (Iter == 65) Relax = 0.25d0
      END DO

      ! warning if not converged
      IF (Iter .GT. (MaxIterCycl-1)) THEN
        IF (FanCoil(FanCoilNum)%MaxIterIndexC == 0) THEN
          CALL ShowWarningMessage('ZoneHVAC:FourPipeFanCoil="'//TRIM(FanCoil(FanCoilNum)%Name)//  &
                                  '" -- Exceeded max iterations while adjusting cycling fan'// &
                                  ' sensible runtime to meet the zone load within the cooling convergence tolerance.')
          CALL ShowContinueErrorTimeStamp('Iterations='//TRIM(TrimSigDigits(MaxIterCycl)))
        ENDIF
        CALL ShowRecurringWarningErrorAtEnd('ZoneHVAC:FourPipeFanCoil="'//TRIM(FanCoil(FanCoilNum)%Name)//  &
                 '"  -- Exceeded max iterations error (sensible runtime) continues...',FanCoil(FanCoilNum)%MaxIterIndexC)
      END IF

      ! at the end calculate output with adjusted PLR
      CALL Calc4PipeFanCoil (FanCoilNum,ControlledZoneNum,FirstHVACIteration,QUnitOut, PLR)

    ELSE IF (UnitOn .and. ZoneSysEnergyDemand(ZoneNum)%RemainingOutputReqToHeatSP > SmallLoad .and. &
             TempControlType(ZoneNum) .NE. SingleCoolingSetPoint) THEN
      ! heating coil action, maximun hot water flow
      mdot = FanCoil(FanCoilNum)%MaxHotWaterFlow
      CALL SetComponentFlowRate(mdot , &
                                FanCoil(FanCoilNum)%HotControlNode, &
                                FanCoil(FanCoilNum)%HotPlantOutletNode, &
                                FanCoil(FanCoilNum)%HWLoopNum, &
                                FanCoil(FanCoilNum)%HWLoopSide, &
                                FanCoil(FanCoilNum)%HWBranchNum, &
                                FanCoil(FanCoilNum)%HWCompNum)
      QZnReq = ZoneSysEnergyDemand(ZoneNum)%RemainingOutputReqToHeatSP
      ControlOffset = FanCoil(FanCoilNum)%HotControlOffset

      ! get the maximum output of the fcu
      CALL Calc4PipeFanCoil (FanCoilNum,ControlledZoneNum,FirstHVACIteration,QUnitOutMax)
      ! calculate the PLR, if load greater than output, PLR = 1 (output = max)
      If(QUnitOutMax .Ne. 0.0d0) PLR = ABS (QZnReq/QUnitOutMax)
      if (PLR .gt. 1.0d0) PLR = 1.0d0

      ! adjust the PLR to meet the heating load calling Calc4PipeFanCoil repeatedly with the PLR adjusted
      do while (ABS(Error) > ControlOffset .and. ABS(AbsError) > SmallLoad .and. Iter < MaxIterCycl .and. PLR.ne.1.0d0 )
        CALL Calc4PipeFanCoil (FanCoilNum,ControlledZoneNum,FirstHVACIteration,QUnitOut, PLR)
        Error = (QZnReq - QUnitOut)/QZnReq
        AbsError = QZnReq - QUnitOut
        DelPLR = (QZnReq-QUnitOut)/QUnitOutMax
        PLR = PLR + Relax * DelPLR
        PLR = MAX(0.0d0,MIN(1.0d0,PLR))
        Iter = Iter + 1
        IF (Iter == 32) Relax = 0.5d0
        IF (Iter == 65) Relax = 0.25d0
      END DO

      ! warning if not converged
      IF (Iter .GT. (MaxIterCycl - 1)) THEN
        IF (FanCoil(FanCoilNum)%MaxIterIndexH == 0) THEN
          CALL ShowWarningMessage('ZoneHVAC:FourPipeFanCoil="'//TRIM(FanCoil(FanCoilNum)%Name)//  &
                                  '" -- Exceeded max iterations while adjusting cycling fan'// &
                                  ' sensible runtime to meet the zone load within the heating convergence tolerance.')
          CALL ShowContinueErrorTimeStamp('Iterations='//TRIM(TrimSigDigits(MaxIterCycl)))
        ENDIF
        CALL ShowRecurringWarningErrorAtEnd('ZoneHVAC:FourPipeFanCoil="'//TRIM(FanCoil(FanCoilNum)%Name)//  &
                  '"  -- Exceeded max iterations error (sensible runtime) continues...',FanCoil(FanCoilNum)%MaxIterIndexH)
      END IF

      ! at the end calculate output with adjusted PLR
      CALL Calc4PipeFanCoil (FanCoilNum,ControlledZoneNum,FirstHVACIteration,QUnitOut, PLR)

      !this part of the code is just if we want ventilation in the deadband zone
      !ELSE IF (AirMassFlow .gt. 0.0d0) THEN
      ! if fan scheduled available : just ventilation, PLR = 1
      !QUnitOut = QUnitOutNOHC
      !PLR = 1.

    ELSE
      ! no action, zero the air flow rate, the unit is off
      Node(InletNode)%MassFlowRate = 0.0d0
      Node(OutletNode)%MassFlowRate = 0.0d0
      FanCoil(FanCoilNum)%SpeedFanSel = 0
      PLR = 0.0d0
      CALL Calc4PipeFanCoil (FanCoilNum,ControlledZoneNum,FirstHVACIteration,QUnitOut,PLR)
    END IF

    AirMassFlow = Node(InletNode)%MassFlowRate
    ! CR9155 Remove specific humidity calculations
    SpecHumOut = Node(OutletNode)%HumRat
    SpecHumIn  = Node(InletNode)%HumRat
    LatentOutput = AirMassFlow * (SpecHumOut - SpecHumIn) ! Latent rate (kg/s), dehumid = negative

    QTotUnitOut = AirMassFlow * (Node(OutletNode)%Enthalpy - Node(InletNode)%Enthalpy)
    ! report variables
    FanCoil(FanCoilNum)%HeatPower = MAX(0.0d0,QUnitOut)
    FanCoil(FanCoilNum)%SensCoolPower = ABS(MIN(constant_zero,QUnitOut))
    FanCoil(FanCoilNum)%TotCoolPower = ABS(MIN(constant_zero,QTotUnitOut))
    FanCoil(FanCoilNum)%ElecPower = FanElecPower
    FanCoil(FanCoilNum)%PLR = PLR
    PowerMet = QUnitOut
    LatOutputProvided = LatentOutput

END SELECT

RETURN
END SUBROUTINE Sim4PipeFanCoil

SUBROUTINE Calc4PipeFanCoil(FanCoilNum,ControlledZoneNum,FirstHVACIteration,LoadMet,PLR)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   March 2000
          !       MODIFIED       July 2012, Chandan Sharma - FSEC: Added zone sys avail managers
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Simulate the components making up the 4 pipe fan coil unit.

          ! METHODOLOGY EMPLOYED:
          ! Simulates the unit components sequentially in the air flow direction.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
USE MixedAir,            ONLY: SimOAMixer
USE SingleDuct,          ONLY: SimATMixer
USE Fans,                ONLY: SimulateFanComponents
USE WaterCoils,          ONLY: SimulateWaterCoilComponents
USE HVACHXAssistedCoolingCoil, ONLY: SimHXAssistedCoolingCoil
USE Psychrometrics,      ONLY: PsyHFnTdbW
USE DataHVACGlobals,     ONLY: ZoneCompTurnFansOn, ZoneCompTurnFansOff
USE DataZoneEquipment,   ONLY: ZoneEquipConfig

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN)  :: FanCoilNum         ! Unit index in fan coil array
  INTEGER, INTENT (IN)  :: ControlledZoneNum  ! ZoneEquipConfig index
  LOGICAL, INTENT (IN)  :: FirstHVACIteration ! flag for 1st HVAV iteration in the time step
  REAL(r64), INTENT (OUT) :: LoadMet          ! load met by unit (watts)
  REAL(r64), INTENT (INOUT), OPTIONAL :: PLR  ! Part Load Ratio, fraction of time step fancoil is on

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
INTEGER :: OutletNode       ! unit air outlet node
INTEGER :: InletNode        ! unit air inlet node
INTEGER :: ATMixOutNode = 0 ! outlet node of ATM Mixer
INTEGER :: ZoneNode = 0     ! zone node
REAL(r64)    :: AirMassFlow      ! total mass flow through the unit
REAL (r64)   :: PartLoad         ! if PLR present PartLoad = PLR
REAL (r64)   :: OASchedValue     ! value of OASchedValue, =1 if not schedule
          ! FLOW

! if PLR present in arguments, get its value, else default PLR = 1
IF (PRESENT(PLR)) THEN
  PartLoad = PLR
ELSE
  PartLoad = 1.0d0
END IF

OutletNode = FanCoil(FanCoilNum)%AirOutNode
InletNode = FanCoil(FanCoilNum)%AirInNode
ZoneNode = ZoneEquipConfig(ControlledZoneNum)%ZoneNode

! Assume the unit is able to vary the flow. A cycling unit is treated as
! if it were variable flow, with the flow being the averaqe flow over the time step
IF (GetCurrentScheduleValue(FanCoil(FanCoilNum)%SchedPtr) .gt. 0.0d0)   &
     Node(InletNode)%MassFlowRate = PartLoad * Node(InletNode)%MassFlowRateMax

! use the value of the outside air schedule if present
IF (FanCoil(FanCoilNum)%SchedOutAirPtr > 0) THEN
  OASchedValue = GetCurrentScheduleValue(FanCoil(FanCoilNum)%SchedOutAirPtr)
ELSE
  OASchedValue = 1.0D0
END IF

IF (FanCoil(FanCoilNum)%ATMixerExists) THEN
  ATMixOutNode = FanCoil(FanCoilNum)%ATMixerOutNode
  IF (FanCoil(FanCoilNum)%ATMixerType == ATMixer_InletSide) THEN
    ! set the primary air inlet mass flow rate
    Node(FanCoil(FanCoilNum)%ATMixerPriNode)%MassFlowRate = MIN(Node(FanCoil(FanCoilNum)%ATMixerPriNode)%MassFlowRateMaxAvail, &
                                                              Node(InletNode)%MassFlowRate)
    ! now calculate the the mixer outlet conditions (and the secondary air inlet flow rate)
    ! the mixer outlet flow rate has already been set above (it is the "inlet" node flow rate)
    CALL SimATMixer(FanCoil(FanCoilNum)%ATMixerName,FirstHVACIteration,FanCoil(FanCoilNum)%ATMixerIndex)
  END IF
  AirMassFlow = Node(InletNode)%MassFlowRate
ELSE
  ! OutdoorAir:Mixer
  IF (FanCoil(FanCoilNum)%CapCtrlMeth_Num .eq. CCM_CycFan) THEN
    Node(FanCoil(FanCoilNum)%OutsideAirNode)%MassFlowRate =   &
        MIN(OASchedValue * Node(FanCoil(FanCoilNum)%OutsideAirNode)%MassFlowRateMax * &
        PartLoad * FanCoil(FanCoilNum)%SpeedFanRatSel,Node(InletNode)%MassFlowRate)
  ELSE
    Node(FanCoil(FanCoilNum)%OutsideAirNode)%MassFlowRate =   &
        MIN(OASchedValue * Node(FanCoil(FanCoilNum)%OutsideAirNode)%MassFlowRateMax * &
        PartLoad, Node(InletNode)%MassFlowRate)
  END IF
  Node(FanCoil(FanCoilNum)%AirReliefNode)%MassFlowRate = Node(FanCoil(FanCoilNum)%OutsideAirNode)%MassFlowRate
  AirMassFlow = Node(InletNode)%MassFlowRate
  CALL SimOAMixer(FanCoil(FanCoilNum)%OAMixName,FirstHVACIteration,FanCoil(FanCoilNum)%OAMixIndex)
END IF

IF(FanCoil(FanCoilNum)%CapCtrlMeth_Num .eq. CCM_CycFan)THEN
  ! cycling fan coil unit calculation
  IF (FanCoil(FanCoilNum)%SpeedFanSel .eq. 1)THEN
    CALL SimulateFanComponents(FanCoil(FanCoilNum)%FanName,FirstHVACIteration,  &
         FanCoil(FanCoilNum)%FanIndex,FanCoil(FanCoilNum)%LowSpeedRatio, &
         ZoneCompTurnFansOn, ZoneCompTurnFansOff)
  ELSE IF (FanCoil(FanCoilNum)%SpeedFanSel .eq. 2)THEN
    CALL SimulateFanComponents(FanCoil(FanCoilNum)%FanName,FirstHVACIteration,  &
         FanCoil(FanCoilNum)%FanIndex,FanCoil(FanCoilNum)%MedSpeedRatio, &
         ZoneCompTurnFansOn, ZoneCompTurnFansOff)
  ELSE
    CALL SimulateFanComponents(FanCoil(FanCoilNum)%FanName,FirstHVACIteration,  &
         FanCoil(FanCoilNum)%FanIndex, 1.0d0, ZoneCompTurnFansOn, ZoneCompTurnFansOff)
  END IF
  IF(FanCoil(FanCoilNum)%CCoilType_Num == CCoil_HXAssist) THEN
    CALL SimHXAssistedCoolingCoil(FanCoil(FanCoilNum)%CCoilName,FirstHVACIteration,On,  &
                                  0.0d0,FanCoil(FanCoilNum)%CCoilName_Index,ContFanCycCoil)
  ELSE
    CALL SimulateWaterCoilComponents(FanCoil(FanCoilNum)%CCoilName,FirstHVACIteration,&
                                     FanCoil(FanCoilNum)%CCoilName_Index,FanOpMode = 1,PartLoadRatio = PLR)
  END IF
  CALL SimulateWaterCoilComponents(FanCoil(FanCoilNum)%HCoilName,FirstHVACIteration,&
                                   FanCoil(FanCoilNum)%HCoilName_Index,FanOpMode = 1,PartLoadRatio = PLR)

ELSE
  ! Constant fan and variable flow calculation AND variable fan
  CALL SimulateFanComponents(FanCoil(FanCoilNum)%FanName,FirstHVACIteration,FanCoil(FanCoilNum)%FanIndex, &
                               ZoneCompTurnFansOn = ZoneCompTurnFansOn,ZoneCompTurnFansOff = ZoneCompTurnFansOff)
  IF(FanCoil(FanCoilNum)%CCoilType_Num == CCoil_HXAssist) THEN
    CALL SimHXAssistedCoolingCoil(FanCoil(FanCoilNum)%CCoilName,FirstHVACIteration,On,  &
                                   0.0d0,FanCoil(FanCoilNum)%CCoilName_Index,ContFanCycCoil)
  ELSE
    CALL SimulateWaterCoilComponents(FanCoil(FanCoilNum)%CCoilName,FirstHVACIteration,FanCoil(FanCoilNum)%CCoilName_Index)
  END IF
  CALL SimulateWaterCoilComponents(FanCoil(FanCoilNum)%HCoilName,FirstHVACIteration,FanCoil(FanCoilNum)%HCoilName_Index)

END IF

IF (FanCoil(FanCoilNum)%ATMixerExists) THEN
  IF (FanCoil(FanCoilNum)%ATMixerType == ATMixer_SupplySide) THEN
    ! Now calculate the ATM mixer if it is on the supply side of the zone unit
    CALL SimATMixer(FanCoil(FanCoilNum)%ATMixerName,FirstHVACIteration,FanCoil(FanCoilNum)%ATMixerIndex)
  END IF
END IF

IF (FanCoil(FanCoilNum)%ATMixerExists) THEN
  IF (FanCoil(FanCoilNum)%ATMixerType == ATMixer_SupplySide) THEN
    LoadMet = Node(ATMixOutNode)%MassFlowRate * (PsyHFnTdbW(Node(ATMixOutNode)%Temp,Node(ZoneNode)%HumRat) &
                                                 - PsyHFnTdbW(Node(ZoneNode)%Temp,Node(ZoneNode)%HumRat))
  ELSE
    ! ATM Mixer on inlet side
    LoadMet = AirMassFlow * (PsyHFnTdbW(Node(OutletNode)%Temp,Node(ZoneNode)%HumRat)  &
                       - PsyHFnTdbW(Node(ZoneNode)%Temp,Node(ZoneNode)%HumRat))
  END IF
ELSE
  LoadMet = AirMassFlow * (PsyHFnTdbW(Node(OutletNode)%Temp,Node(InletNode)%HumRat)  &
                       - PsyHFnTdbW(Node(InletNode)%Temp,Node(InletNode)%HumRat))
END IF

RETURN
END SUBROUTINE Calc4PipeFanCoil


SUBROUTINE ReportFanCoilUnit(FanCoilNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   March 2000
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Fills some of the report variables for the fan coil units

          ! METHODOLOGY EMPLOYED:
          ! NA

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  Use DataHVACGlobals, ONLY: TimeStepSys

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN) :: FanCoilNum ! number of the current fan coil unit being simulated

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
 REAL(r64) :: ReportingConstant

          ! FLOW
ReportingConstant=TimeStepSys * SecInHour
FanCoil(FanCoilNum)%HeatEnergy = FanCoil(FanCoilNum)%HeatPower * ReportingConstant
FanCoil(FanCoilNum)%SensCoolEnergy = FanCoil(FanCoilNum)%SensCoolPower * ReportingConstant
FanCoil(FanCoilNum)%TotCoolEnergy = FanCoil(FanCoilNum)%TotCoolPower * ReportingConstant
FanCoil(FanCoilNum)%ElecEnergy = FanCoil(FanCoilNum)%ElecPower * ReportingConstant

RETURN
END SUBROUTINE ReportFanCoilUnit

INTEGER FUNCTION GetFanCoilZoneInletAirNode(FanCoilNum)

          ! FUNCTION INFORMATION:
          !       AUTHOR         B Griffith
          !       DATE WRITTEN   Dec  2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! lookup function for OA inlet node for ventilation rate reporting

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN)  :: FanCoilNum          !

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
          ! na

  IF (GetFanCoilInputFlag) THEN
    CALL GetFanCoilUnits
    GetFanCoilInputFlag = .FALSE.
  END IF

  GetFanCoilZoneInletAirNode = 0
  IF (FanCoilNum > 0 .and. FanCoilNum <= NumFanCoils) THEN
    GetFanCoilZoneInletAirNode = FanCoil(FanCoilNum)%AirOutNode
  ENDIF

  RETURN

END FUNCTION GetFanCoilZoneInletAirNode


INTEGER FUNCTION GetFanCoilOutAirNode(FanCoilNum)

          ! FUNCTION INFORMATION:
          !       AUTHOR         B Griffith
          !       DATE WRITTEN   Dec  2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! lookup function for OA inlet node for ventilation rate reporting

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN)  :: FanCoilNum          !

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
          ! na

  IF (GetFanCoilInputFlag) THEN
    CALL GetFanCoilUnits
    GetFanCoilInputFlag = .FALSE.
  END IF

  GetFanCoilOutAirNode = 0
  IF (FanCoilNum > 0 .and. FanCoilNum <= NumFanCoils) THEN
    GetFanCoilOutAirNode = FanCoil(FanCoilNum)%OutsideAirNode
  ENDIF

  RETURN

END FUNCTION GetFanCoilOutAirNode

INTEGER FUNCTION GetFanCoilReturnAirNode(FanCoilNum)

          ! FUNCTION INFORMATION:
          !       AUTHOR         B Griffith
          !       DATE WRITTEN   Dec  2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! lookup function for mixer's return node

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE MixedAir, ONLY: GetOAMixerReturnNodeNumber

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN)  :: FanCoilNum          !

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
          ! na

  IF (GetFanCoilInputFlag) THEN
    CALL GetFanCoilUnits
    GetFanCoilInputFlag = .FALSE.
  END IF

  GetFanCoilReturnAirNode = 0
  IF (FanCoilNum > 0 .and. FanCoilNum <= NumFanCoils) THEN
    IF (FanCoil(FanCoilNum)%OAMixIndex > 0)  THEN
      GetFanCoilReturnAirNode = GetOAMixerReturnNodeNumber(FanCoil(FanCoilNum)%OAMixIndex)
    ELSE
      GetFanCoilReturnAirNode = 0
    END IF
  ENDIF

  RETURN

END FUNCTION GetFanCoilReturnAirNode

INTEGER FUNCTION GetFanCoilMixedAirNode(FanCoilNum)

          ! FUNCTION INFORMATION:
          !       AUTHOR         B Griffith
          !       DATE WRITTEN   Dec  2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! lookup function for mixer's return node

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE MixedAir, ONLY: GetOAMixerMixedNodeNumber

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN)  :: FanCoilNum          !

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
          ! na

  IF (GetFanCoilInputFlag) THEN
    CALL GetFanCoilUnits
    GetFanCoilInputFlag = .FALSE.
  END IF

  GetFanCoilMixedAirNode = 0
  IF (FanCoilNum > 0 .and. FanCoilNum <= NumFanCoils) THEN
    IF (FanCoil(FanCoilNum)%OAMixIndex > 0)  THEN
      GetFanCoilMixedAirNode = GetOAMixerMixedNodeNumber(FanCoil(FanCoilNum)%OAMixIndex)
    ELSE
      GetFanCoilMixedAirNode = 0
    END IF
  ENDIF

  RETURN

END FUNCTION GetFanCoilMixedAirNode

INTEGER FUNCTION GetFanCoilInletAirNode(FanCoilNum)

          ! FUNCTION INFORMATION:
          !       AUTHOR         B Griffith
          !       DATE WRITTEN   Dec  2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! lookup function for inlet node for Fan Coil unit

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN)  :: FanCoilNum          !

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
          ! na

  IF (GetFanCoilInputFlag) THEN
    CALL GetFanCoilUnits
    GetFanCoilInputFlag = .FALSE.
  END IF

  GetFanCoilInletAirNode = 0
  IF (FanCoilNum > 0 .and. FanCoilNum <= NumFanCoils) THEN
    GetFanCoilInletAirNode = FanCoil(FanCoilNum)%AirOutNode
  ENDIF

  RETURN

END FUNCTION GetFanCoilInletAirNode

SUBROUTINE GetFanCoilIndex(FanCoilName,FanCoilIndex)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR
          !       DATE WRITTEN   April 2012
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine gets the index for a given PT Unit

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: FindItemInList

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: FanCoilName
  INTEGER, INTENT(INOUT)       :: FanCoilIndex

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

  LOGICAL ErrorsFound ! for error trapping

  IF (GetFanCoilInputFlag) THEN
    CALL GetFanCoilUnits
    GetFanCoilInputFlag = .FALSE.
  END IF

  FanCoilIndex = FindItemInList(FanCoilName,FanCoil%Name,NumFanCoils)
  IF (FanCoilIndex == 0) THEN
      CALL ShowSevereError('GetFanCoilIndex: Fan Coil Unit not found='//TRIM(FanCoilName))
  ENDIF
  ErrorsFound=.TRUE.

  RETURN

END SUBROUTINE GetFanCoilIndex

!     NOTICE
!
!     Copyright © 1996-2013 The Board of Trustees of the University of Illinois
!     and The Regents of the University of California through Ernest Orlando Lawrence
!     Berkeley National Laboratory.  All rights reserved.
!
!     Portions of the EnergyPlus software package have been developed and copyrighted
!     by other individuals, companies and institutions.  These portions have been
!     incorporated into the EnergyPlus software package under license.   For a complete
!     list of contributors, see "Notice" located in EnergyPlus.f90.
!
!     NOTICE: The U.S. Government is granted for itself and others acting on its
!     behalf a paid-up, nonexclusive, irrevocable, worldwide license in this data to
!     reproduce, prepare derivative works, and perform publicly and display publicly.
!     Beginning five (5) years after permission to assert copyright is granted,
!     subject to two possible five year renewals, the U.S. Government is granted for
!     itself and others acting on its behalf a paid-up, non-exclusive, irrevocable
!     worldwide license in this data to reproduce, prepare derivative works,
!     distribute copies to the public, perform publicly and display publicly, and to
!     permit others to do so.
!
!     TRADEMARKS: EnergyPlus is a trademark of the US Department of Energy.
!

END MODULE FanCoilUnits
