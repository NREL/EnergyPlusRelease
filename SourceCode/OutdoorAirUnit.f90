MODULE OutdoorAirUnit
  ! Module containing the routines dealing with the outdoor air unit

  ! MODULE INFORMATION:
  !       AUTHOR         Young Tae Chae, Rick Strand
  !       DATE WRITTEN   AUG. 2009
  !       MODIFIED
  !                      Feb 2013 Bereket Nigusse, FSEC
  !                        Added DX Coil Model For 100% OA systems
  !       RE-ENGINEERED  na

  ! PURPOSE OF THIS MODULE:
  ! Simulate zone outdooor air unit.

  ! METHODOLOGY EMPLOYED:
  ! Systems are modeled as a collection of components:
  ! fan, heat recovery, dehumidifier, heating coil and/or cooling coil plus an integrated control
  ! algorithm that adjusts the hot or cold water flow to meet the setpoint
  ! condition.

  ! REFERENCES:
  ! OTHER NOTES: none

  ! USE STATEMENTS:
  ! Use statements for data only modules
USE DataLoopNode
USE DataGlobals,     ONLY: BeginEnvrnFlag, BeginDayFlag, BeginTimeStepFlag, MaxNameLength, &
                           InitConvTemp, ZoneSizingCalc, SysSizingCalc, WarmUpFlag, DisplayExtraWarnings
USE DataInterfaces
USE DataHVACGlobals,   ONLY: FanElecPower, SmallLoad,SmallAirVolFlow, ContFanCycCoil,smallmassflow,DrawThru, BlowThru

  ! Use statements for access to subroutines in other modules
USE ScheduleManager
USE Psychrometrics
Use FluidProperties
USE General, ONLY: TrimSigDigits
IMPLICIT NONE         ! Enforce explicit typing of all variables

PRIVATE ! Everything private unless explicitly made public

  ! MODULE PARAMETER DEFINITIONS

! component types addressed by this module
CHARACTER(len=*), PARAMETER :: cMO_OutdoorAirUnit='ZoneHVAC:OutdoorAirUnit'

INTEGER, PARAMETER :: WaterCoil_SimpleCool    = 1
INTEGER, PARAMETER :: WaterCoil_Cooling       = 2
INTEGER, PARAMETER :: WaterCoil_SimpleHeat    = 3
INTEGER, PARAMETER :: SteamCoil_AirHeat       = 4
INTEGER, PARAMETER :: WaterCoil_DetailedCool  = 5
INTEGER, PARAMETER :: WaterCoil_CoolingHXAsst = 6
INTEGER, PARAMETER :: Coil_ElectricHeat       = 7
INTEGER, PARAMETER :: Coil_GasHeat            = 8
INTEGER, PARAMETER :: DXSystem                = 9
INTEGER, PARAMETER :: HeatXchngr              = 10
INTEGER, PARAMETER :: Desiccant               = 11
INTEGER, PARAMETER :: DXHeatPumpSystem        = 12
INTEGER, PARAMETER :: UnitarySystem           = 13

!  Control Types
INTEGER, PARAMETER :: Neutral        = 1  ! Controls system using zone mean air temperature
INTEGER, PARAMETER :: Unconditioned  = 2  ! Controls system when outdoor air temperature is identified with control temperature
INTEGER, PARAMETER :: Temperature    = 3  ! Controls system using temperature band

! Operating Options
INTEGER, PARAMETER :: HeatingMode =  1              ! normal heating coil operation
INTEGER, PARAMETER :: CoolingMode =  2              ! normal cooling coil operation
INTEGER, PARAMETER :: NeutralMode =  3              ! signal coil shouldn't run

Character(len=*), PARAMETER, DIMENSION(2) :: CurrentModuleObjects=  &
  (/'ZoneHVAC:OutdoorAirUnit                 ',  &
    'ZoneHVAC:OutdoorAirUnit:EquipmentList   ' /)

! Parameters below (CO - Current module Object.  used primarily in Get Inputs)
! Multiple Get Input routines in this module or these would be in individual routines.
INTEGER, PARAMETER :: CO_OAUnit = 1
INTEGER, PARAMETER :: CO_OAEqList = 2


  ! DERIVED TYPE DEFINITIONS
TYPE OAEquipList
  ! Equipment List Data
    CHARACTER(len=MaxNameLength) :: ComponentName      =' '
    CHARACTER(len=MaxNameLength) :: ComponentType      =' '
    INTEGER                      :: ComponentType_Num  = 0   ! Parameterized Component Types this module can address
    INTEGER                      :: ComponentIndex     = 0   ! Which one in list -- updated by routines called from here
    INTEGER                      :: CoilAirInletNode   = 0
    INTEGER                      :: CoilAirOutletNode  = 0
    INTEGER                      :: CoilWaterInletNode = 0
    INTEGER                      :: CoilWaterOutletNode= 0
    INTEGER                      :: CoilPlantTypeOfNum = 0
    INTEGER                      :: LoopNum = 0
    INTEGER                      :: LoopSideNum = 0
    INTEGER                      :: BranchNum = 0
    INTEGER                      :: CompNum = 0
    INTEGER                      :: FluidIndex         =0        ! used in Steam...
    REAL(r64)                    :: MaxVolWaterFlow    = 0.0d0
    REAL(r64)                    :: MaxWaterMassFlow   = 0.d0
    REAL(r64)                    :: MinVolWaterFlow    = 0.0d0
    REAL(r64)                    :: MinWaterMassFlow   = 0.d0
  ! End Of Equipment list data
END TYPE

TYPE OAUnitData
  ! Input data
  CHARACTER(len=MaxNameLength) :: Name                     =' '     ! name of unit
  CHARACTER(len=MaxNameLength) :: SchedName                =' '     ! availability schedule
  INTEGER                      :: SchedPtr                 =0       ! index to schedule
  CHARACTER(len=MaxNameLength) :: ZoneName                 = ' '    ! Name of zone the system is serving
  INTEGER                      :: ZonePtr                  = 0      ! Point to this zone in the Zone derived type
  INTEGER                      :: ZoneNodeNum              = 0      ! index of zone air node in node structure
  CHARACTER(len=MaxNameLength) :: UnitControlType          = ' '    ! Control type for the system
                                                                    ! (Neutral and setpoint temperatrue)
  INTEGER                      :: ControlType              =0       ! Unit Control type indicator

  INTEGER                      :: AirInletNode             = 0      ! inlet air node number
  INTEGER                      :: AirOutletNode            = 0      ! outlet air node number
  CHARACTER(len=MaxNameLength) :: SFanName                 =' '     ! name of supply fan
  INTEGER                      :: SFan_Index               =0       ! index in fan structure
  INTEGER                      :: SFanType                 =0        ! type of fan in cFanTypes
  INTEGER                      :: SFanAvailSchedPtr        =0        ! supply fan availability sched from fan object
  INTEGER                      :: FanPlace                 =0        ! fan placement; blow through and draw through
  REAL(r64)                    :: FanCorTemp               =0.0d0      ! correction temperature
  LOGICAL                      :: FanEffect                =.FALSE.  ! .TRUE. if unit has a fan type of draw through
  INTEGER                      :: SFanOutletNode           = 0       ! supply fan outlet node number
  CHARACTER(len=MaxNameLength) :: ExtFanName               =' '      ! name of exhaust fan
  INTEGER                      :: ExtFan_Index             =0        ! index in fan structure
  INTEGER                      :: ExtFanType               =0        ! type of fan in cFanTypes
  INTEGER                      :: ExtFanAvailSchedPtr      =0        ! exhaust fan availability sched from fan object
  LOGICAL                      :: ExtFan                   =.FALSE.  ! true if there is an exhaust fan
  CHARACTER(len=MaxNameLength) :: OutAirSchedName          =' '      ! schedule of fraction for outside air (all controls)
  INTEGER                      :: OutAirSchedPtr           =0        ! index to schedule
  INTEGER                      :: OutsideAirNode           =0        ! outside air node number
  REAL(r64)                    :: OutAirVolFlow            =0.0d0      ! m3/s
  REAL(r64)                    :: OutAirMassFlow           =0.0d0      ! kg/s
  REAL(r64)                    :: ExtAirVolFlow            =0.0d0      ! m3/s
  REAL(r64)                    :: ExtAirMassFlow           =0.0d0      ! kg/s
  CHARACTER(len=MaxNameLength) :: ExtAirSchedName          =' '      ! schedule of fraction for exhaust air
  INTEGER                      :: ExtOutAirSchedPtr        =0        ! index to schedule
  REAL(r64)                    :: MaxAirMassFlow           =0.0d0      ! kg/s
  CHARACTER(len=MaxNameLength) :: HiCtrlTempSched          = ' '     ! Schedule name for the High Control Air temperature
  INTEGER                      :: HiCtrlTempSchedPtr       = 0       ! Schedule index for the High Control Air temperature
  CHARACTER(len=MaxNameLength) :: LoCtrlTempSched          = ' '     ! Schedule name for the Low Control Air temperature
  INTEGER                      :: LoCtrlTempSchedPtr       = 0       ! Schedule index for the Low Control Air temperature
  INTEGER                      :: OperatingMode            =0         ! operating condition( NeutralMode, HeatingMode, CoolingMode)
  INTEGER                      :: ControlCompTypeNum       =0
  INTEGER                      :: CompErrIndex             =0
  REAL(r64)                    :: AirMassFlow              =0.0d0       ! kg/s
  INTEGER                      :: UnBalancedErrCount       =0         ! Counter for recurring warning message
  INTEGER                      :: UnBalancedErrIndex       =0              ! Index to recurring warning message
  INTEGER                      :: NumComponents            = 0
  CHARACTER(len=MaxNameLength) :: ComponentListName        = ' '
  REAL(r64)                    :: CompOutSetTemp      =0.0d0   ! component outlet setpoint temperature
  INTEGER                      :: AvailStatus         =0
  CHARACTER(len=MaxNameLength) :: AvailManagerListName = ' ' ! Name of an availability manager list object
  TYPE(OAEquipList),  &
     ALLOCATABLE,DIMENSION(:)  :: OAEquip

  ! Report data
  REAL(r64) :: TotCoolingRate              =0.0d0 ! Rate of total cooling delivered to the zone [W]
  REAL(r64) :: TotCoolingEnergy            =0.0d0 ! Total cooling energy delivered by the OAU supply air to the zone [J]
  REAL(r64) :: SensCoolingRate             =0.0d0 ! Rate of sensible cooling delivered to the zone [W]
  REAL(r64) :: SensCoolingEnergy           =0.0d0 ! Sensible cooling energy delivered by the OAU supply air to the zone [J]
  REAL(r64) :: LatCoolingRate              =0.0d0 ! Rate of latent cooling delivered to the zone [W]
  REAL(r64) :: LatCoolingEnergy            =0.0d0 ! Latent cooling energy delivered by the OAU supply air to the zone [J]
  REAL(r64) :: ElecFanRate                 =0.0d0 ! Total electric use rate (power) for supply/exhaust fans [W]
  REAL(r64) :: ElecFanEnergy               =0.0d0 ! Electric energy use for supply fan and exhaust fan [J]
  REAL(r64) :: SensHeatingEnergy           =0.0d0 ! sensible heating energy delivered by the ERV supply air to the zone [J]
  REAL(r64) :: SensHeatingRate             =0.0d0 ! rate of sensible heating delivered to the zone [W]
  REAL(r64) :: LatHeatingEnergy            =0.0d0 ! latent heating energy delivered by the ERV supply air to the zone [J]
  REAL(r64) :: LatHeatingRate              =0.0d0 ! rate of latent heating delivered to the zone [W]
  REAL(r64) :: TotHeatingEnergy            =0.0d0 ! total heating energy delivered by the ERV supply air to the zone [J]
  REAL(r64) :: TotHeatingRate              =0.0d0 ! rate of total heating delivered to the zone [W]

  END TYPE OAUnitData

TYPE (OAUnitData), ALLOCATABLE, DIMENSION(:) :: OutAirUnit

  ! MODULE VARIABLE DECLARATIONS:
  INTEGER                            :: NumOfOAunits               =0    ! Number of outdoor air unit in the input file
  REAL(r64)                          :: OAMassFlowRate             =0.0d0  ! Outside air mass flow rate for the zone outdoor air unit
  LOGICAL, ALLOCATABLE, DIMENSION(:) :: MyOneTimeErrorFlag
  LOGICAL                            :: GetOutdoorAirUnitInputFlag =.TRUE.  ! Flag set to make sure you get input once

  ! Autosizing variables
  LOGICAL, ALLOCATABLE, DIMENSION(:) :: MySizeFlag
  LOGICAL, ALLOCATABLE, DIMENSION(:) :: CheckEquipName

  ! SUBROUTINE SPECIFICATIONS FOR MODULE OUTDOOR AIR UNIT
PUBLIC  SimOutdoorAirUnit
PRIVATE GetOutdoorAirUnitInputs
PRIVATE InitOutdoorAirUnit
PRIVATE SizeOutdoorAirUnit
PRIVATE CalcOutdoorAirUnit
PUBLIC  SimOutdoorAirEquipComps
!PRIVATE UpdateOutdoorAirUnit
PUBLIC  ReportOutdoorAirUnit
PUBLIC  CalcOAUnitCoilComps
!PUBLIC GetOutAirCoilOutletTemp
PUBLIC  GetOutdoorAirUnitOutAirNode
PUBLIC  GetOutdoorAirUnitZoneInletNode
PUBLIC  GetOutdoorAirUnitReturnAirNode

CONTAINS



SUBROUTINE SimOutdoorAirUnit(CompName,ZoneNum,FirstHVACIteration,PowerMet,LatOutputProvided,CompIndex)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Rick Strand
          !       DATE WRITTEN   May 2000
          !       MODIFIED       na
          !       RE-ENGINEERED
          ! This is re-engineered by Rick Strand and Young T. Chae for OutdoorAirUnit (July, 2009)

          ! PURPOSE OF THIS SUBROUTINE:
          ! This is the main driver subroutine for the outdoor air control unit simulation.

          ! METHODOLOGY EMPLOYED:
          ! Standard EnergyPlus methodology.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: FindItemInList
  USE General,        ONLY: TrimSigDigits

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN)  :: CompName            ! name of the outdoor air unit
  INTEGER,          INTENT(IN)  :: ZoneNum             ! number of zone being served
  LOGICAL,          INTENT(IN)  :: FirstHVACIteration  ! TRUE if 1st HVAC simulation of system timestep
  REAL(r64),        INTENT(OUT) :: PowerMet            ! Sensible power supplied (W)
  REAL(r64),        INTENT(OUT) :: LatOutputProvided   ! Latent add/removal supplied by window AC (kg/s), dehumid = negative
  INTEGER,          INTENT(INOUT) :: CompIndex



          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER      :: OAUnitNum           ! index of outdoor air unit being simulated

          ! FLOW:
  IF (GetOutdoorAirUnitInputFlag) THEN
    CALL GetOutdoorAirUnitInputs
    GetOutdoorAirUnitInputFlag=.false.
  ENDIF

  ! Find the correct Outdoor Air Unit

  IF (CompIndex == 0) THEN
    OAUnitNum=FindItemInList(CompName,OutAirUnit%Name,NumOfOAunits)
    IF (OAUnitNum == 0) THEN
      CALL ShowFatalError('ZoneHVAC:OutdoorAirUnit not found='//TRIM(CompName))
    ENDIF
  ELSE
    OAUnitNum=CompIndex
    IF (OAUnitNum > NumOfOAunits .or. OAUnitNum < 1) THEN
      CALL ShowFatalError('SimOutdoorAirUnit:  Invalid CompIndex passed='//  &
                          TRIM(TrimSigDigits(OAUnitNum))// &
                          ', Number of Units='//TRIM(TrimSigDigits(NumOfOAunits))//  &
                          ', Entered Unit name='//TRIM(CompName))
    ENDIF
    IF (CheckEquipName(OAUnitNum)) THEN
      IF (CompName /= OutAirUnit(OAUnitNum)%Name) THEN
        CALL ShowFatalError('SimOutdoorAirUnit: Invalid CompIndex passed='//  &
                            TRIM(TrimSigDigits(OAUnitNum))// &
                            ', Unit name='//TRIM(CompName)//', stored Unit Name for that index='//  &
                            TRIM(OutAirUnit(OAUnitNum)%Name))
      ENDIF
      CheckEquipName(OAUnitNum)=.false.
    ENDIF
  ENDIF


  IF (ZoneSizingCalc .or. SysSizingCalc) RETURN

  CALL InitOutdoorAirUnit(OAUnitNum,ZoneNum,FirstHVACIteration)

  CALL CalcOutdoorAirUnit(OAUnitNum,ZoneNum,FirstHVACIteration,PowerMet,LatOutputProvided)

! CALL UpdateOutdoorAirUnit(OAUnitNum, FirstHVACIteration)

  CALL ReportOutdoorAirUnit(OAUnitNum)

  RETURN

END SUBROUTINE SimOutdoorAirUnit

SUBROUTINE GetOutdoorAirUnitInputs

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Young Tae Chae, Rick Strand
          !       DATE WRITTEN   July 2009
          !       MODIFIED       July 2012, Chandan Sharma - FSEC: Added zone sys avail managers
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine obtains the input for the outdoor air control unit and sets
          ! up the appropriate derived type.

          ! METHODOLOGY EMPLOYED:
          ! Standard EnergyPlus methodology.

          ! REFERENCES:
          ! Fred Buhl's fan coil module (FanCoilUnits.f90)
          ! Kwang Ho Lee's Unit Ventilator Module (UnitVentilator.f90)
          ! Young Tae Chae's Ventilated Slab System (VentilatedSlab.f90)
          ! Mixed Air.f90

          ! USE STATEMENTS:
  USE InputProcessor
  USE NodeInputManager,         ONLY : GetOnlySingleNode
  USE BranchNodeConnections,    ONLY : SetUpCompSets,TestCompSet
  USE WaterCoils,               ONLY : GetWaterCoilMaxFlowRate=>GetCoilMaxWaterFlowRate,CheckWaterCoilSchedule
  Use SteamCoils,               ONLY : GetCoilAirInletNode, GetCoilAirOutletNode,   &
                                       GetSteamCoilMaxFlowRate=>GetCoilMaxWaterFlowRate, GetSteamCoilIndex, &
                                       GetCoilSteamInletNode
  USE FluidProperties,          ONLY : FindRefrigerant
  USE DataGlobals,              ONLY : NumOfZones, ScheduleAlwaysOn
  USE DataHeatBalance,          ONLY : Zone, Construct
  USE DataSizing,               ONLY : AutoSize
  USE ScheduleManager,          ONLY : GetScheduleIndex
  USE DataLoopNode
  USE DataSurfaceLists
  USE OutAirNodeManager,        ONLY : CheckAndAddAirNodeNumber
  USE WaterCoils,               ONLY : GetCoilWaterInletNode, GetWCoilInletNode=>GetCoilInletNode,  &
                                       GetWCoilOutletNode=>GetCoilOutletNode, GetCoilWaterOutletNode
  USE DXCoils,                  ONLY : GetDXCoilOutletNode => GetCoilOutletNode,GetDXCoilInletNode => GetCoilInletNode
  USE DataGlobals,              ONLY : AnyEnergyManagementSystemInModel
  USE DataInterfaces,           ONLY : SetupEMSActuator
  USE HeatingCoils,             ONLY : GetCoilInletNode, GetCoilOutletNode, GetElecCoilInletNode=>GetCoilInletNode,  &
                                       GetElecCoilOutletNode=>GetCoilOutletNode
  USE HVACHXAssistedCoolingCoil,ONLY : GetHXAssistedCoilFlowRate=>GetCoilMaxWaterFlowRate,   &
                                       GetWHXCoilInletNode=>GetCoilInletNode,GetWHXCoilOutletNode=>GetCoilOutletNode
  USE DataPlant,                ONLY : TypeOf_CoilWaterCooling, TypeOf_CoilWaterSimpleHeating, TypeOf_CoilSteamAirHeating, &
                                       TypeOf_CoilWaterDetailedFlatCooling
  USE Fans,                     ONLY : GetFanIndex, GetFanType, GetFanAvailSchPtr
  USE DataHVACGlobals,          ONLY : cFanTypes, ZoneComp
  USE DataZoneEquipment,        ONLY : OutdoorAirUnit_Num
  USE HVACDXSystem,             ONLY : CheckDXCoolingCoilInOASysExists
  USE HVACUnitarySystem,        ONLY : CheckUnitarySysCoilInOASysExists

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER :: Blank                  = ' '
  CHARACTER(len=*), PARAMETER :: RoutineName            ='GetOutdoorAirUnitInputs: ' ! include trailing blank space

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

  INTEGER :: NumNums   ! Number of real numbers returned by GetObjectItem
  INTEGER :: NumAlphas ! Number of alphanumerics returned by GetObjectItem
  INTEGER :: IOSTAT
  INTEGER :: OAUnitNum
  INTEGER :: CompNum
  INTEGER :: Item
  INTEGER :: NumComponents
  INTEGER :: AlphaNum
  CHARACTER(len=MaxNameLength) :: ComponentListName
  INTEGER :: NumInList
  INTEGER :: InListNum
  INTEGER :: ListNum
  LOGICAL :: ErrorsFound=.false.
  LOGICAL :: IsNotOK                ! Flag to verify name
  LOGICAL :: IsBlank                ! Flag for blank name
  INTEGER :: MaxNums=0              ! Maximum number of numeric input fields
  INTEGER :: MaxAlphas=0            ! Maximum number of alpha input fields
  INTEGER :: TotalArgs=0            ! Total number of alpha and numeric arguments (max) for a
  LOGICAL :: IsValid             ! Set for outside air node check
  CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: cAlphaArgs      ! Alpha input items for object
  CHARACTER(len=MaxNameLength) :: CurrentModuleObject   ! Object type for getting and messages
  CHARACTER(len=MaxNameLength),  ALLOCATABLE, DIMENSION(:) :: cAlphaFields   ! Alpha field names
  CHARACTER(len=MaxNameLength),  ALLOCATABLE, DIMENSION(:) :: cNumericFields ! Numeric field names
  LOGICAL,  ALLOCATABLE, DIMENSION(:) :: lAlphaBlanks     ! Logical array, alpha field input BLANK = .true.
  LOGICAL,  ALLOCATABLE, DIMENSION(:) :: lNumericBlanks   ! Logical array, numeric field input BLANK = .true.
  REAL(r64),  ALLOCATABLE, DIMENSION(:) :: NumArray
  CHARACTER(len=MaxNameLength),  ALLOCATABLE, DIMENSION(:) :: AlphArray
  LOGICAL :: ErrFlag = .FALSE.

          ! FLOW:
          ! Figure out how many outdoor air units there are in the input file

  IF (.not. GetOutdoorAirUnitInputFlag) RETURN

  CALL GetObjectDefMaxArgs(CurrentModuleObjects(CO_OAUnit),TotalArgs,NumAlphas,NumNums)
  MaxNums=MAX(MaxNums,NumNums)
  MaxAlphas=MAX(MaxAlphas,NumAlphas)
  CALL GetObjectDefMaxArgs(CurrentModuleObjects(CO_OAEqList),TotalArgs,NumAlphas,NumNums)
  MaxNums=MAX(MaxNums,NumNums)
  MaxAlphas=MAX(MaxAlphas,NumAlphas)

  ALLOCATE(AlphArray(MaxAlphas))
  AlphArray=' '
  ALLOCATE(cAlphaFields(MaxAlphas))
  cAlphaFields=' '
  ALLOCATE(NumArray(MaxNums))
  NumArray=0.0d0
  ALLOCATE(cNumericFields(MaxNums))
  cNumericFields=' '
  ALLOCATE(lAlphaBlanks(MaxAlphas))
  lAlphaBlanks=.true.
  ALLOCATE(lNumericBlanks(MaxNums))
  lNumericBlanks=.true.
  ALLOCATE(cAlphaArgs(NumAlphas))
  cAlphaArgs=' '


  CurrentModuleObject = CurrentModuleObjects(CO_OAUnit)
  NumofOAUnits = GetNumObjectsFound(CurrentModuleObject)

  ALLOCATE(OutAirUnit(NumofOAUnits))
  ALLOCATE(MyOneTimeErrorFlag(NumofOAUnits))
  MyOneTimeErrorFlag = .TRUE.
  ALLOCATE(CheckEquipName(NumofOAUnits))
  CheckEquipName=.true.

  DO OAUnitNum=1,NumofOAUnits

    CALL GetObjectItem(CurrentModuleObject,OAUnitNum,cAlphaArgs,NumAlphas,NumArray,NumNums,IOSTAT, &
                       NumBlank=lNumericBlanks,AlphaBlank=lAlphaBlanks, &
                       AlphaFieldNames=cAlphaFields,NumericFieldNames=cNumericFields)
    IsNotOK=.false.
    IsBlank=.false.
    CALL VerifyName(cAlphaArgs(1),OutAirUnit%Name,OAUnitNum-1,IsNotOK,IsBlank,TRIM(CurrentModuleObject)//' Name')
    IF (IsNotOK) THEN
      ErrorsFound=.true.
      IF (IsBlank) cAlphaArgs(1)='xxxxx'
    ENDIF
!A1
    OutAirUnit(OAUnitNum)%Name = cAlphaArgs(1)
    IsNotOK=.false.
    IsBlank=.false.

!A2
    OutAirUnit(OAUnitNum)%SchedName = cAlphaArgs(2)
    IF (lAlphaBlanks(2)) THEN
      OutAirUnit(OAUnitNum)%SchedPtr  = ScheduleAlwaysOn
    ELSE
      OutAirUnit(OAUnitNum)%SchedPtr  = GetScheduleIndex(cAlphaArgs(2))  ! convert schedule name to pointer
      IF (OutAirUnit(OAUnitNum)%SchedPtr== 0) THEN
        CALL ShowSevereError(trim(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid '//   &
           trim(cAlphaArgs(2))//'="'//trim(cAlphaArgs(2))//'" not found.')
        ErrorsFound = .TRUE.
      END IF
    END IF

!A3
    OutAirUnit(OAUnitNum)%ZoneName = cAlphaArgs(3)
    OutAirUnit(OAUnitNum)%ZonePtr  = FindIteminList(cAlphaArgs(3),Zone%Name,NumOfZones)

    IF (OutAirUnit(OAUnitNum)%ZonePtr == 0) THEN
      IF (lAlphaBlanks(3)) THEN
        CALL ShowSevereError(trim(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid '//   &
           trim(cAlphaArgs(3))//' is required but input is blank.')
      ELSE
        CALL ShowSevereError(trim(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid '//   &
           trim(cAlphaArgs(3))//'="'//trim(cAlphaArgs(3))//'" not found.')
      END IF
      ErrorsFound=.true.
    END IF
    OutAirUnit(OAUnitNum)%ZoneNodeNum =Zone(OutAirUnit(OAUnitNum)%ZonePtr)%SystemZoneNodeNumber
! Outside air information:
!N1
    OutAirUnit(OAUnitNum)%OutAirVolFlow     = NumArray(1)
!A4
    OutAirUnit(OAUnitNum)%OutAirSchedName   = cAlphaArgs(4)
      ! convert schedule name to pointer
    OutAirUnit(OAUnitNum)%OutAirSchedPtr  = GetScheduleIndex(OutAirUnit(OAUnitNum)%OutAirSchedName)
    IF (OutAirUnit(OAUnitNum)%OutAirSchedPtr == 0) THEN
      CALL ShowSevereError(trim(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid '//   &
            trim(cAlphaFields(4))//'="'//trim(cAlphaArgs(4))//'" not found.')
      ErrorsFound=.TRUE.
    END IF

!A5
    OutAirUnit(OAUnitNum)%SFanName         = cAlphaArgs(5)
    CALL VerifyName(cAlphaArgs(5), OutAirUnit%SFanName, OAUnitNum -1, IsNotOK, IsBlank, 'OA Unit Supply Fan Name')
    If (IsNotOK) THEN
     ErrorsFound=.TRUE.
     IF (IsBlank) cAlphaArgs(5) = 'xxxxx'
    ENDIF
    ErrFlag = .FALSE.
    CALL GetFanType(OutAirUnit(OAUnitNum)%SFanName, OutAirUnit(OAUnitNum)%SFanType, ErrFlag, &
                    TRIM(CurrentModuleObject), OutAirUnit(OAUnitNum)%Name)
    IF (.NOT. ErrFlag) THEN
      OutAirUnit(OAUnitNum)%SFanAvailSchedPtr = GetFanAvailSchPtr(cFanTypes(OutAirUnit(OAUnitNum)%SFanType), &
                                      OutAirUnit(OAUnitNum)%SFanName, ErrFlag)
      ! get fan index
      CALL GetFanIndex(OutAirUnit(OAUnitNum)%SFanName, OutAirUnit(OAUnitNum)%SFan_Index, ErrorsFound)
    ELSE
      ErrorsFound=.TRUE.
    ENDIF
!A6 :Fan Place
    IF (SameString(cAlphaArgs(6),'BlowThrough'))  OutAirUnit(OAUnitNum)%FanPlace = BlowThru
    IF (SameString(cAlphaArgs(6),'DrawThrough'))  OutAirUnit(OAUnitNum)%FanPlace = DrawThru
    IF (OutAirUnit(OAUnitNum)%FanPlace .EQ. 0) THEN
      CALL ShowSevereError('Invalid '//TRIM(cAlphaFields(6))//' = '//TRIM(cAlphaArgs(6)))
      CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(cAlphaArgs(1)))
      ErrorsFound = .TRUE.
    END IF

!A7

    IF (lAlphaBlanks(7)) THEN
      OutAirUnit(OAUnitNum)%ExtFan = .FALSE.
    ELSE IF (.NOT. lAlphaBlanks(7)) THEN
      OutAirUnit(OAUnitNum)%ExtFanName     = cAlphaArgs(7)
      CALL VerifyName(cAlphaArgs(7), OutAirUnit%ExtFanName, OAUnitNum -1, IsNotOK, IsBlank, 'OA Unit Exhaust Fan Name')
      If (IsNotOK) THEN
       ErrorsFound=.TRUE.
       IF (IsBlank) cAlphaArgs(7) = 'xxxxx'
      ENDIF
      ErrFlag = .FALSE.
      CALL GetFanType(OutAirUnit(OAUnitNum)%ExtFanName, OutAirUnit(OAUnitNum)%ExtFanType, ErrFlag, &
                      TRIM(CurrentModuleObject), OutAirUnit(OAUnitNum)%Name)
      IF (.NOT. ErrFlag) THEN
        OutAirUnit(OAUnitNum)%ExtFanAvailSchedPtr = GetFanAvailSchPtr(cFanTypes(OutAirUnit(OAUnitNum)%ExtFanType), &
                                        OutAirUnit(OAUnitNum)%ExtFanName, ErrFlag)
        ! get fan index
        CALL GetFanIndex(OutAirUnit(OAUnitNum)%ExtFanName, OutAirUnit(OAUnitNum)%ExtFan_Index, ErrorsFound)
      ELSE
        ErrorsFound=.TRUE.
      ENDIF
      OutAirUnit(OAUnitNum)%ExtFan = .TRUE.
    END IF

!N2
    OutAirUnit(OAUnitNum)%ExtAirVolFlow     = NumArray(2)
!A8
    OutAirUnit(OAUnitNum)%ExtAirSchedName   = cAlphaArgs(8)
      ! convert schedule name to pointer
    OutAirUnit(OAUnitNum)%ExtOutAirSchedPtr    = GetScheduleIndex(OutAirUnit(OAUnitNum)%ExtAirSchedName)
    IF (OutAirUnit(OAUnitNum)%ExtFan) THEN
      IF ((OutAirUnit(OAUnitNum)%ExtOutAirSchedPtr==0).OR.(lNumericBlanks(2))) THEN
        CALL ShowSevereError(trim(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid '//   &
            trim(cAlphaFields(7))//'="'//trim(cAlphaArgs(8))//'" not found.')
        ErrorsFound=.TRUE.
      END IF
    END IF

    IF (OutAirUnit(OAUnitNum)%ExtFan) THEN

      CALL SetUpCompSets(TRIM(CurrentModuleObject), OutAirUnit(OAUnitNum)%Name, &
                       'UNDEFINED',cAlphaArgs(7),'UNDEFINED','UNDEFINED')
    END IF

! Process the unit control type

    IF (.NOT. lAlphaBlanks(9)) THEN
      SELECT CASE (cAlphaArgs(9))
        CASE ('NEUTRALCONTROL')
          OutAirUnit(OAUnitNum)%ControlType = Neutral
        CASE ('TEMPERATURECONTROL')
          OutAirUnit(OAUnitNum)%ControlType = Temperature
      END SELECt
    ELSE
      CALL ShowSevereError(trim(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid '//   &
            trim(cAlphaFields(9))//'="'//trim(cAlphaArgs(9))//'".')
      CALL ShowContinueError('Control reset to Unconditioned Control.')
      OutAirUnit(OAUnitNum)%ControlType = Neutral
    END IF

!A10:High Control Temp :
    OutAirUnit(OAUnitNum)%HiCtrlTempSched    = cAlphaArgs(10)
    OutAirUnit(OAUnitNum)%HiCtrlTempSchedPtr = GetScheduleIndex(cAlphaArgs(10))
    IF ((OutAirUnit(OAUnitNum)%HiCtrlTempSchedPtr == 0).AND. (.not. lAlphaBlanks(10))) THEN
      CALL ShowSevereError(trim(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid '//   &
            trim(cAlphaFields(10))//'="'//trim(cAlphaArgs(9))//'" not found.')
      ErrorsFound=.true.
    END IF

!A11:Low Control Temp :
    OutAirUnit(OAUnitNum)%LoCtrlTempSched    = cAlphaArgs(11)
    OutAirUnit(OAUnitNum)%LoCtrlTempSchedPtr = GetScheduleIndex(cAlphaArgs(11))
    IF ((OutAirUnit(OAUnitNum)%LoCtrlTempSchedPtr == 0).AND. (.not. lAlphaBlanks(11))) THEN
      CALL ShowSevereError(trim(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid '//   &
            trim(cAlphaFields(11))//'="'//trim(cAlphaArgs(10))//'" not found.')
      ErrorsFound=.true.
    END IF

    OutAirUnit(OAUnitNum)%CompOutSetTemp=0.0d0

!A12~A15 : Node Condition


          ! Main air nodes (except outside air node):

    OutAirUnit(OAUnitNum)%AirOutletNode = &
               GetOnlySingleNode(cAlphaArgs(13),ErrorsFound,TRIM(CurrentModuleObject),cAlphaArgs(1), &
                            NodeType_Air,NodeConnectionType_Outlet,1,ObjectIsParent)
    IF (.not. lAlphaBlanks(14)) THEN
      OutAirUnit(OAUnitNum)%AirInletNode = &
               GetOnlySingleNode(cAlphaArgs(14),ErrorsFound,TRIM(CurrentModuleObject),cAlphaArgs(1), &
                            NodeType_Air,NodeConnectionType_Inlet,1,ObjectIsParent)
    ELSE
      IF (OutAirUnit(OAUnitNum)%ExtFan) THEN
        CALL ShowSevereError(trim(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid '//   &
            trim(cAlphaFields(14))//' cannot be blank when there is an exhaust fan.')
        ErrorsFound=.true.
      ENDIF
    ENDIF

    OutAirUnit(OAUnitNum)%SFanOutletNode = &
               GetOnlySingleNode(cAlphaArgs(15),ErrorsFound,TRIM(CurrentModuleObject),cAlphaArgs(1), &
                            NodeType_Air,NodeConnectionType_Internal,1,ObjectIsNotParent)

    OutAirUnit(OAUnitNum)%OutsideAirNode = &
          !  Set connection type to 'OutdoorAir', because this is hardwired to OA conditions
               GetOnlySingleNode(cAlphaArgs(12),ErrorsFound,CurrentModuleObject,cAlphaArgs(1),&
                            NodeType_Air,NodeConnectionType_OutsideAirReference,1,ObjectIsNotParent)

    IF (.not. lAlphaBlanks(12)) THEN
      CALL CheckAndAddAirNodeNumber(OutAirUnit(OAUnitNum)%OutsideAirNode,IsValid)
      IF (.not. IsValid) THEN
        CALL ShowWarningError(trim(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//   &
            '", Adding OutdoorAir:Node='//TRIM(cAlphaArgs(12)))
      ENDIF

    ENDIF

! When the fan position is "BlowThru", Each node is set up

    IF (OutAirUnit(OAUnitNum)%FanPlace == BlowThru) THEN
      CALL SetUpCompSets(TRIM(CurrentModuleObject), OutAirUnit(OAUnitNum)%Name, &
                       'UNDEFINED',cAlphaArgs(5),cAlphaArgs(12),cAlphaArgs(15))
    END IF


!A16 : component list
    CALL VerifyName(cAlphaArgs(16),OutAirUnit%ComponentListName,OAUnitNum-1,IsNotOK,IsBlank,  &
                      TRIM(CurrentModuleObjects(CO_OAEqList))//' Name')
    IF (IsNotOK) THEN
      ErrorsFound=.true.
      IF (IsBlank) cAlphaArgs(16)='xxxxx'
    ENDIF
    ComponentListName = cAlphaArgs(16)
    OutAirUnit(OAUnitNum)%ComponentListName = ComponentListName
    IF (.NOT. lAlphaBlanks(16)) THEN
      ListNum = GetObjectItemNum(CurrentModuleObjects(CO_OAEqList),ComponentListName)
      IF (ListNum > 0) THEN
        CALL GetObjectItem(CurrentModuleObjects(CO_OAEqList),ListNum,AlphArray,NumAlphas,NumArray,NumNums,IOSTAT)
        NumInList = (NumAlphas-1)/2 ! potential problem if puts in type but not name
        IF (MOD(NumAlphas-1,2) /= 0) NumInList=NumInList+1
        OutAirUnit(OAUnitNum)%NumComponents = NumInList
        ALLOCATE(OutAirUnit(OAUnitNum)%OAEquip(NumInList))

   ! Get information of component
        DO InListNum=1,NumInList
          OutAirUnit(OAUnitNum)%OAEquip(InListNum)%ComponentName = AlphArray(InListNum*2+1)
          OutAirUnit(OAUnitNum)%OAEquip(InListNum)%ComponentType = AlphArray(InListNum*2)
          CompNum=InListNum
          SELECT CASE(MakeUPPERCase(OutAirUnit(OAUnitNum)%OAEquip(CompNum)%ComponentType))
  ! Coil Types
          CASE('COIL:COOLING:WATER')
            OutAirUnit(OAUnitNum)%OAEquip(CompNum)%ComponentType_Num= WaterCoil_Cooling
            OutAirUnit(OAUnitNum)%OAEquip(CompNum)%CoilPlantTypeOfNum = TypeOf_CoilWaterCooling
            OutAirUnit(OAUnitNum)%OAEquip(CompNum)%ComponentIndex   =0
            OutAirUnit(OAUnitNum)%OAEquip(CompNum)%CoilAirInletNode =   &
               GetWCoilInletNode(OutAirUnit(OAUnitNum)%OAEquip(CompNum)%ComponentType,   &
                                 OutAirUnit(OAUnitNum)%OAEquip(CompNum)%ComponentName,ErrorsFound)
            OutAirUnit(OAUnitNum)%OAEquip(CompNum)%CoilAirOutletNode =   &
               GetWCoilOutletNode(OutAirUnit(OAUnitNum)%OAEquip(CompNum)%ComponentType,   &
                                  OutAirUnit(OAUnitNum)%OAEquip(CompNum)%ComponentName,ErrorsFound)
            OutAirUnit(OAUnitNum)%OAEquip(CompNum)%CoilWaterInletNode =   &
               GetCoilWaterInletNode(OutAirUnit(OAUnitNum)%OAEquip(CompNum)%ComponentType,   &
                                     OutAirUnit(OAUnitNum)%OAEquip(CompNum)%ComponentName,ErrorsFound)
            OutAirUnit(OAUnitNum)%OAEquip(CompNum)%CoilWaterOutletNode=   &
               GetCoilWaterOutletNode(OutAirUnit(OAUnitNum)%OAEquip(CompNum)%ComponentType,   &
                                      OutAirUnit(OAUnitNum)%OAEquip(CompNum)%ComponentName,ErrorsFound)
            OutAirUnit(OAUnitNum)%OAEquip(CompNum)%MaxVolWaterFlow=   &
               GetWaterCoilMaxFlowRate(OutAirUnit(OAUnitNum)%OAEquip(CompNum)%ComponentType,  &
                                       OutAirUnit(OAUnitNum)%OAEquip(CompNum)%ComponentName,ErrorsFound)
            OutAirUnit(OAUnitNum)%OAEquip(CompNum)%MinVolWaterFlow=0.0d0

          CASE('COIL:HEATING:WATER')
            OutAirUnit(OAUnitNum)%OAEquip(CompNum)%ComponentType_Num= WaterCoil_SimpleHeat
            OutAirUnit(OAUnitNum)%OAEquip(CompNum)%CoilPlantTypeOfNum = TypeOf_CoilWaterSimpleHeating
            OutAirUnit(OAUnitNum)%OAEquip(CompNum)%ComponentIndex   = 0
            OutAirUnit(OAUnitNum)%OAEquip(CompNum)%CoilAirInletNode =   &
               GetWCoilInletNode(OutAirUnit(OAUnitNum)%OAEquip(CompNum)%ComponentType,   &
                                 OutAirUnit(OAUnitNum)%OAEquip(CompNum)%ComponentName,ErrorsFound)
            OutAirUnit(OAUnitNum)%OAEquip(CompNum)%CoilAirOutletNode =   &
               GetWCoilOutletNode('Coil:Heating:Water',   &
                                 OutAirUnit(OAUnitNum)%OAEquip(CompNum)%ComponentName,ErrorsFound)
            OutAirUnit(OAUnitNum)%OAEquip(CompNum)%CoilWaterInletNode =   &
               GetCoilWaterInletNode(OutAirUnit(OAUnitNum)%OAEquip(CompNum)%ComponentType,  &
                                     OutAirUnit(OAUnitNum)%OAEquip(CompNum)%ComponentName,ErrorsFound)
            OutAirUnit(OAUnitNum)%OAEquip(CompNum)%CoilWaterOutletNode=   &
               GetCoilWaterOutletNode(OutAirUnit(OAUnitNum)%OAEquip(CompNum)%ComponentType,   &
                                      OutAirUnit(OAUnitNum)%OAEquip(CompNum)%ComponentName,ErrorsFound)
            OutAirUnit(OAUnitNum)%OAEquip(CompNum)%MaxVolWaterFlow=   &
               GetWaterCoilMaxFlowRate('Coil:Heating:Water',  &
                                       OutAirUnit(OAUnitNum)%OAEquip(CompNum)%ComponentName,ErrorsFound)
            OutAirUnit(OAUnitNum)%OAEquip(CompNum)%MinVolWaterFlow=0.0d0

          CASE('COIL:HEATING:STEAM')
            OutAirUnit(OAUnitNum)%OAEquip(CompNum)%ComponentType_Num= SteamCoil_AirHeat
            OutAirUnit(OAUnitNum)%OAEquip(CompNum)%CoilPlantTypeOfNum = TypeOf_CoilSteamAirHeating
            OutAirUnit(OAUnitNum)%OAEquip(CompNum)%ComponentIndex=    &
               GetSteamCoilIndex(OutAirUnit(OAUnitNum)%OAEquip(CompNum)%ComponentType,  &
                                 OutAirUnit(OAUnitNum)%OAEquip(CompNum)%ComponentName,ErrorsFound)
            OutAirUnit(OAUnitNum)%OAEquip(CompNum)%CoilAirInletNode =   &
               GetCoilAirInletNode(OutAirUnit(OAUnitNum)%OAEquip(CompNum)%ComponentIndex,  &
                                   OutAirUnit(OAUnitNum)%OAEquip(CompNum)%ComponentName, ErrorsFound)
            OutAirUnit(OAUnitNum)%OAEquip(CompNum)%CoilAirOutletNode=   &
               GetCoilAirOutletNode(OutAirUnit(OAUnitNum)%OAEquip(CompNum)%ComponentIndex,  &
                                    OutAirUnit(OAUnitNum)%OAEquip(CompNum)%ComponentName, ErrorsFound)
            OutAirUnit(OAUnitNum)%OAEquip(CompNum)%CoilWaterInletNode =   &
               GetCoilSteamInletNode(OutAirUnit(OAUnitNum)%OAEquip(CompNum)%ComponentIndex,  &
                                     OutAirUnit(OAUnitNum)%OAEquip(CompNum)%ComponentName, ErrorsFound)

            OutAirUnit(OAUnitNum)%OAEquip(CompNum)%MinVolWaterFlow=0.0d0
              ! below: no extra error needed if steam properties not in input
              ! file because getting the steam coil will have done that.
            OutAirUnit(OAUnitNum)%OAEquip(CompNum)%FluidIndex=FindRefrigerant('Steam')

          CASE('COIL:COOLING:WATER:DETAILEDGEOMETRY')
            OutAirUnit(OAUnitNum)%OAEquip(CompNum)%ComponentType_Num= WaterCoil_DetailedCool
            OutAirUnit(OAUnitNum)%OAEquip(CompNum)%CoilPlantTypeOfNum = TypeOf_CoilWaterDetailedFlatCooling
            OutAirUnit(OAUnitNum)%OAEquip(CompNum)%CoilAirInletNode =   &
               GetWCoilInletNode(OutAirUnit(OAUnitNum)%OAEquip(CompNum)%ComponentType,   &
                                            OutAirUnit(OAUnitNum)%OAEquip(CompNum)%ComponentName,ErrorsFound)
            OutAirUnit(OAUnitNum)%OAEquip(CompNum)%CoilAirOutletNode =   &
               GetWCoilOutletNode(OutAirUnit(OAUnitNum)%OAEquip(CompNum)%ComponentType,  &
                                  OutAirUnit(OAUnitNum)%OAEquip(CompNum)%ComponentName,ErrorsFound)
            OutAirUnit(OAUnitNum)%OAEquip(CompNum)%CoilWaterInletNode =   &
               GetCoilWaterInletNode(OutAirUnit(OAUnitNum)%OAEquip(CompNum)%ComponentType,   &
                                     OutAirUnit(OAUnitNum)%OAEquip(CompNum)%ComponentName,ErrorsFound)
            OutAirUnit(OAUnitNum)%OAEquip(CompNum)%CoilWaterOutletNode =   &
               GetCoilWaterOutletNode(OutAirUnit(OAUnitNum)%OAEquip(CompNum)%ComponentType,   &
                                      OutAirUnit(OAUnitNum)%OAEquip(CompNum)%ComponentName,ErrorsFound)
            OutAirUnit(OAUnitNum)%OAEquip(CompNum)%MaxVolWaterFlow =   &
               GetWaterCoilMaxFlowRate(OutAirUnit(OAUnitNum)%OAEquip(CompNum)%ComponentType,  &
                                       OutAirUnit(OAUnitNum)%OAEquip(CompNum)%ComponentName,ErrorsFound)
            OutAirUnit(OAUnitNum)%OAEquip(CompNum)%MinVolWaterFlow=0.0d0

          CASE('COILSYSTEM:COOLING:WATER:HEATEXCHANGERASSISTED')
            OutAirUnit(OAUnitNum)%OAEquip(CompNum)%ComponentType_Num= WaterCoil_CoolingHXAsst
            OutAirUnit(OAUnitNum)%OAEquip(CompNum)%CoilAirInletNode =   &
              GetWHXCoilInletNode(OutAirUnit(OAUnitNum)%OAEquip(CompNum)%ComponentType,   &
                                  OutAirUnit(OAUnitNum)%OAEquip(CompNum)%ComponentName,ErrorsFound)
            OutAirUnit(OAUnitNum)%OAEquip(CompNum)%CoilAirOutletNode =   &
               GetWHXCoilOutletNode(OutAirUnit(OAUnitNum)%OAEquip(CompNum)%ComponentType,   &
                                    OutAirUnit(OAUnitNum)%OAEquip(CompNum)%ComponentName,ErrorsFound)
            OutAirUnit(OAUnitNum)%OAEquip(CompNum)%CoilWaterInletNode =   &
               GetCoilWaterInletNode(OutAirUnit(OAUnitNum)%OAEquip(CompNum)%ComponentType,   &
                                     OutAirUnit(OAUnitNum)%OAEquip(CompNum)%ComponentName,ErrorsFound)
            OutAirUnit(OAUnitNum)%OAEquip(CompNum)%CoilWaterOutletNode =   &
               GetCoilWaterOutletNode(OutAirUnit(OAUnitNum)%OAEquip(CompNum)%ComponentType,   &
                                      OutAirUnit(OAUnitNum)%OAEquip(CompNum)%ComponentName,ErrorsFound)
            OutAirUnit(OAUnitNum)%OAEquip(CompNum)%MaxVolWaterFlow =   &
               GetHXAssistedCoilFlowRate(OutAirUnit(OAUnitNum)%OAEquip(CompNum)%ComponentType,  &
                                         OutAirUnit(OAUnitNum)%OAEquip(CompNum)%ComponentName,ErrorsFound)
            OutAirUnit(OAUnitNum)%OAEquip(CompNum)%MinVolWaterFlow=0.0d0


          CASE('COIL:HEATING:ELECTRIC')
            OutAirUnit(OAUnitNum)%OAEquip(CompNum)%ComponentType_Num= Coil_ElectricHeat
            OutAirUnit(OAUnitNum)%OAEquip(CompNum)%CoilAirInletNode =   &
               GetElecCoilInletNode(OutAirUnit(OAUnitNum)%OAEquip(CompNum)%ComponentType,   &
                                    OutAirUnit(OAUnitNum)%OAEquip(CompNum)%ComponentName,ErrorsFound)
            OutAirUnit(OAUnitNum)%OAEquip(CompNum)%CoilAirOutletNode =   &
               GetElecCoilOutletNode(OutAirUnit(OAUnitNum)%OAEquip(CompNum)%ComponentType,   &
                                    OutAirUnit(OAUnitNum)%OAEquip(CompNum)%ComponentName,ErrorsFound)

          CASE('COIL:HEATING:GAS')
            OutAirUnit(OAUnitNum)%OAEquip(CompNum)%ComponentType_Num= Coil_GasHeat
            OutAirUnit(OAUnitNum)%OAEquip(CompNum)%CoilAirInletNode =   &
               GetCoilInletNode(OutAirUnit(OAUnitNum)%OAEquip(CompNum)%ComponentType,   &
                                           OutAirUnit(OAUnitNum)%OAEquip(CompNum)%ComponentName,ErrorsFound)
            OutAirUnit(OAUnitNum)%OAEquip(CompNum)%CoilAirOutletNode =   &
               GetCoilOutletNode(OutAirUnit(OAUnitNum)%OAEquip(CompNum)%ComponentType,   &
                                           OutAirUnit(OAUnitNum)%OAEquip(CompNum)%ComponentName,ErrorsFound)

          CASE('COILSYSTEM:COOLING:DX')
            OutAirUnit(OAUnitNum)%OAEquip(CompNum)%ComponentType_Num= DXSystem
            ! set the data for 100% DOAS DX cooling coil
            CALL CheckDXCoolingCoilInOASysExists(OutAirUnit(OAUnitNum)%OAEquip(CompNum)%ComponentName)

          CASE('COILSYSTEM:HEATING:DX')
            OutAirUnit(OAUnitNum)%OAEquip(CompNum)%ComponentType_Num= DXHeatPumpSystem

          CASE('AIRLOOPHVAC:UNITARYSYSTEM')
            OutAirUnit(OAUnitNum)%OAEquip(CompNum)%ComponentType_Num= UnitarySystem
            CALL CheckUnitarySysCoilInOASysExists(OutAirUnit(OAUnitNum)%OAEquip(CompNum)%ComponentName)

  ! Heat recovery
          CASE('HEATEXCHANGER:AIRTOAIR:FLATPLATE')
            OutAirUnit(OAUnitNum)%OAEquip(CompNum)%ComponentType_Num= HeatXchngr

          CASE('HEATEXCHANGER:AIRTOAIR:SENSIBLEANDLATENT')
            OutAirUnit(OAUnitNum)%OAEquip(CompNum)%ComponentType_Num= HeatXchngr
  !        CASE('HEATEXCHANGER:DESICCANT:BALANCEDFLOW')
  !          OutAirUnit(OAUnitNum)%OAEquip(CompNum)%ComponentType_Num= HeatXchngr

  ! Desiccant Dehumidifier
          CASE('DEHUMIDIFIER:DESICCANT:NOFANS')
            OutAirUnit(OAUnitNum)%OAEquip(CompNum)%ComponentType_Num= Desiccant
  ! Futher Enhancement
  !        CASE('DEHUMIDIFIER:DESICCANT:SYSTEM')
  !          OutAirUnit(OAUnitNum)%OAEquip(CompNum)%ComponentType_Num= Desiccant

          CASE DEFAULT
          CALL ShowSevereError(TRIM(CurrentModuleObject)//' = "'//TRIM(AlphArray(1))//'" invalid '//  &
             'Outside Air Component="'//TRIM(OutAirUnit(OAUnitNum)%OAEquip(CompNum)%ComponentType)//'".')
            ErrorsFound=.true.

          END SELECT
          ! Add equipment to component sets array
          ! Node set up
          IF (OutAirUnit(OAUnitNum)%FanPlace == BlowThru) THEN
            IF (InListNum.EQ.1) THEN ! the component is the first one
              CALL SetUpCompSets('ZoneHVAC:OutdoorAirUnit',OutAirUnit(OAUnitNum)%Name, &
                                OutAirUnit(OAUnitNum)%OAEquip(InListNum)%ComponentType, &
                                OutAirUnit(OAUnitNum)%OAEquip(InListNum)%ComponentName, &
                                cAlphaArgs(15),'UNDEFINED')
            ELSE IF (.Not. InListNum.EQ.NumInList) THEN ! the component is placed in b/w components
              CALL SetUpCompSets('ZoneHVAC:OutdoorAirUnit',OutAirUnit(OAUnitNum)%Name, &
                                OutAirUnit(OAUnitNum)%OAEquip(InListNum)%ComponentType, &
                                OutAirUnit(OAUnitNum)%OAEquip(InListNum)%ComponentName, &
                                'UNDEFINED','UNDEFINED')
            ELSE IF(InListNum .EQ.NumInList) THEN ! the component is the last one
              CALL SetUpCompSets('ZoneHVAC:OutdoorAirUnit',OutAirUnit(OAUnitNum)%Name, &
                                 OutAirUnit(OAUnitNum)%OAEquip(InListNum)%ComponentType, &
                                 OutAirUnit(OAUnitNum)%OAEquip(InListNum)%ComponentName, &
                                 'UNDEFINED',cAlphaArgs(13))
            END IF
         ! If fan is on the end of equipment.
          ELSE IF (OutAirUnit(OAUnitNum)%FanPlace == DrawThru) THEN
            IF (InListNum.EQ.1) THEN
              CALL SetUpCompSets('ZoneHVAC:OutdoorAirUnit',OutAirUnit(OAUnitNum)%Name, &
                                OutAirUnit(OAUnitNum)%OAEquip(InListNum)%ComponentType, &
                                OutAirUnit(OAUnitNum)%OAEquip(InListNum)%ComponentName, &
                                cAlphaArgs(12),'UNDEFINED')
            ELSE IF (.Not. InListNum.EQ.NumInList) THEN
              CALL SetUpCompSets('ZoneHVAC:OutdoorAirUnit',OutAirUnit(OAUnitNum)%Name, &
                                OutAirUnit(OAUnitNum)%OAEquip(InListNum)%ComponentType, &
                                OutAirUnit(OAUnitNum)%OAEquip(InListNum)%ComponentName, &
                                'UNDEFINED','UNDEFINED')
            ELSE IF(InListNum .EQ.NumInList) THEN
              CALL SetUpCompSets('ZoneHVAC:OutdoorAirUnit',OutAirUnit(OAUnitNum)%Name, &
                                 OutAirUnit(OAUnitNum)%OAEquip(InListNum)%ComponentType, &
                                 OutAirUnit(OAUnitNum)%OAEquip(InListNum)%ComponentName, &
                                 'UNDEFINED','UNDEFINED')
            END IF
          END IF
        END DO ! End Inlist

        ! In case of draw through, the last component is linked with the zone air supply node
        IF (OutAirUnit(OAUnitNum)%FanPlace == DrawThru) THEN
          CALL SetUpCompSets(TRIM(CurrentModuleObject), OutAirUnit(OAUnitNum)%Name, &
                       'UNDEFINED',cAlphaArgs(5),'UNDEFINED',cAlphaArgs(13))
        END IF

      ELSE ! when ListNum<0
        CALL ShowSevereError(TRIM(CurrentModuleObject)//' = "'//TRIM(cAlphaArgs(1))//'" invalid '//  &
           TRIM(cAlphaFields(16))//'="'//TRIM(cAlphaArgs(16))//'" not found.')
        ErrorsFound=.true.
      END IF
    ELSE ! when Equipment list is left blanked
      CALL ShowSevereError(TRIM(CurrentModuleObject)//' = "'//TRIM(cAlphaArgs(1))//'" invalid '//  &
           TRIM(cAlphaFields(16))//' is blank and must be entered.')
      ErrorsFound=.true.
    ENDIF
    IF (.NOT. lAlphaBlanks(17)) THEN
     OutAirUnit(OAUnitNum)%AvailManagerListName = cAlphaArgs(17)
     ZoneComp(OutdoorAirUnit_Num)%ZoneCompAvailMgrs(OAUnitNum)%AvailManagerListName  = cAlphaArgs(17)
    ENDIF

  END DO

  IF (ErrorsFound) THEN
    CALL ShowFatalError(RoutineName//'Errors found in getting '//TRIM(CurrentModuleObject)//'.')
  ENDIF

  DEALLOCATE(AlphArray)
  DEALLOCATE(cAlphaFields)
  DEALLOCATE(NumArray)
  DEALLOCATE(cNumericFields)
  DEALLOCATE(lAlphaBlanks)
  DEALLOCATE(lNumericBlanks)

  GetOutdoorAirUnitInputFlag = .FALSE.

  ! Setup Report variables for the zone outdoor air unit CurrentModuleObject='ZoneHVAC:OutdoorAirUnit'
  DO OAUnitNum = 1, NumOfOAUnits
    CALL SetupOutputVariable('Zone Outdoor Air Unit Total Heating Rate [W]',        &
                             OutAirUnit(OAUnitNum)%TotHeatingRate,'System', &
                             'Average', OutAirUnit(OAUnitNum)%Name)
    CALL SetupOutputVariable('Zone Outdoor Air Unit Total Heating Energy [J]',        &
                             OutAirUnit(OAUnitNum)%TotHeatingEnergy,'System', &
                             'Sum', OutAirUnit(OAUnitNum)%Name)
    CALL SetupOutputVariable('Zone Outdoor Air Unit Sensible Heating Rate [W]',        &
                             OutAirUnit(OAUnitNum)%SensHeatingRate,'System', &
                             'Average', OutAirUnit(OAUnitNum)%Name)
    CALL SetupOutputVariable('Zone Outdoor Air Unit Sensible Heating Energy [J]',        &
                             OutAirUnit(OAUnitNum)%SensHeatingEnergy,'System', &
                             'Sum', OutAirUnit(OAUnitNum)%Name)
    CALL SetupOutputVariable('Zone Outdoor Air Unit Latent Heating Rate [W]',        &
                             OutAirUnit(OAUnitNum)%LatHeatingRate,'System', &
                             'Average', OutAirUnit(OAUnitNum)%Name)
    CALL SetupOutputVariable('Zone Outdoor Air Unit Latent Heating Energy [J]',        &
                             OutAirUnit(OAUnitNum)%LatHeatingEnergy,'System', &
                             'Sum', OutAirUnit(OAUnitNum)%Name)
    CALL SetupOutputVariable('Zone Outdoor Air Unit Total Cooling Rate [W]',        &
                             OutAirUnit(OAUnitNum)%TotCoolingRate,'System', &
                             'Average', OutAirUnit(OAUnitNum)%Name)
    CALL SetupOutputVariable('Zone Outdoor Air Unit Total Cooling Energy [J]',        &
                             OutAirUnit(OAUnitNum)%TotCoolingEnergy,'System', &
                             'Sum', OutAirUnit(OAUnitNum)%Name)
    CALL SetupOutputVariable('Zone Outdoor Air Unit Sensible Cooling Rate [W]',        &
                             OutAirUnit(OAUnitNum)%SensCoolingRate,'System', &
                             'Average', OutAirUnit(OAUnitNum)%Name)
    CALL SetupOutputVariable('Zone Outdoor Air Unit Sensible Cooling Energy [J]',        &
                             OutAirUnit(OAUnitNum)%SensCoolingEnergy,'System', &
                             'Sum', OutAirUnit(OAUnitNum)%Name)
    CALL SetupOutputVariable('Zone Outdoor Air Unit Latent Cooling Rate [W]',        &
                             OutAirUnit(OAUnitNum)%LatCoolingRate,'System', &
                             'Average', OutAirUnit(OAUnitNum)%Name)
    CALL SetupOutputVariable('Zone Outdoor Air Unit Latent Cooling Energy [J]',        &
                             OutAirUnit(OAUnitNum)%LatCoolingEnergy,'System', &
                             'Sum', OutAirUnit(OAUnitNum)%Name)
    CALL SetupOutputVariable('Zone Outdoor Air Unit Air Mass Flow Rate [kg/s]',   &
                             OutAirUnit(OAUnitNum)%AirMassFlow,'System','Average', &
                             OutAirUnit(OAUnitNum)%Name)
    CALL SetupOutputVariable('Zone Outdoor Air Unit Fan Electric Power [W]',  &
                             OutAirUnit(OAUnitNum)%ElecFanRate,'System', &
                             'Average', OutAirUnit(OAUnitNum)%Name)
    CALL SetupOutputVariable('Zone Outdoor Air Unit Fan Electric Energy [J]',   &
                            OutAirUnit(OAUnitNum)%ElecFanEnergy,'System','Sum', &
                             OutAirUnit(OAUnitNum)%Name)
    CALL SetupOutputVariable('Zone Outdoor Air Unit Fan Availability Status []',OutAirUnit(OAUnitNum)%AvailStatus, &
                             'System','Average',OutAirUnit(OAUnitNum)%Name)
!! Note that the outdoor air unit fan electric is NOT metered because this value is already metered through the fan component

  END DO

  RETURN

END SUBROUTINE GetOutdoorAirUnitInputs


SUBROUTINE InitOutdoorAirUnit(OAUnitNum,ZoneNum,FirstHVACIteration)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Young Tae Chae, Rick Strand
          !       DATE WRITTEN   July 2009
          !       MODIFIED       July 2012, Chandan Sharma - FSEC: Added zone sys avail managers
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine initializes all of the data elements which are necessary
          ! to simulate a zone outdoor air control unit.

          ! METHODOLOGY EMPLOYED:
          ! Uses the status flags to trigger initializations.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataEnvironment,   ONLY : OutBaroPress, OutDryBulbTemp, OutHumRat, StdBaroPress,StdRhoAir
  USE DataGlobals,       ONLY : NumOfZones, AnyPlantInModel
  USE DataLoopNode,      ONLY : Node
  USE ScheduleManager,   ONLY : GetCurrentScheduleValue
  USE DataHeatBalFanSys, ONLY : MAT,ZoneAirHumRat
  USE DataZoneEquipment, ONLY : ZoneEquipInputsFilled,CheckZoneEquipmentList, OutdoorAirUnit_Num
  USE DataHVACGlobals,   ONLY : ShortenTimeStepSys, ZoneComp, ZoneCompTurnFansOn, ZoneCompTurnFansOff
  USE DataPlant,         ONLY : PlantLoop, ScanPlantLoopsForObject, &
                                TypeOf_CoilWaterCooling, TypeOf_CoilWaterSimpleHeating, &
                                TypeOf_CoilSteamAirHeating, TypeOf_CoilWaterDetailedFlatCooling
  USE PlantUtilities,    ONLY : InitComponentNodes

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: OAUnitNum           ! index for the current outdoor air unit
  INTEGER, INTENT(IN) :: ZoneNum             ! number of zone being served
  LOGICAL, INTENT(IN) :: FirstHVACIteration  ! TRUE if 1st HVAC simulation of system timestep

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER :: CurrentModuleObject='ZoneHVAC:OutdoorAirUnit'

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER        :: Loop
  LOGICAL,SAVE   :: MyOneTimeFlag = .TRUE.
  LOGICAL,SAVE   :: ZoneEquipmentListChecked = .false.  ! True after the Zone Equipment List has been checked for items
  LOGICAL, ALLOCATABLE,Save, DIMENSION(:) :: MyEnvrnFlag
  LOGICAL, ALLOCATABLE, SAVE, DIMENSION(:) :: MyPlantScanFlag
  INTEGER        :: InNode             ! inlet node number in outdoor air unit
  INTEGER        :: OutNode            ! outlet node number in outdoor air unit
  INTEGER        :: OutsideAirNode     ! outside air node number outdoor air unit
  REAL(r64)      :: OAFrac             ! possible outside air fraction
  REAL(r64)      :: EAFrac             ! possible exhaust air fraction
  REAL(r64)      :: RhoAir             ! air density at InNode
  REAL(r64)      :: TempSteamIn
  REAL(r64)      :: SteamDensity
  INTEGER        :: EQListNum
  INTEGER        :: EQNum
  INTEGER        :: SteamConNode        ! Hot Steam control node number for steam coil
  INTEGER        :: HotConNode          ! Hot water control node number of hot water coil
  INTEGER        :: ColdConNode         ! Cold water control node number of cold water coil
  INTEGER        :: compLoop !local do loop index
  REAL(r64)      :: rho
  LOGICAL        :: errFlag


! FLOW:
  ! Do the one time initializations

  IF (MyOneTimeFlag) THEN

    ALLOCATE(MyEnvrnFlag(NumOfOAunits))
    ALLOCATE(MySizeFlag(NumOfOAunits))
    ALLOCATE(MyPlantScanFlag(NumOfOAunits))
    MyEnvrnFlag = .TRUE.
    MySizeFlag = .TRUE.
    MyPlantScanFlag = .TRUE.
    MyOneTimeFlag = .FALSE.

  END IF

  IF (ALLOCATED(ZoneComp)) THEN
    ZoneComp(OutdoorAirUnit_Num)%ZoneCompAvailMgrs(OAUnitNum)%ZoneNum = ZoneNum
    OutAirUnit(OAUnitNum)%AvailStatus = ZoneComp(OutdoorAirUnit_Num)%ZoneCompAvailMgrs(OAUnitNum)%AvailStatus
  ENDIF

  IF (MyPlantScanFlag(OAUnitNum) .AND. ALLOCATED(PlantLoop))THEN
    DO compLoop=1, OutAirUnit(OAUnitNum)%NumComponents
      IF( (OutAirUnit(OAUnitNum)%OAEquip(compLoop)%CoilPlantTypeOfNum == TypeOf_CoilWaterCooling) .OR. &
          (OutAirUnit(OAUnitNum)%OAEquip(compLoop)%CoilPlantTypeOfNum == TypeOf_CoilWaterDetailedFlatCooling) .OR. &
          (OutAirUnit(OAUnitNum)%OAEquip(compLoop)%CoilPlantTypeOfNum == TypeOf_CoilWaterSimpleHeating) .OR. &
          (OutAirUnit(OAUnitNum)%OAEquip(compLoop)%CoilPlantTypeOfNum == TypeOf_CoilSteamAirHeating) ) THEN

        errFlag=.false.
        CALL ScanPlantLoopsForObject(   OutAirUnit(OAUnitNum)%OAEquip(compLoop)%ComponentName, &
                                        OutAirUnit(OAUnitNum)%OAEquip(compLoop)%CoilPlantTypeOfNum, &
                                        OutAirUnit(OAUnitNum)%OAEquip(compLoop)%LoopNum, &
                                        OutAirUnit(OAUnitNum)%OAEquip(compLoop)%LoopSideNum, &
                                        OutAirUnit(OAUnitNum)%OAEquip(compLoop)%BranchNum, &
                                        OutAirUnit(OAUnitNum)%OAEquip(compLoop)%CompNum,          &
                                        errFlag=errFlag)
        IF (errFlag) THEN
          CALL ShowFatalError('InitOutdoorAirUnit: Program terminated for previous conditions.')
        ENDIF
      ENDIF
    ENDDO

    MyPlantScanFlag(OAUnitNum) = .FALSE.
  ELSEIF (MyPlantScanFlag(OAUnitNum) .AND. .NOT. AnyPlantInModel) THEN
    MyPlantScanFlag(OAUnitNum) = .FALSE.
  ENDIF

  ! need to check all zone outdoor air control units to see if they are on Zone Equipment List or issue warning
  IF (.not. ZoneEquipmentListChecked .and. ZoneEquipInputsFilled) THEN
    ZoneEquipmentListChecked=.true.
    DO Loop=1,NumOfOAunits
      IF (CheckZoneEquipmentList(CurrentModuleObject,OutAirUnit(Loop)%Name)) CYCLE
      CALL ShowSevereError('InitOutdoorAirUnit: Zone Outdoor Air Unit=['//TRIM(CurrentModuleObject)//','//  &
         TRIM(OutAirUnit(Loop)%Name)//  &
           '] is not on any ZoneHVAC:EquipmentList.  It will not be simulated.')
    ENDDO
  ENDIF

  IF (.NOT. SysSizingCalc .AND. MySizeFlag(OAUnitNum) .AND. .NOT. MyPlantScanFlag(OAUnitNum) ) THEN

    CALL SizeOutdoorAirUnit(OAUnitNum)

    MySizeFlag(OAUnitNum) = .FALSE.

  END IF

  ! Do the one time initializations
  IF (BeginEnvrnFlag .AND. MyEnvrnFlag(OAUnitNum)) THEN
 ! Node Conditions

    OutNode        = OutAirUnit(OAUnitNum)%AirOutletNode
    OutsideAirNode = OutAirUnit(OAUnitNum)%OutsideAirNode
  !Outdoor Air flow rate conditions
    RhoAir         = StdRhoAir
    OAFrac = GetCurrentScheduleValue(OutAirUnit(OAUnitNum)%OutAirSchedPtr)
    OutAirUnit(OAUnitNum)%OutAirMassFlow = RhoAir*OAFrac*OutAirUnit(OAUnitNum)%OutAirVolFlow

    IF (OutAirUnit(OAUnitNum)%ExtFan) THEN
      InNode         = OutAirUnit(OAUnitNum)%AirInletNode
    ! set the exhaust air mass flow rate from input
       IF (OutAirUnit(OAUnitNum)%ExtFan ) THEN
         EAFrac = GetCurrentScheduleValue(OutAirUnit(OAUnitNum)%ExtOutAirSchedPtr)
      OutAirUnit(OAUnitNum)%ExtAirMassFlow = RhoAir*EAFrac*OutAirUnit(OAUnitNum)%ExtAirVolFlow
      ELSE IF (.NOT. OutAirUnit(OAUnitNum)%ExtFan )THEN
         OutAirUnit(OAUnitNum)%ExtAirMassFlow=OutAirUnit(OAUnitNum)%OutAirMassFlow
      END IF
      Node(InNode)%MassFlowRateMax = OutAirUnit(OAUnitNum)%MaxAirMassFlow
      Node(InNode)%MassFlowRateMin = 0.0d0
    ENDIF
    ! set the node max and min mass flow rates
    Node(OutsideAirNode)%MassFlowRateMax = OutAirUnit(OAUnitNum)%OutAirMassFlow
    Node(OutsideAirNode)%MassFlowRateMin = 0.0d0
    Node(OutNode)%MassFlowRate = OutAirUnit(OAUnitNum)%MaxAirMassFlow

    IF (.NOT. MyPlantScanFlag(OAUnitNum)) THEN
      DO compLoop=1, OutAirUnit(OAUnitNum)%NumComponents
        IF( (OutAirUnit(OAUnitNum)%OAEquip(compLoop)%CoilPlantTypeOfNum == TypeOf_CoilWaterCooling) .OR. &
            (OutAirUnit(OAUnitNum)%OAEquip(compLoop)%CoilPlantTypeOfNum == TypeOf_CoilWaterDetailedFlatCooling)  ) THEN
          rho = GetDensityGlycol(PlantLoop(OutAirUnit(OAUnitNum)%OAEquip(compLoop)%LoopNum )%fluidName, &
                                       5.d0, &
                                      PlantLoop( OutAirUnit(OAUnitNum)%OAEquip(compLoop)%LoopNum )%fluidIndex, &
                                      'SizeOutdoorAirUnit' )
          OutAirUnit(OAUnitNum)%OAEquip(compLoop)%MaxWaterMassFlow = rho * OutAirUnit(OAUnitNum)%OAEquip(compLoop)%MaxVolWaterFlow
          OutAirUnit(OAUnitNum)%OAEquip(compLoop)%MinWaterMassFlow = rho * OutAirUnit(OAUnitNum)%OAEquip(compLoop)%MinVolWaterFlow
          CALL InitComponentNodes(           OutAirUnit(OAUnitNum)%OAEquip(compLoop)%MinWaterMassFlow, &
                                             OutAirUnit(OAUnitNum)%OAEquip(compLoop)%MaxWaterMassFlow, &
                                             OutAirUnit(OAUnitNum)%OAEquip(compLoop)%CoilWaterInletNode, &
                                             OutAirUnit(OAUnitNum)%OAEquip(compLoop)%CoilWaterOutletNode, &
                                             OutAirUnit(OAUnitNum)%OAEquip(compLoop)%LoopNum, &
                                             OutAirUnit(OAUnitNum)%OAEquip(compLoop)%LoopSideNum, &
                                             OutAirUnit(OAUnitNum)%OAEquip(compLoop)%BranchNum, &
                                             OutAirUnit(OAUnitNum)%OAEquip(compLoop)%CompNum)

        ENDIF

        IF (OutAirUnit(OAUnitNum)%OAEquip(compLoop)%CoilPlantTypeOfNum == TypeOf_CoilWaterSimpleHeating) THEN
          rho = GetDensityGlycol(PlantLoop(OutAirUnit(OAUnitNum)%OAEquip(compLoop)%LoopNum )%fluidName, &
                                       60.d0, &
                                      PlantLoop( OutAirUnit(OAUnitNum)%OAEquip(compLoop)%LoopNum )%fluidIndex, &
                                      'SizeOutdoorAirUnit' )
          OutAirUnit(OAUnitNum)%OAEquip(compLoop)%MaxWaterMassFlow = rho * OutAirUnit(OAUnitNum)%OAEquip(compLoop)%MaxVolWaterFlow
          OutAirUnit(OAUnitNum)%OAEquip(compLoop)%MinWaterMassFlow = rho * OutAirUnit(OAUnitNum)%OAEquip(compLoop)%MinVolWaterFlow
          CALL InitComponentNodes(           OutAirUnit(OAUnitNum)%OAEquip(compLoop)%MinWaterMassFlow, &
                                             OutAirUnit(OAUnitNum)%OAEquip(compLoop)%MaxWaterMassFlow, &
                                             OutAirUnit(OAUnitNum)%OAEquip(compLoop)%CoilWaterInletNode, &
                                             OutAirUnit(OAUnitNum)%OAEquip(compLoop)%CoilWaterOutletNode, &
                                             OutAirUnit(OAUnitNum)%OAEquip(compLoop)%LoopNum, &
                                             OutAirUnit(OAUnitNum)%OAEquip(compLoop)%LoopSideNum, &
                                             OutAirUnit(OAUnitNum)%OAEquip(compLoop)%BranchNum, &
                                             OutAirUnit(OAUnitNum)%OAEquip(compLoop)%CompNum)
        ENDIF
        IF  (OutAirUnit(OAUnitNum)%OAEquip(compLoop)%CoilPlantTypeOfNum == TypeOf_CoilSteamAirHeating) THEN
          !DSU deal with steam mass flow rate , currenlty just like hot water  DSU?
          rho = GetDensityGlycol(PlantLoop(OutAirUnit(OAUnitNum)%OAEquip(compLoop)%LoopNum )%fluidName, &
                                       60.d0, &
                                      PlantLoop( OutAirUnit(OAUnitNum)%OAEquip(compLoop)%LoopNum )%fluidIndex, &
                                      'SizeOutdoorAirUnit' )
          OutAirUnit(OAUnitNum)%OAEquip(compLoop)%MaxWaterMassFlow = rho * OutAirUnit(OAUnitNum)%OAEquip(compLoop)%MaxVolWaterFlow
          OutAirUnit(OAUnitNum)%OAEquip(compLoop)%MinWaterMassFlow = rho * OutAirUnit(OAUnitNum)%OAEquip(compLoop)%MinVolWaterFlow
          CALL InitComponentNodes(           OutAirUnit(OAUnitNum)%OAEquip(compLoop)%MinWaterMassFlow, &
                                             OutAirUnit(OAUnitNum)%OAEquip(compLoop)%MaxWaterMassFlow, &
                                             OutAirUnit(OAUnitNum)%OAEquip(compLoop)%CoilWaterInletNode, &
                                             OutAirUnit(OAUnitNum)%OAEquip(compLoop)%CoilWaterOutletNode, &
                                             OutAirUnit(OAUnitNum)%OAEquip(compLoop)%LoopNum, &
                                             OutAirUnit(OAUnitNum)%OAEquip(compLoop)%LoopSideNum, &
                                             OutAirUnit(OAUnitNum)%OAEquip(compLoop)%BranchNum, &
                                             OutAirUnit(OAUnitNum)%OAEquip(compLoop)%CompNum)
        ENDIF

      ENDDO
    ENDIF
    MyEnvrnFlag(OAUnitNum) = .FALSE.

  END IF  ! ...end start of environment inits

  IF (.NOT. BeginEnvrnFlag)  MyEnvrnFlag(OAUnitNum) = .TRUE.

 ! These initializations are done every iteration...
 ! Set all the output variable
  OutAirUnit(OAUnitNum)%TotHeatingRate   =0.0d0
  OutAirUnit(OAUnitNum)%SensHeatingRate  =0.0d0
  OutAirUnit(OAUnitNum)%LatHeatingRate   =0.0d0
  OutAirUnit(OAUnitNum)%TotCoolingRate   =0.0d0
  OutAirUnit(OAUnitNum)%SensCoolingRate  =0.0d0
  OutAirUnit(OAUnitNum)%LatCoolingRate   =0.0d0
  OutAirUnit(OAUnitNum)%AirMassFlow      =0.0d0
  OutAirUnit(OAUnitNum)%ElecFanRate      =0.0d0
 ! Node Set

  OutNode        = OutAirUnit(OAUnitNum)%AirOutletNode
  OutsideAirNode = OutAirUnit(OAUnitNum)%OutsideAirNode
  RhoAir         = StdRhoAir
  OAFrac         = GetCurrentScheduleValue(OutAirUnit(OAUnitNum)%OutAirSchedPtr)


 ! set the mass flow rates from the input volume flow rates
  IF (OAFrac > 0.0d0 .OR. ZoneCompTurnFansOn .AND. .NOT. ZoneCompTurnFansOff) THEN ! fan is available
    OutAirUnit(OAUnitNum)%OutAirMassFlow = RhoAir*OAFrac*OutAirUnit(OAUnitNum)%OutAirVolFlow
  ELSE
    OutAirUnit(OAUnitNum)%OutAirMassFlow = 0.d0
  ENDIF

 ! set the exhaust air mass flow rate from input
  IF (OutAirUnit(OAUnitNum)%ExtFan ) THEN
    InNode = OutAirUnit(OAUnitNum)%AirInletNode
    EAFrac = GetCurrentScheduleValue(OutAirUnit(OAUnitNum)%ExtOutAirSchedPtr)
    IF (OutAirUnit(OAUnitNum)%ExtFanAvailSchedPtr > 0.d0) THEN
      OutAirUnit(OAUnitNum)%ExtAirMassFlow = RhoAir*EAFrac*OutAirUnit(OAUnitNum)%ExtAirVolFlow
    ELSE
      OutAirUnit(OAUnitNum)%ExtAirMassFlow =  0.d0
    ENDIF
    Node(InNode)%MassFlowRate                 = OutAirUnit(OAUnitNum)%ExtAirMassFlow
    Node(InNode)%MassFlowRateMaxAvail         = OutAirUnit(OAUnitNum)%ExtAirMassFlow
    Node(InNode)%MassFlowRateMinAvail         = 0.d0
  ELSE IF (.NOT. OutAirUnit(OAUnitNum)%ExtFan )THEN
    OutAirUnit(OAUnitNum)%ExtAirMassFlow= 0.d0
  END IF

  ! First, set the flow conditions up so that there is flow through the unit

  Node(OutNode)%MassFlowRate                = OutAirUnit(OAUnitNum)%OutAirMassFlow
  Node(OutNode)%MassFlowRateMaxAvail        = OutAirUnit(OAUnitNum)%OutAirMassFlow
  Node(OutNode)%MassFlowRateMinAvail        = 0.0d0
  Node(OutsideAirNode)%MassFlowRate         = OutAirUnit(OAUnitNum)%OutAirMassFlow
  Node(OutsideAirNode)%MassFlowRateMaxAvail = OutAirUnit(OAUnitNum)%OutAirMassFlow
  Node(OutsideAirNode)%MassFlowRateMinAvail = 0.0d0

          ! Just in case the system is off and conditions do not get sent through
          ! the system for some reason, set the outlet conditions equal to the inlet
          ! conditions of the zone outdoor air control unit
  IF (OutAirUnit(OAUnitNum)%ExtFan ) THEN
    Node(OutNode)%Temp     = Node(InNode)%Temp
    Node(OutNode)%Press    = Node(InNode)%Press
    Node(OutNode)%HumRat   = Node(InNode)%HumRat
    Node(OutNode)%Enthalpy = Node(InNode)%Enthalpy
  ELSE
    Node(OutNode)%Temp     = Node(OutsideAirNode)%Temp
    Node(OutNode)%Press    = Node(OutsideAirNode)%Press
    Node(OutNode)%HumRat   = Node(OutsideAirNode)%HumRat
    Node(OutNode)%Enthalpy = Node(OutsideAirNode)%Enthalpy
  ENDIF
           !These initializations only need to be done once at the start of the iterations...
  IF (FirstHVACIteration .OR. ShortenTimeStepSys) THEN
          ! Initialize the outside air conditions...
    Node(OutsideAirNode)%Temp     = Node(OutsideAirNode)%OutAirDryBulb
    Node(OutsideAirNode)%HumRat   = OutHumRat
    Node(OutsideAirNode)%Press    = OutBaroPress

  END IF

  RETURN

END SUBROUTINE InitOutdoorAirUnit

SUBROUTINE SizeOutdoorAirUnit(OAUnitNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Young Tae Chae, Rick Strand
          !       DATE WRITTEN   July 2009
          !       MODIFIED       Brent Griffith, March 2010, autosize OA flow rate
          !                      August 2013 Daeho Kang, add component sizing table entries
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is for sizing zoen outdoor air control unit components for which flow rates have not been
          ! specified in the input.

          ! METHODOLOGY EMPLOYED:
          ! Obtains flow rates from the zone sizing arrays and plant sizing data.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataSizing
  USE InputProcessor
  USE WaterCoils,     ONLY: SetCoilDesFlow, GetCoilWaterInletNode, GetCoilWaterOutletNode
  USE SteamCoils,     ONLY: GetCoilSteamInletNode, GetCoilSteamOutletNode
  USE HVACHXAssistedCoolingCoil, ONLY: GetHXDXCoilName, GetHXCoilType
!  USE BranchInputManager, ONLY: MyPlantSizingIndex
  USE DataEnvironment, ONLY: StdRhoAir
  USE FluidProperties, ONLY: GetSpecificHeatGlycol, GetDensityGlycol
  USE DataPlant,       ONLY: PlantLoop, MyPlantSizingIndex
  USE DataHVACGlobals, ONLY: cFanTypes
  USE ReportSizingManager, ONLY: ReportSizingOutput
  USE Fans,            ONLY: SetFanData
  USE General,             ONLY: RoundSigDigits

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  Integer, Intent(IN) :: OAUnitNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER             :: PltSizHeatNum ! index of plant sizing object for 1st heating loop
  INTEGER             :: PltSizCoolNum ! index of plant sizing object for 1st cooling loop
  LOGICAL             :: ErrorsFound
  REAL(r64)           :: CoilInTemp
  REAL(r64)           :: CoilOutTemp
  REAL(r64)           :: CoilOutHumRat
  REAL(r64)           :: CoilInHumRat
  REAL(r64)           :: DesCoilLoad
  REAL(r64)           :: TempSteamIn
  REAL(r64)           :: EnthSteamInDry
  REAL(r64)           :: EnthSteamOutWet
  REAL(r64)           :: LatentHeatSteam
  REAL(r64)           :: SteamDensity
  REAL(r64)           :: RhoAir
  REAL(r64)           :: SizeAirMassFlow
  INTEGER             :: CoilWaterInletNode=0
  INTEGER             :: CoilWaterOutletNode=0
  INTEGER             :: CoilSteamInletNode=0
  INTEGER             :: CoilSteamOutletNode=0
  CHARACTER(len=MaxNameLength) :: CoolingCoilName
  CHARACTER(len=MaxNameLength) :: CoolingCoilType
  INTEGER             :: SizeComp
  INTEGER             :: CompNum
  INTEGER             :: ComponentType_Num
  REAL(r64)           :: rho
  REAL(r64)           :: Cp
  INTEGER             :: DummyWaterIndex = 1
  LOGICAL             :: IsAutosize           ! Indicator to autosize
  REAL(r64)           :: OutAirVolFlowDes     ! Autosized outdoor air flow for reporting
  REAL(r64)           :: OutAirVolFlowUser    ! Hardsized outdoor air flow for reporting
  REAL(r64)           :: ExtAirVolFlowDes     ! Autosized exhaust air flow for reporting
  REAL(r64)           :: ExtAirVolFlowUser    ! Hardsized exhaust air flow for reporting
  REAL(r64)           :: MaxVolWaterFlowDes   ! Autosized maximum water flow for reporting
  REAL(r64)           :: MaxVolWaterFlowUser  ! Hardsized maximum water flow for reporting

  PltSizCoolNum  = 0
  PltSizHeatNum  = 0
  ErrorsFound    = .FALSE.
  RhoAir         = StdRhoAir
  IsAutosize     = .FALSE.
  OutAirVolFlowDes = 0.0d0
  OutAirVolFlowUser = 0.0d0
  ExtAirVolFlowDes = 0.0d0
  ExtAirVolFlowUser = 0.0d0
  MaxVolWaterFlowDes = 0.0d0
  MaxVolWaterFlowUser = 0.0d0

  IF (OutAirUnit(OAUnitNum)%OutAirVolFlow == AutoSize) THEN
    IsAutosize = .TRUE.
  END IF

  IF (CurZoneEqNum > 0) THEN
    IF (.NOT. IsAutosize .AND. .NOT. ZoneSizingRunDone) THEN ! Simulation continue
      IF (OutAirUnit(OAUnitNum)%OutAirVolFlow > 0.0d0) THEN
        CALL ReportSizingOutput(CurrentModuleObjects(1), OutAirUnit(OAUnitNum)%Name, &
                            'User-Specified Outdoor Air Flow Rate [m3/s]', OutAirUnit(OAUnitNum)%OutAirVolFlow)
        CALL ReportSizingOutput(TRIM(cFanTypes(OutAirUnit(OAUnitNum)%SFanType)), OutAirUnit(OAUnitNum)%SFanName, &
                            'User-Specified Maximum Outdoor Air Flow Rate [m3/s]', OutAirUnit(OAUnitNum)%OutAirVolFlow)
      END IF
    ELSE
      CALL CheckZoneSizing(CurrentModuleObjects(1), OutAirUnit(OAUnitNum)%Name)
      OutAirVolFlowDes = FinalZoneSizing(CurZoneEqNum)%MinOA
      IF (OutAirVolFlowDes < SmallAirVolFlow) THEN
        OutAirVolFlowDes = 0.0D0
      ENDIF
      IF (IsAutosize) THEN
        OutAirUnit(OAUnitNum)%OutAirVolFlow = OutAirVolFlowDes
        CALL ReportSizingOutput(CurrentModuleObjects(1), OutAirUnit(OAUnitNum)%Name, &
                            'Design Size Outdoor Air Flow Rate [m3/s]', OutAirVolFlowDes)
        CALL SetFanData(OutAirUnit(OAUnitNum)%SFan_Index, ErrorsFound, OutAirUnit(OAUnitNum)%SFanName, &
                                MaxAirVolFlow = OutAirUnit(OAUnitNum)%OutAirVolFlow, &
                                MinAirVolFlow = 0.d0)
        CALL ReportSizingOutput(cFanTypes(OutAirUnit(OAUnitNum)%SFanType), OutAirUnit(OAUnitNum)%SFanName, &
                            'Design Size Maximum Outdoor Air Flow Rate [m3/s]', OutAirVolFlowDes)
      ELSE
        IF (OutAirUnit(OAUnitNum)%OutAirVolFlow > 0.0d0 .AND. OutAirVolFlowDes > 0.0d0) THEN
          OutAirVolFlowUser = OutAirUnit(OAUnitNum)%OutAirVolFlow
          CALL ReportSizingOutput(CurrentModuleObjects(1), OutAirUnit(OAUnitNum)%Name, &
                            'Design Size Outdoor Air Flow Rate [m3/s]', OutAirVolFlowDes, &
                            'User-Specified Outdoor Air Flow Rate [m3/s]', OutAirVolFlowUser)
          IF (DisplayExtraWarnings) THEN
            IF ((ABS(OutAirVolFlowDes - OutAirVolFlowUser)/OutAirVolFlowUser) > AutoVsHardSizingThreshold) THEN
              CALL ShowMessage('SizeOutdoorAirUnit: Potential issue with equipment sizing for ZoneHVAC:OutdoorAirUnit ' &
                                 //TRIM(OutAirUnit(OAUnitNum)%Name))
              CALL ShowContinueError('User-Specified Outdoor Air Flow Rate of '// &
                                      TRIM(RoundSigDigits(OutAirVolFlowUser,5))// ' [m3/s]')
              CALL ShowContinueError('differs from Design Size Outdoor Air Flow Rate of ' // &
                                      TRIM(RoundSigDigits(OutAirVolFlowDes,5))// ' [m3/s]')
              CALL ShowContinueError('This may, or may not, indicate mismatched component sizes.')
              CALL ShowContinueError('Verify that the value entered is intended and is consistent with other components.')
            END IF
          ENDIF

          CALL ReportSizingOutput(cFanTypes(OutAirUnit(OAUnitNum)%SFanType), OutAirUnit(OAUnitNum)%SFanName, &
                                'Design Size Maximum Outdoor Air Flow Rate [m3/s]', OutAirVolFlowDes, &
                                'User-Specified Maximum Outdoor Air Flow Rate [m3/s]', OutAirVolFlowUser)
          IF (DisplayExtraWarnings) THEN
            IF ((ABS(OutAirVolFlowDes - OutAirVolFlowUser)/OutAirVolFlowUser) > AutoVsHardSizingThreshold) THEN
              CALL ShowMessage('SizeOutdoorAirUnit: Potential issue with equipment sizing for ZoneHVAC:OutdoorAirUnit ' &
                                  //TRIM(cFanTypes(OutAirUnit(OAUnitNum)%SFanType))//' '//  &
                                  TRIM(OutAirUnit(OAUnitNum)%SFanName))
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
    ENDIF
  ENDIF

  IsAutosize = .FALSE.
  IF (OutAirUnit(OAUnitNum)%ExtAirVolFlow == Autosize) THEN
    IsAutosize = .TRUE.
  END IF
  IF (CurZoneEqNum > 0) THEN
    IF (.NOT. IsAutosize .AND. .NOT. ZoneSizingRunDone) THEN ! Simulation continue
      IF (OutAirUnit(OAUnitNum)%ExtAirVolFlow > 0.0d0) THEN
        CALL ReportSizingOutput(CurrentModuleObjects(1), OutAirUnit(OAUnitNum)%Name, &
                            'User-Specified Exhaust Air Flow Rate [m3/s]', OutAirUnit(OAUnitNum)%ExtAirVolFlow)
        CALL ReportSizingOutput(TRIM(cFanTypes(OutAirUnit(OAUnitNum)%ExtFanType)), OutAirUnit(OAUnitNum)%ExtFanName, &
                            'User-Specified Maximum Exhaust Air Flow Rate [m3/s]', OutAirUnit(OAUnitNum)%ExtAirVolFlow)
      END IF
    ELSE
      ! set exhaust flow equal to the oa inlet flow
      ExtAirVolFlowDes = OutAirUnit(OAUnitNum)%OutAirVolFlow
      IF (IsAutosize) THEN
        OutAirUnit(OAUnitNum)%ExtAirVolFlow = ExtAirVolFlowDes
        CALL ReportSizingOutput(CurrentModuleObjects(1), OutAirUnit(OAUnitNum)%Name, &
                            'Design Size Exhaust Air Flow Rate [m3/s]', ExtAirVolFlowDes)
        CALL SetFanData(OutAirUnit(OAUnitNum)%ExtFan_Index, ErrorsFound, OutAirUnit(OAUnitNum)%ExtFanName, &
                                MaxAirVolFlow = ExtAirVolFlowDes, &
                                MinAirVolFlow = 0.d0)
        CALL ReportSizingOutput(TRIM(cFanTypes(OutAirUnit(OAUnitNum)%ExtFanType)), OutAirUnit(OAUnitNum)%ExtFanName, &
                                'Design Size Maximum Exhaust Air Flow Rate [m3/s]', ExtAirVolFlowDes)
      ELSE
        IF (OutAirUnit(OAUnitNum)%ExtAirVolFlow > 0.0d0 .AND. ExtAirVolFlowDes > 0.0d0) THEN
          ExtAirVolFlowUser = OutAirUnit(OAUnitNum)%ExtAirVolFlow
          CALL ReportSizingOutput(CurrentModuleObjects(1), OutAirUnit(OAUnitNum)%Name, &
                                'Design Size Exhaust Air Flow Rate [m3/s]', ExtAirVolFlowDes, &
                                'User-Specified Exhaust Air Flow Rate [m3/s]', ExtAirVolFlowUser)
          IF (DisplayExtraWarnings) THEN
            IF ((ABS(ExtAirVolFlowDes - ExtAirVolFlowUser)/ExtAirVolFlowUser) > AutoVsHardSizingThreshold) THEN
              CALL ShowMessage('SizeOutdoorAirUnit: Potential issue with equipment sizing for ZoneHVAC:OutdoorAirUnit ' &
                                   //TRIM(OutAirUnit(OAUnitNum)%Name))
              CALL ShowContinueError('User-Specified Exhaust Air Flow Rate of '// &
                                    TRIM(RoundSigDigits(ExtAirVolFlowUser,5))// ' [m3/s]')
              CALL ShowContinueError('differs from Design Size Exhaust Air Flow Rate of ' // &
                                    TRIM(RoundSigDigits(ExtAirVolFlowDes,5))// ' [m3/s]')
              CALL ShowContinueError('This may, or may not, indicate mismatched component sizes.')
              CALL ShowContinueError('Verify that the value entered is intended and is consistent with other components.')
            END IF
          ENDIF

          CALL ReportSizingOutput(TRIM(cFanTypes(OutAirUnit(OAUnitNum)%ExtFanType)), OutAirUnit(OAUnitNum)%ExtFanName, &
                                    'Design Size Maximum Exhaust Air Flow Rate [m3/s]', ExtAirVolFlowDes, &
                                    'User-Specified Maximum Exhaust Air Flow Rate [m3/s]', ExtAirVolFlowUser)
          IF (DisplayExtraWarnings) THEN
            IF ((ABS(ExtAirVolFlowDes - ExtAirVolFlowUser)/ExtAirVolFlowUser) > AutoVsHardSizingThreshold) THEN
              CALL ShowMessage('SizeOutdoorAirUnit: Potential issue with equipment sizing for ZoneHVAC:OutdoorAirUnit ' &
                                //TRIM(cFanTypes(OutAirUnit(OAUnitNum)%SFanType))//' '//  &
                                TRIM(OutAirUnit(OAUnitNum)%SFanName))
              CALL ShowContinueError('User-Specified Maximum Exhaust Air Flow Rate of '// &
                                    TRIM(RoundSigDigits(ExtAirVolFlowUser,5))// ' [m3/s]')
              CALL ShowContinueError('differs from Design Size Maximum Exhaust Air Flow Rate of ' // &
                                    TRIM(RoundSigDigits(ExtAirVolFlowDes,5))// ' [m3/s]')
              CALL ShowContinueError('This may, or may not, indicate mismatched component sizes.')
              CALL ShowContinueError('Verify that the value entered is intended and is consistent with other components.')
            END IF
          ENDIF
        END IF
      END IF
    END IF
  END IF

  ! air mass flow of unit component sizing is set by input
  SizeAirMassFlow=RhoAir*OutAirUnit(OAUnitNum)%OutAirVolFlow
  SizeComp=OAUnitNum
  DO SizeComp=1,NumofOAUnits
    DO CompNum=1,OutAirUnit(OAUnitNum)%NumComponents
      IsAutosize = .FALSE.
      SELECT CASE(MakeUPPERCase(OutAirUnit(OAUnitNum)%OAEquip(CompNum)%ComponentType))

        ! Coil Types
        CASE('COIL:COOLING:WATER')
          OutAirUnit(OAUnitNum)%OAEquip(CompNum)%ComponentType_Num= WaterCoil_Cooling
          IF ((OutAirUnit(OAUnitNum)%OAEquip(CompNum)%MaxVolWaterFlow==AutoSize))THEN
            IsAutosize = .TRUE.
          END IF
          IF (CurZoneEqNum > 0) THEN
            IF (.NOT. IsAutosize .AND. .NOT. ZoneSizingRunDone) THEN ! Simulation continue
              IF (OutAirUnit(OAUnitNum)%OAEquip(CompNum)%MaxVolWaterFlow > 0.0d0) THEN
                CALL ReportSizingOutput('ZoneHVAC:OutdoorAirUnit', OutAirUnit(OAUnitNum)%Name, &
                                        'User-Specified Maximum Cold Water Flow [m3/s]', &
                                        OutAirUnit(OAUnitNum)%OAEquip(CompNum)%MaxVolWaterFlow)
              END IF
            ELSE
              CALL CheckZoneSizing('ZoneHVAC:OutdoorAirUnit', OutAirUnit(OAUnitNum)%Name)
              CoolingCoilName = OutAirUnit(OAUnitNum)%OAEquip(CompNum)%ComponentName
              CoolingCoilType = OutAirUnit(OAUnitNum)%OAEquip(CompNum)%ComponentType
              CoilWaterInletNode =  OutAirUnit(OAUnitNum)%OAEquip(CompNum)%CoilWaterInletNode
              CoilWaterOutletNode = OutAirUnit(OAUnitNum)%OAEquip(CompNum)%CoilWaterOutletNode
              IF (IsAutosize) THEN
                PltSizCoolNum = MyPlantSizingIndex(CoolingCoilType,CoolingCoilName, CoilWaterInletNode, &
                                               CoilWaterOutletNode, ErrorsFound)
                IF (PltSizCoolNum > 0) THEN
                  IF (FinalZoneSizing(CurZoneEqNum)%DesCoolMassFlow >= SmallAirVolFlow) THEN
                    CoilInTemp = FinalZoneSizing(CurZoneEqNum)%DesCoolCoilInTemp
                    CoilOutTemp = FinalZoneSizing(CurZoneEqNum)%CoolDesTemp
                    CoilOutHumRat = FinalZoneSizing(CurZoneEqNum)%CoolDesHumRat
                    CoilInHumRat = FinalZoneSizing(CurZoneEqNum)%DesCoolCoilInHumRat
                    DesCoilLoad = FinalZoneSizing(CurZoneEqNum)%DesCoolMassFlow &
                                * (PsyHFnTdbW(CoilInTemp, CoilInHumRat)-PsyHFnTdbW(CoilOutTemp, CoilOutHumRat))
                    DesCoilLoad = MAX(0.d0, DesCoilLoad)
                    rho = GetDensityGlycol(PlantLoop(OutAirUnit(OAUnitNum)%OAEquip(CompNum)%LoopNum )%fluidName, &
                                       5.d0, &
                                      PlantLoop( OutAirUnit(OAUnitNum)%OAEquip(CompNum)%LoopNum )%fluidIndex, &
                                        'SizeOutdoorAirUnit' )

                    Cp  = GetSpecificHeatGlycol(PlantLoop(OutAirUnit(OAUnitNum)%OAEquip(CompNum)%LoopNum )%fluidName, &
                                       5.d0, &
                                      PlantLoop(OutAirUnit(OAUnitNum)%OAEquip(CompNum)%LoopNum )%fluidIndex, &
                                        'SizeOutdoorAirUnit' )

                    MaxVolWaterFlowDes = DesCoilLoad / &
                                       ( PlantSizData(PltSizCoolNum)%DeltaT * &
                                       Cp * rho )
                  ELSE
                    MaxVolWaterFlowDes = 0.0d0
                  END IF
                ELSE
                  CALL ShowSevereError('Autosizing of water flow requires a Sizing:Zone object '//  &
                     'or a cooling loop Sizing:Plant object')
                  CALL ShowContinueError('Occurs in ' // 'ZoneHVAC:OutdoorAirUnit' // ' Object=' &
                                     //TRIM(OutAirUnit(OAUnitNum)%Name))
                  ErrorsFound = .TRUE.
                END IF
              END IF
              IF (IsAutosize) THEN
                OutAirUnit(OAUnitNum)%OAEquip(CompNum)%MaxVolWaterFlow = MaxVolWaterFlowDes
                CALL ReportSizingOutput('ZoneHVAC:OutdoorAirUnit', OutAirUnit(OAUnitNum)%Name, &
                                        'Design Size Maximum Cold Water Flow [m3/s]', MaxVolWaterFlowDes)
              ELSE
                IF (OutAirUnit(OAUnitNum)%OAEquip(CompNum)%MaxVolWaterFlow > 0.0d0 .AND. MaxVolWaterFlowDes > 0.0d0) THEN
                  MaxVolWaterFlowUser = OutAirUnit(OAUnitNum)%OAEquip(CompNum)%MaxVolWaterFlow
                  CALL ReportSizingOutput('ZoneHVAC:OutdoorAirUnit', OutAirUnit(OAUnitNum)%Name, &
                                        'Design Size Maximum Cold Water Flow [m3/s]', MaxVolWaterFlowDes, &
                                        'User-Specified Maximum Cold Water Flow [m3/s]', MaxVolWaterFlowUser)
                  IF (DisplayExtraWarnings) THEN
                    IF ((ABS(MaxVolWaterFlowDes - MaxVolWaterFlowUser)/MaxVolWaterFlowUser) > AutoVsHardSizingThreshold) THEN
                      CALL ShowMessage('SizeOutdoorAirUnit: Potential issue with equipment sizing for ZoneHVAC:OutdoorAirUnit ' &
                                //TRIM(OutAirUnit(OAUnitNum)%Name))
                      CALL ShowContinueError('User-Specified Maximum Cold Water Flow of '// &
                                      TRIM(RoundSigDigits(MaxVolWaterFlowUser,5))// ' [m3/s]')
                      CALL ShowContinueError('differs from Design Size Maximum Cold Water Flow of ' // &
                                      TRIM(RoundSigDigits(MaxVolWaterFlowDes,5))// ' [m3/s]')
                      CALL ShowContinueError('This may, or may not, indicate mismatched component sizes.')
                      CALL ShowContinueError('Verify that the value entered is intended and is consistent with other components.')
                    END IF
                  ENDIF
                END IF
              END IF
            END IF
          END IF
        ! set the design air flow rates for the heating and cooling coils
!          CALL SetCoilDesFlow(CoolingCoilType,CoolingCoilName,OutAirUnit(OAUnitNum)%OutAirVolFlow,&
!                                 ErrorsFound)

        CASE('COIL:HEATING:WATER')
          OutAirUnit(OAUnitNum)%OAEquip(CompNum)%ComponentType_Num= WaterCoil_SimpleHeat
          IF ((OutAirUnit(OAUnitNum)%OAEquip(CompNum)%MaxVolWaterFlow==AutoSize))THEN
            IsAutosize = .TRUE.
          END IF
          IF (CurZoneEqNum > 0) THEN
            IF (.NOT. IsAutosize .AND. .NOT. ZoneSizingRunDone) THEN ! Simulation continue
              IF (OutAirUnit(OAUnitNum)%OAEquip(CompNum)%MaxVolWaterFlow > 0.0d0) THEN
                CALL ReportSizingOutput('ZoneHVAC:OutdoorAirUnit', OutAirUnit(OAUnitNum)%Name, &
                                      'User-Specified Maximum Hot Water Flow [m3/s]', &
                                      OutAirUnit(OAUnitNum)%OAEquip(CompNum)%MaxVolWaterFlow)
              END IF
            ELSE
              CALL CheckZoneSizing('ZoneHVAC:OutdoorAirUnit', OutAirUnit(OAUnitNum)%Name)
              CoilWaterInletNode = OutAirUnit(OAUnitNum)%OAEquip(CompNum)%CoilWaterInletNode
              CoilWaterOutletNode = OutAirUnit(OAUnitNum)%OAEquip(CompNum)%CoilWaterOutletNode
              IF (IsAutosize) THEN
                PltSizHeatNum = MyPlantSizingIndex('Coil:Heating:Water', OutAirUnit(OAUnitNum)%OAEquip(CompNum)%ComponentName,  &
                   CoilWaterInletNode, CoilWaterOutletNode, ErrorsFound)
                IF (PltSizHeatNum > 0) THEN
                  IF (FinalZoneSizing(CurZoneEqNum)%DesHeatMassFlow >= SmallAirVolFlow) THEN
                    CoilInTemp = FinalZoneSizing(CurZoneEqNum)%DesHeatCoilInTemp
                    CoilOutTemp = FinalZoneSizing(CurZoneEqNum)%HeatDesTemp
                    CoilOutHumRat = FinalZoneSizing(CurZoneEqNum)%HeatDesHumRat
                    DesCoilLoad = PsyCpAirFnWTdb(CoilOutHumRat, 0.5d0*(CoilInTemp+CoilOutTemp)) &
                                    * FinalZoneSizing(CurZoneEqNum)%DesHeatMassFlow &
                                    * (CoilOutTemp-CoilInTemp)
                    DesCoilLoad = MAX(0.d0, DesCoilLoad)
                    rho = GetDensityGlycol(PlantLoop(OutAirUnit(OAUnitNum)%OAEquip(CompNum)%LoopNum )%fluidName, &
                                       60.d0, &
                                      PlantLoop( OutAirUnit(OAUnitNum)%OAEquip(CompNum)%LoopNum )%fluidIndex, &
                                        'SizeOutdoorAirUnit' )

                    Cp  = GetSpecificHeatGlycol(PlantLoop(OutAirUnit(OAUnitNum)%OAEquip(CompNum)%LoopNum )%fluidName, &
                                       60.d0, &
                                      PlantLoop( OutAirUnit(OAUnitNum)%OAEquip(CompNum)%LoopNum )%fluidIndex, &
                                        'SizeOutdoorAirUnit' )


                    MaxVolWaterFlowDes = DesCoilLoad / &
                                       (PlantSizData(PltSizHeatNum)%DeltaT * &
                                       Cp * rho )
                  ELSE
                    MaxVolWaterFlowDes = 0.0d0
                  END IF
                ELSE
                  CALL ShowSevereError('Autosizing of water flow requires a Sizing:Zone object '//  &
                     'or a heating loop Sizing:Plant object')
                  CALL ShowContinueError('Occurs in ' // 'ZoneHVAC:OutdoorAirUnit' // ' Object=' &
                                     //TRIM(OutAirUnit(OAUnitNum)%Name))
                  ErrorsFound = .TRUE.
                END IF
              END IF
              IF (IsAutosize) THEN
                OutAirUnit(OAUnitNum)%OAEquip(CompNum)%MaxVolWaterFlow = MaxVolWaterFlowDes
                CALL ReportSizingOutput('ZoneHVAC:OutdoorAirUnit', OutAirUnit(OAUnitNum)%Name, &
                                        'Design Size Maximum Hot Water Flow [m3/s]', MaxVolWaterFlowDes)
              ELSE
                IF (OutAirUnit(OAUnitNum)%OAEquip(CompNum)%MaxVolWaterFlow > 0.0d0 .AND. MaxVolWaterFlowDes > 0.0d0) THEN
                  MaxVolWaterFlowUser = OutAirUnit(OAUnitNum)%OAEquip(CompNum)%MaxVolWaterFlow
                  CALL ReportSizingOutput('ZoneHVAC:OutdoorAirUnit', OutAirUnit(OAUnitNum)%Name, &
                                        'Design Size Maximum Hot Water Flow [m3/s]', MaxVolWaterFlowDes, &
                                        'User-Specified Maximum Hot Water Flow [m3/s]', MaxVolWaterFlowUser)
                  IF (DisplayExtraWarnings) THEN
                    IF ((ABS(MaxVolWaterFlowDes - MaxVolWaterFlowUser)/MaxVolWaterFlowUser) > AutoVsHardSizingThreshold) THEN
                      CALL ShowMessage('SizeOutdoorAirUnit: Potential issue with equipment sizing for ZoneHVAC:OutdoorAirUnit ' &
                                      //TRIM(OutAirUnit(OAUnitNum)%Name))
                      CALL ShowContinueError('User-Specified Maximum Hot Water Flow of '// &
                                      TRIM(RoundSigDigits(MaxVolWaterFlowUser,5))// ' [m3/s]')
                      CALL ShowContinueError('differs from Design Size Maximum Hot Water Flow of ' // &
                                      TRIM(RoundSigDigits(MaxVolWaterFlowDes,5))// ' [m3/s]')
                      CALL ShowContinueError('This may, or may not, indicate mismatched component sizes.')
                      CALL ShowContinueError('Verify that the value entered is intended and is consistent with other components.')
                    END IF
                  ENDIF
                END IF
              END IF
            END IF
          ENDIF
          CALL SetCoilDesFlow(OutAirUnit(OAUnitNum)%OAEquip(CompNum)%ComponentType,  &
                                 OutAirUnit(OAUnitNum)%OAEquip(CompNum)%ComponentName,  &
                                 OutAirUnit(OAUnitNum)%OutAirVolFlow,ErrorsFound)

        CASE('COIL:HEATING:STEAM')
          OutAirUnit(OAUnitNum)%OAEquip(CompNum)%ComponentType_Num= SteamCoil_AirHeat
          IF ((OutAirUnit(OAUnitNum)%OAEquip(CompNum)%MaxVolWaterFlow==AutoSize))THEN
            IsAutosize = .TRUE.
          END IF
          IF (CurZoneEqNum > 0) THEN
            IF (.NOT. IsAutosize .AND. .NOT. ZoneSizingRunDone) THEN ! Simulation continue
              IF (OutAirUnit(OAUnitNum)%OAEquip(CompNum)%MaxVolWaterFlow > 0.0d0) THEN
                CALL ReportSizingOutput('ZoneHVAC:OutdoorAirUnit', OutAirUnit(OAUnitNum)%Name, &
                                      'User-Specified Maximum Steam Flow [m3/s]', &
                                      OutAirUnit(OAUnitNum)%OAEquip(CompNum)%MaxVolWaterFlow)
              END IF
            ELSE
              CALL CheckZoneSizing('ZoneHVAC:OutdoorAirUnit', OutAirUnit(OAUnitNum)%Name)
              CoilSteamInletNode = OutAirUnit(OAUnitNum)%OAEquip(CompNum)%CoilWaterInletNode
              CoilSteamOutletNode =   &
                 GetCoilSteamOutletNode(OutAirUnit(OAUnitNum)%OAEquip(CompNum)%ComponentType,  &
                    OutAirUnit(OAUnitNum)%OAEquip(CompNum)%ComponentName,ErrorsFound)
              IF (IsAutosize) THEN
                PltSizHeatNum = MyPlantSizingIndex(OutAirUnit(OAUnitNum)%OAEquip(CompNum)%ComponentType,   &
                                OutAirUnit(OAUnitNum)%OAEquip(CompNum)%ComponentName, CoilSteamInletNode, &
                                             CoilSteamOutletNode, ErrorsFound)
                IF (PltSizHeatNum > 0) THEN
                  IF (FinalZoneSizing(CurZoneEqNum)%DesHeatMassFlow >= SmallAirVolFlow) THEN
                    CoilInTemp = FinalZoneSizing(CurZoneEqNum)%DesHeatCoilInTemp
                    CoilOutTemp = FinalZoneSizing(CurZoneEqNum)%HeatDesTemp
                    CoilOutHumRat = FinalZoneSizing(CurZoneEqNum)%HeatDesHumRat
                    DesCoilLoad = PsyCpAirFnWTdb(CoilOutHumRat, 0.5d0*(CoilInTemp+CoilOutTemp)) &
                                    * FinalZoneSizing(CurZoneEqNum)%DesHeatMassFlow &
                                    * (CoilOutTemp-CoilInTemp)
                    DesCoilLoad = MAX(0.d0, DesCoilLoad)
                    TempSteamIn= 100.00d0
                    EnthSteamInDry =  GetSatEnthalpyRefrig('STEAM',TempSteamIn,1.0d0,  &
                         OutAirUnit(OAUnitNum)%OAEquip(CompNum)%FluidIndex,'SizeOutdoorAirUnit')
                    EnthSteamOutWet=  GetSatEnthalpyRefrig('STEAM',TempSteamIn,0.0d0,  &
                         OutAirUnit(OAUnitNum)%OAEquip(CompNum)%FluidIndex,'SizeOutdoorAirUnit')
                    LatentHeatSteam=EnthSteamInDry-EnthSteamOutWet
                    SteamDensity=GetSatDensityRefrig('STEAM',TempSteamIn,1.0d0,  &
                        OutAirUnit(OAUnitNum)%OAEquip(CompNum)%FluidIndex,'SizeOutdoorAirUnit')
                  !DSU?  deal with steam properties
                    Cp = GetSpecificHeatGlycol('WATER', 60.d0, DummyWaterIndex, 'SizeOutdoorAirUnit')
                    rho = GetDensityGlycol('WATER', 60.d0, DummyWaterIndex, 'SizeOutdoorAirUnit')
                    MaxVolWaterFlowDes = DesCoilLoad/((PlantSizData(PltSizHeatNum)%DeltaT * &
                                       Cp * rho )+ &
                                       SteamDensity* LatentHeatSteam)
                  ELSE
                    MaxVolWaterFlowDes = 0.0d0
                  END IF
                ELSE
                  CALL ShowSevereError('Autosizing of Steam flow requires a Sizing:Zone object '//  &
                     'or a heating loop Sizing:Plant object')
                  CALL ShowContinueError('Occurs in ' // 'ZoneHVAC:OutdoorAirUnit' // ' Object=' &
                                     //TRIM(OutAirUnit(OAUnitNum)%Name))
                  ErrorsFound = .TRUE.
                END IF
              END IF
              IF (IsAutosize) THEN
                OutAirUnit(OAUnitNum)%OAEquip(CompNum)%MaxVolWaterFlow = MaxVolWaterFlowDes
                CALL ReportSizingOutput('ZoneHVAC:OutdoorAirUnit', OutAirUnit(OAUnitNum)%Name, &
                                        'Design Size Maximum Steam Flow [m3/s]', MaxVolWaterFlowDes)
              ELSE
                IF (OutAirUnit(OAUnitNum)%OAEquip(CompNum)%MaxVolWaterFlow > 0.0d0 .AND. MaxVolWaterFlowDes > 0.0d0) THEN
                  MaxVolWaterFlowUser = OutAirUnit(OAUnitNum)%OAEquip(CompNum)%MaxVolWaterFlow
                  CALL ReportSizingOutput('ZoneHVAC:OutdoorAirUnit', OutAirUnit(OAUnitNum)%Name, &
                                        'Design Size Maximum Steam Flow [m3/s]', MaxVolWaterFlowDes, &
                                        'User-Specified Maximum Steam Flow [m3/s]', MaxVolWaterFlowUser)
                  IF (DisplayExtraWarnings) THEN
                    IF ((ABS(MaxVolWaterFlowDes - MaxVolWaterFlowUser)/MaxVolWaterFlowUser) > AutoVsHardSizingThreshold) THEN
                      CALL ShowMessage('SizeOutdoorAirUnit: Potential issue with equipment sizing for ZoneHVAC:OutdoorAirUnit ' &
                                       //TRIM(OutAirUnit(OAUnitNum)%Name))
                      CALL ShowContinueError('User-Specified Maximum Steam Flow of '// &
                                      TRIM(RoundSigDigits(MaxVolWaterFlowUser,5))// ' [m3/s]')
                      CALL ShowContinueError('differs from Design Size Maximum Steam Flow of ' // &
                                      TRIM(RoundSigDigits(MaxVolWaterFlowDes,5))// ' [m3/s]')
                      CALL ShowContinueError('This may, or may not, indicate mismatched component sizes.')
                      CALL ShowContinueError('Verify that the value entered is intended and is consistent with other components.')
                    END IF
                  ENDIF
                END IF
              END IF
            END IF
          END IF

        CASE('COIL:COOLING:WATER:DETAILEDGEOMETRY')
          OutAirUnit(OAUnitNum)%OAEquip(CompNum)%ComponentType_Num= WaterCoil_DetailedCool
          IF ((OutAirUnit(OAUnitNum)%OAEquip(CompNum)%MaxVolWaterFlow==AutoSize))THEN
            IsAutosize = .TRUE.
          END IF
          IF (CurZoneEqNum > 0) THEN
            IF (.NOT. IsAutosize .AND. .NOT. ZoneSizingRunDone) THEN ! Simulation continue
              IF (OutAirUnit(OAUnitNum)%OAEquip(CompNum)%MaxVolWaterFlow > 0.0d0) THEN
                CALL ReportSizingOutput('ZoneHVAC:OutdoorAirUnit', OutAirUnit(OAUnitNum)%Name, &
                                      'User-Specified Maximum Cold Water Flow [m3/s]', &
                                      OutAirUnit(OAUnitNum)%OAEquip(CompNum)%MaxVolWaterFlow)
              END IF
            ELSE
              CALL CheckZoneSizing('ZoneHVAC:OutdoorAirUnit', OutAirUnit(OAUnitNum)%Name)
              CoilWaterInletNode = OutAirUnit(OAUnitNum)%OAEquip(CompNum)%CoilWaterInletNode
              CoilWaterOutletNode =OutAirUnit(OAUnitNum)%OAEquip(CompNum)%CoilWaterOutletNode
              IF (IsAutosize) THEN
                PltSizCoolNum = MyPlantSizingIndex(OutAirUnit(OAUnitNum)%OAEquip(CompNum)%ComponentType,  &
                                OutAirUnit(OAUnitNum)%OAEquip(CompNum)%ComponentName, CoilWaterInletNode, &
                                               CoilWaterOutletNode, ErrorsFound)
                IF (PltSizCoolNum > 0) THEN
                  IF (SizeAirMassFlow >= SmallAirVolFlow) THEN
                    CoilInTemp = FinalZoneSizing(CurZoneEqNum)%DesCoolCoilInTemp
                    CoilOutTemp = FinalZoneSizing(CurZoneEqNum)%CoolDesTemp
                    CoilOutHumRat = FinalZoneSizing(CurZoneEqNum)%CoolDesHumRat
                    CoilInHumRat = FinalZoneSizing(CurZoneEqNum)%DesCoolCoilInHumRat
                    DesCoilLoad = SizeAirMassFlow  &
                                * (PsyHFnTdbW(CoilInTemp, CoilInHumRat)-PsyHFnTdbW(CoilOutTemp, CoilOutHumRat))
                    DesCoilLoad = MAX(0.d0, DesCoilLoad)
                    rho = GetDensityGlycol(PlantLoop(OutAirUnit(OAUnitNum)%OAEquip(CompNum)%LoopNum )%fluidName, &
                                       5.d0, &
                                      PlantLoop( OutAirUnit(OAUnitNum)%OAEquip(CompNum)%LoopNum )%fluidIndex, &
                                        'SizeOutdoorAirUnit' )

                    Cp  = GetSpecificHeatGlycol(PlantLoop(OutAirUnit(OAUnitNum)%OAEquip(CompNum)%LoopNum )%fluidName, &
                                       5.d0, &
                                      PlantLoop( OutAirUnit(OAUnitNum)%OAEquip(CompNum)%LoopNum )%fluidIndex, &
                                        'SizeOutdoorAirUnit' )

                    MaxVolWaterFlowDes = DesCoilLoad / &
                                       ( PlantSizData(PltSizCoolNum)%DeltaT * &
                                       Cp  * rho )
                  ELSE
                    MaxVolWaterFlowDes = 0.0d0
                  END IF
                ELSE
                  CALL ShowSevereError('Autosizing of water flow requires a Sizing:Zone object '//  &
                     'or a cooling loop Sizing:Plant object')
                  CALL ShowContinueError('Occurs in ' // 'ZoneHVAC:OutdoorAirUnit' // ' Object=' &
                                     //TRIM(OutAirUnit(OAUnitNum)%Name))
                  Errorsfound = .TRUE.
                END IF
              END IF
              IF (IsAutosize) THEN
                OutAirUnit(OAUnitNum)%OAEquip(CompNum)%MaxVolWaterFlow = MaxVolWaterFlowDes
                CALL ReportSizingOutput('ZoneHVAC:OutdoorAirUnit', OutAirUnit(OAUnitNum)%Name, &
                                      'Design Size Maximum Cold Water Flow [m3/s]', MaxVolWaterFlowDes)
              ELSE
                IF (OutAirUnit(OAUnitNum)%OAEquip(CompNum)%MaxVolWaterFlow > 0.0d0 .AND. MaxVolWaterFlowDes > 0.0d0) THEN
                  MaxVolWaterFlowUser = OutAirUnit(OAUnitNum)%OAEquip(CompNum)%MaxVolWaterFlow
                  CALL ReportSizingOutput('ZoneHVAC:OutdoorAirUnit', OutAirUnit(OAUnitNum)%Name, &
                                      'Design Size Maximum Cold Water Flow [m3/s]', MaxVolWaterFlowDes, &
                                      'User-Specified Maximum Cold Water Flow [m3/s]', MaxVolWaterFlowUser)
                  IF (DisplayExtraWarnings) THEN
                    IF ((ABS(MaxVolWaterFlowDes - MaxVolWaterFlowUser)/MaxVolWaterFlowUser) > AutoVsHardSizingThreshold) THEN
                      CALL ShowMessage('SizeOutdoorAirUnit: Potential issue with equipment sizing for ZoneHVAC:OutdoorAirUnit ' &
                                         //TRIM(OutAirUnit(OAUnitNum)%Name))
                      CALL ShowContinueError('User-Specified Maximum Cold Water Flow of '// &
                                      TRIM(RoundSigDigits(MaxVolWaterFlowUser,5))// ' [m3/s]')
                      CALL ShowContinueError('differs from Design Size Maximum Cold Water Flow of ' // &
                                      TRIM(RoundSigDigits(MaxVolWaterFlowDes,5))// ' [m3/s]')
                      CALL ShowContinueError('This may, or may not, indicate mismatched component sizes.')
                      CALL ShowContinueError('Verify that the value entered is intended and is consistent with other components.')
                    END IF
                  ENDIF
                END IF
              END IF
            END IF
          END IF
          CALL SetCoilDesFlow(OutAirUnit(OAUnitNum)%OAEquip(CompNum)%ComponentType,  &
                                 OutAirUnit(OAUnitNum)%OAEquip(CompNum)%ComponentName,  &
                                 OutAirUnit(OAUnitNum)%OutAirVolFlow,ErrorsFound)

        CASE('COILSYSTEM:COOLING:WATER:HEATEXCHANGERASSISTED')
          OutAirUnit(OAUnitNum)%OAEquip(CompNum)%ComponentType_Num= WaterCoil_CoolingHXAsst
          IF ((OutAirUnit(OAUnitNum)%OAEquip(CompNum)%MaxVolWaterFlow==AutoSize))THEN
            IsAutosize = .TRUE.
          END IF
          IF (CurZoneEqNum > 0) THEN
            IF (.NOT. IsAutosize .AND. .NOT. ZoneSizingRunDone) THEN ! Simulation continue
              IF (OutAirUnit(OAUnitNum)%OAEquip(CompNum)%MaxVolWaterFlow > 0.0d0) THEN
                CALL ReportSizingOutput('ZoneHVAC:OutdoorAirUnit', OutAirUnit(OAUnitNum)%Name, &
                                      'User-Specified Maximum Cold Water Flow [m3/s]', &
                                      OutAirUnit(OAUnitNum)%OAEquip(CompNum)%MaxVolWaterFlow)
              END IF
            ELSE
              CALL CheckZoneSizing('ZoneHVAC:OutdoorAirUnit', OutAirUnit(OAUnitNum)%Name)
              CoolingCoilName = OutAirUnit(OAUnitNum)%OAEquip(CompNum)%ComponentName
              CoolingCoilType = OutAirUnit(OAUnitNum)%OAEquip(CompNum)%ComponentType
              CoilWaterInletNode = OutAirUnit(OAUnitNum)%OAEquip(CompNum)%CoilWaterInletNode
              CoilWaterOutletNode = OutAirUnit(OAUnitNum)%OAEquip(CompNum)%CoilWaterOutletNode
              IF (IsAutosize) THEN
                PltSizCoolNum = MyPlantSizingIndex(CoolingCoilType,CoolingCoilName, CoilWaterInletNode, &
                                             CoilWaterOutletNode, ErrorsFound)
                IF (PltSizCoolNum > 0) THEN
                  IF (FinalZoneSizing(CurZoneEqNum)%DesCoolMassFlow >= SmallAirVolFlow) THEN
                    CoilInTemp = FinalZoneSizing(CurZoneEqNum)%DesCoolCoilInTemp
                    CoilOutTemp = FinalZoneSizing(CurZoneEqNum)%CoolDesTemp
                    CoilOutHumRat = FinalZoneSizing(CurZoneEqNum)%CoolDesHumRat
                    CoilInHumRat = FinalZoneSizing(CurZoneEqNum)%DesCoolCoilInHumRat
                    DesCoilLoad = FinalZoneSizing(CurZoneEqNum)%DesCoolMassFlow &
                                * (PsyHFnTdbW(CoilInTemp, CoilInHumRat)-PsyHFnTdbW(CoilOutTemp, CoilOutHumRat))
                    rho = GetDensityGlycol(PlantLoop(OutAirUnit(OAUnitNum)%OAEquip(CompNum)%LoopNum )%fluidName, &
                                       5.d0, &
                                      PlantLoop( OutAirUnit(OAUnitNum)%OAEquip(CompNum)%LoopNum )%fluidIndex, &
                                        'SizeOutdoorAirUnit' )

                    Cp  = GetSpecificHeatGlycol(PlantLoop(OutAirUnit(OAUnitNum)%OAEquip(CompNum)%LoopNum )%fluidName, &
                                       5.d0, &
                                      PlantLoop( OutAirUnit(OAUnitNum)%OAEquip(CompNum)%LoopNum )%fluidIndex, &
                                        'SizeOutdoorAirUnit' )

                    MaxVolWaterFlowDes = DesCoilLoad / &
                                       ( PlantSizData(PltSizCoolNum)%DeltaT * &
                                       Cp  * rho )
                  ELSE
                    MaxVolWaterFlowDes = 0.0d0
                  END IF
                ELSE
                  CALL ShowSevereError('Autosizing of water flow requires a Sizing:Zone object '//  &
                     'or a cooling loop Sizing:Plant object')
                  CALL ShowContinueError('Occurs in ' // 'ZoneHVAC:OutdoorAirUnit' // ' Object=' &
                                     //TRIM(OutAirUnit(OAUnitNum)%Name))
                  Errorsfound = .TRUE.
                END IF
              END IF
              IF (IsAutosize) THEN
                OutAirUnit(OAUnitNum)%OAEquip(CompNum)%MaxVolWaterFlow = MaxVolWaterFlowDes
                CALL ReportSizingOutput('ZoneHVAC:OutdoorAirUnit', OutAirUnit(OAUnitNum)%Name, &
                                      'Design Size Maximum Cold Water Flow [m3/s]', MaxVolWaterFlowDes)
              ELSE
                IF (OutAirUnit(OAUnitNum)%OAEquip(CompNum)%MaxVolWaterFlow > 0.0d0 .AND. MaxVolWaterFlowDes > 0.0d0) THEN
                  MaxVolWaterFlowUser = OutAirUnit(OAUnitNum)%OAEquip(CompNum)%MaxVolWaterFlow
                  CALL ReportSizingOutput('ZoneHVAC:OutdoorAirUnit', OutAirUnit(OAUnitNum)%Name, &
                                      'Design Size Maximum Cold Water Flow [m3/s]', MaxVolWaterFlowDes, &
                                      'User-Specified Maximum Cold Water Flow [m3/s]', MaxVolWaterFlowUser)
                  IF (DisplayExtraWarnings) THEN
                    IF ((ABS(MaxVolWaterFlowDes - MaxVolWaterFlowUser)/MaxVolWaterFlowUser) > AutoVsHardSizingThreshold) THEN
                      CALL ShowMessage('SizeOutdoorAirUnit: Potential issue with equipment sizing for ZoneHVAC:OutdoorAirUnit ' &
                                       //TRIM(OutAirUnit(OAUnitNum)%Name))
                      CALL ShowContinueError('User-Specified Maximum Cold Water Flow of '// &
                                      TRIM(RoundSigDigits(MaxVolWaterFlowUser,5))// ' [m3/s]')
                      CALL ShowContinueError('differs from Design Size Maximum Cold Water Flow of ' // &
                                      TRIM(RoundSigDigits(MaxVolWaterFlowDes,5))// ' [m3/s]')
                      CALL ShowContinueError('This may, or may not, indicate mismatched component sizes.')
                      CALL ShowContinueError('Verify that the value entered is intended and is consistent with other components.')
                    END IF
                  ENDIF
                END IF
              END IF
            END IF
          END IF

        CASE('COILSYSTEM:COOLING:DX')
            OutAirUnit(OAUnitNum)%OAEquip(CompNum)%ComponentType_Num= DXSystem

        CASE('COILSYSTEM:HEATING:DX')
            OutAirUnit(OAUnitNum)%OAEquip(CompNum)%ComponentType_Num= DXHeatPumpSystem

        CASE('AIRLOOPHVAC:UNITARYSYSTEM')
            OutAirUnit(OAUnitNum)%OAEquip(CompNum)%ComponentType_Num= UnitarySystem

    ! Heat recovery
        CASE('HEATEXCHANGER:AIRTOAIR:FLATPLATE')
            OutAirUnit(OAUnitNum)%OAEquip(CompNum)%ComponentType_Num= HeatXchngr
        CASE('HEATEXCHANGER:AIRTOAIR:SENSIBLEANDLATENT')
            OutAirUnit(OAUnitNum)%OAEquip(CompNum)%ComponentType_Num= HeatXchngr
  !        CASE('HEATEXCHANGER:DESICCANT:BALANCEDFLOW')
  !          OutAirUnit(OAUnitNum)%OAEquip(CompNum)%ComponentType_Num= HeatXchngr
! Desiccant Dehumidifier
        CASE('DEHUMIDIFIER:DESICCANT:NOFANS')
            OutAirUnit(OAUnitNum)%OAEquip(CompNum)%ComponentType_Num= Desiccant
!       CASE('DEHUMIDIFIER:DESICCANT:SYSTEM')
   !         OutAirUnit(OAUnitNum)%OAEquip(CompNum)%ComponentType_Num= Desiccant
! Electric Heat Coil
        CASE('COIL:HEATING:ELECTRIC')
            OutAirUnit(OAUnitNum)%OAEquip(CompNum)%ComponentType_Num= Coil_ElectricHeat
! Gas Heat Coil
        CASE('COIL:HEATING:GAS')
            OutAirUnit(OAUnitNum)%OAEquip(CompNum)%ComponentType_Num= Coil_GasHeat
        CASE DEFAULT
          CALL ShowSevereError('ZoneHVAC:OutdoorAirUnit:EquipmentList'//' = "'//  &
             'OutAirUnit(OAUnitNum)%OAEquip(CompNum)%ComponentListName'//'" invalid to sizing'//  &
             'Outside Air Component="'//TRIM(OutAirUnit(OAUnitNum)%OAEquip(CompNum)%ComponentType)//'".')
            ErrorsFound=.true.

      END SELECT
    ENDDO
  ENDDO

  IF (ErrorsFound) THEN
    CALL ShowFatalError('Preceding sizing errors cause program termination')
  END IF

  RETURN

END SUBROUTINE SizeOutdoorAirUnit


SUBROUTINE CalcOutdoorAirUnit(OAUnitNum,ZoneNum,FirstHVACIteration,PowerMet,LatOutputProvided)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Young Tae Chae, Rick Strand
          !       DATE WRITTEN   June 2008
          !       MODIFIED       July 2012, Chandan Sharma - FSEC: Added zone sys avail managers
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine mainly controls the action of the outdoor air unit
          ! (or more exactly, it controls the coil outlet temperature of the unit)
          ! based on the user input for controls and the defined controls
          ! algorithms.

          ! METHODOLOGY EMPLOYED:
          ! Outdoor air unit is controlled based on user input and what is happening in the
          ! simulation.
          ! Note: controls are strictly temperature based and do not factor
          ! humidity into the equation (not an enthalpy economy cycle but rather
          ! a simple return air cycle).

          ! REFERENCES:
          ! ASHRAE Systems and Equipment Handbook (SI), 1996. page 31.3

          ! USE STATEMENTS:


  USE DataZoneEnergyDemands
  USE DataEnvironment,           ONLY : OutDryBulbTemp, OutWetBulbTemp, EnvironmentName, CurMnDy, OutBaroPress
  USE DataHeatBalFanSys,         ONLY : MAT,ZoneAirHumRat
  USE DataLoopNode,              ONLY : Node
  USE ScheduleManager,           ONLY : GetCurrentScheduleValue
  USE HeatingCoils,              ONLY : CheckHeatingCoilSchedule
  USE WaterCoils,                ONLY : CheckWaterCoilSchedule
  USE HVACHXAssistedCoolingCoil, ONLY : CheckHXAssistedCoolingCoilSchedule
  Use SteamCoils,                ONLY : CheckSteamCoilSchedule
  USE Fans,                      ONLY : SimulateFanComponents
  USE DataHVACGlobals,           ONLY : ZoneCompTurnFansOn, ZoneCompTurnFansOff

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(INOUT)      :: OAUnitNum          ! number of the current unit being simulated
  INTEGER, INTENT(IN)         :: ZoneNum            ! number of zone being served
  LOGICAL, INTENT(IN)         :: FirstHVACIteration ! TRUE if 1st HVAC simulation of system timestep
  REAL(r64),    INTENT(OUT)   :: PowerMet           ! power supplied
  REAL(r64), INTENT (OUT)     :: LatOutputProvided  ! Latent power supplied (kg/s), negative = dehumidification


          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! INTERFACE BLOCK SPECIFICATIONS



          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  CHARACTER(len=*), PARAMETER  :: CurrentModuleObject='ZoneHVAC:OutdoorAirUnit'
  INTEGER                      :: CompNum
  CHARACTER(len=MaxNameLength) :: EquipType
  CHARACTER(len=MaxNameLength) :: EquipName
  CHARACTER(len=MaxNameLength) :: CtrlName
  LOGICAL                      :: Sim
  LOGICAL                      :: ReSim
  REAL(r64)                    :: DesOATemp        ! Design OA Temp degree C
  REAL(r64)                    :: AirMassFlow      ! air mass flow rate [kg/s]
  INTEGER                      :: ControlNode      ! the hot water or cold water inlet node
  INTEGER                      :: InletNode        ! Unit air inlet node
  INTEGER                      :: SFanOutletNode   ! Unit supply fan outlet node
  INTEGER                      :: ZoneAirInNode    ! zone supply air node
  REAL(r64)                    :: MaxWaterFlow     ! maximum water flow for heating or cooling [kg/sec]
  REAL(r64)                    :: MinWaterFlow     ! minimum water flow for heating or cooling [kg/sec]
  INTEGER                      :: OutletNode       ! air outlet node
  INTEGER                      :: OutsideAirNode   ! outside air node
  REAL(r64)                    :: QTotUnitOut      ! total unit output [watts]
  REAL(r64)                    :: QUnitOut         ! heating or sens. cooling provided by fan coil unit [watts]
  REAL(r64)                    :: LatLoadMet         ! heating or sens. cooling provided by fan coil unit [watts]
  REAL(r64)                    :: MinHumRat        ! desired temperature after mixing inlet and outdoor air [degrees C]
  REAL(r64)                    :: SetpointTemp     ! temperature that will be used to control the radiant system [Celsius]
  REAL(r64)                    :: HiCtrlTemp       ! Current high point in setpoint temperature range
  REAL(r64)                    :: LoCtrlTemp       ! Current low point in setpoint temperature range
  REAL(r64)                    :: CpFan            ! Intermediate calculational variable for specific heat of air <<NOV9 Updated
  REAL(r64)                    :: airinent         ! RE-calcualte the Enthalpy of supply air
  REAL(r64)                    :: outsideent       ! RE-calculate the Enthalpy of outdoor air
  REAL(r64)                    :: AirOutletTemp
  INTEGER                      :: OperatingMode   =0
  INTEGER                      :: UnitControlType =0
  REAL(r64)                    :: OutSideAirEnt     ! Specific humidity ratio of outlet air (kg moisture / kg moist air)
  REAL(r64)                    :: ZoneSupAirEnt     ! Specific humidity ratio of inlet air (kg moisture / kg moist air)
! Latent output
  REAL(r64)                    :: LatentOutput   ! Latent (moisture) add/removal rate, negative is dehumidification [kg/s]
  REAL(r64)                    :: SpecHumOut     ! Specific humidity ratio of outlet air (kg moisture / kg moist air)
  REAL(r64)                    :: SpecHumIn      ! Specific humidity ratio of inlet air (kg moisture / kg moist air)
  REAL(r64)                    :: OAMassFlowRate
  REAL(r64)                    :: EAMassFlowRate
  LOGICAL                      :: ErrorsFound=.false. ! Set to true if errors in input, fatal at end of routine
  LOGICAL                      :: FatalErrorFlag
  REAL(r64)                    :: ZoneAirEnt    ! zone air enthalphy J/kg

          ! FLOW:

  FanElecPower = 0.0D0
          ! initialize local variables
  ControlNode    = 0
  QUnitOut       = 0.0D0
  IF (OutAirUnit(OAUnitNum)%ExtFan ) InletNode   = OutAirUnit(OAUnitNum)%AirInletNode
  SFanOutletNode = OutAirUnit(OAUnitNum)%SFanOutletNode
  OutletNode     = OutAirUnit(OAUnitNum)%AirOutletNode
  OutsideAirNode = OutAirUnit(OAUnitNum)%OutsideAirNode
  OperatingMode  = OutAirUnit(OAUnitNum)%OperatingMode
  UnitControltype = OutAirUnit(OAUnitNum)%ControlType
  AirOutletTemp =0.0d0
  OutAirUnit(OAUnitNum)%CompOutSetTemp=0.0d0
  OutAirUnit(OAUnitNum)%FanEffect=.FALSE.

  IF ((GetCurrentScheduleValue(OutAirUnit(OAUnitNum)%SchedPtr) <= 0) .OR.   &
      (GetCurrentScheduleValue(OutAirUnit(OAUnitNum)%OutAirSchedPtr) <= 0) .OR. &
      (GetCurrentScheduleValue(OutAirUnit(OAUnitNum)%SFanAvailSchedPtr) <= 0) .AND. &
       .NOT. ZoneCompTurnFansOn .OR. ZoneCompTurnFansOff) THEN
          ! System is off or has no load upon the unit; set the flow rates to zero and then
          ! simulate the components with the no flow conditions
    IF (OutAirUnit(OAUnitNum)%ExtFan ) Node(InletNode)%MassFlowRate                  = 0.0d0
    IF (OutAirUnit(OAUnitNum)%ExtFan ) Node(InletNode)%MassFlowRateMaxAvail          = 0.0d0
    IF (OutAirUnit(OAUnitNum)%ExtFan ) Node(InletNode)%MassFlowRateMinAvail          = 0.0d0
    Node(SFanOutletNode)%MassFlowRate             = 0.0d0
    Node(SFanOutletNode)%MassFlowRateMaxAvail     = 0.0d0
    Node(SFanOutletNode)%MassFlowRateMinAvail     = 0.0d0
    Node(OutletNode)%MassFlowRate                 = 0.0d0
    Node(OutletNode)%MassFlowRateMaxAvail         = 0.0d0
    Node(OutletNode)%MassFlowRateMinAvail         = 0.0d0
    Node(OutsideAirNode)%MassFlowRate             = 0.0d0
    Node(OutsideAirNode)%MassFlowRateMaxAvail     = 0.0d0
    Node(OutsideAirNode)%MassFlowRateMinAvail     = 0.0d0
    AirMassFlow                                   = Node(SFanOutletNode)%MassFlowRate

! Node condition
    IF (OutAirUnit(OAUnitNum)%ExtFan ) THEN
      Node(InletNode)%Temp      = MAT(ZoneNum)
      Node(SFanOutletNode)%Temp = Node(InletNode)%Temp
    ELSE
      Node(SFanOutletNode)%Temp = MAT(ZoneNum)
    ENDIF
    Node(OutletNode)%Temp     = Node(SFanOutletNode)%Temp

    IF (OutAirUnit(OAUnitNum)%FanPlace .EQ. BlowThru) THEN
      CALL SimulateFanComponents(OutAirUnit(OAUnitNum)%SFanName,FirstHVACIteration,OutAirUnit(OAUnitNum)%SFan_Index, &
                                   ZoneCompTurnFansOn = ZoneCompTurnFansOn,ZoneCompTurnFansOff = ZoneCompTurnFansOff)
      OutAirUnit(OAUnitNum)%ElecFanRate=OutAirUnit(OAUnitNum)%ElecFanRate+FanElecPower
      CALL SimZoneOutAirUnitComps(OAUnitNum,FirstHVACIteration)
      IF (OutAirUnit(OAUnitNum)%ExtFan ) CALL SimulateFanComponents(OutAirUnit(OAUnitNum)%ExtFanName, &
                                               FirstHVACIteration,OutAirUnit(OAUnitNum)%ExtFan_Index)
    ELSE IF(OutAirUnit(OAUnitNum)%FanPlace .EQ. DrawThru) THEN
      CALL SimZoneOutAirUnitComps(OAUnitNum,FirstHVACIteration)
      CALL SimulateFanComponents(OutAirUnit(OAUnitNum)%SFanName,FirstHVACIteration,OutAirUnit(OAUnitNum)%SFan_Index, &
                                   ZoneCompTurnFansOn = ZoneCompTurnFansOn,ZoneCompTurnFansOff = ZoneCompTurnFansOff)
      IF (OutAirUnit(OAUnitNum)%ExtFan ) CALL SimulateFanComponents(OutAirUnit(OAUnitNum)%ExtFanName, &
                                               FirstHVACIteration,OutAirUnit(OAUnitNum)%ExtFan_Index)
    END IF

  ELSE ! System On

!Flowrate Check
    IF(Node(OutsideAirNode)%MassFlowRate > 0.0d0) Then
      Node(OutsideAirNode)%MassFlowRate= OutAirUnit(OAUnitNum)%OutAirMassFlow
    ENDIF

!Fan Positioning Check

    IF (OutAirUnit(OAUnitNum)%ExtFan ) THEN
      Node(InletNode)%MassFlowRate=OutAirUnit(OAUnitNum)%ExtAirMassFlow
    END IF

!Air mass balance check (removed because exhaust and supply can be imbalanced
!    IF ((Node(InletNode)%MassFlowRate > Node(OutsideAirNode)%MassFlowRate) &
!             .OR.(Node(InletNode)%MassFlowRate < Node(OutsideAirNode)%MassFlowRate)) THEN
!      OutAirUnit(OAUnitNum)%UnBalancedErrCount = OutAirUnit(OAUnitNum)%UnBalancedErrCount + 1
!      IF (OutAirUnit(OAUnitNum)%UnBalancedErrCount .EQ. 1) THEN
!        CALL ShowWarningError('Air mass flow between zone supply and exhaust is not balanced')
!        CALL ShowContinueError('Occurs in ' // 'ZoneHVAC:OutdoorAirUnit' // ' Object=' &
!                               //TRIM(OutAirUnit(OAUnitNum)%Name))
!        CALL ShowContinueError('Air mass balance is required by other outdoor air units,'// &
!                                  'ZoneMixing, ZoneCrossMixing, or other air flow control inputs.')
!!
!!  CALL ShowContinueErrorTimeStamp('Air volume flow rate ratio = '//TRIM(RoundSigDigits(HXAirVolFlowRatio,3))//'.')
!!ELSE
!! CALL ShowRecurringWarningErrorAtEnd(TRIM(OutAirUnit(OAUnitNum)%Name)//&
!! ':  Air mass balance is required by other outdoor air units, ZoneMixing, ZoneCrossMixing, or other air flow control inputs.'&
!!   , OutAirUnit(OAUnitNum)%UnBalancedErrIndex)
!      END IF
!    END IF

    IF (OutAirUnit(OAUnitNum)%FanPlace .EQ. BlowThru) THEN
      CALL SimulateFanComponents(OutAirUnit(OAUnitNum)%SFanName,FirstHVACIteration,OutAirUnit(OAUnitNum)%SFan_Index)
        DesOATemp = Node(SFanOutletNode)%Temp
    ELSE IF(OutAirUnit(OAUnitNum)%FanPlace .EQ. DrawThru) THEN
        DesOATemp = Node(OutsideAirNode)%Temp
    END IF

!Control type check
    SELECT CASE (UnitControlType)
      CASE (Neutral)
        SetpointTemp = MAT(ZoneNum)
!Neutral Control Condition
        IF (DesOATemp == SetpointTemp) THEN
          OutAirUnit(OAUnitNum)%OperatingMode = NeutralMode
          AirOutletTemp = DesOATemp
          OutAirUnit(OAUnitNum)%CompOutSetTemp=DesOATemp
          Call SimZoneOutAirUnitComps(OAUnitNum,FirstHVACIteration)
        ELSE
          IF (DesOATemp < SetpointTemp) THEN  ! Heating MODE
            OutAirUnit(OAUnitNum)%OperatingMode = HeatingMode
            AirOutletTemp = SetpointTemp
            OutAirUnit(OAUnitNum)%CompOutSetTemp=AirOutletTemp
            Call SimZoneOutAirUnitComps(OAUnitNum,FirstHVACIteration)
          ELSE IF (DesOATemp > SetpointTemp) THEN !Cooling Mode
            OutAirUnit(OAUnitNum)%OperatingMode = CoolingMode
            AirOutletTemp = SetpointTemp
            OutAirUnit(OAUnitNum)%CompOutSetTemp=AirOutletTemp
            Call SimZoneOutAirUnitComps(OAUnitNum,FirstHVACIteration)
          ENDIF
        END IF
!SetPoint Temperature Condition
      CASE (Temperature)
        SetpointTemp = DesOATemp
        HiCtrlTemp = GetCurrentScheduleValue(OutAirUnit(OAUnitNum)%HiCtrlTempSchedPtr)
        LoCtrlTemp = GetCurrentScheduleValue(OutAirUnit(OAUnitNum)%LoCtrlTempSchedPtr)
        IF ((DesOATemp <= HiCtrlTemp).AND.(DesOATemp >=LoCtrlTemp)) THEN
          OutAirUnit(OAUnitNum)%OperatingMode = NeutralMode
          AirOutletTemp = DesOATemp
          OutAirUnit(OAUnitNum)%CompOutSetTemp=DesOATemp
          Call SimZoneOutAirUnitComps(OAUnitNum,FirstHVACIteration)
        ELSE
          IF (SetpointTemp < LoCtrlTemp) THEN
            OutAirUnit(OAUnitNum)%OperatingMode=HeatingMode
            AirOutletTemp = LoCtrlTemp
            OutAirUnit(OAUnitNum)%CompOutSetTemp=AirOutletTemp
              Call SimZoneOutAirUnitComps(OAUnitNum,FirstHVACIteration)
          ELSE  IF (SetpointTemp > HiCtrlTemp) THEN
            OutAirUnit(OAUnitNum)%OperatingMode = CoolingMode
            AirOutletTemp = HiCtrlTemp
            OutAirUnit(OAUnitNum)%CompOutSetTemp=AirOutletTemp
              Call SimZoneOutAirUnitComps(OAUnitNum,FirstHVACIteration)
          END IF
        END IF
    END SELECT

! Fan positioning
    IF(OutAirUnit(OAUnitNum)%FanPlace .EQ. DrawThru) THEN
      CALL SimulateFanComponents(OutAirUnit(OAUnitNum)%SFanName,FirstHVACIteration,OutAirUnit(OAUnitNum)%SFan_Index)
        OutAirUnit(OAUnitNum)%FanEffect=.TRUE. !RE-Simulation to take over the supply fan effect
        OutAirUnit(OAUnitNum)%FanCorTemp=(Node(Outletnode)%Temp-OutAirUnit(OAUnitNum)%CompOutSetTemp)
      CALL SimZoneOutAirUnitComps(OAUnitNum,FirstHVACIteration)
      CALL SimulateFanComponents(OutAirUnit(OAUnitNum)%SFanName,FirstHVACIteration,OutAirUnit(OAUnitNum)%SFan_Index)

      OutAirUnit(OAUnitNum)%FanEffect=.FALSE.
    END IF
    IF (OutAirUnit(OAUnitNum)%ExtFan ) CALL SimulateFanComponents(OutAirUnit(OAUnitNum)%ExtFanName, &
                                             FirstHVACIteration,OutAirUnit(OAUnitNum)%ExtFan_Index)
  END IF    ! ...end of system ON/OFF IF-THEN block

  AirMassFlow = Node(OutletNode)%MassFlowRate
  MinHumRat   = MIN(Node(OutletNode)%HumRat,Node(OutAirUnit(OAUnitNum)%ZoneNodeNum)%HumRat)

  AirInEnt = PsyHFnTdbW(Node(OutletNode)%Temp,MinHumRat)       ! zone supply air node enthalpy
  ZoneAirEnt = PsyHFnTdbW(Node(OutAirUnit(OAUnitNum)%ZoneNodeNum)%Temp,MinHumRat) ! zone air enthalpy
  QUnitOut = AirMassFlow*(AirInEnt-ZoneAirEnt)                 ! Senscooling

! CR9155 Remove specific humidity calculations
  SpecHumOut = Node(OutletNode)%HumRat
  SpecHumIn  = Node(OutAirUnit(OAUnitNum)%ZoneNodeNum)%HumRat
  LatentOutput = AirMassFlow * (SpecHumOut - SpecHumIn) ! Latent rate (kg/s), dehumid = negative

  ZoneAirEnT=PsyHFnTdbW(Node(OutAirUnit(OAUnitNum)%ZoneNodeNum)%Temp,Node(OutAirUnit(OAUnitNum)%ZoneNodeNum)%HumRat)

  ZoneSupAirEnT=PsyHFnTdbW(Node(OutletNode)%Temp,Node(OutletNode)%HumRat)
  QTotUnitOut = AirMassFlow * (ZoneSupAirEnt-ZoneAirEnT)
  LatLoadMet = QTotUnitOut - QUnitOut ! watts

          ! Report variables...

  IF(QUnitOut .LT. 0.0d0) THEN
    OutAirUnit(OAUnitNum)%SensCoolingRate = ABS(QUnitOut)
    OutAirUnit(OAUnitNum)%SensHeatingRate = 0.0d0
  ELSE
    OutAirUnit(OAUnitNum)%SensCoolingRate = 0.0d0
    OutAirUnit(OAUnitNum)%SensHeatingRate = QUnitOut
  END IF

  IF(QTotUnitOut .LT. 0.0d0) THEN
    OutAirUnit(OAUnitNum)%TotCoolingRate = ABS(QTotUnitOut)
    OutAirUnit(OAUnitNum)%TotHeatingRate = 0.0d0
  ELSE
    OutAirUnit(OAUnitNum)%TotCoolingRate = 0.0d0
    OutAirUnit(OAUnitNum)%TotHeatingRate = QTotUnitOut
  END IF

  IF(LatLoadMet .LT. 0.0d0) THEN
    OutAirUnit(OAUnitNum)%LatCoolingRate = ABS(LatLoadMet)
    OutAirUnit(OAUnitNum)%LatHeatingRate = 0.0d0
  ELSE
    OutAirUnit(OAUnitNum)%LatCoolingRate = 0.0d0
    OutAirUnit(OAUnitNum)%LatHeatingRate = LatLoadMet
  END IF

  OutAirUnit(OAUnitNum)%ElecFanRate      = FanElecPower

  PowerMet = QUnitOut
  LatOutputProvided = LatentOutput

  RETURN

END SUBROUTINE CalcOutdoorAirUnit


SUBROUTINE  SimZoneOutAirUnitComps(OAUnitNum,FirstHVACIteration)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   Oct 1998
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE
          ! Simulate the controllers and components in the outside air system.

          ! METHODOLOGY EMPLOYED:

          ! REFERENCES:

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: FindItemInList

  IMPLICIT NONE

          ! SUBROUTINE ARGUMENT DEFINITIONS
  INTEGER, INTENT(IN) :: OAUnitNum
  LOGICAL, INTENT(IN) :: FirstHVACIteration

          ! SUBROUTINE PARAMETER DEFINITIONS:

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na
          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER                      :: EquipNum
  INTEGER                      :: CurOAUnitNum
  CHARACTER(len=MaxNameLength) :: EquipType
  CHARACTER(len=MaxNameLength) :: EquipName
  LOGICAL                      :: FatalErrorFlag
  LOGICAL                      :: Sim

  FatalErrorFlag = .FALSE.
  CurOAUnitNum = OAUnitNum
  Sim = .TRUE.


  DO EquipNum=1, OutAirUnit(OAUnitNum)%NumComponents
    EquipType = OutAirUnit(OAUnitNum)%OAEquip(EquipNum)%ComponentType
    EquipName = OutAirUnit(OAUnitNum)%OAEquip(EquipNum)%ComponentName
    CALL SimOutdoorAirEquipComps(OAUnitNum,EquipType,EquipName,EquipNum,  &
                    OutAirUnit(OAUnitNum)%OAEquip(EquipNum)%ComponentType_Num,  &
                    FirstHVACIteration,OutAirUnit(OAUnitNum)%OAEquip(EquipNum)%ComponentIndex,Sim)
  END DO

  CurOAUnitNum = 0

  RETURN

END SUBROUTINE SimZoneOutAirUnitComps

SUBROUTINE SimOutdoorAirEquipComps(OAUnitNum,EquipType,EquipName,EquipNum,CompTypeNum,FirstHVACIteration,CompIndex,Sim)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Young Tae Chae, Rick Strand
          !       DATE WRITTEN   June 2008
          !       MODIFIED
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Outdoor air unit has various coil options. This subroutine defines the coil loads and execute
          ! to simulate each components
          !
          ! METHODOLOGY EMPLOYED:


          ! REFERENCES:

          ! USE STATEMENTS:


  USE DataZoneEnergyDemands
  USE DataEnvironment,           ONLY : OutDryBulbTemp, OutWetBulbTemp, EnvironmentName, CurMnDy, OutBaroPress
  USE DataHeatBalance,           ONLY : MRT
  USE DataHeatBalFanSys,         ONLY : MAT,ZoneAirHumRat
  USE DataHVACGlobals,           ONLY : SmallLoad
  USE DataLoopNode,              ONLY : Node
  USE ScheduleManager,           ONLY : GetCurrentScheduleValue
  USE General,                   ONLY : TrimSigDigits
  USE NodeInputManager,          ONLY : GetOnlySingleNode
  Use Fans,                      Only:SimulateFanComponents
  Use WaterCoils,                Only:SimulateWaterCoilComponents
  Use HeatingCoils,              Only:SimulateHeatingCoilComponents
  Use HeatRecovery,              Only: SimHeatRecovery
  Use DesiccantDehumidifiers,    Only:SimDesiccantDehumidifier
  Use HVACHXAssistedCoolingCoil, Only:SimHXAssistedCoolingCoil
  Use HVACDXSystem,              Only: SimDXCoolingSystem
  Use HVACDXHeatPumpSystem,      Only: SimDXHeatPumpSystem
  Use SteamCoils,                Only:SimulateSteamCoilComponents
  Use DataInterfaces,            Only:ControlCompOutput
  USE HVACUnitarySystem,         ONLY:SimUnitarySystem
!  Use TranspiredCollector, Only:SimTranspiredCollector
!  Use EvaporativeCoolers, Only:SimEvapCooler
!  USE PhotovoltaicThermalCollectors, ONLY:SimPVTcollectors, CalledFromOutsideAirSystem

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine
          ! SUBROUTINE ARGUMENT DEFINITIONS:
  LOGICAL, INTENT (IN)          :: FirstHVACIteration
  CHARACTER(len=*), INTENT (IN) :: EquipType ! the component type
  CHARACTER(len=*), INTENT (IN) :: EquipName ! the component Name
  INTEGER, INTENT(IN)           :: CompTypeNum ! Component Type -- Integerized for this module
  INTEGER, INTENT(IN)           :: EquipNum
  INTEGER, INTENT(IN)           :: OAUnitNum ! actual outdoor air unit num
  INTEGER, INTENT(INOUT)        :: CompIndex
  LOGICAL, INTENT(IN)           :: Sim        ! if TRUE, simulate component

           ! SUBROUTINE PARAMETER DEFINITIONS: None

           ! INTERFACE BLOCK DEFINITIONS:
           ! see use DataInterfaces

           ! DERIVED TYPE DEFINITIONS: None

           ! SUBROUTINE LOCAL VARIABLE DEFINITIONS
  INTEGER      :: OperatingMode
  REAL(r64)    :: OAMassFlow
  REAL(r64)    :: QCompReq
  INTEGER      :: UnitNum
  REAL(r64)    :: MaxWaterFlow
  REAL(r64)    :: MinWaterFlow
  INTEGER      :: ControlNode
  INTEGER      :: CoilInletNode
  INTEGER      :: OutletNode
  REAL(r64)    :: CpAirZn
  INTEGER      :: AirOutletNode
  INTEGER      :: CoilWaterInletNode
  INTEGER      :: SimCompNum
  INTEGER      :: OpMode
  INTEGER      :: EquipTypeNum
  INTEGER      :: WCCoilInletNode
  INTEGER      :: WCCoilOutletNode
  INTEGER      :: WCCoilContNode
  INTEGER      :: WHCoilInletNode
  INTEGER      :: WHCoilOutletNode
  INTEGER      :: WHCoilContNode
  INTEGER      :: SHCoilInletNode
  INTEGER      :: SHCoilOutletNode
  REAL(r64)    :: Qcoilout
  REAL(r64)    :: QUnitOut
  INTEGER      :: DXSystemIndex =0
  REAL(r64)    :: CompAirOutTemp
  REAL(r64)    :: Faneffect
  LOGICAL      :: DrawFan              !fan position If .True., the temperature increasing by fan operating is considered
  REAL(r64)    :: Dxsystemouttemp
  REAL(r64)    :: DXsystemInlettemp
  LOGICAL      :: ErrorsFound=.FALSE. ! Set to true if errors in input, fatal at end of routine
  LOGICAL      :: HeatActive=.FALSE.
  LOGICAL      :: CoolActive=.FALSE.


  ! Flow!
  UnitNum        = OAUnitNum
  CompAirOutTemp = OutAirUnit(OAUnitNum)%CompOutSetTemp
  OPmode         = OutAirUnit(OAUnitNum)%OperatingMode
  SimCompNum     = EquipNum
  EquipTypeNum   = OutAirUnit(OAUnitNum)%OAEquip(SimCompNum)%ComponentType_Num
  OAMassFlow     = OutAirUnit(OAUnitNum)%OutAirMassFlow
  DrawFan        = OutAirUnit(OAUnitNum)%FanEffect
  DXSystemIndex  = 0


  !check the fan positioning
  IF (DrawFan) THEN
    Faneffect     = OutAirUnit(OAUnitNum)%FanCorTemp ! Heat effect by fan
  ELSE
    Faneffect     = 0.0d0
  END IF

  ! checking equipment index

  SELECT CASE(EquipTypeNum)

  ! Heat recovery
    CASE(HeatXchngr)  ! 'HeatExchanger:AirToAir:FlatPlate', 'HeatExchanger:AirToAir:SensibleAndLatent',
                      ! 'HeatExchanger:Desiccant:BalancedFlow'
      IF (Sim) Then
        CALL SimHeatRecovery(EquipName,FirstHVACIteration,CompIndex, ContFanCycCoil, &
             EconomizerFlag=.FALSE., &
             HighHumCtrlFlag=.FALSE.)
      END IF
  ! Desiccant Dehumidifier
    CASE(Desiccant)  ! 'Dehumidifier:Desiccant:NoFans'
      IF (Sim) Then
        CALL SimDesiccantDehumidifier(EquipName,FirstHVACIteration,CompIndex)
      END IF

    CASE(WaterCoil_SimpleHeat) ! ('Coil:Heating:Water')

      IF (Sim) Then
        ControlNode   = OutAirUnit(OAUnitNum)%OAEquip(EquipNum)%CoilWaterInletNode
        MaxWaterFlow  = OutAirUnit(OAUnitNum)%OAEquip(EquipNum)%MaxWaterMassFlow
        MinWaterFlow  = OutAirUnit(OAUnitNum)%OAEquip(EquipNum)%MinWaterMassFlow
  !On the first HVAC iteration the system values are given to the controller, but after that
  ! the demand limits are in place and there needs to be feedback to the Zone Equipment
        IF((.not. FirstHVACIteration) .and. (ControlNode > 0)) Then
          MaxWaterFlow = Node(ControlNode)%MassFlowRateMaxAvail
          MinWaterFlow = Node(ControlNode)%MassFlowRateMinAvail
        END IF
        WHCoilInletNode =OutAirUnit(OAUnitNum)%OAEquip(EquipNum)%CoilAirInletNode
        WHCoilOutletNode=OutAirUnit(OAUnitNum)%OAEquip(EquipNum)%CoilAirOutletNode

        CpAirZn        = PsyCpAirFnWTdb(Node(WHCoilInletNode)%HumRat,Node(WHCoilInletNode)%Temp)

        IF((OPMode == NeutralMode).OR.(OPMode == CoolingMode)&
            .OR.(Node(WHCoilInletNode)%Temp > CompAirOutTemp) ) THEN
           QCompReq=0.0d0
        ELSE
          QCompReq=CpAirZn*OAMassFlow*((CompAirOutTemp-Node(WHCoilInletNode)%Temp)-faneffect)
          IF (ABS(QCompReq) < SmallLoad) QCompReq = 0.d0
          IF (QCompReq < 0.d0) QCompReq = 0.d0 ! coil can heat only
        END IF

        CALL ControlCompOutput(CompName=OutAirUnit(OAUnitNum)%Name, CompType=cMO_OutdoorAirUnit,CompNum=UnitNum, &
                             FirstHVACIteration=FirstHVACIteration,QZnReq=QCompReq, &
                             ActuatedNode=ControlNode,MaxFlow=MaxWaterFlow, &
                             MinFlow=MinWaterFlow,ControlOffSet=0.0001d0,Action=2,  &
                             ControlCompTypeNum=OutAirUnit(OAUnitNum)%ControlCompTypeNum,&
                             CompErrIndex=OutAirUnit(OAUnitNum)%CompErrIndex,EquipIndex=SimCompNum,&
                             LoopNum     = OutAirUnit(OAUnitNum)%OAEquip(EquipNum)%LoopNum,&
                             LoopSide    = OutAirUnit(OAUnitNum)%OAEquip(EquipNum)%LoopSideNum,&
                             BranchIndex = OutAirUnit(OAUnitNum)%OAEquip(EquipNum)%BranchNum)
      END IF

    CASE(SteamCoil_AirHeat)    ! 'Coil:Heating:Steam'
      IF (Sim) Then
        Call CalcOAUnitCoilComps(unitnum,FirstHVACIteration,SimCompNum,QunitOut)
      END IF

    CASE(Coil_ElectricHeat)    ! 'Coil:Heating:Electric'
      IF (Sim) Then
  !     stand-alone coils are temperature controlled (do not pass QCoilReq in argument list, QCoilReq overrides temp SP)
        Call CalcOAUnitCoilComps(unitnum,FirstHVACIteration,SimCompNum,QunitOut)
      END IF

    CASE(Coil_GasHeat)  ! 'Coil:Heating:Gas'
      IF (Sim) Then
  !     stand-alone coils are temperature controlled (do not pass QCoilReq in argument list, QCoilReq overrides temp SP)
        Call CalcOAUnitCoilComps(unitnum,FirstHVACIteration,SimCompNum,QunitOut)
      END IF

  ! water cooling coil Types

     CASE(WaterCoil_Cooling)    ! 'Coil:Cooling:Water'
      IF (Sim) Then
        ControlNode   = OutAirUnit(OAUnitNum)%OAEquip(EquipNum)%CoilWaterInletNode
        MaxWaterFlow  = OutAirUnit(OAUnitNum)%OAEquip(EquipNum)%MaxWaterMassFlow
        MinWaterFlow  = OutAirUnit(OAUnitNum)%OAEquip(EquipNum)%MinWaterMassFlow
  ! On the first HVAC iteration the system values are given to the controller, but after that
  ! the demand limits are in place and there needs to be feedback to the Zone Equipment
        If((.not. FirstHVACIteration) .and. (ControlNode > 0)) Then
          MaxWaterFlow = Node(ControlNode)%MassFlowRateMaxAvail
          MinWaterFlow = Node(ControlNode)%MassFlowRateMinAvail
        END IF
        WCCoilInletNode=OutAirUnit(OAUnitNum)%OAEquip(EquipNum)%CoilAirInletNode
        WCCoilOutletNode=OutAirUnit(OAUnitNum)%OAEquip(EquipNum)%CoilAirOutletNode

        CpAirZn        = PsyCpAirFnWTdb(Node(WCCoilInletNode)%HumRat,Node(WCCoilInletNode)%Temp)
        OAMassFlow   =  OutAirUnit(OAUnitNum)%OutAirMassFlow
        IF((OPMode == NeutralMode).OR.(OPMode == HeatingMode) &
            .OR.(Node(WCCoilInletNode)%Temp<CompAirOutTemp)) THEN
           QCompReq=0.0d0
          Node(WCCoilOutletNode)%Temp=Node(WCCoilInletNode)%Temp
          Node(WCCoilOutletNode)%HumRat=Node(WCCoilInletNode)%HumRat
          Node(WCCoilOutletNode)%MassFlowRate=Node(WCCoilInletNode)%MassFlowRate

        ELSE

          QCompReq=CpAirZn*OAMassFlow*((CompAirOutTemp-Node(WCCoilInletNode)%Temp)-faneffect)
          IF (ABS(QCompReq) < SmallLoad) QCompReq = 0.d0
          IF (QCompReq > 0.d0) QCompReq = 0.d0 ! coil can cool only
        END IF


        CALL ControlCompOutput(CompName=OutAirUnit(OAUnitNum)%Name, CompType=cMO_OutdoorAirUnit,CompNum=UnitNum, &
                             FirstHVACIteration=FirstHVACIteration,QZnReq=QCompReq, &
                             ActuatedNode=ControlNode,MaxFlow=MaxWaterFlow, &
                             MinFlow=MinWaterFlow,ControlOffSet=0.001d0,Action=1,  &
                             ControlCompTypeNum=OutAirUnit(OAUnitNum)%ControlCompTypeNum,&
                             CompErrIndex=OutAirUnit(OAUnitNum)%CompErrIndex,EquipIndex=SimCompNum,&
                             LoopNum     = OutAirUnit(OAUnitNum)%OAEquip(EquipNum)%LoopNum,&
                             LoopSide    = OutAirUnit(OAUnitNum)%OAEquip(EquipNum)%LoopSideNum,&
                             BranchIndex = OutAirUnit(OAUnitNum)%OAEquip(EquipNum)%BranchNum)

      END IF

    CASE(WaterCoil_DetailedCool) ! 'Coil:Cooling:Water:DetailedGeometry'
      IF (Sim) Then
        ControlNode   = OutAirUnit(OAUnitNum)%OAEquip(EquipNum)%CoilWaterInletNode
        MaxWaterFlow  = OutAirUnit(OAUnitNum)%OAEquip(EquipNum)%MaxWaterMassFlow
        MinWaterFlow  = OutAirUnit(OAUnitNum)%OAEquip(EquipNum)%MinWaterMassFlow
  !On the first HVAC iteration the system values are given to the controller, but after that
  ! the demand limits are in place and there needs to be feedback to the Zone Equipment
        IF((.not. FirstHVACIteration) .and. (ControlNode > 0)) Then
          MaxWaterFlow = Node(ControlNode)%MassFlowRateMaxAvail
          MinWaterFlow = Node(ControlNode)%MassFlowRateMinAvail
        END IF
        WCCoilInletNode=OutAirUnit(OAUnitNum)%OAEquip(EquipNum)%CoilAirInletNode
        WCCoilOutletNode=OutAirUnit(OAUnitNum)%OAEquip(EquipNum)%CoilAirOutletNode

        CpAirZn        = PsyCpAirFnWTdb(Node(WCCoilInletNode)%HumRat,Node(WCCoilInletNode)%Temp)
        OAMassFlow   =  OutAirUnit(OAUnitNum)%OutAirMassFlow

        IF((OPMode == NeutralMode).OR.(OPMode == HeatingMode) &
          .OR.(Node(WCCoilInletNode)%Temp<CompAirOutTemp)) THEN
          QCompReq=0.0d0
        ELSE

          QCompReq=CpAirZn*OAMassFlow*((CompAirOutTemp-Node(WCCoilInletNode)%Temp)-faneffect)
          IF (ABS(QCompReq) < SmallLoad) QCompReq = 0.d0
          IF (QCompReq > 0.d0) QCompReq = 0.d0 ! coil can cool only
        END IF

        CALL ControlCompOutput(CompName=OutAirUnit(OAUnitNum)%Name, CompType='ZONEHVAC:OUTDOORAIRUNIT',CompNum=UnitNum, &
                             FirstHVACIteration=FirstHVACIteration,QZnReq=QCompReq, &
                             ActuatedNode=ControlNode,MaxFlow=MaxWaterFlow, &
                             MinFlow=MinWaterFlow,ControlOffSet=0.001d0,Action=1,  &
                             ControlCompTypeNum=OutAirUnit(OAUnitNum)%ControlCompTypeNum,&
                             CompErrIndex=OutAirUnit(OAUnitNum)%CompErrIndex,EquipIndex=SimCompNum,&
                             LoopNum     = OutAirUnit(OAUnitNum)%OAEquip(EquipNum)%LoopNum,&
                             LoopSide    = OutAirUnit(OAUnitNum)%OAEquip(EquipNum)%LoopSideNum,&
                             BranchIndex = OutAirUnit(OAUnitNum)%OAEquip(EquipNum)%BranchNum)
      END IF

    CASE(WaterCoil_CoolingHXAsst)  ! 'CoilSystem:Cooling:Water:HeatExchangerAssisted'
      IF (Sim) Then
        ControlNode   = OutAirUnit(OAUnitNum)%OAEquip(EquipNum)%CoilWaterInletNode
        MaxWaterFlow  = OutAirUnit(OAUnitNum)%OAEquip(EquipNum)%MaxWaterMassFlow
        MinWaterFlow  = 0.0d0
  !On the first HVAC iteration the system values are given to the controller, but after that
  ! the demand limits are in place and there needs to be feedback to the Zone Equipment
        IF((.not. FirstHVACIteration) .and. (ControlNode > 0)) Then
          MaxWaterFlow = Node(ControlNode)%MassFlowRateMaxAvail
          MinWaterFlow = Node(ControlNode)%MassFlowRateMinAvail
        END IF
        WCCoilInletNode=OutAirUnit(OAUnitNum)%OAEquip(EquipNum)%CoilAirInletNode
        WCCoilOutletNode=OutAirUnit(OAUnitNum)%OAEquip(EquipNum)%CoilAirOutletNode
        CpAirZn        = PsyCpAirFnWTdb(Node(WCCoilInletNode)%HumRat,Node(WCCoilInletNode)%Temp)
        OAMassFlow   =  OutAirUnit(OAUnitNum)%OutAirMassFlow
        IF((OPMode == NeutralMode).OR.(OPMode == HeatingMode) &
          .OR.(Node(WCCoilInletNode)%Temp<CompAirOutTemp)) THEN
          QCompReq=0.0d0
        ELSE
          QCompReq=CpAirZn*OAMassFlow*((CompAirOutTemp-Node(WCCoilInletNode)%Temp)-faneffect)
          IF (ABS(QCompReq) < SmallLoad) QCompReq = 0.d0
          IF (QCompReq > 0.d0) QCompReq = 0.d0 ! coil can cool only
        END IF
        CALL ControlCompOutput(CompName=OutAirUnit(OAUnitNum)%Name, CompType='ZONEHVAC:OUTDOORAIRUNIT',CompNum=UnitNum, &
                             FirstHVACIteration=FirstHVACIteration,QZnReq=QCompReq, &
                             ActuatedNode=ControlNode,MaxFlow=MaxWaterFlow, &
                             MinFlow=MinWaterFlow,ControlOffSet=0.001d0,Action=1,  &
                             ControlCompTypeNum=OutAirUnit(OAUnitNum)%ControlCompTypeNum,&
                             CompErrIndex=OutAirUnit(OAUnitNum)%CompErrIndex,EquipIndex=SimCompNum, &
                             LoopNum     = OutAirUnit(OAUnitNum)%OAEquip(EquipNum)%LoopNum,&
                             LoopSide    = OutAirUnit(OAUnitNum)%OAEquip(EquipNum)%LoopSideNum,&
                             BranchIndex = OutAirUnit(OAUnitNum)%OAEquip(EquipNum)%BranchNum)
      END IF

    CASE(DXSystem)  ! CoilSystem:Cooling:DX  old 'AirLoopHVAC:UnitaryCoolOnly'
      IF (Sim) Then
        IF (((OPMode == NeutralMode).AND.(OutAirUnit(OAUnitNum)%ControlType == Temperature)).OR.(OPMode == HeatingMode)) THEN
          Dxsystemouttemp = 100.0d0 ! There is no cooling demand for the DX system.
        ELSE
          Dxsystemouttemp = CompAirOutTemp-faneffect
        END IF
        CALL SimDXCoolingSystem(EquipName,FirstHVACIteration,-1,DXSystemIndex,UnitNum,Dxsystemouttemp)
      END IF

    CASE(DXHeatPumpSystem)
      IF (Sim) Then
        IF (((OPMode == NeutralMode).AND.(OutAirUnit(OAUnitNum)%ControlType == Temperature)).OR.(OPMode == CoolingMode)) THEN
          Dxsystemouttemp = -20.0d0 ! There is no heating demand for the DX system.
        ELSE
          Dxsystemouttemp = CompAirOutTemp-faneffect
        END IF
        CALL SimDXHeatPumpSystem( EquipName,FirstHVACIteration,-1,DXSystemIndex,UnitNum,Dxsystemouttemp)
      ENDIF

    CASE(UnitarySystem)  ! 'AirLoopHVAC:UnitarySystem'
      IF (Sim) Then
! This may have to be done in the unitary system object since there can be both cooling and heating
        IF (((OPMode == NeutralMode).AND.(OutAirUnit(OAUnitNum)%ControlType == Temperature)).AND.(OPMode == HeatingMode)) THEN
          Dxsystemouttemp = 100 ! There is no cooling demand.
        ELSE IF (((OPMode == NeutralMode).AND.(OutAirUnit(OAUnitNum)%ControlType == Temperature)).AND.(OPMode == CoolingMode)) THEN
          Dxsystemouttemp = -20 ! There is no heating demand.
        ELSE
          Dxsystemouttemp = CompAirOutTemp-faneffect
        END IF
        CALL SimUnitarySystem(EquipName,FirstHVACIteration,-1,DXSystemIndex,HeatActive,CoolActive,UnitNum,Dxsystemouttemp)
      END IF

    CASE DEFAULT
      CALL ShowFatalError('Invalid Outdoor Air Unit Component='//TRIM(EquipType)) ! validate
  END SELECT

RETURN

END SUBROUTINE SimOutdoorAirEquipComps

SUBROUTINE CalcOAUnitCoilComps(CompNum,FirstHVACIteration,EquipIndex,LoadMet)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Young Tae Chae, Rick Strand
          !       DATE WRITTEN   June 2009
          !       MODIFIED
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine mainly controls the action of water components in the unit

          ! METHODOLOGY EMPLOYED:


          ! REFERENCES:

          ! USE STATEMENTS:

  USE HeatingCoils, ONLY : SimulateHeatingCoilComponents
  USE WaterCoils,   ONLY : SimulateWaterCoilComponents
  USE HVACHXAssistedCoolingCoil, ONLY :SimHXAssistedCoolingCoil
  USE SteamCoils,   ONLY: SimulateSteamCoilComponents
  USE DataHVACGlobals, ONLY: SmallLoad

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)           :: CompNum ! actual outdoor air unit num
  LOGICAL, INTENT (IN)          :: FirstHVACIteration
  INTEGER, INTENT(IN)           :: EquipIndex ! Component Type -- Integerized for this module
  REAL(r64), INTENT(OUT)        :: LoadMet

           ! SUBROUTINE LOCAL VARIABLE DEFINITIONS
  INTEGER   :: OAUnitNum
  REAL(r64) :: CpAirZn
  INTEGER   :: CoilIndex
  INTEGER   :: OPMode
  REAL(r64) :: AirMassFlow
  REAL(r64) :: Faneffect
  LOGICAL   :: DrawFan             ! Fan Flag
  INTEGER   :: InletNode
  INTEGER   :: OutletNode
  INTEGER   :: AirOutletNode
  INTEGER   :: WaterCoilIndex =0
  REAL(r64) :: QCompReq            ! Actual equipment load
  REAL(r64) :: CoilInTemp
  REAL(r64) :: MinWaterFlow
  INTEGER   :: SHCoilInletNode
  INTEGER   :: SHCoilOutletNode
  INTEGER   :: CoilWaterInletNode
  INTEGER   :: CoilTypeNum
  LOGICAL   :: ErrorsFound=.FALSE. ! Set to true if errors in input, fatal at end of routine
  REAL(r64) ::CoilAirOutTemp
  INTEGER   :: CoilNum
  INTEGER   :: CompoNum

 ! Flow
  CoilIndex=0
  OAUnitNum=CompNum
  CompoNum=EquipIndex
  CoilTypeNum=OutAirUnit(OAUnitNum)%OAEquip(CompoNum)%ComponentType_Num
  OPMode=OutAirUnit(OAUnitNum)%OperatingMode
  CoilAirOutTemp=OutAirUnit(OAUnitNum)%CompOutSetTemp
  DrawFan=OutAirUnit(OAUnitNum)%FanEffect
  IF (DrawFan) THEN
    Faneffect     = OutAirUnit(OAUnitNum)%FanCorTemp
  ELSE
    Faneffect     = 0.0d0
  END IF

  SELECT CASE(CoilTypeNum)
    CASE (Coil_ElectricHeat)
        InletNode=OutAirUnit(OAUnitNum)%OAEquip(CompoNum)%CoilAirInletNode
        OutletNode=OutAirUnit(OAUnitNum)%OAEquip(CompoNum)%CoilAirOutletNode
        IF((OPMode == NeutralMode).OR.(OPMode == CoolingMode) &
           .OR.(Node(InletNode)%Temp > CoilAirOutTemp)) THEN
          QCompReq=0.0d0
        ELSE
          CpAirZn        = PsyCpAirFnWTdb(Node(InletNode)%HumRat,Node(InletNode)%Temp)
          QCompReq       = Node(InletNode)%MassFlowRate * CpAirZn &
                           *((CoilAirOutTemp-Node(InletNode)%Temp)-faneffect)
          IF (ABS(QCompReq) < SmallLoad) QCompReq = 0.d0
        END IF

        IF (QCompReq .LE. 0.0d0) THEN
            QCompReq = 0.0d0    ! a heating coil can only heat, not cool
          Node(OutletNode)%Temp=Node(InletNode)%Temp
          Node(OutletNode)%HumRat=Node(InletNode)%HumRat
          Node(OutletNode)%Massflowrate=Node(InletNode)%Massflowrate

        END IF
          CALL SimulateHeatingCoilComponents(OutAirUnit(OAUnitNum)%OAEquip(CompoNum)%ComponentName, &
                                              FirstHVACIteration,QCompReq,CoilIndex)


      AirMassFlow = Node(InletNode)%MassFlowRate
      LoadMet = AirMassFlow * (PsyHFnTdbW(Node(OutletNode)%Temp,Node(InletNode)%HumRat)  &
                              - PsyHFnTdbW(Node(InletNode)%Temp,Node(InletNode)%HumRat))

    CASE(Coil_GasHeat)    ! 'Coil:Heating:Steam'
        InletNode=OutAirUnit(OAUnitNum)%OAEquip(CompoNum)%CoilAirInletNode
        OutletNode=OutAirUnit(OAUnitNum)%OAEquip(CompoNum)%CoilAirOutletNode
        IF((OPMode == NeutralMode).OR.(OPMode == CoolingMode) &
            .OR.(Node(InletNode)%Temp > CoilAirOutTemp)) THEN
          QCompReq=0.0d0
        ELSE
          Node(OutletNode)%Massflowrate=Node(InletNode)%Massflowrate
          CpAirZn        = PsyCpAirFnWTdb(Node(InletNode)%HumRat,Node(InletNode)%Temp)
          QCompReq       = Node(InletNode)%MassFlowRate * CpAirZn &
                           *((CoilAirOutTemp-Node(InletNode)%Temp)-faneffect)
          IF (ABS(QCompReq) < SmallLoad) QCompReq = 0.d0
        END IF
        IF (QCompReq .LE. 0.0d0) THEN
          QCompReq = 0.0d0    ! a heating coil can only heat, not cool
          Node(OutletNode)%Temp=Node(InletNode)%Temp
          Node(OutletNode)%HumRat=Node(InletNode)%HumRat
          Node(OutletNode)%Massflowrate=Node(InletNode)%Massflowrate
        END IF
        CALL SimulateHeatingCoilComponents(OutAirUnit(OAUnitNum)%OAEquip(CompoNum)%ComponentName, &
                                           FirstHVACIteration,QCompReq,CoilIndex)

      AirMassFlow = Node(InletNode)%MassFlowRate
      LoadMet = AirMassFlow * (PsyHFnTdbW(Node(OutletNode)%Temp,Node(InletNode)%HumRat)  &
                              - PsyHFnTdbW(Node(InletNode)%Temp,Node(InletNode)%HumRat))

    CASE(SteamCoil_AirHeat)    ! 'Coil:Heating:Steam'
        InletNode=OutAirUnit(OAUnitNum)%OAEquip(CompoNum)%CoilAirInletNode
        OutletNode=OutAirUnit(OAUnitNum)%OAEquip(CompoNum)%CoilAirOutletNode
      IF((OPMode == NeutralMode).OR.(OPMode == CoolingMode) &
          .OR.(Node(InletNode)%Temp > CoilAirOutTemp)) THEN
        QCompReq=0.0d0
      ELSE
        CpAirZn        = PsyCpAirFnWTdb(Node(InletNode)%HumRat,Node(InletNode)%Temp)
        QCompReq       = Node(InletNode)%MassFlowRate * CpAirZn &
                                   *((CoilAirOutTemp-Node(InletNode)%Temp)-faneffect)
        IF (ABS(QCompReq) < SmallLoad) QCompReq = 0.d0
      END IF
      IF (QCompReq .LE. 0.0d0) THEN
            QCompReq = 0.0d0    ! a heating coil can only heat, not cool
        Node(OutletNode)%Temp=Node(InletNode)%Temp
        Node(OutletNode)%HumRat=Node(InletNode)%HumRat
        Node(OutletNode)%Massflowrate=Node(InletNode)%Massflowrate
      END IF
      CALL SimulateSteamCoilComponents(OutAirUnit(OAUnitNum)%OAEquip(CompoNum)%ComponentName, &
                                       FirstHVACIteration,QCompReq,CoilIndex)
        AirMassFlow = Node(InletNode)%MassFlowRate
        LoadMet     = AirMassFlow * (PsyHFnTdbW(Node(OutletNode)%Temp,Node(InletNode)%HumRat)  &
                                   - PsyHFnTdbW(Node(InletNode)%Temp,Node(InletNode)%HumRat))

    CASE(WaterCoil_SimpleHeat) ! 'Coil:Heating:Water')
        CALL SimulateWaterCoilComponents(OutAirUnit(OAUnitNum)%OAEquip(CompoNum)%ComponentName,FirstHVACIteration,CoilIndex)
        InletNode   = OutAirUnit(OAUnitNum)%OAEquip(CompoNum)%CoilAirInletNode
        OutletNode  = OutAirUnit(OAUnitNum)%OAEquip(CompoNum)%CoilAirOutletNode
        AirMassFlow = Node(InletNode)%MassFlowRate
        LoadMet    = AirMassFlow * (PsyHFnTdbW(Node(OutletNode)%Temp,Node(InletNode)%HumRat)  &
                                - PsyHFnTdbW(Node(InletNode)%Temp,Node(InletNode)%HumRat))


    CASE(WaterCoil_Cooling)       ! 'Coil:Cooling:Water'
        CALL SimulateWaterCoilComponents(OutAirUnit(OAUnitNum)%OAEquip(CompoNum)%ComponentName,FirstHVACIteration,CoilIndex)
        InletNode=OutAirUnit(OAUnitNum)%OAEquip(CompoNum)%CoilAirInletNode
        OutletNode=OutAirUnit(OAUnitNum)%OAEquip(CompoNum)%CoilAirOutletNode
        AirMassFlow = Node(InletNode)%MassFlowRate
        LoadMet     = AirMassFlow * (PsyHFnTdbW(Node(OutletNode)%Temp,Node(InletNode)%HumRat)  &
                                 - PsyHFnTdbW(Node(InletNode)%Temp,Node(InletNode)%HumRat))

    CASE(WaterCoil_DetailedCool)
        CALL SimulateWaterCoilComponents(OutAirUnit(OAUnitNum)%OAEquip(CompoNum)%ComponentName,FirstHVACIteration,CoilIndex)
        InletNode=OutAirUnit(OAUnitNum)%OAEquip(CompoNum)%CoilAirInletNode
        OutletNode=OutAirUnit(OAUnitNum)%OAEquip(CompoNum)%CoilAirOutletNode
        AirMassFlow = Node(InletNode)%MassFlowRate
        LoadMet     = AirMassFlow * (PsyHFnTdbW(Node(OutletNode)%Temp,Node(InletNode)%HumRat)  &
                                 - PsyHFnTdbW(Node(InletNode)%Temp,Node(InletNode)%HumRat))

    CASE(WaterCoil_CoolingHXAsst)
        CALL SimHXAssistedCoolingCoil(OutAirUnit(OAUnitNum)%OAEquip(CompoNum)%ComponentName,FirstHVACIteration,1,  &
                 0.0d0,CoilIndex,ContFanCycCoil)
        InletNode=OutAirUnit(OAUnitNum)%OAEquip(CompoNum)%CoilAirInletNode
        OutletNode=OutAirUnit(OAUnitNum)%OAEquip(CompoNum)%CoilAirOutletNode
        AirMassFlow = Node(InletNode)%MassFlowRate
        LoadMet     = AirMassFlow * (PsyHFnTdbW(Node(OutletNode)%Temp,Node(InletNode)%HumRat)  &
                                 - PsyHFnTdbW(Node(InletNode)%Temp,Node(InletNode)%HumRat))

  END SELECT

END SUBROUTINE CalcOAUnitCoilComps

!SUBROUTINE UpdateOutdoorAirUnit
!
! No update routine needed in this module since all of the updates happen on
! the Node derived type directly and these updates are done by other routines.
!
!END SUBROUTINE UpdateOutdoorAirUnit


SUBROUTINE ReportOutdoorAirUnit(OAUnitNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Young T. Chae
          !       DATE WRITTEN   Oct. 2009
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine simply produces output for the outdoor air unit.
          ! METHODOLOGY EMPLOYED:
          ! Standard EnergyPlus methodology.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataGlobals,     ONLY : SecInHour
  USE DataHeatBalance, ONLY : Zone
  USE DataHVACGlobals, ONLY : TimeStepSys
  USE DataLoopNode,    ONLY : Node

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: OAUnitNum  ! Index for the outdoor air unit under consideration within the derived types

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

          ! FLOW:

  OutAirUnit(OAUnitNum)%TotHeatingEnergy    = OutAirUnit(OAUnitNum)%TotHeatingRate*TimeStepSys*SecInHour
  OutAirUnit(OAUnitNum)%SensHeatingEnergy   = OutAirUnit(OAUnitNum)%SensHeatingRate*TimeStepSys*SecInHour
  OutAirUnit(OAUnitNum)%LatHeatingEnergy    = OutAirUnit(OAUnitNum)%LatHeatingRate*TimeStepSys*SecInHour
  OutAirUnit(OAUnitNum)%SensCoolingEnergy   = OutAirUnit(OAUnitNum)%SensCoolingRate*TimeStepSys*SecInHour
  OutAirUnit(OAUnitNum)%LatCoolingEnergy    = OutAirUnit(OAUnitNum)%LatCoolingRate*TimeStepSys*SecInHour
  OutAirUnit(OAUnitNum)%TotCoolingEnergy    = OutAirUnit(OAUnitNum)%TotCoolingRate*TimeStepSys*SecInHour
  OutAirUnit(OAUnitNum)%AirMassFlow         = OutAirUnit(OAUnitNum)%OutAirMassFlow
  OutAirUnit(OAUnitNum)%ElecFanEnergy       = OutAirUnit(OAUnitNum)%ElecFanRate*TimeStepSys*SecInHour

  RETURN
END SUBROUTINE ReportOutdoorAirUnit

INTEGER FUNCTION GetOutdoorAirUnitOutAirNode(OAUnitNum)

          ! FUNCTION INFORMATION:
          !       AUTHOR         B Griffith
          !       DATE WRITTEN   Dec  2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! lookup function for OA inlet node

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN)  :: OAUnitNum          !

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
          ! na
  IF (GetOutdoorAirUnitInputFlag) THEN
    CALL GetOutdoorAirUnitInputs
    GetOutdoorAirUnitInputFlag=.FALSE.
  ENDIF

  GetOutdoorAirUnitOutAirNode = 0
  If (OAUnitNum > 0 .and. OAUnitNum <= NumOfOAUnits) THEN
    GetOutdoorAirUnitOutAirNode = OutAirUnit(OAUnitNum)%OutsideAirNode
  ENDIF

  RETURN

END FUNCTION GetOutdoorAirUnitOutAirNode

INTEGER FUNCTION GetOutdoorAirUnitZoneInletNode(OAUnitNum)

          ! FUNCTION INFORMATION:
          !       AUTHOR         B Griffith
          !       DATE WRITTEN   Dec  2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! lookup function for OA inlet node

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN)  :: OAUnitNum          !

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
          ! na
  IF (GetOutdoorAirUnitInputFlag) THEN
    CALL GetOutdoorAirUnitInputs
    GetOutdoorAirUnitInputFlag=.FALSE.
  ENDIF

  GetOutdoorAirUnitZoneInletNode = 0
  If (OAUnitNum > 0 .and. OAUnitNum <= NumOfOAUnits) THEN
    GetOutdoorAirUnitZoneInletNode = OutAirUnit(OAUnitNum)%AirOutletNode
  ENDIF

  RETURN

END FUNCTION GetOutdoorAirUnitZoneInletNode



INTEGER FUNCTION GetOutdoorAirUnitReturnAirNode(OAUnitNum)

          ! FUNCTION INFORMATION:
          !       AUTHOR         B Griffith
          !       DATE WRITTEN   Dec  2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! lookup function for OA inlet node

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN)  :: OAUnitNum          !

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
          ! na
  IF (GetOutdoorAirUnitInputFlag) THEN
    CALL GetOutdoorAirUnitInputs
    GetOutdoorAirUnitInputFlag=.FALSE.
  ENDIF

  GetOutdoorAirUnitReturnAirNode = 0
  If (OAUnitNum > 0 .and. OAUnitNum <= NumOfOAUnits) THEN
    GetOutdoorAirUnitReturnAirNode = OutAirUnit(OAUnitNum)%AirInletNode
  ENDIF

  RETURN

END FUNCTION GetOutdoorAirUnitReturnAirNode


!*****************************************************************************************

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

END MODULE OutdoorAirUnit
