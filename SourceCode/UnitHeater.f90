MODULE UnitHeater

  ! Module containing the routines dealing with the Unit Heater

  ! MODULE INFORMATION:
  !       AUTHOR         Rick Strand
  !       DATE WRITTEN   May 2000
  !       MODIFIED       Brent Griffith, Sept 2010, plant upgrades, fluid properties
  !       RE-ENGINEERED  na

  ! PURPOSE OF THIS MODULE:
  ! To simulate unit heaters.  It is assumed that unit heaters are zone equipment
  ! without any connection to outside air other than through a separately defined
  ! air loop.

  ! METHODOLOGY EMPLOYED:
  ! Units are modeled as a collection of a fan and a heating coil.  The fan
  ! can either be a continuously running fan or an on-off fan which turns on
  ! only when there is actually a heating load.  This fan control works together
  ! with the unit operation schedule to determine what the unit heater actually
  ! does at a given point in time.

  ! REFERENCES:
  ! ASHRAE Systems and Equipment Handbook (SI), 1996. pp. 31.3-31.8
  ! Rick Strand's unit heater module which was based upon Fred Buhl's fan coil
  ! module (FanCoilUnits.f90)

  ! OTHER NOTES: none

  ! USE STATEMENTS:
  ! Use statements for data only modules
USE DataPrecisionGlobals
USE DataLoopNode
USE DataGlobals,     ONLY: BeginEnvrnFlag, BeginDayFlag, MaxNameLength, InitConvTemp, SysSizingCalc, ScheduleAlwaysOn, &
                           DisplayExtraWarnings
USE DataInterfaces
USE DataHVACGlobals, ONLY: SmallMassFlow, SmallLoad, FanElecPower, SmallAirVolFlow, cFanTypes

  ! Use statements for access to subroutines in other modules
USE ScheduleManager
USE Psychrometrics, ONLY:PsyRhoAirFnPbTdbW,PsyHFnTdbW,PsyCpAirFnWTdb
Use FluidProperties

IMPLICIT NONE         ! Enforce explicit typing of all variables

PRIVATE ! Everything private unless explicitly made public

  ! MODULE PARAMETER DEFINITIONS
  CHARACTER(len=*), PARAMETER :: cMO_UnitHeater = 'ZoneHVAC:UnitHeater'

  ! Character parameters for outside air control types:
  CHARACTER(len=*), PARAMETER :: ElectricCoil   = "ElectricCoil"
  CHARACTER(len=*), PARAMETER :: GasCoil        = "GasCoil"
  CHARACTER(len=*), PARAMETER :: WaterCoil      = "WaterCoil"
  CHARACTER(len=*), PARAMETER :: SteamCoil      = "SteamCoil"
  CHARACTER(len=*), PARAMETER :: ContinuousCtrl = "CONTINUOUS"
  CHARACTER(len=*), PARAMETER :: OnOffCtrl      = "ONOFF"

  ! DERIVED TYPE DEFINITIONS
TYPE UnitHeaterData
  ! Input data
  CHARACTER(len=MaxNameLength) :: Name               =' ' ! name of unit
  CHARACTER(len=MaxNameLength) :: SchedName          =' ' ! availability schedule
  INTEGER                      :: SchedPtr           =0   ! index to schedule
  INTEGER                      :: AirInNode          =0   ! inlet air node number
  INTEGER                      :: AirOutNode         =0   ! outlet air node number
  INTEGER                      :: FanType_Num        =0   ! Fan type number (see DataHVACGlobals)
  CHARACTER(len=MaxNameLength) :: FanType            =' ' ! type of fan
  CHARACTER(len=MaxNameLength) :: FanName            =' ' ! name of fan
  INTEGER                      :: Fan_Index          =0   !
  INTEGER                      :: FanAvailSchedPtr   =0   ! index to fan availability schedule
  INTEGER                      :: ControlCompTypeNum =0
  INTEGER                      :: CompErrIndex       =0
  REAL(r64)                    :: MaxAirVolFlow      =0.0d0 ! m3/s
  REAL(r64)                    :: MaxAirMassFlow     =0.0d0 ! kg/s
  CHARACTER(len=MaxNameLength) :: FanControlType     =' ' ! type of control; options are VARIABLE PERCENT and FIXED TEMPERATURE
  INTEGER                      :: FanOutletNode      =0   ! outlet node number for fan exit
                                                          ! (assumes fan is upstream of heating coil)
  CHARACTER(len=MaxNameLength) :: HCoilType          =' ' ! type of heating coil (water, gas, electric, etc.)
  CHARACTER(len=MaxNameLength) :: HCoilTypeCh        =' ' ! actual object name
  CHARACTER(len=MaxNameLength) :: HCoilName          =' ' ! name of heating coil
  INTEGER                      :: HCoil_Index        =0   !
  INTEGER                      :: HCoil_PlantTypeNum =0  !
  INTEGER                      :: HCoil_FluidIndex   =0
  REAL(r64)                    :: MaxVolHotWaterFlow =0.0d0 ! m3/s
  REAL(r64)                    :: MaxVolHotSteamFlow =0.0d0 ! m3/s
  REAL(r64)                    :: MaxHotWaterFlow    =0.0d0 ! kg/s
  REAL(r64)                    :: MaxHotSteamFlow    =0.0d0 ! m3/s
  REAL(r64)                    :: MinVolHotWaterFlow =0.0d0 ! m3/s
  REAL(r64)                    :: MinVolHotSteamFlow =0.0d0 ! m3/s
  REAL(r64)                    :: MinHotWaterFlow    =0.0d0 ! kg/s
  REAL(r64)                    :: MinHotSteamFlow    =0.0d0 ! kg/s
  INTEGER                      :: HotControlNode     =0   ! hot water control node, inlet of coil
  REAL(r64)                    :: HotControlOffset   =0.0d0 ! control tolerance
  INTEGER                      :: HotCoilOutNodeNum  =0   ! outlet of coil
  INTEGER                      :: HWLoopNum          =0   ! index for plant loop with hot plant coil
  INTEGER                      :: HWLoopSide         =0   ! index for plant loop side for hot plant coil
  INTEGER                      :: HWBranchNum        =0   ! index for plant branch for hot plant coil
  INTEGER                      :: HWCompNum          =0   ! index for plant component for hot plant coil

  ! Report data
  REAL(r64)                    :: HeatPower          =0.0d0 ! unit heating output in watts
  REAL(r64)                    :: HeatEnergy         =0.0d0 ! unit heating output in J
  REAL(r64)                    :: ElecPower          =0.0d0 !
  REAL(r64)                    :: ElecEnergy         =0.0d0 !
  CHARACTER(len=MaxNameLength) :: AvailManagerListName = ' ' ! Name of an availability manager list object
  INTEGER                      :: AvailStatus          = 0
  LOGICAL                      :: FanControlTypeOnOff = .FALSE.   ! True when FanControlType is OnOffCtrl
END TYPE UnitHeaterData

TYPE (UnitHeaterData), ALLOCATABLE, DIMENSION(:) :: UnitHeat

  ! MODULE VARIABLE DECLARATIONS:
LOGICAL :: HCoilOn          ! TRUE if the heating coil (gas or electric especially) should be running
INTEGER :: NumOfUnitHeats   ! Number of unit heaters in the input file
REAL(r64)    :: QZnReq           ! heating or cooling needed by zone [watts]
LOGICAL, ALLOCATABLE, DIMENSION(:) :: MySizeFlag
LOGICAL, ALLOCATABLE, DIMENSION(:) :: CheckEquipName

  ! SUBROUTINE SPECIFICATIONS FOR MODULE UnitHeater
PUBLIC  SimUnitHeater
PRIVATE GetUnitHeaterInput
PRIVATE InitUnitHeater
PRIVATE SizeUnitHeater
PRIVATE CalcUnitHeater
PUBLIC  CalcUnitHeaterComponents
!PRIVATE UpdateUnitHeater
PRIVATE ReportUnitHeater

CONTAINS

SUBROUTINE SimUnitHeater(CompName,ZoneNum,FirstHVACIteration,PowerMet,LatOutputProvided,CompIndex)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Rick Strand
          !       DATE WRITTEN   May 2000
          !       MODIFIED       Don Shirey, Aug 2009 (LatOutputProvided)
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This is the main driver subroutine for the Unit Heater simulation.

          ! METHODOLOGY EMPLOYED:
          ! Standard EnergyPlus methodology.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor,      ONLY: FindItemInList
  USE DataSizing,          ONLY: ZoneHeatingOnlyFan
  USE General,             ONLY: TrimSigDigits

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN)  :: CompName            ! name of the fan coil unit
  INTEGER,          INTENT(IN)  :: ZoneNum             ! number of zone being served
  LOGICAL,          INTENT(IN)  :: FirstHVACIteration  ! TRUE if 1st HVAC simulation of system timestep
  REAL(r64),        INTENT(OUT) :: PowerMet            ! Sensible power supplied (W)
  REAL(r64),        INTENT (OUT) :: LatOutputProvided  ! Latent add/removal supplied by window AC (kg/s), dehumid = negative
  INTEGER,          INTENT(INOUT) :: CompIndex

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER      :: UnitHeatNum            ! index of unit heater being simulated
  LOGICAL,SAVE :: GetInputFlag = .true.  ! First time, input is "gotten"

          ! FLOW:
  IF (GetInputFlag) THEN
    CALL GetUnitHeaterInput
    GetInputFlag=.false.
  ENDIF

  ! Find the correct Unit Heater Equipment
  IF (CompIndex == 0) THEN
    UnitHeatNum = FindItemInList(CompName,UnitHeat%Name,NumOfUnitHeats)
    IF (UnitHeatNum == 0) THEN
      CALL ShowFatalError('SimUnitHeater: Unit not found='//TRIM(CompName))
    ENDIF
    CompIndex=UnitHeatNum
  ELSE
    UnitHeatNum=CompIndex
    IF (UnitHeatNum > NumOfUnitHeats .or. UnitHeatNum < 1) THEN
      CALL ShowFatalError('SimUnitHeater:  Invalid CompIndex passed='//  &
                          TRIM(TrimSigDigits(UnitHeatNum))// &
                          ', Number of Units='//TRIM(TrimSigDigits(NumOfUnitHeats))//  &
                          ', Entered Unit name='//TRIM(CompName))
    ENDIF
    IF (CheckEquipName(UnitHeatNum)) THEN
      IF (CompName /= UnitHeat(UnitHeatNum)%Name) THEN
        CALL ShowFatalError('SimUnitHeater: Invalid CompIndex passed='//  &
                            TRIM(TrimSigDigits(UnitHeatNum))// &
                            ', Unit name='//TRIM(CompName)//', stored Unit Name for that index='//  &
                            TRIM(UnitHeat(UnitHeatNum)%Name))
      ENDIF
      CheckEquipName(UnitHeatNum)=.false.
    ENDIF
  ENDIF

  CALL InitUnitHeater(UnitHeatNum, ZoneNum)

  ZoneHeatingOnlyFan = .TRUE.

  CALL CalcUnitHeater(UnitHeatNum,ZoneNum,FirstHVACIteration,PowerMet,LatOutputProvided)

  ZoneHeatingOnlyFan = .FALSE.

!  CALL UpdateUnitHeater

  CALL ReportUnitHeater(UnitHeatNum)

  RETURN

END SUBROUTINE SimUnitHeater

SUBROUTINE GetUnitHeaterInput

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Rick Strand
          !       DATE WRITTEN   May 2000
          !       MODIFIED       Chandan Sharma, FSEC, March 2011: Added ZoneHVAC sys avail manager
          !                      Bereket Nigusse, FSEC, April 2011: eliminated input node names
          !                                                         & added fan object type
          !
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Obtain the user input data for all of the unit heaters in the input file.

          ! METHODOLOGY EMPLOYED:
          ! Standard EnergyPlus methodology.

          ! REFERENCES:
          ! Fred Buhl's fan coil module (FanCoilUnits.f90)

          ! USE STATEMENTS:
  USE InputProcessor,   ONLY : GetNumObjectsFound, GetObjectItem, VerifyName, SameString,GetObjectDefMaxArgs
  USE NodeInputManager, ONLY : GetOnlySingleNode
  USE BranchNodeConnections, ONLY: SetUpCompSets
!  USE DataIPShortCuts
  USE Fans,         ONLY: GetFanType, GetFanOutletNode, GetFanIndex, GetFanVolFlow, GetFanAvailSchPtr
  USE WaterCoils,   ONLY: GetCoilWaterInletNode
  USE SteamCoils,   ONLY: GetSteamCoilSteamInletNode=>GetCoilSteamInletNode, GetSteamCoilIndex
  USE DataZoneEquipment,         ONLY: UnitHeater_Num, ZoneEquipConfig
  USE DataSizing,                ONLY: AutoSize
  USE General,                   ONLY: TrimSigDigits
  USE DataHVACGlobals,           ONLY: FanType_SimpleConstVolume, FanType_SimpleVAV, ZoneComp
  USE DataGlobals,               ONLY: NumOfZones
  USE DataPlant,                 ONLY : TypeOf_CoilWaterSimpleHeating, TypeOf_CoilSteamAirHeating

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  LOGICAL                        :: ErrorsFound=.false. ! Set to true if errors in input, fatal at end of routine
  INTEGER                        :: IOStatus            ! Used in GetObjectItem
  LOGICAL                        :: IsBlank             ! TRUE if the name is blank
  LOGICAL                        :: IsNotOk             ! TRUE if there was a problem with a list name
  LOGICAL                        :: ErrFlag=.FALSE.     ! interim error flag
  INTEGER                        :: NumAlphas           ! Number of Alphas for each GetObjectItem call
  INTEGER                        :: NumNumbers          ! Number of Numbers for each GetObjectItem call
  INTEGER                        :: NumFields           ! Total number of fields in object
  INTEGER                        :: UnitHeatNum         ! Item to be "gotten"
  CHARACTER(len=*), PARAMETER :: RoutineName='GetUnitHeaterInput: ' ! include trailing blank space
!  LOGICAL                        :: FanErrFlag          ! Error flag used in GetFanIndex call
  REAL(r64)                      :: FanVolFlow          ! Fan volumetric flow rate
  CHARACTER(len=MaxNameLength)   :: CurrentModuleObject
  CHARACTER(len=MaxNameLength), &
                   ALLOCATABLE, DIMENSION(:)  :: Alphas     ! Alpha items for object
  REAL(r64), ALLOCATABLE, DIMENSION(:)        :: Numbers    ! Numeric items for object
  CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:)  :: cAlphaFields   ! Alpha field names
  CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:)  :: cNumericFields ! Numeric field names
  LOGICAL, ALLOCATABLE, DIMENSION(:)  :: lAlphaBlanks     ! Logical array, alpha field input BLANK = .true.
  LOGICAL, ALLOCATABLE, DIMENSION(:)  :: lNumericBlanks   ! Logical array, numeric field input BLANK = .true.
  INTEGER                             :: CtrlZone         ! index to loop counter
  INTEGER                             :: NodeNum          ! index to loop counter
  LOGICAL                             :: ZoneNodeNotFound ! used in error checking

          ! FLOW:

          ! Figure out how many unit heaters there are in the input file
  CurrentModuleObject = cMO_UnitHeater
  NumOfUnitHeats = GetNumObjectsFound(CurrentModuleObject)
  CALL GetObjectDefMaxArgs(CurrentModuleObject,NumFields,NumAlphas,NumNumbers)

  ALLOCATE(Alphas(NumAlphas))
  Alphas=' '
  ALLOCATE(Numbers(NumNumbers))
  Numbers=0.0d0
  ALLOCATE(cAlphaFields(NumAlphas))
  cAlphaFields=' '
  ALLOCATE(cNumericFields(NumNumbers))
  cNumericFields=' '
  ALLOCATE(lAlphaBlanks(NumAlphas))
  lAlphaBlanks=.TRUE.
  ALLOCATE(lNumericBlanks(NumNumbers))
  lNumericBlanks=.TRUE.

          ! Allocate the local derived type and do one-time initializations for all parts of it
  IF (NumOfUnitHeats .GT. 0) THEN
    ALLOCATE(UnitHeat(NumOfUnitHeats))
    ALLOCATE(CheckEquipName(NumOfUnitHeats))
  ENDIF
  CheckEquipName=.true.

  DO UnitHeatNum = 1, NumOfUnitHeats    ! Begin looping over all of the unit heaters found in the input file...

    CALL GetObjectItem(CurrentModuleObject,UnitHeatNum,Alphas,NumAlphas,Numbers,NumNumbers,IOStatus, &
                       NumBlank=lNumericBlanks,AlphaBlank=lAlphaBlanks, &
                       AlphaFieldNames=cAlphaFields,NumericFieldNames=cNumericFields)

    IsNotOK=.FALSE.
    IsBlank=.FALSE.
    CALL VerifyName(Alphas(1),UnitHeat%Name,UnitHeatNum-1,IsNotOK,IsBlank,TRIM(CurrentModuleObject)//' Name')
    IF (IsNotOK) THEN
      ErrorsFound=.true.
      IF (IsBlank) Alphas(1)='xxxxx'
    END IF

    UnitHeat(UnitHeatNum)%Name      = Alphas(1)
    UnitHeat(UnitHeatNum)%SchedName = Alphas(2)
    IF (lAlphaBlanks(2)) THEN
      UnitHeat(UnitHeatNum)%SchedPtr  = ScheduleAlwaysOn
    ELSE
      UnitHeat(UnitHeatNum)%SchedPtr  = GetScheduleIndex(Alphas(2))  ! convert schedule name to pointer
      IF (UnitHeat(UnitHeatNum)%SchedPtr == 0) THEN
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//': invalid '//TRIM(cAlphaFields(2))//  &
           ' entered ='//TRIM(Alphas(2))// &
           ' for '//TRIM(cAlphaFields(1))//'='//TRIM(Alphas(1)))
        ErrorsFound=.TRUE.
      END IF
    END IF

          ! Main air nodes (except outside air node):
    UnitHeat(UnitHeatNum)%AirInNode = &
               GetOnlySingleNode(Alphas(3),ErrorsFound,TRIM(CurrentModuleObject),Alphas(1), &
                            NodeType_Air,NodeConnectionType_Inlet,1,ObjectIsParent)

    UnitHeat(UnitHeatNum)%AirOutNode = &
               GetOnlySingleNode(Alphas(4),ErrorsFound,TRIM(CurrentModuleObject),Alphas(1), &
                            NodeType_Air,NodeConnectionType_Outlet,1,ObjectIsParent)

          ! Fan information:
    UnitHeat(UnitHeatNum)%FanType        = Alphas(5)
    UnitHeat(UnitHeatNum)%FanName        = Alphas(6)
    UnitHeat(UnitHeatNum)%FanControlType = Alphas(7)
    UnitHeat(UnitHeatNum)%MaxAirVolFlow  = Numbers(1)

    IF ( (.NOT.SameString(UnitHeat(UnitHeatNum)%FanControlType,OnOffCtrl)) .AND. &
         (.NOT.SameString(UnitHeat(UnitHeatNum)%FanControlType,ContinuousCtrl)) ) THEN
      ErrorsFound=.TRUE.
      CALL ShowSevereError('Illegal '//TRIM(cAlphaFields(7))//' = '//TRIM(Alphas(7)))
      CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//'='//TRIM(UnitHeat(UnitHeatNum)%Name))
    ELSEIF ( SameString(UnitHeat(UnitHeatNum)%FanControlType,OnOffCtrl)) THEN
      UnitHeat(UnitHeatNum)%FanControlTypeOnOff = .TRUE.
    END IF

    ErrFlag = .FALSE.
    CALL ValidateComponent(UnitHeat(UnitHeatNum)%FanType,UnitHeat(UnitHeatNum)%FanName,ErrFlag,TRIM(CurrentModuleObject))
    IF (ErrFlag) THEN
      CALL ShowContinueError('specified in '//TRIM(CurrentModuleObject)//' = "'//TRIM(UnitHeat(UnitHeatNum)%Name)//'".')
      ErrorsFound=.TRUE.
    ELSE
      CALL GetFanType(UnitHeat(UnitHeatNum)%FanName,UnitHeat(UnitHeatNum)%FanType_Num, &
                    ErrFlag,CurrentModuleObject,UnitHeat(UnitHeatNum)%Name)

      SELECT CASE (UnitHeat(UnitHeatNum)%FanType_Num)
        CASE (FanType_SimpleConstVolume,FanType_SimpleVAV)
          ! Get fan outlet node
           UnitHeat(UnitHeatNum)%FanOutletNode = GetFanOutletNode(UnitHeat(UnitHeatNum)%FanType,&
                                                                  UnitHeat(UnitHeatNum)%FanName,Errflag)
          IF(ErrFlag)THEN
            CALL ShowContinueError('specified in '//TRIM(CurrentModuleObject)//' = "' // TRIM(UnitHeat(UnitHeatNum)%Name)//'".')
            ErrorsFound = .TRUE.
          ENDIF
        CASE DEFAULT
          CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//' = "'//TRIM(Alphas(1))//'"')
          CALL ShowContinueError('Fan Type must be Fan:ConstantVolume or Fan:VariableVolume')
          ErrorsFound=.TRUE.
      END SELECT
      CALL GetFanIndex(UnitHeat(UnitHeatNum)%FanName,UnitHeat(UnitHeatNum)%Fan_Index,ErrFlag,TRIM(CurrentModuleObject))
      IF (ErrFlag) THEN
        ErrorsFound = .TRUE.
      ELSE
        CALL GetFanVolFlow(UnitHeat(UnitHeatNum)%Fan_Index,FanVolFlow)

        IF(FanVolFlow .NE. AutoSize .AND. UnitHeat(UnitHeatNum)%MaxAirVolFlow .NE. AutoSize .AND. &
           FanVolFlow .LT. UnitHeat(UnitHeatNum)%MaxAirVolFlow)THEN
          CALL ShowSevereError('Specified in '//TRIM(CurrentModuleObject)//' = '//TRIM(UnitHeat(UnitHeatNum)%Name))
          CALL ShowContinueError('...air flow rate ('//TRIM(TrimSigDigits(FanVolFlow,7))//') in'// &
          ' fan object '//TRIM(UnitHeat(UnitHeatNum)%FanName)//' is less than the unit heater maximum supply air'// &
          ' flow rate ('//TRIM(TrimSigDigits(UnitHeat(UnitHeatNum)%MaxAirVolFlow,7))//').')
          CALL ShowContinueError('...the fan flow rate must be greater than or equal to the unit heater maximum'// &
                                 ' supply air flow rate.')
          ErrorsFound = .TRUE.
        ELSEIF(FanVolFlow .EQ. AutoSize .AND. UnitHeat(UnitHeatNum)%MaxAirVolFlow .NE. AutoSize)THEN
          CALL ShowWarningError('Specified in '//TRIM(CurrentModuleObject)//' = '//TRIM(UnitHeat(UnitHeatNum)%Name))
          CALL ShowContinueError('...the fan flow rate is autosized while the unit heater flow rate is not.')
          CALL ShowContinueError('...this can lead to unexpected results where the fan flow rate is less than required.')
        ELSEIF(FanVolFlow .NE. AutoSize .AND. UnitHeat(UnitHeatNum)%MaxAirVolFlow .EQ. AutoSize)THEN
          CALL ShowWarningError('Specified in '//TRIM(CurrentModuleObject)//' = '//TRIM(UnitHeat(UnitHeatNum)%Name))
          CALL ShowContinueError('...the unit heater flow rate is autosized while the fan flow rate is not.')
          CALL ShowContinueError('...this can lead to unexpected results where the fan flow rate is less than required.')
        ENDIF
        UnitHeat(UnitHeatNum)%FanAvailSchedPtr = GetFanAvailSchPtr(UnitHeat(UnitHeatNum)%FanType, &
                                                 UnitHeat(UnitHeatNum)%FanName,ErrFlag)
      ENDIF
    ENDIF

          ! Heating coil information:
    SELECT CASE (Alphas(8))
      CASE ('COIL:HEATING:WATER')
        UnitHeat(UnitHeatNum)%HCoilType = WaterCoil
        UnitHeat(UnitHeatNum)%HCoil_PlantTypeNum = TypeOf_CoilWaterSimpleHeating
      CASE ('COIL:HEATING:STEAM')
        UnitHeat(UnitHeatNum)%HCoilType = SteamCoil
        UnitHeat(UnitHeatNum)%HCoil_PlantTypeNum = TypeOf_CoilSteamAirHeating
      CASE ('COIL:HEATING:ELECTRIC')
        UnitHeat(UnitHeatNum)%HCoilType = ElectricCoil
      CASE ('COIL:HEATING:GAS')
        UnitHeat(UnitHeatNum)%HCoilType = GasCoil
      CASE DEFAULT
        CALL ShowSevereError('Illegal '//TRIM(cAlphaFields(8))//' = '//TRIM(Alphas(8)))
        CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//'='//TRIM(UnitHeat(UnitHeatNum)%Name))
        ErrorsFound = .TRUE.
        ErrFlag = .TRUE.
    END SELECT
    IF (.NOT. ErrFlag) THEN
        UnitHeat(UnitHeatNum)%HCoilTypeCh = Alphas(8)
        UnitHeat(UnitHeatNum)%HCoilName   = Alphas(9)
        CALL ValidateComponent(Alphas(8),UnitHeat(UnitHeatNum)%HCoilName,IsNotOK,TRIM(CurrentModuleObject))
        IF (IsNotOK) THEN
          CALL ShowContinueError('specified in '//TRIM(CurrentModuleObject)//' = "' // &
                                  TRIM(UnitHeat(UnitHeatNum)%Name)//'"')
          ErrorsFound = .TRUE.
        ELSE
          ! The heating coil control node is necessary for hot water and steam coils, but not necessary for an
          ! electric or gas coil.
          IF (UnitHeat(UnitHeatNum)%HCoilType .EQ. WaterCoil .OR.    &
              UnitHeat(UnitHeatNum)%HCoilType .EQ. SteamCoil) THEN
            ! mine the hot water or steam node from the coil object
            ErrFlag = .FALSE.
            IF(UnitHeat(UnitHeatNum)%HCoilType .EQ. WaterCoil)THEN
              UnitHeat(UnitHeatNum)%HotControlNode = GetCoilWaterInletNode('Coil:Heating:Water',  &
                                                     UnitHeat(UnitHeatNum)%HCoilName,ErrFlag)
            ELSE ! its a steam coil
              UnitHeat(UnitHeatNum)%HCoil_Index = GetSteamCoilIndex('COIL:HEATING:STEAM',UnitHeat(UnitHeatNum)%HCoilName,ErrFlag)
              UnitHeat(UnitHeatNum)%HotControlNode = GetSteamCoilSteamInletNode(UnitHeat(UnitHeatNum)%HCoil_Index, &
                                                                                UnitHeat(UnitHeatNum)%HCoilName,ErrFlag)
            END IF
            ! Other error checks should trap before it gets to this point in the code, but including just in case.
            IF (ErrFlag) THEN
              CALL ShowContinueError('that was specified in '//TRIM(CurrentModuleObject)//' = "' // &
                                      TRIM(UnitHeat(UnitHeatNum)%Name)//'"')
              ErrorsFound = .TRUE.
            END IF
          END IF
        END IF
    ENDIF

    UnitHeat(UnitHeatNum)%MaxVolHotWaterFlow  = Numbers(2)
    UnitHeat(UnitHeatNum)%MinVolHotWaterFlow  = Numbers(3)
    UnitHeat(UnitHeatNum)%MaxVolHotSteamFlow  = Numbers(2)
    UnitHeat(UnitHeatNum)%MinVolHotSteamFlow  = Numbers(3)

    UnitHeat(UnitHeatNum)%HotControlOffset = Numbers(4)
    ! Set default convergence tolerance
    IF (UnitHeat(UnitHeatNum)%HotControlOffset .LE. 0.0d0) THEN
      UnitHeat(UnitHeatNum)%HotControlOffset = 0.001d0
    END IF

    IF (.NOT. lAlphaBlanks(10)) THEN
      UnitHeat(UnitHeatNum)%AvailManagerListName = Alphas(10)
      ZoneComp(UnitHeater_Num)%ZoneCompAvailMgrs(UnitHeatNum)%AvailManagerListName  = Alphas(10)
    ENDIF

    ! check that unit heater air inlet node must be the same as a zone exhaust node
    ZoneNodeNotFound = .TRUE.
    DO CtrlZone = 1,NumOfZones
      IF (.not. ZoneEquipConfig(CtrlZone)%IsControlled) CYCLE
      DO NodeNum = 1,ZoneEquipConfig(CtrlZone)%NumExhaustNodes
        IF (UnitHeat(UnitHeatNum)%AirInNode .EQ. ZoneEquipConfig(CtrlZone)%ExhaustNode(NodeNum)) THEN
          ZoneNodeNotFound = .FALSE.
          EXIT
        END IF
      END DO
    END DO
    IF(ZoneNodeNotFound)THEN
      CALL ShowSevereError(TRIM(CurrentModuleObject)//' = "'//TRIM(UnitHeat(UnitHeatNum)%Name)//'".'// &
                         ' Unit heater air inlet node name must be the same as a zone exhaust node name.')
      CALL ShowContinueError('..Zone exhaust node name is specified in ZoneHVAC:EquipmentConnections object.')
      CALL ShowContinueError('..Unit heater air inlet node name = '//TRIM(NodeID(UnitHeat(UnitHeatNum)%AirInNode)))
      ErrorsFound=.TRUE.
    END IF
    ! check that unit heater air outlet node is a zone inlet node.
    ZoneNodeNotFound = .TRUE.
    DO CtrlZone = 1,NumOfZones
      IF (.not. ZoneEquipConfig(CtrlZone)%IsControlled) CYCLE
      DO NodeNum = 1,ZoneEquipConfig(CtrlZone)%NumInletNodes
        IF (UnitHeat(UnitHeatNum)%AirOutNode .EQ. ZoneEquipConfig(CtrlZone)%InletNode(NodeNum)) THEN
          ZoneNodeNotFound = .FALSE.
          EXIT
        END IF
      END DO
    END DO
    IF(ZoneNodeNotFound)THEN
      CALL ShowSevereError(TRIM(CurrentModuleObject)//' = "'//TRIM(UnitHeat(UnitHeatNum)%Name)//'".'// &
                           ' Unit heater air outlet node name must be the same as a zone inlet node name.')
      CALL ShowContinueError('..Zone inlet node name is specified in ZoneHVAC:EquipmentConnections object.')
      CALL ShowContinueError('..Unit heater air outlet node name = '//TRIM(NodeID(UnitHeat(UnitHeatNum)%AirOutNode)))
      ErrorsFound=.TRUE.
    END IF

    ! Add fan to component sets array
    CALL SetUpCompSets(TRIM(CurrentModuleObject), UnitHeat(UnitHeatNum)%Name, &
                       UnitHeat(UnitHeatNum)%FanType,UnitHeat(UnitHeatNum)%FanName, &
                       NodeID(UnitHeat(UnitHeatNum)%AirInNode),NodeID(UnitHeat(UnitHeatNum)%FanOutletNode))

    ! Add heating coil to component sets array
    CALL SetUpCompSets(TRIM(CurrentModuleObject), UnitHeat(UnitHeatNum)%Name, &
                       UnitHeat(UnitHeatNum)%HCoilTypeCh,UnitHeat(UnitHeatNum)%HCoilName, &
                       NodeID(UnitHeat(UnitHeatNum)%FanOutletNode),NodeID(UnitHeat(UnitHeatNum)%AirOutNode))

  END DO  ! ...loop over all of the unit heaters found in the input file

  DEALLOCATE(Alphas)
  DEALLOCATE(Numbers)
  DEALLOCATE(cAlphaFields)
  DEALLOCATE(cNumericFields)
  DEALLOCATE(lAlphaBlanks)
  DEALLOCATE(lNumericBlanks)

  IF (ErrorsFound) CALL ShowFatalError(RoutineName//'Errors found in input')

          ! Setup Report variables for the Unit Heaters, CurrentModuleObject='ZoneHVAC:UnitHeater'
  DO UnitHeatNum = 1, NumOfUnitHeats
    CALL SetupOutputVariable('Zone Unit Heater Heating Rate [W]',UnitHeat(UnitHeatNum)%HeatPower, &
                             'System','Average',UnitHeat(UnitHeatNum)%Name)
    CALL SetupOutputVariable('Zone Unit Heater Heating Energy [J]',UnitHeat(UnitHeatNum)%HeatEnergy, &
                             'System','Sum',UnitHeat(UnitHeatNum)%Name)
    CALL SetupOutputVariable('Zone Unit Heater Fan Electric Power [W]',UnitHeat(UnitHeatNum)%ElecPower, &
                             'System','Average',UnitHeat(UnitHeatNum)%Name)
          ! Note that the unit heater fan electric is NOT metered because this value is already metered through the fan component
    CALL SetupOutputVariable('Zone Unit Heater Fan Electric Energy [J]',UnitHeat(UnitHeatNum)%ElecEnergy, &
                             'System','Sum',UnitHeat(UnitHeatNum)%Name)
    CALL SetupOutputVariable('Zone Unit Heater Fan Availability Status []', UnitHeat(UnitHeatNum)%AvailStatus,&
                             'System','Average',UnitHeat(UnitHeatNum)%Name)
  END DO

  RETURN

END SUBROUTINE GetUnitHeaterInput

SUBROUTINE InitUnitHeater(UnitHeatNum, ZoneNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Rick Strand
          !       DATE WRITTEN   May 2000
          !       MODIFIED       Chandan Sharma, FSEC, March 2011: Added ZoneHVAC sys avail manager
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine initializes all of the data elements which are necessary
          ! to simulate a unit heater.

          ! METHODOLOGY EMPLOYED:
          ! Uses the status flags to trigger initializations.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataEnvironment,    ONLY : StdBaroPress, StdRhoAir
  USE DataZoneEquipment,  ONLY: ZoneEquipInputsFilled,CheckZoneEquipmentList, UnitHeater_Num
  USE DataHVACGlobals,    ONLY: ZoneComp, ZoneCompTurnFansOn, ZoneCompTurnFansOff
  USE DataPlant,          ONLY : PlantLoop, TypeOf_CoilWaterSimpleHeating, TypeOf_CoilSteamAirHeating, &
                                 ScanPlantLoopsForObject
  USE FluidProperties,    ONLY : GetDensityGlycol
  USE PlantUtilities,     ONLY : InitComponentNodes
  USE DataGlobals,        ONLY : AnyPlantInModel
  USE DataZoneEnergyDemands

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: UnitHeatNum         ! index for the current unit heater
  INTEGER,          INTENT(IN)  :: ZoneNum   ! number of zone being served

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  LOGICAL,SAVE        :: MyOneTimeFlag = .true.
  LOGICAL, ALLOCATABLE,Save, DIMENSION(:) :: MyEnvrnFlag
  LOGICAL, ALLOCATABLE, SAVE, DIMENSION(:) :: MyPlantScanFlag
  LOGICAL,SAVE        :: ZoneEquipmentListChecked = .false.  ! True after the Zone Equipment List has been checked for items
  Integer             :: Loop
  INTEGER        :: HotConNode         ! hot water control node number in unit heater loop
  INTEGER        :: InNode             ! inlet node number in unit heater loop
  INTEGER        :: OutNode            ! outlet node number in unit heater loop
  REAL(r64)      :: RhoAir             ! air density at InNode
  REAL(r64)      :: TempSteamIn
  REAL(r64)      :: SteamDensity
!  INTEGER        :: RefrigIndex
  REAL(r64)      :: rho  ! local fluid density
  LOGICAL        :: errFlag
  LOGICAL        :: SetMassFlowRateToZero = .FALSE. ! TRUE when mass flow rates need to be set to zero
          ! FLOW:

  ! Do the one time initializations
  IF (MyOneTimeFlag) THEN

    ALLOCATE(MyEnvrnFlag(NumOfUnitHeats))
    ALLOCATE(MySizeFlag(NumOfUnitHeats))
    ALLOCATE(MyPlantScanFlag(NumOfUnitHeats))
    MyEnvrnFlag = .TRUE.
    MySizeFlag = .TRUE.
    MyPlantScanFlag = .TRUE.
    MyOneTimeFlag = .false.

  END IF

  IF (ALLOCATED(ZoneComp)) THEN
    ZoneComp(UnitHeater_Num)%ZoneCompAvailMgrs(UnitHeatNum)%ZoneNum = ZoneNum
    UnitHeat(UnitHeatNum)%AvailStatus = ZoneComp(UnitHeater_Num)%ZoneCompAvailMgrs(UnitHeatNum)%AvailStatus
  ENDIF

  IF (MyPlantScanFlag(UnitHeatNum) .AND. ALLOCATED(PlantLoop)) THEN
    IF ((UnitHeat(UnitHeatNum)%HCoil_PlantTypeNum == TypeOf_CoilWaterSimpleHeating) .OR. &
        (UnitHeat(UnitHeatNum)%HCoil_PlantTypeNum == TypeOf_CoilSteamAirHeating)) THEN
      errFlag=.false.
      CALL ScanPlantLoopsForObject( UnitHeat(UnitHeatNum)%HCoilName, &
                                          UnitHeat(UnitHeatNum)%HCoil_PlantTypeNum, &
                                          UnitHeat(UnitHeatNum)%HWLoopNum,   &
                                          UnitHeat(UnitHeatNum)%HWLoopSide,  &
                                          UnitHeat(UnitHeatNum)%HWBranchNum, &
                                          UnitHeat(UnitHeatNum)%HWCompNum,  &
                                          errFlag=errFlag)
      IF (errFlag) THEN
        CALL ShowContinueError('Reference Unit="'//trim(UnitHeat(UnitHeatNum)%Name)//'", type=ZoneHVAC:UnitHeater')
        CALL ShowFatalError('InitUnitHeater: Program terminated due to previous condition(s).')
      ENDIF

      UnitHeat(UnitHeatNum)%HotCoilOutNodeNum = &
            PlantLoop(UnitHeat(UnitHeatNum)%HWLoopNum)%LoopSide(UnitHeat(UnitHeatNum)%HWLoopSide) &
                         %Branch(UnitHeat(UnitHeatNum)%HWBranchNum)%Comp(UnitHeat(UnitHeatNum)%HWCompNum)%NodeNumOut
    ENDIF
    MyPlantScanFlag(UnitHeatNum) = .FALSE.
  ELSEIF (MyPlantScanFlag(UnitHeatNum) .AND. .NOT. AnyPlantInModel) THEN
    MyPlantScanFlag(UnitHeatNum) = .FALSE.
  ENDIF
  ! need to check all units to see if they are on Zone Equipment List or issue warning
  IF (.not. ZoneEquipmentListChecked .and. ZoneEquipInputsFilled) THEN
    ZoneEquipmentListChecked=.true.
    DO Loop=1,NumOfUnitHeats
      IF (CheckZoneEquipmentList('ZoneHVAC:UnitHeater',UnitHeat(Loop)%Name)) CYCLE
      CALL ShowSevereError('InitUnitHeater: Unit=[UNIT HEATER,'//TRIM(UnitHeat(Loop)%Name)//  &
           '] is not on any ZoneHVAC:EquipmentList.  It will not be simulated.')
    ENDDO
  ENDIF

  IF ( .NOT. SysSizingCalc .AND. MySizeFlag(UnitHeatNum) .AND. .NOT. MyPlantScanFlag(UnitHeatNum) ) THEN

    CALL SizeUnitHeater(UnitHeatNum)

    MySizeFlag(UnitHeatNum) = .FALSE.
  END IF          ! Do the one time initializations

  IF (BeginEnvrnFlag .AND. MyEnvrnFlag(UnitHeatNum) .AND. .NOT. MyPlantScanFlag(UnitHeatNum)) THEN
    InNode         = UnitHeat(UnitHeatNum)%AirInNode
    OutNode        = UnitHeat(UnitHeatNum)%AirOutNode
    HotConNode     = UnitHeat(UnitHeatNum)%HotControlNode
    RhoAir         = StdRhoAir

    ! set the mass flow rates from the input volume flow rates
    UnitHeat(UnitHeatNum)%MaxAirMassFlow = RhoAir*UnitHeat(UnitHeatNum)%MaxAirVolFlow

    ! set the node max and min mass flow rates
    Node(OutNode)%MassFlowRateMax = UnitHeat(UnitHeatNum)%MaxAirMassFlow
    Node(OutNode)%MassFlowRateMin = 0.0d0

    Node(InNode)%MassFlowRateMax = UnitHeat(UnitHeatNum)%MaxAirMassFlow
    Node(InNode)%MassFlowRateMin = 0.0d0

    IF (UnitHeat(UnitHeatNum)%HCoilType == WaterCoil) THEN
      rho = GetDensityGlycol( PlantLoop(UnitHeat(UnitHeatNum)%HWLoopNum)%FluidName, &
                             60.d0, &
                             PlantLoop(UnitHeat(UnitHeatNum)%HWLoopNum)%FluidIndex, &
                             'InitUnitHeater')

      UnitHeat(UnitHeatNum)%MaxHotWaterFlow = rho*UnitHeat(UnitHeatNum)%MaxVolHotWaterFlow
      UnitHeat(UnitHeatNum)%MinHotWaterFlow = rho*UnitHeat(UnitHeatNum)%MinVolHotWaterFlow
      Call InitComponentNodes ( UnitHeat(UnitHeatNum)%MinHotWaterFlow, &
                                UnitHeat(UnitHeatNum)%MaxHotWaterFlow, &
                                UnitHeat(UnitHeatNum)%HotControlNode, &
                                UnitHeat(UnitHeatNum)%HotCoilOutNodeNum, &
                                UnitHeat(UnitHeatNum)%HWLoopNum, &
                                UnitHeat(UnitHeatNum)%HWLoopSide, &
                                UnitHeat(UnitHeatNum)%HWBranchNum, &
                                UnitHeat(UnitHeatNum)%HWCompNum )
    END IF
    IF (UnitHeat(UnitHeatNum)%HCoilType == SteamCoil) THEN
      TempSteamIn= 100.00d0
      SteamDensity=GetSatDensityRefrig('STEAM',TempSteamIn,1.0d0,UnitHeat(UnitHeatNum)%HCoil_FluidIndex,'InitUnitHeater')
      UnitHeat(UnitHeatNum)%MaxHotSteamFlow = SteamDensity*UnitHeat(UnitHeatNum)%MaxVolHotSteamFlow
      UnitHeat(UnitHeatNum)%MinHotSteamFlow = SteamDensity*UnitHeat(UnitHeatNum)%MinVolHotSteamFlow

      Call InitComponentNodes (      UnitHeat(UnitHeatNum)%MinHotSteamFlow, &
                                     UnitHeat(UnitHeatNum)%MaxHotSteamFlow, &
                                     UnitHeat(UnitHeatNum)%HotControlNode, &
                                     UnitHeat(UnitHeatNum)%HotCoilOutNodeNum, &
                                     UnitHeat(UnitHeatNum)%HWLoopNum, &
                                     UnitHeat(UnitHeatNum)%HWLoopSide, &
                                     UnitHeat(UnitHeatNum)%HWBranchNum, &
                                     UnitHeat(UnitHeatNum)%HWCompNum )
    END IF

    MyEnvrnFlag(UnitHeatNum) = .FALSE.
  END IF  ! ...end start of environment inits

  IF (.NOT. BeginEnvrnFlag) MyEnvrnFlag(UnitHeatNum) = .TRUE.

          ! These initializations are done every iteration...
  InNode  = UnitHeat(UnitHeatNum)%AirInNode
  OutNode = UnitHeat(UnitHeatNum)%AirOutNode

  SetMassFlowRateToZero = .FALSE.
  IF(GetCurrentScheduleValue(UnitHeat(UnitHeatNum)%SchedPtr) .GT. 0)THEN
    IF((GetCurrentScheduleValue(UnitHeat(UnitHeatNum)%FanAvailSchedPtr) .GT. 0 &
        .OR. ZoneCompTurnFansOn) .AND. .NOT. ZoneCompTurnFansOff)THEN
      IF ( UnitHeat(UnitHeatNum)%FanControlTypeOnOff &
           .AND. ((ZoneSysEnergyDemand(ZoneNum)%RemainingOutputRequired < SmallLoad) .OR. &
           (CurDeadBandOrSetback(ZoneNum)))) THEN
        SetMassFlowRateToZero = .TRUE.
      ENDIF
    ELSE
      SetMassFlowRateToZero = .TRUE.
    ENDIF
  ELSE
    SetMassFlowRateToZero = .TRUE.
  ENDIF

  IF (SetMassFlowRateToZero) THEN
    Node(InNode)%MassFlowRate          = 0.0d0
    Node(InNode)%MassFlowRateMaxAvail  = 0.0d0
    Node(InNode)%MassFlowRateMinAvail  = 0.0d0
    Node(OutNode)%MassFlowRate         = 0.0d0
    Node(OutNode)%MassFlowRateMaxAvail = 0.0d0
    Node(OutNode)%MassFlowRateMinAvail = 0.0d0
  ELSE
    Node(InNode)%MassFlowRate          = UnitHeat(UnitHeatNum)%MaxAirMassFlow
    Node(InNode)%MassFlowRateMaxAvail  = UnitHeat(UnitHeatNum)%MaxAirMassFlow
    Node(InNode)%MassFlowRateMinAvail  = UnitHeat(UnitHeatNum)%MaxAirMassFlow
    Node(OutNode)%MassFlowRate         = UnitHeat(UnitHeatNum)%MaxAirMassFlow
    Node(OutNode)%MassFlowRateMaxAvail = UnitHeat(UnitHeatNum)%MaxAirMassFlow
    Node(OutNode)%MassFlowRateMinAvail = UnitHeat(UnitHeatNum)%MaxAirMassFlow
  ENDIF

  ! Just in case the unit is off and conditions do not get sent through
  ! the unit for some reason, set the outlet conditions equal to the inlet
  ! conditions of the unit heater
  Node(OutNode)%Temp     = Node(InNode)%Temp
  Node(OutNode)%Press    = Node(InNode)%Press
  Node(OutNode)%HumRat   = Node(InNode)%HumRat
  Node(OutNode)%Enthalpy = Node(InNode)%Enthalpy

  RETURN

END SUBROUTINE InitUnitHeater

SUBROUTINE SizeUnitHeater(UnitHeatNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   February 2002
          !       MODIFIED       August 2013 Daeho Kang, add component sizing table entries
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is for sizing Unit Heater components for which flow rates have not been
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
!  USE BranchInputManager, ONLY: MyPlantSizingIndex
  USE DataPlant,          ONLY: PlantLoop, MyPlantSizingIndex
  USE Psychrometrics,     ONLY: CpHW
  USE ReportSizingManager, ONLY: ReportSizingOutput
  USE General,             ONLY: RoundSigDigits

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  Integer, Intent(IN) :: UnitHeatNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER             :: PltSizHeatNum ! index of plant sizing object for 1st heating loop
  LOGICAL             :: ErrorsFound
  REAL(r64)           :: DesCoilLoad
  REAL(r64)           :: TempSteamIn
  REAL(r64)           :: EnthSteamInDry
  REAL(r64)           :: EnthSteamOutWet
  REAL(r64)           :: LatentHeatSteam
  REAL(r64)           :: SteamDensity
  INTEGER             :: RefrigIndex=0
  INTEGER             :: CoilWaterInletNode=0
  INTEGER             :: CoilWaterOutletNode=0
  INTEGER             :: CoilSteamInletNode=0
  INTEGER             :: CoilSteamOutletNode=0
  REAL(r64)           :: Cp ! local temporary for fluid specific heat
  REAL(r64)           :: rho ! local temporary for fluid density
  LOGICAL             :: IsAutosize             ! Indicator to autosize
  REAL(r64)           :: MaxAirVolFlowDes       ! Autosized maximum air flow for reporting
  REAL(r64)           :: MaxAirVolFlowUser      ! Hardsized maximum air flow for reporting
  REAL(r64)           :: MaxVolHotWaterFlowDes  ! Autosized maximum hot water flow for reporting
  REAL(r64)           :: MaxVolHotWaterFlowUser ! Hardsized maximum hot water flow for reporting
  REAL(r64)           :: MaxVolHotSteamFlowDes  ! Autosized maximum hot steam flow for reporting
  REAL(r64)           :: MaxVolHotSteamFlowUser ! Hardsized maximum hot steam flow for reporting

  PltSizHeatNum = 0
  ErrorsFound = .FALSE.
  IsAutosize = .FALSE.
  MaxAirVolFlowDes = 0.0d0
  MaxAirVolFlowUser = 0.0d0
  MaxVolHotWaterFlowDes = 0.0d0
  MaxVolHotWaterFlowUser = 0.0d0
  MaxVolHotSteamFlowDes = 0.0d0
  MaxVolHotSteamFlowUser = 0.0d0

  IF (UnitHeat(UnitHeatNum)%MaxAirVolFlow == AutoSize) THEN
    IsAutosize = .TRUE.
  END IF
  IF (CurZoneEqNum > 0) THEN
    IF (.NOT. IsAutosize .AND. .NOT. ZoneSizingRunDone) THEN ! Simulation continue
      IF (UnitHeat(UnitHeatNum)%MaxAirVolFlow > 0.0d0) THEN
        CALL ReportSizingOutput('ZoneHVAC:UnitHeater', UnitHeat(UnitHeatNum)%Name, &
                        'User-Specified Maximum Supply Air Flow Rate [m3/s]', UnitHeat(UnitHeatNum)%MaxAirVolFlow)
      END IF
    ELSE
      CALL CheckZoneSizing('ZoneHVAC:UnitHeater', UnitHeat(UnitHeatNum)%Name)
      IF (FinalZoneSizing(CurZoneEqNum)%DesHeatVolFlow >= SmallAirVolFlow) THEN
        MaxAirVolFlowDes = FinalZoneSizing(CurZoneEqNum)%DesHeatVolFlow
      ELSE
        MaxAirVolFlowDes = 0.0d0
      END IF
      IF (IsAutosize) THEN
        UnitHeat(UnitHeatNum)%MaxAirVolFlow = MaxAirVolFlowDes
        CALL ReportSizingOutput('ZoneHVAC:UnitHeater', UnitHeat(UnitHeatNum)%Name, &
                        'Design Size Maximum Supply Air Flow Rate [m3/s]', MaxAirVolFlowDes)
      ELSE
        IF (UnitHeat(UnitHeatNum)%MaxAirVolFlow > 0.0d0 .AND. MaxAirVolFlowDes > 0.0d0) THEN
          MaxAirVolFlowUser = UnitHeat(UnitHeatNum)%MaxAirVolFlow
          CALL ReportSizingOutput('ZoneHVAC:UnitHeater', UnitHeat(UnitHeatNum)%Name, &
                        'Design Size Maximum Supply Air Flow Rate [m3/s]', MaxAirVolFlowDes, &
                        'User-Specified Maximum Supply Air Flow Rate [m3/s]', MaxAirVolFlowUser)
          IF (DisplayExtraWarnings) THEN
            IF ((ABS(MaxAirVolFlowDes - MaxAirVolFlowUser)/MaxAirVolFlowUser) > AutoVsHardSizingThreshold) THEN
              CALL ShowMessage('SizeUnitHeater: Potential issue with equipment sizing for ZoneHVAC:UnitHeater ' &
                               //TRIM(UnitHeat(UnitHeatNum)%Name))
              CALL ShowContinueError('User-Specified Maximum Hot Water Flow of '// &
                                      TRIM(RoundSigDigits(MaxAirVolFlowUser,5))// ' [m3/s]')
              CALL ShowContinueError('.differs from Design Size Maximum Hot Water Flow of ' // &
                                      TRIM(RoundSigDigits(MaxAirVolFlowDes,5))// ' [m3/s]')
              CALL ShowContinueError('This may, or may not, indicate mismatched component sizes.')
              CALL ShowContinueError('Verify that the value entered is intended and is consistent with other components.')
            END IF
          ENDIF
        END IF
      END IF
    END IF
  END IF

  IsAutosize = .FALSE.
  IF(UnitHeat(UnitHeatNum)%MaxVolHotWaterFlow==AutoSize) THEN
    IsAutosize = .TRUE.
  END IF

  IF (UnitHeat(UnitHeatNum)%HCoilType == WaterCoil) THEN

    IF (CurZoneEqNum > 0) THEN
      IF (.NOT. IsAutosize .AND. .NOT. ZoneSizingRunDone) THEN ! Simulation continue
        IF (UnitHeat(UnitHeatNum)%MaxVolHotWaterFlow > 0.0d0) THEN
          CALL ReportSizingOutput('ZoneHVAC:UnitHeater', UnitHeat(UnitHeatNum)%Name, &
                       'User-Specified Maximum Hot Water Flow [m3/s]', UnitHeat(UnitHeatNum)%MaxVolHotWaterFlow)
        END IF
      ELSE
        CALL CheckZoneSizing('ZoneHVAC:UnitHeater', UnitHeat(UnitHeatNum)%Name)

        CoilWaterInletNode = GetCoilWaterInletNode('Coil:Heating:Water',UnitHeat(UnitHeatNum)%HCoilName,ErrorsFound)
        CoilWaterOutletNode = GetCoilWaterOutletNode('Coil:Heating:Water',UnitHeat(UnitHeatNum)%HCoilName,ErrorsFound)
        IF (IsAutosize) THEN
          PltSizHeatNum = MyPlantSizingIndex('Coil:Heating:Water', UnitHeat(UnitHeatNum)%HCoilName, CoilWaterInletNode, &
                                       CoilWaterOutletNode, ErrorsFound)
          IF (PltSizHeatNum > 0) THEN
            DesCoilLoad = FinalZoneSizing(CurZoneEqNum)%DesHeatLoad
            IF (DesCoilLoad >= SmallLOad) THEN

              rho = GetDensityGlycol(PlantLoop(UnitHeat(UnitHeatNum)%HWLoopNum)%FluidName, &
                                    60.d0, &
                                     PlantLoop(UnitHeat(UnitHeatNum)%HWLoopNum)%FluidIndex, &
                                     'SizeUnitHeater')
              Cp = GetSpecificHeatGlycol(PlantLoop(UnitHeat(UnitHeatNum)%HWLoopNum)%FluidName, &
                                    60.d0, &
                                     PlantLoop(UnitHeat(UnitHeatNum)%HWLoopNum)%FluidIndex, &
                                     'SizeUnitHeater')

              MaxVolHotWaterFlowDes = DesCoilLoad / &
                                    ( PlantSizData(PltSizHeatNum)%DeltaT * &
                                    Cp  * rho )
            ELSE
              MaxVolHotWaterFlowDes = 0.0d0
            END IF
          ELSE
            CALL ShowSevereError('Autosizing of water flow requires a heating loop Sizing:Plant object')
            CALL ShowContinueError('Occurs in ' // 'ZoneHVAC:UnitHeater' // ' Object=' &
                                 //TRIM(UnitHeat(UnitHeatNum)%Name))
            ErrorsFound = .TRUE.
          END IF
        END IF
        IF (IsAutosize) THEN
          UnitHeat(UnitHeatNum)%MaxVolHotWaterFlow = MaxVolHotWaterFlowDes
          CALL ReportSizingOutput('ZoneHVAC:UnitHeater', UnitHeat(UnitHeatNum)%Name, &
                       'Design Size Maximum Hot Water Flow [m3/s]', MaxVolHotWaterFlowDes)
        ELSE
          IF (UnitHeat(UnitHeatNum)%MaxVolHotWaterFlow > 0.0d0 .AND. MaxVolHotWaterFlowDes > 0.0d0) THEN
            MaxVolHotWaterFlowUser = UnitHeat(UnitHeatNum)%MaxVolHotWaterFlow
            CALL ReportSizingOutput('ZoneHVAC:UnitHeater', UnitHeat(UnitHeatNum)%Name, &
                       'Design Size Maximum Hot Water Flow [m3/s]', MaxVolHotWaterFlowDes, &
                       'User-Specified Maximum Hot Water Flow [m3/s]', MaxVolHotWaterFlowUser)
            IF (DisplayExtraWarnings) THEN
              IF ((ABS(MaxVolHotWaterFlowDes - MaxVolHotWaterFlowUser)/MaxVolHotWaterFlowUser) > AutoVsHardSizingThreshold) THEN
                CALL ShowMessage('SizeUnitHeater: Potential issue with equipment sizing for ZoneHVAC:UnitHeater ' &
                               //TRIM(UnitHeat(UnitHeatNum)%Name))
                CALL ShowContinueError('User-Specified Maximum Hot Water Flow of '// &
                                      TRIM(RoundSigDigits(MaxVolHotWaterFlowUser,5))// ' [m3/s]')
                CALL ShowContinueError('differs from Design Size Maximum Hot Water Flow of ' // &
                                      TRIM(RoundSigDigits(MaxVolHotWaterFlowDes,5))// ' [m3/s]')
                CALL ShowContinueError('This may, or may not, indicate mismatched component sizes.')
                CALL ShowContinueError('Verify that the value entered is intended and is consistent with other components.')
              END IF
            ENDIF
          END IF
        END IF
      END IF
    END IF
  ELSE
    UnitHeat(UnitHeatNum)%MaxVolHotWaterFlow = 0.0d0
  END IF

  IsAutosize = .FALSE.
  IF (UnitHeat(UnitHeatNum)%MaxVolHotSteamFlow==AutoSize) THEN
    IsAutosize = .TRUE.
  END IF

  IF (UnitHeat(UnitHeatNum)%HCoilType == SteamCoil) THEN

    IF (CurZoneEqNum > 0) THEN
      IF (.NOT. IsAutosize .AND. .NOT. ZoneSizingRunDone) THEN ! Simulation continue
        IF (UnitHeat(UnitHeatNum)%MaxVolHotSteamFlow > 0.0d0) THEN
          CALL ReportSizingOutput('ZoneHVAC:UnitHeater', UnitHeat(UnitHeatNum)%Name, &
                                'User-Specified Maximum Steam Flow [m3/s]', UnitHeat(UnitHeatNum)%MaxVolHotSteamFlow)
        END IF
      ELSE
        CALL CheckZoneSizing('ZoneHVAC:UnitHeater', UnitHeat(UnitHeatNum)%Name)

        CoilSteamInletNode = GetCoilSteamInletNode('Coil:Heating:Steam',UnitHeat(UnitHeatNum)%HCoilName,ErrorsFound)
        CoilSteamOutletNode = GetCoilSteamInletNode('Coil:Heating:Steam',UnitHeat(UnitHeatNum)%HCoilName,ErrorsFound)
        IF (IsAutosize) THEN
          PltSizHeatNum = MyPlantSizingIndex('Coil:Heating:Steam', UnitHeat(UnitHeatNum)%HCoilName, CoilSteamInletNode, &
                                       CoilSteamOutletNode, ErrorsFound)
          IF (PltSizHeatNum > 0) THEN
            DesCoilLoad = FinalZoneSizing(CurZoneEqNum)%DesHeatLoad
            IF (DesCoilLoad >= SmallLOad) THEN
              TempSteamIn= 100.00d0
              EnthSteamInDry =  GetSatEnthalpyRefrig('STEAM',TempSteamIn,1.0d0,RefrigIndex,'SizeUnitHeater')
              EnthSteamOutWet=  GetSatEnthalpyRefrig('STEAM',TempSteamIn,0.0d0,RefrigIndex,'SizeUnitHeater')
              LatentHeatSteam=EnthSteamInDry-EnthSteamOutWet
              SteamDensity=GetSatDensityRefrig('STEAM',TempSteamIn,1.0d0,RefrigIndex,'SizeUnitHeater')
              MaxVolHotSteamFlowDes = DesCoilLoad/(SteamDensity*(LatentHeatSteam + &
                                    PlantSizData(PltSizHeatNum)%DeltaT*CPHW(PlantSizData(PltSizHeatNum)%ExitTemp)))
            ELSE
              MaxVolHotSteamFlowDes = 0.0d0
            END IF
          ELSE
            CALL ShowSevereError('Autosizing of Steam flow requires a heating loop Sizing:Plant object')
            CALL ShowContinueError('Occurs in ' // 'ZoneHVAC:UnitHeater' // ' Object=' &
                                 //TRIM(UnitHeat(UnitHeatNum)%Name))
            ErrorsFound = .TRUE.
          END IF
        END IF
        IF (IsAutosize) THEN
          UnitHeat(UnitHeatNum)%MaxVolHotSteamFlow = MaxVolHotSteamFlowDes
          CALL ReportSizingOutput('ZoneHVAC:UnitHeater', UnitHeat(UnitHeatNum)%Name, &
                       'Design Size Maximum Steam Flow [m3/s]', MaxVolHotSteamFlowDes)
        ELSE
          IF (UnitHeat(UnitHeatNum)%MaxVolHotSteamFlow > 0.0d0 .AND. MaxVolHotSteamFlowDes > 0.0d0) THEN
            MaxVolHotSteamFlowUser = UnitHeat(UnitHeatNum)%MaxVolHotSteamFlow
            CALL ReportSizingOutput('ZoneHVAC:UnitHeater', UnitHeat(UnitHeatNum)%Name, &
                       'Design Size Maximum Steam Flow [m3/s]', MaxVolHotSteamFlowDes, &
                       'User-Specified Maximum Steam Flow [m3/s]', MaxVolHotSteamFlowUser)
            IF (DisplayExtraWarnings) THEN
              IF ((ABS(MaxVolHotSteamFlowDes - MaxVolHotSteamFlowUser)/MaxVolHotSteamFlowUser) > AutoVsHardSizingThreshold) THEN
                CALL ShowMessage('SizeUnitHeater: Potential issue with equipment sizing for ZoneHVAC:UnitHeater ' &
                                  //TRIM(UnitHeat(UnitHeatNum)%Name))
                CALL ShowContinueError('User-Specified Maximum Steam Flow of '// &
                                      TRIM(RoundSigDigits(MaxVolHotSteamFlowUser,5))// ' [m3/s]')
                CALL ShowContinueError('differs from Design Size Maximum Steam Flow of ' // &
                                      TRIM(RoundSigDigits(MaxVolHotSteamFlowDes,5))// ' [m3/s]')
                CALL ShowContinueError('This may, or may not, indicate mismatched component sizes.')
                CALL ShowContinueError('Verify that the value entered is intended and is consistent with other components.')
              END IF
            ENDIF
          END IF
        END IF
      END IF
    END IF
  ELSE
    UnitHeat(UnitHeatNum)%MaxVolHotSteamFlow = 0.0d0
  END IF

  ! set the design air flow rate for the heating coil

  CALL SetCoilDesFlow(UnitHeat(UnitHeatNum)%HCoilTypeCh,UnitHeat(UnitHeatNum)%HCoilName,UnitHeat(UnitHeatNum)%MaxAirVolFlow,&
                       ErrorsFound)

  IF (ErrorsFound) THEN
    CALL ShowFatalError('Preceding sizing errors cause program termination')
  END IF

  RETURN
END SUBROUTINE SizeUnitHeater

SUBROUTINE CalcUnitHeater(UnitHeatNum,ZoneNum,FirstHVACIteration,PowerMet,LatOutputProvided)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Rick Strand
          !       DATE WRITTEN   May 2000
          !       MODIFIED       Don Shirey, Aug 2009 (LatOutputProvided)
          !                      July 2012, Chandan Sharma - FSEC: Added zone sys avail managers
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine mainly controls the action of the unit heater
          ! based on the user input for controls and the defined controls
          ! algorithms.  There are currently (at the initial creation of this
          ! subroutine) two control methods: on-off fan operation or continuous
          ! fan operation.

          ! METHODOLOGY EMPLOYED:
          ! Unit is controlled based on user input and what is happening in the
          ! simulation.  There are various cases to consider:
          ! 1. OFF: Unit is schedule off.  All flow rates are set to zero and
          !    the temperatures are set to zone conditions.
          ! 2. NO LOAD OR COOLING/ON-OFF FAN CONTROL: Unit is available, but
          !    there is no heating load.  All flow rates are set to zero and
          !    the temperatures are set to zone conditions.
          ! 3. NO LOAD OR COOLING/CONTINUOUS FAN CONTROL: Unit is available and
          !    the fan is running (if it is scheduled to be available also).
          !    No heating is provided, only circulation via the fan running.
          ! 4. HEATING: The unit is on/available and there is a heating load.
          !    The heating coil is modulated (constant fan speed) to meet the
          !    heating load.

          ! REFERENCES:
          ! ASHRAE Systems and Equipment Handbook (SI), 1996. page 31.7

          ! USE STATEMENTS:
  USE DataZoneEnergyDemands
  USE DataInterfaces, ONLY: ControlCompOutput
  USE DataHVACGlobals,    ONLY: ZoneCompTurnFansOn, ZoneCompTurnFansOff
  USE DataZoneEquipment,   ONLY: UnitHeater_Num
  USE PlantUtilities, ONLY: SetComponentFlowRate

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(INOUT) :: UnitHeatNum        ! number of the current fan coil unit being simulated
  INTEGER, INTENT(IN)    :: ZoneNum            ! number of zone being served
  LOGICAL, INTENT(IN)    :: FirstHVACIteration ! TRUE if 1st HVAC simulation of system timestep
  REAL(r64),    INTENT(OUT)   :: PowerMet        ! Sensible power supplied (W)
  REAL(r64), INTENT (OUT)  :: LatOutputProvided  ! Latent power supplied (kg/s), negative = dehumidification

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! see use DataInterfaces

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: ControlNode      ! the hot water inlet node
  REAL(r64)    :: ControlOffset    ! tolerance for output control
  INTEGER :: InletNode        ! unit air inlet node
  REAL(r64)    :: MaxWaterFlow     ! maximum water flow for heating or cooling [kg/sec]
  REAL(r64)    :: MinWaterFlow     ! minimum water flow for heating or cooling [kg/sec]
  INTEGER :: OutletNode       ! unit air outlet node
  REAL(r64)    :: QUnitOut         ! heating or sens. cooling provided by fan coil unit [watts]
  REAL(r64)    :: LatentOutput   ! Latent (moisture) add/removal rate, negative is dehumidification [kg/s]
  REAL(r64)    :: SpecHumOut     ! Specific humidity ratio of outlet air (kg moisture / kg moist air)
  REAL(r64)    :: SpecHumIn      ! Specific humidity ratio of inlet air (kg moisture / kg moist air)
  REAL(r64)    :: mdot  ! local temporary for fluid mass flow rate
          ! FLOW:
  FanElecPower = 0.0d0
          ! initialize local variables
  QUnitOut      = 0.0d0
  LatentOutput  = 0.0d0
  MaxWaterFlow  = 0.0d0
  MinWaterFlow  = 0.0d0
  InletNode     = UnitHeat(UnitHeatNum)%AirInNode
  OutletNode    = UnitHeat(UnitHeatNum)%AirOutNode
  ControlNode   = UnitHeat(UnitHeatNum)%HotControlNode
  ControlOffset = UnitHeat(UnitHeatNum)%HotControlOffset

  QZnReq = ZoneSysEnergyDemand(ZoneNum)%RemainingOutputRequired ! zone load needed

  IF (GetCurrentScheduleValue(UnitHeat(UnitHeatNum)%SchedPtr) <= 0 .OR. &
       ((GetCurrentScheduleValue(UnitHeat(UnitHeatNum)%FanAvailSchedPtr) <= 0 &
        .AND. .NOT. ZoneCompTurnFansOn) .OR. ZoneCompTurnFansOff)) THEN
          ! Case 1: OFF-->unit schedule says that it it not available
          !         OR child fan in not available OR child fan not being cycled ON by sys avail manager
          !         OR child fan being forced OFF by sys avail manager
    HCoilOn                               = .FALSE.
    IF (UnitHeat(UnitHeatNum)%HCoilType == WaterCoil) THEN
      mdot = 0.d0 ! try to turn off

      CALL SetComponentFlowRate( mdot, &
                                 UnitHeat(UnitHeatNum)%HotControlNode, &
                                 UnitHeat(UnitHeatNum)%HotCoilOutNodeNum, &
                                 UnitHeat(UnitHeatNum)%HWLoopNum, &
                                 UnitHeat(UnitHeatNum)%HWLoopSide, &
                                 UnitHeat(UnitHeatNum)%HWBranchNum, &
                                 UnitHeat(UnitHeatNum)%HWCompNum )
    END IF
    IF (UnitHeat(UnitHeatNum)%HCoilType == SteamCoil) THEN
      mdot = 0.d0 ! try to turn off

      CALL SetComponentFlowRate( mdot, &
                                 UnitHeat(UnitHeatNum)%HotControlNode, &
                                 UnitHeat(UnitHeatNum)%HotCoilOutNodeNum, &
                                 UnitHeat(UnitHeatNum)%HWLoopNum, &
                                 UnitHeat(UnitHeatNum)%HWLoopSide, &
                                 UnitHeat(UnitHeatNum)%HWBranchNum, &
                                 UnitHeat(UnitHeatNum)%HWCompNum )
    END IF
    CALL CalcUnitHeaterComponents(UnitHeatNum,FirstHVACIteration,QUnitOut)

  ELSE IF ( (QZnReq < SmallLoad) .OR. (CurDeadBandOrSetback(ZoneNum)) ) THEN
          ! Unit is available, but there is no load on it or we are in setback/deadband
    SELECT CASE (UnitHeat(UnitHeatNum)%FanControlType)

    CASE (OnOffCtrl)
          ! Case 2: NO LOAD OR COOLING/ON-OFF FAN CONTROL-->turn everything off
          !         because there is no load on the unit heater
      HCoilOn                               = .FALSE.
      IF (UnitHeat(UnitHeatNum)%HCoilType == WaterCoil) THEN
        mdot = 0.d0 ! try to turn off

        CALL SetComponentFlowRate( mdot, &
                                   UnitHeat(UnitHeatNum)%HotControlNode, &
                                   UnitHeat(UnitHeatNum)%HotCoilOutNodeNum, &
                                   UnitHeat(UnitHeatNum)%HWLoopNum, &
                                   UnitHeat(UnitHeatNum)%HWLoopSide, &
                                   UnitHeat(UnitHeatNum)%HWBranchNum, &
                                   UnitHeat(UnitHeatNum)%HWCompNum )
      END IF
      IF (UnitHeat(UnitHeatNum)%HCoilType == SteamCoil) THEN
        mdot = 0.d0 ! try to turn off

        CALL SetComponentFlowRate( mdot, &
                                   UnitHeat(UnitHeatNum)%HotControlNode, &
                                   UnitHeat(UnitHeatNum)%HotCoilOutNodeNum, &
                                   UnitHeat(UnitHeatNum)%HWLoopNum, &
                                   UnitHeat(UnitHeatNum)%HWLoopSide, &
                                   UnitHeat(UnitHeatNum)%HWBranchNum, &
                                   UnitHeat(UnitHeatNum)%HWCompNum )
      END IF
      CALL CalcUnitHeaterComponents(UnitHeatNum,FirstHVACIteration,QUnitOut)

    CASE (ContinuousCtrl)
          ! Case 3: NO LOAD OR COOLING/CONTINUOUS FAN CONTROL-->let the fan
          !         continue to run even though there is no load (air circulation)
          ! Note that the flow rates were already set in the initialization routine
          ! so there is really nothing else left to do except call the components.

      HCoilOn = .FALSE.
      IF (UnitHeat(UnitHeatNum)%HCoilType == WaterCoil) THEN
        mdot = 0.d0 ! try to turn off

        IF (UnitHeat(UnitHeatNum)%HWLoopNum > 0) THEN
          CALL SetComponentFlowRate( mdot, &
                                   UnitHeat(UnitHeatNum)%HotControlNode, &
                                   UnitHeat(UnitHeatNum)%HotCoilOutNodeNum, &
                                   UnitHeat(UnitHeatNum)%HWLoopNum, &
                                   UnitHeat(UnitHeatNum)%HWLoopSide, &
                                   UnitHeat(UnitHeatNum)%HWBranchNum, &
                                   UnitHeat(UnitHeatNum)%HWCompNum )
        ENDIF
      END IF
      IF (UnitHeat(UnitHeatNum)%HCoilType == SteamCoil) THEN
        mdot = 0.d0 ! try to turn off
        IF (UnitHeat(UnitHeatNum)%HWLoopNum > 0) THEN
          CALL SetComponentFlowRate( mdot, &
                                   UnitHeat(UnitHeatNum)%HotControlNode, &
                                   UnitHeat(UnitHeatNum)%HotCoilOutNodeNum, &
                                   UnitHeat(UnitHeatNum)%HWLoopNum, &
                                   UnitHeat(UnitHeatNum)%HWLoopSide, &
                                   UnitHeat(UnitHeatNum)%HWBranchNum, &
                                   UnitHeat(UnitHeatNum)%HWCompNum )
        ENDIF
      END IF

      CALL CalcUnitHeaterComponents(UnitHeatNum,FirstHVACIteration,QUnitOut)

    END SELECT

  ELSE    ! Case 4: HEATING-->unit is available and there is a heating load

    SELECT CASE (UnitHeat(UnitHeatNum)%HCoilType)

      CASE (WaterCoil)

!On the first HVAC iteration the system values are given to the controller, but after that
! the demand limits are in place and there needs to be feedback to the Zone Equipment
        If(FirstHVACIteration) Then
           MaxWaterFlow = UnitHeat(UnitHeatNum)%MaxHotWaterFlow
           MinWaterFlow = UnitHeat(UnitHeatNum)%MinHotWaterFlow
        Else
           MaxWaterFlow = Node(ControlNode)%MassFlowRateMaxAvail
           MinWaterFlow = Node(ControlNode)%MassFlowRateMinAvail
        End If
          ! control water flow to obtain output matching QZnReq
        CALL ControlCompOutput(CompName=UnitHeat(UnitHeatNum)%Name,CompType=cMO_UnitHeater,CompNum=UnitHeatNum, &
                               FirstHVACIteration=FirstHVACIteration,QZnReq=QZnReq, &
                               ActuatedNode=ControlNode,MaxFlow=MaxWaterFlow, &
                               MinFlow=MinWaterFlow,ControlOffSet=ControlOffset, &
                               ControlCompTypeNum=UnitHeat(UnitHeatNum)%ControlCompTypeNum, &
                               CompErrIndex=UnitHeat(UnitHeatNum)%CompErrIndex,  &
                               LoopNum     = UnitHeat(UnitHeatNum)%HWLoopNum,         &
                               LoopSide    = UnitHeat(UnitHeatNum)%HWLoopSide,        &
                               BranchIndex = UnitHeat(UnitHeatNum)%HWBranchNum)


      CASE (ElectricCoil,GasCoil,SteamCoil)

        HCoilOn = .TRUE.
        CALL CalcUnitHeaterComponents(UnitHeatNum,FirstHVACIteration,QUnitOut)

    END SELECT

  END IF    ! ...end of unit ON/OFF IF-THEN block

! CR9155 Remove specific humidity calculations
  SpecHumOut = Node(OutletNode)%HumRat
  SpecHumIn  = Node(InletNode)%HumRat
  LatentOutput = Node(OutletNode)%MassFlowRate * (SpecHumOut - SpecHumIn) ! Latent rate (kg/s), dehumid = negative

  QUnitOut = Node(OutletNode)%MassFlowRate * (PsyHFnTdbW(Node(OutletNode)%Temp,Node(InletNode)%HumRat)  &
                                            - PsyHFnTdbW(Node(InletNode)%Temp,Node(InletNode)%HumRat))

          ! Report variables...
  UnitHeat(UnitHeatNum)%HeatPower = MAX(0.0d0,QUnitOut)
  UnitHeat(UnitHeatNum)%ElecPower = FanElecPower

  PowerMet = QUnitOut
  LatOutputProvided = LatentOutput

  RETURN

END SUBROUTINE CalcUnitHeater

SUBROUTINE CalcUnitHeaterComponents(UnitHeatNum,FirstHVACIteration,LoadMet)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Rick Strand
          !       DATE WRITTEN   May 2000
          !       MODIFIED       July 2012, Chandan Sharma - FSEC: Added zone sys avail managers
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine launches the individual component simulations.
          ! This is called either when the unit is off to carry null conditions
          ! through the unit or during control iterations to continue updating
          ! what is going on within the unit.

          ! METHODOLOGY EMPLOYED:
          ! Simply calls the different components in order.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE Fans,           ONLY : SimulateFanComponents
  USE HeatingCoils,   ONLY : SimulateHeatingCoilComponents
  USE WaterCoils,     ONLY : SimulateWaterCoilComponents
  USE SteamCoils,     ONLY : SimulateSteamCoilComponents
  USE DataZoneEquipment,   ONLY: UnitHeater_Num
  USE DataHVACGlobals,     ONLY: ZoneCompTurnFansOn, ZoneCompTurnFansOff

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)  :: UnitHeatNum        ! Unit index in unit heater array
  LOGICAL, INTENT(IN)  :: FirstHVACIteration ! flag for 1st HVAV iteration in the time step
  REAL(r64),    INTENT(OUT) :: LoadMet            ! load met by unit (watts)

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64)      :: AirMassFlow     ! total mass flow through the unit
  REAL(r64)      :: CpAirZn         ! specific heat of dry air at zone conditions (zone conditions same as unit inlet)
  INTEGER        :: HCoilInAirNode  ! inlet node number for fan exit/coil inlet
  INTEGER        :: InletNode       ! unit air inlet node
  INTEGER        :: OutletNode      ! unit air outlet node
  REAL(r64)      :: QCoilReq        ! Heat addition required from an electric/gas heating coil
          ! FLOW:

  CALL SimulateFanComponents(UnitHeat(UnitHeatNum)%FanName,FirstHVACIteration,UnitHeat(UnitHeatNum)%Fan_Index, &
                             ZoneCompTurnFansOn = ZoneCompTurnFansOn,ZoneCompTurnFansOff = ZoneCompTurnFansOff)

  SELECT CASE (UnitHeat(UnitHeatNum)%HCoilType)

    CASE (WaterCoil)

      CALL SimulateWaterCoilComponents(UnitHeat(UnitHeatNum)%HCoilName,FirstHVACIteration, &
                                       UnitHeat(UnitHeatNum)%HCoil_Index)
    CASE (SteamCoil)

      IF (.NOT.HCoilOn) THEN
        QCoilReq = 0.0d0
      ELSE
        HCoilInAirNode = UnitHeat(UnitHeatNum)%FanOutletNode
        CpAirZn        = PsyCpAirFnWTdb(Node(UnitHeat(UnitHeatNum)%AirInNode)%HumRat,Node(UnitHeat(UnitHeatNum)%AirInNode)%Temp)
        QCoilReq       = QZnReq - Node(HCoilInAirNode)%MassFlowRate * CpAirZn &
                                  *(Node(HCoilInAirNode)%Temp-Node(UnitHeat(UnitHeatNum)%AirInNode)%Temp)
      END IF
      IF (QCoilReq < 0.0d0) QCoilReq = 0.0d0    ! a heating coil can only heat, not cool
      CALL SimulateSteamCoilComponents(CompName=UnitHeat(UnitHeatNum)%HCoilName, &
                                         FirstHVACIteration=FirstHVACIteration,    &
                                         QCoilReq=QCoilReq,                        &
                                         CompIndex=UnitHeat(UnitHeatNum)%HCoil_Index)

    CASE (ElectricCoil,GasCoil)

      IF (.NOT.HCoilOn) THEN
        QCoilReq = 0.0d0
      ELSE
        HCoilInAirNode = UnitHeat(UnitHeatNum)%FanOutletNode
        CpAirZn        = PsyCpAirFnWTdb(Node(UnitHeat(UnitHeatNum)%AirInNode)%HumRat,Node(UnitHeat(UnitHeatNum)%AirInNode)%Temp)
        QCoilReq       = QZnReq - Node(HCoilInAirNode)%MassFlowRate * CpAirZn &
                                  *(Node(HCoilInAirNode)%Temp-Node(UnitHeat(UnitHeatNum)%AirInNode)%Temp)
      END IF
      IF (QCoilReq < 0.0d0) QCoilReq = 0.0d0    ! a heating coil can only heat, not cool
      CALL SimulateHeatingCoilComponents(CompName=UnitHeat(UnitHeatNum)%HCoilName, &
                                         FirstHVACIteration=FirstHVACIteration,    &
                                         QCoilReq=QCoilReq,                        &
                                         CompIndex=UnitHeat(UnitHeatNum)%HCoil_Index)

  END SELECT

  InletNode   = UnitHeat(UnitHeatNum)%AirInNode
  OutletNode  = UnitHeat(UnitHeatNum)%AirOutNode
  AirMassFlow = Node(OutletNode)%MassFlowRate

  Node(InletNode)%MassFlowRate = Node(OutletNode)%MassFlowRate  ! maintain continuity through unit heater

  LoadMet = AirMassFlow * (PsyHFnTdbW(Node(OutletNode)%Temp,Node(InletNode)%HumRat)  &
                         - PsyHFnTdbW(Node(InletNode)%Temp,Node(InletNode)%HumRat))

  RETURN

END SUBROUTINE CalcUnitHeaterComponents

!SUBROUTINE UpdateUnitHeater
!
! No update routine needed in this module since all of the updates happen on
! the Node derived type directly and these updates are done by other routines.
!
!END SUBROUTINE UpdateUnitHeater

SUBROUTINE ReportUnitHeater(UnitHeatNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Rick Strand
          !       DATE WRITTEN   May 2000
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine needs a description.

          ! METHODOLOGY EMPLOYED:
          ! Needs description, as appropriate.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataGlobals,     ONLY: SecInHour
  USE DataHVACGlobals, ONLY: TimeStepSys

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: UnitHeatNum    ! Unit index in unit heater array

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          ! na

          ! FLOW:
  UnitHeat(UnitHeatNum)%HeatEnergy = UnitHeat(UnitHeatNum)%HeatPower * TimeStepSys * SecInHour
  UnitHeat(UnitHeatNum)%ElecEnergy = UnitHeat(UnitHeatNum)%ElecPower * TimeStepSys * SecInHour

  RETURN

END SUBROUTINE ReportUnitHeater

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

END MODULE UnitHeater
