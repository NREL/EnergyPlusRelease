MODULE UnitVentilator

  ! Module containing the routines dealing with the Unit Ventilator

  ! MODULE INFORMATION:
  !       AUTHOR         Rick Strand
  !       DATE WRITTEN   May 2000
  !       MODIFIED       March 2001   (addition of gas and electric coils)
  !                      October 2003 (addition of cooling coil type)
  !       RE-ENGINEERED  na

  ! PURPOSE OF THIS MODULE:
  ! To simulate unit ventilators.

  ! METHODOLOGY EMPLOYED:
  ! Units are modeled as a collection of components: outside air mixer,
  ! fan, heating coil and/or cooling coil plus an integrated control
  ! algorithm that adjusts the hot or cold water flow to meet the zone
  ! load.  Outside air mixing is handled locally as either fixed percent
  ! or as attempting to meet a prescribed mixed air temperature.

  ! REFERENCES:
  ! ASHRAE Systems and Equipment Handbook (SI), 1996. pp. 31.1-31.3
  ! Fred Buhl's fan coil module (FanCoilUnits.f90)

  ! OTHER NOTES: none

  ! USE STATEMENTS:
  ! Use statements for data only modules
USE DataPrecisionGlobals
USE DataLoopNode
USE DataGlobals,     ONLY: BeginEnvrnFlag, BeginDayFlag, MaxNameLength, InitConvTemp, SysSizingCalc, DisplayExtraWarnings
USE DataInterfaces
USE DataHVACGlobals, ONLY: SmallMassFlow, SmallLoad, FanElecPower, SmallAirVolFlow, ContFanCycCoil

  ! Use statements for access to subroutines in other modules
USE ScheduleManager
Use Psychrometrics
Use FluidProperties

IMPLICIT NONE         ! Enforce explicit typing of all variables

PRIVATE ! Everything private unless explicitly made public

  ! MODULE PARAMETER DEFINITIONS

  ! Currrent Module Unit type
  CHARACTER(len=*), PARAMETER :: cMO_UnitVentilator='ZoneHVAC:UnitVentilator'

  ! Parameters for outside air control types:
  INTEGER, PARAMETER :: Heating_ElectricCoilType     = 1
  INTEGER, PARAMETER :: Heating_GasCoilType          = 2
  INTEGER, PARAMETER :: Heating_WaterCoilType        = 3
  INTEGER, PARAMETER :: Heating_SteamCoilType        = 4
  INTEGER, PARAMETER :: Cooling_CoilWaterCooling     = 1
  INTEGER, PARAMETER :: Cooling_CoilDetailedCooling  = 2
  INTEGER, PARAMETER :: Cooling_CoilHXAssisted       = 3
  ! OA operation modes
  INTEGER, PARAMETER :: VariablePercent  = 1
  INTEGER, PARAMETER :: FixedTemperature = 2
  INTEGER, PARAMETER :: FixedOAControl   = 3
  ! coil operation
  INTEGER, PARAMETER :: On =  1              ! normal coil operation
  INTEGER, PARAMETER :: Off = 0              ! signal coil shouldn't run
  INTEGER, PARAMETER :: NoneOption             = 0
  INTEGER, PARAMETER :: BothOption             = 1
  INTEGER, PARAMETER :: HeatingOption          = 2
  INTEGER, PARAMETER :: CoolingOption          = 3

  ! DERIVED TYPE DEFINITIONS
TYPE UnitVentilatorData
  ! Input data
  CHARACTER(len=MaxNameLength) :: Name                =' ' ! name of unit
  CHARACTER(len=MaxNameLength) :: SchedName           =' ' ! availability schedule
  INTEGER                      :: SchedPtr            =0   ! index to schedule
  INTEGER                      :: AirInNode           =0   ! inlet air node number
  INTEGER                      :: AirOutNode          =0   ! outlet air node number
  INTEGER                      :: FanOutletNode       =0   ! outlet node number for fan exit
                                                           ! (assumes fan is upstream of heating coil)
  INTEGER                      :: FanType_Num         =0   ! Fan type number (see DataHVACGlobals)
  CHARACTER(len=MaxNameLength) :: FanType             =' ' ! type of fan
  CHARACTER(len=MaxNameLength) :: FanName             =' ' ! name of fan
  INTEGER                      :: Fan_Index           =0
  INTEGER                      :: FanAvailSchedPtr    =0   ! index to fan availability schedule
  INTEGER                      :: ControlCompTypeNum =0
  INTEGER                      :: CompErrIndex       =0
  REAL(r64)                    :: MaxAirVolFlow       =0.0d0 ! m3/s
  REAL(r64)                    :: MaxAirMassFlow      =0.0d0 ! kg/s
  INTEGER                      :: OAControlType       =0   ! type of control; options are VARIABLE PERCENT and FIXED TEMPERATURE
  CHARACTER(len=MaxNameLength) :: MinOASchedName      =' ' ! schedule of fraction for minimum outside air (all controls)
  INTEGER                      :: MinOASchedPtr       =0   ! index to schedule
  CHARACTER(len=MaxNameLength) :: MaxOASchedName      =' ' ! schedule of percentages for maximum outside air fraction (variable %)
  INTEGER                      :: MaxOASchedPtr       =0   ! index to schedule
  CHARACTER(len=MaxNameLength) :: TempSchedName       =' ' ! schedule of temperatures for desired "mixed air"
                                                           ! temperature (fixed temp.)
  INTEGER                      :: TempSchedPtr        =0   ! index to schedule
  INTEGER                      :: OutsideAirNode      =0   ! outside air node number
  INTEGER                      :: AirReliefNode       =0   ! relief air node number
  INTEGER                      :: OAMixerOutNode      =0   ! outlet node after the outside air mixer (inlet to coils if present)
  REAL(r64)                    :: OutAirVolFlow       =0.0d0 ! m3/s
  REAL(r64)                    :: OutAirMassFlow      =0.0d0 ! kg/s
  REAL(r64)                    :: MinOutAirVolFlow    =0.0d0 ! m3/s
  REAL(r64)                    :: MinOutAirMassFlow   =0.0d0 ! kg/s
  INTEGER                      :: CoilOption          =0 ! type of coil option; options are BOTH, HEATING, COOLING, AND NONE
  LOGICAL                      :: HCoilPresent        =.FALSE. ! .TRUE. if unit ventilator has a heating coil
  INTEGER                      :: HCoilType           =0   ! type of heating coil (water, gas, electric, etc.)
  CHARACTER(len=MaxNameLength) :: HCoilName           =' ' ! name of heating coil
  CHARACTER(len=MaxNameLength) :: HCoilTypeCh         =' ' ! type of heating coil character string (same as type on idf file).
  INTEGER                      :: HCoil_Index         =0
  INTEGER                      :: HCoil_PlantTypeNum  =0  !
  INTEGER                      :: HCoil_FluidIndex    =0
  CHARACTER(len=MaxNameLength) :: HCoilSchedName      =' ' ! availability schedule for the heating coil
  INTEGER                      :: HCoilSchedPtr       =0   ! index to schedule
  REAL(r64)                    :: HCoilSchedValue     =0.0d0
  REAL(r64)                    :: MaxVolHotWaterFlow  =0.0d0 ! m3/s
  REAL(r64)                    :: MaxVolHotSteamFlow  =0.0d0 ! m3/s
  REAL(r64)                    :: MaxHotWaterFlow     =0.0d0 ! kg/s
  REAL(r64)                    :: MaxHotSteamFlow=0.0d0
  REAL(r64)                    :: MinHotSteamFlow =0.0d0
  REAL(r64)                    :: MinVolHotWaterFlow  =0.0d0 ! m3/s
  REAL(r64)                    :: MinVolHotSteamFlow  =0.0d0 ! m3/s
  REAL(r64)                    :: MinHotWaterFlow     =0.0d0 ! kg/s
  INTEGER                      :: HotControlNode      =0   ! hot water control node
  INTEGER                      :: HotCoilOutNodeNum   =0   ! outlet of coil
  REAL(r64)                    :: HotControlOffset    =0.0d0 ! control tolerance
  INTEGER                      :: HWLoopNum           =0   ! index for plant loop with hot water coil
  INTEGER                      :: HWLoopSide          =0   ! index for plant loop side for hot water coil
  INTEGER                      :: HWBranchNum         =0   ! index for plant branch for hot water coil
  INTEGER                      :: HWCompNum           =0   ! index for plant component for hot water coil

  LOGICAL                      :: CCoilPresent        =.FALSE. ! .TRUE. if unit ventilator has a cooling coil
  CHARACTER(len=MaxNameLength) :: CCoilName           =' ' ! name of cooling coil
  CHARACTER(len=MaxNameLength) :: CCoilTypeCh         =' ' ! type of cooling coil as character string (same as on idf file)
  INTEGER                      :: CCoil_Index         =0
  CHARACTER(len=MaxNameLength) :: CCoilPlantName      =' ' ! name of cooling coil for plant
  CHARACTER(len=MaxNameLength) :: CCoilPlantType      =' ' ! type of cooling coil for plant
  INTEGER                      :: CCoil_PlantTypeNum  =0  !
  INTEGER                      :: CCoilType           =0   ! type of cooling coil:
                                                           ! 'Coil:Cooling:Water:DetailedGeometry' or
                                                           ! 'CoilSystem:Cooling:Water:HeatExchangerAssisted'
  CHARACTER(len=MaxNameLength) :: CCoilSchedName      =' ' ! availability schedule for the cooling coil
  INTEGER                      :: CCoilSchedPtr       =0   ! index to schedule
  REAL(r64)                    :: CCoilSchedValue     =0.0d0
  REAL(r64)                    :: MaxVolColdWaterFlow =0.0d0 ! m3/s
  REAL(r64)                    :: MaxColdWaterFlow    =0.0d0 ! kg/s
  REAL(r64)                    :: MinVolColdWaterFlow =0.0d0 ! m3/s
  REAL(r64)                    :: MinColdWaterFlow    =0.0d0 ! kg/s
  INTEGER                      :: ColdControlNode     =0   ! chilled water control node
  INTEGER                      :: ColdCoilOutNodeNum  =0   ! chilled water coil out node
  REAL(r64)                    :: ColdControlOffset   =0.0d0 ! control tolerance
  INTEGER                      :: CWLoopNum           =0   ! index for plant loop with chilled water coil
  INTEGER                      :: CWLoopSide          =0   ! index for plant loop side for chilled water coil
  INTEGER                      :: CWBranchNum         =0   ! index for plant branch for chilled water coil
  INTEGER                      :: CWCompNum           =0   ! index for plant component for chilled water coil


  ! Report data
  REAL(r64)                    :: HeatPower           =0.0d0 ! unit heating output in watts
  REAL(r64)                    :: HeatEnergy          =0.0d0 ! unit heating output in J
  REAL(r64)                    :: TotCoolPower        =0.0d0
  REAL(r64)                    :: TotCoolEnergy       =0.0d0
  REAL(r64)                    :: SensCoolPower       =0.0d0
  REAL(r64)                    :: SensCoolEnergy      =0.0d0
  REAL(r64)                    :: ElecPower           =0.0d0
  REAL(r64)                    :: ElecEnergy          =0.0d0
  CHARACTER(len=MaxNameLength) :: AvailManagerListName = ' ' ! Name of an availability manager list object
  INTEGER                      :: AvailStatus          = 0
                                                                  ! for unit ventilator object
END TYPE UnitVentilatorData

TYPE (UnitVentilatorData), ALLOCATABLE, DIMENSION(:) :: UnitVent

  ! MODULE VARIABLE DECLARATIONS:
LOGICAL :: HCoilOn         =.FALSE. ! TRUE if the heating coil (gas or electric especially) should be running
INTEGER :: NumOfUnitVents  =0       ! Number of unit ventilators in the input file
REAL(r64)    :: OAMassFlowRate  =0.0d0     ! Outside air mass flow rate for the unit ventilator
REAL(r64)    :: QZnReq          =0.0d0     ! heating or cooling needed by zone [watts]
LOGICAL, ALLOCATABLE, DIMENSION(:) :: MySizeFlag
LOGICAL,SAVE :: GetUnitVentilatorInputFlag = .TRUE.  ! First time, input is "gotten"
LOGICAL, ALLOCATABLE, DIMENSION(:) :: CheckEquipName

  ! SUBROUTINE SPECIFICATIONS FOR MODULE UnitVentilator
PUBLIC  SimUnitVentilator
PRIVATE GetUnitVentilatorInput
PRIVATE InitUnitVentilator
PRIVATE SizeUnitVentilator
PRIVATE CalcUnitVentilator
PUBLIC  CalcUnitVentilatorComponents
PRIVATE SimUnitVentOAMixer
!PRIVATE UpdateUnitVentilator
PRIVATE ReportUnitVentilator

PUBLIC  GetUnitVentilatorOutAirNode
PUBLIC  GetUnitVentilatorReturnAirNode
PUBLIC  GetUnitVentilatorMixedAirNode
PUBLIC  GetUnitVentilatorZoneInletAirNode

CONTAINS

SUBROUTINE SimUnitVentilator(CompName,ZoneNum,FirstHVACIteration,PowerMet,LatOutputProvided,CompIndex)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Rick Strand
          !       DATE WRITTEN   May 2000
          !       MODIFIED       Don Shirey, Aug 2009 (LatOutputProvided)
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This is the main driver subroutine for the Unit Ventilator simulation.

          ! METHODOLOGY EMPLOYED:
          ! Standard EnergyPlus methodology.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor,      ONLY: FindItemInList
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
  INTEGER      :: UnitVentNum            ! index of unit ventilator being simulated

          ! FLOW:
  IF (GetUnitVentilatorInputFlag) THEN
    CALL GetUnitVentilatorInput
    GetUnitVentilatorInputFlag=.FALSE.
  ENDIF

  ! Find the correct Unit Ventilator Equipment
  IF (CompIndex == 0) THEN
    UnitVentNum = FindItemInList(CompName,UnitVent%Name,NumOfUnitVents)
    IF (UnitVentNum == 0) THEN
      CALL ShowFatalError('SimUnitVentilator: Unit not found='//TRIM(CompName))
    ENDIF
    CompIndex=UnitVentNum
  ELSE
    UnitVentNum=CompIndex
    IF (UnitVentNum > NumOfUnitVents .or. UnitVentNum < 1) THEN
      CALL ShowFatalError('SimUnitVentilator:  Invalid CompIndex passed='//  &
                          TRIM(TrimSigDigits(UnitVentNum))// &
                          ', Number of Units='//TRIM(TrimSigDigits(NumOfUnitVents))//  &
                          ', Entered Unit name='//TRIM(CompName))
    ENDIF
    IF (CheckEquipName(UnitVentNum)) THEN
      IF (CompName /= UnitVent(UnitVentNum)%Name) THEN
        CALL ShowFatalError('SimUnitVentilator: Invalid CompIndex passed='//  &
                            TRIM(TrimSigDigits(UnitVentNum))// &
                            ', Unit name='//TRIM(CompName)//', stored Unit Name for that index='//  &
                            TRIM(UnitVent(UnitVentNum)%Name))
      ENDIF
      CheckEquipName(UnitVentNum)=.false.
    ENDIF
  ENDIF

  CALL InitUnitVentilator(UnitVentNum,FirstHVACIteration,ZoneNum)

  CALL CalcUnitVentilator(UnitVentNum,ZoneNum,FirstHVACIteration,PowerMet,LatOutputProvided)

!  CALL UpdateUnitVentilator

  CALL ReportUnitVentilator(UnitVentNum)

  RETURN

END SUBROUTINE SimUnitVentilator

SUBROUTINE GetUnitVentilatorInput

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Rick Strand
          !       DATE WRITTEN   May 2000
          !       MODIFIED       Chandan Sharma, FSEC, March 2011: Added zone sys avail manager
          !                      Bereket Nigusse, FSEC, April 2011: eliminated input node names
          !                                                         & added fan object type
          !
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine obtains the input for unit ventilators and sets
          ! up the appropriate derived type.

          ! METHODOLOGY EMPLOYED:
          ! Standard EnergyPlus methodology.

          ! REFERENCES:
          ! Fred Buhl's fan coil module (FanCoilUnits.f90)

          ! USE STATEMENTS:
  USE InputProcessor,   ONLY: GetNumObjectsFound, GetObjectItem, VerifyName, SameString, GetObjectDefMaxArgs
  USE NodeInputManager, ONLY : GetOnlySingleNode
  USE BranchNodeConnections, ONLY: SetUpCompSets
  USE OutAirNodeManager, ONLY: CheckAndAddAirNodeNumber
  USE WaterCoils,   ONLY :GetWaterCoilMaxFlowRate=>GetCoilMaxWaterFlowRate, GetCoilWaterInletNode
  Use SteamCoils,   ONLY: GetSteamCoilMaxFlowRate=>GetCoilMaxWaterFlowRate, GetSteamCoilIndex,  &
                          GetSteamCoilSteamInletNode=>GetCoilSteamInletNode
  USE HVACHXAssistedCoolingCoil,  ONLY : GetHXAssistedCoilFlowRate=>GetCoilMaxWaterFlowRate, &
                          GetHXCoilWaterInletNode=>GetCoilWaterInletNode, GetHXCoilTypeAndName
  USE Fans,         ONLY: GetFanIndex, GetFanVolFlow, GetFanType, GetFanOutletNode, GetFanAvailSchPtr
  USE DataHVACGlobals, ONLY: FanType_SimpleConstVolume, FanType_SimpleVAV, ZoneComp

  USE DataSizing,   ONLY: AutoSize
  USE General,      ONLY: TrimSigDigits
  USE DataZoneEquipment,         ONLY: UnitVentilator_Num, ZoneEquipConfig
  USE DataGlobals,               ONLY: NumOfZones, ScheduleAlwaysOn
  USE DataPlant,    ONLY: TypeOf_CoilWaterCooling, TypeOf_CoilWaterDetailedFlatCooling, &
                          TypeOf_CoilWaterSimpleHeating, TypeOf_CoilSteamAirHeating

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER    :: RoutineName='GetUnitVentilatorInput: ' ! include trailing blank

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  LOGICAL                        :: ErrorsFound=.FALSE. ! Set to true if errors in input, fatal at end of routine
  INTEGER                        :: IOStatus            ! Used in GetObjectItem
  LOGICAL                        :: IsBlank             ! TRUE if the name is blank
  LOGICAL                        :: IsNotOk             ! TRUE if there was a problem with a list name
  INTEGER                        :: NumFields           ! Total number of fields in object
  INTEGER                        :: NumAlphas           ! Number of Alphas for each GetObjectItem call
  INTEGER                        :: NumNumbers          ! Number of Numbers for each GetObjectItem call
  INTEGER                        :: UnitVentNum         ! Item to be "gotten"
  LOGICAL                        :: IsValid             ! Set for outside air node check
  LOGICAL                        :: Errflag=.FALSE.     ! interim error flag
  CHARACTER(len=MaxNameLength)   :: cCoolingCoilType    ! Cooling coil object type
  CHARACTER(len=MaxNameLength)   :: cHeatingCoilType    ! Heating coil object type
  INTEGER                        :: FanIndex            ! index to fan used for flow checks
  REAL(r64)                      :: FanVolFlow          ! volumetric flow rate of fan
  CHARACTER(len=MaxNameLength)   :: CurrentModuleObject
  CHARACTER(len=MaxNameLength), &
                   ALLOCATABLE, DIMENSION(:) :: Alphas                       ! Alpha items for object
  REAL(r64), ALLOCATABLE, DIMENSION(:)       :: Numbers                      ! Numeric items for object
  CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:)  :: cAlphaFields   ! Alpha field names
  CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:)  :: cNumericFields ! Numeric field names
  LOGICAL, ALLOCATABLE, DIMENSION(:)  :: lAlphaBlanks                        ! Logical array, alpha field input BLANK = .true.
  LOGICAL, ALLOCATABLE, DIMENSION(:)  :: lNumericBlanks                      ! Logical array, numeric field input BLANK = .true.
  INTEGER                             :: CtrlZone    ! index to loop counter
  INTEGER                             :: NodeNum     ! index to loop counter
  LOGICAL                             :: ZoneNodeNotFound ! used in error checking

          ! FLOW:

          ! Figure out how many unit ventilators there are in the input file

  CurrentModuleObject = cMO_UnitVentilator
  NumOfUnitVents=GetNumObjectsFound(CurrentModuleObject)
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
  IF (NumOfUnitVents .GT. 0) THEN
    ALLOCATE(UnitVent(NumOfUnitVents))
    ALLOCATE(CheckEquipName(NumOfUnitVents))
  ENDIF
  CheckEquipName=.true.

  DO UnitVentNum = 1, NumOfUnitVents    ! Begin looping over all of the unit ventilators found in the input file...

    CALL GetObjectItem(CurrentModuleObject,UnitVentNum,Alphas,NumAlphas,Numbers,NumNumbers,IOStatus, &
                       NumBlank=lNumericBlanks,AlphaBlank=lAlphaBlanks, &
                       AlphaFieldNames=cAlphaFields,NumericFieldNames=cNumericFields)

    IsNotOK=.FALSE.
    IsBlank=.FALSE.
    CALL VerifyName(Alphas(1),UnitVent%Name,UnitVentNum-1,IsNotOK,IsBlank,TRIM(CurrentModuleObject)//' Name')
    IF (IsNotOK) THEN
      ErrorsFound=.TRUE.
      IF (IsBlank) Alphas(1)='xxxxx'
    END IF

    UnitVent(UnitVentNum)%Name      = Alphas(1)
    UnitVent(UnitVentNum)%SchedName = Alphas(2)
    IF (lAlphaBlanks(2)) THEN
      UnitVent(UnitVentNum)%SchedPtr  = ScheduleAlwaysOn
    ELSE
      UnitVent(UnitVentNum)%SchedPtr  = GetScheduleIndex(Alphas(2))  ! convert schedule name to pointer
      IF (UnitVent(UnitVentNum)%SchedPtr == 0) THEN
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'// TRIM(Alphas(1))//'", invalid')
        CALL ShowContinueError('not found: '//trim(cAlphaFields(2))//'="'//trim(Alphas(2))//'".')
        ErrorsFound=.TRUE.
      ENDIF
    END IF

    UnitVent(UnitVentNum)%MaxAirVolFlow = Numbers(1)

          ! Outside air information:
    UnitVent(UnitVentNum)%MinOutAirVolFlow  = Numbers(2)

    UnitVent(UnitVentNum)%MinOASchedName = Alphas(4)
    UnitVent(UnitVentNum)%MinOASchedPtr  = GetScheduleIndex(Alphas(4))  ! convert schedule name to pointer
    IF (UnitVent(UnitVentNum)%MinOASchedPtr == 0) THEN
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'// TRIM(UnitVent(UnitVentNum)%Name)//'", invalid')
      CALL ShowContinueError('not found: '//TRIM(cAlphaFields(4))//'="'//TRIM(Alphas(4))//'".')
      ErrorsFound=.TRUE.
    END IF

    UnitVent(UnitVentNum)%OutAirVolFlow  = Numbers(3)
    cCoolingCoilType=' '
    cHeatingCoilType=' '

    SELECT CASE (Alphas(3))
      CASE ('VARIABLEPERCENT')
        UnitVent(UnitVentNum)%OAControlType  = VariablePercent
        UnitVent(UnitVentNum)%MaxOASchedName = Alphas(5)
        UnitVent(UnitVentNum)%MaxOASchedPtr  = GetScheduleIndex(Alphas(5))  ! convert schedule name to pointer
        IF (UnitVent(UnitVentNum)%MaxOASchedPtr == 0) THEN
          CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'// TRIM(UnitVent(UnitVentNum)%Name)//'", invalid')
          CALL ShowContinueError('not found:'//TRIM(cAlphaFields(5))//'="'//TRIM(UnitVent(UnitVentNum)%MaxOASchedName)//'".')
          ErrorsFound=.TRUE.
        ELSEIF (.not. CheckScheduleValueMinMax(UnitVent(UnitVentNum)%MaxOASchedPtr,'>=0',0.0d0,'<=',1.0d0)) THEN
          CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'// TRIM(UnitVent(UnitVentNum)%Name)//'", invalid')
          CALL ShowContinueError('out of range [0,1]: '//TRIM(cAlphaFields(5))//'="'//  &
             TRIM(UnitVent(UnitVentNum)%MaxOASchedName)//'".')
          ErrorsFound=.TRUE.
        END IF
      CASE ('FIXEDAMOUNT')
        UnitVent(UnitVentNum)%OAControlType  = FixedOAControl
        UnitVent(UnitVentNum)%MaxOASchedName = Alphas(5)
        UnitVent(UnitVentNum)%MaxOASchedPtr  = GetScheduleIndex(Alphas(5))  ! convert schedule name to pointer
        IF (UnitVent(UnitVentNum)%MaxOASchedPtr == 0) THEN
          CALL ShowSevereError(TRIM(cAlphaFields(5))//' not found = '//TRIM(UnitVent(UnitVentNum)%MaxOASchedName))
          CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(UnitVent(UnitVentNum)%Name))
          ErrorsFound=.TRUE.
        ELSEIF (.not. CheckScheduleValueMinMax(UnitVent(UnitVentNum)%MaxOASchedPtr,'>=0',0.0d0)) THEN
          CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'// TRIM(UnitVent(UnitVentNum)%Name)//'", invalid')
          CALL ShowContinueError('out of range [0,1]: '//TRIM(cAlphaFields(5))//'="'//  &
             TRIM(UnitVent(UnitVentNum)%MaxOASchedName)//'".')
          ErrorsFound=.TRUE.
        END IF
      CASE ('FIXEDTEMPERATURE')
        UnitVent(UnitVentNum)%OAControlType  = FixedTemperature
        UnitVent(UnitVentNum)%TempSchedName = Alphas(5)
        UnitVent(UnitVentNum)%TempSchedPtr  = GetScheduleIndex(Alphas(5))  ! convert schedule name to pointer
        IF (UnitVent(UnitVentNum)%TempSchedPtr == 0) THEN
          CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'// TRIM(UnitVent(UnitVentNum)%Name)//'", invalid')
          CALL ShowContinueError(' not found: '//TRIM(cAlphaFields(5))//'="'//TRIM(UnitVent(UnitVentNum)%MaxOASchedName)//'".')
          ErrorsFound=.TRUE.
        END IF
      CASE DEFAULT
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'// TRIM(UnitVent(UnitVentNum)%Name)//'", invalid')
        CALL ShowContinueError('Illegal '//TRIM(cAlphaFields(3))//'="'//TRIM(Alphas(3))//'".')
      END SELECT

          ! Main air nodes (except outside air node):
          ! For node connections, this object is both a parent and a non-parent, because the
          ! OA mixing box is not called out as a separate component, its nodes must be connected
          ! as ObjectIsNotParent.  But for the fan and coils, the nodes are connected as ObjectIsParent
          ! To support the diagramming tool, the unit ventilator inlet node must appear both as
          ! an inlet to the unit ventilator parent object and as an inlet to the implied
          ! non-parent OA mixing box within the unit ventilator.
          ! Because there is overlap between the nodes that are parent and non-parent, use a different
          ! object type for the non parent nodes
    UnitVent(UnitVentNum)%AirInNode = &
               GetOnlySingleNode(Alphas(6),ErrorsFound,TRIM(CurrentModuleObject),Alphas(1), &
                            NodeType_Air,NodeConnectionType_Inlet,1,ObjectIsParent)
    UnitVent(UnitVentNum)%AirInNode = &
               GetOnlySingleNode(Alphas(6),ErrorsFound,TRIM(CurrentModuleObject)//'-OA MIXER',Alphas(1), &
                            NodeType_Air,NodeConnectionType_Inlet,1,ObjectIsNotParent)

    UnitVent(UnitVentNum)%AirOutNode = &
               GetOnlySingleNode(Alphas(7),ErrorsFound,TRIM(CurrentModuleObject),Alphas(1), &
                            NodeType_Air,NodeConnectionType_Outlet,1,ObjectIsParent)

          ! Fan information:
!   A11, \field Supply Air Fan Object Type
!        \required-field
!        \type choice
!        \key Fan:ConstantVolume
!        \key Fan:VariableVolume
!        \note Allowable fan types are Fan:ConstantVolume and
!        \note Fan:VariableVolume
!   A12, \field Fan Name
!        \required-field
!        \type object-list
!        \object-list FansCVandVAV

    UnitVent(UnitVentNum)%FanType       = Alphas(11)
    UnitVent(UnitVentNum)%FanName       = Alphas(12)
    ErrFlag = .FALSE.
    CALL ValidateComponent(UnitVent(UnitVentNum)%FanType,UnitVent(UnitVentNum)%FanName,ErrFlag,TRIM(CurrentModuleObject))
    IF (ErrFlag) THEN
      CALL ShowContinueError('specified in '//TRIM(CurrentModuleObject)//' = "'//TRIM(UnitVent(UnitVentNum)%Name)//'".')
      ErrorsFound=.TRUE.
    ELSE
      CALL GetFanType(UnitVent(UnitVentNum)%FanName,UnitVent(UnitVentNum)%FanType_Num, &
                    ErrFlag,CurrentModuleObject,UnitVent(UnitVentNum)%Name)

      SELECT CASE (UnitVent(UnitVentNum)%FanType_Num)
        CASE (FanType_SimpleConstVolume, FanType_SimpleVAV)

            ! Get fan outlet node
            UnitVent(UnitVentNum)%FanOutletNode = GetFanOutletNode(UnitVent(UnitVentNum)%FanType,&
                                                  UnitVent(UnitVentNum)%FanName,ErrFlag)
            IF(ErrFlag)THEN
              CALL ShowContinueError('specified in '//TRIM(CurrentModuleObject)//' = "' // TRIM(UnitVent(UnitVentNum)%Name)//'".')
              ErrorsFound = .TRUE.
            ELSE
              CALL GetFanIndex(UnitVent(UnitVentNum)%FanName,FanIndex,ErrFlag,TRIM(CurrentModuleObject))
              ! Other error checks should trap before it gets to this point in the code, but including just in case.

              CALL GetFanVolFlow(FanIndex,FanVolFlow)
              IF(FanVolFlow .NE. AutoSize .AND. UnitVent(UnitVentNum)%MaxAirVolFlow .NE. AutoSize .AND. &
                 FanVolFlow .LT. UnitVent(UnitVentNum)%MaxAirVolFlow)THEN
                CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(UnitVent(UnitVentNum)%Name)//'"')
                CALL ShowContinueError('...air flow rate ['//TRIM(TrimSigDigits(FanVolFlow,7))//'] in'// &
                ' fan object '//TRIM(UnitVent(UnitVentNum)%FanName)//' is less than the unit ventilator maximum supply air'// &
                ' flow rate ['//TRIM(TrimSigDigits(UnitVent(UnitVentNum)%MaxAirVolFlow,7))//'].')
                CALL ShowContinueError('...the fan flow rate must be greater than or equal to the unit ventilator maximum'// &
                                       ' supply air flow rate.')
                ErrorsFound = .TRUE.
              ELSE IF(FanVolFlow .EQ. AutoSize .AND. UnitVent(UnitVentNum)%MaxAirVolFlow .NE. AutoSize)THEN
                CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(UnitVent(UnitVentNum)%Name)//'"')
                CALL ShowContinueError('...the fan flow rate is autosized while the unit ventilator flow rate is not.')
                CALL ShowContinueError('...this can lead to unexpected results where the fan flow rate is less than required.')
              ELSE IF(FanVolFlow .NE. AutoSize .AND. UnitVent(UnitVentNum)%MaxAirVolFlow .EQ. AutoSize)THEN
                CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(UnitVent(UnitVentNum)%Name)//'"')
                CALL ShowContinueError('...the unit ventilator flow rate is autosized while the fan flow rate is not.')
                CALL ShowContinueError('...this can lead to unexpected results where the fan flow rate is less than required.')
              END IF
              ! Get the fan's availability schedule
              ErrFlag=.FALSE.
              UnitVent(UnitVentNum)%FanAvailSchedPtr = GetFanAvailSchPtr(UnitVent(UnitVentNum)%FanType, &
                                                               UnitVent(UnitVentNum)%FanName,ErrFlag)
               IF (ErrFlag) THEN
                 CALL ShowContinueError('...specified in '//TRIM(CurrentModuleObject)//'="'//TRIM(UnitVent(UnitVentNum)%Name)//'"')
                 ErrorsFound=.TRUE.
               ENDIF
            END IF
        CASE DEFAULT
            CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="' // TRIM(UnitVent(UnitVentNum)%Name)//'"')
            CALL ShowContinueError('Fan Type must be Fan:ConstantVolume or Fan:VariableVolume.')
            ErrorsFound = .TRUE.
      END SELECT
    ENDIF
          ! For node connections, this object is both a parent and a non-parent, because the
          ! OA mixing box is not called out as a separate component, its nodes must be connected
          ! as ObjectIsNotParent.  But for the fan and coils, the nodes are connected as ObjectIsParent
          ! Because there is overlap between the nodes that are parent and non-parent, use a different
          ! object type for the non parent nodes
    UnitVent(UnitVentNum)%OutsideAirNode = &
          !  Set connection type to 'OutdoorAir', because this is hardwired to OA conditions
               GetOnlySingleNode(Alphas(8),ErrorsFound,TRIM(CurrentModuleObject)//'-OA MIXER',Alphas(1), &
                            NodeType_Air,NodeConnectionType_OutsideAirReference,1,ObjectIsNotParent)
    IF ((.NOT. lAlphaBlanks(8))) THEN
      CALL CheckAndAddAirNodeNumber(UnitVent(UnitVentNum)%OutsideAirNode,IsValid)
      IF (.not. IsValid) THEN
        CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//', Adding '//TRIM(cAlphaFields(8))//'='//TRIM(Alphas(8)))
      ENDIF
    ENDIF

    UnitVent(UnitVentNum)%AirReliefNode = &
               GetOnlySingleNode(Alphas(9),ErrorsFound,TRIM(CurrentModuleObject)//'-OA MIXER',Alphas(1), &
                            NodeType_Air,NodeConnectionType_ReliefAir,1,ObjectIsNotParent)

    UnitVent(UnitVentNum)%OAMixerOutNode = &
               GetOnlySingleNode(Alphas(10),ErrorsFound,TRIM(CurrentModuleObject)//'-OA MIXER',Alphas(1), &
                            NodeType_Air,NodeConnectionType_Outlet,1,ObjectIsNotParent)

    IF (UnitVent(UnitVentNum)%OAControlType == FixedOAControl) THEN
      UnitVent(UnitVentNum)%OutAirVolFlow = UnitVent(UnitVentNum)%MinOutAirVolFlow
      UnitVent(UnitVentNum)%MaxOASchedName = UnitVent(UnitVentNum)%MinOASchedName
      UnitVent(UnitVentNum)%MaxOASchedPtr  = GetScheduleIndex(UnitVent(UnitVentNum)%MinOASchedName)
    END IF


   ! Add fan to component sets array
   CALL SetUpCompSets(TRIM(CurrentModuleObject), UnitVent(UnitVentNum)%Name,&
                      UnitVent(UnitVentNum)%FanType, UnitVent(UnitVentNum)%FanName, &
                      NodeID(UnitVent(UnitVentNum)%OAMixerOutNode), NodeID(UnitVent(UnitVentNum)%FanOutletNode))

   IF (.NOT. lAlphaBlanks(18)) THEN
     UnitVent(UnitVentNum)%AvailManagerListName = Alphas(18)
     ZoneComp(UnitVentilator_Num)%ZoneCompAvailMgrs(UnitVentNum)%AvailManagerListName  = Alphas(18)
   ENDIF

!   A13, \field Coil Option
!        \required-field
!        \type choice
!        \key None
!        \key Heating
!        \key Cooling
!        \key HeatingAndCooling

    SELECT CASE (Alphas(13))
      CASE ('HEATINGANDCOOLING')
        UnitVent(UnitVentNum)%CoilOption = BothOption
      CASE ('HEATING')
        UnitVent(UnitVentNum)%CoilOption = HeatingOption
      CASE ('COOLING')
        UnitVent(UnitVentNum)%CoilOption = CoolingOption
      CASE ('NONE')
        UnitVent(UnitVentNum)%CoilOption = NoneOption
      CASE DEFAULT
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(UnitVent(UnitVentNum)%Name)//'", invalid')
        CALL ShowContinueError('illegal value: '//TRIM(cAlphaFields(13))//'="'//TRIM(Alphas(13))//'".')
        ErrorsFound = .TRUE.
    END SELECT


    ! Get Coil information
    IF (UnitVent(UnitVentNum)%CoilOption == BothOption .or. UnitVent(UnitVentNum)%CoilOption == HeatingOption) THEN
       ! Heating coil information:
       ! A14, \field Heating Coil Object Type
       !      \type choice
       !      \key Coil:Heating:Water
       !      \key Coil:Heating:Electric
       !      \key Coil:Heating:Gas
       !      \key Coil:Heating:Steam
       ! A15, \field Heating Coil Name
       !      \type object-list
       !      \object-list HeatingCoilName
      IF ((.NOT. lAlphaBlanks(15))) THEN
        UnitVent(UnitVentNum)%HCoilPresent     = .TRUE.
        errflag=.FALSE.

        cHeatingCoilType=Alphas(14)
        UnitVent(UnitVentNum)%HCoilTypeCh = cHeatingCoilType
        SELECT CASE (cHeatingCoilType)
          CASE ('COIL:HEATING:WATER')
            UnitVent(UnitVentNum)%HCoilType = Heating_WaterCoilType
            UnitVent(UnitVentNum)%HCoil_PlantTypeNum = TypeOf_CoilWaterSimpleHeating
          CASE ('COIL:HEATING:STEAM')
            UnitVent(UnitVentNum)%HCoilType = Heating_SteamCoilType
            UnitVent(UnitVentNum)%HCoil_PlantTypeNum = TypeOf_CoilSteamAirHeating
          CASE ('COIL:HEATING:ELECTRIC')
            UnitVent(UnitVentNum)%HCoilType = Heating_ElectricCoilType
          CASE ('COIL:HEATING:GAS')
            UnitVent(UnitVentNum)%HCoilType = Heating_GasCoilType
          CASE DEFAULT
            CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(UnitVent(UnitVentNum)%Name)//'", invalid')
            CALL ShowContinueError('illegal value: '//TRIM(cAlphaFields(14))//'="'//TRIM(Alphas(14))//'".')
            ErrorsFound = .TRUE.
            errflag = .TRUE.
        END SELECT
        IF (.NOT. errflag) THEN
          UnitVent(UnitVentNum)%HCoilName        = Alphas(15)
          CALL ValidateComponent(cHeatingCoilType,UnitVent(UnitVentNum)%HCoilName,IsNotOK,TRIM(CurrentModuleObject))
          IF (IsNotOK) THEN
             CALL ShowContinueError('...specified in '//TRIM(CurrentModuleObject)//'="'//  &
                                    TRIM(UnitVent(UnitVentNum)%Name)//'".')
             ErrorsFound=.TRUE.
          ELSE
            ! The heating coil control node is necessary for a hot water coil, but not necessary for an
            ! electric or gas coil.
            IF (UnitVent(UnitVentNum)%HCoilType .EQ. Heating_WaterCoilType .OR.   &
               UnitVent(UnitVentNum)%HCoilType .EQ. Heating_SteamCoilType) THEN
               ! mine the hot water or steam node from the coil object
               ErrFlag = .FALSE.
               IF (UnitVent(UnitVentNum)%HCoilType == Heating_WaterCoilType) THEN
                 UnitVent(UnitVentNum)%HotControlNode = GetCoilWaterInletNode('Coil:Heating:Water',  &
                                                        UnitVent(UnitVentNum)%HCoilName,ErrFlag)
               ELSE
                 UnitVent(UnitVentNum)%HCoil_Index = GetSteamCoilIndex('COIL:HEATING:STEAM',UnitVent(UnitVentNum)%HCoilName,ErrFlag)
                 UnitVent(UnitVentNum)%HotControlNode = GetSteamCoilSteamInletNode(UnitVent(UnitVentNum)%HCoil_Index, &
                                                        UnitVent(UnitVentNum)%HCoilName,ErrFlag)
               END IF
               ! Other error checks should trap before it gets to this point in the code, but including just in case.
               IF(ErrFlag)THEN
                 CALL ShowContinueError('...specified in '//TRIM(CurrentModuleObject)//'="' &
                                       //TRIM(UnitVent(UnitVentNum)%Name)//'".')
                 ErrorsFound = .TRUE.
               END IF
            END IF
          ENDIF
        ENDIF


        UnitVent(UnitVentNum)%MinVolHotWaterFlow = 0.0d0
        UnitVent(UnitVentNum)%MinVolHotSteamFlow = 0.0d0

        UnitVent(UnitVentNum)%HotControlOffset = Numbers(4)
        ! Set default convergence tolerance
        IF (UnitVent(UnitVentNum)%HotControlOffset .LE. 0.0d0) THEN
          UnitVent(UnitVentNum)%HotControlOffset = 0.001d0
        END IF
        SELECT CASE(UnitVent(UnitVentNum)%HCoilType)

          CASE(Heating_WaterCoilType)
            UnitVent(UnitVentNum)%MaxVolHotWaterFlow = GetWaterCoilMaxFlowRate('Coil:Heating:Water',  &
                                                       UnitVent(UnitVentNum)%HCoilName,ErrorsFound)
            UnitVent(UnitVentNum)%MaxVolHotSteamFlow = UnitVent(UnitVentNum)%MaxVolHotWaterFlow

          CASE(Heating_SteamCoilType)
            UnitVent(UnitVentNum)%MaxVolHotWaterFlow = GetSteamCoilMaxFlowRate('Coil:Heating:Steam',  &
                                                       UnitVent(UnitVentNum)%HCoilName,ErrorsFound)
            UnitVent(UnitVentNum)%MaxVolHotSteamFlow = UnitVent(UnitVentNum)%MaxVolHotWaterFlow

          CASE(Heating_ElectricCoilType)
          CASE(Heating_GasCoilType)
          CASE DEFAULT
        END SELECT
      ELSE       ! heating coil is required for these options
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(UnitVent(UnitVentNum)%Name)//  &
           '", missing heating coil')
        CALL ShowContinueError('a heating coil is required for '//trim(cAlphaFields(13))//'="'//trim(Alphas(13))//'".')
        ErrorsFound=.true.
      END IF     ! IF (.NOT. lAlphaBlanks(15)) THEN - from the start of heating coil information
    END IF  ! is option both or heating only

    IF (UnitVent(UnitVentNum)%CoilOption == BothOption .or. UnitVent(UnitVentNum)%CoilOption == CoolingOption) THEN
       ! Cooling coil information (if one is present):
       ! A16, \field Cooling Coil Object Type
       !      \type choice
       !      \key Coil:Cooling:Water
       !      \key Coil:Cooling:Water:DetailedGeometry
       !      \key CoilSystem:Cooling:Water:HeatExchangerAssisted
       ! A17, \field Cooling Coil Name
       !      \type object-list
       !      \object-list CoolingCoilsWater
      IF (.NOT. lAlphaBlanks(17)) THEN
        UnitVent(UnitVentNum)%CCoilPresent     = .TRUE.
        errflag=.FALSE.

        cCoolingCoilType=Alphas(16)
        UnitVent(UnitVentNum)%CCoilTypeCh = cCoolingCoilType
        SELECT CASE (cCoolingCoilType)
          CASE ('COIL:COOLING:WATER')
            UnitVent(UnitVentNum)%CCoilType = Cooling_CoilWaterCooling
            UnitVent(UnitVentNum)%CCoil_PlantTypeNum = TypeOf_CoilWaterCooling
            UnitVent(UnitVentNum)%CCoilPlantName=Alphas(17)
          CASE ('COIL:COOLING:WATER:DETAILEDGEOMETRY')
            UnitVent(UnitVentNum)%CCoilType = Cooling_CoilDetailedCooling
            UnitVent(UnitVentNum)%CCoil_PlantTypeNum  = TypeOf_CoilWaterDetailedFlatCooling
            UnitVent(UnitVentNum)%CCoilPlantName=Alphas(17)
          CASE ('COILSYSTEM:COOLING:WATER:HEATEXCHANGERASSISTED')
            UnitVent(UnitVentNum)%CCoilType = Cooling_CoilHXAssisted
            CALL GetHXCoilTypeAndName(cCoolingCoilType,Alphas(17),ErrorsFound,  &
               UnitVent(UnitVentNum)%CCoilPlantType,UnitVent(UnitVentNum)%CCoilPlantName)
            IF (SameString(UnitVent(UnitVentNum)%CCoilPlantType,'Coil:Cooling:Water')) THEN
              UnitVent(UnitVentNum)%CCoil_PlantTypeNum=TypeOf_CoilWaterCooling
            ELSEIF (SameString(UnitVent(UnitVentNum)%CCoilPlantType,'Coil:Cooling:Water:DetailedGeometry')) THEN
              UnitVent(UnitVentNum)%CCoil_PlantTypeNum=TypeOf_CoilWaterDetailedFlatCooling
            ELSE
              CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//trim(UnitVent(UnitVentNum)%Name)//'", invalid')
              CALL ShowContinueError('For: '//TRIM(cAlphaFields(16))//'="'//TRIM(Alphas(16))//'".')
              CALL ShowContinueError('Invalid Coil Type='//trim(UnitVent(UnitVentNum)%CCoilPlantType)//  &
                 ', Name='//trim(UnitVent(UnitVentNum)%CCoilPlantName))
              CALL ShowContinueError('must be "Coil:Cooling:Water" or "Coil:Cooling:Water:DetailedGeometry"')
              ErrorsFound=.true.
            ENDIF
          CASE DEFAULT
            CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(UnitVent(UnitVentNum)%Name)//'", invalid')
            CALL ShowContinueError('illegal value: '//TRIM(cAlphaFields(16))//'="'//TRIM(cCoolingCoilType)//'".')
            ErrorsFound=.TRUE.
            errflag=.TRUE.
        END SELECT

        IF (.NOT. errflag) THEN
          UnitVent(UnitVentNum)%CCoilName = Alphas(17)
          CALL ValidateComponent(cCoolingCoilType,UnitVent(UnitVentNum)%CCoilName,IsNotOK,TRIM(CurrentModuleObject))
          IF (IsNotOK) THEN
            CALL ShowContinueError('...specified in '//TRIM(CurrentModuleObject)//'="'// &
                                   TRIM(UnitVent(UnitVentNum)%Name)//'".')
            ErrorsFound=.TRUE.
          ELSE
            IF (UnitVent(UnitVentNum)%CCoilType /= Cooling_CoilHXAssisted) THEN
              ! mine the cold water node from the coil object
              UnitVent(UnitVentNum)%ColdControlNode = GetCoilWaterInletNode(UnitVent(UnitVentNum)%CCoilTypeCh, &
                                                                            UnitVent(UnitVentNum)%CCoilName,ErrFlag)
            ELSE
              UnitVent(UnitVentNum)%ColdControlNode = GetHXCoilWaterInletNode(UnitVent(UnitVentNum)%CCoilTypeCh, &
                                                                            UnitVent(UnitVentNum)%CCoilName,ErrFlag)
            ENDIF
            ! Other error checks should trap before it gets to this point in the code, but including just in case.
            IF (ErrFlag) THEN
              CALL ShowContinueError('...specified in '//TRIM(CurrentModuleObject)//'="'// &
                                     TRIM(UnitVent(UnitVentNum)%Name)//'".')
              ErrorsFound=.true.
            ENDIF
          ENDIF
        ENDIF

        UnitVent(UnitVentNum)%MinVolColdWaterFlow = 0.0d0
        UnitVent(UnitVentNum)%ColdControlOffset = Numbers(5)
        ! Set default convergence tolerance
        IF (UnitVent(UnitVentNum)%ColdControlOffset .LE. 0.0d0) THEN
          UnitVent(UnitVentNum)%ColdControlOffset = 0.001d0
        END IF
        SELECT CASE(UnitVent(UnitVentNum)%CCoilType)

          CASE(Cooling_CoilWaterCooling)
            UnitVent(UnitVentNum)%MaxVolColdWaterFlow =   &
               GetWaterCoilMaxFlowRate('Coil:Cooling:Water',  &
                  UnitVent(UnitVentNum)%CCoilName,ErrorsFound)
          CASE(Cooling_CoilDetailedCooling)
            UnitVent(UnitVentNum)%MaxVolColdWaterFlow =   &
               GetWaterCoilMaxFlowRate('Coil:Cooling:Water:DetailedGeometry',  &
                  UnitVent(UnitVentNum)%CCoilName,ErrorsFound)
          CASE(Cooling_CoilHXAssisted)
            UnitVent(UnitVentNum)%MaxVolColdWaterFlow =   &
               GetHXAssistedCoilFlowRate('CoilSystem:Cooling:Water:HeatExchangerAssisted',  &
                  UnitVent(UnitVentNum)%CCoilName,ErrorsFound)
          CASE DEFAULT

        END SELECT
      ELSE  ! Cooling Coil is required for this/these options
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(UnitVent(UnitVentNum)%Name)//  &
           '", missing cooling coil')
        CALL ShowContinueError('a cooling coil is required for '//trim(cAlphaFields(13))//'="'//trim(Alphas(13))//'".')
        ErrorsFound=.true.
      END IF     !IF (.NOT. lAlphaBlanks(17)) THEN - from the start of cooling coil information
    END IF

    ! check that unit ventilator air inlet node is the same as a zone exhaust node
    ZoneNodeNotFound = .TRUE.
    DO CtrlZone = 1,NumOfZones
      IF (.not. ZoneEquipConfig(CtrlZone)%IsControlled) CYCLE
      DO NodeNum = 1,ZoneEquipConfig(CtrlZone)%NumExhaustNodes
        IF (UnitVent(UnitVentNum)%AirInNode .EQ. ZoneEquipConfig(CtrlZone)%ExhaustNode(NodeNum)) THEN
          ZoneNodeNotFound = .FALSE.
          EXIT
        END IF
      END DO
    END DO
    IF(ZoneNodeNotFound)THEN
      CALL ShowSevereError(TRIM(CurrentModuleObject)//' = "'//TRIM(UnitVent(UnitVentNum)%Name)//'".'// &
                           ' Unit ventilator air inlet node name must be the same as a zone exhaust node name.')
      CALL ShowContinueError('..Zone exhaust node name is specified in ZoneHVAC:EquipmentConnections object.')
      CALL ShowContinueError('..Unit ventilator air inlet node name = '//TRIM(NodeID(UnitVent(UnitVentNum)%AirInNode)))
      ErrorsFound=.TRUE.
    END IF
    ! check that unit ventilator air outlet node is the same as a zone inlet node.
    ZoneNodeNotFound = .TRUE.
    DO CtrlZone = 1,NumOfZones
      IF (.not. ZoneEquipConfig(CtrlZone)%IsControlled) CYCLE
      DO NodeNum = 1,ZoneEquipConfig(CtrlZone)%NumInletNodes
        IF (UnitVent(UnitVentNum)%AirOutNode .EQ. ZoneEquipConfig(CtrlZone)%InletNode(NodeNum)) THEN
          ZoneNodeNotFound = .FALSE.
          EXIT
        END IF
      END DO
    END DO
    IF(ZoneNodeNotFound)THEN
      CALL ShowSevereError(TRIM(CurrentModuleObject)//' = "'//TRIM(UnitVent(UnitVentNum)%Name)//'".'// &
                           ' Unit ventilator air outlet node name must be the same as a zone inlet node name.')
      CALL ShowContinueError('..Zone inlet node name is specified in ZoneHVAC:EquipmentConnections object.')
      CALL ShowContinueError('..Unit ventilator air outlet node name = '//TRIM(NodeID(UnitVent(UnitVentNum)%AirOutNode)))
      ErrorsFound=.TRUE.
    END IF

    SELECT CASE (UnitVent(UnitVentNum)%CoilOption)
      CASE (BothOption) ! 'HeatingAndCooling'
        ! Add cooling coil to component sets array when present
        CALL SetUpCompSets(TRIM(CurrentModuleObject), UnitVent(UnitVentNum)%Name, &
                      cCoolingCoilType,UnitVent(UnitVentNum)%CCoilName,  &
                      NodeID(UnitVent(UnitVentNum)%FanOutletNode),'UNDEFINED')

        ! Add heating coil to component sets array when cooling coil present
        CALL SetUpCompSets(TRIM(CurrentModuleObject), UnitVent(UnitVentNum)%Name, &
                      cHeatingCoilType,UnitVent(UnitVentNum)%HCoilName,  &
                      'UNDEFINED',NodeID(UnitVent(UnitVentNum)%AirOutNode))

      CASE (HeatingOption) ! 'Heating'
        ! Add heating coil to component sets array when no cooling coil present
        CALL SetUpCompSets(TRIM(CurrentModuleObject), UnitVent(UnitVentNum)%Name, &
                      cHeatingCoilType,UnitVent(UnitVentNum)%HCoilName,  &
                      NodeID(UnitVent(UnitVentNum)%FanOutletNode),NodeID(UnitVent(UnitVentNum)%AirOutNode))

      CASE (CoolingOption) ! 'Cooling'
        ! Add cooling coil to component sets array when no heating coil present
        CALL SetUpCompSets(TRIM(CurrentModuleObject), UnitVent(UnitVentNum)%Name, &
                      cCoolingCoilType,UnitVent(UnitVentNum)%CCoilName,  &
                      NodeID(UnitVent(UnitVentNum)%FanOutletNode),NodeID(UnitVent(UnitVentNum)%AirOutNode))

      CASE (NoneOption)

      CASE DEFAULT

    END SELECT

  END DO  ! ...loop over all of the unit ventilators found in the input file

  DEALLOCATE(Alphas)
  DEALLOCATE(Numbers)
  DEALLOCATE(cAlphaFields)
  DEALLOCATE(cNumericFields)
  DEALLOCATE(lAlphaBlanks)
  DEALLOCATE(lNumericBlanks)

  IF (ErrorsFound) CALL ShowFatalError(RoutineName//'Errors found in input.')

          ! Setup Report variables for the Unit Ventilators, CurrentModuleObject='ZoneHVAC:UnitVentilator'
  DO UnitVentNum = 1, NumOfUnitVents
    CALL SetupOutputVariable('Zone Unit Ventilator Heating Rate [W]',        &
                             UnitVent(UnitVentNum)%HeatPower,'System', &
                             'Average',UnitVent(UnitVentNum)%Name)
    CALL SetupOutputVariable('Zone Unit Ventilator Heating Energy [J]',       &
                             UnitVent(UnitVentNum)%HeatEnergy,'System', &
                             'Sum',UnitVent(UnitVentNum)%Name)
    CALL SetupOutputVariable('Zone Unit Ventilator Total Cooling Rate [W]',     &
                             UnitVent(UnitVentNum)%TotCoolPower,'System', &
                             'Average',UnitVent(UnitVentNum)%Name)
    CALL SetupOutputVariable('Zone Unit Ventilator Total Cooling Energy [J]',    &
                             UnitVent(UnitVentNum)%TotCoolEnergy,'System', &
                             'Sum',UnitVent(UnitVentNum)%Name)
    CALL SetupOutputVariable('Zone Unit Ventilator Sensible Cooling Rate [W]', &
                             UnitVent(UnitVentNum)%SensCoolPower,'System',      &
                             'Average',UnitVent(UnitVentNum)%Name)
    CALL SetupOutputVariable('Zone Unit Ventilator Sensible Cooling Energy [J]',  &
                             UnitVent(UnitVentNum)%SensCoolEnergy,'System', &
                             'Sum',UnitVent(UnitVentNum)%Name)
    CALL SetupOutputVariable('Zone Unit Ventilator Fan Electric Power [W]',  &
                             UnitVent(UnitVentNum)%ElecPower,'System', &
                             'Average',UnitVent(UnitVentNum)%Name)
          ! Note that the unit vent fan electric is NOT metered because this value is already metered through the fan component
    CALL SetupOutputVariable('Zone Unit Ventilator Fan Electric Energy [J]',   &
                             UnitVent(UnitVentNum)%ElecEnergy,'System','Sum', &
                             UnitVent(UnitVentNum)%Name)
    CALL SetupOutputVariable('Zone Unit Ventilator Fan Availability Status []',&
                             UnitVent(UnitVentNum)%AvailStatus,&
                             'System','Average',UnitVent(UnitVentNum)%Name)
  END DO

  RETURN

END SUBROUTINE GetUnitVentilatorInput

SUBROUTINE InitUnitVentilator(UnitVentNum,FirstHVACIteration, ZoneNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Rick Strand
          !       DATE WRITTEN   May 2000
          !       MODIFIED       Chandan Sharma, FSEC, March 2011: Added zone sys avail manager
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine initializes all of the data elements which are necessary
          ! to simulate a unit ventilator.

          ! METHODOLOGY EMPLOYED:
          ! Uses the status flags to trigger initializations.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataEnvironment,    ONLY: StdBaroPress, StdRhoAir
  USE DataZoneEquipment,  ONLY: ZoneEquipInputsFilled,CheckZoneEquipmentList, UnitVentilator_Num
  USE DataHVACGlobals,    ONLY: ZoneComp, ZoneCompTurnFansOn, ZoneCompTurnFansOff
  USE DataPlant,          ONLY : ScanPlantLoopsForObject, PlantLoop,TypeOf_CoilWaterCooling, &
                                 TypeOf_CoilWaterDetailedFlatCooling, TypeOf_CoilWaterSimpleHeating, &
                                 TypeOf_CoilSteamAirHeating
  USE FluidProperties,    ONLY : GetDensityGlycol
  USE PlantUtilities,     ONLY : InitComponentNodes
  USE DataGlobals,        ONLY : AnyPlantInModel
  USE DataZoneEnergyDemands
  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: UnitVentNum         ! index for the current unit ventilator
  LOGICAL, INTENT(IN) :: FirstHVACIteration  ! TRUE if 1st HVAC simulation of system timestep
  INTEGER, INTENT(IN) :: ZoneNum             ! number of zone being served

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER        :: AirRelNode         ! relief air node number in unit ventilator loop
  INTEGER        :: ColdConNode        ! cold water control node number in unit ventilator loop
  LOGICAL,SAVE        :: MyOneTimeFlag = .TRUE.
  LOGICAL,SAVE        :: ZoneEquipmentListChecked = .FALSE.  ! True after the Zone Equipment List has been checked for items
  Integer             :: Loop
  LOGICAL, ALLOCATABLE,Save, DIMENSION(:) :: MyEnvrnFlag
  LOGICAL, ALLOCATABLE, SAVE, DIMENSION(:) :: MyPlantScanFlag
  INTEGER        :: HotConNode         ! hot water control node number in unit ventilator loop
  INTEGER        :: InNode             ! inlet node number in unit ventilator loop
  INTEGER        :: OutNode            ! outlet node number in unit ventilator loop
  INTEGER        :: OutsideAirNode     ! outside air node number in unit ventilator loop
  REAL(r64)      :: RhoAir             ! air density at InNode
  REAL(r64)      :: TempSteamIn
  REAL(r64)      :: SteamDensity
  REAL(r64)      :: rho  ! local fluid density
  LOGICAL        :: errFlag
  LOGICAL        :: SetMassFlowRateToZero ! TRUE when mass flow rates need to be set to zero
  SetMassFlowRateToZero = .FALSE.
          ! FLOW:

! Do the one time initializations
IF (MyOneTimeFlag) THEN

  ALLOCATE(MyEnvrnFlag(NumOfUnitVents))
  ALLOCATE(MySizeFlag(NumOfUnitVents))
  ALLOCATE(MyPlantScanFlag(NumOfUnitVents))
  MyEnvrnFlag = .TRUE.
  MySizeFlag = .TRUE.
  MyPlantScanFlag = .TRUE.
  MyOneTimeFlag = .FALSE.

END IF

IF (ALLOCATED(ZoneComp)) THEN
  ZoneComp(UnitVentilator_Num)%ZoneCompAvailMgrs(UnitVentNum)%ZoneNum = ZoneNum
  UnitVent(UnitVentNum)%AvailStatus = ZoneComp(UnitVentilator_Num)%ZoneCompAvailMgrs(UnitVentNum)%AvailStatus
ENDIF

IF (MyPlantScanFlag(UnitVentNum) .AND. ALLOCATED(PlantLoop)) THEN
  IF ((UnitVent(UnitVentNum)%HCoil_PlantTypeNum == TypeOf_CoilWaterSimpleHeating) .OR. &
      (UnitVent(UnitVentNum)%HCoil_PlantTypeNum == TypeOf_CoilSteamAirHeating)) THEN
    errFlag=.false.
    CALL ScanPlantLoopsForObject(         UnitVent(UnitVentNum)%HCoilName, &
                                          UnitVent(UnitVentNum)%HCoil_PlantTypeNum, &
                                          UnitVent(UnitVentNum)%HWLoopNum,   &
                                          UnitVent(UnitVentNum)%HWLoopSide,  &
                                          UnitVent(UnitVentNum)%HWBranchNum, &
                                          UnitVent(UnitVentNum)%HWCompNum,  &
                                          errFlag=errFlag)
    IF (errFlag) THEN
      CALL ShowContinueError('Reference Unit="'//trim(UnitVent(UnitVentNum)%Name)//'", type=ZoneHVAC:UnitVentilator')
      CALL ShowFatalError('InitUnitVentilator: Program terminated due to previous condition(s).')
    ENDIF

    UnitVent(UnitVentNum)%HotCoilOutNodeNum = &
                      PlantLoop(UnitVent(UnitVentNum)%HWLoopNum)%LoopSide(UnitVent(UnitVentNum)%HWLoopSide) &
                         %Branch(UnitVent(UnitVentNum)%HWBranchNum)%Comp(UnitVent(UnitVentNum)%HWCompNum)%NodeNumOut
  ENDIF
  IF ((UnitVent(UnitVentNum)%CCoil_PlantTypeNum == TypeOf_CoilWaterCooling) .OR. &
      (UnitVent(UnitVentNum)%CCoil_PlantTypeNum == TypeOf_CoilWaterDetailedFlatCooling)) THEN
    errFlag=.false.
    CALL ScanPlantLoopsForObject(         UnitVent(UnitVentNum)%CCoilPlantName, &
                                          UnitVent(UnitVentNum)%CCoil_PlantTypeNum, &
                                          UnitVent(UnitVentNum)%CWLoopNum,   &
                                          UnitVent(UnitVentNum)%CWLoopSide,  &
                                          UnitVent(UnitVentNum)%CWBranchNum, &
                                          UnitVent(UnitVentNum)%CWCompNum,  &
                                          errFlag=errFlag)
    IF (errFlag) THEN
      CALL ShowContinueError('Reference Unit="'//trim(UnitVent(UnitVentNum)%Name)//'", type=ZoneHVAC:UnitVentilator')
      CALL ShowFatalError('InitUnitVentilator: Program terminated due to previous condition(s).')
    ENDIF

    UnitVent(UnitVentNum)%ColdCoilOutNodeNum = &
                      PlantLoop(UnitVent(UnitVentNum)%CWLoopNum)%LoopSide(UnitVent(UnitVentNum)%CWLoopSide) &
                         %Branch(UnitVent(UnitVentNum)%CWBranchNum)%Comp(UnitVent(UnitVentNum)%CWCompNum)%NodeNumOut
  ELSE
    IF (UnitVent(UnitVentNum)%CCoilPresent)  &
         CALL ShowFatalError('InitUnitVentilator: Unit='//trim(UnitVent(UnitVentNum)%Name)//  &
               ', invalid cooling coil type. Program terminated.')
  ENDIF
  MyPlantScanFlag(UnitVentNum) = .FALSE.
ELSEIF (MyPlantScanFlag(UnitVentNum) .AND. .NOT. AnyPlantInModel)THEN
  MyPlantScanFlag(UnitVentNum) = .FALSE.
ENDIF


IF (.not. ZoneEquipmentListChecked .and. ZoneEquipInputsFilled) THEN
  ZoneEquipmentListChecked=.TRUE.
  DO Loop=1,NumOfUnitVents
    IF (CheckZoneEquipmentList('ZoneHVAC:UnitVentilator',UnitVent(Loop)%Name)) CYCLE
    CALL ShowSevereError('InitUnitVentilator: Unit=[UNIT VENTILATOR,'//  &
       TRIM(UnitVent(Loop)%Name)//  &
         '] is not on any ZoneHVAC:EquipmentList.  It will not be simulated.')
  ENDDO
ENDIF

IF ( .NOT. SysSizingCalc .AND. MySizeFlag(UnitVentNum) .AND. .NOT. MyPlantScanFlag(UnitVentNum)) THEN

  CALL SizeUnitVentilator(UnitVentNum)

  MySizeFlag(UnitVentNum) = .FALSE.
END IF

          ! Do the one time initializations
  IF (BeginEnvrnFlag .AND. MyEnvrnFlag(UnitVentNum) .AND. .NOT. MyPlantScanFlag(UnitVentNum)) THEN
    InNode         = UnitVent(UnitVentNum)%AirInNode
    OutNode        = UnitVent(UnitVentNum)%AirOutNode
    HotConNode     = UnitVent(UnitVentNum)%HotControlNode
    ColdConNode    = UnitVent(UnitVentNum)%ColdControlNode
    OutsideAirNode = UnitVent(UnitVentNum)%OutsideAirNode
    RhoAir         = StdRhoAir

    ! set the mass flow rates from the input volume flow rates
    UnitVent(UnitVentNum)%MaxAirMassFlow = RhoAir*UnitVent(UnitVentNum)%MaxAirVolFlow
    UnitVent(UnitVentNum)%OutAirMassFlow = RhoAir*UnitVent(UnitVentNum)%OutAirVolFlow
    UnitVent(UnitVentNum)%MinOutAirMassFlow = RhoAir*UnitVent(UnitVentNum)%MinOutAirVolFlow
    IF (UnitVent(UnitVentNum)%OutAirMassFlow > UnitVent(UnitVentNum)%MaxAirMassFlow) THEN
      UnitVent(UnitVentNum)%OutAirMassFlow = UnitVent(UnitVentNum)%MaxAirMassFlow
      UnitVent(UnitVentNum)%MinOutAirMassFlow = UnitVent(UnitVentNum)%OutAirMassFlow * &
        (UnitVent(UnitVentNum)%MinOutAirVolFlow / UnitVent(UnitVentNum)%OutAirVolFlow)
      CALL ShowWarningError('Outdoor air mass flow rate higher than unit flow rate, reset to unit flow rate for ' &
                            //TRIM(UnitVent(UnitVentNum)%Name))
    END IF

    ! set the node max and min mass flow rates
    Node(OutsideAirNode)%MassFlowRateMax = UnitVent(UnitVentNum)%OutAirMassFlow
    Node(OutsideAirNode)%MassFlowRateMin = 0.0d0

    Node(OutNode)%MassFlowRateMax = UnitVent(UnitVentNum)%MaxAirMassFlow
    Node(OutNode)%MassFlowRateMin = 0.0d0

    Node(InNode)%MassFlowRateMax = UnitVent(UnitVentNum)%MaxAirMassFlow
    Node(InNode)%MassFlowRateMin = 0.0d0

   IF (UnitVent(UnitVentNum)%HCoilPresent) THEN ! Only initialize these if a heating coil is actually present

    IF (UnitVent(UnitVentNum)%HCoilType == Heating_WaterCoilType) THEN

      rho = GetDensityGlycol( PlantLoop(UnitVent(UnitVentNum)%HWLoopNum)%FluidName, &
                             60.d0, &
                             PlantLoop(UnitVent(UnitVentNum)%HWLoopNum)%FluidIndex, &
                             'InitUnitVentilator')

      UnitVent(UnitVentNum)%MaxHotWaterFlow = rho*UnitVent(UnitVentNum)%MaxVolHotWaterFlow
      UnitVent(UnitVentNum)%MinHotWaterFlow = rho*UnitVent(UnitVentNum)%MinVolHotWaterFlow

      CALL InitComponentNodes ( UnitVent(UnitVentNum)%MinHotWaterFlow, &
                                UnitVent(UnitVentNum)%MaxHotWaterFlow, &
                                UnitVent(UnitVentNum)%HotControlNode,  &
                                UnitVent(UnitVentNum)%HotCoilOutNodeNum, &
                                UnitVent(UnitVentNum)%HWLoopNum,   &
                                UnitVent(UnitVentNum)%HWLoopSide,  &
                                UnitVent(UnitVentNum)%HWBranchNum, &
                                UnitVent(UnitVentNum)%HWCompNum )

    END IF
    IF (UnitVent(UnitVentNum)%HCoilType == Heating_SteamCoilType) THEN
      TempSteamIn= 100.00d0
      SteamDensity=GetSatDensityRefrig('STEAM',TempSteamIn,1.0d0,UnitVent(UnitVentNum)%HCoil_FluidIndex,'InitUnitVentilator')
      UnitVent(UnitVentNum)%MaxHotSteamFlow = SteamDensity*UnitVent(UnitVentNum)%MaxVolHotSteamFlow
      UnitVent(UnitVentNum)%MinHotSteamFlow = SteamDensity*UnitVent(UnitVentNum)%MinVolHotSteamFlow

      CALL InitComponentNodes ( UnitVent(UnitVentNum)%MinHotSteamFlow, &
                                UnitVent(UnitVentNum)%MaxHotSteamFlow, &
                                UnitVent(UnitVentNum)%HotControlNode,  &
                                UnitVent(UnitVentNum)%HotCoilOutNodeNum, &
                                UnitVent(UnitVentNum)%HWLoopNum,   &
                                UnitVent(UnitVentNum)%HWLoopSide,  &
                                UnitVent(UnitVentNum)%HWBranchNum, &
                                UnitVent(UnitVentNum)%HWCompNum )
    END IF
   END IF     !(UnitVent(UnitVentNum)%HCoilPresent)

    IF (UnitVent(UnitVentNum)%CCoilPresent) THEN ! Only initialize these if a cooling coil is actually present
      rho = GetDensityGlycol( PlantLoop(UnitVent(UnitVentNum)%CWLoopNum)%FluidName, &
                             5.d0, &
                             PlantLoop(UnitVent(UnitVentNum)%CWLoopNum)%FluidIndex, &
                             'InitUnitVentilator')

      UnitVent(UnitVentNum)%MaxColdWaterFlow = rho * UnitVent(UnitVentNum)%MaxVolColdWaterFlow
      UnitVent(UnitVentNum)%MinColdWaterFlow = rho * UnitVent(UnitVentNum)%MinVolColdWaterFlow
      CALL InitComponentNodes ( UnitVent(UnitVentNum)%MinColdWaterFlow, &
                                UnitVent(UnitVentNum)%MaxColdWaterFlow, &
                                UnitVent(UnitVentNum)%ColdControlNode,  &
                                UnitVent(UnitVentNum)%ColdCoilOutNodeNum, &
                                UnitVent(UnitVentNum)%CWLoopNum,   &
                                UnitVent(UnitVentNum)%CWLoopSide,  &
                                UnitVent(UnitVentNum)%CWBranchNum, &
                                UnitVent(UnitVentNum)%CWCompNum )

    END IF
    MyEnvrnFlag(UnitVentNum) = .FALSE.
  END IF  ! ...end start of environment inits

  IF (.NOT. BeginEnvrnFlag) MyEnvrnFlag(UnitVentNum) = .TRUE.

          ! These initializations are done every iteration...
  InNode         = UnitVent(UnitVentNum)%AirInNode
  OutNode        = UnitVent(UnitVentNum)%AirOutNode
  OutsideAirNode = UnitVent(UnitVentNum)%OutsideAirNode
  AirRelNode     = UnitVent(UnitVentNum)%AirReliefNode

  IF (GetCurrentScheduleValue(UnitVent(UnitVentNum)%SchedPtr) .GT. 0 ) THEN
    IF((GetCurrentScheduleValue(UnitVent(UnitVentNum)%FanAvailSchedPtr) .GT. 0 &
        .OR. ZoneCompTurnFansOn) .AND. .NOT. ZoneCompTurnFansOff)THEN
      IF ((ABS(ZoneSysEnergyDemand(ZoneNum)%RemainingOutputRequired) < SmallLoad) .OR. &
         (CurDeadBandOrSetback(ZoneNum))) THEN
        SetMassFlowRateToZero = .TRUE.
      ENDIF
    ELSE
      SetMassFlowRateToZero = .TRUE.
    ENDIF
  ELSE
    SetMassFlowRateToZero = .TRUE.
  ENDIF

  IF (SetMassFlowRateToZero) THEN
    Node(InNode)%MassFlowRate                 = 0.0d0
    Node(InNode)%MassFlowRateMaxAvail         = 0.0d0
    Node(InNode)%MassFlowRateMinAvail         = 0.0d0
    Node(OutNode)%MassFlowRate                = 0.0d0
    Node(OutNode)%MassFlowRateMaxAvail        = 0.0d0
    Node(OutNode)%MassFlowRateMinAvail        = 0.0d0
    Node(OutsideAirNode)%MassFlowRate         = 0.0d0
    Node(OutsideAirNode)%MassFlowRateMaxAvail = 0.0d0
    Node(OutsideAirNode)%MassFlowRateMinAvail = 0.0d0
    Node(AirRelNode)%MassFlowRate             = 0.0d0
    Node(AirRelNode)%MassFlowRateMaxAvail     = 0.0d0
    Node(AirRelNode)%MassFlowRateMinAvail     = 0.0d0
  ELSE
    Node(InNode)%MassFlowRate                 = UnitVent(UnitVentNum)%MaxAirMassFlow
    Node(InNode)%MassFlowRateMaxAvail         = UnitVent(UnitVentNum)%MaxAirMassFlow
    Node(InNode)%MassFlowRateMinAvail         = UnitVent(UnitVentNum)%MaxAirMassFlow
    Node(OutNode)%MassFlowRate                = UnitVent(UnitVentNum)%MaxAirMassFlow
    Node(OutNode)%MassFlowRateMaxAvail        = UnitVent(UnitVentNum)%MaxAirMassFlow
    Node(OutNode)%MassFlowRateMinAvail        = UnitVent(UnitVentNum)%MaxAirMassFlow
    Node(OutsideAirNode)%MassFlowRate         = UnitVent(UnitVentNum)%OutAirMassFlow
    Node(OutsideAirNode)%MassFlowRateMaxAvail = UnitVent(UnitVentNum)%OutAirMassFlow
    Node(OutsideAirNode)%MassFlowRateMinAvail = UnitVent(UnitVentNum)%OutAirMassFlow
    Node(AirRelNode)%MassFlowRate             = UnitVent(UnitVentNum)%OutAirMassFlow
    Node(AirRelNode)%MassFlowRateMaxAvail     = UnitVent(UnitVentNum)%OutAirMassFlow
    Node(AirRelNode)%MassFlowRateMinAvail     = UnitVent(UnitVentNum)%OutAirMassFlow
  ENDIF

  ! Initialize the relief air (same as inlet conditions to the unit ventilator...
  ! Note that mass flow rates will be taken care of later.
  Node(AirRelNode) = Node(InNode)
  OAMassFlowRate                = 0.0d0

  ! Just in case the unit is off and conditions do not get sent through
  ! the unit for some reason, set the outlet conditions equal to the inlet
  ! conditions of the unit ventilator
  Node(OutNode)%Temp     = Node(InNode)%Temp
  Node(OutNode)%Press    = Node(InNode)%Press
  Node(OutNode)%HumRat   = Node(InNode)%HumRat
  Node(OutNode)%Enthalpy = Node(InNode)%Enthalpy

  ! These initializations only need to be done once at the start of the iterations...
  IF (FirstHVACIteration) THEN
  ! Initialize the outside air conditions...
    Node(OutsideAirNode)%Temp     = Node(OutsideAirNode)%OutAirDryBulb
!    Node(OutsideAirNode)%HumRat   = OutHumRat
!    Node(OutsideAirNode)%Press    = OutBaroPress
!    Node(OutsideAirNode)%Enthalpy = PsyHFnTdbW(OutDryBulbTemp,OutHumRat)
  END IF

  RETURN

END SUBROUTINE InitUnitVentilator

SUBROUTINE SizeUnitVentilator(UnitVentNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   February 2002
          !       MODIFIED       August 2013 Daeho Kang, add component sizing table entries
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is for sizing Unit Ventilator components for which flow rates have not been
          ! specified in the input.

          ! METHODOLOGY EMPLOYED:
          ! Obtains flow rates from the zone sizing arrays and plant sizing data.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataSizing
  USE InputProcessor
  USE General,        ONLY: TrimSigDigits, RoundSigDigits
  USE WaterCoils,     ONLY: SetCoilDesFlow, GetCoilWaterInletNode, GetCoilWaterOutletNode
  USE SteamCoils,     ONLY: GetCoilSteamInletNode, GetCoilSteamOutletNode
  USE HVACHXAssistedCoolingCoil, ONLY: GetHXDXCoilName, GetHXCoilType
!  USE BranchInputManager, ONLY: MyPlantSizingIndex
  USE FluidProperties, ONLY: GetSpecificHeatGlycol, GetDensityGlycol
  USE DataPlant,       ONLY: PlantLoop, MyPlantSizingIndex
  USE ReportSizingManager, ONLY: ReportSizingOutput

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  Integer, Intent(IN) :: UnitVentNum

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
  REAL(r64)           :: CoilInEnthalpy
  REAL(r64)           :: CoilOutEnthalpy
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
  CHARACTER(len=MaxNameLength) :: CoolingCoilName
  CHARACTER(len=MaxNameLength) :: CoolingCoilType
  REAL(r64)           :: rho
  REAL(r64)           :: Cp
  INTEGER             :: DummyWaterIndex = 1
  LOGICAL :: IsAutosize                          ! Index to autosize
  REAL(r64)           :: MaxAirVolFlowDes        ! Autosized maximum air flow for reporting
  REAL(r64)           :: MaxAirVolFlowUser       ! Hardsized maximum air flow for reporting
  REAL(r64)           :: OutAirVolFlowDes        ! Autosized outdoor air flow for reporting
  REAL(r64)           :: OutAirVolFlowUser       ! Hardsized outdoor air flow for reporting
  REAL(r64)           :: MinOutAirVolFlowDes     ! Autosized minimum outdoor air flow for reporting
  REAL(r64)           :: MinOutAirVolFlowUser    ! Hardsized minimum outdoor air flow for reporting
  REAL(r64)           :: MaxVolHotWaterFlowDes   ! Autosized maximum water flow for reporting
  REAL(r64)           :: MaxVolHotWaterFlowUser  ! Hardsized maximum water flow for reporting
  REAL(r64)           :: MaxVolHotSteamFlowDes   ! Autosized maximum steam flow for reporting
  REAL(r64)           :: MaxVolHotSteamFlowUser  ! Hardsized maximum steam flow for reporting
  REAL(r64)           :: MaxVolColdWaterFlowDes  ! Autosized maximum chilled water flow for reporting
  REAL(r64)           :: MaxVolColdWaterFlowUser ! Hardsized maximum chilled water flow for reporting

  PltSizHeatNum = 0
  ErrorsFound = .FALSE.
  IsAutosize = .FALSE.
  MaxAirVolFlowDes = 0.0d0
  MaxAirVolFlowUser = 0.0d0
  OutAirVolFlowDes = 0.0d0
  OutAirVolFlowUser = 0.0d0
  MinOutAirVolFlowDes = 0.0d0
  MinOutAirVolFlowUser = 0.0d0
  MaxVolHotWaterFlowDes = 0.0d0
  MaxVolHotWaterFlowUser = 0.0d0
  MaxVolHotSteamFlowDes = 0.0d0
  MaxVolHotSteamFlowUser = 0.0d0
  MaxVolColdWaterFlowDes = 0.0d0
  MaxVolColdWaterFlowUser = 0.0d0

  IF (UnitVent(UnitVentNum)%MaxAirVolFlow == AutoSize) THEN
    IsAutosize = .TRUE.
  END IF
  IF (CurZoneEqNum > 0) THEN
    IF (.NOT. IsAutosize .AND. .NOT. ZoneSizingRunDone) THEN ! Simulation continue
      IF (UnitVent(UnitVentNum)%MaxAirVolFlow > 0.0d0) THEN
        CALL ReportSizingOutput(cMO_UnitVentilator, UnitVent(UnitVentNum)%Name, &
                     'User-Specified Maximum Supply Air Flow Rate [m3/s]', UnitVent(UnitVentNum)%MaxAirVolFlow)
      END IF
    ELSE
      CALL CheckZoneSizing(cMO_UnitVentilator, UnitVent(UnitVentNum)%Name)

      IF (UnitVent(UnitVentNum)%CoilOption == BothOption) THEN
        MaxAirVolFlowDes = MAX(FinalZoneSizing(CurZoneEqNum)%DesCoolVolFlow, &
                                              FinalZoneSizing(CurZoneEqNum)%DesHeatVolFlow)
      ELSEIF (UnitVent(UnitVentNum)%CoilOption == HeatingOption) THEN
        MaxAirVolFlowDes = FinalZoneSizing(CurZoneEqNum)%DesHeatVolFlow
      ELSEIF (UnitVent(UnitVentNum)%CoilOption == CoolingOption) THEN
        MaxAirVolFlowDes = FinalZoneSizing(CurZoneEqNum)%DesCoolVolFlow
      ELSEIF (UnitVent(UnitVentNum)%CoilOption == NoneOption) THEN
        MaxAirVolFlowDes = FinalZoneSizing(CurZoneEqNum)%MinOA
      END IF
      IF (MaxAirVolFlowDes < SmallAirVolFlow) THEN
        MaxAirVolFlowDes = 0.0d0
      END IF
      IF (IsAutosize) THEN
        UnitVent(UnitVentNum)%MaxAirVolFlow = MaxAirVolFlowDes
        CALL ReportSizingOutput(cMO_UnitVentilator, UnitVent(UnitVentNum)%Name, &
                     'Design Size Maximum Supply Air Flow Rate [m3/s]', MaxAirVolFlowDes)
      ELSE
        IF (UnitVent(UnitVentNum)%MaxAirVolFlow > 0.0d0 .AND. MaxAirVolFlowDes > 0.0d0 ) THEN
          MaxAirVolFlowUser = UnitVent(UnitVentNum)%MaxAirVolFlow
          CALL ReportSizingOutput(cMO_UnitVentilator, UnitVent(UnitVentNum)%Name, &
                     'Design Size Maximum Supply Air Flow Rate [m3/s]', MaxAirVolFlowDes, &
                     'User-Specified Maximum Supply Air Flow Rate [m3/s]', MaxAirVolFlowUser)
          IF (DisplayExtraWarnings) THEN
            IF ((ABS(MaxAirVolFlowDes - MaxAirVolFlowUser)/MaxAirVolFlowUser) > AutoVsHardSizingThreshold) THEN
              CALL ShowMessage('SizeUnitVentilator: Potential issue with equipment sizing for ' &
                                    //TRIM(cMO_UnitVentilator)//' '//TRIM(UnitVent(UnitVentNum)%Name))
              CALL ShowContinueError('User-Specified Maximum Supply Air Flow Rate of '// &
                                      TRIM(RoundSigDigits(MaxAirVolFlowUser,5))// ' [m3/s]')
              CALL ShowContinueError('differs from Design Size Maximum Supply Air Flow Rate of ' // &
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
  IF (UnitVent(UnitVentNum)%OutAirVolFlow == AutoSize) THEN
    IsAutosize = .TRUE.
  END IF
  IF (CurZoneEqNum > 0) THEN
    IF (.NOT. IsAutosize .AND. .NOT. ZoneSizingRunDone) THEN ! Simulation continue
      IF (UnitVent(UnitVentNum)%OutAirVolFlow > 0.0d0) THEN
        CALL ReportSizingOutput(cMO_UnitVentilator, UnitVent(UnitVentNum)%Name, &
                     'User-Specified Maximum Outdoor Air Flow Rate [m3/s]', UnitVent(UnitVentNum)%OutAirVolFlow)
      END IF
    ELSE
      CALL CheckZoneSizing(cMO_UnitVentilator, UnitVent(UnitVentNum)%Name)
      OutAirVolFlowDes = UnitVent(UnitVentNum)%MaxAirVolFlow
      IF (IsAutosize) THEN
        UnitVent(UnitVentNum)%OutAirVolFlow = OutAirVolFlowDes
        CALL ReportSizingOutput(cMO_UnitVentilator, UnitVent(UnitVentNum)%Name, &
                     'Design Size Maximum Outdoor Air Flow Rate [m3/s]', OutAirVolFlowDes)
      ELSE
        IF (UnitVent(UnitVentNum)%OutAirVolFlow > 0.0d0 .AND. OutAirVolFlowDes > 0.0d0) THEN
          OutAirVolFlowUser = UnitVent(UnitVentNum)%OutAirVolFlow
          CALL ReportSizingOutput(cMO_UnitVentilator, UnitVent(UnitVentNum)%Name, &
                     'Design Size Maximum Outdoor Air Flow Rate [m3/s]', OutAirVolFlowDes, &
                     'User-Specified Maximum Outdoor Air Flow Rate [m3/s]', OutAirVolFlowUser)
          IF (DisplayExtraWarnings) THEN
            IF ((ABS(OutAirVolFlowDes - OutAirVolFlowUser)/OutAirVolFlowUser) > AutoVsHardSizingThreshold) THEN
              CALL ShowMessage('SizeUnitVentilator: Potential issue with equipment sizing for ' &
                                  //TRIM(cMO_UnitVentilator)//' '//TRIM(UnitVent(UnitVentNum)%Name))
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
  IF (UnitVent(UnitVentNum)%MinOutAirVolFlow == AutoSize) THEN
    IsAutosize = .TRUE.
  END IF
  IF (CurZoneEqNum > 0) THEN
    IF (.NOT. IsAutosize .AND. .NOT. ZoneSizingRunDone) THEN ! Simulation continue
      IF (UnitVent(UnitVentNum)%MinOutAirVolFlow > 0.0d0) THEN
        CALL ReportSizingOutput(cMO_UnitVentilator, UnitVent(UnitVentNum)%Name, &
                     'User-Specified Minimum Outdoor Air Flow Rate [m3/s]', UnitVent(UnitVentNum)%MinOutAirVolFlow)
      END IF
    ELSE
      CALL CheckZoneSizing(cMO_UnitVentilator, UnitVent(UnitVentNum)%Name)
      MinOutAirVolFlowDes = MIN(FinalZoneSizing(CurZoneEqNum)%MinOA, &
                            UnitVent(UnitVentNum)%MaxAirVolFlow)
      IF (MinOutAirVolFlowDes < SmallAirVolFlow) THEN
        MinOutAirVolFlowDes = 0.0d0
      END IF
      IF (IsAutosize) THEN
        UnitVent(UnitVentNum)%MinOutAirVolFlow = MinOutAirVolFlowDes
        CALL ReportSizingOutput(cMO_UnitVentilator, UnitVent(UnitVentNum)%Name, &
                     'Design Size Minimum Outdoor Air Flow Rate [m3/s]', MinOutAirVolFlowDes)
      ELSE
        IF (UnitVent(UnitVentNum)%MinOutAirVolFlow > 0.0d0 .AND. MinOutAirVolFlowDes > 0.0d0) THEN
          MinOutAirVolFlowUser = UnitVent(UnitVentNum)%MinOutAirVolFlow
          CALL ReportSizingOutput(cMO_UnitVentilator, UnitVent(UnitVentNum)%Name, &
                     'Design Size Minimum Outdoor Air Flow Rate [m3/s]', MinOutAirVolFlowDes, &
                     'User-Specified Minimum Outdoor Air Flow Rate [m3/s]', MinOutAirVolFlowUser)
          IF (DisplayExtraWarnings) THEN
            IF ((ABS(MinOutAirVolFlowDes - MinOutAirVolFlowUser)/MinOutAirVolFlowUser) > AutoVsHardSizingThreshold) THEN
              CALL ShowMessage('SizeUnitVentilator: Potential issue with equipment sizing for ' &
                                     //TRIM(cMO_UnitVentilator)//' '//TRIM(UnitVent(UnitVentNum)%Name))
              CALL ShowContinueError('User-Specified Minimum Outdoor Air Flow Rate of '// &
                                      TRIM(RoundSigDigits(MinOutAirVolFlowUser,5))// ' [m3/s]')
              CALL ShowContinueError('differs from Design Size Minimum Outdoor Air Flow Rate of ' // &
                                      TRIM(RoundSigDigits(MinOutAirVolFlowDes,5))// ' [m3/s]')
              CALL ShowContinueError('This may, or may not, indicate mismatched component sizes.')
              CALL ShowContinueError('Verify that the value entered is intended and is consistent with other components.')
            END IF
          ENDIF
        END IF
      END IF
    END IF
  END IF

  IsAutosize = .FALSE.
  IF (UnitVent(UnitVentNum)%MaxVolHotWaterFlow==AutoSize) THEN
    IsAutosize = .TRUE.
  END IF
  IF (UnitVent(UnitVentNum)%HCoilType == Heating_WaterCoilType) THEN
    IF (CurZoneEqNum > 0) THEN
      IF (.NOT. IsAutosize .AND. .NOT. ZoneSizingRunDone) THEN ! Simulation continue
        IF (UnitVent(UnitVentNum)%MaxVolHotWaterFlow > 0.0d0) THEN
          CALL ReportSizingOutput(cMO_UnitVentilator, UnitVent(UnitVentNum)%Name, &
                     'User-Specified Maximum Hot Water Flow [m3/s]', UnitVent(UnitVentNum)%MaxVolHotWaterFlow)
        END IF
      ELSE
        CALL CheckZoneSizing(cMO_UnitVentilator, UnitVent(UnitVentNum)%Name)

        CoilWaterInletNode = GetCoilWaterInletNode('Coil:Heating:Water',UnitVent(UnitVentNum)%HCoilName,ErrorsFound)
        CoilWaterOutletNode = GetCoilWaterOutletNode('Coil:Heating:Water',UnitVent(UnitVentNum)%HCoilName,ErrorsFound)
        IF (IsAutosize) THEN
          PltSizHeatNum = MyPlantSizingIndex('Coil:Heating:Water', UnitVent(UnitVentNum)%HCoilName, CoilWaterInletNode, &
                                       CoilWaterOutletNode, ErrorsFound)
          IF (PltSizHeatNum > 0) THEN
            IF (FinalZoneSizing(CurZoneEqNum)%DesHeatMassFlow >= SmallAirVolFlow) THEN
              CoilInTemp = FinalZoneSizing(CurZoneEqNum)%DesHeatCoilInTemp
              CoilOutTemp = FinalZoneSizing(CurZoneEqNum)%HeatDesTemp
              CoilOutHumRat = FinalZoneSizing(CurZoneEqNum)%HeatDesHumRat
              DesCoilLoad = PsyCpAirFnWTdb(CoilOutHumRat, 0.5d0*(CoilInTemp+CoilOutTemp)) &
                              * FinalZoneSizing(CurZoneEqNum)%DesHeatMassFlow &
                              * (CoilOutTemp-CoilInTemp)

              rho = GetDensityGlycol(PlantLoop(UnitVent(UnitVentNum)%HWLoopNum)%FluidName, &
                                    60.d0, &
                                     PlantLoop(UnitVent(UnitVentNum)%HWLoopNum)%FluidIndex, &
                                     'SizeUnitVentilator')
              Cp = GetSpecificHeatGlycol(PlantLoop(UnitVent(UnitVentNum)%HWLoopNum)%FluidName, &
                                    60.d0, &
                                     PlantLoop(UnitVent(UnitVentNum)%HWLoopNum)%FluidIndex, &
                                     'SizeUnitVentilator')

              MaxVolHotWaterFlowDes = DesCoilLoad / &
                                     ( PlantSizData(PltSizHeatNum)%DeltaT * &
                                     Cp * rho )
            ELSE
              MaxVolHotWaterFlowDes = 0.0d0
            END IF
          ELSE
            CALL ShowSevereError('Autosizing of water flow requires a heating loop Sizing:Plant object')
            CALL ShowContinueError('Occurs in ' // cMO_UnitVentilator // ' Object=' &
                               //TRIM(UnitVent(UnitVentNum)%Name))
            ErrorsFound = .TRUE.
          END IF
        END IF
        IF (IsAutosize) THEN
            UnitVent(UnitVentNum)%MaxVolHotWaterFlow = MaxVolHotWaterFlowDes
            CALL ReportSizingOutput(cMO_UnitVentilator, UnitVent(UnitVentNum)%Name, &
                     'Design Size Maximum Hot Water Flow [m3/s]', MaxVolHotWaterFlowDes)
        ELSE
          IF (UnitVent(UnitVentNum)%MaxVolHotWaterFlow > 0.0d0 .AND. MaxVolHotWaterFlowDes > 0.0d0) THEN
            MaxVolHotWaterFlowUser = UnitVent(UnitVentNum)%MaxVolHotWaterFlow
            CALL ReportSizingOutput(cMO_UnitVentilator, UnitVent(UnitVentNum)%Name, &
                     'Design Size Maximum Hot Water Flow [m3/s]', MaxVolHotWaterFlowDes, &
                     'User-Specified Maximum Hot Water Flow [m3/s]', MaxVolHotWaterFlowUser)
            IF (DisplayExtraWarnings) THEN
              IF ((ABS(MaxVolHotWaterFlowDes - MaxVolHotWaterFlowUser)/MaxVolHotWaterFlowUser) > AutoVsHardSizingThreshold) THEN
                CALL ShowMessage('SizeUnitVentilator: Potential issue with equipment sizing for ' &
                                    //TRIM(cMO_UnitVentilator)//' '//TRIM(UnitVent(UnitVentNum)%Name))
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
    UnitVent(UnitVentNum)%MaxVolHotWaterFlow = 0.0d0
  END IF

  IsAutosize = .FALSE.
  IF (UnitVent(UnitVentNum)%MaxVolHotSteamFlow==AutoSize) THEN
    IsAutosize = .TRUE.
  END IF
  IF(UnitVent(UnitVentNum)%HCoilType == Heating_SteamCoilType) THEN
    IF (CurZoneEqNum > 0) THEN
      IF (.NOT. IsAutosize .AND. .NOT. ZoneSizingRunDone) THEN ! Simulation continue
        IF (UnitVent(UnitVentNum)%MaxVolHotSteamFlow > 0.0d0) THEN
          CALL ReportSizingOutput(cMO_UnitVentilator, UnitVent(UnitVentNum)%Name, &
                     'User-Specified Maximum Steam Flow [m3/s]', UnitVent(UnitVentNum)%MaxVolHotSteamFlow)
        END IF
      ELSE
        CALL CheckZoneSizing(cMO_UnitVentilator, UnitVent(UnitVentNum)%Name)

        CoilSteamInletNode = GetCoilSteamInletNode('Coil:Heating:Steam',UnitVent(UnitVentNum)%HCoilName,ErrorsFound)
        CoilSteamOutletNode = GetCoilSteamInletNode('Coil:Heating:Steam',UnitVent(UnitVentNum)%HCoilName,ErrorsFound)
        IF (IsAutosize) THEN
          PltSizHeatNum = MyPlantSizingIndex('Coil:Heating:Steam', UnitVent(UnitVentNum)%HCoilName, CoilSteamInletNode, &
                                       CoilSteamOutletNode, ErrorsFound)
          IF (PltSizHeatNum > 0) THEN
            IF (FinalZoneSizing(CurZoneEqNum)%DesHeatMassFlow >= SmallAirVolFlow) THEN
              CoilInTemp = FinalZoneSizing(CurZoneEqNum)%DesHeatCoilInTemp
              CoilOutTemp = FinalZoneSizing(CurZoneEqNum)%HeatDesTemp
              CoilOutHumRat = FinalZoneSizing(CurZoneEqNum)%HeatDesHumRat
              DesCoilLoad = PsyCpAirFnWTdb(CoilOutHumRat, 0.5d0*(CoilInTemp+CoilOutTemp)) &
                              * FinalZoneSizing(CurZoneEqNum)%DesHeatMassFlow &
                              * (CoilOutTemp-CoilInTemp)

              TempSteamIn= 100.00d0
              EnthSteamInDry =  GetSatEnthalpyRefrig('STEAM',TempSteamIn,1.0d0,RefrigIndex,'SizeUnitVentilator')
              EnthSteamOutWet=  GetSatEnthalpyRefrig('STEAM',TempSteamIn,0.0d0,RefrigIndex,'SizeUnitVentilator')
              LatentHeatSteam=EnthSteamInDry-EnthSteamOutWet
              SteamDensity=GetSatDensityRefrig('STEAM',TempSteamIn,1.0d0,RefrigIndex,'SizeUnitVentilator')
              Cp = GetSpecificHeatGlycol('WATER', PlantSizData(PltSizHeatNum)%ExitTemp, DummyWaterIndex, 'SizeUnitVentilator')

              MaxVolHotSteamFlowDes = DesCoilLoad/(SteamDensity*(LatentHeatSteam + &
                                    PlantSizData(PltSizHeatNum)%DeltaT * Cp ))
            ELSE
              MaxVolHotSteamFlowDes = 0.0d0
            END IF
          ELSE
            CALL ShowSevereError('Autosizing of Steam flow requires a heating loop Sizing:Plant object')
            CALL ShowContinueError('Occurs in ' // cMO_UnitVentilator // ' Object=' &
                               //TRIM(UnitVent(UnitVentNum)%Name))
            ErrorsFound = .TRUE.
          END IF
        END IF
        IF (IsAutosize) THEN
          UnitVent(UnitVentNum)%MaxVolHotSteamFlow = MaxVolHotSteamFlowDes
          CALL ReportSizingOutput(cMO_UnitVentilator, UnitVent(UnitVentNum)%Name, &
                     'Design Size Maximum Steam Flow [m3/s]', MaxVolHotSteamFlowDes)
        ELSE
          IF (UnitVent(UnitVentNum)%MaxVolHotSteamFlow > 0.0d0 .AND. MaxVolHotSteamFlowDes > 0.0d0) THEN
            MaxVolHotSteamFlowUser = UnitVent(UnitVentNum)%MaxVolHotSteamFlow
            CALL ReportSizingOutput(cMO_UnitVentilator, UnitVent(UnitVentNum)%Name, &
                     'Design Size Maximum Steam Flow [m3/s]', MaxVolHotSteamFlowDes, &
                     'User-Specified Maximum Steam Flow [m3/s]', MaxVolHotSteamFlowUser)
            IF (DisplayExtraWarnings) THEN
              IF ((ABS(MaxVolHotSteamFlowDes - MaxVolHotSteamFlowUser)/MaxVolHotSteamFlowUser) > AutoVsHardSizingThreshold) THEN
                CALL ShowMessage('SizeUnitVentilator: Potential issue with equipment sizing for ' &
                                      //TRIM(cMO_UnitVentilator)//' '//TRIM(UnitVent(UnitVentNum)%Name))
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
    UnitVent(UnitVentNum)%MaxVolHotSteamFlow = 0.0d0
  END IF

  IsAutosize = .FALSE.
  IF (UnitVent(UnitVentNum)%MaxVolColdWaterFlow == AutoSize) THEN
    IsAutosize = .TRUE.
  END IF
  IF (UnitVent(UnitVentNum)%CCoilType == Cooling_CoilWaterCooling .OR. &
      UnitVent(UnitVentNum)%CCoilType == Cooling_CoilDetailedCooling .OR. &
      UnitVent(UnitVentNum)%CCoilType == Cooling_CoilHXAssisted) THEN

    IF (CurZoneEqNum > 0) THEN
      IF (.NOT. IsAutosize .AND. .NOT. ZoneSizingRunDone) THEN ! Simulation continue
        IF (UnitVent(UnitVentNum)%MaxVolColdWaterFlow > 0.0d0) THEN
          CALL ReportSizingOutput(cMO_UnitVentilator, UnitVent(UnitVentNum)%Name, &
                     'User-Specified Maximum Cold Water Flow [m3/s]', UnitVent(UnitVentNum)%MaxVolColdWaterFlow)
        END IF
      ELSE
        CALL CheckZoneSizing(cMO_UnitVentilator, UnitVent(UnitVentNum)%Name)

        IF (UnitVent(UnitVentNum)%CCoilType == Cooling_CoilHXAssisted) THEN
          CoolingCoilName = GetHXDXCoilName(UnitVent(UnitVentNum)%CCoilTypeCh,UnitVent(UnitVentNum)%CCoilName,ErrorsFound)
          CoolingCoilType = GetHXCoilType(UnitVent(UnitVentNum)%CCoilTypeCh,UnitVent(UnitVentNum)%CCoilName,ErrorsFound)
        ELSE
          CoolingCoilName = UnitVent(UnitVentNum)%CCoilName
          CoolingCoilType = UnitVent(UnitVentNum)%CCoilTypeCh
        END IF
        CoilWaterInletNode = GetCoilWaterInletNode(CoolingCoilType,CoolingCoilName,ErrorsFound)
        CoilWaterOutletNode = GetCoilWaterOutletNode(CoolingCoilType,CoolingCoilName,ErrorsFound)
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
              rho = GetDensityGlycol(PlantLoop(UnitVent(UnitVentNum)%CWLoopNum)%FluidName, &
                                    5.d0, &
                                     PlantLoop(UnitVent(UnitVentNum)%CWLoopNum)%FluidIndex, &
                                     'SizeUnitVentilator')
              Cp = GetSpecificHeatGlycol(PlantLoop(UnitVent(UnitVentNum)%CWLoopNum)%FluidName, &
                                     5.d0, &
                                     PlantLoop(UnitVent(UnitVentNum)%CWLoopNum)%FluidIndex, &
                                     'SizeUnitVentilator')

              MaxVolColdWaterFlowDes = DesCoilLoad / &
                                   ( PlantSizData(PltSizCoolNum)%DeltaT * &
                                   Cp * rho )
              IF(MaxVolColdWaterFlowDes .LT. 0.d0)THEN
                CALL ShowWarningError('Autosizing of water flow resulted in negative value.')
                CALL ShowContinueError('Occurs in ' // cMO_UnitVentilator // ' Object=' &
                               //TRIM(UnitVent(UnitVentNum)%Name))
                CALL ShowContinueError('...Sizing information found during sizing simulation:')
                CALL ShowContinueError('...Coil inlet temperature      = '//TRIM(TrimSigDigits(CoilInTemp,3))//' C')
                CALL ShowContinueError('...Coil inlet humidity ratio   = '//TRIM(TrimSigDigits(CoilInHumRat,6))//' kg/kg')
                CoilInEnthalpy = PsyHFnTdbW(CoilInTemp, CoilInHumRat)
                CALL ShowContinueError('...Coil inlet enthalpy         = '//TRIM(TrimSigDigits(CoilInEnthalpy,3))//' J/kg')
                CALL ShowContinueError('...Sizing information from sizing object:')
                CALL ShowContinueError('...Coil outlet temperature     = '//TRIM(TrimSigDigits(CoilOutTemp,3))//' C')
                CALL ShowContinueError('...Coil outlet humidity ratio  = '//TRIM(TrimSigDigits(CoilOutHumRat,6))//' kg/kg')
                CoilOutEnthalpy = PsyHFnTdbW(CoilOutTemp, CoilOutHumRat)
                CALL ShowContinueError('...Coil outlet enthalpy         = '//TRIM(TrimSigDigits(CoilOutEnthalpy,3))//' J/kg')
                IF(CoilOutEnthalpy .GT. CoilInEnthalpy)THEN
                  CALL ShowContinueError('...Coil outlet air enthalpy is greater than coil inlet air enthalpy.')
                END IF
                CALL ShowContinueError('...Calculated coil design load = '//TRIM(TrimSigDigits(DesCoilLoad,3))//' W')
                CALL ShowContinueError('...Calculated water flow rate  = '// &
                                   TRIM(TrimSigDigits(MaxVolColdWaterFlowDes,3))//' m3/s')
                CALL ShowContinueError('...Water flow rate will be set to 0. Check sizing inputs for zone and plant,'// &
                                   ' inputs for water cooling coil object, and design day specifications.')
                CALL ShowContinueError('...Consider autosizing all inputs if not already doing so.')
                MaxVolColdWaterFlowDes = 0.0d0
              END IF
            ELSE
              MaxVolColdWaterFlowDes = 0.0d0
            END IF
          ELSE
            CALL ShowSevereError('Autosizing of water flow requires a cooling loop Sizing:Plant object')
            CALL ShowContinueError('Occurs in ' // cMO_UnitVentilator // ' Object=' &
                               //TRIM(UnitVent(UnitVentNum)%Name))
            Errorsfound = .TRUE.
          END IF
        END IF
        IF (IsAutosize) THEN
          UnitVent(UnitVentNum)%MaxVolColdWaterFlow = MaxVolColdWaterFlowDes
          CALL ReportSizingOutput(cMO_UnitVentilator, UnitVent(UnitVentNum)%Name, &
                     'Design Size Maximum Cold Water Flow [m3/s]', MaxVolColdWaterFlowDes)
        ELSE
          IF (UnitVent(UnitVentNum)%MaxVolColdWaterFlow > 0.0d0 .AND. MaxVolColdWaterFlowDes > 0.0d0) THEN
            MaxVolColdWaterFlowUser = UnitVent(UnitVentNum)%MaxVolColdWaterFlow
            CALL ReportSizingOutput(cMO_UnitVentilator, UnitVent(UnitVentNum)%Name, &
                     'Design Size Maximum Cold Water Flow [m3/s]', MaxVolColdWaterFlowDes, &
                     'User-Specified Maximum Cold Water Flow [m3/s]', MaxVolColdWaterFlowUser)
            IF (DisplayExtraWarnings) THEN
              IF ((ABS(MaxVolColdWaterFlowDes - MaxVolColdWaterFlowUser)/MaxVolColdWaterFlowUser) > AutoVsHardSizingThreshold) THEN
                CALL ShowMessage('SizeUnitVentilator: Potential issue with equipment sizing for ' &
                                    //TRIM(cMO_UnitVentilator)//' '//TRIM(UnitVent(UnitVentNum)%Name))
                CALL ShowContinueError('User-Specified Maximum Cold Water Flow of '// &
                                      TRIM(RoundSigDigits(MaxVolColdWaterFlowUser,5))// ' [m3/s]')
                CALL ShowContinueError('differs from Design Size Maximum Cold Water Flow of ' // &
                                      TRIM(RoundSigDigits(MaxVolColdWaterFlowDes,5))// ' [m3/s]')
                CALL ShowContinueError('This may, or may not, indicate mismatched component sizes.')
                CALL ShowContinueError('Verify that the value entered is intended and is consistent with other components.')
              END IF
            ENDIF
          END IF
        END IF
      END IF
    END IF
  END IF

  ! set the design air flow rates for the heating and cooling coils
  IF (UnitVent(UnitVentNum)%CCoilType == Cooling_CoilHXAssisted) THEN
    CoolingCoilName = GetHXDXCoilName(UnitVent(UnitVentNum)%CCoilTypeCh,UnitVent(UnitVentNum)%CCoilName,ErrorsFound)
    CoolingCoilType = GetHXCoilType(UnitVent(UnitVentNum)%CCoilTypeCh,UnitVent(UnitVentNum)%CCoilName,ErrorsFound)
  ELSE
    CoolingCoilName = UnitVent(UnitVentNum)%CCoilName
    CoolingCoilType = UnitVent(UnitVentNum)%CCoilTypeCh
  END IF
  CALL SetCoilDesFlow(CoolingCoilType,CoolingCoilName,UnitVent(UnitVentNum)%MaxAirVolFlow,&
                       ErrorsFound)
  CALL SetCoilDesFlow(UnitVent(UnitVentNum)%HCoilTypeCh,UnitVent(UnitVentNum)%HCoilName,UnitVent(UnitVentNum)%MaxAirVolFlow,&
                       ErrorsFound)

  IF (ErrorsFound) THEN
    CALL ShowFatalError('Preceding sizing errors cause program termination')
  END IF

  RETURN
END SUBROUTINE SizeUnitVentilator

SUBROUTINE CalcUnitVentilator(UnitVentNum,ZoneNum,FirstHVACIteration,PowerMet,LatOutputProvided)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Rick Strand
          !       DATE WRITTEN   May 2000
          !       MODIFIED       Don Shirey, Aug 2009 (LatOutputProvided)
          !                      July 2012, Chandan Sharma - FSEC: Added zone sys avail managers
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine mainly controls the action of the unit ventilator
          ! (or more exactly, it controls the amount of outside air brought in)
          ! based on the user input for controls and the defined controls
          ! algorithms.  There are currently (at the initial creation of this
          ! subroutine) two control methods: variable percent (ASHRAE "Cycle I"
          ! or "Cycle II") and fixed temperature (ASHRAE "Cycle III").

          ! METHODOLOGY EMPLOYED:
          ! Unit is controlled based on user input and what is happening in the
          ! simulation.  There are various cases to consider:
          ! 1. OFF: Unit is schedule off or there is no load on it.  All flow
          !    rates are set to zero and the temperatures are set to zone conditions
          !    (except for the outside air inlet).
          ! 2. HEATING/VARIABLE PERCENT: The unit is on, there is a heating load,
          !    and variable percent control is specified.  The outside air fraction
          !    is set to the minimum outside air fraction (schedule based) and the
          !    heating coil is activated.
          ! 3. HEATING/FIXED TEMPERATURE: The unit is on, there is a heating load,
          !    and fixed temperature control is specified.  The outside air fraction
          !    is varied in an attempt to obtain a mixed air temperature equal to
          !    the user specified temperature (schedule based).  The heating coil
          !    is activated, if necessary.
          ! 4. COOLING/NO COIL: The unit is on, there is a cooling load, and no
          !    coil is present or it has been scheduled off.  Set the amount of
          !    outside air based on the control type.  Simulate the "mixing box".
          ! 5. COOLING/WITH COIL: The unit is on, there is a cooling load, and
          !    a cooling coil is present and scheduled on.  Tries to use outside
          !    air as best as possible and then calls a cooling coil
          ! Note: controls are strictly temperature based and do not factor
          ! humidity into the equation (not an enthalpy economy cycle but rather
          ! a simple return air economy cycle).  In addition, temperature predictions
          ! are not strict energy balances here in the control routine though
          ! in the mixing routine an energy balance is preserved.


          ! REFERENCES:
          ! ASHRAE Systems and Equipment Handbook (SI), 1996. page 31.3

          ! USE STATEMENTS:
  USE DataZoneEnergyDemands
  USE HeatingCoils, ONLY : CheckHeatingCoilSchedule
  USE WaterCoils,   ONLY : CheckWaterCoilSchedule
  USE HVACHXAssistedCoolingCoil, ONLY :CheckHXAssistedCoolingCoilSchedule
  Use SteamCoils,   ONLY: CheckSteamCoilSchedule
  USE DataInterfaces, ONLY: ControlCompOutput
  USE DataZoneEquipment,   ONLY: UnitVentilator_Num
  USE DataHVACGlobals,    ONLY: ZoneCompTurnFansOn, ZoneCompTurnFansOff
  USE PlantUtilities, ONLY: SetComponentFlowRate
  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(INOUT) :: UnitVentNum        ! number of the current fan coil unit being simulated
  INTEGER, INTENT(IN)    :: ZoneNum            ! number of zone being served
  LOGICAL, INTENT(IN)    :: FirstHVACIteration ! TRUE if 1st HVAC simulation of system timestep
  REAL(r64), INTENT(OUT)   :: PowerMet           ! Sensible power supplied (W)
  REAL(r64), INTENT (OUT)  :: LatOutputProvided  ! Latent power supplied (kg/s), negative = dehumidification

          ! SUBROUTINE PARAMETER DEFINITIONS:
  REAL(r64), PARAMETER :: LowTempDiff = 0.1d0      ! Smallest allowed temperature difference for comparisons
                                            ! (below this value the temperatures are assumed equal)
  REAL(r64), PARAMETER :: LowOAFracDiff = 0.01d0   ! Smallest allowed outside air fraction difference for comparison
                                            ! (below this value the fractions are assumed equal)

          ! INTERFACE BLOCK SPECIFICATIONS
          ! see use DataInterfaces

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64)    :: AirMassFlow      ! air mass flow rate [kg/sec]
  INTEGER :: AirRelNode       ! outside air relief node
  INTEGER :: ControlNode      ! the hot water or cold water inlet node
  REAL(r64)    :: ControlOffset    ! tolerance for output control
  INTEGER :: InletNode        ! unit air inlet node
  REAL(r64)    :: MaxOAFrac        ! maximum possible outside air fraction
  REAL(r64)    :: MaxWaterFlow     ! maximum water flow for heating or cooling [kg/sec]
  REAL(r64)    :: MinOAFrac        ! minimum possible outside air fraction
  REAL(r64)    :: MinWaterFlow     ! minimum water flow for heating or cooling [kg/sec]
  INTEGER :: OutletNode       ! unit air outlet node
  INTEGER :: OutsideAirNode   ! outside air node
  REAL(r64)    :: QTotUnitOut      ! total unit output [watts]
  REAL(r64)    :: QUnitOut         ! heating or sens. cooling provided by fan coil unit [watts]
  REAL(r64)    :: Tdesired         ! desired temperature after mixing inlet and outdoor air [degrees C]
  REAL(r64)    :: Tinlet           ! temperature of air coming into the unit ventilator [degrees C]
  REAL(r64)    :: Toutdoor         ! temperature of outdoor air being introduced into the unit ventilator [degrees C]
  REAL(r64)    :: MaxSteamFlow
  REAL(r64)    :: MinSteamFlow
  REAL(r64)    :: LatentOutput   ! Latent (moisture) add/removal rate, negative is dehumidification [kg/s]
  REAL(r64)    :: SpecHumOut     ! Specific humidity ratio of outlet air (kg moisture / kg moist air)
  REAL(r64)    :: SpecHumIn      ! Specific humidity ratio of inlet air (kg moisture / kg moist air)
  REAL(r64)    :: mdot

 SELECT CASE (UnitVent(UnitVentNum)%CoilOption)
 CASE (BothOption)

  SELECT CASE(UnitVent(UnitVentNum)%HCoilType)

    CASE(Heating_WaterCoilType)
      CALL CheckWaterCoilSchedule('Coil:Heating:Water',UnitVent(UnitVentNum)%HCoilName,  &
                                  UnitVent(UnitVentNum)%HCoilSchedValue,UnitVent(UnitVentNum)%HCoil_Index)
    CASE(Heating_SteamCoilType)
      CALL CheckSteamCoilSchedule('Coil:Heating:Steam',UnitVent(UnitVentNum)%HCoilName,  &
                                  UnitVent(UnitVentNum)%HCoilSchedValue,UnitVent(UnitVentNum)%HCoil_Index)
    CASE(Heating_ElectricCoilType)
      CALL CheckHeatingCoilSchedule('Coil:Heating:Electric',UnitVent(UnitVentNum)%HCoilName,  &
                                    UnitVent(UnitVentNum)%HCoilSchedValue,UnitVent(UnitVentNum)%HCoil_Index)
    CASE(Heating_GasCoilType)
      CALL CheckHeatingCoilSchedule('Coil:Heating:Gas',UnitVent(UnitVentNum)%HCoilName,  &
                                    UnitVent(UnitVentNum)%HCoilSchedValue,UnitVent(UnitVentNum)%HCoil_Index)
    CASE DEFAULT
!      CALL ShowFatalError('Illegal coil type='//TRIM(UnitVent(UnitVentNum)%HCoilType))
  END SELECT


  SELECT CASE(UnitVent(UnitVentNum)%CCoilType)

    CASE(Cooling_CoilWaterCooling)
      CALL CheckWaterCoilSchedule('Coil:Cooling:Water',UnitVent(UnitVentNum)%CCoilName,  &
                                  UnitVent(UnitVentNum)%CCoilSchedValue,UnitVent(UnitVentNum)%CCoil_Index)
    CASE(Cooling_CoilDetailedCooling)
      CALL CheckWaterCoilSchedule('Coil:Cooling:Water:DetailedGeometry',UnitVent(UnitVentNum)%CCoilName,  &
                                  UnitVent(UnitVentNum)%CCoilSchedValue,UnitVent(UnitVentNum)%CCoil_Index)
    CASE(Cooling_CoilHXAssisted)
      CALL CheckHXAssistedCoolingCoilSchedule('CoilSystem:Cooling:Water:HeatExchangerAssisted',UnitVent(UnitVentNum)%CCoilName,  &
                                    UnitVent(UnitVentNum)%CCoilSchedValue,UnitVent(UnitVentNum)%CCoil_Index)
    CASE DEFAULT
!      CALL ShowFatalError('Illegal coil type='//TRIM(UnitVent(UnitVentNum)%CCoilType))
  END SELECT

 CASE (HeatingOption)

    SELECT CASE(UnitVent(UnitVentNum)%HCoilType)

    CASE(Heating_WaterCoilType)
      CALL CheckWaterCoilSchedule('Coil:Heating:Water',UnitVent(UnitVentNum)%HCoilName,  &
                                  UnitVent(UnitVentNum)%HCoilSchedValue,UnitVent(UnitVentNum)%HCoil_Index)
    CASE(Heating_SteamCoilType)
      CALL CheckSteamCoilSchedule('Coil:Heating:Steam',UnitVent(UnitVentNum)%HCoilName,  &
                                  UnitVent(UnitVentNum)%HCoilSchedValue,UnitVent(UnitVentNum)%HCoil_Index)
    CASE(Heating_ElectricCoilType)
      CALL CheckHeatingCoilSchedule('Coil:Heating:Electric',UnitVent(UnitVentNum)%HCoilName,  &
                                    UnitVent(UnitVentNum)%HCoilSchedValue,UnitVent(UnitVentNum)%HCoil_Index)
    CASE(Heating_GasCoilType)
      CALL CheckHeatingCoilSchedule('Coil:Heating:Gas',UnitVent(UnitVentNum)%HCoilName,  &
                                    UnitVent(UnitVentNum)%HCoilSchedValue,UnitVent(UnitVentNum)%HCoil_Index)
    CASE DEFAULT
!      CALL ShowFatalError('Illegal coil type='//TRIM(UnitVent(UnitVentNum)%HCoilType))
   END SELECT

 CASE (CoolingOption)

  SELECT CASE(UnitVent(UnitVentNum)%CCoilType)

    CASE(Cooling_CoilWaterCooling)
      CALL CheckWaterCoilSchedule('Coil:Cooling:Water',UnitVent(UnitVentNum)%CCoilName,  &
                                  UnitVent(UnitVentNum)%CCoilSchedValue,UnitVent(UnitVentNum)%CCoil_Index)
    CASE(Cooling_CoilDetailedCooling)
      CALL CheckWaterCoilSchedule('Coil:Cooling:Water:DetailedGeometry',UnitVent(UnitVentNum)%CCoilName,  &
                                  UnitVent(UnitVentNum)%CCoilSchedValue,UnitVent(UnitVentNum)%CCoil_Index)
    CASE(Cooling_CoilHXAssisted)
      CALL CheckHXAssistedCoolingCoilSchedule('CoilSystem:Cooling:Water:HeatExchangerAssisted',UnitVent(UnitVentNum)%CCoilName,  &
                                    UnitVent(UnitVentNum)%CCoilSchedValue,UnitVent(UnitVentNum)%CCoil_Index)
    CASE DEFAULT
!      CALL ShowFatalError('Illegal coil type='//TRIM(UnitVent(UnitVentNum)%CCoilType))

  END SELECT

 CASE (NoneOption)

 END SELECT


          ! FLOW:
  FanElecPower = 0.0d0
          ! initialize local variables
  ControlNode    = 0
  QUnitOut       = 0.0d0
  LatentOutput   = 0.0d0
  ControlOffset  = 0.0d0
  MaxWaterFlow   = 0.0d0
  MinWaterFlow   = 0.0d0
  InletNode      = UnitVent(UnitVentNum)%AirInNode
  OutletNode     = UnitVent(UnitVentNum)%AirOutNode
  OutsideAirNode = UnitVent(UnitVentNum)%OutsideAirNode
  AirRelNode     = UnitVent(UnitVentNum)%AirReliefNode

  QZnReq = ZoneSysEnergyDemand(ZoneNum)%RemainingOutputRequired ! zone load needed

  IF ( (ABS(QZnReq) < SmallLoad) .OR. &
       (CurDeadBandOrSetback(ZoneNum)) .OR. &
       (GetCurrentScheduleValue(UnitVent(UnitVentNum)%SchedPtr) <= 0).OR. &
       ((GetCurrentScheduleValue(UnitVent(UnitVentNum)%FanAvailSchedPtr) <= 0 &
        .AND. .NOT. ZoneCompTurnFansOn) .OR. ZoneCompTurnFansOff) ) THEN

          ! Unit is off or has no load upon it; set the flow rates to zero and then
          ! simulate the components with the no flow conditions
    AirMassFlow                               = Node(OutletNode)%MassFlowRate
    HCoilOn                                   = .FALSE.
    IF (UnitVent(UnitVentNum)%HotControlNode > 0) THEN
      mdot = 0.d0
      Call SetComponentFlowRate( mdot, &
                            UnitVent(UnitVentNum)%HotControlNode, &
                            UnitVent(UnitVentNum)%HotCoilOutNodeNum, &
                            UnitVent(UnitVentNum)%HWLoopNum, &
                            UnitVent(UnitVentNum)%HWLoopSide, &
                            UnitVent(UnitVentNum)%HWBranchNum, &
                            UnitVent(UnitVentNum)%HWCompNum)
    ENDIF
    IF (UnitVent(UnitVentNum)%ColdControlNode > 0) THEN
      mdot = 0.d0
      Call SetComponentFlowRate( mdot, &
                            UnitVent(UnitVentNum)%ColdControlNode, &
                            UnitVent(UnitVentNum)%ColdCoilOutNodeNum, &
                            UnitVent(UnitVentNum)%CWLoopNum, &
                            UnitVent(UnitVentNum)%CWLoopSide, &
                            UnitVent(UnitVentNum)%CWBranchNum, &
                            UnitVent(UnitVentNum)%CWCompNum)
    ENDIF
    CALL CalcUnitVentilatorComponents(UnitVentNum,FirstHVACIteration,QUnitOut)

  ELSE    ! Unit is on-->this section is intended to control the outside air and the main
          !              result is to set the outside air flow rate variable OAMassFlowRate

    IF (QZnReq > SmallLoad) THEN  ! HEATING MODE

      ControlNode   = UnitVent(UnitVentNum)%HotControlNode
      ControlOffset = UnitVent(UnitVentNum)%HotControlOffset
      MaxWaterFlow  = UnitVent(UnitVentNum)%MaxHotWaterFlow
      MinWaterFlow  = UnitVent(UnitVentNum)%MinHotWaterFlow
      MaxSteamFlow  = UnitVent(UnitVentNum)%MaxHotSteamFlow
      MinSteamFlow = UnitVent(UnitVentNum)%MinHotSteamFlow
!On the first HVAC iteration the system values are given to the controller, but after that
! the demand limits are in place and there needs to be feedback to the Zone Equipment
      If(.not. FirstHVACIteration .and. UnitVent(UnitVentNum)%HCoilType == Heating_WaterCoilType) Then
         MaxWaterFlow = Node(ControlNode)%MassFlowRateMaxAvail
         MinWaterFlow = Node(ControlNode)%MassFlowRateMinAvail
      End If

      If(.not. FirstHVACIteration .and. UnitVent(UnitVentNum)%HCoilType == Heating_SteamCoilType) Then
         MaxSteamFlow = Node(ControlNode)%MassFlowRateMaxAvail
         MinSteamFlow = Node(ControlNode)%MassFlowRateMinAvail
      End If
      HCoilOn       = .TRUE.

      If(Node(OutsideAirNode)%MassFlowRate > 0.0d0) Then
         MinOAFrac = GetCurrentScheduleValue(UnitVent(UnitVentNum)%MinOASchedPtr) * &
                    (UnitVent(UnitVentNum)%MinOutAirMassFlow / Node(OutsideAirNode)%MassFlowRate)
      Else
         MinOAFrac = 0.0d0
      End If
      MinOAFrac = MIN(1.0d0,MAX(0.0d0,MinOAFrac))




      IF ( (.NOT.UnitVent(UnitVentNum)%HCoilPresent) .OR. &
           (UnitVent(UnitVentNum)%HCoilSchedValue <= 0.0d0) ) THEN
          ! In heating mode, but there is no coil to provide heating.  This is handled
          ! differently than if there was a heating coil present.  Fixed temperature
          ! will still try to vary the amount of outside air to meet the desired
          ! mixed air temperature, while variable percent will go to full ventilation
          ! when it is most advantageous.


      SELECT CASE (UnitVent(UnitVentNum)%OAControlType)

      Case (FixedOAControl)
          ! In this control type, the outdoor air flow rate is fixed to the minimum value
          ! which is equal to the maximum value, regardless of all the other conditions.

        OAMassFlowRate = MinOAFrac*Node(OutsideAirNode)%MassFlowRate

      CASE (VariablePercent)
          ! This algorithm is probably a bit simplistic in that it just bounces
          ! back and forth between the maximum outside air and the minimum.  In
          ! REAL(r64)ity, a system *might* vary between the two based on the load in
          ! the zone.
          Tinlet    = Node(InletNode)%Temp
          Toutdoor  = Node(OutsideAirNode)%Temp

          IF (Tinlet >= Toutdoor) THEN

            OAMassFlowRate = MinOAFrac*Node(OutsideAirNode)%MassFlowRate

          ELSE ! Tinlet < Toutdoor

            MaxOAFrac = GetCurrentScheduleValue(UnitVent(UnitVentNum)%MaxOASchedPtr)
            OAMassFlowRate = MaxOAFrac*Node(OutsideAirNode)%MassFlowRate

          END IF

      CASE (FixedTemperature)
          ! In heating mode, the outside air for "fixed temperature" attempts
          ! to control the outside air fraction so that a desired temperature
          ! is met (if possible).  If this desired temperature is between the
          ! outside air temperature and the zone air temperature (inlet air
          ! temperature), then this is possible.  If not, the control will try
          ! to maximize the amount of air coming from the source that is closer
          ! in temperature to the desired temperature.
        Tdesired  = GetCurrentScheduleValue(UnitVent(UnitVentNum)%TempSchedPtr)
        Tinlet    = Node(InletNode)%Temp
        Toutdoor  = Node(OutsideAirNode)%Temp
        MaxOAFrac = 1.0d0

        IF (ABS(Tinlet-Toutdoor) <= LowTempDiff) THEN ! no difference in indoor and outdoor conditions-->set OA to minimum
          OAMassFlowRate = MinOAFrac*Node(OutsideAirNode)%MassFlowRate
        ELSE IF (ABS(MaxOAFrac-MinOAFrac) <= LowOAFracDiff) THEN  ! no difference in outside air fractions
          OAMassFlowRate = MinOAFrac*Node(OutsideAirNode)%MassFlowRate
        ELSE IF ( ( (Tdesired <= Tinlet) .AND. (Tdesired >= Toutdoor) ) .OR. &
                  ( (Tdesired >= Tinlet) .AND. (Tdesired <= Toutdoor) ) ) THEN
          ! Desired temperature is between the inlet and outdoor temperatures
          ! so vary the flow rate between no outside air and no recirculation air
          ! then applying the maximum and minimum limits the user has scheduled
          ! to make sure too much/little outside air is being introduced
          OAMassFlowRate = ((Tdesired - Tinlet)/(Toutdoor - Tinlet))*Node(InletNode)%MassFlowRate
          OAMassFlowRate = MAX(OAMassFlowRate,(MinOAFrac*Node(OutsideAirNode)%MassFlowRate))
          OAMassFlowRate = MIN(OAMassFlowRate,(MaxOAFrac*Node(OutsideAirNode)%MassFlowRate))
        ELSE IF ( (Tdesired < Tinlet) .AND. (Tdesired < Toutdoor) ) THEN
          ! Desired temperature is below both the inlet and outdoor temperatures
          ! so use whichever flow rate (max or min) that will get closer
          IF (Tinlet < Toutdoor) THEN   ! Tinlet closer to Tdesired so use minimum outside air
            OAMassFlowRate = MinOAFrac*Node(OutsideAirNode)%MassFlowRate
          ELSE  ! Toutdoor closer to Tdesired so use maximum outside air
            OAMassFlowRate = MaxOAFrac*Node(OutsideAirNode)%MassFlowRate
          END IF
        ELSE IF ( (Tdesired > Tinlet) .AND. (Tdesired > Toutdoor) ) THEN
          ! Desired temperature is above both the inlet and outdoor temperatures
          ! so use whichever flow rate (max or min) that will get closer
          IF (Tinlet > Toutdoor) THEN   ! Tinlet closer to Tdesired so use minimum outside air
            OAMassFlowRate = MinOAFrac*Node(OutsideAirNode)%MassFlowRate
          ELSE  ! Toutdoor closer to Tdesired so use maximum outside air
            OAMassFlowRate = MaxOAFrac*Node(OutsideAirNode)%MassFlowRate
          END IF
        ELSE
          ! It should NEVER get to this point, but just in case...
          CALL ShowFatalError('ZoneHVAC:UnitVentilator simulation control: illogical condition for '//  &
             TRIM(UnitVent(UnitVentNum)%Name))
        END IF

      END SELECT

      CALL CalcUnitVentilatorComponents(UnitVentNum,FirstHVACIteration,QUnitOut)



      ELSE     !  Coil/no coil block
          ! There is a heating load and there is a heating coil present (presumably).
          ! Variable percent will throttle outside air back to the minimum while
          ! fixed temperature will still try to vary the outside air amount to meet
          ! the desired mixed air temperature.


      SELECT CASE (UnitVent(UnitVentNum)%OAControlType)

      Case (FixedOAControl)
          ! In this control type, the outdoor air flow rate is fixed to the maximum value
          ! which is equal to the minimum value, regardless of all the other conditions.
        OAMassFlowRate = MinOAFrac*Node(OutsideAirNode)%MassFlowRate

      CASE (VariablePercent)
          ! In heating mode, the outside air for "variable percent" control
          ! is set to the minimum value
        OAMassFlowRate = MinOAFrac*Node(OutsideAirNode)%MassFlowRate

      CASE (FixedTemperature)
          ! In heating mode, the outside air for "fixed temperature" attempts
          ! to control the outside air fraction so that a desired temperature
          ! is met (if possible).  If this desired temperature is between the
          ! outside air temperature and the zone air temperature (inlet air
          ! temperature), then this is possible.  If not, the control will try
          ! to maximize the amount of air coming from the source that is closer
          ! in temperature to the desired temperature.
        Tdesired  = GetCurrentScheduleValue(UnitVent(UnitVentNum)%TempSchedPtr)
        Tinlet    = Node(InletNode)%Temp
        Toutdoor  = Node(OutsideAirNode)%Temp
        MaxOAFrac = 1.0d0

        IF (ABS(Tinlet-Toutdoor) <= LowTempDiff) THEN ! no difference in indoor and outdoor conditions-->set OA to minimum
          OAMassFlowRate = MinOAFrac*Node(OutsideAirNode)%MassFlowRate
        ELSE IF (ABS(MaxOAFrac-MinOAFrac) <= LowOAFracDiff) THEN  ! no difference in outside air fractions
          OAMassFlowRate = MinOAFrac*Node(OutsideAirNode)%MassFlowRate
        ELSE IF ( ( (Tdesired <= Tinlet) .AND. (Tdesired >= Toutdoor) ) .OR. &
                  ( (Tdesired >= Tinlet) .AND. (Tdesired <= Toutdoor) ) ) THEN
          ! Desired temperature is between the inlet and outdoor temperatures
          ! so vary the flow rate between no outside air and no recirculation air
          ! then applying the maximum and minimum limits the user has scheduled
          ! to make sure too much/little outside air is being introduced
          OAMassFlowRate = ((Tdesired - Tinlet)/(Toutdoor - Tinlet))*Node(InletNode)%MassFlowRate
          OAMassFlowRate = MAX(OAMassFlowRate,(MinOAFrac*Node(OutsideAirNode)%MassFlowRate))
          OAMassFlowRate = MIN(OAMassFlowRate,(MaxOAFrac*Node(OutsideAirNode)%MassFlowRate))
        ELSE IF ( (Tdesired < Tinlet) .AND. (Tdesired < Toutdoor) ) THEN
          ! Desired temperature is below both the inlet and outdoor temperatures
          ! so use whichever flow rate (max or min) that will get closer
          IF (Tinlet < Toutdoor) THEN   ! Tinlet closer to Tdesired so use minimum outside air
            OAMassFlowRate = MinOAFrac*Node(OutsideAirNode)%MassFlowRate
          ELSE  ! Toutdoor closer to Tdesired so use maximum outside air
            OAMassFlowRate = MaxOAFrac*Node(OutsideAirNode)%MassFlowRate
          END IF
        ELSE IF ( (Tdesired > Tinlet) .AND. (Tdesired > Toutdoor) ) THEN
          ! Desired temperature is above both the inlet and outdoor temperatures
          ! so use whichever flow rate (max or min) that will get closer
          IF (Tinlet > Toutdoor) THEN   ! Tinlet closer to Tdesired so use minimum outside air
            OAMassFlowRate = MinOAFrac*Node(OutsideAirNode)%MassFlowRate
          ELSE  ! Toutdoor closer to Tdesired so use maximum outside air
            OAMassFlowRate = MaxOAFrac*Node(OutsideAirNode)%MassFlowRate
          END IF
        ELSE
          ! It should NEVER get to this point, but just in case...
          CALL ShowFatalError('ZoneHVAC:UnitVentilator simulation control: illogical condition for '//  &
             TRIM(UnitVent(UnitVentNum)%Name))
        END IF

      END SELECT

      SELECT CASE (UnitVent(UnitVentNum)%HCoilType)

        CASE (Heating_WaterCoilType)
          ! control water flow to obtain output matching QZnReq
          CALL ControlCompOutput(CompName=UnitVent(UnitVentNum)%Name,CompType=cMO_UnitVentilator,CompNum=UnitVentNum, &
                                 FirstHVACIteration=FirstHVACIteration,QZnReq=QZnReq, &
                                 ActuatedNode=ControlNode,MaxFlow=MaxWaterFlow, &
                                 MinFlow=MinWaterFlow,ControlOffSet=ControlOffset, &
                                 ControlCompTypeNum=UnitVent(UnitVentNum)%ControlCompTypeNum, &
                                 CompErrIndex=UnitVent(UnitVentNum)%CompErrIndex, &
                                 LoopNum     = UnitVent(UnitVentNum)%HWLoopNum,   &
                                 LoopSide    = UnitVent(UnitVentNum)%HWLoopSide,  &
                                 BranchIndex = UnitVent(UnitVentNum)%HWBranchNum)

        CASE (Heating_GasCoilType,Heating_ElectricCoilType,Heating_SteamCoilType)

          CALL CalcUnitVentilatorComponents(UnitVentNum,FirstHVACIteration,QUnitOut)

      END SELECT

      END IF     !  Coil/no coil block

    ELSE    ! COOLING MODE

      ControlNode   = UnitVent(UnitVentNum)%ColdControlNode
      ControlOffset = UnitVent(UnitVentNum)%ColdControlOffset
      MaxWaterFlow  = UnitVent(UnitVentNum)%MaxColdWaterFlow
      MinWaterFlow  = UnitVent(UnitVentNum)%MinColdWaterFlow
!On the first HVAC iteration the system values are given to the controller, but after that
! the demand limits are in place and there needs to be feedback to the Zone Equipment
      If((.not. FirstHVACIteration) .and. (ControlNode > 0)  &
         .and. (UnitVent(UnitVentNum)%CCoilPresent)) Then
         MaxWaterFlow = Node(ControlNode)%MassFlowRateMaxAvail
         MinWaterFlow = Node(ControlNode)%MassFlowRateMinAvail
      End If
      HCoilOn       = .FALSE.

      Tinlet    = Node(InletNode)%Temp
      Toutdoor  = Node(OutsideAirNode)%Temp


      If(Node(OutsideAirNode)%MassFlowRate > 0.0d0) Then
         MinOAFrac = GetCurrentScheduleValue(UnitVent(UnitVentNum)%MinOASchedPtr) * &
                    (UnitVent(UnitVentNum)%MinOutAirMassFlow / Node(OutsideAirNode)%MassFlowRate)
      Else
         MinOAFrac = 0.0d0
      End If
      MinOAFrac = MIN(1.0d0,MAX(0.0d0,MinOAFrac))


      IF ( (.NOT.UnitVent(UnitVentNum)%CCoilPresent) .OR. &
           (UnitVent(UnitVentNum)%CCoilSchedValue <= 0.0d0) ) THEN
          ! In cooling mode, but there is no coil to provide cooling.  This is handled
          ! differently than if there was a cooling coil present.  Fixed temperature
          ! will still try to vary the amount of outside air to meet the desired
          ! mixed air temperature, while variable percent will go to full ventilation
          ! when it is most advantageous.
        SELECT CASE (UnitVent(UnitVentNum)%OAControlType)

        Case (FixedOAControl)
          ! In this control type, the outdoor air flow rate is fixed to the maximum value
          ! which is equal to the minimum value, regardless of all the other conditions.
          OAMassFlowRate = MinOAFrac*Node(OutsideAirNode)%MassFlowRate

        CASE (VariablePercent)
          ! This algorithm is probably a bit simplistic in that it just bounces
          ! back and forth between the maximum outside air and the minimum.  In
          ! REAL(r64)ity, a system *might* vary between the two based on the load in
          ! the zone.  This simple flow control might cause some overcooling but
          ! chances are that if there is a cooling load and the zone temperature
          ! gets above the outside temperature that overcooling won't be significant.
          IF (Tinlet <= Toutdoor) THEN

            OAMassFlowRate = MinOAFrac*Node(OutsideAirNode)%MassFlowRate

          ELSE ! Tinlet > Toutdoor

            MaxOAFrac = GetCurrentScheduleValue(UnitVent(UnitVentNum)%MaxOASchedPtr)
            OAMassFlowRate = MaxOAFrac*Node(OutsideAirNode)%MassFlowRate

          END IF

        CASE (FixedTemperature)
          ! This is basically the same algorithm as for the heating case...
          Tdesired  = GetCurrentScheduleValue(UnitVent(UnitVentNum)%TempSchedPtr)
          MaxOAFrac = 1.0d0

          IF (ABS(Tinlet-Toutdoor) <= LowTempDiff) THEN ! no difference in indoor and outdoor conditions-->set OA to minimum
            OAMassFlowRate = MinOAFrac*Node(OutsideAirNode)%MassFlowRate
          ELSE IF (ABS(MaxOAFrac-MinOAFrac) <= LowOAFracDiff) THEN  ! no difference in outside air fractions
            OAMassFlowRate = MinOAFrac*Node(OutsideAirNode)%MassFlowRate
          ELSE IF ( ( (Tdesired <= Tinlet) .AND. (Tdesired >= Toutdoor) ) .OR. &
                    ( (Tdesired >= Tinlet) .AND. (Tdesired <= Toutdoor) ) ) THEN
            ! Desired temperature is between the inlet and outdoor temperatures
            ! so vary the flow rate between no outside air and no recirculation air
            ! then applying the maximum and minimum limits the user has scheduled
            ! to make sure too much/little outside air is being introduced
            OAMassFlowRate = ((Tdesired - Tinlet)/(Toutdoor - Tinlet))*Node(InletNode)%MassFlowRate
            OAMassFlowRate = MAX(OAMassFlowRate,(MinOAFrac*Node(OutsideAirNode)%MassFlowRate))
            OAMassFlowRate = MIN(OAMassFlowRate,(MaxOAFrac*Node(OutsideAirNode)%MassFlowRate))
          ELSE IF ( (Tdesired < Tinlet) .AND. (Tdesired < Toutdoor) ) THEN
            ! Desired temperature is below both the inlet and outdoor temperatures
            ! so use whichever flow rate (max or min) that will get closer
            IF (Tinlet < Toutdoor) THEN   ! Tinlet closer to Tdesired so use minimum outside air
              OAMassFlowRate = MinOAFrac*Node(OutsideAirNode)%MassFlowRate
            ELSE  ! Toutdoor closer to Tdesired so use maximum outside air
              OAMassFlowRate = MaxOAFrac*Node(OutsideAirNode)%MassFlowRate
            END IF
          ELSE IF ( (Tdesired > Tinlet) .AND. (Tdesired > Toutdoor) ) THEN
            ! Desired temperature is above both the inlet and outdoor temperatures
            ! so use whichever flow rate (max or min) that will get closer
            IF (Tinlet > Toutdoor) THEN   ! Tinlet closer to Tdesired so use minimum outside air
              OAMassFlowRate = MinOAFrac*Node(OutsideAirNode)%MassFlowRate
            ELSE  ! Toutdoor closer to Tdesired so use maximum outside air
              OAMassFlowRate = MaxOAFrac*Node(OutsideAirNode)%MassFlowRate
            END IF
          ELSE
            ! It should NEVER get to this point, but just in case...
            CALL ShowFatalError('ZoneHVAC:UnitVentilator simulation control: illogical condition for '//  &
               TRIM(UnitVent(UnitVentNum)%Name))
          END IF

        END SELECT

        CALL CalcUnitVentilatorComponents(UnitVentNum,FirstHVACIteration,QUnitOut)

      ELSE
          ! There is a cooling load and there is a cooling coil present (presumably).
          ! Variable percent will throttle outside air back to the minimum while
          ! fixed temperature will still try to vary the outside air amount to meet
          ! the desired mixed air temperature.

        SELECT CASE (UnitVent(UnitVentNum)%OAControlType)

        Case (FixedOAControl)
            ! In this control type, the outdoor air flow rate is fixed to the maximum value
            ! which is equal to the minimum value, regardless of all the other conditions.
          OAMassFlowRate = MinOAFrac*Node(OutsideAirNode)%MassFlowRate

        CASE (VariablePercent)
          ! A cooling coil is present so let it try to do the cooling...
          OAMassFlowRate = MinOAFrac*Node(OutsideAirNode)%MassFlowRate

        CASE (FixedTemperature)
          ! This is basically the same algorithm as for the heating case...
          Tdesired  = GetCurrentScheduleValue(UnitVent(UnitVentNum)%TempSchedPtr)

          MaxOAFrac = 1.0d0

          IF (ABS(Tinlet-Toutdoor) <= LowTempDiff) THEN ! no difference in indoor and outdoor conditions-->set OA to minimum
            OAMassFlowRate = MinOAFrac*Node(OutsideAirNode)%MassFlowRate
          ELSE IF (ABS(MaxOAFrac-MinOAFrac) <= LowOAFracDiff) THEN  ! no difference in outside air fractions
            OAMassFlowRate = MinOAFrac*Node(OutsideAirNode)%MassFlowRate
          ELSE IF ( ( (Tdesired <= Tinlet) .AND. (Tdesired >= Toutdoor) ) .OR. &
                    ( (Tdesired >= Tinlet) .AND. (Tdesired <= Toutdoor) ) ) THEN
            ! Desired temperature is between the inlet and outdoor temperatures
            ! so vary the flow rate between no outside air and no recirculation air
            ! then applying the maximum and minimum limits the user has scheduled
            ! to make sure too much/little outside air is being introduced
            OAMassFlowRate = ((Tdesired - Tinlet)/(Toutdoor - Tinlet))*Node(InletNode)%MassFlowRate
            OAMassFlowRate = MAX(OAMassFlowRate,(MinOAFrac*Node(OutsideAirNode)%MassFlowRate))
            OAMassFlowRate = MIN(OAMassFlowRate,(MaxOAFrac*Node(OutsideAirNode)%MassFlowRate))
          ELSE IF ( (Tdesired < Tinlet) .AND. (Tdesired < Toutdoor) ) THEN
            ! Desired temperature is below both the inlet and outdoor temperatures
            ! so use whichever flow rate (max or min) that will get closer
            IF (Tinlet < Toutdoor) THEN   ! Tinlet closer to Tdesired so use minimum outside air
              OAMassFlowRate = MinOAFrac*Node(OutsideAirNode)%MassFlowRate
            ELSE  ! Toutdoor closer to Tdesired so use maximum outside air
              OAMassFlowRate = MaxOAFrac*Node(OutsideAirNode)%MassFlowRate
            END IF
          ELSE IF ( (Tdesired > Tinlet) .AND. (Tdesired > Toutdoor) ) THEN
            ! Desired temperature is above both the inlet and outdoor temperatures
            ! so use whichever flow rate (max or min) that will get closer
            IF (Tinlet > Toutdoor) THEN   ! Tinlet closer to Tdesired so use minimum outside air
              OAMassFlowRate = MinOAFrac*Node(OutsideAirNode)%MassFlowRate
            ELSE  ! Toutdoor closer to Tdesired so use maximum outside air
              OAMassFlowRate = MaxOAFrac*Node(OutsideAirNode)%MassFlowRate
            END IF
          ELSE
            ! It should NEVER get to this point, but just in case...
            CALL ShowFatalError('ZoneHVAC:UnitVentilator simulation control: illogical condition for '//  &
               TRIM(UnitVent(UnitVentNum)%Name))
          END IF

        END SELECT

          ! control water flow to obtain output matching QZnReq
        HCoilOn = .FALSE.
        CALL ControlCompOutput(CompName=UnitVent(UnitVentNum)%Name,CompType=cMO_UnitVentilator,CompNum=UnitVentNum, &
                               FirstHVACIteration=FirstHVACIteration,QZnReq=QZnReq, &
                               ActuatedNode=ControlNode,MaxFlow=MaxWaterFlow, &
                               MinFlow=MinWaterFlow,ControlOffSet=ControlOffset, &
                               ControlCompTypeNum=UnitVent(UnitVentNum)%ControlCompTypeNum, &
                               CompErrIndex=UnitVent(UnitVentNum)%CompErrIndex, &
                               LoopNum     = UnitVent(UnitVentNum)%CWLoopNum,   &
                               LoopSide    = UnitVent(UnitVentNum)%CWLoopSide,  &
                               BranchIndex = UnitVent(UnitVentNum)%CWBranchNum)

      END IF

    END IF  ! ...end of HEATING/COOLING IF-THEN block

    AirMassFlow    = Node(OutletNode)%MassFlowRate
    QUnitOut       = AirMassFlow * (PsyHFnTdbW(Node(OutletNode)%Temp,Node(InletNode)%HumRat)  &
                                  - PsyHFnTdbW(Node(InletNode)%Temp,Node(InletNode)%HumRat))

  END IF    ! ...end of unit ON/OFF IF-THEN block

! CR9155 Remove specific humidity calculations
  SpecHumOut = Node(OutletNode)%HumRat
  SpecHumIn  = Node(InletNode)%HumRat
  LatentOutput = AirMassFlow * (SpecHumOut - SpecHumIn) ! Latent rate (kg/s), dehumid = negative

  QTotUnitOut = AirMassFlow * (Node(OutletNode)%Enthalpy - Node(InletNode)%Enthalpy)

          ! Report variables...
  UnitVent(UnitVentNum)%HeatPower     = MAX(0.0d0,QUnitOut)
  UnitVent(UnitVentNum)%SensCoolPower = ABS(MIN(0.0d0,QUnitOut))
  UnitVent(UnitVentNum)%TotCoolPower  = ABS(MIN(0.0d0,QTotUnitOut))
  UnitVent(UnitVentNum)%ElecPower     = FanElecPower

  PowerMet = QUnitOut
  LatOutputProvided = LatentOutput

  RETURN

END SUBROUTINE CalcUnitVentilator

SUBROUTINE CalcUnitVentilatorComponents(UnitVentNum,FirstHVACIteration,LoadMet)

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
          ! Simply calls the different components in order.  Only slight wrinkles
          ! here are that the unit ventilator has it's own outside air mixed and
          ! that a cooling coil must be present in order to call a cooling coil
          ! simulation.  Other than that, the subroutine is very straightforward.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE Fans,         ONLY : SimulateFanComponents
  USE HeatingCoils, ONLY : SimulateHeatingCoilComponents
  USE WaterCoils,   ONLY : SimulateWaterCoilComponents
  USE HVACHXAssistedCoolingCoil, ONLY :SimHXAssistedCoolingCoil
  Use SteamCoils,   ONLY: SimulateSteamCoilComponents
  USE DataZoneEquipment,   ONLY: UnitVentilator_Num
  USE DataHVACGlobals,     ONLY: ZoneCompTurnFansOn, ZoneCompTurnFansOff

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)  :: UnitVentNum        ! Unit index in unit ventilator array
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
  CALL SimUnitVentOAMixer(UnitVentNum)

  CALL SimulateFanComponents(UnitVent(UnitVentNum)%FanName,FirstHVACIteration,UnitVent(UnitVentNum)%Fan_Index, &
                             ZoneCompTurnFansOn = ZoneCompTurnFansOn, ZoneCompTurnFansOff = ZoneCompTurnFansOff)

  IF (UnitVent(UnitVentNum)%CCoilPresent) THEN
    IF(UnitVent(UnitVentNum)%CCoilType == Cooling_CoilHXAssisted) THEN
      CALL SimHXAssistedCoolingCoil(UnitVent(UnitVentNum)%CCoilName,FirstHVACIteration,On,  &
                 0.0d0,UnitVent(UnitVentNum)%CCoil_Index,ContFanCycCoil)
    ELSE
      CALL SimulateWaterCoilComponents(UnitVent(UnitVentNum)%CCoilName,FirstHVACIteration,  &
                                       UnitVent(UnitVentNum)%CCoil_Index)
    END IF
  END IF

  IF (UnitVent(UnitVentNum)%HCoilPresent) THEN

    SELECT CASE (UnitVent(UnitVentNum)%HCoilType)

      CASE (Heating_WaterCoilType)

        CALL SimulateWaterCoilComponents(UnitVent(UnitVentNum)%HCoilName,FirstHVACIteration,  &
                                         UnitVent(UnitVentNum)%HCoil_Index)

      CASE (Heating_SteamCoilType)

        IF (.NOT.HCoilOn) THEN
          QCoilReq = 0.0d0
        ELSE
          HCoilInAirNode = UnitVent(UnitVentNum)%FanOutletNode
          CpAirZn        = PsyCpAirFnWTdb(Node(UnitVent(UnitVentNum)%AirInNode)%HumRat,Node(UnitVent(UnitVentNum)%AirInNode)%Temp)
          QCoilReq       = QZnReq - Node(HCoilInAirNode)%MassFlowRate * CpAirZn &
                                    *(Node(HCoilInAirNode)%Temp-Node(UnitVent(UnitVentNum)%AirInNode)%Temp)
        END IF

        IF (QCoilReq < 0.0d0) QCoilReq = 0.0d0    ! a heating coil can only heat, not cool

        CALL SimulateSteamCoilComponents(CompName=UnitVent(UnitVentNum)%HCoilName, &
                                           FirstHVACIteration=FirstHVACIteration,    &
                                           QCoilReq=QCoilReq,                        &
                                           CompIndex=UnitVent(UnitVentNum)%HCoil_Index)


      CASE (Heating_ElectricCoilType,Heating_GasCoilType)

        IF (.NOT.HCoilOn) THEN
          QCoilReq = 0.0d0
        ELSE
          HCoilInAirNode = UnitVent(UnitVentNum)%FanOutletNode
          CpAirZn        = PsyCpAirFnWTdb(Node(UnitVent(UnitVentNum)%AirInNode)%HumRat,Node(UnitVent(UnitVentNum)%AirInNode)%Temp)
          QCoilReq       = QZnReq - Node(HCoilInAirNode)%MassFlowRate * CpAirZn &
                                    *(Node(HCoilInAirNode)%Temp-Node(UnitVent(UnitVentNum)%AirInNode)%Temp)
        END IF

        IF (QCoilReq < 0.0d0) QCoilReq = 0.0d0    ! a heating coil can only heat, not cool

        CALL SimulateHeatingCoilComponents(CompName=UnitVent(UnitVentNum)%HCoilName, &
                                           FirstHVACIteration=FirstHVACIteration,    &
                                           QCoilReq=QCoilReq,                        &
                                           CompIndex=UnitVent(UnitVentNum)%HCoil_Index)

    END SELECT

  END IF     ! (UnitVent(UnitVentNum)%HCoilPresent)

  InletNode   = UnitVent(UnitVentNum)%AirInNode
  OutletNode  = UnitVent(UnitVentNum)%AirOutNode
  AirMassFlow = Node(OutletNode)%MassFlowRate

  LoadMet = AirMassFlow * (PsyHFnTdbW(Node(OutletNode)%Temp,Node(InletNode)%HumRat)  &
                         - PsyHFnTdbW(Node(InletNode)%Temp,Node(InletNode)%HumRat))

  RETURN

END SUBROUTINE CalcUnitVentilatorComponents

SUBROUTINE SimUnitVentOAMixer(UnitVentNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Rick Strand
          !       DATE WRITTEN   May 2000
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This responsibility of this subroutine is to set the air flow rates
          ! through the mixing box portion of the unit ventilator and then perform
          ! an energy balance to arrive at outlet conditions which then would
          ! serve as inlet conditions to the coils (or outlet conditions for
          ! the device).  There is some question as to whether this needs to be
          ! called every time the coils and fan are called since how the fans and
          ! coil operate won't presumable change how the mixer operates.  The
          ! method in which this routine is called is slightly cleaner though
          ! from a code readability standpoint though less efficient.

          ! METHODOLOGY EMPLOYED:
          ! The OAMassFlowRate has already been calculated in the main control
          ! algorithm.  Use this flow rate to establish all of the other flow
          ! rates and perform an energy balance on the mixing of the return and
          ! outdoor air streams.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataContaminantBalance, ONLY: Contaminant

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: UnitVentNum    ! Unit index in unit ventilator array

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER        :: AirRelNode         ! relief air node number in unit ventilator loop
  INTEGER        :: InletNode          ! inlet node number for unit ventilator loop
  REAL(r64)      :: OAFraction         ! Outside air fraction of inlet air
  INTEGER        :: OAMixOutNode       ! outside air mixer outlet node for unit ventilator loop
  INTEGER        :: OutsideAirNode     ! outside air node number in unit ventilator loop

          ! FLOW:
  AirRelNode     = UnitVent(UnitVentNum)%AirReliefNode
  InletNode      = UnitVent(UnitVentNum)%AirInNode
  OAMixOutNode   = UnitVent(UnitVentNum)%OAMixerOutNode
  OutsideAirNode = UnitVent(UnitVentNum)%OutsideAirNode

          ! "Resolve" the air flow rates...
  Node(OutsideAirNode)%MassFlowRate         = OAMassFlowRate
  Node(OutsideAirNode)%MassFlowRateMinAvail = OAMassFlowRate
  Node(OutsideAirNode)%MassFlowRateMaxAvail = OAMassFlowRate

  Node(AirRelNode)%MassFlowRate         = OAMassFlowRate
  Node(AirRelNode)%MassFlowRateMinAvail = OAMassFlowRate
  Node(AirRelNode)%MassFlowRateMaxAvail = OAMassFlowRate

  Node(OAMixOutNode)%MassFlowRate         = Node(InletNode)%MassFlowRate
  Node(OAMixOutNode)%MassFlowRateMinAvail = Node(InletNode)%MassFlowRate
  Node(OAMixOutNode)%MassFlowRateMaxAvail = Node(InletNode)%MassFlowRate

          ! "Inlet" conditions for InletNode and OutsideAirNode have already
          ! been set elsewhere so we just need to set the "outlet" conditions
  Node(AirRelNode)%Temp     = Node(InletNode)%Temp
  Node(AirRelNode)%Press    = Node(InletNode)%Press
  Node(AirRelNode)%HumRat   = Node(InletNode)%HumRat
  Node(AirRelNode)%Enthalpy = Node(InletNode)%Enthalpy

  IF (Node(InletNode)%MassFlowRate > 0.0d0) THEN
    OAFraction = Node(OutsideAirNode)%MassFlowRate/Node(InletNode)%MassFlowRate
  ELSE
    OAFraction = 0.0d0
  END IF

          ! Perform an energy and moisture mass balance on the mixing portion of the unit ventilator
  Node(OAMixOutNode)%Enthalpy = OAFraction*Node(OutsideAirNode)%Enthalpy &
                               +(1.0d0-OAFraction)*Node(InletNode)%Enthalpy
  Node(OAMixOutNode)%HumRat   = OAFraction*Node(OutsideAirNode)%HumRat &
                               +(1.0d0-OAFraction)*Node(InletNode)%HumRat

          ! Find the other key state points based on calculated conditions
  Node(OAMixOutNode)%Temp  = PsyTdbFnHW(Node(OAMixOutNode)%Enthalpy,Node(OAMixOutNode)%HumRat)
  Node(OAMixOutNode)%Press = Node(InletNode)%Press

  IF (Contaminant%CO2Simulation) Then
    Node(AirRelNode)%CO2 = Node(InletNode)%CO2
    Node(OAMixOutNode)%CO2 = OAFraction*Node(OutsideAirNode)%CO2+(1.0d0-OAFraction)*Node(InletNode)%CO2
  END IF
  IF (Contaminant%GenericContamSimulation) Then
    Node(AirRelNode)%GenContam = Node(InletNode)%GenContam
    Node(OAMixOutNode)%GenContam = OAFraction*Node(OutsideAirNode)%GenContam+(1.0d0-OAFraction)*Node(InletNode)%GenContam
  END IF

  RETURN

END SUBROUTINE SimUnitVentOAMixer

!SUBROUTINE UpdateUnitVentilator
!
! No update routine needed in this module since all of the updates happen on
! the Node derived type directly and these updates are done by other routines.
!
!END SUBROUTINE UpdateUnitVentilator

SUBROUTINE ReportUnitVentilator(UnitVentNum)

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
  INTEGER, INTENT(IN) :: UnitVentNum    ! Unit index in unit ventilator array

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          ! na

          ! FLOW:
  UnitVent(UnitVentNum)%HeatEnergy     = UnitVent(UnitVentNum)%HeatPower*TimeStepSys*SecInHour
  UnitVent(UnitVentNum)%SensCoolEnergy = UnitVent(UnitVentNum)%SensCoolPower*TimeStepSys*SecInHour
  UnitVent(UnitVentNum)%TotCoolEnergy  = UnitVent(UnitVentNum)%TotCoolPower*TimeStepSys*SecInHour
  UnitVent(UnitVentNum)%ElecEnergy     = UnitVent(UnitVentNum)%ElecPower*TimeStepSys*SecInHour

  RETURN

END SUBROUTINE ReportUnitVentilator

INTEGER FUNCTION GetUnitVentilatorOutAirNode(UnitVentNum)

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
  INTEGER, INTENT (IN)  :: UnitVentNum          !

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
          ! na
  IF (GetUnitVentilatorInputFlag) THEN
    CALL GetUnitVentilatorInput
    GetUnitVentilatorInputFlag=.FALSE.
  ENDIF

  GetUnitVentilatorOutAirNode = 0
  If (UnitVentNum > 0 .and. UnitVentNum <= NumOfUnitVents) THEN
    GetUnitVentilatorOutAirNode = UnitVent(UnitVentNum)%OutsideAirNode
  ENDIF

  RETURN

END FUNCTION GetUnitVentilatorOutAirNode

INTEGER FUNCTION GetUnitVentilatorZoneInletAirNode(UnitVentNum)

          ! FUNCTION INFORMATION:
          !       AUTHOR         B Griffith
          !       DATE WRITTEN   Dec  2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! lookup function for zone air inlet node

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN)  :: UnitVentNum          !

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
          ! na
  IF (GetUnitVentilatorInputFlag) THEN
    CALL GetUnitVentilatorInput
    GetUnitVentilatorInputFlag=.FALSE.
  ENDIF

  GetUnitVentilatorZoneInletAirNode = 0
  If (UnitVentNum > 0 .and. UnitVentNum <= NumOfUnitVents) THEN
    GetUnitVentilatorZoneInletAirNode = UnitVent(UnitVentNum)%AirOutNode
  ENDIF

  RETURN

END FUNCTION GetUnitVentilatorZoneInletAirNode

INTEGER FUNCTION GetUnitVentilatorMixedAirNode(UnitVentNum)

          ! FUNCTION INFORMATION:
          !       AUTHOR         B Griffith
          !       DATE WRITTEN   Dec  2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! lookup function for mixed air node

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN)  :: UnitVentNum          !

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
          ! na
  IF (GetUnitVentilatorInputFlag) THEN
    CALL GetUnitVentilatorInput
    GetUnitVentilatorInputFlag=.FALSE.
  ENDIF

  GetUnitVentilatorMixedAirNode = 0
  If (UnitVentNum > 0 .and. UnitVentNum <= NumOfUnitVents) THEN
    GetUnitVentilatorMixedAirNode = UnitVent(UnitVentNum)%OAMixerOutNode
  ENDIF

  RETURN

END FUNCTION GetUnitVentilatorMixedAirNode

INTEGER FUNCTION GetUnitVentilatorReturnAirNode(UnitVentNum)

          ! FUNCTION INFORMATION:
          !       AUTHOR         B Griffith
          !       DATE WRITTEN   Dec  2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! lookup function for return air node into "mixer"

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN)  :: UnitVentNum          !

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
          ! na
  IF (GetUnitVentilatorInputFlag) THEN
    CALL GetUnitVentilatorInput
    GetUnitVentilatorInputFlag=.FALSE.
  ENDIF

  GetUnitVentilatorReturnAirNode = 0
  If (UnitVentNum > 0 .and. UnitVentNum <= NumOfUnitVents) THEN
    GetUnitVentilatorReturnAirNode = UnitVent(UnitVentNum)%AirInNode
  ENDIF

  RETURN

END FUNCTION GetUnitVentilatorReturnAirNode

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

END MODULE UnitVentilator
