MODULE FluidCoolers

  ! Module containing the routines dealing with the objects FluidCooler:SingleSpeed and
  ! FluidCooler:TwoSpeed

  ! MODULE INFORMATION:
  !       AUTHOR         Chandan Sharma
  !       DATE WRITTEN   August 2008
  !       MODIFIED       April 2010, Chandan Sharma, FSEC
  !       RE-ENGINEERED  na

  ! PURPOSE OF THIS MODULE:
  ! Model the performance of fluid coolers

  ! METHODOLOGY EMPLOYED:

  ! REFERENCES:
  ! Based on cooling tower by Shirey, Raustad: Dec 2000; Shirey, Sept 2002

  ! OTHER NOTES:
  ! none

  ! USE STATEMENTS:
  ! Use statements for data only modules
USE DataPrecisionGlobals
USE DataGlobals,    ONLY: MaxNameLength, KelvinConv, SecInHour, WarmupFlag, InitConvTemp
USE DataInterfaces
USE DataHVACGlobals
USE DataLoopNode
USE DataEnvironment, ONLY: StdBaroPress, OutDryBulbTemp, OutHumRat, OutBaroPress, OutWetBulbTemp
USE DataPlant,       ONLY: PlantLoop
USE DataBranchAirLoopPlant, ONLY: MassFlowTolerance

  ! Use statements for access to subroutines in other modules
USE Psychrometrics,  ONLY: PsyWFnTdbTwbPb, PsyRhoAirFnPbTdbW, PsyHFnTdbRhPb, PsyCpAirFnWTdb, &
                           PsyTsatFnHPb, PsyWFnTdbH
USE FluidProperties, ONLY: GetDensityGlycol, GetSpecificHeatGlycol
USE General,         ONLY: TrimSigDigits

IMPLICIT NONE         ! Enforce explicit typing of all variables

PRIVATE ! Everything private unless explicitly made public

          ! MODULE PARAMETER DEFINITIONS:
CHARACTER(len=*), PARAMETER :: cFluidCooler_SingleSpeed = 'FluidCooler:SingleSpeed'
CHARACTER(len=*), PARAMETER :: cFluidCooler_TwoSpeed    = 'FluidCooler:TwoSpeed'

INTEGER, PARAMETER :: PIM_NominalCapacity      = 1
INTEGER, PARAMETER :: PIM_UFactor              = 2

INTEGER, PARAMETER :: FluidCooler_SingleSpeed  = 1
INTEGER, PARAMETER :: FluidCooler_TwoSpeed     = 2

  ! DERIVED TYPE DEFINITIONS
TYPE FluidCoolerspecs
  CHARACTER(len=MaxNameLength) :: Name                   = ' ' ! User identifier
  CHARACTER(len=MaxNameLength) :: FluidCoolerType        = ' ' ! Type of fluid cooler
  INTEGER :: FluidCoolerType_Num = 0
  INTEGER :: PerformanceInputMethod_Num = 0

  LOGICAL    :: Available                    = .TRUE. ! need an array of logicals--load identifiers of available equipment
  LOGICAL    :: ON                           = .TRUE. ! Simulate the machine at it's operating part load ratio
  REAL(r64)  :: DesignWaterFlowRate             = 0.0d0 ! Design water flow rate through the fluid cooler [m3/s]
  REAL(r64)  :: DesWaterMassFlowRate            = 0.0d0 ! Design water flow rate through the fluid cooler [kg/s]
  REAL(r64)  :: HighSpeedAirFlowRate            = 0.0d0 ! Air flow rate through the fluid cooler at high speed [m3/s]
  REAL(r64)  :: HighSpeedFanPower               = 0.0d0 ! Fan power at high fan speed [W]
  REAL(r64)  :: HighSpeedFluidCoolerUA          = 0.0d0 ! UA of fluid cooler at high fan speed [W/C]
  REAL(r64)  :: LowSpeedAirFlowRate             = 0.0d0 ! Air flow rate through fluid cooler at low speed [m3/s]
  REAL(r64)  :: LowSpeedAirFlowRateSizingFactor = 0.0d0 ! sizing factor for low speed air flow rate []
  REAL(r64)  :: LowSpeedFanPower                = 0.0d0 ! Fan power at low fan speed [W]
  REAL(r64)  :: LowSpeedFanPowerSizingFactor    = 0.0d0 ! sizing factor for low speed fan power []
  REAL(r64)  :: LowSpeedFluidCoolerUA           = 0.0d0 ! UA of fluid cooler at low fan speed [W/C]
  REAL(r64)  :: LowSpeedFluidCoolerUASizingFactor = 0.0d0 ! sizing factor for low speed UA []
  REAL(r64)  :: DesignEnteringWaterTemp         = 0.0d0 ! Entering water temperature at design conditions
  REAL(r64)  :: DesignLeavingWaterTemp          = 0.0d0 ! Entering water temperature at design conditions
  REAL(r64)  :: DesignEnteringAirTemp           = 0.0d0 ! Entering water temperature at design conditions
  REAL(r64)  :: DesignEnteringAirWetbulbTemp      = 0.0d0 ! Entering water temperature at design condition
  REAL(r64)  :: FluidCoolerMassFlowRateMultiplier = 0.0d0 ! Maximum fluid cooler flow rate is this multiplier * design flow rate
  REAL(r64)  :: FluidCoolerNominalCapacity        = 0.0d0 ! Nominal capacity of the fluid cooler [W] at high speed
  REAL(r64)  :: FluidCoolerLowSpeedNomCap         = 0.0d0 ! Nominal capacity of the fluid cooler [W] at low speed
  REAL(r64)  :: FluidCoolerLowSpeedNomCapSizingFactor = 0.0d0 !sizing factor for low speed capacity []
  INTEGER    :: WaterInletNodeNum               = 0  ! Node number on the water inlet side of the fluid cooler
  INTEGER    :: WaterOutletNodeNum              = 0  ! Node number on the water outlet side of the fluid cooler
  INTEGER    :: OutdoorAirInletNodeNum          = 0  ! Node number of outdoor air inlet for the fluid cooler
  INTEGER    :: HighMassFlowErrorCount          = 0  ! Counter when mass flow rate is > Design*FluidCoolerMassFlowRateMultiplier
  INTEGER    :: HighMassFlowErrorIndex          = 0  ! Index for high mass flow recurring error message
  INTEGER    :: OutletWaterTempErrorCount       = 0  ! Counter when outlet water temperature is < minimum allowed temperature
  INTEGER    :: OutletWaterTempErrorIndex       = 0  ! Index for outlet water temperature recurring error message
  INTEGER    :: SmallWaterMassFlowErrorCount    = 0  ! Counter when water mass flow rate is very small
  INTEGER    :: SmallWaterMassFlowErrorIndex    = 0  ! Index for very small water mass flow rate recurring error message
  INTEGER    :: WMFRLessThanMinAvailErrCount    = 0  ! Counter when water mass flow rate is less than minimum available
  INTEGER    :: WMFRLessThanMinAvailErrIndex    = 0  ! Index for water mass flow rate less than minavail recurring message
  INTEGER    :: WMFRGreaterThanMaxAvailErrCount = 0  ! Counter when water mass flow rate is greater than minimum available
  INTEGER    :: WMFRGreaterThanMaxAvailErrIndex = 0  ! Index for water mass flow rate > minavail recurring message

  !loop topology variables
  INTEGER    :: LoopNum     = 0
  INTEGER    :: LoopSideNum = 0
  INTEGER    :: BranchNum   = 0
  INTEGER    :: CompNum     = 0

END TYPE FluidCoolerspecs

TYPE FluidCoolerInletConds
  REAL(r64) :: WaterTemp      = 0.0d0  ! Fluid cooler water inlet temperature (C)
  REAL(r64) :: AirTemp        = 0.0d0  ! Fluid cooler air inlet dry-bulb temperature (C)
  REAL(r64) :: AirWetBulb     = 0.0d0  ! Fluid cooler air inlet wet-bulb temperature (C)
  REAL(r64) :: AirPress       = 0.0d0  ! Fluid cooler air barometric pressure
  REAL(r64) :: AirHumRat      = 0.0d0  ! Fluid cooler air inlet humidity ratio (kg/kg)
END TYPE FluidCoolerInletConds

TYPE ReportVars
  REAL(r64)    :: InletWaterTemp         = 0.0d0  ! Fluid cooler inlet water temperature (C)
  REAL(r64)    :: OutletWaterTemp        = 0.0d0  ! Fluid cooler outlet water temperature (C)
  REAL(r64)    :: WaterMassFlowRate      = 0.0d0  ! Fluid cooler water mass flow rate (m3/s)
  REAL(r64)    :: Qactual                = 0.0d0  ! Fluid cooler heat rejection rate (W)
  REAL(r64)    :: FanPower               = 0.0d0  ! Fluid cooler fan power (W)
  REAL(r64)    :: FanEnergy              = 0.0d0  ! Fluid cooler fan energy consumption (J)
END TYPE ReportVars

  ! MODULE VARIABLE DECLARATIONS:
INTEGER           :: NumSimpleFluidCoolers          = 0      ! Number of similar fluid coolers

! The following block of variables are used to carry model results for a fluid cooler instance
! across sim, update, and report routines.  Simulation manager must be careful
! in models with multiple fluid coolers.

REAL(r64)  :: InletWaterTemp           = 0.0d0    ! CW temperature at fluid cooler inlet
REAL(r64)  :: OutletWaterTemp          = 0.0d0    ! CW temperature at fluid cooler outlet
INTEGER    :: WaterInletNode           = 0      ! Node number at fluid cooler inlet
INTEGER    :: WaterOutletNode          = 0      ! Node number at fluid cooler outlet
REAL(r64)  :: WaterMassFlowRate        = 0.0d0    ! WaterMassFlowRate through fluid cooler
!DSU this is plant level stuff now  :: FluidCoolerMassFlowRateMax     = 0.0    ! Max Hardware Mass Flow Rate
!DSU this is plant level stuff now  :: FluidCoolerMassFlowRateMin     = 0.0    ! Min Hardware Mass Flow Rate
!DSU this is plant level stuff now  :: LoopMassFlowRateMaxAvail = 0.0    ! Max Loop Mass Flow Rate available
!DSU this is plant level stuff now  :: LoopMassFlowRateMinAvail = 0.0    ! Min Loop Mass Flow Rate available
REAL(r64)  :: Qactual                  = 0.0d0    ! Fluid cooler heat transfer
REAL(r64)  :: FanPower                 = 0.0d0    ! Fluid cooler fan power used

TYPE (FluidCoolerspecs),      ALLOCATABLE, DIMENSION(:) :: SimpleFluidCooler         ! dimension to number of machines
TYPE (FluidCoolerInletConds), ALLOCATABLE, DIMENSION(:) :: SimpleFluidCoolerInlet    ! inlet conditions
TYPE (ReportVars),      ALLOCATABLE, DIMENSION(:) :: SimpleFluidCoolerReport   ! report variables
LOGICAL, ALLOCATABLE, DIMENSION(:) :: CheckEquipName

          ! SUBROUTINE SPECIFICATIONS FOR MODULE CondenserLoopFluidCoolers

          ! Driver/Manager Routines
PUBLIC     SimFluidCoolers             ! PlantCondLoopSupplySideManager calls this top level Subroutine

          ! Get Input routines for module
PRIVATE    GetFluidCoolerInput         ! Retrieves inputs for specified fluid cooler

          ! Initialization routines for module
PRIVATE    InitFluidCooler             ! Initializes fluid cooler variables
PRIVATE    InitSimVars                 ! Initializes model level variables
PRIVATE    SizeFluidCooler             ! Automatically sizes the fluid cooler;
                                       ! also, calculates UA based on nominal capacity input(s)

          ! Update routines to check convergence and update nodes
PRIVATE    SimSimpleFluidCooler        ! Calculates exiting water temperature of fluid cooler
PRIVATE    SingleSpeedFluidCooler      ! Simulates a single speed fluid cooler using SimSimpleFluidCooler
PRIVATE    TwoSpeedFluidCooler         ! Simulates a two speed fluid cooler using SimSimpleFluidCooler
PRIVATE    SimpleFluidCoolerUAResidual ! Given a design fluid cooler load and a UA, calculates a residual
PRIVATE    UpdateFluidCooler           ! Updates node information and checks mass flow rate
PRIVATE    ReportFluidCooler           ! Outputs report variables

CONTAINS
          ! MODULE SUBROUTINES:

! Beginning of CondenserLoopFluidCoolers Module Driver Subroutines
!*************************************************************************

SUBROUTINE SimFluidCoolers(FluidCoolerType,FluidCoolerName, CompIndex, RunFlag,InitLoopEquip, &
                     MaxCap,MinCap,OptCap)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Chandan Sharma
          !       DATE WRITTEN   August 2008
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Main fluid cooler driver subroutine. Gets called from PlantCondLoopSupplySideManager

          ! METHODOLOGY EMPLOYED:
          ! After being called by PlantCondLoopSupplySideManager, this subroutine
          ! calls GetFluidCoolerInput to get all fluid cooler input info (one time only),
          ! then calls the appropriate subroutine to calculate fluid cooler performance,
          ! update records (node info) and writes output report info.

          ! REFERENCES:
          ! Based on SimTowers subroutine by Fred Buhl, May 2002; Richard Raustad, FSEC, Feb 2005

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: FindItemInList
  USE DataPlant,      ONLY: PlantSizesOkayToFinalize
  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*)      :: FluidCoolerType
  CHARACTER(len=*)      :: FluidCoolerName
  INTEGER               :: CompIndex
  LOGICAL               :: RunFlag
  LOGICAL, INTENT(IN)   :: InitLoopEquip
  REAL(r64)             :: OptCap
  REAL(r64)             :: MaxCap
  REAL(r64)             :: MinCap

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  LOGICAL, SAVE         :: GetInput = .TRUE.
  INTEGER               :: FluidCoolerNum

          !GET INPUT
  IF (GetInput) THEN
    CALL GetFluidCoolerInput
    GetInput = .FALSE.
  END IF
          !INITIALIZE
  ! Find the correct Equipment
  IF (CompIndex == 0) THEN
    FluidCoolerNum = FindItemInList(FluidCoolerName,SimpleFluidCooler%Name,NumSimpleFluidCoolers)
    IF (FluidCoolerNum == 0) THEN
      CALL ShowFatalError('SimFluidCoolers: Unit not found = '//TRIM(FluidCoolerName))
    ENDIF
    CompIndex=FluidCoolerNum
  ELSE
    FluidCoolerNum=CompIndex
    IF (FluidCoolerNum > NumSimpleFluidCoolers .or. FluidCoolerNum < 1) THEN
      CALL ShowFatalError('SimFluidCoolers:  Invalid CompIndex passed = '//  &
                          TRIM(TrimSigDigits(FluidCoolerNum))// &
                          ', Number of Units = '//TRIM(TrimSigDigits(NumSimpleFluidCoolers))//  &
                          ', Entered Unit name = '//TRIM(FluidCoolerName))
    ENDIF
    IF (CheckEquipName(FluidCoolerNum)) THEN
      IF (FluidCoolerName /= SimpleFluidCooler(FluidCoolerNum)%Name) THEN
        CALL ShowFatalError('SimFluidCoolers: Invalid CompIndex passed = '//  &
                            TRIM(TrimSigDigits(FluidCoolerNum))// &
                            ', Unit name = '//TRIM(FluidCoolerName)//', stored Unit Name for that index = '//  &
                            TRIM(SimpleFluidCooler(FluidCoolerNum)%Name))
      ENDIF
      CheckEquipName(FluidCoolerNum)=.false.
    ENDIF
  ENDIF

  CALL InitSimVars

          !CALCULATE
  TypeOfEquip:   &
    SELECT CASE (SimpleFluidCooler(FluidCoolerNum)%FluidCoolerType_Num)

      CASE (FluidCooler_SingleSpeed)
        IF (InitLoopEquip) THEN
          CALL InitFluidCooler(FluidCoolerNum, RunFlag)
          IF (.NOT. PlantSizesOkayToFinalize) CALL SizeFluidCooler(FluidCoolerNum)
          MinCap = 0.0d0
          MaxCap = SimpleFluidCooler(FluidCoolerNum)%FluidCoolerNominalCapacity
          OptCap = SimpleFluidCooler(FluidCoolerNum)%FluidCoolerNominalCapacity
          RETURN
        END IF
        CALL InitFluidCooler(FluidCoolerNum, RunFlag)
        CALL SingleSpeedFluidCooler(FluidCoolerNum)
        CALL UpdateFluidCooler(FluidCoolerNum)
        CALL ReportFluidCooler(RunFlag,FluidCoolerNum)

      CASE (FluidCooler_TwoSpeed)
        IF (InitLoopEquip) THEN
          CALL InitFluidCooler(FluidCoolerNum, RunFlag)
          IF (.NOT. PlantSizesOkayToFinalize) CALL SizeFluidCooler(FluidCoolerNum)
          MinCap = 0.0d0 ! signifies non-load based model (i.e. forward
          MaxCap = SimpleFluidCooler(FluidCoolerNum)%FluidCoolerNominalCapacity
          OptCap = SimpleFluidCooler(FluidCoolerNum)%FluidCoolerNominalCapacity
          RETURN
        END IF
        CALL InitFluidCooler(FluidCoolerNum, RunFlag)
        CALL TwoSpeedFluidCooler(FluidCoolerNum)
        CALL UpdateFluidCooler(FluidCoolerNum)
        CALL ReportFluidCooler(RunFlag,FluidCoolerNum)

      CASE DEFAULT
        CALL ShowFatalError('SimFluidCoolers: Invalid Fluid Cooler Type Requested = '//TRIM(FluidCoolerType))

    END SELECT TypeOfEquip

RETURN
END SUBROUTINE SimFluidCoolers

! End CondenserLoopFluidCoolers Module Driver Subroutines
!******************************************************************************


! Beginning of CondenserLoopFluidCoolers Module Get Input subroutines
!******************************************************************************

SUBROUTINE GetFluidCoolerInput

          ! SUBROUTINE INFORMATION:
          !       AUTHOR:          Chandan Sharma
          !       DATE WRITTEN:    August 2008
          !       MODIFIED         Chandan Sharma, FSEC, April 2010
          !       RE-ENGINEERED    na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Obtains input data for fluid coolers and stores it in SimpleFluidCooler data structure.

          ! METHODOLOGY EMPLOYED:
          ! Uses "Get" routines to read in the data.

          ! REFERENCES:
          ! Based on GetTowerInput subroutine from Don Shirey, Jan 2001 and Sept/Oct 2002;

          ! USE STATEMENTS:
  USE InputProcessor,     ONLY: GetNumObjectsFound, GetObjectItem, VerifyName, SameString
  USE DataIPShortCuts  ! Data for field names, blank numerics
  USE NodeInputManager,   ONLY: GetOnlySingleNode
  USE BranchNodeConnections, ONLY: TestCompSet
  USE DataSizing,         ONLY: AutoSize
  USE CurveManager,       ONLY: GetCurveIndex
  USE ScheduleManager,    ONLY: GetScheduleIndex
  USE OutAirNodeManager,  ONLY: CheckOutAirNodeNumber
  USE General,            ONLY: TrimSigDigits
  USE FluidProperties,    ONLY: CheckFluidPropertyName, FindGlycol

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
    CHARACTER(len=*), PARAMETER :: Blank = ' '

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    INTEGER        :: FluidCoolerNum                  ! Fluid cooler number, reference counter for SimpleFluidCooler data array
    INTEGER        :: NumSingleSpeedFluidCoolers      ! Total number of single-speed Fluid Coolers
    INTEGER        :: SingleSpeedFluidCoolerNumber    ! Specific single-speed fluid cooler of interest
    INTEGER        :: NumTwoSpeedFluidCoolers         ! Number of two-speed Fluid Coolers
    INTEGER        :: TwoSpeedFluidCoolerNumber       ! Specific two-speed fluid cooler of interest
    INTEGER        :: NumAlphas                 ! Number of elements in the alpha array
    INTEGER        :: NumNums                   ! Number of elements in the numeric array
    INTEGER        :: IOStat                    ! IO Status when calling get input subroutine
    LOGICAL        :: IsNotOK                   ! Flag to verify name
    LOGICAL        :: IsBlank                   ! Flag for blank name
    LOGICAL, SAVE  :: ErrorsFound=.false.       ! Logical flag set .true. if errors found while getting input data
    REAL(r64), DIMENSION(16)         :: NumArray              ! Numeric input data array
    CHARACTER(len=MaxNameLength),DIMENSION(5) :: AlphArray   ! Character string input data array

!! LKL - still more renaming stuff to go.

  ! Get number of all Fluid Coolers specified in the input data file (idf)
  NumSingleSpeedFluidCoolers   = GetNumObjectsFound('FluidCooler:SingleSpeed')
  NumTwoSpeedFluidCoolers      = GetNumObjectsFound('FluidCooler:TwoSpeed')
  NumSimpleFluidCoolers        = NumSingleSpeedFluidCoolers + NumTwoSpeedFluidCoolers

  IF (NumSimpleFluidCoolers <= 0 ) &
    CALL ShowFatalError('No fluid cooler objects found in input, however, a branch object has specified a fluid cooler. '//&
                        'Search the input for fluid cooler to determine the cause for this error.')

  ! See if load distribution manager has already gotten the input
  IF (ALLOCATED(SimpleFluidCooler))RETURN

  ! Allocate data structures to hold fluid cooler input data, report data and fluid cooler inlet conditions
  ALLOCATE (SimpleFluidCooler(NumSimpleFluidCoolers))
  ALLOCATE (SimpleFluidCoolerReport(NumSimpleFluidCoolers))
  ALLOCATE (SimpleFluidCoolerInlet(NumSimpleFluidCoolers))
  ALLOCATE(CheckEquipName(NumSimpleFluidCoolers))
  CheckEquipName=.true.

  ! Load data structures with fluid cooler input data
    cCurrentModuleObject = cFluidCooler_SingleSpeed
DO SingleSpeedFluidCoolerNumber = 1 , NumSingleSpeedFluidCoolers
    FluidCoolerNum = SingleSpeedFluidCoolerNumber
    CALL GetObjectItem(cCurrentModuleObject,SingleSpeedFluidCoolerNumber,AlphArray,NumAlphas,NumArray,NumNums,IOSTAT,  &
                   AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                   AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
    IsNotOK=.false.
    IsBlank=.false.
    CALL VerifyName(AlphArray(1),SimpleFluidCooler%Name,FluidCoolerNum-1,IsNotOK,IsBlank,TRIM(cCurrentModuleObject)//' Name')
    IF (IsNotOK) THEN
      ErrorsFound=.true.
      IF (IsBlank) AlphArray(1)='xxxxx'
    ENDIF
    SimpleFluidCooler(FluidCoolerNum)%Name                     = AlphArray(1)
    SimpleFluidCooler(FluidCoolerNum)%FluidCoolerType          = TRIM(cCurrentModuleObject)
    SimpleFluidCooler(FluidCoolerNum)%FluidCoolerType_Num      = FluidCooler_SingleSpeed
    SimpleFluidCooler(FluidCoolerNum)%FluidCoolerMassFlowRateMultiplier = 2.5d0
    SimpleFluidCooler(FluidCoolerNum)%WaterInletNodeNum        = &
               GetOnlySingleNode(AlphArray(2),ErrorsFound,TRIM(cCurrentModuleObject),AlphArray(1), &
               NodeType_Water,NodeConnectionType_Inlet, 1, ObjectIsNotParent)
    SimpleFluidCooler(FluidCoolerNum)%WaterOutletNodeNum       = &
               GetOnlySingleNode(AlphArray(3),ErrorsFound,TRIM(cCurrentModuleObject),AlphArray(1), &
               NodeType_Water,NodeConnectionType_Outlet, 1, ObjectIsNotParent)
    CALL TestCompSet(TRIM(cCurrentModuleObject),AlphArray(1),AlphArray(2),AlphArray(3),'Chilled Water Nodes')
    SimpleFluidCooler(FluidCoolerNum)%HighSpeedFluidCoolerUA        = NumArray(1)
    SimpleFluidCooler(FluidCoolerNum)%FluidCoolerNominalCapacity    = NumArray(2)
    SimpleFluidCooler(FluidCoolerNum)%DesignEnteringWaterTemp       = NumArray(3)
    SimpleFluidCooler(FluidCoolerNum)%DesignEnteringAirTemp         = NumArray(4)
    SimpleFluidCooler(FluidCoolerNum)%DesignEnteringAirWetbulbTemp  = NumArray(5)
    SimpleFluidCooler(FluidCoolerNum)%DesignWaterFlowRate           = NumArray(6)
    SimpleFluidCooler(FluidCoolerNum)%HighSpeedAirFlowRate          = NumArray(7)
    SimpleFluidCooler(FluidCoolerNum)%HighSpeedFanPower             = NumArray(8)

!   outdoor air inlet node
    IF (AlphArray(5) == Blank) THEN
      SimpleFluidCooler(FluidCoolerNum)%OutdoorAirInletNodeNum = 0
    ELSE
      SimpleFluidCooler(FluidCoolerNum)%OutdoorAirInletNodeNum = &
       GetOnlySingleNode(AlphArray(5),ErrorsFound,TRIM(cCurrentModuleObject),SimpleFluidCooler(FluidCoolerNum)%Name, &
                         NodeType_Air,NodeConnectionType_OutsideAirReference,1,ObjectIsNotParent)
      IF(.not. CheckOutAirNodeNumber(SimpleFluidCooler(FluidCoolerNum)%OutdoorAirInletNodeNum))THEN
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//'= "'//TRIM(SimpleFluidCooler(FluidCoolerNum)%Name)// &
                          '" '//trim(cAlphaFieldNames(5))//'= "'//TRIM(AlphArray(5))//'" not valid.')
        CALL ShowContinueError('...does not appear in an OutdoorAir:NodeList or as an OutdoorAir:Node.')
        ErrorsFound=.true.
      END IF
    ENDIF

!   Design entering water temperature, design entering air temperature and design entering air
!   wetbulb temperature must be specified for the both the performance input methods
    IF (SimpleFluidCooler(FluidCoolerNum)%DesignEnteringWaterTemp <= 0.0d0) THEN
      CALL ShowSevereError(trim(cCurrentModuleObject)//' = "'//trim(AlphArray(1))//'", invalid data for "'//   &
        trim(cNumericFieldNames(3))//'", entered value <= 0.0, but must be > 0 ')
       ErrorsFound=.true.
    ENDIF
    IF (SimpleFluidCooler(FluidCoolerNum)%DesignEnteringAirTemp <= 0.0d0) THEN
      CALL ShowSevereError(trim(cCurrentModuleObject)//' = "'//trim(AlphArray(1))//'", invalid data for "'//   &
        trim(cNumericFieldNames(4))//'", entered value <= 0.0, but must be > 0 ')
       ErrorsFound=.true.
    ENDIF
    IF (SimpleFluidCooler(FluidCoolerNum)%DesignEnteringAirWetbulbTemp <= 0.0d0) THEN
      CALL ShowSevereError(trim(cCurrentModuleObject)//' = "'//trim(AlphArray(1))//'", invalid data for "'//   &
        trim(cNumericFieldNames(5))//'", entered value <= 0.0, but must be > 0 ')
       ErrorsFound=.true.
    ENDIF
    IF (SimpleFluidCooler(FluidCoolerNum)%DesignEnteringWaterTemp <= &
                SimpleFluidCooler(FluidCoolerNum)%DesignEnteringAirTemp) THEN
      CALL ShowSevereError(trim(cCurrentModuleObject)//'= "'//trim(AlphArray(1))//'",'//  &
        trim(cNumericFieldNames(3))//' must be greater than '//trim(cNumericFieldNames(4))//'.')
       ErrorsFound=.true.
    ENDIF
    IF (SimpleFluidCooler(FluidCoolerNum)%DesignEnteringAirTemp <= &
                SimpleFluidCooler(FluidCoolerNum)%DesignEnteringAirWetbulbTemp) THEN
      CALL ShowSevereError(trim(cCurrentModuleObject)//'= "'//trim(AlphArray(1))//'",'//  &
        trim(cNumericFieldNames(4))//' must be greater than '//trim(cNumericFieldNames(5))//'.')
      ErrorsFound=.true.
    ENDIF
    IF (SimpleFluidCooler(FluidCoolerNum)%HighSpeedAirFlowRate <= 0.0d0 .AND. &
        SimpleFluidCooler(FluidCoolerNum)%HighSpeedAirFlowRate .NE. AutoSize) THEN
      CALL ShowSevereError(trim(cCurrentModuleObject)//' = "'//trim(AlphArray(1))//'", invalid data for "'//   &
        trim(cNumericFieldNames(7))//'", entered value <= 0.0, but must be > 0 for '//  &
        trim(cAlphaFieldNames(4))//' = "'//trim(AlphArray(4))//'".')
      ErrorsFound=.true.
    ENDIF
    IF (SimpleFluidCooler(FluidCoolerNum)%DesignWaterFlowRate <= 0.0d0 .AND. &
        SimpleFluidCooler(FluidCoolerNum)%DesignWaterFlowRate .NE. AutoSize) THEN
      CALL ShowSevereError(trim(cCurrentModuleObject)//' = "'//trim(AlphArray(1))//'", invalid data for "'//   &
        trim(cNumericFieldNames(6))//'", entered value <= 0.0, but must be > 0 for '//  &
        trim(cAlphaFieldNames(4))//' = "'//trim(AlphArray(4))//'".')
      ErrorsFound=.true.
    ENDIF
    IF (SimpleFluidCooler(FluidCoolerNum)%HighSpeedFanPower <= 0.0d0 .AND. &
        SimpleFluidCooler(FluidCoolerNum)%HighSpeedFanPower .NE. AutoSize) THEN
      CALL ShowSevereError(trim(cCurrentModuleObject)//' = "'//trim(AlphArray(1))//'", invalid data for "'//   &
        trim(cNumericFieldNames(8))//'", entered value <= 0.0, but must be > 0 for '//  &
        trim(cAlphaFieldNames(4))//' = "'//trim(AlphArray(4))//'".')
      ErrorsFound=.true.
    ENDIF

!   Check various inputs for both the performance input methods
    IF (SameString(AlphArray(4),'UFactorTimesAreaAndDesignWaterFlowRate')) THEN
      SimpleFluidCooler(FluidCoolerNum)%PerformanceInputMethod_Num = PIM_UFactor
      IF (SimpleFluidCooler(FluidCoolerNum)%HighSpeedFluidCoolerUA <= 0.0d0 .AND. &
          SimpleFluidCooler(FluidCoolerNum)%HighSpeedFluidCoolerUA .NE. AutoSize) THEN
        CALL ShowSevereError(trim(cCurrentModuleObject)//' = "'//trim(AlphArray(1))//'", invalid data for "'//   &
          trim(cNumericFieldNames(1))//'", entered value <= 0.0, but must be > 0 for '//  &
          trim(cAlphaFieldNames(4))//' = "'//trim(AlphArray(4))//'".')
        ErrorsFound=.true.
      ENDIF
    ELSEIF(SameString(AlphArray(4),'NominalCapacity')) THEN
      SimpleFluidCooler(FluidCoolerNum)%PerformanceInputMethod_Num = PIM_NominalCapacity
      IF (SimpleFluidCooler(FluidCoolerNum)%FluidCoolerNominalCapacity <= 0.0d0) THEN
        CALL ShowSevereError(trim(cCurrentModuleObject)//' = "'//trim(AlphArray(1))//'", invalid data for "'//   &
          trim(cNumericFieldNames(2))//'", entered value <= 0.0, but must be > 0 for '//  &
          trim(cAlphaFieldNames(4))//' = "'//trim(AlphArray(4))//'".')
        ErrorsFound=.true.
      ENDIF
      IF (SimpleFluidCooler(FluidCoolerNum)%HighSpeedFluidCoolerUA .NE. 0.0d0) THEN
         IF (SimpleFluidCooler(FluidCoolerNum)%HighSpeedFluidCoolerUA > 0.0d0) THEN
           CALL ShowSevereError(TRIM(cCurrentModuleObject)//'= "'//TRIM(SimpleFluidCooler(FluidCoolerNum)%Name)//&
                                '". Nominal fluid cooler capacity and design fluid cooler UA have been specified.')
         ELSE
           CALL ShowSevereError(TRIM(cCurrentModuleObject)//'= "'//TRIM(SimpleFluidCooler(FluidCoolerNum)%Name)//&
                        '". Nominal fluid cooler capacity has been specified and design fluid cooler UA is being autosized.')
         ENDIF
         CALL ShowContinueError('Design fluid cooler UA field must be left blank '//  &
            'when nominal fluid cooler capacity performance input method is used.')
         ErrorsFound=.true.
      ENDIF
    ELSE ! Fluid cooler performance input method is not specified as a valid "choice"
      CALL ShowSevereError(trim(cCurrentModuleObject)//'= "'//trim(AlphArray(1))//'", invalid '//   &
          trim(cAlphaFieldNames(4))//' = "'//trim(AlphArray(4))//'".')
      CALL ShowContinueError('... must be "UFactorTimesAreaAndDesignWaterFlowRate" or "NominalCapacity".')
      ErrorsFound=.true.
    ENDIF
  END DO  ! End Single-Speed fluid cooler Loop

  cCurrentModuleObject = cFluidCooler_TwoSpeed
  DO TwoSpeedFluidCoolerNumber = 1 , NumTwoSpeedFluidCoolers
    FluidCoolerNum = NumSingleSpeedFluidCoolers + TwoSpeedFluidCoolerNumber
    CALL GetObjectItem(cCurrentModuleObject,TwoSpeedFluidCoolerNumber,AlphArray,NumAlphas,NumArray,NumNums,IOSTAT,  &
                   AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                   AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
    IsNotOK=.false.
    IsBlank=.false.
    CALL VerifyName(AlphArray(1),SimpleFluidCooler%Name,FluidCoolerNum-1,IsNotOK,IsBlank,TRIM(cCurrentModuleObject)//' Name')
    IF (IsNotOK) THEN
      ErrorsFound=.true.
      IF (IsBlank) AlphArray(1)='xxxxx'
    ENDIF
    SimpleFluidCooler(FluidCoolerNum)%Name                              = AlphArray(1)
    SimpleFluidCooler(FluidCoolerNum)%FluidCoolerType                   = TRIM(cCurrentModuleObject)
    SimpleFluidCooler(FluidCoolerNum)%FluidCoolerType_Num               = FluidCooler_TwoSpeed
    SimpleFluidCooler(FluidCoolerNum)%FluidCoolerMassFlowRateMultiplier = 2.5d0
    SimpleFluidCooler(FluidCoolerNum)%WaterInletNodeNum     = &
               GetOnlySingleNode(AlphArray(2),ErrorsFound,TRIM(cCurrentModuleObject),AlphArray(1), &
               NodeType_Water,NodeConnectionType_Inlet, 1, ObjectIsNotParent)
    SimpleFluidCooler(FluidCoolerNum)%WaterOutletNodeNum    = &
               GetOnlySingleNode(AlphArray(3),ErrorsFound,TRIM(cCurrentModuleObject),AlphArray(1), &
               NodeType_Water,NodeConnectionType_Outlet, 1, ObjectIsNotParent)
    CALL TestCompSet(TRIM(cCurrentModuleObject),AlphArray(1),AlphArray(2),AlphArray(3),'Chilled Water Nodes')

    SimpleFluidCooler(FluidCoolerNum)%HighSpeedFluidCoolerUA        = NumArray(1)
    SimpleFluidCooler(FluidCoolerNum)%LowSpeedFluidCoolerUA         = NumArray(2)
    SimpleFluidCooler(FluidCoolerNum)%LowSpeedFluidCoolerUASizingFactor = NumArray(3)
    SimpleFluidCooler(FluidCoolerNum)%FluidCoolerNominalCapacity    = NumArray(4)
    SimpleFluidCooler(FluidCoolerNum)%FluidCoolerLowSpeedNomCap     = NumArray(5)
    SimpleFluidCooler(FluidCoolerNum)%FluidCoolerLowSpeedNomCapSizingFactor = NumArray(6)
    SimpleFluidCooler(FluidCoolerNum)%DesignEnteringWaterTemp       = NumArray(7)
    SimpleFluidCooler(FluidCoolerNum)%DesignEnteringAirTemp         = NumArray(8)
    SimpleFluidCooler(FluidCoolerNum)%DesignEnteringAirWetbulbTemp  = NumArray(9)
    SimpleFluidCooler(FluidCoolerNum)%DesignWaterFlowRate           = NumArray(10)
    SimpleFluidCooler(FluidCoolerNum)%HighSpeedAirFlowRate          = NumArray(11)
    SimpleFluidCooler(FluidCoolerNum)%HighSpeedFanPower             = NumArray(12)
    SimpleFluidCooler(FluidCoolerNum)%LowSpeedAirFlowRate           = NumArray(13)
    SimpleFluidCooler(FluidCoolerNum)%LowSpeedAirFlowRateSizingFactor = NumArray(14)
    SimpleFluidCooler(FluidCoolerNum)%LowSpeedFanPower              = NumArray(15)
    SimpleFluidCooler(FluidCoolerNum)%LowSpeedFanPowerSizingFactor    = NumArray(16)

!   outdoor air inlet node
    IF (AlphArray(5) == Blank) THEN
      SimpleFluidCooler(FluidCoolerNum)%OutdoorAirInletNodeNum = 0
    ELSE
      SimpleFluidCooler(FluidCoolerNum)%OutdoorAirInletNodeNum = &
       GetOnlySingleNode(AlphArray(5),ErrorsFound,TRIM(cCurrentModuleObject),SimpleFluidCooler(FluidCoolerNum)%Name, &
                         NodeType_Air,NodeConnectionType_OutsideAirReference,1,ObjectIsNotParent)
      IF(.not. CheckOutAirNodeNumber(SimpleFluidCooler(FluidCoolerNum)%OutdoorAirInletNodeNum))THEN
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//'= "'//TRIM(SimpleFluidCooler(FluidCoolerNum)%Name)// &
                          '" '//trim(cAlphaFieldNames(5))//'= "'//TRIM(AlphArray(5))//'" not valid.')
        CALL ShowContinueError('...does not appear in an OutdoorAir:NodeList or as an OutdoorAir:Node.')
        ErrorsFound=.true.
      END IF
    ENDIF

!   Design entering water temperature, design entering air temperature and design entering air
!   wetbulb temperature must be specified for the both the performance input methods
    IF (SimpleFluidCooler(FluidCoolerNum)%DesignEnteringWaterTemp <= 0.0d0) THEN
      CALL ShowSevereError(trim(cCurrentModuleObject)//' = "'//trim(AlphArray(1))//'", invalid data for "'//   &
        trim(cNumericFieldNames(7))//'", entered value <= 0.0, but must be > 0 ')
       ErrorsFound=.true.
    ENDIF
    IF (SimpleFluidCooler(FluidCoolerNum)%DesignEnteringAirTemp <= 0.0d0) THEN
      CALL ShowSevereError(trim(cCurrentModuleObject)//' = "'//trim(AlphArray(1))//'", invalid data for "'//   &
        trim(cNumericFieldNames(8))//'", entered value <= 0.0, but must be > 0 ')
      ErrorsFound=.true.
    ENDIF
    IF (SimpleFluidCooler(FluidCoolerNum)%DesignEnteringAirWetbulbTemp <= 0.0d0) THEN
      CALL ShowSevereError(trim(cCurrentModuleObject)//' = "'//trim(AlphArray(1))//'", invalid data for "'//   &
        trim(cNumericFieldNames(9))//'", entered value <= 0.0, but must be > 0 ')
      ErrorsFound=.true.
    ENDIF
    IF (SimpleFluidCooler(FluidCoolerNum)%DesignEnteringWaterTemp <= &
                SimpleFluidCooler(FluidCoolerNum)%DesignEnteringAirTemp) THEN
      CALL ShowSevereError(trim(cCurrentModuleObject)//' = "'//trim(AlphArray(1))//'", '//  &
        trim(cNumericFieldNames(7))//' must be greater than '//trim(cNumericFieldNames(8))//'.')
      ErrorsFound=.true.
    ENDIF
    IF (SimpleFluidCooler(FluidCoolerNum)%DesignEnteringAirTemp <= &
                SimpleFluidCooler(FluidCoolerNum)%DesignEnteringAirWetbulbTemp) THEN
      CALL ShowSevereError(trim(cCurrentModuleObject)//' = "'//trim(AlphArray(1))//'", '//  &
        trim(cNumericFieldNames(8))//' must be greater than '//trim(cNumericFieldNames(9))//'.')
      ErrorsFound=.true.
    ENDIF

!   Check various inputs for both the performance input methods
    IF (SimpleFluidCooler(FluidCoolerNum)%DesignWaterFlowRate <= 0.0d0 .AND. &
        SimpleFluidCooler(FluidCoolerNum)%DesignWaterFlowRate .NE. AutoSize) THEN
      CALL ShowSevereError(trim(cCurrentModuleObject)//'= "'//trim(AlphArray(1))//'", invalid data for "'//   &
        trim(cNumericFieldNames(10))//'", entered value <= 0.0, but must be > 0 for '//  &
        trim(cAlphaFieldNames(4))//'= "'//trim(AlphArray(4))//'".')
      ErrorsFound=.true.
    ENDIF
    IF (SimpleFluidCooler(FluidCoolerNum)%HighSpeedAirFlowRate <= 0.0d0 .AND. &
        SimpleFluidCooler(FluidCoolerNum)%HighSpeedAirFlowRate .NE. AutoSize) THEN
      CALL ShowSevereError(trim(cCurrentModuleObject)//'= "'//trim(AlphArray(1))//'", invalid data for "'//   &
        trim(cNumericFieldNames(11))//'", entered value <= 0.0, but must be > 0 for '//  &
        trim(cAlphaFieldNames(4))//'= "'//trim(AlphArray(4))//'".')
      ErrorsFound=.true.
    ENDIF
    IF (SimpleFluidCooler(FluidCoolerNum)%LowSpeedAirFlowRate <= 0.0d0 .AND. &
        SimpleFluidCooler(FluidCoolerNum)%LowSpeedAirFlowRate .NE. AutoSize) THEN
      CALL ShowSevereError(trim(cCurrentModuleObject)//'= "'//trim(AlphArray(1))//'", invalid data for "'//   &
        trim(cNumericFieldNames(13))//'", entered value <= 0.0, but must be > 0 for '//  &
        trim(cAlphaFieldNames(4))//'= "'//trim(AlphArray(4))//'".')
      ErrorsFound=.true.
    ENDIF
!   High speed air flow rate must be greater than low speed air flow rate.
!   Can't tell yet if autosized, check later in InitFluidCooler.
    IF (SimpleFluidCooler(FluidCoolerNum)%HighSpeedAirFlowRate <=   &
        SimpleFluidCooler(FluidCoolerNum)%LowSpeedAirFlowRate .and. &
        SimpleFluidCooler(FluidCoolerNum)%HighSpeedAirFlowRate .NE. AutoSize) THEN
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//'= "'//TRIM(SimpleFluidCooler(FluidCoolerNum)%Name)//&
                           '". Fluid cooler air flow rate at low fan speed must be less than the air '// &
                               'flow rate at high fan speed.')
      ErrorsFound=.true.
    ENDIF
    IF (SimpleFluidCooler(FluidCoolerNum)%HighSpeedFanPower <= 0.0d0 .AND. &
        SimpleFluidCooler(FluidCoolerNum)%HighSpeedFanPower .NE. AutoSize) THEN
      CALL ShowSevereError(trim(cCurrentModuleObject)//' = "'//trim(AlphArray(1))//'", invalid data for "'//   &
        trim(cNumericFieldNames(12))//'", entered value <= 0.0, but must be > 0 for '//  &
        trim(cAlphaFieldNames(4))//' = "'//trim(AlphArray(4))//'".')
      ErrorsFound=.true.
    ENDIF
    IF (SimpleFluidCooler(FluidCoolerNum)%LowSpeedFanPower <= 0.0d0 .AND. &
        SimpleFluidCooler(FluidCoolerNum)%LowSpeedFanPower .NE. AutoSize) THEN
      CALL ShowSevereError(trim(cCurrentModuleObject)//' = "'//trim(AlphArray(1))//'", invalid data for "'//   &
        trim(cNumericFieldNames(15))//'", entered value <= 0.0, but must be > 0 for '//  &
        trim(cAlphaFieldNames(4))//' = "'//trim(AlphArray(4))//'".')
      ErrorsFound=.true.
    ENDIF
    IF (SimpleFluidCooler(FluidCoolerNum)%HighSpeedFanPower <=   &
        SimpleFluidCooler(FluidCoolerNum)%LowSpeedFanPower .and. &
        SimpleFluidCooler(FluidCoolerNum)%HighSpeedFanPower .NE. AutoSize) THEN
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//'= "'//TRIM(SimpleFluidCooler(FluidCoolerNum)%Name)//&
                           '". Fluid cooler low speed fan power must be less than high speed fan power.')
      ErrorsFound=.true.
    ENDIF

    IF (SameString(AlphArray(4),'UFactorTimesAreaAndDesignWaterFlowRate')) THEN
      SimpleFluidCooler(FluidCoolerNum)%PerformanceInputMethod_Num = PIM_UFactor
      IF (SimpleFluidCooler(FluidCoolerNum)%HighSpeedFluidCoolerUA <= 0.0d0 .AND. &
          SimpleFluidCooler(FluidCoolerNum)%HighSpeedFluidCoolerUA .NE. AutoSize) THEN
        CALL ShowSevereError(trim(cCurrentModuleObject)//' = "'//trim(AlphArray(1))//'", invalid data for "'//   &
          trim(cNumericFieldNames(1))//'", entered value <= 0.0, but must be > 0 for '//  &
          trim(cAlphaFieldNames(4))//' = "'//trim(AlphArray(4))//'".')
        ErrorsFound=.true.
      ENDIF
      IF (SimpleFluidCooler(FluidCoolerNum)%LowSpeedFluidCoolerUA <= 0.0d0 .AND. &
          SimpleFluidCooler(FluidCoolerNum)%LowSpeedFluidCoolerUA .NE. AutoSize) THEN
        CALL ShowSevereError(trim(cCurrentModuleObject)//' = "'//trim(AlphArray(1))//'", invalid data for "'//   &
          trim(cNumericFieldNames(2))//'", entered value <= 0.0, but must be > 0 for '//  &
          trim(cAlphaFieldNames(4))//' = "'//trim(AlphArray(4))//'".')
        ErrorsFound=.true.
      ENDIF
      IF (SimpleFluidCooler(FluidCoolerNum)%HighSpeedFluidCoolerUA <=   &
          SimpleFluidCooler(FluidCoolerNum)%LowSpeedFluidCoolerUA .and. &
          SimpleFluidCooler(FluidCoolerNum)%HighSpeedFluidCoolerUA .NE. AutoSize) THEN
         CALL ShowSevereError(TRIM(cCurrentModuleObject)//'= "'//TRIM(SimpleFluidCooler(FluidCoolerNum)%Name)//&
                              '". Fluid cooler UA at low fan speed must be less than the fluid cooler UA at high fan speed.')
         ErrorsFound=.true.
      ENDIF
    ELSEIF(SameString(AlphArray(4),'NominalCapacity')) THEN
      SimpleFluidCooler(FluidCoolerNum)%PerformanceInputMethod_Num = PIM_NominalCapacity
      IF (SimpleFluidCooler(FluidCoolerNum)%FluidCoolerNominalCapacity <= 0.0d0) THEN
        CALL ShowSevereError(trim(cCurrentModuleObject)//' = "'//trim(AlphArray(1))//'", invalid data for "'//   &
          trim(cNumericFieldNames(4))//'", entered value <= 0.0, but must be > 0 for '//  &
          trim(cAlphaFieldNames(4))//'= "'//trim(AlphArray(4))//'".')
        ErrorsFound=.true.
      ENDIF
      IF (SimpleFluidCooler(FluidCoolerNum)%FluidCoolerLowSpeedNomCap <= 0.0d0) THEN
        CALL ShowSevereError(trim(cCurrentModuleObject)//' = "'//trim(AlphArray(1))//'", invalid data for "'//   &
          trim(cNumericFieldNames(5))//'", entered value <= 0.0, but must be > 0 for '//  &
          trim(cAlphaFieldNames(4))//'= "'//trim(AlphArray(4))//'".')
        ErrorsFound=.true.
      ENDIF
      IF (SimpleFluidCooler(FluidCoolerNum)%HighSpeedFluidCoolerUA .NE. 0.0d0) THEN
         IF (SimpleFluidCooler(FluidCoolerNum)%HighSpeedFluidCoolerUA > 0.0d0) THEN
           CALL ShowSevereError(TRIM(cCurrentModuleObject)//'= "'//TRIM(SimpleFluidCooler(FluidCoolerNum)%Name)//&
                                '". Nominal capacity input method and fluid cooler UA at high fan speed have been specified.')
         ELSE
           CALL ShowSevereError(TRIM(cCurrentModuleObject)//'= "'//TRIM(SimpleFluidCooler(FluidCoolerNum)%Name)//&
              '". Nominal capacity input method has been specified and fluid cooler UA at high fan speed is being autosized.')
         ENDIF
         CALL ShowContinueError('Fluid cooler UA at high fan speed must be left blank '//  &
            'when nominal fluid cooler capacity performance input method'// &
                                ' is used.')
         ErrorsFound=.true.
      ENDIF
      IF (SimpleFluidCooler(FluidCoolerNum)%LowSpeedFluidCoolerUA .NE. 0.0d0) THEN
         IF (SimpleFluidCooler(FluidCoolerNum)%LowSpeedFluidCoolerUA > 0.0d0) THEN
           CALL ShowSevereError(TRIM(cCurrentModuleObject)//'= "'//TRIM(SimpleFluidCooler(FluidCoolerNum)%Name)//&
                                '". Nominal capacity input method and fluid cooler UA at low fan speed have been specified.')
         ELSE
           CALL ShowSevereError(TRIM(cCurrentModuleObject)//'= "'//TRIM(SimpleFluidCooler(FluidCoolerNum)%Name)//&
              '". Nominal capacity input method has been specified and fluid cooler UA at low fan speed is being autosized.')
         ENDIF
         CALL ShowContinueError('Fluid cooler UA at low fan speed must be left blank '//  &
            'when nominal fluid cooler capacity performance input method is used.')
         ErrorsFound=.true.
      ENDIF
      IF (SimpleFluidCooler(FluidCoolerNum)%FluidCoolerLowSpeedNomCap >=   &
         SimpleFluidCooler(FluidCoolerNum)%FluidCoolerNominalCapacity) THEN
         CALL ShowSevereError(TRIM(cCurrentModuleObject)//' = "'//TRIM(SimpleFluidCooler(FluidCoolerNum)%Name)//&
                              '". Low-speed nominal capacity must be less than the high-speed nominal capacity.')
         ErrorsFound=.true.
      END IF
    ELSE ! Fluid cooler performance input method is not specified as a valid "choice"
      CALL ShowSevereError(trim(cCurrentModuleObject)//'= "'//trim(AlphArray(1))//'", invalid '//   &
          trim(cAlphaFieldNames(4))//'= "'//trim(AlphArray(4))//'".')
      CALL ShowContinueError('... must be "UFactorTimesAreaAndDesignWaterFlowRate" or "NominalCapacity".')
      ErrorsFound=.true.
    ENDIF
  END DO  ! End Two-Speed Fluid Cooler Loop

  IF (ErrorsFound) THEN
    CALL ShowFatalError('Errors found in getting fluid cooler input.')
  ENDIF

! Set up output variables, CurrentModuleObject='FluidCooler:SingleSpeed'
  DO FluidCoolerNum = 1, NumSingleSpeedFluidCoolers
    CALL SetupOutputVariable('Cooling Tower Inlet Temperature [C]', &
          SimpleFluidCoolerReport(FluidCoolerNum)%InletWaterTemp,'System','Average',SimpleFluidCooler(FluidCoolerNum)%Name)
    CALL SetupOutputVariable('Cooling Tower Outlet Temperature [C]', &
          SimpleFluidCoolerReport(FluidCoolerNum)%OutletWaterTemp,'System','Average',SimpleFluidCooler(FluidCoolerNum)%Name)
    CALL SetupOutputVariable('Cooling Tower Mass Flow Rate [kg/s]', &
          SimpleFluidCoolerReport(FluidCoolerNum)%WaterMassFlowRate,'System','Average',SimpleFluidCooler(FluidCoolerNum)%Name)
    CALL SetupOutputVariable('Cooling Tower Heat Transfer Rate [W]', &
          SimpleFluidCoolerReport(FluidCoolerNum)%Qactual,'System','Average',SimpleFluidCooler(FluidCoolerNum)%Name)
    CALL SetupOutputVariable('Cooling Tower Fan Electric Power [W]', &
          SimpleFluidCoolerReport(FluidCoolerNum)%FanPower,'System','Average',SimpleFluidCooler(FluidCoolerNum)%Name)
    CALL SetupOutputVariable('Cooling Tower Fan Electric Energy [J]', &
          SimpleFluidCoolerReport(FluidCoolerNum)%FanEnergy,'System','Sum',SimpleFluidCooler(FluidCoolerNum)%Name, &
          ResourceTypeKey='Electric',EndUseKey='HeatRejection',GroupKey='Plant')
  END DO

  ! CurrentModuleObject='FluidCooler:TwoSpeed'
  DO FluidCoolerNum = NumSingleSpeedFluidCoolers+1, NumSingleSpeedFluidCoolers+NumTwoSpeedFluidCoolers
    CALL SetupOutputVariable('Cooling Tower Inlet Temperature [C]', &
          SimpleFluidCoolerReport(FluidCoolerNum)%InletWaterTemp,'System','Average',SimpleFluidCooler(FluidCoolerNum)%Name)
    CALL SetupOutputVariable('Cooling Tower Outlet Temperature [C]', &
          SimpleFluidCoolerReport(FluidCoolerNum)%OutletWaterTemp,'System','Average',SimpleFluidCooler(FluidCoolerNum)%Name)
    CALL SetupOutputVariable('Cooling Tower Mass Flow Rate [kg/s]', &
          SimpleFluidCoolerReport(FluidCoolerNum)%WaterMassFlowRate,'System','Average',SimpleFluidCooler(FluidCoolerNum)%Name)
    CALL SetupOutputVariable('Cooling Tower Heat Transfer Rate [W]', &
          SimpleFluidCoolerReport(FluidCoolerNum)%Qactual,'System','Average',SimpleFluidCooler(FluidCoolerNum)%Name)
    CALL SetupOutputVariable('Cooling Tower Fan Electric Power [W]', &
          SimpleFluidCoolerReport(FluidCoolerNum)%FanPower,'System','Average',SimpleFluidCooler(FluidCoolerNum)%Name)
    CALL SetupOutputVariable('Cooling Tower Fan Electric Energy [J]', &
          SimpleFluidCoolerReport(FluidCoolerNum)%FanEnergy,'System','Sum',SimpleFluidCooler(FluidCoolerNum)%Name, &
          ResourceTypeKey='Electric',EndUseKey='HeatRejection',GroupKey='Plant')
  END DO

RETURN
END SUBROUTINE GetFluidCoolerInput
! End of Get Input subroutines for the CondenserLoopFluidCoolers Module
!******************************************************************************


! Beginning Initialization Section for the CondenserLoopFluidCoolers Module
!******************************************************************************

SUBROUTINE InitSimVars

          ! SUBROUTINE INFORMATION:
          !       AUTHOR:          Chandan Sharma
          !       DATE WRITTEN:    August 2008
          !       MODIFIED         na
          !       RE-ENGINEERED    na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Initialize the simulation variables.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

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
          ! na

          !INITIALIZE MODULE LEVEL VARIABLES

    InletWaterTemp           = 0.0d0    ! CW temperature at fluid cooler inlet
    OutletWaterTemp          = 0.0d0    ! CW temperature at fluid cooler outlet
    WaterInletNode           = 0      ! Node number at fluid cooler inlet
    WaterOutletNode          = 0      ! Node number at fluid cooler outlet
    WaterMassFlowRate        = 0.0d0    ! WaterMassFlowRate through fluid cooler
!    FluidCoolerMassFlowRateMax     = 0.0    ! Max Hardware Mass Flow Rate
!    FluidCoolerMassFlowRateMin     = 0.0    ! Min Hardware Mass Flow Rate
!    LoopMassFlowRateMaxAvail = 0.0    ! Max Loop Mass Flow Rate available
!    LoopMassFlowRateMinAvail = 0.0    ! Min Loop Mass Flow Rate available
    Qactual                  = 0.0d0    ! Fluid cooler heat transfer
    FanPower                 = 0.0d0    ! Fluid cooler fan power used

RETURN
END SUBROUTINE InitSimVars

SUBROUTINE InitFluidCooler(FluidCoolerNum, RunFlag)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Chandan Sharma
          !       DATE WRITTEN   August 2008
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is for initializations of the fluid cooler components and for
          ! final checking of fluid cooler inputs (post autosizing)

          ! METHODOLOGY EMPLOYED:
          ! Uses the status flags to trigger initializations.

          ! REFERENCES:
          ! Based on InitTower subroutine by Don Shirey Sept/Oct 2002, F Buhl Oct 2002

          ! USE STATEMENTS:
  USE DataGlobals,     ONLY: BeginEnvrnFlag
!  USE Psychrometrics,  ONLY: PsyTwbFnTdbWPb
!  USE FluidProperties, ONLY : GetDensityGlycol
  USE InputProcessor,  ONLY : SameString
  USE DataPlant,       ONLY : TypeOf_FluidCooler_SingleSpd, TypeOf_FluidCooler_TwoSpd, ScanPlantLoopsForObject, &
                              PlantSizeNotComplete, PlantSizesOkayToFinalize
  USE PlantUtilities,  ONLY: InitComponentNodes, SetComponentFlowRate, RegulateCondenserCompFlowReqOp

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN) :: FluidCoolerNum   ! Number of the current fluid cooler being simulated
  LOGICAL, INTENT (IN) :: RunFlag          ! TRUE if fluid cooler is ON

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  LOGICAL, SAVE                           :: ErrorsFound=.false. ! Flag if input data errors are found
  LOGICAL, SAVE                           :: MyOneTimeFlag = .true.
  LOGICAL, ALLOCATABLE,Save, DIMENSION(:) :: MyEnvrnFlag
  LOGICAL, ALLOCATABLE,Save, DIMENSION(:) :: OneTimeFlagForEachFluidCooler
  LOGICAL                                 :: FatalError
  INTEGER   :: TypeOf_Num
  INTEGER   :: LoopNum
  INTEGER   :: LoopSideNum
  INTEGER   :: BranchIndex
  INTEGER   :: CompIndex
  REAL(r64) :: rho           ! local density of fluid

  ! Do the one time initializations
  IF (MyOneTimeFlag) THEN

    ALLOCATE(MyEnvrnFlag(NumSimpleFluidCoolers))
    ALLOCATE(OneTimeFlagForEachFluidCooler(NumSimpleFluidCoolers))

    OneTimeFlagForEachFluidCooler = .TRUE.
    MyEnvrnFlag = .TRUE.
    MyOneTimeFlag = .false.

  END IF

  IF (OneTimeFlagForEachFluidCooler(FluidCoolerNum)) THEN

    IF (SimpleFluidCooler(FluidCoolerNum)%FluidCoolerType_Num == FluidCooler_SingleSpeed) THEN
      TypeOf_Num = TypeOf_FluidCooler_SingleSpd
    ELSEIF (SimpleFluidCooler(FluidCoolerNum)%FluidCoolerType_Num == FluidCooler_TwoSpeed) THEN
      TypeOf_Num = TypeOf_FluidCooler_TwoSpd
    ENDIF

    ! Locate the tower on the plant loops for later usage
    CALL ScanPlantLoopsForObject(SimpleFluidCooler(FluidCoolerNum)%Name, &
                                 TypeOf_Num, &
                                 SimpleFluidCooler(FluidCoolerNum)%LoopNum, &
                                 SimpleFluidCooler(FluidCoolerNum)%LoopSideNum, &
                                 SimpleFluidCooler(FluidCoolerNum)%BranchNum, &
                                 SimpleFluidCooler(FluidCoolerNum)%CompNum,  &
                                 errFlag=ErrorsFound)

    IF (ErrorsFound) THEN
      CALL ShowFatalError('InitFluidCooler: Program terminated due to previous condition(s).')
    ENDIF

    OneTimeFlagForEachFluidCooler(FluidCoolerNum) = .FALSE.

  END IF

  ! Begin environment initializations
  IF(MyEnvrnFlag(FluidCoolerNum) .AND. BeginEnvrnFlag .AND. (PlantSizesOkayToFinalize))Then
    IF (PlantSizeNotComplete) CALL SizeFluidCooler(FluidCoolerNum)
    rho = GetDensityGlycol(PlantLoop(SimpleFluidCooler(FluidCoolerNum)%LoopNum)%FluidName,  &
                                InitConvTemp, &
                                PlantLoop(SimpleFluidCooler(FluidCoolerNum)%LoopNum)%FluidIndex,&
                                'InitFluidCooler')
    SimpleFluidCooler(FluidCoolerNum)%DesWaterMassFlowRate = SimpleFluidCooler(FluidCoolerNum)%DesignWaterFlowRate * rho
    CALL InitComponentNodes(0.0D0,  SimpleFluidCooler(FluidCoolerNum)%DesWaterMassFlowRate , &
                                    SimpleFluidCooler(FluidCoolerNum)%WaterInletNodeNum,     &
                                    SimpleFluidCooler(FluidCoolerNum)%WaterOutletNodeNum,    &
                                    SimpleFluidCooler(FluidCoolerNum)%LoopNum,               &
                                    SimpleFluidCooler(FluidCoolerNum)%LoopSideNum,           &
                                    SimpleFluidCooler(FluidCoolerNum)%BranchNum,             &
                                    SimpleFluidCooler(FluidCoolerNum)%CompNum)
    MyEnvrnFlag(FluidCoolerNum) = .false.
  END IF

  IF (.not. BeginEnvrnFlag) THEN
    MyEnvrnFlag(FluidCoolerNum)=.true.
  ENDIF

  ! Each time initializations
  WaterInletNode = SimpleFluidCooler(FluidCoolerNum)%WaterInletNodeNum
  SimpleFluidCoolerInlet(FluidCoolerNum)%WaterTemp  = Node(WaterInletNode)%Temp

  IF (SimpleFluidCooler(FluidCoolerNum)%OutdoorAirInletNodeNum /= 0) THEN
    SimpleFluidCoolerInlet(FluidCoolerNum)%AirTemp    = Node(SimpleFluidCooler(FluidCoolerNum)%OutdoorAirInletNodeNum)%Temp
    SimpleFluidCoolerInlet(FluidCoolerNum)%AirHumRat  = Node(SimpleFluidCooler(FluidCoolerNum)%OutdoorAirInletNodeNum)%HumRat
    SimpleFluidCoolerInlet(FluidCoolerNum)%AirPress   = Node(SimpleFluidCooler(FluidCoolerNum)%OutdoorAirInletNodeNum)%Press
    SimpleFluidCoolerInlet(FluidCoolerNum)%AirWetBulb = &
                                                 Node(SimpleFluidCooler(FluidCoolerNum)%OutdoorAirInletNodeNum)%OutAirWetBulb
  ELSE
    SimpleFluidCoolerInlet(FluidCoolerNum)%AirTemp    = OutDryBulbTemp
    SimpleFluidCoolerInlet(FluidCoolerNum)%AirHumRat  = OutHumRat
    SimpleFluidCoolerInlet(FluidCoolerNum)%AirPress   = OutBaroPress
    SimpleFluidCoolerInlet(FluidCoolerNum)%AirWetBulb = OutWetBulbTemp
  ENDIF

    LoopNum     = SimpleFluidCooler(FluidCoolerNum)%LoopNum
    LoopSideNum = SimpleFluidCooler(FluidCoolerNum)%LoopSideNum
    BranchIndex = SimpleFluidCooler(FluidCoolerNum)%BranchNum
    CompIndex   = SimpleFluidCooler(FluidCoolerNum)%CompNum

    WaterMassFlowRate = RegulateCondenserCompFlowReqOp(SimpleFluidCooler(FluidCoolerNum)%LoopNum,               &
                                                SimpleFluidCooler(FluidCoolerNum)%LoopSideNum,           &
                                                SimpleFluidCooler(FluidCoolerNum)%BranchNum,             &
                                                SimpleFluidCooler(FluidCoolerNum)%CompNum,     &
                                                SimpleFluidCooler(FluidCoolerNum)%DesWaterMassFlowRate * &
                                                SimpleFluidCooler(FluidCoolerNum)%FluidCoolerMassFlowRateMultiplier)


    CALL SetComponentFlowRate(WaterMassFlowRate, &
                              SimpleFluidCooler(FluidCoolerNum)%WaterInletNodeNum,     &
                              SimpleFluidCooler(FluidCoolerNum)%WaterOutletNodeNum,    &
                              SimpleFluidCooler(FluidCoolerNum)%LoopNum,               &
                              SimpleFluidCooler(FluidCoolerNum)%LoopSideNum,           &
                              SimpleFluidCooler(FluidCoolerNum)%BranchNum,             &
                              SimpleFluidCooler(FluidCoolerNum)%CompNum)
  RETURN
END SUBROUTINE InitFluidCooler

SUBROUTINE SizeFluidCooler(FluidCoolerNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Chandan Sharma
          !       DATE WRITTEN   August 2008
          !       MODIFIED       April 2010, Chandan Sharma, FSEC
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is for sizing fluid cooler Components for which capacities and flow rates
          ! have not been specified in the input. This subroutine also calculates fluid cooler UA if the user
          ! has specified fluid cooler performance via the "Nominal Capacity" method.

          ! METHODOLOGY EMPLOYED:
          ! Obtains condenser flow rate from the plant sizing array. If fluid cooler performance is specified
          ! via the "Nominal Capacity" method, the water flow rate is directly proportional to capacity.

          ! REFERENCES:
          ! Based on SizeTower by Don Shirey, Sept/Oct 2002; Richard Raustad, Feb 2005

          ! USE STATEMENTS:
  USE DataSizing
  USE DataPlant,       ONLY:  PlantSizesOkayToFinalize

  USE DataIPShortCuts  ! Data for field names, blank numerics
  USE General,         ONLY: SolveRegulaFalsi, RoundSigDigits
  USE PlantUtilities,  ONLY: RegisterPlantCompDesignFlow
  USE ReportSizingManager, ONLY: ReportSizingOutput
  USE OutputReportPredefined
  USE InputProcessor , ONLY : SameString
  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: FluidCoolerNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
  INTEGER, PARAMETER          :: MaxIte     =  500         ! Maximum number of iterations
  REAL(r64), PARAMETER        :: Acc        =  0.0001d0    ! Accuracy of result
  CHARACTER(len=*), PARAMETER :: CalledFrom = 'SizeFluidCooler'

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER             :: PltSizCondNum          ! Plant Sizing index for condenser loop
  INTEGER             :: SolFla                 ! Flag of solver
  REAL(r64)           :: DesFluidCoolerLoad     ! Design fluid cooler load [W]
  REAL(r64)           :: UA0                    ! Lower bound for UA [W/C]
  REAL(r64)           :: UA1                    ! Upper bound for UA [W/C]
  REAL(r64)           :: UA                     ! Calculated UA value
  REAL(r64)           :: OutWaterTempAtUA0      ! Water outlet temperature at UA0
  REAL(r64)           :: OutWaterTempAtUA1      ! Water outlet temperature at UA1
  REAL(r64), DIMENSION(5)      :: Par           ! Parameter array need for RegulaFalsi routine
  CHARACTER(len=MaxNameLength) :: equipName
  REAL(r64)                    :: Cp              ! local specific heat for fluid
  REAL(r64)                    :: rho             ! local density for fluid
  REAL(r64)           :: tmpDesignWaterFlowRate ! local temporary for water volume flow rate
  REAL(r64)           :: tmpHighSpeedFanPower  !local temporary for high speed fan power
  REAL(r64)           :: tmpHighSpeedAirFlowRate ! local temporary for high speed air flow rate
  REAL(r64)           :: tmpHighSpeedEvapFluidCoolerUA ! local temporary for high speed cooler UA
  LOGICAL             :: ErrorsFound

  PltSizCondNum = 0
  DesFluidCoolerLoad = 0.0d0
  tmpDesignWaterFlowRate = SimpleFluidCooler(FluidCoolerNum)%DesignWaterFlowRate
  tmpHighSpeedFanPower   = SimpleFluidCooler(FluidCoolerNum)%HighSpeedFanPower
  tmpHighSpeedAirFlowRate= SimpleFluidCooler(FluidCoolerNum)%HighSpeedAirFlowRate
  tmpHighSpeedEvapFluidCoolerUA = SimpleFluidCooler(FluidCoolerNum)%HighSpeedFluidCoolerUA
  ! Find the appropriate Plant Sizing object
  PltSizCondNum = PlantLoop(SimpleFluidCooler(FluidCoolerNum)%LoopNum)%PlantSizNum


  IF(SimpleFluidCooler(FluidCoolerNum)%DesignWaterFlowRate == AutoSize) THEN
    IF (PltSizCondNum > 0) THEN
      IF (PlantSizData(PltSizCondNum)%DesVolFlowRate >= SmallWaterVolFlow) THEN
        tmpDesignWaterFlowRate =  PlantSizData(PltSizCondNum)%DesVolFlowRate
        IF (PlantSizesOkayToFinalize) SimpleFluidCooler(FluidCoolerNum)%DesignWaterFlowRate = tmpDesignWaterFlowRate
      ELSE
        tmpDesignWaterFlowRate = 0.d0
        IF (PlantSizesOkayToFinalize) SimpleFluidCooler(FluidCoolerNum)%DesignWaterFlowRate = tmpDesignWaterFlowRate
      ENDIF
        IF (PlantSizesOkayToFinalize)   &
           CALL ReportSizingOutput(SimpleFluidCooler(FluidCoolerNum)%FluidCoolerType, SimpleFluidCooler(FluidCoolerNum)%Name, &
                         'Autosized design water flow rate [m3/s]', SimpleFluidCooler(FluidCoolerNum)%DesignWaterFlowRate)
    ELSE
      CALL ShowSevereError('Autosizing error for fluid cooler object = '//TRIM(SimpleFluidCooler(FluidCoolerNum)%Name))
      CALL ShowFatalError('Autosizing of fluid cooler condenser flow rate requires a loop Sizing:Plant object.')
    ENDIF
    ! This conditional statement is to trap when the user specified Condenser/Fluid Cooler water design setpoint
    ! temperature is less than design inlet air dry bulb temperature
    IF ( PlantSizData(PltSizCondNum)%ExitTemp <= &
                      SimpleFluidCooler(FluidCoolerNum)%DesignEnteringAirTemp ) THEN
      CALL ShowSevereError('Error when autosizing the UA value for fluid cooler = '&
                            //TRIM(SimpleFluidCooler(FluidCoolerNum)%Name)//'.' )
      CALL ShowContinueError('Design Loop Exit Temperature ('// TRIM(RoundSigDigits(PlantSizData(PltSizCondNum)%ExitTemp,2)) // &
                             ' C) must be greater than design entering air dry-bulb temperature ('// &
                             TRIM(RoundSigDigits(SimpleFluidCooler(FluidCoolerNum)%DesignEnteringAirTemp,2)) //&
                             ' C) when autosizing the fluid cooler UA.')
      CALL ShowContinueError('It is recommended that the Design Loop Exit Temperature = design inlet air dry-bulb temp '// &
                             'plus the Fluid Cooler design approach temperature (e.g., 4 C).')
      CALL ShowContinueError('If using HVACTemplate:Plant:ChilledWaterLoop, then check that input field ' // &
                             'Condenser Water Design Setpoint must be > design inlet air dry-bulb temp if autosizing '// &
                             'the Fluid Cooler.')
      CALL ShowFatalError('Review and revise design input values as appropriate.')
    ENDIF
  ENDIF

  CALL RegisterPlantCompDesignFlow(SimpleFluidCooler(FluidCoolerNum)%WaterInletNodeNum, tmpDesignWaterFlowRate)

  IF (SimpleFluidCooler(FluidCoolerNum)%PerformanceInputMethod_Num == PIM_UFactor .and.  &
      SimpleFluidCooler(FluidCoolerNum)%HighSpeedFluidCoolerUA /= AutoSize) THEN
    IF (PltSizCondNum > 0) THEN
      rho = GetDensityGlycol(PlantLoop(SimpleFluidCooler(FluidCoolerNum)%LoopNum)%FluidName,  &
                             InitConvTemp, &
                             PlantLoop(SimpleFluidCooler(FluidCoolerNum)%LoopNum)%FluidIndex,&
                             'SizeFluidCooler')
      Cp = GetSpecificHeatGlycol(PlantLoop(SimpleFluidCooler(FluidCoolerNum)%LoopNum)%FluidName,  &
                                 PlantSizData(PltSizCondNum)%ExitTemp,                      &
                                 PlantLoop(SimpleFluidCooler(FluidCoolerNum)%LoopNum)%FluidIndex, &
                                 'SizeFluidCooler')
      DesFluidCoolerLoad = rho * Cp * tmpDesignWaterFlowRate * PlantSizData(PltSizCondNum)%DeltaT
      SimpleFluidCooler(FluidCoolerNum)%FluidCoolerNominalCapacity = DesFluidCoolerLoad
    ELSE
      SimpleFluidCooler(FluidCoolerNum)%FluidCoolerNominalCapacity = 0.0d0
    ENDIF
  END IF

  IF (SimpleFluidCooler(FluidCoolerNum)%HighSpeedFanPower == AutoSize) THEN
    ! We assume the nominal fan power is 0.0105 times the design load
    IF (SimpleFluidCooler(FluidCoolerNum)%PerformanceInputMethod_Num == PIM_NominalCapacity) THEN
      tmpHighSpeedFanPower = 0.0105d0 * SimpleFluidCooler(FluidCoolerNum)%FluidCoolerNominalCapacity
      IF (PlantSizesOkayToFinalize) SimpleFluidCooler(FluidCoolerNum)%HighSpeedFanPower = tmpHighSpeedFanPower
    ELSE
      IF(DesFluidCoolerLoad .GT. 0.0d0) THEN
        tmpHighSpeedFanPower =   0.0105d0 * DesFluidCoolerLoad
        IF (PlantSizesOkayToFinalize) SimpleFluidCooler(FluidCoolerNum)%HighSpeedFanPower = tmpHighSpeedFanPower
      ELSEIF (PltSizCondNum > 0) THEN
        IF (PlantSizData(PltSizCondNum)%DesVolFlowRate >= SmallWaterVolFlow) THEN
          ! This conditional statement is to trap when the user specified Condenser/Fluid Cooler water design setpoint
          ! temperature is less than design inlet air dry bulb temperature
          IF ( PlantSizData(PltSizCondNum)%ExitTemp <= &
                            SimpleFluidCooler(FluidCoolerNum)%DesignEnteringAirTemp ) THEN
            CALL ShowSevereError('Error when autosizing the UA value for fluid cooler = '&
                                //TRIM(SimpleFluidCooler(FluidCoolerNum)%Name)//'.' )
            CALL ShowContinueError('Design Loop Exit Temperature ('// &
                                   TRIM(RoundSigDigits(PlantSizData(PltSizCondNum)%ExitTemp,2)) // &
                                   ' C) must be greater than design entering air dry-bulb temperature ('// &
                                    TRIM(RoundSigDigits(SimpleFluidCooler(FluidCoolerNum)%DesignEnteringAirTemp,2)) //&
                                   ' C) when autosizing the fluid cooler UA.')
            CALL ShowContinueError('It is recommended that the Design Loop Exit Temperature = design inlet air '// &
                                   'dry-bulb temp plus the Fluid Cooler design approach temperature (e.g., 4 C).')
            CALL ShowContinueError('If using HVACTemplate:Plant:ChilledWaterLoop, then check that input field ' // &
                                   'Condenser Water Design Setpoint must be > design inlet air dry-bulb temp if autosizing '// &
                                   'the Fluid Cooler.')
            CALL ShowFatalError('Review and revise design input values as appropriate.')
          ENDIF
          rho = GetDensityGlycol(PlantLoop(SimpleFluidCooler(FluidCoolerNum)%LoopNum)%FluidName,  &
                                 InitConvTemp, &
                                 PlantLoop(SimpleFluidCooler(FluidCoolerNum)%LoopNum)%FluidIndex,&
                                 'SizeFluidCooler')
          Cp = GetSpecificHeatGlycol(PlantLoop(SimpleFluidCooler(FluidCoolerNum)%LoopNum)%FluidName,  &
                                     PlantSizData(PltSizCondNum)%ExitTemp,                      &
                                     PlantLoop(SimpleFluidCooler(FluidCoolerNum)%LoopNum)%FluidIndex, &
                                     'SizeFluidCooler')
          DesFluidCoolerLoad = rho * Cp * tmpDesignWaterFlowRate * PlantSizData(PltSizCondNum)%DeltaT
          tmpHighSpeedFanPower = 0.0105d0 * DesFluidCoolerLoad
          IF (PlantSizesOkayToFinalize) SimpleFluidCooler(FluidCoolerNum)%HighSpeedFanPower = tmpHighSpeedFanPower
        ELSE
          tmpHighSpeedFanPower = 0.d0
          IF (PlantSizesOkayToFinalize) SimpleFluidCooler(FluidCoolerNum)%HighSpeedFanPower = tmpHighSpeedFanPower
        ENDIF
      ELSE
        CALL ShowSevereError('Autosizing of fluid cooler fan power requires a loop Sizing:Plant object.')
        CALL ShowFatalError(' Occurs in fluid cooler object = '//TRIM(SimpleFluidCooler(FluidCoolerNum)%Name))
      ENDIF
    ENDIF
    IF (SimpleFluidCooler(FluidCoolerNum)%FluidCoolerType_Num == FluidCooler_SingleSpeed) THEN
      IF (PlantSizesOkayToFinalize) CALL ReportSizingOutput(SimpleFluidCooler(FluidCoolerNum)%FluidCoolerType, &
                              SimpleFluidCooler(FluidCoolerNum)%Name, &
                             'Fan Power at Design Air Flow Rate [W]', SimpleFluidCooler(FluidCoolerNum)%HighSpeedFanPower)
    ELSEIF (SimpleFluidCooler(FluidCoolerNum)%FluidCoolerType_Num == FluidCooler_TwoSpeed) THEN
      IF (PlantSizesOkayToFinalize) CALL ReportSizingOutput(SimpleFluidCooler(FluidCoolerNum)%FluidCoolerType, &
                              SimpleFluidCooler(FluidCoolerNum)%Name, &
                             'Fan Power at High Fan Speed [W]', SimpleFluidCooler(FluidCoolerNum)%HighSpeedFanPower)
    ENDIF
  ENDIF

  IF (SimpleFluidCooler(FluidCoolerNum)%HighSpeedAirFlowRate == AutoSize) THEN
    IF (SimpleFluidCooler(FluidCoolerNum)%PerformanceInputMethod_Num == PIM_NominalCapacity) THEN
      tmpHighSpeedAirFlowRate = SimpleFluidCooler(FluidCoolerNum)%FluidCoolerNominalCapacity / &
                                                           (SimpleFluidCooler(FluidCoolerNum)%DesignEnteringWaterTemp - &
                                                            SimpleFluidCooler(FluidCoolerNum)%DesignEnteringAirTemp) * 4.0d0
      IF (PlantSizesOkayToFinalize) SimpleFluidCooler(FluidCoolerNum)%HighSpeedAirFlowRate = tmpHighSpeedAirFlowRate
    ELSE
      IF(DesFluidCoolerLoad .GT. 0.0d0) THEN
        tmpHighSpeedAirFlowRate = DesFluidCoolerLoad / &
                                                           (SimpleFluidCooler(FluidCoolerNum)%DesignEnteringWaterTemp - &
                                                            SimpleFluidCooler(FluidCoolerNum)%DesignEnteringAirTemp) * 4.0d0
        IF (PlantSizesOkayToFinalize) SimpleFluidCooler(FluidCoolerNum)%HighSpeedAirFlowRate = tmpHighSpeedAirFlowRate
      ELSEIF (PltSizCondNum > 0) THEN
        IF (PlantSizData(PltSizCondNum)%DesVolFlowRate >= SmallWaterVolFlow) THEN
          ! This conditional statement is to trap when the user specified Condenser/Fluid Cooler water design setpoint
          ! temperature is less than design inlet air dry bulb temperature
          IF ( PlantSizData(PltSizCondNum)%ExitTemp <= &
                            SimpleFluidCooler(FluidCoolerNum)%DesignEnteringAirTemp ) THEN
            CALL ShowSevereError('Error when autosizing the UA value for fluid cooler = '//&
                                    TRIM(SimpleFluidCooler(FluidCoolerNum)%Name)//'.' )
            CALL ShowContinueError('Design Loop Exit Temperature ('// &
                                    TRIM(RoundSigDigits(PlantSizData(PltSizCondNum)%ExitTemp,2)) // &
                                   ' C) must be greater than design entering air dry-bulb temperature ('// &
                                    TRIM(RoundSigDigits(SimpleFluidCooler(FluidCoolerNum)%DesignEnteringAirTemp,2)) //&
                                   ' C) when autosizing the fluid cooler UA.')
            CALL ShowContinueError('It is recommended that the Design Loop Exit Temperature = design inlet air '// &
                                   'dry-bulb temp plus the Fluid Cooler design approach temperature (e.g., 4 C).')
            CALL ShowContinueError('If using HVACTemplate:Plant:ChilledWaterLoop, then check that input field ' // &
                                   'Condenser Water Design Setpoint must be > design inlet air dry-bulb temp if autosizing '// &
                                   'the Fluid Cooler.')
            CALL ShowFatalError('Review and revise design input values as appropriate.')
          ENDIF
          rho = GetDensityGlycol(PlantLoop(SimpleFluidCooler(FluidCoolerNum)%LoopNum)%FluidName,  &
                                    InitConvTemp, &
                                    PlantLoop(SimpleFluidCooler(FluidCoolerNum)%LoopNum)%FluidIndex,&
                                    'SizeFluidCooler')
          Cp = GetSpecificHeatGlycol(PlantLoop(SimpleFluidCooler(FluidCoolerNum)%LoopNum)%FluidName,  &
                                   PlantSizData(PltSizCondNum)%ExitTemp,                      &
                                   PlantLoop(SimpleFluidCooler(FluidCoolerNum)%LoopNum)%FluidIndex, &
                                   'SizeFluidCooler')
          DesFluidCoolerLoad = rho * Cp * tmpDesignWaterFlowRate * PlantSizData(PltSizCondNum)%DeltaT
          tmpHighSpeedAirFlowRate = DesFluidCoolerLoad / &
                                                           (SimpleFluidCooler(FluidCoolerNum)%DesignEnteringWaterTemp - &
                                                            SimpleFluidCooler(FluidCoolerNum)%DesignEnteringAirTemp) * 4.0d0
          IF (PlantSizesOkayToFinalize) SimpleFluidCooler(FluidCoolerNum)%HighSpeedAirFlowRate = tmpHighSpeedAirFlowRate
        ELSE
          tmpHighSpeedAirFlowRate = 0.d0
          IF (PlantSizesOkayToFinalize) SimpleFluidCooler(FluidCoolerNum)%HighSpeedAirFlowRate = tmpHighSpeedAirFlowRate
        ENDIF
      ELSE
        CALL ShowSevereError('Autosizing of fluid cooler air flow rate requires a loop Sizing:Plant object')
        CALL ShowFatalError(' Occurs in fluid cooler object = '//TRIM(SimpleFluidCooler(FluidCoolerNum)%Name))
      ENDIF
    ENDIF
    IF (SimpleFluidCooler(FluidCoolerNum)%FluidCoolerType_Num == FluidCooler_SingleSpeed) THEN
      IF (PlantSizesOkayToFinalize)   &
         CALL ReportSizingOutput(SimpleFluidCooler(FluidCoolerNum)%FluidCoolerType, SimpleFluidCooler(FluidCoolerNum)%Name, &
                              'Design Air Flow Rate [m3/s]', SimpleFluidCooler(FluidCoolerNum)%HighSpeedAirFlowRate)
    ELSEIF (SimpleFluidCooler(FluidCoolerNum)%FluidCoolerType == 'FluidCooler:TwoSpeed') THEN
      IF (PlantSizesOkayToFinalize)   &
         CALL ReportSizingOutput(SimpleFluidCooler(FluidCoolerNum)%FluidCoolerType, SimpleFluidCooler(FluidCoolerNum)%Name, &
                              'Air Flow Rate at High Fan Speed [m3/s]', SimpleFluidCooler(FluidCoolerNum)%HighSpeedAirFlowRate)
    ENDIF
  ENDIF

  IF (SimpleFluidCooler(FluidCoolerNum)%HighSpeedFluidCoolerUA == AutoSize) THEN
    IF (PltSizCondNum > 0) THEN
      IF (PlantSizData(PltSizCondNum)%DesVolFlowRate >= SmallWaterVolFlow) THEN
        ! This conditional statement is to trap when the user specified Condenser/Fluid Cooler water design setpoint
        ! temperature is less than design inlet air dry bulb temperature
        IF ( PlantSizData(PltSizCondNum)%ExitTemp <= &
                          SimpleFluidCooler(FluidCoolerNum)%DesignEnteringAirTemp ) THEN
          CALL ShowSevereError('Error when autosizing the UA value for fluid cooler = '//&
                                TRIM(SimpleFluidCooler(FluidCoolerNum)%Name)//'.' )
          CALL ShowContinueError('Design Loop Exit Temperature ('// &
                                  TRIM(RoundSigDigits(PlantSizData(PltSizCondNum)%ExitTemp,2)) // &
                                 ' C) must be greater than design entering air dry-bulb temperature ('// &
                                  TRIM(RoundSigDigits(SimpleFluidCooler(FluidCoolerNum)%DesignEnteringAirTemp,2)) //&
                                 ' C) when autosizing the fluid cooler UA.')
          CALL ShowContinueError('It is recommended that the Design Loop Exit Temperature = design inlet air dry-bulb temp '// &
                                 'plus the Fluid Cooler design approach temperature (e.g., 4 C).')
          CALL ShowContinueError('If using HVACTemplate:Plant:ChilledWaterLoop, then check that input field ' // &
                                 'Condenser Water Design Setpoint must be > design inlet air dry-bulb temp if autosizing '// &
                                 'the Fluid Cooler.')
          CALL ShowFatalError('Review and revise design input values as appropriate.')
        ENDIF
        rho = GetDensityGlycol(PlantLoop(SimpleFluidCooler(FluidCoolerNum)%LoopNum)%FluidName,  &
                               InitConvTemp, &
                               PlantLoop(SimpleFluidCooler(FluidCoolerNum)%LoopNum)%FluidIndex,&
                               'SizeFluidCooler')
        Cp = GetSpecificHeatGlycol(PlantLoop(SimpleFluidCooler(FluidCoolerNum)%LoopNum)%FluidName,  &
                                   PlantSizData(PltSizCondNum)%ExitTemp,                      &
                                   PlantLoop(SimpleFluidCooler(FluidCoolerNum)%LoopNum)%FluidIndex, &
                                   'SizeFluidCooler')
        DesFluidCoolerLoad = rho * Cp * tmpDesignWaterFlowRate * PlantSizData(PltSizCondNum)%DeltaT
        Par(1) = DesFluidCoolerLoad
        Par(2) = REAL(FluidCoolerNum,r64)
        Par(3) = rho * tmpDesignWaterFlowRate ! design water mass flow rate
        Par(4) = tmpHighSpeedAirFlowRate   ! design air volume flow rate
        Par(5) = Cp
        UA0 = 0.0001d0 * DesFluidCoolerLoad ! Assume deltaT = 10000K (limit)
        UA1 = DesFluidCoolerLoad            ! Assume deltaT = 1K
        SimpleFluidCoolerInlet(FluidCoolerNum)%WaterTemp = PlantSizData(PltSizCondNum)%ExitTemp + &
                                                               PlantSizData(PltSizCondNum)%DeltaT
        SimpleFluidCoolerInlet(FluidCoolerNum)%AirTemp = SimpleFluidCooler(FluidCoolerNum)%DesignEnteringAirTemp
        SimpleFluidCoolerInlet(FluidCoolerNum)%AirWetBulb = SimpleFluidCooler(FluidCoolerNum)%DesignEnteringAirWetbulbTemp
        SimpleFluidCoolerInlet(FluidCoolerNum)%AirPress = StdBaroPress
        SimpleFluidCoolerInlet(FluidCoolerNum)%AirHumRat =   &
           PsyWFnTdbTwbPb(SimpleFluidCoolerInlet(FluidCoolerNum)%AirTemp,     &
                          SimpleFluidCoolerInlet(FluidCoolerNum)%AirWetBulb,  &
                          SimpleFluidCoolerInlet(FluidCoolerNum)%AirPress,CalledFrom)
        CALL SolveRegulaFalsi(Acc, MaxIte, SolFla, UA, SimpleFluidCoolerUAResidual, UA0, UA1, Par)
        IF (SolFla == -1) THEN
          CALL ShowWarningError('Iteration limit exceeded in calculating fluid cooler UA.')
          CALL ShowContinueError('Autosizing of fluid cooler UA failed for fluid cooler = '//&
                               TRIM(SimpleFluidCooler(FluidCoolerNum)%Name))
          CALL ShowContinueError('The final UA value =' //TRIM(RoundSigDigits(UA,2))//' W/K, and the simulation continues...')
        ELSEIF (SolFla == -2) THEN
          CALL SimSimpleFluidCooler(INT(Par(2)),Par(3),Par(4),UA0,OutWaterTempAtUA0)
          CALL SimSimpleFluidCooler(INT(Par(2)),Par(3),Par(4),UA1,OutWaterTempAtUA1)
          CALL ShowSevereError(CalledFrom//': The combination of design input values did not allow the calculation of a ')
          CALL ShowContinueError('reasonable UA value. Review and revise design input values as appropriate. Specifying hard')
          CALL ShowContinueError('sizes for some "autosizable" fields while autosizing other "autosizable" fields may be ')
          CALL ShowContinueError('contributing to this problem.')
          CALL ShowContinueError('This model iterates on UA to find the heat transfer required to provide the design outlet ')
          CALL ShowContinueError('water temperature. Initially, the outlet water temperatures at high and low UA values are ')
          CALL ShowContinueError('calculated. The Design Exit Water Temperature should be between the outlet water ')
          CALL ShowContinueError('temperatures calculated at high and low UA values. If the Design Exit Water Temperature is ')
          CALL ShowContinueError('out of this range, the solution will not converge and UA will not be calculated. ')
          CALL ShowContinueError('The possible solutions could be to manually input adjusted water and/or air flow rates based ')
          CALL ShowContinueError('on the autosized values shown below or to adjust design fluid cooler air inlet dry-bulb ' // &
                                 'temperature.')
          CALL ShowContinueError('Plant:Sizing object inputs also influence these results (e.g. DeltaT and ExitTemp).')
          CALL ShowContinueError('Inputs to the fluid cooler object:')
          CALL ShowContinueError('Design Fluid Cooler Load [W]                       = '//TRIM(RoundSigDigits(Par(1),2)))
          CALL ShowContinueError('Design Fluid Cooler Water Volume Flow Rate [m3/s]  = '// &
                                  TRIM(RoundSigDigits(SimpleFluidCooler(FluidCoolerNum)%DesignWaterFlowRate,6)))
          CALL ShowContinueError('Design Fluid Cooler Air Volume Flow Rate [m3/s]    = '//TRIM(RoundSigDigits(Par(4),2)))
          CALL ShowContinueError('Design Fluid Cooler Air Inlet Dry-bulb Temp [C]    = '// &
                                  TRIM(RoundSigDigits(SimpleFluidCoolerInlet(FluidCoolerNum)%AirTemp,2)))
          CALL ShowContinueError('Inputs to the plant sizing object:')
          CALL ShowContinueError('Design Exit Water Temp [C]                         = '// &
                                  TRIM(RoundSigDigits(PlantSizData(PltSizCondNum)%ExitTemp,2)))
          CALL ShowContinueError('Loop Design Temperature Difference [C]             = '// &
                                  TRIM(RoundSigDigits(PlantSizData(PltSizCondNum)%DeltaT,2)))
          CALL ShowContinueError('Design Fluid Cooler Water Inlet Temp [C]           = '// &
                                  TRIM(RoundSigDigits(SimpleFluidCoolerInlet(FluidCoolerNum)%WaterTemp,2)))
          CALL ShowContinueError('Calculated water outlet temp at low UA [C] (UA = '// &
                                  TRIM(RoundSigDigits(UA0,2))//' W/K) = '// TRIM(RoundSigDigits(OutWaterTempAtUA0,2)))
          CALL ShowContinueError('Calculated water outlet temp at high UA [C](UA = '// &
                                  TRIM(RoundSigDigits(UA1,2))//' W/K) = '// TRIM(RoundSigDigits(OutWaterTempAtUA1,2)))
          CALL ShowFatalError('Autosizing of Fluid Cooler UA failed for fluid cooler = '// &
                                  TRIM(SimpleFluidCooler(FluidCoolerNum)%Name))
        ENDIF
        tmpHighSpeedEvapFluidCoolerUA =  UA
        IF (PlantSizesOkayToFinalize)  SimpleFluidCooler(FluidCoolerNum)%HighSpeedFluidCoolerUA = tmpHighSpeedEvapFluidCoolerUA
        SimpleFluidCooler(FluidCoolerNum)%FluidCoolerNominalCapacity = DesFluidCoolerLoad
      ELSE
        tmpHighSpeedEvapFluidCoolerUA = 0.d0
        IF (PlantSizesOkayToFinalize)  SimpleFluidCooler(FluidCoolerNum)%HighSpeedFluidCoolerUA = tmpHighSpeedEvapFluidCoolerUA
      ENDIF
      IF (SimpleFluidCooler(FluidCoolerNum)%FluidCoolerType_Num == FluidCooler_SingleSpeed) THEN
        IF (PlantSizesOkayToFinalize) CALL ReportSizingOutput(SimpleFluidCooler(FluidCoolerNum)%FluidCoolerType, &
                                SimpleFluidCooler(FluidCoolerNum)%Name, &
                               'U-factor Times Area Value at Design Air Flow Rate [W/K]', &
                                SimpleFluidCooler(FluidCoolerNum)%HighSpeedFluidCoolerUA)
      ELSEIF (SimpleFluidCooler(FluidCoolerNum)%FluidCoolerType_Num == FluidCooler_TwoSpeed) THEN
        IF (PlantSizesOkayToFinalize) CALL ReportSizingOutput(SimpleFluidCooler(FluidCoolerNum)%FluidCoolerType, &
                                SimpleFluidCooler(FluidCoolerNum)%Name, &
                               'U-factor Times Area Value at High Fan Speed [W/K]', &
                                SimpleFluidCooler(FluidCoolerNum)%HighSpeedFluidCoolerUA)
      ENDIF
    ELSE
      CALL ShowSevereError('Autosizing error for fluid cooler object = '//TRIM(SimpleFluidCooler(FluidCoolerNum)%Name))
      CALL ShowFatalError('Autosizing of fluid cooler UA requires a loop Sizing:Plant object.')
    ENDIF
  ENDIF

  IF (SimpleFluidCooler(FluidCoolerNum)%PerformanceInputMethod_Num == PIM_NominalCapacity) THEN
    IF (SimpleFluidCooler(FluidCoolerNum)%DesignWaterFlowRate >= SmallWaterVolFlow) THEN
      rho = GetDensityGlycol(PlantLoop(SimpleFluidCooler(FluidCoolerNum)%LoopNum)%FluidName,  &
                             InitConvTemp, &
                             PlantLoop(SimpleFluidCooler(FluidCoolerNum)%LoopNum)%FluidIndex,&
                             'SizeFluidCooler')
      Cp = GetSpecificHeatGlycol(PlantLoop(SimpleFluidCooler(FluidCoolerNum)%LoopNum)%FluidName,  &
                                 SimpleFluidCooler(FluidCoolerNum)%DesignEnteringWaterTemp,                      &
                                 PlantLoop(SimpleFluidCooler(FluidCoolerNum)%LoopNum)%FluidIndex, &
                                 'SizeFluidCooler')
      DesFluidCoolerLoad = SimpleFluidCooler(FluidCoolerNum)%FluidCoolerNominalCapacity
      Par(1) = DesFluidCoolerLoad
      Par(2) = REAL(FluidCoolerNum,r64)
      Par(3) = rho * tmpDesignWaterFlowRate   ! design water mass flow rate
      Par(4) = tmpHighSpeedAirFlowRate        ! design air volume flow rate
      Par(5) = Cp
      UA0 = 0.0001d0 * DesFluidCoolerLoad ! Assume deltaT = 10000K (limit)
      UA1 = DesFluidCoolerLoad            ! Assume deltaT = 1K
      SimpleFluidCoolerInlet(FluidCoolerNum)%WaterTemp =   &
         SimpleFluidCooler(FluidCoolerNum)%DesignEnteringWaterTemp      ! design inlet water temperature
      SimpleFluidCoolerInlet(FluidCoolerNum)%AirTemp =   &
         SimpleFluidCooler(FluidCoolerNum)%DesignEnteringAirTemp        ! design inlet air dry-bulb temp
      SimpleFluidCoolerInlet(FluidCoolerNum)%AirWetBulb =   &
         SimpleFluidCooler(FluidCoolerNum)%DesignEnteringAirWetbulbTemp ! design inlet air wet-bulb temp
      SimpleFluidCoolerInlet(FluidCoolerNum)%AirPress = StdBaroPress
      SimpleFluidCoolerInlet(FluidCoolerNum)%AirHumRat =   &
         PsyWFnTdbTwbPb(SimpleFluidCoolerInlet(FluidCoolerNum)%AirTemp,  &
                        SimpleFluidCoolerInlet(FluidCoolerNum)%AirWetBulb,  &
                        SimpleFluidCoolerInlet(FluidCoolerNum)%AirPress)
      CALL SolveRegulaFalsi(Acc, MaxIte, SolFla, UA, SimpleFluidCoolerUAResidual, UA0, UA1, Par)
      IF (SolFla == -1) THEN
          CALL ShowWarningError('Iteration limit exceeded in calculating fluid cooler UA.')
          CALL ShowContinueError('Autosizing of fluid cooler UA failed for fluid cooler = '//&
                               TRIM(SimpleFluidCooler(FluidCoolerNum)%Name))
          CALL ShowContinueError('The final UA value =' //TRIM(RoundSigDigits(UA,2))//' W/K, and the simulation continues...')
      ELSEIF (SolFla == -2) THEN
        CALL SimSimpleFluidCooler(INT(Par(2)),Par(3),Par(4),UA0,OutWaterTempAtUA0)
        CALL SimSimpleFluidCooler(INT(Par(2)),Par(3),Par(4),UA1,OutWaterTempAtUA1)
        CALL ShowSevereError(CalledFrom//': The combination of design input values did not allow the calculation of a ')
        CALL ShowContinueError('reasonable UA value. Review and revise design input values as appropriate. Specifying hard')
        CALL ShowContinueError('sizes for some "autosizable" fields while autosizing other "autosizable" fields may be ')
        CALL ShowContinueError('contributing to this problem.')
        CALL ShowContinueError('This model iterates on UA to find the heat transfer required to provide the design outlet ')
        CALL ShowContinueError('water temperature. Initially, the outlet water temperatures at high and low UA values are ')
        CALL ShowContinueError('calculated. The Design Exit Water Temperature should be between the outlet water ')
        CALL ShowContinueError('temperatures calculated at high and low UA values. If the Design Exit Water Temperature is ')
        CALL ShowContinueError('out of this range, the solution will not converge and UA will not be calculated. ')
        CALL ShowContinueError('The possible solutions could be to manually input adjusted water and/or air flow rates based ')
        CALL ShowContinueError('on the autosized values shown below or to adjust design fluid cooler air inlet dry-bulb ' // &
                               'temperature.')
        CALL ShowContinueError('Plant:Sizing object inputs also influence these results (e.g. DeltaT and ExitTemp).')
        CALL ShowContinueError('Inputs to the fluid cooler object:')
        CALL ShowContinueError('Design Fluid Cooler Load [W]                       = '//TRIM(RoundSigDigits(Par(1),2)))
        CALL ShowContinueError('Design Fluid Cooler Water Volume Flow Rate [m3/s]  = '// &
                                TRIM(RoundSigDigits(SimpleFluidCooler(FluidCoolerNum)%DesignWaterFlowRate,6)))
        CALL ShowContinueError('Design Fluid Cooler Air Volume Flow Rate [m3/s]    = '//TRIM(RoundSigDigits(Par(4),2)))
        CALL ShowContinueError('Design Fluid Cooler Air Inlet Dry-bulb Temp [C]    = '// &
                                TRIM(RoundSigDigits(SimpleFluidCoolerInlet(FluidCoolerNum)%AirTemp,2)))
        CALL ShowContinueError('Inputs to the plant sizing object:')
        CALL ShowContinueError('Design Exit Water Temp [C]                         = '// &
                                TRIM(RoundSigDigits(PlantSizData(PltSizCondNum)%ExitTemp,2)))
        CALL ShowContinueError('Loop Design Temperature Difference [C]             = '// &
                                TRIM(RoundSigDigits(PlantSizData(PltSizCondNum)%DeltaT,2)))
        CALL ShowContinueError('Design Fluid Cooler Water Inlet Temp [C]           = '// &
                                TRIM(RoundSigDigits(SimpleFluidCoolerInlet(FluidCoolerNum)%WaterTemp,2)))
        CALL ShowContinueError('Calculated water outlet temp at low UA [C] (UA = '// &
                                TRIM(RoundSigDigits(UA0,2))//' W/K) = '// TRIM(RoundSigDigits(OutWaterTempAtUA0,2)))
        CALL ShowContinueError('Calculated water outlet temp at high UA [C] (UA = '// &
                                TRIM(RoundSigDigits(UA1,2))//' W/K) = '// TRIM(RoundSigDigits(OutWaterTempAtUA1,2)))
        CALL ShowFatalError('Autosizing of Fluid Cooler UA failed for fluid cooler = '// &
                                TRIM(SimpleFluidCooler(FluidCoolerNum)%Name))
      ENDIF
      SimpleFluidCooler(FluidCoolerNum)%HighSpeedFluidCoolerUA = UA
    ELSE
      SimpleFluidCooler(FluidCoolerNum)%HighSpeedFluidCoolerUA = 0.0d0
    ENDIF
    IF (SimpleFluidCooler(FluidCoolerNum)%FluidCoolerType_Num == FluidCooler_SingleSpeed) THEN
      IF (PlantSizesOkayToFinalize) CALL ReportSizingOutput(SimpleFluidCooler(FluidCoolerNum)%FluidCoolerType, &
                                                            SimpleFluidCooler(FluidCoolerNum)%Name, &
                              'Fluid cooler UA value at design air flow rate based on nominal capacity input [W/K]', &
                              SimpleFluidCooler(FluidCoolerNum)%HighSpeedFluidCoolerUA)
    ELSEIF (SimpleFluidCooler(FluidCoolerNum)%FluidCoolerType_Num == FluidCooler_TwoSpeed) THEN
      IF (PlantSizesOkayToFinalize) CALL ReportSizingOutput(SimpleFluidCooler(FluidCoolerNum)%FluidCoolerType, &
                                                                         SimpleFluidCooler(FluidCoolerNum)%Name, &
                              'Fluid cooler UA value at high fan speed based on nominal capacity input [W/K]', &
                              SimpleFluidCooler(FluidCoolerNum)%HighSpeedFluidCoolerUA)
    ENDIF
  ENDIF

  IF (SimpleFluidCooler(FluidCoolerNum)%LowSpeedAirFlowRate == AutoSize .AND. PlantSizesOkayToFinalize ) THEN
    SimpleFluidCooler(FluidCoolerNum)%LowSpeedAirFlowRate = SimpleFluidCooler(FluidCoolerNum)%LowSpeedAirFlowRateSizingFactor &
                                                             * SimpleFluidCooler(FluidCoolerNum)%HighSpeedAirFlowRate
    CALL ReportSizingOutput(SimpleFluidCooler(FluidCoolerNum)%FluidCoolerType, SimpleFluidCooler(FluidCoolerNum)%Name, &
                              'Air Flow Rate at Low Fan Speed [m3/s]', SimpleFluidCooler(FluidCoolerNum)%LowSpeedAirFlowRate)
  ENDIF

  IF (SimpleFluidCooler(FluidCoolerNum)%LowSpeedFanPower == AutoSize .AND. PlantSizesOkayToFinalize ) THEN
      SimpleFluidCooler(FluidCoolerNum)%LowSpeedFanPower = SimpleFluidCooler(FluidCoolerNum)%LowSpeedFanPowerSizingFactor &
                                                            * SimpleFluidCooler(FluidCoolerNum)%HighSpeedFanPower
      CALL ReportSizingOutput(SimpleFluidCooler(FluidCoolerNum)%FluidCoolerType, SimpleFluidCooler(FluidCoolerNum)%Name, &
                              'Fan Power at Low Fan Speed [W]', SimpleFluidCooler(FluidCoolerNum)%LowSpeedFanPower)
  ENDIF

  IF (SimpleFluidCooler(FluidCoolerNum)%LowSpeedFluidCoolerUA == AutoSize .AND. PlantSizesOkayToFinalize ) THEN
    SimpleFluidCooler(FluidCoolerNum)%LowSpeedFluidCoolerUA = &
                                           SimpleFluidCooler(FluidCoolerNum)%LowSpeedFluidCoolerUASizingFactor &
                                              * SimpleFluidCooler(FluidCoolerNum)%HighSpeedFluidCoolerUA
    CALL ReportSizingOutput(SimpleFluidCooler(FluidCoolerNum)%FluidCoolerType, SimpleFluidCooler(FluidCoolerNum)%Name, &
                  'U-factor Times Area Value at Low Fan Speed [W/K]', SimpleFluidCooler(FluidCoolerNum)%LowSpeedFluidCoolerUA)
  ENDIF

  IF (SimpleFluidCooler(FluidCoolerNum)%PerformanceInputMethod_Num == PIM_NominalCapacity .AND. &
      SimpleFluidCooler(FluidCoolerNum)%FluidCoolerType_Num == FluidCooler_TwoSpeed) THEN
    IF (SimpleFluidCooler(FluidCoolerNum)%FluidCoolerLowSpeedNomCap == Autosize) THEN
      SimpleFluidCooler(FluidCoolerNum)%FluidCoolerLowSpeedNomCap = &
          SimpleFluidCooler(FluidCoolerNum)%FluidCoolerLowSpeedNomCapSizingFactor &
            * SimpleFluidCooler(FluidCoolerNum)%FluidCoolerNominalCapacity
      CALL ReportSizingOutput(SimpleFluidCooler(FluidCoolerNum)%FluidCoolerType, SimpleFluidCooler(FluidCoolerNum)%Name, &
                  'Low Fan Speed Nominal Capacity [W]', SimpleFluidCooler(FluidCoolerNum)%FluidCoolerLowSpeedNomCap)
    ENDIF

    IF (SimpleFluidCooler(FluidCoolerNum)%DesignWaterFlowRate >= SmallWaterVolFlow .AND.  &
       SimpleFluidCooler(FluidCoolerNum)%FluidCoolerLowSpeedNomCap > 0.0d0) THEN
      rho = GetDensityGlycol(PlantLoop(SimpleFluidCooler(FluidCoolerNum)%LoopNum)%FluidName,  &
                             InitConvTemp, &
                             PlantLoop(SimpleFluidCooler(FluidCoolerNum)%LoopNum)%FluidIndex,&
                             'SizeFluidCooler')
      Cp = GetSpecificHeatGlycol(PlantLoop(SimpleFluidCooler(FluidCoolerNum)%LoopNum)%FluidName,  &
                                 SimpleFluidCooler(FluidCoolerNum)%DesignEnteringWaterTemp,                      &
                                 PlantLoop(SimpleFluidCooler(FluidCoolerNum)%LoopNum)%FluidIndex, &
                                 'SizeFluidCooler')
      DesFluidCoolerLoad = SimpleFluidCooler(FluidCoolerNum)%FluidCoolerLowSpeedNomCap
      Par(1) = DesFluidCoolerLoad
      Par(2) = REAL(FluidCoolerNum,r64)
      Par(3) = rho * tmpDesignWaterFlowRate ! design water mass flow rate
      Par(4) = SimpleFluidCooler(FluidCoolerNum)%LowSpeedAirFlowRate  ! Air volume flow rate at low fan speed
      Par(5) = Cp
      UA0 = 0.0001d0 * DesFluidCoolerLoad ! Assume deltaT = 10000K (limit)
      UA1 = DesFluidCoolerLoad            ! Assume deltaT = 1K
      SimpleFluidCoolerInlet(FluidCoolerNum)%WaterTemp =   &
         SimpleFluidCooler(FluidCoolerNum)%DesignEnteringWaterTemp      ! design inlet water temperature
      SimpleFluidCoolerInlet(FluidCoolerNum)%AirTemp =   &
         SimpleFluidCooler(FluidCoolerNum)%DesignEnteringAirTemp        ! design inlet air dry-bulb temp
      SimpleFluidCoolerInlet(FluidCoolerNum)%AirWetBulb =   &
         SimpleFluidCooler(FluidCoolerNum)%DesignEnteringAirWetbulbTemp ! design inlet air wet-bulb temp
      SimpleFluidCoolerInlet(FluidCoolerNum)%AirPress = StdBaroPress
      SimpleFluidCoolerInlet(FluidCoolerNum)%AirHumRat =   &
         PsyWFnTdbTwbPb(SimpleFluidCoolerInlet(FluidCoolerNum)%AirTemp,  &
                        SimpleFluidCoolerInlet(FluidCoolerNum)%AirWetBulb,  &
                        SimpleFluidCoolerInlet(FluidCoolerNum)%AirPress,CalledFrom)
      CALL SolveRegulaFalsi(Acc, MaxIte, SolFla, UA, SimpleFluidCoolerUAResidual, UA0, UA1, Par)
      IF (SolFla == -1) THEN
        CALL ShowWarningError('Iteration limit exceeded in calculating fluid cooler UA.')
        CALL ShowContinueError('Autosizing of fluid cooler UA failed for fluid cooler = '//&
                             TRIM(SimpleFluidCooler(FluidCoolerNum)%Name))
        CALL ShowContinueError('The final UA value at low fan speed =' //TRIM(RoundSigDigits(UA,2))// &
                               ' W/C, and the simulation continues...')
      ELSEIF (SolFla == -2) THEN
        CALL SimSimpleFluidCooler(INT(Par(2)),Par(3),Par(4),UA0,OutWaterTempAtUA0)
        CALL SimSimpleFluidCooler(INT(Par(2)),Par(3),Par(4),UA1,OutWaterTempAtUA1)
        CALL ShowSevereError(CalledFrom//': The combination of design input values did not allow the calculation of a ')
        CALL ShowContinueError('reasonable low-speed UA value. Review and revise design input values as appropriate. ')
        CALL ShowContinueError('Specifying hard sizes for some "autosizable" fields while autosizing other "autosizable" ')
        CALL ShowContinueError('fields may be contributing to this problem.')
        CALL ShowContinueError('This model iterates on UA to find the heat transfer required to provide the design outlet ')
        CALL ShowContinueError('water temperature. Initially, the outlet water temperatures at high and low UA values are ')
        CALL ShowContinueError('calculated. The Design Exit Water Temperature should be between the outlet water ')
        CALL ShowContinueError('temperatures calculated at high and low UA values. If the Design Exit Water Temperature is ')
        CALL ShowContinueError('out of this range, the solution will not converge and UA will not be calculated. ')
        CALL ShowContinueError('The possible solutions could be to manually input adjusted water and/or air flow rates based ')
        CALL ShowContinueError('on the autosized values shown below or to adjust design fluid cooler air inlet dry-bulb ' // &
                               'temperature.')
        CALL ShowContinueError('Plant:Sizing object inputs also influence these results (e.g. DeltaT and ExitTemp).')
        CALL ShowContinueError('Inputs to the fluid cooler object:')
        CALL ShowContinueError('Design Fluid Cooler Load [W]                         = '//TRIM(RoundSigDigits(Par(1),2)))
        CALL ShowContinueError('Design Fluid Cooler Water Volume Flow Rate [m3/s]    = '// &
                                TRIM(RoundSigDigits(SimpleFluidCooler(FluidCoolerNum)%DesignWaterFlowRate,6)))
        CALL ShowContinueError('Design Fluid Cooler Air Volume Flow Rate [m3/s]      = '//TRIM(RoundSigDigits(Par(4),2)))
        CALL ShowContinueError('Design Fluid Cooler Air Inlet Dry-bulb Temp [C]      = '// &
                                TRIM(RoundSigDigits(SimpleFluidCoolerInlet(FluidCoolerNum)%AirTemp,2)))
        CALL ShowContinueError('Inputs to the plant sizing object:')
        CALL ShowContinueError('Design Exit Water Temp [C]                           = '// &
                                TRIM(RoundSigDigits(PlantSizData(PltSizCondNum)%ExitTemp,2)))
        CALL ShowContinueError('Loop Design Temperature Difference [C]               = '// &
                                TRIM(RoundSigDigits(PlantSizData(PltSizCondNum)%DeltaT,2)))
        CALL ShowContinueError('Design Fluid Cooler Water Inlet Temp [C]             = '// &
                                TRIM(RoundSigDigits(SimpleFluidCoolerInlet(FluidCoolerNum)%WaterTemp,2)))
        CALL ShowContinueError('Calculated water outlet temp at low UA [C](UA = '// &
                                TRIM(RoundSigDigits(UA0,2))//' W/C) = '// TRIM(RoundSigDigits(OutWaterTempAtUA0,2)))
        CALL ShowContinueError('Calculated water outlet temp at high UA [C](UA = '// &
                                TRIM(RoundSigDigits(UA1,2))//' W/C) = '// TRIM(RoundSigDigits(OutWaterTempAtUA1,2)))
        CALL ShowFatalError('Autosizing of Fluid Cooler UA failed for fluid cooler = '//  &
                                TRIM(SimpleFluidCooler(FluidCoolerNum)%Name))
      ENDIF
      SimpleFluidCooler(FluidCoolerNum)%LowSpeedFluidCoolerUA = UA
    ELSE
      SimpleFluidCooler(FluidCoolerNum)%LowSpeedFluidCoolerUA = 0.0d0
    ENDIF
    IF (PlantSizesOkayToFinalize) CALL ReportSizingOutput(SimpleFluidCooler(FluidCoolerNum)%FluidCoolerType, &
                             SimpleFluidCooler(FluidCoolerNum)%Name, &
                            'U-factor Times Area Value at Low Fan Speed [W/C]', &
                             SimpleFluidCooler(FluidCoolerNum)%LowSpeedFluidCoolerUA)
  ENDIF

  ErrorsFound = .FALSE.

  IF (PlantSizesOkayToFinalize) THEN
    !create predefined report
    equipName = SimpleFluidCooler(FluidCoolerNum)%Name
    CALL PreDefTableEntry(pdchMechType,equipName,SimpleFluidCooler(FluidCoolerNum)%FluidCoolerType)
    CALL PreDefTableEntry(pdchMechNomCap,equipName,SimpleFluidCooler(FluidCoolerNum)%FluidCoolerNominalCapacity)
  ENDIF

  IF (SimpleFluidCooler(FluidCoolerNum)%FluidCoolerType_Num == FluidCooler_TwoSpeed) THEN
    IF (SimpleFluidCooler(FluidCoolerNum)%DesignWaterFlowRate > 0.0d0) THEN
      IF (SimpleFluidCooler(FluidCoolerNum)%HighSpeedAirFlowRate <=   &
         SimpleFluidCooler(FluidCoolerNum)%LowSpeedAirFlowRate) THEN
        CALL ShowSevereError('FluidCooler:TwoSpeed  "'//TRIM(SimpleFluidCooler(FluidCoolerNum)%Name)//&
                             '". Low speed air flow rate must be less than high speed air flow rate.')
        ErrorsFound=.true.
      ENDIF
      IF (SimpleFluidCooler(FluidCoolerNum)%HighSpeedFluidCoolerUA <=   &
         SimpleFluidCooler(FluidCoolerNum)%LowSpeedFluidCoolerUA) THEN
         CALL ShowSevereError('FluidCooler:TwoSpeed  "'//TRIM(SimpleFluidCooler(FluidCoolerNum)%Name)//&
                       '". Fluid cooler UA at low fan speed must be less than the fluid cooler UA at high fan speed.')
         ErrorsFound=.true.
      ENDIF
    END IF
  END IF

  IF (ErrorsFound) THEN
    CALL ShowFatalError('SizeFluidCooler: Program terminated due to previous condition(s).')
  ENDIF

  RETURN
END SUBROUTINE SizeFluidCooler

! End Initialization Section for the CondenserLoopFluidCoolers Module
!******************************************************************************

! Beginning of the CondenserLoopFluidCoolers Module Simulation Subroutines
! *****************************************************************************

SUBROUTINE SingleSpeedFluidCooler(FluidCoolerNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Chandan Sharma
          !       DATE WRITTEN   August 2008
          !       MODIFIED       Dec. 2008. BG. added RunFlag logic per original methodology
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! To simulate the operation of a single-speed fan fluid cooler.

          ! METHODOLOGY EMPLOYED:
          ! The fluid cooler is modeled using effectiveness-NTU relationships for
          ! cross flow heat exchangers (both stream unmixed)based on cooling tower model.
          !
          ! The subroutine calculates the period of time required to meet a
          ! leaving water temperature setpoint. It assumes that part-load
          ! operation represents a linear interpolation of two steady-state regimes.
          ! Cyclic losses are neglected. The period of time required to meet the
          ! leaving water temperature setpoint is used to determine the required
          ! fan power and energy.
          !
          ! A RunFlag is passed by the upper level manager to indicate the ON/OFF status,
          ! or schedule, of the fluid cooler. If the fluid cooler is OFF, outlet water
          ! temperature and flow rate are passed through the model from inlet node to
          ! outlet node without intervention. Reports are also updated with fan power
          ! and energy being zero.
          !
          ! When the RunFlag indicates an ON condition for thefluid cooler, the
          ! mass flow rate and water temperature are read from the inlet node of the
          ! fluid cooler (water-side). The outdoor air dry-bulb temperature is used
          ! as the entering condition to thefluid cooler (air-side).Thefluid cooler
          ! fan is turned on and design parameters are used to calculate the leaving
          ! water temperature.If the calculated leaving water temperature is below the setpoint,
          ! a fan run-time fraction is calculated and used to determine fan power. The leaving
          ! water temperature setpoint is placed on the outlet node. If the calculated
          ! leaving water temperature is at or above the setpoint, the calculated
          ! leaving water temperature is placed on the outlet node and the fan runs at
          ! full power. Water mass flow rate is passed from inlet node to outlet node
          ! with no intervention.

          ! REFERENCES:
          ! ASHRAE HVAC1KIT: A Toolkit for Primary HVAC System Energy Calculation. 1999.
          ! Based on SingleSpeedTower subroutine by Dan Fisher ,Sept 1998.

          ! USE STATEMENTS:
!  USE FluidProperties, ONLY : GetSpecificHeatGlycol
  USE DataPlant,       ONLY : SingleSetpoint, DualSetpointDeadband

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER                :: FluidCoolerNum
!  LOGICAL, INTENT(IN)    :: RunFlag

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64)              :: AirFlowRate
  REAL(r64)              :: UAdesign            ! UA value at design conditions (entered by user or calculated)
  REAL(r64)              :: OutletWaterTempOFF
  REAL(r64)              :: FanModeFrac
  REAL(r64)              :: DesignWaterFlowRate
  REAL(r64)              :: FanPowerOn
  REAL(r64)              :: CpWater
  REAL(r64)              :: TempSetPoint
  INTEGER                :: LoopNum
  INTEGER                :: LoopSideNum

    !set inlet and outlet nodes
  WaterInletNode     = SimpleFluidCooler(FluidCoolerNum)%WaterInletNodeNum
  WaterOutletNode    = SimpleFluidCooler(FluidCoolerNum)%WaterOutletNodeNum
  Qactual            = 0.0d0
  FanModeFrac        = 0.0d0
  FanPower           = 0.0d0
  OutletWaterTemp    = Node(WaterInletNode)%Temp
  LoopNum            = SimpleFluidCooler(FluidCoolerNum)%LoopNum
  LoopSideNum        = SimpleFluidCooler(FluidCoolerNum)%LoopSideNum
  SELECT CASE (PlantLoop(LoopNum)%LoopDemandCalcScheme)
  CASE (SingleSetPoint)
    TempSetPoint       = PlantLoop(LoopNum)%LoopSide(LoopSideNum)%TempSetpoint
  CASE (DualSetPointDeadBand)
    TempSetPoint       = PlantLoop(LoopNum)%LoopSide(LoopSideNum)%TempSetpointHi
  END SELECT

!   MassFlowTol is a parameter to indicate a no flow condition
  IF(WaterMassFlowRate .LE. MassFlowTolerance) RETURN

  IF (OutletWaterTemp < TempSetPoint) THEN !already there don't need to run the cooler
    RETURN
  ENDIF

!   Initialize local variables
  DesignWaterFlowRate    = SimpleFluidCooler(FluidCoolerNum)%DesignWaterFlowRate
  OutletWaterTempOFF     = Node(WaterInletNode)%Temp
  OutletWaterTemp        = OutletWaterTempOFF

  UAdesign          = SimpleFluidCooler(FluidCoolerNum)%HighSpeedFluidCoolerUA
  AirFlowRate       = SimpleFluidCooler(FluidCoolerNum)%HighSpeedAirFlowRate
  FanPowerOn        = SimpleFluidCooler(FluidCoolerNum)%HighSpeedFanPower

  Call SimSimpleFluidCooler(FluidCoolerNum,WaterMassFlowRate,AirFlowRate,UAdesign,OutletWaterTemp)

  IF(OutletWaterTemp .LE. TempSetPoint)THEN
!   Setpoint was met with pump ON and fan ON, calculate run-time fraction or just wasn't needed at all
    IF (OutletWaterTemp /= OutletWaterTempOFF) THEN ! don't divide by zero
      FanModeFrac     = (TempSetPoint-OutletWaterTempOFF)/(OutletWaterTemp-OutletWaterTempOFF)
    ENDIF
    FanPower        = Max(FanModeFrac * FanPowerOn, 0.0D0)  ! BG change
    OutletWaterTemp = TempSetPoint
  ELSE
!    Setpoint was not met, fluid cooler ran at full capacity
    FanPower        = FanPowerOn
  END IF
  CpWater =  GetSpecificHeatGlycol(PlantLoop(SimpleFluidCooler(FluidCoolerNum)%LoopNum)%FluidName,  &
                                   Node(WaterInletNode)%Temp,                                   &
                                   PlantLoop(SimpleFluidCooler(FluidCoolerNum)%LoopNum)%FluidIndex,  &
                                   'SingleSpeedFluidCooler')
  Qactual = WaterMassFlowRate * CpWater * (Node(WaterInletNode)%Temp - OutletWaterTemp)

RETURN
END SUBROUTINE SingleSpeedFluidCooler

SUBROUTINE TwoSpeedFluidCooler(FluidCoolerNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Chandan Sharma
          !       DATE WRITTEN   August 2008
          !       MODIFIED       Dec. 2008. BG. added RunFlag logic per original methodology
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! To simulate the operation of a fluid cooler with a two-speed fan.

          ! METHODOLOGY EMPLOYED:
          ! The fluid cooler is modeled using effectiveness-NTU relationships for
          ! cross flow heat exchangers (both stream unmixed)based on cooling tower model.
          !
          ! The subroutine calculates the period of time required to meet a
          ! leaving water temperature setpoint. It assumes that part-load
          ! operation represents a linear interpolation of two steady-state regimes
          ! (high-speed fan operation and low-speed fan operation).
          ! Cyclic losses are neglected. The period of time required to meet the
          ! leaving water temperature setpoint is used to determine the required
          ! fan power and energy.
          !
          ! A RunFlag is passed by the upper level manager to indicate the ON/OFF status,
          ! or schedule, of the fluid cooler. If the fluid cooler is OFF, outlet water
          ! temperature and flow rate are passed through the model from inlet node to
          ! outlet node without intervention.Reports are also updated with fan power
          ! and fan energy being zero.
          !
          ! When the RunFlag indicates an ON condition for the fluid cooler, the
          ! mass flow rate and water temperature are read from the inlet node of the
          ! fluid cooler (water-side). The outdoor air dry-bulb temperature is used
          ! as the entering condition to the fluid cooler (air-side). Input deck
          ! parameters are read for the low fan speed and a leaving water temperature
          ! is calculated.
          !
          ! If the calculated leaving water temperature is below the setpoint,
          ! a fan run-time fraction (FanModeFrac) is calculated and used to determine fan power.
          ! The leaving water temperature setpoint is placed on the outlet node.
          ! If the calculated leaving water temperature is at or above
          ! the setpoint, the fluid cooler fan is turned on 'high speed' and the routine is
          ! repeated. If the calculated leaving water temperature is below the setpoint,
          ! a fan run-time fraction is calculated for the second stage fan and fan power
          ! is calculated as FanModeFrac*HighSpeedFanPower+(1-FanModeFrac)*LowSpeedFanPower.
          ! If the calculated leaving water temperature is above the leaving water temp.
          ! setpoint, the calculated leaving water temperature is placed on the outlet
          ! node and the fan runs at full power (High Speed Fan Power). Water mass flow
          ! rate is passed from inlet node to outlet node with no intervention.

          ! REFERENCES:
          ! ASHRAE HVAC1KIT: A Toolkit for Primary HVAC System Energy Calculation. 1999.
          ! Based on TwoSpeedTower by Dan Fisher ,Sept. 1998.

          ! USE STATEMENTS:
!  USE FluidProperties, ONLY : GetSpecificHeatGlycol
  USE DataPlant,       ONLY : SingleSetpoint, DualSetpointDeadband

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER                :: FluidCoolerNum
!  LOGICAL, INTENT(IN)    :: RunFlag

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64)              :: AirFlowRate
  REAL(r64)              :: UAdesign            ! UA value at design conditions (entered by user) [W/C]
  REAL(r64)              :: OutletWaterTempOFF
  REAL(r64)              :: OutletWaterTemp1stStage
  REAL(r64)              :: OutletWaterTemp2ndStage
  REAL(r64)              :: FanModeFrac
  REAL(r64)              :: designWaterFlowRate
  REAL(r64)              :: FanPowerLow
  REAL(r64)              :: FanPowerHigh
  REAL(r64)              :: CpWater
  REAL(r64)              :: TempSetPoint
  INTEGER                :: LoopNum
  INTEGER                :: LoopSideNum

  WaterInletNode      = SimpleFluidCooler(FluidCoolerNum)%WaterInletNodeNum
  WaterOutletNode     = SimpleFluidCooler(FluidCoolerNum)%WaterOutletNodeNum
  Qactual             = 0.0d0
  FanPower            = 0.0d0
  OutletWaterTemp     = Node(WaterInletNode)%Temp
  FanModeFrac         = 0.0d0
  LoopNum             = SimpleFluidCooler(FluidCoolerNum)%LoopNum
  LoopSideNum         = SimpleFluidCooler(FluidCoolerNum)%LoopSideNum
  SELECT CASE (PlantLoop(LoopNum)%LoopDemandCalcScheme)
  CASE (SingleSetPoint)
    TempSetPoint       = PlantLoop(LoopNum)%LoopSide(LoopSideNum)%TempSetpoint
  CASE (DualSetPointDeadBand)
    TempSetPoint       = PlantLoop(LoopNum)%LoopSide(LoopSideNum)%TempSetpointHi
  END SELECT

  ! MassFlowTol is a parameter to indicate a no flow condition
  IF(WaterMassFlowRate .LE. MassFlowTolerance .OR. PlantLoop(LoopNum)%Loopside(LoopSideNum)%FlowLock .EQ. 0)RETURN

  ! set local variable for fluid cooler
  DesignWaterFlowRate     = SimpleFluidCooler(FluidCoolerNum)%DesignWaterFlowRate
  WaterMassFlowRate       = Node(WaterInletNode)%MassFlowRate
  OutletWaterTempOFF      = Node(WaterInletNode)%Temp
  OutletWaterTemp1stStage = OutletWaterTempOFF
  OutletWaterTemp2ndStage = OutletWaterTempOFF
  FanModeFrac             = 0.0d0

  IF (OutletWaterTempOFF < TempSetPoint) THEN !already there don't need to run the cooler
    RETURN
  ENDIF


  UAdesign        = SimpleFluidCooler(FluidCoolerNum)%LowSpeedFluidCoolerUA
  AirFlowRate     = SimpleFluidCooler(FluidCoolerNum)%LowSpeedAirFlowRate
  FanPowerLow     = SimpleFluidCooler(FluidCoolerNum)%LowSpeedFanPower

  Call SimSimpleFluidCooler(FluidCoolerNum,WaterMassFlowRate,AirFlowRate,UAdesign,OutletWaterTemp1stStage)

  IF(OutletWaterTemp1stStage .LE. TempSetPoint)THEN
    ! Setpoint was met with pump ON and fan ON 1st stage, calculate fan mode fraction
    IF (OutletWaterTemp1stStage /= OutletWaterTempOFF) THEN ! don't divide by zero
      FanModeFrac = (TempSetPoint-OutletWaterTempOFF)/(OutletWaterTemp1stStage-OutletWaterTempOFF)
    ENDIF
    FanPower        = FanModeFrac * FanPowerLow
    OutletWaterTemp = TempSetPoint
    Qactual         = Qactual * FanModeFrac
  ELSE
    ! Setpoint was not met, turn on fluid cooler 2nd stage fan
    UAdesign          = SimpleFluidCooler(FluidCoolerNum)%HighSpeedFluidCoolerUA
    AirFlowRate       = SimpleFluidCooler(FluidCoolerNum)%HighSpeedAirFlowRate
    FanPowerHigh      = SimpleFluidCooler(FluidCoolerNum)%HighSpeedFanPower

    Call SimSimpleFluidCooler(FluidCoolerNum,WaterMassFlowRate,AirFlowRate,UAdesign,OutletWaterTemp2ndStage)

    IF((OutletWaterTemp2ndStage .LE. TempSetPoint).AND. UAdesign .GT. 0.0d0)THEN
      ! Setpoint was met with pump ON and fan ON 2nd stage, calculate fan mode fraction
      FanModeFrac     = (TempSetPoint-OutletWaterTemp1stStage)/(OutletWaterTemp2ndStage-OutletWaterTemp1stStage)
      FanPower        = MAX((FanModeFrac * FanPowerHigh) + (1.d0-FanModeFrac)*FanPowerLow, 0.0D0)
      OutletWaterTemp = TempSetPoint
    ELSE
      ! Setpoint was not met, fluid cooler ran at full capacity
      OutletWaterTemp = OutletWaterTemp2ndStage
      FanPower        = FanPowerHigh
    END IF

  END IF
  CpWater =  GetSpecificHeatGlycol(PlantLoop(SimpleFluidCooler(FluidCoolerNum)%LoopNum)%FluidName,  &
                                   Node(WaterInletNode)%Temp,                                   &
                                   PlantLoop(SimpleFluidCooler(FluidCoolerNum)%LoopNum)%FluidIndex,  &
                                   'TwoSpeedFluidCooler')
  Qactual = WaterMassFlowRate * CpWater * (Node(WaterInletNode)%Temp - OutletWaterTemp)

RETURN
END SUBROUTINE TwoSpeedFluidCooler

SUBROUTINE SimSimpleFluidCooler(FluidCoolerNum,WaterMassFlowRate,AirFlowRate,UAdesign,OutletWaterTemp)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Chandan Sharma
          !       DATE WRITTEN   August 2008
          !       MODIFIED       April 2010, Chandan Sharma, FSEC
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! See purpose for Single Speed or Two Speed Fluid Cooler model

          ! METHODOLOGY EMPLOYED:
          ! See methodology for Single Speed or Two Speed Fluid Cooler model

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
!  USE FluidProperties, ONLY : GetSpecificHeatGlycol

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER                :: FluidCoolerNum
  REAL(r64)              :: WaterMassFlowRate
  REAL(r64)              :: AirFlowRate
  REAL(r64)              :: UAdesign
  REAL(r64)              :: OutletWaterTemp

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64)              :: MdotCpWater          ! Water mass flow rate times the heat capacity [W/K]
  REAL(r64)              :: InletAirTemp         ! Dry-bulb temperature of air entering the fluid cooler [C]
  REAL(r64)              :: CpWater              ! Heat capacity of water [J/kg/K]
  REAL(r64)              :: CpAir                ! Heat capacity of air [J/kg/K]
  REAL(r64)              :: AirDensity           ! Density of air [kg/m3]
  REAL(r64)              :: AirMassFlowRate      ! Mass flow rate of air [kg/s]
  REAL(r64)              :: effectiveness        ! Effectiveness of the heat exchanger [-]
  REAL(r64)              :: OutletAirTemp        ! Drybulb temp of exiting moist air [C]
  REAL(r64)              :: AirCapacity          ! MdotCp of air through the fluid cooler
  REAL(r64)              :: CapacityRatioMin     ! Minimum capacity of airside and waterside
  REAL(r64)              :: CapacityRatioMax     ! Maximum capacity of airside and waterside
  REAL(r64)              :: CapacityRatio        ! Ratio of minimum to maximum capacity
  REAL(r64)              :: NumTransferUnits     ! Number of transfer Units [NTU]
  REAL(r64)              :: Qactual              ! Actual heat transfer rate between fluid cooler water and air [W]
  REAL(r64)              :: ETA                  ! initialize some local variables
  REAL(r64)              :: A                    ! initialize some local variables
  REAL(r64)              :: InletWaterTemp       ! Water inlet temperature

  WaterInletNode    = SimpleFluidCooler(FluidCoolerNum)%WaterInletNodeNum
  WaterOutletNode   = SimpleFluidCooler(FluidCoolerNum)%WaterOutletNodeNum
  Qactual           = 0.0d0
  ! set local fluid cooler inlet and outlet temperature variables
  InletWaterTemp    = SimpleFluidCoolerInlet(FluidCoolerNum)%WaterTemp
  OutletWaterTemp   = InletWaterTemp
  InletAirTemp      = SimpleFluidCoolerInlet(FluidCoolerNum)%AirTemp

  IF(UAdesign.EQ.0.0d0)RETURN

  ! set water and air properties
  AirDensity        = PsyRhoAirFnPbTdbW(SimpleFluidCoolerInlet(FluidCoolerNum)%AirPress,InletAirTemp,  &
                                        SimpleFluidCoolerInlet(FluidCoolerNum)%AirHumRat)
  AirMassFlowRate   = AirFlowRate * AirDensity
  CpAir             = PsyCpAirFnWTdb(SimpleFluidCoolerInlet(FluidCoolerNum)%AirHumRat,InletAirTemp)
  CpWater =  GetSpecificHeatGlycol(PlantLoop(SimpleFluidCooler(FluidCoolerNum)%LoopNum)%FluidName,  &
                                   InletWaterTemp,                                   &
                                   PlantLoop(SimpleFluidCooler(FluidCoolerNum)%LoopNum)%FluidIndex,         &
                                   'SimSimpleFluidCooler')

  ! Calcluate mass flow rates
  MdotCpWater = WaterMassFlowRate * CpWater
  AirCapacity = AirMassFlowRate * CpAir

  ! calculate the minimum to maximum capacity ratios of airside and waterside
  CapacityRatioMin = MIN(AirCapacity,MdotCpWater)
  CapacityRatioMax = MAX(AirCapacity,MdotCpWater)
  CapacityRatio    = CapacityRatioMin/CapacityRatioMax

  ! Calculate number of transfer units (NTU)
  NumTransferUnits = UAdesign/CapacityRatioMin
  ETA=NumTransferUnits**0.22d0
  A=CapacityRatio*NumTransferUnits/ETA
  effectiveness = 1.d0 - Exp((Exp(-A) - 1.d0) / (CapacityRatio / ETA))

  ! calculate water to air heat transfer
  Qactual = effectiveness * CapacityRatioMin * (InletWaterTemp-InletAirTemp)

  ! calculate new exiting dry bulb temperature of airstream
  OutletAirTemp = InletAirTemp + Qactual/AirCapacity

  IF(Qactual .GE. 0.0d0)THEN
    OutletWaterTemp = InletWaterTemp - Qactual/ MdotCpWater
  ELSE
    OutletWaterTemp = InletWaterTemp
  END IF

RETURN
END SUBROUTINE SimSimpleFluidCooler

FUNCTION SimpleFluidCoolerUAResidual(UA, Par) RESULT (Residuum)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Chandan Sharma
          !       DATE WRITTEN   August 2008
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! Calculates residual function (Design fluid cooler load - fluid cooler Output) / Design fluid cooler load.
          ! Fluid cooler output depends on the UA which is being varied to zero the residual.

          ! METHODOLOGY EMPLOYED:
          ! Puts UA into the fluid cooler data structure, calls SimSimpleFluidCooler, and calculates
          ! the residual as defined above.

          ! REFERENCES:
          ! Based on SimpleTowerUAResidual by Fred Buhl, May 2002

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
    REAL(r64), INTENT(IN)  :: UA                         ! UA of fluid cooler
    REAL(r64), INTENT(IN), DIMENSION(:), OPTIONAL :: Par ! par(1) = design fluid cooler load [W]
                                                         ! par(2) = Fluid cooler number
                                                         ! par(3) = design water mass flow rate [kg/s]
                                                         ! par(4) = design air volume flow rate [m3/s]
                                                         ! par(5) = water specific heat [J/(kg*C)]
    REAL(r64)         :: Residuum ! residual to be minimized to zero

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER      :: FluidCoolerIndex                ! index of this fluid cooler
  REAL(r64)    :: OutWaterTemp                    ! outlet water temperature [C]
  REAL(r64)    :: Output                          ! Fluid cooler  output [W]

  FluidCoolerIndex = INT(Par(2))
  CALL SimSimpleFluidCooler(FluidCoolerIndex,Par(3),Par(4),UA,OutWaterTemp)
  Output = Par(5)*Par(3)*(SimpleFluidCoolerInlet(FluidCoolerIndex)%WaterTemp - OutWaterTemp)
  Residuum = (Par(1) - Output) / Par(1)
  RETURN
END FUNCTION SimpleFluidCoolerUAResidual

! End of the CondenserLoopFluidCoolers Module Simulation Subroutines
! *****************************************************************************

! Beginning of Record Keeping subroutines for the FluidCooler Module
! *****************************************************************************

SUBROUTINE UpdateFluidCooler(FluidCoolerNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR:          Chandan Sharma
          !       DATE WRITTEN:    August 2008
          !       MODIFIED         na
          !       RE-ENGINEERED    na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is for passing results to the outlet water node.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataEnvironment, ONLY: EnvironmentName, CurMnDy
!  USE General, ONLY: TrimSigDigits
!  USE FluidProperties, ONLY : GetDensityGlycol
!  USE DataPlant, ONLY : PlantLoop

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)  :: FluidCoolerNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER ::  LowTempFmt="(' ',F6.2)"

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  CHARACTER(len=25)      :: CharErrOut
  CHARACTER(len=25)      :: CharLowOutletTemp
  INTEGER   :: LoopNum
  INTEGER   :: LoopSideNum
  REAL(r64) :: LoopMinTemp

  ! set node information

  Node(WaterOutletNode)%Temp                   = OutletWaterTemp

  LoopNum = SimpleFluidCooler(FluidCoolerNum)%LoopNum
  LoopSideNum = SimpleFluidCooler(FluidCoolerNum)%LoopSideNum
  IF(PlantLoop(LoopNum)%Loopside(LoopSideNum)%FlowLock.EQ.0 .OR. WarmupFlag)RETURN

  !Check flow rate through fluid cooler and compare to design flow rate, show warning if greater than Design * Mulitplier
  IF (Node(WaterOutletNode)%MassFlowRate .GT. SimpleFluidCooler(FluidCoolerNum)%DesWaterMassFlowRate * &
                                              SimpleFluidCooler(FluidCoolerNum)%FluidCoolerMassFlowRateMultiplier) THEN
    SimpleFluidCooler(FluidCoolerNum)%HighMassFlowErrorCount=SimpleFluidCooler(FluidCoolerNum)%HighMassFlowErrorCount+1
    IF (SimpleFluidCooler(FluidCoolerNum)%HighMassFlowErrorCount < 2) THEN
      CALL ShowWarningError (TRIM(SimpleFluidCooler(FluidCoolerNum)%FluidCoolerType)//' "'//  &
         TRIM(SimpleFluidCooler(FluidCoolerNum)%Name)//'"')
      CALL ShowContinueError (' Condenser Loop Mass Flow Rate is much greater than the fluid coolers design mass flow rate.')
      CALL ShowContinueError (' Condenser Loop Mass Flow Rate = '//TrimSigDigits(Node(WaterOutletNode)%MassFlowRate,6))
      CALL ShowContinueError (' Fluid Cooler Design Mass Flow Rate   = '//  &
                                TrimSigDigits(SimpleFluidCooler(FluidCoolerNum)%DesWaterMassFlowRate,6))
      CALL ShowContinueErrorTimeStamp(' ')
    ELSE
      CALL ShowRecurringWarningErrorAtEnd(TRIM(SimpleFluidCooler(FluidCoolerNum)%FluidCoolerType)//' "'//  &
         TRIM(SimpleFluidCooler(FluidCoolerNum)%Name)//&
          '"  Condenser Loop Mass Flow Rate is much greater than the fluid coolers design mass flow rate error continues...' &
          , SimpleFluidCooler(FluidCoolerNum)%HighMassFlowErrorIndex, Node(WaterOutletNode)%MassFlowRate,   &
             Node(WaterOutletNode)%MassFlowRate)
    ENDIF
  END IF

   ! Check if OutletWaterTemp is below the minimum condenser loop temp and warn user
   LoopMinTemp = PlantLoop(LoopNum)%MinTemp
   IF(OutletWaterTemp.LT.LoopMinTemp .AND. WaterMassFlowRate > 0.0d0) THEN
     SimpleFluidCooler(FluidCoolerNum)%OutletWaterTempErrorCount = SimpleFluidCooler(FluidCoolerNum)%OutletWaterTempErrorCount &
                                                                         + 1
     WRITE(CharLowOutletTemp,LowTempFmt) LoopMinTemp
     WRITE(CharErrOut,LowTempFmt) OutletWaterTemp
     CharErrOut=ADJUSTL(CharErrOut)
     IF (SimpleFluidCooler(FluidCoolerNum)%OutletWaterTempErrorCount < 2) THEN
       CALL ShowWarningError (TRIM(SimpleFluidCooler(FluidCoolerNum)%FluidCoolerType)//' "'//  &
          TRIM(SimpleFluidCooler(FluidCoolerNum)%Name)//'"')
       CALL ShowContinueError (' Fluid cooler water outlet temperature ('//TRIM(CharErrOut)//' C) is '// &
                               'below the specified minimum condenser loop temp of '//TRIM(ADJUSTL(CharLowOutletTemp))//' C')
       CALL ShowContinueErrorTimeStamp(' ')
     ELSE
       CALL ShowRecurringWarningErrorAtEnd(TRIM(SimpleFluidCooler(FluidCoolerNum)%FluidCoolerType)//' "'//  &
          TRIM(SimpleFluidCooler(FluidCoolerNum)%Name)//&
          '" Fluid cooler water outlet temperature is below the specified minimum condenser loop temp error continues...' &
          , SimpleFluidCooler(FluidCoolerNum)%OutletWaterTempErrorIndex, OutletWaterTemp, OutletWaterTemp)
     END IF
   END IF

   ! Check if water mass flow rate is small (e.g. no flow) and warn user
   IF(WaterMassFlowRate .GT. 0.0d0 .AND. WaterMassFlowRate .LE. MassFlowTolerance)THEN
     SimpleFluidCooler(FluidCoolerNum)%SmallWaterMassFlowErrorCount =   &
        SimpleFluidCooler(FluidCoolerNum)%SmallWaterMassFlowErrorCount + 1
     IF (SimpleFluidCooler(FluidCoolerNum)%SmallWaterMassFlowErrorCount < 2) THEN
       CALL ShowWarningError (TRIM(SimpleFluidCooler(FluidCoolerNum)%FluidCoolerType)//' "'//  &
          TRIM(SimpleFluidCooler(FluidCoolerNum)%Name)//'"')
       CALL ShowContinueError (' Fluid cooler water mass flow rate near zero.')
       CALL ShowContinueErrorTimeStamp(' ')
       CALL ShowContinueError('Actual Mass flow = '//TRIM(TrimSigDigits(WaterMassFlowRate,2)))
     ELSE
       CALL ShowRecurringWarningErrorAtEnd(TRIM(SimpleFluidCooler(FluidCoolerNum)%FluidCoolerType)//' "'//  &
          TRIM(SimpleFluidCooler(FluidCoolerNum)%Name)//&
          '" Fluid cooler water mass flow rate near zero error continues...' &
          , SimpleFluidCooler(FluidCoolerNum)%SmallWaterMassFlowErrorIndex, WaterMassFlowRate, WaterMassFlowRate)
     ENDIF
   END IF

!   ! Check if water mass flow rate is lower than loop minimum and warn user
!   IF(WaterMassFlowRate .LT. LoopMassFlowRateMinAvail)THEN
!     SimpleFluidCooler(FluidCoolerNum)%WMFRLessThanMinAvailErrCount =   &
!        SimpleFluidCooler(FluidCoolerNum)%WMFRLessThanMinAvailErrCount + 1
!     IF (SimpleFluidCooler(FluidCoolerNum)%WMFRLessThanMinAvailErrCount < 2) THEN
!       CALL ShowWarningError (TRIM(SimpleFluidCooler(FluidCoolerNum)%FluidCoolerType)//' "'//  &
!          TRIM(SimpleFluidCooler(FluidCoolerNum)%Name)//'"')
!       CALL ShowContinueError (' Fluid cooler water mass flow below loop minimum.')
!       CALL ShowContinueErrorTimeStamp(' ')
!       CALL ShowContinueError('Actual Mass flow  = '//TRIM(TrimSigDigits(WaterMassFlowRate,2)))
!       CALL ShowContinueError('Loop Minimum flow = '//TRIM(TrimSigDigits(LoopMassFlowRateMinAvail,2)))
!     ELSE
!       CALL ShowRecurringWarningErrorAtEnd(TRIM(SimpleFluidCooler(FluidCoolerNum)%FluidCoolerType)//' "'//  &
!          TRIM(SimpleFluidCooler(FluidCoolerNum)%Name)//&
!          '" Fluid cooler water mass flow rate below loop minimum error continues...' &
!          , SimpleFluidCooler(FluidCoolerNum)%WMFRLessThanMinAvailErrIndex, WaterMassFlowRate, WaterMassFlowRate)
!     ENDIF
!   END IF
!
!   ! Check if water mass flow rate is greater than loop maximum and warn user
!   IF(WaterMassFlowRate .GT. LoopMassFlowRateMaxAvail)THEN
!     SimpleFluidCooler(FluidCoolerNum)%WMFRGreaterThanMaxAvailErrCount =   &
!        SimpleFluidCooler(FluidCoolerNum)%WMFRGreaterThanMaxAvailErrCount + 1
!     IF (SimpleFluidCooler(FluidCoolerNum)%WMFRGreaterThanMaxAvailErrCount < 2) THEN
!       CALL ShowWarningError (TRIM(SimpleFluidCooler(FluidCoolerNum)%FluidCoolerType)//' "'//  &
!          TRIM(SimpleFluidCooler(FluidCoolerNum)%Name)//'"')
!       CALL ShowContinueError (' Fluid cooler water mass flow above loop maximum.')
!       CALL ShowContinueErrorTimeStamp(' ')
!       CALL ShowContinueError('Actual Mass flow = '//TRIM(TrimSigDigits(WaterMassFlowRate,2)))
!       CALL ShowContinueError('Loop Maximum flow = '//TRIM(TrimSigDigits(LoopMassFlowRateMaxAvail,2)))
!     ELSE
!       CALL ShowRecurringWarningErrorAtEnd(TRIM(SimpleFluidCooler(FluidCoolerNum)%FluidCoolerType)//' "'//  &
!          TRIM(SimpleFluidCooler(FluidCoolerNum)%Name)//&
!          '" Fluid cooler water mass flow rate above loop maximum error continues...' &
!          , SimpleFluidCooler(FluidCoolerNum)%WMFRGreaterThanMaxAvailErrIndex, WaterMassFlowRate, WaterMassFlowRate)
!     ENDIF
!   END IF

RETURN
END SUBROUTINE UpdateFluidCooler
! End of Record Keeping subroutines for the FluidCooler Module
! *****************************************************************************

! Beginning of Reporting subroutines for the FluidCooler Module
! *****************************************************************************

SUBROUTINE ReportFluidCooler(RunFlag, FluidCoolerNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR:          Chandan Sharma
          !       DATE WRITTEN:    August 2008
          !       MODIFIED         na
          !       RE-ENGINEERED    na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine updates the report variables for the fluid cooler.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  LOGICAL, INTENT(IN) :: RunFlag
  INTEGER, INTENT(IN) :: FluidCoolerNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64) :: ReportingConstant

  ReportingConstant = TimeStepSys*SecInHour

  IF (.NOT. RunFlag)THEN
    SimpleFluidCoolerReport(FluidCoolerNum)%InletWaterTemp         = Node(WaterInletNode)%Temp
    SimpleFluidCoolerReport(FluidCoolerNum)%OutletWaterTemp        = Node(WaterInletNode)%Temp
    SimpleFluidCoolerReport(FluidCoolerNum)%WaterMassFlowRate      = WaterMassFlowRate
    SimpleFluidCoolerReport(FluidCoolerNum)%Qactual                = 0.0d0
    SimpleFluidCoolerReport(FluidCoolerNum)%FanPower               = 0.0d0
    SimpleFluidCoolerReport(FluidCoolerNum)%FanEnergy              = 0.0d0

  ELSE
    SimpleFluidCoolerReport(FluidCoolerNum)%InletWaterTemp         = Node(WaterInletNode)%Temp
    SimpleFluidCoolerReport(FluidCoolerNum)%OutletWaterTemp        = OutletWaterTemp
    SimpleFluidCoolerReport(FluidCoolerNum)%WaterMassFlowRate      = WaterMassFlowRate
    SimpleFluidCoolerReport(FluidCoolerNum)%Qactual                = Qactual
    SimpleFluidCoolerReport(FluidCoolerNum)%FanPower               = FanPower
    SimpleFluidCoolerReport(FluidCoolerNum)%FanEnergy              = FanPower*ReportingConstant

  END IF

RETURN
END SUBROUTINE ReportFluidCooler

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
END MODULE FluidCoolers
