MODULE WaterCoils
  ! Module containing the WaterCoil simulation routines

  ! MODULE INFORMATION:
  !       AUTHOR         Richard J. Liesen
  !       DATE WRITTEN   April 1998
  !       MODIFIED       April 2004: Rahul Chillar
  !                      Feb. 2010, Brent Griffith, Plant Demand Side Update, general fluid properties
  !       RE-ENGINEERED  na

  ! PURPOSE OF THIS MODULE:
  ! To encapsulate the data and algorithms required to
  ! manage the WaterCoil System Component

  ! METHODOLOGY EMPLOYED:
  !

  ! REFERENCES:


  ! OTHER NOTES:
  !

  ! USE STATEMENTS:
  ! Use statements for data only modules
USE DataPrecisionGlobals
USE DataLoopNode
USE DataGlobals
USE DataInterfaces
USE DataEnvironment, ONLY: OutBaroPress, StdBaroPress, StdRhoAir
USE DataHVACGlobals
USe Psychrometrics,  ONLY : PsyCpAirFnWTdb, PsyHFnTdbW, PsyTdpFnWPb, PsyWFnTdbH, &
                           PsyWFnTdpPb, PsyTdbFnHW, PsyWFnTdbRhPb, PsyWFnTdbTwbPb, &
                           PsyRhoAirFnPbTdbW, PsyHFnTdbRhPb, PsyTsatFnHPb
USE FluidProperties, ONLY: GetSpecificHeatGlycol, GetDensityGlycol
USE DataPlant,       ONLY: TypeOf_CoilWaterCooling, TypeOf_CoilWaterDetailedFlatCooling, &
                           TypeOf_CoilWaterSimpleHeating, PlantLoop, MyPlantSizingIndex

  ! Use statements for access to subroutines in other modules
USE ScheduleManager

IMPLICIT NONE         ! Enforce explicit typing of all variables

!PRIVATE ! Everything private unless explicitly made public

  !MODULE PARAMETER DEFINITIONS

INTEGER, PARAMETER :: MaxPolynomOrder = 4
INTEGER, PARAMETER :: MaxOrderedPairs = 60

REAL(r64), PARAMETER :: PolyConvgTol = 1.D-05
REAL(r64), PARAMETER :: MinWaterMassFlowFrac = 0.000001d0
REAL(r64), PARAMETER :: MinAirMassFlow = 0.001d0

! coil types in this module
INTEGER, PARAMETER :: WaterCoil_SimpleHeating    = TypeOf_CoilWaterSimpleHeating
INTEGER, PARAMETER :: WaterCoil_DetFlatFinCooling  = TypeOf_CoilWaterDetailedFlatCooling
INTEGER, PARAMETER :: WaterCoil_Cooling  = TypeOf_CoilWaterCooling

INTEGER, PARAMETER :: CoilType_Cooling=1
INTEGER, PARAMETER :: CoilType_Heating=2

INTEGER, PARAMETER :: CoilModel_Simple=1
INTEGER, PARAMETER :: CoilModel_Cooling=2
INTEGER, PARAMETER :: CoilModel_Detailed=3

! Parameters for Heat Exchanger Configuration
INTEGER, PARAMETER :: CounterFlow = 1
INTEGER, PARAMETER :: CrossFlow   = 2
INTEGER, PARAMETER :: SimpleAnalysis = 1
INTEGER, PARAMETER :: DetailedAnalysis = 2

!Water Systems
INTEGER, PARAMETER :: CondensateDiscarded = 1001 ! default mode where water is "lost"
INTEGER, PARAMETER :: CondensateToTank    = 1002 ! collect coil condensate from air and store in water storage tank

!Parameters for COIL:Water:SimpleHeating Coil Performance Input Method
INTEGER, PARAMETER :: UAandFlow = 1 ! for Coil Performance Input Method = UA and Design Water Flow Rate
INTEGER, PARAMETER :: NomCap    = 2 ! for Coil Performance Input Method = Nominal Capacity

! Parameters Subroutine CoolingCoil: design calc or simulation calc.
INTEGER, PARAMETER :: DesignCalc = 1 ! ignore on/off check in CoolingCoil
INTEGER, PARAMETER :: SimCalc = 2    ! pay attention to on/off check in CoolingCoil

  ! DERIVED TYPE DEFINITIONS
TYPE WaterCoilEquipConditions
  CHARACTER(len=MaxNameLength) :: Name           = ' ' ! Name of the WaterCoil
  CHARACTER(len=10)           :: WaterCoilTypeA  = ' ' ! Type of WaterCoil ie. Heating or Cooling
  CHARACTER(len=20)           :: WaterCoilModelA = ' ' ! Type of WaterCoil ie. Simple, Detailed, etc.
  INTEGER      :: WaterCoilType                  = 0    ! Type of WaterCoil ie. Heating or Cooling
  INTEGER      :: WaterCoilModel                 = 0    ! Type of WaterCoil ie. Simple, Detailed, etc.
  INTEGER                   :: WaterCoilType_Num = 0
  CHARACTER(len=MaxNameLength) :: Schedule       = ' ' ! WaterCoil Operation Schedule
  INTEGER      :: SchedPtr                       = 0   ! Pointer to the correct schedule
  LOGICAL      :: RequestingAutoSize             = .false.  ! True if this coil has appropriate autosize fields
  REAL(r64)    :: InletAirMassFlowRate           = 0.0d0 ! MassFlow through the WaterCoil being Simulated [kg/s]
  REAL(r64)    :: OutletAirMassFlowRate          = 0.0d0 ! MassFlow throught the WaterCoil being Simulated[kg/s]
  REAL(r64)    :: InletAirTemp                   = 0.0d0 ! Inlet Air Temperature Operating Condition [C]
  REAL(r64)    :: OutletAirTemp                  = 0.0d0 ! Outlet Air Temperature Operating Condition [C]
  REAL(r64)    :: InletAirHumRat                 = 0.0d0 ! Inlet Air Humidity Ratio Operating Condition
  REAL(r64)    :: OutletAirHumRat                = 0.0d0 ! Outlet Air Humidity Ratio Calculated Condition
  REAL(r64)    :: InletAirEnthalpy               = 0.0d0 ! Inlet Air enthalpy [J/kg]
  REAL(r64)    :: OutletAirEnthalpy              = 0.0d0 ! Outlet Air enthalpy [J/kg]
  REAL(r64)    :: TotWaterCoilLoad               = 0.0d0 ! Total Load on the Coil [W]
  REAL(r64)    :: SenWaterCoilLoad               = 0.0d0 ! Sensible Load on the Coil [W]
  REAL(r64)    :: TotWaterHeatingCoilEnergy      = 0.0d0 ! Total Heating Coil energy of the Coil [J]
  REAL(r64)    :: TotWaterCoolingCoilEnergy      = 0.0d0 ! Total Cooling Coil energy of the Coil [J]
  REAL(r64)    :: SenWaterCoolingCoilEnergy      = 0.0d0 ! Sensible Cooling Coil energy of the Coil [J]
  REAL(r64)    :: DesWaterHeatingCoilRate        = 0.0d0 ! Design Heating Coil Rate used for sizing [W]
  REAL(r64)    :: TotWaterHeatingCoilRate        = 0.0d0 ! Total Heating Coil Rate on the Coil [W]
  REAL(r64)    :: DesWaterCoolingCoilRate        = 0.0d0 ! Design Cooling Coil Rate used for sizing [W]
  REAL(r64)    :: TotWaterCoolingCoilRate        = 0.0d0 ! Total Cooling Coil Rate on the Coil [W]
  REAL(r64)    :: SenWaterCoolingCoilRate        = 0.0d0 ! Sensible Cooling Coil Rate on the Coil [W]
  REAL(r64)    :: UACoil                         = 0.0d0 ! WaterCoil UA Value
  REAL(r64)    :: LeavingRelHum                  = 0.0d0 ! Simple Coil Latent Model requires User input for leaving RH
  REAL(r64)    :: DesiredOutletTemp              = 0.0d0 !
  REAL(r64)    :: DesiredOutletHumRat            = 0.0d0 !
  REAL(r64)    :: InletWaterTemp                 = 0.0d0 ! Inlet Water Temperature [C]
  REAL(r64)    :: OutletWaterTemp                = 0.0d0 ! Outlet Water Temperature [C]
  REAL(r64)    :: InletWaterMassFlowRate         = 0.0d0 ! Inlet Water Mass Flow Rate [Kg/s]
  REAL(r64)    :: OutletWaterMassFlowRate        = 0.0d0 ! Outlet Water Mass Flow Rate [Kg/s]
  REAL(r64)    :: MaxWaterVolFlowRate            = 0.0d0 ! Maximum water Volume flow rate [m3/s]
  REAL(r64)    :: MaxWaterMassFlowRate           = 0.0d0 ! Maximum water mass flow rate [Kg/s]
  REAL(r64)    :: InletWaterEnthalpy             = 0.0d0 ! Inlet Water Enthalpy
  REAL(r64)    :: OutletWaterEnthalpy            = 0.0d0 ! Outlet Water Enthalpy
  !These are the additional Geometry and Design Variables for Detailed Flat Fin Coil
  REAL(r64)    :: TubeOutsideSurfArea            = 0.0d0 !Tube Primary Surface Area
  REAL(r64)    :: TotTubeInsideArea              = 0.0d0 !Total Tube inside Surface Area
  REAL(r64)    :: FinSurfArea                    = 0.0d0 !Fin Surface Area
  REAL(r64)    :: MinAirFlowArea                 = 0.0d0 !
  REAL(r64)    :: CoilDepth                      = 0.0d0 !
  REAL(r64)    :: FinDiam                        = 0.0d0 !Fin Diameter or the Coil Height
  REAL(r64)    :: FinThickness                   = 0.0d0 !
  REAL(r64)    :: TubeInsideDiam                 = 0.0d0 !Inner diameter of Tubes
  REAL(r64)    :: TubeOutsideDiam                = 0.0d0 !Outer Diameter of the Tubes
  REAL(r64)    :: TubeThermConductivity          = 0.0d0 !
  REAL(r64)    :: FinThermConductivity           = 0.0d0 !
  REAL(r64)    :: FinSpacing                     = 0.0d0 !Fin Spacing or Distance
  REAL(r64)    :: TubeDepthSpacing               = 0.0d0 !
  INTEGER      :: NumofTubeRows                  = 0   !
  INTEGER      :: NumofTubesperRow               = 0   !
  !BEGIN calculated parameters for detailed flat fin coil
  REAL(r64)    :: EffectiveFinDiam               = 0.0d0 !
  REAL(r64)    :: TotCoilOutsideSurfArea         = 0.0d0 !
  REAL(r64)    :: CoilEffectiveInsideDiam        = 0.0d0 !
  REAL(r64)    :: GeometryCoef1                  = 0.0d0 !
  REAL(r64)    :: GeometryCoef2                  = 0.0d0 !
  REAL(r64)    :: DryFinEfficncyCoef(5)          = 0.0d0 !
  REAL(r64)    :: SatEnthlCurveConstCoef         = 0.0d0 !
  REAL(r64)    :: SatEnthlCurveSlope             = 0.0d0 !
  REAL(r64)    :: EnthVsTempCurveAppxSlope       = 0.0d0 !
  REAL(r64)    :: EnthVsTempCurveConst           = 0.0d0 !
  REAL(r64)    :: MeanWaterTempSaved             = 0.0d0 !
  REAL(r64)    :: InWaterTempSaved               = 0.0d0 !
  REAL(r64)    :: OutWaterTempSaved              = 0.0d0 !
  REAL(r64)    :: SurfAreaWetSaved               = 0.0d0 !
  REAL(r64)    :: SurfAreaWetFraction            = 0.0d0 !
  !END calculated parameters for detailed flat fin coil
  ! Design Input Variables to the Design Detailed Simple inputs model
  REAL(r64)    :: DesInletWaterTemp         = 0.0d0 ! Entering water temperature at Design(C)
  REAL(r64)    :: DesAirVolFlowRate         = 0.0d0 ! Entering Air Volume Flow Rate Design( m3/s)
  REAL(r64)    :: DesInletAirTemp           = 0.0d0 ! Entering air dry bulb temperature at Design(C)
  REAL(r64)    :: DesInletAirHumRat         = 0.0d0 ! Entering air humidity ratio at design conditions
  REAL(r64)    :: DesTotWaterCoilLoad       = 0.0d0 ! Total heat transfer rate at Design(Watt)
  REAL(r64)    :: DesSenWaterCoilLoad       = 0.0d0 ! Sensible heat transfer rate at Design(Watt)
  !BEGIN calculated parameters for Design Detailed Simple inputs model
  REAL(r64)    :: DesAirMassFlowRate        = 0.0d0 ! Design Air MassFlow through the WaterCoil [kg/Sec]
  REAL(r64)    :: UACoilTotal               = 0.0d0 ! Overall external dry UA (W/C)
  REAL(r64)    :: UACoilInternal            = 0.0d0 ! Overall internal UA(W/C)
  REAL(r64)    :: UACoilExternal            = 0.0d0 ! Overall external heat transfer coefficient(W/C)
  REAL(r64)    :: UACoilInternalDes         = 0.0d0 ! Overall design internal UA(W/C)
  REAL(r64)    :: UACoilExternalDes         = 0.0d0 ! Overall design external heat transfer coefficient(W/C)
  REAL(r64)    :: DesOutletAirTemp          = 0.0d0 ! Leaving air temperature at rating(C)
  REAL(r64)    :: DesOutletAirHumRat        = 0.0d0 ! Humidity ratio of air leaving at design capacity.
  REAL(r64)    :: DesOutletWaterTemp        = 0.0d0 ! Temp of Liquid Leaving the Coil at design Capacity
  INTEGER      :: HeatExchType              = 0   ! Heat exchanger configuration, default to Cross Flow
  INTEGER      :: CoolingCoilAnalysisMode   = 0   ! Mode Of analysis, Simple=1 and Detailed =2
                                                  !    Simple= AllWet-AllDry, Detailed= PartWet-PartDry
  REAL(r64)    :: UACoilInternalPerUnitArea = 0.0d0 ! Internal overall heat transfer coefficient(W/m2 C)
  REAL(r64)    :: UAWetExtPerUnitArea       = 0.0d0 ! External overall heat transfer coefficient(W/m2 C)
  REAL(r64)    :: UADryExtPerUnitArea       = 0.0d0 ! External overall heat transfer coefficient(W/m2 C)
  REAL(r64)    :: SurfAreaWetFractionSaved  = 0.0d0 ! Previous saved value, for numerical efficiency.
  !END calculated parameters for Design Inputs Detailed coil

  ! variables for simple heating coil with variable UA
  REAL(r64)    :: UACoilVariable                 = 0.d0 ! WaterCoil UA value when variable (simple heating coil only)
  REAL(r64)    :: RatioAirSideToWaterSideConvect = 1.d0 !"r" value for coil,
  REAL(r64)    :: AirSideNominalConvect          = 0.d0 ! nominal rating point air side convection term (fin_effic*(hc*A))
  REAL(r64)    :: LiquidSideNominalConvect       = 0.d0 ! nominal rating point water side convection term (hc*A)

  INTEGER      :: Control                        = 0   !Const Vol =1;  Variable Vol = 2
  INTEGER      :: AirInletNodeNum                = 0   !
  INTEGER      :: AirOutletNodeNum               = 0   !
  INTEGER      :: WaterInletNodeNum              = 0   !
  INTEGER      :: WaterOutletNodeNum             = 0   !
  INTEGER      :: WaterLoopNum                   = 0   ! Plant loop index
  INTEGER      :: WaterLoopSide                  = 0   ! Plant loop side index
  INTEGER      :: WaterLoopBranchNum             = 0   ! Plant loop branch index
  INTEGER      :: WaterLoopCompNum               = 0   ! Plant loop Comp index

  !begin variables for Water System interactions
  INTEGER ::CondensateCollectMode                 = CondensateDiscarded !  where does water come from
  CHARACTER(len=MaxNameLength) :: CondensateCollectName = ' ' ! name of water source e.g. water storage tank
  INTEGER ::CondensateTankID                      = 0 !index "pointer" to Storage TAnk array WaterStorage
  INTEGER ::CondensateTankSupplyARRID             = 0 !index pointe to supply Vdot array in WaterStorage
  REAL(r64)   :: CondensateVdot = 0.0d0 ! rate of water condensation from air stream [m3/s]
  REAL(r64)   :: CondensateVol  = 0.0d0 ! amount of water condensed from air stream [m3]
  !end variables for water system interactions

  !COIL:Water:SimpleHeating Coil Performance Input Method
  INTEGER :: CoilPerfInpMeth = 0 ! 1 = UA and Design Water Flow Rate; 2 = Nominal Capacity

END TYPE WaterCoilEquipConditions

  !MODULE VARIABLE DECLARATIONS:
  INTEGER :: NumWaterCoils =0  ! The Number of WaterCoils found in the Input
  TYPE (WaterCoilEquipConditions), ALLOCATABLE, DIMENSION(:) :: WaterCoil
  LOGICAL, ALLOCATABLE, DIMENSION(:) :: MySizeFlag
  LOGICAL, ALLOCATABLE, DIMENSION(:) :: MyUAAndFlowCalcFlag
  LOGICAL, ALLOCATABLE, DIMENSION(:) :: MyCoilDesignFlag
  LOGICAL, ALLOCATABLE, DIMENSION(:) :: CoilWarningOnceFlag
  INTEGER, ALLOCATABLE, DIMENSION(:) :: WaterTempCoolCoilErrs  ! error counting for detailed coils
  INTEGER, ALLOCATABLE, DIMENSION(:) :: PartWetCoolCoilErrs    ! error counting for detailed coils
  Logical        :: GetWaterCoilsInputFlag = .True. ! Flag set to make sure you get input once
  LOGICAL, ALLOCATABLE, DIMENSION(:) :: CheckEquipName

! Subroutine Specifications for the Module
          ! Driver/Manager Routines
PUBLIC  SimulateWaterCoilComponents

          ! Get Input routines for module
PRIVATE GetWaterCoilInput

          ! Initialization routines for module
PRIVATE InitWaterCoil
PRIVATE SizeWaterCoil

          ! Algorithms for the module
PRIVATE CalcSimpleHeatingCoil
PRIVATE CalcDetailFlatFinCoolingCoil
PRIVATE CoolingCoil

          ! Update routine to check convergence and update nodes
PRIVATE UpdateWaterCoil

          ! Reporting routines for module
PRIVATE ReportWaterCoil

          ! Other routines
PRIVATE SimpleHeatingCoilUAResidual
PRIVATE SimpleCoolingCoilUAResidual
PUBLIC  CheckWaterCoilSchedule
PUBLIC  GetCoilMaxWaterFlowRate
PUBLIC  GetCoilInletNode
PUBLIC  GetCoilOutletNode
PUBLIC  GetCoilWaterInletNode
PUBLIC  GetCoilWaterOutletNode
PUBLIC  UpdateWaterToAirCoilPlantConnection  !DSU3
PRIVATE EstimateHEXSurfaceArea
PUBLIC  GetWaterCoilIndex
PUBLIC  GetWaterCoilCapacity
PUBLIC  CheckForSensorAndSetpointNode
PUBLIC  GetWaterCoilAvailScheduleIndex

CONTAINS

! MODULE SUBROUTINES:
!*************************************************************************
SUBROUTINE SimulateWaterCoilComponents(CompName,FirstHVACIteration,CompIndex, QActual, FanOpMode, PartLoadRatio)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Liesen
          !       DATE WRITTEN   February 1998
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine manages WaterCoil component simulation.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: FindItemInList
  USE General, ONLY: TrimSigDigits

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: CompName
  LOGICAL, INTENT (IN):: FirstHVACIteration
  INTEGER, INTENT(INOUT)  :: CompIndex
  REAL(r64), OPTIONAL, INTENT(INOUT)  :: QActual
  INTEGER, OPTIONAL, INTENT(IN)  :: FanOpMode
  REAL(r64), OPTIONAL, INTENT(IN)  :: PartLoadRatio

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER  :: CoilNum      ! The WaterCoil that you are currently loading input into
  INTEGER  :: OpMode       ! fan operating mode
  REAL(r64)     :: PartLoadFrac ! part-load fraction of heating coil

          ! FLOW:

  ! Obtains and Allocates WaterCoil related parameters from input file
  IF (GetWaterCoilsInputFlag) THEN  !First time subroutine has been entered
    CALL GetWaterCoilInput
    GetWaterCoilsInputFlag=.false.
  End If


  ! Find the correct WaterCoilNumber with the Coil Name
  IF (CompIndex == 0) THEN
    CoilNum = FindItemInList(CompName,WaterCoil%Name,NumWaterCoils)
    IF (CoilNum == 0) THEN
      CALL ShowFatalError('SimulateWaterCoilComponents: Coil not found='//TRIM(CompName))
    ENDIF
    CompIndex=CoilNum
  ELSE
    CoilNum=CompIndex
    IF (CoilNum > NumWaterCoils .or. CoilNum < 1) THEN
      CALL ShowFatalError('SimulateWaterCoilComponents: Invalid CompIndex passed='//TRIM(TrimSigDigits(CoilNum))// &
                          ', Number of Water Coils='//TRIM(TrimSigDigits(NumWaterCoils))//', Coil name='//TRIM(CompName))
    ENDIF
    IF (CheckEquipName(CoilNum)) THEN
      IF (CompName /= WaterCoil(CoilNum)%Name) THEN
        CALL ShowFatalError('SimulateWaterCoilComponents: Invalid CompIndex passed='//TRIM(TrimSigDigits(CoilNum))// &
                            ', Coil name='//TRIM(CompName)//', stored Coil Name for that index='//TRIM(WaterCoil(CoilNum)%Name))
      ENDIF
      CheckEquipName(CoilNum)=.false.
    ENDIF
  ENDIF

  ! With the correct CoilNum Initialize
  CALL InitWaterCoil(CoilNum,FirstHVACIteration) ! Initialize all WaterCoil related parameters

  IF(PRESENT(FanOpMode))THEN
    OpMode = FanOpMode
  ELSE
    OpMode = ContFanCycCoil
  END IF
  IF(PRESENT(PartLoadRatio))THEN
    PartLoadFrac = PartLoadRatio
  ELSE
    PartLoadFrac = 1.0d0
  END IF

  ! Calculate the Correct WaterCoil Model with the current CoilNum
  If(WaterCoil(CoilNum)%WaterCoilType_Num == WaterCoil_DetFlatFinCooling) Then
       Call CalcDetailFlatFinCoolingCoil(CoilNum, SimCalc,OpMode,PartLoadFrac)
       IF(PRESENT(QActual))QActual = WaterCoil(CoilNum)%SenWaterCoolingCoilRate
  ElseIf(WaterCoil(CoilNum)%WaterCoilType_Num == WaterCoil_Cooling) Then
       Call CoolingCoil(CoilNum, FirstHVACIteration, SimCalc,OpMode,PartLoadFrac)
       IF(PRESENT(QActual))QActual = WaterCoil(CoilNum)%SenWaterCoolingCoilRate
  End If

  If(WaterCoil(CoilNum)%WaterCoilType_Num == WaterCoil_SimpleHeating) Then
       Call CalcSimpleHeatingCoil(CoilNum, OpMode, PartLoadFrac, SimCalc)
       IF(PRESENT(QActual))QActual = WaterCoil(CoilNum)%TotWaterHeatingCoilRate
  End If


  ! Update the current WaterCoil to the outlet nodes
  Call UpdateWaterCoil(CoilNum)

  ! Report the current WaterCoil
  Call ReportWaterCoil(CoilNum)

  RETURN

END SUBROUTINE SimulateWaterCoilComponents


! Get Input Section of the Module
!******************************************************************************
SUBROUTINE GetWaterCoilInput

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Liesen
          !       DATE WRITTEN   April 1998
          !       MODIFIED       April 2004: Rahul Chillar
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Obtains input data for coils and stores it in coil data structures

          ! METHODOLOGY EMPLOYED:
          ! Uses "Get" routines to read in data.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataSizing, ONLY: AutoSize
  USE InputProcessor
  USE NodeInputManager, ONLY: GetOnlySingleNode
  USE BranchNodeConnections, ONLY: TestCompSet
  USE WaterManager, ONLY: SetupTankSupplyComponent
  USE DataIPShortCuts
  USE GlobalNames, ONLY: VerifyUniqueCoilName
  USE SetPointManager,            ONLY: NodeHasSPMCtrlVarType, iCtrlVarType_Temp, iCtrlVarType_HumRat

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
    CHARACTER(len=*), PARAMETER  :: RoutineName='GetWaterCoilInput: ' ! include trailing blank space

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    INTEGER :: CoilNum      ! The WaterCoil that you are currently loading input into
    INTEGER :: NumSimpHeat=0
    INTEGER :: NumFlatFin=0
    INTEGER :: NumCooling=0
    INTEGER :: SimpHeatNum
    INTEGER :: FlatFinNum
    INTEGER :: CoolingNum
    INTEGER :: NumAlphas
    INTEGER :: NumNums
    INTEGER :: IOSTAT
    CHARACTER(len=MaxNameLength) :: CurrentModuleObject  ! for ease in getting objects
    CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: AlphArray      ! Alpha input items for object
    CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: cAlphaFields   ! Alpha field names
    CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: cNumericFields ! Numeric field names
    REAL(r64), ALLOCATABLE, DIMENSION(:) :: NumArray          ! Numeric input items for object
    LOGICAL, ALLOCATABLE, DIMENSION(:)   :: lAlphaBlanks      ! Logical array, alpha field input BLANK = .true.
    LOGICAL, ALLOCATABLE, DIMENSION(:)   :: lNumericBlanks    ! Logical array, numeric field input BLANK = .true.
    INTEGER :: MaxNums=0               ! Maximum number of numeric input fields
    INTEGER :: MaxAlphas=0             ! Maximum number of alpha input fields
    INTEGER :: TotalArgs=0             ! Total number of alpha and numeric arguments (max) for a
                                       !  certain object in the input file
    LOGICAL :: ErrorsFound = .false.   ! If errors detected in input
    LOGICAL :: IsNotOK                 ! Flag to verify name
    LOGICAL :: IsBlank                 ! Flag for blank name
    LOGICAL :: errflag

          ! Flow
     NumSimpHeat   = GetNumObjectsFound('Coil:Heating:Water')
     NumFlatFin    = GetNumObjectsFound('Coil:Cooling:Water:DetailedGeometry')
     NumCooling    = GetNumObjectsFound('Coil:Cooling:Water')
     NumWaterCoils = NumSimpHeat + NumFlatFin + NumCooling

     IF (NumWaterCoils.GT.0) THEN
       ALLOCATE(WaterCoil(NumWaterCoils))
       ALLOCATE(WaterTempCoolCoilErrs(NumWaterCoils))
       WaterTempCoolCoilErrs=0
       ALLOCATE(PartWetCoolCoilErrs(NumWaterCoils))
       PartWetCoolCoilErrs=0
       ALLOCATE(CheckEquipName(NumWaterCoils))
       CheckEquipName=.true.
     ENDIF

     CALL GetObjectDefMaxArgs('Coil:Heating:Water',TotalArgs,NumAlphas,NumNums)
     MaxNums=MAX(MaxNums,NumNums)
     MaxAlphas=MAX(MaxAlphas,NumAlphas)
     CALL GetObjectDefMaxArgs('Coil:Cooling:Water:DetailedGeometry',TotalArgs,NumAlphas,NumNums)
     MaxNums=MAX(MaxNums,NumNums)
     MaxAlphas=MAX(MaxAlphas,NumAlphas)
     CALL GetObjectDefMaxArgs('Coil:Cooling:Water',TotalArgs,NumAlphas,NumNums)
     MaxNums=MAX(MaxNums,NumNums)
     MaxAlphas=MAX(MaxAlphas,NumAlphas)

     ALLOCATE(AlphArray(MaxAlphas))
     AlphArray=' '
     ALLOCATE(cAlphaFields(MaxAlphas))
     cAlphaFields=' '
     ALLOCATE(cNumericFields(MaxNums))
     cNumericFields=' '
     ALLOCATE(NumArray(MaxNums))
     NumArray=0.0d0
     ALLOCATE(lAlphaBlanks(MaxAlphas))
     lAlphaBlanks=.true.
     ALLOCATE(lNumericBlanks(MaxNums))
     lNumericBlanks=.true.


      CurrentModuleObject = 'Coil:Heating:Water'
      ! Get the data for simple heating coils
      DO SimpHeatNum = 1,  NumSimpHeat

        CoilNum= SimpHeatNum

        CALL GetObjectItem(CurrentModuleObject,SimpHeatNum,AlphArray, &
                           NumAlphas,NumArray,NumNums,IOSTAT, &
                           NumBlank=lNumericBlanks,AlphaBlank=lAlphaBlanks, &
                           AlphaFieldNames=cAlphaFields,NumericFieldNames=cNumericFields)

        IsNotOK=.false.
        IsBlank=.false.
        CALL VerifyName(AlphArray(1),WaterCoil%Name,CoilNum-1,IsNotOK,IsBlank,TRIM(CurrentModuleObject)//' Name')
        IF (IsNotOK) THEN
          ErrorsFound=.true.
          IF (IsBlank) AlphArray(1)='xxxxx'
        ENDIF
        CALL VerifyUniqueCoilName(CurrentModuleObject,AlphArray(1),errflag,TRIM(CurrentModuleObject)//' Name')
        IF (errflag) THEN
          ErrorsFound=.true.
        ENDIF
        WaterCoil(CoilNum)%Name     = AlphArray(1)
        WaterCoil(CoilNum)%Schedule = AlphArray(2)
        IF (lAlphaBlanks(2)) THEN
          WaterCoil(CoilNum)%SchedPtr = ScheduleAlwaysOn
        ELSE
          WaterCoil(CoilNum)%SchedPtr = GetScheduleIndex(AlphArray(2))
          IF (WaterCoil(CoilNum)%SchedPtr == 0) THEN
            CALL ShowSevereError(TRIM(CurrentModuleObject)//': invalid '//TRIM(cAlphaFields(2))//  &
                                ' entered ='//TRIM(AlphArray(2))// &
                                ' for '//TRIM(cAlphaFields(1))//'='//TRIM(AlphArray(1)))
            ErrorsFound=.TRUE.
          END IF
        ENDIF

        WaterCoil(CoilNum)%WaterCoilTypeA  = 'Heating'
        WaterCoil(CoilNum)%WaterCoilType  = CoilType_Heating    ! 'Heating'
        WaterCoil(CoilNum)%WaterCoilModelA = 'SIMPLE'
        WaterCoil(CoilNum)%WaterCoilModel = CoilModel_Simple ! 'SIMPLE'
        WaterCoil(CoilNum)%WaterCoilType_Num = WaterCoil_SimpleHeating

        WaterCoil(CoilNum)%UACoil              = NumArray(1)
        WaterCoil(CoilNum)%UACoilVariable      = WaterCoil(CoilNum)%UACoil
        WaterCoil(CoilNum)%MaxWaterVolFlowRate = NumArray(2)
        WaterCoil(CoilNum)%WaterInletNodeNum    = &
               GetOnlySingleNode(AlphArray(3),ErrorsFound,TRIM(CurrentModuleObject),AlphArray(1),NodeType_Water, &
                                 NodeConnectionType_Inlet,2,ObjectIsNotParent)
        WaterCoil(CoilNum)%WaterOutletNodeNum   = &
               GetOnlySingleNode(AlphArray(4),ErrorsFound,TRIM(CurrentModuleObject),AlphArray(1),NodeType_Water, &
                                 NodeConnectionType_Outlet,2,ObjectIsNotParent)
        WaterCoil(CoilNum)%AirInletNodeNum      = &
               GetOnlySingleNode(AlphArray(5),ErrorsFound,TRIM(CurrentModuleObject),AlphArray(1),NodeType_Air, &
                                 NodeConnectionType_Inlet,1,ObjectIsNotParent)
        WaterCoil(CoilNum)%AirOutletNodeNum     = &
               GetOnlySingleNode(AlphArray(6),ErrorsFound,TRIM(CurrentModuleObject),AlphArray(1),NodeType_Air, &
                                 NodeConnectionType_Outlet,1,ObjectIsNotParent)

        SELECT CASE (AlphArray(7))
          CASE ('UFACTORTIMESAREAANDDESIGNWATERFLOWRATE')
            WaterCoil(CoilNum)%CoilPerfInpMeth = UAandFlow

          CASE ('NOMINALCAPACITY')
            WaterCoil(CoilNum)%CoilPerfInpMeth = NomCap

          CASE DEFAULT
            ! will be caught by input processor
            WaterCoil(CoilNum)%CoilPerfInpMeth = UAandFlow
         END SELECT

        WaterCoil(CoilNum)%DesTotWaterCoilLoad = NumArray(3)

        IF (WaterCoil(CoilNum)%UACoil == Autosize .and. WaterCoil(CoilNum)%CoilPerfInpMeth == UAandFlow)   &
           WaterCoil(CoilNum)%RequestingAutosize=.true.
        IF (WaterCoil(CoilNum)%MaxWaterVolFlowRate == Autosize .and. WaterCoil(CoilNum)%CoilPerfInpMeth == UAandFlow)   &
           WaterCoil(CoilNum)%RequestingAutosize=.true.
        IF (WaterCoil(CoilNum)%DesTotWaterCoilLoad == Autosize .and. WaterCoil(CoilNum)%CoilPerfInpMeth == NomCap)   &
           WaterCoil(CoilNum)%RequestingAutosize=.true.

        WaterCoil(CoilNum)%DesInletWaterTemp   = NumArray(4)
        WaterCoil(CoilNum)%DesInletAirTemp     = NumArray(5)
        WaterCoil(CoilNum)%DesOutletWaterTemp  = NumArray(6)
        WaterCoil(CoilNum)%DesOutletAirTemp    = NumArray(7)
        WaterCoil(CoilNum)%RatioAirSideToWaterSideConvect = NumArray(8)

        IF (WaterCoil(CoilNum)%DesInletWaterTemp <= WaterCoil(CoilNum)%DesOutletWaterTemp) THEN
          CALL ShowSevereError('For '//TRIM(CurrentModuleObject)//', ' // TRIM(AlphArray(1)))
          CALL ShowContinueError('  the '//TRIM(cNumericFields(4))//' must be greater than the '//TRIM(cNumericFields(6))//'.')
          ErrorsFound = .TRUE.
        END IF
        IF (WaterCoil(CoilNum)%DesInletAirTemp >= WaterCoil(CoilNum)%DesOutletAirTemp) THEN
          CALL ShowSevereError('For '//TRIM(CurrentModuleObject)//', ' // TRIM(AlphArray(1)))
          CALL ShowContinueError('  the '//TRIM(cNumericFields(5))//' must be less than the '//TRIM(cNumericFields(7))//'.')
          ErrorsFound = .TRUE.
        END IF
        IF (WaterCoil(CoilNum)%DesInletAirTemp >= WaterCoil(CoilNum)%DesInletWaterTemp) THEN
          CALL ShowSevereError('For '//TRIM(CurrentModuleObject)//', ' // TRIM(AlphArray(1)))
          CALL ShowContinueError('  the '//TRIM(cNumericFields(5))//' must be less than the '//TRIM(cNumericFields(4))//'.')
          ErrorsFound = .TRUE.
        END IF

        CALL TestCompSet(TRIM(CurrentModuleObject),AlphArray(1),AlphArray(3),AlphArray(4),'Water Nodes')
        CALL TestCompSet(TRIM(CurrentModuleObject),AlphArray(1),AlphArray(5),AlphArray(6),'Air Nodes')

        !Setup the Simple Heating Coil reporting variables
        CALL SetupOutputVariable('Heating Coil Heating Energy [J]', WaterCoil(CoilNum)%TotWaterHeatingCoilEnergy, &
                              'System','Sum',WaterCoil(CoilNum)%Name, &
                               ResourceTypeKey='ENERGYTRANSFER',EndUseKey='HEATINGCOILS',GroupKey='System')
        CALL SetupOutputVariable('Heating Coil Source Side Heat Transfer Energy [J]',   &
                              WaterCoil(CoilNum)%TotWaterHeatingCoilEnergy, &
                              'System','Sum',WaterCoil(CoilNum)%Name, &
                               ResourceTypeKey='PLANTLOOPHEATINGDEMAND',EndUseKey='HEATINGCOILS',GroupKey='System')
        CALL SetupOutputVariable('Heating Coil Heating Rate [W]', WaterCoil(CoilNum)%TotWaterHeatingCoilRate, &
                              'System','Average',WaterCoil(CoilNum)%Name)
        CALL SetupOutputVariable('Heating Coil U Factor Times Area Value [W/K]', WaterCoil(CoilNum)%UACoilVariable, &
                              'System','Average',WaterCoil(CoilNum)%Name)

      END DO


      CurrentModuleObject = 'Coil:Cooling:Water:DetailedGeometry'
      ! Get the data for detailed cooling coils.
      DO FlatFinNum = 1,  NumFlatFin

        CoilNum= NumSimpHeat + FlatFinNum

        CALL GetObjectItem(CurrentModuleObject,FlatFinNum,  &
                           AlphArray,NumAlphas,NumArray,NumNums,IOSTAT, &
                           NumBlank=lNumericBlanks,AlphaBlank=lAlphaBlanks, &
                           AlphaFieldNames=cAlphaFields,NumericFieldNames=cNumericFields)

        IsNotOK=.false.
        IsBlank=.false.
        CALL VerifyName(AlphArray(1),WaterCoil%Name,CoilNum-1,IsNotOK,IsBlank,TRIM(CurrentModuleObject)//' Name')
        IF (IsNotOK) THEN
          ErrorsFound=.true.
          IF (IsBlank) AlphArray(1)='xxxxx'
        ENDIF
        CALL VerifyUniqueCoilName(CurrentModuleObject,AlphArray(1),errflag,TRIM(CurrentModuleObject)//' Name')
        IF (errflag) THEN
          ErrorsFound=.true.
        ENDIF
        WaterCoil(CoilNum)%Name     = AlphArray(1)
        WaterCoil(CoilNum)%Schedule = AlphArray(2)
        IF (lAlphaBlanks(2)) THEN
          WaterCoil(CoilNum)%SchedPtr = ScheduleAlwaysOn
        ELSE
          WaterCoil(CoilNum)%SchedPtr = GetScheduleIndex(AlphArray(2))
          IF (WaterCoil(CoilNum)%SchedPtr == 0) THEN
            CALL ShowSevereError(TRIM(CurrentModuleObject)//': invalid '//TRIM(cAlphaFields(2))//  &
                                ' entered ='//TRIM(AlphArray(2))// &
                                ' for '//TRIM(cAlphaFields(1))//'='//TRIM(AlphArray(1)))
            ErrorsFound=.TRUE.
          END IF
        ENDIF

        WaterCoil(CoilNum)%WaterCoilTypeA    = 'Cooling'
        WaterCoil(CoilNum)%WaterCoilType     = CoilType_Cooling       ! 'Cooling'
        WaterCoil(CoilNum)%WaterCoilModelA   = 'DETAILED FLAT FIN'
        WaterCoil(CoilNum)%WaterCoilModel    = CoilModel_Detailed  ! 'DETAILED FLAT FIN'
        WaterCoil(CoilNum)%WaterCoilType_Num = WaterCoil_DetFlatFinCooling

        WaterCoil(CoilNum)%MaxWaterVolFlowRate  = NumArray(1)
        IF (WaterCoil(CoilNum)%MaxWaterVolFlowRate == Autosize) WaterCoil(CoilNum)%RequestingAutosize=.true.
        WaterCoil(CoilNum)%TubeOutsideSurfArea  = NumArray(2)
        IF (WaterCoil(CoilNum)%TubeOutsideSurfArea == Autosize) WaterCoil(CoilNum)%RequestingAutosize=.true.
        WaterCoil(CoilNum)%TotTubeInsideArea    = NumArray(3)
        IF (WaterCoil(CoilNum)%TotTubeInsideArea == Autosize) WaterCoil(CoilNum)%RequestingAutosize=.true.
        WaterCoil(CoilNum)%FinSurfArea          = NumArray(4)
        IF (WaterCoil(CoilNum)%FinSurfArea == Autosize) WaterCoil(CoilNum)%RequestingAutosize=.true.
        WaterCoil(CoilNum)%MinAirFlowArea       = NumArray(5)
        IF (WaterCoil(CoilNum)%MinAirFlowArea == Autosize) WaterCoil(CoilNum)%RequestingAutosize=.true.
        WaterCoil(CoilNum)%CoilDepth            = NumArray(6)
        IF (WaterCoil(CoilNum)%CoilDepth == Autosize) WaterCoil(CoilNum)%RequestingAutosize=.true.
        WaterCoil(CoilNum)%FinDiam              = NumArray(7)
        IF (WaterCoil(CoilNum)%FinDiam == Autosize) WaterCoil(CoilNum)%RequestingAutosize=.true.
        WaterCoil(CoilNum)%FinThickness         = NumArray(8)
        IF ( WaterCoil(CoilNum)%FinThickness .LE. 0.0d0 ) THEN
             CALL ShowSevereError(TRIM(CurrentModuleObject)//': '//TRIM(cNumericFields(8))// &
                         ' must be > 0.0, for '//TRIM(cAlphaFields(1))//' = '//TRIM(WaterCoil(CoilNum)%Name))
            ErrorsFound = .TRUE.
        END IF
        WaterCoil(CoilNum)%TubeInsideDiam       = NumArray(9)
        WaterCoil(CoilNum)%TubeOutsideDiam      = NumArray(10)
        WaterCoil(CoilNum)%TubeThermConductivity= NumArray(11)
        IF ( WaterCoil(CoilNum)%TubeThermConductivity .LE. 0.0d0 ) THEN
             CALL ShowSevereError(TRIM(CurrentModuleObject)//': '//TRIM(cNumericFields(11))// &
                         ' must be > 0.0, for '//TRIM(cAlphaFields(1))//' = '//TRIM(WaterCoil(CoilNum)%Name))
            ErrorsFound = .TRUE.
        END IF
        WaterCoil(CoilNum)%FinThermConductivity = NumArray(12)
        IF ( WaterCoil(CoilNum)%FinThermConductivity .LE. 0.0d0 ) THEN
             CALL ShowSevereError(TRIM(CurrentModuleObject)//': '//TRIM(cNumericFields(12))// &
                         ' must be > 0.0, for '//TRIM(cAlphaFields(1))//' = '//TRIM(WaterCoil(CoilNum)%Name))
            ErrorsFound = .TRUE.
        END IF
        WaterCoil(CoilNum)%FinSpacing           = NumArray(13)
        WaterCoil(CoilNum)%TubeDepthSpacing     = NumArray(14)
        WaterCoil(CoilNum)%NumofTubeRows        = NumArray(15)
        WaterCoil(CoilNum)%NumofTubesperRow     = NumArray(16)
        IF (WaterCoil(CoilNum)%NumofTubesperRow == Autosize) WaterCoil(CoilNum)%RequestingAutosize=.true.
        WaterCoil(CoilNum)%WaterInletNodeNum    = &
               GetOnlySingleNode(AlphArray(3),ErrorsFound,TRIM(CurrentModuleObject),AlphArray(1),NodeType_Water, &
                                 NodeConnectionType_Inlet,2,ObjectIsNotParent)
        WaterCoil(CoilNum)%WaterOutletNodeNum   = &
               GetOnlySingleNode(AlphArray(4),ErrorsFound,TRIM(CurrentModuleObject),AlphArray(1),NodeType_Water, &
                                 NodeConnectionType_Outlet,2,ObjectIsNotParent)
        WaterCoil(CoilNum)%AirInletNodeNum      = &
               GetOnlySingleNode(AlphArray(5),ErrorsFound,TRIM(CurrentModuleObject),AlphArray(1),NodeType_Air, &
                                 NodeConnectionType_Inlet,1,ObjectIsNotParent)
        WaterCoil(CoilNum)%AirOutletNodeNum     = &
               GetOnlySingleNode(AlphArray(6),ErrorsFound,TRIM(CurrentModuleObject),AlphArray(1),NodeType_Air, &
                                 NodeConnectionType_Outlet,1,ObjectIsNotParent)

        ! A7 ; \field Name of Water Storage Tank for Condensate Collection
        WaterCoil(CoilNum)%CondensateCollectName = AlphArray(7)
        IF (lAlphaBlanks(7)) THEN
          WaterCoil(CoilNum)%CondensateCollectMode = CondensateDiscarded
        ELSE
          WaterCoil(CoilNum)%CondensateCollectMode = CondensateToTank
          CALL SetupTankSupplyComponent(WaterCoil(CoilNum)%Name,TRIM(CurrentModuleObject), &
                 WaterCoil(CoilNum)%CondensateCollectName, ErrorsFound, WaterCoil(CoilNum)%CondensateTankID, &
                 WaterCoil(CoilNum)%CondensateTankSupplyARRID )
        ENDIF

        CALL TestCompSet(TRIM(CurrentModuleObject),AlphArray(1),AlphArray(3),AlphArray(4),'Water Nodes')
        CALL TestCompSet(TRIM(CurrentModuleObject),AlphArray(1),AlphArray(5),AlphArray(6),'Air Nodes')

        ! Setup Report variables for the Detailed Flat Fin Cooling Coils
        CALL SetupOutputVariable('Cooling Coil Total Cooling Energy [J]', WaterCoil(CoilNum)%TotWaterCoolingCoilEnergy, &
                              'System','Sum',WaterCoil(CoilNum)%Name, &
                               ResourceTypeKey='ENERGYTRANSFER',EndUseKey='COOLINGCOILS',GroupKey='System')
        CALL SetupOutputVariable('Cooling Coil Source Side Heat Transfer Energy [J]',   &
                              WaterCoil(CoilNum)%TotWaterCoolingCoilEnergy, &
                              'System','Sum',WaterCoil(CoilNum)%Name, &
                               ResourceTypeKey='PLANTLOOPCOOLINGDEMAND',EndUseKey='COOLINGCOILS',GroupKey='System')
        CALL SetupOutputVariable('Cooling Coil Sensible Cooling Energy [J]',WaterCoil(CoilNum)%SenWaterCoolingCoilEnergy, &
                              'System','Sum',WaterCoil(CoilNum)%Name)
        CALL SetupOutputVariable('Cooling Coil Total Cooling Rate [W]', WaterCoil(CoilNum)%TotWaterCoolingCoilRate, &
                              'System','Average',WaterCoil(CoilNum)%Name)
        CALL SetupOutputVariable('Cooling Coil Sensible Cooling Rate [W]', WaterCoil(CoilNum)%SenWaterCoolingCoilRate, &
                              'System','Average',WaterCoil(CoilNum)%Name)

        IF (WaterCoil(CoilNum)%CondensateCollectMode == CondensateToTank) THEN

          CALL SetupOutputVariable('Cooling Coil Condensate Volume Flow Rate [m3/s]',WaterCoil(CoilNum)%CondensateVdot,&
                           'System','Average', WaterCoil(CoilNum)%Name)
          CALL SetupOutputVariable('Cooling Coil Condensate Volume [m3]',WaterCoil(CoilNum)%CondensateVol,&
                           'System','Sum', WaterCoil(CoilNum)%Name,  &
                           ResourceTypeKey='OnSiteWater', &
                           EndUseKey='Condensate', GroupKey='System')
        ENDIF

      END DO


      CurrentModuleObject = 'Coil:Cooling:Water'
      ! Get the data for Cooling coils.
      DO CoolingNum = 1,  NumCooling

        CoilNum= NumSimpHeat + NumFlatFin + CoolingNum

        CALL GetObjectItem(CurrentModuleObject,CoolingNum,  &
                           AlphArray,NumAlphas,NumArray,NumNums,IOSTAT, &
                           NumBlank=lNumericBlanks,AlphaBlank=lAlphaBlanks, &
                           AlphaFieldNames=cAlphaFields,NumericFieldNames=cNumericFields)

        IsNotOK=.false.
        IsBlank=.false.
        CALL VerifyName(AlphArray(1),WaterCoil%Name,CoilNum-1,IsNotOK,IsBlank,TRIM(CurrentModuleObject)//' Name')
        IF (IsNotOK) THEN
          ErrorsFound=.true.
          IF (IsBlank) AlphArray(1)='xxxxx'
        ENDIF
        CALL VerifyUniqueCoilName(CurrentModuleObject,AlphArray(1),errflag,TRIM(CurrentModuleObject)//' Name')
        IF (errflag) THEN
          ErrorsFound=.true.
        ENDIF
        WaterCoil(CoilNum)%Name     = AlphArray(1)
        WaterCoil(CoilNum)%Schedule = AlphArray(2)
        IF (lAlphaBlanks(2)) THEN
          WaterCoil(CoilNum)%SchedPtr = ScheduleAlwaysOn
        ELSE
          WaterCoil(CoilNum)%SchedPtr = GetScheduleIndex(AlphArray(2))
          IF (WaterCoil(CoilNum)%SchedPtr == 0) THEN
            CALL ShowSevereError(TRIM(CurrentModuleObject)//': invalid '//TRIM(cAlphaFields(2))//  &
                                ' entered ='//TRIM(AlphArray(2))// &
                                ' for '//TRIM(cAlphaFields(1))//'='//TRIM(AlphArray(1)))
            ErrorsFound=.TRUE.
          END IF
        ENDIF

        WaterCoil(CoilNum)%WaterCoilTypeA    = 'Cooling'
        WaterCoil(CoilNum)%WaterCoilType     = CoilType_Cooling  ! 'Cooling'
        WaterCoil(CoilNum)%WaterCoilModelA   = 'Cooling'
        WaterCoil(CoilNum)%WaterCoilModel    = CoilModel_Cooling ! 'Cooling'
        WaterCoil(CoilNum)%WaterCoilType_Num = WaterCoil_Cooling

        WaterCoil(CoilNum)%MaxWaterVolFlowRate   = NumArray(1)   !Liquid mass flow rate at Design  kg/s
        IF (WaterCoil(CoilNum)%MaxWaterVolFlowRate == Autosize) WaterCoil(CoilNum)%RequestingAutosize=.true.
        WaterCoil(CoilNum)%DesAirVolFlowRate     = NumArray(2)   !Dry air mass flow rate at Design (kg/s)
        IF (WaterCoil(CoilNum)%DesAirVolFlowRate == Autosize) WaterCoil(CoilNum)%RequestingAutosize=.true.
        WaterCoil(CoilNum)%DesInletWaterTemp     = NumArray(3)   !Entering water temperature at Design C
        IF (WaterCoil(CoilNum)%DesInletWaterTemp == Autosize) WaterCoil(CoilNum)%RequestingAutosize=.true.
        WaterCoil(CoilNum)%DesInletAirTemp       = NumArray(4)   !Entering air dry bulb temperature at Design(C)
        IF (WaterCoil(CoilNum)%DesInletAirTemp == Autosize) WaterCoil(CoilNum)%RequestingAutosize=.true.
        WaterCoil(CoilNum)%DesOutletAirTemp      = NumArray(5)   !Leaving air dry bulb temperature at Design(C)
        IF (WaterCoil(CoilNum)%DesOutletAirTemp == Autosize) WaterCoil(CoilNum)%RequestingAutosize=.true.
        WaterCoil(CoilNum)%DesInletAirHumRat     = NumArray(6)   !Entering air humidity ratio  at Design
        IF (WaterCoil(CoilNum)%DesInletAirHumRat == Autosize) WaterCoil(CoilNum)%RequestingAutosize=.true.
        WaterCoil(CoilNum)%DesOutletAirHumRat    = NumArray(7)   !Leaving air humidity ratio  at Design
        IF (WaterCoil(CoilNum)%DesOutletAirHumRat == Autosize) WaterCoil(CoilNum)%RequestingAutosize=.true.

        WaterCoil(CoilNum)%WaterInletNodeNum    = &
               GetOnlySingleNode(AlphArray(3),ErrorsFound,TRIM(CurrentModuleObject),AlphArray(1),NodeType_Water, &
                                 NodeConnectionType_Inlet,2,ObjectIsNotParent)
        WaterCoil(CoilNum)%WaterOutletNodeNum   = &
               GetOnlySingleNode(AlphArray(4),ErrorsFound,TRIM(CurrentModuleObject),AlphArray(1),NodeType_Water, &
                                 NodeConnectionType_Outlet,2,ObjectIsNotParent)
        WaterCoil(CoilNum)%AirInletNodeNum      = &
               GetOnlySingleNode(AlphArray(5),ErrorsFound,TRIM(CurrentModuleObject),AlphArray(1),NodeType_Air, &
                                 NodeConnectionType_Inlet,1,ObjectIsNotParent)
        WaterCoil(CoilNum)%AirOutletNodeNum     = &
               GetOnlySingleNode(AlphArray(6),ErrorsFound,TRIM(CurrentModuleObject),AlphArray(1),NodeType_Air, &
                                 NodeConnectionType_Outlet,1,ObjectIsNotParent)

       SELECT CASE (AlphArray(7))
        !The default is SimpleAnalysis = 2.  and DetailedAnalysis   =1
         CASE ('SIMPLEANALYSIS')
          WaterCoil(CoilNum)%CoolingCoilAnalysisMode = SimpleAnalysis

         CASE ('DETAILEDANALYSIS')
          WaterCoil(CoilNum)%CoolingCoilAnalysisMode = DetailedAnalysis

         CASE DEFAULT
          WaterCoil(CoilNum)%CoolingCoilAnalysisMode = SimpleAnalysis
        END SELECT

       SELECT CASE (AlphArray(8))
        !The default is CrossFlow = 2.  and CounterFlow=1
        CASE ('CROSSFLOW')
          WaterCoil(CoilNum)%HeatExchType = CrossFlow

        CASE ('COUNTERFLOW')
          WaterCoil(CoilNum)%HeatExchType = CounterFlow

        CASE DEFAULT
          WaterCoil(CoilNum)%HeatExchType = CrossFlow
       END SELECT

       !A9; \field Name of Water Storage Tank for Condensate Collection
       WaterCoil(CoilNum)%CondensateCollectName = AlphArray(9)
       IF (lAlphaBlanks(9)) THEN
         WaterCoil(CoilNum)%CondensateCollectMode = CondensateDiscarded
       ELSE
         WaterCoil(CoilNum)%CondensateCollectMode = CondensateToTank
         CALL SetupTankSupplyComponent(WaterCoil(CoilNum)%Name, TRIM(CurrentModuleObject), &
                 WaterCoil(CoilNum)%CondensateCollectName, ErrorsFound, WaterCoil(CoilNum)%CondensateTankID, &
                 WaterCoil(CoilNum)%CondensateTankSupplyARRID )
       ENDIF


        CALL TestCompSet(TRIM(CurrentModuleObject),AlphArray(1),AlphArray(3),AlphArray(4),'Water Nodes')
        CALL TestCompSet(TRIM(CurrentModuleObject),AlphArray(1),AlphArray(5),AlphArray(6),'Air Nodes')

        ! Setup Report variables for the Design input Cooling Coils
        CALL SetupOutputVariable('Cooling Coil Total Cooling Energy [J]',   &
                              WaterCoil(CoilNum)%TotWaterCoolingCoilEnergy, &
                              'System','Sum',WaterCoil(CoilNum)%Name, &
                               ResourceTypeKey='ENERGYTRANSFER',EndUseKey='COOLINGCOILS',GroupKey='System')
        CALL SetupOutputVariable('Cooling Coil Source Side Heat Transfer Energy [J]',   &
                               WaterCoil(CoilNum)%TotWaterCoolingCoilEnergy, &
                              'System','Sum',WaterCoil(CoilNum)%Name, &
                               ResourceTypeKey='PLANTLOOPCOOLINGDEMAND',EndUseKey='COOLINGCOILS',GroupKey='System')
        CALL SetupOutputVariable('Cooling Coil Sensible Cooling Energy [J]',WaterCoil(CoilNum)%SenWaterCoolingCoilEnergy, &
                              'System','Sum',WaterCoil(CoilNum)%Name)
        CALL SetupOutputVariable('Cooling Coil Total Cooling Rate [W]', WaterCoil(CoilNum)%TotWaterCoolingCoilRate, &
                              'System','Average',WaterCoil(CoilNum)%Name)
        CALL SetupOutputVariable('Cooling Coil Sensible Cooling Rate [W]', WaterCoil(CoilNum)%SenWaterCoolingCoilRate, &
                              'System','Average',WaterCoil(CoilNum)%Name)
        CALL SetupOutputVariable('Cooling Coil Wetted Area Fraction []', WaterCoil(CoilNum)%SurfAreaWetFraction, &
                              'System','Average',WaterCoil(CoilNum)%Name)

        IF (WaterCoil(CoilNum)%CondensateCollectMode == CondensateToTank) THEN

          CALL SetupOutputVariable('Cooling Coil Condensate Volume Flow Rate [m3/s]',WaterCoil(CoilNum)%CondensateVdot,&
                           'System','Average', WaterCoil(CoilNum)%Name)
          CALL SetupOutputVariable('Cooling Coil Condensate Volume [m3]',WaterCoil(CoilNum)%CondensateVol,&
                           'System','Sum', WaterCoil(CoilNum)%Name,  &
                           ResourceTypeKey='OnSiteWater', &
                           EndUseKey='Condensate', GroupKey='System')
        ENDIF

      END DO

      IF (ErrorsFound) THEN
        CALL ShowFatalError(RoutineName//'Errors found in getting input.')
      ENDIF

      DEALLOCATE(AlphArray)
      DEALLOCATE(cAlphaFields)
      DEALLOCATE(cNumericFields)
      DEALLOCATE(NumArray)
      DEALLOCATE(lAlphaBlanks)
      DEALLOCATE(lNumericBlanks)

  RETURN

END SUBROUTINE GetWaterCoilInput

! End of Get Input subroutines for the HB Module
!******************************************************************************


 ! Beginning Initialization Section of the Module
!******************************************************************************
SUBROUTINE InitWaterCoil(CoilNum,FirstHVACIteration)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard J. Liesen
          !       DATE WRITTEN   February 1998
          !       MODIFIED       April 2004: Rahul Chillar
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is for initializations of the WaterCoil Components.

          ! METHODOLOGY EMPLOYED:
          ! Uses the status flags to trigger initializations.

          ! REFERENCES:

          ! USE STATEMENTS:
  USE General,    ONLY: RoundSigDigits, SolveRegulaFalsi, Iterate, &
                        SafeDivide
  USE DataSizing, ONLY: AutoSize
  USE OutputReportPredefined
  USE DataPlant,  ONLY : ScanPlantLoopsForObject
  USE PlantUtilities, ONLY : InitComponentNodes, RegisterPlantCompDesignFlow

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: CoilNum
  LOGICAL, INTENT (IN):: FirstHVACIteration !unused1208

          ! SUBROUTINE PARAMETER DEFINITIONS:
  REAL(r64), PARAMETER  :: SmallNo = 1.d-9 ! SmallNo number in place of zero
  REAL(r64), PARAMETER  :: LargeNo =1.d20  ! Large number in place of infinity
  INTEGER, PARAMETER           :: itmax =10
  INTEGER, PARAMETER           :: MaxIte = 500        ! Maximum number of iterations
  REAL(r64), PARAMETER  :: Acc =  0.0001d0       ! Accuracy of result

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64) DesInletAirEnth               ! Entering air enthalpy at rating (J/kg)
  REAL(r64) DesOutletAirEnth              ! Leaving air enthalpy at rating(J/kg)
  REAL(r64) DesAirApparatusDewPtEnth      ! Air enthalpy at apparatus dew point at rating(J/kg)
  REAL(r64) DesSatEnthAtWaterInTemp       ! Saturated enthalpy at entering liquid temp(J/kg)
  REAL(r64) DesHumRatAtWaterInTemp        ! Enthalpy at water inlet temp and entering air HumRat (J/kg)
  REAL(r64) CapacitanceAir                ! Air-side capacity rate(W/C)
  REAL(r64) DesAirTempApparatusDewPt      ! Temperature apparatus dew point at design capacity
  REAL(r64) DesAirHumRatApparatusDewPt    ! Humdity Ratio at apparatus dew point at design capacity
  REAL(r64) DesByPassFactor               ! ByPass Factor at design condition
  REAL(r64) SlopeTempVsHumRatio           ! Ratio temperature difference to humidity difference
                                          ! between entering and leaving air states
  REAL(r64) TempApparatusDewPtEstimate    ! Estimate of TAdp from SlopeTempVsHumRatio
  REAL(r64) Y1         ! Previous values of dependent variable in ITERATE
  REAL(r64) X1         ! Previous values of independent variable in ITERATE
  REAL(r64) error      ! Deviation of dependent variable in iteration
  INTEGER iter    ! Iteration counter
  INTEGER icvg    ! Iteration convergence flag
  REAL(r64) ResultX    ! Output variable from ITERATE function.
  INTEGER :: Ipass  ! loop index for App_Dewpoint_Loop
  REAL(r64) :: TOutNew = 0.0d0       ! reset outlet air temperature for Coil:Cooling:Water
  REAL(r64) :: WOutNew = 0.0d0       ! reset outlet air humidity ratio for Coil:Cooling:Water

  INTEGER             :: AirInletNode
  INTEGER             :: WaterInletNode
  INTEGER             :: WaterOutletNode

  REAL(r64), SAVE, ALLOCATABLE, DIMENSION(:) :: DesCpAir         ! CPAir at Design Inlet Air Temp
  REAL(r64), SAVE, ALLOCATABLE, DIMENSION(:) :: DesUARangeCheck  ! Value for range check based on Design Inlet Air Humidity Ratio

  LOGICAL,SAVE        :: MyOneTimeFlag = .true.
  LOGICAL, ALLOCATABLE,Save, DIMENSION(:) :: MyEnvrnFlag
  LOGICAL, ALLOCATABLE,Save, DIMENSION(:) :: MyCoilReportFlag
  LOGICAL, ALLOCATABLE,Save, DIMENSION(:) :: PlantLoopScanFlag

  REAL(r64), DIMENSION(5) :: CoefSeries
  REAL(r64) :: FinDiamVar
  REAL(r64) :: TubeToFinDiamRatio

  REAL(r64)           :: RhoAirStd     ! density of air at standard conditions
  REAL(r64)           :: CpAirStd      ! specific heat of air at std conditions
  INTEGER             :: SolFla              ! Flag of solver
  REAL(r64)           :: UA0                 ! lower bound for UA
  REAL(r64)           :: UA1                 ! upper bound for UA
  REAL(r64)           :: UA
  REAL(r64), DIMENSION(4)  :: Par

  LOGICAL :: NoSatCurveIntersect = .FALSE. ! TRUE if failed to find appatatus dew-point
  LOGICAL :: BelowInletWaterTemp = .FALSE. ! TRUE if apparatus dew-point below design inlet water temperature
  LOGICAL :: CBFTooLarge = .FALSE.         ! TRUE if the coil bypass factor is unrealistically large
  LOGICAL :: NoExitCondReset = .FALSE.     ! TRUE if exit condition reset is not to be done

  REAL(r64) :: RatedLatentCapacity = 0.0d0 ! latent cooling capacity at the rating point [W]
  REAL(r64) :: RatedSHR = 0.0d0            ! sensible heat ratio at the rating point
  REAL(r64) :: CapacitanceWater = 0.0d0    ! capacitance of the water stream [W/K]
  REAL(r64) :: CMin             = 0.0d0    ! minimum capacitance of 2 streams [W/K]
  REAL(r64) :: CoilEffectiveness = 0.0d0   ! effectiveness of the coil (rated)
  REAL(r64) :: SurfaceArea = 0.0d0         ! heat exchanger surface area, [m2]
  REAL(r64) :: UATotal = 0.0d0             ! heat exchanger UA total, [W/C]
  LOGICAL,SAVE,DIMENSION(2) :: RptCoilHeaderFlag=.true.
  REAL(r64) :: x_a ! result of Eq.70 in Wetter 1999
  REAL(r64) :: x_w ! result of Eq.72 in Wetter 1999
  REAL(r64) :: AirConvectTerm ! result of Eq.71 in Wetter 1999
  REAL(r64) :: WaterConvectTerm ! result of Eq.73 in Wetter 1999
  REAL(r64) :: WaterConvSensitivity ! "s" in Wetter 1999, temperature sensitivity in water side convection

  REAL(r64) :: DesUACoilExternalEnth     ! enthalpy based UAExternal for wet coil surface {kg/s}
  REAL(r64) :: LogMeanEnthDiff           ! long mean enthalpy difference {J/kg}
  REAL(r64) :: LogMeanTempDiff           ! long mean temperature difference {C}

  REAL(r64) :: DesOutletWaterTemp
  REAL(r64) :: DesSatEnthAtWaterOutTemp
  REAL(r64) :: DesEnthAtWaterOutTempAirInHumRat
  REAL(r64) :: DesEnthWaterOut
  REAL(r64) :: Cp ! local fluid specific heat
  REAL(r64) :: rho ! local fluid density
  LOGICAL   :: errFlag
  REAL(r64) :: EnthCorrFrac = 0.0d0        ! enthalpy correction factor
  REAL(r64) :: TempCorrFrac = 0.0d0        ! temperature correction factor
          ! FLOW:

  IF (MyOneTimeFlag) THEN
    ! initialize the environment and sizing flags
    ALLOCATE(MyEnvrnFlag(NumWaterCoils))
    ALLOCATE(MySizeFlag(NumWaterCoils))
    ALLOCATE(CoilWarningOnceFlag(NumWaterCoils))
    ALLOCATE(DesCpAir(NumWaterCoils))
    ALLOCATE(MyUAAndFlowCalcFlag(NumWaterCoils))
    ALLOCATE(MyCoilDesignFlag(NumWaterCoils))
    ALLOCATE(MyCoilReportFlag(NumWaterCoils))
    ALLOCATE(DesUARangeCheck(NumWaterCoils))
    ALLOCATE(PlantLoopScanFlag(NumWaterCoils))
    DesCpAir=0.0d0
    DesUARangeCheck=0.0d0
    MyEnvrnFlag = .TRUE.
    MySizeFlag = .TRUE.
    CoilWarningOnceFlag = .TRUE.
    MyUAAndFlowCalcFlag = .TRUE.
    MyCoilDesignFlag = .TRUE.
    MyCoilReportFlag =.true.
    MyOneTimeFlag = .false.
    PlantLoopScanFlag = .TRUE.
  END IF

  IF (PlantLoopScanFlag(CoilNum) .AND.  ALLOCATED(PlantLoop)) THEN
    errFlag=.false.
    CALL ScanPlantLoopsForObject(WaterCoil(CoilNum)%Name, &
                                 WaterCoil(CoilNum)%WaterCoilType_Num, &
                                 WaterCoil(CoilNum)%WaterLoopNum, &
                                 WaterCoil(CoilNum)%WaterLoopSide, &
                                 WaterCoil(CoilNum)%WaterLoopBranchNum, &
                                 WaterCoil(CoilNum)%WaterLoopCompNum,   &
                                 errFlag=errFlag)
     IF (errFlag) THEN
       CALL ShowFatalError('InitWaterCoil: Program terminated for previous conditions.')
     ENDIF
    PlantLoopScanFlag(CoilNum) = .FALSE.
  ENDIF
  IF ( .NOT. SysSizingCalc .AND. MySizeFlag(CoilNum)) THEN
    ! for each coil, do the sizing once.
    CALL SizeWaterCoil(CoilNum)

    MySizeFlag(CoilNum) = .FALSE.
  END IF
  rho = GetDensityGlycol(PlantLoop(WaterCoil(CoilNum)%WaterLoopNum)%FluidName,  &
                             InitConvTemp,                      &
                             PlantLoop(WaterCoil(CoilNum)%WaterLoopNum)%FluidIndex, &
                             'InitWaterCoil')

  ! Do the Begin Environment initializations
  IF (BeginEnvrnFlag .and. MyEnvrnFlag(CoilNum)) THEN
    !Initialize all report variables to a known state at beginning of simulation
    WaterCoil(CoilNum)%TotWaterHeatingCoilEnergy = 0.0d0
    WaterCoil(CoilNum)%TotWaterCoolingCoilEnergy = 0.0d0
    WaterCoil(CoilNum)%SenWaterCoolingCoilEnergy = 0.0d0
    WaterCoil(CoilNum)%TotWaterHeatingCoilRate = 0.0d0
    WaterCoil(CoilNum)%TotWaterCoolingCoilRate = 0.0d0
    WaterCoil(CoilNum)%SenWaterCoolingCoilRate = 0.0d0

    ! The rest of the one time initializations
    AirInletNode = WaterCoil(CoilNum)%AirInletNodeNum
    WaterInletNode = WaterCoil(CoilNum)%WaterInletNodeNum
    WaterOutletNode = WaterCoil(CoilNum)%WaterOutletNodeNum

    DesCpAir(CoilNum) = PsyCpAirFnWTdb(0.0d0,WaterCoil(CoilNum)%DesInletAirTemp)
    DesUARangeCheck(CoilNum) = (-1568.6d0*WaterCoil(CoilNum)%DesInletAirHumRat + 20.157d0)

    IF (WaterCoil(CoilNum)%WaterCoilType == CoilType_Cooling) THEN   ! 'Cooling'
      Node(WaterInletNode)%Temp          = 5.0d0

      Cp =  GetSpecificHeatGlycol(PlantLoop(WaterCoil(CoilNum)%WaterLoopNum)%FluidName,  &
                                 Node(WaterInletNode)%Temp,                      &
                                 PlantLoop(WaterCoil(CoilNum)%WaterLoopNum)%FluidIndex, &
                                 'InitWaterCoil')

      Node(WaterInletNode)%Enthalpy      = Cp * Node(WaterInletNode)%Temp
      Node(WaterInletNode)%Quality       = 0.0d0
      Node(WaterInletNode)%Press         = 0.0d0
      Node(WaterInletNode)%HumRat        = 0.0d0
    END IF

    IF (WaterCoil(CoilNum)%WaterCoilType == CoilType_Heating) THEN  ! 'Heating'
      Node(WaterInletNode)%Temp          = 60.0d0

      Cp =  GetSpecificHeatGlycol(PlantLoop(WaterCoil(CoilNum)%WaterLoopNum)%FluidName,  &
                                 Node(WaterInletNode)%Temp,                      &
                                 PlantLoop(WaterCoil(CoilNum)%WaterLoopNum)%FluidIndex, &
                                 'InitWaterCoil')

      Node(WaterInletNode)%Enthalpy      = Cp* Node(WaterInletNode)%Temp
      Node(WaterInletNode)%Quality       = 0.0d0
      Node(WaterInletNode)%Press         = 0.0d0
      Node(WaterInletNode)%HumRat        = 0.0d0

      IF ( (WaterCoil(CoilNum)%DesTotWaterCoilLoad .NE. AutoSize) .AND. MyUAAndFlowCalcFlag(CoilNum) ) THEN
        ! calculate design water flow rate
        IF ( (WaterCoil(CoilNum)%CoilPerfInpMeth == NomCap) .OR. (WaterCoil(CoilNum)%CoilPerfInpMeth == UAandFlow .AND. &
              WaterCoil(CoilNum)%MaxWaterVolFlowRate .EQ. AutoSize) ) THEN
          ! check for very small heating capacity
          IF (WaterCoil(CoilNum)%DesTotWaterCoilLoad > SmallLoad) THEN
            WaterCoil(CoilNum)%MaxWaterVolFlowRate = WaterCoil(CoilNum)%DesTotWaterCoilLoad / (Cp *   &
                  rho * (WaterCoil(CoilNum)%DesInletWaterTemp - WaterCoil(CoilNum)%DesOutletWaterTemp))
            ! save the design water volumetric flow rate for use by the water loop sizing algorithms
            CALL RegisterPlantCompDesignFlow(WaterCoil(CoilNum)%WaterInletNodeNum,WaterCoil(CoilNum)%MaxWaterVolFlowRate)
          ELSE
            WaterCoil(CoilNum)%MaxWaterVolFlowRate = 0.0d0
          END IF
        END IF
        ! calculate the coil UA
        IF ( (WaterCoil(CoilNum)%CoilPerfInpMeth == NomCap) .OR. (WaterCoil(CoilNum)%CoilPerfInpMeth == UAandFlow .AND. &
              WaterCoil(CoilNum)%UACoil .EQ. AutoSize) ) THEN
          ! check for very small heating capacity
          IF (WaterCoil(CoilNum)%DesTotWaterCoilLoad > SmallLoad) THEN
            RhoAirStd = StdRhoAir
            CpAirStd = PsyCpAirFnWTdb(0.0d0,20.0d0)
            Par(1) = WaterCoil(CoilNum)%DesTotWaterCoilLoad
            Par(2) = REAL(CoilNum,r64)
            Par(3) = REAL(ContFanCycCoil,r64) !fan operating mode
            Par(4) = 1.0d0 ! part-load ratio
            WaterCoil(CoilNum)%InletAirTemp = WaterCoil(CoilNum)%DesInletAirTemp
            WaterCoil(CoilNum)%InletAirHumRat = 0.008d0
            WaterCoil(CoilNum)%InletWaterTemp = WaterCoil(CoilNum)%DesInletWaterTemp
            WaterCoil(CoilNum)%InletWaterMassFlowRate = rho * WaterCoil(CoilNum)%MaxWaterVolFlowRate
            WaterCoil(CoilNum)%InletAirMassFlowRate = WaterCoil(CoilNum)%DesTotWaterCoilLoad / (CpAirStd *  &
                                                       (WaterCoil(CoilNum)%DesOutletAirTemp - WaterCoil(CoilNum)%DesInletAirTemp))
            WaterCoil(CoilNum)%DesAirVolFlowRate = WaterCoil(CoilNum)%InletAirMassFlowRate / RhoAirStd
            ! set the lower and upper limits on the UA
            UA0 = .001d0 * WaterCoil(CoilNum)%DesTotWaterCoilLoad
            UA1 = WaterCoil(CoilNum)%DesTotWaterCoilLoad
            ! Invert the simple heating coil model: given the design inlet conditions and the design load, fins the design UA
            CALL SolveRegulaFalsi(Acc, MaxIte, SolFla, UA, SimpleHeatingCoilUAResidual, UA0, UA1, Par)
            ! if the numerical inversion failed, issue error messages.
            IF (SolFla == -1) THEN
              CALL ShowSevereError('Calculation of heating coil UA failed for coil '//TRIM(WaterCoil(CoilNum)%Name))
              CALL ShowContinueError('  Iteration limit exceeded in calculating coil UA')
              CALL ShowFatalError('Preceding error causes program termination')
            ELSE IF (SolFla == -2) THEN
              CALL ShowSevereError('Calculation of heating coil UA failed for coil '//TRIM(WaterCoil(CoilNum)%Name))
              CALL ShowContinueError('  Bad starting values for UA')
              CALL ShowFatalError('Preceding error causes program termination')
            END IF
            WaterCoil(CoilNum)%UACoil = UA
          ELSE
            WaterCoil(CoilNum)%UACoil = 1.0d0
          END IF
        END IF
      END IF
      MyUAAndFlowCalcFlag(CoilNum) = .FALSE.
      !fill values for variable UA
      CpAirStd = PsyCpAirFnWTdb(0.0d0,20.0d0)
      WaterCoil(CoilNum)%DesAirMassFlowRate = StdRhoAir * WaterCoil(CoilNum)%DesAirVolFlowRate
      WaterCoil(CoilNum)%LiquidSideNominalConvect = WaterCoil(CoilNum)%UACoil &
                                  * (WaterCoil(CoilNum)%RatioAirSideToWaterSideConvect + 1)&
                                       /WaterCoil(CoilNum)%RatioAirSideToWaterSideConvect
      WaterCoil(CoilNum)%AirSideNominalConvect =  WaterCoil(CoilNum)%RatioAirSideToWaterSideConvect &
                                                  * WaterCoil(CoilNum)%LiquidSideNominalConvect
    ELSE
      MyUAAndFlowCalcFlag(CoilNum) = .FALSE.
    END IF

    WaterCoil(CoilNum)%MaxWaterMassFlowRate = rho * WaterCoil(CoilNum)%MaxWaterVolFlowRate

    Call InitComponentNodes(0.d0,WaterCoil(CoilNum)%MaxWaterMassFlowRate, &
                                 WaterCoil(CoilNum)%WaterInletNodeNum,    &
                                 WaterCoil(CoilNum)%WaterOutletNodeNum,   &
                                 WaterCoil(CoilNum)%WaterLoopNum,         &
                                 WaterCoil(CoilNum)%WaterLoopSide,        &
                                 WaterCoil(CoilNum)%WaterLoopBranchNum,   &
                                 WaterCoil(CoilNum)%WaterLoopCompNum )

  ! effective fin diameter for detailed flat fin coil
    IF (WaterCoil(CoilNum)%WaterCoilModel == CoilModel_Detailed) THEN  ! 'DETAILED FLAT FIN'
      WaterCoil(CoilNum)%EffectiveFinDiam = SQRT(4.d0 * WaterCoil(CoilNum)%FinDiam * &
                               WaterCoil(CoilNum)%CoilDepth                        &
                                / (Pi * WaterCoil(CoilNum)%NumOfTubeRows *           &
                                      WaterCoil(CoilNum)%NumOfTubesPerRow))

  !   calculate fixed geometric parameters of the coil:
  !   Total Area
      WaterCoil(CoilNum)%TotCoilOutsideSurfArea = WaterCoil(CoilNum)%TubeOutsideSurfArea + &
                                   WaterCoil(CoilNum)%FinSurfArea
  !   Effective Tube Inside Diameter - the model assumes that the coil
  !   can be simulated as a tube with an equivalent hydraulic diameter.
      WaterCoil(CoilNum)%CoilEffectiveInsideDiam = 4.d0 * WaterCoil(CoilNum)%MinAirFlowArea * &
                                      WaterCoil(CoilNum)%CoilDepth /  &
                                      WaterCoil(CoilNum)%TotCoilOutsideSurfArea
  !   Ratio of tube outside diameter to effective fin diameter should always
  !   be less than 1
      TubeToFinDiamRatio = WaterCoil(CoilNum)%TubeOutsideDiam / WaterCoil(CoilNum)%EffectiveFinDiam
      IF (TubeToFinDiamRatio > 1.0d0) THEN
        CALL ShowWarningError('InitWaterCoil: Detailed Flat Fin Coil, TubetoFinDiamRatio > 1.0, ['//  &
              TRIM(RoundSigDigits(TubeToFinDiamRatio,4))//']')
        ! reset tube depth spacing and recalc dependent parameters
        WaterCoil(CoilNum)%TubeDepthSpacing = WaterCoil(CoilNum)%TubeDepthSpacing * (TubeToFinDiamRatio**2 + 0.1d0)
        WaterCoil(CoilNum)%CoilDepth = WaterCoil(CoilNum)%TubeDepthSpacing * WaterCoil(CoilNum)%NumofTubeRows
        WaterCoil(CoilNum)%EffectiveFinDiam = SQRT(4.d0 * WaterCoil(CoilNum)%FinDiam * &
                                                WaterCoil(CoilNum)%CoilDepth                        &
                                                / (Pi * WaterCoil(CoilNum)%NumOfTubeRows *           &
                                                WaterCoil(CoilNum)%NumOfTubesPerRow))
        WaterCoil(CoilNum)%CoilEffectiveInsideDiam = 4.d0 * WaterCoil(CoilNum)%MinAirFlowArea * &
                                                       WaterCoil(CoilNum)%CoilDepth /  &
                                                       WaterCoil(CoilNum)%TotCoilOutsideSurfArea
        TubeToFinDiamRatio = WaterCoil(CoilNum)%TubeOutsideDiam / WaterCoil(CoilNum)%EffectiveFinDiam
        CALL ShowContinueError('  Resetting tube depth spacing to ' //   &
                               TRIM(RoundSigDigits(WaterCoil(CoilNum)%TubeDepthSpacing,4)) // &
                               ' meters')
        CALL ShowContinueError('  Resetting coil depth to ' // TRIM(RoundSigDigits(WaterCoil(CoilNum)%CoilDepth,4)) // &
                               ' meters')
      ENDIF

      CALL CalcDryFinEffCoef(TubeToFinDiamRatio, CoefSeries)

      WaterCoil(CoilNum)%DryFinEfficncyCoef = CoefSeries

      FinDiamVar = 0.5d0 * (WaterCoil(CoilNum)%EffectiveFinDiam - WaterCoil(CoilNum)%TubeOutsideDiam)

      WaterCoil(CoilNum)%GeometryCoef1 = 0.159d0 * (WaterCoil(CoilNum)%FinThickness / &
                             WaterCoil(CoilNum)%CoilEffectiveInsideDiam)**(-0.065d0)   &
                             * (WaterCoil(CoilNum)%FinThickness/FinDiamVar)**0.141d0
      WaterCoil(CoilNum)%GeometryCoef2 = -0.323d0 * (WaterCoil(CoilNum)%FinSpacing/FinDiamVar)**0.049d0 * &
                       (WaterCoil(CoilNum)%EffectiveFinDiam/WaterCoil(CoilNum)%TubeDepthSpacing)**0.549d0 *  &
                       (WaterCoil(CoilNum)%FinThickness/WaterCoil(CoilNum)%FinSpacing)**(-.028d0)

      ! Set some initial values for simulation
      WaterCoil(CoilNum)%SatEnthlCurveConstCoef   = -10.57d0
      WaterCoil(CoilNum)%SatEnthlCurveSlope       =   3.3867d0
      WaterCoil(CoilNum)%EnthVsTempCurveAppxSlope =   3.3867d0
      WaterCoil(CoilNum)%EnthVsTempCurveConst     = -10.57d0
      ! Set Saved Values to Zero
      WaterCoil(CoilNum)%SurfAreaWetSaved   = 0.0d0
      WaterCoil(CoilNum)%MeanWaterTempSaved = 0.0d0
      WaterCoil(CoilNum)%InWaterTempSaved   = 0.0d0
      WaterCoil(CoilNum)%OutWaterTempSaved  = 0.0d0

    END IF  ! End the Detailed Flat Fin Coil Initialization

       ! Calculation for Cooling Coil, The part between the '@@@' are design condition
       ! and are calculated only once to calculate standard values for UAs and other physical parameters of
       ! the cooling coil.
       ! Basic Idea for UA:  Heat Transfer= UAenthalpybased*(Delta enthalpy), this is a necessity since the
       ! coil may be Wet or Dry or Partially Wet-Dry, so latent effects are accounted for in this model while
       ! calculating the UA. A fictitious specific heat is also defined to caculate the conventional UA.
       ! On the air side, enthalpy capacity rate is the air mass flow rate,while on water side it is
       ! enthalpy of saturated air at water temperature.
       !@@@ DESIGN CONDITION BEGIN HERE @@@

    ! Check for zero design cooling capacity as specified by coil design inputs
    IF (MyCoilDesignFlag(CoilNum) .and. (WaterCoil(CoilNum)%WaterCoilModel == CoilModel_Cooling).and. &
                  (WaterCoil(CoilNum)%DesAirVolFlowRate .gt. 0.0d0).and.  &
                           (WaterCoil(CoilNum)%MaxWaterMassFlowRate .gt. 0.0d0))THEN

      DesInletAirEnth=PsyHFnTdbW(WaterCoil(CoilNum)%DesInletAirTemp,WaterCoil(CoilNum)%DesInletAirHumRat)
      DesOutletAirEnth=PsyHFnTdbW(WaterCoil(CoilNum)%DesOutletAirTemp,WaterCoil(CoilNum)%DesOutletAirHumRat)
      DesSatEnthAtWaterInTemp =PsyHFnTdbW(WaterCoil(CoilNum)%DesInletWaterTemp, &
                                             PsyWFnTdpPb(WaterCoil(CoilNum)%DesInletWaterTemp,StdBaroPress))
      ! check for dry coil
      DesHumRatAtWaterInTemp = PsyWFnTdbH(WaterCoil(CoilNum)%DesInletWaterTemp,DesSatEnthAtWaterInTemp,'InitWaterCoil')
      IF(DesHumRatAtWaterInTemp > WaterCoil(CoilNum)%DesOutletAirHumRat .AND. &
         WaterCoil(CoilNum)%DesOutletAirTemp > WaterCoil(CoilNum)%DesInletWaterTemp)THEN
        ! if the design outlet air humrat is lower than the saturated air humrat at the design inlet water temp
        ! and the design outlet air temperature is higher than the design inlet water temp (i.e, cooling possible),
        ! move the design outlet air saturated enthalpy down (i.e., to Twaterin, Wair,out) to allow the coil to size.
        DesSatEnthAtWaterInTemp =PsyHFnTdbW(WaterCoil(CoilNum)%DesInletWaterTemp, WaterCoil(CoilNum)%DesOutletAirHumRat)-0.0001d0
      END IF
      IF ( DesOutletAirEnth >= DesInletAirEnth .OR. WaterCoil(CoilNum)%DesInletWaterTemp >= WaterCoil(CoilNum)%DesInletAirTemp) THEN
        CALL ShowWarningError('The design cooling capacity is zero for Coil:Cooling:Water '// &
                                  TRIM(WaterCoil(CoilNum)%Name))
        CALL ShowContinueError('  The maximum water flow rate for this coil will be set to zero and the coil will do no cooling.')
        CALL ShowContinueError('  Check the following coil design inputs for problems: Tair,in = ' //  &
          TRIM(RoundSigDigits(WaterCoil(CoilNum)%DesInletAirTemp,4)))
        CALL ShowContinueError('                                                       Wair,in = ' //  &
          TRIM(RoundSigDigits(WaterCoil(CoilNum)%DesInletAirHumRat,6)))
        CALL ShowContinueError('                                                       Twater,in = ' //  &
          TRIM(RoundSigDigits(WaterCoil(CoilNum)%DesInletWaterTemp,4)))
        CALL ShowContinueError('                                                       Tair,out = ' //  &
          TRIM(RoundSigDigits(WaterCoil(CoilNum)%DesOutletAirTemp,4)))
        CALL ShowContinueError('                                                       Wair,out = ' //  &
          TRIM(RoundSigDigits(WaterCoil(CoilNum)%DesOutletAirHumRat,6)))
        WaterCoil(CoilNum)%MaxWaterVolFlowRate = 0.0d0
        WaterCoil(CoilNum)%MaxWaterMassFlowRate = 0.0d0
      END IF
    END IF

    IF (MyCoilDesignFlag(CoilNum) .and. (WaterCoil(CoilNum)%WaterCoilModel == CoilModel_Cooling).and. &
                  (WaterCoil(CoilNum)%DesAirVolFlowRate .gt. 0.0d0).and.  &
                           (WaterCoil(CoilNum)%MaxWaterMassFlowRate .gt. 0.0d0))THEN  ! 'Cooling'

      MyCoilDesignFlag(CoilNum) = .FALSE.
      NoSatCurveIntersect = .FALSE.
      BelowInletWaterTemp = .FALSE.
      CBFTooLarge = .FALSE.
      NoExitCondReset = .FALSE.
      Inlet_Conditions_Loop: DO Ipass=1,2
        IF (Ipass == 2) THEN
          IF ( .NOT. NoSatCurveIntersect .AND. .NOT. BelowInletWaterTemp .AND. .NOT. CBFTooLarge ) THEN
            EXIT Inlet_Conditions_Loop ! coil UA calcs OK
          ELSE
            CALL ShowWarningError('In calculating the design coil UA for Coil:Cooling:Water '// &
                                  TRIM(WaterCoil(CoilNum)%Name))
            IF (NoSatCurveIntersect) THEN
              CALL ShowContinueError('no apparatus dew-point can be found for the initial entering and leaving conditions;')
            END IF
            IF (BelowInletWaterTemp) THEN
              CALL ShowContinueError('the apparatus dew-point is below the coil design inlet water temperature;')
            END IF
            IF (CBFTooLarge) THEN
              CALL ShowContinueError('the coil bypass factor is unrealistically large;')
            END IF
            IF ( .not. NoExitCondReset) THEN
              CALL ShowContinueError('the coil outlet design conditions will be changed to correct the problem.')
            END IF
            CALL ShowContinueError('The initial design conditions are: Tair,in = ' //  &
               TRIM(RoundSigDigits(WaterCoil(CoilNum)%DesInletAirTemp,4)))
            CALL ShowContinueError('                                   Wair,in = ' //  &
               TRIM(RoundSigDigits(WaterCoil(CoilNum)%DesInletAirHumRat,6)))
            CALL ShowContinueError('                                   Twater,in = ' //  &
               TRIM(RoundSigDigits(WaterCoil(CoilNum)%DesInletWaterTemp,4)))
            CALL ShowContinueError('                                   Tair,out = ' //  &
               TRIM(RoundSigDigits(WaterCoil(CoilNum)%DesOutletAirTemp,4)))
            CALL ShowContinueError('                                   Wair,out = ' //  &
               TRIM(RoundSigDigits(WaterCoil(CoilNum)%DesOutletAirHumRat,6)))
            IF ( .not. NoExitCondReset) THEN
              CALL ShowContinueError('The revised design conditions are: Tair,out = ' //  &
                 TRIM(RoundSigDigits(TOutNew,4)))
              CALL ShowContinueError('                                   Wair,out = ' //  &
                 TRIM(RoundSigDigits(WOutNew,6)))
              WaterCoil(CoilNum)%DesOutletAirHumRat = WOutNew
              WaterCoil(CoilNum)%DesOutletAirTemp = TOutNew
            END IF
          END IF
        END IF

        ! Volume flow rate being converted to mass flow rate for water
        WaterCoil(CoilNum)%DesAirMassFlowRate  = StdRhoAir * WaterCoil(CoilNum)%DesAirVolFlowRate

        ! Enthalpy of Air at Inlet design conditions
        DesInletAirEnth=PsyHFnTdbW(WaterCoil(CoilNum)%DesInletAirTemp,WaterCoil(CoilNum)%DesInletAirHumRat)

        ! Enthalpy of Air at outlet at design conditions
        DesOutletAirEnth=PsyHFnTdbW(WaterCoil(CoilNum)%DesOutletAirTemp,WaterCoil(CoilNum)%DesOutletAirHumRat)

! already calculated above and possibly reset if dry coil
!        ! Enthalpy of Water at Inlet design conditions
!        DesSatEnthAtWaterInTemp =PsyHFnTdbW(WaterCoil(CoilNum)%DesInletWaterTemp, &
!                                             PsyWFnTdpPb(WaterCoil(CoilNum)%DesInletWaterTemp,StdBaroPress))

        ! Total Coil Load from Inlet and Outlet Air States.
        WaterCoil(CoilNum)%DesTotWaterCoilLoad=WaterCoil(CoilNum)%DesAirMassFlowRate*(DesInletAirEnth-DesOutletAirEnth)

        ! Enthalpy of Water at Intlet design conditions
        Cp  =  GetSpecificHeatGlycol(PlantLoop(WaterCoil(CoilNum)%WaterLoopNum)%FluidName,  &
                                 WaterCoil(CoilNum)%DesInletWaterTemp,                      &
                                 PlantLoop(WaterCoil(CoilNum)%WaterLoopNum)%FluidIndex, &
                                 'InitWaterCoil')

        DesOutletWaterTemp = WaterCoil(CoilNum)%DesInletWaterTemp &
                                    + WaterCoil(CoilNum)%DesTotWaterCoilLoad / &
                           (WaterCoil(CoilNum)%MaxWaterMassFlowRate * CP)

        DesSatEnthAtWaterOutTemp = PsyHFnTdbW(DesOutletWaterTemp, &
                                             PsyWFnTdpPb(DesOutletWaterTemp,StdBaroPress))
        DesEnthAtWaterOutTempAirInHumRat = PsyHFnTdbW(DesOutletWaterTemp,WaterCoil(CoilNum)%DesInletAirHumRat)
        DesEnthWaterOut = MIN(DesSatEnthAtWaterOutTemp,DesEnthAtWaterOutTempAirInHumRat)

        ! dry coil test
        IF (WaterCoil(CoilNum)%DesOutletAirHumRat < WaterCoil(CoilNum)%DesInletAirHumRat .AND. &
            DesHumRatAtWaterInTemp < WaterCoil(CoilNum)%DesInletAirHumRat) THEN ! wet coil

          ! Calculations for BYPASS FACTOR at design conditions
          ! Calculate "slope" of temperature vs. humidity ratio between entering and leaving states
          SlopeTempVsHumRatio=(WaterCoil(CoilNum)%DesInletAirTemp-WaterCoil(CoilNum)%DesOutletAirTemp)/ &
                               MAX((WaterCoil(CoilNum)%DesInletAirHumRat-WaterCoil(CoilNum)%DesOutletAirHumRat),SmallNo)

          ! Initialize iteration parameters
          DesAirTempApparatusDewPt = PsyTdpFnWPb(WaterCoil(CoilNum)%DesOutletAirHumRat,OutBaroPress)

          ! Iterating to calculate Apparatus Dew Point Temperature at Design Conditions
          App_Dewpoint_Loop1: DO iter=1,itmax

            ! Calculate apparatus dewpoint and compare with predicted value
            ! using entering conditions and SlopeTempVsHumRatio
            DesAirHumRatApparatusDewPt=  PsyWFnTdpPb(DesAirTempApparatusDewPt,OutBaroPress)

            ! Initial Estimate for apparatus Dew Point Temperature
            TempApparatusDewPtEstimate = WaterCoil(CoilNum)%DesInletAirTemp - SlopeTempVsHumRatio*  &
                                           (WaterCoil(CoilNum)%DesInletAirHumRat-DesAirHumRatApparatusDewPt)

            ! Iterating to calculate Apparatus Dew Point Temperature at Design Condition
            error = DesAirTempApparatusDewPt-TempApparatusDewPtEstimate
            Call ITERATE (ResultX,0.01d0, DesAirTempApparatusDewPt,error,X1,Y1,iter,icvg)
            DesAirTempApparatusDewPt = ResultX

            ! If converged, exit loop
            IF (icvg .EQ. 1) THEN
              Exit App_Dewpoint_Loop1
            END IF

            ! If not converged due to low Humidity Ratio approximate value at outlet conditions
            IF((Iter.EQ.itmax))Then
              NoSatCurveIntersect = .TRUE.
              DesAirTempApparatusDewPt = PsyTdpFnWPb(WaterCoil(CoilNum)%DesOutletAirHumRat,OutBaroPress)
              DesAirHumRatApparatusDewPt = PsyWFnTdpPb(DesAirTempApparatusDewPt,OutBaroPress)
              Exit App_Dewpoint_Loop1
            End IF

            ! End of Do Loop for Iteration
          End Do App_Dewpoint_Loop1

          ! Air enthalpy at apparatus dew point at design conditions
          DesAirApparatusDewPtEnth = PsyHFnTdbW(DesAirTempApparatusDewPt,DesAirHumRatApparatusDewPt)

          ! Calculate bypass factor from enthalpies calculated above.
          DesByPassFactor = (DesOutletAirEnth-DesAirApparatusDewPtEnth)/(DesInletAirEnth-DesAirApparatusDewPtEnth)

          ! Check for bypass factor for unsuitable value. Note that bypass factor is never used in the coil calculation
          If((DesByPassFactor .GT. 0.5d0) .or. (DesByPassFactor .LT. 0.0d0))Then
            CBFTooLarge = .TRUE.
            DesByPassFactor=0.37d0
          End if

          IF (DesEnthWaterOut > DesInletAirEnth) THEN
            CALL ShowWarningError('In calculating the design coil UA for Coil:Cooling:Water '// &
                                    TRIM(WaterCoil(CoilNum)%Name))
            CALL ShowContinueError('the outlet chilled water design enthalpy is greater than the inlet air design enthalpy.')
            CALL ShowContinueError('To correct this condition the design chilled water flow rate will be increased from ' // &
                                   TRIM(RoundSigDigits(WaterCoil(CoilNum)%MaxWaterVolFlowRate,5)))
            EnthCorrFrac = (DesEnthWaterOut - DesInletAirEnth) / (DesEnthWaterOut - DesSatEnthAtWaterInTemp)
            WaterCoil(CoilNum)%MaxWaterVolFlowRate = (1.0d0 + 2.0d0 * EnthCorrFrac) * WaterCoil(CoilNum)%MaxWaterVolFlowRate
            CALL ShowContinueError('to ' // TRIM(RoundSigDigits(WaterCoil(CoilNum)%MaxWaterVolFlowRate,5)) // ' m3/s')
            WaterCoil(CoilNum)%MaxWaterMassFlowRate = rho * WaterCoil(CoilNum)%MaxWaterVolFlowRate
            DesOutletWaterTemp = WaterCoil(CoilNum)%DesInletWaterTemp &
                                      + WaterCoil(CoilNum)%DesTotWaterCoilLoad / &
                                        (WaterCoil(CoilNum)%MaxWaterMassFlowRate * CP)
            DesSatEnthAtWaterOutTemp = PsyHFnTdbW(DesOutletWaterTemp, &
                                                  PsyWFnTdpPb(DesOutletWaterTemp,StdBaroPress))
            DesEnthAtWaterOutTempAirInHumRat = PsyHFnTdbW(DesOutletWaterTemp,WaterCoil(CoilNum)%DesInletAirHumRat)
            DesEnthWaterOut = MIN(DesSatEnthAtWaterOutTemp,DesEnthAtWaterOutTempAirInHumRat)
          END IF

          ! Determine air-side coefficient, UACoilExternal, assuming that the
          ! surface temperature is at the apparatus dewpoint temperature
          IF (DesAirApparatusDewPtEnth .LE. DesSatEnthAtWaterInTemp ) BelowInletWaterTemp = .TRUE.
          IF ((DesInletAirEnth - DesEnthWaterOut) > SmallNo .AND.   &
              (DesOutletAirEnth - DesSatEnthAtWaterInTemp) > SmallNo) THEN
            LogMeanEnthDiff = ((DesInletAirEnth - DesEnthWaterOut) - (DesOutletAirEnth - DesSatEnthAtWaterInTemp)) / &
                                LOG((DesInletAirEnth - DesEnthWaterOut)/(DesOutletAirEnth - DesSatEnthAtWaterInTemp))
          ELSE
            LogMeanEnthDiff = 2000.0d0 ! UA will be 1/2 the design coil load
          END IF
          DesUACoilExternalEnth = WaterCoil(CoilNum)%DesTotWaterCoilLoad/LogMeanEnthDiff
          WaterCoil(CoilNum)%UACoilExternal = DesUACoilExternalEnth *  &
                           PsyCpAirFnWTdb(WaterCoil(CoilNum)%DesInletAirHumRat,WaterCoil(CoilNum)%DesInletAirTemp)

          IF (Ipass == 1 .AND. (NoSatCurveIntersect .OR. CBFTooLarge .OR. BelowInletWaterTemp) ) THEN
            ! reset outlet conditions to 90% relative humidity at the same outlet enthalpy
            TOutNew = TdbFnHRhPb(DesOutletAirEnth,0.9d0,StdBaroPress)
            WOutNew  = PsyWFnTdbH(TOutNew,DesOutletAirEnth)
            IF (WOutNew >= WaterCoil(CoilNum)%DesInletAirHumRat .or. TOutNew > WaterCoil(CoilNum)%DesOutletAirTemp) THEN
              NoExitCondReset = .TRUE.
            END IF
            CYCLE Inlet_Conditions_Loop
          END IF

          WaterCoil(CoilNum)%UACoilInternal = WaterCoil(CoilNum)%UACoilExternal*3.30d0
          ! Overall heat transfer coefficient
          WaterCoil(CoilNum)%UACoilTotal = 1.0d0/(1.d0/WaterCoil(CoilNum)%UACoilExternal+1.d0/WaterCoil(CoilNum)%UACoilInternal)


        ELSE ! dry coil

          IF (DesOutletWaterTemp > WaterCoil(CoilNum)%DesInletAirTemp) THEN
            CALL ShowWarningError('In calculating the design coil UA for Coil:Cooling:Water '// &
                                    TRIM(WaterCoil(CoilNum)%Name))
            CALL ShowContinueError('the outlet chilled water design temperature is greater than the inlet air design temperature.')
            CALL ShowContinueError('To correct this condition the design chilled water flow rate will be increased from ' // &
                                   TRIM(RoundSigDigits(WaterCoil(CoilNum)%MaxWaterVolFlowRate,5)))
            TempCorrFrac = (DesOutletWaterTemp - WaterCoil(CoilNum)%DesInletAirTemp) /   &
               (DesOutletWaterTemp - WaterCoil(CoilNum)%DesInletWaterTemp)
            WaterCoil(CoilNum)%MaxWaterVolFlowRate = (1.0d0 + 2.0d0 * TempCorrFrac) * WaterCoil(CoilNum)%MaxWaterVolFlowRate
            CALL ShowContinueError('to ' // TRIM(RoundSigDigits(WaterCoil(CoilNum)%MaxWaterVolFlowRate,5)) // ' m3/s')
            WaterCoil(CoilNum)%MaxWaterMassFlowRate = rho * WaterCoil(CoilNum)%MaxWaterVolFlowRate
            DesOutletWaterTemp = WaterCoil(CoilNum)%DesInletWaterTemp &
                                      + WaterCoil(CoilNum)%DesTotWaterCoilLoad / &
                                        (WaterCoil(CoilNum)%MaxWaterMassFlowRate * CP)
          END IF

          IF ((WaterCoil(CoilNum)%DesInletAirTemp - DesOutletWaterTemp) > SmallNo .AND. &
              (WaterCoil(CoilNum)%DesOutletAirTemp - WaterCoil(CoilNum)%DesInletWaterTemp) > SmallNo) THEN
            LogMeanTempDiff = ((WaterCoil(CoilNum)%DesInletAirTemp - DesOutletWaterTemp) - &
                               (WaterCoil(CoilNum)%DesOutletAirTemp - WaterCoil(CoilNum)%DesInletWaterTemp)) / &
                               LOG((WaterCoil(CoilNum)%DesInletAirTemp - DesOutletWaterTemp) / &
                               (WaterCoil(CoilNum)%DesOutletAirTemp - WaterCoil(CoilNum)%DesInletWaterTemp))
            WaterCoil(CoilNum)%UACoilExternal = WaterCoil(CoilNum)%DesTotWaterCoilLoad / LogMeanTempDiff
          ELSE
            WaterCoil(CoilNum)%UACoilExternal = WaterCoil(CoilNum)%DesTotWaterCoilLoad / 2.0d0 ! make the UA large
          END IF
          WaterCoil(CoilNum)%UACoilInternal = WaterCoil(CoilNum)%UACoilExternal*3.30d0
          ! Overall heat transfer coefficient
          WaterCoil(CoilNum)%UACoilTotal = 1.0d0/(1.d0/WaterCoil(CoilNum)%UACoilExternal+1.d0/WaterCoil(CoilNum)%UACoilInternal)
          EXIT Inlet_Conditions_Loop

        END IF

      END DO Inlet_Conditions_Loop


      ! estimate the heat external transfer surface area using typical design over all U value
      WaterCoil(CoilNum)%TotCoilOutsideSurfArea=EstimateHEXSurfaceArea(CoilNum)
      ! calculate internal and external "UA per external surface area"
      WaterCoil(CoilNum)%UACoilInternalPerUnitArea=WaterCoil(CoilNum)%UACoilInternal/  &
                                                                      WaterCoil(CoilNum)%TotCoilOutsideSurfArea
      WaterCoil(CoilNum)%UAWetExtPerUnitArea=WaterCoil(CoilNum)%UACoilExternal/WaterCoil(CoilNum)%TotCoilOutsideSurfArea
      ! approximate the dry UA as 1.0 times wet UA
      WaterCoil(CoilNum)%UADryExtPerUnitArea=WaterCoil(CoilNum)%UAWetExtPerUnitArea

      ! Now use SolveRegulaFalsi to "invert" the cooling coil model to obtain the UA given the specified design inlet and outlet conditions
      ! Note that the UAs we have obtained so far are rough estimates that are the starting points for the the following iterative
      !   calulation of the actual UAs.
      Par(1) = WaterCoil(CoilNum)%DesTotWaterCoilLoad
      Par(2) = REAL(CoilNum,r64)
      Par(3) = REAL(ContFanCycCoil,r64) !fan operating mode
      Par(4) = 1.0d0 ! part-load ratio
      WaterCoil(CoilNum)%InletAirTemp = WaterCoil(CoilNum)%DesInletAirTemp
      WaterCoil(CoilNum)%InletAirHumRat = WaterCoil(CoilNum)%DesInletAirHumRat
      WaterCoil(CoilNum)%InletWaterTemp = WaterCoil(CoilNum)%DesInletWaterTemp
      WaterCoil(CoilNum)%InletWaterMassFlowRate = rho * WaterCoil(CoilNum)%MaxWaterVolFlowRate
      WaterCoil(CoilNum)%InletAirMassFlowRate = WaterCoil(CoilNum)%DesAirMassFlowRate
      ! set the lower and upper limits on the UA
      UA0 = 0.1d0 * WaterCoil(CoilNum)%UACoilExternal
      UA1 = 10.0d0 * WaterCoil(CoilNum)%UACoilExternal
      ! Invert the simple cooling coil model: given the design inlet conditions and the design load, find the design UA
      CALL SolveRegulaFalsi(0.001d0, MaxIte, SolFla, UA, SimpleCoolingCoilUAResidual, UA0, UA1, Par)
      ! if the numerical inversion failed, issue error messages.
      IF (SolFla == -1) THEN
        CALL ShowSevereError('Calculation of cooling coil design UA failed for coil '//TRIM(WaterCoil(CoilNum)%Name))
        CALL ShowContinueError('  Iteration limit exceeded in calculating coil UA')
        ! CALL ShowFatalError('Preceeding error causes program termination')
        WaterCoil(CoilNum)%UACoilExternal = UA0*10.0d0
        WaterCoil(CoilNum)%UACoilInternal = WaterCoil(CoilNum)%UACoilExternal*3.3d0
        WaterCoil(CoilNum)%UACoilTotal = 1.0d0/(1.d0/WaterCoil(CoilNum)%UACoilExternal+1.d0/WaterCoil(CoilNum)%UACoilInternal)
        WaterCoil(CoilNum)%TotCoilOutsideSurfArea=EstimateHEXSurfaceArea(CoilNum)
        WaterCoil(CoilNum)%UACoilInternalPerUnitArea=WaterCoil(CoilNum)%UACoilInternal/  &
                                                       WaterCoil(CoilNum)%TotCoilOutsideSurfArea
        WaterCoil(CoilNum)%UAWetExtPerUnitArea=WaterCoil(CoilNum)%UACoilExternal/WaterCoil(CoilNum)%TotCoilOutsideSurfArea
        WaterCoil(CoilNum)%UADryExtPerUnitArea=WaterCoil(CoilNum)%UAWetExtPerUnitArea
        CALL ShowContinueError(' Coil design UA set to ' // TRIM(RoundSigDigits(WaterCoil(CoilNum)%UACoilTotal,6)) // ' [W/C]')
      ELSE IF (SolFla == -2) THEN
        CALL ShowSevereError('Calculation of cooling coil design UA failed for coil '//TRIM(WaterCoil(CoilNum)%Name))
        CALL ShowContinueError('  Bad starting values for UA')
        ! CALL ShowFatalError('Preceeding error causes program termination')
        WaterCoil(CoilNum)%UACoilExternal = UA0*10.0d0
        WaterCoil(CoilNum)%UACoilInternal = WaterCoil(CoilNum)%UACoilExternal*3.3d0
        WaterCoil(CoilNum)%UACoilTotal = 1.0d0/(1.d0/WaterCoil(CoilNum)%UACoilExternal+1.d0/WaterCoil(CoilNum)%UACoilInternal)
        WaterCoil(CoilNum)%TotCoilOutsideSurfArea=EstimateHEXSurfaceArea(CoilNum)
        WaterCoil(CoilNum)%UACoilInternalPerUnitArea=WaterCoil(CoilNum)%UACoilInternal/  &
                                                       WaterCoil(CoilNum)%TotCoilOutsideSurfArea
        WaterCoil(CoilNum)%UAWetExtPerUnitArea=WaterCoil(CoilNum)%UACoilExternal/WaterCoil(CoilNum)%TotCoilOutsideSurfArea
        WaterCoil(CoilNum)%UADryExtPerUnitArea=WaterCoil(CoilNum)%UAWetExtPerUnitArea
        CALL ShowContinueError(' Coil design UA set to ' // TRIM(RoundSigDigits(WaterCoil(CoilNum)%UACoilTotal,6)) // ' [W/C]')
      END IF

       ! cooling coil surface area
      SurfaceArea = WaterCoil(CoilNum)%TotCoilOutsideSurfArea

       ! cooling coil overall UA value
      UATotal = WaterCoil(CoilNum)%UACoilTotal

      ! save the design internal and external UAs
      WaterCoil(CoilNum)%UACoilExternalDes = WaterCoil(CoilNum)%UACoilExternal
      WaterCoil(CoilNum)%UACoilInternalDes = WaterCoil(CoilNum)%UACoilInternal

    End If

    !@@@@ DESIGN CONDITION END HERE @@@@

    ! Calculate rated Total, latent, sensible capacity, SHR, effectiveness
    IF (WaterCoil(CoilNum)%WaterCoilType_Num == WaterCoil_SimpleHeating) THEN
      WaterCoil(CoilNum)%InletAirTemp = 16.6d0
      WaterCoil(CoilNum)%InletAirHumRat = PsyWFnTdbRhPb(16.6d0,0.5d0,StdBaroPress,calledfrom='InitWaterCoil')
      WaterCoil(CoilNum)%InletWaterTemp =82.2d0
    ELSE
      WaterCoil(CoilNum)%InletAirTemp = 26.67d0
      WaterCoil(CoilNum)%InletAirHumRat = PsyWFnTdbTwbPb(26.67d0,19.44d0,StdBaroPress,calledfrom='InitWaterCoil')
      WaterCoil(CoilNum)%InletWaterTemp = 6.67d0
    END IF
    WaterCoil(CoilNum)%InletAirEnthalpy = PsyHFnTdbW(WaterCoil(CoilNum)%InletAirTemp,WaterCoil(CoilNum)%InletAirHumRat,  &
       calledfrom='InitWaterCoil')
    WaterCoil(CoilNum)%InletWaterMassFlowRate = WaterCoil(CoilNum)%MaxWaterMassFlowRate
    WaterCoil(CoilNum)%InletAirMassFlowRate = StdRhoAir*WaterCoil(CoilNum)%DesAirVolFlowRate
    CapacitanceAir = WaterCoil(CoilNum)%InletAirMassFlowRate *   &
       PsyCpAirFnWTdb(WaterCoil(CoilNum)%InletAirHumRat,WaterCoil(CoilNum)%InletAirTemp,calledfrom='InitWaterCoil')

    Cp  =  GetSpecificHeatGlycol(PlantLoop(WaterCoil(CoilNum)%WaterLoopNum)%FluidName,  &
                   WaterCoil(CoilNum)%InletWaterTemp,                      &
                   PlantLoop(WaterCoil(CoilNum)%WaterLoopNum)%FluidIndex, &
                   'InitWaterCoil')

    CapacitanceWater = WaterCoil(CoilNum)%InletWaterMassFlowRate* Cp
    CMin = MIN(CapacitanceAir,CapacitanceWater)
    IF (CMin > 0.0d0) THEN
      IF (WaterCoil(CoilNum)%WaterCoilType_Num == WaterCoil_Cooling) THEN
        CALL CoolingCoil(CoilNum, FirstHVACIteration, DesignCalc,ContFanCycCoil,1.0d0)
        CoilEffectiveness = (WaterCoil(CoilNum)%InletAirTemp - WaterCoil(CoilNum)%OutletAirTemp) / &
                              (WaterCoil(CoilNum)%InletAirTemp - WaterCoil(CoilNum)%InletWaterTemp) * &
                              (CapacitanceAir / CMin)
        RatedLatentCapacity = WaterCoil(CoilNum)%TotWaterCoolingCoilRate - WaterCoil(CoilNum)%SenWaterCoolingCoilRate
        RatedSHR = WaterCoil(CoilNum)%SenWaterCoolingCoilRate / WaterCoil(CoilNum)%TotWaterCoolingCoilRate
      ELSE IF (WaterCoil(CoilNum)%WaterCoilType_Num == WaterCoil_DetFlatFinCooling) THEN
        CALL CalcDetailFlatFinCoolingCoil(CoilNum, DesignCalc,ContFanCycCoil,1.0d0)
        CoilEffectiveness = (WaterCoil(CoilNum)%InletAirTemp - WaterCoil(CoilNum)%OutletAirTemp) / &
                              (WaterCoil(CoilNum)%InletAirTemp - WaterCoil(CoilNum)%InletWaterTemp) * &
                              (CapacitanceAir / CMin)
        RatedLatentCapacity = WaterCoil(CoilNum)%TotWaterCoolingCoilRate - WaterCoil(CoilNum)%SenWaterCoolingCoilRate
        RatedSHR = WaterCoil(CoilNum)%SenWaterCoolingCoilRate / WaterCoil(CoilNum)%TotWaterCoolingCoilRate
      ELSE IF (WaterCoil(CoilNum)%WaterCoilType_Num == WaterCoil_SimpleHeating) THEN
        CALL CalcSimpleHeatingCoil(CoilNum, ContFanCycCoil, 1.0d0, DesignCalc)
        CoilEffectiveness = (WaterCoil(CoilNum)%OutletAirTemp - WaterCoil(CoilNum)%InletAirTemp) / &
                              (WaterCoil(CoilNum)%InletWaterTemp - WaterCoil(CoilNum)%InletAirTemp) * &
                              (CapacitanceAir / CMin)
      END IF
    ELSE
      CoilEffectiveness = 0.0d0
      WaterCoil(CoilNum)%TotWaterHeatingCoilRate = 0.0d0
      WaterCoil(CoilNum)%TotWaterCoolingCoilRate = 0.0d0
      WaterCoil(CoilNum)%SenWaterCoolingCoilRate = 0.0d0
      RatedLatentCapacity = 0.0d0
      RatedSHR = 0.0d0
    END IF
    MyEnvrnFlag(CoilNum) = .FALSE.

  END IF  ! End If for the Begin Environment initializations

  IF (.not. BeginEnvrnFlag) THEN
    MyEnvrnFlag(CoilNum)=.true.
  ENDIF

  IF (.not. DoingSizing) THEN
    IF (MyCoilReportFlag(CoilNum)) THEN
      !create predefined report entries
      MyCoilReportFlag(CoilNum)=.false.
      SELECT CASE (WaterCoil(CoilNum)%WaterCoilType_Num)
        CASE (WaterCoil_SimpleHeating)
          IF (RptCoilHeaderFlag(1)) THEN
            WRITE(OutputFileInits,'(A)') '! <Water Heating Coil Capacity Information>,Component Type,Name,'//  &
               'Nominal Total Capacity {W}'
            RptCoilHeaderFlag(1)=.false.
          ENDIF
          CALL PreDefTableEntry(pdchHeatCoilType,WaterCoil(CoilNum)%Name,'Coil:Heating:Water')
          CALL PreDefTableEntry(pdchHeatCoilDesCap,WaterCoil(CoilNum)%Name,WaterCoil(CoilNum)%DesWaterHeatingCoilRate)
          CALL PreDefTableEntry(pdchHeatCoilNomCap,WaterCoil(CoilNum)%Name,WaterCoil(CoilNum)%TotWaterHeatingCoilRate)
          CALL PreDefTableEntry(pdchHeatCoilNomEff,WaterCoil(CoilNum)%Name,'-')
          CALL addFootNoteSubTable(pdstHeatCoil, 'Nominal values are gross at rated conditions, i.e., the supply air fan' &
                                                  //' heat and electric power NOT accounted for.')
          WRITE(OutputFileInits,'(A)') 'Water Heating Coil Capacity Information,Coil:Heating:Water,'//  &
             trim(WaterCoil(CoilNum)%Name)//','//  &
             trim(RoundSigDigits(WaterCoil(CoilNum)%TotWaterHeatingCoilRate,2))
        CASE (WaterCoil_DetFlatFinCooling)
          IF (RptCoilHeaderFlag(2)) THEN
            WRITE(OutputFileInits,'(A)') '! <Water Cooling Coil Capacity Information>,Component Type,Name,'//  &
               'Nominal Total Capacity {W},Nominal Sensible Capacity {W},Nominal Latent Capacity {W},'//  &
               'Nominal Sensible Heat Ratio'
            RptCoilHeaderFlag(2)=.false.
          ENDIF
          RatedLatentCapacity = WaterCoil(CoilNum)%TotWaterCoolingCoilRate - WaterCoil(CoilNum)%SenWaterCoolingCoilRate
          RatedSHR = SafeDivide(WaterCoil(CoilNum)%SenWaterCoolingCoilRate , WaterCoil(CoilNum)%TotWaterCoolingCoilRate)
          CALL PreDefTableEntry(pdchCoolCoilType,WaterCoil(CoilNum)%Name,'Coil:Cooling:Water:DetailedGeometry')
          CALL PreDefTableEntry(pdchCoolCoilDesCap,WaterCoil(CoilNum)%Name,WaterCoil(CoilNum)%DesWaterCoolingCoilRate)
          CALL PreDefTableEntry(pdchCoolCoilTotCap,WaterCoil(CoilNum)%Name,WaterCoil(CoilNum)%TotWaterCoolingCoilRate)
          CALL PreDefTableEntry(pdchCoolCoilSensCap,WaterCoil(CoilNum)%Name,WaterCoil(CoilNum)%SenWaterCoolingCoilRate)
          CALL PreDefTableEntry(pdchCoolCoilLatCap,WaterCoil(CoilNum)%Name,RatedLatentCapacity)
          CALL PreDefTableEntry(pdchCoolCoilSHR,WaterCoil(CoilNum)%Name,RatedSHR)
          CALL PreDefTableEntry(pdchCoolCoilNomEff,WaterCoil(CoilNum)%Name,'-')
          CALL addFootNoteSubTable(pdstCoolCoil, 'Nominal values are gross at rated conditions, i.e., the supply air fan' &
                                                  //' heat and electric power NOT accounted for.')
          WRITE(OutputFileInits,'(A)') 'Water Cooling Coil Capacity Information,Coil:Cooling:Water:DetailedGeometry,'//  &
             trim(WaterCoil(CoilNum)%Name)//','//  &
             trim(RoundSigDigits(WaterCoil(CoilNum)%TotWaterCoolingCoilRate,2))//','//  &
             trim(RoundSigDigits(WaterCoil(CoilNum)%SenWaterCoolingCoilRate,2))//','//  &
             trim(RoundSigDigits(RatedLatentCapacity,2))//','//trim(RoundSigDigits(RatedSHR,2))
        CASE (WaterCoil_Cooling)
          IF (RptCoilHeaderFlag(2)) THEN
            WRITE(OutputFileInits,'(A)') '! <Water Cooling Coil Capacity Information>,Component Type,Name,'//  &
              'Nominal Total Capacity {W},Nominal Sensible Capacity {W},Nominal Latent Capacity {W},'//  &
              'Nominal Sensible Heat Ratio, Nominal Coil UA Value {W/C}, Nominal Coil Surface Area {m2}'
            RptCoilHeaderFlag(2)=.false.
          ENDIF
          RatedLatentCapacity = WaterCoil(CoilNum)%TotWaterCoolingCoilRate - WaterCoil(CoilNum)%SenWaterCoolingCoilRate
          RatedSHR = SafeDivide(WaterCoil(CoilNum)%SenWaterCoolingCoilRate , WaterCoil(CoilNum)%TotWaterCoolingCoilRate)
          CALL PreDefTableEntry(pdchCoolCoilType,WaterCoil(CoilNum)%Name,'Coil:Cooling:Water')
          CALL PreDefTableEntry(pdchCoolCoilDesCap,WaterCoil(CoilNum)%Name,WaterCoil(CoilNum)%DesWaterCoolingCoilRate)
          CALL PreDefTableEntry(pdchCoolCoilTotCap,WaterCoil(CoilNum)%Name,WaterCoil(CoilNum)%TotWaterCoolingCoilRate)
          CALL PreDefTableEntry(pdchCoolCoilSensCap,WaterCoil(CoilNum)%Name,WaterCoil(CoilNum)%SenWaterCoolingCoilRate)
          CALL PreDefTableEntry(pdchCoolCoilLatCap,WaterCoil(CoilNum)%Name,RatedLatentCapacity)
          CALL PreDefTableEntry(pdchCoolCoilSHR,WaterCoil(CoilNum)%Name,RatedSHR)
          CALL PreDefTableEntry(pdchCoolCoilNomEff,WaterCoil(CoilNum)%Name,'-')
          CALL PreDefTableEntry(pdchCoolCoilUATotal,WaterCoil(CoilNum)%Name,WaterCoil(CoilNum)%UACoilTotal)
          CALL PreDefTableEntry(pdchCoolCoilArea,WaterCoil(CoilNum)%Name,WaterCoil(CoilNum)%TotCoilOutsideSurfArea)
          CALL addFootNoteSubTable(pdstCoolCoil, 'Nominal values are gross at rated conditions, i.e., the supply air fan' &
                                                  //' heat and electric power NOT accounted for.')
          WRITE(OutputFileInits,'(A)') 'Water Cooling Coil Capacity Information,Coil:Cooling:Water,'//  &
             trim(WaterCoil(CoilNum)%Name)//','//  &
             trim(RoundSigDigits(WaterCoil(CoilNum)%TotWaterCoolingCoilRate,2))//','//  &
             trim(RoundSigDigits(WaterCoil(CoilNum)%SenWaterCoolingCoilRate,2))//','//  &
             trim(RoundSigDigits(RatedLatentCapacity,2))//','//trim(RoundSigDigits(RatedSHR,2))//','//  &
             trim(RoundSigDigits(UATotal,2))//','//trim(RoundSigDigits(SurfaceArea,2))
      END SELECT
      IF(WaterCoil(CoilNum)%DesWaterCoolingCoilRate <= 0.0d0) &
           WaterCoil(CoilNum)%DesWaterCoolingCoilRate = WaterCoil(CoilNum)%TotWaterCoolingCoilRate
      IF(WaterCoil(CoilNum)%DesWaterHeatingCoilRate <= 0.0d0) &
           WaterCoil(CoilNum)%DesWaterHeatingCoilRate = WaterCoil(CoilNum)%TotWaterHeatingCoilRate
    ENDIF
  ENDIF

  ! Do the Begin Day initializations
  ! NONE

  ! Do the begin HVAC time step initializations
  ! NONE


  ! Do the following initializations (every time step): This should be the info from
  ! the previous components outlets or the node data in this section.
  !First set the conditions for the air into the coil model
  AirInletNode = WaterCoil(CoilNum)%AirInletNodeNum
  WaterInletNode = WaterCoil(CoilNum)%WaterInletNodeNum
  WaterCoil(CoilNum)%InletAirMassFlowRate = Node(AirInletNode)%MassFlowRate
  WaterCoil(CoilNum)%InletAirTemp         = Node(AirInletNode)%Temp
  WaterCoil(CoilNum)%InletAirHumRat       = Node(AirInletNode)%HumRat
  WaterCoil(CoilNum)%InletAirEnthalpy     = Node(AirInletNode)%Enthalpy

  WaterCoil(CoilNum)%InletWaterMassFlowRate = Node(WaterInletNode)%MassFlowRate
  WaterCoil(CoilNum)%InletWaterTemp         = Node(WaterInletNode)%Temp
  WaterCoil(CoilNum)%InletWaterEnthalpy     = Node(WaterInletNode)%Enthalpy

  WaterCoil(CoilNum)%UACoilVariable         = WaterCoil(CoilNum)%UACoil
  IF ((WaterCoil(CoilNum)%WaterCoilType_Num == WaterCoil_SimpleHeating) .AND. &
     .NOT. ( MyUAAndFlowCalcFlag(CoilNum)) ) THEN    !update Coil UA based on inlet mass flows and temps

     x_a = 1.d0 + 4.769D-3*(WaterCoil(CoilNum)%InletAirTemp - WaterCoil(CoilNum)%DesInletAirTemp)
     IF (WaterCoil(CoilNum)%DesAirMassFlowRate > 0.d0) THEN
       AirConvectTerm  = x_a * ((WaterCoil(CoilNum)%InletAirMassFlowRate/WaterCoil(CoilNum)%DesAirMassFlowRate)**0.8d0) &
                       * WaterCoil(CoilNum)%AirSideNominalConvect
     ELSE
       AirConvectTerm  = 0.d0
     ENDIF
     WaterConvSensitivity = 0.014d0 / (1.d0 + 0.014d0*WaterCoil(CoilNum)%DesInletWaterTemp)
     x_w = 1.d0 + WaterConvSensitivity *(WaterCoil(CoilNum)%InletWaterTemp - WaterCoil(CoilNum)%DesInletWaterTemp)
     IF (WaterCoil(CoilNum)%MaxWaterMassFlowRate > 0.0d0) THEN
       WaterConvectTerm =  x_w * ((WaterCoil(CoilNum)%InletWaterMassFlowRate/WaterCoil(CoilNum)%MaxWaterMassFlowRate)**0.85d0) &
                         * WaterCoil(CoilNum)%LiquidSideNominalConvect
     ELSE
       WaterConvectTerm = 0.d0
     ENDIF
     IF ((AirConvectTerm > 0.d0) .AND. (WaterConvectTerm > 0.d0 )) Then
       WaterCoil(CoilNum)%UACoilVariable =1.d0 / ( (1.d0/WaterConvectTerm) + (1.d0 / AirConvectTerm) )
     ELSE
       ! use nominal UA since variable UA cannot be calculated
       WaterCoil(CoilNum)%UACoilVariable = WaterCoil(CoilNum)%UACoil
     ENDIF
  ENDIF

  !update Coil UA based on inlet mass flows and temps
  IF ( WaterCoil(CoilNum)%WaterCoilType_Num == WaterCoil_Cooling .and. .not. MyCoilDesignFlag(CoilNum) ) THEN

     x_a = 1.d0 + 4.769D-3*(WaterCoil(CoilNum)%InletAirTemp - WaterCoil(CoilNum)%DesInletAirTemp)
     IF (WaterCoil(CoilNum)%DesAirMassFlowRate > 0.d0) THEN
       WaterCoil(CoilNum)%UACoilExternal  = x_a *   &
             ((WaterCoil(CoilNum)%InletAirMassFlowRate/WaterCoil(CoilNum)%DesAirMassFlowRate)**0.8d0) &
                       * WaterCoil(CoilNum)%UACoilExternalDes
     ELSE
       WaterCoil(CoilNum)%UACoilExternal = WaterCoil(CoilNum)%UACoilExternalDes
     ENDIF
     WaterConvSensitivity = 0.014d0 / (1.d0 + 0.014d0*WaterCoil(CoilNum)%DesInletWaterTemp)
     x_w = 1.d0 + WaterConvSensitivity *(WaterCoil(CoilNum)%InletWaterTemp - WaterCoil(CoilNum)%DesInletWaterTemp)
     IF (WaterCoil(CoilNum)%MaxWaterMassFlowRate > 0.0d0) THEN
       WaterCoil(CoilNum)%UACoilInternal =  x_w *   &
          ((WaterCoil(CoilNum)%InletWaterMassFlowRate/WaterCoil(CoilNum)%MaxWaterMassFlowRate)**0.85d0) &
                         * WaterCoil(CoilNum)%UACoilInternalDes
     ELSE
       WaterCoil(CoilNum)%UACoilInternal = WaterCoil(CoilNum)%UACoilInternalDes
     ENDIF
     IF (WaterCoil(CoilNum)%UACoilInternal > 0.0d0 .and. WaterCoil(CoilNum)%UACoilExternal > 0.0d0) THEN
       WaterCoil(CoilNum)%UACoilTotal = 1.0d0/(1.d0/WaterCoil(CoilNum)%UACoilExternal+1.d0/WaterCoil(CoilNum)%UACoilInternal)
     ELSE
       WaterCoil(CoilNum)%UACoilInternal = WaterCoil(CoilNum)%UACoilInternalDes
       WaterCoil(CoilNum)%UACoilExternal = WaterCoil(CoilNum)%UACoilExternalDes
       WaterCoil(CoilNum)%UACoilTotal = 1.0d0/(1.d0/WaterCoil(CoilNum)%UACoilExternal+1.d0/WaterCoil(CoilNum)%UACoilInternal)
     END IF
     WaterCoil(CoilNum)%UACoilInternalPerUnitArea = WaterCoil(CoilNum)%UACoilInternal/  &
                                                      WaterCoil(CoilNum)%TotCoilOutsideSurfArea
     WaterCoil(CoilNum)%UAWetExtPerUnitArea = WaterCoil(CoilNum)%UACoilExternal/WaterCoil(CoilNum)%TotCoilOutsideSurfArea
     WaterCoil(CoilNum)%UADryExtPerUnitArea = WaterCoil(CoilNum)%UAWetExtPerUnitArea
  ENDIF

  WaterCoil(CoilNum)%TotWaterHeatingCoilRate = 0.0d0
  WaterCoil(CoilNum)%TotWaterCoolingCoilRate = 0.0d0
  WaterCoil(CoilNum)%SenWaterCoolingCoilRate = 0.0d0

  RETURN

END SUBROUTINE InitWaterCoil

SUBROUTINE SizeWaterCoil(CoilNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   November 2001
          !       MODIFIED       August 2013 Daeho Kang, add component sizing table entries
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is for sizing Water Coil Components for which flow rates and UAs have not been
          ! specified in the input.

          ! METHODOLOGY EMPLOYED:
          ! Obtains flow rates from the zone or system sizing arrays and plant sizing data. UAs are
          ! calculated by numerically inverting the individual coil calculation routines.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataSizing
  USE General, ONLY: SolveRegulaFalsi, TrimSigDigits, RoundSigDigits
  USE PlantUtilities, ONLY: RegisterPlantCompDesignFlow
  USE DataEnvironment, ONLY: StdBaroPress
  USE DataAirSystems, ONLY: PrimaryAirSystem
!  USE BranchInputManager, ONLY: MyPlantSizingIndex
  USE ReportSizingManager, ONLY: ReportSizingOutput
  USE General,             ONLY: RoundSigDigits

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  Integer, Intent(IN) :: CoilNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
  INTEGER, PARAMETER          :: MaxIte = 500        ! Maximum number of iterations
  REAL(r64), PARAMETER :: Acc =  0.0001d0       ! Accuracy of result

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER             :: PltSizNum     ! do loop index for plant sizing
  INTEGER             :: PltSizCoolNum ! index of plant sizing object for 1st cooling loop
  INTEGER             :: PltSizHeatNum ! index of plant sizing object for 1st heating loop
  LOGICAL             :: ErrorsFound   ! If errors detected in input
  REAL(r64)           :: CoilInTemp
  REAL(r64)           :: CoilOutTemp
  REAL(r64)           :: CoilOutHumRat
  REAL(r64)           :: CoilInHumRat
  REAL(r64)           :: DesCoilLoad
  REAL(r64)           :: DesMassFlow
  REAL(r64)           :: DesVolFlow
  REAL(r64)           :: MinFlowFrac
  REAL(r64)           :: FCOAFrac
  REAL(r64)           :: OutAirFrac
  REAL(r64)           :: CoilDesWaterDeltaT  ! water delta T used to calculate the design water flow rate
  REAL(r64)           :: RhoAirStd     ! density of air at standard conditions
  REAL(r64)           :: CpAirStd      ! specific heat of air at std conditions
  INTEGER             :: SolFla              ! Flag of solver
  REAL(r64)           :: UA0                 ! lower bound for UA
  REAL(r64)           :: UA1                 ! upper bound for UA
  REAL(r64)           :: UA
  REAL(r64), DIMENSION(4)  :: Par
  LOGICAL             :: LoopErrorsFound
  REAL(r64)           :: Cp  !
  REAL(r64)           :: rho !
  REAL(r64)           :: DesWaterVolFlow = 0.0D0
  REAL(r64)           :: DesSatEnthAtWaterInTemp ! used in testing coil inputs
  REAL(r64)           :: DesHumRatAtWaterInTemp  ! used in testing coil inputs
  REAL(r64)           :: T1Out = 0.0D0
  REAL(r64)           :: T2Out = 0.0D0
  REAL(r64)           :: TDpIn = 0.0D0
  LOGICAL             :: IsAutosize                     ! Indicator to autosize
  LOGICAL             :: IsAutosizeReq                  ! Indicator to autosizereq
  LOGICAL             :: HardSizeNoDesRun               ! Indicator to hardsize with sizing run
  REAL(r64)           :: MaxWaterVolFlowRateDes         ! Autosized max water flow for reporting
  REAL(r64)           :: MaxWaterVolFlowRateUser        ! Hardsized max water flow for reporting
  REAL(r64)           :: DesAirVolFlowRateDes           ! Autosized design air flow for reporting
  REAL(r64)           :: DesAirVolFlowRateUser          ! Hardsized design air flow for reporting
  REAL(r64)           :: DesInletAirtempDes             ! Autosized design inlet air temperature for reporting
  REAL(r64)           :: DesInletAirtempUser            ! Hardsized design inlet air temperature for reporting
  REAL(r64)           :: DesInletWaterTempDes           ! Autosized design inlet water temperature for reporting
  REAL(r64)           :: DesInletWaterTempUser          ! Hardsized design inlet water temperature for reporting
  REAL(r64)           :: DesOutletAirtempDes            ! Autosized design outlet air temperature for reporting
  REAL(r64)           :: DesOutletAirtempUser           ! Hardsized design outlet air temperature for reporting
  REAL(r64)           :: DesInletAirHumRatDes           ! Autosized design inlet air humidity ratio for reporting
  REAL(r64)           :: DesInletAirHumRatUser          ! Hardsized design inlet air humidity ratio for reporting
  REAL(r64)           :: DesOutletAirHumRatDes          ! Autosized design outlet air humidity ratio for reporting
  REAL(r64)           :: DesOutletAirHumRatUser         ! Hardsized design outlet air humidity ratio for reporting
  REAL(r64)           :: NumofTubesperRowDes            ! Autosized number of tubes per row for reporting
  REAL(r64)           :: NumofTubesperRowUser           ! Hardsized number of tubes per row for reporting
  REAL(r64)           :: FinDiamDes                     ! Autosized fin diameter for reporting
  REAL(r64)           :: FinDiamUser                    ! Hardsized fin diameter for reporting
  REAL(r64)           :: MinAirFlowAreaDes              ! Autosized min air flow area for reporting
  REAL(r64)           :: MinAirFlowAreaUser             ! Hardsized min air flow area for reporting
  REAL(r64)           :: FinSurfAreaDes                 ! Autosized fin surface area for reporting
  REAL(r64)           :: FinSurfAreaUser                ! Hardsized fin surface area for reporting
  REAL(r64)           :: TotTubeInsideAreaDes           ! Autosized total tube inside area for reporting
  REAL(r64)           :: TotTubeInsideAreaUser          ! Hardsized total tube inside area for reporting
  REAL(r64)           :: TubeOutsideSurfAreaDes         ! Autosized total tube outside surface area for reporting
  REAL(r64)           :: TubeOutsideSurfAreaUser        ! Hardsized total tube outside surface area for reporting
  REAL(r64)           :: CoilDepthDes                   ! Autosized coil depth for reporting
  REAL(r64)           :: CoilDepthUser                  ! Hardsized coil depth for reporting
  REAL(r64)           :: DesWaterHeatingCoilRateDes     ! Autosized rated capacity for reporting
  REAL(r64)           :: DesWaterHeatingCoilRateUser    ! Hardsized rated capacity for reporting
  REAL(r64)           :: UACoilDes                      ! Autosized UA for reporting
  REAL(r64)           :: UACoilUser                     ! Hardsized UA for reporting
  LOGICAL :: SizingDesRunThisAirSys            ! true if a particular air system had a Sizing:System object and system sizing done
  LOGICAL :: SizingDesRunThisZone              ! true if a particular zone had a Sizing:Zone object and zone sizing was done

  ErrorsFound = .FALSE.
  IsAutosize = .FALSE.
  IsAutosizeReq = .FALSE.
  IF (SysSizingRunDone .OR. ZoneSizingRunDone) THEN
    HardSizeNoDesRun = .FALSE.
  ELSE
    HardSizeNoDesRun = .TRUE.
  ENDIF
  IF (CurSysNum > 0) THEN
    CALL CheckThisAirSystemForSizing(CurSysNum, SizingDesRunThisAirSys )
  ELSE
    SizingDesRunThisAirSys =  .FALSE.
  ENDIF
  IF (CurZoneEqNum > 0) THEN
    CALL CheckThisZoneForSizing(CurZoneEqNum, SizingDesRunThisZone)
  ELSE
    SizingDesRunThisZone =  .FALSE.
  ENDIF
  PltSizCoolNum = 0
  PltSizHeatNum = 0
  PltSizNum = 0
  CoilInTemp = 0.0d0
  CoilInHumRat = 0.0d0
  CoilOutTemp = 0.0d0
  DesCoilLoad = 0.0d0
  MinFlowFrac = 0.0d0
  DesMassFlow = 0.0d0
  RhoAirStd = StdRhoAir
  CpAirStd = PsyCpAirFnWTdb(0.0d0,20.0d0)
  CoilDesWaterDeltaT = 0.0d0
  LoopErrorsFound = .FALSE.
  DesVolFlow = 0.0d0
  MaxWaterVolFlowRateDes = 0.0d0
  MaxWaterVolFlowRateUser = 0.0d0
  DesAirVolFlowRateDes = 0.0d0
  DesAirVolFlowRateUser = 0.0d0
  DesInletAirtempDes = 0.0d0
  DesInletAirtempUser = 0.0d0
  DesInletWaterTempDes = 0.0d0
  DesInletWaterTempUser = 0.0d0
  DesOutletAirtempDes = 0.0d0
  DesOutletAirtempUser = 0.0d0
  DesInletAirHumRatDes = 0.0d0
  DesInletAirHumRatUser = 0.0d0
  DesOutletAirHumRatDes = 0.0d0
  DesOutletAirHumRatUser = 0.0d0
  NumofTubesperRowDes = 0.0d0
  NumofTubesperRowUser = 0.0d0
  FinDiamDes = 0.0d0
  FinDiamUser = 0.0d0
  MinAirFlowAreaDes = 0.0d0
  MinAirFlowAreaUser = 0.0d0
  FinSurfAreaDes = 0.0d0
  FinSurfAreaUser = 0.0d0
  TotTubeInsideAreaDes = 0.0d0
  TotTubeInsideAreaUser = 0.0d0
  TubeOutsideSurfAreaDes = 0.0d0
  TubeOutsideSurfAreaUser = 0.0d0
  CoilDepthDes = 0.0d0
  CoilDepthUser = 0.0d0
  DesWaterHeatingCoilRateDes = 0.0d0
  DesWaterHeatingCoilRateUser = 0.0d0
  UACoilDes = 0.0d0
  UACoilUser = 0.0d0

  ! cooling coils
  IF (WaterCoil(CoilNum)%WaterCoilType == CoilType_Cooling .and.   &
      WaterCoil(CoilNum)%RequestingAutosize) THEN
    ! find the appropriate Plant Sizing object
    PltSizCoolNum = MyPlantSizingIndex("chilled water coil", WaterCoil(CoilNum)%Name, WaterCoil(CoilNum)%WaterInletNodeNum, &
                                       WaterCoil(CoilNum)%WaterOutletNodeNum, LoopErrorsFound)
  ENDIF

  Cp =  GetSpecificHeatGlycol(PlantLoop(WaterCoil(CoilNum)%WaterLoopNum)%FluidName,  &
                       5.0d0,                      &
                       PlantLoop(WaterCoil(CoilNum)%WaterLoopNum)%FluidIndex, &
                       'SizeWaterCoil')

  rho = GetDensityGlycol(PlantLoop(WaterCoil(CoilNum)%WaterLoopNum)%FluidName,  &
                       InitConvTemp,                      &
                       PlantLoop(WaterCoil(CoilNum)%WaterLoopNum)%FluidIndex, &
                       'SizeWaterCoil')

  IF (WaterCoil(CoilNum)%WaterCoilType == CoilType_Cooling) THEN   ! 'Cooling'
    IF (PltSizCoolNum > 0) THEN
      ! if this is a central system coil
      IF (CurSysNum > 0) THEN
        IF (WaterCoil(CoilNum)%RequestingAutosize) THEN
          IsAutosizeReq = .TRUE.
        END IF
        IF (.NOT. IsAutosizeReq .AND. .NOT. SizingDesRunThisAirSys) THEN ! Simulation continue
          IF (WaterCoil(CoilNum)%WaterCoilModel == CoilModel_Cooling) THEN
            IF (WaterCoil(CoilNum)%MaxWaterVolFlowRate > 0.0d0) THEN
              CALL ReportSizingOutput('Coil:Cooling:Water',WaterCoil(CoilNum)%Name, &
                                    'User-Specified Design Water Flow Rate [m3/s]',WaterCoil(CoilNum)%MaxWaterVolFlowRate)
            END IF
            IF (WaterCoil(CoilNum)%DesAirVolFlowRate > 0.0d0) THEN
                CALL ReportSizingOutput('Coil:Cooling:Water',WaterCoil(CoilNum)%Name, &
                                    'User-Specified Design Air Flow Rate [m3/s]',WaterCoil(CoilNum)%DesAirVolFlowRate)
            END IF
            IF (WaterCoil(CoilNum)%DesInletAirtemp > 0.0d0) THEN
              CALL ReportSizingOutput('Coil:Cooling:Water',WaterCoil(CoilNum)%Name, &
                                    'User-Specified Design Inlet Air Temperature [C]',WaterCoil(CoilNum)%DesInletAirtemp)
            END IF
            IF (WaterCoil(CoilNum)%DesInletwatertemp > 0.0d0) THEN
              CALL ReportSizingOutput('Coil:Cooling:Water',WaterCoil(CoilNum)%Name, &
                                    'User-Specified Design Inlet Water Temperature [C]',WaterCoil(CoilNum)%DesInletwatertemp)
            END IF
            IF (WaterCoil(CoilNum)%DesOutletAirTemp > 0.0d0) THEN
              CALL ReportSizingOutput('Coil:Cooling:Water',WaterCoil(CoilNum)%Name, &
                                    'User-Specified Design Outlet Air Temperature [C]',WaterCoil(CoilNum)%DesOutletAirTemp)
            END IF
            IF (WaterCoil(CoilNum)%DesInletAirHumRat > 0.0d0) THEN
              CALL ReportSizingOutput('Coil:Cooling:Water',WaterCoil(CoilNum)%Name,&
                                    'User-Specified Design Inlet Air Humidity Ratio',WaterCoil(CoilNum)%DesInletAirHumRat)
            END IF
            IF (WaterCoil(CoilNum)%DesOutletAirHumRat > 0.0d0) THEN
              CALL ReportSizingOutput('Coil:Cooling:Water',WaterCoil(CoilNum)%Name, &
                                    'User-Specified Design Outlet Air Humidity Ratio',WaterCoil(CoilNum)%DesOutletAirHumRat)
            END IF

          ELSE IF(WaterCoil(CoilNum)%WaterCoilModel == CoilModel_Detailed) THEN

            IF (WaterCoil(CoilNum)%MaxWaterVolFlowRate > 0.0d0) THEN
                CALL ReportSizingOutput('Coil:Cooling:Water:DetailedGeometry',WaterCoil(CoilNum)%Name, &
                                    'User-Specified Maximum Water Flow Rate [m3/s]',WaterCoil(CoilNum)%MaxWaterVolFlowRate)
            END IF
            IF (WaterCoil(CoilNum)%TubeOutsideSurfArea > 0.0d0) THEN
              CALL ReportSizingOutput('Coil:Cooling:Water:DetailedGeometry',WaterCoil(CoilNum)%Name, &
                                    'User-Specified Tube Outside Surface Area [m2]',WaterCoil(CoilNum)%TubeOutsideSurfArea)
            END IF
            IF (WaterCoil(CoilNum)%TotTubeInsideArea > 0.0d0) THEN
              CALL ReportSizingOutput('Coil:Cooling:Water:DetailedGeometry',WaterCoil(CoilNum)%Name, &
                                    'User-Specified Total Tube Inside Area [m2]',WaterCoil(CoilNum)%TotTubeInsideArea)
            END IF
            IF (WaterCoil(CoilNum)%FinSurfArea > 0.0d0) THEN
              CALL ReportSizingOutput('Coil:Cooling:Water:DetailedGeometry',WaterCoil(CoilNum)%Name, &
                                    'User-Specified Fin Surface Area [m2]',WaterCoil(CoilNum)%FinSurfArea)
            END IF
            IF (WaterCoil(CoilNum)%MinAirFlowArea > 0.0d0) THEN
              CALL ReportSizingOutput('Coil:Cooling:Water:DetailedGeometry',WaterCoil(CoilNum)%Name, &
                                    'User-Specified Minimum Airflow Area [m2]',WaterCoil(CoilNum)%MinAirFlowArea)
            END IF
            IF (WaterCoil(CoilNum)%CoilDepth > 0.0d0) THEN
              CALL ReportSizingOutput('Coil:Cooling:Water:DetailedGeometry',WaterCoil(CoilNum)%Name, &
                                    'User-Specified Coil Depth [m]',WaterCoil(CoilNum)%CoilDepth)
            END IF
            IF (WaterCoil(CoilNum)%FinDiam > 0.0d0) THEN
              CALL ReportSizingOutput('Coil:Cooling:Water:DetailedGeometry',WaterCoil(CoilNum)%Name, &
                                    'User-Specified Fin Diameter [m]',WaterCoil(CoilNum)%FinDiam)
            END IF
            IF (WaterCoil(CoilNum)%NumofTubesperRow > 0.0d0) THEN
              NumofTubesperRowUser = WaterCoil(CoilNum)%NumofTubesperRow ! convert integer to real for reporting
              CALL ReportSizingOutput('Coil:Cooling:Water:DetailedGeometry',WaterCoil(CoilNum)%Name, &
                                    'User-Specified Number of Tubes per Row',NumofTubesperRowUser)
            END IF
          END IF
        ELSE ! At least one field is autosize
          If(WaterCoil(CoilNum)%WaterCoilModel == CoilModel_Detailed) Then  ! 'DETAILED FLAT FIN'
            CALL CheckSysSizing('Coil:Cooling:Water:DetailedGeometry',WaterCoil(CoilNum)%Name)
          ElseIf(WaterCoil(CoilNum)%WaterCoilModel == CoilModel_Cooling) Then  ! 'Cooling'
            CALL CheckSysSizing('Coil:Cooling:Water',WaterCoil(CoilNum)%Name)
          End If
        !ENDIF
        ! if the water flow rate needs autosizing, calculate the volume flow rate
!          If(WaterCoil(CoilNum)%WaterCoilModel == CoilModel_Detailed) Then  ! 'DETAILED FLAT FIN'
!            CALL CheckSysSizing('Coil:Cooling:Water:DetailedGeometry',WaterCoil(CoilNum)%Name)
!          ElseIf(WaterCoil(CoilNum)%WaterCoilModel == CoilModel_Cooling) Then  ! 'Cooling'
!            CALL CheckSysSizing('Coil:Cooling:Water',WaterCoil(CoilNum)%Name)
!          End If
          IsAutosize = .FALSE.
          IF (WaterCoil(CoilNum)%MaxWaterVolFlowRate == AutoSize) THEN
            IsAutosize = .TRUE.
          END IF
          IF (CurOASysNum > 0) THEN
            WaterCoil(CoilNum)%InletAirMassFlowRate = RhoAirStd*FinalSysSizing(CurSysNum)%DesOutAirVolFlow
          ELSE
            SELECT CASE(CurDuctType)
              CASE(Main)
                WaterCoil(CoilNum)%InletAirMassFlowRate = RhoAirStd*FinalSysSizing(CurSysNum)%DesMainVolFlow
                DesVolFlow = FinalSysSizing(CurSysNum)%DesMainVolFlow
              CASE(Cooling)
                WaterCoil(CoilNum)%InletAirMassFlowRate = RhoAirStd*FinalSysSizing(CurSysNum)%DesCoolVolFlow
                DesVolFlow = FinalSysSizing(CurSysNum)%DesCoolVolFlow
              CASE(Heating)
                WaterCoil(CoilNum)%InletAirMassFlowRate = RhoAirStd*FinalSysSizing(CurSysNum)%DesHeatVolFlow
                DesVolFlow = FinalSysSizing(CurSysNum)%DesHeatVolFlow
              CASE(Other)
                WaterCoil(CoilNum)%InletAirMassFlowRate = RhoAirStd*FinalSysSizing(CurSysNum)%DesMainVolFlow
                DesVolFlow = FinalSysSizing(CurSysNum)%DesMainVolFlow
              CASE DEFAULT
                WaterCoil(CoilNum)%InletAirMassFlowRate = RhoAirStd*FinalSysSizing(CurSysNum)%DesMainVolFlow
                DesVolFlow = FinalSysSizing(CurSysNum)%DesMainVolFlow
            END SELECT
          END IF
          IF (CurOASysNum > 0) THEN ! coil is in the OA system
            CoilInTemp = FinalSysSizing(CurSysNum)%CoolOutTemp
            CoilOutTemp = FinalSysSizing(CurSysNum)%PrecoolTemp
            CoilOutHumRat = FinalSysSizing(CurSysNum)%PrecoolHumRat
            CoilInHumRat = FinalSysSizing(CurSysNum)%CoolOutHumRat
          ELSE ! coil is in the main air loop
            IF (PrimaryAirSystem(CurSysNum)%NumOACoolCoils == 0) THEN ! there is no precooling of the OA stream
              CoilInTemp = FinalSysSizing(CurSysNum)%CoolMixTemp
              CoilInHumRat = FinalSysSizing(CurSysNum)%CoolMixHumRat
            ELSE ! thereis precooling of the OA stream
              IF (DesVolFlow > 0.0d0) THEN
                OutAirFrac = FinalSysSizing(CurSysNum)%DesOutAirVolFlow / DesVolFlow
              ELSE
                OutAirFrac = 1.0d0
              END IF
              OutAirFrac = MIN(1.0d0,MAX(0.0d0,OutAirFrac))
              CoilInTemp = OutAirFrac*FinalSysSizing(CurSysNum)%PrecoolTemp + &
                         (1.0d0-OutAirFrac)*FinalSysSizing(CurSysNum)%CoolRetTemp
              CoilInHumRat = OutAirFrac*FinalSysSizing(CurSysNum)%PrecoolHumRat + &
                         (1.0d0-OutAirFrac)*FinalSysSizing(CurSysNum)%CoolRetHumRat
            END IF
            CoilOutHumRat = FinalSysSizing(CurSysNum)%CoolSupHumRat
            CoilOutTemp = FinalSysSizing(CurSysNum)%CoolSupTemp
          END IF
! None of these IF tests for ".and. WaterCoil(CoilNum)%WaterCoilModel==CoilModel_Cooling" are needed inside the outermost IF block
          If((CoilOutHumRat.GT.CoilInHumRat).and.(WaterCoil(CoilNum)%WaterCoilModel == CoilModel_Cooling)) Then
            CALL ShowWarningError('SizeWaterCoil: Coil="'//TRIM(WaterCoil(CoilNum)%Name)//  &
                                  '", Cooling Coil has leaving humidity ratio > entering humidity ratio.')
            CALL ShowContinueError('    Wair,in =  ' // TRIM(RoundSigDigits(CoilInHumRat,6)))
            CALL ShowContinueError('    Wair,out = ' // TRIM(RoundSigDigits(CoilOutHumRat,6)))
            IF (CoilInHumRat > .016d0) THEN
              CoilOutHumRat = 0.5d0*CoilInHumRat
            ELSE
              CoilOutHumRat = CoilInHumRat
            END IF
            CALL ShowContinueError('....coil chilled water flow rate will be sized using:')
            CALL ShowContinueError('    Wair,out = ' // TRIM(RoundSigDigits(CoilOutHumRat,6)))
          End if
          DesCoilLoad = WaterCoil(CoilNum)%InletAirMassFlowRate  &
                          * (PsyHFnTdbW(CoilInTemp, CoilInHumRat)-PsyHFnTdbW(CoilOutTemp, CoilOutHumRat))
          IF (CurOASysNum > 0) THEN
            CoilDesWaterDeltaT = 0.5d0 * PlantSizData(PltSizCoolNum)%DeltaT
          ELSE
            CoilDesWaterDeltaT = PlantSizData(PltSizCoolNum)%DeltaT
          END IF
          IF (DesCoilLoad  >= SmallLoad) THEN

            MaxWaterVolFlowRateDes = DesCoilLoad / &
                                     ( CoilDesWaterDeltaT * Cp * rho )
          ELSE
            MaxWaterVolFlowRateDes = 0.0d0
            CALL ShowWarningError('The design coil load is zero for Coil:Cooling:Water ' &
                                  //TRIM(WaterCoil(CoilNum)%Name))
            CALL ShowContinueError('The autosize value for max water flow rate is zero')
          END IF
          WaterCoil(CoilNum)%DesWaterCoolingCoilRate = DesCoilLoad
          IF (IsAutosize) THEN
            WaterCoil(CoilNum)%MaxWaterVolFlowRate = MaxWaterVolFlowRateDes
            If(WaterCoil(CoilNum)%WaterCoilModel == CoilModel_Detailed) Then  ! 'DETAILED FLAT FIN'
              CALL ReportSizingOutput('Coil:Cooling:Water:DetailedGeometry',WaterCoil(CoilNum)%Name, &
                                    'Design Size Design Coil Load [W]',WaterCoil(CoilNum)%DesWaterCoolingCoilRate)
            ElseIf(WaterCoil(CoilNum)%WaterCoilModel == CoilModel_Cooling) Then  ! 'Cooling'
              CALL ReportSizingOutput('Coil:Cooling:Water',WaterCoil(CoilNum)%Name, &
                                    'Design Size Design Coil Load [W]',WaterCoil(CoilNum)%DesWaterCoolingCoilRate)
            END IF
            If(WaterCoil(CoilNum)%WaterCoilModel == CoilModel_Detailed) Then  ! 'DETAILED FLAT FIN'
              CALL ReportSizingOutput('Coil:Cooling:Water:DetailedGeometry',WaterCoil(CoilNum)%Name, &
                                    'Design Size Maximum Water Flow Rate [m3/s]',MaxWaterVolFlowRateDes)
            ElseIf(WaterCoil(CoilNum)%WaterCoilModel == CoilModel_Cooling) Then  ! 'Cooling'
              CALL ReportSizingOutput('Coil:Cooling:Water',WaterCoil(CoilNum)%Name, &
                                    'Design Size Design Water Flow Rate [m3/s]',MaxWaterVolFlowRateDes)
            END IF
          ELSE
            MaxWaterVolFlowRateUser = WaterCoil(CoilNum)%MaxWaterVolFlowRate
            IF (WaterCoil(CoilNum)%MaxWaterVolFlowRate > 0.0d0 .AND. MaxWaterVolFlowRateDes > 0.0d0) THEN
              IF(WaterCoil(CoilNum)%WaterCoilModel == CoilModel_Detailed) THEN  ! 'DETAILED FLAT FIN'
                CALL ReportSizingOutput('Coil:Cooling:Water:DetailedGeometry',WaterCoil(CoilNum)%Name, &
                                    'Design Size Maximum Water Flow Rate [m3/s]',MaxWaterVolFlowRateDes, &
                                    'User-Specified Maximum Water Flow Rate [m3/s]',MaxWaterVolFlowRateUser)
                IF (DisplayExtraWarnings) THEN
                  IF ((ABS(MaxWaterVolFlowRateDes - MaxWaterVolFlowRateUser)/ MaxWaterVolFlowRateUser) &
                            > AutoVsHardSizingThreshold) THEN
                    CALL ShowMessage('SizeWaterCoil: Potential issue with equipment sizing for Coil = ' &
                                       //TRIM(WaterCoil(CoilNum)%Name))
                    CALL ShowContinueError('User-Specified Maximum Water Flow Rate of '// &
                                      TRIM(RoundSigDigits(MaxWaterVolFlowRateUser,5))// ' [m3/s]')
                    CALL ShowContinueError('differs from Design Size Maximum Water Flow Rate of ' // &
                                      TRIM(RoundSigDigits(MaxWaterVolFlowRateDes,5))// ' [m3/s]')
                    CALL ShowContinueError('This may, or may not, indicate mismatched component sizes.')
                    CALL ShowContinueError('Verify that the value entered is intended and is consistent with other components.')
                  END IF
                ENDIF
              ELSEIF (WaterCoil(CoilNum)%WaterCoilModel == CoilModel_Cooling) THEN  ! 'Cooling'
                CALL ReportSizingOutput('Coil:Cooling:Water',WaterCoil(CoilNum)%Name, &
                                    'Design Size Design Water Flow Rate [m3/s]',MaxWaterVolFlowRateDes, &
                                    'User-Specified Design Water Flow Rate [m3/s]',MaxWaterVolFlowRateUser)
                IF (DisplayExtraWarnings) THEN
                  IF ((ABS(MaxWaterVolFlowRateDes - MaxWaterVolFlowRateUser)/MaxWaterVolFlowRateUser) &
                                  > AutoVsHardSizingThreshold) THEN
                    CALL ShowMessage('SizeWaterCoil: Potential issue with equipment sizing for Coil = ' &
                                         //TRIM(WaterCoil(CoilNum)%Name))
                    CALL ShowContinueError('User-Specified Design Water Flow Rate of '// &
                                      TRIM(RoundSigDigits(MaxWaterVolFlowRateUser,5))// ' [m3/s]')
                    CALL ShowContinueError('differs from Design Size Design Water Flow Rate of ' // &
                                      TRIM(RoundSigDigits(MaxWaterVolFlowRateDes,5))// ' [m3/s]')
                    CALL ShowContinueError('This may, or may not, indicate mismatched component sizes.')
                    CALL ShowContinueError('Verify that the value entered is intended and is consistent with other components.')
                  END IF
                ENDIF
              END IF
            END IF
          END IF

        ! if the design input coil design air vol flow rate input needs autosizing, calculate it
          IF (WaterCoil(CoilNum)%WaterCoilModel == CoilModel_Cooling) THEN
            IsAutosize= .FALSE.
            IF (WaterCoil(CoilNum)%DesAirVolFlowRate == AutoSize) THEN   ! 'Cooling'
            IsAutosize = .TRUE.
            END IF
!          CALL CheckSysSizing('Coil:Cooling:Water',WaterCoil(CoilNum)%Name)
            IF (CurOASysNum > 0) THEN
              DesAirVolFlowRateDes = FinalSysSizing(CurSysNum)%DesOutAirVolFlow
            ELSE
              SELECT CASE(CurDuctType)
                CASE(Main)
                  DesAirVolFlowRateDes = FinalSysSizing(CurSysNum)%DesMainVolFlow
                CASE(Cooling)
                 DesAirVolFlowRateDes = FinalSysSizing(CurSysNum)%DesCoolVolFlow
                CASE(Heating)
                  DesAirVolFlowRateDes = FinalSysSizing(CurSysNum)%DesHeatVolFlow
                CASE(Other)
                  DesAirVolFlowRateDes = FinalSysSizing(CurSysNum)%DesMainVolFlow
                CASE DEFAULT
                  DesAirVolFlowRateDes = FinalSysSizing(CurSysNum)%DesMainVolFlow
              END SELECT
            END IF
            IF (DesAirVolFlowRateDes  <= 0.0d0) THEN
              DesAirVolFlowRateDes = 0.0d0
              CALL ShowWarningError('The design air flow rate is zero for Coil:Cooling:Water ' &
                                  //TRIM(WaterCoil(CoilNum)%Name))
              CALL ShowContinueError('The autosize value for max air volume flow rate is zero')
            END IF
            IF (IsAutosize) THEN
              WaterCoil(CoilNum)%DesAirVolFlowRate = DesAirVolFlowRateDes
              IF (WaterCoil(CoilNum)%WaterCoilModel == CoilModel_Cooling) THEN  ! 'Cooling'
                CALL ReportSizingOutput('Coil:Cooling:Water',WaterCoil(CoilNum)%Name, &
                                    'Design Size Design Air Flow Rate [m3/s]',DesAirVolFlowRateDes)
              END IF
            ELSE
              IF (WaterCoil(CoilNum)%DesAirVolFlowRate > 0.0d0 .AND. DesAirVolFlowRateDes > 0.0d0) THEN
                DesAirVolFlowRateUser = WaterCoil(CoilNum)%DesAirVolFlowRate
                IF (WaterCoil(CoilNum)%WaterCoilModel == CoilModel_Cooling) THEN  ! 'Cooling'
                  CALL ReportSizingOutput('Coil:Cooling:Water',WaterCoil(CoilNum)%Name, &
                                    'Design Size Design Air Flow Rate [m3/s]',DesAirVolFlowRateDes, &
                                    'User-Specified Design Air Flow Rate [m3/s]',DesAirVolFlowRateUser)
                  IF (DisplayExtraWarnings) THEN
                    IF ((ABS(DesAirVolFlowRateDes - DesAirVolFlowRateUser)/DesAirVolFlowRateUser) > AutoVsHardSizingThreshold) THEN
                      CALL ShowMessage('SizeWaterCoil: Potential issue with equipment sizing for Coil = ' &
                                       //TRIM(WaterCoil(CoilNum)%Name))
                      CALL ShowContinueError('User-Specified Design Air Flow Rate of '// &
                                      TRIM(RoundSigDigits(DesAirVolFlowRateUser,5))// ' [m3/s]')
                      CALL ShowContinueError('differs from Design Size Design Air Flow Rate of ' // &
                                      TRIM(RoundSigDigits(DesAirVolFlowRateDes,5))// ' [m3/s]')
                      CALL ShowContinueError('This may, or may not, indicate mismatched component sizes.')
                      CALL ShowContinueError('Verify that the value entered is intended and is consistent with other components.')
                    END IF
                  ENDIF
                END IF
              END IF
            END IF

            IsAutosize = .FALSE.
            IF (WaterCoil(CoilNum)%DesInletAirTemp == AutoSize) THEN  !  'Cooling'
              IsAutosize = .TRUE.
            END IF
            IF (CurOASysNum > 0) THEN ! coil is in OA stream
              DesInletAirtempDes = FinalSysSizing(CurSysNum)%CoolOutTemp
            ELSE ! coil is in main air loop
              IF (PrimaryAirSystem(CurSysNum)%NumOACoolCoils == 0) THEN ! there is no precooling of the OA stream
                DesInletAirtempDes = FinalSysSizing(CurSysNum)%CoolMixTemp
              ELSE ! thereis precooling of the OA stream
                IF (DesVolFlow > 0.0d0) THEN
                  OutAirFrac = FinalSysSizing(CurSysNum)%DesOutAirVolFlow / DesVolFlow
                ELSE
                  OutAirFrac = 1.0d0
                END IF
                OutAirFrac = MIN(1.0d0,MAX(0.0d0,OutAirFrac))
                DesInletAirtempDes = OutAirFrac*FinalSysSizing(CurSysNum)%PrecoolTemp + &
                         (1.0-OutAirFrac)*FinalSysSizing(CurSysNum)%CoolRetTemp
              END IF
            END IF
            IF (IsAutosize) THEN
              WaterCoil(CoilNum)%DesInletAirtemp = DesInletAirtempDes
              CALL ReportSizingOutput('Coil:Cooling:Water',WaterCoil(CoilNum)%Name, &
                                    'Design Size Design Inlet Air Temperature [C]',DesInletAirtempDes)
            ELSE
              IF (WaterCoil(CoilNum)%DesInletAirtemp > 0.0d0 .AND. DesInletAirtempDes > 0.0d0) THEN
                DesInletAirtempUser = WaterCoil(CoilNum)%DesInletAirtemp
                CALL ReportSizingOutput('Coil:Cooling:Water',WaterCoil(CoilNum)%Name, &
                                    'Design Size Design Inlet Air Temperature [C]',DesInletAirtempDes, &
                                    'User-Specified Design Inlet Air Temperature [C]',DesInletAirtempUser)
                IF (DisplayExtraWarnings) THEN
                  IF (ABS(DesInletAirtempDes - DesInletAirtempUser) > AutoVsHardSizingDeltaTempThreshold) THEN
                    CALL ShowMessage('SizeWaterCoil: Potential issue with equipment sizing for Coil = ' &
                                    //TRIM(WaterCoil(CoilNum)%Name))
                    CALL ShowContinueError('User-Specified Design Inlet Air Temperature of '// &
                                     TRIM(RoundSigDigits(DesInletAirtempUser,2))// ' [C]')
                    CALL ShowContinueError('differs from Design Size Design Inlet Air Temperature of ' // &
                                      TRIM(RoundSigDigits(DesInletAirtempDes,2))// ' [C]')
                    CALL ShowContinueError('This may, or may not, indicate mismatched component sizes.')
                    CALL ShowContinueError('Verify that the value entered is intended and is consistent with other components.')
                  END IF
                ENDIF
              END IF
            END IF

            IsAutosize = .FALSE.
            IF (WaterCoil(CoilNum)%DesInletWaterTemp == AutoSize) THEN  !  'Cooling'
              IsAutosize = .TRUE.
            END IF
            DesInletWaterTempDes = PlantSizData(PltSizCoolNum)%ExitTemp
            IF (IsAutosize) THEN
              WaterCoil(CoilNum)%DesInletwatertemp = DesInletwatertempDes
              CALL ReportSizingOutput('Coil:Cooling:Water',WaterCoil(CoilNum)%Name, &
                                    'Design Size Design Inlet Water Temperature [C]',DesInletwatertempDes)
            ELSE
              IF (WaterCoil(CoilNum)%DesInletwatertemp > 0.0d0 .AND. DesInletwatertempDes > 0.0d0) THEN
                DesInletwatertempUser = WaterCoil(CoilNum)%DesInletwatertemp
                CALL ReportSizingOutput('Coil:Cooling:Water',WaterCoil(CoilNum)%Name, &
                                    'Design Size Design Inlet Water Temperature [C]',DesInletwatertempDes, &
                                    'User-Specified Design Inlet Water Temperature [C]',DesInletwatertempUser)
                IF (DisplayExtraWarnings) THEN
                  IF (ABS(DesInletwatertempDes - DesInletwatertempUser) > AutoVsHardSizingDeltaTempThreshold) THEN
                    CALL ShowMessage('SizeWaterCoil: Potential issue with equipment sizing for Coil = ' &
                                       //TRIM(WaterCoil(CoilNum)%Name))
                    CALL ShowContinueError('User-Specified Design Inlet Water Temperature of '// &
                                     TRIM(RoundSigDigits(DesInletwatertempUser,2))// ' [C]')
                    CALL ShowContinueError('differs from Design Size Design Inlet Water Temperature of ' // &
                                      TRIM(RoundSigDigits(DesInletWatertempDes,2))// ' [C]')
                    CALL ShowContinueError('This may, or may not, indicate mismatched component sizes.')
                    CALL ShowContinueError('Verify that the value entered is intended and is consistent with other components.')
                  END IF
                ENDIF
              END IF
            END IF

            IsAutosize= .FALSE.
            IF (WaterCoil(CoilNum)%DesOutletAirTemp == AutoSize) THEN  !  'Cooling'
            IsAutosize = .TRUE.
            END IF
            IF (CurOASysNum > 0) THEN
              DesOutletAirTempDes = FinalSysSizing(CurSysNum)%PrecoolTemp
            ELSE
              DesOutletAirTempDes = FinalSysSizing(CurSysNum)%CoolSupTemp
            END IF
            If (DesOutletAirTempDes.LT.WaterCoil(CoilNum)%DesInletWaterTemp &
               .AND. WaterCoil(CoilNum)%MaxWaterVolFlowRate > 0.0d0 ) Then
              CALL ShowWarningError('SizeWaterCoil: Coil="'//TRIM(WaterCoil(CoilNum)%Name)//  &
                                  '", Cooling Coil has leaving air temperature > entering water temperature.')
              CALL ShowContinueError('    Tair,out  =  ' // TRIM(RoundSigDigits(DesOutletAirTempDes,3)))
              CALL ShowContinueError('    Twater,in = ' // TRIM(RoundSigDigits(WaterCoil(CoilNum)%DesInletWaterTemp,3)))
              DesOutletAirTempDes = WaterCoil(CoilNum)%DesInletWaterTemp + 0.5d0
              CALL ShowContinueError('....coil leaving air temperature will be reset to:')
              CALL ShowContinueError('    Tair,out = ' // TRIM(RoundSigDigits(DesOutletAirTempDes,3)))
            End If
            IF (IsAutosize) THEN
              WaterCoil(CoilNum)%DesOutletAirTemp = DesOutletAirTempDes
              CALL ReportSizingOutput('Coil:Cooling:Water',WaterCoil(CoilNum)%Name, &
                                    'Design Size Design Outlet Air Temperature [C]',DesOutletAirTempDes)
            ELSE
              IF (WaterCoil(CoilNum)%DesOutletAirTemp > 0.0d0 .AND. DesOutletAirTempDes > 0.0d0) THEN
                DesOutletAirTempUser = WaterCoil(CoilNum)%DesOutletAirTemp
                CALL ReportSizingOutput('Coil:Cooling:Water',WaterCoil(CoilNum)%Name, &
                                    'Design Size Design Outlet Air Temperature [C]',DesOutletAirTempDes, &
                                    'User-Specified Design Outlet Air Temperature [C]',DesOutletAirTempUser)
                IF (DisplayExtraWarnings) THEN
                  IF (ABS(DesOutletAirTempDes - DesOutletAirTempUser) > AutoVsHardSizingDeltaTempThreshold) THEN
                    CALL ShowMessage('SizeWaterCoil: Potential issue with equipment sizing for Coil = ' &
                                      //TRIM(WaterCoil(CoilNum)%Name))
                    CALL ShowContinueError('User-Specified Design Outlet Air Temperature of '// &
                                     TRIM(RoundSigDigits(DesOutletAirTempUser,2))// ' [C]')
                    CALL ShowContinueError('differs from Design Size Design Outlet Air Temperature of ' // &
                                      TRIM(RoundSigDigits(DesOutletAirTempDes,2))// ' [C]')
                    CALL ShowContinueError('This may, or may not, indicate mismatched component sizes.')
                    CALL ShowContinueError('Verify that the value entered is intended and is consistent with other components.')
                  END IF
                ENDIF
              END IF
            END IF

            IsAutosize= .FALSE.
            IF (WaterCoil(CoilNum)%DesInletAirHumRat == AutoSize) THEN  !  'Cooling'
              IsAutosize = .TRUE.
            END IF
            IF (CurOASysNum > 0) THEN ! coil is in OA stream
              DesInletAirHumRatDes = FinalSysSizing(CurSysNum)%CoolOutHumRat
            ELSE ! coil is in main air loop
              IF (PrimaryAirSystem(CurSysNum)%NumOACoolCoils == 0) THEN ! there is no precooling of the OA stream
                DesInletAirHumRatDes = FinalSysSizing(CurSysNum)%CoolMixHumRat
              ELSE ! there is precooling of the OA stream
                IF (DesVolFlow > 0.0d0) THEN
                  OutAirFrac = FinalSysSizing(CurSysNum)%DesOutAirVolFlow / DesVolFlow
                ELSE
                  OutAirFrac = 1.0d0
                END IF
                OutAirFrac = MIN(1.0d0,MAX(0.0d0,OutAirFrac))
                DesInletAirHumRatDes = OutAirFrac*FinalSysSizing(CurSysNum)%PrecoolHumRat + &
                         (1.0-OutAirFrac)*FinalSysSizing(CurSysNum)%CoolRetHumRat
              END IF
            END IF
            IF (IsAutosize) THEN
              WaterCoil(CoilNum)%DesInletAirHumRat = DesInletAirHumRatDes
              CALL ReportSizingOutput('Coil:Cooling:Water',WaterCoil(CoilNum)%Name, &
                                    'Design Size Design Inlet Air Humidity Ratio',DesInletAirHumRatDes)
            ELSE
              IF (WaterCoil(CoilNum)%DesInletAirHumRat > 0.0d0 .AND. DesInletAirHumRatDes > 0.0d0) THEN
                DesInletAirHumRatUser = WaterCoil(CoilNum)%DesInletAirHumRat
                CALL ReportSizingOutput('Coil:Cooling:Water',WaterCoil(CoilNum)%Name, &
                                    'Design Size Design Inlet Air Humidity Ratio',DesInletAirHumRatDes, &
                                    'User-Specified Design Inlet Air Humidity Ratio',DesInletAirHumRatUser)
                IF (DisplayExtraWarnings) THEN
                  IF ((ABS(DesInletAirHumRatDes - DesInletAirHumRatUser)/DesInletAirHumRatUser) > AutoVsHardSizingThreshold) THEN
                    CALL ShowMessage('SizeWaterCoil: Potential issue with equipment sizing for Coil = ' &
                                           //TRIM(WaterCoil(CoilNum)%Name))
                    CALL ShowContinueError('User-Specified Design Inlet Air Humidity Ratio of '// &
                                     TRIM(RoundSigDigits(DesInletAirHumRatUser,6)))
                    CALL ShowContinueError('differs from Design Size Design Inlet Air Humidity Ratio of ' // &
                                      TRIM(RoundSigDigits(DesInletAirHumRatDes,6)))
                    CALL ShowContinueError('This may, or may not, indicate mismatched component sizes.')
                    CALL ShowContinueError('Verify that the value entered is intended and is consistent with other components.')
                  END IF
                ENDIF
              END IF
            END IF

            IsAutosize = .FALSE.
            IF (WaterCoil(CoilNum)%DesOutletAirHumRat == AutoSize) THEN  !  'Cooling'
              IsAutosize = .TRUE.
            END IF
            IF (CurOASysNum > 0) THEN
              DesOutletAirHumRatDes = FinalSysSizing(CurSysNum)%PrecoolHumRat
            ELSE
              DesOutletAirHumRatDes = FinalSysSizing(CurSysNum)%CoolSupHumRat
            END IF
            If (DesOutletAirHumRatDes .GT. WaterCoil(CoilNum)%DesInletAirHumRat &
              .AND. WaterCoil(CoilNum)%MaxWaterVolFlowRate > 0.0d0 ) Then
              CALL ShowWarningError('SizeWaterCoil: Coil="'//TRIM(WaterCoil(CoilNum)%Name)//  &
                                  '", Cooling Coil has leaving humidity ratio > entering humidity ratio.')
              CALL ShowContinueError('    Wair,in =  ' // TRIM(RoundSigDigits(WaterCoil(CoilNum)%DesInletAirHumRat,6)))
              CALL ShowContinueError('    Wair,out = ' // TRIM(RoundSigDigits(DesOutletAirHumRatDes,6)))
              IF (WaterCoil(CoilNum)%DesInletAirHumRat > .016d0) THEN
                DesOutletAirHumRatDes = 0.5d0*WaterCoil(CoilNum)%DesInletAirHumRat
              ELSE
                DesOutletAirHumRatDes = WaterCoil(CoilNum)%DesInletAirHumRat
              END IF
              CALL ShowContinueError('....coil leaving humidity ratio will be reset to:')
              CALL ShowContinueError('    Wair,out = ' // TRIM(RoundSigDigits(DesOutletAirHumRatDes,6)))
            End If

            ! check for dry coil and reset outlet humrat if needed
            DesSatEnthAtWaterInTemp =PsyHFnTdbW(WaterCoil(CoilNum)%DesInletWaterTemp, &
                                             PsyWFnTdpPb(WaterCoil(CoilNum)%DesInletWaterTemp,StdBaroPress))
            DesHumRatAtWaterInTemp = PsyWFnTdbH(WaterCoil(CoilNum)%DesInletWaterTemp,DesSatEnthAtWaterInTemp,'InitWaterCoil')
            IF (DesOutletAirHumRatDes < WaterCoil(CoilNum)%DesInletAirHumRat .AND. &
              DesHumRatAtWaterInTemp > WaterCoil(CoilNum)%DesInletAirHumRat .AND. &
              WaterCoil(CoilNum)%MaxWaterVolFlowRate > 0.0d0 ) THEN
              IF (WaterCoil(CoilNum)%DesInletAirHumRat > DesOutletAirHumRatDes) THEN
                CALL ShowWarningError('SizeWaterCoil: Coil="'//TRIM(WaterCoil(CoilNum)%Name)//  &
                                  '", Cooling Coil is dry and has air leaving humidity ratio < entering humidity ratio.')
                CALL ShowContinueError('    Wair,in =  ' // TRIM(RoundSigDigits(WaterCoil(CoilNum)%DesInletAirHumRat,6)))
                CALL ShowContinueError('    Wair,out = ' // TRIM(RoundSigDigits(DesOutletAirHumRatDes,6)))
                DesOutletAirHumRatDes = WaterCoil(CoilNum)%DesInletAirHumRat
                CALL ShowContinueError('....coil leaving humidity ratio will be reset to:')
                CALL ShowContinueError('    Wair,out = ' // TRIM(RoundSigDigits(DesOutletAirHumRatDes,6)))
              END IF
            END IF
            IF (IsAutosize) THEN
              WaterCoil(CoilNum)%DesOutletAirHumRat = DesOutletAirHumRatDes
              CALL ReportSizingOutput('Coil:Cooling:Water',WaterCoil(CoilNum)%Name, &
                                    'Design Size Design Outlet Air Humidity Ratio',DesOutletAirHumRatDes)
            ELSE
              IF (WaterCoil(CoilNum)%DesOutletAirHumRat > 0.0d0 .AND. DesOutletAirHumRatDes > 0.0d0) THEN
                DesOutletAirHumRatUser = WaterCoil(CoilNum)%DesOutletAirHumRat
                CALL ReportSizingOutput('Coil:Cooling:Water',WaterCoil(CoilNum)%Name, &
                                    'Design Size Design Outlet Air Humidity Ratio',DesOutletAirHumRatDes, &
                                    'User-Specified Design Outlet Air Humidity Ratio',DesOutletAirHumRatUser)
                IF (DisplayExtraWarnings) THEN
                  IF ((ABS(DesOutletAirHumRatDes - DesOutletAirHumRatUser)/DesOutletAirHumRatUser) &
                                   > AutoVsHardSizingThreshold) THEN
                    CALL ShowMessage('SizeWaterCoil: Potential issue with equipment sizing for Coil = ' &
                                       //TRIM(WaterCoil(CoilNum)%Name))
                    CALL ShowContinueError('User-Specified Design Outlet Air Humidity Ratio of '// &
                                     TRIM(RoundSigDigits(DesOutletAirHumRatUser,6)))
                    CALL ShowContinueError('differs from Design Size Design Outlet Air Humidity Ratio of ' // &
                                      TRIM(RoundSigDigits(DesOutletAirHumRatDes,6)))
                    CALL ShowContinueError('This may, or may not, indicate mismatched component sizes.')
                    CALL ShowContinueError('Verify that the value entered is intended and is consistent with other components.')
                  END IF
                ENDIF
              END IF
            END IF
          END IF ! End of simple coooling coil autosizable fields

          IsAutosize = .FALSE.
          IF (WaterCoil(CoilNum)%NumofTubesperRow == AutoSize .AND. WaterCoil(CoilNum)%WaterCoilModel == CoilModel_Detailed) THEN
            IsAutosize = .TRUE.
          END IF
          NumofTubesperRowDes = INT(13750.d0 * WaterCoil(CoilNum)%MaxWaterVolFlowRate) + 1
          NumofTubesperRowDes = MAX(3.d0, NumofTubesperRowDes)
          IF (IsAutosize) THEN
            WaterCoil(CoilNum)%NumofTubesperRow = NumofTubesperRowDes
            CALL ReportSizingOutput('Coil:Cooling:Water:DetailedGeometry',WaterCoil(CoilNum)%Name, &
                                    'Design Size Number of Tubes per Row',NumofTubesperRowDes)
          ELSE
            IF (WaterCoil(CoilNum)%NumofTubesperRow > 0.0d0 .AND. NumofTubesperRowDes > 0.0d0) THEN
              NumofTubesperRowUser = WaterCoil(CoilNum)%NumofTubesperRow
              CALL ReportSizingOutput('Coil:Cooling:Water:DetailedGeometry',WaterCoil(CoilNum)%Name, &
                                    'Design Size Number of Tubes per Row',NumofTubesperRowDes, &
                                    'User-Specified Number of Tubes per Row',NumofTubesperRowUser)
              IF (DisplayExtraWarnings) THEN
                IF ((ABS(NumofTubesperRowDes - NumofTubesperRowUser)/NumofTubesperRowUser) > AutoVsHardSizingThreshold) THEN
                  CALL ShowMessage('SizeWaterCoil: Potential issue with equipment sizing for Coil = ' &
                                       //TRIM(WaterCoil(CoilNum)%Name))
                  CALL ShowContinueError('User-Specified Number of Tubes per Row of '// &
                                   TRIM(RoundSigDigits(NumofTubesperRowUser,1)))
                  CALL ShowContinueError('differs from Design Size Number of Tubes per Row of ' // &
                                      TRIM(RoundSigDigits(NumofTubesperRowDes,1)))
                  CALL ShowContinueError('This may, or may not, indicate mismatched component sizes.')
                  CALL ShowContinueError('Verify that the value entered is intended and is consistent with other components.')
                END IF
              ENDIF
            END IF
          END IF

          IsAutosize = .FALSE.
          IF (WaterCoil(CoilNum)%FinDiam == AutoSize .AND. WaterCoil(CoilNum)%WaterCoilModel == CoilModel_Detailed) THEN
             IsAutosize = .TRUE.
          END IF
          IF (CurOASysNum > 0) THEN
            WaterCoil(CoilNum)%InletAirMassFlowRate = RhoAirStd*FinalSysSizing(CurSysNum)%DesOutAirVolFlow
          ELSE
            SELECT CASE(CurDuctType)
              CASE(Main)
                WaterCoil(CoilNum)%InletAirMassFlowRate = RhoAirStd*FinalSysSizing(CurSysNum)%DesMainVolFlow
              CASE(Cooling)
                WaterCoil(CoilNum)%InletAirMassFlowRate = RhoAirStd*FinalSysSizing(CurSysNum)%DesCoolVolFlow
              CASE(Heating)
                WaterCoil(CoilNum)%InletAirMassFlowRate = RhoAirStd*FinalSysSizing(CurSysNum)%DesHeatVolFlow
              CASE(Other)
                WaterCoil(CoilNum)%InletAirMassFlowRate = RhoAirStd*FinalSysSizing(CurSysNum)%DesMainVolFlow
              CASE DEFAULT
                WaterCoil(CoilNum)%InletAirMassFlowRate = RhoAirStd*FinalSysSizing(CurSysNum)%DesMainVolFlow
            END SELECT
          END IF
          FinDiamDes = 0.335d0 * WaterCoil(CoilNum)%InletAirMassFlowRate
          IF (IsAutosize) THEN
              WaterCoil(CoilNum)%FinDiam = FinDiamDes
            CALL ReportSizingOutput('Coil:Cooling:Water:DetailedGeometry',WaterCoil(CoilNum)%Name, &
                                  'Design Size Fin Diameter [m]',FinDiamDes)
          ELSE
            IF (WaterCoil(CoilNum)%FinDiam > 0.0d0 .AND. FinDiamDes > 0.0d0) THEN
                FinDiamUser = WaterCoil(CoilNum)%FinDiam
              CALL ReportSizingOutput('Coil:Cooling:Water:DetailedGeometry',WaterCoil(CoilNum)%Name, &
                                    'Design Size Fin Diameter [m]',FinDiamDes, &
                                    'User-Specified Fin Diameter [m]',FinDiamUser)
              IF (DisplayExtraWarnings) THEN
                IF ((ABS(FinDiamDes - FinDiamUser)/FinDiamUser) > AutoVsHardSizingThreshold) THEN
                  CALL ShowMessage('SizeWaterCoil: Potential issue with equipment sizing for Coil = ' &
                                        //TRIM(WaterCoil(CoilNum)%Name))
                  CALL ShowContinueError('User-Specified Fin Diameter of '// &
                                      TRIM(RoundSigDigits(FinDiamUser,6))// ' [m]')
                  CALL ShowContinueError('differs from Design Size Fin Diameter of ' // &
                                      TRIM(RoundSigDigits(FinDiamUser,6))// ' [m]')
                  CALL ShowContinueError('This may, or may not, indicate mismatched component sizes.')
                  CALL ShowContinueError('Verify that the value entered is intended and is consistent with other components.')
                END IF
              ENDIF
            END IF
          END IF

          IsAutosize = .FALSE.
          IF (WaterCoil(CoilNum)%MinAirFlowArea == AutoSize .AND. WaterCoil(CoilNum)%WaterCoilModel == CoilModel_Detailed) THEN
              IsAutosize = .TRUE.
          END IF
          IF (CurOASysNum > 0) THEN
            WaterCoil(CoilNum)%InletAirMassFlowRate = RhoAirStd*FinalSysSizing(CurSysNum)%DesOutAirVolFlow
          ELSE
            SELECT CASE(CurDuctType)
              CASE(Main)
                WaterCoil(CoilNum)%InletAirMassFlowRate = RhoAirStd*FinalSysSizing(CurSysNum)%DesMainVolFlow
              CASE(Cooling)
                WaterCoil(CoilNum)%InletAirMassFlowRate = RhoAirStd*FinalSysSizing(CurSysNum)%DesCoolVolFlow
              CASE(Heating)
                WaterCoil(CoilNum)%InletAirMassFlowRate = RhoAirStd*FinalSysSizing(CurSysNum)%DesHeatVolFlow
              CASE(Other)
                WaterCoil(CoilNum)%InletAirMassFlowRate = RhoAirStd*FinalSysSizing(CurSysNum)%DesMainVolFlow
              CASE DEFAULT
                WaterCoil(CoilNum)%InletAirMassFlowRate = RhoAirStd*FinalSysSizing(CurSysNum)%DesMainVolFlow
            END SELECT
          END IF
          MinAirFlowAreaDes = 0.44d0 * WaterCoil(CoilNum)%InletAirMassFlowRate
          IF (IsAutosize) THEN
            WaterCoil(CoilNum)%MinAirFlowArea = MinAirFlowAreaDes
            CALL ReportSizingOutput('Coil:Cooling:Water:DetailedGeometry',WaterCoil(CoilNum)%Name, &
                                  'Design Size Minimum Airflow Area [m2]',MinAirFlowAreaDes)
          ELSE
            IF (WaterCoil(CoilNum)%MinAirFlowArea > 0.0d0 .AND. MinAirFlowAreaDes > 0.0d0) THEN
              MinAirFlowAreaUser = WaterCoil(CoilNum)%MinAirFlowArea
              CALL ReportSizingOutput('Coil:Cooling:Water:DetailedGeometry',WaterCoil(CoilNum)%Name, &
                                    'Design Size Minimum Airflow Area [m2]',MinAirFlowAreaDes, &
                                    'User-Specified Minimum Airflow Area [m2]',MinAirFlowAreaUser)
              IF (DisplayExtraWarnings) THEN
                IF ((ABS(MinAirFlowAreaDes - MinAirFlowAreaUser)/MinAirFlowAreaUser) > AutoVsHardSizingThreshold) THEN
                  CALL ShowMessage('SizeWaterCoil: Potential issue with equipment sizing for Coil = ' &
                                      //TRIM(WaterCoil(CoilNum)%Name))
                  CALL ShowContinueError('User-Specified Minimum Airflow Area of '// &
                                     TRIM(RoundSigDigits(MinAirFlowAreaUser,6))// ' [m2]')
                  CALL ShowContinueError('differs from Design Size Minimum Airflow Area of ' // &
                                     TRIM(RoundSigDigits(MinAirFlowAreaDes,6))// ' [m2]')
                  CALL ShowContinueError('This may, or may not, indicate mismatched component sizes.')
                  CALL ShowContinueError('Verify that the value entered is intended and is consistent with other components.')
                END IF
              ENDIF
            END IF
          END IF

          IF(MinAirFlowAreaDes .LE. 0.0D0 .AND. WaterCoil(CoilNum)%WaterCoilModel == CoilModel_Detailed)THEN
            CALL ShowSevereError('Coil:Cooling:Water:DetailedGeometry: "'//TRIM(WaterCoil(CoilNum)%Name)//'"')
            CALL ShowContinueError('Coil Minimum Airflow Area must be greater than 0. Coil area = '// &
                                 TRIM(TrimSigDigits(MinAirFlowAreaDes,6)))
            ErrorsFound = .TRUE.
          END IF

          IsAutosize = .FALSE.
          IF (WaterCoil(CoilNum)%FinSurfArea == AutoSize .AND. WaterCoil(CoilNum)%WaterCoilModel == CoilModel_Detailed) THEN
              IsAutosize = .TRUE.
          END IF
          IF (CurOASysNum > 0) THEN
            WaterCoil(CoilNum)%InletAirMassFlowRate = RhoAirStd*FinalSysSizing(CurSysNum)%DesOutAirVolFlow
          ELSE
            SELECT CASE(CurDuctType)
              CASE(Main)
                WaterCoil(CoilNum)%InletAirMassFlowRate = RhoAirStd*FinalSysSizing(CurSysNum)%DesMainVolFlow
              CASE(Cooling)
                WaterCoil(CoilNum)%InletAirMassFlowRate = RhoAirStd*FinalSysSizing(CurSysNum)%DesCoolVolFlow
              CASE(Heating)
                WaterCoil(CoilNum)%InletAirMassFlowRate = RhoAirStd*FinalSysSizing(CurSysNum)%DesHeatVolFlow
              CASE(Other)
                WaterCoil(CoilNum)%InletAirMassFlowRate = RhoAirStd*FinalSysSizing(CurSysNum)%DesMainVolFlow
              CASE DEFAULT
                WaterCoil(CoilNum)%InletAirMassFlowRate = RhoAirStd*FinalSysSizing(CurSysNum)%DesMainVolFlow
            END SELECT
          END IF
          FinSurfAreaDes = 78.5d0 * WaterCoil(CoilNum)%InletAirMassFlowRate
          IF (IsAutosize) THEN
            WaterCoil(CoilNum)%FinSurfArea = FinSurfAreaDes
            CALL ReportSizingOutput('Coil:Cooling:Water:DetailedGeometry',WaterCoil(CoilNum)%Name, &
                                  'Design Size Fin Surface Area [m2]',FinSurfAreaDes)
          ELSE
            IF (WaterCoil(CoilNum)%FinSurfArea > 0.0d0 .AND. FinSurfAreaDes > 0.0d0) THEN
              FinSurfAreaUser = WaterCoil(CoilNum)%FinSurfArea
              CALL ReportSizingOutput('Coil:Cooling:Water:DetailedGeometry',WaterCoil(CoilNum)%Name, &
                                    'Design Size Fin Surface Area [m2]',FinSurfAreaDes, &
                                    'User-Specified Fin Surface Area [m2]',FinSurfAreaUser)
              IF (DisplayExtraWarnings) THEN
                IF ((ABS(FinSurfAreaDes - FinSurfAreaUser)/FinSurfAreaUser) > AutoVsHardSizingThreshold) THEN
                  CALL ShowMessage('SizeWaterCoil: Potential issue with equipment sizing for Coil = ' &
                                     //TRIM(WaterCoil(CoilNum)%Name))
                  CALL ShowContinueError('User-Specified Fin Surface Area of '// &
                                   TRIM(RoundSigDigits(FinSurfAreaUser,6))// ' [m2]')
                  CALL ShowContinueError('differs from Design Size Fin Surface Area of ' // &
                                     TRIM(RoundSigDigits(FinSurfAreaDes,6))// ' [m2]')
                  CALL ShowContinueError('This may, or may not, indicate mismatched component sizes.')
                  CALL ShowContinueError('Verify that the value entered is intended and is consistent with other components.')
                END IF
              ENDIF
            END IF
          END IF

          IsAutosize = .FALSE.
          IF (WaterCoil(CoilNum)%TotTubeInsideArea == AutoSize .AND. WaterCoil(CoilNum)%WaterCoilModel == CoilModel_Detailed) THEN
            IsAutosize = .TRUE.
          END IF
          TotTubeInsideAreaDes = 4.4d0 * WaterCoil(CoilNum)%TubeInsideDiam * &
                                 WaterCoil(CoilNum)%NumofTubeRows * WaterCoil(CoilNum)%NumofTubesperRow
          IF (IsAutosize) THEN
            WaterCoil(CoilNum)%TotTubeInsideArea = TotTubeInsideAreaDes
            CALL ReportSizingOutput('Coil:Cooling:Water:DetailedGeometry',WaterCoil(CoilNum)%Name, &
                                  'Design Size Total Tube Inside Area [m2]',TotTubeInsideAreaDes)
          ELSE
            IF (WaterCoil(CoilNum)%TotTubeInsideArea > 0.0d0 .AND. TotTubeInsideAreaDes > 0.0d0) THEN
                TotTubeInsideAreaUser = WaterCoil(CoilNum)%TotTubeInsideArea
              CALL ReportSizingOutput('Coil:Cooling:Water:DetailedGeometry',WaterCoil(CoilNum)%Name, &
                                    'Design Size Total Tube Inside Area [m2]',TotTubeInsideAreaDes, &
                                    'User-Specified Total Tube Inside Area [m2]',TotTubeInsideAreaUser)
              IF (DisplayExtraWarnings) THEN
              IF ((ABS(TotTubeInsideAreaDes - TotTubeInsideAreaUser)/TotTubeInsideAreaUser) > AutoVsHardSizingThreshold) THEN
                CALL ShowMessage('SizeWaterCoil: Potential issue with equipment sizing for Coil = ' &
                                  //TRIM(WaterCoil(CoilNum)%Name))
                CALL ShowContinueError('User-Specified Total Tube Inside Area of '// &
                                 TRIM(RoundSigDigits(TotTubeInsideAreaUser,6))// ' [m2]')
                CALL ShowContinueError('differs from Design Size Total Tube Inside Area of ' // &
                                   TRIM(RoundSigDigits(TotTubeInsideAreaDes,6))// ' [m2]')
                CALL ShowContinueError('This may, or may not, indicate mismatched component sizes.')
                CALL ShowContinueError('Verify that the value entered is intended and is consistent with other components.')
              END IF
              ENDIF
            END IF
          END IF

          IsAutosize = .FALSE.
          IF (WaterCoil(CoilNum)%TubeOutsideSurfArea == AutoSize .AND. WaterCoil(CoilNum)%WaterCoilModel == CoilModel_Detailed) THEN
              IsAutosize = .TRUE.
          END IF
          TubeOutsideSurfAreaDes = 4.1d0 * WaterCoil(CoilNum)%TubeOutsideDiam * &
                                   WaterCoil(CoilNum)%NumofTubeRows * WaterCoil(CoilNum)%NumofTubesperRow
          IF (IsAutosize) THEN
            WaterCoil(CoilNum)%TubeOutsideSurfArea = TubeOutsideSurfAreaDes
            CALL ReportSizingOutput('Coil:Cooling:Water:DetailedGeometry',WaterCoil(CoilNum)%Name, &
                                  'Design Size Tube Outside Surface Area [m2]',TubeOutsideSurfAreaDes)
          ELSE
            IF (WaterCoil(CoilNum)%TubeOutsideSurfArea > 0.0d0 .AND. TubeOutsideSurfAreaDes > 0.0d0) THEN
                TubeOutsideSurfAreaUser = WaterCoil(CoilNum)%TubeOutsideSurfArea
              CALL ReportSizingOutput('Coil:Cooling:Water:DetailedGeometry',WaterCoil(CoilNum)%Name, &
                                    'Design Size Tube Outside Surface Area [m2]',TubeOutsideSurfAreaDes, &
                                    'User-Specified Tube Outside Surface Area [m2]',TubeOutsideSurfAreaUser)
              IF (DisplayExtraWarnings) THEN
                IF ((ABS(TubeOutsideSurfAreaDes - TubeOutsideSurfAreaUser)/TubeOutsideSurfAreaUser) &
                          > AutoVsHardSizingThreshold) THEN
                  CALL ShowMessage('SizeWaterCoil: Potential issue with equipment sizing for Coil = ' &
                                        //TRIM(WaterCoil(CoilNum)%Name))
                  CALL ShowContinueError('User-Specified Tube Outside Surface Area of '// &
                                   TRIM(RoundSigDigits(TubeOutsideSurfAreaUser,6))// ' [m2]')
                  CALL ShowContinueError('differs from Design Size Tube Outside Surface Area of ' // &
                                     TRIM(RoundSigDigits(TubeOutsideSurfAreaDes,6))// ' [m2]')
                  CALL ShowContinueError('This may, or may not, indicate mismatched component sizes.')
                  CALL ShowContinueError('Verify that the value entered is intended and is consistent with other components.')
                END IF
              ENDIF
            END IF
          END IF
          IF (IsAutosize) THEN
            IF ( (WaterCoil(CoilNum)%FinSurfArea + WaterCoil(CoilNum)%TubeOutsideSurfArea) .LE. 0.0d0 &
                .AND. WaterCoil(CoilNum)%WaterCoilModel == CoilModel_Detailed) THEN
              CALL ShowSevereError('Coil:Cooling:Water:DetailedGeometry: "'//TRIM(WaterCoil(CoilNum)%Name)//'"')
              CALL ShowContinueError('Coil Fin Surface Area plus Coil Tube Outside Surface Area must be greater than 0.'// &
                        ' Total surface area = '// &
                      TRIM(TrimSigDigits((WaterCoil(CoilNum)%FinSurfArea + WaterCoil(CoilNum)%TubeOutsideSurfArea),6)))
              ErrorsFound = .TRUE.
            END IF
          END IF
          IsAutosize = .FALSE.
          IF (WaterCoil(CoilNum)%CoilDepth == AutoSize .AND. WaterCoil(CoilNum)%WaterCoilModel == CoilModel_Detailed) THEN
              IsAutosize = .TRUE.
          END IF
          CoilDepthDes = WaterCoil(CoilNum)%TubeDepthSpacing * WaterCoil(CoilNum)%NumofTubeRows
          IF (IsAutosize) THEN
              WaterCoil(CoilNum)%CoilDepth = CoilDepthDes
            CALL ReportSizingOutput('Coil:Cooling:Water:DetailedGeometry',WaterCoil(CoilNum)%Name, &
                                  'Design Size Coil Depth [m]',CoilDepthDes)
          ELSE
            IF (WaterCoil(CoilNum)%CoilDepth > 0.0d0 .AND. CoilDepthDes > 0.0d0) THEN
                CoilDepthUser = WaterCoil(CoilNum)%CoilDepth
              CALL ReportSizingOutput('Coil:Cooling:Water:DetailedGeometry',WaterCoil(CoilNum)%Name, &
                                    'Design Size Coil Depth [m]',CoilDepthDes, &
                                    'User-Specified Coil Depth [m]',CoilDepthUser)
              IF (DisplayExtraWarnings) THEN
                IF ((ABS(CoilDepthDes - CoilDepthUser)/CoilDepthUser) > AutoVsHardSizingThreshold) THEN
                  CALL ShowMessage('SizeWaterCoil: Potential issue with equipment sizing for Coil = '  &
                               //TRIM(WaterCoil(CoilNum)%Name))
                  CALL ShowContinueError('User-Specified Coil Depth of '// &
                                      TRIM(RoundSigDigits(CoilDepthUser,6))// ' [m]')
                  CALL ShowContinueError('differs from Design Size Coil Depth of ' // &
                                      TRIM(RoundSigDigits(CoilDepthDes,6))// ' [m]')
                  CALL ShowContinueError('This may, or may not, indicate mismatched component sizes.')
                  CALL ShowContinueError('Verify that the value entered is intended and is consistent with other components.')
                END IF
              ENDIF
            END IF
          END IF
        END IF

      ELSE IF (CurZoneEqNum > 0) THEN ! end system coil IF; start zone coil ELSE - IF
        IF (WaterCoil(CoilNum)%RequestingAutosize) THEN
          IsAutosizeReq = .TRUE.
        END IF
        IF (.NOT. IsAutosizeReq .AND. .NOT. SysSizingRunDone) THEN ! Simulation continue
          IF (WaterCoil(CoilNum)%WaterCoilModel == CoilModel_Cooling) THEN
            IF (WaterCoil(CoilNum)%MaxWaterVolFlowRate > 0.0d0) THEN
              CALL ReportSizingOutput('Coil:Cooling:Water',WaterCoil(CoilNum)%Name, &
                                    'User-Specified Design Water Flow Rate [m3/s]',WaterCoil(CoilNum)%MaxWaterVolFlowRate)
            END IF
            IF (WaterCoil(CoilNum)%DesAirVolFlowRate > 0.0d0) THEN
                CALL ReportSizingOutput('Coil:Cooling:Water',WaterCoil(CoilNum)%Name, &
                                    'User-Specified Design Air Flow Rate [m3/s]',WaterCoil(CoilNum)%DesAirVolFlowRate)
            END IF
            IF (WaterCoil(CoilNum)%DesInletAirtemp > 0.0d0) THEN
              CALL ReportSizingOutput('Coil:Cooling:Water',WaterCoil(CoilNum)%Name, &
                                    'User-Specified Design Inlet Air Temperature [C]',WaterCoil(CoilNum)%DesInletAirtemp)
            END IF
            IF (WaterCoil(CoilNum)%DesInletwatertemp > 0.0d0) THEN
              CALL ReportSizingOutput('Coil:Cooling:Water',WaterCoil(CoilNum)%Name, &
                                    'User-Specified Design Inlet Water Temperature [C]',WaterCoil(CoilNum)%DesInletwatertemp)
            END IF
            IF (WaterCoil(CoilNum)%DesOutletAirTemp > 0.0d0) THEN
              CALL ReportSizingOutput('Coil:Cooling:Water',WaterCoil(CoilNum)%Name, &
                                    'User-Specified Design Outlet Air Temperature [C]',WaterCoil(CoilNum)%DesOutletAirTemp)
            END IF
            IF (WaterCoil(CoilNum)%DesInletAirHumRat > 0.0d0) THEN
              CALL ReportSizingOutput('Coil:Cooling:Water',WaterCoil(CoilNum)%Name, &
                                    'User-Specified Design Inlet Air Humidity Ratio',WaterCoil(CoilNum)%DesInletAirHumRat)
            END IF
            IF (WaterCoil(CoilNum)%DesOutletAirHumRat > 0.0d0) THEN
              CALL ReportSizingOutput('Coil:Cooling:Water',WaterCoil(CoilNum)%Name, &
                                    'User-Specified Design Outlet Air Humidity Ratio',WaterCoil(CoilNum)%DesOutletAirHumRat)
            END IF

          ELSE IF(WaterCoil(CoilNum)%WaterCoilModel == CoilModel_Detailed) THEN

            IF (WaterCoil(CoilNum)%MaxWaterVolFlowRate > 0.0d0) THEN
                CALL ReportSizingOutput('Coil:Cooling:Water:DetailedGeometry',WaterCoil(CoilNum)%Name, &
                                    'User-Specified Maximum Water Flow Rate [m3/s]',WaterCoil(CoilNum)%MaxWaterVolFlowRate)
            END IF
            IF (WaterCoil(CoilNum)%TubeOutsideSurfArea > 0.0d0) THEN
              CALL ReportSizingOutput('Coil:Cooling:Water:DetailedGeometry',WaterCoil(CoilNum)%Name, &
                                    'User-Specified Tube Outside Surface Area [m2]',WaterCoil(CoilNum)%TubeOutsideSurfArea)
            END IF
            IF (WaterCoil(CoilNum)%TotTubeInsideArea > 0.0d0) THEN
              CALL ReportSizingOutput('Coil:Cooling:Water:DetailedGeometry',WaterCoil(CoilNum)%Name, &
                                    'User-Specified Total Tube Inside Area [m2]',WaterCoil(CoilNum)%TotTubeInsideArea)
            END IF
            IF (WaterCoil(CoilNum)%FinSurfArea > 0.0d0) THEN
              CALL ReportSizingOutput('Coil:Cooling:Water:DetailedGeometry',WaterCoil(CoilNum)%Name, &
                                    'User-Specified Fin Surface Area [m2]',WaterCoil(CoilNum)%FinSurfArea)
            END IF
            IF (WaterCoil(CoilNum)%MinAirFlowArea > 0.0d0) THEN
              CALL ReportSizingOutput('Coil:Cooling:Water:DetailedGeometry',WaterCoil(CoilNum)%Name, &
                                    'User-Specified Minimum Airflow Area [m2]',WaterCoil(CoilNum)%MinAirFlowArea)
            END IF
            IF (WaterCoil(CoilNum)%CoilDepth > 0.0d0) THEN
              CALL ReportSizingOutput('Coil:Cooling:Water:DetailedGeometry',WaterCoil(CoilNum)%Name, &
                                    'User-Specified Coil Depth [m]',WaterCoil(CoilNum)%CoilDepth)
            END IF
            IF (WaterCoil(CoilNum)%FinDiam > 0.0d0) THEN
              CALL ReportSizingOutput('Coil:Cooling:Water:DetailedGeometry',WaterCoil(CoilNum)%Name, &
                                    'User-Specified Fin Diameter [m]',WaterCoil(CoilNum)%FinDiam)
            END IF
            IF (WaterCoil(CoilNum)%NumofTubesperRow > 0.0d0) THEN
              NumofTubesperRowUser = WaterCoil(CoilNum)%NumofTubesperRow
              CALL ReportSizingOutput('Coil:Cooling:Water:DetailedGeometry',WaterCoil(CoilNum)%Name, &
                                    'User-Specified Number of Tubes per Row',NumofTubesperRowUser)
            END IF
          END IF
        ELSE ! At least one field is autosize
          If(WaterCoil(CoilNum)%WaterCoilModel == CoilModel_Detailed) Then  ! 'DETAILED FLAT FIN'
            CALL CheckZoneSizing('Coil:Cooling:Water:DetailedGeometry',WaterCoil(CoilNum)%Name)
          ElseIf(WaterCoil(CoilNum)%WaterCoilModel == CoilModel_Cooling) Then  ! 'Cooling'
            CALL CheckZoneSizing('Coil:Cooling:Water',WaterCoil(CoilNum)%Name)
          End If
        ! if the water flow rate needs autosizing, calculate the volume flow rate
          IsAutosize = .FALSE.
          IF (WaterCoil(CoilNum)%MaxWaterVolFlowRate == AutoSize) THEN
            IsAutosize = .TRUE.
          END IF
!          If (WaterCoil(CoilNum)%WaterCoilModel == CoilModel_Cooling) Then  ! 'Cooling'
!            CALL CheckZoneSizing('Coil:Cooling:Water',WaterCoil(CoilNum)%Name)
!          End If
          IF (TermUnitIU) THEN
            MaxWaterVolFlowRateDes = TermUnitSizing(CurZoneEqNum)%MaxCWVolFlow
            DesCoilLoad = TermUnitSizing(CurZoneEqNum)%DesCoolingLoad
          ELSE IF (ZoneEqFanCoil) THEN
            MaxWaterVolFlowRateDes = ZoneEqSizing(CurZoneEqNum)%MaxCWVolFlow
            DesCoilLoad = ZoneEqSizing(CurZoneEqNum)%DesCoolingLoad
          ELSE
            CoilInTemp = FinalZoneSizing(CurZoneEqNum)%DesCoolCoilInTemp
            CoilOutTemp = FinalZoneSizing(CurZoneEqNum)%CoolDesTemp
            CoilOutHumRat = FinalZoneSizing(CurZoneEqNum)%CoolDesHumRat
            CoilInHumRat = FinalZoneSizing(CurZoneEqNum)%DesCoolCoilInHumRat
            DesCoilLoad = FinalZoneSizing(CurZoneEqNum)%DesCoolMassFlow &
                            * (PsyHFnTdbW(CoilInTemp, CoilInHumRat)-PsyHFnTdbW(CoilOutTemp, CoilOutHumRat))
            IF (DesCoilLoad >= SmallLoad) THEN
              MaxWaterVolFlowRateDes = DesCoilLoad / &
                                       ( PlantSizData(PltSizCoolNum)%DeltaT * &
                                       Cp * rho )
            ELSE
              MaxWaterVolFlowRateDes = 0.0d0
            END IF
          END IF
          WaterCoil(CoilNum)%DesWaterHeatingCoilRate = DesCoilLoad
          IF (MaxWaterVolFlowRateDes == 0.0d0) THEN
            CALL ShowWarningError('The design coil load is zero for Coil:Cooling:Water ' &
                                  //TRIM(WaterCoil(CoilNum)%Name))
            CALL ShowContinueError('The autosize value for max water flow rate is zero')
          END IF
          IF (IsAutosize) THEN
            WaterCoil(CoilNum)%MaxWaterVolFlowRate = MaxWaterVolFlowRateDes
            If(WaterCoil(CoilNum)%WaterCoilModel == CoilModel_Cooling) Then  ! 'Cooling'
              CALL ReportSizingOutput('Coil:Cooling:Water',WaterCoil(CoilNum)%Name, &
                                    'Design Size Design Water Flow Rate [m3/s]',MaxWaterVolFlowRateDes)
            END IF
          ELSE
            IF (WaterCoil(CoilNum)%MaxWaterVolFlowRate > 0.0d0 .AND. MaxWaterVolFlowRateDes > 0.0d0) THEN
              MaxWaterVolFlowRateUser = WaterCoil(CoilNum)%MaxWaterVolFlowRate
              IF (WaterCoil(CoilNum)%WaterCoilModel == CoilModel_Cooling) THEN  ! 'Cooling'
                CALL ReportSizingOutput('Coil:Cooling:Water',WaterCoil(CoilNum)%Name, &
                                    'Design Size Design Water Flow Rate [m3/s]',MaxWaterVolFlowRateDes, &
                                    'User-Specified Design Water Flow Rate [m3/s]',MaxWaterVolFlowRateUser)
                IF (DisplayExtraWarnings) THEN
                  IF ((ABS(MaxWaterVolFlowRateDes - MaxWaterVolFlowRateUser)/MaxWaterVolFlowRateUser) &
                                     > AutoVsHardSizingThreshold) THEN
                    CALL ShowMessage('SizeWaterCoil: Potential issue with equipment sizing for Coil = ' &
                                     //TRIM(WaterCoil(CoilNum)%Name))
                    CALL ShowContinueError('User-Specified Design Water Flow Rate of '// &
                                      TRIM(RoundSigDigits(MaxWaterVolFlowRateUser,5))// ' [m3/s]')
                    CALL ShowContinueError('differs from Design Size Design Water Flow Rate of ' // &
                                      TRIM(RoundSigDigits(MaxWaterVolFlowRateDes,5))// ' [m3/s]')
                    CALL ShowContinueError('This may, or may not, indicate mismatched component sizes.')
                    CALL ShowContinueError('Verify that the value entered is intended and is consistent with other components.')
                  END IF
                ENDIF
              END IF
            END IF
          END IF

!@@@@@@@@@@ INSERTING BELOW :Autosizing for Coil Water Cooling

        ! if the design input coil design air vol flow rate input needs autosizing, calculate it
        ! Avoid false calculation of temperatures and humidity ratios
          IF (WaterCoil(CoilNum)%WaterCoilModel == CoilModel_Cooling) THEN
            IsAutosize = .FALSE.
            IF (WaterCoil(CoilNum)%DesAirVolFlowRate == AutoSize) THEN
              IsAutosize = .TRUE.
            END IF
!            CALL CheckZoneSizing('Coil:Cooling:Water',WaterCoil(CoilNum)%Name)
            IF (TermUnitIU) THEN
              DesAirVolFlowRateDes = TermUnitSizing(CurZoneEqNum)%AirVolFlow
            ELSE IF (ZoneEqFanCoil) THEN
              DesAirVolFlowRateDes = ZoneEqSizing(CurZoneEqNum)%AirVolFlow
            ELSE
              DesAirVolFlowRateDes = MAX(FinalZoneSizing(CurZoneEqNum)%DesCoolMassFlow, &
                                   FinalZoneSizing(CurZoneEqNum)%DesHeatMassFlow) / RhoAirStd
            END IF
            IF (DesAirVolFlowRateDes <= 0.0d0) THEN
              CALL ShowWarningError('The design air volume flow rate is zero for Coil:Cooling:Water ' &
                                  //TRIM(WaterCoil(CoilNum)%Name))
              CALL ShowContinueError('The autosize value for max air volume flow rate is zero')
            END IF
            IF (IsAutosize) THEN
              WaterCoil(CoilNum)%DesAirVolFlowRate = DesAirVolFlowRateDes
              CALL ReportSizingOutput('Coil:Cooling:Water',WaterCoil(CoilNum)%Name, &
                                    'Design Size Design Air Flow Rate [m3/s]',DesAirVolFlowRateDes)
            ELSE
              IF (WaterCoil(CoilNum)%DesAirVolFlowRate > 0.0d0 .AND. DesAirVolFlowRateDes > 0.0d0) THEN
                DesAirVolFlowRateUser = WaterCoil(CoilNum)%DesAirVolFlowRate
                CALL ReportSizingOutput('Coil:Cooling:Water',WaterCoil(CoilNum)%Name, &
                                    'Design Size Design Air Flow Rate [m3/s]',DesAirVolFlowRateDes, &
                                    'User-Specified Design Air Flow Rate [m3/s]',DesAirVolFlowRateUser)
                IF (DisplayExtraWarnings) THEN
                  IF ((ABS(DesAirVolFlowRateDes - DesAirVolFlowRateUser)/DesAirVolFlowRateUser) > AutoVsHardSizingThreshold) THEN
                    CALL ShowMessage('SizeWaterCoil: Potential issue with equipment sizing for Coil = ' &
                                           //TRIM(WaterCoil(CoilNum)%Name))
                    CALL ShowContinueError('User-Specified Design Air Flow Rate of '// &
                                      TRIM(RoundSigDigits(DesAirVolFlowRateUser,5))// ' [m3/s]')
                    CALL ShowContinueError('differs from Design Size Design Air Flow Rate of ' // &
                                      TRIM(RoundSigDigits(DesAirVolFlowRateDes,5))// ' [ m3/s]')
                    CALL ShowContinueError('This may, or may not, indicate mismatched component sizes.')
                    CALL ShowContinueError('Verify that the value entered is intended and is consistent with other components.')
                  END IF
                ENDIF
              END IF
            END IF

            IsAutosize = .FALSE.
            IF (WaterCoil(CoilNum)%DesInletAirTemp == AutoSize) Then    ! 'Cooling'
              IsAutosize = .TRUE.
            END IF
!          CALL CheckZoneSizing('Coil:Cooling:Water',WaterCoil(CoilNum)%Name)
            IF (TermUnitIU) THEN
              DesInletAirtempDes = FinalZoneSizing(CurZoneEqNum)%ZoneTempAtCoolPeak
            ELSE IF (ZoneEqFanCoil) THEN
              IF (FinalZoneSizing(CurZoneEqNum)%DesCoolMassFlow > 0.0d0) THEN
                FCOAFrac = MIN(RhoAirStd*ZoneEqSizing(CurZoneEqNum)%OAVolFlow/FinalZoneSizing(CurZoneEqNum)%DesCoolMassFlow, 1.0d0)
              ELSE
                FCOAFrac = 0.0d0
              END IF
              DesInletAirtempDes = FCOAFrac*FinalZoneSizing(CurZoneEqNum)%OutTempAtCoolPeak + &
                           (1.0d0-FCOAFrac)*FinalZoneSizing(CurZoneEqNum)%ZoneTempAtCoolPeak
            ELSE
              DesInletAirtempDes = FinalZoneSizing(CurZoneEqNum)%DesCoolCoilInTemp
            END IF
            IF (IsAutosize) THEN
              WaterCoil(CoilNum)%DesInletAirtemp = DesInletAirtempDes
              CALL ReportSizingOutput('Coil:Cooling:Water',WaterCoil(CoilNum)%Name, &
                                    'Design Size Design Inlet Air Temperature [C]',DesInletAirtempDes)
            ELSE
              IF (WaterCoil(CoilNum)%DesInletAirtemp > 0.0d0 .AND. DesInletAirtempDes > 0.0d0) THEN
                DesInletAirtempUser = WaterCoil(CoilNum)%DesInletAirtemp
                CALL ReportSizingOutput('Coil:Cooling:Water',WaterCoil(CoilNum)%Name, &
                                    'Design Size Design Inlet Air Temperature [C]',DesInletAirtempDes, &
                                    'User-Specified Design Inlet Air Temperature [C]',DesInletAirtempUser)
                IF (DisplayExtraWarnings) THEN
                  IF (ABS(DesInletAirtempDes - DesInletAirtempUser) > AutoVsHardSizingDeltaTempThreshold) THEN
                    CALL ShowMessage('SizeWaterCoil: Potential issue with equipment sizing for Coil = ' &
                                         //TRIM(WaterCoil(CoilNum)%Name))
                    CALL ShowContinueError('User-Specified Design Inlet Air Temperature of '// &
                                     TRIM(RoundSigDigits(DesInletAirtempUser,2))// ' [C]')
                    CALL ShowContinueError('differs from Design Size Design Inlet Air Temperature of ' // &
                                      TRIM(RoundSigDigits(DesInletAirtempDes,2))// ' [C]')
                    CALL ShowContinueError('This may, or may not, indicate mismatched component sizes.')
                    CALL ShowContinueError('Verify that the value entered is intended and is consistent with other components.')
                  END IF
                ENDIF
               END IF
            END IF

            IsAutosize = .FALSE.
            IF (WaterCoil(CoilNum)%DesInletWaterTemp == AutoSize) Then    ! 'Cooling'
              IsAutosize = .TRUE.
            END IF
!          CALL CheckZoneSizing('Coil:Cooling:Water',WaterCoil(CoilNum)%Name)
            DesInletWaterTempDes = PlantSizData(PltSizCoolNum)%ExitTemp
            IF (IsAutosize) THEN
              WaterCoil(CoilNum)%DesInletwatertemp = DesInletwatertempDes
              CALL ReportSizingOutput('Coil:Cooling:Water',WaterCoil(CoilNum)%Name, &
                                    'Design Size Design Inlet Water Temperature [C]',DesInletwatertempDes)
            ELSE
              IF (WaterCoil(CoilNum)%DesInletwatertemp > 0.0d0 .AND. DesInletwatertempDes > 0.0d0) THEN
                DesInletwatertempUser = WaterCoil(CoilNum)%DesInletwatertemp
                CALL ReportSizingOutput('Coil:Cooling:Water',WaterCoil(CoilNum)%Name, &
                                    'Design Size Design Inlet Water Temperature [C]',DesInletwatertempDes, &
                                    'User-Specified Design Inlet Water Temperature [C]',DesInletwatertempUser)
                IF (DisplayExtraWarnings) THEN
                  IF (ABS(DesInletwatertempDes - DesInletwatertempUser) > AutoVsHardSizingDeltaTempThreshold) THEN
                    CALL ShowMessage('SizeWaterCoil: Potential issue with equipment sizing for Coil = ' &
                                            //TRIM(WaterCoil(CoilNum)%Name))
                    CALL ShowContinueError('User-Specified Design Inlet Water Temperature of '// &
                                     TRIM(RoundSigDigits(DesInletwatertempUser,2))// ' [C]')
                    CALL ShowContinueError('differs from Design Size Design Inlet Water Temperature of ' // &
                                      TRIM(RoundSigDigits(DesInletWatertempDes,2))// ' [C]')
                    CALL ShowContinueError('This may, or may not, indicate mismatched component sizes.')
                    CALL ShowContinueError('Verify that the value entered is intended and is consistent with other components.')
                  END IF
                ENDIF
              END IF
            END IF

            IsAutosize = .FALSE.
            IF (WaterCoil(CoilNum)%DesInletAirHumRat == AutoSize) THEN  ! 'Cooling'
              IsAutosize = .TRUE.
            END IF
!          CALL CheckZoneSizing('Coil:Cooling:Water',WaterCoil(CoilNum)%Name)
            IF (TermUnitIU) THEN
              DesInletAirHumRatDes = FinalZoneSizing(CurZoneEqNum)%ZoneHumRatAtCoolPeak
            ELSE IF (ZoneEqFanCoil) THEN
              IF (FinalZoneSizing(CurZoneEqNum)%DesCoolMassFlow > 0.0d0) THEN
                FCOAFrac = MIN(RhoAirStd*ZoneEqSizing(CurZoneEqNum)%OAVolFlow/FinalZoneSizing(CurZoneEqNum)%DesCoolMassFlow, 1.0d0)
              ELSE
                FCOAFrac = 0.0d0
              END IF
              DesInletAirHumRatDes = FCOAFrac*FinalZoneSizing(CurZoneEqNum)%OutHumRatAtCoolPeak + &
                           (1.0d0-FCOAFrac)*FinalZoneSizing(CurZoneEqNum)%ZoneHumRatAtCoolPeak
            ELSE
              DesInletAirHumRatDes = FinalZoneSizing(CurZoneEqNum)%DesCoolCoilInHumRat
            END IF
            IF (IsAutosize) THEN
              WaterCoil(CoilNum)%DesInletAirHumRat = DesInletAirHumRatDes
              CALL ReportSizingOutput('Coil:Cooling:Water',WaterCoil(CoilNum)%Name, &
                                    'Design Size Design Inlet Air Humidity Ratio',DesInletAirHumRatDes)
            ELSE
              IF (WaterCoil(CoilNum)%DesInletAirHumRat > 0.0d0 .AND. DesInletAirHumRatDes > 0.0d0) THEN
                DesInletAirHumRatUser = WaterCoil(CoilNum)%DesInletAirHumRat
                CALL ReportSizingOutput('Coil:Cooling:Water',WaterCoil(CoilNum)%Name, &
                                    'Design Size Design Inlet Air Humidity Ratio',DesInletAirHumRatDes, &
                                    'User-Specified Design Inlet Air Humidity Ratio',DesInletAirHumRatUser)
                IF (DisplayExtraWarnings) THEN
                  IF ((ABS(DesInletAirHumRatDes - DesInletAirHumRatUser)/DesInletAirHumRatUser) > AutoVsHardSizingThreshold) THEN
                    CALL ShowMessage('SizeWaterCoil: Potential issue with equipment sizing for Coil = ' &
                                          //TRIM(WaterCoil(CoilNum)%Name))
                    CALL ShowContinueError('User-Specified Design Inlet Air Humidity Ratio of '// &
                                     TRIM(RoundSigDigits(DesInletAirHumRatUser,6)))
                    CALL ShowContinueError('differs from Design Size Design Inlet Air Humidity Ratio of ' // &
                                      TRIM(RoundSigDigits(DesInletAirHumRatDes,6)))
                    CALL ShowContinueError('This may, or may not, indicate mismatched component sizes.')
                    CALL ShowContinueError('Verify that the value entered is intended and is consistent with other components.')
                  END IF
                ENDIF
              END IF
            END IF

            IsAutosize = .FALSE.
            IF (WaterCoil(CoilNum)%DesOutletAirTemp == AutoSize) Then    ! 'Cooling'
              IsAutosize = .TRUE.
            END IF
!          CALL CheckZoneSizing('Coil:Cooling:Water',WaterCoil(CoilNum)%Name)
            IF (TermUnitIU) THEN

              Cp =  GetSpecificHeatGlycol(PlantLoop(WaterCoil(CoilNum)%WaterLoopNum)%FluidName,  &
                       5.0d0,                      &
                       PlantLoop(WaterCoil(CoilNum)%WaterLoopNum)%FluidIndex, &
                       'SizeWaterCoil')

              rho = GetDensityGlycol(PlantLoop(WaterCoil(CoilNum)%WaterLoopNum)%FluidName,  &
                       InitConvTemp,                      &
                       PlantLoop(WaterCoil(CoilNum)%WaterLoopNum)%FluidIndex, &
                       'SizeWaterCoil')
              DesCoilLoad = WaterCoil(CoilNum)%MaxWaterVolFlowRate * PlantSizData(PltSizCoolNum)%DeltaT * &
                            Cp * rho
              T1Out = WaterCoil(CoilNum)%DesInletAirtemp - DesCoilLoad / &
                (RhoAirStd * PsyCpAirFnWTdb(WaterCoil(CoilNum)%DesInletAirHumRat,WaterCoil(CoilNum)%DesInletAirTemp) * &
                 WaterCoil(CoilNum)%DesAirVolFlowRate)
              T2Out = PlantSizData(PltSizCoolNum)%ExitTemp + 2.0d0
              DesOutletAirTempDes = MAX(T1Out,T2Out)
            ELSE
              DesOutletAirTempDes = FinalZoneSizing(CurZoneEqNum)%CoolDesTemp
            END IF

            If (DesOutletAirTempDes.LT.WaterCoil(CoilNum)%DesInletWaterTemp &
              .AND. WaterCoil(CoilNum)%MaxWaterVolFlowRate > 0.0d0 ) Then
              CALL ShowWarningError('SizeWaterCoil: Coil="'//TRIM(WaterCoil(CoilNum)%Name)//  &
                                  '", Cooling Coil has leaving air temperature > entering water temperature.')
              CALL ShowContinueError('    Tair,out  =  ' // TRIM(RoundSigDigits(DesOutletAirTempDes,3)))
              CALL ShowContinueError('    Twater,in = ' // TRIM(RoundSigDigits(WaterCoil(CoilNum)%DesInletWaterTemp,3)))
              DesOutletAirTempDes = WaterCoil(CoilNum)%DesInletWaterTemp + 0.5d0
              CALL ShowContinueError('....coil leaving air temperature will be reset to:')
              CALL ShowContinueError('    Tair,out = ' // TRIM(RoundSigDigits(DesOutletAirTempDes,3)))
            End If
            IF (IsAutosize) THEN
              WaterCoil(CoilNum)%DesOutletAirTemp = DesOutletAirTempDes
              CALL ReportSizingOutput('Coil:Cooling:Water',WaterCoil(CoilNum)%Name, &
                                    'Design Size Design Outlet Air Temperature [C]',DesOutletAirTempDes)
            ELSE
              IF (WaterCoil(CoilNum)%DesOutletAirTemp > 0.0d0 .AND. DesOutletAirTempDes > 0.0d0) THEN
                DesOutletAirTempUser = WaterCoil(CoilNum)%DesOutletAirTemp
                CALL ReportSizingOutput('Coil:Cooling:Water',WaterCoil(CoilNum)%Name, &
                                    'Design Size Design Outlet Air Temperature [C]',DesOutletAirTempDes, &
                                    'User-Specified Design Outlet Air Temperature [C]',DesOutletAirTempUser)
                IF (DisplayExtraWarnings) THEN
                  IF (ABS(DesOutletAirTempDes - DesOutletAirTempUser) > AutoVsHardSizingDeltaTempThreshold) THEN
                    CALL ShowMessage('SizeWaterCoil: Potential issue with equipment sizing for Coil = ' &
                                              //TRIM(WaterCoil(CoilNum)%Name))
                    CALL ShowContinueError('User-Specified Design Outlet Air Temperature of '// &
                                     TRIM(RoundSigDigits(DesOutletAirTempUser,2))// ' [C]')
                    CALL ShowContinueError('differs from Design Size Design Outlet Air Temperature of ' // &
                                      TRIM(RoundSigDigits(DesOutletAirTempDes,2))// ' [C]')
                    CALL ShowContinueError('This may, or may not, indicate mismatched component sizes.')
                    CALL ShowContinueError('Verify that the value entered is intended and is consistent with other components.')
                  END IF
                ENDIF
              END IF
            END IF

            IsAutosize = .FALSE.
            IF (WaterCoil(CoilNum)%DesOutletAirHumRat == AutoSize) Then    ! 'Cooling'
              IsAutosize = .TRUE.
            END IF
            IF (TermUnitIU) THEN
              TDpIn = PsyTdpFnWPb(WaterCoil(CoilNum)%DesInletAirHumRat,StdBaroPress)
              IF (TDpIn .LE. WaterCoil(CoilNum)%DesInletWaterTemp) THEN
                DesOutletAirHumRatDes = WaterCoil(CoilNum)%DesInletAirHumRat
              ELSE
                DesOutletAirHumRatDes = MIN(PsyWFnTdbRhPb(WaterCoil(CoilNum)%DesOutletAirTemp, &
                                      0.9d0,StdBaroPress),WaterCoil(CoilNum)%DesInletAirHumRat)
              END IF
            ELSE
              DesOutletAirHumRatDes = FinalZoneSizing(CurZoneEqNum)%CoolDesHumRat
            END IF

            If (DesOutletAirHumRatDes.GT.WaterCoil(CoilNum)%DesInletAirHumRat &
               .AND. WaterCoil(CoilNum)%MaxWaterVolFlowRate > 0.0d0 ) Then
              CALL ShowWarningError('SizeWaterCoil: Coil="'//TRIM(WaterCoil(CoilNum)%Name)//  &
                                  '", Cooling Coil has leaving humidity ratio > entering humidity ratio.')
              CALL ShowContinueError('    Wair,in =  ' // TRIM(RoundSigDigits(WaterCoil(CoilNum)%DesInletAirHumRat,6)))
              CALL ShowContinueError('    Wair,out = ' // TRIM(RoundSigDigits(DesOutletAirHumRatDes,6)))
              IF (DesInletAirHumRatDes > .016d0) THEN
                DesOutletAirHumRatDes = 0.5d0*WaterCoil(CoilNum)%DesInletAirHumRat
              ELSE
                DesOutletAirHumRatDes = WaterCoil(CoilNum)%DesInletAirHumRat
              END IF
              CALL ShowContinueError('....coil leaving humidity ratio will be reset to:')
              CALL ShowContinueError('    Wair,out = ' // TRIM(RoundSigDigits(DesOutletAirHumRatDes,6)))
            End If
            ! check for dry coil and reset outlet humrat if needed
            DesSatEnthAtWaterInTemp =PsyHFnTdbW(WaterCoil(CoilNum)%DesInletWaterTemp, &
                                             PsyWFnTdpPb(WaterCoil(CoilNum)%DesInletWaterTemp,StdBaroPress))
            DesHumRatAtWaterInTemp = PsyWFnTdbH(WaterCoil(CoilNum)%DesInletWaterTemp,DesSatEnthAtWaterInTemp,'InitWaterCoil')
            IF (DesOutletAirHumRatDes < WaterCoil(CoilNum)%DesInletAirHumRat .AND. &
              DesHumRatAtWaterInTemp > WaterCoil(CoilNum)%DesInletAirHumRat .AND. &
              WaterCoil(CoilNum)%MaxWaterVolFlowRate > 0.0d0 ) THEN
              CALL ShowWarningError('SizeWaterCoil: Coil="'//TRIM(WaterCoil(CoilNum)%Name)//  &
                                  '", Cooling Coil is dry and has air leaving humidity ratio < entering humidity ratio.')
              CALL ShowContinueError('    Wair,in =  ' // TRIM(RoundSigDigits(WaterCoil(CoilNum)%DesInletAirHumRat,6)))
              CALL ShowContinueError('    Wair,out = ' // TRIM(RoundSigDigits(DesOutletAirHumRatDes,6)))
              DesOutletAirHumRatDes = WaterCoil(CoilNum)%DesInletAirHumRat
              CALL ShowContinueError('....coil leaving humidity ratio will be reset to:')
              CALL ShowContinueError('    Wair,out = ' // TRIM(RoundSigDigits(DesOutletAirHumRatDes,6)))
            END IF
            IF (IsAutosize) THEN
              WaterCoil(CoilNum)%DesOutletAirHumRat = DesOutletAirHumRatDes
              CALL ReportSizingOutput('Coil:Cooling:Water',WaterCoil(CoilNum)%Name, &
                                    'Design Size Design Outlet Air Humidity Ratio',DesOutletAirHumRatDes)
            ELSE
              IF (WaterCoil(CoilNum)%DesOutletAirHumRat > 0.0d0 .AND. DesOutletAirHumRatDes > 0.0d0) THEN
                DesOutletAirHumRatUser = WaterCoil(CoilNum)%DesOutletAirHumRat
                CALL ReportSizingOutput('Coil:Cooling:Water',WaterCoil(CoilNum)%Name, &
                                    'Design Size Design Outlet Air Humidity Ratio',DesOutletAirHumRatDes, &
                                    'User-Specified Design Outlet Air Humidity Ratio',DesOutletAirHumRatUser)
                IF (DisplayExtraWarnings) THEN
                  IF ((ABS(DesOutletAirHumRatDes - DesOutletAirHumRatUser)/DesOutletAirHumRatUser) &
                                                     > AutoVsHardSizingThreshold) THEN
                    CALL ShowMessage('SizeWaterCoil: Potential issue with equipment sizing for Coil = ' &
                                           //TRIM(WaterCoil(CoilNum)%Name))
                    CALL ShowContinueError('User-Specified Design Outlet Air Humidity Ratio of '// &
                                     TRIM(RoundSigDigits(DesOutletAirHumRatUser,2)))
                    CALL ShowContinueError('differs from Design Size Design Outlet Air Humidity Ratio of ' // &
                                      TRIM(RoundSigDigits(DesOutletAirHumRatDes,2)))
                    CALL ShowContinueError('This may, or may not, indicate mismatched component sizes.')
                    CALL ShowContinueError('Verify that the value entered is intended and is consistent with other components.')
                  END IF
                ENDIF
              END IF
            END IF
          END IF ! End of simple cooling coil autosizable fields

          IsAutosize = .FALSE.
          IF (WaterCoil(CoilNum)%NumofTubesperRow == AutoSize .AND. WaterCoil(CoilNum)%WaterCoilModel == CoilModel_Detailed) THEN
            IsAutosize = .TRUE.
          END IF
          NumofTubesperRowDes = INT(13750.d0 * WaterCoil(CoilNum)%MaxWaterVolFlowRate) + 1
          NumofTubesperRowDes = MAX(3.d0, NumofTubesperRowDes)
          IF (IsAutosize) THEN
            WaterCoil(CoilNum)%NumofTubesperRow = NumofTubesperRowDes
            CALL ReportSizingOutput('Coil:Cooling:Water:DetailedGeometry',WaterCoil(CoilNum)%Name, &
                                  'Design Size Number of Tubes per Row',NumofTubesperRowDes)
          ELSE
            IF (WaterCoil(CoilNum)%NumofTubesperRow > 0.0d0 .AND. NumofTubesperRowDes > 0.0d0) THEN
              NumofTubesperRowUser = WaterCoil(CoilNum)%NumofTubesperRow
              CALL ReportSizingOutput('Coil:Cooling:Water:DetailedGeometry',WaterCoil(CoilNum)%Name, &
                                    'Design Size Number of Tubes per Row',NumofTubesperRowDes, &
                                    'User-Specified Number of Tubes per Row',NumofTubesperRowUser)
              IF (DisplayExtraWarnings) THEN
                IF ((ABS(NumofTubesperRowDes - NumofTubesperRowUser)/NumofTubesperRowUser) > AutoVsHardSizingThreshold) THEN
                  CALL ShowMessage('SizeWaterCoil: Potential issue with equipment sizing for Coil = ' &
                                        //TRIM(WaterCoil(CoilNum)%Name))
                  CALL ShowContinueError('User-Specified Number of Tubes per Row of '// &
                                     TRIM(RoundSigDigits(NumofTubesperRowUser,0)))
                  CALL ShowContinueError('differs from Design Size Number of Tubes per Row of ' // &
                                      TRIM(RoundSigDigits(NumofTubesperRowDes,0)))
                  CALL ShowContinueError('This may, or may not, indicate mismatched component sizes.')
                  CALL ShowContinueError('Verify that the value entered is intended and is consistent with other components.')
                END IF
              ENDIF
            END IF
          END IF

          IsAutosize = .FALSE.
          IF (WaterCoil(CoilNum)%FinDiam == AutoSize .AND. WaterCoil(CoilNum)%WaterCoilModel == CoilModel_Detailed) THEN
            IsAutosize = .TRUE.
          END IF
          IF (TermUnitIU) THEN
            WaterCoil(CoilNum)%InletAirMassFlowRate = RhoAirStd * TermUnitSizing(CurZoneEqNum)%AirVolFlow
          ELSE
            WaterCoil(CoilNum)%InletAirMassFlowRate = MAX(FinalZoneSizing(CurZoneEqNum)%DesCoolMassFlow, &
                                                      FinalZoneSizing(CurZoneEqNum)%DesHeatMassFlow)
          END IF
          FinDiamDes = 0.335d0 * WaterCoil(CoilNum)%InletAirMassFlowRate
          IF (IsAutosize) THEN
            WaterCoil(CoilNum)%FinDiam = FinDiamDes
            CALL ReportSizingOutput('Coil:Cooling:Water:DetailedGeometry',WaterCoil(CoilNum)%Name, &
                                  'Design Size Fin Diameter [m]',FinDiamDes)
          ELSE
            IF (WaterCoil(CoilNum)%FinDiam > 0.0d0 .AND. FinDiamDes > 0.0d0) THEN
              FinDiamUser = WaterCoil(CoilNum)%FinDiam
              CALL ReportSizingOutput('Coil:Cooling:Water:DetailedGeometry',WaterCoil(CoilNum)%Name, &
                                    'Design Size Fin Diameter [m]',FinDiamDes, &
                                    'User-Specified Fin Diameter [m]',FinDiamUser)
              IF (DisplayExtraWarnings) THEN
                IF ((ABS(FinDiamDes - FinDiamUser)/FinDiamUser) > AutoVsHardSizingThreshold) THEN
                  CALL ShowMessage('SizeWaterCoil: Potential issue with equipment sizing for Coil = ' &
                                       //TRIM(WaterCoil(CoilNum)%Name))
                  CALL ShowContinueError('User-Specified Fin Diameter of '// &
                                      TRIM(RoundSigDigits(FinDiamUser,6))// ' [m]')
                  CALL ShowContinueError('differs from Design Size Fin Diameter of ' // &
                                      TRIM(RoundSigDigits(FinDiamDes,6))// ' [m]')
                  CALL ShowContinueError('This may, or may not, indicate mismatched component sizes.')
                  CALL ShowContinueError('Verify that the value entered is intended and is consistent with other components.')
                END IF
              ENDIF
            END IF
          END IF

          IsAutosize = .FALSE.
          IF (WaterCoil(CoilNum)%MinAirFlowArea == AutoSize .AND. WaterCoil(CoilNum)%WaterCoilModel == CoilModel_Detailed) THEN
            IsAutosize = .TRUE.
          END IF
          IF (TermUnitIU) THEN
            WaterCoil(CoilNum)%InletAirMassFlowRate = RhoAirStd * TermUnitSizing(CurZoneEqNum)%AirVolFlow
          ELSE
           WaterCoil(CoilNum)%InletAirMassFlowRate = MAX(FinalZoneSizing(CurZoneEqNum)%DesCoolMassFlow, &
                                                      FinalZoneSizing(CurZoneEqNum)%DesHeatMassFlow)
          END IF
          MinAirFlowAreaDes = 0.44d0 * WaterCoil(CoilNum)%InletAirMassFlowRate
          IF (IsAutosize) THEN
            WaterCoil(CoilNum)%MinAirFlowArea = MinAirFlowAreaDes
            CALL ReportSizingOutput('Coil:Cooling:Water:DetailedGeometry',WaterCoil(CoilNum)%Name, &
                                  'Design Size Minimum Airflow Area [m2]',MinAirFlowAreaDes)
          ELSE
            IF (WaterCoil(CoilNum)%MinAirFlowArea > 0.0d0 .AND. MinAirFlowAreaDes > 0.0d0) THEN
              MinAirFlowAreaUser = WaterCoil(CoilNum)%MinAirFlowArea
              CALL ReportSizingOutput('Coil:Cooling:Water:DetailedGeometry',WaterCoil(CoilNum)%Name, &
                                    'Design Size Minimum Airflow Area [m2]',MinAirFlowAreaDes, &
                                    'User-Specified Minimum Airflow Area [m2]',MinAirFlowAreaUser)
              IF (DisplayExtraWarnings) THEN
                IF ((ABS(MinAirFlowAreaDes - MinAirFlowAreaUser)/MinAirFlowAreaUser) > AutoVsHardSizingThreshold) THEN
                  CALL ShowMessage('SizeWaterCoil: Potential issue with equipment sizing for Coil = ' &
                                      //TRIM(WaterCoil(CoilNum)%Name))
                  CALL ShowContinueError('User-Specified Minimum Airflow Area of '// &
                                     TRIM(RoundSigDigits(MinAirFlowAreaUser,6))// ' [m2]')
                  CALL ShowContinueError('differs from Design Size Minimum Airflow Area of ' // &
                                     TRIM(RoundSigDigits(MinAirFlowAreaDes,6))// ' [m2]')
                  CALL ShowContinueError('This may, or may not, indicate mismatched component sizes.')
                  CALL ShowContinueError('Verify that the value entered is intended and is consistent with other components.')
                END IF
              ENDIF
            END IF
          END IF

          IsAutosize = .FALSE.
          IF (WaterCoil(CoilNum)%FinSurfArea == AutoSize .AND. WaterCoil(CoilNum)%WaterCoilModel == CoilModel_Detailed) THEN
            IsAutosize = .TRUE.
          END IF
          IF (TermUnitIU) THEN
            WaterCoil(CoilNum)%InletAirMassFlowRate = RhoAirStd * TermUnitSizing(CurZoneEqNum)%AirVolFlow
          ELSE
           WaterCoil(CoilNum)%InletAirMassFlowRate = MAX(FinalZoneSizing(CurZoneEqNum)%DesCoolMassFlow, &
                                                      FinalZoneSizing(CurZoneEqNum)%DesHeatMassFlow)
          END IF
          FinSurfAreaDes = 78.5d0 * WaterCoil(CoilNum)%InletAirMassFlowRate
          IF (IsAutosize) THEN
            WaterCoil(CoilNum)%FinSurfArea = FinSurfAreaDes
            CALL ReportSizingOutput('Coil:Cooling:Water:DetailedGeometry',WaterCoil(CoilNum)%Name, &
                                  'Design Size Fin Surface Area [m2]',FinSurfAreaDes)
          ELSE
            IF (WaterCoil(CoilNum)%FinSurfArea > 0.0d0 .AND. FinSurfAreaDes > 0.0d0 ) THEN
              FinSurfAreaUser = WaterCoil(CoilNum)%FinSurfArea
              CALL ReportSizingOutput('Coil:Cooling:Water:DetailedGeometry',WaterCoil(CoilNum)%Name, &
                                    'Design Size Fin Surface Area [m2]',FinSurfAreaDes, &
                                    'User-Specified Fin Surface Area [m2]',FinSurfAreaUser)
              IF (DisplayExtraWarnings) THEN
                IF ((ABS(FinSurfAreaDes - FinSurfAreaUser)/FinSurfAreaUser) > AutoVsHardSizingThreshold) THEN
                  CALL ShowMessage('SizeWaterCoil: Potential issue with equipment sizing for Coil = ' &
                                     //TRIM(WaterCoil(CoilNum)%Name))
                  CALL ShowContinueError('User-Specified Fin Surface Area of '// &
                                     TRIM(RoundSigDigits(FinSurfAreaUser,6))// ' [m2]')
                  CALL ShowContinueError('differs from Design Size Fin Surface Area of ' // &
                                     TRIM(RoundSigDigits(FinSurfAreaDes,6))// ' [m2]')
                  CALL ShowContinueError('This may, or may not, indicate mismatched component sizes.')
                  CALL ShowContinueError('Verify that the value entered is intended and is consistent with other components.')
                END IF
              ENDIF
            END IF
          END IF

          IsAutosize = .FALSE.
          IF (WaterCoil(CoilNum)%TotTubeInsideArea == AutoSize .AND. WaterCoil(CoilNum)%WaterCoilModel == CoilModel_Detailed) THEN
            IsAutosize = .TRUE.
          END IF
          TotTubeInsideAreaDes = 4.4d0 * WaterCoil(CoilNum)%TubeInsideDiam * &
                                 WaterCoil(CoilNum)%NumofTubeRows * WaterCoil(CoilNum)%NumofTubesperRow
          IF (IsAutosize) THEN
            WaterCoil(CoilNum)%TotTubeInsideArea = TotTubeInsideAreaDes
            CALL ReportSizingOutput('Coil:Cooling:Water:DetailedGeometry',WaterCoil(CoilNum)%Name, &
                                  'Design Size Total Tube Inside Area [m2]',TotTubeInsideAreaDes)
          ELSE
            IF (WaterCoil(CoilNum)%TotTubeInsideArea > 0.0d0 .AND. TotTubeInsideAreaDes > 0.0d0) THEN
              TotTubeInsideAreaUser = WaterCoil(CoilNum)%TotTubeInsideArea
              CALL ReportSizingOutput('Coil:Cooling:Water:DetailedGeometry',WaterCoil(CoilNum)%Name, &
                                    'Design Size Total Tube Inside Area [m2]',TotTubeInsideAreaDes, &
                                    'User-Specified Total Tube Inside Area [m2]',TotTubeInsideAreaUser)
              IF (DisplayExtraWarnings) THEN
                IF ((ABS(TotTubeInsideAreaDes - TotTubeInsideAreaUser)/TotTubeInsideAreaUser) > AutoVsHardSizingThreshold) THEN
                  CALL ShowMessage('SizeWaterCoil: Potential issue with equipment sizing for Coil = ' &
                                      //TRIM(WaterCoil(CoilNum)%Name))
                  CALL ShowContinueError('User-Specified Total Tube Inside Area of '// &
                                     TRIM(RoundSigDigits(TotTubeInsideAreaUser,6))// ' [m2]')
                  CALL ShowContinueError('differs from Design Size Total Tube Inside Area of ' // &
                                     TRIM(RoundSigDigits(TotTubeInsideAreaDes,6))// ' [m2]')
                  CALL ShowContinueError('This may, or may not, indicate mismatched component sizes.')
                  CALL ShowContinueError('Verify that the value entered is intended and is consistent with other components.')
                END IF
              ENDIF
            END IF
          END IF

          IsAutosize = .FALSE.
          IF (WaterCoil(CoilNum)%TubeOutsideSurfArea == AutoSize .AND. WaterCoil(CoilNum)%WaterCoilModel == CoilModel_Detailed) THEN
            IsAutosize = .TRUE.
          END IF
            TubeOutsideSurfAreaDes = 4.1d0 * WaterCoil(CoilNum)%TubeOutsideDiam * &
                                     WaterCoil(CoilNum)%NumofTubeRows * WaterCoil(CoilNum)%NumofTubesperRow
          IF (IsAutosize) THEN
            WaterCoil(CoilNum)%TubeOutsideSurfArea = TubeOutsideSurfAreaDes
            CALL ReportSizingOutput('Coil:Cooling:Water:DetailedGeometry',WaterCoil(CoilNum)%Name, &
                                  'Design Size Tube Outside Surface Area [m2]',TubeOutsideSurfAreaDes)
          ELSE
            IF (WaterCoil(CoilNum)%TubeOutsideSurfArea > 0.0d0 .AND. TubeOutsideSurfAreaDes > 0.0d0) THEN
              TubeOutsideSurfAreaUser = WaterCoil(CoilNum)%TubeOutsideSurfArea
              CALL ReportSizingOutput('Coil:Cooling:Water:DetailedGeometry',WaterCoil(CoilNum)%Name, &
                                    'Design Size Tube Outside Surface Area [m2]',TubeOutsideSurfAreaDes, &
                                    'User-Specified Tube Outside Surface Area [m2]',TubeOutsideSurfAreaUser)
              IF (DisplayExtraWarnings) THEN
                IF ((ABS(TubeOutsideSurfAreaDes - TubeOutsideSurfAreaUser)/TubeOutsideSurfAreaUser) &
                                  > AutoVsHardSizingThreshold) THEN
                  CALL ShowMessage('SizeWaterCoil: Potential issue with equipment sizing for Coil = ' &
                                      //TRIM(WaterCoil(CoilNum)%Name))
                  CALL ShowContinueError('User-Specified Tube Outside Surface Area of '// &
                                     TRIM(RoundSigDigits(TubeOutsideSurfAreaUser,6))// ' [m2]')
                  CALL ShowContinueError('differs from Design Size Tube Outside Surface Area of ' // &
                                     TRIM(RoundSigDigits(TubeOutsideSurfAreaDes,6))// ' [m2]')
                  CALL ShowContinueError('This may, or may not, indicate mismatched component sizes.')
                  CALL ShowContinueError('Verify that the value entered is intended and is consistent with other components.')
                END IF
              ENDIF
            END IF
          END IF

          IsAutosize = .FALSE.
          IF (WaterCoil(CoilNum)%CoilDepth == AutoSize .AND. WaterCoil(CoilNum)%WaterCoilModel == CoilModel_Detailed) THEN
            IsAutosize = .TRUE.
          END IF
            CoilDepthDes = WaterCoil(CoilNum)%TubeDepthSpacing * &
                           WaterCoil(CoilNum)%NumofTubeRows
          IF (IsAutosize) THEN
            WaterCoil(CoilNum)%CoilDepth = CoilDepthDes
            CALL ReportSizingOutput('Coil:Cooling:Water:DetailedGeometry',WaterCoil(CoilNum)%Name, &
                                  'Design Size Coil Depth [m]',CoilDepthDes)
          ELSE
            IF (WaterCoil(CoilNum)%CoilDepth > 0.0d0 .AND. CoilDepthDes > 0.0d0) THEN
              CoilDepthUser = WaterCoil(CoilNum)%CoilDepth
              CALL ReportSizingOutput('Coil:Cooling:Water:DetailedGeometry',WaterCoil(CoilNum)%Name, &
                                    'Design Size Coil Depth [m]',CoilDepthDes, &
                                    'User-Specified Coil Depth [m]',CoilDepthUser)
              IF (DisplayExtraWarnings) THEN
                IF ((ABS(CoilDepthDes - CoilDepthUser)/CoilDepthUser) > AutoVsHardSizingThreshold) THEN
                  CALL ShowMessage('SizeWaterCoil: Potential issue with equipment sizing for Coil = ' &
                                    //TRIM(WaterCoil(CoilNum)%Name))
                  CALL ShowContinueError('User-Specified Coil Depth of '// &
                                      TRIM(RoundSigDigits(CoilDepthUser,6))// ' [m]')
                  CALL ShowContinueError('differs from Design Size Coil Depth of ' // &
                                      TRIM(RoundSigDigits(CoilDepthDes,6))// ' [m]')
                  CALL ShowContinueError('This may, or may not, indicate mismatched component sizes.')
                  CALL ShowContinueError('Verify that the value entered is intended and is consistent with other components.')
                END IF
              ENDIF
            END IF
          END IF
        END IF
      END IF ! end zone coil ELSE - IF
    ELSE
      ! If there is no cooling Plant Sizing object and autosizing was requested, issue fatal error message
      IF (WaterCoil(CoilNum)%RequestingAutosize) THEN
        CALL ShowSevereError('Autosizing of water coil requires a cooling loop Sizing:Plant object')
        CALL ShowContinueError('Occurs in water coil object= '//TRIM(WaterCoil(CoilNum)%Name))
        ErrorsFound = .TRUE.
      END IF
    END IF ! end of cooling Plant Sizing existence IF - ELSE
  END IF ! end cooling coil IF

  ! if this is a heating coil
  IF (WaterCoil(CoilNum)%WaterCoilType == CoilType_Heating .and.   &
      WaterCoil(CoilNum)%RequestingAutosize) THEN
    ! find the appropriate heating Plant Sizing object
    PltSizHeatNum = MyPlantSizingIndex("hot water coil", WaterCoil(CoilNum)%Name, WaterCoil(CoilNum)%WaterInletNodeNum, &
                                       WaterCoil(CoilNum)%WaterOutletNodeNum, LoopErrorsFound)
  ENDIF

  IF (WaterCoil(CoilNum)%WaterCoilType == CoilType_Heating) THEN  ! 'Heating'
    IF (PltSizHeatNum > 0) THEN
      ! if this is a central air system heating coil
      IF (CurSysNum > 0) THEN
        IF (WaterCoil(CoilNum)%RequestingAutosize) THEN
          IsAutosizeReq = .TRUE.
        END IF
        IF (.NOT. IsAutosizeReq .AND. .NOT. SysSizingRunDone) THEN ! Simulation continue
          IF (WaterCoil(CoilNum)%MaxWaterVolFlowRate > 0.0d0) THEN
            CALL ReportSizingOutput('Coil:Heating:Water',WaterCoil(CoilNum)%Name, &
                                'User-Specified Maximum Water Flow Rate [m3/s]',WaterCoil(CoilNum)%MaxWaterVolFlowRate)
          END IF
          IF (WaterCoil(CoilNum)%UACoil > 0.0d0) THEN
            CALL ReportSizingOutput('Coil:Heating:Water',WaterCoil(CoilNum)%Name, &
                                'User-Specified U-Factor Times Area Value [W/K]',WaterCoil(CoilNum)%UACoil)
          END IF
          IF (WaterCoil(CoilNum)%DesTotWaterCoilLoad > 0.0d0) THEN
            CALL ReportSizingOutput('Coil:Heating:Water',WaterCoil(CoilNum)%Name, &
                                'User-Specified Design Coil Load [W]',WaterCoil(CoilNum)%DesTotWaterCoilLoad)
          END IF
        ELSE
          CALL CheckSysSizing('Coil:Heating:Water',WaterCoil(CoilNum)%Name)

        ! if the coil water volume flow rate needs autosizing, then do it
          IsAutosize = .FALSE.
          IF ( (WaterCoil(CoilNum)%CoilPerfInpMeth == UAandFlow .AND. WaterCoil(CoilNum)%MaxWaterVolFlowRate == AutoSize) .OR. &
               (WaterCoil(CoilNum)%CoilPerfInpMeth == NomCap .AND. WaterCoil(CoilNum)%DesTotWaterCoilLoad == AutoSize) ) THEN
            IsAutosize = .TRUE.
          END IF
!          CALL CheckSysSizing('Coil:Heating:Water',WaterCoil(CoilNum)%Name)
          ! set the duct flow rate
          IF (CurOASysNum > 0) THEN
            DesVolFlow = FinalSysSizing(CurSysNum)%DesOutAirVolFlow
          ELSE
            SELECT CASE(CurDuctType)
              CASE(Main)
                DesVolFlow = FinalSysSizing(CurSysNum)%SysAirMinFlowRat*FinalSysSizing(CurSysNum)%DesMainVolFlow
              CASE(Cooling)
                DesVolFlow = FinalSysSizing(CurSysNum)%SysAirMinFlowRat*FinalSysSizing(CurSysNum)%DesCoolVolFlow
              CASE(Heating)
                DesVolFlow = FinalSysSizing(CurSysNum)%DesHeatVolFlow
              CASE(Other)
                DesVolFlow = FinalSysSizing(CurSysNum)%DesMainVolFlow
              CASE DEFAULT
                DesVolFlow = FinalSysSizing(CurSysNum)%DesMainVolFlow
           END SELECT
          END IF
          DesMassFlow = RhoAirStd*DesVolFlow
          ! reset the design air volume flow rate
          WaterCoil(CoilNum)%DesAirVolFlowRate = DesVolFlow
          ! get the outside air fraction
          IF (CurOASysNum > 0) THEN
            OutAirFrac = 1.0d0
          ELSE IF (FinalSysSizing(CurSysNum)%HeatOAOption == MinOA) THEN
            IF (DesVolFlow > 0.0d0) THEN
              OutAirFrac = FinalSysSizing(CurSysNum)%DesOutAirVolFlow / DesVolFlow
            ELSE
              OutAirFrac = 1.0d0
            END IF
            OutAirFrac = MIN(1.0d0,MAX(0.0d0,OutAirFrac))
          ELSE
            OutAirFrac = 1.0d0
          END IF
          ! coil input temperature
          IF (CurOASysNum == 0 .AND. PrimaryAirSystem(CurSysNum)%NumOAHeatCoils > 0) THEN
            CoilInTemp = OutAirFrac*FinalSysSizing(CurSysNum)%PreheatTemp + &
                           (1.0d0-OutAirFrac)*FinalSysSizing(CurSysNum)%HeatRetTemp
          ELSE
            CoilInTemp = OutAirFrac*FinalSysSizing(CurSysNum)%HeatOutTemp + &
                           (1.0d0-OutAirFrac)*FinalSysSizing(CurSysNum)%HeatRetTemp
          END IF
          ! coil load
          IF (CurOASysNum > 0) THEN
            DesCoilLoad = CpAirStd*DesMassFlow*(FinalSysSizing(CurSysNum)%PreheatTemp - CoilInTemp)
          ELSE
            DesCoilLoad = CpAirStd*DesMassFlow*(FinalSysSizing(CurSysNum)%HeatSupTemp - CoilInTemp)
          END IF
          IF (DesCoilLoad >= SmallLoad) THEN

            Cp =  GetSpecificHeatGlycol(PlantLoop(WaterCoil(CoilNum)%WaterLoopNum)%FluidName,  &
                       60.0d0,                      &
                       PlantLoop(WaterCoil(CoilNum)%WaterLoopNum)%FluidIndex, &
                       'SizeWaterCoil')

            rho = GetDensityGlycol(PlantLoop(WaterCoil(CoilNum)%WaterLoopNum)%FluidName,  &
                       InitConvTemp,                      &
                       PlantLoop(WaterCoil(CoilNum)%WaterLoopNum)%FluidIndex, &
                       'SizeWaterCoil')
            MaxWaterVolFlowRateDes = DesCoilLoad / &
                                  ( PlantSizData(PltSizHeatNum)%DeltaT * &
                                  Cp * rho )
          ELSE
            MaxWaterVolFlowRateDes = 0.0d0
            CALL ShowWarningError('The design coil load is zero for Coil:Heating:Water ' &
                                  //TRIM(WaterCoil(CoilNum)%Name))
            CALL ShowContinueError('The autosize value for maximum water flow rate is zero')
            CALL ShowContinueError('To change this, input a value for UA, change the heating design day, or lower')
            CALL ShowContinueError('  the system heating design supply air temperature')
          END IF
          WaterCoil(CoilNum)%DesWaterHeatingCoilRate = DesCoilLoad
          DesWaterHeatingCoilRateDes = DesCoilLoad
          IF (IsAutosize) THEN
            WaterCoil(CoilNum)%MaxWaterVolFlowRate = MaxWaterVolFlowRateDes
            WaterCoil(CoilNum)%DesTotWaterCoilLoad = DesWaterHeatingCoilRateDes
            CALL ReportSizingOutput('Coil:Heating:Water',WaterCoil(CoilNum)%Name, &
                                  'Design Size Design Coil Load [W]',DesWaterHeatingCoilRateDes)
            CALL ReportSizingOutput('Coil:Heating:Water',WaterCoil(CoilNum)%Name, &
                                  'Design Size Maximum Water Flow Rate [m3/s]',MaxWaterVolFlowRateDes)
          ELSE
            MaxWaterVolFlowRateUser = WaterCoil(CoilNum)%MaxWaterVolFlowRate
            DesWaterHeatingCoilRateUser = WaterCoil(CoilNum)%DesTotWaterCoilLoad
            IF (WaterCoil(CoilNum)%MaxWaterVolFlowRate > 0.0d0 .AND. MaxWaterVolFlowRateDes > 0.0d0) THEN
              CALL ReportSizingOutput('Coil:Heating:Water',WaterCoil(CoilNum)%Name, &
                                  'Design Size Maximum Water Flow Rate [m3/s]',MaxWaterVolFlowRateDes, &
                                  'User-Specified Maximum Water Flow Rate [m3/s]',MaxWaterVolFlowRateUser)
              IF (DisplayExtraWarnings) THEN
                IF ((ABS(MaxWaterVolFlowRateDes - MaxWaterVolFlowRateUser)/MaxWaterVolFlowRateUser) >AutoVsHardSizingThreshold) THEN
                  CALL ShowMessage('SizeWaterCoil: Potential issue with equipment sizing for Coil = ' &
                                     //TRIM(WaterCoil(CoilNum)%Name))
                  CALL ShowContinueError('User-Specified Maximum Water Flow Rate of '// &
                                      TRIM(RoundSigDigits(MaxWaterVolFlowRateUser,5))// ' [m3/s]')
                  CALL ShowContinueError('differs from Design Size Maximum Water Flow Rate of ' // &
                                      TRIM(RoundSigDigits(MaxWaterVolFlowRateDes,5))// ' [m3/s]')
                  CALL ShowContinueError('This may, or may not, indicate mismatched component sizes.')
                  CALL ShowContinueError('Verify that the value entered is intended and is consistent with other components.')
                END IF
              ENDIF
            END IF
            IF ( WaterCoil(CoilNum)%DesTotWaterCoilLoad > 0.0d0 .AND. DesWaterHeatingCoilRateDes > 0.0d0) THEN
              CALL ReportSizingOutput('Coil:Heating:Water',WaterCoil(CoilNum)%Name, &
                                  'Design Size Design Coil Load [W]',DesWaterHeatingCoilRateDes, &
                                  'User-Specified Design Coil Load [W]',DesWaterHeatingCoilRateUser)
              IF (DisplayExtraWarnings) THEN
                IF ((ABS(DesWaterHeatingCoilRateDes - DesWaterHeatingCoilRateUser)/DesWaterHeatingCoilRateUser) &
                                       > AutoVsHardSizingThreshold) THEN
                  CALL ShowMessage('SizeWaterCoil: Potential issue with equipment sizing for Coil = ' &
                                       //TRIM(WaterCoil(CoilNum)%Name))
                  CALL ShowContinueError('User-Specified Design Coil Load of '// &
                                      TRIM(RoundSigDigits(DesWaterHeatingCoilRateUser,2))// ' [W]')
                  CALL ShowContinueError('differs from Design Size Design Coil Load of ' // &
                                      TRIM(RoundSigDigits(DesWaterHeatingCoilRateDes,2))// ' [W]')
                  CALL ShowContinueError('This may, or may not, indicate mismatched component sizes.')
                  CALL ShowContinueError('Verify that the value entered is intended and is consistent with other components.')
                END IF
              ENDIF
            END IF
          END IF
          ! if the coil UA needs autosizing, then do it
          IsAutosize = .FALSE.
          IF ( (WaterCoil(CoilNum)%CoilPerfInpMeth == UAandFlow .AND. WaterCoil(CoilNum)%UACoil == AutoSize) .OR. &
             (WaterCoil(CoilNum)%CoilPerfInpMeth == NomCap .AND. WaterCoil(CoilNum)%DesTotWaterCoilLoad == AutoSize) ) THEN
            IsAutosize = .TRUE.
          END IF
          IF (.NOT. IsAutosize) THEN
            ! to avoid a false indication after calling SolveRegulaFalsi
            UACoilUser = WaterCoil(CoilNum)%UACoil
          END IF
 !         CALL CheckSysSizing('Coil:Heating:Water',WaterCoil(CoilNum)%Name)
          ! set the duct flow rate
          IF (CurOASysNum > 0) THEN
            DesVolFlow = FinalSysSizing(CurSysNum)%DesOutAirVolFlow
          ELSE
            SELECT CASE(CurDuctType)
              CASE(Main)
                DesVolFlow = FinalSysSizing(CurSysNum)%SysAirMinFlowRat*FinalSysSizing(CurSysNum)%DesMainVolFlow
              CASE(Cooling)
                DesVolFlow = FinalSysSizing(CurSysNum)%SysAirMinFlowRat*FinalSysSizing(CurSysNum)%DesCoolVolFlow
              CASE(Heating)
                DesVolFlow = FinalSysSizing(CurSysNum)%DesHeatVolFlow
              CASE(Other)
                DesVolFlow = FinalSysSizing(CurSysNum)%DesMainVolFlow
              CASE DEFAULT
                DesVolFlow = FinalSysSizing(CurSysNum)%DesMainVolFlow
           END SELECT
          END IF
          DesMassFlow = RhoAirStd*DesVolFlow
          ! reset the coil design air volume flow rate
          WaterCoil(CoilNum)%DesAirVolFlowRate = DesVolFlow
          ! get the outside air fraction
          IF (CurOASysNum > 0) THEN
            OutAirFrac = 1.0d0
          ELSE IF (FinalSysSizing(CurSysNum)%HeatOAOption == MinOA) THEN
            IF (DesVolFlow > 0.0d0) THEN
              OutAirFrac = FinalSysSizing(CurSysNum)%DesOutAirVolFlow / DesVolFlow
            ELSE
              OutAirFrac = 1.0d0
            END IF
            OutAirFrac = MIN(1.0d0,MAX(0.0d0,OutAirFrac))
          ELSE
            OutAirFrac = 1.0d0
          END IF
          ! coil inlet temperature
          IF (CurOASysNum == 0 .AND. PrimaryAirSystem(CurSysNum)%NumOAHeatCoils > 0) THEN
            CoilInTemp = OutAirFrac*FinalSysSizing(CurSysNum)%PreheatTemp + &
                           (1.0d0-OutAirFrac)*FinalSysSizing(CurSysNum)%HeatRetTemp
            CoilInHumRat = OutAirFrac*FinalSysSizing(CurSysNum)%PreheatHumRat + &
                             (1.0d0-OutAirFrac)*FinalSysSizing(CurSysNum)%HeatRetHumRat
          ELSE
            CoilInTemp = OutAirFrac*FinalSysSizing(CurSysNum)%HeatOutTemp + &
                           (1.0d0-OutAirFrac)*FinalSysSizing(CurSysNum)%HeatRetTemp
            CoilInHumRat = OutAirFrac*FinalSysSizing(CurSysNum)%HeatOutHumRat + &
                           (1.0d0-OutAirFrac)*FinalSysSizing(CurSysNum)%HeatRetHumRat
          END IF
          ! coil load
          IF (CurOASysNum > 0) THEN
            DesCoilLoad = CpAirStd*DesMassFlow*(FinalSysSizing(CurSysNum)%PreheatTemp - CoilInTemp)
          ELSE
            DesCoilLoad = CpAirStd*DesMassFlow*(FinalSysSizing(CurSysNum)%HeatSupTemp - CoilInTemp)
          END IF
          IF (DesCoilLoad >= SmallLoad) THEN
            Par(1) = DesCoilLoad
            Par(2) = REAL(CoilNum,r64)
            Par(3) = REAL(ContFanCycCoil,r64) !fan operating mode
            Par(4) = 1.0d0 ! part-load ratio
            WaterCoil(CoilNum)%InletAirTemp = CoilInTemp
            WaterCoil(CoilNum)%InletAirHumRat = CoilInHumRat
            WaterCoil(CoilNum)%InletWaterTemp = PlantSizData(PltSizHeatNum)%ExitTemp

            rho = GetDensityGlycol(PlantLoop(WaterCoil(CoilNum)%WaterLoopNum)%FluidName,  &
                       InitConvTemp,                      &
                       PlantLoop(WaterCoil(CoilNum)%WaterLoopNum)%FluidIndex, &
                       'SizeWaterCoil')
            WaterCoil(CoilNum)%InletWaterMassFlowRate = rho * WaterCoil(CoilNum)%MaxWaterVolFlowRate
            WaterCoil(CoilNum)%InletAirMassFlowRate = DesMassFlow
            ! set the lower and upper limits on the UA
            UA0 = .001d0 * DesCoilLoad
            UA1 = DesCoilLoad
            ! Invert the simple heating coil model: given the design inlet conditions and the design load,
            ! find the design UA.
            CALL SolveRegulaFalsi(Acc, MaxIte, SolFla, UA, SimpleHeatingCoilUAResidual, UA0, UA1, Par)
            ! if the numerical inversion failed, issue error messages.
            IF (SolFla == -1) THEN
              CALL ShowSevereError('Autosizing of heating coil UA failed for Coil:Heating:Water "'// &
                                    TRIM(WaterCoil(CoilNum)%Name)//'"')
              CALL ShowContinueError('  Iteration limit exceeded in calculating coil UA')
              CALL ShowContinueError('  Lower UA estimate = '//TRIM(TrimSigDigits(UA0,6))//' W/m2-K (1% of Design Coil Load)')
              CALL ShowContinueError('  Upper UA estimate = '//TRIM(TrimSigDigits(UA1,6))//' W/m2-K (100% of Design Coil Load)')
              CALL ShowContinueError('  Final UA estimate when iterations exceeded limit = '//TRIM(TrimSigDigits(UA,6))//' W/m2-K')
              CALL ShowContinueError('  AirloopHVAC "'//TRIM(FinalSysSizing(CurSysNum)%AirPriLoopName)// &
                                     '" coil sizing conditions (may be different than Sizing inputs):')
              CALL ShowContinueError('  Coil inlet air temperature     = '//TRIM(TrimSigDigits(CoilInTemp,3))//' C')
              CALL ShowContinueError('  Coil inlet air humidity ratio  = '//TRIM(TrimSigDigits(CoilInHumRat,3))//  &
                 ' kgWater/kgDryAir')
              CALL ShowContinueError('  Coil inlet air mass flow rate  = '//TRIM(TrimSigDigits(DesMassFlow,6))//' kg/s')
              CALL ShowContinueError('  Design Coil Capacity           = '// &
                                     TRIM(TrimSigDigits(WaterCoil(CoilNum)%TotWaterHeatingCoilRate,3))//' W')
              CALL ShowContinueError('  Design Coil Load               = '//TRIM(TrimSigDigits(DesCoilLoad,3))//' W')
              ErrorsFound = .TRUE.
            ELSE IF (SolFla == -2) THEN
              CALL ShowSevereError('Autosizing of heating coil UA failed for Coil:Heating:Water "'// &
                                    TRIM(WaterCoil(CoilNum)%Name)//'"')
              CALL ShowContinueError('  Bad starting values for UA')
              CALL ShowContinueError('  Lower UA estimate = '//TRIM(TrimSigDigits(UA0,6))//' W/m2-K (1% of Design Coil Load)')
              CALL ShowContinueError('  Upper UA estimate = '//TRIM(TrimSigDigits(UA1,6))//' W/m2-K (100% of Design Coil Load)')
              CALL ShowContinueError('  AirloopHVAC "'//TRIM(FinalSysSizing(CurSysNum)%AirPriLoopName)// &
                                     '" coil sizing conditions (may be different than Sizing inputs):')
              CALL ShowContinueError('  Coil inlet air temperature     = '//TRIM(TrimSigDigits(CoilInTemp,3))//' C')
              CALL ShowContinueError('  Coil inlet air humidity ratio  = '//TRIM(TrimSigDigits(CoilInHumRat,3))//  &
                 ' kgWater/kgDryAir')
              CALL ShowContinueError('  Coil inlet air mass flow rate  = '//TRIM(TrimSigDigits(DesMassFlow,6))//' kg/s')
              CALL ShowContinueError('  Design Coil Capacity           = '// &
                                     TRIM(TrimSigDigits(WaterCoil(CoilNum)%TotWaterHeatingCoilRate,3))//' W')
              CALL ShowContinueError('  Design Coil Load               = '//TRIM(TrimSigDigits(DesCoilLoad,3))//' W')
              IF (WaterCoil(CoilNum)%TotWaterHeatingCoilRate < DesCoilLoad) THEN
                CALL ShowContinueError('  Inadequate water side capacity: in Plant Sizing for this hot water loop')
                CALL ShowContinueError('  increase design loop exit temperature and/or decrease design loop delta T')
                CALL ShowContinueError('  Plant Sizing object = '//TRIM(PlantSizData(PltSizHeatNum)%PlantLoopName))
                CALL ShowContinueError('  Plant design loop exit temperature = '// &
                                          TRIM(TrimSigDigits(PlantSizData(PltSizHeatNum)%ExitTemp,3))//' C')
                CALL ShowContinueError('  Plant design loop delta T          = '// &
                                          TRIM(TrimSigDigits(PlantSizData(PltSizHeatNum)%DeltaT,3))//' C')
              END IF
              ErrorsFound = .TRUE.
            END IF
            UACoilDes = UA
          ELSE
            UACoilDes = 1.0d0
            IF (WaterCoil(CoilNum)%MaxWaterVolFlowRate > 0.0d0) THEN
              ErrorsFound = .TRUE.
              CALL ShowSevereError('The design coil load is zero for Coil:Heating:Water ' &
                                    //TRIM(WaterCoil(CoilNum)%Name))
              CALL ShowContinueError('An autosize value for UA cannot be calculated')
              CALL ShowContinueError('Input a value for UA, change the heating design day, or raise')
              CALL ShowContinueError('  the system heating design supply air temperature')
            END IF
          END IF
          WaterCoil(CoilNum)%DesWaterHeatingCoilRate = DesCoilLoad
          IF (IsAutosize) THEN
            WaterCoil(CoilNum)%UACoil = UACoilDes
            WaterCoil(CoilNum)%DesTotWaterCoilLoad = DesCoilLoad
            CALL ReportSizingOutput('Coil:Heating:Water',WaterCoil(CoilNum)%Name, &
                                  'Design Size U-Factor Times Area Value [W/K]',UACoilDes)
          ELSE
            IF (UACoilUser > 0.0d0 .AND. UACoilDes > 0.0d0) THEN
              CALL ReportSizingOutput('Coil:Heating:Water',WaterCoil(CoilNum)%Name, &
                                  'Design Size U-Factor Times Area Value [W/K]',UACoilDes, &
                                  'User-Specified U-Factor Times Area Value [W/K]',UACoilUser)
              IF (DisplayExtraWarnings) THEN
                IF ((ABS(UACoilDes - UACoilUser)/UACoilUser) > AutoVsHardSizingThreshold) THEN
                  CALL ShowMessage('SizeWaterCoil: Potential issue with equipment sizing for Coil = ' &
                                       //TRIM(WaterCoil(CoilNum)%Name))
                  CALL ShowContinueError('User-Specified U-Factor Times Area Value of '// &
                                      TRIM(RoundSigDigits(UACoilUser,5))// ' [W/K]')
                  CALL ShowContinueError('differs from Design Size U-Factor Times Area Value of ' // &
                                      TRIM(RoundSigDigits(UACoilDes,5))// ' [W/K]')
                  CALL ShowContinueError('This may, or may not, indicate mismatched component sizes.')
                  CALL ShowContinueError('Verify that the value entered is intended and is consistent with other components.')
                END IF
              ENDIF
            END IF
          END IF
        END IF
      ! if this is a zone coil
      ELSE IF (CurZoneEqNum > 0) THEN
        IsAutosizeReq = .FALSE.
        IF (WaterCoil(CoilNum)%RequestingAutosize) THEN
          IsAutosizeReq = .TRUE.
        END IF
        IF (.NOT. IsAutosizeReq .AND. .NOT. ZoneSizingRunDone) THEN
          IF (WaterCoil(CoilNum)%MaxWaterVolFlowRate > 0.0d0) THEN
            CALL ReportSizingOutput('Coil:Heating:Water',WaterCoil(CoilNum)%Name, &
                                'User-Specified Maximum Water Flow Rate [m3/s]',WaterCoil(CoilNum)%MaxWaterVolFlowRate)
          END IF
          IF (WaterCoil(CoilNum)%UACoil > 0.0d0) THEN
            CALL ReportSizingOutput('Coil:Heating:Water',WaterCoil(CoilNum)%Name, &
                                'User-Specified U-Factor Times Area Value [W/K]',WaterCoil(CoilNum)%UACoil)
          END IF
          IF (WaterCoil(CoilNum)%DesTotWaterCoilLoad > 0.0d0) THEN
            CALL ReportSizingOutput('Coil:Heating:Water',WaterCoil(CoilNum)%Name, &
                                'User-Specified Design Coil Load [W]',WaterCoil(CoilNum)%DesTotWaterCoilLoad)
          END IF
        ELSE
          CALL CheckZoneSizing('Coil:Heating:Water',WaterCoil(CoilNum)%Name)
          ! autosize the coil water volume flow rate if needed
          IsAutosize = .FALSE.
          IF ( (WaterCoil(CoilNum)%CoilPerfInpMeth == UAandFlow .AND. WaterCoil(CoilNum)%MaxWaterVolFlowRate == AutoSize) .OR. &
             (WaterCoil(CoilNum)%CoilPerfInpMeth == NomCap .AND. WaterCoil(CoilNum)%DesTotWaterCoilLoad == AutoSize) ) THEN
            IsAutosize = .TRUE.
          END IF
!          CALL CheckZoneSizing('Coil:Heating:Water',WaterCoil(CoilNum)%Name)
          ! if coil is part of a terminal unit just use the terminal unit value
          IF (TermUnitSingDuct .OR. TermUnitPIU .OR. TermUnitIU) THEN
            MaxWaterVolFlowRateDes = TermUnitSizing(CurZoneEqNum)%MaxHWVolFlow
            Cp =  GetSpecificHeatGlycol(PlantLoop(WaterCoil(CoilNum)%WaterLoopNum)%FluidName,  &
                       60.0d0,                      &
                       PlantLoop(WaterCoil(CoilNum)%WaterLoopNum)%FluidIndex, &
                       'SizeWaterCoil')

            rho = GetDensityGlycol(PlantLoop(WaterCoil(CoilNum)%WaterLoopNum)%FluidName,  &
                       InitConvTemp,                      &
                       PlantLoop(WaterCoil(CoilNum)%WaterLoopNum)%FluidIndex, &
                       'SizeWaterCoil')

            DesCoilLoad = MaxWaterVolFlowRateDes * PlantSizData(PltSizHeatNum)%DeltaT * &
                          Cp * rho
          ELSE IF (ZoneEqFanCoil) THEN
            MaxWaterVolFlowRateDes = ZoneEqSizing(CurZoneEqNum)%MaxHWVolFlow
            Cp =  GetSpecificHeatGlycol(PlantLoop(WaterCoil(CoilNum)%WaterLoopNum)%FluidName,  &
                       60.0d0,                      &
                       PlantLoop(WaterCoil(CoilNum)%WaterLoopNum)%FluidIndex, &
                       'SizeWaterCoil')

            rho = GetDensityGlycol(PlantLoop(WaterCoil(CoilNum)%WaterLoopNum)%FluidName,  &
                       InitConvTemp,                      &
                       PlantLoop(WaterCoil(CoilNum)%WaterLoopNum)%FluidIndex, &
                       'SizeWaterCoil')

            DesCoilLoad = MaxWaterVolFlowRateDes * PlantSizData(PltSizHeatNum)%DeltaT * &
                          Cp * rho
          ! if coil is part of a zonal unit, calc coil load to get hot water flow rate
          ELSE
            CoilInTemp = FinalZoneSizing(CurZoneEqNum)%DesHeatCoilInTemp
            CoilOutTemp = FinalZoneSizing(CurZoneEqNum)%HeatDesTemp
            CoilOutHumRat = FinalZoneSizing(CurZoneEqNum)%HeatDesHumRat
            DesMassFlow = FinalZoneSizing(CurZoneEqNum)%DesHeatMassFlow
            DesCoilLoad = PsyCpAirFnWTdb(CoilOutHumRat, 0.5d0*(CoilInTemp+CoilOutTemp)) &
                          * DesMassFlow * (CoilOutTemp-CoilInTemp)
            IF (DesCoilLoad >= SmallLoad) THEN
              Cp =  GetSpecificHeatGlycol(PlantLoop(WaterCoil(CoilNum)%WaterLoopNum)%FluidName,  &
                       60.0d0,                      &
                       PlantLoop(WaterCoil(CoilNum)%WaterLoopNum)%FluidIndex, &
                       'SizeWaterCoil')

              rho = GetDensityGlycol(PlantLoop(WaterCoil(CoilNum)%WaterLoopNum)%FluidName,  &
                       InitConvTemp,                      &
                       PlantLoop(WaterCoil(CoilNum)%WaterLoopNum)%FluidIndex, &
                       'SizeWaterCoil')

              MaxWaterVolFlowRateDes = DesCoilLoad / &
                                       ( PlantSizData(PltSizHeatNum)%DeltaT * &
                                       Cp* rho )
            ELSE
              MaxWaterVolFlowRateDes = 0.0d0
            END IF
            WaterCoil(CoilNum)%DesWaterHeatingCoilRate = DesCoilLoad
            DesWaterHeatingCoilRateDes = DesCoilLoad
          END IF
          ! issue warning if hw coil has zero flow
          IF (MaxWaterVolFlowRateDes == 0.0d0) THEN
            CALL ShowWarningError('The design coil load is zero for Coil:Heating:Water ' &
                                  //TRIM(WaterCoil(CoilNum)%Name))
            CALL ShowContinueError('The autosize value for maximum water flow rate is zero')
            CALL ShowContinueError('To change this, input a value for UA, change the heating design day, or lower')
            CALL ShowContinueError('  the system heating design supply air temperature')
          END IF
          IF (IsAutosize) THEN
            WaterCoil(CoilNum)%MaxWaterVolFlowRate = MaxWaterVolFlowRateDes
            !WaterCoil(CoilNum)%DesTotWaterCoilLoad = DesWaterHeatingCoilRateDes
            CALL ReportSizingOutput('Coil:Heating:Water',WaterCoil(CoilNum)%Name, &
                                  'Design Size Design Coil Load [W]',DesCoilLoad) !DesWaterHeatingCoilRateDes)
            CALL ReportSizingOutput('Coil:Heating:Water',WaterCoil(CoilNum)%Name, &
                                  'Design Size Maximum Water Flow Rate [m3/s]',MaxWaterVolFlowRateDes)
          ELSE
            MaxWaterVolFlowRateUser = WaterCoil(CoilNum)%MaxWaterVolFlowRate
            DesWaterHeatingCoilRateUser = WaterCoil(CoilNum)%DesTotWaterCoilLoad
            IF (WaterCoil(CoilNum)%MaxWaterVolFlowRate > 0.0d0 .AND. MaxWaterVolFlowRateDes > 0.0d0) THEN
              CALL ReportSizingOutput('Coil:Heating:Water',WaterCoil(CoilNum)%Name, &
                                  'Design Size Maximum Water Flow Rate [m3/s]',MaxWaterVolFlowRateDes, &
                                  'User-Specified Maximum Water Flow Rate [m3/s]',MaxWaterVolFlowRateUser)
              IF (DisplayExtraWarnings) THEN
                IF ((ABS(MaxWaterVolFlowRateDes - MaxWaterVolFlowRateUser)/MaxWaterVolFlowRateUser) &
                               > AutoVsHardSizingThreshold) THEN
                  CALL ShowMessage('SizeWaterCoil: Potential issue with equipment sizing for Coil = ' &
                                       //TRIM(WaterCoil(CoilNum)%Name))
                  CALL ShowContinueError('User-Specified Maximum Water Flow Rate of '// &
                                      TRIM(RoundSigDigits(MaxWaterVolFlowRateUser,5))// ' [m3/s]')
                  CALL ShowContinueError('differs from Design Size Maximum Water Flow Rate of ' // &
                                      TRIM(RoundSigDigits(MaxWaterVolFlowRateDes,5))// ' [m3/s]')
                  CALL ShowContinueError('This may, or may not, indicate mismatched component sizes.')
                  CALL ShowContinueError('Verify that the value entered is intended and is consistent with other components.')
                END IF
              ENDIF
            END IF
            IF ( WaterCoil(CoilNum)%DesTotWaterCoilLoad > 0.0d0 .AND. DesWaterHeatingCoilRateDes > 0.0d0) THEN
              CALL ReportSizingOutput('Coil:Heating:Water',WaterCoil(CoilNum)%Name, &
                                  'Design Size Design Coil Load [W]',DesWaterHeatingCoilRateDes, &
                                  'User-Specified Design Coil Load [W]',DesWaterHeatingCoilRateUser)
              IF (DisplayExtraWarnings) THEN
                IF ((ABS(DesWaterHeatingCoilRateDes - DesWaterHeatingCoilRateUser)/DesWaterHeatingCoilRateUser) &
                                > AutoVsHardSizingThreshold) THEN
                  CALL ShowMessage('SizeWaterCoil: Potential issue with equipment sizing for Coil = ' &
                                      //TRIM(WaterCoil(CoilNum)%Name))
                  CALL ShowContinueError('User-Specified Design Coil Load of '// &
                                      TRIM(RoundSigDigits(DesWaterHeatingCoilRateUser,2))// ' [W]')
                  CALL ShowContinueError('differs from Design Size Design Coil Load of ' // &
                                      TRIM(RoundSigDigits(DesWaterHeatingCoilRateDes,2))// ' [W]')
                  CALL ShowContinueError('This may, or may not, indicate mismatched component sizes.')
                  CALL ShowContinueError('Verify that the value entered is intended and is consistent with other components.')
                END IF
              ENDIF
            END IF
          END IF

        ! autosize the coil UA if needed
          IsAutosize = .FALSE.
          IF ( (WaterCoil(CoilNum)%CoilPerfInpMeth == UAandFlow .AND. WaterCoil(CoilNum)%UACoil == AutoSize) .OR. &
             (WaterCoil(CoilNum)%CoilPerfInpMeth == NomCap .AND. WaterCoil(CoilNum)%DesTotWaterCoilLoad == AutoSize) ) THEN
            IsAutosize = .TRUE.
          END IF
          IF (.NOT. IsAutosize) THEN
            UACoilUser = WaterCoil(CoilNum)%UACoil
          END IF
!          CALL CheckZoneSizing('Coil:Heating:Water',WaterCoil(CoilNum)%Name)
          IF (TermUnitPIU) THEN
            MinFlowFrac = TermUnitSizing(CurZoneEqNum)%MinFlowFrac
            CoilInTemp = FinalZoneSizing(CurZoneEqNum)%DesHeatCoilInTempTU * MinFlowFrac + &
                         FinalZoneSizing(CurZoneEqNum)%ZoneTempAtHeatPeak * (1.d0 - MinFlowFrac)
            CoilInHumRat = FinalZoneSizing(CurZoneEqNum)%DesHeatCoilInHumRatTU * MinFlowFrac + &
                         FinalZoneSizing(CurZoneEqNum)%ZoneHumRatAtHeatPeak * (1.d0 - MinFlowFrac)
          ELSE IF (TermUnitIU) THEN
            CoilInTemp = FinalZoneSizing(CurZoneEqNum)%ZoneTempAtHeatPeak
            CoilInHumRat = FinalZoneSizing(CurZoneEqNum)%ZoneHumRatAtHeatPeak
          ELSE IF (TermUnitSingDuct) THEN
            CoilInTemp = FinalZoneSizing(CurZoneEqNum)%DesHeatCoilInTempTU
            CoilInHumRat = FinalZoneSizing(CurZoneEqNum)%DesHeatCoilInHumRatTU
          ELSE IF (ZoneEqFanCoil) THEN
            IF (FinalZoneSizing(CurZoneEqNum)%DesHeatMassFlow > 0.0d0) THEN
              FCOAFrac = MIN(RhoAirStd*ZoneEqSizing(CurZoneEqNum)%OAVolFlow / FinalZoneSizing(CurZoneEqNum)%DesHeatMassFlow, 1.0d0)
            ELSE
              FCOAFrac = 0.0d0
            END IF
            CoilInTemp = FCOAFrac*FinalZoneSizing(CurZoneEqNum)%OutTempAtHeatPeak + &
                           (1.0d0-FCOAFrac)*FinalZoneSizing(CurZoneEqNum)%ZoneTempAtHeatPeak
            CoilInHumRat = FCOAFrac*FinalZoneSizing(CurZoneEqNum)%OutHumRatAtHeatPeak + &
                           (1.0d0-FCOAFrac)*FinalZoneSizing(CurZoneEqNum)%ZoneHumRatAtHeatPeak
          ELSE
            CoilInTemp = FinalZoneSizing(CurZoneEqNum)%DesHeatCoilInTemp
            CoilInHumRat = FinalZoneSizing(CurZoneEqNum)%DesHeatCoilInHumRat
          END IF
          IF (TermUnitSingDuct .OR. TermUnitPIU .OR. TermUnitIU) THEN
            DesMassFlow = RhoAirStd * TermUnitSizing(CurZoneEqNum)%AirVolFlow * TermUnitSizing(CurZoneEqNum)%ReheatAirFlowMult
            Cp =  GetSpecificHeatGlycol(PlantLoop(WaterCoil(CoilNum)%WaterLoopNum)%FluidName,  &
                     60.0d0,                      &
                     PlantLoop(WaterCoil(CoilNum)%WaterLoopNum)%FluidIndex, &
                     'SizeWaterCoil')

            rho = GetDensityGlycol(PlantLoop(WaterCoil(CoilNum)%WaterLoopNum)%FluidName,  &
                     InitConvTemp,                      &
                     PlantLoop(WaterCoil(CoilNum)%WaterLoopNum)%FluidIndex, &
                     'SizeWaterCoil')

            DesCoilLoad = WaterCoil(CoilNum)%MaxWaterVolFlowRate * PlantSizData(PltSizHeatNum)%DeltaT * &
                            Cp * rho * TermUnitSizing(CurZoneEqNum)%ReheatLoadMult
            DesWaterVolFlow =  WaterCoil(CoilNum)%MaxWaterVolFlowRate * TermUnitSizing(CurZoneEqNum)%ReheatLoadMult
          ELSE IF (ZoneEqFanCoil) THEN
            DesMassFlow = RhoAirStd * FinalZoneSizing(CurZoneEqNum)%DesHeatVolFlow
            Cp =  GetSpecificHeatGlycol(PlantLoop(WaterCoil(CoilNum)%WaterLoopNum)%FluidName,  &
                     60.0d0,                      &
                     PlantLoop(WaterCoil(CoilNum)%WaterLoopNum)%FluidIndex, &
                     'SizeWaterCoil')

            rho = GetDensityGlycol(PlantLoop(WaterCoil(CoilNum)%WaterLoopNum)%FluidName,  &
                     InitConvTemp,                      &
                     PlantLoop(WaterCoil(CoilNum)%WaterLoopNum)%FluidIndex, &
                     'SizeWaterCoil')

            DesCoilLoad = WaterCoil(CoilNum)%MaxWaterVolFlowRate * PlantSizData(PltSizHeatNum)%DeltaT * &
                            Cp * rho
            DesWaterVolFlow =  WaterCoil(CoilNum)%MaxWaterVolFlowRate
          ELSE
            DesMassFlow = FinalZoneSizing(CurZoneEqNum)%DesHeatMassFlow
            CoilOutTemp = FinalZoneSizing(CurZoneEqNum)%HeatDesTemp
            CoilOutHumRat = FinalZoneSizing(CurZoneEqNum)%HeatDesHumRat
            DesCoilLoad = PsyCpAirFnWTdb(CoilOutHumRat, 0.5d0*(CoilInTemp+CoilOutTemp)) &
                          * DesMassFlow * (CoilOutTemp-CoilInTemp)
            DesWaterVolFlow =  WaterCoil(CoilNum)%MaxWaterVolFlowRate
          END IF
          IF (DesCoilLoad >= SmallLoad) THEN
          ! pass along the coil number and the design load to the residual calculation
            Par(1) = DesCoilLoad
            Par(2) = REAL(CoilNum,r64)
            Par(3) = REAL(ContFanCycCoil,r64) !fan operating mode
            Par(4) = 1.0d0 ! part-load ratio
            WaterCoil(CoilNum)%InletAirTemp = CoilInTemp
            WaterCoil(CoilNum)%InletAirHumRat = CoilInHumRat
            WaterCoil(CoilNum)%InletAirEnthalpy = PsyHFnTdbW(CoilInTemp,CoilInHumrat) !9216 added
          !  WaterCoil(CoilNum)%InletAirMassFlowRate = MAX(FinalZoneSizing(CurZoneEqNum)%DesCoolMassFlow, &
          !                                   FinalZoneSizing(CurZoneEqNum)%DesHeatMassFlow)
            WaterCoil(CoilNum)%InletAirMassFlowRate = DesMassFlow
            WaterCoil(CoilNum)%InletWaterTemp = PlantSizData(PltSizHeatNum)%ExitTemp
            rho = GetDensityGlycol(PlantLoop(WaterCoil(CoilNum)%WaterLoopNum)%FluidName,  &
                     InitConvTemp,                      &
                     PlantLoop(WaterCoil(CoilNum)%WaterLoopNum)%FluidIndex, &
                     'SizeWaterCoil')
            WaterCoil(CoilNum)%InletWaterMassFlowRate = rho * DesWaterVolFlow
            ! set the lower and upper limits on the UA
            UA0 = .001d0 * DesCoilLoad
            UA1 = DesCoilLoad
            ! Invert the simple heating coil model: given the design inlet conditions and the design load,
            ! find the design UA.
            CALL SolveRegulaFalsi(Acc, MaxIte, SolFla, UA, SimpleHeatingCoilUAResidual, UA0, UA1, Par)
            ! if the numerical inversion failed, issue error messages.
            IF (SolFla == -1) THEN
              CALL ShowSevereError('Autosizing of heating coil UA failed for Coil:Heating:Water "'// &
                                    TRIM(WaterCoil(CoilNum)%Name)//'"')
              CALL ShowContinueError('  Iteration limit exceeded in calculating coil UA')
              CALL ShowContinueError('  Lower UA estimate = '//TRIM(TrimSigDigits(UA0,6))//' W/m2-K (0.1% of Design Coil Load)')
              CALL ShowContinueError('  Upper UA estimate = '//TRIM(TrimSigDigits(UA1,6))//' W/m2-K (100% of Design Coil Load)')
              CALL ShowContinueError('  Final UA estimate when iterations exceeded limit = '//TRIM(TrimSigDigits(UA,6))//' W/m2-K')
              CALL ShowContinueError('  Zone "'//TRIM(FinalZoneSizing(CurZoneEqNum)%ZoneName)// &
                                     '" coil sizing conditions (may be different than Sizing inputs):')
              CALL ShowContinueError('  Coil inlet air temperature     = '//TRIM(TrimSigDigits(CoilInTemp,3))//' C')
              CALL ShowContinueError('  Coil inlet air humidity ratio  = '//TRIM(TrimSigDigits(CoilInHumRat,3))//  &
                 ' kgWater/kgDryAir')
              CALL ShowContinueError('  Coil inlet air mass flow rate  = '//TRIM(TrimSigDigits(DesMassFlow,6))//' kg/s')
! TotWaterHeatingCoilRate is set in CALL to CalcSimpleHeatingCoil
              CALL ShowContinueError('  Design Coil Capacity           = '// &
                                     TRIM(TrimSigDigits(WaterCoil(CoilNum)%TotWaterHeatingCoilRate,3))//' W')
              IF (TermUnitSingDuct .OR. TermUnitPIU .OR. TermUnitIU .OR. ZoneEqFanCoil) THEN
                CALL ShowContinueError('  Design Coil Load               = '//TRIM(TrimSigDigits(DesCoilLoad,3))//' W')
              ELSE
                CALL ShowContinueError('  Design Coil Load               = '//TRIM(TrimSigDigits(DesCoilLoad,3))//' W')
                CALL ShowContinueError('  Coil outlet air temperature    = '//TRIM(TrimSigDigits(CoilOutTemp,3))//' C')
                CALL ShowContinueError('  Coil outlet air humidity ratio = '//TRIM(TrimSigDigits(CoilOutHumRat,3))//  &
                   ' kgWater/kgDryAir')
              END IF
              ErrorsFound = .TRUE.
            ELSE IF (SolFla == -2) THEN
              CALL ShowSevereError('Autosizing of heating coil UA failed for Coil:Heating:Water "'// &
                                    TRIM(WaterCoil(CoilNum)%Name)//'"')
              CALL ShowContinueError('  Bad starting values for UA')
              CALL ShowContinueError('  Lower UA estimate = '//TRIM(TrimSigDigits(UA0,6))//' W/m2-K (0.1% of Design Coil Load)')
              CALL ShowContinueError('  Upper UA estimate = '//TRIM(TrimSigDigits(UA1,6))//' W/m2-K (100% of Design Coil Load)')
              CALL ShowContinueError('  Zone "'//TRIM(FinalZoneSizing(CurZoneEqNum)%ZoneName)// &
                                     '" coil sizing conditions (may be different than Sizing inputs):')
              CALL ShowContinueError('  Coil inlet air temperature     = '//TRIM(TrimSigDigits(CoilInTemp,3))//' C')
              CALL ShowContinueError('  Coil inlet air humidity ratio  = '//TRIM(TrimSigDigits(CoilInHumRat,3))//  &
                 ' kgWater/kgDryAir')
              CALL ShowContinueError('  Coil inlet air mass flow rate  = '//TRIM(TrimSigDigits(DesMassFlow,6))//' kg/s')
              CALL ShowContinueError('  Design Coil Capacity           = '// &
                                     TRIM(TrimSigDigits(WaterCoil(CoilNum)%TotWaterHeatingCoilRate,3))//' W')
              IF (TermUnitSingDuct .OR. TermUnitPIU .OR. TermUnitIU.OR. ZoneEqFanCoil) THEN
                CALL ShowContinueError('  Design Coil Load               = '//TRIM(TrimSigDigits(DesCoilLoad,3))//' W')
              ELSE
                CALL ShowContinueError('  Design Coil Load               = '//TRIM(TrimSigDigits(DesCoilLoad,3))//' W')
                CALL ShowContinueError('  Coil outlet air temperature    = '//TRIM(TrimSigDigits(CoilOutTemp,3))//' C')
                CALL ShowContinueError('  Coil outlet air humidity ratio = '//TRIM(TrimSigDigits(CoilOutHumRat,3))//  &
                   ' kgWater/kgDryAir')
              END IF
! TotWaterHeatingCoilRate is set in CALL to CalcSimpleHeatingCoil
              IF (WaterCoil(CoilNum)%TotWaterHeatingCoilRate < DesCoilLoad) THEN
                CALL ShowContinueError('  Inadequate water side capacity: in Plant Sizing for this hot water loop')
                CALL ShowContinueError('  increase design loop exit temperature and/or decrease design loop delta T')
                CALL ShowContinueError('  Plant Sizing object = '//TRIM(PlantSizData(PltSizHeatNum)%PlantLoopName))
                CALL ShowContinueError('  Plant design loop exit temperature = '// &
                                          TRIM(TrimSigDigits(PlantSizData(PltSizHeatNum)%ExitTemp,3))//' C')
                CALL ShowContinueError('  Plant design loop delta T          = '// &
                                          TRIM(TrimSigDigits(PlantSizData(PltSizHeatNum)%DeltaT,3))//' C')
              END IF
              ErrorsFound = .TRUE.
            END IF
            UACoilDes = UA
          ELSE
            UACoilDes = 1.0d0
            IF (WaterCoil(CoilNum)%MaxWaterVolFlowRate > 0.0d0) THEN
              ErrorsFound = .TRUE.
              CALL ShowSevereError('The design coil load is zero for Coil:Heating:Water ' &
                                    //TRIM(WaterCoil(CoilNum)%Name))
              CALL ShowContinueError('An autosize value for UA cannot be calculated')
              CALL ShowContinueError('Input a value for UA, change the heating design day, or raise')
              CALL ShowContinueError('  the zone heating design supply air temperature')
            END IF
          END IF
          IF (IsAutosize) THEN
            WaterCoil(CoilNum)%UACoil = UACoilDes
            !WaterCoil(CoilNum)%DesTotWaterCoilLoad = DesCoilLoad
            WaterCoil(CoilNum)%DesWaterHeatingCoilRate = DesCoilLoad
            CALL ReportSizingOutput('Coil:Heating:Water',WaterCoil(CoilNum)%Name, &
                                  'Design Size U-Factor Times Area Value [W/K]',UACoilDes)
          ELSE
            IF (UACoilUser > 0.0d0 .AND. UACoilDes > 0.0d0) THEN
              CALL ReportSizingOutput('Coil:Heating:Water',WaterCoil(CoilNum)%Name,&
                                  'Design Size U-Factor Times Area Value [W/K]',UACoilDes, &
                                  'User-Specified U-Factor Times Area Value [W/K]',UACoilUser)
              IF (DisplayExtraWarnings) THEN
                IF ((ABS(UACoilDes - UACoilUser)/UACoilUser) > AutoVsHardSizingThreshold) THEN
                  CALL ShowMessage('SizeWaterCoil: Potential issue with equipment sizing for Coil = ' &
                                     //TRIM(WaterCoil(CoilNum)%Name))
                  CALL ShowContinueError('User-Specified U-Factor Times Area Value of '// &
                                      TRIM(RoundSigDigits(UACoilUser,2))// ' [W/K]')
                  CALL ShowContinueError('differs from Design Size U-Factor Times Area Value of ' // &
                                      TRIM(RoundSigDigits(UACoilDes,2))// ' [W/K]')
                  CALL ShowContinueError('This may, or may not, indicate mismatched component sizes.')
                  CALL ShowContinueError('Verify that the value entered is intended and is consistent with other components.')
                END IF
              ENDIF
            END IF
          END IF
        END IF ! end UA autosize IF
      END IF ! end zone coil ELSE - IF
    ELSE
      ! if there is no heating Plant Sizing object and autosizng was requested, issue an error message
      IF (WaterCoil(CoilNum)%RequestingAutosize) THEN
        CALL ShowSevereError('Autosizing of water coil requires a heating loop Sizing:Plant object')
        CALL ShowContinueError('Occurs in water coil object= '//TRIM(WaterCoil(CoilNum)%Name))
        ErrorsFound = .TRUE.
      END IF
    END IF ! end of heating Plant Sizing existence IF - ELSE
  END IF ! end heating coil IF

  ! save the design water volumetric flow rate for use by the water loop sizing algorithms
  IF (WaterCoil(CoilNum)%MaxWaterVolFlowRate > 0.0d0) THEN
    CALL RegisterPlantCompDesignFlow(WaterCoil(CoilNum)%WaterInletNodeNum,WaterCoil(CoilNum)%MaxWaterVolFlowRate)
  END IF

  IF (ErrorsFound) THEN
    CALL ShowFatalError('Preceding water coil sizing errors cause program termination')
  END IF


  RETURN

END SUBROUTINE SizeWaterCoil

 ! End Initialization Section of the Module
!******************************************************************************


 ! Begin Algorithm Section of the Module
!******************************************************************************
Subroutine CalcSimpleHeatingCoil(CoilNum, FanOpMode, PartLoadRatio, CalcMode)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Rich Liesen
          !       DATE WRITTEN
          !       MODIFIED       Aug. 2007 - R. Raustad, added fan operating mode and part-load ratio to
          !                                  calculate the outlet conditions when fan and coil cycle.
          !                                  Air and water outlet temperature are full output with average
          !                                  air and water mass flow rate when fan and coil cycle.
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Simulates a simple NTU effectiveness model heating coil

          ! METHODOLOGY EMPLOYED:
          ! (1) outlet conditions are calculated from the effectiveness and the inlet conditions.
          ! (2) Effectiveness is calculated from the NTU formula for a cross flow heat exchanger
          !     with both streams unmixed.
          ! Note: UA is input by user and is fixed.

          ! REFERENCES:
          ! See for instance ASHRAE HVAC 2 Toolkit, page 4-4, formula (4-7)

          ! USE STATEMENTS:
          USE DataBranchAirLoopPlant,  ONLY : MassFlowTolerance

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
   INTEGER, INTENT(IN) :: CoilNum        ! index to heating coil
   INTEGER, INTENT(IN) :: FanOpMode      ! fan operating mode
   REAL(r64),    INTENT(IN) :: PartLoadRatio  ! part-load ratio of heating coil
   INTEGER, INTENT(IN) :: CalcMode ! 1 = design calc; 2 = simulation calculation

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
      REAL(r64) WaterMassFlowRate
      REAL(r64) AirMassFlow  ! [kg/sec]
      REAL(r64) TempAirIn    ! [C]
      REAL(r64) TempAirOut   ! [C]
      REAL(r64) Win
      REAL(r64) TempWaterIn
      REAL(r64) TempWaterOut
      REAL(r64) UA
      REAL(r64) CapacitanceAir
      REAL(r64) CapacitanceWater
      REAL(r64) CapacitanceMin
      REAL(r64) CapacitanceMax
      REAL(r64) HeatingCoilLoad
      REAL(r64) NTU, ETA, A, CapRatio, E1, E2, Effec
      REAL(r64) Cp
      Integer Control

   UA           = WaterCoil(CoilNum)%UACoilVariable
   TempAirIn    = WaterCoil(CoilNum)%InletAirTemp
   Win          = WaterCoil(CoilNum)%InletAirHumRat
   Control      = WaterCoil(CoilNum)%Control
   TempWaterIn  = WaterCoil(CoilNum)%InletWaterTemp

!  adjust mass flow rates for cycling fan cycling coil operation
   IF(FanOpMode .EQ. CycFanCycCoil)THEN
     IF(PartLoadRatio .GT. 0.0d0)THEN
       AirMassFlow       = WaterCoil(CoilNum)%InletAirMassFlowRate/PartLoadRatio
       WaterMassFlowRate = MIN(WaterCoil(CoilNum)%InletWaterMassFlowRate/PartLoadRatio, &
                               WaterCoil(CoilNum)%MaxWaterMassFlowRate)
     ELSE
       AirMassFlow       = 0.0d0
       WaterMassFlowRate = 0.0d0
     END IF
   ELSE
     AirMassFlow       = WaterCoil(CoilNum)%InletAirMassFlowRate
     WaterMassFlowRate = WaterCoil(CoilNum)%InletWaterMassFlowRate
   END IF

  IF (WaterMassFlowRate .GT. MassFlowTolerance) THEN     ! If the coil is operating
    CapacitanceAir=PsyCpAirFnWTdb(Win,0.5d0*(TempAirIn+TempWaterIn))*AirMassFlow
    Cp =  GetSpecificHeatGlycol(PlantLoop(WaterCoil(CoilNum)%WaterLoopNum)%FluidName,  &
             TempWaterIn,                      &
             PlantLoop(WaterCoil(CoilNum)%WaterLoopNum)%FluidIndex, &
             'SizeWaterCoil')
    CapacitanceWater=Cp*WaterMassFlowRate
    CapacitanceMin=MIN(CapacitanceAir,CapacitanceWater)
    CapacitanceMax=MAX(CapacitanceAir,CapacitanceWater)
  Else
    CapacitanceAir=0.0d0
    CapacitanceWater=0.0d0
  End If

  ! If the coil is operating there should be some heating capacitance
  !  across the coil, so do the simulation. If not set outlet to inlet and no load.
  !  Also the coil has to be scheduled to be available
  IF(((CapacitanceAir .gt. 0.0d0).and.(CapacitanceWater .gt. 0.0d0)) .and.     &
      (GetCurrentScheduleValue(WaterCoil(CoilNum)%SchedPtr) .gt. 0.0d0 .or. MySizeFlag(CoilNum) .or. &
       MyUAAndFlowCalcFlag(CoilNum) .or. CalcMode == DesignCalc) ) Then

      IF (UA <= 0.0d0) THEN
        CALL ShowFatalError('UA is zero for COIL:Heating:Water '//TRIM(WaterCoil(CoilNum)%Name))
      END IF
      NTU=UA/CapacitanceMin
      ETA=NTU**0.22d0
      CapRatio=CapacitanceMin/CapacitanceMax
      A=CapRatio*NTU/ETA

      IF(A .GT. 20.0d0) Then
        A=ETA*1.0d0/CapRatio
      Else
        E1=EXP(-A)
        A=ETA*(1.d0-E1)/CapRatio
      End If

      IF(A.GT.20.d0) Then
        Effec=1.0d0
      Else
        E2=EXP(-A)
        Effec=1.d0-E2
      End IF

      TempAirOut=TempAirIn+Effec*CapacitanceMin*(TempWaterIn-TempAirIn)/CapacitanceAir
      TempWaterOut=TempWaterIn-CapacitanceAir*(TempAirOut-TempAirIn)/CapacitanceWater
      HeatingCoilLoad=CapacitanceWater*(TempWaterIn-TempWaterOut)
     !The HeatingCoilLoad is the change in the enthalpy of the water
      WaterCoil(CoilNum)%OutletWaterEnthalpy = WaterCoil(CoilNum)%InletWaterEnthalpy- &
        HeatingCoilLoad/WaterCoil(CoilNum)%InletWaterMassFlowRate
      WaterCoil(CoilNum)%OutletWaterMassFlowRate = WaterCoil(CoilNum)%InletWaterMassFlowRate

  ELSE    ! If not running Conditions do not change across coil from inlet to outlet

      TempAirOut=TempAirIn
      TempWaterOut =TempWaterIn
      HeatingCoilLoad=0.0d0
      WaterCoil(CoilNum)%OutletWaterEnthalpy = WaterCoil(CoilNum)%InletWaterEnthalpy
      WaterCoil(CoilNum)%OutletWaterMassFlowRate = 0.0d0
  ENDIF

   IF(FanOpMode .EQ. CycFanCycCoil)THEN
     HeatingCoilLoad = HeatingCoilLoad*PartLoadRatio
   END IF

  ! Set the outlet conditions
   WaterCoil(CoilNum)%TotWaterHeatingCoilRate = HeatingCoilLoad
   WaterCoil(CoilNum)%OutletAirTemp   = TempAirOut
   WaterCoil(CoilNum)%OutletWaterTemp = TempWaterOut

   ! This WaterCoil does not change the moisture or Mass Flow across the component
   WaterCoil(CoilNum)%OutletAirHumRat       = WaterCoil(CoilNum)%InletAirHumRat
   WaterCoil(CoilNum)%OutletAirMassFlowRate = WaterCoil(CoilNum)%InletAirMassFlowRate
   !Set the outlet enthalpys for air and water
   WaterCoil(CoilNum)%OutletAirEnthalpy = PsyHFnTdbW(WaterCoil(CoilNum)%OutletAirTemp, &
                                             WaterCoil(CoilNum)%OutletAirHumRat)

RETURN
END Subroutine CalcSimpleHeatingCoil



SUBROUTINE CalcDetailFlatFinCoolingCoil(CoilNum,CalcMode,FanOpMode,PartLoadRatio)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR(S)      Russell Taylor / Richard Liesen
          !       DATE WRITTEN   Mar 1997
          !       MODIFIED       Feb 2010, B. Nigusse, FSEC, corrected units inconsistency for tube and fins
          !                      materials thermal conductivties. Now input values in the idf are in {W/(m.K)}
          !       RE-ENGINEERED  Sept 1998

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine simulates a chilled water cooling coil.  Provided with
          ! the coil geometry and the flow (i.e. air and water) inlet conditions,
          ! it will calculate the flow outlet conditions and the total and latent
          ! heat extraction rates from the air.  The coil model has some limitations
          ! as noted in the code.

          ! METHODOLOGY EMPLOYED:
          ! successive substitution, solve coil as if all wet, then
          ! again if partly or entirely dry

          ! REFERENCES:
          ! First found in Type 12 from MODSIM, but now
          ! programmed directly from Elmahdy, A.H. and Mitalas, G.P.  "A
          ! Simple Model for Cooling and Dehumidifying Coils for Use in
          ! Calculating Energy Requirements for Buildings"  _ASHRAE
          ! Transactions_ Vol. 83, Part 2, pp. 103-117 (1977).

          ! OTHER NOTES:
          ! Routine was originally adapted for use in IBLAST by R.D. Taylor in l993.
          ! Subsequently rewritten and improved by J.C. Vanderzee in 1994
          ! Revised and further enanced by R.D. Taylor in Jan 1996
          ! Re-engineered for EnergyPlus by Richard Liesen PhD in 1998

          ! USE STATEMENTS:
  USE General, ONLY: TrimSigDigits

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
    INTEGER, INTENT(IN) :: CoilNum
    INTEGER, INTENT(IN) :: CalcMode
    INTEGER, INTENT(IN) :: FanOpMode      ! fan operating mode
    REAL(r64),    INTENT(IN) :: PartLoadRatio  ! part-load ratio of heating coil


          ! SUBROUTINE PARAMETER DEFINITIONS:
    INTEGER, PARAMETER :: MaxCoolCoilErrs = 5

    REAL(r64), PARAMETER :: AirViscosity = 1.846d-5      ! Dynamic Viscosity of Air in kg/(m.s)
    REAL(r64), PARAMETER :: ConvK = 1.0d-3               ! Unit conversion factor
    REAL(r64), PARAMETER :: unity = 1.0d0
    REAL(r64), PARAMETER :: zero = 0.0d0
    REAL(r64), PARAMETER :: TubeFoulFactor = 5.0d-2      ! Inside tube fouling factor for water, in m2K/kW
                                                         ! Changed from m2K/W to m2K/kW for consistency with the
                                                         ! other parameters in "TubeFoulThermResis" calculation

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    INTEGER :: CoefPointer
!    INTEGER :: CoolCoilErrs = 0
    INTEGER :: PartWetIterations
    INTEGER :: WaterTempConvgLoop

    LOGICAL :: CoilPartWetConvg
    LOGICAL :: WaterTempConvg

    REAL(r64) :: AirEnthAtRsdInletWaterTemp
    REAL(r64) :: AirExitEnthlAtCoilSurfTemp
    REAL(r64) :: AirExitCoilSurfTemp
    REAL(r64) :: AirReynoldsNo
    REAL(r64) :: AirEnthAtWetDryIntrfcSurfTemp
    REAL(r64) :: AirSideDrySurfFilmCoef
    REAL(r64) :: AirSideWetSurfFilmCoef
    REAL(r64) :: AirWetDryInterfcTemp
    REAL(r64) :: CoilToAirThermResistDrySurf
    REAL(r64) :: CoilToAirThermResistWetSurf
    REAL(r64) :: DryAirSpecHeat
    REAL(r64) :: DryCoilCoeff1
    REAL(r64) :: DryCoilCoeff
    REAL(r64) :: DryCoilEfficiency
    REAL(r64) :: DryFinEfficncy
    REAL(r64) :: DryCoilInThermResist
    REAL(r64) :: DrySideEffectiveWaterTemp
    REAL(r64) :: EnterAirDewPoint
    REAL(r64) :: EnterAirHumRatDiff
    REAL(r64) :: WetDryInterSurfTempErrorLast
    REAL(r64) :: WetDryInterSurfTempError
    REAL(r64) :: expon
    REAL(r64) :: FilmCoefEqnFactor
    REAL(r64) :: FilmCoefReynldsCorrelatnFact
    REAL(r64) :: FinToTotSurfAreaRatio
    REAL(r64) :: InCoilSurfTemp
    REAL(r64) :: InsdToOutsdThermResistRatio
    REAL(r64) :: InSurfTempSatAirEnthl
    REAL(r64) :: K1
    REAL(r64) :: MeanWaterTemp
    REAL(r64) :: MoistAirSpecificHeat
    REAL(r64) :: OutCoilSurfTemp
    REAL(r64) :: OutSurfTempSatAirEnthl
    REAL(r64) :: RaisedInletWaterTemp
    REAL(r64) :: RsdInletWaterTempSatAirHumRat
    REAL(r64) :: ScaledAirMassFlowRate
    REAL(r64) :: ScaledCoilAirThermResistWetSurf
    REAL(r64) :: ScaledWaterSpecHeat
    REAL(r64) :: ScaledWaterToTubeThermResist
    REAL(r64) :: SensToTotEnthDiffRatio
    REAL(r64) :: SurfAreaWet
    REAL(r64) :: TubeFoulThermResist
    REAL(r64) :: TubeWaterVel
    REAL(r64) :: UACoilAllWet
    REAL(r64) :: UACoilPartWet
    REAL(r64) :: UADryCoil
    REAL(r64) :: WaterToTubeThermResist
    REAL(r64) :: WetAreaChange
    REAL(r64) :: WetAreaLast
    REAL(r64) :: WetCoilCoeff
    REAL(r64) :: WetCoilFinEfficncy
    REAL(r64) :: WetDryInterfcAirEnthl
    REAL(r64) :: WetDryInterfcSurfTemp
    REAL(r64) :: WetDryInterfcWaterTemp
    REAL(r64) :: WetFinEfficncy
    REAL(r64) :: WetSideEffctvWaterTemp
    REAL(r64) :: y
    REAL(r64) :: TempAirIn
    REAL(r64) :: TempAirOut
    REAL(r64) :: InletAirHumRat
    REAL(r64) :: OutletAirHumRat
    REAL(r64) :: InletAirEnthalpy
    REAL(r64) :: OutletAirEnthalpy
    REAL(r64) :: WaterMassFlowRate
    REAL(r64) :: AirMassFlow
    REAL(r64) :: TempWaterIn
    REAL(r64) :: TempWaterOut
    REAL(r64) :: TotWaterCoilLoad
    REAL(r64) :: SenWaterCoilLoad
    REAL(r64) :: AirDensity
    REAL(r64) :: AirVelocity
    REAL(r64) :: denom
    REAL(r64) :: rho
    REAL(r64) :: Cp

   ! Set derived type variables to shorter local variables
    TempAirIn    = WaterCoil(CoilNum)%InletAirTemp
    InletAirHumRat = WaterCoil(CoilNum)%InletAirHumRat
    TempWaterIn  = WaterCoil(CoilNum)%InletWaterTemp

    !  adjust mass flow rates for cycling fan cycling coil operation
    IF(FanOpMode .EQ. CycFanCycCoil)THEN
      IF(PartLoadRatio .GT. 0.0d0)THEN
        AirMassFlow       = WaterCoil(CoilNum)%InletAirMassFlowRate/PartLoadRatio
        WaterMassFlowRate = MIN(WaterCoil(CoilNum)%InletWaterMassFlowRate/PartLoadRatio, &
                                WaterCoil(CoilNum)%MaxWaterMassFlowRate)
      ELSE
        AirMassFlow       = 0.0d0
        WaterMassFlowRate = 0.0d0
      END IF
    ELSE
      AirMassFlow       = WaterCoil(CoilNum)%InletAirMassFlowRate
      WaterMassFlowRate = WaterCoil(CoilNum)%InletWaterMassFlowRate
    END IF

        IF (WaterMassFlowRate.LT.WaterCoil(CoilNum)%MaxWaterMassFlowRate * MinWaterMassFlowFrac) THEN
        WaterMassFlowRate = 0.0d0
    END IF
    IF (TempAirIn.LE.TempWaterIn) THEN
        WaterMassFlowRate = 0.0d0
    END IF
    WetDryInterfcAirEnthl=0.0d0
    OutletAirEnthalpy=0.0d0
    InletAirEnthalpy=0.0d0

    !Warning and error messages for large flow rates for the given user input geometry
    AirDensity = PsyRhoAirFnPbTdbW(OutBaroPress,TempAirIn,InletAirHumRat, 'CalcDetailFlatFinCoolingCoil')
    IF(AirMassFlow > (5.0*WaterCoil(CoilNum)%MinAirFlowArea/AirDensity) .AND. CoilWarningOnceFlag(CoilNum)) THEN
       CALL ShowWarningError('Coil:Cooling:Water:DetailedGeometry in Coil ='//TRIM(WaterCoil(coilNum)%Name))
       CALL ShowContinueError('Air Flow Rate Velocity has greatly exceeded upper design guidelines of ~2.5 m/s')
       CALL ShowContinueError('Air MassFlowRate[kg/s]='//TRIM(TrimSigDigits(AirMassFlow,6)))
       AirVelocity=AirMassFlow*AirDensity/WaterCoil(CoilNum)%MinAirFlowArea
       CALL ShowContinueError('Air Face Velocity[m/s]='//TRIM(TrimSigDigits(AirVelocity,6)))
       CALL ShowContinueError('Approximate MassFlowRate limit for Face Area[kg/s]='  &
                              //TRIM(TrimSigDigits(2.5d0*WaterCoil(CoilNum)%MinAirFlowArea/AirDensity,6)))
       CALL ShowContinueError('Coil:Cooling:Water:DetailedGeometry could be resized/autosized to handle capacity')
       CoilWarningOnceFlag(CoilNum) = .False.
    Else IF(AirMassFlow > (44.7d0*WaterCoil(CoilNum)%MinAirFlowArea/AirDensity)) THEN
       CALL ShowSevereError('Coil:Cooling:Water:DetailedGeometry in Coil ='//TRIM(WaterCoil(coilNum)%Name))
       CALL ShowContinueError('Air Flow Rate Velocity is > 100MPH (44.7m/s) and simulation cannot continue')
       CALL ShowContinueError('Air Mass Flow Rate[kg/s]='//TRIM(TrimSigDigits(AirMassFlow,6)))
       AirVelocity=AirMassFlow*AirDensity/WaterCoil(CoilNum)%MinAirFlowArea
       CALL ShowContinueError('Air Face Velocity[m/s]='//TRIM(TrimSigDigits(AirVelocity,6)))
       CALL ShowContinueError('Approximate MassFlowRate limit for Face Area[kg/s]='  &
                              //TRIM(TrimSigDigits(2.5d0*WaterCoil(CoilNum)%MinAirFlowArea/AirDensity,6)))
       CALL ShowFatalError('Coil:Cooling:Water:DetailedGeometry needs to be resized/autosized to handle capacity')
    END IF

! If Coil is Scheduled ON then do the simulation
IF(((GetCurrentScheduleValue(WaterCoil(CoilNum)%SchedPtr) .gt. 0.0d0) .AND. (WaterMassFlowRate .GT. 0.0d0) &
    .AND. (AirMassFlow .GE. MinAirMassFlow)) .OR. (CalcMode == DesignCalc)) Then
    !        transfer inputs to simulation variables and calculate
    !        known thermodynamic functions
    ! All coil calcs are done in KJoules.  Convert to KJ here and then convert
    !  back to Joules at the end of the Subroutine.
    DryAirSpecHeat       = PsyCpAirFnWTdb(zero,TempAirIn, 'CalcDetailFlatFinCoolingCoil') * ConvK
    MoistAirSpecificHeat = PsyCpAirFnWTdb(InletAirHumRat, TempAirIn, 'CalcDetailFlatFinCoolingCoil') * ConvK
    InletAirEnthalpy     = WaterCoil(CoilNum)%InletAirEnthalpy * ConvK
    EnterAirDewPoint     = PsyTdpFnWPb(InletAirHumRat, OutBaroPress, 'CalcDetailFlatFinCoolingCoil')
    !
    !       Ratio of secondary (fin) to total (secondary plus primary) surface areas
    FinToTotSurfAreaRatio = WaterCoil(CoilNum)%FinSurfArea /   &
                            WaterCoil(CoilNum)%TotCoilOutsideSurfArea
    !
    !      known water and air flow parameters:
    !
    rho = GetDensityGlycol(PlantLoop(WaterCoil(CoilNum)%WaterLoopNum)%FluidName,  &
                           TempWaterIn,                      &
                           PlantLoop(WaterCoil(CoilNum)%WaterLoopNum)%FluidIndex, &
                          'CalcDetailFlatFinCoolingCoil')
    !      water flow velocity - assuming number of water circuits = NumOfTubesPerRow
    TubeWaterVel = WaterMassFlowRate * 4.d0 / (WaterCoil(CoilNum)%NumOfTubesPerRow *  &
                   rho * Pi * WaterCoil(CoilNum)%TubeInsideDiam *  &
                   WaterCoil(CoilNum)%TubeInsideDiam)
    !      air mass flow rate per unit area
    ScaledAirMassFlowRate = (1.d0 + InletAirHumRat) * AirMassFlow/WaterCoil(CoilNum)%MinAirFlowArea
    !      air flow Reynold's Number
    AirReynoldsNo = WaterCoil(CoilNum)%CoilEffectiveInsideDiam * ScaledAirMassFlowRate / AirViscosity
    !
    !       heat transfer coefficients and resistance components:
    !              inside (water)
    WaterToTubeThermResist = WaterCoil(CoilNum)%TubeInsideDiam**0.2d0/(WaterCoil(CoilNum)%TotTubeInsideArea &
                             * 1.429d0 * TubeWaterVel**0.8d0)
    !              metal and fouling
    TubeFoulThermResist = (0.5d0 * (WaterCoil(CoilNum)%TubeOutsideDiam - WaterCoil(CoilNum)%TubeInsideDiam) &
                             / (ConvK * WaterCoil(CoilNum)%TubeThermConductivity) + TubeFoulFactor) /       &
                             WaterCoil(CoilNum)%TotTubeInsideArea
    !
    !              outside (wet and dry coil)
    !
    FilmCoefEqnFactor = WaterCoil(CoilNum)%GeometryCoef1 * AirReynoldsNo**WaterCoil(CoilNum)%GeometryCoef2
    !       (1.23 is 1/Prandt(air)**(2/3))
    AirSideDrySurfFilmCoef = 1.23d0 * FilmCoefEqnFactor * MoistAirSpecificHeat * &
                                     ScaledAirMassFlowRate
    FilmCoefReynldsCorrelatnFact = 1.425d0 + AirReynoldsNo * (-0.51d-3 +  &
                                     AirReynoldsNo * 0.263d-6)
    !
    !       NOTE: the equation for FilmCoefReynldsCorrelatnFact generates valid results over
    !             a limited range of Air Reynolds Numbers as indicated by
    !             deleted code below.  Reynolds Numbers outside this range
    !             may result in inaccurate results or failure of the coil
    !             simulation to obtain a solution
    !
    !             Deleted code by J.C. Vanderzee
    !

    AirSideWetSurfFilmCoef = FilmCoefReynldsCorrelatnFact * AirSideDrySurfFilmCoef
    !--                     need wet fin efficiency for outside
    RaisedInletWaterTemp = TempWaterIn + 0.5d0

    ! By this statement the Inlet Air enthalpy will never be equal to AirEnthAtRsdInletWaterTemp
    If((RaisedInletWaterTemp-TempAirIn) .LT. 0.000001d0) Then
      RaisedInletWaterTemp=TempWaterIn+0.3d0
    End If
    IF (TempAirIn < RaisedInletWaterTemp) THEN
      RaisedInletWaterTemp=TempAirIn - .3d0
    ENDIF

    RsdInletWaterTempSatAirHumRat = PsyWFnTdbRhPb(RaisedInletWaterTemp, unity, OutBaroPress, 'CalcDetailFlatFinCoolingCoil')
    AirEnthAtRsdInletWaterTemp = PsyHFnTdbW(RaisedInletWaterTemp, &
                                    RsdInletWaterTempSatAirHumRat, 'CalcDetailFlatFinCoolingCoil') * ConvK

    SensToTotEnthDiffRatio = DryAirSpecHeat * (TempAirIn - RaisedInletWaterTemp) / &
                                  (InletAirEnthalpy - AirEnthAtRsdInletWaterTemp)

    EnterAirHumRatDiff = InletAirHumRat - RsdInletWaterTempSatAirHumRat
    DryFinEfficncy = 0.5d0 * (WaterCoil(CoilNum)%EffectiveFinDiam - WaterCoil(CoilNum)%TubeOutsideDiam) * &
                       SQRT(2.d0 * AirSideWetSurfFilmCoef / (ConvK * WaterCoil(CoilNum)%FinThermConductivity * &
                       WaterCoil(CoilNum)%FinThickness))
    IF (EnterAirHumRatDiff .LT. 0) THEN
    !       note that this condition indicates dry coil
       EnterAirHumRatDiff = -EnterAirHumRatDiff
       SensToTotEnthDiffRatio = abs(SensToTotEnthDiffRatio)
    END IF

    IF (EnterAirHumRatDiff > 1.0d0) EnterAirHumRatDiff =1.0d0
    IF (EnterAirHumRatDiff < 0.00001d0) EnterAirHumRatDiff =0.00001d0

    IF (DryFinEfficncy > 1.0d0) DryFinEfficncy=1.0d0
    IF (DryFinEfficncy < 0.00001d0) DryFinEfficncy=0.00001d0

    IF (TempAirIn .GT. 48.d0/1.8d0) THEN
       WetFinEfficncy=EXP(-0.41718d0) * SensToTotEnthDiffRatio**(0.09471d0) * &
                      EnterAirHumRatDiff**(0.0108d0) * DryFinEfficncy**(-0.50303d0)
    ELSE
       WetFinEfficncy=EXP(-0.3574d0) * SensToTotEnthDiffRatio**(0.16081d0) * &
                      EnterAirHumRatDiff**(0.01995d0) * DryFinEfficncy**(-0.52951d0)
    END IF

      IF (WetFinEfficncy > 1.0d0) WetFinEfficncy=0.99d0
      IF (WetFinEfficncy < 0.0d0) WetFinEfficncy=0.001d0
    !
    !       wet coil fin efficiency
    !

    WetCoilFinEfficncy = 1.d0 + FinToTotSurfAreaRatio * (WetFinEfficncy - 1.d0)
    !
    !       wet coil outside thermal resistance = [1/UA] (wet coil)
    !
    CoilToAirThermResistWetSurf = MoistAirSpecificHeat / (WaterCoil(CoilNum)%TotCoilOutsideSurfArea *  &
                                        AirSideWetSurfFilmCoef * WetCoilFinEfficncy)
    !--                     and dry fin efficiency
    DryFinEfficncy = 0.5d0 * (WaterCoil(CoilNum)%EffectiveFinDiam - WaterCoil(CoilNum)%TubeOutsideDiam) *  &
                       SQRT(2.d0 * AirSideDrySurfFilmCoef / (ConvK * WaterCoil(CoilNum)%FinThermConductivity * &
                       WaterCoil(CoilNum)%FinThickness))
    !
    !      NOTE: The same caveats on the validity of the FilmCoefReynldsCorrelatnFact equation
    !            hold for the DryFinEfficncy equation.  Values of DryFinEfficncy outside the
    !            specified range of validity are not guaranteed to
    !            produce results
    !
    !             Deleted code by J.C. Vanderzee
    !
    !
    !       dry coil fin efficiency
    !
    DryCoilEfficiency = 0.0d0
    DO CoefPointer=1,5
      DryCoilEfficiency = DryCoilEfficiency + WaterCoil(CoilNum)%DryFinEfficncyCoef(CoefPointer) *  &
                             DryFinEfficncy**(CoefPointer-1)
    END DO ! CoefPointer
    DryCoilEfficiency = 1.d0 + FinToTotSurfAreaRatio * (DryCoilEfficiency - 1.d0)
    !
    !       dry coil outside thermal resistance = [1/UA] (dry coil)
    !
    CoilToAirThermResistDrySurf = 1.d0 / (WaterCoil(CoilNum)%TotCoilOutsideSurfArea * AirSideDrySurfFilmCoef * &
                                  DryCoilEfficiency)
    !
    !       definitions made to simplify some of the expressions used below
    Cp =  GetSpecificHeatGlycol(PlantLoop(WaterCoil(CoilNum)%WaterLoopNum)%FluidName,  &
                       TempWaterIn,                      &
                       PlantLoop(WaterCoil(CoilNum)%WaterLoopNum)%FluidIndex, &
                       'CalcDetailFlatFinCoolingCoil')
    ScaledWaterSpecHeat = WaterMassFlowRate * Cp * ConvK /  &
                          AirMassFlow
    DryCoilCoeff1 = 1.0d0 / (AirMassFlow * MoistAirSpecificHeat) - 1.0d0/  &
                    (WaterMassFlowRate * Cp * ConvK)
    !
    !       perform initialisations for all wet solution
    !
    WetSideEffctvWaterTemp = WaterCoil(CoilNum)%MeanWaterTempSaved +   &
                             (TempWaterIn - WaterCoil(CoilNum)%InWaterTempSaved)
    WaterTempConvgLoop = 0
    WaterTempConvg = .FALSE.
    !
    !       Loop to solve coil as if all wet, converges on MeanWaterTemp eq WetSideEffctvWaterTemp
    !       if conv=.TRUE. at any time program exits loop and proceeds
    !       to part wet / part dry solution
    !
    DO WHILE (WaterTempConvgLoop .LT. 8 .and. .not. WaterTempConvg)
      WaterTempConvgLoop = WaterTempConvgLoop + 1
      ScaledWaterToTubeThermResist = WaterToTubeThermResist/(1.d0 + 0.0146d0 * &
                                     WetSideEffctvWaterTemp)
      ScaledCoilAirThermResistWetSurf = CoilToAirThermResistWetSurf / WaterCoil(CoilNum)%SatEnthlCurveSlope
      UACoilAllWet = 1.0d0 / (WaterCoil(CoilNum)%SatEnthlCurveSlope * (TubeFoulThermResist +  &
                     ScaledWaterToTubeThermResist + ScaledCoilAirThermResistWetSurf))
    !
    !       prevents floating point error when taking exponential
    !       of a very large number
    !
      expon = UACoilAllWet * (1.0d0 / AirMassFlow - WaterCoil(CoilNum)%SatEnthlCurveSlope / &
              (WaterMassFlowRate * Cp * ConvK))
      IF (expon < 20) THEN  !CR7189 changed from ABS(expon) < 20
        !       negative expon can happen, but lead to tiny WetCoilCoef that aren't a problem
        WetCoilCoeff = EXP(expon)
        ! following appears similar to eq. 320 in Eng Ref but neglects K1 term
        TempWaterOut =((1.0d0 - WetCoilCoeff) * (InletAirEnthalpy - WaterCoil(CoilNum)%SatEnthlCurveConstCoef) + &
                         WetCoilCoeff * TempWaterIn *   &
                        (WaterCoil(CoilNum)%SatEnthlCurveSlope - ScaledWaterSpecHeat)) / &
                        (WaterCoil(CoilNum)%SatEnthlCurveSlope - WetCoilCoeff * ScaledWaterSpecHeat)
      ELSE
        ! following appears to be same as above with equation simplified to use only significant terms when WetCoilCoeff very large
        TempWaterOut = ((InletAirEnthalpy - WaterCoil(CoilNum)%SatEnthlCurveConstCoef) -  &
                         TempWaterIn * (WaterCoil(CoilNum)%SatEnthlCurveSlope - &
                         ScaledWaterSpecHeat)) / ScaledWaterSpecHeat

      END IF
    !      above is inverted form of WaterMassFlowRate*cpw*(TempWaterOut-TempWaterIn) = UA(LMHD)
    !      note simplification that hsat = WaterCoil(CoilNum)%SatEnthlCurveConstCoef +  &
    !                                      WaterCoil(CoilNum)%SatEnthlCurveSlope*WetSideEffctvWaterTemp
      MeanWaterTemp = 0.5d0 * (TempWaterIn + TempWaterOut)
      OutletAirEnthalpy = InletAirEnthalpy - (TempWaterOut - TempWaterIn) * ScaledWaterSpecHeat

      InsdToOutsdThermResistRatio = (TubeFoulThermResist + ScaledWaterToTubeThermResist) / &
                                        ScaledCoilAirThermResistWetSurf
      InCoilSurfTemp = UACoilAllWet * ScaledCoilAirThermResistWetSurf * &
                           (WaterCoil(CoilNum)%SatEnthlCurveSlope * TempWaterIn + &
                           (OutletAirEnthalpy - WaterCoil(CoilNum)%SatEnthlCurveConstCoef) * &
                           InsdToOutsdThermResistRatio)
      OutCoilSurfTemp = UACoilAllWet * ScaledCoilAirThermResistWetSurf * &
                            (WaterCoil(CoilNum)%SatEnthlCurveSlope * TempWaterOut + &
                            (InletAirEnthalpy - WaterCoil(CoilNum)%SatEnthlCurveConstCoef) * &
                            InsdToOutsdThermResistRatio)

      IF (abs(MeanWaterTemp - WetSideEffctvWaterTemp) .GT. 0.01d0) THEN
         WetSideEffctvWaterTemp = MeanWaterTemp
         InSurfTempSatAirEnthl = PsyHFnTdbRhPb(InCoilSurfTemp, unity, OutBaroPress, 'CalcDetailFlatFinCoolingCoil') * ConvK
         OutSurfTempSatAirEnthl = PsyHFnTdbRhPb(OutCoilSurfTemp, unity, OutBaroPress, 'CalcDetailFlatFinCoolingCoil') *ConvK

         WaterCoil(CoilNum)%SatEnthlCurveSlope = (OutSurfTempSatAirEnthl - &
                            InSurfTempSatAirEnthl) / (OutCoilSurfTemp - &
                            InCoilSurfTemp)
         WaterCoil(CoilNum)%SatEnthlCurveConstCoef = InSurfTempSatAirEnthl - WaterCoil(CoilNum)%SatEnthlCurveSlope * &
                               InCoilSurfTemp
      ELSE
         WaterTempConvg = .TRUE.
      END IF
    END DO    ! End of iteration loop to get MeanWaterTemp=WetSideEffctvWaterTemp
    !
    !      if 8 CoolCoilErrs are reached without convergence and the
    !      predicted coil surface temperature at the outlet is less than
    !      the dew point coil is apparently all wet but a solution
    !      cannot be obtained
    !
    IF (.not. WaterTempConvg .and. .not. WarmupFlag .and. (OutCoilSurfTemp .LT. EnterAirDewPoint)) THEN
       CALL ShowRecurringWarningErrorAtEnd(TRIM(WaterCoil(CoilNum)%Name)//  &
            ' not converged (8 iterations) due to "Wet Convergence" conditions.',WaterTempCoolCoilErrs(CoilNum),  &
            abs(MeanWaterTemp - WetSideEffctvWaterTemp),abs(MeanWaterTemp - WetSideEffctvWaterTemp))
!       CoolCoilErrs = CoolCoilErrs + 1
!       IF (CoolCoilErrs .LE. MaxCoolCoilErrs) THEN
!          CALL ShowWarningError('tp12c0:  not converged in 8 CoolCoilErrs')
!       END IF
    END IF
    WaterCoil(CoilNum)%MeanWaterTempSaved = MeanWaterTemp
    !
    !      now simulate wet dry coil - test outlet condition from all
    !      wet case to give an idea of the expected solution
    !
    PartWetIterations = 0
    WetDryInterSurfTempError = 0.0d0
    CoilPartWetConvg = .FALSE.
    !
    !      Surface temp at coil water outlet (air inlet) is less than
    !      the dew point - Coil must be completely wet so no need to
    !      simulate wet/dry case
    !
    IF (OutCoilSurfTemp .LT. EnterAirDewPoint) THEN
      CoilPartWetConvg = .TRUE.
      WaterCoil(CoilNum)%SurfAreaWetFraction = 1.0d0
      TotWaterCoilLoad = AirMassFlow*(InletAirEnthalpy - OutletAirEnthalpy)
      AirWetDryInterfcTemp = TempAirIn
      WetDryInterfcAirEnthl = InletAirEnthalpy
    !
    !      Surface temperature at coil water inlet is greater than the
    !      dewpoint - coil cannot be all wet but may be all dry -
    !      initialise with all dry solution
    !
    ELSE IF (InCoilSurfTemp .GT. EnterAirDewPoint) THEN
      SurfAreaWet = 0.0d0
      WaterCoil(CoilNum)%SurfAreaWetFraction = 0.0d0
      WetDryInterfcWaterTemp = TempWaterIn
      TempWaterOut = WaterCoil(CoilNum)%OutWaterTempSaved + (TempWaterIn - WaterCoil(CoilNum)%InWaterTempSaved)
      WetAreaLast = 0.05d0 * WaterCoil(CoilNum)%TotCoilOutsideSurfArea
    !
    !      General case - must be part-wet/part-dry - initialise
    !      accordingly with some non-zero wet area
    !
    ELSE
      IF (WaterCoil(CoilNum)%SurfAreaWetSaved .ne. 0.0d0) THEN
         SurfAreaWet = WaterCoil(CoilNum)%SurfAreaWetSaved
      ELSE
         SurfAreaWet = 0.8d0 * WaterCoil(CoilNum)%TotCoilOutsideSurfArea * (EnterAirDewPoint - &
                        InCoilSurfTemp) / (OutCoilSurfTemp - InCoilSurfTemp)
      END IF
      WetDryInterfcWaterTemp = TempWaterIn + EnterAirDewPoint - &
                                 InCoilSurfTemp
      WetAreaLast = 0.0d0
    END IF
    !       Loop to solve partly wet coil, converges on wet area and
    !       boundary temperature at dew point
    !       Dry coil is special case with zero wet area, converges on
    !       mean water temperature
    DO WHILE (PartWetIterations .LT. 40 .and. .not. CoilPartWetConvg)
      PartWetIterations = PartWetIterations + 1
    !
    !      effective water temp on dry side of coil
    !
      DrySideEffectiveWaterTemp = 0.5d0 * (TempWaterOut + WetDryInterfcWaterTemp)
    !
    !      tube inside thermal resistance
    !
      DryCoilInThermResist = WaterToTubeThermResist / (1.0d0 + 0.0146d0 * DrySideEffectiveWaterTemp)
    !
    !      overall UA, from water to air, of dry portion of coil
    !

      UADryCoil = (WaterCoil(CoilNum)%TotCoilOutsideSurfArea - SurfAreaWet) / (WaterCoil(CoilNum)%TotCoilOutsideSurfArea *  &
              (TubeFoulThermResist + DryCoilInThermResist + CoilToAirThermResistDrySurf))

    ! This is a numerical trap for a very small number in the EXP function that is approaching zero
      If((UADryCoil*DryCoilCoeff1) .lt. -60.0d0) Then
         DryCoilCoeff = 0.0d0
      Else
         DryCoilCoeff = EXP(UADryCoil*DryCoilCoeff1)
      End If

      K1 = WaterMassFlowRate * Cp * ConvK * (DryCoilCoeff - 1.0d0) / &
           (WaterMassFlowRate * Cp * ConvK * DryCoilCoeff     &
           - AirMassFlow * MoistAirSpecificHeat)
      IF (SurfAreaWet .ne. 0) THEN
        WaterCoil(CoilNum)%SurfAreaWetFraction = SurfAreaWet / WaterCoil(CoilNum)%TotCoilOutsideSurfArea
    !
    !      effective water temp on wet side of coil
    !
        WetSideEffctvWaterTemp = 0.5d0 * (TempWaterIn + WetDryInterfcWaterTemp)
    !
    !      tube inside thermal resistance
    !
        ScaledWaterToTubeThermResist = WaterToTubeThermResist / (1.0d0 + 0.0146d0 * WetSideEffctvWaterTemp)
        ScaledCoilAirThermResistWetSurf = CoilToAirThermResistWetSurf / &
                                                  WaterCoil(CoilNum)%EnthVsTempCurveAppxSlope
    !
    !      overall UA, from water to air, of wet portion of coil
    !
        UACoilAllWet = 1.0d0 / (WaterCoil(CoilNum)%EnthVsTempCurveAppxSlope * (TubeFoulThermResist + &
                       ScaledWaterToTubeThermResist +   &
                       ScaledCoilAirThermResistWetSurf))
        UACoilPartWet = WaterCoil(CoilNum)%SurfAreaWetFraction * UACoilAllWet
        expon=UACoilPartWet * (1.0d0 / AirMassFlow - WaterCoil(CoilNum)%EnthVsTempCurveAppxSlope / &
              (WaterMassFlowRate * Cp * ConvK))
    !
    !        prevents floating point error when taking exponential
    !        of a very large number
    !
        IF (expon.LT.20) THEN
          WetCoilCoeff = EXP(expon)
!          write(outputfiledebug,*) ' wcc=',wetcoilcoeff
          denom=(WaterCoil(CoilNum)%EnthVsTempCurveAppxSlope -  &
                                      WetCoilCoeff * ScaledWaterSpecHeat - (1.0d0 - WetCoilCoeff) * K1 * &
                                     MoistAirSpecificHeat)
!          write(outputfiledebug,*) ' denom=',denom
!          WetDryInterfcWaterTemp = ((1.0 - WetCoilCoeff) * (InletAirEnthalpy - WaterCoil(CoilNum)%EnthVsTempCurveConst - K1 *  &
!                                     MoistAirSpecificHeat * TempAirIn) + WetCoilCoeff * &
!                                     TempWaterIn * (WaterCoil(CoilNum)%EnthVsTempCurveAppxSlope -  &
!                                     ScaledWaterSpecHeat)) / (WaterCoil(CoilNum)%EnthVsTempCurveAppxSlope -  &
!                                      WetCoilCoeff * ScaledWaterSpecHeat - (1.0 - WetCoilCoeff) * K1 * &
!                                     MoistAirSpecificHeat)
          WetDryInterfcWaterTemp = ((1.0d0 - WetCoilCoeff) * (InletAirEnthalpy - WaterCoil(CoilNum)%EnthVsTempCurveConst - K1 *  &
                                     MoistAirSpecificHeat * TempAirIn) + WetCoilCoeff * &
                                     TempWaterIn * (WaterCoil(CoilNum)%EnthVsTempCurveAppxSlope -  &
                                     ScaledWaterSpecHeat)) / denom
        ELSE
    !
    !         approximation to equation for WetDryInterfcWaterTemp when WetCoilCoeff-->inf.
    !
        WetDryInterfcWaterTemp = (TempWaterIn * (WaterCoil(CoilNum)%EnthVsTempCurveAppxSlope - &
                                 ScaledWaterSpecHeat) -  &
                                 (InletAirEnthalpy - WaterCoil(CoilNum)%EnthVsTempCurveConst - K1 *  &
                                 MoistAirSpecificHeat * TempAirIn)) &
                                 / (K1 * MoistAirSpecificHeat - ScaledWaterSpecHeat)
        END IF
      END IF
    !
    !        air temperature at wet-dry interface
    !
      AirWetDryInterfcTemp = TempAirIn - (TempAirIn - &
                                      WetDryInterfcWaterTemp) * K1
    !
    !        coil surface temperature at wet-dry interface
    !
      WetDryInterfcSurfTemp = WetDryInterfcWaterTemp + (AirWetDryInterfcTemp - &
                                WetDryInterfcWaterTemp) * (TubeFoulThermResist + &
                                DryCoilInThermResist) / (TubeFoulThermResist + DryCoilInThermResist + &
                                CoilToAirThermResistDrySurf)
      IF (SurfAreaWet .ne. 0) THEN
        WetDryInterfcAirEnthl = InletAirEnthalpy - MoistAirSpecificHeat * &
                                                  (TempAirIn - AirWetDryInterfcTemp)
    !
    !        conservation of energy - wet portion of coil
    !
        OutletAirEnthalpy = WetDryInterfcAirEnthl - WaterMassFlowRate * &
                                 Cp * ConvK * &
                                 (WetDryInterfcWaterTemp - TempWaterIn) / &
                                 AirMassFlow
    !
    !        ratio of inside to outside thermal resistance
    !
        InsdToOutsdThermResistRatio = (TubeFoulThermResist + &
                                                  ScaledWaterToTubeThermResist) / &
                                                  ScaledCoilAirThermResistWetSurf
    !
    !        coil surface temperature at water inlet (air outlet)
    !
        InCoilSurfTemp = UACoilAllWet * ScaledCoilAirThermResistWetSurf * &
                             (WaterCoil(CoilNum)%EnthVsTempCurveAppxSlope * TempWaterIn + (OutletAirEnthalpy - &
                             WaterCoil(CoilNum)%EnthVsTempCurveConst) * &
                             InsdToOutsdThermResistRatio)
        WetDryInterSurfTempErrorLast = WetDryInterSurfTempError
    !
    !        in part-wet/part-dry solution EnterAirDewPoint=WetDryInterfcSurfTemp drives WetDryInterSurfTempError->0
    !
        WetDryInterSurfTempError = EnterAirDewPoint - WetDryInterfcSurfTemp
      ELSE
    !
    !        dry coil solution
    !
        WetDryInterfcAirEnthl = 0.0d0
        OutletAirEnthalpy = InletAirEnthalpy - MoistAirSpecificHeat * &
                                         (TempAirIn - AirWetDryInterfcTemp)
      END IF
    !
    !        total cooling = change in air enmthalpy across coil
    !
      TotWaterCoilLoad = AirMassFlow * (InletAirEnthalpy - &
                            OutletAirEnthalpy)
    !
    !        conservation of energy on water stream gives water outlet
    !        temperature
    !
      TempWaterOut = WaterMassFlowRate * Cp * ConvK  ! Temp for next calc
      TempWaterOut = MIN(TempWaterIn + TotWaterCoilLoad/TempWaterOut, TempAirIn)
    !
    !        update estimate of coil wet area
    !

      IF (SurfAreaWet .EQ. 0) THEN
        MeanWaterTemp = 0.5d0 * (TempWaterOut + WetDryInterfcWaterTemp)
        IF (EnterAirDewPoint .GT. WetDryInterfcSurfTemp) THEN
          SurfAreaWet = 0.5d0 * WetAreaLast
        ELSE IF (ABS(MeanWaterTemp - DrySideEffectiveWaterTemp) .LE. .00002d0) THEN
            CoilPartWetConvg = .TRUE.
        END IF
      ELSE IF(ABS(WetDryInterSurfTempError) .GT. .00002d0 .OR. ABS(SurfAreaWet - WetAreaLast) / &
              WaterCoil(CoilNum)%TotCoilOutsideSurfArea .GT. .00001d0)THEN
        IF (WetAreaLast .EQ. 0) THEN
          WetAreaLast = SurfAreaWet
          SurfAreaWet = SurfAreaWet + 0.4d0 * WaterCoil(CoilNum)%TotCoilOutsideSurfArea * WetDryInterSurfTempError / &
                        (OutCoilSurfTemp - InCoilSurfTemp)
        ELSE IF (WetDryInterSurfTempError .ne. WetDryInterSurfTempErrorLast) THEN
          WetAreaChange = SurfAreaWet - WetAreaLast
          WetAreaLast = SurfAreaWet
          SurfAreaWet = SurfAreaWet - 0.8d0 * WetDryInterSurfTempError * WetAreaChange / &
                        (WetDryInterSurfTempError - WetDryInterSurfTempErrorLast)
        END IF
        IF (SurfAreaWet .GE. WaterCoil(CoilNum)%TotCoilOutsideSurfArea) THEN
          SurfAreaWet = WaterCoil(CoilNum)%TotCoilOutsideSurfArea
          MeanWaterTemp = 0.5d0 * (TempWaterIn + WetDryInterfcWaterTemp)
          IF (WetAreaLast .EQ. WaterCoil(CoilNum)%TotCoilOutsideSurfArea .and. abs(MeanWaterTemp - &
              WetSideEffctvWaterTemp) .LE. .00002d0) THEN
            CoilPartWetConvg = .TRUE.
          END IF
        END IF
        IF (SurfAreaWet .LE. 0) THEN
          SurfAreaWet = 0.0d0
          WaterCoil(CoilNum)%SurfAreaWetFraction = 0.0d0
          WetDryInterfcWaterTemp = TempWaterIn
        END IF
        InSurfTempSatAirEnthl = PsyHFnTdbRhPb(InCoilSurfTemp, unity, OutBaroPress, 'CalcDetailFlatFinCoolingCoil') * ConvK
        IF ((EnterAirDewPoint - InCoilSurfTemp) .GE. .0001d0) THEN
          AirEnthAtWetDryIntrfcSurfTemp = PsyHFnTdbRhPb(EnterAirDewPoint, unity, OutBaroPress, &
                                                               'CalcDetailFlatFinCoolingCoil')*ConvK
          WaterCoil(CoilNum)%EnthVsTempCurveAppxSlope = (AirEnthAtWetDryIntrfcSurfTemp - &
                                             InSurfTempSatAirEnthl) / &
                                             (EnterAirDewPoint - InCoilSurfTemp)
        ELSE
           AirEnthAtWetDryIntrfcSurfTemp = PsyHFnTdbRhPb(InCoilSurfTemp + 0.0001d0, unity, &
                                               OutBaroPress, 'CalcDetailFlatFinCoolingCoil') * ConvK
           WaterCoil(CoilNum)%EnthVsTempCurveAppxSlope = (AirEnthAtWetDryIntrfcSurfTemp - &
                                      InSurfTempSatAirEnthl) / 0.0001d0
        END IF
        WaterCoil(CoilNum)%EnthVsTempCurveConst = InSurfTempSatAirEnthl - WaterCoil(CoilNum)%EnthVsTempCurveAppxSlope * &
                                       InCoilSurfTemp
      ELSE
         CoilPartWetConvg = .TRUE.
      END IF
    END DO
    !
    !      error checking to see if convergence has been achieved
    !
    IF (.not. CoilPartWetConvg .and. .not. WarmupFlag) THEN
       CALL ShowRecurringWarningErrorAtEnd(TRIM(WaterCoil(CoilNum)%Name)//  &
            ' not converged (40 iterations) due to "Partial Wet Convergence" conditions.',PartWetCoolCoilErrs(CoilNum))
!      CoolCoilErrs = CoolCoilErrs + 1
!      IF (CoolCoilErrs .LE. MaxCoolCoilErrs) THEN
!        CALL ShowWarningError('tp12c0:  not converged in 20 CoolCoilErrs')
!      END IF
    END IF
    IF (WaterCoil(CoilNum)%SurfAreaWetFraction .GT. 0 .and. WaterCoil(CoilNum)%SurfAreaWetFraction .LT. 1) THEN
      WaterCoil(CoilNum)%SurfAreaWetSaved= SurfAreaWet
    END IF
    !
    !       calculate TempAirOut, OutletAirHumRat, and SensCoolRate based on equations from
    !       TYPE12 and the ASHRAE toolkit
    !
    IF (WaterCoil(CoilNum)%SurfAreaWetFraction .EQ. 0) THEN
    !
    !       dry coil
    !
      TempAirOut = TempAirIn - TotWaterCoilLoad / (AirMassFlow * &
                                MoistAirSpecificHeat)
      OutletAirHumRat = InletAirHumRat
      SenWaterCoilLoad = TotWaterCoilLoad
    ELSE
    !
    !       coil effectiveness
      expon = WaterCoil(CoilNum)%SurfAreaWetFraction / (CoilToAirThermResistWetSurf * AirMassFlow)
      y = 0.0d0
      IF (expon .LT. 20.d0) y=EXP(-expon)
      AirExitEnthlAtCoilSurfTemp = WetDryInterfcAirEnthl - (WetDryInterfcAirEnthl - &
                                   OutletAirEnthalpy) / (1.0d0 - y)
      AirExitCoilSurfTemp=AirExitEnthlAtCoilSurfTemp/ConvK  ! TEmporary calc
      AirExitCoilSurfTemp=PsyTsatFnHPb(AirExitCoilSurfTemp,OutBaroPress)
    !
    !       Implementation of epsilon*NTU method
      TempAirOut = AirExitCoilSurfTemp + (AirWetDryInterfcTemp - AirExitCoilSurfTemp) * y
      OutletAirHumRat = PsyWFnTdbH(TempAirOut, 1000.d0 * OutletAirEnthalpy, 'CalcDetailFlatFinCoolingCoil')
      SenWaterCoilLoad = AirMassFlow * (PsyCpAirFnWTdb(InletAirHumRat, TempAirIn, 'CalcDetailFlatFinCoolingCoil') * TempAirIn - &
                            PsyCpAirFnWTdb(OutletAirHumRat, TempAirOut, 'CalcDetailFlatFinCoolingCoil') * &
                            TempAirOut) * ConvK
    END IF

   IF(FanOpMode .EQ. CycFanCycCoil)THEN
     TotWaterCoilLoad = TotWaterCoilLoad*PartLoadRatio
     SenWaterCoilLoad = SenWaterCoilLoad*PartLoadRatio
   END IF


   ! Set the outlet conditions
  WaterCoil(CoilNum)%TotWaterCoolingCoilRate  = TotWaterCoilLoad * 1000.0d0
  WaterCoil(CoilNum)%SenWaterCoolingCoilRate  = SenWaterCoilLoad * 1000.0d0
  WaterCoil(CoilNum)%OutletAirTemp     = TempAirOut
  WaterCoil(CoilNum)%OutletWaterTemp   = TempWaterOut
  WaterCoil(CoilNum)%OutletAirEnthalpy = OutletAirEnthalpy * 1000.0d0
  WaterCoil(CoilNum)%OutletAirHumRat   = OutletAirHumRat
   !The CoolingCoilLoad is the change in the enthalpy of the water
  WaterCoil(CoilNum)%OutletWaterEnthalpy = WaterCoil(CoilNum)%InletWaterEnthalpy + &
        WaterCoil(CoilNum)%TotWaterCoolingCoilRate/WaterCoil(CoilNum)%InletWaterMassFlowRate

   !This WaterCoil does not change the Mass Flow across the component
  WaterCoil(CoilNum)%OutletAirMassFlowRate = WaterCoil(CoilNum)%InletAirMassFlowRate
  WaterCoil(CoilNum)%OutletWaterMassFlowRate = WaterCoil(CoilNum)%InletWaterMassFlowRate
Else
! If Coil is scheduled OFF then Outlet conditions are set to Inlet Conditions
  WaterCoil(CoilNum)%TotWaterCoolingCoilRate  = 0.0d0
  WaterCoil(CoilNum)%SenWaterCoolingCoilRate  = 0.0d0
  TempAirOut=TempAirIn
  TempWaterOut =TempWaterIn
! set the outlet conditions to the coil derived type
  WaterCoil(CoilNum)%OutletAirTemp     = TempAirOut
  WaterCoil(CoilNum)%OutletWaterTemp   = TempWaterOut
  WaterCoil(CoilNum)%OutletAirEnthalpy = WaterCoil(CoilNum)%InletAirEnthalpy
  WaterCoil(CoilNum)%OutletAirHumRat   = WaterCoil(CoilNum)%InletAirHumRat
!The CoolingCoilLoad is the change in the enthalpy of the water
  WaterCoil(CoilNum)%OutletWaterEnthalpy = WaterCoil(CoilNum)%InletWaterEnthalpy

!This WaterCoil does not change the Mass Flow across the component
  WaterCoil(CoilNum)%OutletAirMassFlowRate = WaterCoil(CoilNum)%InletAirMassFlowRate
  WaterCoil(CoilNum)%OutletWaterMassFlowRate = 0.0d0
End If

  !Save some of the Values for next Time step
  WaterCoil(CoilNum)%InWaterTempSaved = TempWaterIn
  WaterCoil(CoilNum)%OutWaterTempSaved = TempWaterOut

    RETURN
END SUBROUTINE CalcDetailFlatFinCoolingCoil


SUBROUTINE CoolingCoil(CoilNum, FirstHVACIteration, CalcMode,FanOpMode,PartLoadRatio)

          ! FUNCTION INFORMATION:
          ! AUTHOR         Rahul Chillar
          ! DATE WRITTEN   Mar 2004
          ! MODIFIED       na
          ! RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! The subroutine has the coil logic. Three types of Cooling Coils exist:
          ! They are 1.CoilDry , 2.CoilWet, 3. CoilPartDryPartWet. The logic for
          ! the three individual cases is in this subroutine.

          ! METHODOLOGY EMPLOYED:
          ! Simulates a Coil Model from Design conditions and subsequently uses
          ! configuration values (example: UA)calculated from those design conditions
          ! to calculate new performance of coil from operating inputs.The values are
          ! calculated in the Subroutine InitWaterCoil

          ! REFERENCES:
          ! ASHRAE Secondary HVAC Toolkit TRNSYS.  1990.  A Transient System
          ! Simulation Program: Reference Manual. Solar Energy Laboratory, Univ. Wisconsin-
          ! Madison, pp. 4.6.8-1 - 4.6.8-12.
          ! Threlkeld, J.L.  1970.  Thermal Environmental Engineering, 2nd Edition,
          ! Englewood Cliffs: Prentice-Hall,Inc. pp. 254-270.

          ! USE STATEMENTS:
        USE General, ONLY: SafeDivide

          ! Enforce explicit typing of all variables in this routine
        IMPLICIT NONE

          ! FUNCTION ARGUMENT DEFINITIONS:
        Integer, intent(in) :: CoilNum
        Logical, intent(in) :: FirstHVACIteration
        Integer, intent(in) :: CalcMode
        INTEGER, INTENT(IN) :: FanOpMode      ! fan operating mode
        REAL(r64),    INTENT(IN) :: PartLoadRatio  ! part-load ratio of heating coil

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
      REAL(r64)     :: AirInletCoilSurfTemp  ! Coil surface temperature at air entrance(C)
      REAL(r64)     :: AirDewPointTemp       ! Temperature dew point at operating condition
      REAL(r64)     :: OutletAirTemp         ! Outlet air temperature at operating condition
      REAL(r64)     :: OutletAirHumRat       ! Outlet air humidity ratio at operating condition
      REAL(r64)     :: OutletWaterTemp       ! Outlet water temperature at operating condtitons
      REAL(r64)     :: TotWaterCoilLoad      ! Total heat transfer rate(W)
      REAL(r64)     :: SenWaterCoilLoad      ! Sensible heat transfer rate
      REAL(r64)     :: SurfAreaWetFraction   ! Fraction of surface area wet
      REAL(r64)     :: AirMassFlowRate       ! Air mass flow rate for the calculation

     AirInletCoilSurfTemp=0.0d0  ! Coil surface temperature at air entrance(C)
     AirDewPointTemp=0.0d0       ! Temperature dew point at operating condition
     OutletAirTemp=0.0d0         ! Outlet air temperature at operating condition
     OutletAirHumRat=0.0d0       ! Outlet air humidity ratio at operating condition
     OutletWaterTemp=0.0d0       ! Outlet water temperature at operating condtitons
     TotWaterCoilLoad=0.0d0      ! Total heat transfer rate(W)
     SenWaterCoilLoad=0.0d0      ! Sensible heat transfer rate
     SurfAreaWetFraction=0.0d0   ! Fraction of surface area wet

     IF (FanOpMode == CycFanCycCoil .and. PartLoadRatio > 0.0d0) THEN  !FB Start
       AirMassFlowRate = WaterCoil(CoilNum)%InletAirMassFlowRate / PartLoadRatio
     ELSE
       AirMassFlowRate = WaterCoil(CoilNum)%InletAirMassFlowRate
     END IF

      ! If Coil is Scheduled ON then do the simulation
  IF(((GetCurrentScheduleValue(WaterCoil(CoilNum)%SchedPtr) .gt. 0.0d0)     &
   .AND. (WaterCoil(CoilNum)%InletWaterMassFlowRate .GT. 0.0d0)            &
   .AND. (AirMassFlowRate .GE. MinAirMassFlow)                           &
   .AND. (WaterCoil(CoilNum)%DesAirVolFlowRate .gt. 0.0d0)                 &
   .AND. (WaterCoil(CoilNum)%MaxWaterMassFlowRate .gt. 0.0d0)) .OR. (CalcMode == DesignCalc))THEN

        !Calculate Temperature Dew Point at operating conditions.
      AirDewPointTemp= PsyTdpFnWPb(WaterCoil(CoilNum)%InletAirHumRat,OutBaroPress)


     Select Case(WaterCoil(CoilNum)%CoolingCoilAnalysisMode)
      Case(DetailedAnalysis)
        !Coil is completely dry if AirDewPointTemp is less than InletWaterTemp,hence Call CoilCompletelyDry
        IF (AirDewPointTemp .LE. WaterCoil(CoilNum)%InletWaterTemp) THEN

            !Calculate the leaving conditions and performance of dry coil
          CALL CoilCompletelyDry (CoilNum,WaterCoil(CoilNum)%InletWaterTemp,WaterCoil(CoilNum)%InletAirTemp,&
                                                WaterCoil(CoilNum)%UACoilTotal, OutletWaterTemp,          &
                                                OutletAirTemp,OutletAirHumRat,TotWaterCoilLoad, FanOpMode,PartLoadRatio )

          SenWaterCoilLoad = TotWaterCoilLoad
          SurfAreaWetFraction = 0.0d0

        ELSE
         !Else If AirDewPointTemp is greater than InletWaterTemp then assume the
         !external surface of coil is completely wet,hence Call CoilCompletelyWet
         !Calculate the leaving conditions and performance of wet coil
          CALL CoilCompletelyWet (CoilNum,WaterCoil(CoilNum)%InletWaterTemp,                            &
                                 WaterCoil(CoilNum)%InletAirTemp,WaterCoil(CoilNum)%InletAirHumRat,   &
                                 WaterCoil(CoilNum)%UACoilInternal,WaterCoil(CoilNum)%UACoilExternal, &
                                 OutletWaterTemp,OutletAirTemp,OutletAirHumRat,TotWaterCoilLoad,      &
                                 SenWaterCoilLoad,SurfAreaWetFraction,AirInletCoilSurfTemp, FanOpMode, PartLoadRatio)


           !If AirDewPointTemp is less than temp of coil surface at entry of air
          IF (AirDewPointTemp .LT. AirInletCoilSurfTemp) THEN

             !Then coil is partially wet and dry hence call CoilPartWetPartDry
             !Calculate the leaving conditions and performance of dry coil
              CALL CoilPartWetPartDry (CoilNum,FirstHVACIteration,WaterCoil(CoilNum)%InletWaterTemp,            &
                                     WaterCoil(CoilNum)%InletAirTemp,                                           &
                                     AirDewPointTemp,OutletWaterTemp,OutletAirTemp,OutletAirHumRat,             &
                                     TotWaterCoilLoad,SenWaterCoilLoad,SurfAreaWetFraction,FanOpMode,PartLoadRatio)

           ENDIF  !End if for part wet part dry coil
        ENDIF  !End if for dry coil

      Case(SimpleAnalysis)
        !Coil is completely dry if AirDewPointTemp is less than InletWaterTemp,hence Call CoilCompletelyDry
        IF (AirDewPointTemp .LE. WaterCoil(CoilNum)%InletWaterTemp) THEN

            !Calculate the leaving conditions and performance of dry coil
          CALL CoilCompletelyDry (CoilNum,WaterCoil(CoilNum)%InletWaterTemp,WaterCoil(CoilNum)%InletAirTemp,&
                                                WaterCoil(CoilNum)%UACoilTotal, OutletWaterTemp,          &
                                                OutletAirTemp,OutletAirHumRat,TotWaterCoilLoad, FanOpMode, PartLoadRatio )

          SenWaterCoilLoad = TotWaterCoilLoad
          SurfAreaWetFraction = 0.0d0

        ELSE
         !Else If AirDewPointTemp is greater than InletWaterTemp then assume the
         !external surface of coil is completely wet,hence Call CoilCompletelyWet
         !Calculate the leaving conditions and performance of wet coil
          CALL CoilCompletelyWet (CoilNum,WaterCoil(CoilNum)%InletWaterTemp,                            &
                                 WaterCoil(CoilNum)%InletAirTemp,WaterCoil(CoilNum)%InletAirHumRat,   &
                                 WaterCoil(CoilNum)%UACoilInternal,WaterCoil(CoilNum)%UACoilExternal, &
                                 OutletWaterTemp,OutletAirTemp,OutletAirHumRat,TotWaterCoilLoad,      &
                                 SenWaterCoilLoad,SurfAreaWetFraction,AirInletCoilSurfTemp, FanOpMode, PartLoadRatio)

        ENDIF  !End if for dry coil

     End Select

         ! Report outlet variables at nodes
       WaterCoil(CoilNum)%OutletAirTemp = OutletAirTemp
       WaterCoil(CoilNum)%OutletAirHumRat=OutletAirHumRat
       WaterCoil(CoilNum)%OutletWaterTemp=OutletWaterTemp
         !Report output results if the coil was operating

       IF(FanOpMode .EQ. CycFanCycCoil)THEN
         TotWaterCoilLoad = TotWaterCoilLoad*PartLoadRatio
         SenWaterCoilLoad = SenWaterCoilLoad*PartLoadRatio
       END IF

       WaterCoil(CoilNum)%TotWaterCoolingCoilRate=TotWaterCoilLoad
       WaterCoil(CoilNum)%SenWaterCoolingCoilRate=SenWaterCoilLoad
       WaterCoil(CoilNum)%SurfAreaWetFraction=SurfAreaWetFraction
!       WaterCoil(CoilNum)%OutletWaterEnthalpy = WaterCoil(CoilNum)%InletWaterEnthalpy+ &
!                                WaterCoil(CoilNum)%TotWaterCoolingCoilRate/WaterCoil(CoilNum)%InletWaterMassFlowRate
       WaterCoil(CoilNum)%OutletWaterEnthalpy = WaterCoil(CoilNum)%InletWaterEnthalpy+ &
                                SafeDivide(WaterCoil(CoilNum)%TotWaterCoolingCoilRate,WaterCoil(CoilNum)%InletWaterMassFlowRate)

  ELSE
        !If both mass flow rates are zero, set outputs to inputs and return
        WaterCoil(CoilNum)%OutletWaterTemp = WaterCoil(CoilNum)%InletWaterTemp
        WaterCoil(CoilNum)%OutletAirTemp   = WaterCoil(CoilNum)%InletAirTemp
        WaterCoil(CoilNum)%OutletAirHumRat = WaterCoil(CoilNum)%InletAirHumRat
        WaterCoil(CoilNum)%OutletWaterEnthalpy = WaterCoil(CoilNum)%InletWaterEnthalpy
        WaterCoil(CoilNum)%TotWaterCoolingCoilEnergy=0.0d0
        WaterCoil(CoilNum)%SenWaterCoolingCoilEnergy=0.0d0
        WaterCoil(CoilNum)%SurfAreaWetFraction=0.0d0

  ENDIF  !End of the Flow or No flow If block
       WaterCoil(CoilNum)%OutletWaterMassFlowRate = WaterCoil(CoilNum)%InletWaterMassFlowRate
       WaterCoil(CoilNum)%OutletAirMassFlowRate = WaterCoil(CoilNum)%InletAirMassFlowRate
       WaterCoil(CoilNum)%OutletAirEnthalpy = PsyHFnTdbW(WaterCoil(CoilNum)%OutletAirTemp,WaterCoil(CoilNum)%OutletAirHumRat)


    RETURN
End Subroutine CoolingCoil
! End Algorithm Section of the Module


! Coil Completely Dry Subroutine for Cooling Coil
SUBROUTINE CoilCompletelyDry (CoilNum,WaterTempIn, AirTempIn,CoilUA,  &
                                    OutletWaterTemp,OutletAirTemp,OutletAirHumRat,Q,FanOpMode,PartLoadRatio)

          ! FUNCTION INFORMATION:
          ! AUTHOR         Rahul Chillar
          ! DATE WRITTEN   March 2004
          ! MODIFIED       na
          ! RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! Calculate the performance of a sensible air-liquid heat exchanger.  Calculated
          ! results include outlet air temperature and humidity, outlet water temperature,
          ! and heat transfer rate.


          ! METHODOLOGY EMPLOYED:
          ! Models coil using effectiveness-NTU model.

          ! REFERENCES:
          ! Kays, W.M. and A.L. London.  1964,Compact Heat Exchangers, 2nd Edition,
          ! New York: McGraw-Hill.

          ! USE STATEMENTS:
          ! na

          ! Enforce explicit typing of all variables in this routine
       Implicit None

          ! FUNCTION ARGUMENT DEFINITIONS:
       Integer, intent(in) :: CoilNum      !
       REAL(r64), intent(in)    :: WaterTempIn  ! Entering water temperature
       REAL(r64), intent(in)    :: AirTempIn    ! Entering air dry bulb temperature
       REAL(r64), intent(in)    :: CoilUA       ! Overall heat transfer coefficient
       INTEGER, INTENT(IN) :: FanOpMode      ! fan operating mode
       REAL(r64),    INTENT(IN) :: PartLoadRatio  ! part-load ratio of heating coil

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
       REAL(r64) :: OutletWaterTemp   ! Leaving water temperature
       REAL(r64) :: OutletAirTemp     ! Leaving air dry bulb temperature
       REAL(r64) :: OutletAirHumRat   ! Leaving air humidity ratio
       REAL(r64) :: Q                 ! Heat transfer rate
       REAL(r64) :: CapacitanceAir    ! Air-side capacity rate(W/C)
       REAL(r64) :: CapacitanceWater  ! Water-side capacity rate(W/C)
       REAL(r64) :: AirMassFlow
       REAL(r64) :: WaterMassFlowRate
       REAL(r64) :: Cp

       !  adjust mass flow rates for cycling fan cycling coil operation
       IF(FanOpMode .EQ. CycFanCycCoil)THEN
         IF(PartLoadRatio .GT. 0.0d0)THEN
           AirMassFlow       = WaterCoil(CoilNum)%InletAirMassFlowRate/PartLoadRatio
           WaterMassFlowRate = MIN(WaterCoil(CoilNum)%InletWaterMassFlowRate/PartLoadRatio, &
                               WaterCoil(CoilNum)%MaxWaterMassFlowRate)
         ELSE
           AirMassFlow       = 0.0d0
           WaterMassFlowRate = 0.0d0
         END IF
       ELSE
         AirMassFlow       = WaterCoil(CoilNum)%InletAirMassFlowRate
         WaterMassFlowRate = WaterCoil(CoilNum)%InletWaterMassFlowRate
       END IF

        ! Calculate air and water capacity rates
      CapacitanceAir = AirMassFlow*    &
                              PsyCpAirFnWTdb(WaterCoil(CoilNum)%InletAirHumRat,WaterCoil(CoilNum)%InletAirTemp)
        ! Water Capacity Rate
      Cp =  GetSpecificHeatGlycol(PlantLoop(WaterCoil(CoilNum)%WaterLoopNum)%FluidName,  &
                               WaterTempIn,                      &
                              PlantLoop(WaterCoil(CoilNum)%WaterLoopNum)%FluidIndex, &
                              'CoilCompletelyDry')

      CapacitanceWater = WaterMassFlowRate* Cp

        ! Determine the air and water outlet conditions
      CALL CoilOutletStreamCondition(CoilNum, CapacitanceWater,WaterTempIn,CapacitanceAir,AirTempIn,CoilUA, &
                                OutletWaterTemp,OutletAirTemp)

        ! Calculate the total and sensible heat transfer rate both are equal in case of Dry Coil
      Q=CapacitanceAir*(AirTempIn-OutletAirTemp)

        ! Outlet humidity is equal to Inlet Humidity because its a dry coil
      OutletAirHumRat = WaterCoil(CoilNum)%InletAirHumRat

  RETURN
END SUBROUTINE  CoilCompletelyDry



! Coil Completely Wet Subroutine for Cooling Coil
SUBROUTINE CoilCompletelyWet (CoilNum,WaterTempIn, AirTempIn,AirHumRat,UAInternalTotal,UAExternalTotal, &
                                   OutletWaterTemp,OutletAirTemp,OutletAirHumRat,TotWaterCoilLoad,      &
                                   SenWaterCoilLoad,SurfAreaWetFraction,AirInletCoilSurfTemp,FanOpMode,PartLoadRatio)

          ! FUNCTION INFORMATION:
          ! AUTHOR         Rahul Chillar
          ! DATE WRITTEN   Mar 2004
          ! MODIFIED       na
          ! RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! Calculate the performance of a cooling coil when the external fin surface is
          ! complete wet.  Results include outlet air temperature and humidity,
          ! outlet water temperature, sensible and total cooling capacities, and the wet
          ! fraction of the air-side surface area.

          ! METHODOLOGY EMPLOYED:
          ! Models coil as counterflow heat exchanger. Approximates saturated air enthalpy as
          ! a linear function of temperature
          ! TRNSYS.  1990.  A Transient System Simulation Program: Reference Manual.
          ! Solar Energy Laboratory, Univ. Wisconsin Madison, pp. 4.6.8-1 - 4.6.8-12.
          ! Threlkeld, J.L.  1970.  Thermal Environmental Engineering, 2nd Edition,
          ! Englewood Cliffs: Prentice-Hall,Inc. pp. 254-270.
          ! Coil Uses Enthalpy Based Heat Transfer Coefficents and converts them to
          ! convential UA values. Intermediate value of fictitious Cp is defined. This follow
          ! the same procedure followed in the Design Calculation of the Coil. See the node in
          ! the one time calculation for design condition.

          ! REFERENCES:
          ! Elmahdy, A.H. and Mitalas, G.P.  1977."A Simple Model for Cooling and
          ! Dehumidifying Coils for Use In Calculating Energy Requirements for Buildings,"
          ! ASHRAE Transactions,Vol.83 Part 2, pp. 103-117.

          ! USE STATEMENTS:

          ! Enforce explicit typing of all variables in this routine
      Implicit None

          ! FUNCTION ARGUMENT DEFINITIONS:
      Integer, intent(in) :: CoilNum          ! Number of Coil
      REAL(r64), intent(in) :: WaterTempIn         ! Water temperature IN to this function (C)
      REAL(r64), intent(in) :: AirTempIn           ! Air dry bulb temperature IN to this function(C)
      REAL(r64), intent(in) :: AirHumRat           ! Air Humidity Ratio IN to this funcation (C)
      REAL(r64), intent(in) :: UAInternalTotal     ! Internal overall heat transfer coefficient(W/m2 C)
      REAL(r64), intent(in) :: UAExternalTotal     ! External overall heat transfer coefficient(W/m2 C)
      REAL(r64)     ::  OutletWaterTemp       ! Leaving water temperature (C)
      REAL(r64)     ::  OutletAirTemp         ! Leaving air dry bulb temperature(C)
      REAL(r64)     ::  OutletAirHumRat       ! Leaving air humidity ratio
      REAL(r64)     ::  TotWaterCoilLoad      ! Total heat transfer rate(W)
      REAL(r64)     ::  SenWaterCoilLoad      ! Sensible heat transfer rate(W)
      REAL(r64)     ::  AirInletCoilSurfTemp  ! Surface temperature at air entrance(C)
      REAL(r64)     ::  SurfAreaWetFraction   ! Fraction of surface area wet
      INTEGER, INTENT(IN) :: FanOpMode      ! fan operating mode
      REAL(r64),    INTENT(IN) :: PartLoadRatio  ! part-load ratio of heating coil


          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
      REAL(r64) AirSideResist                    ! Air-side resistance to heat transfer(m2 C/W)
      REAL(r64) WaterSideResist                  ! Liquid-side resistance to heat transfer(m2 C/W)
      REAL(r64) EnteringAirDewPt                 ! Entering air dew point(C)
      REAL(r64) UACoilTotalEnth                  ! Overall enthalpy heat transfer coefficient(kg/s)
      REAL(r64) CapacityRateAirWet               ! Air-side capacity rate(kg/s)
      REAL(r64) CapacityRateWaterWet             ! Liquid-side capacity rate(kg/s)
      REAL(r64) ResistRatio                      ! Ratio of resistances
      REAL(r64) EnthAirOutlet                    ! Outlet air enthalpy
      REAL(r64) EnthSatAirInletWaterTemp         ! Saturated enthalpy of air at entering water temperature(J/kg)
      REAL(r64) EnthSatAirOutletWaterTemp        ! Saturated enthalpy of air at exit water temperature(J/kg)
      REAL(r64) EnthSatAirCoilSurfaceEntryTemp   ! Saturated enthalpy of air at entering surface temperature(J/kg)
      REAL(r64) EnthSatAirCoilSurfaceExitTemp    ! Saturated enthalpy of air at exit surface temperature(J/kg)
      REAL(r64) EnthAirInlet                     ! Enthalpy of air at inlet
      REAL(r64) IntermediateCpSat                ! Coefficient for equation below(J/kg C)
                                            ! EnthSat1-EnthSat2 = IntermediateCpSat*(TSat1-TSat2)
                                            ! (all water and surface temperatures are
                                            ! related to saturated air enthalpies for
                                            ! wet surface heat transfer calculations)
      REAL(r64),Parameter::SmallNo = 1.d-9       ! smallNo used in place of 0
      REAL(r64) :: AirMassFlow
      REAL(r64) :: WaterMassFlowRate
      REAL(r64) :: Cp

      SurfAreaWetFraction = 1.d0
      AirSideResist = 1.d0/MAX(UAExternalTotal,SmallNo)
      WaterSideResist = 1.d0/MAX(UAInternalTotal,SmallNo)

      !  adjust mass flow rates for cycling fan cycling coil operation
        IF(FanOpMode .EQ. CycFanCycCoil)THEN
          IF(PartLoadRatio .GT. 0.0d0)THEN
            AirMassFlow       = WaterCoil(CoilNum)%InletAirMassFlowRate/PartLoadRatio
            WaterMassFlowRate = MIN(WaterCoil(CoilNum)%InletWaterMassFlowRate/PartLoadRatio, &
                                    WaterCoil(CoilNum)%MaxWaterMassFlowRate)
          ELSE
            AirMassFlow       = 0.0d0
            WaterMassFlowRate = 0.0d0
          END IF
        ELSE
          AirMassFlow       = WaterCoil(CoilNum)%InletAirMassFlowRate
          WaterMassFlowRate = WaterCoil(CoilNum)%InletWaterMassFlowRate
        END IF

         ! Calculate enthalpies of entering air and water

         ! Enthalpy of air at inlet to the coil
      EnthAirInlet= PsyHFnTdbW(AirTempIn,AirHumRat )

         ! Saturation Enthalpy of Air at inlet water temperature
      EnthSatAirInletWaterTemp=PsyHFnTdbW(WaterTempIn,PsyWFnTdpPb(WaterTempIn,OutBaroPress))

         ! Estimate IntermediateCpSat using entering air dewpoint and water temperature
      EnteringAirDewPt = PsyTdpFnWPb(AirHumRat ,OutBaroPress)

         ! An intermediate value of Specific heat . EnthSat1-EnthSat2 = IntermediateCpSat*(TSat1-TSat2)
      IntermediateCpSat=(PsyHFnTdbW(EnteringAirDewPt,PsyWFnTdpPb(EnteringAirDewPt,OutBaroPress))- &
                                                            EnthSatAirInletWaterTemp)/(EnteringAirDewPt-WaterTempIn)

         ! Determine air and water enthalpy outlet conditions by modeling
         ! coil as counterflow enthalpy heat exchanger
       UACoilTotalEnth = 1.d0/(IntermediateCpSat*WaterSideResist+AirSideResist*PsyCpAirFnWTdb(0.0d0,AirTempIn))
       CapacityRateAirWet = AirMassFlow
       Cp =  GetSpecificHeatGlycol(PlantLoop(WaterCoil(CoilNum)%WaterLoopNum)%FluidName,  &
                                  WaterTempIn,                      &
                                  PlantLoop(WaterCoil(CoilNum)%WaterLoopNum)%FluidIndex, &
                                  'CoilCompletelyWet')
       CapacityRateWaterWet = WaterMassFlowRate*(Cp/IntermediateCpSat)
       CALL CoilOutletStreamCondition(CoilNum, CapacityRateAirWet,EnthAirInlet,CapacityRateWaterWet,EnthSatAirInletWaterTemp,&
                                      UACoilTotalEnth,EnthAirOutlet,EnthSatAirOutletWaterTemp)

         ! Calculate entering and leaving external surface conditions from
         ! air and water conditions and the ratio of resistances
       ResistRatio=(WaterSideResist)/(WaterSideResist+  &
                                      PsyCpAirFnWTdb(0.0d0,AirTempIn)/IntermediateCpSat*AirSideResist)
       EnthSatAirCoilSurfaceEntryTemp = EnthSatAirOutletWaterTemp + ResistRatio* &
                                               (EnthAirInlet-EnthSatAirOutletWaterTemp)
       EnthSatAirCoilSurfaceExitTemp = EnthSatAirInletWaterTemp + ResistRatio* &
                                               (EnthAirOutlet-EnthSatAirInletWaterTemp)

         ! Calculate Coil Surface Temperature at air entry to the coil
        AirInletCoilSurfTemp=  PsyTsatFnHPb(EnthSatAirCoilSurfaceEntryTemp,OutBaroPress)

         ! Calculate outlet air temperature and humidity from enthalpies and surface conditions.
       TotWaterCoilLoad = WaterCoil(CoilNum)%InletAirMassFlowRate*(EnthAirInlet-EnthAirOutlet)
       OutletWaterTemp = WaterTempIn+TotWaterCoilLoad/MAX(WaterCoil(CoilNum)%InletWaterMassFlowRate,SmallNo)/Cp

         ! Calculates out put variable for  the completely wet coil
       CALL WetCoilOutletCondition(CoilNum,AirTempIn ,EnthAirInlet,EnthAirOutlet,   &
                     UAExternalTotal,OutletAirTemp,OutletAirHumRat,SenWaterCoilLoad)


999   RETURN
END SUBROUTINE CoilCompletelyWet


! Coil Part Wet Part Dry Subroutine for Cooling Coil
SUBROUTINE CoilPartWetPartDry(CoilNum,FirstHVACIteration,InletWaterTemp, InletAirTemp, AirDewPointTemp,  &
                              OutletWaterTemp,OutletAirTemp,OutletAirHumRat,TotWaterCoilLoad,            &
                              SenWaterCoilLoad,SurfAreaWetFraction, FanOpMode, PartLoadRatio)

          ! FUNCTION INFORMATION:
          ! AUTHOR         Rahul Chillar
          ! DATE WRITTEN   March 2004
          ! MODIFIED       na
          ! RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! Calculate the performance of a cooling  coil when the external fin surface is
          ! part wet and part dry.  Results include outlet air temperature and humidity,
          ! outlet liquid temperature, sensible and total cooling capacities, and the wet
          ! fraction of the air-side surface area.

          ! METHODOLOGY EMPLOYED:
          ! Models coil using effectiveness NTU model

          ! REFERENCES:
          ! Elmahdy, A.H. and Mitalas, G.P.  1977. "A Simple Model for Cooling and
          ! Dehumidifying Coils for Use In Calculating Energy Requirements for Buildings,"
          ! ASHRAE Transactions,Vol.83 Part 2, pp. 103-117.
          ! TRNSYS.  1990.  A Transient System Simulation Program: Reference Manual.
          ! Solar Energy Laboratory, Univ. Wisconsin- Madison, pp. 4.6.8-1 - 4.6.8-12.
          ! Threlkeld, J.L.  1970.  Thermal Environmental Engineering, 2nd Edition,
          ! Englewood Cliffs: Prentice-Hall,Inc. pp. 254-270.

          ! USE STATEMENTS:
      USE General, ONLY: Iterate

          ! Enforce explicit typing of all variables in this routine
      Implicit None

          ! FUNCTION ARGUMENT DEFINITIONS:
      Integer, intent(in) :: CoilNum                ! Number of Coil
      REAL(r64), intent(in)    :: InletWaterTemp         ! Entering liquid temperature(C)
      REAL(r64), intent(in)    :: InletAirTemp           ! Entering air dry bulb temperature(C)
      REAL(r64), intent(in)    :: AirDewPointTemp        ! Entering air dew point(C)
      Logical, intent(in) :: FirstHVACIteration     ! Saving Old values
      INTEGER, INTENT(IN) :: FanOpMode      ! fan operating mode
      REAL(r64),    INTENT(IN) :: PartLoadRatio  ! part-load ratio of heating coil
      REAL(r64)           :: OutletWaterTemp        ! Leaving liquid temperature(C)
      REAL(r64)           :: OutletAirTemp          ! Leaving air dry bulb temperature(C)
      REAL(r64)           :: OutletAirHumRat        ! Leaving air humidity ratio
      REAL(r64)           :: TotWaterCoilLoad       ! Total heat transfer rate (W)
      REAL(r64)           :: SenWaterCoilLoad       ! Sensible heat transfer rate (W)
      REAL(r64)           :: SurfAreaWetFraction    ! Fraction of surface area wet

          ! FUNCTION PARAMETER DEFINITIONS:
      Integer, Parameter:: itmax = 60
      real(r64), parameter :: smalltempdiff=1.0d-9

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
      REAL(r64) DryCoilHeatTranfer              ! Heat transfer rate for dry coil(W)
      REAL(r64) WetCoilTotalHeatTransfer        ! Total heat transfer rate for wet coil(W)
      REAL(r64) WetCoilSensibleHeatTransfer     ! Sensible heat transfer rate for wet coil(W)
      REAL(r64) SurfAreaWet                     ! Air-side area of wet coil(m2)
      REAL(r64) SurfAreaDry                     ! Air-side area of dry coil(m2)
      REAL(r64) DryCoilUA                       ! Overall heat transfer coefficient for dry coil(W/C)
      REAL(r64) WetDryInterfcWaterTemp          ! Liquid temperature at wet/dry boundary(C)
      REAL(r64) WetDryInterfcAirTemp            ! Air temperature at wet/dry boundary(C)
      REAL(r64) WetDryInterfcSurfTemp           ! Surface temperature at wet/dry boundary(C)
      REAL(r64) EstimateWetDryInterfcWaterTemp  ! Estimated liquid temperature at wet/dry boundary(C)
      REAL(r64) EstimateSurfAreaWetFraction     ! Initial Estimate for Fraction of Surface Wet with condensation
      REAL(r64) WetPartUAInternal               ! UA of Wet Coil Internal
      REAL(r64) WetPartUAExternal               ! UA of Dry Coil External
      REAL(r64) WetDryInterfcHumRat             ! Humidity Ratio at interface of the wet dry transition
      REAL(r64) X1T                             ! Variables used in the two iteration in this subroutine.
      REAL(r64) NewSurfAreaWetFrac              ! Variables used in the two iteration in this subroutine.
      REAL(r64) ResultXT                        ! Variables used in the two iteration in this subroutine.
      REAL(r64) Y1T                             ! Variables used in the two iterations in this subroutine.
      REAL(r64) errorT                          ! Error in interation for First If loop
      REAL(r64) error                           ! Deviation of dependent variable in iteration
      REAL(r64) SurfAreaFracPrevious, ErrorPrevious,SurfAreaFracLast,ErrorLast
      Integer iter     ! Iteration counter
      Integer icvg     ! Iteration convergence flag
      INTEGER icvgT    ! Iteration Convergence Flag for First If loop
      INTEGER itT      ! Iteration Counter for First If Loop

        ! Iterates on SurfAreaWetFraction to converge on surface temperature equal to
        ! entering air dewpoint at wet/dry boundary.

        ! Preliminary estimates of coil performance to begin iteration
      OutletWaterTemp = InletAirTemp
      DryCoilHeatTranfer = 0.0d0
      WetCoilTotalHeatTransfer = 0.0d0
      WetCoilSensibleHeatTransfer = 0.0d0

      If(FirstHVACIteration)Then
         ! Estimate liquid temperature at boundary as entering air dew point
         WetDryInterfcWaterTemp = AirDewPointTemp

         ! Estimate fraction wet surface area based on liquid temperatures
         IF (ABS(OutletWaterTemp-InletWaterTemp) > smalltempdiff) then
           SurfAreaWetFraction = (WetDryInterfcWaterTemp-InletWaterTemp)/(OutletWaterTemp-InletWaterTemp)
         ELSE
           SurfAreaWetFraction = 0.0d0
         ENDIF

      Else
         SurfAreaWetFraction = WaterCoil(CoilNum)%SurfAreaWetFractionSaved

      End IF
       ! BEGIN LOOP to converge on SurfAreaWetFraction
       ! The method employed in this loop is as follows: The coil is partially wet and partially dry,
       ! we calculate the temperature of the coil at the interface, (the point at which the moisture begins
       ! to condense) temperature of the  water  at interface and air temp is dew point at that location.
       ! This is done by Iterating between the Completely Dry and Completely Wet Coil until the outlet
       ! water temperature of one coil equals the inlet water temperature of another.
       ! Using this value of interface temperature we now iterate to calculate Surface Fraction Wet, Iterate
       ! function perturbs the value of Surface Fraction Wet and based on this new value the entire loop is
       ! repeated to get a new interface water temperature and then surface fraction wet is again calculated.
       ! This process continues till the error between the Wet Dry Interface Temp and Air Dew Point becomes
       ! very negligible and in 95% of the cases its is a complete convergence to give the exact surface Wet
       ! fraction.
     NewSurfAreaWetFrac=SurfAreaWetFraction
     error=0.0d0
     SurfAreaFracPrevious=SurfAreaWetFraction
     ErrorPrevious=0.0d0
     SurfAreaFracLast=SurfAreaWetFraction
     ErrorLast=0.0d0

     DO iter = 1,itmax

           ! Calculating Surface Area Wet and Surface Area Dry
        SurfAreaWet = SurfAreaWetFraction*WaterCoil(CoilNum)%TotCoilOutsideSurfArea
        SurfAreaDry = WaterCoil(CoilNum)%TotCoilOutsideSurfArea-SurfAreaWet

           ! Calculating UA values for the Dry Part of the Coil
        DryCoilUA = SurfAreaDry/(1.d0/WaterCoil(CoilNum)%UACoilInternalPerUnitArea+1.0d0/WaterCoil(CoilNum)%UADryExtPerUnitArea)

           ! Calculating UA Value for the Wet part of the Coil
        WetPartUAExternal = WaterCoil(CoilNum)%UAWetExtPerUnitArea*SurfAreaWet
        WetPartUAInternal = WaterCoil(CoilNum)%UACoilInternalPerUnitArea*SurfAreaWet

           ! Calculating Water Temperature at Wet Dry Interface of the coil
        WetDryInterfcWaterTemp = InletWaterTemp+SurfAreaWetFraction*(OutletWaterTemp-InletWaterTemp)

           ! BEGIN LOOP to converge on liquid temperature at wet/dry boundary
        DO itT = 1,itmax

           ! Calculate dry coil performance with estimated liquid temperature at the boundary.
          CALL CoilCompletelyDry(CoilNum,WetDryInterfcWaterTemp, InletAirTemp,DryCoilUA,   &
                                 OutletWaterTemp,WetDryInterfcAirTemp,WetDryInterfcHumRat, &
                                 DryCoilHeatTranfer,FanOpMode,PartLoadRatio)

           ! Calculate wet coil performance with calculated air temperature at the boundary.
          CALL CoilCompletelyWet (CoilNum,InletWaterTemp,WetDryInterfcAirTemp,WetDryInterfcHumRat, &
                                  WetPartUAInternal,WetPartUAExternal,                             &
                                  EstimateWetDryInterfcWaterTemp,OutletAirTemp,OutletAirHumRat,    &
                                  WetCoilTotalHeatTransfer,WetCoilSensibleHeatTransfer,            &
                                  EstimateSurfAreaWetFraction,WetDryInterfcSurfTemp,FanOpMode,PartLoadRatio)

              ! Iterating to calculate the actual wet dry interface water temperature.
            errorT = EstimateWetDryInterfcWaterTemp - WetDryInterfcWaterTemp
          Call ITERATE (ResultXT,0.001d0, WetDryInterfcWaterTemp,errorT,X1T,Y1T,itT,icvgT)
            WetDryInterfcWaterTemp = ResultXT

            ! IF convergence is achieved then exit the itT to itmax Do loop.
         IF(icvgT .EQ. 1) Exit

        End Do  ! End Do for Liq Boundary temp Convergence

          ! Wet Dry Interface temperature not converged after maximum specified iterations.
          ! Print error message, set return error flag
        IF ((itT > itmax).AND.(.NOT.WarmUpFlag)) THEN
            CALL ShowWarningError('For Coil:Cooling:Water '//TRIM(WaterCoil(CoilNum)%Name))
            CALL ShowContinueError ('CoilPartWetPartDry: Maximum iterations exceeded for Liq Temp, at Interface')
        END IF

          ! If Following condition prevails then surface is dry, calculate dry coil performance and return
        IF(SurfAreaWetFraction .LE. 0.0d0 .AND. WetDryInterfcSurfTemp .GE. AirDewPointTemp) THEN

            ! Calculating Value of Dry UA for the coil
          DryCoilUA = WaterCoil(CoilNum)%TotCoilOutsideSurfArea/(1.0d0/WaterCoil(CoilNum)%UACoilInternalPerUnitArea+ &
                                                                 1.0d0/WaterCoil(CoilNum)%UADryExtPerUnitArea)

            ! Calling the Completely Dry Coil for outputs
          CALL CoilCompletelyDry(CoilNum,InletWaterTemp, InletAirTemp,DryCoilUA,  &
                                 OutletWaterTemp,OutletAirTemp,OutletAirHumRat,TotWaterCoilLoad,FanOpMode,PartLoadRatio)

            ! Sensible load = Total load in a Completely Dry Coil
          SenWaterCoilLoad = TotWaterCoilLoad

            ! All coil is Dry so fraction wet is ofcourse =0
          SurfAreaWetFraction = 0.0d0
          RETURN
        ENDIF

          ! IF the coil is not Dry then iterate to calculate Fraction of surface area that is wet.
        error = WetDryInterfcSurfTemp - AirDewPointTemp
        Call CoilAreaFracIter (NewSurfAreaWetFrac,SurfAreaWetFraction,error,SurfAreaFracPrevious, &
                                                       ErrorPrevious,SurfAreaFracLast,ErrorLast,iter,icvg)
        SurfAreaWetFraction = NewSurfAreaWetFrac

          !If converged, leave iteration loop
        IF (icvg .EQ. 1) Exit

          ! Surface temperature not converged.  Repeat calculations with new
          ! estimate of fraction wet surface area.
        IF(SurfAreaWetFraction  > 1.0d0) SurfAreaWetFraction = 1.0d0
        IF(SurfAreaWetFraction <= 0.0d0) SurfAreaWetFraction = 0.0098d0

     End Do  ! End do for the overall iteration

     ! Calculate sum of total and sensible heat transfer from dry and wet parts.
     TotWaterCoilLoad = DryCoilHeatTranfer+WetCoilTotalHeatTransfer
     SenWaterCoilLoad = DryCoilHeatTranfer+WetCoilSensibleHeatTransfer

     ! Save last iterations values for this current time step
     WaterCoil(CoilNum)%SurfAreaWetFractionSaved = SurfAreaWetFraction


     RETURN
   END SUBROUTINE CoilPartWetPartDry


! Calculating coil UA for Cooling Coil
FUNCTION CalcCoilUAbyEffectNTU (CoilNum,CapacityStream1,EnergyInStreamOne,   &
                                          CapacityStream2,EnergyInStreamTwo, &
                                          DesTotalHeatTransfer)

          ! FUNCTION INFORMATION:
          ! AUTHOR         Rahul Chillar
          ! DATE WRITTEN   March 2004
          ! MODIFIED       na
          ! RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! Calculate the UA of a heat exchanger using the effectiveness-NTU relationships
          ! given the entering capacity rate and temperature of each flow stream, the
          ! heat transfer rate under these conditions and the heat exchanger configuration.

          ! METHODOLOGY EMPLOYED:
          ! Models coil using effectiveness NTU model

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
      USE General, ONLY: Iterate

          ! Enforce explicit typing of all variables in this routine
      Implicit None

          ! FUNCTION ARGUMENT DEFINITIONS:
      Integer, Intent(in) :: CoilNum
      REAL(r64), intent(in)    :: CapacityStream1        ! Capacity rate of stream1.(W/C)
      REAL(r64), intent(in)    :: EnergyInStreamOne      ! Inlet state of stream1.(C)
      REAL(r64), intent(in)    :: CapacityStream2        ! Capacity rate of stream2.(W/C)
      REAL(r64), intent(in)    :: EnergyInStreamTwo      ! Inlet state of stream2.(C)
      REAL(r64), intent(in)    :: DesTotalHeatTransfer   ! Heat transfer rate(W)
      REAL(r64)           :: CalcCoilUAbyEffectNTU  ! Overall heat transfer coefficient(W/C)


          ! FUNCTION PARAMETER DEFINITIONS:
      REAL(r64), Parameter:: SmallNo=1.d-9
      REAL(r64), Parameter:: LargeNo=1.d9
      Integer, Parameter:: itmax=12

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
      REAL(r64) MaxHeatTransfer       ! Maximum heat transfer from inlet conditions (W)
      REAL(r64) EstimatedHeatTransfer ! Estimated heat transfer in iteration(W)
      REAL(r64) CoilUA                ! Estimated heat transfer coefficient(W/C)
      REAL(r64) error                 ! Deviation of dependent variable in iteration
      REAL(r64) X1,Y1 , ResultX       ! Previous values of independent variable in iteration
      REAL(r64) EnergyOutStreamOne ! Intermediate Variable used
      REAL(r64) EnergyOutStreamTwo ! Intermediate variable used
      REAL(r64) DesTotalHeatTransferCheck ! Check value to keep design total heat transfer in range
      INTEGER iter   ! Iteration index
      INTEGER icvg   ! Iteration convergence flag

         ! Check for Q out of range (effectiveness > 1)
      MaxHeatTransfer = ABS(MIN(CapacityStream1,CapacityStream2)*    &
                              (EnergyInStreamOne-EnergyInStreamTwo))

         ! Error Message
      IF((ABS(DesTotalHeatTransfer)-MaxHeatTransfer)/MAX(MaxHeatTransfer,SmallNo) .GT. SmallNo) THEN
         CALL ShowWarningError('For Coil:Cooling:Water '//TRIM(WaterCoil(CoilNum)%Name))
         CALL ShowContinueError('CalcCoilUAbyEffectNTU:Given Q impossible for given inlet states, '//  &
                                'proceeding with MaxHeat Transfer')
         CALL ShowContinueError('Check the Sizing:System and Sizing:Zone cooling design supply air temperature and ')
         CALL ShowContinueError('the Sizing:Plant design Loop exit temperature.  There must be sufficient difference between '//  &
                                'these two temperatures.')
      END IF


         ! Design Heat Transfer cannot exceed Max heat Transfer , setting it value such that effectiveness<1.0
        IF((DesTotalHeatTransfer).GT.(MaxHeatTransfer)) Then
            ! Pegging value so that effectiveness is less than 1.
            DesTotalHeatTransferCheck= 0.9d0*MaxHeatTransfer

           ! Estimate CalcCoilUAbyEffectNTU
          CoilUA = ABS(DesTotalHeatTransferCheck/(EnergyInStreamOne-EnergyInStreamTwo))

        ELSE

           ! Estimate CalcCoilUAbyEffectNTU
          CoilUA = ABS(DesTotalHeatTransfer/(EnergyInStreamOne-EnergyInStreamTwo))

        END IF

         ! BEGIN LOOP to iteratively calculate CalcCoilUAbyEffectNTU
        DO iter = 1,itmax

           ! Calculate heat transfer rate for estimated CalcCoilUAbyEffectNTU
          CALL CoilOutletStreamCondition(CoilNum, CapacityStream1,EnergyInStreamOne,  &
                                 CapacityStream2,EnergyInStreamTwo,                   &
                                 CoilUA,EnergyOutStreamOne,EnergyOutStreamTwo)

            ! Initial Guess for a value of heat transfer
           EstimatedHeatTransfer = CapacityStream1*(EnergyInStreamOne-EnergyOutStreamOne)

            ! Calculate new estimate for CalcCoilUAbyEffectNTU by iteration
                If(DesTotalHeatTransfer>MaxHeatTransfer) Then
                   error = ABS(EstimatedHeatTransfer) - ABS(DesTotalHeatTransferCheck)
                  Else
                   error = ABS(EstimatedHeatTransfer) - ABS(DesTotalHeatTransfer)
                END If
           Call ITERATE (ResultX,0.01d0, CoilUA,error,X1,Y1,iter,icvg)
                CoilUA = ResultX
         ! If converged, leave loop
        IF (icvg .EQ. 1)   Exit
      End Do


         ! If not converged after itmax iterations, return error code
        IF ((iter > itmax).AND.(.NOT.WarmUpFlag)) THEN
            CALL ShowWarningError('For Coil:Cooling:Water '//TRIM(WaterCoil(CoilNum)%Name))
            CALL ShowContinueError ('CalcCoilUAbyEffectNTU: Maximum iterations exceeded:Coil UA calculation')
            CalcCoilUAbyEffectNTU = 0.0d0 !Objexx:Return Line added to set return value: Using non-converged CoilUA value may be preferred but that was not happening
          ELSE

          ! Assign value to CalcCoilUAbyEffectNTU
          CalcCoilUAbyEffectNTU = CoilUA
        END IF

  Return
END FUNCTION  CalcCoilUAbyEffectNTU


! Calculating coil outlet stream conditions and coil UA for Cooling Coil
SUBROUTINE CoilOutletStreamCondition(CoilNum, CapacityStream1,EnergyInStreamOne,           &
                                     CapacityStream2,EnergyInStreamTwo,                    &
                                     CoilUA,EnergyOutStreamOne,EnergyOutStreamTwo)

          ! FUNCTION INFORMATION:
          ! AUTHOR         Rahul Chillar
          ! DATE WRITTEN   March 2004
          ! MODIFIED       na
          ! RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! Calculate the outlet states of a simple heat exchanger using the effectiveness-Ntu
          ! method of analysis.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! Kays, W.M. and A.L. London.  1964.Compact Heat Exchangers, 2nd Ed.McGraw-Hill:New York.

          ! USE STATEMENTS:
          ! na

          ! Enforce explicit typing of all variables in this routine
      Implicit None

          ! FUNCTION ARGUMENT DEFINITIONS:
      Integer, intent(in) :: CoilNum
      REAL(r64), intent(in) :: CapacityStream1              ! Capacity rate of stream1(W/C)
      REAL(r64), intent(in) :: EnergyInStreamOne   ! Inlet state of stream1 (C)
      REAL(r64), intent(in) :: CapacityStream2              ! Capacity rate of stream2 (W/C)
      REAL(r64), intent(in) :: EnergyInStreamTwo   ! Inlet state of stream2 (C)
      REAL(r64), intent(in) :: CoilUA                       ! Heat transfer rateW)
      REAL(r64)        :: EnergyOutStreamOne  ! Outlet state of stream1 (C)
      REAL(r64)        :: EnergyOutStreamTwo  ! Outlet state of stream2 (C)


          ! FUNCTION PARAMETER DEFINITIONS:
      REAL(r64),Parameter:: LargeNo = 1.d10   ! value used in place of infinity
      REAL(r64),Parameter:: SmallNo = 1.d-15  ! value used in place of zero

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
      REAL(r64) MinimumCapacityStream    ! Minimum capacity rate of the streams(W/C)
      REAL(r64) MaximumCapacityStream    ! Maximum capacity rate of the streams(W/C)
      REAL(r64) RatioStreamCapacity      ! Ratio of minimum to maximum capacity rate
      REAL(r64) NTU             ! Number of transfer units
      REAL(r64) :: effectiveness=0.0d0   ! Heat exchanger effectiveness
      REAL(r64) MaxHeatTransfer ! Maximum heat transfer possible(W)
      REAL(r64)  e, eta, b, d   ! Intermediate variables in effectivness equation


         ! NTU and MinimumCapacityStream/MaximumCapacityStream (RatioStreamCapacity) calculations
      MinimumCapacityStream = MIN(CapacityStream1,CapacityStream2)
      MaximumCapacityStream = MAX(CapacityStream1,CapacityStream2)


      IF(ABS(MaximumCapacityStream) .le. 1.d-6) THEN ! .EQ. 0.0d0) THEN
        RatioStreamCapacity = 1.d0
      ELSE
        RatioStreamCapacity = MinimumCapacityStream/MaximumCapacityStream
      ENDIF


      IF(ABS(MinimumCapacityStream) .le. 1.d-6) THEN ! .EQ. 0.0d0) THEN
        NTU = LargeNo
      ELSE
        NTU = CoilUA/MinimumCapacityStream
      ENDIF


       ! Calculate effectiveness for special limiting cases
      IF(NTU .LE. 0.0d0) THEN
        effectiveness = 0.0d0

      ELSE IF(RatioStreamCapacity .LT. SmallNo) THEN
         ! MinimumCapacityStream/MaximumCapacityStream = 0 and effectiveness is independent of configuration
         ! 20 is the Limit Chosen for Exponential Function, beyond which there is float point error.
        If(NTU > 20.d0) THEN
          effectiveness = 1.0d0
        Else
          effectiveness = 1.0d0 - EXP(-NTU)
        End If
         ! Calculate effectiveness depending on heat exchanger configuration
      ELSE IF (WaterCoil(CoilNum)%HeatExchType .EQ. CounterFlow) THEN

          ! Counterflow Heat Exchanger Configuration
        IF (ABS(RatioStreamCapacity-1.d0) .LT. SmallNo) THEN
          effectiveness = NTU/(NTU+1.0d0)
        ELSE
          If(NTU*(1.d0-RatioStreamCapacity) > 20.0d0) Then
            e = 0.0d0
          Else
            e=EXP(-NTU*(1.d0-RatioStreamCapacity))
          End If
          effectiveness = (1.d0-e)/(1.d0-RatioStreamCapacity*e)
        ENDIF

       ELSE IF (WaterCoil(CoilNum)%HeatExchType .EQ. CrossFlow) THEN
         ! Cross flow, both streams unmixed
        eta = NTU**(-0.22d0)
        If((NTU*RatioStreamCapacity*eta)>20.d0) Then
          b=1.0d0/(RatioStreamCapacity*eta)
          If(b>20.d0)  THEN
            effectiveness=1.d0
          Else
            effectiveness = 1.0d0 - EXP(-b)
            If(effectiveness.LT.0.0d0) effectiveness=0.0d0
          End If
        Else
          d=((EXP(-NTU*RatioStreamCapacity*eta)-1.d0)/(RatioStreamCapacity*eta))
          If(d .LT. -20.0d0 .OR. d .GT. 0.0d0) Then
            effectiveness=1.0d0
          Else
            effectiveness = 1.0d0 - EXP((EXP(-NTU*RatioStreamCapacity*eta)-1.d0)/(RatioStreamCapacity*eta))
            If(effectiveness.LT.0.0d0) effectiveness=0.0d0
          End if
        End if


      ENDIF

         ! Determine leaving conditions for the two streams
      MaxHeatTransfer = MAX(MinimumCapacityStream,SmallNo)*(EnergyInStreamOne-EnergyInStreamTwo)
      EnergyOutStreamOne = EnergyInStreamOne - effectiveness*  &
                                             MaxHeatTransfer/MAX(CapacityStream1,SmallNo)
      EnergyOutStreamTwo = EnergyInStreamTwo + effectiveness*  &
                                             MaxHeatTransfer/MAX(CapacityStream2,SmallNo)

      RETURN
      END SUBROUTINE  CoilOutletStreamCondition



! Subroutine for caculating outlet condition if coil is wet , for Cooling Coil
      SUBROUTINE WetCoilOutletCondition(CoilNum,AirTempIn,EnthAirInlet,EnthAirOutlet,UACoilExternal,  &
                                        OutletAirTemp,OutletAirHumRat,SenWaterCoilLoad)

          ! FUNCTION INFORMATION:
          ! AUTHOR         Rahul Chillar
          ! DATE WRITTEN   Mar 2004
          ! MODIFIED       na
          ! RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! Calculate the leaving air temperature,the leaving air humidity ratio and the
          ! sensible cooling capacity of wet cooling coil.

          ! METHODOLOGY EMPLOYED:
          ! Assumes condensate at uniform temperature.

          ! REFERENCES:
          ! Elmahdy, A.H. and Mitalas, G.P.  1977."A Simple Model for Cooling and
          ! Dehumidifying Coils for Use In Calculating Energy Requirements for Buildings,"
          ! ASHRAE Transactions,Vol.83 Part 2, pp. 103-117.

          ! USE STATEMENTS:

          ! Enforce explicit typing of all variables in this routine
      Implicit None

          ! FUNCTION ARGUMENT DEFINITIONS:
      Integer, intent(in) :: CoilNum        !
      REAL(r64), intent(in) :: AirTempIn      ! Entering air dry bulb temperature(C)
      REAL(r64), intent(in) :: EnthAirInlet      ! Entering air enthalpy(J/kg)
      REAL(r64), intent(in) :: EnthAirOutlet     ! Leaving air enthalpy(J/kg)
      REAL(r64), intent(in) :: UACoilExternal    ! Heat transfer coefficient for external surface (W/C)
      REAL(r64)        :: OutletAirTemp     ! Leaving air dry bulb temperature(C)
      REAL(r64)        :: OutletAirHumRat   ! Leaving air humidity ratio
      REAL(r64)        :: SenWaterCoilLoad  ! Sensible heat transfer rate(W)


          ! FUNCTION PARAMETER DEFINITIONS:
      REAL(r64), Parameter:: SmallNo=1.d-9 ! SmallNo value used in place of zero

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
      REAL(r64) CapacitanceAir         ! Air capacity rate(W/C)
      REAL(r64) NTU                    ! Number of heat transfer units
      REAL(r64) effectiveness          ! Heat exchanger effectiveness
      REAL(r64) EnthAirCondensateTemp  ! Saturated air enthalpy at temperature of condensate(J/kg)
      REAL(r64) TempCondensation       ! Temperature of condensate(C)
      REAL(r64) TempAirDewPoint        ! Temperature air dew point


          ! Determine the temperature effectiveness, assuming the temperature
          ! of the condensate is constant (MinimumCapacityStream/MaximumCapacityStream = 0) and the specific heat
          ! of moist air is constant
      CapacitanceAir = WaterCoil(CoilNum)%InletAirMassFlowRate*     &
                                    PsyCpAirFnWTdb(WaterCoil(CoilNum)%InletAirHumRat,AirTempIn)

          ! Calculating NTU from UA and Capacitance.
!del      NTU = UACoilExternal/MAX(CapacitanceAir,SmallNo)
!del      effectiveness = 1 - EXP(-MAX(0.0d0,NTU))
          ! Calculating NTU from UA and Capacitance.
      IF (UACoilExternal > 0.0d0) THEN
        IF (CapacitanceAir > 0.0d0) THEN
          NTU = UACoilExternal/CapacitanceAir
        ELSE
          NTU = 0.0d0
        END IF
        effectiveness = 1.0d0 - EXP(-NTU)
      ELSE
        effectiveness = 0.0d0
      END IF

          ! Calculate coil surface enthalpy and temperature at the exit
          ! of the wet part of the coil using the effectiveness relation
      effectiveness = MAX(effectiveness,SmallNo)
      EnthAirCondensateTemp = EnthAirInlet-(EnthAirInlet-EnthAirOutlet)/effectiveness


        ! Calculate condensate temperature as the saturation temperature
        ! at given saturation enthalpy
      TempCondensation=  PsyTsatFnHPb(EnthAirCondensateTemp,OutBaroPress)

      TempAirDewPoint=PsyTdpFnWPb(WaterCoil(CoilNum)%InletAirHumRat,OutBaroPress)

      If((TempAirDewPoint-TempCondensation).GT.0.1d0)Then

           ! Calculate Outlet Air Temperature using effectivness
           OutletAirTemp = AirTempIn-(AirTempIn-TempCondensation)*effectiveness
           ! Calculate Outlet air humidity ratio from PsyWFnTdbH routine
           OutletAirHumRat  = PsyWFnTdbH(OutletAirTemp,EnthAirOutlet)

      Else
           OutletAirHumRat=WaterCoil(CoilNum)%InletAirHumRat
           OutletAirTemp=PsyTdbFnHW(EnthAirOutlet,OutletAirHumRat)

      EndIf


        ! Calculate Sensible Coil Load
      SenWaterCoilLoad = CapacitanceAir*(AirTempIn-OutletAirTemp)

      RETURN
      END SUBROUTINE WetCoilOutletCondition


! Beginning of Update subroutines for the WaterCoil Module
! *****************************************************************************

SUBROUTINE UpdateWaterCoil(CoilNum)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Liesen
          !       DATE WRITTEN   1998
          !       MODIFIED       April 2004: Rahul Chillar
          !                      Feb 2010 B. Griffith, plant upgrades
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine updates the coil outlet nodes.

          ! METHODOLOGY EMPLOYED:
          ! Data is moved from the coil data structure to the coil outlet nodes.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE PlantUtilities , ONLY: SetComponentFlowRate
  USE DataContaminantBalance, ONLY: Contaminant

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(In) :: CoilNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER             :: AirInletNode
  INTEGER             :: WaterInletNode
  INTEGER             :: AirOutletNode
  INTEGER             :: WaterOutletNode

  AirInletNode    = WaterCoil(CoilNum)%AirInletNodeNum
  WaterInletNode  = WaterCoil(CoilNum)%WaterInletNodeNum
  AirOutletNode   = WaterCoil(CoilNum)%AirOutletNodeNum
  WaterOutletNode = WaterCoil(CoilNum)%WaterOutletNodeNum

  ! Set the outlet air nodes of the WaterCoil
  Node(AirOutletNode)%MassFlowRate = WaterCoil(CoilNum)%OutletAirMassFlowRate
  Node(AirOutletNode)%Temp         = WaterCoil(CoilNum)%OutletAirTemp
  Node(AirOutletNode)%HumRat       = WaterCoil(CoilNum)%OutletAirHumRat
  Node(AirOutletNode)%Enthalpy     = WaterCoil(CoilNum)%OutletAirEnthalpy

  Node(WaterOutletNode)%Temp         = WaterCoil(CoilNum)%OutletWaterTemp
  Node(WaterOutletNode)%Enthalpy     = WaterCoil(CoilNum)%OutletWaterEnthalpy

     ! Set the outlet nodes for properties that just pass through & not used
  Node(AirOutletNode)%Quality             = Node(AirInletNode)%Quality
  Node(AirOutletNode)%Press               = Node(AirInletNode)%Press
  Node(AirOutletNode)%MassFlowRateMin     = Node(AirInletNode)%MassFlowRateMin
  Node(AirOutletNode)%MassFlowRateMax     = Node(AirInletNode)%MassFlowRateMax
  Node(AirOutletNode)%MassFlowRateMinAvail= Node(AirInletNode)%MassFlowRateMinAvail
  Node(AirOutletNode)%MassFlowRateMaxAvail= Node(AirInletNode)%MassFlowRateMaxAvail
  IF (Contaminant%CO2Simulation) THEN
    Node(AirOutletNode)%CO2 = Node(AirInletNode)%CO2
  END IF
  IF (Contaminant%GenericContamSimulation) THEN
    Node(AirOutletNode)%GenContam = Node(AirInletNode)%GenContam
  END IF

  RETURN
END SUBROUTINE UpdateWaterCoil

!        End of Update subroutines for the WaterCoil Module
! *****************************************************************************


! Beginning of Reporting subroutines for the WaterCoil Module
! *****************************************************************************

SUBROUTINE ReportWaterCoil(CoilNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Liesen
          !       DATE WRITTEN   1998
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine updates the report variable for the coils.

          ! METHODOLOGY EMPLOYED:
          ! NA

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataWater, ONLY: WaterStorage

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  Integer, Intent(IN) :: CoilNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64) :: rhoWater
  REAL(r64) :: Tavg
  REAL(r64) :: SpecHumOut
  REAL(r64) :: SpecHumIn
  REAL(r64) :: ReportingConstant

  ReportingConstant = TimeStepSys*SecInHour
 ! report the WaterCoil energy from this component
  WaterCoil(CoilNum)%TotWaterHeatingCoilEnergy= WaterCoil(CoilNum)%TotWaterHeatingCoilRate*ReportingConstant
  WaterCoil(CoilNum)%TotWaterCoolingCoilEnergy= WaterCoil(CoilNum)%TotWaterCoolingCoilRate*ReportingConstant
  WaterCoil(CoilNum)%SenWaterCoolingCoilEnergy= WaterCoil(CoilNum)%SenWaterCoolingCoilRate*ReportingConstant

! report the WaterCoil water collection to water storage tank (if needed)

  If (WaterCoil(CoilNum)%CondensateCollectMode == CondensateToTank) THEN
    ! calculate and report condensation rates  (how much water extracted from the air stream)
    ! water volumetric flow of water in m3/s for water system interactions
    !  put here to catch all types of DX coils
    Tavg =( WaterCoil(CoilNum)%InletAirTemp - WaterCoil(CoilNum)%OutletAirTemp)/2.0d0


    rhoWater = GetDensityGlycol(PlantLoop(WaterCoil(CoilNum)%WaterLoopNum)%FluidName,  &
                       Tavg,                      &
                       PlantLoop(WaterCoil(CoilNum)%WaterLoopNum)%FluidIndex, &
                       'ReportWaterCoil')
!   CR9155 Remove specific humidity calculations
    SpecHumIn = WaterCoil(CoilNum)%InletAirHumRat
    SpecHumOut = WaterCoil(CoilNum)%OutletAirHumRat
    !  mdot * del HumRat / rho water
    WaterCoil(CoilNum)%CondensateVdot = MAX(0.0d0, (WaterCoil(CoilNum)%InletAirMassFlowRate * (SpecHumIn - SpecHumOut) / rhoWater) )
    WaterCoil(CoilNum)%CondensateVol = WaterCoil(CoilNum)%CondensateVdot *ReportingConstant

    WaterStorage(WaterCoil(CoilNum)%CondensateTankID)%VdotAvailSupply(WaterCoil(CoilNum)%CondensateTankSupplyARRID) &
      = WaterCoil(CoilNum)%CondensateVdot
    WaterStorage(WaterCoil(CoilNum)%CondensateTankID)%TwaterSupply(WaterCoil(CoilNum)%CondensateTankSupplyARRID) &
      = WaterCoil(CoilNum)%OutletAirTemp

  ENDIF

  RETURN
END SUBROUTINE ReportWaterCoil

!        End of Reporting subroutines for the WaterCoil Module
! *****************************************************************************

! Beginning of Coil Utility subroutines for the Detailed Model
! *****************************************************************************
SUBROUTINE CalcDryFinEffCoef(OutTubeEffFinDiamRatio,PolynomCoef)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR   Unknown
          !       DATE WRITTEN   Unknown
          !       DATE REWRITTEN  April 1997 by Russell D. Taylor, Ph.D.
          !       MODIFIED
          !       RE-ENGINEERED

          ! PURPOSE OF THIS SUBROUTINE:
          ! The following subroutines are used once per cooling coil
          ! simulation to obtain the coefficients of the dry fin
          ! efficiency equation.  CalcDryFinEffCoef is the main calling
          ! routine which manages calls to the Bessel funtion and polynomial
          ! fit routines.

          ! REFERENCES:
          ! First found in MODSIM.
          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
    REAL(r64) OutTubeEffFinDiamRatio
    REAL(r64), DIMENSION(:) :: PolynomCoef

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    REAL(r64), DIMENSION(2,MaxOrderedPairs) :: OrderedPair
    REAL(r64) FAI,FED
    REAL(r64) FEDnumerator
    INTEGER I,IE1,IE2,IE3,IE4,IE5,IE6
    REAL(r64) R1,R1I1,R1K1,R2,R2I0,R2I1,R2K0,R2K1,RO

    FAI = 0.02d0
    DO I = 1, 60
      FAI = FAI + 0.035d0
      R1 = FAI / (1.0d0 - OutTubeEffFinDiamRatio)
      R2 = R1 * OutTubeEffFinDiamRatio
      RO = 2.0d0 * OutTubeEffFinDiamRatio / (FAI * (1.0d0 + OutTubeEffFinDiamRatio))
      CALL CalcIBesselFunc(R1, 1, R1I1, IE1)
      CALL CalcKBesselFunc(R2, 1, R2K1, IE2)
      CALL CalcIBesselFunc(R2, 1, R2I1, IE3)
      CALL CalcKBesselFunc(R1, 1, R1K1, IE4)
      CALL CalcIBesselFunc(R2, 0, R2I0, IE5)
      CALL CalcKBesselFunc(R2, 0, R2K0, IE6)
      FEDnumerator = RO * (R1I1 * R2K1 - R2I1 * R1K1)
      IF (FEDnumerator /= 0.0d0) THEN
        FED = FEDnumerator / (R2I0 * R1K1 + R1I1 * R2K0)
      ELSE
        FED = 0.0d0
      ENDIF
!      FED = RO * (R1I1 * R2K1 - R2I1 * R1K1) / (R2I0 * R1K1 + R1I1 * R2K0)
      OrderedPair(1,I) = FAI
      OrderedPair(2,I) = FED
    END DO
    CALL CalcPolynomCoef(OrderedPair,PolynomCoef)
    RETURN
END SUBROUTINE CalcDryFinEffCoef

SUBROUTINE CalcIBesselFunc(BessFuncArg,BessFuncOrd,IBessFunc,ErrorCode)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR   Unknown
          !       DATE WRITTEN   Unknown
          !       DATE REWRITTEN  April 1997 by Russell D. Taylor, Ph.D.
          !       MODIFIED
          !       RE-ENGINEERED

          ! PURPOSE OF THIS SUBROUTINE:
          ! To calculate the modified Bessel Function from order 0 to BessFuncOrd
          ! BessFuncArg    ARGUMENT OF BESSEL FUNCTION
          ! BessFuncOrd    ORDER OF BESSEL FUNCTION, GREATER THAN OR EQUAL TO ZERO
          ! IBessFunc   RESULTANT VALUE OF I BESSEL FUNCTION
          ! ErrorCode  RESULTANT ERROR CODE:
          !       ErrorCode = 0   NO ERROR
          !       ErrorCode = 1   BessFuncOrd .LT. 0
          !       ErrorCode = 2   BessFuncArg .LT. 0
          !       ErrorCode = 3   IBessFunc .LT. 10**(-30),     IBessFunc IS SET TO 0
          !       ErrorCode = 4   BessFuncArg .GT. BessFuncOrd & BessFuncArg .GT. 90,  IBessFunc IS SET TO 10**38

          ! REFERENCES:
          ! First found in MODSIM.

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
    REAL(r64) :: BessFuncArg
    INTEGER :: BessFuncOrd
    REAL(r64) :: IBessFunc
    INTEGER :: ErrorCode

          ! SUBROUTINE PARAMETER DEFINITIONS:
    REAL(r64), PARAMETER :: ErrorTol = 1.0d-06

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    INTEGER :: LoopCount

    REAL(r64) :: FI
    REAL(r64) :: FK
    REAL(r64) :: TERM

    ErrorCode=0
    IBessFunc=1.d0
    IF (BessFuncArg .EQ. 0.0d0 .AND. BessFuncOrd .EQ. 0) RETURN

    IF (BessFuncOrd .LT. 0) THEN
      ErrorCode=1
      RETURN
    ELSE IF (BessFuncArg .LT. 0.0d0) THEN
      ErrorCode = 2
      RETURN
    ELSE IF (BessFuncArg .GT. 12.d0 .AND. BessFuncArg .GT. BessFuncOrd) THEN
      IF (BessFuncArg .GT. 90.d0) THEN
        ErrorCode = 4
        IBessFunc = 10.d0**30
        RETURN
      ENDIF
      TERM = 1.d0
      IBessFunc = 1.d0
      DO LoopCount = 1, 30  !Start of 1st LoopCount Loop
        IF (ABS(TERM) .LE. ABS(ErrorTol * IBessFunc)) THEN
          IBessFunc = IBessFunc * EXP(BessFuncArg) / SQRT(2.d0 * PI * BessFuncArg)
          RETURN
        ENDIF
        TERM = TERM * 0.125d0 / BessFuncArg * ((2 * LoopCount - 1)**2 - &
               4 * BessFuncOrd * BessFuncOrd) / REAL(LoopCount,r64)
        IBessFunc = IBessFunc + TERM
      END DO  ! End of 1st LoopCount loop
    ENDIF

    TERM = 1.d0
    IF (BessFuncOrd .GT. 0) THEN
      DO LoopCount = 1, BessFuncOrd !Start of 2nd LoopCount Loop
        FI = LoopCount
        IF (ABS(TERM) .LT. 1.d-30 * FI / (BessFuncArg * 2.0d0)) THEN
          ErrorCode = 3
          IBessFunc = 0.0d0
          RETURN
        ENDIF
        TERM = TERM * BessFuncArg / (2.d0 * FI)
      END DO !End of 2nd LoopCount loop
    ENDIF

    IBessFunc = TERM
    DO LoopCount = 1,1000 !Start of 3rd LoopCount Loop
      IF (ABS(TERM) .LE. ABS(IBessFunc * ErrorTol)) RETURN
      FK = LoopCount * (BessFuncOrd + LoopCount)
      TERM = TERM * (BessFuncArg**2 / (4.d0 * FK))
      IBessFunc = IBessFunc + TERM
    END DO !End of  3rd LoopCount loop
    RETURN

END SUBROUTINE CalcIBesselFunc

SUBROUTINE CalcKBesselFunc(BessFuncArg,BessFuncOrd,KBessFunc,ErrorCode)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR   Unknown
          !       DATE WRITTEN   Unknown
          !       DATE REWRITTEN  April 1997 by Russell D. Taylor, Ph.D.
          !       MODIFIED
          !       RE-ENGINEERED

          ! PURPOSE OF THIS SUBROUTINE:
          ! To calculate the K Bessel Function for a given argument and
          ! order
          !
          !  BessFuncArg    THE ARGUMENT OF THE K BESSEL FUNCTION DESIRED
          !  BessFuncOrd    THE ORDER OF THE K BESSEL FUNCTION DESIRED
          !  KBessFunc   THE RESULTANT K BESSEL FUNCTION
          !  ErrorCode  RESULTANT ERROR CODE:
          !        ErrorCode=0  NO ERROR
          !        ErrorCode=1  BessFuncOrd IS NEGATIVE
          !        ErrorCode=2  BessFuncArg IS ZERO OR NEGATIVE
          !        ErrorCode=3  BessFuncArg .GT. 85, KBessFunc .LT. 10**-38; KBessFunc SET TO 0.
          !        ErrorCode=4  KBessFunc .GT. 10**38; KBessFunc SET TO 10**38
          !
          ! NOTE: BessFuncOrd MUST BE GREATER THAN OR EQUAL TO ZERO
          !
          ! METHOD:
          !  COMPUTES ZERO ORDER AND FIRST ORDER BESSEL FUNCTIONS USING
          !  SERIES APPROXIMATIONS AND THEN COMPUTES BessFuncOrd TH ORDER FUNCTION
          !  USING RECURRENCE RELATION.
          !  RECURRENCE RELATION AND POLYNOMIAL APPROXIMATION TECHNIQUE
          !  AS DESCRIBED BY A.J.M. HITCHCOCK, 'POLYNOMIAL APPROXIMATIONS
          !  TO BESSEL FUNCTIONS OF ORDER ZERO AND ONE AND TO RELATED
          !  FUNCTIONS,' M.T.A.C., V.11, 1957, PP. 86-88, AND G.BessFuncOrd. WATSON,
          !  'A TREATISE ON THE THEORY OF BESSEL FUNCTIONS,' CAMBRIDGE
          !  UNIVERSITY PRESS, 1958, P.62

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
    REAL(r64)    :: BessFuncArg
    INTEGER :: BessFuncOrd
    REAL(r64)    :: KBessFunc
    INTEGER :: ErrorCode

          ! SUBROUTINE PARAMETER DEFINITIONS:
    REAL(r64), PARAMETER :: GJMAX = 1.0d+38

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    INTEGER :: LoopCount
    LOGICAL :: StopLoop

    REAL(r64) :: FACT
    REAL(r64) :: G0
    REAL(r64) :: G1
    REAL(r64) :: GJ
    REAL(r64) :: HJ
    REAL(r64), DIMENSION(12) :: T
    REAL(r64) :: X2J


    KBessFunc=0.0d0
    G0=0.0d0
    GJ=0.0d0

    IF (BessFuncOrd.LT.0.0d0) THEN
      ErrorCode=1
      RETURN
    ELSE IF (BessFuncArg.LE.0.0d0) THEN
      ErrorCode=2
      RETURN
    ELSE IF(BessFuncArg.GT.85.d0) THEN
      ErrorCode=3
      KBessFunc=0.0d0
      RETURN
    ENDIF

    ErrorCode=0

  !     Use polynomial approximation if BessFuncArg > 1.

    IF (BessFuncArg.GT.1.0d0) THEN
      T(1)=1.d0/BessFuncArg
      DO LoopCount = 2,12
        T(LoopCount)=T(LoopCount-1)/BessFuncArg
      END DO !End of LoopCount Loop
      IF (BessFuncOrd.NE.1) THEN

  !     Compute K0 using polynomial approximation

        G0=EXP(-BessFuncArg)*(1.2533141d0-.1566642d0*T(1)+.08811128d0*T(2)-.09139095d0    &
           *T(3)+.1344596d0*T(4)-.2299850d0*T(5)+.3792410d0*T(6)-.5247277d0  &
           *T(7)+.5575368d0*T(8)-.4262633d0*T(9)+.2184518d0*T(10)          &
           -.06680977d0*T(11)+.009189383d0*T(12))*SQRT(1.d0/BessFuncArg)
        IF (BessFuncOrd .EQ. 0) THEN
          KBessFunc=G0
          RETURN
        ENDIF
      ENDIF

  !     Compute K1 using polynomial approximation

      G1=EXP(-BessFuncArg)*(1.2533141d0+.4699927d0*T(1)-.1468583d0*T(2)+.1280427d0*T(3)    &
         -.1736432d0*T(4)+.2847618d0*T(5)-.4594342d0*T(6)+.6283381d0*T(7)     &
         -.6632295d0*T(8)+.5050239d0*T(9)-.2581304d0*T(10)+.07880001d0*T(11)  &
         -.01082418d0*T(12))*SQRT(1.d0/BessFuncArg)
      IF (BessFuncOrd .EQ. 1) THEN
        KBessFunc=G1
        RETURN
      ENDIF
    ELSE

  !     Use series expansion if BessFuncArg <= 1.

      IF (BessFuncOrd.NE.1) THEN

  !     Compute K0 using series expansion

        G0=-(.5772157d0+LOG(BessFuncArg/2.d0))
        X2J=1.d0
        FACT=1.d0
        HJ=0.0d0
        DO LoopCount = 1,6
          X2J=X2J*BessFuncArg**2/4.0d0
          FACT=FACT*(1.0d0/REAL(LoopCount,r64))**2
          HJ=HJ+1.0d0/REAL(LoopCount,r64)
          G0=G0+X2J*FACT*(HJ-(.5772157d0+LOG(BessFuncArg/2.d0)))
        END DO !End of LoopCount Loop
        IF (BessFuncOrd.EQ.0.0d0) THEN
          KBessFunc=G0
          RETURN
        ENDIF
      ENDIF

  !     Compute K1 using series expansion

      X2J=BessFuncArg/2.0d0
      FACT=1.d0
      HJ=1.d0
      G1=1.0d0/BessFuncArg+X2J*(.5d0+(.5772157d0+LOG(BessFuncArg/2.d0))-HJ)
      DO LoopCount = 2,8
        X2J=X2J*BessFuncArg**2/4.0d0
        FACT=FACT*(1.0d0/REAL(LoopCount,r64))**2
        HJ=HJ+1.0d0/REAL(LoopCount,r64)
        G1=G1+X2J*FACT*(.5d0+((.5772157d0+LOG(BessFuncArg/2.d0))-HJ)*REAL(LoopCount,r64))
      END DO !End of LoopCount Loop
      IF (BessFuncOrd.EQ.1) THEN
        KBessFunc=G1
        RETURN
      ENDIF
    ENDIF

  !     From K0 and K1 compute KN using recurrence relation

    LoopCount = 2
    StopLoop = .FALSE.
    DO WHILE (LoopCount .LE. BessFuncOrd .AND. .NOT. StopLoop)
      GJ=2.d0*(REAL(LoopCount,r64)-1.0d0)*G1/BessFuncArg+G0
      IF (GJ-GJMAX .GT. 0.0d0) THEN
        ErrorCode=4
        GJ=GJMAX
        StopLoop = .TRUE.
      ELSE
        G0=G1
        G1=GJ
        LoopCount = LoopCount + 1
      END IF
    END DO !End of LoopCount Loop
    KBessFunc=GJ

    RETURN
END SUBROUTINE CalcKBesselFunc

SUBROUTINE CalcPolynomCoef(OrderedPair,PolynomCoef)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR   Unknown
          !       DATE WRITTEN   Unknown
          !       DATE REWRITTEN  April 1997 by Russell D. Taylor, Ph.D.
          !       MODIFIED
          !       RE-ENGINEERED

          ! PURPOSE OF THIS SUBROUTINE:
          ! Fits polynomial of order from 1 to MaxPolynomOrder to the
          ! ordered pairs of data points X,Y

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
    REAL(r64), DIMENSION(MaxPolynomOrder+1) :: PolynomCoef
    REAL(r64), DIMENSION(2,MaxOrderedPairs) :: OrderedPair

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    LOGICAL :: Converged
    REAL(r64), DIMENSION(2,10) :: OrdPairSum
    REAL(r64), DIMENSION(10,10) :: OrdPairSumMatrix
    REAL(r64) B
    INTEGER I,II,J
    INTEGER :: PolynomOrder
    INTEGER :: CurrentOrder
    INTEGER :: CurrentOrdPair
    REAL(r64) S1,S2

    OrdPairSum(1,1) = MaxOrderedPairs
    OrdPairSum(1,2:3) = 0.0d0
    OrdPairSum(2,:) = 0.0d0
    PolynomCoef = 0.0d0
    DO CurrentOrdPair = 1, MaxOrderedPairs
      OrdPairSum(1,2) = OrdPairSum(1,2) + OrderedPair(1, CurrentOrdPair)
      OrdPairSum(1,3) = OrdPairSum(1,3) + OrderedPair(1, CurrentOrdPair) * &
                        OrderedPair(1, CurrentOrdPair)
      OrdPairSum(2,1) = OrdPairSum(2,1) + OrderedPair(2, CurrentOrdPair)
      OrdPairSum(2,2) = OrdPairSum(2,2) + OrderedPair(1, CurrentOrdPair) * &
                        OrderedPair(2, CurrentOrdPair)
    END DO
    PolynomOrder = 1
    Converged = .FALSE.
    DO WHILE (.NOT. Converged)
      DO CurrentOrder = 1, PolynomOrder + 1
        DO J = 1, PolynomOrder + 1
          OrdPairSumMatrix(CurrentOrder, J) = OrdPairSum(1,J - 1 + CurrentOrder)
        END DO !End of J loop
        OrdPairSumMatrix(CurrentOrder, (PolynomOrder + 2)) = OrdPairSum(2,CurrentOrder)
      END DO !End of CurrentOrder loop

      DO CurrentOrder = 1, PolynomOrder + 1
        OrdPairSumMatrix((PolynomOrder + 2), CurrentOrder)= -1.d0
        DO J = CurrentOrder + 1,(PolynomOrder + 2)
          OrdPairSumMatrix((PolynomOrder + 2), J) = 0.0d0
        END DO !End of J loop

        DO II = 2, (PolynomOrder + 2)
          DO J = CurrentOrder + 1, (PolynomOrder + 2)
            OrdPairSumMatrix(II, J) = OrdPairSumMatrix(II, J) - OrdPairSumMatrix(1, J) &
                                      * OrdPairSumMatrix(II, CurrentOrder) / &
                                      OrdPairSumMatrix(1, CurrentOrder)
          END DO !End of J loop
        END DO !End of II loop
        DO II = 1, PolynomOrder + 1
          DO J = CurrentOrder + 1, (PolynomOrder + 2)
            OrdPairSumMatrix(II, J)=OrdPairSumMatrix(II + 1, J)
          END DO !End of J loop
        END DO !End of II loop
      END DO !End of CurrentOrder loop

      S2 = 0.0d0
      DO CurrentOrdPair = 1, MaxOrderedPairs
        S1 = OrdPairSumMatrix(1, (PolynomOrder + 2))
        DO CurrentOrder = 1, PolynomOrder
          S1 = S1 + OrdPairSumMatrix(CurrentOrder + 1,(PolynomOrder + 2)) &
               * OrderedPair(1, CurrentOrdPair)**CurrentOrder
        END DO !End of CurrentOrder loop
        S2 = S2 + (S1 - OrderedPair(2, CurrentOrdPair)) * &
             (S1 - OrderedPair(2, CurrentOrdPair))
      END DO !End of CurrentOrdPair loop
      B = MaxOrderedPairs - (PolynomOrder + 1)
      IF(S2 .GT. 0.0001d0) S2 = SQRT(S2 / B)
      DO CurrentOrder = 1, PolynomOrder + 1
        PolynomCoef(CurrentOrder) = OrdPairSumMatrix(CurrentOrder, (PolynomOrder + 2))
      END DO !End of CurrentOrder loop

      IF (((PolynomOrder - MaxPolynomOrder) .LT. 0.0d0) .AND. &
          ((S2 - PolyConvgTol) .GT. 0.0d0)) THEN
        PolynomOrder = PolynomOrder + 1
        J = 2 * PolynomOrder
        OrdPairSum(1, J:J+1) = 0.0d0
        OrdPairSum(2, PolynomOrder + 1)=0.0d0
        DO I = 1, MaxOrderedPairs
          OrdPairSum(1, J) = OrdPairSum(1, J) + OrderedPair(1, I)**(J-1)
          OrdPairSum(1, J+1)=OrdPairSum(1, J+1) + OrderedPair(1, I)**J
          OrdPairSum(2, PolynomOrder+1) = OrdPairSum(2, PolynomOrder+1) + &
                                          OrderedPair(2, I) * OrderedPair(1, I)**PolynomOrder
        END DO
      ELSE
        Converged = .TRUE.
      END IF

    END DO

    RETURN
END SUBROUTINE CalcPolynomCoef

FUNCTION SimpleHeatingCoilUAResidual(UA, Par) RESULT (Residuum)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   November 2001
          !       MODIFIED
          !       RE-ENGINEERED

          ! PURPOSE OF THIS FUNCTION:
          ! Calculates residual function (Design Coil Load - Coil Heating Output) / Design Coil Load.
          ! Coil Heating Output depends on the UA which is being varied to zero the residual.

          ! METHODOLOGY EMPLOYED:
          ! Puts UA into the water coil data structure, calls CalcSimpleHeatingCoil, and calculates
          ! the residual as defined above.

          ! REFERENCES:

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
    REAL(r64), INTENT(IN)  :: UA ! UA of coil
    REAL(r64), INTENT(IN), DIMENSION(:), OPTIONAL :: Par ! par(1) = design coil load [W]
    REAL(r64)         :: Residuum ! residual to be minimized to zero

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER :: CoilIndex
  INTEGER :: FanOpMode
  REAL(r64)    :: PartLoadRatio

  CoilIndex = INT(Par(2))
  IF(Par(3) .EQ. 1.0d0) THEN
    FanOpMode = CycFanCycCoil
  ELSE
    FanOpMode = ContFanCycCoil
  END IF
  PartLoadRatio = Par(4)
  WaterCoil(CoilIndex)%UACoilVariable = UA
  CALL CalcSimpleHeatingCoil(CoilIndex, FanOpMode, PartLoadRatio, SimCalc)
  Residuum = (Par(1) - WaterCoil(CoilIndex)%TotWaterHeatingCoilRate) / Par(1)

  RETURN
END FUNCTION SimpleHeatingCoilUAResidual

FUNCTION SimpleCoolingCoilUAResidual(UA, Par) RESULT (Residuum)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   September 2011
          !       MODIFIED
          !       RE-ENGINEERED

          ! PURPOSE OF THIS FUNCTION:
          ! Calculates residual function (Design Coil Load - Coil Cooling Output) / Design Coil Load.
          ! Coil Cooling Output depends on the UA which is being varied to zero the residual.

          ! METHODOLOGY EMPLOYED:
          ! Puts UA into the water coil data structure, calls CoolingCoil, and calculates
          ! the residual as defined above.

          ! REFERENCES:

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
    REAL(r64), INTENT(IN)  :: UA ! UA of coil
    REAL(r64), INTENT(IN), DIMENSION(:), OPTIONAL :: Par ! par(1) = design coil load [W]
    REAL(r64)         :: Residuum ! residual to be minimized to zero

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER :: CoilIndex
  INTEGER :: FanOpMode
  REAL(r64)    :: PartLoadRatio

  CoilIndex = INT(Par(2))
  IF(Par(3) .EQ. 1.0d0) THEN
    FanOpMode = CycFanCycCoil
  ELSE
    FanOpMode = ContFanCycCoil
  END IF
  PartLoadRatio = Par(4)
  WaterCoil(CoilIndex)%UACoilExternal = UA
  WaterCoil(CoilIndex)%UACoilInternal = WaterCoil(CoilIndex)%UACoilExternal*3.3d0
  WaterCoil(CoilIndex)%UACoilTotal = 1.0d0/(1.d0/WaterCoil(CoilIndex)%UACoilExternal+1.d0/WaterCoil(CoilIndex)%UACoilInternal)
  WaterCoil(CoilIndex)%TotCoilOutsideSurfArea=EstimateHEXSurfaceArea(CoilIndex)
  WaterCoil(CoilIndex)%UACoilInternalPerUnitArea=WaterCoil(CoilIndex)%UACoilInternal/  &
                                                 WaterCoil(CoilIndex)%TotCoilOutsideSurfArea
  WaterCoil(CoilIndex)%UAWetExtPerUnitArea=WaterCoil(CoilIndex)%UACoilExternal/WaterCoil(CoilIndex)%TotCoilOutsideSurfArea
  WaterCoil(CoilIndex)%UADryExtPerUnitArea=WaterCoil(CoilIndex)%UAWetExtPerUnitArea

  CALL CoolingCoil(CoilIndex, .TRUE., DesignCalc, FanOpMode, PartLoadRatio)

  Residuum = (Par(1) - WaterCoil(CoilIndex)%TotWaterCoolingCoilRate) / Par(1)

  RETURN
END FUNCTION SimpleCoolingCoilUAResidual

! Iterate Routine for Cooling Coil
Subroutine CoilAreaFracIter(NewSurfAreaWetFrac, SurfAreaFracCurrent,ErrorCurrent,SurfAreaFracPrevious, &
                                       ErrorPrevious,SurfAreaFracLast,ErrorLast,IterNum,ICvg)
          ! FUNCTION INFORMATION:
          ! AUTHOR         Rahul Chillar
          ! DATE WRITTEN   June 2004
          ! MODIFIED       na
          ! RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! Iterately solves for the value of SurfAreaWetFraction for the Cooling Coil.

          ! METHODOLOGY EMPLOYED:
          ! First function generates 2 sets of guess points by perturbation and subsequently
          ! by Linear Fit and using the generated points calculates coeffecients for Quadratic
          ! fit to predict the next value of surface area wet fraction.

          ! REFERENCES:
          ! ME 423 Design of Thermal Systems Class Notes.UIUC. W.F.Stoecker

          ! USE STATEMENTS:
          ! na

          ! Enforce explicit typing of all variables in this routine
       Implicit None

          ! FUNCTION ARGUMENT DEFINITIONS:
       REAL(r64), intent(Out):: NewSurfAreaWetFrac    ! Out Value of variable
       REAL(r64), intent(in) :: SurfAreaFracCurrent   ! Driver Value
       REAL(r64), intent(in) :: ErrorCurrent          ! Objective Function
       REAL(r64), intent(inout) :: ErrorPrevious          ! First Previous value of error
       REAL(r64), intent(inout) :: ErrorLast              ! Second Previous value of error
       REAL(r64), intent(inout) :: SurfAreaFracPrevious   ! First Previous value of Surf Area Fraction
       REAL(r64), intent(inout) :: SurfAreaFracLast       ! Second Previous value of Surf Area Fraction
       Integer, intent(in) :: IterNum             ! Number of Iterations
       Integer, intent(inout)   :: icvg                ! Iteration convergence flag

          ! FUNCTION PARAMETER DEFINITIONS:
       REAL(r64), Parameter:: Tolerance =1.d-5         ! Relative error tolerance
       REAL(r64), Parameter:: PerturbSurfAreaFrac= 0.1d0 ! Perturbation applied to Surf Fraction to initialize iteration
       REAL(r64), Parameter:: SmallNum= 1.d-9          ! Small Number

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
       REAL(r64) :: check                          ! Validity Check for moving to Quad Solution
       REAL(r64) :: QuadCoefThree                  ! Term under radical in quadratic solution
       REAL(r64) :: QuadCoefOne            ! Term under radical in quadratic solution
       REAL(r64) :: QuadCoefTwo            ! Term under radical in quadratic solution
       REAL(r64) :: Slope                  ! Slope for linear fit
       REAL(r64) :: SurfAreaFracOther      ! Intermediate Value of Surf Area
       Integer :: mode                ! Linear/ perturbation option

        ! Convergence Check  by comparing previous and current value of surf area fraction
      IF ((ABS(SurfAreaFracCurrent-SurfAreaFracPrevious) .LT. Tolerance*MAX(ABS(SurfAreaFracCurrent),SmallNum) .AND.               &
          IterNum .NE. 1) .OR. ErrorCurrent .EQ. 0.0d0) THEN
        ! Setting value for surface area fraction for coil
        NewSurfAreaWetFrac = SurfAreaFracCurrent
        ICvg=1  ! Convergance Flag
        RETURN
      ENDIF

        ! If Icvg = 0 , it has not converged.By perturbation for getting second set of
        ! data (mode=1), Getting Third set of data by performing a  linear fit(Mode=2).
        ! Now using the above 3 points generated by perturbation and Linear Fit to perform
        ! a quadratic fit.This will happen after second iteration only.
      ICvg=0 ! Convergance flag = false
        ! For First Iteration Start with perturbation, For second iteration start with linear fit
        ! from the previous two values
      mode=IterNum

10    IF (mode .EQ. 1) THEN

        ! FirstGuess Set of Points provided by perturbation
        IF (ABS(SurfAreaFracCurrent) .GT. SmallNum) THEN
          NewSurfAreaWetFrac = SurfAreaFracCurrent*(1.d0+PerturbSurfAreaFrac)
        ELSE
          NewSurfAreaWetFrac = PerturbSurfAreaFrac
        ENDIF

        ! Second set of values being calculated from the first set of values (incoming & perturb)
      ELSEIF (mode .EQ. 2) THEN

        ! Calculating Slope for interpolating to the New Point (Simple Linear Extrapolation)
        Slope=(ErrorPrevious-ErrorCurrent)/(SurfAreaFracPrevious-SurfAreaFracCurrent)
        ! Error Check for value or Slope
           IF(Slope.EQ.0.0d0) THEN
              mode=1 ! Go back to Perturbation
              GO TO 10
           ENDIF
        ! Guessing New Value for Surface Area Fraction
        NewSurfAreaWetFrac=SurfAreaFracCurrent-ErrorCurrent/Slope
      ELSE

        ! Check for Quadratic Fit possible here ,Previous value of surf area fraction
        ! equals current value then Try linear fit for another point.
        IF (SurfAreaFracCurrent .EQ. SurfAreaFracPrevious) THEN
        ! Assign Value of previous point to Last Variable for storing
        ! Go back and calculate new value for Previous.
          SurfAreaFracPrevious=SurfAreaFracLast
          ErrorPrevious=ErrorLast
          mode=2
          GO TO 10
        ELSEIF (SurfAreaFracCurrent .EQ. SurfAreaFracLast) THEN
        ! Calculate another value using Linear Fit.
          mode=2
          GO TO 10
        ENDIF

        ! Now We have enough previous points to calculate coefficients and
        ! perform a quadratic fit for new guess value of surface area fraction

        ! Calculating First Coefficients for Quadratic Curve Fit
        QuadCoefThree=((ErrorLast-ErrorCurrent)/(SurfAreaFracLast-SurfAreaFracCurrent)- &
                     (ErrorPrevious-ErrorCurrent)/(SurfAreaFracPrevious-SurfAreaFracCurrent))/ &
                                                            (SurfAreaFracLast-SurfAreaFracPrevious)
        ! Calculating Second Coefficients for Quadratic Curve Fit
        QuadCoefTwo=(ErrorPrevious-ErrorCurrent)/   &
                                    (SurfAreaFracPrevious-SurfAreaFracCurrent)-  &
                                      (SurfAreaFracPrevious+SurfAreaFracCurrent)*QuadCoefThree

        ! Calculating Third Coefficients for Quadratic Curve Fit
        QuadCoefOne=ErrorCurrent-(QuadCoefTwo+QuadCoefThree*SurfAreaFracCurrent)*SurfAreaFracCurrent

        ! Check for validity of coefficients , if not REAL(r64) ,Then fit is linear
        IF (ABS(QuadCoefThree) .LT. 1.D-10) THEN
          mode=2  ! going to Linear mode, due to colinear points.
          GO TO 10
        ENDIF

        ! If value of Quadratic coefficients not suitable enought due to round off errors
        ! to predict new point go to linear fit and acertain new values for the coefficients.
        IF (ABS((QuadCoefOne+(QuadCoefTwo+QuadCoefThree*SurfAreaFracPrevious)* &
                  SurfAreaFracPrevious-ErrorPrevious)/ErrorPrevious) .GT. 1.D-4) THEN
           mode=2 ! go to linear mode
           GO TO 10
        ENDIF

        ! Validity Check for Imaginary roots, In this case go back to linear fit.
        check=QuadCoefTwo**2-4.0d0*QuadCoefOne*QuadCoefThree
        ! Imaginary Root Exist
        IF (check .LT. 0) THEN
           mode=2
           GO TO 10
        ELSEIF (check .GT. 0) THEN
          ! real unequal roots exist, Determine the roots nearest to most recent guess
          NewSurfAreaWetFrac=(-QuadCoefTwo+SQRT(check))/QuadCoefThree/2.0d0
          SurfAreaFracOther=-NewSurfAreaWetFrac-QuadCoefTwo/QuadCoefThree
          ! Assigning value to Surface Area Fraction with recent
          IF (ABS(NewSurfAreaWetFrac-SurfAreaFracCurrent) .GT. &
                                   ABS(SurfAreaFracOther-SurfAreaFracCurrent)) NewSurfAreaWetFrac=SurfAreaFracOther
        ELSE
          ! The roots are real, one solution exists.
          NewSurfAreaWetFrac=-QuadCoefTwo/QuadCoefThree/2
        ENDIF

      ENDIF

      IF (mode .LT. 3) THEN
        ! No valid previous points to eliminate, since it just has 2 points.
        ! Loading previous values into last
        SurfAreaFracLast=SurfAreaFracPrevious
        ErrorLast=ErrorPrevious
        ! Loading Current Values into previous
        SurfAreaFracPrevious=SurfAreaFracCurrent
        ErrorPrevious=ErrorCurrent
      ELSE

        ! Elimination the most distance previous point from the answer based on sign and
        ! magnitute of the error. Keeping Current Point
        IF (ErrorPrevious*ErrorCurrent .GT. 0 .AND. ErrorLast*ErrorCurrent .GT. 0) THEN
            ! If sign are same , simply eliminate the one with biggest error value.
          IF (ABS(ErrorLast) .GT. ABS(ErrorPrevious)) THEN
            ! Eliminating Last Value
            SurfAreaFracLast=SurfAreaFracPrevious
            ErrorLast=ErrorPrevious
          ENDIF
        ELSE
            ! If signs are different eliminate previous error with same sign as current error
          IF (ErrorLast*ErrorCurrent .GT. 0)  THEN
            ! Previous Loaded to Last
            SurfAreaFracLast=SurfAreaFracPrevious
            ErrorLast=ErrorPrevious
          ENDIF
        ENDIF
        ! Current Loaded into previous.
        SurfAreaFracPrevious=SurfAreaFracCurrent
        ErrorPrevious=ErrorCurrent
      ENDIF
      RETURN

END SUBROUTINE CoilAreaFracIter

SUBROUTINE CheckWaterCoilSchedule(CompType,CompName,Value,CompIndex)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   October 2005
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! <description>

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: FindItemInList
  USE General, ONLY: TrimSigDigits

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: CompType !unused1208
  CHARACTER(len=*), INTENT(IN) :: CompName
  REAL(r64), INTENT(OUT)            :: Value
  INTEGER, INTENT(INOUT)       :: CompIndex

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER CoilNum

  ! Obtains and Allocates WaterCoil related parameters from input file
  IF (GetWaterCoilsInputFlag) THEN  !First time subroutine has been entered
    CALL GetWaterCoilInput
    GetWaterCoilsInputFlag=.false.
  End If

  ! Find the correct Coil number
  IF (CompIndex == 0) THEN
    CoilNum = FindItemInList(CompName,WaterCoil%Name,NumWaterCoils)
    IF (CoilNum == 0) THEN
      CALL ShowFatalError('CheckWaterCoilSchedule: Coil not found='//TRIM(CompName))
    ENDIF
    CompIndex=CoilNum
    Value=GetCurrentScheduleValue(WaterCoil(CoilNum)%SchedPtr)  ! not scheduled?
  ELSE
    CoilNum=CompIndex
    IF (CoilNum > NumWaterCoils .or. CoilNum < 1) THEN
      CALL ShowFatalError('CheckWaterCoilSchedule: Invalid CompIndex passed='//  &
                          TRIM(TrimSigDigits(CoilNum))// &
                          ', Number of Heating Coils='//TRIM(TrimSigDigits(NumWaterCoils))//  &
                          ', Coil name='//TRIM(CompName))
    ENDIF
    IF (CompName /= WaterCoil(CoilNum)%Name) THEN
      CALL ShowFatalError('CheckWaterCoilSchedule: Invalid CompIndex passed='//  &
                          TRIM(TrimSigDigits(CoilNum))// &
                          ', Coil name='//TRIM(CompName)//', stored Coil Name for that index='//  &
                          TRIM(WaterCoil(CoilNum)%Name))
    ENDIF
    Value=GetCurrentScheduleValue(WaterCoil(CoilNum)%SchedPtr)  ! not scheduled?
  ENDIF

  RETURN

END SUBROUTINE CheckWaterCoilSchedule

FUNCTION GetCoilMaxWaterFlowRate(CoilType,CoilName,ErrorsFound) RESULT(MaxWaterFlowRate)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   November 2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function looks up the max water flow rate for the given coil and returns it.  If
          ! incorrect coil type or name is given, errorsfound is returned as true and capacity is returned
          ! as negative.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor,  ONLY: FindItem,SameString

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: CoilType     ! must match coil types in this module
  CHARACTER(len=*), INTENT(IN) :: CoilName     ! must match coil names for the coil type
  LOGICAL, INTENT(INOUT)       :: ErrorsFound  ! set to true if problem
  REAL(r64)                    :: MaxWaterFlowRate  ! returned max water flow rate of matched coil

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER :: WhichCoil

  ! Obtains and Allocates WaterCoil related parameters from input file
  IF (GetWaterCoilsInputFlag) THEN  !First time subroutine has been entered
    CALL GetWaterCoilInput
    GetWaterCoilsInputFlag=.false.
  End If

  WhichCoil=0
  IF (SameString(CoilType,'Coil:Heating:Water')       .or.  &
      SameString(CoilType,'Coil:Cooling:Water:DetailedGeometry') .or.  &
      SameString(CoilType,'Coil:Cooling:Water')) THEN
    WhichCoil=FindItem(CoilName,WaterCoil%Name,NumWaterCoils)
    IF (WhichCoil /= 0) THEN
      ! coil does not specify MaxWaterFlowRate
      MaxWaterFlowRate=WaterCoil(WhichCoil)%MaxWaterVolFlowRate
    ENDIF
  ELSE
    WhichCoil=0
  ENDIF

  IF (WhichCoil == 0) THEN
    CALL ShowSevereError('GetCoilMaxWaterFlowRate: Could not find Coil, Type="'//TRIM(CoilType)//  &
                         '" Name="'//TRIM(CoilName)//'"')
    CALL ShowContinueError('... Max Water Flow rate returned as -1000.')
    ErrorsFound=.true.
    MaxWaterFlowRate=-1000.d0
  ENDIF

  RETURN

END FUNCTION GetCoilMaxWaterFlowRate

FUNCTION GetCoilInletNode(CoilType,CoilName,ErrorsFound) RESULT(NodeNumber)

          ! FUNCTION INFORMATION:
          !       AUTHOR         R. Raustad
          !       DATE WRITTEN   March 2007
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function looks up the given coil and returns the inlet node number.  If
          ! incorrect coil type or name is given, errorsfound is returned as true and node number is returned
          ! as zero.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor,  ONLY: FindItem,SameString

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: CoilType     ! must match coil types in this module
  CHARACTER(len=*), INTENT(IN) :: CoilName     ! must match coil names for the coil type
  LOGICAL, INTENT(INOUT)       :: ErrorsFound  ! set to true if problem
  INTEGER                      :: NodeNumber   ! returned node number of matched coil

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER :: WhichCoil

  ! Obtains and Allocates DXCoils
  IF (GetWaterCoilsInputFlag) THEN
    CALL GetWaterCoilInput
    GetWaterCoilsInputFlag = .FALSE.
  END IF

  NodeNumber=0
  WhichCoil=0
  IF (SameString(CoilType,'Coil:Heating:Water')       .or.  &
      SameString(CoilType,'Coil:Cooling:Water:DetailedGeometry') .or.  &
      SameString(CoilType,'Coil:Cooling:Water')) THEN
    WhichCoil=FindItem(CoilName,WaterCoil%Name,NumWaterCoils)
    IF (WhichCoil /= 0) THEN
      NodeNumber=WaterCoil(WhichCoil)%AirInletNodeNum
    ENDIF
  ELSE
    WhichCoil=0
  ENDIF

  IF (WhichCoil == 0) THEN
    CALL ShowSevereError('GetCoilInletNode: Could not find Coil, Type="'//TRIM(CoilType)//'" Name="'//TRIM(CoilName)//'"')
    ErrorsFound=.true.
    NodeNumber=0
  ENDIF

  RETURN

END FUNCTION GetCoilInletNode

FUNCTION GetCoilOutletNode(CoilType,CoilName,ErrorsFound) RESULT(NodeNumber)

          ! FUNCTION INFORMATION:
          !       AUTHOR         R. Raustad
          !       DATE WRITTEN   March 2007
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function looks up the given coil and returns the inlet node number.  If
          ! incorrect coil type or name is given, errorsfound is returned as true and node number is returned
          ! as zero.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor,  ONLY: FindItem,SameString

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: CoilType     ! must match coil types in this module
  CHARACTER(len=*), INTENT(IN) :: CoilName     ! must match coil names for the coil type
  LOGICAL, INTENT(INOUT)       :: ErrorsFound  ! set to true if problem
  INTEGER                      :: NodeNumber   ! returned node number of matched coil

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER :: WhichCoil

  ! Obtains and Allocates DXCoils
  IF (GetWaterCoilsInputFlag) THEN
    CALL GetWaterCoilinput
    GetWaterCoilsInputFlag = .FALSE.
  END IF

  WhichCoil=0
  NodeNumber=0
  IF (SameString(CoilType,'Coil:Heating:Water')       .or.  &
      SameString(CoilType,'Coil:Cooling:Water:DetailedGeometry') .or.  &
      SameString(CoilType,'Coil:Cooling:Water')) THEN
    WhichCoil=FindItem(CoilName,WaterCoil%Name,NumWaterCoils)
    IF (WhichCoil /= 0) THEN
      NodeNumber=WaterCoil(WhichCoil)%AirOutletNodeNum
    ENDIF
  ELSE
    WhichCoil=0
  ENDIF

  IF (WhichCoil == 0) THEN
    CALL ShowSevereError('GetCoilOutletNode: Could not find Coil, Type="'//TRIM(CoilType)//'" Name="'//TRIM(CoilName)//  &
       '" when accessing coil outlet node number.')
    ErrorsFound=.true.
    NodeNumber=0
  ENDIF

  RETURN

END FUNCTION GetCoilOutletNode

FUNCTION GetCoilWaterInletNode(CoilType,CoilName,ErrorsFound) RESULT(NodeNumber)

          ! FUNCTION INFORMATION:
          !       AUTHOR         R. Raustad
          !       DATE WRITTEN   July 2007
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function looks up the given coil and returns the inlet water control node number.  If
          ! incorrect coil type or name is given, errorsfound is returned as true and node number is returned
          ! as zero.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor,  ONLY: FindItem,SameString

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: CoilType     ! must match coil types in this module
  CHARACTER(len=*), INTENT(IN) :: CoilName     ! must match coil names for the coil type
  LOGICAL, INTENT(INOUT)       :: ErrorsFound  ! set to true if problem
  INTEGER                      :: NodeNumber   ! returned node number of matched coil

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER :: WhichCoil

  ! Obtains and Allocates DXCoils
  IF (GetWaterCoilsInputFlag) THEN
    CALL GetWaterCoilInput
    GetWaterCoilsInputFlag = .FALSE.
  END IF

  NodeNumber=0
  WhichCoil=0
  IF (SameString(CoilType,'Coil:Heating:Water')       .or.  &
      SameString(CoilType,'Coil:Cooling:Water:DetailedGeometry') .or.  &
      SameString(CoilType,'Coil:Cooling:Water')) THEN
    WhichCoil=FindItem(CoilName,WaterCoil%Name,NumWaterCoils)
    IF (WhichCoil /= 0) THEN
      NodeNumber=WaterCoil(WhichCoil)%WaterInletNodeNum
    ENDIF
  ELSE
    WhichCoil=0
  ENDIF

  IF (WhichCoil == 0) THEN
    CALL ShowSevereError('GetCoilWaterInletNode: Could not find Coil, Type="'//TRIM(CoilType)//'" Name="'//TRIM(CoilName)//'"')
    ErrorsFound=.true.
    NodeNumber=0
  ENDIF

  RETURN

END FUNCTION GetCoilWaterInletNode

FUNCTION GetCoilWaterOutletNode(CoilType,CoilName,ErrorsFound) RESULT(NodeNumber)

          ! FUNCTION INFORMATION:
          !       AUTHOR         R. Raustad
          !       DATE WRITTEN   July 2007
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function looks up the given coil and returns the outlet water node number.  If
          ! incorrect coil type or name is given, errorsfound is returned as true and node number is returned
          ! as zero.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor,  ONLY: FindItem,SameString

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: CoilType     ! must match coil types in this module
  CHARACTER(len=*), INTENT(IN) :: CoilName     ! must match coil names for the coil type
  LOGICAL, INTENT(INOUT)       :: ErrorsFound  ! set to true if problem
  INTEGER                      :: NodeNumber   ! returned node number of matched coil

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER :: WhichCoil

  ! Obtains and Allocates DXCoils
  IF (GetWaterCoilsInputFlag) THEN
    CALL GetWaterCoilInput
    GetWaterCoilsInputFlag = .FALSE.
  END IF

  NodeNumber=0
  WhichCoil=0
  IF (SameString(CoilType,'Coil:Heating:Water')       .or.  &
      SameString(CoilType,'Coil:Cooling:Water:DetailedGeometry') .or.  &
      SameString(CoilType,'Coil:Cooling:Water')) THEN
    WhichCoil=FindItem(CoilName,WaterCoil%Name,NumWaterCoils)
    IF (WhichCoil /= 0) THEN
      NodeNumber=WaterCoil(WhichCoil)%WaterOutletNodeNum
    ENDIF
  ELSE
    WhichCoil=0
  ENDIF

  IF (WhichCoil == 0) THEN
    CALL ShowSevereError('GetCoilWaterOutletNode: Could not find Coil, Type="'//TRIM(CoilType)//'" Name="'//TRIM(CoilName)//'"')
    ErrorsFound=.true.
    NodeNumber=0
  ENDIF

  RETURN

END FUNCTION GetCoilWaterOutletNode

SUBROUTINE SetCoilDesFlow(CoilType,CoilName,CoilDesFlow, ErrorsFound)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   May 2009
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This routine is designed to set the design air volume flow rate in the
          ! water coil data structure. Some of the coil types do not have this datum as
          ! an input parameter and it is needed for calculating capacity for output reporting.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor,  ONLY: FindItem,SameString

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: CoilType     ! must match coil types in this module
  CHARACTER(len=*), INTENT(IN) :: CoilName     ! must match coil names for the coil type
  LOGICAL, INTENT(INOUT)       :: ErrorsFound  ! set to true if problem
  REAL(r64), INTENT(IN)        :: CoilDesFlow  ! coil volumetric air flow rate [m3/s]

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: WhichCoil   ! index to coil

   IF (GetWaterCoilsInputFlag) THEN  !First time subroutine has been entered
    CALL GetWaterCoilInput
    GetWaterCoilsInputFlag=.false.
  End If

  IF (SameString(CoilType,'Coil:Heating:Water') .or. SameString(CoilType,'Coil:Cooling:Water:DetailedGeometry') .or. &
      SameString(CoilType,'Coil:Cooling:Water')) THEN
    WhichCoil=FindItem(CoilName,WaterCoil%Name,NumWaterCoils)
    IF (WhichCoil /= 0) THEN
      IF (SameString(CoilType,'Coil:Cooling:Water') .and. WaterCoil(WhichCoil)%DesAirVolFlowRate < 0.0d0) THEN
        WaterCoil(WhichCoil)%DesAirVolFlowRate = CoilDesFlow
      ELSE
        WaterCoil(WhichCoil)%DesAirVolFlowRate = CoilDesFlow
      END IF
    ELSE
      CALL ShowSevereError('GetCoilMaxWaterFlowRate: Could not find Coil, Type="'//TRIM(CoilType)//  &
                         '" Name="'//TRIM(CoilName)//'"')
      ErrorsFound=.true.
    ENDIF
  ENDIF

  RETURN

END SUBROUTINE SetCoilDesFlow

SUBROUTINE CheckActuatorNode(ActuatorNodeNum, iNodeType, NodeNotFound)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   January 2009
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This subroutine checks that the input actuator node number is matched by
          ! the water inlet node number of some water coil

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)  :: ActuatorNodeNum   ! input actuator node number
  INTEGER, INTENT(OUT) :: iNodeType         ! Cooling or Heating or 0
  LOGICAL, INTENT(OUT) :: NodeNotFound      ! true if matching water inlet node not found

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER :: WhichCoil
  INTEGER :: CoilNum

  ! Obtains and Allocates DXCoils
  IF (GetWaterCoilsInputFlag) THEN
    CALL GetWaterCoilInput
    GetWaterCoilsInputFlag = .FALSE.
  END IF

  WhichCoil = 0
  iNodeType = 0
  NodeNotFound = .TRUE.
  DO CoilNum=1,NumWaterCoils
    IF (WaterCoil(CoilNum)%WaterInletNodeNum == ActuatorNodeNum) THEN
      WhichCoil = CoilNum
      iNodeType = WaterCoil(CoilNum)%WaterCoilType
      NodeNotFound = .FALSE.
    END IF
  END DO

  RETURN

END SUBROUTINE CheckActuatorNode

SUBROUTINE CheckForSensorAndSetpointNode(SensorNodeNum,ControlledVar,NodeNotFound)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Bereket Nigusse
          !       DATE WRITTEN   March 2013
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine checks that the sensor node number matches the air outlet node number
          ! of some water coils

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE SetPointManager,     ONLY: NodeHasSPMCtrlVarType, iCtrlVarType_Temp, iCtrlVarType_HumRat, iCtrlVarType_MaxHumRat
  USE EMSManager,          ONLY: CheckIfNodeSetpointManagedByEMS, iTemperatureSetpoint, iHumidityRatioMaxSetpoint

  !USE HVACControllers,     ONLY: iTemperature, iHumidityRatio, iTemperatureAndHumidityRatio

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)  :: SensorNodeNum      ! controller sensor node number
  INTEGER, INTENT(IN)  :: ControlledVar      ! controlled variable type
  LOGICAL, INTENT(OUT) :: NodeNotFound       ! true if matching air outlet node not found

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER :: RoutineName='CheckForSensorAndSetpointNode: '
  INTEGER, PARAMETER :: iTemperature                 = 1
  INTEGER, PARAMETER :: iHumidityRatio               = 2
  INTEGER, PARAMETER :: iTemperatureAndHumidityRatio = 3

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: WhichCoil                       ! water coil index
  INTEGER :: CoilNum                         ! counter
  CHARACTER(len=40) :: WaterCoilType         ! water coil type
  LOGICAL           :: EMSSetpointErrorFlag  ! flag true is EMS is used to set node setpoints

  ! Obtains and Allocates DXCoils
  IF (GetWaterCoilsInputFlag) THEN
    CALL GetWaterCoilInput
    GetWaterCoilsInputFlag = .FALSE.
  END IF

  WhichCoil = 0
  NodeNotFound = .TRUE.

  DO CoilNum = 1, NumWaterCoils
     IF (SensorNodeNum /= WaterCoil(CoilNum)%AirOutletNodeNum) CYCLE
        NodeNotFound = .FALSE.
        WhichCoil = CoilNum
        EXIT
  END DO
  ! now if the sensor node is on the water coil air outlet node then check that
  ! a setpoint is also specified on the water coil outlet node
  IF ( .NOT. NodeNotFound) THEN
     IF ( WhichCoil > 0) THEN
        If(WaterCoil(CoilNum)%WaterCoilType_Num == WaterCoil_DetFlatFinCooling) Then
                WaterCoilType = 'Coil:Cooling:Water:DetailedGeometry'
        ElseIf(WaterCoil(CoilNum)%WaterCoilType_Num == WaterCoil_Cooling) Then
                WaterCoilType = 'Coil:Cooling:Water'
        ELSEIf(WaterCoil(CoilNum)%WaterCoilType_Num == WaterCoil_SimpleHeating) Then
                WaterCoilType = 'Coil:Heating:Water'
        ENDIF
        EMSSetpointErrorFlag = .FALSE.
        SELECT CASE (ControlledVar)
        CASE (iTemperature)
          CALL CheckIfNodeSetpointManagedByEMS(SensorNodeNum,iTemperatureSetpoint, EMSSetpointErrorFlag)
          IF (EMSSetpointErrorFlag) THEN
            IF(.NOT. NodeHasSPMCtrlVarType(SensorNodeNum, iCtrlVarType_Temp)) THEN
                CALL ShowWarningError(RoutineName//TRIM(WaterCoilType)//'="'//TRIM(WaterCoil(WhichCoil)%Name)//'". ')
                CALL ShowContinueError(' ..Temperature setpoint not found on coil air outlet node.')
                CALL ShowContinueError(' ..The setpoint may have been placed on a node downstream of the coil'// &
                                       ' or on an airloop outlet node.')
                CALL ShowContinueError(' ..Specify the setpoint and the sensor on the coil air outlet node when possible.')
            END IF
          ENDIF
        CASE (iHumidityRatio)
          CALL CheckIfNodeSetpointManagedByEMS(SensorNodeNum,iHumidityRatioMaxSetpoint, EMSSetpointErrorFlag)
          IF (EMSSetpointErrorFlag) THEN
            IF(.NOT. NodeHasSPMCtrlVarType(SensorNodeNum, iCtrlVarType_MaxHumRat)) THEN
                CALL ShowWarningError(RoutineName//TRIM(WaterCoilType)//'="'//TRIM(WaterCoil(WhichCoil)%Name)//'". ')
                CALL ShowContinueError(' ..Humidity ratio setpoint not found on coil air outlet node.')
                CALL ShowContinueError(' ..The setpoint may have been placed on a node downstream of the coil'// &
                                       ' or on an airloop outlet node.')
                CALL ShowContinueError(' ..Specify the setpoint and the sensor on the coil air outlet node when possible.')
            END IF
          ENDIF
        CASE (iTemperatureAndHumidityRatio)
          CALL CheckIfNodeSetpointManagedByEMS(SensorNodeNum,iTemperatureSetpoint, EMSSetpointErrorFlag)
          IF (EMSSetpointErrorFlag) THEN
            IF(.NOT. NodeHasSPMCtrlVarType(SensorNodeNum, iCtrlVarType_Temp)) THEN
                CALL ShowWarningError(RoutineName//TRIM(WaterCoilType)//'="'//TRIM(WaterCoil(WhichCoil)%Name)//'". ')
                CALL ShowContinueError(' ..Temperature setpoint not found on coil air outlet node.')
                CALL ShowContinueError(' ..The setpoint may have been placed on a node downstream of the coil'// &
                                       ' or on an airloop outlet node.')
                CALL ShowContinueError(' ..Specify the setpoint and the sensor on the coil air outlet node when possible.')
            END IF
          ENDIF
          EMSSetpointErrorFlag = .FALSE.
          CALL CheckIfNodeSetpointManagedByEMS(SensorNodeNum,iHumidityRatioMaxSetpoint, EMSSetpointErrorFlag)
          IF (EMSSetpointErrorFlag) THEN
            IF(.NOT. NodeHasSPMCtrlVarType(SensorNodeNum, iCtrlVarType_MaxHumRat)) THEN
                CALL ShowWarningError(RoutineName//TRIM(WaterCoilType)//'="'//TRIM(WaterCoil(WhichCoil)%Name)//'". ')
                CALL ShowContinueError(' ..Humidity ratio setpoint not found on coil air outlet node.')
                CALL ShowContinueError(' ..The setpoint may have been placed on a node downstream of the coil'// &
                                       ' or on an airloop outlet node.')
                CALL ShowContinueError(' ..Specify the setpoint and the sensor on the coil air outlet node when possible.')
            END IF
          ENDIF
        END SELECT

     ENDIF
  ENDIF

  RETURN

END SUBROUTINE CheckForSensorAndSetpointNode

FUNCTION TdbFnHRhPb(H,RH,PB) RESULT(T)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   April 1, 2009
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! Given the specific enthalpy, relative humidity, and the
          ! barometric pressure, the function returns the dry bulb temperature.

          ! METHODOLOGY EMPLOYED:
          ! Inverts PsyHFnTdbRhPb

          ! REFERENCES:
          ! none

          ! USE STATEMENTS:
  USE General,    ONLY: SolveRegulaFalsi,RoundSigDigits

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

            ! FUNCTION ARGUMENT DEFINITIONS:
  REAL(r64), intent(in) :: H      ! specific enthalpy {J/kg}
  REAL(r64), intent(in) :: RH     ! relative humidity value (0.0-1.0)
  REAL(r64), intent(in) :: PB     ! barometric pressure {Pascals}
  REAL(r64)        :: T      ! result=> humidity ratio

          ! FUNCTION PARAMETER DEFINITIONS:
  INTEGER, PARAMETER           :: itmax =10
  INTEGER, PARAMETER           :: MaxIte = 500        ! Maximum number of iterations
  REAL(r64), PARAMETER         :: Acc =  1.0d0       ! Accuracy of result

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER                  :: SolFla             ! Flag of solver
  REAL(r64)                :: T0                 ! lower bound for Tprov [C]
  REAL(r64)                :: T1                 ! upper bound for Tprov [C]
  REAL(r64)                :: Tprov =0.0d0         ! provisional value of drybulb temperature [C]
  REAL(r64), DIMENSION(3)  :: Par                ! Par(1) = desired enthaply H [J/kg]
                                                 ! Par(2) = desired relative humidity (0.0 - 1.0)
                                                 ! Par(3) = barometric pressure [N/m2 (Pascals)]

  T0 = 1.0d0
  T1 = 50.0d0
  Par(1) = H
  Par(2) = Rh
  Par(3) = Pb
  CALL SolveRegulaFalsi(Acc, MaxIte, SolFla, Tprov, EnthalpyResidual, T0, T1, Par)
  ! if the numerical inversion failed, issue error messages.
  IF (SolFla == -1) THEN
    CALL ShowSevereError('Calculation of drybulb temperature failed in TdbFnHRhPb(H,RH,PB)')
    CALL ShowContinueError('   Iteration limit exceeded')
    CALL ShowContinueError('   H=['//trim(RoundSigDigits(H,6))//'], RH=['//trim(RoundSigDigits(RH,4))//  &
                           '], PB=['//trim(RoundSigDigits(PB,5))//'].')
  ELSE IF (SolFla == -2) THEN
    CALL ShowSevereError('Calculation of drybulb temperature failed in TdbFnHRhPb(H,RH,PB)')
    CALL ShowContinueError('  Bad starting values for Tdb')
    CALL ShowContinueError('   H=['//trim(RoundSigDigits(H,6))//'], RH=['//trim(RoundSigDigits(RH,4))//  &
                           '], PB=['//trim(RoundSigDigits(PB,5))//'].')
  END IF
  IF (SolFla < 0) THEN
    T = 0.0d0
  ELSE
    T = Tprov
  END IF

  RETURN
END FUNCTION TdbFnHRhPb

FUNCTION EnthalpyResidual(Tprov, Par) RESULT (Residuum)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   April 2009
          !       MODIFIED
          !       RE-ENGINEERED

          ! PURPOSE OF THIS FUNCTION:
          ! Calculates residual function Hdesired - H(Tdb,Rh,Pb)

          ! METHODOLOGY EMPLOYED:
          ! Calls PsyHFnTdbRhPb

          ! REFERENCES:

          ! USE STATEMENTS:
  USE Psychrometrics,    ONLY: PsyHFnTdbRhPb
  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  REAL(r64), INTENT(IN)  :: Tprov                      ! test value of Tdb [C]
  REAL(r64), INTENT(IN), DIMENSION(:), OPTIONAL :: Par ! Par(1) = desired enthaply H [J/kg]
                                                       ! Par(2) = desired relative humidity (0.0 - 1.0)
                                                       ! Par(3) = barometric pressure [N/m2 (Pascals)]
  REAL(r64)         :: Residuum ! residual to be minimized to zero

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:

  Residuum = Par(1) - PsyHFnTdbRhPb(Tprov,Par(2),Par(3))

  RETURN
END FUNCTION EnthalpyResidual

FUNCTION EstimateHEXSurfaceArea(CoilNum) RESULT(SurfaceArea)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Bereket A Nigusse, FSEC
          !       DATE WRITTEN   July 2010
          !       MODIFIED
          !       RE-ENGINEERED

          ! PURPOSE OF THIS FUNCTION:
          ! Splits the UA value of a simple coil:cooling:water heat exchanger model into
          ! "A" and U" values.

          ! METHODOLOGY EMPLOYED:
          ! A typical design U overall heat transfer coefficient is used to split the "UA" into "A"
          ! and "U" values. Currently a constant U value calculated for a typical cooling coil is
          ! used. The assumptions used to calculate a typical U value are:
          !     (1) tube side water velocity of 2.0 [m/s]
          !     (2) inside to outside total surface area ratio (Ai/Ao) =  0.07 [-]
          !     (3) fins overall efficiency = 0.92 based on aluminum fin, 12 fins per inch, and
          !         fins area to total outside surafce area ratio of about 90%.
          !     (4) air side convection coefficient of 140.0 [W/m2C].  Assumes sensible convection
          !         of 58.0 [W/m2C] and 82.0 [W/m2C] sensible convection equivalent of the mass
          !         transfer coefficient converted using the approximate relation:
          !         hequivalent = hmasstransfer/Cpair.

          ! REFERENCES:

          ! USE STATEMENTS:
  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)    :: CoilNum         ! coil number, [-]
  REAL(r64)              :: SurfaceArea     ! Heat exchanger surface area, [m2]

          ! FUNCTION PARAMETER DEFINITIONS:
  REAL(r64), PARAMETER :: OverallFinEfficiency = 0.92d0  ! Assumes aluminum fins, 12 fins per inch, fins
                                                         ! area of about 90% of external surface area Ao.
  REAL(r64), PARAMETER :: AreaRatio = 0.07d0       ! Heat exchanger Inside to Outside surface area ratio
                                                   ! design values range from (Ai/Ao) = 0.06 to 0.08

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  REAL(r64) :: UOverallHeatTransferCoef      ! over all heat transfer coefficient for coil [W/m2C]
  REAL(r64) :: hAirTubeOutside               ! Air side heat transfer coefficient [W/m2C]
  REAL(r64) :: hWaterTubeInside              ! water (tube) side heat transfer coefficient [W/m2C]

  WaterCoil(CoilNum)%UACoilTotal = 1.d0/(1.d0/WaterCoil(CoilNum)%UACoilExternal &
                                 + 1.d0/WaterCoil(CoilNum)%UACoilInternal)

      ! Tube side water convection heat transfer coefficient of the cooling coil is calculated for
      ! inside tube diameter of 0.0122m (~0.5 inch nominal diameter) and water velocity 2.0 m/s:
  hWaterTubeInside = 1429.d0*(2.d0**0.8d0)*(0.0122d0**(-0.2d0))

      ! Constant value air side heat transfer coefficient is assumed. This coefficient has sensible
      ! (58.d0 [W/m2C]) and latent (82.d0 [W/m2C]) heat transfer coefficient components.
  hAirTubeOutside = 58.d0 + 82.d0

      ! Estimate the overall heat transfer coefficient, UOverallHeatTransferCoef in [W/(m2C)].
      ! Neglecting tube wall and fouling resistance, the overall U value can be estimated as:
      ! 1/UOverallHeatTransferCoef = 1/(hi*AreaRatio) + 1/(ho*OverallFinEfficiency)

  UOverallHeatTransferCoef = 1.d0/(1.d0/(hWaterTubeInside*AreaRatio) &
                           + 1.d0/(hAirTubeOutside*OverallFinEfficiency))

      ! the heat exchanger surface area is calculated as follows:
  SurfaceArea=WaterCoil(CoilNum)%UACoilTotal/UOverallHeatTransferCoef

  RETURN
  END Function EstimateHEXSurfaceArea

FUNCTION GetWaterCoilIndex(CoilType,CoilName,ErrorsFound) RESULT(IndexNum)

          ! FUNCTION INFORMATION:
          !       AUTHOR         B. Nigusse, FSEC
          !       DATE WRITTEN   Feb 2012
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function looks up the index for the given coil and returns it.  If incorrect coil
          ! type or name is given, errorsfound is returned as true and node number is returned
          ! as zero.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor,  ONLY: FindItemInList

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: CoilType     ! must match coil types in this module
  CHARACTER(len=*), INTENT(IN) :: CoilName     ! must match coil names for the coil type
  LOGICAL, INTENT(INOUT)       :: ErrorsFound  ! set to true if problem
  INTEGER                      :: IndexNum     ! returned coil index if matched coil

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
          ! na

  ! Obtains and allocates WaterCoil related parameters from input file
  IF (GetWaterCoilsInputFlag) THEN
    CALL GetWaterCoilInput
    GetWaterCoilsInputFlag = .FALSE.
  END IF

  IndexNum=0
  IF (CoilType == 'COIL:HEATING:WATER') THEN
    IndexNum=FindItemInList(CoilName,WaterCoil%Name,NumWaterCoils)
  ELSE IF (CoilType == 'COIL:COOLING:WATER') THEN
    IndexNum=FindItemInList(CoilName,WaterCoil%Name,NumWaterCoils)
  ELSE IF (CoilType == 'COIL:COOLING:WATER:DETAILEDGEOMETRY') THEN
    IndexNum=FindItemInList(CoilName,WaterCoil%Name,NumWaterCoils)
  ELSE
    IndexNum=0
  ENDIF

  IF (IndexNum == 0) THEN
    CALL ShowSevereError('GetWaterCoilIndex: Could not find CoilType="'//TRIM(CoilType)//  &
                         '" with Name="'//TRIM(CoilName)//'"')
    ErrorsFound=.true.
  ENDIF

  RETURN

END FUNCTION GetWaterCoilIndex

FUNCTION GetWaterCoilCapacity(CoilType,CoilName,ErrorsFound) RESULT(Capacity)

          ! FUNCTION INFORMATION:
          !       AUTHOR         R. Raustad, FSEC
          !       DATE WRITTEN   Sep 2013
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function looks up the capacity for the given coil and returns it.  If incorrect coil
          ! type or name is given, errorsfound is returned as true and capacity is returned
          ! as zero.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor,  ONLY: FindItemInList

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: CoilType     ! must match coil types in this module
  CHARACTER(len=*), INTENT(IN) :: CoilName     ! must match coil names for the coil type
  LOGICAL, INTENT(INOUT)       :: ErrorsFound  ! set to true if problem
  REAL(r64)                    :: Capacity     ! returned coil capacity if matched coil

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER :: IndexNum  ! index to water coil

  ! Obtains and allocates WaterCoil related parameters from input file
  IF (GetWaterCoilsInputFlag) THEN
    CALL GetWaterCoilInput
    GetWaterCoilsInputFlag = .FALSE.
  END IF

  Capacity=-1.0d0

  IF (CoilType == 'COIL:HEATING:WATER') THEN
    IndexNum=FindItemInList(CoilName,WaterCoil%Name,NumWaterCoils)
    Capacity=WaterCoil(IndexNum)%DesWaterHeatingCoilRate
  ELSE IF (CoilType == 'COIL:COOLING:WATER') THEN
    IndexNum=FindItemInList(CoilName,WaterCoil%Name,NumWaterCoils)
    Capacity=WaterCoil(IndexNum)%DesWaterCoolingCoilRate
  ELSE IF (CoilType == 'COIL:COOLING:WATER:DETAILEDGEOMETRY') THEN
    IndexNum=FindItemInList(CoilName,WaterCoil%Name,NumWaterCoils)
    Capacity=WaterCoil(IndexNum)%DesWaterCoolingCoilRate
  ELSE
    IndexNum=0
  ENDIF

  IF (IndexNum == 0) THEN
    CALL ShowSevereError('GetWaterCoilCapacity: Could not find CoilType="'//TRIM(CoilType)//  &
                         '" with Name="'//TRIM(CoilName)//'"')
    ErrorsFound=.true.
  ENDIF

  RETURN

END FUNCTION GetWaterCoilCapacity

SUBROUTINE UpdateWaterToAirCoilPlantConnection(CoilTypeNum,        &
                                               CoilName,           &
                                               EquipFlowCtrl,      &
                                               LoopNum,            &
                                               LoopSide,           &
                                               CompIndex,          &
                                               FirstHVACIteration, &
                                               InitLoopEquip)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B. Griffith
          !       DATE WRITTEN   February 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! update sim routine called from plant

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataGlobals,    ONLY: KickOffSimulation
  Use DataLoopNode,   ONLY: Node
  USE DataPlant,      ONLY: ccSimPlantEquipTypes, PlantLoop
  USE InputProcessor, ONLY: FindItemInList
  USE General,        ONLY: TrimSigDigits
  USE DataHVACGlobals,    ONLY: SimAirLoopsFlag, SimZoneEquipmentFlag
  USE DataInterfaces, ONLY: ShowContinueErrorTimeStamp
  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)    :: CoilTypeNum
  CHARACTER(len=*), INTENT(IN) :: CoilName
  INTEGER, INTENT(IN)    :: EquipFlowCtrl       ! Flow control mode for the equipment
  INTEGER, INTENT(IN)    :: LoopNum             ! Plant loop index for where called from
  INTEGER, INTENT(IN)    :: LoopSide            ! Plant loop side index for where called from
  INTEGER, INTENT(INOUT) :: CompIndex        ! Chiller number pointer
  LOGICAL , INTENT(IN)   :: FirstHVACIteration   !
  LOGICAL, INTENT(INOUT) :: InitLoopEquip       ! If not zero, calculate the max load for operating conditions


          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

  Integer :: CoilNum
  LOGICAL :: DidAnythingChange = .FALSE. ! set to true if conditions changed
  INTEGER :: InletNodeNum
  INTEGER :: OutletNodeNum

    ! Find the correct water coil
  IF (CompIndex == 0) THEN
    CoilNum = FindItemInList(CoilName,WaterCoil%Name,NumWaterCoils)
    IF (CoilNum == 0) THEN
      CALL ShowFatalError('UpdateWaterToAirCoilPlantConnection: Specified Coil not one of Valid water coils='//TRIM(CoilName))
    ENDIF
    CompIndex=CoilNum
  ELSE
    CoilNum=CompIndex
    IF (CoilNum > NumWaterCoils .or. CoilNum < 1) THEN
      CALL ShowFatalError('UpdateWaterToAirCoilPlantConnection:  Invalid CompIndex passed='//  &
                          TRIM(TrimSigDigits(CoilNum))// &
                          ', Number of Coils='//TRIM(TrimSigDigits(NumWaterCoils))//  &
                          ', Entered Coil name='//TRIM(CoilName))
    ENDIF
    IF (KickOffSimulation) THEN
      IF (CoilName /= WaterCoil(CoilNum)%Name) THEN
        CALL ShowFatalError('UpdateWaterToAirCoilPlantConnection: Invalid CompIndex passed='//  &
                            TRIM(TrimSigDigits(CoilNum))// &
                            ', Coil name='//TRIM(CoilName)//', stored Coil Name for that index='//  &
                            TRIM(WaterCoil(CoilNum)%Name))
      ENDIF
      IF (CoilTypeNum /= WaterCoil(CoilNum)%WaterCoilType_Num) THEN
        CALL ShowFatalError('UpdateWaterToAirCoilPlantConnection: Invalid CompIndex passed='//  &
                            TRIM(TrimSigDigits(CoilNum))// &
                            ', Coil name='//TRIM(CoilName)//', stored Coil Name for that index='//  &
                            TRIM(ccSimPlantEquipTypes(CoilTypeNum)) )
      ENDIF
    ENDIF
  ENDIF

  IF (InitLoopEquip) THEN
    RETURN
  END IF

  DidAnythingChange = .FALSE.

  InletNodeNum = WaterCoil(CoilNum)%WaterInletNodeNum
  OutletNodeNum = WaterCoil(CoilNum)%WaterOutletNodeNum

  IF (Node(InletNodeNum)% Temp /= WaterCoil(CoilNum)%InletWaterTemp) DidAnythingChange = .TRUE.

  IF (Node(OutletNodeNum)%Temp /= WaterCoil(CoilNum)%OutletWaterTemp) DidAnythingChange = .TRUE.

  IF (Node(InletNodeNum)%MassFlowRate /= WaterCoil(CoilNum)%OutletWaterMassFlowRate) THEN
    DidAnythingChange = .TRUE.
    Node(OutletNodeNum)%MassFlowRate = Node(InletNodeNum)%MassFlowRate ! make sure flows are consistent
  ENDIF

  IF (Node(OutletNodeNum)%MassFlowRate /= WaterCoil(CoilNum)%OutletWaterMassFlowRate) DidAnythingChange = .TRUE.

  IF (DidAnythingChange) THEN
    ! set sim flag for this loop
    PlantLoop(LoopNum)%LoopSide(LoopSide)%SimLoopSideNeeded = .TRUE.
    !set sim flags for air side users of coils

    SimAirLoopsFlag = .TRUE.
    SimZoneEquipmentFlag = .TRUE.
  ELSE ! nothing changed so turn off sim flag
    PlantLoop(LoopNum)%LoopSide(LoopSide)%SimLoopSideNeeded = .FALSE.
  ENDIF

  RETURN

END SUBROUTINE UpdateWaterToAirCoilPlantConnection

FUNCTION GetWaterCoilAvailScheduleIndex(CoilType,CoilName,ErrorsFound) RESULT(AvailSchIndex)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Chandan Sharma, FSEC
          !       DATE WRITTEN   February 2013
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function looks up the given coil and returns the availability schedule index.  If
          ! incorrect coil type or name is given, errorsfound is returned as true and index is returned
          ! as zero.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor,  ONLY: FindItem, SameString

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: CoilType     ! must match coil types in this module
  CHARACTER(len=*), INTENT(IN) :: CoilName     ! must match coil names for the coil type
  LOGICAL, INTENT(INOUT)       :: ErrorsFound  ! set to true if problem
  INTEGER                      :: AvailSchIndex   ! returned availability schedule of matched coil

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER :: WhichCoil

  ! Obtains and Allocates HeatingCoil related parameters from input file
  ! Obtains and Allocates DXCoils
  IF (GetWaterCoilsInputFlag) THEN
    CALL GetWaterCoilInput
    GetWaterCoilsInputFlag = .FALSE.
  END IF

  WhichCoil=0
  AvailSchIndex=0

  IF (SameString(CoilType,'Coil:Heating:Water') .OR. &
      SameString(CoilType,'Coil:Cooling:Water') .OR. &
      SameString(CoilType,'Coil:Cooling:Water:DetailedGeometry')) THEN
    WhichCoil=FindItem(CoilName,WaterCoil%Name,NumWaterCoils)
    IF (WhichCoil /= 0) THEN
      AvailSchIndex=WaterCoil(WhichCoil)%SchedPtr
    ENDIF
  ELSE
    WhichCoil=0
  ENDIF

  IF (WhichCoil == 0) THEN
    CALL ShowSevereError('GetCoilAvailScheduleIndex: Could not find Coil, Type="'// &
                          TRIM(CoilType)//'" Name="'//TRIM(CoilName)//'"')
    ErrorsFound=.true.
    AvailSchIndex=0
  ENDIF

  RETURN

END FUNCTION GetWaterCoilAvailScheduleIndex

! End of Coil Utility subroutines
! *****************************************************************************

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

End Module WaterCoils
