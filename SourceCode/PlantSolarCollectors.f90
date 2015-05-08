MODULE SolarCollectors

          ! MODULE INFORMATION:
          !       AUTHOR         Peter Graham Ellis
          !       DATE WRITTEN   December 2003
          !       MODIFIED       B. Nigusse, FSEC/UCF, March 2012, added ICS Collector
          !       RE-ENGINEERED  Brent Griffith, for plant upgrade, general fluid props

          ! PURPOSE OF THIS MODULE:
          ! Simulates solar collectors as a component on the plant loop.  Currently only flat-plate collectors (glazed and
          ! unglazed) are implemented.

          ! METHODOLOGY EMPLOYED:
          ! Solar collectors are called as non-zone equipment on the demand side of the plant loop.  The collector object
          ! must be connected to a WATER HEATER object on the supply side of the plant loop.  Water is assumed to be
          ! the heat transfer fluid.

          ! USE STATEMENTS:
USE DataPrecisionGlobals
USE DataGlobals, ONLY: MaxNameLength, BeginEnvrnFlag
USE DataInterfaces, ONLY: SetupOutputVariable, ShowWarningError, ShowFatalError, ShowSevereError, ShowContinueError, &
                          ShowRecurringSevereErrorAtEnd,ShowRecurringWarningErrorAtEnd,ShowSevereMessage,ShowWarningMessage
USE DataSurfaces, ONLY: Surface, TotSurfaces, SurfSunlitArea, SurfSunlitFrac, SurfaceClass_Detached_F, SurfaceClass_Detached_B,  &
                        SurfaceClass_Shading
USE PlantUtilities, ONLY: SetComponentFlowRate

IMPLICIT NONE ! Enforce explicit typing of all variables

PRIVATE ! Everything private unless explicitly made public

          ! MODULE PARAMETER DEFINITIONS:
! Fluid Type Flags
INTEGER, PARAMETER :: WATER = 1
INTEGER, PARAMETER :: AIR = 2

! Test Correlation Type Flags
INTEGER, PARAMETER :: INLET = 1
INTEGER, PARAMETER :: AVERAGE = 2
INTEGER, PARAMETER :: OUTLET = 3

! ICS Collector Type Flag
INTEGER, PARAMETER :: ICSRectangularTank = 1
!INTEGER, PARAMETER :: ICSProgressiveTube = 2

          ! DERIVED TYPE DEFINITIONS:
TYPE ParametersData
  CHARACTER(len=MaxNameLength) :: Name = ''              ! Name of solar collector parameters
  REAL(r64)                    :: Area = 0.0d0             ! Gross area of collector (m2)
  INTEGER                      :: TestFluid = WATER      ! Test fluid (only WATER for now)
  REAL(r64)                    :: TestMassFlowRate = 0.0d0 ! Test volumetric flow rate (m3/s)
  INTEGER                      :: TestType = INLET       ! Test correlation type (INLET | AVERAGE | OUTLET)
  REAL(r64)                    :: eff0                   ! Coefficient 1 of efficiency equation (Y-intercept)
  REAL(r64)                    :: eff1                   ! Coefficient 2 of efficiency equation (1st order)
  REAL(r64)                    :: eff2                   ! Coefficient 3 of efficiency equation (2nd order)
  REAL(r64)                    :: iam1                   ! Coefficient 2 of incident angle modifier (1st order)
  REAL(r64)                    :: iam2                   ! Coefficient 3 of incident angle modifier (2nd order)

  INTEGER                      :: ICSType_Num             = 0   ! ICS collector type
  REAL(r64)                    :: Volume                  = 0.0d0 ! collector water net volume (m3)
  REAL(r64)                    :: SideHeight              = 0.0d0 ! collector side height (m)
  REAL(r64)                    :: ThermalMass             = 0.0d0 ! thermal mass of the absorber plate (J/m2C)
  REAL(r64)                    :: ULossSide               = 0.0d0 ! heat loss conductance for collector side (W/m2C)
  REAL(r64)                    :: ULossBottom             = 0.0d0 ! heat loss conductance for collector bottom (W/m2C)

  REAL(r64)                    :: AspectRatio             = 0.0d0 ! collector aspect ratio (dimensionless)
  INTEGER                      :: NumOfCovers             = 0   ! number of transparent collector covers
  REAL(r64)                    :: CoverSpacing            = 0.0d0 ! collector cover spacings (m)
  REAL(r64)                    :: RefractiveIndex(2)      = 0.0d0 ! refractive idex of inner and outer covers (dimensionless)
  REAL(r64)                    :: ExtCoefTimesThickness(2)= 0.0d0 ! extinction coefficient times thickness of covers (dimensionless)
  REAL(r64)                    :: EmissOfCover(2)         = 0.0d0 ! emissivity of inner and outer covers (dimensionless)
  REAL(r64)                    :: EmissOfAbsPlate         = 0.0d0 ! emissivity Of absorber plate (dimensionless)
  REAL(r64)                    :: AbsorOfAbsPlate         = 0.0d0 ! absorptance of the absorber plate (dimensionless)

END TYPE ParametersData

TYPE , PUBLIC :: CollectorData
  CHARACTER(len=MaxNameLength) :: Name = ''                   ! Name of solar collector
  CHARACTER(len=MaxNameLength) :: BCType                = ' ' ! Boundary condition Type
  CHARACTER(len=MaxNameLength) :: BCName                = ' ' ! Boundary condition Name
  CHARACTER(len=MaxNameLength) :: OSCMName              = ' ' ! OtherSideConditionsModel
  INTEGER                      :: VentCavIndex          = 0   ! index of ventilated cavity object
  INTEGER                      :: ICSType_Num           = 0   ! ICS collector type number
  INTEGER                      :: TypeNum                     ! Plant Side Connection: 'TypeOf_Num' assigned in DataPlant !DSU
  INTEGER                      :: WLoopNum              = 0   ! Water plant loop index number                      !DSU
  INTEGER                      :: WLoopSideNum          = 0   ! Water plant loop side index                        !DSU
  INTEGER                      :: WLoopBranchNum        = 0   ! Water plant loop branch index                      !DSU
  INTEGER                      :: WLoopCompNum          = 0   ! Water plant loop component index                   !DSU
  LOGICAL                      :: Init = .TRUE.               ! Flag for initialization:  TRUE means do the init
  LOGICAL                      :: InitSizing = .TRUE.         ! Flag for initialization of plant sizing
  INTEGER                      :: Parameters = 0              ! Parameters object number
  INTEGER                      :: Surface = 0                 ! Surface object number
  INTEGER                      :: InletNode = 0               ! Inlet node
  REAL(r64)                    :: InletTemp = 0.0d0             ! Inlet temperature from plant (C)
  INTEGER                      :: OutletNode = 0              ! Outlet node
  REAL(r64)                    :: OutletTemp = 0.0d0            ! Outlet temperature or stagnation temperature in the collector (C)
  REAL(r64)                    :: MassFlowRate          = 0.0d0 ! Mass flow rate through the collector (kg/s)
  REAL(r64)                    :: MassFlowRateMax       = 0.0d0 ! Maximum mass flow rate through the collector (kg/s)
  REAL(r64)                    :: VolFlowRateMax        = 0.0d0 ! Maximum volumetric flow rate through the collector (m3/s)
  INTEGER                      :: ErrIndex              = 0   ! Error index for recurring error
  INTEGER                      :: IterErrIndex          = 0   ! Error index for recurring error (iteration - did not converge)

  ! Report variables
  REAL(r64)                    :: IncidentAngleModifier = 0.0d0 ! Net incident angle modifier
  REAL(r64)                    :: Efficiency            = 0.0d0 ! Thermal efficiency of solar energy conversion
  REAL(r64)                    :: Power                 = 0.0d0 ! Heat gain or loss to collector fluid (W)
  REAL(r64)                    :: HeatGain              = 0.0d0 ! Heat gain to collector fluid (W)
  REAL(r64)                    :: HeatLoss              = 0.0d0 ! Heat loss from collector fluid (W)
  REAL(r64)                    :: Energy                = 0.0d0 ! Energy gained (or lost) to collector fluid (J)

  ! Report variables
  REAL(r64)                    :: HeatRate              = 0.0d0   ! Collector useful Heat gain rate [W]
  REAL(r64)                    :: HeatEnergy            = 0.0d0   ! Collector useful Heat gain energy [J]
  REAL(r64)                    :: StoredHeatRate        = 0.0d0   ! net heat gain or loss rate of the collector fluid [W]
  REAL(r64)                    :: StoredHeatEnergy      = 0.0d0   ! net heat gain or loss energy of the collector fluid [J]
  REAL(r64)                    :: HeatGainRate          = 0.0d0   ! Collector useful Heat gain rate [W]
  REAL(r64)                    :: HeatGainEnergy        = 0.0d0   ! Collector useful Heat gain energy (J)
  REAL(r64)                    :: HeatLossRate          = 0.0d0   ! collector useful heat loss rate [W]
  REAL(r64)                    :: HeatLossEnergy        = 0.0d0   ! Collector useful Heat loss energy [J]
  REAL(r64)                    :: SkinHeatLossRate      = 0.0d0   ! collector skin heat loss rate [W]
  REAL(r64)                    :: CollHeatLossEnergy    = 0.0d0   ! collector skin heat loss energy[J]
  REAL(r64)                    :: TauAlpha              = 0.0d0   ! Transmittance-absorptance product total radiation
  REAL(r64)                    :: UTopLoss              = 0.0d0   ! Over all top loss coefficient [W/m2.C]
  REAL(r64)                    :: TempOfWater           = 0.0d0   ! average temperature of the collector water [C]
  REAL(r64)                    :: TempOfAbsPlate        = 0.0d0   ! average temperature of the abs plate [C]
  REAL(r64)                    :: TempOfInnerCover      = 0.0d0   ! temperature of the collector inner cover [C]
  REAL(r64)                    :: TempOfOuterCover      = 0.0d0   ! temperature of the collector inner cover [C]

  ! Data from elsewhere and calculated
  REAL(r64)                    :: TauAlphaNormal        = 0.0d0   ! Transmittance-absorptance product normal radiation
  REAL(r64)                    :: TauAlphaSkyDiffuse    = 0.0d0   ! Transmittance-absorptance product sky diffuse radiation
  REAL(r64)                    :: TauAlphaGndDiffuse    = 0.0d0   ! Transmittance-absorptance product grn diffuse radiation
  REAL(r64)                    :: TauAlphaBeam          = 0.0d0   ! Transmittance-absorptance product beam radiation
  REAL(r64)                    :: CoversAbsSkyDiffuse(2)= 0.0d0   ! sky diffuse solar absorptance of cover
  REAL(r64)                    :: CoversAbsGndDiffuse(2)= 0.0d0   ! ground diffuse solar absorptance of cover
  REAL(r64)                    :: CoverAbs(2)           = 0.0d0   ! solar rad weighted covers absorptance
  REAL(r64)                    :: TimeElapsed           = 0.0d0   ! Fraction of the current hour that has elapsed (h)
                                                                ! Saved in order to identify the beginning of a new system time
  REAL(r64)                    :: UbLoss                = 0.0d0   ! Over all bottom loss coefficient [W/m2C]
  REAL(r64)                    :: UsLoss                = 0.0d0   ! Over all side loss coefficient [W/m2C]
  REAL(r64)                    :: AreaRatio             = 0.0d0   ! Side area to collector area ratio [-]
  REAL(r64)                    :: RefSkyDiffInnerCover  = 0.0d0   ! Sky diffuse refl of inner cover (cover 1)
  REAL(r64)                    :: RefGrnDiffInnerCover  = 0.0d0   ! ground diffuse refl of inner cover (cover 1)
  REAL(r64)                    :: RefDiffInnerCover     = 0.0d0   ! diffuse reflectance of the inner cover (cover 1) from bottom
  REAL(r64)                    :: SavedTempOfWater      = 0.0d0   ! water temp carried from time step to time step [C]
  REAL(r64)                    :: SavedTempOfAbsPlate   = 0.0d0   ! abs plate temp carried from time step to time step [C]

  REAL(r64)                    :: SavedTempOfInnerCover = 0.0d0   ! inner cover temp carried from time step to time step [C]
  REAL(r64)                    :: SavedTempOfOuterCover = 0.0d0   ! outer cover temp carried from time step to time step [C]
  REAL(r64)                    :: SavedTempCollectorOSCM= 0.0d0   ! Temperature of collector back from OSCM at previous time step [C]
  REAL(r64)                    :: Length                = 1.0d0   ! characteristic length of the abs plate
  REAL(r64)                    :: TiltR2V               = 0.0d0   ! collector tilt angle from the vertical [degree]
  REAL(r64)                    :: Tilt                  = 0.0d0   ! collector tilt angle from the horizontal [degree]
  REAL(r64)                    :: CosTilt               = 0.0d0   ! cosine of colector tilt angle [-]
  REAL(r64)                    :: SinTilt               = 0.0d0   ! sine of 1.8 times colector tilt angle [-]
  REAL(r64)                    :: SideArea              = 0.0d0   ! weighted collector side area (m2)
  REAL(r64)                    :: Area                  = 0.0d0   ! collector area (m2)
  REAL(r64)                    :: Volume                = 0.0d0   ! collector net volume (m3)
  LOGICAL                      :: OSCM_ON               = .FALSE. ! Boundary condition is OSCM
  LOGICAL                      :: InitICS               = .FALSE. ! used to initialize ICS variables only

END TYPE CollectorData

          ! MODULE VARIABLE TYPE DECLARATIONS:
TYPE (ParametersData), ALLOCATABLE, DIMENSION(:) :: Parameters
TYPE (CollectorData), ALLOCATABLE, DIMENSION(:), PUBLIC :: Collector

LOGICAL, ALLOCATABLE, DIMENSION(:) :: CheckEquipName

          ! MODULE VARIABLE DECLARATIONS:
INTEGER                       :: NumOfParameters=0
INTEGER,    PUBLIC            :: NumOfCollectors=0

REAL(r64), ALLOCATABLE, DIMENSION(:) :: TransSysSkyDiff  ! transmittance of cover system for sky diffuse solar rad.
REAL(r64), ALLOCATABLE, DIMENSION(:) :: TransSysGrnDiff  ! transmittance of cover system for ground diffuse solar rad.
REAL(r64), ALLOCATABLE, DIMENSION(:) :: RefSysSkyDiff    ! reflectance of cover system for sky diffuse solar rad.
REAL(r64), ALLOCATABLE, DIMENSION(:) :: RefSysGrnDiff    ! reflectance of cover system for ground diffuse solar rad.

          ! SUBROUTINE SPECIFICATIONS:
PUBLIC SimSolarCollector
PRIVATE GetSolarCollectorInput
PRIVATE InitSolarCollector
PRIVATE CalcSolarCollector
PRIVATE IAM
PRIVATE CalcICSSolarCollector
PRIVATE ICSCollectorAnalyticalSoluton
PRIVATE CalcTransAbsorProduct
PRIVATE CalcTransRefAbsOfCover
PRIVATE CalcHeatTransCoeffAndCoverTemp
PRIVATE CalcConvCoeffBetweenPlates
PRIVATE CalcConvCoeffAbsPlateAndWater
PRIVATE UpdateSolarCollector
PRIVATE ReportSolarCollector
PRIVATE GetExtVentedCavityIndex
PRIVATE GetExtVentedCavityTsColl

CONTAINS

          ! MODULE SUBROUTINES:

SUBROUTINE SimSolarCollector( EquipTypeNum, CompName, CompIndex, InitLoopEquip, FirstHVACIteration)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Peter Graham Ellis
          !       DATE WRITTEN   December 2003
          !       MODIFIED       Brent Griffith, March 2010
          !                      Bereket Nigusse, March 2012 Added ICS collector
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Simulates solar collector objects.

          ! METHODOLOGY EMPLOYED:
          ! Standard EnergyPlus methodology.

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: FindItemInList
  USE General,        ONLY: TrimSigDigits
  USE DataPlant,      ONLY: TypeOf_SolarCollectorFlatPlate, TypeOf_SolarCollectorICS

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)          :: EquipTypeNum
  CHARACTER(len=*), INTENT(IN) :: CompName
  INTEGER, INTENT(INOUT)       :: CompIndex
  LOGICAL, INTENT(IN)          :: InitLoopEquip
  LOGICAL, INTENT(IN)          :: FirstHVACIteration

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  LOGICAL,SAVE :: GetInputFlag = .TRUE.
  INTEGER      :: CollectorNum

          ! FLOW:
  IF (GetInputFlag) THEN
    CALL GetSolarCollectorInput
    GetInputFlag = .FALSE.
  END IF

  IF (CompIndex == 0) THEN
    CollectorNum = FindItemInList(CompName, Collector%Name, NumOfCollectors)
    IF (CollectorNum == 0) THEN
      CALL ShowFatalError('SimSolarCollector: Specified solar collector not Valid ='//TRIM(CompName))
    ENDIF
    CompIndex = CollectorNum
  ELSE
    CollectorNum = CompIndex
    IF (CollectorNum > NumOfCollectors .OR. CollectorNum < 1) THEN
      CALL ShowFatalError('SimSolarCollector: Invalid CompIndex passed='//  &
                          TRIM(TrimSigDigits(CollectorNum))// &
                          ', Number of Units='//Trim(TrimSigDigits(NumOfCollectors))//  &
                          ', Entered Unit name='//Trim(CompName))
    ENDIF
    IF (CheckEquipName(CollectorNum)) THEN
      IF (CompName /= Collector(CollectorNum)%Name ) THEN
        CALL ShowFatalError('SimSolarCollector: Invalid CompIndex passed='//  &
                          TRIM(TrimSigDigits(CollectorNum))// &
                          ', Unit name='//Trim(CompName )// ', stored Unit Name for that index='// &
                          TRIM(Collector(CollectorNum)%Name) )

      ENDIF
      CheckEquipName(CollectorNum) = .FALSE.
    ENDIF

  ENDIF

  CALL InitSolarCollector(CollectorNum)

  SELECT CASE (Collector(CollectorNum)%TypeNum)
  ! Select and CALL models based on collector type
  CASE (TypeOf_SolarCollectorFlatPlate)

    CALL CalcSolarCollector(CollectorNum)

  CASE (TypeOf_SolarCollectorICS)

    CALL CalcICSSolarCollector(CollectorNum)

  CASE Default

  END SELECT

  CALL UpdateSolarCollector(CollectorNum)

  CALL ReportSolarCollector(CollectorNum)

  RETURN

END SUBROUTINE SimSolarCollector


SUBROUTINE GetSolarCollectorInput

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Peter Graham Ellis
          !       DATE WRITTEN   December 2003
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Gets the solar collector input from the input file and sets up the parameters and collector objects.

          ! METHODOLOGY EMPLOYED:
          ! Standard EnergyPlus methodology.

          ! USE STATEMENTS:
  USE DataGlobals, ONLY: DegToRadians, InitConvTemp
  USE DataHeatBalance
  USE InputProcessor, ONLY: GetNumObjectsFound, FindItemInList, GetObjectItem, VerifyName, GetObjectDefMaxArgs, &
                            SameString
  USE DataIPShortCuts  ! Data for field names, blank numerics
  USE NodeInputManager, ONLY: GetOnlySingleNode
  USE BranchNodeConnections, ONLY: TestCompSet
  USE Psychrometrics, ONLY: RhoH2O
  USE DataLoopNode
  USE DataPlant   !DSU
  USE General, ONLY: RoundSigDigits
  USE DataSurfaces,     ONLY: Surface, OSCM, TotOSCM, TotSurfaces, OtherSideCondModeledExt

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  LOGICAL                        :: ErrorsFound = .FALSE. ! Set to true if errors in input, fatal at end of routine
  INTEGER                        :: IOStatus              ! Used in GetObjectItem
  LOGICAL                        :: IsBlank               ! TRUE if the name is blank
  LOGICAL                        :: IsNotOk               ! TRUE if there was a problem with a list name
  INTEGER                        :: NumAlphas             ! Number of Alphas for each GetObjectItem call
  INTEGER                        :: NumNumbers            ! Number of Numbers for each GetObjectItem call
  INTEGER                        :: CollectorNum          ! Solar collector object number
  INTEGER                        :: CollectorNum2         ! Second solar collector object number for looping
  INTEGER                        :: ParametersNum         ! Solar collector parameters object number
  INTEGER                        :: SurfNum               ! Collector surface object number
  CHARACTER(len=MaxNameLength)   :: CurrentModuleObject   ! for ease in renaming.
  CHARACTER(len=MaxNameLength)   :: CurrentModuleParamObject   ! for ease in renaming.

  INTEGER :: NumFields                  ! Total number of fields in object
  INTEGER :: MaxAlphas                  ! Maximum number of alpha fields in all objects
  INTEGER :: MaxNumbers                 ! Maximum number of numeric fields in all objects
  INTEGER :: NumOfICSParam=0            ! number of parameter objects for ICS colectors
  INTEGER :: NumOfICSUnits=0            ! number of ICS colector units
  INTEGER :: NumOfFlatPlateParam=0      ! number of parameter objects for flat plate colectors
  INTEGER :: NumFlatPlateUnits=0        ! number of plat plate solar colector units

  INTEGER :: FlatPlateParamNum          ! plat plate solar colector parameters counter
  INTEGER :: ICSParamNum                ! ICS collector parameters counter

  INTEGER :: FlatPlateUnitsNum          ! plat plate solar colector parameters counter
  INTEGER :: ICSUnitsNum                ! ICS collector parameters counter
  INTEGER :: Found                      ! index
  INTEGER :: VentCavIndex               ! vent cavity index
  REAL(r64)         :: Perimeter        ! perimeter of the absorber or collector

  REAL(r64), ALLOCATABLE, DIMENSION(:)                    :: Numbers        ! Numeric data
  CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: Alphas         ! Alpha data
  CHARACTER(Len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: cAlphaFields   ! Alpha field names
  CHARACTER(Len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: cNumericFields ! Numeric field names
  LOGICAL, ALLOCATABLE, DIMENSION(:)   :: lAlphaBlanks      ! Logical array, alpha field input BLANK = .TRUE.
  LOGICAL, ALLOCATABLE, DIMENSION(:)   :: lNumericBlanks    ! Logical array, numeric field input BLANK = .TRUE.

          ! FLOW:
  MaxNumbers=0
  MaxAlphas=0

  CurrentModuleParamObject = 'SolarCollectorPerformance:FlatPlate'
  NumOfFlatPlateParam = GetNumObjectsFound(CurrentModuleParamObject)
  CALL GetObjectDefMaxArgs(TRIM(CurrentModuleParamObject),NumFields,NumAlphas,NumNumbers)
  MaxNumbers=MAX(MaxNumbers,NumNumbers)
  MaxAlphas=MAX(MaxAlphas,NumAlphas)

  CurrentModuleObject = 'SolarCollector:FlatPlate:Water'
  NumFlatPlateUnits = GetNumObjectsFound(CurrentModuleObject)
  CALL GetObjectDefMaxArgs(CurrentModuleObject,NumFields,NumAlphas,NumNumbers)
  MaxNumbers=MAX(MaxNumbers,NumNumbers)
  MaxAlphas=MAX(MaxAlphas,NumAlphas)

  CurrentModuleParamObject = 'SolarCollectorPerformance:IntegralCollectorStorage'
  NumOfICSParam = GetNumObjectsFound(CurrentModuleParamObject)
  CALL GetObjectDefMaxArgs(TRIM(CurrentModuleParamObject),NumFields,NumAlphas,NumNumbers)
  MaxNumbers=MAX(MaxNumbers,NumNumbers)
  MaxAlphas=MAX(MaxAlphas,NumAlphas)

  CurrentModuleObject = 'SolarCollector:IntegralCollectorStorage'
  NumOfICSUnits = GetNumObjectsFound(CurrentModuleObject)
  CALL GetObjectDefMaxArgs(CurrentModuleObject,NumFields,NumAlphas,NumNumbers)
  MaxNumbers=MAX(MaxNumbers,NumNumbers)
  MaxAlphas=MAX(MaxAlphas,NumAlphas)

  ALLOCATE(Alphas(MaxAlphas))
  Alphas=' '
  ALLOCATE(Numbers(MaxNumbers))
  Numbers=0.0d0
  ALLOCATE(cAlphaFields(MaxAlphas))
  cAlphaFields=' '
  ALLOCATE(cNumericFields(MaxNumbers))
  cNumericFields=' '
  ALLOCATE(lAlphaBlanks(MaxAlphas))
  lAlphaBlanks=.TRUE.
  ALLOCATE(lNumericBlanks(MaxNumbers))
  lNumericBlanks=.TRUE.

  NumOfCollectors = NumFlatPlateUnits + NumOfICSUnits
  NumOfParameters = NumOfFlatPlateParam + NumOfICSParam

  IF (NumOfParameters > 0) THEN
    ALLOCATE(Parameters(NumOfParameters))

    CurrentModuleParamObject = 'SolarCollectorPerformance:FlatPlate'

    DO FlatPlateParamNum = 1, NumOfFlatPlateParam

      ParametersNum = FlatPlateParamNum
      CALL GetObjectItem(CurrentModuleParamObject, &
        ParametersNum,cAlphaArgs,NumAlphas,rNumericArgs,NumNumbers,IOStatus,     &
        NumBlank=lNumericFieldBlanks,AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

      ! Collector module parameters name
      IsNotOK = .FALSE.
      IsBlank = .FALSE.
      CALL VerifyName(cAlphaArgs(1),Parameters%Name,ParametersNum-1,IsNotOK,IsBlank,TRIM(CurrentModuleParamObject))
      IF (IsNotOK) THEN
        ErrorsFound = .TRUE.
        IF (IsBlank) cAlphaArgs(1) = 'xxxxx'
      END IF
      Parameters(ParametersNum)%Name = cAlphaArgs(1)

      ! NOTE:  This values serves mainly as a reference.  The area of the associated surface object is used in all calculations.
      Parameters(ParametersNum)%Area = rNumericArgs(1)

      SELECT CASE(TRIM(cAlphaArgs(2)))
        CASE('WATER')
          Parameters(ParametersNum)%TestFluid = WATER
        !CASE('AIR')
        !  Parameters(ParametersNum)%TestFluid = AIR
        CASE DEFAULT
          CALL ShowSevereError(TRIM(CurrentModuleParamObject)//' = '//TRIM(cAlphaArgs(1))// &
            ':  '//TRIM(cAlphaArgs(2))//' is an unsupported Test Fluid for '//TRIM(cAlphaFieldNames(2)))
          ErrorsFound = .TRUE.
      END SELECT

      IF (rNumericArgs(2) > 0.0d0) THEN
        Parameters(ParametersNum)%TestMassFlowRate = rNumericArgs(2) * RhoH2O(InitConvTemp)
      ELSE
        CALL ShowSevereError(TRIM(CurrentModuleParamObject)//' = '//TRIM(cAlphaArgs(1))// &
          ':  flow rate must be greater than zero for ' //TRIM(cNumericFieldNames(2)) )
        ErrorsFound = .TRUE.
      END IF

      SELECT CASE(TRIM(cAlphaArgs(3)))
        CASE('INLET')
          Parameters(ParametersNum)%TestType = INLET
        CASE('AVERAGE')
          Parameters(ParametersNum)%TestType = AVERAGE
        CASE('OUTLET')
          Parameters(ParametersNum)%TestType = OUTLET
        CASE DEFAULT
          CALL ShowSevereError(TRIM(CurrentModuleParamObject)//' = '//TRIM(cAlphaArgs(1))// &
            ':  '//TRIM(cAlphaArgs(3))//' is  not supported for '//TRIM(cAlphaFieldNames(3)))
          ErrorsFound = .TRUE.
      END SELECT

      ! Efficiency equation coefficients
      Parameters(ParametersNum)%eff0 = rNumericArgs(3)
      Parameters(ParametersNum)%eff1 = rNumericArgs(4)

      IF (NumNumbers > 4) THEN
        Parameters(ParametersNum)%eff2 = rNumericArgs(5)
      ELSE
        Parameters(ParametersNum)%eff2 = 0.0d0
      END IF

      ! Incident angle modifier coefficients
      IF (NumNumbers > 5) THEN
        Parameters(ParametersNum)%iam1 = rNumericArgs(6)
      ELSE
        Parameters(ParametersNum)%iam1 = 0.0d0
      END IF

      IF (NumNumbers > 6) THEN
        Parameters(FlatPlateParamNum)%iam2 = rNumericArgs(7)
      ELSE
        Parameters(ParametersNum)%iam2 = 0.0d0
      END IF
    END DO ! ParametersNum

    IF (ErrorsFound) CALL ShowFatalError('Errors in '//TRIM(CurrentModuleParamObject)//' input.')
  END IF

  IF (NumOfCollectors > 0) THEN
    ALLOCATE(Collector(NumOfCollectors))

    CurrentModuleObject = 'SolarCollector:FlatPlate:Water'

    DO FlatPlateUnitsNum = 1, NumFlatPlateUnits

      CollectorNum = FlatPlateUnitsNum

      CALL GetObjectItem(CurrentModuleObject,CollectorNum,cAlphaArgs,NumAlphas,rNumericArgs,NumNumbers,IOStatus)

      ! Collector name
      IsNotOK = .FALSE.
      IsBlank = .FALSE.
      CALL VerifyName(cAlphaArgs(1),Collector%Name,CollectorNum-1,IsNotOK,IsBlank,TRIM(CurrentModuleObject))
      IF (IsNotOK) THEN
        ErrorsFound = .TRUE.
        IF (IsBlank) cAlphaArgs(1) = 'xxxxx'
      END IF
      Collector(CollectorNum)%Name = cAlphaArgs(1)
      Collector(CollectorNum)%TypeNum = TypeOf_SolarCollectorFlatPlate ! parameter assigned in DataPlant !DSU

      ! Get parameters object
      ParametersNum = FindItemInList(cAlphaArgs(2),Parameters%Name,NumOfParameters)

      IF (ParametersNum == 0) THEN
        CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(cAlphaArgs(1))// &
          ': '//Trim(CurrentModuleParamObject)//' object called '//TRIM(cAlphaArgs(2))//' not found.')
        ErrorsFound = .TRUE.
      ELSE
        Collector(CollectorNum)%Parameters = ParametersNum
      END IF

      ! Get surface object
      SurfNum = FindItemInList(cAlphaArgs(3),Surface%Name,TotSurfaces)

      IF (SurfNum == 0) THEN
        CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(cAlphaArgs(1))// &
          ':  Surface '//TRIM(cAlphaArgs(3))//' not found.')
        ErrorsFound = .TRUE.
        CYCLE ! avoid hard crash
      ELSE

        IF (.NOT. Surface(SurfNum)%ExtSolar) THEN
          CALL ShowWarningError(TRIM(CurrentModuleObject)//' = '//TRIM(cAlphaArgs(1))// &
            ':  Surface '//TRIM(cAlphaArgs(3))//' is not exposed to exterior radiation.')
        END IF

        ! check surface orientation, warn if upside down
        IF (( Surface(SurfNum)%Tilt < -95.0D0 ) .OR. (Surface(SurfNum)%Tilt > 95.0D0)) THEN
          CALL ShowWarningError('Suspected input problem with '//TRIM(cAlphaFieldNames(3))//' = '//TRIM(cAlphaArgs(3)) )
          CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1)) )
          CALL ShowContinueError( 'Surface used for solar collector faces down')
          CALL ShowContinueError('Surface tilt angle (degrees from ground outward normal) = ' &
                                   //TRIM(RoundSigDigits(Surface(SurfNum)%Tilt,2) ) )
        ENDIF

        ! Check to make sure other solar collectors are not using the same surface
        ! NOTE:  Must search over all solar collector types
        DO CollectorNum2 = 1, NumFlatPlateUnits
          IF (Collector(CollectorNum2)%Surface == SurfNum) THEN
            CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(cAlphaArgs(1))// &
              ':  Surface '//TRIM(cAlphaArgs(3))//' is referenced by more than one '//TRIM(CurrentModuleObject))
            ErrorsFound = .TRUE.
            EXIT
          END IF
        END DO ! CollectorNum2

        Collector(CollectorNum)%Surface = SurfNum
      END IF

      ! Give warning if surface area and gross area do not match within tolerance
      IF (SurfNum > 0 .AND. ParametersNum > 0 .AND. Parameters(ParametersNum)%Area > 0.0d0 &
        .AND. ABS(Parameters(ParametersNum)%Area - Surface(SurfNum)%Area)/Surface(SurfNum)%Area > 0.01d0) THEN

        CALL ShowWarningError(TRIM(CurrentModuleObject)//' = '//TRIM(cAlphaArgs(1))// &
          ':  Gross Area of solar collector parameters and surface object differ by more than 1%.')
        CALL ShowContinueError('Area of surface object will be used in all calculations.')
      END IF

      Collector(CollectorNum)%InletNode = GetOnlySingleNode(cAlphaArgs(4),ErrorsFound,TRIM(CurrentModuleObject),cAlphaArgs(1), &
               NodeType_Water,NodeConnectionType_Inlet, 1, ObjectIsNotParent)
      Collector(CollectorNum)%OutletNode = GetOnlySingleNode(cAlphaArgs(5),ErrorsFound,TRIM(CurrentModuleObject),cAlphaArgs(1), &
               NodeType_Water,NodeConnectionType_Outlet, 1, ObjectIsNotParent)

      IF (NumNumbers > 0) THEN
        Collector(CollectorNum)%VolFlowRateMax = rNumericArgs(1)  ! Max volumetric flow rate used for plant sizing calculation
      ELSE
        Collector(CollectorNum)%VolFlowRateMax = 0.0d0  ! Max vol flow rate is not specified; no flow for plant sizing calculation
        Collector(CollectorNum)%MassFlowRateMax = 999999.9d0  ! But...set a very high value so that it demands as much as possible
      END IF

      ! Setup report variables
      CALL SetupOutputVariable('Solar Collector Incident Angle Modifier []', Collector(CollectorNum)%IncidentAngleModifier, &
                               'System','Average',Collector(CollectorNum)%Name)

      CALL SetupOutputVariable('Solar Collector Efficiency []', Collector(CollectorNum)%Efficiency, &
                               'System','Average',Collector(CollectorNum)%Name)

      CALL SetupOutputVariable('Solar Collector Heat Transfer Rate [W]', Collector(CollectorNum)%Power, &
                               'System','Average',Collector(CollectorNum)%Name)

      CALL SetupOutputVariable('Solar Collector Heat Gain Rate [W]', Collector(CollectorNum)%HeatGain, &
                               'System','Average',Collector(CollectorNum)%Name)

      CALL SetupOutputVariable('Solar Collector Heat Loss Rate [W]', Collector(CollectorNum)%HeatLoss, &
                               'System','Average',Collector(FlatPlateUnitsNum)%Name)

      CALL SetupOutputVariable('Solar Collector Heat Transfer Energy [J]', Collector(CollectorNum)%Energy, &
                               'System','Sum',Collector(FlatPlateUnitsNum)%Name, &
                               ResourceTypeKey='SolarWater',EndUseKey='HeatProduced',GroupKey='Plant')

      CALL TestCompSet(TRIM(CurrentModuleObject),cAlphaArgs(1),cAlphaArgs(4),cAlphaArgs(5),'Water Nodes')

    END DO ! FlatPlateUnitsNum

    ! Get data for ICS collector
    CurrentModuleParamObject = 'SolarCollectorPerformance:IntegralCollectorStorage'

    DO ICSParamNum = 1, NumOfICSParam

      ParametersNum = ICSParamNum + NumOfFlatPlateParam

      CALL GetObjectItem(CurrentModuleParamObject, &
        ICSParamNum,cAlphaArgs,NumAlphas,rNumericArgs,NumNumbers,IOStatus,     &
        NumBlank=lNumericFieldBlanks,AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

      ! Collector module parameters name
      IsNotOK = .FALSE.
      IsBlank = .FALSE.
      CALL VerifyName(cAlphaArgs(1),Parameters%Name,ParametersNum-1,IsNotOK,IsBlank,TRIM(CurrentModuleParamObject))
      IF (IsNotOK) THEN
        ErrorsFound = .TRUE.
        IF (IsBlank) cAlphaArgs(1) = 'xxxxx'
      END IF
      Parameters(ParametersNum)%Name = cAlphaArgs(1)
      ! NOTE:  currently the only available choice is RectangularTank.  In the future progressive tube type will be
      !        added
      IF (SameString(cAlphaArgs(2), 'RectangularTank')) THEN
         Parameters(ParametersNum)%ICSType_Num = ICSRectangularTank
      ELSE
         CALL ShowSevereError(TRIM(cAlphaFieldNames(2))//' not found='//TRIM(cAlphaArgs(2))// &
              ' in '//TRIM(CurrentModuleParamObject)//' ='//TRIM(Parameters(ParametersNum)%Name))
         ErrorsFound=.true.
      ENDIF
      ! NOTE:  This collector gross area is used in all the calculations.
      Parameters(ParametersNum)%Area = rNumericArgs(1)
      IF ( rNumericArgs(1) <= 0.0d0 ) THEN
          CALL ShowSevereError(TRIM(CurrentModuleParamObject)//' = '//TRIM(cAlphaArgs(1)))
          CALL ShowContinueError('Illegal '//TRIM(cNumericFieldNames(1))//' = '//TRIM(RoundSigDigits(rNumericArgs(1),2)))
          CALL ShowContinueError(' Collector gross area must be always gretaer than zero.')
          ErrorsFound=.TRUE.
      ENDIF
      Parameters(ParametersNum)%Volume = rNumericArgs(2)
      IF ( rNumericArgs(2) <= 0.0d0 ) THEN
          CALL ShowSevereError(TRIM(CurrentModuleParamObject)//' = '//TRIM(cAlphaArgs(1)))
          CALL ShowContinueError('Illegal '//TRIM(cNumericFieldNames(2))//' = '//TRIM(RoundSigDigits(rNumericArgs(2),2)))
          CALL ShowContinueError(' Collector water volume must be always gretaer than zero.')
          ErrorsFound=.TRUE.
      ENDIF
      !
      ! Note: this value is used to calculate the heat loss through the bottom and side of the collector
      !Parameters(ParametersNum)%ULoss = rNumericArgs(3)
      Parameters(ParametersNum)%ULossBottom = rNumericArgs(3)
      Parameters(ParametersNum)%ULossSide = rNumericArgs(4)
      Parameters(ParametersNum)%AspectRatio = rNumericArgs(5)
      Parameters(ParametersNum)%SideHeight = rNumericArgs(6)
      Parameters(ParametersNum)%ThermalMass = rNumericArgs(7)
      Parameters(ParametersNum)%NumOfCovers = rNumericArgs(8)
      Parameters(ParametersNum)%CoverSpacing = rNumericArgs(9)

      IF (Parameters(ParametersNum)%NumOfCovers == 2)THEN
          ! Outer cover refractive index
          Parameters(ParametersNum)%RefractiveIndex(1) = rNumericArgs(10)
          ! Outer cover extinction coefficient times thickness of the cover
          Parameters(ParametersNum)%ExtCoefTimesThickness(1) = rNumericArgs(11)
          ! Outer cover Emissivity
          Parameters(ParametersNum)%EmissOfCover(1) = rNumericArgs(12)

         IF (.NOT. lNumericFieldBlanks(13) .OR. .NOT. lNumericFieldBlanks(14) .OR. .NOT. lNumericFieldBlanks(15))THEN
            Parameters(ParametersNum)%RefractiveIndex(2) = rNumericArgs(13)
            Parameters(ParametersNum)%ExtCoefTimesThickness(2) = rNumericArgs(14)
            Parameters(ParametersNum)%EmissOfCover(2) = rNumericArgs(15)
         ELSE
          CALL ShowSevereError(TRIM(CurrentModuleParamObject)//' = '//TRIM(cAlphaArgs(1)))
          CALL ShowContinueError('Illegal input for one of the three inputs of the inner cover optical properties')
          ErrorsFound=.TRUE.
         ENDIF
      ELSEIF (Parameters(ParametersNum)%NumOfCovers == 1)THEN
          ! Outer cover refractive index
          Parameters(ParametersNum)%RefractiveIndex(1) = rNumericArgs(10)
          ! Outer cover extinction coefficient times thickness of the cover
          Parameters(ParametersNum)%ExtCoefTimesThickness(1) = rNumericArgs(11)
          ! Outer cover emissivity
          Parameters(ParametersNum)%EmissOfCover(1) = rNumericArgs(12)
      ELSE
          CALL ShowSevereError(TRIM(CurrentModuleParamObject)//' = '//TRIM(cAlphaArgs(1)))
          CALL ShowContinueError('Illegal '//TRIM(cNumericFieldNames(8))//' = '//TRIM(RoundSigDigits(rNumericArgs(8),2)))
          ErrorsFound=.TRUE.
      ENDIF
      !
      ! Solar absorptance of the absorber plate
      Parameters(ParametersNum)%AbsorOfAbsPlate = rNumericArgs(16)
      !
      ! thermal emmissivity of the absorber plate
      Parameters(ParametersNum)%EmissOfAbsPlate = rNumericArgs(17)

    END DO ! end of ParametersNum

    IF (ErrorsFound) CALL ShowFatalError('Errors in '//TRIM(CurrentModuleParamObject)//' input.')

    CurrentModuleObject = 'SolarCollector:IntegralCollectorStorage'

    DO ICSUnitsNum = 1, NumOfICSUnits

      CollectorNum = ICSUnitsNum + NumFlatPlateUnits

      CALL GetObjectItem(CurrentModuleObject, &
                         ICSUnitsNum,cAlphaArgs,NumAlphas,rNumericArgs,NumNumbers,IOStatus,  &
                         NumBlank=lNumericFieldBlanks,AlphaBlank=lAlphaBlanks, &
                         AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

      ! Collector name
      IsNotOK = .FALSE.
      IsBlank = .FALSE.
      CALL VerifyName(cAlphaArgs(1),Collector%Name,CollectorNum-1,IsNotOK,IsBlank,TRIM(CurrentModuleObject))
      IF (IsNotOK) THEN
        ErrorsFound = .TRUE.
        IF (IsBlank) cAlphaArgs(1) = 'xxxxx'
      END IF
      Collector(CollectorNum)%Name = cAlphaArgs(1)
      Collector(CollectorNum)%TypeNum = TypeOf_SolarCollectorICS ! parameter assigned in DataPlant

      Collector(CollectorNum)%InitICS = .TRUE.

      ! Get parameters object
      ParametersNum = FindItemInList(cAlphaArgs(2),Parameters%Name,NumOfParameters)

      IF (ParametersNum == 0) THEN
        CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(cAlphaArgs(1))// &
          ': '//Trim(CurrentModuleParamObject)//' object called '//TRIM(cAlphaArgs(2))//' not found.')
        ErrorsFound = .TRUE.
      ELSE
        Collector(CollectorNum)%Parameters = ParametersNum
      END IF

      IF (ParametersNum > 0) THEN
         ! Calculate constant collector parameters only once
          Perimeter = 2.0d0 * SQRT(Parameters(ParametersNum)%Area) &
                    * (SQRT(Parameters(ParametersNum)%AspectRatio) + 1.d0/SQRT(Parameters(ParametersNum)%AspectRatio))
          Collector(CollectorNum)%Length = SQRT(Parameters(ParametersNum)%Area/Parameters(ParametersNum)%AspectRatio)

         ! calculate the collector side heat transfer area and loss coefficient
          Collector(CollectorNum)%ICSType_Num = Parameters(ParametersNum)%ICSType_Num
          Collector(CollectorNum)%Area = Parameters(ParametersNum)%Area
          Collector(CollectorNum)%Volume = Parameters(ParametersNum)%Volume
          Collector(CollectorNum)%SideArea = Perimeter * Parameters(ParametersNum)%SideHeight
          Collector(CollectorNum)%AreaRatio = Collector(CollectorNum)%SideArea / Collector(CollectorNum)%Area
      ENDIF
      ! Get surface object
      SurfNum = FindItemInList(cAlphaArgs(3),Surface%Name,TotSurfaces)

      IF (SurfNum == 0) THEN
        CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(cAlphaArgs(1))// &
          ':  Surface '//TRIM(cAlphaArgs(3))//' not found.')
        ErrorsFound = .TRUE.
        CYCLE ! avoid hard crash
      ELSE

        IF (.NOT. Surface(SurfNum)%ExtSolar) THEN
          CALL ShowWarningError(TRIM(CurrentModuleObject)//' = '//TRIM(cAlphaArgs(1))// &
            ':  Surface '//TRIM(cAlphaArgs(3))//' is not exposed to exterior radiation.')
        END IF

        ! check surface orientation, warn if upside down
        IF (( Surface(SurfNum)%Tilt < -95.0D0 ) .OR. (Surface(SurfNum)%Tilt > 95.0D0)) THEN
          CALL ShowWarningError('Suspected input problem with '//TRIM(cAlphaFieldNames(3))//' = '//TRIM(cAlphaArgs(3)) )
          CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1)) )
          CALL ShowContinueError( 'Surface used for solar collector faces down')
          CALL ShowContinueError('Surface tilt angle (degrees from ground outward normal) = ' &
                                   //TRIM(RoundSigDigits(Surface(SurfNum)%Tilt,2) ) )
        ENDIF

        ! Check to make sure other solar collectors are not using the same surface
        ! NOTE:  Must search over all solar collector types
        DO CollectorNum2 = 1, NumOfCollectors
          IF (Collector(CollectorNum2)%Surface == SurfNum) THEN
            CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(cAlphaArgs(1))// &
              ':  Surface '//TRIM(cAlphaArgs(3))//' is referenced by more than one '//TRIM(CurrentModuleObject))
            ErrorsFound = .TRUE.
            EXIT
          END IF
        END DO ! ICSNum2

        Collector(CollectorNum)%Surface = SurfNum
      END IF

      ! Give warning if surface area and gross area do not match within tolerance
      IF (SurfNum > 0 .AND. ParametersNum > 0 .AND. Parameters(ParametersNum)%Area > 0.0d0 &
        .AND. ABS(Parameters(ParametersNum)%Area - Surface(SurfNum)%Area)/Surface(SurfNum)%Area > 0.01d0) THEN

        CALL ShowWarningError(TRIM(CurrentModuleObject)//' = '//TRIM(cAlphaArgs(1))//': ')
        CALL ShowContinueError('Gross area of solar collector parameters and surface object differ by more than 1%.')
        CALL ShowContinueError('Gross collector area is always used in the calculation.  Modify the surface ')
        CALL ShowContinueError('coordinates to match its area with collector gross area. Otherwise, the underlying ')
        CALL ShowContinueError('surface is assumed to be fully shaded when it is not.')
      END IF

      Collector(CollectorNum)%BCType = cAlphaArgs(4)
      IF (SameString(cAlphaArgs(4), 'AmbientAir')) THEN
         Collector(CollectorNum)%OSCMName = ' '
      ELSEIF (SameString(cAlphaArgs(4), 'OtherSideConditionsModel')) THEN
         Collector(CollectorNum)%OSCMName = cAlphaArgs(5)
         Collector(CollectorNum)%OSCM_ON = .TRUE.
         Found = FindItemInList(Collector(CollectorNum)%OSCMName,OSCM%Name,TotOSCM)
         IF (Found == 0) THEN
             CALL ShowSevereError(TRIM(cAlphaFieldNames(5))//' not found='//TRIM(Collector(CollectorNum)%OSCMName)// &
                  ' in '//TRIM(CurrentModuleObject)//' ='//TRIM(Collector(CollectorNum)%Name))
             ErrorsFound=.true.
         ENDIF
         !Collector(CollectorNum)%OSCMPtr = Found
         !Surface(SurfNum)%IsICS = .true.
      ELSE
         CALL ShowSevereError(TRIM(cAlphaFieldNames(5))//' not found='//TRIM(Collector(CollectorNum)%BCType)// &
              ' in '//TRIM(CurrentModuleObject)//' ='//TRIM(Collector(CollectorNum)%Name))
         ErrorsFound=.true.
      ENDIF

      IF ( Collector(CollectorNum)%OSCM_ON ) THEN
         ! get index of ventilated cavity object
         CALL GetExtVentedCavityIndex(SurfNum, VentCavIndex)
         Collector(CollectorNum)%VentCavIndex = VentCavIndex
      ENDIF

      Collector(CollectorNum)%InletNode = GetOnlySingleNode(cAlphaArgs(6),ErrorsFound,TRIM(CurrentModuleObject),cAlphaArgs(1), &
               NodeType_Water,NodeConnectionType_Inlet, 1, ObjectIsNotParent)
      Collector(CollectorNum)%OutletNode = GetOnlySingleNode(cAlphaArgs(7),ErrorsFound,TRIM(CurrentModuleObject),cAlphaArgs(1), &
               NodeType_Water,NodeConnectionType_Outlet, 1, ObjectIsNotParent)

      IF (NumNumbers > 0) THEN
        Collector(CollectorNum)%VolFlowRateMax = rNumericArgs(1)  ! Max volumetric flow rate used for plant sizing calculation
      ELSE
        Collector(CollectorNum)%VolFlowRateMax = 0.0d0  ! Max vol flow rate is not specified; no flow for plant sizing calculation
        Collector(CollectorNum)%MassFlowRateMax = 999999.9d0  ! But...set a very high value so that it demands as much as possible
      END IF

      ! Setup report variables
      CALL SetupOutputVariable('Solar Collector Transmittance Absorptance Product []', &
                                Collector(CollectorNum)%TauAlpha, 'System','Average', &
                                Collector(CollectorNum)%Name)
      CALL SetupOutputVariable('Solar Collector Overall Top Heat Loss Coefficient [W/m2-C]', &
                                Collector(CollectorNum)%UTopLoss,'System','Average', &
                                Collector(CollectorNum)%Name)
      CALL SetupOutputVariable('Solar Collector Absorber Plate Temperature [C]', &
                                Collector(CollectorNum)%TempOfAbsPlate, 'System','Average', &
                                Collector(CollectorNum)%Name)
      CALL SetupOutputVariable('Solar Collector Storage Water Temperature [C]', &
                                Collector(CollectorNum)%TempOfWater, 'System','Average', &
                                Collector(CollectorNum)%Name)
      CALL SetupOutputVariable('Solar Collector Thermal Efficiency []', &
                                Collector(CollectorNum)%Efficiency, 'System','Average', &
                                Collector(CollectorNum)%Name)
      CALL SetupOutputVariable('Solar Collector Storage Heat Transfer Rate [W]', &
                                Collector(CollectorNum)%StoredHeatRate, 'System','Average', &
                                Collector(CollectorNum)%Name)
      CALL SetupOutputVariable('Solar Collector Storage Heat Transfer Energy [J]', &
                                Collector(CollectorNum)%StoredHeatEnergy, 'System','Sum', &
                                Collector(CollectorNum)%Name,ResourceTypeKey='SolarWater', &
                                EndUseKey='HeatProduced',GroupKey='Plant')
      CALL SetupOutputVariable('Solar Collector Skin Heat Transfer Rate [W]', &
                                Collector(CollectorNum)%SkinHeatLossRate, &
                               'System','Average',Collector(CollectorNum)%Name)
      CALL SetupOutputVariable('Solar Collector Skin Heat Transfer Energy [J]', &
                                Collector(CollectorNum)%CollHeatLossEnergy,'System','Sum', &
                                Collector(CollectorNum)%Name,ResourceTypeKey='SolarWater', &
                                EndUseKey='HeatProduced',GroupKey='Plant')
      CALL SetupOutputVariable('Solar Collector Heat Transfer Rate [W]', &
                                Collector(CollectorNum)%HeatRate, 'System','Average', &
                                Collector(CollectorNum)%Name)
      CALL SetupOutputVariable('Solar Collector Heat Transfer Energy [J]', &
                                Collector(CollectorNum)%HeatEnergy,'System','Sum', &
                                Collector(CollectorNum)%Name, ResourceTypeKey='SolarWater', &
                                EndUseKey='HeatProduced',GroupKey='Plant')

     CALL TestCompSet(TRIM(CurrentModuleObject),cAlphaArgs(1),cAlphaArgs(6),cAlphaArgs(7),'Water Nodes')

    END DO ! ICSNum

    IF (ErrorsFound) CALL ShowFatalError('Errors in '//TRIM(CurrentModuleObject)//' input.')

    IF (NumOfCollectors > 0) THEN
      ALLOCATE(CheckEquipName(NumOfCollectors))
      CheckEquipName = .TRUE.
    ENDIF

  END IF

  RETURN

END SUBROUTINE GetSolarCollectorInput


SUBROUTINE InitSolarCollector(CollectorNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Peter Graham Ellis
          !       DATE WRITTEN   January 2004
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Initializes the solar collector object during the plant simulation.

          ! METHODOLOGY EMPLOYED:
          ! Inlet and outlet nodes are initialized.  The maximum collector flow rate is requested.

          ! USE STATEMENTS:
  USE DataGlobals,      ONLY: SysSizingCalc, InitConvTemp, DegToRadians, TimeStepZone, TimeStep, SecInHour, &
                              WarmupFlag, HourOfDay
  USE DataLoopNode,     ONLY: Node
  USE DataPlant
  USE FluidProperties,  ONLY: GetDensityGlycol
  USE PlantUtilities,   ONLY: InitComponentNodes, SetComponentFlowRate, RegisterPlantCompDesignFlow
  USE DataHVACGlobals, ONLY: SysTimeElapsed, TimeStepSys

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: CollectorNum

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: InletNode
  INTEGER :: OutletNode

  REAL(r64),PARAMETER   ::   BigNumber=9999.9d0     !Component desired mass flow rate

  LOGICAL, SAVE                            :: MyOneTimeFlag = .TRUE. ! one time flag
  LOGICAL, ALLOCATABLE, SAVE, DIMENSION(:) :: SetLoopIndexFlag       ! get loop number flag
  REAL(r64)   :: rho
  !LOGICAL     :: errFlag
!  REAL(r64)                                :: Density                ! density of fluid
  LOGICAL                                  :: errFlag                ! local error flag
  LOGICAL, ALLOCATABLE, SAVE, DIMENSION(:) :: SetDiffRadFlag         ! get diffuse radiation flag

  INTEGER           :: SurfNum                ! Surface object number for collector
  INTEGER           :: ParamNum               ! Collector parameters object number
  REAL(r64)         :: Tilt                   ! Surface tilt angle (degrees)
  REAL(r64)         :: Theta                  ! solar radiation incident angle (radians)
  REAL(r64)         :: TransSys               ! cover system solar transmittance
  REAL(r64)         :: RefSys                 ! cover system solar reflectance
  REAL(r64)         :: AbsCover1              ! Inner cover solar absorbtance
  REAL(r64)         :: AbsCover2              ! Outer cover solar absorbtance
  REAL(r64)         :: RefSysDiffuse          ! cover system solar reflectance
  REAL(r64)         :: TimeElapsed            ! Fraction of the current hour that has elapsed (h)
          ! FLOW:

  ! Do the one time initializations
  IF (MyOneTimeFlag) THEN
    ALLOCATE(SetLoopIndexFlag(NumOfCollectors))
    ALLOCATE(SetDiffRadFlag(NumOfCollectors))
    SetLoopIndexFlag = .TRUE.
    SetDiffRadFlag = .TRUE.
    MyOneTimeFlag = .FALSE.
  END IF

  IF(SetLoopIndexFlag(CollectorNum))THEN
    IF(ALLOCATED(PlantLoop))THEN
      errFlag=.false.
      CALL ScanPlantLoopsForObject(Collector(CollectorNum)%Name, &
                                   Collector(CollectorNum)%TypeNum, &
                                   Collector(CollectorNum)%WLoopNum, &
                                   Collector(CollectorNum)%WLoopSideNum, &
                                   Collector(CollectorNum)%WLoopBranchNum, &
                                   Collector(CollectorNum)%WLoopCompNum,   &
                                   errFlag=errFlag)
    IF (errFlag) THEN
      CALL ShowFatalError('InitSolarCollector: Program terminated due to previous condition(s).')
    ENDIF
      SetLoopIndexFlag(CollectorNum) = .FALSE.
    ENDIF
  ENDIF
          ! FLOW:
  InletNode  = Collector(CollectorNum)%InletNode
  OutletNode = Collector(CollectorNum)%OutletNode

  IF (.NOT. SysSizingCalc .AND. Collector(CollectorNum)%InitSizing) THEN
    CALL RegisterPlantCompDesignFlow(InletNode, Collector(CollectorNum)%VolFlowRateMax)
    Collector(CollectorNum)%InitSizing = .FALSE.
  END IF

  IF (BeginEnvrnFlag .AND. Collector(CollectorNum)%Init) THEN
    ! Clear node initial conditions
    IF (Collector(CollectorNum)%VolFlowRateMax >0)THEN                                                      !CR7425
      rho = GetDensityGlycol(PlantLoop(Collector(CollectorNum)%WLoopNum)%FluidName,  &
                             InitConvTemp, &
                             PlantLoop(Collector(CollectorNum)%WLoopNum)%FluidIndex,&
                            'InitSolarCollector')

      Collector(CollectorNum)%MassFlowRateMax = Collector(CollectorNum)%VolFlowRateMax * rho
    ELSE                                                                                                    !CR7425
      Collector(CollectorNum)%MassFlowRateMax = BigNumber                                                   !CR7425
    ENDIF                                                                                                   !CR7425

    CALL InitComponentNodes(0.d0, Collector(CollectorNum)%MassFlowRateMax, &
                                   InletNode,       &
                                   OutletNode,       &
                                   Collector(CollectorNum)%WLoopNum, &
                                   Collector(CollectorNum)%WLoopSideNum, &
                                   Collector(CollectorNum)%WLoopBranchNum, &
                                   Collector(CollectorNum)%WLoopCompNum)

    Collector(CollectorNum)%Init = .FALSE.

    IF ( Collector(CollectorNum)%InitICS ) THEN
        Collector(CollectorNum)%TempOfWater = 20.0d0
        Collector(CollectorNum)%SavedTempOfWater = Collector(CollectorNum)%TempOfWater
        Collector(CollectorNum)%SavedTempOfAbsPlate = Collector(CollectorNum)%TempOfWater
        Collector(CollectorNum)%TempOfAbsPlate = Collector(CollectorNum)%TempOfWater
        Collector(CollectorNum)%TempOfInnerCover = Collector(CollectorNum)%TempOfWater
        Collector(CollectorNum)%TempOfOuterCover = Collector(CollectorNum)%TempOfWater
        Collector(CollectorNum)%SavedTempOfInnerCover = Collector(CollectorNum)%TempOfWater
        Collector(CollectorNum)%SavedTempOfOuterCover = Collector(CollectorNum)%TempOfWater
        Collector(CollectorNum)%SavedTempCollectorOSCM = Collector(CollectorNum)%TempOfWater
        !Collector(CollectorNum)%SavedTempOfOutdoorAir = Collector(CollectorNum)%TempOfWater
    ENDIF

  END IF

  IF (.NOT. BeginEnvrnFlag) Collector(CollectorNum)%Init = .TRUE.

  IF (SetDiffRadFlag(CollectorNum)  .and. Collector(CollectorNum)%InitICS) THEN
      ! calculates the sky and ground reflective diffuse radiation optical properties (only one time)
      SurfNum = Collector(CollectorNum)%Surface
      ParamNum = Collector(CollectorNum)%Parameters

      Tilt = Surface(SurfNum)%Tilt
      Collector(CollectorNum)%Tilt = Tilt
      Collector(CollectorNum)%TiltR2V = Abs(90.0d0 - Tilt)
      Collector(CollectorNum)%CosTilt = cos(Tilt * DegToRadians)
      Collector(CollectorNum)%SinTilt = sin(1.8d0 * Tilt * DegToRadians)

      ! Diffuse refelectance of the cover for solar radiation diffusely reflected back from the absober
      ! plate to the cover.  The diffuse solar radiation reflected back from the absober plate to the
      ! cover is represented by the 60 degree equivalent incident angle.  This diffuse reflectance is
      ! used to calculate the transmittance - absorptance product (Duffie and Beckman, 1991)
      Theta = 60.0d0 * DegToRadians
      Call CalcTransRefAbsOfCover(CollectorNum,Theta,TransSys,RefSys,AbsCover1,AbsCover2,.TRUE.,RefSysDiffuse)
      Collector(CollectorNum)%RefDiffInnerCover = RefSysDiffuse

      ! transmittance-absorptance product normal incident:
      Theta = 0.0d0
      Call CalcTransRefAbsOfCover(CollectorNum,Theta,TransSys,RefSys,AbsCover1,AbsCover2)
      Collector(CollectorNum)%TauAlphaNormal = TransSys * Parameters(ParamNum)%AbsorOfAbsPlate   &
                                             / (1.d0-(1.d0-Parameters(ParamNum)%AbsorOfAbsPlate) &
                                             * Collector(CollectorNum)%RefDiffInnerCover)

      ! transmittance-absorptance product for sky diffuse radiation.  Uses equivalent incident angle
      ! of sky radiation (radians), and is calculated according to Brandemuehl and Beckman (1980):
      Theta = (59.68d0 - 0.1388d0 * Tilt + 0.001497d0 * Tilt**2) * DegToRadians
      Call CalcTransRefAbsOfCover(CollectorNum,Theta,TransSys,RefSys,AbsCover1,AbsCover2)
      Collector(CollectorNum)%TauAlphaSkyDiffuse = TransSys * Parameters(ParamNum)%AbsorOfAbsPlate   &
                                                 / (1.d0-(1.d0-Parameters(ParamNum)%AbsorOfAbsPlate) &
                                                 * Collector(CollectorNum)%RefDiffInnerCover)
      Collector(CollectorNum)%CoversAbsSkyDiffuse(1) = AbsCover1
      Collector(CollectorNum)%CoversAbsSkyDiffuse(2) = AbsCover2

      ! transmittance-absorptance product for ground diffuse radiation.  Uses equivalent incident angle
      ! of ground radiation (radians), and is calculated according to Brandemuehl and Beckman (1980):
      Theta = (90.0d0 - 0.5788d0 * Tilt + 0.002693d0 * Tilt**2) * DegToRadians
      Call CalcTransRefAbsOfCover(CollectorNum,Theta,TransSys,RefSys,AbsCover1,AbsCover2)
      Collector(CollectorNum)%TauAlphaGndDiffuse = TransSys * Parameters(ParamNum)%AbsorOfAbsPlate     &
                                                 / (1.d0- (1.d0- Parameters(ParamNum)%AbsorOfAbsPlate) &
                                                 * Collector(CollectorNum)%RefDiffInnerCover)
      Collector(CollectorNum)%CoversAbsGndDiffuse(1) = AbsCover1
      Collector(CollectorNum)%CoversAbsGndDiffuse(2) = AbsCover2

      SetDiffRadFlag(CollectorNum) = .FALSE.

  ENDIF


  Collector(CollectorNum)%InletTemp = Node(InletNode)%Temp


  Collector(CollectorNum)%MassFlowRate = Collector(CollectorNum)%MassFlowRateMax

  ! Request the mass flow rate from the plant component flow utility routine
  CALL SetComponentFlowRate(Collector(CollectorNum)%MassFlowRate,InletNode,OutletNode, &
            Collector(CollectorNum)%WLoopNum,Collector(CollectorNum)%WLoopSideNum,     &
            Collector(CollectorNum)%WLoopBranchNum, Collector(CollectorNum)%WLoopCompNum)

  IF (Collector(CollectorNum)%InitICS) THEN

      TimeElapsed = HourOfDay + TimeStep * TimeStepZone + SysTimeElapsed

      SurfNum = Collector(CollectorNum)%Surface

      IF (Collector(CollectorNum)%TimeElapsed /= TimeElapsed) THEN
        ! The simulation has advanced to the next system timestep.  Save conditions from the end of the previous
        ! system timestep for use as initial condition of each iteration that does not advance system timestep.
        Collector(CollectorNum)%SavedTempOfWater = Collector(CollectorNum)%TempOfWater
        Collector(CollectorNum)%SavedTempOfAbsPlate = Collector(CollectorNum)%TempOfAbsPlate
        Collector(CollectorNum)%SavedTempOfInnerCover = Collector(CollectorNum)%TempOfInnerCover
        Collector(CollectorNum)%SavedTempOfOuterCover = Collector(CollectorNum)%TempOfOuterCover
        IF ( Collector(CollectorNum)%OSCM_ON ) THEN
          CALL GetExtVentedCavityTsColl(Collector(CollectorNum)%VentCavIndex, &
                                        Collector(CollectorNum)%SavedTempCollectorOSCM)
        ENDIF
        Collector(CollectorNum)%TimeElapsed = TimeElapsed
      END IF
  ENDIF

  RETURN

END SUBROUTINE InitSolarCollector


SUBROUTINE CalcSolarCollector(CollectorNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Peter Graham Ellis
          !       DATE WRITTEN   January 2004
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Calculates the heat gain (or loss), outlet temperature, and solar energy conversion efficiency for a flat-plate
          ! solar collector when there is a fluid flow.  For the no flow condition, the fluid stagnation temperature is
          ! calculated as the outlet temperature.  Glazed and unglazed collectors are both handled.

          ! METHODOLOGY EMPLOYED:
          ! Calculation is performed using the methodology described in the ASHRAE standards and references below.  Measured
          ! collector performance coefficients (available from the Solar Rating & Certification Corporation, for example)
          ! are modified from the test conditions to match the actual optical (incident angle modifier) and thermal (flow rate
          ! modifier) conditions.  Water is assumed to be the heat transfer fluid.

          ! REFERENCES:
          ! ASHRAE Standard 93-1986 (RA 91), "Methods of Testing to Determine the Thermal Performance of Solar Collectors".
          ! ASHRAE Standard 96-1980 (RA 89), "Methods of Testing to Determine the Thermal Performance of Unglazed Flat-Plate
          !   Liquid-Type Solar Collectors".
          ! Duffie, J. A., and Beckman, W. A.  Solar Engineering of Thermal Processes, Second Edition.  Wiley-Interscience:
          !   New York (1991).

          ! NOTES:
          ! This subroutine has been validated against the TRNSYS Type 1 flat-plate solar collector module.  Results are
          ! identical except for slight differences at extreme incident angles (>80 degrees) and extreme surface tilts (<20
          ! degrees).  The differences are due to the fact that Type 1 does not prevent the *component* incident angle
          ! modifiers from being less than zero.  There is an effect on the net incident angle modifier if one or more
          ! components are less than zero but the net adds up to greater than zero.  The EnergyPlus subroutine, on the other
          ! hand, requires each component incident angle modifier always to be greater than zero.

          ! USE STATEMENTS:
  USE DataGlobals,     ONLY: DegToRadians
  USE DataHeatBalance, ONLY: CosIncidenceAngle, QRadSWOutIncident, QRadSWOutIncidentBeam, QRadSWOutIncidentSkyDiffuse, &
                             QRadSWOutIncidentGndDiffuse, TempConvergTol
  USE FluidProperties, ONLY: GetSpecificHeatGlycol
  USE DataPlant,       ONLY: PlantLoop, ccSimPlantEquipTypes

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: CollectorNum

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: SurfNum            ! Surface object number for collector
  INTEGER :: ParamNum           ! Collector parameters object number
  REAL(r64) :: Tilt                  ! Surface tilt angle (degrees)
  REAL(r64) :: IncidentAngleModifier ! Net incident angle modifier combining beam, sky, and ground radiation
  REAL(r64) :: ThetaBeam             ! Incident angle of beam radiation (radians)
  REAL(r64) :: ThetaSky              ! Equivalent incident angle of sky radiation (radians)
  REAL(r64) :: ThetaGnd              ! Equivalent incident angle of ground radiation (radians)
  REAL(r64) :: InletTemp             ! Inlet temperature from plant (C)
  REAL(r64) :: OutletTemp            ! Outlet temperature or stagnation temperature in the collector (C)
  REAL(r64) :: OutletTempPrev        ! Outlet temperature saved from previous iteration for convergence check (C)
  REAL(r64) :: MassFlowRate          ! Mass flow rate through collector (kg/s)
  REAL(r64) :: Cp                    ! Specific heat of collector fluid (J/kg-K)
  REAL(r64) :: Area                  ! Gross area of collector (m2)
  REAL(r64) :: mCpATest              ! = MassFlowRateTest * Cp / Area (tested area)
  REAL(r64) :: mCpA                  ! = MassFlowRate * Cp / Area
  REAL(r64) :: TestTypeMod           ! Modifier for test correlation type:  INLET, AVERAGE, or OUTLET
  REAL(r64) :: FlowMod               ! Modifier for flow rate different from test flow rate
  REAL(r64) :: FRULpTest             ! FR * ULoss "prime" for test conditions = (eff1 + eff2 * deltaT)
  REAL(r64) :: FpULTest              ! F prime * ULoss for test conditions = collector efficiency factor * overall loss coefficient
  REAL(r64) :: FRTAN                 ! FR * tau * alpha at normal incidence = Y-intercept of collector efficiency
  REAL(r64) :: FRUL                  ! FR * ULoss = 1st order coefficient of collector efficiency
  REAL(r64) :: FRULT                 ! FR * ULoss / T = 2nd order coefficent of collector efficiency
  REAL(r64) :: Q                     ! Heat gain or loss to collector fluid (W)
  REAL(r64) :: Efficiency            ! Thermal efficiency of solar energy conversion
  REAL(r64) :: A, B, C               ! Coefficients for solving the quadratic equation
  REAL(r64) :: qEquation             ! test for negative value in quadratic equation
  INTEGER :: Iteration          ! Counter of iterations until convergence

          ! FLOW:
  SurfNum = Collector(CollectorNum)%Surface
  ParamNum = Collector(CollectorNum)%Parameters

  ! Calculate incident angle modifier
  IF (QRadSWOutIncident(SurfNum) > 0.0d0) THEN
    ThetaBeam = ACOS(CosIncidenceAngle(SurfNum))

    ! Calculate equivalent incident angles for sky and ground radiation according to Brandemuehl and Beckman (1980)
    Tilt = Surface(SurfNum)%Tilt
    ThetaSky = (59.68d0 - 0.1388d0 * Tilt + 0.001497d0 * Tilt**2) * DegToRadians
    ThetaGnd = (90.0d0 - 0.5788d0 * Tilt + 0.002693d0 * Tilt**2) * DegToRadians

    IncidentAngleModifier = (QRadSWOutIncidentBeam(SurfNum) * IAM(ParamNum, ThetaBeam) &
      + QRadSWOutIncidentSkyDiffuse(SurfNum) * IAM(ParamNum, ThetaSky) &
      + QRadSWOutIncidentGndDiffuse(SurfNum) * IAM(ParamNum, ThetaGnd)) &
      / QRadSWOutIncident(SurfNum)
  ELSE
    IncidentAngleModifier = 0.0d0
  END IF

  InletTemp = Collector(CollectorNum)%InletTemp

  MassFlowRate = Collector(CollectorNum)%MassFlowRate

  Cp = GetSpecificHeatGlycol(PlantLoop(Collector(CollectorNum)%WLoopNum)%FluidName, &
                             InletTemp, &
                             PlantLoop(Collector(CollectorNum)%WLoopNum)%FluidIndex, &
                             'CalcSolarCollector')

  Area = Surface(SurfNum)%Area
  mCpA = MassFlowRate * Cp / Area

  ! CR 7920, changed next line to use tested area, not current surface area
  ! mCpATest = Parameters(ParamNum)%TestMassFlowRate * Cp / Area
  mCpATest = Parameters(ParamNum)%TestMassFlowRate * Cp / Parameters(Collector(CollectorNum)%Parameters)%Area

  Iteration = 1
  OutletTemp = 0.0d0
  OutletTempPrev = 999.9d0 ! Set to a ridiculous number so that DO loop runs at least once
  Q = 0.0d0

  DO WHILE (ABS(OutletTemp - OutletTempPrev) > TempConvergTol) ! Check for temperature convergence

    OutletTempPrev = OutletTemp ! Save previous outlet temperature

    ! Modify coefficients depending on test correlation type
    SELECT CASE (Parameters(ParamNum)%TestType)
      CASE (INLET)
        FRULpTest = Parameters(ParamNum)%eff1 + Parameters(ParamNum)%eff2 * (InletTemp - Surface(SurfNum)%OutDryBulbTemp)
        TestTypeMod = 1.0d0

      CASE (AVERAGE)
        FRULpTest = Parameters(ParamNum)%eff1 + Parameters(ParamNum)%eff2 *   &
                                   ((InletTemp + OutletTemp)*0.5d0 - Surface(SurfNum)%OutDryBulbTemp)
        TestTypeMod = 1.0d0/(1.0d0 - FRULpTest/(2.0d0 * mCpATest) )

      CASE (OUTLET)
        FRULpTest = Parameters(ParamNum)%eff1 + Parameters(ParamNum)%eff2 *   &
                                   (OutletTemp - Surface(SurfNum)%OutDryBulbTemp)
        TestTypeMod = 1.0d0/(1.0d0 - FRULpTest/mCpATest )
    END SELECT

    FRTAN = Parameters(ParamNum)%eff0 * TestTypeMod
    FRUL = Parameters(ParamNum)%eff1 * TestTypeMod
    FRULT = Parameters(ParamNum)%eff2 * TestTypeMod
    FRULpTest = FRULpTest * TestTypeMod

    IF (MassFlowRate > 0.0d0) THEN ! Calculate efficiency and heat transfer with flow

      IF ((1.0d0 + FRULpTest / mCpATest) > 0.0d0) THEN
        FpULTest = -mCpATest * LOG(1.0d0 + FRULpTest / mCpATest)
      ELSE
        FpULTest = FRULpTest ! Avoid LOG( <0 )
      END IF

      IF ((-FpULTest / mCpA) < 700.0D0) THEN
        FlowMod = mCpA * (1.0d0 - EXP(-FpULTest / mCpA))
      ELSE ! avoid EXP(too large #)
        FlowMod = FlowMod
      ENDIF
      IF ((-FpULTest / mCpATest) < 700.0D0) THEN
        FlowMod = FlowMod / (mCpATest * (1.0d0 - EXP(-FpULTest / mCpATest)))
      ELSE
        FlowMod = FlowMod
      ENDIF

      ! Calculate fluid heat gain (or loss)
      ! Heat loss is possible if there is no incident radiation and fluid is still flowing.
      Q = (FRTAN * IncidentAngleModifier * QRadSWOutIncident(SurfNum) + FRULpTest *   &
             (InletTemp - Surface(SurfNum)%OutDryBulbTemp) ) &
               * Area * FlowMod

      OutletTemp = InletTemp + Q / (MassFlowRate * Cp)

      ! CR 7877 bound unreasonable result
      IF (OutletTemp < -100) THEN
         OutletTemp = -100.0d0
         Q = MassFlowRate * Cp * (OutletTemp - InletTemp)
      ENDIF
      IF (OutletTemp > 200) THEN
         OutletTemp = 200.0d0
         Q = MassFlowRate * Cp * (OutletTemp - InletTemp)
      ENDIF

      IF (QRadSWOutIncident(SurfNum) > 0.0d0) THEN ! Calculate thermal efficiency
        ! NOTE: Efficiency can be > 1 if Q > QRadSWOutIncident because of favorable delta T, i.e. warm outdoor temperature
        Efficiency = Q / (QRadSWOutIncident(SurfNum) * Area) ! Q has units of W; QRadSWOutIncident has units of W/m2
      ELSE
        Efficiency = 0.0d0
      END IF

    ELSE ! Calculate stagnation temperature of fluid in collector (no flow)
      Q = 0.0d0
      Efficiency = 0.0d0

      ! Calculate temperature of stagnant fluid in collector
      A = -FRULT
      B = -FRUL + 2.0d0 * FRULT * Surface(SurfNum)%OutDryBulbTemp
      C = -FRULT * Surface(SurfNum)%OutDryBulbTemp**2 + FRUL * Surface(SurfNum)%OutDryBulbTemp &
          - FRTAN * IncidentAngleModifier * QRadSWOutIncident(SurfNum)
      qEquation=(B**2 - 4.0d0 * A * C)
      IF (qEquation < 0.0d0) THEN
        IF (Collector(CollectorNum)%ErrIndex == 0) THEN
          CALL ShowSevereMessage('CalcSolarCollector: '//trim(ccSimPlantEquipTypes(Collector(CollectorNum)%TypeNum))//  &
             '="'//trim(Collector(CollectorNum)%Name)//'", possible bad input coefficients.')
          CALL ShowContinueError('...coefficients cause negative quadratic equation part in '//  &
             'calculating temperature of stagnant fluid.')
          CALL ShowContinueError('...examine input coefficients for accuracy. Calculation will be treated as linear.')
        ENDIF
        CALL ShowRecurringSevereErrorAtEnd('CalcSolarCollector: '//  &
             trim(ccSimPlantEquipTypes(Collector(CollectorNum)%TypeNum))//'="'//trim(Collector(CollectorNum)%Name)//  &
                '", coefficient error continues.',    &
          Collector(CollectorNum)%ErrIndex,ReportMinOf=qEquation,ReportMaxOf=qEquation)
      ENDIF
      IF (FRULT == 0.0d0 .or. qEquation < 0.0d0) THEN ! Linear, 1st order solution
        OutletTemp = Surface(SurfNum)%OutDryBulbTemp - FRTAN * IncidentAngleModifier * QRadSWOutIncident(SurfNum) / FRUL
      ELSE ! Quadratic, 2nd order solution
        OutletTemp = (-B + qEquation**0.5d0) / (2.0d0 * A)
      END IF
    END IF

    IF (Parameters(ParamNum)%TestType == INLET) EXIT ! Inlet temperature test correlations do not need to iterate

    IF (Iteration > 100) THEN
      IF (Collector(CollectorNum)%IterErrIndex == 0) THEN
        CALL ShowWarningMessage('CalcSolarCollector: '//trim(ccSimPlantEquipTypes(Collector(CollectorNum)%TypeNum))//  &
         '="'//TRIM(Collector(CollectorNum)%Name)// &
         '":  Solution did not converge.')
      ENDIF
      CALL ShowRecurringWarningErrorAtEnd('CalcSolarCollector: '//  &
         trim(ccSimPlantEquipTypes(Collector(CollectorNum)%TypeNum))//'="'//trim(Collector(CollectorNum)%Name)//  &
         '", solution not converge error continues.',Collector(CollectorNum)%IterErrIndex)
      EXIT
    ELSE
      Iteration = Iteration + 1
    END IF

  END DO ! Check for temperature convergence

  Collector(CollectorNum)%IncidentAngleModifier = IncidentAngleModifier
  Collector(CollectorNum)%Power = Q
  Collector(CollectorNum)%HeatGain = MAX(Q, 0.0d0)
  Collector(CollectorNum)%HeatLoss = MIN(Q, 0.0d0)
  Collector(CollectorNum)%OutletTemp = OutletTemp
  Collector(CollectorNum)%Efficiency = Efficiency

  RETURN

END SUBROUTINE CalcSolarCollector


REAL(r64) FUNCTION IAM(ParamNum, IncidentAngle)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Peter Graham Ellis
          !       DATE WRITTEN   December 2003
          !       MODIFIED       Sept 2008, BG cut off IAM beyond 60 degrees.
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Calculates the incident angle modifier based on the solar collector parameters.  Both first and second order
          ! correlations are allowed.

          ! METHODOLOGY EMPLOYED:
          ! A simple function.

          ! REFERENCES:
          ! ASHRAE Standard 93-1986 (RA 91), "Methods of Testing to Determine the Thermal Performance of Solar Collectors".
          ! ASHRAE Standard 96-1980 (RA 89), "Methods of Testing to Determine the Thermal Performance of Unglazed Flat-Plate
          !   Liquid-Type Solar Collectors".
          ! Duffie, J. A., and Beckman, W. A.  Solar Engineering of Thermal Processes, Second Edition.  Wiley-Interscience:
          !   New York (1991).
  USE DataGlobals, ONLY: DegToRadians

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: ParamNum   ! Collector parameters object number
  REAL(r64), INTENT(IN) :: IncidentAngle ! Angle of incidence (radians)

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64) :: s                              ! Intermediate variable
  CHARACTER(len=MaxNameLength):: String  ! Dummy string for converting numbers to strings
  REAL(r64) :: CutOffAngle
            ! FLOW:

  !cut off IAM for angles greater than 60 degrees. (CR 7534)
  CutOffAngle = 60.0D0 * DegToRadians
  IF (ABS(IncidentAngle) > CutOffAngle) THEN ! cut off, model curves not robust beyond cutoff
    ! curves from FSEC/SRCC testing are only certified to 60 degrees, larger angles can cause numerical problems in curves
    IAM = 0.0D0
  ELSE

    s = (1.0d0 / COS(IncidentAngle)) - 1.0d0

    IAM = 1.0d0 + Parameters(ParamNum)%iam1 * s + Parameters(ParamNum)%iam2 * (s**2)
    IAM = MAX(IAM, 0.0d0) ! Never allow to be less than zero, but greater than one is a possibility

    IF (IAM > 10.0d0) THEN  ! Greater than 10 is probably not a possibility
      CALL ShowSevereError('IAM Function: SolarCollectorPerformance:FlatPlate = '//TRIM(Parameters(ParamNum)%Name)// &
        ':  Incident Angle Modifier is out of bounds due to bad coefficients.')
      WRITE(String, *) Parameters(ParamNum)%iam1
      CALL ShowContinueError('Coefficient 2 of Incident Angle Modifier ='//TRIM(String))
      WRITE(String, *) Parameters(ParamNum)%iam2
      CALL ShowContinueError('Coefficient 3 of Incident Angle Modifier ='//TRIM(String))
      WRITE(String, *) IAM
      CALL ShowContinueError('Calculated Incident Angle Modifier ='//TRIM(String))
      CALL ShowContinueError('Expected Incident Angle Modifier should be approximately 1.5 or less.')
      CALL ShowFatalError('Errors in SolarCollectorPerformance:FlatPlate input.')
    END IF

  ENDIF ! not greater than cut off angle

  RETURN

END FUNCTION IAM

SUBROUTINE CalcICSSolarCollector(ColleNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Bereket Nigusse, FSEC/UCF
          !       DATE WRITTEN   February 2012
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Calculates the heat transfered (gain or loss), energy stored, skin heat loss, outlet temperature, solar energy
          ! conversion efficiency, and transmittance-absorptance product of an ICS solar collector.

          ! METHODOLOGY EMPLOYED:
          ! The governing equations for the absorber and collector water heat balance equations are solved simultaneously.
          ! The two coupled first ODE are solved analytically.
          !
          ! The transmittance-absorptance product of the collector cover-absorber system is calcuated using ray tracing
          ! method according to Duffie and Beckman(1991).
          !
          ! REFERENCES:
          ! Duffie, J. A., and Beckman, W. A.  Solar Engineering of Thermal Processes, 2nd. Edition.  Wiley-Interscience:
          ! New York (1991).
          !
          ! NOTES:
          !

          ! USE STATEMENTS:
  USE DataGlobals,     ONLY: DegToRadians, TimeStepZone, TimeStep, SecInHour, WarmupFlag, HourOfDay
  USE DataHVACGlobals, ONLY: SysTimeElapsed, TimeStepSys
  USE DataHeatBalance, ONLY: CosIncidenceAngle, QRadSWOutIncident
  USE FluidProperties, ONLY: GetSpecificHeatGlycol, GetDensityGlycol
  USE DataPlant,       ONLY: PlantLoop
  USE DataLoopNode,    ONLY: Node

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: ColleNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
!  REAL(r64) :: TimeElapsed             ! Fraction of the current hour that has elapsed (h)
  INTEGER   :: SurfNum                 ! Surface object number for collector
  INTEGER   :: ParamNum                ! Collector parameters object number
  REAL(r64) :: ThetaBeam               ! Incident angle of beam radiation (radians)
  REAL(r64) :: InletTemp               ! Inlet temperature from plant (C)
  REAL(r64) :: OutletTemp              ! collector Outlet temperature (C)
  REAL(r64) :: MassFlowRate            ! Mass flow rate through collector (kg/s)
  REAL(r64) :: TempAbsPlate            ! absorber plate average temperature [C]
  REAL(r64) :: TempAbsPlateOld         ! absorber plate average temperature at previous time step [C]
  REAL(r64) :: TempWater               ! collector water average temperature [C]
  REAL(r64) :: TempWaterOld            ! collector water average temperature at previous time step  [C]
  REAL(r64) :: TempOutdoorAir          ! outdoor air temperature [C]
  REAL(r64) :: TempOSCM                ! Otherside condition model temperature [C]
  REAL(r64) :: hConvCoefA2W            ! convection coeff between absorber plate and water [W/m2K]
  REAL(r64) :: Cpw                     ! Specific heat of collector fluid (J/kg-K)
  REAL(r64) :: Rhow                    ! density of colloctor fluid (kg/m3)
  REAL(r64) :: Area                    ! Gross area of collector (m2)
  REAL(r64) :: aw                      ! thermal mass of the collector water [J/K]
!  REAL(r64) :: bw                      ! coefficients of the ODE of water heat balance
!  REAL(r64) :: cw                      ! coefficients of the ODE of water heat balance
  REAL(r64) :: ap                      ! thermal mass of the absorber plate [J/K]
!  REAL(r64) :: bp                      ! coefficients of the ODE of abs plate heat balance
!  REAL(r64) :: cp                      ! coefficients of the ODE of abs plate heat balance
  REAL(r64) :: SecInTimeStep           ! Seconds in one timestep (s)

!  REAL(r64) :: Q                       ! Heat gain or loss to collector fluid (W)
  REAL(r64) :: Efficiency              ! Thermal efficiency of solar energy conversion
!  REAL(r64) :: StoredHeatRate          ! collector heat storage rate (-ve, or +ve) [W]
!  REAL(r64) :: HeatLossRate            ! heat loss through the top, bottom and side of collector
!  REAL(r64) :: CollectorHeatRate       ! collector net heat gain rate
  REAL(r64) :: QHeatRate               ! heat gain rate (W)
  REAL(r64) :: a1                      ! coefficient of ODE for absorber temperature Tp
  REAL(r64) :: a2                      ! coefficient of ODE for absorber temperature Tw
  REAL(r64) :: a3                      ! conatant term of ODE for absorber temperature
  REAL(r64) :: b1                      ! coefficient of ODE for water temperature Tp
  REAL(r64) :: b2                      ! coefficient of ODE for water temperature Tw
  REAL(r64) :: b3                      ! conatant term of ODE for water temperature
  LOGICAL   :: AbsPlateMassFlag        ! flag if the absober has thermal mass or not

       ! FLOW:
  Efficiency = 0.d0
  QHeatRate = 0.d0

  SecInTimeStep = TimeStepSys * SecInHour
  SurfNum = Collector(ColleNum)%Surface
  ParamNum = Collector(ColleNum)%Parameters
  Area = Parameters(ParamNum)%Area
  TempWater = Collector(ColleNum)%SavedTempOfWater
  TempAbsPlate = Collector(ColleNum)%SavedTempOfAbsPlate
  TempOutdoorAir = Surface(SurfNum)%OutDryBulbTemp
  IF ( Collector(ColleNum)%OSCM_ON ) THEN
    TempOSCM = Collector(ColleNum)%SavedTempCollectorOSCM
  ELSE
    TempOSCM = TempOutdoorAir
  ENDIF

  ! Calculate transmittance-absorptance product of the system
  ThetaBeam = ACOS(CosIncidenceAngle(SurfNum))
  Call CalcTransAbsorProduct(ColleNum,ThetaBeam)

  InletTemp = Collector(ColleNum)%InletTemp

  MassFlowRate = Collector(ColleNum)%MassFlowRate

  Cpw = GetSpecificHeatGlycol(PlantLoop(Collector(ColleNum)%WLoopNum)%FluidName, &
                             InletTemp, &
                             PlantLoop(Collector(ColleNum)%WLoopNum)%FluidIndex, &
                             'CalcICSSolarCollector')

  Rhow = GetDensityGlycol(PlantLoop(Collector(ColleNum)%WLoopNum)%FluidName, &
                             InletTemp, &
                             PlantLoop(Collector(ColleNum)%WLoopNum)%FluidIndex, &
                             'CalcICSSolarCollector')

  ! calculate heat transfer coefficients and covers temperature:
  CALL CalcHeatTransCoeffAndCoverTemp(ColleNum)

  ! Calc convection heat transfer coefficient between the absorber plate and water:
  hConvCoefA2W = CalcConvCoeffAbsPlateAndWater(TempAbsPlate,TempWater,Collector(ColleNum)%Length, &
                                               Collector(ColleNum)%TiltR2V)
  TempWaterOld = TempWater
  TempAbsPlateOld = TempAbsPlate

  IF  ( Parameters(ParamNum)%ThermalMass .GT. 0.0d0) THEN
    AbsPlateMassFlag = .TRUE.
    ap = Parameters(ParamNum)%ThermalMass * Area
    a1 =-Area * (hConvCoefA2W + Collector(ColleNum)%UTopLoss) / ap
    a2 = Area * hConvCoefA2W / ap
    a3 = Area * (Collector(ColleNum)%TauAlpha * QRadSWOutIncident(SurfNum) &
        + Collector(ColleNum)%UTopLoss * TempOutdoorAir) / ap
  ELSE
    AbsPlateMassFlag = .FALSE.
    a1 =-Area * (hConvCoefA2W + Collector(ColleNum)%UTopLoss)
    a2 = Area * hConvCoefA2W
    a3 = Area * (Collector(ColleNum)%TauAlpha * QRadSWOutIncident(SurfNum) &
        + Collector(ColleNum)%UTopLoss * TempOutdoorAir)
  ENDIF
  aw = Parameters(ParamNum)%Volume*Rhow*Cpw
  b1 = Area*hConvCoefA2W / aw
  b2 =-(Area*(hConvCoefA2W+Collector(ColleNum)%UbLoss + Collector(ColleNum)%UsLoss) + MassFlowRate*Cpw)/aw
  b3 = (Area*(Collector(ColleNum)%UbLoss*TempOSCM + Collector(ColleNum)%UsLoss*TempOutdoorAir) &
      + MassFlowRate*Cpw*InletTemp)/aw

  Call ICSCollectorAnalyticalSoluton(ColleNum,SecInTimeStep,a1,a2,a3,b1,b2,b3,TempAbsPlateOld, &
                                     TempWaterOld,TempAbsPlate,TempWater,AbsPlateMassFlag)

  Collector(ColleNum)%SkinHeatLossRate = Area * &
          ( Collector(ColleNum)%UTopLoss*(TempOutdoorAir-TempAbsPlate)  &
          + Collector(ColleNum)%UsLoss*(TempOutdoorAir-TempWater)       &
          + Collector(ColleNum)%UbLoss*(TempOSCM-TempWater) )
  Collector(ColleNum)%StoredHeatRate = aw*(TempWater-TempWaterOld)/SecInTimeStep

  QHeatRate = MassFlowRate*Cpw*(TempWater-InletTemp)
  Collector(ColleNum)%HeatRate = QHeatRate
  Collector(ColleNum)%HeatGainRate = MAX(0.0d0, QHeatRate)
  Collector(ColleNum)%HeatLossRate = MIN(0.0d0, QHeatRate)

  OutletTemp = TempWater
  Collector(ColleNum)%OutletTemp = OutletTemp
  Collector(ColleNum)%TempOfWater = TempWater
  Collector(ColleNum)%TempOfAbsPlate = TempAbsPlate

  IF ( QRadSWOutIncident(SurfNum) .GT. 0.0d0 ) THEN
    Efficiency = (Collector(ColleNum)%HeatGainRate + Collector(ColleNum)%StoredHeatRate) &
               / (QRadSWOutIncident(SurfNum)*Area)
    IF (Efficiency .LT. 0.0d0) Efficiency = 0.0d0
  ENDIF
  Collector(ColleNum)%Efficiency = Efficiency

  RETURN

END SUBROUTINE CalcICSSolarCollector

SUBROUTINE ICSCollectorAnalyticalSoluton(ColleNum,SecInTimeStep,a1,a2,a3,b1,b2,b3,TempAbsPlateOld, &
                                         TempWaterOld,TempAbsPlate,TempWater,AbsorberPlateHasMass)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Bereket Nigusse, FSEC/UCF
          !       DATE WRITTEN   February 2012
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Calculates the absorber plate and collector water temperatures.
          !
          !
          ! METHODOLOGY EMPLOYED:
          ! Analytical method: Solves the coupled absorber plate and collector water energy balance
          ! equations.  The two non-homogeneous ordinary differential equations of the form.
          !          Tp' = a1*Tp + a2*Tw + a3.
          !          Tw' = b1*Tp + b2*Tw + b3.
          !
          ! The general solution of these coupled equation with real routes has the following form:
          !          Tp = ConstantC1*exp(lamda1*t) + ConstantC2*exp(lamda2*t) + ConstOfTpSln
          !          Tw = r1*ConstantC2*exp(lamda1*t) + r2*ConstantC2*exp(lamda2*t) + ConstOfTwSln
          !
          ! REFERENCES:
          !
          ! NOTES:
          !

          ! USE STATEMENTS:
  USE DataGlobals,     ONLY: DegToRadians, TimeStepZone, TimeStep, SecInHour, WarmupFlag, HourOfDay

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)     :: ColleNum              ! solar collector index
  REAL(r64), INTENT(IN)   :: SecInTimeStep         ! seconds in a time step
  REAL(r64), INTENT(IN)   :: a1                    ! coefficient of ODE for Tp
  REAL(r64), INTENT(IN)   :: a2                    ! coefficient of ODE for Tp
  REAL(r64), INTENT(IN)   :: a3                    ! coefficient of ODE for Tp
  REAL(r64), INTENT(IN)   :: b1                    ! coefficient of ODE for TW
  REAL(r64), INTENT(IN)   :: b2                    ! coefficient of ODE for TW
  REAL(r64), INTENT(IN)   :: b3                    ! coefficient of ODE for TW
  REAL(r64), INTENT(IN)   :: TempWaterOld          ! collector water temperature at previous time step [C]
  REAL(r64), INTENT(IN)   :: TempAbsPlateOld       ! absorber plate temperature at previous time step [C]
  REAL(r64), INTENT(OUT)  :: TempWater             ! collector water temperature at current time step [C]
  REAL(r64), INTENT(OUT)  :: TempAbsPlate          ! absorber plate temperature at current time step [C]
  LOGICAL,   INTENT(IN)   :: AbsorberPlateHasMass  ! flag for absober thermal mass

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64) :: BSquareM4TimesATimesC   ! intermediate variable
  REAL(r64) :: r1                      ! ratio of the ODE solution constant coefficients
  REAL(r64) :: r2                      ! ratio of the ODE solution constant coefficients
  REAL(r64) :: ConstantC1              ! coefficient of the ODE solution
  REAL(r64) :: ConstantC2              ! coefficient of the ODE solution
  REAL(r64) :: DetOfMatrix             ! intermediate variable
  REAL(r64) :: ConstOfTpSln            ! the particular solution for the ODE
  REAL(r64) :: ConstOfTwSln            ! the particular solution for the ODE
  REAL(r64) :: lamda1                  ! the real roots of the quadratic equation
  REAL(r64) :: lamda2                  ! the real roots of the quadratic equation
  REAL(r64) :: a                       ! coefficients of quadratic equation a*m2+b*m+c=0
  REAL(r64) :: b                       ! coefficients of quadratic equation a*m2+b*m+c=0
  REAL(r64) :: c                       ! coefficients of quadratic equation a*m2+b*m+c=0
          ! FLOW:

  IF ( AbsorberPlateHasMass ) Then
    a = 1.0d0
    b =-(a1+b2)
    c = a1*b2 - a2*b1
    BSquareM4TimesATimesC = b**2 - 4.0d0*a*c
    IF (BSquareM4TimesATimesC .GT. 0.0d0 )THEN
      lamda1 = (-b + SQRT(BSquareM4TimesATimesC) ) / (2.d0*a)
      lamda2 = (-b - SQRT(BSquareM4TimesATimesC) ) / (2.d0*a)
      DetOfMatrix = c
      ConstOfTpSln = (-a3*b2+b3*a2)/ DetOfMatrix
      ConstOfTwSln = (-a1*b3+b1*a3)/ DetOfMatrix
      r1 = (lamda1 - a1) / a2
      r2 = (lamda2 - a1) / a2
      ConstantC2 = (TempWaterOld + r1*ConstOfTpSln - r1*TempAbsPlateOld - ConstOfTwSln) / (r2-r1)
      ConstantC1 = (TempAbsPlateOld - ConstOfTpSln - ConstantC2)
      TempAbsPlate = ConstantC1*exp(lamda1*SecInTimeStep) + ConstantC2*exp(lamda2*SecInTimeStep) + ConstOfTpSln
      TempWater = r1*ConstantC1*exp(lamda1*SecInTimeStep) + r2*ConstantC2*exp(lamda2*SecInTimeStep) + ConstOfTwSln

    ELSE   ! this should never occur
      CALL ShowSevereError('ICSCollectorAnalyticalSoluton: Unanticipated differential equation coefficient - '// &
                           'report to EnergyPlus Development Team')
      CALL ShowFatalError('Program terminates due to above conditions.')
    ENDIF
  ELSE
    ! In the absence of absorber plate thermal mass, only the collector water heat balance has a
    ! differential equation of the form: Tw' = b1*Tp + b2*Tw + b3. The absorber plate energy balance
    ! equation in the absence of thermal mass is a steady state form:  b1*Tp + b2*Tw + b3 = 0
    b = b2 - b1 * (a2/a1)
    c = b3 - b1 * (a3/a1)
    TempWater = (TempWaterOld + c/b)*exp(b*SecInTimeStep) - c/b
    TempAbsPlate = -(a2*TempWater + a3)/a1

  ENDIF

  RETURN
END SUBROUTINE ICSCollectorAnalyticalSoluton

SUBROUTINE CalcTransAbsorProduct(ColleNum,IncidAngle)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Bereket A Nigusse
          !       DATE WRITTEN   February 2012
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Calculates transmittance-absorptance product and the fraction of total solar radiation
          ! absorbed by each cover of a multicover ICS solar collector.

          ! METHODOLOGY EMPLOYED:
          ! Uses a ray tracing method.

          ! REFERENCES:
          ! Duffie, J. A., and Beckman, W. A.  Solar Engineering of Thermal Processes, Second Edition.
          ! Wiley-Interscience: New York (1991).
          !
  USE DataGlobals,     ONLY: DegToRadians
  USE DataHeatBalance, ONLY: QRadSWOutIncident, QRadSWOutIncidentBeam, QRadSWOutIncidentSkyDiffuse, &
                             QRadSWOutIncidentGndDiffuse

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER,   INTENT(IN)    :: ColleNum        ! Collector object number
  REAL(r64), INTENT(IN)    :: IncidAngle      ! Angle of incidence (radians)

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER           :: ParamNum               ! Collector parameters object number
  INTEGER           :: SurfNum                ! Surface object number for collector
  INTEGER           :: Num                    ! covers counter
  REAL(r64)         :: TuaAlphaBeam           ! trans-abs product of beam radiation
  REAL(r64)         :: TuaAlpha               ! weighted trans-abs product of system
  REAL(r64)         :: TransSys               ! cover system solar transmittance
  REAL(r64)         :: ReflSys                ! cover system solar reflectance
  REAL(r64)         :: AbsCover1              ! Inner cover solar absorbtance
  REAL(r64)         :: AbsCover2              ! Outer cover solar absorbtance
  REAL(r64)         :: CoversAbsBeam(2)       ! Inner and Outer Cover absorptance
                    ! FLOW:

  ! set
  TransSys  = 1.0d0
  ReflSys   = 0.0d0
  AbsCover1 = 0.0d0
  AbsCover2 = 0.0d0
  TuaAlpha  = 0.0d0
  TuaAlphaBeam = 0.0d0
  Collector(ColleNum)%CoverAbs(1) = 0.0d0
  Collector(ColleNum)%CoverAbs(2) = 0.0d0

  SurfNum = Collector(ColleNum)%Surface
  ParamNum = Collector(ColleNum)%Parameters

  IF (QRadSWOutIncident(SurfNum) > 0.d0) THEN

    ! cover system transmittance and reflectance from outer to inner cover
    Call CalcTransRefAbsOfCover(ColleNum,IncidAngle,TransSys,ReflSys,AbsCover1,AbsCover2)

    TuaAlphaBeam = TransSys * Parameters(ParamNum)%AbsorOfAbsPlate        &
                 / (1.0d0- (1.0d0 - Parameters(ParamNum)%AbsorOfAbsPlate) &
                 * Collector(ColleNum)%RefDiffInnerCover)

    Collector(ColleNum)%TauAlphaBeam = MAX(0.d0, TuaAlphaBeam)
    CoversAbsBeam(1) = AbsCover1
    CoversAbsBeam(2) = AbsCover2

    ! calc total solar radiation weighted transmittance-absorptance product
    TuaAlpha = (QRadSWOutIncidentBeam(SurfNum) * Collector(ColleNum)%TauAlphaBeam             &
             + QRadSWOutIncidentSkyDiffuse(SurfNum) * Collector(ColleNum)%TauAlphaSkyDiffuse  &
             + QRadSWOutIncidentGndDiffuse(SurfNum) * Collector(ColleNum)%TauAlphaGndDiffuse) &
             / QRadSWOutIncident(SurfNum)

   IF ( Parameters(ParamNum)%NumOfCovers == 1) THEN
       ! calc total solar radiation weighted cover absorptance
       Collector(ColleNum)%CoverAbs(1) = &
           (QRadSWOutIncidentBeam(SurfNum) * CoversAbsBeam(1) &
         + QRadSWOutIncidentSkyDiffuse(SurfNum) * Collector(ColleNum)%CoversAbsSkyDiffuse(1)  &
         + QRadSWOutIncidentGndDiffuse(SurfNum) * Collector(ColleNum)%CoversAbsGndDiffuse(1)) &
         / QRadSWOutIncident(SurfNum)

   ELSEIF ( Parameters(ParamNum)%NumOfCovers == 2) THEN
      ! Num = 1 represents outer cover and Num = 2 represents inner cover
     DO Num = 1, Parameters(ParamNum)%NumOfCovers
       Collector(ColleNum)%CoverAbs(Num) = &
         (QRadSWOutIncidentBeam(SurfNum) * CoversAbsBeam(Num) &
       + QRadSWOutIncidentSkyDiffuse(SurfNum) * Collector(ColleNum)%CoversAbsSkyDiffuse(Num)  &
       + QRadSWOutIncidentGndDiffuse(SurfNum) * Collector(ColleNum)%CoversAbsGndDiffuse(Num)) &
       / QRadSWOutIncident(SurfNum)
     END DO
   ENDIF

  ELSE
    TuaAlpha  = 0.0d0
  END IF
  Collector(ColleNum)%TauAlpha = TuaAlpha

  RETURN
END SUBROUTINE CalcTransAbsorProduct

SUBROUTINE CalcTransRefAbsOfCover(ColleNum,IncidentAngle,TransSys,ReflSys,AbsCover1,AbsCover2,InOUTFlag,RefSysDiffuse)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Bereket A Nigusse
          !       DATE WRITTEN   February 2012
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Calculates the transmitance, reflectance, and absorptance of the collector covers based on
          ! solar collector optical parameters specified.

          ! METHODOLOGY EMPLOYED:
          ! Uses a ray tracing method.

          ! REFERENCES:
          ! Duffie, J. A., and Beckman, W. A.  Solar Engineering of Thermal Processes, Second Edition.
          ! Wiley-Interscience: New York (1991).
          !
  USE DataGlobals,     ONLY: DegToRadians

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER,   INTENT(IN)  :: ColleNum          ! Collector object number
  REAL(r64), INTENT(IN)  :: IncidentAngle     ! Angle of incidence (radians)
  REAL(r64), INTENT(OUT) :: TransSys          ! cover system solar transmittance
  REAL(r64), INTENT(OUT) :: ReflSys           ! cover system solar reflectance
  REAL(r64), INTENT(OUT) :: AbsCover1         ! Inner cover solar absorbtance
  REAL(r64), INTENT(OUT) :: AbsCover2         ! Outer cover solar absorbtance
  LOGICAL,   INTENT(IN),   OPTIONAL :: InOUTFlag     ! flag for calc. diffuse solar refl of cover from inside out
  REAL(r64), INTENT(OUT),  OPTIONAL :: RefSysDiffuse ! cover system solar reflectance from inner to outer cover

          ! FUNCTION PARAMETER DEFINITIONS:
  REAL(r64), PARAMETER  :: AirRefIndex   = 1.0003d0   ! refractive index of air


          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
!  CHARACTER(len=MaxNameLength):: String       ! Dummy string for converting numbers to strings
  INTEGER      :: nCover                      ! covers count
  INTEGER      :: ParamNum                    ! Collector parameters object number
  REAL(r64)    :: IncAngle                    ! angle of incidence
  REAL(r64)    :: RefrAngle                   ! angle of refraction
  REAL(r64)    :: ParaRad                     ! parallel reflected component of unpolarized solar radiation
  REAL(r64)    :: PerpRad                     ! Perpendicular reflected component of unpolarized solar radiation
  REAL(r64)    :: TransPara(2)                ! cover transmittance parallel component
  REAL(r64)    :: TransPerp(2)                ! cover transmittance perpendicular component
  REAL(r64)    :: ReflPara(2)                 ! cover reflectance parallel component
  REAL(r64)    :: ReflPerp(2)                 ! cover reflectance Perpendicular component
  REAL(r64)    :: AbsorPara(2)                ! cover absorbtance parallel component
  REAL(r64)    :: AbsorPerp(2)                ! cover absorbtance Perpendicular component
  REAL(r64)    :: TransAbsOnly(2)             ! cover transmittance with absorptance only considered
  REAL(r64)    :: CoverRefrIndex              ! refractive index of collector cover
  REAL(r64)    :: TransSysDiff                ! cover system solar transmittance from inner to outer cover
  LOGICAL      :: DiffRefFlag                 ! flag for calc. diffuse refl of cover from inside to outside
!  LOGICAL      :: OneTimeFlag                 ! allows to run only once
        ! FLOW:

  ! set
  TransPerp = 1.0d0
  TransPara = 1.0d0
  ReflPerp  = 0.0d0
  ReflPara  = 0.0d0
  AbsorPerp = 0.0d0
  AbsorPara = 0.0d0
  TransAbsOnly = 1.0d0
  TransSys = 0.0d0
  ReflSys  = 0.0d0
  AbsCover1 = 0.0d0
  AbsCover2 = 0.0d0

  IF (PRESENT(InOUTFlag)) THEN
     DiffRefFlag = InOUTFlag
  ELSE
     DiffRefFlag = .FALSE.
  ENDIF

  ! get the incidence and refraction angles
  IncAngle   = IncidentAngle
  ParamNum     = Collector(ColleNum)%Parameters

  DO nCover = 1, Parameters(ParamNum)%NumOfCovers

    CoverRefrIndex = Parameters(ParamNum)%RefractiveIndex(nCover)

    RefrAngle = ASIN( SIN(IncAngle) * AirRefIndex / CoverRefrIndex )

    ! transmitted component with absorption only considered:
    TransAbsOnly(nCover) = EXP(-Parameters(ParamNum)%ExtCoefTimesThickness(nCover)/COS(RefrAngle))

    ! parallel and perpendicular reflection components:
    IF (IncAngle == 0.d0) THEN
    ParaRad = ((CoverRefrIndex - AirRefIndex)/ (CoverRefrIndex + AirRefIndex))**2
    PerpRad = ((CoverRefrIndex - AirRefIndex)/ (CoverRefrIndex + AirRefIndex))**2
    ELSE
    ParaRad = TAN(RefrAngle - IncAngle)**2 / TAN(RefrAngle + IncAngle)**2
    PerpRad = SIN(RefrAngle - IncAngle)**2 / SIN(RefrAngle + IncAngle)**2
    ENDIF

    ! parallel and perpendicular transmitted components:
    TransPerp(nCover) = TransAbsOnly(nCover)*((1.d0-PerpRad)/(1.d0+PerpRad))*((1.d0-PerpRad**2) &
                    / (1.d0-(PerpRad*TransAbsOnly(nCover))**2))
    TransPara(nCover) = TransAbsOnly(nCover)*((1.d0-ParaRad)/(1.d0+ParaRad))*((1.d0-ParaRad**2) &
                    / (1.d0-(ParaRad*TransAbsOnly(nCover))**2))

    ReflPerp(nCover) = (PerpRad + (((1.d0-PerpRad)**2)*(TransAbsOnly(nCover)**2)*PerpRad)   &
                    / (1.d0-(PerpRad*TransAbsOnly(nCover))**2))
    ReflPara(nCover) = (ParaRad + (((1.d0-ParaRad)**2)*(TransAbsOnly(nCover)**2)*ParaRad)   &
                    / (1.d0-(ParaRad*TransAbsOnly(nCover))**2))

    AbsorPerp(nCover) = 1.d0 - TransPerp(nCover) - ReflPerp(nCover)
    AbsorPara(nCover) = 1.d0 - TransPara(nCover) - ReflPara(nCover)
  END DO

  ! solar absorptance of the individual cover
  AbsCover1 = 0.5d0 * (AbsorPerp(1) + AbsorPara(1))
  if (Parameters(ParamNum)%NumOfCovers == 2) AbsCover2 = 0.5d0 * (AbsorPerp(2) + AbsorPara(2))

  ! calculate from outer to inner cover:
  TransSys = 0.5d0*(TransPerp(1)*TransPerp(2)/(1.d0- ReflPerp(1)*ReflPerp(2))   &
           + TransPara(1)*TransPara(2)/(1.d0 - ReflPara(1)*ReflPara(2)))
  ReflSys  = 0.5d0*(ReflPerp(1)+TransSys*ReflPerp(2)*TransPerp(1)/TransPerp(2)  &
           + ReflPara(1)+TransSys*ReflPara(2)*TransPara(1)/TransPara(2))
  IF (DiffRefFlag) Then
    ! calculate from inner to outer cover:
    TransSysDiff  = 0.5d0*(TransPerp(2)*TransPerp(1)/(1.d0 - ReflPerp(2)*ReflPerp(1))     &
                    + TransPara(2)*TransPara(1)/(1.d0 - ReflPara(2)*ReflPara(1)))
    RefSysDiffuse = 0.5d0*(ReflPerp(2)+TransSysDiff*ReflPerp(1)*TransPerp(2)/TransPerp(1) &
                    + ReflPara(2)+TransSysDiff*ReflPara(1)*TransPara(2)/TransPara(1))
  ENDIF


  RETURN

END SUBROUTINE CalcTransRefAbsOfCover

SUBROUTINE CalcHeatTransCoeffAndCoverTemp(ColleNum)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Bereket A Nigusse, FSEC/UCF
          !       DATE WRITTEN   February 2012
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Calculates the various heat transfer coefficients, and collector cover temperatures.

          ! METHODOLOGY EMPLOYED:
          !

          ! REFERENCES:
          ! Duffie, J. A., and Beckman, W. A.  Solar Engineering of Thermal Processes, Second Edition.
          ! Wiley-Interscience: New York (1991).
          !
  USE DataGlobals,     ONLY: StefanBoltzmann, KelvinConv
  USE DataEnvironment, ONLY: SkyTemp, SkyTempKelvin, GroundTemp, GroundTempKelvin
  USE DataHeatBalance, ONLY: QRadSWOutIncident

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER,   INTENT(IN)  :: ColleNum          ! Collector object number

          ! FUNCTION PARAMETER DEFINITIONS:
   REAL(r64), PARAMETER  :: gravity        = 9.806d0     ! gravitational constant [m/s^2]
   REAL(r64), PARAMETER  :: SmallNumber    = 1.00d-20    ! small number to avoid div by zero

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
!  CHARACTER(len=MaxNameLength):: String        ! Dummy string for converting numbers to strings
  INTEGER            :: ParamNum               ! collector parameters object number
  INTEGER            :: CoverNum               ! counter for number of covers
  INTEGER            :: NumCovers              ! number of covers
  INTEGER            :: SurfNum                ! surface number
  INTEGER            :: Num                    ! covers counter
  REAL(r64)          :: tempnom                ! intermediate variable
  REAL(r64)          :: tempdenom              ! intermediate variable
  REAL(r64)          :: AirGapDepth            ! characteristic length [m]
  REAL(r64)          :: TempAbsPlate           ! absorber plate average temperature [C]
  REAL(r64)          :: TempInnerCover         ! inner cover average temperature [C]
  REAL(r64)          :: TempOuterCover         ! outer cover average temperature [C]
  REAL(r64)          :: TempOutdoorAir         ! outdoor air temperature [C]
  REAL(r64)          :: EmissOfAbsPlate        ! emissivity of absorber plate
  REAL(r64)          :: EmissOfInnerCover      ! emissivity of inner cover
  REAL(r64)          :: EmissOfOuterCover      ! emissivity of outer cover
  REAL(r64)          :: UTopLoss               ! over all top heat loss coefficient [W/m2C]
  REAL(r64)          :: hRadCoefC2Sky          ! radiation coeff from collector to the sky [W/m2C]
  REAL(r64)          :: hRadCoefC2Gnd          ! radiation coeff from collector to the ground [W/m2C]
  REAL(r64)          :: hRadConvOut            ! combined convection-radiation coefficient [W/m2C]
  REAL(r64)          :: hConvCoefA2C           ! convection coeff. between abs plate and cover [W/m2C]
  REAL(r64)          :: hConvCoefC2C           ! convection coeff. between covers [W/m2C]
  REAL(r64)          :: hConvCoefC2O           ! convection coeff. between outer cover and the ambient [W/m2C]
  REAL(r64)          :: hRadCoefA2C            ! radiation coeff. between abs plate and cover [W/m2C]
  REAL(r64)          :: hRadCoefC2C            ! radiation coeff. between covers [W/m2C]
  REAL(r64)          :: hRadCoefC2O            ! radiation coeff. between outer covers and the ambient [W/m2C]

           ! flow

  UTopLoss = 0.d0
  ParamNum = Collector(ColleNum)%Parameters
  NumCovers = Parameters(ParamNum)%NumOfCovers
  SurfNum = Collector(ColleNum)%Surface

  TempAbsPlate = Collector(ColleNum)%SavedTempOfAbsPlate
  TempInnerCover = Collector(ColleNum)%SavedTempOfInnerCover
  TempOuterCover = Collector(ColleNum)%SavedTempOfOuterCover
  TempOutdoorAir = Surface(SurfNum)%OutDryBulbTemp

  EmissOfAbsPlate = Parameters(ParamNum)%EmissOfAbsPlate
  EmissOfOuterCover = Parameters(ParamNum)%EmissOfCover(1)
  EmissOfInnerCover = Parameters(ParamNum)%EmissOfCover(2)
  AirGapDepth = Parameters(ParamNum)%CoverSpacing

  Select Case (NumCovers)
  Case (1)
    ! calc linearized radiation coefficient
    tempnom = StefanBoltzmann*((TempAbsPlate+KelvinConv)+(TempOuterCover+KelvinConv)) &
            * ((TempAbsPlate+KelvinConv)**2+(TempOuterCover+KelvinConv)**2)
    tempdenom = 1.d0/EmissOfAbsPlate+1.d0/EmissOfOuterCover-1.d0
    hRadCoefA2C = tempnom/tempdenom
    hRadCoefC2C = 0.0d0
    hConvCoefC2C= 0.0d0
    ! Calc convection heat transfer coefficient:
    hConvCoefA2C = CalcConvCoeffBetweenPlates(TempAbsPlate,TempOuterCover,AirGapDepth, &
                                              Collector(ColleNum)%CosTilt,Collector(ColleNum)%SinTilt)
  Case (2)
    DO CoverNum = 1, NumCovers
      IF (CoverNum == 1) THEN
        ! calc linearized radiation coefficient
        tempnom = StefanBoltzmann*((TempAbsPlate+KelvinConv)+(TempInnerCover+KelvinConv)) &
                * ((TempAbsPlate+KelvinConv)**2+(TempInnerCover+KelvinConv)**2)
        tempdenom = 1.d0/EmissOfAbsPlate+1.d0/EmissOfInnerCover-1.d0
        hRadCoefA2C = tempnom / tempdenom
        ! Calc convection heat transfer coefficient:
        hConvCoefA2C = CalcConvCoeffBetweenPlates(TempAbsPlate,TempOuterCover,AirGapDepth, &
                                                  Collector(ColleNum)%CosTilt,Collector(ColleNum)%SinTilt)
      ELSE
        ! calculate the linearized radiation coeff.
        tempnom = StefanBoltzmann*((TempInnerCover+KelvinConv)+(TempOuterCover+KelvinConv)) &
                * ((TempInnerCover+KelvinConv)**2+(TempOuterCover+KelvinConv)**2)
        tempdenom = 1.d0/EmissOfInnerCover+1.d0/EmissOfOuterCover-1.d0
        hRadCoefC2C = tempnom / tempdenom
        ! Calc convection heat transfer coefficient:
        hConvCoefC2C = CalcConvCoeffBetweenPlates(TempInnerCover,TempOuterCover,AirGapDepth, &
                                                  Collector(ColleNum)%CosTilt,Collector(ColleNum)%SinTilt)
      ENDIF
    END DO
  END SELECT

  ! Calc collector outside surface convection heat transfer coefficient:
  hConvCoefC2O = 2.8d0+3.0d0*Surface(SurfNum)%WindSpeed

  ! Calc linearized radiation coefficient between outer cover and the surrounding:
  tempnom = Surface(SurfNum)%ViewFactorSky*EmissOfOuterCover*StefanBoltzmann  &
          * ((TempOuterCover+KelvinConv)+SkyTempKelvin) * ((TempOuterCover+KelvinConv)**2 + SkyTempKelvin**2)
  tempdenom = (TempOuterCover-TempOutdoorAir)/(TempOuterCover-SkyTemp)
  IF (tempdenom < 0.0d0) THEN
    ! use approximate linearized radiation coefficient
    hRadCoefC2Sky = tempnom
  ELSEIF (tempdenom == 0.0d0) THEN
    ! if temperature difference is zero, no radiation exchange
    hRadCoefC2Sky = 0.0d0
  ELSE
    hRadCoefC2Sky = tempnom / tempdenom
  ENDIF

  tempnom = Surface(SurfNum)%ViewFactorGround*EmissOfOuterCover*StefanBoltzmann &
          * ((TempOuterCover+KelvinConv)+GroundTempKelvin) * ((TempOuterCover+KelvinConv)**2+GroundTempKelvin**2)
  tempdenom = (TempOuterCover-TempOutdoorAir)/(TempOuterCover-GroundTemp)
  IF (tempdenom < 0.0d0) THEN
    ! use approximate linearized radiation coefficient
    hRadCoefC2Gnd = tempnom
  ELSEIF (tempdenom == 0.0d0) THEN
    ! if temperature difference is zero, no radiation exchange
    hRadCoefC2Gnd = 0.0d0
  ELSE
    hRadCoefC2Gnd = tempnom / tempdenom
  ENDIF

  ! combine the radiation coefficients
  hRadCoefC2O = hRadCoefC2Sky+hRadCoefC2Gnd


  ! calculate the overall top heat loss coefficient:
  IF (NumCovers == 1 ) THEN
    UTopLoss = 1.d0/(1.d0/(hRadCoefA2C+hConvCoefA2C)+1.d0/(hRadCoefC2O + hConvCoefC2O))
  ELSE
    UTopLoss = 1.d0/(1.d0/(hRadCoefA2C+hConvCoefA2C)+1.d0/(hRadCoefC2C + hConvCoefC2C) &
             + 1.d0/(hRadCoefC2O+hConvCoefC2O))
  ENDIF
  Collector(ColleNum)%UTopLoss = UTopLoss

  ! calculate the side loss coefficient.  Adds the insulation resistance and the combined
  ! convection-radiation coefficients in series.
  hRadConvOut = 5.7d0 + 3.8d0 * Surface(SurfNum)%WindSpeed
  Collector(ColleNum)%UsLoss = 1.d0/(1.d0/(Parameters(ParamNum)%ULossSide*Collector(ColleNum)%AreaRatio) &
                             + 1.d0/(hRadConvOut*Collector(ColleNum)%AreaRatio))

  ! the bottom loss coefficient calculation depends on the boundary condition
  IF ( Collector(ColleNum)%OSCM_ON ) THEN     ! OtherSideConditionsModel
    Collector(ColleNum)%UbLoss = Parameters(ParamNum)%ULossBottom
  ELSE                                        ! AmbientAir
    Collector(ColleNum)%UbLoss = 1.d0/(1.0d0/Parameters(ParamNum)%ULossBottom+1.d0/hRadConvOut)
  ENDIF


  ! Calculate current timestep covers temperature
  Select Case (NumCovers)
  Case (1)
        tempnom = Collector(ColleNum)%CoverAbs(1)*QRadSWOutIncident(SurfNum)    &
                + TempOutdoorAir*(hConvCoefC2O+hRadCoefC2O) + TempAbsPlate*(hConvCoefA2C+hRadCoefA2C)
        tempdenom = (hConvCoefC2O+hRadCoefC2O)+(hConvCoefA2C+hRadCoefA2C)
        TempOuterCover = tempnom / tempdenom
  Case (2)
    DO Num = 1,  NumCovers
      IF (Num == 1) THEN
        tempnom = Collector(ColleNum)%CoverAbs(Num)*QRadSWOutIncident(SurfNum)  &
                + TempOutdoorAir*(hConvCoefC2O+hRadCoefC2O) &
                + TempInnerCover*(hConvCoefC2C+hRadCoefC2C)
        tempdenom = (hConvCoefC2O+hRadCoefC2O)+(hConvCoefC2C+hRadCoefC2C)
        TempOuterCover = tempnom / tempdenom
      ELSEIF (Num == 2) THEN
        tempnom = Collector(ColleNum)%CoverAbs(Num)*QRadSWOutIncident(SurfNum)  &
                + TempAbsPlate*(hConvCoefA2C+hRadCoefA2C)   &
                + TempOuterCover*(hConvCoefC2C+hRadCoefC2C)
        tempdenom = (hConvCoefC2C+hRadCoefC2C+hConvCoefA2C+hRadCoefA2C)
        TempInnerCover = tempnom / tempdenom
      ENDIF
    END DO
  End Select
  Collector(ColleNum)%TempOfInnerCover = TempInnerCover
  Collector(ColleNum)%TempOfOuterCover = TempOuterCover

  RETURN
END SUBROUTINE CalcHeatTransCoeffAndCoverTemp

FUNCTION CalcConvCoeffBetweenPlates(TempSurf1,TempSurf2,AirGap,CosTilt,SinTilt) RESULT (hConvCoef)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Bereket Nigusse, FSEC/UCF
          !       DATE WRITTEN   February 2012
          !       MODIFIED
          !       RE-ENGINEERED

          ! PURPOSE OF THIS FUNCTION:
          !   Calculates the converction coefficient for an enclosure between two paralel surfaces
          !   at different temperatures.
          !
          ! METHODOLOGY EMPLOYED:
          !   Uses empirical correlation by Holands et al (1976) to determine free convection between
          !   inclined parallel plates at different temperature.
          !
          ! REFERENCES:
          !   Duffie, J. A., and Beckman, W. A.  Solar Engineering of Thermal Processes, 2nd. Edition.
          !   Wiley-Interscience: New York (1991).
          !   Property data for air at atmospheric pressure were taken from Table A-11, Yunus A Cengel
          !   Heat Transfer: A Practical Approach, McGraw-Hill, Boston, MA, 1998.

          ! USE STATEMENTS:
  USE DataGlobals,     ONLY: StefanBoltzmann, KelvinConv

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  REAL(r64), INTENT(IN)      :: TempSurf1  ! temperature of surface 1
  REAL(r64), INTENT(IN)      :: TempSurf2  ! temperature of surface 1
  REAL(r64), INTENT(IN)      :: SinTilt    ! sine of surface tilt angle relative to the horizontal
  REAL(r64), INTENT(IN)      :: CosTilt    ! cosine of surface tilt angle relative to the horizontal
  REAL(r64), INTENT(IN)      :: AirGap     ! characteristic length [m]

          ! FUNCTION PARAMETER DEFINITIONS:
  REAL(r64), PARAMETER  :: gravity        = 9.806d0         ! gravitational constant [m/s^2]

  INTEGER, PARAMETER :: NumOfPropDivisions = 11
  REAL(r64), PARAMETER, DIMENSION(NumOfPropDivisions) :: Temps=  &            ! Temperature, in C
                   (/-23.15d0,6.85d0,16.85d0,24.85d0,26.85d0,36.85d0,46.85d0,56.85d0,66.85d0,76.85d0,126.85d0/)
  REAL(r64), PARAMETER, DIMENSION(NumOfPropDivisions) :: Mu=  &               ! Viscosity, in kg/(m.s)
                   (/.0000161d0,.0000175d0,.000018d0,.0000184d0,.0000185d0,.000019d0,.0000194d0,.0000199d0, &
                     .0000203d0,.0000208d0,.0000229d0/)
  REAL(r64), PARAMETER, DIMENSION(NumOfPropDivisions) :: Conductivity=  &     ! Conductivity, in W/mK
                   (/.0223d0,.0246d0,.0253d0,.0259d0,.0261d0,.0268d0,.0275d0,.0283d0,.0290d0,.0297d0,.0331d0/)
  REAL(r64), PARAMETER, DIMENSION(NumOfPropDivisions) :: Pr=  &               ! Prandtl number (dimensionless)
                   (/.724d0,.717d0,.714d0,.712d0,.712d0,.711d0,.71d0,.708d0,.707d0,.706d0,.703d0/)
  REAL(r64), PARAMETER, DIMENSION(NumOfPropDivisions) :: Density=  &          ! Density, in kg/m3
                   (/1.413d0,1.271d0,1.224d0,1.186d0,1.177d0,1.143d0,1.110d0,1.076d0,1.043d0,1.009d0,0.883d0/)

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  REAL(r64)          :: CondOfAir              ! thermal conductivity of air [W/mK]
  REAL(r64)          :: VisDOfAir              ! dynamic viscosity of air [kg/m.s]
  REAL(r64)          :: DensOfAir              ! density of air [W/mK]
  REAL(r64)          :: PrOfAir                ! Prantle number of air [W/mK]
  REAL(r64)          :: VolExpAir              ! volumetric expansion of air [1/K]
  REAL(r64)          :: RaNumCosTilt           ! Rayleigh number of air times cosine of collector tilt []
  REAL(r64)          :: DeltaT                 ! temperature difference between absober plate and water
  REAL(r64)          :: Tref                   ! reference temperature for fluid properties [c]
  REAL(r64)          :: RaNum                  ! Rayleigh number
!  REAL(r64)          :: GrNum                  ! Grashof number
  REAL(r64)          :: NuL                    ! Nusselt number
  REAL(r64)          :: hConvCoef              ! convection coefficient
  INTEGER            :: Index                  ! property range index
  REAL(r64)          :: InterpFrac             ! fraction

          ! Flow
  DeltaT = Abs(TempSurf1 - TempSurf2)
  Tref = 0.5d0 * (TempSurf1 + TempSurf2)
  Index = 1
  DO WHILE (Index <= NumOfPropDivisions)
    IF (Tref < Temps(Index)) EXIT ! DO loop
    Index = Index + 1
  END DO

     ! Initialize thermal properties of air
  IF (Index == 1) THEN
    VisDOfAir = Mu(Index)
    CondOfAir = Conductivity(Index)
    PrOfAir   = Pr(Index)
    DensOfAir = Density(Index)
  ELSE IF (Index > NumOfPropDivisions) THEN
    Index     = NumOfPropDivisions
    VisDOfAir = Mu(Index)
    CondOfAir = Conductivity(Index)
    PrOfAir   = Pr(Index)
    DensOfAir = Density(Index)
  ELSE
    InterpFrac = (Tref-Temps(Index-1))/(Temps(Index)-Temps(Index-1))
    VisDOfAir  = Mu(Index-1) + InterpFrac*(Mu(Index)-Mu(Index-1))
    CondOfAir  = Conductivity(Index-1) + InterpFrac*(Conductivity(Index)-Conductivity(Index-1))
    PrOfAir    = Pr(Index-1) + InterpFrac*(Pr(Index)-Pr(Index-1))
    DensOfAir  = Density(Index-1) + InterpFrac*(Density(Index)-Density(Index-1))
  END IF

    VolExpAir = 1.0d0 / (Tref + KelvinConv)

    RaNum = gravity*(DensOfAir**2)*VolExpAir*PrOfAir*DeltaT*(AirGap**3)/(VisDOfAir**2)
    RaNumCosTilt = RaNum * CosTilt
    IF (RaNum == 0.0d0 ) Then
      NuL = 0.0d0
    ELSE
      IF (RaNumCosTilt > 1708.d0) THEN
        NuL = 1.44d0 * (1.0d0 - 1708.d0*(SinTilt**1.6d0)/(RaNum*CosTilt)) * (1.d0 - 1708.0d0/RaNumCosTilt)
      ELSE
        NuL = 0.0d0
      ENDIF
    ENDIF
    IF ( RaNumCosTilt  .GT. 5830.d0 ) THEN
      NuL = NuL + (RaNumCosTilt / 5830.d0 - 1.0d0)**(1.0d0/3.0d0)
    ENDIF
    NuL = 1.0d0 + NuL
    hConvCoef = NuL * CondOfAir / AirGap

  RETURN
END FUNCTION CalcConvCoeffBetweenPlates

FUNCTION CalcConvCoeffAbsPlateAndWater(TAbsorber,TWater,Lc,TiltR2V) RESULT (hConvA2W)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Bereket Nigusse, FSEC/UCF
          !       DATE WRITTEN   February 2012
          !       MODIFIED
          !       RE-ENGINEERED

          ! PURPOSE OF THIS FUNCTION:
          !  Calculates the free converction coefficient between the absorber plate and water.
          !
          ! METHODOLOGY EMPLOYED:
          !  The convection coefficient calculation were based on the Fujii and Imura emperical correlations
          !
          ! REFERENCES:
          !  T.Fujii, and H.Imura,Natural convection heat transfer from aplate with arbitrary inclination.
          !  International Journal of Heat and Mass Transfer: 15(4), (1972), 755-764.
          !
          ! USE STATEMENTS:
  USE DataGlobals,     ONLY: DegToRadians, StefanBoltzmann, KelvinConv
  USE FluidProperties, ONLY: CheckFluidPropertyName, FindGlycol, GetSpecificHeatGlycol, &
                             GetConductivityGlycol,GetViscosityGlycol,GetDensityGlycol

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  REAL(r64), INTENT(IN)   :: TAbsorber  ! temperature of absorber plate [C]
  REAL(r64), INTENT(IN)   :: TWater     ! temperature of water [C]
  REAL(r64), INTENT(IN)   :: TiltR2V    ! collector tilt angle relative to the vertical [degree]
  REAL(r64), INTENT(IN)   :: Lc         ! characteristic length [m]

          ! FUNCTION PARAMETER DEFINITIONS:
  REAL(r64), PARAMETER  :: gravity        = 9.806d0           ! gravitational constant [m/s^2]
  CHARACTER(len=*), PARAMETER :: CalledFrom='SolarCollectors:CalcConvCoeffAbsPlateAndWater'

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  REAL(r64)          :: DensOfWater            ! density of water [kg/m3]
  REAL(r64)          :: CondOfWater            ! thermal conductivity of water [W/mK]
  REAL(r64)          :: VolExpWater            ! volumetric expansion of water, [1/K]
  REAL(r64)          :: VisOfWater             ! dynamic viscosity of water [Ns/m2]
  REAL(r64)          :: WaterSpecHeat          ! specific heat of water
  REAL(r64)          :: PrOfWater              ! Prantle number of water
!  REAL(r64)          :: RaNumCosTilt           ! Rayleigh number of air times cosine of collector tilt []
  REAL(r64)          :: CosTilt                ! cosine of collector tilt angle []
  REAL(r64)          :: DeltaT                 ! temperature difference between absober plate and water
  REAL(r64)          :: TReference             ! reference temperature for fluid properties [c]
  REAL(r64)          :: RaNum                  ! Rayleigh number
  REAL(r64)          :: GrNum                  ! Grashof number
!  REAL(r64)          :: GrcPr                  ! critical Grashof number
  REAL(r64)          :: NuL                    ! Nusselt number
  REAL(r64)          :: hConvA2W               ! convection coefficient, [W/m2K]
  INTEGER            :: WaterIndex             ! fluid type index


  ! Flow
  DeltaT = Abs(TAbsorber-TWater)
  TReference = TAbsorber-0.25d0*(TAbsorber-TWater)
  ! record fluid prop index for water
  WaterIndex=FindGlycol('WATER')
  ! find properties of water - always assume water
  WaterSpecHeat = GetSpecificHeatGlycol('WATER',MAX(TReference,0.d0), WaterIndex,CalledFrom)
  CondOfWater = GetConductivityGlycol('WATER',MAX(TReference,0.d0), WaterIndex,CalledFrom)
  VisOfWater = GetViscosityGlycol('WATER',MAX(TReference,0.d0), WaterIndex,CalledFrom)
  DensOfWater = GetDensityGlycol('WATER',MAX(TReference,0.d0), WaterIndex,CalledFrom)
  PrOfWater = VisOfWater*WaterSpecHeat/CondOfWater
  ! Requires a different reference temperature for volumetric expansion coefficient
  TReference = TWater-0.25d0*(TWater-TAbsorber)
  VolExpWater = -(GetDensityGlycol('WATER',MAX(TReference,10.d0) + 5.d0, WaterIndex,CalledFrom) - &
                  GetDensityGlycol('WATER',MAX(TReference,10.d0) - 5.d0, WaterIndex,CalledFrom)) / &
                 (10.0d0*DensOfWater)

  GrNum  = gravity*VolExpWater*DensOfWater*DensOfWater*PrOfWater*DeltaT*(Lc**3)/(VisOfWater**2)
  CosTilt = cos(TiltR2V * DegToRadians)
  IF(TAbsorber .GT. TWater)THEN
     ! hot absorber plate facing down
     IF ( abs(TiltR2V - 90.0d0) < 1.0d0) THEN
       ! It is a horizontal surface
       RaNum = GrNum * PrOfWater
       IF (RaNum .LE. 1708.0d0) THEN
         NuL = 1.0d0
       ELSE
         NuL = 0.58d0 * (RaNum)**0.20d0
       ENDIF
     ELSE
       RaNum = GrNum * PrOfWater * CosTilt
       IF (RaNum .LE. 1708.0d0) THEN
         NuL = 1.0d0
       ELSE
         NuL = 0.56d0 * (RaNum)**0.25d0
       ENDIF
     ENDIF
  ELSE
     ! cold plate facing down or hot plate facing up
     RaNum = GrNum * PrOfWater
     IF (RaNum .GT. 5.0d8) THEN
       NuL = 0.13d0 * (RaNum)**(1.d0/3.d0)
     ELSE
       NuL = 0.16d0 * (RaNum)**(1.d0/3.d0)
       IF (RaNum .LE. 1708.d0) NuL = 1.0d0
     ENDIF
  ENDIF
  hConvA2W = NuL * CondOfWater / Lc

  RETURN
END FUNCTION CalcConvCoeffAbsPlateAndWater

SUBROUTINE UpdateSolarCollector(CollectorNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Peter Graham Ellis
          !       DATE WRITTEN   January 2004
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Updates the node variables with local variables.

          ! METHODOLOGY EMPLOYED:
          ! Standard EnergyPlus methodology.

          ! USE STATEMENTS:
  USE DataLoopNode,    ONLY: Node
  USE PlantUtilities,  ONLY: SafeCopyPlantNode
  USE DataPlant,       ONLY: PlantLoop
  USE FluidProperties, ONLY: GetSpecificHeatGlycol

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS: na
  INTEGER, INTENT(IN) :: CollectorNum

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: InletNode
  INTEGER :: OutletNode
  REAL(r64) :: Cp

       ! FLOW:
  InletNode  = Collector(CollectorNum)%InletNode
  OutletNode = Collector(CollectorNum)%OutletNode

  CAll SafeCopyPlantNode(InletNode,OutletNode)
  ! Set outlet node variables that are possibly changed
  Node(OutletNode)%Temp = Collector(CollectorNum)%OutletTemp
  Cp = GetSpecificHeatGlycol(PlantLoop(Collector(CollectorNum)%WLoopNum)%FluidName,  &
                             Collector(CollectorNum)%OutletTemp, &
                             PlantLoop(Collector(CollectorNum)%WLoopNum)%FluidIndex,&
                            'UpdateSolarCollector')
  Node(OutletNode)%Enthalpy = Cp * Node(OutletNode)%Temp

  RETURN

END SUBROUTINE UpdateSolarCollector


SUBROUTINE ReportSolarCollector(CollectorNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Peter Graham Ellis
          !       DATE WRITTEN   January 2004
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Calculates report variables.

          ! METHODOLOGY EMPLOYED:
          ! Standard EnergyPlus methodology.

          ! USE STATEMENTS:
  USE DataGlobals, ONLY: SecInHour
  Use DataHVACGlobals, ONLY: TimeStepSys

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: CollectorNum

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64)           :: TimeStepInSecond

          ! FLOW:
  TimeStepInSecond = TimeStepSys * SecInHour

  Collector(CollectorNum)%Energy = Collector(CollectorNum)%Power * TimeStepInSecond
  Collector(CollectorNum)%HeatEnergy = Collector(CollectorNum)%HeatRate * TimeStepInSecond
  Collector(CollectorNum)%HeatGainEnergy = Collector(CollectorNum)%HeatGainRate * TimeStepInSecond
  Collector(CollectorNum)%HeatLossEnergy = Collector(CollectorNum)%HeatLossRate * TimeStepInSecond
  Collector(CollectorNum)%CollHeatLossEnergy = Collector(CollectorNum)%SkinHeatLossRate * TimeStepInSecond
  Collector(CollectorNum)%StoredHeatEnergy = Collector(CollectorNum)%StoredHeatRate * TimeStepInSecond

  RETURN

END SUBROUTINE ReportSolarCollector

SUBROUTINE GetExtVentedCavityIndex(SurfacePtr, VentCavIndex)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B. Nigusse, FSEC. Adopted from Photovoltaics module
          !       DATE WRITTEN   February 2012
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! object oriented "Get" routine for establishing correct integer index from outside this module

          ! METHODOLOGY EMPLOYED:
          ! mine Surface derived type for correct index/number of surface
          ! mine  ExtVentedCavity derived type that has the surface.
          ! Adapated from Photovoltaics module, originally developed by Brent G. (2004)

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor , ONLY: FindItemInList
  USE DataSurfaces   , ONLY: Surface, TotSurfaces, ExtVentedCavity, TotExtVentCav

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER,          INTENT(IN)   :: SurfacePtr
  INTEGER,          INTENT(OUT)  :: VentCavIndex

          ! SUBROUTINE PARAMETER DEFINITIONS:

          ! INTERFACE BLOCK SPECIFICATIONS:

          ! DERIVED TYPE DEFINITIONS:

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER     :: CavNum ! temporary
  INTEGER     :: thisSurf ! temporary
  INTEGER     :: thisCav
  Logical     :: Found

  IF (SurfacePtr == 0) THEN
     ! should be trapped already
     CALL ShowFatalError('Invalid surface passed to GetExtVentedCavityIndex' )
  ENDIF

  CavNum = 0
  Found = .false.
  Do thisCav=1, TotExtVentCav
     Do thisSurf =1, ExtVentedCavity(thisCav)%NumSurfs
        IF (SurfacePtr == ExtVentedCavity(thisCav)%SurfPtrs(thisSurf)) then
          Found = .TRUE.
          CavNum = thisCav
        ENDIF
     ENDDO
  ENDDO

  IF (.NOT. Found) THEN
    CALL ShowFatalError('Did not find surface in Exterior Vented Cavity description in GetExtVentedCavityIndex, '// &
         'Surface name = ' //TRIM(Surface(SurfacePtr)%Name))
  ELSE

    VentCavIndex = CavNum

  ENDIF

  RETURN

END SUBROUTINE GetExtVentedCavityIndex

SUBROUTINE GetExtVentedCavityTsColl(VentModNum, TsColl)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         <author>   Adopted from Photovoltaics module
          !       DATE WRITTEN   <date_written>
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! object oriented "Get" routine for collector surface temperature.

          ! METHODOLOGY EMPLOYED:
          ! access derived type

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:

  USE DataSurfaces ,        ONLY: ExtVentedCavity
  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER,         INTENT(IN)  :: VentModNum
  REAL(r64),       INTENT(OUT) :: TsColl

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  TsColl = ExtVentedCavity(VentModNum)%Tbaffle

  RETURN

END SUBROUTINE GetExtVentedCavityTsColl

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

END MODULE SolarCollectors
