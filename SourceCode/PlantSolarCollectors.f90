MODULE SolarCollectors

          ! MODULE INFORMATION:
          !       AUTHOR         Peter Graham Ellis
          !       DATE WRITTEN   December 2003
          !       MODIFIED       na
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
USE DataInterfaces, ONLY: SetupOutputVariable, ShowWarningError, ShowFatalError, ShowSevereError, ShowContinueError
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

          ! DERIVED TYPE DEFINITIONS:
TYPE ParametersData
  CHARACTER(len=MaxNameLength) :: Name = ''              ! Name of solar collector parameters
  REAL(r64)                    :: Area = 0.0             ! Gross area of collector (m2)
  INTEGER                      :: TestFluid = WATER      ! Test fluid (only WATER for now)
  REAL(r64)                    :: TestMassFlowRate = 0.0 ! Test volumetric flow rate (m3/s)
  INTEGER                      :: TestType = INLET       ! Test correlation type (INLET | AVERAGE | OUTLET)
  REAL(r64)                    :: eff0                   ! Coefficient 1 of efficiency equation (Y-intercept)
  REAL(r64)                    :: eff1                   ! Coefficient 2 of efficiency equation (1st order)
  REAL(r64)                    :: eff2                   ! Coefficient 3 of efficiency equation (2nd order)
  REAL(r64)                    :: iam1                   ! Coefficient 2 of incident angle modifier (1st order)
  REAL(r64)                    :: iam2                   ! Coefficient 3 of incident angle modifier (2nd order)
END TYPE ParametersData

TYPE , PUBLIC :: CollectorData
  CHARACTER(len=MaxNameLength) :: Name = ''                   ! Name of solar collector
  INTEGER                      :: TypeNum                     ! Plant Side Connection: 'TypeOf_Num' assigned in DataPlant !DSU
  INTEGER                      :: WLoopNum     = 0            ! Water plant loop index number                      !DSU
  INTEGER                      :: WLoopSideNum = 0            ! Water plant loop side index                        !DSU
  INTEGER                      :: WLoopBranchNum   = 0        ! Water plant loop branch index                      !DSU
  INTEGER                      :: WLoopCompNum     = 0        ! Water plant loop component index                   !DSU
  LOGICAL                      :: Init = .TRUE.               ! Flag for initialization:  TRUE means do the init
  LOGICAL                      :: InitSizing = .TRUE.         ! Flag for initialization of plant sizing
  INTEGER                      :: Parameters = 0              ! Parameters object number
  INTEGER                      :: Surface = 0                 ! Surface object number
  INTEGER                      :: InletNode = 0               ! Inlet node
  REAL(r64)                    :: InletTemp = 0.0             ! Inlet temperature from plant (C)
  INTEGER                      :: OutletNode = 0              ! Outlet node
  REAL(r64)                    :: OutletTemp = 0.0            ! Outlet temperature or stagnation temperature in the collector (C)
  REAL(r64)                    :: MassFlowRate = 0.0          ! Mass flow rate through the collector (kg/s)
  REAL(r64)                    :: MassFlowRateMax = 0.0       ! Maximum mass flow rate through the collector (kg/s)
  REAL(r64)                    :: VolFlowRateMax = 0.0        ! Maximum volumetric flow rate through the collector (m3/s)

  ! Report variables
  REAL(r64)                    :: IncidentAngleModifier = 0.0 ! Net incident angle modifier
  REAL(r64)                    :: Efficiency            = 0.0 ! Thermal efficiency of solar energy conversion
  REAL(r64)                    :: Power                 = 0.0 ! Heat gain or loss to collector fluid (W)
  REAL(r64)                    :: HeatGain              = 0.0 ! Heat gain to collector fluid (W)
  REAL(r64)                    :: HeatLoss              = 0.0 ! Heat loss from collector fluid (W)
  REAL(r64)                    :: Energy                = 0.0 ! Energy gained (or lost) to collector fluid (J)
END TYPE CollectorData

          ! MODULE VARIABLE TYPE DECLARATIONS:
TYPE (ParametersData), ALLOCATABLE, DIMENSION(:) :: Parameters
TYPE (CollectorData), ALLOCATABLE, DIMENSION(:), PUBLIC :: Collector

LOGICAL, ALLOCATABLE, DIMENSION(:) :: CheckEquipName

          ! MODULE VARIABLE DECLARATIONS:
INTEGER :: NumOfParameters=0
INTEGER, PUBLIC :: NumOfCollectors=0


          ! SUBROUTINE SPECIFICATIONS:
PUBLIC SimSolarCollector
PRIVATE GetSolarCollectorInput
PRIVATE InitSolarCollector
PRIVATE CalcSolarCollector
PRIVATE IAM
PRIVATE UpdateSolarCollector
PRIVATE ReportSolarCollector

CONTAINS

          ! MODULE SUBROUTINES:

SUBROUTINE SimSolarCollector( EquipTypeNum, CompName, CompIndex, InitLoopEquip, FirstHVACIteration)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Peter Graham Ellis
          !       DATE WRITTEN   December 2003
          !       MODIFIED       Brent Griffith, March 2010
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Simulates solar collector objects.

          ! METHODOLOGY EMPLOYED:
          ! Standard EnergyPlus methodology.

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: FindItemInList
  USE General,        ONLY: TrimSigDigits

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

  CALL InitSolarCollector(CollectorNum, FirstHVACIteration)

  CALL CalcSolarCollector(CollectorNum)

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
  USE InputProcessor, ONLY: GetNumObjectsFound, FindItemInList, GetObjectItem, VerifyName
  USE DataIPShortCuts  ! Data for field names, blank numerics
  USE NodeInputManager, ONLY: GetOnlySingleNode
  USE BranchNodeConnections, ONLY: TestCompSet
  USE Psychrometrics, ONLY: RhoH2O
  USE DataLoopNode
  USE DataPlant   !DSU
  USE General, ONLY: RoundSigDigits

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

          ! FLOW:
  CurrentModuleParamObject = 'SolarCollectorPerformance:FlatPlate'
  NumOfParameters = GetNumObjectsFound(TRIM(CurrentModuleParamObject))
  CurrentModuleObject = 'SolarCollector:FlatPlate:Water'
  NumOfCollectors = GetNumObjectsFound(TRIM(CurrentModuleObject))

  IF (NumOfParameters > 0) THEN
    ALLOCATE(Parameters(NumOfParameters))

    DO ParametersNum = 1, NumOfParameters

      CALL GetObjectItem(TRIM(CurrentModuleParamObject), &
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

      IF (rNumericArgs(2) > 0.0) THEN
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
        Parameters(ParametersNum)%eff2 = 0.0
      END IF

      ! Incident angle modifier coefficients
      IF (NumNumbers > 5) THEN
        Parameters(ParametersNum)%iam1 = rNumericArgs(6)
      ELSE
        Parameters(ParametersNum)%iam1 = 0.0
      END IF

      IF (NumNumbers > 6) THEN
        Parameters(ParametersNum)%iam2 = rNumericArgs(7)
      ELSE
        Parameters(ParametersNum)%iam2 = 0.0
      END IF
    END DO ! ParametersNum

    IF (ErrorsFound) CALL ShowFatalError('Errors in '//TRIM(CurrentModuleParamObject)//' input.')
  END IF

  IF (NumOfCollectors > 0) THEN
    ALLOCATE(Collector(NumOfCollectors))

    DO CollectorNum = 1, NumOfCollectors

      CALL GetObjectItem(TRIM(CurrentModuleObject),CollectorNum,cAlphaArgs,NumAlphas,rNumericArgs,NumNumbers,IOStatus)

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
        DO CollectorNum2 = 1, NumOfCollectors
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
      IF (SurfNum > 0 .AND. ParametersNum > 0 .AND. Parameters(ParametersNum)%Area > 0.0 &
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
        Collector(CollectorNum)%VolFlowRateMax = 0.0  ! Max vol flow rate is not specified; no flow for plant sizing calculation
        Collector(CollectorNum)%MassFlowRateMax = 999999.9  ! But...set a very high value so that it demands as much as possible
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
                               'System','Average',Collector(CollectorNum)%Name)

      CALL SetupOutputVariable('Solar Collector Heat Transfer Energy [J]', Collector(CollectorNum)%Energy, &
                               'System','Sum',Collector(CollectorNum)%Name, &
                               ResourceTypeKey='SolarWater',EndUseKey='HeatProduced',GroupKey='Plant')

      CALL TestCompSet(TRIM(CurrentModuleObject),cAlphaArgs(1),cAlphaArgs(4),cAlphaArgs(5),'Water Nodes')

    END DO ! CollectorNum

    IF (ErrorsFound) CALL ShowFatalError('Errors in '//TRIM(CurrentModuleObject)//' input.')

    IF (NumOfCollectors > 0) THEN
      ALLOCATE(CheckEquipName(NumOfCollectors))
      CheckEquipName = .TRUE.
    ENDIF

  END IF

  RETURN

END SUBROUTINE GetSolarCollectorInput


SUBROUTINE InitSolarCollector(CollectorNum, FirstHVACIteration)

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
  USE DataGlobals, ONLY: SysSizingCalc, InitConvTemp
  USE DataLoopNode, ONLY: Node
  USE DataPlant
  USE FluidProperties, ONLY: GetDensityGlycol
  USE PlantUtilities,  ONLY: InitComponentNodes, SetComponentFlowRate, RegisterPlantCompDesignFlow

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: CollectorNum
  LOGICAL, INTENT(IN) :: FirstHVACIteration

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: InletNode
  INTEGER :: OutletNode

  REAL(r64),PARAMETER   ::   BigNumber=9999.9     !Component desired mass flow rate

  LOGICAL, SAVE                            :: MyOneTimeFlag = .TRUE. ! one time flag
  LOGICAL, ALLOCATABLE, SAVE, DIMENSION(:) :: SetLoopIndexFlag       ! get loop number flag
  REAL(r64)   :: rho
  LOGICAL     :: errFlag

          ! FLOW:

  ! Do the one time initializations
  IF (MyOneTimeFlag) THEN
    ALLOCATE(SetLoopIndexFlag(NumOfCollectors))
    SetLoopIndexFlag = .TRUE.
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
  END IF

  IF (.NOT. BeginEnvrnFlag) Collector(CollectorNum)%Init = .TRUE.

  Collector(CollectorNum)%InletTemp = Node(InletNode)%Temp

  IF (FirstHVACIteration) THEN
    Collector(CollectorNum)%MassFlowRate = Collector(CollectorNum)%MassFlowRateMax
  ENDIF
  ! Request the mass flow rate from the plant component flow utility routine
  CALL SetComponentFlowRate(Collector(CollectorNum)%MassFlowRate,InletNode,OutletNode, &
            Collector(CollectorNum)%WLoopNum,Collector(CollectorNum)%WLoopSideNum,     &
            Collector(CollectorNum)%WLoopBranchNum, Collector(CollectorNum)%WLoopCompNum)

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
  USE DataPlant,       ONLY: PlantLoop

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
  INTEGER :: Iteration          ! Counter of iterations until convergence

          ! FLOW:
  SurfNum = Collector(CollectorNum)%Surface
  ParamNum = Collector(CollectorNum)%Parameters

  ! Calculate incident angle modifier
  IF (QRadSWOutIncident(SurfNum) > 0.0) THEN
    ThetaBeam = ACOS(CosIncidenceAngle(SurfNum))

    ! Calculate equivalent incident angles for sky and ground radiation according to Brandemuehl and Beckman (1980)
    Tilt = Surface(SurfNum)%Tilt
    ThetaSky = (59.68 - 0.1388 * Tilt + 0.001497 * Tilt**2.0) * DegToRadians
    ThetaGnd = (90.0 - 0.5788 * Tilt + 0.002693 * Tilt**2.0) * DegToRadians

    IncidentAngleModifier = (QRadSWOutIncidentBeam(SurfNum) * IAM(ParamNum, ThetaBeam) &
      + QRadSWOutIncidentSkyDiffuse(SurfNum) * IAM(ParamNum, ThetaSky) &
      + QRadSWOutIncidentGndDiffuse(SurfNum) * IAM(ParamNum, ThetaGnd)) &
      / QRadSWOutIncident(SurfNum)
  ELSE
    IncidentAngleModifier = 0.0
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
  OutletTemp = 0.0
  OutletTempPrev = 999.9 ! Set to a ridiculous number so that DO loop runs at least once
  Q = 0.0

  DO WHILE (ABS(OutletTemp - OutletTempPrev) > TempConvergTol) ! Check for temperature convergence

    OutletTempPrev = OutletTemp ! Save previous outlet temperature

    ! Modify coefficients depending on test correlation type
    SELECT CASE (Parameters(ParamNum)%TestType)
      CASE (INLET)
        FRULpTest = Parameters(ParamNum)%eff1 + Parameters(ParamNum)%eff2 * (InletTemp - Surface(SurfNum)%OutDryBulbTemp)
        TestTypeMod = 1.0

      CASE (AVERAGE)
        FRULpTest = Parameters(ParamNum)%eff1 + Parameters(ParamNum)%eff2 *   &
                                   ((InletTemp + OutletTemp)*0.5 - Surface(SurfNum)%OutDryBulbTemp)
        TestTypeMod = 1.0/(1.0 - FRULpTest/(2.0 * mCpATest) )

      CASE (OUTLET)
        FRULpTest = Parameters(ParamNum)%eff1 + Parameters(ParamNum)%eff2 *   &
                                   (OutletTemp - Surface(SurfNum)%OutDryBulbTemp)
        TestTypeMod = 1.0/(1.0 - FRULpTest/mCpATest )
    END SELECT

    FRTAN = Parameters(ParamNum)%eff0 * TestTypeMod
    FRUL = Parameters(ParamNum)%eff1 * TestTypeMod
    FRULT = Parameters(ParamNum)%eff2 * TestTypeMod
    FRULpTest = FRULpTest * TestTypeMod

    IF (MassFlowRate > 0.0) THEN ! Calculate efficiency and heat transfer with flow

      IF ((1.0 + FRULpTest / mCpATest) > 0.0) THEN
        FpULTest = -mCpATest * LOG(1.0 + FRULpTest / mCpATest)
      ELSE
        FpULTest = FRULpTest ! Avoid LOG( <0 )
      END IF

      IF ((-FpULTest / mCpA) < 700.0D0) THEN
        FlowMod = mCpA * (1.0 - EXP(-FpULTest / mCpA))
      ELSE ! avoid EXP(too large #)
        FlowMod = FlowMod
      ENDIF
      IF ((-FpULTest / mCpATest) < 700.0D0) THEN
        FlowMod = FlowMod / (mCpATest * (1.0 - EXP(-FpULTest / mCpATest)))
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
         OutletTemp = -100.0
         Q = MassFlowRate * Cp * (OutletTemp - InletTemp)
      ENDIF
      IF (OutletTemp > 200) THEN
         OutletTemp = 200.0
         Q = MassFlowRate * Cp * (OutletTemp - InletTemp)
      ENDIF

      IF (QRadSWOutIncident(SurfNum) > 0.0) THEN ! Calculate thermal efficiency
        ! NOTE: Efficiency can be > 1 if Q > QRadSWOutIncident because of favorable delta T, i.e. warm outdoor temperature
        Efficiency = Q / (QRadSWOutIncident(SurfNum) * Area) ! Q has units of W; QRadSWOutIncident has units of W/m2

      ELSE
        Efficiency = 0.0
      END IF

    ELSE ! Calculate stagnation temperature of fluid in collector (no flow)
      Q = 0.0
      Efficiency = 0.0

      ! Calculate temperature of stagnant fluid in collector
      IF (FRULT == 0.0) THEN ! Linear, 1st order solution
        OutletTemp = Surface(SurfNum)%OutDryBulbTemp - FRTAN * IncidentAngleModifier * QRadSWOutIncident(SurfNum) / FRUL

      ELSE ! Quadratic, 2nd order solution
        A = -FRULT
        B = -FRUL + 2.0 * FRULT * Surface(SurfNum)%OutDryBulbTemp
        C = -FRULT * Surface(SurfNum)%OutDryBulbTemp**2.0 + FRUL * Surface(SurfNum)%OutDryBulbTemp &
          - FRTAN * IncidentAngleModifier * QRadSWOutIncident(SurfNum)
        OutletTemp = (-B + (B**2.0 - 4.0 * A * C)**0.5) / (2.0 * A)

      END IF
    END IF

    IF (Parameters(ParamNum)%TestType == INLET) EXIT ! Inlet temperature test correlations do not need to iterate

    IF (Iteration > 100) THEN
      CALL ShowWarningError('SolarCollector:FlatPlate:Water = '//TRIM(Collector(CollectorNum)%Name)// &
        ':  Solution did not converge.')
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

    IF (IAM > 10.0) THEN  ! Greater than 10 is probably not a possibility
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

          ! FLOW:
  Collector(CollectorNum)%Energy = Collector(CollectorNum)%Power * TimeStepSys * SecInHour

  RETURN

END SUBROUTINE ReportSolarCollector

!     NOTICE
!
!     Copyright © 1996-2011 The Board of Trustees of the University of Illinois
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
