MODULE Boilers

  ! Module containing the routines dealing with the Boilers

  ! MODULE INFORMATION:
  !       AUTHOR         Dan Fisher, Taecheol Kim
  !       DATE WRITTEN   1998, 2000
  !       MODIFIED       na
  !       RE-ENGINEERED  na

  ! PURPOSE OF THIS MODULE:
  ! Perform boiler simulation for plant simulation

  ! METHODOLOGY EMPLOYED:
  ! The BLAST/DOE-2 empirical model based on mfg. data

  ! REFERENCES: none

  ! OTHER NOTES: none

  ! USE STATEMENTS:
  ! Use statements for data only modules
USE DataLoopNode
USE DataHVACGlobals
USE DataPrecisionGlobals
USE DataInterfaces
USE DataGlobals, ONLY: MaxNameLength, InitConvTemp, SecInHour
USE DataPlant, ONLY: PlantLoop, TypeOf_Boiler_Simple, ScanPlantLoopsForObject
USE DataBranchAirLoopPlant, ONLY: ControlType_SeriesActive
USE General, ONLY: TrimSigDigits

!USE FunctionFluidProperties
  ! Use statements for access to subroutines in other modules

IMPLICIT NONE

PRIVATE

  ! MODULE PARAMETER DEFINITIONS

! Boiler normalized efficiency curve types
INTEGER, PARAMETER :: Linear          = 1
INTEGER, PARAMETER :: Bilinear        = 2
INTEGER, PARAMETER :: Quadratic       = 3
INTEGER, PARAMETER :: Biquadratic     = 4
INTEGER, PARAMETER :: Cubic           = 5
INTEGER, PARAMETER :: QuadraticLinear = 6
INTEGER, PARAMETER :: Bicubic         = 7
INTEGER, PARAMETER :: TriQuadratic    = 8

! water temperature evaluation method
INTEGER, PARAMETER :: BoilerTempModeNotSet = 100
INTEGER, PARAMETER :: EnteringBoilerTemp   = 101
INTEGER, PARAMETER :: LeavingBoilerTemp    = 102

!Boiler flow modes
INTEGER, PARAMETER :: FlowModeNotSet           = 200
INTEGER, PARAMETER :: ConstantFlow             = 201
INTEGER, PARAMETER :: NotModulated             = 202
INTEGER, PARAMETER :: LeavingSetpointModulated = 203


  ! DERIVED TYPE DEFINITIONS
TYPE BoilerSpecs
  CHARACTER(len=MaxNameLength) :: Name   =' '      ! user identifier
  INTEGER        :: FuelType             = 0       ! resource type assignment
  INTEGER        :: TypeNum              = 0       ! plant loop type identifier
  INTEGER        :: LoopNum              = 0       ! plant loop connection
  INTEGER        :: LoopSideNum          = 0       ! plant loop side connection
  INTEGER        :: BranchNum            = 0       ! plant loop branch connection
  INTEGER        :: CompNum              = 0        ! plant loop component connection
  LOGICAL        :: Available            = .FALSE. ! TRUE if machine available in current time step
  LOGICAL        :: ON                   = .FALSE. ! TRUE: simulate the machine at it's operating part load ratio
  REAL(r64)      :: NomCap               = 0.0d0   ! W - design nominal capacity of Boiler
  REAL(r64)      :: Effic                = 0.0d0   ! boiler efficiency at design conditions
  REAL(r64)      :: TempDesBoilerOut     = 0.0d0   ! C - Boiler design outlet temperature
  INTEGER        :: FlowMode             = FlowModeNotSet ! one of 3 modes for componet flow during operation
  LOGICAL        :: ModulatedFlowSetToLoop= .FALSE. ! True if the setpoint is missing at the outlet node
  LOGICAL        :: ModulatedFlowErrDone  = .FALSE.  ! true if setpoint warning issued
  REAL(r64)      :: VolFlowRate          = 0.0d0   ! m3/s - Boiler water design volumetric flow rate
  REAL(r64)      :: DesMassFlowRate      = 0.0d0   ! kg/s - Boiler water design mass flow rate
  REAL(r64)      :: MassFlowRate         = 0.0d0   ! kg/s - Boiler water mass flow rate
  REAL(r64)      :: SizFac               = 0.0d0     ! sizing factor
  INTEGER        :: BoilerInletNodeNum   = 0       ! Node number at the boiler inlet
  INTEGER        :: BoilerOutletNodeNum  = 0       ! Node number at the boiler outlet
  REAL(r64)      :: MinPartLoadRat       = 0.0d0   ! Minimum allowed operating part load ratio
  REAL(r64)      :: MaxPartLoadRat       = 0.0d0   ! Maximum allowed operating part load ratio
  REAL(r64)      :: OptPartLoadRat       = 0.0d0   ! Optimal operating part load ratio
  REAL(r64)      :: OperPartLoadRat      = 0.0d0   ! Actual operating part load ratio
  INTEGER        :: CurveTempMode        = BoilerTempModeNotSet ! water temp to use in curve, switch between entering and leaving
  INTEGER        :: EfficiencyCurvePtr   = 0       ! Index to efficiency curve
  INTEGER        :: EfficiencyCurveType  = 0       ! Type of efficiency curve
  REAL(r64)      :: TempUpLimitBoilerOut = 0.0d0   ! C - Boiler outlet maximum temperature limit
  REAL(r64)      :: ParasiticElecLoad    = 0.0d0   ! W - Parasitic electric power (e.g. forced draft fan)
  INTEGER        :: EffCurveOutputError  = 0       ! efficiency curve output <=0 recurring warning error counter
  INTEGER        :: EffCurveOutputIndex  = 0       ! efficiency curve output <=0 recurring warning error message index
  INTEGER        :: CalculatedEffError   = 0       ! calculated efficiency >1.1 recurring warning error counter
  INTEGER        :: CalculatedEffIndex   = 0       ! calculated efficiency >1.1 recurring warning error message index
END TYPE BoilerSpecs

TYPE ReportVars
  REAL(r64)      :: BoilerLoad               = 0.0d0 ! W - Boiler operating load
  REAL(r64)      :: BoilerEnergy             = 0.0d0 ! J - Boiler energy integrated over time
  REAL(r64)      :: FuelUsed                 = 0.0d0 ! W - Boiler fuel used
  REAL(r64)      :: FuelConsumed             = 0.0d0 ! J - Boiler Fuel consumed integrated over time
  REAL(r64)      :: BoilerInletTemp          = 0.0d0 ! C - Boiler inlet temperature
  REAL(r64)      :: BoilerOutletTemp         = 0.0d0 ! C - Boiler outlet temperature
  REAL(r64)      :: Mdot                     = 0.0d0 ! kg/s - Boiler mass flow rate
  REAL(r64)      :: ParasiticElecPower       = 0.0d0 ! W - Parasitic Electrical Power (e.g. forced draft fan)
  REAL(r64)      :: ParasiticElecConsumption = 0.0d0 ! J - Parasitic Electrical Consumption (e.g. forced draft fan)
  REAL(r64)      :: BoilerPLR                = 0.0d0 !     Boiler operating part-load ratio
END TYPE ReportVars


  ! MODULE VARIABLE DECLARATIONS:
INTEGER          :: NumBoilers               = 0     !     Number of boilers
REAL(r64)        :: FuelUsed                 = 0.0d0 ! W - Boiler fuel used
REAL(r64)        :: ParasiticElecPower       = 0.0d0 ! W - Parasitic electrical power (e.g. forced draft fan)
REAL(r64)        :: BoilerLoad               = 0.0d0 ! W - Boiler Load
REAL(r64)        :: BoilerMassFlowRate       = 0.0d0 ! kg/s - Boiler mass flow rate
REAL(r64)        :: BoilerOutletTemp         = 0.0d0 ! W - Boiler outlet temperature
REAL(r64)        :: BoilerPLR                = 0.0d0 !     Boiler operating part-load ratio

TYPE (BoilerSpecs), ALLOCATABLE, DIMENSION(:)  :: Boiler         ! boiler data - dimension to number of machines
TYPE (ReportVars),  ALLOCATABLE, DIMENSION(:)  :: BoilerReport   ! report vars - dimension to number of machines
LOGICAL, ALLOCATABLE, DIMENSION(:) :: CheckEquipName


  ! SUBROUTINE SPECIFICATIONS FOR MODULE Boilers
PUBLIC  SimBoiler
PRIVATE CalcBoilerModel
PRIVATE GetBoilerInput
PRIVATE InitBoiler
PRIVATE SizeBoiler
PRIVATE UpdateBoilerRecords


CONTAINS
          ! MODULE SUBROUTINES:

! Beginning of Boiler Module Driver Subroutines
!*************************************************************************

SUBROUTINE SimBoiler(BoilerType,BoilerName,EquipFlowCtrl, CompIndex,RunFlag, &
                     InitLoopEquip,MyLoad,MaxCap,MinCap,OptCap,GetSizingFactor,SizingFactor)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         DAN FISHER
          !       DATE WRITTEN   Sept. 1998
          !       MODIFIED       Taecheol Kim, May 2000
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subrountine controls the boiler component simulation

          ! METHODOLOGY EMPLOYED: na

          ! REFERENCES: na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: FindItemInList

  IMPLICIT NONE


          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: BoilerType      ! boiler type (used in CASE statement)
  CHARACTER(len=*), INTENT(IN) :: BoilerName      ! boiler identifier
  INTEGER, INTENT(IN)          :: EquipFlowCtrl   ! Flow control mode for the equipment
  INTEGER, INTENT(INOUT)       :: CompIndex       ! boiler counter/identifier
  LOGICAL, INTENT(IN)          :: RunFlag         ! if TRUE run boiler simulation--boiler is ON
  LOGICAL, INTENT(INOUT)       :: InitLoopEquip   ! If not zero, calculate the max load for operating conditions
  REAL(r64), INTENT(INOUT)     :: MyLoad          ! W - Actual demand boiler must satisfy--calculated by load dist. routine
  REAL(r64), INTENT(OUT)       :: MinCap          ! W - minimum boiler operating capacity
  REAL(r64), INTENT(OUT)       :: MaxCap          ! W - maximum boiler operating capacity
  REAL(r64), INTENT(OUT)       :: OptCap          ! W - optimal boiler operating capacity
  LOGICAL, INTENT(IN)          :: GetSizingFactor ! TRUE when just the sizing factor is requested
  REAL(r64), INTENT(OUT)       :: SizingFactor    ! sizing factor
          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  LOGICAL, SAVE :: GetInput = .TRUE.              ! if TRUE read user input
  INTEGER                      :: BoilerNum       ! boiler counter/identifier

          !FLOW

          !Get Input
  IF (GetInput) THEN
    CALL GetBoilerInput
    GetInput=.FALSE.
  END IF

  ! Find the correct Equipment
  IF (CompIndex == 0) THEN
    BoilerNum = FindItemInList(BoilerName,Boiler%Name,NumBoilers)
    IF (BoilerNum == 0) THEN
      CALL ShowFatalError('SimBoiler: Unit not found='//TRIM(BoilerName))
    ENDIF
    CompIndex=BoilerNum
  ELSE
    BoilerNum=CompIndex
    IF (BoilerNum > NumBoilers .or. BoilerNum < 1) THEN
      CALL ShowFatalError('SimBoiler:  Invalid CompIndex passed='//  &
                          TRIM(TrimSigDigits(BoilerNum))// &
                          ', Number of Units='//TRIM(TrimSigDigits(NumBoilers))//  &
                          ', Entered Unit name='//TRIM(BoilerName))
    ENDIF
    IF (CheckEquipName(BoilerNum)) THEN
      IF (BoilerName /= Boiler(BoilerNum)%Name) THEN
        CALL ShowFatalError('SimBoiler: Invalid CompIndex passed='//  &
                            TRIM(TrimSigDigits(BoilerNum))// &
                            ', Unit name='//TRIM(BoilerName)//', stored Unit Name for that index='//  &
                            TRIM(Boiler(BoilerNum)%Name))
      ENDIF
      CheckEquipName(BoilerNum)=.false.
    ENDIF
  ENDIF

   ! Initialize Loop Equipment
  IF (InitLoopEquip) THEN
    CALL InitBoiler(BoilerNum)
    CALL SizeBoiler(BoilerNum)
    MinCap = Boiler(BoilerNum)%NomCap * Boiler(BoilerNum)%MinPartLoadRat
    MaxCap = Boiler(BoilerNum)%NomCap * Boiler(BoilerNum)%MaxPartLoadRat
    OptCap = Boiler(BoilerNum)%NomCap * Boiler(BoilerNum)%OptPartLoadRat
    IF (GetSizingFactor) THEN
      SizingFactor = Boiler(BoilerNum)%SizFac
    END IF
    RETURN
  END IF
          !Calculate Load

          !Select boiler type and call boiler model
  CALL InitBoiler(BoilerNum)
  CALL CalcBoilerModel(BoilerNum,MyLoad,Runflag,EquipFlowCtrl)
  CALL UpdateBoilerRecords(MyLoad,RunFlag,BoilerNum)

RETURN
END SUBROUTINE SimBoiler

SUBROUTINE GetBoilerInput
            ! SUBROUTINE INFORMATION:
            !       AUTHOR:          Dan Fisher
            !       DATE WRITTEN:    April 1998
            !       MODIFIED:        R. Raustad - FSEC, June 2008: added boiler efficiency curve object
            !       RE-ENGINEERED:   na

            ! PURPOSE OF THIS SUBROUTINE:
            ! get all boiler data from input file

            ! METHODOLOGY EMPLOYED:
            ! standard EnergyPlus input retrieval using input Processor

            ! REFERENCES: na

            ! USE STATEMENTS:
    USE DataGlobals,           ONLY: MaxNameLength, AnyEnergyManagementSystemInModel
    USE DataGlobalConstants
    USE InputProcessor,        ONLY: GetNumObjectsFound, GetObjectItem, VerifyName, SameString
    USE DataIPShortCuts  ! Data for field names, blank numerics
    USE BranchNodeConnections, ONLY: TestCompSet
    USE NodeInputManager,      ONLY: GetOnlySingleNode
    USE GlobalNames,           ONLY: VerifyUniqueBoilerName
    USE CurveManager,          ONLY: GetCurveIndex, GetCurveType
    USE General, ONLY: RoundSigDigits

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

            ! PARAMETERS
    CHARACTER(len=*), PARAMETER :: RoutineName='GetBoilerInput: '

            !LOCAL VARIABLES
    INTEGER       :: BoilerNum                             ! boiler identifier
    INTEGER       :: NumAlphas                             ! Number of elements in the alpha array
    INTEGER       :: NumNums                               ! Number of elements in the numeric array
    INTEGER       :: IOStat                                ! IO Status when calling get input subroutine
    LOGICAL, SAVE :: ErrorsFound = .FALSE.                 ! Flag to show errors were found during GetInput
    LOGICAL       :: IsNotOK                               ! Flag to verify name
    LOGICAL       :: IsBlank                               ! Flag for blank name
    LOGICAL       :: errflag                               ! Flag to show errors were found during function call
    CHARACTER(len=MaxNameLength),ALLOCATABLE, DIMENSION(:) :: BoilerFuelTypeForOutputVariable ! used to set up report variables

            !GET NUMBER OF ALL EQUIPMENT
    cCurrentModuleObject = 'Boiler:HotWater'
    NumBoilers = GetNumObjectsFound(cCurrentModuleObject)


    IF (NumBoilers<=0) THEN
      CALL ShowSevereError('No '//TRIM(cCurrentModuleObject)//' Equipment specified in input file')
      ErrorsFound=.TRUE.
    END IF

            !See if load distribution manager has already gotten the input
    IF (ALLOCATED(Boiler))RETURN


    ALLOCATE (Boiler(NumBoilers))
    ALLOCATE (BoilerReport(NumBoilers))
    ALLOCATE (CheckEquipName(NumBoilers))
    ALLOCATE (BoilerFuelTypeForOutputVariable(NumBoilers))
    CheckEquipName=.true.
    BoilerFuelTypeForOutputVariable=' '

             !LOAD ARRAYS WITH CURVE FIT Boiler DATA

    DO BoilerNum = 1 , NumBoilers
      CALL GetObjectItem(cCurrentModuleObject, BoilerNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOSTAT, &
                          NumBlank=lNumericFieldBlanks,AlphaBlank=lAlphaFieldBlanks, &
                          AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

      IsNotOK = .FALSE.
      IsBlank = .FALSE.
      CALL VerifyName(cAlphaArgs(1),Boiler%Name,BoilerNum-1,IsNotOK,IsBlank,TRIM(cCurrentModuleObject)//' Name')
      IF (IsNotOK) THEN
        ErrorsFound=.TRUE.
        IF (IsBlank) cAlphaArgs(1)='xxxxx'
      ENDIF
      CALL VerifyUniqueBoilerName(TRIM(cCurrentModuleObject),cAlphaArgs(1),errflag,TRIM(cCurrentModuleObject)//' Name')
      IF (errflag) THEN
        ErrorsFound=.TRUE.
      ENDIF
      Boiler(BoilerNum)%Name = cAlphaArgs(1)
      Boiler(BoilerNum)%TypeNum = TypeOf_Boiler_Simple

      SELECT CASE (cAlphaArgs(2))

      CASE ('ELECTRICITY','ELECTRIC','ELEC')
        BoilerFuelTypeForOutputVariable(BoilerNum) = 'Electric'
        Boiler(BoilerNum)%FuelType=AssignResourceTypeNum('ELECTRICITY')

      CASE ('GAS','NATURALGAS','NATURAL GAS')
        BoilerFuelTypeForOutputVariable(BoilerNum) = 'Gas'
        Boiler(BoilerNum)%FuelType=AssignResourceTypeNum('NATURALGAS')

      CASE ('DIESEL')
        BoilerFuelTypeForOutputVariable(BoilerNum) = 'Diesel'
        Boiler(BoilerNum)%FuelType=AssignResourceTypeNum('DIESEL')

      CASE ('GASOLINE')
        BoilerFuelTypeForOutputVariable(BoilerNum) = 'Gasoline'
        Boiler(BoilerNum)%FuelType=AssignResourceTypeNum('GASOLINE')

      CASE ('COAL')
        BoilerFuelTypeForOutputVariable(BoilerNum) = 'Coal'
        Boiler(BoilerNum)%FuelType=AssignResourceTypeNum('COAL')

      CASE ('FUEL OIL #1','FUELOIL#1','FUEL OIL','DISTILLATE OIL')
        BoilerFuelTypeForOutputVariable(BoilerNum) = 'FuelOil#1'
        Boiler(BoilerNum)%FuelType=AssignResourceTypeNum('DISTILLATE OIL')

      CASE ('FUEL OIL #2','FUELOIL#2','RESIDUAL OIL')
        BoilerFuelTypeForOutputVariable(BoilerNum) = 'FuelOil#2'
        Boiler(BoilerNum)%FuelType=AssignResourceTypeNum('RESIDUAL OIL')

      CASE ('PROPANE','LPG','PROPANEGAS','PROPANE GAS')
        BoilerFuelTypeForOutputVariable(BoilerNum) = 'Propane'
        Boiler(BoilerNum)%FuelType=AssignResourceTypeNum('PROPANE')

      CASE ('OTHERFUEL1')
        BoilerFuelTypeForOutputVariable(BoilerNum) = 'OtherFuel1'
        Boiler(BoilerNum)%FuelType=AssignResourceTypeNum('OTHERFUEL1')

      CASE ('OTHERFUEL2')
        BoilerFuelTypeForOutputVariable(BoilerNum) = 'OtherFuel2'
        Boiler(BoilerNum)%FuelType=AssignResourceTypeNum('OTHERFUEL2')

      CASE DEFAULT
        CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'",')
        CALL ShowContinueError('Invalid '//TRIM(cAlphaFieldNames(2))//'='//TRIM(cAlphaArgs(2)))
        ! Set to Electric to avoid errors when setting up output variables
        BoilerFuelTypeForOutputVariable(BoilerNum) = 'Electric'
        Boiler(BoilerNum)%FuelType=AssignResourceTypeNum('ELECTRICITY')
        ErrorsFound=.TRUE.
      END SELECT

      Boiler(BoilerNum)%NomCap              = rNumericArgs(1)
      IF (rNumericArgs(1) == 0.0d0) THEN
        CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'",')
        CALL ShowContinueError('Invalid '//TRIM(cNumericFieldNames(1))//'='//TRIM(RoundSigDigits(rNumericArgs(1),2)) )
        CALL ShowContinueError('...'//TRIM(cNumericFieldNames(1))//' must be greater than 0.0')
        ErrorsFound=.TRUE.
      ENDIF

      Boiler(BoilerNum)%Effic               = rNumericArgs(2)
      IF (rNumericArgs(2) == 0.0d0) THEN
        CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'",')
        CALL ShowContinueError('Invalid '//TRIM(cNumericFieldNames(2))//'='//TRIM(RoundSigDigits(rNumericArgs(2),3)) )
        CALL ShowSevereError('...'//TRIM(cNumericFieldNames(2))//' must be greater than 0.0')
        ErrorsFound=.TRUE.
      ENDIF

      SELECT CASE (cAlphaArgs(3))

      CASE ('ENTERINGBOILER')
        Boiler(BoilerNum)%CurveTempMode = EnteringBoilerTemp
      CASE ('LEAVINGBOILER')
        Boiler(BoilerNum)%CurveTempMode = LeavingBoilerTemp
      CASE DEFAULT
        Boiler(BoilerNum)%CurveTempMode = BoilerTempModeNotSet
      END SELECT

      Boiler(BoilerNum)%EfficiencyCurvePtr = GetCurveIndex(cAlphaArgs(4))
      IF(Boiler(BoilerNum)%EfficiencyCurvePtr .GT. 0)THEN
        SELECT CASE(GetCurveType(Boiler(BoilerNum)%EfficiencyCurvePtr))
        CASE('LINEAR')
          Boiler(BoilerNum)%EfficiencyCurveType = Linear
        CASE('QUADRATIC')
          Boiler(BoilerNum)%EfficiencyCurveType = Quadratic
        CASE('QUADRATICLINEAR')
          Boiler(BoilerNum)%EfficiencyCurveType = QuadraticLinear
        CASE('CUBIC')
          Boiler(BoilerNum)%EfficiencyCurveType = Cubic
        CASE('BICUBIC')
          Boiler(BoilerNum)%EfficiencyCurveType = Bicubic
        CASE('BIQUADRATIC')
          Boiler(BoilerNum)%EfficiencyCurveType = Biquadratic
        CASE DEFAULT
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'",')
          CALL ShowContinueError('Invalid '//TRIM(cAlphaFieldNames(4))//'='//TRIM(cAlphaArgs(4)))
          CALL ShowContinueError('...Curve type for '//TRIM(cAlphaFieldNames(4))//'  = '// &
                                 TRIM(GetCurveType(Boiler(BoilerNum)%EfficiencyCurvePtr)))
          ErrorsFound=.TRUE.
        END SELECT
      ELSE IF(.NOT. lAlphaFieldBlanks(4))THEN
        CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'",')
        CALL ShowContinueError('Invalid '//TRIM(cAlphaFieldNames(4))//'='//TRIM(cAlphaArgs(4)) )
        CALL ShowSevereError('...'//TRIM(cAlphaFieldNames(4))//' not found.')
        ErrorsFound=.TRUE.
      ENDIF

      !if curve uses temperature, make sure water temp mode has been set
      SELECT CASE (Boiler(BoilerNum)%EfficiencyCurveType)
      CASE (Biquadratic, QuadraticLinear, Bicubic) !curve uses water temperature
        IF (Boiler(BoilerNum)%CurveTempMode == BoilerTempModeNotSet) THEN ! throw error
          IF (.NOT. lAlphaFieldBlanks(3)) THEN
            CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'",')
            CALL ShowContinueError('Invalid '//TRIM(cAlphaFieldNames(3))//'='//TRIM(cAlphaArgs(3)))
            CALL ShowContinueError('Boiler using curve type of '//  &
                                     TRIM(GetCurveType(Boiler(BoilerNum)%EfficiencyCurvePtr)) // &
                                     ' must specify '//TRIM(cAlphaFieldNames(3)) )
            CALL ShowContinueError('Available choices are EnteringBoiler or LeavingBoiler')
          ELSE
            CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'",')
            CALL ShowContinueError('Field '//TRIM(cAlphaFieldNames(3))//' is blank')
            CALL ShowContinueError('Boiler using curve type of '//  &
                                     TRIM(GetCurveType(Boiler(BoilerNum)%EfficiencyCurvePtr)) // &
                                     ' must specify either EnteringBoiler or LeavingBoiler')
          ENDIF
          ErrorsFound=.TRUE.
        ENDIF
      END SELECT

      Boiler(BoilerNum)%TempDesBoilerOut     = rNumericArgs(3)
      Boiler(BoilerNum)%VolFlowRate          = rNumericArgs(4)
      Boiler(BoilerNum)%MinPartLoadRat       = rNumericArgs(5)
      Boiler(BoilerNum)%MaxPartLoadRat       = rNumericArgs(6)
      Boiler(BoilerNum)%OptPartLoadRat       = rNumericArgs(7)

      Boiler(BoilerNum)%TempUpLimitBoilerOut = rNumericArgs(8)
      ! default to 99.9C if upper temperature limit is left blank.
      IF(Boiler(BoilerNum)%TempUpLimitBoilerOut .LE. 0.0d0)THEN
        Boiler(BoilerNum)%TempUpLimitBoilerOut = 99.9d0
      END IF

      Boiler(BoilerNum)%ParasiticElecLoad    = rNumericArgs(9)
      Boiler(BoilerNum)%SizFac               = rNumericArgs(10)
      IF (Boiler(BoilerNum)%SizFac == 0.0d0) Boiler(BoilerNum)%SizFac = 1.0d0

      Boiler(BoilerNum)%BoilerInletNodeNum   = &
               GetOnlySingleNode(cAlphaArgs(5),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1),NodeType_Water, &
               NodeConnectionType_Inlet, 1, ObjectIsNotParent)
      Boiler(BoilerNum)%BoilerOutletNodeNum  = &
               GetOnlySingleNode(cAlphaArgs(6),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1),NodeType_Water, &
               NodeConnectionType_Outlet, 1, ObjectIsNotParent)
      CALL TestCompSet(TRIM(cCurrentModuleObject),cAlphaArgs(1),cAlphaArgs(5),cAlphaArgs(6),'Hot Water Nodes')

      SELECT CASE (TRIM(cAlphaArgs(7)))
      CASE ('CONSTANTFLOW')
        Boiler(BoilerNum)%FlowMode  = ConstantFlow
      CASE ('VARIABLEFLOW') ! backward compatible, clean out eventually
        Boiler(BoilerNum)%FlowMode  = LeavingSetpointModulated
        CALL ShowWarningError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'",')
        CALL ShowContinueError('Invalid '//TRIM(cAlphaFieldNames(7))//'='//TRIM(cAlphaArgs(7)))
        CALL ShowContinueError('Key choice is now called "LeavingSetpointModulated" and the simulation continues')
      CASE ('LEAVINGSETPOINTMODULATED')
         Boiler(BoilerNum)%FlowMode  = LeavingSetpointModulated
      CASE ('NOTMODULATED')
         Boiler(BoilerNum)%FlowMode  = NotModulated
      CASE DEFAULT
        CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'",')
        CALL ShowContinueError('Invalid '//TRIM(cAlphaFieldNames(7))//'='//TRIM(cAlphaArgs(7)))
        CALL ShowContinueError('Available choices are ConstantFlow, NotModulated, or LeavingSetpointModulated')
        CALL ShowContinueError('Flow mode NotModulated is assumed and the simulation continues.')
         ! We will assume variable flow if not specified
         Boiler(BoilerNum)%FlowMode  = NotModulated
      END SELECT

    END DO

    IF (ErrorsFound) THEN
      CALL ShowFatalError(RoutineName//'Errors found in processing '//TRIM(cCurrentModuleObject)//' input.')
    ENDIF

  DO BoilerNum = 1, NumBoilers
     CALL SetupOutputVariable('Boiler Heating Rate [W]', &
          BoilerReport(BoilerNum)%BoilerLoad,'System','Average',Boiler(BoilerNum)%Name)
     CALL SetupOutputVariable('Boiler Heating Energy [J]', &
          BoilerReport(BoilerNum)%BoilerEnergy,'System','Sum',Boiler(BoilerNum)%Name,  &
                    ResourceTypeKey='ENERGYTRANSFER',EndUseKey='BOILERS',GroupKey='Plant')
     IF (SameString(BoilerFuelTypeForOutputVariable(BoilerNum), 'Electric')) THEN
       CALL SetupOutputVariable('Boiler ' // TRIM(BoilerFuelTypeForOutputVariable(BoilerNum)) //' Power [W]', &
            BoilerReport(BoilerNum)%FuelUsed,'System','Average',Boiler(BoilerNum)%Name)
     ELSE
       CALL SetupOutputVariable('Boiler ' // TRIM(BoilerFuelTypeForOutputVariable(BoilerNum)) //' Rate [W]', &
            BoilerReport(BoilerNum)%FuelUsed,'System','Average',Boiler(BoilerNum)%Name)
     ENDIF
     CALL SetupOutputVariable('Boiler ' // TRIM(BoilerFuelTypeForOutputVariable(BoilerNum)) //' Energy [J]', &
          BoilerReport(BoilerNum)%FuelConsumed,'System','Sum',Boiler(BoilerNum)%Name,  &
                    ResourceTypeKey=TRIM(BoilerFuelTypeForOutputVariable(BoilerNum)),EndUseKey='Heating', &
                    EndUseSubKey='Boiler', GroupKey='Plant')
     CALL SetupOutputVariable('Boiler Inlet Temperature [C]', &
          BoilerReport(BoilerNum)%BoilerInletTemp,'System','Average',Boiler(BoilerNum)%Name)
     CALL SetupOutputVariable('Boiler Outlet Temperature [C]', &
          BoilerReport(BoilerNum)%BoilerOutletTemp,'System','Average',Boiler(BoilerNum)%Name)
     CALL SetupOutputVariable('Boiler Mass Flow Rate [kg/s]', &
          BoilerReport(BoilerNum)%Mdot,'System','Average',Boiler(BoilerNum)%Name)
     CALL SetupOutputVariable('Boiler Ancillary Electric Power [W]', &
          BoilerReport(BoilerNum)%ParasiticElecPower,'System','Average',Boiler(BoilerNum)%Name)
     CALL SetupOutputVariable('Boiler Ancillary Electric Energy [J]', &
          BoilerReport(BoilerNum)%ParasiticElecConsumption,'System','Sum',Boiler(BoilerNum)%Name, &
                    ResourceTypeKey='ELECTRICITY',EndUseKey='Heating',EndUseSubKey='Boiler Parasitic', &
                    GroupKey='Plant')
     CALL SetupOutputVariable('Boiler Part Load Ratio []', &
          BoilerReport(BoilerNum)%BoilerPLR,'System','Average',Boiler(BoilerNum)%Name)
     IF (AnyEnergyManagementSystemInModel) THEN
       CALL SetupEMSInternalVariable('Boiler Nominal Capacity', Boiler(BoilerNum)%Name, '[W]', &
                                     Boiler(BoilerNum)%NomCap  )
     ENDIF

  END DO

  DEALLOCATE(BoilerFuelTypeForOutputVariable)


RETURN
END SUBROUTINE GetBoilerInput

SUBROUTINE InitBoiler(BoilerNum)


          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   April 2002
          !       MODIFIED       na
          !       RE-ENGINEERED  Brent Griffith, rework for plant upgrade

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is for initializations of the Boiler components

          ! METHODOLOGY EMPLOYED:
          ! Uses the status flags to trigger initializations.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataGlobals,     ONLY : BeginEnvrnFlag, AnyEnergyManagementSystemInModel
  USE FluidProperties, ONLY : GetDensityGlycol
  USE PlantUtilities,  ONLY : InitComponentNodes
  USE DataPlant,       ONLY : TypeOf_Boiler_Simple, PlantSizesOkayToFinalize, &
                              PlantSizeNotComplete, LoopFlowStatus_NeedyIfLoopOn, &
                              SingleSetpoint, DualSetpointDeadband
  USE EMSManager,      ONLY: iTemperatureSetpoint, CheckIfNodeSetpointManagedByEMS
  USE DataInterfaces,  ONLY: ShowFatalError, ShowSevereError, ShowContinueError

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN) :: BoilerNum     ! number of the current boiler being simulated

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  LOGICAL, SAVE                            :: MyOneTimeFlag = .TRUE. ! one time flag
  LOGICAL, ALLOCATABLE, SAVE, DIMENSION(:) :: MyEnvrnFlag            ! environment flag
  LOGICAL, ALLOCATABLE, SAVE, DIMENSION(:) :: MyFlag
  REAL(r64) :: rho
  LOGICAL   :: FatalError
  LOGICAL :: errFlag
          ! FLOW:

  ! Do the one time initializations
  IF (MyOneTimeFlag) THEN
    ALLOCATE(MyFlag(NumBoilers))
    ALLOCATE(MyEnvrnFlag(NumBoilers))
    MyFlag = .TRUE.
    MyEnvrnFlag = .TRUE.
    MyOneTimeFlag = .FALSE.
  END IF

  ! Init more variables
  IF (MyFlag(BoilerNum)) THEN
    ! Locate the boilers on the plant loops for later usage
    errFlag=.false.
    CALL ScanPlantLoopsForObject(Boiler(BoilerNum)%Name, &
                                 TypeOf_Boiler_Simple, &
                                 Boiler(BoilerNum)%LoopNum, &
                                 Boiler(BoilerNum)%LoopSideNum, &
                                 Boiler(BoilerNum)%BranchNum, &
                                 Boiler(BoilerNum)%CompNum,  &
                                 HighLimitTemp = Boiler(BoilerNum)%TempUpLimitBoilerOut, &
                                 errFlag=errFlag)
    IF (errFlag) THEN
      CALL ShowFatalError('InitBoiler: Program terminated due to previous condition(s).')
    ENDIF

    IF ((Boiler(BoilerNum)%FlowMode == LeavingSetpointModulated) .OR. (Boiler(BoilerNum)%FlowMode == ConstantFlow)) THEN
      ! reset flow priority
      PlantLoop(Boiler(BoilerNum)%LoopNum)%LoopSide(Boiler(BoilerNum)%LoopSideNum)% &
          Branch(Boiler(BoilerNum)%BranchNum)%Comp(Boiler(BoilerNum)%CompNum)%FlowPriority = LoopFlowStatus_NeedyIfLoopOn
    ENDIF

    MyFlag(BoilerNum)=.FALSE.
  ENDIF

  IF(MyEnvrnFlag(BoilerNum) .and. BeginEnvrnFlag .AND. (PlantSizesOkayToFinalize))Then
    IF (PlantSizeNotComplete) CALL SizeBoiler(BoilerNum)
    rho = GetDensityGlycol(PlantLoop(Boiler(BoilerNum)%LoopNum)%FluidName,  &
               InitConvTemp,                      &
               PlantLoop(Boiler(BoilerNum)%LoopNum)%FluidIndex, &
               'InitBoiler')
    Boiler(BoilerNum)%DesMassFlowRate = Boiler(BoilerNum)%VolFlowRate * rho

    CALL InitComponentNodes(0.d0,Boiler(BoilerNum)%DesMassFlowRate,     &
                                 Boiler(BoilerNum)%BoilerInletNodeNum,  &
                                 Boiler(BoilerNum)%BoilerOutletNodeNum,  &
                                 Boiler(BoilerNum)%LoopNum,             &
                                 Boiler(BoilerNum)%LoopSideNum,         &
                                 Boiler(BoilerNum)%BranchNum,           &
                                 Boiler(BoilerNum)%CompNum)


    IF (Boiler(BoilerNum)%FlowMode == LeavingSetpointModulated) Then ! check if setpoint on outlet node
      IF ((Node(Boiler(BoilerNum)%BoilerOutletNodeNum)%TempSetPoint == SensedNodeFlagValue) .AND. &
          (Node(Boiler(BoilerNum)%BoilerOutletNodeNum)%TempSetPointLo == SensedNodeFlagValue)) THEN
        IF (.NOT. AnyEnergyManagementSystemInModel) THEN
          IF (.NOT. Boiler(BoilerNum)%ModulatedFlowErrDone) THEN
            CALL ShowWarningError('Missing temperature setpoint for LeavingSetpointModulated mode Boiler named ' // &
                                          TRIM(Boiler(BoilerNum)%Name) )
            CALL ShowContinueError('  A temperature setpoint is needed at the outlet node of a boiler ' // &
                                             'in variable flow mode, use a SetpointManager')
            CALL ShowContinueError('  The overall loop setpoint will be assumed for Boiler. The simulation continues ... ')
            Boiler(BoilerNum)%ModulatedFlowErrDone = .TRUE.
          ENDIF
        ELSE
         ! need call to EMS to check node
          FatalError = .FALSE. ! but not really fatal yet, but should be.
          CALL CheckIfNodeSetpointManagedByEMS(Boiler(BoilerNum)%BoilerOutletNodeNum,iTemperatureSetpoint, FatalError)
          IF (FatalError) THEN
            IF (.NOT. Boiler(BoilerNum)%ModulatedFlowErrDone) THEN
              CALL ShowWarningError('Missing temperature setpoint for LeavingSetpointModulated mode Boiler named ' // &
                                          TRIM(Boiler(BoilerNum)%Name) )
              CALL ShowContinueError('  A temperature setpoint is needed at the outlet node of a boiler ' // &
                                             'in variable flow mode')
              CALL ShowContinueError('  use a Setpoint Manager to establish a setpoint at the boiler outlet node ')
              CALL ShowContinueError('  or use an EMS actuator to establish a setpoint at the boiler outlet node ')
              CALL ShowContinueError('  The overall loop setpoint will be assumed for Boiler. The simulation continues ... ')
              Boiler(BoilerNum)%ModulatedFlowErrDone = .TRUE.
            ENDIF
          ENDIF
        ENDIF
        Boiler(BoilerNum)%ModulatedFlowSetToLoop = .TRUE. ! this is for backward compatibility and could be removed
      ENDIF
    ENDIF

    MyEnvrnFlag(BoilerNum) = .FALSE.
  END IF

  IF (.not. BeginEnvrnFlag) THEN
    MyEnvrnFlag(BoilerNum)=.TRUE.
  ENDIF

  ! every iteration inits.  (most in calc routine)

  IF ((Boiler(BoilerNum)%FlowMode == LeavingSetpointModulated) .AND. Boiler(BoilerNum)%ModulatedFlowSetToLoop) THEN
  ! fix for clumsy old input that worked because loop setpoint was spread.
  !  could be removed with transition, testing , model change, period of being obsolete.
    SELECT CASE (PlantLoop(Boiler(BoilerNum)%LoopNum)%LoopDemandCalcScheme)
    CASE (SingleSetPoint)
      Node(Boiler(BoilerNum)%BoilerOutletNodeNum)%TempSetPoint =                        &
           Node(PlantLoop(Boiler(BoilerNum)%LoopNum)%TempSetPointNodeNum)%TempSetPoint
    CASE (DualSetPointDeadBand)
      Node(Boiler(BoilerNum)%BoilerOutletNodeNum)%TempSetPointLo =                        &
           Node(PlantLoop(Boiler(BoilerNum)%LoopNum)%TempSetPointNodeNum)%TempSetPointLo
    END SELECT
  ENDIF

  RETURN

END SUBROUTINE InitBoiler

SUBROUTINE SizeBoiler(BoilerNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   April 2002
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is for sizing Boiler Components for which capacities and flow rates
          ! have not been specified in the input.

          ! METHODOLOGY EMPLOYED:
          ! Obtains hot water flow rate from the plant sizing array. Calculates nominal capacity from
          ! the hot water flow rate and the hot water loop design delta T.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataSizing
  USE DataPlant,        ONLY: PlantLoop, PlantSizesOkayToFinalize

  USE FluidProperties,  ONLY: GetDensityGlycol, GetSpecificHeatGlycol
  USE PlantUtilities,   ONLY: RegisterPlantCompDesignFlow
  USE ReportSizingManager, ONLY: ReportSizingOutput
  USE OutputReportPredefined

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: BoilerNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER                      :: PltSizNum     ! Plant Sizing index corresponding to CurLoopNum
  LOGICAL                      :: ErrorsFound   ! If errors detected in input
  CHARACTER(len=MaxNameLength) :: equipName     ! Name of boiler object
  REAL(r64)                    :: rho
  REAL(r64)                    :: Cp
  REAL(r64)           :: tmpNomCap ! local nominal capacity cooling power
  REAL(r64)           :: tmpBoilerVolFlowRate ! local boiler design volume flow rate


  PltSizNum = 0
  ErrorsFound = .FALSE.

  tmpNomCap = Boiler(BoilerNum)%NomCap
  tmpBoilerVolFlowRate =  Boiler(BoilerNum)%VolFlowRate

  PltSizNum = PlantLoop(Boiler(BoilerNum)%LoopNum)%PlantSizNum

  IF (Boiler(BoilerNum)%NomCap  == AutoSize) THEN
    IF (PltSizNum > 0) THEN
      IF (PlantSizData(PltSizNum)%DesVolFlowRate >= SmallWaterVolFlow) THEN

        rho = GetDensityGlycol(PlantLoop(Boiler(BoilerNum)%LoopNum)%FluidName,  &
             InitConvTemp,                      &
             PlantLoop(Boiler(BoilerNum)%LoopNum)%FluidIndex, &
             'SizeBoiler')
        Cp = GetSpecificHeatGlycol(PlantLoop(Boiler(BoilerNum)%LoopNum)%FluidName,  &
             Boiler(BoilerNum)%TempDesBoilerOut,                      &
             PlantLoop(Boiler(BoilerNum)%LoopNum)%FluidIndex, &
             'SizeBoiler')
        tmpNomCap = Cp * rho * Boiler(BoilerNum)%SizFac &
                                                    * PlantSizData(PltSizNum)%DeltaT &
                                                    * PlantSizData(PltSizNum)%DesVolFlowRate
        IF (PlantSizesOkayToFinalize) Boiler(BoilerNum)%NomCap = tmpNomCap
      ELSE
        tmpNomCap = 0.d0
        IF (PlantSizesOkayToFinalize) Boiler(BoilerNum)%NomCap = tmpNomCap
      END IF
      IF (PlantSizesOkayToFinalize) CALL ReportSizingOutput('Boiler:HotWater', Boiler(BoilerNum)%Name, &
                              'Nominal Capacity [W]', Boiler(BoilerNum)%NomCap)
    ELSE
      CALL ShowSevereError('Autosizing of Boiler nominal capacity requires a loop Sizing:Plant object')
      CALL ShowContinueError('Occurs in Boiler object='//TRIM(Boiler(BoilerNum)%Name))
      ErrorsFound = .TRUE.
    END IF
  END IF

  IF (Boiler(BoilerNum)%VolFlowRate == AutoSize) THEN
    IF (PltSizNum > 0) THEN
      IF (PlantSizData(PltSizNum)%DesVolFlowRate >= SmallWaterVolFlow) THEN
        tmpBoilerVolFlowRate = PlantSizData(PltSizNum)%DesVolFlowRate * Boiler(BoilerNum)%SizFac
        IF (PlantSizesOkayToFinalize) Boiler(BoilerNum)%VolFlowRate = tmpBoilerVolFlowRate
      ELSE
        tmpBoilerVolFlowRate = 0.0d0
        IF (PlantSizesOkayToFinalize) Boiler(BoilerNum)%VolFlowRate = tmpBoilerVolFlowRate
      END IF
      IF (PlantSizesOkayToFinalize) CALL ReportSizingOutput('Boiler:HotWater', Boiler(BoilerNum)%Name, &
                              'Design Water Flow Rate [m3/s]', &
                              Boiler(BoilerNum)%VolFlowRate)
    ELSE
      CALL ShowSevereError('Autosizing of Boiler design flow rate requires a loop Sizing:Plant object')
      CALL ShowContinueError('Occurs in Boiler object='//TRIM(Boiler(BoilerNum)%Name))
      ErrorsFound = .TRUE.
    END IF
  END IF

  CALL RegisterPlantCompDesignFlow(Boiler(BoilerNum)%BoilerInletNodeNum,tmpBoilerVolFlowRate)

  IF (PlantSizesOkayToFinalize) THEN
    !create predefined report
    equipName = Boiler(BoilerNum)%Name
    CALL PreDefTableEntry(pdchMechType,equipName,'Boiler:HotWater')
    CALL PreDefTableEntry(pdchMechNomEff,equipName,Boiler(BoilerNum)%Effic)
    CALL PreDefTableEntry(pdchMechNomCap,equipName,Boiler(BoilerNum)%NomCap)
  ENDIF

  IF (ErrorsFound) THEN
    CALL ShowFatalError('Preceding sizing errors cause program termination')
  END IF

  RETURN
END SUBROUTINE SizeBoiler

SUBROUTINE CalcBoilerModel(BoilerNum,MyLoad,Runflag,EquipFlowCtrl)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Dan Fisher
          !       DATE WRITTEN   April 1999
          !       MODIFIED       Taecheol Kim,May 2000
          !                      R. Raustad - FSEC, June 2008: added boiler efficiency curve object
          !                      B. Griffith - NREL, Aug 2011: added switch for temperature to use in curve
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine calculates the boiler fuel consumption and the associated
          ! hot water demand met by the boiler

          ! METHODOLOGY EMPLOYED:
          ! The model is based on a single combustion efficiency (=1 for electric)
          ! and a second order polynomial fit of performance data to obtain part
          ! load performance

          ! REFERENCES:

          ! USE STATEMENTS:
  USE DataGlobals,    ONLY: BeginEnvrnFlag, WarmupFlag

  USE FluidProperties, ONLY: GetSpecificHeatGlycol
  USE DataBranchAirLoopPlant, ONLY: ControlType_SeriesActive
  USE CurveManager,   ONLY: CurveValue
  USE General,        ONLY: TrimSigDigits
  USE PlantUtilities, ONLY: SetComponentFlowRate
  USE DataPlant,      ONLY: SingleSetpoint, DualSetpointDeadband

  IMPLICIT NONE

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER                :: BoilerNum       ! boiler identifier
  REAL(r64)              :: MyLoad          ! W - hot water demand to be met by boiler
  LOGICAL                :: RunFlag         ! TRUE if boiler operating
  INTEGER, INTENT(IN)    :: EquipFlowCtrl   ! Flow control mode for the equipment

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64)       :: BoilerEFF             ! boiler efficiency
  REAL(r64)       :: BoilerNomCap          ! W - boiler nominal capacity
  REAL(r64)       :: BoilerMaxPLR          ! boiler maximum part load ratio
  REAL(r64)       :: BoilerMinPLR          ! boiler minimum part load ratio
  REAL(r64)       :: TheorFuelUse          ! Theoretical (stoichiometric) fuel use
  REAL(r64)       :: OperPLR               ! operating part load ratio
  REAL(r64)       :: BoilerDeltaTemp       ! C - boiler inlet to outlet temperature difference
  REAL(r64)       :: TempUpLimitBout       ! C - boiler high temperature limit
  INTEGER         :: BoilerInletNode       ! Boiler inlet node number
  INTEGER         :: BoilerOutletNode      ! Boiler outlet node number
  INTEGER         :: LoopNum               ! Plant loop with boiler
  INTEGER         :: LoopSideNum           ! Plant loop side with boiler (supply, demand)
  REAL(r64)       :: BoilerMassFlowRateMax ! Max Design Boiler Mass Flow Rate converted from Volume Flow Rate
  REAL(r64)       :: ParasiticElecLoad     ! Boiler parasitic electric power at full load
  REAL(r64)       :: EffCurveOutput        ! Output of boiler efficiency curve
  REAL(r64)       :: Cp

          !FLOW

  BoilerLoad            = 0.0d0
  ParasiticElecPower    = 0.0d0
  BoilerMassFlowRate    = 0.0d0
  BoilerInletNode       = Boiler(BoilerNum)%BoilerInletNodeNum
  BoilerOutletNode      = Boiler(BoilerNum)%BoilerOutletNodeNum
  BoilerNomCap          = Boiler(BoilerNum)%NomCap
  BoilerMaxPLR          = Boiler(BoilerNum)%MaxPartLoadRat
  BoilerMinPLR          = Boiler(BoilerNum)%MinPartLoadRat
  BoilerEff             = Boiler(BoilerNum)%Effic
  TempUpLimitBout       = Boiler(BoilerNum)%TempUpLimitBoilerOut
  BoilerMassFlowRateMax = Boiler(BoilerNum)%DesMassFlowRate
  ParasiticElecLoad     = Boiler(BoilerNum)%ParasiticElecLoad
  LoopNum               = Boiler(BoilerNum)%LoopNum
  LoopSideNum           = Boiler(BoilerNum)%LoopSideNum

  Cp = GetSpecificHeatGlycol(PlantLoop(Boiler(BoilerNum)%LoopNum)%FluidName,   &
                              Node(BoilerInletNode)%Temp,                      &
                              PlantLoop(Boiler(BoilerNum)%LoopNum)%FluidIndex, &
                              'CalcBoilerModel')


    !If the specified load is 0.0 or the boiler should not run then we leave this subroutine. Before leaving
    !if the component control is SERIESACTIVE we set the component flow to inlet flow so that flow resolver
    !will not shut down the branch
  IF(MyLoad <= 0.0d0 .OR. .NOT. RunFlag) THEN
    IF(EquipFlowCtrl == ControlType_SeriesActive) BoilerMassFlowRate = Node(BoilerInletNode)%MassFlowrate
    RETURN
  END IF

    !Set the current load equal to the boiler load
  BoilerLoad = MyLoad

  IF (PlantLoop(LoopNum)%Loopside(LoopSideNum)%FlowLock==0) THEN
      ! Either set the flow to the Constant value or caluclate the flow for the variable volume
    If ((Boiler(BoilerNum)%FlowMode == ConstantFlow ) .OR. (Boiler(BoilerNum)%FlowMode == NotModulated)) THEN
            ! Then find the flow rate and outlet temp
      BoilerMassFlowRate = BoilerMassFlowRateMax
      CALL SetComponentFlowRate(BoilerMassFlowRate, BoilerInletNode, BoilerOutletNode, &
                                               Boiler(BoilerNum)%LoopNum,     &
                                               Boiler(BoilerNum)%LoopSideNum, &
                                               Boiler(BoilerNum)%BranchNum,   &
                                               Boiler(BoilerNum)%CompNum)

      IF ((BoilerMassFlowRate /= 0.0D0) .AND. (MyLoad > 0.d0)) THEN
        BoilerDeltaTemp = BoilerLoad/BoilerMassFlowRate/Cp
      ELSE
        BoilerDeltaTemp = 0.0D0
      ENDIF

      BoilerOutletTemp = BoilerDeltaTemp + Node(BoilerInletNode)%Temp

    ELSE IF (Boiler(BoilerNum)%FlowMode == LeavingSetpointModulated) THEN
            ! Calculate the Delta Temp from the inlet temp to the boiler outlet setpoint
            ! Then find the flow rate and outlet temp

      SELECT CASE (PlantLoop(Boiler(BoilerNum)%LoopNum)%LoopDemandCalcScheme)
      CASE (SingleSetPoint)
        BoilerDeltaTemp = Node(BoilerOutletNode)%TempSetPoint-Node(BoilerInletNode)%Temp
      CASE (DualSetPointDeadBand)
        BoilerDeltaTemp = Node(BoilerOutletNode)%TempSetPointLo-Node(BoilerInletNode)%Temp
      END SELECT

      BoilerOutletTemp = BoilerDeltaTemp + Node(BoilerInletNode)%Temp

      IF((BoilerDeltaTemp > 0.0d0) .AND. (BoilerLoad > 0.d0) ) THEN
        BoilerMassFlowRate = BoilerLoad/Cp/BoilerDeltaTemp

        BoilerMassFlowRate = MIN(BoilerMassFlowRateMax, BoilerMassFlowRate)

      ELSE
        BoilerMassFlowRate =0.0d0
      END IF
      CALL SetComponentFlowRate(BoilerMassFlowRate, BoilerInletNode, BoilerOutletNode, &
                                               Boiler(BoilerNum)%LoopNum,     &
                                               Boiler(BoilerNum)%LoopSideNum, &
                                               Boiler(BoilerNum)%BranchNum,   &
                                               Boiler(BoilerNum)%CompNum)

    END IF  !End of Constant/Variable Flow If Block

  ELSE    ! If FlowLock is True
        ! Set the boiler flow rate from inlet node and then check performance
    BoilerMassFlowRate = Node(BoilerInletNode)%MassFlowRate

    IF ((MyLoad > 0.d0) .AND. (BoilerMassFlowRate > 0.d0)) THEN ! this boiler has a heat load
      BoilerLoad = MyLoad
      IF (BoilerLoad > BoilerNomCap*BoilerMaxPLR) BoilerLoad = BoilerNomCap*BoilerMaxPLR
      IF (BoilerLoad < BoilerNomCap*BoilerMinPLR) BoilerLoad = BoilerNomCap*BoilerMinPLR
      BoilerOutletTemp = Node(BoilerInletNode)%Temp + BoilerLoad /(BoilerMassFlowRate * Cp)
      BoilerDeltaTemp = BoilerOutletTemp-Node(BoilerInletNode)%Temp
    ELSE
      BoilerLoad = 0.0d0
      BoilerOutletTemp = Node(BoilerInletNode)%Temp
    ENDIF

  END IF  !End of the FlowLock If block

        ! Limit BoilerOutletTemp.  If > max temp, trip boiler off
  IF(BoilerOutletTemp > TempUpLimitBout) THEN
      BoilerDeltaTemp  = 0.0d0
      BoilerLoad       = 0.0d0
      BoilerOutletTemp = Node(BoilerInletNode)%Temp
  END IF


  OperPLR      = BoilerLoad/BoilerNomCap
  OperPLR      = MIN(OperPLR,BoilerMaxPLR)
  OperPLR      = MAX(OperPLR,BoilerMinPLR)

    ! set report variable
  BoilerPLR    = OperPLR

    ! calculate theoretical fuel use based on nominal thermal efficiency
  TheorFuelUse = BoilerLoad/BoilerEff

   ! calculate normalized efficiency based on curve object type
  IF(Boiler(BoilerNum)%EfficiencyCurvePtr .GT. 0)THEN
    IF(Boiler(BoilerNum)%EfficiencyCurveType .EQ. Biquadratic .OR. &
      Boiler(BoilerNum)%EfficiencyCurveType .EQ. QuadraticLinear .OR. &
      Boiler(BoilerNum)%EfficiencyCurveType .EQ. Bicubic)THEN

      IF (Boiler(BoilerNum)%CurveTempMode == EnteringBoilerTemp) THEN
        EffCurveOutput = CurveValue(Boiler(BoilerNum)%EfficiencyCurvePtr,OperPLR, &
                                     Node(BoilerInletNode)%Temp)
      ELSEIF (Boiler(BoilerNum)%CurveTempMode == LeavingBoilerTemp) THEN
        EffCurveOutput = CurveValue(Boiler(BoilerNum)%EfficiencyCurvePtr,OperPLR,BoilerOutletTemp)
      ENDIF

    ELSE
      EffCurveOutput = CurveValue(Boiler(BoilerNum)%EfficiencyCurvePtr,OperPLR)
    END IF
  ELSE
    EffCurveOutput = 1.0d0
  END IF

    ! warn if efficiency curve produces zero or negative results
  IF(.NOT. WarmupFlag .AND. EffCurveOutput .LE. 0.0d0)THEN
    IF(BoilerLoad .GT. 0.0d0)THEN
      IF(Boiler(BoilerNum)%EffCurveOutputError .LT. 1)THEN
        Boiler(BoilerNum)%EffCurveOutputError = Boiler(BoilerNum)%EffCurveOutputError + 1
        CALL ShowWarningError('Boiler:HotWater "'//TRIM(Boiler(BoilerNum)%Name)//'"')
        CALL ShowContinueError('...Normalized Boiler Efficiency Curve output is less than or equal to 0.')
        CALL ShowContinueError('...Curve input x value (PLR)     = '//TrimSigDigits(OperPLR,5))
        IF(Boiler(BoilerNum)%EfficiencyCurveType .EQ. Biquadratic .OR. &
             Boiler(BoilerNum)%EfficiencyCurveType .EQ. QuadraticLinear .OR. &
             Boiler(BoilerNum)%EfficiencyCurveType .EQ. Bicubic)THEN
          IF (Boiler(BoilerNum)%CurveTempMode == EnteringBoilerTemp) THEN
            CALL ShowContinueError('...Curve input y value (Tinlet) = '// &
                                        TrimSigDigits(Node(BoilerInletNode)%Temp,2))
          ELSEIF (Boiler(BoilerNum)%CurveTempMode == LeavingBoilerTemp) THEN
            CALL ShowContinueError('...Curve input y value (Toutlet) = '//TrimSigDigits(BoilerOutletTemp,2))
          ENDIF
        END IF
        CALL ShowContinueError('...Curve output (normalized eff) = '//TrimSigDigits(EffCurveOutput,5))
        CALL ShowContinueError('...Calculated Boiler efficiency  = '//TRIM(TrimSigDigits(EffCurveOutput*BoilerEff,5))// &
                              ' (Boiler efficiency = Nominal Thermal Efficiency * Normalized Boiler Efficiency Curve output)')
        CALL ShowContinueErrorTimeStamp('...Curve output reset to 0.01 and simulation continues.')
      ELSE
        CALL ShowRecurringWarningErrorAtEnd('Boiler:HotWater "'//TRIM(Boiler(BoilerNum)%Name)//'":'//&
            ' Boiler Efficiency Curve output is less than or equal to 0 warning continues...' &
            , Boiler(BoilerNum)%EffCurveOutputIndex, EffCurveOutput, EffCurveOutput)
      END IF
    END If
    EffCurveOutput = 0.01d0
  END IF

    ! warn if overall efficiency greater than 1.1
  IF(.NOT. WarmupFlag .AND. EffCurveOutput*BoilerEff .GT. 1.1d0)THEN
    IF(BoilerLoad .GT. 0.0d0 .AND. Boiler(BoilerNum)%EfficiencyCurvePtr .GT. 0)THEN
      IF(Boiler(BoilerNum)%CalculatedEffError .LT. 1)THEN
        Boiler(BoilerNum)%CalculatedEffError = Boiler(BoilerNum)%CalculatedEffError + 1
        CALL ShowWarningError('Boiler:HotWater "'//TRIM(Boiler(BoilerNum)%Name)//'"')
        CALL ShowContinueError('...Calculated Boiler Efficiency is greater than 1.1.')
        CALL ShowContinueError('...Boiler Efficiency calculations shown below.')
        CALL ShowContinueError('...Curve input x value (PLR)     = '//TrimSigDigits(OperPLR,5))
        IF(Boiler(BoilerNum)%EfficiencyCurveType .EQ. Biquadratic .OR. &
             Boiler(BoilerNum)%EfficiencyCurveType .EQ. QuadraticLinear .OR. &
             Boiler(BoilerNum)%EfficiencyCurveType .EQ. Bicubic)THEN
          IF (Boiler(BoilerNum)%CurveTempMode == EnteringBoilerTemp) THEN
            CALL ShowContinueError('...Curve input y value (Tinlet) = '// &
                                        TrimSigDigits(Node(BoilerInletNode)%Temp,2))
          ELSEIF (Boiler(BoilerNum)%CurveTempMode == LeavingBoilerTemp) THEN
            CALL ShowContinueError('...Curve input y value (Toutlet) = '//TrimSigDigits(BoilerOutletTemp,2))
          ENDIF
        END IF
        CALL ShowContinueError('...Curve output (normalized eff) = '//TrimSigDigits(EffCurveOutput,5))
        CALL ShowContinueError('...Calculated Boiler efficiency  = '//TRIM(TrimSigDigits(EffCurveOutput*BoilerEff,5))// &
                               ' (Boiler efficiency = Nominal Thermal Efficiency * Normalized Boiler Efficiency Curve output)')
        CALL ShowContinueErrorTimeStamp('...Curve output reset to 1.1 and simulation continues.')
      ELSE
        CALL ShowRecurringWarningErrorAtEnd('Boiler:HotWater "'//TRIM(Boiler(BoilerNum)%Name)//'":'//&
            ' Calculated Boiler Efficiency is greater than 1.1 warning continues...' &
            , Boiler(BoilerNum)%CalculatedEffIndex, EffCurveOutput*BoilerEff, EffCurveOutput*BoilerEff)
      END IF
    END If
    EffCurveOutput = 1.1d0
  END IF

    ! calculate fuel used based on normalized boiler efficiency curve (=1 when no curve used)
  FuelUsed=TheorFuelUse/EffCurveOutput
  IF(BoilerLoad .GT. 0.0d0)ParasiticElecPower=ParasiticElecLoad*OperPLR

  RETURN
END SUBROUTINE CalcBoilerModel


! Beginning of Record Keeping subroutines for the BOILER:HOTWATER Module
! *****************************************************************************

SUBROUTINE UpdateBoilerRecords(MyLoad,RunFlag, Num)
            ! SUBROUTINE INFORMATION:
            !       AUTHOR:          Dan Fisher
            !       DATE WRITTEN:    October 1998

            ! PURPOSE OF THIS SUBROUTINE:
            ! boiler simulation reporting


            ! METHODOLOGY EMPLOYED:na

            ! REFERENCES: na

            ! USE STATEMENTS: na

  USE PlantUtilities, ONLY : SafeCopyPlantNode
IMPLICIT NONE

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  REAL(r64), INTENT(IN) :: MyLoad        !boiler operating load
  LOGICAL,   INTENT(IN) :: RunFlag       !boiler on when TRUE
  INTEGER,   INTENT(IN) :: Num           !boiler number

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER               :: BoilerInletNode   ! Boiler inlet node number
  INTEGER               :: BoilerOutletNode  ! Boiler outlet node number
  REAL(r64)             :: ReportingConstant ! constant for converting power to energy

  ReportingConstant = TimeStepSys * SecInHour


  BoilerInletNode   = Boiler(Num)%BoilerInletNodeNum
  BoilerOutletNode  = Boiler(Num)%BoilerOutletNodeNum

  IF (MyLoad<=0 .OR. .NOT. RunFlag)THEN
          !set node temperatures
    CALL SafeCopyPlantNode(BoilerInletNode, BoilerOutletNode)
    Node(BoilerOutletNode)%Temp           = Node(BoilerInletNode)%Temp
    BoilerReport(Num)%BoilerOutletTemp    = Node(BoilerInletNode)%Temp
    BoilerReport(Num)%BoilerLoad          = 0.0d0
    BoilerReport(Num)%FuelUsed            = 0.0d0
    BoilerReport(Num)%ParasiticElecPower  = 0.0d0
    BoilerReport(Num)%BoilerPLR           = 0.0d0

  ELSE
          !set node temperatures
    CALL SafeCopyPlantNode(BoilerInletNode, BoilerOutletNode)
    Node(BoilerOutletNode)%Temp           = BoilerOutletTemp
    BoilerReport(Num)%BoilerOutletTemp    = BoilerOutletTemp
    BoilerReport(Num)%BoilerLoad          = BoilerLoad
    BoilerReport(Num)%FuelUsed            = FuelUsed
    BoilerReport(Num)%ParasiticElecPower  = ParasiticElecPower
    BoilerReport(Num)%BoilerPLR           = BoilerPLR

  END IF

  BoilerReport(Num)%BoilerInletTemp           = Node(BoilerInletNode)%Temp
  BoilerReport(Num)%Mdot                      = Node(BoilerOutletNode)%MassFlowRate

  BoilerReport(Num)%BoilerEnergy              = BoilerReport(Num)%BoilerLoad * ReportingConstant
  BoilerReport(Num)%FuelConsumed              = BoilerReport(Num)%FuelUsed * ReportingConstant
  BoilerReport(Num)%ParasiticElecConsumption  = BoilerReport(Num)%ParasiticElecPower * ReportingConstant

RETURN
END SUBROUTINE UpdateBoilerRecords

! End of Record Keeping subroutines for the BOILER:HOTWATER Module
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

END MODULE Boilers
