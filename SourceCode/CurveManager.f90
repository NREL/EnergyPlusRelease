MODULE CurveManager
  ! Module containing the Curve Manager routines

  ! MODULE INFORMATION:
  !       AUTHOR         Fred Buhl
  !       DATE WRITTEN   May 2000
  !       MODIFIED       January 2006, Rick Strand, added a curve type (quadratic-linear)
  !                      July 2006, L. Gu, added a new curve type (bicubic)

  !                      July 2006, Brent Griffith, added triquadratic curve
  !                       RR added exponential curve
  !                      May 2009 Brent griffith add EMS actuator registry and override (for custom equations)
  !                      August 2010, Richard Raustad, FSEC, added Table:* objects
  !                      Future Improvements:
  !                       1) Merge TableData and TableLookup arrays. Care is needed here since the
  !                          Table:OneIndependentVariable (and Two) use different data patterns.
  !                          For Table:One - a one-to-one correspondence between X and Z
  !                          For Table:Multi - not a one-to-one correspondence between X and Z
  !                          Code does show examples of the translation so each Table object can use
  !                          either interpolation technique.
  !                       2) Subroutine PerformanceTableObject is not really needed (and is probably slower)
  !                          since Subroutine TableLookupObject can do the same thing. The difference
  !                          is that Sub PerformanceTableObject does a linear interpolation without extrapolation.
  !                          More math is also involved. Sub TableLookupObject can also do this if a) the limits
  !                          of the input data use the boundaries of the tabular data, b) the arrays are corrected
  !                          to use this other subroutine, and c) the Number of Interpolation Points is set to 2.
  !
  !                      22Aug2010 Craig Wray, added new curves for fan component model:
  !                          FanPressureRise, ExponentialSkewNormal, Sigmoid, RectangularHyperbola1,
  !                          RectangularHyperbola2, ExponentialDecay
  !                      March 2012, Atefe Makhmalbaf and Heejin Cho, added a new curve type (QuadLinear)
  !       RE-ENGINEERED  na

  ! PURPOSE OF THIS MODULE:
  ! To provide the capabilities of getting the curve data from the input,
  ! validating it, and storing it in such a manner that the curve manager
  ! can provide the simulation with performance curve output.

  ! METHODOLOGY EMPLOYED:
  ! na

  ! REFERENCES:
  ! na

  ! OTHER NOTES:
  !

  ! USE STATEMENTS:
  ! Use statements for data only modules
USE DataPrecisionGlobals
USE DataGlobals,    ONLY: MaxNameLength, AnyEnergyManagementSystemInModel
USE DataInterfaces, ONLY: ShowSevereError, ShowWarningError, ShowFatalError, &
                          ShowContinueError, SetupEMSActuator, ShowRecurringWarningErrorAtEnd
USE DataBranchAirLoopPlant

  ! Use statements for access to subroutines in other modules

IMPLICIT NONE         ! Enforce explicit typing of all variables

PRIVATE ! Everything private unless explicitly made public

  !MODULE PARAMETER DEFINITIONS
CHARACTER(len=*), PARAMETER :: Blank = ' '

! Curve Type parameters, these can differ from object types (e.g. a CurveType_TableOneIV can be linear, quadratic, etc)
INTEGER, PARAMETER :: Linear          = 1
INTEGER, PARAMETER :: BiLinear        = 2
INTEGER, PARAMETER :: Quadratic       = 3
INTEGER, PARAMETER :: BiQuadratic     = 4
INTEGER, PARAMETER :: Cubic           = 5
INTEGER, PARAMETER :: QuadraticLinear = 6
INTEGER, PARAMETER :: Bicubic         = 7
INTEGER, PARAMETER :: TriQuadratic    = 8
INTEGER, PARAMETER :: Exponent        = 9
INTEGER, PARAMETER :: Quartic         = 10
INTEGER, PARAMETER :: FuncPressDrop   = 11
INTEGER, PARAMETER :: MultiVariableLookup = 12
INTEGER, PARAMETER :: FanPressureRise       = 13
INTEGER, PARAMETER :: ExponentialSkewNormal = 14
INTEGER, PARAMETER :: Sigmoid               = 15
INTEGER, PARAMETER :: RectangularHyperbola1 = 16
INTEGER, PARAMETER :: RectangularHyperbola2 = 17
INTEGER, PARAMETER :: ExponentialDecay      = 18
INTEGER, PARAMETER :: DoubleExponentialDecay= 19
INTEGER, PARAMETER :: QuadLinear            = 20

! Interpolation Types
INTEGER, PARAMETER :: LINEARINTERPOLATIONOFTABLE = 1
INTEGER, PARAMETER :: LAGRANGEINTERPOLATIONLINEAREXTRAPOLATION = 2
INTEGER, PARAMETER :: EVALUATECURVETOLIMITS = 3

! Data Format
INTEGER, PARAMETER :: SINGLELINEINDEPENDENTVARIABLEWITHMATRIX = 1

! Sort Order
INTEGER, PARAMETER :: ASCENDING = 1
INTEGER, PARAMETER :: DESCENDING = 2

! parameters describing curve object/table types
INTEGER, PARAMETER :: NumAllCurveTypes          = 21

! curve object/table types (used for warning messages)
INTEGER, PUBLIC, PARAMETER :: CurveType_Linear          = 1
INTEGER, PUBLIC, PARAMETER :: CurveType_Quadratic       = 2
INTEGER, PUBLIC, PARAMETER :: CurveType_Cubic           = 3
INTEGER, PUBLIC, PARAMETER :: CurveType_Quartic         = 4
INTEGER, PUBLIC, PARAMETER :: CurveType_Exponent        = 5
INTEGER, PUBLIC, PARAMETER :: CurveType_BiCubic         = 6
INTEGER, PUBLIC, PARAMETER :: CurveType_BiQuadratic     = 7
INTEGER, PUBLIC, PARAMETER :: CurveType_QuadraticLinear = 8
INTEGER, PUBLIC, PARAMETER :: CurveType_TriQuadratic    = 9
INTEGER, PUBLIC, PARAMETER :: CurveType_FuncPressDrop   = 10
INTEGER, PUBLIC, PARAMETER :: CurveType_TableOneIV      = 11
INTEGER, PUBLIC, PARAMETER :: CurveType_TableTwoIV      = 12
INTEGER, PUBLIC, PARAMETER :: CurveType_TableMultiIV    = 13
INTEGER, PUBLIC, PARAMETER :: CurveType_FanPressureRise        = 14
INTEGER, PUBLIC, PARAMETER :: CurveType_ExponentialSkewNormal  = 15
INTEGER, PUBLIC, PARAMETER :: CurveType_Sigmoid                = 16
INTEGER, PUBLIC, PARAMETER :: CurveType_RectangularHyperbola1  = 17
INTEGER, PUBLIC, PARAMETER :: CurveType_RectangularHyperbola2  = 18
INTEGER, PUBLIC, PARAMETER :: CurveType_ExponentialDecay       = 19
INTEGER, PUBLIC, PARAMETER :: CurveType_DoubleExponentialDecay = 20
INTEGER, PUBLIC, PARAMETER :: CurveType_QuadLinear             = 21

CHARACTER(len=*), PARAMETER, PUBLIC, DIMENSION(NumAllCurveTypes) :: cCurveTypes=  &
       (/'Curve:Linear                  ',  &
         'Curve:Quadratic               ',  &
         'Curve:Cubic                   ',  &
         'Curve:Quartic                 ',  &
         'Curve:Exponent                ',  &
         'Curve:BiCubic                 ',  &
         'Curve:BiQuadratic             ',  &
         'Curve:QuadraitcLinear         ',  &
         'Curve:TriQuadratic            ',  &
         'Curve:Functional:PressureDrop ',  &
         'Table:OneIndependentVariable  ',  &
         'Table:TwoIndependentVariables ',  &
         'Table:MultiVariableLookup     ',  &
         'Curve:FanPressureRise         ',  &
         'Curve:ExponentialSkewNormal   ',  &
         'Curve:Sigmoid                 ',  &
         'Curve:RectangularHyperbola1   ',  &
         'Curve:RectangularHyperbola2   ',  &
         'Curve:ExponentialDecay        ',  &
         'Curve:DoubleExponentialDecay  ',  &
         'Curve:QuadLinear              '/)

  ! DERIVED TYPE DEFINITIONS
Type TriQuadraticCurveDataStruct
   ! this structure is for 27 coefficient full triquadratic (!)
  REAL(r64) :: CoeffA0 = 0.0D0
  REAL(r64) :: CoeffA1 = 0.0D0
  REAL(r64) :: CoeffA2 = 0.0D0
  REAL(r64) :: CoeffA3 = 0.0D0
  REAL(r64) :: CoeffA4 = 0.0D0
  REAL(r64) :: CoeffA5 = 0.0D0
  REAL(r64) :: CoeffA6 = 0.0D0
  REAL(r64) :: CoeffA7 = 0.0D0
  REAL(r64) :: CoeffA8 = 0.0D0
  REAL(r64) :: CoeffA9 = 0.0D0
  REAL(r64) :: CoeffA10 = 0.0D0
  REAL(r64) :: CoeffA11 = 0.0D0
  REAL(r64) :: CoeffA12 = 0.0D0
  REAL(r64) :: CoeffA13 = 0.0D0
  REAL(r64) :: CoeffA14 = 0.0D0
  REAL(r64) :: CoeffA15 = 0.0D0
  REAL(r64) :: CoeffA16 = 0.0D0
  REAL(r64) :: CoeffA17 = 0.0D0
  REAL(r64) :: CoeffA18 = 0.0D0
  REAL(r64) :: CoeffA19 = 0.0D0
  REAL(r64) :: CoeffA20 = 0.0D0
  REAL(r64) :: CoeffA21 = 0.0D0
  REAL(r64) :: CoeffA22 = 0.0D0
  REAL(r64) :: CoeffA23 = 0.0D0
  REAL(r64) :: CoeffA24 = 0.0D0
  REAL(r64) :: CoeffA25 = 0.0D0
  REAL(r64) :: CoeffA26 = 0.0D0
END TYPE TriQuadraticCurveDataStruct

Type TableDataStruct
  REAL(r64) :: NormalPoint = 1.0D0
  REAL(r64), DIMENSION(:), ALLOCATABLE :: X1
  REAL(r64), DIMENSION(:), ALLOCATABLE :: X2
  REAL(r64), DIMENSION(:), ALLOCATABLE :: Y
End Type TableDataStruct

Type PerfCurveTableDataStruct
  REAL(r64), DIMENSION(:), ALLOCATABLE :: X1
  REAL(r64), DIMENSION(:), ALLOCATABLE :: X2
  REAL(r64), DIMENSION(:,:), ALLOCATABLE :: Y
End Type PerfCurveTableDataStruct

TYPE PerfomanceCurveData
  CHARACTER(len=MaxNameLength) :: Name         =Blank ! Curve Name
  INTEGER                      :: ObjectType   =0     ! Curve object type (e.g., integer for Curve:Linear above)
  INTEGER                      :: CurveType    =0     ! Curve type (see parameter definitions above)
  INTEGER                      :: InterpolationType=0     ! table interpolation method
  INTEGER                      :: DataFormat   =0     ! format of tabular data
  INTEGER                      :: TableIndex   =0     ! Index to tablular data (0 if a standard curve object)
  INTEGER                      :: TableVariables=0    ! Number of independent variables (0 if a standard curve object)
  INTEGER                      :: NumIVLowErrorIndex=0   ! Index to table object error message for too few IV's
  INTEGER                      :: NumIVHighErrorIndex=0   ! Index to table object error message for too many IV's
  INTEGER                      :: X1SortOrder=1       ! sort order for table data for X1
  INTEGER                      :: X2SortOrder=1       ! sort order for table data for X2
  REAL(r64)                    :: Coeff1       =0.0D0   ! constant coefficient
  REAL(r64)                    :: Coeff2       =0.0D0   ! linear coeff (1st independent variable)
  REAL(r64)                    :: Coeff3       =0.0D0   ! quadratic coeff (1st independent variable)
  REAL(r64)                    :: Coeff4       =0.0D0   ! linear coeff (2nd ind var) or cubic coeff
  REAL(r64)                    :: Coeff5       =0.0D0   ! quadratic coeff (2nd independent variable)
  REAL(r64)                    :: Coeff6       =0.0D0   ! cross coeff (1st & 2nd ind var)
  REAL(r64)                    :: Coeff7       =0.0D0   ! cubic coeff for bicubic (1st ind var)
  REAL(r64)                    :: Coeff8       =0.0D0   ! cubic coeff for bicubic (2nd ind var)
  REAL(r64)                    :: Coeff9       =0.0D0   ! cross coeff for bicubic (1st quadratic & 2nd linear)
  REAL(r64)                    :: Coeff10      =0.0D0   ! cross coeff for bicubic (1st linear & 2nd quadratic)
  REAL(r64)                    :: Var1Max      =0.0D0   ! maximum of 1st independent variable
  REAL(r64)                    :: Var1Min      =0.0D0   ! minimum of 1st independent variable
  REAL(r64)                    :: Var2Max      =0.0D0   ! maximum of 2nd independent variable
  REAL(r64)                    :: Var2Min      =0.0D0   ! minimum of 2nd independent variable
  REAL(r64)                    :: Var3Max      =0.0D0   ! maximum of 3rd independent variable
  REAL(r64)                    :: Var3Min      =0.0D0   ! minimum of 3rd independent variable
  REAL(r64)                    :: Var4Max      =0.0D0   ! maximum of 4th independent variable
  REAL(r64)                    :: Var4Min      =0.0D0   ! minimum of 4th independent variable
  REAL(r64)                    :: Var5Max      =0.0D0   ! maximum of 5th independent variable
  REAL(r64)                    :: Var5Min      =0.0D0   ! minimum of 5th independent variable
  REAL(r64)                    :: CurveMin     =0.0D0   ! minimum value of curve output
  REAL(r64)                    :: CurveMax     =0.0D0   ! maximum value of curve output
  LOGICAL                      :: CurveMinPresent = .FALSE. ! If TRUE, then cap minimum curve output
  LOGICAL                      :: CurveMaxPresent = .FALSE. ! if TRUE, then cap maximum curve output
  TYPE(TriQuadraticCurveDataStruct),DIMENSION(:), ALLOCATABLE :: Tri2ndOrder   ! structure for triquadratic curve data
  LOGICAL                      :: EMSOverrideOn = .FALSE. ! if TRUE, then EMS is calling to override curve value
  REAL(r64)                    :: EMSOverrideCurveValue = 0.0D0 ! Value of curve result EMS is directing to use
! report variables
  REAL(r64)                    :: CurveOutput = 0.0D0     ! curve output or result
  REAL(r64)                    :: CurveInput1 = 0.0D0     ! curve input #1 (e.g., x or X1 variable)
  REAL(r64)                    :: CurveInput2 = 0.0D0     ! curve input #1 (e.g., y or X2 variable)
  REAL(r64)                    :: CurveInput3 = 0.0D0     ! curve input #1 (e.g., z or X3 variable)
  REAL(r64)                    :: CurveInput4 = 0.0D0     ! curve input #1 (e.g., X4 variable)
  REAL(r64)                    :: CurveInput5 = 0.0D0     ! curve input #1 (e.g., X5 variable)
END TYPE PerfomanceCurveData

TYPE TableLookupData
  INTEGER                      :: NumIndependentVars =0 ! Curve type (see parameter definitions above)
  INTEGER                      :: InterpolationOrder =0 ! number of points to interpolate (table data only)
  INTEGER                      :: NumX1Vars    =0     ! Number of variables for independent variable #1
  REAL(r64), DIMENSION(:), ALLOCATABLE :: X1Var
  INTEGER                      :: NumX2Vars    =0     ! Number of variables for independent variable #2
  REAL(r64), DIMENSION(:), ALLOCATABLE :: X2Var
  INTEGER                      :: NumX3Vars    =0     ! Number of variables for independent variable #3
  REAL(r64), DIMENSION(:), ALLOCATABLE :: X3Var
  INTEGER                      :: NumX4Vars    =0     ! Number of variables for independent variable #4
  REAL(r64), DIMENSION(:), ALLOCATABLE :: X4Var
  INTEGER                      :: NumX5Vars    =0     ! Number of variables for independent variable #5
  REAL(r64), DIMENSION(:), ALLOCATABLE :: X5Var
  REAL(r64), DIMENSION(:,:,:,:,:), ALLOCATABLE :: TableLookupZData
END TYPE TableLookupData

  ! MODULE VARIABLE DECLARATIONS:
TYPE(PerfomanceCurveData), ALLOCATABLE, DIMENSION(:) :: PerfCurve
TYPE(PerfCurveTableDataStruct),DIMENSION(:), ALLOCATABLE :: PerfCurveTableData
TYPE(TableDataStruct),DIMENSION(:), ALLOCATABLE :: TableData
TYPE(TableDataStruct),DIMENSION(:), ALLOCATABLE :: TempTableData
TYPE(TableDataStruct),DIMENSION(:), ALLOCATABLE :: Temp2TableData
TYPE(TableLookupData),DIMENSION(:), ALLOCATABLE :: TableLookup

INTEGER                                               :: NumCurves
LOGICAL  :: GetCurvesInputFlag = .true.  ! First time, input is "gotten"

  ! SUBROUTINE SPECIFICATIONS FOR MODULE
PUBLIC ResetPerformanceCurveOutput
PUBLIC CurveValue
PRIVATE GetCurveInput
PRIVATE PerformanceCurveObject
PRIVATE PerformanceTableObject
PRIVATE TableLookupObject
PRIVATE SolveRegression
PRIVATE Interpolate_Lagrange
PRIVATE ReadTableData
PUBLIC GetCurveIndex
PUBLIC GetCurveCheck
PUBLIC GetCurveType
PUBLIC GetCurveMinMaxValues
PUBLIC SetCurveOutputMinMaxValues
PUBLIC GetCurveName
PUBLIC InitCurveReporting
PUBLIC GetPressureCurveTypeAndIndex
PUBLIC PressureCurveValue
PUBLIC GetCurveObjectTypeNum

CONTAINS

SUBROUTINE ResetPerformanceCurveOutput
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Raustad, FSEC
          !       DATE WRITTEN   August 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na
          !
          ! PURPOSE OF THIS SUBROUTINE:
          ! Reset curve outputs prior to simulating air loops, plant loops, etc.
          ! This allows the report variable for curve/table objects to show an inactive state.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataLoopNode, ONLY: SensedNodeFlagValue

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          ! na

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER :: CurveIndex

  DO CurveIndex = 1, NumCurves
    PerfCurve(CurveIndex)%CurveOutput = SensedNodeFlagValue
    PerfCurve(CurveIndex)%CurveInput1 = SensedNodeFlagValue
    PerfCurve(CurveIndex)%CurveInput2 = SensedNodeFlagValue
    PerfCurve(CurveIndex)%CurveInput3 = SensedNodeFlagValue
    PerfCurve(CurveIndex)%CurveInput4 = SensedNodeFlagValue
    PerfCurve(CurveIndex)%CurveInput5 = SensedNodeFlagValue
  END DO

  RETURN
END SUBROUTINE ResetPerformanceCurveOutput

REAL(r64) FUNCTION CurveValue(CurveIndex,Var1,Var2,Var3, Var4, Var5)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Richard Raustad, FSEC
          !       DATE WRITTEN   May 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! Given the curve index and the values of 1 or 2 independent variables,
          ! calls the curve or table routine to return the value of an equipment performance curve or table.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataGlobals,    ONLY: BeginEnvrnFlag
  USE DataInterfaces, ONLY:ShowFatalError

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER,   INTENT (IN)           :: CurveIndex  ! index of curve in curve array
  REAL(r64), INTENT (IN)           :: Var1        ! 1st independent variable
  REAL(r64), INTENT (IN), OPTIONAL :: Var2        ! 2nd independent variable
  REAL(r64), INTENT (IN), OPTIONAL :: Var3        ! 3rd independent variable
  REAL(r64), INTENT (IN), OPTIONAL :: Var4        ! 4th independent variable
  REAL(r64), INTENT (IN), OPTIONAL :: Var5        ! 5th independent variable

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
LOGICAL, SAVE :: MyBeginTimeStepFlag

! need to be careful on where and how resetting curve outputs to some "iactive value" is done
! EMS can intercept curves and modify output
IF(BeginEnvrnFlag .AND. MyBeginTimeStepFlag)THEN
  CALL ResetPerformanceCurveOutput
  MyBeginTimeStepFlag = .FALSE.
END IF

IF(.NOT. BeginEnvrnFlag)THEN
  MyBeginTimeStepFlag = .TRUE.
END IF

IF ((CurveIndex <= 0) .OR. (CurveIndex > NumCurves)) THEN
  CALL ShowFatalError('CurveValue: Invalid curve passed.')
ENDIF

SELECT CASE(PerfCurve(CurveIndex)%InterpolationType)
  CASE(EvaluateCurveToLimits)
    CurveValue = PerformanceCurveObject(CurveIndex,Var1,Var2,Var3)
  CASE(LinearInterpolationOfTable)
    CurveValue = PerformanceTableObject(CurveIndex,Var1,Var2,Var3)
  CASE(LagrangeInterpolationLinearExtrapolation)
    CurveValue = TableLookupObject(CurveIndex,Var1,Var2,Var3,Var4,Var5)
  CASE DEFAULT
    CALL ShowFatalError('CurveValue: Invalid Interpolation Type')
END SELECT

IF (PerfCurve(CurveIndex)%EMSOverrideOn) CurveValue = PerfCurve(CurveIndex)%EMSOverrideCurveValue

PerfCurve(CurveIndex)%CurveOutput = CurveValue
PerfCurve(CurveIndex)%CurveInput1 = Var1
IF(PRESENT(Var2))PerfCurve(CurveIndex)%CurveInput2 = Var2
IF(PRESENT(Var3))PerfCurve(CurveIndex)%CurveInput3 = Var3
IF(PRESENT(Var4))PerfCurve(CurveIndex)%CurveInput4 = Var4
IF(PRESENT(Var5))PerfCurve(CurveIndex)%CurveInput5 = Var5

RETURN

END FUNCTION CurveValue

SUBROUTINE GetCurveInput

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   May 2000
          !       MODIFIED       January 2006, Rick Strand, added a curve type (quadratic-linear)
          !                      July 2006, L. Gu, added a curve type (bicubic)
          !                      July 2006, BG added triquadratic.
          !                      April 2008, LL Added Linear Curve; July 2008, restructure for easier renaming
          !                      Feb 2009, R. Raustad - FSEC, added exponent curve
          !                      22Aug2010 Craig Wray, added new curves for fan component model:
          !                          FanPressureRise, ExponentialSkewNormal, Sigmoid, RectangularHyperbola1,
          !                          RectangularHyperbola2, ExponentialDecay
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Obtains input data for EnergyPlus equipment performance curves

          ! METHODOLOGY EMPLOYED:
          ! Uses "Get" routines to read in data.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: GetNumObjectsFound, GetObjectItem, VerifyName, GetObjectDefMaxArgs, FindItemInList,MakeUPPERcase
  USE DataIPShortCuts  ! Data for field names, blank numerics
  USE General, ONLY: RoundSigDigits
!  USE DataGlobals, ONLY: DisplayExtraWarnings, OutputFileInits

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
INTEGER :: NumBiquad     ! Number of biquadratic curve objects in the input data file
INTEGER :: NumCubic      ! Number of cubic curve objects in the input data file
INTEGER :: NumQuartic    ! Number of quartic (4th order polynomial) objects in the input data file
INTEGER :: NumQuad       ! Number of quadratic curve objects in the input data file
INTEGER :: NumQuadLinear ! Number of quadratic linear curve objects in the input data file
INTEGER :: NumQLinear    ! Number of quad linear curve objects in the input data file
INTEGER :: NumLinear     ! Number of linear curve objects in the input data file
INTEGER :: NumBicubic    ! Number of bicubic curve objects in the input data file
INTEGER :: NumTriQuad    ! Number of triquadratic curve objects in the input file
INTEGER :: NumExponent   ! Number of exponent curve objects in the input file
INTEGER :: NumOneVarTab  ! Number of one variable table objects in the input file
INTEGER :: NumTwoVarTab  ! Number of two variable table objects in the input file
INTEGER :: NumMultVarLookup ! Number of multivariable tables
INTEGER :: NumLookupTables ! total number of one, two, and multivariable tables
INTEGER :: NumFanPressRise ! cpw22Aug2010 Number of fan pressure rise curve objects in the input file
INTEGER :: NumExpSkewNorm  ! cpw22Aug2010 Number of exponential skew normal curve objects in the input file
INTEGER :: NumSigmoid      ! cpw22Aug2010 Number of sigmoid curve objects in the input file
INTEGER :: NumRectHyper1   ! cpw22Aug2010 Number of rectangular hyperbola Type 1 curve objects in the input file
INTEGER :: NumRectHyper2   ! cpw22Aug2010 Number of rectangular hyperbola Type 2 curve objects in the input file
INTEGER :: NumExpDecay     ! cpw22Aug2010 Number of exponential decay curve objects in the input file
INTEGER :: NumDoubleExpDecay     ! ykt July 2011
INTEGER :: NumTables     ! Total tables in the input file
INTEGER :: CurveIndex ! do loop index
INTEGER :: CurveNum   ! current curve number
CHARACTER(len=MaxNameLength), &
                   DIMENSION(13) :: Alphas  ! Alpha items for object
REAL(r64), DIMENSION(10000)        :: Numbers ! Numeric items for object
INTEGER                         :: NumAlphas  ! Number of Alphas for each GetObjectItem call
INTEGER                         :: NumNumbers ! Number of Numbers for each GetObjectItem call
INTEGER                         :: IOStatus   ! Used in GetObjectItem
LOGICAL                         :: ErrorsFound=.false.  ! Set to true if errors in input, fatal at end of routine
LOGICAL                         :: IsNotOK              ! Flag to verify name
LOGICAL                         :: IsBlank              ! Flag for blank name
CHARACTER(len=MaxNameLength) :: CurrentModuleObject  ! for ease in renaming.
INTEGER   :: MaxTableNums=0     ! Maximum number of numeric input fields in Tables
INTEGER   :: MaxTableData=0     ! Maximum number of numeric input field pairs in Tables
INTEGER   :: TotalArgs=0        ! Total number of alpha and numeric arguments (max) for a
                                !   certain object in the input file
INTEGER   :: TableNum=0         ! Index to TableData structure
INTEGER   :: TableDataIndex=0   ! Loop counter for table data
INTEGER   :: NumTableEntries=0  ! Number of data pairs in table data
INTEGER   :: NumXVar
INTEGER   :: NumX2Var
REAL(r64), ALLOCATABLE, DIMENSION(:):: XVar
REAL(r64), ALLOCATABLE, DIMENSION(:):: X2Var
INTEGER :: VarIndex
INTEGER :: TempVarIndex
INTEGER :: TempVarIndex1
REAL(r64) :: MinTableData
REAL(r64) :: MaxTableDataValue
INTEGER   :: NextXVar
LOGICAL :: FoundNewData
Real(r64), ALLOCATABLE, DIMENSION(:) :: TempArray1
Real(r64), ALLOCATABLE, DIMENSION(:) :: TempArray2
Real(r64), ALLOCATABLE, DIMENSION(:) :: TempArray3

CHARACTER(len=MaxNameLength) :: FileName  ! name of external table data file
INTEGER,EXTERNAL   :: GetNewUnitNumber
LOGICAL            :: ReadFromFile       ! True if external data file exists
INTEGER :: CurveFound

! Find the number of each type of curve (note: Current Module object not used here, must rename manually)

NumBiQuad     = GetNumObjectsFound('Curve:Biquadratic')
NumCubic      = GetNumObjectsFound('Curve:Cubic')
NumQuartic    = GetNumObjectsFound('Curve:Quartic')
NumQuad       = GetNumObjectsFound('Curve:Quadratic')
NumQLinear    = GetNumObjectsFound('Curve:QuadLinear')
NumQuadLinear = GetNumObjectsFound('Curve:QuadraticLinear')
NumLinear     = GetNumObjectsFound('Curve:Linear')
NumBicubic    = GetNumObjectsFound('Curve:Bicubic')
NumTriQuad    = GetNumObjectsFound('Curve:Triquadratic')
NumExponent   = GetNumObjectsFound('Curve:Exponent')
NumMultVarLookup = GetNumObjectsFound('Table:MultiVariableLookup')
NumFanPressRise = GetNumObjectsFound('Curve:FanPressureRise')       !cpw22Aug2010
NumExpSkewNorm  = GetNumObjectsFound('Curve:ExponentialSkewNormal') !cpw22Aug2010
NumSigmoid      = GetNumObjectsFound('Curve:Sigmoid')               !cpw22Aug2010
NumRectHyper1   = GetNumObjectsFound('Curve:RectangularHyperbola1') !cpw22Aug2010
NumRectHyper2   = GetNumObjectsFound('Curve:RectangularHyperbola2') !cpw22Aug2010
NumExpDecay     = GetNumObjectsFound('Curve:ExponentialDecay')      !cpw22Aug2010
NumDoubleExpDecay=GetNumObjectsFound('Curve:DoubleExponentialDecay') !ykt July 2011
NumOneVarTab  = GetNumObjectsFound('Table:OneIndependentVariable')
NumTwoVarTab  = GetNumObjectsFound('Table:TwoIndependentVariables')

NumCurves     = NumBiQuad + NumCubic + NumQuad + NumQuadLinear + NumLinear + NumBicubic &
              + NumTriQuad + NumExponent + NumQuartic + NumOneVarTab + NumTwoVarTab + NumMultVarLookup &
              + NumFanPressRise + NumExpSkewNorm + NumSigmoid + NumRectHyper1 & !cpw22Aug2010
              + NumRectHyper2 + NumExpDecay + NumDoubleExpDecay + NumQLinear

! intermediate count for one and two variable performance tables
NumTables     = NumOneVarTab + NumTwoVarTab
! final count for all tables
NumLookupTables = NumOneVarTab + NumTwoVarTab + NumMultVarLookup
IF(NumLookupTables .GT. 0)ALLOCATE(TableLookup(NumLookupTables))

IF(NumOneVarTab .GT. 0)THEN
  CALL GetObjectDefMaxArgs('Table:OneIndependentVariable',TotalArgs,NumAlphas,NumNumbers)
  MaxTableNums=MAX(MaxTableNums,NumNumbers)
  MaxTableData=MAX(MaxTableData,MaxTableNums)
END IF
IF(NumTwoVarTab .GT. 0)THEN
  CALL GetObjectDefMaxArgs('Table:TwoIndependentVariables',TotalArgs,NumAlphas,NumNumbers)
  MaxTableNums=MAX(MaxTableNums,NumNumbers)
  MaxTableData=MAX(MaxTableData,MaxTableNums)
END IF

! allocate the data structure
ALLOCATE(PerfCurve(NumCurves))
ALLOCATE(PerfCurveTableData(NumLookupTables))
ALLOCATE(TableData(NumLookupTables))
ALLOCATE(TempTableData(NumTables))
ALLOCATE(Temp2TableData(NumTables))
! initialize the array

CurveNum=0
! Loop over biquadratic curves and load data
CurrentModuleObject='Curve:Biquadratic'
DO CurveIndex=1,NumBiQuad
  CALL GetObjectItem(CurrentModuleObject,CurveIndex,Alphas,NumAlphas,Numbers,NumNumbers,IOStatus,     &
                NumBlank=lNumericFieldBlanks,AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
  CurveNum = CurveNum+1
  IsNotOK=.FALSE.
  IsBlank=.FALSE.
  CALL VerifyName(Alphas(1),PerfCurve%Name,CurveNum-1,IsNotOK,IsBlank,TRIM(CurrentModuleObject)//' Name')
  IF (IsNotOK) THEN
    ErrorsFound=.true.
    IF (IsBlank) Alphas(1)='xxxxx'
  ENDIF
  ! Need to verify that this name isn't used in Pressure Curves as well.
  IF (NumPressureCurves > 0) THEN
    CurveFound=FindItemInList(Alphas(1),PressureCurve%Name,NumPressureCurves)
    IF (CurveFound /= 0) THEN
      CALL ShowSevereError('GetCurveInput: '//trim(CurrentModuleObject)//'="'//trim(Alphas(1))//'", duplicate curve name.')
      CALL ShowContinueError('...Curve name duplicates one of the Pressure Curves. Names must be unique across all curves.')
      ErrorsFound=.true.
    ENDIF
  ENDIF
  ! could add checks for blank numeric fields, and use field names for errors.
  PerfCurve(CurveNum)%Name = Alphas(1)
  PerfCurve(CurveNum)%CurveType = Biquadratic
  PerfCurve(CurveNum)%ObjectType = CurveType_Biquadratic
  PerfCurve(CurveNum)%InterpolationType = EVALUATECURVETOLIMITS
  PerfCurve(CurveNum)%Coeff1 = Numbers(1)
  PerfCurve(CurveNum)%Coeff2 = Numbers(2)
  PerfCurve(CurveNum)%Coeff3 = Numbers(3)
  PerfCurve(CurveNum)%Coeff4 = Numbers(4)
  PerfCurve(CurveNum)%Coeff5 = Numbers(5)
  PerfCurve(CurveNum)%Coeff6 = Numbers(6)
  PerfCurve(CurveNum)%Var1Min = Numbers(7)
  PerfCurve(CurveNum)%Var1Max = Numbers(8)
  PerfCurve(CurveNum)%Var2Min = Numbers(9)
  PerfCurve(CurveNum)%Var2Max = Numbers(10)
  IF(NumNumbers > 10 .AND. .NOT. lNumericFieldBlanks(11))THEN
    PerfCurve(CurveNum)%CurveMin        = Numbers(11)
    PerfCurve(CurveNum)%CurveMinPresent = .TRUE.
  END IF
  IF(NumNumbers > 11 .AND. .NOT. lNumericFieldBlanks(12))THEN
    PerfCurve(CurveNum)%CurveMax        = Numbers(12)
    PerfCurve(CurveNum)%CurveMaxPresent = .TRUE.
  END IF

  IF (Numbers(7) > Numbers(8)) THEN  ! error
    CALL ShowSevereError('GetCurveInput: For '//TRIM(CurrentModuleObject)//': '//TRIM(Alphas(1)))
    CALL ShowContinueError(TRIM(cNumericFieldNames(7))//' ['//TRIM(RoundSigDigits(Numbers(7),2))//'] > '//  &
       TRIM(cNumericFieldNames(8))//' ['//TRIM(RoundSigDigits(Numbers(8),2))//']')
    ErrorsFound=.true.
  ENDIF
  IF (Numbers(9) > Numbers(10)) THEN  ! error
    CALL ShowSevereError('GetCurveInput: For '//TRIM(CurrentModuleObject)//': '//TRIM(Alphas(1)))
    CALL ShowContinueError(TRIM(cNumericFieldNames(9))//' ['//TRIM(RoundSigDigits(Numbers(9),2))//'] > '//  &
       TRIM(cNumericFieldNames(10))//' ['//TRIM(RoundSigDigits(Numbers(10),2))//']')
    ErrorsFound=.true.
  ENDIF
  IF (NumAlphas .GE. 2) THEN
    IF (.NOT. IsCurveInputTypeValid(Alphas(2))) THEN
      CALL ShowWarningError('In '//TRIM(CurrentModuleObject)//' named ' // TRIM(Alphas(1)) //   &
                            ' the Input Unit Type for X is invalid.')
    END IF
  END IF
  IF (NumAlphas .GE. 3) THEN
    IF (.NOT. IsCurveInputTypeValid(Alphas(3))) THEN
      CALL ShowWarningError('In '//TRIM(CurrentModuleObject)//' named ' // TRIM(Alphas(1)) //   &
                            ' the Input Unit Type for Y is invalid.')
    END IF
  END IF
  IF (NumAlphas .GE. 4) THEN
    IF (.NOT. IsCurveOutputTypeValid(Alphas(4))) THEN
      CALL ShowWarningError('In '//TRIM(CurrentModuleObject)//' named ' // TRIM(Alphas(1)) //   &
                            ' the Output Unit Type is invalid.')
    END IF
  END IF

END DO

! Loop over cubic curves and load data
CurrentModuleObject='Curve:Cubic'
DO CurveIndex=1,NumCubic
  CALL GetObjectItem(CurrentModuleObject,CurveIndex,Alphas,NumAlphas,Numbers,NumNumbers,IOStatus,     &
                NumBlank=lNumericFieldBlanks,AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
  CurveNum = CurveNum+1
  IsNotOK=.FALSE.
  IsBlank=.FALSE.
  CALL VerifyName(Alphas(1),PerfCurve%Name,CurveNum-1,IsNotOK,IsBlank,TRIM(CurrentModuleObject)//' Name')
  IF (IsNotOK) THEN
    ErrorsFound=.true.
    IF (IsBlank) Alphas(1)='xxxxx'
  ENDIF
  ! Need to verify that this name isn't used in Pressure Curves as well.
  IF (NumPressureCurves > 0) THEN
    CurveFound=FindItemInList(Alphas(1),PressureCurve%Name,NumPressureCurves)
    IF (CurveFound /= 0) THEN
      CALL ShowSevereError('GetCurveInput: '//trim(CurrentModuleObject)//'="'//trim(Alphas(1))//'", duplicate curve name.')
      CALL ShowContinueError('...Curve name duplicates one of the Pressure Curves. Names must be unique across all curves.')
      ErrorsFound=.true.
    ENDIF
  ENDIF
  PerfCurve(CurveNum)%Name = Alphas(1)
  PerfCurve(CurveNum)%CurveType = Cubic
  PerfCurve(CurveNum)%ObjectType = CurveType_Cubic
  PerfCurve(CurveNum)%InterpolationType = EVALUATECURVETOLIMITS
  PerfCurve(CurveNum)%Coeff1 = Numbers(1)
  PerfCurve(CurveNum)%Coeff2 = Numbers(2)
  PerfCurve(CurveNum)%Coeff3 = Numbers(3)
  PerfCurve(CurveNum)%Coeff4 = Numbers(4)
  PerfCurve(CurveNum)%Var1Min = Numbers(5)
  PerfCurve(CurveNum)%Var1Max = Numbers(6)
  IF(NumNumbers > 6 .AND. .NOT. lNumericFieldBlanks(7))THEN
    PerfCurve(CurveNum)%CurveMin        = Numbers(7)
    PerfCurve(CurveNum)%CurveMinPresent = .TRUE.
  END IF
  IF(NumNumbers > 7 .AND. .NOT. lNumericFieldBlanks(8))THEN
    PerfCurve(CurveNum)%CurveMax        = Numbers(8)
    PerfCurve(CurveNum)%CurveMaxPresent = .TRUE.
  END IF

  IF (Numbers(5) > Numbers(6)) THEN  ! error
    CALL ShowSevereError('GetCurveInput: For '//TRIM(CurrentModuleObject)//': '//TRIM(Alphas(1)))
    CALL ShowContinueError(TRIM(cNumericFieldNames(5))//'['//TRIM(RoundSigDigits(Numbers(5),2))//'] > '//  &
       TRIM(cNumericFieldNames(6))//' ['//TRIM(RoundSigDigits(Numbers(6),2))//']')
    ErrorsFound=.true.
  ENDIF
  IF (NumAlphas .GE. 2) THEN
    IF (.NOT. IsCurveInputTypeValid(Alphas(2))) THEN
      CALL ShowWarningError('In '//TRIM(CurrentModuleObject)//' named ' // TRIM(Alphas(1)) //   &
                            ' the Input Unit Type for X is invalid.')
    END IF
  END IF
  IF (NumAlphas .GE. 3) THEN
    IF (.NOT. IsCurveOutputTypeValid(Alphas(3))) THEN
      CALL ShowWarningError('In '//TRIM(CurrentModuleObject)//' named ' // TRIM(Alphas(1)) //   &
                            ' the Output Unit Type is invalid.')
    END IF
  END IF

END DO

! Loop over quadrinomial curves and load data
CurrentModuleObject='Curve:Quartic'
DO CurveIndex=1,NumQuartic
  CALL GetObjectItem(CurrentModuleObject,CurveIndex,Alphas,NumAlphas,Numbers,NumNumbers,IOStatus,     &
                NumBlank=lNumericFieldBlanks,AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
  CurveNum = CurveNum+1
  IsNotOK=.FALSE.
  IsBlank=.FALSE.
  CALL VerifyName(Alphas(1),PerfCurve%Name,CurveNum-1,IsNotOK,IsBlank,TRIM(CurrentModuleObject)//' Name')
  IF (IsNotOK) THEN
    ErrorsFound=.true.
    IF (IsBlank) Alphas(1)='xxxxx'
  ENDIF
  ! Need to verify that this name isn't used in Pressure Curves as well.
  IF (NumPressureCurves > 0) THEN
    CurveFound=FindItemInList(Alphas(1),PressureCurve%Name,NumPressureCurves)
    IF (CurveFound /= 0) THEN
      CALL ShowSevereError('GetCurveInput: '//trim(CurrentModuleObject)//'="'//trim(Alphas(1))//'", duplicate curve name.')
      CALL ShowContinueError('...Curve name duplicates one of the Pressure Curves. Names must be unique across all curves.')
      ErrorsFound=.true.
    ENDIF
  ENDIF
  PerfCurve(CurveNum)%Name = Alphas(1)
  PerfCurve(CurveNum)%CurveType = Quartic
  PerfCurve(CurveNum)%ObjectType = CurveType_Quartic
  PerfCurve(CurveNum)%InterpolationType = EVALUATECURVETOLIMITS
  PerfCurve(CurveNum)%Coeff1 = Numbers(1)
  PerfCurve(CurveNum)%Coeff2 = Numbers(2)
  PerfCurve(CurveNum)%Coeff3 = Numbers(3)
  PerfCurve(CurveNum)%Coeff4 = Numbers(4)
  PerfCurve(CurveNum)%Coeff5 = Numbers(5)
  PerfCurve(CurveNum)%Var1Min = Numbers(6)
  PerfCurve(CurveNum)%Var1Max = Numbers(7)
  IF(NumNumbers > 7 .AND. .NOT. lNumericFieldBlanks(8))THEN
    PerfCurve(CurveNum)%CurveMin        = Numbers(8)
    PerfCurve(CurveNum)%CurveMinPresent = .TRUE.
  END IF
  IF(NumNumbers > 8 .AND. .NOT. lNumericFieldBlanks(9))THEN
    PerfCurve(CurveNum)%CurveMax        = Numbers(9)
    PerfCurve(CurveNum)%CurveMaxPresent = .TRUE.
  END IF

  IF (Numbers(6) > Numbers(7)) THEN  ! error
    CALL ShowSevereError('GetCurveInput: For '//TRIM(CurrentModuleObject)//': '//TRIM(Alphas(1)))
    CALL ShowContinueError(TRIM(cNumericFieldNames(6))//'['//TRIM(RoundSigDigits(Numbers(6),2))//'] > '//  &
       TRIM(cNumericFieldNames(7))//' ['//TRIM(RoundSigDigits(Numbers(7),2))//']')
    ErrorsFound=.true.
  ENDIF
  IF (NumAlphas .GE. 2) THEN
    IF (.NOT. IsCurveInputTypeValid(Alphas(2))) THEN
      CALL ShowWarningError('In '//TRIM(CurrentModuleObject)//' named ' // TRIM(Alphas(1)) //   &
                            ' the Input Unit Type for X is invalid.')
    END IF
  END IF
  IF (NumAlphas .GE. 3) THEN
    IF (.NOT. IsCurveOutputTypeValid(Alphas(3))) THEN
      CALL ShowWarningError('In '//TRIM(CurrentModuleObject)//' named ' // TRIM(Alphas(1)) //   &
                            ' the Output Unit Type is invalid.')
    END IF
  END IF

END DO

! Loop over quadratic curves and load data
CurrentModuleObject='Curve:Quadratic'
DO CurveIndex=1,NumQuad
  CALL GetObjectItem(CurrentModuleObject,CurveIndex,Alphas,NumAlphas,Numbers,NumNumbers,IOStatus,     &
                NumBlank=lNumericFieldBlanks,AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
  CurveNum = CurveNum+1
  IsNotOK=.FALSE.
  IsBlank=.FALSE.
  CALL VerifyName(Alphas(1),PerfCurve%Name,CurveNum-1,IsNotOK,IsBlank,TRIM(CurrentModuleObject)//' Name')
  IF (IsNotOK) THEN
    ErrorsFound=.true.
    IF (IsBlank) Alphas(1)='xxxxx'
  ENDIF
  ! Need to verify that this name isn't used in Pressure Curves as well.
  IF (NumPressureCurves > 0) THEN
    CurveFound=FindItemInList(Alphas(1),PressureCurve%Name,NumPressureCurves)
    IF (CurveFound /= 0) THEN
      CALL ShowSevereError('GetCurveInput: '//trim(CurrentModuleObject)//'="'//trim(Alphas(1))//'", duplicate curve name.')
      CALL ShowContinueError('...Curve name duplicates one of the Pressure Curves. Names must be unique across all curves.')
      ErrorsFound=.true.
    ENDIF
  ENDIF
  PerfCurve(CurveNum)%Name = Alphas(1)
  PerfCurve(CurveNum)%CurveType = Quadratic
  PerfCurve(CurveNum)%ObjectType = CurveType_Quadratic
  PerfCurve(CurveNum)%InterpolationType = EVALUATECURVETOLIMITS
  PerfCurve(CurveNum)%Coeff1 = Numbers(1)
  PerfCurve(CurveNum)%Coeff2 = Numbers(2)
  PerfCurve(CurveNum)%Coeff3 = Numbers(3)
  PerfCurve(CurveNum)%Var1Min = Numbers(4)
  PerfCurve(CurveNum)%Var1Max = Numbers(5)
  IF(NumNumbers > 5 .AND. .NOT. lNumericFieldBlanks(6))THEN
    PerfCurve(CurveNum)%CurveMin        = Numbers(6)
    PerfCurve(CurveNum)%CurveMinPresent = .TRUE.
  END IF
  IF(NumNumbers > 6 .AND. .NOT. lNumericFieldBlanks(7))THEN
    PerfCurve(CurveNum)%CurveMax        = Numbers(7)
    PerfCurve(CurveNum)%CurveMaxPresent = .TRUE.
  END IF

  IF (Numbers(4) > Numbers(5)) THEN  ! error
    CALL ShowSevereError('GetCurveInput: For '//TRIM(CurrentModuleObject)//': '//TRIM(Alphas(1)))
    CALL ShowContinueError(TRIM(cNumericFieldNames(4))//' ['//TRIM(RoundSigDigits(Numbers(4),2))//'] > '//  &
       TRIM(cNumericFieldNames(5))//' ['//TRIM(RoundSigDigits(Numbers(5),2))//']')
    ErrorsFound=.true.
  ENDIF
  IF (NumAlphas .GE. 2) THEN
    IF (.NOT. IsCurveInputTypeValid(Alphas(2))) THEN
      CALL ShowWarningError('In '//TRIM(CurrentModuleObject)//' named ' // TRIM(Alphas(1)) //   &
                            ' the Input Unit Type for X is invalid.')
    END IF
  END IF
  IF (NumAlphas .GE. 3) THEN
    IF (.NOT. IsCurveOutputTypeValid(Alphas(3))) THEN
      CALL ShowWarningError('In '//TRIM(CurrentModuleObject)//' named ' // TRIM(Alphas(1)) //   &
                            ' the Output Unit Type is invalid.')
    END IF
  END IF

END DO

! Loop over quadratic-linear curves and load data
CurrentModuleObject='Curve:QuadraticLinear'
DO CurveIndex=1,NumQuadLinear
  CALL GetObjectItem(CurrentModuleObject,CurveIndex,Alphas,NumAlphas,Numbers,NumNumbers,IOStatus,     &
                NumBlank=lNumericFieldBlanks,AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
  CurveNum = CurveNum+1
  IsNotOK=.FALSE.
  IsBlank=.FALSE.
  CALL VerifyName(Alphas(1),PerfCurve%Name,CurveNum-1,IsNotOK,IsBlank,TRIM(CurrentModuleObject)//' Name')
  IF (IsNotOK) THEN
    ErrorsFound=.true.
    IF (IsBlank) Alphas(1)='xxxxx'
  ENDIF
  ! Need to verify that this name isn't used in Pressure Curves as well.
  IF (NumPressureCurves > 0) THEN
    CurveFound=FindItemInList(Alphas(1),PressureCurve%Name,NumPressureCurves)
    IF (CurveFound /= 0) THEN
      CALL ShowSevereError('GetCurveInput: '//trim(CurrentModuleObject)//'="'//trim(Alphas(1))//'", duplicate curve name.')
      CALL ShowContinueError('...Curve name duplicates one of the Pressure Curves. Names must be unique across all curves.')
      ErrorsFound=.true.
    ENDIF
  ENDIF
  PerfCurve(CurveNum)%Name      = Alphas(1)
  PerfCurve(CurveNum)%CurveType = QuadraticLinear
  PerfCurve(CurveNum)%ObjectType = CurveType_QuadraticLinear
  PerfCurve(CurveNum)%InterpolationType = EVALUATECURVETOLIMITS
  PerfCurve(CurveNum)%Coeff1    = Numbers(1)
  PerfCurve(CurveNum)%Coeff2    = Numbers(2)
  PerfCurve(CurveNum)%Coeff3    = Numbers(3)
  PerfCurve(CurveNum)%Coeff4    = Numbers(4)
  PerfCurve(CurveNum)%Coeff5    = Numbers(5)
  PerfCurve(CurveNum)%Coeff6    = Numbers(6)
  PerfCurve(CurveNum)%Var1Min   = Numbers(7)
  PerfCurve(CurveNum)%Var1Max   = Numbers(8)
  PerfCurve(CurveNum)%Var2Min   = Numbers(9)
  PerfCurve(CurveNum)%Var2Max   = Numbers(10)
  IF(NumNumbers > 10 .AND. .NOT. lNumericFieldBlanks(11))THEN
    PerfCurve(CurveNum)%CurveMin        = Numbers(11)
    PerfCurve(CurveNum)%CurveMinPresent = .TRUE.
  END IF
  IF(NumNumbers > 11 .AND. .NOT. lNumericFieldBlanks(12))THEN
    PerfCurve(CurveNum)%CurveMax        = Numbers(12)
    PerfCurve(CurveNum)%CurveMaxPresent = .TRUE.
  END IF

  IF (Numbers(7) > Numbers(8)) THEN  ! error
    CALL ShowSevereError('GetCurveInput: For '//TRIM(CurrentModuleObject)//': '//TRIM(Alphas(1)))
    CALL ShowContinueError(TRIM(cNumericFieldNames(7))//' ['//TRIM(RoundSigDigits(Numbers(7),2))//'] > '//  &
       TRIM(cNumericFieldNames(8))//' ['//TRIM(RoundSigDigits(Numbers(8),2))//']')
    ErrorsFound=.true.
  ENDIF
  IF (Numbers(9) > Numbers(10)) THEN  ! error
    CALL ShowSevereError('GetCurveInput: For '//TRIM(CurrentModuleObject)//': '//TRIM(Alphas(1)))
    CALL ShowContinueError(TRIM(cNumericFieldNames(9))//' ['//TRIM(RoundSigDigits(Numbers(9),2))//'] > '//  &
       TRIM(cNumericFieldNames(10))//' ['//TRIM(RoundSigDigits(Numbers(10),2))//']')
    ErrorsFound=.true.
  ENDIF
  IF (NumAlphas .GE. 2) THEN
    IF (.NOT. IsCurveInputTypeValid(Alphas(2))) THEN
      CALL ShowWarningError('In '//TRIM(CurrentModuleObject)//' named ' // TRIM(Alphas(1)) //   &
                            ' the Input Unit Type for X is invalid.')
    END IF
  END IF
  IF (NumAlphas .GE. 3) THEN
    IF (.NOT. IsCurveInputTypeValid(Alphas(3))) THEN
      CALL ShowWarningError('In '//TRIM(CurrentModuleObject)//' named ' // TRIM(Alphas(1)) //   &
                            ' the Input Unit Type for Y is invalid.')
    END IF
  END IF
  IF (NumAlphas .GE. 4) THEN
    IF (.NOT. IsCurveOutputTypeValid(Alphas(4))) THEN
      CALL ShowWarningError('In '//TRIM(CurrentModuleObject)//' named ' // TRIM(Alphas(1)) //   &
                            ' the Output Unit Type is invalid.')
    END IF
  END IF
END DO

! Loop over linear curves and load data
CurrentModuleObject='Curve:Linear'
DO CurveIndex=1,NumLinear
  CALL GetObjectItem(CurrentModuleObject,CurveIndex,Alphas,NumAlphas,Numbers,NumNumbers,IOStatus,     &
                NumBlank=lNumericFieldBlanks,AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
  CurveNum = CurveNum+1
  IsNotOK=.FALSE.
  IsBlank=.FALSE.
  CALL VerifyName(Alphas(1),PerfCurve%Name,CurveNum-1,IsNotOK,IsBlank,TRIM(CurrentModuleObject)//' Name')
  IF (IsNotOK) THEN
    ErrorsFound=.true.
    IF (IsBlank) Alphas(1)='xxxxx'
  ENDIF
  ! Need to verify that this name isn't used in Pressure Curves as well.
  IF (NumPressureCurves > 0) THEN
    CurveFound=FindItemInList(Alphas(1),PressureCurve%Name,NumPressureCurves)
    IF (CurveFound /= 0) THEN
      CALL ShowSevereError('GetCurveInput: '//trim(CurrentModuleObject)//'="'//trim(Alphas(1))//'", duplicate curve name.')
      CALL ShowContinueError('...Curve name duplicates one of the Pressure Curves. Names must be unique across all curves.')
      ErrorsFound=.true.
    ENDIF
  ENDIF
  PerfCurve(CurveNum)%Name      = Alphas(1)
  PerfCurve(CurveNum)%CurveType = Linear
  PerfCurve(CurveNum)%ObjectType = CurveType_Linear
  PerfCurve(CurveNum)%InterpolationType = EVALUATECURVETOLIMITS
  PerfCurve(CurveNum)%Coeff1    = Numbers(1)
  PerfCurve(CurveNum)%Coeff2    = Numbers(2)
  PerfCurve(CurveNum)%Var1Min   = Numbers(3)
  PerfCurve(CurveNum)%Var1Max   = Numbers(4)
  IF(NumNumbers > 4 .AND. .NOT. lNumericFieldBlanks(5))THEN
    PerfCurve(CurveNum)%CurveMin        = Numbers(5)
    PerfCurve(CurveNum)%CurveMinPresent = .TRUE.
  END IF
  IF(NumNumbers > 5 .AND. .NOT. lNumericFieldBlanks(6))THEN
    PerfCurve(CurveNum)%CurveMax        = Numbers(6)
    PerfCurve(CurveNum)%CurveMaxPresent = .TRUE.
  END IF

  IF (Numbers(3) > Numbers(4)) THEN  ! error
    CALL ShowSevereError('GetCurveInput: For '//TRIM(CurrentModuleObject)//': '//TRIM(Alphas(1)))
    CALL ShowContinueError(TRIM(cNumericFieldNames(3))//' ['//TRIM(RoundSigDigits(Numbers(3),2))//'] > '//  &
       TRIM(cNumericFieldNames(4))//' ['//TRIM(RoundSigDigits(Numbers(4),2))//']')
    ErrorsFound=.true.
  ENDIF
  IF (NumAlphas .GE. 2) THEN
    IF (.NOT. IsCurveInputTypeValid(Alphas(2))) THEN
      CALL ShowWarningError('In '//TRIM(CurrentModuleObject)//' named ' // TRIM(Alphas(1)) //   &
                            ' the Input Unit Type for X is invalid.')
    END IF
  END IF
  IF (NumAlphas .GE. 3) THEN
    IF (.NOT. IsCurveOutputTypeValid(Alphas(3))) THEN
      CALL ShowWarningError('In '//TRIM(CurrentModuleObject)//' named ' // TRIM(Alphas(1)) //   &
                            ' the Output Unit Type is invalid.')
    END IF
  END IF
END DO

! Loop over bicubic curves and load data
CurrentModuleObject='Curve:Bicubic'
DO CurveIndex=1,NumBicubic
  CALL GetObjectItem(CurrentModuleObject,CurveIndex,Alphas,NumAlphas,Numbers,NumNumbers,IOStatus,     &
                NumBlank=lNumericFieldBlanks,AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
  CurveNum = CurveNum+1
  IsNotOK=.FALSE.
  IsBlank=.FALSE.
  CALL VerifyName(Alphas(1),PerfCurve%Name,CurveNum-1,IsNotOK,IsBlank,TRIM(CurrentModuleObject)//' Name')
  IF (IsNotOK) THEN
    ErrorsFound=.true.
    IF (IsBlank) Alphas(1)='xxxxx'
  ENDIF
  ! Need to verify that this name isn't used in Pressure Curves as well.
  IF (NumPressureCurves > 0) THEN
    CurveFound=FindItemInList(Alphas(1),PressureCurve%Name,NumPressureCurves)
    IF (CurveFound /= 0) THEN
      CALL ShowSevereError('GetCurveInput: '//trim(CurrentModuleObject)//'="'//trim(Alphas(1))//'", duplicate curve name.')
      CALL ShowContinueError('...Curve name duplicates one of the Pressure Curves. Names must be unique across all curves.')
      ErrorsFound=.true.
    ENDIF
  ENDIF
  PerfCurve(CurveNum)%Name      = Alphas(1)
  PerfCurve(CurveNum)%CurveType = Bicubic
  PerfCurve(CurveNum)%ObjectType = CurveType_BiCubic
  PerfCurve(CurveNum)%InterpolationType = EVALUATECURVETOLIMITS
  PerfCurve(CurveNum)%Coeff1    = Numbers(1)
  PerfCurve(CurveNum)%Coeff2    = Numbers(2)
  PerfCurve(CurveNum)%Coeff3    = Numbers(3)
  PerfCurve(CurveNum)%Coeff4    = Numbers(4)
  PerfCurve(CurveNum)%Coeff5    = Numbers(5)
  PerfCurve(CurveNum)%Coeff6    = Numbers(6)
  PerfCurve(CurveNum)%Coeff7    = Numbers(7)
  PerfCurve(CurveNum)%Coeff8    = Numbers(8)
  PerfCurve(CurveNum)%Coeff9    = Numbers(9)
  PerfCurve(CurveNum)%Coeff10   = Numbers(10)
  PerfCurve(CurveNum)%Var1Min   = Numbers(11)
  PerfCurve(CurveNum)%Var1Max   = Numbers(12)
  PerfCurve(CurveNum)%Var2Min   = Numbers(13)
  PerfCurve(CurveNum)%Var2Max   = Numbers(14)
  IF(NumNumbers > 14 .AND. .NOT. lNumericFieldBlanks(15))THEN
    PerfCurve(CurveNum)%CurveMin        = Numbers(15)
    PerfCurve(CurveNum)%CurveMinPresent = .TRUE.
  END IF
  IF(NumNumbers > 15 .AND. .NOT. lNumericFieldBlanks(16))THEN
    PerfCurve(CurveNum)%CurveMax        = Numbers(16)
    PerfCurve(CurveNum)%CurveMaxPresent = .TRUE.
  END IF

  IF (Numbers(11) > Numbers(12)) THEN  ! error
    CALL ShowSevereError('GetCurveInput: For '//TRIM(CurrentModuleObject)//': '//TRIM(Alphas(1)))
    CALL ShowContinueError(TRIM(cNumericFieldNames(11))//' ['//TRIM(RoundSigDigits(Numbers(11),2))//'] > '//  &
       TRIM(cNumericFieldNames(12))//' ['//TRIM(RoundSigDigits(Numbers(12),2))//']')
    ErrorsFound=.true.
  ENDIF
  IF (Numbers(13) > Numbers(14)) THEN  ! error
    CALL ShowSevereError('GetCurveInput: For '//TRIM(CurrentModuleObject)//': '//TRIM(Alphas(1)))
    CALL ShowContinueError(TRIM(cNumericFieldNames(13))//' ['//TRIM(RoundSigDigits(Numbers(13),2))//'] > '//  &
       TRIM(cNumericFieldNames(14))//' ['//TRIM(RoundSigDigits(Numbers(14),2))//']')
    ErrorsFound=.true.
  ENDIF
  IF (NumAlphas .GE. 2) THEN
    IF (.NOT. IsCurveInputTypeValid(Alphas(2))) THEN
      CALL ShowWarningError('In '//TRIM(CurrentModuleObject)//' named ' // TRIM(Alphas(1)) //   &
                            ' the Input Unit Type for X is invalid.')
    END IF
  END IF
  IF (NumAlphas .GE. 3) THEN
    IF (.NOT. IsCurveInputTypeValid(Alphas(3))) THEN
      CALL ShowWarningError('In '//TRIM(CurrentModuleObject)//' named ' // TRIM(Alphas(1)) //   &
                            ' the Input Unit Type for Y is invalid.')
    END IF
  END IF
  IF (NumAlphas .GE. 4) THEN
    IF (.NOT. IsCurveOutputTypeValid(Alphas(4))) THEN
      CALL ShowWarningError('In '//TRIM(CurrentModuleObject)//' named ' // TRIM(Alphas(1)) //   &
                            ' the Output Unit Type is invalid.')
    END IF
  END IF
END DO

! Loop over Triquadratic curves and load data
CurrentModuleObject='Curve:Triquadratic'
DO CurveIndex=1,NumTriQuad
  CALL GetObjectItem(CurrentModuleObject,CurveIndex,Alphas,NumAlphas,Numbers,NumNumbers,IOStatus,     &
                NumBlank=lNumericFieldBlanks,AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
  CurveNum = CurveNum+1
  IsNotOK=.FALSE.
  IsBlank=.FALSE.
  CALL VerifyName(Alphas(1),PerfCurve%Name,CurveNum-1,IsNotOK,IsBlank,TRIM(CurrentModuleObject)//' Name')
  IF (IsNotOK) THEN
    ErrorsFound=.true.
    IF (IsBlank) Alphas(1)='xxxxx'
  ENDIF
  ! Need to verify that this name isn't used in Pressure Curves as well.
  IF (NumPressureCurves > 0) THEN
    CurveFound=FindItemInList(Alphas(1),PressureCurve%Name,NumPressureCurves)
    IF (CurveFound /= 0) THEN
      CALL ShowSevereError('GetCurveInput: '//trim(CurrentModuleObject)//'="'//trim(Alphas(1))//'", duplicate curve name.')
      CALL ShowContinueError('...Curve name duplicates one of the Pressure Curves. Names must be unique across all curves.')
      ErrorsFound=.true.
    ENDIF
  ENDIF
  PerfCurve(CurveNum)%Name = Alphas(1)
  PerfCurve(CurveNum)%CurveType = TriQuadratic
  PerfCurve(CurveNum)%ObjectType = CurveType_TriQuadratic
  PerfCurve(CurveNum)%InterpolationType = EVALUATECURVETOLIMITS
  Allocate(PerfCurve(CurveNum)%Tri2ndOrder(1))
  PerfCurve(CurveNum)%Tri2ndOrder%CoeffA0 = Numbers(1)
  PerfCurve(CurveNum)%Tri2ndOrder%CoeffA1 = Numbers(2)
  PerfCurve(CurveNum)%Tri2ndOrder%CoeffA2 = Numbers(3)
  PerfCurve(CurveNum)%Tri2ndOrder%CoeffA3 = Numbers(4)
  PerfCurve(CurveNum)%Tri2ndOrder%CoeffA4 = Numbers(5)
  PerfCurve(CurveNum)%Tri2ndOrder%CoeffA5 = Numbers(6)
  PerfCurve(CurveNum)%Tri2ndOrder%CoeffA6 = Numbers(7)
  PerfCurve(CurveNum)%Tri2ndOrder%CoeffA7 = Numbers(8)
  PerfCurve(CurveNum)%Tri2ndOrder%CoeffA8 = Numbers(9)
  PerfCurve(CurveNum)%Tri2ndOrder%CoeffA9 = Numbers(10)
  PerfCurve(CurveNum)%Tri2ndOrder%CoeffA10 = Numbers(11)
  PerfCurve(CurveNum)%Tri2ndOrder%CoeffA11 = Numbers(12)
  PerfCurve(CurveNum)%Tri2ndOrder%CoeffA12 = Numbers(13)
  PerfCurve(CurveNum)%Tri2ndOrder%CoeffA13 = Numbers(14)
  PerfCurve(CurveNum)%Tri2ndOrder%CoeffA14 = Numbers(15)
  PerfCurve(CurveNum)%Tri2ndOrder%CoeffA15 = Numbers(16)
  PerfCurve(CurveNum)%Tri2ndOrder%CoeffA16 = Numbers(17)
  PerfCurve(CurveNum)%Tri2ndOrder%CoeffA17 = Numbers(18)
  PerfCurve(CurveNum)%Tri2ndOrder%CoeffA18 = Numbers(19)
  PerfCurve(CurveNum)%Tri2ndOrder%CoeffA19 = Numbers(20)
  PerfCurve(CurveNum)%Tri2ndOrder%CoeffA20 = Numbers(21)
  PerfCurve(CurveNum)%Tri2ndOrder%CoeffA21 = Numbers(22)
  PerfCurve(CurveNum)%Tri2ndOrder%CoeffA22 = Numbers(23)
  PerfCurve(CurveNum)%Tri2ndOrder%CoeffA23 = Numbers(24)
  PerfCurve(CurveNum)%Tri2ndOrder%CoeffA24 = Numbers(25)
  PerfCurve(CurveNum)%Tri2ndOrder%CoeffA25 = Numbers(26)
  PerfCurve(CurveNum)%Tri2ndOrder%CoeffA26 = Numbers(27)
  PerfCurve(CurveNum)%Var1Min = Numbers(28)
  PerfCurve(CurveNum)%Var1Max = Numbers(29)
  PerfCurve(CurveNum)%Var2Min = Numbers(30)
  PerfCurve(CurveNum)%Var2Max = Numbers(31)
  PerfCurve(CurveNum)%Var3Min = Numbers(32)
  PerfCurve(CurveNum)%Var3Max = Numbers(33)
  IF(NumNumbers > 33 .AND. .NOT. lNumericFieldBlanks(34))THEN
    PerfCurve(CurveNum)%CurveMin        = Numbers(34)
    PerfCurve(CurveNum)%CurveMinPresent = .TRUE.
  END IF
  IF(NumNumbers > 34 .AND. .NOT. lNumericFieldBlanks(35))THEN
    PerfCurve(CurveNum)%CurveMax        = Numbers(35)
    PerfCurve(CurveNum)%CurveMaxPresent = .TRUE.
  END IF

  IF (Numbers(28) > Numbers(29)) THEN  ! error
    CALL ShowSevereError('GetCurveInput: For '//TRIM(CurrentModuleObject)//': '//TRIM(Alphas(1)))
    CALL ShowContinueError(TRIM(cNumericFieldNames(28))//' ['//TRIM(RoundSigDigits(Numbers(28),2))//'] > '//  &
       TRIM(cNumericFieldNames(29))//' ['//TRIM(RoundSigDigits(Numbers(29),2))//']')
    ErrorsFound=.true.
  ENDIF
  IF (Numbers(30) > Numbers(31)) THEN  ! error
    CALL ShowSevereError('GetCurveInput: For '//TRIM(CurrentModuleObject)//': '//TRIM(Alphas(1)))
    CALL ShowContinueError(TRIM(cNumericFieldNames(30))//' ['//TRIM(RoundSigDigits(Numbers(30),2))//'] > '//  &
       TRIM(cNumericFieldNames(31))//' ['//TRIM(RoundSigDigits(Numbers(31),2))//']')
    ErrorsFound=.true.
  ENDIF
  IF (Numbers(32) > Numbers(33)) THEN  ! error
    CALL ShowSevereError('GetCurveInput: For '//TRIM(CurrentModuleObject)//': '//TRIM(Alphas(1)))
    CALL ShowContinueError(TRIM(cNumericFieldNames(32))//' ['//TRIM(RoundSigDigits(Numbers(32),2))//'] > '//  &
       TRIM(cNumericFieldNames(33))//' ['//TRIM(RoundSigDigits(Numbers(33),2))//']')
    ErrorsFound=.true.
  ENDIF
  IF (NumAlphas .GE. 2) THEN
    IF (.NOT. IsCurveInputTypeValid(Alphas(2))) THEN
      CALL ShowWarningError('In '//TRIM(CurrentModuleObject)//' named ' // TRIM(Alphas(1)) //   &
                            ' the Input Unit Type for X is invalid.')
    END IF
  END IF
  IF (NumAlphas .GE. 3) THEN
    IF (.NOT. IsCurveInputTypeValid(Alphas(3))) THEN
      CALL ShowWarningError('In '//TRIM(CurrentModuleObject)//' named ' // TRIM(Alphas(1)) //   &
                            ' the Input Unit Type for Y is invalid.')
    END IF
  END IF
  IF (NumAlphas .GE. 4) THEN
    IF (.NOT. IsCurveInputTypeValid(Alphas(4))) THEN
      CALL ShowWarningError('In '//TRIM(CurrentModuleObject)//' named ' // TRIM(Alphas(1)) //   &
                            ' the Input Unit Type for Z is invalid.')
    END IF
  END IF
  IF (NumAlphas .GE. 5) THEN
    IF (.NOT. IsCurveOutputTypeValid(Alphas(5))) THEN
      CALL ShowWarningError('In '//TRIM(CurrentModuleObject)//' named ' // TRIM(Alphas(1)) //   &
                            ' the Output Unit Type is invalid.')
    END IF
  END IF


END DO

! Loop over quad linear curves and load data
CurrentModuleObject='Curve:QuadLinear'
DO CurveIndex=1,NumQLinear
  CALL GetObjectItem(CurrentModuleObject,CurveIndex,Alphas,NumAlphas,Numbers,NumNumbers,IOStatus,     &
                NumBlank=lNumericFieldBlanks,AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
  CurveNum = CurveNum+1
  IsNotOK=.FALSE.
  IsBlank=.FALSE.
  CALL VerifyName(Alphas(1),PerfCurve%Name,CurveNum-1,IsNotOK,IsBlank,TRIM(CurrentModuleObject)//' Name')
  IF (IsNotOK) THEN
    ErrorsFound=.true.
    IF (IsBlank) Alphas(1)='xxxxx'
  ENDIF
  ! Need to verify that this name isn't used in Pressure Curves as well.
  IF (NumPressureCurves > 0) THEN
    CurveFound=FindItemInList(Alphas(1),PressureCurve%Name,NumPressureCurves)
    IF (CurveFound /= 0) THEN
      CALL ShowSevereError('GetCurveInput: '//trim(CurrentModuleObject)//'="'//trim(Alphas(1))//'", duplicate curve name.')
      CALL ShowContinueError('...Curve name duplicates one of the Pressure Curves. Names must be unique across all curves.')
      ErrorsFound=.true.
    ENDIF
  ENDIF
  PerfCurve(CurveNum)%Name      = Alphas(1)
  PerfCurve(CurveNum)%CurveType = QuadLinear
  PerfCurve(CurveNum)%ObjectType = CurveType_QuadLinear
  PerfCurve(CurveNum)%InterpolationType = EVALUATECURVETOLIMITS
  PerfCurve(CurveNum)%Coeff1    = Numbers(1)
  PerfCurve(CurveNum)%Coeff2    = Numbers(2)
  PerfCurve(CurveNum)%Coeff3    = Numbers(3)
  PerfCurve(CurveNum)%Coeff4    = Numbers(4)
  PerfCurve(CurveNum)%Coeff5    = Numbers(5)
  PerfCurve(CurveNum)%Var1Min   = Numbers(6)
  PerfCurve(CurveNum)%Var1Max   = Numbers(7)
  PerfCurve(CurveNum)%Var2Min   = Numbers(8)
  PerfCurve(CurveNum)%Var2Max   = Numbers(9)
  PerfCurve(CurveNum)%Var3Min   = Numbers(10)
  PerfCurve(CurveNum)%Var3Max   = Numbers(11)
  PerfCurve(CurveNum)%Var4Min   = Numbers(12)
  PerfCurve(CurveNum)%Var4Max   = Numbers(13)

  IF(NumNumbers > 13 .AND. .NOT. lNumericFieldBlanks(14))THEN
    PerfCurve(CurveNum)%CurveMin        = Numbers(14)
    PerfCurve(CurveNum)%CurveMinPresent = .TRUE.
  END IF
  IF(NumNumbers > 14 .AND. .NOT. lNumericFieldBlanks(15))THEN
    PerfCurve(CurveNum)%CurveMax        = Numbers(15)
    PerfCurve(CurveNum)%CurveMaxPresent = .TRUE.
  END IF

  IF (Numbers(6) > Numbers(7)) THEN  ! error
    CALL ShowSevereError('GetCurveInput: For '//TRIM(CurrentModuleObject)//': '//TRIM(Alphas(1)))
    CALL ShowContinueError(TRIM(cNumericFieldNames(6))//' ['//TRIM(RoundSigDigits(Numbers(6),2))//'] > '//  &
       TRIM(cNumericFieldNames(7))//' ['//TRIM(RoundSigDigits(Numbers(7),2))//']')
    ErrorsFound=.true.
  ENDIF
  IF (Numbers(8) > Numbers(9)) THEN  ! error
    CALL ShowSevereError('GetCurveInput: For '//TRIM(CurrentModuleObject)//': '//TRIM(Alphas(1)))
    CALL ShowContinueError(TRIM(cNumericFieldNames(8))//' ['//TRIM(RoundSigDigits(Numbers(8),2))//'] > '//  &
       TRIM(cNumericFieldNames(9))//' ['//TRIM(RoundSigDigits(Numbers(9),2))//']')
    ErrorsFound=.true.
  ENDIF
  IF (Numbers(10) > Numbers(11)) THEN  ! error
    CALL ShowSevereError('GetCurveInput: For '//TRIM(CurrentModuleObject)//': '//TRIM(Alphas(1)))
    CALL ShowContinueError(TRIM(cNumericFieldNames(10))//' ['//TRIM(RoundSigDigits(Numbers(10),2))//'] > '//  &
       TRIM(cNumericFieldNames(11))//' ['//TRIM(RoundSigDigits(Numbers(11),2))//']')
    ErrorsFound=.true.
  ENDIF
    IF (Numbers(12) > Numbers(13)) THEN  ! error
    CALL ShowSevereError('GetCurveInput: For '//TRIM(CurrentModuleObject)//': '//TRIM(Alphas(1)))
    CALL ShowContinueError(TRIM(cNumericFieldNames(12))//' ['//TRIM(RoundSigDigits(Numbers(12),2))//'] > '//  &
       TRIM(cNumericFieldNames(13))//' ['//TRIM(RoundSigDigits(Numbers(13),2))//']')
    ErrorsFound=.true.
  ENDIF



  IF (NumAlphas .GE. 2) THEN
    IF (.NOT. IsCurveInputTypeValid(Alphas(2))) THEN
      CALL ShowWarningError('In '//TRIM(CurrentModuleObject)//' named ' // TRIM(Alphas(1)) //   &
                            ' the Input Unit Type for W is invalid.')
    END IF
  END IF
  IF (NumAlphas .GE. 3) THEN
    IF (.NOT. IsCurveInputTypeValid(Alphas(3))) THEN
      CALL ShowWarningError('In '//TRIM(CurrentModuleObject)//' named ' // TRIM(Alphas(1)) //   &
                            ' the Input Unit Type for X is invalid.')
    END IF
  END IF
  IF (NumAlphas .GE. 4) THEN
    IF (.NOT. IsCurveInputTypeValid(Alphas(4))) THEN
      CALL ShowWarningError('In '//TRIM(CurrentModuleObject)//' named ' // TRIM(Alphas(1)) //   &
                            ' the Input Unit Type for Y is invalid.')
    END IF
  END IF
  IF (NumAlphas .GE. 5) THEN
    IF (.NOT. IsCurveInputTypeValid(Alphas(5))) THEN
      CALL ShowWarningError('In '//TRIM(CurrentModuleObject)//' named ' // TRIM(Alphas(1)) //   &
                            ' the Input Unit Type for Z is invalid.')
    END IF
  END IF
  IF (NumAlphas .GE. 6) THEN
    IF (.NOT. IsCurveOutputTypeValid(Alphas(6))) THEN
      CALL ShowWarningError('In '//TRIM(CurrentModuleObject)//' named ' // TRIM(Alphas(1)) //   &
                            ' the Output Unit Type is invalid.')
    END IF
  END IF

END DO
! Loop over Exponent curves and load data
CurrentModuleObject='Curve:Exponent'
DO CurveIndex=1,NumExponent
  CALL GetObjectItem(CurrentModuleObject,CurveIndex,Alphas,NumAlphas,Numbers,NumNumbers,IOStatus,     &
                NumBlank=lNumericFieldBlanks,AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
  CurveNum = CurveNum+1
  IsNotOK=.FALSE.
  IsBlank=.FALSE.
  CALL VerifyName(Alphas(1),PerfCurve%Name,CurveNum-1,IsNotOK,IsBlank,TRIM(CurrentModuleObject)//' Name')
  IF (IsNotOK) THEN
    ErrorsFound=.true.
    IF (IsBlank) Alphas(1)='xxxxx'
  ENDIF
  ! Need to verify that this name isn't used in Pressure Curves as well.
  IF (NumPressureCurves > 0) THEN
    CurveFound=FindItemInList(Alphas(1),PressureCurve%Name,NumPressureCurves)
    IF (CurveFound /= 0) THEN
      CALL ShowSevereError('GetCurveInput: '//trim(CurrentModuleObject)//'="'//trim(Alphas(1))//'", duplicate curve name.')
      CALL ShowContinueError('...Curve name duplicates one of the Pressure Curves. Names must be unique across all curves.')
      ErrorsFound=.true.
    ENDIF
  ENDIF
  PerfCurve(CurveNum)%Name      = Alphas(1)
  PerfCurve(CurveNum)%CurveType = Exponent
  PerfCurve(CurveNum)%ObjectType = CurveType_Exponent
  PerfCurve(CurveNum)%InterpolationType = EVALUATECURVETOLIMITS
  PerfCurve(CurveNum)%Coeff1    = Numbers(1)
  PerfCurve(CurveNum)%Coeff2    = Numbers(2)
  PerfCurve(CurveNum)%Coeff3    = Numbers(3)
  PerfCurve(CurveNum)%Var1Min   = Numbers(4)
  PerfCurve(CurveNum)%Var1Max   = Numbers(5)
  IF(NumNumbers > 5 .AND. .NOT. lNumericFieldBlanks(6))THEN
    PerfCurve(CurveNum)%CurveMin        = Numbers(6)
    PerfCurve(CurveNum)%CurveMinPresent = .TRUE.
  END IF
  IF(NumNumbers > 6 .AND. .NOT. lNumericFieldBlanks(7))THEN
    PerfCurve(CurveNum)%CurveMax        = Numbers(7)
    PerfCurve(CurveNum)%CurveMaxPresent = .TRUE.
  END IF
  IF (NumAlphas .GE. 2) THEN
    IF (.NOT. IsCurveInputTypeValid(Alphas(2))) THEN
      CALL ShowWarningError('In '//TRIM(CurrentModuleObject)//' named ' // TRIM(Alphas(1)) //   &
                            ' the Input Unit Type for X is invalid.')
    END IF
  END IF
  IF (NumAlphas .GE. 3) THEN
    IF (.NOT. IsCurveOutputTypeValid(Alphas(3))) THEN
      CALL ShowWarningError('In '//TRIM(CurrentModuleObject)//' named ' // TRIM(Alphas(1)) //   &
                            ' the Output Unit Type is invalid.')
    END IF
  END IF

END DO

! cpw22Aug2010 Loop over Fan Pressure Rise curves and load data - udated 15Sep2010 for unit types
CurrentModuleObject='Curve:FanPressureRise'
DO CurveIndex=1,NumFanPressRise
  CALL GetObjectItem(CurrentModuleObject,CurveIndex,Alphas,NumAlphas,Numbers,NumNumbers,IOStatus,     &
                NumBlank=lNumericFieldBlanks,AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
  CurveNum = CurveNum+1
  IsNotOK=.FALSE.
  IsBlank=.FALSE.
  CALL VerifyName(Alphas(1),PerfCurve%Name,CurveNum-1,IsNotOK,IsBlank,TRIM(CurrentModuleObject)//' Name')
  IF (IsNotOK) THEN
    ErrorsFound=.true.
    IF (IsBlank) Alphas(1)='xxxxx'
  ENDIF
  ! Need to verify that this name isn't used in Pressure Curves as well.
  IF (NumPressureCurves > 0) THEN
    CurveFound=FindItemInList(Alphas(1),PressureCurve%Name,NumPressureCurves)
    IF (CurveFound /= 0) THEN
      CALL ShowSevereError('GetCurveInput: '//trim(CurrentModuleObject)//'="'//trim(Alphas(1))//'", duplicate curve name.')
      CALL ShowContinueError('...Curve name duplicates one of the Pressure Curves. Names must be unique across all curves.')
      ErrorsFound=.true.
    ENDIF
  ENDIF
  PerfCurve(CurveNum)%Name = Alphas(1)
  PerfCurve(CurveNum)%CurveType = FanPressureRise
  PerfCurve(CurveNum)%ObjectType = CurveType_FanPressureRise
  PerfCurve(CurveNum)%InterpolationType = EVALUATECURVETOLIMITS
  PerfCurve(CurveNum)%Coeff1 = Numbers(1)
  PerfCurve(CurveNum)%Coeff2 = Numbers(2)
  PerfCurve(CurveNum)%Coeff3 = Numbers(3)
  PerfCurve(CurveNum)%Coeff4 = Numbers(4)
  PerfCurve(CurveNum)%Var1Min = Numbers(5)
  PerfCurve(CurveNum)%Var1Max = Numbers(6)
  PerfCurve(CurveNum)%Var2Min = Numbers(7)
  PerfCurve(CurveNum)%Var2Max = Numbers(8)

  IF(NumNumbers > 8 .AND. .NOT. lNumericFieldBlanks(9))THEN
    PerfCurve(CurveNum)%CurveMin        = Numbers(9)
    PerfCurve(CurveNum)%CurveMinPresent = .TRUE.
  END IF
  IF(NumNumbers > 9 .AND. .NOT. lNumericFieldBlanks(10))THEN
    PerfCurve(CurveNum)%CurveMax        = Numbers(10)
    PerfCurve(CurveNum)%CurveMaxPresent = .TRUE.
  END IF

  IF (Numbers(5) > Numbers(6)) THEN  ! error
    CALL ShowSevereError('GetCurveInput: For '//TRIM(CurrentModuleObject)//': '//TRIM(Alphas(1)))
    CALL ShowContinueError(TRIM(cNumericFieldNames(5))//'['//TRIM(RoundSigDigits(Numbers(5),2))//'] > '//  &
       TRIM(cNumericFieldNames(6))//' ['//TRIM(RoundSigDigits(Numbers(6),2))//']')
    ErrorsFound=.true.
  ENDIF
  IF (Numbers(7) > Numbers(8)) THEN  ! error
    CALL ShowSevereError('GetCurveInput: For '//TRIM(CurrentModuleObject)//': '//TRIM(Alphas(1)))
    CALL ShowContinueError(TRIM(cNumericFieldNames(7))//'['//TRIM(RoundSigDigits(Numbers(7),2))//'] > '//  &
       TRIM(cNumericFieldNames(8))//' ['//TRIM(RoundSigDigits(Numbers(8),2))//']')
    ErrorsFound=.true.
  ENDIF

END DO !Fan Pressure Rise

! cpw22Aug2010 Loop over Exponential Skew Normal curves and load data
CurrentModuleObject='Curve:ExponentialSkewNormal'
DO CurveIndex=1,NumExpSkewNorm
  CALL GetObjectItem(CurrentModuleObject,CurveIndex,Alphas,NumAlphas,Numbers,NumNumbers,IOStatus,     &
                NumBlank=lNumericFieldBlanks,AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
  CurveNum = CurveNum+1
  IsNotOK=.FALSE.
  IsBlank=.FALSE.
  CALL VerifyName(Alphas(1),PerfCurve%Name,CurveNum-1,IsNotOK,IsBlank,TRIM(CurrentModuleObject)//' Name')
  IF (IsNotOK) THEN
    ErrorsFound=.true.
    IF (IsBlank) Alphas(1)='xxxxx'
  ENDIF
  ! Need to verify that this name isn't used in Pressure Curves as well.
  IF (NumPressureCurves > 0) THEN
    CurveFound=FindItemInList(Alphas(1),PressureCurve%Name,NumPressureCurves)
    IF (CurveFound /= 0) THEN
      CALL ShowSevereError('GetCurveInput: '//trim(CurrentModuleObject)//'="'//trim(Alphas(1))//'", duplicate curve name.')
      CALL ShowContinueError('...Curve name duplicates one of the Pressure Curves. Names must be unique across all curves.')
      ErrorsFound=.true.
    ENDIF
  ENDIF
  PerfCurve(CurveNum)%Name = Alphas(1)
  PerfCurve(CurveNum)%CurveType = ExponentialSkewNormal
  PerfCurve(CurveNum)%ObjectType = CurveType_ExponentialSkewNormal
  PerfCurve(CurveNum)%InterpolationType = EVALUATECURVETOLIMITS
  PerfCurve(CurveNum)%Coeff1 = Numbers(1)
  PerfCurve(CurveNum)%Coeff2 = Numbers(2)
  PerfCurve(CurveNum)%Coeff3 = Numbers(3)
  PerfCurve(CurveNum)%Coeff4 = Numbers(4)
  PerfCurve(CurveNum)%Var1Min = Numbers(5)
  PerfCurve(CurveNum)%Var1Max = Numbers(6)

  IF(NumNumbers > 6 .AND. .NOT. lNumericFieldBlanks(7))THEN
    PerfCurve(CurveNum)%CurveMin        = Numbers(7)
    PerfCurve(CurveNum)%CurveMinPresent = .TRUE.
  END IF
  IF(NumNumbers > 7 .AND. .NOT. lNumericFieldBlanks(9))THEN
    PerfCurve(CurveNum)%CurveMax        = Numbers(9)
    PerfCurve(CurveNum)%CurveMaxPresent = .TRUE.
  END IF

  IF (Numbers(5) > Numbers(6)) THEN  ! error
    CALL ShowSevereError('GetCurveInput: For '//TRIM(CurrentModuleObject)//': '//TRIM(Alphas(1)))
    CALL ShowContinueError(TRIM(cNumericFieldNames(5))//'['//TRIM(RoundSigDigits(Numbers(5),2))//'] > '//  &
       TRIM(cNumericFieldNames(6))//' ['//TRIM(RoundSigDigits(Numbers(6),2))//']')
    ErrorsFound=.true.
  ENDIF

  IF (NumAlphas .GE. 2) THEN
    IF (.NOT. IsCurveInputTypeValid(Alphas(2))) THEN
      CALL ShowWarningError('In '//TRIM(CurrentModuleObject)//' named ' // TRIM(Alphas(1)) //   &
                            ' the Input Unit Type for X is invalid.')
    END IF
  END IF
  IF (NumAlphas .GE. 3) THEN
    IF (.NOT. IsCurveOutputTypeValid(Alphas(3))) THEN
      CALL ShowWarningError('In '//TRIM(CurrentModuleObject)//' named ' // TRIM(Alphas(1)) //   &
                            ' the Output Unit Type is invalid.')
    END IF
  END IF
END DO !Exponential Skew Normal

! cpw22Aug2010 Loop over Sigmoid curves and load data
CurrentModuleObject='Curve:Sigmoid'
DO CurveIndex=1,NumSigmoid
  CALL GetObjectItem(CurrentModuleObject,CurveIndex,Alphas,NumAlphas,Numbers,NumNumbers,IOStatus,     &
                NumBlank=lNumericFieldBlanks,AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
  CurveNum = CurveNum+1
  IsNotOK=.FALSE.
  IsBlank=.FALSE.
  CALL VerifyName(Alphas(1),PerfCurve%Name,CurveNum-1,IsNotOK,IsBlank,TRIM(CurrentModuleObject)//' Name')
  IF (IsNotOK) THEN
    ErrorsFound=.true.
    IF (IsBlank) Alphas(1)='xxxxx'
  ENDIF
  ! Need to verify that this name isn't used in Pressure Curves as well.
  IF (NumPressureCurves > 0) THEN
    CurveFound=FindItemInList(Alphas(1),PressureCurve%Name,NumPressureCurves)
    IF (CurveFound /= 0) THEN
      CALL ShowSevereError('GetCurveInput: '//trim(CurrentModuleObject)//'="'//trim(Alphas(1))//'", duplicate curve name.')
      CALL ShowContinueError('...Curve name duplicates one of the Pressure Curves. Names must be unique across all curves.')
      ErrorsFound=.true.
    ENDIF
  ENDIF
  PerfCurve(CurveNum)%Name = Alphas(1)
  PerfCurve(CurveNum)%CurveType = Sigmoid
  PerfCurve(CurveNum)%ObjectType = CurveType_Sigmoid
  PerfCurve(CurveNum)%InterpolationType = EVALUATECURVETOLIMITS
  PerfCurve(CurveNum)%Coeff1 = Numbers(1)
  PerfCurve(CurveNum)%Coeff2 = Numbers(2)
  PerfCurve(CurveNum)%Coeff3 = Numbers(3)
  PerfCurve(CurveNum)%Coeff4 = Numbers(4)
  PerfCurve(CurveNum)%Coeff5 = Numbers(5)
  PerfCurve(CurveNum)%Var1Min = Numbers(6)
  PerfCurve(CurveNum)%Var1Max = Numbers(7)

  IF(NumNumbers > 7 .AND. .NOT. lNumericFieldBlanks(8))THEN
    PerfCurve(CurveNum)%CurveMin        = Numbers(8)
    PerfCurve(CurveNum)%CurveMinPresent = .TRUE.
  END IF
  IF(NumNumbers > 8 .AND. .NOT. lNumericFieldBlanks(9))THEN
    PerfCurve(CurveNum)%CurveMax        = Numbers(9)
    PerfCurve(CurveNum)%CurveMaxPresent = .TRUE.
  END IF

  IF (Numbers(6) > Numbers(7)) THEN  ! error
    CALL ShowSevereError('GetCurveInput: For '//TRIM(CurrentModuleObject)//': '//TRIM(Alphas(1)))
    CALL ShowContinueError(TRIM(cNumericFieldNames(6))//'['//TRIM(RoundSigDigits(Numbers(6),2))//'] > '//  &
       TRIM(cNumericFieldNames(7))//' ['//TRIM(RoundSigDigits(Numbers(7),2))//']')
    ErrorsFound=.true.
  ENDIF

  IF (NumAlphas .GE. 2) THEN
    IF (.NOT. IsCurveInputTypeValid(Alphas(2))) THEN
      CALL ShowWarningError('In '//TRIM(CurrentModuleObject)//' named ' // TRIM(Alphas(1)) //   &
                            ' the Input Unit Type for X is invalid.')
    END IF
  END IF
  IF (NumAlphas .GE. 3) THEN
    IF (.NOT. IsCurveOutputTypeValid(Alphas(3))) THEN
      CALL ShowWarningError('In '//TRIM(CurrentModuleObject)//' named ' // TRIM(Alphas(1)) //   &
                            ' the Output Unit Type is invalid.')
    END IF
  END IF
END DO !Sigmoid

! cpw22Aug2010 Loop over Rectangular Hyperbola Type 1 curves and load data
CurrentModuleObject='Curve:RectangularHyperbola1'
DO CurveIndex=1,NumRectHyper1
  CALL GetObjectItem(CurrentModuleObject,CurveIndex,Alphas,NumAlphas,Numbers,NumNumbers,IOStatus,     &
                NumBlank=lNumericFieldBlanks,AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
  CurveNum = CurveNum+1
  IsNotOK=.FALSE.
  IsBlank=.FALSE.
  CALL VerifyName(Alphas(1),PerfCurve%Name,CurveNum-1,IsNotOK,IsBlank,TRIM(CurrentModuleObject)//' Name')
  IF (IsNotOK) THEN
    ErrorsFound=.true.
    IF (IsBlank) Alphas(1)='xxxxx'
  ENDIF
  ! Need to verify that this name isn't used in Pressure Curves as well.
  IF (NumPressureCurves > 0) THEN
    CurveFound=FindItemInList(Alphas(1),PressureCurve%Name,NumPressureCurves)
    IF (CurveFound /= 0) THEN
      CALL ShowSevereError('GetCurveInput: '//trim(CurrentModuleObject)//'="'//trim(Alphas(1))//'", duplicate curve name.')
      CALL ShowContinueError('...Curve name duplicates one of the Pressure Curves. Names must be unique across all curves.')
      ErrorsFound=.true.
    ENDIF
  ENDIF
  PerfCurve(CurveNum)%Name = Alphas(1)
  PerfCurve(CurveNum)%CurveType = RectangularHyperbola1
  PerfCurve(CurveNum)%ObjectType = CurveType_RectangularHyperbola1
  PerfCurve(CurveNum)%InterpolationType = EVALUATECURVETOLIMITS
  PerfCurve(CurveNum)%Coeff1 = Numbers(1)
  PerfCurve(CurveNum)%Coeff2 = Numbers(2)
  PerfCurve(CurveNum)%Coeff3 = Numbers(3)
  PerfCurve(CurveNum)%Var1Min = Numbers(4)
  PerfCurve(CurveNum)%Var1Max = Numbers(5)

  IF(NumNumbers > 5 .AND. .NOT. lNumericFieldBlanks(6))THEN
    PerfCurve(CurveNum)%CurveMin        = Numbers(6)
    PerfCurve(CurveNum)%CurveMinPresent = .TRUE.
  END IF
  IF(NumNumbers > 6 .AND. .NOT. lNumericFieldBlanks(7))THEN
    PerfCurve(CurveNum)%CurveMax        = Numbers(7)
    PerfCurve(CurveNum)%CurveMaxPresent = .TRUE.
  END IF

  IF (Numbers(4) > Numbers(5)) THEN  ! error
    CALL ShowSevereError('GetCurveInput: For '//TRIM(CurrentModuleObject)//': '//TRIM(Alphas(1)))
    CALL ShowContinueError(TRIM(cNumericFieldNames(4))//'['//TRIM(RoundSigDigits(Numbers(4),2))//'] > '//  &
       TRIM(cNumericFieldNames(5))//' ['//TRIM(RoundSigDigits(Numbers(5),2))//']')
    ErrorsFound=.true.
  ENDIF

  IF (NumAlphas .GE. 2) THEN
    IF (.NOT. IsCurveInputTypeValid(Alphas(2))) THEN
      CALL ShowWarningError('In '//TRIM(CurrentModuleObject)//' named ' // TRIM(Alphas(1)) //   &
                            ' the Input Unit Type for X is invalid.')
    END IF
  END IF
  IF (NumAlphas .GE. 3) THEN
    IF (.NOT. IsCurveOutputTypeValid(Alphas(3))) THEN
      CALL ShowWarningError('In '//TRIM(CurrentModuleObject)//' named ' // TRIM(Alphas(1)) //   &
                            ' the Output Unit Type is invalid.')
    END IF
  END IF
END DO !Rectangular Hyperbola Type 1

! cpw22Aug2010 Loop over Rectangular Hyperbola Type 2 curves and load data
CurrentModuleObject='Curve:RectangularHyperbola2'
DO CurveIndex=1,NumRectHyper2
  CALL GetObjectItem(CurrentModuleObject,CurveIndex,Alphas,NumAlphas,Numbers,NumNumbers,IOStatus,     &
                NumBlank=lNumericFieldBlanks,AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
  CurveNum = CurveNum+1
  IsNotOK=.FALSE.
  IsBlank=.FALSE.
  CALL VerifyName(Alphas(1),PerfCurve%Name,CurveNum-1,IsNotOK,IsBlank,TRIM(CurrentModuleObject)//' Name')
  IF (IsNotOK) THEN
    ErrorsFound=.true.
    IF (IsBlank) Alphas(1)='xxxxx'
  ENDIF
  ! Need to verify that this name isn't used in Pressure Curves as well.
  IF (NumPressureCurves > 0) THEN
    CurveFound=FindItemInList(Alphas(1),PressureCurve%Name,NumPressureCurves)
    IF (CurveFound /= 0) THEN
      CALL ShowSevereError('GetCurveInput: '//trim(CurrentModuleObject)//'="'//trim(Alphas(1))//'", duplicate curve name.')
      CALL ShowContinueError('...Curve name duplicates one of the Pressure Curves. Names must be unique across all curves.')
      ErrorsFound=.true.
    ENDIF
  ENDIF
  PerfCurve(CurveNum)%Name = Alphas(1)
  PerfCurve(CurveNum)%CurveType = RectangularHyperbola2
  PerfCurve(CurveNum)%ObjectType = CurveType_RectangularHyperbola2
  PerfCurve(CurveNum)%InterpolationType = EVALUATECURVETOLIMITS
  PerfCurve(CurveNum)%Coeff1 = Numbers(1)
  PerfCurve(CurveNum)%Coeff2 = Numbers(2)
  PerfCurve(CurveNum)%Coeff3 = Numbers(3)
  PerfCurve(CurveNum)%Var1Min = Numbers(4)
  PerfCurve(CurveNum)%Var1Max = Numbers(5)

  IF(NumNumbers > 5 .AND. .NOT. lNumericFieldBlanks(6))THEN
    PerfCurve(CurveNum)%CurveMin        = Numbers(6)
    PerfCurve(CurveNum)%CurveMinPresent = .TRUE.
  END IF
  IF(NumNumbers > 6 .AND. .NOT. lNumericFieldBlanks(7))THEN
    PerfCurve(CurveNum)%CurveMax        = Numbers(7)
    PerfCurve(CurveNum)%CurveMaxPresent = .TRUE.
  END IF

  IF (Numbers(4) > Numbers(5)) THEN  ! error
    CALL ShowSevereError('GetCurveInput: For '//TRIM(CurrentModuleObject)//': '//TRIM(Alphas(1)))
    CALL ShowContinueError(TRIM(cNumericFieldNames(4))//'['//TRIM(RoundSigDigits(Numbers(4),2))//'] > '//  &
       TRIM(cNumericFieldNames(5))//' ['//TRIM(RoundSigDigits(Numbers(5),2))//']')
    ErrorsFound=.true.
  ENDIF

  IF (NumAlphas .GE. 2) THEN
    IF (.NOT. IsCurveInputTypeValid(Alphas(2))) THEN
      CALL ShowWarningError('In '//TRIM(CurrentModuleObject)//' named ' // TRIM(Alphas(1)) //   &
                            ' the Input Unit Type for X is invalid.')
    END IF
  END IF
  IF (NumAlphas .GE. 3) THEN
    IF (.NOT. IsCurveOutputTypeValid(Alphas(3))) THEN
      CALL ShowWarningError('In '//TRIM(CurrentModuleObject)//' named ' // TRIM(Alphas(1)) //   &
                            ' the Output Unit Type is invalid.')
    END IF
  END IF
END DO !Rectangular Hyperbola Type 2

! cpw22Aug2010 Loop over Exponential Decay curves and load data
CurrentModuleObject='Curve:ExponentialDecay'
DO CurveIndex=1,NumExpDecay
  CALL GetObjectItem(CurrentModuleObject,CurveIndex,Alphas,NumAlphas,Numbers,NumNumbers,IOStatus,     &
                NumBlank=lNumericFieldBlanks,AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
  CurveNum = CurveNum+1
  IsNotOK=.FALSE.
  IsBlank=.FALSE.
  CALL VerifyName(Alphas(1),PerfCurve%Name,CurveNum-1,IsNotOK,IsBlank,TRIM(CurrentModuleObject)//' Name')
  IF (IsNotOK) THEN
    ErrorsFound=.true.
    IF (IsBlank) Alphas(1)='xxxxx'
  ENDIF
  ! Need to verify that this name isn't used in Pressure Curves as well.
  IF (NumPressureCurves > 0) THEN
    CurveFound=FindItemInList(Alphas(1),PressureCurve%Name,NumPressureCurves)
    IF (CurveFound /= 0) THEN
      CALL ShowSevereError('GetCurveInput: '//trim(CurrentModuleObject)//'="'//trim(Alphas(1))//'", duplicate curve name.')
      CALL ShowContinueError('...Curve name duplicates one of the Pressure Curves. Names must be unique across all curves.')
      ErrorsFound=.true.
    ENDIF
  ENDIF
  PerfCurve(CurveNum)%Name = Alphas(1)
  PerfCurve(CurveNum)%CurveType = ExponentialDecay
  PerfCurve(CurveNum)%ObjectType = CurveType_ExponentialDecay
  PerfCurve(CurveNum)%InterpolationType = EVALUATECURVETOLIMITS
  PerfCurve(CurveNum)%Coeff1 = Numbers(1)
  PerfCurve(CurveNum)%Coeff2 = Numbers(2)
  PerfCurve(CurveNum)%Coeff3 = Numbers(3)
  PerfCurve(CurveNum)%Var1Min = Numbers(4)
  PerfCurve(CurveNum)%Var1Max = Numbers(5)

  IF(NumNumbers > 5 .AND. .NOT. lNumericFieldBlanks(6))THEN
    PerfCurve(CurveNum)%CurveMin        = Numbers(6)
    PerfCurve(CurveNum)%CurveMinPresent = .TRUE.
  END IF
  IF(NumNumbers > 6 .AND. .NOT. lNumericFieldBlanks(7))THEN
    PerfCurve(CurveNum)%CurveMax        = Numbers(7)
    PerfCurve(CurveNum)%CurveMaxPresent = .TRUE.
  END IF

  IF (Numbers(4) > Numbers(5)) THEN  ! error
    CALL ShowSevereError('GetCurveInput: For '//TRIM(CurrentModuleObject)//': '//TRIM(Alphas(1)))
    CALL ShowContinueError(TRIM(cNumericFieldNames(4))//'['//TRIM(RoundSigDigits(Numbers(4),2))//'] > '//  &
       TRIM(cNumericFieldNames(5))//' ['//TRIM(RoundSigDigits(Numbers(5),2))//']')
    ErrorsFound=.true.
  ENDIF

  IF (NumAlphas .GE. 2) THEN
    IF (.NOT. IsCurveInputTypeValid(Alphas(2))) THEN
      CALL ShowWarningError('In '//TRIM(CurrentModuleObject)//' named ' // TRIM(Alphas(1)) //   &
                            ' the Input Unit Type for X is invalid.')
    END IF
  END IF
  IF (NumAlphas .GE. 3) THEN
    IF (.NOT. IsCurveOutputTypeValid(Alphas(3))) THEN
      CALL ShowWarningError('In '//TRIM(CurrentModuleObject)//' named ' // TRIM(Alphas(1)) //   &
                            ' the Output Unit Type is invalid.')
    END IF
  END IF
END DO !Exponential Decay


! ykt July,2011 Loop over DoubleExponential Decay curves and load data
CurrentModuleObject='Curve:DoubleExponentialDecay'
DO CurveIndex=1,NumDoubleExpDecay
  CALL GetObjectItem(CurrentModuleObject,CurveIndex,Alphas,NumAlphas,Numbers,NumNumbers,IOStatus,     &
                NumBlank=lNumericFieldBlanks,AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
  CurveNum = CurveNum+1
  IsNotOK=.FALSE.
  IsBlank=.FALSE.
  CALL VerifyName(Alphas(1),PerfCurve%Name,CurveNum-1,IsNotOK,IsBlank,TRIM(CurrentModuleObject)//' Name')
  IF (IsNotOK) THEN
    ErrorsFound=.true.
    IF (IsBlank) Alphas(1)='xxxxx'
  ENDIF
  ! Need to verify that this name isn't used in Pressure Curves as well.
  IF (NumPressureCurves > 0) THEN
    CurveFound=FindItemInList(Alphas(1),PressureCurve%Name,NumPressureCurves)
    IF (CurveFound /= 0) THEN
      CALL ShowSevereError('GetCurveInput: '//trim(CurrentModuleObject)//'="'//trim(Alphas(1))//'", duplicate curve name.')
      CALL ShowContinueError('...Curve name duplicates one of the Pressure Curves. Names must be unique across all curves.')
      ErrorsFound=.true.
    ENDIF
  ENDIF
  PerfCurve(CurveNum)%Name = Alphas(1)
  PerfCurve(CurveNum)%CurveType = DoubleExponentialDecay
  PerfCurve(CurveNum)%ObjectType = CurveType_DoubleExponentialDecay
  PerfCurve(CurveNum)%InterpolationType = EVALUATECURVETOLIMITS
  PerfCurve(CurveNum)%Coeff1 = Numbers(1)
  PerfCurve(CurveNum)%Coeff2 = Numbers(2)
  PerfCurve(CurveNum)%Coeff3 = Numbers(3)
  PerfCurve(CurveNum)%Coeff4 = Numbers(4)
  PerfCurve(CurveNum)%Coeff5 = Numbers(5)
  PerfCurve(CurveNum)%Var1Min = Numbers(6)
  PerfCurve(CurveNum)%Var1Max = Numbers(7)

  IF(NumNumbers > 7 .AND. .NOT. lNumericFieldBlanks(8))THEN
    PerfCurve(CurveNum)%CurveMin        = Numbers(8)
    PerfCurve(CurveNum)%CurveMinPresent = .TRUE.
  END IF
  IF(NumNumbers > 8 .AND. .NOT. lNumericFieldBlanks(9))THEN
    PerfCurve(CurveNum)%CurveMax        = Numbers(9)
    PerfCurve(CurveNum)%CurveMaxPresent = .TRUE.
  END IF

!  IF (Numbers(4) > Numbers(5)) THEN  ! error
!    CALL ShowSevereError('GetCurveInput: For '//TRIM(CurrentModuleObject)//': '//TRIM(Alphas(1)))
!    CALL ShowContinueError(TRIM(cNumericFieldNames(4))//'['//TRIM(RoundSigDigits(Numbers(4),2))//'] > '//  &
!       TRIM(cNumericFieldNames(5))//' ['//TRIM(RoundSigDigits(Numbers(5),2))//']')
!    ErrorsFound=.true.
!  ENDIF

  IF (NumAlphas .GE. 2) THEN
    IF (.NOT. IsCurveInputTypeValid(Alphas(2))) THEN
      CALL ShowWarningError('In '//TRIM(CurrentModuleObject)//' named ' // TRIM(Alphas(1)) //   &
                            ' the Input Unit Type for X is invalid.')
    END IF
  END IF
  IF (NumAlphas .GE. 3) THEN
    IF (.NOT. IsCurveOutputTypeValid(Alphas(3))) THEN
      CALL ShowWarningError('In '//TRIM(CurrentModuleObject)//' named ' // TRIM(Alphas(1)) //   &
                            ' the Output Unit Type is invalid.')
    END IF
  END IF
END DO !Exponential Decay
TableNum = 0

! Loop over one variable tables and load data
CurrentModuleObject='Table:OneIndependentVariable'
DO CurveIndex=1,NumOneVarTab
  CALL GetObjectItem(CurrentModuleObject,CurveIndex,Alphas,NumAlphas,Numbers,NumNumbers,IOStatus,     &
                NumBlank=lNumericFieldBlanks,AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
  CurveNum = CurveNum+1
  TableNum = TableNum + 1
  NumTableEntries = (NumNumbers - 5) / 2
  ALLOCATE(TableData(TableNum)%X1(NumTableEntries))
  ALLOCATE(TableData(TableNum)%Y(NumTableEntries))
  IsNotOK=.FALSE.
  IsBlank=.FALSE.
  CALL VerifyName(Alphas(1),PerfCurve%Name,CurveNum-1,IsNotOK,IsBlank,TRIM(CurrentModuleObject)//' Name')
  IF (IsNotOK) THEN
    ErrorsFound=.true.
    IF (IsBlank) Alphas(1)='xxxxx'
  ENDIF
  ! Need to verify that this name isn't used in Pressure Curves as well.
  IF (NumPressureCurves > 0) THEN
    CurveFound=FindItemInList(Alphas(1),PressureCurve%Name,NumPressureCurves)
    IF (CurveFound /= 0) THEN
      CALL ShowSevereError('GetCurveInput: '//trim(CurrentModuleObject)//'="'//trim(Alphas(1))//'", duplicate curve name.')
      CALL ShowContinueError('...Curve name duplicates one of the Pressure Curves. Names must be unique across all curves.')
      ErrorsFound=.true.
    ENDIF
  ENDIF
  PerfCurve(CurveNum)%Name = Alphas(1)
  PerfCurve(CurveNum)%ObjectType = CurveType_TableOneIV
  PerfCurve(CurveNum)%TableIndex = TableNum
  SELECT CASE (Alphas(2))
  CASE('LINEAR')
  PerfCurve(CurveNum)%CurveType = Linear
  TableLookup(TableNum)%InterpolationOrder = 2
  CASE('QUADRATIC')
  PerfCurve(CurveNum)%CurveType = Quadratic
  TableLookup(TableNum)%InterpolationOrder = 3
  CASE('CUBIC')
  PerfCurve(CurveNum)%CurveType = Cubic
  TableLookup(TableNum)%InterpolationOrder = 4
  CASE('QUARTIC')
  PerfCurve(CurveNum)%CurveType = Quartic
  TableLookup(TableNum)%InterpolationOrder = 5
  CASE('EXPONENT')
  PerfCurve(CurveNum)%CurveType = Exponent
  TableLookup(TableNum)%InterpolationOrder = 4
  CASE DEFAULT
    CALL ShowSevereError('GetCurveInput: For '//TRIM(CurrentModuleObject)//': '//TRIM(Alphas(1)))
    CALL ShowContinueError(TRIM(cAlphaFieldNames(2))//' ['//TRIM(Alphas(2))//'] is not a valid choice. ')
    ErrorsFound=.true.
  END SELECT

  SELECT CASE (Alphas(3))
  CASE('LINEARINTERPOLATIONOFTABLE')
    PerfCurve(CurveNum)%InterpolationType = LINEARINTERPOLATIONOFTABLE
  CASE('LAGRANGEINTERPOLATIONLINEAREXTRAPOLATION')
    PerfCurve(CurveNum)%InterpolationType = LAGRANGEINTERPOLATIONLINEAREXTRAPOLATION
  CASE('EVALUATECURVETOLIMITS')
    PerfCurve(CurveNum)%InterpolationType = EVALUATECURVETOLIMITS
  CASE DEFAULT
    CALL ShowSevereError('GetCurveInput: For '//TRIM(CurrentModuleObject)//': '//TRIM(Alphas(1)))
    CALL ShowContinueError(TRIM(cAlphaFieldNames(2))//' ['//TRIM(Alphas(2))//'] is not a valid choice. ')
    ErrorsFound=.true.
  END SELECT

  IF(lNumericFieldBlanks(1))THEN
    PerfCurve(CurveNum)%Var1Min = 99999999999.D0
  ELSE
    PerfCurve(CurveNum)%Var1Min = Numbers(1)
  END IF
  IF(lNumericFieldBlanks(2))THEN
    PerfCurve(CurveNum)%Var1Max = -99999999999.D0
  ELSE
    PerfCurve(CurveNum)%Var1Max = Numbers(2)
  END IF

  IF(.NOT. lNumericFieldBlanks(1) .AND. .NOT. lNumericFieldBlanks(2))THEN
    IF (Numbers(1) > Numbers(2)) THEN  ! error
      CALL ShowSevereError('GetCurveInput: For '//TRIM(CurrentModuleObject)//': '//TRIM(Alphas(1)))
      CALL ShowContinueError(TRIM(cNumericFieldNames(1))//' ['//TRIM(RoundSigDigits(Numbers(1),2))//'] > '//  &
         TRIM(cNumericFieldNames(2))//' ['//TRIM(RoundSigDigits(Numbers(2),2))//']')
      ErrorsFound=.true.
    ENDIF
  END IF
  IF (NumAlphas .GE. 4) THEN
    IF (.NOT. IsCurveInputTypeValid(Alphas(4))) THEN
      CALL ShowSevereError('GetCurveInput: For '//TRIM(CurrentModuleObject)//': '//TRIM(Alphas(1)))
      CALL ShowContinueError(TRIM(cAlphaFieldNames(4))//' ['//TRIM(Alphas(4))//'] is invalid')
    END IF
  END IF
  IF (NumAlphas .GE. 5) THEN
    IF (.NOT. IsCurveOutputTypeValid(Alphas(5))) THEN
      CALL ShowSevereError('GetCurveInput: For '//TRIM(CurrentModuleObject)//': '//TRIM(Alphas(1)))
      CALL ShowContinueError(TRIM(cAlphaFieldNames(5))//' ['//TRIM(Alphas(5))//'] is invlaid')
    END IF
  END IF

  ! read this value first to allow normalization of min/max table output fields
  IF(.NOT. lNumericFieldBlanks(5))THEN
    TableData(TableNum)%NormalPoint = Numbers(5)
    IF(Numbers(5) .EQ. 0.0D0)THEN
      CALL ShowSevereError('GetTableInput: For '//TRIM(CurrentModuleObject)//': '//TRIM(Alphas(1)))
      CALL ShowContinueError('...'//TRIM(cNumericFieldNames(5))// &
                             ' ['//TRIM(RoundSigDigits(Numbers(5),6))//'] is not a valid choice.')
      CALL ShowContinueError('...Setting Normalization Reference to 1 and the simulation continues.')
      TableData(TableNum)%NormalPoint = 1.0D0
    END IF
  ELSE
    TableData(TableNum)%NormalPoint = 1.0D0
  END IF

  IF(.NOT. lNumericFieldBlanks(3))THEN
    PerfCurve(CurveNum)%CurveMin        = Numbers(3)/TableData(TableNum)%NormalPoint
    PerfCurve(CurveNum)%CurveMinPresent = .TRUE.
  END IF
  IF(.NOT. lNumericFieldBlanks(4))THEN
    PerfCurve(CurveNum)%CurveMax        = Numbers(4)/TableData(TableNum)%NormalPoint
    PerfCurve(CurveNum)%CurveMaxPresent = .TRUE.
  END IF

  MaxTableNums = (NumNumbers-5)/2
  IF(MOD((NumNumbers-5),2) .NE. 0)THEN
    CALL ShowSevereError('GetCurveInput: For '//TRIM(CurrentModuleObject)//': '//TRIM(Alphas(1)))
    CALL ShowContinueError('The number of data entries must be evenly divisable by 2. Number of data entries = ' &
                           //trim(RoundSigDigits(NumNumbers-5)))
    ErrorsFound=.true.
  ELSE
    DO TableDataIndex = 1, MaxTableNums
      TableData(TableNum)%X1(TableDataIndex) = Numbers((TableDataIndex-1)*2 + 5 + 1)
      TableData(TableNum)%Y(TableDataIndex)  = Numbers((TableDataIndex-1)*2 + 5 + 2)/ &
                                                     TableData(TableNum)%NormalPoint
    END DO
  END IF

  ! convert raw table data to multidimensional array
  ! find number of x variables
  NumXVar = 1
  NextXVar = 1
  TempTableData = TableData
  DO WHILE (NumXVar .LE. MaxTableNums)

    MinTableData = 999999.0D0
    MinTableData = MINVAL(TempTableData(TableNum)%X1)
    DO VarIndex = 1, MaxTableNums
      IF(TempTableData(TableNum)%X1(VarIndex) .EQ. MinTableData)THEN
        TableData(TableNum)%X1(NumXVar) = TempTableData(TableNum)%X1(VarIndex)
        TableData(TableNum)%Y(NumXVar)  = TempTableData(TableNum)%Y(VarIndex)
        TempTableData(TableNum)%X1(VarIndex) = 999999.0D0
        NumXVar = NumXVar + 1
      END IF
    END DO

    NextXVar = NumXVar

  END DO

  ! move table data to performance curve table data structure
  ALLOCATE(PerfCurveTableData(TableNum)%X1(NumXVar-1))
  ALLOCATE(PerfCurveTableData(TableNum)%Y(NumXVar-1,1))
  PerfCurveTableData(TableNum)%X1 = TableData(TableNum)%X1
  DO VarIndex = 1, NumXVar-1
    PerfCurveTableData(TableNum)%Y(VarIndex,1) = TableData(TableNum)%Y(VarIndex)
  END DO

! create curve objects when regression analysis is required
  IF(PerfCurve(CurveNum)%InterpolationType .EQ. EVALUATECURVETOLIMITS)THEN
    SELECT CASE(PerfCurve(CurveNum)%CurveType)
      CASE(LINEAR,QUADRATIC,CUBIC,QUARTIC)
        ALLOCATE(TempArray1(SIZE(PerfCurveTableData(TableNum)%X1)))
        TempArray1 = PerfCurveTableData(TableNum)%X1
        ALLOCATE(TempArray2(SIZE(PerfCurveTableData(TableNum)%Y)))
        DO VarIndex = 1, SIZE(PerfCurveTableData(TableNum)%Y)
         TempArray2(VarIndex) = PerfCurveTableData(TableNum)%Y(VarIndex,1)
        END DO
        CALL SolveRegression(CurveNum,CurrentModuleObject,PerfCurve(CurveNum)%Name,TempArray1,TempArray2)
        DEALLOCATE(TempArray1)
        DEALLOCATE(TempArray2)
      CASE DEFAULT
        CALL ShowWarningError('GetCurveInput: For '//TRIM(CurrentModuleObject)//': '//TRIM(Alphas(1)))
        CALL ShowContinueError('The requested regression analysis is not available at this time. Curve type = ' &
                              //Alphas(2))
        PerfCurve(CurveIndex)%InterpolationType = LinearInterpolationOfTable
    END SELECT
  END IF
! move table data to more compact array to allow interpolation using multivariable lookup table method
TableLookup(TableNum)%NumIndependentVars = 1
TableLookup(TableNum)%NumX1Vars = SIZE(PerfCurveTableData(TableNum)%X1)
ALLOCATE(TableLookup(TableNum)%X1Var(TableLookup(TableNum)%NumX1Vars))
ALLOCATE(TableLookup(TableNum)%TableLookupZData(SIZE(PerfCurveTableData(TableNum)%Y),1,1,1,1))
TableLookup(TableNum)%X1Var = PerfCurveTableData(TableNum)%X1
TableLookup(TableNum)%TableLookupZData(:,1,1,1,1) = PerfCurveTableData(TableNum)%Y(:,1)
END DO

! Loop over two variable tables and load data
CurrentModuleObject='Table:TwoIndependentVariables'
DO CurveIndex=1,NumTwoVarTab
  CALL GetObjectItem(CurrentModuleObject,CurveIndex,Alphas,NumAlphas,Numbers,NumNumbers,IOStatus,     &
                NumBlank=lNumericFieldBlanks,AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
  CurveNum = CurveNum+1
  TableNum = TableNum + 1
  NumTableEntries = (NumNumbers - 7) / 3
  ALLOCATE(TableData(TableNum)%X1(NumTableEntries))
  ALLOCATE(TableData(TableNum)%X2(NumTableEntries))
  ALLOCATE(TableData(TableNum)%Y(NumTableEntries))
  IsNotOK=.FALSE.
  IsBlank=.FALSE.
  CALL VerifyName(Alphas(1),PerfCurve%Name,CurveNum-1,IsNotOK,IsBlank,TRIM(CurrentModuleObject)//' Name')
  IF (IsNotOK) THEN
    ErrorsFound=.true.
    IF (IsBlank) Alphas(1)='xxxxx'
  ENDIF
  ! Need to verify that this name isn't used in Pressure Curves as well.
  IF (NumPressureCurves > 0) THEN
    CurveFound=FindItemInList(Alphas(1),PressureCurve%Name,NumPressureCurves)
    IF (CurveFound /= 0) THEN
      CALL ShowSevereError('GetCurveInput: '//trim(CurrentModuleObject)//'="'//trim(Alphas(1))//'", duplicate curve name.')
      CALL ShowContinueError('...Curve name duplicates one of the Pressure Curves. Names must be unique across all curves.')
      ErrorsFound=.true.
    ENDIF
  ENDIF
  PerfCurve(CurveNum)%Name = Alphas(1)
  PerfCurve(CurveNum)%ObjectType = CurveType_TableTwoIV
  PerfCurve(CurveNum)%TableIndex = TableNum
  SELECT CASE (Alphas(2))
  CASE('BICUBIC')
  PerfCurve(CurveNum)%CurveType = BiCubic
  TableLookup(TableNum)%InterpolationOrder = 4
  CASE('BIQUADRATIC')
  PerfCurve(CurveNum)%CurveType = BiQuadratic
  TableLookup(TableNum)%InterpolationOrder = 3
  CASE('QUADRATICLINEAR')
  PerfCurve(CurveNum)%CurveType = QuadraticLinear
  TableLookup(TableNum)%InterpolationOrder = 3
  CASE('TRIQUADRATIC')
  PerfCurve(CurveNum)%CurveType = TriQuadratic
  TableLookup(TableNum)%InterpolationOrder = 3
  CASE DEFAULT
    CALL ShowSevereError('GetCurveInput: For '//TRIM(CurrentModuleObject)//': '//TRIM(Alphas(1)))
    CALL ShowContinueError(TRIM(cAlphaFieldNames(2))//' ['//TRIM(Alphas(2))//'] is not a valid choice. ')
    ErrorsFound=.true.
  END SELECT
  SELECT CASE (Alphas(3))
  CASE('LINEARINTERPOLATIONOFTABLE')
    PerfCurve(CurveNum)%InterpolationType = LINEARINTERPOLATIONOFTABLE
  CASE('LAGRANGEINTERPOLATIONLINEAREXTRAPOLATION')
    PerfCurve(CurveNum)%InterpolationType = LAGRANGEINTERPOLATIONLINEAREXTRAPOLATION
  CASE('EVALUATECURVETOLIMITS')
    PerfCurve(CurveNum)%InterpolationType = EVALUATECURVETOLIMITS
  CASE DEFAULT
    CALL ShowSevereError('GetCurveInput: For '//TRIM(CurrentModuleObject)//': '//TRIM(Alphas(1)))
    CALL ShowContinueError(TRIM(cAlphaFieldNames(2))//' ['//TRIM(Alphas(2))//'] is not a valid choice. ')
    ErrorsFound=.true.
  END SELECT

  PerfCurve(CurveNum)%Var1Min = Numbers(1)
  PerfCurve(CurveNum)%Var1Max = Numbers(2)
  PerfCurve(CurveNum)%Var2Min = Numbers(3)
  PerfCurve(CurveNum)%Var2Max = Numbers(4)

  IF (Numbers(1) > Numbers(2)) THEN  ! error
    CALL ShowSevereError('GetCurveInput: For '//TRIM(CurrentModuleObject)//': '//TRIM(Alphas(1)))
    CALL ShowContinueError(TRIM(cNumericFieldNames(1))//' ['//TRIM(RoundSigDigits(Numbers(1),2))//'] > '//  &
       TRIM(cNumericFieldNames(2))//' ['//TRIM(RoundSigDigits(Numbers(2),2))//']')
    ErrorsFound=.true.
  ENDIF
  IF (Numbers(3) > Numbers(4)) THEN  ! error
    CALL ShowSevereError('GetCurveInput: For '//TRIM(CurrentModuleObject)//': '//TRIM(Alphas(1)))
    CALL ShowContinueError(TRIM(cNumericFieldNames(3))//' ['//TRIM(RoundSigDigits(Numbers(3),2))//'] > '//  &
       TRIM(cNumericFieldNames(4))//' ['//TRIM(RoundSigDigits(Numbers(4),2))//']')
    ErrorsFound=.true.
  ENDIF
  IF (NumAlphas .GE. 4) THEN
    IF (.NOT. IsCurveInputTypeValid(Alphas(4))) THEN
      CALL ShowSevereError('GetCurveInput: For '//TRIM(CurrentModuleObject)//': '//TRIM(Alphas(1)))
      CALL ShowContinueError(TRIM(cAlphaFieldNames(4))//' ['//TRIM(Alphas(4))//'] is invlaid')
    END IF
  END IF
  IF (NumAlphas .GE. 5) THEN
    IF (.NOT. IsCurveInputTypeValid(Alphas(5))) THEN
      CALL ShowSevereError('GetCurveInput: For '//TRIM(CurrentModuleObject)//': '//TRIM(Alphas(1)))
      CALL ShowContinueError(TRIM(cAlphaFieldNames(5))//' ['//TRIM(Alphas(5))//'] is invlaid')
    END IF
  END IF
  IF (NumAlphas .GE. 6) THEN
    IF (.NOT. IsCurveOutputTypeValid(Alphas(6))) THEN
      CALL ShowSevereError('GetCurveInput: For '//TRIM(CurrentModuleObject)//': '//TRIM(Alphas(1)))
      CALL ShowContinueError(TRIM(cAlphaFieldNames(6))//' ['//TRIM(Alphas(6))//'] is invlaid')
    END IF
  END IF

  IF(.NOT. lNumericFieldBlanks(7))THEN
    TableData(TableNum)%NormalPoint = Numbers(7)
    IF(Numbers(7) .EQ. 0.0D0)THEN
      CALL ShowSevereError('GetTableInput: For '//TRIM(CurrentModuleObject)//': '//TRIM(Alphas(1)))
      CALL ShowContinueError('...'//TRIM(cNumericFieldNames(7))// &
                             ' ['//TRIM(RoundSigDigits(Numbers(7),6))//'] is not a valid choice.')
      CALL ShowContinueError('...Setting Normalization Reference to 1 and the simulation continues.')
      TableData(TableNum)%NormalPoint = 1.0D0
    END IF
  ELSE
    TableData(TableNum)%NormalPoint = 1.0D0
  END IF

  IF(.NOT. lNumericFieldBlanks(5))THEN
    PerfCurve(CurveNum)%CurveMin        = Numbers(5)/TableData(TableNum)%NormalPoint
    PerfCurve(CurveNum)%CurveMinPresent = .TRUE.
  END IF
  IF(.NOT. lNumericFieldBlanks(6))THEN
    PerfCurve(CurveNum)%CurveMax        = Numbers(6)/TableData(TableNum)%NormalPoint
    PerfCurve(CurveNum)%CurveMaxPresent = .TRUE.
  END IF


  MaxTableNums = (NumNumbers-7)/3
  IF(MOD((NumNumbers-7),3) .NE. 0)THEN
    CALL ShowSevereError('GetCurveInput: For '//TRIM(CurrentModuleObject)//': '//TRIM(Alphas(1)))
    CALL ShowContinueError('The number of data entries must be evenly divisable by 3. Number of data entries = ' &
                           //trim(RoundSigDigits(NumNumbers-7)))
    ErrorsFound=.true.
  ELSE
    DO TableDataIndex = 1, MaxTableNums
      TableData(TableNum)%X1(TableDataIndex) = Numbers((TableDataIndex-1)*3 + 7 + 1)
      TableData(TableNum)%X2(TableDataIndex) = Numbers((TableDataIndex-1)*3 + 7 + 2)
      TableData(TableNum)%Y(TableDataIndex)  = Numbers((TableDataIndex-1)*3 + 7 + 3)/ &
                                                     TableData(TableNum)%NormalPoint
    END DO
  END IF

!  convert raw table data to multidimensional array
   ! find number of x variables
   ALLOCATE(Xvar(MaxTableNums))
   ALLOCATE(X2var(MaxTableNums))
   NumXVar = 1
   NextXVar = 1
   XVar(1) = 1.0D0
   TempTableData = TableData
   Temp2TableData = TableData
   DO WHILE (NumXVar .LE. MaxTableNums)

     MinTableData = 999999.0D0
     MinTableData = MINVAL(TempTableData(TableNum)%X1)
     DO VarIndex = 1, MaxTableNums
       IF(TempTableData(TableNum)%X1(VarIndex) .EQ. MinTableData)THEN
         TableData(TableNum)%X1(NumXVar) = TempTableData(TableNum)%X1(VarIndex)
         TableData(TableNum)%X2(NumXVar) = TempTableData(TableNum)%X2(VarIndex)
         TableData(TableNum)%Y(NumXVar)  = TempTableData(TableNum)%Y(VarIndex)
         TempTableData(TableNum)%X1(VarIndex) = 999999.0D0
         NumXVar = NumXVar + 1
       END IF
     END DO
     Temp2TableData(TableNum)%X2(NextXVar:NumXVar-1) = &
       TableData(TableNum)%X2(NextXVar:NumXVar-1)
     Temp2TableData(TableNum)%Y(NextXVar:NumXVar-1) = &
       TableData(TableNum)%Y(NextXVar:NumXVar-1)


     DO TempVarIndex = NumXVar-1, NextXVar, -1
       MaxTableDataValue = -999999.0D0
       DO TempVarIndex1 = NextXVar, NumXVar-1
         IF(Temp2TableData(TableNum)%X2(TempVarIndex1) .GT. MaxTableDataValue)THEN
           MaxTableDataValue = Temp2TableData(TableNum)%X2(TempVarIndex1)
         END IF
       END DO

       DO TempVarIndex1 = NextXVar, NumXVar-1
         IF(Temp2TableData(TableNum)%X2(TempVarIndex1) .NE. MaxTableDataValue)CYCLE
           TableData(TableNum)%X2(TempVarIndex) = &
             Temp2TableData(TableNum)%X2(TempVarIndex1)
           TableData(TableNum)%Y(TempVarIndex) = &
             Temp2TableData(TableNum)%Y(TempVarIndex1)
           Temp2TableData(TableNum)%X2(TempVarIndex1) = -999999.0D0
         EXIT
       END DO
     END DO

     NextXVar = NumXVar

   END DO
   ! reorganize table data
     NumXVar = 1
     NumX2Var = 1
     XVar(1)= TableData(TableNum)%X1(1)
     DO VarIndex = 2, MaxTableNums
       IF(TableData(TableNum)%X1(VarIndex) .NE. &
            TableData(TableNum)%X1(VarIndex-1)) THEN
         NumXVar = NumXVar + 1
         XVar(NumXVar)= TableData(TableNum)%X1(VarIndex)
       END IF
     END DO
     X2Var(1) = TableData(TableNum)%X2(1)
     DO VarIndex = 2, MaxTableNums
       FoundNewData = .TRUE.
       DO TempVarIndex = 1, NumX2Var
         IF(TableData(TableNum)%X2(VarIndex) .EQ. &
              X2Var(TempVarIndex)) THEN
           FoundNewData = .FALSE.
         END IF
       END DO
       IF(FoundNewData)THEN
         NumX2Var = NumX2Var + 1
         X2Var(NumX2Var) = TableData(TableNum)%X2(VarIndex)
       END IF
     END DO

     ! move table data to performance curve table data structure
     ALLOCATE(PerfCurveTableData(TableNum)%X1(NumXVar))
     ALLOCATE(PerfCurveTableData(TableNum)%X2(NumX2Var))
     ALLOCATE(PerfCurveTableData(TableNum)%Y(NumXVar,NumX2Var))
     PerfCurveTableData(TableNum)%X1 = -9999999.0D0
     PerfCurveTableData(TableNum)%X2 = -9999999.0D0
     PerfCurveTableData(TableNum)%Y = -9999999.0D0
     DO VarIndex = 1, NumXVar
       PerfCurveTableData(TableNum)%X1(VarIndex) = XVar(VarIndex)
       DO TempVarIndex = 1, NumX2Var
         PerfCurveTableData(TableNum)%X2(TempVarIndex) = X2Var(TempVarIndex)
         DO TempVarIndex1 = 1, MaxTableNums
           IF((TableData(TableNum)%X1(TempVarIndex1) .EQ. &
               PerfCurveTableData(TableNum)%X1(VarIndex)) .AND. &
              (TableData(TableNum)%X2(TempVarIndex1) .EQ. &
               PerfCurveTableData(TableNum)%X2(TempVarIndex)))THEN
             PerfCurveTableData(TableNum)%Y(VarIndex,TempVarIndex) = &
               TableData(TableNum)%Y(TempVarIndex1)
           END IF
         END DO
       END DO
     END DO
     DEALLOCATE(Xvar)
     DEALLOCATE(X2var)

! create curve objects when regression analysis is required
  IF(PerfCurve(CurveNum)%InterpolationType .EQ. EVALUATECURVETOLIMITS)THEN
    SELECT CASE(PerfCurve(CurveNum)%CurveType)
      CASE(BIQUADRATIC,QUADRATICLINEAR)
        ALLOCATE(TempArray1(SIZE(TableData(TableNum)%X1)))
        TempArray1 = TableData(TableNum)%X1
        ALLOCATE(TempArray3(SIZE(TableData(TableNum)%X2)))
        TempArray3 = TableData(TableNum)%X2
        ALLOCATE(TempArray2(SIZE(TableData(TableNum)%Y)))
        DO VarIndex = 1, SIZE(TableData(TableNum)%Y)
         TempArray2(VarIndex) = TableData(TableNum)%Y(VarIndex)
        END DO
        CALL SolveRegression(CurveNum,CurrentModuleObject,PerfCurve(CurveNum)%Name,TempArray1,TempArray2,TempArray3)
        DEALLOCATE(TempArray1)
        DEALLOCATE(TempArray2)
        DEALLOCATE(TempArray3)
      CASE DEFAULT
        CALL ShowWarningError('GetCurveInput: For '//TRIM(CurrentModuleObject)//': '//TRIM(Alphas(1)))
        CALL ShowContinueError('The requested regression analysis is not available at this time. Curve type = ' &
                              //Alphas(2))
        PerfCurve(CurveIndex)%InterpolationType = LinearInterpolationOfTable
    END SELECT
  END IF

! move table data to more compact array to allow interpolation using multivariable lookup table method
TableLookup(TableNum)%NumIndependentVars = 2
TableLookup(TableNum)%NumX1Vars = SIZE(PerfCurveTableData(TableNum)%X1)
TableLookup(TableNum)%NumX2Vars = SIZE(PerfCurveTableData(TableNum)%X2)
ALLOCATE(TableLookup(TableNum)%X1Var(TableLookup(TableNum)%NumX1Vars))
ALLOCATE(TableLookup(TableNum)%X2Var(TableLookup(TableNum)%NumX2Vars))
ALLOCATE(TableLookup(TableNum)%TableLookupZData(SIZE(PerfCurveTableData(TableNum)%Y(:,1)),  &
    SIZE(PerfCurveTableData(TableNum)%Y(1,:)),1,1,1))
TableLookup(TableNum)%X1Var = PerfCurveTableData(TableNum)%X1
TableLookup(TableNum)%X2Var = PerfCurveTableData(TableNum)%X2
TableLookup(TableNum)%TableLookupZData(:,:,1,1,1) = PerfCurveTableData(TableNum)%Y(:,:)

END DO

! Loop over multiple variable tables and load data (strict lookup only - no curve creation
CurrentModuleObject='Table:MultiVariableLookup'
TableNum = NumTables
DO CurveIndex=1,NumMultVarLookup
  CALL GetObjectItem(CurrentModuleObject,CurveIndex,Alphas,NumAlphas,Numbers,NumNumbers,IOStatus,     &
                NumBlank=lNumericFieldBlanks,AlphaBlank=lAlphaFieldBlanks,AlphaFieldnames=cAlphaFieldNames, &
                NumericFieldNames=cNumericFieldNames)
  CurveNum = CurveNum+1
  TableNum = TableNum + 1
  IsNotOK=.FALSE.
  IsBlank=.FALSE.
  CALL VerifyName(Alphas(1),PerfCurve%Name,CurveNum-1,IsNotOK,IsBlank,TRIM(CurrentModuleObject)//' Name')
  IF (IsNotOK) THEN
    ErrorsFound=.true.
    IF (IsBlank) Alphas(1)='xxxxx'
  ENDIF
  ! Need to verify that this name isn't used in Pressure Curves as well.
  IF (NumPressureCurves > 0) THEN
    CurveFound=FindItemInList(Alphas(1),PressureCurve%Name,NumPressureCurves)
    IF (CurveFound /= 0) THEN
      CALL ShowSevereError('GetCurveInput: '//trim(CurrentModuleObject)//'="'//trim(Alphas(1))//'", duplicate curve name.')
      CALL ShowContinueError('...Curve name duplicates one of the Pressure Curves. Names must be unique across all curves.')
      ErrorsFound=.true.
    ENDIF
  ENDIF
  PerfCurve(CurveNum)%Name = Alphas(1)
  PerfCurve(CurveNum)%ObjectType = CurveType_TableMultiIV
  PerfCurve(CurveNum)%TableIndex = TableNum
  SELECT CASE (Alphas(2))
    CASE('LINEARINTERPOLATIONOFTABLE')
      PerfCurve(CurveNum)%InterpolationType = LINEARINTERPOLATIONOFTABLE
  CASE('LAGRANGEINTERPOLATIONLINEAREXTRAPOLATION')
      PerfCurve(CurveNum)%InterpolationType = LAGRANGEINTERPOLATIONLINEAREXTRAPOLATION
    CASE('EVALUATECURVETOLIMITS')
     PerfCurve(CurveNum)%InterpolationType = EVALUATECURVETOLIMITS
    CASE DEFAULT
      CALL ShowSevereError('GetTableInput: For '//TRIM(CurrentModuleObject)//': '//TRIM(Alphas(1)))
      CALL ShowContinueError(TRIM(cAlphaFieldNames(2))//' ['//TRIM(Alphas(2))//'] is not a valid choice. ')
      ErrorsFound=.true.
  END SELECT

  SELECT CASE (Alphas(3))
  CASE('LINEAR')
  PerfCurve(CurveNum)%CurveType = Linear
  CASE('QUADRATIC')
  PerfCurve(CurveNum)%CurveType = Quadratic
  CASE('CUBIC')
  PerfCurve(CurveNum)%CurveType = Cubic
  CASE('QUARTIC')
  PerfCurve(CurveNum)%CurveType = Quartic
  CASE('EXPONENT')
  PerfCurve(CurveNum)%CurveType = Exponent
  CASE('BIQUADRATIC')
  PerfCurve(CurveNum)%CurveType = BiQuadratic
  CASE('QUADRATICINEAR')
  PerfCurve(CurveNum)%CurveType = QuadraticLinear
  CASE('BICUBIC')
  PerfCurve(CurveNum)%CurveType = BiCubic
  CASE('TRIQUADRATIC')
  PerfCurve(CurveNum)%CurveType = TriQuadratic
  CASE DEFAULT
    CALL ShowSevereError('GetCurveInput: For '//TRIM(CurrentModuleObject)//': '//TRIM(Alphas(1)))
    CALL ShowContinueError(TRIM(cAlphaFieldNames(3))//' ['//TRIM(Alphas(3))//'] is not a valid choice. ')
    ErrorsFound=.true.
  END SELECT

  SELECT CASE (Alphas(4))
    CASE('SINGLELINEINDEPENDENTVARIABLEWITHMATRIX')
      PerfCurve(CurveNum)%DataFormat = SINGLELINEINDEPENDENTVARIABLEWITHMATRIX
    CASE DEFAULT
      CALL ShowSevereError('GetTableInput: For '//TRIM(CurrentModuleObject)//': '//TRIM(Alphas(1)))
      CALL ShowContinueError(TRIM(cAlphaFieldNames(4))//' ['//TRIM(Alphas(4))//'] is not a valid choice. ')
      ErrorsFound=.true.
  END SELECT

  TableLookup(TableNum)%InterpolationOrder = Numbers(1)

  IF(.NOT. lNumericFieldBlanks(2))THEN
    TableData(TableNum)%NormalPoint = Numbers(2)
    IF(Numbers(2) .EQ. 0.0D0)THEN
      CALL ShowSevereError('GetTableInput: For '//TRIM(CurrentModuleObject)//': '//TRIM(Alphas(1)))
      CALL ShowContinueError('...'//TRIM(cNumericFieldNames(2))// &
                             ' ['//TRIM(RoundSigDigits(Numbers(2),6))//'] is not a valid choice.')
      CALL ShowContinueError('...Setting Normalization Reference to 1 and the simulation continues.')
      TableData(TableNum)%NormalPoint = 1.0D0
    END IF
  ELSE
    TableData(TableNum)%NormalPoint = 1.0D0
  END IF
  PerfCurve(CurveNum)%Var1Min = Numbers(3)
  PerfCurve(CurveNum)%Var1Max = Numbers(4)
  PerfCurve(CurveNum)%Var2Min = Numbers(5)
  PerfCurve(CurveNum)%Var2Max = Numbers(6)
  PerfCurve(CurveNum)%Var3Min = Numbers(7)
  PerfCurve(CurveNum)%Var3Max = Numbers(8)
  PerfCurve(CurveNum)%Var4Min = Numbers(9)
  PerfCurve(CurveNum)%Var4Max = Numbers(10)
  PerfCurve(CurveNum)%Var5Min = Numbers(11)
  PerfCurve(CurveNum)%Var5Max = Numbers(12)

  IF (Numbers(3) > Numbers(4)) THEN  ! error
    CALL ShowSevereError('GetTableInput: For '//TRIM(CurrentModuleObject)//': '//TRIM(Alphas(1)))
    CALL ShowContinueError(TRIM(cNumericFieldNames(3))//' ['//TRIM(RoundSigDigits(Numbers(3),2))//'] > '//  &
       TRIM(cNumericFieldNames(4))//' ['//TRIM(RoundSigDigits(Numbers(4),2))//']')
    ErrorsFound=.true.
  ENDIF
  IF (Numbers(5) > Numbers(6)) THEN  ! error
    CALL ShowSevereError('GetTableInput: For '//TRIM(CurrentModuleObject)//': '//TRIM(Alphas(1)))
    CALL ShowContinueError(TRIM(cNumericFieldNames(5))//' ['//TRIM(RoundSigDigits(Numbers(5),2))//'] > '//  &
       TRIM(cNumericFieldNames(6))//' ['//TRIM(RoundSigDigits(Numbers(6),2))//']')
    ErrorsFound=.true.
  ENDIF
  IF (Numbers(7) > Numbers(8)) THEN  ! error
    CALL ShowSevereError('GetTableInput: For '//TRIM(CurrentModuleObject)//': '//TRIM(Alphas(1)))
    CALL ShowContinueError(TRIM(cNumericFieldNames(7))//' ['//TRIM(RoundSigDigits(Numbers(7),2))//'] > '//  &
       TRIM(cNumericFieldNames(8))//' ['//TRIM(RoundSigDigits(Numbers(8),2))//']')
    ErrorsFound=.true.
  ENDIF
  IF (Numbers(9) > Numbers(10)) THEN  ! error
    CALL ShowSevereError('GetTableInput: For '//TRIM(CurrentModuleObject)//': '//TRIM(Alphas(1)))
    CALL ShowContinueError(TRIM(cNumericFieldNames(9))//' ['//TRIM(RoundSigDigits(Numbers(9),2))//'] > '//  &
       TRIM(cNumericFieldNames(10))//' ['//TRIM(RoundSigDigits(Numbers(10),2))//']')
    ErrorsFound=.true.
  ENDIF
  IF (Numbers(11) > Numbers(12)) THEN  ! error
    CALL ShowSevereError('GetTableInput: For '//TRIM(CurrentModuleObject)//': '//TRIM(Alphas(1)))
    CALL ShowContinueError(TRIM(cNumericFieldNames(11))//' ['//TRIM(RoundSigDigits(Numbers(11),2))//'] > '//  &
       TRIM(cNumericFieldNames(12))//' ['//TRIM(RoundSigDigits(Numbers(12),2))//']')
    ErrorsFound=.true.
  ENDIF
  IF (NumAlphas .GE. 8) THEN
    IF (.NOT. IsCurveInputTypeValid(Alphas(8))) THEN
      CALL ShowSevereError('GetCurveInput: For '//TRIM(CurrentModuleObject)//': '//TRIM(Alphas(1)))
      CALL ShowContinueError(TRIM(cAlphaFieldNames(8))//' ['//TRIM(Alphas(8))//'] is invlaid')
    END IF
  END IF
  IF (NumAlphas .GE. 9) THEN
    IF (.NOT. IsCurveInputTypeValid(Alphas(9))) THEN
      CALL ShowSevereError('GetCurveInput: For '//TRIM(CurrentModuleObject)//': '//TRIM(Alphas(1)))
      CALL ShowContinueError(TRIM(cAlphaFieldNames(9))//' ['//TRIM(Alphas(9))//'] is invlaid')
    END IF
  END IF
  IF (NumAlphas .GE. 10) THEN
    IF (.NOT. IsCurveInputTypeValid(Alphas(10))) THEN
      CALL ShowSevereError('GetCurveInput: For '//TRIM(CurrentModuleObject)//': '//TRIM(Alphas(1)))
      CALL ShowContinueError(TRIM(cAlphaFieldNames(10))//' ['//TRIM(Alphas(10))//'] is invlaid')
    END IF
  END IF
  IF (NumAlphas .GE. 11) THEN
    IF (.NOT. IsCurveInputTypeValid(Alphas(11))) THEN
      CALL ShowSevereError('GetCurveInput: For '//TRIM(CurrentModuleObject)//': '//TRIM(Alphas(1)))
      CALL ShowContinueError(TRIM(cAlphaFieldNames(11))//' ['//TRIM(Alphas(11))//'] is invlaid')
    END IF
  END IF
  IF (NumAlphas .GE. 12) THEN
    IF (.NOT. IsCurveInputTypeValid(Alphas(12))) THEN
      CALL ShowSevereError('GetCurveInput: For '//TRIM(CurrentModuleObject)//': '//TRIM(Alphas(1)))
      CALL ShowContinueError(TRIM(cAlphaFieldNames(12))//' ['//TRIM(Alphas(12))//'] is invlaid')
    END IF
  END IF
  IF (NumAlphas .GE. 13) THEN
    IF (.NOT. IsCurveOutputTypeValid(Alphas(13))) THEN
      CALL ShowSevereError('GetCurveInput: For '//TRIM(CurrentModuleObject)//': '//TRIM(Alphas(1)))
      CALL ShowContinueError(TRIM(cAlphaFieldNames(13))//' ['//TRIM(Alphas(13))//'] is invlaid')
    END IF
  END IF

  IF(.NOT. lNumericFieldBlanks(13))THEN
    PerfCurve(CurveNum)%CurveMin        = Numbers(13)/TableData(TableNum)%NormalPoint
    PerfCurve(CurveNum)%CurveMinPresent = .TRUE.
  END IF
  IF(.NOT. lNumericFieldBlanks(14))THEN
    PerfCurve(CurveNum)%CurveMax        = Numbers(14)/TableData(TableNum)%NormalPoint
    PerfCurve(CurveNum)%CurveMaxPresent = .TRUE.
  END IF

  IF(Alphas(6) .EQ. 'ASCENDING')THEN
   PerfCurve(CurveNum)%X1SortOrder=ASCENDING
  ELSE IF(Alphas(6) .EQ. 'DESCENDING')THEN
   PerfCurve(CurveNum)%X1SortOrder=DESCENDING
  ELSE
    CALL ShowSevereError('GetTableInput: For '//TRIM(CurrentModuleObject)//': '//TRIM(Alphas(1)))
    CALL ShowContinueError('...Invalid '//TRIM(cAlphaFieldNames(6))//' = '//TRIM(Alphas(6)))
    ErrorsFound=.true.
  END IF
  IF(Alphas(7) .EQ. 'ASCENDING')THEN
    PerfCurve(CurveNum)%X2SortOrder=ASCENDING
  ELSE IF(Alphas(7) .EQ. 'DESCENDING')THEN
    PerfCurve(CurveNum)%X2SortOrder=DESCENDING
  ELSE
    CALL ShowSevereError('GetTableInput: For '//TRIM(CurrentModuleObject)//': '//TRIM(Alphas(1)))
    CALL ShowContinueError('...Invalid '//TRIM(cAlphaFieldNames(7))//' = '//TRIM(Alphas(7)))
    ErrorsFound=.true.
  END IF

  IF(.NOT. lAlphaFieldBlanks(5))THEN
    ReadFromFile = .TRUE.
    FileName = Alphas(5)
  ELSE
    ReadFromFile = .FALSE.
    FileName = ' '
  END IF

  CALL ReadTableData(CurveNum, CurrentModuleObject, ReadFromFile, FileName, Alphas, Numbers, NumNumbers, ErrorsFound)

  IF(PerfCurve(CurveNum)%InterpolationType .EQ. EVALUATECURVETOLIMITS)THEN
    SELECT CASE(TableLookup(TableNum)%NumIndependentVars)
      CASE(1)
        ALLOCATE(TempArray1(SIZE(TableLookup(TableNum)%TableLookupZData(:,1,1,1,1))))
        ALLOCATE(TempArray2(SIZE(TempArray1)))
        TempArray1 = TableLookup(TableNum)%X1Var
        TempArray2 = TableLookup(TableNum)%TableLookupZData(:,1,1,1,1)
        CALL SolveRegression(CurveNum,CurrentModuleObject,PerfCurve(CurveNum)%Name,TempArray1,TempArray2)
        DEALLOCATE(TempArray1)
        DEALLOCATE(TempArray2)

        ! Save array info in performance table arrays in case the performance table routine is selected in regression routine
        ALLOCATE(PerfCurveTableData(TableNum)%X1(SIZE(TableLookup(TableNum)%X1Var)))
        ALLOCATE(PerfCurveTableData(TableNum)%Y(SIZE(TableLookup(TableNum)%X1Var),1))
        PerfCurveTableData(TableNum)%X1 = TableLookup(TableNum)%X1Var
        PerfCurveTableData(TableNum)%Y(:,1) = TableLookup(TableNum)%TableLookupZData(:,1,1,1,1)

      CASE(2)
        ALLOCATE(TempArray1(SIZE(TableLookup(TableNum)%TableLookupZData(:,:,1,1,1))))
        ALLOCATE(TempArray2(SIZE(TempArray1)))
        ALLOCATE(TempArray3(SIZE(TempArray1)))
        TableDataIndex = 0
        DO VarIndex = 1, TableLookup(TableNum)%NumX1Vars
          DO TempVarIndex = 1, TableLookup(TableNum)%NumX2Vars
            TableDataIndex = TableDataIndex + 1
            TempArray1(TableDataIndex) = TableLookup(TableNum)%X1Var(VarIndex)
            TempArray2(TableDataIndex) = TableLookup(TableNum)%X2Var(TempVarIndex)
            TempArray3(TableDataIndex) = TableLookup(TableNum)%TableLookupZData(VarIndex,TempVarIndex,1,1,1)
          END DO
        END DO
        CALL SolveRegression(CurveNum,CurrentModuleObject,PerfCurve(CurveNum)%Name,TempArray1,TempArray3,TempArray2)
        DEALLOCATE(TempArray1)
        DEALLOCATE(TempArray2)
        DEALLOCATE(TempArray3)
        ! Save array info in performance table arrays in case the performance table routine is selected in regression routine
        ALLOCATE(PerfCurveTableData(TableNum)%X1(SIZE(TableLookup(TableNum)%X1Var)))
        ALLOCATE(PerfCurveTableData(TableNum)%X2(SIZE(TableLookup(TableNum)%X2Var)))
        ALLOCATE(PerfCurveTableData(TableNum)%Y(SIZE(TableLookup(TableNum)%X1Var),SIZE(TableLookup(TableNum)%X2Var)))
        PerfCurveTableData(TableNum)%X1 = TableLookup(TableNum)%X1Var
        PerfCurveTableData(TableNum)%X2 = TableLookup(TableNum)%X2Var
        PerfCurveTableData(TableNum)%Y(:,:) = TableLookup(TableNum)%TableLookupZData(:,:,1,1,1)
    CASE DEFAULT
      CALL ShowSevereError('GetTableInput: For '//TRIM(CurrentModuleObject)//': '//TRIM(Alphas(1)))
      CALL ShowContinueError('...Invalid '//TRIM(cAlphaFieldNames(2))//' = '//TRIM(Alphas(2)))
      CALL ShowContinueError('...Choice not allowed with more than 2 indpendent variables.')
      ErrorsFound=.true.
    END SELECT
  ELSE
    SELECT CASE(TableLookup(TableNum)%NumIndependentVars)
      CASE(1)
        ! Save array info in performance table arrays in case the performance table routine is selected in regression routine
        ALLOCATE(PerfCurveTableData(TableNum)%X1(SIZE(TableLookup(TableNum)%X1Var)))
        ALLOCATE(PerfCurveTableData(TableNum)%Y(SIZE(TableLookup(TableNum)%X1Var),1))
        PerfCurveTableData(TableNum)%X1 = TableLookup(TableNum)%X1Var
        PerfCurveTableData(TableNum)%Y(:,1) = TableLookup(TableNum)%TableLookupZData(:,1,1,1,1)
        ! if linear interpolation of table is selected, switch interpolation type
      CASE(2)
        ! Save array info in performance table arrays in case the performance table routine is selected in regression routine
        ALLOCATE(PerfCurveTableData(TableNum)%X1(SIZE(TableLookup(TableNum)%X1Var)))
        ALLOCATE(PerfCurveTableData(TableNum)%X2(SIZE(TableLookup(TableNum)%X2Var)))
        ALLOCATE(PerfCurveTableData(TableNum)%Y(SIZE(TableLookup(TableNum)%X1Var),SIZE(TableLookup(TableNum)%X2Var)))
        PerfCurveTableData(TableNum)%X1 = TableLookup(TableNum)%X1Var
        PerfCurveTableData(TableNum)%X2 = TableLookup(TableNum)%X2Var
        PerfCurveTableData(TableNum)%Y(:,:) = TableLookup(TableNum)%TableLookupZData(:,:,1,1,1)
        ! if linear interpolation of table is selected, switch interpolation type
    CASE DEFAULT
      ! if linear interpolation of table is selected, fatal if more than 2 independent variables
      IF(PerfCurve(CurveNum)%InterpolationType == LINEARINTERPOLATIONOFTABLE)THEN
        CALL ShowSevereError('GetTableInput: For '//TRIM(CurrentModuleObject)//': '//TRIM(Alphas(1)))
        CALL ShowContinueError('...Invalid '//TRIM(cAlphaFieldNames(2))//' = '//TRIM(Alphas(2)))
        CALL ShowContinueError('...Choice not allowed with more than 2 indpendent variables.')
        ErrorsFound=.true.
      END IF
    END SELECT
  END IF

END DO

IF (ErrorsFound) THEN
  CALL ShowFatalError('GetCurveInput: Errors found in getting Curve Objects.  Preceding condition(s) cause termination.')
END IF

RETURN

END SUBROUTINE GetCurveInput

SUBROUTINE InitCurveReporting

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   October 2011
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Setting up of curve output variables caused errors in some files. Thus, separating the setup
          ! from the getinput.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataInterfaces, ONLY:SetupOutputVariable

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: CurveIndex

  DO CurveIndex = 1, NumCurves
    SELECT CASE(PerfCurve(CurveIndex)%ObjectType)
       ! CurrentModuleObject='Table:MultiVariableLookup'
      CASE(CurveType_TableMultiIV)
        SELECT CASE(TableLookup(PerfCurve(CurveIndex)%TableIndex)%NumIndependentVars)
          CASE(1) !- 1 independent variable
            CALL SetupOutputVariable('Performance Curve Input Variable 1 Value []',PerfCurve(CurveIndex)%CurveInput1, &
                  'HVAC','Average',PerfCurve(CurveIndex)%Name)
          CASE(2) !- 2 independent variables
            CALL SetupOutputVariable('Performance Curve Input Variable 1 Value []',PerfCurve(CurveIndex)%CurveInput1, &
                  'HVAC','Average',PerfCurve(CurveIndex)%Name)
            CALL SetupOutputVariable('Performance Curve Input Variable 2 Value []',PerfCurve(CurveIndex)%CurveInput2, &
                  'HVAC','Average',PerfCurve(CurveIndex)%Name)
          CASE(3) !- 3 independent variables
            CALL SetupOutputVariable('Performance Curve Input Variable 1 Value []',PerfCurve(CurveIndex)%CurveInput1, &
                  'HVAC','Average',PerfCurve(CurveIndex)%Name)
            CALL SetupOutputVariable('Performance Curve Input Variable 2 Value []',PerfCurve(CurveIndex)%CurveInput2, &
                  'HVAC','Average',PerfCurve(CurveIndex)%Name)
            CALL SetupOutputVariable('Performance Curve Input Variable 3 Value []',PerfCurve(CurveIndex)%CurveInput3, &
                  'HVAC','Average',PerfCurve(CurveIndex)%Name)
          CASE(4) !- 4 independent variables
            CALL SetupOutputVariable('Performance Curve Input Variable 1 Value []',PerfCurve(CurveIndex)%CurveInput1, &
                  'HVAC','Average',PerfCurve(CurveIndex)%Name)
            CALL SetupOutputVariable('Performance Curve Input Variable 2 Value []',PerfCurve(CurveIndex)%CurveInput2, &
                  'HVAC','Average',PerfCurve(CurveIndex)%Name)
            CALL SetupOutputVariable('Performance Curve Input Variable 3 Value []',PerfCurve(CurveIndex)%CurveInput3, &
                  'HVAC','Average',PerfCurve(CurveIndex)%Name)
            CALL SetupOutputVariable('Performance Curve Input Variable 4 Value []',PerfCurve(CurveIndex)%CurveInput4, &
                  'HVAC','Average',PerfCurve(CurveIndex)%Name)
          CASE(5) !- 5 independent variables
            CALL SetupOutputVariable('Performance Curve Input Variable 1 Value []',PerfCurve(CurveIndex)%CurveInput1, &
                  'HVAC','Average',PerfCurve(CurveIndex)%Name)
            CALL SetupOutputVariable('Performance Curve Input Variable 2 Value []',PerfCurve(CurveIndex)%CurveInput2, &
                  'HVAC','Average',PerfCurve(CurveIndex)%Name)
            CALL SetupOutputVariable('Performance Curve Input Variable 3 Value []',PerfCurve(CurveIndex)%CurveInput3, &
                  'HVAC','Average',PerfCurve(CurveIndex)%Name)
            CALL SetupOutputVariable('Performance Curve Input Variable 4 Value []',PerfCurve(CurveIndex)%CurveInput4, &
                  'HVAC','Average',PerfCurve(CurveIndex)%Name)
            CALL SetupOutputVariable('Performance Curve Input Variable 5 Value []',PerfCurve(CurveIndex)%CurveInput5, &
                  'HVAC','Average',PerfCurve(CurveIndex)%Name)
          CASE DEFAULT
        END SELECT
      CASE(CurveType_TableOneIV)
        ! CurrentModuleObject='Table:OneIndependentVariable'
        CALL SetupOutputVariable('Performance Curve Input Variable 1 Value []',PerfCurve(CurveIndex)%CurveInput1, &
                  'HVAC','Average',PerfCurve(CurveIndex)%Name)
      CASE(CurveType_TableTwoIV)
        ! CurrentModuleObject='Table:TwoIndependentVariables'
        CALL SetupOutputVariable('Performance Curve Input Variable 1 Value []',PerfCurve(CurveIndex)%CurveInput1, &
                  'HVAC','Average',PerfCurve(CurveIndex)%Name)
        CALL SetupOutputVariable('Performance Curve Input Variable 2 Value []',PerfCurve(CurveIndex)%CurveInput2, &
                  'HVAC','Average',PerfCurve(CurveIndex)%Name)
      CASE DEFAULT
        SELECT CASE(PerfCurve(CurveIndex)%CurveType)
          CASE(Linear, Quadratic, Cubic, Quartic, Exponent, FuncPressDrop)
            ! CurrentModuleObject='Curve:Linear/Quadratic/Cubic/Quartic/Exponent/Functional:PressureDrop'
            CALL SetupOutputVariable('Performance Curve Input Variable 1 Value []',PerfCurve(CurveIndex)%CurveInput1, &
                  'HVAC','Average',PerfCurve(CurveIndex)%Name)
          CASE(BiQuadratic, QuadraticLinear, BiCubic)
            ! CurrentModuleObject='Curve:BiQuadratic/QuadraticLinear/BiCubic'
            CALL SetupOutputVariable('Performance Curve Input Variable 1 Value []',PerfCurve(CurveIndex)%CurveInput1, &
                  'HVAC','Average',PerfCurve(CurveIndex)%Name)
            CALL SetupOutputVariable('Performance Curve Input Variable 2 Value []',PerfCurve(CurveIndex)%CurveInput2, &
                  'HVAC','Average',PerfCurve(CurveIndex)%Name)
          CASE(TriQuadratic)
            ! CurrentModuleObject='Curve:TriQuadratic'
            CALL SetupOutputVariable('Performance Curve Input Variable 1 Value []',PerfCurve(CurveIndex)%CurveInput1, &
                  'HVAC','Average',PerfCurve(CurveIndex)%Name)
            CALL SetupOutputVariable('Performance Curve Input Variable 2 Value []',PerfCurve(CurveIndex)%CurveInput2, &
                  'HVAC','Average',PerfCurve(CurveIndex)%Name)
            CALL SetupOutputVariable('Performance Curve Input Variable 3 Value []',PerfCurve(CurveIndex)%CurveInput3, &
                  'HVAC','Average',PerfCurve(CurveIndex)%Name)
          CASE(QuadLinear)
            ! CurrentModuleObject='Curve:QuadLinear'
            CALL SetupOutputVariable('Performance Curve Input Variable 1 Value []',PerfCurve(CurveIndex)%CurveInput1, &
                  'HVAC','Average',PerfCurve(CurveIndex)%Name)
            CALL SetupOutputVariable('Performance Curve Input Variable 2 Value []',PerfCurve(CurveIndex)%CurveInput2, &
                  'HVAC','Average',PerfCurve(CurveIndex)%Name)
            CALL SetupOutputVariable('Performance Curve Input Variable 3 Value []',PerfCurve(CurveIndex)%CurveInput3, &
                  'HVAC','Average',PerfCurve(CurveIndex)%Name)
            CALL SetupOutputVariable('Performance Curve Input Variable 4 Value []',PerfCurve(CurveIndex)%CurveInput4, &
                  'HVAC','Average',PerfCurve(CurveIndex)%Name)
        END SELECT
    END SELECT
  ! set the output up last so it shows up after the input in the csv file
    CALL SetupOutputVariable('Performance Curve Output Value []',PerfCurve(CurveIndex)%CurveOutput, &
                              'HVAC','Average',PerfCurve(CurveIndex)%Name)
  END DO

  DO CurveIndex = 1, NumPressureCurves
    CALL SetupOutputVariable('Performance Curve Input Variable 1 Value []',PressureCurve(CurveIndex)%CurveInput1, &
            'HVAC','Average',PressureCurve(CurveIndex)%Name)
    CALL SetupOutputVariable('Performance Curve Input Variable 2 Value []',PressureCurve(CurveIndex)%CurveInput2, &
            'HVAC','Average',PressureCurve(CurveIndex)%Name)
    CALL SetupOutputVariable('Performance Curve Input Variable 3 Value []',PressureCurve(CurveIndex)%CurveInput3, &
            'HVAC','Average',PressureCurve(CurveIndex)%Name)
    CALL SetupOutputVariable('Performance Curve Output Value []',PressureCurve(CurveIndex)%CurveOutput, &
                              'HVAC','Average',PressureCurve(CurveIndex)%Name)
  ENDDO

  IF (AnyEnergyManagementSystemInModel) Then ! provide hook for possible EMS control
    DO CurveIndex = 1,  NumCurves
      CALL SetupEMSActuator('Curve',  PerfCurve(CurveIndex)%Name , &
                            'Curve Result','[unknown]' ,&
                            PerfCurve(CurveIndex)%EMSOverrideOn  , &
                            PerfCurve(CurveIndex)%EMSOverrideCurveValue)
    ENDDO ! All performance curves
  ENDIF
  IF (AnyEnergyManagementSystemInModel) Then ! provide hook for possible EMS control
    DO CurveIndex = 1,  NumPressureCurves
      CALL SetupEMSActuator('Curve',  PressureCurve(CurveIndex)%Name , &
                            'Curve Result','[unknown]' ,&
                            PressureCurve(CurveIndex)%EMSOverrideOn  , &
                            PressureCurve(CurveIndex)%EMSOverrideCurveValue)
    ENDDO ! All pressure curves
  ENDIF


  RETURN

END SUBROUTINE InitCurveReporting


SUBROUTINE ReadTableData(CurveNum, CurrentModuleObject, ReadFromFile, FileName, Alphas, Numbers, NumNumbers, ErrorsFound)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Raustad, FSEC
          !       DATE WRITTEN   August 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Given the curve index, read the table data.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:

USE General,             ONLY: RoundSigDigits
USE DataGlobals,         ONLY: DisplayAdvancedReportVariables, OutputFileInits
USE DataSystemVariables, ONLY: iASCII_CR, iUnicode_end,GoodIOStatValue,TempFullFileName,CheckForActualFileName

          ! SUBROUTINE ARGUMENT DEFINITIONS:
INTEGER, INTENT(IN)          :: CurveNum
CHARACTER(len=MaxNameLength) :: CurrentModuleObject
LOGICAL, INTENT(IN)          :: ReadFromFile
CHARACTER(len=MaxNameLength) :: FileName
CHARACTER(len=MaxNameLength), DIMENSION(:) ::  Alphas
REAL(r64), DIMENSION(:)     :: Numbers
INTEGER, INTENT(IN)         :: NumNumbers
LOGICAL, INTENT(INOUT)      :: ErrorsFound

          ! SUBROUTINE PARAMETER DEFINITIONS:
CHARACTER(len=*), PARAMETER :: fmtA="(A)"

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
INTEGER            :: FileNum
INTEGER,EXTERNAL   :: GetNewUnitNumber
INTEGER            :: endcol
INTEGER            :: TableNum
CHARACTER(len=400) :: NextLine           ! Line of data
INTEGER            :: DataSetCount       ! counter for number of lines read (used in some error messages)
INTEGER            :: ReadStat           ! File read status
LOGICAL            :: EOFonFile          ! True if EOF during file read
INTEGER            :: I, J, NumDataSets  ! Do loop indexes and data set counter
REAL(r64)          :: Var3, Var4, Var5   ! Temp variables for processing table lookup data
INTEGER            :: Var3Index, Var4Index, Var5Index, NumIVars, TotalDataSets
INTEGER            :: NumbersOffset, BaseOffset
LOGICAL, SAVE      :: WriteHeaderOnce = .TRUE. ! eio header file write flag
CHARACTER(len=1000) :: CharTableData      ! used to echo each line of table data read in to eio file
LOGICAL            :: EchoTableDataToEio  ! logical set equal to global and used to report to eio file
LOGICAL            :: FileExists

  EchoTableDataToEio = DisplayAdvancedReportVariables
  TableNum = PerfCurve(CurveNum)%TableIndex

140   FORMAT('! Reading external file tabular data for ',A,' "',A,'"')
150   FORMAT('! Reading tabular data for ',A,' "',A,'"')

  TotalDataSets = 0 !Objexx:Uninit GO TO 1000 will cause TotalDataSets to be used before it is initialized: Line added
  IF(ReadFromFile)THEN
    FileExists=.false.
    CALL CheckForActualFileName(FileName,FileExists,TempFullFileName)
    IF (.not. FileExists) GOTO 999
    FileNum = GetNewUnitNumber()
    OPEN(UNIT=FileNum, FILE=TempFullFileName, Action='read', Err=999)
    READ(Unit=FileNum,FMT=fmtA) NextLine
    endcol=LEN_TRIM(NextLine)
    IF(endcol == 0)THEN
      CALL ShowWarningError('ReadTableData: Blank line found in external file = '//TRIM(FileName))
      CALL ShowContinueError('...Blank lines are not allowed. Will try to read next line.')
      READ(Unit=FileNum, FMT=fmtA) NextLine
      endcol=LEN_TRIM(NextLine)
      IF(endcol == 0)THEN
        CALL ShowWarningError('ReadTableData: Data not found on second line in external file = '//TRIM(FileName))
        CALL ShowContinueError('...Check that file is ASCII text and that file is not locked by other applications.')
        CALL ShowFatalError('External table data not found. Simulation will terminate.')
      ELSE
        CALL ShowWarningError('Second read attempt found data in external file = '//TRIM(FileName))
        CALL ShowFatalError('...Blank lines are not allowed. Simulation will terminate.')
      END IF
    END IF
    IF (endcol > 0) THEN
      IF (ICHAR(NextLine(endcol:endcol)) == iUnicode_end) THEN
        CALL ShowSevereError('ReadTableData: For Table:MultiVariableLookup "'//TRIM(PerfCurve(CurveNum)%Name)//'" '//  &
         ' external file, appears to be a Unicode or binary file.')
        CALL ShowContinueError('...This file cannot be read by this program. Please save as PC or Unix file and try again')
        CALL ShowFatalError('Program terminates due to previous condition.')
      ENDIF
    ENDIF

    REWIND(FileNum)

    READ(FileNum,*,IOSTAT=ReadStat)NumIVars
    IF(NumIVars > 5 .OR. NumIVars < 1)THEN
      CALL ShowSevereError('ReadTableData: For '//TRIM(CurrentModuleObject)//': '//TRIM(Alphas(1)))
      CALL ShowContinueError('...Invalid number of independent variables found in external file = '//TRIM(FileName))
      CALL ShowFatalError('...Only 1 to 5 independent variables are allowed.')
    END IF

    REWIND(FileNum)

    IF(NumIVars .EQ. 1)READ(FileNum,*,IOSTAT=ReadStat)NumIVars, &
      TableLookup(TableNum)%NumX1Vars
    IF(NumIVars .EQ. 2)READ(FileNum,*,IOSTAT=ReadStat)NumIVars, &
      TableLookup(TableNum)%NumX1Vars, &
      TableLookup(TableNum)%NumX2Vars
    IF(NumIVars .EQ. 3)READ(FileNum,*,IOSTAT=ReadStat)NumIVars, &
      TableLookup(TableNum)%NumX1Vars, &
      TableLookup(TableNum)%NumX2Vars, &
      TableLookup(TableNum)%NumX3Vars
    IF(NumIVars .EQ. 4)READ(FileNum,*,IOSTAT=ReadStat)NumIVars, &
      TableLookup(TableNum)%NumX1Vars, &
      TableLookup(TableNum)%NumX2Vars, &
      TableLookup(TableNum)%NumX3Vars, &
      TableLookup(TableNum)%NumX4Vars
    IF(NumIVars .EQ. 5)READ(FileNum,*,IOSTAT=ReadStat)NumIVars, &
      TableLookup(TableNum)%NumX1Vars, &
      TableLookup(TableNum)%NumX2Vars, &
      TableLookup(TableNum)%NumX3Vars, &
      TableLookup(TableNum)%NumX4Vars, &
      TableLookup(TableNum)%NumX5Vars

    IF(ReadStat < GoodIOStatValue) GO TO 1000

    TableLookup(TableNum)%NumIndependentVars = NumIVars
! Echo table data for user verification
110 FORMAT('! <READING LOOKUP TABLE DATA>')
130 FORMAT('READING LOOKUP TABLE DATA')
131 FORMAT('END READING LOOKUP TABLE DATA')
    IF(EchoTableDataToEio)THEN
      IF(WriteHeaderOnce)THEN
        WRITE(OutputFileInits,110)
        WriteHeaderOnce = .FALSE.
      END IF
      WRITE(OutputFileInits,130)
      WRITE(OutputFileInits,140)TRIM(CurrentModuleObject),TRIM(Alphas(1))
    END IF
  ELSE
    IF(EchoTableDataToEio)THEN
      IF(WriteHeaderOnce)THEN
        WRITE(OutputFileInits,110)
        WriteHeaderOnce = .FALSE.
      END IF
      WRITE(OutputFileInits,130)
      WRITE(OutputFileInits,150)TRIM(CurrentModuleObject),TRIM(Alphas(1))
    END IF
    NumIVars = Numbers(15)
    IF(NumIVars > 5 .OR. NumIVars < 1)THEN
      CALL ShowSevereError('ReadTableData: For '//TRIM(CurrentModuleObject)//': '//TRIM(Alphas(1)))
      CALL ShowContinueError('...Invalid number of independent variables.')
      CALL ShowFatalError('...Only 1 to 5 independent variables are allowed.')
    END IF

    BaseOffset = 15
    TableLookup(TableNum)%NumIndependentVars = NumIVars
    TableLookup(TableNum)%NumX1Vars = Numbers(16)
    IF(NumIVars .GT. 1)TableLookup(TableNum)%NumX2Vars = Numbers(17)
    IF(NumIVars .GT. 2)TableLookup(TableNum)%NumX3Vars = Numbers(18)
    IF(NumIVars .GT. 3)TableLookup(TableNum)%NumX4Vars = Numbers(19)
    IF(NumIVars .GT. 4)TableLookup(TableNum)%NumX5Vars = Numbers(20)
  END IF

  IF(EchoTableDataToEio)THEN
    IF(NumIVars .EQ. 1)THEN
      WRITE(CharTableData,160)NumIVars,TableLookup(TableNum)%NumX1Vars
    ELSEIF(NumIVars .EQ. 2)THEN
      WRITE(CharTableData,160)NumIVars,TableLookup(TableNum)%NumX1Vars, &
                            TableLookup(TableNum)%NumX2Vars
    ELSEIF(NumIVars .EQ. 3)THEN
      WRITE(CharTableData,160)NumIVars,TableLookup(TableNum)%NumX1Vars, &
                            TableLookup(TableNum)%NumX2Vars, &
                            TableLookup(TableNum)%NumX3Vars
    ELSEIF(NumIVars .EQ. 4)THEN
      WRITE(CharTableData,160)NumIVars,TableLookup(TableNum)%NumX1Vars, &
                            TableLookup(TableNum)%NumX2Vars, &
                            TableLookup(TableNum)%NumX3Vars, &
                            TableLookup(TableNum)%NumX4Vars
    ELSE
      WRITE(CharTableData,160)NumIVars,TableLookup(TableNum)%NumX1Vars, &
                            TableLookup(TableNum)%NumX2Vars, &
                            TableLookup(TableNum)%NumX3Vars, &
                            TableLookup(TableNum)%NumX4Vars, &
                            TableLookup(TableNum)%NumX5Vars
    END IF

160 FORMAT(1X,10(I2,2X))
    WRITE(OutputFileInits,fmtA)TRIM(CharTableData)
  END IF

  ALLOCATE(TableLookup(TableNum)%X1Var(TableLookup(TableNum)%NumX1Vars))
  ALLOCATE(TableLookup(TableNum)%X2Var(TableLookup(TableNum)%NumX2Vars))
  ALLOCATE(TableLookup(TableNum)%X3Var(TableLookup(TableNum)%NumX3Vars))
  ALLOCATE(TableLookup(TableNum)%X4Var(TableLookup(TableNum)%NumX4Vars))
  ALLOCATE(TableLookup(TableNum)%X5Var(TableLookup(TableNum)%NumX5Vars))

IF(NumIVars .GT. 0)THEN
  IF(ReadFromfile)THEN

    READ(FileNum,*,IOSTAT=ReadStat)(TableLookup(TableNum)%X1Var(I), &
         I=1,TableLookup(TableNum)%NumX1Vars)

    IF(EchoTableDataToEio)THEN
      WRITE(CharTableData,*)(TableLookup(TableNum)%X1Var(I), &
                         I=1,TableLookup(TableNum)%NumX1Vars)
      WRITE(OutputFileInits,fmtA)TRIM(CharTableData)
    END IF

    IF(ReadStat < GoodIOStatValue) GO TO 1000

  ELSE

    IF(NumNumbers .GE. BaseOffset + NumIVars + TableLookup(TableNum)%NumX1Vars)THEN
      DO I = 1, TableLookup(TableNum)%NumX1Vars
        TableLookup(TableNum)%X1Var(I) = Numbers(BaseOffset + NumIVars + I)
      END DO

      IF(EchoTableDataToEio)THEN
        WRITE(CharTableData,*)(TableLookup(TableNum)%X1Var(I), &
                           I=1,TableLookup(TableNum)%NumX1Vars)
        WRITE(OutputFileInits,fmtA)TRIM(CharTableData)
      END IF

    ELSE
      CALL ShowSevereError('ReadTableData: For '//TRIM(CurrentModuleObject)//': '//TRIM(Alphas(1)))
      CALL ShowContinueError('...The number of numeric inputs is less than expected.')
      ErrorsFound=.true.
    END IF
  END IF

  ! check to make sure XVars are in increaseing (ascending) order
  DO I = 1, TableLookup(TableNum)%NumX1Vars
    IF(I.EQ. 1)CYCLE
    IF(TableLookup(TableNum)%X1Var(I) .LE. &
       TableLookup(TableNum)%X1Var(I-1))THEN
      CALL ShowSevereError('ReadTableData: For '//TRIM(CurrentModuleObject)//': '//TRIM(Alphas(1)))
      CALL ShowContinueError('...The values for the independent variable X1 must be in increasing (ascending) order.')
      ErrorsFound=.true.
      EXIT
    END IF
  END DO

  IF(NumIVars .GT. 1)THEN
    IF(ReadFromfile)THEN
      READ(FileNum,*,IOSTAT=ReadStat)(TableLookup(TableNum)%X2Var(I), &
         I=1,TableLookup(TableNum)%NumX2Vars)

      IF(EchoTableDataToEio)THEN
        WRITE(CharTableData,*)(TableLookup(TableNum)%X2Var(I), &
                           I=1,TableLookup(TableNum)%NumX2Vars)
        WRITE(OutputFileInits,fmtA)TRIM(CharTableData)
      END IF

      IF(ReadStat < GoodIOStatValue) GO TO 1000

    ELSE
      IF(NumNumbers .GE. BaseOffset + NumIVars + TableLookup(TableNum)%NumX1Vars + &
            TableLookup(TableNum)%NumX2Vars)THEN
        DO I = 1, TableLookup(TableNum)%NumX2Vars
          TableLookup(TableNum)%X2Var(I) = Numbers(BaseOffset + NumIVars + &
              TableLookup(TableNum)%NumX1Vars + I)
        END DO

        IF(EchoTableDataToEio)THEN
          WRITE(CharTableData,*)(TableLookup(TableNum)%X2Var(I), &
                             I=1,TableLookup(TableNum)%NumX2Vars)
          WRITE(OutputFileInits,fmtA)TRIM(CharTableData)
        END IF

      ELSE
        CALL ShowSevereError('ReadTableData: For '//TRIM(CurrentModuleObject)//': '//TRIM(Alphas(1)))
        CALL ShowContinueError('...The number of numeric inputs is less than expected.')
        ErrorsFound=.true.
      END IF
    END IF

    ! check to make sure XVars are in increaseing (ascending) order
    DO I = 1, TableLookup(TableNum)%NumX2Vars
      IF(I.EQ. 1)CYCLE
      IF(TableLookup(TableNum)%X2Var(I) .LE. &
         TableLookup(TableNum)%X2Var(I-1))THEN
        CALL ShowSevereError('ReadTableData: For '//TRIM(CurrentModuleObject)//': '//TRIM(Alphas(1)))
        CALL ShowContinueError('...The values for the independent variable X2 must be in increasing (ascending) order.')
        ErrorsFound=.true.
        EXIT
      END IF
    END DO

    IF(NumIVars .GT. 2)THEN
      IF(ReadFromfile)THEN
        READ(FileNum,*,IOSTAT=ReadStat)(TableLookup(TableNum)%X3Var(I), &
         I=1,TableLookup(TableNum)%NumX3Vars)

        IF(EchoTableDataToEio)THEN
          WRITE(CharTableData,*)(TableLookup(TableNum)%X3Var(I), &
                             I=1,TableLookup(TableNum)%NumX3Vars)
          WRITE(OutputFileInits,fmtA)TRIM(CharTableData)
        END IF

        IF(ReadStat < GoodIOStatValue) GO TO 1000

      ELSE
        IF(NumNumbers .GE. BaseOffset + NumIVars + TableLookup(TableNum)%NumX1Vars + &
            TableLookup(TableNum)%NumX2Vars + &
            TableLookup(TableNum)%NumX3Vars)THEN

          DO I = 1, TableLookup(TableNum)%NumX3Vars
            TableLookup(TableNum)%X3Var(I) = Numbers(BaseOffset + NumIVars + &
              TableLookup(TableNum)%NumX1Vars + &
              TableLookup(TableNum)%NumX2Vars + I)
          END DO

          IF(EchoTableDataToEio)THEN
            WRITE(CharTableData,*)(TableLookup(TableNum)%X3Var(I), &
                               I=1,TableLookup(TableNum)%NumX3Vars)
            WRITE(OutputFileInits,fmtA)TRIM(CharTableData)
          END IF

        ELSE
          CALL ShowSevereError('ReadTableData: For '//TRIM(CurrentModuleObject)//': '//TRIM(Alphas(1)))
          CALL ShowContinueError('...The number of numeric inputs is less than expected.')
          ErrorsFound=.true.
        END IF
      END IF

      ! check to make sure XVars are in increaseing (ascending) order
      DO I = 1, TableLookup(TableNum)%NumX3Vars
        IF(I.EQ. 1)CYCLE
        IF(TableLookup(TableNum)%X3Var(I) .LE. &
           TableLookup(TableNum)%X3Var(I-1))THEN
          CALL ShowSevereError('ReadTableData: For '//TRIM(CurrentModuleObject)//': '//TRIM(Alphas(1)))
          CALL ShowContinueError('...The values for the independent variable X3 must be in increasing (ascending) order.')
          ErrorsFound=.true.
          EXIT
        END IF
      END DO

      IF(NumIVars .GT. 3)THEN
        IF(ReadFromfile)THEN

          READ(FileNum,*,IOSTAT=ReadStat)(TableLookup(TableNum)%X4Var(I), &
           I=1,TableLookup(TableNum)%NumX4Vars)

          IF(EchoTableDataToEio)THEN
            WRITE(CharTableData,*)(TableLookup(TableNum)%X4Var(I), &
                               I=1,TableLookup(TableNum)%NumX4Vars)
            WRITE(OutputFileInits,fmtA)TRIM(CharTableData)
          END IF

          IF(ReadStat < GoodIOStatValue) GO TO 1000

        ELSE
          IF(NumNumbers .GE. BaseOffset + NumIVars + TableLookup(TableNum)%NumX1Vars + &
              TableLookup(TableNum)%NumX2Vars + &
              TableLookup(TableNum)%NumX3Vars + &
              TableLookup(TableNum)%NumX4Vars)THEN

            DO I = 1, TableLookup(TableNum)%NumX4Vars
              TableLookup(TableNum)%X4Var(I) = Numbers(BaseOffset + NumIVars + &
                TableLookup(TableNum)%NumX1Vars + &
                TableLookup(TableNum)%NumX2Vars + &
                TableLookup(TableNum)%NumX3Vars + I)
            END DO

            IF(EchoTableDataToEio)THEN
              WRITE(CharTableData,*)(TableLookup(TableNum)%X4Var(I), &
                                 I=1,TableLookup(TableNum)%NumX4Vars)
              WRITE(OutputFileInits,fmtA)TRIM(CharTableData)
            END IF

          ELSE
            CALL ShowSevereError('ReadTableData: For '//TRIM(CurrentModuleObject)//': '//TRIM(Alphas(1)))
            CALL ShowContinueError('...The number of numeric inputs is less than expected.')
            ErrorsFound=.true.
          END IF
        END IF

        ! check to make sure XVars are in increaseing (ascending) order
        DO I = 1, TableLookup(TableNum)%NumX4Vars
          IF(I.EQ. 1)CYCLE
          IF(TableLookup(TableNum)%X4Var(I) .LE. &
             TableLookup(TableNum)%X4Var(I-1))THEN
            CALL ShowSevereError('ReadTableData: For '//TRIM(CurrentModuleObject)//': '//TRIM(Alphas(1)))
            CALL ShowContinueError('...The values for the independent variable X4 must be in increasing (ascending) order.')
            ErrorsFound=.true.
            EXIT
          END IF
        END DO

        IF(NumIVars .GT. 4)THEN
          IF(ReadFromfile)THEN
            READ(FileNum,*,IOSTAT=ReadStat)(TableLookup(TableNum)%X5Var(I), &
             I=1,TableLookup(TableNum)%NumX5Vars)

            IF(EchoTableDataToEio)THEN
              WRITE(CharTableData,*)(TableLookup(TableNum)%X5Var(I), &
                                 I=1,TableLookup(TableNum)%NumX5Vars)
              WRITE(OutputFileInits,fmtA)TRIM(CharTableData)
            END IF

            IF(ReadStat < GoodIOStatValue) GO TO 1000

          ELSE
            IF(NumNumbers .GE. BaseOffset + NumIVars + TableLookup(TableNum)%NumX1Vars + &
                TableLookup(TableNum)%NumX2Vars + &
                TableLookup(TableNum)%NumX3Vars + &
                TableLookup(TableNum)%NumX4Vars + &
                TableLookup(TableNum)%NumX5Vars)THEN

              DO I = 1, TableLookup(TableNum)%NumX5Vars
                TableLookup(TableNum)%X5Var(I) = Numbers(BaseOffset + NumIVars + &
                  TableLookup(TableNum)%NumX1Vars + &
                  TableLookup(TableNum)%NumX2Vars + &
                  TableLookup(TableNum)%NumX3Vars + &
                  TableLookup(TableNum)%NumX4Vars + I)
              END DO

              IF(EchoTableDataToEio)THEN
                WRITE(CharTableData,*)(TableLookup(TableNum)%X5Var(I), &
                                   I=1,TableLookup(TableNum)%NumX5Vars)
                WRITE(OutputFileInits,fmtA)TRIM(CharTableData)
              END IF

            ELSE
              CALL ShowSevereError('ReadTableData: For '//TRIM(CurrentModuleObject)//': '//TRIM(Alphas(1)))
              CALL ShowContinueError('...The number of numeric inputs is less than expected.')
              ErrorsFound=.true.
            END IF
          END IF
          ! check to make sure XVars are in increaseing (ascending) order
          DO I = 1, TableLookup(TableNum)%NumX5Vars
            IF(I.EQ. 1)CYCLE
            IF(TableLookup(TableNum)%X5Var(I) .LE. &
               TableLookup(TableNum)%X5Var(I-1))THEN
              CALL ShowSevereError('ReadTableData: For '//TRIM(CurrentModuleObject)//': '//TRIM(Alphas(1)))
              CALL ShowContinueError('...The values for the independent variable X5 must be in increasing (ascending) order.')
              ErrorsFound=.true.
              EXIT
            END IF
          END DO

          ALLOCATE(TableLookup(TableNum)%TableLookupZData &
            (TableLookup(TableNum)%NumX1Vars, &
             TableLookup(TableNum)%NumX2Vars, &
             TableLookup(TableNum)%NumX3Vars, &
             TableLookup(TableNum)%NumX4Vars, &
             TableLookup(TableNum)%NumX5Vars))
        ELSE
          ALLOCATE(TableLookup(TableNum)%TableLookupZData &
           (TableLookup(TableNum)%NumX1Vars, &
            TableLookup(TableNum)%NumX2Vars, &
            TableLookup(TableNum)%NumX3Vars, &
            TableLookup(TableNum)%NumX4Vars,1))
        END IF
      ELSE
        ALLOCATE(TableLookup(TableNum)%TableLookupZData &
          (TableLookup(TableNum)%NumX1Vars, &
           TableLookup(TableNum)%NumX2Vars, &
           TableLookup(TableNum)%NumX3Vars,1,1))
      END IF
    ELSE
      ALLOCATE(TableLookup(TableNum)%TableLookupZData &
          (TableLookup(TableNum)%NumX1Vars, &
           TableLookup(TableNum)%NumX2Vars,1,1,1))
    END IF
  ELSE
    ALLOCATE(TableLookup(TableNum)%TableLookupZData &
          (TableLookup(TableNum)%NumX1Vars,1,1,1,1))
  END IF
END IF

TotalDataSets = 1
DataSetCount = 0
IF(NumIVars .EQ. 3)TotalDataSets = TableLookup(TableNum)%NumX3Vars
IF(NumIVars .EQ. 4)TotalDataSets = TableLookup(TableNum)%NumX3Vars * &
                                   TableLookup(TableNum)%NumX4Vars
IF(NumIVars .EQ. 5)TotalDataSets = TableLookup(TableNum)%NumX3Vars * &
                                   TableLookup(TableNum)%NumX4Vars * &
                                   TableLookup(TableNum)%NumX5Vars

NumbersOffset = 15 + NumIVars + TableLookup(TableNum)%NumX1Vars + &
                                TableLookup(TableNum)%NumX2Vars + &
                                TableLookup(TableNum)%NumX3Vars + &
                                TableLookup(TableNum)%NumX4Vars + &
                                TableLookup(TableNum)%NumX5Vars + 1

! initialize NumX2Vars to 1 so the DO loops work correctly
IF(NumIVars .EQ. 1)TableLookup(TableNum)%NumX2Vars = 1

DO NumDataSets = 1, TotalDataSets

  IF(NumIVars .EQ. 3)THEN
    IF(ReadFromfile)THEN

      READ(FileNum,*,IOSTAT=ReadStat)Var3

      IF(EchoTableDataToEio)THEN
        WRITE(CharTableData,*)Var3
        WRITE(OutputFileInits,fmtA)TRIM(CharTableData)
      END IF

      IF(ReadStat < GoodIOStatValue) GO TO 1000

    ELSE
      Var3 = Numbers(NumbersOffset)
      NumbersOffset = NumbersOffset + 1

      IF(EchoTableDataToEio)THEN
        WRITE(CharTableData,*)Var3
        WRITE(OutputFileInits,fmtA)TRIM(CharTableData)
      END IF

    END IF
  ELSE IF(NumIVars .EQ. 4)THEN
    IF(ReadFromfile)THEN

      READ(FileNum,*,IOSTAT=ReadStat)Var3, Var4

      IF(EchoTableDataToEio)THEN
        WRITE(CharTableData,*)Var3,'  ',Var4
        WRITE(OutputFileInits,fmtA)TRIM(CharTableData)
      END IF

      IF(ReadStat < GoodIOStatValue) GO TO 1000

    ELSE
      Var3 = Numbers(NumbersOffset)
      Var4 = Numbers(NumbersOffset + 1)
      NumbersOffset = NumbersOffset + 2

      IF(EchoTableDataToEio)THEN
        WRITE(CharTableData,*)Var3,'  ',Var4
        WRITE(OutputFileInits,fmtA)TRIM(CharTableData)
      END IF

    END IF
  ELSE IF(NumIVars .EQ. 5)THEN
    IF(ReadFromfile)THEN

      READ(FileNum,*,IOSTAT=ReadStat)Var3, Var4, Var5

      IF(EchoTableDataToEio)THEN
        WRITE(CharTableData,*)Var3,'  ',Var4,'  ',Var5
        WRITE(OutputFileInits,fmtA)TRIM(CharTableData)
      END IF

      IF(ReadStat < GoodIOStatValue) GO TO 1000

    ELSE
      Var3 = Numbers(NumbersOffset)
      Var4 = Numbers(NumbersOffset + 1)
      Var5 = Numbers(NumbersOffset + 2)
      NumbersOffset = NumbersOffset + 3

      IF(EchoTableDataToEio)THEN
        WRITE(CharTableData,*)Var3,'  ',Var4,'  ',Var5
        WRITE(OutputFileInits,fmtA)TRIM(CharTableData)
      END IF

    END IF
  END IF

  IF(NumIVars .GT. 2)THEN
    Var3Index = 0
    ! match the independent variable values to the allowed values to find the index. Input must match allowed values.
    DO I = 1, TableLookup(TableNum)%NumX3Vars
      IF(Var3 .NE. TableLookup(TableNum)%X3Var(I))CYCLE
      Var3Index = I
      EXIT
    END DO
    IF(Var3Index .EQ. 0)THEN
      CALL ShowSevereError('GetTableDataFile: For Table:MultiVariableLookup "'//TRIM(PerfCurve(CurveNum)%Name)//'" ')
      CALL ShowContinueError('...The value of the 3rd independent variable ('//TRIM(RoundSigDigits(Var3,9))//')'// &
                             ' does not match the values listed as valid entries for this independent variable.')
      CALL ShowContinueError('...Valid entries are: ')
          IF(TableLookup(TableNum)%NumX3Vars .GE. 1) &
             CALL ShowContinueError('...'//TRIM(RoundSigDigits(TableLookup(TableNum)%X3Var(1),9)))
          IF(TableLookup(TableNum)%NumX3Vars .GE. 2) &
             CALL ShowContinueError('...'//TRIM(RoundSigDigits(TableLookup(TableNum)%X3Var(2),9)))
          IF(TableLookup(TableNum)%NumX3Vars .GE. 3) &
             CALL ShowContinueError('...'//TRIM(RoundSigDigits(TableLookup(TableNum)%X3Var(3),9)))
          IF(TableLookup(TableNum)%NumX3Vars .GE. 4) &
             CALL ShowContinueError('...'//TRIM(RoundSigDigits(TableLookup(TableNum)%X3Var(4),9)))
          IF(TableLookup(TableNum)%NumX3Vars .GE. 5) &
             CALL ShowContinueError('...'//TRIM(RoundSigDigits(TableLookup(TableNum)%X3Var(5),9)))
          IF(TableLookup(TableNum)%NumX3Vars .GE. 6) &
             CALL ShowContinueError('...'//TRIM(RoundSigDigits(TableLookup(TableNum)%X3Var(6),9)))
          IF(TableLookup(TableNum)%NumX3Vars .GE. 7) &
             CALL ShowContinueError('...'//TRIM(RoundSigDigits(TableLookup(TableNum)%X3Var(7),9)))
          IF(TableLookup(TableNum)%NumX3Vars .GE. 8) &
             CALL ShowContinueError('...'//TRIM(RoundSigDigits(TableLookup(TableNum)%X3Var(8),9)))
          IF(TableLookup(TableNum)%NumX3Vars .GE. 9) &
             CALL ShowContinueError('...'//TRIM(RoundSigDigits(TableLookup(TableNum)%X3Var(9),9)))
          IF(TableLookup(TableNum)%NumX3Vars .GE. 10) &
             CALL ShowContinueError('...'//TRIM(RoundSigDigits(TableLookup(TableNum)%X3Var(10),9)))
      CALL ShowContinueError('...This occurs for data set = '//TRIM(RoundSigDigits(DataSetCount + 1,0))//' in data file = '// &
                             TRIM(FileName))
      ErrorsFound = .TRUE.
      Var3Index = 1
    END IF
  ELSE
    Var3Index = 1
  END IF

  IF(NumIVars .GT. 3)THEN
    Var4Index = 0
    ! match the independent variable values to the allowed values to find the index. Input must match allowed values.
    DO I = 1, TableLookup(TableNum)%NumX4Vars
      IF(Var4 .NE. TableLookup(TableNum)%X4Var(I))CYCLE
      Var4Index = I
      EXIT
    END DO
    IF(Var4Index .EQ. 0)THEN
      CALL ShowSevereError('GetTableDataFile: For Table:MultiVariableLookup "'//TRIM(PerfCurve(CurveNum)%Name)//'" ')
      CALL ShowContinueError('...The value of the 4th independent variable ('//TRIM(RoundSigDigits(Var4,9))//')'// &
                             ' does not match the values listed as valid entries for this independent variable.')
      CALL ShowContinueError('...Valid entries are: ')
          IF(TableLookup(TableNum)%NumX4Vars .GE. 1) &
             CALL ShowContinueError('...'//TRIM(RoundSigDigits(TableLookup(TableNum)%X4Var(1),9)))
          IF(TableLookup(TableNum)%NumX4Vars .GE. 2) &
             CALL ShowContinueError('...'//TRIM(RoundSigDigits(TableLookup(TableNum)%X4Var(2),9)))
          IF(TableLookup(TableNum)%NumX4Vars .GE. 3) &
             CALL ShowContinueError('...'//TRIM(RoundSigDigits(TableLookup(TableNum)%X4Var(3),9)))
          IF(TableLookup(TableNum)%NumX4Vars .GE. 4) &
             CALL ShowContinueError('...'//TRIM(RoundSigDigits(TableLookup(TableNum)%X4Var(4),9)))
          IF(TableLookup(TableNum)%NumX4Vars .GE. 5) &
             CALL ShowContinueError('...'//TRIM(RoundSigDigits(TableLookup(TableNum)%X4Var(5),9)))
          IF(TableLookup(TableNum)%NumX4Vars .GE. 6) &
             CALL ShowContinueError('...'//TRIM(RoundSigDigits(TableLookup(TableNum)%X4Var(6),9)))
          IF(TableLookup(TableNum)%NumX4Vars .GE. 7) &
             CALL ShowContinueError('...'//TRIM(RoundSigDigits(TableLookup(TableNum)%X4Var(7),9)))
          IF(TableLookup(TableNum)%NumX4Vars .GE. 8) &
             CALL ShowContinueError('...'//TRIM(RoundSigDigits(TableLookup(TableNum)%X4Var(8),9)))
          IF(TableLookup(TableNum)%NumX4Vars .GE. 9) &
             CALL ShowContinueError('...'//TRIM(RoundSigDigits(TableLookup(TableNum)%X4Var(9),9)))
          IF(TableLookup(TableNum)%NumX4Vars .GE. 10) &
             CALL ShowContinueError('...'//TRIM(RoundSigDigits(TableLookup(TableNum)%X4Var(10),9)))
      CALL ShowContinueError('...This occurs for data set = '//TRIM(RoundSigDigits(DataSetCount + 1,0))//' in data file = '// &
                             TRIM(FileName))
      ErrorsFound = .TRUE.
      Var4Index = 1
    END IF
  ELSE
    Var4Index = 1
  END IF

  IF(NumIVars .GT. 4)THEN
    Var5Index = 0
    ! match the independent variable values to the allowed values to find the index. Input must match allowed values.
    DO I = 1, TableLookup(TableNum)%NumX5Vars
      IF(Var5 .NE. TableLookup(TableNum)%X5Var(I))CYCLE
      Var5Index = I
      EXIT
    END DO
    IF(Var5Index .EQ. 0 .AND. NumIVars .GT. 4)THEN
      CALL ShowSevereError('GetTableDataFile: For Table:MultiVariableLookup "'//TRIM(PerfCurve(CurveNum)%Name)//'" ')
      CALL ShowContinueError('...The value of the 5th independent variable ('//TRIM(RoundSigDigits(Var5,9))//')'// &
                             ' does not match the values listed as valid entries for this independent variable.')
      CALL ShowContinueError('...Valid entries are: ')
          IF(TableLookup(TableNum)%NumX5Vars .GE. 1) &
             CALL ShowContinueError('...'//TRIM(RoundSigDigits(TableLookup(TableNum)%X5Var(1),9)))
          IF(TableLookup(TableNum)%NumX5Vars .GE. 2) &
             CALL ShowContinueError('...'//TRIM(RoundSigDigits(TableLookup(TableNum)%X5Var(2),9)))
          IF(TableLookup(TableNum)%NumX5Vars .GE. 3) &
             CALL ShowContinueError('...'//TRIM(RoundSigDigits(TableLookup(TableNum)%X5Var(3),9)))
          IF(TableLookup(TableNum)%NumX5Vars .GE. 4) &
             CALL ShowContinueError('...'//TRIM(RoundSigDigits(TableLookup(TableNum)%X5Var(4),9)))
          IF(TableLookup(TableNum)%NumX5Vars .GE. 5) &
             CALL ShowContinueError('...'//TRIM(RoundSigDigits(TableLookup(TableNum)%X5Var(5),9)))
          IF(TableLookup(TableNum)%NumX5Vars .GE. 6) &
             CALL ShowContinueError('...'//TRIM(RoundSigDigits(TableLookup(TableNum)%X5Var(6),9)))
          IF(TableLookup(TableNum)%NumX5Vars .GE. 7) &
             CALL ShowContinueError('...'//TRIM(RoundSigDigits(TableLookup(TableNum)%X5Var(7),9)))
          IF(TableLookup(TableNum)%NumX5Vars .GE. 8) &
             CALL ShowContinueError('...'//TRIM(RoundSigDigits(TableLookup(TableNum)%X5Var(8),9)))
          IF(TableLookup(TableNum)%NumX5Vars .GE. 9) &
             CALL ShowContinueError('...'//TRIM(RoundSigDigits(TableLookup(TableNum)%X5Var(9),9)))
          IF(TableLookup(TableNum)%NumX5Vars .GE. 10) &
             CALL ShowContinueError('...'//TRIM(RoundSigDigits(TableLookup(TableNum)%X5Var(10),9)))
      CALL ShowContinueError('...This occurs for data set = '//TRIM(RoundSigDigits(DataSetCount + 1,0))//' in data file = '// &
                             TRIM(FileName))
      ErrorsFound = .TRUE.
      Var5Index = 1
    END IF
  ELSE
    Var5Index = 1
  END IF

    ! now read in X1 | X2 matrix data set
  IF(PerfCurve(CurveNum)%X1SortOrder == ASCENDING)THEN
    IF(PerfCurve(CurveNum)%X2SortOrder == ASCENDING)THEN
      DO J = 1, TableLookup(TableNum)%NumX2Vars
        IF(ReadFromFile)THEN

          READ(FileNum,*,IOSTAT=ReadStat) &
            (TableLookup(TableNum)%TableLookupZData(I,J,Var3Index,Var4Index,Var5Index), &
              I=1,TableLookup(TableNum)%NumX1Vars)

          IF(ReadStat < GoodIOStatValue) GO TO 1000

        ELSE

          DO I = 1,TableLookup(TableNum)%NumX1Vars
            TableLookup(TableNum)%TableLookupZData(I,J,Var3Index,Var4Index,Var5Index)= &
            Numbers(NumbersOffset)
            NumbersOffset = NumbersOffset + 1
          END DO

        END IF
      END DO
    ELSE ! PerfCurve(CurveNum)%X2SortOrder == DESCENDING

      DO J = TableLookup(TableNum)%NumX2Vars, 1, -1

        IF(ReadFromFile)THEN

          READ(FileNum,*,IOSTAT=ReadStat) &
           (TableLookup(TableNum)%TableLookupZData(I,J,Var3Index,Var4Index,Var5Index), &
             I=1,TableLookup(TableNum)%NumX1Vars)

           IF(ReadStat < GoodIOStatValue) GO TO 1000

        ELSE

          DO I = 1,TableLookup(TableNum)%NumX1Vars
            TableLookup(TableNum)%TableLookupZData(I,J,Var3Index,Var4Index,Var5Index)= &
              Numbers(NumbersOffset)/TableData(TableNum)%NormalPoint
            NumbersOffset = NumbersOffset + 1
          END DO

         END IF
      END DO
    END IF
  ELSE ! PerfCurve(CurveNum)%X1SortOrder == DESCENDING
    IF(PerfCurve(CurveNum)%X2SortOrder == ASCENDING)THEN

      DO J = 1, TableLookup(TableNum)%NumX2Vars

        IF(ReadFromFile)THEN

          READ(FileNum,*,IOSTAT=ReadStat) &
            (TableLookup(TableNum)%TableLookupZData(I,J,Var3Index,Var4Index,Var5Index), &
              I=TableLookup(TableNum)%NumX1Vars,1,-1)

          IF(ReadStat < GoodIOStatValue) GO TO 1000

        ELSE

          DO I = TableLookup(TableNum)%NumX1Vars,1,-1
            TableLookup(TableNum)%TableLookupZData(I,J,Var3Index,Var4Index,Var5Index)= &
            Numbers(NumbersOffset)
            NumbersOffset = NumbersOffset + 1
          END DO

         END IF
      END DO
    ELSE
      DO J = TableLookup(TableNum)%NumX2Vars, 1, -1

        IF(ReadFromFile)THEN

          READ(FileNum,*,IOSTAT=ReadStat) &
           (TableLookup(TableNum)%TableLookupZData(I,J,Var3Index,Var4Index,Var5Index), &
             I=TableLookup(TableNum)%NumX1Vars,1,-1)

          IF(ReadStat < GoodIOStatValue) GO TO 1000

        ELSE

          DO I = TableLookup(TableNum)%NumX1Vars,1,-1
            TableLookup(TableNum)%TableLookupZData(I,J,Var3Index,Var4Index,Var5Index)= &
            Numbers(NumbersOffset)
            NumbersOffset = NumbersOffset + 1
          END DO

        END IF
      END DO
    END IF
  END IF

  DO J = 1, TableLookup(TableNum)%NumX2Vars

      ! write data to eio file in ascending order
      IF(EchoTableDataToEio)THEN
        WRITE(CharTableData,*) &
         (TableLookup(TableNum)%TableLookupZData(I,J,Var3Index,Var4Index,Var5Index), &
            I=1,TableLookup(TableNum)%NumX1Vars)
        WRITE(OutputFileInits,fmtA)TRIM(CharTableData)
      END IF

     ! normalize the data according to the user entered normal point
      DO I=1,TableLookup(TableNum)%NumX1Vars
        TableLookup(TableNum)%TableLookupZData(I,J,Var3Index,Var4Index,Var5Index) = &
          TableLookup(TableNum)%TableLookupZData(I,J,Var3Index,Var4Index,Var5Index)/ &
            TableData(TableNum)%NormalPoint
      END DO

  END DO

  DataSetCount = DataSetCount + 1

END DO

IF(EchoTableDataToEio)THEN
  WRITE(OutputFileInits,131)
END IF

 1000 EOFonFile = .TRUE.
 IF(ReadFromFile) CLOSE (FileNum)

IF(TotalDataSets .LT. DataSetCount)THEN
  CALL ShowSevereError('GetTableDataFile: For Table:MultiVariableLookup "'//TRIM(PerfCurve(CurveNum)%Name)//'" ')
  CALL ShowContinueError('...The required number of data sets ('//trim(RoundSigDigits(TotalDataSets))//  &
     ') is less than the number determined by the number and count of independent variables ('//  &
     trim(RoundSigDigits(DataSetCount))//').')
END IF

RETURN

999   CALL ShowSevereError('CurveManager: SearchTableDataFile: '//   &
       'Could not open Table Data File, expecting it as file name = '//TRIM(FileName))
  CALL ShowContinueError('Certain run environments require a full path to be included with the file name in the input field.')
  CALL ShowContinueError('Try again with putting full path and file name in the field.')
  CALL ShowFatalError('Program terminates due to these conditions.')

END SUBROUTINE ReadTableData

FUNCTION DLAG(XX,YY,X,Y,Z,NX,NY,M,IEXTX,IEXTY)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         AUTHOR: F.D.HAMMERLING, COMPUTING TECHNOLOGY CENTER, ORNL
          !       DATE WRITTEN   2010
          !       MODIFIED       Richard Raustad, FSEC
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
!
!
!         DLAG IS A DOUBLE-PRECISION, TWO-DIMENSIONAL,
!         LAGRANGIAN INTERPOLATION
!
!           INPUT VARIABLES:
!             XX        X-COORDINATE OF THE DESIRED INTERPOLATED POINT
!             YY        Y-COORDINATE OF THE DESIRED INTERPOLATED POINT
!             X         SINGLY DIMENSIONED ARRAY OF X-COORDINATES
!             Y         SINGLY DIMENSIONED ARRAY OF Y-COORDINATES
!             Z         DOUBLY DIMENSIONED ARRAY OF FUNCTION VALUES,
!                       I.E. Z(I,J) = F( X(I), Y(J) )
!             NX        NUMBER OF ELEMENTS IN THE X-ARRAY
!             NY        NUMBER OF ELEMENTS IN THE Y-ARRAY
!             M         THE SQUARE ROOT OF THE NUMBER OF POINTS TO BE
!                       CONSIDERED IN THE INTERPOLATION - NUMBER OF
!                       POINTS IN EACH DIRECTION
!             ID        THE FIRST DIMENSION OF THE Z-ARRAY (AT LEAST NX)
!
!           OUTPUT VARIABLES:
!             IEXTX     =1, IF EXTRAPOLATION OCCURED ABOVE THE X-ARRAY
!                       =0, IF INTERPOLATION OCCURED
!                       =-1, IF EXTRAPOLATION OCCURED BELOW THE X-ARRAY
!             IEXTY     SAME FOR THE Y-ARRAY AS IEXTX IS FOR THE X-ARRAY
!
!      THIS PROGRAM WAS MODIFIED AUGUST 1984 BY CJ EMERSON TO INSURE
!      THAT ITERATIVE CALLS TO DLAG USE THE SAME POINTS FOR INTERPOLATION.
!      ISXPT(ISYPT) ARE CHOSEN FROM THE UPPER END OF THE INTERVAL.
!      ALSO THE PROGRAM WAS MODIFIED SO THAT EXTRAPOLATION ALWAYS USES
!      AT MOST TWO POINTS.
!
!
          !
          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
REAL(r64) :: DLAG
REAL(r64) :: XX, YY
REAL(r64), DIMENSION(:) :: X, Y
REAL(r64), DIMENSION(:,:) :: Z
!DIMENSION Z(ID,NY),X(NX),Y(NY),XLAG(100)
INTEGER   :: NX, NY, IEXTX, IEXTY, M
          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:!
LOGICAL   :: QUITX, QUITY
INTEGER   :: I, ISXPT, IEXPT, J, ISYPT, IEYPT, K, L, M1
REAL(r64) :: MIDX, MIDY
REAL(r64), ALLOCATABLE, DIMENSION(:) :: XLAG, YLAG

!       INITIALIZE
!
        L = SIZE(Z(:,1))
        L = MAX(L,SIZE(Z(1,:)))+1
        ALLOCATE(XLAG(L))
        ALLOCATE(YLAG(L))
        QUITX=.FALSE.
        QUITY=.FALSE.
        IEXTX=0
        IEXTY=0
        L=0
!
!       The following code has been upgraded to current Fortran standards
!       See Starteam Revision 33, August 17, 2010 for legacy code if comparison is needed
!
!       FIND THE RANGE OF INTERPOLATION ALONG X
!
        M1=M                          ! number of points to be interpolated (X direction is first)
        IF (M1 .GT. NX) M1=NX         ! limit to number of X points if necessary

!       loop through X data and find the first x-coordinate less than the interpolated point
!       if the interpolation point is less than or greater than the X data then linearly extrapolate
!       linear extrapolation uses only 2 points (M1=2)
        DO I = 1, NX
          IF(XX-X(I) .LT. 0.0D0)THEN
            MIDX=I                    ! found X point just greater than interpolation point
            IF (MIDX .EQ. 1) THEN
              IEXTX=-1                ! extrapolating at the lower bound of x
              IF(M1 .GT. 2)M1=2       ! limit to linear extrapolation
            END IF
            ISXPT=MIDX-((M1+1)/2)     ! calculate starting point in X array
            IF (ISXPT .LE. 0)ISXPT=1  ! limit to first element in X array
            IEXPT=ISXPT+M1-1          ! calculate ending point in X array
            IF (IEXPT .GT. NX) THEN
              ISXPT=NX-M1+1           ! if upper X array boundary exceeded, recalculate starting point
              IEXPT=NX                ! limit ending point to upper boundary of X array
            END IF
            EXIT
          ELSE IF(XX-X(I) .EQ. 0.0D0)THEN ! interpolation point is equal to element in X array
            QUITX=.TRUE.              ! exact interpolation point found in X array, do not interpolate
            EXIT
          ELSE IF(I .EQ. NX)THEN      ! interpolation point is greater than max X value
            IEXTX=1                   ! extrapolating at the upper bound of X
            IF(M1 .GT. 2)M1=2         ! limit to linear extrapolation
            ISXPT=NX-M1+1             ! calculate starting point in X array
            IEXPT=NX                  ! ending point equals upper bound of X array
            EXIT
          END IF
        END DO

        M1=M                          ! number of points to be interpolated (Y direction is second)
        IF (M1 .GT. NY) M1=NY         ! limit to number of Y points if necessary

        DO J=1,NY
          IF(YY-Y(J) .LT. 0.0D0)THEN
            MIDY=J                    ! found Y point just greater than interpolation point
            IF (MIDY .LE. 1) THEN
              IEXTY=-1                ! extrapolating at the lower bound of y
              IF(M1 .GT. 2)M1=2       ! limit to linear extrapolation
            END IF
            ISYPT=MIDY-((M1+1)/2)     ! calculate starting point in Y array
            IF (ISYPT .LE. 0) ISYPT=1 ! limit to first element in array
            IEYPT=ISYPT+M1-1          ! calculate ending point in X array
            IF (IEYPT .GT. NY) THEN
              ISYPT=NY-M1+1           ! if upper Y array boundary exceeded, recalculate starting point
              IEYPT=NY                ! limit ending point to upper boundary of Y array
            END IF
            EXIT
          ELSE IF(YY-Y(J) .EQ. 0.0D0)THEN ! interpolation point is equal to element in Y array
            QUITY=.TRUE.              ! exact interpolation point found in Y array, do not interpolate
            EXIT
          ELSE IF(J .EQ. NY)THEN      ! interpolation point is greater than max Y value
            IEXTY=1                   ! extrapolating at the upper bound of Y
            IF(M1 .GT. 2)M1=2         ! limit to linear extrapolation
            ISYPT=NY-M1+1             ! calculate starting point in Y array
            IEYPT=NY                  ! ending point equals upper bound of Y array
            EXIT
          END IF
        END DO

        IF (QUITX .AND. QUITY) THEN
          DLAG=Z(I,J)                 ! found exact X and Y point in Z array
        ELSE IF (QUITX .AND. .NOT.QUITY) THEN ! only interpolate in Y direction
          DO L=ISYPT,IEYPT
            XLAG(L)=Z(I,L)            ! store X's at each Y (I = midpoint of array from above)
          END DO
          CALL Interpolate_Lagrange(YY,XLAG,Y,ISYPT,IEYPT,DLAG) ! now interpolate these X's
        ELSE IF (.NOT.QUITX .AND. QUITY) THEN ! only interpolate in X direction
          CALL Interpolate_Lagrange(XX,Z(:,J),X,ISXPT,IEXPT,DLAG) ! (:,J) interpolate X array at fixed Y (J here)
        ELSE                          ! else interpolate in X and Y directions
          DO K=ISYPT,IEYPT
            CALL Interpolate_Lagrange(XX,Z(:,K),X,ISXPT,IEXPT,XLAG(K)) ! (:,K) interpolate X array at all Y's (K here)
          END DO
          CALL Interpolate_Lagrange(YY,XLAG,Y,ISYPT,IEYPT,DLAG)  ! final interpolation of X array
        END IF

        DEALLOCATE(XLAG)
        DEALLOCATE(YLAG)

     RETURN
END FUNCTION DLAG

REAL(r64) FUNCTION PerformanceCurveObject(CurveIndex,Var1,Var2,Var3,Var4) RESULT(CurveValue)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   May 2000
          !       MODIFIED       Lixing Gu, July 2006; B. Griffith July 2006
          !                      22Aug2010 Craig Wray, added new curves for fan component model:
          !                          FanPressureRise, ExponentialSkewNormal, Sigmoid, RectangularHyperbola1,
          !                          RectangularHyperbola2, ExponentialDecay

          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! Given the curve index and the values of 1 or 2 independent variables,
          ! returns the value of an equipment performance curve.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER,   INTENT (IN)           :: CurveIndex  ! index of curve in curve array
  REAL(r64), INTENT (IN)           :: Var1        ! 1st independent variable
  REAL(r64), INTENT (IN), OPTIONAL :: Var2        ! 2nd independent variable
  REAL(r64), INTENT (IN), OPTIONAL :: Var3        ! 3rd independent variable
  REAL(r64), INTENT (IN), OPTIONAL :: Var4        ! 4th independent variable

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:

REAL(r64) :: V1 ! 1st independent variable after limits imposed
REAL(r64) :: V2 ! 2nd independent variable after limits imposed
REAL(r64) :: V3 ! 3rd independent variable after limits imposed
REAL(r64) :: V4 ! 4th independent variable after limits imposed
REAL(r64) :: CoeffZ1 ! cpw22Aug2010 Coefficient Z1 in exponential skew normal curve
REAL(r64) :: CoeffZ2 ! cpw22Aug2010 Coefficient Z2 in exponential skew normal curve
REAL(r64) :: CoeffZ3 ! cpw22Aug2010 Coefficient Z3 in exponential skew normal curve
REAL(r64) :: CurveValueNumer ! cpw22Aug2010 Numerator in in exponential skew normal curve
REAL(r64) :: CurveValueDenom ! cpw22Aug2010 Numerator in in exponential skew normal curve
REAL(r64) :: CurveValueExp ! cpw22Aug2010 Exponential term in sigmoid curve
#ifdef nointrinsicERF
REAL(r64), EXTERNAL :: ERF
#endif

V1 = MAX(MIN(Var1,PerfCurve(CurveIndex)%Var1Max),PerfCurve(CurveIndex)%Var1Min)

IF (PRESENT(Var2)) THEN
  V2 = MAX(MIN(Var2,PerfCurve(CurveIndex)%Var2Max),PerfCurve(CurveIndex)%Var2Min)
ELSE
  V2 = 0.0d0
END IF

IF (PRESENT(Var3)) THEN
  V3 = MAX(MIN(Var3,PerfCurve(CurveIndex)%Var3Max),PerfCurve(CurveIndex)%Var3Min)
ELSE
  V3 = 0.0d0
END IF

IF (PRESENT(Var4)) THEN
  V4 = MAX(MIN(Var4,PerfCurve(CurveIndex)%Var4Max),PerfCurve(CurveIndex)%Var4Min)
ELSE
  V4 = 0.0d0
END IF
SELECT CASE (PerfCurve(CurveIndex)%CurveType)

  CASE(Linear)

    CurveValue = PerfCurve(CurveIndex)%Coeff1 + V1*PerfCurve(CurveIndex)%Coeff2

  CASE(Quadratic)

    CurveValue = PerfCurve(CurveIndex)%Coeff1 + V1*(PerfCurve(CurveIndex)%Coeff2 + V1*PerfCurve(CurveIndex)%Coeff3)
  CASE(QuadLinear)

    CurveValue = PerfCurve(CurveIndex)%Coeff1 + V1*PerfCurve(CurveIndex)%Coeff2 + V2*PerfCurve(CurveIndex)%Coeff3 &
                + V3*PerfCurve(CurveIndex)%Coeff4 + V4*PerfCurve(CurveIndex)%Coeff5

  CASE(Cubic)

    CurveValue = PerfCurve(CurveIndex)%Coeff1 + V1*(PerfCurve(CurveIndex)%Coeff2 + &
                   V1*(PerfCurve(CurveIndex)%Coeff3 + V1*PerfCurve(CurveIndex)%Coeff4))

  CASE(Quartic)

    CurveValue = PerfCurve(CurveIndex)%Coeff1 + &
                 V1*(PerfCurve(CurveIndex)%Coeff2 + &
                     V1*(PerfCurve(CurveIndex)%Coeff3 + &
                         V1*(PerfCurve(CurveIndex)%Coeff4 + &
                             V1*PerfCurve(CurveIndex)%Coeff5)))

  CASE(Biquadratic)

    CurveValue = PerfCurve(CurveIndex)%Coeff1 + V1*(PerfCurve(CurveIndex)%Coeff2 + V1*PerfCurve(CurveIndex)%Coeff3) &
                   + V2*(PerfCurve(CurveIndex)%Coeff4 + V2*PerfCurve(CurveIndex)%Coeff5) &
                   + V1*V2*PerfCurve(CurveIndex)%Coeff6

  CASE(QuadraticLinear)

    CurveValue = (PerfCurve(CurveIndex)%Coeff1 + V1*(PerfCurve(CurveIndex)%Coeff2 + V1*PerfCurve(CurveIndex)%Coeff3)) &
                +(PerfCurve(CurveIndex)%Coeff4 + V1*(PerfCurve(CurveIndex)%Coeff5 + V1*PerfCurve(CurveIndex)%Coeff6))*V2

  CASE(Bicubic)

    CurveValue = PerfCurve(CurveIndex)%Coeff1 + V1*PerfCurve(CurveIndex)%Coeff2 + V1*V1*PerfCurve(CurveIndex)%Coeff3  &
                + V2*PerfCurve(CurveIndex)%Coeff4 + V2*V2*PerfCurve(CurveIndex)%Coeff5 + V1*V2*PerfCurve(CurveIndex)%Coeff6 &
                + V1**3*PerfCurve(CurveIndex)%Coeff7 + V2**3*PerfCurve(CurveIndex)%Coeff8 &
                + V1*V1*V2*PerfCurve(CurveIndex)%Coeff9+ V1*V2*V2*PerfCurve(CurveIndex)%Coeff10

  CASE(TriQuadratic)

    CurveValue =   PerfCurve(Curveindex)%Tri2ndOrder(1)%CoeffA0                 &
                 + PerfCurve(Curveindex)%Tri2ndOrder(1)%CoeffA1 * V1**2         &
                 + PerfCurve(Curveindex)%Tri2ndOrder(1)%CoeffA2 * V1            &
                 + PerfCurve(Curveindex)%Tri2ndOrder(1)%CoeffA3 * V2**2         &
                 + PerfCurve(Curveindex)%Tri2ndOrder(1)%CoeffA4 * V2            &
                 + PerfCurve(Curveindex)%Tri2ndOrder(1)%CoeffA5 * V3**2         &
                 + PerfCurve(Curveindex)%Tri2ndOrder(1)%CoeffA6 * V3            &
                 + PerfCurve(Curveindex)%Tri2ndOrder(1)%CoeffA7 * V1**2 * V2**2 &
                 + PerfCurve(Curveindex)%Tri2ndOrder(1)%CoeffA8 * V1 * V2       &
                 + PerfCurve(Curveindex)%Tri2ndOrder(1)%CoeffA9 * V1 * V2**2    &
                 + PerfCurve(Curveindex)%Tri2ndOrder(1)%CoeffA10 * V1**2 * V2   &
                 + PerfCurve(Curveindex)%Tri2ndOrder(1)%CoeffA11 * V1**2 * V3**2 &
                 + PerfCurve(Curveindex)%Tri2ndOrder(1)%CoeffA12 * V1 * V3       &
                 + PerfCurve(Curveindex)%Tri2ndOrder(1)%CoeffA13 * V1 * V3**2    &
                 + PerfCurve(Curveindex)%Tri2ndOrder(1)%CoeffA14 * V1**2 * V3    &
                 + PerfCurve(Curveindex)%Tri2ndOrder(1)%CoeffA15 * V2**2 * V3**2 &
                 + PerfCurve(Curveindex)%Tri2ndOrder(1)%CoeffA16 * V2 * V3       &
                 + PerfCurve(Curveindex)%Tri2ndOrder(1)%CoeffA17 * V2 * V3**2    &
                 + PerfCurve(Curveindex)%Tri2ndOrder(1)%CoeffA18 * V2**2 * V3    &
                 + PerfCurve(Curveindex)%Tri2ndOrder(1)%CoeffA19 * V1**2 * V2**2 * V3**2 &
                 + PerfCurve(Curveindex)%Tri2ndOrder(1)%CoeffA20 * V1**2 * V2**2 * V3    &
                 + PerfCurve(Curveindex)%Tri2ndOrder(1)%CoeffA21 * V1**2 * V2 * V3**2    &
                 + PerfCurve(Curveindex)%Tri2ndOrder(1)%CoeffA22 * V1 * V2**2 * V3**2    &
                 + PerfCurve(Curveindex)%Tri2ndOrder(1)%CoeffA23 * V1**2 * V2 * V3       &
                 + PerfCurve(Curveindex)%Tri2ndOrder(1)%CoeffA24 * V1 * V2**2 * V3       &
                 + PerfCurve(Curveindex)%Tri2ndOrder(1)%CoeffA25 * V1 * V2 * V3**2       &
                 + PerfCurve(Curveindex)%Tri2ndOrder(1)%CoeffA26 * V1 * V2 * V3

  CASE(Exponent)

    CurveValue = PerfCurve(CurveIndex)%Coeff1 + PerfCurve(CurveIndex)%Coeff2 * (V1**PerfCurve(CurveIndex)%Coeff3)

  !cpw22Aug2010 Added Fan Pressure Rise curve
  CASE(FanPressureRise)

    CurveValue = PerfCurve(CurveIndex)%Coeff1 * V1**2 + PerfCurve(CurveIndex)%Coeff2 * V1 &
                 + PerfCurve(CurveIndex)%Coeff3 * V1 * SQRT(V2) + PerfCurve(CurveIndex)%Coeff4 * V2

  !cpw22Aug2010 Added Exponential Skew Normal curve
  CASE(ExponentialSkewNormal)

    CoeffZ1 = (V1 - PerfCurve(CurveIndex)%Coeff1) / PerfCurve(CurveIndex)%Coeff2
    CoeffZ2 = (PerfCurve(CurveIndex)%Coeff4 * V1 * EXP(PerfCurve(CurveIndex)%Coeff3 * V1) &
              - PerfCurve(CurveIndex)%Coeff1) / PerfCurve(CurveIndex)%Coeff2
    CoeffZ3 = -PerfCurve(CurveIndex)%Coeff1 / PerfCurve(CurveIndex)%Coeff2

!    CurveValueNumer = EXP(-0.5d0 * CoeffZ1**2) * (1.d0 + SIGN(1.0d0,CoeffZ2) * ErfFunction(ABS(CoeffZ2)/SQRT(2.0d0)))
!    CurveValueDenom = EXP(-0.5d0 * CoeffZ3**2) * (1.d0 + SIGN(1.0d0,CoeffZ3) * ErfFunction(ABS(CoeffZ3)/SQRT(2.0d0)))
    CurveValueNumer = EXP(-0.5d0 * CoeffZ1**2) * (1.d0 + SIGN(1.0d0,CoeffZ2) * ERF(ABS(CoeffZ2)/SQRT(2.0d0)))
    CurveValueDenom = EXP(-0.5d0 * CoeffZ3**2) * (1.d0 + SIGN(1.0d0,CoeffZ3) * ERF(ABS(CoeffZ3)/SQRT(2.0d0)))
    CurveValue =  CurveValueNumer / CurveValueDenom

  !cpw22Aug2010 Added Sigmoid curve
  CASE(Sigmoid)

    CurveValueExp = EXP((PerfCurve(CurveIndex)%Coeff3 - V1) / PerfCurve(CurveIndex)%Coeff4)
    CurveValue = PerfCurve(CurveIndex)%Coeff1 + PerfCurve(CurveIndex)%Coeff2 &
                 / ((1.0d0 + CurveValueExp)**PerfCurve(CurveIndex)%Coeff5)

  !cpw22Aug2010 Added Rectangular Hyperbola Type 1 curve
  CASE(RectangularHyperbola1)

    CurveValueNumer = PerfCurve(CurveIndex)%Coeff1 * V1
    CurveValueDenom = PerfCurve(CurveIndex)%Coeff2 + V1
    CurveValue = (CurveValueNumer / CurveValueDenom) + PerfCurve(CurveIndex)%Coeff3

  !cpw22Aug2010 Added Rectangular Hyperbola Type 2 curve
  CASE(RectangularHyperbola2)

    CurveValueNumer = PerfCurve(CurveIndex)%Coeff1 * V1
    CurveValueDenom = PerfCurve(CurveIndex)%Coeff2 + V1
    CurveValue = (CurveValueNumer / CurveValueDenom) + (PerfCurve(CurveIndex)%Coeff3 * V1)

  !cpw22Aug2010 Added Exponential Decay curve
  CASE(ExponentialDecay)

    CurveValue = PerfCurve(CurveIndex)%Coeff1 + PerfCurve(CurveIndex)%Coeff2 &
                 * EXP(PerfCurve(CurveIndex)%Coeff3 * V1)

  !ykt Jul 2011 Added Double Exponential Decay curve
  CASE(DoubleExponentialDecay)

    CurveValue = PerfCurve(CurveIndex)%Coeff1 + PerfCurve(CurveIndex)%Coeff2 &
                 * EXP(PerfCurve(CurveIndex)%Coeff3 * V1)+PerfCurve(CurveIndex)%Coeff4*EXP(PerfCurve(CurveIndex)%Coeff5 * V1)

  CASE DEFAULT

    CurveValue = 0.0d0

END SELECT

IF(PerfCurve(CurveIndex)%CurveMinPresent) CurveValue = MAX(CurveValue , PerfCurve(CurveIndex)%CurveMin)
IF(PerfCurve(CurveIndex)%CurveMaxPresent) CurveValue = MIN(CurveValue , PerfCurve(CurveIndex)%CurveMax)

RETURN

END FUNCTION PerformanceCurveObject

REAL(r64) FUNCTION PerformanceTableObject(CurveIndex,Var1,Var2,Var3) RESULT(TableValue)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Richard Raustad, FSEC
          !       DATE WRITTEN   May 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! Given the curve index and the values of 1 or 2 independent variables,
          ! returns the value of an equipment performance table lookup.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataInterfaces, ONLY:ShowFatalError, ShowSevereError, ShowContinueError

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER,   INTENT (IN)           :: CurveIndex  ! index of curve in curve array
  REAL(r64), INTENT (IN)           :: Var1        ! 1st independent variable
  REAL(r64), INTENT (IN), OPTIONAL :: Var2        ! 2nd independent variable
  REAL(r64), INTENT (IN), OPTIONAL :: Var3        ! 3rd independent variable

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:

REAL(r64) :: V1 ! 1st independent variable after limits imposed
REAL(r64) :: V2 ! 2nd independent variable after limits imposed
REAL(r64) :: V3 ! 3rd independent variable after limits imposed
Real(r64) :: TempX1Low
Real(r64) :: TempX1High
Real(r64) :: TempX2Low
Real(r64) :: TempX2High
!INTEGER   :: ATempX1LowPtr(1)
!INTEGER   :: ATempX1HighPtr(1)
!INTEGER   :: ATempX2LowPtr(1)
!INTEGER   :: ATempX2HighPtr(1)
INTEGER   :: TempX1LowPtr
INTEGER   :: TempX1HighPtr
INTEGER   :: TempX2LowPtr
INTEGER   :: TempX2HighPtr
Real(r64) :: X1Frac
Real(r64) :: X2Frac
Real(r64) :: X1ValLow
Real(r64) :: X1ValHigh
!INTEGER   :: MaxSizeArray
INTEGER :: X1Val
INTEGER :: X2Val
INTEGER :: TableIndex

TableIndex = PerfCurve(CurveIndex)%TableIndex

V1 = MAX(MIN(Var1,PerfCurve(CurveIndex)%Var1Max),PerfCurve(CurveIndex)%Var1Min)

IF (PRESENT(Var2)) THEN
  V2 = MAX(MIN(Var2,PerfCurve(CurveIndex)%Var2Max),PerfCurve(CurveIndex)%Var2Min)
ELSE
  V2 = 0.0d0
END IF

IF (PRESENT(Var3)) THEN
  V3 = MAX(MIN(Var3,PerfCurve(CurveIndex)%Var3Max),PerfCurve(CurveIndex)%Var3Min)
ELSE
  V3 = 0.0d0
END IF

SELECT CASE(TableLookup(TableIndex)%NumIndependentVars)
  CASE(1)

    TempX1Low = MINVAL(PerfCurveTableData(TableIndex)%X1)
    TempX1High = MAXVAL(PerfCurveTableData(TableIndex)%X1)
    IF(V1 .LE. TempX1Low)THEN
      TempX1LowPtr = 1
      TempX1HighPtr = 1
    ELSEIF(V1 .GE. TempX1High)THEN
      TempX1LowPtr = SIZE(PerfCurveTableData(TableIndex)%X1)
      TempX1HighPtr = TempX1LowPtr
    ELSE
      DO X1VAL = 1, SIZE(PerfCurveTableData(TableIndex)%X1)
        IF(V1 .GE. PerfCurveTableData(TableIndex)%X1(X1VAL))TempX1LowPtr = X1VAL
      END DO
      IF(V1 .EQ. PerfCurveTableData(TableIndex)%X1(TempX1LowPtr))THEN
        TempX1HighPtr = TempX1LowPtr
      ELSE
        TempX1HighPtr = TempX1LowPtr + 1
      END IF
    END IF
    IF(TempX1LowPtr .EQ. TempX1HighPtr)THEN
      TableValue = PerfCurveTableData(TableIndex)%Y(TempX1LowPtr,1)
    ELSE
      X1Frac = (V1-PerfCurveTableData(TableIndex)%X1(TempX1LowPtr)) / &
               (PerfCurveTableData(TableIndex)%X1(TempX1HighPtr)- &
                PerfCurveTableData(TableIndex)%X1(TempX1LowPtr))
      TableValue = X1Frac * PerfCurveTableData(TableIndex)%Y(TempX1HighPtr,1) + &
                  (1-X1Frac) * PerfCurveTableData(TableIndex)%Y(TempX1LowPtr,1)
    END IF

  CASE(2)

    TempX1Low = MINVAL(PerfCurveTableData(TableIndex)%X1)
    TempX1High = MAXVAL(PerfCurveTableData(TableIndex)%X1)
    IF(V1 .LE. TempX1Low)THEN
      TempX1LowPtr = 1
      TempX1HighPtr = 1
    ELSEIF(V1 .GE. TempX1High)THEN
      TempX1LowPtr = SIZE(PerfCurveTableData(TableIndex)%X1)
      TempX1HighPtr = TempX1LowPtr
    ELSE
      DO X1VAL = 1, SIZE(PerfCurveTableData(TableIndex)%X1)
        IF(V1 .GE. PerfCurveTableData(TableIndex)%X1(X1VAL))TempX1LowPtr = X1VAL
      END DO
      IF(V1 .EQ. PerfCurveTableData(TableIndex)%X1(TempX1LowPtr))THEN
        TempX1HighPtr = TempX1LowPtr
      ELSE
        TempX1HighPtr = TempX1LowPtr + 1
      END IF
    END IF
    TempX2Low = MINVAL(PerfCurveTableData(TableIndex)%X2)
    TempX2High = MAXVAL(PerfCurveTableData(TableIndex)%X2)

    IF(V2 .LE. TempX2Low)THEN
      TempX2LowPtr = 1
      TempX2HighPtr = 1
    ELSEIF(V2 .GE. TempX2High)THEN
      TempX2LowPtr = SIZE(PerfCurveTableData(TableIndex)%X2)
      TempX2HighPtr = TempX2LowPtr
    ELSE
      DO X2VAL = 1, SIZE(PerfCurveTableData(TableIndex)%X2)
        IF(V2 .GE. PerfCurveTableData(TableIndex)%X2(X2VAL))TempX2LowPtr = X2VAL
      END DO
      IF(V2 .EQ. PerfCurveTableData(TableIndex)%X2(TempX2LowPtr))THEN
        TempX2HighPtr = TempX2LowPtr
      ELSE
        TempX2HighPtr = TempX2LowPtr + 1
      END IF
    END IF

    IF(TempX1LowPtr .EQ. TempX1HighPtr)THEN
      IF(TempX2LowPtr .EQ. TempX2HighPtr)THEN
        TableValue = PerfCurveTableData(TableIndex)%Y(TempX1LowPtr,TempX2LowPtr)
      ELSE
        X2Frac = (V2-PerfCurveTableData(TableIndex)%X2(TempX2LowPtr)) / &
                 (PerfCurveTableData(TableIndex)%X2(TempX2HighPtr)- &
                  PerfCurveTableData(TableIndex)%X2(TempX2LowPtr))
        TableValue = X2Frac * PerfCurveTableData(TableIndex)%Y(TempX1LowPtr,TempX2HighPtr) + &
                    (1-X2Frac) * PerfCurveTableData(TableIndex)%Y(TempX1LowPtr,TempX2LowPtr)
      END IF
    ELSE
      X1Frac = (V1-PerfCurveTableData(TableIndex)%X1(TempX1LowPtr)) / &
               (PerfCurveTableData(TableIndex)%X1(TempX1HighPtr)- &
                PerfCurveTableData(TableIndex)%X1(TempX1LowPtr))
      IF(TempX2LowPtr .EQ. TempX2HighPtr)THEN
        TableValue = X1Frac * PerfCurveTableData(TableIndex)%Y(TempX1HighPtr,TempX2LowPtr) + &
                    (1-X1Frac) * PerfCurveTableData(TableIndex)%Y(TempX1LowPtr,TempX2LowPtr)
      ELSE
        X1ValLow = X1Frac * PerfCurveTableData(TableIndex)%Y(TempX1HighPtr,TempX2LowPtr) + &
                  (1-X1Frac) * PerfCurveTableData(TableIndex)%Y(TempX1LowPtr,TempX2LowPtr)
        X1ValHigh = X1Frac * PerfCurveTableData(TableIndex)%Y(TempX1HighPtr,TempX2HighPtr) + &
                   (1-X1Frac) * PerfCurveTableData(TableIndex)%Y(TempX1LowPtr,TempX2HighPtr)
        X2Frac = (V2-PerfCurveTableData(TableIndex)%X2(TempX2LowPtr)) / &
                 (PerfCurveTableData(TableIndex)%X2(TempX2HighPtr)- &
                  PerfCurveTableData(TableIndex)%X2(TempX2LowPtr))
        TableValue = X2Frac * X1ValHigh + (1-X2Frac) * X1ValLow
      END IF
    END IF

  CASE DEFAULT
    TableValue = 0.0D0
    CALL ShowSevereError('Errors found in table output calculation for '//TRIM(PerfCurve(CurveIndex)%Name))
    CALL ShowContinueError('...Possible causes are selection of Interpolation Method or Type or Number' // &
                           ' of Independent Variables or Points.')
    CALL ShowFatalError('PerformanceTableObject: Previous error causes program termination.')
END SELECT

IF(PerfCurve(CurveIndex)%CurveMinPresent) TableValue = MAX(TableValue , PerfCurve(CurveIndex)%CurveMin)
IF(PerfCurve(CurveIndex)%CurveMaxPresent) TableValue = MIN(TableValue , PerfCurve(CurveIndex)%CurveMax)

RETURN

END FUNCTION PerformanceTableObject

REAL(r64) FUNCTION TableLookupObject(CurveIndex,Var1,Var2,Var3,Var4,Var5) RESULT(TableValue)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Richard Raustad, FSEC
          !       DATE WRITTEN   May 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! Given the curve index and the values of 1 or 2 independent variables,
          ! returns the value of an equipment performance table lookup.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataInterfaces, ONLY:ShowFatalError, ShowSevereError, ShowContinueError

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER,   INTENT (IN)           :: CurveIndex  ! index of curve in curve array
  REAL(r64), INTENT (IN)           :: Var1        ! 1st independent variable
  REAL(r64), INTENT (IN), OPTIONAL :: Var2        ! 2nd independent variable
  REAL(r64), INTENT (IN), OPTIONAL :: Var3        ! 3rd independent variable
  REAL(r64), INTENT (IN), OPTIONAL :: Var4        ! 4th independent variable
  REAL(r64), INTENT (IN), OPTIONAL :: Var5        ! 5th independent variable

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:

REAL(r64) :: V1 ! 1st independent variable after limits imposed
REAL(r64) :: V2 ! 2nd independent variable after limits imposed
REAL(r64) :: V3 ! 3rd independent variable after limits imposed
REAL(r64) :: V4 ! 4th independent variable after limits imposed
REAL(r64) :: V5 ! 5th independent variable after limits imposed

INTEGER :: NX, NY, NV3, NV4, NV5
INTEGER :: TableIndex
!REAL(r64), ALLOCATABLE, DIMENSION(:)     :: ONEDVALS
REAL(r64), ALLOCATABLE, DIMENSION(:,:)   :: TWODVALS
REAL(r64), ALLOCATABLE, DIMENSION(:,:,:) :: THREEDVALS
REAL(r64), ALLOCATABLE, DIMENSION(:) :: VALSX, VALSY, VALSV3, VALSV4, VALSV5
!REAL(r64), ALLOCATABLE, DIMENSION(:,:,:) :: HPVAL
!REAL(r64), ALLOCATABLE, DIMENSION(:,:,:,:) :: HPVALS
!REAL(r64), ALLOCATABLE, DIMENSION(:,:,:,:,:) :: DVLTRN
!REAL(r64), ALLOCATABLE, DIMENSION(:,:,:,:,:) :: FiveDArray
!REAL(r64), ALLOCATABLE, DIMENSION(:,:,:,:) :: FourDArray
!REAL(r64), ALLOCATABLE, DIMENSION(:,:,:) :: ThreeDArray
!REAL(r64), ALLOCATABLE, DIMENSION(:,:) :: TwoDArray
!REAL(r64), ALLOCATABLE, DIMENSION(:) :: OneDArray
INTEGER :: IV3, IV4, IV5, IEXTX, IEXTY, IEXTV3, IEXTV4, IEXTV5, NUMPT

TableIndex = PerfCurve(CurveIndex)%TableIndex

V1 = MAX(MIN(Var1,PerfCurve(CurveIndex)%Var1Max),PerfCurve(CurveIndex)%Var1Min)

IF (PRESENT(Var2)) THEN
  V2 = MAX(MIN(Var2,PerfCurve(CurveIndex)%Var2Max),PerfCurve(CurveIndex)%Var2Min)
  IF(TableLookup(TableIndex)%NumIndependentVars .LT. 2)THEN
    IF(PerfCurve(CurveIndex)%NumIVHighErrorIndex == 0)THEN
      CALL ShowSevereError('TableLookupObject: '//TRIM(cCurveTypes(PerfCurve(CurveIndex)%ObjectType))// &
                         '" '//TRIM(PerfCurve(CurveIndex)%Name)//'"')
      CALL ShowContinueError('...Excess number of independent variables (2) passed to subroutine '// &
                           'when only 1 is required.')
    END IF
    CALL ShowRecurringWarningErrorAtEnd(TRIM(cCurveTypes(PerfCurve(CurveIndex)%ObjectType))//  &
          ' "'//TRIM(PerfCurve(CurveIndex)%Name)//'":'//&
          ' Excess number of independent variables warning continues...' &
          , PerfCurve(CurveIndex)%NumIVHighErrorIndex, 2.0D0, 2.0D0)
  END IF
ELSE
  IF(TableLookup(TableIndex)%NumIndependentVars .GT. 1)THEN
    IF(PerfCurve(CurveIndex)%NumIVLowErrorIndex == 0)THEN
      CALL ShowSevereError('TableLookupObject: '//TRIM(cCurveTypes(PerfCurve(CurveIndex)%ObjectType))// &
                         '" '//TRIM(PerfCurve(CurveIndex)%Name)//'"')
      CALL ShowContinueError('...Insufficient number of independent variables (1) passed to subroutine '// &
                           'when at least 2 are required.')
    END IF
    CALL ShowRecurringWarningErrorAtEnd(TRIM(cCurveTypes(PerfCurve(CurveIndex)%ObjectType))//  &
          ' "'//TRIM(PerfCurve(CurveIndex)%Name)//'":'//&
          ' Insufficient number of independent variables warning continues...' &
          , PerfCurve(CurveIndex)%NumIVLowErrorIndex, 1.0D0, 1.0D0)
  END IF
  V2 = 0.0d0
END IF

IF (PRESENT(Var3)) THEN
  V3 = MAX(MIN(Var3,PerfCurve(CurveIndex)%Var3Max),PerfCurve(CurveIndex)%Var3Min)
  IF(TableLookup(TableIndex)%NumIndependentVars .LT. 3)THEN
    IF(PerfCurve(CurveIndex)%NumIVHighErrorIndex == 0)THEN
      CALL ShowSevereError('TableLookupObject: '//TRIM(cCurveTypes(PerfCurve(CurveIndex)%ObjectType))// &
                         '" '//TRIM(PerfCurve(CurveIndex)%Name)//'"')
      CALL ShowContinueError('...Excess number of independent variables (3) passed to subroutine '// &
                           'when 2 or less are required.')
    END IF
    CALL ShowRecurringWarningErrorAtEnd(TRIM(cCurveTypes(PerfCurve(CurveIndex)%ObjectType))//  &
          ' "'//TRIM(PerfCurve(CurveIndex)%Name)//'":'//&
          ' Excess number of independent variables warning continues...' &
          , PerfCurve(CurveIndex)%NumIVHighErrorIndex, 3.0D0, 3.0D0)
  END IF
ELSE
  IF(TableLookup(TableIndex)%NumIndependentVars .GT. 2)THEN
    IF(PerfCurve(CurveIndex)%NumIVLowErrorIndex == 0)THEN
      CALL ShowSevereError('TableLookupObject: '//TRIM(cCurveTypes(PerfCurve(CurveIndex)%ObjectType))// &
                         '" '//TRIM(PerfCurve(CurveIndex)%Name)//'"')
      CALL ShowContinueError('...Insufficient number of independent variables (2) passed to subroutine '// &
                           'when at least 3 are required.')
    END IF
    CALL ShowRecurringWarningErrorAtEnd(TRIM(cCurveTypes(PerfCurve(CurveIndex)%ObjectType))//  &
          ' "'//TRIM(PerfCurve(CurveIndex)%Name)//'":'//&
          ' Insufficient number of independent variables warning continues...' &
          , PerfCurve(CurveIndex)%NumIVLowErrorIndex, 2.0D0, 2.0D0)
  END IF
  V3 = 0.0d0
END IF

IF (PRESENT(Var4)) THEN
  V4 = MAX(MIN(Var4,PerfCurve(CurveIndex)%Var4Max),PerfCurve(CurveIndex)%Var4Min)
  IF(TableLookup(TableIndex)%NumIndependentVars .LT. 4)THEN
    IF(PerfCurve(CurveIndex)%NumIVHighErrorIndex == 0)THEN
      CALL ShowSevereError('TableLookupObject: '//TRIM(cCurveTypes(PerfCurve(CurveIndex)%ObjectType))// &
                         '" '//TRIM(PerfCurve(CurveIndex)%Name)//'"')
      CALL ShowContinueError('...Excess number of independent variables (4) passed to subroutine '// &
                           'when 3 or less are required.')
    END IF
    CALL ShowRecurringWarningErrorAtEnd(TRIM(cCurveTypes(PerfCurve(CurveIndex)%ObjectType))//  &
          ' "'//TRIM(PerfCurve(CurveIndex)%Name)//'":'//&
          ' Excess number of independent variables warning continues...' &
          , PerfCurve(CurveIndex)%NumIVHighErrorIndex, 4.0D0, 4.0D0)
  END IF
ELSE
  IF(TableLookup(TableIndex)%NumIndependentVars .GT. 3)THEN
    IF(PerfCurve(CurveIndex)%NumIVLowErrorIndex == 0)THEN
      CALL ShowSevereError('TableLookupObject: '//TRIM(cCurveTypes(PerfCurve(CurveIndex)%ObjectType))// &
                         '" '//TRIM(PerfCurve(CurveIndex)%Name)//'"')
      CALL ShowContinueError('...Insufficient number of independent variables (3) passed to subroutine '// &
                           'when at least 4 are required.')
    END IF
    CALL ShowRecurringWarningErrorAtEnd(TRIM(cCurveTypes(PerfCurve(CurveIndex)%ObjectType))//  &
          ' "'//TRIM(PerfCurve(CurveIndex)%Name)//'":'//&
          ' Insufficient number of independent variables warning continues...' &
          , PerfCurve(CurveIndex)%NumIVLowErrorIndex, 3.0D0, 3.0D0)
  END IF
  V4 = 0.0d0
END IF

IF (PRESENT(Var5)) THEN
  V5 = MAX(MIN(Var5,PerfCurve(CurveIndex)%Var5Max),PerfCurve(CurveIndex)%Var5Min)
  IF(TableLookup(TableIndex)%NumIndependentVars .LT. 5)THEN
    IF(PerfCurve(CurveIndex)%NumIVHighErrorIndex == 0)THEN
      CALL ShowSevereError('TableLookupObject: '//TRIM(cCurveTypes(PerfCurve(CurveIndex)%ObjectType))// &
                         '" '//TRIM(PerfCurve(CurveIndex)%Name)//'"')
      CALL ShowContinueError('...Excess number of independent variables (5) passed to subroutine '// &
                           'when 4 or less are required.')
    END IF
    CALL ShowRecurringWarningErrorAtEnd(TRIM(cCurveTypes(PerfCurve(CurveIndex)%ObjectType))//  &
          ' "'//TRIM(PerfCurve(CurveIndex)%Name)//'":'//&
          ' Excess number of independent variables warning continues...' &
          , PerfCurve(CurveIndex)%NumIVHighErrorIndex, 5.0D0, 5.0D0)
  END IF
ELSE
  IF(TableLookup(TableIndex)%NumIndependentVars .GT. 4)THEN
    IF(PerfCurve(CurveIndex)%NumIVLowErrorIndex == 0)THEN
      CALL ShowSevereError('TableLookupObject: '//TRIM(cCurveTypes(PerfCurve(CurveIndex)%ObjectType))// &
                         '" '//TRIM(PerfCurve(CurveIndex)%Name)//'"')
      CALL ShowContinueError('...Insufficient number of independent variables (4) passed to subroutine '// &
                           'when at least 5 are required.')
    END IF
    CALL ShowRecurringWarningErrorAtEnd(TRIM(cCurveTypes(PerfCurve(CurveIndex)%ObjectType))//  &
          ' "'//TRIM(PerfCurve(CurveIndex)%Name)//'":'//&
          ' Insufficient number of independent variables warning continues...' &
          , PerfCurve(CurveIndex)%NumIVLowErrorIndex, 4.0D0, 4.0D0)
  END IF
  V5 = 0.0d0
END IF

      SELECT CASE(TableLookup(TableIndex)%NumIndependentVars)
        CASE(1)
          NX=TableLookup(TableIndex)%NumX1Vars
          NY=1
          NUMPT=TableLookup(TableIndex)%InterpolationOrder
          ALLOCATE(VALSX(NX))
          VALSX = TableLookup(TableIndex)%X1Var
          TableValue = DLAG(V1,VALSX(1),VALSX,VALSX, &
              TableLookup(TableIndex)%TableLookupZData(:,:,1,1,1),NX,NY,NUMPT,IEXTX,IEXTY)
          DEALLOCATE(VALSX)
        CASE(2)
          NX=TableLookup(TableIndex)%NumX1Vars
          NY=TableLookup(TableIndex)%NumX2Vars
          NUMPT=TableLookup(TableIndex)%InterpolationOrder
          ALLOCATE(VALSX(NX))
          VALSX = TableLookup(TableIndex)%X1Var
          ALLOCATE(VALSY(NY))
          VALSY = TableLookup(TableIndex)%X2Var
          TableValue = DLAG(V1,V2,VALSX,VALSY, &
              TableLookup(TableIndex)%TableLookupZData(:,:,1,1,1),NX,NY,NUMPT,IEXTX,IEXTY)
          DEALLOCATE(VALSX)
          DEALLOCATE(VALSY)
        CASE(3)
          NX=TableLookup(TableIndex)%NumX1Vars
          NY=TableLookup(TableIndex)%NumX2Vars
          NV3=TableLookup(TableIndex)%NumX3Vars
          NUMPT=TableLookup(TableIndex)%InterpolationOrder
          ALLOCATE(VALSX(NX))
          VALSX = TableLookup(TableIndex)%X1Var
          ALLOCATE(VALSY(NY))
          VALSY = TableLookup(TableIndex)%X2Var
          ALLOCATE(VALSV3(NV3))
          VALSV3 = TableLookup(TableIndex)%X3Var
          ALLOCATE(TWODVALS(NV3,1))
          ! perform 2-D interpolation of X (V1) and Y (V2) and save in 2-D array
          DO IV3=1,NV3
            TWODVALS(IV3,1) = DLAG(V1,V2,VALSX,VALSY, &
              TableLookup(TableIndex)%TableLookupZData(:,:,IV3,1,1),NX,NY,NUMPT,IEXTX,IEXTY)
          END DO
          IF(NV3 .EQ. 1)THEN
            TableValue = TWODVALS(1,1)
          ELSE
            TableValue = DLAG(V3,1.0D0,VALSV3,VALSV3,TWODVALS,NV3,1,NUMPT,IEXTV3,IEXTV4)
          END IF
          DEALLOCATE(TWODVALS)
          DEALLOCATE(VALSX)
          DEALLOCATE(VALSY)
          DEALLOCATE(VALSV3)
        CASE(4)
          NX=TableLookup(TableIndex)%NumX1Vars
          NY=TableLookup(TableIndex)%NumX2Vars
          NV3=TableLookup(TableIndex)%NumX3Vars
          NV4=TableLookup(TableIndex)%NumX4Vars
          NUMPT=TableLookup(TableIndex)%InterpolationOrder
          ALLOCATE(VALSX(NX))
          VALSX = TableLookup(TableIndex)%X1Var
          ALLOCATE(VALSY(NY))
          VALSY = TableLookup(TableIndex)%X2Var
          ALLOCATE(VALSV3(NV3))
          VALSV3 = TableLookup(TableIndex)%X3Var
          ALLOCATE(VALSV4(NV4))
          VALSV4 = TableLookup(TableIndex)%X4Var
          ALLOCATE(TWODVALS(NV3,NV4))
          ! perform 2-D interpolation of X (V1) and Y (V2) and save in 2-D array
          DO IV4=1,NV4
            DO IV3=1,NV3
              TWODVALS(IV3,IV4) = DLAG(V1,V2,VALSX,VALSY, &
                TableLookup(TableIndex)%TableLookupZData(:,:,IV3,IV4,1),NX,NY,NUMPT,IEXTX,IEXTY)
            END DO
          END DO
          ! final interpolation of 2-D array in V3 and V4
          TableValue = DLAG(V3,V4,VALSV3,VALSV4,TWODVALS,NV3,NV4,NUMPT,IEXTV3,IEXTV4)
          DEALLOCATE(TWODVALS)
          DEALLOCATE(VALSX)
          DEALLOCATE(VALSY)
          DEALLOCATE(VALSV3)
          DEALLOCATE(VALSV4)
        CASE(5)
          NX=TableLookup(TableIndex)%NumX1Vars
          NY=TableLookup(TableIndex)%NumX2Vars
          NV3=TableLookup(TableIndex)%NumX3Vars
          NV4=TableLookup(TableIndex)%NumX4Vars
          NV5=TableLookup(TableIndex)%NumX5Vars
          NUMPT=TableLookup(TableIndex)%InterpolationOrder
          ALLOCATE(VALSX(NX))
          VALSX = TableLookup(TableIndex)%X1Var
          ALLOCATE(VALSY(NY))
          VALSY = TableLookup(TableIndex)%X2Var
          ALLOCATE(VALSV3(NV3))
          VALSV3 = TableLookup(TableIndex)%X3Var
          ALLOCATE(VALSV4(NV4))
          VALSV4 = TableLookup(TableIndex)%X4Var
          ALLOCATE(VALSV5(NV5))
          VALSV5 = TableLookup(TableIndex)%X5Var
          ALLOCATE(THREEDVALS(NV3,NV4,NV5))
          DO IV5=1,NV5
            DO IV4=1,NV4
              DO IV3=1,NV3
                THREEDVALS(IV3,IV4,IV5) = DLAG(V1,V2,VALSX,VALSY, &
                  TableLookup(TableIndex)%TableLookupZData(:,:,IV3,IV4,IV5),NX,NY,NUMPT,IEXTX,IEXTY)
              END DO
            END DO
          END DO
          ALLOCATE(TWODVALS(NV5,1))
          DO IV5=1,NV5
            TWODVALS(IV5,1) = DLAG(V3,V4,VALSV3,VALSV4,THREEDVALS(:,:,IV5),NV3,NV4,NUMPT,IEXTX,IEXTY)
          END DO
          IF(NV5 .EQ. 1)THEN
            TableValue = TWODVALS(1,1)
          ELSE
            TableValue = DLAG(V5,1.0D0,VALSV5,VALSV5,TWODVALS,NV5,1,NUMPT,IEXTV5,IEXTV4)
          END IF
          DEALLOCATE(TWODVALS)
          DEALLOCATE(THREEDVALS)
          DEALLOCATE(VALSX)
          DEALLOCATE(VALSY)
          DEALLOCATE(VALSV3)
          DEALLOCATE(VALSV4)
          DEALLOCATE(VALSV5)
        CASE DEFAULT
          TableValue = 0.0D0
          CALL ShowSevereError('Errors found in table output calculation for '//TRIM(PerfCurve(CurveIndex)%Name))
          CALL ShowContinueError('...Possible causes are selection of Interpolation Method or Type or Number' // &
                                 ' of Independent Variables or Points.')
          CALL ShowFatalError('PerformanceTableObject: Previous error causes program termination.')
      END SELECT

IF(PerfCurve(CurveIndex)%CurveMinPresent) TableValue = MAX(TableValue , PerfCurve(CurveIndex)%CurveMin)
IF(PerfCurve(CurveIndex)%CurveMaxPresent) TableValue = MIN(TableValue , PerfCurve(CurveIndex)%CurveMax)

RETURN

END FUNCTION TableLookupObject

SUBROUTINE SolveRegression(CurveNum, TableType, CurveName, RawDataX, RawDataY, RawDataX2)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Raustad, FSEC
          !       DATE WRITTEN   June 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! Given the curve index and the values of 1 or 2 independent variables,
          ! calls the curve or table routine to return the value of an equipment performance curve or table.
          ! The solution requires use of linear algebra and forms a matrix of sums of the data set.
          ! For a linear equation of the form Z = a + bX + cX^2, the general solution is as follows.
          !
          ! Phi = SUM1ToN[Zi - f(Xi)]^2 = SUM1ToN[Zi - (a+bX+cX^2)]^2 = minimum
          !
          ! substitue Y = X^2 in the equations above.
          ! then set up the partials of Phi with respect to a, the partial of Phi with respect to b, etc.
          !
          ! PartialPhiRespectToa = 2 * SUM1ToN[1*(Zi-(a+bXi+cYi))] = 0
          ! PartialPhiRespectTob = 2 * SUM1ToN[Xi(Zi-(a+bXi+cYi))] = 0
          ! PartialPhiRespectTob = 2 * SUM1ToN[Yi(Zi-(a+bXi+cYi))] = 0
          !
          ! then set up the square matrix by solving the above partials.
          !
          ! SUM1ToN(Zi)   = a * SUM1ToN(1)  + b * SUM1ToN(Xi)   + c * SUM1ToN(Yi)
          ! SUM1ToN(ZiXi) = a * SUM1ToN(Xi) + b * SUM1ToN(Xi)^2 + c * SUM1ToN(XiYi)
          ! SUM1ToN(ZiYi) = a * SUM1ToN(Yi) + b * SUM1ToN(XiYi) + c * SUM1ToN(Yi)^2
          !
          ! the matirx (A) is then the 3x3 matrix on the right, with a solution of the 1x3 matrix on the left
          ! Note symmetry about the diagonal.
          ! (i.e., A(1,2)=A(2,1), A(1,3)=A(3,1), A(3,2)=A(2,3), and diagonal are all squared terms)
          !      _                                          _              _              _
          !     |  SUM1ToN(1)   SUM1ToN(Xi)   SUM1ToN(Yi)    |            |  SUM1ToN(Zi)   |
          ! A = |  SUM1ToN(Xi)  SUM1ToN(Xi)^2 SUM1ToN(XiYi)  |  Results = |  SUM1ToN(ZiXi) |
          !     |_ SUM1ToN(Yi)  SUM1ToN(XiYi) SUM1ToN(Yi)^2 _|            |_ SUM1ToN(ZiYi)_|
          !
          ! The linear algebra equation is then solved using foward elimination and reverse substitution
          ! This solution (Results) provides the coefficients of the associated performance curve (a,b,and c in the eq. above).
          !
          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE General,        ONLY: RoundSigDigits, TrimSigDigits
  USE DataGlobals,    ONLY: DisplayAdvancedReportVariables, OutputFileInits
  USE DataInterfaces, ONLY: ShowContinueError, ShowSevereError

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
 INTEGER                      :: CurveNum        ! index to performance curve
 Character(len=MaxNameLength) :: TableType       ! tabular data object type
 Character(len=MaxNameLength) :: CurveName       ! performance curve name
 Real(R64), DIMENSION(:)      :: RawDataX        ! table data X values (1st independent variable)
 Real(R64), DIMENSION(:)      :: RawDataY        ! table data Y values (dependent variables)
 Real(R64), OPTIONAL, DIMENSION(:) :: RawDataX2  ! table data X2 values (2nd independent variable)

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
Real(r64) :: X, X2, Y, V, U, T, Z  ! linear algebra equation coefficients
INTEGER   :: MatrixSize            ! square matrix array size (MatrixSize,MatrixSize)
INTEGER   :: LoopCount             ! loop counter
INTEGER   :: N, i, j, k            ! loop variables
Real(r64) :: C                     ! intermediate calculation of a constant in matrix solution
Real(r64) :: sX                    ! sum of the X
Real(r64) :: sX2                   ! sum of the X^2
Real(r64) :: sX3                   ! sum of the X^3
Real(r64) :: sY                    ! sum of the Y
Real(r64) :: sY2                   ! sum of the Y^2
Real(r64) :: sV                    ! sum of the V
Real(r64) :: sV2                   ! sum of the V^2
Real(r64) :: sU                    ! sum of the U
Real(r64) :: sU2                   ! sum of the U^2
Real(r64) :: sT                    ! sum of the T
Real(r64) :: sT2                   ! sum of the T^2
Real(r64) :: sXY                   ! sum of the XY
Real(r64) :: sXV                   ! sum of the XV
Real(r64) :: sXU                   ! sum of the XU
Real(r64) :: sXT                   ! sum of the XT
Real(r64) :: sYV                   ! sum of the TV
Real(r64) :: sYU                   ! sum of the YU
Real(r64) :: sYT                   ! sum of the YT
Real(r64) :: sVU                   ! sum of the VU
Real(r64) :: sVT                   ! sum of the VT
Real(r64) :: sUT                   ! sum of the UT
Real(r64) :: Results1              ! regression coefficient #1
Real(r64) :: Results2              ! regression coefficient #2
Real(r64) :: Results3              ! regression coefficient #3
Real(r64) :: Results4              ! regression coefficient #4
Real(r64) :: Results5              ! regression coefficient #5
Real(r64) :: Results6              ! regression coefficient #6
Real(r64) :: MinX, MaxX, MinX2, MaxX2, MinY, MaxY    ! equation variable min/max statistics
Real(r64) :: Mean, RSquared, StandardError, Est      ! statistical parameters
Real(r64),ALLOCATABLE,DIMENSION(:) :: Results        ! performance curve coefficients
Real(r64),ALLOCATABLE,DIMENSION(:,:) :: A            ! linear algebra matrix
Character(len=MaxNameLength) :: StrCurve             ! string representation of curve type
LOGICAL, SAVE :: WriteHeaderOnce = .TRUE.
LOGICAL :: EchoTableDataToEio            ! logical set equal to global and used to report to eio file

EchoTableDataToEio = DisplayAdvancedReportVariables


SELECT CASE(PerfCurve(CurveNum)%CurveType)
  CASE(LINEAR)
    MatrixSize = 2
    StrCurve   = 'Linear'
  CASE(QUADRATIC)
    MatrixSize = 3
    StrCurve   = 'Quadratic'
  CASE(CUBIC)
    MatrixSize = 4
    StrCurve   = 'Cubic'
  CASE(QUARTIC)
    MatrixSize = 5
    StrCurve   = 'Quartic'
  CASE(BIQUADRATIC)
    MatrixSize = 6
    StrCurve   = 'BiQuadratic'
  CASE(QUADRATICLINEAR)
    MatrixSize = 6
    StrCurve   = 'QuadraticLinear'
  CASE DEFAULT
    RETURN
END SELECT

IF(SIZE(RawDataX) .LT. (MatrixSize))THEN
  SELECT CASE(PerfCurve(CurveNum)%ObjectType)
    CASE(CurveType_TableOneIV)
      CALL ShowSevereError('TABLE:ONEINDEPENDENTVARIABLE: "'//TRIM(PerfCurve(CurveNum)%Name)//'"')
    CASE(CurveType_TableTwoIV)
      CALL ShowSevereError('TABLE:TWOINDEPENDENTVARIABLES: "'//TRIM(PerfCurve(CurveNum)%Name)//'"')
    CASE(CurveType_TableMultiIV)
      CALL ShowSevereError('TABLE:MULTIVARIABLELOOKUP: "'//TRIM(PerfCurve(CurveNum)%Name)//'"')
    CASE DEFAULT
      CALL ShowSevereError('SOLVEREGRESSION: Incorrect object type with name = '//TRIM(PerfCurve(CurveNum)%Name)//'"')
  END SELECT
  CALL ShowContinueError('Insufficient data to calculate regression coefficients.')
  CALL ShowContinueError('Required data pairs = '//trim(RoundSigDigits(MatrixSize)))
  CALL ShowContinueError('Entered data pairs  = '//trim(RoundSigDigits(SIZE(RawDataX))))
  CALL ShowContinueError('Setting interpolation type equal to LinearInterpolationOfTable and simulation continues.')
  PerfCurve(CurveNum)%InterpolationType = LinearInterpolationOfTable
  RETURN
END IF

ALLOCATE(Results(MatrixSize))
Results = 0.0D0
ALLOCATE(A(MatrixSize,MatrixSize))
!   ' Sum data
   N = 0
   sX = 0.0d0
   SX2 = 0.0d0
   SY = 0.0d0
   SY2 = 0.0d0
   SV = 0.0d0
   SV2 = 0.0d0
   SU = 0.0d0
   SU2 = 0.0d0
   ST = 0.0d0
   ST2 = 0.0d0
   SXY = 0.0d0
   SXV = 0.0d0
   SXU = 0.0d0
   SXT = 0.0d0
   SYV = 0.0d0
   SYU = 0.0d0
   SYT = 0.0d0
   SVU = 0.0d0
   SVT = 0.0d0
   SUT = 0.0d0
   Results = 0.0d0
   Results1 = 0.0d0
   Results2 = 0.0d0
   Results3 = 0.0d0
   Results4 = 0.0d0
   Results5 = 0.0d0
   Results6 = 0.0d0
   X2 = 0.0D0
   Y  = 0.0D0
   V  = 0.0D0
   U  = 0.0D0
   T  = 0.0D0
   DO LoopCount = 1, SIZE(RawDataX)
     X = RawDataX(LoopCount)
     IF(Present(RawDataX2))X2 = RawDataX2(LoopCount)
     SELECT CASE(PerfCurve(CurveNum)%CurveType)
       CASE(LINEAR,QUADRATIC,CUBIC,QUARTIC)
         Y=X*X
         V=X*Y
         U=X*V
       CASE(BIQUADRATIC)
         Y=X*X
         V=X2
         U=V*V
         T=X*X2
       CASE(QUADRATICLINEAR)
         Y=X*X
         V=X2
         U=X*V
         T=Y*X2
       CASE DEFAULT
     END SELECT
     Z = RawDataY(LoopCount)
     N = N + 1                   ! Count
     sX = sX + X                 ! Sum X
     SX2 = SX2 + X * X           ! Sum X*X
     SY = SY + Y                 ! Sum Y
     SY2 = SY2 + Y * Y           ! Sum Y*Y
     SV = SV + V                 ! Sum V
     SV2 = SV2 + V * V           ! Sum V*V
     SU = SU + U                 ! Sum U
     SU2 = SU2 + U * U           ! Sum U*U
     ST = ST + T                 ! Sum T
     ST2 = ST2 + T * T           ! Sum T*T
     SXY = SXY + X * Y           ! Sum XY
     SXV = SXV + X * V           ! Sum XV
     SXU = SXU + X * U           ! Sum XU
     SXT = SXT + X * T           ! Sum XT
     SYV = SYV + Y * V           ! Sum YV
     SYU = SYU + Y * U           ! Sum YU
     SYT = SYT + Y * T           ! Sum YT
     SVU = SVU + V * U           ! Sum VU
     SVT = SVT + V * T           ! Sum VT
     SUT = SUT + U * T           ! Sum UT
     Results1 = Results1 + Z     ! Sum Z
     Results2 = Results2 + Z * X ! Sum ZX
     Results3 = Results3 + Z * Y ! Sum ZY
     Results4 = Results4 + Z * V ! Sum ZV
     Results5 = Results5 + Z * U ! Sum ZU
     Results6 = Results6 + Z * T ! Sum ZT
   END DO

  Results(1) = Results1
  Results(2) = Results2
  SELECT CASE(PerfCurve(CurveNum)%CurveType)
    CASE(LINEAR)
    CASE(QUADRATIC)
      Results(3) = Results3
    CASE(CUBIC)
      Results(3) = Results3
      Results(4) = Results4
    CASE(QUARTIC)
      Results(3) = Results3
      Results(4) = Results4
      Results(5) = Results5
    CASE(BIQUADRATIC,QUADRATICLINEAR)
      Results(3) = Results3
      Results(4) = Results4
      Results(5) = Results5
      Results(6) = Results6
  END SELECT

   Mean = Results(1) / N

!    ' Form "A" Matrix
   A(1, 1) = Real(N,r64)
   A(1, 2) = sX
   A(2, 2) = SX2
SELECT CASE(PerfCurve(CurveNum)%CurveType)
  CASE(LINEAR)
  CASE(QUADRATIC)
   A(1, 3) = SY
   A(2, 3) = SXY
   A(3, 3) = SY2
  CASE(CUBIC)
   A(1, 3) = SY
   A(1, 4) = SV
   A(2, 3) = SXY
   A(2, 4) = SXV
   A(3, 3) = SY2
   A(3, 4) = SYV
   A(4, 4) = SV2
  CASE(QUARTIC)
   A(1, 3) = SY
   A(1, 4) = SV
   A(1, 5) = SU
   A(2, 3) = SXY
   A(2, 4) = SXV
   A(2, 5) = SXU
   A(3, 3) = SY2
   A(3, 4) = SYV
   A(3, 5) = SYU
   A(4, 4) = SV2
   A(4, 5) = SVU
   A(5, 5) = SU2
  CASE(BIQUADRATIC,QUADRATICLINEAR)
   A(1, 3) = SY
   A(1, 4) = SV
   A(1, 5) = SU
   A(1, 6) = ST
   A(2, 3) = SXY
   A(2, 4) = SXV
   A(2, 5) = SXU
   A(2, 6) = SXT
   A(3, 3) = SY2
   A(3, 4) = SYV
   A(3, 5) = SYU
   A(3, 6) = SYT
   A(4, 4) = SV2
   A(4, 5) = SVU
   A(4, 6) = SVT
   A(5, 5) = SU2
   A(5, 6) = SUT
   A(6, 6) = ST2
  CASE DEFAULT
END SELECT

!  copy elements to bottom half of symmetrical square matrix
DO i = 1, MatrixSize - 1
  DO j = i + 1, MatrixSize
      A(j, i) = A(i, j)
  END DO
END DO

!   Forward Eliminiation
DO i = 1, MatrixSize - 1
   If (A(i, i) .EQ. 0.0D0) Then
     CALL ShowSevereError('SolveRegression: Zero value on the diagonal.')
     CALL ShowContinueError('Setting interpolation type equal to LinearInterpolationOfTable and simulation continues.')
     PerfCurve(CurveNum)%InterpolationType = LinearInterpolationOfTable
     RETURN
   End If
   DO j = i + 1, MatrixSize
!      find the ratio of the element to the one above it
       C = A(j, i) / A(i, i)
!      replace the element by reducing it by the ratio multiplied by the element above it
!      this makes the bottom half of symmetrical square matix 0's
       DO k = i, MatrixSize
           A(j, k) = A(j, k) - C * A(i, k)
       END DO
       Results(j) = Results(j) - C * Results(i)
   END DO
END DO

!    ' Back Substitution
   If (A(MatrixSize, MatrixSize) .EQ. 0.0D0) Then
     CALL ShowSevereError('SolveRegression: Zero value on the diagonal end point.')
     CALL ShowContinueError('Setting interpolation type equal to LinearInterpolationOfTable and simulation continues.')
     PerfCurve(CurveNum)%InterpolationType = LinearInterpolationOfTable
     RETURN
   End If
!  now starting at the lower right corner of the matrix solve for the last coefficient
   Results(MatrixSize) = Results(MatrixSize) / A(MatrixSize, MatrixSize)
!  substitute that coefficient back into the equation above it and solve for the 2nd to last coefficient
!  proceed until all coefficients are found
   DO i = MatrixSize - 1, 1, -1
       C = Results(i)
       DO j = 1, MatrixSize - i
           C = C - A(i, i + j) * Results(i + j)
       END DO
       Results(i) = C / A(i, i)
   END DO

!  calculate the regression statistics
   sX = 0.0D0
   sX2 = 0.0D0
   sX3 = 0.0D0
   MinX = 9999999.0D0
   MaxX = -9999999.0D0
   MinX2 = 9999999.0D0
   MaxX2 = -9999999.0D0
   MinY = 9999999.0D0
   MaxY = -9999999.0D0
   DO LoopCount = 1, N
    X = RawDataX(LoopCount)
     IF(Present(RawDataX2))X2 = RawDataX2(LoopCount)
     SELECT CASE(PerfCurve(CurveNum)%CurveType)
       CASE(LINEAR,QUADRATIC,CUBIC,QUARTIC)
         Y=X*X
         V=X*Y
         U=X*V
       CASE(BIQUADRATIC)
         Y=X*X
         V=X2
         U=V*V
         T=X*X2
       CASE(QUADRATICLINEAR)
         Y=X*X
         V=X2
         U=X*V
         T=Y*X2
       CASE DEFAULT
     END SELECT
    Z = RawDataY(LoopCount)
    IF(MinX .GT. X)MinX = X
    IF(MaxX .LT. X)MaxX = X
    IF(MinX2 .GT. X2)MinX2 = X2
    IF(MaxX2 .LT. X2)MaxX2 = X2
    IF(MinY .GT. Z)MinY = Z
    IF(MaxY .LT. Z)MaxY = Z

    SELECT CASE(PerfCurve(CurveNum)%CurveType)
      CASE(LINEAR)
        Est = Results(1) + X*Results(2)
      CASE(QUADRATIC)
        Est = Results(1) + X*Results(2) + Y*Results(3)
      CASE(CUBIC)
        Est = Results(1) + X*Results(2) + Y*Results(3) + V*Results(4)
      CASE(QUARTIC)
        Est = Results(1) + X*Results(2) + Y*Results(3) + V*Results(4) + U*Results(5)
      CASE(BIQUADRATIC,QUADRATICLINEAR)
        Est = Results(1) + X*Results(2) + Y*Results(3) + V*Results(4) + U*Results(5) + T*Results(6)
      CASE DEFAULT
    END SELECT
     sX = sX + (Est - Mean) * (Est - Mean)
     sX2 = sX2 + (Z-Mean) * (Z-Mean)
     sX3 = sX3 + (Z-Est) * (Z-Est)
   END DO
   IF(sX2 .NE. 0.0D0)THEN
     RSquared = sX / sX2
   ELSE
     RSquared = 0.0D0
   END IF
   IF(N .GT. MatrixSize)THEN
     StandardError = SQRT(sX3/(N-MatrixSize))
   ELSE
     StandardError = 0.0D0
   END IF

SELECT CASE(PerfCurve(CurveNum)%InterpolationType)
  CASE(LinearInterpolationOfTable)
  CASE(EvaluateCurveToLimits)
    MinX = MIN(MinX,PerfCurve(CurveNum)%Var1Min)
    MaxX = MAX(MaxX,PerfCurve(CurveNum)%Var1Max)
    MinX2 = MIN(MinX2,PerfCurve(CurveNum)%Var2Min)
    MaxX2 = MAX(MaxX2,PerfCurve(CurveNum)%Var2Max)
    MinY = MIN(MinY,PerfCurve(CurveNum)%CurveMin)
    MaxY = MAX(MaxY,PerfCurve(CurveNum)%CurveMax)
  CASE DEFAULT
END SELECT

! echo new curve object to eio file
110 FORMAT('! <CREATING NEW CURVE OBJECT>')
130 FORMAT('CREATING NEW CURVE OBJECT')
   IF(EchoTableDataToEio)THEN
     IF(WriteHeaderOnce)THEN
       WRITE(OutputFileInits,110)
       WriteHeaderOnce = .FALSE.
     END IF
140 FORMAT('! Input as ',A,' "',A,'"')
150 FORMAT('! RSquared       = ',A)
160 FORMAT('! Standard Error = ',A)
170 FORMAT('! Sample Size    = ',A)
180 FORMAT('Curve:',A,',')
190 FORMAT('FromTable_',A,',  !- Name')
200 FORMAT('  ',A,',  !- Coefficient1 Constant')
210 FORMAT('  ',A,',  !- Coefficient2 x')
300 FORMAT('  ',A,',  !- Minimum Value of x')
310 FORMAT('  ',A,',  !- Maximum Value of x')
340 FORMAT('  ',A,',  !- Minimum Curve Output')
350 FORMAT('  ',A,';  !- Maximum Curve Output')
360 FORMAT('END CREATING NEW CURVE OBJECT')

     WRITE(OutputFileInits,130)
     WRITE(OutputFileInits,140)TRIM(TableType),TRIM(CurveName)
     WRITE(OutputFileInits,150)TRIM(RoundSigDigits(RSquared,10))
     WRITE(OutputFileInits,160)TRIM(RoundSigDigits(StandardError,10))
     WRITE(OutputFileInits,170)TRIM(TrimSigDigits(N))
     WRITE(OutputFileInits,180)TRIM(StrCurve)
     WRITE(OutputFileInits,190)TRIM(CurveName)
     WRITE(OutputFileInits,200)TRIM(RoundSigDigits(Results(1),10))
     SELECT CASE(PerfCurve(CurveNum)%CurveType)
       CASE(LINEAR,QUADRATIC,CUBIC,QUARTIC,BIQUADRATIC,QUADRATICLINEAR)
         WRITE(OutputFileInits,210)TRIM(RoundSigDigits(Results(2),10))
       CASE DEFAULT
     END SELECT
     SELECT CASE(PerfCurve(CurveNum)%CurveType)
       CASE(QUADRATIC,CUBIC,QUARTIC,BIQUADRATIC,QUADRATICLINEAR)
220      FORMAT('  ',A,',  !- Coefficient3 x**2')
         WRITE(OutputFileInits,220)TRIM(RoundSigDigits(Results(3),10))
       CASE DEFAULT
     END SELECT
     SELECT CASE(PerfCurve(CurveNum)%CurveType)
       CASE(CUBIC,QUARTIC)
230      FORMAT('  ',A,',  !- !- Coefficient4 x**3')
         WRITE(OutputFileInits,230)TRIM(RoundSigDigits(Results(4),10))
       CASE(BIQUADRATIC,QUADRATICLINEAR)
240      FORMAT('  ',A,',  !- Coefficient4 y')
         WRITE(OutputFileInits,240)TRIM(RoundSigDigits(Results(4),10))
       CASE DEFAULT
     END SELECT
     SELECT CASE(PerfCurve(CurveNum)%CurveType)
       CASE(QUARTIC)
250       FORMAT('  ',A,',  !- !- Coefficient5 x**4')
          WRITE(OutputFileInits,250)TRIM(RoundSigDigits(Results(5),10))
       CASE(BIQUADRATIC)
260       FORMAT('  ',A,',  !- Coefficient5 y**2')
          WRITE(OutputFileInits,260)TRIM(RoundSigDigits(Results(5),10))
       CASE(QUADRATICLINEAR)
270       FORMAT('  ',A,',  !- Coefficient5 xy')
          WRITE(OutputFileInits,270)TRIM(RoundSigDigits(Results(5),10))
       CASE DEFAULT
     END SELECT
     SELECT CASE(PerfCurve(CurveNum)%CurveType)
       CASE(BIQUADRATIC)
280       FORMAT('  ',A,',  !- Coefficient6 x*y')
          WRITE(OutputFileInits,280)TRIM(RoundSigDigits(Results(6),10))
       CASE(QUADRATICLINEAR)
290       FORMAT('  ',A,',  !- Coefficient6 x**2y')
          WRITE(OutputFileInits,290)TRIM(RoundSigDigits(Results(6),10))
       CASE DEFAULT
     END SELECT
     WRITE(OutputFileInits,300)TRIM(RoundSigDigits(MinX,10))
     WRITE(OutputFileInits,310)TRIM(RoundSigDigits(MaxX,10))
     SELECT CASE(PerfCurve(CurveNum)%CurveType)
       CASE(QUARTIC)
       CASE(BIQUADRATIC,QUADRATICLINEAR)
320      FORMAT('  ',A,',  !- Minimum Value of y')
330      FORMAT('  ',A,',  !- Maximum Value of y')
         WRITE(OutputFileInits,320)TRIM(RoundSigDigits(MinX2,10))
         WRITE(OutputFileInits,330)TRIM(RoundSigDigits(MaxX2,10))
       CASE DEFAULT
     END SELECT
     WRITE(OutputFileInits,340)TRIM(RoundSigDigits(MinY,10))
     WRITE(OutputFileInits,350)TRIM(RoundSigDigits(MaxY,10))
     WRITE(OutputFileInits,360)
   END IF

! save results in performance curve structure
SELECT CASE(PerfCurve(CurveNum)%CurveType)
  CASE(LINEAR)
    PerfCurve(CurveNum)%Coeff1    = Results(1)
    PerfCurve(CurveNum)%Coeff2    = Results(2)
  CASE(QUADRATIC)
    PerfCurve(CurveNum)%Coeff1    = Results(1)
    PerfCurve(CurveNum)%Coeff2    = Results(2)
    PerfCurve(CurveNum)%Coeff3    = Results(3)
  CASE(CUBIC)
    PerfCurve(CurveNum)%Coeff1    = Results(1)
    PerfCurve(CurveNum)%Coeff2    = Results(2)
    PerfCurve(CurveNum)%Coeff3    = Results(3)
    PerfCurve(CurveNum)%Coeff4    = Results(4)
  CASE(QUARTIC)
    PerfCurve(CurveNum)%Coeff1    = Results(1)
    PerfCurve(CurveNum)%Coeff2    = Results(2)
    PerfCurve(CurveNum)%Coeff3    = Results(3)
    PerfCurve(CurveNum)%Coeff4    = Results(4)
    PerfCurve(CurveNum)%Coeff5    = Results(5)
  CASE(BIQUADRATIC,QUADRATICLINEAR)
    PerfCurve(CurveNum)%Coeff1    = Results(1)
    PerfCurve(CurveNum)%Coeff2    = Results(2)
    PerfCurve(CurveNum)%Coeff3    = Results(3)
    PerfCurve(CurveNum)%Coeff4    = Results(4)
    PerfCurve(CurveNum)%Coeff5    = Results(5)
    PerfCurve(CurveNum)%Coeff6    = Results(6)
  CASE DEFAULT
END SELECT

PerfCurve(CurveNum)%Var1Min = MinX
PerfCurve(CurveNum)%Var1Max = MaxX
PerfCurve(CurveNum)%Var2Min = MinX2
PerfCurve(CurveNum)%Var2Max = MaxX2
PerfCurve(CurveNum)%CurveMin = MinY
PerfCurve(CurveNum)%CurveMax = MaxY
PerfCurve(CurveNum)%CurveMinPresent = .TRUE.
PerfCurve(CurveNum)%CurveMaxPresent = .TRUE.

DEALLOCATE(A)
DEALLOCATE(Results)

END SUBROUTINE SolveRegression

SUBROUTINE Interpolate_Lagrange(DataPoint,FunctionArray,Ordinate,ISPT,IEPT,ALAG)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Raustad, FSEC
          !       DATE WRITTEN   July 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !
          ! Solves the lagrange polynomial equation for interpolation. For second-order:
          ! F(x) = y1 * ((x-x2)(x-x3) / (x1-x2)(x1-x3)) +
          !        y2 * ((x-x1)(x-x3) / (x2-x1)(x2-x3)) +
          !        y3 * ((x-x1)(x-x2) / (x3-x1)(x3-x2))
          ! where xn, yn represent data points 1-n, and x represents the interpolation point.
          !
          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
REAL(r64), INTENT(IN)   :: DataPoint       ! point used for interpolating output (x)
REAL(r64), DIMENSION(:) :: FunctionArray   ! array of output data (Y's)
REAL(r64), DIMENSION(:) :: Ordinate        ! array of input data (X's)
!        DIMENSION FunctionAry(IEPT),Ordinate(IEPT)
INTEGER, INTENT(IN)     :: ISPT            ! the starting point in the interpolated array
INTEGER, INTENT(IN)     :: IEPT            ! the ending point in the interpolated array
REAL(r64), INTENT(OUT)  :: ALAG            ! the interpolated output (y or F(x) in equation above)
          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
REAL(r64) :: Lagrange    ! intermediate variable
INTEGER   :: J, K        ! loop coungters

   ALAG = 0.0D0
   DO J=ISPT,IEPT
     Lagrange=1.0d0
     DO K=ISPT,IEPT
       IF (K .NE. J) THEN
         Lagrange = Lagrange*((DataPoint-Ordinate(K))/(Ordinate(J)-Ordinate(K)))
       END IF
     END DO
     ALAG = ALAG + Lagrange*FunctionArray(J)
   END DO
   RETURN
END SUBROUTINE Interpolate_Lagrange

LOGICAL FUNCTION IsCurveInputTypeValid(InInputType)
          ! FUNCTION INFORMATION:
          !       AUTHOR         Jason Glazer
          !       DATE WRITTEN   Oct 2009
          !       MODIFIED
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! Returns true if the input unit type is valid

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY : SameString

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=MaxNameLength), INTENT (IN) :: InInputType  ! index of curve in curve array

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
          ! na
IF (LEN_TRIM(InInputType) .GT. 0) THEN
  IF (SameString(InInputType,'DIMENSIONLESS')) THEN
    IsCurveInputTypeValid = .TRUE.
  ELSEIF (SameString(InInputType,'TEMPERATURE')) THEN
    IsCurveInputTypeValid = .TRUE.
  ELSEIF (SameString(InInputType,'PRESSURE')) THEN !cpw22Aug2010
    IsCurveInputTypeValid = .TRUE.                 !cpw22Aug2010
! CR8124 Glazer - Need to use volumetricflow and massflow not just flow
!  ELSEIF (SameString(InInputType,'FLOW')) THEN
!    IsCurveInputTypeValid = .TRUE.
  ELSEIF (SameString(InInputType,'VOLUMETRICFLOW')) THEN
    IsCurveInputTypeValid = .TRUE.
  ELSEIF (SameString(InInputType,'MASSFLOW')) THEN
    IsCurveInputTypeValid = .TRUE.
  ELSEIF (SameString(InInputType,'POWER')) THEN
    IsCurveInputTypeValid = .TRUE.
  ELSEIF (SameString(InInputType,'DISTANCE')) THEN
    IsCurveInputTypeValid = .TRUE.
  ELSE
    IsCurveInputTypeValid = .FALSE.
  END IF
ELSE
  IsCurveInputTypeValid = .TRUE. !if not used it is valid
END IF
END FUNCTION

LOGICAL FUNCTION IsCurveOutputTypeValid(InOutputType)
          ! FUNCTION INFORMATION:
          !       AUTHOR         Jason Glazer
          !       DATE WRITTEN   Oct 2009
          !       MODIFIED
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! Returns true if the output unit type is valid

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY : SameString

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=MaxNameLength), INTENT (IN) :: InOutputType  ! index of curve in curve array

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
          ! na

  IF (SameString(InOutputType,'DIMENSIONLESS')) THEN
    IsCurveOutputTypeValid = .TRUE.
  ELSEIF (SameString(InOutputType,'PRESSURE')) THEN !cpw22Aug2010
    IsCurveOutputTypeValid = .TRUE.                 !cpw22Aug2010
  ELSEIF (SameString(InOutputType,'TEMPERATURE')) THEN
    IsCurveOutputTypeValid = .TRUE.
  ELSEIF (SameString(InOutputType,'CAPACITY')) THEN
    IsCurveOutputTypeValid = .TRUE.
  ELSEIF (SameString(InOutputType,'POWER')) THEN
    IsCurveOutputTypeValid = .TRUE.
  ELSE
    IsCurveOutputTypeValid = .FALSE.
  END IF
END FUNCTION

CHARACTER(len=32) FUNCTION GetCurveType (CurveIndex)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Kenneth Tang
          !       DATE WRITTEN   Oct 2004
          !       MODIFIED       January 2006, Rick Strand; July 2006, Lixing Gu
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! Given a curve index, returns the curve type

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN)           :: CurveIndex  ! index of curve in curve array

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
          ! na

  IF (CurveIndex > 0) THEN
    SELECT CASE (PerfCurve(CurveIndex)%CurveType)
    CASE(Linear)
      GetCurveType = 'LINEAR'
    CASE(Bilinear)
      GetCurveType = 'BILINEAR'
    CASE(QuadLinear)
      GetCurveType = 'QUADLINEAR'
    CASE(Quadratic)
      GetCurveType = 'QUADRATIC'
    CASE(Cubic)
      GetCurveType = 'CUBIC'
    CASE(Biquadratic)
      GetCurveType = 'BIQUADRATIC'
    CASE(QuadraticLinear)
      GetCurveType = 'QUADRATICLINEAR'
    CASE(Bicubic)
      GetCurveType = 'BICUBIC'
    CASE(TriQuadratic)
      GetCurveType = 'TRIQUADRATIC'
    CASE(Exponent)
      GetCurveType = 'EXPONENT'
    CASE(Quartic)
      GetCurveType = 'QUARTIC'
    CASE(FanPressureRise)
      GetCurveType = 'FANPRESSURERISE'
    CASE(ExponentialSkewNormal)
      GetCurveType = 'EXPONENTIALSKEWNORMAL'
    CASE(Sigmoid)
      GetCurveType = 'SIGMOID'
    CASE(RectangularHyperbola1)
      GetCurveType = 'RECTANGULARHYPERBOLA1'
    CASE(RectangularHyperbola2)
      GetCurveType = 'RECTANGULARHYPERBOLA2'
    CASE(ExponentialDecay)
      GetCurveType = 'EXPONENTIALDECAY'
    CASE(DoubleExponentialDecay)
      GetCurveType = 'DOUBLEEXPONENTIALDECAY'

    END SELECT
  ELSE
    GetCurveType = ' '
  END IF
  RETURN
END FUNCTION GetCurveType

CHARACTER(len=MaxNameLength) FUNCTION GetCurveName (CurveIndex)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Bereket Nigusse
          !       DATE WRITTEN   May 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! Given a curve index, returns the curve name

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN)           :: CurveIndex  ! index of curve in curve array

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
          ! na

  IF (CurveIndex > 0) THEN
    GetCurveName = PerfCurve(CurveIndex)%Name
  ELSE
    GetCurveName = ' '
  END IF
  RETURN
END FUNCTION GetCurveName

INTEGER FUNCTION GetCurveIndex(CurveName)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   May 2000
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! Given a curve name, returns the curve index

          ! METHODOLOGY EMPLOYED:
          ! uses FindItemInList to search the curve array for the curve name

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: FindItemInList

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT (IN)  :: CurveName            ! name of the curve

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
          ! na

! First time GetCurveIndex is called, get the input for all the performance curves
IF (GetCurvesInputFlag) THEN
  CALL GetCurveInput
  CALL GetPressureSystemInput
  GetCurvesInputFlag = .FALSE.
END IF

IF (NumCurves > 0) THEN
  GetCurveIndex = FindItemInList(CurveName,PerfCurve(1:NumCurves)%Name,NumCurves)
ELSE
  GetCurveIndex = 0
END IF

RETURN

END FUNCTION GetCurveIndex

! This utility function grabs a curve index and performs the
! error checking
FUNCTION GetCurveCheck(alph, errFlag, ObjName) RESULT (GetCurveCheckOut)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Jason Glazer
          !       DATE WRITTEN   March 2001
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function provides a simple call to both return a curve index as well
          ! as check for validity and produce an error message.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN)   :: alph ! curve name
  LOGICAL, INTENT(INOUT)         :: errFlag
  CHARACTER(len=*), INTENT(IN)   :: ObjName ! parent object of curve
  INTEGER                        :: GetCurveCheckOut

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
          ! na

  GetCurveCheckOut = GetCurveIndex(alph)  ! convert curve name to pointer
  IF (GetCurveCheckOut .EQ. 0) THEN
    CALL ShowSevereError('Curve Not Found for Object="' // TRIM(ObjName) // '" :: '// TRIM(alph))
    errFlag=.TRUE.
  END IF
  RETURN

END FUNCTION GetCurveCheck

SUBROUTINE GetCurveMinMaxValues(CurveIndex,Var1Min,Var1Max,Var2Min,Var2Max, Var3Min, Var3Max)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Lixing Gu
          !       DATE WRITTEN   July 2006
          !       MODIFIED       B. Griffith Aug 2006 add third independent variable
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! Given the curve index, returns the minimum and maximum values specified in the input
          ! for the independent variables of the performance curve.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN)           :: CurveIndex  ! index of curve in curve array
  REAL(r64), INTENT (OUT)             :: Var1Min     ! Minimum values of 1st independent variable
  REAL(r64), INTENT (OUT)             :: Var1Max     ! Maximum values of 1st independent variable
  REAL(r64), INTENT (OUT), OPTIONAL   :: Var2Min     ! Minimum values of 2nd independent variable
  REAL(r64), INTENT (OUT), OPTIONAL   :: Var2Max     ! Maximum values of 2nd independent variable
  REAL(r64), INTENT (OUT), OPTIONAL   :: Var3Min     ! Minimum values of 2nd independent variable
  REAL(r64), INTENT (OUT), OPTIONAL   :: Var3Max     ! Maximum values of 2nd independent variable
          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:

  Var1Min = PerfCurve(CurveIndex)%Var1Min
  Var1Max = PerfCurve(CurveIndex)%Var1Max
  If (PRESENT(Var2Min)) Var2Min = PerfCurve(CurveIndex)%Var2Min
  If (PRESENT(Var2Max)) Var2Max = PerfCurve(CurveIndex)%Var2Max
  IF (PRESENT(Var3Min)) Var3Min = PerfCurve(CurveIndex)%Var3Min
  IF (PRESENT(Var3Max)) Var3Max = PerfCurve(CurveIndex)%Var3Max

RETURN

END SUBROUTINE GetCurveMinMaxValues

SUBROUTINE SetCurveOutputMinMaxValues(CurveIndex,ErrorsFound,CurveMin,CurveMax)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Richard Raustad
          !       DATE WRITTEN   Feb 2009
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! Given the curve index, sets the minimum and maximum possible value for this curve.
          ! Certain curve types have set limits (e.g., PLF curve should not be greater than 1 or less than 0.7).

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE General, ONLY: TrimSigDigits

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN)              :: CurveIndex  ! index of curve in curve array
  LOGICAL, INTENT (INOUT)           :: ErrorsFound ! TRUE when errors occur
  REAL(r64), INTENT (IN), OPTIONAL :: CurveMin ! Minimum value of curve output
  REAL(r64), INTENT (IN), OPTIONAL :: CurveMax ! Maximum values of curve output

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:

  IF(CurveIndex .GT. 0 .AND. CurveIndex .LE. NumCurves)THEN

    IF (PRESENT(CurveMin))THEN
      PerfCurve(CurveIndex)%CurveMin = CurveMin
      PerfCurve(CurveIndex)%CurveMinPresent = .TRUE.
    END IF

    IF (PRESENT(CurveMax))THEN
      PerfCurve(CurveIndex)%CurveMax = CurveMax
      PerfCurve(CurveIndex)%CurveMaxPresent = .TRUE.
    END IF

  ELSE

    CALL ShowSevereError('SetCurveOutputMinMaxValues: CurveIndex=['//trim(TrimSigDigits(CurveIndex))//  &
       '] not in range of curves=[1:'//trim(TrimSigDigits(NumCurves))//'].')
    ErrorsFound = .TRUE.

  END IF

RETURN

END SUBROUTINE SetCurveOutputMinMaxValues

SUBROUTINE GetPressureSystemInput()

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Edwin Lee
          !       DATE WRITTEN   August 2009
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Currently it just reads the input for pressure curve objects

          ! METHODOLOGY EMPLOYED:
          ! General EnergyPlus Methodology

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY  :  GetNumObjectsFound, GetObjectItem, VerifyName, FindItemInList
  USE DataIPShortcuts
!  USE PlantPressureSystem, ONLY: PlantPressureCurveData, PressureCurve

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=MaxNameLength), PARAMETER :: CurveObjectName = 'Curve:Functional:PressureDrop'

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER                         ::  NumPressure
  CHARACTER(len=MaxNameLength),DIMENSION(1) :: Alphas  ! Alpha items for object
  REAL(r64), DIMENSION(5)         :: Numbers ! Numeric items for object
  INTEGER                         :: NumAlphas  ! Number of Alphas for each GetObjectItem call
  INTEGER                         :: NumNumbers ! Number of Numbers for each GetObjectItem call
  INTEGER                         :: IOStatus   ! Used in GetObjectItem
  LOGICAL                         :: ErrsFound=.false.  ! Set to true if errors in input, fatal at end of routine
  LOGICAL                         :: IsNotOK              ! Flag to verify name
  LOGICAL                         :: IsBlank              ! Flag for blank name
  INTEGER                         :: CurveNum
  INTEGER                         :: CurveFound

  NumPressure = GetNumObjectsFound(CurveObjectName)
  ALLOCATE(PressureCurve(NumPressure))
  DO CurveNum = 1, NumPressure
    CALL GetObjectItem(CurveObjectName,CurveNum,Alphas,NumAlphas,Numbers,NumNumbers,IOStatus,     &
                NumBlank=lNumericFieldBlanks,AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
    IsNotOK=.FALSE.
    IsBlank=.FALSE.
    CALL VerifyName(Alphas(1),PressureCurve%Name,CurveNum-1,IsNotOK,IsBlank,TRIM(CurveObjectName)//' Name')
    IF (IsNotOK) THEN
      ErrsFound=.true.
      IF (IsBlank) Alphas(1)='xxxxx'
    ENDIF
    ! Need to verify that this name isn't used in Performance Curves as well.
    IF (NumCurves > 0) THEN
      CurveFound=FindItemInList(Alphas(1),PerfCurve%Name,NumCurves)
      IF (CurveFound /= 0) THEN
        CALL ShowSevereError('GetPressureCurveInput: '//trim(CurveObjectName)//'="'//trim(Alphas(1))//'", duplicate curve name.')
        CALL ShowContinueError('...Curve name duplicates one of the Performance Curves. Names must be unique across all curves.')
        ErrsFound=.true.
      ENDIF
    ENDIF
    PressureCurve(CurveNum)%Name      = Alphas(1)
    PressureCurve(CurveNum)%EquivDiameter  = Numbers(1)
    PressureCurve(CurveNum)%MinorLossCoeff = Numbers(2)
    PressureCurve(CurveNum)%EquivLength    = Numbers(3)
    PressureCurve(CurveNum)%EquivRoughness = Numbers(4)
    IF (NumNumbers > 4 .AND. .NOT. lNumericFieldBlanks(5)) THEN
      IF (Numbers(5) .NE. 0.0d0) THEN
        PressureCurve(CurveNum)%ConstantFpresent   = .TRUE.
        PressureCurve(CurveNum)%ConstantF          = Numbers(5)
      END IF
    END IF
  END DO

  NumPressureCurves=NumPressure

  IF (ErrsFound) THEN
    CALL ShowFatalError('GetPressureCurveInput: Errors found in Curve Objects.  Preceding condition(s) cause termination.')
  END IF

  RETURN

END SUBROUTINE GetPressureSystemInput

SUBROUTINE GetPressureCurveTypeAndIndex(PressureCurveName, PressureCurveType, PressureCurveIndex)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Edwin Lee
          !       DATE WRITTEN   August 2009
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Given a curve name, returns the curve type and index

          ! METHODOLOGY EMPLOYED:
          ! Curve types are:
          !  PressureCurve_Error       = pressure name was given, but curve is not available
          !  PressureCurve_None        = no pressure curve for this branch
          !  PressureCurve_Pressure    = pressure curve based on friction/minor loss
          !  PressureCurve_Generic     = curvemanager held curve which is function of flow rate

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY : FindItemInList
!  USE CurveManager,   ONLY : GetCurveIndex, GetCurveType
  USE DataBranchAirLoopPlant,  ONLY : PressureCurve_None, PressureCurve_Pressure, PressureCurve_Generic, PressureCurve_Error
!  USE PlantPressureSystem, ONLY: PlantPressureCurveData, PressureCurve

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT (IN)  :: PressureCurveName            ! name of the curve
  INTEGER, INTENT(INOUT)         :: PressureCurveType
  INTEGER, INTENT(INOUT)         :: PressureCurveIndex

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER           :: TempCurveIndex
  LOGICAL           :: FoundCurve
  CHARACTER(len=32) :: GenericCurveType

  !If input is not gotten, go ahead and get it now
  IF (GetCurvesInputFlag) THEN
    CALL GetCurveInput
    CALL GetPressureSystemInput
    GetCurvesInputFlag=.false.
  ENDIF

  !Initialize
  FoundCurve = .FALSE.
  PressureCurveType = PressureCurve_None
  PressureCurveIndex = 0

  !Try to retrieve a curve manager object
  TempCurveIndex = GetCurveIndex(PressureCurveName)

  !See if it is valid
  IF (TempCurveIndex > 0) THEN
    !We have to check the type of curve to make sure it is single independent variable type
    GenericCurveType = GetCurveType(TempCurveIndex)
    SELECT CASE (GenericCurveType)
      CASE ('LINEAR', 'QUADRATIC', 'CUBIC', 'QUARTIC', 'EXPONENT')
        PressureCurveType = PressureCurve_Generic
        PressureCurveIndex = TempCurveIndex
      CASE DEFAULT
        CALL ShowSevereError('Plant Pressure Simulation: Found error for curve: '//PressureCurveName)
        CALL ShowContinueError('Curve type detected: '//GenericCurveType)
        CALL ShowContinueError('Generic curves should be single independent variable such that DeltaP = f(mdot)')
        CALL ShowContinueError(' Therefore they should be of type: Linear, Quadratic, Cubic, Quartic, or Exponent')
        CALL ShowFatalError('Errors in pressure simulation input cause program termination')
    END SELECT
    RETURN
  END IF

  !Then try to retrieve a pressure curve object
  IF (ALLOCATED(PressureCurve)) THEN
    IF (SIZE(PressureCurve) > 0) THEN
      TempCurveIndex = FindItemInList(PressureCurveName,PressureCurve(1:SIZE(PressureCurve))%Name,SIZE(PressureCurve))
    ELSE
      TempCurveIndex = 0
    END IF
  END IF

  !See if it is valid
  IF (TempCurveIndex > 0) THEN
    PressureCurveType = PressureCurve_Pressure
    PressureCurveIndex = TempCurveIndex
    RETURN
  END IF

  !If we made it here, we didn't find either type of match

  !Last check, see if it is blank:
  IF (PressureCurveName == ' ') THEN
    PressureCurveType = PressureCurve_None
    RETURN
  END IF

  !At this point, we had a non-blank user entry with no match
  PressureCurveType = PressureCurve_Error
  RETURN

RETURN

END SUBROUTINE GetPressureCurveTypeAndIndex

REAL(r64) FUNCTION PressureCurveValue(PressureCurveIndex, MassFlow, Density, Viscosity)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Edwin Lee
          !       DATE WRITTEN   August 2009
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This will evaluate the pressure drop for components which use pressure information

          ! METHODOLOGY EMPLOYED:
          ! Friction factor pressure drop equation:
          ! DP = [f*(L/D) + K] * (rho * V^2) / 2

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  Use DataGlobals, ONLY : Pi

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)      ::  PressureCurveIndex
  REAL(r64), INTENT(IN)    ::  MassFlow
  REAL(r64), INTENT(IN)    ::  Density
  REAL(r64), INTENT(IN)    ::  Viscosity

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  REAL(r64)                ::  Diameter
  REAL(r64)                ::  MinorLossCoeff
  REAL(r64)                ::  Length
  REAL(r64)                ::  Roughness
  LOGICAL                  ::  IsConstFPresent
  REAL(r64)                ::  ConstantF
  REAL(r64)                ::  FrictionFactor
  REAL(r64)                ::  CrossSectArea
  REAL(r64)                ::  Velocity
  REAL(r64)                ::  ReynoldsNumber
  REAL(r64)                ::  RoughnessRatio

  !Retrieve data from structure
  Diameter        = PressureCurve(PressureCurveIndex)%EquivDiameter
  MinorLossCoeff  = PressureCurve(PressureCurveIndex)%MinorLossCoeff
  Length          = PressureCurve(PressureCurveIndex)%EquivLength
  Roughness       = PressureCurve(PressureCurveIndex)%EquivRoughness
  IsConstFPresent = PressureCurve(PressureCurveIndex)%ConstantFPresent
  ConstantF       = PressureCurve(PressureCurveIndex)%ConstantF

  !Intermediate calculations
  CrossSectArea         =  (Pi / 4.0d0) * Diameter**2
  Velocity              =  MassFlow / (Density * CrossSectArea)
  ReynoldsNumber        =  Density * Diameter * Velocity / Viscosity !assuming mu here
  RoughnessRatio        =  Roughness / Diameter

  !If we don't have any flow then exit out
  IF (MassFlow .LT. MassFlowTolerance) THEN
    PressureCurveValue = 0.0d0
    PressureCurve(PressureCurveIndex)%CurveInput1=MassFlow
    PressureCurve(PressureCurveIndex)%CurveInput2=Density
    PressureCurve(PressureCurveIndex)%CurveInput3=Velocity
    PressureCurve(PressureCurveIndex)%CurveOutput=0.0d0
    RETURN
  END IF

  !Calculate the friction factor
  IF (IsConstFPresent) THEN   !use the constant value
    FrictionFactor    =  ConstantF
  ELSE ! must calculate f
    FrictionFactor    =  CalculateMoodyFrictionFactor(ReynoldsNumber,RoughnessRatio)
  END IF

  !Pressure drop calculation
  PressureCurveValue  =  (FrictionFactor * (Length / Diameter) + MinorLossCoeff) * (Density * Velocity**2) / 2.0d0

  IF (PressureCurve(PressureCurveIndex)%EMSOverrideOn)   &
     PressureCurveValue = PressureCurve(PressureCurveIndex)%EMSOverrideCurveValue

  PressureCurve(PressureCurveIndex)%CurveInput1=MassFlow
  PressureCurve(PressureCurveIndex)%CurveInput2=Density
  PressureCurve(PressureCurveIndex)%CurveInput3=Velocity
  PressureCurve(PressureCurveIndex)%CurveOutput=PressureCurveValue

END FUNCTION PressureCurveValue

REAL(r64) FUNCTION CalculateMoodyFrictionFactor(ReynoldsNumber, RoughnessRatio)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Edwin Lee
          !       DATE WRITTEN   August 2009
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This will evaluate the moody friction factor based on Reynolds number and roughness ratio

          ! METHODOLOGY EMPLOYED:
          ! General empirical correlations for friction factor based on Moody Chart data

          ! REFERENCES:
          ! Haaland, SE (1983). "Simple and Explicit Formulas for the Friction Factor in Turbulent Flow".
          !   Trans. ASIVIE, J. of Fluids Engineering 103: 89-90.

          ! USE STATEMENTS:
  USE General, ONLY: RoundSigDigits

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  REAL(r64), INTENT(IN)    ::  ReynoldsNumber
  REAL(r64), INTENT(IN)    ::  RoughnessRatio

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  REAL(r64)                    ::  Term1, Term2, Term3
  CHARACTER(len=MaxNameLength) ::  RR, Re
  LOGICAL, SAVE                ::  FrictionFactorErrorHasOccurred = .FALSE.

  !Check for no flow before calculating values
  IF (ReynoldsNumber .EQ. 0.0d0) THEN
    CalculateMoodyFrictionFactor = 0.0d0
    RETURN
  END IF

  !Check for no roughness also here
  IF (RoughnessRatio .EQ. 0.0d0) THEN
    CalculateMoodyFrictionFactor = 0.0d0
    RETURN
  END IF

  !Calculate the friction factor
  Term1 = (RoughnessRatio/3.7d0)**(1.11d0)
  Term2 = 6.9d0/ReynoldsNumber
  Term3 = -1.8d0 * LOG10(Term1 + Term2)
  IF (Term3 .NE. 0.0d0) THEN
    CalculateMoodyFrictionFactor = Term3 ** (-2.0d0)
  ELSE
    IF (.NOT. FrictionFactorErrorHasOccurred) THEN
      RR=RoundSigDigits(RoughnessRatio,7)
      Re=RoundSigDigits(ReynoldsNumber,1)
      CALL ShowSevereError('Plant Pressure System: Error in moody friction factor calculation')
      CALL ShowContinueError('Current Conditions: Roughness Ratio='//TRIM(RR)//'; Reynolds Number='//TRIM(Re))
      CALL ShowContinueError('These conditions resulted in an unhandled numeric issue.')
      CALL ShowContinueError('Please contact EnergyPlus support/development team to raise an alert about this issue')
      CALL ShowContinueError('This issue will occur only one time.  The friction factor has been reset to 0.04 for calculations')
      FrictionFactorErrorHasOccurred = .TRUE.
    END IF
    CalculateMoodyFrictionFactor = 0.04d0
  END IF

  RETURN

END FUNCTION CalculateMoodyFrictionFactor

FUNCTION GetCurveObjectTypeNum( CurveIndex ) RESULT (CurveOrTableObjectTypeNum)

          ! FUNCTION INFORMATION:
          !       AUTHOR         B. Griffith
          !       DATE WRITTEN   April 2013
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! get the object type integer identifier for curves and tables

          ! METHODOLOGY EMPLOYED:
          ! retrieve from data structure.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN)           :: CurveIndex  ! index of curve in curve array
  INTEGER                        :: CurveOrTableObjectTypeNum

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
          ! na
  IF (CurveIndex > 0) THEN
    CurveOrTableObjectTypeNum = PerfCurve(CurveIndex)%ObjectType
  ELSE
    CurveOrTableObjectTypeNum = 0

  ENDIF

  RETURN

END FUNCTION GetCurveObjectTypeNum

!=================================================================================================!

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

END MODULE CurveManager
