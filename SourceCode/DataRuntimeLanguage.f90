!Data only module for EMS runtime language

MODULE DataRuntimeLanguage      ! EnergyPlus Data-Only Module

          ! MODULE INFORMATION:
          !       AUTHOR         Peter Graham Ellis
          !       DATE WRITTEN   June 2006
          !       MODIFIED       Brent Griffith, May 2009
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS MODULE:
          !

          ! METHODOLOGY EMPLOYED: na
          !

          ! USE STATEMENTS:
USE DataPrecisionGlobals
USE DataGlobals, ONLY: MaxNameLength

IMPLICIT NONE   ! Enforce explicit typing of all variables

PUBLIC          ! By definition, all variables which are placed in this data-only
                ! module should be available to other modules and routines.
                ! Thus, all variables in this module must be PUBLIC.

          ! MODULE PARAMETER DEFINITIONS:
INTEGER, PARAMETER :: ValueNull       = 0 ! Erl entity type, "Null" value
INTEGER, PARAMETER :: ValueNumber     = 1 ! Erl entity type,  hard numeric value
INTEGER, PARAMETER :: ValueString     = 2 ! Erl entity type,  character data
INTEGER, PARAMETER :: ValueArray      = 3 ! Erl entity type,  not used yet, for future array type
INTEGER, PARAMETER :: ValueVariable   = 4 ! Erl entity type,  Erl variable
INTEGER, PARAMETER :: ValueExpression = 5 ! Erl entity type,  Erl expression
INTEGER, PARAMETER :: ValueTrend      = 6 ! Erl entity type,  Erl trend variable
INTEGER, PARAMETER :: ValueError      = 7 ! Erl entity type, processing of an expression failed, returned error

INTEGER, PARAMETER :: PntrReal        = 301 ! data type for overloaded pointer management, double real
INTEGER, PARAMETER :: PntrInteger     = 302 ! data type for overloaded pointer management, integer
INTEGER, PARAMETER :: PntrLogical     = 303 ! data type for overloaded pointer management, logical

INTEGER, PARAMETER :: MaxWhileLoopIterations = 1000000 ! protect from infinite loop in WHILE loops

! Parameters for identifying operator types in Erl
! The number of these parameters indicates the order of precedence
INTEGER, PARAMETER :: OperatorLiteral        = 1  ! Just stores a literal value
INTEGER, PARAMETER :: OperatorNegative       = 2  ! -  (unary) No LHS?
INTEGER, PARAMETER :: OperatorDivide         = 3  ! /
INTEGER, PARAMETER :: OperatorMultiply       = 4  ! *
INTEGER, PARAMETER :: OperatorSubtract       = 5  ! -  (binary)
INTEGER, PARAMETER :: OperatorAdd            = 6  ! +  (binary)
INTEGER, PARAMETER :: OperatorEqual          = 7  ! ==
INTEGER, PARAMETER :: OperatorNotEqual       = 8  ! <>
INTEGER, PARAMETER :: OperatorLessOrEqual    = 9  ! <=
INTEGER, PARAMETER :: OperatorGreaterOrEqual = 10 ! >=
INTEGER, PARAMETER :: OperatorLessThan       = 11 ! <
INTEGER, PARAMETER :: OperatorGreaterThan    = 12 ! >
INTEGER, PARAMETER :: OperatorRaiseToPower   = 13 ! ^
INTEGER, PARAMETER :: OperatorLogicalAND     = 14 ! &&
INTEGER, PARAMETER :: OperatiorLogicalOR     = 15 ! ||
! note there is an important check "> 15" to distinguish operators from functions
!  so becareful if renumber these parameters.  Binary operator additions should get inserted here rather than appended

!parameters for built-in Erl functions, these are processed like operators and numbering
! must be sequential with the operators.
! math functions
INTEGER, PARAMETER :: FuncRound              = 16 ! accessor for Fortran's DNINT()
INTEGER, PARAMETER :: FuncMod                = 17 ! accessor for Fortran's MOD()
INTEGER, PARAMETER :: FuncSin                = 18 ! accessor for Fortran's Sin()
INTEGER, PARAMETER :: FuncCos                = 19 ! accessor for Fortran's Cos()
INTEGER, PARAMETER :: FuncArcSin             = 20 ! accessor for Fortran's ASIN()
INTEGER, PARAMETER :: FuncArcCos             = 21 ! accessor for Fortran's ACOS()
INTEGER, PARAMETER :: FuncDegToRad           = 22 ! Multiplies degrees by DegToRad
INTEGER, PARAMETER :: FuncRadToDeg           = 23 ! Divides radians by DegToRad
INTEGER, PARAMETER :: FuncExp                = 24 ! accessor for Fortran's EXP()
INTEGER, PARAMETER :: FuncLn                 = 25 ! accessor for Fortran's LOG()
INTEGER, PARAMETER :: FuncMax                = 26 ! accessor for Fortran's MAX()
INTEGER, PARAMETER :: FuncMin                = 27 ! accessor for Fortran's MIN()
INTEGER, PARAMETER :: FuncABS                = 28 ! accessor for Fortran's ABS()
INTEGER, PARAMETER :: FuncRandU              = 29 ! accessor for Fortran's Random_Number() intrinsic, uniform distribution
INTEGER, PARAMETER :: FuncRandG              = 30 ! accessor for Gaussian/normal distribution random number
INTEGER, PARAMETER :: FuncRandSeed           = 31 ! accessor for Fortran's Random_Seed() intrinsic

! begin psychrometric routines
INTEGER, PARAMETER :: FuncRhoAirFnPbTdbW     = 32 ! accessor for E+ psych routine
INTEGER, PARAMETER :: FuncCpAirFnWTdb        = 33 ! accessor for E+ psych routine
INTEGER, PARAMETER :: FuncHfgAirFnWTdb       = 34 ! accessor for E+ psych routine
INTEGER, PARAMETER :: FuncHgAirFnWTdb        = 35 ! accessor for E+ psych routine
INTEGER, PARAMETER :: FuncTdpFnTdbTwbPb      = 36 ! accessor for E+ psych routine
INTEGER, PARAMETER :: FuncTdpFnWPb           = 37 ! accessor for E+ psych routine
INTEGER, PARAMETER :: FuncHFnTdbW            = 38 ! accessor for E+ psych routine
INTEGER, PARAMETER :: FuncHFnTdbRhPb         = 39 ! accessor for E+ psych routine
INTEGER, PARAMETER :: FuncTdbFnHW            = 40 ! accessor for E+ psych routine
INTEGER, PARAMETER :: FuncRhovFnTdbRh        = 41 ! accessor for E+ psych routine
INTEGER, PARAMETER :: FuncRhovFnTdbRhLBnd0C  = 42 ! accessor for E+ psych routine
INTEGER, PARAMETER :: FuncRhovFnTdbWPb       = 43 ! accessor for E+ psych routine
INTEGER, PARAMETER :: FuncRhFnTdbRhov        = 44 ! accessor for E+ psych routine
INTEGER, PARAMETER :: FuncRhFnTdbRhovLBnd0C  = 45 ! accessor for E+ psych routine
INTEGER, PARAMETER :: FuncRhFnTdbWPb         = 46 ! accessor for E+ psych routine
INTEGER, PARAMETER :: FuncTwbFnTdbWPb        = 47 ! accessor for E+ psych routine
INTEGER, PARAMETER :: FuncVFnTdbWPb          = 48 ! accessor for E+ psych routine
INTEGER, PARAMETER :: FuncWFnTdpPb           = 49 ! accessor for E+ psych routine
INTEGER, PARAMETER :: FuncWFnTdbH            = 50 ! accessor for E+ psych routine
INTEGER, PARAMETER :: FuncWFnTdbTwbPb        = 51 ! accessor for E+ psych routine
INTEGER, PARAMETER :: FuncWFnTdbRhPb         = 52 ! accessor for E+ psych routine
INTEGER, PARAMETER :: FuncPsatFnTemp         = 53 ! accessor for E+ psych routine
INTEGER, PARAMETER :: FuncTsatFnHPb          = 54 ! accessor for E+ psych routine
INTEGER, PARAMETER :: FuncTsatFnPb           = 55 ! not public in PsychRoutines.f90 so not really available in EMS.
INTEGER, PARAMETER :: FuncCpCW               = 56 ! accessor for E+ psych routine
INTEGER, PARAMETER :: FuncCpHW               = 57 ! accessor for E+ psych routine
INTEGER, PARAMETER :: FuncRhoH2O             = 58 ! accessor for E+ psych routine

! Simulation Management Functions
INTEGER, PARAMETER :: FuncFatalHaltEp        = 59 ! accessor for E+ error management, "Fatal" level
INTEGER, PARAMETER :: FuncSevereWarnEp       = 60 ! accessor for E+ error management, "Severe" level
INTEGER, PARAMETER :: FuncWarnEp             = 61 ! accessor for E+ error management, "Warning" level

! Trend variable handling Functions
INTEGER, PARAMETER :: FuncTrendValue         = 62 ! accessor for Erl Trend variables, instance value
INTEGER, PARAMETER :: FuncTrendAverage       = 63 ! accessor for Erl Trend variables, average value
INTEGER, PARAMETER :: FuncTrendMax           = 64 ! accessor for Erl Trend variables, max value
INTEGER, PARAMETER :: FuncTrendMin           = 65 ! accessor for Erl Trend variables, min value
INTEGER, PARAMETER :: FuncTrendDirection     = 66 ! accessor for Erl Trend variables, slope value
INTEGER, PARAMETER :: FuncTrendSum           = 67 ! accessor for Erl Trend variables, sum value

! Curve and Table access function
INTEGER, PARAMETER :: FuncCurveValue         = 68

INTEGER, PARAMETER :: NumPossibleOperators   = 68 ! total number of operators and built-in functions

          ! DERIVED TYPE DEFINITIONS:
TYPE OutputVarSensorType
  CHARACTER(len=MaxNameLength)            :: Name          = '' ! name of associated Erl Variable
  CHARACTER(len=MaxNameLength)            :: UniqueKeyName = '' ! unique key name associated with output variable
  CHARACTER(len=MaxNameLength)            :: OutputVarName = '' ! name of output variable
  LOGICAL                                 :: CheckedOkay   = .FALSE. ! set to true once checked out okay
  INTEGER                                 :: Type          = 0  ! type of output var, 1=integer, 2=real, 3=meter
  INTEGER                                 :: Index         = 0  ! ref index in output processor, points to variable
  INTEGER                                 :: VariableNum   = 0  ! ref to global variable in runtime language
  INTEGER                                 :: SchedNum      = 0  ! ref index ptr to schedule service (filled if Schedule Value)
!  INTEGER                                 :: VarType       = 0
END TYPE OutputVarSensorType

TYPE InternalVarsAvailableType
  ! structure for internal data available for use in Erl that are not sourced by output variables
  CHARACTER(len=MaxNameLength)            :: DataTypeName    = '' ! general internal variable name registered, All uppercase
  CHARACTER(len=MaxNameLength)            :: UniqueIDName    = '' ! unique id for internal var, All uppercase
  CHARACTER(len=MaxNameLength)            :: Units           = '' ! registered units, used for reporting and checks.
  INTEGER                                 :: PntrVarTypeUsed = 0 ! data type used: integer (PntrInteger) or real (PntrReal)
  REAL(r64), POINTER                      :: RealValue    ! fortran POINTER to the REAL value that is being accessed
  INTEGER, POINTER                        :: IntValue     ! fortran POINTER to the Integer value that is being accessed
END TYPE InternalVarsAvailableType

TYPE InternalVarsUsedType
  ! structure for internal data that user has selected to use in Erl.
  CHARACTER(len=MaxNameLength)            :: Name                 = '' ! Erl variable name
  CHARACTER(len=MaxNameLength)            :: InternalDataTypeName = '' ! general internal variable name, All uppercase
  CHARACTER(len=MaxNameLength)            :: UniqueIDName         = '' ! unique id for internal var, All uppercase
  LOGICAL                                 :: CheckedOkay          = .FALSE. ! set to true once matched to available internal var
  INTEGER                                 :: ErlVariableNum       = 0  ! points to global Erl variable, matches Name
  INTEGER                                 :: InternVarNum         = 0 ! points to index match in EMSInternalVarsAvailable structure
END TYPE InternalVarsUsedType

TYPE EMSActuatorAvailableType
  ! structure for all the actuators available for use in Erl
  CHARACTER(len=MaxNameLength)            :: ComponentTypeName = '' ! general actuator name registered, All uppercase
  CHARACTER(len=MaxNameLength)            :: UniqueIDName      = '' ! unique id for actuator, All uppercase
  CHARACTER(len=MaxNameLength)            :: ControlTypeName   = '' ! control type id for actuator, All uppercase
  CHARACTER(len=MaxNameLength)            :: Units             = '' ! control value units, used for reporting and checks.
  INTEGER                                 :: PntrVarTypeUsed   = 0 ! data type used: integer (PntrInteger), real (PntrReal)
                                                                   ! or logical (PntrLogical)
  LOGICAL, POINTER                        :: Actuated  ! fortran POINTER to the logical value that signals EMS is actuating
  REAL(r64), POINTER                      :: RealValue ! fortran POINTER to the REAL value that is being actuated
  INTEGER, POINTER                        :: IntValue  ! fortran POINTER to the Integer value that is being actuated
  LOGICAL, POINTER                        :: LogValue  ! fortran POINTER to the Logical value that is being actuated

END TYPE EMSActuatorAvailableType

TYPE ActuatorUsedType
  ! structure for actuators user selected to use in Erl
  CHARACTER(len=MaxNameLength)            :: Name                = '' ! Erl variable name
  CHARACTER(len=MaxNameLength)            :: ComponentTypeName   = '' ! general actuator name, All uppercase
  CHARACTER(len=MaxNameLength)            :: UniqueIDName        = '' ! unique id for actuator, All uppercase
  CHARACTER(len=MaxNameLength)            :: ControlTypeName     = '' ! control type id for actuator, All uppercase
  LOGICAL                                 :: CheckedOkay         = .FALSE.  ! set to true once matched to available actuator
  INTEGER                                 :: ErlVariableNum      = 0   ! points to global Erl variable, matches Name
  INTEGER                                 :: ActuatorVariableNum = 0   ! points to index match in EMSActuatorAvailable structure
END TYPE ActuatorUsedType

TYPE EMSProgramCallManagementType
  ! structure for Erl program calling managers
  CHARACTER(len=MaxNameLength)            :: Name           = ' ' ! user defined name for calling manager
  INTEGER                                 :: CallingPoint   = 0 ! EMS Calling point for this manager, see parameters emsCallFrom*
  INTEGER                                 :: NumErlPrograms = 0 ! count of total number of Erl programs called by this manager
  INTEGER, DIMENSION(:), ALLOCATABLE      :: ErlProgramARR ! list of integer pointers to Erl programs used by this manager
END TYPE EMSProgramCallManagementType

TYPE ErlValueType
  ! instance data structure for the values taken by Erl variables, nested structure in ErlVariable
  INTEGER                        :: Type            = 0   ! value type, eg. ValueNumber,
  REAL(r64)                      :: Number          = 0.0d0 ! numeric value instance for Erl variable
  CHARACTER(len=2*MaxNameLength) :: String          = ''  ! string data types in Erl (not used yet)
  INTEGER                        :: Variable        = 0   ! Pointer to another Erl variable
!  Might be good to change names to VariableNum and ExpressionNum just to be clear
  INTEGER                        :: Expression      = 0   ! Pointer to another Erl expression (e.g. compound operators)
  LOGICAL                        :: TrendVariable   = .FALSE. ! true if Erl variable is really a trend variable
  INTEGER                        :: TrendVarPointer = 0   ! index to match in TrendVariable structure
  CHARACTER(len=2*MaxNameLength) :: Error           = ''  ! holds error message string for reporting
END TYPE ErlValueType

TYPE ErlVariableType
  ! structure for Erl variables
  CHARACTER(len=MaxNameLength)   :: Name     = ''      ! Erl Variable Name
  INTEGER                        :: StackNum = 0       ! 0 for global Erl variables, index in ErlStack structure if local
  TYPE(ErlValueType)             :: Value              ! values taken by Erl variables
  LOGICAL                        :: ReadOnly = .FALSE. ! true if Erl variable is read-only
  LOGICAL                        :: SetByExternalInterface = .FALSE.  ! set to true if value is set by ExternalInterface
END TYPE ErlVariableType

TYPE InstructionType
  ! nested structure inside ErlStack that holds program instructions
  INTEGER                        :: LineNum   = 0 ! Erl program line number reference
  INTEGER                        :: Keyword   = 0 ! type of instruction for this line, e.g. KeywordSet, KeywordIf, etc
  INTEGER                        :: Argument1 = 0 ! Index to a variable, function, expression, or stack
  INTEGER                        :: Argument2 = 0 ! Index to a variable, function, expression, or stack
END TYPE InstructionType

TYPE ErlStackType  !  Stores Erl programs in a stack of statements/instructions
  CHARACTER(len=MaxNameLength)                     :: Name = ''           ! Erl program or subroutine name, user defined
  INTEGER                                          :: NumLines = 0        ! count of lines in Erl program or subroutine
  CHARACTER(len=2*MaxNameLength),    DIMENSION(:), ALLOCATABLE :: Line    ! string array holding lines of Erl code (for processing)
  INTEGER                                          :: NumInstructions = 0 ! count of program instructions in stack
  TYPE(InstructionType), DIMENSION(:), ALLOCATABLE :: Instruction         ! structure array of program instructions
  INTEGER                                          :: NumErrors = 0       ! count of errors during stack parsing
  CHARACTER(len=2*MaxNameLength),    DIMENSION(:), ALLOCATABLE :: Error   ! array of error messages from stack parsing
END TYPE ErlStackType

TYPE ErlExpressionType
  INTEGER                                       :: Operator    = 0 ! indicates the type of operator or function 1..64
  INTEGER                                       :: NumOperands = 0 ! count of operands in expression
  TYPE(ErlValueType), DIMENSION(:), ALLOCATABLE :: Operand         ! holds Erl values for operands in expression
END TYPE ErlExpressionType

TYPE OperatorType
  ! structure for operators and functions, used to look up information about each operator or function
  CHARACTER(len=20)            :: Symbol      = '' ! string representation of operator or function (for reporting)
  INTEGER                      :: Code        = 0  ! integer code 1..64, identifies operator or function
  INTEGER                      :: NumOperands = 0  ! count of operands or function arguments.
END TYPE OperatorType

Type TrendVariableType
  CHARACTER(len=MaxNameLength)   :: Name               = '' !
  Integer                        :: ErlVariablePointer = 0  ! the Erl variable being logged in trend
  INTEGER                        :: LogDepth = 0            ! number of timesteps back
  REAL(r64), DIMENSION(:), ALLOCATABLE :: TrendValARR       ! the main storage of trend data
  REAL(r64), DIMENSION(:), ALLOCATABLE :: tempTrendARR      ! temporary holder during push
  REAL(r64), DIMENSION(:), ALLOCATABLE :: TimeARR           ! hours back in time for trend points
END TYPE TrendVariableType

          ! MODULE VARIABLE TYPE DECLARATIONS:
TYPE(ErlVariableType),     DIMENSION(:), ALLOCATABLE :: ErlVariable       ! holds Erl variables in a structure array
TYPE(ErlStackType),        DIMENSION(:), ALLOCATABLE :: ErlStack          ! holds Erl programs in separate "stacks"
TYPE(ErlExpressionType),   DIMENSION(:), ALLOCATABLE :: ErlExpression     ! holds Erl expressions in structure array
TYPE(OperatorType),        DIMENSION(:), ALLOCATABLE :: PossibleOperators ! hard library of available operators and functions
TYPE(TrendVariableType),   DIMENSION(:), ALLOCATABLE :: TrendVariable     ! holds Erl trend varialbes in a structure array
TYPE(OutputVarSensorType), DIMENSION(:), ALLOCATABLE :: Sensor            ! EMS:SENSOR objects used (from output variables)

TYPE(EMSActuatorAvailableType),  DIMENSION(:), ALLOCATABLE :: EMSActuatorAvailable ! actuators that could be used
TYPE(ActuatorUsedType),          DIMENSION(:), ALLOCATABLE :: EMSActuatorUsed  ! actuators that are used
TYPE(InternalVarsAvailableType), DIMENSION(:), ALLOCATABLE :: EMSInternalVarsAvailable ! internal data that could be used
TYPE(InternalVarsUsedType),      DIMENSION(:), ALLOCATABLE :: EMSInternalVarsUsed ! internal data that are used

TYPE(EMSProgramCallManagementType), DIMENSION(:), ALLOCATABLE :: EMSProgramCallManager ! program calling managers

TYPE(ErlValueType),SAVE :: Null  =ErlValueType(0,0.0,' ',0,0,.FALSE.,0,' ') ! special "null" Erl variable value instance
TYPE(ErlValueType),SAVE :: False =ErlValueType(0,0.0,' ',0,0,.FALSE.,0,' ') ! special "false" Erl variable value instance
TYPE(ErlValueType),SAVE :: True  =ErlValueType(0,0.0,' ',0,0,.FALSE.,0,' ') ! special "True" Erl variable value instance, gets reset

          ! INTERFACE BLOCK SPECIFICATIONS: na

          ! MODULE VARIABLE DECLARATIONS:
INTEGER, DIMENSION(:), ALLOCATABLE :: EMSProgram

INTEGER :: NumProgramCallManagers      = 0 ! count of Erl program managers with calling points
INTEGER :: NumSensors                  = 0 ! count of EMS sensors used in model (data from output variables)
INTEGER :: numActuatorsUsed            = 0 ! count of EMS actuators used in model
INTEGER :: numEMSActuatorsAvailable    = 0 ! count of EMS actuators available for use in such a model
INTEGER :: maxEMSActuatorsAvailable    = 0 ! count of EMS current maximum actuators available for use in such a model
INTEGER :: numInternalVariablesUsed    = 0 ! count of EMS internal variables used in model
INTEGER :: numEMSInternalVarsAvailable = 0 ! count of EMS internal variables available for use in such a model
INTEGER :: maxEMSInternalVarsAvailable = 0 ! count of EMS current maximum internal variables available for use in such a model
INTEGER :: varsAvailableAllocInc       =1000 ! allocation increment for variable arrays

INTEGER :: NumErlPrograms              = 0 ! count of Erl programs in model
INTEGER :: NumErlSubroutines           = 0 ! count of Erl subroutines in model
INTEGER :: NumUserGlobalVariables      = 0 ! count of global EMS variables defined by user
INTEGER :: NumErlVariables             = 0 ! count of Erl variables
INTEGER :: NumErlStacks                = 0 ! count of Erl program stacks in model. sum of programs and subroutines
INTEGER :: NumExpressions              = 0 ! count of Erl expressions
INTEGER :: NumEMSOutputVariables       = 0 ! count of EMS output variables, custom output variables from Erl
INTEGER :: NumEMSMeteredOutputVariables= 0 ! count of EMS metered output variables, custom meters from Erl
INTEGER :: NumErlTrendVariables        = 0 ! count of EMS trend variables in model
INTEGER :: NumEMSCurveIndices          = 0 ! count of EMS curve index variables in model
INTEGER :: NumEMSConstructionIndices   = 0 ! count of EMS construction index variables in model

!######################################################################################################################################
!code for ExternalInterface
INTEGER :: NumExternalInterfaceGlobalVariables = 0 ! count of ExternalInterface runtime variable
INTEGER :: NumExternalInterfaceFunctionalMockupUnitImportGlobalVariables = 0 ! count of ExternalInterface runtime variable for FMUImport
                                                   ! will be updated with values from ExternalInterface
INTEGER :: NumExternalInterfaceFunctionalMockupUnitExportGlobalVariables = 0 ! count of ExternalInterface runtime variable for FMUExport
                                                   ! will be updated with values from ExternalInterface
INTEGER :: NumExternalInterfaceActuatorsUsed   = 0 ! count of ExternalInterface Actuators
INTEGER :: NumExternalInterfaceFunctionalMockupUnitImportActuatorsUsed   = 0 ! count of ExternalInterface Actuators for FMUImport
INTEGER :: NumExternalInterfaceFunctionalMockupUnitExportActuatorsUsed   = 0 ! count of ExternalInterface Actuators for FMUExport

!######################################################################################################################################

INTEGER :: OutputEMSFileUnitNum        = 0 ! file lun handle for open EMS output file
LOGICAL :: OutputEDDFile               = .FALSE. ! set to true if user requests EDD output file be written
LOGICAL :: OutputFullEMSTrace          = .FALSE. ! how much to write out to trace, if true do verbose for each line
LOGICAL :: OutputEMSErrors             = .FALSE. ! how much to write out to trace, if true include Erl error messages
LOGICAL :: OutputEMSActuatorAvailFull  = .FALSE. ! how much to write out to EDD file, if true dump full combinatorial actuator list
LOGICAL :: OutputEMSActuatorAvailSmall = .FALSE. ! how much to write out to EDD file, if true dump actuator list without key names
LOGICAL :: OutputEMSInternalVarsFull   = .FALSE. ! how much to write out to EDD file, if true dump full combinatorial internal list
LOGICAL :: OutputEMSInternalVarsSmall  = .FALSE. ! how much to write out to EDD file, if true dump internal list without key names

LOGICAL, DIMENSION(:,:), ALLOCATABLE  :: EMSConstructActuatorChecked
LOGICAL, DIMENSION(:,:), ALLOCATABLE  :: EMSConstructActuatorIsOkay

CONTAINS

SUBROUTINE ValidateEMSVariableName(cModuleObject,cFieldValue,cFieldName,errFlag,ErrorsFound)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   May 2012
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Consolidate error checking on EMS variable names.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataInterfaces, ONLY: ShowSevereError,ShowContinueError

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: cModuleObject  ! the current object name
  CHARACTER(len=*), INTENT(IN) :: cFieldValue    ! the field value
  CHARACTER(len=*), INTENT(IN) :: cFieldName     ! the current field name
  LOGICAL, INTENT(OUT)         :: errFlag        ! true if errors found in this routine.
  LOGICAL, INTENT(INOUT)       :: ErrorsFound    ! true if errors found in this routine.


          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER  :: InvalidStartCharacters='0123456789'

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
 INTEGER :: pos

  errFlag=.false.
  IF (SCAN(TRIM(cFieldValue), ' ') > 0) THEN
    CALL ShowSevereError(TRIM(cModuleObject)//'="'//TRIM(cFieldValue)//'", Invalid variable name entered.')
    CALL ShowContinueError('...'//TRIM(cFieldName)//'; Names used as EMS variables cannot contain spaces')
    errFlag=.true.
    ErrorsFound = .TRUE.
  ENDIF
  IF (SCAN(TRIM(cFieldValue), '-') > 0) THEN
    CALL ShowSevereError(TRIM(cModuleObject)//'="'//TRIM(cFieldValue)//'", Invalid variable name entered.')
    CALL ShowContinueError('...'//TRIM(cFieldName)//'; Names used as EMS variables cannot contain "-" characters.')
    errFlag=.true.
    ErrorsFound = .TRUE.
  ENDIF
  IF (SCAN(TRIM(cFieldValue), '+') > 0) THEN
    CALL ShowSevereError(TRIM(cModuleObject)//'="'//TRIM(cFieldValue)//'", Invalid variable name entered.')
    CALL ShowContinueError('...'//TRIM(cFieldName)//'; Names used as EMS variables cannot contain "+" characters.')
    errFlag=.true.
    ErrorsFound = .TRUE.
  ENDIF
  IF (SCAN(TRIM(cFieldValue), '.') > 0) THEN
    CALL ShowSevereError(TRIM(cModuleObject)//'="'//TRIM(cFieldValue)//'", Invalid variable name entered.')
    CALL ShowContinueError('...'//TRIM(cFieldName)//'; Names used as EMS variables cannot contain "." characters.')
    errFlag=.true.
    ErrorsFound = .TRUE.
  ENDIF
  pos=SCAN(cFieldValue(1:1),InvalidStartCharacters)
  IF (pos > 0) THEN
    CALL ShowSevereError(TRIM(cModuleObject)//'="'//TRIM(cFieldValue)//'", Invalid variable name entered.')
    CALL ShowContinueError('...'//TRIM(cFieldName)//'; Names used as EMS variables cannot start with numeric characters.')
    errFlag=.true.
    ErrorsFound = .TRUE.
  ENDIF

  RETURN

END SUBROUTINE ValidateEMSVariableName

SUBROUTINE ValidateEMSProgramName(cModuleObject,cFieldValue,cFieldName,cSubType,errFlag,ErrorsFound)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   May 2012
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Consolidate error checking on EMS variable names.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataInterfaces, ONLY: ShowSevereError,ShowContinueError

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: cModuleObject  ! the current object name
  CHARACTER(len=*), INTENT(IN) :: cFieldValue    ! the field value
  CHARACTER(len=*), INTENT(IN) :: cFieldName     ! the current field name
  CHARACTER(len=*), INTENT(IN) :: cSubType       ! sub type = Program or Subroutine
  LOGICAL, INTENT(OUT)         :: errFlag        ! true if errors found in this routine.
  LOGICAL, INTENT(INOUT)       :: ErrorsFound    ! true if errors found in this routine.


          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER  :: InvalidStartCharacters='0123456789'

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
 INTEGER :: pos

  errFlag=.false.
  IF (SCAN(TRIM(cFieldValue), ' ') > 0) THEN
    CALL ShowSevereError(TRIM(cModuleObject)//'="'//TRIM(cFieldValue)//'", Invalid variable name entered.')
    CALL ShowContinueError('...'//TRIM(cFieldName)//'; Names used for EMS '//trim(cSubType)//' cannot contain spaces')
    errFlag=.true.
    ErrorsFound = .TRUE.
  ENDIF
  IF (SCAN(TRIM(cFieldValue), '-') > 0) THEN
    CALL ShowSevereError(TRIM(cModuleObject)//'="'//TRIM(cFieldValue)//'", Invalid variable name entered.')
    CALL ShowContinueError('...'//TRIM(cFieldName)//'; Names used for EMS '//trim(cSubType)//' cannot contain "-" characters.')
    errFlag=.true.
    ErrorsFound = .TRUE.
  ENDIF
  IF (SCAN(TRIM(cFieldValue), '+') > 0) THEN
    CALL ShowSevereError(TRIM(cModuleObject)//'="'//TRIM(cFieldValue)//'", Invalid variable name entered.')
    CALL ShowContinueError('...'//TRIM(cFieldName)//'; Names used for EMS '//trim(cSubType)//' cannot contain "+" characters.')
    errFlag=.true.
    ErrorsFound = .TRUE.
  ENDIF
!  pos=SCAN(cFieldValue(1:1),InvalidStartCharacters)
!  IF (pos > 0) THEN
!    CALL ShowSevereError(TRIM(cModuleObject)//'="'//TRIM(cFieldValue)//'", Invalid variable name entered.')
!    CALL ShowContinueError('...'//TRIM(cFieldName)//'; Names used as EMS variables cannot start with numeric characters.')
!    errFlag=.true.
!    ErrorsFound = .TRUE.
!  ENDIF

  RETURN

END SUBROUTINE ValidateEMSProgramName

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

END MODULE DataRuntimeLanguage
