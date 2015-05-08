MODULE RootFinder
  ! Module containing the derivative-free, root finder routines for smooth,
  ! "mostly" monotonic functions in one variable.
  !
  ! The algorithm is guaranteed to find the root if:
  ! - it is bracketed between min/max bounds
  ! - there is an odd number of roots between the min/max bounds
  !
  ! Note that there is an even number of roots between the min/max bounds then it is possible
  ! that the algorithm will terminate with a min or max constrained solution instead of the
  ! actual root.
  !

  ! MODULE INFORMATION:
  !       AUTHOR         Dimitri Curtil (LBNL)
  !       DATE WRITTEN   February 2006
  !       MODIFIED       na
  !       RE-ENGINEERED  na

  ! PURPOSE OF THIS MODULE:
  ! To encapsulate the data and algorithms required to
  ! find the root of a 1-dimensional function F(X) = Y .
  !
  ! At any moment during the solution process, the following condition is satisfied:
  ! - For an overall increasing function F(X):
  !  MinPoint%X <= LowerPoint%X <= CurrentPoint%X <= UpperPoint%X <= MaxPoint%X
  !  MinPoint%Y <= LowerPoint%Y <= CurrentPoint%Y <= UpperPoint%Y <= MaxPoint%Y
  ! - For an overall decreasing function F(X):
  !  MinPoint%X <= LowerPoint%X <= CurrentPoint%X <= UpperPoint%X <= MaxPoint%X
  !  MinPoint%Y >= LowerPoint%Y >= CurrentPoint%Y >= UpperPoint%Y >= MaxPoint%Y
  !
  ! Requirements for function F():
  ! - smooth, continuous, deterministic function
  ! - root XRoot bracketed within the min and max bounds, referred to as XMin, XMax
  ! - XMin and XMax must remain constant during iterations
  ! - If F(X) is increasing, set action to iSlopeIncreasing
  !   This implies that if X1 < X2, then F(X1) < F(X2) must be satisfied.
  ! - If F(X) is decreasing, set action to iSlopeDecreasing
  !   This implies that if X1 < X2, then F(X1) > F(X2) must be satisfied.
  !
  ! Note that the residual function is allowed to be non-monotonic between the min and max
  ! points as long as the slope specification is satisfied at the min and max points.
  !
  ! Unconstrained convergence conditions:
  ! - ABS(F(XRoot)) <= ATolY
  ! - ABS(X2-X1)    <= TolX * (ABS(X2)+ABS(X1))/2 + ATolX
  !
  ! Constrained convergence conditions:
  ! For iSlopeIncreasing action:
  !   - YMin >= 0
  !   - YMax <= 0
  ! For iSlopeDecreasing action:
  !   - YMin <= 0
  !   - YMax >= 0

  ! METHODOLOGY EMPLOYED:
  ! Implements an hybrid solution method consisting of:
  ! - bisection method (aka interval halving)
  ! - false position method (aka regula falsi)
  ! - secant method
  ! - Brent's method (inverse quadratic interpolation)
  !
  ! with safeguards against:
  ! - out-of-range error
  ! - singularity (i.e., locally singular Jacobian)
  ! - bad slope
  ! - round-off error
  !
  ! Typical usage of the root finder, assuming that :
  ! - the function is called MyFunc()
  ! - the function is strictly increasing between the min and max bounds
  ! - the root is bracketed between XMin=0 and XMax=1
  ! - the function is defined with the prototype real FUNCTION MyFunc( X )
  ! - the solution method to use is the Brent's method
  ! - the absolute tolerance for Y=MyFunc(X) is ATolY=1.0E-4
  ! As a safeguard the iterative process stops after 50 iterations if the root has not been
  ! located within the specified tolerance yet.
  !
  ! <BEGIN CODE SNIPPET>
  !
  ! TYPE(RootFinderDataType)  :: RF
  ! INTEGER                   :: IterationCount
  ! REAL(r64)                 :: X, Y
  ! LOGICAL                   :: IsDoneFlag
  !
  ! CALL SetupRootFinder(   &
  !   RF,                   & ! RootFinderData
  !   iSlopeIncreasing,     & ! SlopeType
  !   iMethodBrent,         & ! MethodType
  !   1.0d-6,               & ! TolX
  !   1.0d-6,               & ! ATolX
  !   1.0d-4                & ! ATolY
  ! )
  ! CALL InitializeRootFinder(   &
  !   RF,                   & ! RootFinderData
  !   0.0d0,                & ! XMin
  !   1.0d0                 & ! XMax
  ! )
  !
  ! IterationCount = 0
  ! IsDoneFlag = .FALSE.
  ! RF%XCandidate = 0.1d0 ! Sets X to initial value XInit
  !
  ! DO WHILE ( .NOT.IsDoneFlag  )
  !   IterationCount = IterationCount+1
  !
  !   IF ( IterationCount>50 ) THEN
  !     WRITE(*,*) 'Error: Too many iterations..."
  !     EXIT
  !   END IF
  !
  !   ! Evaluate function with new root candidate
  !   X = RF%XCandidate
  !   Y = MyFunc( X )
  !
  !   ! Update root finder data with new iterate
  !   CALL IterateRootFinder( RF, X, Y, IsDoneFlag )
  ! END DO
  !
  ! ! Write root finder status description to standard output
  ! CALL WriteRootFinderStatus( 6, RF )
  ! <END CODE SNIPPET>
  !

  ! REFERENCES:
  ! "Numerical Recipes in FORTRAN", Chapter 9 "Root Finding and Nonlinear Sets of Equations", pp.340-352
  ! Used for formulas, but not for code.
  !

  ! OTHER NOTES:
  ! na

  ! USE STATEMENTS:
  ! Use statements for data only modules
  USE DataPrecisionGlobals
  USE DataRootFinder
  USE DataGlobals
  USE General,        ONLY : TrimSigDigits
  USE DataInterfaces

IMPLICIT NONE         ! Enforce explicit typing of all variables

PUBLIC

  ! MODULE PARAMETER DEFINITIONS
  ! na

  ! DERIVED TYPE DEFINITIONS
  ! na

  !MODULE VARIABLE DECLARATIONS:
  ! na

  ! SUBROUTINE SPECIFICATIONS FOR THE MODULE
  PUBLIC  SetupRootFinder
  PUBLIC  InitializeRootFinder
  PUBLIC  IterateRootFinder

  PUBLIC  WriteRootFinderTraceHeader
  PUBLIC  WriteRootFinderTrace
  PUBLIC  WriteRootFinderStatus

  PUBLIC  CheckRootFinderConvergence   ! ABS(Y) <= Controls%ATolY
  PUBLIC  CheckRootFinderCandidate     ! CheckMinMaxRange(X) .AND. CheckLowerUpperBracket(X)

  PRIVATE ResetRootFinder
  PRIVATE UpdateRootFinder
  PRIVATE AdvanceRootFinder

  PRIVATE CheckMinMaxRange             ! MinPoint%X <= X <= MaxPoint%X
  PRIVATE CheckLowerUpperBracket       ! LowerPoint%X < X < UpperPoint%X
  PRIVATE CheckInternalConsistency     ! For debugging only
  PRIVATE CheckSlope                   ! For an increasing function, MinPoint%Y < MaxPoint%Y;
                                       ! For a decreasing function,  MinPoint%Y > MaxPoint%Y
  PRIVATE CheckNonSingularity          ! MinPoint%Y == MaxPoint%Y
  PRIVATE CheckMinConstraint
  PRIVATE CheckMaxConstraint
  PRIVATE CheckIncrementRoundOff
  PRIVATE CheckBracketRoundOff

  PRIVATE UpdateMinMax
  PRIVATE UpdateBracket
  PRIVATE UpdateHistory
  PRIVATE SortHistory

  PRIVATE WritePoint
  PUBLIC  DebugRootFinder

  PRIVATE BracketRoot
  PRIVATE BisectionMethod
  PRIVATE FalsePositionMethod
  PRIVATE SecantMethod
  PRIVATE SecantFormula
  PRIVATE BrentMethod

CONTAINS

! MODULE SUBROUTINES:
!*************************************************************************

SUBROUTINE SetupRootFinder( RootFinderData, SlopeType, MethodType, TolX, ATolX, ATolY )

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Dimitri Curtil (LBNL)
          !       DATE WRITTEN   February 2006
          !       MODIFIED
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine loads the numerical controls for the root finder.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  TYPE(RootFinderDataType), INTENT(INOUT) :: RootFinderData     ! Data used by root finding algorithm
  INTEGER, INTENT(IN)                     :: SlopeType          ! Either iSlopeIncreasing or iSlopeDecreasing
  INTEGER, INTENT(IN)                     :: MethodType         ! Any of the iMethod<name> code but iMethodNone
  REAL(r64), INTENT(IN)                        :: TolX               ! Relative tolerance for X variables
  REAL(r64), INTENT(IN)                        :: ATolX              ! Absolute tolerance for X variables
  REAL(r64), INTENT(IN)                        :: ATolY              ! Absolute tolerance for Y variables

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          ! na

          ! FLOW:

  ! Load assumed action for underlying function F(X)
  IF ( SlopeType /= iSlopeIncreasing .AND. SlopeType /= iSlopeDecreasing ) THEN
    CALL ShowSevereError('SetupRootFinder: Invalid function slope specification. Valid choices are:' )
    CALL ShowContinueError('SetupRootFinder: iSlopeIncreasing='//TRIM(TrimSigDigits(iSlopeIncreasing)))
    CALL ShowContinueError('SetupRootFinder: iSlopeDecreasing='//TRIM(TrimSigDigits(iSlopeDecreasing)))
    CALL ShowFatalError('SetupRootFinder: Preceding error causes program termination.')
  END IF
  RootFinderData%Controls%SlopeType = SlopeType

  ! Load solution method
  IF ( MethodType /= iMethodBisection .AND.     &
       MethodType /= iMethodFalsePosition .AND. &
       MethodType /= iMethodSecant .AND.        &
       MethodType /= iMethodBrent ) THEN

    CALL ShowSevereError('SetupRootFinder: Invalid solution method specification. Valid choices are:' )
    CALL ShowContinueError('SetupRootFinder: iMethodBisection='//TRIM(TrimSigDigits(iMethodBisection)))
    CALL ShowContinueError('SetupRootFinder: iMethodFalsePosition='//TRIM(TrimSigDigits(iMethodFalsePosition)))
    CALL ShowContinueError('SetupRootFinder: iMethodSecant='//TRIM(TrimSigDigits(iMethodSecant)))
    CALL ShowContinueError('SetupRootFinder: iMethodBrent='//TRIM(TrimSigDigits(iMethodBrent)))
    CALL ShowFatalError('SetupRootFinder: Preceding error causes program termination.')

  END IF
  RootFinderData%Controls%MethodType = MethodType

  ! Load relative tolerance parameter for X variables
  IF ( TolX < 0.0d0 ) THEN
    CALL ShowFatalError('SetupRootFinder: Invalid tolerance specification for X variables. TolX >= 0')
  END IF
  RootFinderData%Controls%TolX = TolX

  ! Load absolute tolerance parameter for X variables
  IF ( ATolX < 0.0d0 ) THEN
    CALL ShowFatalError('SetupRootFinder: Invalid absolute tolerance specification for X variables. ATolX >= 0')
  END IF
  RootFinderData%Controls%ATolX = ATolX

  ! Load absolute tolerance parameter for Y variables
  IF ( ATolY < 0.0d0 ) THEN
    CALL ShowFatalError('SetupRootFinder: Invalid absolute tolerance specification for Y variables. ATolY >= 0')
  END IF
  RootFinderData%Controls%ATolY = ATolY

  ! Reset internal data for root finder with fictive min and max values
  CALL ResetRootFinder( RootFinderData, constant_zero, constant_zero )

  RETURN

END SUBROUTINE SetupRootFinder


SUBROUTINE ResetRootFinder( RootFinderData, XMin, XMax )

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Dimitri Curtil (LBNL)
          !       DATE WRITTEN   February 2006
          !       MODIFIED
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine initializes the data for the root finder.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  TYPE(RootFinderDataType), INTENT(INOUT) :: RootFinderData ! Data used by root finding algorithm
  REAL(r64), INTENT(IN)                        :: XMin           ! Minimum X value allowed
  REAL(r64), INTENT(IN)                        :: XMax           ! Maximum X value allowed

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          ! na

          ! FLOW:

  ! Reset min point
  RootFinderData%MinPoint%X           = XMin
  RootFinderData%MinPoint%Y           = 0.0d0
  RootFinderData%MinPoint%DefinedFlag = .FALSE.

  ! Reset max point
  RootFinderData%MaxPoint%X           = XMax
  RootFinderData%MaxPoint%Y           = 0.0d0
  RootFinderData%MaxPoint%DefinedFlag = .FALSE.

  ! Reset lower point
  RootFinderData%LowerPoint%X           = 0.0d0
  RootFinderData%LowerPoint%Y           = 0.0d0
  RootFinderData%LowerPoint%DefinedFlag = .FALSE.

  ! Reset upper point
  RootFinderData%UpperPoint%X           = 0.0d0
  RootFinderData%UpperPoint%Y           = 0.0d0
  RootFinderData%UpperPoint%DefinedFlag = .FALSE.

  ! Reset previous point
  RootFinderData%CurrentPoint%X           = 0.0d0
  RootFinderData%CurrentPoint%Y           = 0.0d0
  RootFinderData%CurrentPoint%DefinedFlag = .FALSE.

  ! Reset iterate history with last 3 best points
  RootFinderData%NumHistory             = 0
  RootFinderData%History(:)%X           = 0.0d0
  RootFinderData%History(:)%Y           = 0.0d0
  RootFinderData%History(:)%DefinedFlag = .FALSE.

  ! Reset increments over successive iterationes
  RootFinderData%Increment%X            = 0.0d0
  RootFinderData%Increment%Y            = 0.0d0
  RootFinderData%Increment%DefinedFlag  = .FALSE.

  RootFinderData%XCandidate = 0.0d0

  ! Reset default state
  RootFinderData%StatusFlag = iStatusNone
  RootFinderData%CurrentMethodType = iMethodNone
  RootFinderData%ConvergenceRate = -1.0d0

  RETURN

END SUBROUTINE ResetRootFinder


SUBROUTINE InitializeRootFinder( RootFinderData, XMin, XMax )

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Dimitri Curtil (LBNL)
          !       DATE WRITTEN   February 2006
          !       MODIFIED
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine initializes the min and max for the root finder before
          ! finding a new root.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  TYPE(RootFinderDataType), INTENT(INOUT) :: RootFinderData ! Data used by root finding algorithm
  REAL(r64), INTENT(IN)                        :: XMin           ! Minimum X value allowed
  REAL(r64), INTENT(IN)                        :: XMax           ! Maximum X value allowed

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64)                               :: SavedXCandidate
  REAL(r64)                               :: XMinReset
          ! FLOW:
  XMinReset = XMin
  IF ( XMin > XMax) THEN
    IF (XMax == 0.0d0) THEN
      XMinReset = XMax
    ELSE
      CALL ShowFatalError( &
        'InitializeRootFinder: Invalid min/max bounds '// &
        'XMin='// TRIM(TrimSigDigits(XMin,6))//' must be smaller than '// &
        'XMax='// TRIM(TrimSigDigits(XMax,6)) &
      )
    END IF
  END IF

  ! First save current candidate value before it is overriden in ResetRootFinder()
  SavedXCandidate = RootFinderData%XCandidate

  ! Reset internal data for root finder with actual min and max values
  !
  ! NOTE: This resets the value of RootFinderData%XCandidate to zero
  CALL ResetRootFinder( RootFinderData, XMinReset, XMax )

  ! Enforce min/max constraints on previous candidate if available
  !
  ! NOTE: If XMin == XMax then this forces the value of XCandidateto the desired solution
  RootFinderData%XCandidate = MIN( RootFinderData%MaxPoint%X, &
                                    MAX( SavedXCandidate, &
                                         RootFinderData%MinPoint%X ) )

  RETURN

END SUBROUTINE InitializeRootFinder


SUBROUTINE IterateRootFinder( RootFinderData, X, Y, IsDoneFlag )

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Dimitri Curtil (LBNL)
          !       DATE WRITTEN   March 2006
          !       MODIFIED
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is the workhorse of the root finder framework.
          ! It should be invoked with every new candidate (X,Y) until
          ! convergence is achieved or abnormal termination is detected.
          !
          ! The subroutine performs the following tasks:
          ! - it checks for convergence
          ! - it updates the internal data with the current iterate (X,Y)
          ! - it computes a new root candidate if not converged yet.
          !
          ! Sets IsDoneFlag to FALSE when iteration should continue with the new candidate value.
          ! Sets IsDoneFlag to TRUE  when iteration should be stopped because convergence has been achieved
          ! or because of a fatal error.
          !
          ! Note that only upon normal termination (iStatusOK<...> codes)
          ! will the XCandidate value contain the root.
          ! If the root has not been located yet the XCandidate value contains
          ! the next candidate root to try.
          !
          ! Status                          IsDoneFlag          XCandidate
          ! ========================================================================
          ! iStatusErrorRange               TRUE                na
          ! iStatusErrorSingular            TRUE                na
          ! iStatusErrorSlope               TRUE                na
          ! iStatusErrorBracket             TRUE                na
          ! ------------------------------------------------------------------------
          ! iStatusOKMin                    TRUE                MinPoint%X
          ! iStatusOKMax                    TRUE                MaxPoint%X
          ! iStatusOK                       TRUE                X
          ! iStatusOKRoundOff               TRUE                X
          ! ------------------------------------------------------------------------
          ! iStatusNone                     FALSE               AdvanceRootFinder()
          ! iStatusWarningNonMonotonic      FALSE               AdvanceRootFinder()
          ! iStatusWarningSingular          FALSE               AdvanceRootFinder()
          !

          ! METHODOLOGY EMPLOYED:
          ! The methodology reflects the same approach implemented in the subroutine CalcSimpleController()
          ! whereby the iteration was exited by checking the following conditions in this specified
          ! sequence:
          !   1. Check for singular function so that YMin /= YMax
          !   2. Check for slope condition for the residual function
          !      - increasing: YMin < YMax
          !      - decreasing: YMin > YMax
          !   3. Check for min constraint
          !      - increasing: YMin <= 0
          !      - decreasing: YMin >= 0
          !   4. Check for max constraint
          !      - increasing: YMax > 0
          !      - decreasing: YMax < 0
          !   5. Check unconstrained convergence
          !
          ! Note that the slope condition was not explicitly checked in the original implementation
          ! in CalcSimpleController().
          !
          ! Finally, we also check the X increments between successive iterations to detect possible
          ! cases whereby the allowed precision in the X space limits the precision attainable in
          ! the Y space. This check is implemented in:
          ! - CheckIncrementRoundOff()
          ! - CheckBracketRoundOff()
          !

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  TYPE(RootFinderDataType), INTENT(INOUT) :: RootFinderData ! Data used by root finding algorithm
  REAL(r64), INTENT(IN)                        :: X              ! X value of current iterate
  REAL(r64), INTENT(IN)                        :: Y              ! Y value of current iterate
  LOGICAL, INTENT(OUT), OPTIONAL          :: IsDoneFlag     ! If TRUE indicates that the iteration should be stopped

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          ! na

          ! FLOW:

  ! Reset status flag
  RootFinderData%StatusFlag = iStatusNone

  ! Check that MinPoint%X <= X <= MaxPoint%X
  IF ( .NOT.CheckMinMaxRange( RootFinderData, X ) ) THEN
    RootFinderData%StatusFlag = iStatusErrorRange

    ! Fatal error: No need to continue iterating
    IsDoneFlag = .TRUE.
    RETURN
  END IF

  ! Update min/max support points with current iterate
  CALL UpdateMinMax( RootFinderData, X, Y )

  !----------------------------------------------------------------------------
  ! Check "global" singularity and bad slope conditions between min and
  ! max points
  !----------------------------------------------------------------------------

  ! NOTE: Performed before checking min and max constraints to mimic original implementation
  !       in ManagerControllers()
  IF ( RootFinderData%MinPoint%DefinedFlag .AND. RootFinderData%MaxPoint%DefinedFlag ) THEN

    ! Check that min and max points are distinct
    IF (RootFinderData%MinPoint%X == RootFinderData%MaxPoint%X) THEN
      RootFinderData%StatusFlag = iStatusOKMin
      RootFinderData%XCandidate = RootFinderData%MinPoint%X

      ! Solution found: No need to continue iterating
      IsDoneFlag = .TRUE.
      RETURN
    END IF

    IF ( RootFinderData%MinPoint%DefinedFlag ) THEN
      IF ( CheckMinConstraint( RootFinderData ) ) THEN
        RootFinderData%StatusFlag = iStatusOKMin
        RootFinderData%XCandidate = RootFinderData%MinPoint%X

        ! Solution found: No need to continue iterating
        IsDoneFlag = .TRUE.
        RETURN
      END IF
    END IF

    ! Check singularity condition between min and max points
    IF ( .NOT.CheckNonSingularity(RootFinderData) ) THEN
      RootFinderData%StatusFlag = iStatusErrorSingular

      ! Fatal error: No need to continue iterating
      IsDoneFlag = .TRUE.
      RETURN
    END IF

    ! Check slope condition between min and max points
    IF ( .NOT.CheckSlope(RootFinderData) ) THEN
      RootFinderData%StatusFlag = iStatusErrorSlope

      ! Fatal error: No need to continue iterating
      IsDoneFlag = .TRUE.
      RETURN
    END IF
  END IF

  !----------------------------------------------------------------------------
  ! Check that F(X) is not min or max constrained
  !----------------------------------------------------------------------------

  ! Check min constraint before max constraint to mimic original implementation
  ! in ManagerControllers()
  IF ( RootFinderData%MinPoint%DefinedFlag ) THEN
    IF ( CheckMinConstraint( RootFinderData ) ) THEN
      RootFinderData%StatusFlag = iStatusOKMin
      RootFinderData%XCandidate = RootFinderData%MinPoint%X

      ! Solution found: No need to continue iterating
      IsDoneFlag = .TRUE.
      RETURN
    END IF
  END IF

  ! Min point should always be evaluated first to ensure that we return with the min
  ! consrained solution even in cases where the residual function has inconsistent slope.
  !
  ! TODO: Force to evaluate min point before exiting with max constrained solution
  !       in order to be able to detect singularity and bad slope conditions.
  IF ( RootFinderData%MaxPoint%DefinedFlag ) THEN
    IF ( CheckMaxConstraint( RootFinderData ) ) THEN

      RootFinderData%StatusFlag = iStatusOKMax
      RootFinderData%XCandidate = RootFinderData%MaxPoint%X

      ! Solution found: No need to continue iterating
      IsDoneFlag = .TRUE.
      RETURN
    END IF
  END IF

  !----------------------------------------------------------------------------
  ! Check convergence of current iterate
  !----------------------------------------------------------------------------

  ! Check unconstrained convergence after we are sure that the candidate X value lies
  ! within the allowed min/max range
  IF ( CheckRootFinderConvergence( RootFinderData, Y ) ) THEN
    RootFinderData%StatusFlag = iStatusOK
    RootFinderData%XCandidate = X

    ! Update root finder internal data with current iterate (X,Y)
    CALL UpdateRootFinder( RootFinderData, X, Y )

    ! Solution found: No need to continue iterating
    IsDoneFlag = .TRUE.
    RETURN
  END IF

  ! Check last as this was not done in the original implementation
  !
  ! Essentially we stop the iteration if:
  ! - the increment beween successive iterates is smaller than the user-specified
  !   tolerance for the X variables.
  ! - the distance between the lower and upper bounds is smaller than the user-specified
  !   tolerance for the X variables. (USING brackets from previous iteration)
  !
  ! BUG: Relaxed check to avoid detecting round-off in case 2 successive iterates are the same!
  !
  !IF ( CheckIncrementRoundOff( RootFinderData, X ) ) THEN
  !  RootFinderData%StatusFlag = iStatusOKRoundOff
  !  RETURN
  !END IF
  IF ( CheckBracketRoundOff( RootFinderData ) ) THEN
    RootFinderData%StatusFlag = iStatusOKRoundOff
    RootFinderData%XCandidate = X

    ! Update root finder internal data with current iterate (X,Y)
    CALL UpdateRootFinder( RootFinderData, X, Y )

    ! Solution found: No need to continue iterating
    IsDoneFlag = .TRUE.
    RETURN
  END IF

  !----------------------------------------------------------------------------
  ! If the current iterate lies within the current lower and upper brackets,
  ! proceed with normal processing to identify the next root candidate:
  ! - update lower/upper bracket with current iterate
  ! - update history
  ! - update increments across successive iterations
  ! - update current point
  ! - compute next candidate (see AdvanceRootFinder() ).
  !----------------------------------------------------------------------------

  ! Check that current iterate is within the current lower and upper points
  IF ( .NOT.CheckLowerUpperBracket( RootFinderData, X ) ) THEN
    RootFinderData%StatusFlag = iStatusErrorBracket

    ! Fatal error: No need to continue iterating
    IsDoneFlag = .TRUE.
    RETURN
  END IF

  ! Update root finder internal data with current iterate (X,Y)
  CALL UpdateRootFinder( RootFinderData, X, Y )

  ! Compute new root candidate and store value in in RootFinderData%XCandidate
  ! - First attempt to bracket root within lower and upper points
  ! - Then use whatever requested solution method in SetupRootFinder() to
  !   compute the next candidate.
  CALL AdvanceRootFinder( RootFinderData )

  ! Indicates that we should continue iterating with new candidate
  IsDoneFlag = .FALSE.

  RETURN
END SUBROUTINE IterateRootFinder


INTEGER FUNCTION CheckInternalConsistency( RootFinderData )
          ! FUNCTION INFORMATION:
          !       AUTHOR         Dimitri Curtil (LBNL)
          !       DATE WRITTEN   March 2006
          !       MODIFIED
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function checks whether the lower and upper points (if defined)
          ! determine a consistent interval bracketting the root.
          !
          ! Returns the status code accordingly.
          !
          ! This function does not modify the argument RooFinderData.
          !
          ! Only used internally for debugging.
          !

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  TYPE(RootFinderDataType), INTENT(IN)    :: RootFinderData ! Data used by root finding algorithm

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
          ! na

          ! FLOW:

  ! Default initialization
  CheckInternalConsistency = iStatusNone

  ! Internal consistency check involving both support points
  IF ( RootFinderData%LowerPoint%DefinedFlag .AND. RootFinderData%UpperPoint%DefinedFlag ) THEN

    ! Check that the existing lower and upper points do bracket the root
    IF ( RootFinderData%LowerPoint%X > RootFinderData%UpperPoint%X ) THEN
      CheckInternalConsistency = iStatusErrorRange
      RETURN
    END IF

    ! Check for non-monotonicity between the existing lower and upper points
    SELECT CASE ( RootFinderData%Controls%SlopeType )
    CASE (iSlopeIncreasing)
      ! Y-value of lower point must be strictly smaller than Y-value of upper point
      IF ( RootFinderData%LowerPoint%Y > RootFinderData%UpperPoint%Y ) THEN
        CheckInternalConsistency = iStatusWarningNonMonotonic
        RETURN
      END IF

    CASE (iSlopeDecreasing)
      ! Y-value of lower point must be strictly larger than Y-value of upper point
      IF ( RootFinderData%LowerPoint%Y < RootFinderData%UpperPoint%Y ) THEN
        CheckInternalConsistency = iStatusWarningNonMonotonic
        RETURN
      END IF

    CASE DEFAULT
      ! Should never happen
      CALL ShowSevereError('CheckInternalConsistency: Invalid function slope specification. Valid choices are:')
      CALL ShowContinueError('CheckInternalConsistency: iSlopeIncreasing='//TRIM(TrimSigDigits(iSlopeIncreasing)))
      CALL ShowContinueError('CheckInternalConsistency: iSlopeDecreasing='//TRIM(TrimSigDigits(iSlopeDecreasing)))
      CALL ShowFatalError('CheckInternalConsistency: Preceding error causes program termination.')
    END SELECT

    ! Check for in singularity with respect to the existing lower and upper points
    ! Only check if the lower and upper points are distinct!
    IF ( RootFinderData%UpperPoint%X > RootFinderData%LowerPoint%X ) THEN
      IF ( RootFinderData%UpperPoint%Y == RootFinderData%LowerPoint%Y ) THEN
        CheckInternalConsistency = iStatusErrorSingular
        RETURN
      END IF
    END IF

  END IF


  ! Check min constraint for min point if already defined
  IF ( RootFinderData%MinPoint%DefinedFlag ) THEN
    SELECT CASE ( RootFinderData%Controls%SlopeType )
    CASE ( iSlopeIncreasing )
      IF ( RootFinderData%MinPoint%Y >= 0.0d0 ) THEN
        CheckInternalConsistency = iStatusOKMin
        RETURN
      END IF

    CASE ( iSlopeDecreasing )
      IF ( RootFinderData%MinPoint%Y <= 0.0d0 ) THEN
        CheckInternalConsistency = iStatusOKMin
        RETURN
      END IF

    CASE DEFAULT
      ! Should never happen
      CALL ShowSevereError('CheckInternalConsistency: Invalid function slope specification. Valid choices are:')
      CALL ShowContinueError('CheckInternalConsistency: iSlopeIncreasing='//TRIM(TrimSigDigits(iSlopeIncreasing)))
      CALL ShowContinueError('CheckInternalConsistency: iSlopeDecreasing='//TRIM(TrimSigDigits(iSlopeDecreasing)))
      CALL ShowFatalError('CheckInternalConsistency: Preceding error causes program termination.')
    END SELECT
  END IF


  ! Check max constraint for max point if already defined
  IF ( RootFinderData%MaxPoint%DefinedFlag ) THEN
    SELECT CASE ( RootFinderData%Controls%SlopeType )
    CASE ( iSlopeIncreasing )
      IF ( RootFinderData%MaxPoint%Y <= 0.0d0 ) THEN
        CheckInternalConsistency = iStatusOKMax
        RETURN
      END IF

    CASE ( iSlopeDecreasing )
      IF ( RootFinderData%MaxPoint%Y >= 0.0d0 ) THEN
        CheckInternalConsistency = iStatusOKMax
        RETURN
      END IF

    CASE DEFAULT
      ! Should never happen
      CALL ShowSevereError('CheckInternalConsistency: Invalid function slope specification. Valid choices are:')
      CALL ShowContinueError('CheckInternalConsistency: iSlopeIncreasing='//TRIM(TrimSigDigits(iSlopeIncreasing)))
      CALL ShowContinueError('CheckInternalConsistency: iSlopeDecreasing='//TRIM(TrimSigDigits(iSlopeDecreasing)))
      CALL ShowFatalError('CheckInternalConsistency: Preceding error causes program termination.')
    END SELECT
  END IF

  RETURN
END FUNCTION CheckInternalConsistency


LOGICAL FUNCTION CheckRootFinderCandidate( RootFinderData, X )
          ! FUNCTION INFORMATION:
          !       AUTHOR         Dimitri Curtil (LBNL)
          !       DATE WRITTEN   February 2006
          !       MODIFIED
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function checks whether the root candidate X lies within the specified
          ! min/max limits as well as within the current lower and upper brackets (if defined).
          !
          ! Returns TRUE if X value is a valid root candidate.
          ! Returns FALSE otherwise.
          !

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  TYPE(RootFinderDataType), INTENT(IN)    :: RootFinderData ! Data used by root finding algorithm
  REAL(r64), INTENT(IN)                        :: X              ! X value for current iterate

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
          ! na

          ! FLOW:

  IF ( CheckMinMaxRange( RootFinderData, X ) .AND. CheckLowerUpperBracket( RootFinderData, X ) ) THEN
    CheckRootFinderCandidate = .TRUE.
  ELSE
    CheckRootFinderCandidate = .FALSE.
  END IF

  RETURN
END FUNCTION CheckRootFinderCandidate


LOGICAL FUNCTION CheckMinMaxRange( RootFinderData, X )
          ! FUNCTION INFORMATION:
          !       AUTHOR         Dimitri Curtil (LBNL)
          !       DATE WRITTEN   February 2006
          !       MODIFIED       Brent Griffith (NREL) added DefinedFlag traps
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function checks whether the current iterate X lies within the specified min/max limits
          ! or not.
          !
          ! Returns TRUE if current iterate satisfies min/max constraints.
          ! Returns FALSE if current iterate is out-of-range.
          !

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  TYPE(RootFinderDataType), INTENT(IN)    :: RootFinderData ! Data used by root finding algorithm
  REAL(r64), INTENT(IN)                        :: X              ! X value for current iterate

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
          ! na

          ! FLOW:
  IF (RootFinderData%MinPoint%DefinedFlag) THEN  !DSU3 testing
    IF ( X < RootFinderData%MinPoint%X ) THEN
      CheckMinMaxRange = .FALSE.
      RETURN
    END IF
  ENDIF

  IF (RootFinderData%MaxPoint%DefinedFlag) THEN  !DSU3 testing
    IF ( X > RootFinderData%MaxPoint%X ) THEN
      CheckMinMaxRange = .FALSE.
      RETURN
    END IF
  ENDIF

  CheckMinMaxRange = .TRUE.

  RETURN
END FUNCTION CheckMinMaxRange


LOGICAL FUNCTION CheckLowerUpperBracket( RootFinderData, X )
          ! FUNCTION INFORMATION:
          !       AUTHOR         Dimitri Curtil (LBNL)
          !       DATE WRITTEN   February 2006
          !       MODIFIED       Brent Griffith, March 2010, changed to LowerPoint%X <= X <= UpperPoint%X
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function checks whether the current iterate X lies within the current lower
          ! and upper points or not (if defined):
          !   LowerPoint%X < X < UpperPoint%X
          !
          ! Returns TRUE if current iterate lies within the lower/upper bracket.
          ! Returns FALSE otherwise.
          !

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  TYPE(RootFinderDataType), INTENT(IN)    :: RootFinderData ! Data used by root finding algorithm
  REAL(r64), INTENT(IN)                        :: X              ! X value for current iterate

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
          ! na

          ! FLOW:


  IF ( RootFinderData%LowerPoint%DefinedFlag ) THEN
    IF ( X < RootFinderData%LowerPoint%X ) THEN  !DSU3 test with < instead of <=
      CheckLowerUpperBracket = .FALSE.
      RETURN
    END IF
  END IF

  IF ( RootFinderData%UpperPoint%DefinedFlag ) THEN
    IF ( X > RootFinderData%UpperPoint%X ) THEN !DSU3 test with > instead of >=
      CheckLowerUpperBracket = .FALSE.
      RETURN
    END IF
  END IF

  CheckLowerUpperBracket = .TRUE.

  RETURN
END FUNCTION CheckLowerUpperBracket


LOGICAL FUNCTION CheckSlope( RootFinderData )
          ! FUNCTION INFORMATION:
          !       AUTHOR         Dimitri Curtil (LBNL)
          !       DATE WRITTEN   February 2006
          !       MODIFIED
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function checks whether the current iterate (X,Y) satisfies the slope
          ! requirement between the min and max points.
          !
          ! Returns FALSE if the slope requirement is NOT satisfied.
          ! Returns TRUE if the slope requirement is satisfied.
          !
          ! PRECONDITION:
          ! - Function assumes that both the min and max points are defined.
          !
          ! POSTCONDITION:
          ! - RootFinderData is NOT changed by this function.
          !

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  TYPE(RootFinderDataType), INTENT(IN)    :: RootFinderData ! Data used by root finding algorithm

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
          ! na

          ! FLOW:

  ! Check that the slope requirement is respected at the min and max points
  !
  ! Note that the singularity check takes care of RootFinderData%MinPoint%Y == RootFinderData%MaxPoint%Y
  ! therefore we use strict comparison operators < and >.
  SelectSlope: SELECT CASE ( RootFinderData%Controls%SlopeType )
  CASE ( iSlopeIncreasing )
    IF ( RootFinderData%MinPoint%Y < RootFinderData%MaxPoint%Y ) THEN
      CheckSlope = .TRUE.
      RETURN
    END IF

  CASE ( iSlopeDecreasing )
    IF ( RootFinderData%MinPoint%Y > RootFinderData%MaxPoint%Y ) THEN
      CheckSlope = .TRUE.
      RETURN
    END IF

  CASE DEFAULT
    ! Should never happen
    CALL ShowSevereError('CheckSlope: Invalid function slope specification. Valid choices are:')
    CALL ShowContinueError('CheckSlope: iSlopeIncreasing='//TRIM(TrimSigDigits(iSlopeIncreasing)))
    CALL ShowContinueError('CheckSlope: iSlopeDecreasing='//TRIM(TrimSigDigits(iSlopeDecreasing)))
    CALL ShowFatalError('CheckSlope: Preceding error causes program termination.')

  END SELECT SelectSlope

  CheckSlope = .FALSE.

  RETURN
END FUNCTION CheckSlope


LOGICAL FUNCTION CheckNonSingularity( RootFinderData )
          ! FUNCTION INFORMATION:
          !       AUTHOR         Dimitri Curtil (LBNL)
          !       DATE WRITTEN   February 2006
          !       MODIFIED
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function checks whether the min and max points define a locally, singular
          ! equation system. In a 1-dimensional system, "singularity" is detected if the
          ! min and max points have the same y-values, thereby producing a zero slope
          ! across the min/max range.
          !
          ! Returns TRUE if the function satisfies the non-singularity condition.
          ! Returns FALSE otherwise (i.e., F(X) essentially displays a zero slope
          ! between the min and max points) .
          !
          ! PRECONDITION:
          ! - Function assumes that both the min and max points are defined.
          !
          ! POSTCONDITION:
          ! - RootFinderData is NOT changed by this function.
          !

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  TYPE(RootFinderDataType), INTENT(IN)    :: RootFinderData ! Data used by root finding algorithm

          ! FUNCTION PARAMETER DEFINITIONS:
  ! Safety factor used to detect a singular residual function between the min and max
  ! points.
  !
  ! NOTE: Requesting exactly the same value is obtained by setting SafetyFactor = 0.0
  REAL(r64), PARAMETER                         :: SafetyFactor = 0.1d0

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  REAL(r64)   :: DeltaY     ! Difference between min and max Y-values
  REAL(r64)   :: ATolY      ! Absolute tolerance used to detected equal min and max Y-values

          ! FLOW:

  ! Added this check based on an absolute tolerance test for y values to avoid incorrectly detecting
  ! functions with bad slope due to numerical noise.
  !
  ! Typically, this takes care of situations where the controlled equipment in ManageControllers()
  ! would be misdiagnosed as displaying the "wrong slope" instead of being treated as "singular"
  ! (i.e. in inactive mode).
  DeltaY = ABS(RootFinderData%MinPoint%Y - RootFinderData%MaxPoint%Y)
  ATolY  = SafetyFactor * RootFinderData%Controls%ATolY

  IF ( ABS(DeltaY) <= ATolY ) THEN
    CheckNonSingularity = .FALSE.
  ELSE
    CheckNonSingularity = .TRUE.
  END IF

  RETURN
END FUNCTION CheckNonSingularity


LOGICAL FUNCTION CheckMinConstraint( RootFinderData )
          ! FUNCTION INFORMATION:
          !       AUTHOR         Dimitri Curtil (LBNL)
          !       DATE WRITTEN   February 2006
          !       MODIFIED
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function checks whether the min point satisfies the min constraint
          ! condition or not.
          !
          ! PRECONDITION:
          ! - Function assumes that the min point is defined.
          !
          ! POSTCONDITION:
          ! - RootFinderData is NOT changed by this function.
          !

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  TYPE(RootFinderDataType), INTENT(IN)    :: RootFinderData ! Data used by root finding algorithm

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
          ! na

          ! FLOW:

  SelectSlope: SELECT CASE ( RootFinderData%Controls%SlopeType )
  CASE ( iSlopeIncreasing )
    IF ( RootFinderData%MinPoint%Y >= 0.0d0 ) THEN
      CheckMinConstraint = .TRUE.
      RETURN
    END IF

  CASE ( iSlopeDecreasing )
    IF ( RootFinderData%MinPoint%Y <= 0.0d0 ) THEN
      CheckMinConstraint = .TRUE.
      RETURN
    END IF

  CASE DEFAULT
    ! Should never happen
    CALL ShowSevereError('CheckMinConstraint: Invalid function slope specification. Valid choices are:')
    CALL ShowContinueError('CheckMinConstraint: iSlopeIncreasing='//TRIM(TrimSigDigits(iSlopeIncreasing)))
    CALL ShowContinueError('CheckMinConstraint: iSlopeDecreasing='//TRIM(TrimSigDigits(iSlopeDecreasing)))
    CALL ShowFatalError('CheckMinConstraint: Preceding error causes program termination.')
  END SELECT SelectSlope

  CheckMinConstraint = .FALSE.

  RETURN
END FUNCTION CheckMinConstraint


LOGICAL FUNCTION CheckMaxConstraint( RootFinderData )
          ! FUNCTION INFORMATION:
          !       AUTHOR         Dimitri Curtil (LBNL)
          !       DATE WRITTEN   February 2006
          !       MODIFIED
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function checks whether the max point satisfies the max constraint
          ! condition or not.
          !
          ! PRECONDITION:
          ! - Function assumes that the max point is defined.
          !
          ! POSTCONDITION:
          ! - RootFinderData is NOT changed by this function.
          !

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  TYPE(RootFinderDataType), INTENT(IN)    :: RootFinderData ! Data used by root finding algorithm

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
          ! na

          ! FLOW:

  ! Check for max constrained convergence with respect to the new iterate (X,Y)
  SelectSlope: SELECT CASE ( RootFinderData%Controls%SlopeType )
  CASE ( iSlopeIncreasing )
    IF ( RootFinderData%MaxPoint%Y <= 0.0d0 ) THEN
      CheckMaxConstraint = .TRUE.
      RETURN
    END IF

  CASE ( iSlopeDecreasing )
    IF ( RootFinderData%MaxPoint%Y >= 0.0d0 ) THEN
      CheckMaxConstraint = .TRUE.
      RETURN
    END IF

  CASE DEFAULT
    ! Should never happen
    CALL ShowSevereError('CheckMaxConstraint: Invalid function slope specification. Valid choices are:')
    CALL ShowContinueError('CheckMaxConstraint: iSlopeIncreasing='//TRIM(TrimSigDigits(iSlopeIncreasing)))
    CALL ShowContinueError('CheckMaxConstraint: iSlopeDecreasing='//TRIM(TrimSigDigits(iSlopeDecreasing)))
    CALL ShowFatalError('CheckMaxConstraint: Preceding error causes program termination.')
  END SELECT SelectSlope

  CheckMaxConstraint = .FALSE.

  RETURN
END FUNCTION CheckMaxConstraint


LOGICAL FUNCTION CheckRootFinderConvergence( RootFinderData, Y )
          ! FUNCTION INFORMATION:
          !       AUTHOR         Dimitri Curtil (LBNL)
          !       DATE WRITTEN   February 2006
          !       MODIFIED
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function checks whether the current iterate (X,Y) satisfies the
          ! unconstrained convergence criterion or not.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  TYPE(RootFinderDataType), INTENT(IN)    :: RootFinderData ! Data used by root finding algorithm
  REAL(r64), INTENT(IN)                        :: Y              ! Y value for current iterate

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
          ! na

          ! FLOW:

  ! Check for unconstrained convergence
  IF ( ABS(Y) <= RootFinderData%Controls%ATolY ) THEN
      CheckRootFinderConvergence = .TRUE.
      RETURN
  END IF

  CheckRootFinderConvergence = .FALSE.

  RETURN
END FUNCTION CheckRootFinderConvergence


LOGICAL FUNCTION CheckIncrementRoundOff( RootFinderData, X )
          ! FUNCTION INFORMATION:
          !       AUTHOR         Dimitri Curtil (LBNL)
          !       DATE WRITTEN   February 2006
          !       MODIFIED
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function checks whether the current iterate X satisfies the
          ! round-off criterion or not.
          !

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  TYPE(RootFinderDataType), INTENT(IN)    :: RootFinderData ! Data used by root finding algorithm
  REAL(r64), INTENT(IN)                   :: X              ! X value for current iterate

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  REAL(r64)       :: DeltaX             ! Increment in X since last iterate
  REAL(r64)       :: TolX               ! Tolerance to satisfy for X increment
          ! FLOW:

  CheckIncrementRoundOff = .FALSE.
  ! Check for round-off error in X increments since last iterate
  IF ( RootFinderData%CurrentPoint%DefinedFlag ) THEN
    ! TODO: Use typical value for X averaged over successive iterations
    TolX   = RootFinderData%Controls%TolX * ABS(X) + RootFinderData%Controls%ATolX
    DeltaX = X - RootFinderData%CurrentPoint%Y

    IF ( ABS(DeltaX) <= ABS(TolX) ) THEN
        CheckIncrementRoundOff = .TRUE.
        RETURN
    END IF
  END IF

  RETURN
END FUNCTION CheckIncrementRoundOff


LOGICAL FUNCTION CheckBracketRoundOff( RootFinderData )
          ! FUNCTION INFORMATION:
          !       AUTHOR         Dimitri Curtil (LBNL)
          !       DATE WRITTEN   February 2006
          !       MODIFIED
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function checks whether the current lower and upper brackets satisfies
          ! the round-off criterion or not.
          !

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  TYPE(RootFinderDataType), INTENT(IN)    :: RootFinderData ! Data used by root finding algorithm

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  REAL(r64)       :: DeltaUL            ! Distance between lower and upper points
  REAL(r64)       :: TypUL              ! Typical value for values lying within lower/upper interval
  REAL(r64)       :: TolUL              ! Tolerance to satisfy for lower-upper distance
          ! FLOW:

  ! Check for round-off error in Lower/Upper interval
  IF ( RootFinderData%LowerPoint%DefinedFlag .AND.  RootFinderData%UpperPoint%DefinedFlag ) THEN
    DeltaUL = RootFinderData%UpperPoint%X - RootFinderData%LowerPoint%X
    TypUL   = (ABS(RootFinderData%UpperPoint%X) + ABS(RootFinderData%LowerPoint%X))/2.0d0
    TolUL   = RootFinderData%Controls%TolX * ABS(TypUL) + RootFinderData%Controls%ATolX

    ! Halve tolerance to reflect the fact that solution can be anywhere between the lower and upper points.
    IF ( ABS(DeltaUL) <= 0.5d0 * ABS(TolUL) ) THEN
        CheckBracketRoundOff = .TRUE.
        RETURN
    END IF
  END IF

  CheckBracketRoundOff = .FALSE.

  RETURN
END FUNCTION CheckBracketRoundOff


SUBROUTINE UpdateMinMax( RootFinderData, X, Y )
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Dimitri Curtil (LBNL)
          !       DATE WRITTEN   February 2006
          !       MODIFIED
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine updates the min/max support points in the root finder data.
          !

          ! METHODOLOGY EMPLOYED:
          !
          ! PRECONDITION:
          ! na
          !
          ! POSTCONDITION:
          ! - RootFinderData%MinPoint possibly updated
          ! - RootFinderData%MaxPoint possibly updated
          !

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  TYPE(RootFinderDataType), INTENT(INOUT) :: RootFinderData ! Data used by root finding algorithm
  REAL(r64), INTENT(IN)                        :: X              ! X value for current iterate
  REAL(r64), INTENT(IN)                        :: Y              ! Y value for current iterate, F(X)=Y

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          ! na

          ! FLOW:

  ! Update min support point
  IF ( X == RootFinderData%MinPoint%X ) THEN
    RootFinderData%MinPoint%Y = Y
    RootFinderData%MinPoint%DefinedFlag = .TRUE.
  END IF

  ! Update max support point
  IF ( X == RootFinderData%MaxPoint%X ) THEN
    RootFinderData%MaxPoint%Y = Y
    RootFinderData%MaxPoint%DefinedFlag = .TRUE.
  END IF

  RETURN
END SUBROUTINE UpdateMinMax


SUBROUTINE UpdateBracket( RootFinderData, X, Y )
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Dimitri Curtil (LBNL)
          !       DATE WRITTEN   March 2006
          !       MODIFIED
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine updates the lower/upper support points in the root finder data
          ! with the current iterate (X,Y).
          !

          ! METHODOLOGY EMPLOYED:
         !
          ! PRECONDITION:
          ! - The current iterate (X,Y) must satisfy:
          !   MinPoint%X <= LowerPoint%X < X < UpperPoint%X <= MaxPoint%X
          ! - RootFinderData%StatusFlag == iStatusNone
          !
          ! POSTCONDITION:
          ! - RootFinderData%LowerPoint possibly updated
          ! - RootFinderData%UpperPoint possibly updated
          ! - RootFinderData%StatusFlag possibly updated with:
          !   - iStatusWarningNonMonotonic
          !   - iStatusWarningSingular
          !

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  TYPE(RootFinderDataType), INTENT(INOUT) :: RootFinderData ! Data used by root finding algorithm
  REAL(r64), INTENT(IN)                        :: X              ! X value for current iterate
  REAL(r64), INTENT(IN)                        :: Y              ! Y value for current iterate, F(X)=Y

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          ! na

          ! FLOW:

  SelectSlope: SELECT CASE ( RootFinderData%Controls%SlopeType )

  CASE ( iSlopeIncreasing )
    ! Update lower point
    IF ( Y <= 0.0d0 ) THEN
      IF ( .NOT.RootFinderData%LowerPoint%DefinedFlag ) THEN
        RootFinderData%LowerPoint%DefinedFlag = .TRUE.
        RootFinderData%LowerPoint%X = X
        RootFinderData%LowerPoint%Y = Y
      ELSE
        IF ( X >= RootFinderData%LowerPoint%X ) THEN
          IF ( Y == RootFinderData%LowerPoint%Y ) THEN
            RootFinderData%StatusFlag = iStatusWarningSingular
          ELSE IF ( Y < RootFinderData%LowerPoint%Y ) THEN
            RootFinderData%StatusFlag = iStatusWarningNonMonotonic
          END IF
          ! Update lower point with current iterate
          RootFinderData%LowerPoint%X = X
          RootFinderData%LowerPoint%Y = Y
        ELSE
          ! Should never happen if CheckLowerUpperBracket() is called before
          CALL ShowSevereError('UpdateBracket: Current iterate is smaller than the lower bracket.')
          CALL ShowContinueError( &
            'UpdateBracket: '// &
            'X='//TRIM(TrimSigDigits(X,15))//', '// &
            'Y='//TRIM(TrimSigDigits(Y,15)) &
          )
          CALL ShowContinueError( &
            'UpdateBracket: '// &
            'XLower='//TRIM(TrimSigDigits(RootFinderData%LowerPoint%X,15))//', '// &
            'YLower='//TRIM(TrimSigDigits(RootFinderData%LowerPoint%Y,15)) &
          )
          CALL ShowFatalError('UpdateBracket: Preceding error causes program termination.')
        END IF
      END IF

    ! Update upper point
    ELSE
      IF ( .NOT.RootFinderData%UpperPoint%DefinedFlag ) THEN
        RootFinderData%UpperPoint%DefinedFlag = .TRUE.
        RootFinderData%UpperPoint%X = X
        RootFinderData%UpperPoint%Y = Y
      ELSE
        IF ( X <= RootFinderData%UpperPoint%X ) THEN
          IF ( Y == RootFinderData%UpperPoint%Y ) THEN
            RootFinderData%StatusFlag = iStatusWarningSingular
          ELSE IF ( Y > RootFinderData%UpperPoint%Y ) THEN
            RootFinderData%StatusFlag = iStatusWarningNonMonotonic
          END IF
          ! Update upper point with current iterate
          RootFinderData%UpperPoint%X = X
          RootFinderData%UpperPoint%Y = Y
        ELSE
          ! Should never happen if CheckLowerUpperBracket() is called before
          CALL ShowSevereError('UpdateBracket: Current iterate is greater than the upper bracket.')
          CALL ShowContinueError( &
            'UpdateBracket: '// &
            'X='//TRIM(TrimSigDigits(X,15))//', '// &
            'Y='//TRIM(TrimSigDigits(Y,15)) &
          )
          CALL ShowContinueError( &
            'UpdateBracket: '// &
            'XUpper='//TRIM(TrimSigDigits(RootFinderData%UpperPoint%X,15))//', '// &
            'YUpper='//TRIM(TrimSigDigits(RootFinderData%UpperPoint%Y,15)) &
          )
          CALL ShowFatalError('UpdateBracket: Preceding error causes program termination.')
        END IF
      END IF
    END IF

  ! Monotone, decreasing function
  CASE ( iSlopeDecreasing )
    ! Update lower point
    IF ( Y >= 0.0d0 ) THEN
      IF ( .NOT.RootFinderData%LowerPoint%DefinedFlag ) THEN
        RootFinderData%LowerPoint%DefinedFlag = .TRUE.
        RootFinderData%LowerPoint%X = X
        RootFinderData%LowerPoint%Y = Y
      ELSE
        IF ( X >= RootFinderData%LowerPoint%X ) THEN
          IF ( Y == RootFinderData%LowerPoint%Y ) THEN
            RootFinderData%StatusFlag = iStatusWarningSingular
          ELSE IF ( Y > RootFinderData%LowerPoint%Y ) THEN
            RootFinderData%StatusFlag = iStatusWarningNonMonotonic
          END IF
          ! Update lower point with current iterate
          RootFinderData%LowerPoint%X = X
          RootFinderData%LowerPoint%Y = Y
        ELSE
          ! Should never happen if CheckLowerUpperBracket() is called before
          CALL ShowSevereError('UpdateBracket: Current iterate is smaller than the lower bracket.')
          CALL ShowContinueError( &
            'UpdateBracket: '// &
            'X='//TRIM(TrimSigDigits(X,15))//', '// &
            'Y='//TRIM(TrimSigDigits(Y,15)) &
          )
          CALL ShowContinueError( &
            'UpdateBracket: '// &
            'XLower='//TRIM(TrimSigDigits(RootFinderData%LowerPoint%X,15))//', '// &
            'YLower='//TRIM(TrimSigDigits(RootFinderData%LowerPoint%Y,15)) &
          )
          CALL ShowFatalError('UpdateBracket: Preceding error causes program termination.')
        END IF
      END IF

    ! Update upper point
    ELSE
      IF ( .NOT.RootFinderData%UpperPoint%DefinedFlag ) THEN
        RootFinderData%UpperPoint%DefinedFlag = .TRUE.
        RootFinderData%UpperPoint%X = X
        RootFinderData%UpperPoint%Y = Y
      ELSE
        IF ( X <= RootFinderData%UpperPoint%X ) THEN
          IF ( Y == RootFinderData%UpperPoint%Y ) THEN
            RootFinderData%StatusFlag = iStatusWarningSingular
          ELSE IF ( Y < RootFinderData%UpperPoint%Y ) THEN
            RootFinderData%StatusFlag = iStatusWarningNonMonotonic
          END IF
          ! Update upper point with current iterate
          RootFinderData%UpperPoint%X = X
          RootFinderData%UpperPoint%Y = Y
        ELSE
          ! Should never happen if CheckLowerUpperBracket() is called before
          CALL ShowSevereError('UpdateBracket: Current iterate is greater than the upper bracket.')
          CALL ShowContinueError( &
            'UpdateBracket: '// &
            'X='//TRIM(TrimSigDigits(X,15))//', '// &
            'Y='//TRIM(TrimSigDigits(Y,15)) &
          )
          CALL ShowContinueError( &
            'UpdateBracket: '// &
            'XUpper='//TRIM(TrimSigDigits(RootFinderData%UpperPoint%X,15))//', '// &
            'YUpper='//TRIM(TrimSigDigits(RootFinderData%UpperPoint%Y,15)) &
          )
          CALL ShowFatalError('UpdateBracket: Preceding error causes program termination.')
        END IF
      END IF
    END IF

  CASE DEFAULT
    ! Should never happen
    CALL ShowSevereError('UpdateBracket: Invalid function slope specification. Valid choices are:')
    CALL ShowContinueError('UpdateBracket: iSlopeIncreasing='//TRIM(TrimSigDigits(iSlopeIncreasing)))
    CALL ShowContinueError('UpdateBracket: iSlopeDecreasing='//TRIM(TrimSigDigits(iSlopeDecreasing)))
    CALL ShowFatalError('UpdateBracket: Preceding error causes program termination.')

  END SELECT SelectSlope

  RETURN
END SUBROUTINE UpdateBracket


SUBROUTINE UpdateHistory( RootFinderData, X, Y )
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Dimitri Curtil (LBNL)
          !       DATE WRITTEN   February 2006
          !       MODIFIED
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine updates the min/max support points in the root finder data.
          !

          ! METHODOLOGY EMPLOYED:
          !
          ! PRECONDITION:
          ! - The current iterate (X,Y) must be a valid iterate:
          !   MinPoint%X <= LowerPoint%X < X < UpperPoint%X <= MaxPoint%X
          !
          ! POSTCONDITION:
          ! - RootFinderData%History(:) updated with last 3 best iterates
          !

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  TYPE(RootFinderDataType), INTENT(INOUT) :: RootFinderData ! Data used by root finding algorithm
  REAL(r64), INTENT(IN)                        :: X              ! X value for current iterate
  REAL(r64), INTENT(IN)                        :: Y              ! Y value for current iterate, F(X)=Y

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER                                 :: NumHistory

          ! FLOW:

  ! Update history with best iterates so that:
  !   ABS(History(1)%Y) <= ABS(History(2)%Y) <= ABS(History(3)%Y)
  !
  ! Note that the history points are sorted so that
  !   SIGN(History(1)%Y) = - SIGN(History(3)%Y)
  ! to ensure that the history points bracket the candidate root.
  RootFinderData%History(:)%DefinedFlag = .FALSE.
  RootFinderData%History(:)%X = 0.0d0
  RootFinderData%History(:)%Y = 0.0d0

  NumHistory = 0
  IF ( RootFinderData%LowerPoint%DefinedFlag ) THEN
    NumHistory = NumHistory+1
    RootFinderData%History(NumHistory)%DefinedFlag = RootFinderData%LowerPoint%DefinedFlag
    RootFinderData%History(NumHistory)%X = RootFinderData%LowerPoint%X
    RootFinderData%History(NumHistory)%Y = RootFinderData%LowerPoint%Y
  END IF
  IF ( RootFinderData%UpperPoint%DefinedFlag ) THEN
    NumHistory = NumHistory+1
    RootFinderData%History(NumHistory)%DefinedFlag = RootFinderData%UpperPoint%DefinedFlag
    RootFinderData%History(NumHistory)%X = RootFinderData%UpperPoint%X
    RootFinderData%History(NumHistory)%Y = RootFinderData%UpperPoint%Y
  END IF
  NumHistory = NumHistory+1
  RootFinderData%History(NumHistory)%DefinedFlag = .TRUE.
  RootFinderData%History(NumHistory)%X = X
  RootFinderData%History(NumHistory)%Y = Y

  RootFinderData%NumHistory = NumHistory
  CALL SortHistory( NumHistory, RootFinderData%History )

  RETURN
END SUBROUTINE UpdateHistory


SUBROUTINE UpdateRootFinder( RootFinderData, X, Y )
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Dimitri Curtil (LBNL)
          !       DATE WRITTEN   February 2006
          !       MODIFIED
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine updates the root finder internal data to account for
          ! the current iterate (X,Y):
          ! - Lower / Upper support points
          ! - Increments for successive iterates
          ! - Convergence rate
          !

          ! METHODOLOGY EMPLOYED:
          !
          ! PRECONDITION:
          ! - The current iterate (X,Y) must be a valid iterate:
          !   MinPoint%X <= LowerPoint%X < X < UpperPoint%X <= MaxPoint%X
          ! - Invoke UpdateRootFinder() only if:
          !   - CheckRootFinderCandidate() returned TRUE
          !   - CheckNonSingularity() returned TRUE
          !   - CheckSlope() returned TRUE
          !
          ! POSTCONDITION:
          ! - RootFinderData%LowerPoint possibly updated
          ! - RootFinderData%UpperPoint possibly updated
          ! - RootFinderData%CurrentPoint updated with current iterate (X,Y)
          ! - RootFinderData%History(:) updated with last 3 best iterates
          ! - RootFinderData%Increment updated
          ! - RootFinderData%ConvergenceRate updated
          !

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  TYPE(RootFinderDataType), INTENT(INOUT) :: RootFinderData ! Data used by root finding algorithm
  REAL(r64), INTENT(IN)                        :: X              ! X value for current iterate
  REAL(r64), INTENT(IN)                        :: Y              ! Y value for current iterate, F(X)=Y

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          ! na

          ! FLOW:


  ! Update history with best iterates so that:
  !   ABS(History(1)%Y) <= ABS(History(2)%Y) <= ABS(History(3)%Y)
  !
  ! Note that we must update the history before updating the lower/upper points
  CALL UpdateHistory( RootFinderData, X, Y )

  ! Update lower and upper points
  CALL UpdateBracket( RootFinderData, X, Y )

  ! Update increments and convergence rate
  IF ( RootFinderData%CurrentPoint%DefinedFlag ) THEN
    RootFinderData%Increment%DefinedFlag = .TRUE.
    RootFinderData%Increment%X = X - RootFinderData%CurrentPoint%X
    RootFinderData%Increment%Y = Y - RootFinderData%CurrentPoint%Y

    IF ( ABS(RootFinderData%CurrentPoint%Y) > 0.0d0 ) THEN
      ! NOTE: Should be smaller than one for convergent process
      RootFinderData%ConvergenceRate = ABS(Y) / ABS(RootFinderData%CurrentPoint%Y)
    ELSE
      ! NOTE: Should never happen
      RootFinderData%ConvergenceRate = -1.0d0
    END IF
  END IF

  ! Finally update CurrentPoint (must be done last)
  RootFinderData%CurrentPoint%DefinedFlag = .TRUE.
  RootFinderData%CurrentPoint%X = X
  RootFinderData%CurrentPoint%Y = Y

  RETURN

END SUBROUTINE UpdateRootFinder


SUBROUTINE SortHistory( N, History )
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Dimitri Curtil (LBNL)
          !       DATE WRITTEN   March 2006
          !       MODIFIED
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine orders the N points in the history array in increasing
          ! order of ABS(Y) values.
          !

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)            :: N           ! Number of points to sort in history array
  TYPE(PointType), INTENT(INOUT) :: History(:)  ! Array of PointType variables. At least N of them

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER                       :: I, J
  REAL(r64)                     :: XTemp, YTemp
          ! FLOW:

  ! Nothing to do if only one point stored in history
  IF ( N <= 1 ) THEN
    RETURN
  END IF

  DO I=1,N-1
    DO J=I+1,N
      IF ( History(J)%DefinedFlag ) THEN
        ! Swap I and J elements
        IF ( ABS(History(J)%Y) < ABS(History(I)%Y) ) THEN
          XTemp = History(I)%X
          YTemp = History(I)%Y
          History(I)%X = History(J)%X
          History(I)%Y = History(J)%Y
          History(J)%X = XTemp
          History(J)%Y = YTemp
        END IF
      END IF
    END DO
  END DO

  RETURN
END SUBROUTINE SortHistory


SUBROUTINE AdvanceRootFinder( RootFinderData )
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Dimitri Curtil (LBNL)
          !       DATE WRITTEN   February 2006
          !       MODIFIED
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine computes the next candidate value based on the information available so far.
          ! Stores new value into RootFinderData%XCandidate
          !
          ! PRECONDITION:
          ! na
          !
          ! POSTCONDITION:
          ! - LowerPoint%X < XCandidate < UpperPoint%X
          ! - RootFinderData%CurrentMethodType update with current solution method.
          !

          ! METHODOLOGY EMPLOYED:
          ! The subroutine first attempts to bracket the root within a lower and upper point.
          ! Once it is bracketed, then we use the specified solution methods (Bisection,
          ! False position, Secant and Brent) to compute the next candidate.
          !


          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  TYPE(RootFinderDataType), INTENT(INOUT) :: RootFinderData ! Data used by root finding algorithm

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64)                               :: XNext = 0.0d0

          ! FLOW:

  !----------------------------------------------------------------------------
  ! First attempt to bracket root between a lower point and an upper point.
  !----------------------------------------------------------------------------

  ! Detect the lower bracket
  IF ( .NOT.RootFinderData%LowerPoint%DefinedFlag ) THEN
    RootFinderData%CurrentMethodType = iMethodBracket
    ! If we have 2 points already, try to detect lower point using the Secant formula
    IF ( BracketRoot( RootFinderData, XNext ) ) THEN
        RootFinderData%XCandidate = XNext
    ELSE
      IF ( .NOT.RootFinderData%MinPoint%DefinedFlag ) THEN
        RootFinderData%XCandidate = RootFinderData%MinPoint%X
      ELSE
        ! Should never happen
        CALL ShowFatalError('AdvanceRootFinder: Cannot find lower bracket.')
      END IF
    END IF

  ! Detect the upper bracket
  ELSE IF ( .NOT.RootFinderData%UpperPoint%DefinedFlag ) THEN
    RootFinderData%CurrentMethodType = iMethodBracket
    ! If we have 2 points already, try to detect upper point using the Secant formula
    IF ( BracketRoot( RootFinderData, XNext ) ) THEN
        RootFinderData%XCandidate = XNext
    ELSE
      IF ( .NOT.RootFinderData%MaxPoint%DefinedFlag ) THEN
        RootFinderData%XCandidate = RootFinderData%MaxPoint%X
      ELSE
        ! Should never happen
        CALL ShowFatalError('AdvanceRootFinder: Cannot find upper bracket.')
      END IF
    END IF


  !----------------------------------------------------------------------------
  ! Root finding can start ...
  !
  ! Assumptions:
  ! - the lower and upper support points are defined.
  ! - the increments are defined (at least 2 history points are available)
  !----------------------------------------------------------------------------
  ELSE
    SelectRecoveryMethod: SELECT CASE ( RootFinderData%StatusFlag )
    CASE ( iStatusOKRoundOff )
      ! Should never happen if we exit the root finder upon detecting round-off condition
      RootFinderData%XCandidate = BisectionMethod(RootFinderData)

    CASE ( iStatusWarningSingular, iStatusWarningNonMonotonic )
      ! Following local singularity or non-monotonicity warnings we attempt
      ! to recover with the false position method to avoid running into trouble
      ! because the latest iterate did nt produce any improvement compared to
      ! the previous lower and upper brackets.
      RootFinderData%XCandidate = FalsePositionMethod(RootFinderData)

    CASE DEFAULT
      ! Assuming that the root is bracketed between the lower and upper points,
      ! we execute the requested solution method to produce the next candidate value
      ! for the root.
      SelectMethod: SELECT CASE(RootFinderData%Controls%MethodType)
      CASE (iMethodBisection)
        ! Bisection method (aka interval halving)
        RootFinderData%XCandidate = BisectionMethod(RootFinderData)
      CASE (iMethodFalsePosition)
        ! False position method (aka regula falsi)
        RootFinderData%XCandidate = FalsePositionMethod(RootFinderData)
      CASE (iMethodSecant)
        ! Secant method
        RootFinderData%XCandidate = SecantMethod(RootFinderData)
      CASE (iMethodBrent)
        ! Brent method
        RootFinderData%XCandidate = BrentMethod(RootFinderData)
      CASE DEFAULT
        CALL ShowSevereError('AdvanceRootFinder: Invalid solution method specification. Valid choices are:' )
        CALL ShowContinueError('AdvanceRootFinder: iMethodBisection='//TRIM(TrimSigDigits(iMethodBisection)))
        CALL ShowContinueError('AdvanceRootFinder: iMethodFalsePosition='//TRIM(TrimSigDigits(iMethodFalsePosition)))
        CALL ShowContinueError('AdvanceRootFinder: iMethodSecant='//TRIM(TrimSigDigits(iMethodSecant)))
        CALL ShowContinueError('AdvanceRootFinder: iMethodBrent='//TRIM(TrimSigDigits(iMethodBrent)))
        CALL ShowFatalError('AdvanceRootFinder: Preceding error causes program termination.')
      END SELECT SelectMethod
    END SELECT SelectRecoveryMethod
  END IF

  RETURN
END SUBROUTINE AdvanceRootFinder


LOGICAL FUNCTION BracketRoot( RootFinderData, XNext )
          ! FUNCTION INFORMATION:
          !       AUTHOR         Dimitri Curtil (LBNL)
          !       DATE WRITTEN   February 2006
          !       MODIFIED
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function attempts to compute a new point that will bracket the root
          ! using the secant formula to take advantage of the slope between the last 2
          ! iterates.
          !
          ! Returns TRUE if successfully computed a new bracket in XNext.
          ! Else returns FASLE and does not update the XNext argument.
          !
          ! Should only be used while in braketing mode (iMethodBracket).
          ! When the lower and upper brackets are detected then the FUNCTION SecantMethod
          ! should be used instead.
          !
          ! PRECONDITION:
          ! na
          !
          ! POSTCONDITION:
          ! - MinPoint%X <= XNext <= MaxPoint%X
          ! - LowerPoint%X < XNext < UpperPoint%X
          !

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  TYPE(RootFinderDataType), INTENT(IN)       :: RootFinderData ! Data used by root finding algorithm
  REAL(r64), INTENT(OUT)                          :: XNext          ! Next value

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
          ! na

          ! FLOW:

  ! Cannot use Secant method unless there are at least 2 points
  ! Also do not use Secant method more than once, i.e. NumHistory==3, in order to avoid
  ! the pathological case whereby the secant method always comes up short of bracketting
  ! the root because the function slope flattens as we come closer to either min/max point.
  IF ( RootFinderData%NumHistory /= 2 ) THEN
    BracketRoot = .FALSE.
    RETURN
  END IF

  ! Should not use Secant method if the last 2 points produced a warning
  IF ( RootFinderData%StatusFlag == iStatusWarningSingular .OR. &
       RootFinderData%StatusFlag == iStatusWarningNonMonotonic ) THEN
    BracketRoot = .FALSE.
    RETURN
  END IF

  ! Try to compute next root candidate using Secant formula
  IF ( SecantFormula( RootFinderData, XNext ) ) THEN

    ! Check that next candidate is consistent with min/max constraints and lower/upper brackets
    IF ( CheckRootFinderCandidate( RootFinderData, XNext ) ) THEN
      BracketRoot = .TRUE.
      RETURN
    END IF
  END IF

  BracketRoot = .FALSE.

  RETURN
END FUNCTION BracketRoot


REAL(r64) FUNCTION BisectionMethod( RootFinderData )
          ! FUNCTION INFORMATION:
          !       AUTHOR         Dimitri Curtil (LBNL)
          !       DATE WRITTEN   February 2006
          !       MODIFIED
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function computes the next iterate using the bisection method (aka interval halving).
          ! Convergence rate is at best linear.
          !
          ! PRECONDITION:
          ! Lower and upper points must be defined and distinct.
          !
          ! POSTCONDITION:
          ! - LowerPoint%X < XCandidate < UpperPoint%X
          ! - RootFinderData%CurrentMethodType update with current solution method.
          !

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  TYPE(RootFinderDataType), INTENT(INOUT)    :: RootFinderData ! Data used by root finding algorithm

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
          ! na

          ! FLOW:
  RootFinderData%CurrentMethodType = iMethodBisection
  BisectionMethod = (RootFinderData%LowerPoint%X + RootFinderData%UpperPoint%X)/2.0d0

  RETURN
END FUNCTION BisectionMethod


REAL(r64) FUNCTION FalsePositionMethod( RootFinderData )
          ! FUNCTION INFORMATION:
          !       AUTHOR         Dimitri Curtil (LBNL)
          !       DATE WRITTEN   February 2006
          !       MODIFIED
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function computes the next iterate using the false position method (aka regula falsi).
          ! If new iterate does not lie within the lower and upper points then
          ! the Bisection method is used instead.
          !
          ! Convergence rate is at best superlinear.
          !
          ! PRECONDITION:
          ! Lower and upper points must be defined and distinct.
          !
          ! POSTCONDITION:
          ! - LowerPoint%X < XCandidate < UpperPoint%X
          ! - RootFinderData%CurrentMethodType update with current solution method.
          !

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  TYPE(RootFinderDataType), INTENT(INOUT)    :: RootFinderData ! Data used by root finding algorithm

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  REAL(r64)                     :: XCandidate
  REAL(r64)                     :: Num, Den

          ! FLOW:

  Num = RootFinderData%UpperPoint%X - RootFinderData%LowerPoint%X
  Den = RootFinderData%UpperPoint%Y - RootFinderData%LowerPoint%Y

  IF ( Den /= 0.0d0 ) THEN
    ! False position method
    RootFinderData%CurrentMethodType = iMethodFalsePosition
    XCandidate = RootFinderData%LowerPoint%X - RootFinderData%LowerPoint%Y * Num/Den

    ! Check that new candidate is within range and brackets
    IF ( .NOT.CheckRootFinderCandidate(RootFinderData, XCandidate) ) THEN
      ! Recovery method
      XCandidate = BisectionMethod(RootFinderData)
    END IF
  ELSE
    ! Recovery method
    XCandidate = BisectionMethod(RootFinderData)
  END IF

  FalsePositionMethod = XCandidate
  RETURN
END FUNCTION FalsePositionMethod


REAL(r64) FUNCTION SecantMethod( RootFinderData )
          ! FUNCTION INFORMATION:
          !       AUTHOR         Dimitri Curtil (LBNL)
          !       DATE WRITTEN   February 2006
          !       MODIFIED
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function computes the next iterate using the secant method.
          ! If new iterate does not lie within the lower and upper points then
          ! the false position method is used instead.
          !
          ! Convergence rate is at best superlinear.
          !
          ! PRECONDITION:
          ! There must be at least 2 history points so that RootFinderData%Increment is defined.
          ! See FUNCTION SecantFormula.
          !
          ! POSTCONDITION:
          ! - LowerPoint%X < XCandidate < UpperPoint%X
          ! - RootFinderData%CurrentMethodType update with current solution method.
          !

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  TYPE(RootFinderDataType), INTENT(INOUT)    :: RootFinderData ! Data used by root finding algorithm

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  REAL(r64)                     :: XCandidate

          ! FLOW:

  ! Recover with false position
  IF ( SecantFormula(RootFinderData, XCandidate) ) THEN
    ! Secant method
    RootFinderData%CurrentMethodType = iMethodSecant

    ! Check that new candidate is within range and brackets
    IF ( .NOT.CheckRootFinderCandidate(RootFinderData, XCandidate) ) THEN
      ! Recovery method
      XCandidate = FalsePositionMethod(RootFinderData)
    END IF
  ELSE
    ! Recovery method
    XCandidate = FalsePositionMethod(RootFinderData)
  END IF

  SecantMethod = XCandidate
  RETURN
END FUNCTION SecantMethod


LOGICAL FUNCTION SecantFormula( RootFinderData, XNext )
          ! FUNCTION INFORMATION:
          !       AUTHOR         Dimitri Curtil (LBNL)
          !       DATE WRITTEN   April 2006
          !       MODIFIED
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function computes the next iterate using the secant formula.
          ! If the new iterate cannot be computed the function returns FALSE, else TRUE.
          !
          ! Convergence rate is at best superlinear.
          !
          ! PRECONDITION:
          ! There must be at least 2 history points so that RootFinderData%Increment is defined.
          !
          ! POSTCONDITION:
          ! XNext contains the result from applying the Secant formula.
          ! If XNext could not be computed then leave XNext unchanged.
          !

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  TYPE(RootFinderDataType), INTENT(IN)       :: RootFinderData ! Data used by root finding algorithm
  REAL(r64), INTENT(OUT)                          :: XNext          ! Result from Secant formula if possible to compute

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  REAL(r64)                     :: Num
  REAL(r64)                     :: Den

          ! FLOW:

  Num = RootFinderData%Increment%X
  Den = RootFinderData%Increment%Y

  ! Cannot use secant with infinite slope (Den==0).
  ! Cannot use secant with null slope (Num==0).
  IF ( Den /= 0.0d0 .AND. Num /= 0.0d0 ) THEN
    XNext = RootFinderData%CurrentPoint%X - RootFinderData%CurrentPoint%Y * Num/Den
    SecantFormula = .TRUE.
  ELSE
    SecantFormula = .FALSE.
  END IF

  RETURN
END FUNCTION SecantFormula


REAL(r64) FUNCTION BrentMethod( RootFinderData )
          ! FUNCTION INFORMATION:
          !       AUTHOR         Dimitri Curtil (LBNL)
          !       DATE WRITTEN   March 2006
          !       MODIFIED
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function computes the next iterate using the Brent's method.
          ! If new iterate does not lie within the lower and upper points then
          ! the secant method is used instead.
          !
          ! Convergence rate is at best quadratic.
          !
          ! PRECONDITION:
          ! Lower and upper points must be defined and distinct.
          !
          ! POSTCONDITION:
          ! - LowerPoint%X < XCandidate < UpperPoint%X
          ! - RootFinderData%CurrentMethodType update with current solution method.
          !

          ! METHODOLOGY EMPLOYED:
          ! Inverse quadratic interpolation using the last 3 best iterates.
          ! The next root estimate is x = B + P/Q whereby B is the current best estimate
          ! of the root.
          !

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  TYPE(RootFinderDataType), INTENT(INOUT)    :: RootFinderData ! Data used by root finding algorithm

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  REAL(r64)                     :: XCandidate
  REAL(r64)                     :: A, FA, B, FB, C, FC
  REAL(r64)                     :: R, S, T, P, Q
          ! FLOW:

  ! Only attempt Brent's method if enough history points are available
  ! and if the root finder is converging (not diverging) since the previous
  ! iterate.
  !
  ! We assume that;
  ! - the root is bracketed between the lower and upper points (see AdvanceRootFinder() ).
  ! - there are at least 3 history points
  IF ( RootFinderData%NumHistory == 3 ) THEN

    A  = RootFinderData%History(2)%X
    FA = RootFinderData%History(2)%Y
    B  = RootFinderData%History(1)%X
    FB = RootFinderData%History(1)%Y
    C  = RootFinderData%History(3)%X
    FC = RootFinderData%History(3)%Y

    ! Should never happen if CheckRootFinderConvergence() is invoked prior to this subroutine
    IF (FC == 0.0d0) THEN
      BrentMethod = C
      RETURN
    ! Should never happen if CheckRootFinderConvergence() is invoked prior to this subroutine
    ELSE IF (FA == 0.0d0) THEN
      BrentMethod = A
      RETURN
    ELSE
      R = FB/FC
      S = FB/FA
      T = FA/FC

      P = S*(T*(R-T)*(C-B)-(1.0d0-R)*(B-A))
      Q = (T-1.0d0)*(R-1.0d0)*(S-1.0d0)

      ! Only accept correction if it is small enough (75% of previous increment)
      IF ( ABS(P) <= 0.75d0*ABS(Q*RootFinderData%Increment%X) ) THEN
        RootFinderData%CurrentMethodType = iMethodBrent
        XCandidate = B + P/Q

        ! Check that new candidate is within range and brackets
        IF ( .NOT.CheckRootFinderCandidate(RootFinderData, XCandidate) ) THEN
          ! Recovery method
          XCandidate = FalsePositionMethod( RootFinderData )
        END IF
      ELSE
        ! Recover from bad correction with bisection
        ! Biscetion produced the best numerical performance in testing compared to
        ! - Secant
        ! - False position (very slow recovery)
        XCandidate = BisectionMethod( RootFinderData )
      END IF
    END IF
  ELSE
    ! Not enough history to try Brent's method yet: use Secant's method
    XCandidate = SecantMethod( RootFinderData )
  END IF

  BrentMethod = XCandidate
  RETURN
END FUNCTION BrentMethod


SUBROUTINE WriteRootFinderTraceHeader( TraceFileUnit )
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Dimitri Curtil (LBNL)
          !       DATE WRITTEN   March 2006
          !       MODIFIED
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine writes the header for the trace file to the specified
          ! file unit using CSV formatting.
          !

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)                  :: TraceFileUnit  ! Unit for trace file

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          ! na

          ! FLOW:

  WRITE(TraceFileUnit,'(20(A,A))',ADVANCE='No') &
    'Status', ',', 'Method', ',', &
    'CurrentPoint%X', ',', 'CurrentPoint%Y', ',', &
    'XCandidate', ',', 'ConvergenceRate', ',', &
    !'MinPoint%DefinedFlag', ',', &
    'MinPoint%X', ',', 'MinPoint%Y', ',', &
    !'LowerPoint%DefinedFlag', ',', &
    'LowerPoint%X', ',', 'LowerPoint%Y', ',', &
    !'UpperPoint%DefinedFlag', ',', &
    'UpperPoint%X', ',', 'UpperPoint%Y', ',', &
    !'MaxPoint%DefinedFlag', ',', &
    'MaxPoint%X', ',', 'MaxPoint%Y', ',', &
    !'History(1)%DefinedFlag', ',', &
    'History(1)%X', ',', 'History(1)%Y', ',', &
    !'History(2)%DefinedFlag', ',', &
    'History(2)%X', ',', 'History(2)%Y', ',', &
    !'History(3)%DefinedFlag', ',', &
    'History(3)%X', ',', 'History(3)%Y', ','

  RETURN
END SUBROUTINE WriteRootFinderTraceHeader


SUBROUTINE WriteRootFinderTrace( TraceFileUnit, RootFinderData )
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Dimitri Curtil (LBNL)
          !       DATE WRITTEN   March 2006
          !       MODIFIED
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine writes the current state of the root finder data to the trace file
          ! unit using CSV formatting.
          !

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE General, ONLY: TrimSigDigits

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)                  :: TraceFileUnit  ! Unit for trace file
  TYPE(RootFinderDataType), INTENT(IN) :: RootFinderData ! Data used by root finding algorithm

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          ! na

          ! FLOW:

  WRITE(TraceFileUnit,'(2(A,A))',ADVANCE='No') &
    TRIM(TrimSigDigits(RootFinderData%StatusFlag)), ',', &
    TRIM(TrimSigDigits(RootFinderData%CurrentMethodType)), ','

  ! Only show current point if defined.
  CALL WritePoint( TraceFileUnit, RootFinderData%CurrentPoint, .FALSE. )

  WRITE(TraceFileUnit,'(2(F20.10,A))',ADVANCE='No') &
    RootFinderData%XCandidate, ',', &
    RootFinderData%ConvergenceRate, ','

  ! Always show min and max points.
  ! Only show lower and upper points if defined.
  CALL WritePoint( TraceFileUnit, RootFinderData%MinPoint,   .TRUE. )
  CALL WritePoint( TraceFileUnit, RootFinderData%LowerPoint, .FALSE. )
  CALL WritePoint( TraceFileUnit, RootFinderData%UpperPoint, .FALSE. )
  CALL WritePoint( TraceFileUnit, RootFinderData%MaxPoint,   .TRUE. )
  ! Only show history points if defined.
  CALL WritePoint( TraceFileUnit, RootFinderData%History(1), .FALSE. )
  CALL WritePoint( TraceFileUnit, RootFinderData%History(2), .FALSE. )
  CALL WritePoint( TraceFileUnit, RootFinderData%History(3), .FALSE. )

  RETURN
END SUBROUTINE WriteRootFinderTrace


SUBROUTINE WritePoint( TraceFileUnit, PointData, ShowXValue )
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Dimitri Curtil (LBNL)
          !       DATE WRITTEN   March 2006
          !       MODIFIED
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine writes the current point data to the trace file
          ! unit using CSV formatting.
          ! If not defined writes an empty string instead.
          !

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)                  :: TraceFileUnit  ! Unit for trace file
  TYPE(PointType), INTENT(IN)          :: PointData      ! Point data structure
  ! If set to TRUE, ten always show the X value even if not defined
  LOGICAL, INTENT(IN)                  :: ShowXValue

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=1)                     :: NoValue = ' '  ! String used whenever the value is not available

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          ! na

          ! FLOW:

    IF ( PointData%DefinedFlag ) THEN
      WRITE(TraceFileUnit,'(2(F20.10,A))',ADVANCE='No')  &
        PointData%X, ',', &
        PointData%Y, ','
    ELSE
      IF ( ShowXValue ) THEN
        WRITE(TraceFileUnit,'(1(F20.10,A),1(A,A))',ADVANCE='No')  &
          PointData%X, ',', &
          NoValue, ','
      ELSE
        WRITE(TraceFileUnit,'(2(A,A))',ADVANCE='No')  &
          NoValue, ',', &
          NoValue, ','
      END IF
    END IF

  RETURN
END SUBROUTINE WritePoint


SUBROUTINE DebugRootFinder( FileUnit, RootFinderData )
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Dimitri Curtil (LBNL)
          !       DATE WRITTEN   April 2006
          !       MODIFIED
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine writes the current min/max range and lower/upper bracket to
          ! the standard output file.
          !
          ! Used only for debugging.
          !

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  TYPE(RootFinderDataType), INTENT(IN) :: RootFinderData ! Data used by root finding algorithm

          ! SUBROUTINE PARAMETER DEFINITIONS:
  INTEGER, INTENT(IN)                  :: FileUnit       ! File unit where to write debugging info

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          ! na

          ! FLOW:

  ! UNIT=0 should correspond to the standard output file (screen).
  WRITE(FileUnit,'(A)',ADVANCE='No') 'Current = '
  CALL WritePoint( FileUnit, RootFinderData%CurrentPoint,   .TRUE. )
  WRITE(FileUnit,*)

  WRITE(FileUnit,'(A)',ADVANCE='No') 'Min     = '
  CALL WritePoint( FileUnit, RootFinderData%MinPoint,   .TRUE. )
  WRITE(FileUnit,*)

  WRITE(FileUnit,'(A)',ADVANCE='No') 'Lower   = '
  CALL WritePoint( FileUnit, RootFinderData%LowerPoint, .FALSE. )
  WRITE(FileUnit,*)

  WRITE(FileUnit,'(A)',ADVANCE='No') 'Upper   = '
  CALL WritePoint( FileUnit, RootFinderData%UpperPoint, .FALSE. )
  WRITE(FileUnit,*)

  WRITE(FileUnit,'(A)',ADVANCE='No') 'Max     = '
  CALL WritePoint( FileUnit, RootFinderData%MaxPoint,   .TRUE. )
  WRITE(FileUnit,*)

  RETURN
END SUBROUTINE  DebugRootFinder


SUBROUTINE WriteRootFinderStatus( FileUnit, RootFinderData )
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Dimitri Curtil (LBNL)
          !       DATE WRITTEN   May 2006
          !       MODIFIED
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)                     :: FileUnit   ! File unit where to write the status description
  TYPE(RootFinderDataType), INTENT(IN)    :: RootFinderData ! Data used by root finding algorithm

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          ! na

          ! FLOW:

  SELECT CASE ( RootFinderData%StatusFlag )
  CASE (iStatusOK)
    WRITE(FileUnit,'(A)') 'Found unconstrained root'
  CASE (iStatusOKMin)
    WRITE(FileUnit,'(A)') 'Found min constrained root'
  CASE (iStatusOKMax)
    WRITE(FileUnit,'(A)') 'Found max constrained root'
  CASE (iStatusOKRoundOff )
    WRITE(FileUnit,'(A)') 'Detected round-off convergence in bracket'

  CASE (iStatusWarningSingular )
    WRITE(FileUnit,'(A)') 'Detected singularity warning'
  CASE (iStatusWarningNonMonotonic )
    WRITE(FileUnit,'(A)') 'Detected non-monotonicity warning'

  CASE (iStatusErrorRange)
    WRITE(FileUnit,'(A)') 'Detected out-of-range error'
  CASE (iStatusErrorBracket)
    WRITE(FileUnit,'(A)') 'Detected bracket error'
  CASE (iStatusErrorSlope)
    WRITE(FileUnit,'(A)') 'Detected slope error'
  CASE (iStatusErrorSingular)
    WRITE(FileUnit,'(A)') 'Detected singularity error'

  CASE DEFAULT
    WRITE(FileUnit,'(A)') 'Detected bad root finder status'
  END SELECT

  RETURN
END SUBROUTINE WriteRootFinderStatus

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

END MODULE RootFinder
