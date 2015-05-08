MODULE DataRootFinder   ! EnergyPlus Data-Only Module

          ! MODULE INFORMATION:
          !       AUTHOR         Dimitri Curtil
          !       DATE WRITTEN   February 2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS MODULE:
          ! This data-only module is a repository for variables and types used by the
          ! RootFinder module.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! OTHER NOTES:
          ! na


          ! USE STATEMENTS:
USE DataPrecisionGlobals

IMPLICIT NONE   ! Enforce explicit typing of all variables

PUBLIC          ! By definition, all variables which are placed in this data
                ! -only module should be available to other modules and routines.
                ! Thus, all variables in this module must be PUBLIC.

          ! MODULE PARAMETER DEFINITIONS
  INTEGER, PARAMETER :: iSlopeNone        = 0  ! Undefined slope specification
  INTEGER, PARAMETER :: iSlopeIncreasing  = 1  ! For overall increasing function F(X) between min and max points
  INTEGER, PARAMETER :: iSlopeDecreasing  = -1 ! For overall decreasing function F(X) between min and max points


  ! Error because the overall slope appears to be flat between the min and max points,
  ! implying that the function might be singular over the interval:
  ! F(XMin) == F(XMax)
  INTEGER, PARAMETER :: iStatusErrorSingular      = -4
  ! Error because the overall slope assumption is not observed at the min and max points:
  ! - for an increasing function F(X), we expect F(XMin) < F(XMax)  otherwise error
  ! - for a decreasing function F(X),  we expect F(XMin) > F(XMax)  otherwise error
  ! Note that this error status does not detect strict monotonicity at points
  ! between the min and max points.
  INTEGER, PARAMETER :: iStatusErrorSlope          = -3
  ! Error because the current candidate X does not lie within the current lower an upper points:
  ! X < XLower or X > XUpper
  INTEGER, PARAMETER :: iStatusErrorBracket        = -2
  ! Error because the current candidate X does not lie within the min and max points:
  ! X < XMin or X > XMax
  INTEGER, PARAMETER :: iStatusErrorRange          = -1


  INTEGER, PARAMETER :: iStatusNone       =  0   ! Indeterminate error state (not converged), also default state
  INTEGER, PARAMETER :: iStatusOK         =  1   ! Unconstrained convergence achieved with root solution so that:
                                                 ! XMin < XRoot < XMax
  INTEGER, PARAMETER :: iStatusOKMin      =  2   ! Constrained convergence achieved with solution XRoot==XMin
  INTEGER, PARAMETER :: iStatusOKMax      =  3   ! Constrained convergence achieved with solution XRoot==XMax
  INTEGER, PARAMETER :: iStatusOKRoundOff =  4   ! Reached requested tolerance in X variables although Y=F(X) does not
                                                 ! satisfy unconstrained convergence check

  INTEGER, PARAMETER :: iStatusWarningNonMonotonic = 10  ! Error because F(X) is not strictly monotonic between the
                                                         ! lower and upper points
  INTEGER, PARAMETER :: iStatusWarningSingular     = 11  ! Error because F(X) == YLower or F(X) == YUpper


  INTEGER, PARAMETER :: iMethodNone          = -1 ! No solution method (used internally only when root finder is reset)
  INTEGER, PARAMETER :: iMethodBracket       = 0  ! Bracketting mode (used internally only to bracket root)
  INTEGER, PARAMETER :: iMethodBisection     = 1  ! Step performed using bisection method (aka interval halving)
  INTEGER, PARAMETER :: iMethodFalsePosition = 2  ! Step performed using false position method (aka regula falsi)
  INTEGER, PARAMETER :: iMethodSecant        = 3  ! Step performed using secant method
  INTEGER, PARAMETER :: iMethodBrent         = 4  ! Step performed using Brent's method
  ! Names for each solution method type
  CHARACTER(LEN=*), PARAMETER, DIMENSION(-1:4) :: SolutionMethodTypes =  &
                     (/'No solution method   ',  &
                       'Bracketting method   ',  &
                       'Bisection method     ',  &
                       'False position method',  &
                       'Secant method        ',  &
                       'Brent method         '/)


          ! DERIVED TYPE DEFINITIONS
  ! Type declaration for the numerical controls.
  TYPE ControlsType
    INTEGER           :: SlopeType  = iSlopeNone   ! Set to any of the iSlope<...> codes
    INTEGER           :: MethodType = iMethodNone  ! Desired solution method.
                                ! Set to any of the iMethod<...> codes except for iMethodNone and iMethodBracket
    REAL(r64)         :: TolX   = 1.0d-3           ! Relative tolerance for variable X
    REAL(r64)         :: ATolX  = 1.0d-3           ! Absolute tolerance for variable X
    REAL(r64)         :: ATolY  = 1.0d-3           ! Absolute tolerance for variable Y
  END TYPE ControlsType

  ! Type declaration for iterate tracking.
  TYPE PointType
    LOGICAL           :: DefinedFlag = .FALSE.     ! Set to true if point has been set; false otherwise
    REAL(r64)         :: X = 0.0d0                   ! X value
    REAL(r64)         :: Y = 0.0d0                   ! Y value = F(X)
  END TYPE PointType

  ! Type declaration for the root finder solution technique.
  TYPE RootFinderDataType
    TYPE (ControlsType)       :: Controls
    INTEGER                   :: StatusFlag = iStatusNone     ! Current status of root finder
                                                ! Valid values are any of the STATUS_<code> constants
    INTEGER                   :: CurrentMethodType = iMethodNone ! Solution method used to perform current step
    REAL(r64)                 :: XCandidate = 0.0d0       ! Candidate X value to use next when evaluating F(X)
    REAL(r64)                 :: ConvergenceRate = 0.0d0  ! Convergence rate achieved over the last 2 successive iterations
    TYPE (PointType)          :: Increment              ! Increment between last 2 iterations
    TYPE (PointType)          :: MinPoint               ! Point { XMin, F(XMin) }
    TYPE (PointType)          :: MaxPoint               ! Point { XMax, F(XMax) }
    TYPE (PointType)          :: LowerPoint             ! Point { XLower, F(XLower) } so that XLower <= XRoot
    TYPE (PointType)          :: UpperPoint             ! Point { XUpper, F(XUpper) } so that XRoot <= YUpper
    TYPE (PointType)          :: CurrentPoint           ! Last evaluated point { X, F(X) }
    INTEGER                   :: NumHistory = 0         ! Number of points stored in History
    TYPE (PointType)          :: History(3)             ! Vector containing last 3 best iterates
  END TYPE RootFinderDataType

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! MODULE VARIABLE DECLARATIONS:
          ! na


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

END MODULE DataRootFinder
