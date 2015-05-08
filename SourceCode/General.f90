MODULE General

  ! Module containing routines for general use

  ! MODULE INFORMATION:
  !       AUTHOR         Fred Buhl, Linda Lawrie
  !       DATE WRITTEN   December 2001
  !       MODIFIED       na
  !       RE-ENGINEERED  na

  ! PURPOSE OF THIS MODULE:
  ! contains routines (most likely numeric) that may be needed in several parts
  ! of EnergyPlus

  ! METHODOLOGY EMPLOYED:
  ! na

  ! REFERENCES: none

  ! OTHER NOTES: none

  ! USE STATEMENTS:
USE DataPrecisionGlobals

IMPLICIT NONE         ! Enforce explicit typing of all variables

PRIVATE ! Everything private unless explicitly made public

  ! This module should not contain variables in the module sense as it is
  ! intended strictly to provide "interfaces" to routines used by other
  ! parts of the simulation.

  ! MODULE PARAMETER DEFINITIONS
  ! na

  ! DERIVED TYPE DEFINITIONS
  ! na

  ! INTERFACE DEFINITIONS
INTERFACE RoundSigDigits
  MODULE PROCEDURE rRoundSigDigits, iRoundSigDigits
END INTERFACE RoundSigDigits
INTERFACE TrimSigDigits
  MODULE PROCEDURE rTrimSigDigits, iTrimSigDigits
END INTERFACE TrimSigDigits

  ! MODULE VARIABLE DECLARATIONS:
  ! na

  !SUBROUTINE SPECIFICATIONS FOR MODULE General
PUBLIC  SolveRegulaFalsi
PUBLIC  InterpSw
PUBLIC  InterpBlind
PUBLIC  InterpSlatAng
PUBLIC  InterpProfAng
PUBLIC  InterpProfSlatAng
PUBLIC  BlindBeamBeamTrans
PUBLIC  POLYF
PUBLIC  POLY1F  ! Not currently used in EnergyPlus (Dec 2001)
PUBLIC  POLY2F  ! Not currently used in EnergyPlus (Dec 2001)
PUBLIC  TrimSigDigits ! used for better formatting of numeric variables
PUBLIC  RoundSigDigits ! used for better formatting of numeric variables
PUBLIC  RemoveTrailingZeros ! used for better formating of numeric variables
PUBLIC  MovingAvg ! used for smoothing a sequence of data by calculating a moving average
PUBLIC  ProcessDateString  ! Used by ScheduleManager and WeatherManager
PRIVATE ValidateMonthDay   ! Used internally here (ProcessDateString)
PUBLIC  InvJulianDay       ! Used by ScheduleManager and WeatherManager and internally here
PUBLIC  JulianDay          ! Used by ScheduleManager and WeatherManager
PUBLIC  BetweenDates       ! Used by WeatherManager and "StormWindows"
PUBLIC  CreateSysTimeIntervalString  ! Used in error messages for System Time Interval
PUBLIC  SafeDivide   ! Prevents divide by zero.
!PUBLIC  SaveCompDesWaterFlow
PUBLIC  Invert3By3Matrix  ! Not currently used (June 2004)
PUBLIC  Iterate
PUBLIC  EncodeMonDayHrMin
PUBLIC  DetermineMinuteForReporting
PUBLIC  DecodeMonDayHrMin
PUBLIC  FindNumberinList
PUBLIC  LogicalToInteger
PUBLIC  CreateHVACTimeIntervalString
PUBLIC  CreateTimeString
PUBLIC  GetCurrentHVACTime
PUBLIC  GetPreviousHVACTime
PRIVATE ParseTime
PUBLIC  ScanForReports
PUBLIC  ReallocateRealArray
PUBLIC  CheckCreatedZoneItemName
!PUBLIC  ErfFunction

CONTAINS

RECURSIVE SUBROUTINE SolveRegulaFalsi(Eps, MaxIte, Flag, XRes, f, X_0, X_1, Par)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Michael Wetter
          !       DATE WRITTEN   March 1999
          !       MODIFIED       Fred Buhl November 2000, R. Raustad October 2006 - made subroutine RECURSIVE
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Find the value of x between x0 and x1 such that f(x,[,Par])
          ! is equal to zero.

          ! METHODOLOGY EMPLOYED:
          ! Uses the Regula Falsi (false position) method (similar to secant method)

          ! REFERENCES:
          ! See Press et al., Numerical Recipes in Fortran, Cambridge University Press,
          ! 2nd edition, 1992. Page 347 ff.

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  REAL(r64), INTENT(IN)     :: Eps    ! required absolute accuracy
  INTEGER, INTENT(IN)  :: MaxIte ! maximum number of allowed iterations
  INTEGER, INTENT(OUT) :: Flag   ! integer storing exit status
                                 ! = -2: f(x0) and f(x1) have the same sign
                                 ! = -1: no convergence
                                 ! >  0: number of iterations performed
  REAL(r64), INTENT(OUT)    :: XRes   ! value of x that solves f(x [,Par]) = 0
  REAL(r64), INTENT(IN)     :: X_0    ! 1st bound of interval that contains the solution
  REAL(r64), INTENT(IN)     :: X_1    ! 2nd bound of interval that contains the solution
  REAL(r64), DIMENSION(:), INTENT(IN), OPTIONAL :: Par ! array with additional parameters used for function evaluation
                                                  ! optional
          ! SUBROUTINE PARAMETER DEFINITIONS:
  REAL(r64), PARAMETER :: SMALL = 1.d-10

          ! INTERFACE BLOCK SPECIFICATIONS
  INTERFACE ! Interface to function to be solved for zero: f(X, Par) = 0
    FUNCTION f(X, Par) RESULT (Y)
      USE DataPrecisionGlobals
      REAL(r64), INTENT(IN) :: X
      REAL(r64), INTENT(IN), DIMENSION(:), OPTIONAL :: Par
      REAL(r64)        :: Y
    END FUNCTION
  END INTERFACE

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64) :: X0           ! present 1st bound
  REAL(r64) :: X1           ! present 2nd bound
  REAL(r64) :: XTemp        ! new estimate
  REAL(r64) :: Y0           ! f at X0
  REAL(r64) :: Y1           ! f at X1
  REAL(r64) :: YTemp        ! f at XTemp
  REAL(r64) :: DY          ! DY = Y0 - Y1
  LOGICAL :: Conv          ! flag, true if convergence is achieved
  LOGICAL :: StopMaxIte    ! stop due to exceeding of maximum # of iterations
  LOGICAL :: Cont          ! flag, if true, continue searching
  INTEGER  :: NIte         ! number of interations

  X0 = X_0
  X1 = X_1
  Conv       = .FALSE.
  StopMaxIte = .FALSE.
  Cont       = .TRUE.
  NIte = 0

  IF (PRESENT(Par)) THEN
    Y0 = f(X0, Par)
    Y1 = f(X1, Par)
  ELSE
    Y0 = f(X0)
    Y1 = f(X1)
  END IF
  ! check initial values
  IF ( Y0*Y1 > 0 ) THEN
    Flag = -2
    XRes = X0
    RETURN
  END IF

  DO WHILE (Cont)

    DY = Y0 - Y1
    IF (ABS(DY) < SMALL)    DY = SMALL
    ! new estimation
    XTemp = (Y0 * X1 - Y1 * X0 ) / DY
    IF (PRESENT(Par)) THEN
      YTemp = f(XTemp, Par)
    ELSE
      YTemp = f(XTemp)
    END IF

    NIte = NIte + 1

    ! check convergence
    IF (ABS(YTemp) < Eps) Conv = .TRUE.

    IF (NIte > MaxIte) StopMaxIte = .TRUE.

    IF ((.NOT.Conv).AND.(.NOT.StopMaxIte)) THEN
      Cont = .TRUE.
    ELSE
      Cont = .FALSE.
    END IF

    IF (Cont) THEN

    ! reassign values (only if further iteration required)
      IF ( Y0 < 0.0d0 ) THEN
        IF ( YTemp < 0.0d0 ) THEN
          X0 = XTemp
          Y0 = YTemp
        ELSE
          X1 = XTemp
          Y1 = YTemp
        END IF
      ELSE
        IF ( YTemp < 0.0d0 ) THEN
          X1 = XTemp
          Y1 = YTemp
        ELSE
          X0 = XTemp
          Y0 = YTemp
        END IF
      END IF ! ( Y0 < 0 )

    END IF ! (Cont)

  END DO ! Cont

  IF (Conv) THEN
    Flag = NIte
  ELSE
    Flag = -1
  END IF
  XRes = XTemp

RETURN

END SUBROUTINE SolveRegulaFalsi

REAL(r64) FUNCTION InterpSw(SwitchFac,A,B)
          ! FUNCTION INFORMATION:
          !       AUTHOR         Fred Winkelmann
          !       DATE WRITTEN   February 1999

          ! PURPOSE OF THIS FUNCTION:
          ! For switchable glazing, calculates a weighted average of properties
          ! A and B

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
REAL(r64), INTENT(IN)  :: SwitchFac ! Switching factor: 0.0 if glazing is unswitched, = 1.0 if fully switched
REAL(r64), INTENT(IN)  :: A        ! Glazing property in unswitched state
REAL(r64), INTENT(IN)  :: B        ! Glazing property in fully switched state

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
REAL(r64) :: locSwitchFac
! bound SwitchFac
locSwitchFac = MIN(SwitchFac, 1.d0)
locSwitchFac = MAX(locSwitchFac, 0.d0)

InterpSw = (1.0d0-locSwitchFac)*A + locSwitchFac*B
RETURN
END FUNCTION InterpSw

REAL(r64) FUNCTION InterpBlind(ProfAng,PropArray)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Winkelmann
          !       DATE WRITTEN   May 2001
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Does profile-angle interpolation of window blind solar-thermal properties

          ! METHODOLOGY EMPLOYED:
          ! Linear interpolation.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataGlobals, ONLY: Pi,PiOvr2

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
REAL(r64), INTENT(IN)                :: ProfAng    ! Profile angle (rad)
REAL(r64), INTENT(IN), DIMENSION(37) :: PropArray  ! Array of blind properties

          ! FUNCTION PARAMETER DEFINITIONS:
REAL(r64), PARAMETER    :: DeltaAngRad =  Pi/36.d0  ! Profile angle increment (rad)

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
REAL(r64) InterpFac      ! Interpolation factor
INTEGER IAlpha      ! Profile angle index

IF(ProfAng > PiOvr2 .OR. ProfAng < -PiOvr2) THEN
  InterpBlind = 0.0d0
ELSE
  IAlpha = 1 + INT((ProfAng+PiOvr2)/DeltaAngRad)
  InterpFac = (ProfAng - (-PiOvr2 + DeltaAngRad*(IAlpha-1)))/DeltaAngRad
  InterpBlind = (1.d0-InterpFac)*PropArray(IAlpha) + InterpFac*PropArray(IAlpha+1)
END IF
RETURN
END FUNCTION InterpBlind

REAL(r64) FUNCTION InterpProfAng(ProfAng,PropArray)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Winkelmann
          !       DATE WRITTEN   May 2001
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Does profile-angle interpolation of window blind solar-thermal properties

          ! METHODOLOGY EMPLOYED:
          ! Linear interpolation.

          ! REFERENCES:na

          ! USE STATEMENTS:
  USE DataGlobals, ONLY: Pi,PiOvr2

IMPLICIT NONE

          ! FUNCTION ARGUMENT DEFINITIONS:
REAL(r64),INTENT(IN) :: PropArray(37)  ! Array of blind properties
REAL(r64),INTENT(IN) :: ProfAng        ! Profile angle (rad)

          ! FUNCTION PARAMETER DEFINITIONS:
REAL(r64), PARAMETER    :: DeltaAngRad =  Pi/36.d0  ! Profile angle increment (rad)

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
REAL(r64) InterpFac      ! Interpolation factor
INTEGER IAlpha      ! Profile angle index

! DeltaAng = Pi/36
IF(ProfAng > PiOvr2 .OR. ProfAng < -PiOvr2) THEN
  InterpProfAng = 0.0d0
ELSE
  IAlpha = 1 + INT((ProfAng+PiOvr2)/DeltaAngRad)
  InterpFac = (ProfAng - (-PiOvr2 + DeltaAngRad*(IAlpha-1)))/DeltaAngRad
  InterpProfAng = (1.0d0-InterpFac)*PropArray(IAlpha) + InterpFac*PropArray(IAlpha+1)
END IF
RETURN
END FUNCTION InterpProfAng

REAL(r64) FUNCTION InterpSlatAng(SlatAng,VarSlats,PropArray)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Winkelmann
          !       DATE WRITTEN   Dec 2001
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Does slat-angle interpolation of window blind solar-thermal properties that
          ! do not depend on profile angle

          ! METHODOLOGY EMPLOYED:
          ! Linear interpolation.

          ! REFERENCES:na

          ! USE STATEMENTS:
  USE DataGlobals, ONLY: Pi,PiOvr2
  USE DataSurfaces,   ONLY : MaxSlatAngs

IMPLICIT NONE

          ! FUNCTION ARGUMENT DEFINITIONS:
REAL(r64), INTENT(IN)   :: SlatAng        ! Slat angle (rad)
LOGICAL,INTENT(IN) :: VarSlats    ! True if slat angle is variable
REAL(r64),INTENT(IN)    :: PropArray(MaxSlatAngs) ! Array of blind properties as function of slat angle

          ! FUNCTION PARAMETER DEFINITIONS:
REAL(r64), PARAMETER    :: DeltaAng =  Pi/(REAL(MaxSlatAngs,r64)-1.d0)

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
REAL(r64) InterpFac      ! Interpolation factor
INTEGER IBeta       ! Slat angle index
REAL(r64) SlatAng1

IF(SlatAng > Pi .OR. SlatAng < 0.0d0) THEN
!  InterpSlatAng = 0.0
!  RETURN
!END IF
  SlatAng1 = MIN(MAX(SlatAng,0.0d0),PI)
ELSE
  SlatAng1 = SlatAng
END IF

IF(VarSlats) THEN  ! Variable-angle slats
  IBeta = 1 + INT(SlatAng1/DeltaAng)
  InterpFac = (SlatAng1 - DeltaAng*(IBeta-1))/DeltaAng
  InterpSlatAng = PropArray(IBeta) + &
       InterpFac*(PropArray(MIN(MaxSlatAngs,IBeta+1))-PropArray(IBeta))
ELSE               ! Fixed-angle slats or shade
  InterpSlatAng = PropArray(1)
END IF

RETURN
END FUNCTION InterpSlatAng

REAL(r64) FUNCTION InterpProfSlatAng(ProfAng,SlatAng,VarSlats,PropArray)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Winkelmann
          !       DATE WRITTEN   Dec 2001
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Does simultaneous profile-angle and slat-angle interpolation of window
          ! blind solar-thermal properties that depend on profile angle and slat angle

          ! METHODOLOGY EMPLOYED:
          ! Linear interpolation.

          ! REFERENCES:na

          ! USE STATEMENTS:
  USE DataGlobals, ONLY: Pi,PiOvr2
  USE DataSurfaces,   ONLY : MaxSlatAngs

IMPLICIT NONE

          ! FUNCTION ARGUMENT DEFINITIONS:
REAL(r64),INTENT(IN   ) :: ProfAng        ! Profile angle (rad)
REAL(r64),INTENT(IN)    :: SlatAng        ! Slat angle (rad)
LOGICAL,INTENT(IN) :: VarSlats    ! True if variable-angle slats
REAL(r64),INTENT(IN)    :: PropArray(37,MaxSlatAngs) ! Array of blind properties

          ! FUNCTION PARAMETER DEFINITIONS:
REAL(r64), PARAMETER    :: DeltaProfAng = Pi/36.d0
REAL(r64), PARAMETER    :: DeltaSlatAng = Pi/(REAL(MaxSlatAngs,r64)-1.d0)

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
REAL(r64) ProfAngRatio   ! Profile angle interpolation factor
REAL(r64) SlatAngRatio   ! Slat angle interpolation factor
INTEGER IAlpha      ! Profile angle index
INTEGER IBeta       ! Slat angle index
REAL(r64) Val1,Val2,Val3,Val4 ! Property values at points enclosing the given ProfAngle and SlatAngle
REAL(r64) ValA,ValB      ! Property values at given SlatAngle to be interpolated in profile angle
REAL(r64) SlatAng1
REAL(r64) ProfAng1

IF(SlatAng > Pi .OR. SlatAng < 0.0d0 .OR. ProfAng > PiOvr2 .OR. ProfAng < -PiOvr2) THEN
!  InterpProfSlatAng = 0.0
!  RETURN
  SlatAng1 = MIN(MAX(SlatAng,0.0d0),PI)

  ! This is not correct, fixed 2/17/2010
  !ProfAng1 = MIN(MAX(SlatAng,-PiOvr2),PiOvr2)
  ProfAng1 = MIN(MAX(ProfAng,-PiOvr2),PiOvr2)
ELSE
  SlatAng1 = SlatAng
  ProfAng1 = ProfAng
END IF

IAlpha = INT((ProfAng1+PiOvr2)/DeltaProfAng) + 1
ProfAngRatio = (ProfAng1 + PiOvr2 - (IAlpha-1)*DeltaProfAng)/DeltaProfAng

IF(VarSlats) THEN  ! Variable-angle slats: interpolate in profile angle and slat angle
  IBeta  = INT(SlatAng1/DeltaSlatAng) + 1
  SlatAngRatio = (SlatAng1 - (IBeta-1)*DeltaSlatAng)/DeltaSlatAng
  Val1 = PropArray(IAlpha,IBeta)
  Val2 = PropArray(IAlpha,MIN(MaxSlatAngs,IBeta+1))
  Val3 = PropArray(MIN(37,IAlpha+1),IBeta)
  Val4 = PropArray(MIN(37,IAlpha+1),MIN(MaxSlatAngs,IBeta+1))
  ValA = Val1 + SlatAngRatio*(Val2-Val1)
  ValB = Val3 + SlatAngRatio*(Val4-Val3)
  InterpProfSlatAng = ValA + ProfAngRatio*(ValB-ValA)
ELSE      ! Fixed-angle slats: interpolate only in profile angle
  Val1 = PropArray(IAlpha,1)
  Val2 = PropArray(MIN(37,IAlpha+1),1)
  InterpProfSlatAng = Val1 + ProfAngRatio*(Val2-Val1)
END IF

RETURN
END FUNCTION InterpProfSlatAng

REAL(r64) FUNCTION BlindBeamBeamTrans(ProfAng,SlatAng,SlatWidth,SlatSeparation,SlatThickness)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Fred Winkelmann
          !       DATE WRITTEN   Jan 2002
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Calculates beam-to-beam transmittance of a window blind

          ! METHODOLOGY EMPLOYED:
          ! Based on solar profile angle and slat geometry

          ! REFERENCES:na

          ! USE STATEMENTS:
  USE DataGlobals, ONLY: Pi,PiOvr2

IMPLICIT NONE

          ! FUNCTION ARGUMENT DEFINITIONS:
REAL(r64),INTENT(IN) :: ProfAng        ! Solar profile angle (rad)
REAL(r64),INTENT(IN) :: SlatAng        ! Slat angle (rad)
REAL(r64),INTENT(IN) :: SlatWidth      ! Slat width (m)
REAL(r64),INTENT(IN) :: SlatSeparation ! Slat separation (distance between surfaces of adjacent slats) (m)
REAL(r64),INTENT(IN) :: SlatThickness  ! Slat thickness (m)

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
REAL(r64) fEdge          ! Slat edge correction factor
REAL(r64) wbar           ! Intermediate variable
REAL(r64) gamma          ! Intermediate variable
REAL(r64) fEdge1         ! Intermediate variable
REAL(r64) CosProfAng     ! Cosine of profile angle

CosProfAng = COS(ProfAng)
gamma = SlatAng - ProfAng
wbar = SlatSeparation
IF(CosProfAng /= 0.0d0) wbar = SlatWidth * COS(gamma)/CosProfAng
BlindBeamBeamTrans = MAX(0.0d0,1.0d0-ABS(wbar/SlatSeparation))

IF(BlindBeamBeamTrans > 0.0d0) THEN

  ! Correction factor that accounts for finite thickness of slats. It is used to modify the
  ! blind transmittance to account for reflection and absorption by the slat edges.
  ! fEdge is ratio of area subtended by edge of slat to area between tops of adjacent slats.

  fEdge  = 0.0d0
  fEdge1 = 0.0d0
  IF(ABS(SIN(gamma))>0.01d0) THEN
    IF((SlatAng > 0.0d0 .AND. SlatAng <= PiOvr2 .AND. ProfAng <= SlatAng) .OR. &
       (SlatAng > PiOvr2 .AND. SlatAng <= Pi .AND. ProfAng > -(Pi-SlatAng))) &
      fEdge1 = SlatThickness * ABS(SIN(gamma)) / &
                  ((SlatSeparation + SlatThickness/ABS(SIN(SlatAng)))*CosProfAng)
    fEdge = MIN(1.0d0,ABS(fEdge1))
  END IF
  BlindBeamBeamTrans = BlindBeamBeamTrans * (1.0d0-fEdge)

END IF

END FUNCTION BlindBeamBeamTrans

REAL(r64) FUNCTION POLYF(X,A)
          ! FUNCTION INFORMATION:
          !       AUTHOR         Fred Winkelmann
          !       DATE WRITTEN   February 1999
          !       DATE MODIFIED  October 1999, FW: change to 6th order polynomial over
          !                        entire incidence angle range

          ! PURPOSE OF THIS FUNCTION:
          ! Evaluates glazing beam transmittance or absorptance of the form
          ! A(1)*X + A(2)*X^2 + A(3)*X^3 + A(4)*X^4 + A(5)*X^5 + A(6)*X^6
          ! where X is the cosine of the angle of incidence (0.0 to 1.0)

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
REAL(r64), INTENT(IN)                :: X   ! Cosine of angle of incidence
REAL(r64), DIMENSION(6), INTENT(IN)  :: A   ! Polynomial coefficients

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
          ! na

IF(X < 0.0d0 .OR. X > 1.0d0) THEN
  POLYF = 0.0d0
ELSE
  POLYF = X*(A(1)+X*(A(2)+X*(A(3)+X*(A(4)+X*(A(5)+X*A(6))))))
END IF
RETURN
END FUNCTION POLYF

REAL(r64) FUNCTION POLY1F(X,A,N)

          ! FUNCTION INFORMATION:
          !       AUTHOR         George N. Walton
          !       DATE WRITTEN   May 1977
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function evaluates a polynomial of the form:
          ! POLY = A(1) + A(2)*X + A(3)*X**2 + ... + A(N)*X**(N-1)

          ! METHODOLOGY EMPLOYED:
          ! Uses Horner's Rule.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER  N     ! number of terms in polynomial
  REAL(r64)     X     ! independent variable
  REAL(r64)     A(N)  ! array of polynomial coefficients

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER  I    ! Loop parameter
  REAL(r64)     SUM  ! Temporary summation variable

  SUM=A(N)
  DO I=2,N
    SUM=SUM*X+A(N-I+1)
  ENDDO

  POLY1F=SUM

  RETURN

END FUNCTION POLY1F

REAL(r64) FUNCTION POLY2F(X,A,N)
          ! FUNCTION INFORMATION:
          !       AUTHOR         George N. Walton
          !       DATE WRITTEN   May 1977
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function evaluates a polynomial of the form:
          ! POLY = A(1)*X + A(2)*X**2 + ... + A(N)*X**N

          ! METHODOLOGY EMPLOYED:
          ! Uses Horner's Rule.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER  N     ! number of terms in polynomial
  REAL(r64)     X     ! independent variable
  REAL(r64)     A(N)  ! array of polynomial coefficients

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER  I    ! Loop parameter
  REAL(r64)     SUM  ! Temporary summation variable

  SUM=A(N)*X
  DO I=2,N
    SUM=X*(SUM+A(N-I+1))
  ENDDO

  POLY2F=SUM

  RETURN

END FUNCTION POLY2F

FUNCTION rTrimSigDigits(RealValue,SigDigits) RESULT(OutputString)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   March 2002
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function accepts a number as parameter as well as the number of
          ! significant digits after the decimal point to report and returns a string
          ! that is appropriate.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  !USE, INTRINSIC :: IEEE_ARITHMETIC, ONLY : IEEE_IS_NAN ! Use IEEE_IS_NAN when GFortran supports it

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  REAL(r64), INTENT(IN) :: RealValue
  INTEGER, INTENT(IN) :: SigDigits
  CHARACTER(len=32) OutputString

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER DotPos               ! Position of decimal point in original string
  INTEGER EPos                 ! Position of E in original string format xxEyy
  INTEGER SLen                 ! Length of String (w/o E part)
  CHARACTER(len=32) String     ! Working string
  CHARACTER(len=10) EString    ! E string retained from original string
  LOGICAL IncludeDot           ! True when decimal point output

  IF (RealValue /= 0.0d0) THEN
    WRITE(String,*) RealValue
  ELSE
    String='0.000000000000000000000000000'
  ENDIF
  EPos=INDEX(String,'E')
  IF (EPos > 0) THEN
    EString=String(EPos:)
    String(EPos:)=' '
  ELSE
    EString=' '
  ENDIF
  DotPos=INDEX(String,'.')
  SLen=LEN_TRIM(String)
  IF (SigDigits > 0 .or. EString /= ' ') THEN
    IncludeDot=.true.
  ELSE
    IncludeDot=.false.
  ENDIF
  IF (IncludeDot) THEN
    String=String(1:MIN(DotPos+SigDigits,SLen))//EString
  ELSE
    String=String(1:DotPos-1)
  ENDIF
  IF (ISNAN(RealValue)) THEN ! Use IEEE_IS_NAN when GFortran supports it
    String='NAN'
  ENDIF
  OutputString=ADJUSTL(String)

  RETURN

END FUNCTION rTrimSigDigits

FUNCTION iTrimSigDigits(IntegerValue,SigDigits) RESULT(OutputString)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   March 2002
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function accepts a number as parameter as well as the number of
          ! significant digits after the decimal point to report and returns a string
          ! that is appropriate.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: IntegerValue
  INTEGER, INTENT(IN), OPTIONAL :: SigDigits  ! ignored
  CHARACTER(len=32) OutputString

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  CHARACTER(len=32) String     ! Working string

  WRITE(String,*) IntegerValue
  OutputString=ADJUSTL(String)

  RETURN

END FUNCTION iTrimSigDigits

FUNCTION rRoundSigDigits(RealValue,SigDigits) RESULT(OutputString)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   March 2002
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function accepts a number as parameter as well as the number of
          ! significant digits after the decimal point to report and returns a string
          ! that is appropriate.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  !USE, INTRINSIC :: IEEE_ARITHMETIC, ONLY : IEEE_IS_NAN ! Use IEEE_IS_NAN when GFortran supports it

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  REAL(r64), INTENT(IN) :: RealValue
  INTEGER, INTENT(IN) :: SigDigits
  CHARACTER(len=32) OutputString

          ! FUNCTION PARAMETER DEFINITIONS:
  CHARACTER(len=11) :: DigitChar='01234567890'

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER DotPos               ! Position of decimal point in original string
  INTEGER EPos                 ! Position of E in original string format xxEyy
  INTEGER TPos                 ! Position of Testchar in Digit string
  INTEGER NPos                 ! Position of "next" char in Digit String
  INTEGER TPos1                ! Position of "next" char rounded in Digit string
  INTEGER SPos                 ! Actual string position being replaced
  INTEGER SLen                 ! Length of String (w/o E part)
  CHARACTER(len=32) String     ! Working string
  CHARACTER(len=10) EString    ! E string retained from original string
  CHARACTER(len=1) TestChar    ! Test character (digit) for rounding, if position in digit string > 5 (digit is 5 or greater)
                               ! then will round
  CHARACTER(len=1) Char2Rep    ! Character (digit) to be replaced
  LOGICAL IncludeDot           ! True when decimal point output

  IF (RealValue /= 0.0d0) THEN
    WRITE(String,*) RealValue
  ELSE
    String='0.000000000000000000000000000'
  ENDIF
  EPos=INDEX(String,'E')
  IF (EPos > 0) THEN
    EString=String(EPos:)
    String(Epos:)=' '
  ELSE
    EString=' '
  ENDIF

  DotPos=INDEX(String,'.')
  TestChar=String(DotPos+SigDigits+1:DotPos+SigDigits+1)
  TPos=INDEX(DigitChar,TestChar)

  IF (SigDigits == 0) THEN
    SPos=DotPos-1
  ELSE
    SPos=DotPos+SigDigits
  ENDIF

  IF (TPos > 5) THEN  ! Must round to next Digit
    Char2Rep=String(SPos:SPos)
    NPos=INDEX(DigitChar,Char2Rep)
    String(SPos:SPos)=DigitChar(NPos+1:NPos+1)
    DO WHILE (NPos == 10)
        ! Must change other char too
      IF (SigDigits == 1) THEN
        TestChar=String(SPos-2:SPos-2)
        IF (TestChar == '.') THEN
          TestChar=String(SPos-3:SPos-3)
          SPos=SPos-2
        ENDIF
        IF (TestChar == ' ') TestChar='0'  ! all 999s
        TPos1=INDEX(DigitChar,TestChar)
        String(SPos-2:SPos-2)=DigitChar(TPos1+1:TPos1+1)
      ELSE
        TestChar=String(SPos-1:SPos-1)
        IF (TestChar == '.') THEN
          TestChar=String(SPos-2:SPos-2)
          SPos=SPos-1
        ENDIF
        IF (TestChar == ' ') TestChar='0'  ! all 999s
        TPos1=INDEX(DigitChar,TestChar)
        String(SPos-1:SPos-1)=DigitChar(TPos1+1:TPos1+1)
      ENDIF
      SPos=SPos-1
      NPos=TPos1
    ENDDO
  ENDIF

  SLen=LEN_TRIM(String)
  IF (SigDigits > 0 .or. EString /= ' ') THEN
    IncludeDot=.true.
  ELSE
    IncludeDot=.false.
  ENDIF
  IF (IncludeDot) THEN
    String=String(1:MIN(DotPos+SigDigits,SLen))//EString
  ELSE
    String=String(1:DotPos-1)
  ENDIF
  IF (ISNAN(RealValue)) THEN ! Use IEEE_IS_NAN when GFortran supports it
    String='NAN'
  ENDIF
  OutputString=ADJUSTL(String)

  RETURN

END FUNCTION rRoundSigDigits

FUNCTION iRoundSigDigits(IntegerValue,SigDigits) RESULT(OutputString)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   March 2002
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function accepts a number as parameter as well as the number of
          ! significant digits after the decimal point to report and returns a string
          ! that is appropriate.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: IntegerValue
  INTEGER, INTENT(IN), OPTIONAL :: SigDigits  ! ignored
  CHARACTER(len=32) OutputString

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  CHARACTER(len=32) String     ! Working string

  WRITE(String,*) IntegerValue
  OutputString=ADJUSTL(String)

  RETURN

END FUNCTION iRoundSigDigits

FUNCTION RemoveTrailingZeros(InputString) RESULT(ResultString)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   September 2005
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! Remove trailing zeroes from output strings.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN)    :: InputString
  CHARACTER(len=LEN(InputString)) :: ResultString

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER Pos


  Pos=SCAN(InputString,'ED')

  IF (Pos == 0) THEN
    Pos=SCAN(InputString,'.')
    IF (Pos /= 0) THEN
      Pos=SCAN(InputString,'123456789.',.true.)
      IF (Pos > 0) THEN
        ResultString=InputString(1:Pos)
      ELSE
        ResultString='0.'
      ENDIF
    ELSE  ! no decimal, an integer.  leave as is
      ResultString=InputString
    ENDIF
  ELSE  ! for now, ignore x.xExx
    ResultString=InputString
  ENDIF

  RETURN

END FUNCTION RemoveTrailingZeros

SUBROUTINE MovingAvg(DataIn,NumDataItems,NumItemsInAvg,SmoothedData)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   January 2003
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Smooth the data in the 1-d array DataIn by averaging over a window NumItemsInAvg
          ! wide. Return the results in the 1-d array SmoothedData

          ! METHODOLOGY EMPLOYED:
          ! Note that DataIn and SmoothedData should have the same size. This is the reponsibility
          ! of the calling routine. NumItemsInAvg should be no bigger than the size of DataIn.

          ! REFERENCES:
          ! na.

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) ::             NumDataItems               ! number of values in DataIn
  REAL(r64), INTENT(IN), DIMENSION(NumDataItems) ::  DataIn          ! input data that needs smoothing
  INTEGER, INTENT(IN) ::             NumItemsInAvg              ! number of items in the averaging window
  REAL(r64), INTENT(OUT), DIMENSION(NumDataItems) :: SmoothedData    ! output data after smoothing

          ! SUBROUTINE PARAMETER DEFINITIONS:

          ! INTERFACE BLOCK SPECIFICATIONS

          ! DERIVED TYPE DEFINITIONS

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: i             ! loop index
  INTEGER :: j             ! inner loop index
  REAL(r64), DIMENSION(:), ALLOCATABLE :: TempData ! a scratch array

  ALLOCATE(TempData(3*NumDataItems))

  DO i=1,NumDataItems
    TempData(i) = DataIn(i)
    TempData(NumDataItems+i) = DataIn(i)
    TempData(2*NumDataItems+i) = DataIn(i)
    SmoothedData(i) = 0.0d0
  END DO

  DO i=1,NumDataItems
    DO j=1,NumItemsInAvg
      SmoothedData(i) = SmoothedData(i) + TempData(NumDataItems+i-NumItemsInAvg+j)
    END DO
    SmoothedData(i) = SmoothedData(i) / REAL(NumItemsInAvg,r64)
  END DO

  DEALLOCATE(TempData)

  RETURN

END SUBROUTINE MovingAvg

SUBROUTINE ProcessDateString(String,PMonth,PDay,PWeekDay,DateType,ErrorsFound,PYear)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   December 1999
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine will process a date from a string and determine
          ! the proper month and day for that date string.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: ProcessNumber
  USE DataStringGlobals
  USE DataInterfaces, ONLY: ShowSevereError

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: String
  INTEGER, INTENT(OUT) :: PMonth
  INTEGER, INTENT(OUT) :: PDay
  INTEGER, INTENT(OUT) :: PWeekDay
  INTEGER, INTENT(OUT) :: DateType      ! DateType found (-1=invalid, 1=month/day, 2=nth day in month, 3=last day in month)
  LOGICAL, INTENT(INOUT) :: ErrorsFound
  INTEGER, INTENT(OUT), OPTIONAL :: PYear

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER FstNum
  LOGICAL ErrFlag
  INTEGER NumTokens
  INTEGER TokenDay
  INTEGER TokenMonth
  INTEGER TokenWeekDay
  INTEGER TokenYear

  FstNum=INT(ProcessNumber(String,ErrFlag))
  DateType=-1
  IF (.not. ErrFlag) THEN
    ! Entered single number, do inverse JDay
    IF (FstNum == 0) THEN
      PMonth=0
      PDay=0
      DateType=1
    ELSEIF (FstNum < 0 .or. FstNum > 366) THEN
      CALL ShowSevereError('Invalid Julian date Entered='//TRIM(String))
      ErrorsFound=.true.
    ELSE
      CALL InvJulianDay(FstNum,PMonth,PDay,0)
      DateType=1
    ENDIF
  ELSE
    ! Error when processing as number, try x/x
    IF (.not. PRESENT(PYear)) THEN
      CALL DetermineDateTokens(String,NumTokens,TokenDay,TokenMonth,TokenWeekDay,DateType,ErrorsFound)
    ELSE
      CALL DetermineDateTokens(String,NumTokens,TokenDay,TokenMonth,TokenWeekDay,DateType,ErrorsFound,TokenYear)
      PYear=TokenYear
    ENDIF
    IF (DateType == 1) THEN
      PDay=TokenDay
      PMonth=TokenMonth
    ELSEIF (DateType == 2 .or. DateType == 3) THEN
      ! interpret as TokenDay TokenWeekDay in TokenMonth
      PDay=TokenDay
      PMonth=TokenMonth
      PWeekDay=TokenWeekDay
    ENDIF
  ENDIF

  RETURN

END SUBROUTINE ProcessDateString

SUBROUTINE DetermineDateTokens(String,NumTokens,TokenDay,TokenMonth,TokenWeekday,DateType,ErrorsFound,TokenYear)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   August 2000
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is invoked for date fields that appear to be strings (give
          ! error when ProcessNumber is used).

          ! METHODOLOGY EMPLOYED:
          ! Delete everything that is extraneous to the date information needed.  Process what
          ! is left.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataGlobals, ONLY: MaxNameLength
  USE DataInterfaces, ONLY: ShowSevereError
  USE InputProcessor, ONLY: FindItemInList,ProcessNumber

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: String
  INTEGER, INTENT(OUT) :: NumTokens     ! Number of tokens found in string
  INTEGER, INTENT(OUT) :: TokenDay      ! Value of numeric field found
  INTEGER, INTENT(OUT) :: TokenMonth    ! Value of Month field found (1=Jan, 2=Feb, etc)
  INTEGER, INTENT(OUT) :: TokenWeekDay  ! Value of Weekday field found (1=Sunday, 2=Monday, etc), 0 if none
  INTEGER, INTENT(OUT) :: DateType      ! DateType found (-1=invalid, 1=month/day, 2=nth day in month, 3=last day in month)
  LOGICAL, INTENT(INOUT) :: ErrorsFound   ! Set to true if cannot process this string as a date
  INTEGER, INTENT(OUT), OPTIONAL :: TokenYear ! Value of Year if one appears to be present and this argument is present

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=1), PARAMETER :: Blank=' '
  INTEGER, PARAMETER :: NumSingleChars=3
  CHARACTER(len=1), PARAMETER, DIMENSION(NumSingleChars) :: SingleChars=(/"/",":","-"/)
  INTEGER, PARAMETER :: NumDoubleChars=6
  CHARACTER(len=3), PARAMETER, DIMENSION(NumDoubleChars) :: DoubleChars=(/"ST ","ND ","RD ","TH ","OF ","IN "/)
  CHARACTER(len=*), PARAMETER, DIMENSION(12) :: Months=(/"JAN","FEB","MAR","APR","MAY", &
                    "JUN","JUL","AUG","SEP","OCT","NOV","DEC"/)
  CHARACTER(len=*), PARAMETER, DIMENSION(7) :: Weekdays=(/"SUN","MON","TUE","WED","THU","FRI","SAT"/)
  CHARACTER(len=*), PARAMETER :: Numbers="0123456789"


          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  CHARACTER(len=MaxNameLength) :: CurrentString
  INTEGER Pos
  INTEGER Loop
  CHARACTER(len=15), DIMENSION(3) :: Fields
  INTEGER NumField1
  INTEGER NumField2
  INTEGER NumField3
  LOGICAL ErrFlag
  LOGICAL InternalError
  LOGICAL :: WkDayInMonth

  CurrentString=String
  NumTokens=0
  TokenDay=0
  TokenMonth=0
  TokenWeekday=0
  DateType=-1
  InternalError=.false.
  WkDayInMonth=.false.
  IF (PRESENT(TokenYear)) TokenYear=0
  ! Take out separator characters, other extraneous stuff

  DO Loop=1,NumSingleChars
    Pos=INDEX(CurrentString,SingleChars(Loop))
    DO WHILE (Pos > 0)
      CurrentString(Pos:Pos)=' '
      Pos=INDEX(CurrentString,SingleChars(Loop))
    ENDDO
  ENDDO

  DO Loop=1,NumDoubleChars
    Pos=INDEX(CurrentString,DoubleChars(Loop))
    DO WHILE (Pos > 0)
      CurrentString(Pos:Pos+1)='  '
      Pos=INDEX(CurrentString,DoubleChars(Loop))
      WkDayInMonth=.true.
    ENDDO
  ENDDO

  CurrentString=ADJUSTL(CurrentString)
  IF (CurrentString == Blank) THEN
    CALL ShowSevereError('Invalid date field='//TRIM(String))
    ErrorsFound=.true.
  ELSE
    Loop=0
    DO WHILE (Loop < 3)  ! Max of 3 fields
      IF (CurrentString == Blank) EXIT
      Pos=INDEX(CurrentString,' ')
      Loop=Loop+1
      Fields(Loop)=CurrentString(1:Pos-1)
      CurrentString=CurrentString(Pos:)
      CurrentString=ADJUSTL(CurrentString)
    ENDDO
    IF (CurrentString /= Blank) THEN
      CALL ShowSevereError('Invalid date field='//TRIM(String))
      ErrorsFound=.true.
    ELSEIF (Loop == 2) THEN
      ! Field must be Day Month or Month Day (if both numeric, mon / day)
      InternalError=.false.
      NumField1=INT(ProcessNumber(Fields(1),ErrFlag))
      IF (ErrFlag) THEN
        ! Month day, but first field is not numeric, 2nd must be
        NumField2=INT(ProcessNumber(Fields(2),ErrFlag))
        IF (ErrFlag) THEN
          CALL ShowSevereError('Invalid date field='//TRIM(String))
          InternalError=.true.
        ELSE
          TokenDay=NumField2
        ENDIF
        TokenMonth=FindItemInList(Fields(1)(1:3),Months,12)
        CALL ValidateMonthDay(String,TokenDay,TokenMonth,InternalError)
        IF (.not. InternalError) THEN
          DateType=1
        ELSE
          ErrorsFound=.true.
        ENDIF
      ELSE
        ! Month Day, first field was numeric, if 2nd is, then it's month<num> day<num>
        NumField2=INT(ProcessNumber(Fields(2),ErrFlag))
        IF (.not. ErrFlag) THEN
          TokenMonth=NumField1
          TokenDay=NumField2
          CALL ValidateMonthDay(String,TokenDay,TokenMonth,InternalError)
          IF (.not. InternalError) THEN
            DateType=1
          ELSE
            ErrorsFound=.true.
          ENDIF
        ELSE  ! 2nd field was not numeric.  Must be Month
          TokenDay=NumField1
          TokenMonth=FindItemInList(Fields(2)(1:3),Months,12)
          CALL ValidateMonthDay(String,TokenDay,TokenMonth,InternalError)
          IF (.not. InternalError) THEN
            DateType=1
            NumTokens=2
          ELSE
            ErrorsFound=.true.
          ENDIF
        ENDIF
      ENDIF
    ELSEIF (Loop == 3) THEN
      ! Field must be some combination of <num> Weekday Month (if WkDayInMonth true)
      IF (WkDayInMonth) THEN
        NumField1=INT(ProcessNumber(Fields(1),ErrFlag))
        IF (.not. ErrFlag) THEN ! the expected result
          TokenDay=NumField1
          TokenWeekDay=FindItemInList(Fields(2)(1:3),Weekdays,7)
          IF (TokenWeekDay == 0) THEN
            TokenMonth=FindItemInList(Fields(2)(1:3),Months,12)
            TokenWeekDay=FindItemInList(Fields(3)(1:3),Weekdays,7)
            IF (TokenMonth == 0 .or. TokenWeekDay == 0) InternalError=.true.
          ELSE
            TokenMonth=FindItemInList(Fields(3)(1:3),Months,12)
            IF (TokenMonth == 0) InternalError=.true.
          ENDIF
          DateType=2
          NumTokens=3
          IF (TokenDay < 0 .or. TokenDay > 5) InternalError=.true.
        ELSE   ! first field was not numeric....
          IF (Fields(1) == 'LA ') THEN
            DateType=3
            NumTokens=3
            TokenWeekDay=FindItemInList(Fields(2)(1:3),Weekdays,7)
            IF (TokenWeekDay == 0) THEN
              TokenMonth=FindItemInList(Fields(2)(1:3),Months,12)
              TokenWeekDay=FindItemInList(Fields(3)(1:3),Weekdays,7)
              IF (TokenMonth == 0 .or. TokenWeekDay == 0) InternalError=.true.
            ELSE
              TokenMonth=FindItemInList(Fields(3)(1:3),Months,12)
              IF (TokenMonth == 0) InternalError=.true.
            ENDIF
          ELSE  ! error....
            CALL ShowSevereError('First date field not numeric, field='//TRIM(String))
          ENDIF
        ENDIF
      ELSE  ! mm/dd/yyyy or yyyy/mm/dd
        NumField1=INT(ProcessNumber(Fields(1),ErrFlag))
        NumField2=INT(ProcessNumber(Fields(2),ErrFlag))
        NumField3=INT(ProcessNumber(Fields(3),ErrFlag))
        DateType=1
        ! error detection later..
        IF (NumField1 > 100) THEN
          IF (PRESENT(TokenYear)) THEN
            TokenYear=NumField1
          ENDIF
          TokenMonth=NumField2
          TokenDay=NumField3
        ELSEIF (NumField3 > 100) THEN
          IF (PRESENT(TokenYear)) THEN
            TokenYear=NumField3
          ENDIF
          TokenMonth=NumField1
          TokenDay=NumField2
        ENDIF
      ENDIF
    ELSE
      ! Not enough or too many fields
      CALL ShowSevereError('Invalid date field='//TRIM(String))
      ErrorsFound=.true.
    ENDIF
  ENDIF

  IF (InternalError) THEN
    DateType=-1
    ErrorsFound=.true.
  ENDIF

  RETURN

END SUBROUTINE DetermineDateTokens

SUBROUTINE ValidateMonthDay(String,Day,Month,ErrorsFound)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   August 2000
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine validates a potential Day, Month values, produces an error
          ! message when not valid, and sets error flag.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataInterfaces, ONLY: ShowSevereError

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: String  ! REAL(r64) string being processed
  INTEGER, INTENT(IN) :: Day
  INTEGER, INTENT(IN) :: Month
  LOGICAL, INTENT(OUT) :: ErrorsFound

          ! SUBROUTINE PARAMETER DEFINITIONS:
  INTEGER, PARAMETER, DIMENSION(12) :: EndMonthDay=(/31,29,31,30,31,30,31,31,30,31,30,31/)

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  LOGICAL InternalError

  InternalError=.false.
  IF (Month < 1 .or. Month > 12) InternalError=.true.
  IF (.not. InternalError) THEN
    IF (Day < 1 .or. Day > EndMonthDay(Month)) InternalError=.true.
  ENDIF
  IF (InternalError) THEN
    CALL ShowSevereError('Invalid Month Day date format='//TRIM(String))
    ErrorsFound=.true.
  ELSE
    ErrorsFound=.false.
  ENDIF


  RETURN

END SUBROUTINE ValidateMonthDay

INTEGER FUNCTION JulianDay (Month,Day,LeapYearValue)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   September 1997
          !       MODIFIED       na
          !       RE-ENGINEERED  from JDAYF in BLAST/IBLAST

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine returns the appropriate Julian Day value for the input
          ! Month and Day.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: Month         ! Month, 1..12
  INTEGER, INTENT(IN) :: Day           ! Day of Month, not validated by month
  INTEGER, INTENT(IN) :: LeapYearValue ! 1 if leap year indicated, 0 if not
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER, DIMENSION(12) :: EndDayofMonth = (/31,59,90,120,151,181,212,243,273,304,334,365/)
                            ! End day numbers of each month (without Leap Year)
!
      SELECT CASE (Month)

      CASE(1)
!                                       CASE 1: JANUARY
        JulianDay = Day

      CASE(2)
!                                       CASE 2: FEBRUARY
        JulianDay = Day + EndDayofMonth(1)

      CASE(3:12)
!                                       CASE 3: REMAINING MONTHS
        JulianDay= Day + EndDayofMonth(Month-1) + LeapYearValue

      CASE DEFAULT
        JulianDay=0

      END SELECT

      RETURN

END FUNCTION JulianDay

SUBROUTINE InvJulianDay(Number,PMonth,PDay,LeapYr)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   December 1999
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine performs and inverse Julian Day
          ! calculation, using an input JulianDay and returning
          ! appropriate Month and Day.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)  :: Number
  INTEGER, INTENT(OUT) :: PMonth
  INTEGER, INTENT(OUT) :: PDay
  INTEGER, INTENT(IN)  :: LeapYr

          ! SUBROUTINE PARAMETER DEFINITIONS:
  INTEGER, PARAMETER, DIMENSION(0:12) :: EndOfMonth=(/0,31,59,90,120,151,181,212,243,273,304,334,365/)

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER WMonth
  INTEGER LeapAddPrev
  INTEGER LeapAddCur

  IF (Number < 0 .or. Number > 366) RETURN
  DO WMonth=1,12
    IF (WMonth == 1) THEN
      LeapAddPrev=0
      LeapAddCur=0
    ELSEIF (WMonth == 2) THEN
      LeapAddPrev=0
      LeapAddCur=LeapYr
    ELSE
      LeapAddPrev=LeapYr
      LeapAddCur=LeapYr
    ENDIF
    IF (Number > (EndOfMonth(WMonth-1)+LeapAddPrev) .and. Number <= (EndOfMonth(WMonth)+LeapAddCur)) EXIT
  ENDDO
  PMonth=WMonth
  PDay=Number-(EndOfMonth(WMonth-1)+LeapAddCur)


  RETURN

END SUBROUTINE InvJulianDay

LOGICAL FUNCTION BetweenDates(TestDate,StartDate,EndDate)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   June 2000
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function returns true if the TestDate is between
          ! (StartDate <= TestDate <= EndDate).

          ! METHODOLOGY EMPLOYED:
          ! The input dates are Julian Day format, year is irrelevant.
          ! Thus, if StartDate > EndDate (i.e. StartDate = 1Dec and EndDate = 31Jan),
          ! this routine accomodates.


          ! REFERENCES:
          ! Adapted from BLAST BTWEEN function.

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
          INTEGER, INTENT(IN) :: TestDate  ! Date to test
          INTEGER, INTENT(IN) :: StartDate ! Start date in sequence
          INTEGER, INTENT(IN) :: EndDate   ! End date in sequence

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
          ! na

  BetweenDates=.false.  ! Default case

  IF (StartDate <= EndDate) THEN  ! Start Date <= End Date
    IF (TestDate >= StartDate .and. TestDate <= EndDate) BetweenDates=.true.
  ELSE  ! EndDate <= StartDate
    IF (TestDate <= EndDate .or. TestDate >= StartDate) BetweenDates=.true.
  ENDIF

  RETURN

END FUNCTION BetweenDates

FUNCTION CreateSysTimeIntervalString() RESULT(OutputString)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   April 2003
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function creates the current time interval of the system
          ! time step.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataGlobals,     ONLY: CurrentTime,TimeStepZone
  USE DataHVACGlobals, ONLY: TimeStepSys,SysTimeElapsed

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=70) :: OutputString

          ! FUNCTION PARAMETER DEFINITIONS:
  CHARACTER(len=*),PARAMETER :: TstmpFmt="(I2.2,':',F3.0)"
  CHARACTER(len=*),PARAMETER :: TstmpFmti="(I2.2,':',I2.2)"
  REAL(r64),PARAMETER :: FracToMin=60.0d0

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  REAL(r64) ActualTimeS  ! Start of current interval (HVAC time step)
  REAL(r64) ActualTimeE  ! End of current interval (HVAC time step)
  INTEGER ActualTimeHrS
!  INTEGER ActualTimeHrE
  CHARACTER(len=10) TimeStmpS  ! Character representation of start of interval
  CHARACTER(len=10) TimeStmpE  ! Character representation of end of interval
  integer ActualTimeMinS


!  ActualTimeS=INT(CurrentTime)+(SysTimeElapsed+(CurrentTime - INT(CurrentTime)))
! CR6902  ActualTimeS=INT(CurrentTime-TimeStepZone)+SysTimeElapsed
! [DC] TODO: Improve display accuracy up to fractional seconds using hh:mm:ss.0 format
  ActualTimeS = CurrentTime-TimeStepZone+SysTimeElapsed
  ActualtimeE = ActualTimeS+TimeStepSys
  ActualTimeHrS=INT(ActualTimeS)
!  ActualTimeHrE=INT(ActualTimeE)
  ActualTimeMinS=NINT((ActualTimeS - ActualTimeHrS)*FracToMin)

  IF (ActualTimeMinS == 60) then
    ActualTimeHrS=ActualTimeHrS+1
    ActualTimeMinS = 0
  ENDIF
  WRITE(TimeStmpS,TStmpFmti) ActualTimeHrS,ActualTimeMinS

  WRITE(TimeStmpE,TStmpFmt) INT(ActualTimeE),(ActualTimeE - INT(ActualTimeE))*FracToMin
  IF (TimeStmpE(4:4) == ' ') TimeStmpE(4:4)='0'
  TimeStmpE=ADJUSTL(TimeStmpE)
  TimeStmpE(6:6)=' '

  OutputString=TRIM(TimeStmpS)//' - '//TRIM(TimeStmpE)

  RETURN

END FUNCTION CreateSysTimeIntervalString

FUNCTION SafeDivide(a, b) RESULT (c)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! returns a / b while preventing division by zero
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  REAL(r64), INTENT(IN) :: a, b
  REAL(r64) :: c

  REAL(r64), PARAMETER :: SMALL=1.D-10

  IF (ABS(b) >= SMALL) THEN
    c = a / b
  ELSE
    c = a / SIGN(SMALL, b)
  END IF
END FUNCTION

!SUBROUTINE SaveCompDesWaterFlow(WaterInletNodeNum,DesWaterFlow)
!
!          ! SUBROUTINE INFORMATION:
!          !       AUTHOR         Fred Buhl
!          !       DATE WRITTEN   January 2004
!          !       MODIFIED       na
!          !       RE-ENGINEERED  na
!
!          ! PURPOSE OF THIS SUBROUTINE:
!          ! Save the design water flow rates of those components using water as an energy source
!          ! or sink in an array that can be accessed by the water loop managers for sizing calculations.
!
!          ! METHODOLOGY EMPLOYED:
!          ! The design flow rate is stored in a dynamic array along with the water inlet node number
!          ! (which is used by the water loops as a component identifier instead if name and type).
!
!          ! REFERENCES:
!          ! na
!
!          ! USE STATEMENTS:
!  USE DataSizing
!
!  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine
!
!          ! SUBROUTINE ARGUMENT DEFINITIONS:
!  INTEGER :: WaterInletNodeNum ! the component's water inlet node number (condenser side for water / water compoennts)
!  REAL(r64)    :: DesWaterFlow      ! the component's design water flow rate [m3/s]
!
!          ! SUBROUTINE PARAMETER DEFINITIONS:
!          ! na
!
!          ! INTERFACE BLOCK SPECIFICATIONS:
!          ! na
!
!          ! DERIVED TYPE DEFINITIONS:
!          ! na
!
!          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
!  TYPE (CompDesWaterFlowData), ALLOCATABLE, DIMENSION(:) :: CompDesWaterFlow0 ! scratch array to store components'
!                                                                            ! design water flow rate
!  INTEGER :: WaterCompNum ! component do loop index
!
!  NumWaterComps = NumWaterComps + 1 ! increment the number of components that use water as a source of heat or coolth
!  ! save the existing data in a scratch array
!  IF (NumWaterComps > 1) THEN
!    ALLOCATE(CompDesWaterFlow0(NumWaterComps-1))
!    DO WaterCompNum=1,NumWaterComps-1
!      CompDesWaterFlow0(WaterCompNum)%SupNode = CompDesWaterFlow(WaterCompNum)%SupNode
!      CompDesWaterFlow0(WaterCompNum)%DesVolFlowRate = CompDesWaterFlow(WaterCompNum)%DesVolFlowRate
!    END DO
!    ! get rid of the old array
!    DEALLOCATE(CompDesWaterFlow)
!  END IF
!  ! allocate a new array
!  ALLOCATE(CompDesWaterFlow(NumWaterComps))
!  ! save the new data
!  CompDesWaterFlow(NumWaterComps)%SupNode = WaterInletNodeNum
!  CompDesWaterFlow(NumWaterComps)%DesVolFlowRate = DesWaterFlow
!  ! move the old data back from the scratch array
!  IF (NumWaterComps > 1) THEN
!    DO WaterCompNum=1,NumWaterComps-1
!      CompDesWaterFlow(WaterCompNum)%SupNode = CompDesWaterFlow0(WaterCompNum)%SupNode
!      CompDesWaterFlow(WaterCompNum)%DesVolFlowRate = CompDesWaterFlow0(WaterCompNum)%DesVolFlowRate
!    END DO
!    DEALLOCATE(CompDesWaterFlow0)
!  END IF
!
!  RETURN
!
!END SUBROUTINE SaveCompDesWaterFlow

SUBROUTINE Invert3By3Matrix(A,InverseA)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         George Walton
          !       DATE WRITTEN   August 1976
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine computes the inverse of a 3x3 matrix by the
          ! cofactor method.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataGlobals, ONLY: OutputFileStandard
  USE DataInterfaces, ONLY: ShowFatalError

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  REAL(r64), INTENT(IN)  ::  A(3,3)       ! Input 3X3 Matrix
  REAL(r64), INTENT(OUT) :: InverseA(3,3) ! Output 3X3 Matrix - Inverse Of A

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
 REAL(r64) :: Determinant ! Determinant of Matrix A

          ! Compute Determinant

  Determinant = A(1,1)*A(2,2)*A(3,3)+A(1,2)*A(2,3)*A(3,1)+A(1,3)*A(2,1)*A(3,2) &
       -A(1,1)*A(3,2)*A(2,3)-A(2,1)*A(1,2)*A(3,3)-A(3,1)*A(2,2)*A(1,3)

  IF (ABS(Determinant) < .1D-12) THEN
    CALL ShowFatalError('Determinant = [Zero] in Invert3By3Matrix',OutputFileStandard)
  END IF

          ! Compute Inverse

  InverseA(1,1) = (A(2,2)*A(3,3)-A(3,2)*A(2,3))/Determinant
  InverseA(2,1) = (A(3,1)*A(2,3)-A(2,1)*A(3,3))/Determinant
  InverseA(3,1) = (A(2,1)*A(3,2)-A(3,1)*A(2,2))/Determinant
  InverseA(1,2) = (A(3,2)*A(1,3)-A(1,2)*A(3,3))/Determinant
  InverseA(2,2) = (A(1,1)*A(3,3)-A(3,1)*A(1,3))/Determinant
  InverseA(3,2) = (A(3,1)*A(1,2)-A(1,1)*A(3,2))/Determinant
  InverseA(1,3) = (A(1,2)*A(2,3)-A(2,2)*A(1,3))/Determinant
  InverseA(2,3) = (A(2,1)*A(1,3)-A(1,1)*A(2,3))/Determinant
  InverseA(3,3) = (A(1,1)*A(2,2)-A(2,1)*A(1,2))/Determinant

  RETURN

END SUBROUTINE Invert3By3Matrix

SUBROUTINE ITERATE(ResultX,Tol,X0,Y0,X1,Y1,Iter,Cnvg)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Liesen
          !       DATE WRITTEN   March 2004
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Iterately solves for the value of X which satisfies Y(X)=0.
          ! The subroutine tests for convergence and provides a new guess for the value of the
          ! independent variable X.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! Linear Correction based on the RegulaFalsi routine in EnergyPlus

          ! USE STATEMENTS:
  USE DataPrecisionGlobals
!unused0909  use dataglobals, only: outputfiledebug

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  REAL(r64), INTENT(Out)   :: ResultX     !ResultX is the final Iteration result passed back to the calling routine
  REAL(r64), INTENT(In)    :: Tol         !Tolerance for Convergence
  REAL(r64), INTENT(InOut) :: X0          !Current value of X
  REAL(r64), INTENT(InOut) :: Y0          !Current value of the function Y(X)
  REAL(r64), INTENT(InOut) :: X1          !First Previous values of X
  REAL(r64), INTENT(InOut) :: Y1          !First Previous values of Y(X1)

  Integer, Intent(In) :: Iter        !Number of iterations
  Integer, Intent(Out):: Cnvg        !Convergence flag  Cnvg = 0:  Not converged
                                     !                  Cnvg = 1:  Converged

          ! SUBROUTINE PARAMETER DEFINITIONS:
  REAL(r64), PARAMETER :: small  = 1.d-9 !Small Number used to approximate zero
  REAL(r64), PARAMETER :: Perturb=0.1d0  !Perturbation applied to X to initialize iteration

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

  REAL(r64) :: Dy                         !Linear fit result

          ! FLOW:

          ! Check for convergence by comparing change in X
      IF (Iter .ne. 1) THEN
        IF (ABS(X0-X1) .LT. Tol .OR. Y0 .EQ. 0.0d0) THEN
          ResultX = X0
          Cnvg=1
          RETURN
        ENDIF
      ENDIF

          ! Not converged.
      Cnvg=0
      IF (Iter .EQ. 1) THEN

          ! New guess is specified by Perturb
        IF (ABS(X0) .GT. small) THEN
          ResultX = X0*(1.0d0+Perturb)
        ELSE
          ResultX = Perturb
        ENDIF

      ELSE

         ! New guess calculated from LINEAR FIT of most recent two points
         DY = Y0 - Y1
         IF (ABS(DY) < SMALL)    DY = SMALL
         ! new estimation

         ResultX = (Y0 * X1 - Y1 * X0 ) / DY

      ENDIF

        X1=X0
        Y1=Y0

      RETURN

END SUBROUTINE ITERATE

INTEGER FUNCTION FindNumberinList(WhichNumber,ListofItems,NumItems)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   September 2001
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function looks up a number(integer) in a similar list of
          ! items and returns the index of the item in the list, if
          ! found.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: WhichNumber
  INTEGER, INTENT(IN), DIMENSION(*) :: ListofItems
  INTEGER, INTENT(IN) :: NumItems

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER Count

  FindNumberinList=0

  DO Count=1,NumItems
    IF (WhichNumber == ListofItems(Count)) THEN
      FindNumberinList=Count
      EXIT
    ENDIF
  END DO

  RETURN

END FUNCTION FindNumberinList

SUBROUTINE DecodeMonDayHrMin(Item,Month,Day,Hour,Minute)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   March 2000
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine decodes the "packed" integer representation of
          ! the Month, Day, Hour, and Minute.  Packed integers are used to
          ! save memory allocation.  Original idea for this routine is contained
          ! in DECMDH, BLAST code, by Jean Baugh.

          ! METHODOLOGY EMPLOYED:
          ! Using maximum integer concept the original date can be decoded
          ! from the packed single word.  This relies on 4 byte integer representation
          ! as a minimum (capable of representing up to 2,147,483,647).

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)  :: Item   ! word containing encoded month, day, hour, minute
                                 ! ((month*100 + day)*100 + hour)*100 + minute
  INTEGER, INTENT(OUT) :: Month  ! month in integer format (1-12)
  INTEGER, INTENT(OUT) :: Day    ! day in integer format (1-31)
  INTEGER, INTENT(OUT) :: Hour   ! hour in integer format (1-24)
  INTEGER, INTENT(OUT) :: Minute ! minute in integer format (0:59)

          ! SUBROUTINE PARAMETER DEFINITIONS:
  INTEGER, PARAMETER :: DecMon=100*100*100
  INTEGER, PARAMETER :: DecDay=100*100
  INTEGER, PARAMETER :: DecHr =100

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER TmpItem

  TmpItem=Item
  Month=TmpItem/DecMon
  TmpItem=(TmpItem-Month*DecMon)
  Day=TmpItem/DecDay
  TmpItem=TmpItem-Day*DecDay
  Hour=TmpItem/DecHr
  Minute=MOD(TmpItem,DecHr)

  RETURN

END SUBROUTINE DecodeMonDayHrMin

FUNCTION DetermineMinuteForReporting(IndexTypeKey) RESULT(ActualTimeMin)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   January 2012
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! When reporting peaks, minutes are used but not necessarily easily calculated.

          ! METHODOLOGY EMPLOYED:
          ! Could use the access to the minute as OP (OutputProcessor) does but uses
          ! external calculation.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataPrecisionGlobals
  USE DataGlobals, ONLY: ZoneTSReporting, HVACTSReporting, TimeStepZone, CurrentTime
  USE DataHVACGlobals, ONLY: TimeStepSys, SysTimeElapsed


  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: IndexTypeKey ! kind of reporting, Zone Timestep or System
  INTEGER             :: ActualTimeMin    ! calculated Minute for reporting

          ! FUNCTION PARAMETER DEFINITIONS:
  REAL(r64),PARAMETER :: FracToMin=60.0d0

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  real(r64) ActualTimeS  ! Start of current interval (HVAC time step)
  real(r64) ActualTimeE  ! End of current interval (HVAC time step)
  integer ActualTimeHrS

  if (IndexTypeKey == HVACTSReporting) then
    ActualTimeS = CurrentTime-TimeStepZone+SysTimeElapsed
    ActualtimeE = ActualTimeS+TimeStepSys
    ActualTimeHrS=INT(ActualTimeS)
    ActualTimeMin=NINT((ActualtimeE - ActualTimeHrS)*FracToMin)
  else
    ActualTimeMin = (CurrentTime - INT(CurrentTime))*FracToMin
  endif

  RETURN

END FUNCTION DetermineMinuteForReporting

SUBROUTINE EncodeMonDayHrMin(Item,Month,Day,Hour,Minute)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   March 2000
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine encodes the "packed" integer representation of
          ! the Month, Day, Hour, and Minute.  Packed integers are used to
          ! save memory allocation.  Original idea for this routine is contained
          ! in DECMDH, BLAST code, by Jean Baugh.

          ! METHODOLOGY EMPLOYED:
          ! Using maximum integer concept the original date can be decoded
          ! from the packed single word.  This relies on 4 byte integer representation
          ! as a minimum (capable of representing up to 2,147,483,647).

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(OUT) :: Item   ! word containing encoded month, day, hour, minute
                                 ! ((month*100 + day)*100 + hour)*100 + minute
  INTEGER, INTENT(IN)  :: Month  ! month in integer format (1:12)
  INTEGER, INTENT(IN)  :: Day    ! day in integer format (1:31)
  INTEGER, INTENT(IN)  :: Hour   ! hour in integer format (1:24)
  INTEGER, INTENT(IN)  :: Minute ! minute in integer format (0:59)

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          ! na

  Item=((Month*100 + Day)*100 + Hour)*100 + Minute

  RETURN

END SUBROUTINE EncodeMonDayHrMin


  INTEGER FUNCTION LogicalToInteger( Flag )
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Dimitri Curtil
          !       DATE WRITTEN   November 2004
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This subroutine uses an input logical and makes
          ! an integer (true=1, false=0)

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
    LOGICAL, INTENT(IN) ::  Flag

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
          ! na

    IF ( Flag ) THEN
      LogicalToInteger = 1
    ELSE
      LogicalToInteger = 0
    ENDIF

    RETURN

  END FUNCTION LogicalToInteger

  REAL(r64) FUNCTION GetCurrentHVACTime()
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Dimitri Curtil
          !       DATE WRITTEN   November 2004
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This routine returns the time in seconds at the end of the current HVAC step.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
    USE DataGlobals,       ONLY : CurrentTime, TimeStepZone, SecInHour
    USE DataHVACGlobals,   ONLY : SysTimeElapsed, TimeStepSys

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
          ! na

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
    REAL(r64)  :: CurrentHVACTime

    ! This is the correct formula that does not use MinutesPerSystemTimeStep, which would
    ! erronously truncate all sub-minute system time steps down to the closest full minute.
    !
    ! Maybe later TimeStepZone, TimeStepSys and SysTimeElapsed could also be specified
    ! as real.
    CurrentHVACTime = (CurrentTime - TimeStepZone) + SysTimeElapsed + TimeStepSys
    GetCurrentHVACTime = CurrentHVACTime * SecInHour

    RETURN

  END FUNCTION GetCurrentHVACTime


  REAL(r64) FUNCTION GetPreviousHVACTime()
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Dimitri Curtil
          !       DATE WRITTEN   November 2004
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This routine returns the time in seconds at the beginning of the current HVAC step.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
    USE DataGlobals,       ONLY : CurrentTime, TimeStepZone, SecInHour
    USE DataHVACGlobals,   ONLY : SysTimeElapsed

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
          ! na

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
    REAL(r64)  :: PreviousHVACTime

    ! This is the correct formula that does not use MinutesPerSystemTimeStep, which would
    ! erronously truncate all sub-minute system time steps down to the closest full minute.
    PreviousHVACTime = (CurrentTime - TimeStepZone) + SysTimeElapsed
    GetPreviousHVACTime = PreviousHVACTime * SecInHour

    RETURN

  END FUNCTION GetPreviousHVACTime


  FUNCTION CreateHVACTimeIntervalString() RESULT(OutputString)

            ! FUNCTION INFORMATION:
            !       AUTHOR         Dimitri Curtil
            !       DATE WRITTEN   January 2005
            !       MODIFIED       na
            !       RE-ENGINEERED  na

            ! PURPOSE OF THIS FUNCTION:
            ! This function creates the time stamp with the current time interval for the HVAC
            ! time step.

            ! METHODOLOGY EMPLOYED:
            ! na

            ! REFERENCES:
            ! na

            ! USE STATEMENTS:
    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

            ! FUNCTION ARGUMENT DEFINITIONS:
    CHARACTER(len=32) :: OutputString

            ! FUNCTION PARAMETER DEFINITIONS:
            ! na

            ! INTERFACE BLOCK SPECIFICATIONS:
            ! na

            ! DERIVED TYPE DEFINITIONS:
            ! na

            ! FUNCTION LOCAL VARIABLE DECLARATIONS:
            ! na

    OutputString = CreateTimeIntervalString( &
      GetPreviousHVACTime(), &
      GetCurrentHVACTime() )

    RETURN

  END FUNCTION CreateHVACTimeIntervalString


  FUNCTION CreateTimeString( Time ) RESULT(OutputString)

            ! FUNCTION INFORMATION:
            !       AUTHOR         Dimitri Curtil
            !       DATE WRITTEN   January 2005
            !       MODIFIED       na
            !       RE-ENGINEERED  na

            ! PURPOSE OF THIS FUNCTION:
            ! This function creates the time stamp string from the time value specified in seconds.
            ! Inspired by similar function CreateSysTimeIntervalString() in General.f90
            ! However, this function provides better accuracy for sub-minute time steps
            ! by also showing information down to the 10th of a second.
            !
            ! Note that Time is expected to be specified in REAL(r64).

            ! METHODOLOGY EMPLOYED:
            ! na

            ! REFERENCES:
            ! na

            ! USE STATEMENTS:
            ! na

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

            ! FUNCTION ARGUMENT DEFINITIONS:
    REAL(r64), INTENT(IN) :: Time         ! Time in seconds
    CHARACTER(len=10)            :: OutputString ! Contains time stamp

            ! FUNCTION PARAMETER DEFINITIONS:
    CHARACTER(len=*), PARAMETER :: TStampFmt="(I2.2,':',I2.2,':',F4.1)"

            ! INTERFACE BLOCK SPECIFICATIONS:
            ! na

            ! DERIVED TYPE DEFINITIONS:
            ! na

            ! FUNCTION LOCAL VARIABLE DECLARATIONS:
    CHARACTER(len=10) TimeStamp  ! Character representation of time using hh:mm:ss.ssss format
    INTEGER Hours                ! Number of hours <= 24
    INTEGER Minutes              ! Remaining minutes < 60
    REAL(r64) Seconds     ! Remaining seconds < 60

    CALL ParseTime( Time, Hours, Minutes, Seconds )

    TimeStamp(:) = ' '
    ! TimeStamp written with formatting
    ! "hh:mm:ss.s"
    ! "1234567890"
    WRITE(TimeStamp,TStampFmt) Hours, Minutes, Seconds
    IF (TimeStamp(4:4) == ' ') TimeStamp(4:4)='0'
    IF (TimeStamp(7:7) == ' ') TimeStamp(7:7)='0'
    IF (TimeStamp(10:10) == ' ') TimeStamp(10:10)='0'
    TimeStamp = ADJUSTL(TimeStamp)

    OutputString = TimeStamp

! For debugging only
!WRITE(*,'(A)') '  UtilityRoutines::CreateTimeString()'
!WRITE(*,'(A,F15.10)') '    Time    = ', Time
!WRITE(*,*) '    Hours   = ', Hours
!WRITE(*,*) '    Minutes = ', Minutes
!WRITE(*,*) '    Seconds = ', Seconds
!WRITE(*,*) '    TimeStamp    = ', TimeStamp
!WRITE(*,*) '    OutputString = ', OutputString

    RETURN

  END FUNCTION CreateTimeString


  FUNCTION CreateTimeIntervalString(StartTime, EndTime) RESULT(OutputString)

            ! FUNCTION INFORMATION:
            !       AUTHOR         Dimitri Curtil
            !       DATE WRITTEN   January 2005
            !       MODIFIED       na
            !       RE-ENGINEERED  na

            ! PURPOSE OF THIS FUNCTION:
            ! This function creates the time stamp with the current time interval from start and end
            ! time values specified in seconds.
            ! Inspired by similar function CreateSysTimeIntervalString() in General.f90

            ! METHODOLOGY EMPLOYED:
            ! na

            ! REFERENCES:
            ! na

            ! USE STATEMENTS:
            ! na

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

            ! FUNCTION ARGUMENT DEFINITIONS:
    REAL(r64), INTENT(IN) :: StartTime    ! Start of current interval in seconds
    REAL(r64), INTENT(IN) :: EndTime      ! End of current interval in seconds
    CHARACTER(len=32)            :: OutputString ! Contains time stamp

            ! FUNCTION PARAMETER DEFINITIONS:
            ! na

            ! INTERFACE BLOCK SPECIFICATIONS:
            ! na

            ! DERIVED TYPE DEFINITIONS:
            ! na

            ! FUNCTION LOCAL VARIABLE DECLARATIONS:
    CHARACTER(len=10) TimeStmpS  ! Character representation of start of interval
    CHARACTER(len=10) TimeStmpE  ! Character representation of end of interval

    TimeStmpS = CreateTimeString( StartTime )
    TimeStmpS = ADJUSTL(TimeStmpS)

    TimeStmpE = CreateTimeString( EndTime )
    TimeStmpE = ADJUSTL(TimeStmpE)

    OutputString = TRIM(TimeStmpS)//' - '//TRIM(TimeStmpE)

    RETURN

  END FUNCTION CreateTimeIntervalString

  SUBROUTINE ParseTime( Time, Hours, Minutes, Seconds )
            ! FUNCTION INFORMATION:
            !       AUTHOR         Dimitri Curtil
            !       DATE WRITTEN   January 2005
            !       MODIFIED       na
            !       RE-ENGINEERED  na

            ! PURPOSE OF THIS FUNCTION:
            ! This subroutine decomposes a time value specified in seconds
            ! into a triplet { hours : minutes : seconds } such that
            ! - minutes < 60
            ! - seconds < 60

            ! METHODOLOGY EMPLOYED:
            ! na

            ! REFERENCES:
            ! na

            ! USE STATEMENTS:
            ! na

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

            ! SUBROUTINE ARGUMENT DEFINITIONS:
    REAL(r64), INTENT(IN)  :: Time         ! Time value in seconds
    INTEGER, INTENT(OUT)          :: Hours        ! Number of hours
    INTEGER, INTENT(OUT)          :: Minutes      ! Number of minutes < 60
    REAL(r64), INTENT(OUT) :: Seconds      ! Number of seconds < 60

            ! SUBROUTINE PARAMETER DEFINITIONS:
    REAL(r64), PARAMETER               :: MinToSec  = 60.0d0
    REAL(r64), PARAMETER               :: HourToSec  = MinToSec * 60.0d0

            ! INTERFACE BLOCK SPECIFICATIONS:
            ! na

            ! DERIVED TYPE DEFINITIONS:
            ! na

            ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    REAL(r64)         :: Remainder = 0.0d0


    ! Get number of hours
    ! This might undershoot the actual number of hours. See DO WHILE loop.
    Hours = INT( Time/HourToSec )

    ! Compute remainder in seconds
    Remainder = (Time - Hours*3600.0d0)

    ! Correct number of hours whenever Remainder >= 60 to fix round-off errors
    ! E.g., Time = 2.0 would return Hours=1 and Minutes=60 instead of Hours=2!
    DO WHILE ( ANINT(Remainder/MinToSec,r64) .GE. 60.0d0 )
      Hours = Hours + 1
      Remainder = (Time - Hours*3600.0d0)
    END DO


    ! Compute minutes
    Minutes = INT(Remainder/MinToSec)

    ! Compute remainder in seconds
    Remainder = (Time - Hours*3600.0d0 - Minutes*60.0d0)

    ! Correct number of minutes whenever Remainder >= 60 to fix round-off errors
    DO WHILE ( ANINT(Remainder,r64) .GE. 60.0d0 )
      Minutes = Minutes + 1
      Remainder = (Time - Hours*3600.0d0 - Minutes*60.0d0)
    END DO

    ! Compute seconds
    Seconds = Remainder

    RETURN

  END SUBROUTINE ParseTime

SUBROUTINE ScanForReports(ReportName,DoReport,ReportKey,Option1,Option2)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   March 2009
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This routine scans for the global "reports" settings, such as Variable Dictionary,
          ! Surfaces (and options), Constructions, etc.

          ! METHODOLOGY EMPLOYED:
          ! First time routine is called, all the viable combinations/settings for the reports are
          ! stored in SAVEd variables.  Later callings will retrieve those.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataIPShortCuts
  USE InputProcessor, ONLY: GetNumObjectsFound,GetObjectItem,MakeUPPERCase,FindItemInList,GetNumSectionsFound,SameString
  USE DataInterfaces, ONLY: ShowWarningError, ShowContinueError
  USE DataRuntimeLanguage, ONLY: OutputFullEMSTrace, OutputEMSErrors, OutputEMSActuatorAvailFull, &
                                 OutputEMSActuatorAvailSmall, OutputEMSInternalVarsFull, OutputEMSInternalVarsSmall
  USE DataGlobals, ONLY: ShowDecayCurvesInEIO

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: ReportName
  LOGICAL, INTENT(OUT)         :: DoReport
  CHARACTER(len=*), INTENT(IN), OPTIONAL :: ReportKey
  CHARACTER(len=*), INTENT(INOUT), OPTIONAL :: Option1
  CHARACTER(len=*), INTENT(INOUT), OPTIONAL :: Option2

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER NumReports
  INTEGER RepNum
  INTEGER NumNames
  INTEGER NumNumbers
  INTEGER IOStat
  LOGICAL,SAVE :: SurfVert    =.false.
  LOGICAL,SAVE :: SurfDet     =.false.
  LOGICAL,SAVE :: SurfDetWVert=.false.
  LOGICAL,SAVE :: DXFReport   =.false.
  CHARACTER(len=MaxNameLength),SAVE :: DXFOption1
  CHARACTER(len=MaxNameLength),SAVE :: DXFOption2
  LOGICAL,SAVE :: DXFWFReport   =.false.
  CHARACTER(len=MaxNameLength),SAVE :: DXFWFOption1
  CHARACTER(len=MaxNameLength),SAVE :: DXFWFOption2
  LOGICAL,SAVE :: VRMLReport    =.false.
  CHARACTER(len=MaxNameLength),SAVE :: VRMLOption1
  CHARACTER(len=MaxNameLength),SAVE :: VRMLOption2
  LOGICAL,SAVE :: CostInfo    =.false.
  LOGICAL,SAVE :: ViewFactorInfo    =.false.
  CHARACTER(len=MaxNameLength),SAVE :: ViewRptOption1=' '
  LOGICAL,SAVE :: Constructions    =.false.
  LOGICAL,SAVE :: Materials    =.false.
  LOGICAL,SAVE :: LineRpt    =.false.
  CHARACTER(len=MaxNameLength),SAVE :: LineRptOption1=' '
  LOGICAL,SAVE :: VarDict    =.false.
  LOGICAL, SAVE :: EMSoutput = .false.
  CHARACTER(len=MaxNameLength),SAVE :: VarDictOption1=' '
  CHARACTER(len=MaxNameLength),SAVE :: VarDictOption2=' '
!  LOGICAL,SAVE :: SchRpt = .false.
!  CHARACTER(len=MaxNameLength) :: SchRptOption
  LOGICAL,SAVE :: GetReportInput=.true.

  IF (GetReportInput) THEN

    cCurrentModuleObject='Output:Surfaces:List'

    NumReports=GetNumObjectsFound(cCurrentModuleObject)
    DO RepNum=1,NumReports
      CALL GetObjectItem(cCurrentModuleObject,RepNum,cAlphaArgs,NumNames,rNumericArgs,NumNumbers,IOStat,  &
                     AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                     AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

      SELECT CASE(cAlphaArgs(1))

        CASE ('LINES')
          LineRpt=.true.
          LineRptOption1=cAlphaArgs(2)

        CASE ('VERTICES')
          SurfVert=.true.

        CASE ('DETAILS','DETAILED','DETAIL')
          SurfDet=.true.

        CASE ('DETAILSWITHVERTICES','DETAILVERTICES')
          SurfDetWVert=.true.

        CASE ('COSTINFO')
          !   Custom case for reporting surface info for cost estimates (for first costs in opitimzing)
          CostInfo=.true.

        CASE ('VIEWFACTORINFO')  ! actual reporting is in HeatBalanceIntRadExchange
          ViewFactorInfo=.true.
          ViewRptOption1=cAlphaArgs(2)

        CASE ('DECAYCURVESFROMZONECOMPONENTLOADS') !Should the Radiant to Convective Decay Curves from the load component report appear in the EIO file
          ShowDecayCurvesInEIO = .TRUE.

        CASE (' ')
          CALL ShowWarningError(trim(cCurrentModuleObject)//': No '//trim(cAlphaFieldNames(1))//' supplied.')
          CALL ShowContinueError(' Legal values are: "Lines", "Vertices", "Details", "DetailsWithVertices", '//  &
            '"CostInfo", "ViewFactorIinfo".')

        CASE DEFAULT
          CALL ShowWarningError(trim(cCurrentModuleObject)//': Invalid '//trim(cAlphaFieldNames(1))//'="'//  &
             trim(cAlphaArgs(1))//'" supplied.')
          CALL ShowContinueError(' Legal values are: "Lines", "Vertices", "Details", "DetailsWithVertices", '//  &
            '"CostInfo", "ViewFactorIinfo".')

      END SELECT
    ENDDO

    cCurrentModuleObject='Output:Surfaces:Drawing'

    NumReports=GetNumObjectsFound(cCurrentModuleObject)
    DO RepNum=1,NumReports
      CALL GetObjectItem(cCurrentModuleObject,RepNum,cAlphaArgs,NumNames,rNumericArgs,NumNumbers,IOStat,  &
                     AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                     AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

      SELECT CASE(cAlphaArgs(1))

        CASE ('DXF')
          DXFReport=.true.
          DXFOption1=cAlphaArgs(2)
          DXFOption2=cAlphaArgs(3)

        CASE ('DXF:WIREFRAME')
          DXFWFReport=.true.
          DXFWFOption1=cAlphaArgs(2)
          DXFWFOption2=cAlphaArgs(3)

        CASE ('VRML')
          VRMLReport=.true.
          VRMLOption1=cAlphaArgs(2)
          VRMLOption2=cAlphaArgs(3)

        CASE (' ')
          CALL ShowWarningError(trim(cCurrentModuleObject)//': No '//trim(cAlphaFieldNames(1))//' supplied.')
          CALL ShowContinueError(' Legal values are: "DXF", "DXF:WireFrame", "VRML".')

        CASE DEFAULT
          CALL ShowWarningError(trim(cCurrentModuleObject)//': Invalid '//trim(cAlphaFieldNames(1))//'="'//  &
             trim(cAlphaArgs(1))//'" supplied.')
          CALL ShowContinueError(' Legal values are: "DXF", "DXF:WireFrame", "VRML".')

      END SELECT
    ENDDO

    RepNum=GetNumSectionsFound('Report Variable Dictionary')
    IF (RepNum > 0) THEN
      VarDict=.true.
      VarDictOption1='REGULAR'
      VarDictOption2=' '
    ENDIF

    cCurrentModuleObject='Output:VariableDictionary'

    NumReports=GetNumObjectsFound(cCurrentModuleObject)
    DO RepNum=1,NumReports
      CALL GetObjectItem(cCurrentModuleObject,RepNum,cAlphaArgs,NumNames,rNumericArgs,NumNumbers,IOStat,  &
                     AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                     AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
     VarDict=.true.
     VarDictOption1=cAlphaArgs(1)
     VarDictOption2=cAlphaArgs(2)

    ENDDO

    cCurrentModuleObject='Output:Constructions'
    NumReports=GetNumObjectsFound(cCurrentModuleObject)
    DO RepNum=1,NumReports
      CALL GetObjectItem(cCurrentModuleObject,RepNum,cAlphaArgs,NumNames,rNumericArgs,NumNumbers,IOStat,  &
                     AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                     AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
      IF (cAlphaArgs(1)(1:9) == 'CONSTRUCT') THEN
        Constructions=.true.
      ELSEIF (cAlphaArgs(1)(1:3) == 'MAT') THEN
        Materials=.true.
      ENDIF
      IF (NumNames > 1) THEN
        IF (cAlphaArgs(2)(1:9) == 'CONSTRUCT') THEN
          Constructions=.true.
        ELSEIF (cAlphaArgs(2)(1:3) == 'MAT') THEN
          Materials=.true.
        ENDIF
      ENDIF
    ENDDO

    cCurrentModuleObject  = 'Output:EnergyManagementSystem'
    NumReports=GetNumObjectsFound(cCurrentModuleObject)
    DO RepNum=1,NumReports
      CALL GetObjectItem(cCurrentModuleObject,RepNum,cAlphaArgs,NumNames,rNumericArgs,NumNumbers,IOStat,  &
                     AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                     AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

      EMSoutput = .TRUE.

      SELECT CASE (TRIM(cAlphaArgs(1)))

      CASE ('NONE')
        OutputEMSActuatorAvailSmall = .FALSE.
        OutputEMSActuatorAvailFull  = .FALSE.
      CASE ('NOTBYUNIQUEKEYNAMES')
        OutputEMSActuatorAvailSmall = .TRUE.
        OutputEMSActuatorAvailFull  = .FALSE.
      CASE ('VERBOSE')
        OutputEMSActuatorAvailSmall = .FALSE.
        OutputEMSActuatorAvailFull  = .TRUE.

      CASE (' ')
          CALL ShowWarningError(trim(cCurrentModuleObject)//': Blank '//trim(cAlphaFieldNames(1))//' supplied.')
          CALL ShowContinueError(' Legal values are: "None", "NotByUniqueKeyNames", "Verbose". "None" will be used.')
          OutputEMSActuatorAvailSmall = .FALSE.
          OutputEMSActuatorAvailFull  = .FALSE.
      CASE DEFAULT
          CALL ShowWarningError(trim(cCurrentModuleObject)//': Invalid '//trim(cAlphaFieldNames(1))//'="'//  &
             trim(cAlphaArgs(1))//'" supplied.')
          CALL ShowContinueError(' Legal values are: "None", "NotByUniqueKeyNames", "Verbose". "None" will be used.')
          OutputEMSActuatorAvailSmall = .FALSE.
          OutputEMSActuatorAvailFull  = .FALSE.
      END SELECT

      SELECT CASE (cAlphaArgs(2))

      CASE ('NONE')
        OutputEMSInternalVarsFull  = .FALSE.
        OutputEMSInternalVarsSmall = .FALSE.
      CASE ('NOTBYUNIQUEKEYNAMES')
        OutputEMSInternalVarsFull  = .FALSE.
        OutputEMSInternalVarsSmall = .TRUE.
      CASE ('VERBOSE')
        OutputEMSInternalVarsFull  = .TRUE.
        OutputEMSInternalVarsSmall = .FALSE.
      CASE (' ')
          CALL ShowWarningError(trim(cCurrentModuleObject)//': Blank '//trim(cAlphaFieldNames(2))//' supplied.')
          CALL ShowContinueError(' Legal values are: "None", "NotByUniqueKeyNames", "Verbose". "None" will be used.')
          OutputEMSInternalVarsFull  = .FALSE.
          OutputEMSInternalVarsSmall = .FALSE.
      CASE DEFAULT
          CALL ShowWarningError(trim(cCurrentModuleObject)//': Invalid '//trim(cAlphaFieldNames(2))//'="'//  &
             trim(cAlphaArgs(1))//'" supplied.')
          CALL ShowContinueError(' Legal values are: "None", "NotByUniqueKeyNames", "Verbose". "None" will be used.')
          OutputEMSInternalVarsFull  = .FALSE.
          OutputEMSInternalVarsSmall = .FALSE.
      END SELECT

      SELECT CASE (cAlphaArgs(3))

      CASE ('NONE')
        OutputEMSErrors    = .FALSE.
        OutputFullEMSTrace = .FALSE.
      CASE ('ERRORSONLY')
        OutputEMSErrors    = .TRUE.
        OutputFullEMSTrace = .FALSE.
      CASE ('VERBOSE')
        OutputFullEMSTrace = .TRUE.
        OutputEMSErrors    = .TRUE.
      CASE (' ')
          CALL ShowWarningError(trim(cCurrentModuleObject)//': Blank '//trim(cAlphaFieldNames(3))//' supplied.')
          CALL ShowContinueError(' Legal values are: "None", "ErrorsOnly", "Verbose". "None" will be used.')
          OutputEMSErrors    = .FALSE.
          OutputFullEMSTrace = .FALSE.
      CASE DEFAULT
          CALL ShowWarningError(trim(cCurrentModuleObject)//': Invalid '//trim(cAlphaFieldNames(3))//'="'//  &
             trim(cAlphaArgs(1))//'" supplied.')
          CALL ShowContinueError(' Legal values are: "None", "ErrorsOnly", "Verbose". "None" will be used.')
          OutputEMSErrors    = .FALSE.
          OutputFullEMSTrace = .FALSE.
      END SELECT

    ENDDO

!    cCurrentModuleObject='Output:Schedules'
!    NumReports=GetNumObjectsFound(cCurrentModuleObject)
!    DO RepNum=1,NumReports
!      CALL GetObjectItem(cCurrentModuleObject,RepNum,cAlphaArgs,NumNames,rNumericArgs,NumNumbers,IOStat,  &
!                     AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
!                     AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
!      SchRpt=.true.
!      SchRptOption=cAlphaArgs(1)
!    ENDDO

    GetReportInput=.false.

  ENDIF

  ! Process the Scan Request
  DoReport=.false.

  SELECT CASE (MakeUPPERCase(ReportName))
    CASE ('CONSTRUCTIONS')
      IF (PRESENT(ReportKey)) THEN
        IF (SameString(ReportKey,'Constructions')) DoReport=Constructions
        IF (SameString(ReportKey,'Materials')) DoReport=Materials
      ENDIF
    CASE ('VIEWFACTORINFO')
      DoReport=ViewFactorInfo
      IF (PRESENT(Option1)) Option1=ViewRptOption1
    CASE ('VARIABLEDICTIONARY')
      DoReport=VarDict
      IF (PRESENT(Option1)) Option1=VarDictOption1
      IF (PRESENT(Option2)) Option2=VarDictOption2
!    CASE ('SCHEDULES')
!     DoReport=SchRpt
!      IF (PRESENT(Option1)) Option1=SchRptOption
    CASE ('SURFACES')
      SELECT CASE (MakeUPPERCase(ReportKey)) !Objexx:OPTIONAL ReportKey used without PRESENT check
        CASE ('COSTINFO')
          DoReport=CostInfo
        CASE ('DXF')
          DoReport=DXFReport
          IF (PRESENT(Option1)) Option1=DXFOption1
          IF (PRESENT(Option2)) Option2=DXFOption2
        CASE ('DXF:WIREFRAME')
          DoReport=DXFWFReport
          IF (PRESENT(Option1)) Option1=DXFWFOption1
          IF (PRESENT(Option2)) Option2=DXFWFOption2
        CASE ('VRML')
          DoReport=VRMLReport
          IF (PRESENT(Option1)) Option1=VRMLOption1
          IF (PRESENT(Option2)) Option2=VRMLOption2
        CASE ('VERTICES')
          DoReport=SurfVert
        CASE ('DETAILS')
          DoReport=SurfDet
        CASE ('DETAILSWITHVERTICES')
          DoReport=SurfDetWVert
        CASE ('LINES')
          DoReport=LineRpt
          IF (PRESENT(Option1)) Option1=LineRptOption1
        CASE DEFAULT
      END SELECT
    CASE ('ENERGYMANAGEMENTSYSTEM')
      DoReport = EMSoutput
    CASE DEFAULT
  END SELECT

  RETURN

END SUBROUTINE ScanForReports


SUBROUTINE ReallocateRealArray(Array,ArrayMax,ArrayInc)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   October 2012
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine reallocates (preserving data) a REAL(r64) array.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  REAL(r64), ALLOCATABLE, DIMENSION(:), INTENT(INOUT) :: Array
  INTEGER, INTENT(INOUT)                            :: ArrayMax  ! Current and resultant dimension for Array
  INTEGER, INTENT(IN)                               :: ArrayInc  ! increment for redimension

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64), DIMENSION(:), ALLOCATABLE    :: NewArray

  ALLOCATE(NewArray(ArrayMax+ArrayInc))
  NewArray=0.0d0

  IF (ArrayMax > 0) THEN
    NewArray(1:ArrayMax)=Array(1:ArrayMax)
  ENDIF
  DEALLOCATE(Array)
  ArrayMax=ArrayMax+ArrayInc
  ALLOCATE(Array(ArrayMax))
  Array=NewArray
  DEALLOCATE(NewArray)

  RETURN

END SUBROUTINE ReallocateRealArray

SUBROUTINE CheckCreatedZoneItemName(calledFrom,CurrentObject,ZoneName,MaxZoneNameLength,ItemName,  &
                                     ItemNames,NumItems,ResultName,ErrFlag)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   December 2012
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This routine checks "global" objects (that is, ones with ZoneList used in the name
          ! specification) along with a specific name for the current object for length and duplication
          ! with previous objects of that class.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataGlobals,    ONLY: MaxNameLength
  USE DataInterfaces, ONLY: ShowWarningError,ShowSevereError,ShowContinueError
  USE InputProcessor, ONLY: FindItemInList

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: calledFrom        ! routine called from
  CHARACTER(len=*), INTENT(IN) :: CurrentObject     ! object being parsed
  CHARACTER(len=*), INTENT(IN) :: ZoneName          ! Zone Name associated
  INTEGER, INTENT(IN)          :: MaxZoneNameLength ! maximum length of zonelist zone names
  CHARACTER(len=*), INTENT(IN) :: ItemName          ! Item name (People, Lights, etc object)
  CHARACTER(len=*), DIMENSION(:), INTENT(IN) :: ItemNames  ! Item Names to check for duplication
  INTEGER, INTENT(IN) :: NumItems                   ! Number of items in ItemNames array
  CHARACTER(len=*), INTENT(INOUT) :: ResultName     ! Resultant name
  LOGICAL, INTENT(INOUT) :: ErrFlag                 ! Error flag set to true if error found here.

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: ItemLength
  LOGICAL :: DuplicateNameError
  INTEGER :: FoundItem
  INTEGER :: ItemNameLength
  LOGICAL :: TooLong

  ErrFlag=.false.
  DuplicateNameError=.false.
  ItemNameLength=len_trim(ItemName)
  ItemLength=len_trim(ZoneName) +  ItemNameLength
  ResultName=trim(ZoneName)//' '//ItemName
  TooLong=.false.
  IF (ItemLength > MaxNameLength) THEN
    CALL ShowWarningError(calledFrom//trim(CurrentObject)//' Combination of ZoneList and Object Name generate a name too long.')
    CALL ShowContinueError('Object Name="'//trim(ItemName)//'".')
    CALL ShowContinueError('ZoneList/Zone Name="'//trim(ZoneName)//'".')
    CALL ShowContinueError('Item length=['//trim(RoundSigDigits(ItemLength))//'] > Maximum Length=['//  &
       trim(RoundSigDigits(MaxNameLength))//']. You may need to shorten the names.')
    CALL ShowContinueError('Shortening the Object Name by ['//  &
       trim(RoundSigDigits((MaxZoneNameLength+1+ItemNameLength)-MaxNameLength))//  &
       '] characters will assure uniqueness for this ZoneList.')
    CALL ShowContinueError('name that will be used (may be needed in reporting)="'//trim(ResultName)//'".')
    TooLong=.true.
  ENDIF

  FoundItem=FindItemInList(ResultName,ItemNames,NumItems)

  IF (FoundItem /= 0) THEN
    CALL ShowSevereError(calledFrom//trim(CurrentObject)//'="'//trim(ItemName)//'", Duplicate Generated name encountered.')
    CALL ShowContinueError('name="'//trim(ResultName)//'" has already been generated or entered as '//  &
       trim(CurrentObject)//' item=['//trim(RoundSigDigits(FoundItem))//'].')
    IF (TooLong) CALL ShowContinueError('Duplicate name likely caused by the previous "too long" warning.')
    ResultName='xxxxxxx'
    ErrFlag=.true.
  ENDIF

  RETURN

END SUBROUTINE CheckCreatedZoneItemName

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

END MODULE General
