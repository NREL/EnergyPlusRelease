SUBROUTINE Get_Environment_Variable(EnvName,EnvValue) !,EnvLength,EnvStatus,EnvTrim)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   May 2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Use often implemented extension routine "GetEnv" to mimic the
          ! F2003 standard "Get_Environment_Variable".

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DFPORT

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: EnvName  ! name of environment variable
  CHARACTER(len=*), INTENT(OUT), OPTIONAL :: EnvValue ! result of request
!  INTEGER, INTENT(OUT), OPTIONAL :: EnvLength ! Length of Value (in characters)
!  INTEGER, INTENT(OUT), OPTIONAL :: EnvStatus ! 0 if variable exists/assigned a value,
!                                              ! 1 if variable does not exist
!                                              ! 2 or greater -- processor/compiler dependent
!  LOGICAL, INTENT(IN),  OPTIONAL :: EnvTrim ! false if trailing blanks are significant

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  CHARACTER(len=255) :: Value

  CALL GetEnv(EnvName,Value)
  IF (PRESENT(EnvValue)) EnvValue=Value
!  IF (PRESENT(EnvLength)) EnvLength=Len_Trim(Value)
!  IF (PRESENT(EnvStatus) .and. Value /= ' ') THEN
!    EnvStatus=0
!  ELSE
!    EnvStatus=1
!  ENDIF

  RETURN

END SUBROUTINE Get_Environment_Variable

FUNCTION COMMAND_ARGUMENT_COUNT() RESULT(NumArgs)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   Feb 2007
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! Use often implemented extension routine "GetEnv" to mimic the
          ! F2003 standard "Command_Argument_Count".

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DFLIB

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER :: numargs

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
          ! na

  numargs=NARGS()-1

  RETURN

END FUNCTION COMMAND_ARGUMENT_COUNT

SUBROUTINE GET_COMMAND(command,length,status)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   Feb 2007
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! <description>

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DFLIB

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), OPTIONAL :: command
  INTEGER, OPTIONAL :: length
  INTEGER, OPTIONAL :: status

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: numargs
  CHARACTER(len=255) :: commandline
  CHARACTER(len=100) :: carg
  INTEGER :: arg

  numargs=NARGS()-1
  commandline=' '
  do arg=0,numargs
    call get_command_argument(arg,carg)
    if (arg == 0) then
      commandline=carg
    else
      commandline=' '//trim(commandline)//carg
    endif
  enddo

  if (present(command)) then
    command=commandline
  endif
  if (present(length)) then
    length=len_trim(commandline)
  endif
  if (present(status)) then
    status = -1  ! not exactly correct
  endif

  RETURN

END SUBROUTINE GET_COMMAND

SUBROUTINE GET_COMMAND_ARGUMENT(n,value,length,status)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   Feb 2007
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! <description>

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DFLIB

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER :: n
  CHARACTER(len=*), OPTIONAL :: value
  INTEGER, OPTIONAL :: length
  INTEGER, OPTIONAL :: status

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER*2 getarg_n
  CHARACTER(len=255) getarg_buffer
  INTEGER*2 getarg_status

  getarg_n=n

  CALL GETARG(getarg_n,getarg_buffer,getarg_status)

  if (present(value)) then
    value=getarg_buffer
  endif
  if (present(length)) then
    if (getarg_status > 0) then
      length=getarg_status
    else
      length=0
    endif
  endif
  if (present(status)) then
    if (getarg_status < 0) then
      status=10
    elseif (getarg_status > 0) then
      status=-1
    else
      status=0
    endif
  endif

  RETURN

END SUBROUTINE GET_COMMAND_ARGUMENT

FUNCTION ERF(Var1) RESULT(ErfValue)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Craig Wray, LBNL
          !       DATE WRITTEN   25Aug2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! Given the independent variable, returns the value of the error function

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! Geelen, B.D. 1993. "Technical Note: An Accurate Solution FORTRAN Algorithm for the erf
          ! and Related Error Functions", Advances in Engineering Software, 18, pp.67-71

          ! USE STATEMENTS:
   USE DataPrecisionGlobals
   USE DataGlobals, ONLY: PI

   IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
   REAL(r64), INTENT (IN) :: Var1        ! 1st independent variable (x) [-]
   REAL(r64)              :: ErfValue    ! Error function result [-]

          ! FUNCTION PARAMETER DEFINITIONS:
   REAL(r64), PARAMETER ::   F12 = 132.d0 * 3628800.d0 ! Twelve factorial [-]

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
   INTEGER   :: SumTermID !Summation term counter (integer) [-]
   INTEGER   :: AN
!   REAL(r64) :: AN        !Term counter (double precision) [-]
   REAL(r64) :: FACT      !Factorial number [-]
   REAL(r64) :: A1        !Numerator term 1 [-]
   REAL(r64) :: A2        !Numerator term 2 power [-]
   REAL(r64) :: A3        !Numerator [-]
   REAL(r64) :: A4        !Denominator [-]
   REAL(r64) :: A5        !Summation term [-]

   REAL(r64) :: TwoX      !Two times Var1 (X) [-]
   REAL(r64) :: B1        !Var1 squared[-]
   REAL(r64) :: B2        !Scalar numerator [-]
   REAL(r64) :: B3        !Scalar denominator [-]
   REAL(r64) :: B4        !Series scalar [-]
   REAL(r64) :: BT1       !Series term 1 [-]
   REAL(r64) :: BT2       !Series term 2 [-]
   REAL(r64) :: BT3       !Series term 3 [-]
   REAL(r64) :: BT4       !Series term 4 [-]
   REAL(r64) :: BT5       !Series term 5 [-]
   REAL(r64) :: BT6       !Series term 6 [-]
   REAL(r64) :: BT7       !Series term 7 [-]
   REAL(r64) :: BT8       !Series term 8 [-]
   REAL(r64) :: BT9       !Series term 9 [-]
   REAL(r64) :: SeriesSum !Sum: 1 + terms 1 through 9 [-]
   LOGICAL, SAVE :: firstTime=.true.
   REAL(r64),SAVE :: EXP1_Value=0.0d0  ! value of EXP(1), i.e., 2.71828182845905
   REAL(r64),SAVE :: PKON=0.0d0        ! value of 2.0/SQRT(PI), i.e. 1.12837916709551
   REAL(r64),SAVE :: PI_SQRT           ! value of SQRT(PI)

   IF (firstTime) THEN
     ! Set up some globals:
     EXP1_Value=EXP(1.0d0)
     PI_SQRT=SQRT(PI)
     PKON=2.0d0/PI_SQRT
     firstTime=.false.
   ENDIF

   IF (Var1 < -2.0d0) THEN
     ErfValue=-1.0d0
     RETURN
   ENDIF
   
   !Select region for evaluating functions
   IF(Var1 < 3.6d0) THEN !Summation solution for 0 <= Var1 < 3.6 (Geelen's equation 16): A-range
     !Initialize values
     AN = -1 !.d0
     FACT = 1.d0
     A5 = 0.d0

     DO SumTermID=1,60
       AN = AN + 1 !.d0
       FACT = FACT * REAL(AN,r64)
       IF (AN == 0) FACT = 1.d0
       A1 = (-1.d0)**AN
       A2 = (2.d0 * REAL(AN,r64)) + 1.d0
       A3 = A1 * Var1**A2
       A4 = FACT * ((2.d0 * REAL(AN,r64)) + 1.d0)
       A5 = (A3 / A4) + A5
     END DO
     ErfValue = PKON * A5

   ELSE !Series solution (Geelen's equation 22): B-range
     !Factorial values not combined so that source of terms more clear
     TwoX = 2.d0 * Var1

     B1 = Var1**2
     B2 = EXP1_Value**(-B1)
     B3 = Var1 * PI_SQRT
     B4 = B2 / B3

     BT1 = 2.d0 /(TwoX**2)
     BT2 = 24.d0 / (2.d0 * (TwoX**4))
     BT3 = 720.0d0 / (6.d0 * (TwoX**6))
     BT4 = 40320.d0 / (24.d0 * (TwoX**8))
     BT5 = 3628800.d0 / (120.d0 * (TwoX**10))
     BT6 = F12 / (720.d0 * (TwoX**12.d0))
     BT7 = (F12 * 182.d0) / (5040.d0 * (TwoX**14))
     BT8 = (F12 * 240.d0 * 182.d0) / (40320.d0 * (TwoX**16))
     BT9 = (F12 * 306.d0 * 240.d0 * 182.d0) / (362880.d0 * (TwoX**18))
     SeriesSum = 1.d0 - BT1 + BT2 - BT3 + BT4 - BT5 + BT6 - BT7 + BT8 - BT9

     ErfValue = 1.d0 - B4 * SeriesSum
   END IF

RETURN
END FUNCTION ERF
