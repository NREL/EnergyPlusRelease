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
