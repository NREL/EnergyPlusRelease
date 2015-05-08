MODULE ExternalInterface

          ! Module containing the routines dealing with the BCVTB interface

          ! MODULE INFORMATION:
          !       AUTHOR         Michael Wetter
          !       DATE WRITTEN   2Dec2007
          !       MODIFIED       Rui Zhang July 2009
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS MODULE:
          ! To encapsulate the data and routines required to interface
          ! the Building Controls Virtual Test Bed (BCVTB)

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! http://simulationresearch.lbl.gov/bcvtb

          ! OTHER NOTES:
          ! na

          ! USE STATEMENTS:
  USE DataGlobals, ONLY: MaxNameLength
  USE DataInterfaces, ONLY: ShowWarningError, ShowSevereError, &
       ShowFatalError, SetupOutputVariable, ShowContinueError
  USE General, ONLY: TrimSigDigits

          ! <use statements for access to subroutines in other modules>

  IMPLICIT NONE ! Enforce explicit typing of all variables

  PRIVATE ! Everything private unless explicitly made public

          ! MODULE PARAMETER DEFINITIONS:
  INTEGER, PARAMETER :: maxVar = 1024              ! Maximum number of variables to be exchanged
  INTEGER, PARAMETER :: maxErrMsgLength = 10000    ! Maximum error message length from xml schema validation
  INTEGER, PARAMETER :: indexSchedule = 1   ! Index for schedule in inpVarTypes
  INTEGER, PARAMETER :: indexVariable = 2   ! Index for variable in inpVarTypes
  INTEGER, PARAMETER :: indexActuator = 3   ! Index for actuator in inpVarTypes
  INTEGER, PARAMETER :: nInKeys       = 3   ! number of input variables available in ExternalInterface (=highest index* number)
  CHARACTER(len=*), PARAMETER :: socCfgFilNam="socket.cfg" ! socket configuration file


          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! MODULE VARIABLE DECLARATIONS:
  INTEGER, PUBLIC                         :: NumExternalInterfaces = 0        ! Number of ExternalInterface objects
  INTEGER                                 :: simulationStatus = 1 ! Status flag. Used to report during
                                                                  ! which phase an error occured.
                                                                  ! (1=initialization, 2=time stepping)

  INTEGER,      ALLOCATABLE, DIMENSION(:) :: keyVarIndexes        ! Array index for specific key name
  INTEGER,      ALLOCATABLE, DIMENSION(:) :: varTypes             ! Types of variables in keyVarIndexes
  INTEGER,      ALLOCATABLE, DIMENSION(:) :: varInd               ! Index of ErlVariables for ExternalInterface
  INTEGER                                 :: socketFD = -1        ! socket file descriptor
  LOGICAL                                 :: ErrorsFound=.false.  ! Set to true if errors are found
  LOGICAL                                 :: noMoreValues=.false. ! Flag, true if no more values will
                                                                  ! will be sent by the server

  CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: varKeys  ! Keys of report variables used for data exchange
  CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: varNames ! Names of report variables used for data exchange
  INTEGER, ALLOCATABLE, DIMENSION(:)                      :: inpVarTypes! Names of report variables used for data exchange
  CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: inpVarNames ! Names of report variables used for data exchange

  LOGICAL       :: configuredControlPoints = .FALSE. ! True if control points have been configured
  LOGICAL       :: useEMS = .false.  ! Will be set to true if ExternalInterface writes to EMS variables or actuators

          ! SUBROUTINE SPECIFICATIONS FOR MODULE ExternalInterface:

  PUBLIC  ExternalInterfaceExchangeVariables
  PUBLIC  CloseSocket
  PRIVATE  InitExternalInterface
  PRIVATE GetExternalInterfaceInput
  PRIVATE CalcExternalInterface
  PRIVATE ParseString
  PRIVATE GetReportVariableKey
  PRIVATE StopExternalInterfaceIfError
  PRIVATE ValidateRunControl
  PRIVATE WarnIfExternalInterfaceObjectsAreUsed

CONTAINS

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE ExternalInterfaceExchangeVariables

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Michael Wetter
          !       DATE WRITTEN   2Dec2007
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Exchanges variables between EnergyPlus and the BCVTB socket.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataGlobals, ONLY: WarmupFlag, KindOfSim, ksRunPeriodWeather

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:


          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  LOGICAL,SAVE :: GetInputFlag = .true.  ! First time, input is "gotten"

  IF (GetInputFlag) THEN
    CALL GetExternalInterfaceInput
    GetInputFlag=.false.
  ENDIF


! Parameters for KindOfSim
!INTEGER, PARAMETER :: ksDesignDay = 1
!INTEGER, PARAMETER :: ksRunPeriodDesign = 2
!INTEGER, PARAMETER :: ksRunPeriodWeather = 3

  IF (NumExternalInterfaces == 1) THEN
     CALL InitExternalInterface()
     ! Exchange data only after sizing and after warm-up.
     ! Note that checking for ZoneSizingCalc SysSizingCalc does not work here, hence we
     ! use the KindOfSim flag
     IF (.NOT.  (WarmupFlag)  .AND. (KindOfSim .EQ. ksRunPeriodWeather)) THEN
        CALL CalcExternalInterface()
     ENDIF
  END IF
  ! There is nothing to update or report
!!  CALL UpdateExternalInterface()

!!  CALL ReportExternalInterface()

  RETURN

END SUBROUTINE ExternalInterfaceExchangeVariables

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE GetExternalInterfaceInput

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Michael Wetter
          !       DATE WRITTEN   2Dec2007
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Obtains input data for ExternalInterface

          ! METHODOLOGY EMPLOYED:
          ! Uses InputProcessor "Get" routines to obtain data.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: GetNumObjectsFound, GetObjectItem, VerifyName

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
  CHARACTER(len=MaxNameLength), DIMENSION(3) :: Alphas  ! Alpha items for object
  REAL, DIMENSION(1)             :: Numbers ! Numeric items for object
  INTEGER                        :: NumAlphas  ! Number of Alphas for each GetObjectItem call
  INTEGER                        :: NumNumbers ! Number of Numbers for each GetObjectItem call
  INTEGER                        :: IOStatus   ! Used in GetObjectItem
  LOGICAL                        :: IsNotOK              ! Flag to verify name
  LOGICAL                        :: IsBlank              ! Flag for blank name

  NumExternalInterfaces = GetNumObjectsFound('ExternalInterface')
  IF (NumExternalInterfaces == 0) THEN
     CALL WarnIfExternalInterfaceObjectsAreUsed('ExternalInterface:Schedule')
     CALL WarnIfExternalInterfaceObjectsAreUsed('ExternalInterface:Variable')
     CALL WarnIfExternalInterfaceObjectsAreUsed('ExternalInterface:Actuator')
  ENDIF
  IF (NumExternalInterfaces == 1) THEN
     CALL DisplayString('Instantiating Building Controls Virtual Test Bed')
     ALLOCATE(varKeys(maxVar))  ! Keys of report variables used for data exchange
     varKeys=' '
     ALLOCATE(varNames(maxVar)) ! Names of report variables used for data exchange
     varNames=' '
     ALLOCATE(inpVarTypes(maxVar)) ! Names of report variables used for data exchange
     inpVarTypes=0
     ALLOCATE(inpVarNames(maxVar)) ! Names of report variables used for data exchange
     inpVarNames=' '
     CALL VerifyExternalInterfaceObject
  ENDIF
  IF (NumExternalInterfaces > 1) THEN
     CALL ShowSevereError('GetExternalInterfaceInput: Cannot have more than one ExternalInterface object.')
     CALL ShowContinueError('GetExternalInterfaceInput: Errors found in input.')
     ErrorsFound = .true.
  ENDIF

  CALL StopExternalInterfaceIfError

  RETURN

END SUBROUTINE GetExternalInterfaceInput


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE StopExternalInterfaceIfError
  ! SUBROUTINE INFORMATION:
  !       AUTHOR         Michael Wetter
  !       DATE WRITTEN   9Jan2008
  !       MODIFIED       na
  !       RE-ENGINEERED  na

  ! PURPOSE OF THIS SUBROUTINE:
  ! This subroutine gracefully stops the ExternalInterface if an error has been found.
  ! It sends an appropriate message to the ExternalInterface
  ! and then calls a fatal error to stop EnergyPlus.

  ! METHODOLOGY EMPLOYED:
  ! na

  ! REFERENCES:
  ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

  ! INTERFACE BLOCK SPECIFICATIONS:
  INTERFACE
     INTEGER(C_INT) FUNCTION sendclientmessage(socketFD, flaWri) BIND (C, NAME="sendclientmessage")
       USE ISO_C_BINDING, ONLY: C_INT
       INTEGER(C_INT) socketFD ! socket file descriptor
       INTEGER(C_INT) flaWri   ! flag to write to the socket
     END FUNCTION sendclientmessage
  END INTERFACE

  ! USE STATEMENTS:
  ! na

  ! SUBROUTINE ARGUMENT DEFINITIONS:
  ! na

  ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER retVal ! Return value, needed to catch return value of function call

   IF ( ErrorsFound ) THEN
      ! Check if the socket is open
      IF ( socketFD .GE. 0 ) THEN
         ! Socket is open
         IF (simulationStatus == 1) THEN
            retVal = sendclientmessage(socketFD, -10)
         ELSE
            retVal = sendclientmessage(socketFD, -20)
         ENDIF
      ENDIF
      CALL ShowFatalError('Error in ExternalInterface: Check EnergyPlus *.err file.')
   ENDIF
END SUBROUTINE StopExternalInterfaceIfError

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE CloseSocket(FlagToWriteToSocket)
  ! SUBROUTINE INFORMATION:
  !       AUTHOR         Michael Wetter
  !       DATE WRITTEN   December 2008
  !       MODIFIED       na
  !       RE-ENGINEERED  na

  ! PURPOSE OF THIS SUBROUTINE:
  ! This subroutine tries to write the optional error code to the
  ! socket and then closes the socket

  ! METHODOLOGY EMPLOYED:
  ! na

  ! REFERENCES:
  ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

  ! INTERFACE BLOCK SPECIFICATIONS:
  INTERFACE
     INTEGER(C_INT) FUNCTION establishclientsocket(fileName) BIND (C, NAME="establishclientsocket")
       USE ISO_C_BINDING, ONLY: C_INT, C_CHAR
       CHARACTER(kind=C_CHAR), DIMENSION(*) :: fileName  ! file from which socket port number will be read
     END FUNCTION establishclientsocket
  END INTERFACE

  INTERFACE
     INTEGER(C_INT) function sendclientmessage(socketFD, flaWri) BIND (C, NAME="sendclientmessage")
       USE ISO_C_BINDING, ONLY: C_INT
       INTEGER(C_INT) socketFD ! socket file descriptor
       INTEGER(C_INT) flaWri   ! flag to write to the socket
     END FUNCTION sendclientmessage
  END INTERFACE

  ! USE STATEMENTS:
  ! na

  ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: FlagToWriteToSocket  ! flag to write to the socket
                                              ! +1: E+ reached final time
                                              ! -1: E+ had some error

  ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER retVal ! Return value, needed to catch return value of function call
  LOGICAL fileExist ! Set to true if file exists

  ! Try to establish socket connection. This is needed if Ptolemy started E+,
  ! but E+ had an error before the call to InitExternalInterface.

  INQUIRE (FILE=socCfgFilNam, EXIST=fileExist)

  IF ( socketFD ==  -1 .AND. fileExist) THEN
       socketFD = establishclientsocket(socCfgFilNam)
  END IF

  IF ( socketFD .GE. 0 ) THEN
     retVal = sendclientmessage(socketFD, FlagToWriteToSocket)
! Don't close socket as this may give sometimes an IOException in Windows
! This problem seems to affect only Windows but not Mac
!     close(socketFD)
  ENDIF
  RETURN

END SUBROUTINE CloseSocket

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE ParseString(str, ele, nEle)
  ! SUBROUTINE INFORMATION:
  !       AUTHOR         Michael Wetter
  !       DATE WRITTEN   8Jan2008
  !       MODIFIED       na
  !       RE-ENGINEERED  na

  ! PURPOSE OF THIS SUBROUTINE:
  ! This subroutine parses the semicolon separated string xmlStr
  ! and assigns each element to ele

  ! METHODOLOGY EMPLOYED:
  ! na

  ! REFERENCES:
  ! na

  ! USE STATEMENTS:
  USE InputProcessor, ONLY: MakeUPPERCase

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

  ! SUBROUTINE ARGUMENT DEFINITIONS:
           ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: str                          ! The string, with all elements separated by ';'
  CHARACTER(len=MaxNameLength), DIMENSION(:), INTENT(OUT) :: ele  ! The elements
  INTEGER, INTENT(IN)                  :: nEle ! The number of elements

  ! SUBROUTINE PARAMETER DEFINITIONS:
  ! na
  INTEGER :: i ! Counter
  INTEGER :: iSta ! Start of substring
  INTEGER :: iEnd ! End of substring
  INTEGER :: lenStr ! Length of string
  lenStr = len(str)
  iEnd = 1
  DO i = 1, nEle
     iSta = iEnd  ! add one to skip ';'
     iEnd = iSta+INDEX(str(iSta:lenStr), ';')
     ele(i) = TRIM(MakeUPPERCase(str(iSta:(iEnd-2))))
  ENDDO
END SUBROUTINE ParseString

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE InitExternalInterface()

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Michael Wetter
          !       DATE WRITTEN   2Dec2007
          !       MODIFIED       Rui Zhang Aug 2009
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is for initializations of the ExternalInterface

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE ScheduleManager, ONLY: GetDayScheduleIndex
  USE RuntimeLanguageProcessor, ONLY: isExternalInterfaceErlVariable, FindEMSVariable
  USE DataGlobals, ONLY: WeathSimReq

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:

  LOGICAL,SAVE :: firstCall    = .TRUE.  ! First time, input has been read

  CHARACTER(len=*), PARAMETER :: simCfgFilNam="variables.cfg"

  INTEGER :: i, j         ! Loop counter
          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

  INTERFACE
     INTEGER(C_INT) FUNCTION establishclientsocket(fileName) BIND (C, NAME="establishclientsocket")
       ! Returns the logical unit number of the socket
       USE ISO_C_BINDING, ONLY: C_INT, C_CHAR
       CHARACTER(kind=C_CHAR), DIMENSION(*) :: fileName  ! file from which socket port number will be read
     END FUNCTION establishclientsocket
  END INTERFACE

  INTERFACE
     INTEGER(C_INT) FUNCTION getmainversionnumber BIND (C, NAME="getmainversionnumber")
       ! Returns the main version number of the interface, or a negative
       ! number if a dummy dll is used
       USE ISO_C_BINDING, ONLY: C_INT
     END FUNCTION getmainversionnumber
  END INTERFACE

  INTERFACE
     INTEGER(C_INT) FUNCTION getnumberofxmlvalues(fileName, xpathExpr)
       ! Returns the number of XML values in the file, or a negative
       ! value if an error occurred
       USE ISO_C_BINDING, ONLY: C_INT, C_CHAR
       CHARACTER(kind=C_CHAR), DIMENSION(*) :: fileName  ! Name of XML file
       CHARACTER(kind=C_CHAR), DIMENSION(*) :: xpathExpr ! XPath expression
     END FUNCTION getnumberofxmlvalues
  END INTERFACE

  INTERFACE
     INTEGER(C_INT) FUNCTION getxmlvalue(fileName, xpathExpr, res, nAtr) BIND (C, NAME="getxmlvalue")
       ! Gets the xml values, and returns -1 if an error occurred, or 0 otherwise
       USE ISO_C_BINDING, ONLY: C_INT, C_CHAR
       CHARACTER(kind=C_CHAR), DIMENSION(*) :: fileName  ! Name of XML file
       CHARACTER(kind=C_CHAR), DIMENSION(*) :: xpathExpr ! XPath expression
       CHARACTER(kind=C_CHAR), DIMENSION(*) :: res       ! The values of the xml attributes will be stored here
       INTEGER(C_INT)                :: nAtr             ! The number of attributes to be obtained
     END FUNCTION getxmlvalue
  END INTERFACE

  INTERFACE
     INTEGER(C_INT) FUNCTION getepvariables(fileName, outVarsNam, outVarsTyp, nOutVars, inVarsKey, &
                             nInVarsKeys, inVars, nInVars, inVarsTyp, strLen) BIND (C, NAME="getepvariables")
       ! Gets the EnergyPlus variables with which data is being exchanged.
       ! This function return -1 if an error occurred, or 0 otherwise.
       USE ISO_C_BINDING, ONLY: C_INT, C_CHAR
       CHARACTER(kind=C_CHAR), DIMENSION(*) :: fileName    ! Name of the XMLfile
       CHARACTER(kind=C_CHAR), DIMENSION(*) :: outVarsNam  ! Comma seperated string for returned
                                                           ! output variable names
       CHARACTER(kind=C_CHAR), DIMENSION(*) :: outVarsTyp  ! Comma seperated string for returned
                                                           ! output variable types
       INTEGER(C_INT)                :: nOutVars           ! Number of output variables found
       CHARACTER(kind=C_CHAR), DIMENSION(*) :: inVarsKey   ! The available variable types from
                                                           ! ExternalInterface for EP input(comma seperated)
       INTEGER(C_INT)                :: nInVarsKeys        ! Number of available variables types
                                                           ! from ExternalInterface for EP input
       CHARACTER(kind=C_CHAR), DIMENSION(*) :: inVars      ! Comma seperated string for returned
                                                           ! input variables names
       INTEGER(C_INT)                :: nInVars            ! Number of input variables found
       INTEGER(C_INT), DIMENSION(*)  :: inVarsTyp          ! Comma seperated string for returned
                                                           ! input variable type
       INTEGER(C_INT)                :: strLen             ! Length of the xml string
                                                           ! to be returned
     END FUNCTION getepvariables
  END INTERFACE

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER, PARAMETER :: lenXmlStr = maxVar*MaxNameLength ! Length of xml string
  CHARACTER(len=lenXmlStr)     :: xmlStrOut       ! xml values in string, separated by ';'
  CHARACTER(len=lenXmlStr)     :: xmlStrOutTyp    ! xml values in string, separated by ';'
  CHARACTER(len=lenXmlStr)     :: xmlStrInKey     ! xml values in string, separated by ';'
  CHARACTER(len=lenXmlStr)     :: xmlStrIn        ! xml values in string, separated by ';'
  CHARACTER(len=lenXmlStr)     :: xmlStrInTyp     ! xml values in string, separated by ';'
  INTEGER, SAVE                :: nOutVal         ! Number of output values (E+ -> ExternalInterface)
  INTEGER, SAVE                :: nInpVar         ! Number of input values (ExternalInterface -> E+)
  INTEGER                      :: retVal          ! Return value of function call, used for error handling
  INTEGER                      :: counter = 0     ! Counter for ErlVariables
  INTEGER                      :: mainVersion     ! The version number

  CHARACTER(len=MaxNameLength), DIMENSION(maxVar) :: curVals ! Names of schedules (i.e., schedule names)
  INTEGER curNumInpVal                                       ! current number of input values for the InputValType
  CHARACTER(len=maxErrMsgLength)   :: validateErrMsg         ! error returned when xml Schema validate failed
  INTEGER                          :: errMsgLen              ! the length of the error message
  LOGICAL socFileExist                                       ! Set to true if socket configuration
                                                             ! file exists
  LOGICAL simFileExist                                       ! Set to true if simulation configuration
                                                             ! file exists
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


  IF (FirstCall) THEN
     CALL DisplayString('ExternalInterface initializes.')
     ! do one time initializations

     !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     ! Check version number
     mainVersion = getmainversionnumber()
     IF (mainVersion .LT. 0.0) THEN
        CALL ShowSevereError('ExternalInterface: BCVTB is not installed in this version.')
        ErrorsFound = .true.
        CALL StopExternalInterfaceIfError
     END IF

     !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     ! Get port number
      INQUIRE (FILE=socCfgFilNam, EXIST=socFileExist)
      IF (socFileExist) THEN
        socketFD = establishclientsocket(socCfgFilNam)
        IF (socketFD .LT. 0) THEN
           CALL ShowSevereError('ExternalInterface: Could not open socket. File descriptor = ' &
                //TRIM(TrimSigDigits(socketFD))//'.')
           ErrorsFound = .true.
        END IF
     ELSE
        CALL ShowSevereError('ExternalInterface: Did not find file "'//socCfgFilNam//'".')
        CALL ShowContinueError('This file needs to be in same directory as in.idf.')
        CALL ShowContinueError('Check the documentation for the ExternalInterface.')
        ErrorsFound = .true.
     END IF
     !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     ! Make sure that idf file specified a run period other than
     ! design day and system sizing.
     CALL ValidateRunControl

     !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     CALL StopExternalInterfaceIfError

     xmlStrOut(1:lenXmlStr) = " "
     xmlStrOutTyp(1:lenXmlStr) = " "
     xmlStrInKey(1:lenXmlStr) = " "
     xmlStrIn(1:lenXmlStr) = " "
     !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     ! Get input and output variables for EnergyPlus in sequence
      xmlStrInKey= "schedule," &
           //"variable," &
           //"actuator"//char(0)
      ! Check if simCfgFilNam exists.
      INQUIRE (FILE=simCfgFilNam, EXIST=simFileExist)
      IF (simFileExist) THEN
         retVal = getepvariables(simCfgFilNam, &
              xmlStrOutTyp ,xmlStrOut, nOutVal, &
              xmlStrInKey, nInKeys, &
              xmlStrIn, nInpVar, inpVarTypes, lenXmlStr)

         ! handle errors when reading variables.cfg file
         IF ( retVal .LT. 0 ) THEN
            CALL ShowSevereError('ExternalInterface: Error when getting input and output variables for EnergyPlus,')
            CALL ShowContinueError('check simulation.log for error message.')
            ErrorsFound = .true.
         END IF
      ELSE
        CALL ShowSevereError('ExternalInterface: Did not find file "'//simCfgFilNam//'".')
        CALL ShowContinueError('This file needs to be in same directory as in.idf.')
        CALL ShowContinueError('Check the documentation for the ExternalInterface.')
        ErrorsFound = .true.
      END IF
     CALL StopExternalInterfaceIfError

     IF ( nOutVal + nInpVar .GT. maxVar ) THEN
        CALL ShowSevereError('ExternalInterface: Too many variables to be exchanged.')
        CALL ShowContinueError('Attempted to exchange '//TRIM(TrimSigDigits(nOutVal))//' outputs')
        CALL ShowContinueError('plus '//TRIM(TrimSigDigits(nOutVal))//' inputs.')
        CALL ShowContinueError('Maximum allowed is sum is '//TRIM(TrimSigDigits(maxVar))//'.')
        CALL ShowContinueError('To fix, increase maxVar in ExternalInterface.f90')
        ErrorsFound = .true.
     END IF
     CALL StopExternalInterfaceIfError
     !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     !!!!!!!!!!!!!!!!!!!!!
     IF ( nOutVal .LT. 0 ) THEN
        CALL ShowSevereError('ExternalInterface: Error when getting number of xml values for outputs.')
        ErrorsFound = .true.
     ELSE
        CALL ParseString(xmlStrOut, varNames, nOutVal)
        CALL ParseString(xmlStrOutTyp, varkeys, nOutVal)
     END IF
     CALL StopExternalInterfaceIfError

     IF ( nInpVar .LT. 0 ) THEN
        CALL ShowSevereError('ExternalInterface: Error when getting number of xml values for inputs.')
        ErrorsFound = .true.
     ELSE
        CALL ParseString(xmlStrIn, inpVarNames, nInpVar)
     END IF
     CALL StopExternalInterfaceIfError

     CALL DisplayString('Number of outputs in ExternalInterface = '//TrimSigDigits(nOutVal))
     CALL DisplayString('Number of inputs  in ExternalInterface = '//TrimSigDigits(nInpVar))

     FirstCall = .FALSE.

  ELSEIF (.NOT.ConfiguredControlPoints) THEN
     ALLOCATE(keyVarIndexes( nOutVal ))
     ALLOCATE(varTypes( nOutVal ))
     CALL GetReportVariableKey(varKeys, nOutVal, varNames, keyVarIndexes, varTypes)
     ALLOCATE(varInd( nInpVar ))
     DO i=1, nInpVar
        IF (inpVarTypes(i) .EQ. indexSchedule) THEN
           varInd(i) = GetDayScheduleIndex(inpVarNames(i))
        ELSEIF (inpVarTypes(i) .EQ. indexVariable) THEN
           varInd(i) = FindEMSVariable(inpVarNames(i), 0)
        ELSEIF (inpVarTypes(i) .EQ. indexActuator) THEN
           varInd(i) = FindEMSVariable(inpVarNames(i), 0)
        END IF
        IF (varInd(i) .LE. 0) THEN
           CALL ShowSevereError('ExternalInterface: Error, xml file "' // simCfgFilNam // '" declares variable "' &
                //TRIM(inpVarNames(i))//'",')
           CALL ShowContinueError('but variable was not found in idf file.')
           ErrorsFound = .true.
        END IF
     ENDDO
     CALL StopExternalInterfaceIfError
     ! Configure Erl variables
     DO i=1, nInpVar
        IF (inpVarTypes(i) .EQ. indexVariable) THEN ! ems-globalvariable
           useEMS = .true.
           IF ( .NOT. isExternalInterfaceErlVariable(varInd(i)) ) THEN
              CALL ShowSevereError('ExternalInterface: Error, xml file "' // simCfgFilNam // '" declares variable "' &
                   //TRIM(inpVarNames(i))//'",')
              CALL ShowContinueError('But this variable is an ordinary Erl variable, not an ExternalInterface variable.')
              CALL ShowContinueError('You must specify a variable of type "ExternalInterface:Variable".')
              ErrorsFound = .true.
           ENDIF
        ELSEIF (inpVarTypes(i) .EQ. indexActuator) THEN ! ems-actuator
           useEMS = .true.
           IF ( .NOT. isExternalInterfaceErlVariable(varInd(i)) ) THEN
              CALL ShowSevereError('ExternalInterface: Error, xml file "' // simCfgFilNam // '" declares variable "' &
                   //TRIM(inpVarNames(i))//'",')
              CALL ShowContinueError('But this variable is an ordinary Erl actuator, not an ExternalInterface actuator.')
              CALL ShowContinueError('You must specify a variable of type "ExternalInterface:Actuator".')
              ErrorsFound = .true.
           ENDIF
        END IF
     ENDDO
     ConfiguredControlPoints = .TRUE.
  END IF
  CALL StopExternalInterfaceIfError
  RETURN

END SUBROUTINE InitExternalInterface

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE ValidateRunControl

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Michael Wetter
          !       DATE WRITTEN   December 2009
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine ensures that the RunControl object is valid.

          ! METHODOLOGY EMPLOYED:
          ! Use GetObjectItem from the Input Processor

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: GetNumObjectsFound, GetObjectItem
  USE DataIPShortCuts

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER                        :: NumAlphas  = 0 ! Number of Alphas for each GetObjectItem call
  INTEGER                        :: NumNumbers = 0 ! Number of Numbers for each GetObjectItem call
  INTEGER                        :: IOStatus = 0  ! Used in GetObjectItem

  INTEGER :: NumRunControl

  cCurrentModuleObject='SimulationControl'
  NumRunControl = GetNumObjectsFound(TRIM(cCurrentModuleObject))
  IF (NumRunControl > 0) THEN
     CALL GetObjectItem(TRIM(cCurrentModuleObject),1,cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, IOStatus, &
                    AlphaFieldnames=cAlphaFieldNames, NumericFieldNames=cNumericFieldNames)
     IF (cAlphaArgs(5).EQ.'NO') THEN ! This run does not have a weather file simulation.
        CALL ShowSevereError(  'ExternalInterface: Error in idf file, section SimulationControl:')
        CALL ShowContinueError('When using the ExternalInterface, a run period from the weather file must be specified')
        CALL ShowContinueError('in the idf file, because the ExternalInterface interface is not active during')
        CALL ShowContinueError('warm-up and during sizing.')
        ErrorsFound = .true.
     END IF
  END IF
END SUBROUTINE ValidateRunControl

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE CalcExternalInterface()

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Michael Wetter
          !       DATE WRITTEN   2Dec2007
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! na

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataGlobals, ONLY: SimTimeSteps, MinutesPerTimeStep, emsCallFromExternalInterface
  USE DataInterfaces, ONLY: GetInternalVariableValueExternalInterface
  USE ScheduleManager, ONLY: ExternalInterfaceSetSchedule
  USE RuntimeLanguageProcessor, ONLY: ExternalInterfaceSetErlVariable
  USE EMSManager, ONLY: ManageEMS

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
  ! These parameters are also declared in an interface below.
  ! Change all together.
  INTEGER, PARAMETER :: nDblMax = 1024   ! Maximum number of doubles
  INTEGER, PARAMETER :: nIntMax = 0      ! Maximum number of integers
  INTEGER, PARAMETER :: nBooMax = 0      ! Maximum number of booleans


          ! INTERFACE BLOCK SPECIFICATIONS:
  INTERFACE
     INTEGER(C_INT) FUNCTION exchangeDoublesWithSocket(socketFD, &
          flaWri, flaRea, &
          nDblWri, nDblRea, &
          simTimWri, dblValWri, &
          simTimRea, dblValRea) BIND (C, NAME="exchangedoubleswithsocket")
       ! Exchanges data with the socket
       USE ISO_C_BINDING, ONLY: C_INT

       ! These parameters are also declared in an interface below.
       ! Change all together.
       INTEGER(C_INT), PARAMETER :: nDblMax = 1024       ! Maximum number of doubles

       INTEGER(C_INT) socketFD                           ! socket file descriptor
       INTEGER(C_INT) flaWri                             ! flag to write to the socket
       INTEGER(C_INT) flaRea                             ! flag read from the socket
       INTEGER(C_INT) nDblWri                            ! number of doubles to write to socket
       INTEGER(C_INT) nDblRea                            ! number of doubles to read from socket
       DOUBLE PRECISION simTimWri                        ! simulation time to write to socket
       DOUBLE PRECISION simTimRea                        ! simulation time to read from socket
       DOUBLE PRECISION, DIMENSION(nDblMax) :: dblValWri ! dbl values to be written to the socket
       DOUBLE PRECISION, DIMENSION(nDblMax) :: dblValRea ! dbl values to be read from the socket
     END FUNCTION exchangeDoublesWithSocket
  END INTERFACE


          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER      :: i, j    ! Loop counter
  INTEGER      :: retVal  ! Return value from socket

  INTEGER flaWri   ! flag to write to the socket
  INTEGER flaRea   ! flag read from the socket
  INTEGER nDblWri ! number of doubles to write to socket
  INTEGER nDblRea ! number of doubles to read from socket
  DOUBLE PRECISION :: curSimTim ! current simulation time
  DOUBLE PRECISION :: preSimTim ! previous time step's simulation time

  DOUBLE PRECISION, DIMENSION(nDblMax) :: dblValWri
  DOUBLE PRECISION, DIMENSION(nDblMax) :: dblValRea
  character*5 retValCha
  LOGICAL                              :: continueSimulation ! Flag, true if simulation should continue
  LOGICAL, SAVE                        :: firstCall = .true.
  LOGICAL, SAVE                        :: showContinuationWithoutUpdate = .true.

  IF (firstCall) THEN
     CALL DisplayString('ExternalInterface starts first data exchange.')
     simulationStatus = 2
     firstCall = .false.
     preSimTim = 0 ! In the first call, E+ did not reset SimTimeSteps to zero
  ELSE
     preSimTim = SimTimeSteps * MinutesPerTimeStep * 60.0d0
  ENDIF

  ! Socket asked to terminate simulation, but simulation continues
  IF (noMoreValues .and. showContinuationWithoutUpdate) THEN
     CALL ShowWarningError('ExternalInterface: Continue simulation without updated values from server at t =' &
          //TrimSigDigits(preSimTim/3600, 2) // ' hours')
     showContinuationWithoutUpdate = .false.
  ENDIF


  ! Usual branch, control is configured and simulation should continue
  IF (ConfiguredControlPoints .AND. (.NOT.noMoreValues)) THEN
     ! Data to be exchanged
     nDblWri = SIZE(varTypes)
     nDblRea = 0
     flaWri  = 0
     !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     ! Get EnergyPlus variables
     DO i = 1, nDblWri
       !CR - 8481 fix - 08/19/2011 
       dblValWri(i) = GetInternalVariableValueExternalInterface(varTypes(i), keyVarIndexes(i))     
     ENDDO

     !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     ! Exchange data with socket
     retVal = 0
     flaRea = 0
     retVal = exchangeDoublesWithSocket(socketFD, &
          flaWri, flaRea, &
          nDblWri, nDblRea, &
          preSimTim, dblValWri, &
          curSimTim, dblValRea)
     continueSimulation = .true.
     !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     ! Check for errors, in which case we terminate the simulation loop
     IF (retVal .NE. 0) THEN
        continueSimulation = .false.
        write (retValCha,1000) retVal
        CALL ShowSevereError('ExternalInterface: Socket communication received error value "' &
             //TRIM(retValCha)// '" at time = ' &
             //TRIM(TrimSigDigits(preSimTim/3600,2 ))// ' hours.')
        write (retValCha,1000) flaRea
        CALL ShowContinueError('ExternalInterface: Flag from server "' &
             //TRIM(retValCha)// '".')

        ErrorsFound = .true.
        CALL StopExternalInterfaceIfError
     END IF
     !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     ! Check communication flag
     IF (flaRea .NE. 0) THEN
        ! No more values will be received in future steps
        noMoreValues = .true.
        write (retValCha,1000) flaRea
        CALL ShowSevereError('ExternalInterface: Received end of simulation flag at time = ' &
             //TRIM(TrimSigDigits(preSimTim/3600,2 ))// ' hours.')
        CALL StopExternalInterfaceIfError
     END IF
     !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     ! Make sure we get the right number of double values, unless retVal != 0
     IF ( (flaRea .EQ. 0 ) .AND. (.NOT. ErrorsFound) .AND. &
          continueSimulation .AND. (nDblRea .NE. SIZE(varInd) ) ) THEN
        CALL ShowSevereError('ExternalInterface: Received "' &
             //TRIM(TrimSigDigits(nDblRea))// '" double values, expected "' &
             //TRIM(TrimSigDigits(SIZE(varInd)))// '".')
        ErrorsFound = .true.
        CALL StopExternalInterfaceIfError
     END IF

1000  format(I2)
     !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     ! No errors found. Assign exchanged variables
     IF ( (flaRea .EQ. 0 ) .AND. continueSimulation ) THEN
        DO i = 1, SIZE(varInd)
           IF ( (inpVarTypes(i) .EQ. indexSchedule) ) THEN
             CALL ExternalInterfaceSetSchedule(varInd(i), dblValRea(i))
           ELSE IF ( (inpVarTypes(i) .EQ. indexVariable) .OR. (inpVarTypes(i) .EQ. indexActuator) ) THEN
             CALL ExternalInterfaceSetErlVariable(varInd(i), dblValRea(i))
           ELSE
             CALL ShowContinueError('ExternalInterface: Error in finding the type of the input variable for EnergyPlus')
             CALL ShowContinueError('variable index: '//TrimSigDigits(i, 2)//'. Variable will not be updated.')
           ENDIF
        ENDDO
     ENDIF
  ENDIF
  ! If we have Erl variables, we need to call ManageEMS so that they get updated in the Erl data structure
  IF (useEMS) THEN
     CALL ManageEMS(emsCallFromExternalInterface)
  END IF

  RETURN

END SUBROUTINE CalcExternalInterface

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE GetReportVariableKey(varKeys, numberOfKeys, varNames, keyVarIndexes, varTypes)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Michael Wetter
          !       DATE WRITTEN   2Dec2007
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Gets the sensor key index and type for the specified variable key and name

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataInterfaces, ONLY: GetVariableKeyCountandType, GetVariableKeys

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)                                :: numberOfKeys  ! Number of keys=size(varKeys)
  CHARACTER(len=*), INTENT(IN), DIMENSION(:)         :: varKeys       ! Standard variable name
  CHARACTER(len=*), INTENT(IN), DIMENSION(:)         :: varNames      ! Standard variable name
  INTEGER, INTENT(OUT), DIMENSION(:)                 :: keyVarIndexes ! Array index
  INTEGER, INTENT(OUT), DIMENSION(:)                 :: varTypes      ! Types of variables in keyVarIndexes

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          ! na
  INTEGER                           :: varType      = 0 ! 0=not found, 1=integer, 2=real, 3=meter
  INTEGER                           :: numKeys      = 0 ! Number of keys found
  INTEGER                           :: varAvgSum    = 0 ! Variable  is Averaged=1 or Summed=2
  INTEGER                           :: varStepType  = 0 ! Variable time step is Zone=1 or HVAC=2
  CHARACTER(len=10)                 :: varUnits         ! Units sting, may be blank
  INTEGER, DIMENSION(:), ALLOCATABLE :: keyIndexes ! Array index for
  CHARACTER(len=MaxNameLength), DIMENSION(:), ALLOCATABLE :: NamesOfKeys      ! Specific key name
  INTEGER                           :: Loop, iKey       ! Loop counter

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Get pointers for variables to be sent to Ptolemy
  DO Loop=1, numberOfKeys
     CALL GetVariableKeyCountandType(varNames(Loop),numKeys,varType,varAvgSum,varStepType,varUnits)
     IF ( varType .NE. 0 ) THEN
        ALLOCATE(NamesOfKeys(numKeys))
        ALLOCATE(keyIndexes(numKeys))
        CALL GetVariableKeys(varNames(Loop), varType, NamesOfKeys, keyIndexes)

        ! Find key index whose keyName is equal to keyNames(Loop)
        LoopKey: DO iKey = 1, SIZE(NamesOfKeys)
           IF ( TRIM( NamesOfKeys(iKey) ) == TRIM(varKeys(Loop)) ) THEN
              keyVarIndexes(Loop) = keyIndexes( iKey )
              varTypes(Loop) = varType
              EXIT LoopKey
           END IF
        END DO LoopKey
        DEALLOCATE(keyIndexes)
        DEALLOCATE(NamesOfKeys)
     ENDIF
     IF (( varType == 0 ).OR. (iKey > SIZE(NamesOfKeys)) ) THEN
        CALL ShowSevereError('ExternalInterface: Simulation model has no variable "' &
             //TRIM(varNames(Loop))// '" with key "'//TRIM(varKeys(Loop))//'".')
        ErrorsFound = .true.
     ENDIF
  END DO

  RETURN

END SUBROUTINE GetReportVariableKey

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE WarnIfExternalInterfaceObjectsAreUsed(ObjectWord)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Michael Wetter
          !       DATE WRITTEN   December 2009
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine writes a warning if ExternalInterface objects are used in the
          ! idf file, but the ExternalInterface link is not specified.

          ! METHODOLOGY EMPLOYED:
          ! Use GetObjectItem from the Input Processor

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*),INTENT(IN) :: ObjectWord

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: NumObjects   ! Number of objects found in idf file

  NumObjects = GetNumObjectsFound(TRIM(ObjectWord))
  IF (NumObjects > 0) THEN
     CALL ShowWarningError( 'IDF file contains object "'//TRIM(ObjectWord)//'",')
     CALL ShowContinueError('but object "ExternalInterface" is not specified. Values will not be updated.')
  END IF

END SUBROUTINE WarnIfExternalInterfaceObjectsAreUsed

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE VerifyExternalInterfaceObject
  ! SUBROUTINE INFORMATION:
  !       AUTHOR         Michael Wetter
  !       DATE WRITTEN   12Dec2009
  !       MODIFIED       na
  !       RE-ENGINEERED  na

  ! PURPOSE OF THIS SUBROUTINE:
  ! This subroutine verifies the correctness of the fields of
  ! the ExternalInterface object in the idf file

  ! USE STATEMENTS:
  USE InputProcessor, ONLY: GetObjectItem, SameString
  USE DataIPShortCuts

  ! METHODOLOGY EMPLOYED:
  ! na

  ! REFERENCES:
  ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

  ! INTERFACE BLOCK SPECIFICATIONS:
  ! na

  ! SUBROUTINE ARGUMENT DEFINITIONS:
  ! na

  ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER retVal ! Return value, needed to catch return value of function call

  INTEGER                        :: NumAlphas  = 0 ! Number of Alphas for each GetObjectItem call
  INTEGER                        :: NumNumbers = 0 ! Number of Numbers for each GetObjectItem call
  INTEGER                        :: IOStatus = 0  ! Used in GetObjectItem

  cCurrentModuleObject='ExternalInterface'
  CALL GetObjectItem(cCurrentModuleObject, 1, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, IOStatus, &
       AlphaFieldnames=cAlphaFieldNames, NumericFieldNames=cNumericFieldNames)
  IF ( .NOT. SameString(cAlphaArgs(1), 'PtolemyServer')) THEN
     CALL ShowSevereError('VerifyExternalInterfaceObject: '//trim(cCurrentModuleObject)//   &
        ', invalid '//trim(cAlphaFieldNames(1))//'="'//trim(cAlphaArgs(1))//'".')
     CALL ShowContinueError('only "PtolemyServer" allowed.')
     ErrorsFound = .TRUE.
  ENDIF

END SUBROUTINE VerifyExternalInterfaceObject



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

END MODULE ExternalInterface

