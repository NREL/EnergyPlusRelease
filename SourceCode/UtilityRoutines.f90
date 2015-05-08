#include "Timer.h"

SUBROUTINE AbortEnergyPlus(NoIdf,NoIDD)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   December 1997
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine causes the program to halt due to a fatal error.

          ! METHODOLOGY EMPLOYED:
          ! Puts a message on output files.
          ! Closes files.
          ! Stops the program.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataPrecisionGlobals
  USE DataSystemVariables
  USE DataTimings
  USE DataErrorTracking
  USE DataInterfaces, ONLY: ShowMessage
  USE General, ONLY: RoundSigDigits
  USE NodeInputManager, ONLY:  SetupNodeVarsForReporting,CheckMarkedNodes
  USE BranchInputManager, ONLY: TestBranchIntegrity
  USE BranchNodeConnections, ONLY: CheckNodeConnections,TestCompSetInletOutletNodes
  USE SimulationManager, ONLY: ReportLoopConnections
  USE SystemReports, ONLY: ReportAirLoopConnections
  USE SolarShading, ONLY: ReportSurfaceErrors
  USE PlantManager, ONLY: CheckPlantOnAbort
  USE ExternalInterface, ONLY: NumExternalInterfaces, CloseSocket
  USE SQLiteProcedures, ONLY: UpdateSQLiteSimulationRecord, WriteOutputToSQLite

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  LOGICAL, INTENT(IN) :: NoIdf   ! Set to true when "noidf" was found
  LOGICAL, INTENT(IN) :: NoIDD   ! Set to true when "noidd" was found

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER :: OutFmt="('Press ENTER to continue after reading above message>',$)"
  CHARACTER(len=*), PARAMETER :: ETimeFmt="(I2.2,'hr ',I2.2,'min ',F5.2,'sec')"

          ! INTERFACE BLOCK SPECIFICATIONS
  ! see DataInterfaces

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER tempfl
  INTEGER, EXTERNAL :: GetNewUnitNumber
  CHARACTER(len=32) NumWarnings
  CHARACTER(len=32) NumSevere
  CHARACTER(len=32) NumWarningsDuringWarmup
  CHARACTER(len=32) NumSevereDuringWarmup
  CHARACTER(len=32) NumWarningsDuringSizing
  CHARACTER(len=32) NumSevereDuringSizing
  CHARACTER(len=32) Elapsed
  INTEGER Hours   ! Elapsed Time Hour Reporting
  INTEGER Minutes ! Elapsed Time Minute Reporting
  REAL(r64) Seconds ! Elapsed Time Second Reporting
  LOGICAL ErrFound
  LOGICAL TerminalError
  INTEGER :: write_stat
  IF( WriteOutputToSQLite ) THEN
    CALL UpdateSQLiteSimulationRecord(.true.,.false.)
  ENDIF

  AbortProcessing=.true.
  IF (AskForConnectionsReport) THEN
    AskForConnectionsReport=.false.   ! Set false here in case any further fatal errors in below processing...

    CALL ShowMessage('Fatal error -- final processing.  More error messages may appear.')
    CALL SetupNodeVarsForReporting

    ErrFound=.false.
    TerminalError=.false.
    CALL TestBranchIntegrity(ErrFound)
    IF (ErrFound) TerminalError = .TRUE.
    CALL TestAirPathIntegrity(ErrFound)
    IF (ErrFound) TerminalError = .TRUE.
    CALL CheckMarkedNodes(ErrFound)
    IF (ErrFound) TerminalError = .TRUE.
    CALL CheckNodeConnections(ErrFound)
    IF (ErrFound) TerminalError = .TRUE.
    CALL TestCompSetInletOutletNodes(ErrFound)
    IF (ErrFound) TerminalError = .TRUE.

    IF (.not. TerminalError) THEN
      CALL ReportAirLoopConnections
      CALL ReportLoopConnections
    ENDIF

  ELSEIF (.not. ExitDuringSimulations) THEN
    CALL ShowMessage('Warning:  Node connection errors not checked - most system input has not been read (see previous warning).')
    CALL ShowMessage('Fatal error -- final processing.  Program exited before simulations began.  See previous error messages.')
  ENDIF

  IF (AskForSurfacesReport) THEN
    CALL ReportSurfaces
  ENDIF

  CALL ReportSurfaceErrors
  CALL CheckPlantOnAbort
  CALL ShowRecurringErrors
  CALL SummarizeErrors
  CALL CloseMiscOpenFiles
  NumWarnings=RoundSigDigits(TotalWarningErrors)
  NumWarnings=ADJUSTL(NumWarnings)
  NumSevere=RoundSigDigits(TotalSevereErrors)
  NumSevere=ADJUSTL(NumSevere)
  NumWarningsDuringWarmup=RoundSigDigits(TotalWarningErrorsDuringWarmup)
  NumWarningsDuringWarmup=ADJUSTL(NumWarningsDuringWarmup)
  NumSevereDuringWarmup=RoundSigDigits(TotalSevereErrorsDuringWarmup)
  NumSevereDuringWarmup=ADJUSTL(NumSevereDuringWarmup)
  NumWarningsDuringSizing=RoundSigDigits(TotalWarningErrorsDuringSizing)
  NumWarningsDuringSizing=ADJUSTL(NumWarningsDuringSizing)
  NumSevereDuringSizing=RoundSigDigits(TotalSevereErrorsDuringSizing)
  NumSevereDuringSizing=ADJUSTL(NumSevereDuringSizing)

  IF (NoIDD) THEN
    CALL DisplayString('No EnergyPlus Data Dictionary (Energy+.idd) was found.  It is possible ')
    CALL DisplayString('you "double-clicked"EnergyPlus.exe rather than using one of the methods')
    CALL DisplayString('to run Energyplus as found in the GettingStarted document in the')
    CALL DisplayString('documentation folder.  Using EP-Launch may be best -- ')
    CALL DisplayString('it provides extra help for new users.')
    CALL ShowMessage('No EnergyPlus Data Dictionary (Energy+.idd) was found. It is possible you "double-clicked" EnergyPlus.exe ')
    CALL ShowMessage('rather than using one of the methods to run Energyplus as found in the GettingStarted document')
    CALL ShowMessage('in the documentation folder.  Using EP-Launch may be best -- it provides extra help for new users.')
    WRITE(*,OutFmt)
    READ(*,*)
  ENDIF

  IF (NoIdf) THEN
    CALL DisplayString('No input file (in.idf) was found.  It is possible you "double-clicked"')
    CALL DisplayString('EnergyPlus.exe rather than using one of the methods to run Energyplus')
    CALL DisplayString('as found in the GettingStarted document in the documentation folder.')
    CALL DisplayString('Using EP-Launch may be best -- it provides extra help for new users.')
    CALL ShowMessage('No input file (in.idf) was found.  It is possible you "double-clicked" EnergyPlus.exe rather than')
    CALL ShowMessage('using one of the methods to run Energyplus as found in the GettingStarted document in the documentation')
    CALL ShowMessage('folder.  Using EP-Launch may be best -- it provides extra help for new users.')
    WRITE(*,OutFmt)
    READ(*,*)
  ENDIF

! catch up with timings if in middle
                             Time_Finish=epElapsedTime()
                             if (Time_Finish < Time_Start) Time_Finish=Time_Finish+24.0d0*3600.0d0
                             Elapsed_Time=Time_Finish-Time_Start
#ifdef EP_Detailed_Timings
                             CALL epStopTime('EntireRun=')
#endif
  IF (Elapsed_Time < 0.0d0) Elapsed_Time=0.0d0
  Hours=Elapsed_Time/3600.d0
  Elapsed_Time=Elapsed_Time-Hours*3600.0d0
  Minutes=Elapsed_Time/60.0d0
  Elapsed_Time=Elapsed_Time-Minutes*60.0d0
  Seconds=Elapsed_Time
  IF (Seconds < 0.0d0) Seconds=0.0d0
  WRITE(Elapsed,ETimeFmt) Hours,Minutes,Seconds

  CALL ShowMessage('EnergyPlus Warmup Error Summary. During Warmup: '//TRIM(NumWarningsDuringWarmup)//  &
            ' Warning; '//TRIM(NumSevereDuringWarmup)//' Severe Errors.')
  CALL ShowMessage('EnergyPlus Sizing Error Summary. During Sizing: '//TRIM(NumWarningsDuringSizing)//  &
            ' Warning; '//TRIM(NumSevereDuringSizing)//' Severe Errors.')
  CALL ShowMessage('EnergyPlus Terminated--Fatal Error Detected. '//TRIM(NumWarnings)//' Warning; '//  &
                   TRIM(NumSevere)//' Severe Errors;'// &
                   ' Elapsed Time='//TRIM(Elapsed))
  CALL DisplayString('EnergyPlus Run Time='//TRIM(Elapsed))
  tempfl=GetNewUnitNumber()
  open(tempfl,file='eplusout.end', Action='write',iostat=write_stat)
  IF (write_stat /= 0) THEN
    CALL DisplayString('AbortEnergyPlus: Could not open file "eplusout.end" for output (write).')
  ENDIF
  write(tempfl,*) 'EnergyPlus Terminated--Fatal Error Detected. '//TRIM(NumWarnings)//' Warning; '//  &
                           TRIM(NumSevere)//' Severe Errors;'//' Elapsed Time='//TRIM(Elapsed)

  close(tempfl)
#ifdef EP_Detailed_Timings
  CALL epSummaryTimes(Time_Finish-Time_Start)
#endif
  CALL CloseOutOpenFiles
  ! Close the socket used by ExternalInterface. This call also sends the flag "-1" to the ExternalInterface,
  ! indicating that E+ terminated with an error.
  IF (NumExternalInterfaces > 0) CALL CloseSocket(-1)
  STOP 'EnergyPlus Terminated--Error(s) Detected.'

  RETURN

END SUBROUTINE AbortEnergyPlus

SUBROUTINE CloseMiscOpenFiles

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   December 1997
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine scans potential unit numbers and closes
          ! any that are still open.

          ! METHODOLOGY EMPLOYED:
          ! Use INQUIRE to determine if file is open.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DaylightingManager, ONLY: CloseReportIllumMaps, CloseDFSFile
  USE DataGlobals, ONLY: OutputFileDebug
  USE DataReportingFlags, ONLY: DebugOutput

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
   INTEGER, PARAMETER :: MaxUnitNumber = 1000

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  CHARACTER(len=20) DebugPosition


!      LOGICAL :: exists, opened
!      INTEGER :: UnitNumber
!      INTEGER :: ios

      CALL CloseReportIllumMaps
      CALL CloseDFSFile

  !  In case some debug output was produced, it appears that the
  !  position on the INQUIRE will not be 'ASIS' (3 compilers tested)
  !  So, will want to keep....

  INQUIRE(OutputFileDebug,POSITION=DebugPosition)
  IF (TRIM(DebugPosition) /= 'ASIS') THEN
    DebugOutput=.True.
  ENDIF
  IF (DebugOutput) THEN
    CLOSE (OutputFileDebug)
  ELSE
    CLOSE (OutputFileDebug,STATUS='DELETE')
  END IF


  RETURN

END SUBROUTINE CloseMiscOpenFiles

SUBROUTINE CloseOutOpenFiles

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   April 2012
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine scans potential unit numbers and closes
          ! any that are still open.

          ! METHODOLOGY EMPLOYED:
          ! Use INQUIRE to determine if file is open.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
   INTEGER, PARAMETER :: MaxUnitNumber = 1000

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:


      LOGICAL :: exists, opened
      INTEGER :: UnitNumber
      INTEGER :: ios

      DO UnitNumber = 1, MaxUnitNumber
         INQUIRE (UNIT = UnitNumber, EXIST = exists,  OPENED = opened, IOSTAT = ios)
         IF (exists .and. opened .and. ios == 0) CLOSE(UnitNumber)
      END DO

  RETURN

END SUBROUTINE CloseOutOpenFiles

SUBROUTINE EndEnergyPlus

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   December 1997
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine causes the program to terminate when complete (no errors).

          ! METHODOLOGY EMPLOYED:
          ! Puts a message on output files.
          ! Closes files.
          ! Stops the program.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataPrecisionGlobals
  USE DataSystemVariables
  USE DataTimings
  USE DataErrorTracking
  USE DataInterfaces, ONLY: ShowMessage
  USE General, ONLY: RoundSigDigits
  USE SolarShading, ONLY: ReportSurfaceErrors
  USE ExternalInterface, ONLY: NumExternalInterfaces, CloseSocket, haveExternalInterfaceBCVTB
  USE SQLiteProcedures, ONLY: UpdateSQLiteSimulationRecord, WriteOutputToSQLite

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER :: ETimeFmt="(I2.2,'hr ',I2.2,'min ',F5.2,'sec')"

          ! INTERFACE BLOCK SPECIFICATIONS
  ! see DataInterfaces

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER tempfl
  INTEGER, EXTERNAL :: GetNewUnitNumber
  CHARACTER(len=32) NumWarnings
  CHARACTER(len=32) NumSevere
  CHARACTER(len=32) NumWarningsDuringWarmup
  CHARACTER(len=32) NumSevereDuringWarmup
  CHARACTER(len=32) NumWarningsDuringSizing
  CHARACTER(len=32) NumSevereDuringSizing
  CHARACTER(len=32) Elapsed
  INTEGER Hours   ! Elapsed Time Hour Reporting
  INTEGER Minutes ! Elapsed Time Minute Reporting
  REAL(r64) Seconds ! Elapsed Time Second Reporting
  INTEGER :: write_stat

  IF(WriteOutputToSQLite) THEN
    CALL UpdateSQLiteSimulationRecord(.true.,.true.)
  ENDIF

  CALL ReportSurfaceErrors
  CALL ShowRecurringErrors
  CALL SummarizeErrors
  CALL CloseMiscOpenFiles
  NumWarnings=RoundSigDigits(TotalWarningErrors)
  NumWarnings=ADJUSTL(NumWarnings)
  NumSevere=RoundSigDigits(TotalSevereErrors)
  NumSevere=ADJUSTL(NumSevere)
  NumWarningsDuringWarmup=RoundSigDigits(TotalWarningErrorsDuringWarmup)
  NumWarningsDuringWarmup=ADJUSTL(NumWarningsDuringWarmup)
  NumSevereDuringWarmup=RoundSigDigits(TotalSevereErrorsDuringWarmup)
  NumSevereDuringWarmup=ADJUSTL(NumSevereDuringWarmup)
  NumWarningsDuringSizing=RoundSigDigits(TotalWarningErrorsDuringSizing)
  NumWarningsDuringSizing=ADJUSTL(NumWarningsDuringSizing)
  NumSevereDuringSizing=RoundSigDigits(TotalSevereErrorsDuringSizing)
  NumSevereDuringSizing=ADJUSTL(NumSevereDuringSizing)

                             Time_Finish=epElapsedTime()
                             if (Time_Finish < Time_Start) Time_Finish=Time_Finish+24.0d0*3600.0d0
                             Elapsed_Time=Time_Finish-Time_Start
#ifdef EP_Detailed_Timings
                             CALL epStopTime('EntireRun=')
#endif
  Hours=Elapsed_Time/3600.0d0
  Elapsed_Time=Elapsed_Time-Hours*3600.0d0
  Minutes=Elapsed_Time/60.0d0
  Elapsed_Time=Elapsed_Time-Minutes*60.0d0
  Seconds=Elapsed_Time
  IF (Seconds < 0.0d0) Seconds=0.0d0
  WRITE(Elapsed,ETimeFmt) Hours,Minutes,Seconds

  CALL ShowMessage('EnergyPlus Warmup Error Summary. During Warmup: '//TRIM(NumWarningsDuringWarmup)//  &
            ' Warning; '//TRIM(NumSevereDuringWarmup)//' Severe Errors.')
  CALL ShowMessage('EnergyPlus Sizing Error Summary. During Sizing: '//TRIM(NumWarningsDuringSizing)//  &
            ' Warning; '//TRIM(NumSevereDuringSizing)//' Severe Errors.')
  CALL ShowMessage('EnergyPlus Completed Successfully-- '//TRIM(NumWarnings)//' Warning; '//TRIM(NumSevere)//' Severe Errors;'// &
                   ' Elapsed Time='//TRIM(Elapsed))
  CALL DisplayString('EnergyPlus Run Time='//TRIM(Elapsed))
  tempfl=GetNewUnitNumber()
  open(tempfl,file='eplusout.end', Action='write',iostat=write_stat)
  IF (write_stat /= 0) THEN
    CALL DisplayString('EndEnergyPlus: Could not open file "eplusout.end" for output (write).')
  ENDIF
  write(tempfl,'(A)') 'EnergyPlus Completed Successfully-- '//TRIM(NumWarnings)//' Warning; '//TRIM(NumSevere)//  &
                          ' Severe Errors;'//' Elapsed Time='//TRIM(Elapsed)
  close(tempfl)
#ifdef EP_Detailed_Timings
  CALL epSummaryTimes(Time_Finish-Time_Start)
#endif
  CALL CloseOutOpenFiles
  ! Close the ExternalInterface socket. This call also sends the flag "1" to the ExternalInterface,
  ! indicating that E+ finished its simulation
  IF ((NumExternalInterfaces > 0).AND. haveExternalInterfaceBCVTB) CALL CloseSocket(1)
  STOP 'EnergyPlus Completed Successfully.'

  RETURN

END SUBROUTINE EndEnergyPlus

FUNCTION GetNewUnitNumber ()  RESULT (UnitNumber)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Linda K. Lawrie, adapted from reference
          !       DATE WRITTEN   September 1997
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! Returns a unit number of a unit that can exist and is not connected.  Note
          ! this routine does not magically mark that unit number in use.  In order to
          ! have the unit "used", the source code must OPEN the file.

          ! METHODOLOGY EMPLOYED:
          ! Use Inquire function to find out if proposed unit: exists or is opened.
          ! If not, can be used for a new unit number.

          ! REFERENCES:
          ! Copyright (c) 1994 Unicomp, Inc.  All rights reserved.
          !
          ! Developed at Unicomp, Inc.
          !
          ! Permission to use, copy, modify, and distribute this
          ! software is freely granted, provided that this notice
          ! is preserved.

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER UnitNumber  ! Result from scanning currently open files

          ! FUNCTION PARAMETER DEFINITIONS:
!  IO Status Values:

  INTEGER, PARAMETER :: END_OF_RECORD = -2
  INTEGER, PARAMETER :: END_OF_FILE = -1

!  Indicate default input and output units:

  INTEGER, PARAMETER :: DEFAULT_INPUT_UNIT = 5
  INTEGER, PARAMETER :: DEFAULT_OUTPUT_UNIT = 6

!  Indicate number and value of preconnected units

  INTEGER, PARAMETER :: NUMBER_OF_PRECONNECTED_UNITS = 2
  INTEGER, PARAMETER :: PRECONNECTED_UNITS (NUMBER_OF_PRECONNECTED_UNITS) = (/ 5, 6 /)

!  Largest allowed unit number (or a large number, if none)
  INTEGER, PARAMETER :: MaxUnitNumber = 1000

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  LOGICAL :: exists  ! File exists
  LOGICAL :: opened  ! Unit is open
  INTEGER :: ios     ! return value from Inquire intrinsic

  DO UnitNumber = 1, MaxUnitNumber
    IF (UnitNumber == DEFAULT_INPUT_UNIT .or. &
        UnitNumber == DEFAULT_OUTPUT_UNIT) CYCLE
    IF (ANY (UnitNumber == PRECONNECTED_UNITS)) CYCLE
    INQUIRE (UNIT = UnitNumber, EXIST = exists,  OPENED = opened, IOSTAT = ios)
    IF (exists .and. .not. opened .and. ios == 0) RETURN      ! result is set in UnitNumber
  END DO

  UnitNumber = -1

END FUNCTION GetNewUnitNumber

FUNCTION FindUnitNumber (FileName) RESULT (UnitNumber)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   September 1997, adapted from reference
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! Returns a unit number for the file name that is either opened or exists.

          ! METHODOLOGY EMPLOYED:
          ! Use Inquire function to find out if proposed unit: exists or is opened.
          ! If not, can be used for a new unit number.

          ! REFERENCES:
          ! Copyright (c) 1994 Unicomp, Inc.  All rights reserved.
          !
          ! Developed at Unicomp, Inc.
          !
          ! Permission to use, copy, modify, and distribute this
          ! software is freely granted, provided that this notice
          ! is preserved.

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=*) FileName  ! File name to be searched.
  INTEGER UnitNumber         ! Unit number that should be used

          ! FUNCTION PARAMETER DEFINITIONS:
!  Largest allowed unit number (or a large number, if none)
  INTEGER, PARAMETER :: MaxUnitNumber = 1000

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  CHARACTER(Len=255) TestFileName       ! File name returned from opened file
  INTEGER TestFileLength                ! Length from INQUIRE intrinsic
  INTEGER,EXTERNAL :: GetNewUnitNumber  ! Function to call if file not opened
  LOGICAL :: exists                     ! True if file already exists
  LOGICAL :: opened                     ! True if file is open
  INTEGER Pos                           ! Position pointer
  INTEGER FileNameLength                ! Length of requested file
  INTEGER :: ios                        ! Status indicator from INQUIRE intrinsic

  INQUIRE (FILE=FileName, EXIST = exists,  OPENED = opened, IOSTAT = ios)
  IF (.not. OPENED) THEN
    UnitNumber=GetNewUnitNumber()
    OPEN(UNIT=UnitNumber,FILE=FileName,POSITION='APPEND',iostat=ios)
    IF (ios /= 0) THEN
      CALL DisplayString('FindUnitNumber: Could not open file "'//trim(FileName)//'" for append.')
    ENDIF
  ELSE
    FileNameLength=LEN_TRIM(FileName)
    DO UnitNumber=1,MaxUnitNumber
      INQUIRE(UNIT=UnitNumber,NAME=TestFileName,OPENED=opened)
      !  Powerstation returns just file name
      !  DVF (Digital Fortran) returns whole path
      TestFileLength=LEN_TRIM(TestFileName)
      Pos=INDEX(TestFileName,FileName)
      IF (Pos .ne. 0) THEN
        !  Must be the last part of the file
        IF (Pos+FileNameLength-1 .eq. TestFileLength) EXIT
      ENDIF
    END DO
  ENDIF

  RETURN

END FUNCTION FindUnitNumber

SUBROUTINE ConvertCasetoUpper(InputString,OutputString)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   September 1997
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Convert a string to upper case

          ! METHODOLOGY EMPLOYED:
          ! This routine is not dependant upon the ASCII
          ! code.  It works by storing the upper and lower case alphabet.  It
          ! scans the whole input string.  If it finds a character in the lower
          ! case alphabet, it makes an appropriate substitution.


          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataStringGlobals

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: InputString    ! Input string
  CHARACTER(len=*), INTENT(OUT) :: OutputString  ! Output string (in UpperCase)

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
      INTEGER A,B

      OutputString=' '

      DO A=1,LEN_TRIM(InputString)
          B=INDEX(LowerCase,InputString(A:A))
          IF (B .NE. 0) THEN
              OutputString(A:A)=UpperCase(B:B)
          ELSE
              OutputString(A:A)=InputString(A:A)
          ENDIF
      END DO

      RETURN

END SUBROUTINE ConvertCasetoUpper

SUBROUTINE ConvertCasetoLower(InputString,OutputString)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   September 1997
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Convert a string to lower case

          ! METHODOLOGY EMPLOYED:
          ! This routine is not dependant upon the ASCII
          ! code.  It works by storing the upper and lower case alphabet.  It
          ! scans the whole input string.  If it finds a character in the lower
          ! case alphabet, it makes an appropriate substitution.


          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataStringGLobals

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: InputString    ! Input string
  CHARACTER(len=*), INTENT(OUT) :: OutputString  ! Output string (in LowerCase)

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
      INTEGER A,B

      OutputString=' '

      DO A=1,LEN_TRIM(InputString)
          B=INDEX(UpperCase,InputString(A:A))
          IF (B .NE. 0) THEN
              OutputString(A:A)=LowerCase(B:B)
          ELSE
              OutputString(A:A)=InputString(A:A)
          ENDIF
      END DO

      RETURN

END SUBROUTINE ConvertCasetoLower

INTEGER FUNCTION FindNonSpace(String)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   September 1997
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function finds the first non-space character in the passed string
          ! and returns that position as the result to the calling program.

          ! METHODOLOGY EMPLOYED:
          ! Scan string for character not equal to blank.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: String  ! String to be scanned

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
      INTEGER I,ILEN

      FindNonSpace=0
      ILEN=LEN_TRIM(String)
      DO I=1,ILEN
        IF (String(I:I) .NE. ' ') THEN
          FindNonSpace=I
          EXIT
        END IF
      END DO

      RETURN

END FUNCTION FindNonSpace

SUBROUTINE ShowFatalError(ErrorMessage,OutUnit1,OutUnit2)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   September 1997
          !       MODIFIED       Kyle Benne
          !                      August 2010
          !                      Added sqlite output
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine puts ErrorMessage with a Fatal designation on
          ! designated output files.  Then, the program is aborted.

          ! METHODOLOGY EMPLOYED:
          ! Calls ShowErrorMessage utility routine.
          ! Calls AbortEnergyPlus

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataErrorTracking
  USE DataInterfaces, ONLY: ShowErrorMessage
  USE General, ONLY: RoundSigDigits
  USE SQLiteProcedures, ONLY: CreateSQLiteErrorRecord, WriteOutputToSQLite

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*) ErrorMessage
  INTEGER, OPTIONAL :: OutUnit1
  INTEGER, OPTIONAL :: OutUnit2

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
  ! see DataInterfaces

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  LOGICAL :: NoIdf=.false.
  LOGICAL :: NoIDD=.false.

  CALL ShowErrorMessage(' **  Fatal  ** '//ErrorMessage,OutUnit1,OutUnit2)
  CALL DisplayString('**FATAL:'//TRIM(ErrorMessage))
  IF (INDEX(ErrorMessage,'in.idf missing') > 0) NoIdf=.true.
  IF (INDEX(ErrorMessage,'Energy+.idd missing') > 0) NoIDD=.true.
  CALL ShowErrorMessage(' ...Summary of Errors that led to program termination:',OutUnit1,OutUnit2)
  CALL ShowErrorMessage(' ..... Reference severe error count='//TRIM(RoundSigDigits(TotalSevereErrors)),OutUnit1,OutUnit2)
  CALL ShowErrorMessage(' ..... Last severe error='//TRIM(LastSevereError),OutUnit1,OutUnit2)
  IF(WriteOutputToSQLite) THEN
    CALL CreateSQLiteErrorRecord(1,2,ErrorMessage,1)
  ENDIF
  CALL AbortEnergyPlus(NoIdf,NoIDD)

  RETURN

END SUBROUTINE ShowFatalError

SUBROUTINE ShowSevereError(ErrorMessage,OutUnit1,OutUnit2)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   September 1997
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine puts ErrorMessage with a Severe designation on
          ! designated output files.

          ! METHODOLOGY EMPLOYED:
          ! Calls ShowErrorMessage utility routine.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataStringGlobals
  USE DataErrorTracking
  USE DataInterfaces, ONLY: ShowErrorMessage
  USE DataGlobals,    ONLY : WarmupFlag,DoingSizing,KickOffSimulation
  USE SQLiteProcedures, ONLY: CreateSQLiteErrorRecord, WriteOutputToSQLite

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*) ErrorMessage
  INTEGER, OPTIONAL :: OutUnit1
  INTEGER, OPTIONAL :: OutUnit2

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
  ! see DataInterfaces

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER Loop

  DO Loop=1,SearchCounts
    IF (INDEX(ErrorMessage,TRIM(MessageSearch(Loop))) > 0) MatchCounts(Loop)=MatchCounts(Loop)+1
  ENDDO

  TotalSevereErrors=TotalSevereErrors+1
  IF (WarmupFlag .and. .not. DoingSizing .and. .not. KickOffSimulation .and. .not. AbortProcessing)   &
     TotalSevereErrorsDuringWarmup=TotalSevereErrorsDuringWarmup+1
  IF (DoingSizing) TotalSevereErrorsDuringSizing=TotalSevereErrorsDuringSizing+1
  CALL ShowErrorMessage(' ** Severe  ** '//ErrorMessage,OutUnit1,OutUnit2)
  LastSevereError=ErrorMessage

  !  Could set a variable here that gets checked at some point?

  IF(WriteOutputToSQLite) THEN
    CALL CreateSQLiteErrorRecord(1,1,ErrorMessage,1)
  ENDIF
  RETURN

END SUBROUTINE ShowSevereError

SUBROUTINE ShowSevereMessage(ErrorMessage,OutUnit1,OutUnit2)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   September 2009
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine puts ErrorMessage with a Severe designation on
          ! designated output files.
          ! But does not bump the error count so can be used in conjunction with recurring
          ! error calls.

          ! METHODOLOGY EMPLOYED:
          ! Calls ShowErrorMessage utility routine.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataStringGlobals
  USE DataErrorTracking
  USE DataInterfaces, ONLY: ShowErrorMessage
  USE SQLiteProcedures, ONLY: CreateSQLiteErrorRecord, WriteOutputToSQLite

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*) ErrorMessage
  INTEGER, OPTIONAL :: OutUnit1
  INTEGER, OPTIONAL :: OutUnit2

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
  ! see DataInterfaces

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER Loop

  DO Loop=1,SearchCounts
    IF (INDEX(ErrorMessage,TRIM(MessageSearch(Loop))) > 0) MatchCounts(Loop)=MatchCounts(Loop)+1
  ENDDO

  CALL ShowErrorMessage(' ** Severe  ** '//ErrorMessage,OutUnit1,OutUnit2)
  LastSevereError=ErrorMessage

  !  Could set a variable here that gets checked at some point?

  IF(WriteOutputToSQLite) THEN
    CALL CreateSQLiteErrorRecord(1,1,ErrorMessage,0)
  ENDIF
  RETURN

END SUBROUTINE ShowSevereMessage

SUBROUTINE ShowContinueError(Message,OutUnit1,OutUnit2)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   October 2001
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine displays a 'continued error' message on designated output files.

          ! METHODOLOGY EMPLOYED:
          ! Calls ShowErrorMessage utility routine.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataInterfaces, ONLY: ShowErrorMessage
  USE SQLiteProcedures, ONLY: UpdateSQLiteErrorRecord, WriteOutputToSQLite

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*) Message
  INTEGER, OPTIONAL :: OutUnit1
  INTEGER, OPTIONAL :: OutUnit2

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
  ! see DataInterfaces

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          ! na

  CALL ShowErrorMessage(' **   ~~~   ** '//Message,OutUnit1,OutUnit2)
  IF(WriteOutputToSQLite) THEN
    CALL UpdateSQLiteErrorRecord(Message)
  ENDIF

  RETURN

END SUBROUTINE ShowContinueError

SUBROUTINE ShowContinueErrorTimeStamp(Message,OutUnit1,OutUnit2)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   February 2004
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine displays a 'continued error' timestamp message on designated output files.

          ! METHODOLOGY EMPLOYED:
          ! Calls ShowErrorMessage utility routine.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE General,                         ONLY : CreateSysTimeIntervalString
  USE DataEnvironment,                 ONLY : EnvironmentName,CurMnDy
  USE DataGlobals,                     ONLY : WarmupFlag,DoingSizing
  USE DataInterfaces, ONLY: ShowErrorMessage
  USE SQLiteProcedures, ONLY: UpdateSQLiteErrorRecord, WriteOutputToSQLite

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*) Message
  INTEGER, OPTIONAL :: OutUnit1
  INTEGER, OPTIONAL :: OutUnit2

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
  ! see DataInterfaces

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  CHARACTER(len=100) :: cEnvHeader

  IF (WarmupFlag) THEN
    IF (.not. DoingSizing) THEN
      cEnvHeader=' During Warmup, Environment='
    ELSE
      cEnvHeader=' During Warmup & Sizing, Environment='
    ENDIF
  ELSE
    IF (.not. DoingSizing) THEN
      cEnvHeader=' Environment='
    ELSE
      cEnvHeader=' During Sizing, Environment='
    ENDIF
  ENDIF

  IF (Len_Trim(Message) < 50) THEN
    CALL ShowErrorMessage(' **   ~~~   ** '//TRIM(Message)//TRIM(cEnvHeader)//TRIM(EnvironmentName)//', at Simulation time='//  &
                                             TRIM(CurMnDy)//' '//TRIM(CreateSysTimeIntervalString()),  &
                                                OutUnit1,OutUnit2)
    IF(WriteOutputToSQLite) THEN
      CALL UpdateSQLiteErrorRecord(TRIM(Message)//TRIM(cEnvHeader)//TRIM(EnvironmentName)//', at Simulation time='//  &
                                TRIM(CurMnDy)//' '//TRIM(CreateSysTimeIntervalString()))
    ENDIF

  ELSE
    CALL ShowErrorMessage(' **   ~~~   ** '//TRIM(Message))
    CALL ShowErrorMessage(' **   ~~~   ** '//TRIM(cEnvHeader)//TRIM(EnvironmentName)//', at Simulation time='//  &
                                             TRIM(CurMnDy)//' '//TRIM(CreateSysTimeIntervalString()),  &
                                                OutUnit1,OutUnit2)
    IF(WriteOutputToSQLite) THEN
      CALL UpdateSQLiteErrorRecord(TRIM(Message)// &
                                 TRIM(cEnvHeader)//TRIM(EnvironmentName)//', at Simulation time='//  &
                                 TRIM(CurMnDy)//' '//TRIM(CreateSysTimeIntervalString()))
    ENDIF
  ENDIF

  RETURN

END SUBROUTINE ShowContinueErrorTimeStamp

SUBROUTINE ShowMessage(Message,OutUnit1,OutUnit2)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   September 1997
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine displays a simple message on designated output files.

          ! METHODOLOGY EMPLOYED:
          ! Calls ShowErrorMessage utility routine.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataInterfaces, ONLY: ShowErrorMessage

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*) Message
  INTEGER, OPTIONAL :: OutUnit1
  INTEGER, OPTIONAL :: OutUnit2

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
  ! see DataInterfaces

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          ! na

  CALL ShowErrorMessage(' ************* '//Message,OutUnit1,OutUnit2)

  RETURN

END SUBROUTINE ShowMessage

SUBROUTINE ShowWarningError(ErrorMessage,OutUnit1,OutUnit2)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   September 1997
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine puts ErrorMessage with a Warning designation on
          ! designated output files.

          ! METHODOLOGY EMPLOYED:
          ! Calls ShowErrorMessage utility routine.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataStringGlobals
  USE DataErrorTracking
  USE DataInterfaces, ONLY: ShowErrorMessage
  USE DataGlobals,         ONLY : WarmupFlag,DoingSizing,KickOffSimulation
  USE SQLiteProcedures, ONLY: CreateSQLiteErrorRecord, WriteOutputToSQLite

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*) ErrorMessage
  INTEGER, OPTIONAL :: OutUnit1
  INTEGER, OPTIONAL :: OutUnit2

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
  ! see DataInterfaces

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER Loop

  DO Loop=1,SearchCounts
    IF (INDEX(ErrorMessage,TRIM(MessageSearch(Loop))) > 0) MatchCounts(Loop)=MatchCounts(Loop)+1
  ENDDO

  TotalWarningErrors=TotalWarningErrors+1
  IF (WarmupFlag .and. .not. DoingSizing .and. .not. KickOffSimulation .and. .not. AbortProcessing)   &
     TotalWarningErrorsDuringWarmup=TotalWarningErrorsDuringWarmup+1
  IF (DoingSizing) TotalWarningErrorsDuringSizing=TotalWarningErrorsDuringSizing+1
  CALL ShowErrorMessage(' ** Warning ** '//ErrorMessage,OutUnit1,OutUnit2)

  IF(WriteOutputToSQLite) THEN
    CALL CreateSQLiteErrorRecord(1,0,ErrorMessage,1)
  ENDIF
  RETURN

END SUBROUTINE ShowWarningError

SUBROUTINE ShowWarningMessage(ErrorMessage,OutUnit1,OutUnit2)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   September 2009
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine puts ErrorMessage with a Warning designation on
          ! designated output files.
          ! But does not bump the error count so can be used in conjunction with recurring
          ! error calls.

          ! METHODOLOGY EMPLOYED:
          ! Calls ShowErrorMessage utility routine.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataStringGlobals
  USE DataErrorTracking
  USE DataInterfaces, ONLY: ShowErrorMessage
  USE SQLiteProcedures, ONLY: CreateSQLiteErrorRecord, WriteOutputToSQLite

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*) ErrorMessage
  INTEGER, OPTIONAL :: OutUnit1
  INTEGER, OPTIONAL :: OutUnit2

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
  ! see DataInterfaces

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER Loop

  DO Loop=1,SearchCounts
    IF (INDEX(ErrorMessage,TRIM(MessageSearch(Loop))) > 0) MatchCounts(Loop)=MatchCounts(Loop)+1
  ENDDO

  CALL ShowErrorMessage(' ** Warning ** '//ErrorMessage,OutUnit1,OutUnit2)
  IF(WriteOutputToSQLite) THEN
    CALL CreateSQLiteErrorRecord(1,0,ErrorMessage,0)
  ENDIF

  RETURN

END SUBROUTINE ShowWarningMessage

SUBROUTINE ShowRecurringSevereErrorAtEnd(Message,MsgIndex,ReportMaxOf,ReportMinOf,ReportSumOf,  &
                                                          ReportMaxUnits,ReportMinUnits,ReportSumUnits)


          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Michael J. Witte
          !       DATE WRITTEN   August 2004
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine stores a recurring ErrorMessage with a Severe designation
          ! for output at the end of the simulation with automatic tracking of number
          ! of occurences and optional tracking of associated min, max, and sum values

          ! METHODOLOGY EMPLOYED:
          ! Calls StoreRecurringErrorMessage utility routine.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataPrecisionGlobals
  USE DataStringGlobals
  USE DataErrorTracking

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*)              :: Message     ! Message automatically written to "error file" at end of simulation
  INTEGER, INTENT(INOUT)        :: MsgIndex    ! Recurring message index, if zero, next available index is assigned
  REAL(r64),    INTENT(IN), OPTIONAL :: ReportMaxOf ! Track and report the max of the values passed to this argument
  REAL(r64),    INTENT(IN), OPTIONAL :: ReportMinOf ! Track and report the min of the values passed to this argument
  REAL(r64),    INTENT(IN), OPTIONAL :: ReportSumOf ! Track and report the sum of the values passed to this argument
  CHARACTER(len=*), INTENT(IN), OPTIONAL :: ReportMaxUnits ! optional char string (<=15 length) of units for max value
  CHARACTER(len=*), INTENT(IN), OPTIONAL :: ReportMinUnits ! optional char string (<=15 length) of units for min value
  CHARACTER(len=*), INTENT(IN), OPTIONAL :: ReportSumUnits ! optional char string (<=15 length) of units for sum value

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
  INTERFACE
    SUBROUTINE StoreRecurringErrorMessage(ErrorMessage,ErrorMsgIndex,ErrorReportMaxOf,ErrorReportMinOf,ErrorReportSumOf,  &
                                                                     ErrorReportMaxUnits,ErrorReportMinUnits,ErrorReportSumUnits)
    USE DataPrecisionGlobals
    !  Use for recurring "warning" error messages shown once at end of simulation
    !  with count of occurences and optional max, min, sum
    CHARACTER(len=*) ErrorMessage    ! Message automatically written to "error file" at end of simulation
    INTEGER, INTENT(INOUT)        :: ErrorMsgIndex    ! Recurring message index, if zero, next available index is assigned
    REAL(r64),    INTENT(IN), OPTIONAL :: ErrorReportMaxOf ! Track and report the max of the values passed to this argument
    REAL(r64),    INTENT(IN), OPTIONAL :: ErrorReportMinOf ! Track and report the min of the values passed to this argument
    REAL(r64),    INTENT(IN), OPTIONAL :: ErrorReportSumOf ! Track and report the sum of the values passed to this argument
    CHARACTER(len=*), INTENT(IN), OPTIONAL :: ErrorReportMaxUnits ! Units for "max" reporting
    CHARACTER(len=*), INTENT(IN), OPTIONAL :: ErrorReportMinUnits ! Units for "min" reporting
    CHARACTER(len=*), INTENT(IN), OPTIONAL :: ErrorReportSumUnits ! Units for "sum" reporting
    END SUBROUTINE
  END INTERFACE

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER Loop

  DO Loop=1,SearchCounts
    IF (INDEX(Message,TRIM(MessageSearch(Loop))) > 0) MatchCounts(Loop)=MatchCounts(Loop)+1
  ENDDO

  TotalSevereErrors=TotalSevereErrors+1
  CALL StoreRecurringErrorMessage(' ** Severe  ** '//Message,MsgIndex,ReportMaxOf,ReportMinOf,ReportSumOf,  &
                                                                      ReportMaxUnits,ReportMinUnits,ReportSumUnits)

  RETURN

END SUBROUTINE ShowRecurringSevereErrorAtEnd

SUBROUTINE ShowRecurringWarningErrorAtEnd(Message,MsgIndex,ReportMaxOf,ReportMinOf,ReportSumOf,  &
                                                           ReportMaxUnits,ReportMinUnits,ReportSumUnits)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Michael J. Witte
          !       DATE WRITTEN   August 2004
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine stores a recurring ErrorMessage with a Warning designation
          ! for output at the end of the simulation with automatic tracking of number
          ! of occurences and optional tracking of associated min, max, and sum values

          ! METHODOLOGY EMPLOYED:
          ! Calls StoreRecurringErrorMessage utility routine.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataPrecisionGlobals
  USE DataStringGlobals
  USE DataErrorTracking

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*)              :: Message     ! Message automatically written to "error file" at end of simulation
  INTEGER, INTENT(INOUT)        :: MsgIndex    ! Recurring message index, if zero, next available index is assigned
  REAL(r64),    INTENT(IN), OPTIONAL :: ReportMaxOf ! Track and report the max of the values passed to this argument
  REAL(r64),    INTENT(IN), OPTIONAL :: ReportMinOf ! Track and report the min of the values passed to this argument
  REAL(r64),    INTENT(IN), OPTIONAL :: ReportSumOf ! Track and report the sum of the values passed to this argument
  CHARACTER(len=*), INTENT(IN), OPTIONAL :: ReportMaxUnits ! optional char string (<=15 length) of units for max value
  CHARACTER(len=*), INTENT(IN), OPTIONAL :: ReportMinUnits ! optional char string (<=15 length) of units for min value
  CHARACTER(len=*), INTENT(IN), OPTIONAL :: ReportSumUnits ! optional char string (<=15 length) of units for sum value

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
  INTERFACE
    SUBROUTINE StoreRecurringErrorMessage(ErrorMessage,ErrorMsgIndex,ErrorReportMaxOf,ErrorReportMinOf,ErrorReportSumOf,  &
                                                                     ErrorReportMaxUnits,ErrorReportMinUnits,ErrorReportSumUnits)
    USE DataPrecisionGlobals
    !  Use for recurring "warning" error messages shown once at end of simulation
    !  with count of occurences and optional max, min, sum
    CHARACTER(len=*) ErrorMessage    ! Message automatically written to "error file" at end of simulation
    INTEGER, INTENT(INOUT)        :: ErrorMsgIndex    ! Recurring message index, if zero, next available index is assigned
    REAL(r64),    INTENT(IN), OPTIONAL :: ErrorReportMaxOf ! Track and report the max of the values passed to this argument
    REAL(r64),    INTENT(IN), OPTIONAL :: ErrorReportMinOf ! Track and report the min of the values passed to this argument
    REAL(r64),    INTENT(IN), OPTIONAL :: ErrorReportSumOf ! Track and report the sum of the values passed to this argument
    CHARACTER(len=*), INTENT(IN), OPTIONAL :: ErrorReportMaxUnits ! Units for "max" reporting
    CHARACTER(len=*), INTENT(IN), OPTIONAL :: ErrorReportMinUnits ! Units for "min" reporting
    CHARACTER(len=*), INTENT(IN), OPTIONAL :: ErrorReportSumUnits ! Units for "sum" reporting
    END SUBROUTINE
  END INTERFACE

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER Loop

  DO Loop=1,SearchCounts
    IF (INDEX(Message,TRIM(MessageSearch(Loop))) > 0) MatchCounts(Loop)=MatchCounts(Loop)+1
  ENDDO

  TotalWarningErrors=TotalWarningErrors+1
  CALL StoreRecurringErrorMessage(' ** Warning ** '//Message,MsgIndex,ReportMaxOf,ReportMinOf,ReportSumOf,  &
                                                                      ReportMaxUnits,ReportMinUnits,ReportSumUnits)

  RETURN

END SUBROUTINE ShowRecurringWarningErrorAtEnd

SUBROUTINE ShowRecurringContinueErrorAtEnd(Message,MsgIndex,ReportMaxOf,ReportMinOf,ReportSumOf,  &
                                                            ReportMaxUnits,ReportMinUnits,ReportSumUnits)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Michael J. Witte
          !       DATE WRITTEN   August 2004
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine stores a recurring ErrorMessage with a continue designation
          ! for output at the end of the simulation with automatic tracking of number
          ! of occurences and optional tracking of associated min, max, and sum values

          ! METHODOLOGY EMPLOYED:
          ! Calls StoreRecurringErrorMessage utility routine.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataPrecisionGlobals
  USE DataStringGlobals
  USE DataErrorTracking

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*)              :: Message     ! Message automatically written to "error file" at end of simulation
  INTEGER, INTENT(INOUT)        :: MsgIndex    ! Recurring message index, if zero, next available index is assigned
  REAL(r64),    INTENT(IN), OPTIONAL :: ReportMaxOf ! Track and report the max of the values passed to this argument
  REAL(r64),    INTENT(IN), OPTIONAL :: ReportMinOf ! Track and report the min of the values passed to this argument
  REAL(r64),    INTENT(IN), OPTIONAL :: ReportSumOf ! Track and report the sum of the values passed to this argument
  CHARACTER(len=*), INTENT(IN), OPTIONAL :: ReportMaxUnits ! optional char string (<=15 length) of units for max value
  CHARACTER(len=*), INTENT(IN), OPTIONAL :: ReportMinUnits ! optional char string (<=15 length) of units for min value
  CHARACTER(len=*), INTENT(IN), OPTIONAL :: ReportSumUnits ! optional char string (<=15 length) of units for sum value

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
  INTERFACE
    SUBROUTINE StoreRecurringErrorMessage(ErrorMessage,ErrorMsgIndex,ErrorReportMaxOf,ErrorReportMinOf,ErrorReportSumOf,  &
                                                                     ErrorReportMaxUnits,ErrorReportMinUnits,ErrorReportSumUnits)
    USE DataPrecisionGlobals
    !  Use for recurring "warning" error messages shown once at end of simulation
    !  with count of occurences and optional max, min, sum
    CHARACTER(len=*) ErrorMessage    ! Message automatically written to "error file" at end of simulation
    INTEGER, INTENT(INOUT)        :: ErrorMsgIndex    ! Recurring message index, if zero, next available index is assigned
    REAL(r64),    INTENT(IN), OPTIONAL :: ErrorReportMaxOf ! Track and report the max of the values passed to this argument
    REAL(r64),    INTENT(IN), OPTIONAL :: ErrorReportMinOf ! Track and report the min of the values passed to this argument
    REAL(r64),    INTENT(IN), OPTIONAL :: ErrorReportSumOf ! Track and report the sum of the values passed to this argument
    CHARACTER(len=*), INTENT(IN), OPTIONAL :: ErrorReportMaxUnits ! Units for "max" reporting
    CHARACTER(len=*), INTENT(IN), OPTIONAL :: ErrorReportMinUnits ! Units for "min" reporting
    CHARACTER(len=*), INTENT(IN), OPTIONAL :: ErrorReportSumUnits ! Units for "sum" reporting
    END SUBROUTINE
  END INTERFACE

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER Loop

  DO Loop=1,SearchCounts
    IF (INDEX(Message,TRIM(MessageSearch(Loop))) > 0) MatchCounts(Loop)=MatchCounts(Loop)+1
  ENDDO

  CALL StoreRecurringErrorMessage(' **   ~~~   ** '//Message,MsgIndex,ReportMaxOf,ReportMinOf,ReportSumOf,  &
                                                                      ReportMaxUnits,ReportMinUnits,ReportSumUnits)

  RETURN

END SUBROUTINE ShowRecurringContinueErrorAtEnd

SUBROUTINE StoreRecurringErrorMessage(ErrorMessage,ErrorMsgIndex,ErrorReportMaxOf,ErrorReportMinOf,ErrorReportSumOf,  &
                                                                 ErrorReportMaxUnits,ErrorReportMinUnits,ErrorReportSumUnits)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Michael J. Witte
          !       DATE WRITTEN   August 2004
          !       MODIFIED       September 2005;LKL;Added Units
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine stores a recurring ErrorMessage with
          ! for output at the end of the simulation with automatic tracking of number
          ! of occurences and optional tracking of associated min, max, and sum values

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataPrecisionGlobals
  USE DataStringGlobals
  USE DataErrorTracking
  USE DataGlobals,         ONLY : WarmupFlag,DoingSizing

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*) ErrorMessage    ! Message automatically written to "error file" at end of simulation
  INTEGER, INTENT(INOUT)        :: ErrorMsgIndex    ! Recurring message index, if zero, next available index is assigned
  REAL(r64),    INTENT(IN), OPTIONAL :: ErrorReportMaxOf ! Track and report the max of the values passed to this argument
  REAL(r64),    INTENT(IN), OPTIONAL :: ErrorReportMinOf ! Track and report the min of the values passed to this argument
  REAL(r64),    INTENT(IN), OPTIONAL :: ErrorReportSumOf ! Track and report the sum of the values passed to this argument
  CHARACTER(len=*), INTENT(IN), OPTIONAL :: ErrorReportMaxUnits ! Units for "max" reporting
  CHARACTER(len=*), INTENT(IN), OPTIONAL :: ErrorReportMinUnits ! Units for "min" reporting
  CHARACTER(len=*), INTENT(IN), OPTIONAL :: ErrorReportSumUnits ! Units for "sum" reporting

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  TYPE (RecurringErrorData),   ALLOCATABLE, DIMENSION(:) :: TempRecurringErrors

  ! If Index is zero, then assign next available index and reallocate array
  IF (ErrorMsgIndex == 0) THEN
    NumRecurringErrors = NumRecurringErrors + 1
    ErrorMsgIndex = NumRecurringErrors
    IF (NumRecurringErrors == 1) THEN
      ALLOCATE(RecurringErrors(NumRecurringErrors))
    ELSEIF (NumRecurringErrors > 1) THEN
      ALLOCATE(TempRecurringErrors(NumRecurringErrors))
      TempRecurringErrors(1:NumRecurringErrors-1)=RecurringErrors(1:NumRecurringErrors-1)
      DEALLOCATE(RecurringErrors)
      ALLOCATE(RecurringErrors(NumRecurringErrors))
      RecurringErrors = TempRecurringErrors
      DEALLOCATE(TempRecurringErrors)
    ENDIF
  ! The message string only needs to be stored once when a new recurring message is created
    RecurringErrors(ErrorMsgIndex)%Message = TRIM(ErrorMessage)
    RecurringErrors(ErrorMsgIndex)%Count   = 1
    IF (WarmupFlag) RecurringErrors(ErrorMsgIndex)%WarmupCount   = 1
    IF (DoingSizing) RecurringErrors(ErrorMsgIndex)%SizingCount   = 1


  ! For max, min, and sum values, store the current value when a new recurring message is created
    IF (PRESENT(ErrorReportMaxOf)) THEN
      RecurringErrors(ErrorMsgIndex)%MaxValue = ErrorReportMaxOf
      RecurringErrors(ErrorMsgIndex)%ReportMax = .TRUE.
      IF (PRESENT(ErrorReportMaxUnits)) THEN
        RecurringErrors(ErrorMsgIndex)%MaxUnits=ErrorReportMaxUnits
      ENDIF
    ENDIF
    IF (PRESENT(ErrorReportMinOf)) THEN
      RecurringErrors(ErrorMsgIndex)%MinValue = ErrorReportMinOf
      RecurringErrors(ErrorMsgIndex)%ReportMin = .TRUE.
      IF (PRESENT(ErrorReportMinUnits)) THEN
        RecurringErrors(ErrorMsgIndex)%MinUnits=ErrorReportMinUnits
      ENDIF
    ENDIF
    IF (PRESENT(ErrorReportSumOf)) THEN
      RecurringErrors(ErrorMsgIndex)%SumValue = ErrorReportSumOf
      RecurringErrors(ErrorMsgIndex)%ReportSum = .TRUE.
      IF (PRESENT(ErrorReportSumUnits)) THEN
        RecurringErrors(ErrorMsgIndex)%SumUnits=ErrorReportSumUnits
      ENDIF
    ENDIF

  ELSEIF (ErrorMsgIndex > 0) THEN
    ! Do stats and store
    RecurringErrors(ErrorMsgIndex)%Count = RecurringErrors(ErrorMsgIndex)%Count + 1
    IF (WarmupFlag) RecurringErrors(ErrorMsgIndex)%WarmupCount = RecurringErrors(ErrorMsgIndex)%WarmupCount + 1
    IF (DoingSizing) RecurringErrors(ErrorMsgIndex)%SizingCount = RecurringErrors(ErrorMsgIndex)%SizingCount + 1

    IF (PRESENT(ErrorReportMaxOf)) THEN
      RecurringErrors(ErrorMsgIndex)%MaxValue = MAX(ErrorReportMaxOf,RecurringErrors(ErrorMsgIndex)%MaxValue)
      RecurringErrors(ErrorMsgIndex)%ReportMax = .TRUE.
    ENDIF
    IF (PRESENT(ErrorReportMinOf)) THEN
      RecurringErrors(ErrorMsgIndex)%MinValue = MIN(ErrorReportMinOf,RecurringErrors(ErrorMsgIndex)%MinValue)
      RecurringErrors(ErrorMsgIndex)%ReportMin = .TRUE.
    ENDIF
    IF (PRESENT(ErrorReportSumOf)) THEN
      RecurringErrors(ErrorMsgIndex)%SumValue = ErrorReportSumOf + RecurringErrors(ErrorMsgIndex)%SumValue
      RecurringErrors(ErrorMsgIndex)%ReportSum = .TRUE.
    ENDIF
  ELSE
    ! If ErrorMsgIndex < 0, then do nothing
  ENDIF

  RETURN

END SUBROUTINE StoreRecurringErrorMessage

SUBROUTINE ShowErrorMessage(ErrorMessage,OutUnit1,OutUnit2)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   December 1997
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine displays the error messages on the indicated
          ! file unit numbers, in addition to the "standard error output"
          ! unit.

          ! METHODOLOGY EMPLOYED:
          ! If arguments OutUnit1 and/or OutUnit2 are present the
          ! error message is written to these as well and the standard one.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataStringGlobals, ONLY: VerString,IDDVerString
  USE DataInterfaces, ONLY: ShowFatalError
  USE DataGlobals,    ONLY: DoingInputProcessing,CacheIPErrorFile

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*) ErrorMessage
  INTEGER, OPTIONAL :: OutUnit1
  INTEGER, OPTIONAL :: OutUnit2

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER :: ErrorFormat='(2X,A)'
  CHARACTER(len=*), PARAMETER :: fmtA='(A)'

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER,SAVE  :: TotalErrors=0        ! used to determine when to open standard error output file.
  INTEGER,SAVE  :: StandardErrorOutput
  INTEGER,EXTERNAL  :: GetNewUnitNumber
  INTEGER :: write_stat
  LOGICAL, SAVE :: ErrFileOpened=.false.

  IF (TotalErrors .eq. 0 .and. .not. ErrFileOpened) THEN
    StandardErrorOutput=GetNewUnitNumber()
    OPEN(StandardErrorOutput,FILE='eplusout.err', Action='write',iostat=write_stat)
    IF (write_stat /= 0) THEN
      CALL DisplayString('Trying to display error: "'//trim(ErrorMessage)//'"')
      CALL ShowFatalError('ShowErrorMessage: Could not open file "eplusout.err" for output (write).')
    ENDIF
    WRITE(StandardErrorOutput,'(A)') 'Program Version,'//TRIM(VerString)//','//TRIM(IDDVerString)
    ErrFileOpened=.true.
  ENDIF

  IF (.not. DoingInputProcessing) THEN
    TotalErrors=TotalErrors+1
    WRITE(StandardErrorOutput,ErrorFormat) TRIM(ErrorMessage)
  ELSE
    WRITE(CacheIPErrorFile,fmtA) TRIM(ErrorMessage)
  ENDIF
  IF (PRESENT(OutUnit1)) THEN
    WRITE(OutUnit1,ErrorFormat) TRIM(ErrorMessage)
  ENDIF
  IF (PRESENT(OutUnit2)) THEN
    WRITE(OutUnit2,ErrorFormat) TRIM(ErrorMessage)
  ENDIF


  RETURN

END SUBROUTINE ShowErrorMessage

SUBROUTINE SummarizeErrors

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   March 2003
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine provides a summary of certain errors that might
          ! otherwise get lost in the shuffle of many similar messages.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataErrorTracking
  USE DataInterfaces, ONLY: ShowMessage

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
  INTEGER Loop
  INTEGER StartC
  INTEGER EndC

  IF (Any(MatchCounts > 0)) THEN
    CALL ShowMessage(' ')
    CALL ShowMessage('===== Final Error Summary =====')
    CALL ShowMessage('The following error categories occurred.  Consider correcting or noting.')
    DO Loop=1,SearchCounts
      IF (MatchCounts(Loop) > 0) THEN
        CALL ShowMessage(TRIM(Summaries(Loop)))
        IF (MoreDetails(Loop) /= ' ') THEN
          StartC=1
          EndC=Len_Trim(MoreDetails(Loop))
          DO WHILE (EndC > 0)
            EndC=INDEX(MoreDetails(Loop)(StartC:),'<CR')
            CALL ShowMessage('..'//MoreDetails(Loop)(StartC:StartC+EndC-2))
            IF (MoreDetails(Loop)(StartC+EndC-1:StartC+EndC+3) == '<CRE>') EXIT
            StartC=StartC+EndC+3
            EndC=Len_Trim(MoreDetails(Loop)(StartC:))
          ENDDO
        ENDIF
      ENDIF
    ENDDO
    CALL ShowMessage(' ')
  ENDIF

  RETURN

END SUBROUTINE SummarizeErrors

SUBROUTINE ShowRecurringErrors

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   March 2003
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine provides a summary of certain errors that might
          ! otherwise get lost in the shuffle of many similar messages.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataErrorTracking
  USE DataInterfaces, ONLY: ShowMessage
  USE General, ONLY: RoundSigDigits,RemoveTrailingZeros
  USE SQLiteProcedures, ONLY: UpdateSQLiteErrorRecord,CreateSQLiteErrorRecord
  USE SQLiteProcedures, ONLY: WriteOutputToSQLite

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER (len=15), PARAMETER :: StatMessageStart = ' **   ~~~   ** '

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER Loop
  CHARACTER (len=MaxRecurringErrorMsgLength) StatMessage, MaxOut, MinOut, SumOut

  IF (NumRecurringErrors > 0) THEN
    CALL ShowMessage(' ')
    CALL ShowMessage('===== Recurring Error Summary =====')
    CALL ShowMessage('The following recurring error messages occurred.')
    DO Loop=1,NumRecurringErrors
      ! Suppress reporting the count if it is a continue error
      IF (RecurringErrors(Loop)%Message(1:15) == ' **   ~~~   ** ') THEN
        CALL ShowMessage(TRIM(RecurringErrors(Loop)%Message))
        IF(WriteOutputToSQLite) THEN
          CALL UpdateSQLiteErrorRecord(TRIM(RecurringErrors(Loop)%Message))
        ENDIF
      ELSE
        CALL ShowMessage(' ')
        CALL ShowMessage(TRIM(RecurringErrors(Loop)%Message))
        CALL ShowMessage(StatMessageStart//'  This error occurred '//TRIM(RoundSigDigits(RecurringErrors(Loop)%Count))//  &
           ' total times;')
        CALL ShowMessage(StatMessageStart//'  during Warmup '//TRIM(RoundSigDigits(RecurringErrors(Loop)%WarmupCount))//' times;')
        CALL ShowMessage(StatMessageStart//'  during Sizing '//TRIM(RoundSigDigits(RecurringErrors(Loop)%SizingCount))//' times.')
        IF(WriteOutputToSQLite) THEN
          IF (RecurringErrors(Loop)%Message(1:15) == ' ** Warning ** ') THEN
            CALL CreateSQLiteErrorRecord(1,0,TRIM(RecurringErrors(Loop)%Message(16:)),RecurringErrors(Loop)%Count)
          ELSEIF (RecurringErrors(Loop)%Message(1:15) == ' ** Severe  ** ') THEN
            CALL CreateSQLiteErrorRecord(1,1,TRIM(RecurringErrors(Loop)%Message(16:)),RecurringErrors(Loop)%Count)
          ENDIF
        ENDIF
      ENDIF
      StatMessage = ' '
      IF (RecurringErrors(Loop)%ReportMax) THEN
        MaxOut = RoundSigDigits(RecurringErrors(Loop)%MaxValue,6)
        MaxOut = RemoveTrailingZeros(MaxOut)
        StatMessage = TRIM(StatMessage)//'  Max='//TRIM(Maxout)//' '//TRIM(RecurringErrors(Loop)%MaxUnits)
      ENDIF
      IF (RecurringErrors(Loop)%ReportMin) THEN
        MinOut = RoundSigDigits(RecurringErrors(Loop)%MinValue,6)
        MinOut = RemoveTrailingZeros(MinOut)
        StatMessage = TRIM(StatMessage)//'  Min='//TRIM(Minout)//' '//TRIM(RecurringErrors(Loop)%MinUnits)
      ENDIF
      IF (RecurringErrors(Loop)%ReportSum) THEN
        SumOut = RoundSigDigits(RecurringErrors(Loop)%SumValue,6)
        SumOut = RemoveTrailingZeros(SumOut)
        StatMessage = TRIM(StatMessage)//'  Sum='//TRIM(Sumout)//' '//TRIM(RecurringErrors(Loop)%SumUnits)
      ENDIF
      IF (RecurringErrors(Loop)%ReportMax .OR. RecurringErrors(Loop)%ReportMin .OR. RecurringErrors(Loop)%ReportSum) THEN
        CALL ShowMessage(StatMessageStart//TRIM(StatMessage))
      ENDIF
    ENDDO
    CALL ShowMessage(' ')
  ENDIF

  RETURN

END SUBROUTINE ShowRecurringErrors

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

