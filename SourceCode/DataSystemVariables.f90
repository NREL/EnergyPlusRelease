MODULE DataSystemVariables      ! EnergyPlus Data-Only Module

          ! MODULE INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   May 2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS MODULE:
          ! This data-only module is a repository for system (such as environment) variables that are set
          ! before a run or set of runs.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! OTHER NOTES:
          ! na

          ! USE STATEMENTS:
USE DataPrecisionGlobals
USE DataStringGlobals, ONLY: pathChar, altpathChar, CurrentWorkingFolder, ProgramPath

IMPLICIT NONE   ! Enforce explicit typing of all variables

PUBLIC          ! By definition, all variables which are placed in this data
                ! -only module should be available to other modules and routines.
                ! Thus, all variables in this module must be PUBLIC.


          ! MODULE PARAMETER DEFINITIONS:
  INTEGER, PARAMETER :: iASCII_CR=13      ! endline value when just CR instead of CR/LF
  INTEGER, PARAMETER :: iUnicode_end=0    ! endline value when Unicode file
  CHARACTER(len=1), PARAMETER :: tabchar=CHAR(9)
  INTEGER, PARAMETER :: GoodIOStatValue=0 ! good value for IOStat during reads/writes
  INTEGER, PARAMETER :: MaxTimingStringLength=250 ! string length for timing string array

  CHARACTER(len=*),  PARAMETER :: DDOnlyEnvVar='DDONLY'                ! Only run design days
  CHARACTER(len=*),  PARAMETER :: ReverseDDEnvVar='REVERSEDD'          ! Reverse DD during run
  CHARACTER(len=*),  PARAMETER :: FullAnnualSimulation='FULLANNUALRUN' ! Generate annual run
  CHARACTER(len=*),  PARAMETER :: cDeveloperFlag='DeveloperFlag'
  CHARACTER(len=*),  PARAMETER :: cDisplayAllWarnings='DisplayAllWarnings'
  CHARACTER(len=*),  PARAMETER :: cDisplayExtraWarnings='DisplayExtraWarnings'
  CHARACTER(len=*),  PARAMETER :: cDisplayAdvancedReportVariables='DisplayAdvancedReportVariables'
  CHARACTER(len=*),  PARAMETER :: cDisplayUnusedObjects='DisplayUnusedObjects'
  CHARACTER(len=*),  PARAMETER :: cDisplayUnusedSchedules='DisplayUnusedSchedules'
  CHARACTER(len=*),  PARAMETER :: cDisplayZoneAirHeatBalanceOffBalance='DisplayZoneAirHeatBalanceOffBalance'
  CHARACTER(len=*),  PARAMETER :: cSortIDD='SortIDD'
  CHARACTER(len=*),  PARAMETER :: cReportDuringWarmup='ReportDuringWarmup'
  CHARACTER(len=*),  PARAMETER :: cIgnoreSolarRadiation='IgnoreSolarRadiation'
  CHARACTER(len=*),  PARAMETER :: cIgnoreBeamRadiation='IgnoreBeamRadiation'
  CHARACTER(len=*),  PARAMETER :: cIgnoreDiffuseRadiation='IgnoreDiffuseRadiation'
  CHARACTER(len=*),  PARAMETER :: cSutherlandHodgman='SutherlandHodgman'
  CHARACTER(len=*),  PARAMETER :: cMinimalSurfaceVariables='CreateMinimalSurfaceVariables'
  CHARACTER(len=*),  PARAMETER :: cMinimalShadowing='MinimalShadowing'
  CHARACTER(len=*),  PARAMETER :: cNumThreads='OMP_NUM_THREADS'
  CHARACTER(len=*),  PARAMETER :: cepNumThreads='EP_OMP_NUM_THREADS'
  CHARACTER(len=*),  PARAMETER :: cNumActiveSims='cntActv'
  CHARACTER(len=*),  PARAMETER :: cInputPath1='epin'  ! EP-Launch setting.  Full path + project name
  CHARACTER(len=*),  PARAMETER :: cInputPath2='input_path'  ! RunEplus.bat setting.  Full path
  CHARACTER(len=*),  PARAMETER :: cProgramPath='program_path'
  CHARACTER(len=*),  PARAMETER :: cTimingFlag='TimingFlag'
  CHARACTER(LEN=*),  PARAMETER :: TrackAirLoopEnvVar='TRACK_AIRLOOP' ! To generate a file with runtime statistics
                                                                     ! for each controller on each air loop
  CHARACTER(LEN=*),  PARAMETER :: TraceAirLoopEnvVar='TRACE_AIRLOOP'  ! To generate a trace file with the converged
                                            ! solutions of all controllers on each air loop at each call to SimAirLoop()
  CHARACTER(LEN=*),  PARAMETER :: TraceHVACControllerEnvVar='TRACE_HVACCONTROLLER' ! To generate a trace file for
                                            !  each individual HVAC controller with all controller iterations

  CHARACTER(len=*),  PARAMETER :: MinReportFrequencyEnvVar='MINREPORTFREQUENCY' ! environment var for reporting frequency.

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! MODULE VARIABLE DECLARATIONS:
  LOGICAL :: DDOnly=.false.              ! TRUE if design days (sizingperiod:*) only are to be run.
  LOGICAL :: ReverseDD=.false.           ! TRUE if reverse design days (reordering sizingperiod:*) are to be run.
  LOGICAL :: FullAnnualRun=.false.       ! TRUE if full annual simulation is to be run.
  LOGICAL :: DeveloperFlag=.false.       ! TRUE if developer flag is turned on. (turns on more displays to console)
  LOGICAL :: TimingFlag=.false.          ! TRUE if timing flag is turned on. (turns on more timing displays to console)
  LOGICAL :: SutherlandHodgman=.true.    ! TRUE if SutherlandHodgman algorithm for polygon clipping is to be used.
  LOGICAL :: DetailedSkyDiffuseAlgorithm=.false.  ! use detailed diffuse shading algorithm for sky (shading transmittance varies)
  LOGICAL :: DetailedSolarTimestepIntegration=.false. ! when true, use detailed timestep integration for all solar,shading, etc.
  LOGICAL :: TrackAirLoopEnvFlag=.FALSE. ! If TRUE generates a file with runtime statistics for each HVAC
                                         !  controller on each air loop
  LOGICAL :: TraceAirLoopEnvFlag=.FALSE. ! If TRUE generates a trace file with the converged solutions of all
                                         ! HVAC controllers on each air loop at each call to SimAirLoop()
  LOGICAL :: TraceHVACControllerEnvFlag=.FALSE. ! If TRUE generates a trace file for each individual HVAC
                                                ! controller with all controller iterations
  LOGICAL :: ReportDuringWarmup=.false.  ! True when the report outputs even during warmup
  LOGICAL :: ReportDetailedWarmupConvergence=.false.  ! True when the detailed warmup convergence is requested
  LOGICAL :: UpdateDataDuringWarmupExternalInterface=.false.   ! variable sets in the external interface.
                                                               ! This update the value during the warmup added for FMI
  REAL(r64)   :: Elapsed_Time=0.0d0          ! For showing elapsed time at end of run
  REAL(r64)   :: Time_Start=0.0d0            ! Call to CPU_Time for start time of simulation
  REAL(r64)   :: Time_Finish=0.0d0           ! Call to CPU_Time for end time of simulation
  CHARACTER(len=15)  :: cMinReportFrequency = ' '   ! String for minimum reporting frequency
  INTEGER            :: MinReportFrequency = -2     ! Frequency var turned into integer during get report var input.
  LOGICAL :: SortedIDD=.true.   ! after processing, use sorted IDD to obtain Defs, etc.
  LOGICAL :: lMinimalShadowing=.false.  ! TRUE if MinimalShadowing is to override Solar Distribution flag
  CHARACTER(len=500) :: TempFullFileName=' '
  CHARACTER(len=255) :: envinputpath1=' '
  CHARACTER(len=255) :: envinputpath2=' '
  CHARACTER(len=255) :: envprogrampath=' '
  LOGICAL :: TestAllPaths=.false.
  INTEGER :: iEnvSetThreads = 0
  LOGICAL :: lEnvSetThreadsInput=.false.
  INTEGER :: iepEnvSetThreads = 0
  LOGICAL :: lepSetThreadsInput=.false.
  INTEGER :: iIDFSetThreads = 0
  LOGICAL :: lIDFSetThreadsInput=.false.
  INTEGER :: inumActiveSims = 1
  LOGICAL :: lnumActiveSims=.false.
  INTEGER :: MaxNumberOfThreads = 1
  INTEGER :: NumberIntRadThreads = 1
  INTEGER :: iNominalTotSurfaces = 0
  LOGICAL :: Threading=.false.

CONTAINS

subroutine CheckForActualFileName(originalInputFileName,FileFound,CheckedFileName)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   October 2011
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! With the Windows version, there are subfolders set and the input file names may not
          ! be accurate. This searches a few folders (CurrentWorkingFolder, Program folder) to see
          ! if the file can be found. (It may have been input with full path so that is checked first.)

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: originalInputFileName ! name as input for object
  LOGICAL, INTENT(INOUT) :: FileFound           ! Set to true if file found and is in CheckedFileName
  CHARACTER(len=*), INTENT(INOUT) :: CheckedFileName  ! Blank if not found.

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER :: blank=' '

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  LOGICAL :: FileExist
  INTEGER, SAVE :: EchoInputFile  ! found unit number for "eplusout.audit"
  INTEGER, EXTERNAL :: FindUnitNumber
  LOGICAL, SAVE :: firstTime=.true.
  CHARACTER(len=LEN(originalInputFileName)) :: InputFileName ! save for changing out path characters
  INTEGER :: pos

  IF (firstTime) THEN
    EchoInputFile=FindUnitNumber('eplusout.audit')
    envinputpath1=blank
    CALL Get_Environment_Variable(cInputPath1,envinputpath1)
    IF (envinputpath1 /= blank) THEN
      pos=INDEX(envinputpath1,pathchar,.true.)  ! look backwards for pathchar
      IF (Pos /= 0) envinputpath1=envinputpath1(1:pos)
    ENDIF
    envinputpath2=blank
    CALL Get_Environment_Variable(cInputPath2,envinputpath2)
    programpath=blank
    CALL Get_Environment_Variable(cProgramPath,programpath)
    firsttime=.false.
  ENDIF

  CheckedFileName=blank
  InputFileName=originalInputFileName
  pos=INDEX(InputFileName,AltPathChar)
  DO WHILE(pos > 0)
    InputFileName(pos:pos)=PathChar
    pos=INDEX(InputFileName,AltPathChar)
  ENDDO

  INQUIRE(File=trim(InputFileName),Exist=FileExist)
  IF (FileExist) THEN
    FileFound=.true.
    CheckedFileName=InputFileName
    WRITE(EchoInputFile,'(A)') 'found (user input)='//trim(InputFileName)
    RETURN
  ELSE
    WRITE(EchoInputFile,'(A)') 'not found (user input)='//trim(InputFileName)
  ENDIF

  ! Look relative to input path
  INQUIRE(File=trim(envinputpath1)//trim(InputFileName),Exist=FileExist)
  IF (FileExist) THEN
    FileFound=.true.
    CheckedFileName=trim(envinputpath1)//trim(InputFileName)
    WRITE(EchoInputFile,'(A)') 'found (epin)='//trim(CheckedFileName)
    RETURN
  ELSE
    WRITE(EchoInputFile,'(A)') 'not found (epin)='//trim(envinputpath1)//trim(InputFileName)
  ENDIF

  ! Look relative to input path
  INQUIRE(File=trim(envinputpath2)//trim(InputFileName),Exist=FileExist)
  IF (FileExist) THEN
    FileFound=.true.
    CheckedFileName=trim(envinputpath2)//trim(InputFileName)
    WRITE(EchoInputFile,'(A)') 'found (input_path)='//trim(CheckedFileName)
    RETURN
  ELSE
    WRITE(EchoInputFile,'(A)') 'not found (input_path)='//trim(envinputpath2)//trim(InputFileName)
  ENDIF

  ! Look relative to program path
  INQUIRE(File=trim(envprogrampath)//trim(InputFileName),Exist=FileExist)
  IF (FileExist) THEN
    FileFound=.true.
    CheckedFileName=trim(envprogrampath)//trim(InputFileName)
    WRITE(EchoInputFile,'(A)') 'found (program_path)='//trim(CheckedFileName)
    RETURN
  ELSE
    WRITE(EchoInputFile,'(A)') 'not found (program_path)='//trim(envprogrampath)//trim(InputFileName)
  ENDIF

  IF (.not. TestAllPaths) RETURN

  ! Look relative to current working folder
  INQUIRE(File=trim(CurrentWorkingFolder)//trim(InputFileName),Exist=FileExist)
  IF (FileExist) THEN
    FileFound=.true.
    CheckedFileName=trim(CurrentWorkingFolder)//trim(InputFileName)
    WRITE(EchoInputFile,'(A)') 'found (CWF)='//trim(CheckedFileName)
    RETURN
  ELSE
    WRITE(EchoInputFile,'(A)') 'not found (CWF)='//trim(CurrentWorkingFolder)//trim(InputFileName)
  ENDIF

  ! Look relative to program path
  INQUIRE(File=trim(ProgramPath)//trim(InputFileName),Exist=FileExist)
  IF (FileExist) THEN
    FileFound=.true.
    CheckedFileName=trim(ProgramPath)//trim(InputFileName)
    WRITE(EchoInputFile,'(A)') 'found (program path - ini)='//trim(CheckedFileName)
    RETURN
  ELSE
    WRITE(EchoInputFile,'(A)') 'not found (program path - ini)='//trim(ProgramPath)//trim(InputFileName)
  ENDIF

  RETURN

end subroutine CheckForActualFileName

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

END MODULE DataSystemVariables
