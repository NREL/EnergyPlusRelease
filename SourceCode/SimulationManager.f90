#include "Timer.h"

! HBIRE_USE_OMP defined, then openMP instructions are used.  Compiler may have to have switch for openmp
! HBIRE_NO_OMP defined, then old code is used without any openmp instructions

! HBIRE - loop in HeatBalanceIntRadExchange.f90
#ifdef HBIRE_USE_OMP
#undef HBIRE_NO_OMP
#else
#define HBIRE_NO_OMP
#endif

MODULE SimulationManager        ! EnergyPlus Simulation Module

          ! MODULE INFORMATION:
          !       AUTHOR         Rick Strand
          !       DATE WRITTEN   January 1997
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS MODULE:
          ! This module contains the main driver routine which manages the major
          ! control loops of the EnergyPlus simulation.  This module is also
          ! responsible for setting the global environment flags for these
          ! loops.

          ! METHODOLOGY EMPLOYED:
          ! This module was constructed from the remnants of (I)BLAST routines
          ! SIMBLD (Simulate Building), SIMZG (Simulate Zone Group), and SIMZGD
          ! (Simulate Zone Group for a Day).

          ! REFERENCES:
          ! (I)BLAST legacy code, internal Reverse Engineering documentation,
          ! and internal Evolutionary Engineering documentation.

          ! USE STATEMENTS:
USE DataPrecisionGlobals
USE DataGlobals
USE DataSizing
USE DataReportingFlags
USE DataInterfaces
USE HeatBalanceManager
USE WeatherManager
USE ExternalInterface

IMPLICIT NONE    ! Enforce explicit typing of all variables

PRIVATE

          ! MODULE PARAMETER DEFINITIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! MODULE VARIABLE DECLARATIONS:
LOGICAL :: RunPeriodsInInput=.false.
LOGICAL :: RunControlInInput=.false.

          ! SUBROUTINE SPECIFICATIONS FOR MODULE SimulationManager

PUBLIC  ManageSimulation
PRIVATE GetProjectData
PRIVATE OpenOutputFiles
PRIVATE CloseOutputFiles
PRIVATE SetupSimulation
PRIVATE CheckForMisMatchedEnvironmentSpecifications
PRIVATE CheckForRequestedReporting
PUBLIC  ReportLoopConnections

CONTAINS

          ! MODULE SUBROUTINES:

SUBROUTINE ManageSimulation     ! Main driver routine for this module

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Rick Strand
          !       DATE WRITTEN   January 1997
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is the main driver of the simulation manager module.
          ! It contains the main environment-time loops for the building
          ! simulation.  This includes the environment loop, a day loop, an
          ! hour loop, and a time step loop.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataHVACGlobals,     ONLY: TimeStepSys
  USE DataEnvironment,     ONLY: EnvironmentName,CurMnDy,CurrentOverallSimDay,TotalOverallSimDays,TotDesDays,  &
                                 TotRunDesPersDays,EndMonthFlag
  USE InputProcessor,      ONLY: GetNumRangeCheckErrorsFound,GetNumObjectsFound
  USE SizingManager,       ONLY: ManageSizing
  USE ExteriorEnergyUse,   ONLY: ManageExteriorEnergyUse
  USE OutputReportTabular, ONLY: WriteTabularReports,OpenOutputTabularFile,CloseOutputTabularFile
  USE DataErrorTracking,   ONLY: AskForConnectionsReport,ExitDuringSimulations
  USE OutputProcessor,     ONLY: SetupTimePointers, ReportForTabularReports
  USE CostEstimateManager, ONLY: SimCostEstimate
  USE EconomicTariff,      ONLY: ComputeTariff,WriteTabularTariffReports  !added for computing annual utility costs
  USE General,             ONLY: TrimSigDigits
  USE OutputReportPredefined, ONLY: SetPredefinedTables
  USE HVACControllers,     ONLY: DumpAirLoopStatistics
  USE NodeInputManager,    ONLY: SetupNodeVarsForReporting, CheckMarkedNodes
  USE BranchNodeConnections, ONLY: CheckNodeConnections,TestCompSetInletOutletNodes
  Use PollutionModule,     ONLY: SetupPollutionMeterReporting, SetupPollutionCalculations, CheckPollutionMeterReporting
  USE SystemReports,       ONLY: ReportAirLoopConnections, CreateEnergyReportStructure
  USE BranchInputManager,  ONLY: ManageBranchInput,TestBranchIntegrity,InvalidBranchDefinitions
  USE ManageElectricPower, ONLY: VerifyCustomMetersElecPowerMgr
  USE MixedAir,            ONLY: CheckControllerLists
  USE EMSManager ,         ONLY: CheckIFAnyEMS, ManageEMS
  USE EconomicLifeCycleCost, ONLY: GetInputForLifeCycleCost, ComputeLifeCycleCostAndReport
  USE SQLiteProcedures,    ONLY: WriteOutputToSQLite, CreateSQLiteSimulationsRecord, InitializeIndexes, &
                                 CreateSQLiteEnvironmentPeriodRecord,CreateZoneExtendedOutput, SQLiteBegin, SQLiteCommit
  USE DemandManager,       ONLY: InitDemandManagers
  USE PlantManager,        ONLY: CheckIfAnyPlant
  USE CurveManager,        ONLY: InitCurveReporting
  USE DataTimings
  USE DataSystemVariables, ONLY: DeveloperFlag, TimingFlag, FullAnnualRun
  USE SetPointManager,     ONLY: CheckIFAnyIdealCondEntSetPoint
  USE Psychrometrics,      ONLY: InitializePsychRoutines
  USE FaultsManager

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  LOGICAL, SAVE :: Available ! an environment is available to process
  LOGICAL, SAVE :: ErrorsFound=.false.
  LOGICAL, SAVE :: TerminalError = .FALSE.
  LOGICAL       :: SimsDone
  LOGICAL       :: ErrFound
!  real(r64) :: t0,t1,st0,st1

!  CHARACTER(len=70) :: tdstring
!  CHARACTER(len=138) :: tdstringlong

  INTEGER :: EnvCount

          ! FLOW:
  CALL PostIPProcessing

  CALL InitializePsychRoutines

  BeginSimFlag = .TRUE.
  BeginFullSimFlag = .FALSE.
  DoOutputReporting = .FALSE.
  DisplayPerfSimulationFlag=.false.
  DoWeatherInitReporting=.false.
  RunPeriodsInInput=(GetNumObjectsFound('RunPeriod')>0 .or. GetNumObjectsFound('RunPeriod:CustomRange')>0 .or. FullAnnualRun)
  AskForConnectionsReport=.false.    ! set to false until sizing is finished

  CALL OpenOutputFiles
  CALL CheckThreading
  CALL GetProjectData
  CALL CheckForMisMatchedEnvironmentSpecifications
  CALL CheckForRequestedReporting
  CALL SetPredefinedTables

  CALL SetupTimePointers('Zone',TimeStepZone)  ! Set up Time pointer for HB/Zone Simulation
  Call SetupTimePointers('HVAC',TimeStepSys)

  CALL CheckIFAnyEMS
  CALL CheckIFAnyPlant

  CALL CheckIFAnyIdealCondEntSetPoint

  CALL CheckAndReadFaults

  CALL ManageBranchInput  ! just gets input and returns.

  DoingSizing = .TRUE.
  CALL ManageSizing

  BeginFullSimFlag = .TRUE.
  SimsDone=.false.
  IF (DoDesDaySim .OR. DoWeathSim) THEN
    DoOutputReporting = .TRUE.
  END IF
  DoingSizing = .FALSE.

  IF ((DoZoneSizing .or. DoSystemSizing .or. DoPlantSizing) .and.   &
      .not. (DoDesDaySim .or. (DoWeathSim .and. RunPeriodsInInput) ) ) THEN
    CALL ShowWarningError('ManageSimulation: Input file has requested Sizing Calculations but no Simulations are requested '//  &
       '(in SimulationControl object). Succeeding warnings/errors may be confusing.')
  ENDIF
  Available=.true.

  IF (InvalidBranchDefinitions) THEN
    CALL ShowFatalError('Preceding error(s) in Branch Input cause termination.')
  ENDIF

  CALL DisplayString('Initializing Simulation')
  KickOffSimulation=.true.

  CALL ResetEnvironmentCounter
  CALL SetupSimulation(ErrorsFound)
  CALL InitCurveReporting

  AskForConnectionsReport=.true.    ! set to true now that input processing and sizing is done.
  KickOffSimulation=.false.
  WarmupFlag=.false.
  DoWeatherInitReporting=.true.

!  Note:  All the inputs have been 'gotten' by the time we get here.
  ErrFound=.false.
  IF (DoOutputReporting) THEN
    CALL DisplayString('Reporting Surfaces')

    CALL ReportSurfaces

    CALL SetupNodeVarsForReporting
    MetersHaveBeenInitialized=.true.
    CALL SetupPollutionMeterReporting
    CALL UpdateMeterReporting
    CALL CheckPollutionMeterReporting
    CALL VerifyCustomMetersElecPowerMgr
    CALL SetupPollutionCalculations
    CALL InitDemandManagers

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
    CALL CheckControllerLists(ErrFound)
    IF (ErrFound) TerminalError = .TRUE.

    IF (DoDesDaySim .OR. DoWeathSim) THEN
      CALL ReportLoopConnections
      CALL ReportAirLoopConnections
      CALL ReportNodeConnections
      ! Debug reports
!      CALL ReportCompSetMeterVariables
!      CALL ReportParentChildren
    END IF

    CALL CreateEnergyReportStructure

    CALL ManageEMS(emsCallFromSetupSimulation) ! point to finish setup processing EMS, sensor ready now

    CALL ProduceRDDMDD

    IF (TerminalError) THEN
      CALL ShowFatalError('Previous Conditions cause program termination.')
    END IF
  END IF

  IF (WriteOutputToSQLite) THEN
    CALL SQLiteBegin
    CALL CreateSQLiteSimulationsRecord(1)
    CALL SQLiteCommit
  END IF

  CALL GetInputForLifeCycleCost !must be prior to WriteTabularReports -- do here before big simulation stuff.

  CALL ShowMessage('Beginning Simulation')
  CALL ResetEnvironmentCounter

  EnvCount=0
  WarmupFlag=.true.

  DO WHILE (Available)

    CALL GetNextEnvironment(Available,ErrorsFound)

    IF (.not. Available) EXIT
    IF (ErrorsFound) EXIT
    IF ( (.NOT. DoDesDaySim) .AND. (KindOfSim /= ksRunPeriodWeather) ) CYCLE
    IF ( (.NOT. DoWeathSim) .AND. (KindOfSim == ksRunPeriodWeather)) CYCLE

    EnvCount=EnvCount+1

    IF (WriteOutputToSQLite) THEN
      CALL SQLiteBegin
      CALL CreateSQLiteEnvironmentPeriodRecord()
      CALL SQLiteCommit
    END IF

    ExitDuringSimulations=.true.
    SimsDone=.true.
    call DisplayString('Initializing New Environment Parameters')

    BeginEnvrnFlag = .TRUE.
    EndEnvrnFlag   = .FALSE.
    EndMonthFlag   = .FALSE.
    WarmupFlag     = .TRUE.
    DayOfSim       =  0
    DayOfSimChr    ='0'
    NumOfWarmupDays=  0

    CALL ManageEMS(emsCallFromBeginNewEvironment) ! calling point

    DO WHILE ((DayOfSim.LT.NumOfDayInEnvrn).OR.(WarmupFlag))  ! Begin day loop ...

      IF (WriteOutputToSQLite) CALL SQLiteBegin ! setup for one transaction per day

      DayOfSim     = DayOfSim + 1
      WRITE(DayOfSimChr,*) DayOfSim
      DayOfSimChr=ADJUSTL(DayOfSimChr)
      IF (.not. WarmUpFlag) THEN
        CurrentOverallSimDay=CurrentOverallSimDay+1
        CALL DisplaySimDaysProgress(CurrentOverallSimDay,TotalOverallSimDays)
      ELSE
        DayOfSimChr='0'
      ENDIF
      BeginDayFlag = .TRUE.
      EndDayFlag   = .FALSE.

      IF (WarmupFlag) THEN
        NumOfWarmupDays=NumOfWarmupDays+1
        cWarmupDay=TrimSigDigits(NumOfWarmupDays)
        CALL DisplayString('Warming up {'//TRIM(cWarmUpDay)//'}')
      ELSEIF (DayOfSim == 1)   THEN
        CALL DisplayString('Starting Simulation at '//TRIM(CurMnDy)//' for '//TRIM(EnvironmentName))
        WRITE(OutputFileInits,700)   NumOfWarmupDays
 700    FORMAT('Environment:WarmupDays,',I3)
      ELSEIF (DisplayPerfSimulationFlag) THEN
        CALL DisplayString('Continuing Simulation at '//TRIM(CurMnDy)//' for '//TRIM(EnvironmentName))
        DisplayPerfSimulationFlag=.false.
      END IF

      DO HourOfDay = 1, 24      ! Begin hour loop ...

        BeginHourFlag = .TRUE.
        EndHourFlag   = .FALSE.

        DO TimeStep = 1, NumOfTimeStepInHour

          BeginTimeStepFlag = .TRUE.
          CALL ExternalInterfaceExchangeVariables

          ! Set the End__Flag variables to true if necessary.  Note that
          ! each flag builds on the previous level.  EndDayFlag cannot be
          ! .true. unless EndHourFlag is also .true., etc.  Note that the
          ! EndEnvrnFlag and the EndSimFlag cannot be set during warmup.
          ! Note also that BeginTimeStepFlag, EndTimeStepFlag, and the
          ! SubTimeStepFlags can/will be set/reset in the HVAC Manager.

          IF ((TimeStep.EQ.NumOfTimeStepInHour)) THEN
            EndHourFlag = .TRUE.
            IF (HourOfDay.EQ.24) THEN
              EndDayFlag = .TRUE.
              IF ((.NOT.WarmupFlag).AND.(DayOfSim.EQ.NumOfDayInEnvrn)) THEN
                EndEnvrnFlag = .TRUE.
              END IF
            END IF
          END IF

          CALL ManageWeather

          CALL ManageExteriorEnergyUse

          CALL ManageHeatBalance

          !  After the first iteration of HeatBalance, all the 'input' has been gotten
          IF (BeginFullSimFlag) THEN
            IF (GetNumRangeCheckErrorsFound() > 0) THEN
              CALL ShowFatalError('Out of "range" values found in input')
            ENDIF
          ENDIF

          BeginHourFlag  = .FALSE.
          BeginDayFlag   = .FALSE.
          BeginEnvrnFlag = .FALSE.
          BeginSimFlag   = .FALSE.
          BeginFullSimFlag = .FALSE.


        END DO  ! TimeStep loop

        PreviousHour=HourOfDay

      END DO                    ! ... End hour loop.

      IF (WriteOutputToSQLite) CALL SQLiteCommit  ! one transaction per day

    END DO                      ! ... End day loop.

    ! Need one last call to send latest states to middleware
    CALL ExternalInterfaceExchangeVariables

  END DO                        ! ... End environment loop.

  WarmupFlag=.false.
  IF (.not. SimsDone .and. DoDesDaySim) THEN
    IF ((TotDesDays+TotRunDesPersDays) == 0) THEN   ! if sum is 0, then there was no sizing done.
      CALL ShowWarningError('ManageSimulation: SizingPeriod:* were requested in SimulationControl  '//  &
         'but no SizingPeriod:* objects in input.')
    ENDIF
  ENDIF

  IF (.not. SimsDone .and. DoWeathSim) THEN
    IF (.not. RunPeriodsInInput) THEN   ! if no run period requested, and sims not done
      CALL ShowWarningError('ManageSimulation: Weather Simulation was requested in SimulationControl '//  &
         'but no RunPeriods in input.')
    ENDIF
  ENDIF

  IF (WriteOutputToSQLite) CALL SQLiteBegin  ! for final data to write

#ifdef EP_Detailed_Timings
                             CALL epStartTime('Closeout Reporting=')
#endif
  CALL SimCostEstimate

  CALL ComputeTariff          !     Compute the utility bills

  CALL ReportForTabularReports  ! For Energy Meters (could have other things that need to be pushed to after simulation)

  CALL OpenOutputTabularFile

  CALL WriteTabularReports    !     Create the tabular reports at completion of each

  CALL WriteTabularTariffReports

  CALL ComputeLifeCycleCostAndReport !must be after WriteTabularReports and WriteTabularTariffReports

  CALL CloseOutputTabularFile

  CALL DumpAirLoopStatistics        ! Dump runtime statistics for air loop controller simulation to csv file

#ifdef EP_Detailed_Timings
                             CALL epStopTime('Closeout Reporting=')
#endif
  CALL CloseOutputFiles

  CALL CreateZoneExtendedOutput

  IF (WriteOutputToSQLite) THEN
    CALL DisplayString('Writing final SQL reports')
    CALL SQLiteCommit ! final transactions
    CALL InitializeIndexes  ! do not create indexes (SQL) until all is done.
  ENDIF

  IF (ErrorsFound) THEN
    CALL ShowFatalError('Error condition occurred.  Previous Severe Errors cause termination.')
  ENDIF

  RETURN

END SUBROUTINE ManageSimulation

SUBROUTINE GetProjectData

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   November 1997
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine gets global project data from the input file.

          ! METHODOLOGY EMPLOYED:
          ! Use GetObjectItem from the Input Processor

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor
  USE DataStringGlobals, ONLY: MatchVersion
  USE DataConvergParams
  USE DataSystemVariables
  USE DataHVACGlobals, ONLY: LimitNumSysSteps,deviationFromSetPtThresholdHtg,  &
                             deviationFromSetPtThresholdClg
  USE General, ONLY: RoundSigDigits
  USE DataEnvironment, ONLY: DisplayWeatherMissingDataWarnings,IgnoreSolarRadiation,IgnoreBeamRadiation,  &
     IgnoreDiffuseRadiation
  USE DataIPShortCuts

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
  INTEGER, PARAMETER, DIMENSION(12) :: Div60=(/1,2,3,4,5,6,10,12,15,20,30,60/)
  CHARACTER(len=*), PARAMETER :: Blank=' '
  CHARACTER(len=*), PARAMETER :: fmtA='(A)'

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  CHARACTER(len=MaxNameLength), DIMENSION(5) :: Alphas
  REAL(r64), DIMENSION(4) :: Number
  INTEGER NumAlpha, NumNumber, IOStat
  INTEGER :: NumDebugOut
  INTEGER :: MinInt
  INTEGER :: Num
  INTEGER :: Which
  LOGICAL :: ErrorsFound
  INTEGER :: Num1
  INTEGER :: NumA
  INTEGER :: NumRunControl
  CHARACTER(len=20) :: VersionID=' '
  CHARACTER(len=MaxNameLength) :: CurrentModuleObject
  LOGICAL :: CondFDAlgo
  INTEGER :: Item

  ErrorsFound=.false.

  CurrentModuleObject='Version'
  Num=GetNumObjectsFound(CurrentModuleObject)
  IF (Num == 1) THEN
    CALL GetObjectItem(CurrentModuleObject,1,Alphas,NumAlpha,Number,NumNumber,IOStat,  &
                   AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                   AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
    Num1=LEN_TRIM(MatchVersion)
    IF (MatchVersion(Num1:Num1) == '0') THEN
      Which=INDEX(Alphas(1)(1:Num1-2),MatchVersion(1:Num1-2))
    ELSE
      Which=INDEX(Alphas(1),MatchVersion)
    ENDIF
    IF (Which /= 1) THEN
      CALL ShowWarningError(TRIM(CurrentModuleObject)//': in IDF="'//TRIM(Alphas(1))//  &
         '" not the same as expected="'//TRIM(MatchVersion)//'"')
    ENDIF
    VersionID=Alphas(1)
  ELSEIF (Num == 0) THEN
    CALL ShowWarningError(TRIM(CurrentModuleObject)//': missing in IDF, processing for EnergyPlus version="'//  &
       TRIM(MatchVersion)//'"')
  ELSE
    CALL ShowSevereError('Too many '//TRIM(CurrentModuleObject)//' Objects found.')
    ErrorsFound=.true.
  ENDIF


  ! Do Mini Gets on HB Algorithm and by-surface overrides
  CurrentModuleObject='HeatBalanceAlgorithm'
  Num=GetNumObjectsFound(CurrentModuleObject)
  CondFDAlgo=.false.
  IF (Num > 0) THEN
    CALL GetObjectItem(CurrentModuleObject,1,Alphas,NumAlpha,Number,NumNumber,IOStat,  &
                   AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                   AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
    SELECT CASE (Alphas(1))
    CASE ('CONDUCTIONFINITEDIFFERENCE','CONDFD','CONDUCTIONFINITEDIFFERENCEDETAILED','CONDUCTIONFINITEDIFFERENCESIMPLIFIED')
        CondFDAlgo=.true.
    CASE DEFAULT
    END SELECT
  ENDIF
  CurrentModuleObject = 'SurfaceProperty:HeatTransferAlgorithm'
  Num=GetNumObjectsFound(CurrentModuleObject)
  IF (Num > 0) THEN
    DO Item = 1, Num
      CALL GetObjectItem(CurrentModuleObject,Item,Alphas,NumAlpha,Number,NumNumber,IOStat,  &
                  AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                  AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
      SELECT CASE (Alphas(2))
      CASE ('CONDUCTIONFINITEDIFFERENCE')
         CondFDAlgo=.true.

      CASE DEFAULT
      END SELECT
    ENDDO
  ENDIF
  CurrentModuleObject = 'SurfaceProperty:HeatTransferAlgorithm:MultipleSurface'
  Num=GetNumObjectsFound(CurrentModuleObject)
  IF (Num > 0) THEN
    DO Item = 1, Num
      CALL GetObjectItem(CurrentModuleObject,1,Alphas,NumAlpha,Number,NumNumber,IOStat,  &
                     AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                     AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
      SELECT CASE (Alphas(3))
      CASE ('CONDUCTIONFINITEDIFFERENCE')
          CondFDAlgo=.true.
      CASE DEFAULT
      END SELECT
    ENDDO
  ENDIF
  CurrentModuleObject = 'SurfaceProperty:HeatTransferAlgorithm:SurfaceList'
  Num=GetNumObjectsFound(CurrentModuleObject)
  IF (Num > 0) THEN
    DO Item = 1, Num
      CALL GetObjectItem(CurrentModuleObject,1,cAlphaArgs,NumAlpha,Number,NumNumber,IOStat,  &
                     AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                     AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
      SELECT CASE (cAlphaArgs(2))
      CASE ('CONDUCTIONFINITEDIFFERENCE')
          CondFDAlgo=.true.
      CASE DEFAULT
      END SELECT
    ENDDO
  ENDIF
  CurrentModuleObject = 'SurfaceProperty:HeatTransferAlgorithm:Construction'
  Num=GetNumObjectsFound(CurrentModuleObject)
  IF (Num > 0) THEN
    DO Item = 1, Num
      CALL GetObjectItem(CurrentModuleObject,1,cAlphaArgs,NumAlpha,Number,NumNumber,IOStat,  &
                     AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                     AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
      SELECT CASE (cAlphaArgs(2))
      CASE ('CONDUCTIONFINITEDIFFERENCE')
          CondFDAlgo=.true.
      CASE DEFAULT
      END SELECT
    ENDDO
  ENDIF

  CurrentModuleObject='Timestep'
  Num=GetNumObjectsFound(CurrentModuleObject)
  IF (Num == 1) THEN
    CALL GetObjectItem(CurrentModuleObject,1,Alphas,NumAlpha,Number,NumNumber,IOStat,  &
                   AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                   AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
    NumOfTimeStepInHour=Number(1)
    IF (NumOfTimeStepInHour <= 0 .or. NumOfTimeStepInHour > 60) THEN
      Alphas(1)=RoundSigDigits(NumOfTimeStepInHour)
      CALL ShowWarningError(TRIM(CurrentModuleObject)//': Requested number ('//TRIM(Alphas(1))//') invalid, Defaulted to 4')
      NumOfTimeStepInHour=4
    ELSEIF (MOD(60,NumOfTimeStepInHour) /= 0) THEN
      MinInt=9999
      DO Num=1,12
        IF (ABS(NumOfTimeStepInHour-Div60(Num)) > MinInt) CYCLE
        MinInt=NumOfTimeStepInHour-Div60(Num)
        Which=Num
      ENDDO
      CALL ShowWarningError(TRIM(CurrentModuleObject)//': Requested number ('//TRIM(RoundSigDigits(NumOfTimeStepInHour))//  &
         ') not evenly divisible into 60, '//'defaulted to nearest ('//TRIM(RoundSigDigits(Div60(Which)))//').')
      NumOfTimeStepInHour=Div60(Which)
    ENDIF
    IF (CondFDAlgo .and. NumOfTimeStepInHour < 20) THEN
      CALL ShowWarningError(TRIM(CurrentModuleObject)//': Requested number ('//TRIM(RoundSigDigits(NumOfTimeStepInHour))//  &
         ') cannot be used when Conduction Finite Difference algorithm is selected.')
      CALL ShowContinueError('...'//trim(CurrentModuleObject)//' is set to 20.')
      NumOfTimeStepInHour=20
    ENDIF
    IF (NumOfTimeStepInHour < 4 .and. GetNumObjectsFound('Zone') > 0) THEN
      CALL ShowWarningError(TRIM(CurrentModuleObject)//': Requested number ('//TRIM(RoundSigDigits(NumOfTimeStepInHour))//  &
         ') is less than the suggested minimum of 4.')
      CALL ShowContinueError('Please see entry for '//TRIM(CurrentModuleObject)//  &
         ' in Input/Output Reference for discussion of considerations.')
    ENDIF
  ELSEIF (Num == 0 .and. GetNumObjectsFound('Zone') > 0 .and. .not. CondFDAlgo) THEN
    CALL ShowWarningError('No '//TRIM(CurrentModuleObject)//' object found.  Number of TimeSteps in Hour defaulted to 4.')
    NumOfTimeStepInHour=4
  ELSEIF (Num == 0 .and. .not. CondFDAlgo) THEN
    NumOfTimeStepInHour=4
  ELSEIF (Num == 0 .and. GetNumObjectsFound('Zone') > 0 .and. CondFDAlgo) THEN
    CALL ShowWarningError('No '//TRIM(CurrentModuleObject)//' object found.  Number of TimeSteps in Hour defaulted to 20.')
    CALL ShowContinueError('...Due to presence of Conduction Finite Difference Algorithm selection.')
    NumOfTimeStepInHour=20
  ELSEIF (Num == 0 .and. CondFDAlgo) THEN
    NumOfTimeStepInHour=20
  ELSE
    CALL ShowSevereError('Too many '//TRIM(CurrentModuleObject)//' Objects found.')
    ErrorsFound=.true.
  ENDIF

  TimeStepZone=1.0d0/REAL(NumOfTimeStepInHour,r64)
  MinutesPerTimeStep=TimeStepZone*60

  CurrentModuleObject='ConvergenceLimits'
  Num=GetNumObjectsFound(CurrentModuleObject)
  IF (Num == 1) THEN
    CALL GetObjectItem(CurrentModuleObject,1,Alphas,NumAlpha,Number,NumNumber,IOStat,  &
                   AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                   AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
    MinInt=INT(Number(1))
    IF (MinInt > MinutesPerTimeStep) THEN
      MinInt=MinutesPerTimeStep
    ENDIF
    IF (MinInt < 0 .or. MinInt > 60) THEN
      CALL ShowWarningError(TRIM(CurrentModuleObject)//': Requested '//TRIM(cNumericFieldNames(1))//  &
         ' ('//TRIM(RoundSigDigits(MinInt))//') invalid. Set to 1 minute.')
      MinTimeStepSys=1.d0/60.d0
    ELSEIF (MinInt == 0) THEN  ! Set to TimeStepZone
      MinTimeStepSys=TimeStepZone
    ELSE
      MinTimeStepSys=REAL(MinInt,r64)/60.0d0
    ENDIF
    MaxIter=INT(Number(2))
    IF (MaxIter <= 0) THEN
      MaxIter=20
    ENDIF
    IF (.NOT. lNumericFieldBlanks(3))  MinPlantSubIterations = INT(Number(3))
    IF (.NOT. lNumericFieldBlanks(4))  MaxPlantSubIterations = INT(Number(4))
    ! trap bad values
    IF (MinPlantSubIterations < 1) MinPlantSubIterations = 1
    IF (MaxPlantSubIterations < 3) MaxPlantSubIterations = 3
    IF (MinPlantSubIterations > MaxPlantSubIterations) MaxPlantSubIterations = MinPlantSubIterations + 1

  ELSEIF (Num == 0) THEN
    MinTimeStepSys=1.d0/60.d0
    MaxIter=20
    MinPlantSubIterations = 2
    MaxPlantSubIterations = 8
  ELSE
    CALL ShowSevereError('Too many '//TRIM(CurrentModuleObject)//' Objects found.')
    ErrorsFound=.true.
  ENDIF

  LimitNumSysSteps       = INT(TimeStepZone/MinTimeStepSys)


  DebugOutput = .FALSE.
  EvenDuringWarmup = .FALSE.
  CurrentModuleObject='Output:DebuggingData'
  NumDebugOut = GetNumObjectsFound(CurrentModuleObject)
  IF (NumDebugOut > 0) THEN
    CALL GetObjectItem(CurrentModuleObject,1,Alphas,NumAlpha,Number,NumNumber,IOStat)
    IF (INT(Number(1)) == 1) THEN
      DebugOutput = .TRUE.
    END IF
    IF (INT(Number(2)) == 1) THEN
      EvenDuringWarmup = .TRUE.
    END IF
  END IF

  CurrentModuleObject='Output:Diagnostics'
  Num=GetNumObjectsFound(CurrentModuleObject)
  DO Num1=1,Num
    CALL GetObjectItem(CurrentModuleObject,Num1,Alphas,NumAlpha,Number,NumNumber,IOStat)
    DO NumA=1,NumAlpha
      IF (SameString(Alphas(NumA),'DisplayExtraWarnings')) THEN
        DisplayExtraWarnings=.true.
      ELSEIF (SameString(Alphas(NumA),'DisplayAdvancedReportVariables')) THEN
        DisplayAdvancedReportVariables=.true.
      ELSEIF (SameString(Alphas(NumA),'DisplayAllWarnings')) THEN
        DisplayAllWarnings=.true.
        DisplayExtraWarnings=.true.
        DisplayUnusedObjects=.true.
        DisplayUnusedSchedules=.true.
      ELSEIF (SameString(Alphas(NumA),'DisplayUnusedObjects')) THEN
        DisplayUnusedObjects=.true.
      ELSEIF (SameString(Alphas(NumA),'DisplayUnusedSchedules')) THEN
        DisplayUnusedSchedules=.true.
      ELSEIF (SameString(Alphas(NumA),'DisplayZoneAirHeatBalanceOffBalance')) THEN
        DisplayZoneAirHeatBalanceOffBalance=.true.
      ELSEIF (SameString(Alphas(NumA),'DoNotMirrorDetachedShading')) THEN
        MakeMirroredDetachedShading=.false.
      ELSEIF (SameString(Alphas(NumA),'DoNotMirrorAttachedShading')) THEN
        MakeMirroredAttachedShading=.false.
      ELSEIF (SameString(Alphas(NumA),'IgnoreInteriorWindowTransmission')) THEN
        IgnoreInteriorWindowTransmission=.true.
      ELSEIF (SameString(Alphas(NumA),'ReportDuringWarmup')) THEN
        ReportDuringWarmup=.true.
      ELSEIF (SameString(Alphas(NumA),'DisplayWeatherMissingDataWarnings')) THEN
        DisplayWeatherMissingDataWarnings=.true.
      ELSEIF (SameString(Alphas(NumA),'IgnoreSolarRadiation')) THEN
        IgnoreSolarRadiation=.true.
      ELSEIF (SameString(Alphas(NumA),'IgnoreBeamRadiation')) THEN
        IgnoreBeamRadiation=.true.
      ELSEIF (SameString(Alphas(NumA),'IgnoreDiffuseRadiation')) THEN
        IgnoreDiffuseRadiation=.true.
      ELSEIF (SameString(Alphas(NumA),'DeveloperFlag')) THEN
        DeveloperFlag=.true.
      ELSEIF (SameString(Alphas(NumA),'TimingFlag')) THEN
        TimingFlag=.true.
      ELSEIF (SameString(Alphas(NumA),'ReportDetailedWarmupConvergence')) THEN
        ReportDetailedWarmupConvergence=.true.
      ELSEIF (SameString(Alphas(NumA),'CreateMinimalSurfaceVariables')) THEN
        CYCLE
!        CreateMinimalSurfaceVariables=.true.
      ELSEIF (SameString(Alphas(NumA),'CreateNormalSurfaceVariables')) THEN
        CYCLE
!        IF (CreateMinimalSurfaceVariables) THEN
!          CALL ShowWarningError('GetProjectData: '//trim(CurrentModuleObject)//'=''//  &
!             TRIM(Alphas(NumA))//'', prior set=true for this condition reverts to false.')
!        ENDIF
!        CreateMinimalSurfaceVariables=.false.
      ELSEIF (Alphas(NumA) /= Blank) THEN
        CALL ShowWarningError('GetProjectData: '//trim(CurrentModuleObject)//'="'//  &
           TRIM(Alphas(NumA))//'", Invalid value for field, entered value ignored.')
      ENDIF
    ENDDO
  ENDDO

  CurrentModuleObject='OutputControl:ReportingTolerances'
  Num = GetNumObjectsFound(CurrentModuleObject)
  IF (Num > 0) THEN
    CALL GetObjectItem(CurrentModuleObject,1,Alphas,NumAlpha,Number,NumNumber,IOStat,  &
                   AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                   AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
    IF (.not. lNumericFieldBlanks(1)) THEN
      deviationFromSetPtThresholdHtg=-Number(1)
    ELSE
      deviationFromSetPtThresholdHtg=-.2d0
    ENDIF
    IF (.not. lNumericFieldBlanks(2)) THEN
      deviationFromSetPtThresholdClg=Number(2)
    ELSE
      deviationFromSetPtThresholdClg=.2d0
    ENDIF
  END IF

  DoZoneSizing = .FALSE.
  DoSystemSizing = .FALSE.
  DoPlantSizing = .FALSE.
  DoDesDaySim = .TRUE.
  DoWeathSim = .TRUE.
  CurrentModuleObject='SimulationControl'
  NumRunControl = GetNumObjectsFound(CurrentModuleObject)
  IF (NumRunControl > 0) THEN
    RunControlInInput=.true.
    CALL GetObjectItem(CurrentModuleObject,1,Alphas,NumAlpha,Number,NumNumber,IOStat,  &
                   AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                   AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
    IF (Alphas(1).EQ.'YES')  DoZoneSizing = .TRUE.
    IF (Alphas(2).EQ.'YES')  DoSystemSizing = .TRUE.
    IF (Alphas(3).EQ.'YES')  DoPlantSizing = .TRUE.
    IF (Alphas(4).EQ.'NO')  DoDesDaySim = .FALSE.
    IF (Alphas(5).EQ.'NO')  DoWeathSim = .FALSE.
  END IF
  IF (DDOnly) THEN
    DoDesDaySim=.true.
    DoWeathSim=.false.
  ENDIF
  IF (FullAnnualRun) THEN
    DoDesDaySim=.false.
    DoWeathSim=.true.
  ENDIF

  IF (ErrorsFound) THEN
    CALL ShowFatalError('Errors found getting Project Input')
  ENDIF

  Write(OutputFileInits,fmtA) '! <Version>, Version ID'
  Write(OutputFileInits,721) TRIM(VersionID)

721 Format(' Version, ',A)

  Write(OutputFileInits,fmtA) '! <Timesteps per Hour>, #TimeSteps, Minutes per TimeStep {minutes}'
  Write(OutputFileInits,731) NumOfTimeStepInHour,INT(MinutesPerTimeStep)
731 Format(' Timesteps per Hour, ',I2,', ',I2)

  Write(OutputFileInits,fmtA) '! <System Convergence Limits>, Minimum System TimeStep {minutes}, Max HVAC Iterations, '//  &
      ' Minimum Plant Iterations, Maximum Plant Iterations'
  MinInt=MinTimeStepSys*60.d0
  Write(OutputFileInits,733) trim(RoundSigDigits(MinInt)),trim(RoundSigDigits(MaxIter)),  &
     trim(RoundSigDigits(MinPlantSubIterations)),trim(RoundSigDigits(MaxPlantSubIterations))
733 Format(' System Convergence Limits',4(', ',A))

  IF (DoZoneSizing) THEN
    Alphas(1)='Yes'
  ELSE
    Alphas(1)='No'
  ENDIF
  IF (DoSystemSizing) THEN
    Alphas(2)='Yes'
  ELSE
    Alphas(2)='No'
  ENDIF
  IF (DoPlantSizing) THEN
    Alphas(3)='Yes'
  ELSE
    Alphas(3)='No'
  ENDIF
  IF (DoDesDaySim) THEN
    Alphas(4)='Yes'
  ELSE
    Alphas(4)='No'
  ENDIF
  IF (DoWeathSim) THEN
    Alphas(5)='Yes'
  ELSE
    Alphas(5)='No'
  ENDIF


  Write(OutputFileInits,fmtA) '! <Simulation Control>, Do Zone Sizing, Do System Sizing, '//  &
     'Do Plant Sizing, Do Design Days, Do Weather Simulation'
  Write(OutputFileInits,741) (TRIM(Alphas(Num)),Num=1,5)
741 Format(' Simulation Control',5(', ',A))

  Write(OutputFileInits,fmtA) '! <Output Reporting Tolerances>, Tolerance for Time Heating Setpoint Not Met, '//  &
   'Tolerance for Zone Cooling Setpoint Not Met Time'
  Write(OutputFileInits,751) trim(RoundSigDigits(abs(deviationFromSetPtThresholdHtg),3)),  &
                             trim(RoundSigDigits(deviationFromSetPtThresholdClg,3))
751 Format(' Output Reporting Tolerances',5(', ',A))

!  IF (DisplayExtraWarnings) THEN
!    Write(OutputFileInits,740)
!    Write(OutputFileInits,741) (TRIM(Alphas(Num)),Num=1,5)
!742 Format('! <Display Extra Warnings>, Display Advanced Report Variables, Do Not Mirror Detached Shading')
!    IF (DisplayAdvancedReportVariables) THEN
!      NumOut1='Yes'
!    ELSE
!      NumOut2='No'
!    ENDIF
!    IF (.not. MakeMirroredDetachedShading) THEN
!      NumOut1='Yes'
!    ELSE
!      NumOut2='No'
!    ENDIF
!unused0909743 Format(' Display Extra Warnings',2(', ',A))
!  ENDIF

  RETURN

END SUBROUTINE GetProjectData

SUBROUTINE CheckForMisMatchedEnvironmentSpecifications

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   August 2008
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! In response to CR 7518, this routine will check to see if a proper combination of SimulationControl, RunPeriod,
          ! SizingPeriod:*, etc are entered to proceed with a simulation.

          ! METHODOLOGY EMPLOYED:
          ! For now (8/2008), the routine will query several objects in the input.  And try to produce warnings or
          ! fatals as a result.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: GetNumObjectsFound

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
  INTEGER :: NumZoneSizing
  INTEGER :: NumSystemSizing
  INTEGER :: NumPlantSizing
  INTEGER :: NumDesignDays
  INTEGER :: NumRunPeriodDesign
  INTEGER :: NumSizingDays
  LOGICAL :: WeatherFileAttached
  LOGICAL :: ErrorsFound

  ErrorsFound=.false.
  NumZoneSizing=GetNumObjectsFound('Sizing:Zone')
  NumSystemSizing=GetNumObjectsFound('Sizing:System')
  NumPlantSizing=GetNumObjectsFound('Sizing:Plant')
  NumDesignDays=GetNumObjectsFound('SizingPeriod:DesignDay')
  NumRunPeriodDesign=GetNumObjectsFound('SizingPeriod:WeatherFileDays')+GetNumObjectsFound('SizingPeriod:WeatherFileConditionType')
  NumSizingDays=NumDesignDays+NumRunPeriodDesign
  INQUIRE(FILE='in.epw',EXIST=WeatherFileAttached)

  IF (RunControlInInput) THEN
    IF (DoZoneSizing) THEN
      IF (NumZoneSizing > 0 .and. NumSizingDays == 0) THEN
        ErrorsFound=.true.
        CALL ShowSevereError('CheckEnvironmentSpecifications: Sizing for Zones has been requested but there are no '//  &
           'design environments specified.')
        CALL ShowContinueError('...Add appropriate SizingPeriod:* objects for your simulation.')
      ENDIF
      IF (NumZoneSizing > 0 .and. NumRunPeriodDesign > 0 .and. .not. WeatherFileAttached) THEN
        ErrorsFound=.true.
        CALL ShowSevereError('CheckEnvironmentSpecifications: Sizing for Zones has been requested; Design period from '//  &
           'the weather file requested; but no weather file specified.')
      ENDIF
    ENDIF
    IF (DoSystemSizing) THEN
      IF (NumSystemSizing > 0 .and. NumSizingDays == 0) THEN
        ErrorsFound=.true.
        CALL ShowSevereError('CheckEnvironmentSpecifications: Sizing for Systems has been requested but there are no '//  &
           'design environments specified.')
        CALL ShowContinueError('...Add appropriate SizingPeriod:* objects for your simulation.')
      ENDIF
      IF (NumSystemSizing > 0 .and. NumRunPeriodDesign > 0 .and. .not. WeatherFileAttached) THEN
        ErrorsFound=.true.
        CALL ShowSevereError('CheckEnvironmentSpecifications: Sizing for Systems has been requested; Design period from '//  &
           'the weather file requested; but no weather file specified.')
      ENDIF
    ENDIF
    IF (DoPlantSizing) THEN
      IF (NumPlantSizing > 0 .and. NumSizingDays == 0) THEN
        ErrorsFound=.true.
        CALL ShowSevereError('CheckEnvironmentSpecifications: Sizing for Equipment/Plants has been requested but there are no '//  &
           'design environments specified.')
        CALL ShowContinueError('...Add appropriate SizingPeriod:* objects for your simulation.')
      ENDIF
      IF (NumPlantSizing > 0 .and. NumRunPeriodDesign > 0 .and. .not. WeatherFileAttached) THEN
        ErrorsFound=.true.
        CALL ShowSevereError('CheckEnvironmentSpecifications: Sizing for Equipment/Plants has been requested; '//   &
            'Design period from the weather file requested; but no weather file specified.')
      ENDIF
    ENDIF
    IF (DoDesDaySim .and. NumSizingDays == 0) THEN
      CALL ShowWarningError('CheckEnvironmentSpecifications: SimulationControl specified doing design day simulations, but '//  &
       'no design environments specified.')
      CALL ShowContinueError('...No design environment results produced. For these results, '//  &
         'add appropriate SizingPeriod:* objects for your simulation.')
    ENDIF
    IF (DoDesDaySim .and. NumRunPeriodDesign > 0 .and. .not. WeatherFileAttached) THEN
      ErrorsFound=.true.
      CALL ShowSevereError('CheckEnvironmentSpecifications: SimulationControl specified doing design day simulations; weather '//  &
       'file design environments specified; but no weather file specified.')
    ENDIF
    IF (DoWeathSim .and. .not. RunPeriodsInInput) THEN
      CALL ShowWarningError('CheckEnvironmentSpecifications: SimulationControl specified doing weather simulations, but '//  &
       'no run periods for weather file specified.  No annual results produced.')
    ENDIF
    IF (DoWeathSim .and. RunPeriodsInInput .and. .not. WeatherFileAttached) THEN
      CALL ShowWarningError('CheckEnvironmentSpecifications: SimulationControl specified doing weather simulations; '//  &
       'run periods for weather file specified; but no weather file specified.')
    ENDIF
  ENDIF
  IF (.not. DoDesDaySim .and. .not. DoWeathSim) THEN
    CALL ShowWarningError('"Do the design day simulations" and "Do the weather file simulation"'// &
           ' are both set to "No".  No simulations will be performed, and most input will not be read.')
  ENDIF
  IF (.not. DoZoneSizing .and. .not. DoSystemSizing .and. .not. DoPlantSizing .and.  &
      .not. DoDesDaySim .and. .not. DoWeathSim) THEN
    CALL ShowSevereError('All elements of SimulationControl are set to "No". No simulations can be done.  Program terminates.')
    ErrorsFound=.true.
  ENDIF

  IF (ErrorsFound) THEN
    CALL ShowFatalError('Program terminates due to preceding conditions.')
  ENDIF

  RETURN

END SUBROUTINE CheckForMisMatchedEnvironmentSpecifications

SUBROUTINE CheckForRequestedReporting

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   January 2009
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! EnergyPlus does not automatically produce any results files.  Because of this, users may not request
          ! reports and may get confused when nothing is produced.  This routine will provide a warning when
          ! results should be produced (either sizing periods or weather files are run) but no reports are
          ! requested.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: GetNumObjectsFound

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
  LOGICAL :: SimPeriods
  LOGICAL :: ReportingRequested

  ReportingRequested=.false.
  SimPeriods = (GetNumObjectsFound('SizingPeriod:DesignDay') > 0 .or.   &
                GetNumObjectsFound('SizingPeriod:WeatherFileDays') > 0 .or. &
                GetNumObjectsFound('SizingPeriod:WeatherFileConditionType') > 0 .or.  &
                GetNumObjectsFound('RunPeriod') > 0)

  IF ((DoDesDaySim .or. DoWeathSim) .and. SimPeriods) THEN
    ReportingRequested=(GetNumObjectsFound('Output:Table:SummaryReports') > 0 .or.   &
                GetNumObjectsFound('Output:Table:TimeBins') > 0 .or. &
                GetNumObjectsFound('Output:Table:Monthly') > 0 .or.  &
                GetNumObjectsFound('Output:Variable') > 0 .or.  &
                GetNumObjectsFound('Output:Meter') > 0 .or.  &
                GetNumObjectsFound('Output:Meter:MeterFileOnly') > 0 .or.  &
                GetNumObjectsFound('Output:Meter:Cumulative') > 0 .or.  &
                GetNumObjectsFound('Output:Meter:Cumulative:MeterFileOnly') > 0)
    ! Not testing for : Output:SQLite or Output:EnvironmentalImpactFactors
    IF (.not. ReportingRequested) THEN
      CALL ShowWarningError('No reporting elements have been requested. No simulation results produced.')
      CALL ShowContinueError('...Review requirements such as "Output:Table:SummaryReports", "Output:Table:Monthly", '//  &
        '"Output:Variable", "Output:Meter" and others.')
    ENDIF
  ENDIF

  RETURN

END SUBROUTINE CheckForRequestedReporting

SUBROUTINE OpenOutputFiles

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Rick Strand
          !       DATE WRITTEN   June 1997
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine opens all of the input and output files needed for
          ! an EnergyPlus run.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataStringGlobals, ONLY: VerString

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
  integer,external :: GetNewUnitNumber  ! External function for a new unit number
  integer :: write_stat

          ! FLOW:
  OutputFileStandard=GetNewUnitNumber()
  StdOutputRecordCount=0
  OPEN (UNIT=OutputFileStandard,FILE='eplusout.eso',STATUS='UNKNOWN',Action='write',iostat=write_stat)
  IF (write_stat /= 0) THEN
    CALL ShowFatalError('OpenOutputFiles: Could not open file "eplusout.eso" for output (write).')
  ENDIF
  WRITE(OutputFileStandard,'(A)') 'Program Version,'//TRIM(VerString)

  ! Open the Initialization Output File
  OutputFileInits=GetNewUnitNumber()
  OPEN (UNIT=OutputFileInits,FILE='eplusout.eio',STATUS='UNKNOWN',Action='write',iostat=write_stat)
  IF (write_stat /= 0) THEN
    CALL ShowFatalError('OpenOutputFiles: Could not open file "eplusout.eio" for output (write).')
  ENDIF
  WRITE(OutputFileInits,'(A)') 'Program Version,'//TRIM(VerString)

  ! Open the Meters Output File
  OutputFileMeters=GetNewUnitNumber()
  StdMeterRecordCount=0
  OPEN (UNIT=OutputFileMeters,FILE='eplusout.mtr',STATUS='UNKNOWN',Action='write',iostat=write_stat)
  IF (write_stat /= 0) THEN
    CALL ShowFatalError('OpenOutputFiles: Could not open file "eplusout.mtr" for output (write).')
  ENDIF
  WRITE(OutputFileMeters,'(A)') 'Program Version,'//TRIM(VerString)

  ! Open the Branch-Node Details Output File
  OutputFileBNDetails=GetNewUnitNumber()
  OPEN (UNIT=OutputFileBNDetails,FILE='eplusout.bnd',STATUS='UNKNOWN',Action='write',iostat=write_stat)
  IF (write_stat /= 0) THEN
    CALL ShowFatalError('OpenOutputFiles: Could not open file "eplusout.bnd" for output (write).')
  ENDIF
  WRITE(OutputFileBNDetails,'(A)') 'Program Version,'//TRIM(VerString)

  RETURN

END SUBROUTINE OpenOutputFiles

SUBROUTINE CloseOutputFiles

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Rick Strand
          !       DATE WRITTEN   June 1997
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine closes all of the input and output files needed for
          ! an EnergyPlus run.  It also prints the end of data marker for each
          ! output file.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataOutputs
  USE OutputProcessor, ONLY: MaxRVariable,NumOfRVariable_Setup,NumOfRVariable_Sum,NumOfRVariable_Meter,  &
                             MaxIVariable,NumOfIVariable_Setup,NumOfIVariable_Sum,                       &
                             NumOfRVariable,NumOfIVariable,                                              &
                             NumEnergyMeters,NumVarMeterArrays,  &
                             NumTotalRVariable,NumTotalIVariable, NumReportList, InstMeterCacheSize
  USE OutputReportTabular, ONLY: maxUniqueKeyCount,MonthlyFieldSetInputCount
  USE SolarShading, ONLY: maxNumberOfFigures, MAXHCArrayBounds
  USE DataRunTimeLanguage
  USE DataBranchNodeConnections, ONLY: NumOfNodeConnections, MaxNumOfNodeConnections
  USE DataHeatBalance, ONLY: CondFDRelaxFactor, HeatTransferAlgosUsed, UseCondFD, CondFDRelaxFactorInput
  USE General, ONLY: RoundSigDigits
  USE DataSystemVariables !, ONLY: MaxNumberOfThreads,NumberIntRadThreads,iEnvSetThreads
  USE DataSurfaces, ONLY: MaxVerticesPerSurface
  USE DataTimings

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER :: EndOfDataFormat = '("End of Data")'  ! Signifies the end of the data block in the output file
  CHARACTER(len=*), PARAMETER :: ThreadingHeader = '! <Program Control Information:Threads/Parallel Sims>, '//  &
          'Threading Supported,Maximum Number of Threads, Env Set Threads (OMP_NUM_THREADS), '//  &
          'EP Env Set Threads (EP_OMP_NUM_THREADS), IDF Set Threads, Number of Threads Used (Interior Radiant Exchange), '//  &
          'Number Nominal Surfaces, Number Parallel Sims'
  CHARACTER(len=*), PARAMETER :: fmtA='(A)'


          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER EchoInputFile  ! found unit number for 'eplusout.audit'
  INTEGER, EXTERNAL :: FindUnitNumber
  CHARACTER(len=10) :: cEnvSetThreads
  CHARACTER(len=10) :: cepEnvSetThreads
  CHARACTER(len=10) :: cIDFSetThreads

  EchoInputFile=FindUnitNumber('eplusout.audit')
  ! Record some items on the audit file
  WRITE (EchoInputFile,*) 'NumOfRVariable=',NumOfRVariable_Setup
  WRITE (EchoInputFile,*) 'NumOfRVariable(Total)=',NumTotalRVariable
  WRITE (EchoInputFile,*) 'NumOfRVariable(Actual)=',NumOfRVariable
  WRITE (EchoInputFile,*) 'NumOfRVariable(Summed)=',NumOfRVariable_Sum
  WRITE (EchoInputFile,*) 'NumOfRVariable(Meter)=',NumOfRVariable_Meter
  WRITE (EchoInputFile,*) 'NumOfIVariable=',NumOfIVariable_Setup
  WRITE (EchoInputFile,*) 'NumOfIVariable(Total)=',NumTotalIVariable
  WRITE (EchoInputFile,*) 'NumOfIVariable(Actual)=',NumOfIVariable
  WRITE (EchoInputFile,*) 'NumOfIVariable(Summed)=',NumOfIVariable_Sum
  WRITE (EchoInputFile,*) 'MaxRVariable=',MaxRVariable
  WRITE (EchoInputFile,*) 'MaxIVariable=',MaxIVariable
  WRITE (EchoInputFile,*) 'NumEnergyMeters=',NumEnergyMeters
  WRITE (EchoInputFile,*) 'NumVarMeterArrays=',NumVarMeterArrays
  WRITE (EchoInputFile,*) 'maxUniqueKeyCount=',maxUniqueKeyCount
  WRITE (EchoInputFile,*) 'maxNumberOfFigures=',maxNumberOfFigures
  WRITE (EchoInputFile,*) 'MAXHCArrayBounds=',MAXHCArrayBounds
  WRITE (EchoInputFile,*) 'MaxVerticesPerSurface=',MaxVerticesPerSurface
  WRITE (EchoInputFile,*) 'NumReportList=',NumReportList
  WRITE (EchoInputFile,*) 'InstMeterCacheSize=',InstMeterCacheSize
  IF (SutherlandHodgman) THEN
    WRITE (EchoInputFile,*) 'ClippingAlgorithm=SutherlandHodgman'
  ELSE
    WRITE (EchoInputFile,*) 'ClippingAlgorithm=ConvexWeilerAtherton'
  ENDIF
  WRITE (EchoInputFile,*) 'MonthlyFieldSetInputCount=',MonthlyFieldSetInputCount
  WRITE (EchoInputFile,*) 'NumConsideredOutputVariables=',NumConsideredOutputVariables
  WRITE (EchoInputFile,*) 'MaxConsideredOutputVariables=',MaxConsideredOutputVariables

  WRITE (EchoInputFile,*) 'numActuatorsUsed=',numActuatorsUsed
  WRITE (EchoInputFile,*) 'numEMSActuatorsAvailable=',numEMSActuatorsAvailable
  WRITE (EchoInputFile,*) 'maxEMSActuatorsAvailable=',maxEMSActuatorsAvailable
  WRITE (EchoInputFile,*) 'numInternalVariablesUsed=',numInternalVariablesUsed
  WRITE (EchoInputFile,*) 'numEMSInternalVarsAvailable=',numEMSInternalVarsAvailable
  WRITE (EchoInputFile,*) 'maxEMSInternalVarsAvailable=',maxEMSInternalVarsAvailable

  WRITE (EchoInputFile,*) 'NumOfNodeConnections=',NumOfNodeConnections
  WRITE (EchoInputFile,*) 'MaxNumOfNodeConnections=',MaxNumOfNodeConnections
#ifdef EP_Count_Calls
  WRITE (EchoInputFile,*) 'NumShadow_Calls=',NumShadow_Calls
  WRITE (EchoInputFile,*) 'NumShadowAtTS_Calls=',NumShadowAtTS_Calls
  WRITE (EchoInputFile,*) 'NumClipPoly_Calls=',NumClipPoly_Calls
  WRITE (EchoInputFile,*) 'NumInitSolar_Calls=',NumInitSolar_Calls
  WRITE (EchoInputFile,*) 'NumAnisoSky_Calls=',NumAnisoSky_Calls
  WRITE (EchoInputFile,*) 'NumDetPolyOverlap_Calls=',NumDetPolyOverlap_Calls
  WRITE (EchoInputFile,*) 'NumCalcPerSolBeam_Calls=',NumCalcPerSolBeam_Calls
  WRITE (EchoInputFile,*) 'NumDetShadowCombs_Calls=',NumDetShadowCombs_Calls
  WRITE (EchoInputFile,*) 'NumIntSolarDist_Calls=',NumIntSolarDist_Calls
  WRITE (EchoInputFile,*) 'NumIntRadExchange_Calls=',NumIntRadExchange_Calls
  WRITE (EchoInputFile,*) 'NumIntRadExchangeZ_Calls=',NumIntRadExchangeZ_Calls
  WRITE (EchoInputFile,*) 'NumIntRadExchangeMain_Calls=',NumIntRadExchangeMain_Calls
  WRITE (EchoInputFile,*) 'NumIntRadExchangeOSurf_Calls=',NumIntRadExchangeOSurf_Calls
  WRITE (EchoInputFile,*) 'NumIntRadExchangeISurf_Calls=',NumIntRadExchangeISurf_Calls
  WRITE (EchoInputFile,*) 'NumMaxInsideSurfIterations=',NumMaxInsideSurfIterations
  WRITE (EchoInputFile,*) 'NumCalcScriptF_Calls=',NumCalcScriptF_Calls
#endif

  WRITE (OutputFileStandard,EndOfDataFormat)
  WRITE (OutputFileStandard,*) 'Number of Records Written=',StdOutputRecordCount
  IF (StdOutputRecordCount > 0) THEN
    CLOSE (OutputFileStandard)
  ELSE
    CLOSE (OutputFileStandard,STATUS='DELETE')
  ENDIF

  IF (ANY(HeatTransferAlgosUsed == UseCondFD)) THEN ! echo out relaxation factor, it may have been changed by the program
     Write(OutputFileInits,'(A)') '! <ConductionFiniteDifference Numerical Parameters>, '//  &
        'Starting Relaxation Factor, Final Relaxation Factor'
     Write(OutputFileInits,'(A)') 'ConductionFiniteDifference Numerical Parameters, '// &
                                      TRIM(RoundSigDigits(CondFDRelaxFactorInput,3)) // ', '// &
                                      TRIM(RoundSigDigits(CondFDRelaxFactor,3))
  ENDIF
  ! Report number of threads to eio file
  IF (Threading) THEN
    IF (iEnvSetThreads == 0) THEN
      cEnvSetThreads='Not Set'
    ELSE
      cEnvSetThreads=RoundSigDigits(iEnvSetThreads)
    ENDIF
    IF (iepEnvSetThreads == 0) THEN
      cepEnvSetThreads='Not Set'
    ELSE
      cepEnvSetThreads=RoundSigDigits(iepEnvSetThreads)
    ENDIF
    IF (iIDFSetThreads == 0) THEN
      cIDFSetThreads='Not Set'
    ELSE
      cIDFSetThreads=RoundSigDigits(iIDFSetThreads)
    ENDIF
    IF (lnumActiveSims) THEN
      Write(OutputFileInits,fmtA) ThreadingHeader
      Write(OutputFileInits,'(A)') 'Program Control:Threads/Parallel Sims, Yes,'// &
                                        TRIM(RoundSigDigits(MaxNumberOfThreads)) // ', '// &
                                        TRIM(cEnvSetThreads) // ', '// &
                                        TRIM(cepEnvSetThreads) // ', '// &
                                        TRIM(cIDFSetThreads) // ', '// &
                                        TRIM(RoundSigDigits(NumberIntRadThreads)) // ', '// &
                                        TRIM(RoundSigDigits(iNominalTotSurfaces)) // ', '// &
                                        TRIM(RoundSigDigits(inumActiveSims))
    ELSE
      Write(OutputFileInits,fmtA) ThreadingHeader
      Write(OutputFileInits,'(A)') 'Program Control:Threads/Parallel Sims, Yes,'// &
                                        TRIM(RoundSigDigits(MaxNumberOfThreads)) // ', '// &
                                        TRIM(cEnvSetThreads) // ', '// &
                                        TRIM(cepEnvSetThreads) // ', '// &
                                        TRIM(cIDFSetThreads) // ', '// &
                                        TRIM(RoundSigDigits(NumberIntRadThreads)) // ', '// &
                                        TRIM(RoundSigDigits(iNominalTotSurfaces)) // ', '// &
                                        'N/A'
    ENDIF
  ELSE ! no threading
    IF (lnumActiveSims) THEN
      Write(OutputFileInits,fmtA) ThreadingHeader
      Write(OutputFileInits,'(A)') 'Program Control:Threads/Parallel Sims, No,'// &
                                        TRIM(RoundSigDigits(MaxNumberOfThreads)) // ', '// &
                                        'N/A, N/A, N/A, N/A, N/A, '// &
                                        TRIM(RoundSigDigits(inumActiveSims))
    ELSE
      Write(OutputFileInits,fmtA) ThreadingHeader
      Write(OutputFileInits,'(A)') 'Program Control:Threads/Parallel Sims, No,'// &
                                        TRIM(RoundSigDigits(MaxNumberOfThreads)) // ', '// &
                                        'N/A, N/A, N/A, N/A, N/A, N/A'
    ENDIF
  ENDIF

  ! Close the Initialization Output File
  WRITE (OutputFileInits,EndOfDataFormat)
  CLOSE (OutputFileInits)


  ! Close the Meters Output File
  WRITE (OutputFileMeters,EndOfDataFormat)
  WRITE (OutputFileMeters,*) 'Number of Records Written=',StdMeterRecordCount
  IF (StdMeterRecordCount > 0) THEN
    CLOSE (OutputFileMeters)
  ELSE
    CLOSE (OutputFileMeters,STATUS='DELETE')
  ENDIF

  RETURN

END SUBROUTINE CloseOutputFiles

SUBROUTINE SetupSimulation(ErrorsFound)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B. Griffith/L. Lawrie
          !       DATE WRITTEN   May 2008
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !  execute a few time steps of a simulation to facilitate setting up model
          !  developed to resolve reverse DD problems caused be the differences
          !  that stem from setup and information gathering that occurs during the first pass.

          ! METHODOLOGY EMPLOYED:
          ! Using global flag (kickoff simulation), only a few time steps are executed.
          ! global flag is used in other parts of simulation to terminate quickly.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE ExteriorEnergyUse,   ONLY: ManageExteriorEnergyUse
  USE DataEnvironment ,    ONLY: EndMonthFlag, EnvironmentName
  USE InputProcessor,      ONLY: GetNumRangeCheckErrorsFound
  USE CostEstimateManager, ONLY: SimCostEstimate
  USE General, ONLY: TrimSigDigits
  USE DataTimings

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  LOGICAL, INTENT(INOUT) :: ErrorsFound

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  LOGICAL :: Available=.false. ! an environment is available to process
!  integer :: env_iteration=0
!  CHARACTER(len=32) :: cEnvChar

!  return  ! remove comment to do 'old way'

  Available = .true.

  DO WHILE (Available)  ! do for each environment

    CALL GetNextEnvironment(Available,ErrorsFound)

    IF (.not. Available) EXIT
    IF (ErrorsFound) EXIT

    BeginEnvrnFlag = .TRUE.
    EndEnvrnFlag   = .FALSE.
    EndMonthFlag   = .FALSE.
    WarmupFlag     = .TRUE.
    DayOfSim       =  0


      DayOfSim     = DayOfSim + 1
      BeginDayFlag = .TRUE.
      EndDayFlag   = .FALSE.

      HourOfDay = 1

        BeginHourFlag = .TRUE.
        EndHourFlag   = .FALSE.

        TimeStep = 1

        IF (DeveloperFlag) CALL DisplayString('Initializing Simulation - timestep 1:'//trim(EnvironmentName))

          BeginTimeStepFlag = .TRUE.

          CALL ManageWeather

          CALL ManageExteriorEnergyUse

          CALL ManageHeatBalance

          !  After the first iteration of HeatBalance, all the 'input' has been gotten
          IF (BeginFullSimFlag) THEN
            IF (GetNumRangeCheckErrorsFound() > 0) THEN
              CALL ShowFatalError('Out of "range" values found in input')
            ENDIF
          ENDIF

          BeginHourFlag  = .FALSE.
          BeginDayFlag   = .FALSE.
          BeginEnvrnFlag = .FALSE.
          BeginSimFlag   = .FALSE.
          BeginFullSimFlag = .FALSE.

!          ! do another timestep=1
          IF (DeveloperFlag) CALL DisplayString('Initializing Simulation - 2nd timestep 1:'//trim(EnvironmentName))

          CALL ManageWeather

          CALL ManageExteriorEnergyUse

          CALL ManageHeatBalance

!         do an end of day, end of environment time step

          HourOfDay=24
          TimeStep=NumOfTimeStepInHour
          EndEnvrnFlag   = .True.

          IF (DeveloperFlag) CALL DisplayString('Initializing Simulation - hour 24 timestep 1:'//trim(EnvironmentName))
          CALL ManageWeather

          CALL ManageExteriorEnergyUse

          CALL ManageHeatBalance

  END DO                        ! ... End environment loop.

  IF (.not. ErrorsFound) CALL SimCostEstimate  ! basically will get and check input
  IF (ErrorsFound) CALL ShowFatalError('Previous Conditions cause program termination.')

  RETURN

END SUBROUTINE SetupSimulation

SUBROUTINE ReportNodeConnections

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   February 2004
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine 'reports' the NodeConnection data structure.  It groups the
          ! report/dump by parent, non-parent objects.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: SameString, MakeUPPERCase
  USE DataBranchNodeConnections
  USE DataGlobals, ONLY: OutputFileBNDetails
  USE DataLoopNode, ONLY: NumOfNodes, NodeID

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
  INTEGER Loop1
  INTEGER NumParents
  INTEGER NumNonParents
  INTEGER NumNonConnected
  CHARACTER(len=20) ChrOut
  LOGICAL, ALLOCATABLE, DIMENSION(:) :: NonConnectedNodes
  LOGICAL ParentComponentFound

  ALLOCATE(NonConnectedNodes(NumOfNodes))
  NonConnectedNodes=.true.

  NumParents=0
  NumNonParents=0
  DO Loop=1,NumOfNodeConnections
    IF (NodeConnections(Loop)%ObjectIsParent) CYCLE
    NumNonParents=NumNonParents+1
  ENDDO
  NumParents=NumOfNodeConnections-NumNonParents
  ALLOCATE(ParentNodeList(NumParents))

  !  Do Parent Objects
  WRITE(OutputFileBNDetails,701) '! ==============================================================='
  WRITE(OutputFileBNDetails,702) 'Parent','Parent'
  WRITE(ChrOut,*) NumParents
  WRITE(OutputFileBNDetails,701) ' #Parent Node Connections,'//TRIM(ADJUSTL(ChrOut))
  WRITE(OutputFileBNDetails,703) 'Parent'

  DO Loop=1,NumOfNodeConnections
    IF (.not. NodeConnections(Loop)%ObjectIsParent) CYCLE
    NonConnectedNodes(NodeConnections(Loop)%NodeNumber)=.false.
    WRITE(ChrOut,*) NodeConnections(Loop)%FluidStream
    ChrOut=ADJUSTL(ChrOut)
    WRITE(OutputFileBNDetails,701) ' Parent Node Connection,'//TRIM(NodeConnections(Loop)%NodeName)//','// &
                      TRIM(NodeConnections(Loop)%ObjectType)//','//TRIM(NodeConnections(Loop)%ObjectName)//','//  &
                      TRIM(NodeConnections(Loop)%ConnectionType)//','//TRIM(ChrOut)
    ! Build ParentNodeLists
    IF (SameString(NodeConnections(Loop)%ConnectionType,'Inlet') .or.   &
        SameString(NodeConnections(Loop)%ConnectionType,'Outlet')) THEN
      ParentComponentFound=.false.
      DO Loop1=1,NumOfActualParents
        IF (ParentNodeList(Loop1)%CType /= NodeConnections(Loop)%ObjectType .or.   &
            ParentNodeList(Loop1)%CName /= NodeConnections(Loop)%ObjectName) CYCLE
        ParentComponentFound=.true.
        SELECT CASE (MakeUPPERCase(NodeConnections(Loop)%ConnectionType))
          CASE('INLET')
            ParentNodeList(Loop1)%InletNodeName=NodeConnections(Loop)%NodeName
          CASE('OUTLET')
            ParentNodeList(Loop1)%OutletNodeName=NodeConnections(Loop)%NodeName
        END SELECT
      ENDDO
      IF (.not. ParentComponentFound) THEN
        NumOfActualParents=NumOfActualParents+1
        ParentNodeList(NumOfActualParents)%CType=NodeConnections(Loop)%ObjectType
        ParentNodeList(NumOfActualParents)%CName=NodeConnections(Loop)%ObjectName
        SELECT CASE (MakeUPPERCase(NodeConnections(Loop)%ConnectionType))
          CASE('INLET')
            ParentNodeList(NumOfActualParents)%InletNodeName=NodeConnections(Loop)%NodeName
          CASE('OUTLET')
            ParentNodeList(NumOfActualParents)%OutletNodeName=NodeConnections(Loop)%NodeName
        END SELECT
      ENDIF
    ENDIF
  ENDDO

  !  Do non-Parent Objects
  WRITE(OutputFileBNDetails,701) '! ==============================================================='
  WRITE(OutputFileBNDetails,702) 'Non-Parent','Non-Parent'
  WRITE(ChrOut,*) NumNonParents
  WRITE(OutputFileBNDetails,701) ' #Non-Parent Node Connections,'//TRIM(ADJUSTL(ChrOut))
  WRITE(OutputFileBNDetails,703) 'Non-Parent'

  DO Loop=1,NumOfNodeConnections
    IF (NodeConnections(Loop)%ObjectIsParent) CYCLE
    NonConnectedNodes(NodeConnections(Loop)%NodeNumber)=.false.
    WRITE(ChrOut,*) NodeConnections(Loop)%FluidStream
    ChrOut=ADJUSTL(ChrOut)
    WRITE(OutputFileBNDetails,701) ' Non-Parent Node Connection,'//TRIM(NodeConnections(Loop)%NodeName)//','// &
                      TRIM(NodeConnections(Loop)%ObjectType)//','//TRIM(NodeConnections(Loop)%ObjectName)//','//  &
                      TRIM(NodeConnections(Loop)%ConnectionType)//','//TRIM(ChrOut)
  ENDDO

  NumNonConnected=0
  DO Loop=1,NumOfNodes
    IF (NonConnectedNodes(Loop)) NumNonConnected=NumNonConnected+1
  ENDDO

  IF (NumNonConnected > 0) THEN
    WRITE(OutputFileBNDetails,701) '! ==============================================================='
    WRITE(ChrOut,*) NumNonConnected
    WRITE(OutputFileBNDetails,705) TRIM(ADJUSTL(ChrOut))
    WRITE(OutputFileBNDetails,706)
    DO Loop=1,NumOfNodes
      IF (.not. NonConnectedNodes(Loop)) CYCLE
      WRITE(ChrOut,*) Loop
      ChrOut=ADJUSTL(ChrOut)
      WRITE(OutputFileBNDetails,701) ' NonConnected Node,'//TRIM(ChrOut)//','//TRIM(NodeID(Loop))
    ENDDO
  ENDIF

  DEALLOCATE(NonConnectedNodes)

 701 FORMAT(A)
 702 FORMAT('! <#',A,' Node Connections>,<Number of ',A,' Node Connections>')
 703 FORMAT('! <',A,' Node Connection>,<Node Name>,<Node ObjectType>,<Node ObjectName>,',  &
            '<Node ConnectionType>,<Node FluidStream>')

 705 FORMAT('! <#NonConnected Nodes>,<Number of NonConnected Nodes>',/,' #NonConnected Nodes,',A)
 706 FORMAT('! <NonConnected Node>,<NonConnected Node Number>,<NonConnected Node Name>')

  RETURN

END SUBROUTINE ReportNodeConnections

SUBROUTINE ReportLoopConnections

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   December 2001
          !       MODIFIED       March 2003; added other reporting
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine reports on the node connections in various parts of the
          ! HVAC syste: Component Sets, Air Loop, Plant and Condenser Loop, Supply and
          ! return air paths, controlled zones.
          ! This information should be useful in diagnosing node connection input errors.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: SameString
  USE DataAirLoop
  USE DataBranchNodeConnections
  USE DataLoopNode, ONLY: NumOfNodes,NodeID
  USE DataHVACGlobals
  USE DataHeatBalance, ONLY: Zone
  USE DataPlant
  USE DataZoneEquipment
  USE OutAirNodeManager, ONLY: OutsideAirNodeList, NumOutsideAirNodes
  USE DataErrorTracking, ONLY: AbortProcessing,AskForConnectionsReport  ! used here to turn off Node Connection Error reporting
  USE DualDuct, ONLY: ReportDualDuctConnections
  USE DataGlobals, ONLY: OutputFileBNDetails

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER :: errstring='**error**'
  CHARACTER(len=*), PARAMETER :: Blank=' '

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  CHARACTER(len=20) ChrOut
  CHARACTER(len=20) ChrOut2
  CHARACTER(len=20) ChrOut3
  CHARACTER(len=6)  LoopString
  CHARACTER(len=MaxNameLength) ChrName
  INTEGER Count
  INTEGER Count1
  INTEGER LoopSideNum
  INTEGER Num
  LOGICAL :: WarningOut=.true.
  INTEGER :: NumOfControlledZones

          ! Report outside air node names on the Branch-Node Details file
  WRITE(OutputFileBNDetails,701) '! ==============================================================='
  WRITE(OutputFileBNDetails,701) '! #Outdoor Air Nodes,<Number of Outdoor Air Nodes>'
  WRITE(ChrOut,*) NumOutsideAirNodes
  WRITE(OutputFileBNDetails,701) ' #Outdoor Air Nodes,'//ADJUSTL(ChrOut)
  IF (NumOutsideAirNodes > 0) THEN
    WRITE(OutputFileBNDetails,701) '! <Outdoor Air Node>,<NodeNumber>,<Node Name>'
  ENDIF
  DO Count = 1, NumOutsideAirNodes
    WRITE(ChrOut,*) OutsideAirNodeList(Count)
    ChrOut=ADJUSTL(ChrOut)
    WRITE(OutputFileBNDetails,701) ' Outdoor Air Node,'//TRIM(ChrOut)//','//TRIM(NodeID(OutsideAirNodeList(Count)))
  ENDDO
          ! Component Sets
  WRITE(OutputFileBNDetails,701) '! ==============================================================='
  WRITE(OutputFileBNDetails,700)
  WRITE(ChrOut,*) NumCompSets
  WRITE(OutputFileBNDetails,701) ' #Component Sets,'//TRIM(ADJUSTL(ChrOut))
  WRITE(OutputFileBNDetails,702)

 700 FORMAT('! <#Component Sets>,<Number of Component Sets>')
 701 FORMAT(A)
 702 FORMAT('! <Component Set>,<Component Set Count>,<Parent Object Type>,<Parent Object Name>,',  &
            '<Component Type>,<Component Name>,<Inlet Node ID>,<Outlet Node ID>,<Description>')
 707 FORMAT(1X,A)
 713 FORMAT(A)


  DO Count=1,NumCompSets
    WRITE(ChrOut,*) Count
    ChrOut=ADJUSTL(ChrOut)
    WRITE(OutputFileBNDetails,701) ' Component Set,'//TRIM(ChrOut)//','//  &
                                    TRIM(CompSets(Count)%ParentCType)//','//TRIM(CompSets(Count)%ParentCName)//','//  &
                                    TRIM(CompSets(Count)%CType)//','//TRIM(CompSets(Count)%CName)//','//  &
                                    TRIM(CompSets(Count)%InletNodeName)//','//TRIM(CompSets(Count)%OutletNodeName)//','// &
                                    TRIM(CompSets(Count)%Description)

    IF (CompSets(Count)%ParentCType == 'UNDEFINED' .or. &
        CompSets(Count)%InletNodeName == 'UNDEFINED' .or. &
        CompSets(Count)%OutletNodeName == 'UNDEFINED') THEN
      IF (AbortProcessing .and. WarningOut) THEN
        CALL ShowWarningError('Node Connection errors shown during "fatal error" processing may be false '// &
                              'because not all inputs may have been retrieved.')
        WarningOut=.false.
      ENDIF
      CALL ShowWarningError ('Node Connection Error for object '//TRIM(CompSets(Count)%CType)// &
                             ', name='//TRIM(CompSets(Count)%CName))
      CALL ShowContinueError('  '//TRIM(CompSets(Count)%Description)//' not on any Branch or Parent Object')
      CALL ShowContinueError('  Inlet Node : '//TRIM(CompSets(Count)%InletNodeName))
      CALL ShowContinueError('  Outlet Node: '//TRIM(CompSets(Count)%OutletNodeName))
      NumNodeConnectionErrors=NumNodeConnectionErrors+1
      IF (SameString(CompSets(Count)%CType, 'SolarCollector:UnglazedTranspired') ) Then
       CALL ShowContinueError('This report does not necessarily indicate a problem for a MultiSystem Transpired Collector')
      ENDIF
    ENDIF
    IF (CompSets(Count)%Description == 'UNDEFINED') THEN
      IF (AbortProcessing .and. WarningOut) THEN
        CALL ShowWarningError('Node Connection errors shown during "fatal error" processing may be false '// &
                              'because not all inputs may have been retrieved.')
        WarningOut=.false.
      ENDIF
      CALL ShowWarningError ('Potential Node Connection Error for object '//TRIM(CompSets(Count)%CType)// &
                             ', name='//TRIM(CompSets(Count)%CName))
      CALL ShowContinueError('  Node Types are still UNDEFINED -- See Branch/Node Details file for further information')
      CALL ShowContinueError('  Inlet Node : '//TRIM(CompSets(Count)%InletNodeName))
      CALL ShowContinueError('  Outlet Node: '//TRIM(CompSets(Count)%OutletNodeName))
      NumNodeConnectionErrors=NumNodeConnectionErrors+1


    ENDIF
  ENDDO

  DO Count=1,NumCompSets
    DO Count1=Count+1,NumCompSets
      IF (CompSets(Count)%CType /= CompSets(Count1)%CType) CYCLE
      IF (CompSets(Count)%CName /= CompSets(Count1)%CName) CYCLE
      IF (CompSets(Count)%InletNodeName /= CompSets(Count1)%InletNodeName) CYCLE
      IF (CompSets(Count)%OutletNodeName /= CompSets(Count1)%OutletNodeName) CYCLE
      IF (AbortProcessing .and. WarningOut) THEN
        CALL ShowWarningError('Node Connection errors shown during "fatal error" processing may be false '// &
                              'because not all inputs may have been retrieved.')
        WarningOut=.false.
      ENDIF
      CALL ShowWarningError  ('Component plus inlet/outlet node pair used more than once:')
      CALL ShowContinueError ('  Component  : '//TRIM(CompSets(Count)%CType)//', name='//TRIM(CompSets(Count)%CName))
      CALL ShowContinueError ('  Inlet Node : '//TRIM(CompSets(Count)%InletNodeName))
      CALL ShowContinueError ('  Outlet Node: '//TRIM(CompSets(Count)%OutletNodeName))
      CALL ShowContinueError ('  Used by    : '//TRIM(CompSets(Count)%ParentCType)//' '//TRIM(CompSets(Count)%ParentCName))
      CALL ShowContinueError ('  and  by    : '//TRIM(CompSets(Count1)%ParentCType)//' '//TRIM(CompSets(Count1)%ParentCName))
      NumNodeConnectionErrors=NumNodeConnectionErrors+1
    ENDDO
  ENDDO
          !  Plant Loops
  WRITE(OutputFileBNDetails,701) '! ==============================================================='
  WRITE(ChrOut,*) NumPlantLoops
  ChrOut=ADJUSTL(ChrOut)
  WRITE(OutputFileBNDetails,713) '! <# Plant Loops>,<Number of Plant Loops>'
  WRITE(OutputFileBNDetails,707) '#Plant Loops,'//TRIM(ChrOut)
  WRITE(OutputFileBNDetails,713) '! <Plant Loop>,<Plant Loop Name>,<Loop Type>,<Inlet Node Name>,'//  &
                                 '<Outlet Node Name>,<Branch List>,<Connector List>'
  WRITE(OutputFileBNDetails,713) '! <Plant Loop Connector>,<Connector Type>,<Connector Name>,'// &
                                 '<Loop Name>,<Loop Type>,<Number of Inlets/Outlets>'
  WRITE(OutputFileBNDetails,713) '! <Plant Loop Connector Branches>,<Connector Node Count>,<Connector Type>,'// &
                                 '<Connector Name>,<Inlet Branch>,<Outlet Branch>,'// &
                                 '<Loop Name>,<Loop Type>'
  WRITE(OutputFileBNDetails,713) '! <Plant Loop Connector Nodes>,<Connector Node Count>,<Connector Type>,'// &
                                 '<Connector Name>,<Inlet Node>,<Outlet Node>,'// &
                                 '<Loop Name>,<Loop Type>'
  WRITE(OutputFileBNDetails,713) '! <Plant Loop Supply Connection>,<Plant Loop Name>,<Supply Side Outlet Node Name>,'//  &
                                 '<Demand Side Inlet Node Name>'
  WRITE(OutputFileBNDetails,713) '! <Plant Loop Return Connection>,<Plant Loop Name>,<Demand Side Outlet Node Name>,'//  &
                                 '<Supply Side Inlet Node Name>'
 DO Count=1,NumPlantLoops
  DO LoopSideNum = DemandSide, SupplySide
          !  Plant Supply Side Loop
    ! Demandside and supplyside is parametrized in DataPlant
    IF (LoopSideNum == DemandSide) THEN
     LoopString = 'Demand'
    ELSE IF(LoopSideNum == SupplySide) THEN
     LoopString = 'Supply'
    END IF

    WRITE(OutputFileBNDetails,713) ' Plant Loop,'//TRIM(PlantLoop(Count)%Name)//','//LoopString//','//  &
          TRIM(PlantLoop(Count)%LoopSide(LoopSideNum)%NodeNameIn)//','//   &
          TRIM(PlantLoop(Count)%LoopSide(LoopSideNum)%NodeNameOut)//','//  &
          TRIM(PlantLoop(Count)%LoopSide(LoopSideNum)%BranchList)//','//      &
          TRIM(PlantLoop(Count)%LoopSide(LoopSideNum)%ConnectList)
          !  Plant Supply Side Splitter
   DO Num = 1,PlantLoop(Count)%LoopSide(LoopSideNum)%NumSplitters
    IF (PlantLoop(Count)%LoopSide(LoopSideNum)%Splitter(Num)%Exists) THEN
      WRITE(ChrOut,*) PlantLoop(Count)%LoopSide(LoopSideNum)%Splitter(Num)%TotalOutletNodes
      WRITE(OutputFileBNDetails,713) '   Plant Loop Connector,Splitter,'// &
            TRIM(PlantLoop(Count)%LoopSide(LoopSideNum)%Splitter(Num)%Name)//','//  &
            TRIM(PlantLoop(Count)%Name)//','//LoopString//','//  &
            TRIM(ADJUSTL(ChrOut))
      DO Count1=1,PlantLoop(Count)%LoopSide(LoopSideNum)%Splitter(Num)%TotalOutletNodes
        WRITE(ChrOut,*) Count1
        ChrOut2=Blank
        ChrOut3=Blank
        IF (PlantLoop(Count)%LoopSide(LoopSideNum)%Splitter(Num)%BranchNumIn <= 0) THEN
          ChrOut2=errstring
        ENDIF
        IF (PlantLoop(Count)%LoopSide(LoopSideNum)%Splitter(Num)%BranchNumOut(Count1) <= 0) THEN
          ChrOut3=errstring
        ENDIF
        WRITE(OutputFileBNDetails,713,advance='No') '     Plant Loop Connector Branches,'//TRIM(ADJUSTL(ChrOut))//',Splitter,'// &
              TRIM(PlantLoop(Count)%LoopSide(LoopSideNum)%Splitter(Num)%Name)//','
        IF (ChrOut2 /= errstring) THEN
          WRITE(OutputFileBNDetails,713,advance='No')   &
             TRIM(PlantLoop(Count)%LoopSide(LoopSideNum)%Branch(PlantLoop(Count)%  &
                                     LoopSide(LoopSideNum)%Splitter(Num)%BranchNumIn)%Name)//','
        ELSE
          WRITE(OutputFileBNDetails,713,advance='No') TRIM(ChrOut2)//','
        ENDIF
        IF (ChrOut3 /= errstring) THEN
          WRITE(OutputFileBNDetails,713)   &
              TRIM(PlantLoop(Count)%LoopSide(LoopSideNum)%Branch(PlantLoop(Count)%  &
                                     LoopSide(LoopSideNum)%Splitter(Num)%BranchNumOut(Count1))%Name)//','// &
              TRIM(PlantLoop(Count)%Name)//','//LoopString
        ELSE
          WRITE(OutputFileBNDetails,713)   &
              TRIM(ChrOut3)//','// &
              TRIM(PlantLoop(Count)%Name)//','//LoopString
        ENDIF
        WRITE(OutputFileBNDetails,713) '     Plant Loop Connector Nodes,   '//TRIM(ADJUSTL(ChrOut))//',Splitter,'// &
              TRIM(PlantLoop(Count)%LoopSide(LoopSideNum)%Splitter(Num)%Name)//','//  &
              TRIM(PlantLoop(Count)%LoopSide(LoopSideNum)%Splitter(Num)%NodeNameIn)//','//  &
              TRIM(PlantLoop(Count)%LoopSide(LoopSideNum)%Splitter(Num)%NodeNameOut(Count1))//','//  &
              TRIM(PlantLoop(Count)%Name)//','//LoopString
      ENDDO
    ENDIF
   END DO
          !  Plant Supply Side Mixer
   DO Num = 1, PlantLoop(Count)%LoopSide(LoopSideNum)%NumMixers
    IF (PlantLoop(Count)%LoopSide(LoopSideNum)%Mixer(Num)%Exists) THEN
      WRITE(ChrOut,*) PlantLoop(Count)%LoopSide(LoopSideNum)%Mixer(Num)%TotalInletNodes
      WRITE(OutputFileBNDetails,713) '   Plant Loop Connector,Mixer,'// &
            TRIM(PlantLoop(Count)%LoopSide(LoopSideNum)%Mixer(Num)%Name)//','//  &
            TRIM(PlantLoop(Count)%Name)//','//LoopString//','//     &    !',Supply,'//  &
            TRIM(ADJUSTL(ChrOut))
      DO Count1=1,PlantLoop(Count)%LoopSide(LoopSideNum)%Mixer(Num)%TotalInletNodes
        WRITE(ChrOut,*) Count1
        ChrOut2=Blank
        ChrOut3=Blank
        IF (PlantLoop(Count)%LoopSide(LoopSideNum)%Mixer(Num)%BranchNumIn(Count1) <= 0) THEN
          ChrOut2=errstring
        ENDIF
        IF (PlantLoop(Count)%LoopSide(LoopSideNum)%Mixer(Num)%BranchNumOut <= 0) THEN
          ChrOut3=errstring
        ENDIF
        WRITE(OutputFileBNDetails,713,advance='No') '     Plant Loop Connector Branches,'//TRIM(ADJUSTL(ChrOut))//',Mixer,'// &
              TRIM(PlantLoop(Count)%LoopSide(LoopSideNum)%Mixer(Num)%Name)//','
        IF (ChrOut2 /= errstring) THEN
          WRITE(OutputFileBNDetails,713,advance='No')  &
              TRIM(PlantLoop(Count)%LoopSide(LoopSideNum)%Branch(PlantLoop(Count)%  &
                                     LoopSide(LoopSideNum)%Mixer(Num)%BranchNumIn(Count1))%Name)//','
        ELSE
          WRITE(OutputFileBNDetails,713,advance='No') TRIM(ChrOut2)//','
        ENDIF
        IF (ChrOut3 /= errstring) THEN
          WRITE(OutputFileBNDetails,713)  &
              TRIM(PlantLoop(Count)%LoopSide(LoopSideNum)%Branch(PlantLoop(Count)%  &
                                      LoopSide(LoopSideNum)%Mixer(Num)%BranchNumOut)%Name)//','//  &
              TRIM(PlantLoop(Count)%Name)//','//LoopString
        ELSE
          WRITE(OutputFileBNDetails,713)   &
              TRIM(ChrOut3)//','// &
              TRIM(PlantLoop(Count)%Name)//',Supply'
        ENDIF
        WRITE(OutputFileBNDetails,713) '     Plant Loop Connector Nodes,   '//TRIM(ADJUSTL(ChrOut))//',Mixer,'// &
              TRIM(PlantLoop(Count)%LoopSide(LoopSideNum)%Mixer(Num)%Name)//','//  &
              TRIM(PlantLoop(Count)%LoopSide(LoopSideNum)%Mixer(Num)%NodeNameIn(Count1))//','//  &
              TRIM(PlantLoop(Count)%LoopSide(LoopSideNum)%Mixer(Num)%NodeNameOut)//','//  &
              TRIM(PlantLoop(Count)%Name)//','//LoopString
      ENDDO
    ENDIF
   END DO
  END DO
    WRITE(OutputFileBNDetails,713) ' Plant Loop Supply Connection,'//TRIM(PlantLoop(Count)%Name)//','//  &
          TRIM(PlantLoop(Count)%LoopSide(SupplySide)%NodeNameOut)//','//  &
          TRIM(PlantLoop(Count)%LoopSide(DemandSide)%NodeNameIn)
    WRITE(OutputFileBNDetails,713) ' Plant Loop Return Connection,'//TRIM(PlantLoop(Count)%Name)//','//  &
          TRIM(PlantLoop(Count)%LoopSide(DemandSide)%NodeNameOut)//','//  &
          TRIM(PlantLoop(Count)%LoopSide(SupplySide)%NodeNameIn)

 END DO         !  Plant Demand Side Loop

          !  Condenser Loops
  WRITE(OutputFileBNDetails,701) '! ==============================================================='
  WRITE(ChrOut,*) NumCondLoops
  ChrOut=ADJUSTL(ChrOut)
  WRITE(OutputFileBNDetails,713) '! <# Condenser Loops>,<Number of Condenser Loops>'
  WRITE(OutputFileBNDetails,707) '#Condenser Loops,'//TRIM(ChrOut)
  WRITE(OutputFileBNDetails,713) '! <Condenser Loop>,<Condenser Loop Name>,<Loop Type>,<Inlet Node Name>,'//  &
                                 '<Outlet Node Name>,<Branch List>,<Connector List>'
  WRITE(OutputFileBNDetails,713) '! <Condenser Loop Connector>,<Connector Type>,<Connector Name>,'// &
                                 '<Loop Name>,<Loop Type>,<Number of Inlets/Outlets>'
  WRITE(OutputFileBNDetails,713) '! <Condenser Loop Connector Branches>,<Connector Node Count>,<Connector Type>,'// &
                                 '<Connector Name>,<Inlet Branch>,<Outlet Branch>,'// &
                                 '<Loop Name>,<Loop Type>'
  WRITE(OutputFileBNDetails,713) '! <Condenser Loop Connector Nodes>,<Connector Node Count>,<Connector Type>,'// &
                                 '<Connector Name>,<Inlet Node>,<Outlet Node>,'// &
                                 '<Loop Name>,<Loop Type>'
  WRITE(OutputFileBNDetails,713) '! <Condenser Loop Supply Connection>,<Condenser Loop Name>,<Supply Side Outlet Node Name>,'//  &
                                 '<Demand Side Inlet Node Name>'
  WRITE(OutputFileBNDetails,713) '! <Condenser Loop Return Connection>,<Condenser Loop Name>,<Demand Side Outlet Node Name>,'//  &
                                 '<Supply Side Inlet Node Name>'

 DO Count=NumPlantLoops+1,TotNumLoops
  DO LoopSideNum = DemandSide, SupplySide
          !  Plant Supply Side Loop
    ! Demandside and supplyside is parametrized in DataPlant
    IF (LoopSideNum == DemandSide) THEN
     LoopString = 'Demand'
    ELSE IF(LoopSideNum == SupplySide) THEN
     LoopString = 'Supply'
    END IF

    WRITE(OutputFileBNDetails,713) ' Plant Loop,'//TRIM(PlantLoop(Count)%Name)//','//LoopString//','//  &
          TRIM(PlantLoop(Count)%LoopSide(LoopSideNum)%NodeNameIn)//','//   &
          TRIM(PlantLoop(Count)%LoopSide(LoopSideNum)%NodeNameOut)//','//  &
          TRIM(PlantLoop(Count)%LoopSide(LoopSideNum)%BranchList)//','//      &
          TRIM(PlantLoop(Count)%LoopSide(LoopSideNum)%ConnectList)
          !  Plant Supply Side Splitter
   DO Num = 1,PlantLoop(Count)%LoopSide(LoopSideNum)%NumSplitters
    IF (PlantLoop(Count)%LoopSide(LoopSideNum)%Splitter(Num)%Exists) THEN
      WRITE(ChrOut,*) PlantLoop(Count)%LoopSide(LoopSideNum)%Splitter(Num)%TotalOutletNodes
      WRITE(OutputFileBNDetails,713) '   Plant Loop Connector,Splitter,'// &
            TRIM(PlantLoop(Count)%LoopSide(LoopSideNum)%Splitter(Num)%Name)//','//  &
            TRIM(PlantLoop(Count)%Name)//','//LoopString//','//  &
            TRIM(ADJUSTL(ChrOut))
      DO Count1=1,PlantLoop(Count)%LoopSide(LoopSideNum)%Splitter(Num)%TotalOutletNodes
        WRITE(ChrOut,*) Count1
        ChrOut2=Blank
        ChrOut3=Blank
        IF (PlantLoop(Count)%LoopSide(LoopSideNum)%Splitter(Num)%BranchNumIn <= 0) THEN
          ChrOut2=errstring
        ENDIF
        IF (PlantLoop(Count)%LoopSide(LoopSideNum)%Splitter(Num)%BranchNumOut(Count1) <= 0) THEN
          ChrOut3=errstring
        ENDIF
        WRITE(OutputFileBNDetails,713,advance='No') '     Plant Loop Connector Branches,'//TRIM(ADJUSTL(ChrOut))//',Splitter,'// &
              TRIM(PlantLoop(Count)%LoopSide(LoopSideNum)%Splitter(Num)%Name)//','
        IF (ChrOut2 /= errstring) THEN
          WRITE(OutputFileBNDetails,713,advance='No')   &
             TRIM(PlantLoop(Count)%LoopSide(LoopSideNum)%Branch(PlantLoop(Count)%  &
                                     LoopSide(LoopSideNum)%Splitter(Num)%BranchNumIn)%Name)//','
        ELSE
          WRITE(OutputFileBNDetails,713,advance='No') TRIM(ChrOut2)//','
        ENDIF
        IF (ChrOut3 /= errstring) THEN
          WRITE(OutputFileBNDetails,713)   &
              TRIM(PlantLoop(Count)%LoopSide(LoopSideNum)%Branch(PlantLoop(Count)%  &
                                     LoopSide(LoopSideNum)%Splitter(Num)%BranchNumOut(Count1))%Name)//','// &
              TRIM(PlantLoop(Count)%Name)//','//LoopString
        ELSE
          WRITE(OutputFileBNDetails,713)   &
              TRIM(ChrOut3)//','// &
              TRIM(PlantLoop(Count)%Name)//','//LoopString
        ENDIF
        WRITE(OutputFileBNDetails,713) '     Plant Loop Connector Nodes,   '//TRIM(ADJUSTL(ChrOut))//',Splitter,'// &
              TRIM(PlantLoop(Count)%LoopSide(LoopSideNum)%Splitter(Num)%Name)//','//  &
              TRIM(PlantLoop(Count)%LoopSide(LoopSideNum)%Splitter(Num)%NodeNameIn)//','//  &
              TRIM(PlantLoop(Count)%LoopSide(LoopSideNum)%Splitter(Num)%NodeNameOut(Count1))//','//  &
              TRIM(PlantLoop(Count)%Name)//','//LoopString
      ENDDO
    ENDIF
   END DO
          !  Plant Supply Side Mixer
   DO Num = 1, PlantLoop(Count)%LoopSide(LoopSideNum)%NumMixers
    IF (PlantLoop(Count)%LoopSide(LoopSideNum)%Mixer(Num)%Exists) THEN
      WRITE(ChrOut,*) PlantLoop(Count)%LoopSide(LoopSideNum)%Mixer(Num)%TotalInletNodes
      WRITE(OutputFileBNDetails,713) '   Plant Loop Connector,Mixer,'// &
            TRIM(PlantLoop(Count)%LoopSide(LoopSideNum)%Mixer(Num)%Name)//','//  &
            TRIM(PlantLoop(Count)%Name)//','//LoopString//','//     &    !',Supply,'//  &
            TRIM(ADJUSTL(ChrOut))
      DO Count1=1,PlantLoop(Count)%LoopSide(LoopSideNum)%Mixer(Num)%TotalInletNodes
        WRITE(ChrOut,*) Count1
        ChrOut2=Blank
        ChrOut3=Blank
        IF (PlantLoop(Count)%LoopSide(LoopSideNum)%Mixer(Num)%BranchNumIn(Count1) <= 0) THEN
          ChrOut2=errstring
        ENDIF
        IF (PlantLoop(Count)%LoopSide(LoopSideNum)%Mixer(Num)%BranchNumOut <= 0) THEN
          ChrOut3=errstring
        ENDIF
        WRITE(OutputFileBNDetails,713,advance='No') '     Plant Loop Connector Branches,'//TRIM(ADJUSTL(ChrOut))//',Mixer,'// &
              TRIM(PlantLoop(Count)%LoopSide(LoopSideNum)%Mixer(Num)%Name)//','
        IF (ChrOut2 /= errstring) THEN
          WRITE(OutputFileBNDetails,713,advance='No')  &
              TRIM(PlantLoop(Count)%LoopSide(LoopSideNum)%Branch(PlantLoop(Count)%  &
                                     LoopSide(LoopSideNum)%Mixer(Num)%BranchNumIn(Count1))%Name)//','
        ELSE
          WRITE(OutputFileBNDetails,713,advance='No') TRIM(ChrOut2)//','
        ENDIF
        IF (ChrOut3 /= errstring) THEN
          WRITE(OutputFileBNDetails,713)  &
              TRIM(PlantLoop(Count)%LoopSide(LoopSideNum)%Branch(PlantLoop(Count)%  &
                                      LoopSide(LoopSideNum)%Mixer(Num)%BranchNumOut)%Name)//','//  &
              TRIM(PlantLoop(Count)%Name)//','//LoopString
        ELSE
          WRITE(OutputFileBNDetails,713)   &
              TRIM(ChrOut3)//','// &
              TRIM(PlantLoop(Count)%Name)//',Supply'
        ENDIF
        WRITE(OutputFileBNDetails,713) '     Plant Loop Connector Nodes,   '//TRIM(ADJUSTL(ChrOut))//',Mixer,'// &
              TRIM(PlantLoop(Count)%LoopSide(LoopSideNum)%Mixer(Num)%Name)//','//  &
              TRIM(PlantLoop(Count)%LoopSide(LoopSideNum)%Mixer(Num)%NodeNameIn(Count1))//','//  &
              TRIM(PlantLoop(Count)%LoopSide(LoopSideNum)%Mixer(Num)%NodeNameOut)//','//  &
              TRIM(PlantLoop(Count)%Name)//','//LoopString
      ENDDO
    ENDIF
   END DO
  END DO
    WRITE(OutputFileBNDetails,713) ' Plant Loop Supply Connection,'//TRIM(PlantLoop(Count)%Name)//','//  &
          TRIM(PlantLoop(Count)%LoopSide(SupplySide)%NodeNameOut)//','//  &
          TRIM(PlantLoop(Count)%LoopSide(DemandSide)%NodeNameIn)
    WRITE(OutputFileBNDetails,713) ' Plant Loop Return Connection,'//TRIM(PlantLoop(Count)%Name)//','//  &
          TRIM(PlantLoop(Count)%LoopSide(DemandSide)%NodeNameOut)//','//  &
          TRIM(PlantLoop(Count)%LoopSide(SupplySide)%NodeNameIn)

 END DO         !  Plant Demand Side Loop

  WRITE(OutputFileBNDetails,701) '! ==============================================================='
  NumOfControlledZones=0
  DO Count=1,NumOfZones
    IF (.not. ALLOCATED(ZoneEquipConfig)) CYCLE
    IF (ZoneEquipConfig(Count)%IsControlled) NumOfControlledZones=NumOfControlledZones+1
  ENDDO
  WRITE(ChrOut,*) NumOfControlledZones
  ChrOut=ADJUSTL(ChrOut)
  IF (NumOfControlledZones > 0) THEN
    WRITE(OutputFileBNDetails,713) '! <# Controlled Zones>,<Number of Controlled Zones>'
    WRITE(OutputFileBNDetails,707) '#Controlled Zones,'//TRIM(ChrOut)
    WRITE(OutputFileBNDetails,713) '! <Controlled Zone>,<Controlled Zone Name>,<Equip List Name>,<Control List Name>,'//  &
                                   '<Zone Node Name>,<Return Air Node Name>,<# Inlet Nodes>,<# Exhaust Nodes>'
    WRITE(OutputFileBNDetails,713) '! <Controlled Zone Inlet>,<Inlet Node Count>,<Controlled Zone Name>,'//  &
                                   '<Supply Air Inlet Node Name>,<SD Sys:Cooling/Heating [DD:Cooling] Inlet Node Name>,'// &
                                   '<DD Sys:Heating Inlet Node Name>'
    WRITE(OutputFileBNDetails,713) '! <Controlled Zone Exhaust>,<Exhaust Node Count>,<Controlled Zone Name>,'// &
                                   '<Exhaust Air Node Name>'
    DO Count=1,NumOfZones
      IF (.not. ZoneEquipConfig(Count)%IsControlled) CYCLE
      WRITE(ChrOut,*) ZoneEquipConfig(Count)%NumInletNodes
      WRITE(ChrOut2,*) ZoneEquipConfig(Count)%NumExhaustNodes
      ChrOut=ADJUSTL(ChrOut)
      ChrOut2=ADJUSTL(ChrOut2)
      WRITE(OutputFileBNDetails,713) ' Controlled Zone,'//TRIM(ZoneEquipConfig(Count)%ZoneName)//','//  &
                             TRIM(ZoneEquipConfig(Count)%EquipListName)//','//TRIM(ZoneEquipConfig(Count)%ControlListName)//','//  &
                             TRIM(NodeID(ZoneEquipConfig(Count)%ZoneNode))//','//  &
                             TRIM(NodeID(ZoneEquipConfig(Count)%ReturnAirNode))//','//TRIM(ChrOut)//','//TRIM(ChrOut2)
      DO Count1=1,ZoneEquipConfig(Count)%NumInletNodes
        WRITE(ChrOut,*) Count1
        ChrOut=ADJUSTL(ChrOut)
        ChrName=NodeID(ZoneEquipConfig(Count)%AirDistUnitHeat(Count1)%InNode)
        IF (ChrName == 'Undefined') ChrName='N/A'
        WRITE(OutputFileBNDetails,713) '   Controlled Zone Inlet,'//TRIM(ChrOut)//','//  &
                             TRIM(ZoneEquipConfig(Count)%ZoneName)//','//  &
                             TRIM(NodeID(ZoneEquipConfig(Count)%InletNode(Count1)))//','//  &
                             TRIM(NodeID(ZoneEquipConfig(Count)%AirDistUnitCool(Count1)%InNode))//','//  &
                             TRIM(ChrName)
      ENDDO
      DO Count1=1,ZoneEquipConfig(Count)%NumExhaustNodes
        WRITE(ChrOut,*) Count1
        ChrOut=ADJUSTL(ChrOut)
        WRITE(OutputFileBNDetails,713) '   Controlled Zone Exhaust,'//TRIM(ChrOut)//','//  &
                             TRIM(ZoneEquipConfig(Count)%ZoneName)//','//  &
                             TRIM(NodeID(ZoneEquipConfig(Count)%ExhaustNode(Count1)))
      ENDDO
    ENDDO

            !Report Zone Equipment Lists to BND File
    WRITE(OutputFileBNDetails,721) '! ==============================================================='
    WRITE(OutputFileBNDetails,720)
    WRITE(ChrOut,*) NumOfControlledZones
    WRITE(OutputFileBNDetails,721) ' #Zone Equipment Lists,'//TRIM(ADJUSTL(ChrOut))
    WRITE(OutputFileBNDetails,722)
    WRITE(OutputFileBNDetails,723)
   720 FORMAT('! <#Zone Equipment Lists>,<Number of Zone Equipment Lists>')
   721 FORMAT(A)
   722 FORMAT('! <Zone Equipment List>,<Zone Equipment List Count>,<Zone Equipment List Name>,<Zone Name>,<Number of Components>')
   723 FORMAT('! <Zone Equipment Component>,<Component Count>,<Component Type>,<Component Name>,', &
                 '<Zone Name>,<Heating Priority>,<Cooling Priority>')

    DO Count=1,NumOfZones
            ! Zone equipment list array parallels controlled zone equipment array, so
            ! same index finds corresponding data from both arrays
      IF (.not. ZoneEquipConfig(Count)%IsControlled) CYCLE
      WRITE(ChrOut,*) Count
      WRITE(ChrOut2,*) ZoneEquipList(Count)%NumOfEquipTypes
      WRITE(OutputFileBNDetails,721) ' Zone Equipment List,'//TRIM(ADJUSTL(ChrOut))//','// &
            TRIM(ZoneEquipList(Count)%Name)//','//  &
            TRIM(ZoneEquipConfig(Count)%ZoneName)//','// &
            TRIM(ADJUSTL(ChrOut2))

      DO Count1=1,ZoneEquipList(Count)%NumOfEquipTypes
        WRITE(ChrOut,*) Count1
        WRITE(ChrOut2,*) ZoneEquipList(Count)%CoolingPriority(Count1)
        WRITE(ChrOut3,*) ZoneEquipList(Count)%HeatingPriority(Count1)
        WRITE(OutputFileBNDetails,721) '   Zone Equipment Component,'//TRIM(ADJUSTL(ChrOut))//','// &
              TRIM(ZoneEquipList(Count)%EquipType(Count1))//','//  &
              TRIM(ZoneEquipList(Count)%EquipName(Count1))//','//  &
              TRIM(ZoneEquipConfig(Count)%ZoneName)//','// &
              TRIM(ADJUSTL(ChrOut2))//','//  &
              TRIM(ADJUSTL(ChrOut3))
      ENDDO
    ENDDO
  ENDIF

          !Report Dual Duct Dampers to BND File
  CALL ReportDualDuctConnections

  IF (NumNodeConnectionErrors == 0) THEN
    CALL ShowMessage('No node connection errors were found.')
  ELSE
    WRITE(ChrOut,*) NumNodeConnectionErrors
    ChrOut=ADJUSTL(ChrOut)
    IF (NumNodeConnectionErrors > 1) THEN
      CALL ShowMessage('There were '//TRIM(ChrOut)//' node connection errors noted.')
    ELSE
      CALL ShowMessage('There was '//TRIM(ChrOut)//' node connection error noted.')
    ENDIF
  ENDIF

  AskForConnectionsReport=.false.

  RETURN

END SUBROUTINE ReportLoopConnections

SUBROUTINE ReportParentChildren

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   May 2005
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Reports parent compsets with ensuing children data.

          ! METHODOLOGY EMPLOYED:
          ! Uses IsParentObject,GetNumChildren,GetChildrenData

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na
  USE DataGlobals, ONLY: OutputFileDebug
  USE General, ONLY: TrimSigDigits
  USE DataBranchNodeConnections
  USE BranchNodeConnections

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), PARAMETER :: Blank=' '

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER Loop
  INTEGER Loop1
  CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: ChildCType
  CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: ChildCName
  CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: ChildInNodeName
  CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: ChildOutNodeName
  INTEGER, ALLOCATABLE, DIMENSION(:)                      :: ChildInNodeNum
  INTEGER, ALLOCATABLE, DIMENSION(:)                      :: ChildOutNodeNum
  INTEGER NumChildren
  LOGICAL ErrorsFound

  ErrorsFound=.false.
  WRITE(OutputFileDebug,'(A)') 'Node Type,CompSet Name,Inlet Node,OutletNode'
  DO Loop=1,NumOfActualParents
    NumChildren=GetNumChildren(ParentNodeList(Loop)%CType,ParentNodeList(Loop)%CName)
    IF (NumChildren > 0) THEN
      ALLOCATE(ChildCType(NumChildren))
      ALLOCATE(ChildCName(NumChildren))
      ALLOCATE(ChildInNodeName(NumChildren))
      ALLOCATE(ChildOutNodeName(NumChildren))
      ALLOCATE(ChildInNodeNum(NumChildren))
      ALLOCATE(ChildOutNodeNum(NumChildren))
      ChildCType=Blank
      ChildCName=Blank
      ChildInNodeName=Blank
      ChildOutNodeName=Blank
      ChildInNodeNum=0
      ChildOutNodeNum=0
      CALL GetChildrenData(ParentNodeList(Loop)%CType,ParentNodeList(Loop)%CName,NumChildren,               &
        ChildCType,ChildCName,ChildInNodeName,ChildInNodeNum,ChildOutNodeName,ChildOutNodeNum,  &
        ErrorsFound)
      if (Loop > 1) WRITE(outputfiledebug,'(1X,60("="))')
      WRITE(outputfiledebug,'(A)') ' Parent Node,'//TRIM(ParentNodeList(Loop)%CType)//':'//  &
                              TRIM(ParentNodeList(Loop)%CName)//','//  &
                              TRIM(ParentNodeList(Loop)%InletNodeName)//','//TRIM(ParentNodeList(Loop)%OutletNodeName)
      DO Loop1=1,NumChildren
        WRITE(outputfiledebug,'(A)') '..ChildNode,'//TRIM(ChildCType(Loop1))//':'//TRIM(ChildCName(Loop1))//','//  &
                        TRIM(ChildInNodeName(Loop1))//','//TRIM(ChildOutNodeName(Loop1))
      ENDDO
      DEALLOCATE(ChildCType)
      DEALLOCATE(ChildCName)
      DEALLOCATE(ChildInNodeName)
      DEALLOCATE(ChildOutNodeName)
      DEALLOCATE(ChildInNodeNum)
      DEALLOCATE(ChildOutNodeNum)
    ELSE
      if (Loop > 1) WRITE(outputfiledebug,'(1X,60("="))')
      WRITE(outputfiledebug,'(A)') ' Parent Node (no children),'//TRIM(ParentNodeList(Loop)%CType)//':'//  &
                              TRIM(ParentNodeList(Loop)%CName)//','//  &
                              TRIM(ParentNodeList(Loop)%InletNodeName)//','//TRIM(ParentNodeList(Loop)%OutletNodeName)
    ENDIF
  ENDDO

  RETURN

END SUBROUTINE ReportParentChildren

SUBROUTINE ReportCompSetMeterVariables

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   May 2005
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Reports comp set meter variables.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  use dataglobals, only: outputfiledebug
  USE DataBranchNodeConnections
  USE BranchNodeConnections
  USE DataGlobalConstants

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
  INTERFACE GetNumMeteredVariables
    FUNCTION GetNumMeteredVariables(ComponentType,ComponentName) RESULT(NumVariables)
      CHARACTER(len=*), INTENT(IN) :: ComponentType  ! Given Component Type
      CHARACTER(len=*), INTENT(IN) :: ComponentName  ! Given Component Name (user defined)
      INTEGER                      :: NumVariables
    END FUNCTION
  END INTERFACE

  INTERFACE GetMeteredVariables
    SUBROUTINE GetMeteredVariables(ComponentType,ComponentName,VarIndexes,VarTypes,IndexTypes,  &
                                   UnitsStrings,ResourceTypes,EndUses,Groups,Names,NumFound,VarIDs)
      CHARACTER(len=*),      INTENT(IN)            :: ComponentType  ! Given Component Type
      CHARACTER(len=*),      INTENT(IN)            :: ComponentName  ! Given Component Name (user defined)
      INTEGER, DIMENSION(:), INTENT(OUT)           :: VarIndexes     ! Variable Numbers
      INTEGER, DIMENSION(:), INTENT(OUT)           :: VarTypes       ! Variable Types (1=integer, 2=real, 3=meter)
      INTEGER, DIMENSION(:), INTENT(OUT)           :: IndexTypes     ! Variable Index Types (1=Zone,2=HVAC)
      CHARACTER(len=*), DIMENSION(:), INTENT(OUT)  :: UnitsStrings   ! UnitsStrings for each variable
      INTEGER, DIMENSION(:), INTENT(OUT)  :: ResourceTypes  ! ResourceTypes for each variable
      CHARACTER(len=*), DIMENSION(:),   &
                            OPTIONAL, INTENT(OUT)  :: EndUses        ! EndUses for each variable
      CHARACTER(len=*), DIMENSION(:),   &
                            OPTIONAL, INTENT(OUT)  :: Groups         ! Groups for each variable
      CHARACTER(len=*), DIMENSION(:),   &
                            OPTIONAL, INTENT(OUT)  :: Names          ! Variable Names for each variable
      INTEGER, OPTIONAL, INTENT(OUT)               :: NumFound       ! Number Found
      INTEGER, DIMENSION(:), OPTIONAL, INTENT(OUT) :: VarIDs         ! Variable Report Numbers
    END SUBROUTINE
  END INTERFACE

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: Loop
  INTEGER :: Loop1
  INTEGER :: NumVariables
  INTEGER, DIMENSION(:), ALLOCATABLE :: VarIndexes
  INTEGER, DIMENSION(:), ALLOCATABLE :: VarIDs
  INTEGER, DIMENSION(:), ALLOCATABLE :: IndexTypes
  INTEGER, DIMENSION(:), ALLOCATABLE :: VarTypes
  CHARACTER(len=MaxNameLength), DIMENSION(:), ALLOCATABLE :: UnitsStrings
  CHARACTER(len=MaxNameLength*2+1), DIMENSION(:), ALLOCATABLE :: VarNames
  INTEGER, DIMENSION(:), ALLOCATABLE :: ResourceTypes
  CHARACTER(len=MaxNameLength), DIMENSION(:), ALLOCATABLE :: EndUses
  CHARACTER(len=MaxNameLength), DIMENSION(:), ALLOCATABLE :: Groups

  WRITE(outputfiledebug,'(A)') ' CompSet,ComponentType,ComponentName,NumMeteredVariables'
  WRITE(outputfiledebug,'(A)') ' RepVar,ReportIndex,ReportID,ReportName,Units,ResourceType,EndUse,Group,IndexType'

  DO Loop=1,NumCompSets
    NumVariables=GetNumMeteredVariables(CompSets(Loop)%CType,CompSets(Loop)%CName)
    write(outputfiledebug,'(1X,"CompSet,",A,",",A,",",I5)') TRIM(CompSets(Loop)%CType),TRIM(CompSets(Loop)%CName),NumVariables
    IF (NumVariables <= 0) CYCLE
    ALLOCATE(VarIndexes(NumVariables))
    VarIndexes=0
    ALLOCATE(VarIDs(NumVariables))
    VarIDs=0
    ALLOCATE(IndexTypes(NumVariables))
    IndexTypes=0
    ALLOCATE(VarTypes(NumVariables))
    VarTypes=0
    ALLOCATE(VarNames(NumVariables))
    VarNames=' '
    ALLOCATE(UnitsStrings(NumVariables))
    UnitsStrings=' '
    ALLOCATE(ResourceTypes(NumVariables))
    ResourceTypes=0
    ALLOCATE(EndUses(NumVariables))
    EndUses=' '
    ALLOCATE(Groups(NumVariables))
    Groups=' '
    CALL GetMeteredVariables(CompSets(Loop)%CType,CompSets(Loop)%CName,VarIndexes,VarTypes,IndexTypes,  &
                               UnitsStrings,Names=VarNames,ResourceTypes=ResourceTypes,EndUses=EndUses, &
                               Groups=Groups,VarIDs=VarIDs)
    DO Loop1=1,NumVariables
      write(outputfiledebug,'(1X,"RepVar,",I5,",",I5,",",A,",[",A,"],",A,",",A,",",A,",",I5)')   &
                 VarIndexes(Loop1),VarIDs(Loop1),TRIM(VarNames(Loop1)),  &
                         TRIM(UnitsStrings(Loop1)),TRIM(GetResourceTypeChar(ResourceTypes(Loop1))),TRIM(EndUses(Loop1)),  &
                         TRIM(Groups(Loop1)),IndexTypes(Loop1)
    ENDDO
    DEALLOCATE(VarIndexes)
    DEALLOCATE(IndexTypes)
    DEALLOCATE(VarTypes)
    DEALLOCATE(VarIDs)
    DEALLOCATE(VarNames)
    DEALLOCATE(UnitsStrings)
    DEALLOCATE(ResourceTypes)
    DEALLOCATE(EndUses)
    DEALLOCATE(Groups)
  ENDDO

  RETURN

END SUBROUTINE ReportCompSetMeterVariables

SUBROUTINE PostIPProcessing

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   August 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This provides post processing (for errors, etc) directly after the InputProcessor
          ! finishes.  Code originally in the Input Processor.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE SQLiteProcedures,    ONLY: CreateSQLiteDatabase
  USE InputProcessor,      ONLY: PreProcessorCheck, OverallErrorFlag, CompactObjectsCheck,  &
                                 ParametricObjectsCheck, GetNumSectionsFound, PreScanReportingVariables, &
                                 NumOutOfRangeErrorsFound,NumBlankReqFieldFound,NumMiscErrorsFound
  USE FluidProperties,     ONLY: FluidIndex_Water,FluidIndex_EthyleneGlycol,FluidIndex_PropoleneGlycol,FindGlycol

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
   LOGICAL :: PreP_Fatal=.false.  ! True if a preprocessor flags a fatal error

   DoingInputProcessing=.false.

   Call CreateSQLiteDataBase

   Call PreProcessorCheck(PreP_Fatal)  ! Check Preprocessor objects for warning, severe, etc errors.

   Call CheckCachedIPErrors

   IF (PreP_Fatal) THEN
     CALL ShowFatalError('Preprocessor condition(s) cause termination.')
   ENDIF

   IF (OverallErrorFlag) THEN
     CALL ShowFatalError('IP: Errors occurred on processing IDF file. Preceding condition(s) cause termination.')
   ENDIF

   Call CompactObjectsCheck  ! Check to see if Compact Objects (CompactHVAC, etc) are in input file.
                             ! If so, ExpandObjects didn't get called...
   Call ParametricObjectsCheck ! check to see if any parametric objects are in the input file
                               ! parametric preprocessor was not run

   IF (NumOutOfRangeErrorsFound+NumBlankReqFieldFound+NumMiscErrorsFound > 0) THEN
     CALL ShowSevereError('IP: Out of "range" values and/or blank required fields found in input')
     CALL ShowFatalError('IP: Errors occurred on processing IDF file. Preceding condition(s) cause termination.')
   ENDIF

   IF (GetNumSectionsFound('DISPLAYALLWARNINGS') > 0) THEN
     DisplayAllWarnings=.true.
     DisplayExtraWarnings=.true.
     DisplayUnusedSchedules=.true.
     DisplayUnusedObjects=.true.
   ENDIF

   IF (GetNumSectionsFound('DISPLAYEXTRAWARNINGS') > 0) THEN
     DisplayExtraWarnings=.true.
   ENDIF

   IF (GetNumSectionsFound('DISPLAYUNUSEDOBJECTS') > 0) THEN
     DisplayUnusedObjects=.true.
   ENDIF

   IF (GetNumSectionsFound('DISPLAYUNUSEDSCHEDULES') > 0) THEN
     DisplayUnusedSchedules=.true.
   ENDIF

   IF (GetNumSectionsFound('DisplayZoneAirHeatBalanceOffBalance') > 0) THEN
     DisplayZoneAirHeatBalanceOffBalance=.true.
   ENDIF

   IF (GetNumSectionsFound('DISPLAYADVANCEDREPORTVARIABLES') > 0) THEN
     DisplayAdvancedReportVariables=.true.
   ENDIF

   !Set up more globals - process fluid input.
   FluidIndex_Water=FindGlycol('Water')
   FluidIndex_EthyleneGlycol=FindGlycol('EthyleneGlycol')
   FluidIndex_PropoleneGlycol=FindGlycol('PropoleneGlycol')

   CALL PreScanReportingVariables

  RETURN

END SUBROUTINE PostIPProcessing

SUBROUTINE CheckCachedIPErrors

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   August 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This routine displays the cached error messages after the preprocessor
          ! errors have been checked and produced.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE SQLiteProcedures,    ONLY: WriteOutputToSQLite,CreateSQLiteErrorRecord,UpdateSQLiteErrorRecord

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER :: fmta='(A)'

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: iostatus
  CHARACTER(len=500) ErrorMessage

  CLOSE(Unit=CacheIPErrorFile)
  OPEN(Unit=CacheIPErrorFile,File='eplusout.iperr')
  iostatus=0
  DO WHILE(iostatus == 0)
    READ(CacheIPErrorFile,fmta,iostat=iostatus) ErrorMessage
    if (iostatus /= 0) EXIT
    CALL ShowErrorMessage(trim(ErrorMessage))
    IF(WriteOutputToSQLite) THEN
      ! Following code relies on specific formatting of Severes, Warnings, and continues
      ! that occur in the IP processing.  Later ones -- i.e. Fatals occur after the
      ! automatic sending of error messages to SQLite are turned on.
      IF (ErrorMessage(5:5) == 'S') THEN
        CALL CreateSQLiteErrorRecord(1,1,ErrorMessage,0)
      ELSEIF (ErrorMessage(5:5) == 'W') THEN
        CALL CreateSQLiteErrorRecord(1,0,ErrorMessage,0)
      ELSEIF (ErrorMessage(7:7) == '~') THEN
        CALL UpdateSQLiteErrorRecord(ErrorMessage)
      ENDIF
    ENDIF
  ENDDO

  CLOSE(Unit=CacheIPErrorFile,Status='delete')

  RETURN

END SUBROUTINE CheckCachedIPErrors

SUBROUTINE CheckThreading

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   April 2012
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Check number of threads available versus number of surfaces, etc.

          ! METHODOLOGY EMPLOYED:
          ! Check Max Threads (OMP_NUM_THREADS) = MaxNumberOfThreads, iEnvSetThreads
          ! Check EP Max Threads (EP_OMP_NUM_THREADS) = iepEnvSetThreads
          ! Check if IDF input (ProgramControl) = iIDFSetThreads
          ! Check # active sims (cntActv) = inumActiveSims [report only?]

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataSystemVariables
  USE InputProcessor, ONLY: GetNumObjectsFound, GetObjectItem
  USE DataIPShortCuts
  USE General, ONLY: RoundSigDigits
#if defined(_OPENMP) && defined(HBIRE_USE_OMP)
use omp_lib, ONLY: omp_get_max_threads,omp_get_num_threads,omp_set_num_threads
#endif

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER :: Blank=' '

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  CHARACTER(len=10) :: cEnvValue
  INTEGER :: ios
  INTEGER :: TotHTSurfs        ! Number of BuildingSurface:Detailed items to obtain
  INTEGER :: TotDetailedWalls  ! Number of Wall:Detailed items to obtain
  INTEGER :: TotDetailedRoofs  ! Number of RoofCeiling:Detailed items to obtain
  INTEGER :: TotDetailedFloors ! Number of Floor:Detailed items to obtain
  INTEGER :: TotHTSubs         ! Number of FenestrationSurface:Detailed items to obtain
  INTEGER :: TotIntMass        ! Number of InternalMass items to obtain
  ! Simple Surfaces (Rectangular)
  INTEGER :: TotRectExtWalls   ! Number of Exterior Walls to obtain
  INTEGER :: TotRectIntWalls   ! Number of Adiabatic Walls to obtain
  INTEGER :: TotRectIZWalls    ! Number of Interzone Walls to obtain
  INTEGER :: TotRectUGWalls    ! Number of Underground to obtain
  INTEGER :: TotRectRoofs      ! Number of Roofs to obtain
  INTEGER :: TotRectCeilings   ! Number of Adiabatic Ceilings to obtain
  INTEGER :: TotRectIZCeilings ! Number of Interzone Ceilings to obtain
  INTEGER :: TotRectGCFloors   ! Number of Floors with Ground Contact to obtain
  INTEGER :: TotRectIntFloors  ! Number of Adiabatic Walls to obtain
  INTEGER :: TotRectIZFloors   ! Number of Interzone Floors to obtain
  INTEGER :: TotRectWindows
  INTEGER :: TotRectDoors
  INTEGER :: TotRectGlazedDoors
  INTEGER :: TotRectIZWindows
  INTEGER :: TotRectIZDoors
  INTEGER :: TotRectIZGlazedDoors
  INTEGER :: iIDFsetThreadsInput
  INTEGER :: NumAlphas
  INTEGER :: NumNumbers

  ! Figure out how many surfaces there are.
  TotHTSurfs            =GetNumObjectsFound('BuildingSurface:Detailed')
  TotDetailedWalls      =GetNumObjectsFound('Wall:Detailed')
  TotDetailedRoofs      =GetNumObjectsFound('RoofCeiling:Detailed')
  TotDetailedFloors     =GetNumObjectsFound('Floor:Detailed')
  TotHTSubs             =GetNumObjectsFound('FenestrationSurface:Detailed')
  TotIntMass            =GetNumObjectsFound('InternalMass')
  TotRectWindows        =GetNumObjectsFound('Window')
  TotRectDoors          =GetNumObjectsFound('Door')
  TotRectGlazedDoors    =GetNumObjectsFound('GlazedDoor')
  TotRectIZWindows      =GetNumObjectsFound('Window:Interzone')
  TotRectIZDoors        =GetNumObjectsFound('Door:Interzone')
  TotRectIZGlazedDoors  =GetNumObjectsFound('GlazedDoor:Interzone')
  TotRectExtWalls       =GetNumObjectsFound('Wall:Exterior')
  TotRectIntWalls       =GetNumObjectsFound('Wall:Adiabatic')
  TotRectIZWalls        =GetNumObjectsFound('Wall:Interzone')
  TotRectUGWalls        =GetNumObjectsFound('Wall:Underground')
  TotRectRoofs          =GetNumObjectsFound('Roof')
  TotRectCeilings       =GetNumObjectsFound('Ceiling:Adiabatic')
  TotRectIZCeilings     =GetNumObjectsFound('Ceiling:Interzone')
  TotRectGCFloors       =GetNumObjectsFound('Floor:GroundContact')
  TotRectIntFloors      =GetNumObjectsFound('Floor:Adiabatic ')
  TotRectIZFloors       =GetNumObjectsFound('Floor:Interzone')

  iNominalTotSurfaces=TotHTSurfs + TotDetailedWalls + TotDetailedRoofs + TotDetailedFloors +              &
        TotHTSubs + TotIntMass + TotRectWindows + TotRectDoors + TotRectGlazedDoors + TotRectIZWindows + &
        TotRectIZDoors + TotRectIZGlazedDoors + TotRectExtWalls + TotRectIntWalls + TotRectIZWalls +     &
        TotRectUGWalls + TotRectRoofs + TotRectCeilings + TotRectIZCeilings + TotRectGCFloors +          &
        TotRectIntFloors + TotRectIZFloors

#ifdef HBIRE_USE_OMP
  MaxNumberOfThreads=MAXTHREADS()
  Threading=.true.

  cEnvValue=' '
  CALL Get_Environment_Variable(cNumThreads,cEnvValue)
  IF (cEnvValue /= Blank) THEN
    lEnvSetThreadsInput=.true.
    READ(cEnvValue,*,IOSTAT=ios) iEnvSetThreads
    IF (ios /= 0) iEnvSetThreads=MaxNumberOfThreads
    IF (iEnvSetThreads == 0) iEnvSetThreads=MaxNumberOfThreads
  ENDIF

  cEnvValue=' '
  CALL Get_Environment_Variable(cepNumThreads,cEnvValue)
  IF (cEnvValue /= Blank) THEN
    lepSetThreadsInput=.true.
    READ(cEnvValue,*,IOSTAT=ios) iepEnvSetThreads
    IF (ios /= 0) iepEnvSetThreads=MaxNumberOfThreads
    IF (iepEnvSetThreads == 0) iepEnvSetThreads=MaxNumberOfThreads
  ENDIF

  cCurrentModuleObject='ProgramControl'
  IF (GetNumObjectsFound(cCurrentModuleObject) > 0) THEN
    CALL GetObjectItem(cCurrentModuleObject,1,cAlphaArgs,NumAlphas,rNumericArgs,NumNumbers,ios,  &
                   AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                   AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
    iIDFsetThreads=INT(rNumericArgs(1))
    lIDFsetThreadsInput=.true.
    IF (iIDFsetThreads <= 0) THEN
      iIDFsetThreads=MaxNumberOfThreads
      IF (lEnvSetThreadsInput) iIDFsetThreads=iEnvSetThreads
      IF (lepSetThreadsInput)  iIDFsetThreads=iepEnvSetThreads
    ENDIF
    IF (iIDFSetThreads > MaxNumberOfThreads) THEN
      CALL ShowWarningError('CheckThreading: Your chosen number of threads=['//trim(RoundSigDigits(iIDFSetThreads))//  &
         '] is greater than the maximum number of threads=['//trim(RoundSigDigits(MaxNumberOfThreads))//'].')
      CALL ShowContinueError('...execution time for this run may be degraded.')
    ENDIF
  ENDIF

  IF (iNominalTotSurfaces <= 30) THEN
    NumberIntRadThreads=1
    IF (lEnvSetThreadsInput) NumberIntRadThreads=iEnvSetThreads
    IF (lepSetThreadsInput)  NumberIntRadThreads=iepEnvSetThreads
    IF (lIDFSetThreadsInput) NumberIntRadThreads=iIDFSetThreads
  ELSE
    NumberIntRadThreads=MaxNumberOfThreads
    IF (lEnvSetThreadsInput) NumberIntRadThreads=iEnvSetThreads
    IF (lepSetThreadsInput)  NumberIntRadThreads=iepEnvSetThreads
    IF (lIDFSetThreadsInput) NumberIntRadThreads=iIDFSetThreads
  ENDIF
#else
  Threading=.false.
  cCurrentModuleObject='ProgramControl'
  IF (GetNumObjectsFound(cCurrentModuleObject) > 0) THEN
    CALL GetObjectItem(cCurrentModuleObject,1,cAlphaArgs,NumAlphas,rNumericArgs,NumNumbers,ios,  &
                   AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                   AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
    iIDFsetThreadsInput=INT(rNumericArgs(1))
    IF (iIDFsetThreads > 1) THEN
      CALL ShowWarningError('CheckThreading: '//trim(cCurrentModuleObject)//' is not available in this version.')
      CALL ShowContinueError('...user requested ['//trim(RoundSigDigits(iIDFsetThreadsInput))//'] threads.')
    ENDIF
  ENDIF
  MaxNumberOfThreads=1
#endif
  ! just reporting
  cEnvValue=' '
  CALL Get_Environment_Variable(cNumActiveSims,cEnvValue)
  IF (cEnvValue /= Blank) THEN
    lnumActiveSims=.true.
    READ(cEnvValue,*,IOSTAT=ios) inumActiveSims
  ENDIF

  RETURN

END SUBROUTINE CheckThreading

END MODULE SimulationManager


! EXTERNAL SUBROUTINES:

SUBROUTINE Resimulate(ResimExt, ResimHB, ResimHVAC)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Peter Graham Ellis
          !       DATE WRITTEN   August 2005
          !       MODIFIED       Sep 2011 LKL/BG - resimulate only zones needing it for Radiant systems
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is called as necessary by the Demand Manager to resimulate some of the modules that have
          ! already been simulated for the current timestep.  For example, if LIGHTS are demand limited, the lighting
          ! power is reduced which also impacts the zone internal heat gains and therefore requires that the entire
          ! zone heat balance must be resimulated.

          ! METHODOLOGY EMPLOYED:
          ! If the zone heat balance must be resimulated, all the major subroutines are called sequentially in order
          ! to recalculate the impacts of demand limiting.  This routine is called from ManageHVAC _before_ any variables
          ! are reported or histories are updated.  This routine can be called multiple times without the overall
          ! simulation moving forward in time.
          !
          ! If only HVAC components are demand limited, then the HVAC system is resimulated, not the entire heat balance.
          ! Similarly, if ony exterior lights and equipment are demand limited, it is only necessary to resimulate the
          ! exterior energy use, not the entire heat balance, nor the HVAC system.
          !
          ! Below is the hierarchy of subroutine calls.  The calls marked with an asterisk are resimulated here.
          !
          ! ManageSimulation
          !     ManageWeather
          !     ManageDemand
          !   * ManageExteriorEnergyUse
          !     ManageHeatBalance
          !       * InitHeatBalance
          !             PerformSolarCalculations
          !         ManageSurfaceHeatBalance
          !           * InitSurfaceHeatBalance
          !                 ManageInternalHeatGains
          !           * CalcHeatBalanceOutsideSurf
          !           * CalcHeatBalanceInsideSurf
          !             ManageAirHeatBalance
          !                *InitAirHeatBalance
          !                 CalcHeatBalanceAir
          !                   * CalcAirFlow
          !                   * ManageRefrigeratedCaseRacks
          !                     ManageHVAC
          !                       * ManageZoneAirUpdates 'GET ZONE SETPOINTS'
          !                       * ManageZoneAirUpdates 'PREDICT'
          !                       * SimHVAC
          !                         UpdateDataandReport
          !                 ReportAirHeatBalance
          !             UpdateFinalSurfaceHeatBalance
          !             UpdateThermalHistories
          !             UpdateMoistureHistories
          !             ManageThermalComfort
          !             ReportSurfaceHeatBalance
          !         RecKeepHeatBalance
          !         ReportHeatBalance

          ! USE STATEMENTS:
  USE DataPrecisionGlobals
  USE DemandManager, ONLY: DemandManagerExtIterations, DemandManagerHBIterations, DemandManagerHVACIterations
  USE ExteriorEnergyUse, ONLY: ManageExteriorEnergyUse
  USE HeatBalanceSurfaceManager, ONLY: InitSurfaceHeatBalance
  USE HeatBalanceAirManager, ONLY: InitAirHeatBalance
  USE RefrigeratedCase, ONLY: ManageRefrigeratedCaseRacks
  USE ZoneTempPredictorCorrector, ONLY: ManageZoneAirUpdates
  USE DataHeatBalFanSys, ONLY: iGetZoneSetpoints, iPredictStep, iCorrectStep
  USE HVACManager, ONLY: SimHVAC, CalcAirFlowSimple
  USE DataInterfaces, ONLY: ShowSevereError, ShowContinueErrorTimeStamp, &
                            CalcHeatBalanceOutsideSurf, CalcHeatBalanceInsideSurf
  USE DataHVACGlobals, ONLY: UseZoneTimeStepHistory !, InitDSwithZoneHistory
  USE ZoneContaminantPredictorCorrector, ONLY: ManageZoneContaminanUpdates
  USE DataContaminantBalance, ONLY: Contaminant

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  LOGICAL, INTENT(INOUT) :: ResimExt   ! Flag to resimulate the exterior energy use simulation
  LOGICAL, INTENT(INOUT) :: ResimHB    ! Flag to resimulate the heat balance simulation (including HVAC)
  LOGICAL, INTENT(INOUT) :: ResimHVAC  ! Flag to resimulate the HVAC simulation

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64) :: ZoneTempChange               ! Dummy variable needed for calling ManageZoneAirUpdates

          ! FLOW:
  IF (ResimExt) THEN
    CALL ManageExteriorEnergyUse

    DemandManagerExtIterations = DemandManagerExtIterations + 1
  END IF

  IF (ResimHB) THEN
    ! Surface simulation
    CALL InitSurfaceHeatBalance
    CALL CalcHeatBalanceOutsideSurf
    CALL CalcHeatBalanceInsideSurf

    ! Air simulation
    CALL InitAirHeatBalance
    CALL ManageRefrigeratedCaseRacks

    DemandManagerHBIterations = DemandManagerHBIterations + 1
    ResimHVAC = .TRUE.  ! Make sure HVAC is resimulated too
  END IF

  IF (ResimHVAC) THEN
    ! HVAC simulation
    CALL ManageZoneAirUpdates(iGetZoneSetpoints,ZoneTempChange,.FALSE.,  UseZoneTimeStepHistory, &
              0.0D0 )
    If (Contaminant%SimulateContaminants) &
      CALL ManageZoneContaminanUpdates(iGetZoneSetpoints,.FALSE.,UseZoneTimeStepHistory,0.0D0)
    CALL CalcAirFlowSimple
    CALL ManageZoneAirUpdates(iPredictStep,ZoneTempChange,.FALSE.,  UseZoneTimeStepHistory,  &
               0.0D0 )
    If (Contaminant%SimulateContaminants) &
      CALL ManageZoneContaminanUpdates(iPredictStep,.FALSE.,UseZoneTimeStepHistory,0.0D0 )
    CALL SimHVAC

    DemandManagerHVACIterations = DemandManagerHVACIterations + 1
  END IF

  RETURN

END SUBROUTINE Resimulate

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
