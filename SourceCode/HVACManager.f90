MODULE HVACManager

          ! PURPOSE OF THIS MODULE:
          ! This module contains the high level HVAC control
          ! subroutines.  Subroutine ManageHVAC, which is called from the heat balance,
          ! calls the HVAC simulation and is the most probable insertion point for
          ! connections to other HVAC engines.  ManageHVAC also controls the system
          ! timestep, automatically shortening the timestep to meet convergence criteria.

          ! METHODOLOGY EMPLOYED:
          ! The basic solution technique is iteration with lagging.
          ! The timestep is shortened using a bisection method.

          ! REFERENCES:


          ! USE STATEMENTS:
USE DataPrecisionGlobals
USE DataGlobals, ONLY: TimeStepZone,WarmUpFlag,EndHourFlag,  &
                       BeginDayFlag,BeginEnvrnFlag,SecInHour,              &
                       BeginTimeStepFlag,KickOffSimulation,                &
                       DayOfSim,HourOfDay,TimeStep,NumOfZones,             &
                       OutputFileDebug,OutputFileStandard,OutputFileMeters,&
                       ZoneSizingCalc,DuringDay,DoOutputReporting,HVACTSReporting,  &
                       SysSizingCalc, DisplayExtraWarnings, MetersHaveBeenInitialized, &
                       emsCallFromBeginTimestepBeforePredictor, &
                       emsCallFromEndSystemTimestepAfterHVACReporting, emsCallFromBeforeHVACManagers, &
                       emsCallFromAfterHVACManagers, emsCallFromHVACIterationLoop, &
                       AnyEnergyManagementSystemInModel, &
                       emsCallFromEndSystemTimestepBeforeHVACReporting, &
                       AnyIdealCondEntSetPointInModel, &
                       RunOptCondEntTemp, isPulseZoneSizing
USE DataInterfaces, ONLY: SetupOutputVariable,ShowFatalError,                 &
                       ShowWarningError,ShowContinueError,ShowMessage,     &
                       ShowContinueErrorTimeStamp, ShowRecurringWarningErrorAtEnd
USE DataEnvironment

USE DataHeatBalFanSys, ONLY: ZTAV, ZT, MAT, ZoneAirHumRat, ZoneAirHumRatAvg, &
                                        iGetZoneSetpoints, iPredictStep, iCorrectStep, &
                                        iPushZoneTimestepHistories, iRevertZoneTimestepHistories, &
                                        iPushSystemTimestepHistories
USE DataHVACGlobals
USE DataLoopNode
USE DataAirLoop
USE DataConvergParams
USE DataAirflowNetwork, ONLY: SimulateAirflowNetwork,AirflowNetworkControlSimple
USE DataReportingFlags

IMPLICIT NONE ! Enforce explicit typing of all variables

          !MODULE PARAMETER DEFINITIONS:
          ! na

          !MODULE VARIABLE DECLARATIONS:
PRIVATE

INTEGER :: HVACManageIteration = 0            ! counts iterations to enforce maximum iteration limit
INTEGER :: RepIterAir = 0



LOGICAL, ALLOCATABLE, DIMENSION(:) :: CrossMixingReportFlag ! TRUE when Cross Mixing is active based on controls
LOGICAL, ALLOCATABLE, DIMENSION(:) :: MixingReportFlag      ! TRUE when Mixing is active based on controls
REAL(r64),    ALLOCATABLE, DIMENSION(:) :: VentMCP               ! product of mass rate and Cp for each Venitlation object


          !SUBROUTINE SPECIFICATIONS FOR MODULE PrimaryPlantLoops
PUBLIC     ManageHVAC              ! Call Zone Update and HVAC Simulation, Set system timestep.
PUBLIC     SimHVAC                 ! Manage HVAC simulation by calling plant loop, air loop
                                   ! and zone equipment simulations
PRIVATE    SimSelectedEquipment    !
PRIVATE    ResolveAirLoopFlowLimits
PRIVATE    ResetHVACControl
PRIVATE    ResolveLockoutFlags
PRIVATE    UpdateZoneListAndGroupLoads
PUBLIC     CalcAirFlowSimple
PRIVATE    ReportAirHeatBalance
PRIVATE    GetStandAloneERVNodes
PRIVATE    UpdateZoneInletConvergenceLog

CONTAINS
          ! MODULE SUBROUTINES:

SUBROUTINE ManageHVAC

          ! SUBROUTINE INFORMATION:
          !       AUTHORS:  Russ Taylor, Dan Fisher
          !       DATE WRITTEN:  Jan. 1998
          !       MODIFIED       Jul 2003 (CC) added a subroutine call for air models
          !       RE-ENGINEERED  May 2008, Brent Griffith, revised variable time step method and zone conditions history

          ! PURPOSE OF THIS SUBROUTINE:
          ! This routine effectively replaces the IBLAST
          ! "SystemDriver" routine.  The main function of the routine
          ! is to set the system timestep, "TimeStepSys", call the models related to zone
          ! air temperatures, and .

          ! METHODOLOGY EMPLOYED:
          !  manage calls to Predictor and Corrector and other updates in ZoneTempPredictorCorrector
          !  manage variable time step and when zone air histories are updated.
          !
          !

          ! REFERENCES:

          ! USE STATEMENTS:
  USE DataConvergParams, ONLY: MinTimeStepSys,   & ! =0.0166667     != 1 minute
                               MaxZoneTempDiff     ! 0.3 C = (1% OF 300 C) =max allowable diff between ZoneAirTemp at Time=T & T-1

  USE ZoneTempPredictorCorrector, ONLY: ManageZoneAirUpdates,DetectOscillatingZoneTemp

  USE NodeInputManager, ONLY: CalcMoreNodeInfo
  USE ZoneEquipmentManager,         ONLY : UpdateZoneSizing
  USE OutputReportTabular,          ONLY : UpdateTabularReports,GatherComponentLoadsHVAC !added for writing tabular output reports
  USE DataGlobals,                  ONLY : CompLoadReportIsReq
  USE SystemReports,                ONLY : InitEnergyReports, ReportMaxVentilationLoads, ReportSystemEnergyUse
  USE PollutionModule,              ONLY : CalculatePollution
  USE DemandManager,                ONLY : ManageDemand, UpdateDemandManagers
  USE EMSManager,                   ONLY : ManageEMS
  USE IceThermalStorage,            ONLY : UpdateIceFractions
  USE OutAirNodeManager,            ONLY : SetOutAirNodes
  USE AirflowNetworkBalanceManager, ONLY : ManageAirflowNetworkBalance
  USE DataAirflowNetwork,           ONLY : RollBackFlag
  USE WaterManager,                 ONLY : ManageWater, ManageWaterInits
  USE RefrigeratedCase,             ONLY : ManageRefrigeratedCaseRacks
  USE SystemAvailabilityManager,    ONLY : ManageHybridVentilation
  USE DataHeatBalFanSys, ONLY: SysDepZoneLoads, SysDepZoneLoadsLagged, ZTAVComf, ZoneAirHumRatAvgComf
  USE DataSystemVariables,          ONLY : ReportDuringWarmup, UpdateDataDuringWarmupExternalInterface ! added for FMI
  USE PlantManager,                 ONLY : UpdateNodeThermalHistory
  USE ZoneContaminantPredictorCorrector, ONLY: ManageZoneContaminanUpdates
  USE DataContaminantBalance,       ONLY: Contaminant, ZoneAirCO2, ZoneAirCO2Temp, ZoneAirCO2Avg, OutdoorCO2, &
                                          ZoneAirGC, ZoneAirGCTemp, ZoneAirGCAvg, OutdoorGC
  USE ScheduleManager,              ONLY: GetCurrentScheduleValue
  USE ManageElectricPower,          ONLY : ManageElectricLoadCenters
  USE InternalHeatGains,            ONLY : UpdateInternalGainValues

  IMPLICIT NONE ! Enforce explicit typing of all variables

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER :: EndOfHeaderFormat = "('End of Data Dictionary')"    ! End of data dictionary marker
  CHARACTER(len=*), PARAMETER :: EnvironmentStampFormat = "(a,',',a,3(',',f7.2),',',f7.2)" ! Format descriptor for environ stamp

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

        ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64)              :: PriorTimeStep                !magnitude of time step for previous history terms
  REAL(r64)              :: ZoneTempChange                  !change in zone air temperature from timestep t-1 to t
  Integer                :: NodeNum
  LOGICAL                :: ReportDebug
  LOGICAL,SAVE           :: TriggerGetAFN = .TRUE.
  INTEGER                :: ZoneNum
  LOGICAL, SAVE :: PrintedWarmup=.false.

  LOGICAL,SAVE           :: MyEnvrnFlag = .TRUE.
  LOGICAL,SAVE           :: InitVentReportFlag = .TRUE.
  LOGICAL,SAVE           :: DebugNamesReported=.false.

  INTEGER                :: ZTempTrendsNumSysSteps = 0
  INTEGER                :: SysTimestepLoop = 0
  LOGICAL                :: DummyLogical

          !SYSTEM INITIALIZATION
  IF (TriggerGetAFN) THEN
    TriggerGetAFN = .FALSE.
    CALL DisplayString('Initializing HVAC')
    CALL ManageAirflowNetworkBalance  ! first call only gets input and returns.
  END IF

  ZT = MAT
  ! save for use with thermal comfort control models (Fang, Pierce, and KSU)
  ZTAVComf = ZTAV
  ZoneAirHumRatAvgComf = ZoneAirHumRatAvg
  ZTAV = 0.0D0
  ZoneAirHumRatAvg = 0.0D0
  PrintedWarmup=.false.
  IF (Contaminant%CO2Simulation) Then
    OutdoorCO2 = GetCurrentScheduleValue(Contaminant%CO2OutdoorSchedPtr)
    ZoneAirCO2Avg = 0.0D0
  END IF
  IF (Contaminant%GenericContamSimulation) Then
    OutdoorGC = GetCurrentScheduleValue(Contaminant%GenericContamOutdoorSchedPtr)
    IF (ALLOCATED(ZoneAirGCAvg)) ZoneAirGCAvg = 0.0D0
  END IF

  IF (BeginEnvrnFlag .AND. MyEnvrnFlag) THEN
    CALL ResetNodeData
    AirLoopsSimOnce = .FALSE.
    MyEnvrnFlag = .FALSE.
    InitVentReportFlag = .TRUE.
    NumOfSysTimeStepsLastZoneTimeStep = 1
    PreviousTimeStep = TimeStepZone
  END IF
  IF (.NOT. BeginEnvrnFlag) THEN
    MyEnvrnFlag = .TRUE.
  END IF

  SysTimeElapsed              = 0.0d0
  TimeStepSys                 = TimeStepZone
  FirstTimeStepSysFlag        = .TRUE.
  ShortenTimeStepSys          = .FALSE.
  UseZoneTimeStepHistory      = .TRUE.
  PriorTimeStep               = TimeStepZone
  NumOfSysTimeSteps           = 1
  FracTimeStepZone            = TimeStepSys/TimeStepZone

  CALL ManageEMS(emsCallFromBeginTimestepBeforePredictor) !calling point

  CALL SetOutAirNodes

  CALL ManageRefrigeratedCaseRacks

   !ZONE INITIALIZATION  'Get Zone Setpoints'
  CALL ManageZoneAirUpdates(iGetZoneSetpoints,ZoneTempChange,ShortenTimeStepSys, &
                              UseZoneTimeStepHistory,PriorTimeStep )
  If (Contaminant%SimulateContaminants) &
    CALL ManageZoneContaminanUpdates(iGetZoneSetpoints,ShortenTimeStepSys,UseZoneTimeStepHistory,PriorTimeStep)

  CALL ManageHybridVentilation

  CALL CalcAirFlowSimple
  IF (SimulateAirflowNetwork .gt. AirflowNetworkControlSimple) THEN
    RollBackFlag = .FALSE.
    CALL ManageAirflowNetworkBalance(.FALSE.)
  END IF

  CALL SetHeatToReturnAirFlag

  SysDepZoneLoadsLagged = SysDepZoneLoads


  CALL UpdateInternalGainValues(SuppressRadiationUpdate = .TRUE., SumLatentGains = .TRUE.)

  CALL ManageZoneAirUpdates(iPredictStep,ZoneTempChange,ShortenTimeStepSys, &
                     UseZoneTimeStepHistory, &
                      PriorTimeStep )

  If (Contaminant%SimulateContaminants) &
    CALL ManageZoneContaminanUpdates(iPredictStep,ShortenTimeStepSys,UseZoneTimeStepHistory,PriorTimeStep)

  CALL SimHVAC

  IF (AnyIdealCondEntSetPointInModel .and. MetersHaveBeenInitialized .and. .NOT. WarmUpFlag) THEN
    RunOptCondEntTemp = .TRUE.
    DO WHILE (RunOptCondEntTemp)
      CALL SimHVAC
    END DO
  END IF

  CALL ManageWaterInits

  ! Only simulate once per zone timestep; must be after SimHVAC
  IF (FirstTimeStepSysFlag .and. MetersHaveBeenInitialized) THEN
    CALL ManageDemand
  END IF

  BeginTimeStepFlag = .FALSE.  ! At this point, we have been through the first pass through SimHVAC so this needs to be set

  CALL ManageZoneAirUpdates(iCorrectStep,ZoneTempChange,ShortenTimeStepSys, &
                           UseZoneTimeStepHistory, &
                           PriorTimeStep )
  If (Contaminant%SimulateContaminants) &
    CALL ManageZoneContaminanUpdates(iCorrectStep,ShortenTimeStepSys,UseZoneTimeStepHistory,PriorTimeStep)


  IF (ZoneTempChange > MaxZoneTempDiff .and. .not. KickOffSimulation)  THEN
    !determine value of adaptive system time step
    ! model how many system timesteps we want in zone timestep
    ZTempTrendsNumSysSteps = INT(ZoneTempChange / MaxZoneTempDiff + 1.0D0 ) ! add 1 for truncation
    NumOfSysTimeSteps      = MIN( ZTempTrendsNumSysSteps , LimitNumSysSteps )
    !then determine timestep length for even distribution, protect div by zero
    IF (NumOfSysTimeSteps > 0) TimeStepSys =   TimeStepZone / NumOfSysTimeSteps
    TimeStepSys = MAX( TimeStepSys  , MinTimeStepSys)
    UseZoneTimeStepHistory = .FALSE.
    ShortenTimeStepSys     = .TRUE.
  ELSE

    NumOfSysTimeSteps = 1
    UseZoneTimeStepHistory = .TRUE.
  ENDIF

  If (UseZoneTimeStepHistory) PreviousTimeStep = TimeStepZone
  DO SysTimestepLoop = 1,  NumOfSysTimeSteps

    IF (TimeStepSys .LT. TimeStepZone) THEN

      CALL ManageHybridVentilation
      CALL CalcAirFlowSimple(SysTimestepLoop)
      if (SimulateAirflowNetwork .gt. AirflowNetworkControlSimple) then
        RollBackFlag = .FALSE.
        CALL ManageAirflowNetworkBalance(.FALSE.)
      end if

      CALL UpdateInternalGainValues(SuppressRadiationUpdate = .TRUE., SumLatentGains = .TRUE.)

      CALL ManageZoneAirUpdates(iPredictStep,ZoneTempChange,ShortenTimeStepSys, &
                      UseZoneTimeStepHistory, &
                     PriorTimeStep )

      If (Contaminant%SimulateContaminants) &
        CALL ManageZoneContaminanUpdates(iPredictStep,ShortenTimeStepSys,UseZoneTimeStepHistory,PriorTimeStep)
      CALL SimHVAC

      IF (AnyIdealCondEntSetPointInModel .and. MetersHaveBeenInitialized .and. .NOT. WarmUpFlag) THEN
        RunOptCondEntTemp = .TRUE.
        DO WHILE (RunOptCondEntTemp)
          CALL SimHVAC
        END DO
      END IF

      CALL ManageWaterInits

      !Need to set the flag back since we do not need to shift the temps back again in the correct step.
      ShortenTimeStepSys = .FALSE.

      CALL ManageZoneAirUpdates(iCorrectStep,ZoneTempChange,ShortenTimeStepSys, &
                              UseZoneTimeStepHistory, PriorTimeStep )
      If (Contaminant%SimulateContaminants) &
        CALL ManageZoneContaminanUpdates(iCorrectStep,ShortenTimeStepSys,UseZoneTimeStepHistory,PriorTimeStep)

      CALL ManageZoneAirUpdates(iPushSystemTimeStepHistories,ZoneTempChange,ShortenTimeStepSys, &
                             UseZoneTimeStepHistory,  PriorTimeStep )
      If (Contaminant%SimulateContaminants) &
        CALL ManageZoneContaminanUpdates(iPushSystemTimeStepHistories,ShortenTimeStepSys,UseZoneTimeStepHistory,PriorTimeStep)
        PreviousTimeStep = TimeStepSys
    END IF

    FracTimeStepZone=TimeStepSys/TimeStepZone

    DO ZoneNum = 1, NumofZones
      ZTAV(ZoneNum) = ZTAV(ZoneNum) + ZT(ZoneNum) * FracTimeStepZone
      ZoneAirHumRatAvg(ZoneNum) = ZoneAirHumRatAvg(ZoneNum) + ZoneAirHumRat(ZoneNum)* FracTimeStepZone
      IF (Contaminant%CO2Simulation) &
        ZoneAirCO2Avg(ZoneNum) = ZoneAirCO2Avg(ZoneNum) + ZoneAirCO2(ZoneNum)* FracTimeStepZone
      IF (Contaminant%GenericContamSimulation) &
        ZoneAirGCAvg(ZoneNum) = ZoneAirGCAvg(ZoneNum) + ZoneAirGC(ZoneNum)* FracTimeStepZone
    END DO

    CALL DetectOscillatingZoneTemp
    CALL UpdateZoneListAndGroupLoads ! Must be called before UpdateDataandReport(HVACTSReporting)
    CALL UpdateIceFractions          ! Update fraction of ice stored in TES
    CALL ManageWater
    ! update electricity data for net, purchased, sold etc.
    DummyLogical = .FALSE.
    CALL ManageElectricLoadCenters(.FALSE.,DummyLogical, .TRUE. )
    ! Update the plant and condenser loop capacitance model temperature history.
    CALL UpdateNodeThermalHistory

    CALL ManageEMS(emsCallFromEndSystemTimestepBeforeHVACReporting)  ! EMS calling point

    ! This is where output processor data is updated for System Timestep reporting
    IF (.NOT. WarmUpFlag) THEN
      IF (DoOutputReporting) THEN
        CALL CalcMoreNodeInfo
        CALL CalculatePollution
        CALL InitEnergyReports
        CALL ReportSystemEnergyUse
      END IF
      IF (DoOutputReporting .OR. (ZoneSizingCalc .AND. CompLoadReportIsReq)) THEN
        CALL ReportAirHeatBalance
        IF (ZoneSizingCalc) CALL GatherComponentLoadsHVAC
      END IF
      IF (DoOutputReporting) THEN
        CALL ReportMaxVentilationLoads
        CALL UpdateDataandReport(HVACTSReporting)
        CALL UpdateTabularReports(HVACTSReporting)
      END IF
      IF (ZoneSizingCalc) THEN
        CALL UpdateZoneSizing(DuringDay)
      END IF
    ELSEIF (.not. KickOffSimulation .and. DoOutputReporting .and. ReportDuringWarmup) THEN
      IF (BeginDayFlag .and. .not. PrintEnvrnStampWarmupPrinted) THEN
        PrintEnvrnStampWarmup=.true.
        PrintEnvrnStampWarmupPrinted=.true.
      ENDIF
      IF (.not. BeginDayFlag) PrintEnvrnStampWarmupPrinted=.false.
      IF (PrintEnvrnStampWarmup) THEN
        IF (PrintEndDataDictionary .AND. DoOutputReporting .and. .not. PrintedWarmup) THEN
          WRITE (OutputFileStandard,EndOfHeaderFormat)
          WRITE (OutputFileMeters,EndOfHeaderFormat)
          PrintEndDataDictionary = .FALSE.
        ENDIF
        IF (DoOutputReporting .and. .not. PrintedWarmup) THEN
          WRITE (OutputFileStandard,EnvironmentStampFormat) '1', &
                    'Warmup {'//trim(cWarmupDay)//'} '//Trim(EnvironmentName),Latitude,Longitude,TimeZoneNumber,Elevation
          WRITE (OutputFileMeters,EnvironmentStampFormat) '1', &
                    'Warmup {'//trim(cWarmupDay)//'} '//Trim(EnvironmentName),Latitude,Longitude,TimeZoneNumber,Elevation
          PrintEnvrnStampWarmup=.FALSE.
        END IF
        PrintedWarmup=.true.
      END IF
      CALL CalcMoreNodeInfo
      CALL UpdateDataandReport(HVACTSReporting)
    ELSEIF (UpdateDataDuringWarmupExternalInterface) THEN ! added for FMI
      IF (BeginDayFlag .and. .not. PrintEnvrnStampWarmupPrinted) THEN
        PrintEnvrnStampWarmup=.true.
        PrintEnvrnStampWarmupPrinted=.true.
      ENDIF
      IF (.not. BeginDayFlag) PrintEnvrnStampWarmupPrinted=.false.
      IF (PrintEnvrnStampWarmup) THEN
        IF (PrintEndDataDictionary .AND. DoOutputReporting .and. .not. PrintedWarmup) THEN
          WRITE (OutputFileStandard,EndOfHeaderFormat)
          WRITE (OutputFileMeters,EndOfHeaderFormat)
          PrintEndDataDictionary = .FALSE.
        ENDIF
        IF (DoOutputReporting .and. .not. PrintedWarmup) THEN
          WRITE (OutputFileStandard,EnvironmentStampFormat) '1', &
                    'Warmup {'//trim(cWarmupDay)//'} '//Trim(EnvironmentName),Latitude,Longitude,TimeZoneNumber,Elevation
          WRITE (OutputFileMeters,EnvironmentStampFormat) '1', &
                    'Warmup {'//trim(cWarmupDay)//'} '//Trim(EnvironmentName),Latitude,Longitude,TimeZoneNumber,Elevation
          PrintEnvrnStampWarmup=.FALSE.
        END IF
        PrintedWarmup=.true.
      ENDIF
      CALL UpdateDataandReport(HVACTSReporting)
    END IF
    CALL ManageEMS(emsCallFromEndSystemTimestepAfterHVACReporting) ! EMS calling point
   !UPDATE SYSTEM CLOCKS
    SysTimeElapsed = SysTimeElapsed + TimeStepSys

    FirstTimeStepSysFlag = .FALSE.
  END DO !system time step  loop (loops once if no downstepping)

  CALL ManageZoneAirUpdates(iPushZoneTimeStepHistories,ZoneTempChange,ShortenTimeStepSys, &
                            UseZoneTimeStepHistory, PriorTimeStep )
  If (Contaminant%SimulateContaminants) &
    CALL ManageZoneContaminanUpdates(iPushZoneTimeStepHistories,ShortenTimeStepSys,UseZoneTimeStepHistory,PriorTimeStep)

  NumOfSysTimeStepsLastZoneTimeStep = NumOfSysTimeSteps

  CALL UpdateDemandManagers

  ! DO FINAL UPDATE OF RECORD KEEPING VARIABLES
  ! Report the Node Data to Aid in Debugging
  IF (DebugOutput) THEN
   IF (EvenDuringWarmup) THEN
     ReportDebug = .TRUE.
   ELSE
     ReportDebug = .NOT. WarmupFlag
   END IF
   IF ((ReportDebug).AND.(DayOfSim.GT.0)) THEN  ! Report the node data
    IF (SIZE(node) > 0 .and. .not. DebugNamesReported) THEN
      WRITE(OutputFileDebug,11)
      DO NodeNum = 1, SIZE(Node)
        WRITE(OutputFileDebug,30) NodeNum,trim(NodeID(NodeNum))
      ENDDO
      DebugNamesReported=.true.
    ENDIF
    IF (SIZE(node) > 0) THEN
      Write(OutputFileDebug,*)
      Write(OutputFileDebug,*)
      Write(OutputFileDebug,*)  'Day of Sim     Hour of Day    Time'
      Write(OutputFileDebug,*)  DayofSim, HourOfDay, TimeStep*TimeStepZone
      Write(OutputFileDebug,10)
    END IF
    DO NodeNum = 1, SIZE(Node)
      WRITE(OutputFileDebug,20) NodeNum,Node(NodeNum)%Temp,Node(NodeNum)%MassFlowRateMinAvail,   &
        Node(NodeNum)%MassFlowRateMaxAvail,Node(NodeNum)%TempSetPoint, &
        Node(NodeNum)%MassFlowRate,Node(NodeNum)%MassFlowRateMin,Node(NodeNum)%MassFlowRateMax,  &
        Node(NodeNum)%MassFlowRateSetPoint,Node(NodeNum)%Press,Node(NodeNum)%Enthalpy,Node(NodeNum)%HumRat,  &
        TRIM(ValidNodeFluidTypes(Node(NodeNum)%FluidType))
    END DO
   END IF
  END IF

10 FORMAT('node #   Temp   MassMinAv  MassMaxAv TempSP      MassFlow       MassMin       ',  &
          'MassMax        MassSP    Press        Enthal     HumRat Fluid Type')
11 FORMAT('node #   Name')
20 FORMAT(1x,I3,1x,F8.2,2(2x,F8.3),2x,F8.2,4(1x,F13.2),2x,F8.0,2x,F11.2,2x,F9.5,2x,A)
30 FORMAT(1x,I3,5x,A)

  RETURN

END SUBROUTINE ManageHVAC


SUBROUTINE SimHVAC

          ! SUBROUTINE INFORMATION:
          !       AUTHOR:          Dan Fisher
          !       DATE WRITTEN:    April 1997
          !       DATE MODIFIED:   May 1998 (RKS,RDT)

          ! PURPOSE OF THIS SUBROUTINE: Selects and calls the HVAC loop managers

          ! METHODOLOGY EMPLOYED: Each loop manager is called or passed over
          ! in succession based on the logical flags associated with the manager.
          ! The logical flags are set in the manager routines and passed
          ! as parameters to this routine.  Each loop manager potentially
          ! affects a different set of other loop managers.

          ! Future development could involve specifying any number of user
          ! selectable control schemes based on the logical flags used in
          ! this default control algorithm.

          ! REFERENCES: none

          ! USE STATEMENTS:
  USE DataConvergParams
  USE SetPointManager,                 ONLY : ManageSetPoints
  USE SystemAvailabilityManager,       ONLY : ManageSystemAvailability
  USE ZoneEquipmentManager,            ONLY : ManageZoneEquipment
  USE NonZoneEquipmentManager,         ONLY : ManageNonZoneEquipment
  USE ManageElectricPower,             ONLY : ManageElectricLoadCenters
  USE DataEnvironment,                 ONLY : EnvironmentName,CurMnDy
  USE General,                         ONLY : CreateSysTimeIntervalString,RoundSigDigits
  USE EMSManager,                      ONLY : ManageEMS
  USE PlantManager,                    ONLY : GetPlantLoopData,GetPlantInput,SetupReports, &
                                              ManagePlantLoops, SetupInitialPlantCallingOrder , SetupBranchControlTypes , &
                                              ReInitPlantLoopsAtFirstHVACIteration, InitOneTimePlantSizingInfo
  USE PlantCondLoopOperation,          ONLY:  SetupPlantEMSActuators
  USE SimAirServingZones,              ONLY : ManageAirLoops
  USE DataPlant,                       ONLY : SetAllPlantSimFlagsToValue, TotNumLoops, &
                                              PlantManageSubIterations, PlantManageHalfLoopCalls, &
                                              DemandSide, SupplySide, PlantLoop, NumConvergenceHistoryTerms, &
                                              ConvergenceHistoryARR
  USE PlantUtilities,                  ONLY : CheckPlantMixerSplitterConsistency, &
                                              CheckForRunawayPlantTemps, AnyPlantSplitterMixerLacksContinuity
  USE DataGlobals,                     ONLY : AnyPlantInModel

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
  LOGICAL, PARAMETER :: IsPlantLoop = .TRUE.
  LOGICAL, PARAMETER :: NotPlantLoop = .FALSE.
  LOGICAL, PARAMETER :: SimWithPlantFlowUnlocked = .FALSE.
  LOGICAL, PARAMETER :: SimWithPlantFlowLocked   = .TRUE.

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  LOGICAL :: FirstHVACIteration     ! True when solution technique on first iteration

  LOGICAL, SAVE :: IterSetup = .false. ! Set to TRUE after the variable is setup for Output Reporting
  INTEGER, SAVE :: ErrCount=0          ! Number of times that the maximum iterations was exceeded
  LOGICAL, SAVE :: MySetPointInit = .TRUE.
  CHARACTER(len=10) :: CharErrOut       ! a character string equivalent of ErrCount
  INTEGER :: que
  INTEGER, SAVE :: MaxErrCount=0
  CHARACTER(len=MaxNameLength*2),SAVE :: ErrEnvironmentName=' '
  INTEGER  :: LoopNum
  INTEGER  :: LoopSide
  INTEGER  :: ThisLoopSide

  INTEGER  :: AirSysNum
  INTEGER  :: StackDepth
  CHARACTER(len=10) :: HistoryStack
  CHARACTER(len=500) :: HistoryTrace
  INTEGER   :: ZoneInSysIndex
  REAL(r64) :: SlopeHumRat
  REAL(r64) :: SlopeMdot
  REAL(r64) :: SlopeTemps
  REAL(r64) :: AvgValue
  LOGICAL   :: FoundOscillationByDuplicate
  INTEGER   :: ZoneNum
  INTEGER   :: NodeIndex
  LOGICAL   :: MonotonicIncreaseFound
  LOGICAL   :: MonotonicDecreaseFound

          ! Initialize all of the simulation flags to true for the first iteration
  SimZoneEquipmentFlag     = .TRUE.
  SimNonZoneEquipmentFlag  = .TRUE.
  SimAirLoopsFlag          = .TRUE.
  SimPlantLoopsFlag        = .TRUE.
  SimElecCircuitsFlag      = .TRUE.
  FirstHVACIteration       = .TRUE.

  IF(AirLoopInputsFilled)THEN
    ! Reset air loop control info for cooling coil active flag (used in TU's for reheat air flow control)
    AirLoopControlInfo%CoolingActiveFlag = .FALSE.
    ! Reset air loop control info for heating coil active flag (used in OA controller for HX control)
    AirLoopControlInfo%HeatingActiveFlag = .FALSE.
    ! reset outside air system HX to off first time through
    AirLoopControlInfo%HeatRecoveryBypass = .TRUE.
    ! set HX check status flag to check for custom control in MixedAir.f90
    AirLoopControlInfo%CheckHeatRecoveryBypassStatus = .TRUE.
    ! set OA comp simulated flag to false
    AirLoopControlInfo%OASysComponentsSimulated = .FALSE.
    ! set economizer flow locked flag to false, will reset if custom HX control is used
    AirLoopControlInfo%EconomizerFlowLocked = .FALSE.
    ! set air loop resim flags for when heat recovery is used and air loop needs another iteration
    AirLoopControlInfo%HeatRecoveryResimFlag = .TRUE.
    AirLoopControlInfo%HeatRecoveryResimFlag2 = .FALSE.
    AirLoopControlInfo%ResimAirLoopFlag = .FALSE.
  END IF

  ! This setups the reports for the Iteration variable that limits how many times
  !  it goes through all of the HVAC managers before moving on.
  ! The plant loop 'get inputs' and initialization are also done here in order to allow plant loop connected components
  ! simulated by managers other than the plant manager to run correctly.
  HVACManageIteration = 0
  PLANTManageSubIterations = 0
  PlantManageHalfLoopCalls = 0
  CALL SetAllPlantSimFlagsToValue(.TRUE.)
  IF (.not. IterSetup) THEN
    CALL SetupOutputVariable('HVAC System Solver Iteration Count []',HVACManageIteration,'HVAC','Sum','SimHVAC')
    CALL SetupOutputVariable('Air System Solver Iteration Count []',RepIterAir,'HVAC','Sum','SimHVAC')
    CALL ManageSetPoints  !need to call this before getting plant loop data so setpoint checks can complete okay
    CALL GetPlantLoopData
    CALL GetPlantInput
    CALL SetupInitialPlantCallingOrder
    CALL SetupBranchControlTypes  !new routine to do away with input for branch control type
!    CALL CheckPlantLoopData
    CALL SetupReports
    IF (AnyEnergyManagementSystemInModel) THEN
      CALL SetupPlantEMSActuators
    ENDIF

    IF (TotNumLoops > 0) THEN
      CALL SetupOutputVariable('Plant Solver Sub Iteration Count []',PLANTManageSubIterations,'HVAC','Sum','SimHVAC')
      CALL SetupOutputVariable('Plant Solver Half Loop Calls Count []',PlantManageHalfLoopCalls,'HVAC','Sum','SimHVAC')
      DO LoopNum = 1, TotNumLoops
        ! init plant sizing numbers in main plant data structure
        CALL InitOneTimePlantSizingInfo(LoopNum)
      ENDDO
    ENDIF
    IterSetup=.true.
  ENDIF


  IF (ZoneSizingCalc) THEN
    CALL ManageZoneEquipment(FirstHVACIteration,SimZoneEquipmentFlag,SimAirLoopsFlag)
    ! need to call non zone equipment so water use zone gains can be included in sizing calcs
    CALL ManageNonZoneEquipment(FirstHVACIteration,SimNonZoneEquipmentFlag)
    CALL ManageElectricLoadCenters(FirstHVACIteration,SimElecCircuitsFlag, .FALSE.)
    RETURN
  END IF

          ! Before the HVAC simulation, reset control flags and specified flow
          ! rates that might have been set by the set point and availability
          ! managers.

  CALL ResetHVACControl



          ! Before the HVAC simulation, call ManageSetPoints to set all the HVAC
          ! node setpoints

  CAll ManageEMS(emsCallFromBeforeHVACManagers) ! calling point

  CALL ManageSetPoints

          ! re-initialize plant loop and nodes.
  CALL ReInitPlantLoopsAtFirstHVACIteration

          ! Before the HVAC simulation, call ManageSystemAvailability to set
          ! the system on/off flags
  CALL ManageSystemAvailability

  CAll ManageEMS(emsCallFromAfterHVACManagers)! calling point

! first explicitly call each system type with FirstHVACIteration,


          ! Manages the various component simulations
  CALL SimSelectedEquipment(SimAirLoopsFlag,SimZoneEquipmentFlag,SimNonZoneEquipmentFlag,SimPlantLoopsFlag,&
                              SimElecCircuitsFlag,     FirstHVACIteration, SimWithPlantFlowUnlocked)

          ! Eventually, when all of the flags are set to false, the
          ! simulation has converged for this system time step.

  SimPlantLoopsFlag = .TRUE.
  CALL SetAllPlantSimFlagsToValue(.TRUE.) !set so loop to simulate at least once on non-first hvac

  FirstHVACIteration = .FALSE.


! then iterate among all systems after first HVAC iteration is over

          ! Main iteration loop for HVAC.  If any of the simulation flags are
          ! true, then specific components must be resimulated.
  DO WHILE ( (SimAirLoopsFlag .OR. SimZoneEquipmentFlag .OR. SimNonZoneEquipmentFlag .OR. SimPlantLoopsFlag .OR. &
              SimElecCircuitsFlag )  .AND. (HVACManageIteration.LE.MaxIter) )

    CAll ManageEMS(emsCallFromHVACIterationLoop) ! calling point id

          ! Manages the various component simulations
    CALL SimSelectedEquipment(SimAirLoopsFlag,SimZoneEquipmentFlag,SimNonZoneEquipmentFlag,SimPlantLoopsFlag,&
                              SimElecCircuitsFlag, FirstHVACIteration, SimWithPlantFlowUnlocked)

          ! Eventually, when all of the flags are set to false, the
          ! simulation has converged for this system time step.

    CALL UpdateZoneInletConvergenceLog

    HVACManageIteration = HVACManageIteration + 1   ! Increment the iteration counter

  END DO
  IF (AnyPlantInModel) THEN
    If (AnyPlantSplitterMixerLacksContinuity()) THEN
      ! rerun systems in a "Final flow lock/last iteration" mode
      ! now call for one second to last plant simulation
      SimAirLoopsFlag = .FALSE.
      SimZoneEquipmentFlag = .FALSE.
      SimNonZoneEquipmentFlag = .FALSE.
      SimPlantLoopsFlag = .TRUE.
      SimElecCircuitsFlag = .FALSE.
      CALL SimSelectedEquipment(SimAirLoopsFlag,SimZoneEquipmentFlag,SimNonZoneEquipmentFlag,SimPlantLoopsFlag,&
                                  SimElecCircuitsFlag, FirstHVACIteration, SimWithPlantFlowUnlocked)
      ! now call for all non-plant simulation, but with plant flow lock on
      SimAirLoopsFlag = .TRUE.
      SimZoneEquipmentFlag = .TRUE.
      SimNonZoneEquipmentFlag = .TRUE.
      SimPlantLoopsFlag = .FALSE.
      SimElecCircuitsFlag = .TRUE.
      CALL SimSelectedEquipment(SimAirLoopsFlag,SimZoneEquipmentFlag,SimNonZoneEquipmentFlag,SimPlantLoopsFlag,&
                                  SimElecCircuitsFlag, FirstHVACIteration, SimWithPlantFlowLocked)
      CALL UpdateZoneInletConvergenceLog
      ! now call for a last plant simulation
      SimAirLoopsFlag = .FALSE.
      SimZoneEquipmentFlag = .FALSE.
      SimNonZoneEquipmentFlag = .FALSE.
      SimPlantLoopsFlag = .TRUE.
      SimElecCircuitsFlag = .FALSE.
      CALL SimSelectedEquipment(SimAirLoopsFlag,SimZoneEquipmentFlag,SimNonZoneEquipmentFlag,SimPlantLoopsFlag,&
                                  SimElecCircuitsFlag, FirstHVACIteration, SimWithPlantFlowUnlocked)
      ! now call for a last all non-plant simulation, but with plant flow lock on
      SimAirLoopsFlag = .TRUE.
      SimZoneEquipmentFlag = .TRUE.
      SimNonZoneEquipmentFlag = .TRUE.
      SimPlantLoopsFlag = .FALSE.
      SimElecCircuitsFlag = .TRUE.
      CALL SimSelectedEquipment(SimAirLoopsFlag,SimZoneEquipmentFlag,SimNonZoneEquipmentFlag,SimPlantLoopsFlag,&
                                  SimElecCircuitsFlag, FirstHVACIteration, SimWithPlantFlowLocked)
      CALL UpdateZoneInletConvergenceLog
    ENDIF
  ENDIF

  !DSU  Test plant loop for errors
  DO LoopNum = 1, TotNumLoops
    DO LoopSide = DemandSide,SupplySide
      CALL CheckPlantMixerSplitterConsistency(LoopNum,LoopSide,1, 1,FirstHVACIteration)
      Call CheckForRunawayPlantTemps(LoopNum,LoopSide)
    ENDDO
  ENDDO


  IF ((HVACManageIteration > MaxIter).AND.(.NOT.WarmUpFlag)) THEN
    ErrCount = ErrCount + 1
    IF (ErrCount < 15) THEN
      ErrEnvironmentName=EnvironmentName
      WRITE(CharErrOut,'(I5)') MaxIter
      CharErrOut=ADJUSTL(CharErrOut)
      CALL ShowWarningError ('SimHVAC: Maximum iterations ('//TRIM(CharErrOut)//') exceeded for all HVAC loops, at '//  &
                              TRIM(EnvironmentName)//', '//TRIM(CurMnDy)//' '//TRIM(CreateSysTimeIntervalString()))
      IF (SimAirLoopsFlag) THEN
        CALL ShowContinueError('The solution for one or more of the Air Loop HVAC systems did not appear to converge')
      ENDIF
      IF (SimZoneEquipmentFlag) THEN
        CALL ShowContinueError('The solution for zone HVAC equipment did not appear to converge')
      ENDIF
      IF (SimNonZoneEquipmentFlag) THEN
        CALL ShowContinueError('The solution for non-zone equipment did not appear to converge')
      ENDIF
      IF (SimPlantLoopsFlag) THEN
        CALL ShowContinueError('The solution for one or more plant systems did not appear to converge')
      ENDIF
      IF (SimElecCircuitsFlag) THEN
        CALL ShowContinueError('The solution for on-site electric generators did not appear to converge')
      ENDIF
      IF (ErrCount == 1 .and. .not. DisplayExtraWarnings) THEN
        CALL ShowContinueError('...use Output:Diagnostics,DisplayExtraWarnings; '// &
        '  to show more details on each max iteration exceeded.')
      ENDIF
      IF (DisplayExtraWarnings) THEN

        DO AirSysNum = 1, NumPrimaryAirSys

          IF (AirLoopConvergence(AirSysNum)%HVACMassFlowNotConverged) THEN

            CALL ShowContinueError('Air System Named = '//TRIM(AirToZoneNodeInfo(AirSysNum)%AirLoopName)  &
                                   // ' did not converge for mass flow rate')
            CALL ShowContinueError('Check values should be zero. Most Recent values listed first.')
            HistoryTrace = ''
            DO StackDepth = 1, ConvergLogStackDepth
              HistoryTrace = TRIM(HistoryTrace) &
                  // TRIM(roundSigDigits(AirLoopConvergence(AirSysNum)%HVACFlowDemandToSupplyTolValue(StackDepth), 6)) &
                  // ','
            ENDDO

            CALL ShowContinueError('Demand-to-Supply interface mass flow rate check value iteration history trace: ' &
                   //TRIM(HistoryTrace) )
            HistoryTrace = ''
            DO StackDepth = 1, ConvergLogStackDepth
              HistoryTrace = TRIM(HistoryTrace) &
                  // TRIM(roundSigDigits(AirLoopConvergence(AirSysNum)%HVACFlowSupplyDeck1ToDemandTolValue(StackDepth), 6)) &
                  // ','
            ENDDO
            CALL ShowContinueError('Supply-to-demand interface deck 1 mass flow rate check value iteration history trace: ' &
                   //TRIM(HistoryTrace) )

            IF (AirToZoneNodeInfo(AirSysNum)%NumSupplyNodes >= 2) THEN
              HistoryTrace = ''
              DO StackDepth = 1, ConvergLogStackDepth
                HistoryTrace = TRIM(HistoryTrace) &
                    // TRIM(roundSigDigits(AirLoopConvergence(AirSysNum)%HVACFlowSupplyDeck2ToDemandTolValue(StackDepth), 6)) &
                    // ','
              ENDDO
              CALL ShowContinueError('Supply-to-demand interface deck 2 mass flow rate check value iteration history trace: ' &
                   //TRIM(HistoryTrace) )
            ENDIF
          ENDIF ! mass flow rate not converged

          IF (AirLoopConvergence(AirSysNum)%HVACHumRatNotConverged) THEN

            CALL ShowContinueError('Air System Named = '//TRIM(AirToZoneNodeInfo(AirSysNum)%AirLoopName)  &
                                   // ' did not converge for humidity ratio')
            CALL ShowContinueError('Check values should be zero. Most Recent values listed first.')
            HistoryTrace = ''
            DO StackDepth = 1, ConvergLogStackDepth
              HistoryTrace = TRIM(HistoryTrace) &
                  // TRIM(roundSigDigits(AirLoopConvergence(AirSysNum)%HVACHumDemandToSupplyTolValue(StackDepth), 6)) &
                  // ','
            ENDDO
            CALL ShowContinueError('Demand-to-Supply interface humidity ratio check value iteration history trace: ' &
                   //TRIM(HistoryTrace) )
            HistoryTrace = ''
            DO StackDepth = 1, ConvergLogStackDepth
              HistoryTrace = TRIM(HistoryTrace) &
                  // TRIM(roundSigDigits(AirLoopConvergence(AirSysNum)%HVACHumSupplyDeck1ToDemandTolValue(StackDepth), 6)) &
                  // ','
            ENDDO
            CALL ShowContinueError('Supply-to-demand interface deck 1 humidity ratio check value iteration history trace: ' &
                   //TRIM(HistoryTrace) )

            IF (AirToZoneNodeInfo(AirSysNum)%NumSupplyNodes >= 2) THEN
              HistoryTrace = ''
              DO StackDepth = 1, ConvergLogStackDepth
                HistoryTrace = TRIM(HistoryTrace) &
                    // TRIM(roundSigDigits(AirLoopConvergence(AirSysNum)%HVACHumSupplyDeck2ToDemandTolValue(StackDepth), 6)) &
                    // ','
              ENDDO
              CALL ShowContinueError('Supply-to-demand interface deck 2 humidity ratio check value iteration history trace: ' &
                   //TRIM(HistoryTrace) )
            ENDIF
          ENDIF ! humidity ratio not converged

          IF (AirLoopConvergence(AirSysNum)%HVACTempNotConverged) THEN

            CALL ShowContinueError('Air System Named = '//TRIM(AirToZoneNodeInfo(AirSysNum)%AirLoopName)  &
                                   // ' did not converge for temperature')
            CALL ShowContinueError('Check values should be zero. Most Recent values listed first.')
            HistoryTrace = ''
            DO StackDepth = 1, ConvergLogStackDepth
              HistoryTrace = TRIM(HistoryTrace) &
                  // TRIM(roundSigDigits(AirLoopConvergence(AirSysNum)%HVACTempDemandToSupplyTolValue(StackDepth), 6)) &
                  // ','
            ENDDO
            CALL ShowContinueError('Demand-to-Supply interface temperature check value iteration history trace: ' &
                   //TRIM(HistoryTrace) )
            HistoryTrace = ''
            DO StackDepth = 1, ConvergLogStackDepth
              HistoryTrace = TRIM(HistoryTrace) &
                  // TRIM(roundSigDigits(AirLoopConvergence(AirSysNum)%HVACTempSupplyDeck1ToDemandTolValue(StackDepth), 6)) &
                  // ','
            ENDDO
            CALL ShowContinueError('Supply-to-demand interface deck 1 temperature check value iteration history trace: ' &
                   //TRIM(HistoryTrace) )

            IF (AirToZoneNodeInfo(AirSysNum)%NumSupplyNodes >= 2) THEN
              HistoryTrace = ''
              DO StackDepth = 1, ConvergLogStackDepth
                HistoryTrace = TRIM(HistoryTrace) &
                    // TRIM(roundSigDigits(AirLoopConvergence(AirSysNum)%HVACTempSupplyDeck1ToDemandTolValue(StackDepth), 6)) &
                    // ','
              ENDDO
              CALL ShowContinueError('Supply-to-demand interface deck 2 temperature check value iteration history trace: ' &
                   //TRIM(HistoryTrace) )
            ENDIF
          ENDIF ! Temps not converged
          IF (AirLoopConvergence(AirSysNum)%HVACEnergyNotConverged) THEN

            CALL ShowContinueError('Air System Named = '//TRIM(AirToZoneNodeInfo(AirSysNum)%AirLoopName)  &
                                   // ' did not converge for energy')
            CALL ShowContinueError('Check values should be zero. Most Recent values listed first.')
            HistoryTrace = ''
            DO StackDepth = 1, ConvergLogStackDepth
              HistoryTrace = TRIM(HistoryTrace) &
                  // TRIM(roundSigDigits(AirLoopConvergence(AirSysNum)%HVACEnergyDemandToSupplyTolValue(StackDepth), 6)) &
                  // ','
            ENDDO
            CALL ShowContinueError('Demand-to-Supply interface energy check value iteration history trace: ' &
                   //TRIM(HistoryTrace) )
            HistoryTrace = ''
            DO StackDepth = 1, ConvergLogStackDepth
              HistoryTrace = TRIM(HistoryTrace) &
                  // TRIM(roundSigDigits(AirLoopConvergence(AirSysNum)%HVACEnergySupplyDeck1ToDemandTolValue(StackDepth), 6)) &
                  // ','
            ENDDO
            CALL ShowContinueError('Supply-to-demand interface deck 1 energy check value iteration history trace: ' &
                   //TRIM(HistoryTrace) )

            IF (AirToZoneNodeInfo(AirSysNum)%NumSupplyNodes >= 2) THEN
              HistoryTrace = ''
              DO StackDepth = 1, ConvergLogStackDepth
                HistoryTrace = TRIM(HistoryTrace) &
                    // TRIM(roundSigDigits(AirLoopConvergence(AirSysNum)%HVACEnergySupplyDeck2ToDemandTolValue(StackDepth), 6)) &
                    // ','
              ENDDO
              CALL ShowContinueError('Supply-to-demand interface deck 2 energy check value iteration history trace: ' &
                   //TRIM(HistoryTrace) )
            ENDIF
          ENDIF ! energy not converged

        ENDDO  ! loop over air loop systems

        ! loop over zones and check for issues with zone inlet nodes
        DO ZoneNum = 1, NumOfZones

          DO NodeIndex = 1, ZoneInletConvergence(ZoneNum)%NumInletNodes
            ZoneInletConvergence(ZoneNum)%InletNode(NodeIndex)%NotConvergedHumRate  = .FALSE.
            ZoneInletConvergence(ZoneNum)%InletNode(NodeIndex)%NotConvergedMassFlow = .FALSE.
            ZoneInletConvergence(ZoneNum)%InletNode(NodeIndex)%NotConvergedTemp     = .FALSE.

            ! Check humidity ratio
            FoundOscillationByDuplicate = .FALSE.
            MonotonicDecreaseFound = .FALSE.
            MonotonicIncreaseFound = .FALSE.
                        ! check for evidence of oscillation by indentify duplicates when latest value not equal to average
            AvgValue = SUM(ZoneInletConvergence(ZoneNum)%InletNode(NodeIndex)%HumidityRatio) &
                         / REAL(ConvergLogStackDepth, r64)
            IF (ABS(ZoneInletConvergence(ZoneNum)%InletNode(NodeIndex)%HumidityRatio(1) - AvgValue) &
                > HVACHumRatOscillationToler ) THEN ! last iterate differs from average
              FoundOscillationByDuplicate = .FALSE.
              DO StackDepth = 2, ConvergLogStackDepth
                IF (ABS(ZoneInletConvergence(ZoneNum)%InletNode(NodeIndex)%HumidityRatio(1) - &
                   ZoneInletConvergence(ZoneNum)%InletNode(NodeIndex)%HumidityRatio(StackDepth)) &
                    < HVACHumRatOscillationToler) THEN
                  FoundOscillationByDuplicate = .TRUE.
                  ZoneInletConvergence(ZoneNum)%InletNode(NodeIndex)%NotConvergedHumRate = .TRUE.
                  CALL ShowContinueError('Node named ' &
                           //TRIM(NodeID(ZoneInletConvergence(ZoneNum)%InletNode(NodeIndex)%NodeNum)) &
                           //' shows oscillating humidity ratio across iterations with a repeated value of ' &
                           //TRIM(RoundSigDigits(  &
                              ZoneInletConvergence(ZoneNum)%InletNode(NodeIndex)%HumidityRatio(1), 6) ) )
                  EXIT
                ENDIF
              ENDDO
              IF (.NOT. FoundOscillationByDuplicate) THEN
                SlopeHumRat = ( SUM( ConvergLogStackARR) &
                            *SUM(ZoneInletConvergence(ZoneNum)%InletNode(NodeIndex)%HumidityRatio)&
                           - REAL(ConvergLogStackDepth, r64) * SUM((ConvergLogStackARR &
                                 * ZoneInletConvergence(ZoneNum)%InletNode(NodeIndex)%HumidityRatio)) ) &
                            / ( SUM(ConvergLogStackARR)**2 &
                             -  REAL(ConvergLogStackDepth, r64)*SUM( ConvergLogStackARR**2) )
                IF (ABS(SlopeHumRat) > HVACHumRatSlopeToler) THEN

                  IF (SlopeHumRat < 0.d0) THEN  ! check for monotic decrease
                    MonotonicDecreaseFound = .TRUE.
                    DO StackDepth = 2, ConvergLogStackDepth
                      IF (ZoneInletConvergence(ZoneNum)%InletNode(NodeIndex)%HumidityRatio(StackDepth-1) > &
                          ZoneInletConvergence(ZoneNum)%InletNode(NodeIndex)%HumidityRatio(StackDepth)) THEN
                        MonotonicDecreaseFound = .FALSE.
                        EXIT
                      ENDIF
                    ENDDO
                    IF (MonotonicDecreaseFound) THEN
                      CALL ShowContinueError('Node named ' &
                           //TRIM(NodeID(ZoneInletConvergence(ZoneNum)%InletNode(NodeIndex)%NodeNum)) &
                           //' shows monotonically decreasing humidity ratio with a trend rate across iterations of ' &
                           //TRIM(RoundSigDigits(SlopeHumRat, 6)) &
                           //' [ kg-water/kg-dryair/iteration]' )
                    ENDIF
                  ELSE  ! check for monotic incrase
                    MonotonicIncreaseFound = .TRUE.
                    DO StackDepth = 2, ConvergLogStackDepth
                      IF (ZoneInletConvergence(ZoneNum)%InletNode(NodeIndex)%HumidityRatio(StackDepth-1) < &
                          ZoneInletConvergence(ZoneNum)%InletNode(NodeIndex)%HumidityRatio(StackDepth)) THEN
                        MonotonicIncreaseFound = .FALSE.
                        EXIT
                      ENDIF
                    ENDDO
                    IF (MonotonicIncreaseFound) THEN
                      CALL ShowContinueError('Node named ' &
                           //TRIM(NodeID(ZoneInletConvergence(ZoneNum)%InletNode(NodeIndex)%NodeNum)) &
                           //' shows monotonically increasing humidity ratio with a trend rate across iterations of ' &
                           //TRIM(RoundSigDigits(SlopeHumRat, 6)) &
                           //' [ kg-water/kg-dryair/iteration]' )
                    ENDIF
                  ENDIF
                ENDIF ! significant slope in iterates
              ENDIF !no osciallation
            ENDIF ! last value does not equal average of stack.

            IF (MonotonicDecreaseFound .OR. MonotonicIncreaseFound .OR. FoundOscillationByDuplicate) THEN
              HistoryTrace = ' '
              DO StackDepth = 1, ConvergLogStackDepth
                HistoryTrace = TRIM(HistoryTrace) &
                // TRIM(roundSigDigits(ZoneInletConvergence(ZoneNum)%InletNode(NodeIndex) &
                                        %HumidityRatio(StackDepth), 6) ) // ','
              ENDDO
              CALL ShowContinueError('Node named ' &
                           //TRIM(NodeID(ZoneInletConvergence(ZoneNum)%InletNode(NodeIndex)%NodeNum)) &
                           //' humidity ratio [kg-water/kg-dryair] iteration history trace (most recent first): ' &
                           //TRIM(HistoryTrace) )
            ENDIF ! need to report trace
            ! end humidity ratio

            ! Check Mass flow rate
            FoundOscillationByDuplicate = .FALSE.
            MonotonicDecreaseFound = .FALSE.
            MonotonicIncreaseFound = .FALSE.
                        ! check for evidence of oscillation by indentify duplicates when latest value not equal to average
            AvgValue = SUM(ZoneInletConvergence(ZoneNum)%InletNode(NodeIndex)%MassFlowRate) &
                         / REAL(ConvergLogStackDepth, r64)
            IF (ABS(ZoneInletConvergence(ZoneNum)%InletNode(NodeIndex)%MassFlowRate(1) - AvgValue) &
                > HVACFlowRateOscillationToler ) THEN ! last iterate differs from average
              FoundOscillationByDuplicate = .FALSE.
              DO StackDepth = 2, ConvergLogStackDepth
                IF (ABS(ZoneInletConvergence(ZoneNum)%InletNode(NodeIndex)%MassFlowRate(1) - &
                   ZoneInletConvergence(ZoneNum)%InletNode(NodeIndex)%MassFlowRate(StackDepth)) &
                    < HVACFlowRateOscillationToler) THEN
                  FoundOscillationByDuplicate = .TRUE.
                  ZoneInletConvergence(ZoneNum)%InletNode(NodeIndex)%NotConvergedMassFlow = .TRUE.
                  CALL ShowContinueError('Node named ' &
                           //TRIM(NodeID(ZoneInletConvergence(ZoneNum)%InletNode(NodeIndex)%NodeNum)) &
                           //' shows oscillating mass flow rate across iterations with a repeated value of ' &
                           //TRIM(RoundSigDigits(  &
                              ZoneInletConvergence(ZoneNum)%InletNode(NodeIndex)%MassFlowRate(1), 6) ) )
                  EXIT
                ENDIF
              ENDDO
              IF (.NOT. FoundOscillationByDuplicate) THEN
                SlopeMdot = ( SUM( ConvergLogStackARR) &
                            *SUM(ZoneInletConvergence(ZoneNum)%InletNode(NodeIndex)%MassFlowRate)&
                           - REAL(ConvergLogStackDepth, r64) * SUM((ConvergLogStackARR &
                                 * ZoneInletConvergence(ZoneNum)%InletNode(NodeIndex)%MassFlowRate)) ) &
                            / ( SUM(ConvergLogStackARR)**2 &
                             -  REAL(ConvergLogStackDepth, r64)*SUM( ConvergLogStackARR**2) )
                IF (ABS(SlopeMdot) > HVACFlowRateSlopeToler) THEN
                  ZoneInletConvergence(ZoneNum)%InletNode(NodeIndex)%NotConvergedMassFlow = .TRUE.
                  IF (SlopeMdot < 0.d0) THEN  ! check for monotic decrease
                    MonotonicDecreaseFound = .TRUE.
                    DO StackDepth = 2, ConvergLogStackDepth
                      IF (ZoneInletConvergence(ZoneNum)%InletNode(NodeIndex)%MassFlowRate(StackDepth-1) > &
                          ZoneInletConvergence(ZoneNum)%InletNode(NodeIndex)%MassFlowRate(StackDepth)) THEN
                        MonotonicDecreaseFound = .FALSE.
                        EXIT
                      ENDIF
                    ENDDO
                    IF (MonotonicDecreaseFound) THEN
                      CALL ShowContinueError('Node named ' &
                           //TRIM(NodeID(ZoneInletConvergence(ZoneNum)%InletNode(NodeIndex)%NodeNum)) &
                           //' shows monotonically decreasing mass flow rate with a trend rate across iterations of ' &
                           //TRIM(RoundSigDigits(SlopeMdot, 6)) &
                           //' [kg/s/iteration]' )
                    ENDIF
                  ELSE  ! check for monotic incrase
                    MonotonicIncreaseFound = .TRUE.
                    DO StackDepth = 2, ConvergLogStackDepth
                      IF (ZoneInletConvergence(ZoneNum)%InletNode(NodeIndex)%MassFlowRate(StackDepth-1) < &
                          ZoneInletConvergence(ZoneNum)%InletNode(NodeIndex)%MassFlowRate(StackDepth)) THEN
                        MonotonicIncreaseFound = .FALSE.
                        EXIT
                      ENDIF
                    ENDDO
                    IF (MonotonicIncreaseFound) THEN
                      CALL ShowContinueError('Node named ' &
                           //TRIM(NodeID(ZoneInletConvergence(ZoneNum)%InletNode(NodeIndex)%NodeNum)) &
                           //' shows monotonically increasing mass flow rate with a trend rate across iterations of ' &
                           //TRIM(RoundSigDigits(SlopeMdot, 6)) &
                           //' [kg/s/iteration]' )
                    ENDIF
                  ENDIF
                ENDIF ! significant slope in iterates
              ENDIF !no osciallation
            ENDIF ! last value does not equal average of stack.

            IF (MonotonicDecreaseFound .OR. MonotonicIncreaseFound .OR. FoundOscillationByDuplicate) THEN
              HistoryTrace = ' '
              DO StackDepth = 1, ConvergLogStackDepth
                HistoryTrace = TRIM(HistoryTrace) &
                // TRIM(roundSigDigits(ZoneInletConvergence(ZoneNum)%InletNode(NodeIndex) &
                                        %MassFlowRate(StackDepth), 6) ) // ','
              ENDDO
              CALL ShowContinueError('Node named ' &
                           //TRIM(NodeID(ZoneInletConvergence(ZoneNum)%InletNode(NodeIndex)%NodeNum)) &
                           //' mass flow rate [kg/s] iteration history trace (most recent first): ' &
                           //TRIM(HistoryTrace) )
            ENDIF ! need to report trace
            ! end mass flow rate

            ! Check Temperatures
            FoundOscillationByDuplicate = .FALSE.
            MonotonicDecreaseFound = .FALSE.
            MonotonicIncreaseFound = .FALSE.
                        ! check for evidence of oscillation by indentify duplicates when latest value not equal to average
            AvgValue = SUM(ZoneInletConvergence(ZoneNum)%InletNode(NodeIndex)%Temperature) &
                         / REAL(ConvergLogStackDepth, r64)
            IF (ABS(ZoneInletConvergence(ZoneNum)%InletNode(NodeIndex)%Temperature(1) - AvgValue) &
                > HVACTemperatureOscillationToler ) THEN ! last iterate differs from average
              FoundOscillationByDuplicate = .FALSE.
              DO StackDepth = 2, ConvergLogStackDepth
                IF (ABS(ZoneInletConvergence(ZoneNum)%InletNode(NodeIndex)%Temperature(1) - &
                   ZoneInletConvergence(ZoneNum)%InletNode(NodeIndex)%Temperature(StackDepth)) &
                    < HVACTemperatureOscillationToler) THEN
                  FoundOscillationByDuplicate = .TRUE.
                  ZoneInletConvergence(ZoneNum)%InletNode(NodeIndex)%NotConvergedTemp = .TRUE.
                  CALL ShowContinueError('Node named ' &
                           //TRIM(NodeID(ZoneInletConvergence(ZoneNum)%InletNode(NodeIndex)%NodeNum)) &
                           //' shows oscillating temperatures across iterations with a repeated value of ' &
                           //TRIM(RoundSigDigits(  &
                              ZoneInletConvergence(ZoneNum)%InletNode(NodeIndex)%Temperature(1), 6) ) )
                  EXIT
                ENDIF
              ENDDO
              IF (.NOT. FoundOscillationByDuplicate) THEN
                SlopeTemps = ( SUM( ConvergLogStackARR) &
                            *SUM(ZoneInletConvergence(ZoneNum)%InletNode(NodeIndex)%Temperature)&
                           - REAL(ConvergLogStackDepth, r64) * SUM((ConvergLogStackARR &
                                 * ZoneInletConvergence(ZoneNum)%InletNode(NodeIndex)%Temperature)) ) &
                            / ( SUM(ConvergLogStackARR)**2 &
                             -  REAL(ConvergLogStackDepth, r64)*SUM( ConvergLogStackARR**2) )
                IF (ABS(SlopeTemps) > HVACTemperatureSlopeToler) THEN
                  ZoneInletConvergence(ZoneNum)%InletNode(NodeIndex)%NotConvergedTemp = .TRUE.
                  IF (SlopeTemps < 0.d0) THEN  ! check for monotic decrease
                    MonotonicDecreaseFound = .TRUE.
                    DO StackDepth = 2, ConvergLogStackDepth
                      IF (ZoneInletConvergence(ZoneNum)%InletNode(NodeIndex)%Temperature(StackDepth-1) > &
                          ZoneInletConvergence(ZoneNum)%InletNode(NodeIndex)%Temperature(StackDepth)) THEN
                        MonotonicDecreaseFound = .FALSE.
                        EXIT
                      ENDIF
                    ENDDO
                    IF (MonotonicDecreaseFound) THEN
                      CALL ShowContinueError('Node named ' &
                           //TRIM(NodeID(ZoneInletConvergence(ZoneNum)%InletNode(NodeIndex)%NodeNum)) &
                           //' shows monotonically decreasing temperature with a trend rate across iterations of ' &
                           //TRIM(RoundSigDigits(SlopeTemps, 4)) &
                           //' [C/iteration]' )
                    ENDIF
                  ELSE  ! check for monotic incrase
                    MonotonicIncreaseFound = .TRUE.
                    DO StackDepth = 2, ConvergLogStackDepth
                      IF (ZoneInletConvergence(ZoneNum)%InletNode(NodeIndex)%Temperature(StackDepth-1) < &
                          ZoneInletConvergence(ZoneNum)%InletNode(NodeIndex)%Temperature(StackDepth)) THEN
                        MonotonicIncreaseFound = .FALSE.
                        EXIT
                      ENDIF
                    ENDDO
                    IF (MonotonicIncreaseFound) THEN
                      CALL ShowContinueError('Node named ' &
                           //TRIM(NodeID(ZoneInletConvergence(ZoneNum)%InletNode(NodeIndex)%NodeNum)) &
                           //' shows monotonically increasing temperatures with a trend rate across iterations of ' &
                           //TRIM(RoundSigDigits(SlopeTemps, 4)) &
                           //' [C/iteration]' )
                    ENDIF
                  ENDIF
                ENDIF ! significant slope in iterates
              ENDIF !no osciallation
            ENDIF ! last value does not equal average of stack.

            IF (MonotonicDecreaseFound .OR. MonotonicIncreaseFound .OR. FoundOscillationByDuplicate) THEN
              HistoryTrace = ' '
              DO StackDepth = 1, ConvergLogStackDepth
                HistoryTrace = TRIM(HistoryTrace) &
                // TRIM(roundSigDigits(ZoneInletConvergence(ZoneNum)%InletNode(NodeIndex) &
                                        %Temperature(StackDepth), 6) ) // ','
              ENDDO
              CALL ShowContinueError('Node named ' &
                           //TRIM(NodeID(ZoneInletConvergence(ZoneNum)%InletNode(NodeIndex)%NodeNum)) &
                           //' temperature [C] iteration history trace (most recent first): ' &
                           //TRIM(HistoryTrace) )
            ENDIF ! need to report trace
            ! end Temperature checks

          ENDDO ! loop over zone inlet nodes
        ENDDO ! loop over zones

        DO LoopNum = 1, TotNumLoops

          IF (PlantConvergence(LoopNum)%PlantMassFlowNotConverged) THEN
            CALL ShowContinueError('Plant System Named = '//TRIM(PlantLoop(LoopNum)%Name)  &
                                   // ' did not converge for mass flow rate')
            CALL ShowContinueError('Check values should be zero. Most Recent values listed first.')
            HistoryTrace = ''
            DO StackDepth = 1, ConvergLogStackDepth
              HistoryTrace = TRIM(HistoryTrace) &
                  // TRIM(roundSigDigits(PlantConvergence(LoopNum)%PlantFlowDemandToSupplyTolValue(StackDepth), 6)) &
                  // ','
            ENDDO
            CALL ShowContinueError('Demand-to-Supply interface mass flow rate check value iteration history trace: ' &
                 //TRIM(HistoryTrace) )
            HistoryTrace = ''
            DO StackDepth = 1, ConvergLogStackDepth
              HistoryTrace = TRIM(HistoryTrace) &
                  // TRIM(roundSigDigits(PlantConvergence(LoopNum)%PlantFlowSupplyToDemandTolValue(StackDepth), 6)) &
                  // ','
            ENDDO
            CALL ShowContinueError('Supply-to-Demand interface mass flow rate check value iteration history trace: ' &
                 //TRIM(HistoryTrace) )

            ! now work with history logs for mass flow to detect issues
            DO ThisLoopSide = 1, SIZE(PlantLoop(LoopNum)%LoopSide)
              ! loop side inlet node
              FoundOscillationByDuplicate = .FALSE.
              MonotonicDecreaseFound = .FALSE.
              MonotonicIncreaseFound = .FALSE.
              AvgValue = SUM(PlantLoop(LoopNum)%LoopSide(ThisLoopSide)%InletNode%MassFlowRateHistory) &
                          / REAL(NumConvergenceHistoryTerms, r64)
              IF (ABS(PlantLoop(LoopNum)%LoopSide(ThisLoopSide)%InletNode%MassFlowRateHistory(1) - AvgValue) &
                   > PlantFlowRateOscillationToler) THEN
                FoundOscillationByDuplicate = .FALSE.
                DO StackDepth = 2, NumConvergenceHistoryTerms
                  IF (ABS(PlantLoop(LoopNum)%LoopSide(ThisLoopSide)%InletNode%MassFlowRateHistory(1) - &
                       PlantLoop(LoopNum)%LoopSide(ThisLoopSide)%InletNode%MassFlowRateHistory(StackDepth)) &
                         < PlantFlowRateOscillationToler) THEN
                    FoundOscillationByDuplicate = .TRUE.
                    CALL ShowContinueError('Node named ' &
                         //TRIM(PlantLoop(LoopNum)%LoopSide(ThisLoopSide)%NodeNameIn) &
                         //' shows oscillating flow rates across iterations with a repeated value of ' &
                         //TRIM(RoundSigDigits(  &
                            PlantLoop(LoopNum)%LoopSide(ThisLoopSide)%InletNode%MassFlowRateHistory(1), 7) ) )
                    EXIT
                  ENDIF
                ENDDO
              ENDIF
              IF (.NOT. FoundOscillationByDuplicate) THEN
                SlopeMdot = ( SUM( ConvergenceHistoryARR) &
                          *SUM(PlantLoop(LoopNum)%LoopSide(ThisLoopSide)%InletNode%MassFlowRateHistory)&
                         - REAL(NumConvergenceHistoryTerms, r64) * SUM((ConvergenceHistoryARR &
                               * PlantLoop(LoopNum)%LoopSide(ThisLoopSide)%InletNode%MassFlowRateHistory)) ) &
                          / ( SUM(ConvergenceHistoryARR)**2 &
                           -  REAL(NumConvergenceHistoryTerms, r64)*SUM( ConvergenceHistoryARR**2) )
                IF (ABS(SlopeMdot) > PlantFlowRateSlopeToler) THEN
                  IF (SlopeMdot < 0.d0) THEN  ! check for monotonic decrease
                    MonotonicDecreaseFound = .TRUE.
                    DO StackDepth = 2, NumConvergenceHistoryTerms
                      IF (PlantLoop(LoopNum)%LoopSide(ThisLoopSide)%InletNode%MassFlowRateHistory(StackDepth-1) > &
                          PlantLoop(LoopNum)%LoopSide(ThisLoopSide)%InletNode%MassFlowRateHistory(StackDepth)) THEN
                        MonotonicDecreaseFound = .FALSE.
                        EXIT
                      ENDIF
                    ENDDO
                    IF (MonotonicDecreaseFound) THEN
                      CALL ShowContinueError('Node named ' &
                           //TRIM(PlantLoop(LoopNum)%LoopSide(ThisLoopSide)%NodeNameIn) &
                           //' shows monotonically decreasing mass flow rate with a trend rate across iterations of ' &
                           //TRIM(RoundSigDigits(SlopeMdot, 7)) &
                           //' [kg/s/iteration]' )
                    ENDIF
                  ELSE  ! check for monotonic incrase
                    MonotonicIncreaseFound = .TRUE.
                    DO StackDepth = 2, NumConvergenceHistoryTerms
                      IF (PlantLoop(LoopNum)%LoopSide(ThisLoopSide)%InletNode%MassFlowRateHistory(StackDepth-1) < &
                          PlantLoop(LoopNum)%LoopSide(ThisLoopSide)%InletNode%MassFlowRateHistory(StackDepth)) THEN
                        MonotonicIncreaseFound = .FALSE.
                        EXIT
                      ENDIF
                    ENDDO
                    IF (MonotonicIncreaseFound) THEN
                      CALL ShowContinueError('Node named ' &
                           //TRIM(PlantLoop(LoopNum)%LoopSide(ThisLoopSide)%NodeNameIn) &
                           //' shows monotonically increasing mass flow rate with a trend rate across iterations of ' &
                           //TRIM(RoundSigDigits(SlopeMdot, 7)) &
                           //' [kg/s/iteration]' )
                    ENDIF
                  ENDIF
                ENDIF  ! significant slope found
              ENDIF ! no oscillation found

              IF (MonotonicDecreaseFound .OR. MonotonicIncreaseFound .OR. FoundOscillationByDuplicate) THEN
                HistoryTrace = ' '
                DO StackDepth = 1, NumConvergenceHistoryTerms
                  HistoryTrace = TRIM(HistoryTrace) &
                  // TRIM(roundSigDigits(PlantLoop(LoopNum)%LoopSide(ThisLoopSide)%InletNode &
                                            %MassFlowRateHistory(StackDepth), 7) ) // ','
                ENDDO
                CALL ShowContinueError('Node named ' &
                             //TRIM(PlantLoop(LoopNum)%LoopSide(ThisLoopSide)%NodeNameIn) &
                             //' mass flow rate [kg/s] iteration history trace (most recent first): ' &
                             //TRIM(HistoryTrace) )
              ENDIF ! need to report trace
              ! end of inlet node

              ! loop side outlet node
              FoundOscillationByDuplicate = .FALSE.
              MonotonicDecreaseFound = .FALSE.
              MonotonicIncreaseFound = .FALSE.
              AvgValue = SUM(PlantLoop(LoopNum)%LoopSide(ThisLoopSide)%OutletNode%MassFlowRateHistory) &
                          / REAL(NumConvergenceHistoryTerms, r64)
              IF (ABS(PlantLoop(LoopNum)%LoopSide(ThisLoopSide)%OutletNode%MassFlowRateHistory(1) - AvgValue) &
                   > PlantFlowRateOscillationToler) THEN
                FoundOscillationByDuplicate = .FALSE.
                DO StackDepth = 2, NumConvergenceHistoryTerms
                  IF (ABS(PlantLoop(LoopNum)%LoopSide(ThisLoopSide)%OutletNode%MassFlowRateHistory(1) - &
                       PlantLoop(LoopNum)%LoopSide(ThisLoopSide)%OutletNode%MassFlowRateHistory(StackDepth)) &
                         < PlantFlowRateOscillationToler) THEN
                    FoundOscillationByDuplicate = .TRUE.
                    CALL ShowContinueError('Node named ' &
                         //TRIM(PlantLoop(LoopNum)%LoopSide(ThisLoopSide)%NodeNameOut) &
                         //' shows oscillating flow rates across iterations with a repeated value of ' &
                         //TRIM(RoundSigDigits(  &
                            PlantLoop(LoopNum)%LoopSide(ThisLoopSide)%OutletNode%MassFlowRateHistory(1), 7) ) )
                    EXIT
                  ENDIF
                ENDDO
              ENDIF
              IF (.NOT. FoundOscillationByDuplicate) THEN
                SlopeMdot = ( SUM( ConvergenceHistoryARR) &
                          *SUM(PlantLoop(LoopNum)%LoopSide(ThisLoopSide)%OutletNode%MassFlowRateHistory)&
                         - REAL(NumConvergenceHistoryTerms, r64) * SUM((ConvergenceHistoryARR &
                               * PlantLoop(LoopNum)%LoopSide(ThisLoopSide)%OutletNode%MassFlowRateHistory)) ) &
                          / ( SUM(ConvergenceHistoryARR)**2 &
                           -  REAL(NumConvergenceHistoryTerms, r64)*SUM( ConvergenceHistoryARR**2) )
                IF (ABS(SlopeMdot) > PlantFlowRateSlopeToler) THEN
                  IF (SlopeMdot < 0.d0) THEN  ! check for monotonic decrease
                    MonotonicDecreaseFound = .TRUE.
                    DO StackDepth = 2, NumConvergenceHistoryTerms
                      IF (PlantLoop(LoopNum)%LoopSide(ThisLoopSide)%OutletNode%MassFlowRateHistory(StackDepth-1) > &
                          PlantLoop(LoopNum)%LoopSide(ThisLoopSide)%OutletNode%MassFlowRateHistory(StackDepth)) THEN
                        MonotonicDecreaseFound = .FALSE.
                        EXIT
                      ENDIF
                    ENDDO
                    IF (MonotonicDecreaseFound) THEN
                      CALL ShowContinueError('Node named ' &
                           //TRIM(PlantLoop(LoopNum)%LoopSide(ThisLoopSide)%NodeNameOut) &
                           //' shows monotonically decreasing mass flow rate with a trend rate across iterations of ' &
                           //TRIM(RoundSigDigits(SlopeMdot, 7)) &
                           //' [kg/s/iteration]' )
                    ENDIF
                  ELSE  ! check for monotonic incrase
                    MonotonicIncreaseFound = .TRUE.
                    DO StackDepth = 2, NumConvergenceHistoryTerms
                      IF (PlantLoop(LoopNum)%LoopSide(ThisLoopSide)%OutletNode%MassFlowRateHistory(StackDepth-1) < &
                          PlantLoop(LoopNum)%LoopSide(ThisLoopSide)%OutletNode%MassFlowRateHistory(StackDepth)) THEN
                        MonotonicIncreaseFound = .FALSE.
                        EXIT
                      ENDIF
                    ENDDO
                    IF (MonotonicIncreaseFound) THEN
                      CALL ShowContinueError('Node named ' &
                           //TRIM(PlantLoop(LoopNum)%LoopSide(ThisLoopSide)%NodeNameOut) &
                           //' shows monotonically increasing mass flow rate with a trend rate across iterations of ' &
                           //TRIM(RoundSigDigits(SlopeMdot, 7)) &
                           //' [kg/s/iteration]' )
                    ENDIF
                  ENDIF
                ENDIF  ! significant slope found
              ENDIF ! no oscillation found

              IF (MonotonicDecreaseFound .OR. MonotonicIncreaseFound .OR. FoundOscillationByDuplicate) THEN
                HistoryTrace = ' '
                DO StackDepth = 1, NumConvergenceHistoryTerms
                  HistoryTrace = TRIM(HistoryTrace) &
                  // TRIM(roundSigDigits(PlantLoop(LoopNum)%LoopSide(ThisLoopSide)%OutletNode &
                                            %MassFlowRateHistory(StackDepth), 7) ) // ','
                ENDDO
                CALL ShowContinueError('Node named ' &
                             //TRIM(PlantLoop(LoopNum)%LoopSide(ThisLoopSide)%NodeNameOut) &
                             //' mass flow rate [kg/s] iteration history trace (most recent first): ' &
                             //TRIM(HistoryTrace) )
              ENDIF ! need to report trace
              ! end of Outlet node

            END DO  ! plant loop sides

          ENDIF ! mass flow not converged

          IF (PlantConvergence(LoopNum)%PlantTempNotConverged) THEN
            CALL ShowContinueError('Plant System Named = '//TRIM(PlantLoop(LoopNum)%Name)  &
                                   // ' did not converge for temperature')
            CALL ShowContinueError('Check values should be zero. Most Recent values listed first.')
            HistoryTrace = ''
            DO StackDepth = 1, ConvergLogStackDepth
              HistoryTrace = TRIM(HistoryTrace) &
                  // TRIM(roundSigDigits(PlantConvergence(LoopNum)%PlantTempDemandToSupplyTolValue(StackDepth), 6)) &
                  // ','
            ENDDO
            CALL ShowContinueError('Demand-to-Supply interface temperature check value iteration history trace: ' &
                 //TRIM(HistoryTrace) )
            HistoryTrace = ''
            DO StackDepth = 1, ConvergLogStackDepth
              HistoryTrace = TRIM(HistoryTrace) &
                  // TRIM(roundSigDigits(PlantConvergence(LoopNum)%PlantTempSupplyToDemandTolValue(StackDepth), 6)) &
                  // ','
            ENDDO
            CALL ShowContinueError('Supply-to-Demand interface temperature check value iteration history trace: ' &
                 //TRIM(HistoryTrace) )

            ! now work with history logs for mass flow to detect issues
            DO ThisLoopSide = 1, SIZE(PlantLoop(LoopNum)%LoopSide)
              ! loop side inlet node
              FoundOscillationByDuplicate = .FALSE.
              MonotonicDecreaseFound = .FALSE.
              MonotonicIncreaseFound = .FALSE.
              AvgValue = SUM(PlantLoop(LoopNum)%LoopSide(ThisLoopSide)%InletNode%TemperatureHistory) &
                          / REAL(NumConvergenceHistoryTerms, r64)
              IF (ABS(PlantLoop(LoopNum)%LoopSide(ThisLoopSide)%InletNode%TemperatureHistory(1) - AvgValue) &
                   > PlantTemperatureOscillationToler) THEN
                FoundOscillationByDuplicate = .FALSE.
                DO StackDepth = 2, NumConvergenceHistoryTerms
                  IF (ABS(PlantLoop(LoopNum)%LoopSide(ThisLoopSide)%InletNode%TemperatureHistory(1) - &
                       PlantLoop(LoopNum)%LoopSide(ThisLoopSide)%InletNode%TemperatureHistory(StackDepth)) &
                         < PlantTemperatureOscillationToler) THEN
                    FoundOscillationByDuplicate = .TRUE.
                    CALL ShowContinueError('Node named ' &
                         //TRIM(PlantLoop(LoopNum)%LoopSide(ThisLoopSide)%NodeNameIn) &
                         //' shows oscillating temperatures across iterations with a repeated value of ' &
                         //TRIM(RoundSigDigits(  &
                            PlantLoop(LoopNum)%LoopSide(ThisLoopSide)%InletNode%TemperatureHistory(1), 5) ) )
                    EXIT
                  ENDIF
                ENDDO
              ENDIF
              IF (.NOT. FoundOscillationByDuplicate) THEN
                SlopeTemps = ( SUM( ConvergenceHistoryARR) &
                          *SUM(PlantLoop(LoopNum)%LoopSide(ThisLoopSide)%InletNode%TemperatureHistory)&
                         - REAL(NumConvergenceHistoryTerms, r64) * SUM((ConvergenceHistoryARR &
                               * PlantLoop(LoopNum)%LoopSide(ThisLoopSide)%InletNode%TemperatureHistory)) ) &
                          / ( SUM(ConvergenceHistoryARR)**2 &
                           -  REAL(NumConvergenceHistoryTerms, r64)*SUM( ConvergenceHistoryARR**2) )
                IF (ABS(SlopeTemps) > PlantTemperatureSlopeToler) THEN
                  IF (SlopeTemps < 0.d0) THEN  ! check for monotic decrease
                    MonotonicDecreaseFound = .TRUE.
                    DO StackDepth = 2, NumConvergenceHistoryTerms
                      IF (PlantLoop(LoopNum)%LoopSide(ThisLoopSide)%InletNode%TemperatureHistory(StackDepth-1) > &
                          PlantLoop(LoopNum)%LoopSide(ThisLoopSide)%InletNode%TemperatureHistory(StackDepth)) THEN
                        MonotonicDecreaseFound = .FALSE.
                        EXIT
                      ENDIF
                    ENDDO
                    IF (MonotonicDecreaseFound) THEN
                      CALL ShowContinueError('Node named ' &
                           //TRIM(PlantLoop(LoopNum)%LoopSide(ThisLoopSide)%NodeNameIn) &
                           //' shows monotonically decreasing temperatures with a trend rate across iterations of ' &
                           //TRIM(RoundSigDigits(SlopeTemps, 5)) &
                           //' [C/iteration]' )
                    ENDIF
                  ELSE  ! check for monotic incrase
                    MonotonicIncreaseFound = .TRUE.
                    DO StackDepth = 2, NumConvergenceHistoryTerms
                      IF (PlantLoop(LoopNum)%LoopSide(ThisLoopSide)%InletNode%TemperatureHistory(StackDepth-1) < &
                          PlantLoop(LoopNum)%LoopSide(ThisLoopSide)%InletNode%TemperatureHistory(StackDepth)) THEN
                        MonotonicIncreaseFound = .FALSE.
                        EXIT
                      ENDIF
                    ENDDO
                    IF (MonotonicIncreaseFound) THEN
                      CALL ShowContinueError('Node named ' &
                           //TRIM(PlantLoop(LoopNum)%LoopSide(ThisLoopSide)%NodeNameIn) &
                           //' shows monotonically increasing temperatures with a trend rate across iterations of ' &
                           //TRIM(RoundSigDigits(SlopeTemps, 5)) &
                           //' [C/iteration]' )
                    ENDIF
                  ENDIF
                ENDIF  ! significant slope found
              ENDIF ! no oscillation found

              IF (MonotonicDecreaseFound .OR. MonotonicIncreaseFound .OR. FoundOscillationByDuplicate) THEN
                HistoryTrace = ' '
                DO StackDepth = 1, NumConvergenceHistoryTerms
                  HistoryTrace = TRIM(HistoryTrace) &
                  // TRIM(roundSigDigits(PlantLoop(LoopNum)%LoopSide(ThisLoopSide)%InletNode &
                                            %TemperatureHistory(StackDepth), 5) ) // ','
                ENDDO
                CALL ShowContinueError('Node named ' &
                             //TRIM(PlantLoop(LoopNum)%LoopSide(ThisLoopSide)%NodeNameIn) &
                             //' temperature [C] iteration history trace (most recent first): ' &
                             //TRIM(HistoryTrace) )
              ENDIF ! need to report trace
              ! end of inlet node

              ! loop side outlet node
              FoundOscillationByDuplicate = .FALSE.
              MonotonicDecreaseFound = .FALSE.
              MonotonicIncreaseFound = .FALSE.
              AvgValue = SUM(PlantLoop(LoopNum)%LoopSide(ThisLoopSide)%OutletNode%TemperatureHistory) &
                          / REAL(NumConvergenceHistoryTerms, r64)
              IF (ABS(PlantLoop(LoopNum)%LoopSide(ThisLoopSide)%OutletNode%TemperatureHistory(1) - AvgValue) &
                   > PlantTemperatureOscillationToler) THEN
                FoundOscillationByDuplicate = .FALSE.
                DO StackDepth = 2, NumConvergenceHistoryTerms
                  IF (ABS(PlantLoop(LoopNum)%LoopSide(ThisLoopSide)%OutletNode%TemperatureHistory(1) - &
                       PlantLoop(LoopNum)%LoopSide(ThisLoopSide)%OutletNode%TemperatureHistory(StackDepth)) &
                         < PlantTemperatureOscillationToler) THEN
                    FoundOscillationByDuplicate = .TRUE.
                    CALL ShowContinueError('Node named ' &
                         //TRIM(PlantLoop(LoopNum)%LoopSide(ThisLoopSide)%NodeNameOut) &
                         //' shows oscillating temperatures across iterations with a repeated value of ' &
                         //TRIM(RoundSigDigits(  &
                            PlantLoop(LoopNum)%LoopSide(ThisLoopSide)%OutletNode%TemperatureHistory(1), 5) ) )
                    EXIT
                  ENDIF
                ENDDO
              ENDIF
              IF (.NOT. FoundOscillationByDuplicate) THEN
                SlopeTemps = ( SUM( ConvergenceHistoryARR) &
                          *SUM(PlantLoop(LoopNum)%LoopSide(ThisLoopSide)%OutletNode%TemperatureHistory)&
                         - REAL(NumConvergenceHistoryTerms, r64) * SUM((ConvergenceHistoryARR &
                               * PlantLoop(LoopNum)%LoopSide(ThisLoopSide)%OutletNode%TemperatureHistory)) ) &
                          / ( SUM(ConvergenceHistoryARR)**2 &
                           -  REAL(NumConvergenceHistoryTerms, r64)*SUM( ConvergenceHistoryARR**2) )
                IF (ABS(SlopeTemps) > PlantFlowRateSlopeToler) THEN
                  IF (SlopeTemps < 0.d0) THEN  ! check for monotic decrease
                    MonotonicDecreaseFound = .TRUE.
                    DO StackDepth = 2, NumConvergenceHistoryTerms
                      IF (PlantLoop(LoopNum)%LoopSide(ThisLoopSide)%OutletNode%TemperatureHistory(StackDepth-1) > &
                          PlantLoop(LoopNum)%LoopSide(ThisLoopSide)%OutletNode%TemperatureHistory(StackDepth)) THEN
                        MonotonicDecreaseFound = .FALSE.
                        EXIT
                      ENDIF
                    ENDDO
                    IF (MonotonicDecreaseFound) THEN
                      CALL ShowContinueError('Node named ' &
                           //TRIM(PlantLoop(LoopNum)%LoopSide(ThisLoopSide)%NodeNameOut) &
                           //' shows monotonically decreasing temperatures with a trend rate across iterations of ' &
                           //TRIM(RoundSigDigits(SlopeTemps, 5)) &
                           //' [C/iteration]' )
                    ENDIF
                  ELSE  ! check for monotic incrase
                    MonotonicIncreaseFound = .TRUE.
                    DO StackDepth = 2, NumConvergenceHistoryTerms
                      IF (PlantLoop(LoopNum)%LoopSide(ThisLoopSide)%OutletNode%TemperatureHistory(StackDepth-1) < &
                          PlantLoop(LoopNum)%LoopSide(ThisLoopSide)%OutletNode%TemperatureHistory(StackDepth)) THEN
                        MonotonicIncreaseFound = .FALSE.
                        EXIT
                      ENDIF
                    ENDDO
                    IF (MonotonicIncreaseFound) THEN
                      CALL ShowContinueError('Node named ' &
                           //TRIM(PlantLoop(LoopNum)%LoopSide(ThisLoopSide)%NodeNameOut) &
                           //' shows monotonically increasing temperatures with a trend rate across iterations of ' &
                           //TRIM(RoundSigDigits(SlopeTemps, 5)) &
                           //' [C/iteration]' )
                    ENDIF
                  ENDIF
                ENDIF  ! significant slope found
              ENDIF ! no oscillation found

              IF (MonotonicDecreaseFound .OR. MonotonicIncreaseFound .OR. FoundOscillationByDuplicate) THEN
                HistoryTrace = ' '
                DO StackDepth = 1, NumConvergenceHistoryTerms
                  HistoryTrace = TRIM(HistoryTrace) &
                  // TRIM(roundSigDigits(PlantLoop(LoopNum)%LoopSide(ThisLoopSide)%OutletNode &
                                            %TemperatureHistory(StackDepth), 5) ) // ','
                ENDDO
                CALL ShowContinueError('Node named ' &
                             //TRIM(PlantLoop(LoopNum)%LoopSide(ThisLoopSide)%NodeNameOut) &
                             //' temperature [C] iteration history trace (most recent first): ' &
                             //TRIM(HistoryTrace) )
              ENDIF ! need to report trace
              ! end of Outlet node

            END DO  ! plant loop sides

          ENDIF !temperature not converged
        ENDDO ! loop over plant loop systems
      ENDIF
    ELSE
      IF (EnvironmentName == ErrEnvironmentName) THEN
        CALL ShowRecurringWarningErrorAtEnd('SimHVAC: Exceeding Maximum iterations for all HVAC loops, during '//  &
                              TRIM(EnvironmentName)//' continues', MaxErrCount)
      ELSE
        MaxErrCount=0
        ErrEnvironmentName=EnvironmentName
        CALL ShowRecurringWarningErrorAtEnd('SimHVAC: Exceeding Maximum iterations for all HVAC loops, during '//  &
                              TRIM(EnvironmentName)//' continues', MaxErrCount)
      ENDIF
    ENDIF

  END IF
  ! Set node setpoints to a flag value so that controllers can check whether their sensed nodes
  ! have a setpoint
  IF ( .NOT. ZoneSizingCalc .AND. .NOT. SysSizingCalc) THEN
    IF (MySetPointInit) THEN
      IF (NumOfNodes > 0) THEN
        Node%TempSetPoint = SensedNodeFlagValue
        Node%HumRatSetPoint = SensedNodeFlagValue
        Node%HumRatMin = SensedNodeFlagValue
        Node%HumRatMax = SensedNodeFlagValue
        Node%MassFlowRateSetPoint = SensedNodeFlagValue ! BG 5-26-2009 (being checked in HVACControllers.f90)
        DefaultNodeValues%TempSetPoint = SensedNodeFlagValue
        DefaultNodeValues%HumRatSetPoint = SensedNodeFlagValue
        DefaultNodeValues%HumRatMin = SensedNodeFlagValue
        DefaultNodeValues%HumRatMax = SensedNodeFlagValue
        DefaultNodeValues%MassFlowRateSetPoint = SensedNodeFlagValue ! BG 5-26-2009 (being checked in HVACControllers.f90)
      ENDIF
      MySetPointInit = .FALSE.
      DoSetPointTest = .TRUE.
    ELSE
      DoSetPointTest = .FALSE.
    END IF
  END IF
  IF (SetPointErrorFlag) THEN
    CALL ShowFatalError('Previous severe set point errors cause program termination')
  END IF

  RETURN

END SUBROUTINE SimHVAC


SUBROUTINE SimSelectedEquipment(SimAirLoops, SimZoneEquipment, SimNonZoneEquipment, SimPlantLoops, &
                                SimElecCircuits,  FirstHVACIteration, LockPlantFlows)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Russ Taylor, Rick Strand
          !       DATE WRITTEN   May 1998
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine receives the flags from SimHVAC which determines
          ! which middle-level managers must be called.

          ! METHODOLOGY EMPLOYED:
          ! Each flag is checked and the appropriate manager is then called.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE ZoneEquipmentManager,         ONLY: ManageZoneEquipment
  USE NonZoneEquipmentManager,      ONLY: ManageNonZoneEquipment
  USE SimAirServingZones,           ONLY: ManageAirLoops
  USE PlantManager,                 ONLY: ManagePlantLoops
  USE ManageElectricPower,          ONLY: ManageElectricLoadCenters
  USE AirflowNetworkBalanceManager, ONLY: ManageAirflowNetworkBalance
  USE DataErrorTracking,            ONLY: AskForPlantCheckOnAbort
  USE PlantUtilities,               ONLY: SetAllFlowLocks, ResetAllPlantInterConnectFlags
  USE DataPlant,                    ONLY: FlowUnlocked, FlowLocked, AnyPlantLoopSidesNeedSim

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  LOGICAL :: SimAirLoops            ! True when the air loops need to be (re)simulated
  LOGICAL :: SimZoneEquipment       ! True when zone equipment components need to be (re)simulated
  LOGICAL :: SimNonZoneEquipment    ! True when non-zone equipment components need to be (re)simulated
  LOGICAL :: SimPlantLoops          ! True when the main plant loops need to be (re)simulated
  LOGICAL :: SimElecCircuits        ! True when electic circuits need to be (re)simulated
  LOGICAL :: FirstHVACIteration     ! True when solution technique on first iteration
  LOGICAL :: ResimulateAirZone      ! True when solution technique on third iteration used in AirflowNetwork
  LOGICAL, INTENT(IN) :: LockPlantFlows

          ! SUBROUTINE PARAMETER DEFINITIONS:
  Integer, PARAMETER :: MaxAir    = 5    ! Iteration Max for Air Simulation Iterations
  Integer, PARAMETER :: MaxPlant  = 3    ! Iteration Max for Plant Simulation Iteration
  Integer, PARAMETER :: MaxCond   = 3    ! Iteration Max for Plant Simulation Iteration

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: IterAir                ! counts iterations to enforce maximum iteration limit
  LOGICAL,SAVE  :: MyEnvrnFlag = .true.
  LOGICAL,SAVE :: FlowMaxAvailAlreadyReset = .FALSE.
  LOGICAL :: FlowResolutionNeeded = .FALSE.

          ! FLOW:

  IterAir   = 0

  ! Set all plant flow locks to UNLOCKED to allow air side components to operate properly
  ! This requires that the plant flow resolver carefully set the min/max avail limits on
  !  air side components to ensure they request within bounds.
  IF (LockPlantFlows) THEN
    CALL SetAllFlowLocks(FlowLocked)
  ELSE
    CALL SetAllFlowLocks(FlowUnlocked)
  ENDIF
  CALL ResetAllPlantInterConnectFlags()

  IF (BeginEnvrnFlag .and. MyEnvrnFlag) THEN
    ! Following comment is incorrect!  (LKL) Even the first time through this does more than read in data.
    ! Zone equipment data needs to be read in before air loop data to allow the
    ! determination of which zones are connected to which air loops.
    ! This call of ManageZoneEquipment does nothing except force the
    ! zone equipment data to be read in.
    CALL ManageZoneEquipment(FirstHVACIteration,SimZoneEquipment,SimAirLoops)
    MyEnvrnFlag = .false.
  END IF
  IF (.not. BeginEnvrnFlag) THEN
    MyEnvrnFlag=.true.
  ENDIF

  IF (FirstHVACIteration) THEN
    RepIterAir = 0
  ! Call AirflowNetwork simulation to calculate air flows and pressures
    if (SimulateAirflowNetwork .gt. AirflowNetworkControlSimple) then
      CALL ManageAirflowNetworkBalance(FirstHVACIteration)
    end if
    CALL ManageAirLoops(FirstHVACIteration,SimAirLoops,SimZoneEquipment)
    AirLoopInputsFilled = .True. ! all air loop inputs have been read in
    SimAirLoops = .True.  !Need to make sure that SimAirLoop is simulated at min twice to calculate PLR in some air loop equipment
    AirLoopsSimOnce = .True. ! air loops simulated once for this environment
    CALL ResetTerminalUnitFlowLimits
    FlowMaxAvailAlreadyReset = .TRUE.
    CALL ManageZoneEquipment(FirstHVACIteration,SimZoneEquipment,SimAirLoops)
    SimZoneEquipment = .True.  !needs to be simulated at least twice for flow resolution to propagate to this routine
    CALL ManageNonZoneEquipment(FirstHVACIteration,SimNonZoneEquipment)

    CALL ManageElectricLoadCenters(FirstHVACIteration,SimElecCircuits, .FALSE.)

    CALL ManagePlantLoops(FirstHVACIteration,SimAirLoops,SimZoneEquipment,SimNonZoneEquipment, &
                          SimPlantLoops,SimElecCircuits)

    AskForPlantCheckOnAbort = .true.  ! need to make a first pass through plant calcs before this check make sense
    CALL ManageElectricLoadCenters(FirstHVACIteration,SimElecCircuits, .FALSE.)
  ELSE
    FlowResolutionNeeded = .FALSE.
    DO WHILE ((SimAirLoops .OR. SimZoneEquipment) .AND. (IterAir.LE.MaxAir) )
      IterAir = IterAir + 1   ! Increment the iteration counter
     ! Call AirflowNetwork simulation to calculate air flows and pressures
      ResimulateAirZone = .FALSE.
      if (SimulateAirflowNetwork .gt. AirflowNetworkControlSimple) then
        CALL ManageAirflowNetworkBalance(FirstHVACIteration,IterAir,ResimulateAirZone)
      end if
      IF (SimAirLoops) THEN
        CALL ManageAirLoops(FirstHVACIteration,SimAirLoops,SimZoneEquipment)
        SimElecCircuits =.TRUE.       !If this was simulated there are possible electric changes that need to be simulated
      END IF

      ! make sure flow resolution gets done
      IF (FlowResolutionNeeded) THEN
        SimZoneEquipment = .TRUE.
      END IF
      IF (SimZoneEquipment) THEN
        If ((IterAir == 1) .and. (.not. FlowMaxAvailAlreadyReset)) THEN ! don't do reset if already done in FirstHVACIteration
          CALL ResetTerminalUnitFlowLimits
          FlowResolutionNeeded = .TRUE.
        ELSE
          CALL ResolveAirLoopFlowLimits
          FlowResolutionNeeded = .FALSE.
        END IF
        CALL ManageZoneEquipment(FirstHVACIteration,SimZoneEquipment,SimAirLoops)
        SimElecCircuits =.TRUE.      ! If this was simulated there are possible electric changes that need to be simulated

      END IF
      FlowMaxAvailAlreadyReset = .FALSE.

!      IterAir = IterAir + 1   ! Increment the iteration counter
      IF (SimulateAirflowNetwork .gt. AirflowNetworkControlSimple) THEN
        If (ResimulateAirZone) then ! Need to make sure that SimAirLoop and SimZoneEquipment are simulated
          SimAirLoops = .TRUE.       ! at min three times using ONOFF fan with the AirflowNetwork model
          SimZoneEquipment = .TRUE.
        End If
      END IF


    END DO

    RepIterAir = RepIterAir + IterAir
    IF (IterAir > MaxAir) THEN
      AirLoopConvergFail = 1
    ELSE
      AirLoopConvergFail = 0
    END IF
    ! Check to see if any components have been locked out. If so, SimAirLoops will be reset to TRUE.
    CALL ResolveLockoutFlags(SimAirLoops)



    IF (SimNonZoneEquipment) THEN
      CALL ManageNonZoneEquipment(FirstHVACIteration,SimNonZoneEquipment)
      SimElecCircuits =.TRUE.      ! If this was simulated there are possible electric changes that need to be simulated
    END IF

    IF (SimElecCircuits) THEN
     CALL ManageElectricLoadCenters(FirstHVACIteration,SimElecCircuits, .FALSE.)
    END IF

    IF (.NOT. SimPlantLoops) THEN
     ! check to see if any air side component may have requested plant resim
      IF (AnyPlantLoopSidesNeedSim()) THEN
        SimPlantLoops = .TRUE.
      ENDIF

    ENDIF

    IF (SimPlantLoops) THEN
      CALL ManagePlantLoops(FirstHVACIteration,SimAirLoops,SimZoneEquipment,SimNonZoneEquipment, &
                          SimPlantLoops,SimElecCircuits)
    ENDIF

    IF (SimElecCircuits) THEN
      CALL ManageElectricLoadCenters(FirstHVACIteration,SimElecCircuits, .FALSE.)
    END IF

  END IF

  RETURN

END SUBROUTINE SimSelectedEquipment

SUBROUTINE ResetTerminalUnitFlowLimits

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   Feb 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Reset the max flow available limits at the inlet nodes of terminal units

          ! METHODOLOGY EMPLOYED:
          ! Loops through all air loops, finds the inlet nodes of the terminal units
          ! served by each air loop, and resets the node MassFlowRateMaxAvail (and MinAvail) to
          ! the hard max and mins.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataZoneEquipment,       ONLY : ZoneEquipConfig

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER             :: AirLoopIndex
  INTEGER             :: ZonesCooledIndex
  INTEGER             :: ZonesHeatedIndex
  INTEGER             :: TermInletNode

  DO AirLoopIndex=1,NumPrimaryAirSys ! loop over the primary air loops
    DO ZonesCooledIndex=1,AirToZoneNodeInfo(AirLoopIndex)%NumZonesCooled ! loop over the zones cooled by this air loop
      TermInletNode = AirToZoneNodeInfo(AirLoopIndex)%TermUnitCoolInletNodes(ZonesCooledIndex)
      ! reset the max avail flow rate at the terminal unit cold air inlet to the max
      Node(TermInletNode)%MassFlowRateMaxAvail = Node(TermInletNode)%MassFlowRateMax
      Node(TermInletNode)%MassFlowRateMinAvail = Node(TermInletNode)%MassFlowRateMin
    END DO
    DO ZonesHeatedIndex=1,AirToZoneNodeInfo(AirLoopIndex)%NumZonesHeated ! loop over the zones heated by this air loop
      TermInletNode = AirToZoneNodeInfo(AirLoopIndex)%TermUnitHeatInletNodes(ZonesHeatedIndex)
      ! reset the max avail flow rate at the terminal unit hot air inlet to the max
      Node(TermInletNode)%MassFlowRateMaxAvail = Node(TermInletNode)%MassFlowRateMax
      Node(TermInletNode)%MassFlowRateMinAvail = Node(TermInletNode)%MassFlowRateMin
    END DO
  END DO

  RETURN

  END SUBROUTINE ResetTerminalUnitFlowLimits

SUBROUTINE ResolveAirLoopFlowLimits

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   August 2003
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is for resolving hard flow mismatches between zone equipment and
          ! the primary air loop. Such a mismatch can occur when the air terminal units are
          ! requsting more air than the central air system can supply.

          ! METHODOLOGY EMPLOYED:
          ! Sets the MassFlowRateMaxAvail on the terminal unit inlet nodes to match the
          ! maximum available from the primary air loop.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataZoneEquipment,       ONLY : ZoneEquipConfig
  USE DataConvergParams, ONLY : HVACFlowRateToler

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER             :: AirLoopIndex
  INTEGER             :: ZonesCooledIndex
  INTEGER             :: ZonesHeatedIndex
  INTEGER             :: TermInletNode
  INTEGER             :: SupplyIndex
  INTEGER             :: SupplyNode
  REAL(r64)           :: FlowRatio

  DO AirLoopIndex=1,NumPrimaryAirSys ! loop over the primary air loops
    DO SupplyIndex=1,AirToZoneNodeInfo(AirLoopIndex)%NumSupplyNodes ! loop over the air loop supply outlets
      IF (AirToZoneNodeInfo(AirLoopIndex)%SupplyDuctType(SupplyIndex) == Cooling) THEN ! check for cooling duct
        ! check if terminal units requesting more air than air loop can supply; if so, set terminal unit inlet
        ! node mass flow max avail to what air loop can supply
        SupplyNode = AirToZoneNodeInfo(AirLoopIndex)%AirLoopSupplyNodeNum(SupplyIndex)
        IF (Node(SupplyNode)%MassFlowRate > 0.0d0) THEN
          IF ( (Node(SupplyNode)%MassFlowRateSetPoint - Node(SupplyNode)%MassFlowRate) > HVACFlowRateToler * 0.01d0) THEN
            FlowRatio = Node(SupplyNode)%MassFlowRate / Node(SupplyNode)%MassFlowRateSetPoint
            DO ZonesCooledIndex=1,AirToZoneNodeInfo(AirLoopIndex)%NumZonesCooled
              TermInletNode = AirToZoneNodeInfo(AirLoopIndex)%TermUnitCoolInletNodes(ZonesCooledIndex)
              Node(TermInletNode)%MassFlowRateMaxAvail = Node(TermInletNode)%MassFlowRate * FlowRatio
            END DO
          END IF
          IF ( (Node(SupplyNode)%MassFlowRateSetPoint - Node(SupplyNode)%MassFlowRate) < - HVACFlowRateToler * 0.01d0) THEN
            IF (Node(SupplyNode)%MassFlowRateSetPoint == 0.0d0) THEN
!               CALL ShowFatalError('ResolveAirLoopFlowLimits: Node MassFlowRateSetPoint = 0.0, Node='//  &
!                                   TRIM(NodeID(SupplyNode))//  &
!                                   ', check for Node Connection Errors in the following messages.')
              DO ZonesCooledIndex=1,AirToZoneNodeInfo(AirLoopIndex)%NumZonesCooled
                TermInletNode = AirToZoneNodeInfo(AirLoopIndex)%TermUnitCoolInletNodes(ZonesCooledIndex)
                Node(TermInletNode)%MassFlowRateMaxAvail = Node(TermInletNode)%MassFlowRateMax
                Node(TermInletNode)%MassFlowRateMinAvail = Node(SupplyNode)%MassFlowRate /   &
                         REAL(AirToZoneNodeInfo(AirLoopIndex)%NumZonesCooled,r64)
              END DO
            ELSE
              FlowRatio = Node(SupplyNode)%MassFlowRate / Node(SupplyNode)%MassFlowRateSetPoint
              DO ZonesCooledIndex=1,AirToZoneNodeInfo(AirLoopIndex)%NumZonesCooled
                TermInletNode = AirToZoneNodeInfo(AirLoopIndex)%TermUnitCoolInletNodes(ZonesCooledIndex)
                Node(TermInletNode)%MassFlowRateMinAvail = Node(TermInletNode)%MassFlowRate * FlowRatio
              END DO
            END IF
          END IF
        END IF
      END IF
    END DO
    DO SupplyIndex=1,AirToZoneNodeInfo(AirLoopIndex)%NumSupplyNodes ! loop over the air loop supply outlets
      IF (AirToZoneNodeInfo(AirLoopIndex)%SupplyDuctType(SupplyIndex) == Heating) THEN ! check for heating duct
        ! check if terminal units requesting more air than air loop can supply; if so, set terminal unit inlet
        ! node mass flow max avail to what air loop can supply
        SupplyNode = AirToZoneNodeInfo(AirLoopIndex)%AirLoopSupplyNodeNum(SupplyIndex)
        IF (Node(SupplyNode)%MassFlowRate > 0.0d0) THEN
          IF ( (Node(SupplyNode)%MassFlowRateSetPoint - Node(SupplyNode)%MassFlowRate) > HVACFlowRateToler * 0.01d0) THEN
            FlowRatio = Node(SupplyNode)%MassFlowRate / Node(SupplyNode)%MassFlowRateSetPoint
            DO ZonesHeatedIndex=1,AirToZoneNodeInfo(AirLoopIndex)%NumZonesHeated
              TermInletNode = AirToZoneNodeInfo(AirLoopIndex)%TermUnitHeatInletNodes(ZonesHeatedIndex)
              Node(TermInletNode)%MassFlowRateMaxAvail = Node(TermInletNode)%MassFlowRate * FlowRatio
            END DO
          END IF
          IF ( (Node(SupplyNode)%MassFlowRateSetPoint - Node(SupplyNode)%MassFlowRate) < - HVACFlowRateToler * 0.01d0) THEN
            IF (Node(SupplyNode)%MassFlowRateSetPoint == 0.0d0) THEN
              ! CALL ShowFatalError('ResolveAirLoopFlowLimits: Node MassFlowRateSetPoint = 0.0, Node='//  &
                                  ! TRIM(NodeID(SupplyNode))//  &
                                  ! ', check for Node Connection Errors in the following messages.')
              DO ZonesHeatedIndex=1,AirToZoneNodeInfo(AirLoopIndex)%NumZonesHeated
                TermInletNode = AirToZoneNodeInfo(AirLoopIndex)%TermUnitHeatInletNodes(ZonesHeatedIndex)
                Node(TermInletNode)%MassFlowRateMaxAvail = Node(TermInletNode)%MassFlowRateMax
                Node(TermInletNode)%MassFlowRateMinAvail = Node(SupplyNode)%MassFlowRate /   &
                         REAL(AirToZoneNodeInfo(AirLoopIndex)%NumZonesCooled,r64)
              END DO
            ELSE
              FlowRatio = Node(SupplyNode)%MassFlowRate / Node(SupplyNode)%MassFlowRateSetPoint
              DO ZonesHeatedIndex=1,AirToZoneNodeInfo(AirLoopIndex)%NumZonesHeated
                TermInletNode = AirToZoneNodeInfo(AirLoopIndex)%TermUnitHeatInletNodes(ZonesHeatedIndex)
                Node(TermInletNode)%MassFlowRateMinAvail = Node(TermInletNode)%MassFlowRate * FlowRatio
              END DO
            ENDIF
          END IF
        END IF
      END IF
    END DO
  END DO

  RETURN

END SUBROUTINE ResolveAirLoopFlowLimits

SUBROUTINE ResolveLockoutFlags(SimAir)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   December 2003
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine checks for components lockout flags and asks for air loop resimulation
          ! if any components have been locked out

          ! METHODOLOGY EMPLOYED:
          ! Checks if loop lockout flags are .TRUE.; if so, sets SimAirLoops to .TRUE.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  LOGICAL, INTENT(INOUT) :: SimAir ! TRUE means air loops must be (re)simulated

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER             :: AirLoopIndex

    DO AirLoopIndex=1,NumPrimaryAirSys ! loop over the primary air loops
      ! check if economizer ia active and if there is a request that it be locked out
      IF (AirLoopControlInfo(AirLoopIndex)%EconoActive .AND. &
           (AirLoopControlInfo(AirLoopIndex)%ReqstEconoLockoutWithCompressor .OR. &
            AirLoopControlInfo(AirLoopIndex)%ReqstEconoLockoutWithHeating)) THEN
        AirLoopControlInfo(AirLoopIndex)%EconoLockout = .TRUE.
        SimAir = .TRUE.
      END IF
    END DO

  RETURN

END SUBROUTINE ResolveLockoutFlags

SUBROUTINE ResetHVACControl

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   December 2004
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine resets loop control flags and specified flow rates that may
          ! have been set by the set point and availability managers in the previous
          ! time step

          ! METHODOLOGY EMPLOYED:

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

    IF (NumPrimaryAirSys == 0) RETURN
    AirLoopControlInfo%NightVent = .FALSE.
    AirLoopControlInfo%LoopFlowRateSet = .FALSE.
    AirLoopFlow%ReqSupplyFrac = 1.0d0

  RETURN

END SUBROUTINE ResetHVACControl

SUBROUTINE ResetNodeData

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   March 2005
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This routine resets all node data to "initial" conditions.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

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
          ! na
  IF (NumOfNodes <= 0) RETURN

  Node%Temp                      = DefaultNodeValues%Temp
  Node%TempMin                   = DefaultNodeValues%TempMin
  Node%TempMax                   = DefaultNodeValues%TempMax
  Node%TempSetPoint              = DefaultNodeValues%TempSetPoint
  Node%MassFlowRate              = DefaultNodeValues%MassFlowRate
  Node%MassFlowRateMin           = DefaultNodeValues%MassFlowRateMin
  Node%MassFlowRateMax           = DefaultNodeValues%MassFlowRateMax
  Node%MassFlowRateMinAvail      = DefaultNodeValues%MassFlowRateMinAvail
  Node%MassFlowRateMaxAvail      = DefaultNodeValues%MassFlowRateMaxAvail
  Node%MassFlowRateSetPoint      = DefaultNodeValues%MassFlowRateSetPoint
  Node%Quality                   = DefaultNodeValues%Quality
  Node%Press                     = DefaultNodeValues%Press
  Node%Enthalpy                  = DefaultNodeValues%Enthalpy
  Node%HumRat                    = DefaultNodeValues%HumRat
  Node%HumRatMin                 = DefaultNodeValues%HumRatMin
  Node%HumRatMax                 = DefaultNodeValues%HumRatMax
  Node%HumRatSetPoint            = DefaultNodeValues%HumRatSetPoint
  Node%TempSetPointHi            = DefaultNodeValues%TempSetPointHi
  Node%TempSetPointLo            = DefaultNodeValues%TempSetPointLo

  IF (ALLOCATED(MoreNodeInfo)) THEN
    MoreNodeInfo%WetbulbTemp       = DefaultNodeValues%Temp
    MoreNodeInfo%RelHumidity       = 0.0d0
    MoreNodeInfo%ReportEnthalpy    = DefaultNodeValues%Enthalpy
    MoreNodeInfo%VolFlowRateStdRho = 0.0d0
    MoreNodeInfo%VolFlowRateCrntRho= 0.0d0
    MoreNodeInfo%Density           = 0.0d0
  ENDIF

  RETURN

END SUBROUTINE ResetNodeData

SUBROUTINE UpdateZoneListAndGroupLoads

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Apparently someone who doesn't believe in documenting.
          !       DATE WRITTEN   ???
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Don't know.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataHeatBalance

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
  INTEGER :: ZoneNum, ListNum, GroupNum, Mult

          ! FLOW:
  ! Sum ZONE LIST and ZONE GROUP report variables
  ListSNLoadHeatEnergy = 0.0d0
  ListSNLoadCoolEnergy = 0.0d0
  ListSNLoadHeatRate = 0.0d0
  ListSNLoadCoolRate = 0.0d0

  DO ListNum = 1, NumOfZoneLists
    DO ZoneNum = 1, ZoneList(ListNum)%NumOfZones
      Mult = Zone(ZoneNum)%Multiplier
      ListSNLoadHeatEnergy(ListNum) = ListSNLoadHeatEnergy(ListNum) + SNLoadHeatEnergy(ZoneList(ListNum)%Zone(ZoneNum)) * Mult
      ListSNLoadCoolEnergy(ListNum) = ListSNLoadCoolEnergy(ListNum) + SNLoadCoolEnergy(ZoneList(ListNum)%Zone(ZoneNum)) * Mult
      ListSNLoadHeatRate(ListNum) = ListSNLoadHeatRate(ListNum) + SNLoadHeatRate(ZoneList(ListNum)%Zone(ZoneNum)) * Mult
      ListSNLoadCoolRate(ListNum) = ListSNLoadCoolRate(ListNum) + SNLoadCoolRate(ZoneList(ListNum)%Zone(ZoneNum)) * Mult
    END DO ! ZoneNum
  END DO ! ListNum

  DO GroupNum = 1, NumOfZoneGroups
    Mult = ZoneGroup(GroupNum)%Multiplier
    GroupSNLoadHeatEnergy(GroupNum) = ListSNLoadHeatEnergy(ZoneGroup(GroupNum)%ZoneList) * Mult
    GroupSNLoadCoolEnergy(GroupNum) = ListSNLoadCoolEnergy(ZoneGroup(GroupNum)%ZoneList) * Mult
    GroupSNLoadHeatRate(GroupNum) = ListSNLoadHeatRate(ZoneGroup(GroupNum)%ZoneList) * Mult
    GroupSNLoadCoolRate(GroupNum) = ListSNLoadCoolRate(ZoneGroup(GroupNum)%ZoneList) * Mult
  END DO ! GroupNum

  RETURN

END SUBROUTINE UpdateZoneListAndGroupLoads

SUBROUTINE CalcAirFlowSimple(SysTimestepLoop)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Legacy Code
          !       DATE WRITTEN   na
          !       MODIFIED       Shirey, Jan 2008 (MIXING objects, use avg. conditions for Cp, Air Density and Hfg)
          !       MODIFIED       L. Lawrie and L. GU, Jan. 2008 (Allow multiple infiltration and ventilation objects)
          !                      B. Griffith. Jan 2009 add infiltration, residential basic/sherman-grimsrud and enhanced/AIM2
          !                      L. Lawrie - March 2009 - move ventilation electric calculation to this routine (for
          !                        Electricity Net.
          !                      L. Gu - Dec. 2009 - Added a new ventilation object to calculate flow rate based on wind and stack
          !                        effect through an opening.
          !       MODIFIED       Stovall - Aug 2011 (add refrigerator door mixing)
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine calculates the air component of the heat balance.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataEnvironment, ONLY: OutBaroPress, OutHumRat, OutEnthalpy, WindSpeed
  USE DataHeatBalFanSys
  USE DataHeatBalance
  USE Psychrometrics, ONLY:PsyRhoAirFnPbTdbW,PsyCpAirFnWTdb,PsyTdbFnHW
  USE DataRoomAirModel, ONLY: ZTJET,AirModel,RoomAirModel_UCSDDV,RoomAirModel_UCSDCV
  USE ScheduleManager, ONLY: GetCurrentScheduleValue
  USE DataAirflowNetwork, ONLY: SimulateAirflowNetwork,AirflowNetworkControlSimple,AirflowNetworkControlSimpleADS,  &
                       AirflowNetworkZoneFlag
  USE EarthTube, ONLY : ManageEarthTube
  USE CoolTower, ONLY: ManageCoolTower
  USE ThermalChimney, ONLY : ManageThermalChimney
  USE DataZoneEquipment, ONLY: ZoneEquipAvail
  USE DataHVACGlobals, ONLY: CycleOn, CycleOnZoneFansOnly
  USE DataContaminantBalance, ONLY: Contaminant, ZoneAirCO2, MixingMassFlowCO2, ZoneAirGC, MixingMassFlowGC

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN), OPTIONAL :: SysTimestepLoop               ! System time step index

          ! SUBROUTINE PARAMETER DEFINITIONS:
    REAL(r64), PARAMETER :: StdGravity    = 9.80665d0   ! The acceleration of gravity at the sea level (m/s2)

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
   REAL(r64) MCP
   REAL(r64) MCPxM
   REAL(r64) MCPxN
   REAL(r64)  TZM  ! Temperature of From Zone
   REAL(r64)  TZN  ! Temperature of this zone
   REAL(r64) TD                ! Delta Temp limit of Mixing statement
   REAL(r64) Tavg  ! Average temperature in two zones exchanging air
   REAL(r64) Wavg  ! Average humidity ratio in two zones exchanging air
   INTEGER M              ! Index to From Zone
   INTEGER N              ! Index of this zone
   INTEGER J              ! Loop Counter
   INTEGER NZ             ! A pointer
   INTEGER I              ! Ventilation master object index
   INTEGER NH             ! Hybrid controlled zone number
   REAL(r64) AirDensity        ! Density of air (kg/m^3)
   REAL(r64) CpAir             ! Heat capacity of air (J/kg-C)
   REAL(r64) OutletAirEnthalpy ! Enthlapy of outlet air (VENTILATION objects)
   REAL(r64) :: TempExt
   REAL(r64) :: WindExt
   LOGICAL MixingLimitFlag
   REAL(r64) :: MixingTmin
   REAL(r64) :: MixingTmax

   REAL(r64) :: IVF  !DESIGN INFILTRATION FLOW RATE (M**3/SEC)
   REAL(r64) :: VVF  !DESIGN VENTILATION FLOW RATE (M**3/SEC)
   REAL(r64) :: MCpI_temp
   REAL(r64) :: VAMFL_temp
   REAL(r64), ALLOCATABLE, SAVE, DIMENSION(:) :: ZMAT ! Zone air temperature
   REAL(r64), ALLOCATABLE, SAVE, DIMENSION(:) :: ZHumRat  ! Zone air humidity ratio
   REAL(r64) :: Cw   ! Opening effectivenss
   REAL(r64) :: Cd   ! Discharge coefficent
   REAL(r64) :: Angle   ! Angle between wind direction and effective angle
   REAL(r64) :: Qw   ! Volumetric flow driven by wind
   REAL(r64) :: Qst  ! Volumetric flow driven by stack effect
   REAL(r64) :: MassFlowDiff
   !following variables used for refrigeration door mixing and all defined in EngRef
   INTEGER :: ZoneA
   INTEGER :: ZoneB
   REAL(r64) :: TZoneA
   REAL(r64) :: TZoneB
   REAL(r64) :: HumRatZoneA
   REAL(r64) :: HumRatZoneB
   REAL(r64) :: AirDensityZoneA
   REAL(r64) :: CpAirZoneA
   REAL(r64) :: AirDensityZoneB
   REAL(r64) :: CpAirZoneB
   REAL(r64) :: AirDensityAvg
   REAL(r64) :: MassFlowDryAir
   REAL(r64) :: SchedDoorOpen
   REAL(r64) :: DoorHeight
   REAL(r64) :: DoorArea
   REAL(r64) :: DoorProt
   REAL(r64) :: FDens
   REAL(r64) :: Fb
   REAL(r64) :: FFlow
   REAL(r64) :: MassFlowToA
   REAL(r64) :: MassFlowToB
   REAL(r64) :: MassFlowXCpToA
   REAL(r64) :: MassFlowXCpToB
   REAL(r64) :: MassFlowXCpXTempToA
   REAL(r64) :: MassFlowXCpXTempToB
   REAL(r64) :: MassFlowXHumRatToA
   REAL(r64) :: MassFlowXHumRatToB
!
   ! Allocate the ZMAT and ZHumRat arrays
   IF (.NOT. ALLOCATED(ZMAT)) ALLOCATE(ZMAT(NumOfZones))
   IF (.NOT. ALLOCATED(ZHumRat)) ALLOCATE(ZHumRat(NumOfZones))
   IF (.NOT. ALLOCATED(VentMCP)) ALLOCATE(VentMCP(TotVentilation))

   ! Allocate module level logical arrays for MIXING and CROSS MIXING reporting
   IF (.NOT. ALLOCATED(CrossMixingReportFlag)) ALLOCATE(CrossMixingReportFlag(TotCrossMixing))
   IF (.NOT. ALLOCATED(MixingReportFlag)) ALLOCATE(MixingReportFlag(TotMixing))

   IF (.not. ALLOCATED(MCPTThermChim)) ALLOCATE(MCPTThermChim(NumOfZones))
   IF (.not. ALLOCATED(MCPThermChim))  ALLOCATE(MCPThermChim(NumOfZones))
   IF (.not. ALLOCATED(ThermChimAMFL)) ALLOCATE(ThermChimAMFL(NumOfZones))

!                                      COMPUTE ZONE AIR MIXINGS
!
   MCPM=0.0d0
   MCPTM=0.0d0
   MixingMassFlowZone = 0.0d0
   MixingMassFlowXHumRat = 0.0d0
   CrossMixingFlag = .FALSE.
   CrossMixingReportFlag = .FALSE.
   MixingReportFlag = .FALSE.
   IF (Contaminant%CO2Simulation .AND. TotMixing+TotCrossMixing+TotRefDoorMixing > 0) MixingMassFlowCO2 = 0.0d0
   IF (Contaminant%GenericContamSimulation .AND. TotMixing+TotCrossMixing+TotRefDoorMixing > 0) MixingMassFlowGC = 0.0d0

   IVF = 0.0d0
   MCPTI = 0.0d0
   MCPI = 0.0d0
   OAMFL = 0.0d0
   VVF = 0.0d0
   MCPTV = 0.0d0
   MCPV = 0.0d0
   VAMFL = 0.0d0
   VentMCP = 0.0d0
   MDotCPOA = 0.0d0
   MDotOA = 0.0d0

   MCPThermChim = 0.0d0
   ThermChimAMFL = 0.0d0
   MCPTThermChim = 0.0d0


   IF (AirFlowFlag .NE. UseSimpleAirFlow) RETURN
   ! AirflowNetwork Multizone field /= SIMPLE
   IF (.NOT. (SimulateAirflowNetwork .EQ. AirflowNetworkControlSimple .OR. &
              SimulateAirflowNetwork .EQ. AirflowNetworkControlSimpleADS)) RETURN

   CALL ManageEarthTube
   CALL ManageCoolTower
   CALL ManageThermalChimney

   ! Assign zone air temperature
   DO J=1,NumOfZones
      ZMAT(J) = MAT(J)
      ZHumRat(J) = ZoneAirHumRat(J)
      ! This is only temperory fix for CR8867.  (L. Gu 8/12)
      If (PRESENT(SysTimestepLoop) .AND. SysTimestepLoop == 1) Then
        ZMAT(J) = XMPT(J)
        ZHumRat(J) = WZoneTimeMinusP(J)
      End If
   END DO

      ! Process the scheduled Ventilation for air heat balance
   IF (TotVentilation > 0) THEN
     ZnAirRpt%VentilFanElec=0.0d0
   ENDIF

   ! Initialization of ZoneAirBalance
   If (TotZoneAirBalance .gt. 0) Then
     ZoneAirBalance%BalMassFlowRate =0.0d0
     ZoneAirBalance%InfMassFlowRate =0.0d0
     ZoneAirBalance%NatMassFlowRate =0.0d0
     ZoneAirBalance%ExhMassFlowRate =0.0d0
     ZoneAirBalance%IntMassFlowRate =0.0d0
     ZoneAirBalance%ERVMassFlowRate =0.0d0
   End If

   DO J = 1, TotVentilation
     NZ = Ventilation(J)%ZonePtr
     Ventilation(J)%FanPower = 0.0d0
     TempExt = Zone(NZ)%OutDryBulbTemp
     WindExt = Zone(NZ)%WindSpeed
     AirDensity = PsyRhoAirFnPbTdbW(OutBaroPress,TempExt,OutHumRat)
     CpAir = PsyCpAirFnWTdb(OutHumRat,TempExt)
  !CR7751 should maybe use code below, indoor conditions instead of outdoor conditions
  !   AirDensity = PsyRhoAirFnPbTdbW(OutBaroPress, ZMAT(NZ), ZHumRat(NZ))
  !   CpAir = PsyCpAirFnWTdb(ZHumRat(NZ),ZMAT(NZ))
     ! Hybrid ventilation global control
     If (Ventilation(J)%HybridControlType == HybridControlTypeGlobal .AND. Ventilation(J)%HybridControlMasterNum > 0) Then
       I = Ventilation(J)%HybridControlMasterNum
       NH = Ventilation(I)%ZonePtr
       If (J .eq. I) Ventilation(J)%HybridControlMasterStatus = .FALSE.
     Else
       I = J
       NH = NZ
     End If
     ! Check scheduled temperatures
     If (Ventilation(I)%MinIndoorTempSchedPtr > 0) Then
       Ventilation(I)%MinIndoorTemperature = GetCurrentScheduleValue(Ventilation(I)%MinIndoorTempSchedPtr)
     End If
     If (Ventilation(I)%MaxIndoorTempSchedPtr > 0) Then
       Ventilation(I)%MaxIndoorTemperature = GetCurrentScheduleValue(Ventilation(I)%MaxIndoorTempSchedPtr)
     End If
     ! Ensure the minimum indoor temperature <= the maximum indoor temperature
     If (Ventilation(I)%MinIndoorTempSchedPtr > 0 .OR. Ventilation(I)%MaxIndoorTempSchedPtr > 0) Then
       If (Ventilation(I)%MinIndoorTemperature > Ventilation(I)%MaxIndoorTemperature) Then
         Ventilation(I)%IndoorTempErrCount = Ventilation(I)%IndoorTempErrCount + 1
         if (Ventilation(I)%IndoorTempErrCount< 2) then
           CALL ShowWarningError('Ventilation indoor temperature control: The minimum indoor temperature is above '// &
             'the maximum indoor temperature in '//TRIM(Ventilation(I)%Name))
           CALL ShowContinueError('The minimum indoor temperature is set to the maximum indoor temperature. ' &
                                 //'Simulation continues.')
           CALL ShowContinueErrorTimeStamp(' Occurrence info: ')
         else
           CALL ShowRecurringWarningErrorAtEnd('The minimum indoor temperature is still above '// &
             'the maximum indoor temperature',Ventilation(I)%IndoorTempErrIndex, &
              Ventilation(I)%MinIndoorTemperature, Ventilation(I)%MinIndoorTemperature)
         end if
         Ventilation(I)%MinIndoorTemperature = Ventilation(I)%MaxIndoorTemperature
       End If
     End If
     If (Ventilation(I)%MinOutdoorTempSchedPtr > 0) Then
       Ventilation(I)%MinOutdoorTemperature = GetCurrentScheduleValue(Ventilation(I)%MinOutdoorTempSchedPtr)
     End If
     If (Ventilation(I)%MaxOutdoorTempSchedPtr > 0) Then
       Ventilation(I)%MaxOutdoorTemperature = GetCurrentScheduleValue(Ventilation(I)%MaxOutdoorTempSchedPtr)
     End If
     ! Ensure the minimum outdoor temperature <= the maximum outdoor temperature
     If (Ventilation(I)%MinOutdoorTempSchedPtr > 0 .OR. Ventilation(I)%MaxOutdoorTempSchedPtr > 0) Then
       If (Ventilation(I)%MinOutdoorTemperature > Ventilation(I)%MaxOutdoorTemperature) Then
         Ventilation(I)%OutdoorTempErrCount = Ventilation(I)%OutdoorTempErrCount + 1
         if (Ventilation(I)%OutdoorTempErrCount< 2) then
           CALL ShowWarningError('Ventilation outdoor temperature control: The minimum outdoor temperature is above '// &
             'the maximum outdoor temperature in '//TRIM(Ventilation(I)%Name))
           CALL ShowContinueError('The minimum outdoor temperature is set to the maximum outdoor temperature. ' &
                                 //'Simulation continues.')
           CALL ShowContinueErrorTimeStamp(' Occurrence info: ')
         else
           CALL ShowRecurringWarningErrorAtEnd('The minimum outdoor temperature is still above '// &
             'the maximum outdoor temperature',Ventilation(I)%OutdoorTempErrIndex, &
              Ventilation(I)%MinOutdoorTemperature, Ventilation(I)%MinOutdoorTemperature)
         end if
         Ventilation(I)%MinIndoorTemperature = Ventilation(I)%MaxIndoorTemperature
       End If
     End If
     If (Ventilation(I)%DeltaTempSchedPtr > 0) Then
       Ventilation(I)%DelTemperature = GetCurrentScheduleValue(Ventilation(I)%DeltaTempSchedPtr)
     End If
        ! Skip this if the zone is below the minimum indoor temperature limit
     IF ((ZMAT(NH) < Ventilation(I)%MinIndoorTemperature) .AND. (.NOT. Ventilation(J)%EMSSimpleVentOn ))CYCLE
        ! Skip this if the zone is above the maximum indoor temperature limit
     IF ((ZMAT(NH) > Ventilation(I)%MaxIndoorTemperature) .AND. (.NOT. Ventilation(J)%EMSSimpleVentOn ))CYCLE
        ! Skip if below the temperature difference limit (3/12/03 Negative DelTemperature allowed now)
     IF (((ZMAT(NH)-TempExt) < Ventilation(I)%DelTemperature) .AND. (.NOT. Ventilation(J)%EMSSimpleVentOn ))CYCLE
        ! Skip this if the outdoor temperature is below the minimum outdoor temperature limit
     IF ((TempExt < Ventilation(I)%MinOutdoorTemperature) .AND. (.NOT. Ventilation(J)%EMSSimpleVentOn ))CYCLE
        ! Skip this if the outdoor temperature is above the maximum outdoor temperature limit
     IF ((TempExt > Ventilation(I)%MaxOutdoorTemperature) .AND. (.NOT. Ventilation(J)%EMSSimpleVentOn ))CYCLE
        ! Skip this if the outdoor wind speed is above the maximum windspeed limit
     IF ((WindExt > Ventilation(I)%MaxWindSpeed) .AND. (.NOT. Ventilation(J)%EMSSimpleVentOn ))CYCLE

     ! Hybrid ventilation controls
     If ((Ventilation(J)%HybridControlType == HybridControlTypeClose) .AND. (.NOT. Ventilation(J)%EMSSimpleVentOn ))Cycle
     If (Ventilation(J)%HybridControlType == HybridControlTypeGlobal .AND. Ventilation(J)%HybridControlMasterNum > 0) Then
       If (J .EQ. I) Ventilation(J)%HybridControlMasterStatus = .TRUE.
     End IF

     IF (Ventilation(J)%ModelType == VentilationDesignFlowRate) Then
       ! CR6845 if calculated < 0, don't propagate.
       VVF  = Ventilation(J)%DesignLevel*GetCurrentScheduleValue(Ventilation(J)%SchedPtr)

       IF (Ventilation(J)%EMSSimpleVentOn) VVF  = Ventilation(J)%EMSimpleVentFlowRate

       IF (VVF < 0.0d0) VVF=0.0d0
       VentMCP(J)=VVF*AirDensity*CpAir*( Ventilation(J)%ConstantTermCoef      &
               + ABS(TempExt-ZMAT(NZ))*Ventilation(J)%TemperatureTermCoef &
               + WindExt*(Ventilation(J)%VelocityTermCoef + WindExt*Ventilation(J)%VelocitySQTermCoef) )
       IF (VentMCP(J) < 0.0d0) VentMCP(J)=0.0d0
       VAMFL_temp=VentMCP(J)/CpAir
       If (Ventilation(J)%QuadratureSum) Then
         SELECT CASE (Ventilation(J)%FanType)  ! ventilation type based calculation
           CASE (ExhaustVentilation)
             ZoneAirBalance(Ventilation(J)%OABalancePtr)%ExhMassFlowRate = &
               ZoneAirBalance(Ventilation(J)%OABalancePtr)%ExhMassFlowRate + VentMCP(J)/CpAir
           CASE (IntakeVentilation)
             ZoneAirBalance(Ventilation(J)%OABalancePtr)%IntMassFlowRate = &
               ZoneAirBalance(Ventilation(J)%OABalancePtr)%IntMassFlowRate + VentMCP(J)/CpAir
           CASE (NaturalVentilation)
             ZoneAirBalance(Ventilation(J)%OABalancePtr)%NatMassFlowRate = &
               ZoneAirBalance(Ventilation(J)%OABalancePtr)%NatMassFlowRate + VentMCP(J)/CpAir
           CASE (BalancedVentilation)
             ZoneAirBalance(Ventilation(J)%OABalancePtr)%BalMassFlowRate = &
               ZoneAirBalance(Ventilation(J)%OABalancePtr)%BalMassFlowRate + VentMCP(J)/CpAir
         END SELECT
       Else
         MCPV(NZ) = MCPV(NZ)+VentMCP(J)
         VAMFL(NZ) = VAMFL(NZ)+VAMFL_temp
       End If
       IF (Ventilation(J)%FanEfficiency > 0.0d0) THEN
         Ventilation(J)%FanPower = VAMFL_temp*Ventilation(J)%FanPressure/(Ventilation(J)%FanEfficiency*AirDensity)
         IF (Ventilation(J)%FanType == BalancedVentilation) Ventilation(J)%FanPower = 2.0d0*Ventilation(J)%FanPower
         ! calc electric
         IF (SimulateAirflowNetwork .EQ. AirflowNetworkControlSimpleADS) THEN
! CR7608 IF (.not. TurnFansOn .or. .not. AirflowNetworkZoneFlag(NZ)) &
           IF (.not. KickOffSimulation) THEN
             IF (.not. (ZoneEquipAvail(NZ).EQ.CycleOn .OR. ZoneEquipAvail(NZ).EQ.CycleOnZoneFansOnly) .or. &
               .not. AirflowNetworkZoneFlag(NZ)) &
               ZnAirRpt(NZ)%VentilFanElec  = ZnAirRpt(NZ)%VentilFanElec+Ventilation(J)%FanPower*TimeStepSys*SecInHour
           ELSEIF (.not. AirflowNetworkZoneFlag(NZ)) THEN
             ZnAirRpt(NZ)%VentilFanElec  = ZnAirRpt(NZ)%VentilFanElec+Ventilation(J)%FanPower*TimeStepSys*SecInHour
           ENDIF
         ELSE
           ZnAirRpt(NZ)%VentilFanElec  = ZnAirRpt(NZ)%VentilFanElec+Ventilation(J)%FanPower*TimeStepSys*SecInHour
         END IF
       END IF
       ! Intake fans will add some heat to the air, raising the temperature for an intake fan...
       IF (Ventilation(J)%FanType == IntakeVentilation .OR. Ventilation(J)%FanType == BalancedVentilation) THEN
         IF (VAMFL_temp == 0.0d0) Then
            OutletAirEnthalpy         = OutEnthalpy
         ELSE
           IF (Ventilation(J)%FanPower > 0.0d0) THEN
             If (Ventilation(J)%FanType == BalancedVentilation) Then
               OutletAirEnthalpy = OutEnthalpy + Ventilation(J)%FanPower/VAMFL_temp/2.0d0 ! Half fan power to calculate inlet T
             Else
               OutletAirEnthalpy = OutEnthalpy + Ventilation(J)%FanPower/VAMFL_temp
             End If
           ELSE
             OutletAirEnthalpy         = OutEnthalpy
           ENDIF
         END IF
         Ventilation(J)%AirTemp = PsyTdbFnHW(OutletAirEnthalpy,OutHumRat)
       ELSE
         Ventilation(J)%AirTemp = TempExt
       END IF
       If (.NOT. Ventilation(J)%QuadratureSum) MCPTV(NZ) = MCPTV(NZ)+VentMCP(J)*Ventilation(J)%AirTemp
     END IF

     If (Ventilation(J)%ModelType == VentilationWindAndStack) Then
       If (Ventilation(J)%OpenEff /= AutoCalculate) Then
         Cw = Ventilation(J)%OpenEff
       Else
         ! linear interpolation between effective angle and wind direction
         angle = abs(WindDir - Ventilation(J)%EffAngle)
         If (angle > 180.d0) angle = angle - 180.d0
         Cw = 0.55d0 + angle/180.d0*(0.3d0-0.55d0)
       End If
       If (Ventilation(J)%DiscCoef /= AutoCalculate) Then
         Cd = Ventilation(J)%DiscCoef
       Else
         Cd = 0.40d0 + 0.0045d0*ABS(TempExt-ZMAT(NZ))
       End If
       Qw = Cw*Ventilation(J)%OpenArea*GetCurrentScheduleValue(Ventilation(J)%OpenAreaSchedPtr)*WindExt
       Qst = Cd*Ventilation(J)%OpenArea*GetCurrentScheduleValue(Ventilation(J)%OpenAreaSchedPtr)* &
            SQRT(2.d0*9.81d0*Ventilation(J)%DH*ABS(TempExt-ZMAT(NZ))/(ZMAT(NZ)+273.15d0))
       VVF  = SQRT(Qw*Qw + Qst*Qst)
       IF (Ventilation(J)%EMSSimpleVentOn) VVF  = Ventilation(J)%EMSimpleVentFlowRate
       IF (VVF < 0.0d0) VVF=0.0d0
       VentMCP(J)=VVF*AirDensity*CpAir
       IF (VentMCP(J) < 0.0d0) VentMCP(J)=0.0d0
       If (Ventilation(J)%QuadratureSum) Then
         ZoneAirBalance(Ventilation(J)%OABalancePtr)%NatMassFlowRate = &
           ZoneAirBalance(Ventilation(J)%OABalancePtr)%NatMassFlowRate + VentMCP(J)/CpAir
       Else
         MCPV(NZ) = MCPV(NZ)+VentMCP(J)
         VAMFL_temp=VentMCP(J)/CpAir
         VAMFL(NZ) = VAMFL(NZ)+VAMFL_temp
         Ventilation(J)%AirTemp = TempExt
         MCPTV(NZ) = MCPTV(NZ)+VentMCP(J)*Ventilation(J)%AirTemp
       End If
     End If
   END DO

   ! Process Mixing
   DO J=1,TotMixing
     N=Mixing(J)%ZonePtr
     M=Mixing(J)%FromZone
     TD=Mixing(J)%DeltaTemperature
     ! Get scheduled delta temperature
     If (Mixing(J)%DeltaTempSchedPtr > 0) Then
       TD = GetCurrentScheduleValue(Mixing(J)%DeltaTempSchedPtr)
     End If
     TZN=ZMAT(N)
     TZM=ZMAT(M)

     ! Hybrid ventilation controls
     If (Mixing(J)%HybridControlType == HybridControlTypeClose) Cycle
     ! Check temperature limit
     MixingLimitFlag = .FALSE.

     ! Hybrid ventilation global control
   If (Mixing(J)%HybridControlType == HybridControlTypeGlobal .AND. Mixing(J)%HybridControlMasterNum > 0) Then
     I = Mixing(J)%HybridControlMasterNum
     If (.NOT. Ventilation(I)%HybridControlMasterStatus) Cycle
   Else
     ! Ensure the minimum indoor temperature <= the maximum indoor temperature
     If (Mixing(J)%MinIndoorTempSchedPtr > 0) MixingTmin = GetCurrentScheduleValue(Mixing(J)%MinIndoorTempSchedPtr)
     If (Mixing(J)%MaxIndoorTempSchedPtr > 0) MixingTmax = GetCurrentScheduleValue(Mixing(J)%MaxIndoorTempSchedPtr)
     If (Mixing(J)%MinIndoorTempSchedPtr > 0 .AND. Mixing(J)%MaxIndoorTempSchedPtr > 0) Then
       If (MixingTmin > MixingTmax) Then
         Mixing(J)%IndoorTempErrCount = Mixing(J)%IndoorTempErrCount + 1
         if (Mixing(J)%IndoorTempErrCount< 2) then
           CALL ShowWarningError('Mixing zone temperature control: The minimum zone temperature is above '// &
             'the maximum zone temperature in '//TRIM(Mixing(J)%Name))
           CALL ShowContinueError('The minimum zone temperature is set to the maximum zone temperature. ' &
                                 //'Simulation continues.')
           CALL ShowContinueErrorTimeStamp(' Occurrence info: ')
         else
           CALL ShowRecurringWarningErrorAtEnd('The minimum zone temperature is still above '// &
             'the maximum zone temperature',Mixing(J)%IndoorTempErrIndex, MixingTmin, MixingTmin)
         end if
         MixingTmin = MixingTmax
       End If
     End If
     If (Mixing(J)%MinIndoorTempSchedPtr > 0) Then
       If (TZN < MixingTmin) MixingLimitFlag = .TRUE.
     End If
     If (Mixing(J)%MaxIndoorTempSchedPtr > 0) Then
       If (TZN > MixingTmax) MixingLimitFlag = .TRUE.
     End If
     ! Ensure the minimum source temperature <= the maximum source temperature
     If (Mixing(J)%MinSourceTempSchedPtr > 0) MixingTmin = GetCurrentScheduleValue(Mixing(J)%MinSourceTempSchedPtr)
     If (Mixing(J)%MaxSourceTempSchedPtr > 0) MixingTmax = GetCurrentScheduleValue(Mixing(J)%MaxSourceTempSchedPtr)
     If (Mixing(J)%MinSourceTempSchedPtr > 0 .AND. Mixing(J)%MaxSourceTempSchedPtr > 0) Then
       If (MixingTmin > MixingTmax) Then
         Mixing(J)%SourceTempErrCount = Mixing(J)%SourceTempErrCount + 1
         if (Mixing(J)%SourceTempErrCount< 2) then
           CALL ShowWarningError('Mixing source temperature control: The minimum source temperature is above '// &
             'the maximum source temperature in '//TRIM(Mixing(J)%Name))
           CALL ShowContinueError('The minimum source temperature is set to the maximum source temperature. ' &
                                 //'Simulation continues.')
           CALL ShowContinueErrorTimeStamp(' Occurrence info: ')
         else
           CALL ShowRecurringWarningErrorAtEnd('The minimum source temperature is still above '// &
             'the maximum source temperature',Mixing(J)%SourceTempErrIndex, MixingTmin, MixingTmin)
         end if
         MixingTmin = MixingTmax
       End If
     End If
     If (Mixing(J)%MinSourceTempSchedPtr > 0) Then
       If (TZM < MixingTmin) MixingLimitFlag = .TRUE.
     End If
     If (Mixing(J)%MaxSourceTempSchedPtr > 0) Then
       If (TZM > MixingTmax) MixingLimitFlag = .TRUE.
     End If
     ! Ensure the minimum outdoor temperature <= the maximum outdoor temperature
     TempExt = Zone(N)%OutDryBulbTemp
     If (Mixing(J)%MinOutdoorTempSchedPtr > 0) MixingTmin = GetCurrentScheduleValue(Mixing(J)%MinOutdoorTempSchedPtr)
     If (Mixing(J)%MaxOutdoorTempSchedPtr > 0) MixingTmax = GetCurrentScheduleValue(Mixing(J)%MaxOutdoorTempSchedPtr)
     If (Mixing(J)%MinOutdoorTempSchedPtr > 0 .AND. Mixing(J)%MaxOutdoorTempSchedPtr > 0) Then
       If (MixingTmin > MixingTmax) Then
         Mixing(J)%OutdoorTempErrCount = Mixing(J)%OutdoorTempErrCount + 1
         if (Mixing(J)%OutdoorTempErrCount< 2) then
           CALL ShowWarningError('Mixing outdoor temperature control: The minimum outdoor temperature is above '// &
             'the maximum outdoor temperature in '//TRIM(Mixing(J)%Name))
           CALL ShowContinueError('The minimum outdoor temperature is set to the maximum source temperature. ' &
                                 //'Simulation continues.')
           CALL ShowContinueErrorTimeStamp(' Occurrence info: ')
         else
           CALL ShowRecurringWarningErrorAtEnd('The minimum outdoor temperature is still above '// &
             'the maximum outdoor temperature',Mixing(J)%OutdoorTempErrIndex, MixingTmin, MixingTmin)
         end if
         MixingTmin = MixingTmax
       End If
     End If
     If (Mixing(J)%MinOutdoorTempSchedPtr > 0) Then
       If (TempExt < MixingTmin) MixingLimitFlag = .TRUE.
     End If
     If (Mixing(J)%MaxOutdoorTempSchedPtr > 0) Then
       If (TempExt > MixingTmax) MixingLimitFlag = .TRUE.
     End If
   End IF

     If (Mixing(J)%HybridControlType /= HybridControlTypeGlobal .AND. MixingLimitFlag) Cycle
     If (Mixing(J)%HybridControlType == HybridControlTypeGlobal) TD = 0.0d0

!  If TD equals zero (default) set coefficients for full mixing otherwise test
!    for mixing conditions if user input delta temp > 0, then from zone temp (TZM)
!    must be td degrees warmer than zone temp (TZN).  If user input delta temp < 0,
!    then from zone temp (TZM) must be TD degrees cooler than zone temp (TZN).
     IF (TD < 0.0d0) THEN
       IF (TZM < TZN+TD) THEN
!            Per Jan 17, 2008 conference call, agreed to use average conditions for Rho, Cp and Hfg
!             RhoAirM = PsyRhoAirFnPbTdbW(OutBaroPress,tzm,ZHumRat(m))
!             MCP=Mixing(J)%DesiredAirFlowRate * PsyCpAirFnWTdb(ZHumRat(m),tzm) * RhoAirM
          AirDensity = PsyRhoAirFnPbTdbW(OutBaroPress,(tzn+tzm)/2.0d0,(ZHumRat(n)+ZHumRat(m))/2.0d0)
          CpAir = PsyCpAirFnWTdb((ZHumRat(n)+ZHumRat(m))/2.0d0,(tzn+tzm)/2.0d0) ! Use average conditions
          MCP=Mixing(J)%DesiredAirFlowRate * CpAir * AirDensity
          MCPM(N)=MCPM(N)+MCP
          MCPTM(N)=MCPTM(N)+MCP*TZM

          ! Now to determine the moisture conditions
          MixingMassFlowZone(N) = MixingMassFlowZone(N) + Mixing(J)%DesiredAirFlowRate * AirDensity
          MixingMassFlowXHumRat(N) = MixingMassFlowXHumRat(N) + Mixing(J)%DesiredAirFlowRate * AirDensity * ZHumRat(M)
          IF (Contaminant%CO2Simulation) Then
            MixingMassFlowCO2(N) = MixingMassFlowCO2(N) + Mixing(J)%DesiredAirFlowRate * AirDensity * ZoneAirCO2(M)
          END IF
          IF (Contaminant%GenericContamSimulation) Then
            MixingMassFlowGC(N) = MixingMassFlowGC(N) + Mixing(J)%DesiredAirFlowRate * AirDensity * ZoneAirGC(M)
          END IF
          MixingReportFlag(J) = .TRUE.
       END IF
     END IF
     IF (TD > 0.0d0) THEN
       IF (TZM > TZN+TD) THEN
!             RhoAirM = PsyRhoAirFnPbTdbW(OutBaroPress,tzm,ZHumRat(m))
!             MCP=Mixing(J)%DesiredAirFlowRate * PsyCpAirFnWTdb(ZHumRat(m),tzm) * RhoAirM
         AirDensity = PsyRhoAirFnPbTdbW(OutBaroPress,(tzn+tzm)/2.0d0,(ZHumRat(n)+ZHumRat(m))/2.0d0) ! Use avg conditions
         CpAir = PsyCpAirFnWTdb((ZHumRat(n)+ZHumRat(m))/2.0d0,(tzn+tzm)/2.0d0) ! Use average conditions
         MCP=Mixing(J)%DesiredAirFlowRate * CpAir * AirDensity
         MCPM(N)=MCPM(N)+MCP
         MCPTM(N)=MCPTM(N)+MCP*TZM
         ! Now to determine the moisture conditions
         MixingMassFlowZone(N) = MixingMassFlowZone(N) + Mixing(J)%DesiredAirFlowRate * AirDensity
         MixingMassFlowXHumRat(N) = MixingMassFlowXHumRat(N) + Mixing(J)%DesiredAirFlowRate * AirDensity * ZHumRat(M)
         IF (Contaminant%CO2Simulation) Then
           MixingMassFlowCO2(N) = MixingMassFlowCO2(N) + Mixing(J)%DesiredAirFlowRate * AirDensity * ZoneAirCO2(M)
         END IF
         IF (Contaminant%GenericContamSimulation) Then
           MixingMassFlowGC(N) = MixingMassFlowGC(N) + Mixing(J)%DesiredAirFlowRate * AirDensity * ZoneAirGC(M)
         END IF
         MixingReportFlag(J) = .TRUE.
       END IF
     END IF
     IF (TD == 0.0d0) THEN
!          RhoAirM = PsyRhoAirFnPbTdbW(OutBaroPress,tzm,ZHumRat(m))
!          MCP=Mixing(J)%DesiredAirFlowRate * PsyCpAirFnWTdb(ZHumRat(m),tzm) * RhoAirM
       AirDensity = PsyRhoAirFnPbTdbW(OutBaroPress,(tzn+tzm)/2.0d0,(ZHumRat(n)+ZHumRat(m))/2.0d0,  &
          calledfrom='CalcAirFlowSimple:Mixing') ! Use avg conditions
       CpAir = PsyCpAirFnWTdb((ZHumRat(n)+ZHumRat(m))/2.0d0,(tzn+tzm)/2.0d0,  &
          calledfrom='CalcAirFlowSimple:Mixing') ! Use average conditions
       MCP=Mixing(J)%DesiredAirFlowRate * CpAir * AirDensity
       MCPM(N)=MCPM(N)+MCP
       MCPTM(N)=MCPTM(N)+MCP*TZM
       ! Now to determine the moisture conditions
       MixingMassFlowZone(N) = MixingMassFlowZone(N) + Mixing(J)%DesiredAirFlowRate * AirDensity
       MixingMassFlowXHumRat(N) = MixingMassFlowXHumRat(N) + Mixing(J)%DesiredAirFlowRate * AirDensity * ZHumRat(M)
       IF (Contaminant%CO2Simulation) Then
         MixingMassFlowCO2(N) = MixingMassFlowCO2(N) + Mixing(J)%DesiredAirFlowRate * AirDensity * ZoneAirCO2(M)
       END IF
       IF (Contaminant%GenericContamSimulation) Then
         MixingMassFlowGC(N) = MixingMassFlowGC(N) + Mixing(J)%DesiredAirFlowRate * AirDensity * ZoneAirGC(M)
       END IF
       MixingReportFlag(J) = .TRUE.
    END IF
   END DO

!                              COMPUTE CROSS ZONE
!                              AIR MIXING
   DO J=1,TotCrossMixing
     N=CrossMixing(J)%ZonePtr
     M=CrossMixing(J)%FromZone
     TD=MTC(J)
     ! Get scheduled delta temperature
     If (CrossMixing(J)%DeltaTempSchedPtr > 0) Then
       TD = GetCurrentScheduleValue(CrossMixing(J)%DeltaTempSchedPtr)
     End If

     IF (TD .GE. 0.0d0) THEN
!
       TZN=ZMAT(N)
       TZM=ZMAT(M)
       ! Check temperature limit
       MixingLimitFlag = .FALSE.
       ! Ensure the minimum indoor temperature <= the maximum indoor temperature
       If (CrossMixing(J)%MinIndoorTempSchedPtr > 0) MixingTmin = GetCurrentScheduleValue(CrossMixing(J)%MinIndoorTempSchedPtr)
       If (CrossMixing(J)%MaxIndoorTempSchedPtr > 0) MixingTmax = GetCurrentScheduleValue(CrossMixing(J)%MaxIndoorTempSchedPtr)
       If (CrossMixing(J)%MinIndoorTempSchedPtr > 0 .AND. CrossMixing(J)%MaxIndoorTempSchedPtr > 0) Then
         If (MixingTmin > MixingTmax) Then
           CrossMixing(J)%IndoorTempErrCount = CrossMixing(J)%IndoorTempErrCount + 1
           if (CrossMixing(J)%IndoorTempErrCount< 2) then
             CALL ShowWarningError('CrossMixing zone temperature control: The minimum zone temperature is above '// &
               'the maximum zone temperature in '//TRIM(CrossMixing(J)%Name))
             CALL ShowContinueError('The minimum zone temperature is set to the maximum zone temperature. ' &
                                   //'Simulation continues.')
             CALL ShowContinueErrorTimeStamp(' Occurrence info: ')
           else
             CALL ShowRecurringWarningErrorAtEnd('The minimum zone temperature is still above '// &
               'the maximum zone temperature',CrossMixing(J)%IndoorTempErrIndex, MixingTmin, MixingTmin)
           end if
           MixingTmin = MixingTmax
         End If
       End If
       If (CrossMixing(J)%MinIndoorTempSchedPtr > 0) Then
         If (TZN < MixingTmin) MixingLimitFlag = .TRUE.
       End If
       If (CrossMixing(J)%MaxIndoorTempSchedPtr > 0) Then
         If (TZN > MixingTmax) MixingLimitFlag = .TRUE.
       End If
       ! Ensure the minimum source temperature <= the maximum source temperature
       If (CrossMixing(J)%MinSourceTempSchedPtr > 0) MixingTmin = GetCurrentScheduleValue(CrossMixing(J)%MinSourceTempSchedPtr)
       If (CrossMixing(J)%MaxSourceTempSchedPtr > 0) MixingTmax = GetCurrentScheduleValue(CrossMixing(J)%MaxSourceTempSchedPtr)
       If (CrossMixing(J)%MinSourceTempSchedPtr > 0 .AND. CrossMixing(J)%MaxSourceTempSchedPtr > 0) Then
         If (MixingTmin > MixingTmax) Then
           CrossMixing(J)%SourceTempErrCount = CrossMixing(J)%SourceTempErrCount + 1
           if (CrossMixing(J)%SourceTempErrCount< 2) then
             CALL ShowWarningError('CrossMixing source temperature control: The minimum source temperature is above '// &
               'the maximum source temperature in '//TRIM(CrossMixing(J)%Name))
             CALL ShowContinueError('The minimum source temperature is set to the maximum source temperature. ' &
                                 //'Simulation continues.')
             CALL ShowContinueErrorTimeStamp(' Occurrence info: ')
           else
             CALL ShowRecurringWarningErrorAtEnd('The minimum source temperature is still above '// &
               'the maximum source temperature',CrossMixing(J)%SourceTempErrIndex, MixingTmin, MixingTmin)
           end if
           MixingTmin = MixingTmax
         End If
       End If
       If (CrossMixing(J)%MinSourceTempSchedPtr > 0) Then
         If (TZM < MixingTmin) MixingLimitFlag = .TRUE.
       End If
       If (CrossMixing(J)%MaxSourceTempSchedPtr > 0) Then
         If (TZM > MixingTmax) MixingLimitFlag = .TRUE.
       End If
       ! Ensure the minimum outdoor temperature <= the maximum outdoor temperature
       TempExt = Zone(N)%OutDryBulbTemp
       If (CrossMixing(J)%MinOutdoorTempSchedPtr > 0) MixingTmin = GetCurrentScheduleValue(CrossMixing(J)%MinOutdoorTempSchedPtr)
       If (CrossMixing(J)%MaxOutdoorTempSchedPtr > 0) MixingTmax = GetCurrentScheduleValue(CrossMixing(J)%MaxOutdoorTempSchedPtr)
       If (CrossMixing(J)%MinOutdoorTempSchedPtr > 0 .AND. CrossMixing(J)%MaxOutdoorTempSchedPtr > 0) Then
         If (MixingTmin > MixingTmax) Then
           CrossMixing(J)%OutdoorTempErrCount = CrossMixing(J)%OutdoorTempErrCount + 1
           if (CrossMixing(J)%OutdoorTempErrCount< 2) then
             CALL ShowWarningError('CrossMixing outdoor temperature control: The minimum outdoor temperature is above '// &
               'the maximum outdoor temperature in '//TRIM(Mixing(J)%Name))
             CALL ShowContinueError('The minimum outdoor temperature is set to the maximum source temperature. ' &
                                   //'Simulation continues.')
             CALL ShowContinueErrorTimeStamp(' Occurrence info: ')
           else
             CALL ShowRecurringWarningErrorAtEnd('The minimum outdoor temperature is still above '// &
               'the maximum outdoor temperature',CrossMixing(J)%OutdoorTempErrIndex, MixingTmin, MixingTmin)
           end if
           MixingTmin = MixingTmax
         End If
       End If
       If (CrossMixing(J)%MinOutdoorTempSchedPtr > 0) Then
         If (TempExt < MixingTmin) MixingLimitFlag = .TRUE.
       End If
       If (CrossMixing(J)%MaxOutdoorTempSchedPtr > 0) Then
         If (TempExt > MixingTmax) MixingLimitFlag = .TRUE.
       End If
       If (MixingLimitFlag) Cycle

       IF ( ( TD .EQ. 0.0d0 .OR. ( TD .GT. 0.0d0 .AND. (TZM-TZN) .GE. TD) ) ) THEN
         CrossMixingReportFlag(J) = .TRUE. ! set reporting flag
       END IF

       IF ( ( TD .LE. 0.0d0 .AND. (.NOT. CrossMixingFlag(N) .AND. .NOT. CrossMixingFlag(M) ) &
            .OR. ( TD .GT. 0.0d0 .AND. (TZM-TZN) .GE. TD) ) ) THEN
!                                      SET COEFFICIENTS .
         CrossMixingFlag(N) = .TRUE.
         CrossMixingFlag(M) = .TRUE.

         Tavg = (tzn+tzm)/2.0d0
         Wavg = (ZHumRat(n)+ZHumRat(m))/2.0d0
         AirDensity = PsyRhoAirFnPbTdbW(OutBaroPress,Tavg,Wavg,calledfrom='CalcAirFlowSimple:CrossMixing')
         CpAir = PsyCpAirFnWTdb(Wavg,Tavg,calledfrom='CalcAirFlowSimple:CrossMixing')
         MCPxN=MVFC(J) * CpAir * AirDensity
         MCPM(N)=MCPM(N)+MCPxN

         MCPxM=MVFC(J) * CpAir * AirDensity
         MCPM(M)=MCPM(M)+MCPxM
         MCPTM(N)=MCPTM(N)+MCPxM*TZM
         MCPTM(M)=MCPTM(M)+MCPxN*TZN

         ! Now to determine the moisture conditions
         MixingMassFlowZone(M) = MixingMassFlowZone(M) + MVFC(J)*AirDensity
         MixingMassFlowXHumRat(M) = MixingMassFlowXHumRat(M) + MVFC(J) * AirDensity * ZHumRat(n)

         MixingMassFlowZone(N) = MixingMassFlowZone(N) + MVFC(J)*AirDensity
         MixingMassFlowXHumRat(N) = MixingMassFlowXHumRat(N) + MVFC(J)*AirDensity*ZHumRat(m)
         IF (Contaminant%CO2Simulation) Then
           MixingMassFlowCO2(M) = MixingMassFlowCO2(M) + MVFC(J) * AirDensity * ZoneAirCO2(n)
           MixingMassFlowCO2(N) = MixingMassFlowCO2(N) + MVFC(J) * AirDensity * ZoneAirCO2(m)
         END IF
         IF (Contaminant%GenericContamSimulation) Then
           MixingMassFlowGC(M) = MixingMassFlowGC(M) + MVFC(J) * AirDensity * ZoneAirGC(n)
           MixingMassFlowGC(N) = MixingMassFlowGC(N) + MVFC(J) * AirDensity * ZoneAirGC(m)
         END IF
       END IF
     END IF
   END DO

!                              COMPUTE REFRIGERATION DOOR
!                              AIR MIXING
IF(TotRefDoorMixing > 0) THEN
  !Zone loops structured in getinput so only do each pair of zones bounding door once, even if multiple doors in one zone
  DO ZoneA=1,(NumofZones - 1)
    IF(.NOT. RefDoorMixing(ZoneA)%RefDoorMixFlag)CYCLE
    DO J=1,RefDoorMixing(ZoneA)%NumRefDoorConnections
       ZoneB = RefDoorMixing(ZoneA)%MateZonePtr(J)
       TZoneA=ZMAT(ZoneA)
       TZoneB=ZMAT(ZoneB)
       HumRatZoneA=ZHumRat(ZoneA)
       HumRatZoneB=ZHumRat(ZoneB)
       AirDensityZoneA = PsyRhoAirFnPbTdbW(OutBaroPress,TZoneA,HumRatZoneA,calledfrom='CalcAirFlowSimple:RefrigerationDoorMixing')
       CpAirZoneA = PsyCpAirFnWTdb(HumRatZoneA,TZoneA)
       AirDensityZoneB = PsyRhoAirFnPbTdbW(OutBaroPress,TZoneB,HumRatZoneB,calledfrom='CalcAirFlowSimple:RefrigerationDoorMixing')
       CpAirZoneB = PsyCpAirFnWTdb(HumRatZoneB,TZoneB,calledfrom='CalcAirFlowSimple:RefrigerationDoorMixing')
       Tavg = (TZoneA + TZoneB)/2.0d0
       Wavg = (HumRatZoneA + HumRatZoneB)/2.0d0
       AirDensityAvg = PsyRhoAirFnPbTdbW(OutBaroPress,Tavg,Wavg,calledfrom='CalcAirFlowSimple:RefrigerationDoorMixing')

       IF(RefDoorMixing(ZoneA)%EMSRefDoorMixingOn(J)) THEN
         MassFlowDryAir = RefDoorMixing(ZoneA)%VolRefDoorFlowRate(J) * AirDensityAvg
       ELSE
         SchedDoorOpen = GetCurrentScheduleValue(RefDoorMixing(ZoneA)%OpenSchedPtr(J))
         IF(SchedDoorOpen == 0.d0)  CYCLE
         DoorHeight = RefDoorMixing(ZoneA)%DoorHeight(J)
         DoorArea   = RefDoorMixing(ZoneA)%DoorArea(J)
         DoorProt   = RefDoorMixing(ZoneA)%Protection(J)
         IF(AirDensityZoneA .GE. AirDensityZoneB) THEN
           ! Mass of dry air flow between zones is equal,
           ! but have to calc directionally to avoid sqrt(neg number)
           FDens = (2.d0/(1.d0+((AirDensityZoneA/AirDensityZoneB)**(1.d0/3.d0))))**1.5d0
           FB    = 0.221d0 * DoorArea * AirDensityZoneA * FDens * &
                   SQRT((1.d0-AirDensityZoneB/AirDensityZoneA)*StdGravity*DoorHeight)
         ELSE !ZoneADens < ZoneBDens
           FDens = (2.d0/(1.d0+((AirDensityZoneB/AirDensityZoneA)**(1.d0/3.d0))))**1.5d0
           FB    = 0.221d0 * DoorArea * AirDensityZoneB * FDens * &
                   SQRT((1.d0-AirDensityZoneA/AirDensityZoneB)*StdGravity*DoorHeight)
         END IF !ZoneADens .GE. ZoneBDens
         ! FFlow = Doorway flow factor, is determined by temperature difference
         FFlow = 1.1d0
         IF(ABS(TZoneA - TZoneB) > 11.d0)FFlow = 0.8d0
         MassFlowDryAir = FB *SchedDoorOpen * FFlow * (1.d0 - DoorProt)
         RefDoorMixing(ZoneA)%VolRefDoorFlowRate(J) = MassFlowDryAir/AirDensityAvg
         !Note - VolRefDoorFlowRate is used ONLY for reporting purposes, where it is
         !       used with the avg density to generate a reported mass flow
         !       Considering the small values typical for HumRat, this is not far off.
       END IF ! EMSRefDoorMixingOn

       MassFlowToA = MassFlowDryAir * (1.d0 + HumRatZoneB)
       MassFlowToB = MassFlowDryAir * (1.d0 + HumRatZoneA)
       MassFlowXCpToA = MassFlowToA * CpAirZoneB
       MassFlowXCpToB = MassFlowToB * CpAirZoneA
       MassFlowXCpXTempToA = MassFlowXCpToA * TZoneB
       MassFlowXCpXTempToB = MassFlowXCpToB * TZoneA
       MassFlowXHumRatToA  = MassFlowToA * HumRatZoneB
       MassFlowXHumRatToB  = MassFlowToB * HumRatZoneA

       MCPM(ZoneA)  = MCPM(ZoneA) + MassFlowXCpToA
       MCPM(ZoneB)  = MCPM(ZoneB) + MassFlowXCpToB
       MCPTM(ZoneA) = MCPTM(ZoneA)+ MassFlowXCpXTempToA
       MCPTM(ZoneB) = MCPTM(ZoneB)+ MassFlowXCpXTempToB

       ! Now to determine the moisture conditions
       MixingMassFlowZone(ZoneA) = MixingMassFlowZone(ZoneA) + MassFlowToA
       MixingMassFlowZone(ZoneB) = MixingMassFlowZone(ZoneB) + MassFlowToB
       MixingMassFlowXHumRat(ZoneA) = MixingMassFlowXHumRat(ZoneA) + MassFlowXHumRatToA
       MixingMassFlowXHumRat(ZoneB) = MixingMassFlowXHumRat(ZoneB) + MassFlowXHumRatToB

       ! Now to determine the CO2 and generic contaminant conditions
       IF (Contaminant%CO2Simulation) Then
         MixingMassFlowCO2(ZoneA) = MixingMassFlowCO2(ZoneA) + MassFlowToA * ZoneAirCO2(ZoneB)
         MixingMassFlowCO2(ZoneB) = MixingMassFlowCO2(ZoneB) + MassFlowToB * ZoneAirCO2(ZoneA)
       END IF
       IF (Contaminant%GenericContamSimulation) Then
         MixingMassFlowCO2(ZoneA) = MixingMassFlowCO2(ZoneA) + MassFlowToA * ZoneAirGC(ZoneB)
         MixingMassFlowCO2(ZoneB) = MixingMassFlowCO2(ZoneB) + MassFlowToB * ZoneAirGC(ZoneA)
       END IF

     END DO ! J=1,RefDoorMixing(ZoneA)%NumRefDoorConnections
  END DO !ZoneA=1,(NumofZones - 1)
END IF !(TotRefrigerationDoorMixing > 0) THEN

   ! Process the scheduled Infiltration for air heat balance depending on model type
   DO J=1,TotInfiltration

     NZ=Infiltration(J)%ZonePtr

     TempExt = Zone(NZ)%OutDryBulbTemp
     WindExt = Zone(NZ)%WindSpeed
     AirDensity = PsyRhoAirFnPbTdbW(OutBaroPress,TempExt,OutHumRat,calledfrom='CalcAirFlowSimple:Infiltration')
     CpAir = PsyCpAirFnWTdb(OutHumRat,TempExt)
  !CR7751  should maybe use code below, indoor conditions instead of outdoor conditions
  !   AirDensity = PsyRhoAirFnPbTdbW(OutBaroPress, ZMAT(NZ), ZHumRat(NZ))
  !   CpAir = PsyCpAirFnWTdb(ZHumRat(NZ),ZMAT(NZ))
     Select Case (Infiltration(J)%ModelType)

     CASE (InfiltrationDesignFlowRate)

       IVF=Infiltration(J)%DesignLevel*GetCurrentScheduleValue(Infiltration(J)%SchedPtr)
       ! CR6845 if calculated < 0.0, don't propagate
       IF (IVF < 0.0d0) IVF=0.0D0
       MCpI_temp=IVF*AirDensity*CpAir*( Infiltration(J)%ConstantTermCoef &
                 + ABS(TempExt-ZMAT(NZ))*Infiltration(J)%TemperatureTermCoef &
                 + WindExt*(Infiltration(J)%VelocityTermCoef + WindExt*Infiltration(J)%VelocitySQTermCoef) )

       IF (MCpI_temp < 0.0D0) MCpI_temp=0.0D0
     CASE (InfiltrationShermanGrimsrud)
       ! Sherman Grimsrud model as formulated in ASHRAE HoF
       WindExt = WindSpeed ! formulated to use wind at Meterological Station rather than local
       IVF=GetCurrentScheduleValue(Infiltration(J)%SchedPtr) &
           * Infiltration(J)%LeakageArea / 1000.0D0          &
           * SQRT( Infiltration(J)%BasicStackCoefficient * ABS(TempExt-ZMAT(NZ))  &
                  + Infiltration(J)%BasicWindCoefficient * WindExt**2  )
       IF (IVF < 0.0D0) IVF=0.0D0
       MCpI_temp=IVF*AirDensity*CpAir
       IF (MCpI_temp < 0.0d0) MCpI_temp=0.0D0
     CASE (InfiltrationAIM2)
       ! Walker Wilson model as formulated in ASHRAE HoF
       IVF=GetCurrentScheduleValue(Infiltration(J)%SchedPtr) &
           * SQRT(  ( Infiltration(J)%FlowCoefficient * Infiltration(J)%AIM2StackCoefficient &
                      * (ABS(TempExt-ZMAT(NZ)) )**Infiltration(J)%PressureExponent)**2       &
                      + (Infiltration(J)%FlowCoefficient * Infiltration(J)%AIM2WindCoefficient &
                         * (Infiltration(J)%ShelterFactor * WindExt)**(2.0D0*Infiltration(J)%PressureExponent) )**2 )
       IF (IVF < 0.0D0) IVF=0.0D0
       MCpI_temp=IVF*AirDensity*CpAir
       IF (MCpI_temp < 0.0d0) MCpI_temp=0.0d0
     END SELECT

     IF (Infiltration(J)%EMSOverrideOn) THEN
       IVF= Infiltration(J)%EMSAirFlowRateValue
       IF (IVF < 0.0D0) IVF=0.0D0
       MCpI_temp=IVF*AirDensity*CpAir
       IF (MCpI_temp < 0.0D0) MCpI_temp=0.0D0
     ENDIF

     If (Infiltration(J)%QuadratureSum) Then
       ZoneAirBalance(Infiltration(J)%OABalancePtr)%InfMassFlowRate = &
         ZoneAirBalance(Infiltration(J)%OABalancePtr)%InfMassFlowRate + MCpI_temp/CpAir
     Else
       MCPI(NZ) = MCPI(NZ)+MCpI_temp
       OAMFL(NZ)=OAMFL(NZ)+MCpI_temp/CpAir
       MCPTI(NZ)=MCPTI(NZ)+MCpI_temp*TempExt
     End If

   ENDDO

      ! Add infiltration rate enhanced by the existence of thermal chimney
   DO NZ=1,NumOfZones
     MCPI(NZ)= MCPI(NZ) + MCPThermChim(NZ)
     OAMFL(NZ)= OAMFL(NZ) + ThermChimAMFL(NZ)
     MCPTI(NZ)= MCPTI(NZ) + MCPTThermChim(NZ)
   END DO

   ! Calculate combined outdoor air flows
   Do J =1, TotZoneAirBalance
     IF (ZoneAirBalance(J)%BalanceMethod== AirBalanceQuadrature) THEN
       IF (.NOT. ZoneAirBalance(j)%OneTimeFlag)  Call GetStandAloneERVNodes(J)
       If (ZoneAirBalance(J)%NumOfERVs > 0) Then
         Do I=1,ZoneAirBalance(j)%NumOfERVs
           MassFlowDiff = Node(ZoneAirBalance(j)%ERVExhaustNode(I))%MassFlowRate - &
                          Node(ZoneAirBalance(j)%ERVInletNode(I))%MassFlowRate
           If (MassFlowDiff > 0.d0) Then
             ZoneAirBalance(J)%ERVMassFlowRate = ZoneAirBalance(J)%ERVMassFlowRate + MassFlowDiff
           End If
         End Do
       End If
       NZ = ZoneAirBalance(j)%ZonePtr
       AirDensity = PsyRhoAirFnPbTdbW(OutBaroPress,Zone(NZ)%OutDryBulbTemp,OutHumRat,calledfrom='CalcAirFlowSimple:ZoneAirBalance')
       CpAir = PsyCpAirFnWTdb(OutHumRat,Zone(NZ)%OutDryBulbTemp)
       ZoneAirBalance(J)%ERVMassFlowRate = AirDensity*ZoneAirBalance(J)%ERVMassFlowRate
       MdotOA(NZ) = SQRT((ZoneAirBalance(j)%NatMassFlowRate)**2 + (ZoneAirBalance(j)%IntMassFlowRate)**2 + &
         (ZoneAirBalance(j)%ExhMassFlowRate)**2 + (ZoneAirBalance(j)%ERVMassFlowRate)**2 + &
         (ZoneAirBalance(j)%InfMassFlowRate)**2 +  &
         (AirDensity*ZoneAirBalance(j)%InducedAirRate*GetCurrentScheduleValue(ZoneAirBalance(J)%InducedAirSchedPtr))**2) + &
         ZoneAirBalance(j)%BalMassFlowRate
       MdotCPOA(NZ) = MdotOA(NZ)*CpAir
     END IF
   End Do

  RETURN

END SUBROUTINE CalcAirFlowSimple

SUBROUTINE ReportAirHeatBalance

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   July 2000
          !       MODIFIED       Shirey, Jan 2008 (MIXING/CROSS MIXING outputs)
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine updates the report variables for the AirHeatBalance.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataGlobals, ONLY: SecInHour
  USE DataEnvironment, ONLY: StdBaroPress, OutBaroPress, OutHumRat, StdRhoAir
  USE DataHeatBalance,   ONLY: Zone, TotVentilation, Ventilation, ZnAirRpt, TotMixing, TotCrossMixing, Mixing, CrossMixing, MVFC, &
                               TotZoneAirBalance, ZoneAirBalance, AirBalanceQuadrature, TotRefDoorMixing, RefDoorMixing
  USE DataHVACGlobals, ONLY: CycleOn, CycleOnZoneFansOnly
  USE DataHeatBalFanSys, ONLY: MCPI, MCPV, MdotOA, MdotCPOA !, MCPTI, MCPTV, MCPM, MCPTM, MixingMassFlowZone
  USE Psychrometrics, ONLY:PsyRhoAirFnPbTdbW,PsyCpAirFnWTdb,PsyHgAirFnWTdb

  USE AirflowNetworkBalanceManager, ONLY: ReportAirflowNetwork
  USE DataAirflowNetwork, ONLY: SimulateAirflowNetwork,AirflowNetworkZoneFlag,AirflowNetworkControlSimple, &
                                AirflowNetworkControlSimpleADS
  USE DataZoneEquipment, ONLY: ZoneEquipAvail

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
  INTEGER  :: ZoneLoop                ! Counter for the # of zones (nz)
  INTEGER  :: ZoneA                   ! Mated zone number for pair pf zones sharing refrigeration door opening
  INTEGER  :: ZoneB                   ! Mated zone number for pair pf zones sharing refrigeration door opening
  INTEGER  :: VentNum                 ! Counter for ventilation statements
  REAL(r64) ::  AirDensity                 ! Density of air (kg/m^3)
  REAL(r64) :: CpAir                       ! Heat capacity of air (J/kg-C)
  REAL(r64) :: ADSCorrectionFactor         ! Correction factor of air flow model values when ADS is simulated
  REAL(r64) :: H2OHtOfVap                  ! Heat of vaporization of air
  REAL(r64) :: TotalLoad                   ! Total loss or gain
  INTEGER  :: MixNum                  ! Counter for MIXING and Cross Mixing statements
  REAL(r64),ALLOCATABLE,DIMENSION(:),SAVE :: MixSenLoad             ! Mixing sensible loss or gain
  REAL(r64),ALLOCATABLE,DIMENSION(:),SAVE :: MixLatLoad             ! Mixing latent loss or gain
  INTEGER  :: J                       ! Index in a do-loop
  INTEGER :: VentZoneNum              ! Number of ventilation object per zone
  REAL(r64) :: VentZoneMassflow       ! Total mass flow rate per zone
  REAL(r64) :: VentZoneAirTemp        ! Average Zone inlet temperature
  LOGICAL, SAVE :: FirstTime=.true.

  ! Ensure no airflownetwork and simple calculations
  IF (SimulateAirflowNetwork .eq. 0) RETURN

  IF (SimulateAirflowNetwork .GT. AirflowNetworkControlSimple) CALL ReportAirflowNetwork

  ! Report results for SIMPLE option only
  IF (.NOT. (SimulateAirflowNetwork .EQ. AirflowNetworkControlSimple .OR. &
             SimulateAirflowNetwork .EQ. AirflowNetworkControlSimpleADS)) RETURN

  IF (FirstTime) THEN
    ALLOCATE(MixSenLoad(NumOfZones))
    ALLOCATE(MixLatLoad(NumOfZones))
    FirstTime=.false.
  END IF

  DO ZoneLoop = 1, NumOfZones   ! Start of zone loads report variable update loop ...

          ! Break the infiltration load into heat gain and loss components
    ADSCorrectionFactor = 1.0d0

    IF (SimulateAirflowNetwork .EQ. AirflowNetworkControlSimpleADS) THEN
! CR7608 IF (TurnFansOn .AND. AirflowNetworkZoneFlag(ZoneLoop)) ADSCorrectionFactor=0
       IF ((ZoneEquipAvail(ZoneLoop).EQ.CycleOn .OR. &
           ZoneEquipAvail(ZoneLoop).EQ.CycleOnZoneFansOnly) .AND. &
           AirflowNetworkZoneFlag(ZoneLoop)) ADSCorrectionFactor=0
    END IF

    IF (MAT(ZoneLoop) > Zone(ZoneLoop)%OutDryBulbTemp) THEN

      ZnAirRPT(ZoneLoop)%InfilHeatLoss = 0.001d0*MCPI(ZoneLoop)*(MAT(ZoneLoop)-Zone(ZoneLoop)%OutDryBulbTemp)* &
                                      TimeStepSys*SecInHour*1000.0d0*ADSCorrectionFactor
      ZnAirRPT(ZoneLoop)%InfilHeatGain=0.0d0

    ELSE IF (MAT(ZoneLoop) <= Zone(ZoneLoop)%OutDryBulbTemp) THEN

      ZnAirRPT(ZoneLoop)%InfilHeatGain = 0.001d0*MCPI(ZoneLoop)*(Zone(ZoneLoop)%OutDryBulbTemp-MAT(ZoneLoop))* &
                                      TimeStepSys*SecInHour*1000.0d0*ADSCorrectionFactor
      ZnAirRPT(ZoneLoop)%InfilHeatLoss =0.0d0

    END IF
    ! Report infiltration latent gains and losses
    CpAir = PsyCpAirFnWTdb(OutHumRat,Zone(ZoneLoop)%OutDryBulbTemp,calledfrom='ReportAirHeatBalance')
    H2OHtOfVap = PsyHgAirFnWTdb(OutHumRat, Zone(ZoneLoop)%OutDryBulbTemp,calledfrom='ReportAirHeatBalance:1')
    IF (ZoneAirHumRat(ZoneLoop) > OutHumRat) THEN

      ZnAirRPT(ZoneLoop)%InfilLatentLoss = 0.001d0*MCPI(ZoneLoop)/CpAir*(ZoneAirHumRat(ZoneLoop)-OutHumRat)*H2OHtOfVap* &
                                      TimeStepSys*SecInHour*1000.0d0*ADSCorrectionFactor
      ZnAirRPT(ZoneLoop)%InfilLatentGain=0.0d0

    ELSE IF (ZoneAirHumRat(ZoneLoop) <= OutHumRat) THEN

      ZnAirRPT(ZoneLoop)%InfilLatentGain = 0.001d0*MCPI(ZoneLoop)/CpAir*(OutHumRat-ZoneAirHumRat(ZoneLoop))*H2OHtOfVap* &
                                      TimeStepSys*SecInHour*1000.0d0*ADSCorrectionFactor
      ZnAirRPT(ZoneLoop)%InfilLatentLoss =0.0d0

    END IF
    ! Total infiltration losses and gains
    TotalLoad = ZnAirRPT(ZoneLoop)%InfilHeatGain + ZnAirRPT(ZoneLoop)%InfilLatentGain - &
                ZnAirRPT(ZoneLoop)%InfilHeatLoss - ZnAirRPT(ZoneLoop)%InfilLatentLoss
    IF (TotalLoad > 0) THEN
      ZnAirRPT(ZoneLoop)%InfilTotalGain = TotalLoad*ADSCorrectionFactor
      ZnAirRPT(ZoneLoop)%InfilTotalLoss = 0.0d0
    ELSE
      ZnAirRPT(ZoneLoop)%InfilTotalGain = 0.0d0
      ZnAirRPT(ZoneLoop)%InfilTotalLoss = -TotalLoad*ADSCorrectionFactor
    END IF

  ! first calculate mass flows using outside air heat capacity for consistency with input to heat balance
    CpAir     = PsyCpAirFnWTdb(OutHumRat,Zone(ZoneLoop)%OutDryBulbTemp,calledfrom='ReportAirHeatBalance:2')
    ZnAirRpt(ZoneLoop)%InfilMass                = (MCPI(ZoneLoop)/CpAir)*TimeStepSys*SecInHour*ADSCorrectionFactor
    ZnAirRpt(ZoneLoop)%VentilMass               = (MCPV(ZoneLoop)/CpAir)*TimeStepSys*SecInHour*ADSCorrectionFactor

  !CR7751  second, calculate using indoor conditions for density property
    AirDensity = PsyRhoAirFnPbTdbW(OutBaroPress, MAT(ZoneLoop), ZoneAirHumRatAvg(ZoneLoop),calledfrom='ReportAirHeatBalance:3')
    CpAir      = PsyCpAirFnWTdb(ZoneAirHumRatAvg(ZoneLoop),MAT(ZoneLoop),calledfrom='ReportAirHeatBalance:4')
    ZnAirRpt(ZoneLoop)%InfilVolumeCurDensity    = (MCPI(ZoneLoop)/CpAir/AirDensity)*TimeStepSys*SecInHour*ADSCorrectionFactor
    ZnAirRpt(ZoneLoop)%InfilAirChangeRate       = ZnAirRpt(ZoneLoop)%InfilVolumeCurDensity/(TimeStepSys*Zone(ZoneLoop)%Volume)
    ZnAirRpt(ZoneLoop)%InfilVdotCurDensity      = (MCPI(ZoneLoop)/CpAir/AirDensity)*ADSCorrectionFactor
    ZnAirRpt(ZoneLoop)%VentilVolumeCurDensity   = (MCPV(ZoneLoop)/CpAir/AirDensity)*TimeStepSys*SecInHour*ADSCorrectionFactor
    ZnAirRpt(ZoneLoop)%VentilAirChangeRate      =  ZnAirRpt(ZoneLoop)%VentilVolumeCurDensity/(TimeStepSys*Zone(ZoneLoop)%Volume)
    ZnAirRpt(ZoneLoop)%VentilVdotCurDensity     = (MCPV(ZoneLoop)/CpAir/AirDensity)*ADSCorrectionFactor

  !CR7751 third, calculate using standard dry air at nominal elevation
    AirDensity = StdRhoAir
    ZnAirRpt(ZoneLoop)%InfilVolumeStdDensity    = (MCPI(ZoneLoop)/CpAir/AirDensity)*TimeStepSys*SecInHour*ADSCorrectionFactor
    ZnAirRpt(ZoneLoop)%InfilVdotStdDensity      = (MCPI(ZoneLoop)/CpAir/AirDensity)*ADSCorrectionFactor
    ZnAirRpt(ZoneLoop)%VentilVolumeStdDensity   = (MCPV(ZoneLoop)/CpAir/AirDensity)*TimeStepSys*SecInHour*ADSCorrectionFactor
    ZnAirRpt(ZoneLoop)%VentilVdotStdDensity     = (MCPV(ZoneLoop)/CpAir/AirDensity)*ADSCorrectionFactor

!    ZnAirRpt(ZoneLoop)%VentilFanElec = 0.0
    ZnAirRpt(ZoneLoop)%VentilAirTemp = 0.0d0
    ZnAirRpt(ZoneLoop)%VentilHeatLoss = 0.0d0
    ZnAirRPT(ZoneLoop)%VentilHeatGain =0.0d0
    VentZoneNum = 0
    VentZoneMassflow = 0.0d0
    VentZoneAirTemp = 0.0d0

    DO VentNum = 1, TotVentilation
      IF (Ventilation(VentNum)%ZonePtr == ZoneLoop) THEN
! moved into CalcAirFlowSimple
!        ZnAirRpt(ZoneLoop)%VentilFanElec  = ZnAirRpt(ZoneLoop)%VentilFanElec+Ventilation(VentNum)%FanPower*TimeStepSys*SecInHour &
!          *ADSCorrectionFactor
        IF (ADSCorrectionFactor > 0) THEN
           ZnAirRpt(ZoneLoop)%VentilAirTemp  = ZnAirRpt(ZoneLoop)%VentilAirTemp+Ventilation(VentNum)%AirTemp*VentMCP(VentNum)
           VentZoneMassflow = VentZoneMassflow+VentMCP(VentNum)
           VentZoneAirTemp = VentZoneAirTemp + Ventilation(VentNum)%AirTemp
        ELSE
           ZnAirRpt(ZoneLoop)%VentilAirTemp  = Zone(ZoneLoop)%OutDryBulbTemp
        END IF
        ! Break the ventilation load into heat gain and loss components
        IF (MAT(ZoneLoop) > Ventilation(VentNum)%AirTemp) THEN
          ZnAirRpt(ZoneLoop)%VentilHeatLoss = ZnAirRpt(ZoneLoop)%VentilHeatLoss+VentMCP(VentNum)*(MAT(ZoneLoop)- &
                                           Ventilation(VentNum)%AirTemp)*TimeStepSys*SecInHour*ADSCorrectionFactor
        ELSE IF (MAT(ZoneLoop) <= Ventilation(VentNum)%AirTemp) THEN
          ZnAirRpt(ZoneLoop)%VentilHeatGain = ZnAirRpt(ZoneLoop)%VentilHeatGain+VentMCP(VentNum)* &
                       (Ventilation(VentNum)%AirTemp-MAT(ZoneLoop))*TimeStepSys*SecInHour*ADSCorrectionFactor
        END IF

        VentZoneNum = VentZoneNum+1
        IF (VentZoneNum > 1) CYCLE

        ! Report ventilation latent gains and losses
        H2OHtOfVap = PsyHgAirFnWTdb(OutHumRat, Zone(ZoneLoop)%OutDryBulbTemp,calledfrom='ReportAirHeatBalance:5')
        IF (ZoneAirHumRat(ZoneLoop) > OutHumRat) THEN
          ZnAirRPT(ZoneLoop)%VentilLatentLoss = 0.001d0*MCPV(ZoneLoop)/CpAir*(ZoneAirHumRat(ZoneLoop)-OutHumRat)*H2OHtOfVap* &
                                        TimeStepSys*SecInHour*1000.0d0*ADSCorrectionFactor
          ZnAirRPT(ZoneLoop)%VentilLatentGain=0.0d0
        ELSE IF (ZoneAirHumRat(ZoneLoop) <= OutHumRat) THEN
          ZnAirRPT(ZoneLoop)%VentilLatentGain = 0.001d0*MCPV(ZoneLoop)/CpAir*(OutHumRat-ZoneAirHumRat(ZoneLoop))*H2OHtOfVap* &
                                        TimeStepSys*SecInHour*1000.0d0*ADSCorrectionFactor
          ZnAirRPT(ZoneLoop)%VentilLatentLoss =0.0d0
        END IF
        ! Total ventilation losses and gains
        TotalLoad = ZnAirRPT(ZoneLoop)%VentilHeatGain + ZnAirRPT(ZoneLoop)%VentilLatentGain - &
                    ZnAirRPT(ZoneLoop)%VentilHeatLoss - ZnAirRPT(ZoneLoop)%VentilLatentLoss
        IF (TotalLoad > 0) THEN
          ZnAirRPT(ZoneLoop)%VentilTotalGain = TotalLoad*ADSCorrectionFactor
          ZnAirRPT(ZoneLoop)%VentilTotalLoss = 0.0d0
        ELSE
          ZnAirRPT(ZoneLoop)%VentilTotalGain = 0.0d0
          ZnAirRPT(ZoneLoop)%VentilTotalLoss = -TotalLoad*ADSCorrectionFactor
        END IF

      END IF
    END DO

    IF (ADSCorrectionFactor > 0 .AND. VentZoneNum > 1 .AND. VentZoneMassflow > 0.0d0) Then
       ZnAirRpt(ZoneLoop)%VentilAirTemp  = ZnAirRpt(ZoneLoop)%VentilAirTemp/VentZoneMassflow
    ELse If (ADSCorrectionFactor > 0 .AND. VentZoneNum .eq. 1) Then
       ZnAirRpt(ZoneLoop)%VentilAirTemp = VentZoneAirTemp
    Else ! Just in case
       ZnAirRpt(ZoneLoop)%VentilAirTemp  = Zone(ZoneLoop)%OutDryBulbTemp
    End If

    ! Report mixing sensible and latent loads
    MixSenLoad = 0.0d0 ! Initialize arrays to zero before starting to sum
    MixLatLoad = 0.0d0
    ZnAirRpt(ZoneLoop)%MixVolume = 0.0d0 ! zero reported volume prior to summations below
    ZnAirRpt(ZoneLoop)%MixMass = 0.0d0 ! ! zero reported mass prior to summations below
!    MixingLoad = 0.0d0

    DO MixNum=1,TotMixing
      IF ((Mixing(MixNum)%ZonePtr .eq. ZoneLoop) .AND. MixingReportFlag(MixNum)) THEN
!        MixSenLoad(ZoneLoop) = MixSenLoad(ZoneLoop)+MCPM(ZoneLoop)*MAT(Mixing(MixNum)%FromZone)
!        H2OHtOfVap = PsyHgAirFnWTdb(ZoneAirHumRat(ZoneLoop), MAT(ZoneLoop))
!        Per Jan 17, 2008 conference call, agreed to use average conditions for Rho, Cp and Hfg
!           and to recalculate the report variable using end of time step temps and humrats
        AirDensity = PsyRhoAirFnPbTdbW(OutBaroPress, (MAT(ZoneLoop)+MAT(Mixing(MixNum)%FromZone))/2.0d0, &
                                      (ZoneAirHumRat(ZoneLoop)+ZoneAirHumRat(Mixing(MixNum)%FromZone))/2.0d0)
        CpAir      = PsyCpAirFnWTdb((ZoneAirHumRat(ZoneLoop)+ZoneAirHumRat(Mixing(MixNum)%FromZone))/2.0d0,  &
                                     (MAT(ZoneLoop)+MAT(Mixing(MixNum)%FromZone))/2.0d0)
        ZnAirRpt(ZoneLoop)%MixVolume = ZnAirRpt(ZoneLoop)%MixVolume + &
                                       Mixing(MixNum)%DesiredAirFlowRate*TimeStepSys*SecInHour*ADSCorrectionFactor
        ZnAirRpt(ZoneLoop)%MixMass = ZnAirRpt(ZoneLoop)%MixMass + &
                                 Mixing(MixNum)%DesiredAirFlowRate * AirDensity * TimeStepSys * SecInHour * ADSCorrectionFactor
        MixSenLoad(ZoneLoop) = MixSenLoad(ZoneLoop)+Mixing(MixNum)%DesiredAirFlowRate * AirDensity *  CpAir * &
                               (MAT(ZoneLoop) - MAT(Mixing(MixNum)%FromZone))
        H2OHtOfVap = PsyHgAirFnWTdb((ZoneAirHumRat(ZoneLoop)+ZoneAirHumRat(Mixing(MixNum)%FromZone))/2.0d0, &
                                    (MAT(ZoneLoop)+MAT(Mixing(MixNum)%FromZone))/2.0d0,'ReportAirHeatBalance:Mixing')
!        MixLatLoad(ZoneLoop) = MixLatLoad(ZoneLoop)+MixingMassFlowZone(ZoneLoop)*(ZoneAirHumRat(ZoneLoop)- &
!                     ZoneAirHumRat(Mixing(MixNum)%FromZone))*H2OHtOfVap
        MixLatLoad(ZoneLoop) = MixLatLoad(ZoneLoop)+ Mixing(MixNum)%DesiredAirFlowRate * AirDensity * &
                               (ZoneAirHumRat(ZoneLoop)-ZoneAirHumRat(Mixing(MixNum)%FromZone))*H2OHtOfVap
      END IF
    END DO

    DO MixNum=1,TotCrossMixing
      IF ((CrossMixing(MixNum)%ZonePtr .eq. ZoneLoop) .AND. CrossMixingReportFlag(MixNum)) THEN
!        MixSenLoad(ZoneLoop) = MixSenLoad(ZoneLoop)+MCPM(ZoneLoop)*MAT(CrossMixing(MixNum)%FromZone)
!        Per Jan 17, 2008 conference call, agreed to use average conditions for Rho, Cp and Hfg
!           and to recalculate the report variable using end of time step temps and humrats
        AirDensity = PsyRhoAirFnPbTdbW(OutBaroPress, (MAT(ZoneLoop)+MAT(CrossMixing(MixNum)%FromZone))/2.0d0, &
                                      (ZoneAirHumRat(ZoneLoop)+ZoneAirHumRat(CrossMixing(MixNum)%FromZone))/2.0d0)
        CpAir      = PsyCpAirFnWTdb((ZoneAirHumRat(ZoneLoop)+ZoneAirHumRat(CrossMixing(MixNum)%FromZone))/2.0d0,  &
                                     (MAT(ZoneLoop)+MAT(CrossMixing(MixNum)%FromZone))/2.0d0)
        ZnAirRpt(ZoneLoop)%MixVolume = ZnAirRpt(ZoneLoop)%MixVolume + &
                                       MVFC(MixNum)*TimeStepSys*SecInHour*ADSCorrectionFactor
        ZnAirRpt(ZoneLoop)%MixMass = ZnAirRpt(ZoneLoop)%MixMass + &
                                 MVFC(MixNum) * AirDensity * TimeStepSys * SecInHour * ADSCorrectionFactor
        MixSenLoad(ZoneLoop) = MixSenLoad(ZoneLoop)+ MVFC(MixNum) * AirDensity *  CpAir * &
                               (MAT(ZoneLoop) - MAT(CrossMixing(MixNum)%FromZone))
        H2OHtOfVap = PsyHgAirFnWTdb((ZoneAirHumRat(ZoneLoop)+ZoneAirHumRat(CrossMixing(MixNum)%FromZone))/2.0d0, &
                                    (MAT(ZoneLoop)+MAT(CrossMixing(MixNum)%FromZone))/2.0d0,'ReportAirHeatBalance:XMixing')
!       MixLatLoad(ZoneLoop) = MixLatLoad(ZoneLoop)+MixingMassFlowZone(ZoneLoop)*(ZoneAirHumRat(ZoneLoop)- &
!                     ZoneAirHumRat(CrossMixing(MixNum)%FromZone))*H2OHtOfVap
        MixLatLoad(ZoneLoop) = MixLatLoad(ZoneLoop)+ MVFC(MixNum) * AirDensity * &
                               (ZoneAirHumRat(ZoneLoop)-ZoneAirHumRat(CrossMixing(MixNum)%FromZone))*H2OHtOfVap

      END IF
    END DO

IF (TotRefDoorMixing .GT. 0) THEN
  !IF(ZoneLoop .NE. NumofZones)THEN  !Refrigeration Door Mixing
  !Note - do each Pair a Single time, so must do increment reports for both zones
  !       Can't have a pair that has ZoneA zone number = NumofZones because organized
  !       in input with lowest zone # first no matter how input in idf
  IF(RefDoorMixing(ZoneLoop)%RefDoorMixFlag) THEN ! .TRUE. for both zoneA and zoneB
    IF (RefDoorMixing(ZoneLoop)%ZonePtr .EQ. ZoneLoop) THEN
      DO J=1,RefDoorMixing(ZoneLoop)%NumRefDoorConnections
        !    Capture impact when zoneloop is the 'primary zone'
        !    that is, the zone of a pair with the lower zone number
        IF(RefDoorMixing(ZoneLoop)%VolRefDoorFlowRate(J).GT. 0.d0) THEN
          ZoneB = RefDoorMixing(ZoneLoop)%MateZonePtr(J)
          AirDensity = PsyRhoAirFnPbTdbW(OutBaroPress, (MAT(ZoneLoop)+MAT(ZoneB))/2.0d0, &
                                      (ZoneAirHumRat(ZoneLoop)+ZoneAirHumRat(ZoneB))/2.0d0)
          CpAir      = PsyCpAirFnWTdb((ZoneAirHumRat(ZoneLoop)+ZoneAirHumRat(ZoneB))/2.0d0,  &
                                     (MAT(ZoneLoop)+MAT(ZoneB))/2.0d0)
          H2OHtOfVap = PsyHgAirFnWTdb((ZoneAirHumRat(ZoneLoop)+ZoneAirHumRat(ZoneB))/2.0d0, &
                                    (MAT(ZoneLoop)+MAT(ZoneB))/2.0d0,'ReportAirHeatBalance:XMixing')
          ZnAirRpt(ZoneLoop)%MixVolume = ZnAirRpt(ZoneLoop)%MixVolume + &
                                       RefDoorMixing(ZoneLoop)%VolRefDoorFlowRate(J)*&
                                       TimeStepSys*SecInHour*ADSCorrectionFactor
          ZnAirRpt(ZoneLoop)%MixMass = ZnAirRpt(ZoneLoop)%MixMass + &
                                 RefDoorMixing(ZoneLoop)%VolRefDoorFlowRate(J) * &
                                 AirDensity * TimeStepSys * SecInHour * ADSCorrectionFactor
          MixSenLoad(ZoneLoop) = MixSenLoad(ZoneLoop)+ RefDoorMixing(ZoneLoop)%VolRefDoorFlowRate(J) * &
                               AirDensity *  CpAir * (MAT(ZoneLoop) - MAT(ZoneB))
          MixLatLoad(ZoneLoop) = MixLatLoad(ZoneLoop)+ RefDoorMixing(ZoneLoop)%VolRefDoorFlowRate(J) * AirDensity * &
                               (ZoneAirHumRat(ZoneLoop)-ZoneAirHumRat(ZoneB))*H2OHtOfVap
        END IF !flow > 0
      END DO ! J-1, numref connections
    END IF ! zone A (zoneptr = zoneloop)
    DO ZoneA = 1,(ZoneLoop - 1)
      !    Capture impact when zoneloop is the 'mating zone'
      !    that is, the zone of a pair with the higher zone number(matezoneptr = zoneloop)
      IF(RefDoorMixing(ZoneA)%RefDoorMixFlag) THEN
        DO J=1,RefDoorMixing(ZoneA)%NumRefDoorConnections
          IF (RefDoorMixing(ZoneA)%MateZonePtr(J) .EQ. ZoneLoop) THEN
            IF (RefDoorMixing(ZoneA)%VolRefDoorFlowRate(J).GT. 0.d0) THEN
              AirDensity = PsyRhoAirFnPbTdbW(OutBaroPress, (MAT(ZoneLoop)+MAT(ZoneA))/2.0d0, &
                                      (ZoneAirHumRat(ZoneLoop)+ZoneAirHumRat(ZoneA))/2.0d0)
              CpAir      = PsyCpAirFnWTdb((ZoneAirHumRat(ZoneLoop)+ZoneAirHumRat(ZoneA))/2.0d0,  &
                                     (MAT(ZoneLoop)+MAT(ZoneA))/2.0d0)
              H2OHtOfVap = PsyHgAirFnWTdb((ZoneAirHumRat(ZoneLoop)+ZoneAirHumRat(ZoneA))/2.0d0, &
                                    (MAT(ZoneLoop)+MAT(ZoneA))/2.0d0,'ReportAirHeatBalance:XMixing')
              ZnAirRpt(ZoneLoop)%MixVolume = ZnAirRpt(ZoneLoop)%MixVolume + &
                                       RefDoorMixing(ZoneA)%VolRefDoorFlowRate(J)*&
                                       TimeStepSys*SecInHour*ADSCorrectionFactor
              ZnAirRpt(ZoneLoop)%MixMass = ZnAirRpt(ZoneLoop)%MixMass + &
                                 RefDoorMixing(ZoneA)%VolRefDoorFlowRate(J) * &
                                 AirDensity * TimeStepSys * SecInHour * ADSCorrectionFactor
              MixSenLoad(ZoneLoop) = MixSenLoad(ZoneLoop)+ RefDoorMixing(ZoneA)%VolRefDoorFlowRate(J) * &
                                 AirDensity *  CpAir * (MAT(ZoneLoop) - MAT(ZoneA))
              MixLatLoad(ZoneLoop) = MixLatLoad(ZoneLoop)+ RefDoorMixing(ZoneA)%VolRefDoorFlowRate(J) * AirDensity * &
                               (ZoneAirHumRat(ZoneLoop)-ZoneAirHumRat(ZoneA))*H2OHtOfVap
            END IF ! volflowrate > 0
          END IF ! matezoneptr (zoneB) = Zonelooop
        END DO ! NumRefDoorConnections
      END IF ! Refdoormix flag on ZoneA
    END DO ! zone A from 1 to (zoneloop - 1)
  END IF ! Refdoormix flag on zoneloop
END IF !(TotRefDoorMixing .GT. 0)
!end refrigeration door mixing reports


!    MixingLoad(ZoneLoop) = MCPM(ZoneLoop)*MAT(ZoneLoop) - MixSenLoad(ZoneLoop)
    IF (MixSenLoad(ZoneLoop) > 0.0d0) THEN
      ZnAirRpt(ZoneLoop)%MixHeatLoss = MixSenLoad(ZoneLoop)*TimeStepSys*SecInHour*ADSCorrectionFactor
      ZnAirRpt(ZoneLoop)%MixHeatGain = 0.0d0
    ELSE
      ZnAirRpt(ZoneLoop)%MixHeatLoss = 0.0d0
      ZnAirRpt(ZoneLoop)%MixHeatGain = -MixSenLoad(ZoneLoop)*TimeStepSys*SecInHour*ADSCorrectionFactor
    END IF
    ! Report mixing latent loads
!    MixingLoad(ZoneLoop) = MixLatLoad(ZoneLoop)
    IF (MixLatLoad(ZoneLoop) > 0.0d0) THEN
      ZnAirRpt(ZoneLoop)%MixLatentLoss = MixLatLoad(ZoneLoop)*TimeStepSys*SecInHour*ADSCorrectionFactor
      ZnAirRpt(ZoneLoop)%MixLatentGain = 0.0d0
    ELSE
      ZnAirRpt(ZoneLoop)%MixLatentLoss = 0.0d0
      ZnAirRpt(ZoneLoop)%MixLatentGain = -MixLatLoad(ZoneLoop)*TimeStepSys*SecInHour*ADSCorrectionFactor
    END IF
    ! Total Mixing losses and gains
    TotalLoad = ZnAirRPT(ZoneLoop)%MixHeatGain + ZnAirRPT(ZoneLoop)%MixLatentGain - &
                ZnAirRPT(ZoneLoop)%MixHeatLoss - ZnAirRPT(ZoneLoop)%MixLatentLoss
    IF (TotalLoad > 0) THEN
      ZnAirRPT(ZoneLoop)%MixTotalGain = TotalLoad*ADSCorrectionFactor
      ZnAirRPT(ZoneLoop)%MixTotalLoss = 0.0d0
    ELSE
      ZnAirRPT(ZoneLoop)%MixTotalGain = 0.0d0
      ZnAirRPT(ZoneLoop)%MixTotalLoss = -TotalLoad*ADSCorrectionFactor
    END IF

    ! Reporting combined outdoor air flows
    Do J=1, TotZoneAirBalance
      IF (ZoneAirBalance(j)%BalanceMethod== AirBalanceQuadrature .AND. ZoneLoop == ZoneAirBalance(j)%ZonePtr) THEN
        IF (MAT(ZoneLoop) > Zone(ZoneLoop)%OutDryBulbTemp) THEN
          ZnAirRpt(ZoneLoop)%OABalanceHeatLoss = MdotCPOA(ZoneLoop)*(MAT(ZoneLoop)- &
                                           Zone(ZoneLoop)%OutDryBulbTemp)*TimeStepSys*SecInHour*ADSCorrectionFactor
          ZnAirRpt(ZoneLoop)%OABalanceHeatGain = 0.0d0
        ELSE
          ZnAirRpt(ZoneLoop)%OABalanceHeatLoss = 0.0d0
          ZnAirRpt(ZoneLoop)%OABalanceHeatGain = -MdotCPOA(ZoneLoop)*(MAT(ZoneLoop)- &
                                           Zone(ZoneLoop)%OutDryBulbTemp)*TimeStepSys*SecInHour*ADSCorrectionFactor
        END IF
        H2OHtOfVap = PsyHgAirFnWTdb(OutHumRat, Zone(ZoneLoop)%OutDryBulbTemp,'ReportAirHeatBalance:2')
        IF (ZoneAirHumRat(ZoneLoop) > OutHumRat) THEN
          ZnAirRPT(ZoneLoop)%OABalanceLatentLoss = 0.001d0*MdotOA(ZoneLoop)*(ZoneAirHumRat(ZoneLoop)-OutHumRat)*H2OHtOfVap* &
                                        TimeStepSys*SecInHour*1000.0d0*ADSCorrectionFactor
          ZnAirRPT(ZoneLoop)%OABalanceLatentGain=0.0d0
        ELSE IF (ZoneAirHumRat(ZoneLoop) <= OutHumRat) THEN
          ZnAirRPT(ZoneLoop)%OABalanceLatentGain = 0.001d0*MdotOA(ZoneLoop)*(OutHumRat-ZoneAirHumRat(ZoneLoop))*H2OHtOfVap* &
                                        TimeStepSys*SecInHour*1000.0d0*ADSCorrectionFactor
          ZnAirRPT(ZoneLoop)%OABalanceLatentLoss =0.0d0
        END IF
        ! Total ventilation losses and gains
        TotalLoad = ZnAirRPT(ZoneLoop)%OABalanceHeatGain + ZnAirRPT(ZoneLoop)%OABalanceLatentGain - &
                    ZnAirRPT(ZoneLoop)%OABalanceHeatLoss - ZnAirRPT(ZoneLoop)%OABalanceLatentLoss
        IF (TotalLoad > 0) THEN
          ZnAirRPT(ZoneLoop)%OABalanceTotalGain = TotalLoad*ADSCorrectionFactor
          ZnAirRPT(ZoneLoop)%OABalanceTotalLoss = 0.0d0
        ELSE
          ZnAirRPT(ZoneLoop)%OABalanceTotalGain = 0.0d0
          ZnAirRPT(ZoneLoop)%OABalanceTotalLoss = -TotalLoad*ADSCorrectionFactor
        END IF
        ZnAirRpt(ZoneLoop)%OABalanceMass               = (MdotOA(ZoneLoop))*TimeStepSys*SecInHour*ADSCorrectionFactor
        AirDensity = PsyRhoAirFnPbTdbW(OutBaroPress, MAT(ZoneLoop), ZoneAirHumRatAvg(ZoneLoop))
        ZnAirRpt(ZoneLoop)%OABalanceVolumeCurDensity = (MdotOA(ZoneLoop)/AirDensity)*TimeStepSys*SecInHour*ADSCorrectionFactor
        ZnAirRpt(ZoneLoop)%OABalanceAirChangeRate =ZnAirRpt(ZoneLoop)%OABalanceVolumeCurDensity/(TimeStepSys*Zone(ZoneLoop)%Volume)
        ZnAirRpt(ZoneLoop)%OABalanceVdotCurDensity     = (MdotOA(ZoneLoop)/AirDensity)*ADSCorrectionFactor
        AirDensity = StdRhoAir
        ZnAirRpt(ZoneLoop)%OABalanceVolumeStdDensity   = (MdotOA(ZoneLoop)/AirDensity)*TimeStepSys*SecInHour*ADSCorrectionFactor
        ZnAirRpt(ZoneLoop)%OABalanceVdotStdDensity     = (MdotOA(ZoneLoop)/AirDensity)*ADSCorrectionFactor
        ZnAirRpt(ZoneLoop)%OABalanceFanElec = ZnAirRpt(ZoneLoop)%VentilFanElec
      END IF
    End Do

  END DO

  RETURN

END SUBROUTINE ReportAirHeatBalance

SUBROUTINE SetHeatToReturnAirFlag

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   February 2008
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This sets some flags at the air loop and zone level: these flags indicate
          ! whether an air loop represents a "unitary" system, and whether the system is operating
          ! in a on/off (cycling fan) mode. At the zone level flags are set to indicate whether
          ! the zone is served by a zonal system only, and whether the air loop serving the zone (idf any)
          ! is in cycling fan mode. Using this information, the subroutine sets a flag at the zone level
          ! to tell ManageZoneAirUpdates (predict and correct) what to do with the heat to return air.

          ! METHODOLOGY EMPLOYED:
          ! Uses program data structures AirLoopControlInfo and ZoneEquipInfo

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataHVACGlobals,      ONLY : NumPrimaryAirSys
  USE DataZoneEquipment, ONLY : ZoneEquipConfig
  USE DataHeatBalance, ONLY: Zone, Lights, TotLights
  USE ScheduleManager, ONLY: CheckScheduleValue, GetCurrentScheduleValue, GetScheduleMaxValue
  USE DataSurfaces, ONLY: SurfaceWindow, AirFlowWindow_Destination_ReturnAir

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENTS:

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER      :: AirLoopNum=0               ! the air loop index
  INTEGER      :: ControlledZoneNum          ! controlled zone index
  LOGICAL,SAVE :: MyOneTimeFlag = .true.
  LOGICAL      :: CyclingFan = .false.       ! TRUE means air loop operates in cycling fan mode at some point
  INTEGER      :: ZoneNum=0                  ! zone index
  INTEGER      :: LightNum                   ! Lights object index
  INTEGER      :: SurfNum                    ! Surface index
  REAL(r64)    :: CycFanMaxVal = 0.0d0         ! max value of cycling fan schedule

  IF (.not. AirLoopsSimOnce) RETURN

  IF (MyOneTimeFlag) THEN
    ! set the air loop Any Continuous Fan flag
    DO AirLoopNum=1,NumPrimaryAirSys
      IF (AirLoopControlInfo(AirLoopNum)%UnitarySys) THEN ! for unitary systems check the cycling fan schedule
        IF (AirLoopControlInfo(AirLoopNum)%CycFanSchedPtr > 0) THEN
          CycFanMaxVal = GetScheduleMaxValue(AirLoopControlInfo(AirLoopNum)%CycFanSchedPtr)
          IF (CycFanMaxVal > 0.0d0) THEN
            AirLoopControlInfo(AirLoopNum)%AnyContFan = .TRUE.
          ELSE
            AirLoopControlInfo(AirLoopNum)%AnyContFan = .FALSE.
          END IF
        ELSE ! no schedule means always cycling fan
          AirLoopControlInfo(AirLoopNum)%AnyContFan = .FALSE.
        END IF
      ELSE  ! for nonunitary (central) all systems are continuous fan
        AirLoopControlInfo(AirLoopNum)%AnyContFan = .TRUE.
      END IF
    END DO
    ! check to see if a controlled zone is served exclusively by a zonal system
    DO ControlledZoneNum = 1,  NumOfZones
      ZoneNum = ZoneEquipConfig(ControlledZoneNum)%ActualZoneNum
      IF (ZoneEquipConfig(ControlledZoneNum)%AirLoopNum == 0 .AND. &
          ZoneEquipConfig(ControlledZoneNum)%NumInletNodes == ZoneEquipConfig(ControlledZoneNum)%NumExhaustNodes) THEN
        ZoneEquipConfig(ControlledZoneNum)%ZonalSystemOnly = .TRUE.
      END IF
    END DO
    ! issue warning messages if zone is served by a zonal system or a cycling system and the input calls for
    ! heat gain to return air
    DO ControlledZoneNum = 1,  NumOfZones
      IF (.not. ZoneEquipConfig(ControlledZoneNum)%IsControlled) CYCLE
      ZoneNum = ZoneEquipConfig(ControlledZoneNum)%ActualZoneNum
      CyclingFan = .FALSE.
      AirLoopNum = ZoneEquipConfig(ControlledZoneNum)%AirLoopNum
      IF (AirLoopNum > 0) THEN
        IF (AirLoopControlInfo(AirLoopNum)%CycFanSchedPtr > 0) THEN
          CyclingFan = CheckScheduleValue(AirLoopControlInfo(AirLoopNum)%CycFanSchedPtr,0.0d0)
        END IF
      END IF
      IF (ZoneEquipConfig(ControlledZoneNum)%ZonalSystemOnly .OR. CyclingFan) THEN
        IF (Zone(ZoneNum)%RefrigCaseRA) THEN
          CALL ShowWarningError('For zone='//TRIM(Zone(ZoneNum)%Name) // ' return air cooling by refrigerated cases will be' // &
                                ' applied to the zone air.')
          CALL ShowContinueError('  This zone has no return air or is served by an on/off HVAC system.')
        END IF
        DO LightNum=1,TotLights
          IF (Lights(LightNum)%ZonePtr /= ZoneNum) CYCLE
            IF (Lights(LightNum)%FractionReturnAir > 0.0d0) THEN
              CALL ShowWarningError('For zone='//TRIM(Zone(ZoneNum)%Name) // ' return air heat gain from lights will be' // &
                                    ' applied to the zone air.')
              CALL ShowContinueError('  This zone has no return air or is served by an on/off HVAC system.')
              EXIT
            END IF
        ENDDO
        DO SurfNum = Zone(ZoneNum)%SurfaceFirst,Zone(ZoneNum)%SurfaceLast
          IF (SurfaceWindow(SurfNum)%AirflowDestination == AirFlowWindow_Destination_ReturnAir) THEN
            CALL ShowWarningError('For zone='//TRIM(Zone(ZoneNum)%Name)//   &
                    ' return air heat gain from air flow windows will be applied to the zone air.')
            CALL ShowContinueError('  This zone has no return air or is served by an on/off HVAC system.')
          END IF
        END DO
      END IF
    END DO
    MyOneTimeFlag = .FALSE.
  END IF

  ! set the air loop fan operation mode
  DO AirLoopNum=1,NumPrimaryAirSys
    IF (AirLoopControlInfo(AirLoopNum)%CycFanSchedPtr > 0) THEN
      IF (GetCurrentScheduleValue(AirLoopControlInfo(AirLoopNum)%CycFanSchedPtr) .EQ. 0.0d0) THEN
        AirLoopControlInfo(AirLoopNum)%FanOpMode = CycFanCycCoil
      ELSE
        AirLoopControlInfo(AirLoopNum)%FanOpMode = ContFanCycCoil
      END IF
    END IF
  END DO
  ! set the zone level NoHeatToReturnAir flag and the ZoneEquip fan operation mode
  DO ControlledZoneNum = 1,  NumOfZones
    IF (.not. ZoneEquipConfig(ControlledZoneNum)%IsControlled) CYCLE
    ZoneNum = ZoneEquipConfig(ControlledZoneNum)%ActualZoneNum
    AirLoopNum = ZoneEquipConfig(ControlledZoneNum)%AirLoopNum
    IF (AirLoopNum > 0) THEN
      ZoneEquipConfig(ControlledZoneNum)%FanOpMode = AirLoopControlInfo(AirLoopNum)%FanOpMode
    ELSE
      ZoneEquipConfig(ControlledZoneNum)%FanOpMode = 0
    END IF
    IF (ZoneEquipConfig(ControlledZoneNum)%FanOpMode == CycFanCycCoil .or. ZoneEquipConfig(ControlledZoneNum)%ZonalSystemOnly) THEN
      Zone(ZoneNum)%NoHeatToReturnAir = .TRUE.
    ELSE
      Zone(ZoneNum)%NoHeatToReturnAir = .FALSE.
    END IF
  END DO

  RETURN

END SUBROUTINE SetHeatToReturnAirFlag

SUBROUTINE GetStandAloneERVNodes(OutdoorNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Lixing Gu
          !       DATE WRITTEN   July 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine gets node numbers of stand alone ERVs to calculate combined outdoor air flows.

          ! METHODOLOGY EMPLOYED:
          ! Uses program data structures ZoneEquipInfo

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataZoneEquipment, ONLY : ZoneEquipList, ERVStandAlone_Num
  USE DataHeatBalance,   ONLY: ZoneAirBalance, AirBalanceQuadrature
  USE HVACStandAloneERV, ONLY: GetStandAloneERVOutAirNode, GetStandAloneERVReturnAirNode

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENTS:
  INTEGER, INTENT (IN)  :: OutdoorNum   ! Zone Air Balance Outdoor index

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
!  INTEGER      :: ERVNum=0                   ! the stand alone ERV index
  INTEGER      :: ZoneNum=0                  ! zone index
  INTEGER      :: J                          ! index
  INTEGER      :: I                          ! index

  If (ALLOCATED(ZoneEquipList)) Then
    ZoneNum = ZoneAirBalance(OutdoorNum)%ZonePtr
    ZoneAirBalance(OutdoorNum)%OneTimeFlag = .TRUE.
    If (ZoneEquipList(ZoneNum)%NumOfEquipTypes .GT. 0) Then
      Do I=1,ZoneEquipList(ZoneNum)%NumOfEquipTypes
        If (ZoneEquipList(ZoneNum)%EquipType_Num(I) == ERVStandAlone_Num) Then
          ZoneAirBalance(OutdoorNum)%NumOfERVs = ZoneAirBalance(OutdoorNum)%NumOfERVs + 1
        End If
      End Do
      If (ZoneAirBalance(OutdoorNum)%NumOfERVs > 0) Then
        ALLOCATE(ZoneAirBalance(OutdoorNum)%ERVInletNode(ZoneAirBalance(OutdoorNum)%NumOfERVs))
        ALLOCATE(ZoneAirBalance(OutdoorNum)%ERVExhaustNode(ZoneAirBalance(OutdoorNum)%NumOfERVs))
        J = 1
        Do I=1,ZoneEquipList(ZoneNum)%NumOfEquipTypes
          If (ZoneEquipList(ZoneNum)%EquipType_Num(I) == ERVStandAlone_Num) Then
            ZoneAirBalance(OutdoorNum)%ERVInletNode(J) = GetStandAloneERVOutAirNode(ZoneEquipList(ZoneNum)%EquipIndex(I))
            ZoneAirBalance(OutdoorNum)%ERVExhaustNode(J) = GetStandAloneERVReturnAirNode(ZoneEquipList(ZoneNum)%EquipIndex(I))
            J = J+1
          End If
        End Do
      End If
    End If
  End If

  RETURN

END SUBROUTINE GetStandAloneERVNodes

SUBROUTINE UpdateZoneInletConvergenceLog

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         <author>
          !       DATE WRITTEN   <date_written>
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! <description>

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataConvergPArams, ONLY: ZoneInletConvergence, ConvergLogStackDepth
  USE DataLoopNode ,     ONLY: Node
  USE DataGlobals,       ONLY: NumOfZones

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
  INTEGER  :: ZoneNum
  INTEGER  :: ZoneIndex
  INTEGER  :: NodeIndex
  INTEGER  :: NodeNum
  REAL(r64), DIMENSION(ConvergLogStackDepth) :: tmpRealARR

  DO ZoneNum = 1, NumOfZones

    DO NodeIndex = 1, ZoneInletConvergence(ZoneNum)%NumInletNodes
      NodeNum = ZoneInletConvergence(ZoneNum)%InletNode(NodeIndex)%NodeNum

      tmpRealARR = ZoneInletConvergence(ZoneNum)%InletNode(NodeIndex)%HumidityRatio
      ZoneInletConvergence(ZoneNum)%InletNode(NodeIndex)%HumidityRatio(1) = Node(NodeNum)%HumRat
      ZoneInletConvergence(ZoneNum)%InletNode(NodeIndex)%HumidityRatio(2:ConvergLogStackDepth) = &
                     TmpRealARR(1:ConvergLogStackDepth-1)

      tmpRealARR = ZoneInletConvergence(ZoneNum)%InletNode(NodeIndex)%MassFlowRate
      ZoneInletConvergence(ZoneNum)%InletNode(NodeIndex)%MassFlowRate(1) = Node(NodeNum)%MassFlowRate
      ZoneInletConvergence(ZoneNum)%InletNode(NodeIndex)%MassFlowRate(2:ConvergLogStackDepth) = &
                     TmpRealARR(1:ConvergLogStackDepth-1)

      tmpRealARR = ZoneInletConvergence(ZoneNum)%InletNode(NodeIndex)%Temperature
      ZoneInletConvergence(ZoneNum)%InletNode(NodeIndex)%Temperature(1) = Node(NodeNum)%Temp
      ZoneInletConvergence(ZoneNum)%InletNode(NodeIndex)%Temperature(2:ConvergLogStackDepth) = &
                     TmpRealARR(1:ConvergLogStackDepth-1)
    ENDDO
  ENDDO

  RETURN

END SUBROUTINE UpdateZoneInletConvergenceLog



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

END MODULE HVACManager
