MODULE ZoneContaminantPredictorCorrector

          ! MODULE INFORMATION:
          !       AUTHOR         Lixing Gu
          !       DATE WRITTEN   May, 2010
          !       MODIFIED       None
          !       RE-ENGINEERED  None

          ! PURPOSE OF THIS MODULE:
          ! This module contains routines to predict and correct zone contaminants.
          !  also includes zone contaminant controlling
          !

          ! METHODOLOGY EMPLOYED:
          ! Similar apporach to ZoneTempPredictorCorrector
          !

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
USE DataPrecisionGlobals
USE DataGlobals
USE DataHVACGlobals
USE DataHeatBalance
USE DataHeatBalFanSys
USE DataEnvironment, ONLY: OutBaroPress
USE Psychrometrics
USE DataAirflowNetwork,ONLY:SimulateAirflowNetwork,AirflowNetworkExchangeData,AirflowNetworkZoneExhaustFan, &
                            AirflowNetworkNumOfExhFan,AirflowNetworkFanActivated,AirflowNetworkControlMultizone, &
                            AirflowNetworkControlSimpleADS,AirflowNetworkControlMultiADS
USE DataZoneControls
USE DataInterfaces
USE DataContaminantBalance
USE ZoneTempPredictorCorrector, ONLY:   DownInterpolate4HistoryValues
!  iGetZoneSetpoints, iPredictStep, iCorrectStep, &
!                                        iPushZoneTimestepHistories, iRevertZoneTimestepHistories, &
!                                        iPushSystemTimestepHistories, 

IMPLICIT NONE ! Enforce explicit typing of all variables

PRIVATE

          ! MODULE PARAMETER DEFINITIONS:
          ! na


          ! DERIVED TYPE DEFINITIONS:
          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! MODULE VARIABLE DECLARATIONS:

LOGICAL :: GetZoneAirInputFlag = .TRUE.  ! True when need to get input


          ! SUBROUTINE SPECIFICATIONS:
PUBLIC ManageZoneContaminanUpdates
PRIVATE InitZoneContSetpoints
PRIVATE GetZoneContaminanSetpoints ! get input routines for all types of zone controls
PRIVATE CorrectZoneContaminants
PRIVATE PredictZoneContaminants

PRIVATE PushZoneTimestepHistories
PRIVATE PushSystemTimestepHistories
PRIVATE RevertZoneTimestepHistories ! not now being used can remove...

CONTAINS

SUBROUTINE ManageZoneContaminanUpdates(UpdateType, ShortenTimeStepSys,   &
                                UseZoneTimeStepHistory, PriorTimeStep )

          ! SUBROUTINE INFORMATION
          !       AUTHOR         Lixing Gu
          !       DATE WRITTEN   July, 2010
          !       MODIFIED       na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine predicts or corrects the zone air temperature
          ! depending on the simulation status and determines the correct
          ! temperature setpoint for each zone from the schedule manager.
          ! This module is revised from subroutine ManageZoneAirUpdates in
          ! ZoneTempPredictorCorrector module.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: UpdateType             ! Can be iGetZoneSetpoints, iPredictStep, iCorrectStep
  LOGICAL, INTENT(IN) :: ShortenTimeStepSys
  LOGICAL, INTENT(IN) :: UseZoneTimeStepHistory      ! if true then use zone timestep history, if false use system time step
  REAL(r64),    INTENT(IN) :: PriorTimeStep  ! the old value for timestep length is passed for possible use in interpolating

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
!unused1208  INTEGER :: zoneloop

  IF (GetZoneAirInputFlag) THEN
    CALL GetZoneContaminanSetpoints
    GetZoneAirInputFlag = .FALSE.
  END IF

  IF (.NOT. Contaminant%CO2Simulation) Return

  SELECT CASE(UpdateType)

    CASE(iGetZoneSetpoints)
      CALL InitZoneContSetpoints

    CASE(iPredictStep)
      CALL PredictZoneContaminants(ShortenTimeStepSys, UseZoneTimeStepHistory, PriorTimeStep )

    CASE(iCorrectStep)
      CALL CorrectZoneContaminants(ShortenTimeStepSys, UseZoneTimeStepHistory, PriorTimeStep)

    CASE (iRevertZoneTimestepHistories)
      Call RevertZoneTimestepHistories

    CASE (iPushZoneTimestepHistories)
      Call PushZoneTimestepHistories

    CASE (iPushSystemTimestepHistories)
      Call PushSystemTimestepHistories

  END SELECT

  RETURN

END SUBROUTINE ManageZoneContaminanUpdates

SUBROUTINE GetZoneContaminanSetpoints

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Lixing Gu
          !       DATE WRITTEN   May 2010
          !       MODIFIED       NA
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine gets the inputs related to contaminant control.

          ! METHODOLOGY EMPLOYED:
          ! Uses the status flags to trigger events.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataIPShortCuts
  USE InputProcessor
  USE ScheduleManager, ONLY: GetScheduleIndex, CheckScheduleValueMinMax, GetScheduleMinValue, GetScheduleMaxValue,  &
                             CheckScheduleValue
  USE General, ONLY: TrimSigDigits

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:


          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
  TYPE NeededControlTypes
    LOGICAL, DIMENSION(4) :: MustHave=.false.  ! 4= the four control types
    LOGICAL, DIMENSION(4) :: DidHave =.false.
  END TYPE

  TYPE NeededComfortControlTypes
    LOGICAL, DIMENSION(12) :: MustHave=.false.  ! 4= the four control types
    LOGICAL, DIMENSION(12) :: DidHave =.false.
  END TYPE

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: ContControlledZoneNum      ! The Splitter that you are currently loading input into
  INTEGER :: NumAlphas
  INTEGER :: NumNums
  INTEGER :: IOSTAT
!unused1208  REAL(r64), DIMENSION(2) :: NumArray
!unused1208  CHARACTER(len=MaxNameLength), DIMENSION(29) :: AlphArray
  LOGICAL :: ErrorsFound = .FALSE.
  LOGICAL :: IsNotOK               ! Flag to verify name
  LOGICAL :: IsBlank               ! Flag for blank name
  LOGICAL :: ValidScheduleType

  INTEGER, EXTERNAL :: FindNumberInList

          ! FLOW:
  cCurrentModuleObject='ZoneControl:ContaminantController'
  NumContControlledZones = GetNumObjectsFound(trim(cCurrentModuleObject))

  IF (NumContControlledZones .GT. 0) THEN
    ALLOCATE(ContaminantControlledZone(NumContControlledZones))
  ENDIF

  DO ContControlledZoneNum = 1, NumContControlledZones
    CALL GetObjectItem(trim(cCurrentModuleObject),ContControlledZoneNum,cAlphaArgs,NumAlphas,rNumericArgs,NumNums,IOSTAT, &
                       NumBlank=lNumericFieldBlanks,AlphaBlank=lAlphaFieldBlanks, &
                       AlphaFieldNames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
    IsNotOK = .FALSE.
    IsBlank = .FALSE.
    CALL VerifyName(cAlphaArgs(1),ContaminantControlledZone%Name,ContControlledZoneNum-1,IsNotOK,IsBlank, &
                    trim(cCurrentModuleObject)//' Name')
    IF (IsNotOK) THEN
      ErrorsFound = .TRUE.
      IF (IsBlank) cAlphaArgs(1) = 'xxxxx'
    END IF
    ContaminantControlledZone(ContControlledZoneNum)%Name = cAlphaArgs(1)
    ContaminantControlledZone(ContControlledZoneNum)%ZoneName = cAlphaArgs(2)
    ContaminantControlledZone(ContControlledZoneNum)%ActualZoneNum = FindIteminList(cAlphaArgs(2),Zone%Name,NumOfZones)
    IF (ContaminantControlledZone(ContControlledZoneNum)%ActualZoneNum == 0) THEN
      CALL ShowSevereError(trim(cCurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid '//  &
         trim(cAlphaFieldNames(2))//'="'//trim(cAlphaArgs(2))//'" not found.')
      ErrorsFound = .TRUE.
    ELSE
!      Zone(ContaminantControlledZone(ContControlledZoneNum)%ActualZoneNum)%TempControlledZoneIndex = ContControlledZoneNum
    END IF

    ContaminantControlledZone(ContControlledZoneNum)%AvaiSchedule = cAlphaArgs(3)
    ContaminantControlledZone(ContControlledZoneNum)%AvaiSchedPtr=GetScheduleIndex(cAlphaArgs(3))
    IF (ContaminantControlledZone(ContControlledZoneNum)%AvaiSchedPtr == 0) THEN
      CALL ShowSevereError(trim(cCurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid '//  &
         trim(cAlphaFieldNames(3))//'="'//trim(cAlphaArgs(3))//'" not found.')
      ErrorsFound = .TRUE.
    ELSE
      ! Check validity of control types.
      ValidScheduleType=CheckScheduleValueMinMax(ContaminantControlledZone(ContControlledZoneNum)%AvaiSchedPtr,'>=',0.0,'<=',1.0)
      IF (.not. ValidScheduleType) THEN
        CALL ShowSevereError(trim(cCurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid range '//  &
           trim(cAlphaFieldNames(3))//'="'//trim(cAlphaArgs(3))//'"')
        CALL ShowContinueError('..contains values outside of range [0,1].')
        ErrorsFound=.TRUE.
      ELSE
        Zone(ContaminantControlledZone(ContControlledZoneNum)%ActualZoneNum)%ZoneContamControllerSchedIndex = &
                                         ContaminantControlledZone(ContControlledZoneNum)%AvaiSchedPtr
      ENDIF
    END IF

    ContaminantControlledZone(ContControlledZoneNum)%SetPointSchedName = cAlphaArgs(4)
    ContaminantControlledZone(ContControlledZoneNum)%SPSchedIndex=GetScheduleIndex(cAlphaArgs(4))
    IF (ContaminantControlledZone(ContControlledZoneNum)%SPSchedIndex == 0) THEN
      CALL ShowSevereError(trim(cCurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid '//  &
         trim(cAlphaFieldNames(4))//'="'//trim(cAlphaArgs(4))//'" not found.')
      ErrorsFound = .TRUE.
    ELSE
      ! Check validity of control types.
      ValidScheduleType=CheckScheduleValueMinMax(ContaminantControlledZone(ContControlledZoneNum)%SPSchedIndex,'>=',0.0,  &
                                                       '<=',2000.0)
      IF (.not. ValidScheduleType) THEN
        CALL ShowSevereError(trim(cCurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid range '//  &
           trim(cAlphaFieldNames(4))//'="'//trim(cAlphaArgs(4))//'"')
        CALL ShowContinueError('..contains values outside of range [0,2000 ppm].')
        ErrorsFound=.TRUE.
      ENDIF
    END IF

    ContaminantControlledZone(ContControlledZoneNum)%ZoneMinCO2SchedName = cAlphaArgs(5)
    ContaminantControlledZone(ContControlledZoneNum)%ZoneMinCO2SchedIndex=GetScheduleIndex(cAlphaArgs(5))
    IF (ContaminantControlledZone(ContControlledZoneNum)%ZoneMinCO2SchedIndex .GT. 0) THEN
      ! Check validity of control types.
      ValidScheduleType=CheckScheduleValueMinMax(ContaminantControlledZone(ContControlledZoneNum)%ZoneMinCO2SchedIndex,'>=',0.0,  &
                                                       '<=',2000.0)
      IF (.not. ValidScheduleType) THEN
        CALL ShowSevereError(trim(cCurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid range '//  &
           trim(cAlphaFieldNames(5))//'="'//trim(cAlphaArgs(5))//'"')
        CALL ShowContinueError('..contains values outside of range [0,2000 ppm].')
        ErrorsFound=.TRUE.
      ELSE
        Zone(ContaminantControlledZone(ContControlledZoneNum)%ActualZoneNum)%ZoneMinCO2SchedIndex = &
                                         ContaminantControlledZone(ContControlledZoneNum)%ZoneMinCO2SchedIndex
      ENDIF
    END IF

  END DO ! ContControlledZoneNum

  IF (ErrorsFound) THEN
    CALL ShowFatalError('Errors getting Zone Contaminant Control input data.  Preceding condition(s) cause termination.')
  END IF

  RETURN

END SUBROUTINE GetZoneContaminanSetpoints

SUBROUTINE InitZoneContSetpoints

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Lixing Gu
          !       DATE WRITTEN   May 2010
          !       MODIFIED       NA
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine initializes the data for the zone air contaminant setpoints.

          ! METHODOLOGY EMPLOYED:
          ! Uses the status flags to trigger events.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataZoneEquipment, ONLY: ZoneEquipConfig
  USE DataSurfaces, ONLY: Surface
  USE ScheduleManager, ONLY: GetCurrentScheduleValue

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
  INTEGER :: Loop, ZoneNum
  LOGICAL,SAVE  :: MyOneTimeFlag = .TRUE.
  LOGICAL,SAVE  :: MyEnvrnFlag = .TRUE.
  LOGICAL,SAVE  :: MyDayFlag = .TRUE.
  REAL(r64)      :: CO2Gain                  ! Zone CO2 gain
  LOGICAL,SAVE  :: MyConfigOneTimeFlag = .TRUE.
  INTEGER :: AirLoopNum
  INTEGER :: ContZoneNum
  INTEGER :: I
  LOGICAL :: ErrorsFound=.false.

          ! FLOW:

  IF (Contaminant%CO2Simulation) Then
    OutdoorCO2 = GetCurrentScheduleValue(Contaminant%CO2OutdoorSchedPtr)
  END IF

  IF (MyOneTimeFlag) THEN
    ! CO2
    IF (Contaminant%CO2Simulation) Then
      ALLOCATE(ZoneCO2Setpoint(NumOfZones))
      ZoneCO2Setpoint=0.0
      ALLOCATE(CO2PredictedRate(NumOfZones))
      CO2PredictedRate=0.0
      ALLOCATE(CO2ZoneTimeMinus1(NumOfZones))
      CO2ZoneTimeMinus1=0.0
      ALLOCATE(CO2ZoneTimeMinus2(NumOfZones))
      CO2ZoneTimeMinus2=0.0
      ALLOCATE(CO2ZoneTimeMinus3(NumOfZones))
      CO2ZoneTimeMinus3=0.0
      ALLOCATE(CO2ZoneTimeMinus4(NumOfZones))
      CO2ZoneTimeMinus4=0.0
      ALLOCATE(DSCO2ZoneTimeMinus1(NumOfZones))
      DSCO2ZoneTimeMinus1=0.0
      ALLOCATE(DSCO2ZoneTimeMinus2(NumOfZones))
      DSCO2ZoneTimeMinus2=0.0
      ALLOCATE(DSCO2ZoneTimeMinus3(NumOfZones))
      DSCO2ZoneTimeMinus3=0.0
      ALLOCATE(DSCO2ZoneTimeMinus4(NumOfZones))
      DSCO2ZoneTimeMinus4=0.0
!      ALLOCATE(ZoneAirCO2Temp(NumOfZones))
!      ZoneAirCO2Temp=0.0
!      ALLOCATE(ZoneAirCO2(NumOfZones))
!      ZoneAirCO2=0.0
      ALLOCATE(CO2ZoneTimeMinus1Temp(NumOfZones))
      CO2ZoneTimeMinus1Temp=0.0
      ALLOCATE(CO2ZoneTimeMinus2Temp(NumOfZones))
      CO2ZoneTimeMinus2Temp=0.0
      ALLOCATE(CO2ZoneTimeMinus3Temp(NumOfZones))
      CO2ZoneTimeMinus3Temp=0.0
      ALLOCATE(ZoneCO2MX(NumOfZones))
      ZoneCO2MX = 0.0d0
      ALLOCATE(ZoneCO2M2(NumOfZones))
      ZoneCO2M2 = 0.0d0
      ALLOCATE(ZoneCO21(NumOfZones))
      ZoneCO21=0.0

      ALLOCATE(ZoneSysContDemand(NumOfZones))
      ALLOCATE(ZoneCO2Gain(NumOfZones))
      ZoneCO2Gain=0.0
      ALLOCATE(ZoneCO2GainFromPeople(NumOfZones))
      ZoneCO2GainFromPeople=0.0      
      ALLOCATE(MixingMassFlowCO2(NumOfZones))
      MixingMassFlowCO2=0.0
      ALLOCATE(ZoneAirDensityCO(NumOfZones))
      ZoneAirDensityCO = 0.d0
      !
      ALLOCATE(AZ(NumOfZones))
      AZ = 0.d0
      ALLOCATE(BZ(NumOfZones))
      BZ = 0.d0
      ALLOCATE(CZ(NumOfZones))
      CZ = 0.d0
    END IF

    ALLOCATE(CONTRAT(NumOfZones))
    CONTRAT = 0.0

    ! Allocate Derived Types


    DO Loop = 1, NumOfZones
      ! Zone CO2
      IF (Contaminant%CO2Simulation) Then
        CALL SetupOutputVariable('Zone Air Carbon Dioxide Concentration [ppm]',ZoneAirCO2(Loop),'System','Average',Zone(Loop)%Name)
        CALL SetupOutputVariable('Zone/Sys CO2 Load Rate Predicted to Carbon Dioxide Setpoint [kg/s]', &
                               CO2PredictedRate(Loop),'System','Average',Zone(Loop)%Name)
        CALL SetupOutputVariable('Zone/Sys Carbon Dioxide Setpoint [ppm]', ZoneCO2Setpoint(Loop), &
                               'System','Average',Zone(Loop)%Name)
        CALL SetupOutputVariable('Zone Carbon Dioxide Internal Gain [m3/s]', ZoneCO2Gain(Loop), &
                               'System','Average',Zone(Loop)%Name)
      END IF


    END DO ! Loop

    MyOneTimeFlag = .FALSE.
  END IF

  ! Do the Begin Environment initializations
  IF (MyEnvrnFlag .AND. BeginEnvrnFlag) THEN
    IF (Contaminant%CO2Simulation) Then
      CONTRAT = 0.0
      CO2ZoneTimeMinus1 = OutdoorCO2
      CO2ZoneTimeMinus2 = OutdoorCO2
      CO2ZoneTimeMinus3 = OutdoorCO2
      CO2ZoneTimeMinus4 = OutdoorCO2
      DSCO2ZoneTimeMinus1 = OutdoorCO2
      DSCO2ZoneTimeMinus2 = OutdoorCO2
      DSCO2ZoneTimeMinus3 = OutdoorCO2
      DSCO2ZoneTimeMinus4 = OutdoorCO2
      CO2ZoneTimeMinus1Temp = 0.0
      CO2ZoneTimeMinus2Temp = 0.0
      CO2ZoneTimeMinus3Temp = 0.0
      ZoneAirCO2Temp = OutdoorCO2
      ZoneCO2Setpoint = 0.0
      CO2PredictedRate = 0.0
      ZoneAirCO2 = OutdoorCO2
      ZoneCO21 = OutdoorCO2
      ZoneCO2MX = OutdoorCO2
      ZoneCO2M2 = OutdoorCO2
    END IF
    MyEnvrnFlag = .FALSE.
  END IF

  IF (.NOT. BeginEnvrnFlag) THEN
    MyEnvrnFlag=.TRUE.
  END IF

  ! Do the Begin Day initializations
  IF (MyDayFlag .AND. BeginDayFlag) THEN
    MyDayFlag=.FALSE.
  END IF

  IF (.NOT. BeginDayFlag) THEN
    MyDayFlag=.TRUE.
  END IF

  If (ALLOCATED(ZoneEquipConfig) .AND. MyConfigOneTimeFlag) Then
    DO ContZoneNum = 1, NumContControlledZones
      ZoneNum = ContaminantControlledZone(ContZoneNum)%ActualZoneNum
      AirLoopNum = ZoneEquipConfig(ZoneNum)%AirLoopNum
      ContaminantControlledZone(ContZoneNum)%NumOfZones = 0
      DO Loop = 1, NumOfZones
        IF (.not. ZoneEquipConfig(Loop)%IsControlled) CYCLE
        If (AirLoopNum == ZoneEquipConfig(Loop)%AirLoopNum) Then
          ContaminantControlledZone(ContZoneNum)%NumOfZones=ContaminantControlledZone(ContZoneNum)%NumOfZones+1
        End If
      End Do
      If (ContaminantControlledZone(ContZoneNum)%NumOfZones > 0) Then
        ALLOCATE(ContaminantControlledZone(ContZoneNum)%ControlZoneNum(ContaminantControlledZone(ContZoneNum)%NumOfZones))
        I = 1
        DO Loop = 1, NumOfZones
          IF (.not. ZoneEquipConfig(Loop)%IsControlled) CYCLE
          If (AirLoopNum == ZoneEquipConfig(Loop)%AirLoopNum) Then
            ContaminantControlledZone(ContZoneNum)%ControlZoneNum(I) = Loop
            I = I + 1
          End If
        End Do
      Else
        CALL ShowSevereError('ZoneControl:ContaminantController: a corresponding AirLoopHVAC is not found for the ' &
           //'controlled zone ='//Trim(Zone(ZoneNum)%Name) )
        ErrorsFound = .TRUE.
      End If
    End Do
    MyConfigOneTimeFlag = .FALSE.
    IF (ErrorsFound) THEN
      CALL ShowFatalError('ZoneControl:ContaminantController: Program terminates for preceding reason(s).')
    ENDIF
  End If

  DO Loop = 1, NumContControlledZones
    IF (Contaminant%CO2Simulation) Then
      ZoneNum = ContaminantControlledZone(Loop)%ActualZoneNum
      ZoneCO2Setpoint(ZoneNum)=GetCurrentScheduleValue(ContaminantControlledZone(Loop)%SPSchedIndex)
    END IF
  END DO

  ! CO2 gain
  IF (Contaminant%CO2Simulation) Then
    ZoneCO2Gain = 0.d0
    ZoneCO2GainFromPeople = 0.d0
    ! From people
    Do Loop = 1, TotPeople
      ZoneNum = People(Loop)%ZonePtr
      CO2Gain = People(Loop)%NumberOfPeople * GetCurrentScheduleValue(People(Loop)%NumberOfPeoplePtr) * &
              GetCurrentScheduleValue(People(Loop)%ActivityLevelPtr) * People(Loop)%CO2Rate
      ZoneCO2Gain(ZoneNum) = ZoneCO2Gain(ZoneNum) + CO2Gain
      ZoneCO2GainFromPeople(ZoneNum) = ZoneCO2Gain(ZoneNum)
    End Do
    ! from gas equipment
    Do Loop = 1, TotGasEquip
      ZoneNum = ZoneGas(Loop)%ZonePtr
      CO2Gain = ZoneGas(Loop)%Power * ZoneGas(Loop)%CO2Rate
      ZoneCO2Gain(ZoneNum) = ZoneCO2Gain(ZoneNum) + CO2Gain
    End Do
    ! from CO2 sinks and sources
    DO Loop = 1, TotCO2Gen
      ZoneNum = ZoneCO2Gen(Loop)%ZonePtr
      CO2Gain = ZoneCO2Gen(Loop)%CO2Rate * GetCurrentScheduleValue(ZoneCO2Gen(Loop)%SchedPtr)
      ZoneCO2Gain(ZoneNum) = ZoneCO2Gain(ZoneNum) + CO2Gain
    End Do
  END IF

  RETURN

END SUBROUTINE InitZoneContSetpoints

SUBROUTINE PredictZoneContaminants(ShortenTimeStepSys, UseZoneTimeStepHistory, PriorTimeStep)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Lixing Gu
          !       DATE WRITTEN   May 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine does the prediction step for contaminant control

          ! METHODOLOGY EMPLOYED:
          ! This solves for the required outdoor airflow to achieve the desired
          ! contaminant setpoint in the Zone

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE ScheduleManager, ONLY: GetCurrentScheduleValue
  USE General,         ONLY: RoundSigDigits
  USE DataLoopNode, ONLY: Node

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  LOGICAL, INTENT(IN) :: ShortenTimeStepSys
  LOGICAL, INTENT(IN) :: UseZoneTimeStepHistory      ! if true then use zone timestep history, if false use system time step
  REAL(r64),    INTENT(IN) :: PriorTimeStep  ! the old value for timestep length is passed for possible use in interpolating

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64) :: CO2Gain ! Zone latent load
  REAL(r64) :: RhoAir
  REAL(r64) :: A
  REAL(r64) :: B
  REAL(r64) :: C
  REAL(r64) :: SysTimeStepInSeconds
  LOGICAL   :: ControlledCO2ZoneFlag     ! This determines whether this is a humidity controlled zone or not
  REAL(r64) :: LoadToCO2SetPoint           ! Moisture load at humidifying set point
  INTEGER   :: ContControlledZoneNum      ! The Splitter that you are currently loading input into
  INTEGER   :: ZoneNum
  INTEGER   :: I
  REAL(r64) :: ZoneAirCO2Setpoint

          ! FLOW:

  ! Update zone CO2
  DO ZoneNum = 1, NumofZones

    IF (ShortenTimeStepSys) THEN  !

      IF (Zone(ZoneNum)%SystemZoneNodeNumber > 0) THEN ! roll back result for zone air node,
        IF (Contaminant%CO2Simulation) &
          Node(Zone(ZoneNum)%SystemZoneNodeNumber)%CO2   = CO2ZoneTimeMinus1(ZoneNum)
      ENDIF

      IF (NumOfSysTimeSteps /= NumOfSysTimeStepsLastZoneTimeStep) THEN ! cannot reuse existing DS data, interpolate from zone time

        IF (Contaminant%CO2Simulation) &
          Call DownInterpolate4HistoryValues(PriorTimeStep,TimeStepSys, &
                            CO2ZoneTimeMinus1(ZoneNum),   CO2ZoneTimeMinus2(ZoneNum),   &
                                          CO2ZoneTimeMinus3(ZoneNum),   CO2ZoneTimeMinus4(ZoneNum),  CO2ZoneTimeMinus4(ZoneNum),&
                             ZoneAirCO2(ZoneNum), DSCO2ZoneTimeMinus1(ZoneNum), DSCO2ZoneTimeMinus2(ZoneNum), &
                             DSCO2ZoneTimeMinus3(ZoneNum), DSCO2ZoneTimeMinus4(ZoneNum))

      ELSE ! reuse history data in DS terms from last zone time step to preserve information that would be lost
         ! do nothing because DS history would have been pushed prior and should be ready

      ENDIF

    ENDIF
    ! now update the variables actually used in the balance equations.
    IF(UseZoneTimeStepHistory) THEN

      IF (Contaminant%CO2Simulation) Then
        CO2ZoneTimeMinus1Temp(ZoneNum) = CO2ZoneTimeMinus1(ZoneNum)
        CO2ZoneTimeMinus2Temp(ZoneNum) = CO2ZoneTimeMinus2(ZoneNum)
        CO2ZoneTimeMinus3Temp(ZoneNum) = CO2ZoneTimeMinus3(ZoneNum)
      End If

    ELSE  ! use down-stepped history

      IF (Contaminant%CO2Simulation) Then
        CO2ZoneTimeMinus1Temp(ZoneNum) = DSCO2ZoneTimeMinus1(ZoneNum)
        CO2ZoneTimeMinus2Temp(ZoneNum) = DSCO2ZoneTimeMinus2(ZoneNum)
        CO2ZoneTimeMinus3Temp(ZoneNum) = DSCO2ZoneTimeMinus3(ZoneNum)
      End If

    END IF

    If (ZoneAirSolutionAlgo .NE. Use3rdOrder) Then
      IF (Contaminant%CO2Simulation) Then
        If (ShortenTimeStepSys .and. TimeStepSys .LT. TimeStepZone) Then
          If (PreviousTimeStep < TimeStepZone) Then
            ZoneCO21(ZoneNum) = ZoneCO2M2(ZoneNum)
          Else
            ZoneCO21(ZoneNum) = ZoneCO2MX(ZoneNum)
          End If
          ShortenTimeStepSysRoomAir = .TRUE.
        Else
          ZoneCO21(ZoneNum) = ZoneAirCO2(ZoneNum)
        End If
      End If
    End If

    IF (Contaminant%CO2Simulation) Then

      CO2PredictedRate(ZoneNum) = 0.0
      LoadToCO2SetPoint=0.0
      ZoneSysContDemand(ZoneNum)%OutputRequiredToCO2SP = 0.0

      ! Check to see if this is a "CO2 controlled zone"
      ControlledCO2ZoneFlag = .FALSE.
      ! Check all the controlled zones to see if it matches the zone simulated
      DO ContControlledZoneNum = 1, NumContControlledZones
        IF (GetCurrentScheduleValue(ContaminantControlledZone(ContControlledZoneNum)%AvaiSchedPtr) .GT. 0.d0) Then
          ZoneAirCO2Setpoint = ZoneCO2Setpoint(ContaminantControlledZone(ContControlledZoneNum)%ActualZoneNum)
          IF (ContaminantControlledZone(ContControlledZoneNum)%EMSOverrideCO2SetpointOn) THEN
            ZoneAirCO2Setpoint = ContaminantControlledZone(ContControlledZoneNum)%EMSOverrideCO2SetpointValue
          End If
          If (ContaminantControlledZone(ContControlledZoneNum)%NumOfZones > 1) Then
            IF (ContaminantControlledZone(ContControlledZoneNum)%ActualZoneNum /= ZoneNum) Then
              Do I=1,ContaminantControlledZone(ContControlledZoneNum)%NumOfZones
                If (ContaminantControlledZone(ContControlledZoneNum)%ControlZoneNum(I) == ZoneNum) Then
                  ControlledCO2ZoneFlag = .TRUE.
                  Exit
                End If
              End Do
              If (ControlledCO2ZoneFlag) Exit
            ELSE
              ControlledCO2ZoneFlag = .TRUE.
              EXIT
            End If
          ENDIF
        ENDIF
      END DO ! CO2ControlledZoneNum

      If (ControlledCO2ZoneFlag) Then
        ! The density of air
        RhoAir = PsyRhoAirFnPbTdbW(OutBaroPress,ZT(ZoneNum),ZoneAirHumRat(ZoneNum),'PredictZoneContaminants')

        ! Calculate Co2 from infiltration + humidity added from latent load
        ! to determine system added/subtracted moisture.
        CO2Gain = ZoneCO2Gain(ZoneNum)*RhoAir*1.0E6

        SysTimeStepInSeconds = SecInHour * TimeStepSys

        ! Calculate the coefficients for the 3rd Order derivative for final
        ! zone CO2.  The A, B, C coefficients are analogous to the CO2 balance.
        ! Assume that the system will have flow
        if (SimulateAirflowNetwork == AirflowNetworkControlMultizone.OR.SimulateAirflowNetwork == AirflowNetworkControlMultiADS &
            .OR. (SimulateAirflowNetwork == AirflowNetworkControlSimpleADS .AND. AirflowNetworkFanActivated)) then
          ! Multizone airflow calculated in AirflowNetwork
          B = CO2Gain + AirflowNetworkExchangeData(ZoneNum)%SumMHrCO + &
              AirflowNetworkExchangeData(ZoneNum)%SumMMHrCO
          A = AirflowNetworkExchangeData(ZoneNum)%SumMHr+AirflowNetworkExchangeData(ZoneNum)%SumMMHr
        else
          B = CO2Gain + ((oamfl(ZoneNum) + vamfl(ZoneNum) + eamfl(ZoneNum) + ctmfl(ZoneNum)) * OutdoorCO2) + &
               MixingMassFlowCO2(ZoneNum)
          A = oamfl(ZoneNum) + vamfl(ZoneNum) + eamfl(ZoneNum) + ctmfl(ZoneNum) + MixingMassFlowZone(ZoneNum)
        end if
        C = RhoAir * Zone(ZoneNum)%Volume * ZoneVolCapMultpCO2 / SysTimeStepInSeconds

        ! Use a 3rd Order derivative to predict zone moisture addition or removal and
        ! smooth the changes using the zone air capacitance.  Positive values of CO2 Load means that
        ! this amount of CO2 must be added to the zone to reach the setpoint.  Negative values represent
        ! the amount of CO2 that must be removed by the system.
        SELECT CASE (ZoneAirSolutionAlgo)
          CASE (Use3rdOrder)
            LoadToCO2SetPoint = ((11.0d0/6.0d0) * C + A) * ZoneAirCO2Setpoint - &
             (B + C * (3.0d0 * CO2ZoneTimeMinus1Temp(ZoneNum) - (3.0d0/2.0d0) * CO2ZoneTimeMinus2Temp(ZoneNum) + &
             (1.0d0/3.0d0) * CO2ZoneTimeMinus3Temp(ZoneNum)))
          ! Exact solution
          CASE (UseAnalyticalSolution)
            If (A .eq. 0.0d0) Then ! B=0
              LoadToCO2SetPoint = C*(ZoneAirCO2Setpoint-ZoneCO21(ZoneNum)) - B
            Else
              LoadToCO2SetPoint = A*(ZoneAirCO2Setpoint-ZoneCO21(ZoneNum)*exp(MIN(700.d0,-A/C)))/(1.0d0-exp(MIN(700.d0,-A/C))) - B
            End If
          CASE (UseEulerMethod)
            LoadToCO2SetPoint = C*(ZoneAirCO2Setpoint-ZoneCO21(ZoneNum)) + A*ZoneAirCO2Setpoint - B
        END SELECT
        If (ZoneAirCO2Setpoint .GT. OutdoorCO2 .AND. LoadToCo2SetPoint < 0.0) Then
          ZoneSysContDemand(ZoneNum)%OutputRequiredToCO2SP = LoadToCo2SetPoint/(OutdoorCO2 - ZoneAirCO2Setpoint)
        End If
      End If

      ! Apply the Zone Multiplier to the total zone moisture load
      ZoneSysContDemand(ZoneNum)%OutputRequiredToCO2SP = ZoneSysContDemand(ZoneNum)%OutputRequiredToCO2SP &
                                                       * Zone(ZoneNum)%Multiplier * Zone(ZoneNum)%ListMultiplier
      CO2PredictedRate(ZoneNum) = ZoneSysContDemand(ZoneNum)%OutputRequiredToCO2SP
    End If
  END DO

  RETURN

END SUBROUTINE PredictZoneContaminants

SUBROUTINE PushZoneTimestepHistories

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Lixing Gu
          !       DATE WRITTEN   July, 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! push histories for timestep advancing.
          ! This subroutine is modified from PushZoneTimestepHistories in ZoneTempPredictorCorrector module

          ! METHODOLOGY EMPLOYED:
          ! <description>

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
  INTEGER :: ZoneNum

        ! Push the temperature and humidity ratio histories

  DO ZoneNum = 1, NumOfZones
    IF (Contaminant%CO2Simulation) Then
      CO2ZoneTimeMinus4(ZoneNum) = CO2ZoneTimeMinus3(ZoneNum)
      CO2ZoneTimeMinus3(ZoneNum) = CO2ZoneTimeMinus2(ZoneNum)
      CO2ZoneTimeMinus2(ZoneNum) = CO2ZoneTimeMinus1(ZoneNum)
      CO2ZoneTimeMinus1(ZoneNum) = ZoneAirCO2Avg(ZoneNum) ! using average for whole zone time step.
      ZoneAirCO2(ZoneNum) = ZoneAirCO2Temp(ZoneNum)

      If (ZoneAirSolutionAlgo .NE. Use3rdOrder) Then
        ZoneCO2M2(ZoneNum) = ZoneCO2MX(ZoneNum)
        ZoneCO2MX(ZoneNum) = ZoneAirCO2Avg(ZoneNum) ! using average for whole zone time step.
      End If
    END IF
  ENDDO ! zone loop
  RETURN

END SUBROUTINE PushZoneTimestepHistories

SUBROUTINE PushSystemTimestepHistories

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Lixing Gu
          !       DATE WRITTEN   July, 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! push histories
          ! This subroutine is modified from PushSystemTimestepHistories in ZoneTempPredictorCorrector module

          ! METHODOLOGY EMPLOYED:
          ! <description>

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
  INTEGER :: ZoneNum

  ! Push the temperature and humidity ratio histories back in time

  DO ZoneNum = 1, NumOfZones
    IF (Contaminant%CO2Simulation) Then
      DSCO2ZoneTimeMinus4(ZoneNum) = DSCO2ZoneTimeMinus3(ZoneNum)
      DSCO2ZoneTimeMinus3(ZoneNum) = DSCO2ZoneTimeMinus2(ZoneNum)
      DSCO2ZoneTimeMinus2(ZoneNum) = DSCO2ZoneTimeMinus1(ZoneNum)
      DSCO2ZoneTimeMinus1(ZoneNum) = ZoneAirCO2(ZoneNum)
    ENDIF
  ENDDO ! zone loop

  If (ZoneAirSolutionAlgo .NE. Use3rdOrder) Then
    DO ZoneNum = 1, NumOfZones
      IF (Contaminant%CO2Simulation) Then
        ZoneCO2M2(ZoneNum) = ZoneCO2MX(ZoneNum)
        ZoneCO2MX(ZoneNum) = ZoneAirCO2Temp(ZoneNum) ! using average for whole zone time step.
      ENDIF
    ENDDO ! zone loop
  End If

  RETURN

END SUBROUTINE PushSystemTimestepHistories

SUBROUTINE RevertZoneTimestepHistories

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Lixing Gu
          !       DATE WRITTEN   July, 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! rewind histories to undo inadvertent pushing
          ! This subroutine is modified from RevertZoneTimestepHistories in ZoneTempPredictorCorrector module

          ! METHODOLOGY EMPLOYED:
          ! <description>

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
  INTEGER :: ZoneNum

  ! REvert the contaminnants histories

  DO ZoneNum = 1, NumOfZones
    IF (Contaminant%CO2Simulation) Then
      CO2ZoneTimeMinus1(ZoneNum) = CO2ZoneTimeMinus2(ZoneNum)
      CO2ZoneTimeMinus2(ZoneNum) = CO2ZoneTimeMinus3(ZoneNum)
      CO2ZoneTimeMinus3(ZoneNum) = CO2ZoneTimeMinus4(ZoneNum)
    ENDIF
  ENDDO ! zone loop

  RETURN

END SUBROUTINE RevertZoneTimestepHistories

SUBROUTINE CorrectZoneContaminants(ShortenTimeStepSys, UseZoneTimeStepHistory, PriorTimeStep)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Lixing Gu
          !       DATE WRITTEN   July, 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine updates the zone contaminants.
          ! This subroutine is modified from CorrectZoneHumRat in ZoneTempPredictorCorrector module

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! Routine FinalZnCalcs - FINAL ZONE CALCULATIONS, authored by Dale Herron
          ! for BLAST.

          ! USE STATEMENTS:
  USE DataLoopNode, ONLY: Node
  USE DataZoneEquipment, ONLY: ZoneEquipConfig
  USE ZonePlenum, ONLY: ZoneRetPlenCond, ZoneSupPlenCond, NumZoneReturnPlenums, NumZoneSupplyPlenums
  USE DataDefineEquip, ONLY: AirDistUnit, NumAirDistUnits

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  LOGICAL, INTENT(IN)  :: ShortenTimeStepSys
  LOGICAL, INTENT(IN)  :: UseZoneTimeStepHistory  ! if true then use zone timestep history, if false use system time step history
  REAL(r64),  INTENT(IN)  :: PriorTimeStep  ! the old value for timestep length is passed for possible use in interpolating

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: NodeNum
  INTEGER :: ZoneNodeNum
  INTEGER :: ZoneEquipConfigNum
  LOGICAL :: ControlledZoneAirFlag
  INTEGER :: ZoneRetPlenumNum
  INTEGER :: ZoneSupPlenumNum
  LOGICAL :: ZoneRetPlenumAirFlag
  LOGICAL :: ZoneSupPlenumAirFlag
  REAL(r64) :: CO2Gain ! Zone CO2 internal gain
  REAL(r64) :: RhoAir
  REAL(r64) :: A
  REAL(r64) :: B
  REAL(r64) :: C
  REAL(r64) :: CO2MassFlowRate
  REAL(r64) :: ExhMassFlowRate
  REAL(r64) :: TotExitMassFlowRate
  REAL(r64) :: ZoneMassFlowRate
  REAL(r64) :: SysTimeStepInSeconds
  REAL(r64) :: ZoneMult
  INTEGER :: ADUListIndex
  INTEGER :: ADUNum
  INTEGER :: ADUInNode
  INTEGER :: ADUOutNode
  INTEGER :: ZoneNum

          ! FLOW:
  ! Update zone CO2
  DO ZoneNum = 1, NumOfZones

    AZ(ZoneNum) = 0.0
    BZ(ZoneNum) = 0.0
    CZ(ZoneNum) = 0.0
    ! Update variables
    IF (ShortenTimeStepSys) THEN
      !time step has gotten smaller, use zone timestep history to interpolate new set of "DS" history terms.
      If (NumOfSysTimeSteps /= NumOfSysTimeStepsLastZoneTimeStep)  then ! cannot reuse existing DS data, interpolate from zone time
        IF (Contaminant%CO2Simulation) Then
          Call DownInterpolate4HistoryValues(PriorTimeStep,TimeStepSys, &
                                ZoneAirCO2(ZoneNum), CO2ZoneTimeMinus1(ZoneNum),   CO2ZoneTimeMinus2(ZoneNum),   & !
                                                     CO2ZoneTimeMinus3(ZoneNum),   CO2ZoneTimeMinus4(ZoneNum), & !
                                ZoneAirCO2(ZoneNum), DSCO2ZoneTimeMinus1(ZoneNum), DSCO2ZoneTimeMinus2(ZoneNum), &
                                                     DSCO2ZoneTimeMinus3(ZoneNum), DSCO2ZoneTimeMinus4(ZoneNum))
        ENDIF

      ELSE ! reuse history data in DS terms from last zone time step to preserve information that would be lost
         ! do nothing because DS history would have been pushed prior and should be ready?

      ENDIF
    ENDIF

    ! now update the variables actually used in the balance equations.
    IF(.not. UseZoneTimeStepHistory) THEN
      IF (Contaminant%CO2Simulation) Then
        CO2ZoneTimeMinus1Temp(ZoneNum) = DSCO2ZoneTimeMinus1(ZoneNum)
        CO2ZoneTimeMinus2Temp(ZoneNum) = DSCO2ZoneTimeMinus2(ZoneNum)
        CO2ZoneTimeMinus3Temp(ZoneNum) = DSCO2ZoneTimeMinus3(ZoneNum)
      END IF
    ELSE
      IF (Contaminant%CO2Simulation) Then
        CO2ZoneTimeMinus1Temp(ZoneNum) = CO2ZoneTimeMinus1(ZoneNum)
        CO2ZoneTimeMinus2Temp(ZoneNum) = CO2ZoneTimeMinus2(ZoneNum)
        CO2ZoneTimeMinus3Temp(ZoneNum) = CO2ZoneTimeMinus3(ZoneNum)
      END IF
    END IF

    ! Start to calculate zone CO2 levels
    CO2MassFlowRate = 0.0
    ZoneMassFlowRate = 0.0
    ExhMassFlowRate = 0.0
    TotExitMassFlowRate = 0.0
    ZoneMult = Zone(ZoneNum)%Multiplier * Zone(ZoneNum)%ListMultiplier

    ! Check to see if this is a controlled zone
    ControlledZoneAirFlag = .FALSE.
    DO ZoneEquipConfigNum = 1, NumOfZones
      IF (.not. Zone(ZoneEquipConfigNum)%IsControlled) CYCLE
      IF (ZoneEquipConfig(ZoneEquipConfigNum)%ActualZoneNum /= ZoneNum) CYCLE
      ControlledZoneAirFlag = .TRUE.
      EXIT
    END DO ! ZoneEquipConfigNum

    ! Check to see if this is a plenum zone
    ZoneRetPlenumAirFlag = .FALSE.
    DO ZoneRetPlenumNum = 1, NumZoneReturnPlenums
      IF (ZoneRetPlenCond(ZoneRetPlenumNum)%ActualZoneNum /= ZoneNum) CYCLE
      ZoneRetPlenumAirFlag = .TRUE.
      EXIT
    END DO ! ZoneRetPlenumNum
    ZoneSupPlenumAirFlag = .FALSE.
    DO ZoneSupPlenumNum = 1, NumZoneSupplyPlenums
      IF (ZoneSupPlenCond(ZoneSupPlenumNum)%ActualZoneNum /= ZoneNum) CYCLE
      ZoneSupPlenumAirFlag = .TRUE.
      EXIT
    END DO ! ZoneSupPlenumNum

    IF (ControlledZoneAirFlag) THEN ! If there is system flow then calculate the flow rates

      ! Calculate moisture flow rate into each zone
      DO NodeNum = 1, ZoneEquipConfig(ZoneEquipConfigNum)%NumInletNodes

        IF (Contaminant%CO2Simulation) Then
          CO2MassFlowRate = CO2MassFlowRate + &
                           (Node(ZoneEquipConfig(ZoneEquipConfigNum)%InletNode(NodeNum))%MassFlowRate * &
                            Node(ZoneEquipConfig(ZoneEquipConfigNum)%InletNode(NodeNum))%CO2) / ZoneMult
        End If
        ZoneMassFlowRate = ZoneMassFlowRate + &
                            Node(ZoneEquipConfig(ZoneEquipConfigNum)%InletNode(NodeNum))%MassFlowRate / ZoneMult
      END DO ! NodeNum

      DO NodeNum = 1, ZoneEquipConfig(ZoneEquipConfigNum)%NumExhaustNodes
        ExhMassFlowRate = ExhMassFlowRate + Node(ZoneEquipConfig(ZoneEquipConfigNum)%ExhaustNode(NodeNum))%MassFlowRate / ZoneMult
      END DO ! NodeNum

      IF (ZoneEquipConfig(ZoneEquipConfigNum)%ReturnAirNode > 0) THEN
        TotExitMassFlowRate = ExhMassFlowRate + Node(ZoneEquipConfig(ZoneEquipConfigNum)%ReturnAirNode)%MassFlowRate / ZoneMult
      END IF

    ! Do the calculations for the plenum zone
    ELSE IF (ZoneRetPlenumAirFlag) THEN
      DO NodeNum = 1, ZoneRetPlenCond(ZoneRetPlenumNum)%NumInletNodes

        IF (Contaminant%CO2Simulation) Then
          CO2MassFlowRate = CO2MassFlowRate + &
                           (Node(ZoneRetPlenCond(ZoneRetPlenumNum)%InletNode(NodeNum))%MassFlowRate * &
                            Node(ZoneRetPlenCond(ZoneRetPlenumNum)%InletNode(NodeNum))%CO2) / ZoneMult
        End If
        ZoneMassFlowRate = ZoneMassFlowRate + &
                            Node(ZoneRetPlenCond(ZoneRetPlenumNum)%InletNode(NodeNum))%MassFlowRate / ZoneMult
      END DO ! NodeNum
      ! add in the leak flow
      DO ADUListIndex=1,ZoneRetPlenCond(ZoneRetPlenumNum)%NumADUs
        ADUNum = ZoneRetPlenCond(ZoneRetPlenumNum)%ADUIndex(ADUListIndex)
        IF (AirDistUnit(ADUNum)%UpStreamLeak) THEN
          ADUInNode = AirDistUnit(ADUNum)%InletNodeNum
          IF (Contaminant%CO2Simulation) Then
            CO2MassFlowRate = CO2MassFlowRate + &
                               (AirDistUnit(ADUNum)%MassFlowRateUpStrLk * Node(ADUInNode)%CO2) / ZoneMult
          End If
          ZoneMassFlowRate = ZoneMassFlowRate + AirDistUnit(ADUNum)%MassFlowRateUpStrLk / ZoneMult
        END IF
        IF (AirDistUnit(ADUNum)%DownStreamLeak) THEN
          ADUOutNode = AirDistUnit(ADUNum)%OutletNodeNum
          IF (Contaminant%CO2Simulation) Then
            CO2MassFlowRate = CO2MassFlowRate + &
                               (AirDistUnit(ADUNum)%MassFlowRateDnStrLk * Node(ADUOutNode)%CO2) / ZoneMult
          End If
          ZoneMassFlowRate = ZoneMassFlowRate + AirDistUnit(ADUNum)%MassFlowRateDnStrLk / ZoneMult
        END IF
      END DO
      ! Do not allow exhaust mass flow for a plenum zone
      ExhMassFlowRate = 0.0
      TotExitMassFlowRate = ExhMassFlowRate + ZoneMassFlowRate

    ELSE IF (ZoneSupPlenumAirFlag) THEN

      IF (Contaminant%CO2Simulation) Then
        CO2MassFlowRate = CO2MassFlowRate + &
                            (Node(ZoneSupPlenCond(ZoneSupPlenumNum)%InletNode)%MassFlowRate * &
                            Node(ZoneSupPlenCond(ZoneSupPlenumNum)%InletNode)%CO2) / ZoneMult
      END IF
      ZoneMassFlowRate = ZoneMassFlowRate + &
                           Node(ZoneSupPlenCond(ZoneSupPlenumNum)%InletNode)%MassFlowRate / ZoneMult
      ! Do not allow exhaust mass flow for a plenum zone
      ExhMassFlowRate = 0.0
      TotExitMassFlowRate = ExhMassFlowRate + ZoneMassFlowRate
    END IF

    SysTimeStepInSeconds = SecInHour * TimeStepSys

    ! Calculate the coefficients for the 3rd order derivative for final
    ! zone humidity ratio.  The A, B, C coefficients are analogous to the
    ! CO2 balance.  There are 2 cases that should be considered, system
    ! operating and system shutdown.

    RhoAir = PsyRhoAirFnPbTdbW(OutBaroPress,ZT(ZoneNum),ZoneAirHumRat(ZoneNum),'CorrectZoneContaminants')
!    RhoAir = ZoneAirDensityCO(ZoneNum)

    ZoneAirDensityCO(ZoneNum) = RhoAir
    ! Calculate Co2 internal gain
    CO2Gain = ZoneCO2Gain(ZoneNum)*RhoAir*1.0E6

    ! Check for the flow and NO flow condition
    IF (ZoneMassFlowRate .GT. 0.0) THEN
      IF (Contaminant%CO2Simulation) Then
        B = CO2Gain+((oamfl(ZoneNum)+vamfl(ZoneNum)+eamfl(ZoneNum)+ctmfl(ZoneNum))* OutdoorCO2) &
                               +(CO2MassFlowRate) &
                               +MixingMassFlowCO2(ZoneNum)
        A = TotExitMassFlowRate + oamfl(ZoneNum) + vamfl(ZoneNum) + eamfl(ZoneNum) + ctmfl(ZoneNum) &
                            + MixingMassFlowZone(ZoneNum)
        if (SimulateAirflowNetwork == AirflowNetworkControlMultizone .OR.   &
            SimulateAirflowNetwork == AirflowNetworkControlMultiADS  .OR.   &
           (SimulateAirflowNetwork == AirflowNetworkControlSimpleADS .AND. AirflowNetworkFanActivated)) then
          ! Multizone airflow calculated in AirflowNetwork
          B = CO2Gain+(AirflowNetworkExchangeData(ZoneNum)%SumMHrCO+AirflowNetworkExchangeData(ZoneNum)%SumMMHrCO)+CO2MassFlowRate
          A = TotExitMassFlowRate + AirflowNetworkExchangeData(ZoneNum)%SumMHr +AirflowNetworkExchangeData(ZoneNum)%SumMMHr
        end if
        C = RhoAir*Zone(ZoneNum)%Volume*ZoneVolCapMultpCO2/SysTimeStepInSeconds
      End If
    ELSE IF (ZoneMassFlowRate .LE. 0.0) THEN
      IF (Contaminant%CO2Simulation) Then
        B = CO2Gain+((oamfl(ZoneNum)+vamfl(ZoneNum)+eamfl(ZoneNum)+ctmfl(ZoneNum)+ExhMassFlowRate)* OutdoorCO2) &
                               + MixingMassFlowCO2(ZoneNum)
        A = oamfl(ZoneNum) + vamfl(ZoneNum) + eamfl(ZoneNum) +ctmfl(ZoneNum) + ExhMassFlowRate + MixingMassFlowZone(ZoneNum)
        if (SimulateAirflowNetwork == AirflowNetworkControlMultizone .OR.   &
            SimulateAirflowNetwork == AirflowNetworkControlMultiADS  .OR.   &
           (SimulateAirflowNetwork == AirflowNetworkControlSimpleADS .AND. AirflowNetworkFanActivated)) then
          ! Multizone airflow calculated in AirflowNetwork
          B = CO2Gain+AirflowNetworkExchangeData(ZoneNum)%SumMHrCO+AirflowNetworkExchangeData(ZoneNum)%SumMMHrCO
          A = AirflowNetworkExchangeData(ZoneNum)%SumMHr+AirflowNetworkExchangeData(ZoneNum)%SumMMHr
        end if
        C = RhoAir*Zone(ZoneNum)%Volume*ZoneVolCapMultpCO2/SysTimeStepInSeconds
      End If
    END IF

    IF (Contaminant%CO2Simulation) Then
      if (SimulateAirflowNetwork > AirflowNetworkControlMultizone) then
        B = B+AirflowNetworkExchangeData(ZoneNum)%TotalCO2
      end if

      AZ(ZoneNum) = A
      BZ(ZoneNum) = B
      CZ(ZoneNum) = C

      ! Use a 3rd order derivative to predict final zone humidity ratio and
      ! smooth the changes using the zone air capacitance.
      SELECT CASE (ZoneAirSolutionAlgo)
        CASE (Use3rdOrder)
          ZoneAirCO2Temp(ZoneNum)=(B+C*(3.0d0*CO2ZoneTimeMinus1Temp(ZoneNum)-(3.0d0/2.0d0)*Co2ZoneTimeMinus2Temp(ZoneNum)+ &
                             (1.0d0/3.0d0)*Co2ZoneTimeMinus3Temp(ZoneNum)))/((11.0d0/6.0d0)*C+A)
        ! Exact solution
        CASE (UseAnalyticalSolution)
          If (A .eq. 0.0d0) Then ! B=0
            ZoneAirCO2Temp(ZoneNum)= ZoneCO21(ZoneNum) + B/C
          Else
            ZoneAirCO2Temp(ZoneNum)= (ZoneCO21(ZoneNum)-B/A)*exp(MIN(700.d0,-A/C))+B/A
          End If
        CASE (UseEulerMethod)
          ZoneAirCO2Temp(ZoneNum) = (C*ZoneCO21(ZoneNum)+B)/(C+A)
      END SELECT

      ! Set the CO2 to zero if the zone has been large sinks
      IF (ZoneAirCO2Temp(ZoneNum) .LT. 0.0) ZoneAirCO2Temp(ZoneNum) = 0.0

      ZoneAirCO2(ZoneNum) = ZoneAirCO2Temp(ZoneNum)

      ! Now put the calculated info into the actual zone nodes; ONLY if there is zone air flow, i.e. controlled zone or plenum zone
      ZoneNodeNum = Zone(ZoneNum)%SystemZoneNodeNumber
      IF (ZoneNodeNum > 0) THEN
        Node(ZoneNodeNum)%CO2 = ZoneAirCO2Temp(ZoneNum)
      END IF
    END IF

  END DO

  RETURN

END SUBROUTINE CorrectZoneContaminants



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

END MODULE ZoneContaminantPredictorCorrector
