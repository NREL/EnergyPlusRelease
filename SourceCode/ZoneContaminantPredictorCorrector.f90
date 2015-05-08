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

LOGICAL :: GetZoneAirContamInputFlag = .TRUE.  ! True when need to get input
INTEGER :: TotGCGenConstant    = 0       ! Number of constant generic contaminant sources and sinks
INTEGER :: TotGCGenPDriven     = 0       ! Number of pressure driven generic contaminant sources and sinks
INTEGER :: TotGCGenCutoff      = 0       ! Number of cutoff model generic contaminant sources and sinks
INTEGER :: TotGCGenDecay       = 0       ! Number of decay model generic contaminant sources and sinks
INTEGER :: TotGCBLDiff         = 0       ! Number of boudary layer diffusion generic contaminant model
INTEGER :: TotGCDVS            = 0       ! Number of deposition velocity sink generic contaminant model
INTEGER :: TotGCDRS            = 0       ! Number of deposition rate sink generic contaminant model


          ! SUBROUTINE SPECIFICATIONS:
PUBLIC ManageZoneContaminanUpdates
PRIVATE InitZoneContSetpoints
PRIVATE GetZoneContaminanInputs
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

  IF (GetZoneAirContamInputFlag) THEN
    If (Contaminant%GenericContamSimulation) CALL GetZoneContaminanInputs
    CALL GetZoneContaminanSetpoints
    GetZoneAirContamInputFlag = .FALSE.
  END IF

  IF (.NOT. Contaminant%SimulateContaminants) Return

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

SUBROUTINE GetZoneContaminanInputs

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Lixing Gu
          !       DATE WRITTEN   Dec. 2011
          !       MODIFIED       NA
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine gets the inputs related to generic contaminant internal gain.

          ! METHODOLOGY EMPLOYED:
          ! Uses the status flags to trigger events.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataIPShortCuts
  USE InputProcessor
  USE ScheduleManager, ONLY: GetScheduleIndex, CheckScheduleValueMinMax, GetScheduleMinValue, GetScheduleMaxValue,  &
                             CheckScheduleValue
  USE General, ONLY: TrimSigDigits, RoundSigDigits, FindNumberInList
  USE DataAirflowNetwork, ONLY: AirflowNetworkNumOfSurfaces, MultizoneSurfaceData
  USE DataSurfaces, ONLY: Surface,TotSurfaces, ExternalEnvironment

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER :: RoutineName='GetSourcesAndSinks: '

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  CHARACTER(len=MaxNameLength), DIMENSION(:), ALLOCATABLE :: AlphaName
  REAL(r64), DIMENSION(:), ALLOCATABLE        :: IHGNumbers
  REAL(r64) SchMin
  REAL(r64) SchMax
  INTEGER :: NumAlpha
  INTEGER :: NumNumber
  INTEGER :: IOSTAT
  INTEGER :: MaxAlpha
  INTEGER :: MaxNumber
  INTEGER :: Loop
  INTEGER :: ZonePtr
  LOGICAL :: ErrorsFound = .FALSE.
  LOGICAL :: IsNotOK               ! Flag to verify name
  LOGICAL :: IsBlank               ! Flag for blank name
!  LOGICAL :: ValidScheduleType
  LOGICAL, DIMENSION(:), ALLOCATABLE          :: RepVarSet
  CHARACTER(len=MaxNameLength) :: CurrentModuleObject

          ! FLOW:

  ALLOCATE(RepVarSet(NumOfZones))
  RepVarSet=.true.

  MaxAlpha=-100
  MaxNumber=-100
  CurrentModuleObject='ZoneContaminantSourceAndSink:Generic:Constant'
  CALL GetObjectDefMaxArgs(CurrentModuleObject,Loop,NumAlpha,NumNumber)
  MaxAlpha=MAX(MaxAlpha,NumAlpha)
  MaxNumber=MAX(MaxNumber,NumNumber)
  CurrentModuleObject='SurfaceContaminantSourceAndSink:Generic:PressureDriven'
  CALL GetObjectDefMaxArgs(CurrentModuleObject,Loop,NumAlpha,NumNumber)
  MaxAlpha=MAX(MaxAlpha,NumAlpha)
  MaxNumber=MAX(MaxNumber,NumNumber)
  CurrentModuleObject='ZoneContaminantSourceAndSink:Generic:CutoffModel'
  CALL GetObjectDefMaxArgs(CurrentModuleObject,Loop,NumAlpha,NumNumber)
  MaxAlpha=MAX(MaxAlpha,NumAlpha)
  MaxNumber=MAX(MaxNumber,NumNumber)
  CurrentModuleObject='ZoneContaminantSourceAndSink:Generic:DecaySource'
  CALL GetObjectDefMaxArgs(CurrentModuleObject,Loop,NumAlpha,NumNumber)
  MaxAlpha=MAX(MaxAlpha,NumAlpha)
  MaxNumber=MAX(MaxNumber,NumNumber)
  CurrentModuleObject='SurfaceContaminantSourceAndSink:Generic:BoundaryLayerDiffusion'
  CALL GetObjectDefMaxArgs(CurrentModuleObject,Loop,NumAlpha,NumNumber)
  MaxAlpha=MAX(MaxAlpha,NumAlpha)
  MaxNumber=MAX(MaxNumber,NumNumber)
  CurrentModuleObject='SurfaceContaminantSourceAndSink:Generic:DepositionVelocitySink'
  CALL GetObjectDefMaxArgs(CurrentModuleObject,Loop,NumAlpha,NumNumber)
  MaxAlpha=MAX(MaxAlpha,NumAlpha)
  MaxNumber=MAX(MaxNumber,NumNumber)
  CurrentModuleObject='ZoneContaminantSourceAndSink:Generic:DepositionRateSink'
  CALL GetObjectDefMaxArgs(CurrentModuleObject,Loop,NumAlpha,NumNumber)
  MaxAlpha=MAX(MaxAlpha,NumAlpha)
  MaxNumber=MAX(MaxNumber,NumNumber)
  ALLOCATE(IHGNumbers(MaxNumber))
  ALLOCATE(AlphaName(MaxAlpha))
  IHGNumbers=0.0d0
  AlphaName=' '

  CurrentModuleObject='ZoneContaminantSourceAndSink:Generic:Constant'
  TotGCGenConstant=GetNumObjectsFound(CurrentModuleObject)
  ALLOCATE(ZoneContamGenericConstant(TotGCGenConstant))

  DO Loop=1,TotGCGenConstant
    AlphaName='  '
    IHGNumbers=0.0d0
    CALL GetObjectItem(CurrentModuleObject,Loop,AlphaName,NumAlpha,IHGNumbers,NumNumber,IOStat,  &
                   AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                   AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

    IsNotOK = .FALSE.
    IsBlank = .FALSE.
    CALL VerifyName(AlphaName(1),ZoneContamGenericConstant%Name,Loop-1,IsNotOK,IsBlank,TRIM(CurrentModuleObject)//' Name')
    IF (IsNotOK) THEN
      ErrorsFound = .TRUE.
      IF (IsBlank) AlphaName(1) = 'xxxxx'
    END IF
    ZoneContamGenericConstant(Loop)%Name = AlphaName(1)

    ZoneContamGenericConstant(Loop)%ZoneName=AlphaName(2)
    ZoneContamGenericConstant(Loop)%ActualZoneNum=FindIteminList(AlphaName(2),Zone%Name,NumOfZones)
    IF (ZoneContamGenericConstant(Loop)%ActualZoneNum == 0) THEN
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(AlphaName(1))//  &
                           '", invalid '//TRIM(cAlphaFieldNames(2))//  &
                           ' entered='//TRIM(AlphaName(2)))
      ErrorsFound=.true.
    ENDIF

    ZoneContamGenericConstant(Loop)%GCGenerateRateSchedPtr=GetScheduleIndex(AlphaName(3))
    IF (ZoneContamGenericConstant(Loop)%GCGenerateRateSchedPtr == 0) THEN
      IF (lAlphaFieldBlanks(3)) THEN
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(AlphaName(1))//  &
                             '", '//TRIM(cAlphaFieldNames(3))//' is required.')
      ELSE
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(AlphaName(1))//  &
                             '", invalid '//TRIM(cAlphaFieldNames(3))//' entered='//TRIM(AlphaName(3)))
      ENDIF
      ErrorsFound=.true.
    ELSE  ! check min/max on schedule
      SchMin=GetScheduleMinValue(ZoneContamGenericConstant(Loop)%GCGenerateRateSchedPtr)
      SchMax=GetScheduleMaxValue(ZoneContamGenericConstant(Loop)%GCGenerateRateSchedPtr)
      IF (SchMin < 0.0d0 .or. SchMax < 0.0d0) THEN
        IF (SchMin < 0.0d0) THEN
          CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(AlphaName(1))//  &
                             '", '//TRIM(cAlphaFieldNames(3))//', minimum is < 0.0')
          CALL ShowContinueError('Schedule="'//TRIM(AlphaName(3))//  &
                             '". Minimum is ['//TRIM(RoundSigDigits(SchMin,1))//']. Values must be >= 0.0.')
          ErrorsFound=.true.
        ENDIF
        IF (SchMax < 0.0d0) THEN
          CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(AlphaName(1))//  &
                             '", '//TRIM(cAlphaFieldNames(3))//', maximum is < 0.0')
          CALL ShowContinueError('Schedule="'//TRIM(AlphaName(3))//  &
                             '". Maximum is ['//TRIM(RoundSigDigits(SchMax,1))//']. Values must be >= 0.0.')
          ErrorsFound=.true.
        ENDIF
      ENDIF
    ENDIF

    ZoneContamGenericConstant(Loop)%GCGenerateRate = IHGNumbers(1)
    ZoneContamGenericConstant(Loop)%GCRemovalCoef  = IHGNumbers(2)

    ZoneContamGenericConstant(Loop)%GCRemovalCoefSchedPtr=GetScheduleIndex(AlphaName(4))
    IF (ZoneContamGenericConstant(Loop)%GCRemovalCoefSchedPtr == 0) THEN
      IF (lAlphaFieldBlanks(3)) THEN
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(AlphaName(1))//  &
                             '", '//TRIM(cAlphaFieldNames(4))//' is required.')
      ELSE
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(AlphaName(1))//  &
                             '", invalid '//TRIM(cAlphaFieldNames(4))//' entered='//TRIM(AlphaName(4)))
      ENDIF
      ErrorsFound=.true.
    ELSE  ! check min/max on schedule
      SchMin=GetScheduleMinValue(ZoneContamGenericConstant(Loop)%GCRemovalCoefSchedPtr)
      SchMax=GetScheduleMaxValue(ZoneContamGenericConstant(Loop)%GCRemovalCoefSchedPtr)
      IF (SchMin < 0.0d0 .or. SchMax < 0.0d0) THEN
        IF (SchMin < 0.0d0) THEN
          CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(AlphaName(1))//  &
                             '", '//TRIM(cAlphaFieldNames(4))//', minimum is < 0.0')
          CALL ShowContinueError('Schedule="'//TRIM(AlphaName(4))//  &
                             '". Minimum is ['//TRIM(RoundSigDigits(SchMin,1))//']. Values must be >= 0.0.')
          ErrorsFound=.true.
        ENDIF
        IF (SchMax < 0.0d0) THEN
          CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(AlphaName(1))//  &
                             '", '//TRIM(cAlphaFieldNames(4))//', maximum is < 0.0')
          CALL ShowContinueError('Schedule="'//TRIM(AlphaName(4))//  &
                             '". Maximum is ['//TRIM(RoundSigDigits(SchMax,1))//']. Values must be >= 0.0.')
          ErrorsFound=.true.
        ENDIF
      ENDIF
    ENDIF

    IF (ZoneContamGenericConstant(Loop)%ActualZoneNum <=0) CYCLE   ! Error, will be caught and terminated later

    ! Object report variables
    CALL SetupOutputVariable('Generic Air Contaminant Constant Source Generation Volume Flow Rate [m3/s]', &
                              ZoneContamGenericConstant(Loop)%GCGenRate, &
                             'Zone','Average',ZoneContamGenericConstant(Loop)%Name)

    ! Zone total report variables
    ZonePtr = ZoneContamGenericConstant(Loop)%ActualZoneNum
    IF (RepVarSet(ZonePtr)) THEN
      RepVarSet(ZonePtr)=.false.
      CALL SetupOutputVariable('Zone Generic Air Contaminant Generation Volume Flow Rate [m3/s]', &
          ZnRpt(ZonePtr)%GCRate,'Zone','Average', Zone(ZonePtr)%Name)
    ENDIF
    CALL SetupZoneInternalGain(ZonePtr, 'ZoneContaminantSourceAndSink:GenericContaminant', &
               ZoneContamGenericConstant(Loop)%Name, IntGainTypeOf_ZoneContaminantSourceAndSinkGenericContam, &
               GenericContamGainRate       = ZoneContamGenericConstant(Loop)%GCGenRate)

  END DO

  CurrentModuleObject='SurfaceContaminantSourceAndSink:Generic:PressureDriven'
  TotGCGenPDriven=GetNumObjectsFound(CurrentModuleObject)
  ALLOCATE(ZoneContamGenericPDriven(TotGCGenPDriven))

  DO Loop=1,TotGCGenPDriven
    AlphaName='  '
    IHGNumbers=0.0d0
    CALL GetObjectItem(CurrentModuleObject,Loop,AlphaName,NumAlpha,IHGNumbers,NumNumber,IOStat,  &
                   AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                   AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

    IsNotOK = .FALSE.
    IsBlank = .FALSE.
    CALL VerifyName(AlphaName(1),ZoneContamGenericPDriven%Name,Loop-1,IsNotOK,IsBlank,TRIM(CurrentModuleObject)//' Name')
    IF (IsNotOK) THEN
      ErrorsFound = .TRUE.
      IF (IsBlank) AlphaName(1) = 'xxxxx'
    END IF
    ZoneContamGenericPDriven(Loop)%Name = AlphaName(1)

    ZoneContamGenericPDriven(Loop)%SurfName=AlphaName(2)
    ZoneContamGenericPDriven(Loop)%SurfNum=FindIteminList(AlphaName(2),MultizoneSurfaceData%SurfName,AirflowNetworkNumOfSurfaces)
    IF (ZoneContamGenericPDriven(Loop)%SurfNum == 0) THEN
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(AlphaName(1))//  &
                           '", invalid '//TRIM(cAlphaFieldNames(2))//  &
                           ' entered='//TRIM(AlphaName(2)))
      ErrorsFound=.true.
    ENDIF
    ! Ensure external surface
    If (Surface(ZoneContamGenericPDriven(Loop)%SurfNum)%ExtBoundCond .NE. ExternalEnvironment) Then
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(AlphaName(1))//  &
                   '. The entered surface ('//TRIM(AlphaName(2))//') is not an exterior surface')
      ErrorsFound=.true.
    End If

    ZoneContamGenericPDriven(Loop)%GCGenRateCoefSchedPtr=GetScheduleIndex(AlphaName(3))
    IF (ZoneContamGenericPDriven(Loop)%GCGenRateCoefSchedPtr == 0) THEN
      IF (lAlphaFieldBlanks(3)) THEN
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(AlphaName(1))//  &
                             '", '//TRIM(cAlphaFieldNames(3))//' is required.')
      ELSE
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(AlphaName(1))//  &
                             '", invalid '//TRIM(cAlphaFieldNames(3))//' entered='//TRIM(AlphaName(3)))
      ENDIF
      ErrorsFound=.true.
    ELSE  ! check min/max on schedule
      SchMin=GetScheduleMinValue(ZoneContamGenericPDriven(Loop)%GCGenRateCoefSchedPtr)
      SchMax=GetScheduleMaxValue(ZoneContamGenericPDriven(Loop)%GCGenRateCoefSchedPtr)
      IF (SchMin < 0.0d0 .or. SchMax < 0.0d0) THEN
        IF (SchMin < 0.0d0) THEN
          CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(AlphaName(1))//  &
                             '", '//TRIM(cAlphaFieldNames(3))//', minimum is < 0.0')
          CALL ShowContinueError('Schedule="'//TRIM(AlphaName(3))//  &
                             '". Minimum is ['//TRIM(RoundSigDigits(SchMin,1))//']. Values must be >= 0.0.')
          ErrorsFound=.true.
        ENDIF
        IF (SchMax < 0.0d0) THEN
          CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(AlphaName(1))//  &
                             '", '//TRIM(cAlphaFieldNames(3))//', maximum is < 0.0')
          CALL ShowContinueError('Schedule="'//TRIM(AlphaName(3))//  &
                             '". Maximum is ['//TRIM(RoundSigDigits(SchMax,1))//']. Values must be >= 0.0.')
          ErrorsFound=.true.
        ENDIF
      ENDIF
    ENDIF

    ZoneContamGenericPDriven(Loop)%GCGenRateCoef = IHGNumbers(1)
    If (IHGNumbers(1) .LT. 0.d0) Then
      CALL ShowSevereError(RoutineName//'Negative values are not allowed for '//TRIM(cNumericFieldNames(1))// &
                            ' in '//TRIM(CurrentModuleObject)//' = '//TRIM(AlphaName(1)))
      CALL ShowContinueError('The input value is '//TRIM(RoundSigDigits(IHGNumbers(1),2)))
      ErrorsFound=.true.
    End If

    ZoneContamGenericPDriven(Loop)%GCExpo = IHGNumbers(2)
    If (IHGNumbers(2) .LE. 0.d0) Then
      CALL ShowSevereError(RoutineName//'Negative or zero value is not allowed for '//TRIM(cNumericFieldNames(2))// &
                            ' in '//TRIM(CurrentModuleObject)//' = '//TRIM(AlphaName(1)))
      CALL ShowContinueError('The input value is '//TRIM(RoundSigDigits(IHGNumbers(2),2)))
      ErrorsFound=.true.
    End If
    If (IHGNumbers(2) .GT. 1.d0) Then
      CALL ShowSevereError(RoutineName//'The value greater than 1.0 is not allowed for '//TRIM(cNumericFieldNames(2))// &
                            ' in '//TRIM(CurrentModuleObject)//' = '//TRIM(AlphaName(1)))
      CALL ShowContinueError('The input value is '//TRIM(RoundSigDigits(IHGNumbers(2),2)))
      ErrorsFound=.true.
    End If

    ! Object report variables
    CALL SetupOutputVariable('Generic Air Contaminant Pressure Driven Generation Volume Flow Rate [m3/s]', &
                             ZoneContamGenericPDriven(Loop)%GCGenRate,'Zone','Average',ZoneContamGenericPDriven(Loop)%Name)

    ZonePtr = Surface(ZoneContamGenericPDriven(Loop)%SurfNum)%Zone
    ! Zone total report variables
    IF (RepVarSet(ZonePtr)) THEN
      RepVarSet(ZonePtr)=.false.
      CALL SetupOutputVariable('Zone Generic Air Contaminant Generation Volume Flow Rate [m3/s]', &
          ZnRpt(ZonePtr)%GCRate,'Zone','Average', Zone(ZonePtr)%Name)
    ENDIF
    CALL SetupZoneInternalGain(ZonePtr, 'ZoneContaminantSourceAndSink:GenericContaminant', &
               ZoneContamGenericPDriven(Loop)%Name, &
               IntGainTypeOf_ZoneContaminantSourceAndSinkGenericContam, &
               GenericContamGainRate       = ZoneContamGenericPDriven(Loop)%GCGenRate)
  END DO

  CurrentModuleObject='ZoneContaminantSourceAndSink:Generic:CutoffModel'
  TotGCGenCutoff=GetNumObjectsFound(CurrentModuleObject)
  ALLOCATE(ZoneContamGenericCutoff(TotGCGenCutoff))

  DO Loop=1,TotGCGenCutoff
    AlphaName='  '
    IHGNumbers=0.0d0
    CALL GetObjectItem(CurrentModuleObject,Loop,AlphaName,NumAlpha,IHGNumbers,NumNumber,IOStat,  &
                   AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                   AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

    IsNotOK = .FALSE.
    IsBlank = .FALSE.
    CALL VerifyName(AlphaName(1),ZoneContamGenericCutoff%Name,Loop-1,IsNotOK,IsBlank,TRIM(CurrentModuleObject)//' Name')
    IF (IsNotOK) THEN
      ErrorsFound = .TRUE.
      IF (IsBlank) AlphaName(1) = 'xxxxx'
    END IF
    ZoneContamGenericCutoff(Loop)%Name = AlphaName(1)

    ZoneContamGenericCutoff(Loop)%ZoneName=AlphaName(2)
    ZoneContamGenericCutoff(Loop)%ActualZoneNum=FindIteminList(AlphaName(2),Zone%Name,NumOfZones)
    IF (ZoneContamGenericCutoff(Loop)%ActualZoneNum == 0) THEN
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(AlphaName(1))//  &
                           '", invalid '//TRIM(cAlphaFieldNames(2))//  &
                           ' entered='//TRIM(AlphaName(2)))
      ErrorsFound=.true.
    ENDIF

    ZoneContamGenericCutoff(Loop)%GCGenerateRateSchedPtr=GetScheduleIndex(AlphaName(3))
    IF (ZoneContamGenericCutoff(Loop)%GCGenerateRateSchedPtr == 0) THEN
      IF (lAlphaFieldBlanks(3)) THEN
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(AlphaName(1))//  &
                             '", '//TRIM(cAlphaFieldNames(3))//' is required.')
      ELSE
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(AlphaName(1))//  &
                             '", invalid '//TRIM(cAlphaFieldNames(3))//' entered='//TRIM(AlphaName(3)))
      ENDIF
      ErrorsFound=.true.
    ELSE  ! check min/max on schedule
      SchMin=GetScheduleMinValue(ZoneContamGenericCutoff(Loop)%GCGenerateRateSchedPtr)
      SchMax=GetScheduleMaxValue(ZoneContamGenericCutoff(Loop)%GCGenerateRateSchedPtr)
      IF (SchMin < 0.0d0 .or. SchMax < 0.0d0) THEN
        IF (SchMin < 0.0d0) THEN
          CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(AlphaName(1))//  &
                             '", '//TRIM(cAlphaFieldNames(3))//', minimum is < 0.0')
          CALL ShowContinueError('Schedule="'//TRIM(AlphaName(3))//  &
                             '". Minimum is ['//TRIM(RoundSigDigits(SchMin,1))//']. Values must be >= 0.0.')
          ErrorsFound=.true.
        ENDIF
        IF (SchMax < 0.0d0) THEN
          CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(AlphaName(1))//  &
                             '", '//TRIM(cAlphaFieldNames(3))//', maximum is < 0.0')
          CALL ShowContinueError('Schedule="'//TRIM(AlphaName(3))//  &
                             '". Maximum is ['//TRIM(RoundSigDigits(SchMax,1))//']. Values must be >= 0.0.')
          ErrorsFound=.true.
        ENDIF
      ENDIF
    ENDIF

    ZoneContamGenericCutoff(Loop)%GCGenerateRate = IHGNumbers(1)
    ZoneContamGenericCutoff(Loop)%GCCutoffValue  = IHGNumbers(2)

    If (IHGNumbers(1) .LT. 0.d0) Then
      CALL ShowSevereError(RoutineName//'Negative values are not allowed for '//TRIM(cNumericFieldNames(1))// &
                            ' in '//TRIM(CurrentModuleObject)//' = '//TRIM(AlphaName(1)))
      CALL ShowContinueError('The input value is '//TRIM(RoundSigDigits(IHGNumbers(1),2)))
      ErrorsFound=.true.
    End If
    If (IHGNumbers(2) .LE. 0.d0) Then
      CALL ShowSevereError(RoutineName//'Negative values or zero are not allowed for '//TRIM(cNumericFieldNames(2))// &
                            ' in '//TRIM(CurrentModuleObject)//' = '//TRIM(AlphaName(1)))
      CALL ShowContinueError('The input value is '//TRIM(RoundSigDigits(IHGNumbers(2),2)))
      ErrorsFound=.true.
    End If

    ! Object report variables
    CALL SetupOutputVariable('Generic Air Contaminant Cutoff Model Generation Volume Flow Rate [m3/s]', &
                             ZoneContamGenericCutoff(Loop)%GCGenRate,'Zone','Average',ZoneContamGenericCutoff(Loop)%Name)

    ! Zone total report variables
    ZonePtr = ZoneContamGenericCutoff(Loop)%ActualZoneNum
    IF (RepVarSet(ZonePtr)) THEN
      RepVarSet(ZonePtr)=.false.
      CALL SetupOutputVariable('Zone Generic Air Contaminant Generation Volume Flow Rate [m3/s]', &
          ZnRpt(ZonePtr)%GCRate,'Zone','Average', Zone(ZonePtr)%Name)
    ENDIF
    CALL SetupZoneInternalGain(ZonePtr, 'ZoneContaminantSourceAndSink:GenericContaminant', &
               ZoneContamGenericCutoff(Loop)%Name, IntGainTypeOf_ZoneContaminantSourceAndSinkGenericContam, &
               GenericContamGainRate       = ZoneContamGenericCutoff(Loop)%GCGenRate)
  END DO

  CurrentModuleObject='ZoneContaminantSourceAndSink:Generic:DecaySource'
  TotGCGenDecay=GetNumObjectsFound(CurrentModuleObject)
  ALLOCATE(ZoneContamGenericDecay(TotGCGenDecay))

  DO Loop=1,TotGCGenDecay
    AlphaName='  '
    IHGNumbers=0.0d0
    CALL GetObjectItem(CurrentModuleObject,Loop,AlphaName,NumAlpha,IHGNumbers,NumNumber,IOStat,  &
                   AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                   AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

    IsNotOK = .FALSE.
    IsBlank = .FALSE.
    CALL VerifyName(AlphaName(1),ZoneContamGenericDecay%Name,Loop-1,IsNotOK,IsBlank,TRIM(CurrentModuleObject)//' Name')
    IF (IsNotOK) THEN
      ErrorsFound = .TRUE.
      IF (IsBlank) AlphaName(1) = 'xxxxx'
    END IF
    ZoneContamGenericDecay(Loop)%Name = AlphaName(1)

    ZoneContamGenericDecay(Loop)%ZoneName=AlphaName(2)
    ZoneContamGenericDecay(Loop)%ActualZoneNum=FindIteminList(AlphaName(2),Zone%Name,NumOfZones)
    IF (ZoneContamGenericDecay(Loop)%ActualZoneNum == 0) THEN
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(AlphaName(1))//  &
                           '", invalid '//TRIM(cAlphaFieldNames(2))//  &
                           ' entered='//TRIM(AlphaName(2)))
      ErrorsFound=.true.
    ENDIF

    ZoneContamGenericDecay(Loop)%GCEmiRateSchedPtr=GetScheduleIndex(AlphaName(3))
    IF (ZoneContamGenericDecay(Loop)%GCEmiRateSchedPtr == 0) THEN
      IF (lAlphaFieldBlanks(3)) THEN
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(AlphaName(1))//  &
                             '", '//TRIM(cAlphaFieldNames(3))//' is required.')
      ELSE
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(AlphaName(1))//  &
                             '", invalid '//TRIM(cAlphaFieldNames(3))//' entered='//TRIM(AlphaName(3)))
      ENDIF
      ErrorsFound=.true.
    ELSE  ! check min/max on schedule
      SchMin=GetScheduleMinValue(ZoneContamGenericDecay(Loop)%GCEmiRateSchedPtr)
      SchMax=GetScheduleMaxValue(ZoneContamGenericDecay(Loop)%GCEmiRateSchedPtr)
      IF (SchMin < 0.0d0 .or. SchMax < 0.0d0) THEN
        IF (SchMin < 0.0d0) THEN
          CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(AlphaName(1))//  &
                             '", '//TRIM(cAlphaFieldNames(3))//', minimum is < 0.0')
          CALL ShowContinueError('Schedule="'//TRIM(AlphaName(3))//  &
                             '". Minimum is ['//TRIM(RoundSigDigits(SchMin,1))//']. Values must be >= 0.0.')
          ErrorsFound=.true.
        ENDIF
        IF (SchMax < 0.0d0) THEN
          CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(AlphaName(1))//  &
                             '", '//TRIM(cAlphaFieldNames(3))//', maximum is < 0.0')
          CALL ShowContinueError('Schedule="'//TRIM(AlphaName(3))//  &
                             '". Maximum is ['//TRIM(RoundSigDigits(SchMax,1))//']. Values must be >= 0.0.')
          ErrorsFound=.true.
        ENDIF
      ENDIF
    ENDIF

    ZoneContamGenericDecay(Loop)%GCInitEmiRate = IHGNumbers(1)
    ZoneContamGenericDecay(Loop)%GCDelayTime  = IHGNumbers(2)

    If (IHGNumbers(1) .LT. 0.d0) Then
      CALL ShowSevereError(RoutineName//'Negative values are not allowed for '//TRIM(cNumericFieldNames(1))// &
                            ' in '//TRIM(CurrentModuleObject)//' = '//TRIM(AlphaName(1)))
      CALL ShowContinueError('The input value is '//TRIM(RoundSigDigits(IHGNumbers(1),2)))
      ErrorsFound=.true.
    End If
    If (IHGNumbers(2) .LE. 0.d0) Then
      CALL ShowSevereError(RoutineName//'Negative values or zero are not allowed for '//TRIM(cNumericFieldNames(2))// &
                            ' in '//TRIM(CurrentModuleObject)//' = '//TRIM(AlphaName(1)))
      CALL ShowContinueError('The input value is '//TRIM(RoundSigDigits(IHGNumbers(2),2)))
      ErrorsFound=.true.
    End If

    ! Object report variables
    CALL SetupOutputVariable('Generic Air Contaminant Decay Model Generation Volume Flow Rate [m3/s]', &
                             ZoneContamGenericDecay(Loop)%GCGenRate,'Zone','Average',ZoneContamGenericDecay(Loop)%Name)
    CALL SetupOutputVariable('Generic Air Contaminant Decay Model Generation Emission Start Elapsed Time [s]', &
                             ZoneContamGenericDecay(Loop)%GCTime,'Zone','Average',ZoneContamGenericDecay(Loop)%Name)

    ! Zone total report variables
    ZonePtr = ZoneContamGenericDecay(Loop)%ActualZoneNum
    IF (RepVarSet(ZonePtr)) THEN
      RepVarSet(ZonePtr)=.false.
      CALL SetupOutputVariable('Zone Generic Air Contaminant Generation Volume Flow Rate [m3/s]', &
          ZnRpt(ZonePtr)%GCRate,'Zone','Average', Zone(ZonePtr)%Name)
    ENDIF
    CALL SetupZoneInternalGain(ZonePtr, 'ZoneContaminantSourceAndSink:GenericContaminant', &
               ZoneContamGenericDecay(Loop)%Name, IntGainTypeOf_ZoneContaminantSourceAndSinkGenericContam, &
               GenericContamGainRate       = ZoneContamGenericDecay(Loop)%GCGenRate)
  END DO

  CurrentModuleObject='SurfaceContaminantSourceAndSink:Generic:BoundaryLayerDiffusion'
  TotGCBLDiff =GetNumObjectsFound(CurrentModuleObject)
  ALLOCATE(ZoneContamGenericBLDiff(TotGCBLDiff))

  DO Loop=1,TotGCBLDiff
    AlphaName='  '
    IHGNumbers=0.0d0
    CALL GetObjectItem(CurrentModuleObject,Loop,AlphaName,NumAlpha,IHGNumbers,NumNumber,IOStat,  &
                   AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                   AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

    IsNotOK = .FALSE.
    IsBlank = .FALSE.
    CALL VerifyName(AlphaName(1),ZoneContamGenericBLDiff%Name,Loop-1,IsNotOK,IsBlank,TRIM(CurrentModuleObject)//' Name')
    IF (IsNotOK) THEN
      ErrorsFound = .TRUE.
      IF (IsBlank) AlphaName(1) = 'xxxxx'
    END IF
    ZoneContamGenericBLDiff(Loop)%Name = AlphaName(1)

    ZoneContamGenericBLDiff(Loop)%SurfName=AlphaName(2)
    ZoneContamGenericBLDiff(Loop)%SurfNum=FindIteminList(AlphaName(2),Surface%Name,TotSurfaces)
    IF (ZoneContamGenericBLDiff(Loop)%SurfNum == 0) THEN
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(AlphaName(1))//  &
                           '", invalid '//TRIM(cAlphaFieldNames(2))//  &
                           ' entered='//TRIM(AlphaName(2)))
      ErrorsFound=.true.
    ENDIF

    ZoneContamGenericBLDiff(Loop)%GCTranCoefSchedPtr=GetScheduleIndex(AlphaName(3))
    IF (ZoneContamGenericBLDiff(Loop)%GCTranCoefSchedPtr == 0) THEN
      IF (lAlphaFieldBlanks(3)) THEN
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(AlphaName(1))//  &
                             '", '//TRIM(cAlphaFieldNames(3))//' is required.')
      ELSE
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(AlphaName(1))//  &
                             '", invalid '//TRIM(cAlphaFieldNames(3))//' entered='//TRIM(AlphaName(3)))
      ENDIF
      ErrorsFound=.true.
    ELSE  ! check min/max on schedule
      SchMin=GetScheduleMinValue(ZoneContamGenericBLDiff(Loop)%GCTranCoefSchedPtr)
      SchMax=GetScheduleMaxValue(ZoneContamGenericBLDiff(Loop)%GCTranCoefSchedPtr)
      IF (SchMin < 0.0d0 .or. SchMax < 0.0d0) THEN
        IF (SchMin < 0.0d0) THEN
          CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(AlphaName(1))//  &
                             '", '//TRIM(cAlphaFieldNames(3))//', minimum is < 0.0')
          CALL ShowContinueError('Schedule="'//TRIM(AlphaName(3))//  &
                             '". Minimum is ['//TRIM(RoundSigDigits(SchMin,1))//']. Values must be >= 0.0.')
          ErrorsFound=.true.
        ENDIF
        IF (SchMax < 0.0d0) THEN
          CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(AlphaName(1))//  &
                             '", '//TRIM(cAlphaFieldNames(3))//', maximum is < 0.0')
          CALL ShowContinueError('Schedule="'//TRIM(AlphaName(3))//  &
                             '". Maximum is ['//TRIM(RoundSigDigits(SchMax,1))//']. Values must be >= 0.0.')
          ErrorsFound=.true.
        ENDIF
      ENDIF
    ENDIF

    ZoneContamGenericBLDiff(Loop)%GCTranCoef = IHGNumbers(1)
    ZoneContamGenericBLDiff(Loop)%GCHenryCoef = IHGNumbers(2)
    If (IHGNumbers(1) .LT. 0.d0) Then
      CALL ShowSevereError(RoutineName//'Negative values are not allowed for '//TRIM(cNumericFieldNames(1))// &
                            ' in '//TRIM(CurrentModuleObject)//' = '//TRIM(AlphaName(1)))
      CALL ShowContinueError('The input value is '//TRIM(RoundSigDigits(IHGNumbers(1),2)))
      ErrorsFound=.true.
    End If
    If (IHGNumbers(2) .LE. 0.d0) Then
      CALL ShowSevereError(RoutineName//'Negative values or zero are not allowed for '//TRIM(cNumericFieldNames(2))// &
                            ' in '//TRIM(CurrentModuleObject)//' = '//TRIM(AlphaName(1)))
      CALL ShowContinueError('The input value is '//TRIM(RoundSigDigits(IHGNumbers(2),2)))
      ErrorsFound=.true.
    End If

    ! Object report variables
    CALL SetupOutputVariable('Generic Air Contaminant Boundary Layer Diffusion Generation Volume Flow Rate [m3/s]', &
                             ZoneContamGenericBLDiff(Loop)%GCGenRate,'Zone','Average',ZoneContamGenericBLDiff(Loop)%Name)
    If (ZoneContamGenericBLDiff(Loop)%SurfNum .GT. 0) &
      CALL SetupOutputVariable('Generic Air Contaminant Boundary Layer Diffusion Inside Face Concentration [ppm]', &
       Surface(ZoneContamGenericBLDiff(Loop)%SurfNum)%GenericContam,'Zone','Average',ZoneContamGenericBLDiff(Loop)%SurfName)

    ZonePtr = Surface(ZoneContamGenericBLDiff(Loop)%SurfNum)%Zone
    ! Zone total report variables
    IF (RepVarSet(ZonePtr)) THEN
      RepVarSet(ZonePtr)=.false.
      CALL SetupOutputVariable('Zone Generic Air Contaminant Generation Volume Flow Rate [m3/s]', &
          ZnRpt(ZonePtr)%GCRate,'Zone','Average', Zone(ZonePtr)%Name)
    ENDIF
    CALL SetupZoneInternalGain(ZonePtr, 'ZoneContaminantSourceAndSink:GenericContaminant', &
               ZoneContamGenericBLDiff(Loop)%Name, IntGainTypeOf_ZoneContaminantSourceAndSinkGenericContam, &
               GenericContamGainRate       = ZoneContamGenericBLDiff(Loop)%GCGenRate)
  END DO

  CurrentModuleObject='SurfaceContaminantSourceAndSink:Generic:DepositionVelocitySink'
  TotGCDVS =GetNumObjectsFound(CurrentModuleObject)
  ALLOCATE(ZoneContamGenericDVS(TotGCDVS))

  DO Loop=1,TotGCDVS
    AlphaName='  '
    IHGNumbers=0.0d0
    CALL GetObjectItem(CurrentModuleObject,Loop,AlphaName,NumAlpha,IHGNumbers,NumNumber,IOStat,  &
                   AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                   AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

    IsNotOK = .FALSE.
    IsBlank = .FALSE.
    CALL VerifyName(AlphaName(1),ZoneContamGenericDVS%Name,Loop-1,IsNotOK,IsBlank,TRIM(CurrentModuleObject)//' Name')
    IF (IsNotOK) THEN
      ErrorsFound = .TRUE.
      IF (IsBlank) AlphaName(1) = 'xxxxx'
    END IF
    ZoneContamGenericDVS(Loop)%Name = AlphaName(1)

    ZoneContamGenericDVS(Loop)%SurfName=AlphaName(2)
    ZoneContamGenericDVS(Loop)%SurfNum=FindIteminList(AlphaName(2),Surface%Name,TotSurfaces)
    IF (ZoneContamGenericDVS(Loop)%SurfNum == 0) THEN
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(AlphaName(1))//  &
                           '", invalid '//TRIM(cAlphaFieldNames(2))//  &
                           ' entered='//TRIM(AlphaName(2)))
      ErrorsFound=.true.
    ENDIF

    ZoneContamGenericDVS(Loop)%GCDepoVeloPtr=GetScheduleIndex(AlphaName(3))
    IF (ZoneContamGenericDVS(Loop)%GCDepoVeloPtr == 0) THEN
      IF (lAlphaFieldBlanks(3)) THEN
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(AlphaName(1))//  &
                             '", '//TRIM(cAlphaFieldNames(3))//' is required.')
      ELSE
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(AlphaName(1))//  &
                             '", invalid '//TRIM(cAlphaFieldNames(3))//' entered='//TRIM(AlphaName(3)))
      ENDIF
      ErrorsFound=.true.
    ELSE  ! check min/max on schedule
      SchMin=GetScheduleMinValue(ZoneContamGenericDVS(Loop)%GCDepoVeloPtr)
      SchMax=GetScheduleMaxValue(ZoneContamGenericDVS(Loop)%GCDepoVeloPtr)
      IF (SchMin < 0.0d0 .or. SchMax < 0.0d0) THEN
        IF (SchMin < 0.0d0) THEN
          CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(AlphaName(1))//  &
                             '", '//TRIM(cAlphaFieldNames(3))//', minimum is < 0.0')
          CALL ShowContinueError('Schedule="'//TRIM(AlphaName(3))//  &
                             '". Minimum is ['//TRIM(RoundSigDigits(SchMin,1))//']. Values must be >= 0.0.')
          ErrorsFound=.true.
        ENDIF
        IF (SchMax < 0.0d0) THEN
          CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(AlphaName(1))//  &
                             '", '//TRIM(cAlphaFieldNames(3))//', maximum is < 0.0')
          CALL ShowContinueError('Schedule="'//TRIM(AlphaName(3))//  &
                             '". Maximum is ['//TRIM(RoundSigDigits(SchMax,1))//']. Values must be >= 0.0.')
          ErrorsFound=.true.
        ENDIF
      ENDIF
    ENDIF

    ZoneContamGenericDVS(Loop)%GCDepoVelo = IHGNumbers(1)
    If (IHGNumbers(1) .LT. 0.d0) Then
      CALL ShowSevereError(RoutineName//'Negative values are not allowed for '//TRIM(cNumericFieldNames(1))// &
                            ' in '//TRIM(CurrentModuleObject)//' = '//TRIM(AlphaName(1)))
      CALL ShowContinueError('The input value is '//TRIM(RoundSigDigits(IHGNumbers(1),2)))
      ErrorsFound=.true.
    End If

    ! Object report variables
    CALL SetupOutputVariable('Generic Air Contaminant Deposition Velocity Removal Volume Flow Rate [m3/s]', &
                             ZoneContamGenericDVS(Loop)%GCGenRate,'Zone','Average',ZoneContamGenericDVS(Loop)%Name)

    ZonePtr = Surface(ZoneContamGenericDVS(Loop)%SurfNum)%Zone
    ! Zone total report variables
    IF (RepVarSet(ZonePtr)) THEN
      RepVarSet(ZonePtr)=.false.
      CALL SetupOutputVariable('Zone Generic Air Contaminant Generation Volume Flow Rate [m3/s]', &
          ZnRpt(ZonePtr)%GCRate,'Zone','Average', Zone(ZonePtr)%Name)
    ENDIF
    CALL SetupZoneInternalGain(ZonePtr, 'ZoneContaminantSourceAndSink:GenericContaminant', &
               ZoneContamGenericDVS(Loop)%Name, IntGainTypeOf_ZoneContaminantSourceAndSinkGenericContam, &
               GenericContamGainRate       = ZoneContamGenericDVS(Loop)%GCGenRate)
  END DO

  CurrentModuleObject='ZoneContaminantSourceAndSink:Generic:DepositionRateSink'
  TotGCDRS=GetNumObjectsFound(CurrentModuleObject)
  ALLOCATE(ZoneContamGenericDRS(TotGCDRS))

  DO Loop=1,TotGCDRS
    AlphaName='  '
    IHGNumbers=0.0d0
    CALL GetObjectItem(CurrentModuleObject,Loop,AlphaName,NumAlpha,IHGNumbers,NumNumber,IOStat,  &
                   AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                   AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

    IsNotOK = .FALSE.
    IsBlank = .FALSE.
    CALL VerifyName(AlphaName(1),ZoneContamGenericDRS%Name,Loop-1,IsNotOK,IsBlank,TRIM(CurrentModuleObject)//' Name')
    IF (IsNotOK) THEN
      ErrorsFound = .TRUE.
      IF (IsBlank) AlphaName(1) = 'xxxxx'
    END IF
    ZoneContamGenericDRS(Loop)%Name = AlphaName(1)

    ZoneContamGenericDRS(Loop)%ZoneName=AlphaName(2)
    ZoneContamGenericDRS(Loop)%ActualZoneNum=FindIteminList(AlphaName(2),Zone%Name,NumOfZones)
    IF (ZoneContamGenericDRS(Loop)%ActualZoneNum == 0) THEN
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(AlphaName(1))//  &
                           '", invalid '//TRIM(cAlphaFieldNames(2))//  &
                           ' entered='//TRIM(AlphaName(2)))
      ErrorsFound=.true.
    ENDIF

    ZoneContamGenericDRS(Loop)%GCDepoRatePtr=GetScheduleIndex(AlphaName(3))
    IF (ZoneContamGenericDRS(Loop)%GCDepoRatePtr == 0) THEN
      IF (lAlphaFieldBlanks(3)) THEN
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(AlphaName(1))//  &
                             '", '//TRIM(cAlphaFieldNames(3))//' is required.')
      ELSE
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(AlphaName(1))//  &
                             '", invalid '//TRIM(cAlphaFieldNames(3))//' entered='//TRIM(AlphaName(3)))
      ENDIF
      ErrorsFound=.true.
    ELSE  ! check min/max on schedule
      SchMin=GetScheduleMinValue(ZoneContamGenericDRS(Loop)%GCDepoRatePtr)
      SchMax=GetScheduleMaxValue(ZoneContamGenericDRS(Loop)%GCDepoRatePtr)
      IF (SchMin < 0.0d0 .or. SchMax < 0.0d0) THEN
        IF (SchMin < 0.0d0) THEN
          CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(AlphaName(1))//  &
                             '", '//TRIM(cAlphaFieldNames(3))//', minimum is < 0.0')
          CALL ShowContinueError('Schedule="'//TRIM(AlphaName(3))//  &
                             '". Minimum is ['//TRIM(RoundSigDigits(SchMin,1))//']. Values must be >= 0.0.')
          ErrorsFound=.true.
        ENDIF
        IF (SchMax < 0.0d0) THEN
          CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(AlphaName(1))//  &
                             '", '//TRIM(cAlphaFieldNames(3))//', maximum is < 0.0')
          CALL ShowContinueError('Schedule="'//TRIM(AlphaName(3))//  &
                             '". Maximum is ['//TRIM(RoundSigDigits(SchMax,1))//']. Values must be >= 0.0.')
          ErrorsFound=.true.
        ENDIF
      ENDIF
    ENDIF

    ZoneContamGenericDRS(Loop)%GCDepoRate = IHGNumbers(1)

    If (IHGNumbers(1) .LT. 0.d0) Then
      CALL ShowSevereError(RoutineName//'Negative values are not allowed for '//TRIM(cNumericFieldNames(1))// &
                            ' in '//TRIM(CurrentModuleObject)//' = '//TRIM(AlphaName(1)))
      CALL ShowContinueError('The input value is '//TRIM(RoundSigDigits(IHGNumbers(1),2)))
      ErrorsFound=.true.
    End If

    ! Object report variables
    CALL SetupOutputVariable('Generic Air Contaminant Deposition Rate Removal Volume Flow Rate [m3/s]', &
                             ZoneContamGenericDRS(Loop)%GCGenRate,'Zone','Average',ZoneContamGenericDRS(Loop)%Name)

    ZonePtr = ZoneContamGenericDRS(Loop)%ActualZoneNum
    ! Zone total report variables
    IF (RepVarSet(ZonePtr)) THEN
      RepVarSet(ZonePtr)=.false.
      CALL SetupOutputVariable('Zone Generic Air Contaminant Generation Volume Flow Rate [m3/s]', &
          ZnRpt(ZonePtr)%GCRate,'Zone','Average', Zone(ZonePtr)%Name)
    ENDIF
    CALL SetupZoneInternalGain(ZonePtr, 'ZoneContaminantSourceAndSink:GenericContaminant', &
               ZoneContamGenericDRS(Loop)%Name, IntGainTypeOf_ZoneContaminantSourceAndSinkGenericContam, &
               GenericContamGainRate       = ZoneContamGenericDRS(Loop)%GCGenRate)
  END DO

  DEALLOCATE(RepVarSet)
  DEALLOCATE(IHGNumbers)
  DEALLOCATE(AlphaName)

  IF (ErrorsFound) THEN
    CALL ShowFatalError('Errors getting Zone Contaminant Sources and Sinks input data.  Preceding condition(s) cause termination.')
  END IF


  RETURN

END SUBROUTINE GetZoneContaminanInputs

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

          ! FLOW:
  cCurrentModuleObject='ZoneControl:ContaminantController'
  NumContControlledZones = GetNumObjectsFound(cCurrentModuleObject)

  IF (NumContControlledZones .GT. 0) THEN
    ALLOCATE(ContaminantControlledZone(NumContControlledZones))
  ENDIF

  DO ContControlledZoneNum = 1, NumContControlledZones
    CALL GetObjectItem(cCurrentModuleObject,ContControlledZoneNum,cAlphaArgs,NumAlphas,rNumericArgs,NumNums,IOSTAT, &
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
    IF (lAlphaFieldBlanks(3)) THEN
      ContaminantControlledZone(ContControlledZoneNum)%AvaiSchedPtr=ScheduleAlwaysOn ! (Returns 1.0)
    ELSE
      ContaminantControlledZone(ContControlledZoneNum)%AvaiSchedPtr=GetScheduleIndex(cAlphaArgs(3))
      IF (ContaminantControlledZone(ContControlledZoneNum)%AvaiSchedPtr == 0) THEN
        CALL ShowSevereError(trim(cCurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid '//  &
           trim(cAlphaFieldNames(3))//'="'//trim(cAlphaArgs(3))//'" not found.')
        ErrorsFound = .TRUE.
      ELSE
        ! Check validity of control types.
        ValidScheduleType=  &
           CheckScheduleValueMinMax(ContaminantControlledZone(ContControlledZoneNum)%AvaiSchedPtr,'>=',0.0d0,'<=',1.0d0)
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
    ENDIF

    ContaminantControlledZone(ContControlledZoneNum)%SetPointSchedName = cAlphaArgs(4)
    ContaminantControlledZone(ContControlledZoneNum)%SPSchedIndex=GetScheduleIndex(cAlphaArgs(4))
    IF (ContaminantControlledZone(ContControlledZoneNum)%SPSchedIndex == 0) THEN
      CALL ShowSevereError(trim(cCurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid '//  &
         trim(cAlphaFieldNames(4))//'="'//trim(cAlphaArgs(4))//'" not found.')
      ErrorsFound = .TRUE.
    ELSE
      ! Check validity of control types.
      ValidScheduleType=  &
         CheckScheduleValueMinMax(ContaminantControlledZone(ContControlledZoneNum)%SPSchedIndex,'>=',0.0d0,'<=',2000.0d0)
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
      ValidScheduleType=CheckScheduleValueMinMax(ContaminantControlledZone(ContControlledZoneNum)%ZoneMinCO2SchedIndex,  &
             '>=',0.0d0,'<=',2000.0d0)
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

    If (NumAlphas .GT. 5) Then
      ContaminantControlledZone(ContControlledZoneNum)%GCAvaiSchedule = cAlphaArgs(6)
      IF (lAlphaFieldBlanks(6)) THEN
        ContaminantControlledZone(ContControlledZoneNum)%GCAvaiSchedPtr=ScheduleAlwaysOn
      ELSE
        ContaminantControlledZone(ContControlledZoneNum)%GCAvaiSchedPtr=GetScheduleIndex(cAlphaArgs(6))
        IF (ContaminantControlledZone(ContControlledZoneNum)%AvaiSchedPtr == 0) THEN
          CALL ShowSevereError(trim(cCurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid '//  &
           trim(cAlphaFieldNames(3))//'="'//trim(cAlphaArgs(6))//'" not found.')
          ErrorsFound = .TRUE.
        ELSE
        ! Check validity of control types.
          ValidScheduleType=CheckScheduleValueMinMax(ContaminantControlledZone(ContControlledZoneNum)%GCAvaiSchedPtr,  &
             '>=',0.0d0,'<=',1.0d0)
          IF (.not. ValidScheduleType) THEN
            CALL ShowSevereError(trim(cCurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid range '//  &
               trim(cAlphaFieldNames(3))//'="'//trim(cAlphaArgs(6))//'"')
            CALL ShowContinueError('..contains values outside of range [0,1].')
            ErrorsFound=.TRUE.
          ENDIF
        ENDIF
      END IF
      If (lAlphaFieldBlanks(7)) Then
        CALL ShowSevereError(trim(cCurrentModuleObject)//' "'//trim(cAlphaArgs(7))//'" is required, but blank.')
        ErrorsFound=.TRUE.
      Else
        ContaminantControlledZone(ContControlledZoneNum)%GCSetPointSchedName = cAlphaArgs(7)
        ContaminantControlledZone(ContControlledZoneNum)%GCSPSchedIndex=GetScheduleIndex(cAlphaArgs(7))
        IF (ContaminantControlledZone(ContControlledZoneNum)%GCSPSchedIndex == 0) THEN
          CALL ShowSevereError(trim(cCurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid '//  &
          trim(cAlphaFieldNames(7))//'="'//trim(cAlphaArgs(7))//'" not found.')
          ErrorsFound = .TRUE.
        END IF
      End If
    End If

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
  USE InternalHeatGains, ONLY: SumAllInternalCO2Gains, SumInternalCO2GainsByTypes, SumAllInternalGenericContamGains
  USE DataAirflowNetwork, ONLY: MultizoneSurfaceData, AirflowNetworkNodeSimu, AirflowNetworkNumOfZones,SimulateAirflowNetwork, &
                                AirflowNetworkControlSimple

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
  INTEGER :: Loop, ZoneNum, SurfNum
  LOGICAL,SAVE  :: MyOneTimeFlag = .TRUE.
  LOGICAL,SAVE  :: MyEnvrnFlag = .TRUE.
  LOGICAL,SAVE  :: MyDayFlag = .TRUE.
!  REAL(r64)      :: CO2Gain                  ! Zone CO2 gain
  REAL(r64)      :: GCGain                   ! Zone generic contaminant gain
  REAL(r64)      :: Pi                       ! Pressue at zone i
  REAL(r64)      :: Pj                       ! Pressue at zone j
  REAL(r64)      :: Sch                      ! Schedule value
  REAL(r64)      :: Cs                       ! Surface concentration level for the Boundary Layer Diffusion Controlled Model
  LOGICAL,SAVE  :: MyConfigOneTimeFlag = .TRUE.
  INTEGER :: AirLoopNum
  INTEGER :: ContZoneNum
  INTEGER :: I
  LOGICAL :: ErrorsFound=.false.

          ! FLOW:

  IF (Contaminant%CO2Simulation) Then
    OutdoorCO2 = GetCurrentScheduleValue(Contaminant%CO2OutdoorSchedPtr)
  END IF

  IF (Contaminant%GenericContamSimulation) Then
    OutdoorGC = GetCurrentScheduleValue(Contaminant%GenericContamOutdoorSchedPtr)
  END IF

  IF (MyOneTimeFlag) THEN
    ! CO2
    IF (Contaminant%CO2Simulation) Then
      ALLOCATE(ZoneCO2Setpoint(NumOfZones))
      ZoneCO2Setpoint=0.0d0
      ALLOCATE(CO2PredictedRate(NumOfZones))
      CO2PredictedRate=0.0d0
      ALLOCATE(CO2ZoneTimeMinus1(NumOfZones))
      CO2ZoneTimeMinus1=0.0d0
      ALLOCATE(CO2ZoneTimeMinus2(NumOfZones))
      CO2ZoneTimeMinus2=0.0d0
      ALLOCATE(CO2ZoneTimeMinus3(NumOfZones))
      CO2ZoneTimeMinus3=0.0d0
      ALLOCATE(CO2ZoneTimeMinus4(NumOfZones))
      CO2ZoneTimeMinus4=0.0d0
      ALLOCATE(DSCO2ZoneTimeMinus1(NumOfZones))
      DSCO2ZoneTimeMinus1=0.0d0
      ALLOCATE(DSCO2ZoneTimeMinus2(NumOfZones))
      DSCO2ZoneTimeMinus2=0.0d0
      ALLOCATE(DSCO2ZoneTimeMinus3(NumOfZones))
      DSCO2ZoneTimeMinus3=0.0d0
      ALLOCATE(DSCO2ZoneTimeMinus4(NumOfZones))
      DSCO2ZoneTimeMinus4=0.0d0
      ALLOCATE(CO2ZoneTimeMinus1Temp(NumOfZones))
      CO2ZoneTimeMinus1Temp=0.0d0
      ALLOCATE(CO2ZoneTimeMinus2Temp(NumOfZones))
      CO2ZoneTimeMinus2Temp=0.0d0
      ALLOCATE(CO2ZoneTimeMinus3Temp(NumOfZones))
      CO2ZoneTimeMinus3Temp=0.0d0
      ALLOCATE(ZoneCO2MX(NumOfZones))
      ZoneCO2MX = 0.0d0
      ALLOCATE(ZoneCO2M2(NumOfZones))
      ZoneCO2M2 = 0.0d0
      ALLOCATE(ZoneCO21(NumOfZones))
      ZoneCO21=0.0d0

      ALLOCATE(ZoneSysContDemand(NumOfZones))
      ALLOCATE(ZoneCO2Gain(NumOfZones))
      ZoneCO2Gain=0.0d0
      ALLOCATE(ZoneCO2GainFromPeople(NumOfZones))
      ZoneCO2GainFromPeople=0.0d0
      ALLOCATE(MixingMassFlowCO2(NumOfZones))
      MixingMassFlowCO2=0.0d0
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
    CONTRAT = 0.0d0

    ! Allocate Derived Types


    DO Loop = 1, NumOfZones
      ! Zone CO2
      IF (Contaminant%CO2Simulation) Then
        CALL SetupOutputVariable('Zone Air CO2 Concentration [ppm]',ZoneAirCO2(Loop),'System','Average',Zone(Loop)%Name)
        CALL SetupOutputVariable('Zone Air CO2 Predicted Load to Setpoint Mass Flow Rate [kg/s]', &
                               CO2PredictedRate(Loop),'System','Average',Zone(Loop)%Name)
        CALL SetupOutputVariable('Zone Air CO2 Setpoint Concentration [ppm]', ZoneCO2Setpoint(Loop), &
                               'System','Average',Zone(Loop)%Name)
        CALL SetupOutputVariable('Zone Air CO2 Internal Gain Volume Flow Rate [m3/s]', ZoneCO2Gain(Loop), &
                               'System','Average',Zone(Loop)%Name)
      END IF


    END DO ! Loop

    ! Generic contaminant
    IF (Contaminant%GenericContamSimulation) Then
      ALLOCATE(ZoneGCSetpoint(NumOfZones))
      ZoneGCSetpoint=0.0d0
      ALLOCATE(GCPredictedRate(NumOfZones))
      GCPredictedRate=0.0d0
      ALLOCATE(GCZoneTimeMinus1(NumOfZones))
      GCZoneTimeMinus1=0.0d0
      ALLOCATE(GCZoneTimeMinus2(NumOfZones))
      GCZoneTimeMinus2=0.0d0
      ALLOCATE(GCZoneTimeMinus3(NumOfZones))
      GCZoneTimeMinus3=0.0d0
      ALLOCATE(GCZoneTimeMinus4(NumOfZones))
      GCZoneTimeMinus4=0.0d0
      ALLOCATE(DSGCZoneTimeMinus1(NumOfZones))
      DSGCZoneTimeMinus1=0.0d0
      ALLOCATE(DSGCZoneTimeMinus2(NumOfZones))
      DSGCZoneTimeMinus2=0.0d0
      ALLOCATE(DSGCZoneTimeMinus3(NumOfZones))
      DSGCZoneTimeMinus3=0.0d0
      ALLOCATE(DSGCZoneTimeMinus4(NumOfZones))
      DSGCZoneTimeMinus4=0.0d0
      ALLOCATE(GCZoneTimeMinus1Temp(NumOfZones))
      GCZoneTimeMinus1Temp=0.0d0
      ALLOCATE(GCZoneTimeMinus2Temp(NumOfZones))
      GCZoneTimeMinus2Temp=0.0d0
      ALLOCATE(GCZoneTimeMinus3Temp(NumOfZones))
      GCZoneTimeMinus3Temp=0.0d0
      ALLOCATE(ZoneGCMX(NumOfZones))
      ZoneGCMX = 0.0d0
      ALLOCATE(ZoneGCM2(NumOfZones))
      ZoneGCM2 = 0.0d0
      ALLOCATE(ZoneGC1(NumOfZones))
      ZoneGC1=0.0d0

      IF(.NOT. ALLOCATED(ZoneSysContDemand)) ALLOCATE(ZoneSysContDemand(NumOfZones))
      ALLOCATE(ZoneGCGain(NumOfZones))
      ZoneGCGain=0.0d0
      ALLOCATE(MixingMassFlowGC(NumOfZones))
      MixingMassFlowGC=0.0d0
      ALLOCATE(ZoneAirDensityGC(NumOfZones))
      ZoneAirDensityGC = 0.d0
      !
      ALLOCATE(AZGC(NumOfZones))
      AZGC = 0.d0
      ALLOCATE(BZGC(NumOfZones))
      BZGC = 0.d0
      ALLOCATE(CZGC(NumOfZones))
      CZGC = 0.d0
    END IF

    ALLOCATE(CONTRATGC(NumOfZones))
    CONTRATGC = 0.0d0

    ! Allocate Derived Types


    DO Loop = 1, NumOfZones
      ! Zone CO2
      IF (Contaminant%GenericContamSimulation) Then
        CALL SetupOutputVariable('Zone Air Generic Air Contaminant Concentration [ppm]',ZoneAirGC(Loop),  &
           'System','Average',Zone(Loop)%Name)
        CALL SetupOutputVariable('Zone Generic Air Contaminant Predicted Load to Setpoint Mass Flow Rate [kg/s]', &
                               GCPredictedRate(Loop),'System','Average',Zone(Loop)%Name)
        CALL SetupOutputVariable('Zone Generic Air Contaminant Setpoint Concentration [ppm]', ZoneGCSetpoint(Loop), &
                               'System','Average',Zone(Loop)%Name)
      END IF
    END DO ! Loop

    MyOneTimeFlag = .FALSE.
  END IF

  ! Do the Begin Environment initializations
  IF (MyEnvrnFlag .AND. BeginEnvrnFlag) THEN
    IF (Contaminant%CO2Simulation) Then
      CONTRAT = 0.0d0
      CO2ZoneTimeMinus1 = OutdoorCO2
      CO2ZoneTimeMinus2 = OutdoorCO2
      CO2ZoneTimeMinus3 = OutdoorCO2
      CO2ZoneTimeMinus4 = OutdoorCO2
      DSCO2ZoneTimeMinus1 = OutdoorCO2
      DSCO2ZoneTimeMinus2 = OutdoorCO2
      DSCO2ZoneTimeMinus3 = OutdoorCO2
      DSCO2ZoneTimeMinus4 = OutdoorCO2
      CO2ZoneTimeMinus1Temp = 0.0d0
      CO2ZoneTimeMinus2Temp = 0.0d0
      CO2ZoneTimeMinus3Temp = 0.0d0
      ZoneAirCO2Temp = OutdoorCO2
      ZoneCO2Setpoint = 0.0d0
      CO2PredictedRate = 0.0d0
      ZoneAirCO2 = OutdoorCO2
      ZoneCO21 = OutdoorCO2
      ZoneCO2MX = OutdoorCO2
      ZoneCO2M2 = OutdoorCO2
    END IF
    IF (Contaminant%GenericContamSimulation) Then
      CONTRAT = 0.0d0
      GCZoneTimeMinus1 = OutdoorGC
      GCZoneTimeMinus2 = OutdoorGC
      GCZoneTimeMinus3 = OutdoorGC
      GCZoneTimeMinus4 = OutdoorGC
      DSGCZoneTimeMinus1 = OutdoorGC
      DSGCZoneTimeMinus2 = OutdoorGC
      DSGCZoneTimeMinus3 = OutdoorGC
      DSGCZoneTimeMinus4 = OutdoorGC
      GCZoneTimeMinus1Temp = 0.0d0
      GCZoneTimeMinus2Temp = 0.0d0
      GCZoneTimeMinus3Temp = 0.0d0
      ZoneAirGCTemp = OutdoorGC
      ZoneGCSetpoint = 0.0d0
      GCPredictedRate = 0.0d0
      ZoneAirGC = OutdoorGC
      ZoneGC1 = OutdoorGC
      ZoneGCMX = OutdoorGC
      ZoneGCM2 = OutdoorGC
      DO Loop = 1, TotGCBLDiff
        Surface(ZoneContamGenericBLDiff(Loop)%SurfNum)%GenericContam = OutdoorGC
      End Do
      If (TotGCGenDecay > 0) ZoneContamGenericDecay%GCTime = 0.0d0
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
    IF (Contaminant%GenericContamSimulation) Then
      ZoneNum = ContaminantControlledZone(Loop)%ActualZoneNum
      ZoneGCSetpoint(ZoneNum)=GetCurrentScheduleValue(ContaminantControlledZone(Loop)%GCSPSchedIndex)
    END IF
  END DO

  ! CO2 gain
  IF (Contaminant%CO2Simulation) Then
    DO Loop = 1, NumOfZones
      CALL SumAllInternalCO2Gains (Loop, ZoneCO2Gain(loop) )
      CALL SumInternalCO2GainsByTypes(Loop,  (/IntGainTypeOf_People/), ZoneCO2GainFromPeople(loop))
    ENDDO
  END IF

  ! Generic contaminant gain
  IF (Contaminant%GenericContamSimulation) Then
    ZoneGCGain = 0.d0
    ! from constant model
    Do Loop = 1, TotGCGenConstant
      ZoneNum = ZoneContamGenericConstant(Loop)%ActualZoneNum
      GCGain = ZoneContamGenericConstant(Loop)%GCGenerateRate * &
          GetCurrentScheduleValue(ZoneContamGenericConstant(Loop)%GCGenerateRateSchedPtr) - &
          ZoneContamGenericConstant(Loop)%GCRemovalCoef * &
          GetCurrentScheduleValue(ZoneContamGenericConstant(Loop)%GCRemovalCoefSchedPtr) * ZoneAirGC(ZoneNum)*1.0d-6
      ZoneContamGenericConstant(Loop)%GCGenRate = GCGain
    End Do

    ! from pressure driven model
    If (SimulateAirflowNetwork .GT. AirflowNetworkControlSimple) Then
      Do Loop = 1, TotGCGenPDriven
        SurfNum = ZoneContamGenericPDriven(Loop)%SurfNum
        Pi = AirflowNetworkNodeSimu(MultizoneSurfaceData(SurfNum)%NodeNums(1))%PZ
        Pj = AirflowNetworkNodeSimu(MultizoneSurfaceData(SurfNum)%NodeNums(2))%PZ
        If (Pj .GE. Pi) Then
          GCGain = ZoneContamGenericPDriven(Loop)%GCGenRateCoef * &
              GetCurrentScheduleValue(ZoneContamGenericPDriven(Loop)%GCGenRateCoefSchedPtr) * &
              (Pj - Pi)**ZoneContamGenericPDriven(Loop)%GCExpo
        Else
          GCGain = 0.0d0
        End If
        ZoneContamGenericPDriven(Loop)%GCGenRate = GCGain
      End Do
    End If

    ! from cutoff model
    DO Loop = 1, TotGCGenCutoff
      ZoneNum = ZoneContamGenericCutoff(Loop)%ActualZoneNum
      If (ZoneAirGC(ZoneNum) < ZoneContamGenericCutoff(Loop)%GCCutoffValue) Then
        GCGain = ZoneContamGenericCutoff(Loop)%GCGenerateRate * &
              GetCurrentScheduleValue(ZoneContamGenericCutoff(Loop)%GCGenerateRateSchedPtr) * &
              (1.d0 - ZoneAirGC(ZoneNum)/ZoneContamGenericCutoff(Loop)%GCCutoffValue)
      Else
        GCGain = 0.0d0
      End If
      ZoneContamGenericCutoff(Loop)%GCGenRate = GCGain
    End Do

    ! From decay model
    DO Loop = 1, TotGCGenDecay
      Sch = GetCurrentScheduleValue(ZoneContamGenericDecay(Loop)%GCEmiRateSchedPtr)
      ZoneNum = ZoneContamGenericDecay(Loop)%ActualZoneNum
      If (Sch .eq. 0.0d0 .OR. BeginEnvrnFlag .OR. WarmupFlag) Then
        ZoneContamGenericDecay(Loop)%GCTime = 0.0d0
      Else
        ZoneContamGenericDecay(Loop)%GCTime = ZoneContamGenericDecay(Loop)%GCTime + TimeStepZone*SecInHour
      End If
      GCGain = ZoneContamGenericDecay(Loop)%GCInitEmiRate * Sch * &
               exp(-ZoneContamGenericDecay(Loop)%GCTime/ZoneContamGenericDecay(Loop)%GCDelayTime)
      ZoneContamGenericDecay(Loop)%GCGenRate = GCGain
    End Do

    ! From boudary layer diffusion
    DO Loop = 1, TotGCBLDiff
      SurfNum = ZoneContamGenericBLDiff(Loop)%SurfNum
      ZoneNum = Surface(SurfNum)%Zone
      Cs = Surface(SurfNum)%GenericContam
      Sch = GetCurrentScheduleValue(ZoneContamGenericBLDiff(Loop)%GCTranCoefSchedPtr)
      GCGain = ZoneContamGenericBLDiff(Loop)%GCTranCoef * Sch * Surface(SurfNum)%Area * Surface(SurfNum)%Multiplier * &
               (Cs/ZoneContamGenericBLDiff(Loop)%GCHenryCoef-ZoneAirGC(ZoneNum))*1.0d-6
      ZoneContamGenericBLDiff(Loop)%GCGenRate = GCGain
      ! Surface concentration level based on steady-state assumption
      Surface(SurfNum)%GenericContam = Cs - GCGain*1.0d6/Surface(SurfNum)%Multiplier/Surface(SurfNum)%Area
    End Do

    ! From deposition velocity sink model
    DO Loop = 1, TotGCDVS
      SurfNum = ZoneContamGenericDVS(Loop)%SurfNum
      ZoneNum = Surface(SurfNum)%Zone
      Sch = GetCurrentScheduleValue(ZoneContamGenericDVS(Loop)%GCDepoVeloPtr)
      GCGain = -ZoneContamGenericDVS(Loop)%GCDepoVelo*Surface(SurfNum)%Area*Sch*ZoneAirGC(ZoneNum)* &
               Surface(SurfNum)%Multiplier*1.0d-6
      ZoneContamGenericDVS(Loop)%GCGenRate = GCGain
    End Do

    ! From deposition rate sink model
    DO Loop = 1, TotGCDRS
      ZoneNum = ZoneContamGenericDRS(Loop)%ActualZoneNum
      Sch = GetCurrentScheduleValue(ZoneContamGenericDRS(Loop)%GCDepoRatePtr)
      GCGain = - ZoneContamGenericDRS(Loop)%GCDepoRate*Zone(Zonenum)%Volume*Sch*ZoneAirGC(ZoneNum)*1.0d-6
      ZoneContamGenericDRS(Loop)%GCGenRate = GCGain
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
  REAL(r64) :: CO2Gain                     ! Zone CO2 internal load
  REAL(r64) :: RhoAir                      ! Zone air density
  REAL(r64) :: A                           ! Coefficient of storage term in a zone balance equation
  REAL(r64) :: B                           ! Coefficient of variable term in a zone balance equation
  REAL(r64) :: C                           ! Coefficient of constnat term in a zone balance equation
  REAL(r64) :: SysTimeStepInSeconds        ! System time step lenght [s]
  LOGICAL   :: ControlledCO2ZoneFlag       ! This determines whether this is a CO2 controlled zone or not
  REAL(r64) :: LoadToCO2SetPoint           ! CO2 load at CO2 set point
  INTEGER   :: ContControlledZoneNum       ! The Splitter that you are currently loading input into
  INTEGER   :: ZoneNum
  INTEGER   :: I
  REAL(r64) :: ZoneAirCO2Setpoint          ! Zone CO2 setpoint
  REAL(r64) :: LoadToGCSetPoint            ! Generic contaminant load at generic contaminant set point
  LOGICAL   :: ControlledGCZoneFlag        ! This determines whether this is a generic contaminant controlled zone or not
  REAL(r64) :: ZoneAirGCSetpoint           ! Zone generic contaminant setpoint
  REAL(r64) :: GCGain                      ! Zone generic contaminant internal load
!  REAL(r64) :: Temp                      ! Zone generic contaminant internal load

          ! FLOW:

  ! Update zone CO2
  DO ZoneNum = 1, NumofZones

    IF (ShortenTimeStepSys) THEN  !

      IF (Zone(ZoneNum)%SystemZoneNodeNumber > 0) THEN ! roll back result for zone air node,
        IF (Contaminant%CO2Simulation) &
          Node(Zone(ZoneNum)%SystemZoneNodeNumber)%CO2   = CO2ZoneTimeMinus1(ZoneNum)
        IF (Contaminant%GenericContamSimulation) &
          Node(Zone(ZoneNum)%SystemZoneNodeNumber)%GenContam = GCZoneTimeMinus1(ZoneNum)
      ENDIF

      IF (NumOfSysTimeSteps /= NumOfSysTimeStepsLastZoneTimeStep) THEN ! cannot reuse existing DS data, interpolate from zone time

        IF (Contaminant%CO2Simulation) &
          Call DownInterpolate4HistoryValues(PriorTimeStep,TimeStepSys, &
                            CO2ZoneTimeMinus1(ZoneNum),   CO2ZoneTimeMinus2(ZoneNum),   &
                                          CO2ZoneTimeMinus3(ZoneNum),   CO2ZoneTimeMinus4(ZoneNum),  CO2ZoneTimeMinus4(ZoneNum),&
                             ZoneAirCO2(ZoneNum), DSCO2ZoneTimeMinus1(ZoneNum), DSCO2ZoneTimeMinus2(ZoneNum), &
                             DSCO2ZoneTimeMinus3(ZoneNum), DSCO2ZoneTimeMinus4(ZoneNum))
        IF (Contaminant%GenericContamSimulation) &
          Call DownInterpolate4HistoryValues(PriorTimeStep,TimeStepSys, &
                            GCZoneTimeMinus1(ZoneNum),   GCZoneTimeMinus2(ZoneNum),   &
                                          GCZoneTimeMinus3(ZoneNum),   GCZoneTimeMinus4(ZoneNum),  GCZoneTimeMinus4(ZoneNum),&
                             ZoneAirGC(ZoneNum), DSGCZoneTimeMinus1(ZoneNum), DSGCZoneTimeMinus2(ZoneNum), &
                             DSGCZoneTimeMinus3(ZoneNum), DSGCZoneTimeMinus4(ZoneNum))

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
      IF (Contaminant%GenericContamSimulation) Then
        GCZoneTimeMinus1Temp(ZoneNum) = GCZoneTimeMinus1(ZoneNum)
        GCZoneTimeMinus2Temp(ZoneNum) = GCZoneTimeMinus2(ZoneNum)
        GCZoneTimeMinus3Temp(ZoneNum) = GCZoneTimeMinus3(ZoneNum)
      End If

    ELSE  ! use down-stepped history

      IF (Contaminant%CO2Simulation) Then
        CO2ZoneTimeMinus1Temp(ZoneNum) = DSCO2ZoneTimeMinus1(ZoneNum)
        CO2ZoneTimeMinus2Temp(ZoneNum) = DSCO2ZoneTimeMinus2(ZoneNum)
        CO2ZoneTimeMinus3Temp(ZoneNum) = DSCO2ZoneTimeMinus3(ZoneNum)
      End If
      IF (Contaminant%GenericContamSimulation) Then
        GCZoneTimeMinus1Temp(ZoneNum) = DSGCZoneTimeMinus1(ZoneNum)
        GCZoneTimeMinus2Temp(ZoneNum) = DSGCZoneTimeMinus2(ZoneNum)
        GCZoneTimeMinus3Temp(ZoneNum) = DSGCZoneTimeMinus3(ZoneNum)
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
      IF (Contaminant%GenericContamSimulation) Then
        If (ShortenTimeStepSys .and. TimeStepSys .LT. TimeStepZone) Then
          If (PreviousTimeStep < TimeStepZone) Then
            ZoneGC1(ZoneNum) = ZoneGCM2(ZoneNum)
          Else
            ZoneGC1(ZoneNum) = ZoneGCMX(ZoneNum)
          End If
          ShortenTimeStepSysRoomAir = .TRUE.
        Else
          ZoneGC1(ZoneNum) = ZoneAirGC(ZoneNum)
        End If
      End If
    End If

    IF (Contaminant%CO2Simulation) Then

      CO2PredictedRate(ZoneNum) = 0.0d0
      LoadToCO2SetPoint=0.0d0
      ZoneSysContDemand(ZoneNum)%OutputRequiredToCO2SP = 0.0d0

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
        CO2Gain = ZoneCO2Gain(ZoneNum)*RhoAir*1.0d6

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
        If (ZoneAirCO2Setpoint .GT. OutdoorCO2 .AND. LoadToCo2SetPoint < 0.0d0) Then
          ZoneSysContDemand(ZoneNum)%OutputRequiredToCO2SP = LoadToCo2SetPoint/(OutdoorCO2 - ZoneAirCO2Setpoint)
        End If
      End If

      ! Apply the Zone Multiplier to the total zone moisture load
      ZoneSysContDemand(ZoneNum)%OutputRequiredToCO2SP = ZoneSysContDemand(ZoneNum)%OutputRequiredToCO2SP &
                                                       * Zone(ZoneNum)%Multiplier * Zone(ZoneNum)%ListMultiplier
      CO2PredictedRate(ZoneNum) = ZoneSysContDemand(ZoneNum)%OutputRequiredToCO2SP
    End If

    IF (Contaminant%GenericContamSimulation) Then

      GCPredictedRate(ZoneNum) = 0.0d0
      LoadToGCSetPoint=0.0d0
      ZoneSysContDemand(ZoneNum)%OutputRequiredToGCSP = 0.0d0

      ! Check to see if this is a "GC controlled zone"
      ControlledGCZoneFlag = .FALSE.
      ! Check all the controlled zones to see if it matches the zone simulated
      DO ContControlledZoneNum = 1, NumContControlledZones
        IF (GetCurrentScheduleValue(ContaminantControlledZone(ContControlledZoneNum)%AvaiSchedPtr) .GT. 0.d0) Then
          ZoneAirGCSetpoint = ZoneGCSetpoint(ContaminantControlledZone(ContControlledZoneNum)%ActualZoneNum)
          IF (ContaminantControlledZone(ContControlledZoneNum)%EMSOverrideGCSetpointOn) THEN
            ZoneAirGCSetpoint = ContaminantControlledZone(ContControlledZoneNum)%EMSOverrideGCSetpointValue
          End If
          If (ContaminantControlledZone(ContControlledZoneNum)%NumOfZones > 1) Then
            IF (ContaminantControlledZone(ContControlledZoneNum)%ActualZoneNum /= ZoneNum) Then
              Do I=1,ContaminantControlledZone(ContControlledZoneNum)%NumOfZones
                If (ContaminantControlledZone(ContControlledZoneNum)%ControlZoneNum(I) == ZoneNum) Then
                  ControlledGCZoneFlag = .TRUE.
                  Exit
                End If
              End Do
              If (ControlledGCZoneFlag) Exit
            ELSE
              ControlledGCZoneFlag = .TRUE.
              EXIT
            End If
          ENDIF
        ENDIF
      END DO ! GCControlledZoneNum

      If (ControlledGCZoneFlag) Then
        ! The density of air
        RhoAir = PsyRhoAirFnPbTdbW(OutBaroPress,ZT(ZoneNum),ZoneAirHumRat(ZoneNum),'PredictZoneContaminants')

        ! Calculate generic contaminant from infiltration + humidity added from latent load
        ! to determine system added/subtracted moisture.
        GCGain = ZoneGCGain(ZoneNum)*RhoAir*1.0d6

        SysTimeStepInSeconds = SecInHour * TimeStepSys

        ! Calculate the coefficients for the 3rd Order derivative for final
        ! zone GC.  The A, B, C coefficients are analogous to the GC balance.
        ! Assume that the system will have flow
        if (SimulateAirflowNetwork == AirflowNetworkControlMultizone.OR.SimulateAirflowNetwork == AirflowNetworkControlMultiADS &
            .OR. (SimulateAirflowNetwork == AirflowNetworkControlSimpleADS .AND. AirflowNetworkFanActivated)) then
          ! Multizone airflow calculated in AirflowNetwork
          B = GCGain + AirflowNetworkExchangeData(ZoneNum)%SumMHrGC + &
              AirflowNetworkExchangeData(ZoneNum)%SumMMHrGC
          A = AirflowNetworkExchangeData(ZoneNum)%SumMHr+AirflowNetworkExchangeData(ZoneNum)%SumMMHr
        else
          B = GCGain + ((oamfl(ZoneNum) + vamfl(ZoneNum) + eamfl(ZoneNum) + ctmfl(ZoneNum)) * OutdoorGC) + &
               MixingMassFlowGC(ZoneNum)
          A = oamfl(ZoneNum) + vamfl(ZoneNum) + eamfl(ZoneNum) + ctmfl(ZoneNum) + MixingMassFlowZone(ZoneNum)
        end if
        C = RhoAir * Zone(ZoneNum)%Volume * ZoneVolCapMultpGenContam / SysTimeStepInSeconds

        ! Use a 3rd Order derivative to predict zone moisture addition or removal and
        ! smooth the changes using the zone air capacitance.  Positive values of GC Load means that
        ! this amount of GC must be added to the zone to reach the setpoint.  Negative values represent
        ! the amount of GC that must be removed by the system.
        SELECT CASE (ZoneAirSolutionAlgo)
          CASE (Use3rdOrder)
            LoadToGCSetPoint = ((11.0d0/6.0d0) * C + A) * ZoneAirGCSetpoint - &
             (B + C * (3.0d0 * GCZoneTimeMinus1Temp(ZoneNum) - (3.0d0/2.0d0) * GCZoneTimeMinus2Temp(ZoneNum) + &
             (1.0d0/3.0d0) * GCZoneTimeMinus3Temp(ZoneNum)))
          ! Exact solution
          CASE (UseAnalyticalSolution)
            If (A .eq. 0.0d0) Then ! B=0
              LoadToGCSetPoint = C*(ZoneAirGCSetpoint-ZoneGC1(ZoneNum)) - B
            Else
              LoadToGCSetPoint = A*(ZoneAirGCSetpoint-ZoneGC1(ZoneNum)*exp(MIN(700.d0,-A/C)))/(1.0d0-exp(MIN(700.d0,-A/C))) - B
            End If
          CASE (UseEulerMethod)
            LoadToGCSetPoint = C*(ZoneAirGCSetpoint-ZoneGC1(ZoneNum)) + A*ZoneAirGCSetpoint - B
        END SELECT
        If (ZoneAirGCSetpoint .GT. OutdoorGC .AND. LoadToGCSetPoint < 0.0d0) Then
          ZoneSysContDemand(ZoneNum)%OutputRequiredToGCSP = LoadToGCSetPoint/(OutdoorGC - ZoneAirGCSetpoint)
        End If
      End If

      ! Apply the Zone Multiplier to the total zone moisture load
      ZoneSysContDemand(ZoneNum)%OutputRequiredToGCSP = ZoneSysContDemand(ZoneNum)%OutputRequiredToGCSP &
                                                       * Zone(ZoneNum)%Multiplier * Zone(ZoneNum)%ListMultiplier
      GCPredictedRate(ZoneNum) = ZoneSysContDemand(ZoneNum)%OutputRequiredToGCSP
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

    IF (Contaminant%GenericContamSimulation) Then
      GCZoneTimeMinus4(ZoneNum) = GCZoneTimeMinus3(ZoneNum)
      GCZoneTimeMinus3(ZoneNum) = GCZoneTimeMinus2(ZoneNum)
      GCZoneTimeMinus2(ZoneNum) = GCZoneTimeMinus1(ZoneNum)
      GCZoneTimeMinus1(ZoneNum) = ZoneAirGCAvg(ZoneNum) ! using average for whole zone time step.
      ZoneAirGC(ZoneNum) = ZoneAirGCTemp(ZoneNum)

      If (ZoneAirSolutionAlgo .NE. Use3rdOrder) Then
        ZoneGCM2(ZoneNum) = ZoneGCMX(ZoneNum)
        ZoneGCMX(ZoneNum) = ZoneAirGCAvg(ZoneNum) ! using average for whole zone time step.
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
    IF (Contaminant%GenericContamSimulation) Then
      DSGCZoneTimeMinus4(ZoneNum) = DSGCZoneTimeMinus3(ZoneNum)
      DSGCZoneTimeMinus3(ZoneNum) = DSGCZoneTimeMinus2(ZoneNum)
      DSGCZoneTimeMinus2(ZoneNum) = DSGCZoneTimeMinus1(ZoneNum)
      DSGCZoneTimeMinus1(ZoneNum) = ZoneAirGC(ZoneNum)
    ENDIF
  ENDDO ! zone loop

  If (ZoneAirSolutionAlgo .NE. Use3rdOrder) Then
    DO ZoneNum = 1, NumOfZones
      IF (Contaminant%CO2Simulation) Then
        ZoneCO2M2(ZoneNum) = ZoneCO2MX(ZoneNum)
        ZoneCO2MX(ZoneNum) = ZoneAirCO2Temp(ZoneNum) ! using average for whole zone time step.
      ENDIF
      IF (Contaminant%GenericContamSimulation) Then
        ZoneGCM2(ZoneNum) = ZoneGCMX(ZoneNum)
        ZoneGCMX(ZoneNum) = ZoneAirGCTemp(ZoneNum) ! using average for whole zone time step.
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
    IF (Contaminant%GenericContamSimulation) Then
      GCZoneTimeMinus1(ZoneNum) = GCZoneTimeMinus2(ZoneNum)
      GCZoneTimeMinus2(ZoneNum) = GCZoneTimeMinus3(ZoneNum)
      GCZoneTimeMinus3(ZoneNum) = GCZoneTimeMinus4(ZoneNum)
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
  REAL(r64) :: GCGain  ! Zone generic contaminant internal gain
  REAL(r64) :: RhoAir
  REAL(r64) :: A
  REAL(r64) :: B
  REAL(r64) :: C
  REAL(r64) :: CO2MassFlowRate
  REAL(r64) :: GCMassFlowRate
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

    IF (Contaminant%CO2Simulation) Then
      AZ(ZoneNum) = 0.0d0
      BZ(ZoneNum) = 0.0d0
      CZ(ZoneNum) = 0.0d0
    End If
    IF (Contaminant%GenericContamSimulation) Then
      AZGC(ZoneNum) = 0.0d0
      BZGC(ZoneNum) = 0.0d0
      CZGC(ZoneNum) = 0.0d0
    End If
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
        IF (Contaminant%GenericContamSimulation) Then
          Call DownInterpolate4HistoryValues(PriorTimeStep,TimeStepSys, &
                                ZoneAirGC(ZoneNum), GCZoneTimeMinus1(ZoneNum),   GCZoneTimeMinus2(ZoneNum),   & !
                                                     GCZoneTimeMinus3(ZoneNum),   GCZoneTimeMinus4(ZoneNum), & !
                                ZoneAirGC(ZoneNum), DSGCZoneTimeMinus1(ZoneNum), DSGCZoneTimeMinus2(ZoneNum), &
                                                     DSGCZoneTimeMinus3(ZoneNum), DSGCZoneTimeMinus4(ZoneNum))
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
      IF (Contaminant%GenericContamSimulation) Then
        GCZoneTimeMinus1Temp(ZoneNum) = DSGCZoneTimeMinus1(ZoneNum)
        GCZoneTimeMinus2Temp(ZoneNum) = DSGCZoneTimeMinus2(ZoneNum)
        GCZoneTimeMinus3Temp(ZoneNum) = DSGCZoneTimeMinus3(ZoneNum)
      END IF
    ELSE
      IF (Contaminant%CO2Simulation) Then
        CO2ZoneTimeMinus1Temp(ZoneNum) = CO2ZoneTimeMinus1(ZoneNum)
        CO2ZoneTimeMinus2Temp(ZoneNum) = CO2ZoneTimeMinus2(ZoneNum)
        CO2ZoneTimeMinus3Temp(ZoneNum) = CO2ZoneTimeMinus3(ZoneNum)
      END IF
      IF (Contaminant%GenericContamSimulation) Then
        GCZoneTimeMinus1Temp(ZoneNum) = GCZoneTimeMinus1(ZoneNum)
        GCZoneTimeMinus2Temp(ZoneNum) = GCZoneTimeMinus2(ZoneNum)
        GCZoneTimeMinus3Temp(ZoneNum) = GCZoneTimeMinus3(ZoneNum)
      END IF
    END IF

    ! Start to calculate zone CO2 and genric contaminant levels
    CO2MassFlowRate = 0.0d0
    GCMassFlowRate = 0.0d0
    ZoneMassFlowRate = 0.0d0
    ExhMassFlowRate = 0.0d0
    TotExitMassFlowRate = 0.0d0
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
        IF (Contaminant%GenericContamSimulation) Then
          GCMassFlowRate = GCMassFlowRate + &
                           (Node(ZoneEquipConfig(ZoneEquipConfigNum)%InletNode(NodeNum))%MassFlowRate * &
                            Node(ZoneEquipConfig(ZoneEquipConfigNum)%InletNode(NodeNum))%GenContam) / ZoneMult
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
        IF (Contaminant%GenericContamSimulation) Then
          GCMassFlowRate = GCMassFlowRate + &
                           (Node(ZoneRetPlenCond(ZoneRetPlenumNum)%InletNode(NodeNum))%MassFlowRate * &
                            Node(ZoneRetPlenCond(ZoneRetPlenumNum)%InletNode(NodeNum))%GenContam) / ZoneMult
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
          IF (Contaminant%GenericContamSimulation) Then
            GCMassFlowRate = GCMassFlowRate + &
                               (AirDistUnit(ADUNum)%MassFlowRateUpStrLk * Node(ADUInNode)%GenContam) / ZoneMult
          End If
          ZoneMassFlowRate = ZoneMassFlowRate + AirDistUnit(ADUNum)%MassFlowRateUpStrLk / ZoneMult
        END IF
        IF (AirDistUnit(ADUNum)%DownStreamLeak) THEN
          ADUOutNode = AirDistUnit(ADUNum)%OutletNodeNum
          IF (Contaminant%CO2Simulation) Then
            CO2MassFlowRate = CO2MassFlowRate + &
                               (AirDistUnit(ADUNum)%MassFlowRateDnStrLk * Node(ADUOutNode)%CO2) / ZoneMult
          End If
          IF (Contaminant%GenericContamSimulation) Then
            GCMassFlowRate = GCMassFlowRate + &
                               (AirDistUnit(ADUNum)%MassFlowRateDnStrLk * Node(ADUOutNode)%GenContam) / ZoneMult
          End If
          ZoneMassFlowRate = ZoneMassFlowRate + AirDistUnit(ADUNum)%MassFlowRateDnStrLk / ZoneMult
        END IF
      END DO
      ! Do not allow exhaust mass flow for a plenum zone
      ExhMassFlowRate = 0.0d0
      TotExitMassFlowRate = ExhMassFlowRate + ZoneMassFlowRate

    ELSE IF (ZoneSupPlenumAirFlag) THEN

      IF (Contaminant%CO2Simulation) Then
        CO2MassFlowRate = CO2MassFlowRate + &
                            (Node(ZoneSupPlenCond(ZoneSupPlenumNum)%InletNode)%MassFlowRate * &
                            Node(ZoneSupPlenCond(ZoneSupPlenumNum)%InletNode)%CO2) / ZoneMult
      END IF
      IF (Contaminant%GenericContamSimulation) Then
        GCMassFlowRate = GCMassFlowRate + &
                            (Node(ZoneSupPlenCond(ZoneSupPlenumNum)%InletNode)%MassFlowRate * &
                            Node(ZoneSupPlenCond(ZoneSupPlenumNum)%InletNode)%GenContam) / ZoneMult
      END IF
      ZoneMassFlowRate = ZoneMassFlowRate + &
                           Node(ZoneSupPlenCond(ZoneSupPlenumNum)%InletNode)%MassFlowRate / ZoneMult
      ! Do not allow exhaust mass flow for a plenum zone
      ExhMassFlowRate = 0.0d0
      TotExitMassFlowRate = ExhMassFlowRate + ZoneMassFlowRate
    END IF

    SysTimeStepInSeconds = SecInHour * TimeStepSys

    ! Calculate the coefficients for the 3rd order derivative for final
    ! zone humidity ratio.  The A, B, C coefficients are analogous to the
    ! CO2 balance.  There are 2 cases that should be considered, system
    ! operating and system shutdown.

    RhoAir = PsyRhoAirFnPbTdbW(OutBaroPress,ZT(ZoneNum),ZoneAirHumRat(ZoneNum),'CorrectZoneContaminants')
!    RhoAir = ZoneAirDensityCO(ZoneNum)

    IF (Contaminant%CO2Simulation) ZoneAirDensityCO(ZoneNum) = RhoAir
    ! Calculate Co2 internal gain
    IF (Contaminant%CO2Simulation) CO2Gain = ZoneCO2Gain(ZoneNum)*RhoAir*1.0d6
    IF (Contaminant%GenericContamSimulation) GCGain = ZoneGCGain(ZoneNum)*RhoAir*1.0d6

    ! Check for the flow and NO flow condition
    IF (ZoneMassFlowRate .GT. 0.0d0) THEN
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
          A = ZoneMassFlowRate + AirflowNetworkExchangeData(ZoneNum)%SumMHr +AirflowNetworkExchangeData(ZoneNum)%SumMMHr
        end if
        C = RhoAir*Zone(ZoneNum)%Volume*ZoneVolCapMultpCO2/SysTimeStepInSeconds
      End If
    ELSE IF (ZoneMassFlowRate .LE. 0.0d0) THEN
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

      ! Use a 3rd order derivative to predict final zone CO2 and
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
      IF (ZoneAirCO2Temp(ZoneNum) .LT. 0.0d0) ZoneAirCO2Temp(ZoneNum) = 0.0d0

      ZoneAirCO2(ZoneNum) = ZoneAirCO2Temp(ZoneNum)

      ! Now put the calculated info into the actual zone nodes; ONLY if there is zone air flow, i.e. controlled zone or plenum zone
      ZoneNodeNum = Zone(ZoneNum)%SystemZoneNodeNumber
      IF (ZoneNodeNum > 0) THEN
        Node(ZoneNodeNum)%CO2 = ZoneAirCO2Temp(ZoneNum)
      END IF
    END IF

    IF (ZoneMassFlowRate .GT. 0.0d0) THEN
      IF (Contaminant%GenericContamSimulation) Then
        B = GCGain+((oamfl(ZoneNum)+vamfl(ZoneNum)+eamfl(ZoneNum)+ctmfl(ZoneNum))* OutdoorGC) &
                               +(GCMassFlowRate) &
                               +MixingMassFlowGC(ZoneNum)
        A = TotExitMassFlowRate + oamfl(ZoneNum) + vamfl(ZoneNum) + eamfl(ZoneNum) + ctmfl(ZoneNum) &
                            + MixingMassFlowZone(ZoneNum)
        if (SimulateAirflowNetwork == AirflowNetworkControlMultizone .OR.   &
            SimulateAirflowNetwork == AirflowNetworkControlMultiADS  .OR.   &
           (SimulateAirflowNetwork == AirflowNetworkControlSimpleADS .AND. AirflowNetworkFanActivated)) then
          ! Multizone airflow calculated in AirflowNetwork
          B = GCGain+(AirflowNetworkExchangeData(ZoneNum)%SumMHrGC+AirflowNetworkExchangeData(ZoneNum)%SumMMHrGC)+GCMassFlowRate
          A = ZoneMassFlowRate + AirflowNetworkExchangeData(ZoneNum)%SumMHr +AirflowNetworkExchangeData(ZoneNum)%SumMMHr
        end if
        C = RhoAir*Zone(ZoneNum)%Volume*ZoneVolCapMultpGenContam/SysTimeStepInSeconds
      End If
    ELSE IF (ZoneMassFlowRate .LE. 0.0d0) THEN
      IF (Contaminant%GenericContamSimulation) Then
        B = GCGain+((oamfl(ZoneNum)+vamfl(ZoneNum)+eamfl(ZoneNum)+ctmfl(ZoneNum)+ExhMassFlowRate)* OutdoorGC) &
                               + MixingMassFlowGC(ZoneNum)
        A = oamfl(ZoneNum) + vamfl(ZoneNum) + eamfl(ZoneNum) +ctmfl(ZoneNum) + ExhMassFlowRate + MixingMassFlowZone(ZoneNum)
        if (SimulateAirflowNetwork == AirflowNetworkControlMultizone .OR.   &
            SimulateAirflowNetwork == AirflowNetworkControlMultiADS  .OR.   &
           (SimulateAirflowNetwork == AirflowNetworkControlSimpleADS .AND. AirflowNetworkFanActivated)) then
          ! Multizone airflow calculated in AirflowNetwork
          B = GCGain+AirflowNetworkExchangeData(ZoneNum)%SumMHrGC+AirflowNetworkExchangeData(ZoneNum)%SumMMHrGC
          A = AirflowNetworkExchangeData(ZoneNum)%SumMHr+AirflowNetworkExchangeData(ZoneNum)%SumMMHr
        end if
        C = RhoAir*Zone(ZoneNum)%Volume*ZoneVolCapMultpGenContam/SysTimeStepInSeconds
      End If
    END IF

    IF (Contaminant%GenericContamSimulation) Then
      if (SimulateAirflowNetwork > AirflowNetworkControlMultizone) then
        B = B+AirflowNetworkExchangeData(ZoneNum)%TotalGC
      end if

      AZGC(ZoneNum) = A
      BZGC(ZoneNum) = B
      CZGC(ZoneNum) = C

      ! Use a 3rd order derivative to predict final zone generic contaminant and
      ! smooth the changes using the zone air capacitance.
      SELECT CASE (ZoneAirSolutionAlgo)
        CASE (Use3rdOrder)
          ZoneAirGCTemp(ZoneNum)=(B+C*(3.0d0*GCZoneTimeMinus1Temp(ZoneNum)-(3.0d0/2.0d0)*GCZoneTimeMinus2Temp(ZoneNum)+ &
                             (1.0d0/3.0d0)*GCZoneTimeMinus3Temp(ZoneNum)))/((11.0d0/6.0d0)*C+A)
        ! Exact solution
        CASE (UseAnalyticalSolution)
          If (A .eq. 0.0d0) Then ! B=0
            ZoneAirGCTemp(ZoneNum)= ZoneGC1(ZoneNum) + B/C
          Else
            ZoneAirGCTemp(ZoneNum)= (ZoneGC1(ZoneNum)-B/A)*exp(MIN(700.d0,-A/C))+B/A
          End If
        CASE (UseEulerMethod)
          ZoneAirGCTemp(ZoneNum) = (C*ZoneGC1(ZoneNum)+B)/(C+A)
      END SELECT

      ! Set the generic contaminant to zero if the zone has been large sinks
      IF (ZoneAirGCTemp(ZoneNum) .LT. 0.0d0) ZoneAirGCTemp(ZoneNum) = 0.0d0

      ZoneAirGC(ZoneNum) = ZoneAirGCTemp(ZoneNum)

      ! Now put the calculated info into the actual zone nodes; ONLY if there is zone air flow, i.e. controlled zone or plenum zone
      ZoneNodeNum = Zone(ZoneNum)%SystemZoneNodeNumber
      IF (ZoneNodeNum > 0) THEN
        Node(ZoneNodeNum)%GenContam = ZoneAirGCTemp(ZoneNum)
      END IF
    END IF

  END DO

  RETURN

END SUBROUTINE CorrectZoneContaminants



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

END MODULE ZoneContaminantPredictorCorrector
