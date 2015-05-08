MODULE ZoneTempPredictorCorrector

          ! MODULE INFORMATION:
          !       AUTHOR         Russell D. Taylor
          !       DATE WRITTEN   1997
          !       MODIFIED       Aug 2001(FW): make SNLoadHeatRate public
          !                      Nov 2010  BN(FSEC) added TemperatureAndHumidity Control
          !       RE-ENGINEERED  July 2003 (Peter Graham Ellis)
          !                      July 2006 (BG) added operative temp control
          !                      February 2008 (BG) reworked zone air temp histories

          ! PURPOSE OF THIS MODULE:
          ! This module contains routines to predict and correct zone temperatures.
          !  also includes zone thermostatic controlling
          !  Model the "Air Heat Balance" part of the the "Zone Heat Balance Method."

          ! METHODOLOGY EMPLOYED:
          ! apply model equations for air heat balance solved for zone air temp.
          !    sum up values for the terms (e.g SUMHAT, SUMHA etc. )
          !    "Predict" step is used to get zone loads for HVAC equipment
          !    "correct" step determines zone air temp with available HVAC

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
USE DataPrecisionGlobals
USE DataGlobals
USE DataHVACGlobals
USE DataHeatBalance
USE DataHeatBalFanSys
USE DataEnvironment, ONLY: OutHumRat, OutBaroPress
USE DataZoneEnergyDemands, ONLY: ZoneSysEnergyDemand, ZoneSysMoistureDemand, DeadbandOrSetback, CurDeadbandOrSetback, &
                                  SetBack
USE Psychrometrics
USE DataAirflowNetwork,ONLY:SimulateAirflowNetwork,AirflowNetworkExchangeData,AirflowNetworkZoneExhaustFan, &
                            AirflowNetworkNumOfExhFan,AirflowNetworkFanActivated,AirflowNetworkControlMultizone, &
                            AirflowNetworkControlSimpleADS,AirflowNetworkControlMultiADS
USE DataRoomAirModel
USE DataZoneControls
USE DataInterfaces

IMPLICIT NONE ! Enforce explicit typing of all variables

PRIVATE

          ! MODULE PARAMETER DEFINITIONS:
! Controls for PredictorCorrector
!INTEGER, PUBLIC, PARAMETER :: iGetZoneSetpoints             = 1
!INTEGER, PUBLIC, PARAMETER :: iPredictStep                  = 2
!INTEGER, PUBLIC, PARAMETER :: iCorrectStep                  = 3
!INTEGER, PUBLIC, PARAMETER :: iRevertZoneTimestepHistories  = 4
!INTEGER, PUBLIC, PARAMETER :: iPushZoneTimestepHistories    = 5
!INTEGER, PUBLIC, PARAMETER :: iPushSystemTimestepHistories  = 6

  CHARACTER(len=*), PARAMETER, DIMENSION(4) ::   &
     ValidControlTypes=(/'ThermostatSetpoint:SingleHeating         ', &
                         'ThermostatSetpoint:SingleCooling         ', &
                         'ThermostatSetpoint:SingleHeatingOrCooling', &
                         'ThermostatSetpoint:DualSetpoint          '/)

  CHARACTER(len=*), PARAMETER, DIMENSION(12) :: &
     ValidComfortControlTypes=(/'ThermostatSetpoint:ThermalComfort:Fanger:SingleHeating         ', &
                                'ThermostatSetpoint:ThermalComfort:Fanger:SingleCooling         ', &
                                'ThermostatSetpoint:ThermalComfort:Fanger:SingleHeatingOrCooling', &
                                'ThermostatSetpoint:ThermalComfort:Fanger:DualSetpoint          ', &
                                'ThermostatSetpoint:ThermalComfort:Pierce:SingleHeating         ', &
                                'ThermostatSetpoint:ThermalComfort:Pierce:SingleCooling         ', &
                                'ThermostatSetpoint:ThermalComfort:Pierce:SingleHeatingOrCooling', &
                                'ThermostatSetpoint:ThermalComfort:Pierce:DualSetpoint          ', &
                                'ThermostatSetpoint:ThermalComfort:KSU:SingleHeating            ', &
                                'ThermostatSetpoint:ThermalComfort:KSU:SingleCooling            ', &
                                'ThermostatSetpoint:ThermalComfort:KSU:SingleHeatingOrCooling   ', &
                                'ThermostatSetpoint:ThermalComfort:KSU:DualSetpoint             '/)

  CHARACTER(len=*), PARAMETER, DIMENSION(6) ::   &
          cZControlTypes=(/'ZoneControl:Thermostat                       ',    &
                           'ZoneControl:Thermostat:ThermalComfort        ',    &
                           'ZoneControl:Thermostat:OperativeTemperature  ',    &
                           'ZoneControl:Humidistat                       ',    &
                           'ZoneControl:Thermostat:TemperatureAndHumidity',    &
                           'ZoneControl:Thermostat:StagedDualSetpoint    '/)

  INTEGER, PARAMETER :: iZC_TStat=1
  INTEGER, PARAMETER :: iZC_TCTStat=2
  INTEGER, PARAMETER :: iZC_OTTStat=3
  INTEGER, PARAMETER :: iZC_HStat=4
  INTEGER, PARAMETER :: iZC_TandHStat=5
  INTEGER, PARAMETER :: iZC_StagedDual=6
  INTEGER, PARAMETER, DIMENSION(6) ::  &
          iZControlTypes=(/iZC_TStat,iZC_TCTStat,iZC_OTTStat,iZC_HStat,iZC_TandHStat,iZC_StagedDual/)

INTEGER, PARAMETER :: SglHeatSetPoint =1
INTEGER, PARAMETER :: SglCoolSetPoint =2
INTEGER, PARAMETER :: SglHCSetPoint   =3
INTEGER, PARAMETER :: DualSetPoint    =4
INTEGER, PARAMETER :: SglHeatSetPointFanger =1
INTEGER, PARAMETER :: SglCoolSetPointFanger =2
INTEGER, PARAMETER :: SglHCSetPointFanger   =3
INTEGER, PARAMETER :: DualSetPointFanger    =4
INTEGER, PARAMETER :: SglHeatSetPointPierce =5
INTEGER, PARAMETER :: SglCoolSetPointPierce =6
INTEGER, PARAMETER :: SglHCSetPointPierce   =7
INTEGER, PARAMETER :: DualSetPointPierce    =8
INTEGER, PARAMETER :: SglHeatSetPointKSU    =9
INTEGER, PARAMETER :: SglCoolSetPointKSU    =10
INTEGER, PARAMETER :: SglHCSetPointKSU      =11
INTEGER, PARAMETER :: DualSetPointKSU       =12

! Average method parameter with multiple people objects in a zone
INTEGER, PARAMETER :: AverageMethodNum_NO =0 ! No multiple people objects
INTEGER, PARAMETER :: AverageMethodNum_SPE=1 ! Specific people object
INTEGER, PARAMETER :: AverageMethodNum_OBJ=2 ! People object average
INTEGER, PARAMETER :: AverageMethodNum_PEO=3 ! People number average

          ! DERIVED TYPE DEFINITIONS:
TYPE ZoneTempControlType
  CHARACTER(len=MaxNameLength) :: Name                  =' ' ! Name of the zone
  CHARACTER(len=MaxNameLength) :: TempSchedName         =' ' ! Name of the schedule which determines the zone temp setpoint
  INTEGER                      :: TempSchedIndex        =0
  CHARACTER(len=MaxNameLength) :: HeatTempSetptSchedName=' '
  INTEGER                      :: HeatTempSchedIndex    =0
  CHARACTER(len=MaxNameLength) :: CoolTempSetptSchedName=' '
  INTEGER                      :: CoolTempSchedIndex    =0
END TYPE ZoneTempControlType

TYPE ZoneComfortFangerControlType
  CHARACTER(len=MaxNameLength) :: Name                 =' ' ! Name of the zone
  CHARACTER(len=MaxNameLength) :: PMVSchedName         =' ' ! Name of the schedule which determines the zone temp setpoint
  INTEGER                      :: PMVSchedIndex        =0   ! Index to PMV dual set point schedule
  CHARACTER(len=MaxNameLength) :: HeatPMVSetptSchedName=' ' ! Name of PMV heating set point schedule
  INTEGER                      :: HeatPMVSchedIndex    =0   ! Index to PMV heating set point schedule
  CHARACTER(len=MaxNameLength) :: CoolPMVSetptSchedName=' ' ! Name of PMV cooling set point schedule
  INTEGER                      :: CoolPMVSchedIndex    =0   ! INdex to PMV cooling set point schedule
END TYPE ZoneComfortFangerControlType

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! MODULE VARIABLE DECLARATIONS:

TYPE (ZoneTempControlType), ALLOCATABLE, DIMENSION(:) :: SetPointSingleHeating
TYPE (ZoneTempControlType), ALLOCATABLE, DIMENSION(:) :: SetPointSingleCooling
TYPE (ZoneTempControlType), ALLOCATABLE, DIMENSION(:) :: SetPointSingleHeatCool
TYPE (ZoneTempControlType), ALLOCATABLE, DIMENSION(:) :: SetPointDualHeatCool

TYPE (ZoneComfortFangerControlType), ALLOCATABLE, DIMENSION(:) :: SetPointSingleHeatingFanger
TYPE (ZoneComfortFangerControlType), ALLOCATABLE, DIMENSION(:) :: SetPointSingleCoolingFanger
TYPE (ZoneComfortFangerControlType), ALLOCATABLE, DIMENSION(:) :: SetPointSingleHeatCoolFanger
TYPE (ZoneComfortFangerControlType), ALLOCATABLE, DIMENSION(:) :: SetPointDualHeatCoolFanger

INTEGER :: NumSingleTempHeatingControls    =0
INTEGER :: NumSingleTempCoolingControls    =0
INTEGER :: NumSingleTempHeatCoolControls   =0
INTEGER :: NumDualTempHeatCoolControls     =0

! Number of Thermal comfort control types
INTEGER :: NumSingleFangerHeatingControls  =0
INTEGER :: NumSingleFangerCoolingControls  =0
INTEGER :: NumSingleFangerHeatCoolControls =0
INTEGER :: NumDualFangerHeatCoolControls   =0

! Number of zone with staged controlled objects
INTEGER :: NumStageCtrZone                 =0

REAL(r64),DIMENSION(:),ALLOCATABLE  :: ZoneSetPointLast
REAL(r64),DIMENSION(:),ALLOCATABLE  :: TempIndZnLd
REAL(r64),DIMENSION(:),ALLOCATABLE  :: TempDepZnLd
REAL(r64),DIMENSION(:),ALLOCATABLE  :: ZoneAirRelHum  ! Zone relative humidity in percent

! Zone temperature history - used only for oscillation test
REAL(r64),DIMENSION(:,:),ALLOCATABLE ::  ZoneTempHist
REAL(r64),DIMENSION(:),ALLOCATABLE   ::  ZoneTempOscillate
REAL(r64)                       ::  AnyZoneTempOscillate


          ! SUBROUTINE SPECIFICATIONS:
PUBLIC ManageZoneAirUpdates
PRIVATE CorrectZoneAirTemp
PRIVATE PredictSystemLoads
PRIVATE CalcPredictedSystemLoad
PRIVATE CalcZoneAirTempSetpoints
PRIVATE InitZoneAirSetpoints
PRIVATE GetZoneAirSetpoints ! get input routines for all types of zone controls
PRIVATE CorrectZoneHumRat
PRIVATE CalcPredictedHumidityRatio
PRIVATE CalcZoneSums
PUBLIC  VerifyThermostatInZone
PUBLIC DetectOscillatingZoneTemp
PRIVATE CalcZoneComponentLoadSums

PRIVATE CalcZoneAirComfortSetpoints !sets the thermal comfort setpoints
PRIVATE GetComfortSetpoints !
PRIVATE PMVResidual

PUBLIC DownInterpolate4HistoryValues
PRIVATE AdjustAirSetpointsforOpTempCntrl
PRIVATE PushZoneTimestepHistories
PRIVATE PushSystemTimestepHistories
PRIVATE RevertZoneTimestepHistories ! not now being used can remove...
PRIVATE AdjustCoolingSetPointforTempAndHumidityControl ! resets cooling setpoint temp based on high RH

CONTAINS

SUBROUTINE ManageZoneAirUpdates(UpdateType, ZoneTempChange, ShortenTimeStepSys,   &
                                UseZoneTimeStepHistory, PriorTimeStep )

          ! SUBROUTINE INFORMATION
          !       AUTHOR         Russ Taylor
          !       DATE WRITTEN   September 1998
          !       MODIFIED       na
          !       RE-ENGINEERED  Brent Griffith Feb. 2008,  added arguments

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine predicts or corrects the zone air temperature
          ! depending on the simulation status and determines the correct
          ! temperature setpoint for each zone from the schedule manager.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: UpdateType             ! Can be iGetZoneSetpoints, iPredictStep, iCorrectStep
  REAL(r64), INTENT(INOUT) :: ZoneTempChange    ! Temp change in zone air btw previous and current timestep
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

  IF (GetZoneAirStatsInputFlag) THEN
    CALL GetZoneAirSetpoints
    GetZoneAirStatsInputFlag = .FALSE.
  END IF

  CALL InitZoneAirSetpoints

  SELECT CASE(UpdateType)

    CASE(iGetZoneSetpoints)
      CALL CalcZoneAirTempSetpoints

    CASE(iPredictStep)
      CALL PredictSystemLoads(ShortenTimeStepSys, UseZoneTimeStepHistory,&
                               PriorTimeStep )

    CASE(iCorrectStep)
      CALL CorrectZoneAirTemp(ZoneTempChange, ShortenTimeStepSys, &
            UseZoneTimeStepHistory, PriorTimeStep)

    CASE (iRevertZoneTimestepHistories)
      Call RevertZoneTimestepHistories

    CASE (iPushZoneTimestepHistories)
      Call PushZoneTimestepHistories

    CASE (iPushSystemTimestepHistories)
      Call PushSystemTimestepHistories

  END SELECT

  RETURN

END SUBROUTINE ManageZoneAirUpdates

SUBROUTINE GetZoneAirSetpoints

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Russell Taylor
          !       DATE WRITTEN   September 1998
          !       MODIFIED       L.Gu, May 2006, B. Griffith June 2006
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine gets the inputs related to thermostatic control.

          ! METHODOLOGY EMPLOYED:
          ! Uses the status flags to trigger events.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataIPShortCuts
  USE InputProcessor
  USE ScheduleManager, ONLY: GetScheduleIndex, CheckScheduleValueMinMax, GetScheduleMinValue, GetScheduleMaxValue,  &
                             CheckScheduleValue
  USE General, ONLY: TrimSigDigits, FindNumberInList, RoundSigDigits, CheckCreatedZoneItemName

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER :: RoutineName='GetZoneAirSetpoints: '

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
  INTEGER :: TempControlledZoneNum      ! The Splitter that you are currently loading input into
  INTEGER :: NumAlphas
  INTEGER :: NumNums
  INTEGER :: SingleTempHeatingControlNum
  INTEGER :: SingleTempCoolingControlNum
  INTEGER :: SingleTempHeatCoolControlNum
  INTEGER :: DualTempHeatCoolControlNum
  INTEGER :: ControlTypeNum
  INTEGER :: IOSTAT
!unused1208  REAL(r64), DIMENSION(2) :: NumArray
!unused1208  CHARACTER(len=MaxNameLength), DIMENSION(29) :: AlphArray
  LOGICAL :: ErrorsFound = .FALSE.
  LOGICAL :: ErrFlag
  LOGICAL :: IsNotOK               ! Flag to verify name
  LOGICAL :: IsBlank               ! Flag for blank name
  INTEGER :: CTIndex
  INTEGER :: HumidControlledZoneNum      ! The Humidity Controller that information is being loaded into
  LOGICAL :: ValidScheduleControlType
  LOGICAL :: ValidRadFractSched          ! check for if radiative fraction schedule has valid numbers
  LOGICAL :: ValidZoneOvercoolRangeSched ! check for if Zone Overcool range schedule has valid numbers
  INTEGER :: TempIndex
  INTEGER :: SchedMin
  INTEGER :: SchedMax
  INTEGER :: ActualZoneNum
  INTEGER :: SchedTypeIndex

  INTEGER :: ComfortControlledZoneNum      ! The Splitter that you are currently loading input into
  INTEGER :: I, IZoneCount

  INTEGER :: OpTempContrlNum  ! do loop index
  INTEGER :: found

  INTEGER :: TempHumidityCntrlNum   ! do loop index for overcooled controlled zone

  INTEGER :: SingleFangerHeatingControlNum
  INTEGER :: SingleFangerCoolingControlNum
  INTEGER :: SingleFangerHeatCoolControlNum
  INTEGER :: DualFangerHeatCoolControlNum
  INTEGER :: ComfortIndex
  INTEGER :: ZoneAssigned

  INTEGER :: NumStageControlledZones ! Number of staged controlled objects
  INTEGER :: StageControlledZoneNum  ! Index for staged controlled zones

  TYPE (NeededControlTypes), ALLOCATABLE, DIMENSION(:) :: TStatControlTypes
  TYPE (NeededComfortControlTypes), ALLOCATABLE, DIMENSION(:) :: TComfortControlTypes
  INTEGER, ALLOCATABLE, DIMENSION(:) :: CTSchedMapToControlledZone
  INTEGER, ALLOCATABLE, DIMENSION(:) :: CCmSchedMapToControlledZone
  INTEGER :: Item
  INTEGER :: Item1
  INTEGER :: ZLItem

          ! FLOW:
  cCurrentModuleObject=cZControlTypes(iZC_TStat)
  NumTStatStatements = GetNumObjectsFound(cCurrentModuleObject)
  ALLOCATE(TStatObjects(NumTStatStatements))

! Pre-scan for use of Zone lists in TStat statements (i.e. Global application of TStat)
  NumTempControlledZones=0
  DO Item=1,NumTStatStatements
    CALL GetObjectItem(cCurrentModuleObject,Item,cAlphaArgs,NumAlphas,rNumericArgs,NumNums,IOSTAT, &
                       NumBlank=lNumericFieldBlanks,AlphaBlank=lAlphaFieldBlanks, &
                       AlphaFieldNames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
    IsNotOK = .FALSE.
    IsBlank = .FALSE.
    CALL VerifyName(cAlphaArgs(1),TStatObjects%Name,Item-1,IsNotOK,IsBlank, &
                    trim(cCurrentModuleObject)//' Name')
    IF (IsNotOK) THEN
      ErrorsFound = .TRUE.
      IF (IsBlank) cAlphaArgs(1) = 'xxxxx'
    END IF
    TStatObjects(Item)%Name=cAlphaArgs(1)
    Item1=FindItemInList(cAlphaArgs(2),Zone%Name,NumOfZones)
    ZLItem=0
    IF (Item1 == 0 .and. NumOfZoneLists > 0) &
        ZLItem=FindItemInList(cAlphaArgs(2),ZoneList%Name,NumOfZoneLists)
    IF (Item1 > 0) THEN
      TStatObjects(Item)%TempControlledZoneStartPtr=NumTempControlledZones+1
      NumTempControlledZones=NumTempControlledZones+1
      TStatObjects(Item)%NumOfZones=1
      TStatObjects(Item)%ZoneListActive=.false.
      TStatObjects(Item)%ZoneOrZoneListPtr=Item1
    ELSEIF (ZLItem > 0) THEN
      TStatObjects(Item)%TempControlledZoneStartPtr=NumTempControlledZones+1
      NumTempControlledZones=NumTempControlledZones+ZoneList(ZLItem)%NumOfZones
      TStatObjects(Item)%NumOfZones=ZoneList(ZLItem)%NumOfZones
      TStatObjects(Item)%ZoneListActive=.true.
      TStatObjects(Item)%ZoneOrZoneListPtr=ZLItem
    ELSE
      CALL ShowSevereError(trim(cCurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid '//  &
           trim(cAlphaFieldNames(2))//'="'//trim(cAlphaArgs(2))//'" not found.')
      ErrorsFound=.true.
    ENDIF
  ENDDO

  IF (ErrorsFound) THEN
    CALL ShowSevereError('GetZoneAirSetpoints: Errors with invalid names in '//trim(cCurrentModuleObject)//  &
       ' objects.')
    CALL ShowContinueError('...These will not be read in.  Other errors may occur.')
    NumTempControlledZones=0
  ENDIF

  IF (NumTempControlledZones > 0) THEN
    ALLOCATE(TempControlledZone(NumTempControlledZones))
    ALLOCATE(TStatControlTypes(NumTempControlledZones))  ! Number of set point types
    ALLOCATE(CTSchedMapToControlledZone(NumTempControlledZones))
    CTSchedMapToControlledZone=0

    TempControlledZoneNum = 0
    DO Item = 1, NumTStatStatements
      CALL GetObjectItem(cCurrentModuleObject,Item,cAlphaArgs,NumAlphas,rNumericArgs,NumNums,IOSTAT, &
                         NumBlank=lNumericFieldBlanks,AlphaBlank=lAlphaFieldBlanks, &
                         AlphaFieldNames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
      DO Item1=1,TStatObjects(Item)%NumOfZones
        TempControlledZoneNum=TempControlledZoneNum+1
        IF (TStatObjects(Item)%ZoneListActive) THEN
          cAlphaArgs(2)=Zone(ZoneList(TStatObjects(Item)%ZoneOrZoneListPtr)%Zone(Item1))%Name
        ENDIF
        ZoneAssigned=FindItemInList(cAlphaArgs(2),TempControlledZone%ZoneName,TempControlledZoneNum-1)
        IF (ZoneAssigned == 0) THEN
          TempControlledZone(TempControlledZoneNum)%ZoneName = cAlphaArgs(2)
          TempControlledZone(TempControlledZoneNum)%ActualZoneNum = FindIteminList(cAlphaArgs(2),Zone%Name,NumOfZones)
          IF (TempControlledZone(TempControlledZoneNum)%ActualZoneNum == 0) THEN
            CALL ShowSevereError(trim(cCurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid '//  &
               trim(cAlphaFieldNames(2))//'="'//trim(cAlphaArgs(2))//'" not found.')
            ErrorsFound = .TRUE.
          ELSE
            Zone(TempControlledZone(TempControlledZoneNum)%ActualZoneNum)%TempControlledZoneIndex = TempControlledZoneNum
          END IF
        ELSE
          TempControlledZone(TempControlledZoneNum)%ZoneName = cAlphaArgs(2)  ! for continuity
          CALL ShowSevereError(trim(cCurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid '//  &
             trim(cAlphaFieldNames(2))//'="'//trim(cAlphaArgs(2))//'" zone previously assigned.')
          CALL ShowContinueError('...Zone was previously assigned to Thermostat="'//  &
             trim(TempControlledZone(ZoneAssigned)%Name)//'".')
          ErrorsFound = .TRUE.
          CYCLE
        ENDIF

        IF (.not. TStatObjects(Item)%ZoneListActive) THEN
          TempControlledZone(TempControlledZoneNum)%Name = cAlphaArgs(1)
        ELSE
          CALL CheckCreatedZoneItemName(RoutineName,cCurrentModuleObject,  &
                                        Zone(ZoneList(TStatObjects(Item)%ZoneOrZoneListPtr)%Zone(Item1))%Name,  &
                                        ZoneList(TStatObjects(Item)%ZoneOrZoneListPtr)%MaxZoneNameLength,  &
                                        TStatObjects(Item)%Name,     &
                                        TempControlledZone%Name,           &
                                        TempControlledZoneNum-1,                       &
                                        TempControlledZone(TempControlledZoneNum)%Name,            &
                                        ErrFlag)
          IF (ErrFlag) ErrorsFound=.true.
        ENDIF

        TempControlledZone(TempControlledZoneNum)%ControlTypeSchedName = cAlphaArgs(3)
        TempControlledZone(TempControlledZoneNum)%CTSchedIndex=GetScheduleIndex(cAlphaArgs(3))
        IF (Item1 == 1) THEN  ! only show error on first of several if zone list
          IF (TempControlledZone(TempControlledZoneNum)%CTSchedIndex == 0) THEN
            CALL ShowSevereError(trim(cCurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid '//  &
               trim(cAlphaFieldNames(3))//'="'//trim(cAlphaArgs(3))//'" not found.')
            ErrorsFound = .TRUE.
          ELSE
            ! Check validity of control types.
            ValidScheduleControlType=CheckScheduleValueMinMax(TempControlledZone(TempControlledZoneNum)%CTSchedIndex,  &
                                                               '>=',0.0d0,'<=',4.0d0)
            IF (.not. ValidScheduleControlType) THEN
              CALL ShowSevereError(trim(cCurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid range '//  &
                 trim(cAlphaFieldNames(2))//'="'//trim(cAlphaArgs(2))//'"')
              CALL ShowContinueError('..contains values outside of range [0,4].')
              ErrorsFound=.TRUE.
            ENDIF
          END IF
        ENDIF

        TempControlledZone(TempControlledZoneNum)%NumControlTypes = NINT((NumAlphas - 3.0d0)/2.0d0)
        ALLOCATE(TempControlledZone(TempControlledZoneNum)%ControlType(TempControlledZone(  &
           TempControlledZoneNum)%NumControlTypes))
        ALLOCATE(TempControlledZone(TempControlledZoneNum)%ControlTypeName(TempControlledZone(  &
           TempControlledZoneNum)%NumControlTypes))
        ALLOCATE(TempControlledZone(TempControlledZoneNum)%ControlTypeSchIndx(TempControlledZone(  &
                                                                                TempControlledZoneNum)%NumControlTypes))

        DO ControlTypeNum = 1, TempControlledZone(TempControlledZoneNum)%NumControlTypes

          TempControlledZone(TempControlledZoneNum)%ControlType(ControlTypeNum) = cAlphaArgs(NINT(2.0d0*ControlTypeNum-1+3))
          TempControlledZone(TempControlledZoneNum)%ControlTypeName(ControlTypeNum) = cAlphaArgs(NINT(2.0d0*ControlTypeNum+3))

          IF (TempControlledZone(TempControlledZoneNum)%ControlType(ControlTypeNum) /= ' ') THEN
            CTIndex=FindItem(TempControlledZone(TempControlledZoneNum)%ControlType(ControlTypeNum),ValidControlTypes,4)
            IF (CTIndex == 0) THEN
              CALL ShowSevereError(trim(cCurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid '//  &
                 trim(cAlphaFieldNames(NINT(2.0d0*ControlTypeNum-1+3)))//'="'//  &
                    trim(cAlphaArgs(NINT(2.0d0*ControlTypeNum-1+3)))//'"')
              ErrorsFound = .TRUE.
            END IF
          ELSE
            CALL ShowSevereError(trim(cCurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid '//  &
                 trim(cAlphaFieldNames(NINT(2.0d0*ControlTypeNum-1+3)))//'="<blank>"')
            ErrorsFound = .TRUE.
          ENDIF
          TempControlledZone(TempControlledZoneNum)%ControlTypeSchIndx(ControlTypeNum) = 0
        END DO
      END DO
    END DO ! NumTStatStatements
  ENDIF ! Check on number of TempControlledZones

  cCurrentModuleObject=ValidControlTypes(SglHeatSetPoint)
  NumSingleTempHeatingControls = GetNumObjectsFound(cCurrentModuleObject)

  IF (NumSingleTempHeatingControls .GT. 0) ALLOCATE(SetPointSingleHeating(NumSingleTempHeatingControls))

  DO SingleTempHeatingControlNum = 1, NumSingleTempHeatingControls
    CALL GetObjectItem(cCurrentModuleObject,SingleTempHeatingControlNum,cAlphaArgs,NumAlphas,rNumericArgs,NumNums,IOSTAT, &
                       NumBlank=lNumericFieldBlanks,AlphaBlank=lAlphaFieldBlanks, &
                       AlphaFieldNames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
    IsNotOK = .FALSE.
    IsBlank = .FALSE.
    CALL VerifyName(cAlphaArgs(1),SetPointSingleHeating%Name,SingleTempHeatingControlNum-1,IsNotOK,IsBlank, &
                    trim(cCurrentModuleObject)//' Name')
    IF (IsNotOK) THEN
      ErrorsFound = .TRUE.
      IF (IsBlank) cAlphaArgs(1) = 'xxxxx'
    END IF
    SetPointSingleHeating(SingleTempHeatingControlNum)%Name = cAlphaArgs(1)
    SetPointSingleHeating(SingleTempHeatingControlNum)%TempSchedName = cAlphaArgs(2)
    SetPointSingleHeating(SingleTempHeatingControlNum)%TempSchedIndex = GetScheduleIndex(cAlphaArgs(2))
    IF (SetPointSingleHeating(SingleTempHeatingControlNum)%TempSchedIndex == 0) THEN
      CALL ShowSevereError(trim(cCurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid '//  &
         trim(cAlphaFieldNames(2))//'="'//trim(cAlphaArgs(2))//'" not found.')
      ErrorsFound = .TRUE.
    END IF

  END DO ! SingleTempHeatingControlNum

  cCurrentModuleObject=ValidControlTypes(SglCoolSetPoint)
  NumSingleTempCoolingControls = GetNumObjectsFound(cCurrentModuleObject)

  IF (NumSingleTempCoolingControls .GT. 0) ALLOCATE(SetPointSingleCooling(NumSingleTempCoolingControls))

  DO SingleTempCoolingControlNum = 1, NumSingleTempCoolingControls
    CALL GetObjectItem(cCurrentModuleObject,SingleTempCoolingControlNum,cAlphaArgs,NumAlphas,rNumericArgs,NumNums,IOSTAT, &
                       NumBlank=lNumericFieldBlanks,AlphaBlank=lAlphaFieldBlanks, &
                       AlphaFieldNames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
    IsNotOK = .FALSE.
    IsBlank = .FALSE.
    CALL VerifyName(cAlphaArgs(1),SetPointSingleCooling%Name,SingleTempCoolingControlNum-1,IsNotOK,IsBlank, &
                    trim(cCurrentModuleObject)//' Name')
    IF (IsNotOK) THEN
      ErrorsFound = .TRUE.
      IF (IsBlank) cAlphaArgs(1) = 'xxxxx'
    END IF
    SetPointSingleCooling(SingleTempCoolingControlNum)%Name = cAlphaArgs(1)
    SetPointSingleCooling(SingleTempCoolingControlNum)%TempSchedName = cAlphaArgs(2)
    SetPointSingleCooling(SingleTempCoolingControlNum)%TempSchedIndex = GetScheduleIndex(cAlphaArgs(2))
    IF (SetPointSingleCooling(SingleTempCoolingControlNum)%TempSchedIndex == 0) THEN
      CALL ShowSevereError(trim(cCurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid '//  &
         trim(cAlphaFieldNames(2))//'="'//trim(cAlphaArgs(2))//'" not found.')
      ErrorsFound = .TRUE.
    END IF

  END DO ! SingleTempCoolingControlNum

  cCurrentModuleObject=ValidControlTypes(SglHCSetPoint)
  NumSingleTempHeatCoolControls = GetNumObjectsFound(cCurrentModuleObject)

  IF (NumSingleTempHeatCoolControls .GT. 0) ALLOCATE(SetPointSingleHeatCool(NumSingleTempHeatCoolControls))

  DO SingleTempHeatCoolControlNum = 1, NumSingleTempHeatCoolControls
    CALL GetObjectItem(cCurrentModuleObject,SingleTempHeatCoolControlNum,cAlphaArgs,NumAlphas,rNumericArgs,NumNums,IOSTAT, &
                       NumBlank=lNumericFieldBlanks,AlphaBlank=lAlphaFieldBlanks, &
                       AlphaFieldNames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
    SetPointSingleHeatCool(SingleTempHeatCoolControlNum)%Name = cAlphaArgs(1)
    SetPointSingleHeatCool(SingleTempHeatCoolControlNum)%TempSchedName = cAlphaArgs(2)
    SetPointSingleHeatCool(SingleTempHeatCoolControlNum)%TempSchedIndex = GetScheduleIndex(cAlphaArgs(2))
    IF (SetPointSingleHeatCool(SingleTempHeatCoolControlNum)%TempSchedIndex == 0) THEN
      CALL ShowSevereError(trim(cCurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid '//  &
         trim(cAlphaFieldNames(2))//'="'//trim(cAlphaArgs(2))//'" not found.')
      ErrorsFound = .TRUE.
    END IF

  END DO ! SingleTempHeatCoolControlNum

  cCurrentModuleObject=ValidControlTypes(DualSetPoint)
  NumDualTempHeatCoolControls = GetNumObjectsFound(cCurrentModuleObject)

  IF (NumDualTempHeatCoolControls .GT. 0) ALLOCATE(SetPointDualHeatCool(NumDualTempHeatCoolControls))

  DO DualTempHeatCoolControlNum = 1, NumDualTempHeatCoolControls
    CALL GetObjectItem(cCurrentModuleObject,DualTempHeatCoolControlNum,cAlphaArgs,NumAlphas,rNumericArgs,NumNums,IOSTAT, &
                       NumBlank=lNumericFieldBlanks,AlphaBlank=lAlphaFieldBlanks, &
                       AlphaFieldNames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
    IsNotOK = .FALSE.
    IsBlank = .FALSE.
    CALL VerifyName(cAlphaArgs(1),SetPointDualHeatCool%Name,DualTempHeatCoolControlNum-1,IsNotOK,IsBlank, &
                    trim(cCurrentModuleObject)//' Name')
    IF (IsNotOK) THEN
      ErrorsFound = .TRUE.
      IF (IsBlank) cAlphaArgs(1) = 'xxxxx'
    END IF
    SetPointDualHeatCool(DualTempHeatCoolControlNum)%Name = cAlphaArgs(1)
    SetPointDualHeatCool(DualTempHeatCoolControlNum)%HeatTempSetptSchedName = cAlphaArgs(2)
    SetPointDualHeatCool(DualTempHeatCoolControlNum)%HeatTempSchedIndex = GetScheduleIndex(cAlphaArgs(2))
    IF (SetPointDualHeatCool(DualTempHeatCoolControlNum)%HeatTempSchedIndex == 0) THEN
      CALL ShowSevereError(trim(cCurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid '//  &
         trim(cAlphaFieldNames(2))//'="'//trim(cAlphaArgs(2))//'" not found.')
      ErrorsFound = .TRUE.
    END IF
    SetPointDualHeatCool(DualTempHeatCoolControlNum)%CoolTempSetptSchedName = cAlphaArgs(3)
    SetPointDualHeatCool(DualTempHeatCoolControlNum)%CoolTempSchedIndex = GetScheduleIndex(cAlphaArgs(3))
    IF (SetPointDualHeatCool(DualTempHeatCoolControlNum)%CoolTempSchedIndex == 0) THEN
      CALL ShowSevereError(trim(cCurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid '//  &
         trim(cAlphaFieldNames(3))//'="'//trim(cAlphaArgs(3))//'" not found.')
      ErrorsFound = .TRUE.
    END IF

  END DO ! DualTempHeatCoolControlNum

 ! Finish filling in Schedule pointing indexes
  DO TempControlledZoneNum = 1, NumTempControlledZones
    TempIndex = FindItem(trim(ValidControlTypes(SglHeatSetPoint)),   &
       TempControlledZone(TempControlledZoneNum)%ControlType,TempControlledZone(TempControlledZoneNum)%NumControlTypes)
    TempControlledZone(TempControlledZoneNum)%SchIndx_SingleHeatSetPoint = TempIndex
    IF (TempIndex > 0) THEN
      TempControlledZone(TempControlledZoneNum)%ControlTypeSchIndx(TempIndex) =   &
                           FindItem(TempControlledZone(TempControlledZoneNum)%ControlTypeName(TempIndex), &
                                        SetPointSingleHeating%Name, NumSingleTempHeatingControls)
      TStatControlTypes(TempControlledZoneNum)%MustHave(SingleHeatingSetpoint)=.true.
    ENDIF

    TempIndex = FindItem(trim(ValidControlTypes(SglCoolSetPoint)),  &
      TempControlledZone(TempControlledZoneNum)%ControlType,TempControlledZone(TempControlledZoneNum)%NumControlTypes)
    TempControlledZone(TempControlledZoneNum)%SchIndx_SingleCoolSetPoint = TempIndex
    IF (TempIndex > 0) THEN
      TempControlledZone(TempControlledZoneNum)%ControlTypeSchIndx(TempIndex) =   &
                           FindItem(TempControlledZone(TempControlledZoneNum)%ControlTypeName(TempIndex), &
                                        SetPointSingleCooling%Name, NumSingleTempCoolingControls)
      TStatControlTypes(TempControlledZoneNum)%MustHave(SingleCoolingSetpoint)=.true.
    ENDIF

    TempIndex = FindItem(trim(ValidControlTypes(SglHCSetPoint)),  &
      TempControlledZone(TempControlledZoneNum)%ControlType,TempControlledZone(TempControlledZoneNum)%NumControlTypes)
    TempControlledZone(TempControlledZoneNum)%SchIndx_SingleHeatCoolSetPoint = TempIndex
    IF (TempIndex > 0) THEN
      TempControlledZone(TempControlledZoneNum)%ControlTypeSchIndx(TempIndex) =   &
                           FindItem(TempControlledZone(TempControlledZoneNum)%ControlTypeName(TempIndex), &
                                        SetPointSingleHeatCool%Name, NumSingleTempHeatCoolControls)
      TStatControlTypes(TempControlledZoneNum)%MustHave(SingleHeatCoolSetPoint)=.true.
    ENDIF

    TempIndex = FindItem(trim(ValidControlTypes(DualSetPoint)),  &
      TempControlledZone(TempControlledZoneNum)%ControlType,TempControlledZone(TempControlledZoneNum)%NumControlTypes)
    TempControlledZone(TempControlledZoneNum)%SchIndx_DualSetPointWDeadBand = TempIndex
    IF (TempIndex > 0) THEN
      TempControlledZone(TempControlledZoneNum)%ControlTypeSchIndx(TempIndex) =   &
                           FindItem(TempControlledZone(TempControlledZoneNum)%ControlTypeName(TempIndex), &
                                        SetPointDualHeatCool%Name, NumDualTempHeatCoolControls)
      TStatControlTypes(TempControlledZoneNum)%MustHave(DualSetPointWithDeadBand)=.true.
    ENDIF
  ENDDO

  ! Now, Check the schedule values/indices for validity

  DO TempControlledZoneNum = 1, NumTempControlledZones

    ActualZoneNum = TempControlledZone(TempControlledZoneNum)%ActualZoneNum
    CTIndex = TempControlledZone(TempControlledZoneNum)%CTSchedIndex
    IF (CTIndex == 0) CYCLE   ! error will be caught elsewhere
    SchedMin=GetScheduleMinValue(CTIndex)
    SchedMax=GetScheduleMaxValue(CTIndex)

    IF (SchedMin == 0 .and. SchedMax == 0) THEN
      IF (FindNumberInList(CTIndex,CTSchedMapToControlledZone,NumTempControlledZones) == 0) THEN
        CALL ShowSevereError('Control Type Schedule='//TRIM(TempControlledZone(TempControlledZoneNum)%ControlTypeSchedName))
        CALL ShowContinueError('..specifies control type 0 for all entries.')
        CALL ShowContinueError('All zones using this Control Type Schedule have no heating or cooling available.')
      ENDIF
      CTSchedMapToControlledZone(TempControlledZoneNum)=CTIndex
    ENDIF

    DO ControlTypeNum=SchedMin,SchedMax

      SELECT CASE (ControlTypeNum)

        CASE (0) ! Uncontrolled

        CASE (SingleHeatingSetPoint)

          TempIndex=TempControlledZone(TempControlledZoneNum)%SchIndx_SingleHeatSetPoint
          TStatControlTypes(TempControlledZoneNum)%DidHave(SingleHeatingSetPoint)=.true.
          IF (TempIndex /= 0) THEN
            SchedTypeIndex = TempControlledZone(TempControlledZoneNum)%ControlTypeSchIndx(TempIndex)
            IF (SchedTypeIndex == 0) THEN
              CALL ShowSevereError('GetZoneAirSetpoints: Could not find '//trim(ValidControlTypes(SglHeatSetPoint))//  &
                 ' Schedule='//TRIM(TempControlledZone(TempControlledZoneNum)%ControlTypeName(TempIndex)))
              ErrorsFound=.true.
            END IF
          ELSE   ! TempIndex = 0
            IF (CheckScheduleValue(CTIndex,SingleHeatingSetPoint)) THEN
              CALL ShowSevereError('Control Type Schedule='//TRIM(TempControlledZone(TempControlledZoneNum)%ControlTypeSchedName))
              CALL ShowContinueError('..specifies control type 1 ('//trim(ValidControlTypes(SglHeatSetPoint))//  &
                 ') as the control type. Not valid for this zone.')
              CALL ShowContinueError('..reference '//trim(cZControlTypes(iZC_TStat))//'='//  &
                 TRIM(TempControlledZone(TempControlledZoneNum)%Name))
              CALL ShowContinueError('..reference ZONE='//TRIM(TempControlledZone(TempControlledZoneNum)%ZoneName))
              ErrorsFound=.true.
            ENDIF
          ENDIF

        CASE (SingleCoolingSetPoint)

          TempIndex = TempControlledZone(TempControlledZoneNum)%SchIndx_SingleCoolSetPoint
          TStatControlTypes(TempControlledZoneNum)%DidHave(SingleCoolingSetPoint)=.true.
          IF (TempIndex /= 0) THEN
            SchedTypeIndex = TempControlledZone(TempControlledZoneNum)%ControlTypeSchIndx(TempIndex)
            IF (SchedTypeIndex == 0) THEN
              CALL ShowSevereError('GetZoneAirSetpoints: Could not find '//trim(ValidControlTypes(SglCoolSetPoint))//  &
                 ' Schedule='//TRIM(TempControlledZone(TempControlledZoneNum)%ControlTypeName(TempIndex)))
              ErrorsFound=.true.
            END IF
          ELSE   ! TempIndex = 0
            IF (CheckScheduleValue(CTIndex,SingleCoolingSetPoint)) THEN
              CALL ShowSevereError('Control Type Schedule='//TRIM(TempControlledZone(TempControlledZoneNum)%ControlTypeSchedName))
              CALL ShowContinueError('..specifies control type 2 ('//trim(ValidControlTypes(SglCoolSetPoint))//  &
                 ') as the control type. Not valid for this zone.')
              CALL ShowContinueError('..reference '//trim(cZControlTypes(iZC_TStat))//'='//  &
                 TRIM(TempControlledZone(TempControlledZoneNum)%Name))
              CALL ShowContinueError('..reference ZONE='//TRIM(TempControlledZone(TempControlledZoneNum)%ZoneName))
              ErrorsFound=.true.
            ENDIF
          ENDIF

        CASE (SingleHeatCoolSetPoint)

          TempIndex = TempControlledZone(TempControlledZoneNum)%SchIndx_SingleHeatCoolSetPoint
          TStatControlTypes(TempControlledZoneNum)%DidHave(SingleHeatCoolSetPoint)=.true.
          IF (TempIndex /= 0) THEN
            SchedTypeIndex = TempControlledZone(TempControlledZoneNum)%ControlTypeSchIndx(TempIndex)
            IF (SchedTypeIndex == 0) THEN
              CALL ShowSevereError('GetZoneAirSetpoints: Could not find '//trim(ValidControlTypes(SglHCSetPoint))//  &
                 ' Schedule='//TRIM(TempControlledZone(TempControlledZoneNum)%ControlTypeName(TempIndex)))
              ErrorsFound=.true.
            END IF
          ELSE   ! TempIndex = 0
            IF (CheckScheduleValue(CTIndex,SingleHeatCoolSetPoint)) THEN
              CALL ShowSevereError('Schedule='//TRIM(TempControlledZone(TempControlledZoneNum)%ControlTypeSchedName))
              CALL ShowContinueError('..specifies control type 3 ('//trim(ValidControlTypes(SglHCSetPoint))//  &
                 ') as the control type. Not valid for this zone.')
              CALL ShowContinueError('..reference '//trim(cZControlTypes(iZC_TStat))//'='//  &
                 TRIM(TempControlledZone(TempControlledZoneNum)%Name))
              CALL ShowContinueError('..reference ZONE='//TRIM(TempControlledZone(TempControlledZoneNum)%ZoneName))
              ErrorsFound=.true.
            ENDIF
          ENDIF

        CASE (DualSetPointWithDeadBand)

          TempIndex = TempControlledZone(TempControlledZoneNum)%SchIndx_DualSetPointWDeadBand
          TStatControlTypes(TempControlledZoneNum)%DidHave(DualSetPointWithDeadBand)=.true.
          IF (TempIndex /= 0) THEN
            SchedTypeIndex = TempControlledZone(TempControlledZoneNum)%ControlTypeSchIndx(TempIndex)
            IF (SchedTypeIndex == 0) THEN
              CALL ShowSevereError('GetZoneAirSetpoints: Could not find '//trim(ValidControlTypes(DualSetPoint))//  &
                 ' Schedule='//TRIM(TempControlledZone(TempControlledZoneNum)%ControlTypeName(TempIndex)))
              ErrorsFound=.true.
            END IF
          ELSE   ! TempIndex = 0
            IF (CheckScheduleValue(CTIndex,DualSetPointWithDeadBand)) THEN
              CALL ShowSevereError('Schedule='//TRIM(TempControlledZone(TempControlledZoneNum)%ControlTypeSchedName))
              CALL ShowContinueError('..specifies control type 4 ('//trim(ValidControlTypes(DualSetPoint))//  &
                 ') as the control type. Not valid for this zone.')
              CALL ShowContinueError('..reference '//trim(cZControlTypes(iZC_TStat))//'='//  &
                 TRIM(TempControlledZone(TempControlledZoneNum)%Name))
              CALL ShowContinueError('..reference ZONE='//TRIM(TempControlledZone(TempControlledZoneNum)%ZoneName))
              ErrorsFound=.true.
            ENDIF
          ENDIF

        CASE DEFAULT
          CALL ShowSevereError('GetZoneAirSetpoints: Illegal control type for Zone='//TRIM(Zone(ActualZoneNum)%Name)//  &
                               ', Found value='//TRIM(TrimSigDigits(ControlTypeNum))//', in Schedule='//        &
                               TRIM(TempControlledZone(TempControlledZoneNum)%ControlTypeSchedName))
          CALL ShowContinueError('..valid range values are [0,4].')
          ErrorsFound=.true.

      END SELECT
    END DO

  END DO

  DO TempControlledZoneNum = 1, NumTempControlledZones

    ActualZoneNum = TempControlledZone(TempControlledZoneNum)%ActualZoneNum
    CTIndex = TempControlledZone(TempControlledZoneNum)%CTSchedIndex
    IF (CTIndex == 0) CYCLE  ! error caught elsewhere -- would just be confusing here

    DO ControlTypeNum=1,4
      IF (TStatControlTypes(TempControlledZoneNum)%MustHave(ControlTypeNum) .and.  &
          TStatControlTypes(TempControlledZoneNum)%DidHave(ControlTypeNum)) CYCLE

      SELECT CASE (ControlTypeNum)

        CASE (SingleHeatingSetPoint)
          IF (.not. TStatControlTypes(TempControlledZoneNum)%MustHave(ControlTypeNum)) CYCLE
          CALL ShowWarningError('Schedule='//TRIM(TempControlledZone(TempControlledZoneNum)%ControlTypeSchedName))
          CALL ShowContinueError('...should include control type 1 ('//trim(ValidControlTypes(SglHeatSetPoint))//  &
                      ') but does not.')
          CALL ShowContinueError('..reference '//trim(cZControlTypes(iZC_TStat))//'='//  &
                   TRIM(TempControlledZone(TempControlledZoneNum)%Name))
          CALL ShowContinueError('..reference ZONE='//TRIM(TempControlledZone(TempControlledZoneNum)%ZoneName))

        CASE (SingleCoolingSetPoint)
          IF (.not. TStatControlTypes(TempControlledZoneNum)%MustHave(ControlTypeNum)) CYCLE
          CALL ShowWarningError('Schedule='//TRIM(TempControlledZone(TempControlledZoneNum)%ControlTypeSchedName))
          CALL ShowContinueError('...should include control type 2 ('//trim(ValidControlTypes(SglCoolSetPoint))//  &
                      ') but does not.')
          CALL ShowContinueError('..reference '//trim(cZControlTypes(iZC_TStat))//'='//  &
                   TRIM(TempControlledZone(TempControlledZoneNum)%Name))
          CALL ShowContinueError('..reference ZONE='//TRIM(TempControlledZone(TempControlledZoneNum)%ZoneName))

        CASE (SingleHeatCoolSetPoint)
          IF (.not. TStatControlTypes(TempControlledZoneNum)%MustHave(ControlTypeNum)) CYCLE
          CALL ShowWarningError('Schedule='//TRIM(TempControlledZone(TempControlledZoneNum)%ControlTypeSchedName))
          CALL ShowContinueError('...should include control type 3 ('//trim(ValidControlTypes(SglHCSetPoint))//  &
                      ') but does not.')
          CALL ShowContinueError('..reference '//trim(cZControlTypes(iZC_TStat))//'='//  &
                   TRIM(TempControlledZone(TempControlledZoneNum)%Name))
          CALL ShowContinueError('..reference ZONE='//TRIM(TempControlledZone(TempControlledZoneNum)%ZoneName))

        CASE (DualSetPointWithDeadBand)
          IF (.not. TStatControlTypes(TempControlledZoneNum)%MustHave(ControlTypeNum)) CYCLE
          CALL ShowWarningError('Schedule='//TRIM(TempControlledZone(TempControlledZoneNum)%ControlTypeSchedName))
          CALL ShowContinueError('...should include control type 4 ('//trim(ValidControlTypes(DualSetPoint))//  &
                      ') but does not.')
          CALL ShowContinueError('..reference '//trim(cZControlTypes(iZC_TStat))//'='//  &
                   TRIM(TempControlledZone(TempControlledZoneNum)%Name))
          CALL ShowContinueError('..reference ZONE='//TRIM(TempControlledZone(TempControlledZoneNum)%ZoneName))

        CASE DEFAULT
      END SELECT
    ENDDO
  ENDDO

  IF (ALLOCATED(TStatControlTypes)) DEALLOCATE(TStatControlTypes)
  ! This starts the Humidity Control Get Input section
  cCurrentModuleObject=cZControlTypes(iZC_HStat)
  NumHumidityControlZones = GetNumObjectsFound(cCurrentModuleObject)

  IF (NumHumidityControlZones .GT. 0) ALLOCATE(HumidityControlZone(NumHumidityControlZones))

  DO HumidControlledZoneNum = 1, NumHumidityControlZones
    CALL GetObjectItem(cCurrentModuleObject,HumidControlledZoneNum,cAlphaArgs,NumAlphas,rNumericArgs,NumNums,IOSTAT, &
                       NumBlank=lNumericFieldBlanks,AlphaBlank=lAlphaFieldBlanks, &
                       AlphaFieldNames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
    IsNotOK = .FALSE.
    IsBlank = .FALSE.
    CALL VerifyName(cAlphaArgs(1),HumidityControlZone%ControlName,HumidControlledZoneNum-1,IsNotOK,IsBlank, &
                    trim(cCurrentModuleObject)//' Name')
    IF (IsNotOK) THEN
      ErrorsFound = .TRUE.
      IF (IsBlank) cAlphaArgs(1) = 'xxxxx'
    END IF
    HumidityControlZone(HumidControlledZoneNum)%ControlName = cAlphaArgs(1)
    ! Ensure unique zone name
    IsNotOK = .FALSE.
    IsBlank = .FALSE.
    CALL VerifyName(cAlphaArgs(2),HumidityControlZone%ZoneName,HumidControlledZoneNum-1,IsNotOK,IsBlank, &
                    trim(cCurrentModuleObject)//' Zone Name')
    IF (IsNotOK) THEN
      ErrorsFound = .TRUE.
      IF (IsBlank) cAlphaArgs(2) = 'xxxxx'
    END IF

    HumidityControlZone(HumidControlledZoneNum)%ZoneName = cAlphaArgs(2)
    HumidityControlZone(HumidControlledZoneNum)%ActualZoneNum = FindItem(cAlphaArgs(2),Zone%Name,NumOfZones)
    IF (HumidityControlZone(HumidControlledZoneNum)%ActualZoneNum == 0) THEN
      CALL ShowSevereError(trim(cCurrentModuleObject)//'="'//trim(cAlphaArgs(1))//' invalid '//  &
         trim(cAlphaFieldNames(2))//'="'//trim(cAlphaArgs(2))//'" not found.')
      ErrorsFound = .TRUE.
    END IF
    HumidityControlZone(HumidControlledZoneNum)%HumidifyingSched = cAlphaArgs(3)
    HumidityControlZone(HumidControlledZoneNum)%HumidifyingSchedIndex = GetScheduleIndex(cAlphaArgs(3))
    IF (HumidityControlZone(HumidControlledZoneNum)%HumidifyingSchedIndex == 0) THEN
      CALL ShowSevereError(trim(cCurrentModuleObject)//'="'//trim(cAlphaArgs(1))//' invalid '//  &
         trim(cAlphaFieldNames(3))//'="'//trim(cAlphaArgs(3))//'" not found.')
      ErrorsFound = .TRUE.
    END IF
    If (NumAlphas .eq. 4) Then
      HumidityControlZone(HumidControlledZoneNum)%DehumidifyingSched = cAlphaArgs(4)
      HumidityControlZone(HumidControlledZoneNum)%DehumidifyingSchedIndex = GetScheduleIndex(cAlphaArgs(4))
      IF (HumidityControlZone(HumidControlledZoneNum)%DehumidifyingSchedIndex == 0) THEN
        CALL ShowSevereError(trim(cCurrentModuleObject)//'="'//trim(cAlphaArgs(1))//' invalid '//  &
           trim(cAlphaFieldNames(4))//'="'//trim(cAlphaArgs(4))//'" not found.')
        ErrorsFound = .TRUE.
      END IF
    Else
      HumidityControlZone(HumidControlledZoneNum)%DehumidifyingSched = cAlphaArgs(3)
      HumidityControlZone(HumidControlledZoneNum)%DehumidifyingSchedIndex = GetScheduleIndex(cAlphaArgs(3))
    End If

  END DO ! HumidControlledZoneNum

  ! Start to read Thermal comfort control objects
  cCurrentModuleObject=cZControlTypes(iZC_TCTStat)
  NumComfortTStatStatements = GetNumObjectsFound(cCurrentModuleObject)
  ALLOCATE(ComfortTStatObjects(NumComfortTStatStatements))

! Pre-scan for use of Zone lists in TStat statements (i.e. Global application of TStat)
  NumComfortControlledZones=0
  ErrFlag=.false.
  DO Item=1,NumComfortTStatStatements
    CALL GetObjectItem(cCurrentModuleObject,Item,cAlphaArgs,NumAlphas,rNumericArgs,NumNums,IOSTAT, &
                       NumBlank=lNumericFieldBlanks,AlphaBlank=lAlphaFieldBlanks, &
                       AlphaFieldNames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
    ! will not do much verifying -- that will come later.
    IsNotOK = .FALSE.
    IsBlank = .FALSE.
    CALL VerifyName(cAlphaArgs(1),ComfortTStatObjects%Name,Item-1,IsNotOK,IsBlank, &
                    trim(cCurrentModuleObject)//' Name')
    IF (IsNotOK) THEN
      ErrFlag = .TRUE.
      ErrorsFound=.true.
      IF (IsBlank) cAlphaArgs(1) = 'xxxxx'
    END IF
    Item1=FindItemInList(cAlphaArgs(2),Zone%Name,NumOfZones)
    ZLItem=0
    IF (Item1 == 0 .and. NumOfZoneLists > 0) &
      ZLItem=FindItemInList(cAlphaArgs(2),ZoneList%Name,NumOfZoneLists)
    ComfortTStatObjects(Item)%Name=cAlphaArgs(1)
    IF (Item1 > 0) THEN
      ComfortTStatObjects(Item)%ComfortControlledZoneStartPtr=NumComfortControlledZones+1
      NumComfortControlledZones=NumComfortControlledZones+1
      ComfortTStatObjects(Item)%NumOfZones=1
      ComfortTStatObjects(Item)%ZoneListActive=.false.
      ComfortTStatObjects(Item)%ZoneOrZoneListPtr=Item1
    ELSEIF (ZLItem > 0) THEN
      ComfortTStatObjects(Item)%ComfortControlledZoneStartPtr=NumComfortControlledZones+1
      NumComfortControlledZones=NumComfortControlledZones+ZoneList(ZLItem)%NumOfZones
      ComfortTStatObjects(Item)%NumOfZones=ZoneList(ZLItem)%NumOfZones
      ComfortTStatObjects(Item)%ZoneListActive=.true.
      ComfortTStatObjects(Item)%ZoneOrZoneListPtr=ZLItem
    ELSE
      CALL ShowSevereError(trim(cCurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid '//  &
           trim(cAlphaFieldNames(2))//'="'//trim(cAlphaArgs(2))//'" not found.')
      ErrFlag=.true.
      ErrorsFound=.true.
    ENDIF
  ENDDO

  IF (ErrFlag) THEN
    CALL ShowSevereError('GetZoneAirSetpoints: Errors with invalid names in '//trim(cCurrentModuleObject)//  &
       ' objects.')
    CALL ShowContinueError('...These will not be read in.  Other errors may occur.')
    NumComfortControlledZones=0
  ENDIF

  IF (NumComfortControlledZones > 0) THEN
    ALLOCATE(ComfortControlledZone(NumComfortControlledZones))
    ALLOCATE(TComfortControlTypes(NumComfortControlledZones))  ! Number of set point types
    ALLOCATE(CCmSchedMapToControlledZone(NumComfortControlledZones))
    CCmSchedMapToControlledZone=0

    ComfortControlledZoneNum=0
    DO Item = 1, NumComfortTStatStatements
      CALL GetObjectItem(cCurrentModuleObject,Item,cAlphaArgs,NumAlphas,rNumericArgs,NumNums,IOSTAT, &
                         NumBlank=lNumericFieldBlanks,AlphaBlank=lAlphaFieldBlanks, &
                         AlphaFieldNames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
      DO Item1=1,ComfortTStatObjects(Item)%NumOfZones
        ComfortControlledZoneNum=ComfortControlledZoneNum+1
        IF (ComfortTStatObjects(Item)%ZoneListActive) THEN
          cAlphaArgs(2)=Zone(ZoneList(ComfortTStatObjects(Item)%ZoneOrZoneListPtr)%Zone(Item1))%Name
        ENDIF
        ZoneAssigned=FindItemInList(cAlphaArgs(2),ComfortControlledZone%ZoneName,ComfortControlledZoneNum-1)
        IF (ZoneAssigned == 0) THEN
          ComfortControlledZone(ComfortControlledZoneNum)%ZoneName = cAlphaArgs(2)
          ComfortControlledZone(ComfortControlledZoneNum)%ActualZoneNum = FindIteminList(cAlphaArgs(2),Zone%Name,NumOfZones)
          IF (ComfortControlledZone(ComfortControlledZoneNum)%ActualZoneNum == 0) THEN
            CALL ShowSevereError(trim(cCurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid '//  &
               trim(cAlphaFieldNames(2))//'="'//trim(cAlphaArgs(2))//'" not found.')
            ErrorsFound = .TRUE.
          END IF
        ELSE
          ComfortControlledZone(ComfortControlledZoneNum)%ZoneName = cAlphaArgs(2)  ! for continuity
          CALL ShowSevereError(trim(cCurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid '//  &
             trim(cAlphaFieldNames(2))//'="'//trim(cAlphaArgs(2))//'" zone previously assigned.')
          CALL ShowContinueError('...Zone was previously assigned to Thermostat="'//  &
             trim(ComfortControlledZone(ZoneAssigned)%Name)//'".')
          ErrorsFound = .TRUE.
          CYCLE
        ENDIF

        IF (.not. ComfortTStatObjects(Item)%ZoneListActive) THEN
          ComfortControlledZone(ComfortControlledZoneNum)%Name = cAlphaArgs(1)
        ELSE
          ComfortControlledZone(ComfortControlledZoneNum)%Name =   &
             trim(Zone(ZoneList(ComfortTStatObjects(Item)%ZoneOrZoneListPtr)%Zone(Item1))%Name)//  &
                ' '//trim(ComfortTStatObjects(Item)%Name)
        ENDIF

        ! Read Fields A3 and A4 for averaging method
        IZoneCount = 0
        Do i=1,TotPeople
          If (ComfortControlledZone(ComfortControlledZoneNum)%ActualZoneNum == People(I)%ZonePtr) then
            IZoneCount = IZoneCount+1
          End If
        End Do
        ! Could not find a people object for this particular zone
        If (IZoneCount == 0 .and. ComfortControlledZone(ComfortControlledZoneNum)%ActualZoneNum > 0) then
          CALL ShowSevereError(trim(cCurrentModuleObject)//'="'//trim(cAlphaArgs(1))//' no PEOPLE in '//  &
             trim(cAlphaFieldNames(2))//'="'//trim(cAlphaArgs(2))//'" - cannot use Comfort Control.')
          ErrorsFound=.true.
        End If
        ComfortControlledZone(ComfortControlledZoneNum)%AverageMethodNum = AverageMethodNum_NO
        If (IZoneCount > 1) then
          ComfortControlledZone(ComfortControlledZoneNum)%AverageMethodName = cAlphaArgs(3)
          If (SameString(cAlphaArgs(3),'SpecificObject')) then
            ComfortControlledZone(ComfortControlledZoneNum)%AverageMethodNum = AverageMethodNum_SPE
          End If
          If (SameString(cAlphaArgs(3),'ObjectAverage')) then
            ComfortControlledZone(ComfortControlledZoneNum)%AverageMethodNum = AverageMethodNum_OBJ
          End If
          If (SameString(cAlphaArgs(3),'PeopleAverage')) then
            ComfortControlledZone(ComfortControlledZoneNum)%AverageMethodNum = AverageMethodNum_PEO
          End If
          If (ComfortControlledZone(ComfortControlledZoneNum)%AverageMethodNum == 0) then
            CALL ShowSevereError(trim(cCurrentModuleObject)//'="'//trim(cAlphaArgs(1))//' invalid '//  &
             trim(cAlphaFieldNames(3))//'="'//trim(cAlphaArgs(3))//'".')
            CALL ShowContinueError('Allowed keys are SpecificObject, ObjectAverage, or PeopleAverage')
            ErrorsFound=.true.
          End If
          If (ComfortControlledZone(ComfortControlledZoneNum)%AverageMethodNum == AverageMethodNum_SPE) then
            ComfortControlledZone(ComfortControlledZoneNum)%AverageObjectName = cAlphaArgs(4)
            If (FindItem(cAlphaArgs(4),People%Name,TotPeople) == 0) then
              CALL ShowSevereError(trim(cCurrentModuleObject)//'="'//trim(cAlphaArgs(1))//' invalid '//  &
                 trim(cAlphaFieldNames(4))//'="'//trim(cAlphaArgs(4))//'".')
              ErrorsFound=.true.
            Else
              ComfortControlledZone(ComfortControlledZoneNum)%SpecificObjectNum =  &
                                  FindItem(cAlphaArgs(4),People%Name,TotPeople)
            End If
          End If
        Else
          Do i=1,TotPeople
            If (ComfortControlledZone(ComfortControlledZoneNum)%ActualZoneNum == People(I)%ZonePtr) Exit
          End Do
          ComfortControlledZone(ComfortControlledZoneNum)%SpecificObjectNum = i
        End If
        ! Check values used for thermal comfort calculation
        Do i=1,TotPeople
          If (ComfortControlledZone(ComfortControlledZoneNum)%ActualZoneNum == People(I)%ZonePtr) then
            ! Check activity level
            If (People(i)%ActivityLevelPtr > 0) then
              ValidScheduleControlType=CheckScheduleValueMinMax( &
                                   People(i)%ActivityLevelPtr, '>=',72.0d0, '<=',909.0d0)
              IF (.not. ValidScheduleControlType) THEN
                CALL ShowSevereError('GetPeople Activity Level: Invalid activity level values entered '// &
                                 'for thermal comfort calculation')
                CALL ShowContinueError('Outside of range values [72,909], Reference object='//TRIM(People(i)%Name))
                ErrorsFound=.TRUE.
              ENDIF
            Else
              CALL ShowSevereError('GetPeople Activity Level: Activity level schedule is not found='//TRIM(People(i)%Name))
              CALL ShowContinueError('Required when the zone has Thermal Comfort Controls.')
              ErrorsFound=.TRUE.
            End If
            ! Check Work Efficiency
            If (People(i)%WorkEffPtr > 0) then
              ValidScheduleControlType=CheckScheduleValueMinMax( &
                                     People(i)%WorkEffPtr, '>=',0.0d0, '<=',1.0d0)
              IF (.not. ValidScheduleControlType) THEN
                CALL ShowSevereError('GetPeople work efficiency: Invalid work efficiency values entered '// &
                                   'for thermal comfort calculation')
                CALL ShowContinueError('Outside of range values [0,1], Reference object='//TRIM(People(i)%Name))
                ErrorsFound=.TRUE.
              ENDIF
            Else
              CALL ShowSevereError('GetPeople work efficiency: Work efficiency schedule is not found='//TRIM(People(i)%Name))
              CALL ShowContinueError('Required when the zone has Thermal Comfort Controls.')
              ErrorsFound=.TRUE.
            End If
            ! Check Clothing Insulation
            If (People(i)%ClothingPtr > 0) then
              ValidScheduleControlType=CheckScheduleValueMinMax( &
                                     People(i)%ClothingPtr, '>',0.0d0, '<=',2.0d0)
              IF (.not. ValidScheduleControlType) THEN
                CALL ShowSevereError('GetPeople Clothing Insulation: Invalid Clothing Insulation values entered '// &
                                   'for thermal comfort calculation')
                CALL ShowContinueError('Outside of range values [0.0,2.0], Reference object='//TRIM(People(i)%Name))
                ErrorsFound=.TRUE.
              ENDIF
            Else
              CALL ShowSevereError('GetPeople Clothing Insulation: Clothing Insulation schedule is not found='//  &
                 TRIM(People(i)%Name))
              CALL ShowContinueError('Required when the zone has Thermal Comfort Controls.')
              ErrorsFound=.TRUE.
            End If
            ! Check Air velocity
            If (People(i)%AirVelocityPtr .LE. 0) then
              CALL ShowSevereError('GetPeople Air Velocity: Air velocity schedule is not found='//TRIM(People(i)%Name))
              CALL ShowContinueError('Required when the zone has Thermal Comfort Controls.')
              ErrorsFound=.TRUE.
            End If
          End If
        End Do

        ! Read Max and Min temperature setpoint
        If (NumNums > 0) then
          ComfortControlledZone(ComfortControlledZoneNum)%TdbMinSetPoint = rNumericArgs(1)
          If (rNumericArgs(1) > 50 .OR. rNumericArgs(1) < 0) then
            CALL ShowSevereError(trim(cCurrentModuleObject)//'="'//trim(cAlphaArgs(1))//' invalid '//  &
             trim(cNumericFieldNames(1))//'=['//trim(TrimSigDigits(rNumericArgs(1),0))//'].')
            CALL ShowContinueError('..Allowable values must be between 0 C and 50 C')
            ErrorsFound = .TRUE.
          End If
        End If
        If (NumNums > 1) then
          ComfortControlledZone(ComfortControlledZoneNum)%TdbMaxSetPoint = rNumericArgs(2)
          If (rNumericArgs(2) > 50 .OR. rNumericArgs(2) < 0) then
            CALL ShowSevereError(trim(cCurrentModuleObject)//'="'//trim(cAlphaArgs(1))//' invalid '//  &
             trim(cNumericFieldNames(2))//'=['//trim(TrimSigDigits(rNumericArgs(2),0))//'].')
            CALL ShowContinueError('..Allowable values must be between 0 C and 50 C')
            ErrorsFound = .TRUE.
          End If
        End If
        ! Ensure MaxTemp >= MinTemp
        If (ComfortControlledZone(ComfortControlledZoneNum)%TdbMinSetPoint > &
            ComfortControlledZone(ComfortControlledZoneNum)%TdbMaxSetPoint) then
            CALL ShowSevereError(trim(cCurrentModuleObject)//'="'//trim(cAlphaArgs(1)))
            CALL ShowContinueError('..'//trim(cNumericFieldNames(1))//' > '//trim(cNumericFieldNames(2)))
            CALL ShowContinueError('..['//trim(TrimSigDigits(rNumericArgs(1),0))//'] > ['//  &
               trim(TrimSigDigits(rNumericArgs(2),0))//'].')
            ErrorsFound = .TRUE.
        End If
        ! If MaxTemp = MinTemp, no thermal comfort control
        If (ComfortControlledZone(ComfortControlledZoneNum)%TdbMinSetPoint == &
            ComfortControlledZone(ComfortControlledZoneNum)%TdbMaxSetPoint) then
          CALL ShowSevereError(trim(cCurrentModuleObject)//'="'//trim(cAlphaArgs(1)))
          CALL ShowContinueError('..'//trim(cNumericFieldNames(1))//' = '//trim(cNumericFieldNames(2)))
          CALL ShowContinueError('The zone will be controlled using this dry-bulb temperature setpoint.')
        End If
        ! read Thermal comfort type schedule name
        ComfortControlledZone(ComfortControlledZoneNum)%ControlTypeSchedName = cAlphaArgs(5)
        ComfortControlledZone(ComfortControlledZoneNum)%ComfortSchedIndex=GetScheduleIndex(cAlphaArgs(5))
        IF (ComfortControlledZone(ComfortControlledZoneNum)%ComfortSchedIndex == 0) THEN
          CALL ShowSevereError(trim(cCurrentModuleObject)//'="'//trim(cAlphaArgs(1))//' invalid '//  &
             trim(cAlphaFieldNames(5))//'="'//trim(cAlphaArgs(5))//'" not found.')
          ErrorsFound = .TRUE.
        ELSE
          ! Check validity of control types.
          ValidScheduleControlType=CheckScheduleValueMinMax(ComfortControlledZone(ComfortControlledZoneNum)%ComfortSchedIndex, &
                                                           '>=',0.0d0, '<=',4.0d0)
          IF (.not. ValidScheduleControlType) THEN
            CALL ShowSevereError(trim(cCurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid range '//  &
               trim(cAlphaFieldNames(5))//'="'//trim(cAlphaArgs(5))//'"')
            CALL ShowContinueError('..contains values outside of range [0,4].')
            ErrorsFound=.TRUE.
          ENDIF
        END IF
        ComfortControlledZone(ComfortControlledZoneNum)%NumControlTypes = NINT((NumAlphas - 5.0d0)/2.0d0)
        ALLOCATE(ComfortControlledZone(ComfortControlledZoneNum)%ControlType(ComfortControlledZone( &
                                       ComfortControlledZoneNum)%NumControlTypes))
        ALLOCATE(ComfortControlledZone(ComfortControlledZoneNum)%ControlTypeName(ComfortControlledZone( &
                                       ComfortControlledZoneNum)%NumControlTypes))
        ALLOCATE(ComfortControlledZone(ComfortControlledZoneNum)%ControlTypeSchIndx(ComfortControlledZone(  &
                                       ComfortControlledZoneNum)%NumControlTypes))

        DO ControlTypeNum = 1, ComfortControlledZone(ComfortControlledZoneNum)%NumControlTypes
          ComfortControlledZone(ComfortControlledZoneNum)%ControlType(ControlTypeNum) = cAlphaArgs(NINT(2.0d0*ControlTypeNum-1+5))
          ComfortControlledZone(ComfortControlledZoneNum)%ControlTypeName(ControlTypeNum) = cAlphaArgs(NINT(2.0d0*ControlTypeNum+5))
          IF (ComfortControlledZone(ComfortControlledZoneNum)%ControlType(ControlTypeNum) /= ' ') THEN
            CTIndex=FindItem(ComfortControlledZone(ComfortControlledZoneNum)%ControlType(ControlTypeNum), &
                                   ValidComfortControlTypes,12)
            IF (CTIndex == 0) THEN
              CALL ShowSevereError(trim(cCurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid '//  &
                 trim(cAlphaFieldNames(NINT(2.0d0*ControlTypeNum-1+5)))//'="'//  &
                    trim(cAlphaArgs(NINT(2.0d0*ControlTypeNum-1+5)))//'"')
              ErrorsFound = .TRUE.
            END IF
            IF (CTIndex > 4) THEN ! For Fanger control only for the time being
              CALL ShowSevereError(trim(cCurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid '//  &
                 trim(cAlphaFieldNames(NINT(2.0d0*ControlTypeNum-1+5)))//'="'//  &
                    trim(cAlphaArgs(NINT(2.0d0*ControlTypeNum-1+5)))//'"')
              CALL ShowContinueError('..Fanger is the only valid model.')
              ErrorsFound = .TRUE.
            END IF
          ELSE
            CALL ShowSevereError(trim(cCurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid '//  &
                 trim(cAlphaFieldNames(NINT(2.0d0*ControlTypeNum-1+5)))//'="<blank>"')
            ErrorsFound = .TRUE.
          ENDIF
          ComfortControlledZone(ComfortControlledZoneNum)%ControlTypeSchIndx(ControlTypeNum) = 0
        END DO
      END DO
    END DO ! NumComfortTStatStatements
  ENDIF
  ! End of Thermal comfort control reading and checking

  cCurrentModuleObject=ValidComfortControlTypes(SglHeatSetPointFanger)
  NumSingleFangerHeatingControls = GetNumObjectsFound(cCurrentModuleObject)

  IF (NumSingleFangerHeatingControls .GT. 0) ALLOCATE(SetPointSingleHeatingFanger(NumSingleFangerHeatingControls))

  DO SingleFangerHeatingControlNum = 1, NumSingleFangerHeatingControls
    CALL GetObjectItem(cCurrentModuleObject,SingleFangerHeatingControlNum,  &
                       cAlphaArgs,NumAlphas,rNumericArgs,NumNums,IOSTAT, &
                       NumBlank=lNumericFieldBlanks,AlphaBlank=lAlphaFieldBlanks, &
                       AlphaFieldNames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
    IsNotOK = .FALSE.
    IsBlank = .FALSE.
    CALL VerifyName(cAlphaArgs(1),SetPointSingleHeatingFanger%Name,SingleFangerHeatingControlNum-1,IsNotOK,IsBlank, &
                    trim(cCurrentModuleObject)//' Name')
    IF (IsNotOK) THEN
      ErrorsFound = .TRUE.
      IF (IsBlank) cAlphaArgs(1) = 'xxxxx'
    END IF
    SetPointSingleHeatingFanger(SingleFangerHeatingControlNum)%Name = cAlphaArgs(1)
    SetPointSingleHeatingFanger(SingleFangerHeatingControlNum)%PMVSchedName = cAlphaArgs(2)
    SetPointSingleHeatingFanger(SingleFangerHeatingControlNum)%PMVSchedIndex = GetScheduleIndex(cAlphaArgs(2))
    IF (SetPointSingleHeatingFanger(SingleFangerHeatingControlNum)%PMVSchedIndex == 0) THEN
      CALL ShowSevereError(trim(cCurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid '//  &
         trim(cAlphaFieldNames(2))//'="'//trim(cAlphaArgs(2))//'" not found.')
      ErrorsFound = .TRUE.
    ELSE
      ValidScheduleControlType=CheckScheduleValueMinMax( &
         SetPointSingleHeatingFanger(SingleFangerHeatingControlNum)%PMVSchedIndex, '>=',-3.0d0, '<=',3.0d0)
      IF (.not. ValidScheduleControlType) THEN
        CALL ShowSevereError(trim(cCurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid PMV values '//  &
           trim(cAlphaFieldNames(2))//'="'//trim(cAlphaArgs(2))//'" entered.')
        CALL ShowContinueError('..Values outside of range [-3,+3].')
        ErrorsFound=.TRUE.
      ENDIF
    END IF
  END DO ! SingleFangerHeatingControlNum

  cCurrentModuleObject=ValidComfortControlTypes(SglCoolSetPointFanger)
  NumSingleFangerCoolingControls = GetNumObjectsFound(cCurrentModuleObject)

  IF (NumSingleFangerCoolingControls .GT. 0) ALLOCATE(SetPointSingleCoolingFanger(NumSingleFangerCoolingControls))

  DO SingleFangerCoolingControlNum = 1, NumSingleFangerCoolingControls
    CALL GetObjectItem(cCurrentModuleObject,SingleFangerCoolingControlNum,  &
                       cAlphaArgs,NumAlphas,rNumericArgs,NumNums,IOSTAT, &
                       NumBlank=lNumericFieldBlanks,AlphaBlank=lAlphaFieldBlanks, &
                       AlphaFieldNames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
    IsNotOK = .FALSE.
    IsBlank = .FALSE.
    CALL VerifyName(cAlphaArgs(1),SetPointSingleCoolingFanger%Name,SingleFangerCoolingControlNum-1,IsNotOK,IsBlank, &
                    trim(cCurrentModuleObject)//' Name')
    IF (IsNotOK) THEN
      ErrorsFound = .TRUE.
      IF (IsBlank) cAlphaArgs(1) = 'xxxxx'
    END IF
    SetPointSingleCoolingFanger(SingleFangerCoolingControlNum)%Name = cAlphaArgs(1)
    SetPointSingleCoolingFanger(SingleFangerCoolingControlNum)%PMVSchedName = cAlphaArgs(2)
    SetPointSingleCoolingFanger(SingleFangerCoolingControlNum)%PMVSchedIndex = GetScheduleIndex(cAlphaArgs(2))
    IF (SetPointSingleCoolingFanger(SingleFangerCoolingControlNum)%PMVSchedIndex == 0) THEN
      CALL ShowSevereError(trim(cCurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid '//  &
         trim(cAlphaFieldNames(2))//'="'//trim(cAlphaArgs(2))//'" not found.')
      ErrorsFound = .TRUE.
    ELSE
      ValidScheduleControlType=CheckScheduleValueMinMax( &
         SetPointSingleCoolingFanger(SingleFangerCoolingControlNum)%PMVSchedIndex, '>=',-3.0d0, '<=',3.0d0)
      IF (.not. ValidScheduleControlType) THEN
        CALL ShowSevereError(trim(cCurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid PMV values '//  &
           trim(cAlphaFieldNames(2))//'="'//trim(cAlphaArgs(2))//'" entered.')
        CALL ShowContinueError('..Values outside of range [-3,+3].')
        ErrorsFound=.TRUE.
      ENDIF
    END IF

  END DO ! SingleFangerCoolingControlNum

  cCurrentModuleObject=ValidComfortControlTypes(SglHCSetPointFanger)
  NumSingleFangerHeatCoolControls = GetNumObjectsFound(cCurrentModuleObject)

  IF (NumSingleFangerHeatCoolControls .GT. 0) ALLOCATE(SetPointSingleHeatCoolFanger(NumSingleFangerHeatCoolControls))

  DO SingleFangerHeatCoolControlNum = 1, NumSingleFangerHeatCoolControls
    CALL GetObjectItem(cCurrentModuleObject,SingleFangerHeatCoolControlNum,  &
                       cAlphaArgs,NumAlphas,rNumericArgs,NumNums,IOSTAT, &
                       NumBlank=lNumericFieldBlanks,AlphaBlank=lAlphaFieldBlanks, &
                       AlphaFieldNames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
    IsNotOK = .FALSE.
    IsBlank = .FALSE.
    CALL VerifyName(cAlphaArgs(1),SetPointSingleCoolingFanger%Name,SingleFangerCoolingControlNum-1,IsNotOK,IsBlank, &
                    trim(cCurrentModuleObject)//' Name')
    IF (IsNotOK) THEN
      ErrorsFound = .TRUE.
      IF (IsBlank) cAlphaArgs(1) = 'xxxxx'
    END IF

    SetPointSingleHeatCoolFanger(SingleFangerHeatCoolControlNum)%Name = cAlphaArgs(1)
    SetPointSingleHeatCoolFanger(SingleFangerHeatCoolControlNum)%PMVSchedName = cAlphaArgs(2)
    SetPointSingleHeatCoolFanger(SingleFangerHeatCoolControlNum)%PMVSchedIndex = GetScheduleIndex(cAlphaArgs(2))
    IF (SetPointSingleHeatCoolFanger(SingleFangerHeatCoolControlNum)%PMVSchedIndex == 0) THEN
      CALL ShowSevereError(trim(cCurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid '//  &
         trim(cAlphaFieldNames(2))//'="'//trim(cAlphaArgs(2))//'" not found.')
      ErrorsFound = .TRUE.
    ELSE
      ValidScheduleControlType=CheckScheduleValueMinMax( &
         SetPointSingleHeatCoolFanger(SingleFangerHeatCoolControlNum)%PMVSchedIndex, '>=',-3.0d0, '<=',3.0d0)
      IF (.not. ValidScheduleControlType) THEN
        CALL ShowSevereError(trim(cCurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid PMV values '//  &
           trim(cAlphaFieldNames(2))//'="'//trim(cAlphaArgs(2))//'" entered.')
        CALL ShowContinueError('..Values outside of range [-3,+3].')
        ErrorsFound=.TRUE.
      ENDIF
    END IF

  END DO ! SingleFangerHeatCoolControlNum

  cCurrentModuleObject=ValidComfortControlTypes(DualSetPointFanger)
  NumDualFangerHeatCoolControls = GetNumObjectsFound(cCurrentModuleObject)

  IF (NumDualFangerHeatCoolControls .GT. 0) ALLOCATE(SetPointDualHeatCoolFanger(NumDualFangerHeatCoolControls))

  DO DualFangerHeatCoolControlNum = 1, NumDualFangerHeatCoolControls
    CALL GetObjectItem(cCurrentModuleObject,DualFangerHeatCoolControlNum,  &
                       cAlphaArgs,NumAlphas,rNumericArgs,NumNums,IOSTAT, &
                       NumBlank=lNumericFieldBlanks,AlphaBlank=lAlphaFieldBlanks, &
                       AlphaFieldNames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
    IsNotOK = .FALSE.
    IsBlank = .FALSE.
    CALL VerifyName(cAlphaArgs(1),SetPointDualHeatCoolFanger%Name,DualFangerHeatCoolControlNum-1,IsNotOK,IsBlank, &
                    trim(cCurrentModuleObject)//' Name')
    IF (IsNotOK) THEN
      ErrorsFound = .TRUE.
      IF (IsBlank) cAlphaArgs(1) = 'xxxxx'
    END IF
    SetPointDualHeatCoolFanger(DualFangerHeatCoolControlNum)%Name = cAlphaArgs(1)
    SetPointDualHeatCoolFanger(DualFangerHeatCoolControlNum)%HeatPMVSetptSchedName = cAlphaArgs(2)
    SetPointDualHeatCoolFanger(DualFangerHeatCoolControlNum)%HeatPMVSchedIndex = GetScheduleIndex(cAlphaArgs(2))
    IF (SetPointDualHeatCoolFanger(DualFangerHeatCoolControlNum)%HeatPMVSchedIndex == 0) THEN
      CALL ShowSevereError(trim(cCurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid '//  &
         trim(cAlphaFieldNames(2))//'="'//trim(cAlphaArgs(2))//'" not found.')
      ErrorsFound = .TRUE.
    END IF
    SetPointDualHeatCoolFanger(DualFangerHeatCoolControlNum)%CoolPMVSetptSchedName = cAlphaArgs(3)
    SetPointDualHeatCoolFanger(DualFangerHeatCoolControlNum)%CoolPMVSchedIndex = GetScheduleIndex(cAlphaArgs(3))
    IF (SetPointDualHeatCoolFanger(DualFangerHeatCoolControlNum)%CoolPMVSchedIndex == 0) THEN
      CALL ShowSevereError(trim(cCurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid '//  &
         trim(cAlphaFieldNames(3))//'="'//trim(cAlphaArgs(3))//'" not found.')
      ErrorsFound = .TRUE.
    ELSE
      ValidScheduleControlType=CheckScheduleValueMinMax( &
         SetPointDualHeatCoolFanger(DualFangerHeatCoolControlNum)%HeatPMVSchedIndex, '>=',-3.0d0, '<=',3.0d0)
      IF (.not. ValidScheduleControlType) THEN
        CALL ShowSevereError(trim(cCurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid PMV values '//  &
           trim(cAlphaFieldNames(2))//'="'//trim(cAlphaArgs(2))//'" entered.')
        CALL ShowContinueError('..Values outside of range [-3,+3].')
        ErrorsFound=.TRUE.
      ENDIF
      ValidScheduleControlType=CheckScheduleValueMinMax( &
         SetPointDualHeatCoolFanger(DualFangerHeatCoolControlNum)%CoolPMVSchedIndex, '>=',-3.0d0, '<=',3.0d0)
      IF (.not. ValidScheduleControlType) THEN
        CALL ShowSevereError(trim(cCurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid PMV values '//  &
           trim(cAlphaFieldNames(3))//'="'//trim(cAlphaArgs(3))//'" entered.')
        CALL ShowContinueError('..Values outside of range [-3,+3].')
        ErrorsFound=.TRUE.
      ENDIF
    END IF

  END DO ! DualFangerHeatCoolControlNum

 ! Finish filling in Schedule pointing indexes for Thermal Comfort Control
  DO ComfortControlledZoneNum = 1, NumComfortControlledZones
    ComfortIndex = FindItem(trim(ValidComfortControlTypes(SglHeatSetPointFanger)),  &
                                  ComfortControlledZone(ComfortControlledZoneNum)%ControlType,   &
                                  ComfortControlledZone(ComfortControlledZoneNum)%NumControlTypes)
    ComfortControlledZone(ComfortControlledZoneNum)%SchIndx_SglHeatSetPointFanger = ComfortIndex
    IF (ComfortIndex > 0) THEN
      ComfortControlledZone(ComfortControlledZoneNum)%ControlTypeSchIndx(ComfortIndex) =   &
            FindItem(ComfortControlledZone(ComfortControlledZoneNum)%ControlTypeName(ComfortIndex), &
                                        SetPointSingleHeatingFanger%Name, NumSingleFangerHeatingControls)
      TComfortControlTypes(ComfortControlledZoneNum)%MustHave(SglHeatSetPointFanger)=.true.
    ENDIF

    ComfortIndex = FindItem(trim(ValidComfortControlTypes(SglCoolSetPointFanger)),  &
                                  ComfortControlledZone(ComfortControlledZoneNum)%ControlType,   &
                                  ComfortControlledZone(ComfortControlledZoneNum)%NumControlTypes)
    ComfortControlledZone(ComfortControlledZoneNum)%SchIndx_SglCoolSetPointFanger = ComfortIndex
    IF (ComfortIndex > 0) THEN
      ComfortControlledZone(ComfortControlledZoneNum)%ControlTypeSchIndx(ComfortIndex) =   &
            FindItem(ComfortControlledZone(ComfortControlledZoneNum)%ControlTypeName(ComfortIndex), &
                                        SetPointSingleCoolingFanger%Name, NumSingleFangerCoolingControls)
      TComfortControlTypes(ComfortControlledZoneNum)%MustHave(SglCoolSetpointFanger)=.true.
    ENDIF

    ComfortIndex = FindItem(trim(ValidComfortControlTypes(SglHCSetPointFanger)),  &
                                  ComfortControlledZone(ComfortControlledZoneNum)%ControlType,   &
                                  ComfortControlledZone(ComfortControlledZoneNum)%NumControlTypes)
    ComfortControlledZone(ComfortControlledZoneNum)%SchIndx_SglHCSetPointFanger = ComfortIndex
    IF (ComfortIndex > 0) THEN
      ComfortControlledZone(ComfortControlledZoneNum)%ControlTypeSchIndx(ComfortIndex) =   &
            FindItem(ComfortControlledZone(ComfortControlledZoneNum)%ControlTypeName(ComfortIndex), &
                                        SetPointSingleHeatCoolFanger%Name, NumSingleFangerHeatCoolControls)
      TComfortControlTypes(ComfortControlledZoneNum)%MustHave(SglHCSetPointFanger)=.true.
    ENDIF

    ComfortIndex = FindItem(trim(ValidComfortControlTypes(DualSetPointFanger)),  &
                                  ComfortControlledZone(ComfortControlledZoneNum)%ControlType,   &
                                  ComfortControlledZone(ComfortControlledZoneNum)%NumControlTypes)
    ComfortControlledZone(ComfortControlledZoneNum)%SchIndx_DualSetPointFanger = ComfortIndex
    IF (ComfortIndex > 0) THEN
      ComfortControlledZone(ComfortControlledZoneNum)%ControlTypeSchIndx(ComfortIndex) =   &
            FindItem(ComfortControlledZone(ComfortControlledZoneNum)%ControlTypeName(ComfortIndex), &
                                        SetPointDualHeatCoolFanger%Name, NumDualFangerHeatCoolControls)
      TComfortControlTypes(ComfortControlledZoneNum)%MustHave(DualSetPointFanger)=.true.
    ENDIF
  ENDDO

  ! Now, Check the schedule values/indices for validity for Thermal Comfort Control

   DO ComfortControlledZoneNum = 1, NumComfortControlledZones

    ActualZoneNum = ComfortControlledZone(ComfortControlledZoneNum)%ActualZoneNum
    CTIndex = ComfortControlledZone(ComfortControlledZoneNum)%ComfortSchedIndex
    IF (CTIndex == 0) CYCLE   ! error will be caught elsewhere
    SchedMin=GetScheduleMinValue(CTIndex)
    SchedMax=GetScheduleMaxValue(CTIndex)

    IF (SchedMin == 0 .and. SchedMax == 0) THEN
      IF (FindNumberInList(CTIndex,CCmSchedMapToControlledZone,NumComfortControlledZones) == 0) THEN
        CALL ShowWarningError('Control Type Schedule='//TRIM(ComfortControlledZone(ComfortControlledZoneNum)%ControlTypeSchedName))
        CALL ShowContinueError('..specifies control type 0 for all entries.')
        CALL ShowContinueError('All zones using this Control Type Schedule have no thermal comfort control.')
      ENDIF
      CCmSchedMapToControlledZone(ComfortControlledZoneNum)=CTIndex
    END IF

    DO ControlTypeNum=SchedMin,SchedMax

      SELECT CASE (ControlTypeNum)

        CASE (0) ! Thermal comfort uncontrolled

        CASE (SglHeatSetpointFanger)

          ComfortIndex=ComfortControlledZone(ComfortControlledZoneNum)%SchIndx_SglHeatSetPointFanger
          TComfortControlTypes(ComfortControlledZoneNum)%DidHave(SglHeatSetPointFanger)=.true.
          IF (ComfortIndex /= 0) THEN
            SchedTypeIndex = ComfortControlledZone(ComfortControlledZoneNum)%ControlTypeSchIndx(ComfortIndex)
            IF (SchedTypeIndex == 0) THEN
              CALL ShowSevereError('GetZoneAirSetpoints: Could not find '//  &
                 trim(ValidComfortControlTypes(SglHeatSetPointFanger))//  &
                 ' Schedule='//TRIM(ComfortControlledZone(ComfortControlledZoneNum)%ControlTypeName(ComfortIndex)))
              ErrorsFound=.true.
            END IF
          ELSE   ! ComfortIndex = 0
            IF (CheckScheduleValue(CTIndex,SglHeatSetPointFanger)) THEN
              CALL ShowSevereError('Control Type Schedule='// &
                   TRIM(ComfortControlledZone(ComfortControlledZoneNum)%ControlTypeSchedName))
              CALL ShowContinueError('..specifies thermal control type 1 ('//  &
                 trim(ValidComfortControlTypes(SglHeatSetPointFanger))//  &
                 ') as the control type. Not valid for this zone.')
              CALL ShowContinueError('..reference '//trim(cZControlTypes(iZC_TCTStat))//'='//  &
                 TRIM(ComfortControlledZone(ComfortControlledZoneNum)%Name))
              CALL ShowContinueError('..reference ZONE='//TRIM(ComfortControlledZone(ComfortControlledZoneNum)%ZoneName))
              ErrorsFound=.true.
            ENDIF
          ENDIF

        CASE (SglCoolSetPointFanger)

          ComfortIndex = ComfortControlledZone(ComfortControlledZoneNum)%SchIndx_SglCoolSetPointFanger
          TComfortControlTypes(ComfortControlledZoneNum)%DidHave(SglCoolSetPointFanger)=.true.
          IF (ComfortIndex /= 0) THEN
            SchedTypeIndex = ComfortControlledZone(ComfortControlledZoneNum)%ControlTypeSchIndx(ComfortIndex)
            IF (SchedTypeIndex == 0) THEN
              CALL ShowSevereError('GetZoneAirSetpoints: Could not find '//  &
                 trim(ValidComfortControlTypes(SglCoolSetPointFanger))//  &
                 ' Schedule='//TRIM(ComfortControlledZone(ComfortControlledZoneNum)%ControlTypeName(ComfortIndex)))
              ErrorsFound=.true.
            END IF
          ELSE   ! ComfortIndex = 0
            IF (CheckScheduleValue(CTIndex,SglCoolSetPointFanger)) THEN
              CALL ShowSevereError('Control Type Schedule='// &
                                   TRIM(ComfortControlledZone(ComfortControlledZoneNum)%ControlTypeSchedName))
              CALL ShowContinueError('..specifies thermal control type 2 ('//  &
                 trim(ValidComfortControlTypes(SglCoolSetPointFanger))//  &
                 ') as the control type. Not valid for this zone.')
              CALL ShowContinueError('..reference '//trim(cZControlTypes(iZC_TCTStat))//'='//  &
                 TRIM(ComfortControlledZone(ComfortControlledZoneNum)%Name))
              CALL ShowContinueError('..reference ZONE='//TRIM(ComfortControlledZone(ComfortControlledZoneNum)%ZoneName))
              ErrorsFound=.true.
            ENDIF
          ENDIF

        CASE (SglHCSetPointFanger)

          ComfortIndex = ComfortControlledZone(ComfortControlledZoneNum)%SchIndx_SglHCSetPointFanger
          TComfortControlTypes(ComfortControlledZoneNum)%DidHave(SglHCSetPointFanger)=.true.
          IF (ComfortIndex /= 0) THEN
            SchedTypeIndex = ComfortControlledZone(ComfortControlledZoneNum)%ControlTypeSchIndx(ComfortIndex)
            IF (SchedTypeIndex == 0) THEN
              CALL ShowSevereError('GetZoneAirSetpoints: Could not find '//trim(ValidComfortControlTypes(SglHCSetPointFanger))//  &
                      ' Schedule='//TRIM(ComfortControlledZone(ComfortControlledZoneNum)%ControlTypeName(ComfortIndex)))
              ErrorsFound=.true.
            END IF
          ELSE   ! ComfortIndex = 0
            IF (CheckScheduleValue(CTIndex,SglHCSetPointFanger)) THEN
              CALL ShowSevereError('Schedule='//TRIM(ComfortControlledZone(ComfortControlledZoneNum)%ControlTypeSchedName))
              CALL ShowContinueError('..specifies thermal control type 3 ('//  &
                 trim(ValidComfortControlTypes(SglHCSetPointFanger))//  &
                 ') as the control type. Not valid for this zone.')
              CALL ShowContinueError('..reference '//trim(cZControlTypes(iZC_TCTStat))//'='//  &
                 TRIM(ComfortControlledZone(ComfortControlledZoneNum)%Name))
              CALL ShowContinueError('..reference ZONE='//TRIM(ComfortControlledZone(ComfortControlledZoneNum)%ZoneName))
              ErrorsFound=.true.
            ENDIF
          ENDIF

        CASE (DualSetPointFanger)

          ComfortIndex = ComfortControlledZone(ComfortControlledZoneNum)%SchIndx_DualSetPointFanger
          TComfortControlTypes(ComfortControlledZoneNum)%DidHave(DualSetPointFanger)=.true.
          IF (ComfortIndex /= 0) THEN
            SchedTypeIndex = ComfortControlledZone(ComfortControlledZoneNum)%ControlTypeSchIndx(ComfortIndex)
            IF (SchedTypeIndex == 0) THEN
              CALL ShowSevereError('GetZoneAirSetpoints: Could not find '//trim(ValidComfortControlTypes(DualSetPointFanger))//  &
                      ' Schedule='//TRIM(ComfortControlledZone(ComfortControlledZoneNum)%ControlTypeName(ComfortIndex)))
              ErrorsFound=.true.
            END IF
          ELSE   ! ComfortIndex = 0
            IF (CheckScheduleValue(CTIndex,DualSetPointFanger)) THEN
              CALL ShowSevereError('Schedule='//TRIM(ComfortControlledZone(ComfortControlledZoneNum)%ControlTypeSchedName))
              CALL ShowContinueError('..specifies thermal control type 4 ('//trim(ValidComfortControlTypes(DualSetPointFanger))//  &
                 ') as the control type. Not valid for this zone.')
              CALL ShowContinueError('..reference '//trim(cZControlTypes(iZC_TCTStat))//'='//  &
                 TRIM(ComfortControlledZone(ComfortControlledZoneNum)%Name))
              CALL ShowContinueError('..reference ZONE='//TRIM(ComfortControlledZone(ComfortControlledZoneNum)%ZoneName))
              ErrorsFound=.true.
            ENDIF
          ENDIF
        ! CASE PIERCE
        ! CASE KSU

        CASE DEFAULT
          CALL ShowSevereError('GetZoneAirSetpoints: Illegal control type for Zone='//TRIM(Zone(ActualZoneNum)%Name)//  &
                               ', Found value='//TRIM(TrimSigDigits(ControlTypeNum))//', in Schedule='//        &
                               TRIM(ComfortControlledZone(ComfortControlledZoneNum)%ControlTypeSchedName))
          CALL ShowContinueError('..valid range values are [0,4].')
          ErrorsFound=.true.

      END SELECT
    END DO

  END DO

  DO ComfortControlledZoneNum = 1, NumComfortControlledZones

    ActualZoneNum = ComfortControlledZone(ComfortControlledZoneNum)%ActualZoneNum
    CTIndex = ComfortControlledZone(ComfortControlledZoneNum)%ComfortSchedIndex
    IF (CTIndex == 0) CYCLE  ! error caught elsewhere -- would just be confusing here

    DO ControlTypeNum=1,12
      IF (TComfortControlTypes(ComfortControlledZoneNum)%MustHave(ControlTypeNum) .and.  &
          TComfortControlTypes(ComfortControlledZoneNum)%DidHave(ControlTypeNum)) CYCLE

      SELECT CASE (ControlTypeNum)

        CASE (SglHeatSetpointFanger)
          IF (.not. TComfortControlTypes(ComfortControlledZoneNum)%MustHave(ControlTypeNum)) CYCLE
          CALL ShowWarningError('Schedule='//TRIM(ComfortControlledZone(ComfortControlledZoneNum)%ControlTypeSchedName))
          CALL ShowContinueError('...should include control type 1 ('//trim(ValidComfortControlTypes(SglHeatSetpointFanger))//  &
                      ') but does not.')
          CALL ShowContinueError('..reference '//trim(cZControlTypes(iZC_TCTStat))//'='//  &
                                 TRIM(ComfortControlledZone(ComfortControlledZoneNum)%Name))
          CALL ShowContinueError('..reference ZONE='//TRIM(ComfortControlledZone(ComfortControlledZoneNum)%ZoneName))

        CASE (SglCoolSetpointFanger)
          IF (.not. TComfortControlTypes(ComfortControlledZoneNum)%MustHave(ControlTypeNum)) CYCLE
          CALL ShowWarningError('Schedule='//TRIM(ComfortControlledZone(ComfortControlledZoneNum)%ControlTypeSchedName))
          CALL ShowContinueError('...should include control type 2 ('//trim(ValidComfortControlTypes(SglCoolSetpointFanger))//  &
                      ') but does not.')
          CALL ShowContinueError('..reference '//trim(cZControlTypes(iZC_TCTStat))//'='//  &
                                 TRIM(ComfortControlledZone(ComfortControlledZoneNum)%Name))
          CALL ShowContinueError('..reference ZONE='//TRIM(ComfortControlledZone(ComfortControlledZoneNum)%ZoneName))

        CASE (SglHCSetpointFanger)
          IF (.not. TComfortControlTypes(ComfortControlledZoneNum)%MustHave(ControlTypeNum)) CYCLE
          CALL ShowWarningError('Schedule='//TRIM(ComfortControlledZone(ComfortControlledZoneNum)%ControlTypeSchedName))
          CALL ShowContinueError('...should include control type 3 ('//trim(ValidComfortControlTypes(SglHCSetpointFanger))//  &
                      ') but does not.')
          CALL ShowContinueError('..reference '//trim(cZControlTypes(iZC_TCTStat))//'='//  &
                                 TRIM(ComfortControlledZone(ComfortControlledZoneNum)%Name))
          CALL ShowContinueError('..reference ZONE='//TRIM(ComfortControlledZone(ComfortControlledZoneNum)%ZoneName))

        CASE (DualSetPointFanger)
          IF (.not. TComfortControlTypes(ComfortControlledZoneNum)%MustHave(ControlTypeNum)) CYCLE
          CALL ShowWarningError('Schedule='//TRIM(ComfortControlledZone(ComfortControlledZoneNum)%ControlTypeSchedName))
          CALL ShowContinueError('...should include control type 4 ('//trim(ValidComfortControlTypes(DualSetPointFanger))//  &
                      ') but does not.')
          CALL ShowContinueError('..reference '//trim(cZControlTypes(iZC_TCTStat))//'='//  &
                                 TRIM(ComfortControlledZone(ComfortControlledZoneNum)%Name))
          CALL ShowContinueError('..reference ZONE='//TRIM(ComfortControlledZone(ComfortControlledZoneNum)%ZoneName))

        ! CASE PIERCE
        ! CASE KSU

        CASE DEFAULT
      END SELECT
    ENDDO
  ENDDO

  IF (ALLOCATED(TComfortControlTypes)) DEALLOCATE(TComfortControlTypes)

  ! Get the Zone Air Capacitance Multiplier for use in the Predictor-Corrrector Procedure
  cCurrentModuleObject='ZoneCapacitanceMultiplier:ResearchSpecial'
  NumNums=GetNumObjectsFound(cCurrentModuleObject)
  IF (NumNums == 0) THEN
    ZoneVolCapMultpSens  = 1.d0
    ZoneVolCapMultpMoist = 1.d0
    ZoneVolCapMultpCO2   = 1.d0
    ZoneVolCapMultpGenContam = 1.0d0
  ELSE
    CALL GetObjectItem(cCurrentModuleObject,1,cAlphaArgs,NumAlphas,rNumericArgs,NumNums,IOStat, &
                       NumBlank=lNumericFieldBlanks,AlphaBlank=lAlphaFieldBlanks, &
                       AlphaFieldNames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
    ZoneVolCapMultpSens  = rNumericArgs(1)
    ZoneVolCapMultpMoist = rNumericArgs(2)
    ZoneVolCapMultpCO2   = rNumericArgs(3)
    ZoneVolCapMultpGenContam = rNumericArgs(4)
  END IF

  WRITE(OutputFileInits, 700)
  WRITE(OutputFileInits,701) ZoneVolCapMultpSens, ZoneVolCapMultpMoist, ZoneVolCapMultpCO2, ZoneVolCapMultpGenContam
700 FORMAT ('! <Zone Volume Capacitance Multiplier>, Sensible Heat Capacity Multiplier, Moisture Capacity Multiplier, ',  &
                   'Carbon Dioxide Capacity Multiplier, Generic Contaminant Capacity Multiplier')
701 FORMAT( 'Zone Volume Capacitance Multiplier,' , F8.3,' ,', F8.3,',', F8.3,',', F8.3)

  cCurrentModuleObject=cZControlTypes(iZC_OTTStat)
  NumOpTempControlledZones =  GetNumObjectsFound(cCurrentModuleObject)

  IF (NumOpTempControlledZones > 0) then
    AnyOpTempControl = .TRUE.

    DO OpTempContrlNum = 1, NumOpTempControlledZones
       CALL GetObjectItem(cCurrentModuleObject,OpTempContrlNum,  &
                       cAlphaArgs,NumAlphas,rNumericArgs,NumNums,IOSTAT, &
                       NumBlank=lNumericFieldBlanks,AlphaBlank=lAlphaFieldBlanks, &
                       AlphaFieldNames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
       ! find matching name of  ZONECONTROL:THERMOSTAT object
       found = FindItem(cAlphaArgs(1),TStatObjects%Name,NumTStatStatements)
       IF (found == 0) THEN
         ! It might be in the TempControlledZones
         found=FindItem(cAlphaArgs(1),TempControlledZone%Name,NumTempControlledZones)
         IF (found == 0) THEN ! throw error
          CALL ShowSevereError(trim(cCurrentModuleObject)//'='//trim(cAlphaArgs(1))//' invalid '//  &
            trim(cZControlTypes(iZC_TStat))//' reference not found.')
          ErrorsFound = .TRUE.
         ELSE
           TempControlledZoneNum = found
           TempControlledZone(TempControlledZoneNum)%OperativeTempControl = .TRUE.
           IF (SameString(cAlphaArgs(2), 'Scheduled')) THEN
             TempControlledZone(TempControlledZoneNum)%OpTempCntrlModeScheduled=.TRUE.
           ENDIF
           IF ( ( .NOT. (SameString(cAlphaArgs(2), 'Scheduled')))  .AND. ( .NOT. (SameString(cAlphaArgs(2), 'Constant'))) ) THEN
              CALL ShowSevereError(trim(cCurrentModuleObject)//'='//trim(cAlphaArgs(1))//' invalid '//  &
                trim(cAlphaFieldNames(2))//'="'//trim(cAlphaArgs(2))//'".')
             ErrorsFound = .TRUE.
           ENDIF

           TempControlledZone(TempControlledZoneNum)%FixedRadiativeFraction    = rNumericArgs(1)
           TempControlledZone(TempControlledZoneNum)%OpTempRadiativeFractionSched = GetScheduleIndex(cAlphaArgs(3))
           If ( (TempControlledZone(TempControlledZoneNum)%OpTempRadiativeFractionSched == 0) &
               .AND. (TempControlledZone(TempControlledZoneNum)%OpTempCntrlModeScheduled) ) THEN !throw error
             CALL ShowSevereError(trim(cCurrentModuleObject)//'='//trim(cAlphaArgs(1))//' invalid '//  &
                trim(cAlphaFieldNames(3))//'="'//trim(cAlphaArgs(3))//'" not found.')
             ErrorsFound = .TRUE.
           ENDIF

           !check validity of fixed radiative fraction
           IF ( (TempControlledZone(TempControlledZoneNum)%FixedRadiativeFraction < 0.0d0)  &
                .AND. (.NOT. (TempControlledZone(TempControlledZoneNum)%OpTempCntrlModeScheduled)) ) THEN
             CALL ShowSevereError(trim(cCurrentModuleObject)//'='//trim(cAlphaArgs(1))//' invalid '//  &
                 trim(cNumericFieldNames(1))//'=['//trim(TrimSigDigits(rNumericArgs(1),2))//'" cannot be negative.')
             ErrorsFound = .TRUE.
           ENDIF
           IF ( (TempControlledZone(TempControlledZoneNum)%FixedRadiativeFraction >= 0.9d0) &
                .AND. (.NOT. (TempControlledZone(TempControlledZoneNum)%OpTempCntrlModeScheduled)) ) THEN
             CALL ShowSevereError(trim(cCurrentModuleObject)//'='//trim(cAlphaArgs(1))//' invalid '//  &
                 trim(cNumericFieldNames(1))//'=['//trim(TrimSigDigits(rNumericArgs(1),2))//'" cannot >= .9.')
             ErrorsFound = .TRUE.
           ENDIF

           ! check schedule min max.
           IF (TempControlledZone(TempControlledZoneNum)%OpTempCntrlModeScheduled) THEN
             ValidRadFractSched =   &
                CheckScheduleValueMinMax(TempControlledZone(TempControlledZoneNum)%OpTempRadiativeFractionSched,  &
                                                '>=',0.0,'<',0.9)
             IF (.not. ValidRadFractSched) THEN
               CALL ShowSevereError(trim(cCurrentModuleObject)//'='//trim(cAlphaArgs(1))//' invalid values '//  &
                  trim(cAlphaFieldNames(3))//'=['//trim(cAlphaArgs(3))//'".')
               CALL ShowContinueError('..Values outside of range [0.0,0.9).')
               ErrorsFound=.TRUE.
             ENDIF
           ENDIF

           ! CurrentModuleObject='ZoneControl:Thermostat:OperativeTemperature'
           CALL SetupOutputVariable('Zone Thermostat Operative Temperature [C]',  &
                                     ZnAirRpt(TempControlledZone(TempControlledZoneNum)%ActualZoneNum)%ThermOperativeTemp,  &
                                     'Zone','Average',Zone(TempControlledZone(TempControlledZoneNum)%ActualZoneNum)%Name)
         ENDIF
       ELSE
         DO Item=1,TStatObjects(found)%NumOfZones
           TempControlledZoneNum = TStatObjects(found)%TempControlledZoneStartPtr+Item-1
           IF (NumTempControlledZones == 0) CYCLE
           TempControlledZone(TempControlledZoneNum)%OperativeTempControl = .TRUE.
           IF (SameString(cAlphaArgs(2), 'Scheduled')) THEN
             TempControlledZone(TempControlledZoneNum)%OpTempCntrlModeScheduled=.TRUE.
           ENDIF
           IF (Item == 1) THEN
             IF ( ( .NOT. (SameString(cAlphaArgs(2), 'Scheduled')))  .AND. ( .NOT. (SameString(cAlphaArgs(2), 'Constant'))) ) THEN
                CALL ShowSevereError(trim(cCurrentModuleObject)//'='//trim(cAlphaArgs(1))//' invalid '//  &
                  trim(cAlphaFieldNames(2))//'="'//trim(cAlphaArgs(2))//'".')
               ErrorsFound = .TRUE.
             ENDIF
           ENDIF

           TempControlledZone(TempControlledZoneNum)%FixedRadiativeFraction    = rNumericArgs(1)
           TempControlledZone(TempControlledZoneNum)%OpTempRadiativeFractionSched = GetScheduleIndex(cAlphaArgs(3))
           IF (Item == 1) THEN
             If ( (TempControlledZone(TempControlledZoneNum)%OpTempRadiativeFractionSched == 0) &
                 .AND. (TempControlledZone(TempControlledZoneNum)%OpTempCntrlModeScheduled) ) THEN !throw error
                CALL ShowSevereError(trim(cCurrentModuleObject)//'='//trim(cAlphaArgs(1))//' invalid '//  &
                  trim(cAlphaFieldNames(3))//'="'//trim(cAlphaArgs(3))//'" not found.')
               ErrorsFound = .TRUE.
             ENDIF
           ENDIF

           !check validity of fixed radiative fraction
           IF (Item == 1) THEN
             IF ( (TempControlledZone(TempControlledZoneNum)%FixedRadiativeFraction < 0.0d0)  &
                  .AND. (.NOT. (TempControlledZone(TempControlledZoneNum)%OpTempCntrlModeScheduled)) ) THEN
                CALL ShowSevereError(trim(cCurrentModuleObject)//'='//trim(cAlphaArgs(1))//' invalid '//  &
                  trim(cNumericFieldNames(1))//'=['//trim(TrimSigDigits(rNumericArgs(1),2))//'" cannot be negative.')
               ErrorsFound = .TRUE.
             ENDIF
           ENDIF
           IF (Item == 1) THEN
             IF ( (TempControlledZone(TempControlledZoneNum)%FixedRadiativeFraction >= 0.9d0) &
                  .AND. (.NOT. (TempControlledZone(TempControlledZoneNum)%OpTempCntrlModeScheduled)) ) THEN
                CALL ShowSevereError(trim(cCurrentModuleObject)//'='//trim(cAlphaArgs(1))//' invalid '//  &
                  trim(cNumericFieldNames(1))//'=['//trim(TrimSigDigits(rNumericArgs(1),2))//'" cannot >= .9.')
               ErrorsFound = .TRUE.
             ENDIF
           ENDIF

           ! check schedule min max.
           IF (Item == 1) THEN
             IF (TempControlledZone(TempControlledZoneNum)%OpTempCntrlModeScheduled) THEN
               ValidRadFractSched =   &
                  CheckScheduleValueMinMax(TempControlledZone(TempControlledZoneNum)%OpTempRadiativeFractionSched,  &
                                                  '>=',0.0,'<',0.9)
               IF (.not. ValidRadFractSched) THEN
                  CALL ShowSevereError(trim(cCurrentModuleObject)//'='//trim(cAlphaArgs(1))//' invalid values '//  &
                    trim(cAlphaFieldNames(3))//'=['//trim(cAlphaArgs(3))//'".')
                  CALL ShowContinueError('..Values outside of range [0.0,0.9).')
                  ErrorsFound=.TRUE.
               ENDIF
             ENDIF
           ENDIF

           ! CurrentModuleObject='ZoneControl:Thermostat:OperativeTemperature'
           CALL SetupOutputVariable('Zone Thermostat Operative Temperature [C]',  &
                                     ZnAirRpt(TempControlledZone(TempControlledZoneNum)%ActualZoneNum)%ThermOperativeTemp,  &
                                     'Zone','Average',Zone(TempControlledZone(TempControlledZoneNum)%ActualZoneNum)%Name)
        ENDDO  ! TStat Objects Loop
      ENDIF  ! found thermostat referene
    ENDDO !loop over NumOpTempControlledZones
  ENDIF ! NumOpTempControlledZones > 0


  ! Overcool dehumidificaton GetInput starts here
  cCurrentModuleObject=cZControlTypes(iZC_TandHStat)
  NumTempAndHumidityControlledZones =  GetNumObjectsFound(cCurrentModuleObject)

  IF (NumTempAndHumidityControlledZones > 0) THEN
    AnyZoneTempAndHumidityControl = .TRUE.

    DO TempHumidityCntrlNum = 1, NumTempAndHumidityControlledZones
       CALL GetObjectItem(cCurrentModuleObject,TempHumidityCntrlNum,  &
                       cAlphaArgs,NumAlphas,rNumericArgs,NumNums,IOSTAT, &
                       NumBlank=lNumericFieldBlanks,AlphaBlank=lAlphaFieldBlanks, &
                       AlphaFieldNames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
       ! find matching name of  ZONECONTROL:THERMOSTAT object
       found = FindItem(cAlphaArgs(1),TStatObjects%Name,NumTStatStatements)
       IF (found == 0) THEN
         ! It might be in the TempControlledZones
         found=FindItem(cAlphaArgs(1),TempControlledZone%Name,NumTempControlledZones)
         IF (found == 0) THEN ! throw error
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1))//' invalid '//  &
            TRIM(cZControlTypes(iZC_TStat))//' reference not found.')
          ErrorsFound = .TRUE.
         ELSE
           TempControlledZoneNum = found
           TempControlledZone(TempControlledZoneNum)%DehumidifyingSched = cAlphaArgs(2)
           TempControlledZone(TempControlledZoneNum)%DehumidifyingSchedIndex = GetScheduleIndex(cAlphaArgs(2))
           IF (TempControlledZone(TempControlledZoneNum)%DehumidifyingSchedIndex == 0) THEN
               CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//' invalid '//  &
               TRIM(cAlphaFieldNames(2))//'="'//TRIM(cAlphaArgs(2))//'" not found.')
               ErrorsFound = .TRUE.
           END IF
           TempControlledZone(TempControlledZoneNum)%ZoneOvercoolControl =.TRUE.
           IF ( (SameString(cAlphaArgs(3), 'None')) ) THEN
             TempControlledZone(TempControlledZoneNum)%ZoneOvercoolControl =.FALSE.
           ENDIF
           IF (SameString(cAlphaArgs(4), 'Scheduled')) THEN
             TempControlledZone(TempControlledZoneNum)%OverCoolCntrlModeScheduled=.TRUE.
           ENDIF
           IF ( ( .NOT. (SameString(cAlphaArgs(4), 'Scheduled')))  .AND. ( .NOT. (SameString(cAlphaArgs(4), 'Constant'))) ) THEN
              CALL ShowSevereError(TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1))//' invalid '//  &
                trim(cAlphaFieldNames(4))//'="'//TRIM(cAlphaArgs(4))//'".')
             ErrorsFound = .TRUE.
           ENDIF

           TempControlledZone(TempControlledZoneNum)%ZoneOvercoolConstRange = rNumericArgs(1)
           TempControlledZone(TempControlledZoneNum)%ZoneOverCoolRangeSchedIndex = GetScheduleIndex(cAlphaArgs(4))
           IF ( (TempControlledZone(TempControlledZoneNum)%ZoneOverCoolRangeSchedIndex == 0) &
               .AND. (TempControlledZone(TempControlledZoneNum)%OverCoolCntrlModeScheduled) ) THEN !throw error
             CALL ShowSevereError(TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1))//' invalid '//  &
                trim(cAlphaFieldNames(5))//'="'//TRIM(cAlphaArgs(5))//'" not found.')
             ErrorsFound = .TRUE.
           ENDIF

           !check validity of zone Overcool constant range
           IF ( (TempControlledZone(TempControlledZoneNum)%ZoneOvercoolConstRange < 0.0d0)  &
                .AND. (.NOT. (TempControlledZone(TempControlledZoneNum)%OverCoolCntrlModeScheduled)) ) THEN
             CALL ShowSevereError(TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1))//' invalid '//  &
                 TRIM(cNumericFieldNames(1))//'=['//TRIM(TrimSigDigits(rNumericArgs(1),2))//'" cannot be negative.')
             ErrorsFound = .TRUE.
           ENDIF
           IF ( (TempControlledZone(TempControlledZoneNum)%ZoneOvercoolConstRange > 3.0d0) &
                .AND. (.NOT. (TempControlledZone(TempControlledZoneNum)%OvercoolCntrlModeScheduled)) ) THEN
             CALL ShowSevereError(TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1))//' invalid '//  &
                 TRIM(cNumericFieldNames(1))//'=['//TRIM(TrimSigDigits(rNumericArgs(1),2))//'" cannot be > 3.0')
             ErrorsFound = .TRUE.
           ENDIF

           ! check zone Overcool range schedule min/max values.
           IF (TempControlledZone(TempControlledZoneNum)%OvercoolCntrlModeScheduled) THEN
             ValidZoneOvercoolRangeSched =   &
                CheckScheduleValueMinMax(TempControlledZone(TempControlledZoneNum)%ZoneOvercoolRangeSchedIndex,  &
                                                '>=',0.0d0,'<=',3.0d0)
             IF (.not. ValidZoneOvercoolRangeSched) THEN
               CALL ShowSevereError(trim(cCurrentModuleObject)//'='//trim(cAlphaArgs(1))//' invalid values '//  &
                  trim(cAlphaFieldNames(5))//'=['//trim(cAlphaArgs(5))//'".')
               CALL ShowContinueError('..Values outside of range [0.0,3.0].')
               ErrorsFound=.TRUE.
             ENDIF
           ENDIF
           ! check Overcool Control Ratio limits
           TempControlledZone(TempControlledZoneNum)%ZoneOvercoolControlRatio = rNumericArgs(2)
           IF (TempControlledZone(TempControlledZoneNum)%ZoneOvercoolControlRatio < 0.0d0) THEN
             CALL ShowSevereError(TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(2))//' invalid '//  &
                 trim(cNumericFieldNames(2))//'=['//TRIM(TrimSigDigits(rNumericArgs(2),2))//'" cannot be negative.')
             ErrorsFound = .TRUE.
           ENDIF
         ENDIF
       ELSE
         DO Item=1,TStatObjects(found)%NumOfZones
           TempControlledZoneNum = TStatObjects(found)%TempControlledZoneStartPtr+Item-1
           TempControlledZone(TempControlledZoneNum)%DehumidifyingSched = cAlphaArgs(2)
           TempControlledZone(TempControlledZoneNum)%DehumidifyingSchedIndex = GetScheduleIndex(cAlphaArgs(2))
           IF (TempControlledZone(TempControlledZoneNum)%DehumidifyingSchedIndex == 0) THEN
               CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//' invalid '//  &
               TRIM(cAlphaFieldNames(2))//'="'//TRIM(cAlphaArgs(2))//'" not found.')
               ErrorsFound = .TRUE.
           ENDIF
           TempControlledZone(TempControlledZoneNum)%ZoneOvercoolControl =.TRUE.
           IF ( (SameString(cAlphaArgs(3), 'None'))) THEN
             TempControlledZone(TempControlledZoneNum)%ZoneOvercoolControl =.FALSE.
           ENDIF
           IF (SameString(cAlphaArgs(4), 'Scheduled')) THEN
             TempControlledZone(TempControlledZoneNum)%OvercoolCntrlModeScheduled =.FALSE.
           ENDIF
           IF (Item == 1) THEN
             IF ( ( .NOT. (SameString(cAlphaArgs(4), 'Scheduled')))  .AND. ( .NOT. (SameString(cAlphaArgs(4), 'Constant'))) ) THEN
                CALL ShowSevereError(TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1))//' invalid '//  &
                  TRIM(cAlphaFieldNames(4))//'="'//TRIM(cAlphaArgs(4))//'".')
               ErrorsFound = .TRUE.
             ENDIF
           ENDIF
           TempControlledZone(TempControlledZoneNum)%ZoneOvercoolConstRange = rNumericArgs(1)
           TempControlledZone(TempControlledZoneNum)%ZoneOvercoolRangeSchedIndex = GetScheduleIndex(cAlphaArgs(6))
           IF (Item == 1) THEN
             If ( (TempControlledZone(TempControlledZoneNum)%ZoneOvercoolRangeSchedIndex == 0) &
                 .AND. (TempControlledZone(TempControlledZoneNum)%OvercoolCntrlModeScheduled) ) THEN !throw error
                CALL ShowSevereError(TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1))//' invalid '//  &
                  TRIM(cAlphaFieldNames(5))//'="'//TRIM(cAlphaArgs(5))//'" not found.')
               ErrorsFound = .TRUE.
             ENDIF
           ENDIF
           !check validity of zone Overcool constant range
           IF (Item == 1) THEN
             IF ( (TempControlledZone(TempControlledZoneNum)%ZoneOvercoolConstRange < 0.0d0)  &
                  .AND. (.NOT. (TempControlledZone(TempControlledZoneNum)%OvercoolCntrlModeScheduled)) ) THEN
                CALL ShowSevereError(TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1))//' invalid '//  &
                  TRIM(cNumericFieldNames(1))//'=['//TRIM(TrimSigDigits(rNumericArgs(1),2))//'" cannot be negative.')
               ErrorsFound = .TRUE.
             ENDIF
           ENDIF
           IF (Item == 1) THEN
             IF ( (TempControlledZone(TempControlledZoneNum)%ZoneOvercoolConstRange > 3.0d0) &
                  .AND. (.NOT. (TempControlledZone(TempControlledZoneNum)%OvercoolCntrlModeScheduled)) ) THEN
                CALL ShowSevereError(TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1))//' invalid '//  &
                  TRIM(cNumericFieldNames(1))//'=['//TRIM(TrimSigDigits(rNumericArgs(1),2))//'" cannot > 3.0')
               ErrorsFound = .TRUE.
             ENDIF
           ENDIF
           ! check zone Overcool range schedule min/max values.
           IF (Item == 1) THEN
             IF (TempControlledZone(TempControlledZoneNum)%OvercoolCntrlModeScheduled) THEN
               ValidZoneOverCoolRangeSched =   &
                  CheckScheduleValueMinMax(TempControlledZone(TempControlledZoneNum)%ZoneOvercoolRangeSchedIndex,  &
                                                  '>=',0.0d0,'<=',3.0d0)
               IF (.not. ValidZoneOverCoolRangeSched) THEN
                  CALL ShowSevereError(TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1))//' invalid values '//  &
                    TRIM(cAlphaFieldNames(5))//'=['//TRIM(cAlphaArgs(5))//'".')
                  CALL ShowContinueError('..Values outside of range [0.0,3.0].')
                  ErrorsFound=.TRUE.
               ENDIF
             ENDIF
           ENDIF
           TempControlledZone(TempControlledZoneNum)%ZoneOvercoolControlRatio = rNumericArgs(2)
           ! check Overcool Control Ratio limits
           IF (Item == 1) THEN
             IF (TempControlledZone(TempControlledZoneNum)%ZoneOvercoolControlRatio < 0.0d0) THEN
               CALL ShowSevereError(TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(2))//' invalid '//  &
                  trim(cNumericFieldNames(2))//'=['//TRIM(TrimSigDigits(rNumericArgs(2),2))//'" cannot be negative.')
               ErrorsFound = .TRUE.
             ENDIF
           ENDIF

        ENDDO  ! TStat Objects Loop
      ENDIF  ! found thermostat reference
    ENDDO !loop over NumTempAndHumidityControlledZones
  ENDIF ! NumTempAndHumidityControlledZones > 0

  ! Staged thermostat control inputs start
  cCurrentModuleObject=cZControlTypes(iZC_StagedDual)
  NumStageControlledZones =  GetNumObjectsFound(cCurrentModuleObject)
  If (NumStageControlledZones > 0) ALLOCATE(StagedTStatObjects(NumStageControlledZones))

! Pre-scan for use of Zone lists in TStat statements (i.e. Global application of TStat)
  NumStageCtrZone=0
  DO Item=1,NumStageControlledZones
    CALL GetObjectItem(cCurrentModuleObject,Item,cAlphaArgs,NumAlphas,rNumericArgs,NumNums,IOSTAT, &
                       NumBlank=lNumericFieldBlanks,AlphaBlank=lAlphaFieldBlanks, &
                       AlphaFieldNames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
    IsNotOK = .FALSE.
    IsBlank = .FALSE.
    CALL VerifyName(cAlphaArgs(1),StagedTStatObjects%Name,Item-1,IsNotOK,IsBlank, &
                    trim(cCurrentModuleObject)//' Name')
    IF (IsNotOK) THEN
      ErrorsFound = .TRUE.
      IF (IsBlank) cAlphaArgs(1) = 'xxxxx'
    END IF
    StagedTStatObjects(Item)%Name=cAlphaArgs(1)
    Item1=FindItemInList(cAlphaArgs(2),Zone%Name,NumOfZones)
    ZLItem=0
    IF (Item1 == 0 .and. NumOfZoneLists > 0) &
        ZLItem=FindItemInList(cAlphaArgs(2),ZoneList%Name,NumOfZoneLists)
    IF (Item1 > 0) THEN
      StagedTStatObjects(Item)%StageControlledZoneStartPtr=NumStageCtrZone+1
      NumStageCtrZone=NumStageCtrZone+1
      StagedTStatObjects(Item)%NumOfZones=1
      StagedTStatObjects(Item)%ZoneListActive=.false.
      StagedTStatObjects(Item)%ZoneOrZoneListPtr=Item1
    ELSEIF (ZLItem > 0) THEN
      StagedTStatObjects(Item)%TempControlledZoneStartPtr=NumStageCtrZone+1
      NumStageCtrZone=NumStageCtrZone+ZoneList(ZLItem)%NumOfZones
      StagedTStatObjects(Item)%NumOfZones=ZoneList(ZLItem)%NumOfZones
      StagedTStatObjects(Item)%ZoneListActive=.true.
      StagedTStatObjects(Item)%ZoneOrZoneListPtr=ZLItem
    ELSE
      CALL ShowSevereError(trim(cCurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid '//  &
           trim(cAlphaFieldNames(2))//'="'//trim(cAlphaArgs(2))//'" not found.')
      ErrorsFound=.true.
    ENDIF
  ENDDO

  IF (ErrorsFound) THEN
    CALL ShowSevereError('GetStagedDualSetpoint: Errors with invalid names in '//trim(cCurrentModuleObject)//  &
       ' objects.')
    CALL ShowContinueError('...These will not be read in.  Other errors may occur.')
    NumStageCtrZone=0
  ENDIF

  IF (NumStageCtrZone > 0) THEN
    ALLOCATE(StageControlledZone(NumStageCtrZone))
    ALLOCATE(StageZoneLogic(NumOfZones))
    StageZoneLogic = .FALSE.

    StageControlledZoneNum = 0
    DO Item = 1, NumStageControlledZones
      CALL GetObjectItem(cCurrentModuleObject,Item,cAlphaArgs,NumAlphas,rNumericArgs,NumNums,IOSTAT, &
                         NumBlank=lNumericFieldBlanks,AlphaBlank=lAlphaFieldBlanks, &
                         AlphaFieldNames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
      DO Item1=1,StagedTStatObjects(Item)%NumOfZones
        StageControlledZoneNum=StageControlledZoneNum+1
        IF (StagedTStatObjects(Item)%ZoneListActive) THEN
          cAlphaArgs(2)=Zone(ZoneList(StagedTStatObjects(Item)%ZoneOrZoneListPtr)%Zone(Item1))%Name
        ENDIF
        ZoneAssigned=FindItemInList(cAlphaArgs(2),StageControlledZone%ZoneName,StageControlledZoneNum-1)
        IF (ZoneAssigned == 0) THEN
          StageControlledZone(StageControlledZoneNum)%ZoneName = cAlphaArgs(2)
          StageControlledZone(StageControlledZoneNum)%ActualZoneNum = FindIteminList(cAlphaArgs(2),Zone%Name,NumOfZones)
          IF (StageControlledZone(StageControlledZoneNum)%ActualZoneNum == 0) THEN
            CALL ShowSevereError(trim(cCurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid '//  &
               trim(cAlphaFieldNames(2))//'="'//trim(cAlphaArgs(2))//'" not found.')
            ErrorsFound = .TRUE.
          ELSE
 !           Zone(StageControlledZone(StageControlledZoneNum)%ActualZoneNum)%StageControlledZoneIndex = StageControlledZoneNum
          END IF
          StageZoneLogic(StageControlledZone(StageControlledZoneNum)%ActualZoneNum) = .TRUE.
        ELSE
          StageControlledZone(StageControlledZoneNum)%ZoneName = cAlphaArgs(2)  ! for continuity
          CALL ShowSevereError(trim(cCurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid '//  &
             trim(cAlphaFieldNames(2))//'="'//trim(cAlphaArgs(2))//'" zone previously assigned.')
          CALL ShowContinueError('...Zone was previously assigned to Thermostat="'//  &
             trim(StageControlledZone(ZoneAssigned)%Name)//'".')
          ErrorsFound = .TRUE.
          CYCLE
        ENDIF

        IF (.not. StagedTStatObjects(Item)%ZoneListActive) THEN
          StageControlledZone(StageControlledZoneNum)%Name = cAlphaArgs(1)
        ELSE
          CALL CheckCreatedZoneItemName(RoutineName,cCurrentModuleObject,  &
                                        Zone(ZoneList(StagedTStatObjects(Item)%ZoneOrZoneListPtr)%Zone(Item1))%Name,  &
                                        ZoneList(StagedTStatObjects(Item)%ZoneOrZoneListPtr)%MaxZoneNameLength,  &
                                        StagedTStatObjects(Item)%Name,     &
                                        StageControlledZone%Name,           &
                                        StageControlledZoneNum-1,                       &
                                        StageControlledZone(StageControlledZoneNum)%Name,            &
                                        ErrFlag)
          IF (ErrFlag) ErrorsFound=.true.
        ENDIF

        StageControlledZone(StageControlledZoneNum)%NumOfHeatStages = rNumericArgs(1)
        If (rNumericArgs(1) .LT. 1 .or. rNumericArgs(1) .GT. 4) Then
          CALL ShowSevereError(trim(cCurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid range '//  &
                 trim(cNumericFieldNames(1))//'="'//trim(RoundSigDigits(rNumericArgs(1),0))//'"')
          CALL ShowContinueError('..contains values outside of range [1,4].')
          ErrorsFound=.TRUE.
        End If

        StageControlledZone(StageControlledZoneNum)%HeatSetBaseSchedName = cAlphaArgs(3)
        StageControlledZone(StageControlledZoneNum)%HSBchedIndex=GetScheduleIndex(cAlphaArgs(3))
        IF (Item1 == 1) THEN  ! only show error on first of several if zone list
          IF (StageControlledZone(StageControlledZoneNum)%HSBchedIndex == 0) THEN
            CALL ShowSevereError(trim(cCurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid '//  &
               trim(cAlphaFieldNames(3))//'="'//trim(cAlphaArgs(3))//'" not found.')
            ErrorsFound = .TRUE.
          END IF
        ENDIF

        StageControlledZone(StageControlledZoneNum)%HeatThroRange = rNumericArgs(2)
        If (rNumericArgs(1) .LT. 0.d0) Then
          CALL ShowSevereError(trim(cCurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" negative value is found at '//  &
                 trim(cNumericFieldNames(2))//'="'//trim(RoundSigDigits(rNumericArgs(2),1))//'"')
          CALL ShowContinueError('.. The minumum value is 0.')
          ErrorsFound=.TRUE.
        End If

        If (StageControlledZone(StageControlledZoneNum)%NumOfHeatStages .GT. 0) Then
          ALLOCATE(StageControlledZone(StageControlledZoneNum)%HeatTOffset( &
                   StageControlledZone(StageControlledZoneNum)%NumOfHeatStages))
          Do i=1, StageControlledZone(StageControlledZoneNum)%NumOfHeatStages
            StageControlledZone(StageControlledZoneNum)%HeatTOffset(i) = rNumericArgs(2+i)
            If (rNumericArgs(2+i) .GT. 0.d0) Then
              CALL ShowSevereError(trim(cCurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" positive value is found at '//  &
                 trim(cNumericFieldNames(2+i))//'="'//trim(RoundSigDigits(rNumericArgs(2+i),1))//'"')
              CALL ShowContinueError('.. The maximum value is 0.')
              ErrorsFound=.TRUE.
            End If
            If (lNumericFieldBlanks(2+i)) Then
              CALL ShowSevereError(trim(cCurrentModuleObject)//' object ='//TRIM(cAlphaArgs(1))// &
                '. The input of ' //TRIM(cNumericFieldNames(2+i))//' is required, but a blank is found.')
              ErrorsFound=.TRUE.
            End If
            If (i > 1) Then
              If (rNumericArgs(2+i) >= rNumericArgs(1+i)) Then
                CALL ShowSevereError(trim(cCurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" The value at '//  &
                 trim(cNumericFieldNames(2+i))//'="'//trim(RoundSigDigits(rNumericArgs(2+i),1))//'" has to be less than ')
                CALL ShowContinueError(trim(cNumericFieldNames(1+i))//'="'//trim(RoundSigDigits(rNumericArgs(1+i),1)))
                ErrorsFound=.TRUE.
              End If
            End If
          End Do
        End If

        StageControlledZone(StageControlledZoneNum)%NumOfCoolStages = rNumericArgs(7)
        If (rNumericArgs(7) .LT. 1 .or. rNumericArgs(7) .GT. 4) Then
          CALL ShowSevereError(trim(cCurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid range '//  &
                 trim(cNumericFieldNames(7))//'="'//trim(RoundSigDigits(rNumericArgs(7),0))//'"')
          CALL ShowContinueError('..contains values outside of range [1,4].')
          ErrorsFound=.TRUE.
        End If

        StageControlledZone(StageControlledZoneNum)%CoolSetBaseSchedName = cAlphaArgs(4)
        StageControlledZone(StageControlledZoneNum)%CSBchedIndex=GetScheduleIndex(cAlphaArgs(4))
        IF (Item1 == 1) THEN  ! only show error on first of several if zone list
          IF (StageControlledZone(StageControlledZoneNum)%CSBchedIndex == 0) THEN
            CALL ShowSevereError(trim(cCurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid '//  &
               trim(cAlphaFieldNames(4))//'="'//trim(cAlphaArgs(4))//'" not found.')
            ErrorsFound = .TRUE.
          END IF
        ENDIF

        StageControlledZone(StageControlledZoneNum)%CoolThroRange = rNumericArgs(8)
        If (rNumericArgs(8) .LT. 0.d0) Then
          CALL ShowSevereError(trim(cCurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" negative value is found at '//  &
                 trim(cNumericFieldNames(8))//'="'//trim(RoundSigDigits(rNumericArgs(8),1))//'"')
          CALL ShowContinueError('.. The minumum value is 0.')
          ErrorsFound=.TRUE.
        End If

        If (StageControlledZone(StageControlledZoneNum)%NumOfCoolStages .GT. 0) Then
          ALLOCATE(StageControlledZone(StageControlledZoneNum)%CoolTOffset( &
            StageControlledZone(StageControlledZoneNum)%NumOfCoolStages))
          Do i=1, StageControlledZone(StageControlledZoneNum)%NumOfCoolStages
            StageControlledZone(StageControlledZoneNum)%CoolTOffset(i) = rNumericArgs(8+i)
            If (rNumericArgs(8+i) .LT. 0.d0) Then
              CALL ShowSevereError(trim(cCurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" negative value is found at '//  &
                 trim(cNumericFieldNames(8+i))//'="'//trim(RoundSigDigits(rNumericArgs(8+i),1))//'"')
              CALL ShowContinueError('.. The minimum value is 0.')
              ErrorsFound=.TRUE.
            End If
            If (lNumericFieldBlanks(8+i)) Then
              CALL ShowSevereError(trim(cCurrentModuleObject)//' object ='//TRIM(cAlphaArgs(1))// &
                '. The input of ' //TRIM(cNumericFieldNames(8+i))//' is required, but a blank is found.')
              ErrorsFound=.TRUE.
            End If
            If (i > 1) Then
              If (rNumericArgs(8+i) <= rNumericArgs(7+i)) Then
                CALL ShowSevereError(trim(cCurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" The value at '//  &
                 trim(cNumericFieldNames(8+i))//'="'//trim(RoundSigDigits(rNumericArgs(8+i),1))//'" has to be greater than ')
                CALL ShowContinueError(trim(cNumericFieldNames(7+i))//'="'//trim(RoundSigDigits(rNumericArgs(7+i),1)))
                ErrorsFound=.TRUE.
              End If
            End If
          End Do
        End If
      End Do
    ENDDO !loop over NumStageControlledZones
    If ((GetNumObjectsFound('AirLoopHVAC:UnitaryHeatPump:AirToAir:MultiSpeed') .EQ. 0) .AND. &
        (GetNumObjectsFound('AirLoopHVAC:UnitarySystem') .EQ. 0) .AND. &
        (GetNumObjectsFound('SetpointManager:SingleZone:OneStageCooling') == 0) .AND. &
        (GetNumObjectsFound('SetpointManager:SingleZone:OneStageHeating') == 0) )     THEN
      CALL ShowWarningError(Trim(cCurrentModuleObject)// &
        ' is applicable to only selected HVAC objects which are missing from input.' )
      CALL ShowContinueError('Model should include one or more of the following objects:  ')
      CALL ShowContinueError('AirLoopHVAC:UnitaryHeatPump:AirToAir:MultiSpeed, AirLoopHVAC:UnitarySystem, ')
      CALL ShowContinueError('SetpointManager:SingleZone:OneStageCooling, '// &
                             'and/or SetpointManager:SingleZone:OneStageHeating. The simulation continues...')
    End If
  ENDIF ! NumStageControlledZones > 0

  IF (ErrorsFound) THEN
    CALL ShowFatalError('Errors getting Zone Control input data.  Preceding condition(s) cause termination.')
  END IF

  RETURN

END SUBROUTINE GetZoneAirSetpoints

SUBROUTINE InitZoneAirSetpoints

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Russell Taylor
          !       DATE WRITTEN   September 1998
          !       MODIFIED       November 2004, M. J. Witte additional report variables
          !       MODIFIED       L.Gu, May 2006
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine initializes the data for the zone air setpoints.

          ! METHODOLOGY EMPLOYED:
          ! Uses the status flags to trigger events.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataZoneEquipment, ONLY: ZoneEquipConfig, ZoneEquipInputsFilled
  USE DataSurfaces, ONLY: Surface

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER :: RoutineName='InitZoneAirSetpoints: '

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: Loop, ZoneNum
  LOGICAL,SAVE  :: MyOneTimeFlag = .TRUE.
  LOGICAL,SAVE  :: MyEnvrnFlag = .TRUE.
  LOGICAL,SAVE  :: MyDayFlag = .TRUE.
  LOGICAL, SAVE :: ErrorsFound=.false.
  LOGICAL, SAVE :: ControlledZonesChecked=.false.
  LOGICAL :: FirstSurfFlag
  INTEGER :: TRefFlag  ! Flag for Reference Temperature process in Zones
  INTEGER :: SurfNum

          ! FLOW:
  IF (MyOneTimeFlag) THEN
    ALLOCATE(TempZoneThermostatSetPoint(NumOfZones))
    TempZoneThermostatSetPoint=0.0d0
    ALLOCATE(ZoneThermostatSetPointHi(NumOfZones))
    ZoneThermostatSetPointHi=0.0d0
    ALLOCATE(ZoneThermostatSetPointLo(NumOfZones))
    ZoneThermostatSetPointLo=0.0d0

    ALLOCATE(LoadCorrectionFactor(NumOfZones))  !PH 3/3/04
    LoadCorrectionFactor=0.0d0
    ALLOCATE(TempControlType(NumOfZones))
    TempControlType=0
    If (NumComfortControlledZones > 0) then
      ALLOCATE(ComfortControlType(NumOfZones))
      ComfortControlType=0
      ALLOCATE(ZoneComfortControlsFanger(NumOfZones))
    End If
    ALLOCATE(ZoneSetPointLast(NumOfZones))
    ZoneSetPointLast=0.0d0
    ALLOCATE(Setback(NumOfZones))
    Setback=.false.
    ALLOCATE(DeadbandOrSetback(NumOfZones))
    DeadbandOrSetback=.false.
    ALLOCATE(CurDeadbandOrSetback(NumOfZones))
    CurDeadbandOrSetback=.false.
    ALLOCATE(SNLoadHeatEnergy(NumOfZones))
    SNLoadHeatEnergy=0.0d0
    ALLOCATE(SNLoadCoolEnergy(NumOfZones))
    SNLoadCoolEnergy=0.0d0
    ALLOCATE(SNLoadHeatRate(NumOfZones))
    SNLoadHeatRate=0.0d0
    ALLOCATE(SNLoadCoolRate(NumOfZones))
    SNLoadCoolRate=0.0d0
    ALLOCATE(SNLoadPredictedRate(NumOfZones))
    SNLoadPredictedRate=0.0d0
    ALLOCATE(SNLoadPredictedHSPRate(NumOfZones))
    SNLoadPredictedHSPRate=0.0d0
    ALLOCATE(SNLoadPredictedCSPRate(NumOfZones))
    SNLoadPredictedCSPRate=0.0d0
    ALLOCATE(MoisturePredictedRate(NumOfZones))
    MoisturePredictedRate=0.0d0
    ALLOCATE(WZoneTimeMinus1(NumOfZones))
    WZoneTimeMinus1=0.0d0
    ALLOCATE(WZoneTimeMinus2(NumOfZones))
    WZoneTimeMinus2=0.0d0
    ALLOCATE(WZoneTimeMinus3(NumOfZones))
    WZoneTimeMinus3=0.0d0
    ALLOCATE(WZoneTimeMinus4(NumOfZones))
    WZoneTimeMinus4=0.0d0
    ALLOCATE(DSWZoneTimeMinus1(NumOfZones))
    DSWZoneTimeMinus1=0.0d0
    ALLOCATE(DSWZoneTimeMinus2(NumOfZones))
    DSWZoneTimeMinus2=0.0d0
    ALLOCATE(DSWZoneTimeMinus3(NumOfZones))
    DSWZoneTimeMinus3=0.0d0
    ALLOCATE(DSWZoneTimeMinus4(NumOfZones))
    DSWZoneTimeMinus4=0.0d0
    ALLOCATE(ZoneAirHumRatTemp(NumOfZones))
    ZoneAirHumRatTemp=0.0d0
    ALLOCATE(WZoneTimeMinus1Temp(NumOfZones))
    WZoneTimeMinus1Temp=0.0d0
    ALLOCATE(WZoneTimeMinus2Temp(NumOfZones))
    WZoneTimeMinus2Temp=0.0d0
    ALLOCATE(WZoneTimeMinus3Temp(NumOfZones))
    WZoneTimeMinus3Temp=0.0d0
    ALLOCATE(WZoneTimeMinusP(NumOfZones))
    WZoneTimeMinusP=0.0d0
    ALLOCATE(TempIndZnLd(NumOfZones))
    TempIndZnLd=0.0d0
    ALLOCATE(TempDepZnLd(NumOfZones))
    TempDepZnLd=0.0d0
    ALLOCATE(NonAirSystemResponse(NumOfZones))
    NonAirSystemResponse=0.0d0
    ALLOCATE(SysDepZoneLoads(NumOfZones))
    SysDepZoneLoads=0.0d0
    ALLOCATE(SysDepZoneLoadsLagged(NumOfZones))
    SysDepZoneLoadsLagged=0.0d0
    ALLOCATE(ZoneAirRelHum(NumOfZones))
    ZoneAirRelHum=0.0d0
    ALLOCATE(ZoneWMX(NumOfZones))
    ZoneWMX = 0.0d0
    ALLOCATE(ZoneWM2(NumOfZones))
    ZoneWM2 = 0.0d0
    ALLOCATE(ZoneT1(NumOfZones))
    ZoneT1=0.0d0
    ALLOCATE(ZoneW1(NumOfZones))
    ZoneW1=0.0d0

    ALLOCATE(ListSNLoadHeatEnergy(NumOfZoneLists))
    ListSNLoadHeatEnergy=0.0d0
    ALLOCATE(ListSNLoadCoolEnergy(NumOfZoneLists))
    ListSNLoadCoolEnergy=0.0d0
    ALLOCATE(ListSNLoadHeatRate(NumOfZoneLists))
    ListSNLoadHeatRate=0.0d0
    ALLOCATE(ListSNLoadCoolRate(NumOfZoneLists))
    ListSNLoadCoolRate=0.0d0

    ALLOCATE(GroupSNLoadHeatEnergy(NumOfZoneGroups))
    GroupSNLoadHeatEnergy=0.0d0
    ALLOCATE(GroupSNLoadCoolEnergy(NumOfZoneGroups))
    GroupSNLoadCoolEnergy=0.0d0
    ALLOCATE(GroupSNLoadHeatRate(NumOfZoneGroups))
    GroupSNLoadHeatRate=0.0d0
    ALLOCATE(GroupSNLoadCoolRate(NumOfZoneGroups))
    GroupSNLoadCoolRate=0.0d0
    ALLOCATE(AIRRAT(NumOfZones))
    AIRRAT = 0.0d0
    ALLOCATE(ZTM1(NumOfZones))
    ZTM1 = 0.0d0
    ALLOCATE(ZTM2(NumOfZones))
    ZTM2 = 0.0d0
    ALLOCATE(ZTM3(NumOfZones))
    ZTM3 = 0.0d0

    ! Allocate Derived Types
    ALLOCATE(ZoneSysEnergyDemand(NumOfZones))
    ALLOCATE(ZoneSysMoistureDemand(NumOfZones))

    DO Loop = 1, NumOfZones
      FirstSurfFlag=.true.
      DO SurfNum = Zone(Loop)%SurfaceFirst,Zone(Loop)%SurfaceLast
        IF (.NOT. Surface(SurfNum)%HeatTransSurf) CYCLE ! Skip non-heat transfer surfaces

        IF (FirstSurfFlag) THEN
          TrefFlag = Surface(SurfNum)%TAirRef
          FirstSurfFlag=.false.
        END IF
        ! for each particular zone, the reference air temperature(s) should be the same
        ! (either mean air, bulk air, or supply air temp).
        IF (Surface(SurfNum)%TAirRef /= TrefFlag) THEN
          CALL ShowWarningError('Different reference air temperatures for difference surfaces encountered in zone '//  &
                                  TRIM(Zone(Loop)%Name))
        END IF

      ENDDO
    ENDDO

    ! CurrentModuleObject='Zone'
    DO Loop = 1, NumOfZones
      CALL SetupOutputVariable('Zone Air System Sensible Heating Energy [J]',SNLoadHeatEnergy(Loop), &
                               'System','Sum',Zone(Loop)%Name,ResourceTypeKey='ENERGYTRANSFER', &
                                EndUseKey='Heating',GroupKey='Building',ZoneKey=Zone(Loop)%Name, &
                                ZoneMult=Zone(Loop)%Multiplier, &
                                ZoneListMult=Zone(Loop)%ListMultiplier)
      CALL SetupOutputVariable('Zone Air System Sensible Cooling Energy [J]',SNLoadCoolEnergy(Loop), &
                               'System','Sum',Zone(Loop)%Name,ResourceTypeKey='ENERGYTRANSFER', &
                                EndUseKey='Cooling',GroupKey='Building',ZoneKey=Zone(Loop)%Name, &
                                ZoneMult=Zone(Loop)%Multiplier, &
                                ZoneListMult=Zone(Loop)%ListMultiplier)
      CALL SetupOutputVariable('Zone Air System Sensible Heating Rate [W]',SNLoadHeatRate(Loop),'System','Average',Zone(Loop)%Name)
      CALL SetupOutputVariable('Zone Air System Sensible Cooling Rate [W]',SNLoadCoolRate(Loop),'System','Average',Zone(Loop)%Name)
      CALL SetupOutputVariable('Zone Air Temperature [C]',Zt(Loop),'System','Average',Zone(Loop)%Name)
      CALL SetupOutputVariable('Zone Thermostat Air Temperature [C]',TempTstatAir(Loop),'System','Average',Zone(Loop)%Name)
      CALL SetupOutputVariable('Zone Air Humidity Ratio []',ZoneAirHumRat(Loop),'System','Average',Zone(Loop)%Name)
      CALL SetupOutputVariable('Zone Air Relative Humidity [%]',ZoneAirRelHum(Loop),'System', &
                               'Average',Zone(Loop)%Name)
      ! This output variable is for the predicted Heating/Cooling load for the zone which can be compared to actual load
      ! These report variables are not multiplied by zone and group multipliers
      CALL SetupOutputVariable('Zone Predicted Sensible Load to Setpoint Heat Transfer Rate [W]', SNLoadPredictedRate(Loop), &
                               'System','Average',Zone(Loop)%Name)
      CALL SetupOutputVariable('Zone Predicted Sensible Load to Heating Setpoint Heat Transfer Rate [W]', &
                                SNLoadPredictedHSPRate(Loop), &
                               'System','Average',Zone(Loop)%Name)
      CALL SetupOutputVariable('Zone Predicted Sensible Load to Cooling Setpoint Heat Transfer Rate [W]', &
                                SNLoadPredictedCSPRate(Loop), &
                               'System','Average',Zone(Loop)%Name)
      ! This output variable is for the predicted moisture load for the zone with humidity controlled specified.
      CALL SetupOutputVariable('Zone Predicted Moisture Load Moisture Transfer Rate [kgWater/s]', MoisturePredictedRate(Loop), &
                               'System','Average',Zone(Loop)%Name)
      CALL SetupOutputVariable('Zone Predicted Moisture Load to Humidifying Setpoint Moisture Transfer Rate [kgWater/s]', &
                               ZoneSysMoistureDemand(Loop)%OutputRequiredToHumidifyingSP,'System','Average',Zone(Loop)%Name)
      CALL SetupOutputVariable('Zone Predicted Moisture Load to Dehumidifying Setpoint Moisture Transfer Rate [kgWater/s]', &
                               ZoneSysMoistureDemand(Loop)%OutputRequiredToDehumidifyingSP,'System','Average',Zone(Loop)%Name)
      ! Zone thermostat setpoints
      CALL SetupOutputVariable('Zone Thermostat Control Type []', &
                                TempControlType(Loop), &
                               'Zone','Average',Zone(Loop)%Name)
      CALL SetupOutputVariable('Zone Thermostat Heating Setpoint Temperature [C]', &
                                ZoneThermostatSetPointLo(Loop), &
                               'Zone','Average',Zone(Loop)%Name)
      CALL SetupOutputVariable('Zone Thermostat Cooling Setpoint Temperature [C]', &
                                ZoneThermostatSetPointHi(Loop), &
                               'Zone','Average',Zone(Loop)%Name)

      CALL SetupOutputVariable('Zone Predicted Sensible Load Room Air Correction Factor [ ]', &
                                LoadCorrectionFactor(loop), &
                               'System','Average',Zone(Loop)%Name)

      If (ALLOCATED(StageZoneLogic)) Then
        If (StageZoneLogic(Loop)) Then
          CALL SetupOutputVariable('Zone Thermostat Staged Number []',  &
                                     ZoneSysEnergyDemand(Loop)%StageNum,'System','Average',Zone(Loop)%Name)
        End If
      End If

    END DO ! Loop

    ! Thermal comfort control output
    If (NumComfortControlledZones > 0) then
     ! CurrentModuleObject='ZoneControl:Thermostat:ThermalComfort'
      DO Loop = 1, NumComfortControlledZones
        ZoneNum = ComfortControlledZone(Loop)%ActualZoneNum
        CALL SetupOutputVariable('Zone Thermal Comfort Control Type []', &
                                ComfortControlType(ZoneNum), 'Zone','Average',Zone(ZoneNum)%Name)
        CALL SetupOutputVariable('Zone Thermal Comfort Control Fanger Low Setpoint PMV []', &
                                ZoneComfortControlsFanger(ZoneNum)%LowPMV, 'Zone','Average',Zone(ZoneNum)%Name)
        CALL SetupOutputVariable('Zone Thermal Comfort Control Fanger High Setpoint PMV []', &
                                ZoneComfortControlsFanger(ZoneNum)%HighPMV, 'Zone','Average',Zone(ZoneNum)%Name)
      END DO
    End If

    ! CurrentModuleObject='ZoneList'
    DO Loop = 1, NumOfZoneLists
      CALL SetupOutputVariable('Zone List Sensible Heating Energy [J]',ListSNLoadHeatEnergy(Loop), &
                               'System','Sum',ZoneList(Loop)%Name)
      CALL SetupOutputVariable('Zone List Sensible Cooling Energy [J]',ListSNLoadCoolEnergy(Loop), &
                               'System','Sum',ZoneList(Loop)%Name)
      CALL SetupOutputVariable('Zone List Sensible Heating Rate [W]',ListSNLoadHeatRate(Loop),'System','Average',  &
                                ZoneList(Loop)%Name)
      CALL SetupOutputVariable('Zone List Sensible Cooling Rate [W]',ListSNLoadCoolRate(Loop),'System','Average',  &
                                ZoneList(Loop)%Name)
    END DO ! Loop

    ! CurrentModuleObject='ZoneGroup'
    DO Loop = 1, NumOfZoneGroups
      CALL SetupOutputVariable('Zone Group Sensible Heating Energy [J]',GroupSNLoadHeatEnergy(Loop), &
                               'System','Sum',ZoneGroup(Loop)%Name)
      CALL SetupOutputVariable('Zone Group Sensible Cooling Energy [J]',GroupSNLoadCoolEnergy(Loop), &
                               'System','Sum',ZoneGroup(Loop)%Name)
      CALL SetupOutputVariable('Zone Group Sensible Heating Rate [W]',GroupSNLoadHeatRate(Loop),'System','Average',  &
                                   ZoneGroup(Loop)%Name)
      CALL SetupOutputVariable('Zone Group Sensible Cooling Rate [W]',GroupSNLoadCoolRate(Loop),'System','Average',  &
                                   ZoneGroup(Loop)%Name)
    END DO ! Loop

    MyOneTimeFlag = .FALSE.
  END IF

  ! Do the Begin Environment initializations
  IF (MyEnvrnFlag .AND. BeginEnvrnFlag) THEN
    AIRRAT = 0.0d0
    ZTM1 = 0.0d0
    ZTM2 = 0.0d0
    ZTM3 = 0.0d0
    WZoneTimeMinus1 = OutHumRat
    WZoneTimeMinus2 = OutHumRat
    WZoneTimeMinus3 = OutHumRat
    WZoneTimeMinus4 = OutHumRat
    WZoneTimeMinusP = OutHumRat
    DSWZoneTimeMinus1 = OutHumRat
    DSWZoneTimeMinus2 = OutHumRat
    DSWZoneTimeMinus3 = OutHumRat
    DSWZoneTimeMinus4 = OutHumRat
    WZoneTimeMinus1Temp = 0.0d0
    WZoneTimeMinus2Temp = 0.0d0
    WZoneTimeMinus3Temp = 0.0d0
    ZoneAirHumRatTemp = 0.0d0
    TempZoneThermostatSetPoint = 0.0d0
    ZoneThermostatSetPointHi = 0.0d0
    ZoneThermostatSetPointLo = 0.0d0

    LoadCorrectionFactor = 1.d0 !PH 3/3/04
    TempControlType = 0
    ZoneSysEnergyDemand(1:NumOfZones)%RemainingOutputRequired = 0.0d0
    ZoneSysEnergyDemand(1:NumOfZones)%TotalOutputRequired = 0.0d0
    ZoneSysMoistureDemand(1:NumOfZones)%RemainingOutputRequired = 0.0d0
    ZoneSysMoistureDemand(1:NumOfZones)%TotalOutputRequired = 0.0d0
    DO ZoneNum = 1, NumOfZones
      IF (ALLOCATED(ZoneSysEnergyDemand(ZoneNum)%SequencedOutputRequired)) &
        ZoneSysEnergyDemand(ZoneNum)%SequencedOutputRequired = 0.d0
      IF (ALLOCATED(ZoneSysEnergyDemand(ZoneNum)%SequencedOutputRequiredToHeatingSP)) &
        ZoneSysEnergyDemand(ZoneNum)%SequencedOutputRequiredToHeatingSP = 0.d0
      IF (ALLOCATED(ZoneSysEnergyDemand(ZoneNum)%SequencedOutputRequiredToCoolingSP)) &
        ZoneSysEnergyDemand(ZoneNum)%SequencedOutputRequiredToCoolingSP = 0.d0
      IF (ALLOCATED(ZoneSysMoistureDemand(ZoneNum)%SequencedOutputRequired)) &
        ZoneSysMoistureDemand(ZoneNum)%SequencedOutputRequired = 0.d0
      IF (ALLOCATED(ZoneSysMoistureDemand(ZoneNum)%SequencedOutputRequiredToHumidSP)) &
        ZoneSysMoistureDemand(ZoneNum)%SequencedOutputRequiredToHumidSP = 0.d0
      IF (ALLOCATED(ZoneSysMoistureDemand(ZoneNum)%SequencedOutputRequiredToDehumidSP)) &
        ZoneSysMoistureDemand(ZoneNum)%SequencedOutputRequiredToDehumidSP = 0.d0
    ENDDO


    DeadbandOrSetback = .FALSE.
    SNLoadHeatEnergy = 0.0d0
    SNLoadCoolEnergy = 0.0d0
    SNLoadHeatRate = 0.0d0
    SNLoadCoolRate = 0.0d0
    SNLoadPredictedRate = 0.0d0
    SNLoadPredictedHSPRate = 0.0d0
    SNLoadPredictedCSPRate = 0.0d0
    MoisturePredictedRate = 0.0d0
    TempIndZnLd = 0.0d0
    TempDepZnLd = 0.0d0
    NonAirSystemResponse = 0.0d0
    SysDepZoneLoads = 0.0d0
    SysDepZoneLoadsLagged = 0.0d0
    ZoneAirRelHum = 0.0d0
    Zone%NoHeatToReturnAir = .FALSE.
    ZoneT1 = 0.0d0
    ZoneW1 = OutHumRat
    ZoneWMX = OutHumRat
    ZoneWM2 = OutHumRat

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

  DO Loop = 1, NumTempControlledZones
    IF (ZoneEquipInputsFilled .and. .not. ControlledZonesChecked) THEN
      IF (.not. VerifyControlledZoneForThermostat(TempControlledZone(Loop)%ZoneName)) THEN
        CALL ShowSevereError(RoutineName//'Zone="'//TRIM(TempControlledZone(Loop)%ZoneName)//'" has specified a'//  &
                             ' Thermostatic control but is not a controlled zone.')
        CALL ShowContinueError('...must have a ZoneHVAC:EquipmentConnections specification for this zone.')
        ErrorsFound=.true.
      ENDIF
    ENDIF

    IF (TempControlledZone(Loop)%ManageDemand) THEN
      ZoneNum = TempControlledZone(Loop)%ActualZoneNum


      SELECT CASE(TempControlType(ZoneNum))

        CASE (0) ! Uncontrolled

        CASE (SingleHeatingSetPoint)
          IF (TempZoneThermostatSetPoint(ZoneNum) > TempControlledZone(Loop)%HeatingResetLimit) THEN
            TempZoneThermostatSetPoint(ZoneNum) = TempControlledZone(Loop)%HeatingResetLimit
            ZoneThermostatSetPointLo(ZoneNum) = TempZoneThermostatSetPoint(ZoneNum)
          END IF

        CASE (SingleCoolingSetPoint)
          IF (TempZoneThermostatSetPoint(ZoneNum) < TempControlledZone(Loop)%CoolingResetLimit) THEN
            TempZoneThermostatSetPoint(ZoneNum) = TempControlledZone(Loop)%CoolingResetLimit
            ZoneThermostatSetPointHi(ZoneNum) = TempZoneThermostatSetPoint(ZoneNum)
          END IF

        CASE (SingleHeatCoolSetPoint)
          IF ((TempZoneThermostatSetPoint(ZoneNum) > TempControlledZone(Loop)%HeatingResetLimit) &
            .OR. (TempZoneThermostatSetPoint(ZoneNum) < TempControlledZone(Loop)%CoolingResetLimit)) THEN

            TempControlType(ZoneNum) = DualSetPointWithDeadBand
            ZoneThermostatSetPointLo(ZoneNum) = TempZoneThermostatSetPoint(ZoneNum)
            ZoneThermostatSetPointHi(ZoneNum) = TempZoneThermostatSetPoint(ZoneNum)

            IF (ZoneThermostatSetPointLo(ZoneNum) > TempControlledZone(Loop)%HeatingResetLimit) &
              ZoneThermostatSetPointLo(ZoneNum) = TempControlledZone(Loop)%HeatingResetLimit
            IF (ZoneThermostatSetPointHi(ZoneNum) < TempControlledZone(Loop)%CoolingResetLimit) &
              ZoneThermostatSetPointHi(ZoneNum) = TempControlledZone(Loop)%CoolingResetLimit

          END IF

        CASE (DualSetPointWithDeadBand)
          IF (ZoneThermostatSetPointLo(ZoneNum) > TempControlledZone(Loop)%HeatingResetLimit) &
            ZoneThermostatSetPointLo(ZoneNum) = TempControlledZone(Loop)%HeatingResetLimit
          IF (ZoneThermostatSetPointHi(ZoneNum) < TempControlledZone(Loop)%CoolingResetLimit) &
            ZoneThermostatSetPointHi(ZoneNum) = TempControlledZone(Loop)%CoolingResetLimit

        CASE DEFAULT
          ! Do nothing

      END SELECT
    END IF
  END DO

  DO Loop = 1, NumComfortControlledZones
    IF (ZoneEquipInputsFilled .and. .not. ControlledZonesChecked) THEN
      IF (.not. VerifyControlledZoneForThermostat(ComfortControlledZone(Loop)%ZoneName)) THEN
        CALL ShowSevereError(RoutineName//'Zone="'//TRIM(ComfortControlledZone(Loop)%ZoneName)//'" has specified a'//  &
                             ' Comfort control but is not a controlled zone.')
        CALL ShowContinueError('...must have a ZoneHVAC:EquipmentConnections specification for this zone.')
        ErrorsFound=.true.
      ENDIF
    ENDIF
    IF (ComfortControlledZone(Loop)%ManageDemand) THEN
      ZoneNum = ComfortControlledZone(Loop)%ActualZoneNum

      SELECT CASE(ComfortControlType(ZoneNum))

        CASE (0) ! Uncontrolled

        CASE (SglHeatSetPointFanger)
          IF (TempZoneThermostatSetPoint(ZoneNum) >= ComfortControlledZone(Loop)%HeatingResetLimit) THEN
            TempZoneThermostatSetPoint(ZoneNum) = ComfortControlledZone(Loop)%HeatingResetLimit
            ZoneThermostatSetPointLo(ZoneNum) = TempZoneThermostatSetPoint(ZoneNum)
            TempControlType(ZoneNum) = SingleHeatingSetPoint
          END IF

        CASE (SglCoolSetPointFanger)
          IF (TempZoneThermostatSetPoint(ZoneNum) <= ComfortControlledZone(Loop)%CoolingResetLimit) THEN
            TempZoneThermostatSetPoint(ZoneNum) = ComfortControlledZone(Loop)%CoolingResetLimit
            ZoneThermostatSetPointHi(ZoneNum) = TempZoneThermostatSetPoint(ZoneNum)
            TempControlType(ZoneNum) = SingleCoolingSetPoint
          END IF

        CASE (SglHCSetPointFanger)
          IF ((TempZoneThermostatSetPoint(ZoneNum) >= ComfortControlledZone(Loop)%HeatingResetLimit) &
            .OR. (TempZoneThermostatSetPoint(ZoneNum) <= ComfortControlledZone(Loop)%CoolingResetLimit)) THEN

            TempControlType(ZoneNum) = DualSetPointWithDeadBand
            ZoneThermostatSetPointLo(ZoneNum) = TempZoneThermostatSetPoint(ZoneNum)
            ZoneThermostatSetPointHi(ZoneNum) = TempZoneThermostatSetPoint(ZoneNum)

            IF (ZoneThermostatSetPointLo(ZoneNum) >= ComfortControlledZone(Loop)%HeatingResetLimit) &
              ZoneThermostatSetPointLo(ZoneNum) = ComfortControlledZone(Loop)%HeatingResetLimit
            IF (ZoneThermostatSetPointHi(ZoneNum) <= ComfortControlledZone(Loop)%CoolingResetLimit) &
              ZoneThermostatSetPointHi(ZoneNum) = ComfortControlledZone(Loop)%CoolingResetLimit

          END IF

        CASE (DualSetPointFanger)
          TempControlType(ZoneNum) = DualSetPointWithDeadBand
          IF (ZoneThermostatSetPointLo(ZoneNum) >= ComfortControlledZone(Loop)%HeatingResetLimit) &
            ZoneThermostatSetPointLo(ZoneNum) = ComfortControlledZone(Loop)%HeatingResetLimit
          IF (ZoneThermostatSetPointHi(ZoneNum) <= ComfortControlledZone(Loop)%CoolingResetLimit) &
            ZoneThermostatSetPointHi(ZoneNum) = ComfortControlledZone(Loop)%CoolingResetLimit

        CASE DEFAULT
          ! Do nothing

      END SELECT
    END IF !Demand manager
  END DO

  DO Loop = 1, NumTempControlledZones
    IF (TempControlledZone(Loop)%EMSOverrideHeatingSetpointOn) THEN
      ZoneNum = TempControlledZone(Loop)%ActualZoneNum

      SELECT CASE(TempControlType(ZoneNum))

      CASE (0) ! Uncontrolled

      CASE (SingleHeatingSetPoint)
        TempZoneThermostatSetPoint(ZoneNum) = TempControlledZone(Loop)%EMSOverrideHeatingSetpointValue
        ZoneThermostatSetPointLo(ZoneNum)   = TempControlledZone(Loop)%EMSOverrideHeatingSetpointValue
      CASE (SingleCoolingSetPoint)
        ! do nothing
      CASE (SingleHeatCoolSetPoint)
        TempZoneThermostatSetPoint(ZoneNum) = TempControlledZone(Loop)%EMSOverrideHeatingSetpointValue
        ZoneThermostatSetPointLo(ZoneNum)   = TempControlledZone(Loop)%EMSOverrideHeatingSetpointValue
      CASE (DualSetPointWithDeadBand)
        ZoneThermostatSetPointLo(ZoneNum)   = TempControlledZone(Loop)%EMSOverrideHeatingSetpointValue
      CASE DEFAULT
          ! Do nothing
      END SELECT
    ENDIF
    IF (TempControlledZone(Loop)%EMSOverrideCoolingSetpointOn) THEN
      ZoneNum = TempControlledZone(Loop)%ActualZoneNum

      SELECT CASE(TempControlType(ZoneNum))

      CASE (0) ! Uncontrolled

      CASE (SingleHeatingSetPoint)
        ! do nothing
      CASE (SingleCoolingSetPoint)
        TempZoneThermostatSetPoint(ZoneNum) = TempControlledZone(Loop)%EMSOverrideCoolingSetpointValue
        ZoneThermostatSetPointHi(ZoneNum)   = TempControlledZone(Loop)%EMSOverrideCoolingSetpointValue
      CASE (SingleHeatCoolSetPoint)
        TempZoneThermostatSetPoint(ZoneNum) = TempControlledZone(Loop)%EMSOverrideCoolingSetpointValue
        ZoneThermostatSetPointHi(ZoneNum)   = TempControlledZone(Loop)%EMSOverrideCoolingSetpointValue
      CASE (DualSetPointWithDeadBand)
        ZoneThermostatSetPointHi(ZoneNum)   = TempControlledZone(Loop)%EMSOverrideCoolingSetpointValue
      CASE DEFAULT
          ! Do nothing
      END SELECT
    ENDIF
  ENDDO
  DO Loop = 1, NumComfortControlledZones
    IF (ComfortControlledZone(Loop)%EMSOverrideHeatingSetpointOn) THEN
      ZoneNum = ComfortControlledZone(Loop)%ActualZoneNum
      SELECT CASE(ComfortControlType(ZoneNum))

      CASE (0) ! Uncontrolled

      CASE (SglHeatSetPointFanger)
        TempZoneThermostatSetPoint(ZoneNum) = ComfortControlledZone(Loop)%EMSOverrideHeatingSetpointValue
        ZoneThermostatSetPointLo(ZoneNum)   = ComfortControlledZone(Loop)%EMSOverrideHeatingSetpointValue
      CASE (SglCoolSetPointFanger)
        ! do nothing
      CASE (SglHCSetPointFanger)
        TempZoneThermostatSetPoint(ZoneNum) = ComfortControlledZone(Loop)%EMSOverrideHeatingSetpointValue
        ZoneThermostatSetPointLo(ZoneNum)   = ComfortControlledZone(Loop)%EMSOverrideHeatingSetpointValue
      CASE (DualSetPointFanger)
        ZoneThermostatSetPointLo(ZoneNum)   = ComfortControlledZone(Loop)%EMSOverrideHeatingSetpointValue
      CASE DEFAULT
          ! Do nothing
      END SELECT
    ENDIF
    IF (ComfortControlledZone(Loop)%EMSOverrideCoolingSetpointOn) THEN
      ZoneNum = ComfortControlledZone(Loop)%ActualZoneNum
      SELECT CASE(ComfortControlType(ZoneNum))

      CASE (0) ! Uncontrolled

      CASE (SglHeatSetPointFanger)
        ! do nothing
      CASE (SglCoolSetPointFanger)
        TempZoneThermostatSetPoint(ZoneNum) = ComfortControlledZone(Loop)%EMSOverrideCoolingSetpointValue
        ZoneThermostatSetPointHi(ZoneNum)   = ComfortControlledZone(Loop)%EMSOverrideCoolingSetpointValue
      CASE (SglHCSetPointFanger)
        TempZoneThermostatSetPoint(ZoneNum) = ComfortControlledZone(Loop)%EMSOverrideCoolingSetpointValue
        ZoneThermostatSetPointHi(ZoneNum)   = ComfortControlledZone(Loop)%EMSOverrideCoolingSetpointValue
      CASE (DualSetPointFanger)
        ZoneThermostatSetPointHi(ZoneNum)   = ComfortControlledZone(Loop)%EMSOverrideCoolingSetpointValue
      CASE DEFAULT
          ! Do nothing
      END SELECT
    ENDIF

  ENDDO



  IF (ErrorsFound) THEN
    CALL ShowFatalError('InitZoneAirSetpoints - program terminates due to previous condition.')
  ENDIF

  IF (ZoneEquipInputsFilled) THEN
    ControlledZonesChecked=.true.
  ENDIF

  RETURN

END SUBROUTINE InitZoneAirSetpoints

SUBROUTINE PredictSystemLoads(ShortenTimeStepSys, UseZoneTimeStepHistory, PriorTimeStep )

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Russ Taylor
          !       DATE WRITTEN   May 1997
          !       MODIFIED       na
          !       RE-ENGINEERED  July 2003 (Peter Graham Ellis)

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is responsible for determining
          ! how much of each type of energy every zone requires.
          ! In effect, this subroutine defines and simulates all
          ! the system types and in the case of hybrid systems
          ! which use more than one type of energy must determine
          ! how to apportion the load. An example of a hybrid system
          ! is a water loop heat pump with supplemental air.  In
          ! this case, a zone will require water from the loop and
          ! cooled or heated air from the air system. A simpler
          ! example would be a VAV system with baseboard heaters.

          !  Basic Air System Types
          !  1) Constant Volume Single Duct
          !  2) Variable Volume Single Duct
          !  3) Constant Volume Dual Duct
          !  4) Variable Volume Dual Duct

          ! METHODOLOGY EMPLOYED:
          ! 0.  Determine if simulation has downstepped and readjust history and revert node results
          ! 1.  Determine zone load - this is zone temperature dependent
          ! 2.  Determine balance point - the temperature at which the
          !     zone load is balanced by the system output. The way the
          !     balance point is determined will be different depending on
          !     the type of system being simulated.
          ! 3.  Calculate zone energy requirements

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:

  USE DataRoomAirModel,     ONLY: AirModel,RoomAirModel_Mixing,RoomAirModel_UCSDDV,IsZoneDV,ZoneDVMixedFlag,IsZoneUI, &
                                  ZTFloor,MATFloor,XMATFloor,XM2TFloor,XM3TFloor,XM4TFloor, &
                                  ZTOC,ZTM1OC,MATOC,XMATOC,XM2TOC,XM3TOC,XM4TOC, &
                                  ZTMX,ZTM1MX,MATMX,XMATMX,XM2TMX,XM3TMX,XM4TMX, RoomAirModel_Mundt, RoomAirModel_UserDefined

  USE General, ONLY: TrimSigDigits
  USE DataEnvironment, ONLY: Month, DayOfMonth
  USE DataLoopNode, ONLY: Node
  USE ScheduleManager, ONLY: GetCurrentScheduleValue
  USE DataGlobals,     ONLY: CurrentTime
  Use DataEnvironment, ONLY: DayOfYear

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
  REAL(r64)      :: SumIntGain                  ! Zone sum of convective internal gains
  REAL(r64)      :: SumHA                       ! Zone sum of Hc*Area
  REAL(r64)      :: SumHATsurf                  ! Zone sum of Hc*Area*Tsurf
  REAL(r64)      :: SumHATref                   ! Zone sum of Hc*Area*Tref, for ceiling diffuser convection correlation
  REAL(r64)      :: SumMCp                      ! Zone sum of MassFlowRate*Cp
  REAL(r64)      :: SumMCpT                     ! Zone sum of MassFlowRate*Cp*T
  REAL(r64)      :: SumSysMCp                   ! Zone sum of air system MassFlowRate*Cp
  REAL(r64)      :: SumSysMCpT                  ! Zone sum of air system MassFlowRate*Cp*T
  REAL(r64)      :: TempDepCoef                 ! Formerly CoefSumha
  REAL(r64)      :: TempIndCoef                 ! Formerly CoefSumhat
  REAL(r64)      :: AirCap                      ! Formerly CoefAirrat
!unused1208  REAL(r64)      :: TimeStepSeconds
  REAL(r64)      :: TempHistoryTerm
  INTEGER        :: ZoneNum
  REAL(r64)      :: ZoneT                       ! Zone temperature at previous time step
  INTEGER        :: RelativeZoneNum
  INTEGER        :: ActualZoneNum
  INTEGER :: I
  INTEGER :: Itemp
  REAL(r64)      :: SetpointOffset


  ! Staged thermostat setpoint
  IF (NumStageCtrZone > 0) THEN
    DO RelativeZoneNum = 1, NumStageCtrZone
      ActualZoneNum = StageControlledZone(RelativeZoneNum)%ActualZoneNum
      ZoneT = MAT(ActualZoneNum)
      IF (ShortenTimeStepSys) ZoneT = XMPT(ActualZoneNum)
      StageControlledZone(RelativeZoneNum)%HeatSetpoint=GetCurrentScheduleValue(StageControlledZone(RelativeZoneNum)%HSBchedIndex)
      StageControlledZone(RelativeZoneNum)%CoolSetpoint=GetCurrentScheduleValue(StageControlledZone(RelativeZoneNum)%CSBchedIndex)
      If (StageControlledZone(RelativeZoneNum)%HeatSetpoint .GE. StageControlledZone(RelativeZoneNum)%CoolSetpoint) Then
        StageControlledZone(RelativeZoneNum)%StageErrCount = StageControlledZone(RelativeZoneNum)%StageErrCount + 1
        if (StageControlledZone(RelativeZoneNum)%StageErrCount < 2) then
          CALL ShowWarningError('ZoneControl:Thermostat:StagedDualSetpoint: The heating setpoint is equal to or above '// &
              'the cooling setpoint in '//TRIM(StageControlledZone(RelativeZoneNum)%Name))
          CALL ShowContinueError('The zone heating setpoint is set to the cooling setpoint - 0.1C.')
          CALL ShowContinueErrorTimeStamp(' Occurrence info: ')
        else
          CALL ShowRecurringWarningErrorAtEnd('The heating setpoint is still above '// &
              'the cooling setpoint',StageControlledZone(RelativeZoneNum)%StageErrIndex, &
               StageControlledZone(RelativeZoneNum)%HeatSetpoint, StageControlledZone(RelativeZoneNum)%HeatSetpoint)
        end if
        StageControlledZone(RelativeZoneNum)%HeatSetpoint = StageControlledZone(RelativeZoneNum)%CoolSetpoint-0.1 !???????????
      End If
      ! Determine either cooling or heating
      If (StageControlledZone(RelativeZoneNum)%CoolSetpoint < ZoneT) Then ! Cooling
        SetpointOffset = ZoneT - StageControlledZone(RelativeZoneNum)%CoolSetpoint
        Itemp = 0
        Do I=1, StageControlledZone(RelativeZoneNum)%NumOfCoolStages
          If (SetpointOffset >=  StageControlledZone(RelativeZoneNum)%CoolTOffset(I)) Then
            Itemp = -I
          End If
        End Do
        ZoneSysEnergyDemand(ActualZoneNum)%StageNum = Itemp
        If (SetpointOffset >= 0.5d0* StageControlledZone(RelativeZoneNum)%CoolThroRange) Then
          ZoneThermostatSetPointHi(ActualZoneNum) = StageControlledZone(RelativeZoneNum)%CoolSetpoint - &
             0.5d0*StageControlledZone(RelativeZoneNum)%CoolThroRange
        Else
          ZoneThermostatSetPointHi(ActualZoneNum) = StageControlledZone(RelativeZoneNum)%CoolSetpoint + &
             0.5d0*StageControlledZone(RelativeZoneNum)%CoolThroRange
        End If
        ZoneThermostatSetPointLo(ActualZoneNum) = ZoneThermostatSetPointHi(ActualZoneNum)
      Else If (StageControlledZone(RelativeZoneNum)%HeatSetpoint > ZoneT) Then ! heating
        SetpointOffset = ZoneT - StageControlledZone(RelativeZoneNum)%HeatSetpoint
        Itemp = 0
        Do I=1,StageControlledZone(RelativeZoneNum)%NumOfHeatStages
          If (ABS(SetpointOffset) >=  ABS(StageControlledZone(RelativeZoneNum)%HeatTOffset(I))) Then
            Itemp = I
          End If
        End Do
        ZoneSysEnergyDemand(ActualZoneNum)%StageNum = Itemp
        If (ABS(SetpointOffset) >= 0.5d0* StageControlledZone(RelativeZoneNum)%CoolThroRange) Then
          ZoneThermostatSetPointLo(ActualZoneNum) = StageControlledZone(RelativeZoneNum)%HeatSetpoint + &
             0.5d0*StageControlledZone(RelativeZoneNum)%HeatThroRange
        Else
          ZoneThermostatSetPointLo(ActualZoneNum) = StageControlledZone(RelativeZoneNum)%HeatSetpoint - &
             0.5d0*StageControlledZone(RelativeZoneNum)%HeatThroRange
        End If
        ZoneThermostatSetPointHi(ActualZoneNum) = ZoneThermostatSetPointLo(ActualZoneNum)
      Else
        ZoneThermostatSetPointHi(ActualZoneNum) = StageControlledZone(RelativeZoneNum)%CoolSetpoint + &
                                                0.5* StageControlledZone(RelativeZoneNum)%CoolThroRange
        ZoneThermostatSetPointLo(ActualZoneNum) = StageControlledZone(RelativeZoneNum)%HeatSetpoint - &
                                                0.5* StageControlledZone(RelativeZoneNum)%HeatThroRange
        ZoneSysEnergyDemand(ActualZoneNum)%StageNum = 0
      End If
    End Do
  End If

  ! Update zone temperatures
  DO ZoneNum = 1, NumofZones

    IF (ShortenTimeStepSys) THEN  !
      ! timestep has just shifted from full zone timestep to a new shorter system timestep
      !throw away last updates in corrector and rewind for resimulating smaller timestep
      IF (Zone(ZoneNum)%SystemZoneNodeNumber > 0) THEN ! roll back result for zone air node,
        Node(Zone(ZoneNum)%SystemZoneNodeNumber)%Temp     = XMAT(ZoneNum)
        TempTstatAir(ZoneNum) = XMAT(ZoneNum)
        Node(Zone(ZoneNum)%SystemZoneNodeNumber)%HumRat   = WZoneTimeMinus1(ZoneNum)
        Node(Zone(ZoneNum)%SystemZoneNodeNumber)%Enthalpy = PsyHFnTdbW(XMAT(ZoneNum),WZoneTimeMinus1(ZoneNum))
      ENDIF

      IF (NumOfSysTimeSteps /= NumOfSysTimeStepsLastZoneTimeStep) THEN ! cannot reuse existing DS data, interpolate from zone time

        Call DownInterpolate4HistoryValues(PriorTimeStep,TimeStepSys, &
                           !  MAT(ZoneNum),   XMAT(ZoneNum),   XM2T(ZoneNum),   XM3T(ZoneNum),   XM4T(ZoneNum), &
                             XMAT(ZoneNum),  XM2T(ZoneNum),   XM3T(ZoneNum),   XM4T(ZoneNum), XM4T(ZoneNum),  &
                             MAT(ZoneNum), DSXMAT(ZoneNum), DSXM2T(ZoneNum), DSXM3T(ZoneNum), DSXM4T(ZoneNum))
        Call DownInterpolate4HistoryValues(PriorTimeStep,TimeStepSys, &
                        !     ZoneAirHumRat(ZoneNum),   WZoneTimeMinus1(ZoneNum),   WZoneTimeMinus2(ZoneNum),   &
                        !                                 WZoneTimeMinus3(ZoneNum),   WZoneTimeMinus4(ZoneNum), &
                            WZoneTimeMinus1(ZoneNum),   WZoneTimeMinus2(ZoneNum),   &
                                          WZoneTimeMinus3(ZoneNum),   WZoneTimeMinus4(ZoneNum),  WZoneTimeMinus4(ZoneNum),&
                             ZoneAirHumRat(ZoneNum), DSWZoneTimeMinus1(ZoneNum), DSWZoneTimeMinus2(ZoneNum), &
                             DSWZoneTimeMinus3(ZoneNum), DSWZoneTimeMinus4(ZoneNum))


        IF (IsZoneDV(ZoneNum) .or. IsZoneUI(ZoneNum)) THEN

          CALL DownInterpolate4HistoryValues(PriorTimeStep,TimeStepSys, &
                             !   MATFloor(ZoneNum),   XMATFloor(ZoneNum),    XM2TFloor(ZoneNum),  &
                             !                        XM3TFloor(ZoneNum),    XM4TFloor(ZoneNum) ,   &
                                XMATFloor(ZoneNum),    XM2TFloor(ZoneNum),  &
                                                     XM3TFloor(ZoneNum),    XM4TFloor(ZoneNum) ,  XM4TFloor(ZoneNum) , &
                                MATFloor(ZoneNum), DSXMATFloor(ZoneNum),  DSXM2TFloor(ZoneNum),  &
                                                   DSXM3TFloor(ZoneNum),  DSXM4TFloor(ZoneNum))
          CALL DownInterpolate4HistoryValues(PriorTimeStep,TimeStepSys, &
                          !      MATOC(ZoneNum),   XMATOC(ZoneNum),    XM2TOC(ZoneNum),  &
                          !                        XM3TOC(ZoneNum),    XM4TOC(ZoneNum) ,   &
                                XMATOC(ZoneNum),    XM2TOC(ZoneNum),  &
                                                  XM3TOC(ZoneNum),    XM4TOC(ZoneNum) ,   XM4TOC(ZoneNum) , &
                                MATOC(ZoneNum), DSXMATOC(ZoneNum),  DSXM2TOC(ZoneNum),  &
                                                DSXM3TOC(ZoneNum),  DSXM4TOC(ZoneNum))
          CALL DownInterpolate4HistoryValues(PriorTimeStep,TimeStepSys, &
                              !  MATMX(ZoneNum),   XMATMX(ZoneNum),    XM2TMX(ZoneNum),  &
                              !                    XM3TMX(ZoneNum),    XM4TMX(ZoneNum) ,   &
                                XMATMX(ZoneNum),    XM2TMX(ZoneNum),  &
                                                  XM3TMX(ZoneNum),    XM4TMX(ZoneNum) ,  XM4TMX(ZoneNum) , &
                                MATMX(ZoneNum), DSXMATMX(ZoneNum),  DSXM2TMX(ZoneNum),  &
                                                DSXM3TMX(ZoneNum),  DSXM4TMX(ZoneNum))
        ENDIF

      ELSE ! reuse history data in DS terms from last zone time step to preserve information that would be lost
         ! do nothing because DS history would have been pushed prior and should be ready

      ENDIF

    ENDIF
    ! now update the variables actually used in the balance equations.
    IF(UseZoneTimeStepHistory) THEN
      ZTM1(ZoneNum)                = XMAT(ZoneNum)
      ZTM2(ZoneNum)                = XM2T(ZoneNum)
      ZTM3(ZoneNum)                = XM3T(ZoneNum)

      WZoneTimeMinus1Temp(ZoneNum) = WZoneTimeMinus1(ZoneNum)
      WZoneTimeMinus2Temp(ZoneNum) = WZoneTimeMinus2(ZoneNum)
      WZoneTimeMinus3Temp(ZoneNum) = WZoneTimeMinus3(ZoneNum)

    ELSE  ! use down-stepped history
      ZTM1(ZoneNum)                = DSXMAT(ZoneNum)
      ZTM2(ZoneNum)                = DSXM2T(ZoneNum)
      ZTM3(ZoneNum)                = DSXM3T(ZoneNum)

      WZoneTimeMinus1Temp(ZoneNum) = DSWZoneTimeMinus1(ZoneNum)
      WZoneTimeMinus2Temp(ZoneNum) = DSWZoneTimeMinus2(ZoneNum)
      WZoneTimeMinus3Temp(ZoneNum) = DSWZoneTimeMinus3(ZoneNum)

    END IF

    AIRRAT(ZoneNum) = Zone(ZoneNum)%Volume*ZoneVolCapMultpSens* &
               PsyRhoAirFnPbTdbW(OutBaroPress,MAT(ZoneNum),ZoneAirHumRat(ZoneNum))* &
               PsyCpAirFnWTdb(ZoneAirHumRat(ZoneNum),MAT(ZoneNum))/(TimeStepSys*SecInHour)
    AirCap = AIRRAT(ZoneNum)


    ! Calculate the various heat balance sums

    ! NOTE: SumSysMCp and SumSysMCpT are not used in the predict step
    CALL CalcZoneSums(ZoneNum, SumIntGain, SumHA, SumHATsurf, SumHATref, SumMCp, SumMCpT, SumSysMCp, SumSysMCpT)

    TempDepCoef = SumHA + SumMCp
    TempIndCoef = SumIntGain + SumHATsurf - SumHATref + SumMCpT + SysDepZoneLoadsLagged(ZoneNum)
    IF (AirModel(ZoneNum)%AirModelType.EQ.RoomAirModel_Mixing) THEN
        TempHistoryTerm = AirCap * (3.0d0 * ZTM1(ZoneNum) - (3.0d0/2.0d0) * ZTM2(ZoneNum) + (1.0d0/3.0d0) * ZTM3(ZoneNum))
        TempDepZnLd(ZoneNum) = (11.0d0/6.0d0) * AirCap + TempDepCoef
        TempIndZnLd(ZoneNum) = TempHistoryTerm + TempIndCoef
    ELSEIF (IsZoneDV(ZoneNum)) THEN
       ! UCSD displacement ventilation model - make dynamic term independent of TimeStepSys
        TempHistoryTerm = AirCap * (3.0d0 * ZTM1(ZoneNum) - (3.0d0/2.0d0) * ZTM2(ZoneNum) + (1.0d0/3.0d0) * ZTM3(ZoneNum))
        TempDepZnLd(ZoneNum) = (11.0d0/6.0d0) * AirCap + TempDepCoef
        TempIndZnLd(ZoneNum) = TempHistoryTerm + TempIndCoef
    ELSEIF (IsZoneUI(ZoneNum)) THEN
       ! UCSD UFAD model - make dynamic term independent of TimeStepSys
        TempHistoryTerm = AirCap * (3.0d0 * ZTM1(ZoneNum) - (3.0d0/2.0d0) * ZTM2(ZoneNum) + (1.0d0/3.0d0) * ZTM3(ZoneNum))
        TempDepZnLd(ZoneNum) = (11.0d0/6.0d0) * AirCap + TempDepCoef
        TempIndZnLd(ZoneNum) = TempHistoryTerm + TempIndCoef
    ELSE ! other imperfectly mixed room models
        TempHistoryTerm = AirCap * (3.0d0 * ZTM1(ZoneNum) - (3.0d0/2.0d0) * ZTM2(ZoneNum) + (1.0d0/3.0d0) * ZTM3(ZoneNum))
        TempDepZnLd(ZoneNum) = (11.0d0/6.0d0) * AirCap + TempDepCoef
        TempIndZnLd(ZoneNum) = TempHistoryTerm + TempIndCoef
    END IF

    ! Exact solution or Euler method
    ShortenTimeStepSysRoomAir = .FALSE.
    If (ZoneAirSolutionAlgo .NE. Use3rdOrder) Then
      If (ShortenTimeStepSys .and. TimeStepSys .LT. TimeStepZone) Then
        If (PreviousTimeStep < TimeStepZone) Then
          ZoneT1(ZoneNum) = ZoneTM2(ZoneNum)
          ZoneW1(ZoneNum) = ZoneWM2(ZoneNum)
        Else
          ZoneT1(ZoneNum) = ZoneTMX(ZoneNum)
          ZoneW1(ZoneNum) = ZoneWMX(ZoneNum)
        End If
        ShortenTimeStepSysRoomAir = .TRUE.
      Else
        ZoneT1(ZoneNum) = ZT(ZoneNum)
        ZoneW1(ZoneNum) = ZoneAirHumRat(ZoneNum)
      End If
      TempDepZnLd(ZoneNum) = TempDepCoef
      TempIndZnLd(ZoneNum) = TempIndCoef
    End If

    ! Calculate the predicted zone load to be provided by the system with the given desired zone air temperature
    CALL CalcPredictedSystemLoad(ZoneNum)

    ! Calculate the predicted zone load to be provided by the system with the given desired humidity ratio
    CALL CalcPredictedHumidityRatio(ZoneNum)

  END DO

  RETURN

END SUBROUTINE PredictSystemLoads

SUBROUTINE CalcZoneAirTempSetpoints

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Russ Taylor
          !       DATE WRITTEN   Nov 1997
          !       MODIFIED       Aug 2013, Xiufeng Pang (XP) - Added code for updating set points during
          !                      optimum start period
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This routine sets what the setpoints for each controlled zone should be based on schedules.
          ! This is called each time step.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE ScheduleManager, ONLY: GetCurrentScheduleValue, GetScheduleValuesForDay
  USE General, ONLY: TrimSigDigits
  USE DataZoneControls, ONLY: OccRoomTSetPointHeat, OccRoomTSetPointCool

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
  INTEGER :: RelativeZoneNum
  INTEGER :: ActualZoneNum
  INTEGER :: TempControlSchedIndex
  INTEGER :: SetPointTempSchedIndex
  INTEGER :: SetPointTempSchedIndexHot
  INTEGER :: SetPointTempSchedIndexCold
  INTEGER :: SchedNameIndex
  INTEGER :: SchedTypeIndex
  REAL(r64), ALLOCATABLE, DIMENSION(:,:) :: DaySPValues !Day room temp setpoint values - for optimum start
  REAL(r64) :: OccRoomSP !Occupied room temp set point - for optimum start
  INTEGER :: OccStartTime ! Occupancy start time - for optimum start

          ! FLOW:
   TempControlType = 0 ! Default

   ! Place holder for occupied heating and cooling set points - for optimum start
   IF (.NOT. ALLOCATED(OccRoomTSetPointHeat)) THEN
        ALLOCATE(OccRoomTSetPointHeat(NumOfZones))
   END IF
   IF (.NOT. ALLOCATED(OccRoomTSetPointCool)) THEN
        ALLOCATE(OccRoomTSetPointCool(NumOfZones))
   END IF
   OccRoomTSetPointHeat = 0.0d0
   OccRoomTSetPointCool = 100.0d0

   DO RelativeZoneNum = 1, NumTempControlledZones

    ! What if this zone not controlled???
    ActualZoneNum = TempControlledZone(RelativeZoneNum)%ActualZoneNum
    TempControlSchedIndex = TempControlledZone(RelativeZoneNum)%CTSchedIndex
    TempControlType(ActualZoneNum) = GetCurrentScheduleValue(TempControlSchedIndex)

    ! Error detection for these values is done in the Get routine

    SELECT CASE (TempControlType(ActualZoneNum)) ! Is this missing the possibility of sometimes having no control on a zone
                                                 ! during the simulation?
      CASE (0) ! Uncontrolled

      CASE (SingleHeatingSetPoint)

        SchedNameIndex = TempControlledZone(RelativeZoneNum)%SchIndx_SingleHeatSetPoint

        SchedTypeIndex = TempControlledZone(RelativeZoneNum)%ControlTypeSchIndx(SchedNameIndex)

        SetPointTempSchedIndex = SetPointSingleHeating(SchedTypeIndex)%TempSchedIndex
        TempZoneThermostatSetPoint(ActualZoneNum) = GetCurrentScheduleValue(SetPointTempSchedIndex)
        Call AdjustAirSetpointsforOpTempCntrl(RelativeZoneNum, ActualZoneNum, TempZoneThermostatSetPoint(ActualZoneNum))

        ZoneThermostatSetPointLo(ActualZoneNum) = TempZoneThermostatSetPoint(ActualZoneNum)
!        ZoneThermostatSetPointHi(ActualZoneNum) = TempZoneThermostatSetPoint(ActualZoneNum)

      CASE (SingleCoolingSetPoint)

        SchedNameIndex = TempControlledZone(RelativeZoneNum)%SchIndx_SingleCoolSetPoint

        SchedTypeIndex = TempControlledZone(RelativeZoneNum)%ControlTypeSchIndx(SchedNameIndex)

        SetPointTempSchedIndex = SetPointSingleCooling(SchedTypeIndex)%TempSchedIndex
        TempZoneThermostatSetPoint(ActualZoneNum) = GetCurrentScheduleValue(SetPointTempSchedIndex)
        Call AdjustAirSetpointsforOpTempCntrl(RelativeZoneNum, ActualZoneNum, TempZoneThermostatSetPoint(ActualZoneNum))
        ZoneThermostatSetPointHi(ActualZoneNum) = TempZoneThermostatSetPoint(ActualZoneNum)
!        ZoneThermostatSetPointLo(ActualZoneNum) = TempZoneThermostatSetPoint(ActualZoneNum)

        CALL AdjustCoolingSetPointforTempAndHumidityControl(RelativeZoneNum,ActualZoneNum)

      CASE (SingleHeatCoolSetPoint)

        SchedNameIndex = TempControlledZone(RelativeZoneNum)%SchIndx_SingleHeatCoolSetPoint

        SchedTypeIndex = TempControlledZone(RelativeZoneNum)%ControlTypeSchIndx(SchedNameIndex)

        SetPointTempSchedIndex = SetPointSingleHeatCool(SchedTypeIndex)%TempSchedIndex
        TempZoneThermostatSetPoint(ActualZoneNum) = GetCurrentScheduleValue(SetPointTempSchedIndex)
        Call AdjustAirSetpointsforOpTempCntrl(RelativeZoneNum, ActualZoneNum, TempZoneThermostatSetPoint(ActualZoneNum))

        ZoneThermostatSetPointHi(ActualZoneNum) = TempZoneThermostatSetPoint(ActualZoneNum)
        ZoneThermostatSetPointLo(ActualZoneNum) = TempZoneThermostatSetPoint(ActualZoneNum)

        !Change the room set point to occupied set point during optimum start period--------------

         IF (Allocated(OptStartData%OptStartFlag)) THEN
            IF (.not. allocated(DaySPValues)) THEN
                ALLOCATE (DaySPValues(24,NumOfTimeStepInHour))
            END IF
            IF (OptStartData%ActualZoneNum(ActualZoneNum)==ActualZoneNum) THEN
              Call GetScheduleValuesForDay(SetPointTempSchedIndexCold,DaySPValues)
              OccStartTime = ceiling(OptStartData%OccStartTime(ActualZoneNum)) + 1
              TempZoneThermostatSetPoint(ActualZoneNum)=DaySPValues(OccStartTime,1)
            END IF

        IF (OptStartData%OptStartFlag(ActualZoneNum)) THEN
           ZoneThermostatSetPointHi(ActualZoneNum) = TempZoneThermostatSetPoint(ActualZoneNum)
           ZoneThermostatSetPointLo(ActualZoneNum) = TempZoneThermostatSetPoint(ActualZoneNum)
        END IF
        END IF
        !--------------------------------------------------------------------------------------------


      CASE (DualSetPointWithDeadBand)

        SchedNameIndex = TempControlledZone(RelativeZoneNum)%SchIndx_DualSetPointWDeadBand

        SchedTypeIndex = TempControlledZone(RelativeZoneNum)%ControlTypeSchIndx(SchedNameIndex)

        SetPointTempSchedIndexHot = SetPointDualHeatCool(SchedTypeIndex)%HeatTempSchedIndex
        SetPointTempSchedIndexCold = SetPointDualHeatCool(SchedTypeIndex)%CoolTempSchedIndex
        ZoneThermostatSetPointHi(ActualZoneNum) = GetCurrentScheduleValue(SetPointTempSchedIndexCold)
        Call AdjustAirSetpointsforOpTempCntrl(RelativeZoneNum, ActualZoneNum, ZoneThermostatSetPointHi(ActualZoneNum))

        ZoneThermostatSetPointLo(ActualZoneNum) = GetCurrentScheduleValue(SetPointTempSchedIndexHot)
        Call AdjustAirSetpointsforOpTempCntrl(RelativeZoneNum, ActualZoneNum, ZoneThermostatSetPointLo(ActualZoneNum))

        !Change the room set point to occupied set point during optimum start period--------------

        IF (Allocated(OptStartData%OptStartFlag)) THEN
            IF (.not. allocated(DaySPValues)) THEN
                ALLOCATE (DaySPValues(24,NumOfTimeStepInHour))
            END IF
            IF (OptStartData%ActualZoneNum(ActualZoneNum)==ActualZoneNum) THEN
              Call GetScheduleValuesForDay(SetPointTempSchedIndexCold,DaySPValues)
              OccStartTime = ceiling(OptStartData%OccStartTime(ActualZoneNum)) + 1
              OccRoomTSetPointCool(ActualZoneNum) = DaySPValues(OccStartTime,1)
              Call GetScheduleValuesForDay(SetPointTempSchedIndexHot,DaySPValues)
              OccRoomTSetPointHeat(ActualZoneNum) = DaySPValues(OccStartTime,1)
            END IF

        IF (OptStartData%OptStartFlag(ActualZoneNum)) THEN
           ZoneThermostatSetPointHi(ActualZoneNum) = OccRoomTSetPointCool(ActualZoneNum)
           ZoneThermostatSetPointLo(ActualZoneNum) = OccRoomTSetPointHeat(ActualZoneNum)
        END IF
        END IF
        !--------------------------------------------------------------------------------------------

        CALL AdjustCoolingSetPointforTempAndHumidityControl(RelativeZoneNum,ActualZoneNum)

      CASE DEFAULT
        CALL ShowSevereError('CalcZoneAirTempSetpoints: Illegal control type for Zone='//TRIM(Zone(ActualZoneNum)%Name)//  &
                               ', Found value='//TRIM(TrimSigDigits(TempControlType(ActualZoneNum)))//  &
                               ', in Schedule='//TRIM(TempControlledZone(RelativeZoneNum)%ControlTypeSchedName))

    END SELECT

  END DO

  If (NumComfortControlledZones > 0) Call CalcZoneAirComfortSetpoints
  RETURN

END SUBROUTINE CalcZoneAirTempSetpoints

SUBROUTINE CalcPredictedSystemLoad(ZoneNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Russ Taylor
          !       DATE WRITTEN   Nov 1997
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine calculates the predicted system load for a time step.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE ScheduleManager, ONLY: GetCurrentScheduleValue
  USE DataLoopNode, ONLY: Node
  USE General, ONLY: RoundSigDigits

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)  :: ZoneNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64) :: LoadToHeatingSetPoint
  REAL(r64) :: LoadToCoolingSetPoint
  REAL(r64) :: ZoneSetPoint

          ! FLOW:
  DeadBandOrSetback(ZoneNum) = .FALSE.
  ZoneSetPoint = 0.0d0
  LoadToHeatingSetPoint=0.0d0
  LoadToCoolingSetPoint=0.0d0

  SELECT CASE (TempControlType(ZoneNum))

    CASE (0)
      ! Uncontrolled Zone
      LoadToHeatingSetPoint = 0.0d0
      LoadToCoolingSetPoint = 0.0d0
      ZoneSysEnergyDemand(ZoneNum)%TotalOutputRequired = 0.0d0

    CASE (SingleHeatingSetPoint)
      ! Determine zone load based on
      ! Qload + Qsys = 0 and Qsys = mCp(Tsys-Tzone)
      ! System Load Sign Convention:
      !     - -> Cooling required to reach setpoint
      !     + -> Heating required to reach setpoint

!PH 3/2/04      LoadToHeatingSetPoint = (TempDepZnLd(ZoneNum) * TempZoneThermostatSetPoint(ZoneNum) - TempIndZnLd(ZoneNum))
      SELECT CASE (ZoneAirSolutionAlgo)
        CASE (Use3rdOrder)
          LoadToHeatingSetPoint = (TempDepZnLd(ZoneNum) * TempZoneThermostatSetPoint(ZoneNum) - TempIndZnLd(ZoneNum))
      ! Exact solution
        CASE (UseAnalyticalSolution)
          If (TempDepZnLd(ZoneNum) .eq. 0.0d0) Then ! B=0
            LoadToHeatingSetPoint = AIRRAT(ZoneNum)*(TempZoneThermostatSetPoint(ZoneNum) - &
                                ZoneT1(ZoneNum)) - TempIndZnLd(ZoneNum)
          Else
            LoadToHeatingSetPoint = TempDepZnLd(ZoneNum)*(TempZoneThermostatSetPoint(ZoneNum) &
            -ZoneT1(ZoneNum)*exp(MIN(700.d0,-TempDepZnLd(ZoneNum)/AIRRAT(ZoneNum))))/ &
            (1.0d0-exp(MIN(700.d0,-TempDepZnLd(ZoneNum)/AIRRAT(ZoneNum))))-TempIndZnLd(ZoneNum)
          End If
        CASE (UseEulerMethod)
          LoadToHeatingSetPoint = AIRRAT(ZoneNum)*(TempZoneThermostatSetPoint(ZoneNum)- &
                                ZoneT1(ZoneNum)) + TempDepZnLd(ZoneNum)*(TempZoneThermostatSetPoint(ZoneNum)) &
                                - TempIndZnLd(ZoneNum)
      END SELECT
      ZoneSysEnergyDemand(ZoneNum)%TotalOutputRequired = LoadToHeatingSetPoint
      ZoneSetPoint = TempZoneThermostatSetPoint(ZoneNum)
      LoadToCoolingSetPoint = LoadToHeatingSetPoint
        ! for consistency with the other cases, use LE instead of LT and don't subtract 1.0 Watt as a way of pushing the zero load
        ! case over the threshold
      IF ((ZoneSysEnergyDemand(ZoneNum)%TotalOutputRequired) .LE. 0.0d0) DeadBandOrSetback(ZoneNum) = .TRUE.

    CASE (SingleCoolingSetPoint)

!PH 3/2/04      LoadToCoolingSetPoint = (TempDepZnLd(ZoneNum) * TempZoneThermostatSetPoint(ZoneNum) - TempIndZnLd(ZoneNum))
      SELECT CASE (ZoneAirSolutionAlgo)
        CASE (Use3rdOrder)
          LoadToCoolingSetPoint = (TempDepZnLd(ZoneNum) * &
                              (TempZoneThermostatSetPoint(ZoneNum)) - TempIndZnLd(ZoneNum))
        CASE (UseAnalyticalSolution)
          If (TempDepZnLd(ZoneNum) .eq. 0.0d0) Then ! B=0
            LoadToCoolingSetPoint = AIRRAT(ZoneNum)*(TempZoneThermostatSetPoint(ZoneNum)- &
                                ZoneT1(ZoneNum)) - TempIndZnLd(ZoneNum)
          Else
            LoadToCoolingSetPoint = TempDepZnLd(ZoneNum)*(TempZoneThermostatSetPoint(ZoneNum) &
                  -ZoneT1(ZoneNum)*exp(MIN(700.d0,-TempDepZnLd(ZoneNum)/AIRRAT(ZoneNum)))) &
                                /(1.0d0-exp(MIN(700.d0,-TempDepZnLd(ZoneNum)/AIRRAT(ZoneNum)))) - TempIndZnLd(ZoneNum)
          End If
        CASE (UseEulerMethod)
          LoadToCoolingSetPoint = AIRRAT(ZoneNum)*(TempZoneThermostatSetPoint(ZoneNum)- &
                                ZoneT1(ZoneNum)) + TempDepZnLd(ZoneNum)*TempZoneThermostatSetPoint(ZoneNum) - TempIndZnLd(ZoneNum)
      END SELECT
      ZoneSysEnergyDemand(ZoneNum)%TotalOutputRequired = LoadToCoolingSetPoint
      ZoneSetPoint = TempZoneThermostatSetPoint(ZoneNum)
      LoadToHeatingSetPoint = LoadToCoolingSetPoint
        ! for consistency with the other cases, use GE instead of GT and don't add 1.0 Watt as a way of pushing the zero load
        ! case over the threshold
      IF ((ZoneSysEnergyDemand(ZoneNum)%TotalOutputRequired) .GE. 0.0d0) DeadBandOrSetback(ZoneNum) = .TRUE.

    CASE (SingleHeatCoolSetPoint)

!PH 3/2/04      LoadToHeatingSetPoint = (TempDepZnLd(ZoneNum) * TempZoneThermostatSetPoint(ZoneNum) - TempIndZnLd(ZoneNum))
!PH 3/2/04      !LoadToCoolingSetPoint = (TempDepZnLd(ZoneNum) * TempZoneThermostatSetPoint(ZoneNum) - TempIndZnLd(ZoneNum))
      SELECT CASE (ZoneAirSolutionAlgo)
        CASE (Use3rdOrder)
          LoadToHeatingSetPoint = (TempDepZnLd(ZoneNum) * &
                              (TempZoneThermostatSetPoint(ZoneNum)) - TempIndZnLd(ZoneNum))
          LoadToCoolingSetPoint = (TempDepZnLd(ZoneNum) * &
                              (TempZoneThermostatSetPoint(ZoneNum)) - TempIndZnLd(ZoneNum))
      ! Exact solution
        CASE (UseAnalyticalSolution)
          If (TempDepZnLd(ZoneNum) .eq. 0.0d0) Then ! B=0
            LoadToHeatingSetPoint = AIRRAT(ZoneNum)*(TempZoneThermostatSetPoint(ZoneNum)- &
                                ZoneT1(ZoneNum)) - TempIndZnLd(ZoneNum)
            LoadToCoolingSetPoint = AIRRAT(ZoneNum)*(TempZoneThermostatSetPoint(ZoneNum)- &
                                ZoneT1(ZoneNum)) - TempIndZnLd(ZoneNum)
          Else
            LoadToHeatingSetPoint = TempDepZnLd(ZoneNum)*(TempZoneThermostatSetPoint(ZoneNum) &
                         -ZoneT1(ZoneNum)*exp(MIN(700.d0,-TempDepZnLd(ZoneNum)/AIRRAT(ZoneNum)))) &
                                  /(1.0d0-exp(MIN(700.d0,-TempDepZnLd(ZoneNum)/AIRRAT(ZoneNum)))) - TempIndZnLd(ZoneNum)
            LoadToCoolingSetPoint = TempDepZnLd(ZoneNum)*(TempZoneThermostatSetPoint(ZoneNum) &
                         -ZoneT1(ZoneNum)*exp(MIN(700.d0,-TempDepZnLd(ZoneNum)/AIRRAT(ZoneNum)))) &
                                /(1.0d0-exp(MIN(700.d0,-TempDepZnLd(ZoneNum)/AIRRAT(ZoneNum)))) - TempIndZnLd(ZoneNum)
          End If
        CASE (UseEulerMethod)
          LoadToHeatingSetPoint = AIRRAT(ZoneNum)*(TempZoneThermostatSetPoint(ZoneNum) - &
                                ZoneT1(ZoneNum)) + TempDepZnLd(ZoneNum)*TempZoneThermostatSetPoint(ZoneNum) - TempIndZnLd(ZoneNum)
          LoadToCoolingSetPoint = AIRRAT(ZoneNum)*(TempZoneThermostatSetPoint(ZoneNum) - &
                                ZoneT1(ZoneNum)) + TempDepZnLd(ZoneNum)*TempZoneThermostatSetPoint(ZoneNum) - TempIndZnLd(ZoneNum)
      END SELECT
      ZoneSetPoint = TempZoneThermostatSetPoint(ZoneNum)

!PH 3/2/04      ZoneSysEnergyDemand(ZoneNum)%TotalOutputRequired = LoadToHeatingSetPoint ! = LoadToCoolingSetPoint
! Note that LoadToHeatingSetPoint is generally not equal to LoadToCoolingSetPoint
! when the heating and cooling set-points are equal if the zone is unmixed,
! e.g. displacement ventilation or UFAD, since the stratification is generally not the same in heating and cooling modes

      ! Possible combinations:
      ! 1/  LoadToHeatingSetPoint > 0 & LoadToCoolingSetPoint > 0 -->  Heating required
      ! 2/  LoadToHeatingSetPoint  >  LoadToCoolingSetPoint       -->  Possible in the unmixed case but should be trapped
      !                                                                 as a poor choice of set-points
      ! 3/  LoadToHeatingSetPoint < 0 & LoadToCoolingSetPoint < 0 -->  Cooling Required
      ! 4/  LoadToHeatingSetPoint <=0 & LoadToCoolingSetPoint >=0 -->  Dead Band Operation ! includes zero load cases
! First trap bad set-points
      IF (LoadToHeatingSetPoint .GT. LoadToCoolingSetPoint ) THEN
         CALL ShowSevereError('SingleHeatCoolSetPoint: Effective heating set-point higher than effective cooling set-point - '// &
                             'use DualSetPointWithDeadBand if using unmixed air model')
         CALL ShowContinueErrorTimeStamp('occurs in Zone='//TRIM(Zone(ZoneNum)%Name))
         CALL ShowContinueError('LoadToHeatingSetPoint='//TRIM(RoundSigDigits(LoadToHeatingSetPoint,3))//  &
                                ', LoadToCoolingSetPoint='//TRIM(RoundSigDigits(LoadToCoolingSetPoint,3)))
         CALL ShowContinueError('Zone TempDepZnLd='//TRIM(RoundSigDigits(TempDepZnLd(ZoneNum),2)))
         CALL ShowContinueError('Zone TempIndZnLd='//TRIM(RoundSigDigits(TempIndZnLd(ZoneNum),2)))
         CALL ShowContinueError('Zone ThermostatSetPoint='//TRIM(RoundSigDigits(TempZoneThermostatSetPoint(ZoneNum),2)))
         CALL ShowFatalError('Program terminates due to above conditions.')
      END IF

      IF (LoadToHeatingSetPoint .GT. 0.0d0 .AND. LoadToCoolingSetPoint .GT. 0.0d0) THEN
         ZoneSysEnergyDemand(ZoneNum)%TotalOutputRequired = LoadToHeatingSetPoint
      ELSE IF (LoadToHeatingSetPoint .LT. 0.0d0 .AND. LoadToCoolingSetPoint .LT. 0.0d0) THEN
         ZoneSysEnergyDemand(ZoneNum)%TotalOutputRequired = LoadToCoolingSetPoint
      ELSE IF (LoadToHeatingSetPoint .LE. 0.0d0 .AND. LoadToCoolingSetPoint .GE. 0.0d0) THEN ! deadband includes zero loads
         ZoneSysEnergyDemand(ZoneNum)%TotalOutputRequired = 0.0d0
         IF(Zone(ZoneNum)%SystemZoneNodeNumber > 0) THEN
            ZoneSetPoint = Node(Zone(ZoneNum)%SystemZoneNodeNumber)%Temp
            ZoneSetPoint = MAX(ZoneSetPoint, ZoneThermostatSetPointLo(ZoneNum)) ! trap out of deadband
            ZoneSetPoint = MIN(ZoneSetPoint, ZoneThermostatSetPointHi(ZoneNum)) ! trap out of deadband
         END IF
         DeadBandOrSetback(ZoneNum) = .TRUE.
      ELSE  ! this should never occur!
         CALL ShowSevereError('SingleHeatCoolSetPoint: Unanticipated combination of heating and cooling loads - '// &
                             'report to EnergyPlus Development Team')
         CALL ShowContinueErrorTimeStamp('occurs in Zone='//TRIM(Zone(ZoneNum)%Name))
         CALL ShowContinueError('LoadToHeatingSetPoint='//TRIM(RoundSigDigits(LoadToHeatingSetPoint,3))//  &
                                ', LoadToCoolingSetPoint='//TRIM(RoundSigDigits(LoadToCoolingSetPoint,3)))
         CALL ShowContinueError('Zone TempDepZnLd='//TRIM(RoundSigDigits(TempDepZnLd(ZoneNum),2)))
         CALL ShowContinueError('Zone TempIndZnLd='//TRIM(RoundSigDigits(TempIndZnLd(ZoneNum),2)))
         CALL ShowContinueError('Zone ThermostatSetPoint='//TRIM(RoundSigDigits(TempZoneThermostatSetPoint(ZoneNum),2)))
         CALL ShowFatalError('Program terminates due to above conditions.')
      END IF

    CASE (DualSetPointWithDeadBand)

      SELECT CASE (ZoneAirSolutionAlgo)
        CASE (Use3rdOrder)
          LoadToHeatingSetPoint = (TempDepZnLd(ZoneNum) * &
                              (ZoneThermostatSetPointLo(ZoneNum)) - TempIndZnLd(ZoneNum))
          LoadToCoolingSetPoint = (TempDepZnLd(ZoneNum) * &
                              (ZoneThermostatSetPointHi(ZoneNum)) - TempIndZnLd(ZoneNum))
      ! Exact solution
        CASE (UseAnalyticalSolution)
          If (TempDepZnLd(ZoneNum) .eq. 0.0d0) Then ! B=0
            LoadToHeatingSetPoint = AIRRAT(ZoneNum)*(ZoneThermostatSetPointLo(ZoneNum)- &
                                ZoneT1(ZoneNum)) - TempIndZnLd(ZoneNum)
            LoadToCoolingSetPoint = AIRRAT(ZoneNum)*(ZoneThermostatSetPointHi(ZoneNum)- &
                                ZoneT1(ZoneNum)) - TempIndZnLd(ZoneNum)
          Else
            LoadToHeatingSetPoint = TempDepZnLd(ZoneNum)*(ZoneThermostatSetPointLo(ZoneNum) &
                    -ZoneT1(ZoneNum)*exp(MIN(700.d0,-TempDepZnLd(ZoneNum)/AIRRAT(ZoneNum))))/ &
                                (1.0d0-exp(MIN(700.d0,-TempDepZnLd(ZoneNum)/AIRRAT(ZoneNum)))) - TempIndZnLd(ZoneNum)
            LoadToCoolingSetPoint = TempDepZnLd(ZoneNum)*(ZoneThermostatSetPointHi(ZoneNum) &
                    -ZoneT1(ZoneNum)*exp(MIN(700.d0,-TempDepZnLd(ZoneNum)/AIRRAT(ZoneNum))))/ &
                                (1.0d0-exp(MIN(700.d0,-TempDepZnLd(ZoneNum)/AIRRAT(ZoneNum)))) - TempIndZnLd(ZoneNum)
          End If
        CASE (UseEulerMethod)
          LoadToHeatingSetPoint = AIRRAT(ZoneNum)*(ZoneThermostatSetPointLo(ZoneNum)- &
                                ZoneT1(ZoneNum)) + TempDepZnLd(ZoneNum)*ZoneThermostatSetPointLo(ZoneNum) - TempIndZnLd(ZoneNum)
          LoadToCoolingSetPoint = AIRRAT(ZoneNum)*(ZoneThermostatSetPointHi(ZoneNum)- &
                                ZoneT1(ZoneNum)) + TempDepZnLd(ZoneNum)*ZoneThermostatSetPointHi(ZoneNum) - TempIndZnLd(ZoneNum)
      END SELECT

      ! Possible combinations:
      ! 1/  LoadToHeatingSetPoint > 0 & LoadToCoolingSetPoint > 0 -->  Heating required
      ! 2/  LoadToHeatingSetPoint  >  LoadToCoolingSetPoint       -->  Possible in the unmixed case but should be trapped
      !                                                                  as a poor choice of set-points
      ! 3/  LoadToHeatingSetPoint < 0 & LoadToCoolingSetPoint < 0 -->  Cooling Required
      ! 4/  LoadToHeatingSetPoint <=0 & LoadToCoolingSetPoint >=0 -->  Dead Band Operation - includes zero load cases
! First trap bad set-points
      IF (LoadToHeatingSetPoint .GT. LoadToCoolingSetPoint ) THEN
         CALL ShowSevereError('DualSetPointWithDeadBand: Effective heating set-point higher than effective cooling set-point - '// &
                             'increase deadband if using unmixed air model')
         CALL ShowContinueErrorTimeStamp('occurs in Zone='//TRIM(Zone(ZoneNum)%Name))
         CALL ShowContinueError('LoadToHeatingSetPoint='//TRIM(RoundSigDigits(LoadToHeatingSetPoint,3))//  &
                                ', LoadToCoolingSetPoint='//TRIM(RoundSigDigits(LoadToCoolingSetPoint,3)))
         CALL ShowContinueError('Zone TempDepZnLd='//TRIM(RoundSigDigits(TempDepZnLd(ZoneNum),2)))
         CALL ShowContinueError('Zone TempIndZnLd='//TRIM(RoundSigDigits(TempIndZnLd(ZoneNum),2)))
         CALL ShowContinueError('Zone Heating ThermostatSetPoint='//TRIM(RoundSigDigits(ZoneThermostatSetPointLo(ZoneNum),2)))
         CALL ShowContinueError('Zone Cooling ThermostatSetPoint='//TRIM(RoundSigDigits(ZoneThermostatSetPointHi(ZoneNum),2)))
         CALL ShowFatalError('Program terminates due to above conditions.')
      END IF
      IF (LoadToHeatingSetPoint .GT. 0.0d0 .AND. LoadToCoolingSetPoint .GT. 0.0d0) THEN
         ZoneSysEnergyDemand(ZoneNum)%TotalOutputRequired = LoadToHeatingSetPoint
         ZoneSetPoint = ZoneThermostatSetPointLo(ZoneNum)
      ELSE IF (LoadToHeatingSetPoint .LT. 0.0d0 .AND. LoadToCoolingSetPoint .LT. 0.0d0) THEN
         ZoneSysEnergyDemand(ZoneNum)%TotalOutputRequired = LoadToCoolingSetPoint
         ZoneSetPoint = ZoneThermostatSetPointHi(ZoneNum)
      ELSE IF (LoadToHeatingSetPoint .LE. 0.0d0 .AND. LoadToCoolingSetPoint .GE. 0.0d0) THEN ! deadband includes zero loads
        ! this turns out to cause instabilities sometimes? that lead to setpoint errors if predictor is off.
         ZoneSysEnergyDemand(ZoneNum)%TotalOutputRequired = 0.0d0
         IF(Zone(ZoneNum)%SystemZoneNodeNumber > 0) THEN
            ZoneSetPoint = Node(Zone(ZoneNum)%SystemZoneNodeNumber)%Temp
            ZoneSetPoint = MAX(ZoneSetPoint, ZoneThermostatSetPointLo(ZoneNum)) ! trap out of deadband
            ZoneSetPoint = MIN(ZoneSetPoint, ZoneThermostatSetPointHi(ZoneNum)) ! trap out of deadband
         END IF
         DeadBandOrSetback(ZoneNum) = .TRUE.
      ELSE  ! this should never occur!
         CALL ShowSevereError('DualSetPointWithDeadBand: Unanticipated combination of heating and cooling loads - '// &
                             'report to EnergyPlus Development Team')
         CALL ShowContinueErrorTimeStamp('occurs in Zone='//TRIM(Zone(ZoneNum)%Name))
         CALL ShowContinueError('LoadToHeatingSetPoint='//TRIM(RoundSigDigits(LoadToHeatingSetPoint,3))//  &
                                ', LoadToCoolingSetPoint='//TRIM(RoundSigDigits(LoadToCoolingSetPoint,3)))
         CALL ShowContinueError('Zone Heating Set-point='//TRIM(RoundSigDigits(ZoneThermostatSetPointLo(ZoneNum),2)))
         CALL ShowContinueError('Zone Cooling Set-point='//TRIM(RoundSigDigits(ZoneThermostatSetPointHi(ZoneNum),2)))
         CALL ShowContinueError('Zone TempDepZnLd='//TRIM(RoundSigDigits(TempDepZnLd(ZoneNum),2)))
         CALL ShowContinueError('Zone TempIndZnLd='//TRIM(RoundSigDigits(TempIndZnLd(ZoneNum),2)))
         CALL ShowContinueError('Zone ThermostatSetPoint='//TRIM(RoundSigDigits(TempZoneThermostatSetPoint(ZoneNum),2)))

         CALL ShowFatalError('Program terminates due to above conditions.')
      END IF

  END SELECT

    ! Staged control zone
    IF (NumStageCtrZone > 0) THEN
      If (StageZoneLogic(ZoneNum)) Then
        If (ZoneSysEnergyDemand(ZoneNum)%StageNum ==0) Then ! No load
          LoadToHeatingSetPoint = 0.0d0
          LoadToCoolingSetPoint = 0.0d0
          ZoneSysEnergyDemand(ZoneNum)%TotalOutputRequired = 0.0d0
          IF(Zone(ZoneNum)%SystemZoneNodeNumber > 0) THEN
            ZoneSetPoint = Node(Zone(ZoneNum)%SystemZoneNodeNumber)%Temp
            ZoneSetPoint = MAX(ZoneSetPoint, ZoneThermostatSetPointLo(ZoneNum)) ! trap out of deadband
            ZoneSetPoint = MIN(ZoneSetPoint, ZoneThermostatSetPointHi(ZoneNum)) ! trap out of deadband
          END IF
          DeadBandOrSetback(ZoneNum) = .TRUE.
        Else If (ZoneSysEnergyDemand(ZoneNum)%StageNum <0) Then ! Cooling load
          SELECT CASE (ZoneAirSolutionAlgo)
            CASE (Use3rdOrder)
              LoadToCoolingSetPoint = (TempDepZnLd(ZoneNum) * &
                              (ZoneThermostatSetPointHi(ZoneNum)) - TempIndZnLd(ZoneNum))
            CASE (UseAnalyticalSolution)
              If (TempDepZnLd(ZoneNum) .eq. 0.0d0) Then ! B=0
                LoadToCoolingSetPoint = AIRRAT(ZoneNum)*(ZoneThermostatSetPointHi(ZoneNum)- &
                                ZoneT1(ZoneNum)) - TempIndZnLd(ZoneNum)
              Else
                LoadToCoolingSetPoint = TempDepZnLd(ZoneNum)*(ZoneThermostatSetPointHi(ZoneNum) &
                  -ZoneT1(ZoneNum)*exp(MIN(700.d0,-TempDepZnLd(ZoneNum)/AIRRAT(ZoneNum)))) &
                                /(1.0d0-exp(MIN(700.d0,-TempDepZnLd(ZoneNum)/AIRRAT(ZoneNum)))) - TempIndZnLd(ZoneNum)
              End If
            CASE (UseEulerMethod)
              LoadToCoolingSetPoint = AIRRAT(ZoneNum)*(ZoneThermostatSetPointHi(ZoneNum)- &
                                ZoneT1(ZoneNum)) + TempDepZnLd(ZoneNum)*ZoneThermostatSetPointHi(ZoneNum) - TempIndZnLd(ZoneNum)
          END SELECT
          ZoneSysEnergyDemand(ZoneNum)%TotalOutputRequired = LoadToCoolingSetPoint
          ZoneSetPoint = ZoneThermostatSetPointHi(ZoneNum)
          LoadToHeatingSetPoint = LoadToCoolingSetPoint
          IF ((ZoneSysEnergyDemand(ZoneNum)%TotalOutputRequired) .GE. 0.0) DeadBandOrSetback(ZoneNum) = .TRUE.
        Else ! Heating load
          SELECT CASE (ZoneAirSolutionAlgo)
            CASE (Use3rdOrder)
              LoadToHeatingSetPoint = (TempDepZnLd(ZoneNum) * ZoneThermostatSetPointLo(ZoneNum) - TempIndZnLd(ZoneNum))
            ! Exact solution
            CASE (UseAnalyticalSolution)
              If (TempDepZnLd(ZoneNum) .eq. 0.0d0) Then ! B=0
                LoadToHeatingSetPoint = AIRRAT(ZoneNum)*(ZoneThermostatSetPointLo(ZoneNum) - &
                                ZoneT1(ZoneNum)) - TempIndZnLd(ZoneNum)
              Else
                LoadToHeatingSetPoint = TempDepZnLd(ZoneNum)*(ZoneThermostatSetPointLo(ZoneNum) &
                 -ZoneT1(ZoneNum)*exp(MIN(700.d0,-TempDepZnLd(ZoneNum)/AIRRAT(ZoneNum))))/ &
                 (1.0d0-exp(MIN(700.d0,-TempDepZnLd(ZoneNum)/AIRRAT(ZoneNum))))-TempIndZnLd(ZoneNum)
              End If
            CASE (UseEulerMethod)
              LoadToHeatingSetPoint = AIRRAT(ZoneNum)*(ZoneThermostatSetPointLo(ZoneNum)- &
                                ZoneT1(ZoneNum)) + TempDepZnLd(ZoneNum)*(ZoneThermostatSetPointLo(ZoneNum)) &
                                - TempIndZnLd(ZoneNum)
          END SELECT
          ZoneSysEnergyDemand(ZoneNum)%TotalOutputRequired = LoadToHeatingSetPoint
          ZoneSetPoint = ZoneThermostatSetPointLo(ZoneNum)
          LoadToCoolingSetPoint = LoadToHeatingSetPoint
          IF ((ZoneSysEnergyDemand(ZoneNum)%TotalOutputRequired) .LE. 0.0) DeadBandOrSetback(ZoneNum) = .TRUE.
        End If
      End If
    End If

  !If the ZoneNodeNum has been set for a Controlled Zone, then the zone setpoint is placed on the node.
  IF(Zone(ZoneNum)%SystemZoneNodeNumber > 0) THEN
     Node(Zone(ZoneNum)%SystemZoneNodeNumber)%TempSetPoint = ZoneSetPoint
  END IF

  IF(ZoneSetPoint .GT. ZoneSetPointLast(ZoneNum))THEN
    SetBack(ZoneNum) = .TRUE.
  ELSE
    SetBack(ZoneNum) = .FALSE.
  END IF

  ZoneSetPointLast(ZoneNum) = ZoneSetPoint

  ! Save the unmultiplied zone load to a report variable
  SNLoadPredictedRate(ZoneNum) = ZoneSysEnergyDemand(ZoneNum)%TotalOutputRequired * LoadCorrectionFactor(ZoneNum)
  SNLoadPredictedHSPRate(ZoneNum) = LoadToHeatingSetPoint * LoadCorrectionFactor(ZoneNum)
  SNLoadPredictedCSPRate(ZoneNum) = LoadToCoolingSetPoint * LoadCorrectionFactor(ZoneNum)
  CurDeadBandOrSetback(ZoneNum) = DeadBandOrSetback(ZoneNum)

  ! Apply the Zone Multiplier and Load Correction factor to the total zone load
  ZoneSysEnergyDemand(ZoneNum)%TotalOutputRequired = ZoneSysEnergyDemand(ZoneNum)%TotalOutputRequired &
    * Zone(ZoneNum)%Multiplier * Zone(ZoneNum)%ListMultiplier * LoadCorrectionFactor(ZoneNum)
  ZoneSysEnergyDemand(ZoneNum)%OutputRequiredToHeatingSP = LoadToHeatingSetPoint &
    * Zone(ZoneNum)%Multiplier * Zone(ZoneNum)%ListMultiplier * LoadCorrectionFactor(ZoneNum)
  ZoneSysEnergyDemand(ZoneNum)%OutputRequiredToCoolingSP = LoadToCoolingSetPoint &
    * Zone(ZoneNum)%Multiplier * Zone(ZoneNum)%ListMultiplier * LoadCorrectionFactor(ZoneNum)

  !init each sequenced demand to the full output
  IF (ALLOCATED(ZoneSysEnergyDemand(ZoneNum)%SequencedOutputRequired)) &
      ZoneSysEnergyDemand(ZoneNum)%SequencedOutputRequired = & ! array assignment
                        ZoneSysEnergyDemand(ZoneNum)%TotalOutputRequired
  IF (ALLOCATED(ZoneSysEnergyDemand(ZoneNum)%SequencedOutputRequiredToHeatingSP)) &
      ZoneSysEnergyDemand(ZoneNum)%SequencedOutputRequiredToHeatingSP = & ! array assignment
                        ZoneSysEnergyDemand(ZoneNum)%OutputRequiredToHeatingSP
  IF (ALLOCATED(ZoneSysEnergyDemand(ZoneNum)%SequencedOutputRequiredToCoolingSP)) &
      ZoneSysEnergyDemand(ZoneNum)%SequencedOutputRequiredToCoolingSP = & ! array assignment
                        ZoneSysEnergyDemand(ZoneNum)%OutputRequiredToCoolingSP

  RETURN

END SUBROUTINE CalcPredictedSystemLoad

SUBROUTINE CalcPredictedHumidityRatio(ZoneNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard J. Liesen
          !       DATE WRITTEN   May 2001
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine does the prediction step for humidity control

          ! METHODOLOGY EMPLOYED:
          ! This solves for the required system moisture required to try and achieve the desired
          ! Humidity Ratio in the Zone

          ! REFERENCES:
          ! Routine FinalZnCalcs - FINAL ZONE CALCULATIONS, authored by Dale Herron
          ! for BLAST.

          ! USE STATEMENTS:
  USE ScheduleManager, ONLY: GetCurrentScheduleValue
  USE General,         ONLY: RoundSigDigits
  USE DataSurfaces,    ONLY: Surface, HeatTransferModel_EMPD, HeatTransferModel_HAMT

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: ZoneNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64) :: LatentGain ! Zone latent load
  REAL(r64) :: RhoAir
  REAL(r64) :: A
  REAL(r64) :: B
  REAL(r64) :: C
  REAL(r64) :: SysTimeStepInSeconds
  REAL(r64) :: H2OHtOfVap
  REAL(r64) :: RHSetPoint             ! Relative Humidity in percent
  REAL(r64) :: WZoneSetPoint
  INTEGER   :: HumidControlledZoneNum
  LOGICAL   :: ControlledHumidZoneFlag     ! This determines whether this is a humidity controlled zone or not
  REAL(r64) :: ZoneRHHumidifyingSetPoint   ! Zone humidifying set point (%)
  REAL(r64) :: ZoneRHDehumidifyingSetPoint ! Zone dehumidifying set point (%)
  REAL(r64) :: LoadToHumidifySetPoint      ! Moisture load at humidifying set point
  REAL(r64) :: LoadToDehumidifySetPoint    ! Moisture load at dehumidifying set point
  REAL(r64) :: ZoneAirRH                   ! Zone air relative humidity
  LOGICAL   :: SingleSetpoint              ! This determines whether both setpoint are equal or not

          ! FLOW:
  LoadToHumidifySetPoint=0.0d0
  LoadToDehumidifySetPoint=0.0d0
  SingleSetpoint = .FALSE.
  ZoneSysMoistureDemand(ZoneNum)%TotalOutputRequired = 0.0d0
  ZoneSysMoistureDemand(ZoneNum)%OutputRequiredToHumidifyingSP = 0.0d0
  ZoneSysMoistureDemand(ZoneNum)%OutputRequiredToDehumidifyingSP = 0.0d0

  ! Check to see if this is a "humidity controlled zone"
  ControlledHumidZoneFlag = .FALSE.
  ! Check all the controlled zones to see if it matches the zone simulated
  DO HumidControlledZoneNum = 1, NumHumidityControlZones
    IF (HumidityControlZone(HumidControlledZoneNum)%ActualZoneNum /= ZoneNum) CYCLE
    ZoneAirRH = PsyRhFnTdbWPb(MAT(ZoneNum),ZoneAirHumRat(ZoneNum),OutBaroPress)*100.d0
    ZoneRHHumidifyingSetPoint = GetCurrentScheduleValue(HumidityControlZone(HumidControlledZoneNum)%HumidifyingSchedIndex)
    ZoneRHDehumidifyingSetPoint = GetCurrentScheduleValue(HumidityControlZone(HumidControlledZoneNum)%DehumidifyingSchedIndex)
    IF (HumidityControlZone(HumidControlledZoneNum)%EMSOverrideHumidifySetpointOn) THEN
      ZoneRHHumidifyingSetPoint = HumidityControlZone(HumidControlledZoneNum)%EMSOverrideHumidifySetpointValue
    ENDIF
    IF (HumidityControlZone(HumidControlledZoneNum)%EMSOverrideDehumidifySetpointOn) THEN
      ZoneRHDehumidifyingSetPoint = HumidityControlZone(HumidControlledZoneNum)%EMSOverrideDehumidifySetpointValue
    ENDIF

    ! Run-time error check
    If (ZoneRHHumidifyingSetPoint > ZoneRHDehumidifyingSetPoint) then
!      HumidityControlZone(HumidControlledZoneNum)%ErrorCount = HumidityControlZone(HumidControlledZoneNum)%ErrorCount + 1
      if (HumidityControlZone(HumidControlledZoneNum)%ErrorIndex == 0) then
        CALL ShowWarningMessage('HUMIDISTAT: The humidifying setpoint is above '// &
                'the dehumidifying setpoint in '//TRIM(HumidityControlZone(HumidControlledZoneNum)%ControlName))
        CALL ShowContinueError('The zone humidifying setpoint is set to the dehumidifying setpoint.')
        CALL ShowContinueErrorTimeStamp(' Occurrence info: ')
      endif
      CALL ShowRecurringWarningErrorAtEnd('The humidifying setpoint is still above '// &
            'the dehumidifying setpoint',HumidityControlZone(HumidControlledZoneNum)%ErrorIndex, &
             ZoneRHHumidifyingSetPoint, ZoneRHHumidifyingSetPoint)
      ZoneRHHumidifyingSetPoint = ZoneRHDehumidifyingSetPoint
    End If
    IF (ZoneRHHumidifyingSetPoint == ZoneRHDehumidifyingSetPoint) SingleSetpoint = .TRUE.
    ControlledHumidZoneFlag = .TRUE.


    EXIT
  END DO ! HumidControlledZoneNum

  If (ControlledHumidZoneFlag) Then

    ! Calculate hourly humidity ratio from infiltration + humidity added from latent load
    ! to determine system added/subtracted moisture.
    LatentGain = ZoneLatentGain(ZoneNum) + SumLatentHTRadSys(ZoneNum)

    SysTimeStepInSeconds = SecInHour * TimeStepSys

    ! Calculate the coefficients for the 3rd Order derivative for final
    ! zone humidity ratio.  The A, B, C coefficients are analogous to the heat balance.
    ! SumHmARaW and SumHmARa will be used with the Moisture Balance on the building elements and
    ! are currently set to zero when the CTF only version is used.

    ! if no surface in the zone uses EMPD or HAMT then zero
    IF ((.NOT. ANY(Surface(Zone(ZoneNum)%SurfaceFirst:Zone(ZoneNum)%SurfaceLast)%HeatTransferAlgorithm == HeatTransferModel_EMPD))&
      .AND. &
      (.NOT. ANY(Surface(Zone(ZoneNum)%SurfaceFirst:Zone(ZoneNum)%SurfaceLast)%HeatTransferAlgorithm == HeatTransferModel_HAMT))) &
       THEN
      SumHmARaW(ZoneNum) = 0.0d0
      SumHmARa(ZoneNum) = 0.0d0
    END IF

    ! The density of air and latent heat of vaporization are calculated as functions.
    RhoAir = PsyRhoAirFnPbTdbW(OutBaroPress,ZT(ZoneNum),ZoneAirHumRat(ZoneNum),'CalcPredictedHumidityRatio')
    H2OHtOfVap = PsyHgAirFnWTdb(ZoneAirHumRat(ZoneNum),ZT(ZoneNum),'CalcPredictedHumidityRatio')

    ! Assume that the system will have flow
    if (SimulateAirflowNetwork == AirflowNetworkControlMultizone.OR.SimulateAirflowNetwork == AirflowNetworkControlMultiADS .OR. &
       (SimulateAirflowNetwork == AirflowNetworkControlSimpleADS .AND. AirflowNetworkFanActivated)) then
      ! Multizone airflow calculated in AirflowNetwork
      B = (LatentGain / H2OHtOfVap) + AirflowNetworkExchangeData(ZoneNum)%SumMHrW + &
          AirflowNetworkExchangeData(ZoneNum)%SumMMHrW+SumHmARaW(ZoneNum)
      A = AirflowNetworkExchangeData(ZoneNum)%SumMHr+AirflowNetworkExchangeData(ZoneNum)%SumMMHr + &
          SumHmARa(ZoneNum)
    else
      B = (LatentGain / H2OHtOfVap) + ((oamfl(ZoneNum) + vamfl(ZoneNum) + eamfl(ZoneNum) + ctmfl(ZoneNum)) * OutHumRat) + &
             SumHmARaW(ZoneNum) + MixingMassFlowXHumRat(ZoneNum) + MDotOA(ZoneNum)* OutHumRat
      A = oamfl(ZoneNum) + vamfl(ZoneNum) + eamfl(ZoneNum) + ctmfl(ZoneNum) + SumHmARa(ZoneNum) + MixingMassFlowZone(ZoneNum) &
          + MDotOA(ZoneNum)
    end if
    C = RhoAir * Zone(ZoneNum)%Volume * ZoneVolCapMultpMoist / SysTimeStepInSeconds

    ! Use a 3rd Order derivative to predict zone moisture addition or removal and
    ! smooth the changes using the zone air capacitance.  Positive values of Moist Load means that
    ! this amount of moisture must be added to the zone to reach the setpoint.  Negative values represent
    ! the amount of moisture that must be removed by the system.
    !MoistLoadHumidSetPoint = massflow * HumRat = kg air/sec  * kg H2O/kg Air = kg H2O/sec
    WZoneSetPoint = PsyWFnTdbRhPb(ZT(ZoneNum),(ZoneRHHumidifyingSetPoint/100.0d0),OutBaroPress,'CalcPredictedHumidityRatio')
    SELECT CASE (ZoneAirSolutionAlgo)
      CASE (Use3rdOrder)
        LoadToHumidifySetPoint = ((11.0d0/6.0d0) * C + A) * WZoneSetPoint - &
           (B + C * (3.0d0 * WZoneTimeMinus1Temp(ZoneNum) - (3.0d0/2.0d0) * WZoneTimeMinus2Temp(ZoneNum) + &
           (1.0d0/3.0d0) * WZoneTimeMinus3Temp(ZoneNum)))
      ! Exact solution
      CASE (UseAnalyticalSolution)
        If (A .eq. 0.0d0) Then ! B=0
          LoadToHumidifySetPoint = C*(WZoneSetPoint-ZoneW1(ZoneNum)) - B
        Else
          LoadToHumidifySetPoint = A*(WZoneSetPoint-ZoneW1(ZoneNum)*exp(MIN(700.d0,-A/C)))/(1.0d0-exp(MIN(700.d0,-A/C))) - B
        End If
      CASE (UseEulerMethod)
        LoadToHumidifySetPoint = C*(WZoneSetPoint-ZoneW1(ZoneNum)) + A*WZoneSetPoint - B
    END SELECT
    ZoneSysMoistureDemand(ZoneNum)%OutputRequiredToHumidifyingSP = LoadToHumidifySetPoint
    WZoneSetPoint = PsyWFnTdbRhPb(ZT(ZoneNum),(ZoneRHDehumidifyingSetPoint / 100.0d0),OutBaroPress,'CalcPredictedHumidityRatio')
    SELECT CASE (ZoneAirSolutionAlgo)
      CASE (Use3rdOrder)
        LoadToDehumidifySetPoint = ((11.0d0/6.0d0) * C + A) * WZoneSetPoint - &
           (B + C * (3.0d0 * WZoneTimeMinus1Temp(ZoneNum) - (3.0d0/2.0d0) * WZoneTimeMinus2Temp(ZoneNum) + &
           (1.0d0/3.0d0) * WZoneTimeMinus3Temp(ZoneNum)))
      ! Exact solution
      CASE (UseAnalyticalSolution)
        If (A .eq. 0.0d0) Then ! B=0
          LoadToDehumidifySetPoint = C*(WZoneSetPoint-ZoneW1(ZoneNum)) - B
        Else
          LoadToDehumidifySetPoint = A*(WZoneSetPoint-ZoneW1(ZoneNum)*exp(MIN(700.d0,-A/C)))/(1.0d0-exp(MIN(700.d0,-A/C))) - B
        End If
      CASE (UseEulerMethod)
        LoadToDehumidifySetPoint = C*(WZoneSetPoint-ZoneW1(ZoneNum)) + A*WZoneSetPoint - B
    END SELECT
    ZoneSysMoistureDemand(ZoneNum)%OutputRequiredToDehumidifyingSP = LoadToDehumidifySetPoint

    ! The load is added to the TotalOutputRequired as in the Temperature Predictor.  There is also the remaining
    ! output variable for those who will use this for humidity control and stored in DataZoneEnergyDemands with the
    ! analogous temperature terms.
    If (SingleSetpoint) Then
      ZoneSysMoistureDemand(ZoneNum)%TotalOutputRequired = LoadToHumidifySetPoint
    Else
      IF (LoadToHumidifySetPoint .GT. 0.0d0 .AND. LoadToDehumidifySetPoint .GT. 0.0d0) THEN
        ZoneSysMoistureDemand(ZoneNum)%TotalOutputRequired = LoadToHumidifySetPoint
        RHSetPoint = ZoneRHHumidifyingSetPoint
      ELSE IF (LoadToHumidifySetPoint .LT. 0.0d0 .AND. LoadToDehumidifySetPoint .LT. 0.0d0) THEN
        ZoneSysMoistureDemand(ZoneNum)%TotalOutputRequired = LoadToDehumidifySetPoint
        RHSetPoint = ZoneRHDehumidifyingSetPoint
      ELSE IF (LoadToHumidifySetPoint .LE. 0.0d0 .AND. LoadToDehumidifySetPoint .GE. 0.0d0) THEN ! deadband includes zero loads
        ZoneSysMoistureDemand(ZoneNum)%TotalOutputRequired = 0.0d0
      ELSE  ! this should never occur!
        CALL ShowSevereError('Humidistat: Unanticipated combination of humidifying and dehumidifying loads - '// &
                           'report to EnergyPlus Development Team')
        CALL ShowContinueErrorTimeStamp('occurs in Zone='//TRIM(Zone(ZoneNum)%Name))
        CALL ShowContinueError('LoadToHumidifySetPoint='//TRIM(RoundSigDigits(LoadToHumidifySetPoint,5))//  &
                             ', LoadToDehumidifySetPoint='//TRIM(RoundSigDigits(LoadToDehumidifySetPoint,5)))
        CALL ShowContinueError('Zone RH Humidifying Set-point='//TRIM(RoundSigDigits(ZoneRHHumidifyingSetPoint,1)))
        CALL ShowContinueError('Zone RH Dehumidifying Set-point='//TRIM(RoundSigDigits(ZoneRHDehumidifyingSetPoint,2)))
        CALL ShowFatalError('Program terminates due to above conditions.')
      END IF
    End If
  End If

  ! Save the unmultiplied zone moisture load to a report variable
  MoisturePredictedRate(ZoneNum) = ZoneSysMoistureDemand(ZoneNum)%TotalOutputRequired

  ! Apply the Zone Multiplier to the total zone moisture load
  ZoneSysMoistureDemand(ZoneNum)%TotalOutputRequired = ZoneSysMoistureDemand(ZoneNum)%TotalOutputRequired &
    * Zone(ZoneNum)%Multiplier * Zone(ZoneNum)%ListMultiplier
  ZoneSysMoistureDemand(ZoneNum)%OutputRequiredToHumidifyingSP = ZoneSysMoistureDemand(ZoneNum)%OutputRequiredToHumidifyingSP &
    * Zone(ZoneNum)%Multiplier * Zone(ZoneNum)%ListMultiplier
  ZoneSysMoistureDemand(ZoneNum)%OutputRequiredToDehumidifyingSP = ZoneSysMoistureDemand(ZoneNum)%OutputRequiredToDehumidifyingSP &
    * Zone(ZoneNum)%Multiplier * Zone(ZoneNum)%ListMultiplier

  !init each sequenced demand to the full output
  IF (ALLOCATED(ZoneSysMoistureDemand(ZoneNum)%SequencedOutputRequired)) &
    ZoneSysMoistureDemand(ZoneNum)%SequencedOutputRequired = & ! array assignment
                        ZoneSysMoistureDemand(ZoneNum)%TotalOutputRequired
  IF (ALLOCATED(ZoneSysMoistureDemand(ZoneNum)%SequencedOutputRequiredToHumidSP))  &
    ZoneSysMoistureDemand(ZoneNum)%SequencedOutputRequiredToHumidSP = & ! array assignment
                        ZoneSysMoistureDemand(ZoneNum)%OutputRequiredToHumidifyingSP
  IF (ALLOCATED(ZoneSysMoistureDemand(ZoneNum)%SequencedOutputRequiredToDehumidSP))  &
    ZoneSysMoistureDemand(ZoneNum)%SequencedOutputRequiredToDehumidSP = & ! array assignment
                        ZoneSysMoistureDemand(ZoneNum)%OutputRequiredToDehumidifyingSP

  RETURN

END SUBROUTINE CalcPredictedHumidityRatio

SUBROUTINE CorrectZoneAirTemp(ZoneTempChange, ShortenTimeStepSys,  &
                                UseZoneTimeStepHistory,  PriorTimeStep )

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Russell Taylor
          !       DATE WRITTEN   ???
          !       MODIFIED       November 1999, LKL;
          !       RE-ENGINEERED  July 2003 (Peter Graham Ellis)
          !                      February 2008 (Brent Griffith reworked history )

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine updates the zone air temperature and modifies the system
          ! time step.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataLoopNode, ONLY: Node
  USE DataRoomAirModel,     ONLY: AirModel,RoomAirModel_Mixing,RoomAirModel_UCSDDV,IsZoneDV,ZoneDVMixedFlag,IsZoneUI, &
                                  ZTFloor,MATFloor,XMATFloor,XM2TFloor,XM3TFloor,XM4TFloor, &
                                  ZTOC,ZTM1OC,MATOC,XMATOC,XM2TOC,XM3TOC,XM4TOC, &
                                  ZTMX,ZTM1MX,MATMX,XMATMX,XM2TMX,XM3TMX,XM4TMX, RoomAirModel_Mundt, RoomAirModel_UserDefined
  USE RoomAirModelManager , ONLY: ManageAirModel
 USE DataEnvironment, ONLY: Month, DayOfMonth
 USE General, ONLY: TrimSigDigits
  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  REAL(r64) , INTENT(OUT) :: ZoneTempChange     ! Temperature change in zone air between previous and current timestep
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
  REAL(r64)               :: CpAir                           !specific heat of air
  REAL(r64)      :: SumIntGain =0.0d0                 ! Zone sum of convective internal gains
  REAL(r64)      :: SumHA      =0.0d0                 ! Zone sum of Hc*Area
  REAL(r64)      :: SumHATsurf =0.0d0                 ! Zone sum of Hc*Area*Tsurf
  REAL(r64)      :: SumHATref  =0.0d0                 ! Zone sum of Hc*Area*Tref, for ceiling diffuser convection correlation
  REAL(r64)      :: SumMCp     =0.0d0                 ! Zone sum of MassFlowRate*Cp
  REAL(r64)      :: SumMCpT    =0.0d0                 ! Zone sum of MassFlowRate*Cp*T
  REAL(r64)      :: SumSysMCp  =0.0d0                 ! Zone sum of air system MassFlowRate*Cp
  REAL(r64)      :: SumSysMCpT =0.0d0                 ! Zone sum of air system MassFlowRate*Cp*T
  REAL(r64)      :: ZoneEnthalpyIn =0.0d0             ! Zone inlet air enthalpy
  REAL(r64)      :: TempDepCoef=0.0d0                 ! Formerly CoefSumha, coef in zone temp equation with dimensions of h*A
  REAL(r64)      :: TempIndCoef=0.0d0                 ! Formerly CoefSumhat, coef in zone temp equation with dimensions of h*A(T1
  REAL(r64)      :: AirCap     =0.0d0                 ! Formerly CoefAirrat, coef in zone temp eqn with dim of "air power capacity"
  REAL(r64)      :: SNLoad     =0.0d0                 ! Sensible load calculated for zone in watts and then loaded in report variables
  INTEGER :: ZoneNum   =0
  INTEGER :: ZoneNodeNum=0                        ! System node number for air flow through zone either by system or as a plenum
!  LOGICAL,SAVE   :: OneTimeFlag = .TRUE.
!unusd1208  LOGICAL,SAVE   :: MyEnvrnFlag = .TRUE.
  REAL(r64)      :: TempSupplyAir
  REAL(r64)      :: ZoneMult
!unused1208  REAL(r64)           :: TimeStepSeconds  ! dt term for denominator under Cz in Seconds

          ! FLOW:
  ! Initializations
  ZoneTempChange                      = constant_zero

  ! Update zone temperatures
  DO ZoneNum = 1, NumOfZones

    ZoneMult = Zone(ZoneNum)%Multiplier * Zone(ZoneNum)%ListMultiplier

    IF (ShortenTimeStepSys) THEN
      !time step has gotten smaller, use zone timestep history to interpolate new set of "DS" history terms.
      If (NumOfSysTimeSteps /= NumOfSysTimeStepsLastZoneTimeStep)  then ! cannot reuse existing DS data, interpolate from zone time
        Call DownInterpolate4HistoryValues(PriorTimeStep,TimeStepSys, &
                                MAT(ZoneNum),   XMAT(ZoneNum),    XM2T(ZoneNum),   XM3T(ZoneNum), XM4T(ZoneNum) ,   &  !
                                MAT(ZoneNum), DSXMAT(ZoneNum), DSXM2T(ZoneNum), DSXM3T(ZoneNum), DSXM4T(ZoneNum))
        Call DownInterpolate4HistoryValues(PriorTimeStep,TimeStepSys, &
                                ZoneAirHumRat(ZoneNum),   WZoneTimeMinus1(ZoneNum),   WZoneTimeMinus2(ZoneNum),   & !
                                                          WZoneTimeMinus3(ZoneNum),   WZoneTimeMinus4(ZoneNum), & !
                                ZoneAirHumRat(ZoneNum), DSWZoneTimeMinus1(ZoneNum), DSWZoneTimeMinus2(ZoneNum), &
                                                        DSWZoneTimeMinus3(ZoneNum), DSWZoneTimeMinus4(ZoneNum))
        IF (IsZoneDV(ZoneNum) .or. IsZoneUI(ZoneNum)) THEN
          CALL DownInterpolate4HistoryValues(PriorTimeStep,TimeStepSys, &
                                MATFloor(ZoneNum),   XMATFloor(ZoneNum),    XM2TFloor(ZoneNum),  &
                                                     XM3TFloor(ZoneNum),    XM4TFloor(ZoneNum) ,   &
                                MATFloor(ZoneNum), DSXMATFloor(ZoneNum),  DSXM2TFloor(ZoneNum),  &
                                                   DSXM3TFloor(ZoneNum),  DSXM4TFloor(ZoneNum))
          CALL DownInterpolate4HistoryValues(PriorTimeStep,TimeStepSys, &
                                MATOC(ZoneNum),   XMATOC(ZoneNum),    XM2TOC(ZoneNum),  &
                                                  XM3TOC(ZoneNum),    XM4TOC(ZoneNum) ,   &
                                MATOC(ZoneNum), DSXMATOC(ZoneNum),  DSXM2TOC(ZoneNum),  &
                                                DSXM3TOC(ZoneNum),  DSXM4TOC(ZoneNum))
          CALL DownInterpolate4HistoryValues(PriorTimeStep,TimeStepSys, &
                                MATMX(ZoneNum),   XMATMX(ZoneNum),    XM2TMX(ZoneNum),  &
                                                  XM3TMX(ZoneNum),    XM4TMX(ZoneNum) ,   &
                                MATMX(ZoneNum), DSXMATMX(ZoneNum),  DSXM2TMX(ZoneNum),  &
                                                DSXM3TMX(ZoneNum),  DSXM4TMX(ZoneNum))
        ENDIF

      ELSE ! reuse history data in DS terms from last zone time step to preserve information that would be lost
         ! do nothing because DS history would have been pushed prior and should be ready?

      ENDIF
    ENDIF

    ! now update the variables actually used in the balance equations.
    IF(.not. UseZoneTimeStepHistory) THEN
      ZTM1(ZoneNum)                = DSXMAT(ZoneNum)
      ZTM2(ZoneNum)                = DSXM2T(ZoneNum)
      ZTM3(ZoneNum)                = DSXM3T(ZoneNum)

      WZoneTimeMinus1Temp(ZoneNum) = DSWZoneTimeMinus1(ZoneNum)
      WZoneTimeMinus2Temp(ZoneNum) = DSWZoneTimeMinus2(ZoneNum)
      WZoneTimeMinus3Temp(ZoneNum) = DSWZoneTimeMinus3(ZoneNum)
    ELSE
      ZTM1(ZoneNum)                = XMAT(ZoneNum)
      ZTM2(ZoneNum)                = XM2T(ZoneNum)
      ZTM3(ZoneNum)                = XM3T(ZoneNum)

      WZoneTimeMinus1Temp(ZoneNum) = WZoneTimeMinus1(ZoneNum)
      WZoneTimeMinus2Temp(ZoneNum) = WZoneTimeMinus2(ZoneNum)
      WZoneTimeMinus3Temp(ZoneNum) = WZoneTimeMinus3(ZoneNum)

    END IF

    AIRRAT(ZoneNum) = Zone(ZoneNum)%Volume*ZoneVolCapMultpSens*   &
               PsyRhoAirFnPbTdbW(OutBaroPress,MAT(ZoneNum),ZoneAirHumRat(ZoneNum),'CorrectZoneAirTemp')*   &
               PsyCpAirFnWTdb(ZoneAirHumRat(ZoneNum),MAT(ZoneNum),'CorrectZoneAirTemp')/(TimeStepSys*SecInHour)

    AirCap = AIRRAT(ZoneNum)

    CALL ManageAirModel(ZoneNum)

    ! Calculate the various heat balance sums
    CALL CalcZoneSums(ZoneNum, SumIntGain, SumHA, SumHATsurf, SumHATref, SumMCp, SumMCpT, SumSysMCp, SumSysMCpT)
!    ZoneTempHistoryTerm = (3.0D0 * ZTM1(ZoneNum) - (3.0D0/2.0D0) * ZTM2(ZoneNum) + (1.0D0/3.0D0) * ZTM3(ZoneNum))
    ZoneNodeNum = Zone(ZoneNum)%SystemZoneNodeNumber

    SNLOAD=0.0d0

    IF (ZoneNodeNum > 0) THEN ! This zone is controlled by a zone equipment configuration or zone plenum

       ! Heat balance coefficients for controlled zone, i.e. with system air flow
       TempDepCoef = SumHA + SumMCp + SumSysMCp
       TempIndCoef = SumIntGain + SumHATsurf - SumHATref + SumMCpT + SumSysMCpT + (NonAirSystemResponse(ZoneNum) / ZoneMult + &
                     SysDepZoneLoadsLagged(ZoneNum))
   !    TempHistoryTerm = AirCap * (3.0 * ZTM1(ZoneNum) - (3.0/2.0) * ZTM2(ZoneNum) + (1.0/3.0) * ZTM3(ZoneNum)) !debug only

       if (SimulateAirflowNetwork .GT. AirflowNetworkControlMultizone) then
          TempIndCoef = TempIndCoef+AirflowNetworkExchangeData(ZoneNum)%TotalSen
       end if
   !    TempDepZnLd(ZoneNum) = (11.0/6.0) * AirCap + TempDepCoef
   !    TempIndZnLd(ZoneNum) = TempHistoryTerm + TempIndCoef
       ! Solve for zone air temperature
       SELECT CASE (ZoneAirSolutionAlgo)
         CASE (Use3rdOrder)
           ZT(ZoneNum) = (TempIndCoef + AirCap*(3.0d0*ZTM1(ZoneNum) - (3.0d0/2.0d0)*ZTM2(ZoneNum) + (1.0d0/3.0d0)*ZTM3(ZoneNum))) &
                   / ((11.0d0/6.0d0) * AirCap + TempDepCoef)
         ! Exact solution
         CASE (UseAnalyticalSolution)
           If (TempDepCoef .eq. 0.0d0) Then ! B=0
             ZT(ZoneNum) = ZoneT1(ZoneNum) + TempIndCoef/AirCap
           Else
             ZT(ZoneNum) = (ZoneT1(ZoneNum)-TempIndCoef/TempDepCoef)*exp(MIN(700.d0,-TempDepCoef/AirCap))+TempIndCoef/TempDepCoef
           End If
         CASE (UseEulerMethod)
           ZT(ZoneNum) = (AirCap*ZoneT1(ZoneNum)+TempIndCoef)/(AirCap+TempDepCoef)
       END SELECT
       ! Update zone node temperature and thermostat temperature unless already updated in Room Air Model,
       ! calculate load correction factor
       IF ((AirModel(ZoneNum)%AirModelType ==  RoomAirModel_Mixing) .or. (.not.AirModel(ZoneNum)%SimAirModel)) THEN
           ! Fully mixed
           Node(ZoneNodeNum)%Temp  = ZT(ZoneNum)
           TempTstatAir(ZoneNum)   = ZT(ZoneNum)
           LoadCorrectionFactor(ZoneNum) = 1.0d0
       ELSEIF (IsZoneDV(ZoneNum) .or. IsZoneUI(ZoneNum)) THEN
           ! UCSDDV: Not fully mixed - calculate factor to correct load for fully mixed assumption
           IF (SumSysMCp > SmallMassFlow) THEN
               TempSupplyAir = SumSysMCpT / SumSysMCp   ! Non-negligible flow, calculate supply air temperature
               IF (ABS(TempSupplyAir - ZT(ZoneNum)) > TempConvergTol) THEN
                 LoadCorrectionFactor(ZoneNum) = (TempSupplyAir-Node(ZoneNodeNum)%Temp)/(TempSupplyAir-ZT(ZoneNum))
                 ! constrain value to something reasonable
                 LoadCorrectionFactor(ZoneNum) = MAX(-3.d0, LoadCorrectionFactor(ZoneNum))
                 LoadCorrectionFactor(ZoneNum) = MIN(3.d0, LoadCorrectionFactor(ZoneNum))

               ELSE
                 LoadCorrectionFactor(ZoneNum) = 1.0d0  ! Indeterminate
               ENDIF
           ELSE
               ! Negligible flow, assume mixed - reasonable lagged starting value for first step time with significant flow
               LoadCorrectionFactor(ZoneNum) = 1.0d0
           ENDIF
       ELSEIF (AirModel(ZoneNum)%SimAirModel .AND.               &
               ((AirModel(ZoneNum)%AirModelType == RoomAirModel_UserDefined)  &
                 .OR. (AirModel(ZoneNum)%AirModelType == RoomAirModel_Mundt)) ) THEN
           IF (SumSysMCp > SmallMassFlow) THEN
               TempSupplyAir = SumSysMCpT / SumSysMCp   ! Non-negligible flow, calculate supply air temperature
               IF (ABS(TempSupplyAir - ZT(ZoneNum)) > TempConvergTol) THEN
                 LoadCorrectionFactor(ZoneNum) = (TempSupplyAir-Node(ZoneNodeNum)%Temp)/(TempSupplyAir-ZT(ZoneNum))
                 ! constrain value
                 LoadCorrectionFactor(ZoneNum) = MAX(-3.d0, LoadCorrectionFactor(ZoneNum))
                 LoadCorrectionFactor(ZoneNum) = MIN(3.d0, LoadCorrectionFactor(ZoneNum))

               ELSE
                 LoadCorrectionFactor(ZoneNum) = 1.0d0  ! Indeterminate
               ENDIF
           ELSE
               ! Negligible flow, assume mixed - reasonable lagged starting value for first step time with significant flow
               LoadCorrectionFactor(ZoneNum) = 1.0d0
           ENDIF
       ELSE
         Node(ZoneNodeNum)%Temp  = ZT(ZoneNum)
         TempTstatAir(ZoneNum)   = ZT(ZoneNum)
         LoadCorrectionFactor(ZoneNum) = 1.0d0
       ENDIF

       ! Sensible load is the enthalpy into the zone minus the enthalpy that leaves the zone.
       CpAir = PsyCpAirFnWTdb(ZoneAirHumRat(ZoneNum), Node(ZoneNodeNum)%Temp)
       ZoneEnthalpyIn = SumSysMCpT

       ! SNLOAD is the single zone load, without Zone Multiplier or Zone List Multiplier
       SNLOAD = ZoneEnthalpyIn - (Node(ZoneNodeNum)%MassFlowRate / ZoneMult) * CpAir * Node(ZoneNodeNum)%Temp &
              + NonAirSystemResponse(ZoneNum) / ZoneMult + SysDepZoneLoadsLagged(ZoneNum)

    ELSE

        ! Heat balance coefficients for uncontrolled zone, i.e. without system air flow
       TempDepCoef = SumHA + SumMCp
       TempIndCoef = SumIntGain + SumHATsurf - SumHATref + SumMCpT

  !      TempHistoryTerm = AirCap * (3.0 * ZTM1(ZoneNum) - (3.0/2.0) * ZTM2(ZoneNum) + (1.0/3.0) * ZTM3(ZoneNum)) !debug only

       if (SimulateAirflowNetwork > AirflowNetworkControlMultizone) then
          TempIndCoef = TempIndCoef+AirflowNetworkExchangeData(ZoneNum)%TotalSen
       end if
  !      TempDepZnLd(ZoneNum) = (11.0/6.0) * AirCap + TempDepCoef
  !      TempIndZnLd(ZoneNum) = TempHistoryTerm + TempIndCoef

        ! Solve for zone air temperature
       SELECT CASE (ZoneAirSolutionAlgo)
         CASE (Use3rdOrder)
           ZT(ZoneNum) = (TempIndCoef + AirCap*(3.0d0*ZTM1(ZoneNum) - (3.0d0/2.0d0)*ZTM2(ZoneNum) + (1.0d0/3.0d0)*ZTM3(ZoneNum))) &
                    / ((11.0d0/6.0d0)*AirCap + TempDepCoef)
         ! Exact solution
         CASE (UseAnalyticalSolution)
           If (TempDepCoef .eq. 0.0d0) Then ! B=0
             ZT(ZoneNum) = ZoneT1(ZoneNum) + TempIndCoef/AirCap
           Else
             ZT(ZoneNum) = (ZoneT1(ZoneNum)-TempIndCoef/TempDepCoef)*exp(MIN(700.d0,-TempDepCoef/AirCap))+TempIndCoef/TempDepCoef
           End If
         CASE (UseEulerMethod)
           ZT(ZoneNum) = (AirCap*ZoneT1(ZoneNum)+TempIndCoef)/(AirCap+TempDepCoef)
       END SELECT

        ! No sensible load
        SNLOAD = 0.0d0
    END IF

    MAT(ZoneNum)  = ZT(ZoneNum)

    ! Determine sensible load heating/cooling rate and energy
    SNLoadHeatRate(ZoneNum) = MAX(SNLOAD,0.0d0)
    SNLoadCoolRate(ZoneNum) = ABS(MIN(SNLOAD,0.0d0))
    SNLoadHeatEnergy(ZoneNum) = MAX(SNLOAD,0.0d0) * TimeStepSys * SecInHour
    SNLoadCoolEnergy(ZoneNum) = ABS(MIN(SNLOAD,0.0d0) * TimeStepSys * SecInHour)

    ! Final humidity calcs
    CALL CorrectZoneHumRat(ZoneNum)

    ZoneAirHumRat(ZoneNum) = ZoneAirHumRatTemp(ZoneNum)
    ZoneAirRelHum(ZoneNum) = 100.0d0 * PsyRhFnTdbWPb(ZT(ZoneNum),ZoneAirHumRat(ZoneNum),OutBaroPress,'CorrectZoneAirTemp')

    ! ZoneTempChange is used by HVACManager to determine if the timestep needs to be shortened.
    SELECT CASE (ZoneAirSolutionAlgo)
      CASE (Use3rdOrder)
        IF (IsZoneDV(ZoneNum)) THEN
          IF (ZoneDVMixedFlag(ZoneNum)==0) THEN
            ZoneTempChange = MAX(ZoneTempChange,MAX(ABS(ZTOC(ZoneNum) - ZTM1OC(ZoneNum)),ABS(ZTMX(ZoneNum) - ZTM1MX(ZoneNum))))
          ELSE
            ZoneTempChange = MAX(ZoneTempChange, ABS(ZT(ZoneNum) - ZTM1(ZoneNum)))
          ENDIF
        ELSE IF (IsZoneUI(ZoneNum)) THEN
          IF (ZoneUFMixedFlag(ZoneNum)==0) THEN
            ZoneTempChange = MAX(ZoneTempChange,MAX(ABS(ZTOC(ZoneNum) - ZTM1OC(ZoneNum)),ABS(ZTMX(ZoneNum) - ZTM1MX(ZoneNum))))
          ELSE
            ZoneTempChange = MAX(ZoneTempChange, ABS(ZT(ZoneNum) - ZTM1(ZoneNum)))
          ENDIF
        ELSE
          ZoneTempChange = MAX(ZoneTempChange, ABS(ZT(ZoneNum) - ZTM1(ZoneNum)))
        ENDIF
      CASE (UseAnalyticalSolution,UseEulerMethod)
        IF (IsZoneDV(ZoneNum)) THEN
          IF (ZoneDVMixedFlag(ZoneNum)==0) THEN
             ZoneTempChange = MAX(ZoneTempChange,MAX(ABS(ZTOC(ZoneNum) - Zone1OC(ZoneNum)),ABS(ZTMX(ZoneNum) - Zone1MX(ZoneNum))))
           ELSE
             ZoneTempChange = MAX(ZoneTempChange, ABS(ZT(ZoneNum) - ZoneT1(ZoneNum)))
          ENDIF
        ELSE IF (IsZoneUI(ZoneNum)) THEN
          IF (ZoneUFMixedFlag(ZoneNum)==0) THEN
            ZoneTempChange = MAX(ZoneTempChange,MAX(ABS(ZTOC(ZoneNum) - Zone1OC(ZoneNum)),ABS(ZTMX(ZoneNum) - Zone1MX(ZoneNum))))
          ELSE
            ZoneTempChange = MAX(ZoneTempChange, ABS(ZT(ZoneNum) - ZoneT1(ZoneNum)))
          ENDIF
        ELSE
          ZoneTempChange = MAX(ZoneTempChange, ABS(ZT(ZoneNum) - ZoneT1(ZoneNum)))
        ENDIF
   END SELECT

   Call CalcZoneComponentLoadSums(ZoneNum,  TempDepCoef, TempIndCoef,         &
                                            ZnAirRPT(ZoneNum)%SumIntGains,    & ! convection part of internal gains
                                            ZnAirRPT(ZoneNum)%SumHADTsurfs,   & ! surface convection heat transfer
                                            ZnAirRPT(ZoneNum)%SumMCpDTzones,  & ! interzone mixing
                                            ZnAirRPT(ZoneNum)%SumMCpDtInfil,  & ! OA of various kinds except via system
                                            ZnAirRPT(ZoneNum)%SumMCpDTsystem, & ! air system
                                            ZnAirRpt(ZoneNum)%SumNonAirSystem, & ! non air system
                                            ZnAirRPT(ZoneNum)%CzdTdt ,        & ! air mass energy storage term
                                            ZnAirRPT(ZoneNum)%imBalance )       ! measure of imbalance in zone air heat balance


  END DO ! ZoneNum

  RETURN

END SUBROUTINE CorrectZoneAirTemp

SUBROUTINE PushZoneTimestepHistories

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Brent Griffith
          !       DATE WRITTEN   February 2008
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! push histories for timestep advancing

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
        XM4T(ZoneNum) = XM3T(ZoneNum)
        XM3T(ZoneNum) = XM2T(ZoneNum)
        XM2T(ZoneNum) = XMAT(ZoneNum)
        XMAT(ZoneNum) = ZTAV(ZoneNum) ! using average for whole zone time step.
        XMPT(ZoneNum)  = ZT(ZoneNum)
  !      MAT(ZoneNum)  = ZT(ZoneNum)

        WZoneTimeMinus4(ZoneNum) = WZoneTimeMinus3(ZoneNum)
        WZoneTimeMinus3(ZoneNum) = WZoneTimeMinus2(ZoneNum)
        WZoneTimeMinus2(ZoneNum) = WZoneTimeMinus1(ZoneNum)
        WZoneTimeMinus1(ZoneNum) = ZoneAirHumRatAvg(ZoneNum) ! using average for whole zone time step.
        ZoneAirHumRat(ZoneNum) = ZoneAirHumRatTemp(ZoneNum)
        WZoneTimeMinusP(ZoneNum) = ZoneAirHumRatTemp(ZoneNum)
        ZoneAirRelHum(ZoneNum) = 100.0d0 * PsyRhFnTdbWPb(ZT(ZoneNum),ZoneAirHumRat(ZoneNum),OutBaroPress,'CorrectZoneAirTemp')

        IF (AirModel(ZoneNum)%AirModelType ==  RoomAirModel_UCSDDV .or. AirModel(ZoneNum)%AirModelType ==  RoomAirModel_UCSDUFI &
            .or. AirModel(ZoneNum)%AirModelType ==  RoomAirModel_UCSDUFE) THEN
          XM4TFloor(ZoneNum) = XM3TFloor(ZoneNum)
          XM3TFloor(ZoneNum) = XM2TFloor(ZoneNum)
          XM2TFloor(ZoneNum) = XMATFloor(ZoneNum)
          XMATFloor(ZoneNum) = ZTFloor(ZoneNum)
          MATFloor(ZoneNum) = ZTFloor(ZoneNum)

          XM4TOC(ZoneNum) = XM3TOC(ZoneNum)
          XM3TOC(ZoneNum) = XM2TOC(ZoneNum)
          XM2TOC(ZoneNum) = XMATOC(ZoneNum)
          XMATOC(ZoneNum) = ZTOC(ZoneNum)
          MATOC(ZoneNum) = ZTOC(ZoneNum)

          XM4TMX(ZoneNum) = XM3TMX(ZoneNum)
          XM3TMX(ZoneNum) = XM2TMX(ZoneNum)
          XM2TMX(ZoneNum) = XMATMX(ZoneNum)
          XMATMX(ZoneNum) = ZTMX(ZoneNum)
          MATMX(ZoneNum) = ZTMX(ZoneNum)
        ENDIF

        If (ZoneAirSolutionAlgo .NE. Use3rdOrder) Then
          ZoneTM2(ZoneNum) = ZoneTMX(ZoneNum)
          ZoneTMX(ZoneNum) = ZTAV(ZoneNum) ! using average for whole zone time step.
          ZoneWM2(ZoneNum) = ZoneWMX(ZoneNum)
          ZoneWMX(ZoneNum) = ZoneAirHumRatAvg(ZoneNum) ! using average for whole zone time step.
          IF (AirModel(ZoneNum)%AirModelType ==  RoomAirModel_UCSDDV .or. AirModel(ZoneNum)%AirModelType ==  RoomAirModel_UCSDUFI &
            .or. AirModel(ZoneNum)%AirModelType ==  RoomAirModel_UCSDUFE) THEN
            ZoneM2Floor(ZoneNum) = ZoneMXFloor(ZoneNum)
            ZoneMXFloor(ZoneNum) = ZTFloor(ZoneNum) ! using average for whole zone time step.
            ZoneM2OC(ZoneNum) = ZoneMXOC(ZoneNum)
            ZoneMXOC(ZoneNum) = ZTOC(ZoneNum) ! using average for whole zone time step.
            ZoneM2MX(ZoneNum) = ZoneMXMX(ZoneNum)
            ZoneMXMX(ZoneNum) = ZTMX(ZoneNum) ! using average for whole zone time step.
          END IF
        End If
  ENDDO ! zone loop
  RETURN

END SUBROUTINE PushZoneTimestepHistories

SUBROUTINE PushSystemTimestepHistories

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Brent Griffith
          !       DATE WRITTEN   April 2008
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! push histories

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
    DSXM4T(ZoneNum) = DSXM3T(ZoneNum)
    DSXM3T(ZoneNum) = DSXM2T(ZoneNum)
    DSXM2T(ZoneNum) = DSXMAT(ZoneNum)
    DSXMAT(ZoneNum) = MAT(ZoneNum)

    DSWZoneTimeMinus4(ZoneNum) = DSWZoneTimeMinus3(ZoneNum)
    DSWZoneTimeMinus3(ZoneNum) = DSWZoneTimeMinus2(ZoneNum)
    DSWZoneTimeMinus2(ZoneNum) = DSWZoneTimeMinus1(ZoneNum)
    DSWZoneTimeMinus1(ZoneNum) = ZoneAirHumRat(ZoneNum)

    IF (IsZoneDV(ZoneNum) .or. IsZoneUI(ZoneNum)) Then
      DSXM4TFloor(ZoneNum)  = DSXM3TFloor(ZoneNum)
      DSXM3TFloor(ZoneNum)  = DSXM2TFloor(ZoneNum)
      DSXM2TFloor(ZoneNum)  = DSXMATFloor(ZoneNum)
      DSXMATFloor(ZoneNum)  = MATFloor(ZoneNum)

      DSXM4TOC(ZoneNum)     = DSXM3TOC(ZoneNum)
      DSXM3TOC(ZoneNum)     = DSXM2TOC(ZoneNum)
      DSXM2TOC(ZoneNum)     = DSXMATOC(ZoneNum)
      DSXMATOC(ZoneNum)     = MATOC(ZoneNum)

      DSXM4TMX(ZoneNum)     = DSXM3TMX(ZoneNum)
      DSXM3TMX(ZoneNum)     = DSXM2TMX(ZoneNum)
      DSXM2TMX(ZoneNum)     = DSXMATMX(ZoneNum)
      DSXMATMX(ZoneNum)     = MATMX(ZoneNum)
    ENDIF

  ENDDO ! zone loop

  If (ZoneAirSolutionAlgo .NE. Use3rdOrder) Then
    DO ZoneNum = 1, NumOfZones
      ZoneTM2(ZoneNum) = ZoneTMX(ZoneNum)
      ZoneTMX(ZoneNum) = MAT(ZoneNum) ! using average for whole zone time step.
      ZoneWM2(ZoneNum) = ZoneWMX(ZoneNum)
      ZoneWMX(ZoneNum) = ZoneAirHumRatTemp(ZoneNum) ! using average for whole zone time step.

      IF (AirModel(ZoneNum)%AirModelType ==  RoomAirModel_UCSDDV .or. AirModel(ZoneNum)%AirModelType ==  RoomAirModel_UCSDUFI &
            .or. AirModel(ZoneNum)%AirModelType ==  RoomAirModel_UCSDUFE) THEN
        ZoneM2Floor(ZoneNum) = ZoneMXFloor(ZoneNum)
        ZoneMXFloor(ZoneNum) = ZTFloor(ZoneNum) ! using average for whole zone time step.
        ZoneM2OC(ZoneNum) = ZoneMXOC(ZoneNum)
        ZoneMXOC(ZoneNum) = ZTOC(ZoneNum) ! using average for whole zone time step.
        ZoneM2MX(ZoneNum) = ZoneMXMX(ZoneNum)
        ZoneMXMX(ZoneNum) = ZTMX(ZoneNum) ! using average for whole zone time step.
      ENDIF
    ENDDO ! zone loop
  End If

  RETURN

END SUBROUTINE PushSystemTimestepHistories

SUBROUTINE RevertZoneTimestepHistories
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Brent Griffith
          !       DATE WRITTEN   February 2008
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! rewind histories to undo inadvertent pushing

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

  ! REvert the temperature and humidity ratio histories

  DO ZoneNum = 1, NumOfZones
   !  MAT(ZoneNum)  = XMAT(ZoneNum)
     XMAT(ZoneNum) = XM2T(ZoneNum)
     XM2T(ZoneNum) = XM3T(ZoneNum)
     XM3T(ZoneNum) = XM4T(ZoneNum)

  !   ZoneAirHumRat(ZoneNum)  = WZoneTimeMinus1(ZoneNum)
     WZoneTimeMinus1(ZoneNum) = WZoneTimeMinus2(ZoneNum)
     WZoneTimeMinus2(ZoneNum) = WZoneTimeMinus3(ZoneNum)
     WZoneTimeMinus3(ZoneNum) = WZoneTimeMinus4(ZoneNum)

     IF (AirModel(ZoneNum)%AirModelType ==  RoomAirModel_UCSDDV .or. AirModel(ZoneNum)%AirModelType ==  RoomAirModel_UCSDUFI &
            .or. AirModel(ZoneNum)%AirModelType ==  RoomAirModel_UCSDUFE) THEN

    !      MATFloor(ZoneNum)= XMATFloor(ZoneNum)
          XMATFloor(ZoneNum)=XM2TFloor(ZoneNum)
          XM2TFloor(ZoneNum)=XM3TFloor(ZoneNum)
          XM3TFloor(ZoneNum)=XM4TFloor(ZoneNum)
    !      MATOC(ZoneNum) = XMATOC(ZoneNum)
          XMATOC(ZoneNum)=XM2TOC(ZoneNum)
          XM2TOC(ZoneNum)=  XM3TOC(ZoneNum)
          XM3TOC(ZoneNum)=XM4TOC(ZoneNum)

     !     MATMX(ZoneNum)=  XMATMX(ZoneNum)
          XMATMX(ZoneNum)=XM2TMX(ZoneNum)
          XM2TMX(ZoneNum)=XM3TMX(ZoneNum)
          XM3TMX(ZoneNum)=XM4TMX(ZoneNum)

    ENDIF

  ENDDO ! zone loop
  RETURN

END SUBROUTINE RevertZoneTimestepHistories

SUBROUTINE CorrectZoneHumRat(ZoneNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Liesen
          !       DATE WRITTEN   2000
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine updates the zone humidities.

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
  USE DataSurfaces,  ONLY: Surface, HeatTransferModel_HAMT, HeatTransferModel_EMPD

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: ZoneNum

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
  REAL(r64) :: LatentGain ! Zone latent load
  REAL(r64) :: RhoAir
  REAL(r64) :: A
  REAL(r64) :: B
  REAL(r64) :: C
  REAL(r64) :: WZSat
  REAL(r64) :: MoistureMassFlowRate
  REAL(r64) :: ExhMassFlowRate
  REAL(r64) :: TotExitMassFlowRate
  REAL(r64) :: ZoneMassFlowRate
  REAL(r64) :: SysTimeStepInSeconds
  REAL(r64) :: H2OHtOfVap
  REAL(r64) :: ZoneMult
  INTEGER :: ADUListIndex
  INTEGER :: ADUNum
  INTEGER :: ADUInNode
  INTEGER :: ADUOutNode

          ! FLOW:
  MoistureMassFlowRate = 0.0d0
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

      MoistureMassFlowRate = MoistureMassFlowRate + &
                           (Node(ZoneEquipConfig(ZoneEquipConfigNum)%InletNode(NodeNum))%MassFlowRate * &
                            Node(ZoneEquipConfig(ZoneEquipConfigNum)%InletNode(NodeNum))%HumRat) / ZoneMult
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

     MoistureMassFlowRate = MoistureMassFlowRate + &
                           (Node(ZoneRetPlenCond(ZoneRetPlenumNum)%InletNode(NodeNum))%MassFlowRate * &
                            Node(ZoneRetPlenCond(ZoneRetPlenumNum)%InletNode(NodeNum))%HumRat) / ZoneMult
     ZoneMassFlowRate = ZoneMassFlowRate + &
                            Node(ZoneRetPlenCond(ZoneRetPlenumNum)%InletNode(NodeNum))%MassFlowRate / ZoneMult
    END DO ! NodeNum
    ! add in the leak flow
    DO ADUListIndex=1,ZoneRetPlenCond(ZoneRetPlenumNum)%NumADUs
      ADUNum = ZoneRetPlenCond(ZoneRetPlenumNum)%ADUIndex(ADUListIndex)
      IF (AirDistUnit(ADUNum)%UpStreamLeak) THEN
        ADUInNode = AirDistUnit(ADUNum)%InletNodeNum
        MoistureMassFlowRate = MoistureMassFlowRate + &
                               (AirDistUnit(ADUNum)%MassFlowRateUpStrLk * Node(ADUInNode)%HumRat) / ZoneMult
        ZoneMassFlowRate = ZoneMassFlowRate + AirDistUnit(ADUNum)%MassFlowRateUpStrLk / ZoneMult
      END IF
      IF (AirDistUnit(ADUNum)%DownStreamLeak) THEN
        ADUOutNode = AirDistUnit(ADUNum)%OutletNodeNum
        MoistureMassFlowRate = MoistureMassFlowRate + &
                               (AirDistUnit(ADUNum)%MassFlowRateDnStrLk * Node(ADUOutNode)%HumRat) / ZoneMult
        ZoneMassFlowRate = ZoneMassFlowRate + AirDistUnit(ADUNum)%MassFlowRateDnStrLk / ZoneMult
      END IF
    END DO
    ! Do not allow exhaust mass flow for a plenum zone
    ExhMassFlowRate = 0.0d0
    TotExitMassFlowRate = ExhMassFlowRate + ZoneMassFlowRate

  ELSE IF (ZoneSupPlenumAirFlag) THEN

    MoistureMassFlowRate = MoistureMassFlowRate + &
                           (Node(ZoneSupPlenCond(ZoneSupPlenumNum)%InletNode)%MassFlowRate * &
                           Node(ZoneSupPlenCond(ZoneSupPlenumNum)%InletNode)%HumRat) / ZoneMult
    ZoneMassFlowRate = ZoneMassFlowRate + &
                           Node(ZoneSupPlenCond(ZoneSupPlenumNum)%InletNode)%MassFlowRate / ZoneMult
    ! Do not allow exhaust mass flow for a plenum zone
    ExhMassFlowRate = 0.0d0
    TotExitMassFlowRate = ExhMassFlowRate + ZoneMassFlowRate
  END IF

  ! Calculate hourly humidity ratio from infiltration + humdidity added from latent load + system added moisture
  LatentGain = ZoneLatentGain(ZoneNum) + SumLatentHTRadSys(ZoneNum)

  SysTimeStepInSeconds = SecInHour * TimeStepSys

  ! Calculate the coefficients for the 3rd order derivative for final
  ! zone humidity ratio.  The A, B, C coefficients are analogous to the
  ! heat balance.  There are 2 cases that should be considered, system
  ! operating and system shutdown.
  ! SumHmARaW and SumHmARa will be used with the moisture balance on the building elements and
  ! are currently set to zero to remind us where they need to be in the future
  IF ((.NOT. ANY(Surface(Zone(ZoneNum)%SurfaceFirst:Zone(ZoneNum)%SurfaceLast)%HeatTransferAlgorithm == HeatTransferModel_EMPD))&
      .AND. &
      (.NOT. ANY(Surface(Zone(ZoneNum)%SurfaceFirst:Zone(ZoneNum)%SurfaceLast)%HeatTransferAlgorithm == HeatTransferModel_HAMT)))&
     THEN
    SumHmARaW(ZoneNum) = 0.0d0
    SumHmARa(ZoneNum) = 0.0d0
  END IF

  RhoAir = PsyRhoAirFnPbTdbW(OutBaroPress,ZT(ZoneNum),ZoneAirHumRat(ZoneNum),'CorrectZoneHumRat')
  H2OHtOfVap = PsyHgAirFnWTdb(ZoneAirHumRat(ZoneNum),ZT(ZoneNum),'CorrectZoneHumRat')

  ! Check for the flow and NO flow condition
  IF (ZoneMassFlowRate .GT. 0.0d0) THEN
    B = (LatentGain/H2OHtOfVap)+((oamfl(ZoneNum)+vamfl(ZoneNum)+eamfl(ZoneNum)+ctmfl(ZoneNum))* OutHumRat) &
                               +(MoistureMassFlowRate) &
                               +SumHmARaW(ZoneNum)  &
                               +MixingMassFlowXHumRat(ZoneNum) + MDotOA(ZoneNum) * OutHumRat
    A = TotExitMassFlowRate + oamfl(ZoneNum) + vamfl(ZoneNum) + eamfl(ZoneNum) + ctmfl(ZoneNum) &
                            + SumHmARa(ZoneNum) + MixingMassFlowZone(ZoneNum) + MDotOA(ZoneNum)
    if (SimulateAirflowNetwork == AirflowNetworkControlMultizone .OR. SimulateAirflowNetwork == AirflowNetworkControlMultiADS.OR. &
       (SimulateAirflowNetwork == AirflowNetworkControlSimpleADS .AND. AirflowNetworkFanActivated)) then
      ! Multizone airflow calculated in AirflowNetwork
      B = (LatentGain/H2OHtOfVap)+(AirflowNetworkExchangeData(ZoneNum)%SumMHrW+AirflowNetworkExchangeData(ZoneNum)%SumMMHrW)+ &
          (MoistureMassFlowRate)+SumHmARaW(ZoneNum)
      A = TotExitMassFlowRate + AirflowNetworkExchangeData(ZoneNum)%SumMHr +AirflowNetworkExchangeData(ZoneNum)%SumMMHr + &
          SumHmARa(ZoneNum)
    end if
    C = RhoAir*Zone(ZoneNum)%Volume*ZoneVolCapMultpMoist/SysTimeStepInSeconds
  ELSE IF (ZoneMassFlowRate .LE. 0.0d0) THEN
    B = (LatentGain/H2OHtOfVap)+((oamfl(ZoneNum)+vamfl(ZoneNum)+eamfl(ZoneNum)+ctmfl(ZoneNum)+ExhMassFlowRate)* OutHumRat) &
                               +SumHmARaW(ZoneNum) + MixingMassFlowXHumRat(ZoneNum)
    A = oamfl(ZoneNum) + vamfl(ZoneNum) + eamfl(ZoneNum) +ctmfl(ZoneNum) + ExhMassFlowRate + SumHmARa(ZoneNum) &
                       + MixingMassFlowZone(ZoneNum)
    if (SimulateAirflowNetwork == AirflowNetworkControlMultizone .OR. SimulateAirflowNetwork == AirflowNetworkControlMultiADS .OR. &
       (SimulateAirflowNetwork == AirflowNetworkControlSimpleADS .AND. AirflowNetworkFanActivated)) then
      ! Multizone airflow calculated in AirflowNetwork
      B = (LatentGain/H2OHtOfVap) + SumHmARaW(ZoneNum)+ &
          AirflowNetworkExchangeData(ZoneNum)%SumMHrW+AirflowNetworkExchangeData(ZoneNum)%SumMMHrW
      A = AirflowNetworkExchangeData(ZoneNum)%SumMHr+AirflowNetworkExchangeData(ZoneNum)%SumMMHr+ &
          SumHmARa(ZoneNum)
    end if
    C = RhoAir*Zone(ZoneNum)%Volume*ZoneVolCapMultpMoist/SysTimeStepInSeconds
  END IF

  if (SimulateAirflowNetwork > AirflowNetworkControlMultizone) then
     B = B+AirflowNetworkExchangeData(ZoneNum)%TotalLat
  end if

  ! Use a 3rd order derivative to predict final zone humidity ratio and
  ! smooth the changes using the zone air capacitance.
  SELECT CASE (ZoneAirSolutionAlgo)
    CASE (Use3rdOrder)
      ZoneAirHumRatTemp(ZoneNum)=(B+C*(3.0d0*WZoneTimeMinus1Temp(ZoneNum)-(3.0d0/2.0d0)*WZoneTimeMinus2Temp(ZoneNum)+ &
                             (1.0d0/3.0d0)*WZoneTimeMinus3Temp(ZoneNum)))/((11.0d0/6.0d0)*C+A)
    ! Exact solution
    CASE (UseAnalyticalSolution)
      If (A .eq. 0.0d0) Then ! B=0
        ZoneAirHumRatTemp(ZoneNum)= ZoneW1(ZoneNum) + B/C
      Else
        ZoneAirHumRatTemp(ZoneNum)= (ZoneW1(ZoneNum)-B/A)*exp(MIN(700.d0,-A/C))+B/A
      End If
    CASE (UseEulerMethod)
      ZoneAirHumRatTemp(ZoneNum) = (C*ZoneW1(ZoneNum)+B)/(C+A)
  END SELECT

  ! Set the humidity ratio to zero if the zone has been dried out
  IF (ZoneAirHumRatTemp(ZoneNum) .LT. 0.0d0) ZoneAirHumRatTemp(ZoneNum) = 0.0d0

  ! Check to make sure that is saturated there is condensation in the zone
  ! by resetting to saturation conditions.
  Wzsat = PsyWFnTdbRhPb(Zt(ZoneNum),1.0d0,OutBaroPress,'CorrectZoneHumRat')

  IF (ZoneAirHumRatTemp(ZoneNum) .GT. Wzsat) ZoneAirHumRatTemp(ZoneNum) = Wzsat

  ! Now put the calculated info into the actual zone nodes; ONLY if there is zone air flow, i.e. controlled zone or plenum zone
  ZoneNodeNum = Zone(ZoneNum)%SystemZoneNodeNumber
  IF (ZoneNodeNum > 0) THEN
    Node(ZoneNodeNum)%HumRat = ZoneAirHumRatTemp(ZoneNum)
    Node(ZoneNodeNum)%Enthalpy = PsyHFnTdbW(ZT(ZoneNum),ZoneAirHumRatTemp(ZoneNum))
  END IF

  RETURN

END SUBROUTINE CorrectZoneHumRat

SUBROUTINE DownInterpolate4HistoryValues ( OldTimeStep, NewTimeStep,                   &
                                        oldVal0, oldVal1, oldVal2, oldVal3, oldVal4,   &
                                        newVal0, newVal1, newVal2, newVal3, newVal4)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Brent Griffith
          !       DATE WRITTEN   Feb 2008
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! provide a reusable routine for the various places that need to
          ! interpolate a new set of history values on a different time scale
          ! Once the systemtimestep has shortened, the new history terms need to be interpolated
          !

          ! METHODOLOGY EMPLOYED:
          ! This routine assumes that the direction is to a shorter timestep.
          ! The down step ratio, DSRatio = OldTimeStep/ NewTimeStep
          !  is expected to be roughly integer-valued and near 2.0 or 3.0 or 4.0 or more.
          !

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  REAL(r64), INTENT(IN)    :: OldTimeStep
  REAL(r64), INTENT(IN)    :: NewTimeStep
  REAL(r64), INTENT(INOUT) :: oldVal0
  REAL(r64), INTENT(INOUT) :: oldVal1
  REAL(r64), INTENT(INOUT) :: oldVal2
  REAL(r64), INTENT(INOUT) :: oldVal3
  REAL(r64), INTENT(INOUT) :: oldVal4
  REAL(r64), INTENT(INOUT) :: newVal0
  REAL(r64), INTENT(INOUT) :: newVal1
  REAL(r64), INTENT(INOUT) :: newVal2
  REAL(r64), INTENT(INOUT) :: newVal3  ! unused 1208
  REAL(r64), INTENT(INOUT) :: newVal4  ! unused 1208

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64) :: oldTime0
  REAL(r64) :: oldTime1
  REAL(r64) :: oldTime2
  REAL(r64) :: oldTime3
  REAL(r64) :: oldTime4
  REAL(r64) :: newTime0
  REAL(r64) :: newTime1
  REAL(r64) :: newTime2
  REAL(r64) :: newTime3
  REAL(r64) :: newTime4

  REAL(r64) :: DSRatio

  ! first construct data on timestamps for interpolating with later
  oldTime0 = 0.0D0
  oldTime1 = oldTime0 - OldTimeStep
  oldTime2 = oldTime1 - OldTimeStep
  oldTime3 = oldTime2 - OldTimeStep
  oldTime4 = oldTime3 - OldTimeStep

  newTime0 = 0.0D0
  newTime1 = newTime0 - NewTimeStep
  newTime2 = newTime1 - NewTimeStep
  newTime3 = newTime2 - NewTimeStep
  newTime4 = newTime3 - NewTimeStep

  DSRatio = OldTimeStep/ NewTimeStep ! should pretty much be an integer value 2, 3, 4, etc.

  newVal0 = oldVal0

  IF (ABS(DSRatio - 2.0D0) < 0.01D0) THEN  ! DSRatio = 2
    ! first two points lie between oldVal0 and oldVal1
    newVal1 = oldVal0 + (oldVal1 - oldVal0) * ((oldTime0 - newTime1) / (OldTimeStep))
    newVal2 = oldVal0 + (oldVal1 - oldVal0) * ((oldTime0 - newTime2) / (OldTimeStep))
    ! last two points lie between oldVal1 and oldVal2
    newVal3 = oldVal1 + (oldVal2 - oldVal1) * ((oldTime1 - newTime3)/ (OldTimeStep))
    newVal4 = oldVal1 + (oldVal2 - oldVal1) * ((oldTime1 - newTime4)/ (OldTimeStep))
  ELSE IF (ABS(DSRatio - 3.0D0) < 0.01D0) THEN  ! DSRatio = 3
    ! first three points lie between oldVal0 and oldVal1
    newVal1 = oldVal0 + (oldVal1 - oldVal0) * ((oldTime0 - newTime1) / (OldTimeStep))
    newVal2 = oldVal0 + (oldVal1 - oldVal0) * ((oldTime0 - newTime2) / (OldTimeStep))
    newVal3 = oldVal0 + (oldVal1 - oldVal0) * ((oldTime0 - newTime3) / (OldTimeStep))
    ! last point lie between oldVal1 and oldVal2
    newVal4 = oldVal1 + (oldVal2 - oldVal1) * ((oldTime1 - newTime4)/ (OldTimeStep))

  ELSE IF (DSRatio > 3.99D0) THEN ! DSRatio = 4 or more
    !all new points lie between oldVal0 and oldVal1
    newVal1 = oldVal0 + (oldVal1 - oldVal0) * ((oldTime0 - newTime1) / (OldTimeStep))
    newVal2 = oldVal0 + (oldVal1 - oldVal0) * ((oldTime0 - newTime2) / (OldTimeStep))
    newVal3 = oldVal0 + (oldVal1 - oldVal0) * ((oldTime0 - newTime3) / (OldTimeStep))
    newVal4 = oldVal0 + (oldVal1 - oldVal0) * ((oldTime0 - newTime4) / (OldTimeStep))
  ENDIF

  RETURN
END SUBROUTINE DownInterpolate4HistoryValues

SUBROUTINE CalcZoneSums(ZoneNum, SumIntGain, SumHA, SumHATsurf, SumHATref, SumMCp, SumMCpT, SumSysMCp, SumSysMCpT)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Peter Graham Ellis
          !       DATE WRITTEN   July 2003
          !       MODIFIED       Aug 2003, FCW: add SumHA contributions from window frame and divider
          !                      Aug 2003, CC: change how the reference temperatures are used
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine calculates the various sums that go into the zone heat balance
          ! equation.  This replaces the SUMC, SUMHA, and SUMHAT calculations that were
          ! previously done in various places throughout the program.
          ! The SumHAT portion of the code is reproduced in RadiantSystemHighTemp and
          ! RadiantSystemLowTemp and should be updated accordingly.
          !
          ! A reference temperature (Tref) is specified for use with the ceiling diffuser
          ! convection correlation.  A bogus value of Tref = -999.9 defaults to using
          ! the zone air (i.e. outlet) temperature for the reference temperature.
          ! If Tref is applied to all surfaces, SumHA = 0, and SumHATref /= 0.
          ! If Tref is not used at all, SumHATref = 0, and SumHA /= 0.
          !
          ! For future implementations, Tref can be easily converted into an array to
          ! allow a different reference temperature to be specified for each surface.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataSurfaces
  USE DataHeatBalance
  USE DataHeatBalSurface
  USE DataLoopNode, ONLY: Node
  USE DataZoneEquipment, ONLY: ZoneEquipConfig
  USE ZonePlenum, ONLY: ZoneRetPlenCond, ZoneSupPlenCond, NumZoneReturnPlenums, NumZoneSupplyPlenums
  USE DataDefineEquip, ONLY: AirDistUnit, NumAirDistUnits
  USE InternalHeatGains, ONLY: SumAllInternalConvectionGains, SumAllReturnAirConvectionGains

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: ZoneNum               ! Zone number
  REAL(r64), INTENT(OUT)   :: SumIntGain            ! Zone sum of convective internal gains
  REAL(r64), INTENT(OUT)   :: SumHA                 ! Zone sum of Hc*Area
  REAL(r64), INTENT(OUT)   :: SumHATsurf            ! Zone sum of Hc*Area*Tsurf
  REAL(r64), INTENT(OUT)   :: SumHATref             ! Zone sum of Hc*Area*Tref, for ceiling diffuser convection correlation
  REAL(r64), INTENT(OUT)   :: SumMCp                ! Zone sum of MassFlowRate*Cp
  REAL(r64), INTENT(OUT)   :: SumMCpT               ! Zone sum of MassFlowRate*Cp*T
  REAL(r64), INTENT(OUT)   :: SumSysMCp             ! Zone sum of air system MassFlowRate*Cp
  REAL(r64), INTENT(OUT)   :: SumSysMCpT            ! Zone sum of air system MassFlowRate*Cp*T

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER             :: NodeNum               ! System node number
  REAL(r64)           :: NodeTemp              ! System node temperature
  REAL(r64)           :: MassFlowRate          ! System node mass flow rate
  INTEGER             :: ZoneEquipConfigNum
  LOGICAL             :: ControlledZoneAirFlag
  INTEGER             :: ZoneRetPlenumNum
  INTEGER             :: ZoneSupPlenumNum
  LOGICAL             :: ZoneRetPlenumAirFlag
  LOGICAL             :: ZoneSupPlenumAirFlag
  REAL(r64)           :: CpAir                 ! Specific heat of air
  INTEGER             :: SurfNum               ! Surface number
  REAL(r64)           :: HA                    ! Hc*Area
  REAL(r64)           :: Area                  ! Effective surface area
  REAL(r64)           :: RefAirTemp            ! Reference air temperature for surface convection calculations
  REAL(r64)           :: ZoneMult
  INTEGER             :: ADUListIndex
  INTEGER             :: ADUNum
  INTEGER             :: ADUInNode
  INTEGER             :: ADUOutNode
  REAL(r64)           :: RetAirGain

          ! FLOW:
  SumIntGain = 0.0d0
  SumHA = 0.0d0
  SumHATsurf = 0.0d0
  SumHATref = 0.0d0
  SumMCp = 0.0d0
  SumMCpT = 0.0d0
  SumSysMCp = 0.0d0
  SumSysMCpT = 0.0d0

  ! Sum all convective internal gains: SumIntGain

  CALL SumAllInternalConvectionGains(ZoneNum, SumIntGain)
  SumIntGain = SumIntGain   + SumConvHTRadSys(ZoneNum)

  ! Add heat to return air if zonal system (no return air) or cycling system (return air frequently very
  ! low or zero)
  IF (Zone(ZoneNum)%NoHeatToReturnAir) THEN
    CALL SumAllReturnAirConvectionGains(ZoneNum, RetAirGain)
    SumIntGain = SumIntGain + RetAirGain
  END IF

  ! Sum all non-system air flow, i.e. infiltration, simple ventilation, mixing, earth tube: SumMCp, SumMCpT
  SumMCp = MCPI(ZoneNum) + MCPV(ZoneNum) + MCPM(ZoneNum) + MCPE(ZoneNum) + MCPC(ZoneNum) + MdotCPOA(ZoneNum)
  SumMCpT = MCPTI(ZoneNum) + MCPTV(ZoneNum) + MCPTM(ZoneNum) + MCPTE(ZoneNum) + MCPTC(ZoneNum) + &
            MdotCPOA(ZoneNum)*Zone(ZoneNum)%OutDryBulbTemp

  ! Sum all multizone air flow calculated from AirflowNetwork by assuming no simple air infiltration model
  if (SimulateAirflowNetwork == AirflowNetworkControlMultizone .OR. SimulateAirflowNetwork == AirflowNetworkControlMultiADS .OR. &
     (SimulateAirflowNetwork == AirflowNetworkControlSimpleADS .AND. AirflowNetworkFanActivated)) then
    ! Multizone airflow calculated in AirflowNetwork
    SumMCp = AirflowNetworkExchangeData(ZoneNum)%SumMCp+AirflowNetworkExchangeData(ZoneNum)%SumMMCp
    SumMCpT = AirflowNetworkExchangeData(ZoneNum)%SumMCpT+AirflowNetworkExchangeData(ZoneNum)%SumMMCpT
  end if

  ! Sum all system air flow: SumSysMCp, SumSysMCpT
  ! Check to see if this is a controlled zone

  ControlledZoneAirFlag = .FALSE.
!  If (Zone(ZoneNum)%IsControlled) Then   ! more CR 7384
!    ControlledZoneAirFlag = .TRUE.       ! more CR 7384
!    ZoneEquipConfigNum = ZoneNum         ! more CR 7384
!  endif
    ! BG feb 2008 repeating this do loop every time seems crazy, store ControlledZoneAirFlag in Zone structure?
  DO ZoneEquipConfigNum = 1, NumOfZones
    IF (.not. Zone(ZoneEquipConfigNum)%IsControlled) CYCLE
    IF (ZoneEquipConfig(ZoneEquipConfigNum)%ActualZoneNum /= ZoneNum) CYCLE
    ControlledZoneAirFlag = .TRUE.
    EXIT !sloppy way of finding ZoneEquipConfigNum for later use.
  END DO ! ZoneEquipConfigNum

  ! Check to see if this is a plenum zone
  ! BG feb 2008 repeating this do loop every time seems crazy, store ControlledZoneAirFlag in Zone structure?
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

  ! Plenum and controlled zones have a different set of inlet nodes which must be calculated.
  IF (ControlledZoneAirFlag) THEN
    DO NodeNum = 1, ZoneEquipConfig(ZoneEquipConfigNum)%NumInletNodes
      ! Get node conditions
      !  this next block is of interest to irratic system loads... maybe nodes are not accurate at time of call?
      !  how can we tell?  predict step must be lagged ?  correct step, systems have run.
      NodeTemp = Node(ZoneEquipConfig(ZoneEquipConfigNum)%InletNode(NodeNum))%Temp
      MassFlowRate = Node(ZoneEquipConfig(ZoneEquipConfigNum)%InletNode(NodeNum))%MassFlowRate
      CpAir = PsyCpAirFnWTdb(ZoneAirHumRat(ZoneNum), NodeTemp)

      SumSysMCp = SumSysMCp + MassFlowRate * CpAir
      SumSysMCpT = SumSysMCpT + MassFlowRate * CpAir * NodeTemp
    END DO ! NodeNum

  ELSE IF (ZoneRetPlenumAirFlag) THEN
    DO NodeNum = 1, ZoneRetPlenCond(ZoneRetPlenumNum)%NumInletNodes
      ! Get node conditions
      NodeTemp = Node(ZoneRetPlenCond(ZoneRetPlenumNum)%InletNode(NodeNum))%Temp
      MassFlowRate = Node(ZoneRetPlenCond(ZoneRetPlenumNum)%InletNode(NodeNum))%MassFlowRate
      CpAir = PsyCpAirFnWTdb(ZoneAirHumRat(ZoneNum), NodeTemp)

      SumSysMCp = SumSysMCp + MassFlowRate * CpAir
      SumSysMCpT = SumSysMCpT + MassFlowRate * CpAir * NodeTemp
    END DO ! NodeNum
    ! add in the leaks
    DO ADUListIndex=1,ZoneRetPlenCond(ZoneRetPlenumNum)%NumADUs
      ADUNum = ZoneRetPlenCond(ZoneRetPlenumNum)%ADUIndex(ADUListIndex)
      IF (AirDistUnit(ADUNum)%UpStreamLeak) THEN
        ADUInNode = AirDistUnit(ADUNum)%InletNodeNum
        NodeTemp = Node(ADUInNode)%Temp
        MassFlowRate = AirDistUnit(ADUNum)%MassFlowRateUpStrLk
        CpAir = PsyCpAirFnWTdb(ZoneAirHumRat(ZoneNum), NodeTemp)
        SumSysMCp = SumSysMCp + MassFlowRate * CpAir
        SumSysMCpT = SumSysMCpT + MassFlowRate * CpAir * NodeTemp
      END IF
      IF (AirDistUnit(ADUNum)%DownStreamLeak) THEN
        ADUOutNode = AirDistUnit(ADUNum)%OutletNodeNum
        NodeTemp = Node(ADUOutNode)%Temp
        MassFlowRate = AirDistUnit(ADUNum)%MassFlowRateDnStrLk
        CpAir = PsyCpAirFnWTdb(ZoneAirHumRat(ZoneNum), NodeTemp)
        SumSysMCp = SumSysMCp + MassFlowRate * CpAir
        SumSysMCpT = SumSysMCpT + MassFlowRate * CpAir * NodeTemp
      END IF
    END DO

  ELSE IF (ZoneSupPlenumAirFlag) THEN
    ! Get node conditions
    NodeTemp = Node(ZoneSupPlenCond(ZoneSupPlenumNum)%InletNode)%Temp
    MassFlowRate = Node(ZoneSupPlenCond(ZoneSupPlenumNum)%InletNode)%MassFlowRate
    CpAir = PsyCpAirFnWTdb(ZoneAirHumRat(ZoneNum), NodeTemp)

    SumSysMCp = SumSysMCp + MassFlowRate * CpAir
    SumSysMCpT = SumSysMCpT + MassFlowRate * CpAir * NodeTemp

  END IF

  ZoneMult = Zone(ZoneNum)%Multiplier * Zone(ZoneNum)%ListMultiplier

  SumSysMCp = SumSysMCp / ZoneMult
  SumSysMCpT = SumSysMCpT / ZoneMult

  ! Sum all surface convection: SumHA, SumHATsurf, SumHATref (and additional contributions to SumIntGain)
  DO SurfNum = Zone(ZoneNum)%SurfaceFirst,Zone(ZoneNum)%SurfaceLast

    IF (.NOT. Surface(SurfNum)%HeatTransSurf) CYCLE ! Skip non-heat transfer surfaces

    HA = 0.0d0
    Area = Surface(SurfNum)%Area  ! For windows, this is the glazing area

    IF (Surface(SurfNum)%Class == SurfaceClass_Window) THEN

      ! Add to the convective internal gains
      IF (SurfaceWindow(SurfNum)%ShadingFlag == IntShadeOn .OR. SurfaceWindow(SurfNum)%ShadingFlag == IntBlindOn) THEN
        ! The shade area covers the area of the glazing plus the area of the dividers.
        Area = Area + SurfaceWindow(SurfNum)%DividerArea
        ! If interior shade or blind is present it is assumed that both the convective and IR radiative gain
        ! from the inside surface of the divider goes directly into the zone air -- i.e., the IR radiative
        ! interaction between divider and shade or blind is ignored due to the difficulty of calculating this interaction
        ! at the same time that the interaction between glass and shade is calculated.
        SumIntGain = SumIntGain + SurfaceWindow(SurfNum)%DividerConduction
      END IF

      ! Other convection term is applicable to equivalent layer window (ASHWAT) model
      IF (Construct(Surface(SurfNum)%Construction)%WindowTypeEQL) &
         SumIntGain = SumIntGain + SurfaceWindow(SurfNum)%OtherConvHeatGain

      ! Convective heat gain from natural convection in gap between glass and interior shade or blind
      IF (SurfaceWindow(SurfNum)%ShadingFlag == IntShadeOn &
        .OR. SurfaceWindow(SurfNum)%ShadingFlag == IntBlindOn) &
        SumIntGain = SumIntGain + SurfaceWindow(SurfNum)%ConvHeatFlowNatural

      ! Convective heat gain from airflow window
      IF (SurfaceWindow(SurfNum)%AirFlowThisTS > 0.0d0) THEN
        SumIntGain = SumIntGain + SurfaceWindow(SurfNum)%ConvHeatGainToZoneAir
        IF (Zone(ZoneNum)%NoHeatToReturnAir) THEN
          SumIntGain = SumIntGain + SurfaceWindow(SurfNum)%RetHeatGainToZoneAir
          WinHeatGain(SurfNum) = WinHeatGain(SurfNum) + SurfaceWindow(SurfNum)%RetHeatGainToZoneAir
          IF(WinHeatGain(SurfNum) >= 0.0d0) THEN
            WinHeatGainRep(SurfNum) = WinHeatGain(SurfNum)
            WinHeatGainRepEnergy(SurfNum) = WinHeatGainRep(SurfNum) * TimeStepZone * SecInHour
          ELSE
            WinHeatLossRep(SurfNum) = -WinHeatGain(SurfNum)
            WinHeatLossRepEnergy(SurfNum) = WinHeatLossRep(SurfNum) * TimeStepZone * SecInHour
          END IF
        END IF
      END IF

      ! Add to the surface convection sums
      IF (SurfaceWindow(SurfNum)%FrameArea > 0.0d0) THEN
        ! Window frame contribution
        SumHATsurf = SumHATsurf + HConvIn(SurfNum) * SurfaceWindow(SurfNum)%FrameArea &
          * (1.0d0 + SurfaceWindow(SurfNum)%ProjCorrFrIn) * SurfaceWindow(SurfNum)%FrameTempSurfIn
        HA = HA + HConvIn(SurfNum) * SurfaceWindow(SurfNum)%FrameArea * (1.0d0 + SurfaceWindow(SurfNum)%ProjCorrFrIn)
      END IF

      IF (SurfaceWindow(SurfNum)%DividerArea > 0.0d0 .AND. SurfaceWindow(SurfNum)%ShadingFlag /= IntShadeOn &
           .AND. SurfaceWindow(SurfNum)%ShadingFlag /= IntBlindOn) THEN
        ! Window divider contribution (only from shade or blind for window with divider and interior shade or blind)
        SumHATsurf = SumHATsurf + HConvIn(SurfNum) * SurfaceWindow(SurfNum)%DividerArea &
          * (1.0d0 + 2.0d0 * SurfaceWindow(SurfNum)%ProjCorrDivIn) * SurfaceWindow(SurfNum)%DividerTempSurfIn
        HA = HA + HConvIn(SurfNum) * SurfaceWindow(SurfNum)%DividerArea * (1.0d0 + 2.0d0 * SurfaceWindow(SurfNum)%ProjCorrDivIn)
      END IF

    END IF  ! End of check if window

    HA = HA + HConvIn(SurfNum) * Area
    SumHATsurf = SumHATsurf + HConvIn(SurfNum) * Area * TempSurfInTmp(SurfNum)

    ! determine reference air temperature for this surface
    SELECT CASE (Surface(SurfNum)%TAirRef)
        CASE (ZoneMeanAirTemp)
            ! The zone air is the reference temperature (which is to be solved for in CorrectZoneAirTemp).
            RefAirTemp = MAT(ZoneNum)
            SumHA      = SumHA + HA
        CASE (AdjacentAirTemp)
            RefAirTemp = TempEffBulkAir(SurfNum)
            SumHATref = SumHATref + HA * RefAirTemp
        CASE (ZoneSupplyAirTemp)
            ! check whether this zone is a controlled zone or not
            IF (.NOT.ControlledZoneAirFlag) THEN
                CALL ShowFatalError('Zones must be controlled for Ceiling-Diffuser Convection model. No system serves zone '//  &
                                    TRIM(Zone(ZoneNum)%Name))
                RETURN
            END IF
            ! determine supply air temperature as a weighted average of the inlet temperatures.
            IF (SumSysMCp > 0.0d0) THEN
              RefAirTemp = SumSysMCpT/SumSysMCp
            ELSE
              ! no system flow (yet) so just use last value for inlet node temp, this can happen early in the environment
              RefAirTemp = NodeTemp
            ENDIF
            SumHATref = SumHATref + HA * RefAirTemp
        CASE DEFAULT
            ! currently set to mean air temp but should add error warning here
            RefAirTemp = MAT(ZoneNum)
            SumHA      = SumHA + HA
    END SELECT

  END DO ! SurfNum

  RETURN

END SUBROUTINE CalcZoneSums

SUBROUTINE CalcZoneComponentLoadSums(ZoneNum, TempDepCoef, TempIndCoef, SumIntGains, SumHADTsurfs, SumMCpDTzones, &
          SumMCpDtInfil, SumMCpDTsystem, SumNonAirSystem, CzdTdt , imBalance )

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Brent Griffith
          !       DATE WRITTEN   Feb 2008
          !       MODIFIED
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine calculates the various sums that go into the zone heat balance
          ! equation for reporting (and diagnostic) purposes only.
          ! It was derived from CalcZoneSums but differs in that that routine
          ! breaks up the component's dependence on zone air temp in order to *solve* for zone air temp,
          ! but here we *use* the result for zone air temp and calculate the terms of the heat balance
          ! Go back and calculate each of the 6 terms in Equation 5 and fill report variables.
          ! notes on these raw terms for zone air heat balance model :
          !  these are state variables at the end of the last system timestep.
          !  they are not necessarily proper averages for what happend over entire zone time step
          !  these are not mulitplied by zone multipliers.
          !  The values are all Watts.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! Equation 5 in Engineering Reference.
          !

          ! USE STATEMENTS:
  USE DataSurfaces
  USE DataHeatBalance
  USE DataHeatBalSurface
  USE DataLoopNode, ONLY: Node
  USE DataZoneEquipment, ONLY: ZoneEquipConfig
  USE ZonePlenum, ONLY: ZoneRetPlenCond, ZoneSupPlenCond, NumZoneReturnPlenums, NumZoneSupplyPlenums
  USE DataDefineEquip, ONLY: AirDistUnit, NumAirDistUnits
  USE General, ONLY: RoundSigDigits
  USE InternalHeatGains, ONLY: SumAllInternalConvectionGains, SumAllReturnAirConvectionGains

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: ZoneNum            ! Zone number
  REAL(r64), INTENT(IN)  :: TempDepCoef     ! Dependent coefficient
  REAL(r64), INTENT(IN)  :: TempIndCoef     ! Independent coefficient
  REAL(r64), INTENT(OUT) :: SumIntGains     ! Zone sum of convective internal gains
  REAL(r64), INTENT(OUT) :: SumHADTsurfs    ! Zone sum of Hc*Area*(Tsurf - Tz)
  REAL(r64), INTENT(OUT) :: SumMCpDTzones   ! zone sum of MassFlowRate*cp*(TremotZone - Tz) transfer air from other zone, Mixing
  REAL(r64), INTENT(OUT) :: SumMCpDtInfil   ! Zone sum of MassFlowRate*Cp*(Tout - Tz) transfer from outside, ventil, earth tube
  REAL(r64), INTENT(OUT) :: SumMCpDTsystem  ! Zone sum of air system MassFlowRate*Cp*(Tsup - Tz)
  REAL(r64), INTENT(OUT) :: SumNonAirSystem ! Zone sum of non air system convective heat gains
  REAL(r64), INTENT(OUT) :: CzdTdt          ! Zone air energy storage term.
  REAL(r64), INTENT(OUT) :: imBalance       ! put all terms in eq. 5 on RHS , should be zero

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER             :: NodeNum               ! System node number
  REAL(r64)           :: NodeTemp              ! System node temperature
  REAL(r64)           :: MassFlowRate          ! System node mass flow rate
  INTEGER             :: ZoneEquipConfigNum
  LOGICAL             :: ControlledZoneAirFlag
  INTEGER             :: ZoneRetPlenumNum
  INTEGER             :: ZoneSupPlenumNum
  LOGICAL             :: ZoneRetPlenumAirFlag
  LOGICAL             :: ZoneSupPlenumAirFlag
  REAL(r64)           :: RhoAir
  REAL(r64)           :: CpAir                 ! Specific heat of air
  INTEGER             :: SurfNum               ! Surface number
!unused  REAL(r64)           :: HA                    ! Hc*Area
  REAL(r64)           :: Area                  ! Effective surface area
  REAL(r64)           :: RefAirTemp            ! Reference air temperature for surface convection calculations
!unused  LOGICAL             :: FirstTimeFlag
!unused  INTEGER             :: Tref           ! Used to check if reference air temp for all surfaces in the zone are the same
!unused  REAL(r64)           :: ZoneMult
  INTEGER             :: ADUListIndex
  INTEGER             :: ADUNum
  INTEGER             :: ADUInNode
  INTEGER             :: ADUOutNode
  REAL(r64)           :: SumSysMCp
  REAL(r64)           :: SumSysMCpT
  REAL(r64)           :: Threshold
  REAL(r64)           :: SumRetAirGains

  SumIntGains    = 0.0D0    ! Zone sum of convective internal gains
  SumHADTsurfs   = 0.0D0    ! Zone sum of Hc*Area*(Tsurf - Tz)
  SumMCpDTzones  = 0.0D0    ! zone sum of MassFlowRate*cp*(TremotZone - Tz) transfer air from other zone, Mixing
  SumMCpDtInfil  = 0.0D0    ! Zone sum of MassFlowRate*Cp*(Tout - Tz)
  SumMCpDTsystem = 0.0D0    ! Zone sum of air system MassFlowRate*Cp*(Tsup - Tz)
  SumNonAirSystem= 0.0D0    !
  CzdTdt         = 0.0D0    !
  imBalance      = 0.0D0
  SumSysMCp      = 0.0d0
  SumSysMCpT     = 0.0d0

  ! Sum all convective internal gains: SumIntGain
  Call SumAllInternalConvectionGains(ZoneNum, SumIntGains)

  ! Add heat to return air if zonal system (no return air) or cycling system (return air frequently very
  ! low or zero)
  IF (Zone(ZoneNum)%NoHeatToReturnAir) THEN
   Call SumAllReturnAirConvectionGains(ZoneNum, SumRetAirGains)
   SumIntGains = SumIntGains + SumRetAirGains
  END IF

  ! sum non-system air flow transfers between zones
  SumMCpDTzones = MCPTM(ZoneNum) - MCPM(ZoneNum)* MAT(ZoneNum)  ! but maybe it should be ZTAV(ZoneNum)

  ! Sum non-system air flow, i.e. infiltration, simple ventilation, earth tube
  !  reuse SumMCp, SumMCpT from CalcZoneSum but use MAT (or maybe ZTAV?) to complete
  SumMCpDtInfil =  (MCPTI(ZoneNum) -  MCPI(ZoneNum)* MAT(ZoneNum)) &  ! infiltration
                 + (MCPTV(ZoneNum) -  MCPV(ZoneNum)* MAT(ZoneNum)) &  ! Ventilation (simple)
                 + (MCPTE(ZoneNum) -  MCPE(ZoneNum)* MAT(ZoneNum)) &  ! Earth tube.
                 + (MCPTC(ZoneNum) -  MCPC(ZoneNum)* MAT(ZoneNum)) &   ! Cooltower
                 + (MDotCPOA(ZoneNum)*Zone(ZoneNum)%OutDryBulbTemp -  MDotCPOA(ZoneNum)* MAT(ZoneNum))   ! combined OA flow

  ! Sum all multizone air flow calculated from AirflowNetwork by assuming no simple air infiltration model (if used)
  IF (SimulateAirflowNetwork == AirflowNetworkControlMultizone .OR. SimulateAirflowNetwork == AirflowNetworkControlMultiADS .OR. &
     (SimulateAirflowNetwork == AirflowNetworkControlSimpleADS .AND. AirflowNetworkFanActivated)) THEN
    ! Multizone airflow calculated in AirflowNetwork
    SumMCpDtInfil = AirflowNetworkExchangeData(ZoneNum)%SumMCpT - AirflowNetworkExchangeData(ZoneNum)%SumMCp * MAT(ZoneNum)
    SumMCpDTzones = AirflowNetworkExchangeData(ZoneNum)%SumMMCpT - AirflowNetworkExchangeData(ZoneNum)%SumMMCp * MAT(ZoneNum)
  END IF

  ! Sum all system air flow: reusing how SumSysMCp, SumSysMCpT are calculated in CalcZoneSums
  ! Check to see if this is a controlled zone

  ! CR 7384 continuation needed below.  eliminate do loop for speed and clarity
  ControlledZoneAirFlag = .FALSE.
  DO ZoneEquipConfigNum = 1, NumOfZones
    IF (.not. ZoneEquipConfig(ZoneEquipConfigNum)%IsControlled) CYCLE
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

  ! Plenum and controlled zones have a different set of inlet nodes which must be calculated.
  IF (ControlledZoneAirFlag) THEN
    DO NodeNum = 1, ZoneEquipConfig(ZoneEquipConfigNum)%NumInletNodes
      ! Get node conditions
      NodeTemp = Node(ZoneEquipConfig(ZoneEquipConfigNum)%InletNode(NodeNum))%Temp
      MassFlowRate = Node(ZoneEquipConfig(ZoneEquipConfigNum)%InletNode(NodeNum))%MassFlowRate
      CpAir = PsyCpAirFnWTdb(ZoneAirHumRat(ZoneNum), NodeTemp)

      SumMCpDTsystem = SumMCpDTsystem + MassFlowRate * CpAir * (NodeTemp - MAT(ZoneNum))

    END DO ! NodeNum

  ELSE IF (ZoneRetPlenumAirFlag) THEN
    DO NodeNum = 1, ZoneRetPlenCond(ZoneRetPlenumNum)%NumInletNodes
      ! Get node conditions
      NodeTemp = Node(ZoneRetPlenCond(ZoneRetPlenumNum)%InletNode(NodeNum))%Temp
      MassFlowRate = Node(ZoneRetPlenCond(ZoneRetPlenumNum)%InletNode(NodeNum))%MassFlowRate
      CpAir = PsyCpAirFnWTdb(ZoneAirHumRat(ZoneNum), NodeTemp)

      SumMCpDTsystem = SumMCpDTsystem + MassFlowRate * CpAir * (NodeTemp - MAT(ZoneNum))

    END DO ! NodeNum
    ! add in the leaks
    DO ADUListIndex=1,ZoneRetPlenCond(ZoneRetPlenumNum)%NumADUs
      ADUNum = ZoneRetPlenCond(ZoneRetPlenumNum)%ADUIndex(ADUListIndex)
      IF (AirDistUnit(ADUNum)%UpStreamLeak) THEN
        ADUInNode = AirDistUnit(ADUNum)%InletNodeNum
        NodeTemp = Node(ADUInNode)%Temp
        MassFlowRate = AirDistUnit(ADUNum)%MassFlowRateUpStrLk
        CpAir = PsyCpAirFnWTdb(ZoneAirHumRat(ZoneNum), NodeTemp)
        SumMCpDTsystem = SumMCpDTsystem + MassFlowRate * CpAir * (NodeTemp - MAT(ZoneNum))
      END IF
      IF (AirDistUnit(ADUNum)%DownStreamLeak) THEN
        ADUOutNode = AirDistUnit(ADUNum)%OutletNodeNum
        NodeTemp = Node(ADUOutNode)%Temp
        MassFlowRate = AirDistUnit(ADUNum)%MassFlowRateDnStrLk
        CpAir = PsyCpAirFnWTdb(ZoneAirHumRat(ZoneNum), NodeTemp)
        SumMCpDTsystem = SumMCpDTsystem + MassFlowRate * CpAir * (NodeTemp - MAT(ZoneNum))
      END IF
    END DO

  ELSE IF (ZoneSupPlenumAirFlag) THEN
    ! Get node conditions
    NodeTemp = Node(ZoneSupPlenCond(ZoneSupPlenumNum)%InletNode)%Temp
    MassFlowRate = Node(ZoneSupPlenCond(ZoneSupPlenumNum)%InletNode)%MassFlowRate
    CpAir = PsyCpAirFnWTdb(ZoneAirHumRat(ZoneNum), NodeTemp)

    SumMCpDTsystem = SumMCpDTsystem + MassFlowRate * CpAir * (NodeTemp - MAT(ZoneNum))

  END IF

  ! non air system response.
  SumNonAirSystem  = NonAirSystemResponse(ZoneNum) + SumConvHTRadSys(ZoneNum)

  ! Sum all surface convection: SumHA, SumHATsurf, SumHATref (and additional contributions to SumIntGain)
  DO SurfNum = Zone(ZoneNum)%SurfaceFirst,Zone(ZoneNum)%SurfaceLast

    IF (.NOT. Surface(SurfNum)%HeatTransSurf) CYCLE ! Skip non-heat transfer surfaces

    Area = Surface(SurfNum)%Area  ! For windows, this is the glazing area
    ! determine reference air temperature for this surface's convective heat transfer model
    SELECT CASE (Surface(SurfNum)%TAirRef)
        CASE (ZoneMeanAirTemp)
            ! The zone air is the reference temperature
            RefAirTemp = MAT(ZoneNum)
        CASE (AdjacentAirTemp)
            RefAirTemp = TempEffBulkAir(SurfNum)
        CASE (ZoneSupplyAirTemp)
            ! check whether this zone is a controlled zone or not
            IF (.NOT.ControlledZoneAirFlag) THEN
                CALL ShowFatalError('Zones must be controlled for Ceiling-Diffuser Convection model. No system serves zone '//  &
                                    TRIM(Zone(ZoneNum)%Name))
                RETURN
            END IF
            ! determine supply air temperature as a weighted average of the inlet temperatures.
            DO NodeNum = 1, ZoneEquipConfig(ZoneEquipConfigNum)%NumInletNodes
               ! Get node conditions
               NodeTemp = Node(ZoneEquipConfig(ZoneEquipConfigNum)%InletNode(NodeNum))%Temp
               MassFlowRate = Node(ZoneEquipConfig(ZoneEquipConfigNum)%InletNode(NodeNum))%MassFlowRate
               CpAir = PsyCpAirFnWTdb(ZoneAirHumRat(ZoneNum), NodeTemp)

               SumSysMCp = SumSysMCp + MassFlowRate * CpAir
               SumSysMCpT = SumSysMCpT + MassFlowRate * CpAir * NodeTemp

            END DO ! NodeNum
            IF (SumSysMCp > 0.0d0) THEN
              RefAirTemp = SumSysMCpT/SumSysMCp
            ELSE
              ! no system flow (yet) so just use last value for inlet node temp, this can happen early in the environment
              RefAirTemp = NodeTemp
            ENDIF

        CASE DEFAULT
            ! currently set to mean air temp but should add error warning here
            RefAirTemp = MAT(ZoneNum)

    END SELECT

    IF (Surface(SurfNum)%Class == SurfaceClass_Window) THEN

      ! Add to the convective internal gains
      IF (SurfaceWindow(SurfNum)%ShadingFlag == IntShadeOn .OR. SurfaceWindow(SurfNum)%ShadingFlag == IntBlindOn) THEN
        ! The shade area covers the area of the glazing plus the area of the dividers.
        Area = Area + SurfaceWindow(SurfNum)%DividerArea
        ! If interior shade or blind is present it is assumed that both the convective and IR radiative gain
        ! from the inside surface of the divider goes directly into the zone air -- i.e., the IR radiative
        ! interaction between divider and shade or blind is ignored due to the difficulty of calculating this interaction
        ! at the same time that the interaction between glass and shade is calculated.
        SumIntGains = SumIntGains + SurfaceWindow(SurfNum)%DividerConduction
      END IF

      ! Other convection term is applicable to equivalent layer window (ASHWAT) model
      IF (Construct(Surface(SurfNum)%Construction)%WindowTypeEQL) &
        SumIntGains = SumIntGains + SurfaceWindow(SurfNum)%OtherConvHeatGain

      ! Convective heat gain from natural convection in gap between glass and interior shade or blind
      IF (SurfaceWindow(SurfNum)%ShadingFlag == IntShadeOn &
        .OR. SurfaceWindow(SurfNum)%ShadingFlag == IntBlindOn) &
        SumIntGains = SumIntGains + SurfaceWindow(SurfNum)%ConvHeatFlowNatural

      ! Convective heat gain from airflow window
      IF (SurfaceWindow(SurfNum)%AirFlowThisTS > 0.0d0) THEN
        SumIntGains = SumIntGains + SurfaceWindow(SurfNum)%ConvHeatGainToZoneAir
        IF (Zone(ZoneNum)%NoHeatToReturnAir) THEN
          SumIntGains = SumIntGains + SurfaceWindow(SurfNum)%RetHeatGainToZoneAir
        END IF
      ENDIF

      ! Add to the surface convection sums
      IF (SurfaceWindow(SurfNum)%FrameArea > 0.0d0) THEN
        ! Window frame contribution

        SumHADTsurfs = SumHADTsurfs + HConvIn(SurfNum) * SurfaceWindow(SurfNum)%FrameArea *   &
                                 (1.0d0 + SurfaceWindow(SurfNum)%ProjCorrFrIn) &
                                       * (SurfaceWindow(SurfNum)%FrameTempSurfIn - RefAirTemp)

      END IF

      IF (SurfaceWindow(SurfNum)%DividerArea > 0.0d0 .AND. SurfaceWindow(SurfNum)%ShadingFlag /= IntShadeOn &
           .AND. SurfaceWindow(SurfNum)%ShadingFlag /= IntBlindOn) THEN
        ! Window divider contribution (only from shade or blind for window with divider and interior shade or blind)
        SumHADTsurfs = SumHADTsurfs +  HConvIn(SurfNum) * SurfaceWindow(SurfNum)%DividerArea *   &
                                 (1.0d0 + 2.0d0 * SurfaceWindow(SurfNum)%ProjCorrDivIn) &
                                       * (SurfaceWindow(SurfNum)%DividerTempSurfIn - RefAirTemp)

      END IF

    END IF  ! End of check if window

    SumHADTsurfs = SumHADTsurfs + HConvIn(SurfNum) * Area * ( TempSurfInTmp(SurfNum) - RefAirTemp)

  END DO ! SurfNum

  ! now calculate air energy storage source term.
   ! capacitance is volume * density * heat capacity
  CpAir = PsyCpAirFnWTdb(ZoneAirHumRat(ZoneNum), MAT(ZoneNum))
  RhoAir = PsyRhoAirFnPbTdbW(OutBaroPress,MAT(ZoneNum),ZoneAirHumRat(ZoneNum))

  SELECT CASE (ZoneAirSolutionAlgo)
    CASE (Use3rdOrder)
      CzdTdt = RhoAir *  CpAir  * Zone(ZoneNum)%Volume*ZoneVolCapMultpSens                &
          * ( MAT(ZoneNum) - ZTM1(ZoneNum))   &
            / (TimeStepSys*SecInHour)
    ! Exact solution
    CASE (UseAnalyticalSolution)
      CzdTdt = TempIndCoef-TempDepCoef*MAT(ZoneNum)
    CASE (UseEulerMethod)
      CzdTdt = AIRRAT(ZoneNum)*(MAT(ZoneNum) - ZoneT1(ZoneNum))
  END SELECT

  IF (DisplayZoneAirHeatBalanceOffBalance) THEN
    imBalance      = SumIntGains + SumHADTsurfs + SumMCpDTzones + SumMCpDtInfil + SumMCpDTsystem + SumNonAirSystem - CzdTdt

    ! throw warning if seriously out of balance (this may need to be removed if too noisy... )
    ! formulate dynamic threshold value based on 20% of quadrature sum of components
    Threshold   =  0.2D0 * SQRT(SumIntGains**2 + SumHADTsurfs**2 + SumMCpDTzones**2 + SumMCpDtInfil**2 &
                                + SumMCpDTsystem**2 + SumNonAirSystem**2 + CzdTdt**2)
    IF ((ABS(ImBalance) > Threshold) .AND. (.NOT. WarmUpFlag) &
        .AND. (.NOT. DoingSizing) ) THEN ! air balance is out by more than threshold
      IF (Zone(ZoneNum)%AirHBimBalanceErrIndex == 0) THEN
        Call ShowWarningMessage('Zone Air Heat Balance is out of balance for zone named ' &
                             //trim(Zone(ZoneNum)%Name))
        Call ShowContinueError('Zone Air Heat Balance Deviation Rate is more than ' &
            // Trim(RoundSigDigits(Threshold,1)) //' {W}')
        If (TurnFansOn) then
          Call ShowContinueError('Night cycle fan operation may be causing above error')
        ENDIF

        Call ShowContinueErrorTimeStamp(' Occurance info:  ')
      ENDIF
      Call ShowRecurringWarningErrorAtEnd('Zone Air Heat Balance is out of balance ... zone named ' // &
          trim(Zone(ZoneNum)%Name) , Zone(ZoneNum)%AirHBimBalanceErrIndex, &
          ReportMinOf=ABS(ImBalance)-Threshold,ReportMaxOf=ABS(ImBalance)-Threshold,ReportMinUnits='{W}',ReportMaxUnits='{W}')
    ENDIF
  ENDIF

  RETURN

END SUBROUTINE CalcZoneComponentLoadSums

FUNCTION VerifyThermostatInZone(ZoneName) RESULT (HasThermostat)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   Feb 2005
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function verifies that a zone (by name) has a Zone Control:Thermostatic
          ! object entered.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: FindItemInList

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: ZoneName  ! Zone to verify
  LOGICAL :: HasThermostat   ! True if does, false if not.

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
          ! na

  IF (GetZoneAirStatsInputFlag) THEN
    CALL GetZoneAirSetpoints
    GetZoneAirStatsInputFlag = .FALSE.
  END IF
  IF (NumTempControlledZones > 0) THEN
    IF (FindItemInList(ZoneName,TempControlledZone%ZoneName,NumTempControlledZones) > 0) THEN
      HasThermostat=.true.
    ELSE
      HasThermostat=.false.
    ENDIF
  ELSE
    HasThermostat=.false.
  ENDIF
  RETURN

END FUNCTION VerifyThermostatInZone

FUNCTION VerifyControlledZoneForThermostat(ZoneName) RESULT (IsControlled)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   Mar 2007
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function verifies that a zone (by name) has a ZoneHVAC:EquipmentConnections
          ! object entered.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: FindItemInList
  USE DataZoneEquipment, ONLY: ZoneEquipConfig

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: ZoneName  ! Zone to verify
  LOGICAL :: IsControlled   ! True if does, false if not.

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
          ! na

  IF (FindItemInList(ZoneName,ZoneEquipConfig%ZoneName,NumOfZones) > 0) THEN
    IsControlled=.true.
  ELSE
    IsControlled=.false.
  ENDIF
  RETURN

END FUNCTION VerifyControlledZoneForThermostat

SUBROUTINE DetectOscillatingZoneTemp
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Jason Glazer
          !       DATE WRITTEN   August 2005
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Oscillating temperatures between HVAC timesteps indicate that the
          ! simulation may be poor. Code is trying to be fast since the purpose
          ! is to see the impact on oscillating by trying longer time steps in
          ! an attempt to speed up the simulation.
          !
          ! Note that the OscillateMagnitude threshold must be less than
          ! MaxZoneTempDiff since ManageHVAC keeps shortening the timestep
          ! until that is reached unless it goes to less than the
          ! MinTimeStepSys.

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
INTEGER :: iZone
REAL(r64)    :: NegOscillateMagnitude
LOGICAL :: isOscillate
REAL(r64)    :: Diff12
REAL(r64)    :: Diff23
REAL(r64)    :: Diff34
LOGICAL,SAVE :: SetupOscillationOutputFlag = .TRUE.
LOGICAL :: isAnyZoneOscillating

!first time run allocate arrays and setup output variable
IF (SetupOscillationOutputFlag) THEN
  ALLOCATE (ZoneTempHist(NumOfZones,4))
  ZoneTempHist = 0.0d0
  ALLOCATE (ZoneTempOscillate(NumOfZones))
  ZoneTempOscillate = 0.0d0
  !set up zone by zone variables
  ! CurrentModuleObject='Zone'
  DO iZone = 1, NumOfZones
    CALL SetupOutputVariable('Zone Oscillating Temperatures Time [hr]',ZoneTempOscillate(iZone), &
                              'System','Sum',Zone(iZone)%Name)
  END DO
  !set up a variable covering all zones
  CALL SetupOutputVariable('Facility Any Zone Oscillating Temperatures Time [hr]',AnyZoneTempOscillate, &
                              'System','Sum','Facility')
  SetupOscillationOutputFlag = .FALSE.
END IF
!precalc the negative value for performance
NegOscillateMagnitude = -OscillateMagnitude
!assume no zone is oscillating
isAnyZoneOscillating = .FALSE.
DO iZone = 1, NumOfZones
  isOscillate = .FALSE.
  ZoneTempHist(iZone,4) = ZoneTempHist(iZone,3)
  ZoneTempHist(iZone,3) = ZoneTempHist(iZone,2)
  ZoneTempHist(iZone,2) = ZoneTempHist(iZone,1)
  ZoneTempHist(iZone,1) = ZT(iZone)
  Diff34 = ZoneTempHist(iZone,3) - ZoneTempHist(iZone,4)
  Diff23 = ZoneTempHist(iZone,2) - ZoneTempHist(iZone,3)
  Diff12 = ZoneTempHist(iZone,1) - ZoneTempHist(iZone,2)
  ! roll out the conditionals for increased performance
  IF (Diff12 .GT. OscillateMagnitude) THEN
    IF (Diff23 .LT. NegOscillateMagnitude) THEN
      IF (Diff34 .GT. OscillateMagnitude) THEN
        isOscillate = .TRUE.
      END IF
    END IF
  END IF
  ! now try the opposite sequence of swings
  IF (Diff12 .LT. NegOscillateMagnitude) THEN
    IF (Diff23 .GT. OscillateMagnitude) THEN
      IF (Diff34 .LT. NegOscillateMagnitude) THEN
        isOscillate = .TRUE.
      END IF
    END IF
  END IF
  IF (isOscillate) THEN
    ZoneTempOscillate(iZone) = TimeStepSys
    isAnyZoneOscillating = .TRUE.
  ELSE
    ZoneTempOscillate(iZone) = 0.0d0
  END IF
END DO
!any zone variable
IF (isAnyZoneOscillating) THEN
  AnyZoneTempOscillate = TimeStepSys
ELSE
  AnyZoneTempOscillate = 0.0d0
END IF
END SUBROUTINE DetectOscillatingZoneTemp

SUBROUTINE AdjustAirSetpointsforOpTempCntrl(TempControlledZoneID, ActualZoneNum, ZoneAirSetpoint)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B. Griffith
          !       DATE WRITTEN   June 2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine modifies the air temperature setpoint to effect operative temperature control

          ! METHODOLOGY EMPLOYED:
          ! pass in data and alter setpoint if needed

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataHeatBalance,   ONLY: MRT
  USE ScheduleManager, ONLY: GetCurrentScheduleValue

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: TempControlledZoneID
  INTEGER, INTENT(IN) :: ActualZoneNum
  REAL(r64),    INTENT(INOUT) :: ZoneAirSetpoint

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64) :: thisMRT  ! local variable for mean radiant temperature in this zone
  REAL(r64) :: thisMRTFraction ! local variable for fraction that MRT is in Op Temp definition

  IF (.NOT.(AnyOpTempControl)) RETURN  ! do nothing to setpoint

  IF (.NOT. (TempControlledZone(TempControlledZoneID)%OperativeTempControl)) RETURN ! do nothing to setpoint

  ! is operative temp radiative fraction scheduled or fixed?
  If (TempControlledZone(TempControlledZoneID)%OpTempCntrlModeScheduled) then
    thisMRTFraction = GetCurrentScheduleValue(TempControlledZone(TempControlledZoneID)%OpTempRadiativeFractionSched)
  ELSE
    thisMRTFraction = TempControlledZone(TempControlledZoneID)%FixedRadiativeFraction
  ENDIF

  ! get mean radiant temperature for zone
  thisMRT = MRT(ActualZoneNum)

  ! modify setpoint for operative temperature control
  !  traping for MRT fractions between 0.0 and 0.9 during get input, so shouldn't be able to divide by zero here.
  ZoneAirSetpoint = (ZoneAirSetpoint - thisMRTFraction * thisMRT) / (1.d0 - thisMRTFraction)

  RETURN

END SUBROUTINE AdjustAirSetpointsforOpTempCntrl


SUBROUTINE CalcZoneAirComfortSetpoints

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Lixing Gu
          !       DATE WRITTEN   May 2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This routine sets the thermal comfort setpoints for each controlled zone based on air tempeature
          ! obtained from thermal comfort models.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE ScheduleManager, ONLY: GetCurrentScheduleValue
  USE General, ONLY: TrimSigDigits
  USE ThermalComfort, ONLY: ManageThermalComfort
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
  INTEGER :: RelativeZoneNum
  INTEGER :: ActualZoneNum
  INTEGER :: ComfortControlSchedIndex
  INTEGER :: SetPointComfortSchedIndex
  INTEGER :: SetPointComfortSchedIndexHot
  INTEGER :: SetPointComfortSchedIndexCold
  INTEGER :: SchedNameIndex
  INTEGER :: SchedTypeIndex
  INTEGER :: PeopleNum
  INTEGER :: ObjectCount
  REAL(r64)    :: PeopleCount
  REAL(r64)    :: SetPointLo
  REAL(r64)    :: SetPointHi
  REAL(r64)    :: NumberOccupants
  REAL(r64)    :: Tset

  LOGICAL,SAVE :: FirstTimeFlag = .TRUE. ! Flag set to make sure you get input once

          ! FLOW:
   ! Call thermal comfort module to read zone control comfort object
   IF (FirstTimeFlag) THEN
     CALL ManageThermalComfort(InitializeOnly=.true.)
     FirstTimeFlag = .FALSE.
   END IF

   ComfortControlType = 0 ! Default

   DO RelativeZoneNum = 1, NumComfortControlledZones

    ActualZoneNum = ComfortControlledZone(RelativeZoneNum)%ActualZoneNum
    ComfortControlSchedIndex = ComfortControlledZone(RelativeZoneNum)%ComfortSchedIndex
    ComfortControlType(ActualZoneNum) = GetCurrentScheduleValue(ComfortControlSchedIndex)

    ! Get PMV values

    SELECT CASE (ComfortControlType(ActualZoneNum)) ! Is this missing the possibility of sometimes having no control on a zone
                                                 ! during the simulation?
      CASE (0) ! Uncontrolled for thermal comfort
        ZoneComfortControlsFanger(ActualZoneNum)%LowPMV = -999.0d0
        ZoneComfortControlsFanger(ActualZoneNum)%HighPMV = -999.0d0

      CASE (SglHeatSetPointFanger)

        SchedNameIndex = ComfortControlledZone(RelativeZoneNum)%SchIndx_SglHeatSetPointFanger
        SchedTypeIndex = ComfortControlledZone(RelativeZoneNum)%ControlTypeSchIndx(SchedNameIndex)
        SetPointComfortSchedIndex = SetPointSingleHeatingFanger(SchedTypeIndex)%PMVSchedIndex
        ZoneComfortControlsFanger(ActualZoneNum)%FangerType = SglHeatSetPointFanger
        ZoneComfortControlsFanger(ActualZoneNum)%LowPMV = GetCurrentScheduleValue(SetPointComfortSchedIndex)
        ZoneComfortControlsFanger(ActualZoneNum)%HighPMV = -999.0d0

      CASE (SglCoolSetPointFanger)

        SchedNameIndex = ComfortControlledZone(RelativeZoneNum)%SchIndx_SglCoolSetPointFanger
        SchedTypeIndex = ComfortControlledZone(RelativeZoneNum)%ControlTypeSchIndx(SchedNameIndex)
        SetPointComfortSchedIndex = SetPointSingleCoolingFanger(SchedTypeIndex)%PMVSchedIndex
        ZoneComfortControlsFanger(ActualZoneNum)%FangerType = SglCoolSetPointFanger
        ZoneComfortControlsFanger(ActualZoneNum)%LowPMV = -999.0d0
        ZoneComfortControlsFanger(ActualZoneNum)%HighPMV = GetCurrentScheduleValue(SetPointComfortSchedIndex)

      CASE (SglHCSetPointFanger)

        SchedNameIndex = ComfortControlledZone(RelativeZoneNum)%SchIndx_SglHCSetPointFanger
        SchedTypeIndex = ComfortControlledZone(RelativeZoneNum)%ControlTypeSchIndx(SchedNameIndex)
        SetPointComfortSchedIndex = SetPointSingleHeatCoolFanger(SchedTypeIndex)%PMVSchedIndex
        ZoneComfortControlsFanger(ActualZoneNum)%FangerType = SglHCSetPointFanger
        ZoneComfortControlsFanger(ActualZoneNum)%LowPMV = GetCurrentScheduleValue(SetPointComfortSchedIndex)
        ZoneComfortControlsFanger(ActualZoneNum)%HighPMV = GetCurrentScheduleValue(SetPointComfortSchedIndex)

      CASE (DualSetPointFanger)

        SchedNameIndex = ComfortControlledZone(RelativeZoneNum)%SchIndx_DualSetPointFanger
        SchedTypeIndex = ComfortControlledZone(RelativeZoneNum)%ControlTypeSchIndx(SchedNameIndex)
        SetPointComfortSchedIndexHot = SetPointDualHeatCoolFanger(SchedTypeIndex)%HeatPMVSchedIndex
        SetPointComfortSchedIndexCold = SetPointDualHeatCoolFanger(SchedTypeIndex)%CoolPMVSchedIndex
        ZoneComfortControlsFanger(ActualZoneNum)%FangerType = DualSetPointFanger
        ZoneComfortControlsFanger(ActualZoneNum)%LowPMV = GetCurrentScheduleValue(SetPointComfortSchedIndexHot)
        ZoneComfortControlsFanger(ActualZoneNum)%HighPMV = GetCurrentScheduleValue(SetPointComfortSchedIndexCold)
        If (ZoneComfortControlsFanger(ActualZoneNum)%LowPMV > ZoneComfortControlsFanger(ActualZoneNum)%HighPMV) then
          ZoneComfortControlsFanger(ActualZoneNum)%DualPMVErrCount = ZoneComfortControlsFanger(ActualZoneNum)%DualPMVErrCount + 1
          if (ZoneComfortControlsFanger(ActualZoneNum)%DualPMVErrCount < 2) then
            CALL ShowWarningError('ThermostatSetpoint:ThermalComfort:Fanger:DualSetpoint: The heating PMV setpoint is above '// &
                'the cooling PMV setpoint in '//TRIM(SetPointDualHeatCoolFanger(SchedTypeIndex)%Name))
            CALL ShowContinueError('The zone dual heating PMV setpoint is set to the dual cooling PMV setpoint.')
            CALL ShowContinueErrorTimeStamp(' Occurrence info: ')
          else
            CALL ShowRecurringWarningErrorAtEnd('The heating PMV setpoint is still above '// &
                'the cooling PMV setpoint',ZoneComfortControlsFanger(ActualZoneNum)%DualPMVErrIndex, &
                 ZoneComfortControlsFanger(ActualZoneNum)%LowPMV, ZoneComfortControlsFanger(ActualZoneNum)%LowPMV)
          end if
          ZoneComfortControlsFanger(ActualZoneNum)%LowPMV = ZoneComfortControlsFanger(ActualZoneNum)%HighPMV
        End If

      CASE DEFAULT
        CALL ShowSevereError('CalcZoneAirTempSetpoints: Illegal thermal control control type for Zone='//  &
                               TRIM(Zone(ActualZoneNum)%Name)//  &
                               ', Found value='//TRIM(TrimSigDigits(ComfortControlType(ActualZoneNum)))//  &
                               ', in Schedule='//TRIM(COmfortControlledZone(RelativeZoneNum)%ControlTypeSchedName))

    END SELECT

    ! Check Average method
    SELECT CASE (ComfortControlledZone(RelativeZoneNum)%AverageMethodNum)
      CASE (AverageMethodNum_NO)
        PeopleNum = ComfortControlledZone(RelativeZoneNum)%SpecificObjectNum
        If (ComfortControlType(ActualZoneNum) == SglCoolSetPointFanger) then
          CALL GetComfortSetpoints(PeopleNum, RelativeZoneNum, ZoneComfortControlsFanger(ActualZoneNum)%HighPMV,SetPointLo)
        Else
          CALL GetComfortSetpoints(PeopleNum, RelativeZoneNum, ZoneComfortControlsFanger(ActualZoneNum)%LowPMV,SetPointLo)
        End If
        If (ComfortControlType(ActualZoneNum) == DualSetPointFanger) &
        CALL GetComfortSetpoints(PeopleNum, RelativeZoneNum, ZoneComfortControlsFanger(ActualZoneNum)%HighPMV,SetPointHi)
      CASE (AverageMethodNum_SPE)
        PeopleNum = ComfortControlledZone(RelativeZoneNum)%SpecificObjectNum
        If (ComfortControlType(ActualZoneNum) == SglCoolSetPointFanger) then
          CALL GetComfortSetpoints(PeopleNum, RelativeZoneNum, ZoneComfortControlsFanger(ActualZoneNum)%HighPMV,SetPointLo)
        Else
          CALL GetComfortSetpoints(PeopleNum, RelativeZoneNum, ZoneComfortControlsFanger(ActualZoneNum)%LowPMV,SetPointLo)
        End If
        If (ComfortControlType(ActualZoneNum) == DualSetPointFanger) &
        CALL GetComfortSetpoints(PeopleNum, RelativeZoneNum, ZoneComfortControlsFanger(ActualZoneNum)%HighPMV,SetPointHi)
      CASE (AverageMethodNum_OBJ)
        ObjectCount = 0
        SetPointLo = 0.0d0
        SetPointHi = 0.0d0
        Do PeopleNum=1,TotPeople
          If (ActualZoneNum == People(PeopleNum)%ZonePtr) then
            ObjectCount=ObjectCount+1
            CALL GetComfortSetpoints(PeopleNum, RelativeZoneNum, ZoneComfortControlsFanger(ActualZoneNum)%LowPMV,Tset)
            SetPointLo = SetPointLo + Tset
            If (ComfortControlType(ActualZoneNum) == DualSetPointFanger) then
              CALL GetComfortSetpoints(PeopleNum, RelativeZoneNum, ZoneComfortControlsFanger(ActualZoneNum)%HighPMV,Tset)
              SetPointHi = SetPointHi + Tset
            End If
          End IF
        End Do
        SetPointLo = SetPointLo/ObjectCount
        If (ComfortControlType(ActualZoneNum) == DualSetPointFanger) SetPointHi = SetPointHi/ObjectCount
      CASE (AverageMethodNum_PEO)
        PeopleCount = 0.0d0
        SetPointLo = 0.0d0
        SetPointHi = 0.0d0
        Do PeopleNum=1,TotPeople
          If (ActualZoneNum == People(PeopleNum)%ZonePtr) then
            NumberOccupants = People(PeopleNum)%NumberOfPeople * GetCurrentScheduleValue(People(PeopleNum)%NumberOfPeoplePtr)
            PeopleCount=PeopleCount+NumberOccupants
            CALL GetComfortSetpoints(PeopleNum, RelativeZoneNum, ZoneComfortControlsFanger(ActualZoneNum)%LowPMV,Tset)
            SetPointLo = SetPointLo + Tset*NumberOccupants
            If (ComfortControlType(ActualZoneNum) == DualSetPointFanger) then
              CALL GetComfortSetpoints(PeopleNum, RelativeZoneNum, ZoneComfortControlsFanger(ActualZoneNum)%HighPMV,Tset)
              SetPointHi = SetPointHi+ Tset*NumberOccupants
            End If
          End If
        End Do
        If (PeopleCount > 0) then
          SetPointLo = SetPointLo/PeopleCount
          If (ComfortControlType(ActualZoneNum) == DualSetPointFanger) SetPointHi = SetPointHi/PeopleCount
        Else
          ! reccurring warnings
!          ComfortControlledZone(RelativeZoneNum)%PeopleAverageErrCount = &
!                                           ComfortControlledZone(RelativeZoneNum)%PeopleAverageErrCount + 1
          if (ComfortControlledZone(RelativeZoneNum)%PeopleAverageErrIndex == 0) then
            Call ShowWarningMessage('ZoneControl:Thermostat:ThermalComfort: The total number of people in Zone = '// &
                 Trim(Zone(ActualZoneNum)%Name)//' is zero. The People Average option is not used.')
            Call ShowContinueError('The Object Average option is used instead. Simulation continues .....')
            CALL ShowContinueErrorTimeStamp(' Occurrence info: ')
          end if
          Call ShowRecurringWarningErrorAtEnd('ZoneControl:Thermostat:ThermalComfort: The total number of people in Zone = '// &
                 Trim(Zone(ActualZoneNum)%Name)//' is still zero. The People Average option is not used', &
                 ComfortControlledZone(RelativeZoneNum)%PeopleAverageErrIndex, PeopleCount, PeopleCount)
          ObjectCount = 0
          SetPointLo = 0.0d0
          SetPointHi = 0.0d0
          Do PeopleNum=1,TotPeople
            If (ActualZoneNum == People(PeopleNum)%ZonePtr) then
              ObjectCount=ObjectCount+1
              CALL GetComfortSetpoints(PeopleNum, RelativeZoneNum, ZoneComfortControlsFanger(ActualZoneNum)%LowPMV,Tset)
              SetPointLo = SetPointLo + Tset
              If (ComfortControlType(ActualZoneNum) == DualSetPointFanger) then
                CALL GetComfortSetpoints(PeopleNum, RelativeZoneNum, ZoneComfortControlsFanger(ActualZoneNum)%HighPMV,Tset)
                SetPointHi = SetPointHi + Tset
              End If
            End IF
          End Do
          SetPointLo = SetPointLo/ObjectCount
          If (ComfortControlType(ActualZoneNum) == DualSetPointFanger) SetPointHi = SetPointHi/ObjectCount
        End If
    END SELECT

    ! Assign setpoint
    SELECT CASE (ComfortControlType(ActualZoneNum)) ! Is this missing the possibility of sometimes having no control on a zone
                                                 ! during the simulation?
      CASE (0) ! Uncontrolled for thermal comfort
        SELECT CASE (TempControlType(ActualZoneNum))
          CASE (SingleHeatingSetPoint)
            ZoneThermostatSetPointHi(ActualZoneNum) = 0.0d0
          CASE (SingleCoolingSetPoint)
            ZoneThermostatSetPointLo(ActualZoneNum) = 0.0d0
        END SELECT

      CASE (SglHeatSetPointFanger)
        If (SetPointLo < ComfortControlledZone(RelativeZoneNum)%TdbMinSetPoint) then
          SetPointLo = ComfortControlledZone(RelativeZoneNum)%TdbMinSetPoint
!          ComfortControlledZone(RelativeZoneNum)%TdbMinErrCount = ComfortControlledZone(RelativeZoneNum)%TdbMinErrCount + 1
          if (ComfortControlledZone(RelativeZoneNum)%TdbMinErrIndex < 2) then
            CALL ShowWarningMessage('ThermostatSetpoint:ThermalComfort:Fanger:SingleHeating temperature is below '// &
                'the Minimum dry-bulb temperature setpoint '//TRIM(ComfortControlledZone(RelativeZoneNum)%Name))
            CALL ShowContinueError('The zone heating setpoint is set to the Minimum dry-bulb temperature setpoint')
            CALL ShowContinueErrorTimeStamp(' Occurrence info: ')
          end if
          CALL ShowRecurringWarningErrorAtEnd('ThermostatSetpoint:ThermalComfort:Fanger:SingleHeating temperature is still'// &
                ' below the Minimum dry-bulb temperature setpoint ...', &
                ComfortControlledZone(RelativeZoneNum)%TdbMinErrIndex, SetPointLo, SetPointLo)
        End If
        TempZoneThermostatSetPoint(ActualZoneNum) = SetPointLo
        ZoneThermostatSetPointLo(ActualZoneNum) = TempZoneThermostatSetPoint(ActualZoneNum)
        TempControlType(ActualZoneNum) = SingleHeatingSetPoint

      CASE (SglCoolSetPointFanger)

        If (SetPointLo > ComfortControlledZone(RelativeZoneNum)%TdbMaxSetPoint) then
          SetPointLo = ComfortControlledZone(RelativeZoneNum)%TdbMaxSetPoint
!          ComfortControlledZone(RelativeZoneNum)%TdbMaxErrCount = ComfortControlledZone(RelativeZoneNum)%TdbMaxErrCount + 1
          if (ComfortControlledZone(RelativeZoneNum)%TdbMaxErrIndex == 0) then
            CALL ShowWarningMessage('ThermostatSetpoint:ThermalComfort:Fanger:SingleCooling temperature is above  '// &
                'the Maximum dry-bulb temperature setpoint '//TRIM(ComfortControlledZone(RelativeZoneNum)%Name))
            CALL ShowContinueError('The zone cooling setpoint is set to the Maximum dry-bulb temperature setpoint')
            CALL ShowContinueErrorTimeStamp(' Occurrence info: ')
          end if
          CALL ShowRecurringWarningErrorAtEnd('ThermostatSetpoint:ThermalComfort:Fanger:SingleCooling temperature is still'// &
                ' above the Maximum dry-bulb temperature setpoint ...', &
                ComfortControlledZone(RelativeZoneNum)%TdbMaxErrIndex, SetPointLo, SetPointLo)
        End If
        TempZoneThermostatSetPoint(ActualZoneNum) = SetPointLo
        ZoneThermostatSetPointHi(ActualZoneNum) = TempZoneThermostatSetPoint(ActualZoneNum)
        TempControlType(ActualZoneNum) = SingleCoolingSetPoint

      CASE (SglHCSetPointFanger)

        If (ComfortControlledZone(RelativeZoneNum)%TdbMaxSetPoint == ComfortControlledZone(RelativeZoneNum)%TdbMinSetPoint) then
          SetPointLo = ComfortControlledZone(RelativeZoneNum)%TdbMaxSetPoint
        End If
        If (SetPointLo > ComfortControlledZone(RelativeZoneNum)%TdbMaxSetPoint) &
          SetPointLo = ComfortControlledZone(RelativeZoneNum)%TdbMaxSetPoint
        If (SetPointLo < ComfortControlledZone(RelativeZoneNum)%TdbMinSetPoint) &
          SetPointLo = ComfortControlledZone(RelativeZoneNum)%TdbMinSetPoint
        If (SetPointLo < ComfortControlledZone(RelativeZoneNum)%TdbMinSetPoint .or. &
            SetPointLo > ComfortControlledZone(RelativeZoneNum)%TdbMaxSetPoint) then
!          ComfortControlledZone(RelativeZoneNum)%TdbHCErrCount = ComfortControlledZone(RelativeZoneNum)%TdbHCErrCount + 1
          if (ComfortControlledZone(RelativeZoneNum)%TdbHCErrIndex == 0) then
            CALL ShowWarningMessage('ThermostatSetpoint:ThermalComfort:Fanger:SingleHeatingOrCooling temperature is above  '// &
                'the Maximum or below the Minimum dry-bulb temperature setpoint '//  &
                TRIM(ComfortControlledZone(RelativeZoneNum)%Name))
            CALL ShowContinueError('The zone setpoint is set to the Maximum dry-bulb temperature setpoint if above or ' &
                 //'the Minimum dry-bulb temperature setpoint if below')
            CALL ShowContinueErrorTimeStamp(' Occurrence info: ')
          end if
          CALL ShowRecurringWarningErrorAtEnd('ThermostatSetpoint:ThermalComfort:Fanger:SingleHeatingOrCooling '//  &
               'temperature is still beyond the range between Maximum and Minimum dry-bulb temperature setpoint ...', &
                ComfortControlledZone(RelativeZoneNum)%TdbHCErrIndex, SetPointLo, SetPointLo)
        End If
        TempZoneThermostatSetPoint(ActualZoneNum) = SetPointLo
        ZoneThermostatSetPointHi(ActualZoneNum) = TempZoneThermostatSetPoint(ActualZoneNum)
        ZoneThermostatSetPointLo(ActualZoneNum) = TempZoneThermostatSetPoint(ActualZoneNum)
        TempControlType(ActualZoneNum) = SingleHeatCoolSetPoint

      CASE (DualSetPointFanger)

        If (SetPointLo < ComfortControlledZone(RelativeZoneNum)%TdbMinSetPoint) then
          SetPointLo = ComfortControlledZone(RelativeZoneNum)%TdbMinSetPoint
!          ComfortControlledZone(RelativeZoneNum)%TdbDualMinErrCount = ComfortControlledZone(RelativeZoneNum)%TdbDualMinErrCount+1
          if (ComfortControlledZone(RelativeZoneNum)%TdbDualMinErrIndex == 0) then
            CALL ShowWarningMessage('ThermostatSetpoint:ThermalComfort:Fanger:DualSetpoint temperature is below '// &
                'the Minimum dry-bulb temperature setpoint '//TRIM(ComfortControlledZone(RelativeZoneNum)%Name))
            CALL ShowContinueError('The zone dual heating setpoint is set to the Minimum dry-bulb temperature setpoint')
            CALL ShowContinueErrorTimeStamp(' Occurrence info: ')
          end if
          CALL ShowRecurringWarningErrorAtEnd('ThermostatSetpoint:ThermalComfort:Fanger:DualSetpoint temperature is still'// &
                ' below the Minimum dry-bulb temperature setpoint ...', &
                ComfortControlledZone(RelativeZoneNum)%TdbDualMinErrIndex, SetPointLo, SetPointLo)
        End If
        If (SetPointHi > ComfortControlledZone(RelativeZoneNum)%TdbMaxSetPoint) then
          SetPointHi = ComfortControlledZone(RelativeZoneNum)%TdbMaxSetPoint
!          ComfortControlledZone(RelativeZoneNum)%TdbDualMaxErrCount = ComfortControlledZone(RelativeZoneNum)%TdbDualMaxErrCount + 1
          if (ComfortControlledZone(RelativeZoneNum)%TdbDualMaxErrIndex == 0) then
            CALL ShowWarningMessage('ThermostatSetpoint:ThermalComfort:Fanger:DualSetpoint temperature is above  '// &
                'the Maximum dry-bulb temperature setpoint '//TRIM(ComfortControlledZone(RelativeZoneNum)%Name))
            CALL ShowContinueError('The zone dual cooling setpoint is set to the Maximum dry-bulb temperature setpoint')
            CALL ShowContinueErrorTimeStamp(' Occurrence info: ')
          end if
          CALL ShowRecurringWarningErrorAtEnd('ThermostatSetpoint:ThermalComfort:Fanger:DualSetpoint temperature is still'// &
                ' above the Maximum dry-bulb temperature setpoint ...', &
                ComfortControlledZone(RelativeZoneNum)%TdbDualMaxErrIndex, SetPointLo, SetPointLo)
        End If

        ZoneThermostatSetPointLo(ActualZoneNum) = SetPointLo
        ZoneThermostatSetPointHi(ActualZoneNum) = SetPointHi
        TempControlType(ActualZoneNum) = DualSetPointWithDeadBand

      CASE DEFAULT
        CALL ShowSevereError('CalcZoneAirComfortSetpoints: Illegal thermal control control type for Zone='//  &
                               TRIM(Zone(ActualZoneNum)%Name)//  &
                               ', Found value='//TRIM(TrimSigDigits(ComfortControlType(ActualZoneNum)))//  &
                               ', in Schedule='//TRIM(ComfortControlledZone(ActualZoneNum)%ControlTypeSchedName))

    END SELECT

  END DO

  RETURN

END SUBROUTINE CalcZoneAirComfortSetpoints

SUBROUTINE GetComfortSetpoints(PeopleNum, ComfortControlNum, PMVSet, Tset)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Lixing Gu
          !       DATE WRITTEN   May, 2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na
          ! PURPOSE OF THIS SUBROUTINE:

          ! This routine sets what the thermal comfort setpoints for each controlled zone should be based on air tempeature
          ! obtained from thermal comfort models.
          ! This is called each time step.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE General, ONLY: SolveRegulaFalsi
  USE ThermalComfort, ONLY: CalcThermalComfortFanger
  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
       INTEGER, INTENT(IN)    :: PeopleNum                    !
       INTEGER, INTENT(IN)    :: ComfortControlNum                    !
       REAL(r64), INTENT(IN)       :: PMVSet                      !
       REAL(r64), INTENT(OUT)      :: TSet              ! drybulb setpoint temperature for a given PMV value
                                                   ! 0 = Solution; 1 = Set to Min; 2 Set to Max

          ! SUBROUTINE PARAMETER DEFINITIONS:
          REAL(r64),PARAMETER :: Acc = 0.001D0    ! accuracy control for SolveRegulaFalsi
          INTEGER,PARAMETER          :: MaxIter = 500  !iteration control for SolveRegulaFalsi

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          REAL(r64) :: Tmin ! Minimun drybulb setpoint temperature
          REAL(r64) :: Tmax ! Maximun drybulb setpoint temperature
          REAL(r64) :: PMVResult ! Calculated PMV value
          REAL(r64) :: PMVMin ! Minimum allowed PMV value
          REAL(r64) :: PMVMax ! Calculated PMV value
          REAL(r64) :: Par(2) ! Passed parameter for RegularFalsi function
          INTEGER  :: SolFla !feed back flag from SolveRegulaFalsi
          INTEGER, SAVE :: IterLimitExceededNum1 = 0
          INTEGER, SAVE :: IterLimitErrIndex1 = 0
          INTEGER, SAVE :: IterLimitExceededNum2 = 0
          INTEGER, SAVE :: IterLimitErrIndex2 = 0

    Tmin = ComfortControlledZone(ComfortControlNum)%TdbMinSetPoint
    Tmax = ComfortControlledZone(ComfortControlNum)%TdbMaxSetPoint

    CALL CalcThermalComfortFanger(PeopleNum,Tmin,PMVResult)
    PMVmin = PMVResult
    CALL CalcThermalComfortFanger(PeopleNum,Tmax,PMVResult)
    PMVmax = PMVResult
    If (PMVset > PMVmin .AND. PMVset < PMVmax) then
      Par(1)=PMVset
      Par(2)=REAL(PeopleNum,r64)
      CALL SolveRegulaFalsi(Acc, MaxIter, SolFla, Tset, PMVResidual, Tmin, Tmax, Par)
      IF (SolFla == -1) THEN
        IF(.NOT. WarmupFlag)THEN
          IterLimitExceededNum1 = IterLimitExceededNum1 + 1
          IF (IterLimitExceededNum1 .EQ. 1) THEN
            CALL ShowWarningError(TRIM(ComfortControlledZone(ComfortControlNum)%Name) //  &
            ': Iteration limit exceeded calculating thermal comfort Fanger setpoint and non-converged setpoint is used')
          ELSE
            CALL ShowRecurringWarningErrorAtEnd(TRIM(ComfortControlledZone(ComfortControlNum)%Name)// &
             ':  Iteration limit exceeded calculating thermal comfort setpoint.', &
             IterLimitErrIndex1, Tset, Tset)
          END IF
        END IF
      ELSE IF (SolFla == -2) THEN
        IF(.NOT. WarmupFlag)THEN
          IterLimitExceededNum2 = IterLimitExceededNum2 + 1
          IF (IterLimitExceededNum2 .EQ. 1) THEN
            CALL ShowWarningError(TRIM(ComfortControlledZone(ComfortControlNum)%Name) //  &
            ': Solution is not found in calculating thermal comfort Fanger setpoint and the minimum setpoint is used')
          ELSE
            CALL ShowRecurringWarningErrorAtEnd(TRIM(ComfortControlledZone(ComfortControlNum)%Name)// &
             ':  Solution is not found in  calculating thermal comfort Fanger setpoint.', &
             IterLimitErrIndex2, Tset, Tset)
          END IF
        END IF
      END IF
    else if (PMVset < PMVmin) then
      Tset = Tmin
    else if (PMVset > PMVmax) then
      Tset = Tmax
    End If

  RETURN

END SUBROUTINE GetComfortSetpoints

REAL(r64) FUNCTION PMVResidual(Tset, Par)
          ! FUNCTION INFORMATION:
          !       AUTHOR         Richard Raustad
          !       DATE WRITTEN   May 2006
          !       MODIFIED       L.Gu, May 2006
          !       RE-ENGINEERED

          ! PURPOSE OF THIS FUNCTION:
          !  Calculates residual function (desired PMV value - actual PMV value) for thermal comfort control.

          ! METHODOLOGY EMPLOYED:
          !  Calls CalcThermalComfortFanger to get PMV value at the given zone and people conditions
          !  and calculates the residual as defined above

          ! REFERENCES:

          ! USE STATEMENTS:
  USE ThermalComfort, ONLY: CalcThermalComfortFanger

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  REAL(r64), INTENT(IN)     :: Tset                    !
  REAL(r64), INTENT(IN), DIMENSION(:), OPTIONAL :: Par ! par(1) = PMV set point

          ! FUNCTION PARAMETER DEFINITIONS:
          !  na

          ! INTERFACE BLOCK SPECIFICATIONS
          !  na

          ! DERIVED TYPE DEFINITIONS
          !  na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER :: PeopleNum          ! index of people object
  REAL(r64)    :: PMVresult          ! resulting PMV values

  PeopleNum = INT(Par(2))
  CALL CalcThermalComfortFanger(PeopleNum, Tset, PMVresult)
  PMVResidual = Par(1) - PMVresult
  RETURN

END FUNCTION PMVResidual

SUBROUTINE AdjustCoolingSetPointforTempAndHumidityControl(TempControlledZoneID, ActualZoneNum)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Bereket A Nigusse, FSEC/UCF
          !       DATE WRITTEN   Nov 2010
          !       MODIFIED
          !       RE-ENGINEERED

          ! PURPOSE OF THIS SUBROUTINE:
          !  This subroutine modifies the air cooling setpoint temperature to effect zone air Temperature
          !  and humidity control
          !
          ! METHODOLOGY EMPLOYED:
          !  Alter the zone air cooling setpoint if the zone air relative humidity value exceeds the
          !  the zone dehumidifying relative humidity setpoint.

          ! REFERENCES:

          ! USE STATEMENTS:
  USE ScheduleManager,   ONLY: GetCurrentScheduleValue

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)     :: TempControlledZoneID
  INTEGER, INTENT(IN)     :: ActualZoneNum            ! controlled zone actual zone number

          ! SUBROUTINE PARAMETER DEFINITIONS:
          !  na

          ! INTERFACE BLOCK SPECIFICATIONS
          !  na

          ! DERIVED TYPE DEFINITIONS
          !  na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64)    :: MaxAllowedOvercoolRange    ! Maximum allowed zone overcool range [DeltaC]
  REAL(r64)    :: RelativeHumidityDiff       ! Difference between zone air relative humidity and dehumidifying setpoint [%RH]
  REAL(r64)    :: ZoneOvercoolRange          !
  REAL(r64)    :: ZoneOvercoolControlRatio   !


  IF (.NOT.(AnyZoneTempAndHumidityControl)) RETURN  ! do nothing to setpoint

  IF (.NOT. (TempControlledZone(TempControlledZoneID)%ZoneOvercoolControl)) RETURN ! do nothing to setpoint

  IF (TempControlledZone(TempControlledZoneID)%OverCoolCntrlModeScheduled)THEN
       ZoneOvercoolRange = &
       GetCurrentScheduleValue(TempControlledZone(TempControlledZoneID)%ZoneOverCoolRangeSchedIndex)
  ELSE
       ZoneOvercoolRange = TempControlledZone(TempControlledZoneID)%ZoneOvercoolConstRange
  ENDIF
  ZoneOvercoolControlRatio = TempControlledZone(TempControlledZoneID)%ZoneOvercoolControlRatio

  ! For Dual Setpoint thermostat the overcool range is limited by the temperature difference between cooling
  ! and heating setpoints
  MaxAllowedOvercoolRange = ZoneThermostatSetPointHi(ActualZoneNum) - ZoneThermostatSetPointLo(ActualZoneNum)
  IF (MaxAllowedOvercoolRange > 0.0d0)THEN
      ZoneOvercoolRange = MIN(ZoneOvercoolRange, MaxAllowedOvercoolRange)
  ENDIF
  ! Calculate difference between zone air relative humidity and the dehumidifying setpoint
  RelativeHumidityDiff = ZoneAirRelHum(ActualZoneNum) &
                 - GetCurrentScheduleValue(TempControlledZone(TempControlledZoneID)%DehumidifyingSchedIndex)
  IF (RelativeHumidityDiff > 0.0d0 .AND. ZoneOvercoolControlRatio > 0.0d0) THEN
    ! proportionally reset the cooling setpoint temperature downward (zone Overcool)
    ZoneOvercoolRange = MIN(ZoneOvercoolRange, RelativeHumidityDiff / ZoneOvercoolControlRatio)
    ZoneThermostatSetPointHi(ActualZoneNum) = ZoneThermostatSetPointHi(ActualZoneNum) - ZoneOvercoolRange
  ENDIF
  RETURN

END SUBROUTINE AdjustCoolingSetPointforTempAndHumidityControl

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

END MODULE ZoneTempPredictorCorrector
