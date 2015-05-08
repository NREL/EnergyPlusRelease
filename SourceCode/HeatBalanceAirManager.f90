MODULE HeatBalanceAirManager
          ! Module containing the air heat balance simulation routines
          ! calculation (initialization) routines

          ! MODULE INFORMATION:
          !       AUTHOR         Richard J. Liesen
          !       DATE WRITTEN   February 1998
          !       MODIFIED       May-July 2000 Joe Huang for Comis Link
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS MODULE:
          ! To encapsulate the data and algorithms required to
          ! manage the air simluation heat balance on the building.

          ! METHODOLOGY EMPLOYED:
          !

          ! REFERENCES:
          ! The heat balance method is outlined in the "Tarp Alogorithms Manual"
          ! The methods are also summarized in many BSO Theses and papers.

          ! OTHER NOTES:
          ! This module was created from IBLAST subroutines
          !

          ! USE STATEMENTS:
          ! Use statements for data only modules
USE DataPrecisionGlobals
USE DataGlobals
USE DataEnvironment
USE DataHeatBalFanSys
USE DataHeatBalance
USE DataSurfaces
USE DataInterfaces

          ! Use statements for access to subroutines in other modules
Use Psychrometrics, Only: PsyRhoAirFnPbTdbW, PsyCpAirFnWTdb,PsyHFnTdbW,PsyTdbFnHW

IMPLICIT NONE         ! Enforce explicit typing of all variables

PRIVATE

  ! MODULE PARAMETER DEFINITIONS:
  ! na

!         Subroutine Specifications for the Heat Balance Module
          ! Driver Routines
PUBLIC ManageAirHeatBalance

          ! Get Input routines for module
PRIVATE GetAirHeatBalanceInput
PRIVATE GetAirFlowFlag
PRIVATE GetSimpleAirModelInputs
PRIVATE GetRoomAirModelParameters

          ! Initialization routines for module
PUBLIC InitAirHeatBalance
PRIVATE AllocateAirHeatBalArrays
PRIVATE InitSimpleMixingConvectiveHeatGains

          ! Algorithms for the module
PRIVATE CalcHeatBalanceAir
          ! Reporting routines for module
PRIVATE ReportZoneMeanAirTemp


CONTAINS

! MODULE SUBROUTINES:
!*************************************************************************
SUBROUTINE ManageAirHeatBalance

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Liesen
          !       DATE WRITTEN   February 1998
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine manages the heat air balance method of calculating
          ! building thermal loads.  It is called from the HeatBalanceManager
          ! at the time step level.  This driver manages the calls to all of
          ! the other drivers and simulation algorithms.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

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
  LOGICAL, SAVE :: GetInputFlag=.TRUE.

          ! FLOW:

  ! Obtains and Allocates heat balance related parameters from input file
  IF (GetInputFlag) THEN
    CALL GetAirHeatBalanceInput
    GetInputFlag=.FALSE.
  ENDIF

  CALL InitAirHeatBalance  ! Initialize all heat balance related parameters

    ! Solve the zone heat balance 'Detailed' solution
    ! Call the air surface heat balances
  CALL CalcHeatBalanceAir

  CALL ReportZoneMeanAirTemp

  RETURN

END SUBROUTINE ManageAirHeatBalance

! Get Input Section of the Module
!******************************************************************************
SUBROUTINE GetAirHeatBalanceInput  ! Heat Balance Input Reader Manager

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Liesen
          !       DATE WRITTEN   February 1998
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is the main routine to call other input routines

          ! METHODOLOGY EMPLOYED:
          ! Uses the status flags to trigger events.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

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
  LOGICAL :: ErrorsFound=.FALSE.

          ! FLOW:

  ALLOCATE(CrossMixingFlag(NumOfZones))
  CrossMixingFlag = .FALSE.
  CALL GetAirFlowFlag(ErrorsFound)

  ! get input parameters for modeling of room air flow
  CALL GetRoomAirModelParameters(ErrorsFound)

  IF (ErrorsFound) THEN
    CALL ShowFatalError('GetAirHeatBalanceInput: Errors found in getting Air inputs')
  ENDIF

  RETURN

END SUBROUTINE GetAirHeatBalanceInput

SUBROUTINE GetAirFlowFlag(ErrorsFound)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Garrett Westmacott
          !       DATE WRITTEN   February 2000
          !       MODIFIED       Oct 2003, FCW: Change "Infiltration-Air Change Rate" from Sum to State
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine calls the routine to get simple air flow input data.

          ! METHODOLOGY EMPLOYED:
          ! Modelled after 'Modual Example' in Guide for Module Developers

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: GetNumObjectsFound,GetObjectItem,FindItemInList,VerifyName
  USE ScheduleManager, ONLY: GetScheduleIndex

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  LOGICAL, INTENT(INOUT) :: ErrorsFound ! Set to true if errors found

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

    AirFlowFlag=UseSimpleAirFlow

    CALL GetSimpleAirModelInputs(ErrorsFound)
    IF (TotInfiltration+TotVentilation+TotMixing+TotCrossMixing + TotRefDoorMixing > 0) THEN
      WRITE(OutputFileInits,720) 'Simple'
    END IF

720 Format('! <AirFlow Model>, Simple',/,' AirFlow Model, ',A)
    RETURN

END SUBROUTINE GetAirFlowFlag

SUBROUTINE GetSimpleAirModelInputs(ErrorsFound)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   July 2000
          !       MODIFIED       Oct 2003,FCW: change "Infiltration-Air Change Rate" from Sum to State
          !       MODIFIED       Jan 2008,LG: Allow multiple infiltration and ventilation objects per zone
          !                      May 2009, BG: added calls to setup for possible EMS override
          !                      August 2011, TKS: added refrigeration door mixing
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine gets the input for the "simple" air flow model.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! IDD Statements
          ! INFILTRATION,A1 [Zone Name],A2 [SCHEDULE Name],N1 [Design level KW],
          !     N2 [Constant Term Coefficient], N3 [Temperature Term Coefficient],
          !     N4 [Velocity Term Coefficient], N5 [Velocity Squared Term Coefficient];
          ! MIXING,A1 [Zone Name],A2 [SCHEDULE Name],N1 [Design Level], A3 [Source Zone Name],
          !     N2 [Delta Temperature delta C];
          ! CROSS MIXING,A1 [Zone Name],A2 [SCHEDULE Name],N1 [Design Level],
          !     A3 [Source Zone Name], N2 [Delta Temperature delta C];
          ! REFRIGERATION DOOR MIXING,A1 [Zone Name],A2 [Mate Zone Name],N1 [Design Level],
          !     A3 [Source Zone Name], N2 [Delta Temperature delta C];

          ! USE STATEMENTS:
  USE InputProcessor,  ONLY: GetNumObjectsFound,GetObjectItem,FindItemInList,VerifyName,GetObjectDefMaxArgs
  USE ScheduleManager, ONLY: GetScheduleIndex,GetScheduleValuesForDay,CheckScheduleValueMinMax,GetScheduleMinValue,GetScheduleName
  USE General,         ONLY: RoundSigDigits,CheckCreatedZoneItemName
!  USE DataIPShortCuts
  USE SystemAvailabilityManager, ONLY: GetHybridVentilationControlStatus

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  LOGICAL, INTENT(INOUT) :: ErrorsFound ! IF errors found in input

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER :: Blank=' '
  CHARACTER(len=*), PARAMETER :: fmta='(A)'
  REAL(r64),        PARAMETER :: VentilTempLimit = 100.d0 ! degrees Celsius
  REAL(r64),        PARAMETER :: MixingTempLimit = 100.d0 ! degrees Celsius
  REAL(r64),        PARAMETER :: VentilWSLimit = 40.d0 ! m/s
  CHARACTER(len=*), PARAMETER :: RoutineName='GetSimpleAirModelInputs: ' ! include trailing blank space
  ! Refrigeration Door Mixing Protection types, factors used to moderate mixing flow.
  REAL(r64), PARAMETER :: RefDoorNone = 0.0d0
  REAL(r64), PARAMETER :: RefDoorAirCurtain = 0.5d0
  REAL(r64), PARAMETER :: RefDoorStripCurtain =0.9d0
          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64), ALLOCATABLE, DIMENSION(:,:) :: SVals1
  REAL(r64), ALLOCATABLE, DIMENSION(:,:) :: SVals2
  INTEGER :: NumAlpha                ! Number of Alphas for each GetobjectItem call
  INTEGER :: NumNumber               ! Number of Numbers for each GetobjectItem call
  INTEGER :: maxAlpha                ! max of Alphas for allocation
  INTEGER :: maxNumber               ! max of Numbers for allocation
  INTEGER :: NumArgs
  INTEGER :: IOStat
  CHARACTER(len=MaxNameLength),ALLOCATABLE, DIMENSION(:) :: cAlphaFieldNames
  CHARACTER(len=MaxNameLength),ALLOCATABLE, DIMENSION(:) :: cNumericFieldNames
  LOGICAL, ALLOCATABLE, DIMENSION(:) :: lNumericFieldBlanks
  LOGICAL, ALLOCATABLE, DIMENSION(:) :: lAlphaFieldBlanks
  CHARACTER(len=MaxNameLength),ALLOCATABLE, DIMENSION(:) :: cAlphaArgs
  REAL(r64),ALLOCATABLE, DIMENSION(:) :: rNumericArgs
  CHARACTER(len=MaxNameLength) :: cCurrentModuleObject

  INTEGER                         :: i
  INTEGER                         :: Loop
  INTEGER                         :: Loop1
  INTEGER                         :: JDay
  INTEGER                         :: Hr
  LOGICAL, DIMENSION(:), ALLOCATABLE :: RepVarSet
  LOGICAL, DIMENSION(:), ALLOCATABLE :: OverLap
  INTEGER TS
  LOGICAL :: IsNotOK
  LOGICAL :: IsBlank
  INTEGER :: ZoneNum
  CHARACTER(len=MaxNameLength+50) :: StringOut
  CHARACTER(len=MaxNameLength) :: NameThisObject
  INTEGER :: InfiltCount
  INTEGER :: VentiCount
  LOGICAL :: ControlFlag
  INTEGER :: Item
  INTEGER :: Item1
  LOGICAL :: ErrFlag
  INTEGER :: ZLItem
  REAL(r64), ALLOCATABLE, DIMENSION(:) :: TotInfilVentFlow
  REAL(r64), ALLOCATABLE, DIMENSION(:) :: TotMixingFlow
  INTEGER :: ConnectTest
  INTEGER :: ConnectionNumber
  INTEGER :: NumbNum
  INTEGER :: AlphaNum
  INTEGER :: Zone1Num
  INTEGER :: Zone2Num
  INTEGER :: ZoneNumA
  INTEGER :: ZoneNumB

  ALLOCATE(RepVarSet(NumOfZones))
  RepVarSet=.TRUE.

  ! Following used for reporting
  ALLOCATE(ZnAirRpt(NumOfZones))

  DO Loop=1,NumOfZones
    ! CurrentModuleObject='Zone'
    CALL SetupOutputVariable('Zone Mean Air Temperature [C]',ZnAirRpt(Loop)%MeanAirTemp,  &
       'Zone','Average',Zone(Loop)%Name)
    CALL SetupOutputVariable('Zone Operative Temperature [C]',ZnAirRpt(Loop)%OperativeTemp,  &
       'Zone','Average',Zone(Loop)%Name)
    CALL SetupOutputVariable('Zone Mean Air Dewpoint Temperature [C]',ZnAirRpt(Loop)%MeanAirDewpointTemp,  &
       'Zone','Average',Zone(Loop)%Name)
    CALL SetupOutputVariable('Zone Mean Air Humidity Ratio [kgWater/kgDryAir]',ZnAirRpt(Loop)%MeanAirHumRat,  &
       'Zone','Average',Zone(Loop)%Name)
    CALL SetupOutputVariable('Zone Air Heat Balance Internal Convective Heat Gain Rate [W]',ZnAirRpt(Loop)%SumIntGains,  &
       'System','Average',Zone(Loop)%Name)
    CALL SetupOutputVariable('Zone Air Heat Balance Surface Convection Rate [W]',ZnAirRpt(Loop)%SumHADTsurfs,  &
       'System','Average',Zone(Loop)%Name)
    CALL SetupOutputVariable('Zone Air Heat Balance Interzone Air Transfer Rate [W]',ZnAirRpt(Loop)%SumMCpDTzones,  &
       'System','Average',Zone(Loop)%Name)
    CALL SetupOutputVariable('Zone Air Heat Balance Outdoor Air Transfer Rate [W]',ZnAirRpt(Loop)%SumMCpDtInfil,  &
       'System','Average',Zone(Loop)%Name)
    CALL SetupOutputVariable('Zone Air Heat Balance System Air Transfer Rate [W]',ZnAirRpt(Loop)%SumMCpDTsystem,  &
       'System','Average',Zone(Loop)%Name)
    CALL SetupOutputVariable('Zone Air Heat Balance System Convective Heat Gain Rate [W]',ZnAirRpt(Loop)%SumNonAirSystem,  &
       'System','Average',Zone(Loop)%Name)
    CALL SetupOutputVariable('Zone Air Heat Balance Air Energy Storage Rate [W]',ZnAirRpt(Loop)%CzdTdt,  &
       'System','Average',Zone(Loop)%Name)
    IF (DisplayAdvancedReportVariables) THEN
      CALL SetupOutputVariable('Zone Air Heat Balance Deviation Rate [W]',ZnAirRpt(Loop)%imBalance,  &
         'System','Average',Zone(Loop)%Name)
    ENDIF
  END DO

  cCurrentModuleObject='ZoneAirBalance:OutdoorAir'
  CALL GetObjectDefMaxArgs(cCurrentModuleObject,NumArgs,NumAlpha,NumNumber)
  maxAlpha=NumAlpha
  maxNumber=NumNumber
  cCurrentModuleObject='ZoneInfiltration:EffectiveLeakageArea'
  CALL GetObjectDefMaxArgs(cCurrentModuleObject,NumArgs,NumAlpha,NumNumber)
  maxAlpha=MAX(NumAlpha,maxAlpha)
  maxNumber=MAX(NumNumber,maxNumber)
  cCurrentModuleObject='ZoneInfiltration:FlowCoefficient'
  CALL GetObjectDefMaxArgs(cCurrentModuleObject,NumArgs,NumAlpha,NumNumber)
  maxAlpha=MAX(NumAlpha,maxAlpha)
  maxNumber=MAX(NumNumber,maxNumber)
  cCurrentModuleObject='ZoneInfiltration:DesignFlowRate'
  CALL GetObjectDefMaxArgs(cCurrentModuleObject,NumArgs,NumAlpha,NumNumber)
  maxAlpha=MAX(NumAlpha,maxAlpha)
  maxNumber=MAX(NumNumber,maxNumber)
  cCurrentModuleObject='ZoneVentilation:DesignFlowRate'
  CALL GetObjectDefMaxArgs(cCurrentModuleObject,NumArgs,NumAlpha,NumNumber)
  maxAlpha=MAX(NumAlpha,maxAlpha)
  maxNumber=MAX(NumNumber,maxNumber)
  cCurrentModuleObject='ZoneVentilation:WindandStackOpenArea'
  CALL GetObjectDefMaxArgs(cCurrentModuleObject,NumArgs,NumAlpha,NumNumber)
  maxAlpha=MAX(NumAlpha,maxAlpha)
  maxNumber=MAX(NumNumber,maxNumber)
  cCurrentModuleObject='ZoneMixing'
  CALL GetObjectDefMaxArgs(cCurrentModuleObject,NumArgs,NumAlpha,NumNumber)
  maxAlpha=MAX(NumAlpha,maxAlpha)
  maxNumber=MAX(NumNumber,maxNumber)
  cCurrentModuleObject='ZoneCrossMixing'
  CALL GetObjectDefMaxArgs(cCurrentModuleObject,NumArgs,NumAlpha,NumNumber)
  maxAlpha=MAX(NumAlpha,maxAlpha)
  maxNumber=MAX(NumNumber,maxNumber)
  cCurrentModuleObject='ZoneRefrigerationDoorMixing'
  CALL GetObjectDefMaxArgs(cCurrentModuleObject,NumArgs,NumAlpha,NumNumber)
  maxAlpha=MAX(NumAlpha,maxAlpha)
  maxNumber=MAX(NumNumber,maxNumber)

  ALLOCATE(cAlphaArgs(maxAlpha))
  cAlphaArgs=' '
  ALLOCATE(cAlphaFieldNames(maxAlpha))
  cAlphaFieldNames=' '
  ALLOCATE(cNumericFieldNames(maxNumber))
  cNumericFieldNames=' '
  ALLOCATE(rNumericArgs(maxNumber))
  rNumericArgs=0.0d0
  ALLOCATE(lAlphaFieldBlanks(maxAlpha))
  lAlphaFieldBlanks=.true.
  ALLOCATE(lNumericFieldBlanks(maxNumber))
  lNumericFieldBlanks=.true.

  cCurrentModuleObject='ZoneAirBalance:OutdoorAir'
  TotZoneAirBalance=GetNumObjectsFound(cCurrentModuleObject)

  ALLOCATE(ZoneAirBalance(TotZoneAirBalance))

  DO Loop=1,TotZoneAirBalance
    CALL GetObjectItem(cCurrentModuleObject,Loop,cAlphaArgs,NumAlpha,rNumericArgs,NumNumber,IOStat, &
                       NumBlank=lNumericFieldBlanks,AlphaBlank=lAlphaFieldBlanks, &
                       AlphaFieldNames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
    IsNotOK = .FALSE.
    IsBlank = .FALSE.
    CALL VerifyName(cAlphaArgs(1),ZoneAirBalance%Name,Loop-1,IsNotOK,IsBlank,TRIM(cCurrentModuleObject)//' Name')
    IF (IsNotOK) THEN
      ErrorsFound = .TRUE.
      IF (IsBlank) cAlphaArgs(1) = 'xxxxx'
    END IF
    ZoneAirBalance(Loop)%Name = cAlphaArgs(1)
    ZoneAirBalance(Loop)%ZoneName = cAlphaArgs(2)
    ZoneAirBalance(Loop)%ZonePtr=FindIteminList(cAlphaArgs(2),Zone%Name,NumOfZones)
    IF (ZoneAirBalance(Loop)%ZonePtr == 0) THEN
      CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))// &
           '", invalid (not found) '//trim(cAlphaFieldNames(2))//'="'//TRIM(cAlphaArgs(2))//'".')
      ErrorsFound=.TRUE.
    ENDIF
    CALL VerifyName(cAlphaArgs(2),ZoneAirBalance%ZoneName,Loop-1,IsNotOK,IsBlank,TRIM(cCurrentModuleObject)//' Name')
    IF (IsNotOK) THEN
      CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))// &
           '", a duplicated object '//trim(cAlphaFieldNames(2))//'="'//TRIM(cAlphaArgs(2))//'" is found.')
      CALL ShowContinueError('A zone can only have one '//TRIM(cCurrentModuleObject)//' object.')
      ErrorsFound = .TRUE.
    END IF

    SELECT CASE (cAlphaArgs(3))  ! Aie balance method type character input-->convert to integer
      CASE ('QUADRATURE')
        ZoneAirBalance(Loop)%BalanceMethod = AirBalanceQuadrature
      CASE ('NONE')
        ZoneAirBalance(Loop)%BalanceMethod = AirBalanceNone
      CASE DEFAULT
        ZoneAirBalance(Loop)%BalanceMethod = AirBalanceNone
        CALL ShowWarningError(RoutineName//TRIM(cAlphaFieldNames(3))//' = '//TRIM(cAlphaArgs(3))//  &
           ' not valid choice for '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
        CALL ShowContinueError('The dafualt choice "NONE" is assigned')
    END SELECT

    ZoneAirBalance(Loop)%InducedAirRate = rNumericArgs(1)
    If (rNumericArgs(1) < 0.0d0) Then
      CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
        '", invalid Induced Outdoor Air Due to Duct Leakage Unbalance specification [<0.0]='//   &
        TRIM(RoundSigDigits(rNumericArgs(1),3)))
      ErrorsFound=.TRUE.
    End If

    ZoneAirBalance(Loop)%InducedAirSchedPtr=GetScheduleIndex(cAlphaArgs(4))
    IF (ZoneAirBalance(Loop)%InducedAirSchedPtr == 0) THEN
      IF (lAlphaFieldBlanks(4)) THEN
        CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'",'//   &
             TRIM(cAlphaFieldNames(4))//' is required but field is blank.')
      ELSE
        CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'",'//   &
             ' invalid (not found) '//TRIM(cAlphaFieldNames(4))//'="'//trim(cAlphaArgs(4))//'".')
      END IF
      ErrorsFound=.TRUE.
    ENDIF
    IF (.NOT. CheckScheduleValueMinMax(ZoneAirBalance(Loop)%InducedAirSchedPtr,'>=',0.0d0,'<=',1.0d0)) THEN
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//' = '//TRIM(ZoneAirBalance(Loop)%Name)//&
           ':  Error found in '//TRIM(cAlphaFieldNames(4))//' = '//TRIM(cAlphaArgs(4)))
      CALL ShowContinueError('Schedule values must be (>=0., <=1.)')
      ErrorsFound=.true.
    END IF

    ! Check whether this zone is also controleld by hybrid ventilation object with ventilation control option or not
    ControlFlag = GetHybridVentilationControlStatus(ZoneAirBalance(Loop)%ZonePtr)
    If (ControlFlag .AND. ZoneAirBalance(Loop)%BalanceMethod == AirBalanceQuadrature) Then
      ZoneAirBalance(Loop)%BalanceMethod = AirBalanceNone
      CALL ShowWarningError(TRIM(cCurrentModuleObject)//' = '//TRIM(ZoneAirBalance(Loop)%Name)//': This Zone ('// &
        TRIM(cAlphaArgs(2))//') is controlled by AvailabilityManager:HybridVentilation with Simple Airflow Control Type option.')
      CALL ShowContinueError('Air balance method type QUADRATURE and Simple Airflow Control Type cannot co-exist. ' &
        //'The NONE method is assigned')
    End If

    IF (ZoneAirBalance(Loop)%BalanceMethod == AirBalanceQuadrature) THEN
      CALL SetupOutputVariable('Zone Combined Outdoor Air Sensible Heat Loss Energy [J]',  &
          ZnAirRpt(ZoneAirBalance(Loop)%ZonePtr)%OABalanceHeatLoss,'System','Sum',Zone(ZoneAirBalance(Loop)%ZonePtr)%Name)
      CALL SetupOutputVariable('Zone Combined Outdoor Air Sensible Heat Gain Energy [J]',  &
          ZnAirRpt(ZoneAirBalance(Loop)%ZonePtr)%OABalanceHeatGain,'System','Sum',Zone(ZoneAirBalance(Loop)%ZonePtr)%Name)
      CALL SetupOutputVariable('Zone Combined Outdoor Air Latent Heat Loss Energy [J]',  &
          ZnAirRpt(ZoneAirBalance(Loop)%ZonePtr)%OABalanceLatentLoss,'System','Sum',Zone(ZoneAirBalance(Loop)%ZonePtr)%Name)
      CALL SetupOutputVariable('Zone Combined Outdoor Air Latent Heat Gain Energy [J]', &
          ZnAirRpt(ZoneAirBalance(Loop)%ZonePtr)%OABalanceLatentGain,'System','Sum',Zone(ZoneAirBalance(Loop)%ZonePtr)%Name)
      CALL SetupOutputVariable('Zone Combined Outdoor Air Total Heat Loss Energy [J]', &
          ZnAirRpt(ZoneAirBalance(Loop)%ZonePtr)%OABalanceTotalLoss,'System','Sum',Zone(ZoneAirBalance(Loop)%ZonePtr)%Name)
      CALL SetupOutputVariable('Zone Combined Outdoor Air Total Heat Gain Energy [J]',  &
          ZnAirRpt(ZoneAirBalance(Loop)%ZonePtr)%OABalanceTotalGain,'System','Sum',Zone(ZoneAirBalance(Loop)%ZonePtr)%Name)
      CALL SetupOutputVariable('Zone Combined Outdoor Air Current Density Volume Flow Rate [m3/s]',  &
                                  ZnAirRpt(ZoneAirBalance(Loop)%ZonePtr)%OABalanceVdotCurDensity,   &
                                 'System','Sum',Zone(ZoneAirBalance(Loop)%ZonePtr)%Name)
      CALL SetupOutputVariable('Zone Combined Outdoor Air Standard Density Volume Flow Rate [m3/s]', &
                                  ZnAirRpt(ZoneAirBalance(Loop)%ZonePtr)%OABalanceVdotStdDensity,   &
                                 'System','Sum',Zone(ZoneAirBalance(Loop)%ZonePtr)%Name)
      CALL SetupOutputVariable('Zone Combined Outdoor Air Current Density Volume [m3]',              &
                                  ZnAirRpt(ZoneAirBalance(Loop)%ZonePtr)%OABalanceVolumeCurDensity, &
                                 'System','Sum',Zone(ZoneAirBalance(Loop)%ZonePtr)%Name)
      CALL SetupOutputVariable('Zone Combined Outdoor Air Standard Density Volume [m3]',             &
                                  ZnAirRpt(ZoneAirBalance(Loop)%ZonePtr)%OABalanceVolumeStdDensity, &
                                 'System','Sum',Zone(ZoneAirBalance(Loop)%ZonePtr)%Name)
      CALL SetupOutputVariable('Zone Combined Outdoor Air Mass [kg]',ZnAirRpt(ZoneAirBalance(Loop)%ZonePtr)%OABalanceMass,  &
                                 'System','Sum',Zone(ZoneAirBalance(Loop)%ZonePtr)%Name)
      CALL SetupOutputVariable('Zone Combined Outdoor Air Changes per Hour [ach]',                   &
                                  ZnAirRpt(ZoneAirBalance(Loop)%ZonePtr)%OABalanceAirChangeRate,   &
                                 'System','Average',Zone(ZoneAirBalance(Loop)%ZonePtr)%Name)
      CALL SetupOutputVariable('Zone Combined Outdoor Air Fan Electric Energy [J]',     &
                                  ZnAirRpt(ZoneAirBalance(Loop)%ZonePtr)%OABalanceFanElec,  &
                                 'System','Sum',Zone(ZoneAirBalance(Loop)%ZonePtr)%Name, &
                                  ResourceTypeKey='Electricity',GroupKey='Building', &
                                  ZoneKey=Zone(ZoneAirBalance(Loop)%ZonePtr)%Name, &
                                  EndUseKey='Fans', EndUseSubKey='Ventilation (simple)')
   END IF

  END DO

  cCurrentModuleObject='ZoneInfiltration:EffectiveLeakageArea'
  TotShermGrimsInfiltration=GetNumObjectsFound(cCurrentModuleObject)

  cCurrentModuleObject='ZoneInfiltration:FlowCoefficient'
  TotAIM2Infiltration=GetNumObjectsFound(cCurrentModuleObject)

  cCurrentModuleObject='ZoneInfiltration:DesignFlowRate'
  NumInfiltrationStatements=GetNumObjectsFound(cCurrentModuleObject)

  ALLOCATE(InfiltrationObjects(NumInfiltrationStatements))

  TotDesignFlowInfiltration=0
  ErrFlag=.false.
  DO Item=1,NumInfiltrationStatements
    CALL GetObjectItem(cCurrentModuleObject,Item,cAlphaArgs,NumAlpha,rNumericArgs,NumNumber,IOStat, &
                       NumBlank=lNumericFieldBlanks,AlphaBlank=lAlphaFieldBlanks, &
                       AlphaFieldNames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
    IsNotOK = .FALSE.
    IsBlank = .FALSE.
    CALL VerifyName(cAlphaArgs(1),InfiltrationObjects%Name,Item-1,IsNotOK,IsBlank,TRIM(cCurrentModuleObject)//' Name')
    IF (IsNotOK) THEN
      ErrorsFound = .TRUE.
      ErrFlag=.true.
      IF (IsBlank) cAlphaArgs(1) = 'xxxxx'
    END IF
    InfiltrationObjects(Item)%Name = cAlphaArgs(1)
    Item1=FindItemInList(cAlphaArgs(2),Zone%Name,NumOfZones)
    ZLItem=0
    IF (Item1 == 0 .and. NumOfZoneLists > 0) &
        ZLItem=FindItemInList(cAlphaArgs(2),ZoneList%Name,NumOfZoneLists)
    IF (Item1 > 0) THEN
      InfiltrationObjects(Item)%StartPtr=TotDesignFlowInfiltration+1
      TotDesignFlowInfiltration=TotDesignFlowInfiltration+1
      InfiltrationObjects(Item)%NumOfZones=1
      InfiltrationObjects(Item)%ZoneListActive=.false.
      InfiltrationObjects(Item)%ZoneOrZoneListPtr=Item1
    ELSEIF (ZLItem > 0) THEN
      InfiltrationObjects(Item)%StartPtr=TotDesignFlowInfiltration+1
      TotDesignFlowInfiltration=TotDesignFlowInfiltration+ZoneList(ZLItem)%NumOfZones
      InfiltrationObjects(Item)%NumOfZones=ZoneList(ZLItem)%NumOfZones
      InfiltrationObjects(Item)%ZoneListActive=.true.
      InfiltrationObjects(Item)%ZoneOrZoneListPtr=ZLItem
    ELSE
      CALL ShowSevereError(trim(cCurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid '//  &
           trim(cAlphaFieldNames(2))//'="'//trim(cAlphaArgs(2))//'" not found.')
      ErrorsFound=.true.
      ErrFlag=.true.
    ENDIF
  ENDDO

  IF (ErrFlag) THEN
    CALL ShowSevereError(RoutineName//'Errors with invalid names in '//trim(cCurrentModuleObject)//  &
       ' objects.')
    CALL ShowContinueError('...These will not be read in.  Other errors may occur.')
    TotDesignFlowInfiltration=0
  ENDIF

  TotInfiltration = TotDesignFlowInfiltration + TotShermGrimsInfiltration + TotAIM2Infiltration

  ALLOCATE(Infiltration(TotInfiltration))

  IF (TotDesignFlowInfiltration > 0) THEN
    Loop=0
    cCurrentModuleObject='ZoneInfiltration:DesignFlowRate'
    DO Item = 1, NumInfiltrationStatements

      CALL GetObjectItem(cCurrentModuleObject,Item,cAlphaArgs,NumAlpha,rNumericArgs,NumNumber,IOStat, &
                       NumBlank=lNumericFieldBlanks,AlphaBlank=lAlphaFieldBlanks, &
                       AlphaFieldNames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

      DO Item1=1,InfiltrationObjects(Item)%NumOfZones
        Loop=Loop+1
        IF (.not. InfiltrationObjects(Item)%ZoneListActive) THEN
          Infiltration(Loop)%Name = cAlphaArgs(1)
          Infiltration(Loop)%ZonePtr = InfiltrationObjects(Item)%ZoneOrZoneListPtr
        ELSE
          CALL CheckCreatedZoneItemName(RoutineName,cCurrentModuleObject,  &
                                        Zone(ZoneList(InfiltrationObjects(Item)%ZoneOrZoneListPtr)%Zone(Item1))%Name,  &
                                        ZoneList(InfiltrationObjects(Item)%ZoneOrZoneListPtr)%MaxZoneNameLength,  &
                                        InfiltrationObjects(Item)%Name,     &
                                        Infiltration%Name,           &
                                        Loop-1,                       &
                                        Infiltration(Loop)%Name,            &
                                        ErrFlag)
          Infiltration(Loop)%ZonePtr = ZoneList(InfiltrationObjects(Item)%ZoneOrZoneListPtr)%Zone(Item1)
          IF (ErrFlag) ErrorsFound=.true.
        ENDIF

        Infiltration(Loop)%ModelType = InfiltrationDesignFlowRate
        Infiltration(Loop)%SchedPtr=GetScheduleIndex(cAlphaArgs(3))
        IF (Infiltration(Loop)%SchedPtr == 0) THEN
          IF (Item1 == 1) THEN
            IF (lAlphaFieldBlanks(3)) THEN
              CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'",'//   &
                   TRIM(cAlphaFieldNames(3))//' is required but field is blank.')
            ELSE
              CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'",'//   &
                   ' invalid (not found) '//TRIM(cAlphaFieldNames(3))//'="'//trim(cAlphaArgs(3))//'".')
            END IF
            ErrorsFound=.TRUE.
          ENDIF
        ENDIF

        ! setup a flag if the outdoor air balance method is applied
        IF (Infiltration(Loop)%ZonePtr > 0 .AND. TotZoneAirBalance > 0) Then
          DO I=1,TotZoneAirBalance
            IF (Infiltration(Loop)%ZonePtr == ZoneAirBalance(i)%ZonePtr) THEN
              IF (ZoneAirBalance(i)%BalanceMethod == AirBalanceQuadrature) THEN
                Infiltration(Loop)%QuadratureSum = .TRUE.
                Infiltration(Loop)%OABalancePtr = I
                EXIT
              END IF
            END IF
          END DO
        End If

        ! Infiltration equipment design level calculation method.
        SELECT CASE (cAlphaArgs(4))
          CASE('FLOW','FLOW/ZONE')
            Infiltration(Loop)%DesignLevel=rNumericArgs(1)
            IF (lNumericFieldBlanks(1)) THEN
              CALL ShowWarningError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(Infiltration(Loop)%Name)//  &
                '", '//trim(cAlphaFieldNames(4))//' specifies '//TRIM(cNumericFieldNames(1))//  &
                ', but that field is blank.  0 Infiltration will result.')
            ENDIF

          CASE('FLOW/AREA')
            IF (Infiltration(Loop)%ZonePtr /= 0) THEN
              IF (rNumericArgs(2) >= 0.0d0) THEN
                Infiltration(Loop)%DesignLevel=rNumericArgs(2)*Zone(Infiltration(Loop)%ZonePtr)%FloorArea
                IF (Infiltration(Loop)%ZonePtr > 0) THEN
                  IF (Zone(Infiltration(Loop)%ZonePtr)%FloorArea <= 0.0d0) THEN
                    CALL ShowWarningError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(Infiltration(Loop)%Name)//  &
                      '", '//trim(cAlphaFieldNames(4))//' specifies '//TRIM(cNumericFieldNames(2))//  &
                      ', but Zone Floor Area = 0.  0 Infiltration will result.')
                  ENDIF
                ENDIF
              ELSE
                CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(Infiltration(Loop)%Name)//  &
                                 '", invalid flow/area specification [<0.0]='//   &
                                 TRIM(RoundSigDigits(rNumericArgs(2),3)))
                ErrorsFound=.TRUE.
              ENDIF
            ENDIF
            IF (lNumericFieldBlanks(2)) THEN
              CALL ShowWarningError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(Infiltration(Loop)%Name)//  &
                '", '//trim(cAlphaFieldNames(4))//' specifies '//TRIM(cNumericFieldNames(2))//  &
                ', but that field is blank.  0 Infiltration will result.')
            ENDIF

          CASE('FLOW/EXTERIORAREA')
            IF (Infiltration(Loop)%ZonePtr /= 0) THEN
              IF (rNumericArgs(3) >= 0.0d0) THEN
                Infiltration(Loop)%DesignLevel=rNumericArgs(3)*Zone(Infiltration(Loop)%ZonePtr)%ExteriorTotalSurfArea
                IF (Zone(Infiltration(Loop)%ZonePtr)%ExteriorTotalSurfArea <= 0.0d0) THEN
                  CALL ShowWarningError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(Infiltration(Loop)%Name)//  &
                    '", '//trim(cAlphaFieldNames(4))//' specifies '//TRIM(cNumericFieldNames(3))//  &
                    ', but Exterior Surface Area = 0.  0 Infiltration will result.')
                ENDIF
              ELSE
                CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//' = "'//TRIM(Infiltration(Loop)%Name)//  &
                                 '", invalid flow/exteriorarea specification [<0.0]='//   &
                                 TRIM(RoundSigDigits(rNumericArgs(3),3)))
                ErrorsFound=.TRUE.
              ENDIF
            ENDIF
            IF (lNumericFieldBlanks(3)) THEN
              CALL ShowWarningError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(Infiltration(Loop)%Name)//  &
                '", '//trim(cAlphaFieldNames(4))//' specifies '//TRIM(cNumericFieldNames(3))//  &
                ', but that field is blank.  0 Infiltration will result.')
            ENDIF
          CASE('FLOW/EXTERIORWALLAREA')
            IF (Infiltration(Loop)%ZonePtr /= 0) THEN
              IF (rNumericArgs(3) >= 0.0d0) THEN
                Infiltration(Loop)%DesignLevel=rNumericArgs(3)*Zone(Infiltration(Loop)%ZonePtr)%ExtGrossWallArea
                IF (Zone(Infiltration(Loop)%ZonePtr)%ExtGrossWallArea <= 0.0d0) THEN
                  CALL ShowWarningError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(Infiltration(Loop)%Name)//  &
                    '", '//trim(cAlphaFieldNames(4))//' specifies '//TRIM(cNumericFieldNames(3))//  &
                    ', but Exterior Wall Area = 0.  0 Infiltration will result.')
                ENDIF
              ELSE
                CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//' = "'//TRIM(Infiltration(Loop)%Name)//  &
                                 '", invalid flow/exteriorwallarea specification [<0.0]='//   &
                                 TRIM(RoundSigDigits(rNumericArgs(3),3)))
                ErrorsFound=.TRUE.
              ENDIF
            ENDIF
            IF (lNumericFieldBlanks(3)) THEN
              CALL ShowWarningError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(Infiltration(Loop)%Name)//  &
                '", '//trim(cAlphaFieldNames(4))//' specifies '//TRIM(cNumericFieldNames(3))//  &
                ', but that field is blank.  0 Infiltration will result.')
            ENDIF
          CASE('AIRCHANGES/HOUR')
            IF (Infiltration(Loop)%ZonePtr /= 0) THEN
              IF (rNumericArgs(4) >= 0.0d0) THEN
                Infiltration(Loop)%DesignLevel=rNumericArgs(4)*Zone(Infiltration(Loop)%ZonePtr)%Volume/SecInHour
                IF (Zone(Infiltration(Loop)%ZonePtr)%Volume <= 0.0d0) THEN
                  CALL ShowWarningError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(Infiltration(Loop)%Name)//  &
                    '", '//trim(cAlphaFieldNames(4))//' specifies '//TRIM(cNumericFieldNames(4))//  &
                    ', but Zone Volume = 0.  0 Infiltration will result.')
                ENDIF
              ELSE
                CALL ShowSevereError(RoutineName//'In '//TRIM(cCurrentModuleObject)//' = "'//TRIM(Infiltration(Loop)%Name)//  &
                                 '", invalid ACH (air changes per hour) specification [<0.0]='//   &
                                 TRIM(RoundSigDigits(rNumericArgs(4),3)))
                ErrorsFound=.TRUE.
              ENDIF
            ENDIF
            IF (lNumericFieldBlanks(4)) THEN
              CALL ShowWarningError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(Infiltration(Loop)%Name)//  &
                '", '//trim(cAlphaFieldNames(4))//' specifies '//TRIM(cNumericFieldNames(4))//  &
                ', but that field is blank.  0 Infiltration will result.')
            ENDIF

          CASE DEFAULT
            IF (Item1 == 1) THEN
              CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
                                   '", invalid calculation method='//   &
                                   TRIM(cAlphaArgs(4)))
              ErrorsFound=.TRUE.
            ENDIF
        END SELECT

        IF (.not. lNumericFieldBlanks(5)) THEN
          Infiltration(Loop)%ConstantTermCoef=rNumericArgs(5)
        ELSE
          Infiltration(Loop)%ConstantTermCoef=1.0d0
        ENDIF
        IF (.not. lNumericFieldBlanks(6)) THEN
          Infiltration(Loop)%TemperatureTermCoef=rNumericArgs(6)
        ELSE
          Infiltration(Loop)%TemperatureTermCoef=0.0d0
        ENDIF
        IF (.not. lNumericFieldBlanks(7)) THEN
          Infiltration(Loop)%VelocityTermCoef=rNumericArgs(7)
        ELSE
          Infiltration(Loop)%VelocityTermCoef=0.0d0
        ENDIF
        IF (.not. lNumericFieldBlanks(8)) THEN
          Infiltration(Loop)%VelocitySQTermCoef=rNumericArgs(8)
        ELSE
          Infiltration(Loop)%VelocitySQTermCoef=0.0d0
        ENDIF

        IF (Infiltration(Loop)%ConstantTermCoef == 0.0d0 .and. Infiltration(Loop)%TemperatureTermCoef == 0.0d0 .and.  &
            Infiltration(Loop)%VelocityTermCoef == 0.0d0 .and. Infiltration(Loop)%VelocitySQTermCoef == 0.0d0)  THEN
            IF (Item1 == 1) THEN
              CALL ShowWarningError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'", in '// &
                                      TRIM(cAlphaFieldNames(2))//'="'//TRIM(cAlphaArgs(2))//'".')
              CALL ShowContinueError('Infiltration Coefficients are all zero.  No Infiltration will be reported.')
            ENDIF
        ENDIF
      END DO
    END DO
  ENDIF

  cCurrentModuleObject='ZoneInfiltration:EffectiveLeakageArea'
  InfiltCount=TotDesignFlowInfiltration
  DO Loop=1,TotShermGrimsInfiltration
    CALL GetObjectItem(cCurrentModuleObject,Loop,cAlphaArgs,NumAlpha,rNumericArgs,NumNumber,IOStat, &
                       NumBlank=lNumericFieldBlanks,AlphaBlank=lAlphaFieldBlanks, &
                       AlphaFieldNames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

    IsNotOK = .FALSE.
    IsBlank = .FALSE.
    InfiltCount = InfiltCount + 1
    CALL VerifyName(cAlphaArgs(1),Infiltration%Name,InfiltCount-1,IsNotOK,IsBlank,TRIM(cCurrentModuleObject)//' Name')
    IF (IsNotOK) THEN
      ErrorsFound = .TRUE.
      IF (IsBlank) cAlphaArgs(1) = 'xxxxx'
    END IF
    Infiltration(InfiltCount)%Name =  cAlphaArgs(1)
    Infiltration(InfiltCount)%ModelType = InfiltrationShermanGrimsrud
    Infiltration(InfiltCount)%ZonePtr=FindIteminList(cAlphaArgs(2),Zone%Name,NumOfZones)
    IF (Infiltration(InfiltCount)%ZonePtr == 0) THEN
      CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))// &
           '", invalid (not found) '//trim(cAlphaFieldNames(2))//'="'//TRIM(cAlphaArgs(2))//'".')
      ErrorsFound=.TRUE.
    ENDIF

    ! setup a flag if the outdoor air balance method is applied
    IF (Infiltration(Loop)%ZonePtr > 0 .AND. TotZoneAirBalance > 0) Then
      DO I=1,TotZoneAirBalance
        IF (Infiltration(Loop)%ZonePtr == ZoneAirBalance(i)%ZonePtr) THEN
          IF (ZoneAirBalance(i)%BalanceMethod == AirBalanceQuadrature) THEN
            Infiltration(Loop)%QuadratureSum = .TRUE.
            Infiltration(Loop)%OABalancePtr = I
            EXIT
          END IF
        END IF
      END DO
    End If

    Infiltration(InfiltCount)%SchedPtr=GetScheduleIndex(cAlphaArgs(3))
    IF (Infiltration(InfiltCount)%SchedPtr == 0) THEN
      IF (lAlphaFieldBlanks(3)) THEN
        CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'",'//   &
             TRIM(cAlphaFieldNames(3))//' is required but field is blank.')
      ELSE
        CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'",'//   &
             ' invalid (not found) '//TRIM(cAlphaFieldNames(3))//'="'//trim(cAlphaArgs(3))//'".')
      END IF
      ErrorsFound=.TRUE.
    ENDIF
    Infiltration(InfiltCount)%LeakageArea           = rNumericArgs(1)
    Infiltration(InfiltCount)%BasicStackCoefficient = rNumericArgs(2)
    Infiltration(InfiltCount)%BasicWindCoefficient  = rNumericArgs(3)

    !check if zone has exterior surfaces
    IF (Infiltration(InfiltCount)%ZonePtr > 0) THEN
      IF (Zone(Infiltration(InfiltCount)%ZonePtr)%ExteriorTotalSurfArea <= 0.0d0) THEN
        CALL ShowWarningError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//   &
           '", '//trim(cAlphaFieldNames(2))//'="'//trim(cAlphaArgs(2))//'" does not have surfaces exposed to outdoors.')
        CALL ShowContinueError('Infiltration model is appropriate for exterior zones not interior zones, simulation continues.')
      ENDIF
    ENDIF
  ENDDO

  cCurrentModuleObject='ZoneInfiltration:FlowCoefficient'
  DO Loop=1,TotAIM2Infiltration
    CALL GetObjectItem(cCurrentModuleObject,Loop,cAlphaArgs,NumAlpha,rNumericArgs,NumNumber,IOStat, &
                       NumBlank=lNumericFieldBlanks,AlphaBlank=lAlphaFieldBlanks, &
                       AlphaFieldNames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
    IsNotOK = .FALSE.
    IsBlank = .FALSE.
    InfiltCount = InfiltCount + 1
    CALL VerifyName(cAlphaArgs(1),Infiltration%Name,InfiltCount-1,IsNotOK,IsBlank,TRIM(cCurrentModuleObject)//' Name')
    IF (IsNotOK) THEN
      ErrorsFound = .TRUE.
      IF (IsBlank) cAlphaArgs(1) = 'xxxxx'
    END IF
    Infiltration(InfiltCount)%Name =  cAlphaArgs(1)
    Infiltration(InfiltCount)%ModelType = InfiltrationAIM2
    Infiltration(InfiltCount)%ZonePtr=FindIteminList(cAlphaArgs(2),Zone%Name,NumOfZones)
    IF (Infiltration(InfiltCount)%ZonePtr == 0) THEN
      CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))// &
           '", invalid (not found) '//trim(cAlphaFieldNames(2))//'="'//TRIM(cAlphaArgs(2))//'".')
      ErrorsFound=.TRUE.
    ENDIF

    ! setup a flag if the outdoor air balance method is applied
    IF (Infiltration(Loop)%ZonePtr > 0 .AND. TotZoneAirBalance > 0) Then
      DO I=1,TotZoneAirBalance
        IF (Infiltration(Loop)%ZonePtr == ZoneAirBalance(i)%ZonePtr) THEN
          IF (ZoneAirBalance(i)%BalanceMethod == AirBalanceQuadrature) THEN
            Infiltration(Loop)%QuadratureSum = .TRUE.
            Infiltration(Loop)%OABalancePtr = I
            EXIT
          END IF
        END IF
      END DO
    End If

    Infiltration(InfiltCount)%SchedPtr=GetScheduleIndex(cAlphaArgs(3))
    IF (Infiltration(InfiltCount)%SchedPtr == 0) THEN
      IF (lAlphaFieldBlanks(3)) THEN
        CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'",'//   &
             TRIM(cAlphaFieldNames(3))//' is required but field is blank.')
      ELSE
        CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'",'//   &
             ' invalid (not found) '//TRIM(cAlphaFieldNames(3))//'="'//trim(cAlphaArgs(3))//'".')
      END IF
      ErrorsFound=.TRUE.
    ENDIF
    Infiltration(InfiltCount)%FlowCoefficient      = rNumericArgs(1)
    Infiltration(InfiltCount)%AIM2StackCoefficient = rNumericArgs(2)
    Infiltration(InfiltCount)%PressureExponent     = rNumericArgs(3)
    Infiltration(InfiltCount)%AIM2WindCoefficient  = rNumericArgs(4)
    Infiltration(InfiltCount)%ShelterFactor        = rNumericArgs(5)

        !check if zone has exterior surfaces
    IF (Infiltration(InfiltCount)%ZonePtr > 0) THEN
      IF (Zone(Infiltration(InfiltCount)%ZonePtr)%ExteriorTotalSurfArea <= 0.0d0) THEN
        CALL ShowWarningError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//   &
           '", '//trim(cAlphaFieldNames(2))//'="'//trim(cAlphaArgs(2))//'" does not have surfaces exposed to outdoors.')
        CALL ShowContinueError('Infiltration model is appropriate for exterior zones not interior zones, simulation continues.')
      ENDIF
    ENDIF

  ENDDO

  !setup zone-level infiltration reports
  DO Loop=1, TotInfiltration
    IF (Infiltration(Loop)%ZonePtr > 0 .AND. .NOT. Infiltration(Loop)%QuadratureSum) THEN
      IF (RepVarSet(Infiltration(Loop)%ZonePtr)) THEN
        RepVarSet(Infiltration(Loop)%ZonePtr)=.FALSE.
        CALL SetupOutputVariable('Zone Infiltration Sensible Heat Loss Energy [J]',  &
                                 ZnAirRpt(Infiltration(Loop)%ZonePtr)%InfilHeatLoss,  &
                                 'System','Sum',Zone(Infiltration(Loop)%ZonePtr)%Name)
        CALL SetupOutputVariable('Zone Infiltration Sensible Heat Gain Energy [J]',  &
                                 ZnAirRpt(Infiltration(Loop)%ZonePtr)%InfilHeatGain,  &
                                 'System','Sum',Zone(Infiltration(Loop)%ZonePtr)%Name)
        CALL SetupOutputVariable('Zone Infiltration Latent Heat Loss Energy [J]', &
                                  ZnAirRpt(Infiltration(Loop)%ZonePtr)%InfilLatentLoss,  &
                                 'System','Sum',Zone(Infiltration(Loop)%ZonePtr)%Name)
        CALL SetupOutputVariable('Zone Infiltration Latent Heat Gain Energy [J]', &
                                  ZnAirRpt(Infiltration(Loop)%ZonePtr)%InfilLatentGain,  &
                                 'System','Sum',Zone(Infiltration(Loop)%ZonePtr)%Name)
        CALL SetupOutputVariable('Zone Infiltration Total Heat Loss Energy [J]',  &
                                 ZnAirRpt(Infiltration(Loop)%ZonePtr)%InfilTotalLoss,  &
                                 'System','Sum',Zone(Infiltration(Loop)%ZonePtr)%Name)
        CALL SetupOutputVariable('Zone Infiltration Total Heat Gain Energy [J]',  &
                                 ZnAirRpt(Infiltration(Loop)%ZonePtr)%InfilTotalGain,  &
                                 'System','Sum',Zone(Infiltration(Loop)%ZonePtr)%Name)
        CALL SetupOutputVariable('Zone Infiltration Current Density Volume Flow Rate [m3/s]', &
                                  ZnAirRpt(Infiltration(Loop)%ZonePtr)%InfilVdotCurDensity,  &
                                 'System','Average',Zone(Infiltration(Loop)%ZonePtr)%Name)
        CALL SetupOutputVariable('Zone Infiltration Standard Density Volume Flow Rate [m3/s]', &
                                  ZnAirRpt(Infiltration(Loop)%ZonePtr)%InfilVdotStdDensity,  &
                                 'System','Average',Zone(Infiltration(Loop)%ZonePtr)%Name)
        CALL SetupOutputVariable('Zone Infiltration Current Density Volume [m3]',  &
                                  ZnAirRpt(Infiltration(Loop)%ZonePtr)%InfilVolumeCurDensity,  &
                                 'System','Sum',Zone(Infiltration(Loop)%ZonePtr)%Name)
        CALL SetupOutputVariable('Zone Infiltration Standard Density Volume [m3]', &
                                  ZnAirRpt(Infiltration(Loop)%ZonePtr)%InfilVolumeStdDensity,  &
                                 'System','Sum',Zone(Infiltration(Loop)%ZonePtr)%Name)
        CALL SetupOutputVariable('Zone Infiltration Mass [kg]',ZnAirRpt(Infiltration(Loop)%ZonePtr)%InfilMass,  &
                                 'System','Sum',Zone(Infiltration(Loop)%ZonePtr)%Name)
        CALL SetupOutputVariable('Zone Infiltration Air Change Rate [ach]',  &
                                  ZnAirRpt(Infiltration(Loop)%ZonePtr)%InfilAirChangeRate,  &
                                 'System','Average',Zone(Infiltration(Loop)%ZonePtr)%Name)
      ENDIF
    ENDIF

    IF (AnyEnergyManagementSystemInModel) THEN
      CALL SetupEMSActuator('Zone Infiltration', Infiltration(Loop)%Name, 'Air Exchange Flow Rate', '[m3/s]', &
                Infiltration(Loop)%EMSOverrideOn  , Infiltration(Loop)%EMSAirFlowRateValue )
    ENDIF

  ENDDO
          ! VENTILATION Section: The following section is responsible for obtaining the simple ventilation
          ! from the user's input file.
  RepVarSet = .TRUE.

  cCurrentModuleObject='ZoneVentilation:DesignFlowRate'
  NumVentilationStatements=GetNumObjectsFound(cCurrentModuleObject)

  cCurrentModuleObject='ZoneVentilation:WindandStackOpenArea'
  TotWindAndStackVentilation=GetNumObjectsFound(cCurrentModuleObject)

  ALLOCATE(VentilationObjects(NumVentilationStatements))

  TotDesignFlowVentilation=0
  ErrFlag=.false.
  cCurrentModuleObject='ZoneVentilation:DesignFlowRate'
  DO Item=1,NumVentilationStatements
    CALL GetObjectItem(cCurrentModuleObject,Item,cAlphaArgs,NumAlpha,rNumericArgs,NumNumber,IOStat, &
                       NumBlank=lNumericFieldBlanks,AlphaBlank=lAlphaFieldBlanks, &
                       AlphaFieldNames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
    IsNotOK = .FALSE.
    IsBlank = .FALSE.
    CALL VerifyName(cAlphaArgs(1),VentilationObjects%Name,Item-1,IsNotOK,IsBlank,TRIM(cCurrentModuleObject)//' Name')
    IF (IsNotOK) THEN
      ErrorsFound = .TRUE.
      ErrFlag=.true.
      IF (IsBlank) cAlphaArgs(1) = 'xxxxx'
    END IF
    VentilationObjects(Item)%Name = cAlphaArgs(1)

    Item1=FindItemInList(cAlphaArgs(2),Zone%Name,NumOfZones)
    ZLItem=0
    IF (Item1 == 0 .and. NumOfZoneLists > 0) &
        ZLItem=FindItemInList(cAlphaArgs(2),ZoneList%Name,NumOfZoneLists)
    IF (Item1 > 0) THEN
      VentilationObjects(Item)%StartPtr=TotDesignFlowVentilation+1
      TotDesignFlowVentilation=TotDesignFlowVentilation+1
      VentilationObjects(Item)%NumOfZones=1
      VentilationObjects(Item)%ZoneListActive=.false.
      VentilationObjects(Item)%ZoneOrZoneListPtr=Item1
    ELSEIF (ZLItem > 0) THEN
      VentilationObjects(Item)%StartPtr=TotDesignFlowVentilation+1
      TotDesignFlowVentilation=TotDesignFlowVentilation+ZoneList(ZLItem)%NumOfZones
      VentilationObjects(Item)%NumOfZones=ZoneList(ZLItem)%NumOfZones
      VentilationObjects(Item)%ZoneListActive=.true.
      VentilationObjects(Item)%ZoneOrZoneListPtr=ZLItem
    ELSE
      CALL ShowSevereError(trim(cCurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid '//  &
           trim(cAlphaFieldNames(2))//'="'//trim(cAlphaArgs(2))//'" not found.')
      ErrorsFound=.true.
      ErrFlag=.true.
    ENDIF
  ENDDO

  IF (ErrFlag) THEN
    CALL ShowSevereError(RoutineName//'Errors with invalid names in '//trim(cCurrentModuleObject)//  &
       ' objects.')
    CALL ShowContinueError('...These will not be read in.  Other errors may occur.')
    TotDesignFlowVentilation=0
  ENDIF

  TotVentilation = TotDesignFlowVentilation + TotWindAndStackVentilation
  ALLOCATE(Ventilation(TotVentilation))

  IF (TotDesignFlowVentilation > 0) THEN
    Loop=0
    cCurrentModuleObject='ZoneVentilation:DesignFlowRate'
    DO Item = 1, NumVentilationStatements

      CALL GetObjectItem(cCurrentModuleObject,Item,cAlphaArgs,NumAlpha,rNumericArgs,NumNumber,IOStat, &
                       NumBlank=lNumericFieldBlanks,AlphaBlank=lAlphaFieldBlanks, &
                       AlphaFieldNames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

      DO Item1=1,VentilationObjects(Item)%NumOfZones
        Loop=Loop+1
        IF (.not. VentilationObjects(Item)%ZoneListActive) THEN
          Ventilation(Loop)%Name = cAlphaArgs(1)
          Ventilation(Loop)%ZonePtr = VentilationObjects(Item)%ZoneOrZoneListPtr
        ELSE
          CALL CheckCreatedZoneItemName(RoutineName,cCurrentModuleObject,  &
                                        Zone(ZoneList(VentilationObjects(Item)%ZoneOrZoneListPtr)%Zone(Item1))%Name,  &
                                        ZoneList(VentilationObjects(Item)%ZoneOrZoneListPtr)%MaxZoneNameLength,  &
                                        VentilationObjects(Item)%Name,     &
                                        Ventilation%Name,           &
                                        Loop-1,                       &
                                        Ventilation(Loop)%Name,            &
                                        ErrFlag)
          Ventilation(Loop)%ZonePtr = ZoneList(VentilationObjects(Item)%ZoneOrZoneListPtr)%Zone(Item1)
          IF (ErrFlag) ErrorsFound=.true.
        ENDIF

        ! setup a flag if the outdoor air balance method is applied
        IF (Ventilation(Loop)%ZonePtr > 0 .AND. TotZoneAirBalance > 0) Then
          DO I=1,TotZoneAirBalance
            IF (Ventilation(Loop)%ZonePtr == ZoneAirBalance(i)%ZonePtr) THEN
              IF (ZoneAirBalance(i)%BalanceMethod == AirBalanceQuadrature) THEN
                Ventilation(Loop)%QuadratureSum = .TRUE.
                Ventilation(Loop)%OABalancePtr = I
                EXIT
              END IF
            END IF
          END DO
        End If

        Ventilation(Loop)%ModelType = VentilationDesignFlowRate
        Ventilation(Loop)%SchedPtr  = GetScheduleIndex(cAlphaArgs(3))
        IF (Ventilation(Loop)%SchedPtr == 0) THEN
          IF (Item1 == 1) THEN
            IF (lAlphaFieldBlanks(3)) THEN
              CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'",'//   &
                   TRIM(cAlphaFieldNames(3))//' is required but field is blank.')
            ELSE
              CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'",'//   &
                   ' invalid (not found) '//TRIM(cAlphaFieldNames(3))//'="'//trim(cAlphaArgs(3))//'".')
            ENDIF
          END IF
          ErrorsFound = .TRUE.
        ENDIF

        ! Ventilation equipment design level calculation method
        SELECT CASE (cAlphaArgs(4))
          CASE('FLOW','FLOW/ZONE')
            Ventilation(Loop)%DesignLevel = rNumericArgs(1)
            IF (lNumericFieldBlanks(1)) THEN
              CALL ShowWarningError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(Ventilation(Loop)%Name)//  &
                '", '//trim(cAlphaFieldNames(4))//' specifies '//TRIM(cNumericFieldNames(1))//  &
                ', but that field is blank.  0 Ventilation will result.')
            ENDIF

          CASE('FLOW/AREA')
            IF (Ventilation(Loop)%ZonePtr /= 0) THEN
              IF (rNumericArgs(2) >= 0.0d0) THEN
                Ventilation(Loop)%DesignLevel=rNumericArgs(2)*Zone(Ventilation(Loop)%ZonePtr)%FloorArea
                IF (Zone(Ventilation(Loop)%ZonePtr)%FloorArea <= 0.0d0) THEN
                  CALL ShowWarningError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(Ventilation(Loop)%Name)//  &
                    '", '//trim(cAlphaFieldNames(4))//' specifies '//TRIM(cNumericFieldNames(2))//  &
                    ', but Zone Floor Area = 0.  0 Ventilation will result.')
                ENDIF
              ELSE
                CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(Ventilation(Loop)%Name)//  &
                                 '", invalid flow/area specification [<0.0]='//   &
                                 TRIM(RoundSigDigits(rNumericArgs(2),3)))
                ErrorsFound=.TRUE.
              ENDIF
            ENDIF
            IF (lNumericFieldBlanks(2)) THEN
              CALL ShowWarningError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(Ventilation(Loop)%Name)//  &
                '", '//trim(cAlphaFieldNames(4))//' specifies '//TRIM(cNumericFieldNames(2))//  &
                ', but that field is blank.  0 Ventilation will result.')
            ENDIF

          CASE('FLOW/PERSON')
            IF (Ventilation(Loop)%ZonePtr /= 0) THEN
              IF (rNumericArgs(3) >= 0.0d0) THEN
                Ventilation(Loop)%DesignLevel=rNumericArgs(3)*Zone(Ventilation(Loop)%ZonePtr)%TotOccupants
                IF (Zone(Ventilation(Loop)%ZonePtr)%TotOccupants <= 0.0d0) THEN
                  CALL ShowWarningError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(Ventilation(Loop)%Name)//  &
                    '", '//trim(cAlphaFieldNames(4))//' specifies '//TRIM(cNumericFieldNames(3))//  &
                    ', but Zone Total Occupants = 0.  0 Ventilation will result.')
                ENDIF
              ELSE
                CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(Ventilation(Loop)%Name)//  &
                                 '", invalid flow/person specification [<0.0]='//   &
                                 TRIM(RoundSigDigits(rNumericArgs(3),3)))
                ErrorsFound=.TRUE.
              ENDIF
            ENDIF
            IF (lNumericFieldBlanks(3)) THEN
              CALL ShowWarningError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(Ventilation(Loop)%Name)//  &
                '", '//trim(cAlphaFieldNames(4))//'specifies '//TRIM(cNumericFieldNames(3))//  &
                ', but that field is blank.  0 Ventilation will result.')
            ENDIF

          CASE('AIRCHANGES/HOUR')
            IF (Ventilation(Loop)%ZonePtr /= 0) THEN
              IF (rNumericArgs(4) >= 0.0d0) THEN
                Ventilation(Loop)%DesignLevel=rNumericArgs(4)*Zone(Ventilation(Loop)%ZonePtr)%Volume/SecInHour
                IF (Zone(Ventilation(Loop)%ZonePtr)%Volume <= 0.0d0) THEN
                  CALL ShowWarningError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(Ventilation(Loop)%Name)//  &
                    '", '//trim(cAlphaFieldNames(4))//' specifies '//TRIM(cNumericFieldNames(4))//  &
                    ', but Zone Volume = 0.  0 Ventilation will result.')
                ENDIF
              ELSE
                CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(Ventilation(Loop)%Name)//  &
                                 '", invalid ACH (air changes per hour) specification [<0.0]='//   &
                                 TRIM(RoundSigDigits(rNumericArgs(5),3)))
                ErrorsFound=.TRUE.
              ENDIF
            ENDIF
            IF (lNumericFieldBlanks(4)) THEN
              CALL ShowWarningError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(Ventilation(Loop)%Name)//  &
                '", '//trim(cAlphaFieldNames(4))//' specifies '//TRIM(cNumericFieldNames(4))//  &
                ', but that field is blank.  0 Ventilation will result.')
            ENDIF

          CASE DEFAULT
            IF (Item1 == 1) THEN
              CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
                                   '", invalid calculation method='//   &
                                   TRIM(cAlphaArgs(4)))
              ErrorsFound=.TRUE.
            ENDIF
        END SELECT

        SELECT CASE (cAlphaArgs(5))  ! Fan type character input-->convert to integer
          CASE ('EXHAUST')
            Ventilation(Loop)%FanType = ExhaustVentilation
          CASE ('INTAKE')
            Ventilation(Loop)%FanType = IntakeVentilation
          CASE ('NATURAL','NONE',Blank)
            Ventilation(Loop)%FanType = NaturalVentilation
          CASE ('BALANCED')
            Ventilation(Loop)%FanType = BalancedVentilation
          CASE DEFAULT
            IF (Item1 == 1) THEN
              CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(Ventilation(Loop)%Name)//  &
                  '". invalid '//trim(cAlphaFieldNames(5))//'="'//TRIM(cAlphaArgs(5))//'".')
              ErrorsFound = .TRUE.
            ENDIF
        END SELECT

        Ventilation(Loop)%FanPressure   = rNumericArgs(5)
        IF (Ventilation(Loop)%FanPressure < 0.0d0) THEN
          IF (Item1 == 1) THEN
            CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(Ventilation(Loop)%Name)//  &
            '", '//trim(cNumericFieldNames(5))//' must be >=0')
            ErrorsFound = .TRUE.
          ENDIF
        END IF

        Ventilation(Loop)%FanEfficiency = rNumericArgs(6)
        IF ((Ventilation(Loop)%FanEfficiency <= 0.0d0) .OR. &
            (Ventilation(Loop)%FanEfficiency >  1.0d0)) THEN
          IF (Item1 == 1) THEN
            CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(Ventilation(Loop)%Name)//  &
               '",'//trim(cNumericFieldNames(6))//' must be in range >0 and <= 1')
            ErrorsFound = .TRUE.
          ENDIF
        END IF

              ! Override any user input for cases where natural ventilation is being used
        IF (Ventilation(Loop)%FanType == NaturalVentilation) THEN
          Ventilation(Loop)%FanPressure   = 0.0d0
          Ventilation(Loop)%FanEfficiency = 1.0d0
        END IF

        IF (.not. lNumericFieldBlanks(7)) THEN
          Ventilation(Loop)%ConstantTermCoef    = rNumericArgs(7)
        ELSE
          Ventilation(Loop)%ConstantTermCoef    = 1.0d0
        ENDIF
        IF (.not. lNumericFieldBlanks(8)) THEN
          Ventilation(Loop)%TemperatureTermCoef = rNumericArgs(8)
        ELSE
          Ventilation(Loop)%TemperatureTermCoef = 0.0d0
        ENDIF
        IF (.not. lNumericFieldBlanks(9)) THEN
          Ventilation(Loop)%VelocityTermCoef    = rNumericArgs(9)
        ELSE
          Ventilation(Loop)%VelocityTermCoef    = 0.0d0
        ENDIF
        IF (.not. lNumericFieldBlanks(10)) THEN
          Ventilation(Loop)%VelocitySQTermCoef  = rNumericArgs(10)
        ELSE
          Ventilation(Loop)%VelocitySQTermCoef  = 0.0d0
        ENDIF

        IF (Ventilation(Loop)%ConstantTermCoef == 0.0d0 .and. Ventilation(Loop)%TemperatureTermCoef == 0.0d0 .and.  &
            Ventilation(Loop)%VelocityTermCoef == 0.0d0 .and. Ventilation(Loop)%VelocitySQTermCoef == 0.0d0)  THEN
            IF (Item1 == 1) THEN
              CALL ShowWarningError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'", in '// &
                                      TRIM(cAlphaFieldNames(2))//'="'//TRIM(cAlphaArgs(2))//'".')
              CALL ShowContinueError('Ventilation Coefficients are all zero.  No Ventilation will be reported.')
            ENDIF
        ENDIF

        IF (.not. lNumericFieldBlanks(11)) THEN
          Ventilation(Loop)%MinIndoorTemperature = rNumericArgs(11)
        ELSE
          Ventilation(Loop)%MinIndoorTemperature = -VentilTempLimit
        ENDIF
    !    Ventilation(Loop)%MinIndoorTemperature = rNumericArgs(11)
        IF ((Ventilation(Loop)%MinIndoorTemperature < -VentilTempLimit) .OR. &
            (Ventilation(Loop)%MinIndoorTemperature >  VentilTempLimit))  THEN
          IF (Item1 == 1) THEN
            CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1)) // &
                                  '" must have '//trim(cNumericFieldNames(11))//' between -100C and 100C.')
            CALL ShowContinueError('...value entered=['//trim(RoundSigDigits(rNumericArgs(11),2))//'].')
            ErrorsFound = .TRUE.
          ENDIF
        END IF

        Ventilation(Loop)%MinIndoorTempSchedPtr  = GetScheduleIndex(cAlphaArgs(6))
        IF (Ventilation(Loop)%MinIndoorTempSchedPtr > 0) THEN
          IF (Item1 == 1) THEN
            IF (.NOT. lNumericFieldBlanks(11)) &
              CALL ShowWarningError(RoutineName//'The Minimum Indoor Temperature value and schedule are provided.' &
                //' The scheduled temperature will be used in the '//TRIM(cCurrentModuleObject)//' object = '//TRIM(cAlphaArgs(1)))
            ! Check min and max values in the schedule to ensure both values are within the range
            IF (.NOT.CheckScheduleValueMinMax(Ventilation(Loop)%MinIndoorTempSchedPtr,'>=',-VentilTempLimit,  &
               '<=',VentilTempLimit)) THEN
              CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//' statement = '//TRIM(cAlphaArgs(1)) // &
                      ' must have a minimum indoor temperature between -100C and 100C defined in the schedule = '//  &
                         TRIM(cAlphaArgs(6)))
              ErrorsFound = .TRUE.
            END IF
          ENDIF
        END IF
        IF (Ventilation(Loop)%MinIndoorTempSchedPtr == 0 .AND. lNumericFieldBlanks(11).AND. (.NOT. lAlphaFieldBlanks(6))) THEN
          IF (Item1 == 1) THEN
            CALL ShowWarningError(RoutineName//trim(cNumericFieldNames(11))//': '//  &
               'the value field is blank and schedule field is invalid. The default '// &
               'value will be used ('//TRIM(RoundSigDigits(-VentilTempLimit,1))//') ')
            CALL ShowContinueError('in the '//TRIM(cCurrentModuleObject)//' object = '//TRIM(cAlphaArgs(1))// &
                                   ' and the simulation continues...')
          END IF
        ENDIF
        ! Check Minimum indoor temperature value and schedule fields
        IF (.NOT. lNumericFieldBlanks(11) .AND. (cAlphaArgs(6) .NE. Blank .AND. Ventilation(Loop)%MinIndoorTempSchedPtr == 0)) THEN
          IF (Item1 == 1) THEN
            CALL ShowWarningError(RoutineName//trim(cAlphaFieldNames(6))//' = '//TRIM(cAlphaArgs(6))//  &
                         ' is invalid. The constant value will be used at '//TRIM(RoundSigDigits(rNumericArgs(11),1))//  &
                         ' degrees C ')
            CALL ShowContinueError('in the '//TRIM(cCurrentModuleObject)//' object = '//TRIM(cAlphaArgs(1))// &
                                   ' and the simulation continues...')
          ENDIF
        END IF

        IF (.not. lNumericFieldBlanks(12)) THEN
          Ventilation(Loop)%MaxIndoorTemperature = rNumericArgs(12)
        ELSE
          Ventilation(Loop)%MaxIndoorTemperature = VentilTempLimit
        ENDIF
        IF ((Ventilation(Loop)%MaxIndoorTemperature < -VentilTempLimit) .OR. &
            (Ventilation(Loop)%MaxIndoorTemperature >  VentilTempLimit))  THEN
          IF (Item1 == 1) THEN
            CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1)) // &
                                  ' must have a maximum indoor temperature between -100C and 100C')
            ErrorsFound = .TRUE.
          ENDIF
        END IF

        Ventilation(Loop)%MaxIndoorTempSchedPtr  = GetScheduleIndex(cAlphaArgs(7))
        IF (Ventilation(Loop)%MaxIndoorTempSchedPtr > 0) THEN
          IF (Item1 == 1) THEN
            IF (.NOT. lNumericFieldBlanks(12)) &
              CALL ShowWarningError(RoutineName//'The Maximum Indoor Temperature value and schedule are provided. '//  &
               'The scheduled temperature will be used in the '//TRIM(cCurrentModuleObject)//' object = '//TRIM(cAlphaArgs(1)))
            ! Check min and max values in the schedule to ensure both values are within the range
            IF (.NOT.CheckScheduleValueMinMax(Ventilation(Loop)%MaxIndoorTempSchedPtr,'>=',-VentilTempLimit,  &
                                                 '<=',VentilTempLimit)) THEN
              CALL ShowSevereError(TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1)) // &
               ' must have a maximum indoor temperature between -100C and 100C defined in the schedule = '//TRIM(cAlphaArgs(7)))
              ErrorsFound = .TRUE.
            END IF
          ENDIF
        END IF
        IF (Ventilation(Loop)%MaxIndoorTempSchedPtr == 0 .AND. lNumericFieldBlanks(12) .AND. (.NOT. lAlphaFieldBlanks(7))) THEN
          IF (Item1 == 1) THEN
            CALL ShowWarningError(RoutineName//trim(cNumericFieldNames(12))//': '//  &
               'the value field is blank and schedule field is invalid. The default '// &
               'value will be used ('//TRIM(RoundSigDigits(VentilTempLimit,1))//') ')
            CALL ShowContinueError('in the '//TRIM(cCurrentModuleObject)//' object = '//TRIM(cAlphaArgs(1))// &
                                   ' and the simulation continues...')
          ENDIF
        END IF
        ! Check Maximum indoor temperature value and schedule fields
        IF (.NOT. lNumericFieldBlanks(12) .AND. ((.NOT. lAlphaFieldBlanks(7))   &
            .AND. Ventilation(Loop)%MaxIndoorTempSchedPtr==0)) THEN
          IF (Item1 == 1) THEN
            CALL ShowWarningError(RoutineName//trim(cAlphaFieldNames(7))//' = '//  &
               TRIM(cAlphaArgs(7))//' is invalid. ' &
               //'The constant value will be used at '//TRIM(RoundSigDigits(rNumericArgs(12),1))//' degrees C ')
            CALL ShowContinueError('in the '//TRIM(cCurrentModuleObject)//' object = '//TRIM(cAlphaArgs(1))// &
                                   ' and the simulation continues...')
          ENDIF
        END IF

        IF (.not. lNumericFieldBlanks(13)) THEN
          Ventilation(Loop)%DelTemperature = rNumericArgs(13)
        ELSE
          Ventilation(Loop)%DelTemperature = -VentilTempLimit
        ENDIF
    !    Ventilation(Loop)%DelTemperature = rNumericArgs(13)  !  3/12/03  Negative del temp now allowed COP

        Ventilation(Loop)%DeltaTempSchedPtr  = GetScheduleIndex(cAlphaArgs(8))
        IF (Ventilation(Loop)%DeltaTempSchedPtr > 0) THEN
          IF (Item1 == 1) THEN
            IF (.NOT. lNumericFieldBlanks(13)) &
              CALL ShowWarningError(RoutineName//'The Delta Temperature value and schedule are provided. '//  &
                 'The scheduled temperature will be used in the '//TRIM(cCurrentModuleObject)//' object = '//TRIM(cAlphaArgs(1)))
              ! Check min value in the schedule to ensure both values are within the range
            IF (GetScheduleMinValue(Ventilation(Loop)%DeltaTempSchedPtr) < -VentilTempLimit) THEN
              CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//' statement = '//TRIM(cAlphaArgs(1)) // &
               ' must have a delta temperature equal to or above -100C defined in the schedule = '//TRIM(cAlphaArgs(8)))
              ErrorsFound = .TRUE.
            END IF
          ENDIF
        END IF
        IF (Ventilation(Loop)%DeltaTempSchedPtr == 0 .AND. lNumericFieldBlanks(13) .AND. (.NOT. lAlphaFieldBlanks(8))) THEN
          IF (Item1 == 1) THEN
            CALL ShowWarningError(RoutineName//trim(cNumericFieldNames(13))//  &
               ': the value field is blank and schedule field is invalid. ' &
                //'The default value will be used ('//TRIM(RoundSigDigits(VentilTempLimit,1))//') ')
            CALL ShowContinueError('in the '//TRIM(cCurrentModuleObject)//' object = '//TRIM(cAlphaArgs(1))//&
                                   ' and the simulation continues...')
          END IF
        ENDIF

        ! Check delta temperature value and schedule fields
    !    IF (lNumericFieldBlanks(13) .AND. cAlphaArgs(8) .EQ. Blank) THEN
    !      CALL ShowWarningError(RoutineName//'Both the delta temperature value and delta schedule are blank. ')
    !      CALL ShowContinueError('Will set the temperature to a constant value of '//TRIM(RoundSigDigits(-VentilTempLimit,1)) &
    !           //' degrees C ')
    !      CALL ShowContinueError('in the Ventilation object = '//TRIM(cAlphaArgs(1))//' and the simulation continues...')
    !    END IF
        IF (.NOT. lNumericFieldBlanks(13) .AND. ((.NOT. lAlphaFieldBlanks(8)) .AND. Ventilation(Loop)%DeltaTempSchedPtr == 0)) THEN
          IF (Item1 == 1) THEN
            CALL ShowWarningError(RoutineName//trim(cAlphaFieldNames(8))//' = '//TRIM(cAlphaArgs(8))//' is invalid. ' &
               //'The constant value will be used at '//TRIM(RoundSigDigits(rNumericArgs(13),1))//' degrees C ')
            CALL ShowContinueError('in the '//TRIM(cCurrentModuleObject)//' object = '//TRIM(cAlphaArgs(1))// &
                                   ' and the simulation continues...')
          END IF
        ENDIF

        IF (.not. lNumericFieldBlanks(14)) THEN
          Ventilation(Loop)%MinOutdoorTemperature = rNumericArgs(14)
        ELSE
          Ventilation(Loop)%MinOutdoorTemperature = -VentilTempLimit
        ENDIF
        IF ((Ventilation(Loop)%MinOutdoorTemperature < -VentilTempLimit) .OR. &
            (Ventilation(Loop)%MinOutdoorTemperature >  VentilTempLimit))  THEN
          IF (Item1 == 1) THEN
            CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//' statement = '//TRIM(cAlphaArgs(1)) // &
                                  ' must have '//trim(cNumericFieldNames(14))//' between -100C and 100C')
            ErrorsFound = .TRUE.
          ENDIF
        END IF

        Ventilation(Loop)%MinOutdoorTempSchedPtr  = GetScheduleIndex(cAlphaArgs(9))
        IF (Item1 == 1) THEN
          IF (Ventilation(Loop)%MinOutdoorTempSchedPtr > 0) THEN
            IF (.not. lNumericFieldBlanks(14)) &
              CALL ShowWarningError(RoutineName//'The Minimum Outdoor Temperature value and schedule are provided. '//  &
                 'The scheduled temperature will be used in the '//TRIM(cCurrentModuleObject)//' object = '//TRIM(cAlphaArgs(1)))
            ! Check min and max values in the schedule to ensure both values are within the range
            IF (.NOT.CheckScheduleValueMinMax(Ventilation(Loop)%MinOutdoorTempSchedPtr,  &
                                          '>=',-VentilTempLimit,'<=',VentilTempLimit)) THEN
              CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//' statement = '//TRIM(cAlphaArgs(1)) // &
               ' must have a minimum outdoor temperature between -100C and 100C defined in the schedule = '//TRIM(cAlphaArgs(9)))
              ErrorsFound = .TRUE.
            END IF
          END IF
          IF (Ventilation(Loop)%MinOutdoorTempSchedPtr == 0 .AND. lNumericFieldBlanks(14).AND. (.NOT. lAlphaFieldBlanks(9))) THEN
            CALL ShowWarningError(RoutineName//'Minimum Outdoor Temperature: '//  &
               'the value field is blank and schedule field is invalid. The default ' &
                //'value will be used ('//TRIM(RoundSigDigits(-VentilTempLimit,1))//') ')
            CALL ShowContinueError('in the '//TRIM(cCurrentModuleObject)//' object = '//TRIM(cAlphaArgs(1))// &
                                   ' and the simulation continues...')
          END IF
          ! Check Minimum outdoor temperature value and schedule fields
          IF (.NOT. lNumericFieldBlanks(14) .AND.   &
             ((.NOT. lAlphaFieldBlanks(9)) .AND. Ventilation(Loop)%MinOutdoorTempSchedPtr==0)) THEN
            CALL ShowWarningError(RoutineName//trim(cAlphaFieldNames(9))//' = '//TRIM(cAlphaArgs(9))//  &
               ' is invalid. The constant value will be used at '//TRIM(RoundSigDigits(rNumericArgs(14),1))//' degrees C ')
            CALL ShowContinueError('in the '//TRIM(cCurrentModuleObject)//' object = '//TRIM(cAlphaArgs(1))// &
                                   ' and the simulation continues...')
          END IF
        ENDIF

        IF (.not. lNumericFieldBlanks(15)) THEN
          Ventilation(Loop)%MaxOutdoorTemperature = rNumericArgs(15)
        ELSE
          Ventilation(Loop)%MaxOutdoorTemperature = VentilTempLimit
        ENDIF
        IF (Item1 == 1) THEN
          IF ((Ventilation(Loop)%MaxOutdoorTemperature < -VentilTempLimit) .OR. &
              (Ventilation(Loop)%MaxOutdoorTemperature >  VentilTempLimit))  THEN
            CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//' statement = '//TRIM(cAlphaArgs(1)) // &
                                  ' must have a '//TRIM(cNumericFieldNames(15))//' between -100C and 100C')
            ErrorsFound = .TRUE.
          END IF
        ENDIF

        Ventilation(Loop)%MaxOutdoorTempSchedPtr  = GetScheduleIndex(cAlphaArgs(10))
        IF (Item1 == 1) THEN
          IF (Ventilation(Loop)%MaxOutdoorTempSchedPtr > 0) THEN
            IF (.not. lNumericFieldBlanks(15)) &
            CALL ShowWarningError(RoutineName//'The Maximum Outdoor Temperature value and schedule are provided. '//  &
                'The scheduled temperature will be used in the '//TRIM(cCurrentModuleObject)//' object = '//TRIM(cAlphaArgs(1)))
            IF (.NOT.CheckScheduleValueMinMax(Ventilation(Loop)%MaxOutdoorTempSchedPtr,  &
                                                '>=',-VentilTempLimit,'<=',VentilTempLimit)) THEN
              CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//' statement = '//TRIM(cAlphaArgs(1)) // &
               ' must have a maximum outdoor temperature between -100C and 100C defined in the schedule = '//TRIM(cAlphaArgs(10)))
              ErrorsFound = .TRUE.
            END IF
          END IF
          IF (Ventilation(Loop)%MaxOutdoorTempSchedPtr == 0 .AND.   &
              lNumericFieldBlanks(15).AND. (.NOT. lAlphaFieldBlanks(10))) THEN
            CALL ShowWarningError(RoutineName//trim(cNumericFieldNames(15))//': '//  &
               'the value field is blank and schedule field is invalid.'//  &
               ' The default value will be used ('//TRIM(RoundSigDigits(VentilTempLimit,1))//') ')
            CALL ShowContinueError('in the '//TRIM(cCurrentModuleObject)//' object = '//TRIM(cAlphaArgs(1))// &
                                   ' and the simulation continues...')
          END IF
          ! Check Maximum outdoor temperature value and schedule fields
          IF (.NOT. lNumericFieldBlanks(15) .AND.   &
             ((.NOT. lAlphaFieldBlanks(10)) .AND. Ventilation(Loop)%MaxOutdoorTempSchedPtr==0)) THEN
            CALL ShowWarningError(RoutineName//trim(cAlphaFieldNames(10))//' = '//TRIM(cAlphaArgs(10))//  &
               'is invalid. The constant value will be used at '//TRIM(RoundSigDigits(rNumericArgs(15),1))//' degrees C ')
            CALL ShowContinueError('in the '//TRIM(cCurrentModuleObject)//' object = '//TRIM(cAlphaArgs(1))// &
                                   ' and the simulation continues...')
          END IF
        ENDIF

        IF (.not. lNumericFieldBlanks(16)) THEN
          Ventilation(Loop)%MaxWindSpeed = rNumericArgs(16)
        ELSE
          Ventilation(Loop)%MaxWindSpeed = VentilWSLimit
        ENDIF
        IF (Item1 == 1) THEN
          IF ((Ventilation(Loop)%MaxWindSpeed < -VentilWSLimit) .OR. &
              (Ventilation(Loop)%MaxWindSpeed >  VentilWSLimit))  THEN
            CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//' statement = '//TRIM(cAlphaArgs(1)) // &
                                  ' must have a maximum wind speed between -40 m/s and 40 m/s')
            ErrorsFound = .TRUE.
          END IF
        ENDIF

        ! Report variables should be added for individual VENTILATION objects, in addition to zone totals below

        IF (Ventilation(Loop)%ZonePtr > 0) THEN
          IF (RepVarSet(Ventilation(Loop)%ZonePtr) .AND. .NOT. Ventilation(Loop)%QuadratureSum) THEN
            RepVarSet(Ventilation(Loop)%ZonePtr)=.FALSE.
            CALL SetupOutputVariable('Zone Ventilation Sensible Heat Loss Energy [J]',  &
               ZnAirRpt(Ventilation(Loop)%ZonePtr)%VentilHeatLoss,  &
                                     'System','Sum',Zone(Ventilation(Loop)%ZonePtr)%Name)
            CALL SetupOutputVariable('Zone Ventilation Sensible Heat Gain Energy [J]',  &
               ZnAirRpt(Ventilation(Loop)%ZonePtr)%VentilHeatGain,  &
                                     'System','Sum',Zone(Ventilation(Loop)%ZonePtr)%Name)
            CALL SetupOutputVariable('Zone Ventilation Latent Heat Loss Energy [J]',  &
               ZnAirRpt(Ventilation(Loop)%ZonePtr)%VentilLatentLoss,  &
                                     'System','Sum',Zone(Ventilation(Loop)%ZonePtr)%Name)
            CALL SetupOutputVariable('Zone Ventilation Latent Heat Gain Energy [J]',  &
               ZnAirRpt(Ventilation(Loop)%ZonePtr)%VentilLatentGain,  &
                                     'System','Sum',Zone(Ventilation(Loop)%ZonePtr)%Name)
            CALL SetupOutputVariable('Zone Ventilation Total Heat Loss Energy [J]', &
                                      ZnAirRpt(Ventilation(Loop)%ZonePtr)%VentilTotalLoss,  &
                                     'System','Sum',Zone(Ventilation(Loop)%ZonePtr)%Name)
            CALL SetupOutputVariable('Zone Ventilation Total Heat Gain Energy [J]', &
                                      ZnAirRpt(Ventilation(Loop)%ZonePtr)%VentilTotalGain,  &
                                     'System','Sum',Zone(Ventilation(Loop)%ZonePtr)%Name)
            CALL SetupOutputVariable('Zone Ventilation Current Density Volume Flow Rate [m3/s]',  &
                                      ZnAirRpt(Ventilation(Loop)%ZonePtr)%VentilVdotCurDensity,   &
                                     'System','Average',Zone(Ventilation(Loop)%ZonePtr)%Name)
            CALL SetupOutputVariable('Zone Ventilation Standard Density Volume Flow Rate [m3/s]', &
                                      ZnAirRpt(Ventilation(Loop)%ZonePtr)%VentilVdotStdDensity,   &
                                     'System','Average',Zone(Ventilation(Loop)%ZonePtr)%Name)
            CALL SetupOutputVariable('Zone Ventilation Current Density Volume [m3]',              &
                                      ZnAirRpt(Ventilation(Loop)%ZonePtr)%VentilVolumeCurDensity, &
                                     'System','Sum',Zone(Ventilation(Loop)%ZonePtr)%Name)
            CALL SetupOutputVariable('Zone Ventilation Standard Density Volume [m3]',             &
                                      ZnAirRpt(Ventilation(Loop)%ZonePtr)%VentilVolumeStdDensity, &
                                     'System','Sum',Zone(Ventilation(Loop)%ZonePtr)%Name)
            CALL SetupOutputVariable('Zone Ventilation Mass [kg]',ZnAirRpt(Ventilation(Loop)%ZonePtr)%VentilMass,  &
                                     'System','Sum',Zone(Ventilation(Loop)%ZonePtr)%Name)
            CALL SetupOutputVariable('Zone Ventilation Air Change Rate [ach]',                   &
                                      ZnAirRpt(Ventilation(Loop)%ZonePtr)%VentilAirChangeRate,   &
                                     'System','Average',Zone(Ventilation(Loop)%ZonePtr)%Name)
            CALL SetupOutputVariable('Zone Ventilation Fan Electric Energy [J]',     &
                                      ZnAirRpt(Ventilation(Loop)%ZonePtr)%VentilFanElec,  &
                                     'System','Sum',Zone(Ventilation(Loop)%ZonePtr)%Name, &
                                      ResourceTypeKey='Electricity',GroupKey='Building', &
                                      ZoneKey=Zone(Ventilation(Loop)%ZonePtr)%Name, &
                                      EndUseKey='Fans', EndUseSubKey='Ventilation (simple)')
            CALL SetupOutputVariable('Zone Ventilation Air Inlet Temperature [C]', &
                                     ZnAirRpt(Ventilation(Loop)%ZonePtr)%VentilAirTemp, &
                                     'System','Average',Zone(Ventilation(Loop)%ZonePtr)%Name)
          END IF
        END IF

        IF (AnyEnergyManagementSystemInModel) THEN
          CALL SetupEMSActuator('Zone Ventilation', Ventilation(Loop)%Name, 'Air Exchange Flow Rate', '[m3/s]', &
                    Ventilation(Loop)%EMSSimpleVentOn  , Ventilation(Loop)%EMSimpleVentFlowRate )
        ENDIF

      END DO
    END DO
  ENDIF

  cCurrentModuleObject='ZoneVentilation:WindandStackOpenArea'
  VentiCount = TotDesignFlowVentilation
  DO Loop=1,TotWindAndStackVentilation

    CALL GetObjectItem(cCurrentModuleObject,Loop,cAlphaArgs,NumAlpha,rNumericArgs,NumNumber,IOStat, &
                       NumBlank=lNumericFieldBlanks,AlphaBlank=lAlphaFieldBlanks, &
                       AlphaFieldNames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

    IsNotOK = .FALSE.
    IsBlank = .FALSE.
    VentiCount = TotDesignFlowVentilation + Loop
    CALL VerifyName(cAlphaArgs(1),Ventilation%Name,VentiCount-1,IsNotOK,IsBlank,TRIM(cCurrentModuleObject)//' Name')
    IF (IsNotOK) THEN
      ErrorsFound = .TRUE.
      IF (IsBlank) cAlphaArgs(1) = 'xxxxx'
    END IF
    Ventilation(VentiCount)%Name = cAlphaArgs(1)
    Ventilation(VentiCount)%ModelType = VentilationWindAndStack

    Ventilation(VentiCount)%ZonePtr = FindIteminList(cAlphaArgs(2),Zone%Name,NumOfZones)
    IF (Ventilation(VentiCount)%ZonePtr == 0) THEN
      CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))// &
           '", invalid (not found) '//trim(cAlphaFieldNames(2))//'="'//TRIM(cAlphaArgs(2))//'".')
      ErrorsFound = .TRUE.
    ENDIF

    ! setup a flag if the outdoor air balance method is applied
    IF (Ventilation(VentiCount)%ZonePtr > 0 .AND. TotZoneAirBalance > 0) Then
      DO I=1,TotZoneAirBalance
        IF (Ventilation(VentiCount)%ZonePtr == ZoneAirBalance(i)%ZonePtr) THEN
          IF (ZoneAirBalance(i)%BalanceMethod == AirBalanceQuadrature) THEN
            Ventilation(VentiCount)%QuadratureSum = .TRUE.
            Ventilation(VentiCount)%OABalancePtr = I
            EXIT
          END IF
        END IF
      END DO
    End If

    Ventilation(VentiCount)%OpenArea   = rNumericArgs(1)
    IF (Ventilation(VentiCount)%OpenArea < 0.0d0) THEN
      CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'", '//   &
                   trim(cNumericFieldNames(1))//' must be positive.')
      ErrorsFound = .TRUE.
    END IF

    Ventilation(VentiCount)%OpenAreaSchedPtr  = GetScheduleIndex(cAlphaArgs(3))
    IF (Ventilation(VentiCount)%OpenAreaSchedPtr == 0) THEN
      IF (lAlphaFieldBlanks(3)) THEN
        CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'", '//   &
             TRIM(cAlphaFieldNames(3))//' is required but field is blank.')
      ELSE
        CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'",'//   &
             ' invalid (not found) '//TRIM(cAlphaFieldNames(3))//'="'//trim(cAlphaArgs(3))//'".')
      END IF
      ErrorsFound = .TRUE.
    ENDIF

    Ventilation(VentiCount)%OpenEff   = rNumericArgs(2)
    IF (Ventilation(VentiCount)%OpenEff /= AutoCalculate .and. &
       (Ventilation(VentiCount)%OpenEff < 0.0d0 .OR. Ventilation(VentiCount)%OpenEff > 1)) THEN
      CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'", '//   &
                       trim(cNumericFieldNames(2))//' must be between 0 and 1.')
      ErrorsFound = .TRUE.
    END IF

    Ventilation(VentiCount)%EffAngle   = rNumericArgs(3)
    IF (Ventilation(VentiCount)%EffAngle < 0.0d0 .OR. Ventilation(VentiCount)%EffAngle >= 360) THEN
      CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'", '//   &
                       trim(cNumericFieldNames(3))//' must be between 0 and 360.')
      ErrorsFound = .TRUE.
    END IF

    Ventilation(VentiCount)%DH   = rNumericArgs(4)
    IF (Ventilation(VentiCount)%DH < 0.0d0) THEN
      CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'", '//   &
                       trim(cNumericFieldNames(4))//' must be positive.')
      ErrorsFound = .TRUE.
    END IF

    Ventilation(VentiCount)%DiscCoef   = rNumericArgs(5)
    IF (Ventilation(VentiCount)%DiscCoef /= AutoCalculate .and. &
       (Ventilation(VentiCount)%DiscCoef < 0.0d0 .OR. Ventilation(VentiCount)%DiscCoef > 1)) THEN
      CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'", '//   &
                       trim(cNumericFieldNames(5))//' must be between 0 and 1.')
      ErrorsFound = .TRUE.
    END IF

    IF (.not. lNumericFieldBlanks(6)) THEN
      Ventilation(VentiCount)%MinIndoorTemperature = rNumericArgs(6)
    ELSE
      Ventilation(VentiCount)%MinIndoorTemperature = -VentilTempLimit
    ENDIF
    IF ((Ventilation(VentiCount)%MinIndoorTemperature < -VentilTempLimit) .OR. &
        (Ventilation(VentiCount)%MinIndoorTemperature >  VentilTempLimit))  THEN
      CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//' statement = '//TRIM(cAlphaArgs(1)) // &
                            ' must have '//trim(cNumericFieldNames(6))//' between -100C and 100C')
      ErrorsFound = .TRUE.
    END IF

    Ventilation(VentiCount)%MinIndoorTempSchedPtr  = GetScheduleIndex(cAlphaArgs(4))
    IF (Ventilation(VentiCount)%MinIndoorTempSchedPtr > 0) THEN
      IF (.NOT. lNumericFieldBlanks(6)) &
      CALL ShowWarningError(RoutineName//'The Minimum Indoor Temperature value and schedule are provided.' &
          //' The scheduled temperature will be used in the '//TRIM(cCurrentModuleObject)//' object = '//TRIM(cAlphaArgs(1)))
      ! Check min and max values in the schedule to ensure both values are within the range
      IF (.NOT.CheckScheduleValueMinMax(Ventilation(VentiCount)%MinIndoorTempSchedPtr,'>=', &
          -VentilTempLimit,'<=',VentilTempLimit)) THEN
        CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//' statement = '//TRIM(cAlphaArgs(1)) // &
                ' must have a minimum indoor temperature between -100C and 100C defined in the schedule = '//TRIM(cAlphaArgs(4)))
        ErrorsFound = .TRUE.
      END IF
    END IF
    IF (Ventilation(VentiCount)%MinIndoorTempSchedPtr == 0 .AND. lNumericFieldBlanks(6).AND. (.NOT. lAlphaFieldBlanks(4))) THEN
      CALL ShowWarningError(RoutineName//trim(cNumericFieldNames(6))//': '//  &
         'the value field is blank and schedule field is invalid. The default '// &
         'value will be used ('//TRIM(RoundSigDigits(-VentilTempLimit,1))//') ')
      CALL ShowContinueError('in the '//TRIM(cCurrentModuleObject)//' object = '//TRIM(cAlphaArgs(1))// &
                             ' and the simulation continues...')
    END IF
    ! Check Minimum indoor temperature value and schedule fields
    IF (.NOT. lNumericFieldBlanks(6) .AND. (cAlphaArgs(4).NE.Blank.AND.Ventilation(VentiCount)%MinIndoorTempSchedPtr == 0)) THEN
      CALL ShowWarningError(RoutineName//trim(cAlphaFieldNames(4))//' = '//TRIM(cAlphaArgs(4))//  &
                   ' is invalid. The constant value will be used at '//TRIM(RoundSigDigits(rNumericArgs(11),1))//' degrees C ')
      CALL ShowContinueError('in the '//TRIM(cCurrentModuleObject)//' object = '//TRIM(cAlphaArgs(1))// &
                             ' and the simulation continues...')
    END IF

    IF (.not. lNumericFieldBlanks(7)) THEN
      Ventilation(VentiCount)%MaxIndoorTemperature = rNumericArgs(7)
    ELSE
      Ventilation(VentiCount)%MaxIndoorTemperature = VentilTempLimit
    ENDIF
    IF ((Ventilation(VentiCount)%MaxIndoorTemperature < -VentilTempLimit) .OR. &
        (Ventilation(VentiCount)%MaxIndoorTemperature >  VentilTempLimit))  THEN
      CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1)) // &
                            '" must have a maximum indoor temperature between -100C and 100C')
      ErrorsFound = .TRUE.
    END IF

    Ventilation(VentiCount)%MaxIndoorTempSchedPtr  = GetScheduleIndex(cAlphaArgs(5))
    IF (Ventilation(VentiCount)%MaxIndoorTempSchedPtr > 0) THEN
      IF (.NOT. lNumericFieldBlanks(7)) &
      CALL ShowWarningError(RoutineName//'The Maximum Indoor Temperature value and schedule are provided. '//  &
         'The scheduled temperature will be used in the '//TRIM(cCurrentModuleObject)//' object = '//TRIM(cAlphaArgs(1)))
      ! Check min and max values in the schedule to ensure both values are within the range
      IF (.NOT.CheckScheduleValueMinMax(Ventilation(VentiCount)%MaxIndoorTempSchedPtr,'>=', &
         -VentilTempLimit,'<=',VentilTempLimit)) THEN
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1)) // &
         ' must have a maximum indoor temperature between -100C and 100C defined in the schedule = '//TRIM(cAlphaArgs(5)))
        ErrorsFound = .TRUE.
      END IF
    END IF
    IF (Ventilation(VentiCount)%MaxIndoorTempSchedPtr == 0 .AND. lNumericFieldBlanks(7) .AND. (.NOT. lAlphaFieldBlanks(5))) THEN
      CALL ShowWarningError(RoutineName//trim(cNumericFieldNames(7))//': '//  &
         'the value field is blank and schedule field is invalid. The default '// &
         'value will be used ('//TRIM(RoundSigDigits(VentilTempLimit,1))//') ')
      CALL ShowContinueError('in the '//TRIM(cCurrentModuleObject)//' object = '//TRIM(cAlphaArgs(1))// &
                             ' and the simulation continues...')
    END IF
    ! Check Maximum indoor temperature value and schedule fields
    IF (.NOT. lNumericFieldBlanks(7) .AND. ((.NOT. lAlphaFieldBlanks(5)) .AND.  &
        Ventilation(VentiCount)%MaxIndoorTempSchedPtr==0)) THEN
      CALL ShowWarningError(RoutineName//trim(cAlphaFieldNames(7))//' = '//  &
         TRIM(cAlphaArgs(5))//' is invalid. ' &
         //'The constant value will be used at '//TRIM(RoundSigDigits(rNumericArgs(7),1))//' degrees C ')
      CALL ShowContinueError('in the '//TRIM(cCurrentModuleObject)//' object = '//TRIM(cAlphaArgs(1))// &
                             ' and the simulation continues...')
    END IF

    IF (.not. lNumericFieldBlanks(8)) THEN
      Ventilation(VentiCount)%DelTemperature = rNumericArgs(8)
    ELSE
      Ventilation(VentiCount)%DelTemperature = -VentilTempLimit
    ENDIF

    Ventilation(VentiCount)%DeltaTempSchedPtr  = GetScheduleIndex(cAlphaArgs(6))
    IF (Ventilation(VentiCount)%DeltaTempSchedPtr > 0) THEN
      IF (.NOT. lNumericFieldBlanks(8)) &
        CALL ShowWarningError(RoutineName//'The Delta Temperature value and schedule are provided. The scheduled temperature will'&
          //' be used in the '//TRIM(cCurrentModuleObject)//' object = '//TRIM(cAlphaArgs(1)))
      ! Check min value in the schedule to ensure both values are within the range
      IF (GetScheduleMinValue(Ventilation(VentiCount)%DeltaTempSchedPtr) < -VentilTempLimit) THEN
        CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//' statement = '//TRIM(cAlphaArgs(1)) // &
         ' must have a delta temperature equal to or above -100C defined in the schedule = '//TRIM(cAlphaArgs(8)))
        ErrorsFound = .TRUE.
      END IF
    END IF
    IF (Ventilation(VentiCount)%DeltaTempSchedPtr == 0 .AND. lNumericFieldBlanks(8) .AND. (.NOT. lAlphaFieldBlanks(6))) THEN
      CALL ShowWarningError(RoutineName//trim(cNumericFieldNames(8))//': the value field is blank and schedule field is invalid. ' &
          //'The default value will be used ('//TRIM(RoundSigDigits(VentilTempLimit,1))//') ')
      CALL ShowContinueError('in the '//TRIM(cCurrentModuleObject)//' object = '//TRIM(cAlphaArgs(1))//&
                             ' and the simulation continues...')
    END IF
    IF (.NOT. lNumericFieldBlanks(8) .AND. ((.NOT.lAlphaFieldBlanks(6)).AND.Ventilation(VentiCount)%DeltaTempSchedPtr == 0)) THEN
      CALL ShowWarningError(RoutineName//trim(cAlphaFieldNames(6))//' = '//TRIM(cAlphaArgs(6))//' is invalid. ' &
         //'The constant value will be used at '//TRIM(RoundSigDigits(rNumericArgs(8),1))//' degrees C ')
      CALL ShowContinueError('in the '//TRIM(cCurrentModuleObject)//' object = '//TRIM(cAlphaArgs(1))// &
                             ' and the simulation continues...')
    END IF

    IF (.not. lNumericFieldBlanks(9)) THEN
      Ventilation(VentiCount)%MinOutdoorTemperature = rNumericArgs(9)
    ELSE
      Ventilation(VentiCount)%MinOutdoorTemperature = -VentilTempLimit
    ENDIF
    IF ((Ventilation(VentiCount)%MinOutdoorTemperature < -VentilTempLimit) .OR. &
        (Ventilation(VentiCount)%MinOutdoorTemperature >  VentilTempLimit))  THEN
      CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//' statement = '//TRIM(cAlphaArgs(1)) // &
                            ' must have '//trim(cNumericFieldNames(9))//' between -100C and 100C')
      ErrorsFound = .TRUE.
    END IF

    Ventilation(VentiCount)%MinOutdoorTempSchedPtr  = GetScheduleIndex(cAlphaArgs(7))
    IF (Ventilation(VentiCount)%MinOutdoorTempSchedPtr > 0) THEN
      IF (.not. lNumericFieldBlanks(9)) &
        CALL ShowWarningError(RoutineName//'The Minimum Outdoor Temperature value and schedule are provided. '//  &
         'The scheduled temperature will be used in the '//TRIM(cCurrentModuleObject)//' object = '//TRIM(cAlphaArgs(1)))
      ! Check min and max values in the schedule to ensure both values are within the range
      IF (.NOT.CheckScheduleValueMinMax(Ventilation(VentiCount)%MinOutdoorTempSchedPtr,'>=', &
          -VentilTempLimit,'<=',VentilTempLimit)) THEN
        CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//' statement = '//TRIM(cAlphaArgs(1)) // &
         ' must have a minimum outdoor temperature between -100C and 100C defined in the schedule = '//TRIM(cAlphaArgs(7)))
        ErrorsFound = .TRUE.
      END IF
    END IF
    IF (Ventilation(VentiCount)%MinOutdoorTempSchedPtr == 0 .AND. lNumericFieldBlanks(9).AND. (.NOT. lAlphaFieldBlanks(7))) THEN
      CALL ShowWarningError(RoutineName//'Minimum Outdoor Temperature: '//  &
         'the value field is blank and schedule field is invalid. The default ' &
          //'value will be used ('//TRIM(RoundSigDigits(-VentilTempLimit,1))//') ')
      CALL ShowContinueError('in the '//TRIM(cCurrentModuleObject)//' object = '//TRIM(cAlphaArgs(1))// &
                             ' and the simulation continues...')
    END IF
    ! Check Minimum outdoor temperature value and schedule fields
    IF (.NOT. lNumericFieldBlanks(9) .AND. ((.NOT.lAlphaFieldBlanks(7)).AND.Ventilation(VentiCount)%MinOutdoorTempSchedPtr==0)) THEN
      CALL ShowWarningError(RoutineName//trim(cAlphaFieldNames(7))//' = '//TRIM(cAlphaArgs(7))//  &
         ' is invalid. The constant value will be used at '//TRIM(RoundSigDigits(rNumericArgs(14),1))//' degrees C ')
      CALL ShowContinueError('in the '//TRIM(cCurrentModuleObject)//' object = '//TRIM(cAlphaArgs(1))// &
                             ' and the simulation continues...')
    END IF

    IF (.not. lNumericFieldBlanks(10)) THEN
      Ventilation(VentiCount)%MaxOutdoorTemperature = rNumericArgs(10)
    ELSE
      Ventilation(VentiCount)%MaxOutdoorTemperature = VentilTempLimit
    ENDIF
    IF ((Ventilation(VentiCount)%MaxOutdoorTemperature < -VentilTempLimit) .OR. &
        (Ventilation(VentiCount)%MaxOutdoorTemperature >  VentilTempLimit))  THEN
      CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//' statement = '//TRIM(cAlphaArgs(1)) // &
                            ' must have a '//TRIM(cNumericFieldNames(10))//' between -100C and 100C')
      ErrorsFound = .TRUE.
    END IF

    Ventilation(VentiCount)%MaxOutdoorTempSchedPtr  = GetScheduleIndex(cAlphaArgs(8))
    IF (Ventilation(VentiCount)%MaxOutdoorTempSchedPtr > 0) THEN
      IF (.not. lNumericFieldBlanks(10)) &
      CALL ShowWarningError(RoutineName//'The Maximum Outdoor Temperature value and schedule are provided. '//  &
          'The scheduled temperature will be used in the '//TRIM(cCurrentModuleObject)//' object = '//TRIM(cAlphaArgs(1)))
      IF (.NOT.CheckScheduleValueMinMax(Ventilation(VentiCount)%MaxOutdoorTempSchedPtr,'>=', &
          -VentilTempLimit,'<=',VentilTempLimit)) THEN
        CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//' statement = '//TRIM(cAlphaArgs(1)) // &
         ' must have a maximum outdoor temperature between -100C and 100C defined in the schedule = '//TRIM(cAlphaArgs(8)))
        ErrorsFound = .TRUE.
      END IF
    END IF
    IF (Ventilation(VentiCount)%MaxOutdoorTempSchedPtr == 0 .AND. lNumericFieldBlanks(10).AND. (.NOT. lAlphaFieldBlanks(8))) THEN
      CALL ShowWarningError(RoutineName//trim(cNumericFieldNames(10))//': '//  &
         'the value field is blank and schedule field is invalid.'//  &
         ' The default value will be used ('//TRIM(RoundSigDigits(VentilTempLimit,1))//') ')
      CALL ShowContinueError('in the '//TRIM(cCurrentModuleObject)//' object = '//TRIM(cAlphaArgs(1))// &
                             ' and the simulation continues...')
    END IF
    ! Check Maximum outdoor temperature value and schedule fields
    IF (.NOT. lNumericFieldBlanks(10) .AND. ((.NOT. lAlphaFieldBlanks(8)) .AND. &
        Ventilation(VentiCount)%MaxOutdoorTempSchedPtr==0)) THEN
      CALL ShowWarningError(RoutineName//trim(cAlphaFieldNames(8))//' = '//TRIM(cAlphaArgs(8))//  &
         'is invalid. The constant value will be used at '//TRIM(RoundSigDigits(rNumericArgs(10),1))//' degrees C ')
      CALL ShowContinueError('in the '//TRIM(cCurrentModuleObject)//' object = '//TRIM(cAlphaArgs(1))// &
                             ' and the simulation continues...')
    END IF

    IF (.not. lNumericFieldBlanks(11)) THEN
      Ventilation(VentiCount)%MaxWindSpeed = rNumericArgs(11)
    ELSE
      Ventilation(VentiCount)%MaxWindSpeed = VentilWSLimit
    ENDIF
    IF ((Ventilation(VentiCount)%MaxWindSpeed < -VentilWSLimit) .OR. &
        (Ventilation(VentiCount)%MaxWindSpeed >  VentilWSLimit))  THEN
      CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//' statement = '//TRIM(cAlphaArgs(1)) // &
                            ' must have a maximum wind speed between 0 m/s and 40 m/s')
      ErrorsFound = .TRUE.
    END IF

    ! Report variables should be added for individual VENTILATION objects, in addition to zone totals below

    IF (Ventilation(VentiCount)%ZonePtr > 0) THEN
      IF (RepVarSet(Ventilation(VentiCount)%ZonePtr) .AND. .NOT. Ventilation(Loop)%QuadratureSum) THEN
        RepVarSet(Ventilation(VentiCount)%ZonePtr)=.FALSE.
        CALL SetupOutputVariable('Zone Ventilation Sensible Heat Loss Energy [J]', &
             ZnAirRpt(Ventilation(VentiCount)%ZonePtr)%VentilHeatLoss, 'System','Sum',Zone(Ventilation(VentiCount)%ZonePtr)%Name)
        CALL SetupOutputVariable('Zone Ventilation Sensible Heat Gain Energy [J]', &
             ZnAirRpt(Ventilation(VentiCount)%ZonePtr)%VentilHeatGain, 'System','Sum',Zone(Ventilation(VentiCount)%ZonePtr)%Name)
        CALL SetupOutputVariable('Zone Ventilation Latent Heat Loss Energy [J]',  &
             ZnAirRpt(Ventilation(VentiCount)%ZonePtr)%VentilLatentLoss,'System','Sum',Zone(Ventilation(VentiCount)%ZonePtr)%Name)
        CALL SetupOutputVariable('Zone Ventilation Latent Heat Gain Energy [J]', &
             ZnAirRpt(Ventilation(VentiCount)%ZonePtr)%VentilLatentGain,'System','Sum',Zone(Ventilation(VentiCount)%ZonePtr)%Name)
        CALL SetupOutputVariable('Zone Ventilation Total Heat Loss Energy [J]',  &
             ZnAirRpt(Ventilation(VentiCount)%ZonePtr)%VentilTotalLoss,'System','Sum',Zone(Ventilation(VentiCount)%ZonePtr)%Name)
        CALL SetupOutputVariable('Zone Ventilation Total Heat Gain Energy [J]',  &
             ZnAirRpt(Ventilation(VentiCount)%ZonePtr)%VentilTotalGain,'System','Sum',Zone(Ventilation(VentiCount)%ZonePtr)%Name)
        CALL SetupOutputVariable('Zone Ventilation Current Density Volume Flow Rate [m3/s]',  &
                                  ZnAirRpt(Ventilation(VentiCount)%ZonePtr)%VentilVdotCurDensity,   &
                                 'System','Average',Zone(Ventilation(VentiCount)%ZonePtr)%Name)
        CALL SetupOutputVariable('Zone Ventilation Standard Density Volume Flow Rate [m3/s]', &
                                  ZnAirRpt(Ventilation(VentiCount)%ZonePtr)%VentilVdotStdDensity,   &
                                 'System','Average',Zone(Ventilation(VentiCount)%ZonePtr)%Name)
        CALL SetupOutputVariable('Zone Ventilation Current Density Volume [m3]',              &
                                  ZnAirRpt(Ventilation(VentiCount)%ZonePtr)%VentilVolumeCurDensity, &
                                 'System','Sum',Zone(Ventilation(VentiCount)%ZonePtr)%Name)
        CALL SetupOutputVariable('Zone Ventilation Standard Density Volume [m3]',             &
                                  ZnAirRpt(Ventilation(VentiCount)%ZonePtr)%VentilVolumeStdDensity, &
                                 'System','Sum',Zone(Ventilation(VentiCount)%ZonePtr)%Name)
        CALL SetupOutputVariable('Zone Ventilation Mass [kg]',ZnAirRpt(Ventilation(VentiCount)%ZonePtr)%VentilMass,  &
                                 'System','Sum',Zone(Ventilation(VentiCount)%ZonePtr)%Name)
        CALL SetupOutputVariable('Zone Ventilation Air Change Rate [ach]',                   &
                                  ZnAirRpt(Ventilation(VentiCount)%ZonePtr)%VentilAirChangeRate,   &
                                 'System','Average',Zone(Ventilation(VentiCount)%ZonePtr)%Name)
        CALL SetupOutputVariable('Zone Ventilation Fan Electric Energy [J]',     &
                                  ZnAirRpt(Ventilation(VentiCount)%ZonePtr)%VentilFanElec,  &
                                 'System','Sum',Zone(Ventilation(VentiCount)%ZonePtr)%Name, &
                                  ResourceTypeKey='Electricity',GroupKey='Building', &
                                  ZoneKey=Zone(Ventilation(VentiCount)%ZonePtr)%Name, &
                                  EndUseKey='Fans', EndUseSubKey='Ventilation (simple)')
        CALL SetupOutputVariable('Zone Ventilation Air Inlet Temperature [C]', &
                                 ZnAirRpt(Ventilation(VentiCount)%ZonePtr)%VentilAirTemp, &
                                 'System','Average',Zone(Ventilation(VentiCount)%ZonePtr)%Name)
      END IF
    END IF

    IF (AnyEnergyManagementSystemInModel) THEN
      CALL SetupEMSActuator('Zone Ventilation', Ventilation(VentiCount)%Name, 'Air Exchange Flow Rate', '[m3/s]', &
                Ventilation(VentiCount)%EMSSimpleVentOn  , Ventilation(VentiCount)%EMSimpleVentFlowRate )
    ENDIF

  END DO

  RepVarSet=.TRUE.

  cCurrentModuleObject='ZoneMixing'
  TotMixing=GetNumObjectsFound(cCurrentModuleObject)
  ALLOCATE(Mixing(TotMixing))

  DO Loop=1,TotMixing

    CALL GetObjectItem(cCurrentModuleObject,Loop,cAlphaArgs,NumAlpha,rNumericArgs,NumNumber,IOStat, &
                       NumBlank=lNumericFieldBlanks,AlphaBlank=lAlphaFieldBlanks, &
                       AlphaFieldNames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

    IsNotOK = .FALSE.
    IsBlank = .FALSE.
    CALL VerifyName(cAlphaArgs(1),Mixing%Name,Loop-1,IsNotOK,IsBlank,TRIM(cCurrentModuleObject)//' Name')
    IF (IsNotOK) THEN
      ErrorsFound = .TRUE.
      IF (IsBlank) cAlphaArgs(1) = 'xxxxx'
    END IF
    Mixing(Loop)%Name = cAlphaArgs(1)

    Mixing(Loop)%ZonePtr=FindIteminList(cAlphaArgs(2),Zone%Name,NumOfZones)
    IF (Mixing(Loop)%ZonePtr == 0) THEN
      CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))// &
           '", invalid (not found) '//trim(cAlphaFieldNames(2))//'="'//TRIM(cAlphaArgs(2))//'".')
      ErrorsFound=.TRUE.
    ENDIF

    Mixing(Loop)%SchedPtr=GetScheduleIndex(cAlphaArgs(3))

    IF (Mixing(Loop)%SchedPtr == 0) THEN
      IF (lAlphaFieldBlanks(3)) THEN
        CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'",'//   &
             TRIM(cAlphaFieldNames(3))//' is required but field is blank.')
      ELSE
        CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'",'//   &
             ' invalid (not found) '//TRIM(cAlphaFieldNames(3))//'="'//trim(cAlphaArgs(3))//'".')
      END IF
      ErrorsFound = .TRUE.
    ENDIF

    ! Mixing equipment design level calculation method
    SELECT CASE (cAlphaArgs(4))
      CASE('FLOW/ZONE','FLOW')
        Mixing(Loop)%DesignLevel=rNumericArgs(1)
        IF (lNumericFieldBlanks(1)) THEN
          CALL ShowWarningError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
            '", '//trim(cAlphaFieldNames(4))//' specifies '//TRIM(cNumericFieldNames(1))//  &
            ', but that field is blank.  0 Mixing will result.')
        ENDIF

      CASE('FLOW/AREA')
        IF (Mixing(Loop)%ZonePtr /= 0) THEN
          IF (rNumericArgs(2) >= 0.0d0) THEN
            Mixing(Loop)%DesignLevel=rNumericArgs(2)*Zone(Mixing(Loop)%ZonePtr)%FloorArea
            IF (Zone(Mixing(Loop)%ZonePtr)%FloorArea <= 0.0d0) THEN
              CALL ShowWarningError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
                '", '//trim(cAlphaFieldNames(4))//' specifies '//TRIM(cNumericFieldNames(2))//  &
                ', but Zone Floor Area = 0.  0 Mixing will result.')
            ENDIF
          ELSE
            CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
                             '", invalid flow/person specification [<0.0]='//   &
                             TRIM(RoundSigDigits(rNumericArgs(2),3)))
            ErrorsFound=.TRUE.
          ENDIF
        ENDIF
        IF (lNumericFieldBlanks(2)) THEN
          CALL ShowWarningError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
            '", '//trim(cAlphaFieldNames(4))//' specifies '//TRIM(cNumericFieldNames(2))//  &
            ', but that field is blank.  0 Mixing will result.')
        ENDIF

      CASE('FLOW/PERSON')
        IF (Mixing(Loop)%ZonePtr /= 0) THEN
          IF (rNumericArgs(3) >= 0.0d0) THEN
            Mixing(Loop)%DesignLevel=rNumericArgs(3)*Zone(Mixing(Loop)%ZonePtr)%TotOccupants
            IF (Zone(Mixing(Loop)%ZonePtr)%TotOccupants <= 0.0d0) THEN
              CALL ShowWarningError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
                '", '//trim(cAlphaFieldNames(4))//' specifies '//TRIM(cNumericFieldNames(3))//  &
                ', but Zone Total Occupants = 0.  0 Mixing will result.')
            ENDIF
          ELSE
            CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
                             '", invalid flow/person specification [<0.0]='//   &
                             TRIM(RoundSigDigits(rNumericArgs(3),3)))
            ErrorsFound=.TRUE.
          ENDIF
        ENDIF
        IF (lNumericFieldBlanks(3)) THEN
          CALL ShowWarningError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
            '", '//trim(cAlphaFieldNames(4))//' specifies '//TRIM(cNumericFieldNames(3))//  &
            ', but that field is blank.  0 Mixing will result.')
        ENDIF

      CASE('AIRCHANGES/HOUR')
        IF (Mixing(Loop)%ZonePtr /= 0) THEN
          IF (rNumericArgs(4) >= 0.0d0) THEN
            Mixing(Loop)%DesignLevel=rNumericArgs(4)*Zone(Mixing(Loop)%ZonePtr)%Volume/SecInHour
            IF (Zone(Mixing(Loop)%ZonePtr)%Volume <= 0.0d0) THEN
              CALL ShowWarningError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
                '", '//trim(cAlphaFieldNames(4))//' specifies '//TRIM(cNumericFieldNames(4))//  &
                ', but Zone Volume = 0.  0 Mixing will result.')
            ENDIF
          ELSE
            CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
                             '", invalid flow/person specification [<0.0]='//   &
                             TRIM(RoundSigDigits(rNumericArgs(4),3)))
            ErrorsFound=.TRUE.
          ENDIF
        ENDIF
        IF (lNumericFieldBlanks(4)) THEN
          CALL ShowWarningError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
            '", '//trim(cAlphaFieldNames(4))//' specifies '//TRIM(cNumericFieldNames(4))//  &
            ', but that field is blank.  0 Mixing will result.')
        ENDIF

      CASE DEFAULT
        CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
                             '", invalid calculation method='//   &
                             TRIM(cAlphaArgs(4)))
        ErrorsFound=.TRUE.
    END SELECT

    Mixing(Loop)%FromZone=FindIteminList(cAlphaArgs(5),Zone%Name,NumOfZones)
    IF (Mixing(Loop)%FromZone == 0) THEN
      CALL ShowSevereError(RoutineName//trim(cAlphaFieldNames(5))//' not found='//TRIM(cAlphaArgs(5))//' for '// &
                           TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
      ErrorsFound=.TRUE.
    ENDIF
    Mixing(Loop)%DeltaTemperature=rNumericArgs(5)

    IF (NumAlpha > 5) THEN
      Mixing(Loop)%DeltaTempSchedPtr=GetScheduleIndex(cAlphaArgs(6))
      IF (Mixing(Loop)%DeltaTempSchedPtr > 0) THEN
        IF (.NOT. lNumericFieldBlanks(5)) &
        CALL ShowWarningError(RoutineName//'The Delta Temperature value and schedule are provided. The scheduled temperature'// &
                              ' will be used in the '//TRIM(cCurrentModuleObject)//' object = '//TRIM(cAlphaArgs(1)))
        IF (GetScheduleMinValue(Mixing(Loop)%DeltaTempSchedPtr) < -MixingTempLimit) THEN
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//' statement = '//TRIM(cAlphaArgs(1))//' must have'// &
                               ' a delta temperature equal to or above -100C defined in the schedule = '//TRIM(cAlphaArgs(6)))
          ErrorsFound = .TRUE.
        END IF
      END IF
    END IF
    IF (Mixing(Loop)%DeltaTempSchedPtr == 0 .AND. lNumericFieldBlanks(5).AND. (.NOT. lAlphaFieldBlanks(6))) THEN
      CALL ShowWarningError(RoutineName//trim(cNumericFieldNames(5))//': the value field is blank and schedule field is invalid. '&
          //'The default value will be used ('//TRIM(RoundSigDigits(rNumericArgs(5),1))//') ')
      CALL ShowContinueError('in the '//TRIM(cCurrentModuleObject)//' object = '//TRIM(cAlphaArgs(1))// &
                             ' and the simulation continues...')
    END IF
    IF (.NOT. lNumericFieldBlanks(5) .AND. ((.NOT. lAlphaFieldBlanks(6)) .AND. Mixing(Loop)%DeltaTempSchedPtr == 0)) THEN
      CALL ShowWarningError(RoutineName//trim(cAlphaFieldNames(6))//' = '//TRIM(cAlphaArgs(6))//' is invalid. ' &
         //'The constant value will be used at '//TRIM(RoundSigDigits(rNumericArgs(5),1))//' degrees C ')
      CALL ShowContinueError('in the '//TRIM(cCurrentModuleObject)//' object = '//TRIM(cAlphaArgs(1))// &
                             ' and the simulation continues...')
    END IF

    IF (NumAlpha > 6) THEN
      Mixing(Loop)%MinIndoorTempSchedPtr=GetScheduleIndex(cAlphaArgs(7))
      IF (Mixing(Loop)%MinIndoorTempSchedPtr == 0) THEN
        IF ((.NOT. lAlphaFieldBlanks(7))) THEN
          CALL ShowSevereError(RoutineName//trim(cAlphaFieldNames(7))//' not found='//TRIM(cAlphaArgs(7))// &
                             ' for '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
          ErrorsFound=.TRUE.
        ENDIF
      ENDIF
      IF (Mixing(Loop)%MinIndoorTempSchedPtr > 0) THEN
        ! Check min and max values in the schedule to ensure both values are within the range
        IF (.NOT.CheckScheduleValueMinMax(Mixing(Loop)%MinIndoorTempSchedPtr,'>=',-MixingTempLimit,'<=',MixingTempLimit)) THEN
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//' statement = '//TRIM(cAlphaArgs(1)) // &
           ' must have a minimum zone temperature between -100C and 100C defined in the schedule = '//TRIM(cAlphaArgs(7)))
          ErrorsFound = .TRUE.
        END IF
      END IF
    END IF

    IF (NumAlpha > 7) THEN
      Mixing(Loop)%MaxIndoorTempSchedPtr=GetScheduleIndex(cAlphaArgs(8))
      IF (Mixing(Loop)%MaxIndoorTempSchedPtr == 0) THEN
        IF ((.NOT. lAlphaFieldBlanks(8))) THEN
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'", '//  &
                          trim(cAlphaFieldNames(8))//' not found="'//TRIM(cAlphaArgs(8))//'".')
          ErrorsFound=.TRUE.
        ENDIF
      ENDIF
      IF (Mixing(Loop)%MaxIndoorTempSchedPtr > 0) THEN
        ! Check min and max values in the schedule to ensure both values are within the range
        IF (.NOT.CheckScheduleValueMinMax(Mixing(Loop)%MaxIndoorTempSchedPtr,'>=',-MixingTempLimit,'<=',MixingTempLimit)) THEN
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1)) // &
           '" must have a maximum zone temperature between -100C and 100C defined in the schedule = '//TRIM(cAlphaArgs(8)))
          ErrorsFound = .TRUE.
        END IF
      END IF
    END IF

    IF (NumAlpha > 8) THEN
      Mixing(Loop)%MinSourceTempSchedPtr=GetScheduleIndex(cAlphaArgs(9))
      IF (Mixing(Loop)%MinSourceTempSchedPtr == 0) THEN
        IF ((.NOT. lAlphaFieldBlanks(9))) THEN
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'", '//  &
                             trim(cAlphaFieldNames(9))//' not found="'//TRIM(cAlphaArgs(9))//'".')
          ErrorsFound=.TRUE.
        ENDIF
      ENDIF
      IF (Mixing(Loop)%MinSourceTempSchedPtr > 0) THEN
        ! Check min and max values in the schedule to ensure both values are within the range
        IF (.NOT.CheckScheduleValueMinMax(Mixing(Loop)%MinSourceTempSchedPtr,'>=',-MixingTempLimit,'<=',MixingTempLimit)) THEN
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1)) // &
           '" must have a minimum source temperature between -100C and 100C defined in the schedule = '//TRIM(cAlphaArgs(9)))
          ErrorsFound = .TRUE.
        END IF
      END IF
    END IF

    IF (NumAlpha > 9) THEN
      Mixing(Loop)%MaxSourceTempSchedPtr=GetScheduleIndex(cAlphaArgs(10))
      IF (Mixing(Loop)%MaxSourceTempSchedPtr == 0) THEN
        IF ((.NOT. lAlphaFieldBlanks(10))) THEN
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'", '//  &
                         trim(cAlphaFieldNames(10))//' not found="'//TRIM(cAlphaArgs(10))//'".')
          ErrorsFound=.TRUE.
        ENDIF
      ENDIF
      IF (Mixing(Loop)%MaxSourceTempSchedPtr > 0) THEN
        ! Check min and max values in the schedule to ensure both values are within the range
        IF (.NOT.CheckScheduleValueMinMax(Mixing(Loop)%MaxSourceTempSchedPtr,'>=',-MixingTempLimit,'<=',MixingTempLimit)) THEN
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//' statement ="'//TRIM(cAlphaArgs(1)) // &
           '" must have a maximum source temperature between -100C and 100C defined in the schedule = '//TRIM(cAlphaArgs(10)))
          ErrorsFound = .TRUE.
        END IF
      END IF
    END IF

    IF (NumAlpha > 10) THEN
      Mixing(Loop)%MinOutdoorTempSchedPtr=GetScheduleIndex(cAlphaArgs(11))
      IF (Mixing(Loop)%MinOutdoorTempSchedPtr == 0) THEN
        IF ((.NOT. lAlphaFieldBlanks(11))) THEN
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'", '//  &
                         trim(cAlphaFieldNames(11))//' not found="'//TRIM(cAlphaArgs(11))//'".')
          ErrorsFound=.TRUE.
        ENDIF
      ENDIF
      IF (Mixing(Loop)%MinOutdoorTempSchedPtr > 0) THEN
        ! Check min and max values in the schedule to ensure both values are within the range
        IF (.NOT.CheckScheduleValueMinMax(Mixing(Loop)%MinOutdoorTempSchedPtr,'>=',-MixingTempLimit,'<=',MixingTempLimit)) THEN
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//' ="'//TRIM(cAlphaArgs(1)) // &
           '" must have a minimum outdoor temperature between -100C and 100C defined in the schedule = '//TRIM(cAlphaArgs(11)))
          ErrorsFound = .TRUE.
        END IF
      END IF
    END IF

    IF (NumAlpha > 11) THEN
      Mixing(Loop)%MaxOutdoorTempSchedPtr=GetScheduleIndex(cAlphaArgs(12))
      IF (Mixing(Loop)%MaxOutdoorTempSchedPtr == 0) THEN
        IF ((.NOT. lAlphaFieldBlanks(12))) THEN
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'", '//  &
                        trim(cAlphaFieldNames(12))//' not found="'//TRIM(cAlphaArgs(12))//'".')
          ErrorsFound=.TRUE.
        ENDIF
      ENDIF
      IF (Mixing(Loop)%MaxOutdoorTempSchedPtr > 0) THEN
        ! Check min and max values in the schedule to ensure both values are within the range
        IF (.NOT.CheckScheduleValueMinMax(Mixing(Loop)%MaxOutdoorTempSchedPtr,'>=',-MixingTempLimit,'<=',MixingTempLimit)) THEN
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//' ="'//TRIM(cAlphaArgs(1)) // &
           '" must have a maximum outdoor temperature between -100C and 100C defined in the schedule = '//TRIM(cAlphaArgs(12)))
          ErrorsFound = .TRUE.
        END IF
      END IF
    END IF

    IF (Mixing(Loop)%ZonePtr > 0) THEN
      IF (RepVarSet(Mixing(Loop)%ZonePtr)) THEN
        RepVarSet(Mixing(Loop)%ZonePtr)=.FALSE.
        CALL SetupOutputVariable('Zone Mixing Volume [m3]',ZnAirRpt(Mixing(Loop)%ZonePtr)%MixVolume,  &
                                 'System','Sum',Zone(Mixing(Loop)%ZonePtr)%Name)
        CALL SetupOutputVariable('Zone Mixing Mass [kg]',ZnAirRpt(Mixing(Loop)%ZonePtr)%MixMass,  &
                                 'System','Sum',Zone(Mixing(Loop)%ZonePtr)%Name)
        CALL SetupOutputVariable('Zone Mixing Sensible Heat Loss Energy [J]',ZnAirRpt(Mixing(Loop)%ZonePtr)%MixHeatLoss,  &
                                 'System','Sum',Zone(Mixing(Loop)%ZonePtr)%Name)
        CALL SetupOutputVariable('Zone Mixing Sensible Heat Gain Energy [J]',ZnAirRpt(Mixing(Loop)%ZonePtr)%MixHeatGain,  &
                                 'System','Sum',Zone(Mixing(Loop)%ZonePtr)%Name)
        CALL SetupOutputVariable('Zone Mixing Latent Heat Loss Energy [J]',ZnAirRpt(Mixing(Loop)%ZonePtr)%MixLatentLoss,  &
                                 'System','Sum',Zone(Mixing(Loop)%ZonePtr)%Name)
        CALL SetupOutputVariable('Zone Mixing Latent Heat Gain Energy [J]',ZnAirRpt(Mixing(Loop)%ZonePtr)%MixLatentGain,  &
                                 'System','Sum',Zone(Mixing(Loop)%ZonePtr)%Name)
        CALL SetupOutputVariable('Zone Mixing Total Heat Loss Energy [J]',ZnAirRpt(Mixing(Loop)%ZonePtr)%MixTotalLoss,  &
                                 'System','Sum',Zone(Mixing(Loop)%ZonePtr)%Name)
        CALL SetupOutputVariable('Zone Mixing Total Heat Gain Energy [J]',ZnAirRpt(Mixing(Loop)%ZonePtr)%MixTotalGain,  &
                                 'System','Sum',Zone(Mixing(Loop)%ZonePtr)%Name)
      ENDIF
    ENDIF
    IF (AnyEnergyManagementSystemInModel) THEN
      CALL SetupEMSActuator('ZoneMixing', Mixing(Loop)%Name, 'Air Exchange Flow Rate', '[m3/s]', &
                Mixing(Loop)%EMSSimpleMixingOn  , Mixing(Loop)%EMSimpleMixingFlowRate )
    ENDIF

  END DO

  cCurrentModuleObject='ZoneCrossMixing'
  TotCrossMixing=GetNumObjectsFound(cCurrentModuleObject)
  ALLOCATE(CrossMixing(TotCrossMixing))

  DO Loop=1,TotCrossMixing

    CALL GetObjectItem(cCurrentModuleObject,Loop,cAlphaArgs,NumAlpha,rNumericArgs,NumNumber,IOStat, &
                       NumBlank=lNumericFieldBlanks,AlphaBlank=lAlphaFieldBlanks, &
                       AlphaFieldNames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

    IsNotOK = .FALSE.
    IsBlank = .FALSE.
    CALL VerifyName(cAlphaArgs(1),CrossMixing%Name,Loop-1,IsNotOK,IsBlank,TRIM(cCurrentModuleObject)//' Name')

    IF (IsNotOK) THEN
      ErrorsFound = .TRUE.
      IF (IsBlank) cAlphaArgs(1) = 'xxxxx'
    END IF
    CrossMixing(Loop)%Name = cAlphaArgs(1)

    CrossMixing(Loop)%ZonePtr=FindIteminList(cAlphaArgs(2),Zone%Name,NumOfZones)
    IF (CrossMixing(Loop)%ZonePtr == 0) THEN
      CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))// &
           '", invalid (not found) '//trim(cAlphaFieldNames(2))//'="'//TRIM(cAlphaArgs(2))//'".')
      ErrorsFound=.TRUE.
    ENDIF

    CrossMixing(Loop)%SchedPtr=GetScheduleIndex(cAlphaArgs(3))
    IF (CrossMixing(Loop)%SchedPtr == 0) THEN
      IF (lAlphaFieldBlanks(3)) THEN
        CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'",'//   &
             TRIM(cAlphaFieldNames(3))//' is required but field is blank.')
      ELSE
        CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'",'//   &
             ' invalid (not found) '//TRIM(cAlphaFieldNames(3))//'="'//trim(cAlphaArgs(3))//'".')
      END IF
      ErrorsFound=.TRUE.
    ENDIF

    ! Mixing equipment design level calculation method.
    SELECT CASE (cAlphaArgs(4))
      CASE('FLOW/ZONE','FLOW')
        CrossMixing(Loop)%DesignLevel=rNumericArgs(1)
        IF (lNumericFieldBlanks(1)) THEN
          CALL ShowWarningError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
            '", '//trim(cAlphaFieldNames(4))//' specifies '//TRIM(cNumericFieldNames(1))//  &
            ', but that field is blank.  0 Cross Mixing will result.')
        ENDIF

      CASE('FLOW/AREA')
        IF (CrossMixing(Loop)%ZonePtr /= 0) THEN
          IF (rNumericArgs(2) >= 0.0d0) THEN
            CrossMixing(Loop)%DesignLevel=rNumericArgs(2)*Zone(CrossMixing(Loop)%ZonePtr)%FloorArea
            IF (Zone(CrossMixing(Loop)%ZonePtr)%FloorArea <= 0.0d0) THEN
              CALL ShowWarningError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
                '", '//trim(cAlphaFieldNames(4))//' specifies '//TRIM(cNumericFieldNames(2))//  &
                ', but Zone Floor Area = 0.  0 Cross Mixing will result.')
            ENDIF
          ELSE
            CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
                             '", invalid flow/person specification [<0.0]='//   &
                             TRIM(RoundSigDigits(rNumericArgs(2),3)))
            ErrorsFound=.TRUE.
          ENDIF
        ENDIF
        IF (lNumericFieldBlanks(2)) THEN
          CALL ShowWarningError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
            '", '//trim(cAlphaFieldNames(4))//' specifies '//TRIM(cNumericFieldNames(2))//  &
            ', but that field is blank.  0 Cross Mixing will result.')
        ENDIF

      CASE('FLOW/PERSON')
        IF (CrossMixing(Loop)%ZonePtr /= 0) THEN
          IF (rNumericArgs(3) >= 0.0d0) THEN
            CrossMixing(Loop)%DesignLevel=rNumericArgs(3)*Zone(CrossMixing(Loop)%ZonePtr)%TotOccupants
            IF (Zone(CrossMixing(Loop)%ZonePtr)%TotOccupants <= 0.0d0) THEN
              CALL ShowWarningError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
                '", '//trim(cAlphaFieldNames(4))//' specifies '//TRIM(cNumericFieldNames(3))//  &
                ', but Zone Total Occupants = 0.  0 Cross Mixing will result.')
            ENDIF
          ELSE
            CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
                             '", invalid flow/person specification [<0.0]='//   &
                             TRIM(RoundSigDigits(rNumericArgs(3),3)))
            ErrorsFound=.TRUE.
          ENDIF
        ENDIF
        IF (lNumericFieldBlanks(3)) THEN
          CALL ShowWarningError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
            '", '//trim(cAlphaFieldNames(4))//' specifies '//TRIM(cNumericFieldNames(3))//  &
            ', but that field is blank.  0 Cross Mixing will result.')
        ENDIF

      CASE('AIRCHANGES/HOUR')
        IF (CrossMixing(Loop)%ZonePtr /= 0) THEN
          IF (rNumericArgs(4) >= 0.0d0) THEN
            CrossMixing(Loop)%DesignLevel=rNumericArgs(4)*Zone(CrossMixing(Loop)%ZonePtr)%Volume/SecInHour
            IF (Zone(CrossMixing(Loop)%ZonePtr)%Volume <= 0.0d0) THEN
              CALL ShowWarningError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
                '", '//trim(cAlphaFieldNames(4))//' specifies '//TRIM(cNumericFieldNames(4))//  &
                ', but Zone Volume = 0.  0 Cross Mixing will result.')
            ENDIF
          ELSE
            CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
                             '", invalid flow/person specification [<0.0]='//   &
                             TRIM(RoundSigDigits(rNumericArgs(4),3)))
            ErrorsFound=.TRUE.
          ENDIF
        ENDIF
        IF (lNumericFieldBlanks(4)) THEN
          CALL ShowWarningError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
            '", '//trim(cAlphaFieldNames(4))//' specifies '//TRIM(cNumericFieldNames(4))//  &
            ', but that field is blank.  0 Cross Mixing will result.')
        ENDIF

      CASE DEFAULT
        CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
                             '", invalid calculation method='//   &
                             TRIM(cAlphaArgs(4)))
        ErrorsFound=.TRUE.
    END SELECT

    CrossMixing(Loop)%FromZone=FindIteminList(cAlphaArgs(5),Zone%Name,NumOfZones)
    IF (CrossMixing(Loop)%FromZone == 0) THEN
      CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))// &
           '", invalid (not found) '//trim(cAlphaFieldNames(5))//'="'//TRIM(cAlphaArgs(5))//'".')
      ErrorsFound=.TRUE.
    ENDIF
    CrossMixing(Loop)%DeltaTemperature=rNumericArgs(5)

    IF (NumAlpha > 5) THEN
      CrossMixing(Loop)%DeltaTempSchedPtr=GetScheduleIndex(cAlphaArgs(6))
      IF (CrossMixing(Loop)%DeltaTempSchedPtr > 0) THEN
        IF (.NOT. lNumericFieldBlanks(5)) &
        CALL ShowWarningError(RoutineName//'The Delta Temperature value and schedule are provided. The scheduled temperature ' &
                              //'will be used in the '//TRIM(cCurrentModuleObject)//' object = '//TRIM(cAlphaArgs(1)))
        IF (GetScheduleMinValue(CrossMixing(Loop)%DeltaTempSchedPtr) < 0.0d0) THEN
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1)) // &
         ' must have a delta temperature equal to or above 0 C defined in the schedule = '//TRIM(cAlphaArgs(6)))
          ErrorsFound = .TRUE.
        END IF
      END IF
    END IF
    IF (CrossMixing(Loop)%DeltaTempSchedPtr == 0 .AND. lNumericFieldBlanks(5).AND. (.NOT. lAlphaFieldBlanks(6))) THEN
      CALL ShowWarningError(RoutineName//trim(cNumericFieldNames(5))//': the value field is blank and schedule field is invalid. ' &
          //'The default value will be used ('//TRIM(RoundSigDigits(rNumericArgs(5),1))//') ')
      CALL ShowContinueError('in '//TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1))//' and the simulation continues...')
    END IF
    IF (.NOT. lNumericFieldBlanks(5) .AND. ((.NOT. lAlphaFieldBlanks(6)) .AND. CrossMixing(Loop)%DeltaTempSchedPtr == 0)) THEN
      CALL ShowWarningError(RoutineName//trim(cAlphaFieldNames(6))//' = '//TRIM(cAlphaArgs(6))//' is invalid. ' &
         //'The constant value will be used at '//TRIM(RoundSigDigits(rNumericArgs(5),1))//' degrees C ')
      CALL ShowContinueError('in the '//TRIM(cCurrentModuleObject)//' object = '//TRIM(cAlphaArgs(1))// &
                             ' and the simulation continues...')
    END IF

    IF (NumAlpha > 6) THEN
      CrossMixing(Loop)%MinIndoorTempSchedPtr=GetScheduleIndex(cAlphaArgs(7))
      IF (CrossMixing(Loop)%MinIndoorTempSchedPtr == 0) THEN
        IF ((.NOT. lAlphaFieldBlanks(7))) THEN
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'",'//   &
                          trim(cAlphaFieldNames(7))//' not found='//TRIM(cAlphaArgs(7))//'".')
          ErrorsFound=.TRUE.
        ENDIF
      ENDIF
      IF (CrossMixing(Loop)%MinIndoorTempSchedPtr > 0) THEN
        ! Check min and max values in the schedule to ensure both values are within the range
        IF (.NOT.CheckScheduleValueMinMax(CrossMixing(Loop)%MinIndoorTempSchedPtr,'>=',-MixingTempLimit,'<=',MixingTempLimit)) THEN
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1)) // &
           ' must have a minimum zone temperature between -100C and 100C defined in the schedule = '//TRIM(cAlphaArgs(7)))
          ErrorsFound = .TRUE.
        END IF
      END IF
    END IF

    IF (NumAlpha > 7) THEN
      CrossMixing(Loop)%MaxIndoorTempSchedPtr=GetScheduleIndex(cAlphaArgs(8))
      IF (CrossMixing(Loop)%MaxIndoorTempSchedPtr == 0) THEN
        IF ((.NOT. lAlphaFieldBlanks(8))) THEN
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'",'//   &
                           trim(cAlphaFieldNames(8))//' not found="'//TRIM(cAlphaArgs(8))//'".')
          ErrorsFound=.TRUE.
        ENDIF
      ENDIF
      IF (CrossMixing(Loop)%MaxIndoorTempSchedPtr > 0) THEN
        ! Check min and max values in the schedule to ensure both values are within the range
        IF (.NOT.CheckScheduleValueMinMax(CrossMixing(Loop)%MaxIndoorTempSchedPtr,'>=',-MixingTempLimit,'<=',MixingTempLimit)) THEN
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1)) // &
           ' must have a maximum zone temperature between -100C and 100C defined in the schedule = '//TRIM(cAlphaArgs(8)))
          ErrorsFound = .TRUE.
        END IF
      END IF
    END IF

    IF (NumAlpha > 8) THEN
      CrossMixing(Loop)%MinSourceTempSchedPtr=GetScheduleIndex(cAlphaArgs(9))
      IF (CrossMixing(Loop)%MinSourceTempSchedPtr == 0) THEN
        IF ((.NOT. lAlphaFieldBlanks(9))) THEN
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'",'//   &
                           trim(cAlphaFieldNames(9))//' not found="'//TRIM(cAlphaArgs(9))//'".')
          ErrorsFound=.TRUE.
        ENDIF
      ENDIF
      IF (CrossMixing(Loop)%MinSourceTempSchedPtr > 0) THEN
        ! Check min and max values in the schedule to ensure both values are within the range
        IF (.NOT.CheckScheduleValueMinMax(CrossMixing(Loop)%MinSourceTempSchedPtr,'>=',-MixingTempLimit,'<=',MixingTempLimit)) THEN
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1)) // &
           ' must have a minimum source temperature between -100C and 100C defined in the schedule = '//TRIM(cAlphaArgs(9)))
          ErrorsFound = .TRUE.
        END IF
      END IF
    END IF

    IF (NumAlpha > 9) THEN
      CrossMixing(Loop)%MaxSourceTempSchedPtr=GetScheduleIndex(cAlphaArgs(10))
      IF (CrossMixing(Loop)%MaxSourceTempSchedPtr == 0) THEN
        IF ((.NOT. lAlphaFieldBlanks(10))) THEN
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'",'//   &
                           trim(cAlphaFieldNames(10))//' not found="'//TRIM(cAlphaArgs(9))//'".')
          ErrorsFound=.TRUE.
        ENDIF
      ENDIF
      IF (CrossMixing(Loop)%MaxSourceTempSchedPtr > 0) THEN
        ! Check min and max values in the schedule to ensure both values are within the range
        IF (.NOT.CheckScheduleValueMinMax(CrossMixing(Loop)%MaxSourceTempSchedPtr,'>=',-MixingTempLimit,'<=',MixingTempLimit)) THEN
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1)) // &
           ' must have a maximum source temperature between -100C and 100C defined in the schedule = '//TRIM(cAlphaArgs(10)))
          ErrorsFound = .TRUE.
        END IF
      END IF
    END IF

    IF (NumAlpha > 10) THEN
      CrossMixing(Loop)%MinOutdoorTempSchedPtr=GetScheduleIndex(cAlphaArgs(11))
      IF (CrossMixing(Loop)%MinOutdoorTempSchedPtr == 0) THEN
        IF ((.NOT. lAlphaFieldBlanks(11))) THEN
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'",'//   &
                           trim(cAlphaFieldNames(11))//' not found="'//TRIM(cAlphaArgs(9))//'".')
          ErrorsFound=.TRUE.
        ENDIF
      ENDIF
      IF (CrossMixing(Loop)%MinOutdoorTempSchedPtr > 0) THEN
        ! Check min and max values in the schedule to ensure both values are within the range
        IF (.NOT.CheckScheduleValueMinMax(CrossMixing(Loop)%MinOutdoorTempSchedPtr,'>=',-MixingTempLimit,'<=',MixingTempLimit))THEN
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1)) // &
           ' must have a minimum outdoor temperature between -100C and 100C defined in the schedule = '//TRIM(cAlphaArgs(11)))
          ErrorsFound = .TRUE.
        END IF
      END IF
    END IF

    IF (NumAlpha > 11) THEN
      CrossMixing(Loop)%MaxOutdoorTempSchedPtr=GetScheduleIndex(cAlphaArgs(12))
      IF (CrossMixing(Loop)%MaxOutdoorTempSchedPtr == 0) THEN
        IF ((.NOT. lAlphaFieldBlanks(12))) THEN
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'",'//   &
                           trim(cAlphaFieldNames(12))//' not found="'//TRIM(cAlphaArgs(9))//'".')
          ErrorsFound=.TRUE.
        ENDIF
      ENDIF
      IF (CrossMixing(Loop)%MaxOutdoorTempSchedPtr > 0) THEN
        ! Check min and max values in the schedule to ensure both values are within the range
        IF (.NOT.CheckScheduleValueMinMax(CrossMixing(Loop)%MaxOutdoorTempSchedPtr,'>=',-MixingTempLimit,'<=',MixingTempLimit))THEN
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1)) // &
           ' must have a maximum outdoor temperature between -100C and 100C defined in the schedule = '//TRIM(cAlphaArgs(12)))
          ErrorsFound = .TRUE.
        END IF
      END IF
    END IF

    IF (CrossMixing(Loop)%ZonePtr > 0) THEN
      IF (RepVarSet(CrossMixing(Loop)%ZonePtr)) THEN
        RepVarSet(CrossMixing(Loop)%ZonePtr)=.FALSE.
        CALL SetupOutputVariable('Zone Mixing Volume [m3]',ZnAirRpt(CrossMixing(Loop)%ZonePtr)%MixVolume,  &
                                 'System','Sum',Zone(CrossMixing(Loop)%ZonePtr)%Name)
        CALL SetupOutputVariable('Zone Mixing Mass [kg]',ZnAirRpt(CrossMixing(Loop)%ZonePtr)%MixMass,  &
                                 'System','Sum',Zone(CrossMixing(Loop)%ZonePtr)%Name)
        CALL SetupOutputVariable('Zone Mixing Sensible Heat Loss Energy [J]',ZnAirRpt(CrossMixing(Loop)%ZonePtr)%MixHeatLoss,  &
                                 'System','Sum',Zone(CrossMixing(Loop)%ZonePtr)%Name)
        CALL SetupOutputVariable('Zone Mixing Sensible Heat Gain Energy [J]',ZnAirRpt(CrossMixing(Loop)%ZonePtr)%MixHeatGain,  &
                                 'System','Sum',Zone(CrossMixing(Loop)%ZonePtr)%Name)
        CALL SetupOutputVariable('Zone Mixing Latent Heat Loss Energy [J]',ZnAirRpt(CrossMixing(Loop)%ZonePtr)%MixLatentLoss,  &
                                 'System','Sum',Zone(CrossMixing(Loop)%ZonePtr)%Name)
        CALL SetupOutputVariable('Zone Mixing Latent Heat Gain Energy [J]',ZnAirRpt(CrossMixing(Loop)%ZonePtr)%MixLatentGain,  &
                                 'System','Sum',Zone(CrossMixing(Loop)%ZonePtr)%Name)
        CALL SetupOutputVariable('Zone Mixing Total Heat Loss Energy [J]',ZnAirRpt(CrossMixing(Loop)%ZonePtr)%MixTotalLoss,  &
                                 'System','Sum',Zone(CrossMixing(Loop)%ZonePtr)%Name)
        CALL SetupOutputVariable('Zone Mixing Total Heat Gain Energy [J]',ZnAirRpt(CrossMixing(Loop)%ZonePtr)%MixTotalGain,  &
                                 'System','Sum',Zone(CrossMixing(Loop)%ZonePtr)%Name)
      ENDIF
    ENDIF
    IF (AnyEnergyManagementSystemInModel) THEN
      CALL SetupEMSActuator('ZoneCrossMixing', CrossMixing(Loop)%Name, 'Air Exchange Flow Rate', '[m3/s]', &
                CrossMixing(Loop)%EMSSimpleMixingOn  , CrossMixing(Loop)%EMSimpleMixingFlowRate )
    ENDIF

  END DO

  ! Detect invalid Crossmixings
  IF (TotCrossMixing > 1 .and. .not. ErrorsFound) THEN
    ALLOCATE(SVals1(24,NumOfTimeStepInHour))
    SVals1=0.0d0
    ALLOCATE(SVals2(24,NumOfTimeStepInHour))
    SVals2=0.0d0
    ALLOCATE(OverLap(TotCrossMixing))
    OverLap = .FALSE.
    DO Loop=1,TotCrossMixing
      DO Loop1=1,TotCrossMixing

        IF (Loop == Loop1) CYCLE

        IF (CrossMixing(Loop)%ZonePtr == CrossMixing(Loop1)%FromZone .AND. &  ! Reciprocal cross mixing is OK
            CrossMixing(Loop)%FromZone == CrossMixing(Loop1)%ZonePtr) CYCLE

        IF ( .NOT. (CrossMixing(Loop)%ZonePtr == CrossMixing(Loop1)%ZonePtr .OR. &   ! Any other overlap is not OK
            CrossMixing(Loop)%ZonePtr == CrossMixing(Loop1)%FromZone .OR. &          ! if the schedules overlap
            CrossMixing(Loop)%FromZone == CrossMixing(Loop1)%FromZone .OR. &
            CrossMixing(Loop)%FromZone == CrossMixing(Loop1)%ZonePtr) ) CYCLE

        DO JDay=1,366
          CALL GetScheduleValuesForDay(CrossMixing(Loop)%SchedPtr,SVals1,JDay)
          IF (.not. ANY(SVals1>0.0d0)) CYCLE
          CALL GetScheduleValuesForDay(CrossMixing(Loop1)%SchedPtr,SVals2,JDay)
          IF (.not. ANY(SVals2>0.0d0)) CYCLE
          IF (OverLap(Loop) .AND. OverLap(Loop1)) CYCLE   ! Already problem for these Cross Mixings
  HrLoop: DO Hr=1,24
  TSLoop:   DO TS=1,NumOfTimeStepInHour
              IF (SVals1(Hr,TS) == 0.0d0 .or. SVals2(Hr,TS) == 0.0d0) CYCLE
              CALL ShowSevereError(RoutineName//'Overlapping Cross Mixings found')
              CALL ShowContinueError('Cross Mixing with receiving zone '// &
                                      TRIM(Zone(CrossMixing(Loop)%ZonePtr)%Name) // &
                                     ', source zone ' // TRIM(Zone(CrossMixing(Loop)%FromZone)%Name))
              CALL ShowContinueError('overlaps with Cross Mixing with receiving zone '// &
                                      TRIM(Zone(CrossMixing(Loop1)%ZonePtr)%Name) // &
                                     ', source zone ' // TRIM(Zone(CrossMixing(Loop1)%FromZone)%Name))
              OverLap(Loop) = .TRUE.
              OverLap(Loop1) = .TRUE.
              ErrorsFound=.TRUE.
              EXIT HrLoop
            ENDDO TSLoop
          ENDDO HrLoop
        ENDDO
      ENDDO
    ENDDO
    DEALLOCATE(SVals1)
    DEALLOCATE(SVals2)
    DEALLOCATE(OverLap)
  ENDIF

  cCurrentModuleObject='ZoneRefrigerationDoorMixing'
  TotRefDoorMixing=GetNumObjectsFound(cCurrentModuleObject)
  IF(TotRefDoorMixing > 0) THEN
    ALLOCATE(RefDoorMixing(NumofZones))
    RefDoorMixing%NumRefDoorConnections = 0

    DO Loop=1,TotRefDoorMixing

      CALL GetObjectItem(cCurrentModuleObject,Loop,cAlphaArgs,NumAlpha,rNumericArgs,NumNumber,IOStat, &
                       NumBlank=lNumericFieldBlanks,AlphaBlank=lAlphaFieldBlanks, &
                       AlphaFieldNames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

      IsNotOK = .FALSE.
      IsBlank = .FALSE.
      CALL VerifyName(cAlphaArgs(1),RefDoorMixing%Name,Loop-1,IsNotOK,IsBlank,TRIM(cCurrentModuleObject)//' Name')
      IF (IsNotOK) THEN
        ErrorsFound = .TRUE.
        IF (IsBlank) cAlphaArgs(1) = 'xxxxx'
      END IF
      NameThisObject = cAlphaArgs(1)

      AlphaNum = 2
      Zone1Num=FindIteminList(cAlphaArgs(AlphaNum),Zone%Name,NumOfZones)
      IF (Zone1Num == 0) THEN
        CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))// &
           '", invalid (not found) '//trim(cAlphaFieldNames(AlphaNum))//'="'//TRIM(cAlphaArgs(AlphaNum))//'".')
        ErrorsFound=.TRUE.
      ENDIF

      AlphaNum = AlphaNum + 1  !3
      Zone2Num=FindIteminList(cAlphaArgs(AlphaNum),Zone%Name,NumOfZones)
      IF (Zone2Num == 0) THEN
        CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))// &
           '", invalid (not found) '//trim(cAlphaFieldNames(AlphaNum))//'="'//TRIM(cAlphaArgs(AlphaNum))//'".')
        ErrorsFound=.TRUE.
      ENDIF
      IF(Zone1Num .EQ. Zone2Num)THEN
        CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'",'//   &
             ' The same zone name has been entered for both sides of a refrigerated door '// &
             TRIM(cAlphaFieldNames(AlphaNum))//'="'//trim(cAlphaArgs(AlphaNum))//'".')
        ErrorsFound=.TRUE.
      ELSE IF(zone1Num .LT. zone2Num)THEN !zone 1 will come first in soln loop, id zone 2 as mate zone
        ZoneNumA=Zone1Num
        ZoneNumB=Zone2Num
      ELSE IF(zone2Num .LT. zone1Num)THEN  !zone 2 will come first in soln loop, id zone 1 as mate zone
        ZoneNumA=Zone2Num
        ZoneNumB=Zone1Num
      END IF

      IF(.NOT. ALLOCATED(RefDoorMixing(ZoneNumA)%OpenSchedPtr)) THEN
        ALLOCATE(RefDoorMixing(ZoneNumA)%DoorMixingObjectName(NumofZones))
        ALLOCATE(RefDoorMixing(ZoneNumA)%OpenSchedPtr(NumofZones))
        ALLOCATE(RefDoorMixing(ZoneNumA)%DoorHeight(NumofZones))
        ALLOCATE(RefDoorMixing(ZoneNumA)%DoorArea(NumofZones))
        ALLOCATE(RefDoorMixing(ZoneNumA)%Protection(NumofZones))
        ALLOCATE(RefDoorMixing(ZoneNumA)%MateZonePtr(NumofZones))
        ALLOCATE(RefDoorMixing(ZoneNumA)%EMSRefDoorMixingOn(NumofZones))
        ALLOCATE(RefDoorMixing(ZoneNumA)%EMSRefDoorFlowRate(NumofZones))
        ALLOCATE(RefDoorMixing(ZoneNumA)%VolRefDoorFlowRate(NumofZones))
        ALLOCATE(RefDoorMixing(ZoneNumA)%DoorProtTypeName(NumofZones))
        RefDoorMixing(ZoneNumA)%DoorMixingObjectName = " "
        RefDoorMixing(ZoneNumA)%OpenSchedPtr = 0
        RefDoorMixing(ZoneNumA)%DoorHeight = 0.0d0
        RefDoorMixing(ZoneNumA)%DoorArea = 0.0d0
        RefDoorMixing(ZoneNumA)%Protection = RefDoorNone
        RefDoorMixing(ZoneNumA)%MateZonePtr = 0
        RefDoorMixing(ZoneNumA)%EMSRefDoorMixingOn = .FALSE.
        RefDoorMixing(ZoneNumA)%EMSRefDoorFlowRate = 0.0D0
        RefDoorMixing(ZoneNumA)%VolRefDoorFlowRate = 0.0D0
        RefDoorMixing(ZoneNumA)%DoorProtTypeName = " "
      END IF !First refrigeration mixing in this zone

      IF(.NOT. ALLOCATED(RefDoorMixing(ZoneNumB)%OpenSchedPtr)) THEN
        ALLOCATE(RefDoorMixing(ZoneNumB)%DoorMixingObjectName(NumofZones))
        ALLOCATE(RefDoorMixing(ZoneNumB)%OpenSchedPtr(NumofZones))
        ALLOCATE(RefDoorMixing(ZoneNumB)%DoorHeight(NumofZones))
        ALLOCATE(RefDoorMixing(ZoneNumB)%DoorArea(NumofZones))
        ALLOCATE(RefDoorMixing(ZoneNumB)%Protection(NumofZones))
        ALLOCATE(RefDoorMixing(ZoneNumB)%MateZonePtr(NumofZones))
        ALLOCATE(RefDoorMixing(ZoneNumB)%EMSRefDoorMixingOn(NumofZones))
        ALLOCATE(RefDoorMixing(ZoneNumB)%EMSRefDoorFlowRate(NumofZones))
        ALLOCATE(RefDoorMixing(ZoneNumB)%VolRefDoorFlowRate(NumofZones))
        ALLOCATE(RefDoorMixing(ZoneNumB)%DoorProtTypeName(NumofZones))
        RefDoorMixing(ZoneNumB)%DoorMixingObjectName = " "
        RefDoorMixing(ZoneNumB)%OpenSchedPtr = 0
        RefDoorMixing(ZoneNumB)%DoorHeight = 0.0d0
        RefDoorMixing(ZoneNumB)%DoorArea = 0.0d0
        RefDoorMixing(ZoneNumB)%Protection = RefDoorNone
        RefDoorMixing(ZoneNumB)%MateZonePtr = 0
        RefDoorMixing(ZoneNumB)%EMSRefDoorMixingOn = .FALSE.
        RefDoorMixing(ZoneNumB)%EMSRefDoorFlowRate = 0.0D0
        RefDoorMixing(ZoneNumB)%VolRefDoorFlowRate = 0.0D0
        RefDoorMixing(ZoneNumB)%DoorProtTypeName = " "
      END IF !First refrigeration mixing in this zone

      ConnectionNumber = RefDoorMixing(ZoneNumA)%NumRefDoorConnections +1
      RefDoorMixing(ZoneNumA)%NumRefDoorConnections        = ConnectionNumber
      RefDoorMixing(ZoneNumA)%ZonePtr                      = ZoneNumA
      RefDoorMixing(ZoneNumA)%MateZonePtr(ConnectionNumber)= ZoneNumB
      RefDoorMixing(ZoneNumA)%DoorMixingObjectName(ConnectionNumber) = NameThisObject
      !need to make sure same pair of zones is only entered once.
      IF(RefDoorMixing(ZoneNumA)%RefDoorMixFlag .AND. RefDoorMixing(ZoneNumB)%RefDoorMixFlag) THEN
        IF(RefDoorMixing(ZoneNumA)%NumRefDoorConnections > 1) THEN
          DO ConnectTest = 1,(ConnectionNumber-1)
            IF(RefDoorMixing(ZoneNumA)%MateZonePtr(ConnectTest) .NE. &
               RefDoorMixing(ZoneNumA)%MateZonePtr(ConnectionNumber))CYCLE
            CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'",'//   &
                 ' and '//TRIM(RefDoorMixing(ZoneNumA)%DoorMixingObjectName(ConnectTest)))
            CALL ShowContinueError(' Share same pair of zones: "'//TRIM(Zone(ZoneNumA)%Name)//'" and "'// &
                 TRIM(Zone(ZoneNumB)%Name)// &
                 '". Only one RefrigerationDoorMixing object is allowed for any unique pair of zones.')
            ErrorsFound = .TRUE.
          END DO !ConnectTest
        END IF   !NumRefDoorconnections > 1
      ELSE       !Both zones need to be flagged with ref doors
        RefDoorMixing(ZoneNumA)%RefDoorMixFlag = .TRUE.
        RefDoorMixing(ZoneNumB)%RefDoorMixFlag = .TRUE.
      END IF     !Both zones already flagged with ref doors

      AlphaNum = AlphaNum + 1  !4
      IF (lAlphaFieldBlanks(AlphaNum)) THEN
        CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'",'//   &
             TRIM(cAlphaFieldNames(AlphaNum))//' is required but field is blank.')
        ErrorsFound=.TRUE.
      ELSE !(lAlphaFieldBlanks(AlphaNum)) THEN
        RefDoorMixing(ZoneNumA)%OpenSchedPtr(ConnectionNumber)=GetScheduleIndex(cAlphaArgs(AlphaNum))
        IF (RefDoorMixing(ZoneNumA)%OpenSchedPtr(ConnectionNumber) == 0) THEN
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'",'//   &
             ' invalid (not found) '//TRIM(cAlphaFieldNames(AlphaNum))//'="'//trim(cAlphaArgs(AlphaNum))//'".')
          ErrorsFound=.TRUE.
        ELSE !OpenSchedPtr(ConnectionNumber) ne 0)
          IF (.NOT. CheckScheduleValueMinMax(RefDoorMixing(ZoneNumA)%OpenSchedPtr(ConnectionNumber),'>=',0.0d0,'<=',1.0d0)) THEN
            CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'",'//   &
               TRIM(cAlphaFieldNames(AlphaNum))//'="'//trim(cAlphaArgs(AlphaNum))//'" has schedule values < 0 or > 1.')
            ErrorsFound=.TRUE.
          END IF ! check door opening schedule values between 0 and 1
        ENDIF !OpenSchedPtr(ConnectionNumber) == 0)
      ENDIF !(lAlphaFieldBlanks(AlphaNum)) THEN

      NumbNum = 1
      IF (lNumericFieldBlanks(NumbNum)) THEN
        RefDoorMixing(ZoneNumA)%DoorHeight(ConnectionNumber)= 3.0d0 ! default height of 3 meters
        CALL ShowWarningError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
            TRIM(cNumericFieldNames(NumbNum))//  &
            ' is blank and the default value of 3.0 will be used.')
      ELSE
        RefDoorMixing(ZoneNumA)%DoorHeight(ConnectionNumber)=rNumericArgs(NumbNum)
        IF ((RefDoorMixing(ZoneNumA)%DoorHeight(ConnectionNumber) < 0) .OR. &
          (RefDoorMixing(ZoneNumA)%DoorHeight(ConnectionNumber) > 50.0d0 )) THEN
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1)) // &
           ' must have a door height between 0 and 50 meters. ')
          ErrorsFound = .TRUE.
        END IF
      END IF

      NumbNum = NumbNum + 1  !2
      IF (lNumericFieldBlanks(NumbNum)) THEN
        RefDoorMixing(ZoneNumA)%DoorArea(ConnectionNumber)=9.0d0 ! default area of 9 m2
        CALL ShowWarningError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
            TRIM(cNumericFieldNames(NumbNum))//  &
            ' is blank and the default value of 9 m2 will be used.')
      ELSE
        RefDoorMixing(ZoneNumA)%DoorArea(ConnectionNumber)=rNumericArgs(NumbNum)
        IF ((RefDoorMixing(ZoneNumA)%DoorArea(ConnectionNumber) < 0) .OR. &
          (RefDoorMixing(ZoneNumA)%DoorArea(ConnectionNumber) > 400.0d0 )) THEN
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1)) // &
           ' must have a door height between 0 and 400 square meters. ')
          ErrorsFound = .TRUE.
        END IF
      END IF

      AlphaNum = AlphaNum + 1  !5
      ! Door protection type.
      IF (lAlphaFieldBlanks(AlphaNum)) THEN
        RefDoorMixing(ZoneNumA)%Protection(ConnectionNumber)=RefDoorNone !Default
        RefDoorMixing(ZoneNumA)%DoorProtTypeName(ConnectionNumber)="None" !Default
        CALL ShowWarningError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
              '"  '//TRIM(cAlphaFieldNames(AlphaNum))//' is blank. Default of no door protection will be used')
      ELSE
        IF    (cAlphaArgs(AlphaNum) .EQ. 'NONE') THEN
            RefDoorMixing(ZoneNumA)%Protection(ConnectionNumber)=RefDoorNone
            RefDoorMixing(ZoneNumA)%DoorProtTypeName(ConnectionNumber)="None"
        ELSEIF(cAlphaArgs(AlphaNum) .EQ.'AIRCURTAIN') THEN
            RefDoorMixing(ZoneNumA)%Protection(ConnectionNumber)=RefDoorAirCurtain
            RefDoorMixing(ZoneNumA)%DoorProtTypeName(ConnectionNumber)="AirCurtain"
        ELSEIF(cAlphaArgs(AlphaNum) .EQ.'STRIPCURTAIN') THEN
            RefDoorMixing(ZoneNumA)%Protection(ConnectionNumber)=RefDoorStripCurtain
            RefDoorMixing(ZoneNumA)%DoorProtTypeName(ConnectionNumber)="StripCurtain"
        ELSE
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
                             '", invalid calculation method='//   &
                             TRIM(cAlphaArgs(AlphaNum))//' with alphanum of 5: '// &
                             TRIM(cAlphaArgs(5)))
          ErrorsFound=.TRUE.
        END IF ! =none, etc.
       END IF !Blank

    IF (ZoneNumA > 0) THEN
      IF (RepVarSet(ZoneNumA)) THEN
        RepVarSet(ZoneNumA)=.FALSE.
        CALL SetupOutputVariable('Zone Mixing Volume [m3]',ZnAirRpt(ZoneNumA)%MixVolume,  &
                                 'System','Sum',Zone(ZoneNumA)%Name)
        CALL SetupOutputVariable('Zone Mixing Mass [kg]',ZnAirRpt(ZoneNumA)%MixMass,  &
                                 'System','Sum',Zone(ZoneNumA)%Name)
        CALL SetupOutputVariable('Zone Mixing Sensible Heat Loss Energy [J]',ZnAirRpt(ZoneNumA)%MixHeatLoss,  &
                                 'System','Sum',Zone(ZoneNumA)%Name)
        CALL SetupOutputVariable('Zone Mixing Sensible Heat Gain Energy [J]',ZnAirRpt(ZoneNumA)%MixHeatGain,  &
                                 'System','Sum',Zone(ZoneNumA)%Name)
        CALL SetupOutputVariable('Zone Mixing Latent Heat Loss Energy [J]',ZnAirRpt(ZoneNumA)%MixLatentLoss,  &
                                 'System','Sum',Zone(ZoneNumA)%Name)
        CALL SetupOutputVariable('Zone Mixing Latent Heat Gain Energy [J]',ZnAirRpt(ZoneNumA)%MixLatentGain,  &
                                 'System','Sum',Zone(ZoneNumA)%Name)
        CALL SetupOutputVariable('Zone Mixing Total Heat Loss Energy [J]',ZnAirRpt(ZoneNumA)%MixTotalLoss,  &
                                 'System','Sum',Zone(ZoneNumA)%Name)
        CALL SetupOutputVariable('Zone Mixing Total Heat Gain Energy [J]',ZnAirRpt(ZoneNumA)%MixTotalGain,  &
                                 'System','Sum',Zone(ZoneNumA)%Name)
      ENDIF
    ENDIF
    IF (AnyEnergyManagementSystemInModel) THEN
      CALL SetupEMSActuator('ZoneRefDoorMixing', RefDoorMixing(ZoneNumA)%Name, 'Air Exchange Flow Rate', '[m3/s]', &
                RefDoorMixing(ZoneNumA)%EMSRefDoorMixingOn(ConnectionNumber), &
                RefDoorMixing(ZoneNumA)%EMSRefDoorFlowRate(ConnectionNumber) )
    ENDIF

    IF (ZoneNumB > 0) THEN
      IF (RepVarSet(ZoneNumB)) THEN
        RepVarSet(ZoneNumB)=.FALSE.
        CALL SetupOutputVariable('Zone Mixing Volume [m3]',ZnAirRpt(ZoneNumB)%MixVolume,  &
                                 'System','Sum',Zone(ZoneNumB)%Name)
        CALL SetupOutputVariable('Zone Mixing Mass [kg]',ZnAirRpt(ZoneNumB)%MixMass,  &
                                 'System','Sum',Zone(ZoneNumB)%Name)
        CALL SetupOutputVariable('Zone Mixing Sensible Heat Loss Energy [J]',ZnAirRpt(ZoneNumB)%MixHeatLoss,  &
                                 'System','Sum',Zone(ZoneNumB)%Name)
        CALL SetupOutputVariable('Zone Mixing Sensible Heat Gain Energy [J]',ZnAirRpt(ZoneNumB)%MixHeatGain,  &
                                 'System','Sum',Zone(ZoneNumB)%Name)
        CALL SetupOutputVariable('Zone Mixing Latent Heat Loss Energy [J]',ZnAirRpt(ZoneNumB)%MixLatentLoss,  &
                                 'System','Sum',Zone(ZoneNumB)%Name)
        CALL SetupOutputVariable('Zone Mixing Latent Heat Gain Energy [J]',ZnAirRpt(ZoneNumB)%MixLatentGain,  &
                                 'System','Sum',Zone(ZoneNumB)%Name)
        CALL SetupOutputVariable('Zone Mixing Total Heat Loss Energy [J]',ZnAirRpt(ZoneNumB)%MixTotalLoss,  &
                                 'System','Sum',Zone(ZoneNumB)%Name)
        CALL SetupOutputVariable('Zone Mixing Total Heat Gain Energy [J]',ZnAirRpt(ZoneNumB)%MixTotalGain,  &
                                 'System','Sum',Zone(ZoneNumB)%Name)
      ENDIF
    ENDIF
    IF (AnyEnergyManagementSystemInModel) THEN
      CALL SetupEMSActuator('ZoneRefDoorMixing', RefDoorMixing(ZoneNumB)%Name, 'Air Exchange Flow Rate', '[m3/s]', &
                RefDoorMixing(ZoneNumA)%EMSRefDoorMixingOn(ConnectionNumber), &
                RefDoorMixing(ZoneNumA)%EMSRefDoorFlowRate(ConnectionNumber) )
    ENDIF

  END DO !DO Loop=1,TotRefDoorMixing
END IF !TotRefDoorMixing > 0)

  DEALLOCATE(RepVarSet)
  DEALLOCATE(cAlphaArgs)
  DEALLOCATE(cAlphaFieldNames)
  DEALLOCATE(cNumericFieldNames)
  DEALLOCATE(rNumericArgs)
  DEALLOCATE(lAlphaFieldBlanks)
  DEALLOCATE(lNumericFieldBlanks)


  ALLOCATE(TotInfilVentFlow(NumOfZones))
  TotInfilVentFlow=0.0d0

  DO Loop=1,TotInfiltration
    IF (Loop == 1) WRITE(OutputFileInits,721) 'Infiltration','Design Volume Flow Rate {m3/s},'//  &
       'Volume Flow Rate/Floor Area {m3/s/m2},Volume Flow Rate/Exterior Surface Area {m3/s/m2},'//  &
       'ACH - Air Changes per Hour,Equation A - Constant Term Coefficient {},'//   &
       'Equation B - Temperature Term Coefficient {1/C},'//  &
       'Equation C - Velocity Term Coefficient {s/m}, Equation D - Velocity Squared Term Coefficient {s2/m2}'

    ZoneNum=Infiltration(Loop)%ZonePtr
    IF (ZoneNum == 0) THEN
      WRITE(OutputFileInits,722) 'Infiltration-Illegal Zone specified',TRIM(Infiltration(Loop)%Name)
      CYCLE
    ENDIF
    TotInfilVentFlow(ZoneNum)=TotInfilVentFlow(ZoneNum)+Infiltration(Loop)%DesignLevel
    WRITE(OutputFileInits,720,advance='No') 'ZoneInfiltration',TRIM(Infiltration(Loop)%Name),  &
       TRIM(GetScheduleName(Infiltration(Loop)%SchedPtr)),  &
       TRIM(Zone(ZoneNum)%Name),TRIM(RoundSigDigits(Zone(ZoneNum)%FloorArea,2)),TRIM(RoundSigDigits(Zone(ZoneNum)%TotOccupants,1))
    StringOut=RoundSigDigits(Infiltration(Loop)%DesignLevel,3)
    WRITE(OutputFileInits,fmta,advance='No') TRIM(StringOut)//','
    IF (Zone(ZoneNum)%FloorArea > 0.0d0) THEN
      StringOut=RoundSigDigits(Infiltration(Loop)%DesignLevel/Zone(ZoneNum)%FloorArea,3)
    ELSE
      StringOut='N/A'
    ENDIF
    WRITE(OutputFileInits,fmta,advance='No') TRIM(StringOut)//','
    IF (Zone(ZoneNum)%ExteriorTotalSurfArea > 0.0d0) THEN
      StringOut=RoundSigDigits(Infiltration(Loop)%DesignLevel/Zone(ZoneNum)%ExteriorTotalSurfArea,3)
    ELSE
      StringOut='N/A'
    ENDIF
    WRITE(OutputFileInits,fmta,advance='No') TRIM(StringOut)//','
    IF (Zone(ZoneNum)%Volume > 0.0d0) THEN
      StringOut=RoundSigDigits(Infiltration(Loop)%DesignLevel*SecInHour/Zone(ZoneNum)%Volume,3)
    ELSE
      StringOut='N/A'
    ENDIF
    WRITE(OutputFileInits,fmta,advance='No') TRIM(StringOut)//','
    StringOut=RoundSigDigits(Infiltration(Loop)%ConstantTermCoef,3)
    WRITE(OutputFileInits,fmta,advance='No') TRIM(StringOut)//','
    StringOut=RoundSigDigits(Infiltration(Loop)%TemperatureTermCoef,3)
    WRITE(OutputFileInits,fmta,advance='No') TRIM(StringOut)//','
    StringOut=RoundSigDigits(Infiltration(Loop)%VelocityTermCoef,3)
    WRITE(OutputFileInits,fmta,advance='No') TRIM(StringOut)//','
    StringOut=RoundSigDigits(Infiltration(Loop)%VelocitySQTermCoef,3)
    WRITE(OutputFileInits,fmta) TRIM(StringOut)
  ENDDO

  DO Loop=1,TotVentilation
    IF (Loop == 1) WRITE(OutputFileInits,721) 'ZoneVentilation','Design Volume Flow Rate {m3/s},'//  &
       'Volume Flow Rate/Floor Area {m3/s/m2},Volume Flow Rate/person Area {m3/s/person},'//  &
       'ACH - Air Changes per Hour,Fan Type {Exhaust;Intake;Natural},Fan Pressure Rise {Pa},'//  &
       'Fan Efficiency {},Equation A - Constant Term Coefficient {},'//   &
       'Equation B - Temperature Term Coefficient {1/C},'//  &
       'Equation C - Velocity Term Coefficient {s/m}, Equation D - Velocity Squared Term Coefficient {s2/m2},'//  &
       'Minimum Indoor Temperature{C}/Schedule,Maximum Indoor Temperature{C}/Schedule,Delta Temperature{C}/Schedule,'//    &
       'Minimum Outdoor Temperature{C}/Schedule,Maximum Outdoor Temperature{C}/Schedule,Maximum WindSpeed{m/s}'

    ZoneNum=Ventilation(Loop)%ZonePtr
    IF (ZoneNum == 0) THEN
      WRITE(OutputFileInits,722) 'Ventilation-Illegal Zone specified',TRIM(Ventilation(Loop)%Name)
      CYCLE
    ENDIF
    TotInfilVentFlow(ZoneNum)=TotInfilVentFlow(ZoneNum)+Ventilation(Loop)%DesignLevel
    WRITE(OutputFileInits,720,advance='No') 'ZoneVentilation',TRIM(Ventilation(Loop)%Name),  &
       TRIM(GetScheduleName(Ventilation(Loop)%SchedPtr)),  &
       TRIM(Zone(ZoneNum)%Name),TRIM(RoundSigDigits(Zone(ZoneNum)%FloorArea,2)),TRIM(RoundSigDigits(Zone(ZoneNum)%TotOccupants,1))
    StringOut=RoundSigDigits(Ventilation(Loop)%DesignLevel,3)
    WRITE(OutputFileInits,fmta,advance='No') TRIM(StringOut)//','
    IF (Zone(ZoneNum)%FloorArea > 0.0d0) THEN
      StringOut=RoundSigDigits(Ventilation(Loop)%DesignLevel/Zone(ZoneNum)%FloorArea,3)
    ELSE
      StringOut='N/A'
    ENDIF
    WRITE(OutputFileInits,fmta,advance='No') TRIM(StringOut)//','
    IF (Zone(ZoneNum)%TotOccupants > 0.0d0) THEN
      StringOut=RoundSigDigits(Ventilation(Loop)%DesignLevel/(Zone(ZoneNum)%TotOccupants),3)
    ELSE
      StringOut='N/A'
    ENDIF
    WRITE(OutputFileInits,fmta,advance='No') TRIM(StringOut)//','
    IF (Zone(ZoneNum)%Volume > 0.0d0) THEN
      StringOut=RoundSigDigits(Ventilation(Loop)%DesignLevel*SecInHour/Zone(ZoneNum)%Volume,3)
    ELSE
      StringOut='N/A'
    ENDIF
    WRITE(OutputFileInits,fmta,advance='No') TRIM(StringOut)//','
    IF (Ventilation(Loop)%FanType == ExhaustVentilation) THEN
      StringOut='Exhaust'
    ELSEIF (Ventilation(Loop)%FanType == IntakeVentilation) THEN
      StringOut='Intake'
    ELSEIF (Ventilation(Loop)%FanType == NaturalVentilation) THEN
      StringOut='Natural'
    ELSEIF (Ventilation(Loop)%FanType == BalancedVentilation) THEN
      StringOut='Balanced'
    ENDIF
    WRITE(OutputFileInits,fmta,advance='No') TRIM(StringOut)//','
    StringOut=RoundSigDigits(Ventilation(Loop)%FanPressure,3)
    WRITE(OutputFileInits,fmta,advance='No') TRIM(StringOut)//','
    StringOut=RoundSigDigits(Ventilation(Loop)%FanEfficiency,1)
    WRITE(OutputFileInits,fmta,advance='No') TRIM(StringOut)//','
    StringOut=RoundSigDigits(Ventilation(Loop)%ConstantTermCoef,3)
    WRITE(OutputFileInits,fmta,advance='No') TRIM(StringOut)//','
    StringOut=RoundSigDigits(Ventilation(Loop)%TemperatureTermCoef,3)
    WRITE(OutputFileInits,fmta,advance='No') TRIM(StringOut)//','
    StringOut=RoundSigDigits(Ventilation(Loop)%VelocityTermCoef,3)
    WRITE(OutputFileInits,fmta,advance='No') TRIM(StringOut)//','
    StringOut=RoundSigDigits(Ventilation(Loop)%VelocitySQTermCoef,3)
    WRITE(OutputFileInits,fmta,advance='No') TRIM(StringOut)//','
    IF (Ventilation(Loop)%MinIndoorTempSchedPtr > 0) THEN
      StringOut=GetScheduleName(Ventilation(Loop)%MinIndoorTempSchedPtr)
    ELSE
      StringOut=RoundSigDigits(Ventilation(Loop)%MinIndoorTemperature,2)
    ENDIF
    WRITE(OutputFileInits,fmta,advance='No') TRIM(StringOut)//','
    IF (Ventilation(Loop)%MaxIndoorTempSchedPtr > 0) THEN
      StringOut='Schedule: '//GetScheduleName(Ventilation(Loop)%MaxIndoorTempSchedPtr)
    ELSE
      StringOut=RoundSigDigits(Ventilation(Loop)%MaxIndoorTemperature,2)
    ENDIF
    WRITE(OutputFileInits,fmta,advance='No') TRIM(StringOut)//','
    IF (Ventilation(Loop)%DeltaTempSchedPtr > 0) THEN
      StringOut='Schedule: '//GetScheduleName(Ventilation(Loop)%DeltaTempSchedPtr)
    ELSE
      StringOut=RoundSigDigits(Ventilation(Loop)%DelTemperature,2)
    ENDIF
    WRITE(OutputFileInits,fmta,advance='No') TRIM(StringOut)//','
    IF (Ventilation(Loop)%MinOutdoorTempSchedPtr > 0) THEN
      StringOut='Schedule: '//GetScheduleName(Ventilation(Loop)%MinOutdoorTempSchedPtr)
    ELSE
      StringOut=RoundSigDigits(Ventilation(Loop)%MinOutdoorTemperature,2)
    ENDIF
    WRITE(OutputFileInits,fmta,advance='No') TRIM(StringOut)//','
    IF (Ventilation(Loop)%MaxOutdoorTempSchedPtr > 0) THEN
      StringOut='Schedule: '//GetScheduleName(Ventilation(Loop)%MaxOutdoorTempSchedPtr)
    ELSE
      StringOut=RoundSigDigits(Ventilation(Loop)%MaxOutdoorTemperature,2)
    ENDIF
    WRITE(OutputFileInits,fmta,advance='No') TRIM(StringOut)//','
    StringOut=RoundSigDigits(Ventilation(Loop)%MaxWindSpeed,2)
    WRITE(OutputFileInits,fmta) TRIM(StringOut)
  ENDDO

  ALLOCATE(TotMixingFlow(NumOfZones))
  TotMixingFlow=0.0d0
  DO Loop=1,TotMixing
    IF (Loop == 1) WRITE(OutputFileInits,721) 'Mixing','Design Volume Flow Rate {m3/s},'//  &
       'Volume Flow Rate/Floor Area {m3/s/m2},Volume Flow Rate/person Area {m3/s/person},'//  &
       'ACH - Air Changes per Hour,From/Source Zone,Delta Temperature {C}'

    ZoneNum=Mixing(Loop)%ZonePtr
    IF (ZoneNum == 0) THEN
      WRITE(OutputFileInits,722) 'Mixing-Illegal Zone specified',TRIM(Mixing(Loop)%Name)
      CYCLE
    ENDIF
    TotMixingFlow(ZoneNum)=TotMixingFlow(ZoneNum)+Mixing(Loop)%DesignLevel
    WRITE(OutputFileInits,720,advance='No') 'Mixing',TRIM(Mixing(Loop)%Name),  &
       TRIM(GetScheduleName(Mixing(Loop)%SchedPtr)),  &
       TRIM(Zone(ZoneNum)%Name),TRIM(RoundSigDigits(Zone(ZoneNum)%FloorArea,2)),TRIM(RoundSigDigits(Zone(ZoneNum)%TotOccupants,1))
    StringOut=RoundSigDigits(Mixing(Loop)%DesignLevel,3)
    WRITE(OutputFileInits,fmta,advance='No') TRIM(StringOut)//','
    IF (Zone(ZoneNum)%FloorArea > 0.0d0) THEN
      StringOut=RoundSigDigits(Mixing(Loop)%DesignLevel/Zone(ZoneNum)%FloorArea,3)
    ELSE
      StringOut='N/A'
    ENDIF
    WRITE(OutputFileInits,fmta,advance='No') TRIM(StringOut)//','
    IF (Zone(ZoneNum)%TotOccupants > 0.0d0) THEN
      StringOut=RoundSigDigits(Mixing(Loop)%DesignLevel/(Zone(ZoneNum)%TotOccupants),3)
    ELSE
      StringOut='N/A'
    ENDIF
    WRITE(OutputFileInits,fmta,advance='No') TRIM(StringOut)//','
    IF (Zone(ZoneNum)%Volume > 0.0d0) THEN
      StringOut=RoundSigDigits(Mixing(Loop)%DesignLevel*SecInHour/Zone(ZoneNum)%Volume,3)
    ELSE
      StringOut='N/A'
    ENDIF
    WRITE(OutputFileInits,fmta,advance='No') TRIM(StringOut)//','
    WRITE(OutputFileInits,fmta,advance='No') TRIM(Zone(Mixing(Loop)%FromZone)%Name)//','
    StringOut=RoundSigDigits(Mixing(Loop)%DeltaTemperature,2)
    WRITE(OutputFileInits,fmta) TRIM(StringOut)
  ENDDO

  DO Loop=1,TotCrossMixing
    IF (Loop == 1) WRITE(OutputFileInits,721) 'CrossMixing','Design Volume Flow Rate {m3/s},'//  &
       'Volume Flow Rate/Floor Area {m3/s/m2},Volume Flow Rate/person Area {m3/s/person},'//  &
       'ACH - Air Changes per Hour,From/Source Zone,Delta Temperature {C}'

    ZoneNum=CrossMixing(Loop)%ZonePtr
    IF (ZoneNum == 0) THEN
      WRITE(OutputFileInits,722) 'CrossMixing-Illegal Zone specified',TRIM(CrossMixing(Loop)%Name)
      CYCLE
    ENDIF
    TotMixingFlow(ZoneNum)=TotMixingFlow(ZoneNum)+CrossMixing(Loop)%DesignLevel
    WRITE(OutputFileInits,720,advance='No') 'CrossMixing',TRIM(CrossMixing(Loop)%Name),  &
       TRIM(GetScheduleName(CrossMixing(Loop)%SchedPtr)),  &
       TRIM(Zone(ZoneNum)%Name),TRIM(RoundSigDigits(Zone(ZoneNum)%FloorArea,2)),TRIM(RoundSigDigits(Zone(ZoneNum)%TotOccupants,1))
    StringOut=RoundSigDigits(CrossMixing(Loop)%DesignLevel,3)
    WRITE(OutputFileInits,fmta,advance='No') TRIM(StringOut)//','
    IF (Zone(ZoneNum)%FloorArea > 0.0d0) THEN
      StringOut=RoundSigDigits(CrossMixing(Loop)%DesignLevel/Zone(ZoneNum)%FloorArea,3)
    ELSE
      StringOut='N/A'
    ENDIF
    WRITE(OutputFileInits,fmta,advance='No') TRIM(StringOut)//','
    IF (Zone(ZoneNum)%TotOccupants > 0.0d0) THEN
      StringOut=RoundSigDigits(CrossMixing(Loop)%DesignLevel/(Zone(ZoneNum)%TotOccupants),3)
    ELSE
      StringOut='N/A'
    ENDIF
    WRITE(OutputFileInits,fmta,advance='No') TRIM(StringOut)//','
    IF (Zone(ZoneNum)%Volume > 0.0d0) THEN
      StringOut=RoundSigDigits(CrossMixing(Loop)%DesignLevel*SecInHour/Zone(ZoneNum)%Volume,3)
    ELSE
      StringOut='N/A'
    ENDIF
    WRITE(OutputFileInits,fmta,advance='No') TRIM(StringOut)//','
    WRITE(OutputFileInits,fmta,advance='No') TRIM(Zone(CrossMixing(Loop)%FromZone)%Name)//','
    StringOut=RoundSigDigits(CrossMixing(Loop)%DeltaTemperature,2)
    WRITE(OutputFileInits,fmta) TRIM(StringOut)
  ENDDO

  IF (TotRefDoorMixing .GT. 0) THEN
    WRITE(OutputFileInits,724) 'RefrigerationDoorMixing ','Name, Zone 1 Name,'//  &
       'Zone 2 Name,Door Opening Schedule Name,'//  &
       'Door Height {m},Door Area {m2},Door Protection Type'
    DO ZoneNumA = 1,(NumOfZones - 1)
      IF(.NOT. RefDoorMixing(ZoneNumA)%RefDoorMixFlag)CYCLE
      DO ConnectionNumber=1,RefDoorMixing(ZoneNumA)%NumRefDoorConnections
        ZoneNumB=RefDoorMixing(ZoneNumA)%MateZonePtr(ConnectionNumber)
        !TotMixingFlow(ZoneNum)=TotMixingFlow(ZoneNum)+RefDoorMixing(Loop)%!DesignLevel
        WRITE(OutputFileInits,723) 'RefrigerationDoorMixing', &
              TRIM(RefDoorMixing(ZoneNumA)%DoorMixingObjectName(ConnectionNumber)),  &
              TRIM(Zone(ZoneNumA)%Name), TRIM(Zone(ZoneNumB)%Name), &
              TRIM(GetScheduleName(RefDoorMixing(ZoneNumA)%OpenSchedPtr(ConnectionNumber))),  &
              TRIM(RoundSigDigits(RefDoorMixing(ZoneNumA)%DoorHeight(ConnectionNumber),3)), &
              TRIM(RoundSigDigits(RefDoorMixing(ZoneNumA)%DoorArea(ConnectionNumber),3)), &
              TRIM(RefDoorMixing(ZoneNumA)%DoorProtTypeName(ConnectionNumber))
      END DO ! ConnectionNumber
    ENDDO ! ZoneNumA
  END IF !(TotRefDoorMixing .GT. 0)

  DO ZoneNum=1,NumOfZones
    Zone(ZoneNum)%NominalInfilVent=TotInfilVentFlow(ZoneNum)
    Zone(ZoneNum)%NominalMixing=TotMixingFlow(ZoneNum)
  ENDDO

  DEALLOCATE(TotInfilVentFlow)
  DEALLOCATE(TotMixingFlow)
720 FORMAT(' ',A,' Airflow Stats, ',A,',',A,',',A,',',A,',',A,',')
721 FORMAT('! <',A,' Airflow Stats - Nominal>,Name,Schedule Name,Zone Name, Zone Floor Area {m2}, # Zone Occupants,',A)
!           ' Area per Occupant {m2/person}, Occupant per Area {person/m2}, Interior Lighting {W/m2}, ',  &
!           'Electric Load {W/m2}, Gas Load {W/m2}, Other Load {W/m2}, Hot Water Eq {W/m2}, Outdoor Controlled Baseboard Heat')
722 FORMAT(' ',A,', ',A)
723 FORMAT(' ',A,' Airflow Stats, ',A,',',A,',',A,',',A,',',A,',',A,',',A)
724 FORMAT('! <',A,' Airflow Stats - Nominal>, ',A)

  RETURN

END SUBROUTINE GetSimpleAirModelInputs

!*****************************************************************************************
! This subroutine was moved from 'RoomAirManager' Module

SUBROUTINE  GetRoomAirModelParameters(ErrFlag)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Brent Griffith
          !       DATE WRITTEN   August 2001
          !       MODIFIED       na
          !       RE-ENGINEERED  April 2003, Weixiu Kong
          !                      December 2003, CC

          ! PURPOSE OF THIS SUBROUTINE:
          !     Get room air model parameters for all zones at once

          ! METHODOLOGY EMPLOYED:
          !     Use input processer to get input from idf file

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
    USE InputProcessor,             ONLY : GetNumObjectsFound, GetObjectItem, FindItemInList
    USE DataIPShortCuts
    USE DataGlobals,                ONLY : NumOfZones, MaxNameLength
    USE DataInterfaces,             ONLY : ShowWarningError
    USE DataHeatBalance,            ONLY : Zone
    USE DataRoomAirModel,           ONLY : AirModel, RoomAirModel_Mixing, RoomAirModel_Mundt, RoomAirModel_UCSDDV, &
                                           RoomAirModel_UCSDCV, DirectCoupling, IndirectCoupling, MundtModelUsed,   &
                                           UCSDModelUsed, ChAirModel, UserDefinedUsed, RoomAirModel_UserDefined,   &
                                           RoomAirModel_UCSDUFI, RoomAirModel_UCSDUFE

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
    LOGICAL, INTENT(INOUT) :: ErrFlag ! True if errors found during this input routine

          ! SUBROUTINE PARAMETER DEFINITIONS:
    CHARACTER(Len=*), PARAMETER :: Blank = ' '
    CHARACTER(len=*), PARAMETER :: RoomAirHeader=  &
                       "('! <RoomAir Model>, Zone Name, Mixing/Mundt/UCSDDV/UCSDCV/UCSDUFI/UCSDUFE/User Defined')"
    CHARACTER(len=*), PARAMETER :: RoomAirZoneFmt="('RoomAir Model,',A,',',A)"

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    INTEGER                                     :: NumAlphas    ! States which alpha value to read from a
                                                                ! "Number" line
    INTEGER                                     :: NumNumbers   ! Number of numbers encountered
    INTEGER                                     :: Status       ! Notes if there was an error in processing the input
    INTEGER                                     :: AirModelNum
    INTEGER                                     :: NumOfAirModels
    INTEGER                                     :: ZoneNum
    LOGICAL                                     :: ErrorsFound
    LOGICAL                                     :: IsNotOK

          ! FLOW:

    ! Initialize default values for air model parameters
    ALLOCATE (AirModel(NumOfZones))

    ErrorsFound=.FALSE.

    cCurrentModuleObject = 'RoomAirModelType'
    NumOfAirModels = GetNumObjectsFound(cCurrentModuleObject)
    IF (NumOfAirModels.GT.NumOfZones) THEN
      CALL ShowSevereError('Too many '//TRIM(cCurrentModuleObject)//'.  Cannot exceed the number of Zones.')
      ErrorsFound=.TRUE.
    END IF


    DO AirModelNum=1,NumOfAirModels
      CALL GetObjectItem(cCurrentModuleObject,AirModelNum,cAlphaArgs,NumAlphas,rNumericArgs,NumNumbers,Status, &
                         AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
      ZoneNum=FindItemInList(cAlphaArgs(2),Zone%Name,NumOfZones)
      IF (ZoneNum /= 0) THEN
        IF (AirModel(ZoneNum)%AirModelName /= Blank) THEN
          CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(2))//' = '//TRIM(cAlphaArgs(2)) )
          CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1)) )
          CALL ShowContinueError('Duplicate zone name, only one type of roomair model is allowed per zone' )
          CALL ShowContinueError('Zone '//TRIM(cAlphaArgs(2))//' was already assigned a roomair model by '// &
                                  TRIM(cCurrentModuleObject)//' = '//TRIM(AirModel(ZoneNum)%AirModelName))
          CALL ShowContinueError('Air Model Type for zone already set to '//TRIM(ChAirModel(AirModel(ZoneNum)%AirModelType)))
          CALL ShowContinueError('Trying to overwrite with model type = '//TRIM(cAlphaArgs(3)))
          ErrorsFound=.TRUE.
        ENDIF
        AirModel(ZoneNum)%AirModelName  = cAlphaArgs(1)
        AirModel(ZoneNum)%ZoneName      = cAlphaArgs(2)

        SELECT CASE (cAlphaArgs(3))
          CASE ('MIXING')
            AirModel(ZoneNum)%AirModelType = RoomAirModel_Mixing
          CASE ('ONENODEDISPLACEMENTVENTILATION')
            AirModel(ZoneNum)%AirModelType = RoomAirModel_Mundt
            AirModel(ZoneNum)%SimAirModel  = .TRUE.
            MundtModelUsed=.TRUE.
            IsNotOK=.FALSE.
            CALL ValidateComponent('RoomAirSettings:OneNodeDisplacementVentilation',cAlphaArgs(2),IsNotOK,  &
               'GetRoomAirModelParameters')
            IF (IsNotOK) THEN
              CALL ShowContinueError('In '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1))//'.')
              ErrorsFound=.TRUE.
            ENDIF
          CASE ('THREENODEDISPLACEMENTVENTILATION')
            AirModel(ZoneNum)%AirModelType = RoomAirModel_UCSDDV
            AirModel(ZoneNum)%SimAirModel  = .TRUE.
            UCSDModelUsed=.TRUE.
            IsNotOK=.FALSE.
            CALL ValidateComponent('RoomAirSettings:ThreeNodeDisplacementVentilation',cAlphaArgs(2),IsNotOK,  &
               'GetRoomAirModelParameters')
            IF (IsNotOK) THEN
              CALL ShowContinueError('In '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1))//'.')
              ErrorsFound=.TRUE.
            ENDIF
         CASE ('CROSSVENTILATION')
            AirModel(ZoneNum)%AirModelType = RoomAirModel_UCSDCV
            AirModel(ZoneNum)%SimAirModel  = .TRUE.
            UCSDModelUsed=.TRUE.
            IsNotOK=.FALSE.
            CALL ValidateComponent('RoomAirSettings:CrossVentilation',cAlphaArgs(2),IsNotOK,'GetRoomAirModelParameters')
            IF (IsNotOK) THEN
              CALL ShowContinueError('In '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1))//'.')
              ErrorsFound=.TRUE.
            ENDIF
         CASE ('UNDERFLOORAIRDISTRIBUTIONINTERIOR')
            AirModel(ZoneNum)%AirModelType = RoomAirModel_UCSDUFI
            AirModel(ZoneNum)%SimAirModel  = .TRUE.
            UCSDModelUsed=.TRUE.
            CALL ValidateComponent('RoomAirSettings:UnderFloorAirDistributionInterior',cAlphaArgs(2),IsNotOK,  &
               'GetRoomAirModelParameters')
            IF (IsNotOK) THEN
              CALL ShowContinueError('In '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1))//'.')
              ErrorsFound=.TRUE.
            ENDIF
         CASE ('UNDERFLOORAIRDISTRIBUTIONEXTERIOR')
            AirModel(ZoneNum)%AirModelType = RoomAirModel_UCSDUFE
            AirModel(ZoneNum)%SimAirModel  = .TRUE.
            UCSDModelUsed=.TRUE.
            CALL ValidateComponent('RoomAirSettings:UnderFloorAirDistributionExterior',cAlphaArgs(2),IsNotOK,  &
               'GetRoomAirModelParameters')
            IF (IsNotOK) THEN
              CALL ShowContinueError('In '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1))//'.')
              ErrorsFound=.TRUE.
            ENDIF
         CASE ('USERDEFINED')
            AirModel(ZoneNum)%AirModelType = RoomAirModel_UserDefined
            AirModel(ZoneNum)%SimAirModel  = .TRUE.
            UserDefinedUsed  = .TRUE.
            ! Need to make sure that Room Air controls are used for this one.
         CASE DEFAULT
            CALL ShowWarningError('Invalid '//TRIM(cAlphaFieldNames(3))//' = '//TRIM(cAlphaArgs(3)) )
            CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1)) )
            CALL ShowContinueError('The mixing air model will be used for Zone ='//TRIM(cAlphaArgs(2)) )
            AirModel(ZoneNum)%AirModelType = RoomAirModel_Mixing
        END SELECT

        SELECT CASE (cAlphaArgs(4))
          CASE ('DIRECT')
            AirModel(ZoneNum)%TempCoupleScheme = DirectCoupling
          CASE ('INDIRECT')
            AirModel(ZoneNum)%TempCoupleScheme = InDirectCoupling
          CASE DEFAULT
            CALL ShowWarningError('Invalid '//TRIM(cAlphaFieldNames(4))//' = '//TRIM(cAlphaArgs(4)) )
            CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1)) )
            CALL ShowContinueError('The direct coupling scheme will be used for Zone ='//TRIM(cAlphaArgs(2)) )
            AirModel(ZoneNum)%TempCoupleScheme = DirectCoupling
        END SELECT
      ELSE  ! Zone Not Found
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//', Zone not found='//TRIM(cAlphaArgs(2)))
        CALL ShowContinueError('occurs in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
        ErrorsFound=.TRUE.
      END IF
    END DO ! AirModel_Param_Loop

    DO ZoneNum = 1, NumOfZones
        IF (NumOfAirModels .EQ. 0) THEN
            AirModel(ZoneNum)%AirModelName  = 'MIXING AIR MODEL FOR ' // TRIM(Zone(ZoneNum)%Name)
            AirModel(ZoneNum)%ZoneName      = Zone(ZoneNum)%Name
        ELSEIF (AirModel(ZoneNum)%ZoneName == Blank) THEN
                  ! no 'select air model' object for this zone so the mixing model is used for this zone
                AirModel(ZoneNum)%AirModelName  = 'MIXING AIR MODEL FOR ' // TRIM(Zone(ZoneNum)%Name)
                AirModel(ZoneNum)%ZoneName      = Zone(ZoneNum)%Name
        END IF
    END DO

    ! Write RoomAir Model details onto EIO file
    WRITE(OutputFileInits,RoomAirHeader)
    DO ZoneNum=1,NumOfZones
      SELECT CASE(AirModel(ZoneNum)%AirModelType)
        CASE(RoomAirModel_Mixing)
          WRITE(OutputFileInits,RoomAirZoneFmt) TRIM(Zone(ZoneNum)%Name),'Mixing/Well-Stirred'
        CASE(RoomAirModel_Mundt)
          WRITE(OutputFileInits,RoomAirZoneFmt) TRIM(Zone(ZoneNum)%Name),'OneNodeDisplacementVentilation'
        CASE(RoomAirModel_UCSDDV)
          WRITE(OutputFileInits,RoomAirZoneFmt) TRIM(Zone(ZoneNum)%Name),'ThreeNodeDisplacementVentilation'
        CASE(RoomAirModel_UCSDCV)
          WRITE(OutputFileInits,RoomAirZoneFmt) TRIM(Zone(ZoneNum)%Name),'CrossVentilation'
        CASE(RoomAirModel_UCSDUFI)
          WRITE(OutputFileInits,RoomAirZoneFmt) TRIM(Zone(ZoneNum)%Name),'UnderFloorAirDistributionInterior'
        CASE(RoomAirModel_UCSDUFE)
          WRITE(OutputFileInits,RoomAirZoneFmt) TRIM(Zone(ZoneNum)%Name),'UnderFloorAirDistributionExterior'
        CASE(RoomAirModel_UserDefined)
          WRITE(OutputFileInits,RoomAirZoneFmt) TRIM(Zone(ZoneNum)%Name),'UserDefined'
      END SELECT
    ENDDO

    IF (ErrorsFound) THEN
      CALL ShowSevereError('Errors found in processing input for '//TRIM(cCurrentModuleObject) )
      ErrFlag=.TRUE.
    ENDIF

    RETURN

END SUBROUTINE  GetRoomAirModelParameters

! END of Get Input subroutines for the HBAir Module
!******************************************************************************



 ! Beginning Initialization Section of the Module
!******************************************************************************

SUBROUTINE InitAirHeatBalance  ! Surface Heat Balance Initialization Manager

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard J. Liesen
          !       DATE WRITTEN   February 1998
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is for  initializations within the
          ! air heat balance.

          ! METHODOLOGY EMPLOYED:
          ! Uses the status flags to trigger events.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

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
  LOGICAL, SAVE :: FirstCall=.TRUE.

          ! FLOW:


  ! Do the Begin Simulation initializations
  IF (FirstCall) THEN
    CAll AllocateAirHeatBalArrays ! Allocate the Module Arrays
    FirstCall=.FALSE.
  END IF


  ! Do the Begin Environment initializations
  IF (BeginEnvrnFlag) THEN
    MVFC=0.0d0
    MTC=0.0d0
  END IF


  ! Do the Begin Day initializations
  IF (BeginDayFlag) THEN

  END IF

  ! Do the following initializations (every time step):
  CALL InitSimpleMixingConvectiveHeatGains

  RETURN

END SUBROUTINE InitAirHeatBalance


SUBROUTINE AllocateAirHeatBalArrays  ! Heat Balance Array Allocation

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Liesen
          !       DATE WRITTEN   February 1998
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine allocates the arrays at execution time

          ! METHODOLOGY EMPLOYED:
          ! Uses the status flags to trigger variable allocation.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:

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
          ! na

          ! FLOW:

! Use the total number of zones to allocate variables to avoid a zone number limit
    ALLOCATE(MVFC(NumOfZones))
    MVFC=0.0d0
    ALLOCATE(MTC(NumOfZones))
    MTC=0.0d0


  RETURN

END SUBROUTINE AllocateAirHeatBalArrays

SUBROUTINE InitSimpleMixingConvectiveHeatGains
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Liesen
          !       DATE WRITTEN   February 1998
          !       MODIFIED       March 2003, FCW: allow individual window/door venting control
          !       DATE MODIFIED  April 2000
          !                      May 2009, Brent Griffith added EMS override to mixing and cross mixing flows
          !                      renamed routine and did some cleanup
          !                      August 2011, Therese Stovall added refrigeration door mixing flows
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine sets up the mixing and cross mixing flows

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE ScheduleManager, ONLY: GetCurrentScheduleValue, GetScheduleIndex
  USE General, ONLY: RoundSigDigits

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
   INTEGER Loop ! local loop index
   INTEGER NZ   ! local index for zone number
   INTEGER J    ! local index for second zone in refrig door pair

                !  Zero out time step variables
   MTC = 0.0d0
   MVFC = 0.0d0

   ! Select type of airflow calculation

   SELECT CASE (AirFlowFlag)

     CASE(UseSimpleAirFlow)  ! Simplified airflow calculation
     ! Process the scheduled Mixing for air heat balance
      DO Loop=1,TotMixing
        NZ=Mixing(Loop)%ZonePtr
        Mixing(Loop)%DesiredAirFlowRate=Mixing(Loop)%DesignLevel *  &
                     GetCurrentScheduleValue(Mixing(Loop)%SchedPtr)
        IF (Mixing(Loop)%EMSSimpleMixingOn) Mixing(Loop)%DesiredAirFlowRate = Mixing(Loop)%EMSimpleMixingFlowRate
      ENDDO

      ! Process the scheduled CrossMixing for air heat balance
      DO Loop=1,TotCrossMixing
        NZ=CrossMixing(Loop)%ZonePtr
        CrossMixing(Loop)%DesiredAirFlowRate=CrossMixing(Loop)%DesignLevel * &
                     GetCurrentScheduleValue(CrossMixing(Loop)%SchedPtr)
        IF (CrossMixing(Loop)%EMSSimpleMixingOn) CrossMixing(Loop)%DesiredAirFlowRate = CrossMixing(Loop)%EMSimpleMixingFlowRate
        MTC(Loop)=CrossMixing(Loop)%DeltaTemperature
        MVFC(Loop)=CrossMixing(Loop)%DesiredAirFlowRate
      ENDDO

  !Note - do each Pair a Single time, so must do increment reports for both zones
  !       Can't have a pair that has ZoneA zone number = NumofZones because organized
  !       in input with lowest zone # first no matter how input in idf

    ! Process the scheduled Refrigeration Door mixing for air heat balance
    IF(TotRefDoorMixing .GT. 0) THEN
      DO NZ=1,(NumOfZones - 1) ! Can't have %ZonePtr==NumOfZones because lesser zone # of pair placed in ZonePtr in input
        IF(.NOT. RefDoorMixing(NZ)%RefDoorMixFlag)CYCLE
        IF ((RefDoorMixing(NZ)%ZonePtr .EQ. NZ)) THEN
          DO J = 1,RefDoorMixing(NZ)%NumRefDoorConnections
            RefDoorMixing(NZ)%VolRefDoorFlowRate(J)=0.0d0
            IF (RefDoorMixing(NZ)%EMSRefDoorMixingOn(J)) &
               RefDoorMixing(NZ)%VolRefDoorFlowRate(J) = RefDoorMixing(NZ)%EMSRefDoorFlowRate(J)
          END DO
        END IF
      ENDDO
    END IF !TotRefDoorMixing

! Infiltration and ventilation calculations have been moved to a subroutine of CalcAirFlowSimple in HVAC Manager

     CASE DEFAULT
   END SELECT

  RETURN

END SUBROUTINE InitSimpleMixingConvectiveHeatGains

 ! END Initialization Section of the Module
!******************************************************************************


 ! Begin Algorithm Section of the Module
!******************************************************************************
SUBROUTINE CalcHeatBalanceAir

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Legacy Code
          !       DATE WRITTEN   na
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine calculates the air component of the heat balance.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE HVACManager, ONLY: ManageHVAC

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
          ! na

  CALL ManageHVAC

  ! Do Final Temperature Calculations for Heat Balance before next Time step
  SumHmAW=0.0D0
  SumHmARa=0.0D0
  SumHmARaW=0.0D0

  RETURN

END SUBROUTINE CalcHeatBalanceAir

! END Algorithm Section of the Module

SUBROUTINE ReportZoneMeanAirTemp
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   July 2000
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine updates the report variables for the AirHeatBalance.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataHeatBalance, ONLY: MRT
  USE DataZoneControls, ONLY: AnyOpTempControl, TempControlledZone
  USE ScheduleManager, ONLY: GetCurrentScheduleValue
  USE Psychrometrics, ONLY: PsyTdpFnWPb
  USE DataEnvironment, ONLY: OutBaroPress

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
  INTEGER  :: TempControlledZoneID    ! index for zone in TempConrolled Zone structure
  REAL(r64)     :: thisMRTFraction         ! temp working value for radiative fraction/weight

  DO ZoneLoop = 1, NumOfZones
          ! The mean air temperature is actually ZTAV which is the average
          ! temperature of the air temperatures at the system time step for the
          ! entire zone time step.
    ZnAirRpt(ZoneLoop)%MeanAirTemp = ZTAV(ZoneLoop)
    ZnAirRpt(ZoneLoop)%MeanAirHumRat = ZoneAirHumRatAvg(ZoneLoop)
    ZnAirRpt(ZoneLoop)%OperativeTemp = 0.5d0*(ZTAV(ZoneLoop)+MRT(ZoneLoop))
    ZnAirRpt(ZoneLoop)%MeanAirDewpointTemp = PsyTdpFnWPb(ZnAirRpt(ZoneLoop)%MeanAirHumRat,OutBaroPress)

    ! if operative temperature control is being used, then radiative fraction/weighting
    !  might be defined by user to be something different than 0.5, even scheduled over simulation period
    IF (AnyOpTempControl) THEN ! dig further...
      ! find TempControlledZoneID from ZoneLoop index
      TempControlledZoneID = Zone(ZoneLoop)%TempControlledZoneIndex
      IF (Zone(ZoneLoop)%IsControlled) THEN
        IF ((TempControlledZone(TempControlledZoneID)%OperativeTempControl)) THEN
          ! is operative temp radiative fraction scheduled or fixed?
          IF (TempControlledZone(TempControlledZoneID)%OpTempCntrlModeScheduled) THEN
            thisMRTFraction = GetCurrentScheduleValue(TempControlledZone(TempControlledZoneID)%OpTempRadiativeFractionSched)
          ELSE
            thisMRTFraction = TempControlledZone(TempControlledZoneID)%FixedRadiativeFraction
          ENDIF
          ZnAirRpt(ZoneLoop)%ThermOperativeTemp = (1.0d0-thisMRTFraction) * ZTAV(ZoneLoop) +   &
                                                        thisMRTFraction * MRT(ZoneLoop)
        ENDIF
      ENDIF
    ENDIF
  END DO

  RETURN
END SUBROUTINE ReportZoneMeanAirTemp

! *****************************************************************************

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

END MODULE HeatBalanceAirManager
