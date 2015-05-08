MODULE InternalHeatGains
  ! Module containing the routines dealing with the internal heat gains

  ! MODULE INFORMATION:
  !       AUTHOR         Rick Strand
  !       DATE WRITTEN   August 2000
  !       MODIFIED       Aug 2005, PGE (Added object names and report variables)
  !                      Feb 2006, PGE (Added end-use subcategories)
  !       RE-ENGINEERED  na

  ! PURPOSE OF THIS MODULE:
  ! Part of the heat balance modularization/re-engineering.  Purpose of this
  ! module is to contain the internal heat gain routines in a single location.

  ! METHODOLOGY EMPLOYED:
  ! Routines are called as subroutines to supply the data-only module structures
  ! with the proper values.

  ! REFERENCES:
  ! Legacy BLAST code

  ! OTHER NOTES: none

  ! USE STATEMENTS:
USE DataPrecisionGlobals
USE DataGlobals
USE DataEnvironment
USE DataHeatBalance
USE DataSurfaces
USE DataInterfaces

IMPLICIT NONE         ! Enforce explicit typing of all variables

PRIVATE ! Everything private unless explicitly made public

  ! MODULE PARAMETER DEFINITIONS:


LOGICAL :: GetInternalHeatGainsInputFlag = .TRUE.   ! Controls the GET routine calling (limited to first time)

  ! SUBROUTINE SPECIFICATIONS FOR MODULE InternalHeatGains
PUBLIC  ManageInternalHeatGains
PUBLIC  UpdateInternalGainValues
PRIVATE GetInternalHeatGainsInput
PRIVATE InitInternalHeatGains
PRIVATE ReportInternalHeatGains
PUBLIC  GetDesignLightingLevelForZone
PUBLIC  CheckLightsReplaceableMinMaxForZone
PUBLIC  SumAllInternalConvectionGains
PUBLIC  SumInternalConvectionGainsByTypes
!PUBLIC  SumInternalConvectionGainsByIndices
PUBLIC  SumAllReturnAirConvectionGains
PUBLIC  SumReturnAirConvectionGainsByTypes
!PUBLIC SumReturnAirConvectionGainsByIndices
PUBLIC  SumAllInternalRadiationGains
PUBLIC  SumInternalRadiationGainsByTypes
!PUBLIC  SumInternalRadiationGainsByIndices
PUBLIC  SumAllInternalLatentGains
PUBLIC  SumInternalLatentGainsByTypes
!PUBLIC  SumInternalLatentGainsByIndices
PUBLIC  SumAllReturnAirLatentGains
!PUBLIC
PUBLIC  SumAllInternalCO2Gains
PUBLIC  SumInternalCO2GainsByTypes
PUBLIC  SumAllInternalGenericContamGains
!PUBLIC  SumInternalCO2GainsByIndices
!PUBLIC  GetInternalGainDeviceIndex



CONTAINS

SUBROUTINE ManageInternalHeatGains(InitOnly)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Rick Strand
          !       DATE WRITTEN   May 2000
          !       MODIFIED       Mar 2004, FCW: move call to DayltgElecLightingControl from InitSurfaceHeatBalance
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This is the main driver subroutine for the internal heat gains.

          ! METHODOLOGY EMPLOYED:
          ! Standard EnergyPlus methodology.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  LOGICAL, INTENT(IN), OPTIONAL            :: InitOnly  ! when true, just calls the get input, if appropriate and returns.

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
!unused  INTEGER       :: ZoneNum              ! Zone number

          ! FLOW:
  IF (GetInternalHeatGainsInputFlag) THEN
    CALL GetInternalHeatGainsInput
    GetInternalHeatGainsInputFlag = .FALSE.
  END IF

  IF (PRESENT(InitOnly)) THEN
    IF (InitOnly) RETURN
  ENDIF

  CALL InitInternalHeatGains

  CALL ReportInternalHeatGains

  !for the load component report, gather the load components for each timestep but not when doing pulse
  IF (ZoneSizingCalc) CALL GatherComponentLoadsIntGain
  RETURN

END SUBROUTINE ManageInternalHeatGains


SUBROUTINE GetInternalHeatGainsInput

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   September 1997
          !       MODIFIED       September 1998, FW
          !                      May 2009, BG: added calls to setup for possible EMS override
          !       RE-ENGINEERED  August 2000, RKS

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine gets the Internal Heat Gain Data for the Zones.
          ! Sets up the various data that will be used later with the
          ! schedulemanager to determine the actual values.

          ! METHODOLOGY EMPLOYED:
          ! The GetObjectItem routines are employed to retrieve the data.

          ! REFERENCES:
          ! IDD Objects:
          ! People
          ! Lights
          ! ElectricEquipment
          ! GasEquipment
          ! SteamEquipment
          ! HotWaterEquipment
          ! OtherEquipment
          ! ZoneBaseboard:OutdoorTemperatureControlled

          ! USE STATEMENTS:
  USE DataIPShortCuts
  USE InputProcessor
  USE ScheduleManager
  USE General, ONLY: RoundSigDigits, CheckCreatedZoneItemName
  USE OutputReportPredefined
  USE DataInterfaces, ONLY: SetupOutputVariable, SetupEMSInternalVariable

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER :: Blank=' '
  CHARACTER(len=*), PARAMETER :: fmta='(A)'
  CHARACTER(len=*), PARAMETER :: RoutineName='GetInternalHeatGains: '
          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  CHARACTER(len=MaxNameLength), DIMENSION(:), ALLOCATABLE :: AlphaName
  LOGICAL                                     :: ErrorsFound=.false. ! If errors found in input
  LOGICAL                                     :: IsNotOK             ! Flag to verify name
  REAL(r64), DIMENSION(:), ALLOCATABLE        :: IHGNumbers
  INTEGER                                     :: IOStat
  LOGICAL                                     :: IsBlank
  INTEGER                                     :: Loop
  LOGICAL                                     :: MustInpSch
  INTEGER                                     :: NumAlpha
  INTEGER                                     :: NumNumber
  INTEGER                                     :: MaxAlpha
  INTEGER                                     :: MaxNumber
  INTEGER                                     :: OptionNum
  INTEGER                                     :: lastOption
  LOGICAL, DIMENSION(:), ALLOCATABLE          :: RepVarSet
  !   Variables for reporting nominal internal gains
  REAL(r64) LightTot     ! Total Lights for calculating lights per square meter
  REAL(r64) ElecTot       ! Total Electric Load for calculating electric per square meter
  REAL(r64) GasTot        ! Total Gas load for calculating gas per square meter
  REAL(r64) OthTot        ! Total Other load for calculating other load per square meter
  REAL(r64) HWETot        ! Total Hot Water Equipment for calculating HWE per square meter
  REAL(r64) StmTot        ! Total Steam for calculating Steam per square meter
  CHARACTER(len=3) BBHeatInd ! Yes if BBHeat in zone, no if not.
  INTEGER Loop1
  CHARACTER(len=MaxNameLength) :: StringOut
  REAL(r64) SchMin
  REAL(r64) SchMax
  LOGICAL :: UsingThermalComfort=.false.
!unused  LOGICAL :: ErrFlag
  CHARACTER(len=MaxNameLength) :: liteName
  INTEGER :: zonePt
  REAL(r64) :: mult
  REAL(r64) :: sumArea = 0.0d0
  REAL(r64) :: sumPower = 0.0d0
  INTEGER   :: ZoneNum
  REAL(r64) :: maxOccupLoad
  CHARACTER(len=MaxNameLength) :: CurrentModuleObject
  LOGICAL :: ErrFlag
  INTEGER :: Item
  INTEGER :: ZLItem
  INTEGER :: Item1
  INTEGER :: MaxZoneNameLengthInZoneList

          ! FLOW:
  ALLOCATE(ZoneIntGain(NumOfZones))
  ALLOCATE(ZnRpt(NumOfZones))
  ALLOCATE(ZoneIntEEuse(NumOfZones))
  ALLOCATE(RefrigCaseCredit(NumOfZones))

  ALLOCATE(RepVarSet(NumOfZones))
  RepVarSet=.true.

  ! Determine argument length of objects gotten by this routine
  MaxAlpha=-100
  MaxNumber=-100
  CurrentModuleObject='People'
  CALL GetObjectDefMaxArgs(CurrentModuleObject,Loop,NumAlpha,NumNumber)
  MaxAlpha=MAX(MaxAlpha,NumAlpha)
  MaxNumber=MAX(MaxNumber,NumNumber)
  CurrentModuleObject='Lights'
  CALL GetObjectDefMaxArgs(CurrentModuleObject,Loop,NumAlpha,NumNumber)
  MaxAlpha=MAX(MaxAlpha,NumAlpha)
  MaxNumber=MAX(MaxNumber,NumNumber)
  CurrentModuleObject='ElectricEquipment'
  CALL GetObjectDefMaxArgs(CurrentModuleObject,Loop,NumAlpha,NumNumber)
  MaxAlpha=MAX(MaxAlpha,NumAlpha)
  MaxNumber=MAX(MaxNumber,NumNumber)
  CurrentModuleObject='GasEquipment'
  CALL GetObjectDefMaxArgs(CurrentModuleObject,Loop,NumAlpha,NumNumber)
  MaxAlpha=MAX(MaxAlpha,NumAlpha)
  MaxNumber=MAX(MaxNumber,NumNumber)
  CurrentModuleObject='HotWaterEquipment'
  CALL GetObjectDefMaxArgs(CurrentModuleObject,Loop,NumAlpha,NumNumber)
  MaxAlpha=MAX(MaxAlpha,NumAlpha)
  MaxNumber=MAX(MaxNumber,NumNumber)
  CurrentModuleObject='SteamEquipment'
  CALL GetObjectDefMaxArgs(CurrentModuleObject,Loop,NumAlpha,NumNumber)
  MaxAlpha=MAX(MaxAlpha,NumAlpha)
  MaxNumber=MAX(MaxNumber,NumNumber)
  CurrentModuleObject='OtherEquipment'
  CALL GetObjectDefMaxArgs(CurrentModuleObject,Loop,NumAlpha,NumNumber)
  MaxAlpha=MAX(MaxAlpha,NumAlpha)
  MaxNumber=MAX(MaxNumber,NumNumber)
  CurrentModuleObject='ZoneBaseboard:OutdoorTemperatureControlled'
  CALL GetObjectDefMaxArgs(CurrentModuleObject,Loop,NumAlpha,NumNumber)
  MaxAlpha=MAX(MaxAlpha,NumAlpha)
  MaxNumber=MAX(MaxNumber,NumNumber)
  CurrentModuleObject='ZoneContaminantSourceAndSink:CarbonDioxide'
  CALL GetObjectDefMaxArgs(CurrentModuleObject,Loop,NumAlpha,NumNumber)
  MaxAlpha=MAX(MaxAlpha,NumAlpha)
  MaxNumber=MAX(MaxNumber,NumNumber)

  ALLOCATE(IHGNumbers(MaxNumber))
  ALLOCATE(AlphaName(MaxAlpha))
  IHGNumbers=0.0d0
  AlphaName=' '

  !CurrentModuleObject='Zone'
  DO Loop=1,NumOfZones
  ! Overall Zone Variables
    CALL SetupOutputVariable('Zone Total Internal Radiant Heating Energy [J]',ZnRpt(Loop)%TotRadiantGain,  &
                    'Zone','Sum',Zone(Loop)%Name)
    CALL SetupOutputVariable('Zone Total Internal Radiant Heating Rate [W]',ZnRpt(Loop)%TotRadiantGainRate,  &
                    'Zone','Average',Zone(Loop)%Name)
    CALL SetupOutputVariable('Zone Total Internal Visible Radiation Heating Energy [J]',ZnRpt(Loop)%TotVisHeatGain,  &
                    'Zone','Sum',Zone(Loop)%Name)
    CALL SetupOutputVariable('Zone Total Internal Visible Radiation Heating Rate [W]',ZnRpt(Loop)%TotVisHeatGainRate,  &
                    'Zone','Average',Zone(Loop)%Name)
    CALL SetupOutputVariable('Zone Total Internal Convective Heating Energy [J]',ZnRpt(Loop)%TotConvectiveGain,  &
                    'Zone','Sum',Zone(Loop)%Name)
    CALL SetupOutputVariable('Zone Total Internal Convective Heating Rate [W]',ZnRpt(Loop)%TotConvectiveGainRate,  &
                    'Zone','Average',Zone(Loop)%Name)
    CALL SetupOutputVariable('Zone Total Internal Latent Gain Energy [J]',ZnRpt(Loop)%TotLatentGain,  &
                    'Zone','Sum',Zone(Loop)%Name)
    CALL SetupOutputVariable('Zone Total Internal Latent Gain Rate [W]',ZnRpt(Loop)%TotLatentGainRate,  &
                    'Zone','Average',Zone(Loop)%Name)
    CALL SetupOutputVariable('Zone Total Internal Total Heating Energy [J]',ZnRpt(Loop)%TotTotalHeatGain,  &
                    'Zone','Sum',Zone(Loop)%Name)
    CALL SetupOutputVariable('Zone Total Internal Total Heating Rate [W]',ZnRpt(Loop)%TotTotalHeatGainRate,  &
                    'Zone','Average',Zone(Loop)%Name)
  END DO

  ! PEOPLE: Includes both information related to the heat balance and thermal comfort
  ! First, allocate and initialize the People derived type
  CurrentModuleObject='People'
  NumPeopleStatements=GetNumObjectsFound(CurrentModuleObject)
  ALLOCATE(PeopleObjects(NumPeopleStatements))

  TotPeople=0
  ErrFlag=.false.
  DO Item=1,NumPeopleStatements
    CALL GetObjectItem(CurrentModuleObject,Item,AlphaName,NumAlpha,IHGNumbers,NumNumber,IOStat,  &
                   AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                   AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
    IsNotOK = .FALSE.
    IsBlank = .FALSE.
    CALL VerifyName(AlphaName(1),PeopleObjects%Name,Item-1,IsNotOK,IsBlank,TRIM(CurrentModuleObject)//' Name')
    IF (IsNotOK) THEN
      ErrorsFound = .TRUE.
      ErrFlag=.true.
      IF (IsBlank) AlphaName(1) = 'xxxxx'
    END IF
    PeopleObjects(Item)%Name = AlphaName(1)

    Item1=FindItemInList(AlphaName(2),Zone%Name,NumOfZones)
    ZLItem=0
    IF (Item1 == 0 .and. NumOfZoneLists > 0) &
        ZLItem=FindItemInList(AlphaName(2),ZoneList%Name,NumOfZoneLists)
    IF (Item1 > 0) THEN
      PeopleObjects(Item)%StartPtr=TotPeople+1
      TotPeople=TotPeople+1
      PeopleObjects(Item)%NumOfZones=1
      PeopleObjects(Item)%ZoneListActive=.false.
      PeopleObjects(Item)%ZoneOrZoneListPtr=Item1
    ELSEIF (ZLItem > 0) THEN
      PeopleObjects(Item)%StartPtr=TotPeople+1
      TotPeople=TotPeople+ZoneList(ZLItem)%NumOfZones
      PeopleObjects(Item)%NumOfZones=ZoneList(ZLItem)%NumOfZones
      PeopleObjects(Item)%ZoneListActive=.true.
      PeopleObjects(Item)%ZoneOrZoneListPtr=ZLItem
    ELSE
      CALL ShowSevereError(trim(CurrentModuleObject)//'="'//trim(AlphaName(1))//'" invalid '//  &
           trim(cAlphaFieldNames(2))//'="'//trim(AlphaName(2))//'" not found.')
      ErrorsFound=.true.
      ErrFlag=.true.
    ENDIF
  ENDDO

  IF (ErrFlag) THEN
    CALL ShowSevereError(RoutineName//'Errors with invalid names in '//trim(CurrentModuleObject)//  &
       ' objects.')
    CALL ShowContinueError('...These will not be read in.  Other errors may occur.')
    TotPeople=0
  ENDIF

  ALLOCATE(People(TotPeople))

  IF (TotPeople > 0) THEN
    Loop=0
    DO Item = 1, NumPeopleStatements
      AlphaName  = Blank
      IHGNumbers = 0.0d0

      CALL GetObjectItem(CurrentModuleObject,Item,AlphaName,NumAlpha,IHGNumbers,NumNumber,IOStat,  &
                     AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                     AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

      DO Item1=1,PeopleObjects(Item)%NumOfZones
        Loop=Loop+1
        IF (.not. PeopleObjects(Item)%ZoneListActive) THEN
          People(Loop)%Name = AlphaName(1)
          People(Loop)%ZonePtr = PeopleObjects(Item)%ZoneOrZoneListPtr
        ELSE
          CALL CheckCreatedZoneItemName(RoutineName,CurrentModuleObject,  &
                                        Zone(ZoneList(PeopleObjects(Item)%ZoneOrZoneListPtr)%Zone(Item1))%Name,  &
                                        ZoneList(PeopleObjects(Item)%ZoneOrZoneListPtr)%MaxZoneNameLength,  &
                                        PeopleObjects(Item)%Name,     &
                                        People%Name,           &
                                        Loop-1,                       &
                                        People(Loop)%Name,            &
                                        ErrFlag)
          People(Loop)%ZonePtr = ZoneList(PeopleObjects(Item)%ZoneOrZoneListPtr)%Zone(Item1)
          IF (ErrFlag) ErrorsFound=.true.
        ENDIF

        People(Loop)%NumberOfPeoplePtr  = GetScheduleIndex(AlphaName(3))
        SchMin=0.0d0
        SchMax=0.0d0
        IF (People(Loop)%NumberOfPeoplePtr == 0) THEN
          IF (Item1 == 1) THEN  ! only show error on first one
            IF (lAlphaFieldBlanks(3)) THEN
              CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(AlphaName(1))//  &
                                   '", '//TRIM(cAlphaFieldNames(3))//' is required.')
            ELSE
              CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(AlphaName(1))//  &
                                   '", invalid '//TRIM(cAlphaFieldNames(3))//' entered='//TRIM(AlphaName(3)))
            ENDIF
            ErrorsFound=.true.
          ENDIF
        ELSE  ! check min/max on schedule
          SchMin=GetScheduleMinValue(People(Loop)%NumberOfPeoplePtr)
          SchMax=GetScheduleMaxValue(People(Loop)%NumberOfPeoplePtr)
          IF (SchMin < 0.0d0 .or. SchMax < 0.0d0) THEN
            IF (Item1 == 1) THEN
              IF (SchMin < 0.0d0) THEN
                CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(AlphaName(1))//  &
                                   '", '//TRIM(cAlphaFieldNames(3))//', minimum is < 0.0')
                CALL ShowContinueError('Schedule="'//TRIM(AlphaName(3))//  &
                                   '". Minimum is ['//TRIM(RoundSigDigits(SchMin,1))//']. Values must be >= 0.0.')
                ErrorsFound=.true.
              ENDIF
            ENDIF
            IF (Item1 == 1) THEN
              IF (SchMax < 0.0d0) THEN
                CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(AlphaName(1))//  &
                                   '", '//TRIM(cAlphaFieldNames(3))//', maximum is < 0.0')
                CALL ShowContinueError('Schedule="'//TRIM(AlphaName(3))//  &
                                   '". Maximum is ['//TRIM(RoundSigDigits(SchMax,1))//']. Values must be >= 0.0.')
                ErrorsFound=.true.
              ENDIF
            ENDIF
          ENDIF
        ENDIF

        ! Number of people calculation method.
        SELECT CASE (AlphaName(4))
          CASE('PEOPLE')
            People(Loop)%NumberOfPeople = IHGNumbers(1)
            IF (lNumericFieldBlanks(1)) THEN
              CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(People(Loop)%Name)//  &
                '", specifies '//TRIM(cNumericFieldNames(1))//', but that field is blank.  0 People will result.')
            ENDIF

          CASE('PEOPLE/AREA')
            IF (People(Loop)%ZonePtr /= 0) THEN
              IF (IHGNumbers(2) >= 0.0d0) THEN
                People(Loop)%NumberOfPeople = IHGNumbers(2)*Zone(People(Loop)%ZonePtr)%FloorArea
                IF (Zone(People(Loop)%ZonePtr)%FloorArea <= 0.0d0) THEN
                  CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(People(Loop)%Name)//  &
                    '", specifies '//TRIM(cNumericFieldNames(2))//', but Zone Floor Area = 0.  0 People will result.')
                ENDIF
              ELSE
                CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(People(Loop)%Name)//  &
                                 '", invalid '//TRIM(cNumericFieldNames(2))//', value  [<0.0]='//  &
                                 TRIM(RoundSigDigits(IHGNumbers(2),3)))
                ErrorsFound=.true.
              ENDIF
            ENDIF
            IF (lNumericFieldBlanks(2)) THEN
              CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(People(Loop)%Name)//  &
                '", specifies '//TRIM(cNumericFieldNames(2))//', but that field is blank.  0 People will result.')
            ENDIF

          CASE('AREA/PERSON')
            IF (People(Loop)%ZonePtr /= 0) THEN
              IF (IHGNumbers(3) > 0.0d0) THEN
                People(Loop)%NumberOfPeople = Zone(People(Loop)%ZonePtr)%FloorArea/IHGNumbers(3)
                IF (Zone(People(Loop)%ZonePtr)%FloorArea <= 0.0d0) THEN
                  CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(People(Loop)%Name)//  &
                    '", specifies '//TRIM(cNumericFieldNames(2))//', but Zone Floor Area = 0.  0 People will result.')
                ENDIF
              ELSE
                CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(People(Loop)%Name)//  &
                                 '", invalid '//TRIM(cNumericFieldNames(3))//', value  [<0.0]='//  &
                                 TRIM(RoundSigDigits(IHGNumbers(3),3)))
                ErrorsFound=.true.
              ENDIF
            ENDIF
            IF (lNumericFieldBlanks(3)) THEN
              CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(People(Loop)%Name)//  &
                '", specifies '//TRIM(cNumericFieldNames(3))//', but that field is blank.  0 People will result.')
            ENDIF

          CASE DEFAULT
            IF (Item1 == 1) THEN
              CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(AlphaName(1))//  &
                                 '", invalid '//TRIM(cAlphaFieldNames(4))//', value  ='//  &
                                 TRIM(AlphaName(4)))
              CALL ShowContinueError('...Valid values are "People", "People/Area", "Area/Person".')
              ErrorsFound=.true.
            ENDIF
        END SELECT

        ! Calculate nominal min/max people
        People(Loop)%NomMinNumberPeople=People(Loop)%NumberOfPeople*SchMin
        People(Loop)%NomMaxNumberPeople=People(Loop)%NumberOfPeople*SchMax

        IF (People(Loop)%ZonePtr > 0) THEN
          Zone(People(Loop)%ZonePtr)%TotOccupants = Zone(People(Loop)%ZonePtr)%TotOccupants + People(Loop)%NumberOfPeople
        ENDIF

        People(Loop)%FractionRadiant   = IHGNumbers(4)
        People(Loop)%FractionConvected = 1.0d0-People(Loop)%FractionRadiant
        IF (Item1 == 1) THEN
          IF (People(Loop)%FractionConvected < 0.0d0) THEN
            CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(AlphaName(1))//  &
                                   '", '//TRIM(cNumericFieldNames(4))//' < 0.0, value ='//  &
                                   TRIM(RoundSigDigits(IHGNumbers(4),2)))
            ErrorsFound=.true.
          ENDIF
        ENDIF

        IF (NumNumber .GE. 5 .and. .not. lNumericFieldBlanks(5)) THEN
          People(Loop)%UserSpecSensFrac = IHGNumbers(5)
        ELSE
          People(Loop)%UserSpecSensFrac = AutoCalculate
        ENDIF

        IF (NumNumber == 6 .and. .not. lNumericFieldBlanks(6)) THEN
          People(Loop)%CO2RateFactor = IHGNumbers(6)
        ELSE
          People(Loop)%CO2RateFactor = 3.82d-8 ! m3/s-W
        ENDIF
        If (People(Loop)%CO2RateFactor .LT. 0.d0) Then
          CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(AlphaName(1))//  &
                             '", '//TRIM(cNumericFieldNames(6))//' < 0.0, value ='//  &
                             TRIM(RoundSigDigits(IHGNumbers(6),2)))
          ErrorsFound=.true.
        End If

        People(Loop)%ActivityLevelPtr  = GetScheduleIndex(AlphaName(5))
        IF (People(Loop)%ActivityLevelPtr == 0) THEN
          IF (Item1 == 1) THEN
            IF (lAlphaFieldBlanks(5)) THEN
              CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(AlphaName(1))//  &
                                   '", '//TRIM(cAlphaFieldNames(5))//' is required.')
            ELSE
              CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(AlphaName(1))//  &
                                   '", invalid '//TRIM(cAlphaFieldNames(5))//' entered='//TRIM(AlphaName(5)))
            ENDIF
            ErrorsFound=.true.
          ENDIF
        ELSE   ! Check values in Schedule
          SchMin=GetScheduleMinValue(People(Loop)%ActivityLevelPtr)
          SchMax=GetScheduleMaxValue(People(Loop)%ActivityLevelPtr)
          IF (SchMin < 0.0d0 .or. SchMax < 0.0d0) THEN
            IF (Item1 == 1) THEN
              IF (SchMin < 0.0d0) THEN
                CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(AlphaName(1))//  &
                                   '", '//TRIM(cAlphaFieldNames(5))//' minimum is < 0.0')
                CALL ShowContinueError('Schedule="'//TRIM(AlphaName(5))//  &
                                   '". Minimum is ['//TRIM(RoundSigDigits(SchMin,1))//']. Values must be >= 0.0.')
                ErrorsFound=.true.
              ENDIF
            ENDIF
            IF (Item1 == 1) THEN
              IF (SchMax < 0.0d0) THEN
                CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(AlphaName(1))//  &
                                   '", '//TRIM(cAlphaFieldNames(5))//' maximum is < 0.0')
                CALL ShowContinueError('Schedule="'//TRIM(AlphaName(5))//  &
                                   '". Maximum is ['//TRIM(RoundSigDigits(SchMax,1))//']. Values must be >= 0.0.')
                ErrorsFound=.true.
              ENDIF
            ENDIF
          ELSEIF (SchMin < 70.0d0 .or. SchMax > 1000.0d0) THEN
            IF (Item1 == 1) THEN
              CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(AlphaName(1))//  &
                                   '", '//TRIM(cAlphaFieldNames(5))//' values')
              CALL ShowContinueError('fall outside typical range [70,1000] W/person for Thermal Comfort Reporting.')
              CALL ShowContinueError('Odd comfort values may result; Schedule="'//TRIM(AlphaName(5))//'".')
              CALL ShowContinueError('Entered min/max range=['//trim(RoundSigDigits(SchMin,1))//','//  &
                                   trim(RoundSigDigits(SchMax,1))//'] W/person.')
            ENDIF
          ENDIF
        ENDIF

        ! Following is an optional parameter (ASHRAE 55 warnings
        IF (NumAlpha >= 6) THEN
          IF (SameString(AlphaName(6),'Yes')) THEN
            People(Loop)%Show55Warning = .TRUE.
          ELSEIF (.not. SameString(AlphaName(6),'No') .and. .not. lAlphaFieldBlanks(6)) THEN
            IF (Item1 == 1) THEN
              CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(AlphaName(1))//  &
                   '", '//TRIM(cAlphaFieldNames(6))//' field should be Yes or No')
              CALL ShowContinueError('...Field value="'//trim(AlphaName(6))//'" is invalid.')
              ErrorsFound=.true.
            ENDIF
          END IF
        END IF

        IF (NumAlpha > 6) THEN ! Optional parameters present--thermal comfort data follows...
          MustInpSch=.false.
          UsingThermalComfort=.false.
          lastOption = NumAlpha

          DO OptionNum = 14,lastOption

            SELECT CASE (AlphaName(OptionNum))

              CASE ('FANGER')
                People(Loop)%Fanger = .TRUE.
                MustInpSch=.true.
                UsingThermalComfort=.true.

              CASE ('PIERCE')
                People(Loop)%Pierce = .TRUE.
                MustInpSch=.true.
                UsingThermalComfort=.true.

              CASE ('KSU')
                People(Loop)%KSU    = .TRUE.
                MustInpSch=.true.
                UsingThermalComfort=.true.

              CASE ('ADAPTIVEASH55')
                People(Loop)%AdaptiveASH55 = .TRUE.
                AdaptiveComfortRequested_ASH55=.true.
                MustInpSch=.true.
                UsingThermalComfort=.true.

              CASE ('ADAPTIVECEN15251')
                People(Loop)%AdaptiveCEN15251 = .TRUE.
                AdaptiveComfortRequested_CEN15251=.true.
                MustInpSch=.true.
                UsingThermalComfort=.true.

              CASE (Blank) ! Blank input field--just ignore this

              CASE DEFAULT ! An invalid keyword was entered--warn but ignore
                IF (Item1 == 1) THEN
                  CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(AlphaName(1))//  &
                               '", invalid '//TRIM(cAlphaFieldNames(OptionNum))//' Option='//TRIM(AlphaName(OptionNum)))
                  CALL ShowContinueError('Valid Values are "Fanger", "Pierce", "KSU", "AdaptiveASH55", "AdaptiveCEN15251"')
                ENDIF
            END SELECT

          END DO

          IF (UsingThermalComfort) THEN

            ! Set the default value of MRTCalcType as 'ZoneAveraged'
            People(Loop)%MRTCalcType = ZoneAveraged

            ! MRT Calculation Type and Surface Name
            SELECT CASE (AlphaName(7))

                CASE ('ZONEAVERAGED')
                  People(Loop)%MRTCalcType = ZoneAveraged

                CASE ('SURFACEWEIGHTED')
                  People(Loop)%MRTCalcType = SurfaceWeighted
                  People(Loop)%SurfacePtr = FindIteminList(AlphaName(8),Surface%Name,TotSurfaces)
                  IF (People(Loop)%SurfacePtr == 0) THEN
                    IF (Item1 == 1) THEN
                      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(AlphaName(1))//  &
                                     '", '//TRIM(cAlphaFieldNames(7))//'='//TRIM(AlphaName(7))//  &
                                     ' invalid Surface Name='//TRIM(AlphaName(8)))
                      ErrorsFound=.true.
                    ENDIF
                  ELSEIF (Surface(People(Loop)%SurfacePtr)%Zone /= People(Loop)%ZonePtr) THEN
                    CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(AlphaName(1))//  &
                                   '", Surface referenced in '//TRIM(cAlphaFieldNames(7))//'='//TRIM(AlphaName(8))//  &
                                   ' in different zone.')
                    CALL ShowContinueError('Surface is in Zone='//TRIM(Zone(Surface(People(Loop)%SurfacePtr)%Zone)%Name)// &
                                     ' and '//TRIM(CurrentModuleObject)//' is in Zone='//TRIM(AlphaName(2)))
                    ErrorsFound=.true.
                  ENDIF

                CASE ('ANGLEFACTOR')
                  People(Loop)%MRTCalcType = AngleFactor
                  People(Loop)%AngleFactorListName = AlphaName(8)

                CASE (Blank) ! Blank input field--just ignore this
                    IF (MustInpSch .and. Item1 == 1)   &
                       CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(AlphaName(1))//  &
                                   '", blank '//TRIM(cAlphaFieldNames(7)))

                CASE DEFAULT ! An invalid keyword was entered--warn but ignore
                    IF (MustInpSch .and. Item1 == 1)  THEN
                      CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(AlphaName(1))//  &
                                  '", invalid '//TRIM(cAlphaFieldNames(7))//'='//TRIM(AlphaName(7)))
                      CALL ShowContinueError('...Valid values are "ZoneAveraged", "SurfaceWeighted", "AngleFactor".')
                    ENDIF
            END SELECT

            IF (.not. lAlphaFieldBlanks(9)) THEN
              People(Loop)%WorkEffPtr  = GetScheduleIndex(AlphaName(9))
              IF (People(Loop)%WorkEffPtr == 0) THEN
                IF (Item1 == 1) THEN
                  CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(AlphaName(1))//  &
                                       '", invalid '//TRIM(cAlphaFieldNames(9))//' entered='//TRIM(AlphaName(9)))
                  ErrorsFound=.true.
                ENDIF
              ELSE  ! check min/max on schedule
                SchMin=GetScheduleMinValue(People(Loop)%WorkEffPtr)
                SchMax=GetScheduleMaxValue(People(Loop)%WorkEffPtr)
                IF (SchMin < 0.0d0 .or. SchMax < 0.0d0) THEN
                  IF (SchMin < 0.0d0) THEN
                    IF (Item1 == 1) THEN
                      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(AlphaName(1))//  &
                                         '", '//TRIM(cAlphaFieldNames(9))//', minimum is < 0.0')
                      CALL ShowContinueError('Schedule="'//TRIM(AlphaName(9))//  &
                                         '". Minimum is ['//TRIM(RoundSigDigits(SchMin,1))//']. Values must be >= 0.0.')
                      ErrorsFound=.true.
                    ENDIF
                  ENDIF
                  IF (SchMax < 0.0d0) THEN
                    IF (Item1 == 1) THEN
                      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(AlphaName(1))//  &
                                         '", '//TRIM(cAlphaFieldNames(9))//', maximum is < 0.0')
                      CALL ShowContinueError('Schedule="'//TRIM(AlphaName(9))//  &
                                         '". Maximum is ['//TRIM(RoundSigDigits(SchMax,1))//']. Values must be >= 0.0.')
                      ErrorsFound=.true.
                    ENDIF
                  ENDIF
                ENDIF
                IF (SchMax > 1.0d0) THEN
                  IF (Item1 == 1) THEN
                    CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(AlphaName(1))//  &
                                       '", '//TRIM(cAlphaFieldNames(9))//', maximum is > 1.0')
                    CALL ShowContinueError('Schedule="'//TRIM(AlphaName(9))//'"; '//  &
                                    'Entered min/max range=['//trim(RoundSigDigits(SchMin,1))//','//  &
                                     trim(RoundSigDigits(SchMax,1))//'] Work Efficiency.')
                  ENDIF
                ENDIF
              ENDIF
            ELSEIF (MustInpSch) THEN
              IF (Item1 == 1) THEN
                CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(AlphaName(1))//  &
                                     '", blank '//TRIM(cAlphaFieldNames(9))//' is required for this item.')
                ErrorsFound=.true.
              ENDIF
            ENDIF

            IF (.not. lAlphaFieldBlanks(10) .or. AlphaName(10) /= ' ') THEN
              SELECT CASE (AlphaName(10))
                CASE('CLOTHINGINSULATIONSCHEDULE')
                  People(Loop)%ClothingType = 1
                  People(Loop)%ClothingPtr  = GetScheduleIndex(AlphaName(12))
                  IF (People(Loop)%ClothingPtr == 0) THEN
                    IF (Item1 == 1) THEN
                      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(AlphaName(1))//  &
                                           '", invalid '//TRIM(cAlphaFieldNames(12))//' entered='//TRIM(AlphaName(12)))
                      ErrorsFound=.true.
                    ENDIF
                  ELSE  ! check min/max on schedule
                    SchMin=GetScheduleMinValue(People(Loop)%ClothingPtr)
                    SchMax=GetScheduleMaxValue(People(Loop)%ClothingPtr)
                    IF (SchMin < 0.0d0 .or. SchMax < 0.0d0) THEN
                      IF (SchMin < 0.0d0) THEN
                        IF (Item1 == 1) THEN
                          CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(AlphaName(1))//  &
                                             '", '//TRIM(cAlphaFieldNames(12))//', minimum is < 0.0')
                          CALL ShowContinueError('Schedule="'//TRIM(AlphaName(12))//  &
                                             '". Minimum is ['//TRIM(RoundSigDigits(SchMin,1))//']. Values must be >= 0.0.')
                          ErrorsFound=.true.
                        ENDIF
                      ENDIF
                      IF (SchMax < 0.0d0) THEN
                        IF (Item1 == 1) THEN
                          CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(AlphaName(1))//  &
                                             '", '//TRIM(cAlphaFieldNames(12))//', maximum is < 0.0')
                          CALL ShowContinueError('Schedule="'//TRIM(AlphaName(12))//  &
                                             '". Maximum is ['//TRIM(RoundSigDigits(SchMax,1))//']. Values must be >= 0.0.')
                          ErrorsFound=.true.
                        ENDIF
                      ENDIF
                    ENDIF
                    IF (SchMax > 2.0d0) THEN
                      IF (Item1 == 1) THEN
                        CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(AlphaName(1))//  &
                                           '", '//TRIM(cAlphaFieldNames(12))//', maximum is > 2.0')
                        CALL ShowContinueError('Schedule="'//TRIM(AlphaName(12))//'"; '//  &
                                        'Entered min/max range=['//trim(RoundSigDigits(SchMin,1))//','//  &
                                         trim(RoundSigDigits(SchMax,1))//'] Clothing.')
                      ENDIF
                    ENDIF
                  ENDIF

                CASE('DYNAMICCLOTHINGMODELASHRAE55')
                  People(Loop)%ClothingType = 2

                CASE('CALCULATIONMETHODSCHEDULE')
                  People(Loop)%ClothingType = 3
                  People(Loop)%ClothingMethodPtr  = GetScheduleIndex(AlphaName(11))
                  IF (People(Loop)%ClothingMethodPtr == 0) THEN
                    IF (Item1 == 1) THEN
                      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(AlphaName(1))//  &
                                           '", invalid '//TRIM(cAlphaFieldNames(11))//' entered='//TRIM(AlphaName(11)))
                      ErrorsFound=.true.
                    ENDIF
                  ENDIF
                  IF (CheckScheduleValue(People(Loop)%ClothingMethodPtr,1)) THEN
                    People(Loop)%ClothingPtr  = GetScheduleIndex(AlphaName(12))
                    IF (People(Loop)%ClothingPtr == 0) THEN
                      IF (Item1 == 1) THEN
                        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(AlphaName(1))//  &
                                           '", invalid '//TRIM(cAlphaFieldNames(12))//' entered='//TRIM(AlphaName(12)))
                        ErrorsFound=.true.
                      ENDIF
                    ENDIF
                  ENDIF

                CASE DEFAULT
                    CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(People(Loop)%Name)//  &
                                       '", invalid '//TRIM(cAlphaFieldNames(10))//', value  ='//  &
                                       TRIM(AlphaName(10)))
                    CALL ShowContinueError('...Valid values are "ClothingInsulationSchedule",'//   &
                                           '"DynamicClothingModelASHRAE55a", "CalculationMethodSchedule".')
                    ErrorsFound=.true.
              END SELECT
            ENDIF

            IF (.not. lAlphaFieldBlanks(13)) THEN
              People(Loop)%AirVelocityPtr  = GetScheduleIndex(AlphaName(13))
              IF (People(Loop)%AirVelocityPtr == 0) THEN
                IF (Item1 == 1) THEN
                  CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(AlphaName(1))//  &
                                       '", invalid '//TRIM(cAlphaFieldNames(13))//' entered='//TRIM(AlphaName(13)))
                  ErrorsFound=.true.
                ENDIF
              ELSE  ! check min/max on schedule
                SchMin=GetScheduleMinValue(People(Loop)%AirVelocityPtr)
                SchMax=GetScheduleMaxValue(People(Loop)%AirVelocityPtr)
                IF (SchMin < 0.0d0 .or. SchMax < 0.0d0) THEN
                  IF (SchMin < 0.0d0) THEN
                    IF (Item1 == 1) THEN
                      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(AlphaName(1))//  &
                                         '", '//TRIM(cAlphaFieldNames(13))//', minimum is < 0.0')
                      CALL ShowContinueError('Schedule="'//TRIM(AlphaName(13))//  &
                                         '". Minimum is ['//TRIM(RoundSigDigits(SchMin,1))//']. Values must be >= 0.0.')
                      ErrorsFound=.true.
                    ENDIF
                  ENDIF
                  IF (SchMax < 0.0d0) THEN
                    IF (Item1 == 1) THEN
                      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(AlphaName(1))//  &
                                         '", '//TRIM(cAlphaFieldNames(13))//', maximum is < 0.0')
                      CALL ShowContinueError('Schedule="'//TRIM(AlphaName(13))//  &
                                         '". Maximum is ['//TRIM(RoundSigDigits(SchMax,1))//']. Values must be >= 0.0.')
                      ErrorsFound=.true.
                    ENDIF
                  ENDIF
                ENDIF
              ENDIF
            ELSEIF (MustInpSch) THEN
              IF (Item1 == 1) THEN
                CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(AlphaName(1))//  &
                                     '", blank '//TRIM(cAlphaFieldNames(13))//' is required for this item.')
                ErrorsFound=.true.
              ENDIF
            ENDIF

          ENDIF ! usingthermalcomfort block

        END IF  ! ...end of thermal comfort data IF-THEN block  (NumAlphas > 6)

        IF (People(Loop)%ZonePtr <=0) CYCLE   ! Error, will be caught and terminated later

        ! Object report variables
        CALL SetupOutputVariable('People Occupant Count []',People(Loop)%NumOcc, &
                                 'Zone','Average',People(Loop)%Name)
        CALL SetupOutputVariable('People Radiant Heating Energy [J]',People(Loop)%RadGainEnergy, &
                                 'Zone','Sum',People(Loop)%Name)
        CALL SetupOutputVariable('People Radiant Heating Rate [W]',People(Loop)%RadGainRate, &
                                 'Zone','Average',People(Loop)%Name)
        CALL SetupOutputVariable('People Convective Heating Energy [J]',People(Loop)%ConGainEnergy, &
                                 'Zone','Sum',People(Loop)%Name)
        CALL SetupOutputVariable('People Convective Heating Rate [W]',People(Loop)%ConGainRate, &
                                 'Zone','Average',People(Loop)%Name)
        CALL SetupOutputVariable('People Sensible Heating Energy [J]',People(Loop)%SenGainEnergy, &
                                 'Zone','Sum',People(Loop)%Name)
        CALL SetupOutputVariable('People Sensible Heating Rate [W]',People(Loop)%SenGainRate, &
                                 'Zone','Average',People(Loop)%Name)
        CALL SetupOutputVariable('People Latent Gain Energy [J]',People(Loop)%LatGainEnergy, &
                                 'Zone','Sum',People(Loop)%Name)
        CALL SetupOutputVariable('People Latent Gain Rate [W]',People(Loop)%LatGainRate, &
                                 'Zone','Average',People(Loop)%Name)
        CALL SetupOutputVariable('People Total Heating Energy [J]',People(Loop)%TotGainEnergy, &
                                 'Zone','Sum',People(Loop)%Name)
        CALL SetupOutputVariable('People Total Heating Rate [W]',People(Loop)%TotGainRate, &
                                 'Zone','Average',People(Loop)%Name)
        CALL SetupOutputVariable('People Air Temperature [C]',People(Loop)%TemperatureInZone, &
                                 'Zone','Average',People(Loop)%Name)
        CALL SetupOutputVariable('People Air Relative Humidity [%]',People(Loop)%RelativeHumidityInZone, &
                                 'Zone','Average',People(Loop)%Name)

        ! Zone total report variables
        IF (RepVarSet(People(Loop)%ZonePtr)) THEN
          RepVarSet(People(Loop)%ZonePtr)=.false.
          CALL SetupOutputVariable('Zone People Occupant Count []',ZnRpt(People(Loop)%ZonePtr)%PeopleNumOcc, &
                                   'Zone','Average',Zone(People(Loop)%ZonePtr)%Name)
          CALL SetupOutputVariable('Zone People Radiant Heating Energy [J]',ZnRpt(People(Loop)%ZonePtr)%PeopleRadGain, &
                                   'Zone','Sum',Zone(People(Loop)%ZonePtr)%Name)
          CALL SetupOutputVariable('Zone People Radiant Heating Rate [W]',ZnRpt(People(Loop)%ZonePtr)%PeopleRadGainRate, &
                                   'Zone','Average',Zone(People(Loop)%ZonePtr)%Name)
          CALL SetupOutputVariable('Zone People Convective Heating Energy [J]',ZnRpt(People(Loop)%ZonePtr)%PeopleConGain, &
                                   'Zone','Sum',Zone(People(Loop)%ZonePtr)%Name)
          CALL SetupOutputVariable('Zone People Convective Heating Rate [W]',ZnRpt(People(Loop)%ZonePtr)%PeopleConGainRate, &
                                   'Zone','Average',Zone(People(Loop)%ZonePtr)%Name)
          CALL SetupOutputVariable('Zone People Sensible Heating Energy [J]',ZnRpt(People(Loop)%ZonePtr)%PeopleSenGain, &
                                   'Zone','Sum',Zone(People(Loop)%ZonePtr)%Name)
          CALL SetupOutputVariable('Zone People Sensible Heating Rate [W]',ZnRpt(People(Loop)%ZonePtr)%PeopleSenGainRate, &
                                   'Zone','Average',Zone(People(Loop)%ZonePtr)%Name)
          CALL SetupOutputVariable('Zone People Latent Gain Energy [J]',ZnRpt(People(Loop)%ZonePtr)%PeopleLatGain, &
                                   'Zone','Sum',Zone(People(Loop)%ZonePtr)%Name)
          CALL SetupOutputVariable('Zone People Latent Gain Rate [W]',ZnRpt(People(Loop)%ZonePtr)%PeopleLatGainRate, &
                                   'Zone','Average',Zone(People(Loop)%ZonePtr)%Name)
          CALL SetupOutputVariable('Zone People Total Heating Energy [J]',ZnRpt(People(Loop)%ZonePtr)%PeopleTotGain, &
                                   'Zone','Sum',Zone(People(Loop)%ZonePtr)%Name)
          CALL SetupOutputVariable('Zone People Total Heating Rate [W]',ZnRpt(People(Loop)%ZonePtr)%PeopleTotGainRate, &
                                   'Zone','Average',Zone(People(Loop)%ZonePtr)%Name)
        ENDIF

        IF (AnyEnergyManagementSystemInModel) THEN
          CALL SetupEMSActuator('People', People(Loop)%Name, 'Number of People', '[each]', &
                    People(Loop)%EMSPeopleOn  , People(Loop)%EMSNumberOfPeople )
          CALL SetupEMSInternalVariable( 'People Count Design Level'  , People(Loop)%Name,  '[each]', &
                                       People(Loop)%NumberOfPeople )
        ENDIF

        !setup internal gains
        IF (.not. ErrorsFound)   &
           CALL SetupZoneInternalGain(People(Loop)%ZonePtr, &
                           'People', &
                           People(Loop)%Name, &
                           IntGainTypeOf_People, &
                           ConvectionGainRate       = People(Loop)%ConGainRate,&
                           ThermalRadiationGainRate = People(Loop)%RadGainRate, &
                           LatentGainRate           = People(Loop)%LatGainRate, &
                           CarbonDioxideGainRate    = People(Loop)%CO2GainRate)

      END DO ! Item1 - number of zones
    END DO ! Item - number of people statements
  ENDIF ! TotPeople > 0

  !transfer the nominal number of people in a zone to the tabular reporting
  DO Loop = 1,NumOfZones
    IF (Zone(Loop)%TotOccupants > 0.0d0) THEN
      IF (Zone(Loop)%FloorArea > 0.0d0 .and. Zone(Loop)%FloorArea/Zone(Loop)%TotOccupants < .1d0) THEN
        CALL ShowWarningError(RoutineName//'Zone="'//trim(Zone(Loop)%Name)//'" occupant density is extremely high.')
        IF (Zone(Loop)%FloorArea > 0.0d0) THEN
          CALL ShowContinueError('Occupant Density=['//trim(RoundSigDigits(Zone(Loop)%TotOccupants/Zone(Loop)%FloorArea,0))//  &
              '] person/m2.')
        ENDIF
        CALL ShowContinueError('Occupant Density=['//trim(RoundSigDigits(Zone(Loop)%FloorArea/Zone(Loop)%TotOccupants,3))//  &
           '] m2/person. Problems in Temperature Out of Bounds may result.')
      ENDIF
      maxOccupLoad=0.0d0
      DO Loop1=1,TotPeople
        IF (People(Loop1)%ZonePtr /= Loop) CYCLE
        IF (maxOccupLoad < GetScheduleMaxValue(People(Loop1)%NumberOfPeoplePtr)*People(Loop1)%NumberOfPeople) THEN
          maxOccupLoad=GetScheduleMaxValue(People(Loop1)%NumberOfPeoplePtr)*People(Loop1)%NumberOfPeople
          MaxNumber=People(Loop1)%NumberOfPeoplePtr
          OptionNum=Loop1
        ENDIF
      ENDDO
      IF (maxOccupLoad > Zone(Loop)%TotOccupants) THEN
        IF (Zone(Loop)%FloorArea > 0.0d0 .and. Zone(Loop)%FloorArea/maxOccupLoad < .1d0) THEN
          CALL ShowWarningError(RoutineName//'Zone="'//trim(Zone(Loop)%Name)//  &
               '" occupant density at a maximum schedule value is extremely high.')
          IF (Zone(Loop)%FloorArea > 0.0d0) THEN
            CALL ShowContinueError('Occupant Density=['//  &
               trim(RoundSigDigits(maxOccupLoad/Zone(Loop)%FloorArea,0))//  &
                '] person/m2.')
          ENDIF
          CALL ShowContinueError('Occupant Density=['//trim(RoundSigDigits(Zone(Loop)%FloorArea/maxOccupLoad,3))//  &
             '] m2/person. Problems in Temperature Out of Bounds may result.')
          CALL ShowContinueError('Check values in People='//trim(People(OptionNum)%Name)//', Number of People Schedule='//  &
             trim(GetScheduleName(MaxNumber)))
        ENDIF
      ENDIF
    ENDIF

    IF (Zone(Loop)%IsNominalControlled) THEN !conditioned zones only
      IF (Zone(Loop)%TotOccupants .GT. 0.0d0) THEN
        Zone(Loop)%isNominalOccupied = .true.
        CALL PreDefTableEntry(pdchOaoNomNumOcc1,Zone(Loop)%name,Zone(Loop)%TotOccupants)
        CALL PreDefTableEntry(pdchOaoNomNumOcc2,Zone(Loop)%name,Zone(Loop)%TotOccupants)
      END IF
    END IF
  END DO

  RepVarSet=.true.
  CurrentModuleObject='Lights'
  NumLightsStatements=GetNumObjectsFound(CurrentModuleObject)
  ALLOCATE(LightsObjects(NumLightsStatements))

  TotLights=0
  ErrFlag=.false.
  DO Item=1,NumLightsStatements
    CALL GetObjectItem(CurrentModuleObject,Item,AlphaName,NumAlpha,IHGNumbers,NumNumber,IOStat,  &
                   AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                   AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
    IsNotOK = .FALSE.
    IsBlank = .FALSE.
    CALL VerifyName(AlphaName(1),LightsObjects%Name,Item-1,IsNotOK,IsBlank,TRIM(CurrentModuleObject)//' Name')
    IF (IsNotOK) THEN
      ErrorsFound = .TRUE.
      ErrFlag=.true.
      IF (IsBlank) AlphaName(1) = 'xxxxx'
    END IF
    LightsObjects(Item)%Name = AlphaName(1)

    Item1=FindItemInList(AlphaName(2),Zone%Name,NumOfZones)
    ZLItem=0
    IF (Item1 == 0 .and. NumOfZoneLists > 0) &
        ZLItem=FindItemInList(AlphaName(2),ZoneList%Name,NumOfZoneLists)
    IF (Item1 > 0) THEN
      LightsObjects(Item)%StartPtr=TotLights+1
      TotLights=TotLights+1
      LightsObjects(Item)%NumOfZones=1
      LightsObjects(Item)%ZoneListActive=.false.
      LightsObjects(Item)%ZoneOrZoneListPtr=Item1
    ELSEIF (ZLItem > 0) THEN
      LightsObjects(Item)%StartPtr=TotLights+1
      TotLights=TotLights+ZoneList(ZLItem)%NumOfZones
      LightsObjects(Item)%NumOfZones=ZoneList(ZLItem)%NumOfZones
      LightsObjects(Item)%ZoneListActive=.true.
      LightsObjects(Item)%ZoneOrZoneListPtr=ZLItem
    ELSE
      CALL ShowSevereError(trim(CurrentModuleObject)//'="'//trim(AlphaName(1))//'" invalid '//  &
           trim(cAlphaFieldNames(2))//'="'//trim(AlphaName(2))//'" not found.')
      ErrorsFound=.true.
      ErrFlag=.true.
    ENDIF
  ENDDO

  IF (ErrFlag) THEN
    CALL ShowSevereError(RoutineName//'Errors with invalid names in '//trim(CurrentModuleObject)//  &
       ' objects.')
    CALL ShowContinueError('...These will not be read in.  Other errors may occur.')
    TotLights=0
  ENDIF

  ALLOCATE(Lights(TotLights))

  IF (TotLights > 0) THEN
    Loop=0
    DO Item = 1, NumLightsStatements
      AlphaName  = Blank
      IHGNumbers = 0.0d0

      CALL GetObjectItem(CurrentModuleObject,Item,AlphaName,NumAlpha,IHGNumbers,NumNumber,IOStat,  &
                     AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                     AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

      DO Item1=1,LightsObjects(Item)%NumOfZones
        Loop=Loop+1
        IF (.not. LightsObjects(Item)%ZoneListActive) THEN
          Lights(Loop)%Name = AlphaName(1)
          Lights(Loop)%ZonePtr = LightsObjects(Item)%ZoneOrZoneListPtr
        ELSE
          CALL CheckCreatedZoneItemName(RoutineName,CurrentModuleObject,  &
                                        Zone(ZoneList(LightsObjects(Item)%ZoneOrZoneListPtr)%Zone(Item1))%Name,  &
                                        ZoneList(LightsObjects(Item)%ZoneOrZoneListPtr)%MaxZoneNameLength,  &
                                        LightsObjects(Item)%Name,     &
                                        Lights%Name,           &
                                        Loop-1,                       &
                                        Lights(Loop)%Name,            &
                                        ErrFlag)
          Lights(Loop)%ZonePtr = ZoneList(LightsObjects(Item)%ZoneOrZoneListPtr)%Zone(Item1)
          IF (ErrFlag) ErrorsFound=.true.
        ENDIF

        Lights(Loop)%SchedPtr=GetScheduleIndex(AlphaName(3))
        SchMin=0.0d0
        SchMax=0.0d0
        IF (Lights(Loop)%SchedPtr == 0) THEN
          IF (Item1 == 1) THEN
            IF (lAlphaFieldBlanks(3)) THEN
              CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(AlphaName(1))//  &
                                   '", '//TRIM(cAlphaFieldNames(3))//' is required.')
            ELSE
              CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(AlphaName(1))//  &
                                   '", invalid '//TRIM(cAlphaFieldNames(3))//' entered='//TRIM(AlphaName(3)))
            ENDIF
            ErrorsFound=.true.
          ENDIF
        ELSE  ! check min/max on schedule
          SchMin=GetScheduleMinValue(Lights(Loop)%SchedPtr)
          SchMax=GetScheduleMaxValue(Lights(Loop)%SchedPtr)
          IF (SchMin < 0.0d0 .or. SchMax < 0.0d0) THEN
            IF (Item1 == 1) THEN
              IF (SchMin < 0.0d0) THEN
                CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(AlphaName(1))//  &
                                   '", '//TRIM(cAlphaFieldNames(3))//', minimum is < 0.0')
                CALL ShowContinueError('Schedule="'//TRIM(AlphaName(3))//  &
                                   '". Minimum is ['//TRIM(RoundSigDigits(SchMin,1))//']. Values must be >= 0.0.')
                ErrorsFound=.true.
              ENDIF
            ENDIF
            IF (Item1 == 1) THEN
              IF (SchMax < 0.0d0) THEN
                CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(AlphaName(1))//  &
                                   '", '//TRIM(cAlphaFieldNames(3))//', maximum is < 0.0')
                CALL ShowContinueError('Schedule="'//TRIM(AlphaName(3))//  &
                                   '". Maximum is ['//TRIM(RoundSigDigits(SchMax,1))//']. Values must be >= 0.0.')
                ErrorsFound=.true.
              ENDIF
            ENDIF
          ENDIF
        ENDIF

        ! Lights Design Level calculation method.
        SELECT CASE (AlphaName(4))
          CASE('LIGHTINGLEVEL')
            Lights(Loop)%DesignLevel=IHGNumbers(1)
            IF (lNumericFieldBlanks(1)) THEN
              CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(Lights(Loop)%Name)//  &
                '", specifies '//TRIM(cNumericFieldNames(1))//', but that field is blank.  0 Lights will result.')
            ENDIF

          CASE('WATTS/AREA')
            IF (Lights(Loop)%ZonePtr /= 0) THEN
              IF (IHGNumbers(2) >= 0.0d0) THEN
                Lights(Loop)%DesignLevel=IHGNumbers(2)*Zone(Lights(Loop)%ZonePtr)%FloorArea
                IF (Zone(Lights(Loop)%ZonePtr)%FloorArea <= 0.0d0) THEN
                  CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(Lights(Loop)%Name)//  &
                    '", specifies '//TRIM(cNumericFieldNames(2))//', but Zone Floor Area = 0.  0 Lights will result.')
                ENDIF
              ELSE
                CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(Lights(Loop)%Name)//  &
                                 '", invalid '//TRIM(cNumericFieldNames(2))//', value  [<0.0]='//  &
                                 TRIM(RoundSigDigits(IHGNumbers(2),3)))
                ErrorsFound=.true.
              ENDIF
            ENDIF
            IF (lNumericFieldBlanks(2)) THEN
              CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(Lights(Loop)%Name)//  &
                '", specifies '//TRIM(cNumericFieldNames(2))//', but that field is blank.  0 Lights will result.')
            ENDIF

          CASE('WATTS/PERSON')
            IF (Lights(Loop)%ZonePtr /= 0) THEN
              IF (IHGNumbers(3) >= 0.0d0) THEN
                Lights(Loop)%DesignLevel=IHGNumbers(3)*Zone(Lights(Loop)%ZonePtr)%TotOccupants
                IF (Zone(Lights(Loop)%ZonePtr)%TotOccupants <= 0.0d0) THEN
                  CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(Lights(Loop)%Name)//  &
                    '", specifies '//TRIM(cNumericFieldNames(2))//', but Total Occupants = 0.  0 Lights will result.')
                ENDIF
              ELSE
                CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(Lights(Loop)%Name)//  &
                                 '", invalid '//TRIM(cNumericFieldNames(3))//', value  [<0.0]='//  &
                                 TRIM(RoundSigDigits(IHGNumbers(3),3)))
                ErrorsFound=.true.
              ENDIF
            ENDIF
            IF (lNumericFieldBlanks(3)) THEN
              CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(Lights(Loop)%Name)//  &
                '", specifies '//TRIM(cNumericFieldNames(3))//', but that field is blank.  0 Lights will result.')
            ENDIF

          CASE DEFAULT
            IF (Item1 == 1) THEN
              CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(AlphaName(1))//  &
                                   '", invalid '//TRIM(cAlphaFieldNames(4))//', value  ='//  &
                                   TRIM(AlphaName(4)))
              CALL ShowContinueError('...Valid values are "LightingLevel", "Watts/Area", "Watts/Person".')
              ErrorsFound=.true.
            ENDIF
        END SELECT

        ! Calculate nominal min/max lighting level
        Lights(Loop)%NomMinDesignLevel=Lights(Loop)%DesignLevel*SchMin
        Lights(Loop)%NomMaxDesignLevel=Lights(Loop)%DesignLevel*SchMax

        Lights(Loop)%FractionReturnAir=IHGNumbers(4)
        Lights(Loop)%FractionRadiant=IHGNumbers(5)
        Lights(Loop)%FractionShortWave=IHGNumbers(6)
        Lights(Loop)%FractionReplaceable=IHGNumbers(7)
        Lights(Loop)%FractionReturnAirPlenTempCoeff1=IHGNumbers(8)
        Lights(Loop)%FractionReturnAirPlenTempCoeff2=IHGNumbers(9)

        Lights(Loop)%FractionConvected=1.0d0 - (Lights(Loop)%FractionReturnAir +   &
                                              Lights(Loop)%FractionRadiant   +   &
                                              Lights(Loop)%FractionShortWave)
        IF (ABS(Lights(Loop)%FractionConvected) <= .001d0) Lights(Loop)%FractionConvected=0.0d0
        IF (Lights(Loop)%FractionConvected < 0.0d0) THEN
          IF (Item1 == 1) THEN
            CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(AlphaName(1))//  &
                                   '", Sum of Fractions > 1.0')
            ErrorsFound=.true.
          ENDIF
        ENDIF

        ! Note: if FractionReturnAirIsCalculated = Yes and there is a return-air plenum:
        ! (1) The input values of FractionReturnAir, FractionRadiant and FractionShortWave, and the
        ! value of FractionConvected calculated from these are used in the zone sizing calculations;
        ! (2) in the regular calculation, FractionReturnAir is calculated each time step in
        ! Subr. InitInternalHeatGains as a function of the zone's return plenum air temperature
        ! using FractionReturnAirPlenTempCoeff1 and FractionReturnAirPlenTempCoeff2; then
        ! FractionRadiant and FractionConvected are adjusted from their input values such that
        ! FractionReturnAir + FractionRadiant + FractionShortWave + FractionConvected = 1.0, assuming
        ! FractionShortWave is constant and equal to its input value.

        IF (NumAlpha > 4) THEN
          Lights(Loop)%EndUseSubcategory = AlphaName(5)
        ELSE
          Lights(Loop)%EndUseSubcategory = 'General'
        END IF

        IF (lAlphaFieldBlanks(6)) THEN
          Lights(Loop)%FractionReturnAirIsCalculated=.false.
        ELSEIF (AlphaName(6) /= 'YES' .and. AlphaName(6) /= 'NO') THEN
          IF (Item1 == 1) THEN
            CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(AlphaName(1))//  &
                                   '", invalid '//TRIM(cAlphaFieldNames(6))//', value  ='//  &
                                   TRIM(AlphaName(6)))
            CALL ShowContinueError('.. Return Air Fraction from Plenum will NOT be calculated.')
          ENDIF
          Lights(Loop)%FractionReturnAirIsCalculated=.false.
        ELSE
          Lights(Loop)%FractionReturnAirIsCalculated=(AlphaName(6) == 'YES')
        ENDIF

        IF (Lights(Loop)%ZonePtr <=0) CYCLE   ! Error, will be caught and terminated later

        ! Object report variables
        CALL SetupOutputVariable('Lights Electric Power [W]',Lights(Loop)%Power, &
                                  'Zone','Average',Lights(Loop)%Name)

        CALL SetupOutputVariable('Lights Radiant Heating Energy [J]',Lights(Loop)%RadGainEnergy, &
                                  'Zone','Sum',Lights(Loop)%Name)
        CALL SetupOutputVariable('Lights Radiant Heating Rate [W]',Lights(Loop)%RadGainRate, &
                                  'Zone','Average',Lights(Loop)%Name)
        CALL SetupOutputVariable('Lights Visible Radiation Heating Energy [J]',Lights(Loop)%VisGainEnergy, &
                                  'Zone','Sum',Lights(Loop)%Name)
        CALL SetupOutputVariable('Lights Visible Radiation Heating Rate [W]',Lights(Loop)%VisGainRate, &
                                  'Zone','Average',Lights(Loop)%Name)
        CALL SetupOutputVariable('Lights Convective Heating Energy [J]',Lights(Loop)%ConGainEnergy, &
                                  'Zone','Sum',Lights(Loop)%Name)
        CALL SetupOutputVariable('Lights Convective Heating Rate [W]',Lights(Loop)%ConGainRate, &
                                  'Zone','Average',Lights(Loop)%Name)
        CALL SetupOutputVariable('Lights Return Air Heating Energy [J]',Lights(Loop)%RetAirGainEnergy, &
                                  'Zone','Sum',Lights(Loop)%Name)
        CALL SetupOutputVariable('Lights Return Air Heating Rate [W]',Lights(Loop)%RetAirGainRate, &
                                  'Zone','Average',Lights(Loop)%Name)
        CALL SetupOutputVariable('Lights Total Heating Energy [J]',Lights(Loop)%TotGainEnergy, &
                                  'Zone','Sum',Lights(Loop)%Name)
        CALL SetupOutputVariable('Lights Total Heating Rate [W]',Lights(Loop)%TotGainRate, &
                                  'Zone','Average',Lights(Loop)%Name)
        CALL SetupOutputVariable('Lights Electric Energy [J]',Lights(Loop)%Consumption, &
                                  'Zone','Sum',Lights(Loop)%Name,ResourceTypeKey='Electricity', &
                                  GroupKey='Building',ZoneKey=Zone(Lights(Loop)%ZonePtr)%Name, &
                                  EndUseKey='InteriorLights',EndUseSubKey=Lights(Loop)%EndUseSubcategory, &
                                  ZoneMult=Zone(Lights(Loop)%ZonePtr)%Multiplier, &
                                  ZoneListMult=Zone(Lights(Loop)%ZonePtr)%ListMultiplier)

        ! Zone total report variables
        IF (RepVarSet(Lights(Loop)%ZonePtr)) THEN
          RepVarSet(Lights(Loop)%ZonePtr)=.false.
          CALL SetupOutputVariable('Zone Lights Electric Power [W]',ZnRpt(Lights(Loop)%ZonePtr)%LtsPower, &
                                    'Zone','Average',Zone(Lights(Loop)%ZonePtr)%Name)
          CALL SetupOutputVariable('Zone Lights Electric Energy [J]',ZnRpt(Lights(Loop)%ZonePtr)%LtsElecConsump,  &
                                    'Zone','Sum',Zone(Lights(Loop)%ZonePtr)%Name)
          CALL SetupOutputVariable('Zone Lights Radiant Heating Energy [J]',ZnRpt(Lights(Loop)%ZonePtr)%LtsRadGain, &
                                    'Zone','Sum',Zone(Lights(Loop)%ZonePtr)%Name)
          CALL SetupOutputVariable('Zone Lights Radiant Heating Rate [W]',ZnRpt(Lights(Loop)%ZonePtr)%LtsRadGainRate, &
                                    'Zone','Average',Zone(Lights(Loop)%ZonePtr)%Name)
          CALL SetupOutputVariable('Zone Lights Visible Radiation Heating Energy [J]',ZnRpt(Lights(Loop)%ZonePtr)%LtsVisGain,  &
                                    'Zone','Sum',Zone(Lights(Loop)%ZonePtr)%Name)
          CALL SetupOutputVariable('Zone Lights Visible Radiation Heating Rate [W]',ZnRpt(Lights(Loop)%ZonePtr)%LtsVisGainRate,  &
                                    'Zone','Average',Zone(Lights(Loop)%ZonePtr)%Name)
          CALL SetupOutputVariable('Zone Lights Convective Heating Energy [J]',ZnRpt(Lights(Loop)%ZonePtr)%LtsConGain,  &
                                    'Zone','Sum',Zone(Lights(Loop)%ZonePtr)%Name)
          CALL SetupOutputVariable('Zone Lights Convective Heating Rate [W]',ZnRpt(Lights(Loop)%ZonePtr)%LtsConGainRate,  &
                                    'Zone','Average',Zone(Lights(Loop)%ZonePtr)%Name)
          CALL SetupOutputVariable('Zone Lights Return Air Heating Energy [J]',ZnRpt(Lights(Loop)%ZonePtr)%LtsRetAirGain,  &
                                    'Zone','Sum',Zone(Lights(Loop)%ZonePtr)%Name)
          CALL SetupOutputVariable('Zone Lights Return Air Heating Rate [W]',ZnRpt(Lights(Loop)%ZonePtr)%LtsRetAirGainRate,  &
                                    'Zone','Average',Zone(Lights(Loop)%ZonePtr)%Name)
          CALL SetupOutputVariable('Zone Lights Total Heating Energy [J]',ZnRpt(Lights(Loop)%ZonePtr)%LtsTotGain,  &
                                    'Zone','Sum',Zone(Lights(Loop)%ZonePtr)%Name)
          CALL SetupOutputVariable('Zone Lights Total Heating Rate [W]',ZnRpt(Lights(Loop)%ZonePtr)%LtsTotGainRate,  &
                                    'Zone','Average',Zone(Lights(Loop)%ZonePtr)%Name)
        END IF

        IF (AnyEnergyManagementSystemInModel) Then
          CALL SetupEMSActuator('Lights', Lights(Loop)%Name, 'Electric Power Level', '[W]', &
                    Lights(Loop)%EMSLightsOn  , Lights(Loop)%EMSLightingPower )
          CALL SetupEMSInternalVariable( 'Lighting Power Design Level', Lights(Loop)%Name, '[W]' ,&
                                           Lights(Loop)%DesignLevel )
        ENDIF ! EMS
                !setup internal gains
        IF (.not. ErrorsFound)   &
          CALL SetupZoneInternalGain(Lights(Loop)%ZonePtr, &
                           'Lights', &
                           Lights(Loop)%Name, &
                           IntGainTypeOf_Lights, &
                           ConvectionGainRate          = Lights(Loop)%ConGainRate,&
                           ThermalRadiationGainRate    = Lights(Loop)%RadGainRate, &
                           ReturnAirConvectionGainRate = Lights(Loop)%RetAirGainRate)


        ! send values to predefined lighting summary report
        liteName = Lights(loop)%Name
        zonePt = Lights(loop)%ZonePtr
        mult = Zone(zonePt)%Multiplier * Zone(zonePt)%ListMultiplier
        sumArea = sumArea + Zone(zonePt)%FloorArea * mult
        sumPower = sumPower + Lights(loop)%DesignLevel * mult
        CALL PreDefTableEntry(pdchInLtZone,liteName,Zone(zonePt)%Name)
        IF(Zone(zonePt)%FloorArea .GT. 0.0d0)THEN
          CALL PreDefTableEntry(pdchInLtDens,liteName,Lights(loop)%DesignLevel / Zone(zonePt)%FloorArea, 4)
        ELSE
          CALL PreDefTableEntry(pdchInLtDens,liteName,constant_zero, 4)
        END IF
        CALL PreDefTableEntry(pdchInLtArea,liteName,Zone(zonePt)%FloorArea * mult)
        CALL PreDefTableEntry(pdchInLtPower,liteName,Lights(loop)%DesignLevel * mult)
        CALL PreDefTableEntry(pdchInLtEndUse,liteName,Lights(loop)%EndUseSubcategory)
        CALL PreDefTableEntry(pdchInLtSchd,liteName,GetScheduleName(Lights(loop)%SchedPtr))
        CALL PreDefTableEntry(pdchInLtRetAir,liteName,Lights(loop)%FractionReturnAir,4)
      END DO ! Item1 - zones
    END DO ! Item = Number of Lights Objects
  END IF  ! TotLights > 0 check
  ! add total line to lighting summary table
  IF(sumArea .GT. 0.0d0)THEN
    CALL PreDefTableEntry(pdchInLtDens,'Interior Lighting Total',sumPower/sumArea, 4)  !** line 792
  ELSE
    CALL PreDefTableEntry(pdchInLtDens,'Interior Lighting Total',constant_zero, 4)
  END IF
  CALL PreDefTableEntry(pdchInLtArea,'Interior Lighting Total',sumArea)
  CALL PreDefTableEntry(pdchInLtPower,'Interior Lighting Total',sumPower)

  RepVarSet=.true.
  CurrentModuleObject='ElectricEquipment'
  NumZoneElectricStatements=GetNumObjectsFound(CurrentModuleObject)
  ALLOCATE(ZoneElectricObjects(NumZoneElectricStatements))

  TotElecEquip=0
  ErrFlag=.false.
  DO Item=1,NumZoneElectricStatements
    CALL GetObjectItem(CurrentModuleObject,Item,AlphaName,NumAlpha,IHGNumbers,NumNumber,IOStat,  &
                   AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                   AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
    IsNotOK = .FALSE.
    IsBlank = .FALSE.
    CALL VerifyName(AlphaName(1),ZoneElectricObjects%Name,Item-1,IsNotOK,IsBlank,TRIM(CurrentModuleObject)//' Name')
    IF (IsNotOK) THEN
      ErrorsFound = .TRUE.
      ErrFlag=.true.
      IF (IsBlank) AlphaName(1) = 'xxxxx'
    END IF
    ZoneElectricObjects(Item)%Name = AlphaName(1)

    Item1=FindItemInList(AlphaName(2),Zone%Name,NumOfZones)
    ZLItem=0
    IF (Item1 == 0 .and. NumOfZoneLists > 0) &
        ZLItem=FindItemInList(AlphaName(2),ZoneList%Name,NumOfZoneLists)
    IF (Item1 > 0) THEN
      ZoneElectricObjects(Item)%StartPtr=TotElecEquip+1
      TotElecEquip=TotElecEquip+1
      ZoneElectricObjects(Item)%NumOfZones=1
      ZoneElectricObjects(Item)%ZoneListActive=.false.
      ZoneElectricObjects(Item)%ZoneOrZoneListPtr=Item1
    ELSEIF (ZLItem > 0) THEN
      ZoneElectricObjects(Item)%StartPtr=TotElecEquip+1
      TotElecEquip=TotElecEquip+ZoneList(ZLItem)%NumOfZones
      ZoneElectricObjects(Item)%NumOfZones=ZoneList(ZLItem)%NumOfZones
      ZoneElectricObjects(Item)%ZoneListActive=.true.
      ZoneElectricObjects(Item)%ZoneOrZoneListPtr=ZLItem
    ELSE
      CALL ShowSevereError(trim(CurrentModuleObject)//'="'//trim(AlphaName(1))//'" invalid '//  &
           trim(cAlphaFieldNames(2))//'="'//trim(AlphaName(2))//'" not found.')
      ErrorsFound=.true.
      ErrFlag=.true.
    ENDIF
  ENDDO

  IF (ErrFlag) THEN
    CALL ShowSevereError(RoutineName//'Errors with invalid names in '//trim(CurrentModuleObject)//  &
       ' objects.')
    CALL ShowContinueError('...These will not be read in.  Other errors may occur.')
    TotElecEquip=0
  ENDIF

  ALLOCATE(ZoneElectric(TotElecEquip))

  IF (TotElecEquip > 0) THEN
    Loop=0
    DO Item = 1, NumZoneElectricStatements
      AlphaName  = Blank
      IHGNumbers = 0.0d0

      CALL GetObjectItem(CurrentModuleObject,Item,AlphaName,NumAlpha,IHGNumbers,NumNumber,IOStat,  &
                     AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                     AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

      DO Item1=1,ZoneElectricObjects(Item)%NumOfZones
        Loop=Loop+1
        IF (.not. ZoneElectricObjects(Item)%ZoneListActive) THEN
          ZoneElectric(Loop)%Name = AlphaName(1)
          ZoneElectric(Loop)%ZonePtr = ZoneElectricObjects(Item)%ZoneOrZoneListPtr
        ELSE
          CALL CheckCreatedZoneItemName(RoutineName,CurrentModuleObject,  &
                                        Zone(ZoneList(ZoneElectricObjects(Item)%ZoneOrZoneListPtr)%Zone(Item1))%Name,  &
                                        ZoneList(ZoneElectricObjects(Item)%ZoneOrZoneListPtr)%MaxZoneNameLength,  &
                                        ZoneElectricObjects(Item)%Name,     &
                                        ZoneElectric%Name,           &
                                        Loop-1,                       &
                                        ZoneElectric(Loop)%Name,            &
                                        ErrFlag)
          ZoneElectric(Loop)%ZonePtr = ZoneList(ZoneElectricObjects(Item)%ZoneOrZoneListPtr)%Zone(Item1)
          IF (ErrFlag) ErrorsFound=.true.
        ENDIF

        ZoneElectric(Loop)%SchedPtr=GetScheduleIndex(AlphaName(3))
        SchMin=0.0d0
        SchMax=0.0d0
        IF (ZoneElectric(Loop)%SchedPtr == 0) THEN
          IF (lAlphaFieldBlanks(3)) THEN
            CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(AlphaName(1))//  &
                                 '", '//TRIM(cAlphaFieldNames(3))//' is required.')
          ELSE
            CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(AlphaName(1))//  &
                                 '", invalid '//TRIM(cAlphaFieldNames(3))//' entered='//TRIM(AlphaName(3)))
          ENDIF
          ErrorsFound=.true.
        ELSE  ! check min/max on schedule
          SchMin=GetScheduleMinValue(ZoneElectric(Loop)%SchedPtr)
          SchMax=GetScheduleMaxValue(ZoneElectric(Loop)%SchedPtr)
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

        ! Electric equipment design level calculation method.
        SELECT CASE (AlphaName(4))
          CASE('EQUIPMENTLEVEL')
            ZoneElectric(Loop)%DesignLevel=IHGNumbers(1)
            IF (lNumericFieldBlanks(1)) THEN
              CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(AlphaName(1))//  &
                '", specifies '//TRIM(cNumericFieldNames(1))//', but that field is blank.  0 Electric Equipment will result.')
            ENDIF

          CASE('WATTS/AREA')
            IF (ZoneElectric(Loop)%ZonePtr /= 0) THEN
              IF (IHGNumbers(2) >= 0.0d0) THEN
                ZoneElectric(Loop)%DesignLevel=IHGNumbers(2)*Zone(ZoneElectric(Loop)%ZonePtr)%FloorArea
                IF (Zone(ZoneElectric(Loop)%ZonePtr)%FloorArea <= 0.0d0) THEN
                  CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(AlphaName(1))//  &
                    '", specifies '//TRIM(cNumericFieldNames(2))//', but Zone Floor Area = 0.  0 Electric Equipment will result.')
                ENDIF
              ELSE
                CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(AlphaName(1))//  &
                                 '", invalid '//TRIM(cNumericFieldNames(2))//', value  [<0.0]='//  &
                                 TRIM(RoundSigDigits(IHGNumbers(2),3)))
                ErrorsFound=.true.
              ENDIF
            ENDIF
            IF (lNumericFieldBlanks(2)) THEN
              CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(AlphaName(1))//  &
                '", specifies '//TRIM(cNumericFieldNames(2))//', but that field is blank.  0 Electric Equipment will result.')
            ENDIF

          CASE('WATTS/PERSON')
            IF (ZoneElectric(Loop)%ZonePtr /= 0) THEN
              IF (IHGNumbers(3) >= 0.0d0) THEN
                ZoneElectric(Loop)%DesignLevel=IHGNumbers(3)*Zone(ZoneElectric(Loop)%ZonePtr)%TotOccupants
                IF (Zone(ZoneElectric(Loop)%ZonePtr)%TotOccupants <= 0.0d0) THEN
                  CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(AlphaName(1))//  &
                    '", specifies '//TRIM(cNumericFieldNames(2))//', but Total Occupants = 0.  0 Electric Equipment will result.')
                ENDIF
              ELSE
                CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(AlphaName(1))//  &
                                 '", invalid '//TRIM(cNumericFieldNames(3))//', value  [<0.0]='//  &
                                 TRIM(RoundSigDigits(IHGNumbers(3),3)))
                ErrorsFound=.true.
              ENDIF
            ENDIF
            IF (lNumericFieldBlanks(3)) THEN
              CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(AlphaName(1))//  &
                '", specifies '//TRIM(cNumericFieldNames(3))//', but that field is blank.  0 Electric Equipment will result.')
            ENDIF

          CASE DEFAULT
            IF (Item1 == 1) THEN
              CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(AlphaName(1))//  &
                                 '", invalid '//TRIM(cAlphaFieldNames(4))//', value  ='//  &
                                 TRIM(AlphaName(4)))
              CALL ShowContinueError('...Valid values are "EquipmentLevel", "Watts/Area", "Watts/Person".')
              ErrorsFound=.true.
            ENDIF
        END SELECT

        ! Calculate nominal min/max equipment level
        ZoneElectric(Loop)%NomMinDesignLevel=ZoneElectric(Loop)%DesignLevel*SchMin
        ZoneElectric(Loop)%NomMaxDesignLevel=ZoneElectric(Loop)%DesignLevel*SchMax

        ZoneElectric(Loop)%FractionLatent=IHGNumbers(4)
        ZoneElectric(Loop)%FractionRadiant=IHGNumbers(5)
        ZoneElectric(Loop)%FractionLost=IHGNumbers(6)
                   ! FractionConvected is a calculated field
        ZoneElectric(Loop)%FractionConvected=1.0d0 - (ZoneElectric(Loop)%FractionLatent +   &
                                                    ZoneElectric(Loop)%FractionRadiant +   &
                                                    ZoneElectric(Loop)%FractionLost)
        IF (ABS(ZoneElectric(Loop)%FractionConvected) <= .001d0) ZoneElectric(Loop)%FractionConvected=0.0d0
        IF (ZoneElectric(Loop)%FractionConvected < 0.0d0) THEN
          CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(AlphaName(1))//  &
                                 '", Sum of Fractions > 1.0')
          ErrorsFound=.true.
        ENDIF

        IF (NumAlpha > 4) THEN
          ZoneElectric(Loop)%EndUseSubcategory = AlphaName(5)
        ELSE
          ZoneElectric(Loop)%EndUseSubcategory = 'General'
        END IF

        IF (ZoneElectric(Loop)%ZonePtr <=0) CYCLE   ! Error, will be caught and terminated later

        ! Object report variables
        CALL SetupOutputVariable('Electric Equipment Electric Power [W]',ZoneElectric(Loop)%Power, &
                                 'Zone','Average',ZoneElectric(Loop)%Name)
        CALL SetupOutputVariable('Electric Equipment Electric Energy [J]',ZoneElectric(Loop)%Consumption, &
                                 'Zone','Sum',ZoneElectric(Loop)%Name,ResourceTypeKey='Electricity', &
                                 GroupKey='Building',ZoneKey=Zone(ZoneElectric(Loop)%ZonePtr)%Name, &
                                 EndUseKey='InteriorEquipment',EndUseSubKey=ZoneElectric(Loop)%EndUseSubcategory, &
                                 ZoneMult=Zone(ZoneElectric(Loop)%ZonePtr)%Multiplier, &
                                 ZoneListMult=Zone(ZoneElectric(Loop)%ZonePtr)%ListMultiplier)

        CALL SetupOutputVariable('Electric Equipment Radiant Heating Energy [J]',ZoneElectric(Loop)%RadGainEnergy, &
                                 'Zone','Sum',ZoneElectric(Loop)%Name)
        CALL SetupOutputVariable('Electric Equipment Radiant Heating Rate [W]',ZoneElectric(Loop)%RadGainRate, &
                                 'Zone','Average',ZoneElectric(Loop)%Name)
        CALL SetupOutputVariable('Electric Equipment Convective Heating Energy [J]',ZoneElectric(Loop)%ConGainEnergy, &
                                 'Zone','Sum',ZoneElectric(Loop)%Name)
        CALL SetupOutputVariable('Electric Equipment Convective Heating Rate [W]',ZoneElectric(Loop)%ConGainRate, &
                                 'Zone','Average',ZoneElectric(Loop)%Name)
        CALL SetupOutputVariable('Electric Equipment Latent Gain Energy [J]',ZoneElectric(Loop)%LatGainEnergy, &
                                 'Zone','Sum',ZoneElectric(Loop)%Name)
        CALL SetupOutputVariable('Electric Equipment Latent Gain Rate [W]',ZoneElectric(Loop)%LatGainRate, &
                                 'Zone','Average',ZoneElectric(Loop)%Name)
        CALL SetupOutputVariable('Electric Equipment Lost Heat Energy [J]',ZoneElectric(Loop)%LostEnergy, &
                                 'Zone','Sum',ZoneElectric(Loop)%Name)
        CALL SetupOutputVariable('Electric Equipment Lost Heat Rate [W]',ZoneElectric(Loop)%LostRate, &
                                 'Zone','Average',ZoneElectric(Loop)%Name)
        CALL SetupOutputVariable('Electric Equipment Total Heating Energy [J]',ZoneElectric(Loop)%TotGainEnergy, &
                                 'Zone','Sum',ZoneElectric(Loop)%Name)
        CALL SetupOutputVariable('Electric Equipment Total Heating Rate [W]',ZoneElectric(Loop)%TotGainRate, &
                                 'Zone','Average',ZoneElectric(Loop)%Name)

        ! Zone total report variables
        IF (RepVarSet(ZoneElectric(Loop)%ZonePtr)) THEN
          RepVarSet(ZoneElectric(Loop)%ZonePtr)=.false.
          CALL SetupOutputVariable('Zone Electric Equipment Electric Power [W]',  &
             ZnRpt(ZoneElectric(Loop)%ZonePtr)%ElecPower, &
                                   'Zone','Average',Zone(ZoneElectric(Loop)%ZonePtr)%Name)
          CALL SetupOutputVariable('Zone Electric Equipment Electric Energy [J]',  &
             ZnRpt(ZoneElectric(Loop)%ZonePtr)%ElecConsump, &
                                   'Zone','Sum',Zone(ZoneElectric(Loop)%ZonePtr)%Name)

          CALL SetupOutputVariable('Zone Electric Equipment Radiant Heating Energy [J]',  &
             ZnRpt(ZoneElectric(Loop)%ZonePtr)%ElecRadGain, &
                                   'Zone','Sum',Zone(ZoneElectric(Loop)%ZonePtr)%Name)
          CALL SetupOutputVariable('Zone Electric Equipment Radiant Heating Rate [W]',  &
             ZnRpt(ZoneElectric(Loop)%ZonePtr)%ElecRadGainRate, &
                                   'Zone','Average',Zone(ZoneElectric(Loop)%ZonePtr)%Name)
          CALL SetupOutputVariable('Zone Electric Equipment Convective Heating Energy [J]',  &
             ZnRpt(ZoneElectric(Loop)%ZonePtr)%ElecConGain, &
                                   'Zone','Sum',Zone(ZoneElectric(Loop)%ZonePtr)%Name)
          CALL SetupOutputVariable('Zone Electric Equipment Convective Heating Rate [W]',  &
             ZnRpt(ZoneElectric(Loop)%ZonePtr)%ElecConGainRate, &
                                   'Zone','Average',Zone(ZoneElectric(Loop)%ZonePtr)%Name)
          CALL SetupOutputVariable('Zone Electric Equipment Latent Gain Energy [J]',  &
             ZnRpt(ZoneElectric(Loop)%ZonePtr)%ElecLatGain, &
                                   'Zone','Sum',Zone(ZoneElectric(Loop)%ZonePtr)%Name)
          CALL SetupOutputVariable('Zone Electric Equipment Latent Gain Rate [W]',  &
             ZnRpt(ZoneElectric(Loop)%ZonePtr)%ElecLatGainRate, &
                                   'Zone','Average',Zone(ZoneElectric(Loop)%ZonePtr)%Name)
          CALL SetupOutputVariable('Zone Electric Equipment Lost Heat Energy [J]',  &
             ZnRpt(ZoneElectric(Loop)%ZonePtr)%ElecLost, &
                                   'Zone','Sum',Zone(ZoneElectric(Loop)%ZonePtr)%Name)
          CALL SetupOutputVariable('Zone Electric Equipment Lost Heat Rate [W]',  &
             ZnRpt(ZoneElectric(Loop)%ZonePtr)%ElecLostRate, &
                                   'Zone','Average',Zone(ZoneElectric(Loop)%ZonePtr)%Name)
          CALL SetupOutputVariable('Zone Electric Equipment Total Heating Energy [J]',  &
             ZnRpt(ZoneElectric(Loop)%ZonePtr)%ElecTotGain, &
                                   'Zone','Sum',Zone(ZoneElectric(Loop)%ZonePtr)%Name)
          CALL SetupOutputVariable('Zone Electric Equipment Total Heating Rate [W]',  &
             ZnRpt(ZoneElectric(Loop)%ZonePtr)%ElecTotGainRate, &
                                   'Zone','Average',Zone(ZoneElectric(Loop)%ZonePtr)%Name)
        ENDIF

        IF (AnyEnergyManagementSystemInModel) Then
          CALL SetupEMSActuator('ElectricEquipment', ZoneElectric(Loop)%Name, 'Electric Power Level', '[W]', &
                    ZoneElectric(Loop)%EMSZoneEquipOverrideOn  , ZoneElectric(Loop)%EMSEquipPower )
          CALL SetupEMSInternalVariable( 'Plug and Process Power Design Level', ZoneElectric(Loop)%Name, '[W]' ,&
                                           ZoneElectric(Loop)%DesignLevel )
        ENDIF ! EMS

        IF (.not. ErrorsFound)   &
          CALL SetupZoneInternalGain(ZoneElectric(Loop)%ZonePtr, &
                   'ElectricEquipment', &
                   ZoneElectric(Loop)%Name, &
                   IntGainTypeOf_ElectricEquipment, &
                   ConvectionGainRate          = ZoneElectric(Loop)%ConGainRate,&
                   ThermalRadiationGainRate    = ZoneElectric(Loop)%RadGainRate, &
                   LatentGainRate              = ZoneElectric(Loop)%LatGainRate)

      END DO ! Item1
    END DO ! Item - Number of ZoneElectric objects
  END IF ! Check on number of ZoneElectric


  RepVarSet=.true.
  CurrentModuleObject='GasEquipment'
  NumZoneGasStatements=GetNumObjectsFound(CurrentModuleObject)
  ALLOCATE(ZoneGasObjects(NumZoneGasStatements))

  TotGasEquip=0
  ErrFlag=.false.
  DO Item=1,NumZoneGasStatements
    CALL GetObjectItem(CurrentModuleObject,Item,AlphaName,NumAlpha,IHGNumbers,NumNumber,IOStat,  &
                   AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                   AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
    IsNotOK = .FALSE.
    IsBlank = .FALSE.
    CALL VerifyName(AlphaName(1),ZoneGasObjects%Name,Item-1,IsNotOK,IsBlank,TRIM(CurrentModuleObject)//' Name')
    IF (IsNotOK) THEN
      ErrorsFound = .TRUE.
      ErrFlag=.true.
      IF (IsBlank) AlphaName(1) = 'xxxxx'
    END IF
    ZoneGasObjects(Item)%Name = AlphaName(1)

    Item1=FindItemInList(AlphaName(2),Zone%Name,NumOfZones)
    ZLItem=0
    IF (Item1 == 0 .and. NumOfZoneLists > 0) &
        ZLItem=FindItemInList(AlphaName(2),ZoneList%Name,NumOfZoneLists)
    IF (Item1 > 0) THEN
      ZoneGasObjects(Item)%StartPtr=TotGasEquip+1
      TotGasEquip=TotGasEquip+1
      ZoneGasObjects(Item)%NumOfZones=1
      ZoneGasObjects(Item)%ZoneListActive=.false.
      ZoneGasObjects(Item)%ZoneOrZoneListPtr=Item1
    ELSEIF (ZLItem > 0) THEN
      ZoneGasObjects(Item)%StartPtr=TotGasEquip+1
      TotGasEquip=TotGasEquip+ZoneList(ZLItem)%NumOfZones
      ZoneGasObjects(Item)%NumOfZones=ZoneList(ZLItem)%NumOfZones
      ZoneGasObjects(Item)%ZoneListActive=.true.
      ZoneGasObjects(Item)%ZoneOrZoneListPtr=ZLItem
    ELSE
      CALL ShowSevereError(trim(CurrentModuleObject)//'="'//trim(AlphaName(1))//'" invalid '//  &
           trim(cAlphaFieldNames(2))//'="'//trim(AlphaName(2))//'" not found.')
      ErrorsFound=.true.
      ErrFlag=.true.
    ENDIF
  ENDDO

  IF (ErrFlag) THEN
    CALL ShowSevereError(RoutineName//'Errors with invalid names in '//trim(CurrentModuleObject)//  &
       ' objects.')
    CALL ShowContinueError('...These will not be read in.  Other errors may occur.')
    TotGasEquip=0
  ENDIF

  ALLOCATE(ZoneGas(TotGasEquip))

  IF (TotGasEquip > 0) THEN
    Loop=0
    DO Item = 1, NumZoneGasStatements
      AlphaName  = Blank
      IHGNumbers = 0.0d0

      CALL GetObjectItem(CurrentModuleObject,Item,AlphaName,NumAlpha,IHGNumbers,NumNumber,IOStat,  &
                     AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                     AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

      DO Item1=1,ZoneGasObjects(Item)%NumOfZones
        Loop=Loop+1
        IF (.not. ZoneGasObjects(Item)%ZoneListActive) THEN
          ZoneGas(Loop)%Name = AlphaName(1)
          ZoneGas(Loop)%ZonePtr = ZoneGasObjects(Item)%ZoneOrZoneListPtr
        ELSE
          CALL CheckCreatedZoneItemName(RoutineName,CurrentModuleObject,  &
                                        Zone(ZoneList(ZoneGasObjects(Item)%ZoneOrZoneListPtr)%Zone(Item1))%Name,  &
                                        ZoneList(ZoneGasObjects(Item)%ZoneOrZoneListPtr)%MaxZoneNameLength,  &
                                        ZoneGasObjects(Item)%Name,     &
                                        ZoneGas%Name,           &
                                        Loop-1,                       &
                                        ZoneGas(Loop)%Name,            &
                                        ErrFlag)
          ZoneGas(Loop)%ZonePtr = ZoneList(ZoneGasObjects(Item)%ZoneOrZoneListPtr)%Zone(Item1)
          IF (ErrFlag) ErrorsFound=.true.
        ENDIF

        ZoneGas(Loop)%SchedPtr=GetScheduleIndex(AlphaName(3))
        SchMin=0.0d0
        SchMax=0.0d0
        IF (ZoneGas(Loop)%SchedPtr == 0) THEN
          IF (Item1 == 1) THEN
            IF (lAlphaFieldBlanks(3)) THEN
              CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(AlphaName(1))//  &
                                   '", '//TRIM(cAlphaFieldNames(3))//' is required.')
            ELSE
              CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(AlphaName(1))//  &
                                   '", invalid '//TRIM(cAlphaFieldNames(3))//' entered='//TRIM(AlphaName(3)))
            ENDIF
            ErrorsFound=.true.
          ENDIF
        ELSE  ! check min/max on schedule
          SchMin=GetScheduleMinValue(ZoneGas(Loop)%SchedPtr)
          SchMax=GetScheduleMaxValue(ZoneGas(Loop)%SchedPtr)
          IF (SchMin < 0.0d0 .or. SchMax < 0.0d0) THEN
            IF (Item1 == 1) THEN
              IF (SchMin < 0.0d0) THEN
                CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(AlphaName(1))//  &
                                   '", '//TRIM(cAlphaFieldNames(3))//', minimum is < 0.0')
                CALL ShowContinueError('Schedule="'//TRIM(AlphaName(3))//  &
                                   '". Minimum is ['//TRIM(RoundSigDigits(SchMin,1))//']. Values must be >= 0.0.')
                ErrorsFound=.true.
              ENDIF
            ENDIF
            IF (Item1 == 1) THEN
              IF (SchMax < 0.0d0) THEN
                CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(AlphaName(1))//  &
                                   '", '//TRIM(cAlphaFieldNames(3))//', maximum is < 0.0')
                CALL ShowContinueError('Schedule="'//TRIM(AlphaName(3))//  &
                                   '". Maximum is ['//TRIM(RoundSigDigits(SchMax,1))//']. Values must be >= 0.0.')
                ErrorsFound=.true.
              ENDIF
            ENDIF
          ENDIF
        ENDIF

        ! equipment design level calculation method.
        SELECT CASE (AlphaName(4))
          CASE('EQUIPMENTLEVEL')
            ZoneGas(Loop)%DesignLevel=IHGNumbers(1)
            IF (lNumericFieldBlanks(1)) THEN
              CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(ZoneGas(Loop)%Name)//  &
                '", specifies '//TRIM(cNumericFieldNames(1))//', but that field is blank.  0 Gas Equipment will result.')
            ENDIF

          CASE('WATTS/AREA','POWER/AREA')
            IF (ZoneGas(Loop)%ZonePtr /= 0) THEN
              IF (IHGNumbers(2) >= 0.0d0) THEN
                ZoneGas(Loop)%DesignLevel=IHGNumbers(2)*Zone(ZoneGas(Loop)%ZonePtr)%FloorArea
                IF (Zone(ZoneGas(Loop)%ZonePtr)%FloorArea <= 0.0d0) THEN
                  CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(ZoneGas(Loop)%Name)//  &
                    '", specifies '//TRIM(cNumericFieldNames(2))//', but Zone Floor Area = 0.  0 Gas Equipment will result.')
                ENDIF
              ELSE
                CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(ZoneGas(Loop)%Name)//  &
                                 '", invalid '//TRIM(cNumericFieldNames(2))//', value  [<0.0]='//  &
                                 TRIM(RoundSigDigits(IHGNumbers(2),3)))
                ErrorsFound=.true.
              ENDIF
            ENDIF
            IF (lNumericFieldBlanks(2)) THEN
              CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(ZoneGas(Loop)%Name)//  &
                '", specifies '//TRIM(cNumericFieldNames(2))//', but that field is blank.  0 Gas Equipment will result.')
            ENDIF

          CASE('WATTS/PERSON','POWER/PERSON')
            IF (ZoneGas(Loop)%ZonePtr /= 0) THEN
              IF (IHGNumbers(3) >= 0.0d0) THEN
                ZoneGas(Loop)%DesignLevel=IHGNumbers(3)*Zone(ZoneGas(Loop)%ZonePtr)%TotOccupants
                IF (Zone(ZoneGas(Loop)%ZonePtr)%TotOccupants <= 0.0d0) THEN
                  CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(ZoneGas(Loop)%Name)//  &
                    '", specifies '//TRIM(cNumericFieldNames(2))//', but Total Occupants = 0.  0 Gas Equipment will result.')
                ENDIF
              ELSE
                CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(ZoneGas(Loop)%Name)//  &
                                 '", invalid '//TRIM(cNumericFieldNames(3))//', value  [<0.0]='//  &
                                 TRIM(RoundSigDigits(IHGNumbers(3),3)))
                ErrorsFound=.true.
              ENDIF
            ENDIF
            IF (lNumericFieldBlanks(3)) THEN
              CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(ZoneGas(Loop)%Name)//  &
                '", specifies '//TRIM(cNumericFieldNames(3))//', but that field is blank.  0 Gas Equipment will result.')
            ENDIF

          CASE DEFAULT
            IF (Item1 == 1) THEN
              CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(AlphaName(1))//  &
                                 '", invalid '//TRIM(cAlphaFieldNames(4))//', value  ='//  &
                                 TRIM(AlphaName(4)))
              CALL ShowContinueError('...Valid values are "EquipmentLevel", "Watts/Area", "Watts/Person".')
              ErrorsFound=.true.
            ENDIF
        END SELECT

        ! Calculate nominal min/max equipment level
        ZoneGas(Loop)%NomMinDesignLevel=ZoneGas(Loop)%DesignLevel*SchMin
        ZoneGas(Loop)%NomMaxDesignLevel=ZoneGas(Loop)%DesignLevel*SchMax

        ZoneGas(Loop)%FractionLatent=IHGNumbers(4)
        ZoneGas(Loop)%FractionRadiant=IHGNumbers(5)
        ZoneGas(Loop)%FractionLost=IHGNumbers(6)

        IF ((NumNumber .EQ. 7) .OR. (.not. lNumericFieldBlanks(7))) THEN
          ZoneGas(Loop)%CO2RateFactor=IHGNumbers(7)
        END IF
        If (ZoneGas(Loop)%CO2RateFactor .LT. 0.d0) Then
          CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(AlphaName(1))//  &
                           '", '//TRIM(cNumericFieldNames(7))//' < 0.0, value ='//  &
                           TRIM(RoundSigDigits(IHGNumbers(7),2)))
          ErrorsFound=.true.
        End If
        If (ZoneGas(Loop)%CO2RateFactor .GT. 4.0d-7) Then
          CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(AlphaName(1))//  &
                             '", '//TRIM(cNumericFieldNames(7))//' > 4.0E-7, value ='//  &
                             TRIM(RoundSigDigits(IHGNumbers(7),2)))
          ErrorsFound=.true.
        End If
                   ! FractionConvected is a calculated field
        ZoneGas(Loop)%FractionConvected=1.0d0 - (ZoneGas(Loop)%FractionLatent  +  &
                                               ZoneGas(Loop)%FractionRadiant  +  &
                                               ZoneGas(Loop)%FractionLost)
        IF (ABS(ZoneGas(Loop)%FractionConvected) <= .001d0) ZoneGas(Loop)%FractionConvected=0.0d0
        IF (ZoneGas(Loop)%FractionConvected < 0.0d0) THEN
          IF (Item1 == 1) THEN
            CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(AlphaName(1))//  &
                                   '", Sum of Fractions > 1.0')
            ErrorsFound=.true.
          ENDIF
        ENDIF

        IF (NumAlpha > 4) THEN
          ZoneGas(Loop)%EndUseSubcategory = AlphaName(5)
        ELSE
          ZoneGas(Loop)%EndUseSubcategory = 'General'
        END IF

        IF (ZoneGas(Loop)%ZonePtr <=0) CYCLE   ! Error, will be caught and terminated later

        ! Object report variables
        CALL SetupOutputVariable('Gas Equipment Gas Rate [W]',ZoneGas(Loop)%Power, &
                                 'Zone','Average',ZoneGas(Loop)%Name)
        CALL SetupOutputVariable('Gas Equipment Gas Energy [J]',ZoneGas(Loop)%Consumption, &
                                 'Zone','Sum',ZoneGas(Loop)%Name,ResourceTypeKey='Gas', &
                                 GroupKey='Building',ZoneKey=Zone(ZoneGas(Loop)%ZonePtr)%Name, &
                                 EndUseKey='InteriorEquipment',EndUseSubKey=ZoneGas(Loop)%EndUseSubcategory, &
                                 ZoneMult=Zone(ZoneGas(Loop)%ZonePtr)%Multiplier, &
                                 ZoneListMult=Zone(ZoneGas(Loop)%ZonePtr)%ListMultiplier)

        CALL SetupOutputVariable('Gas Equipment Radiant Heating Energy [J]',ZoneGas(Loop)%RadGainEnergy, &
                                 'Zone','Sum',ZoneGas(Loop)%Name)
        CALL SetupOutputVariable('Gas Equipment Convective Heating Energy [J]',ZoneGas(Loop)%ConGainEnergy, &
                                 'Zone','Sum',ZoneGas(Loop)%Name)
        CALL SetupOutputVariable('Gas Equipment Latent Gain Energy [J]',ZoneGas(Loop)%LatGainEnergy, &
                                 'Zone','Sum',ZoneGas(Loop)%Name)
        CALL SetupOutputVariable('Gas Equipment Lost Heat Energy [J]',ZoneGas(Loop)%LostEnergy, &
                                 'Zone','Sum',ZoneGas(Loop)%Name)
        CALL SetupOutputVariable('Gas Equipment Total Heating Energy [J]',ZoneGas(Loop)%TotGainEnergy, &
                                 'Zone','Sum',ZoneGas(Loop)%Name)
        CALL SetupOutputVariable('Gas Equipment Radiant Heating Rate [W]',ZoneGas(Loop)%RadGainRate, &
                                 'Zone','Average',ZoneGas(Loop)%Name)
        CALL SetupOutputVariable('Gas Equipment Convective Heating Rate [W]',ZoneGas(Loop)%ConGainRate, &
                                 'Zone','Average',ZoneGas(Loop)%Name)
        CALL SetupOutputVariable('Gas Equipment Latent Gain Rate [W]',ZoneGas(Loop)%LatGainRate, &
                                 'Zone','Average',ZoneGas(Loop)%Name)
        CALL SetupOutputVariable('Gas Equipment Lost Heat Rate [W]',ZoneGas(Loop)%LostRate, &
                                 'Zone','Average',ZoneGas(Loop)%Name)
        CALL SetupOutputVariable('Gas Equipment Total Heating Rate [W]',ZoneGas(Loop)%TotGainRate, &
                                 'Zone','Average',ZoneGas(Loop)%Name)

        ! Zone total report variables
        IF (RepVarSet(ZoneGas(Loop)%ZonePtr)) THEN
          RepVarSet(ZoneGas(Loop)%ZonePtr)=.false.

          CALL SetupOutputVariable('Zone Gas Equipment Gas Rate [W]',ZnRpt(ZoneGas(Loop)%ZonePtr)%GasPower, &
                                   'Zone','Average',Zone(ZoneGas(Loop)%ZonePtr)%Name)
          CALL SetupOutputVariable('Zone Gas Equipment Gas Energy [J]',ZnRpt(ZoneGas(Loop)%ZonePtr)%GasConsump, &
                                   'Zone','Sum',Zone(ZoneGas(Loop)%ZonePtr)%Name)

          CALL SetupOutputVariable('Zone Gas Equipment Radiant Heating Energy [J]',ZnRpt(ZoneGas(Loop)%ZonePtr)%GasRadGain, &
                                   'Zone','Sum',Zone(ZoneGas(Loop)%ZonePtr)%Name)
          CALL SetupOutputVariable('Zone Gas Equipment Radiant Heating Rate [W]',ZnRpt(ZoneGas(Loop)%ZonePtr)%GasRadGainRate, &
                                   'Zone','Average',Zone(ZoneGas(Loop)%ZonePtr)%Name)
          CALL SetupOutputVariable('Zone Gas Equipment Convective Heating Energy [J]',ZnRpt(ZoneGas(Loop)%ZonePtr)%GasConGain, &
                                   'Zone','Sum',Zone(ZoneGas(Loop)%ZonePtr)%Name)
          CALL SetupOutputVariable('Zone Gas Equipment Convective Heating Rate [W]',ZnRpt(ZoneGas(Loop)%ZonePtr)%GasConGainRate, &
                                   'Zone','Average',Zone(ZoneGas(Loop)%ZonePtr)%Name)
          CALL SetupOutputVariable('Zone Gas Equipment Latent Gain Energy [J]',ZnRpt(ZoneGas(Loop)%ZonePtr)%GasLatGain, &
                                   'Zone','Sum',Zone(ZoneGas(Loop)%ZonePtr)%Name)
          CALL SetupOutputVariable('Zone Gas Equipment Latent Gain Rate [W]',ZnRpt(ZoneGas(Loop)%ZonePtr)%GasLatGainRate, &
                                   'Zone','Average',Zone(ZoneGas(Loop)%ZonePtr)%Name)
          CALL SetupOutputVariable('Zone Gas Equipment Lost Heat Energy [J]',ZnRpt(ZoneGas(Loop)%ZonePtr)%GasLost, &
                                   'Zone','Sum',Zone(ZoneGas(Loop)%ZonePtr)%Name)
          CALL SetupOutputVariable('Zone Gas Equipment Lost Heat Rate [W]',ZnRpt(ZoneGas(Loop)%ZonePtr)%GasLostRate, &
                                   'Zone','Average',Zone(ZoneGas(Loop)%ZonePtr)%Name)
          CALL SetupOutputVariable('Zone Gas Equipment Total Heating Energy [J]',ZnRpt(ZoneGas(Loop)%ZonePtr)%GasTotGain, &
                                   'Zone','Sum',Zone(ZoneGas(Loop)%ZonePtr)%Name)
          CALL SetupOutputVariable('Zone Gas Equipment Total Heating Rate [W]',ZnRpt(ZoneGas(Loop)%ZonePtr)%GasTotGainRate, &
                                   'Zone','Average',Zone(ZoneGas(Loop)%ZonePtr)%Name)
        ENDIF

        IF (AnyEnergyManagementSystemInModel) Then
          CALL SetupEMSActuator('GasEquipment', ZoneGas(Loop)%Name, 'Gas Power Level', '[W]', &
                    ZoneGas(Loop)%EMSZoneEquipOverrideOn  , ZoneGas(Loop)%EMSEquipPower )
          CALL SetupEMSInternalVariable( 'Gas Process Power Design Level', ZoneGas(Loop)%Name, '[W]' ,&
                                           ZoneGas(Loop)%DesignLevel )
        ENDIF ! EMS

        IF (.not. ErrorsFound)   &
          CALL SetupZoneInternalGain(ZoneGas(Loop)%ZonePtr, &
                   'GasEquipment', &
                   ZoneGas(Loop)%Name, &
                   IntGainTypeOf_GasEquipment, &
                   ConvectionGainRate          = ZoneGas(Loop)%ConGainRate, &
                   ThermalRadiationGainRate    = ZoneGas(Loop)%RadGainRate, &
                   CarbonDioxideGainRate       = ZoneGas(Loop)%CO2GainRate, &
                   LatentGainRate              = ZoneGas(Loop)%LatGainRate)

      END DO ! Item1
    END DO ! Item - number of gas statements
  ENDIF  ! check for number of gas statements


  RepVarSet=.true.
  CurrentModuleObject='HotWaterEquipment'
  NumHotWaterEqStatements=GetNumObjectsFound(CurrentModuleObject)
  ALLOCATE(HotWaterEqObjects(NumHotWaterEqStatements))

  TotHWEquip=0
  ErrFlag=.false.
  DO Item=1,NumHotWaterEqStatements
    CALL GetObjectItem(CurrentModuleObject,Item,AlphaName,NumAlpha,IHGNumbers,NumNumber,IOStat,  &
                   AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                   AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
    IsNotOK = .FALSE.
    IsBlank = .FALSE.
    CALL VerifyName(AlphaName(1),HotWaterEqObjects%Name,Item-1,IsNotOK,IsBlank,TRIM(CurrentModuleObject)//' Name')
    IF (IsNotOK) THEN
      ErrorsFound = .TRUE.
      ErrFlag=.true.
      IF (IsBlank) AlphaName(1) = 'xxxxx'
    END IF
    HotWaterEqObjects(Item)%Name = AlphaName(1)

    Item1=FindItemInList(AlphaName(2),Zone%Name,NumOfZones)
    ZLItem=0
    IF (Item1 == 0 .and. NumOfZoneLists > 0) &
        ZLItem=FindItemInList(AlphaName(2),ZoneList%Name,NumOfZoneLists)
    IF (Item1 > 0) THEN
      HotWaterEqObjects(Item)%StartPtr=TotHWEquip+1
      TotHWEquip=TotHWEquip+1
      HotWaterEqObjects(Item)%NumOfZones=1
      HotWaterEqObjects(Item)%ZoneListActive=.false.
      HotWaterEqObjects(Item)%ZoneOrZoneListPtr=Item1
    ELSEIF (ZLItem > 0) THEN
      HotWaterEqObjects(Item)%StartPtr=TotHWEquip+1
      TotHWEquip=TotHWEquip+ZoneList(ZLItem)%NumOfZones
      HotWaterEqObjects(Item)%NumOfZones=ZoneList(ZLItem)%NumOfZones
      HotWaterEqObjects(Item)%ZoneListActive=.true.
      HotWaterEqObjects(Item)%ZoneOrZoneListPtr=ZLItem
    ELSE
      CALL ShowSevereError(trim(CurrentModuleObject)//'="'//trim(AlphaName(1))//'" invalid '//  &
           trim(cAlphaFieldNames(2))//'="'//trim(AlphaName(2))//'" not found.')
      ErrorsFound=.true.
      ErrFlag=.true.
    ENDIF
  ENDDO

  IF (ErrFlag) THEN
    CALL ShowSevereError(RoutineName//'Errors with invalid names in '//trim(CurrentModuleObject)//  &
       ' objects.')
    CALL ShowContinueError('...These will not be read in.  Other errors may occur.')
    TotHWEquip=0
  ENDIF

  ALLOCATE(ZoneHWEq(TotHWEquip))

  IF (TotHWEquip > 0) THEN
    Loop=0
    DO Item = 1, NumHotWaterEqStatements
      AlphaName  = Blank
      IHGNumbers = 0.0d0

      CALL GetObjectItem(CurrentModuleObject,Item,AlphaName,NumAlpha,IHGNumbers,NumNumber,IOStat,  &
                     AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                     AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

      DO Item1=1,HotWaterEqObjects(Item)%NumOfZones
        Loop=Loop+1
        IF (.not. HotWaterEqObjects(Item)%ZoneListActive) THEN
          ZoneHWEq(Loop)%Name = AlphaName(1)
          ZoneHWEq(Loop)%ZonePtr = HotWaterEqObjects(Item)%ZoneOrZoneListPtr
        ELSE
          CALL CheckCreatedZoneItemName(RoutineName,CurrentModuleObject,  &
                                        Zone(ZoneList(HotWaterEqObjects(Item)%ZoneOrZoneListPtr)%Zone(Item1))%Name,  &
                                        ZoneList(HotWaterEqObjects(Item)%ZoneOrZoneListPtr)%MaxZoneNameLength,  &
                                        HotWaterEqObjects(Item)%Name,     &
                                        ZoneHWEq%Name,           &
                                        Loop-1,                       &
                                        ZoneHWEq(Loop)%Name,            &
                                        ErrFlag)
          ZoneHWEq(Loop)%ZonePtr = ZoneList(HotWaterEqObjects(Item)%ZoneOrZoneListPtr)%Zone(Item1)
          IF (ErrFlag) ErrorsFound=.true.
        ENDIF

        ZoneHWEq(Loop)%SchedPtr=GetScheduleIndex(AlphaName(3))
        SchMin=0.0d0
        SchMax=0.0d0
        IF (ZoneHWEq(Loop)%SchedPtr == 0) THEN
          IF (lAlphaFieldBlanks(3)) THEN
            CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(AlphaName(1))//  &
                                 '", '//TRIM(cAlphaFieldNames(3))//' is required.')
          ELSE
            CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(AlphaName(1))//  &
                                 '", invalid '//TRIM(cAlphaFieldNames(3))//' entered='//TRIM(AlphaName(3)))
          ENDIF
          ErrorsFound=.true.
        ELSE  ! check min/max on schedule
          SchMin=GetScheduleMinValue(ZoneHWEq(Loop)%SchedPtr)
          SchMax=GetScheduleMaxValue(ZoneHWEq(Loop)%SchedPtr)
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

        ! Hot Water equipment design level calculation method.
        SELECT CASE (AlphaName(4))
          CASE('EQUIPMENTLEVEL')
            ZoneHWEq(Loop)%DesignLevel=IHGNumbers(1)
            IF (lNumericFieldBlanks(1)) THEN
              CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(AlphaName(1))//  &
                '", specifies '//TRIM(cNumericFieldNames(1))//', but that field is blank.  0 Hot Water Equipment will result.')
            ENDIF

          CASE('WATTS/AREA','POWER/AREA')
            IF (ZoneHWEq(Loop)%ZonePtr /= 0) THEN
              IF (IHGNumbers(2) >= 0.0d0) THEN
                ZoneHWEq(Loop)%DesignLevel=IHGNumbers(2)*Zone(ZoneHWEq(Loop)%ZonePtr)%FloorArea
                IF (Zone(ZoneHWEq(Loop)%ZonePtr)%FloorArea <= 0.0d0) THEN
                  CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(AlphaName(1))//  &
                    '", specifies '//TRIM(cNumericFieldNames(2))//', but Zone Floor Area = 0.  0 Hot Water Equipment will result.')
                ENDIF
              ELSE
                CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(AlphaName(1))//  &
                                 '", invalid '//TRIM(cNumericFieldNames(2))//', value  [<0.0]='//  &
                                 TRIM(RoundSigDigits(IHGNumbers(2),3)))
                ErrorsFound=.true.
              ENDIF
            ENDIF
            IF (lNumericFieldBlanks(2)) THEN
              CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(AlphaName(1))//  &
                '", specifies '//TRIM(cNumericFieldNames(2))//', but that field is blank.  0 Hot Water Equipment will result.')
            ENDIF

          CASE('WATTS/PERSON','POWER/PERSON')
            IF (ZoneHWEq(Loop)%ZonePtr /= 0) THEN
              IF (IHGNumbers(3) >= 0.0d0) THEN
                ZoneHWEq(Loop)%DesignLevel=IHGNumbers(3)*Zone(ZoneHWEq(Loop)%ZonePtr)%TotOccupants
                IF (Zone(ZoneHWEq(Loop)%ZonePtr)%TotOccupants <= 0.0d0) THEN
                  CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(AlphaName(1))//  &
                    '", specifies '//TRIM(cNumericFieldNames(2))//', but Total Occupants = 0.  0 Hot Water Equipment will result.')
                ENDIF
              ELSE
                CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(AlphaName(1))//  &
                                 '", invalid '//TRIM(cNumericFieldNames(3))//', value  [<0.0]='//  &
                                 TRIM(RoundSigDigits(IHGNumbers(3),3)))
                ErrorsFound=.true.
              ENDIF
            ENDIF
            IF (lNumericFieldBlanks(3)) THEN
              CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(AlphaName(1))//  &
                '", specifies '//TRIM(cNumericFieldNames(3))//', but that field is blank.  0 Hot Water Equipment will result.')
            ENDIF

          CASE DEFAULT
            IF (Item1 == 1) THEN
              CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(AlphaName(1))//  &
                                 '", invalid '//TRIM(cAlphaFieldNames(4))//', value  ='//  &
                                 TRIM(AlphaName(4)))
              CALL ShowContinueError('...Valid values are "EquipmentLevel", "Watts/Area", "Watts/Person".')
              ErrorsFound=.true.
            ENDIF
        END SELECT

        ! Calculate nominal min/max equipment level
        ZoneHWEq(Loop)%NomMinDesignLevel=ZoneHWEq(Loop)%DesignLevel*SchMin
        ZoneHWEq(Loop)%NomMaxDesignLevel=ZoneHWEq(Loop)%DesignLevel*SchMax

        ZoneHWEq(Loop)%FractionLatent=IHGNumbers(4)
        ZoneHWEq(Loop)%FractionRadiant=IHGNumbers(5)
        ZoneHWEq(Loop)%FractionLost=IHGNumbers(6)
                    ! FractionConvected is a calculated field
        ZoneHWEq(Loop)%FractionConvected=1.0d0 - (ZoneHWEq(Loop)%FractionLatent +   &
                                                    ZoneHWEq(Loop)%FractionRadiant +   &
                                                    ZoneHWEq(Loop)%FractionLost)
        IF (ABS(ZoneHWEq(Loop)%FractionConvected) <= .001d0) ZoneHWEq(Loop)%FractionConvected=0.0d0
        IF (ZoneHWEq(Loop)%FractionConvected < 0.0d0) THEN
          CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(AlphaName(1))//  &
                                 '", Sum of Fractions > 1.0')
          ErrorsFound=.true.
        ENDIF

        IF (NumAlpha > 4) THEN
          ZoneHWEq(Loop)%EndUseSubcategory = AlphaName(5)
        ELSE
          ZoneHWEq(Loop)%EndUseSubcategory = 'General'
        END IF

        IF (ZoneHWEq(Loop)%ZonePtr <=0) CYCLE   ! Error, will be caught and terminated later

        ! Object report variables
        CALL SetupOutputVariable('Hot Water Equipment District Heating Rate [W]', ZoneHWEq(Loop)%Power, &
                                 'Zone','Average',ZoneHWEq(Loop)%Name)
        CALL SetupOutputVariable('Hot Water Equipment District Heating Energy [J]',ZoneHWEq(Loop)%Consumption, &
                                 'Zone','Sum',ZoneHWEq(Loop)%Name,ResourceTypeKey='DistrictHeating', &
                                 GroupKey='Building',ZoneKey=Zone(ZoneHWEq(Loop)%ZonePtr)%Name, &
                                 EndUseKey='InteriorEquipment',EndUseSubKey=ZoneHWEq(Loop)%EndUseSubcategory, &
                                 ZoneMult=Zone(ZoneHWEq(Loop)%ZonePtr)%Multiplier, &
                                 ZoneListMult=Zone(ZoneHWEq(Loop)%ZonePtr)%ListMultiplier)

        CALL SetupOutputVariable('Hot Water Equipment Radiant Heating Energy [J]',ZoneHWEq(Loop)%RadGainEnergy, &
                                 'Zone','Sum',ZoneHWEq(Loop)%Name)
        CALL SetupOutputVariable('Hot Water Equipment Radiant Heating Rate [W]',ZoneHWEq(Loop)%RadGainRate, &
                                 'Zone','Average',ZoneHWEq(Loop)%Name)
        CALL SetupOutputVariable('Hot Water Equipment Convective Heating Energy [J]',ZoneHWEq(Loop)%ConGainEnergy, &
                                 'Zone','Sum',ZoneHWEq(Loop)%Name)
        CALL SetupOutputVariable('Hot Water Equipment Convective Heating Rate [W]',ZoneHWEq(Loop)%ConGainRate, &
                                 'Zone','Average',ZoneHWEq(Loop)%Name)
        CALL SetupOutputVariable('Hot Water Equipment Latent Gain Energy [J]',ZoneHWEq(Loop)%LatGainEnergy, &
                                 'Zone','Sum',ZoneHWEq(Loop)%Name)
        CALL SetupOutputVariable('Hot Water Equipment Latent Gain Rate [W]',ZoneHWEq(Loop)%LatGainRate, &
                                 'Zone','Average',ZoneHWEq(Loop)%Name)
        CALL SetupOutputVariable('Hot Water Equipment Lost Heat Energy [J]',ZoneHWEq(Loop)%LostEnergy, &
                                 'Zone','Sum',ZoneHWEq(Loop)%Name)
        CALL SetupOutputVariable('Hot Water Equipment Lost Heat Rate [W]',ZoneHWEq(Loop)%LostRate, &
                                 'Zone','Average',ZoneHWEq(Loop)%Name)
        CALL SetupOutputVariable('Hot Water Equipment Total Heating Energy [J]',ZoneHWEq(Loop)%TotGainEnergy, &
                                 'Zone','Sum',ZoneHWEq(Loop)%Name)
        CALL SetupOutputVariable('Hot Water Equipment Total Heating Rate [W]',ZoneHWEq(Loop)%TotGainRate, &
                                 'Zone','Average',ZoneHWEq(Loop)%Name)

        ! Zone total report variables
        IF (RepVarSet(ZoneHWEq(Loop)%ZonePtr)) THEN
          RepVarSet(ZoneHWEq(Loop)%ZonePtr)=.false.
          CALL SetupOutputVariable('Zone Hot Water Equipment District Heating Rate [W]', &
                                   ZnRpt(ZoneHWEq(Loop)%ZonePtr)%HWPower, &
                                   'Zone','Average',Zone(ZoneHWEq(Loop)%ZonePtr)%Name)
          CALL SetupOutputVariable('Zone Hot Water Equipment District Heating Energy [J]', &
                                   ZnRpt(ZoneHWEq(Loop)%ZonePtr)%HWConsump, &
                                   'Zone','Sum',Zone(ZoneHWEq(Loop)%ZonePtr)%Name)

          CALL SetupOutputVariable('Zone Hot Water Equipment Radiant Heating Energy [J]',  &
             ZnRpt(ZoneHWEq(Loop)%ZonePtr)%HWRadGain, &
                                   'Zone','Sum',Zone(ZoneHWEq(Loop)%ZonePtr)%Name)
          CALL SetupOutputVariable('Zone Hot Water Equipment Radiant Heating Rate [W]',  &
             ZnRpt(ZoneHWEq(Loop)%ZonePtr)%HWRadGainRate, &
                                   'Zone','Average',Zone(ZoneHWEq(Loop)%ZonePtr)%Name)
          CALL SetupOutputVariable('Zone Hot Water Equipment Convective Heating Energy [J]',  &
             ZnRpt(ZoneHWEq(Loop)%ZonePtr)%HWConGain, &
                                   'Zone','Sum',Zone(ZoneHWEq(Loop)%ZonePtr)%Name)
          CALL SetupOutputVariable('Zone Hot Water Equipment Convective Heating Rate [W]',  &
             ZnRpt(ZoneHWEq(Loop)%ZonePtr)%HWConGainRate, &
                                   'Zone','Average',Zone(ZoneHWEq(Loop)%ZonePtr)%Name)
          CALL SetupOutputVariable('Zone Hot Water Equipment Latent Gain Energy [J]',  &
             ZnRpt(ZoneHWEq(Loop)%ZonePtr)%HWLatGain, &
                                   'Zone','Sum',Zone(ZoneHWEq(Loop)%ZonePtr)%Name)
          CALL SetupOutputVariable('Zone Hot Water Equipment Latent Gain Rate [W]',  &
             ZnRpt(ZoneHWEq(Loop)%ZonePtr)%HWLatGainRate, &
                                   'Zone','Average',Zone(ZoneHWEq(Loop)%ZonePtr)%Name)
          CALL SetupOutputVariable('Zone Hot Water Equipment Lost Heat Energy [J]',  &
             ZnRpt(ZoneHWEq(Loop)%ZonePtr)%HWLost, &
                                   'Zone','Sum',Zone(ZoneHWEq(Loop)%ZonePtr)%Name)
          CALL SetupOutputVariable('Zone Hot Water Equipment Lost Heat Rate [W]',  &
             ZnRpt(ZoneHWEq(Loop)%ZonePtr)%HWLostRate, &
                                   'Zone','Average',Zone(ZoneHWEq(Loop)%ZonePtr)%Name)
          CALL SetupOutputVariable('Zone Hot Water Equipment Total Heating Energy [J]',  &
             ZnRpt(ZoneHWEq(Loop)%ZonePtr)%HWTotGain, &
                                   'Zone','Sum',Zone(ZoneHWEq(Loop)%ZonePtr)%Name)
          CALL SetupOutputVariable('Zone Hot Water Equipment Total Heating Rate [W]',  &
             ZnRpt(ZoneHWEq(Loop)%ZonePtr)%HWTotGainRate, &
                                   'Zone','Average',Zone(ZoneHWEq(Loop)%ZonePtr)%Name)
        ENDIF

        IF (AnyEnergyManagementSystemInModel) Then
          CALL SetupEMSActuator('HotWaterEquipment', ZoneHWEq(Loop)%Name, 'District Heating Power Level', '[W]', &
                    ZoneHWEq(Loop)%EMSZoneEquipOverrideOn  , ZoneHWEq(Loop)%EMSEquipPower )
          CALL SetupEMSInternalVariable( 'Process District Heat Design Level', ZoneHWEq(Loop)%Name, '[W]' ,&
                                           ZoneHWEq(Loop)%DesignLevel )
        ENDIF ! EMS

        IF (.not. ErrorsFound)   &
          CALL SetupZoneInternalGain(ZoneHWEq(Loop)%ZonePtr, &
                   'HotWaterEquipment', &
                   ZoneHWEq(Loop)%Name, &
                   IntGainTypeOf_HotWaterEquipment, &
                   ConvectionGainRate          = ZoneHWEq(Loop)%ConGainRate,&
                   ThermalRadiationGainRate    = ZoneHWEq(Loop)%RadGainRate, &
                   LatentGainRate              = ZoneHWEq(Loop)%LatGainRate)

      END DO ! Item1
    END DO ! Item - number of hot water statements
  END IF

  RepVarSet=.true.
  CurrentModuleObject='SteamEquipment'
  NumSteamEqStatements=GetNumObjectsFound(CurrentModuleObject)
  ALLOCATE(SteamEqObjects(NumSteamEqStatements))

  TotStmEquip=0
  ErrFlag=.false.
  DO Item=1,NumSteamEqStatements
    CALL GetObjectItem(CurrentModuleObject,Item,AlphaName,NumAlpha,IHGNumbers,NumNumber,IOStat,  &
                   AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                   AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
    IsNotOK = .FALSE.
    IsBlank = .FALSE.
    CALL VerifyName(AlphaName(1),SteamEqObjects%Name,Item-1,IsNotOK,IsBlank,TRIM(CurrentModuleObject)//' Name')
    IF (IsNotOK) THEN
      ErrorsFound = .TRUE.
      ErrFlag=.true.
      IF (IsBlank) AlphaName(1) = 'xxxxx'
    END IF
    SteamEqObjects(Item)%Name = AlphaName(1)

    Item1=FindItemInList(AlphaName(2),Zone%Name,NumOfZones)
    ZLItem=0
    IF (Item1 == 0 .and. NumOfZoneLists > 0) &
        ZLItem=FindItemInList(AlphaName(2),ZoneList%Name,NumOfZoneLists)
    IF (Item1 > 0) THEN
      SteamEqObjects(Item)%StartPtr=TotStmEquip+1
      TotStmEquip=TotStmEquip+1
      SteamEqObjects(Item)%NumOfZones=1
      SteamEqObjects(Item)%ZoneListActive=.false.
      SteamEqObjects(Item)%ZoneOrZoneListPtr=Item1
    ELSEIF (ZLItem > 0) THEN
      SteamEqObjects(Item)%StartPtr=TotStmEquip+1
      TotStmEquip=TotStmEquip+ZoneList(ZLItem)%NumOfZones
      SteamEqObjects(Item)%NumOfZones=ZoneList(ZLItem)%NumOfZones
      SteamEqObjects(Item)%ZoneListActive=.true.
      SteamEqObjects(Item)%ZoneOrZoneListPtr=ZLItem
    ELSE
      CALL ShowSevereError(trim(CurrentModuleObject)//'="'//trim(AlphaName(1))//'" invalid '//  &
           trim(cAlphaFieldNames(2))//'="'//trim(AlphaName(2))//'" not found.')
      ErrorsFound=.true.
      ErrFlag=.true.
    ENDIF
  ENDDO

  IF (ErrFlag) THEN
    CALL ShowSevereError(RoutineName//'Errors with invalid names in '//trim(CurrentModuleObject)//  &
       ' objects.')
    CALL ShowContinueError('...These will not be read in.  Other errors may occur.')
    TotStmEquip=0
  ENDIF

  ALLOCATE(ZoneSteamEq(TotStmEquip))

  IF (TotStmEquip > 0) THEN
    Loop=0
    DO Item = 1, NumSteamEqStatements
      AlphaName  = Blank
      IHGNumbers = 0.0d0

      CALL GetObjectItem(CurrentModuleObject,Item,AlphaName,NumAlpha,IHGNumbers,NumNumber,IOStat,  &
                     AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                     AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

      DO Item1=1,SteamEqObjects(Item)%NumOfZones
        Loop=Loop+1
        IF (.not. SteamEqObjects(Item)%ZoneListActive) THEN
          ZoneSteamEq(Loop)%Name = AlphaName(1)
          ZoneSteamEq(Loop)%ZonePtr = SteamEqObjects(Item)%ZoneOrZoneListPtr
        ELSE
          CALL CheckCreatedZoneItemName(RoutineName,CurrentModuleObject,  &
                                        Zone(ZoneList(SteamEqObjects(Item)%ZoneOrZoneListPtr)%Zone(Item1))%Name,  &
                                        ZoneList(SteamEqObjects(Item)%ZoneOrZoneListPtr)%MaxZoneNameLength,  &
                                        SteamEqObjects(Item)%Name,     &
                                        ZoneSteamEq%Name,           &
                                        Loop-1,                       &
                                        ZoneSteamEq(Loop)%Name,            &
                                        ErrFlag)
          ZoneSteamEq(Loop)%ZonePtr = ZoneList(SteamEqObjects(Item)%ZoneOrZoneListPtr)%Zone(Item1)
          IF (ErrFlag) ErrorsFound=.true.
        ENDIF

        ZoneSteamEq(Loop)%SchedPtr=GetScheduleIndex(AlphaName(3))
        SchMin=0.0d0
        SchMax=0.0d0
        IF (ZoneSteamEq(Loop)%SchedPtr == 0) THEN
          IF (lAlphaFieldBlanks(3)) THEN
            CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(AlphaName(1))//  &
                                 '", '//TRIM(cAlphaFieldNames(3))//' is required.')
          ELSE
            CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(AlphaName(1))//  &
                                 '", invalid '//TRIM(cAlphaFieldNames(3))//' entered='//TRIM(AlphaName(3)))
          ENDIF
          ErrorsFound=.true.
        ELSE  ! check min/max on schedule
          SchMin=GetScheduleMinValue(ZoneSteamEq(Loop)%SchedPtr)
          SchMax=GetScheduleMaxValue(ZoneSteamEq(Loop)%SchedPtr)
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

        ! Hot Water equipment design level calculation method.
        SELECT CASE (AlphaName(4))
          CASE('EQUIPMENTLEVEL')
            ZoneSteamEq(Loop)%DesignLevel=IHGNumbers(1)
            IF (lNumericFieldBlanks(1)) THEN
              CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(AlphaName(1))//  &
                '", specifies '//TRIM(cNumericFieldNames(1))//', but that field is blank.  0 Hot Water Equipment will result.')
            ENDIF

          CASE('WATTS/AREA','POWER/AREA')
            IF (ZoneSteamEq(Loop)%ZonePtr /= 0) THEN
              IF (IHGNumbers(2) >= 0.0d0) THEN
                ZoneSteamEq(Loop)%DesignLevel=IHGNumbers(2)*Zone(ZoneSteamEq(Loop)%ZonePtr)%FloorArea
                IF (Zone(ZoneSteamEq(Loop)%ZonePtr)%FloorArea <= 0.0d0) THEN
                  CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(AlphaName(1))//  &
                    '", specifies '//TRIM(cNumericFieldNames(2))//', but Zone Floor Area = 0.  0 Hot Water Equipment will result.')
                ENDIF
              ELSE
                CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(AlphaName(1))//  &
                                 '", invalid '//TRIM(cNumericFieldNames(2))//', value  [<0.0]='//  &
                                 TRIM(RoundSigDigits(IHGNumbers(2),3)))
                ErrorsFound=.true.
              ENDIF
            ENDIF
            IF (lNumericFieldBlanks(2)) THEN
              CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(AlphaName(1))//  &
                '", specifies '//TRIM(cNumericFieldNames(2))//', but that field is blank.  0 Hot Water Equipment will result.')
            ENDIF

          CASE('WATTS/PERSON','POWER/PERSON')
            IF (ZoneSteamEq(Loop)%ZonePtr /= 0) THEN
              IF (IHGNumbers(3) >= 0.0d0) THEN
                ZoneSteamEq(Loop)%DesignLevel=IHGNumbers(3)*Zone(ZoneSteamEq(Loop)%ZonePtr)%TotOccupants
                IF (Zone(ZoneSteamEq(Loop)%ZonePtr)%TotOccupants <= 0.0d0) THEN
                  CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(AlphaName(1))//  &
                    '", specifies '//TRIM(cNumericFieldNames(2))//', but Total Occupants = 0.  0 Hot Water Equipment will result.')
                ENDIF
              ELSE
                CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(AlphaName(1))//  &
                                 '", invalid '//TRIM(cNumericFieldNames(3))//', value  [<0.0]='//  &
                                 TRIM(RoundSigDigits(IHGNumbers(3),3)))
                ErrorsFound=.true.
              ENDIF
            ENDIF
            IF (lNumericFieldBlanks(3)) THEN
              CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(AlphaName(1))//  &
                '", specifies '//TRIM(cNumericFieldNames(3))//', but that field is blank.  0 Hot Water Equipment will result.')
            ENDIF

          CASE DEFAULT
            IF (Item1 == 1) THEN
              CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(AlphaName(1))//  &
                                 '", invalid '//TRIM(cAlphaFieldNames(4))//', value  ='//  &
                                 TRIM(AlphaName(4)))
              CALL ShowContinueError('...Valid values are "EquipmentLevel", "Watts/Area", "Watts/Person".')
              ErrorsFound=.true.
            ENDIF
        END SELECT

        ! Calculate nominal min/max equipment level
        ZoneSteamEq(Loop)%NomMinDesignLevel=ZoneSteamEq(Loop)%DesignLevel*SchMin
        ZoneSteamEq(Loop)%NomMaxDesignLevel=ZoneSteamEq(Loop)%DesignLevel*SchMax

        ZoneSteamEq(Loop)%FractionLatent=IHGNumbers(4)
        ZoneSteamEq(Loop)%FractionRadiant=IHGNumbers(5)
        ZoneSteamEq(Loop)%FractionLost=IHGNumbers(6)
                    ! FractionConvected is a calculated field
        ZoneSteamEq(Loop)%FractionConvected=1.0d0 - (ZoneSteamEq(Loop)%FractionLatent +   &
                                                    ZoneSteamEq(Loop)%FractionRadiant +   &
                                                    ZoneSteamEq(Loop)%FractionLost)
        IF (ABS(ZoneSteamEq(Loop)%FractionConvected) <= .001d0) ZoneSteamEq(Loop)%FractionConvected=0.0d0
        IF (ZoneSteamEq(Loop)%FractionConvected < 0.0d0) THEN
          CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(AlphaName(1))//  &
                                 '", Sum of Fractions > 1.0')
          ErrorsFound=.true.
        ENDIF

        IF (NumAlpha > 4) THEN
          ZoneSteamEq(Loop)%EndUseSubcategory = AlphaName(5)
        ELSE
          ZoneSteamEq(Loop)%EndUseSubcategory = 'General'
        END IF

        IF (ZoneSteamEq(Loop)%ZonePtr <=0) CYCLE   ! Error, will be caught and terminated later

        ! Object report variables
        CALL SetupOutputVariable('Steam Equipment District Heating Rate [W]', ZoneSteamEq(Loop)%Power, &
                                 'Zone','Average',ZoneSteamEq(Loop)%Name)
        CALL SetupOutputVariable('Steam Equipment District Heating Energy [J]', ZoneSteamEq(Loop)%Consumption, &
                                 'Zone','Sum',ZoneSteamEq(Loop)%Name,ResourceTypeKey='DistrictHeating', &
                                 GroupKey='Building',ZoneKey=Zone(ZoneSteamEq(Loop)%ZonePtr)%Name, &
                                 EndUseKey='InteriorEquipment',EndUseSubKey=ZoneSteamEq(Loop)%EndUseSubcategory, &
                                 ZoneMult=Zone(ZoneSteamEq(Loop)%ZonePtr)%Multiplier, &
                                 ZoneListMult=Zone(ZoneSteamEq(Loop)%ZonePtr)%ListMultiplier)

        CALL SetupOutputVariable('Steam Equipment Radiant Heating Energy [J]',ZoneSteamEq(Loop)%RadGainEnergy, &
                                 'Zone','Sum',ZoneSteamEq(Loop)%Name)
        CALL SetupOutputVariable('Steam Equipment Radiant Heating Rate [W]',ZoneSteamEq(Loop)%RadGainRate, &
                                 'Zone','Average',ZoneSteamEq(Loop)%Name)
        CALL SetupOutputVariable('Steam Equipment Convective Heating Energy [J]',ZoneSteamEq(Loop)%ConGainEnergy, &
                                 'Zone','Sum',ZoneSteamEq(Loop)%Name)
        CALL SetupOutputVariable('Steam Equipment Convective Heating Rate [W]',ZoneSteamEq(Loop)%ConGainRate, &
                                 'Zone','Average',ZoneSteamEq(Loop)%Name)
        CALL SetupOutputVariable('Steam Equipment Latent Gain Energy [J]',ZoneSteamEq(Loop)%LatGainEnergy, &
                                 'Zone','Sum',ZoneSteamEq(Loop)%Name)
        CALL SetupOutputVariable('Steam Equipment Latent Gain Rate [W]',ZoneSteamEq(Loop)%LatGainRate, &
                                 'Zone','Average',ZoneSteamEq(Loop)%Name)
        CALL SetupOutputVariable('Steam Equipment Lost Heat Energy [J]',ZoneSteamEq(Loop)%LostEnergy, &
                                 'Zone','Sum',ZoneSteamEq(Loop)%Name)
        CALL SetupOutputVariable('Steam Equipment Lost Heat Rate [W]',ZoneSteamEq(Loop)%LostRate, &
                                 'Zone','Average',ZoneSteamEq(Loop)%Name)
        CALL SetupOutputVariable('Steam Equipment Total Heating Energy [J]',ZoneSteamEq(Loop)%TotGainEnergy, &
                                 'Zone','Sum',ZoneSteamEq(Loop)%Name)
        CALL SetupOutputVariable('Steam Equipment Total Heating Rate [W]',ZoneSteamEq(Loop)%TotGainRate, &
                                 'Zone','Average',ZoneSteamEq(Loop)%Name)

        ! Zone total report variables
        IF (RepVarSet(ZoneSteamEq(Loop)%ZonePtr)) THEN
          RepVarSet(ZoneSteamEq(Loop)%ZonePtr)=.false.
          CALL SetupOutputVariable('Zone Steam Equipment District Heating Rate [W]', &
                                   ZnRpt(ZoneSteamEq(Loop)%ZonePtr)%SteamPower, &
                                   'Zone','Average',Zone(ZoneSteamEq(Loop)%ZonePtr)%Name)
          CALL SetupOutputVariable('Zone Steam Equipment District Heating Energy [J]', &
                                   ZnRpt(ZoneSteamEq(Loop)%ZonePtr)%SteamConsump, &
                                   'Zone','Sum',Zone(ZoneSteamEq(Loop)%ZonePtr)%Name)

          CALL SetupOutputVariable('Zone Steam Equipment Radiant Heating Energy [J]',  &
             ZnRpt(ZoneSteamEq(Loop)%ZonePtr)%SteamRadGain, &
                                   'Zone','Sum',Zone(ZoneSteamEq(Loop)%ZonePtr)%Name)
          CALL SetupOutputVariable('Zone Steam Equipment Radiant Heating Rate [W]',  &
             ZnRpt(ZoneSteamEq(Loop)%ZonePtr)%SteamRadGainRate, &
                                   'Zone','Average',Zone(ZoneSteamEq(Loop)%ZonePtr)%Name)
          CALL SetupOutputVariable('Zone Steam Equipment Convective Heating Energy [J]',  &
             ZnRpt(ZoneSteamEq(Loop)%ZonePtr)%SteamConGain, &
                                   'Zone','Sum',Zone(ZoneSteamEq(Loop)%ZonePtr)%Name)
          CALL SetupOutputVariable('Zone Steam Equipment Convective Heating Rate [W]',  &
             ZnRpt(ZoneSteamEq(Loop)%ZonePtr)%SteamConGainRate, &
                                   'Zone','Average',Zone(ZoneSteamEq(Loop)%ZonePtr)%Name)
          CALL SetupOutputVariable('Zone Steam Equipment Latent Gain Energy [J]',  &
             ZnRpt(ZoneSteamEq(Loop)%ZonePtr)%SteamLatGain, &
                                   'Zone','Sum',Zone(ZoneSteamEq(Loop)%ZonePtr)%Name)
          CALL SetupOutputVariable('Zone Steam Equipment Latent Gain Rate [W]',  &
             ZnRpt(ZoneSteamEq(Loop)%ZonePtr)%SteamLatGainRate, &
                                   'Zone','Average',Zone(ZoneSteamEq(Loop)%ZonePtr)%Name)
          CALL SetupOutputVariable('Zone Steam Equipment Lost Heat Energy [J]',  &
             ZnRpt(ZoneSteamEq(Loop)%ZonePtr)%SteamLost, &
                                   'Zone','Sum',Zone(ZoneSteamEq(Loop)%ZonePtr)%Name)
          CALL SetupOutputVariable('Zone Steam Equipment Lost Heat Rate [W]',  &
             ZnRpt(ZoneSteamEq(Loop)%ZonePtr)%SteamLostRate, &
                                   'Zone','Average',Zone(ZoneSteamEq(Loop)%ZonePtr)%Name)
          CALL SetupOutputVariable('Zone Steam Equipment Total Heating Energy [J]',  &
             ZnRpt(ZoneSteamEq(Loop)%ZonePtr)%SteamTotGain, &
                                   'Zone','Sum',Zone(ZoneSteamEq(Loop)%ZonePtr)%Name)
          CALL SetupOutputVariable('Zone Steam Equipment Total Heating Rate [W]',  &
             ZnRpt(ZoneSteamEq(Loop)%ZonePtr)%SteamTotGainRate, &
                                   'Zone','Average',Zone(ZoneSteamEq(Loop)%ZonePtr)%Name)
        ENDIF

        IF (AnyEnergyManagementSystemInModel) Then
          CALL SetupEMSActuator('SteamEquipment', ZoneSteamEq(Loop)%Name, 'District Heating Power Level', '[W]', &
                    ZoneSteamEq(Loop)%EMSZoneEquipOverrideOn  , ZoneSteamEq(Loop)%EMSEquipPower )
          CALL SetupEMSInternalVariable( 'Process Steam District Heat Design Level', ZoneSteamEq(Loop)%Name, '[W]' ,&
                                           ZoneSteamEq(Loop)%DesignLevel )
        ENDIF ! EMS

        IF (.not. ErrorsFound)   &
          CALL SetupZoneInternalGain(ZoneSteamEq(Loop)%ZonePtr, &
                       'SteamEquipment', &
                       ZoneSteamEq(Loop)%Name, &
                       IntGainTypeOf_SteamEquipment, &
                       ConvectionGainRate          = ZoneSteamEq(Loop)%ConGainRate,&
                       ThermalRadiationGainRate    = ZoneSteamEq(Loop)%RadGainRate, &
                       LatentGainRate              = ZoneSteamEq(Loop)%LatGainRate)

      END DO ! Item1
    END DO ! Item - number of hot water statements
  END IF

  RepVarSet=.true.
  CurrentModuleObject='OtherEquipment'
  NumOtherEqStatements=GetNumObjectsFound(CurrentModuleObject)
  ALLOCATE(OtherEqObjects(NumOtherEqStatements))

  TotOthEquip=0
  ErrFlag=.false.
  DO Item=1,NumOtherEqStatements
    CALL GetObjectItem(CurrentModuleObject,Item,AlphaName,NumAlpha,IHGNumbers,NumNumber,IOStat,  &
                   AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                   AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
    IsNotOK = .FALSE.
    IsBlank = .FALSE.
    CALL VerifyName(AlphaName(1),OtherEqObjects%Name,Item-1,IsNotOK,IsBlank,TRIM(CurrentModuleObject)//' Name')
    IF (IsNotOK) THEN
      ErrorsFound = .TRUE.
      ErrFlag=.true.
      IF (IsBlank) AlphaName(1) = 'xxxxx'
    END IF
    OtherEqObjects(Item)%Name = AlphaName(1)

    Item1=FindItemInList(AlphaName(2),Zone%Name,NumOfZones)
    ZLItem=0
    IF (Item1 == 0 .and. NumOfZoneLists > 0) &
        ZLItem=FindItemInList(AlphaName(2),ZoneList%Name,NumOfZoneLists)
    IF (Item1 > 0) THEN
      OtherEqObjects(Item)%StartPtr=TotOthEquip+1
      TotOthEquip=TotOthEquip+1
      OtherEqObjects(Item)%NumOfZones=1
      OtherEqObjects(Item)%ZoneListActive=.false.
      OtherEqObjects(Item)%ZoneOrZoneListPtr=Item1
    ELSEIF (ZLItem > 0) THEN
      OtherEqObjects(Item)%StartPtr=TotOthEquip+1
      TotOthEquip=TotOthEquip+ZoneList(ZLItem)%NumOfZones
      OtherEqObjects(Item)%NumOfZones=ZoneList(ZLItem)%NumOfZones
      OtherEqObjects(Item)%ZoneListActive=.true.
      OtherEqObjects(Item)%ZoneOrZoneListPtr=ZLItem
    ELSE
      CALL ShowSevereError(trim(CurrentModuleObject)//'="'//trim(AlphaName(1))//'" invalid '//  &
           trim(cAlphaFieldNames(2))//'="'//trim(AlphaName(2))//'" not found.')
      ErrorsFound=.true.
      ErrFlag=.true.
    ENDIF
  ENDDO

  IF (ErrFlag) THEN
    CALL ShowSevereError(RoutineName//'Errors with invalid names in '//trim(CurrentModuleObject)//  &
       ' objects.')
    CALL ShowContinueError('...These will not be read in.  Other errors may occur.')
    TotOthEquip=0
  ENDIF

  ALLOCATE(ZoneOtherEq(TotOthEquip))

  IF (TotOthEquip > 0) THEN
    Loop=0
    DO Item = 1, NumOtherEqStatements
      AlphaName  = Blank
      IHGNumbers = 0.0d0

      CALL GetObjectItem(CurrentModuleObject,Item,AlphaName,NumAlpha,IHGNumbers,NumNumber,IOStat,  &
                     AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                     AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

      DO Item1=1,OtherEqObjects(Item)%NumOfZones
        Loop=Loop+1
        IF (.not. OtherEqObjects(Item)%ZoneListActive) THEN
          ZoneOtherEq(Loop)%Name = AlphaName(1)
          ZoneOtherEq(Loop)%ZonePtr = OtherEqObjects(Item)%ZoneOrZoneListPtr
        ELSE
          CALL CheckCreatedZoneItemName(RoutineName,CurrentModuleObject,  &
                                        Zone(ZoneList(OtherEqObjects(Item)%ZoneOrZoneListPtr)%Zone(Item1))%Name,  &
                                        ZoneList(OtherEqObjects(Item)%ZoneOrZoneListPtr)%MaxZoneNameLength,  &
                                        OtherEqObjects(Item)%Name,     &
                                        ZoneOtherEq%Name,           &
                                        Loop-1,                       &
                                        ZoneOtherEq(Loop)%Name,            &
                                        ErrFlag)
          ZoneOtherEq(Loop)%ZonePtr = ZoneList(OtherEqObjects(Item)%ZoneOrZoneListPtr)%Zone(Item1)
          IF (ErrFlag) ErrorsFound=.true.
        ENDIF

        ZoneOtherEq(Loop)%SchedPtr=GetScheduleIndex(AlphaName(3))
        SchMin=0.0d0
        SchMax=0.0d0
        IF (ZoneOtherEq(Loop)%SchedPtr == 0) THEN
          IF (lAlphaFieldBlanks(3)) THEN
            CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(AlphaName(1))//  &
                                 '", '//TRIM(cAlphaFieldNames(3))//' is required.')
          ELSE
            CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(AlphaName(1))//  &
                                 '", invalid '//TRIM(cAlphaFieldNames(3))//' entered='//TRIM(AlphaName(3)))
          ENDIF
          ErrorsFound=.true.
        ELSE  ! check min/max on schedule
          SchMin=GetScheduleMinValue(ZoneOtherEq(Loop)%SchedPtr)
          SchMax=GetScheduleMaxValue(ZoneOtherEq(Loop)%SchedPtr)
        ENDIF

        ! Hot Water equipment design level calculation method.
        SELECT CASE (AlphaName(4))
          CASE('EQUIPMENTLEVEL')
            ZoneOtherEq(Loop)%DesignLevel=IHGNumbers(1)
            IF (lNumericFieldBlanks(1)) THEN
              CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(AlphaName(1))//  &
                '", specifies '//TRIM(cNumericFieldNames(1))//', but that field is blank.  0 Hot Water Equipment will result.')
            ENDIF

          CASE('WATTS/AREA','POWER/AREA')
            IF (ZoneOtherEq(Loop)%ZonePtr /= 0) THEN
              ZoneOtherEq(Loop)%DesignLevel=IHGNumbers(2)*Zone(ZoneOtherEq(Loop)%ZonePtr)%FloorArea
              IF (Zone(ZoneOtherEq(Loop)%ZonePtr)%FloorArea <= 0.0d0) THEN
                CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(AlphaName(1))//  &
                  '", specifies '//TRIM(cNumericFieldNames(2))//', but Zone Floor Area = 0.  0 Hot Water Equipment will result.')
              ENDIF
            ENDIF
            IF (lNumericFieldBlanks(2)) THEN
              CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(AlphaName(1))//  &
                '", specifies '//TRIM(cNumericFieldNames(2))//', but that field is blank.  0 Hot Water Equipment will result.')
            ENDIF

          CASE('WATTS/PERSON','POWER/PERSON')
            IF (ZoneOtherEq(Loop)%ZonePtr /= 0) THEN
              ZoneOtherEq(Loop)%DesignLevel=IHGNumbers(3)*Zone(ZoneOtherEq(Loop)%ZonePtr)%TotOccupants
              IF (Zone(ZoneOtherEq(Loop)%ZonePtr)%TotOccupants <= 0.0d0) THEN
                CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(AlphaName(1))//  &
                  '", specifies '//TRIM(cNumericFieldNames(2))//', but Total Occupants = 0.  0 Hot Water Equipment will result.')
              ENDIF
            ENDIF
            IF (lNumericFieldBlanks(3)) THEN
              CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(AlphaName(1))//  &
                '", specifies '//TRIM(cNumericFieldNames(3))//', but that field is blank.  0 Hot Water Equipment will result.')
            ENDIF

          CASE DEFAULT
            IF (Item1 == 1) THEN
              CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(AlphaName(1))//  &
                                 '", invalid '//TRIM(cAlphaFieldNames(4))//', value  ='//  &
                                 TRIM(AlphaName(4)))
              CALL ShowContinueError('...Valid values are "EquipmentLevel", "Watts/Area", "Watts/Person".')
              ErrorsFound=.true.
            ENDIF
        END SELECT

        ! Calculate nominal min/max equipment level
        ZoneOtherEq(Loop)%NomMinDesignLevel=ZoneOtherEq(Loop)%DesignLevel*SchMin
        ZoneOtherEq(Loop)%NomMaxDesignLevel=ZoneOtherEq(Loop)%DesignLevel*SchMax

        ZoneOtherEq(Loop)%FractionLatent=IHGNumbers(4)
        ZoneOtherEq(Loop)%FractionRadiant=IHGNumbers(5)
        ZoneOtherEq(Loop)%FractionLost=IHGNumbers(6)
                    ! FractionConvected is a calculated field
        ZoneOtherEq(Loop)%FractionConvected=1.0d0 - (ZoneOtherEq(Loop)%FractionLatent +   &
                                                    ZoneOtherEq(Loop)%FractionRadiant +   &
                                                    ZoneOtherEq(Loop)%FractionLost)
        IF (ABS(ZoneOtherEq(Loop)%FractionConvected) <= .001d0) ZoneOtherEq(Loop)%FractionConvected=0.0d0
        IF (ZoneOtherEq(Loop)%FractionConvected < 0.0d0) THEN
          CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(AlphaName(1))//  &
                                 '", Sum of Fractions > 1.0')
          ErrorsFound=.true.
        ENDIF

        IF (NumAlpha > 4) THEN
          ZoneOtherEq(Loop)%EndUseSubcategory = AlphaName(5)
        ELSE
          ZoneOtherEq(Loop)%EndUseSubcategory = 'General'
        END IF

        IF (ZoneOtherEq(Loop)%ZonePtr <=0) CYCLE   ! Error, will be caught and terminated later

        ! Object report variables
        CALL SetupOutputVariable('Other Equipment Radiant Heating Energy [J]',ZoneOtherEq(Loop)%RadGainEnergy, &
                                 'Zone','Sum',ZoneOtherEq(Loop)%Name)
        CALL SetupOutputVariable('Other Equipment Radiant Heating Rate [W]',ZoneOtherEq(Loop)%RadGainRate, &
                                 'Zone','Average',ZoneOtherEq(Loop)%Name)
        CALL SetupOutputVariable('Other Equipment Convective Heating Energy [J]',ZoneOtherEq(Loop)%ConGainEnergy, &
                                 'Zone','Sum',ZoneOtherEq(Loop)%Name)
        CALL SetupOutputVariable('Other Equipment Convective Heating Rate [W]',ZoneOtherEq(Loop)%ConGainRate, &
                                 'Zone','Average',ZoneOtherEq(Loop)%Name)
        CALL SetupOutputVariable('Other Equipment Latent Gain Energy [J]',ZoneOtherEq(Loop)%LatGainEnergy, &
                                 'Zone','Sum',ZoneOtherEq(Loop)%Name)
        CALL SetupOutputVariable('Other Equipment Latent Gain Rate [W]',ZoneOtherEq(Loop)%LatGainRate, &
                                 'Zone','Average',ZoneOtherEq(Loop)%Name)
        CALL SetupOutputVariable('Other Equipment Lost Heat Energy [J]',ZoneOtherEq(Loop)%LostEnergy, &
                                 'Zone','Sum',ZoneOtherEq(Loop)%Name)
        CALL SetupOutputVariable('Other Equipment Lost Heat Rate [W]',ZoneOtherEq(Loop)%LostRate, &
                                 'Zone','Average',ZoneOtherEq(Loop)%Name)
        CALL SetupOutputVariable('Other Equipment Total Heating Energy [J]',ZoneOtherEq(Loop)%TotGainEnergy, &
                                 'Zone','Sum',ZoneOtherEq(Loop)%Name)
        CALL SetupOutputVariable('Other Equipment Total Heating Rate [W]',ZoneOtherEq(Loop)%TotGainRate, &
                                 'Zone','Average',ZoneOtherEq(Loop)%Name)

        ! Zone total report variables
        IF (RepVarSet(ZoneOtherEq(Loop)%ZonePtr)) THEN
          RepVarSet(ZoneOtherEq(Loop)%ZonePtr)=.false.
          CALL SetupOutputVariable('Zone Other Equipment Radiant Heating Energy [J]',  &
                                    ZnRpt(ZoneOtherEq(Loop)%ZonePtr)%OtherRadGain, &
                                   'Zone','Sum',Zone(ZoneOtherEq(Loop)%ZonePtr)%Name)
          CALL SetupOutputVariable('Zone Other Equipment Radiant Heating Rate [W]',  &
                                    ZnRpt(ZoneOtherEq(Loop)%ZonePtr)%OtherRadGainRate, &
                                   'Zone','Average',Zone(ZoneOtherEq(Loop)%ZonePtr)%Name)
          CALL SetupOutputVariable('Zone Other Equipment Convective Heating Energy [J]',  &
                                    ZnRpt(ZoneOtherEq(Loop)%ZonePtr)%OtherConGain, &
                                   'Zone','Sum',Zone(ZoneOtherEq(Loop)%ZonePtr)%Name)
          CALL SetupOutputVariable('Zone Other Equipment Convective Heating Rate [W]',  &
                                    ZnRpt(ZoneOtherEq(Loop)%ZonePtr)%OtherConGainRate, &
                                   'Zone','Average',Zone(ZoneOtherEq(Loop)%ZonePtr)%Name)
          CALL SetupOutputVariable('Zone Other Equipment Latent Gain Energy [J]',  &
                                    ZnRpt(ZoneOtherEq(Loop)%ZonePtr)%OtherLatGain, &
                                   'Zone','Sum',Zone(ZoneOtherEq(Loop)%ZonePtr)%Name)
          CALL SetupOutputVariable('Zone Other Equipment Latent Gain Rate [W]',  &
                                    ZnRpt(ZoneOtherEq(Loop)%ZonePtr)%OtherLatGainRate, &
                                   'Zone','Average',Zone(ZoneOtherEq(Loop)%ZonePtr)%Name)
          CALL SetupOutputVariable('Zone Other Equipment Lost Heat Energy [J]',  &
                                    ZnRpt(ZoneOtherEq(Loop)%ZonePtr)%OtherLost, &
                                   'Zone','Sum',Zone(ZoneOtherEq(Loop)%ZonePtr)%Name)
          CALL SetupOutputVariable('Zone Other Equipment Lost Heat Rate [W]',  &
                                    ZnRpt(ZoneOtherEq(Loop)%ZonePtr)%OtherLostRate, &
                                   'Zone','Average',Zone(ZoneOtherEq(Loop)%ZonePtr)%Name)
          CALL SetupOutputVariable('Zone Other Equipment Total Heating Energy [J]',  &
                                    ZnRpt(ZoneOtherEq(Loop)%ZonePtr)%OtherTotGain, &
                                   'Zone','Sum',Zone(ZoneOtherEq(Loop)%ZonePtr)%Name)
          CALL SetupOutputVariable('Zone Other Equipment Total Heating Rate [W]',  &
                                    ZnRpt(ZoneOtherEq(Loop)%ZonePtr)%OtherTotGainRate, &
                                   'Zone','Average',Zone(ZoneOtherEq(Loop)%ZonePtr)%Name)
        ENDIF
        IF (AnyEnergyManagementSystemInModel) Then
          CALL SetupEMSActuator('OtherEquipment', ZoneOtherEq(Loop)%Name, 'Power Level', '[W]', &
                    ZoneOtherEq(Loop)%EMSZoneEquipOverrideOn  , ZoneOtherEq(Loop)%EMSEquipPower )
          CALL SetupEMSInternalVariable( 'Other Equipment Design Level', ZoneOtherEq(Loop)%Name, '[W]' ,&
                                           ZoneOtherEq(Loop)%DesignLevel )
        ENDIF ! EMS

        IF (.not. ErrorsFound)   &
          CALL SetupZoneInternalGain(ZoneOtherEq(Loop)%ZonePtr, &
                       'OtherEquipment', &
                       ZoneOtherEq(Loop)%Name, &
                       IntGainTypeOf_OtherEquipment, &
                       ConvectionGainRate          = ZoneOtherEq(Loop)%ConGainRate,&
                       ThermalRadiationGainRate    = ZoneOtherEq(Loop)%RadGainRate, &
                       LatentGainRate              = ZoneOtherEq(Loop)%LatGainRate)

      END DO ! Item1
    END DO ! Item - number of hot water statements
  END IF


  RepVarSet=.true.
  CurrentModuleObject='ZoneBaseboard:OutdoorTemperatureControlled'
  TotBBHeat=GetNumObjectsFound(CurrentModuleObject)
  ALLOCATE(ZoneBBHeat(TotBBHeat))

  DO Loop=1,TotBBHeat
    AlphaName='  '
    IHGNumbers=0.0d0
    CALL GetObjectItem(CurrentModuleObject,Loop,AlphaName,NumAlpha,IHGNumbers,NumNumber,IOStat,  &
                   AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                   AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

    IsNotOK = .FALSE.
    IsBlank = .FALSE.
    CALL VerifyName(AlphaName(1),ZoneBBHeat%Name,Loop-1,IsNotOK,IsBlank,TRIM(CurrentModuleObject)//' Name')
    IF (IsNotOK) THEN
      ErrorsFound = .TRUE.
      IF (IsBlank) AlphaName(1) = 'xxxxx'
    END IF
    ZoneBBHeat(Loop)%Name = AlphaName(1)

    ZoneBBHeat(Loop)%ZonePtr=FindIteminList(AlphaName(2),Zone%Name,NumOfZones)
    IF (ZoneBBHeat(Loop)%ZonePtr == 0) THEN
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(AlphaName(1))//  &
                           '", invalid '//TRIM(cAlphaFieldNames(2))//  &
                           ' entered='//TRIM(AlphaName(2)))
      ErrorsFound=.true.
    ENDIF

    ZoneBBHeat(Loop)%SchedPtr=GetScheduleIndex(AlphaName(3))
    IF (ZoneBBHeat(Loop)%SchedPtr == 0) THEN
      IF (lAlphaFieldBlanks(3)) THEN
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(AlphaName(1))//  &
                             '", '//TRIM(cAlphaFieldNames(3))//' is required.')
      ELSE
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(AlphaName(1))//  &
                             '", invalid '//TRIM(cAlphaFieldNames(3))//' entered='//TRIM(AlphaName(3)))
      ENDIF
      ErrorsFound=.true.
    ELSE  ! check min/max on schedule
      SchMin=GetScheduleMinValue(ZoneBBHeat(Loop)%SchedPtr)
      SchMax=GetScheduleMaxValue(ZoneBBHeat(Loop)%SchedPtr)
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

    IF (NumAlpha > 3) THEN
      ZoneBBHeat(Loop)%EndUseSubcategory = AlphaName(4)
    ELSE
      ZoneBBHeat(Loop)%EndUseSubcategory = 'General'
    END IF

    ZoneBBHeat(Loop)%CapatLowTemperature=IHGNumbers(1)
    ZoneBBHeat(Loop)%LowTemperature=IHGNumbers(2)
    ZoneBBHeat(Loop)%CapatHighTemperature=IHGNumbers(3)
    ZoneBBHeat(Loop)%HighTemperature=IHGNumbers(4)
    ZoneBBHeat(Loop)%FractionRadiant=IHGNumbers(5)
    ZoneBBHeat(Loop)%FractionConvected=1.0d0-ZoneBBHeat(Loop)%FractionRadiant
    IF (ZoneBBHeat(Loop)%FractionConvected < 0.0d0) THEN
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(AlphaName(1))//  &
                             '", Sum of Fractions > 1.0')
      ErrorsFound=.true.
    ENDIF

    IF (ZoneBBHeat(Loop)%ZonePtr <=0) CYCLE   ! Error, will be caught and terminated later

    ! Object report variables
    CALL SetupOutputVariable('Baseboard Electric Power [W]',ZoneBBHeat(Loop)%Power, &
                             'Zone','Average',ZoneBBHeat(Loop)%Name)
    CALL SetupOutputVariable('Baseboard Electric Energy [J]',ZoneBBHeat(Loop)%Consumption, &
                             'Zone','Sum',ZoneBBHeat(Loop)%Name, ResourceTypeKey='Electricity', &
                             GroupKey='Building',ZoneKey=Zone(ZoneBBHeat(Loop)%ZonePtr)%Name, &
                             EndUseKey='InteriorEquipment',EndUseSubKey=ZoneBBHeat(Loop)%EndUseSubcategory, &
                             ZoneMult=Zone(ZoneBBHeat(Loop)%ZonePtr)%Multiplier, &
                             ZoneListMult=Zone(ZoneBBHeat(Loop)%ZonePtr)%ListMultiplier)

    CALL SetupOutputVariable('Baseboard Radiant Heating Energy [J]',ZoneBBHeat(Loop)%RadGainEnergy, &
                             'Zone','Sum',ZoneBBHeat(Loop)%Name)
    CALL SetupOutputVariable('Baseboard Radiant Heating Rate [W]',ZoneBBHeat(Loop)%RadGainRate, &
                             'Zone','Average',ZoneBBHeat(Loop)%Name)
    CALL SetupOutputVariable('Baseboard Convective Heating Energy [J]',ZoneBBHeat(Loop)%ConGainEnergy, &
                             'Zone','Sum',ZoneBBHeat(Loop)%Name)
    CALL SetupOutputVariable('Baseboard Convective Heating Rate [W]',ZoneBBHeat(Loop)%ConGainRate, &
                             'Zone','Average',ZoneBBHeat(Loop)%Name)
    CALL SetupOutputVariable('Baseboard Total Heating Energy [J]',ZoneBBHeat(Loop)%TotGainEnergy, &
                             'Zone','Sum',ZoneBBHeat(Loop)%Name)
    CALL SetupOutputVariable('Baseboard Total Heating Rate [W]',ZoneBBHeat(Loop)%TotGainRate, &
                             'Zone','Average',ZoneBBHeat(Loop)%Name)

    ! Zone total report variables
    IF (RepVarSet(ZoneBBHeat(Loop)%ZonePtr)) THEN
      RepVarSet(ZoneBBHeat(Loop)%ZonePtr)=.false.
      CALL SetupOutputVariable('Zone Baseboard Electric Power [W]',ZnRpt(ZoneBBHeat(Loop)%ZonePtr)%BaseHeatPower, &
                               'Zone','Average',Zone(ZoneBBHeat(Loop)%ZonePtr)%Name)
      CALL SetupOutputVariable('Zone Baseboard Electric Energy [J]',ZnRpt(ZoneBBHeat(Loop)%ZonePtr)%BaseHeatElecCons, &
                               'Zone','Sum',Zone(ZoneBBHeat(Loop)%ZonePtr)%Name)

      CALL SetupOutputVariable('Zone Baseboard Radiant Heating Energy [J]',  &
         ZnRpt(ZoneBBHeat(Loop)%ZonePtr)%BaseHeatRadGain, &
                               'Zone','Sum',Zone(ZoneBBHeat(Loop)%ZonePtr)%Name)
      CALL SetupOutputVariable('Zone Baseboard Radiant Heating Rate [W]',  &
         ZnRpt(ZoneBBHeat(Loop)%ZonePtr)%BaseHeatRadGainRate, &
                               'Zone','Average',Zone(ZoneBBHeat(Loop)%ZonePtr)%Name)
      CALL SetupOutputVariable('Zone Baseboard Convective Heating Energy [J]',  &
         ZnRpt(ZoneBBHeat(Loop)%ZonePtr)%BaseHeatConGain, &
                               'Zone','Sum',Zone(ZoneBBHeat(Loop)%ZonePtr)%Name)
      CALL SetupOutputVariable('Zone Baseboard Convective Heating Rate [W]',  &
         ZnRpt(ZoneBBHeat(Loop)%ZonePtr)%BaseHeatConGainRate, &
                               'Zone','Average',Zone(ZoneBBHeat(Loop)%ZonePtr)%Name)
      CALL SetupOutputVariable('Zone Baseboard Total Heating Energy [J]',  &
         ZnRpt(ZoneBBHeat(Loop)%ZonePtr)%BaseHeatTotGain, &
                               'Zone','Sum',Zone(ZoneBBHeat(Loop)%ZonePtr)%Name)
      CALL SetupOutputVariable('Zone Baseboard Total Heating Rate [W]',  &
         ZnRpt(ZoneBBHeat(Loop)%ZonePtr)%BaseHeatTotGainRate, &
                               'Zone','Average',Zone(ZoneBBHeat(Loop)%ZonePtr)%Name)
    ENDIF

    IF (AnyEnergyManagementSystemInModel) Then
      CALL SetupEMSActuator('ZoneBaseboard:OutdoorTemperatureControlled', ZoneBBHeat(Loop)%Name, 'Power Level', '[W]', &
                ZoneBBHeat(Loop)%EMSZoneBaseboardOverrideOn  , ZoneBBHeat(Loop)%EMSZoneBaseboardPower )
      CALL SetupEMSInternalVariable( 'Simple Zone Baseboard Capacity At Low Temperature', ZoneBBHeat(Loop)%Name, '[W]' ,&
                                       ZoneBBHeat(Loop)%CapatLowTemperature )
      CALL SetupEMSInternalVariable( 'Simple Zone Baseboard Capacity At High Temperature', ZoneBBHeat(Loop)%Name, '[W]' ,&
                                       ZoneBBHeat(Loop)%CapatHighTemperature )
    ENDIF ! EMS

    CALL SetupZoneInternalGain(ZoneBBHeat(Loop)%ZonePtr, &
                   'ZoneBaseboard:OutdoorTemperatureControlled', &
                   ZoneBBHeat(Loop)%Name, &
                   IntGainTypeOf_ZoneBaseboardOutdoorTemperatureControlled, &
                   ConvectionGainRate          = ZoneBBHeat(Loop)%ConGainRate,&
                   ThermalRadiationGainRate    = ZoneBBHeat(Loop)%RadGainRate)

  END DO

  RepVarSet=.true.
  CurrentModuleObject='ZoneContaminantSourceAndSink:CarbonDioxide'
  TotCO2Gen=GetNumObjectsFound(CurrentModuleObject)
  ALLOCATE(ZoneCO2Gen(TotCO2Gen))

  DO Loop=1,TotCO2Gen
    AlphaName='  '
    IHGNumbers=0.0d0
    CALL GetObjectItem(CurrentModuleObject,Loop,AlphaName,NumAlpha,IHGNumbers,NumNumber,IOStat,  &
                   AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                   AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

    IsNotOK = .FALSE.
    IsBlank = .FALSE.
    CALL VerifyName(AlphaName(1),ZoneCO2Gen%Name,Loop-1,IsNotOK,IsBlank,TRIM(CurrentModuleObject)//' Name')
    IF (IsNotOK) THEN
      ErrorsFound = .TRUE.
      IF (IsBlank) AlphaName(1) = 'xxxxx'
    END IF
    ZoneCO2Gen(Loop)%Name = AlphaName(1)

    ZoneCO2Gen(Loop)%ZonePtr=FindIteminList(AlphaName(2),Zone%Name,NumOfZones)
    IF (ZoneCO2Gen(Loop)%ZonePtr == 0) THEN
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(AlphaName(1))//  &
                           '", invalid '//TRIM(cAlphaFieldNames(2))//  &
                           ' entered='//TRIM(AlphaName(2)))
      ErrorsFound=.true.
    ENDIF

    ZoneCO2Gen(Loop)%SchedPtr=GetScheduleIndex(AlphaName(3))
    IF (ZoneCO2Gen(Loop)%SchedPtr == 0) THEN
      IF (lAlphaFieldBlanks(3)) THEN
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(AlphaName(1))//  &
                             '", '//TRIM(cAlphaFieldNames(3))//' is required.')
      ELSE
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(AlphaName(1))//  &
                             '", invalid '//TRIM(cAlphaFieldNames(3))//' entered='//TRIM(AlphaName(3)))
      ENDIF
      ErrorsFound=.true.
    ELSE  ! check min/max on schedule
      SchMin=GetScheduleMinValue(ZoneCO2Gen(Loop)%SchedPtr)
      SchMax=GetScheduleMaxValue(ZoneCO2Gen(Loop)%SchedPtr)
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

    ZoneCO2Gen(Loop)%CO2DesignRate=IHGNumbers(1)

    IF (ZoneCO2Gen(Loop)%ZonePtr <=0) CYCLE   ! Error, will be caught and terminated later

    ! Object report variables
    CALL SetupOutputVariable('Contaminant Source or Sink CO2 Gain Volume Flow Rate [m3/s]',ZoneCO2Gen(Loop)%CO2GainRate, &
                             'Zone','Average',ZoneCO2Gen(Loop)%Name)

    ! Zone total report variables
    IF (RepVarSet(ZoneCO2Gen(Loop)%ZonePtr)) THEN
      RepVarSet(ZoneCO2Gen(Loop)%ZonePtr)=.false.

      CALL SetupOutputVariable('Zone Contaminant Source or Sink CO2 Gain Volume Flow Rate [m3/s]',&
                                ZnRpt(ZoneCO2Gen(Loop)%ZonePtr)%CO2Rate, &
                               'Zone','Average',Zone(ZoneCO2Gen(Loop)%ZonePtr)%Name)

    ENDIF

    CALL SetupZoneInternalGain(ZoneCO2Gen(Loop)%ZonePtr, &
               'ZoneContaminantSourceAndSink:CarbonDioxide', &
               ZoneCO2Gen(Loop)%Name, &
               IntGainTypeOf_ZoneContaminantSourceAndSinkCarbonDioxide, &
               CarbonDioxideGainRate       = ZoneCO2Gen(Loop)%CO2GainRate)

  END DO


  DEALLOCATE(RepVarSet)
  DEALLOCATE(IHGNumbers)
  DEALLOCATE(AlphaName)

  IF (ErrorsFound) THEN
    CALL ShowFatalError(RoutineName//'Errors found in Getting Internal Gains Input, Program Stopped')
  ENDIF

  WRITE(OutputFileInits,721)
  DO Loop=1,NumOfZones
    LightTot=0.0d0
    ElecTot=0.0d0
    GasTot=0.0d0
    OthTot=0.0d0
    HWETot=0.0d0
    STMTot=0.0d0
    BBHeatInd='No'
    DO Loop1=1,TotLights
      IF (Lights(Loop1)%ZonePtr /= Loop) CYCLE
      LightTot=LightTot+Lights(Loop1)%DesignLevel
    ENDDO
    DO Loop1=1,TotElecEquip
      IF (ZoneElectric(Loop1)%ZonePtr /= Loop) CYCLE
      ElecTot=ElecTot+ZoneElectric(Loop1)%DesignLevel
    ENDDO
    DO Loop1=1,TotGasEquip
      IF (ZoneGas(Loop1)%ZonePtr /= Loop) CYCLE
      GasTot=GasTot+ZoneGas(Loop1)%DesignLevel
    ENDDO
    DO Loop1=1,TotOthEquip
      IF (ZoneOtherEq(Loop1)%ZonePtr /= Loop) CYCLE
      OthTot=OthTot+ZoneOtherEq(Loop1)%DesignLevel
    ENDDO
    DO Loop1=1,TotStmEquip
      IF (ZoneSteamEq(Loop1)%ZonePtr /= Loop) CYCLE
      STMTot=STMTot+ZoneSteamEq(Loop1)%DesignLevel
    ENDDO
    DO Loop1=1,TotHWEquip
      IF (ZoneHWEq(Loop1)%ZonePtr /= Loop) CYCLE
      HWETot=HWETot+ZoneHWEq(Loop1)%DesignLevel
    ENDDO
    DO Loop1=1,TotBBHeat
      IF (ZoneBBHeat(Loop1)%ZonePtr /= Loop) CYCLE
      BBHeatInd='Yes'
    ENDDO
    Zone(Loop)%InternalHeatGains=LightTot+ElecTot+GasTot+OthTot+HWETot+StmTot
    IF (Zone(Loop)%FloorArea > 0.0d0) THEN
      WRITE(OutputFileInits,720,advance='No') TRIM(Zone(Loop)%Name),TRIM(RoundSigDigits(Zone(Loop)%FloorArea,2)),  &
                TRIM(RoundSigDigits(Zone(Loop)%TotOccupants,1))
      IF (Zone(Loop)%TotOccupants > 0.0d0) THEN
        StringOut=RoundSigDigits(Zone(Loop)%FloorArea/Zone(Loop)%TotOccupants,3)
      ELSE
        StringOut='N/A'
      ENDIF
      WRITE(OutputFileInits,fmta,advance='No') TRIM(StringOut)//','
      StringOut=RoundSigDigits(Zone(Loop)%TotOccupants/Zone(Loop)%FloorArea,3)
      WRITE(OutputFileInits,fmta,advance='No') TRIM(StringOut)//','
      StringOut=RoundSigDigits(LightTot/Zone(Loop)%FloorArea,3)
      WRITE(OutputFileInits,fmta,advance='No') TRIM(StringOut)//','
      StringOut=RoundSigDigits(ElecTot/Zone(Loop)%FloorArea,3)
      WRITE(OutputFileInits,fmta,advance='No') TRIM(StringOut)//','
      StringOut=RoundSigDigits(GasTot/Zone(Loop)%FloorArea,3)
      WRITE(OutputFileInits,fmta,advance='No') TRIM(StringOut)//','
      StringOut=RoundSigDigits(OthTot/Zone(Loop)%FloorArea,3)
      WRITE(OutputFileInits,fmta,advance='No') TRIM(StringOut)//','
      StringOut=RoundSigDigits(HWETot/Zone(Loop)%FloorArea,3)
      WRITE(OutputFileInits,fmta,advance='No') TRIM(StringOut)//','
      StringOut=RoundSigDigits(StmTot/Zone(Loop)%FloorArea,3)
      WRITE(OutputFileInits,fmta,advance='No') TRIM(StringOut)//','
      StringOut=RoundSigDigits(Zone(Loop)%InternalHeatGains/Zone(Loop)%FloorArea,3)
      WRITE(OutputFileInits,fmta) TRIM(StringOut)//','//TRIM(BBHeatInd)
    ELSE
      WRITE(OutputFileInits,720,advance='No') TRIM(Zone(Loop)%Name),TRIM(RoundSigDigits(Zone(Loop)%FloorArea,2)),  &
                TRIM(RoundSigDigits(Zone(Loop)%TotOccupants,1))
      WRITE(OutputFileInits,fmta)'0.0,N/A,N/A,N/A,N/A,N/A,N/A,N/A,N/A'//TRIM(BBHeatInd)
    ENDIF
  ENDDO
  DO Loop=1,TotPeople
    IF (Loop == 1) WRITE(OutputFileInits,723,advance='No') 'People','Number of People {},'//  &
       'People/Floor Area {person/m2},Floor Area per person {m2/person},'//  &
       'Fraction Radiant,Fraction Convected,Sensible Fraction Calculation,Activity level,'//  &
       'ASHRAE 55 Warnings,Carbon Dioxide Generation Rate,Nominal Minimum Number of People,Nominal Maximum Number of People'
    IF (Loop == 1) THEN
      IF (People(Loop)%Fanger .or. People(Loop)%Pierce .or. People(Loop)%KSU) THEN
        WRITE(OutputFileInits,fmta) ',MRT Calculation Type,Work Efficiency, Clothing Insulation Calculation Method,' // &
          'Clothing Insulation Calculation Method Schedule,Clothing,Air Velocity,Fanger Calculation,Pierce Calculation,' // &
          'KSU Calculation'
      ELSE
        WRITE(OutputFileInits,fmta) ' '
      ENDIF
    ENDIF

    ZoneNum=People(Loop)%ZonePtr

    IF (ZoneNum == 0) THEN
      WRITE(OutputFileInits,724) 'People-Illegal Zone specified',TRIM(People(Loop)%Name)
      CYCLE
    ENDIF

    WRITE(OutputFileInits,722,advance='No') 'People',TRIM(People(Loop)%Name),  &
       TRIM(GetScheduleName(People(Loop)%NumberOfPeoplePtr)),  &
       TRIM(Zone(ZoneNum)%Name),TRIM(RoundSigDigits(Zone(ZoneNum)%FloorArea,2)),   &
       TRIM(RoundSigDigits(Zone(ZoneNum)%TotOccupants,1))
    WRITE(OutputFileInits,fmta,advance='No') TRIM(RoundSigDigits(People(Loop)%NumberOfPeople,1))//','
    IF (Zone(ZoneNum)%FloorArea > 0.0d0) THEN
      StringOut=RoundSigDigits(People(Loop)%NumberOfPeople/Zone(ZoneNum)%FloorArea,3)
    ELSE
      StringOut='N/A'
    ENDIF
    WRITE(OutputFileInits,fmta,advance='No') TRIM(StringOut)//','
    IF (People(Loop)%NumberOfPeople > 0.0d0) THEN
      IF (Zone(ZoneNum)%FloorArea > 0.0d0) THEN
        StringOut=RoundSigDigits(Zone(ZoneNum)%FloorArea/People(Loop)%NumberOfPeople,3)
      ELSE
        StringOut='N/A'
      ENDIF
    ELSE
      StringOut='N/A'
    ENDIF
    WRITE(OutputFileInits,fmta,advance='No') TRIM(StringOut)//','
    StringOut=RoundSigDigits(People(Loop)%FractionRadiant,3)
    WRITE(OutputFileInits,fmta,advance='No') TRIM(StringOut)//','
    StringOut=RoundSigDigits(People(Loop)%FractionConvected,3)
    WRITE(OutputFileInits,fmta,advance='No') TRIM(StringOut)//','
    IF (People(Loop)%UserSpecSensFrac == AutoCalculate) THEN
      StringOut='AutoCalculate'
    ELSE
      StringOut=RoundSigDigits(People(Loop)%UserSpecSensFrac,3)
    ENDIF
    WRITE(OutputFileInits,fmta,advance='No') TRIM(StringOut)//','
    StringOut=GetScheduleName(People(Loop)%ActivityLevelPtr)
    WRITE(OutputFileInits,fmta,advance='No') TRIM(StringOut)//','
    IF (People(Loop)%Show55Warning) THEN
      StringOut='Yes'
    ELSE
      StringOut='No'
    ENDIF
    WRITE(OutputFileInits,fmta,advance='No') TRIM(StringOut)//','
    StringOut=RoundSigDigits(People(Loop)%CO2RateFactor,4)
    WRITE(OutputFileInits,fmta,advance='No') TRIM(StringOut)//','
    StringOut=RoundSigDigits(People(Loop)%NomMinNumberPeople,0)
    WRITE(OutputFileInits,fmta,advance='No') TRIM(StringOut)//','
    StringOut=RoundSigDigits(People(Loop)%NomMaxNumberPeople,0)
    IF (People(Loop)%Fanger .or. People(Loop)%Pierce .or. People(Loop)%KSU) THEN
      WRITE(OutputFileInits,fmta,advance='No') TRIM(StringOut)//','
      IF (People(Loop)%MRTCalcType == ZoneAveraged) THEN
        StringOut='Zone Averaged'
      ELSEIF (People(Loop)%MRTCalcType == SurfaceWeighted) THEN
        StringOut='Surface Weighted'
      ELSEIF (People(Loop)%MRTCalcType == AngleFactor) THEN
        StringOut='Angle Factor'
      ELSE
        StringOut='N/A'
      ENDIF
      WRITE(OutputFileInits,fmta,advance='No') TRIM(StringOut)//','
      WRITE(OutputFileInits,fmta,advance='No') TRIM(GetScheduleName(People(Loop)%WorkEffPtr))//','

      IF (People(Loop)%ClothingType == 1) THEN
        StringOut='Clothing Insulation Schedule'
      ELSEIF (People(Loop)%ClothingType == 2) THEN
        StringOut='Dynamic Clothing Model ASHRAE55'
      ELSEIF (People(Loop)%ClothingType == 3) THEN
          StringOut='Calculation Method Schedule'
      ELSE
        StringOut='N/A'
      ENDIF
      WRITE(OutputFileInits,fmta,advance='No') TRIM(StringOut)//','

      IF (People(Loop)%ClothingType == 3) THEN
        StringOut = GetScheduleName(People(Loop)%ClothingMethodPtr)
      ELSE
        StringOut='N/A'
      ENDIF
      WRITE(OutputFileInits,fmta,advance='No') TRIM(StringOut)//','

      WRITE(OutputFileInits,fmta,advance='No') TRIM(GetScheduleName(People(Loop)%ClothingPtr))//','
      WRITE(OutputFileInits,fmta,advance='No') TRIM(GetScheduleName(People(Loop)%AirVelocityPtr))//','
      IF (People(Loop)%Fanger) THEN
        StringOut='Yes'
      ELSE
        StringOut='No'
      ENDIF
      WRITE(OutputFileInits,fmta,advance='No') TRIM(StringOut)//','
      IF (People(Loop)%Pierce) THEN
        StringOut='Yes'
      ELSE
        StringOut='No'
      ENDIF
      WRITE(OutputFileInits,fmta,advance='No') TRIM(StringOut)//','
      IF (People(Loop)%KSU) THEN
        StringOut='Yes'
      ELSE
        StringOut='No'
      ENDIF
      WRITE(OutputFileInits,fmta) TRIM(StringOut)
    ELSE
      WRITE(OutputFileInits,fmta) TRIM(StringOut)
    ENDIF
  ENDDO
  DO Loop=1,TotLights
    IF (Loop == 1) WRITE(OutputFileInits,723) 'Lights','Lighting Level {W},'//  &
       'Lights/Floor Area {W/m2},Lights per person {W/person},'//  &
       'Fraction Return Air,Fraction Radiant,Fraction Short Wave,Fraction Convected,Fraction Replaceable,End-Use Category,'//   &
       'Nominal Minimum Lighting Level {W},Nominal Maximum Lighting Level {W}'

    ZoneNum=Lights(Loop)%ZonePtr

    IF (ZoneNum == 0) THEN
      WRITE(OutputFileInits,724) 'Lights-Illegal Zone specified',TRIM(Lights(Loop)%Name)
      CYCLE
    ENDIF

    WRITE(OutputFileInits,722,advance='No') 'Lights',TRIM(Lights(Loop)%Name),  &
       TRIM(GetScheduleName(Lights(Loop)%SchedPtr)),  &
       TRIM(Zone(ZoneNum)%Name),TRIM(RoundSigDigits(Zone(ZoneNum)%FloorArea,2)),   &
       TRIM(RoundSigDigits(Zone(ZoneNum)%TotOccupants,1))

    WRITE(OutputFileInits,fmta,advance='No') TRIM(RoundSigDigits(Lights(Loop)%DesignLevel,3))//','
    IF (Zone(ZoneNum)%FloorArea > 0.0d0) THEN
      StringOut=RoundSigDigits(Lights(Loop)%DesignLevel/Zone(ZoneNum)%FloorArea,3)
    ELSE
      StringOut='N/A'
    ENDIF
    WRITE(OutputFileInits,fmta,advance='No') TRIM(StringOut)//','
    IF (Zone(ZoneNum)%TotOccupants > 0.0d0) THEN
      StringOut=RoundSigDigits(Lights(Loop)%DesignLevel/Zone(ZoneNum)%TotOccupants,3)
    ELSE
      StringOut='N/A'
    ENDIF
    WRITE(OutputFileInits,fmta,advance='No') TRIM(StringOut)//','
    StringOut=RoundSigDigits(Lights(Loop)%FractionReturnAir,3)
    WRITE(OutputFileInits,fmta,advance='No') TRIM(StringOut)//','
    StringOut=RoundSigDigits(Lights(Loop)%FractionRadiant,3)
    WRITE(OutputFileInits,fmta,advance='No') TRIM(StringOut)//','
    StringOut=RoundSigDigits(Lights(Loop)%FractionShortWave,3)
    WRITE(OutputFileInits,fmta,advance='No') TRIM(StringOut)//','
    StringOut=RoundSigDigits(Lights(Loop)%FractionConvected,3)
    WRITE(OutputFileInits,fmta,advance='No') TRIM(StringOut)//','
    StringOut=RoundSigDigits(Lights(Loop)%FractionReplaceable,3)
    WRITE(OutputFileInits,fmta,advance='No') TRIM(StringOut)//','
    WRITE(OutputFileInits,fmta,advance='No') TRIM(Lights(Loop)%EndUseSubcategory)//','
    StringOut=RoundSigDigits(Lights(Loop)%NomMinDesignLevel,3)
    WRITE(OutputFileInits,fmta,advance='No') TRIM(StringOut)//','
    StringOut=RoundSigDigits(Lights(Loop)%NomMaxDesignLevel,3)
    WRITE(OutputFileInits,fmta) TRIM(StringOut)
  ENDDO
  DO Loop=1,TotElecEquip
    IF (Loop == 1) WRITE(OutputFileInits,723) 'ElectricEquipment','Equipment Level {W},'//  &
       'Equipment/Floor Area {W/m2},Equipment per person {W/person},'//  &
       'Fraction Latent,Fraction Radiant,Fraction Lost,Fraction Convected,End-Use SubCategory,'//  &
       'Nominal Minimum Equipment Level {W},Nominal Maximum Equipment Level {W}'

    ZoneNum=ZoneElectric(Loop)%ZonePtr

    IF (ZoneNum == 0) THEN
      WRITE(OutputFileInits,724) 'Electric Equipment-Illegal Zone specified',TRIM(ZoneElectric(Loop)%Name)
      CYCLE
    ENDIF

    WRITE(OutputFileInits,722,advance='No') 'ElectricEquipment',TRIM(ZoneElectric(Loop)%Name),  &
       TRIM(GetScheduleName(ZoneElectric(Loop)%SchedPtr)),  &
       TRIM(Zone(ZoneNum)%Name),TRIM(RoundSigDigits(Zone(ZoneNum)%FloorArea,2)),   &
       TRIM(RoundSigDigits(Zone(ZoneNum)%TotOccupants,1))

    WRITE(OutputFileInits,fmta,advance='No') TRIM(RoundSigDigits(ZoneElectric(Loop)%DesignLevel,3))//','
    IF (Zone(ZoneNum)%FloorArea > 0.0d0) THEN
      StringOut=RoundSigDigits(ZoneElectric(Loop)%DesignLevel/Zone(ZoneNum)%FloorArea,3)
    ELSE
      StringOut='N/A'
    ENDIF
    WRITE(OutputFileInits,fmta,advance='No') TRIM(StringOut)//','
    IF (Zone(ZoneNum)%TotOccupants > 0.0d0) THEN
      StringOut=RoundSigDigits(ZoneElectric(Loop)%DesignLevel/Zone(ZoneNum)%TotOccupants,3)
    ELSE
      StringOut='N/A'
    ENDIF
    WRITE(OutputFileInits,fmta,advance='No') TRIM(StringOut)//','
    StringOut=RoundSigDigits(ZoneElectric(Loop)%FractionLatent,3)
    WRITE(OutputFileInits,fmta,advance='No') TRIM(StringOut)//','
    StringOut=RoundSigDigits(ZoneElectric(Loop)%FractionRadiant,3)
    WRITE(OutputFileInits,fmta,advance='No') TRIM(StringOut)//','
    StringOut=RoundSigDigits(ZoneElectric(Loop)%FractionLost,3)
    WRITE(OutputFileInits,fmta,advance='No') TRIM(StringOut)//','
    StringOut=RoundSigDigits(ZoneElectric(Loop)%FractionConvected,3)
    WRITE(OutputFileInits,fmta,advance='No') TRIM(StringOut)//','
    WRITE(OutputFileInits,fmta,advance='No') TRIM(ZoneElectric(Loop)%EndUseSubcategory)//','
    StringOut=RoundSigDigits(ZoneElectric(Loop)%NomMinDesignLevel,3)
    WRITE(OutputFileInits,fmta,advance='No') TRIM(StringOut)//','
    StringOut=RoundSigDigits(ZoneElectric(Loop)%NomMaxDesignLevel,3)
    WRITE(OutputFileInits,fmta) TRIM(StringOut)
  ENDDO
  DO Loop=1,TotGasEquip
    IF (Loop == 1) WRITE(OutputFileInits,723) 'GasEquipment','Equipment Level {W},'//  &
       'Equipment/Floor Area {W/m2},Equipment per person {W/person},'//  &
       'Fraction Latent,Fraction Radiant,Fraction Lost,Fraction Convected,End-Use SubCategory,'//  &
       'Nominal Minimum Equipment Level {W},Nominal Maximum Equipment Level {W}'

    ZoneNum=ZoneGas(Loop)%ZonePtr

    IF (ZoneNum == 0) THEN
      WRITE(OutputFileInits,724) 'Gas Equipment-Illegal Zone specified',TRIM(ZoneGas(Loop)%Name)
      CYCLE
    ENDIF

    WRITE(OutputFileInits,722,advance='No') 'GasEquipment',TRIM(ZoneGas(Loop)%Name),  &
       TRIM(GetScheduleName(ZoneGas(Loop)%SchedPtr)),  &
       TRIM(Zone(ZoneNum)%Name),TRIM(RoundSigDigits(Zone(ZoneNum)%FloorArea,2)),   &
       TRIM(RoundSigDigits(Zone(ZoneNum)%TotOccupants,1))

    WRITE(OutputFileInits,fmta,advance='No') TRIM(RoundSigDigits(ZoneGas(Loop)%DesignLevel,3))//','

    IF (Zone(ZoneNum)%FloorArea > 0.0d0) THEN
      StringOut=RoundSigDigits(ZoneGas(Loop)%DesignLevel/Zone(ZoneNum)%FloorArea,3)
    ELSE
      StringOut='N/A'
    ENDIF
    WRITE(OutputFileInits,fmta,advance='No') TRIM(StringOut)//','
    IF (Zone(ZoneNum)%TotOccupants > 0.0d0) THEN
      StringOut=RoundSigDigits(ZoneGas(Loop)%DesignLevel/Zone(ZoneNum)%TotOccupants,3)
    ELSE
      StringOut='N/A'
    ENDIF
    WRITE(OutputFileInits,fmta,advance='No') TRIM(StringOut)//','
    StringOut=RoundSigDigits(ZoneGas(Loop)%FractionLatent,3)
    WRITE(OutputFileInits,fmta,advance='No') TRIM(StringOut)//','
    StringOut=RoundSigDigits(ZoneGas(Loop)%FractionRadiant,3)
    WRITE(OutputFileInits,fmta,advance='No') TRIM(StringOut)//','
    StringOut=RoundSigDigits(ZoneGas(Loop)%FractionLost,3)
    WRITE(OutputFileInits,fmta,advance='No') TRIM(StringOut)//','
    StringOut=RoundSigDigits(ZoneGas(Loop)%FractionConvected,3)
    WRITE(OutputFileInits,fmta,advance='No') TRIM(StringOut)//','
    WRITE(OutputFileInits,fmta,advance='No') TRIM(ZoneGas(Loop)%EndUseSubcategory)//','
    StringOut=RoundSigDigits(ZoneGas(Loop)%NomMinDesignLevel,3)
    WRITE(OutputFileInits,fmta,advance='No') TRIM(StringOut)//','
    StringOut=RoundSigDigits(ZoneGas(Loop)%NomMaxDesignLevel,3)
    WRITE(OutputFileInits,fmta) TRIM(StringOut)
  ENDDO

  DO Loop=1,TotHWEquip
    IF (Loop == 1) WRITE(OutputFileInits,723) 'HotWaterEquipment','Equipment Level {W},'//  &
       'Equipment/Floor Area {W/m2},Equipment per person {W/person},'//  &
       'Fraction Latent,Fraction Radiant,Fraction Lost,Fraction Convected,End-Use SubCategory,'//  &
       'Nominal Minimum Equipment Level {W},Nominal Maximum Equipment Level {W}'

    ZoneNum=ZoneHWEq(Loop)%ZonePtr

    IF (ZoneNum == 0) THEN
      WRITE(OutputFileInits,724) 'Hot Water Equipment-Illegal Zone specified',TRIM(ZoneHWEq(Loop)%Name)
      CYCLE
    ENDIF

    WRITE(OutputFileInits,722,advance='No') 'HotWaterEquipment',TRIM(ZoneHWEq(Loop)%Name),  &
       TRIM(GetScheduleName(ZoneHWEq(Loop)%SchedPtr)),  &
       TRIM(Zone(ZoneNum)%Name),TRIM(RoundSigDigits(Zone(ZoneNum)%FloorArea,2)),   &
       TRIM(RoundSigDigits(Zone(ZoneNum)%TotOccupants,1))

    WRITE(OutputFileInits,fmta,advance='No') TRIM(RoundSigDigits(ZoneHWEq(Loop)%DesignLevel,3))//','

    IF (Zone(ZoneNum)%FloorArea > 0.0d0) THEN
      StringOut=RoundSigDigits(ZoneHWEq(Loop)%DesignLevel/Zone(ZoneNum)%FloorArea,3)
    ELSE
      StringOut='N/A'
    ENDIF
    WRITE(OutputFileInits,fmta,advance='No') TRIM(StringOut)//','
    IF (Zone(ZoneNum)%TotOccupants > 0.0d0) THEN
      StringOut=RoundSigDigits(ZoneHWEq(Loop)%DesignLevel/Zone(ZoneNum)%TotOccupants,3)
    ELSE
      StringOut='N/A'
    ENDIF
    WRITE(OutputFileInits,fmta,advance='No') TRIM(StringOut)//','
    StringOut=RoundSigDigits(ZoneHWEq(Loop)%FractionLatent,3)
    WRITE(OutputFileInits,fmta,advance='No') TRIM(StringOut)//','
    StringOut=RoundSigDigits(ZoneHWEq(Loop)%FractionRadiant,3)
    WRITE(OutputFileInits,fmta,advance='No') TRIM(StringOut)//','
    StringOut=RoundSigDigits(ZoneHWEq(Loop)%FractionLost,3)
    WRITE(OutputFileInits,fmta,advance='No') TRIM(StringOut)//','
    StringOut=RoundSigDigits(ZoneHWEq(Loop)%FractionConvected,3)
    WRITE(OutputFileInits,fmta,advance='No') TRIM(StringOut)//','
    WRITE(OutputFileInits,fmta,advance='No') TRIM(ZoneHWEq(Loop)%EndUseSubcategory)//','
    StringOut=RoundSigDigits(ZoneHWEq(Loop)%NomMinDesignLevel,3)
    WRITE(OutputFileInits,fmta,advance='No') TRIM(StringOut)//','
    StringOut=RoundSigDigits(ZoneHWEq(Loop)%NomMaxDesignLevel,3)
    WRITE(OutputFileInits,fmta) TRIM(StringOut)
  ENDDO

  DO Loop=1,TotStmEquip
    IF (Loop == 1) WRITE(OutputFileInits,723) 'SteamEquipment','Equipment Level {W},'//  &
       'Equipment/Floor Area {W/m2},Equipment per person {W/person},'//  &
       'Fraction Latent,Fraction Radiant,Fraction Lost,Fraction Convected,End-Use SubCategory,'//  &
       'Nominal Minimum Equipment Level {W},Nominal Maximum Equipment Level {W}'

    ZoneNum=ZoneSteamEq(Loop)%ZonePtr

    IF (ZoneNum == 0) THEN
      WRITE(OutputFileInits,724) 'Steam Equipment-Illegal Zone specified',TRIM(ZoneSteamEq(Loop)%Name)
      CYCLE
    ENDIF

    WRITE(OutputFileInits,722,advance='No') 'SteamEquipment',TRIM(ZoneSteamEq(Loop)%Name),  &
       TRIM(GetScheduleName(ZoneSteamEq(Loop)%SchedPtr)),  &
       TRIM(Zone(ZoneNum)%Name),TRIM(RoundSigDigits(Zone(ZoneNum)%FloorArea,2)),   &
       TRIM(RoundSigDigits(Zone(ZoneNum)%TotOccupants,1))

    WRITE(OutputFileInits,fmta,advance='No') TRIM(RoundSigDigits(ZoneSteamEq(Loop)%DesignLevel,3))//','

    IF (Zone(ZoneNum)%FloorArea > 0.0d0) THEN
      StringOut=RoundSigDigits(ZoneSteamEq(Loop)%DesignLevel/Zone(ZoneNum)%FloorArea,3)
    ELSE
      StringOut='N/A'
    ENDIF
    WRITE(OutputFileInits,fmta,advance='No') TRIM(StringOut)//','
    IF (Zone(ZoneNum)%TotOccupants > 0.0d0) THEN
      StringOut=RoundSigDigits(ZoneSteamEq(Loop)%DesignLevel/Zone(ZoneNum)%TotOccupants,3)
    ELSE
      StringOut='N/A'
    ENDIF
    WRITE(OutputFileInits,fmta,advance='No') TRIM(StringOut)//','
    StringOut=RoundSigDigits(ZoneSteamEq(Loop)%FractionLatent,3)
    WRITE(OutputFileInits,fmta,advance='No') TRIM(StringOut)//','
    StringOut=RoundSigDigits(ZoneSteamEq(Loop)%FractionRadiant,3)
    WRITE(OutputFileInits,fmta,advance='No') TRIM(StringOut)//','
    StringOut=RoundSigDigits(ZoneSteamEq(Loop)%FractionLost,3)
    WRITE(OutputFileInits,fmta,advance='No') TRIM(StringOut)//','
    StringOut=RoundSigDigits(ZoneSteamEq(Loop)%FractionConvected,3)
    WRITE(OutputFileInits,fmta,advance='No') TRIM(StringOut)//','
    WRITE(OutputFileInits,fmta,advance='No') TRIM(ZoneSteamEq(Loop)%EndUseSubcategory)//','
    StringOut=RoundSigDigits(ZoneSteamEq(Loop)%NomMinDesignLevel,3)
    WRITE(OutputFileInits,fmta,advance='No') TRIM(StringOut)//','
    StringOut=RoundSigDigits(ZoneSteamEq(Loop)%NomMaxDesignLevel,3)
    WRITE(OutputFileInits,fmta) TRIM(StringOut)
  ENDDO

  DO Loop=1,TotOthEquip
    IF (Loop == 1) WRITE(OutputFileInits,723) 'OtherEquipment','Equipment Level {W},'//  &
       'Equipment/Floor Area {W/m2},Equipment per person {W/person},'//  &
       'Fraction Latent,Fraction Radiant,Fraction Lost,Fraction Convected,'//  &
       'Nominal Minimum Equipment Level {W},Nominal Maximum Equipment Level {W}'
    ZoneNum=ZoneOtherEq(Loop)%ZonePtr

    IF (ZoneNum == 0) THEN
      WRITE(OutputFileInits,724) 'Other Equipment-Illegal Zone specified',TRIM(ZoneOtherEq(Loop)%Name)
      CYCLE
    ENDIF

    WRITE(OutputFileInits,722,advance='No') 'OtherEquipment',TRIM(ZoneOtherEq(Loop)%Name),  &
       TRIM(GetScheduleName(ZoneOtherEq(Loop)%SchedPtr)),  &
       TRIM(Zone(ZoneNum)%Name),TRIM(RoundSigDigits(Zone(ZoneNum)%FloorArea,2)),   &
       TRIM(RoundSigDigits(Zone(ZoneNum)%TotOccupants,1))

    WRITE(OutputFileInits,fmta,advance='No') TRIM(RoundSigDigits(ZoneOtherEq(Loop)%DesignLevel,3))//','

    IF (Zone(ZoneNum)%FloorArea > 0.0d0) THEN
      StringOut=RoundSigDigits(ZoneOtherEq(Loop)%DesignLevel/Zone(ZoneNum)%FloorArea,3)
    ELSE
      StringOut='N/A'
    ENDIF
    WRITE(OutputFileInits,fmta,advance='No') TRIM(StringOut)//','
    IF (Zone(ZoneNum)%TotOccupants > 0.0d0) THEN
      StringOut=RoundSigDigits(ZoneOtherEq(Loop)%DesignLevel/Zone(ZoneNum)%TotOccupants,3)
    ELSE
      StringOut='N/A'
    ENDIF
    WRITE(OutputFileInits,fmta,advance='No') TRIM(StringOut)//','
    StringOut=RoundSigDigits(ZoneOtherEq(Loop)%FractionLatent,3)
    WRITE(OutputFileInits,fmta,advance='No') TRIM(StringOut)//','
    StringOut=RoundSigDigits(ZoneOtherEq(Loop)%FractionRadiant,3)
    WRITE(OutputFileInits,fmta,advance='No') TRIM(StringOut)//','
    StringOut=RoundSigDigits(ZoneOtherEq(Loop)%FractionLost,3)
    WRITE(OutputFileInits,fmta,advance='No') TRIM(StringOut)//','
    StringOut=RoundSigDigits(ZoneOtherEq(Loop)%FractionConvected,3)
    WRITE(OutputFileInits,fmta,advance='No') TRIM(StringOut)//','
    StringOut=RoundSigDigits(ZoneOtherEq(Loop)%NomMinDesignLevel,3)
    WRITE(OutputFileInits,fmta,advance='No') TRIM(StringOut)//','
    StringOut=RoundSigDigits(ZoneOtherEq(Loop)%NomMaxDesignLevel,3)
    WRITE(OutputFileInits,fmta) TRIM(StringOut)
  ENDDO

  DO Loop=1,TotBBHeat
    IF (Loop == 1) WRITE(OutputFileInits,723) 'Outdoor Controlled Baseboard Heat','Capacity at Low Temperature {W},'//  &
       'Low Temperature {C},Capacity at High Temperature {W},High Temperature {C},'//  &
       'Fraction Radiant,Fraction Convected,End-Use Subcategory'

    ZoneNum=ZoneBBHeat(Loop)%ZonePtr

    IF (ZoneNum == 0) THEN
      WRITE(OutputFileInits,724) 'Outdoor Controlled Baseboard Heat-Illegal Zone specified',TRIM(ZoneBBHeat(Loop)%Name)
      CYCLE
    ENDIF

    WRITE(OutputFileInits,722,advance='No') 'Outdoor Controlled Baseboard Heat',TRIM(ZoneBBHeat(Loop)%Name),  &
       TRIM(GetScheduleName(ZoneBBHeat(Loop)%SchedPtr)),   &
       TRIM(Zone(ZoneNum)%Name),TRIM(RoundSigDigits(Zone(ZoneNum)%FloorArea,2)),   &
       TRIM(RoundSigDigits(Zone(ZoneNum)%TotOccupants,1))

    StringOut=RoundSigDigits(ZoneBBHeat(Loop)%CapatLowTemperature,3)
    WRITE(OutputFileInits,fmta,advance='No') TRIM(StringOut)//','
    StringOut=RoundSigDigits(ZoneBBHeat(Loop)%LowTemperature,3)
    WRITE(OutputFileInits,fmta,advance='No') TRIM(StringOut)//','
    StringOut=RoundSigDigits(ZoneBBHeat(Loop)%CapatHighTemperature,3)
    WRITE(OutputFileInits,fmta,advance='No') TRIM(StringOut)//','
    StringOut=RoundSigDigits(ZoneBBHeat(Loop)%HighTemperature,3)
    WRITE(OutputFileInits,fmta,advance='No') TRIM(StringOut)//','
    StringOut=RoundSigDigits(ZoneBBHeat(Loop)%FractionRadiant,3)
    WRITE(OutputFileInits,fmta,advance='No') TRIM(StringOut)//','
    StringOut=RoundSigDigits(ZoneBBHeat(Loop)%FractionConvected,3)
    WRITE(OutputFileInits,fmta,advance='No') TRIM(StringOut)//','
    WRITE(OutputFileInits,fmta) TRIM(ZoneBBHeat(Loop)%EndUseSubcategory)
  ENDDO

720 FORMAT(' Zone Internal Gains, ',A,',',A,',',A,',')
721 FORMAT('! <Zone Internal Gains/Equipment Information - Nominal>,Zone Name, Floor Area {m2},# Occupants,', &
           'Area per Occupant {m2/person},Occupant per Area {person/m2},Interior Lighting {W/m2},',  &
           'Electric Load {W/m2},Gas Load {W/m2},Other Load {W/m2},Hot Water Eq {W/m2},',  &
           'Steam Equipment {W/m2},Sum Loads per Area {W/m2},Outdoor Controlled Baseboard Heat')

722 FORMAT(' ',A,' Internal Gains, ',A,',',A,',',A,',',A,',',A,',')
723 FORMAT('! <',A,' Internal Gains - Nominal>,Name,Schedule Name,Zone Name,Zone Floor Area {m2},# Zone Occupants,',A)
724 FORMAT(' ',A,', ',A)

 RETURN

END SUBROUTINE GetInternalHeatGainsInput


SUBROUTINE InitInternalHeatGains

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   September 1997
          !       MODIFIED       November 1998, FW: add adjustment to elec lights for dayltg controls
          !                      August 2003, FCW: add optional calculation of light-to-return fraction
          !                       as a function of return plenum air temperature.
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine sets up the zone internal heat gains
          ! that are independent of the zone air temperature.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE ScheduleManager
  USE DataHeatBalFanSys, ONLY: MAT, SumConvHTRadSys, ZoneLatentGain
  USE DataDaylighting
  USE DataZoneEquipment, ONLY: ZoneEquipConfig
  USE ZonePlenum, ONLY: ZoneRetPlenCond
  USE Psychrometrics, ONLY: PsyRhoAirFnPbTdbW
  USE DataRoomAirModel, ONLY: IsZoneDV, TCMF, IsZoneUI
  USE WaterThermalTanks, ONLY: CalcWaterThermalTankZoneGains
  USE PipeHeatTransfer, ONLY: CalcZonePipesHeatGain
  USE WaterUse, ONLY: CalcWaterUseZoneGains
  USE FuelCellElectricGenerator, ONLY: FigureFuelCellZoneGains
  USE MicroCHPElectricGenerator, ONLY: FigureMicroCHPZoneGains
  USE ManageElectricPower,       ONLY: FigureInverterZoneGains, FigureElectricalStorageZoneGains, &
                                       FigureTransformerZoneGains
  USE DaylightingDevices,        ONLY: FigureTDDZoneGains
  USE RefrigeratedCase,          ONLY: FigureRefrigerationZoneGains
  USE OutputReportTabular, ONLY: radiantPulseUsed,radiantPulseTimestep,radiantPulseReceived
  USE DataGlobals, ONLY: CompLoadReportIsReq
  USE DataGlobalConstants, ONLY: endUseHeating,endUseCooling
  USE OutputReportTabular, ONLY: AllocateLoadComponentArrays
  USE DataSizing, ONLY: CurOverallSimDay

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
  REAL(r64), PARAMETER, dIMENSION(9) :: C=(/ 6.4611027d0, .946892d0, .0000255737d0, 7.139322d0, -.0627909d0,     &
            .0000589271d0, -.198550d0, .000940018d0, -.00000149532d0 /)

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64) :: ActivityLevel_WperPerson  ! Units on Activity Level (Schedule)
  REAL(r64) :: NumberOccupants       ! Number of occupants
  INTEGER :: SurfNum            ! DO loop counter for surfaces
  INTEGER :: Loop
  INTEGER :: NZ
  REAL(r64) :: Q !, QR
  REAL(r64) :: TotalPeopleGain    ! Total heat gain from people (intermediate calculational variable)
  REAL(r64) :: SensiblePeopleGain ! Sensible heat gain from people (intermediate calculational variable)
  REAL(r64) :: FractionConvected  ! For general lighting, fraction of heat from lights convected to zone air
  REAL(r64) :: FractionReturnAir  ! For general lighting, fraction of heat from lights convected to zone's return air
  REAL(r64) :: FractionRadiant    ! For general lighting, fraction of heat from lights to zone that is long wave
  INTEGER :: ReturnZonePlenumCondNum ! Number of ZoneRetPlenCond for a zone's return air plenum, if it exists
  REAL(r64) :: ReturnPlenumTemp   ! Air temperature of a zone's return air plenum (C)
  REAL(r64) :: pulseMultipler     ! use to create a pulse for the load component report computations
  REAL(r64) :: curQL = 0.0d0        ! radiant value prior to adjustment for pulse for load component report
  REAL(r64) :: adjQL = 0.0d0        ! radiant value including adjustment for pulse for load component report

!  REAL(r64), ALLOCATABLE, SAVE, DIMENSION(:) :: QSA

!  IF (.NOT. ALLOCATED(QSA)) ALLOCATE(QSA(NumOfZones))

                !  Zero out time step variables
  ZoneIntGain%NOFOCC = 0.d0
  ZoneIntGain%QOCTOT = 0.d0
  ZoneIntGain%QOCSEN = 0.d0
  ZoneIntGain%QOCLAT = 0.d0
  ZoneIntGain%QOCRAD = 0.d0
  ZoneIntGain%QOCCON = 0.d0
  ZoneIntGain%QLTSW  = 0.d0
  ZoneIntGain%QLTCRA = 0.d0
  ZoneIntGain%QLTRAD = 0.d0
  ZoneIntGain%QLTCON = 0.d0
  ZoneIntGain%QLTTOT = 0.d0

  ZoneIntGain%QEELAT = 0.d0
  ZoneIntGain%QEERAD = 0.d0
  ZoneIntGain%QEECON = 0.d0
  ZoneIntGain%QEELost = 0.d0
  ZoneIntGain%QGELAT = 0.d0
  ZoneIntGain%QGERAD = 0.d0
  ZoneIntGain%QGECON = 0.d0
  ZoneIntGain%QGELost= 0.d0
  ZoneIntGain%QBBRAD = 0.d0
  ZoneIntGain%QBBCON = 0.d0
  ZoneIntGain%QOELAT = 0.d0
  ZoneIntGain%QOERAD = 0.d0
  ZoneIntGain%QOECON = 0.d0
  ZoneIntGain%QOELost= 0.d0
  ZoneIntGain%QHWLAT = 0.d0
  ZoneIntGain%QHWRAD = 0.d0
  ZoneIntGain%QHWCON = 0.d0
  ZoneIntGain%QHWLost= 0.d0
  ZoneIntGain%QSELAT = 0.d0
  ZoneIntGain%QSERAD = 0.d0
  ZoneIntGain%QSECON = 0.d0
  ZoneIntGain%QSELost= 0.d0

  DO Loop = 0, 25
    ZoneIntEEuse%EEConvected(Loop) = 0.0d0
    ZoneIntEEuse%EERadiated(Loop) = 0.0d0
    ZoneIntEEuse%EELost(Loop) = 0.0d0
    ZoneIntEEuse%EELatent(Loop) = 0.0d0
  ENDDO

  ZnRpt%LtsPower = 0.0d0
  ZnRpt%ElecPower = 0.0d0
  ZnRpt%GasPower = 0.0d0
  ZnRpt%HWPower = 0.0d0
  ZnRpt%SteamPower = 0.0d0
  ZnRpt%BaseHeatPower = 0.0d0

!  QSA = 0.0

          ! Process Internal Heat Gains, People done below
          ! Occupant Stuff
          !   METHOD:
          !       The function is based on a curve fit to data presented in
          !       Table 48 'Heat Gain From People' of Chapter 1 of the 'Carrier
          !       Handbook of Air Conditioning System Design', 1965.  Values of
          !       Sensible gain were obtained from the table at average adjusted
          !       metabolic rates 350, 400, 450, 500, 750, 850, 1000, and
          !       1450 Btu/hr each at temperatures 82, 80, 78, 75, and 70F.
          !       Sensible gains of 0.0 at 96F and equal to the metabolic rate
          !       at 30F were assumed in order to give reasonable values beyond
          !       The reported temperature range.
  DO Loop = 1, TotPeople
    NZ = People(Loop)%ZonePtr
    NumberOccupants = People(Loop)%NumberOfPeople * GetCurrentScheduleValue(People(Loop)%NumberOfPeoplePtr)
    If (People(Loop)%EMSPeopleOn) NumberOccupants = People(Loop)%EMSNumberOfPeople

    TotalPeopleGain =  0.0d0
    SensiblePeopleGain = 0.0d0

    IF (NumberOccupants > 0.0d0) THEN
      ActivityLevel_WperPerson = GetCurrentScheduleValue(People(Loop)%ActivityLevelPtr)
      TotalPeopleGain = NumberOccupants*ActivityLevel_WperPerson
      ! if the user did not specify a sensible fraction, calculate the sensible heat gain
      IF (People(Loop)%UserSpecSensFrac == AutoCalculate ) THEN
        IF ( .not. (IsZoneDV(NZ) .or. IsZoneUI(NZ)) ) THEN
          SensiblePeopleGain = NumberOccupants*( C(1)+ActivityLevel_WperPerson*(C(2)+ActivityLevel_WperPerson*C(3))            &
                                                +MAT(NZ)*((C(4)+ActivityLevel_WperPerson*(C(5)+ActivityLevel_WperPerson*C(6))) &
                                                +MAT(NZ)*( C(7)+ActivityLevel_WperPerson*(C(8)+ActivityLevel_WperPerson*C(9)))) )
        ELSE ! UCSD - DV or UI
          SensiblePeopleGain = NumberOccupants*( C(1)+ActivityLevel_WperPerson*(C(2)+ActivityLevel_WperPerson*C(3))            &
                                                +TCMF(NZ)*((C(4)+ActivityLevel_WperPerson*(C(5)+ActivityLevel_WperPerson*C(6))) &
                                                +TCMF(NZ)*( C(7)+ActivityLevel_WperPerson*(C(8)+ActivityLevel_WperPerson*C(9)))) )
        END IF
      ELSE ! if the user did specify a sensible fraction, use it
        SensiblePeopleGain = TotalPeopleGain * People(Loop)%UserSpecSensFrac
      END IF

      IF (SensiblePeopleGain > TotalPeopleGain) SensiblePeopleGain = TotalPeopleGain
      IF (SensiblePeopleGain < 0.0d0) SensiblePeopleGain = 0.0d0

      !For predefined tabular reports related to outside air ventilation
      ZonePreDefRep(NZ)%isOccupied = .true. !set flag to occupied to be used in tabular reporting for ventilation
      ZonePreDefRep(NZ)%NumOccAccum = ZonePreDefRep(NZ)%NumOccAccum + NumberOccupants * TimeStepZone
      ZonePreDefRep(NZ)%NumOccAccumTime = ZonePreDefRep(NZ)%NumOccAccumTime + TimeStepZone
    ELSE
      ZonePreDefRep(NZ)%isOccupied = .false. !set flag to occupied to be used in tabular reporting for ventilation
    END IF

    People(Loop)%NumOcc = NumberOccupants
    People(Loop)%RadGainRate = SensiblePeopleGain * People(Loop)%FractionRadiant
    People(Loop)%ConGainRate = SensiblePeopleGain * People(Loop)%FractionConvected
    People(Loop)%SenGainRate = SensiblePeopleGain
    People(Loop)%LatGainRate = TotalPeopleGain - SensiblePeopleGain
    People(Loop)%TotGainRate = TotalPeopleGain
    People(Loop)%CO2GainRate = TotalPeopleGain * People(Loop)%CO2RateFactor

    ZoneIntGain(NZ)%NOFOCC = ZoneIntGain(NZ)%NOFOCC + People(Loop)%NumOcc
    ZoneIntGain(NZ)%QOCRAD = ZoneIntGain(NZ)%QOCRAD + People(Loop)%RadGainRate
    ZoneIntGain(NZ)%QOCCON = ZoneIntGain(NZ)%QOCCON + People(Loop)%ConGainRate
    ZoneIntGain(NZ)%QOCSEN = ZoneIntGain(NZ)%QOCSEN + People(Loop)%SenGainRate
    ZoneIntGain(NZ)%QOCLAT = ZoneIntGain(NZ)%QOCLAT + People(Loop)%LatGainRate
    ZoneIntGain(NZ)%QOCTOT = ZoneIntGain(NZ)%QOCTOT + People(Loop)%TotGainRate
  END DO

  DO Loop = 1, TotLights
    NZ = Lights(Loop)%ZonePtr
    Q = Lights(Loop)%DesignLevel * GetCurrentScheduleValue(Lights(Loop)%SchedPtr)

    IF (ZoneDaylight(NZ)%DaylightType == DetailedDaylighting &
      .OR. ZoneDaylight(NZ)%DaylightType == DElightDaylighting) THEN

      IF (Lights(Loop)%FractionReplaceable > 0.0d0) THEN  ! FractionReplaceable can only be 0 or 1 for these models
        Q = Q * ZoneDaylight(NZ)%ZonePowerReductionFactor
      END IF
    END IF

    ! Reduce lighting power due to demand limiting
    IF (Lights(Loop)%ManageDemand .AND. (Q > Lights(Loop)%DemandLimit)) Q = Lights(Loop)%DemandLimit

    ! Set Q to EMS override if being called for by EMs
    IF (Lights(Loop)%EMSLightsOn) Q = Lights(Loop)%EMSLightingPower

    FractionConvected = Lights(Loop)%FractionConvected
    FractionReturnAir = Lights(Loop)%FractionReturnAir
    FractionRadiant   = Lights(Loop)%FractionRadiant
    IF(Lights(Loop)%FractionReturnAirIsCalculated .AND. .NOT.ZoneSizingCalc .AND. SimTimeSteps > 1) THEN
      ! Calculate FractionReturnAir based on conditions in the zone's return air plenum, if there is one.
      IF(Zone(NZ)%IsControlled) THEN
        ReturnZonePlenumCondNum = ZoneEquipConfig(NZ)%ReturnZonePlenumCondNum
        IF(ReturnZonePlenumCondNum > 0) THEN
          ReturnPlenumTemp  = ZoneRetPlenCond(ReturnZonePlenumCondNum)%ZoneTemp
          FractionReturnAir = Lights(Loop)%FractionReturnAirPlenTempCoeff1 - &
                              Lights(Loop)%FractionReturnAirPlenTempCoeff2 * ReturnPlenumTemp
          FractionReturnAir = MAX(0.0d0,MIN(1.0d0,FractionReturnAir))
          IF(FractionReturnAir >= (1.0d0 - Lights(Loop)%FractionShortWave)) THEN
            FractionReturnAir = 1.0d0 - Lights(Loop)%FractionShortWave
            FractionRadiant   = 0.0d0
            FractionConvected = 0.0d0
          ELSE
            FractionRadiant   = ((1.0d0 - FractionReturnAir - Lights(Loop)%FractionShortWave)/ &
              (Lights(Loop)%FractionRadiant + Lights(Loop)%FractionConvected))* Lights(Loop)%FractionRadiant
            FractionConvected = 1.0d0 - (FractionReturnAir + FractionRadiant + Lights(Loop)%FractionShortWave)
          END IF
        END IF
      END IF
    END IF

    Lights(Loop)%Power = Q
    Lights(Loop)%RadGainRate = Q * FractionRadiant
    Lights(Loop)%VisGainRate = Q * Lights(Loop)%FractionShortWave
    Lights(Loop)%ConGainRate = Q * FractionConvected
    Lights(Loop)%RetAirGainRate = Q * FractionReturnAir
    Lights(Loop)%TotGainRate = Q

    ZnRpt(NZ)%LtsPower = ZnRpt(NZ)%LtsPower + Lights(Loop)%Power
    ZoneIntGain(NZ)%QLTRAD = ZoneIntGain(NZ)%QLTRAD + Lights(Loop)%RadGainRate
    ZoneIntGain(NZ)%QLTSW = ZoneIntGain(NZ)%QLTSW + Lights(Loop)%VisGainRate
    ZoneIntGain(NZ)%QLTCON = ZoneIntGain(NZ)%QLTCON + Lights(Loop)%ConGainRate
    ZoneIntGain(NZ)%QLTCRA = ZoneIntGain(NZ)%QLTCRA + Lights(Loop)%RetAirGainRate
    ZoneIntGain(NZ)%QLTTOT = ZoneIntGain(NZ)%QLTTOT + Lights(Loop)%TotGainRate
  END DO

  DO Loop = 1, TotElecEquip
    Q = ZoneElectric(Loop)%DesignLevel * GetCurrentScheduleValue(ZoneElectric(Loop)%SchedPtr)

    ! Reduce equipment power due to demand limiting
    IF (ZoneElectric(Loop)%ManageDemand .AND. (Q > ZoneElectric(Loop)%DemandLimit)) Q = ZoneElectric(Loop)%DemandLimit

    ! Set Q to EMS override if being called for by EMs
    IF (ZoneElectric(Loop)%EMSZoneEquipOverrideOn) Q = ZoneElectric(Loop)%EMSEquipPower

    ZoneElectric(Loop)%Power = Q
    ZoneElectric(Loop)%RadGainRate = Q * ZoneElectric(Loop)%FractionRadiant
    ZoneElectric(Loop)%ConGainRate = Q * ZoneElectric(Loop)%FractionConvected
    ZoneElectric(Loop)%LatGainRate = Q * ZoneElectric(Loop)%FractionLatent
    ZoneElectric(Loop)%LostRate = Q * ZoneElectric(Loop)%FractionLost
    ZoneElectric(Loop)%TotGainRate = Q - ZoneElectric(Loop)%LostRate

    NZ = ZoneElectric(Loop)%ZonePtr
    ZnRpt(NZ)%ElecPower = ZnRpt(NZ)%ElecPower + ZoneElectric(Loop)%Power
    ZoneIntGain(NZ)%QEERAD = ZoneIntGain(NZ)%QEERAD + ZoneElectric(Loop)%RadGainRate
    ZoneIntGain(NZ)%QEECON = ZoneIntGain(NZ)%QEECON + ZoneElectric(Loop)%ConGainRate
    ZoneIntGain(NZ)%QEELAT = ZoneIntGain(NZ)%QEELAT + ZoneElectric(Loop)%LatGainRate
    ZoneIntGain(NZ)%QEELost = ZoneIntGain(NZ)%QEELost + ZoneElectric(Loop)%LostRate
  END DO

  DO Loop = 1, TotGasEquip
    Q = ZoneGas(Loop)%DesignLevel * GetCurrentScheduleValue(ZoneGas(Loop)%SchedPtr)

    ! Set Q to EMS override if being called for by EMs
    IF (ZoneGas(Loop)%EMSZoneEquipOverrideOn) Q = ZoneGas(Loop)%EMSEquipPower

    ZoneGas(Loop)%Power = Q
    ZoneGas(Loop)%RadGainRate = Q * ZoneGas(Loop)%FractionRadiant
    ZoneGas(Loop)%ConGainRate = Q * ZoneGas(Loop)%FractionConvected
    ZoneGas(Loop)%LatGainRate = Q * ZoneGas(Loop)%FractionLatent
    ZoneGas(Loop)%LostRate    = Q * ZoneGas(Loop)%FractionLost
    ZoneGas(Loop)%TotGainRate = Q - ZoneGas(Loop)%LostRate
    ZoneGas(Loop)%CO2GainRate = Q * ZoneGas(Loop)%CO2RateFactor

    NZ = ZoneGas(Loop)%ZonePtr
    ZnRpt(NZ)%GasPower = ZnRpt(NZ)%GasPower + ZoneGas(Loop)%Power
    ZoneIntGain(NZ)%QGERAD=ZoneIntGain(NZ)%QGERAD + ZoneGas(Loop)%RadGainRate
    ZoneIntGain(NZ)%QGECON=ZoneIntGain(NZ)%QGECON + ZoneGas(Loop)%ConGainRate
    ZoneIntGain(NZ)%QGELAT=ZoneIntGain(NZ)%QGELAT + ZoneGas(Loop)%LatGainRate
    ZoneIntGain(NZ)%QGELost=ZoneIntGain(NZ)%QGELost + ZoneGas(Loop)%LostRate
  END DO

  DO Loop = 1, TotOthEquip
    Q = ZoneOtherEq(Loop)%DesignLevel * GetCurrentScheduleValue(ZoneOtherEq(Loop)%SchedPtr)

    ! Set Q to EMS override if being called for by EMs
    IF (ZoneOtherEq(Loop)%EMSZoneEquipOverrideOn) Q = ZoneOtherEq(Loop)%EMSEquipPower

    ZoneOtherEq(Loop)%Power = Q
    ZoneOtherEq(Loop)%RadGainRate = Q * ZoneOtherEq(Loop)%FractionRadiant
    ZoneOtherEq(Loop)%ConGainRate = Q * ZoneOtherEq(Loop)%FractionConvected
    ZoneOtherEq(Loop)%LatGainRate = Q * ZoneOtherEq(Loop)%FractionLatent
    ZoneOtherEq(Loop)%LostRate = Q * ZoneOtherEq(Loop)%FractionLost
    ZoneOtherEq(Loop)%TotGainRate = Q - ZoneOtherEq(Loop)%LostRate

    NZ = ZoneOtherEq(Loop)%ZonePtr
    ZoneIntGain(NZ)%QOERAD = ZoneIntGain(NZ)%QOERAD + ZoneOtherEq(Loop)%RadGainRate
    ZoneIntGain(NZ)%QOECON = ZoneIntGain(NZ)%QOECON + ZoneOtherEq(Loop)%ConGainRate
    ZoneIntGain(NZ)%QOELAT = ZoneIntGain(NZ)%QOELAT + ZoneOtherEq(Loop)%LatGainRate
    ZoneIntGain(NZ)%QOELost = ZoneIntGain(NZ)%QOELost + ZoneOtherEq(Loop)%LostRate
  END DO

  DO Loop = 1, TotHWEquip
    Q = ZoneHWEq(Loop)%DesignLevel * GetCurrentScheduleValue(ZoneHWEq(Loop)%SchedPtr)

    ! Set Q to EMS override if being called for by EMs
    IF (ZoneHWEq(Loop)%EMSZoneEquipOverrideOn) Q = ZoneHWEq(Loop)%EMSEquipPower

    ZoneHWEq(Loop)%Power = Q
    ZoneHWEq(Loop)%RadGainRate = Q * ZoneHWEq(Loop)%FractionRadiant
    ZoneHWEq(Loop)%ConGainRate = Q * ZoneHWEq(Loop)%FractionConvected
    ZoneHWEq(Loop)%LatGainRate = Q * ZoneHWEq(Loop)%FractionLatent
    ZoneHWEq(Loop)%LostRate = Q * ZoneHWEq(Loop)%FractionLost
    ZoneHWEq(Loop)%TotGainRate = Q - ZoneHWEq(Loop)%LostRate

    NZ = ZoneHWEq(Loop)%ZonePtr
    ZnRpt(NZ)%HWPower = ZnRpt(NZ)%HWPower + ZoneHWEq(Loop)%Power
    ZoneIntGain(NZ)%QHWRAD = ZoneIntGain(NZ)%QHWRAD + ZoneHWEq(Loop)%RadGainRate
    ZoneIntGain(NZ)%QHWCON = ZoneIntGain(NZ)%QHWCON + ZoneHWEq(Loop)%ConGainRate
    ZoneIntGain(NZ)%QHWLAT = ZoneIntGain(NZ)%QHWLAT + ZoneHWEq(Loop)%LatGainRate
    ZoneIntGain(NZ)%QHWLost = ZoneIntGain(NZ)%QHWLost + ZoneHWEq(Loop)%LostRate
  END DO

  DO Loop = 1, TotStmEquip
    Q = ZoneSteamEq(Loop)%DesignLevel * GetCurrentScheduleValue(ZoneSteamEq(Loop)%SchedPtr)

    ! Set Q to EMS override if being called for by EMs
    IF (ZoneSteamEq(Loop)%EMSZoneEquipOverrideOn) Q = ZoneSteamEq(Loop)%EMSEquipPower

    ZoneSteamEq(Loop)%Power = Q
    ZoneSteamEq(Loop)%RadGainRate = Q * ZoneSteamEq(Loop)%FractionRadiant
    ZoneSteamEq(Loop)%ConGainRate = Q * ZoneSteamEq(Loop)%FractionConvected
    ZoneSteamEq(Loop)%LatGainRate = Q * ZoneSteamEq(Loop)%FractionLatent
    ZoneSteamEq(Loop)%LostRate = Q * ZoneSteamEq(Loop)%FractionLost
    ZoneSteamEq(Loop)%TotGainRate = Q - ZoneSteamEq(Loop)%LostRate

    NZ = ZoneSteamEq(Loop)%ZonePtr
    ZnRpt(NZ)%SteamPower = ZnRpt(NZ)%SteamPower + ZoneSteamEq(Loop)%Power
    ZoneIntGain(NZ)%QSERAD = ZoneIntGain(NZ)%QSERAD + ZoneSteamEq(Loop)%RadGainRate
    ZoneIntGain(NZ)%QSECON = ZoneIntGain(NZ)%QSECON + ZoneSteamEq(Loop)%ConGainRate
    ZoneIntGain(NZ)%QSELAT = ZoneIntGain(NZ)%QSELAT + ZoneSteamEq(Loop)%LatGainRate
    ZoneIntGain(NZ)%QSELost = ZoneIntGain(NZ)%QSELost + ZoneSteamEq(Loop)%LostRate
  END DO

  DO Loop = 1, TotBBHeat
    NZ = ZoneBBHeat(Loop)%ZonePtr
    IF (Zone(NZ)%OutDryBulbTemp >= ZoneBBHeat(Loop)%HighTemperature) THEN
      Q = 0.0d0
    ELSE IF (Zone(NZ)%OutDryBulbTemp > ZoneBBHeat(Loop)%LowTemperature) THEN
      Q = (Zone(NZ)%OutDryBulbTemp - ZoneBBHeat(Loop)%LowTemperature) &
        * (ZoneBBHeat(Loop)%CapatHighTemperature - ZoneBBHeat(Loop)%CapatLowTemperature) &
        / (ZoneBBHeat(Loop)%HighTemperature - ZoneBBHeat(Loop)%LowTemperature) &
        + ZoneBBHeat(Loop)%CapatLowTemperature
    ELSE
      Q = ZoneBBHeat(Loop)%CapatLowTemperature
    END IF
    Q = Q * GetCurrentScheduleValue(ZoneBBHeat(Loop)%SchedPtr)

    ! set with EMS value if being called for.
    IF (ZoneBBHeat(Loop)%EMSZoneBaseboardOverrideOn) Q = ZoneBBHeat(Loop)%EMSZoneBaseboardPower

    ZoneBBHeat(Loop)%Power = Q
    ZoneBBHeat(Loop)%RadGainRate = Q * ZoneBBHeat(Loop)%FractionRadiant
    ZoneBBHeat(Loop)%ConGainRate = Q * ZoneBBHeat(Loop)%FractionConvected
    ZoneBBHeat(Loop)%TotGainRate = Q

    NZ = ZoneBBHeat(Loop)%ZonePtr
    ZnRpt(NZ)%BaseHeatPower = ZnRpt(NZ)%BaseHeatPower + ZoneBBHeat(Loop)%Power
    ZoneIntGain(NZ)%QBBRAD = ZoneIntGain(NZ)%QBBRAD + ZoneBBHeat(Loop)%RadGainRate
    ZoneIntGain(NZ)%QBBCON = ZoneIntGain(NZ)%QBBCON + ZoneBBHeat(Loop)%ConGainRate
  END DO

  DO Loop=1, TotCO2Gen
    NZ = ZoneCO2Gen(Loop)%ZonePtr
    ZoneCO2Gen(Loop)%CO2GainRate = ZoneCO2Gen(Loop)%CO2DesignRate * GetCurrentScheduleValue(ZoneCO2Gen(Loop)%SchedPtr)
    ZnRpt(NZ)%CO2Rate = ZnRpt(NZ)%CO2Rate + ZoneCO2Gen(Loop)%CO2GainRate
  ENDDO

  CALL CalcWaterThermalTankZoneGains
  CALL CalcZonePipesHeatGain
  CALL CalcWaterUseZoneGains
  CALL FigureFuelCellZoneGains
  CALL FigureMicroCHPZoneGains
  CALL FigureInverterZoneGains
  CALL FigureElectricalStorageZoneGains
  CALL FigureTransformerZoneGains
  CALL FigureTDDZoneGains
  CALL FigureRefrigerationZoneGains

! store pointer values to hold generic internal gain values constant for entire timestep
  CALL UpdateInternalGainValues

  DO NZ = 1, NumOfZones
    CALL SumAllInternalRadiationGains(NZ, QL(NZ))

    CALL SumAllInternalLatentGains(NZ, ZoneLatentGain(NZ))

  END DO

  SumConvHTRadSys = 0.0d0

  pulseMultipler = 0.01d0   ! the W/sqft pulse for the zone
  IF (CompLoadReportIsReq) THEN
    CALL AllocateLoadComponentArrays
  END IF
  DO SurfNum = 1, TotSurfaces
    NZ = Surface(SurfNum)%Zone
    IF (.NOT. Surface(SurfNum)%HeatTransSurf .OR. NZ == 0) CYCLE ! Skip non-heat transfer surfaces
    IF (.NOT. doLoadComponentPulseNow) THEN
      QRadThermInAbs(SurfNum) = QL(NZ) * TMULT(NZ) * ITABSF(SurfNum)
    ELSE
      curQL = QL(NZ)
      ! for the loads component report during the special sizing run increase the radiant portion
      ! a small amount to create a "pulse" of heat that is used for the
      adjQL = curQL + Zone(NZ)%FloorArea * pulseMultipler
      ! ITABSF is the Inside Thermal Absorptance
      ! TMULT is a mulipliter for each zone
      ! QRadThermInAbs is the thermal radiation absorbed on inside surfaces
      QRadThermInAbs(SurfNum) = adjQL * TMULT(NZ) * ITABSF(SurfNum)
      ! store the magnitude and time of the pulse
      radiantPulseUsed(NZ,CurOverallSimDay) = adjQL - curQL
      radiantPulseTimestep(NZ,CurOverallSimDay) = (HourOfDay-1)*NumOfTimeStepInHour + TimeStep
      radiantPulseReceived(SurfNum,CurOverallSimDay) = (adjQL - curQL) * TMULT(NZ) * ITABSF(SurfNum) * Surface(SurfNum)%area
    END IF
  END DO

  RETURN

END SUBROUTINE InitInternalHeatGains

SUBROUTINE ReportInternalHeatGains

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Liesen
          !       DATE WRITTEN   June 1997
          !       MODIFIED       July 1997 RKS
          !       RE-ENGINEERED  December 1998 LKL

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine currently creates the values for standard "zone loads" reporting
          ! from the heat balance module.

          ! METHODOLOGY EMPLOYED:
          ! The reporting methodology is described in the OutputDataStructure.doc
          ! as the "modified modular" format.

          ! REFERENCES:
          ! OutputDataStructure.doc (EnergyPlus documentation)

          ! USE STATEMENTS:
  USE DataGlobals, ONLY: SecInHour
  USE OutputReportTabular, ONLY: WriteTabularFiles
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
  INTEGER :: Loop
  INTEGER :: ZoneLoop                ! Counter for the # of zones (nz)
  INTEGER, DIMENSION(7) :: TradIntGainTypes = (/IntGainTypeOf_People , &
                                                IntGainTypeOf_Lights ,&
                                                IntGainTypeOf_ElectricEquipment  , &
                                                IntGainTypeOf_GasEquipment  , &
                                                IntGainTypeOf_HotWaterEquipment   , &
                                                IntGainTypeOf_SteamEquipment   , &
                                                IntGainTypeOf_OtherEquipment   /)


          ! FLOW:
  DO Loop = 1, TotPeople
    People(Loop)%RadGainEnergy = People(Loop)%RadGainRate * TimeStepZone * SecInHour
    People(Loop)%ConGainEnergy = People(Loop)%ConGainRate * TimeStepZone * SecInHour
    People(Loop)%SenGainEnergy = People(Loop)%SenGainRate * TimeStepZone * SecInHour
    People(Loop)%LatGainEnergy = People(Loop)%LatGainRate * TimeStepZone * SecInHour
    People(Loop)%TotGainEnergy = People(Loop)%TotGainRate * TimeStepZone * SecInHour
  END DO

  DO Loop = 1, TotLights
    Lights(Loop)%Consumption = Lights(Loop)%Power * TimeStepZone * SecInHour
    Lights(Loop)%RadGainEnergy = Lights(Loop)%RadGainRate * TimeStepZone * SecInHour
    Lights(Loop)%VisGainEnergy = Lights(Loop)%VisGainRate * TimeStepZone * SecInHour
    Lights(Loop)%ConGainEnergy = Lights(Loop)%ConGainRate * TimeStepZone * SecInHour
    Lights(Loop)%RetAirGainEnergy = Lights(Loop)%RetAirGainRate * TimeStepZone * SecInHour
    Lights(Loop)%TotGainEnergy = Lights(Loop)%TotGainRate * TimeStepZone * SecInHour
    IF (.NOT. WarmUpFlag) THEN
      IF (DoOutputReporting .AND.  WriteTabularFiles .and. (KindOfSim == ksRunPeriodWeather)) THEN !for weather simulations only
        !for tabular report, accumlate the total electricity used for each Light object
        Lights(Loop)%SumConsumption = Lights(Loop)%SumConsumption + Lights(Loop)%Consumption
        !for tabular report, accumulate the time when each Light has consumption (using a very small threshold instead of zero)
        IF (Lights(Loop)%Power > 0.01d0 * Lights(Loop)%DesignLevel) THEN
          Lights(Loop)%SumTimeNotZeroCons = Lights(Loop)%SumTimeNotZeroCons + TimeStepZone
        END IF
      ENDIF
    END IF
  END DO

  DO Loop = 1, TotElecEquip
    ZoneElectric(Loop)%Consumption = ZoneElectric(Loop)%Power * TimeStepZone * SecInHour
    ZoneElectric(Loop)%RadGainEnergy = ZoneElectric(Loop)%RadGainRate * TimeStepZone * SecInHour
    ZoneElectric(Loop)%ConGainEnergy = ZoneElectric(Loop)%ConGainRate * TimeStepZone * SecInHour
    ZoneElectric(Loop)%LatGainEnergy = ZoneElectric(Loop)%LatGainRate * TimeStepZone * SecInHour
    ZoneElectric(Loop)%LostEnergy = ZoneElectric(Loop)%LostRate * TimeStepZone * SecInHour
    ZoneElectric(Loop)%TotGainEnergy = ZoneElectric(Loop)%TotGainRate * TimeStepZone * SecInHour
  END DO

  DO Loop = 1, TotGasEquip
    ZoneGas(Loop)%Consumption = ZoneGas(Loop)%Power * TimeStepZone * SecInHour
    ZoneGas(Loop)%RadGainEnergy = ZoneGas(Loop)%RadGainRate * TimeStepZone * SecInHour
    ZoneGas(Loop)%ConGainEnergy = ZoneGas(Loop)%ConGainRate * TimeStepZone * SecInHour
    ZoneGas(Loop)%LatGainEnergy = ZoneGas(Loop)%LatGainRate * TimeStepZone * SecInHour
    ZoneGas(Loop)%LostEnergy = ZoneGas(Loop)%LostRate * TimeStepZone * SecInHour
    ZoneGas(Loop)%TotGainEnergy = ZoneGas(Loop)%TotGainRate * TimeStepZone * SecInHour
  END DO

  DO Loop = 1, TotOthEquip
    ZoneOtherEq(Loop)%Consumption = ZoneOtherEq(Loop)%Power * TimeStepZone * SecInHour
    ZoneOtherEq(Loop)%RadGainEnergy = ZoneOtherEq(Loop)%RadGainRate * TimeStepZone * SecInHour
    ZoneOtherEq(Loop)%ConGainEnergy = ZoneOtherEq(Loop)%ConGainRate * TimeStepZone * SecInHour
    ZoneOtherEq(Loop)%LatGainEnergy = ZoneOtherEq(Loop)%LatGainRate * TimeStepZone * SecInHour
    ZoneOtherEq(Loop)%LostEnergy = ZoneOtherEq(Loop)%LostRate * TimeStepZone * SecInHour
    ZoneOtherEq(Loop)%TotGainEnergy = ZoneOtherEq(Loop)%TotGainRate * TimeStepZone * SecInHour
  END DO

  DO Loop = 1, TotHWEquip
    ZoneHWEq(Loop)%Consumption = ZoneHWEq(Loop)%Power * TimeStepZone * SecInHour
    ZoneHWEq(Loop)%RadGainEnergy = ZoneHWEq(Loop)%RadGainRate * TimeStepZone * SecInHour
    ZoneHWEq(Loop)%ConGainEnergy = ZoneHWEq(Loop)%ConGainRate * TimeStepZone * SecInHour
    ZoneHWEq(Loop)%LatGainEnergy = ZoneHWEq(Loop)%LatGainRate * TimeStepZone * SecInHour
    ZoneHWEq(Loop)%LostEnergy = ZoneHWEq(Loop)%LostRate * TimeStepZone * SecInHour
    ZoneHWEq(Loop)%TotGainEnergy = ZoneHWEq(Loop)%TotGainRate * TimeStepZone * SecInHour
  END DO

  DO Loop = 1, TotStmEquip
    ZoneSteamEq(Loop)%Consumption = ZoneSteamEq(Loop)%Power * TimeStepZone * SecInHour
    ZoneSteamEq(Loop)%RadGainEnergy = ZoneSteamEq(Loop)%RadGainRate * TimeStepZone * SecInHour
    ZoneSteamEq(Loop)%ConGainEnergy = ZoneSteamEq(Loop)%ConGainRate * TimeStepZone * SecInHour
    ZoneSteamEq(Loop)%LatGainEnergy = ZoneSteamEq(Loop)%LatGainRate * TimeStepZone * SecInHour
    ZoneSteamEq(Loop)%LostEnergy = ZoneSteamEq(Loop)%LostRate * TimeStepZone * SecInHour
    ZoneSteamEq(Loop)%TotGainEnergy = ZoneSteamEq(Loop)%TotGainRate * TimeStepZone * SecInHour
  END DO

  DO Loop = 1, TotBBHeat
    ZoneBBHeat(Loop)%Consumption = ZoneBBHeat(Loop)%Power * TimeStepZone * SecInHour
    ZoneBBHeat(Loop)%RadGainEnergy = ZoneBBHeat(Loop)%RadGainRate * TimeStepZone * SecInHour
    ZoneBBHeat(Loop)%ConGainEnergy = ZoneBBHeat(Loop)%ConGainRate * TimeStepZone * SecInHour
    ZoneBBHeat(Loop)%TotGainEnergy = ZoneBBHeat(Loop)%TotGainRate * TimeStepZone * SecInHour
  END DO

  DO ZoneLoop = 1, NumOfZones
    ! People
    ZnRpt(ZoneLoop)%PeopleNumOcc = ZoneIntGain(ZoneLoop)%NOFOCC
    ZnRpt(ZoneLoop)%PeopleRadGain = ZoneIntGain(ZoneLoop)%QOCRAD*TimeStepZone*SecInHour
    ZnRpt(ZoneLoop)%PeopleConGain = ZoneIntGain(ZoneLoop)%QOCCON*TimeStepZone*SecInHour
    ZnRpt(ZoneLoop)%PeopleSenGain = ZoneIntGain(ZoneLoop)%QOCSEN*TimeStepZone*SecInHour
    ZnRpt(ZoneLoop)%PeopleLatGain = ZoneIntGain(ZoneLoop)%QOCLAT*TimeStepZone*SecInHour
    ZnRpt(ZoneLoop)%PeopleTotGain = ZoneIntGain(ZoneLoop)%QOCTOT*TimeStepZone*SecInHour
    ZnRpt(ZoneLoop)%PeopleRadGainRate = ZoneIntGain(ZoneLoop)%QOCRAD
    ZnRpt(ZoneLoop)%PeopleConGainRate = ZoneIntGain(ZoneLoop)%QOCCON
    ZnRpt(ZoneLoop)%PeopleSenGainRate = ZoneIntGain(ZoneLoop)%QOCSEN
    ZnRpt(ZoneLoop)%PeopleLatGainRate = ZoneIntGain(ZoneLoop)%QOCLAT
    ZnRpt(ZoneLoop)%PeopleTotGainRate = ZoneIntGain(ZoneLoop)%QOCTOT

    ! General Lights
    ZnRpt(ZoneLoop)%LtsRetAirGain = ZoneIntGain(ZoneLoop)%QLTCRA*TimeStepZone*SecInHour
    ZnRpt(ZoneLoop)%LtsRadGain    = ZoneIntGain(ZoneLoop)%QLTRAD*TimeStepZone*SecInHour
    ZnRpt(ZoneLoop)%LtsTotGain    = ZoneIntGain(ZoneLoop)%QLTTOT*TimeStepZone*SecInHour
    ZnRpt(ZoneLoop)%LtsConGain    = ZoneIntGain(ZoneLoop)%QLTCON*TimeStepZone*SecInHour
    ZnRpt(ZoneLoop)%LtsVisGain    = ZoneIntGain(ZoneLoop)%QLTSW*TimeStepZone*SecInHour
    ZnRpt(ZoneLoop)%LtsRetAirGainRate = ZoneIntGain(ZoneLoop)%QLTCRA
    ZnRpt(ZoneLoop)%LtsRadGainRate= ZoneIntGain(ZoneLoop)%QLTRAD
    ZnRpt(ZoneLoop)%LtsTotGainRate= ZoneIntGain(ZoneLoop)%QLTTOT
    ZnRpt(ZoneLoop)%LtsConGainRate= ZoneIntGain(ZoneLoop)%QLTCON
    ZnRpt(ZoneLoop)%LtsVisGainRate= ZoneIntGain(ZoneLoop)%QLTSW
    ZnRpt(ZoneLoop)%LtsElecConsump= ZnRpt(ZoneLoop)%LtsTotGain

    ! Electric Equipment
    ZnRpt(ZoneLoop)%ElecConGain    = ZoneIntGain(ZoneLoop)%QEECON*TimeStepZone*SecInHour
    ZnRpt(ZoneLoop)%ElecRadGain    = ZoneIntGain(ZoneLoop)%QEERAD*TimeStepZone*SecInHour
    ZnRpt(ZoneLoop)%ElecLatGain    = ZoneIntGain(ZoneLoop)%QEELAT*TimeStepZone*SecInHour
    ZnRpt(ZoneLoop)%ElecLost       = ZoneIntGain(ZoneLoop)%QEELost*TimeStepZone*SecInHour
    ZnRpt(ZoneLoop)%ElecConGainRate= ZoneIntGain(ZoneLoop)%QEECON
    ZnRpt(ZoneLoop)%ElecRadGainRate= ZoneIntGain(ZoneLoop)%QEERAD
    ZnRpt(ZoneLoop)%ElecLatGainRate= ZoneIntGain(ZoneLoop)%QEELAT
    ZnRpt(ZoneLoop)%ElecLostRate   = ZoneIntGain(ZoneLoop)%QEELost
    ZnRpt(ZoneLoop)%ElecConsump = ZnRpt(ZoneLoop)%ElecConGain+ZnRpt(ZoneLoop)%ElecRadGain+  &
                                  ZnRpt(ZoneLoop)%ElecLatGain+ZnRpt(ZoneLoop)%ElecLost
    ZnRpt(ZoneLoop)%ElecTotGain       = ZnRpt(ZoneLoop)%ElecConGain+ZnRpt(ZoneLoop)%ElecRadGain+ZnRpt(ZoneLoop)%ElecLatGain
    ZnRpt(ZoneLoop)%ElecTotGainRate   = ZnRpt(ZoneLoop)%ElecConGainRate+ZnRpt(ZoneLoop)%ElecRadGainRate+  &
       ZnRpt(ZoneLoop)%ElecLatGainRate

    ! Gas Equipment
    ZnRpt(ZoneLoop)%GasConGain    = ZoneIntGain(ZoneLoop)%QGECON*TimeStepZone*SecInHour
    ZnRpt(ZoneLoop)%GasRadGain    = ZoneIntGain(ZoneLoop)%QGERAD*TimeStepZone*SecInHour
    ZnRpt(ZoneLoop)%GasLatGain    = ZoneIntGain(ZoneLoop)%QGELAT*TimeStepZone*SecInHour
    ZnRpt(ZoneLoop)%GasLost       = ZoneIntGain(ZoneLoop)%QGELost*TimeStepZone*SecInHour
    ZnRpt(ZoneLoop)%GasConGainRate= ZoneIntGain(ZoneLoop)%QGECON
    ZnRpt(ZoneLoop)%GasRadGainRate= ZoneIntGain(ZoneLoop)%QGERAD
    ZnRpt(ZoneLoop)%GasLatGainRate= ZoneIntGain(ZoneLoop)%QGELAT
    ZnRpt(ZoneLoop)%GasLostRate   = ZoneIntGain(ZoneLoop)%QGELost
    ZnRpt(ZoneLoop)%GasConsump = ZnRpt(ZoneLoop)%GasConGain+ZnRpt(ZoneLoop)%GasRadGain+  &
                                  ZnRpt(ZoneLoop)%GasLatGain+ZnRpt(ZoneLoop)%GasLost
    ZnRpt(ZoneLoop)%GasTotGain       = ZnRpt(ZoneLoop)%GasConGain+ZnRpt(ZoneLoop)%GasRadGain+ZnRpt(ZoneLoop)%GasLatGain
    ZnRpt(ZoneLoop)%GasTotGainRate   = ZnRpt(ZoneLoop)%GasConGainRate+ZnRpt(ZoneLoop)%GasRadGainRate+  &
       ZnRpt(ZoneLoop)%GasLatGainRate

    ! Hot Water Equipment
    ZnRpt(ZoneLoop)%HWConGain    = ZoneIntGain(ZoneLoop)%QHWCON*TimeStepZone*SecInHour
    ZnRpt(ZoneLoop)%HWRadGain    = ZoneIntGain(ZoneLoop)%QHWRAD*TimeStepZone*SecInHour
    ZnRpt(ZoneLoop)%HWLatGain    = ZoneIntGain(ZoneLoop)%QHWLAT*TimeStepZone*SecInHour
    ZnRpt(ZoneLoop)%HWLost       = ZoneIntGain(ZoneLoop)%QHWLost*TimeStepZone*SecInHour
    ZnRpt(ZoneLoop)%HWConGainRate= ZoneIntGain(ZoneLoop)%QHWCON
    ZnRpt(ZoneLoop)%HWRadGainRate= ZoneIntGain(ZoneLoop)%QHWRAD
    ZnRpt(ZoneLoop)%HWLatGainRate= ZoneIntGain(ZoneLoop)%QHWLAT
    ZnRpt(ZoneLoop)%HWLostRate   = ZoneIntGain(ZoneLoop)%QHWLost
    ZnRpt(ZoneLoop)%HWConsump = ZnRpt(ZoneLoop)%HWConGain+ZnRpt(ZoneLoop)%HWRadGain+  &
                                  ZnRpt(ZoneLoop)%HWLatGain+ZnRpt(ZoneLoop)%HWLost
    ZnRpt(ZoneLoop)%HWTotGain       = ZnRpt(ZoneLoop)%HWConGain+ZnRpt(ZoneLoop)%HWRadGain+ZnRpt(ZoneLoop)%HWLatGain
    ZnRpt(ZoneLoop)%HWTotGainRate   = ZnRpt(ZoneLoop)%HWConGainRate+ZnRpt(ZoneLoop)%HWRadGainRate+  &
       ZnRpt(ZoneLoop)%HWLatGainRate

    ! Steam Equipment
    ZnRpt(ZoneLoop)%SteamConGain    = ZoneIntGain(ZoneLoop)%QSECON*TimeStepZone*SecInHour
    ZnRpt(ZoneLoop)%SteamRadGain    = ZoneIntGain(ZoneLoop)%QSERAD*TimeStepZone*SecInHour
    ZnRpt(ZoneLoop)%SteamLatGain    = ZoneIntGain(ZoneLoop)%QSELAT*TimeStepZone*SecInHour
    ZnRpt(ZoneLoop)%SteamLost       = ZoneIntGain(ZoneLoop)%QSELost*TimeStepZone*SecInHour
    ZnRpt(ZoneLoop)%SteamConGainRate= ZoneIntGain(ZoneLoop)%QSECON
    ZnRpt(ZoneLoop)%SteamRadGainRate= ZoneIntGain(ZoneLoop)%QSERAD
    ZnRpt(ZoneLoop)%SteamLatGainRate= ZoneIntGain(ZoneLoop)%QSELAT
    ZnRpt(ZoneLoop)%SteamLostRate   = ZoneIntGain(ZoneLoop)%QSELost
    ZnRpt(ZoneLoop)%SteamConsump = ZnRpt(ZoneLoop)%SteamConGain+ZnRpt(ZoneLoop)%SteamRadGain+  &
                                  ZnRpt(ZoneLoop)%SteamLatGain+ZnRpt(ZoneLoop)%SteamLost
    ZnRpt(ZoneLoop)%SteamTotGain       = ZnRpt(ZoneLoop)%SteamConGain+ZnRpt(ZoneLoop)%SteamRadGain+ZnRpt(ZoneLoop)%SteamLatGain
    ZnRpt(ZoneLoop)%SteamTotGainRate   = ZnRpt(ZoneLoop)%SteamConGainRate+ZnRpt(ZoneLoop)%SteamRadGainRate+  &
       ZnRpt(ZoneLoop)%SteamLatGainRate

    ! Other Equipment
    ZnRpt(ZoneLoop)%OtherConGain    = ZoneIntGain(ZoneLoop)%QOECON*TimeStepZone*SecInHour
    ZnRpt(ZoneLoop)%OtherRadGain    = ZoneIntGain(ZoneLoop)%QOERAD*TimeStepZone*SecInHour
    ZnRpt(ZoneLoop)%OtherLatGain    = ZoneIntGain(ZoneLoop)%QOELAT*TimeStepZone*SecInHour
    ZnRpt(ZoneLoop)%OtherLost       = ZoneIntGain(ZoneLoop)%QOELost*TimeStepZone*SecInHour
    ZnRpt(ZoneLoop)%OtherConGainRate= ZoneIntGain(ZoneLoop)%QOECON
    ZnRpt(ZoneLoop)%OtherRadGainRate= ZoneIntGain(ZoneLoop)%QOERAD
    ZnRpt(ZoneLoop)%OtherLatGainRate= ZoneIntGain(ZoneLoop)%QOELAT
    ZnRpt(ZoneLoop)%OtherLostRate   = ZoneIntGain(ZoneLoop)%QOELost
    ZnRpt(ZoneLoop)%OtherTotGain       = ZnRpt(ZoneLoop)%OtherConGain+ZnRpt(ZoneLoop)%OtherRadGain+ZnRpt(ZoneLoop)%OtherLatGain
    ZnRpt(ZoneLoop)%OtherTotGainRate   = ZnRpt(ZoneLoop)%OtherConGainRate+ZnRpt(ZoneLoop)%OtherRadGainRate+  &
       ZnRpt(ZoneLoop)%OtherLatGainRate

    ! Baseboard Heat
    ZnRpt(ZoneLoop)%BaseHeatConGain   = ZoneIntGain(ZoneLoop)%QBBCON*TimeStepZone*SecInHour
    ZnRpt(ZoneLoop)%BaseHeatRadGain   = ZoneIntGain(ZoneLoop)%QBBRAD*TimeStepZone*SecInHour
    ZnRpt(ZoneLoop)%BaseHeatConGainRate   = ZoneIntGain(ZoneLoop)%QBBCON
    ZnRpt(ZoneLoop)%BaseHeatRadGainRate   = ZoneIntGain(ZoneLoop)%QBBRAD
    ZnRpt(ZoneLoop)%BaseHeatTotGain   = ZnRpt(ZoneLoop)%BaseHeatConGain + ZnRpt(ZoneLoop)%BaseHeatRadGain
    ZnRpt(ZoneLoop)%BaseHeatTotGainRate   = ZnRpt(ZoneLoop)%BaseHeatConGainRate + ZnRpt(ZoneLoop)%BaseHeatRadGainRate
    ZnRpt(ZoneLoop)%BaseHeatElecCons  = ZnRpt(ZoneLoop)%BaseHeatTotGain

    ! Overall Zone Variables

    ! these overalls include component gains from devices like water heater, water use, and generators
    !   working vars QFCConv QGenConv QFCRad QGenRad  WaterUseLatentGain WaterThermalTankGain WaterUseSensibleGain

    ZnRpt(ZoneLoop)%TotVisHeatGain      = ZnRpt(ZoneLoop)%LtsVisGain
    ZnRpt(ZoneLoop)%TotVisHeatGainRate  = ZnRpt(ZoneLoop)%LtsVisGainRate

    CALL SumInternalRadiationGainsByTypes(ZoneLoop, TradIntGainTypes, ZnRpt(ZoneLoop)%TotRadiantGainRate)
    ZnRpt(ZoneLoop)%TotRadiantGain = ZnRpt(ZoneLoop)%TotRadiantGainRate*TimeStepZone*SecInHour

    CALL SumInternalConvectionGainsByTypes(ZoneLoop, TradIntGainTypes, ZnRpt(ZoneLoop)%TotConvectiveGainRate)
    ZnRpt(ZoneLoop)%TotConvectiveGain = ZnRpt(ZoneLoop)%TotConvectiveGainRate *TimeStepZone*SecInHour

    CALL SumInternalLatentGainsByTypes( ZoneLoop, TradIntGainTypes, ZnRpt(ZoneLoop)%TotLatentGainRate)
    ZnRpt(ZoneLoop)%TotLatentGain = ZnRpt(ZoneLoop)%TotLatentGainRate *TimeStepZone*SecInHour

    ZnRpt(ZoneLoop)%TotTotalHeatGainRate  = ZnRpt(ZoneLoop)%TotLatentGainRate + ZnRpt(ZoneLoop)%TotRadiantGainRate &
                                            + ZnRpt(ZoneLoop)%TotConvectiveGainRate + ZnRpt(ZoneLoop)%TotVisHeatGainRate
    ZnRpt(ZoneLoop)%TotTotalHeatGain  = ZnRpt(ZoneLoop)%TotTotalHeatGainRate * TimeStepZone * SecInHour
  END DO

  RETURN

END SUBROUTINE ReportInternalHeatGains

FUNCTION GetDesignLightingLevelForZone(WhichZone) RESULT(DesignLightingLevelSum)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   April 2007; January 2008 - moved to InternalGains
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This routine sums the Lighting Level for a zone.
          ! Will issue a severe error for illegal zone.
          ! Must be called after InternalHeatGains get input.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataHeatBalance
  USE DataGlobals

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)          :: WhichZone ! name of zone
  REAL(r64)                    :: DesignLightingLevelSum ! Sum of design lighting level for this zone

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER :: Loop

  IF (GetInternalHeatGainsInputFlag) THEN
    CALL ShowFatalError('GetDesignLightingLevelForZone: Function called prior to Getting Lights Input.')
  ENDIF

  DesignLightingLevelSum=0.0d0

  DO Loop=1,TotLights
    IF (Lights(Loop)%ZonePtr == WhichZone) THEN
      DesignLightingLevelSum=DesignLightingLevelSum+Lights(Loop)%DesignLevel
    ENDIF
  ENDDO

  RETURN

END FUNCTION GetDesignLightingLevelForZone

SUBROUTINE CheckLightsReplaceableMinMaxForZone(WhichZone)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   April 2007
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Daylighting is not available unless Lights (replaceable) is 0.0 or 1.0.  No dimming will be done
          ! unless the lights replaceable fraction is 1.0.  This is documented in the InputOutputReference but
          ! not warned about.  Also, this will sum the Zone Design Lighting level, in case the calling routine
          ! would like to have an error if the lights is zero and daylighting is requested.

          ! METHODOLOGY EMPLOYED:
          ! Traverse the LIGHTS structure and get fraction replaceable - min/max as well as lighting
          ! level for a zone.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataDaylighting

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: WhichZone  ! Zone Number

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: Loop
  REAL(r64) :: LightsRepMin  ! Minimum Lighting replacement fraction for any lights statement for this zone
  REAL(r64) :: LightsRepMax  ! Maximum Lighting replacement fraction for any lights statement for this zone
  INTEGER :: NumLights  ! Number of Lights statement for that zone.

  IF (GetInternalHeatGainsInputFlag) THEN
    CALL ShowFatalError('CheckLightsReplaceableMinMaxForZone: Function called prior to Getting Lights Input.')
  ENDIF

  LightsRepMin=99999.d0
  LightsRepMax=-99999.d0
  NumLights=0

  DO Loop=1,TotLights
    IF (Lights(Loop)%ZonePtr /= WhichZone) CYCLE
    LightsRepMin=MIN(LightsRepMin,Lights(Loop)%FractionReplaceable)
    LightsRepMax=MAX(LightsRepMax,Lights(Loop)%FractionReplaceable)
    NumLights=NumLights+1
    IF ((ZoneDaylight(Lights(Loop)%ZonePtr)%DaylightType == DetailedDaylighting &
      .OR. ZoneDaylight(Lights(Loop)%ZonePtr)%DaylightType == DElightDaylighting) &
      .AND. (Lights(Loop)%FractionReplaceable > 0.0d0 .AND. Lights(Loop)%FractionReplaceable < 1.0d0)) THEN
      CALL ShowWarningError('CheckLightsReplaceableMinMaxForZone: '//  &
         'Fraction Replaceable must be 0.0 or 1.0 if used with daylighting.')
      CALL ShowContinueError('..Lights="'//TRIM(Lights(Loop)%Name)//  &
          '", Fraction Replaceable will be reset to 1.0 to allow dimming controls')
      CALL ShowContinueError('..in Zone='//TRIM(Zone(WhichZone)%Name))
      Lights(Loop)%FractionReplaceable = 1.0d0
    END IF
  ENDDO

  IF (ZoneDaylight(WhichZone)%DaylightType == DetailedDaylighting) THEN
    IF (LightsRepMax == 0.0d0) THEN
      CALL ShowWarningError('CheckLightsReplaceable: Zone "'//TRIM(Zone(WhichZone)%Name)//  &
                     '" has Daylighting:Controls.')
      CALL ShowContinueError('but all of the LIGHTS object in that zone have zero Fraction Replaceable.')
      CALL ShowContinueError('The daylighting controls will have no effect.')
    ENDIF
    IF (NumLights == 0) THEN
      CALL ShowWarningError('CheckLightsReplaceable: Zone "'//TRIM(Zone(WhichZone)%Name)//  &
                     '" has Daylighting:Controls.')
      CALL ShowContinueError('but there are no LIGHTS objects in that zone.')
      CALL ShowContinueError('The daylighting controls will have no effect.')
    ENDIF
  ELSEIF (ZoneDaylight(WhichZone)%DaylightType == DElightDaylighting) THEN
    IF (LightsRepMax == 0.0d0) THEN
      CALL ShowWarningError('CheckLightsReplaceable: Zone "'//TRIM(Zone(WhichZone)%Name)//  &
                     '" has Daylighting:Controls.')
      CALL ShowContinueError('but all of the LIGHTS object in that zone have zero Fraction Replaceable.')
      CALL ShowContinueError('The daylighting controls will have no effect.')
    ENDIF
    IF (NumLights == 0) THEN
      CALL ShowWarningError('CheckLightsReplaceable: Zone "'//TRIM(Zone(WhichZone)%Name)//  &
                     '" has Daylighting:Controls.')
      CALL ShowContinueError('but there are no LIGHTS objects in that zone.')
      CALL ShowContinueError('The daylighting controls will have no effect.')
    ENDIF
  ENDIF

  RETURN

END SUBROUTINE CheckLightsReplaceableMinMaxForZone

SUBROUTINE UpdateInternalGainValues(SuppressRadiationUpdate, SumLatentGains)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B. Griffith
          !       DATE WRITTEN   Dec. 2011
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! <description>

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataHeatBalFanSys, ONLY: ZoneLatentGain
  USE DataContaminantBalance, ONLY: Contaminant, ZoneGCGain

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  LOGICAL , OPTIONAL, INTENT(IN)  :: SuppressRadiationUpdate
  LOGICAL , OPTIONAL, INTENT(IN)  :: SumLatentGains


          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: Loop
  INTEGER :: NZ
  LOGICAL :: DoRadiationUpdate
  LOGICAL :: ReSumLatentGains

  DoRadiationUpdate = .TRUE.
  ReSumLatentGains  = .FALSE.

  IF (PRESENT(SuppressRadiationUpdate)) THEN
    IF (SuppressRadiationUpdate) DoRadiationUpdate = .FALSE.
  ENDIF

  IF (PRESENT(SumLatentGains)) THEN
    IF (SumLatentGains) ReSumLatentGains = .TRUE.
  ENDIF

! store pointer values to hold generic internal gain values constant for entire timestep
  DO NZ = 1, NumOfZones
    DO Loop=1, ZoneIntGain(NZ)%NumberOfDevices
      ZoneIntGain(NZ)%Device(Loop)%ConvectGainRate         = ZoneIntGain(NZ)%Device(Loop)%PtrConvectGainRate
      ZoneIntGain(NZ)%Device(Loop)%ReturnAirConvGainRate   = ZoneIntGain(NZ)%Device(Loop)%PtrReturnAirConvGainRate
      IF (DoRadiationUpdate) ZoneIntGain(NZ)%Device(Loop)%RadiantGainRate         = ZoneIntGain(NZ)%Device(Loop)%PtrRadiantGainRate
      ZoneIntGain(NZ)%Device(Loop)%LatentGainRate          = ZoneIntGain(NZ)%Device(Loop)%PtrLatentGainRate
      ZoneIntGain(NZ)%Device(Loop)%ReturnAirLatentGainRate = ZoneIntGain(NZ)%Device(Loop)%PtrReturnAirLatentGainRate
      ZoneIntGain(NZ)%Device(Loop)%CarbonDioxideGainRate   = ZoneIntGain(NZ)%Device(Loop)%PtrCarbonDioxideGainRate
      ZoneIntGain(NZ)%Device(Loop)%GenericContamGainRate   = ZoneIntGain(NZ)%Device(Loop)%PtrGenericContamGainRate
    ENDDO
    IF (ReSumLatentGains) THEN
      CALL SumAllInternalLatentGains(NZ, ZoneLatentGain(NZ))
    ENDIF
  ENDDO

  If (Contaminant%GenericContamSimulation .AND. ALLOCATED(ZoneGCGain)) Then
    DO NZ = 1, NumOfZones
      CALL SumAllInternalGenericContamGains(NZ, ZoneGCGain(NZ) )
      ZnRpt(NZ)%GCRate = ZoneGCGain(NZ)
    ENDDO
  END IF

  RETURN

END SUBROUTINE UpdateInternalGainValues

SUBROUTINE SumAllInternalConvectionGains(ZoneNum, SumConvGainRate)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B. Griffith
          !       DATE WRITTEN   Nov. 2011
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! worker routine for summing all the internal gain types

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: ZoneNum  ! zone index pointer for which zone to sum gains for
  REAL(r64), INTENT(OUT)    :: SumConvGainRate

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64) :: tmpSumConvGainRate
  INTEGER  :: DeviceNum

  tmpSumConvGainRate = 0.d0

  IF (ZoneIntGain(ZoneNum)%NumberOfDevices == 0) THEN
    SumConvGainRate = 0.d0
    RETURN
  ENDIF

  DO DeviceNum = 1, ZoneIntGain(ZoneNum)%NumberOfDevices
    tmpSumConvGainRate = tmpSumConvGainRate + ZoneIntGain(ZoneNum)%Device(DeviceNum)%ConvectGainRate
  ENDDO

  SumConvGainRate = tmpSumConvGainRate

  RETURN

END SUBROUTINE SumAllInternalConvectionGains

SUBROUTINE SumInternalConvectionGainsByTypes(ZoneNum, GainTypeARR, SumConvGainRate)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B. Griffith
          !       DATE WRITTEN   Nov. 2011
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! worker routine for summing a subset of the internal gain types

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: ZoneNum  ! zone index pointer for which zone to sum gains for
  INTEGER, DIMENSION(:), INTENT(IN) :: GainTypeARR ! variable length 1-d array of integer valued gain types
  REAL(r64), INTENT(OUT)    :: SumConvGainRate

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: NumberOfTypes
  REAL(r64) :: tmpSumConvGainRate
  INTEGER  :: DeviceNum
  INTEGER  :: TypeNum

  NumberOfTypes = Size(GainTypeARR)
  tmpSumConvGainRate = 0.d0

  IF (ZoneIntGain(ZoneNum)%NumberOfDevices == 0) THEN
    SumConvGainRate = 0.d0
    RETURN
  ENDIF

  DO DeviceNum = 1, ZoneIntGain(ZoneNum)%NumberOfDevices
    DO TypeNum = 1, NumberOfTypes

      IF (ZoneIntGain(ZoneNum)%Device(DeviceNum)%CompTypeOfNum == GainTypeARR(TypeNum)) THEN
        tmpSumConvGainRate = tmpSumConvGainRate + ZoneIntGain(ZoneNum)%Device(DeviceNum)%ConvectGainRate
      ENDIF

    ENDDO
  ENDDO

  SumConvGainRate = tmpSumConvGainRate
  RETURN

END SUBROUTINE SumInternalConvectionGainsByTypes


SUBROUTINE SumAllReturnAirConvectionGains(ZoneNum, SumReturnAirGainRate)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B. Griffith
          !       DATE WRITTEN   Dec. 2011
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! worker routine for summing all the internal gain types

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: ZoneNum  ! zone index pointer for which zone to sum gains for
  REAL(r64), INTENT(OUT)    :: SumReturnAirGainRate

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64) :: tmpSumRetAirGainRate
  INTEGER  :: DeviceNum

  tmpSumRetAirGainRate = 0.d0

  IF (ZoneIntGain(ZoneNum)%NumberOfDevices == 0) THEN
    SumReturnAirGainRate = 0.d0
    RETURN
  ENDIF

  DO DeviceNum = 1, ZoneIntGain(ZoneNum)%NumberOfDevices
    tmpSumRetAirGainRate = tmpSumRetAirGainRate + ZoneIntGain(ZoneNum)%Device(DeviceNum)%ReturnAirConvGainRate
  ENDDO

  SumReturnAirGainRate = tmpSumRetAirGainRate

  RETURN

END SUBROUTINE SumAllReturnAirConvectionGains

SUBROUTINE SumReturnAirConvectionGainsByTypes(ZoneNum, GainTypeARR, SumReturnAirGainRate)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B. Griffith
          !       DATE WRITTEN   Nov. 2011
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! worker routine for summing a subset of the internal gain types

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: ZoneNum  ! zone index pointer for which zone to sum gains for
  INTEGER, DIMENSION(:), INTENT(IN) :: GainTypeARR ! variable length 1-d array of integer valued gain types
  REAL(r64), INTENT(OUT)    :: SumReturnAirGainRate

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: NumberOfTypes
  REAL(r64) :: tmpSumRetAirConvGainRate
  INTEGER  :: DeviceNum
  INTEGER  :: TypeNum

  NumberOfTypes = Size(GainTypeARR)
  tmpSumRetAirConvGainRate = 0.d0

  IF (ZoneIntGain(ZoneNum)%NumberOfDevices == 0) THEN
    SumReturnAirGainRate = 0.d0
    RETURN
  ENDIF

  DO DeviceNum = 1, ZoneIntGain(ZoneNum)%NumberOfDevices
    DO TypeNum = 1, NumberOfTypes

      IF (ZoneIntGain(ZoneNum)%Device(DeviceNum)%CompTypeOfNum == GainTypeARR(TypeNum)) THEN
        tmpSumRetAirConvGainRate = tmpSumRetAirConvGainRate + ZoneIntGain(ZoneNum)%Device(DeviceNum)%ReturnAirConvGainRate
      ENDIF

    ENDDO
  ENDDO

  SumReturnAirGainRate = tmpSumRetAirConvGainRate
  RETURN

END SUBROUTINE SumReturnAirConvectionGainsByTypes


SUBROUTINE SumAllInternalRadiationGains(ZoneNum, SumRadGainRate)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B. Griffith
          !       DATE WRITTEN   Nov. 2011
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! worker routine for summing all the internal gain types

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: ZoneNum  ! zone index pointer for which zone to sum gains for
  REAL(r64), INTENT(OUT)    :: SumRadGainRate

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64) :: tmpSumRadGainRate
  INTEGER  :: DeviceNum

  tmpSumRadGainRate = 0.d0

  IF (ZoneIntGain(ZoneNum)%NumberOfDevices == 0) THEN
    SumRadGainRate = 0.d0
    RETURN
  ENDIF

  DO DeviceNum = 1, ZoneIntGain(ZoneNum)%NumberOfDevices
    tmpSumRadGainRate = tmpSumRadGainRate + ZoneIntGain(ZoneNum)%Device(DeviceNum)%RadiantGainRate
  ENDDO

  SumRadGainRate = tmpSumRadGainRate

  RETURN

END SUBROUTINE SumAllInternalRadiationGains

SUBROUTINE SumInternalRadiationGainsByTypes(ZoneNum, GainTypeARR, SumRadiationGainRate)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B. Griffith
          !       DATE WRITTEN   Dec. 2011
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! worker routine for summing a subset of the internal gain types

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: ZoneNum  ! zone index pointer for which zone to sum gains for
  INTEGER, DIMENSION(:), INTENT(IN) :: GainTypeARR ! variable length 1-d array of integer valued gain types
  REAL(r64), INTENT(OUT)    :: SumRadiationGainRate

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: NumberOfTypes
  REAL(r64) :: tmpSumRadiationGainRate
  INTEGER  :: DeviceNum
  INTEGER  :: TypeNum

  NumberOfTypes = Size(GainTypeARR)
  tmpSumRadiationGainRate = 0.d0

  IF (ZoneIntGain(ZoneNum)%NumberOfDevices == 0) THEN
    SumRadiationGainRate = 0.d0
    RETURN
  ENDIF

  DO DeviceNum = 1, ZoneIntGain(ZoneNum)%NumberOfDevices
    DO TypeNum = 1, NumberOfTypes

      IF (ZoneIntGain(ZoneNum)%Device(DeviceNum)%CompTypeOfNum == GainTypeARR(TypeNum)) THEN
        tmpSumRadiationGainRate = tmpSumRadiationGainRate + ZoneIntGain(ZoneNum)%Device(DeviceNum)%RadiantGainRate
      ENDIF

    ENDDO
  ENDDO

  SumRadiationGainRate = tmpSumRadiationGainRate
  RETURN

END SUBROUTINE SumInternalRadiationGainsByTypes

SUBROUTINE SumAllInternalLatentGains(ZoneNum, SumLatentGainRate)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B. Griffith
          !       DATE WRITTEN   Nov. 2011
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! worker routine for summing all the internal gain types

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: ZoneNum  ! zone index pointer for which zone to sum gains for
  REAL(r64), INTENT(OUT)    :: SumLatentGainRate

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64) :: tmpSumLatentGainRate
  INTEGER  :: DeviceNum

  tmpSumLatentGainRate = 0.d0

  IF (ZoneIntGain(ZoneNum)%NumberOfDevices == 0) THEN
    SumLatentGainRate = 0.d0
    RETURN
  ENDIF

  DO DeviceNum = 1, ZoneIntGain(ZoneNum)%NumberOfDevices
    tmpSumLatentGainRate = tmpSumLatentGainRate + ZoneIntGain(ZoneNum)%Device(DeviceNum)%LatentGainRate
  ENDDO

  SumLatentGainRate = tmpSumLatentGainRate

  RETURN

END SUBROUTINE SumAllInternalLatentGains

SUBROUTINE SumInternalLatentGainsByTypes(ZoneNum, GainTypeARR, SumLatentGainRate)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B. Griffith
          !       DATE WRITTEN   Dec. 2011
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! worker routine for summing a subset of the internal gain types

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: ZoneNum  ! zone index pointer for which zone to sum gains for
  INTEGER, DIMENSION(:), INTENT(IN) :: GainTypeARR ! variable length 1-d array of integer valued gain types
  REAL(r64), INTENT(OUT)    :: SumLatentGainRate

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: NumberOfTypes
  REAL(r64) :: tmpSumLatentGainRate
  INTEGER  :: DeviceNum
  INTEGER  :: TypeNum

  NumberOfTypes = Size(GainTypeARR)
  tmpSumLatentGainRate = 0.d0

  IF (ZoneIntGain(ZoneNum)%NumberOfDevices == 0) THEN
    SumLatentGainRate = 0.d0
    RETURN
  ENDIF

  DO DeviceNum = 1, ZoneIntGain(ZoneNum)%NumberOfDevices
    DO TypeNum = 1, NumberOfTypes

      IF (ZoneIntGain(ZoneNum)%Device(DeviceNum)%CompTypeOfNum == GainTypeARR(TypeNum)) THEN
        tmpSumLatentGainRate = tmpSumLatentGainRate + ZoneIntGain(ZoneNum)%Device(DeviceNum)%LatentGainRate
      ENDIF

    ENDDO
  ENDDO

  SumLatentGainRate = tmpSumLatentGainRate
  RETURN

END SUBROUTINE SumInternalLatentGainsByTypes

SUBROUTINE SumAllReturnAirLatentGains(ZoneNum, SumRetAirLatentGainRate)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B. Griffith
          !       DATE WRITTEN   Nov. 2011
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! worker routine for summing all the internal gain types

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: ZoneNum  ! zone index pointer for which zone to sum gains for
  REAL(r64), INTENT(OUT)    :: SumRetAirLatentGainRate

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64) :: tmpSumLatentGainRate
  INTEGER  :: DeviceNum

  tmpSumLatentGainRate = 0.d0

  IF (ZoneIntGain(ZoneNum)%NumberOfDevices == 0) THEN
    SumRetAirLatentGainRate = 0.d0
    RETURN
  ENDIF

  DO DeviceNum = 1, ZoneIntGain(ZoneNum)%NumberOfDevices
    tmpSumLatentGainRate = tmpSumLatentGainRate + ZoneIntGain(ZoneNum)%Device(DeviceNum)%ReturnAirLatentGainRate
  ENDDO

  SumRetAirLatentGainRate = tmpSumLatentGainRate

  RETURN

END SUBROUTINE SumAllReturnAirLatentGains

SUBROUTINE SumAllInternalCO2Gains(ZoneNum, SumCO2GainRate)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B. Griffith
          !       DATE WRITTEN   Dec. 2011
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! worker routine for summing all the internal gain types

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: ZoneNum  ! zone index pointer for which zone to sum gains for
  REAL(r64), INTENT(OUT)    :: SumCO2GainRate

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64) :: tmpSumCO2GainRate
  INTEGER  :: DeviceNum

  tmpSumCO2GainRate = 0.d0

  IF (ZoneIntGain(ZoneNum)%NumberOfDevices == 0) THEN
    SumCO2GainRate = 0.d0
    RETURN
  ENDIF

  DO DeviceNum = 1, ZoneIntGain(ZoneNum)%NumberOfDevices
    tmpSumCO2GainRate = tmpSumCO2GainRate + ZoneIntGain(ZoneNum)%Device(DeviceNum)%CarbonDioxideGainRate
  ENDDO

  SumCO2GainRate = tmpSumCO2GainRate

  RETURN

END SUBROUTINE SumAllInternalCO2Gains

SUBROUTINE SumInternalCO2GainsByTypes(ZoneNum, GainTypeARR, SumCO2GainRate)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B. Griffith
          !       DATE WRITTEN   Dec. 2011
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! worker routine for summing a subset of the internal gain types

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: ZoneNum  ! zone index pointer for which zone to sum gains for
  INTEGER, DIMENSION(:), INTENT(IN) :: GainTypeARR ! variable length 1-d array of integer valued gain types
  REAL(r64), INTENT(OUT)    :: SumCO2GainRate

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: NumberOfTypes
  REAL(r64) :: tmpSumCO2GainRate
  INTEGER  :: DeviceNum
  INTEGER  :: TypeNum

  NumberOfTypes = Size(GainTypeARR)
  tmpSumCO2GainRate = 0.d0

  IF (ZoneIntGain(ZoneNum)%NumberOfDevices == 0) THEN
    SumCO2GainRate = 0.d0
    RETURN
  ENDIF

  DO DeviceNum = 1, ZoneIntGain(ZoneNum)%NumberOfDevices
    DO TypeNum = 1, NumberOfTypes

      IF (ZoneIntGain(ZoneNum)%Device(DeviceNum)%CompTypeOfNum == GainTypeARR(TypeNum)) THEN
        tmpSumCO2GainRate = tmpSumCO2GainRate + ZoneIntGain(ZoneNum)%Device(DeviceNum)%CarbonDioxideGainRate
      ENDIF

    ENDDO
  ENDDO

  SumCO2GainRate = tmpSumCO2GainRate
  RETURN

END SUBROUTINE SumInternalCO2GainsByTypes

SUBROUTINE SumAllInternalGenericContamGains(ZoneNum, SumGCGainRate)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         L. Gu
          !       DATE WRITTEN   Feb. 2012
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! worker routine for summing all the internal gain types based on the existing subrotine SumAllInternalCO2Gains

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: ZoneNum  ! zone index pointer for which zone to sum gains for
  REAL(r64), INTENT(OUT)    :: SumGCGainRate

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64) :: tmpSumGCGainRate
  INTEGER  :: DeviceNum

  tmpSumGCGainRate = 0.d0

  IF (ZoneIntGain(ZoneNum)%NumberOfDevices == 0) THEN
    SumGCGainRate = 0.d0
    RETURN
  ENDIF

  DO DeviceNum = 1, ZoneIntGain(ZoneNum)%NumberOfDevices
    tmpSumGCGainRate = tmpSumGCGainRate + ZoneIntGain(ZoneNum)%Device(DeviceNum)%GenericContamGainRate
  ENDDO

  SumGCGainRate = tmpSumGCGainRate

  RETURN

END SUBROUTINE SumAllInternalGenericContamGains

SUBROUTINE GatherComponentLoadsIntGain
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Jason Glazer
          !       DATE WRITTEN   September 2012
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !   Gather values during sizing used for loads component report.

          ! METHODOLOGY EMPLOYED:
          !   Save sequence of values for report during sizing.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na
USE DataHeatBalance
USE DataGlobals, ONLY: NumOfTimeStepInHour, CompLoadReportIsReq, isPulseZoneSizing
USE DataSizing, ONLY: CurOverallSimDay
USE OutputReportTabular, ONLY: peopleInstantSeq, peopleLatentSeq, peopleRadSeq, &
                               lightInstantSeq, lightRetAirSeq, lightLWRadSeq, &
                               equipInstantSeq, equipLatentSeq, equipRadSeq, &
                               refrigInstantSeq, refrigRetAirSeq, refrigLatentSeq, &
                               waterUseInstantSeq, waterUseLatentSeq, &
                               hvacLossInstantSeq, hvacLossRadSeq, &
                               powerGenInstantSeq, powerGenRadSeq

IMPLICIT NONE

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
INTEGER :: iZone = 0
INTEGER :: TimeStepInDay = 0
INTEGER, DIMENSION(1) :: IntGainTypesPeople = (/    IntGainTypeOf_People/)
INTEGER, DIMENSION(1) :: IntGainTypesLight =  (/    IntGainTypeOf_Lights/)
INTEGER, DIMENSION(5) :: IntGainTypesEquip = (/     IntGainTypeOf_ElectricEquipment, &
                                                    IntGainTypeOf_GasEquipment, &
                                                    IntGainTypeOf_HotWaterEquipment ,&
                                                    IntGainTypeOf_SteamEquipment, &
                                                    IntGainTypeOf_OtherEquipment/)
INTEGER, DIMENSION(7) :: IntGainTypesRefrig = (/    IntGainTypeOf_RefrigerationCase, &
                                                    IntGainTypeOf_RefrigerationCompressorRack, &
                                                    IntGainTypeOf_RefrigerationSystemAirCooledCondenser ,&
                                                    IntGainTypeOf_RefrigerationSystemSuctionPipe, &
                                                    IntGainTypeOf_RefrigerationSecondaryReceiver, &
                                                    IntGainTypeOf_RefrigerationSecondaryPipe, &
                                                    IntGainTypeOf_RefrigerationWalkIn/)
INTEGER, DIMENSION(3) :: IntGainTypesWaterUse = (/  IntGainTypeOf_WaterUseEquipment, &
                                                    IntGainTypeOf_WaterHeaterMixed, &
                                                    IntGainTypeOf_WaterHeaterStratified/)
INTEGER, DIMENSION(13) :: IntGainTypesHvacLoss = (/ IntGainTypeOf_ZoneBaseboardOutdoorTemperatureControlled, &
                                                    IntGainTypeOf_ThermalStorageChilledWaterMixed, &
                                                    IntGainTypeOf_ThermalStorageChilledWaterStratified, &
                                                    IntGainTypeOf_PipeIndoor, &
                                                    IntGainTypeOf_Pump_VarSpeed, &
                                                    IntGainTypeOf_Pump_ConSpeed, &
                                                    IntGainTypeOf_Pump_Cond, &
                                                    IntGainTypeOf_PumpBank_VarSpeed, &
                                                    IntGainTypeOf_PumpBank_ConSpeed, &
                                                    IntGainTypeOf_PlantComponentUserDefined, &
                                                    IntGainTypeOf_CoilUserDefined, &
                                                    IntGainTypeOf_ZoneHVACForcedAirUserDefined, &
                                                    IntGainTypeOf_AirTerminalUserDefined/)
INTEGER, DIMENSION(8) :: IntGainTypesPowerGen = (/  IntGainTypeOf_GeneratorFuelCell, &
                                                    IntGainTypeOf_GeneratorMicroCHP, &
                                                    IntGainTypeOf_ElectricLoadCenterTransformer ,&
                                                    IntGainTypeOf_ElectricLoadCenterInverterSimple, &
                                                    IntGainTypeOf_ElectricLoadCenterInverterFunctionOfPower, &
                                                    IntGainTypeOf_ElectricLoadCenterInverterLookUpTable, &
                                                    IntGainTypeOf_ElectricLoadCenterStorageBattery, &
                                                    IntGainTypeOf_ElectricLoadCenterStorageSimple/)

IF (CompLoadReportIsReq .AND. .NOT. isPulseZoneSizing) THEN
  TimeStepInDay = (HourOfDay-1)*NumOfTimeStepInHour + TimeStep
  DO iZone = 1, NumOfZones
    CALL SumInternalConvectionGainsByTypes(iZone, IntGainTypesPeople, peopleInstantSeq(iZone,TimeStepInDay,CurOverallSimDay))
    CALL SumInternalLatentGainsByTypes(iZone, IntGainTypesPeople, peopleLatentSeq(iZone,TimeStepInDay,CurOverallSimDay))
    CALL SumInternalRadiationGainsByTypes(iZone, IntGainTypesPeople, peopleRadSeq(iZone,TimeStepInDay,CurOverallSimDay))

    CALL SumInternalConvectionGainsByTypes(iZone, IntGainTypesLight, lightInstantSeq(iZone,TimeStepInDay,CurOverallSimDay))
    CALL SumReturnAirConvectionGainsByTypes(iZone, IntGainTypesLight, lightRetAirSeq(iZone,TimeStepInDay,CurOverallSimDay))
    CALL SumInternalRadiationGainsByTypes(iZone, IntGainTypesLight, lightLWRadSeq(iZone,TimeStepInDay,CurOverallSimDay))

    CALL SumInternalConvectionGainsByTypes(iZone, IntGainTypesEquip, equipInstantSeq(iZone,TimeStepInDay,CurOverallSimDay))
    CALL SumInternalLatentGainsByTypes(iZone, IntGainTypesEquip, equipLatentSeq(iZone,TimeStepInDay,CurOverallSimDay))
    CALL SumInternalRadiationGainsByTypes(iZone, IntGainTypesEquip, equipRadSeq(iZone,TimeStepInDay,CurOverallSimDay))

    CALL SumInternalConvectionGainsByTypes(iZone, IntGainTypesRefrig, refrigInstantSeq(iZone,TimeStepInDay,CurOverallSimDay))
    CALL SumReturnAirConvectionGainsByTypes(iZone, IntGainTypesRefrig, refrigRetAirSeq(iZone,TimeStepInDay,CurOverallSimDay))
    CALL SumInternalLatentGainsByTypes(iZone, IntGainTypesRefrig, refrigLatentSeq(iZone,TimeStepInDay,CurOverallSimDay))

    CALL SumInternalConvectionGainsByTypes(iZone, IntGainTypesWaterUse, waterUseInstantSeq(iZone,TimeStepInDay,CurOverallSimDay))
    CALL SumInternalLatentGainsByTypes(iZone, IntGainTypesWaterUse, waterUseLatentSeq(iZone,TimeStepInDay,CurOverallSimDay))

    CALL SumInternalConvectionGainsByTypes(iZone, IntGainTypesHvacLoss, hvacLossInstantSeq(iZone,TimeStepInDay,CurOverallSimDay))
    CALL SumInternalRadiationGainsByTypes(iZone, IntGainTypesHvacLoss, hvacLossRadSeq(iZone,TimeStepInDay,CurOverallSimDay))

    CALL SumInternalConvectionGainsByTypes(iZone, IntGainTypesPowerGen, powerGenInstantSeq(iZone,TimeStepInDay,CurOverallSimDay))
    CALL SumInternalRadiationGainsByTypes(iZone, IntGainTypesPowerGen, powerGenRadSeq(iZone,TimeStepInDay,CurOverallSimDay))
  END DO
END IF
END SUBROUTINE GatherComponentLoadsIntGain


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

END MODULE InternalHeatGains

! outside of Module

SUBROUTINE SetupZoneInternalGain(ZoneNum, cComponentObject , cComponentName, IntGainComp_TypeOfNum, ConvectionGainRate , &
                     ReturnAirConvectionGainRate, ThermalRadiationGainRate, LatentGainRate, ReturnAirLatentGainRate, &
                     CarbonDioxideGainRate, GenericContamGainRate)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B. Griffith
          !       DATE WRITTEN   November 2011
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! provide a general interface for setting up devices with internal gains

          ! METHODOLOGY EMPLOYED:
          ! use pointers to access gain rates in device models
          ! devices are internal gains like people, lights, electric equipment
          ! and HVAC components with skin loss models like thermal tanks, and power conditioning.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataPrecisionGlobals
  USE DataHeatBalance
  USE InputProcessor, ONLY: MakeUpperCase, SameString
  USE DataInterfaces, ONLY: ShowSevereError, ShowContinueError

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER,           INTENT(IN) :: ZoneNum
  CHARACTER(len=*),  INTENT(IN) :: cComponentObject ! object class name for device contributing internal gain
  CHARACTER(len=*),  INTENT(IN) :: cComponentName  ! user unique name for device
  INTEGER         ,  INTENT(IN) :: IntGainComp_TypeOfNum
  REAL(r64), TARGET, OPTIONAL, INTENT(IN) :: ConvectionGainRate ! pointer target for remote convection gain value to be accessed
  REAL(r64), TARGET, OPTIONAL, INTENT(IN) :: ReturnAirConvectionGainRate
  REAL(r64), TARGET, OPTIONAL, INTENT(IN) :: ThermalRadiationGainRate  ! pointer target for remote IR radiation gain value to be accessed
  REAL(r64), TARGET, OPTIONAL, INTENT(IN) :: LatentGainRate
  REAL(r64), TARGET, OPTIONAL, INTENT(IN) :: ReturnAirLatentGainRate
  REAL(r64), TARGET, OPTIONAL, INTENT(IN) :: CarbonDioxideGainRate
  REAL(r64), TARGET, OPTIONAL, INTENT(IN) :: GenericContamGainRate

          ! SUBROUTINE PARAMETER DEFINITIONS:
  INTEGER, PARAMETER :: DeviceAllocInc = 100


          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: IntGainsNum
  LOGICAL :: FoundIntGainsType
  LOGICAL :: FoundDuplicate
  CHARACTER(len=MaxNameLength) :: UpperCaseObjectType
  CHARACTER(len=MaxNameLength) :: UpperCaseObjectName

  TYPE(GenericComponentZoneIntGainStruct), DIMENSION(:), ALLOCATABLE :: TempGenDeviceIntGainsArr

  FoundIntGainsType = .FALSE.
  FoundDuplicate    = .FALSE.
  UpperCaseObjectType = MakeUpperCase(cComponentObject)
  UpperCaseObjectName = MakeUpperCase(cComponentName)

  ! Check if IntGainComp_TypeOfNum and cComponentObject are consistent
  If (.NOT. SameString(UpperCaseObjectType, ZoneIntGainDeviceTypes(IntGainComp_TypeOfNum)) ) THEN
    CALL ShowSevereError('SetupZoneInternalGain: developer error, trapped inconsistent internal gains object types' &
                         // ' sent to SetupZoneInternalGain')
    CALL ShowContinueError('Object type character = '//Trim(cComponentObject) )
    CALL ShowContinueError('Type of Num object name = '//TRIM(ZoneIntGainDeviceTypes(IntGainComp_TypeOfNum)) )
    RETURN
  ENDIF


  DO IntGainsNum = 1, ZoneIntGain(ZoneNum)%NumberOfDevices
    IF ((ZoneIntGain(ZoneNum)%Device(IntGainsNum)%CompObjectType == UpperCaseObjectType) &
        .AND. (ZoneIntGain(ZoneNum)%Device(IntGainsNum)%CompTypeOfNum ==  IntGainComp_TypeOfNum)) THEN
      FoundIntGainsType = .TRUE.
      IF (ZoneIntGain(ZoneNum)%Device(IntGainsNum)%CompObjectName == UpperCaseObjectName) THEN
        FoundDuplicate = .TRUE.
        EXIT
      ENDIF
    ENDIF
  ENDDO

  IF (FoundDuplicate) THEN
    CALL ShowSevereError('SetupZoneInternalGain: developer error, trapped duplicate internal gains sent to SetupZoneInternalGain')
    CALL ShowContinueError('The duplicate object user name ='//TRIM(cComponentName) )
    CALL ShowContinueError('The duplicate object type = '//TRIM(cComponentObject) )
    CALL ShowContinueError('This internal gain will not be modeled, and the simulation continues')
    RETURN
  ENDIF

  IF (ZoneIntGain(ZoneNum)%NumberOfDevices == 0) THEN
    ALLOCATE( ZoneIntGain(ZoneNum)%Device(DeviceAllocInc) )
    ZoneIntGain(ZoneNum)%NumberOfDevices   = 1
    ZoneIntGain(ZoneNum)%MaxNumberOfDevices = DeviceAllocInc
  ELSE
    IF (ZoneIntGain(ZoneNum)%NumberOfDevices +1 > ZoneIntGain(ZoneNum)%MaxNumberOfDevices) THEN
      ALLOCATE(TempGenDeviceIntGainsArr(ZoneIntGain(ZoneNum)%MaxNumberOfDevices + DeviceAllocInc) )
      TempGenDeviceIntGainsArr(1:ZoneIntGain(ZoneNum)%NumberOfDevices)  &
         = ZoneIntGain(ZoneNum)%Device(1:ZoneIntGain(ZoneNum)%NumberOfDevices)
      DEALLOCATE(ZoneIntGain(ZoneNum)%Device)
      ALLOCATE(ZoneIntGain(ZoneNum)%Device(ZoneIntGain(ZoneNum)%MaxNumberOfDevices + DeviceAllocInc) )
      ZoneIntGain(ZoneNum)%MaxNumberOfDevices = ZoneIntGain(ZoneNum)%MaxNumberOfDevices + DeviceAllocInc
      ZoneIntGain(ZoneNum)%Device(1:ZoneIntGain(ZoneNum)%NumberOfDevices) &
         = TempGenDeviceIntGainsArr(1:ZoneIntGain(ZoneNum)%NumberOfDevices)
      DEALLOCATE(TempGenDeviceIntGainsArr)
    ENDIF
    ZoneIntGain(ZoneNum)%NumberOfDevices = ZoneIntGain(ZoneNum)%NumberOfDevices + 1
  ENDIF

  ZoneIntGain(ZoneNum)%Device(ZoneIntGain(ZoneNum)%NumberOfDevices)%CompObjectType = UpperCaseObjectType
  ZoneIntGain(ZoneNum)%Device(ZoneIntGain(ZoneNum)%NumberOfDevices)%CompObjectName = UpperCaseObjectName
  ZoneIntGain(ZoneNum)%Device(ZoneIntGain(ZoneNum)%NumberOfDevices)%CompTypeOfNum  = IntGainComp_TypeOfNum

  ! note pointer assignments in code below!
  IF (PRESENT(ConvectionGainRate)) THEN
    ZoneIntGain(ZoneNum)%Device(ZoneIntGain(ZoneNum)%NumberOfDevices)%PtrConvectGainRate => ConvectionGainRate
  ELSE
    ZoneIntGain(ZoneNum)%Device(ZoneIntGain(ZoneNum)%NumberOfDevices)%PtrConvectGainRate => ZeroPointerVal
  ENDIF

  IF (PRESENT(ReturnAirConvectionGainRate)) THEN
    ZoneIntGain(ZoneNum)%Device(ZoneIntGain(ZoneNum)%NumberOfDevices)%PtrReturnAirConvGainRate => ReturnAirConvectionGainRate
  ELSE
    ZoneIntGain(ZoneNum)%Device(ZoneIntGain(ZoneNum)%NumberOfDevices)%PtrReturnAirConvGainRate => ZeroPointerVal
  ENDIF

  IF (PRESENT(ThermalRadiationGainRate)) THEN
    ZoneIntGain(ZoneNum)%Device(ZoneIntGain(ZoneNum)%NumberOfDevices)%PtrRadiantGainRate => ThermalRadiationGainRate
  ELSE
    ZoneIntGain(ZoneNum)%Device(ZoneIntGain(ZoneNum)%NumberOfDevices)%PtrRadiantGainRate => ZeroPointerVal
  ENDIF

  IF (PRESENT(LatentGainRate)) THEN
    ZoneIntGain(ZoneNum)%Device(ZoneIntGain(ZoneNum)%NumberOfDevices)%PtrLatentGainRate => LatentGainRate
  ELSE
    ZoneIntGain(ZoneNum)%Device(ZoneIntGain(ZoneNum)%NumberOfDevices)%PtrLatentGainRate => ZeroPointerVal
  ENDIF

  IF (PRESENT(ReturnAirLatentGainRate)) THEN
    ZoneIntGain(ZoneNum)%Device(ZoneIntGain(ZoneNum)%NumberOfDevices)%PtrReturnAirLatentGainRate => ReturnAirLatentGainRate
  ELSE
    ZoneIntGain(ZoneNum)%Device(ZoneIntGain(ZoneNum)%NumberOfDevices)%PtrReturnAirLatentGainRate => ZeroPointerVal
  ENDIF

  IF (PRESENT(CarbonDioxideGainRate)) THEN
    ZoneIntGain(ZoneNum)%Device(ZoneIntGain(ZoneNum)%NumberOfDevices)%PtrCarbonDioxideGainRate => CarbonDioxideGainRate
  ELSE
    ZoneIntGain(ZoneNum)%Device(ZoneIntGain(ZoneNum)%NumberOfDevices)%PtrCarbonDioxideGainRate => ZeroPointerVal
  ENDIF

  IF (PRESENT(GenericContamGainRate)) THEN
    ZoneIntGain(ZoneNum)%Device(ZoneIntGain(ZoneNum)%NumberOfDevices)%PtrGenericContamGainRate => GenericContamGainRate
  ELSE
    ZoneIntGain(ZoneNum)%Device(ZoneIntGain(ZoneNum)%NumberOfDevices)%PtrGenericContamGainRate => ZeroPointerVal
  ENDIF

  RETURN

END SUBROUTINE SetupZoneInternalGain
