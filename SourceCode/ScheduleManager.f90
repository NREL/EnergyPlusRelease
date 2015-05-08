MODULE ScheduleManager
          ! Module containing the Schedule Manager routines

          ! MODULE INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   September 1997
          !       MODIFIED       January 2003 -- added sub-hourly schedule possibility (and interval scheduling)
          !                      J. Glazer January 2005 -- added Schedule:File
          !                      Michael Wetter February 2010 -- added Schedule for external Interface
          !                      L Lawrie - October 2012 - added sub-hourly option for Schedule:File
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS MODULE:
          ! To provide the capabilities of getting the schedule data from the input,
          ! validating it, and storing it in such a manner that the schedule manager
          ! can provide the scheduling value needs for the simulation.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! Proposal for Schedule Manager in EnergyPlus (Rick Strand)



          ! OTHER NOTES:
          !
          !

          ! USE STATEMENTS:
  USE DataPrecisionGlobals
  USE DataGlobals, ONLY: MaxNameLength, HourOfDay, OutputFileInits, NumOfTimeStepInHour, MinutesPerTimeStep, TimeStep,  &
    OutputFileDebug
  USE DataInterfaces, ONLY: ShowSevereError, ShowWarningError, ShowFatalError, ShowContinueError,   &
                        SetupOutputVariable, ShowMessage
  USE DataEnvironment, ONLY: MonthTomorrow, DayOfWeek, DayOfWeekTomorrow, &
                            DayOfMonthTomorrow,                     &
                            HolidayIndex, HolidayIndexTomorrow,                 &
                            DSTIndicator

          ! Use statements for access to subroutines in other modules

  IMPLICIT NONE         ! Enforce explicit typing of all variables
  PRIVATE

          !MODULE PARAMETER DEFINITIONS
  INTEGER, PARAMETER :: MaxDayTypes=12
  CHARACTER(len=1), PARAMETER :: Blank=' '
  CHARACTER(len=*), PARAMETER, DIMENSION(MaxDayTypes) :: ValidDayTypes= &
                                (/"Sunday         ","Monday         ","Tuesday        ",  &
                                  "Wednesday      ","Thursday       ","Friday         ",  &
                                  "Saturday       ","Holiday        ","SummerDesignDay",  &
                                  "WinterDesignDay","CustomDay1     ","CustomDay2     "/)

  INTEGER, PARAMETER :: NumScheduleTypeLimitUnitTypes = 14
  CHARACTER(len=*), PARAMETER, DIMENSION(NumScheduleTypeLimitUnitTypes) :: ScheduleTypeLimitUnitTypes=  &
       (/'Dimensionless               ',&
         'Temperature                 ',&
         'DeltaTemperature            ',&
         'PrecipitationRate           ',&
         'Angle                       ',&
         'ConvectionCoefficient       ',&
         'ActivityLevel               ',&
         'Velocity                    ',&
         'Capacity                    ',&
         'Power                       ',&
         'Availability                ',&
         'Percent                     ',&
         'Control                     ',&
         'Mode                        '/)

  INTEGER, PARAMETER :: ScheduleInput_year    = 1
  INTEGER, PARAMETER :: ScheduleInput_compact = 2
  INTEGER, PARAMETER :: ScheduleInput_file    = 3
  INTEGER, PARAMETER :: ScheduleInput_constant= 4
  INTEGER, PARAMETER :: ScheduleInput_external= 5

          ! DERIVED TYPE DEFINITIONS
  TYPE ScheduleTypeData
       CHARACTER(len=MaxNameLength) :: Name = Blank ! Schedule Type Name
       LOGICAL :: Limited     = .false. ! True if this Schedule Type has limits
       REAL(r64) :: Minimum   = 0.0d0   ! Minimum for limited schedule
       REAL(r64) :: Maximum   = 0.0d0   ! Maximum for limited schedule
       LOGICAL :: IsReal      = .true.  ! True if this is a "real" schedule, false if integer
       INTEGER :: UnitType    = 0       ! reference ScheduleTypeLimit table
  END TYPE

  TYPE DayScheduleData
       CHARACTER(len=MaxNameLength) :: Name = Blank ! Day Schedule Name
       INTEGER :: ScheduleTypePtr           = 0     ! Index of Schedule Type
       LOGICAL :: IntervalInterpolated   = .false. ! Indicator for interval interpolation. If not "interpolated", False.  Else True
       LOGICAL :: Used                   = .false. ! Indicator for this schedule being "used".
       REAL(r64), ALLOCATABLE, DIMENSION(:,:) :: TSValue     ! Value array by simulation timestep
       REAL(r64) :: TSValMax               = 0.0D0   !maximum of all TSValue's
       REAL(r64) :: TSValMin               = 0.0D0   !minimum of all TSValue's
  END TYPE

  TYPE WeekScheduleData
       CHARACTER(len=MaxNameLength) :: Name = Blank   ! Week Schedule Name
       LOGICAL :: Used                   = .false. ! Indicator for this schedule being "used".
       INTEGER :: DaySchedulePointer(MaxDayTypes) = 0 ! Index of Day Schedule
  END TYPE

  TYPE :: ScheduleData
       CHARACTER(len=MaxNameLength) :: Name = Blank   ! Schedule Name
       INTEGER :: ScheduleTypePtr           = 0       ! Index of Schedule Type
       INTEGER :: WeekSchedulePointer(366)  = 0       ! one created for each day of possible simulation
       INTEGER :: SchType                   = 0       ! what kind of object has been input.
       LOGICAL :: Used                   = .false. ! Indicator for this schedule being "used".
       LOGICAL :: MaxMinSet              = .false.    ! Max/min values have been stored for this schedule
       REAL(r64) :: MaxValue             = 0.0D0     ! Maximum value for this schedule
       REAL(r64) :: MinValue             = 0.0D0     ! Minimum value for this schedule
       REAL(r64) :: CurrentValue         = 0.0D0     ! For Reporting
       LOGICAL   :: EMSActuatedOn        = .FALSE. ! indicates if EMS computed
       REAL(r64) :: EMSValue             = 0.0D0
  END TYPE


          ! INTERFACE BLOCK SPECIFICATIONS
INTERFACE CheckScheduleValueMinMax
  MODULE PROCEDURE dCheckScheduleValueMinMax1, dCheckScheduleValueMinMax2, rCheckScheduleValueMinMax1, rCheckScheduleValueMinMax2
END INTERFACE CheckScheduleValueMinMax

INTERFACE CheckScheduleValue
  MODULE PROCEDURE rCheckScheduleValue, iCheckScheduleValue
END INTERFACE CheckScheduleValue

INTERFACE CheckDayScheduleValueMinMax
  MODULE PROCEDURE rCheckDayScheduleValueMinMax, sCheckDayScheduleValueMinMax
END INTERFACE CheckDayScheduleValueMinMax

          ! MODULE VARIABLE DECLARATIONS:

!Integer Variables for the Module
  INTEGER :: NumScheduleTypes  =0
  INTEGER :: NumDaySchedules   =0
  INTEGER :: NumWeekSchedules  =0
  INTEGER :: NumSchedules      =0

!Logical Variables for Module
  LOGICAL :: ScheduleInputProcessed = .false.            ! This is false until the Schedule Input has been processed.
  LOGICAL :: ScheduleDSTSFileWarningIssued = .false.

!Derived Types Variables

  TYPE (ScheduleTypeData), ALLOCATABLE, DIMENSION(:) :: ScheduleType   ! Allowed Schedule Types
  TYPE (DayScheduleData),  ALLOCATABLE, DIMENSION(:) :: DaySchedule    ! Day Schedule Storage
  TYPE (WeekScheduleData), ALLOCATABLE, DIMENSION(:) :: WeekSchedule   ! Week Schedule Storage
  TYPE (ScheduleData),     ALLOCATABLE, DIMENSION(:) :: Schedule       ! Schedule Storage

PRIVATE ProcessScheduleInput
PUBLIC  GetScheduleIndex
PUBLIC  GetDayScheduleIndex
PUBLIC  GetScheduleValuesForDay
PUBLIC  GetSingleDayScheduleValues
PUBLIC  GetCurrentScheduleValue
PUBLIC  LookUpScheduleValue
PUBLIC  CheckScheduleValueMinMax
PUBLIC  GetScheduleMinValue
PUBLIC  GetScheduleMaxValue
PUBLIC  CheckDayScheduleValueMinMax
PUBLIC  CheckScheduleValue
PUBLIC  HasFractionalScheduleValue
PUBLIC  ReportScheduleValues
PUBLIC  ScheduleAverageHoursPerWeek
PUBLIC  GetScheduleType
PUBLIC  GetScheduleName
PRIVATE ReportScheduleDetails
PUBLIC  ReportOrphanSchedules
PUBLIC  GetNumberOfSchedules
PUBLIC  UpdateScheduleValues
PUBLIC  ExternalInterfaceSetSchedule

CONTAINS

! MODULE SUBROUTINES:
!*************************************************************************

SUBROUTINE ProcessScheduleInput

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   September 1997
          !       MODIFIED       Rui Zhang February 2010
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine processes the schedules input for EnergyPlus.

          ! METHODOLOGY EMPLOYED:
          ! Uses the standard get routines in the InputProcessor.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: GetNumObjectsFound, GetObjectItem, FindItemInList, ProcessNumber, VerifyName, &
                            GetObjectDefMaxArgs, SameString, FindItem
  USE General,        ONLY: ProcessDateString, JulianDay, RoundSigDigits, TrimSigDigits
  USE DataIPShortCuts
  USE DataStringGlobals, ONLY: CharTab,CharComma,CharSpace,CharSemicolon
  Use DataGlobals,    ONLY: AnyEnergyManagementSystemInModel
  Use DataInterfaces, ONLY: SetupEMSActuator
  USE DataSystemVariables, ONLY: iASCII_CR, iUnicode_end, TempFullFileName,CheckForActualFileName

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER :: RoutineName='ProcessScheduleInput: '

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER,EXTERNAL :: GetNewUnitNumber  ! Function to call if file not opened

  INTEGER DaysInYear(366)
  INTEGER UnitNumber
  INTEGER, EXTERNAL :: FindUnitNumber
  INTEGER LoopIndex
  INTEGER InLoopIndex
  INTEGER DayIndex,WeekIndex
  CHARACTER(Len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: Alphas
  CHARACTER(Len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: cAlphaFields
  CHARACTER(Len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: cNumericFields
  REAL(r64), ALLOCATABLE, DIMENSION(:) :: Numbers
  LOGICAL, ALLOCATABLE, DIMENSION(:) :: lAlphaBlanks
  LOGICAL, ALLOCATABLE, DIMENSION(:) :: lNumericBlanks
  INTEGER NumAlphas
  INTEGER NumNumbers
  INTEGER Status
  INTEGER StartMonth,StartDay,EndMonth,EndDay
  INTEGER StartPointer,EndPointer
  INTEGER NumPointer
  INTEGER Count
  INTEGER CheckIndex
  LOGICAL :: ErrorsFound=.false.
  LOGICAL       :: IsNotOK       ! Flag to verify name
  LOGICAL       :: IsBlank       ! Flag for blank name
  LOGICAL NumErrorFlag
  INTEGER SchedTypePtr
  CHARACTER(len=20) CFld         ! Character field for error message
!  CHARACTER(len=20) CFld1        ! Character field for error message
  INTEGER NumHrDaySchedules      ! Number of "hourly" dayschedules
  INTEGER NumIntDaySchedules     ! Number of "interval" dayschedules
  INTEGER NumExternalInterfaceSchedules ! Number of "PtolemyServer ExternalInterface" "compact" Schedules
  INTEGER NumExternalInterfaceFunctionalMockupUnitImportSchedules ! Number of "FunctionalMockupUnitImport ExternalInterface"
                                                            ! "compact" Schedules ! added for FMU Import
  INTEGER NumExternalInterfaceFunctionalMockupUnitExportSchedules ! Number of "FunctionalMockupUnitExport ExternalInterface"
                                                            ! "compact" Schedules ! added for FMU Export
  INTEGER NumLstDaySchedules     ! Number of "list" dayschedules
  INTEGER NumRegDaySchedules     ! Number of hourly+interval+list dayschedules
  INTEGER NumRegWeekSchedules    ! Number of "regular" Weekschedules
  INTEGER NumRegSchedules        ! Number of "regular" Schedules
  INTEGER NumCptWeekSchedules    ! Number of "compact" WeekSchedules
  INTEGER NumCptSchedules        ! Number of "compact" Schedules
  INTEGER NumCommaFileSchedules  ! Number of Schedule:File schedules
  INTEGER NumConstantSchedules   ! Number of "constant" schedules
  INTEGER TS                     ! Counter for Num Of Time Steps in Hour
  INTEGER Hr                     ! Hour Counter
  REAL(r64), ALLOCATABLE, DIMENSION(:,:) :: MinuteValue  ! Temporary for processing interval schedules
  LOGICAL, ALLOCATABLE, DIMENSION(:,:) :: SetMinuteValue  ! Temporary for processing interval schedules
  INTEGER NumFields
  INTEGER SCount
!  LOGICAL RptSchedule
  INTEGER RptLevel
  INTEGER CurMinute
  INTEGER MinutesPerItem
  INTEGER NumExpectedItems
  INTEGER MaxNums
  INTEGER MaxAlps
  INTEGER AddWeekSch
  INTEGER AddDaySch
  LOGICAL AllDays(MaxDayTypes)
  LOGICAL Thesedays(MaxDayTypes)
  LOGICAL ErrorHere
  INTEGER SchNum
  INTEGER WkCount
  INTEGER DyCount
  INTEGER NumField
  INTEGER PDateType
  INTEGER PWeekDay
  INTEGER ThruField
  CHARACTER(len=25) ExtraField
  INTEGER UntilFld
  INTEGER xxcount
!  REAL(r64) tempval
  LOGICAL :: FullYearSet=.false.
  CHARACTER(len=MaxNameLength) :: CurrentThrough=blank
  CHARACTER(len=MaxNameLength) :: LastFor=blank
  CHARACTER(len=220) :: errmsg=blank
  integer kdy
  LOGICAL :: FileExists
  ! for SCHEDULE:FILE
  REAL(r64), ALLOCATABLE, DIMENSION(:) :: hourlyFileValues
  INTEGER :: SchdFile
  INTEGER :: colCnt
  INTEGER :: rowCnt
  INTEGER :: wordStart
  INTEGER :: wordEnd
  INTEGER :: sepPos
  CHARACTER(len=1000) :: LineIn
  CHARACTER(len=MaxNameLength) :: subString
  REAL(r64) :: columnValue
  INTEGER :: read_stat
  INTEGER :: iDay
  INTEGER :: hDay
  INTEGER :: jHour
  INTEGER :: kDayType
  REAL(r64)    :: curHrVal
  LOGICAL :: errflag
  INTEGER :: sPos
  CHARACTER(len=MaxNameLength) :: CurrentModuleObject  ! for ease in getting objects
  INTEGER :: MaxNums1
  LOGICAL :: StripCR  ! If true, strip last character (<cr> off each schedule:file line)
  INTEGER :: endLine
  CHARACTER(len=1) :: ColumnSep
  LOGICAL :: firstLine
  LOGICAL :: FileIntervalInterpolated
  INTEGER :: rowLimitCount
  INTEGER :: skiprowCount
  INTEGER :: curcolCount
  INTEGER :: numHourlyValues
  INTEGER :: numerrors
  INTEGER :: ifld
  INTEGER :: hrLimitCount


  MaxNums=1 ! Need at least 1 number because it's used as a local variable in the Schedule Types loop
  MaxAlps=0

  CurrentModuleObject='ScheduleTypeLimits'
  NumScheduleTypes=GetNumObjectsFound(CurrentModuleObject)
  IF (NumScheduleTypes > 0) THEN
    CALL GetObjectDefMaxArgs(CurrentModuleObject,Count,NumAlphas,NumNumbers)
    MaxNums=MAX(MaxNums,NumNumbers)
    MaxAlps=MAX(MaxAlps,NumAlphas)
  ENDIF
  CurrentModuleObject='Schedule:Day:Hourly'
  NumHrDaySchedules=GetNumObjectsFound(CurrentModuleObject)
  IF (NumHrDaySchedules > 0) THEN
    CALL GetObjectDefMaxArgs(CurrentModuleObject,Count,NumAlphas,NumNumbers)
    MaxNums=MAX(MaxNums,NumNumbers)
    MaxAlps=MAX(MaxAlps,NumAlphas)
  ENDIF
  CurrentModuleObject='Schedule:Day:Interval'
  NumIntDaySchedules=GetNumObjectsFound(CurrentModuleObject)
  IF (NumIntDaySchedules > 0) THEN
    CALL GetObjectDefMaxArgs(CurrentModuleObject,Count,NumAlphas,NumNumbers)
    MaxNums=MAX(MaxNums,NumNumbers)
    MaxAlps=MAX(MaxAlps,NumAlphas)
  ENDIF
  CurrentModuleObject='Schedule:Day:List'
  NumLstDaySchedules=GetNumObjectsFound(CurrentModuleObject)
  IF (NumLstDaySchedules > 0) THEN
    CALL GetObjectDefMaxArgs(CurrentModuleObject,Count,NumAlphas,NumNumbers)
    MaxNums=MAX(MaxNums,NumNumbers)
    MaxAlps=MAX(MaxAlps,NumAlphas)
  ENDIF
  CurrentModuleObject='Schedule:Week:Daily'
  NumRegWeekSchedules=GetNumObjectsFound(CurrentModuleObject)
  IF (NumRegWeekSchedules > 0) THEN
    CALL GetObjectDefMaxArgs(CurrentModuleObject,Count,NumAlphas,NumNumbers)
    MaxNums=MAX(MaxNums,NumNumbers)
    MaxAlps=MAX(MaxAlps,NumAlphas)
  ENDIF
  CurrentModuleObject='Schedule:Week:Compact'
  NumCptWeekSchedules=GetNumObjectsFound(CurrentModuleObject)
  IF (NumCptWeekSchedules > 0) THEN
    CALL GetObjectDefMaxArgs(CurrentModuleObject,Count,NumAlphas,NumNumbers)
    MaxNums=MAX(MaxNums,NumNumbers)
    MaxAlps=MAX(MaxAlps,NumAlphas)
  ENDIF
  CurrentModuleObject='Schedule:Year'
  NumRegSchedules=GetNumObjectsFound(CurrentModuleObject)
  IF (NumRegSchedules > 0) THEN
    CALL GetObjectDefMaxArgs(CurrentModuleObject,Count,NumAlphas,NumNumbers)
    MaxNums=MAX(MaxNums,NumNumbers)
    MaxAlps=MAX(MaxAlps,NumAlphas)
  ENDIF
  CurrentModuleObject='Schedule:Compact'
  NumCptSchedules=GetNumObjectsFound(CurrentModuleObject)
  IF (NumCptSchedules > 0) THEN
    CALL GetObjectDefMaxArgs(CurrentModuleObject,Count,NumAlphas,NumNumbers)
    MaxNums=MAX(MaxNums,NumNumbers)
    MaxAlps=MAX(MaxAlps,NumAlphas+1)
  ENDIF
  CurrentModuleObject='Schedule:File'
  NumCommaFileSchedules = GetNumObjectsFound(CurrentModuleObject)
  IF (NumCommaFileSchedules > 0) THEN
    CALL GetObjectDefMaxArgs(CurrentModuleObject,Count,NumAlphas,NumNumbers)
    MaxNums=MAX(MaxNums,NumNumbers)
    MaxAlps=MAX(MaxAlps,NumAlphas)
  ENDIF
  CurrentModuleObject='Schedule:Constant'
  NumConstantSchedules = GetNumObjectsFound(CurrentModuleObject)
  IF (NumConstantSchedules > 0) THEN
    CALL GetObjectDefMaxArgs(CurrentModuleObject,Count,NumAlphas,NumNumbers)
    MaxNums=MAX(MaxNums,NumNumbers)
    MaxAlps=MAX(MaxAlps,NumAlphas)
  ENDIF
  CurrentModuleObject='ExternalInterface:Schedule'
  NumExternalInterfaceSchedules=GetNumObjectsFound(CurrentModuleObject)
  ! added for FMI
  IF (NumCptSchedules > 0) THEN
    CALL GetObjectDefMaxArgs(CurrentModuleObject,Count,NumAlphas,NumNumbers)
    MaxNums=MAX(MaxNums,NumNumbers)
    MaxAlps=MAX(MaxAlps,NumAlphas+1)
  ENDIF
  ! added for FMU Import
  CurrentModuleObject='ExternalInterface:FunctionalMockupUnitImport:To:Schedule'
  NumExternalInterfaceFunctionalMockupUnitImportSchedules=GetNumObjectsFound(CurrentModuleObject)
  IF (NumCptSchedules > 0) THEN
    CALL GetObjectDefMaxArgs(CurrentModuleObject,Count,NumAlphas,NumNumbers)
    MaxNums=MAX(MaxNums,NumNumbers)
    MaxAlps=MAX(MaxAlps,NumAlphas+1)
  ENDIF
  ! added for FMU Export
  CurrentModuleObject='ExternalInterface:FunctionalMockupUnitExport:To:Schedule'
  NumExternalInterfaceFunctionalMockupUnitExportSchedules=GetNumObjectsFound(CurrentModuleObject)
  IF (NumCptSchedules > 0) THEN
    CALL GetObjectDefMaxArgs(CurrentModuleObject,Count,NumAlphas,NumNumbers)
    MaxNums=MAX(MaxNums,NumNumbers)
    MaxAlps=MAX(MaxAlps,NumAlphas+1)
  ENDIF
  CurrentModuleObject='Output:Schedules'
  CALL GetObjectDefMaxArgs(CurrentModuleObject,Count,NumAlphas,NumNumbers)
  MaxNums=MAX(MaxNums,NumNumbers)
  MaxAlps=MAX(MaxAlps,NumAlphas)

  ALLOCATE(Alphas(MaxAlps)) ! Maximum Alphas possible
  Alphas=' '
  ALLOCATE(cAlphaFields(MaxAlps))
  cAlphaFields=' '
  ALLOCATE(cNumericFields(MaxNums))
  cNumericFields=' '
  ALLOCATE(Numbers(MaxNums)) ! Maximum Numbers possible
  Numbers=0.0d0
  ALLOCATE(lAlphaBlanks(MaxAlps))
  lAlphaBlanks=.true.
  ALLOCATE(lNumericBlanks(MaxNums))
  lNumericBlanks=.true.

  ! Prescan to determine extra day and week schedules due to compact schedule input
  AddWeekSch=0
  AddDaySch=0
  CurrentModuleObject='Schedule:Compact'
  MaxNums1=0
  DO LoopIndex=1,NumCptSchedules
    CALL GetObjectItem(CurrentModuleObject,LoopIndex,Alphas,NumAlphas,Numbers,NumNumbers,Status)
    ! # 'THROUGH" => Number of additional week schedules
    ! # 'FOR' => Number of additional day schedules
    DO Count=3,NumAlphas
      IF (Alphas(Count)(1:7) == 'THROUGH') AddWeekSch=AddWeekSch+1
      IF (Alphas(Count)(1:3) == 'FOR') AddDaySch=AddDaySch+1
      IF (Alphas(Count)(1:5) == 'UNTIL') MaxNums1=MaxNums1+1
    ENDDO
  ENDDO
  IF (MaxNums1 > MaxNums) THEN
    MaxNums=MaxNums1
    DEALLOCATE(cNumericFields)
    DEALLOCATE(Numbers)
    DEALLOCATE(lNumericBlanks)
    ALLOCATE(cNumericFields(MaxNums))
    cNumericFields=' '
    ALLOCATE(Numbers(MaxNums)) ! Maximum Numbers possible
    Numbers=0.0d0
    ALLOCATE(lNumericBlanks(MaxNums))
    lNumericBlanks=.true.
  ENDIF
  ! add week and day schedules for each FILE:COMMA schedule
  AddWeekSch = AddWeekSch + NumCommaFileSchedules * 366 !number of days/year because need a week for each day
  AddDaySch = AddDaySch + NumCommaFileSchedules * 366  !number of days/year
  AddWeekSch = AddWeekSch + NumConstantSchedules
  AddDaySch = AddDaySch + NumConstantSchedules
  ! add week and day schedules for each ExternalInterface:Schedule schedule
  AddWeekSch = AddWeekSch + NumExternalInterfaceSchedules * 366 !number of days/year because need a week for each day
  AddDaySch = AddDaySch + NumExternalInterfaceSchedules  !one day schedule for ExternalInterface to update during run time
  ! added for FMU Import
  ! add week and day schedules for each ExternalInterface:FunctionalMockupUnitImport:Schedule
  AddWeekSch = AddWeekSch + NumExternalInterfaceFunctionalMockupUnitImportSchedules * 366 !number of days/year
                                                                                    !because need a week for each day
  AddDaySch = AddDaySch + NumExternalInterfaceFunctionalMockupUnitImportSchedules  ! one day schedule for ExternalInterface
                                                                             ! to update during run time
  ! added for FMU Export
  ! add week and day schedules for each ExternalInterface:FunctionalMockupUnitExport:Schedule
  AddWeekSch = AddWeekSch + NumExternalInterfaceFunctionalMockupUnitExportSchedules * 366 !number of days/year
                                                                                    !because need a week for each day
  AddDaySch = AddDaySch + NumExternalInterfaceFunctionalMockupUnitExportSchedules  ! one day schedule for ExternalInterface
                                                                             ! to update during run time


  ! include additional schedules in with count
  NumRegDaySchedules=NumHrDaySchedules+NumIntDaySchedules+NumLstDaySchedules
  NumDaySchedules=NumRegDaySchedules+AddDaySch
  NumWeekSchedules=NumRegWeekSchedules+NumCptWeekSchedules+AddWeekSch
  NumSchedules = NumRegSchedules + NumCptSchedules + NumCommaFileSchedules &
                   + NumConstantSchedules + NumExternalInterfaceSchedules &
                   + NumExternalInterfaceFunctionalMockupUnitImportSchedules &
                   + NumExternalInterfaceFunctionalMockupUnitExportSchedules

!!  Most initializations in the schedule data structures are taken care of in
!!  the definitions (see above)

  ALLOCATE (ScheduleType(0:NumScheduleTypes))

  ALLOCATE (DaySchedule(0:NumDaySchedules))
  !    Initialize
  DO LoopIndex=0,NumDaySchedules
    ALLOCATE(DaySchedule(LoopIndex)%TSValue(24,NumOfTimeStepInHour))
    DO Count=1,24
      DO TS=1,NumOfTimeStepInHour
        DaySchedule(LoopIndex)%TSValue(Count,TS)=0.0d0
      ENDDO
    ENDDO
  ENDDO

  ALLOCATE (WeekSchedule(0:NumWeekSchedules))

  ALLOCATE (Schedule(-1:NumSchedules))
  Schedule(-1)%ScheduleTypePtr=-1
  Schedule(-1)%WeekSchedulePointer=1
  Schedule(0)%ScheduleTypePtr=0
  Schedule(0)%WeekSchedulePointer=0

  UnitNumber=FindUnitNumber('eplusout.audit')
  WRITE(UnitNumber,*) ' Processing Schedule Input -- Start'

!!! Get Schedule Types

  CurrentModuleObject='ScheduleTypeLimits'
  DO LoopIndex=1,NumScheduleTypes
    CALL GetObjectItem(CurrentModuleObject,LoopIndex,Alphas,NumAlphas,Numbers,NumNumbers,Status,  &
                   AlphaBlank=lAlphaBlanks,NumBlank=lNumericBlanks,  &
                   AlphaFieldnames=cAlphaFields,NumericFieldNames=cNumericFields)
    IsNotOK=.false.
    IsBlank=.false.
    CALL VerifyName(Alphas(1),ScheduleType(1:NumScheduleTypes)%Name,LoopIndex-1,IsNotOK,IsBlank,TRIM(CurrentModuleObject)//' Name')
    IF (IsNotOK) THEN
      ErrorsFound=.true.
      IF (IsBlank) Alphas(1)='xxxxx'
    ENDIF
    ScheduleType(LoopIndex)%Name=Alphas(1)
    IF (lNumericBlanks(1) .or. lNumericBlanks(2)) THEN
      ScheduleType(LoopIndex)%Limited=.false.
    ELSEIF (.not. lNumericBlanks(1) .and. .not. lNumericBlanks(2)) THEN
      ScheduleType(LoopIndex)%Limited=.true.
    ENDIF
    IF (.not. lNumericBlanks(1)) THEN
      ScheduleType(LoopIndex)%Minimum=Numbers(1)
    ENDIF
    IF (.not. lNumericBlanks(2)) THEN
      ScheduleType(LoopIndex)%Maximum=Numbers(2)
    ENDIF
    IF (ScheduleType(LoopIndex)%Limited) THEN
      IF (Alphas(2) == 'DISCRETE' .or. Alphas(2) == 'INTEGER') THEN
        ScheduleType(LoopIndex)%IsReal=.false.
      ELSE
        IF (Alphas(2) /= 'CONTINUOUS' .and. Alphas(2) /= 'REAL') THEN
          CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//'="'//  &
             TRIM(ScheduleType(LoopIndex)%Name)//'", invalid '//TRIM(cAlphaFields(2))//'='//TRIM(Alphas(2)))
          ErrorsFound=.true.
        ENDIF
        ScheduleType(LoopIndex)%IsReal=.true.
      ENDIF
    ENDIF
    IF (NumAlphas .ge. 3) THEN
      IF (.not. lAlphaBlanks(3)) THEN
        ScheduleType(LoopIndex)%UnitType = FindItem(Alphas(3), &
                            ScheduleTypeLimitUnitTypes,NumScheduleTypeLimitUnitTypes)
        IF (ScheduleType(LoopIndex)%UnitType .eq. 0) THEN
          CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//'="' // TRIM(Alphas(1)) //   &
                              '", '//trim(cAlphaFields(3))//'="' // TRIM(Alphas(3)) //'" is invalid.')
        END IF
      END IF
    END IF
    IF (ScheduleType(LoopIndex)%Limited) THEN
      IF (ScheduleType(LoopIndex)%Minimum > ScheduleType(LoopIndex)%Maximum) THEN
        IF (ScheduleType(LoopIndex)%IsReal) THEN
          CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="' // TRIM(Alphas(1)) //   &
                              '", '//trim(cNumericFields(1))//' ['//trim(RoundSigDigits(ScheduleType(LoopIndex)%Minimum,2))//  &
                          '] > '//trim(cNumericFields(2))//' ['//trim(RoundSigDigits(ScheduleType(LoopIndex)%Maximum,2))//'].')
          CALL ShowContinueError('  Other warning/severes about schedule values may appear.')
        ELSE
          CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="' // TRIM(Alphas(1)) //   &
                              '", '//trim(cNumericFields(1))//' ['//trim(RoundSigDigits(ScheduleType(LoopIndex)%Minimum,0))//  &
                          '] > '//trim(cNumericFields(2))//' ['//trim(RoundSigDigits(ScheduleType(LoopIndex)%Maximum,0))//'].')
          CALL ShowContinueError('  Other warning/severes about schedule values may appear.')
        ENDIF
      ENDIF
    ENDIF
  ENDDO

!!! Get Day Schedules (all types)

!!!=> Get "DAYSCHEDULE" (Hourly)

  Count=0
  CurrentModuleObject='Schedule:Day:Hourly'
  DO LoopIndex=1,NumHrDaySchedules
    CALL GetObjectItem(CurrentModuleObject,LoopIndex,Alphas,NumAlphas,Numbers,NumNumbers,Status,  &
                   AlphaBlank=lAlphaBlanks,NumBlank=lNumericBlanks,  &
                   AlphaFieldnames=cAlphaFields,NumericFieldNames=cNumericFields)
    IsNotOK=.false.
    IsBlank=.false.
    CALL VerifyName(Alphas(1),DaySchedule%Name,Count,IsNotOK,IsBlank,TRIM(CurrentModuleObject)//' Name')
    IF (IsNotOK) THEN
      ErrorsFound=.true.
      IF (IsBlank) Alphas(1)='xxxxx'
    ENDIF
    Count=Count+1
    DaySchedule(Count)%Name=Alphas(1)
    ! Validate ScheduleType
    IF (NumScheduleTypes > 0) THEN
      CheckIndex=FindIteminList(Alphas(2),ScheduleType(1:NumScheduleTypes)%Name,NumScheduleTypes)
      IF (CheckIndex == 0) THEN
        IF (.not. lAlphaBlanks(2)) THEN
          CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(Alphas(1))//  &
             '", '//TRIM(cAlphaFields(2))//'="'//TRIM(Alphas(2))//  &
             '" not found -- will not be validated')
        ELSE
          CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(Alphas(1))//  &
             '", Blank '//TRIM(cAlphaFields(2))//' input -- will not be validated.')
        ENDIF
      ELSE
        DaySchedule(Count)%ScheduleTypePtr=CheckIndex
      ENDIF
    ENDIF
    DO HR=1,24
      DaySchedule(Count)%TSValue(Hr,1:NumOfTimeStepInHour)=Numbers(Hr)
    ENDDO
    DaySchedule(Count)%IntervalInterpolated=.false.
    SchedTypePtr=DaySchedule(Count)%ScheduleTypePtr
    IF (ScheduleType(SchedTypePtr)%Limited) THEN
      IF (ANY(DaySchedule(Count)%TSValue < ScheduleType(SchedTypePtr)%Minimum) .or.  &
          ANY(DaySchedule(Count)%TSValue > ScheduleType(SchedTypePtr)%Maximum) ) THEN
          CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(Alphas(1))//  &
             '", Values are outside of range for '//TRIM(cAlphaFields(2))//'='//TRIM(Alphas(2)))
      ENDIF
    ENDIF
    IF (.not. ScheduleType(SchedTypePtr)%IsReal) THEN
         ! Make sure each is integer
      NumErrorFlag=.false.   ! only show error message once
      DO Hr=1,24
        DO TS=1,NumOfTimeStepInHour
          IF (DaySchedule(Count)%TSValue(Hr,TS) /= INT(DaySchedule(Count)%TSValue(Hr,TS)) ) THEN
            IF (.not. NumErrorFlag) THEN
              CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(Alphas(1))//  &
                 '", One or more values are not integer as required by '//TRIM(cAlphaFields(2))//'='//TRIM(Alphas(2)))
              NumErrorFlag=.true.
            ENDIF
          ENDIF
        ENDDO
      ENDDO
    ENDIF
  END DO

  ALLOCATE(MinuteValue(24,60))
  ALLOCATE(SetMinuteValue(24,60))

!!! Get "DaySchedule:Interval"

  CurrentModuleObject='Schedule:Day:Interval'
  DO LoopIndex=1,NumIntDaySchedules
    CALL GetObjectItem(CurrentModuleObject,LoopIndex,Alphas,NumAlphas,Numbers,NumNumbers,Status,  &
                   AlphaBlank=lAlphaBlanks,NumBlank=lNumericBlanks,  &
                   AlphaFieldnames=cAlphaFields,NumericFieldNames=cNumericFields)
    IsNotOK=.false.
    IsBlank=.false.
    CALL VerifyName(Alphas(1),DaySchedule%Name,Count,IsNotOK,IsBlank,TRIM(CurrentModuleObject)//' Name')
    IF (IsNotOK) THEN
      ErrorsFound=.true.
      IF (IsBlank) Alphas(1)='xxxxx'
    ENDIF
    Count=Count+1
    DaySchedule(Count)%Name=Alphas(1)
    ! Validate ScheduleType
    IF (NumScheduleTypes > 0) THEN
      CheckIndex=FindIteminList(Alphas(2),ScheduleType(1:NumScheduleTypes)%Name,NumScheduleTypes)
      IF (CheckIndex == 0) THEN
        IF (.not. lAlphaBlanks(2)) THEN
          CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(Alphas(1))//  &
             '", '//TRIM(cAlphaFields(2))//'="'//TRIM(Alphas(2))//  &
             '" not found -- will not be validated')
        ELSE
          CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(Alphas(1))//  &
             '", Blank '//TRIM(cAlphaFields(2))//' input -- will not be validated.')
        ENDIF
      ELSE
        DaySchedule(Count)%ScheduleTypePtr=CheckIndex
      ENDIF
    ENDIF
    NumFields=NumAlphas-3
! check to see if numfield=0
    IF (NumFields == 0) THEN
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(Alphas(1))//  &
         '", Insufficient data entered for a full schedule day.')
      CALL ShowContinueError('...Number of interval fields = = ['//trim(RoundSigDigits(NumFields))//'].')
      ErrorsFound=.true.
    ENDIF

    CALL ProcessIntervalFields(Alphas(4:),Numbers,NumFields,NumNumbers,MinuteValue,SetMinuteValue,ErrorsFound,  &
                               Alphas(1),TRIM(CurrentModuleObject))
    ! Depending on value of "Interpolate" field, the value for each time step in each hour gets processed:
    IF (Alphas(3) /= 'NO' .and. Alphas(3) /= 'YES') THEN
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(Alphas(1))//  &
         'Invalid value for "'//TRIM(cAlphaFields(3))//'" field="'//TRIM(Alphas(3))//'"')
      ErrorsFound=.true.
    ELSEIF (Alphas(3) /= 'YES') THEN  ! No validation done on the value of the interpolation field
      DaySchedule(Count)%IntervalInterpolated=.false.
      DO Hr=1,24
        CurMinute=MinutesPerTimeStep
        DO TS=1,NumOfTimeStepInHour
          DaySchedule(Count)%TSValue(Hr,TS)=MinuteValue(Hr,CurMinute)
          Curminute=CurMinute+MinutesPerTimeStep
        ENDDO
      ENDDO
    ELSE
      DaySchedule(Count)%IntervalInterpolated=.true.
      DO Hr=1,24
        SCount=1
        CurMinute=MinutesPerTimeStep
        DO TS=1,NumOfTimeStepInHour
          DaySchedule(Count)%TSValue(Hr,TS)=SUM(MinuteValue(Hr,SCount:CurMinute))/REAL(MinutesPerTimeStep,r64)
          SCount=CurMinute+1
          CurMinute=CurMinute+MinutesPerTimeStep
        ENDDO
      ENDDO
    ENDIF

    SchedTypePtr=DaySchedule(Count)%ScheduleTypePtr
    IF (ScheduleType(SchedTypePtr)%Limited) THEN
      IF (ANY(DaySchedule(Count)%TSValue < ScheduleType(SchedTypePtr)%Minimum) .or.  &
          ANY(DaySchedule(Count)%TSValue > ScheduleType(SchedTypePtr)%Maximum) ) THEN
        CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(Alphas(1))//  &
           '", Values are outside of range for '//TRIM(cAlphaFields(2))//'='//TRIM(Alphas(2)))
      ENDIF
    ENDIF
    IF (.not. ScheduleType(SchedTypePtr)%IsReal) THEN
         ! Make sure each is integer
      NumErrorFlag=.false.   ! only show error message once
      DO Hr=1,24
        DO TS=1,NumOfTimeStepInHour
          IF (DaySchedule(Count)%TSValue(Hr,TS) /= INT(DaySchedule(Count)%TSValue(Hr,TS)) ) THEN
            IF (.not. NumErrorFlag) THEN
              CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(Alphas(1))//  &
                 '", , One or more values are not integer as required by '//TRIM(cAlphaFields(2))//'='//TRIM(Alphas(2)))
              NumErrorFlag=.true.
            ENDIF
          ENDIF
        ENDDO
      ENDDO
    ENDIF
  ENDDO

!!! Get "DaySchedule:List"

  CurrentModuleObject='Schedule:Day:List'
  DO LoopIndex=1,NumLstDaySchedules
    CALL GetObjectItem(CurrentModuleObject,LoopIndex,Alphas,NumAlphas,Numbers,NumNumbers,Status,  &
                   AlphaBlank=lAlphaBlanks,NumBlank=lNumericBlanks,  &
                   AlphaFieldnames=cAlphaFields,NumericFieldNames=cNumericFields)
    IsNotOK=.false.
    IsBlank=.false.
    CALL VerifyName(Alphas(1),DaySchedule%Name,Count,IsNotOK,IsBlank,TRIM(CurrentModuleObject)//' Name')
    IF (IsNotOK) THEN
      ErrorsFound=.true.
      IF (IsBlank) Alphas(1)='xxxxx'
    ENDIF
    Count=Count+1
    DaySchedule(Count)%Name=Alphas(1)
    ! Validate ScheduleType
    IF (NumScheduleTypes > 0) THEN
      CheckIndex=FindIteminList(Alphas(2),ScheduleType(1:NumScheduleTypes)%Name,NumScheduleTypes)
      IF (CheckIndex == 0) THEN
        IF (.not. lAlphaBlanks(2)) THEN
          CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(Alphas(1))//  &
             '", '//TRIM(cAlphaFields(2))//'="'//TRIM(Alphas(2))//  &
             '" not found -- will not be validated')
        ELSE
          CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(Alphas(1))//  &
             '", Blank '//TRIM(cAlphaFields(2))//' input -- will not be validated.')
        ENDIF
      ELSE
        DaySchedule(Count)%ScheduleTypePtr=CheckIndex
      ENDIF
    ENDIF

    ! Depending on value of "Interpolate" field, the value for each time step in each hour gets processed:
    IF (Alphas(3) /= 'NO' .and. Alphas(3) /= 'YES') THEN
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(Alphas(1))//  &
         'Invalid value for "'//TRIM(cAlphaFields(3))//'" field="'//TRIM(Alphas(3))//'"')
      ErrorsFound=.true.
    ELSEIF (Alphas(3) /= 'YES') THEN  ! No validation done on the value of the interpolation field
      DaySchedule(Count)%IntervalInterpolated=.false.
    ELSE
      DaySchedule(Count)%IntervalInterpolated=.true.
    ENDIF

! check to see if there are any fields
    IF (Numbers(1) <= 0.0d0) THEN
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(Alphas(1))//  &
         '", Insufficient data entered for a full schedule day.')
      CALL ShowContinueError('...Minutes per Item field = ['//trim(RoundSigDigits(INT(Numbers(1))))//'].')
      ErrorsFound=.true.
      CYCLE
    ENDIF
    IF (NumNumbers < 25) THEN
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(Alphas(1))//  &
         '", Insufficient data entered for a full schedule day.')
      CALL ShowContinueError('...Minutes per Item field = ['//trim(RoundSigDigits(INT(Numbers(1))))//'] and '//  &
         ' only ['//trim(RoundSigDigits(NumNumbers-1))//'] to apply to list fields.')
      ErrorsFound=.true.
      CYCLE
    ENDIF
    MinutesPerItem=INT(Numbers(1))
    NumExpectedItems=1440/MinutesPerItem
    IF ((NumNumbers-1) /= NumExpectedItems) THEN
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(Alphas(1))//  &
         ', Number of Entered Items='//TRIM(RoundSigDigits(NumNumbers-1))//  &
         ' not equal number of expected items='//TRIM(RoundSigDigits(NumExpectedItems)))
      CALL ShowContinueError('based on '//TRIM(cNumericFields(1))//' field value='//TRIM(RoundSigDigits(MinutesPerItem)))
      ErrorsFound=.true.
      CYCLE
    ENDIF

    IF (MOD(60,MinutesPerItem) /= 0) THEN
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(Alphas(1)))
      CALL ShowContinueError('Requested '//TRIM(cNumericFields(1))//' field value ('//  &
         TRIM(RoundSigDigits(MinutesPerItem))//') not evenly divisible into 60')
      ErrorsFound=.true.
      CYCLE
    ENDIF

    ! Number of numbers in the Numbers list okay to process
    Hr=1
    CurMinute=MinutesPerItem
    SCount=1
    DO NumFields=2,NumNumbers
      MinuteValue(Hr,SCount:CurMinute)=Numbers(NumFields)
      SCount=CurMinute+1
      CurMinute=CurMinute+MinutesPerItem
      IF (CurMinute > 60) THEN
        CurMinute=MinutesPerItem
        SCount=1
        Hr=Hr+1
      ENDIF
    ENDDO

    ! Now parcel into TS Value....

    IF (DaySchedule(Count)%IntervalInterpolated) THEN
      DO Hr=1,24
        SCount=1
        CurMinute=MinutesPerTimeStep
        DO TS=1,NumOfTimeStepInHour
          DaySchedule(Count)%TSValue(Hr,TS)=SUM(MinuteValue(Hr,SCount:CurMinute))/REAL(MinutesPerTimeStep,r64)
          SCount=CurMinute+1
          CurMinute=CurMinute+MinutesPerTimeStep
        ENDDO
      ENDDO
    ELSE
      DO Hr=1,24
        CurMinute=MinutesPerTimeStep
        DO TS=1,NumOfTimeStepInHour
          DaySchedule(Count)%TSValue(Hr,TS)=MinuteValue(Hr,CurMinute)
          Curminute=CurMinute+MinutesPerTimeStep
        ENDDO
      ENDDO
    ENDIF

    SchedTypePtr=DaySchedule(Count)%ScheduleTypePtr
    IF (ScheduleType(SchedTypePtr)%Limited) THEN
      IF (ANY(DaySchedule(Count)%TSValue < ScheduleType(SchedTypePtr)%Minimum) .or.  &
          ANY(DaySchedule(Count)%TSValue > ScheduleType(SchedTypePtr)%Maximum) ) THEN
        CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(Alphas(1))//  &
           '", Values are outside of range for '//TRIM(cAlphaFields(2))//'='//TRIM(Alphas(2)))
      ENDIF
    ENDIF
    IF (.not. ScheduleType(SchedTypePtr)%IsReal) THEN
         ! Make sure each is integer
      NumErrorFlag=.false.   ! only show error message once
      DO Hr=1,24
        DO TS=1,NumOfTimeStepInHour
          IF (DaySchedule(Count)%TSValue(Hr,TS) /= INT(DaySchedule(Count)%TSValue(Hr,TS)) ) THEN
            IF (.not. NumErrorFlag) THEN
              CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(Alphas(1))//  &
                 '", , One or more values are not integer as required by '//TRIM(cAlphaFields(2))//'='//TRIM(Alphas(2)))
              NumErrorFlag=.true.
            ENDIF
          ENDIF
        ENDDO
      ENDDO
    ENDIF
  ENDDO

!!! Get Week Schedules - regular

  CurrentModuleObject='Schedule:Week:Daily'
  DO LoopIndex=1,NumRegWeekSchedules
    CALL GetObjectItem(CurrentModuleObject,LoopIndex,Alphas,NumAlphas,Numbers,NumNumbers,Status,  &
                   AlphaBlank=lAlphaBlanks,NumBlank=lNumericBlanks,  &
                   AlphaFieldnames=cAlphaFields,NumericFieldNames=cNumericFields)
    IsNotOK=.false.
    IsBlank=.false.
    CALL VerifyName(Alphas(1),WeekSchedule(1:NumRegWeekSchedules)%Name,LoopIndex-1,IsNotOK,IsBlank,  &
       TRIM(CurrentModuleObject)//' Name')
    IF (IsNotOK) THEN
      ErrorsFound=.true.
      IF (IsBlank) Alphas(1)='xxxxx'
    ENDIF
    WeekSchedule(LoopIndex)%Name=Alphas(1)
    ! Rest of Alphas are processed into Pointers
    DO InLoopIndex=1,MaxDayTypes
      DayIndex=FindIteminList(Alphas(InLoopIndex+1),DaySchedule(1:NumRegDaySchedules)%Name,NumRegDaySchedules)
      IF (DayIndex == 0) THEN
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(Alphas(1))//  &
           '", '//TRIM(cAlphaFields(InLoopIndex+1))//' "'//TRIM(Alphas(InLoopIndex+1))//'" not Found',UnitNumber)
        ErrorsFound=.true.
      ELSE
        WeekSchedule(LoopIndex)%DaySchedulePointer(InLoopIndex)=DayIndex
      ENDIF
    END DO
  END DO

!!! Get Week Schedules - compact
  Count=NumRegWeekSchedules
  CurrentModuleObject='Schedule:Week:Compact'
  DO LoopIndex=1,NumCptWeekSchedules
    CALL GetObjectItem(CurrentModuleObject,LoopIndex,Alphas,NumAlphas,Numbers,NumNumbers,Status,  &
                   AlphaBlank=lAlphaBlanks,NumBlank=lNumericBlanks,  &
                   AlphaFieldnames=cAlphaFields,NumericFieldNames=cNumericFields)
    IsNotOK=.false.
    IsBlank=.false.
    IF (Count > 0) THEN
      CALL VerifyName(Alphas(1),WeekSchedule(1:Count)%Name,Count,IsNotOK,IsBlank,TRIM(CurrentModuleObject)//' Name')
      IF (IsNotOK) THEN
        ErrorsFound=.true.
        IF (IsBlank) Alphas(1)='xxxxx'
      ENDIF
    ENDIF
    Count=Count+1
    WeekSchedule(Count)%Name=Alphas(1)
    AllDays=.false.
    ! Rest of Alphas are processed into Pointers
    DO InLoopIndex=2,NumAlphas,2
      DayIndex=FindIteminList(Alphas(InLoopIndex+1),DaySchedule(1:NumRegDaySchedules)%Name,NumRegDaySchedules)
      IF (DayIndex == 0) THEN
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(Alphas(1))//  &
           '", '//TRIM(cAlphaFields(InLoopIndex+1))//' "'//TRIM(Alphas(InLoopIndex+1))//'" not Found',UnitNumber)
        CALL ShowContinueError('ref: '//TRIM(cAlphaFields(InLoopIndex))//' "'//TRIM(Alphas(InLoopIndex))//'"')
        ErrorsFound=.true.
      ELSE
        TheseDays=.false.
        ErrorHere=.false.
        CALL ProcessForDayTypes(Alphas(InLoopIndex),TheseDays,AllDays,ErrorHere)
        IF (ErrorHere) THEN
          CALL ShowContinueError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(Alphas(1)))
          ErrorsFound=.true.
        ELSE
          DO Hr=1,MaxDayTypes
            IF (TheseDays(Hr)) THEN
              WeekSchedule(Count)%DaySchedulePointer(Hr)=DayIndex
            ENDIF
          ENDDO
        ENDIF
      ENDIF
    END DO
    !  Have processed all named days, check to make sure all given
    IF (.not. ALL(AllDays)) THEN
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(Alphas(1))//  &
         '", Missing some day assignments')
      ErrorsFound=.true.
    ENDIF
  END DO
  NumRegWeekSchedules=Count

!!! Get Schedules (all types)

!!! Get Regular Schedules

  CurrentModuleObject='Schedule:Year'
  DO LoopIndex=1,NumRegSchedules
    CALL GetObjectItem(CurrentModuleObject,LoopIndex,Alphas,NumAlphas,Numbers,NumNumbers,Status,  &
                   AlphaBlank=lAlphaBlanks,NumBlank=lNumericBlanks,  &
                   AlphaFieldnames=cAlphaFields,NumericFieldNames=cNumericFields)
    IsNotOK=.false.
    IsBlank=.false.
    CALL VerifyName(Alphas(1),Schedule(1:NumSchedules)%Name,LoopIndex-1,IsNotOK,IsBlank,TRIM(CurrentModuleObject)//' Name')
    IF (IsNotOK) THEN
      ErrorsFound=.true.
      IF (IsBlank) Alphas(1)='xxxxx'
    ENDIF
    Schedule(LoopIndex)%Name=Alphas(1)
    Schedule(LoopIndex)%SchType=ScheduleInput_year
    ! Validate ScheduleType
    IF (NumScheduleTypes > 0) THEN
      CheckIndex=FindIteminList(Alphas(2),ScheduleType(1:NumScheduleTypes)%Name,NumScheduleTypes)
      IF (CheckIndex == 0) THEN
        IF (.not. lAlphaBlanks(2)) THEN
          CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(Alphas(1))//  &
             '", '//TRIM(cAlphaFields(2))//'="'//TRIM(Alphas(2))//  &
             '" not found -- will not be validated')
        ELSE
          CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(Alphas(1))//  &
             '", Blank '//TRIM(cAlphaFields(2))//' input -- will not be validated.')
        ENDIF
      ELSE
        Schedule(LoopIndex)%ScheduleTypePtr=CheckIndex
      ENDIF
    ENDIF
    NumPointer=0
    DaysInYear=0
    ! Rest of Alphas (Weekschedules) are processed into Pointers
    DO InLoopIndex=3,NumAlphas
      WeekIndex=FindIteminList(Alphas(InLoopIndex),WeekSchedule(1:NumRegWeekSchedules)%Name,NumRegWeekSchedules)
      IF (WeekIndex == 0) THEN
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(Alphas(1))//  &
             '", '//TRIM(cAlphaFields(InLoopIndex))//'="'//TRIM(Alphas(InLoopIndex))//  &
             '" not found.',UnitNumber)
        ErrorsFound=.true.
      ELSE
        ! Process for month, day
        StartMonth=INT(Numbers(NumPointer+1))
        StartDay=INT(Numbers(NumPointer+2))
        EndMonth=INT(Numbers(NumPointer+3))
        EndDay=INT(Numbers(NumPointer+4))
        NumPointer=NumPointer+4
        StartPointer=JulianDay(StartMonth,StartDay,1)
        EndPointer=JulianDay(EndMonth,EndDay,1)
        IF (StartPointer <= EndPointer) THEN
          DO Count=StartPointer,EndPointer
            DaysInYear(Count)=DaysInYear(Count)+1
            Schedule(LoopIndex)%WeekSchedulePointer(Count)=WeekIndex
          END DO
        ELSE
          DO Count=StartPointer,366
            DaysInYear(Count)=DaysInYear(Count)+1
            Schedule(LoopIndex)%WeekSchedulePointer(Count)=WeekIndex
          END DO
          DO Count=1,EndPointer
            DaysInYear(Count)=DaysInYear(Count)+1
            Schedule(LoopIndex)%WeekSchedulePointer(Count)=WeekIndex
          END DO
        ENDIF
      ENDIF
    END DO
    ! Perform Error checks on this item
    ! Do special test for Feb 29.  Make equal to Feb 28.
    IF (DaysinYear(60) == 0) THEN
      DaysinYear(60)=DaysinYear(59)
      Schedule(LoopIndex)%WeekSchedulePointer(60)=Schedule(LoopIndex)%WeekSchedulePointer(59)
    ENDIF
    IF (ANY(DaysinYear == 0)) THEN
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(Schedule(LoopIndex)%Name)//  &
                 '" has missing days in its schedule pointers',UnitNumber)
      ErrorsFound=.true.
    ENDIF
    IF (ANY(DaysinYear > 1)) THEN
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(Schedule(LoopIndex)%Name)//  &
                 '" has overlapping days in its schedule pointers',UnitNumber)
      ErrorsFound=.true.
    ENDIF

    IF  (AnyEnergyManagementSystemInModel) THEN ! setup constant schedules as actuators
      CALL SetupEMSActuator('Schedule:Year', &
          Schedule(LoopIndex)%Name, 'Schedule Value', '[ ]',&
          Schedule(LoopIndex)%EMSActuatedOn, Schedule(LoopIndex)%EMSValue )
    ENDIF

  END DO

!!! Get Compact Schedules
!SCHEDULE:COMPACT,
!   \memo Irregular object.  Does not follow the usual definition for fields.  Fields A3... are:
!   \memo Through: Date
!   \memo For: Applicable days (ref: Weekschedule:Compact)
!   \memo Interpolate: Yes/No (ref: Dayschedule:interval) -- optional, if not used will be "No"
!   \memo Until: <Time> (ref: Dayschedule:Interval)
!   \memo <numeric value>
!   \memo words "Through","For","Interpolate","Until" must be included.
!  A1 , \field Name
!       \required-field
!       \type alpha
!       \reference ScheduleNames
!  A2 , \field ScheduleType
!       \type object-list
!       \object-list ScheduleTypeNames
!  A3 , \field Complex Field #1
!  A4 , \field Complex Field #2
!  A5 , \field Complex Field #3

  SchNum=NumRegSchedules
  AddWeekSch=NumRegWeekSchedules
  AddDaySch=NumRegDaySchedules
  CurrentModuleObject='Schedule:Compact'
  DO LoopIndex=1,NumCptSchedules
    CALL GetObjectItem(CurrentModuleObject,LoopIndex,Alphas,NumAlphas,Numbers,NumNumbers,Status,  &
                   AlphaBlank=lAlphaBlanks,NumBlank=lNumericBlanks,  &
                   AlphaFieldnames=cAlphaFields,NumericFieldNames=cNumericFields)
    IsNotOK=.false.
    IsBlank=.false.
    CALL VerifyName(Alphas(1),Schedule(1:NumSchedules)%Name,SchNum,IsNotOK,IsBlank,TRIM(CurrentModuleObject)//' Name')
    IF (IsNotOK) THEN
      ErrorsFound=.true.
      IF (IsBlank) Alphas(1)='xxxxx'
    ENDIF
    SchNum=SchNum+1
    Schedule(SchNum)%Name=Alphas(1)
    Schedule(SchNum)%SchType=ScheduleInput_compact
    ! Validate ScheduleType
    CheckIndex=FindIteminList(Alphas(2),ScheduleType(1:NumScheduleTypes)%Name,NumScheduleTypes)
    IF (CheckIndex == 0) THEN
        IF (.not. lAlphaBlanks(2)) THEN
          CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(Alphas(1))//  &
             '", '//TRIM(cAlphaFields(2))//'="'//TRIM(Alphas(2))//  &
             '" not found -- will not be validated')
        ELSE
          CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(Alphas(1))//  &
             '", Blank '//TRIM(cAlphaFields(2))//' input -- will not be validated.')
        ENDIF
    ELSE
      Schedule(SchNum)%ScheduleTypePtr=CheckIndex
    ENDIF
    NumPointer=0
    DaysInYear=0
    ! Process the "complex" fields -- so named because they are not a 1:1 correspondence
    ! as other objects are
    NumField=3
    StartPointer=1
    WkCount=0
    DyCount=0
    FullYearSet=.false.
Through:    DO WHILE (NumField < NumAlphas)
       !   Process "Through"
      IF (Alphas(NumField)(1:8) /= 'THROUGH:' .and. Alphas(NumField)(1:7) /= 'THROUGH') THEN
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(Schedule(SchNum)%Name)//  &
             '", Expecting "Through:" date')
        CALL ShowContinueError('Instead, found entry='//TRIM(Alphas(NumField)))
        ErrorsFound=.true.
        EXIT Through
      ELSE
        IF (Alphas(NumField)(8:8) == ':') THEN
          sPos=9
        ELSE
          sPos=8
        ENDIF
        Alphas(NumField)=Alphas(NumField)(sPos:)
        Alphas(NumField)=ADJUSTL(Alphas(NumField))
      ENDIF
      CurrentThrough=Alphas(NumField)
      ErrorHere=.false.
      CALL ProcessDateString(Alphas(NumField),EndMonth,EndDay,PWeekDay,PDateType,ErrorHere)
      IF (PDateType > 1) THEN
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(Schedule(SchNum)%Name)//  &
             '", Invalid "Through:" date')
        CALL ShowContinueError('Found entry='//TRIM(Alphas(NumField)))
        ErrorsFound=.true.
        EXIT Through
      ELSEIF (ErrorHere) THEN
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(Schedule(SchNum)%Name)//  &
             '", Invalid "Through:" date')
        CALL ShowContinueError('Found entry='//TRIM(Alphas(NumField)))
        ErrorsFound=.true.
        EXIT Through
      ELSE
        EndPointer=JulianDay(EndMonth,EndDay,1)
        IF (EndPointer == 366) THEN
          IF (FullYearSet) THEN
            CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(Schedule(SchNum)%Name)//  &
                '", New "Through" entry when "full year" already set')
            CALL ShowContinueError('"Through" field='//TRIM(CurrentThrough))
            ErrorsFound=.true.
          ENDIF
          FullYearSet=.true.
        ENDIF
      ENDIF
      WkCount=WkCount+1
      AddWeekSch=AddWeekSch+1
      WRITE(ExtraField,*) WkCount
      ExtraField=ADJUSTL(ExtraField)
      WeekSchedule(AddWeekSch)%Name=TRIM(Alphas(1))//'_wk_'//TRIM(ExtraField)
      WeekSchedule(AddWeekSch)%Used=.true.
      DO Hr=StartPointer,EndPointer
        Schedule(SchNum)%WeekSchedulePointer(Hr)=AddWeekSch
        DaysInYear(Hr)=DaysInYear(Hr)+1
      ENDDO
      StartPointer=EndPointer+1
      ThruField=NumField
      AllDays=.false.
      NumField=NumField+1
For:  DO WHILE (NumField < NumAlphas) ! Continues until next "Through"
        IF (Alphas(NumField)(1:7) == 'THROUGH') EXIT For
        !   "For" must be next, adds to "# Day Schedules"
        IF (Alphas(NumField)(1:3) == 'FOR') THEN
          DyCount=DyCount+1
          AddDaySch=AddDaySch+1
          WRITE(ExtraField,*) DyCount
          ExtraField=ADJUSTL(ExtraField)
          DaySchedule(AddDaySch)%Name=TRIM(Alphas(1))//'_dy_'//TRIM(ExtraField)
          DaySchedule(AddDaySch)%ScheduleTypePtr=Schedule(SchNum)%ScheduleTypePtr
          DaySchedule(AddDaySch)%Used=.true.
          TheseDays=.false.
          ErrorHere=.false.
          LastFor=Alphas(NumField)
          CALL ProcessForDayTypes(Alphas(NumField),TheseDays,AllDays,ErrorHere)
          IF (ErrorHere) THEN
            CALL ShowContinueError('ref '//TRIM(CurrentModuleObject)//'="'//TRIM(Alphas(1))//'"')
            CALL ShowContinueError('ref Through field='//TRIM(Alphas(ThruField)))
            ErrorsFound=.true.
          ELSE
            DO Hr=1,MaxDayTypes
              IF (TheseDays(Hr)) THEN
                WeekSchedule(AddWeekSch)%DaySchedulePointer(Hr)=AddDaySch
              ENDIF
            ENDDO
          ENDIF
        ELSE
          CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(Alphas(1))//  &
               '", Looking for "For" field, found='//TRIM(Alphas(NumField)))
          ErrorsFound=.true.
!          CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(Schedule(SchNum)%Name)//  &
!               '", Expecting "For:" day types')
!          CALL ShowContinueError('Instead, found entry='//TRIM(Alphas(NumField)))
          EXIT Through
        ENDIF
        ! Check for "Interpolate"
        NumField=NumField+1
        IF (Alphas(NumField)(1:11) == 'INTERPOLATE') THEN
          IF (INDEX(Alphas(NumField),'YES') > 0) THEN
            DaySchedule(AddDaySch)%IntervalInterpolated=.true.
          ELSE
            DaySchedule(AddDaySch)%IntervalInterpolated=.false.
          ENDIF
          NumField=NumField+1
        ELSE
          IF (Alphas(NumField)(1:5) /= 'UNTIL') THEN
            IF (INDEX(Alphas(NumField),'YES') > 0) THEN
              DaySchedule(AddDaySch)%IntervalInterpolated=.true.
            ELSEIF (INDEX(Alphas(NumField),'NO') > 0) THEN
              DaySchedule(AddDaySch)%IntervalInterpolated=.false.
            ELSE
              CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(Alphas(1))//  &
                  '", Illegal Field entered ='//TRIM(Alphas(NumField)))
              ErrorsFound=.true.
            ENDIF
            NumField=NumField+1
          ENDIF
        ENDIF
        NumNumbers=0
        xxcount=0
        UntilFld=NumField
Until:  DO
        IF (Alphas(NumField)(1:3) == 'FOR') EXIT Until
        IF (Alphas(NumField)(1:7) == 'THROUGH') EXIT Until
        IF (Alphas(NumField)(1:5) == 'UNTIL') THEN
          ! Process Until/Value pairs for later processing by other routine.
          NumField=NumField+1
          xxcount=xxcount+1
          NumNumbers=NumNumbers+1
          Numbers(NumNumbers)=ProcessNumber(Alphas(NumField),ErrorHere)
          IF (ErrorHere) THEN
            CALL ShowSevereError(TRIM(CurrentModuleObject)//'="'//TRIM(Alphas(1))//'"')
            CALL ShowContinueError('Until field=['//trim(Alphas(NumField-1))//'] has illegal value field=['//  &
                      trim(Alphas(NumField))//'].')
            ErrorsFound=.true.
          ENDIF
          NumField=NumField+1
          Alphas(UntilFld+xxcount)=Alphas(NumField)  ! Incase next is "until"
        ELSE
          CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(Alphas(1))//  &
               '", Looking for "Until" field, found='//TRIM(Alphas(NumField)))
          ErrorsFound=.true.
          EXIT Through
        ENDIF
        IF (Alphas(NumField) == Blank) EXIT Until
        ENDDO Until
        ! Process Untils, Numbers
        IF (NumNumbers > 0) THEN
          NumFields=NumNumbers
          ErrorHere=.false.
          CALL ProcessIntervalFields(Alphas(UntilFld:),Numbers,NumFields,NumNumbers,MinuteValue,SetMinuteValue,  &
                                     ErrorHere,DaySchedule(AddDaySch)%Name,TRIM(CurrentModuleObject)//' DaySchedule Fields')
          ! Depending on value of "Interpolate" field, the value for each time step in each hour gets processed:
          IF (ErrorHere) THEN
            CALL ShowContinueError('ref '//TRIM(CurrentModuleObject)//'="'//TRIM(Alphas(1))//'"')
            ErrorsFound=.true.
          ENDIF
          IF (.not. DaySchedule(AddDaySch)%IntervalInterpolated) THEN  ! No validation done on the value of the interpolation field
            DO Hr=1,24
              CurMinute=MinutesPerTimeStep
              DO TS=1,NumOfTimeStepInHour
                DaySchedule(AddDaySch)%TSValue(Hr,TS)=MinuteValue(Hr,CurMinute)
                Curminute=CurMinute+MinutesPerTimeStep
              ENDDO
            ENDDO
          ELSE
            DO Hr=1,24
              SCount=1
              CurMinute=MinutesPerTimeStep
              DO TS=1,NumOfTimeStepInHour
!                tempval=SUM(MinuteValue(Hr,SCount:CurMinute))/REAL(MinutesPerTimeStep,r64)
                DaySchedule(AddDaySch)%TSValue(Hr,TS)=SUM(MinuteValue(Hr,SCount:CurMinute))/REAL(MinutesPerTimeStep,r64)
                SCount=CurMinute+1
                CurMinute=CurMinute+MinutesPerTimeStep
              ENDDO
            ENDDO
          ENDIF
        ENDIF
      ENDDO For
      IF (Any(.not. AllDays)) THEN
        CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(Schedule(SchNum)%Name)// &
                   '" has missing day types in Through='//TRIM(CurrentThrough))
        CALL ShowContinueError('Last "For" field='//TRIM(LastFor))
        errmsg='Missing day types=,'
        DO kdy=1,MaxDayTypes
          IF (AllDays(kdy)) CYCLE
          errmsg=errmsg(1:len_trim(errmsg)-1)//'"'//trim(ValidDayTypes(kdy))//'",-'
        ENDDO
        errmsg=errmsg(1:len_trim(errmsg)-2)
        CALL ShowContinueError(trim(errmsg))
        CALL ShowContinueError('Missing day types will have 0.0 as Schedule Values')
      ENDIF
    ENDDO Through
    IF (DaysinYear(60) == 0) THEN
      DaysinYear(60)=DaysinYear(59)
      Schedule(LoopIndex)%WeekSchedulePointer(60)=Schedule(LoopIndex)%WeekSchedulePointer(59)
    ENDIF
    IF (ANY(DaysinYear == 0)) THEN
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(Schedule(SchNum)%Name)//  &
                 '" has missing days in its schedule pointers',UnitNumber)
      ErrorsFound=.true.
    ENDIF
    IF (ANY(DaysinYear > 1)) THEN
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(Schedule(SchNum)%Name)//  &
                 '" has overlapping days in its schedule pointers',UnitNumber)
      ErrorsFound=.true.
    ENDIF

    IF  (AnyEnergyManagementSystemInModel) THEN ! setup constant schedules as actuators
      Call SetupEMSActuator('Schedule:Compact', &
          Schedule(SchNum)%Name, 'Schedule Value', '[ ]',&
          Schedule(SchNum)%EMSActuatedOn, Schedule(SchNum)%EMSValue )
    ENDIF

  END DO

!  Schedule:File,
!   \min-fields 5
!         \memo A Schedule:File points to a text computer file that has 8760-8784 hours of data.
!    A1 , \field Name
!         \required-field
!         \type alpha
!         \reference ScheduleNames
!    A2 , \field Schedule Type Limits Name
!         \type object-list
!         \object-list ScheduleTypeLimitsNames
!    A3 , \field File Name
!         \required-field
!         \retaincase
!    N1 , \field Column Number
!         \required-field
!         \type integer
!         \minimum 1
!    N2 , \field Rows to Skip at Top
!         \required-field
!         \type integer
!         \minimum 0
!    N3 , \field Number of Hours of Data
!         \note 8760 hours does not account for leap years, 8784 does.
!         \note should be either 8760 or 8784
!         \default 8760
!         \minimum 8760
!         \maximum 8784
!    A4 , \field Column Separator
!         \type choice
!         \key Comma
!         \key Tab
!         \key Fixed
!         \key Semicolon
!         \default Comma
!    A5 , \field Interpolate to Timestep
!         \note when the interval does not match the user specified timestep a "Yes" choice will average between the intervals request (to
!         \note timestep resolution.  a "No" choice will use the interval value at the simulation timestep without regard to if it matches
!         \note the boundary or not.
!         \type choice
!         \key Yes
!         \key No
!         \default No
!    N4 ; \field Minutes per Item
!         \note Must be evenly divisible into 60
!         \type integer
!         \minimum 1
!         \maximum 60

! continue adding to SchNum,AddWeekSch,AddDaySch
  IF (NumCommaFileSchedules > 0) THEN
    ALLOCATE(hourlyFileValues(8784*60))   ! sized to accomodate any interval for schedule file.
  ENDIF
  CurrentModuleObject='Schedule:File'
  DO LoopIndex=1,NumCommaFileSchedules
    CALL GetObjectItem(CurrentModuleObject,LoopIndex,Alphas,NumAlphas,Numbers,NumNumbers,Status,  &
                   AlphaBlank=lAlphaBlanks,NumBlank=lNumericBlanks,  &
                   AlphaFieldnames=cAlphaFields,NumericFieldNames=cNumericFields)
    IsNotOK=.false.
    IsBlank=.false.
    CALL VerifyName(Alphas(1),Schedule(1:NumSchedules)%Name,SchNum,IsNotOK,IsBlank,TRIM(CurrentModuleObject)//' Name')
    IF (IsNotOK) THEN
      ErrorsFound=.true.
      IF (IsBlank) Alphas(1)='xxxxx'
    ENDIF
    SchNum = SchNum + 1
    Schedule(SchNum)%Name=Alphas(1)
    Schedule(SchNum)%SchType=ScheduleInput_file
    ! Validate ScheduleType
    IF (NumScheduleTypes > 0) THEN
      CheckIndex=0
      IF (.not. lAlphaBlanks(2)) &
           CheckIndex=FindIteminList(Alphas(2),ScheduleType(1:NumScheduleTypes)%Name,NumScheduleTypes)
      IF (CheckIndex == 0) THEN
        IF (.not. lAlphaBlanks(2)) THEN
          CALL ShowWarningError('ProcessScheduleInput: For '//TRIM(CurrentModuleObject)//'="'//TRIM(Alphas(1))//  &
             '", '//TRIM(cAlphaFields(2))//'="'//TRIM(Alphas(2))//  &
             '" not found -- will not be validated')
        ELSE
          CALL ShowWarningError('For '//TRIM(CurrentModuleObject)//'="'//TRIM(Alphas(1))//  &
             '", Blank '//TRIM(cAlphaFields(2))//' input -- will not be validated.')
        ENDIF
      ELSE
        Schedule(SchNum)%ScheduleTypePtr=CheckIndex
      ENDIF
    ENDIF
    hourlyFileValues = 0.0d0 !set default values to zero

    ! Numbers(1) - which column
    curcolCount=Numbers(1)
    ! Numbers(2) - number of rows to skip
    skiprowCount=Numbers(2)
    IF (Numbers(3) == 0) Numbers(3)=8760.0d0
    IF (Numbers(3) /= 8760 .and. Numbers(3) /= 8784) THEN
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(Alphas(1))//  &
         '", '//TRIM(cNumericFields(3))//' must = 8760 or 8784 (for a leap year)')
      CALL ShowContinueError('..Value for field = '//TRIM(TrimSigDigits(Numbers(3),0))//', Schedule not processed.')
      ErrorsFound=.true.
      CYCLE
    ENDIF

    IF (lAlphaBlanks(4) .or. SameString(Alphas(4),'comma')) THEN
      ColumnSep=CharComma
      Alphas(4)='comma'
    ELSEIF (SameString(Alphas(4),'semicolon')) THEN
      ColumnSep=CharSemicolon
    ELSEIF (SameString(Alphas(4),'tab')) THEN
      ColumnSep=CharTab
    ELSEIF (SameString(Alphas(4),'space')) THEN
      ColumnSep=CharSpace
    ELSE
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(Alphas(1))//  &
         '", '//TRIM(cAlphaFields(4))//' illegal value="'//trim(Alphas(4))//'".')
      CALL ShowContinueError('..must be Comma, Semicolon, Tab, or Space.')
      ErrorsFound=.true.
      CYCLE
    ENDIF

    ! Depending on value of "Interpolate" field, the value for each time step in each hour gets processed:
    FileIntervalInterpolated=.false.
    IF (lAlphaBlanks(5)) Alphas(5)='NO'
    IF (Alphas(5) /= 'NO' .and. Alphas(5) /= 'YES') THEN
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(Alphas(1))//  &
         'Invalid value for "'//TRIM(cAlphaFields(5))//'" field="'//TRIM(Alphas(5))//'"')
      ErrorsFound=.true.
    ELSEIF (Alphas(5) /= 'YES') THEN  ! No validation done on the value of the interpolation field
      FileIntervalInterpolated=.false.
    ELSE
      FileIntervalInterpolated=.true.
    ENDIF

    ! is it a sub-hourly schedule or not?
    MinutesPerItem=60
    IF (NumNumbers > 3) THEN
      MinutesPerItem=INT(Numbers(4))
      NumExpectedItems=1440/MinutesPerItem
      IF (MOD(60,MinutesPerItem) /= 0) THEN
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(Alphas(1)))
        CALL ShowContinueError('Requested '//TRIM(cNumericFields(4))//' field value ('//  &
           TRIM(RoundSigDigits(MinutesPerItem))//') not evenly divisible into 60')
        ErrorsFound=.true.
        CYCLE
      ENDIF
    ENDIF

    numHourlyValues=Numbers(3)
    rowLimitCount=(Numbers(3)*60.0d0)/MinutesPerItem
    hrLimitCount=60/MinutesPerItem

!    ! Number of numbers in the Numbers list okay to process
!    Hr=1
!    CurMinute=MinutesPerItem
!    SCount=1
!    DO NumFields=2,NumNumbers
!      MinuteValue(Hr,SCount:CurMinute)=Numbers(NumFields)
!      SCount=CurMinute+1
!      CurMinute=CurMinute+MinutesPerItem
!      IF (CurMinute > 60) THEN
!        CurMinute=MinutesPerItem
!        SCount=1
!        Hr=Hr+1
!      ENDIF
!    ENDDO
!
!    ! Now parcel into TS Value....
!
!    IF (DaySchedule(Count)%IntervalInterpolated) THEN
!      DO Hr=1,24
!        SCount=1
!        CurMinute=MinutesPerTimeStep
!        DO TS=1,NumOfTimeStepInHour
!          DaySchedule(Count)%TSValue(Hr,TS)=SUM(MinuteValue(Hr,SCount:CurMinute))/REAL(MinutesPerTimeStep,r64)
!          SCount=CurMinute+1
!          CurMinute=CurMinute+MinutesPerTimeStep
!        ENDDO
!      ENDDO
!    ELSE
!      DO Hr=1,24
!        CurMinute=MinutesPerTimeStep
!        DO TS=1,NumOfTimeStepInHour
!          DaySchedule(Count)%TSValue(Hr,TS)=MinuteValue(Hr,CurMinute)
!          Curminute=CurMinute+MinutesPerTimeStep
!        ENDDO
!      ENDDO
!    ENDIF

    CALL CheckForActualFileName(Alphas(3),FileExists,TempFullFileName)

!    INQUIRE(file=Alphas(3),EXIST=FileExists)
! Setup file reading parameters
    StripCR=.false.
    IF (.not. FileExists) THEN
      CALL DisplayString('Missing '//TRIM(Alphas(3)))
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(Alphas(1))//  &
         '", '//TRIM(cAlphaFields(3))//'="'//TRIM(Alphas(3))//'" not found.')
      CALL ShowContinueError('Certain run environments require a full path to be included with the file name in the input field.')
      CALL ShowContinueError('Try again with putting full path and file name in the field.')
      ErrorsFound=.true.
    ELSE
      SchdFile = GetNewUnitNumber()
      OPEN(unit=SchdFile, file=TempFullFileName, action='read', IOSTAT=read_stat)
      IF (read_stat /= 0) THEN
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(Alphas(1))//  &
         '", '//TRIM(cAlphaFields(3))//'="'//TRIM(Alphas(3))//'" cannot be opened.')
        CALL ShowContinueError('... It may be open in another program (such as Excel).  Please close and try again.')
        CALL ShowFatalError('Program terminates due to previous condition.')
      ENDIF
      ! check for stripping
      READ(Unit=SchdFile, FMT="(A)", IOSTAT=read_stat) LineIn
      endLine=LEN_TRIM(LineIn)
      IF (endLine > 0) THEN
        IF (ICHAR(LineIn(endLine:endLine)) == iASCII_CR) THEN
          StripCR=.true.
          LineIn(endLine:endLine)=Blank
        ENDIF
        IF (ICHAR(LineIn(endLine:endLine)) == iUnicode_end) THEN
          CLOSE(unit=SchdFile)
          CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(Alphas(1))//  &
           '", '//TRIM(cAlphaFields(3))//'="'//TRIM(Alphas(3))//' appears to be a Unicode or binary file.')
          CALL ShowContinueError('...This file cannot be read by this program. Please save as PC or Unix file and try again')
          CALL ShowFatalError('Program terminates due to previous condition.')
        ENDIF
      ENDIF
      BACKSPACE(Unit=SchdFile)

 ! skip lines if any need to be skipped.
      numerrors=0
      rowCnt = 0
      read_stat=0
      IF (skiprowCount > 0) THEN  ! Numbers(2) has number of rows to skip
        DO WHILE (read_stat == 0) !end of file
          READ(UNIT=SchdFile, FMT="(A)", IOSTAT=read_stat) LineIn
          IF (StripCR) THEN
            endLine=LEN_TRIM(LineIn)
            IF (endLine > 0) THEN
              IF (ICHAR(LineIn(endLine:endLine)) == iASCII_CR) LineIn(endLine:endLine)=Blank
            ENDIF
          ENDIF
          rowCnt = rowCnt + 1
          IF (rowCnt == skiprowCount) THEN
            EXIT
          END IF
        END DO
      ENDIF

!  proper number of lines are skipped.  read the file
      ! for the rest of the lines read from the file
      rowCnt = 0
      firstLine=.true.
      DO WHILE (read_stat == 0) !end of file
        READ(UNIT=SchdFile, FMT="(A)", IOSTAT=read_stat) LineIn
        IF (StripCR) THEN
          endLine=LEN_TRIM(LineIn)
          IF (endLine > 0) THEN
            IF (ICHAR(LineIn(endLine:endLine)) == iASCII_CR) LineIn(endLine:endLine)=Blank
          ENDIF
        ENDIF
        rowCnt = rowCnt + 1
        colCnt = 0
        wordStart = 1
        columnValue = 0.0d0
        !scan through the line looking for a specific column
        DO
          sepPos = INDEX(LineIn, ColumnSep)
          colCnt = colCnt + 1
          IF (sepPos > 0) THEN
            if (sepPos > 1) then
              wordEnd = sepPos - 1
            else
              wordEnd = wordStart
            endif
            subString = TRIM(LineIn(wordStart:wordEnd))
            !the next word will start after the comma
            wordStart = sepPos + 1
            !get rid of separator so next INDEX will find next separator
            LineIn=LineIn(wordStart:)
            firstLine=.false.
            wordStart=1
          ELSE
            !no more commas
            subString = LineIn(wordStart:)
            if (firstLine .and. subString == Blank) then
              CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(Alphas(1))//  &
                '" first line does not contain the indicated column separator='//trim(Alphas(4))//'.')
              CALL ShowContinueError('...first 40 characters of line=['//trim(LineIn(1:40))//']')
              firstLine=.false.
            endif
            EXIT
          END IF
          IF (colCnt .eq. curcolCount) EXIT
        END DO
        IF (colCnt .eq. curcolCount) THEN
          columnValue = ProcessNumber(subString,errflag)
          IF (errflag) THEN
            numerrors=numerrors+1
            columnValue = 0.0d0
          ENDIF
        ELSE
          columnValue = 0.0d0
        END IF
        hourlyFileValues(rowCnt) = columnValue
        IF (rowCnt .eq. rowLimitCount) EXIT
      END DO
      CLOSE(SchdFile)

      ! schedule values have been filled into the hourlyFileValues array.

      IF (numerrors > 0) THEN
        CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(Alphas(1))//  &
           '" '//trim(RoundSigDigits(numerrors))//' records had errors - these values are set to 0.')
        CALL ShowContinueError('Use Output:Diagnostics,DisplayExtraWarnings; to see individual records in error.')
      ENDIF
      IF (rowCnt < rowLimitCount) THEN
        CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(Alphas(1))//  &
           '" less than '//trim(RoundSigDigits(numHourlyValues))//' hourly values read from file.')
        CALL ShowContinueError('..Number read='//TRIM(TrimSigDigits((rowCnt*60)/MinutesPerItem))//'.')
      END IF
      IF (rowCnt < rowLimitCount) THEN
        CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(Alphas(1))//  &
           '" less than specified hourly values read from file.')
        CALL ShowContinueError('..Specified Number of Hourly Values='//TRIM(TrimSigDigits(numHourlyValues,0))//  &
               ' Actual number of hourly values included='//TRIM(TrimSigDigits((rowCnt*60)/MinutesPerItem)))
      ENDIF
      ! process the data into the normal schedule data structures
      ! note -- schedules are ALWAYS 366 days so some special measures have to be done at 29 Feb "day of year" (60)
      iDay=0
      hDay=0
      ifld=0
      DO
        ! create string of which day of year
        iDay=iDay+1
        hDay=hDay+1
        IF (iDay > 366) EXIT
        ExtraField=RoundSigDigits(iDay)
        ! increment both since a week schedule is being defined for each day so that a day is valid
        ! no matter what the day type that is used in a design day.
        AddWeekSch = AddWeekSch + 1
        AddDaySch = AddDaySch + 1
        ! define week schedule
        WeekSchedule(AddWeekSch)%Name=TRIM(Alphas(1))//'_wk_'//ExtraField
        ! for all day types point the week schedule to the newly defined day schedule
        DO kDayType = 1, MaxDayTypes
          WeekSchedule(AddWeekSch)%DaySchedulePointer(kDayType) = AddDaySch
        END DO
        ! day schedule
        DaySchedule(AddDaySch)%Name=TRIM(Alphas(1))//'_dy_'//ExtraField
        DaySchedule(AddDaySch)%ScheduleTypePtr = Schedule(SchNum)%ScheduleTypePtr
        ! schedule is pointing to the week schedule
        Schedule(SchNum)%WeekSchedulePointer(iDay) = AddWeekSch
        IF (MinutesPerItem == 60) THEN
          DO jHour = 1, 24
            ifld=ifld+1
            curHrVal = hourlyFileValues(ifld) ! hourlyFileValues((hDay - 1) * 24 + jHour)
            DO TS=1,NumOfTimeStepInHour
              DaySchedule(AddDaySch)%TSValue(jHour,TS) = curHrVal
            END DO
          END DO
        ELSE  ! Minutes Per Item < 60
          DO Hr=1,24
            CurMinute=MinutesPerItem
            SCount=1
            DO NumFields=1,hrLimitCount
              ifld=ifld+1
              MinuteValue(Hr,SCount:CurMinute)=hourlyFileValues(ifld)
              SCount=CurMinute+1
              CurMinute=CurMinute+MinutesPerItem
            ENDDO
          ENDDO
          IF (FileIntervalInterpolated) THEN
            DO Hr=1,24
              SCount=1
              CurMinute=MinutesPerTimeStep
              DO TS=1,NumOfTimeStepInHour
                DaySchedule(AddDaySch)%TSValue(Hr,TS)=SUM(MinuteValue(Hr,SCount:CurMinute))/REAL(MinutesPerTimeStep,r64)
                SCount=CurMinute+1
                CurMinute=CurMinute+MinutesPerTimeStep
              ENDDO
            ENDDO
          ELSE
            DO Hr=1,24
              CurMinute=MinutesPerTimeStep
              DO TS=1,NumOfTimeStepInHour
                DaySchedule(AddDaySch)%TSValue(Hr,TS)=MinuteValue(Hr,CurMinute)
                Curminute=CurMinute+MinutesPerTimeStep
              ENDDO
            ENDDO
          ENDIF
        ENDIF
        IF (iDay == 59 .and. rowCnt < 8784*hrlimitcount) THEN   ! 28 Feb
          ! Dup 28 Feb to 29 Feb (60)
          iDay=iDay+1
          Schedule(SchNum)%WeekSchedulePointer(iDay)=Schedule(SchNum)%WeekSchedulePointer(iDay-1)
        ENDIF
      END DO
    ENDIF

    IF  (AnyEnergyManagementSystemInModel) THEN ! setup constant schedules as actuators
      Call SetupEMSActuator('Schedule:File', &
          Schedule(SchNum)%Name, 'Schedule Value', '[ ]',&
          Schedule(SchNum)%EMSActuatedOn, Schedule(SchNum)%EMSValue )
    ENDIF
  END DO
  IF (NumCommaFileSchedules > 0) THEN
    DEALLOCATE(hourlyFileValues)
  ENDIF

  DEALLOCATE(MinuteValue)
  DEALLOCATE(SetMinuteValue)

  ! Constant Schedules
  CurrentModuleObject='Schedule:Constant'
  DO LoopIndex=1,NumConstantSchedules
    CALL GetObjectItem(CurrentModuleObject,LoopIndex,Alphas,NumAlphas,Numbers,NumNumbers,Status,  &
                   AlphaBlank=lAlphaBlanks,NumBlank=lNumericBlanks,  &
                   AlphaFieldnames=cAlphaFields,NumericFieldNames=cNumericFields)
    IsNotOK=.false.
    IsBlank=.false.
    CALL VerifyName(Alphas(1),Schedule(1:NumSchedules)%Name,SchNum,IsNotOK,IsBlank,TRIM(CurrentModuleObject)//' Name')
    IF (IsNotOK) THEN
      ErrorsFound=.true.
      IF (IsBlank) Alphas(1)='xxxxx'
    ENDIF
    SchNum = SchNum + 1
    Schedule(SchNum)%Name=Alphas(1)
    Schedule(SchNum)%SchType=ScheduleInput_constant
    ! Validate ScheduleType
    IF (NumScheduleTypes > 0) THEN
      CheckIndex=FindIteminList(Alphas(2),ScheduleType(1:NumScheduleTypes)%Name,NumScheduleTypes)
      IF (CheckIndex == 0) THEN
        IF (.not. lAlphaBlanks(2)) THEN
          CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(Alphas(1))//  &
             '", '//TRIM(cAlphaFields(2))//'="'//TRIM(Alphas(2))//  &
             '" not found -- will not be validated')
        ELSE
          CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(Alphas(1))//  &
             '", Blank '//TRIM(cAlphaFields(2))//' input -- will not be validated.')
        ENDIF
      ELSE
        Schedule(SchNum)%ScheduleTypePtr=CheckIndex
      ENDIF
    ENDIF
    AddWeekSch = AddWeekSch + 1
    AddDaySch = AddDaySch + 1
    ! define week schedule
    WeekSchedule(AddWeekSch)%Name=TRIM(Alphas(1))//'_wk_'
    ! for all day types point the week schedule to the newly defined day schedule
    DO kDayType = 1, MaxDayTypes
      WeekSchedule(AddWeekSch)%DaySchedulePointer(kDayType) = AddDaySch
    END DO
    ! day schedule
    DaySchedule(AddDaySch)%Name=TRIM(Alphas(1))//'_dy_'
    DaySchedule(AddDaySch)%ScheduleTypePtr = Schedule(SchNum)%ScheduleTypePtr
    ! schedule is pointing to the week schedule
    Schedule(SchNum)%WeekSchedulePointer = AddWeekSch
    curHrVal = Numbers(1)
    DaySchedule(AddDaySch)%TSValue = Numbers(1)

    IF  (AnyEnergyManagementSystemInModel) THEN ! setup constant schedules as actuators
      Call SetupEMSActuator('Schedule:Constant', &
          Schedule(SchNum)%Name, 'Schedule Value', '[ ]',&
          Schedule(SchNum)%EMSActuatedOn, Schedule(SchNum)%EMSValue )
    ENDIF
  END DO


  CurrentModuleObject='ExternalInterface:Schedule'
  DO LoopIndex=1,NumExternalInterfaceSchedules

    CALL GetObjectItem(CurrentModuleObject,LoopIndex,Alphas,NumAlphas,Numbers,NumNumbers,Status, &
                   AlphaBlank=lAlphaBlanks,NumBlank=lNumericBlanks,  &
                   AlphaFieldnames=cAlphaFields,NumericFieldNames=cNumericFields)
    IsNotOK=.false.
    IsBlank=.false.

    CALL VerifyName(Alphas(1),Schedule(1:NumSchedules)%Name,SchNum,IsNotOK,IsBlank,TRIM(CurrentModuleObject)//' Name') ! Bug fix
    IF (IsNotOK) THEN
      ErrorsFound=.true.
      IF (IsBlank) Alphas(1)='xxxxx'
    ENDIF
    SchNum=SchNum+1
    Schedule(SchNum)%Name=Alphas(1)
    Schedule(SchNum)%SchType=ScheduleInput_external

    ! Validate ScheduleType
    CheckIndex=FindIteminList(Alphas(2),ScheduleType(1:NumScheduleTypes)%Name,NumScheduleTypes)
    IF (CheckIndex == 0) THEN
        IF (.not. lAlphaBlanks(2)) THEN
          CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(Alphas(1))//  &
             '", '//TRIM(cAlphaFields(2))//'="'//TRIM(Alphas(2))//  &
             '" not found -- will not be validated')
        ELSE
          CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(Alphas(1))//  &
             '", Blank '//TRIM(cAlphaFields(2))//' input -- will not be validated.')
        ENDIF
    ELSE
      Schedule(SchNum)%ScheduleTypePtr=CheckIndex
    ENDIF
    AddWeekSch=AddWeekSch+1
    WeekSchedule(AddWeekSch)%Name=TRIM(Alphas(1))
    WeekSchedule(AddWeekSch)%Used=.true.
    DO Hr=1,366
      Schedule(SchNum)%WeekSchedulePointer(Hr)=AddWeekSch
    ENDDO
    AddDaySch=AddDaySch+1
    DaySchedule(AddDaySch)%Name=TRIM(Alphas(1))
    DaySchedule(AddDaySch)%ScheduleTypePtr=Schedule(SchNum)%ScheduleTypePtr
    DaySchedule(AddDaySch)%Used=.true.
    DO Hr = 1, MaxDayTypes
      WeekSchedule(AddWeekSch)%DaySchedulePointer(Hr) = AddDaySch
    END DO
!   Initialize the ExternalInterface day schedule for the ExternalInterface compact schedule.
!   It will be overwritten during run time stepping after the warm up period
    IF (NumNumbers<1) THEN
      CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(Alphas(1))//  &
         '", initial value is not numeric or is missing. Fix idf file.')
      NumErrorFlag=.true.
    ENDIF
    CALL ExternalInterfaceSetSchedule(AddDaySch, Numbers(1))

  ENDDO
  ! added for FMU Import
  CurrentModuleObject='ExternalInterface:FunctionalMockupUnitImport:To:Schedule'
  DO LoopIndex=1,NumExternalInterfaceFunctionalMockupUnitImportSchedules

    CALL GetObjectItem(CurrentModuleObject,LoopIndex,Alphas,NumAlphas,Numbers,NumNumbers,Status, &
                   AlphaBlank=lAlphaBlanks,NumBlank=lNumericBlanks,  &
                   AlphaFieldnames=cAlphaFields,NumericFieldNames=cNumericFields)
    IsNotOK=.false.
    IsBlank=.false.

    IF (NumExternalInterfaceSchedules .ge. 1) THEN
     CALL VerifyName(Alphas(1),Schedule(1:NumSchedules)%Name,SchNum,IsNotOK,IsBlank, 'The schedule object with the name "' &
              //TRIM(Alphas(1))//'" is defined as an ExternalInterface:Schedule and ' &
              //'ExternalInterface:FunctionalMockupUnitImport:To:Schedule. This will cause the schedule to be overwritten' &
              //' by PtolemyServer and FunctionalMockUpUnitImport.')
    ELSE
      CALL VerifyName(Alphas(1),Schedule(1:NumSchedules)%Name,SchNum,IsNotOK,IsBlank,TRIM(CurrentModuleObject)//' Name')
    END IF
      IF (IsNotOK) THEN
        ErrorsFound=.true.
        IF (IsBlank) Alphas(1)='xxxxx'
      ENDIF
   ! END IF
    SchNum=SchNum+1
    Schedule(SchNum)%Name=Alphas(1)
    Schedule(SchNum)%SchType=ScheduleInput_external

    ! Validate ScheduleType
    CheckIndex=FindIteminList(Alphas(2),ScheduleType(1:NumScheduleTypes)%Name,NumScheduleTypes)
    IF (CheckIndex == 0) THEN
        IF (.not. lAlphaBlanks(2)) THEN
          CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(Alphas(1))//  &
             '", '//TRIM(cAlphaFields(2))//'="'//TRIM(Alphas(2))//  &
             '" not found -- will not be validated')
        ELSE
          CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(Alphas(1))//  &
             '", Blank '//TRIM(cAlphaFields(2))//' input -- will not be validated.')
        ENDIF
    ELSE
      Schedule(SchNum)%ScheduleTypePtr=CheckIndex
    ENDIF
    AddWeekSch=AddWeekSch+1
    WeekSchedule(AddWeekSch)%Name=TRIM(Alphas(1))
    WeekSchedule(AddWeekSch)%Used=.true.
    DO Hr=1,366
      Schedule(SchNum)%WeekSchedulePointer(Hr)=AddWeekSch
    ENDDO
    AddDaySch=AddDaySch+1
    DaySchedule(AddDaySch)%Name=TRIM(Alphas(1))
    DaySchedule(AddDaySch)%ScheduleTypePtr=Schedule(SchNum)%ScheduleTypePtr
    DaySchedule(AddDaySch)%Used=.true.
    DO Hr = 1, MaxDayTypes
      WeekSchedule(AddWeekSch)%DaySchedulePointer(Hr) = AddDaySch
    END DO
!   Initialize the ExternalInterface day schedule for the ExternalInterface compact schedule.
!   It will be overwritten during run time stepping after the warm up period
    IF (NumNumbers<1) THEN
      CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(Alphas(1))//  &
         '", initial value is not numeric or is missing. Fix idf file.')
      NumErrorFlag=.true.
    ENDIF
    CALL ExternalInterfaceSetSchedule(AddDaySch, Numbers(1))

  ENDDO

  ! added for FMU Export
  CurrentModuleObject='ExternalInterface:FunctionalMockupUnitExport:To:Schedule'
  DO LoopIndex=1,NumExternalInterfaceFunctionalMockupUnitExportSchedules

    CALL GetObjectItem(CurrentModuleObject,LoopIndex,Alphas,NumAlphas,Numbers,NumNumbers,Status, &
                   AlphaBlank=lAlphaBlanks,NumBlank=lNumericBlanks,  &
                   AlphaFieldnames=cAlphaFields,NumericFieldNames=cNumericFields)
    IsNotOK=.false.
    IsBlank=.false.

    IF (NumExternalInterfaceSchedules .ge. 1) THEN
     CALL VerifyName(Alphas(1),Schedule(1:NumSchedules)%Name,SchNum,IsNotOK,IsBlank, 'The schedule object with the name "' &
              //TRIM(Alphas(1))//'" is defined as an ExternalInterface:Schedule and ' &
              //'ExternalInterface:FunctionalMockupUnitExport:To:Schedule. This will cause the schedule to be overwritten' &
              //' by PtolemyServer and FunctionalMockUpUnitExport.')
    ELSE
      CALL VerifyName(Alphas(1),Schedule(1:NumSchedules)%Name,SchNum,IsNotOK,IsBlank,TRIM(CurrentModuleObject)//' Name')
    END IF
      IF (IsNotOK) THEN
        ErrorsFound=.true.
        IF (IsBlank) Alphas(1)='xxxxx'
      ENDIF

    SchNum=SchNum+1
    Schedule(SchNum)%Name=Alphas(1)
    Schedule(SchNum)%SchType=ScheduleInput_external

    ! Validate ScheduleType
    CheckIndex=FindIteminList(Alphas(2),ScheduleType(1:NumScheduleTypes)%Name,NumScheduleTypes)
    IF (CheckIndex == 0) THEN
        IF (.not. lAlphaBlanks(2)) THEN
          CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(Alphas(1))//  &
             '", '//TRIM(cAlphaFields(2))//'="'//TRIM(Alphas(2))//  &
             '" not found -- will not be validated')
        ELSE
          CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(Alphas(1))//  &
             '", Blank '//TRIM(cAlphaFields(2))//' input -- will not be validated.')
        ENDIF
    ELSE
      Schedule(SchNum)%ScheduleTypePtr=CheckIndex
    ENDIF
    AddWeekSch=AddWeekSch+1
    WeekSchedule(AddWeekSch)%Name=TRIM(Alphas(1))
    WeekSchedule(AddWeekSch)%Used=.true.
    DO Hr=1,366
      Schedule(SchNum)%WeekSchedulePointer(Hr)=AddWeekSch
    ENDDO
    AddDaySch=AddDaySch+1
    DaySchedule(AddDaySch)%Name=TRIM(Alphas(1))
    DaySchedule(AddDaySch)%ScheduleTypePtr=Schedule(SchNum)%ScheduleTypePtr
    DaySchedule(AddDaySch)%Used=.true.
    DO Hr = 1, MaxDayTypes
      WeekSchedule(AddWeekSch)%DaySchedulePointer(Hr) = AddDaySch
    END DO
!   Initialize the ExternalInterface day schedule for the ExternalInterface compact schedule.
!   It will be overwritten during run time stepping after the warm up period
    IF (NumNumbers<1) THEN
      CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(Alphas(1))//  &
         '", initial value is not numeric or is missing. Fix idf file.')
      NumErrorFlag=.true.
    ENDIF
    CALL ExternalInterfaceSetSchedule(AddDaySch, Numbers(1))

  ENDDO


  ! Validate by ScheduleLimitsType
  DO SchNum=1,NumSchedules
    NumPointer=Schedule(SchNum)%ScheduleTypePtr
    IF (.not. ScheduleType(NumPointer)%Limited) CYCLE
    IF (CheckScheduleValueMinMax(SchNum,'>=',ScheduleType(NumPointer)%Minimum,'<=',ScheduleType(NumPointer)%Maximum)) CYCLE
    CALL ShowSevereError(RoutineName//'Schedule="'//TRIM(Schedule(SchNum)%Name)//  &
        '" has values outside its Schedule Type ('//    &
        TRIM(ScheduleType(NumPointer)%Name)//') range')
    CALL ShowContinueError('  Minimum should be >='//TRIM(RoundSigDigits(ScheduleType(NumPointer)%Minimum,3))//     &
                           ' and Maximum should be <='//TRIM(RoundSigDigits(ScheduleType(NumPointer)%Maximum,3)))
    ErrorsFound=.true.
  ENDDO

  IF (ErrorsFound) THEN
    CALL ShowFatalError(RoutineName//'Preceding Errors cause termination.')
  ENDIF

  IF (NumScheduleTypes+NumDaySchedules+NumWeekSchedules+NumSchedules > 0) THEN  ! Report to EIO file
    CurrentModuleObject='Output:Schedules'
    NumFields=GetNumObjectsFound(CurrentModuleObject)

!    RptSchedule=.false.
    RptLevel=1
    DO Count=1,NumFields
      CALL GetObjectItem(CurrentModuleObject,Count,Alphas,NumAlphas,Numbers,NumNumbers,Status)
!      RptSchedule=.true.

      SELECT CASE (Alphas(1))

        CASE ('HOURLY')
          RptLevel=1
          CALL ReportScheduleDetails(RptLevel)

        CASE ('TIMESTEP','DETAILED')
          RptLevel=2
          CALL ReportScheduleDetails(RptLevel)

        CASE ('IDF')
          RptLevel=3
          CALL ReportScheduleDetails(RptLevel)

        CASE DEFAULT
          CALL ShowWarningError(RoutineName//'Report for Schedules should specify "HOURLY" or "TIMESTEP" ("DETAILED")')
          CALL ShowContinueError('HOURLY report will be done')
          RptLevel=1
          CALL ReportScheduleDetails(RptLevel)

      END SELECT
    ENDDO
  ENDIF

  DEALLOCATE(Alphas)
  DEALLOCATE(cAlphaFields)
  DEALLOCATE(cNumericFields)
  DEALLOCATE(Numbers)
  DEALLOCATE(lAlphaBlanks)
  DEALLOCATE(lNumericBlanks)

  WRITE(UnitNumber,*) ' Processing Schedule Input -- Complete'

RETURN

END SUBROUTINE ProcessScheduleInput

SUBROUTINE ReportScheduleDetails(LevelOfDetail)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   January 2003
          !       MODIFIED       February 2008 - add IDF outputs (compact schedules)
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine puts the details of the Schedules on the .eio file (Inits file).

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE General, ONLY: RoundSigDigits, InvJulianDay
  USE DataGlobals, ONLY: OutputFileDebug

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: LevelOfDetail  ! =1: hourly; =2: timestep; = 3: make IDF excerpt

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER, DIMENSION(12)   :: Months=(/'Jan','Feb','Mar','Apr','May','Jun',  &
                                                           'Jul','Aug','Sep','Oct','Nov','Dec'/)
  CHARACTER(len=*), PARAMETER, DIMENSION(0:24) :: HrField=(/"00","01","02","03","04","05","06",  &
                                                            "07","08","09","10","11","12",  &
                                                            "13","14","15","16","17","18",  &
                                                            "19","20","21","22","23","24"/)
  CHARACTER(len=*), PARAMETER :: SchTFmt0="('! Schedule Details Report=',A,' =====================')"
  CHARACTER(len=*), PARAMETER :: SchTFmt="('! <ScheduleType>,Name,Limited? {Yes/No},Minimum,Maximum,',  &
                                           & 'Continuous? {Yes/No - Discrete}')"
  CHARACTER(len=*), PARAMETER :: SchSFmt="('! <Schedule>,Name,ScheduleType,{Until Date,WeekSchedule}** Repeated until Dec 31')"
  CHARACTER(len=*), PARAMETER :: SchTFmtdata="('ScheduleTypeLimits',5(',',A))"
  CHARACTER(len=*), PARAMETER :: SchWFmtdata="('Schedule:Week:Daily',13(',',A))"
  CHARACTER(len=*), PARAMETER :: fmta='(A)'
  CHARACTER(len=*), PARAMETER :: CMinFmt='(I2.2)'
  CHARACTER(len=*), PARAMETER :: ThruFmt="(',Through ',A,1X,I2.2,',',A)"
  CHARACTER(len=*), PARAMETER :: SchDFmt0="('! <DaySchedule>,Name,ScheduleType,Interpolated {Yes/No},Time (HH:MM) =>',"
  CHARACTER(len=*), PARAMETER :: SchDFmtdata0="('DaySchedule,',A,',',A,',',A,',',A,"

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER Count
  INTEGER Hr
  INTEGER TS
  INTEGER NumF
  INTEGER PMon
  INTEGER PDay
  INTEGER iWeek
  INTEGER iDay
  INTEGER DT
  INTEGER iDayP
  CHARACTER(len=2), ALLOCATABLE, DIMENSION(:) :: ShowMinute
  INTEGER CurMinute
  CHARACTER(len=5), ALLOCATABLE, DIMENSION(:) :: TimeHHMM
  CHARACTER(len=500) :: SchWFmt="('! <WeekSchedule>,Name"
  CHARACTER(len=100) :: SchDFmt=' '
  CHARACTER(len=100) :: SchDFmtdata=' '
  CHARACTER(len=3) :: YesNo1
  CHARACTER(len=3) :: YesNo2
  CHARACTER(len=32) :: Num1
  CHARACTER(len=32) :: Num2
  CHARACTER(len=32), ALLOCATABLE, DIMENSION(:,:)  :: RoundTSValue

  ALLOCATE(ShowMinute(NumOfTimeStepInHour))
  ALLOCATE(TimeHHMM(NumOfTimeStepInHour*24))
  ALLOCATE(RoundTSValue(24,NumOfTimeStepInHour))
  ShowMinute=Blank
  TimeHHMM=Blank
  RoundTSValue=Blank

  CurMinute=MinutesPerTimeStep
  DO Count=1,NumOfTimeStepInHour-1
    WRITE(ShowMinute(Count),CMinFmt) CurMinute
    CurMinute=CurMinute+MinutesPerTimeStep
  ENDDO
  ShowMinute(NumOfTimeStepInHour)='00'

  SELECT CASE (LevelOfDetail)

    CASE(1:2)
      NumF=1
      DO Hr=1,24
        IF (LevelOfDetail == 2) THEN
          DO TS=1,NumOfTimeStepInHour-1
            TimeHHMM(NumF)=HrField(Hr-1)//':'//ShowMinute(TS)
            NumF=NumF+1
          ENDDO
        ENDIF
        TimeHHMM(NumF)=HrField(Hr)//':'//ShowMinute(NumOfTimeStepInHour)
        NumF=NumF+1
      ENDDO
      NumF=NumF-1

      ! SchTFmt Schedule Types Header
      IF (LevelOfDetail == 1) THEN
        WRITE(OutputFileInits,SchTFmt0) 'Hourly'
        SchDFmt=TRIM(SchDFmt0)//"24(',',A))"
        SchDFmtdata=TRIM(SchDFmtdata0)//"24(',',A))"
      ELSE
        WRITE(OutputFileInits,SchTFmt0) 'Timestep'
        WRITE(Num1,*) NumOfTimeStepInHour*24
        Num1=ADJUSTL(Num1)
        SchDFmt=TRIM(SchDFmt0)//TRIM(Num1)//"(',',A))"
        SchDFmtdata=TRIM(SchDFmtdata0)//TRIM(Num1)//"(',',A))"
      ENDIF

      WRITE(OutputFileInits,SchTFmt)
      ! SchDFmt Header (DaySchedule) builds the appropriate set of commas/times based on detail level
!      DO Count=1,NumF
!        SchDFmt=TRIM(SchDFmt)//'A'
!        IF (Count /= NumF) SchDFmt=TRIM(SchDFmt)//",',',"
!      ENDDO
!      SchDFmt=TRIM(SchDFmt)//')'
      WRITE(OutputFileInits,SchDFmt)  (TimeHHMM(Count),Count=1,NumF)
      ! SchWFmt Header (WeekSchedule)
      DO Count=1,MaxDayTypes
        SchWFmt=TRIM(SchWFmt)//','//TRIM(ValidDayTypes(Count))
      ENDDO
      SchWFmt=TRIM(SchWFmt)//"')"
      WRITE(OutputFileInits,SchWFmt)
      WRITE(OutputFileInits,SchSFmt)

      DO Count=1,NumScheduleTypes
        IF (ScheduleType(Count)%Limited) THEN
          YesNo1='Yes'
          Num1=RoundSigDigits(ScheduleType(Count)%Minimum,2)
          Num1=ADJUSTL(Num1)
          Num2=RoundSigDigits(ScheduleType(Count)%Maximum,2)
          Num2=ADJUSTL(Num2)
          IF (ScheduleType(Count)%IsReal) THEN
            YesNo2='Yes'
          ELSE
            YesNo2='No'
            WRITE(Num1,*) INT(ScheduleType(Count)%Minimum)
            Num1=ADJUSTL(Num1)
            WRITE(Num2,*) INT(ScheduleType(Count)%Maximum)
            Num2=ADJUSTL(Num2)
          ENDIF
        ELSE
          YesNo1='No'
          Num1='N/A'
          Num2='N/A'
          YesNo2='N/A'
        ENDIF
        WRITE(OutputFileInits,SchTFmtdata) TRIM(ScheduleType(Count)%Name),TRIM(YesNo1),TRIM(Num1),TRIM(Num2),TRIM(YesNo2)
      ENDDO

!      WRITE(Num1,*) NumOfTimeStepInHour*24
!      Num1=ADJUSTL(Num1)
!      SchDFmtdata=TRIM(SchDFmtdata)//TRIM(Num1)//"(',',A))"
      DO Count=1,NumDaySchedules
        IF (DaySchedule(Count)%IntervalInterpolated) THEN
          YesNo1='Yes'
        ELSE
          YesNo1='No'
        ENDIF
        DO Hr=1,24
          DO TS=1,NumOfTimeStepInHour
            RoundTSValue(Hr,TS)=RoundSigDigits(DaySchedule(Count)%TSValue(Hr,TS),2)
          ENDDO
        ENDDO
        IF (LevelOfDetail == 1) THEN
          WRITE(OutputFileInits,SchDFmtdata) TRIM(DaySchedule(Count)%Name),  &
              TRIM(ScheduleType(DaySchedule(Count)%ScheduleTypePtr)%Name),TRIM(YesNo1),'Values:',  &
              (TRIM(RoundTSValue(Hr,NumOfTimeStepInHour)),Hr=1,24)
        ELSEIF (LevelOfDetail == 2) THEN
          WRITE(OutputFileInits,SchDFmtdata) TRIM(DaySchedule(Count)%Name),  &
              TRIM(ScheduleType(DaySchedule(Count)%ScheduleTypePtr)%Name),TRIM(YesNo1),'Values:',  &
              ((TRIM(RoundTSValue(Hr,TS)),TS=1,NumOfTimeStepInHour),Hr=1,24)
        ENDIF
      ENDDO

      DO Count=1,NumWeekSchedules
        WRITE(OutputFileInits,SchWFmtdata) TRIM(WeekSchedule(Count)%Name),  &
            (TRIM(DaySchedule(WeekSchedule(Count)%DaySchedulePointer(NumF))%Name),NumF=1,MaxDayTypes)
      ENDDO

      DO Count=1,NumSchedules
        NumF=1
        WRITE(OutputFileInits,"('Schedule,',A,',',A)",ADVANCE='No') TRIM(Schedule(Count)%Name),  &
                        TRIM(ScheduleType(Schedule(Count)%ScheduleTypePtr)%Name)
        DO WHILE (NumF <= 366)
          TS=Schedule(Count)%WeekSchedulePointer(NumF)
          DO WHILE (Schedule(Count)%WeekSchedulePointer(NumF) == TS .and. NumF <= 366)
            IF (NumF == 366) THEN
              CALL InvJulianDay(NumF,PMon,PDay,1)
              WRITE(OutputFileInits,ThruFmt,ADVANCE='No') TRIM(Months(PMon)),PDay,  &
                                     TRIM(WeekSchedule(TS)%Name)
            ENDIF
            NumF=NumF+1
            IF (NumF > 366) EXIT  ! compound If might have a problem unless this included.
          ENDDO
          IF (NumF <= 366) THEN
            CALL InvJulianDay(NumF-1,PMon,PDay,1)
            WRITE(OutputFileInits,ThruFmt,ADVANCE='No') TRIM(Months(PMon)),PDay,  &
                                   TRIM(WeekSchedule(TS)%Name)
          ENDIF
        ENDDO
        WRITE(OutputFileInits,'(1X)')
      ENDDO

    CASE(3)
      DO Count=1,NumSchedules
        WRITE(OutputFileDebug,'(A)') ' '
        WRITE(OutputFileDebug,'(A)') '  Schedule:Compact,'
        WRITE(OutputFileDebug,'(A)') '    '//TRIM(Schedule(Count)%Name)//',           !- Name'
        WRITE(OutputFileDebug,'(A)') '    '//TRIM(ScheduleType(Schedule(Count)%ScheduleTypePtr)%Name)//  &
                                       ',          !- ScheduleTypeLimits'
        NumF=1
        DO WHILE (NumF <= 366)
          TS=Schedule(Count)%WeekSchedulePointer(NumF)
          DO WHILE (Schedule(Count)%WeekSchedulePointer(NumF) == TS .and. NumF <= 366)
            IF (NumF == 366) THEN
              CALL InvJulianDay(NumF,PMon,PDay,1)
              WRITE(OutputFileDebug,'(A)') '    Through: '//TRIM(RoundSigDigits(PMon))// &
                         '/'//TRIM(RoundSigDigits(PDay))//','
              iDayP=0
              DO DT=2,6
                WRITE(OutputFileDebug,'(A)') '    For: '//TRIM(ValidDayTypes(DT))//','
                iWeek=Schedule(Count)%WeekSchedulePointer(NumF-1)
                iDay=WeekSchedule(iWeek)%DaySchedulePointer(DT)
                if (iDay /= iDayP) then
                  DO Hr=1,24
                    WRITE(OutputFileDebug,'(A)') '    Until: '//TRIM(RoundSigDigits(Hr))//':'//  &
                         TRIM(ShowMinute(NumOfTimeStepInHour))//','//  &
                         TRIM(RoundSigDigits(DaySchedule(iDay)%TSValue(Hr,NumOfTimeStepInHour),2))//','
                  ENDDO
                else
                  WRITE(OutputFileDebug,'(A)') '    Same as previous'
                endif
                iDayP=iDay
              ENDDO
              DT=1
                WRITE(OutputFileDebug,'(A)') '    For: '//TRIM(ValidDayTypes(DT))//','
                iWeek=Schedule(Count)%WeekSchedulePointer(NumF-1)
                iDay=WeekSchedule(iWeek)%DaySchedulePointer(DT)
                if (iDay /= iDayP) then
                  DO Hr=1,24
                    WRITE(OutputFileDebug,'(A)') '    Until: '//TRIM(RoundSigDigits(Hr))//':'//  &
                         TRIM(ShowMinute(NumOfTimeStepInHour))//','//  &
                         TRIM(RoundSigDigits(DaySchedule(iDay)%TSValue(Hr,NumOfTimeStepInHour),2))//','
                  ENDDO
                else
                  WRITE(OutputFileDebug,'(A)') '    Same as previous'
                endif
                iDayP=iDay
              DO DT=7,MaxDayTypes
                WRITE(OutputFileDebug,'(A)') '    For: '//TRIM(ValidDayTypes(DT))//','
                iWeek=Schedule(Count)%WeekSchedulePointer(NumF-1)
                iDay=WeekSchedule(iWeek)%DaySchedulePointer(DT)
                if (iDay /= iDayP) then
                  DO Hr=1,24
                    WRITE(OutputFileDebug,'(A)') '    Until: '//TRIM(RoundSigDigits(Hr))//':'//  &
                         TRIM(ShowMinute(NumOfTimeStepInHour))//','//  &
                         TRIM(RoundSigDigits(DaySchedule(iDay)%TSValue(Hr,NumOfTimeStepInHour),2))//','
                  ENDDO
                else
                  WRITE(OutputFileDebug,'(A)') '    Same as previous'
                endif
                iDayP=iDay
              ENDDO
            ENDIF
            NumF=NumF+1
            IF (NumF > 366) EXIT  ! compound If might have a problem unless this included.
          ENDDO
          IF (NumF <= 366) THEN
            CALL InvJulianDay(NumF-1,PMon,PDay,1)
            WRITE(OutputFileDebug,'(A)') '    Through: '//TRIM(RoundSigDigits(PMon))// &
                         '/'//TRIM(RoundSigDigits(PDay))//','
              iDayP=0
              DO DT=2,6
                WRITE(OutputFileDebug,'(A)') '    For: '//TRIM(ValidDayTypes(DT))//','
                iWeek=Schedule(Count)%WeekSchedulePointer(NumF-1)
                iDay=WeekSchedule(iWeek)%DaySchedulePointer(DT)
                if (iDay /= iDayP) then
                  DO Hr=1,24
                    WRITE(OutputFileDebug,'(A)') '    Until: '//TRIM(RoundSigDigits(Hr))//':'//  &
                         TRIM(ShowMinute(NumOfTimeStepInHour))//','//  &
                         TRIM(RoundSigDigits(DaySchedule(iDay)%TSValue(Hr,NumOfTimeStepInHour),2))//','
                  ENDDO
                else
                  WRITE(OutputFileDebug,'(A)') '    Same as previous'
                endif
                iDayP=iDay
              ENDDO
              DT=1
                WRITE(OutputFileDebug,'(A)') '    For: '//TRIM(ValidDayTypes(DT))//','
                iWeek=Schedule(Count)%WeekSchedulePointer(NumF-1)
                iDay=WeekSchedule(iWeek)%DaySchedulePointer(DT)
                if (iDay /= iDayP) then
                  DO Hr=1,24
                    WRITE(OutputFileDebug,'(A)') '    Until: '//TRIM(RoundSigDigits(Hr))//':'//  &
                         TRIM(ShowMinute(NumOfTimeStepInHour))//','//  &
                         TRIM(RoundSigDigits(DaySchedule(iDay)%TSValue(Hr,NumOfTimeStepInHour),2))//','
                  ENDDO
                else
                  WRITE(OutputFileDebug,'(A)') '    Same as previous'
                endif
                iDayP=iDay
              DO DT=7,MaxDayTypes
                WRITE(OutputFileDebug,'(A)') '    For: '//TRIM(ValidDayTypes(DT))//','
                iWeek=Schedule(Count)%WeekSchedulePointer(NumF-1)
                iDay=WeekSchedule(iWeek)%DaySchedulePointer(DT)
                if (iDay /= iDayP) then
                  DO Hr=1,24
                    WRITE(OutputFileDebug,'(A)') '    Until: '//TRIM(RoundSigDigits(Hr))//':'//  &
                         TRIM(ShowMinute(NumOfTimeStepInHour))//','//  &
                         TRIM(RoundSigDigits(DaySchedule(iDay)%TSValue(Hr,NumOfTimeStepInHour),2))//','
                  ENDDO
                else
                  WRITE(OutputFileDebug,'(A)') '    Same as previous'
                endif
                iDayP=iDay
              ENDDO
          ENDIF
        ENDDO
      ENDDO

    CASE DEFAULT
  END SELECT

  DEALLOCATE(ShowMinute)
  DEALLOCATE(TimeHHMM)
  DEALLOCATE(RoundTSValue)

  RETURN

END SUBROUTINE ReportScheduleDetails

REAL(r64) FUNCTION GetCurrentScheduleValue(ScheduleIndex)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   September 1997
          !       MODIFIED       August 2011; adapt Autodesk changes (time reduction)
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function returns the hourly schedule value for the current day.

          ! METHODOLOGY EMPLOYED:
          ! Use internal Schedule data structure to return value.  Note that missing values in
          ! input will equate to 0 indices in arrays -- which has been set up to return legally with
          ! 0.0 values.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER ScheduleIndex

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
          ! na

  IF (.not. ScheduleDSTSFileWarningIssued) THEN
    IF (DSTIndicator == 1) THEN
      IF (Schedule(ScheduleIndex)%SchType == ScheduleInput_file) THEN
        CALL ShowWarningError('GetCurrentScheduleValue: Schedule="'//trim(Schedule(ScheduleIndex)%Name)//  &
           '" is a Schedule:File')
        CALL ShowContinueError('...Use of Schedule:File when DaylightSavingTime is in effect is not recommended.')
        CALL ShowContinueError('...1) Remove RunperiodControl:DaylightSavingTime object or remove DST period from Weather File.')
        CALL ShowContinueError('...2) Configure other schedules and Schedule:File to account for occupant behavior during DST.')
        CALL ShowContinueError('...   If you have already done this, you can ignore this message.')
        CALL ShowContinueError('...When active, DaylightSavingTime will shift all scheduled items by one hour, '//  &
           'retaining the same day type as the original.')
        ScheduleDSTSFileWarningIssued=.true.
      ENDIF
    ENDIF
  ENDIF

  IF (ScheduleIndex == -1) THEN
    GetCurrentScheduleValue=1.0d0
    RETURN
  ELSEIF (ScheduleIndex == 0) THEN
    GetCurrentScheduleValue=0.0d0
    RETURN
  ENDIF

  IF (.not. Schedule(ScheduleIndex)%EMSActuatedOn) THEN
    GetCurrentScheduleValue=Schedule(ScheduleIndex)%CurrentValue
  ELSE
    GetCurrentScheduleValue=Schedule(ScheduleIndex)%EMSValue
  ENDIF

  RETURN

END FUNCTION GetCurrentScheduleValue

SUBROUTINE UpdateScheduleValues

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   August 2011; adapted from Autodesk (time reduction)
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This routine calculates all the scheduled values as a time reduction measure and
          ! stores them in the CurrentValue item of the schedule data structure.

          ! METHODOLOGY EMPLOYED:
          ! Use internal Schedule data structure to calculate current value.  Note that missing values in
          ! input will equate to 0 indices in arrays -- which has been set up to return legally with
          ! 0.0 values.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataEnvironment, ONLY: DayOfYear_Schedule

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
  INTEGER ScheduleIndex
  INTEGER WhichHour
  INTEGER WeekSchedulePointer
  INTEGER DaySchedulePointer

  IF (.not. ScheduleInputProcessed) THEN
    CALL ProcessScheduleInput
    ScheduleInputProcessed=.true.
  ENDIF

  WhichHour=HourOfDay+DSTIndicator

  DO ScheduleIndex=1,NumSchedules

    ! Determine which Week Schedule is used
    !  Cant use stored day of year because of leap year inconsistency
    WeekSchedulePointer=Schedule(ScheduleIndex)%WeekSchedulePointer(DayOfYear_Schedule)

    ! Now, which day?
    IF (DayofWeek <= 7 .and. HolidayIndex > 0) THEN
      DaySchedulePointer=WeekSchedule(WeekSchedulePointer)%DaySchedulePointer(7+HolidayIndex)
    ELSE
      DaySchedulePointer=WeekSchedule(WeekSchedulePointer)%DaySchedulePointer(DayofWeek)
    ENDIF

    ! Hourly Value
    IF (WhichHour <= 24) THEN
      Schedule(ScheduleIndex)%CurrentValue=DaySchedule(DaySchedulePointer)%TSValue(WhichHour,TimeStep)
    ELSEIF (TimeStep <= NumOfTimeStepInHour) THEN
      Schedule(ScheduleIndex)%CurrentValue=DaySchedule(DaySchedulePointer)%TSValue(WhichHour-24,TimeStep)
    ELSE
      Schedule(ScheduleIndex)%CurrentValue=DaySchedule(DaySchedulePointer)%TSValue(WhichHour-24,NumOfTimeStepInHour)
    ENDIF

  ENDDO

  RETURN

END SUBROUTINE UpdateScheduleValues

REAL(r64) FUNCTION LookUpScheduleValue(ScheduleIndex, ThisHour, ThisTimeStep, ThisDayOfYear)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   January 2003
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function provides a method to look up schedule values for any hour, timestep, day
          ! of the year (rather than just the "current time").

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataEnvironment, ONLY: DayOfYear_Schedule
  USE General,         ONLY: JulianDay

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER ScheduleIndex
  INTEGER, OPTIONAL :: ThisHour
  INTEGER, OPTIONAL :: ThisTimeStep
  INTEGER, OPTIONAL :: ThisDayOfYear

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER WeekSchedulePointer
  INTEGER DaySchedulePointer
  INTEGER WhichHour
  INTEGER WhichTimeStep

  IF (.not. ScheduleInputProcessed) THEN
    CALL ProcessScheduleInput
    ScheduleInputProcessed=.true.
  ENDIF

  IF (ScheduleIndex == -1) THEN
    LookUpScheduleValue=1.0d0
    RETURN
  ELSEIF (ScheduleIndex == 0) THEN
    LookUpScheduleValue=0.0d0
    RETURN
  ENDIF

  IF (.not. PRESENT(ThisHour)) THEN
    LookUpScheduleValue=GetCurrentScheduleValue(ScheduleIndex)

!  ELSEIF (ThisHour == 0) THEN  ! odd answers when thishour=0 (initialization of shadowing)
!    LookUpScheduleValue=GetCurrentScheduleValue(ScheduleIndex)

  ELSEIF (.not. PRESENT(ThisDayOfYear)) THEN ! ThisHour present, check other optional parameters
    !  so, current date, but maybe TimeStep added

    ! Determine which Week Schedule is used
    !  Cant use stored day of year because of leap year inconsistency
    WeekSchedulePointer=Schedule(ScheduleIndex)%WeekSchedulePointer(DayOfYear_Schedule)

    ! Now, which day?
    IF (DayofWeek <= 7 .and. HolidayIndex > 0) THEN
      DaySchedulePointer=WeekSchedule(WeekSchedulePointer)%DaySchedulePointer(7+HolidayIndex)
    ELSE
      DaySchedulePointer=WeekSchedule(WeekSchedulePointer)%DaySchedulePointer(DayofWeek)
    ENDIF


    ! Hourly Value
    WhichHour=HourOfDay+DSTIndicator
    IF (WhichHour <= 24) THEN
      LookUpScheduleValue=DaySchedule(DaySchedulePointer)%TSValue(WhichHour,TimeStep)
    ELSE
      LookUpScheduleValue=DaySchedule(DaySchedulePointer)%TSValue(WhichHour-24,TimeStep)
    ENDIF
    WhichHour=ThisHour
    DO WHILE (WhichHour < 1)
      WhichHour=WhichHour+24
    END DO
    IF (WhichHour > 24) THEN
      DO WHILE (WhichHour > 24)
        WeekSchedulePointer=Schedule(ScheduleIndex)%WeekSchedulePointer(JulianDay(MonthTomorrow,DayOfMonthTomorrow,1))
        IF (DayofWeekTomorrow <=7 .and. HolidayIndexTomorrow > 0) THEN
          DaySchedulePointer=WeekSchedule(WeekSchedulePointer)%DaySchedulePointer(7+HolidayIndexTomorrow)
        ELSE
          DaySchedulePointer=WeekSchedule(WeekSchedulePointer)%DaySchedulePointer(DayofWeekTomorrow)
        ENDIF
        WhichHour=WhichHour-24
      END DO
    ELSE
      ! Determine which Week Schedule is used
      !  Cant use stored day of year because of leap year inconsistency
      WeekSchedulePointer=Schedule(ScheduleIndex)%WeekSchedulePointer(DayOfYear_Schedule)

      ! Now, which day?
      IF (DayofWeek <= 7 .and. HolidayIndex > 0) THEN
        DaySchedulePointer=WeekSchedule(WeekSchedulePointer)%DaySchedulePointer(7+HolidayIndex)
      ELSE
        DaySchedulePointer=WeekSchedule(WeekSchedulePointer)%DaySchedulePointer(DayofWeek)
      ENDIF
    ENDIF
    WhichHour=WhichHour+DSTIndicator
    IF (PRESENT(ThisTimeStep)) THEN
      IF (ThisTimeStep == 0) THEN
        WhichTimeStep=NumOfTimeStepInHour
      ELSE
        WhichTimeStep=ThisTimeStep
      ENDIF
      IF (WhichHour <= 24) THEN
        LookUpScheduleValue=DaySchedule(DaySchedulePointer)%TSValue(WhichHour,WhichTimeStep)
      ELSEIF (ThisTimeStep <= NumOfTimeStepInHour) THEN
        LookUpScheduleValue=DaySchedule(DaySchedulePointer)%TSValue(WhichHour-24,WhichTimeStep)
      ELSE
        LookUpScheduleValue=DaySchedule(DaySchedulePointer)%TSValue(WhichHour-24,NumOfTimeStepInHour)
      ENDIF
    ELSE
      IF (WhichHour <= 24) THEN
        LookUpScheduleValue=DaySchedule(DaySchedulePointer)%TSValue(WhichHour,NumOfTimeStepInHour)
      ELSE
        LookUpScheduleValue=DaySchedule(DaySchedulePointer)%TSValue(WhichHour-24,NumOfTimeStepInHour)
      ENDIF
    ENDIF

  ELSE ! date present, not ready for that yet.
    CALL ShowFatalError('DayofYear Requested in LookUpScheduleValue, not implemented yet')
  ENDIF

  RETURN

END FUNCTION LookUpScheduleValue

INTEGER FUNCTION GetScheduleIndex(ScheduleName)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   September 1997
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function returns the internal pointer to Schedule "ScheduleName".

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: FindItemInList

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=*) ScheduleName

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER :: DayCtr
  INTEGER :: WeekCtr

  IF (.not. ScheduleInputProcessed) THEN
    CALL ProcessScheduleInput
    ScheduleInputProcessed=.True.
  ENDIF

  IF (NumSchedules > 0) THEN
    GetScheduleIndex=FindIteminList(ScheduleName,Schedule(1:NumSchedules)%Name,NumSchedules)
    IF (GetScheduleIndex >0) THEN
      IF (.not. Schedule(GetScheduleIndex)%Used) THEN
        Schedule(GetScheduleIndex)%Used=.true.
        DO WeekCtr=1,366
          IF (Schedule(GetScheduleIndex)%WeekSchedulePointer(WeekCtr) > 0) THEN
            WeekSchedule(Schedule(GetScheduleIndex)%WeekSchedulePointer(WeekCtr))%Used=.true.
            DO DayCtr=1,MaxDayTypes
              DaySchedule(  &
                 WeekSchedule(  &
                    Schedule(GetScheduleIndex)%WeekSchedulePointer(WeekCtr))%DaySchedulePointer(DayCtr))%Used=.true.
            ENDDO
          ENDIF
        ENDDO
      ENDIF
    ENDIF
  ELSE
    GetScheduleIndex=0
  ENDIF

  RETURN

END FUNCTION GetScheduleIndex

FUNCTION GetScheduleType(ScheduleIndex) RESULT (TypeOfSchedule)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Jason Glazer
          !       DATE WRITTEN   July 2007
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function returns the internal pointer to Schedule "ScheduleName".

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: ScheduleIndex
  CHARACTER(len=MaxNameLength) :: TypeOfSchedule

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER :: curSchType

  IF (.not. ScheduleInputProcessed) THEN
    CALL ProcessScheduleInput
    ScheduleInputProcessed=.True.
  ENDIF

  IF ((ScheduleIndex .gt. 0) .AND. (ScheduleIndex .le. NumSchedules)) THEN
    curSchType = Schedule(ScheduleIndex)%ScheduleTypePtr
    IF ((curSchType .gt. 0) .AND. (curSchType .le. NumScheduleTypes)) THEN
      TypeOfSchedule = ScheduleType(curSchType)%Name
    ELSE
      TypeOfSchedule = ''
    END IF
  ELSE
    TypeOfSchedule = ''
  ENDIF
END FUNCTION GetScheduleType

INTEGER FUNCTION GetDayScheduleIndex(ScheduleName)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   August 2003
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function returns the internal pointer to Day Schedule "ScheduleName".

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: FindItemInList

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=*) ScheduleName

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
          ! na

  IF (.not. ScheduleInputProcessed) THEN
    CALL ProcessScheduleInput
    ScheduleInputProcessed=.True.
  ENDIF

  IF (NumDaySchedules > 0) THEN
    GetDayScheduleIndex=FindIteminList(ScheduleName,DaySchedule(1:NumDaySchedules)%Name,NumDaySchedules)
    IF (GetDayScheduleIndex >0) THEN
      DaySchedule(GetDayScheduleIndex)%Used=.true.
    ENDIF
  ELSE
    GetDayScheduleIndex=0
  ENDIF

  RETURN

END FUNCTION GetDayScheduleIndex

SUBROUTINE GetScheduleValuesForDay(ScheduleIndex,DayValues,JDay,CurDayofWeek)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   September 1997
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine returns an entire day's worth of schedule values.

          ! METHODOLOGY EMPLOYED:
          ! Use internal data to fill DayValues array.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataEnvironment, ONLY: DayOfYear_Schedule

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)           :: ScheduleIndex
  REAL(r64), INTENT(OUT)        :: DayValues(:,:)
  INTEGER, INTENT(IN), OPTIONAL :: JDay
  INTEGER, INTENT(IN), OPTIONAL :: CurDayofWeek

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER WeekSchedulePointer
  INTEGER DaySchedulePointer

  IF (.not. ScheduleInputProcessed) THEN
    CALL ProcessScheduleInput
    ScheduleInputProcessed=.True.
  ENDIF

  IF (ScheduleIndex == -1) THEN
    DayValues(1:24,1:NumOfTimeStepInHour)=1.0d0
    RETURN
  ELSEIF (ScheduleIndex == 0) THEN
    DayValues(1:24,1:NumOfTimeStepInHour)=0.0d0
    RETURN
  ENDIF

  ! Determine which Week Schedule is used
  IF (.not. PRESENT(JDay)) THEN
    WeekSchedulePointer=Schedule(ScheduleIndex)%WeekSchedulePointer(DayOfYear_Schedule)
  ELSE
    WeekSchedulePointer=Schedule(ScheduleIndex)%WeekSchedulePointer(JDay)
  ENDIF

  ! Now, which day?
  IF (.not. PRESENT(CurDayofWeek)) THEN
    IF (DayofWeek <= 7 .and. HolidayIndex > 0) THEN
      DaySchedulePointer=WeekSchedule(WeekSchedulePointer)%DaySchedulePointer(7+HolidayIndex)
    ELSE
      DaySchedulePointer=WeekSchedule(WeekSchedulePointer)%DaySchedulePointer(DayofWeek)
    ENDIF
  ELSEIF (CurDayofWeek <= 7 .and. HolidayIndex > 0) THEN
    DaySchedulePointer=WeekSchedule(WeekSchedulePointer)%DaySchedulePointer(7+HolidayIndex)
  ELSE
    DaySchedulePointer=WeekSchedule(WeekSchedulePointer)%DaySchedulePointer(CurDayofWeek)
  ENDIF

  ! Return Values
  DayValues(1:24,1:NumOfTimeStepInHour)=DaySchedule(DaySchedulePointer)%TSValue

  RETURN

END SUBROUTINE GetScheduleValuesForDay

SUBROUTINE GetSingleDayScheduleValues(DayScheduleIndex,DayValues)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   August 2003
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine returns an entire day's worth of schedule values for a specified Day Schedule Index item.

          ! METHODOLOGY EMPLOYED:
          ! Use internal data to fill DayValues array.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)           :: DayScheduleIndex  ! Index of the DaySchedule for values
  REAL(r64), INTENT(OUT)             :: DayValues(:,:)    ! Returned set of values

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          ! na

  IF (.not. ScheduleInputProcessed) THEN
    CALL ProcessScheduleInput
    ScheduleInputProcessed=.True.
  ENDIF

  ! Return Values
  DayValues(1:24,1:NumOfTimeStepInHour)=DaySchedule(DayScheduleIndex)%TSValue

  RETURN

END SUBROUTINE GetSingleDayScheduleValues

SUBROUTINE ExternalInterfaceSetSchedule(ScheduleIndex, Value)
          ! FUNCTION INFORMATION:
          !       AUTHOR         Michael Wetter
          !       DATE WRITTEN   February 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine sets all values of the schedule referenced by 'ScheduleIndex'
          ! to the value specified by 'Value'. The subroutine is used by the ExternalInterface to
          ! write real-time data into a schedule so that EnergyPlus modules can use
          ! real-time data by referencing a schedule. This allows overwriting setpoint
          ! for supervisory controls or internal gains obtained from real-time occupancy
          ! measurements.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataGlobals, ONLY: NumOfTimeStepInHour

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER    ScheduleIndex
  REAL(r64)  Value             ! The new value for the schedule

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER TS                     ! Counter for Num Of Time Steps in Hour
  INTEGER Hr                     ! Hour Counter

    ! Assign the value of the variable
    DO Hr=1,24
       DO TS=1,NumOfTimeStepInHour
          DaySchedule(ScheduleIndex)%TSValue(Hr,TS)=Value
       ENDDO
    ENDDO
END SUBROUTINE ExternalInterfaceSetSchedule

SUBROUTINE ProcessIntervalFields(Untils,Numbers,NumUntils,NumNumbers,MinuteValue,SetMinutevalue,ErrorsFound,  &
                                 DayScheduleName,ErrContext)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         <author>
          !       DATE WRITTEN   <date_written>
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine processes the "interval" fields with/without optional "until" in front of
          ! time (hh:mm).

          ! METHODOLOGY EMPLOYED:
          ! na.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), DIMENSION(:), INTENT(IN) :: Untils
  REAL(r64), DIMENSION(:), INTENT(IN)        :: Numbers
  INTEGER, INTENT(IN)                        :: NumUntils
  INTEGER, INTENT(IN)                        :: NumNumbers
  REAL(r64), DIMENSION(24,60), INTENT(OUT)   :: MinuteValue
  LOGICAL, DIMENSION(24,60), INTENT(OUT)     :: SetMinuteValue
  LOGICAL, INTENT(INOUT)                     :: ErrorsFound
  CHARACTER(len=*), INTENT(IN)               :: DayScheduleName  ! Name (used for errors)
  CHARACTER(len=*), INTENT(IN)               :: ErrContext       ! Context (used for errors)

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER Count
  INTEGER Pos
  INTEGER HHField
  INTEGER MMField
  INTEGER Hr
  INTEGER Min
  INTEGER SHr
  INTEGER SMin
  INTEGER EHr
  INTEGER EMin
  INTEGER sFld

  MinuteValue=0.0d0
  SetMinuteValue=.false.
  SHr=1
  SMin=1
  EHr=0
  EMin=0
  sFld=0

  IF (NumUntils /= NumNumbers) THEN
    CALL ShowSevereError('ProcessScheduleInput: ProcessIntervalFields, '//  &
       '  number of Time fields does not match number of value fields, '//  &
                         TRIM(ErrContext)//'='//TRIM(DayScheduleName))
    ErrorsFound=.true.
    RETURN
  ENDIF

UntilLoop:  DO Count=1,NumUntils
    Pos=INDEX(Untils(Count),'UNTIL')
    IF (Pos /= 0 .and. Pos == 1) THEN
      IF (Untils(Count)(6:6) == ':') THEN
        sFld=7
      ELSE
        sFld=6
      ENDIF
    ENDIF
    IF (Pos /= 0 .and. Pos == 1) THEN
      CALL DecodeHHMMField(Untils(Count)(sFld:),HHField,MMField,ErrorsFound,DayScheduleName,Untils(Count))
    ELSEIF (Pos == 0) THEN
      CALL DecodeHHMMField(Untils(Count),HHField,MMField,ErrorsFound,DayScheduleName,Untils(Count))
    ELSE  ! Until found but wasn't first field
      CALL ShowSevereError('ProcessScheduleInput: ProcessIntervalFields, '//  &
                'Invalid "Until" field encountered='//TRIM(Untils(Count)))
      CALL ShowContinueError('Occurred in Day Schedule='//TRIM(DayScheduleName))
      ErrorsFound=.true.
      CYCLE
    ENDIF
    ! Field decoded
    IF (HHField < 0 .or. HHField > 24 .or. MMField < 0 .or. MMField > 60) THEN
      CALL ShowSevereError('ProcessScheduleInput: ProcessIntervalFields, '//  &
                 'Invalid "Until" field encountered='//TRIM(Untils(Count)))
      CALL ShowContinueError('Occurred in Day Schedule='//TRIM(DayScheduleName))
      ErrorsFound=.true.
      CYCLE
    ENDIF
    IF (HHFIeld == 24 .and. MMField > 0 .and. MMField < 60) THEN
      CALL ShowWarningError('ProcessScheduleInput: ProcessIntervalFields, '//  &
                 'Invalid "Until" field encountered='//TRIM(Untils(Count)))
      CALL ShowContinueError('Occurred in Day Schedule='//TRIM(DayScheduleName))
      CALL ShowContinueError('Terminating the field at 24:00')
      MMField=0
    ENDIF

    ! Fill in values
    IF (MMField == 0) THEN
      EHr=HHField+1
      EMin=60
    ENDIF
    IF (MMField < 60) THEN
      EHr=HHField+1
      EMin=MMField
    ENDIF

    IF (SHr == EHr) THEN
      DO Min=SMin,EMin
        IF (SetMinuteValue(SHr,Min)) THEN
          CALL ShowSevereError('ProcessScheduleInput: ProcessIntervalFields, '//  &
                        'Processing time fields, overlapping times detected, '//  &
                               TRIM(ErrContext)//'='//TRIM(DayScheduleName))
          ErrorsFound=.true.
          EXIT UntilLoop
        ENDIF
        MinuteValue(SHr,Min)=Numbers(Count)
        SetMinutevalue(SHr,Min)=.true.
      ENDDO
      SMin=EMin+1
      IF (SMin > 60) THEN
        SHr=SHr+1
        SMin=1
      ENDIF
    ELSEIF (EHr < SHr) THEN
          CALL ShowSevereError('ProcessScheduleInput: ProcessIntervalFields, '//  &
                        'Processing time fields, overlapping times detected, '//  &
                               TRIM(ErrContext)//'='//TRIM(DayScheduleName))
      ErrorsFound=.true.
    ELSE
      DO Min=SMin,60
        MinuteValue(SHr,Min)=Numbers(Count)
        SetMinutevalue(SHr,Min)=.true.
      ENDDO
      DO Hr=SHr+1,EHr-1
        MinuteValue(Hr,:)=Numbers(Count)
        SetMinutevalue(Hr,:)=.true.
      ENDDO
      DO Min=1,EMin
        MinuteValue(EHr,Min)=Numbers(Count)
        SetMinutevalue(EHr,Min)=.true.
      ENDDO
      SHr=EHr
      SMin=EMin+1
      IF (SMin > 60) THEN
        SHr=SHr+1
        SMin=1
      ENDIF
    ENDIF

  ENDDO UntilLoop

  IF (.not. ALL(SetMinuteValue)) THEN
    CALL ShowSevereError('ProcessScheduleInput: ProcessIntervalFields, '//  &
                     'Processing time fields, incomplete day detected, '// &
                         TRIM(ErrContext)//'='//TRIM(DayScheduleName))
    ErrorsFound=.true.
  ENDIF

  RETURN

END SUBROUTINE ProcessIntervalFields

SUBROUTINE DecodeHHMMField(FieldValue,RetHH,RetMM,ErrorsFound,DayScheduleName,FullFieldValue)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda K Lawrie
          !       DATE WRITTEN   January 2003
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine decodes a hhmm date field input as part of the "until" time in a schedule
          ! representation.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: FieldValue  ! Input field value
  INTEGER, INTENT(OUT)         :: RetHH       ! Returned "hour"
  INTEGER, INTENT(OUT)         :: RetMM       ! Returned "minute"
  LOGICAL, INTENT(INOUT)       :: ErrorsFound ! True if errors found in this field
  CHARACTER(len=*), INTENT(IN) :: DayScheduleName ! originating day schedule name
  CHARACTER(len=*), INTENT(IN) :: FullFieldValue  ! Full Input field value

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER :: hhmmFormat='(I2.2)'

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER Pos  ! Position value for scanning the Field
  CHARACTER(len=LEN(FieldValue)) String
  INTEGER :: IOS
  REAL(r64) :: rRetHH       ! real Returned "hour"
  REAL(r64) :: rRetMM       ! real Returned "minute"
  LOGICAL :: nonIntegral
  CHARACTER(len=2) :: hHour
  CHARACTER(len=2) :: mMinute

  String=ADJUSTL(FieldValue)
  Pos=INDEX(String,':')
  nonIntegral=.false.
  IF (Pos == 0) THEN
    CALL ShowSevereError('ProcessScheduleInput: DecodeHHMMField, '//  &
          'Invalid "until" field submitted (no : separator in hh:mm)='//TRIM(ADJUSTL(FullFieldValue)))
    CALL ShowContinueError('Occurred in Day Schedule='//TRIM(DayScheduleName))
    ErrorsFound=.true.
    RETURN
  ELSEIF (Pos == 1) THEN
    RetHH=0
  ELSE
    READ(String(1:Pos-1),*,IOSTAT=IOS) rRetHH
    RetHH=INT(rRetHH)
    IF (REAL(RetHH,r64) /= rRetHH .or. IOS /= 0 .or. rRetHH < 0.0d0) THEN
      IF (REAL(RetHH,r64) /= rRetHH .and. rRetHH >= 0.0d0) THEN
        CALL ShowWarningError('ProcessScheduleInput: DecodeHHMMField, '//  &
               'Invalid "until" field submitted (non-integer numeric in HH)='//TRIM(ADJUSTL(FullFieldValue)))
        CALL ShowContinueError('Other errors may result. Occurred in Day Schedule='//TRIM(DayScheduleName))
        nonIntegral=.true.
      ELSE
        CALL ShowSevereError('ProcessScheduleInput: DecodeHHMMField, '//  &
               'Invalid "until" field submitted (invalid numeric in HH)='//TRIM(ADJUSTL(FullFieldValue)))
        CALL ShowContinueError('Field values must be integer and represent hours:minutes. Occurred in Day Schedule='//  &
           TRIM(DayScheduleName))
        ErrorsFound=.true.
        RETURN
      ENDIF
    ENDIF
  ENDIF

  String=String(Pos+1:)
  READ(String,*,IOSTAT=IOS) rRetMM
  RetMM=INT(rRetMM)
  IF (REAL(RetMM,r64) /= rRetMM .or. IOS /=0 .or. rRetMM < 0.0d0) THEN
    IF (REAL(RetMM,r64) /= rRetMM .and. rRetMM >= 0.0d0) THEN
      CALL ShowWarningError('ProcessScheduleInput: DecodeHHMMField, '//  &
             'Invalid "until" field submitted (non-integer numeric in MM)='//TRIM(ADJUSTL(FullFieldValue)))
      CALL ShowContinueError('Other errors may result. Occurred in Day Schedule='//TRIM(DayScheduleName))
      nonIntegral=.true.
    ELSE
      CALL ShowSevereError('ProcessScheduleInput: DecodeHHMMField, '//  &
               'Invalid "until" field submitted (invalid numeric in MM)='//TRIM(ADJUSTL(FullFieldValue)))
      CALL ShowContinueError('Field values must be integer and represent hours:minutes. Occurred in Day Schedule='//  &
         TRIM(DayScheduleName))
      ErrorsFound=.true.
      RETURN
    ENDIF
  ENDIF

  IF (nonIntegral) THEN
    write(hHour,hhmmFormat) RetHH
    write(mMinute,hhmmFormat) RetMM
    CALL ShowContinueError('Until value to be used will be: '//hhour//':'//mMinute)
  ENDIF

  RETURN

END SUBROUTINE DecodeHHMMField

SUBROUTINE ProcessForDayTypes(ForDayField,TheseDays,AlReady,ErrorsFound)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   February 2003
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine processes a field "For: day types" and returns
          ! those day types (can be multiple) from field.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN)  :: ForDayField  ! Field containing the "FOR:..."
  LOGICAL, DIMENSION(MaxDayTypes), INTENT(INOUT) :: TheseDays  ! Array to contain returned "true" days
  LOGICAL, DIMENSION(MaxDayTypes), INTENT(INOUT) :: AlReady    ! Array of days already done
  LOGICAL, INTENT(INOUT) :: ErrorsFound     ! Will be true if error found.

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: DayT
  LOGICAL :: OneValid
  LOGICAL :: DupAssignment

  OneValid=.false.
  DupAssignment=.false.
  ! Just test for specific days
  IF (INDEX(ForDayField,'WEEKDAY') > 0) THEN
    TheseDays(2:6)=.true.
    IF (ANY(AlReady(2:6))) THEN
      DupAssignment=.true.
    ELSE
      AlReady(2:6)=.true.
    ENDIF
    OneValid=.true.
  ENDIF
  IF (INDEX(ForDayField,'MONDAY') > 0) THEN
    TheseDays(2)=.true.
    IF (AlReady(2)) THEN
      DupAssignment=.true.
    ELSE
      AlReady(2)=.true.
    ENDIF
    OneValid=.true.
  ENDIF
  IF (INDEX(ForDayField,'TUESDAY') > 0) THEN
    TheseDays(3)=.true.
    IF (AlReady(3)) THEN
      DupAssignment=.true.
    ELSE
      AlReady(3)=.true.
    ENDIF
    OneValid=.true.
  ENDIF
  IF (INDEX(ForDayField,'WEDNESDAY') > 0) THEN
    TheseDays(4)=.true.
    IF (AlReady(4)) THEN
      DupAssignment=.true.
    ELSE
      AlReady(4)=.true.
    ENDIF
    OneValid=.true.
  ENDIF
  IF (INDEX(ForDayField,'THURSDAY') > 0) THEN
    TheseDays(5)=.true.
    IF (AlReady(5)) THEN
      DupAssignment=.true.
    ELSE
      AlReady(5)=.true.
    ENDIF
    OneValid=.true.
  ENDIF
  IF (INDEX(ForDayField,'FRIDAY') > 0) THEN
    TheseDays(6)=.true.
    IF (AlReady(6)) THEN
      DupAssignment=.true.
    ELSE
      AlReady(6)=.true.
    ENDIF
    OneValid=.true.
  ENDIF
  IF (INDEX(ForDayField,'WEEKEND') > 0) THEN
    TheseDays(1)=.true.
    TheseDays(7)=.true.
    IF (AlReady(1)) THEN
      DupAssignment=.true.
    ELSE
      AlReady(1)=.true.
    ENDIF
    IF (AlReady(7)) THEN
      DupAssignment=.true.
    ELSE
      AlReady(7)=.true.
    ENDIF
    OneValid=.true.
  ENDIF
  IF (INDEX(ForDayField,'SATURDAY') > 0) THEN
    TheseDays(7)=.true.
    IF (AlReady(7)) THEN
      DupAssignment=.true.
    ELSE
      AlReady(7)=.true.
    ENDIF
    OneValid=.true.
  ENDIF
  IF (INDEX(ForDayField,'SUNDAY') > 0) THEN
    TheseDays(1)=.true.
    IF (AlReady(1)) THEN
      DupAssignment=.true.
    ELSE
      AlReady(1)=.true.
    ENDIF
    OneValid=.true.
  ENDIF
  IF (INDEX(ForDayField,'CUSTOMDAY1') > 0) THEN
    TheseDays(11)=.true.
    IF (AlReady(11)) THEN
      DupAssignment=.true.
    ELSE
      AlReady(11)=.true.
    ENDIF
    OneValid=.true.
  ENDIF
  IF (INDEX(ForDayField,'CUSTOMDAY2') > 0) THEN
    TheseDays(12)=.true.
    IF (AlReady(12)) THEN
      DupAssignment=.true.
    ELSE
      AlReady(12)=.true.
    ENDIF
    OneValid=.true.
  ENDIF
  IF (INDEX(ForDayField,'ALLDAY') > 0) THEN
    TheseDays(1:MaxDayTypes)=.true.
    IF (ANY(AlReady(1:MaxDayTypes))) THEN
      DupAssignment=.true.
    ELSE
      AlReady(1:MaxDayTypes)=.true.
    ENDIF
    OneValid=.true.
  ENDIF
  IF (INDEX(ForDayField,'HOLIDAY') > 0) THEN
    TheseDays(8)=.true.
    IF (AlReady(8)) THEN
      DupAssignment=.true.
    ELSE
      AlReady(8)=.true.
    ENDIF
    OneValid=.true.
  ENDIF
  IF (INDEX(ForDayField,'SUMMER') > 0) THEN
    TheseDays(9)=.true.
    IF (AlReady(9)) THEN
      DupAssignment=.true.
    ELSE
      AlReady(9)=.true.
    ENDIF
    OneValid=.true.
  ENDIF
  IF (INDEX(ForDayField,'WINTER') > 0) THEN
    TheseDays(10)=.true.
    IF (AlReady(10)) THEN
      DupAssignment=.true.
    ELSE
      AlReady(10)=.true.
    ENDIF
    OneValid=.true.
  ENDIF
  IF (INDEX(ForDayField,'ALLOTHERDAY') > 0) THEN
    DO DayT=1,MaxDayTypes
      IF (AlReady(DayT)) CYCLE
      TheseDays(DayT)=.true.
      AlReady(DayT)=.true.
    ENDDO
    OneValid=.true.
  ENDIF

  IF (DupAssignment) THEN
    CALL ShowSevereError('ProcessScheduleInput: ProcessForDayTypes, '//  &
      'Duplicate assignment attempted in "for" days field='//TRIM(ForDayField))
    ErrorsFound=.true.
  ENDIF
  IF (.not. OneValid) THEN
    CALL ShowSevereError('ProcessScheduleInput: ProcessForDayTypes, '//  &
        'No valid day assignments found in "for" days field='//TRIM(ForDayField))
    ErrorsFound=.true.
  ENDIF

  RETURN

END SUBROUTINE ProcessForDayTypes

LOGICAL FUNCTION dCheckScheduleValueMinMax1(ScheduleIndex,MinString,Minimum) !,MaxString,Maximum)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   February 2003
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function checks the indicated schedule values for validity.  Uses the ScheduleIndex
          ! from (GetScheduleIndex), a minimum and a maximum -- one or other optional to check "internals".

          ! METHODOLOGY EMPLOYED:
          ! Schedule data structure stores this on first validity check.  If there, then is returned else
          ! looks up minimum and maximum values for the schedule and then sets result of function based on
          ! requested minimum/maximum checks.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)                    :: ScheduleIndex  ! Which Schedule being tested
  CHARACTER(len=*), INTENT(IN) :: MinString      ! Minimum indicator ('>', '>=')
  REAL(r64), INTENT(IN)             :: Minimum        ! Minimum desired value

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER Loop  ! Loop Control variable
  INTEGER DayT  ! Day Type Loop control
  INTEGER WkSch ! Pointer for WeekSchedule value
  REAL(r64) MinValue ! For total minimum
  REAL(r64) Maxvalue ! For total maximum
  LOGICAL :: MinValueOk
  LOGICAL :: MaxValueOk

  IF (ScheduleIndex == -1) THEN
    MinValue = 1.0d0
    MaxValue = 1.0d0
  ELSEIF (ScheduleIndex == 0) THEN
    MinValue = 0.0d0
    MaxValue = 0.0d0
  ELSEIF (ScheduleIndex < 1 .or. ScheduleIndex > NumSchedules) THEN
    CALL ShowFatalError('CheckScheduleValueMinMax called with ScheduleIndex out of range')
  ENDIF

  IF (ScheduleIndex > 0) THEN
    IF (.not. Schedule(ScheduleIndex)%MaxMinSet) THEN  ! Set Minimum/Maximums for this schedule
      WkSch=Schedule(ScheduleIndex)%WeekSchedulePointer(1)
      MinValue=MINVAL(DaySchedule(WeekSchedule(WkSch)%DaySchedulePointer(1))%TSValue)
      MaxValue=MAXVAL(DaySchedule(WeekSchedule(WkSch)%DaySchedulePointer(1))%TSValue)
      DO DayT=2,MaxDayTypes
        MinValue=MIN(MinValue,MINVAL(DaySchedule(WeekSchedule(WkSch)%DaySchedulePointer(DayT))%TSValue))
        MaxValue=MAX(MaxValue,MAXVAL(DaySchedule(WeekSchedule(WkSch)%DaySchedulePointer(DayT))%TSValue))
      ENDDO
      DO Loop=2,366
        WkSch=Schedule(ScheduleIndex)%WeekSchedulePointer(Loop)
        DO DayT=1,MaxDayTypes
          MinValue=MIN(MinValue,MINVAL(DaySchedule(WeekSchedule(WkSch)%DaySchedulePointer(DayT))%TSValue))
          MaxValue=MAX(MaxValue,MAXVAL(DaySchedule(WeekSchedule(WkSch)%DaySchedulePointer(DayT))%TSValue))
        ENDDO
      ENDDO
      Schedule(ScheduleIndex)%MaxMinSet=.true.
      Schedule(ScheduleIndex)%MinValue=MinValue
      Schedule(ScheduleIndex)%MaxValue=MaxValue
    ENDIF
  ENDIF

  !  Min/max for schedule has been set.  Test.
  MinValueOk=.true.
  MaxValueOk=.true.
  MinValueOk=(Schedule(ScheduleIndex)%MinValue >= Minimum)
  IF (MinString == '>') THEN
    MinValueOk=(Schedule(ScheduleIndex)%MinValue > Minimum)
  ELSE
    MinValueOk=(Schedule(ScheduleIndex)%MinValue >= Minimum)
  ENDIF

  dCheckScheduleValueMinMax1=(MinValueOk .and. MaxValueOk)

  RETURN

END FUNCTION dCheckScheduleValueMinMax1

LOGICAL FUNCTION dCheckScheduleValueMinMax2(ScheduleIndex,MinString,Minimum,MaxString,Maximum)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   February 2003
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function checks the indicated schedule values for validity.  Uses the ScheduleIndex
          ! from (GetScheduleIndex), a minimum and a maximum -- one or other optional to check "internals".

          ! METHODOLOGY EMPLOYED:
          ! Schedule data structure stores this on first validity check.  If there, then is returned else
          ! looks up minimum and maximum values for the schedule and then sets result of function based on
          ! requested minimum/maximum checks.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)                    :: ScheduleIndex  ! Which Schedule being tested
  CHARACTER(len=*), INTENT(IN) :: MinString      ! Minimum indicator ('>', '>=')
  REAL(r64), INTENT(IN)             :: Minimum        ! Minimum desired value
  CHARACTER(len=*), INTENT(IN) :: MaxString      ! Maximum indicator ('<', ',=')
  REAL(r64), INTENT(IN)             :: Maximum        ! Maximum desired value

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER Loop  ! Loop Control variable
  INTEGER DayT  ! Day Type Loop control
  INTEGER WkSch ! Pointer for WeekSchedule value
  REAL(r64) MinValue ! For total minimum
  REAL(r64) Maxvalue ! For total maximum
  LOGICAL :: MinValueOk
  LOGICAL :: MaxValueOk
  LOGICAL,SAVE :: RunOnceOnly = .TRUE.

  !precompute the dayschedule max and min so that it is not in nested loop
  IF (RunOnceOnly) THEN
    DO Loop = 0,NumDaySchedules
      DaySchedule(Loop)%TSValMin = MINVAL(DaySchedule(Loop)%TSValue)
      DaySchedule(Loop)%TSValMax = MAXVAL(DaySchedule(Loop)%TSValue)
    END DO
    RunOnceOnly = .FALSE.
  END IF

  IF (ScheduleIndex == -1) THEN
    MinValue = 1.0d0
    MaxValue = 1.0d0
  ELSEIF (ScheduleIndex == 0) THEN
    MinValue = 0.0d0
    MaxValue = 0.0d0
  ELSEIF (ScheduleIndex < 1 .or. ScheduleIndex > NumSchedules) THEN
    CALL ShowFatalError('CheckScheduleValueMinMax called with ScheduleIndex out of range')
  ENDIF

  IF (ScheduleIndex > 0) THEN
    IF (.not. Schedule(ScheduleIndex)%MaxMinSet) THEN  ! Set Minimum/Maximums for this schedule
      WkSch=Schedule(ScheduleIndex)%WeekSchedulePointer(1)
      MinValue=DaySchedule(WeekSchedule(WkSch)%DaySchedulePointer(1))%TSValMin
      MaxValue=DaySchedule(WeekSchedule(WkSch)%DaySchedulePointer(1))%TSValMax
      DO DayT=2,MaxDayTypes
        MinValue=MIN(MinValue,DaySchedule(WeekSchedule(WkSch)%DaySchedulePointer(DayT))%TSValMin)
        MaxValue=MAX(MaxValue,DaySchedule(WeekSchedule(WkSch)%DaySchedulePointer(DayT))%TSValMax)
      ENDDO
      DO Loop=2,366
        WkSch=Schedule(ScheduleIndex)%WeekSchedulePointer(Loop)
        DO DayT=1,MaxDayTypes
          MinValue=MIN(MinValue,DaySchedule(WeekSchedule(WkSch)%DaySchedulePointer(DayT))%TSValMin)
          MaxValue=MAX(MaxValue,DaySchedule(WeekSchedule(WkSch)%DaySchedulePointer(DayT))%TSValMax)
        ENDDO
      ENDDO
      Schedule(ScheduleIndex)%MaxMinSet=.true.
      Schedule(ScheduleIndex)%MinValue=MinValue
      Schedule(ScheduleIndex)%MaxValue=MaxValue
    ENDIF
  ENDIF

  !  Min/max for schedule has been set.  Test.
  MinValueOk=.true.
  MaxValueOk=.true.
  IF (MinString == '>') THEN
    MinValueOk=(Schedule(ScheduleIndex)%MinValue > Minimum)
  ELSE
    MinValueOk=(Schedule(ScheduleIndex)%MinValue >= Minimum)
  ENDIF

  MaxValueOk=(Schedule(ScheduleIndex)%MaxValue <= Maximum)
  IF (MaxString == '<') THEN
    MaxValueOk=(Schedule(ScheduleIndex)%MaxValue < Maximum)
  ELSE
    MaxValueOk=(Schedule(ScheduleIndex)%MaxValue <= Maximum)
  ENDIF

  dCheckScheduleValueMinMax2=(MinValueOk .and. MaxValueOk)

  RETURN

END FUNCTION dCheckScheduleValueMinMax2

LOGICAL FUNCTION rCheckScheduleValueMinMax1(ScheduleIndex,MinString,Minimum)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   February 2003
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function checks the indicated schedule values for validity.  Uses the ScheduleIndex
          ! from (GetScheduleIndex), a minimum and a maximum -- one or other optional to check "internals".

          ! METHODOLOGY EMPLOYED:
          ! Schedule data structure stores this on first validity check.  If there, then is returned else
          ! looks up minimum and maximum values for the schedule and then sets result of function based on
          ! requested minimum/maximum checks.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)                    :: ScheduleIndex  ! Which Schedule being tested
  CHARACTER(len=*), INTENT(IN) :: MinString      ! Minimum indicator ('>', '>=')
  REAL(r32), INTENT(IN)        :: Minimum        ! Minimum desired value

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER Loop  ! Loop Control variable
  INTEGER DayT  ! Day Type Loop control
  INTEGER WkSch ! Pointer for WeekSchedule value
  REAL(r64) MinValue ! For total minimum
  REAL(r64) Maxvalue ! For total maximum
  LOGICAL :: MinValueOk
  LOGICAL :: MaxValueOk

  IF (ScheduleIndex == -1) THEN
    MinValue = 1.0d0
    MaxValue = 1.0d0
  ELSEIF (ScheduleIndex == 0) THEN
    MinValue = 0.0d0
    MaxValue = 0.0d0
  ELSEIF (ScheduleIndex < 1 .or. ScheduleIndex > NumSchedules) THEN
    CALL ShowFatalError('CheckScheduleValueMinMax called with ScheduleIndex out of range')
  ENDIF

  IF (ScheduleIndex > 0) THEN
    IF (.not. Schedule(ScheduleIndex)%MaxMinSet) THEN  ! Set Minimum/Maximums for this schedule
      WkSch=Schedule(ScheduleIndex)%WeekSchedulePointer(1)
      MinValue=MINVAL(DaySchedule(WeekSchedule(WkSch)%DaySchedulePointer(1))%TSValue)
      MaxValue=MAXVAL(DaySchedule(WeekSchedule(WkSch)%DaySchedulePointer(1))%TSValue)
      DO DayT=2,MaxDayTypes
        MinValue=MIN(MinValue,MINVAL(DaySchedule(WeekSchedule(WkSch)%DaySchedulePointer(DayT))%TSValue))
        MaxValue=MAX(MaxValue,MAXVAL(DaySchedule(WeekSchedule(WkSch)%DaySchedulePointer(DayT))%TSValue))
      ENDDO
      DO Loop=2,366
        WkSch=Schedule(ScheduleIndex)%WeekSchedulePointer(Loop)
        DO DayT=1,MaxDayTypes
          MinValue=MIN(MinValue,MINVAL(DaySchedule(WeekSchedule(WkSch)%DaySchedulePointer(DayT))%TSValue))
          MaxValue=MAX(MaxValue,MAXVAL(DaySchedule(WeekSchedule(WkSch)%DaySchedulePointer(DayT))%TSValue))
        ENDDO
      ENDDO
      Schedule(ScheduleIndex)%MaxMinSet=.true.
      Schedule(ScheduleIndex)%MinValue=MinValue
      Schedule(ScheduleIndex)%MaxValue=MaxValue
    ENDIF
  ENDIF

  !  Min/max for schedule has been set.  Test.
  MinValueOk=.true.
  MaxValueOk=.true.
  MinValueOk=(Schedule(ScheduleIndex)%MinValue >= Minimum)
  IF (MinString == '>') THEN
    MinValueOk=(Schedule(ScheduleIndex)%MinValue > Minimum)
  ELSE
    MinValueOk=(Schedule(ScheduleIndex)%MinValue >= Minimum)
  ENDIF

  rCheckScheduleValueMinMax1=(MinValueOk .and. MaxValueOk)

  RETURN

END FUNCTION rCheckScheduleValueMinMax1

LOGICAL FUNCTION rCheckScheduleValueMinMax2(ScheduleIndex,MinString,Minimum,MaxString,Maximum)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   February 2003
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function checks the indicated schedule values for validity.  Uses the ScheduleIndex
          ! from (GetScheduleIndex), a minimum and a maximum -- one or other optional to check "internals".

          ! METHODOLOGY EMPLOYED:
          ! Schedule data structure stores this on first validity check.  If there, then is returned else
          ! looks up minimum and maximum values for the schedule and then sets result of function based on
          ! requested minimum/maximum checks.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)                    :: ScheduleIndex  ! Which Schedule being tested
  CHARACTER(len=*), INTENT(IN) :: MinString      ! Minimum indicator ('>', '>=')
  REAL(r32), INTENT(IN)             :: Minimum        ! Minimum desired value
  CHARACTER(len=*), INTENT(IN) :: MaxString      ! Maximum indicator ('<', ',=')
  REAL(r32), INTENT(IN)             :: Maximum        ! Maximum desired value

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER Loop  ! Loop Control variable
  INTEGER DayT  ! Day Type Loop control
  INTEGER WkSch ! Pointer for WeekSchedule value
  REAL(r64) MinValue ! For total minimum
  REAL(r64) Maxvalue ! For total maximum
  LOGICAL :: MinValueOk
  LOGICAL :: MaxValueOk

  IF (ScheduleIndex == -1) THEN
    MinValue = 1.0d0
    MaxValue = 1.0d0
  ELSEIF (ScheduleIndex == 0) THEN
    MinValue = 0.0d0
    MaxValue = 0.0d0
  ELSEIF (ScheduleIndex < 1 .or. ScheduleIndex > NumSchedules) THEN
    CALL ShowFatalError('CheckScheduleValueMinMax called with ScheduleIndex out of range')
  ENDIF

  IF (ScheduleIndex > 0) THEN
    IF (.not. Schedule(ScheduleIndex)%MaxMinSet) THEN  ! Set Minimum/Maximums for this schedule
      WkSch=Schedule(ScheduleIndex)%WeekSchedulePointer(1)
      MinValue=MINVAL(DaySchedule(WeekSchedule(WkSch)%DaySchedulePointer(1))%TSValue)
      MaxValue=MAXVAL(DaySchedule(WeekSchedule(WkSch)%DaySchedulePointer(1))%TSValue)
      DO DayT=2,MaxDayTypes
        MinValue=MIN(MinValue,MINVAL(DaySchedule(WeekSchedule(WkSch)%DaySchedulePointer(DayT))%TSValue))
        MaxValue=MAX(MaxValue,MAXVAL(DaySchedule(WeekSchedule(WkSch)%DaySchedulePointer(DayT))%TSValue))
      ENDDO
      DO Loop=2,366
        WkSch=Schedule(ScheduleIndex)%WeekSchedulePointer(Loop)
        DO DayT=1,MaxDayTypes
          MinValue=MIN(MinValue,MINVAL(DaySchedule(WeekSchedule(WkSch)%DaySchedulePointer(DayT))%TSValue))
          MaxValue=MAX(MaxValue,MAXVAL(DaySchedule(WeekSchedule(WkSch)%DaySchedulePointer(DayT))%TSValue))
        ENDDO
      ENDDO
      Schedule(ScheduleIndex)%MaxMinSet=.true.
      Schedule(ScheduleIndex)%MinValue=MinValue
      Schedule(ScheduleIndex)%MaxValue=MaxValue
    ENDIF
  ENDIF

  !  Min/max for schedule has been set.  Test.
  MinValueOk=.true.
  MaxValueOk=.true.
  IF (MinString == '>') THEN
    MinValueOk=(Schedule(ScheduleIndex)%MinValue > Minimum)
  ELSE
    MinValueOk=(Schedule(ScheduleIndex)%MinValue >= Minimum)
  ENDIF

  MaxValueOk=(Schedule(ScheduleIndex)%MaxValue <= Maximum)
  IF (MaxString == '<') THEN
    MaxValueOk=(Schedule(ScheduleIndex)%MaxValue < Maximum)
  ELSE
    MaxValueOk=(Schedule(ScheduleIndex)%MaxValue <= Maximum)
  ENDIF

  rCheckScheduleValueMinMax2=(MinValueOk .and. MaxValueOk)

  RETURN

END FUNCTION rCheckScheduleValueMinMax2

LOGICAL FUNCTION rCheckScheduleValue(ScheduleIndex,Value)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   November 2004
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function checks the indicated schedule value for validity.  Uses the ScheduleIndex
          ! from (GetScheduleIndex).

          ! METHODOLOGY EMPLOYED:
          ! This routine is best used with "discrete" schedules.  The routine must traverse all values
          ! in the schedule and compares by equality.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)                    :: ScheduleIndex  ! Which Schedule being tested
  REAL(r64), INTENT(IN)                       :: Value          ! Actual desired value

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER Loop  ! Loop Control variable
  INTEGER DayT  ! Day Type Loop control
  INTEGER WkSch ! Pointer for WeekSchedule value

  rCheckScheduleValue=.false.

  IF (ScheduleIndex == -1) THEN
    rCheckScheduleValue=(Value == 1.0d0)
  ELSEIF (ScheduleIndex == 0) THEN
    rCheckScheduleValue=(Value == 0.0d0)
  ELSEIF (ScheduleIndex < 1 .or. ScheduleIndex > NumSchedules) THEN
    CALL ShowFatalError('CheckScheduleValue called with ScheduleIndex out of range')
  ENDIF

  IF (ScheduleIndex > 0) THEN
    rCheckScheduleValue=.false.
  DayLoop:  DO Loop=1,366
        WkSch=Schedule(ScheduleIndex)%WeekSchedulePointer(Loop)
        DO DayT=1,MaxDayTypes
          IF (ANY(DaySchedule(WeekSchedule(WkSch)%DaySchedulePointer(DayT))%TSValue == Value)) THEN
            rCheckScheduleValue=.true.
            EXIT DayLoop
          ENDIF
        ENDDO
      ENDDO DayLoop
  ENDIF

  RETURN

END FUNCTION rCheckScheduleValue

LOGICAL FUNCTION iCheckScheduleValue(ScheduleIndex,Value)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   November 2004
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function checks the indicated schedule value for validity.  Uses the ScheduleIndex
          ! from (GetScheduleIndex).

          ! METHODOLOGY EMPLOYED:
          ! This routine is best used with "discrete" schedules.  The routine must traverse all values
          ! in the schedule and compares by equality.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)                    :: ScheduleIndex  ! Which Schedule being tested
  INTEGER, INTENT(IN)                       :: Value          ! Actual desired value

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER Loop  ! Loop Control variable
  INTEGER DayT  ! Day Type Loop control
  INTEGER WkSch ! Pointer for WeekSchedule value

  iCheckScheduleValue=.false.
  IF (ScheduleIndex == -1) THEN
    iCheckScheduleValue=(Value == 1)
  ELSEIF (ScheduleIndex == 0) THEN
    iCheckScheduleValue=(Value == 0)
  ELSEIF (ScheduleIndex < 1 .or. ScheduleIndex > NumSchedules) THEN
    CALL ShowFatalError('CheckScheduleValue called with ScheduleIndex out of range')
  ENDIF

  IF (ScheduleIndex > 0) THEN

  DayLoop:  DO Loop=1,366
        WkSch=Schedule(ScheduleIndex)%WeekSchedulePointer(Loop)
        DO DayT=1,MaxDayTypes
          IF (ANY(DaySchedule(WeekSchedule(WkSch)%DaySchedulePointer(DayT))%TSValue == REAL(Value,r64))) THEN
            iCheckScheduleValue=.true.
            EXIT DayLoop
          ENDIF
        ENDDO
      ENDDO DayLoop
  ENDIF

  RETURN

END FUNCTION iCheckScheduleValue

LOGICAL FUNCTION rCheckDayScheduleValueMinMax(ScheduleIndex,Minimum,MinString,Maximum,MaxString)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   February 2003
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function checks the indicated schedule values for validity.  Uses the ScheduleIndex
          ! from (GetScheduleIndex), a minimum and a maximum -- one or other optional to check "internals".

          ! METHODOLOGY EMPLOYED:
          ! Schedule data structure stores this on first validity check.  If there, then is returned else
          ! looks up minimum and maximum values for the schedule and then sets result of function based on
          ! requested minimum/maximum checks.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)                    :: ScheduleIndex  ! Which Day Schedule being tested
  REAL(r64), INTENT(IN)    :: Minimum        ! Minimum desired value
  CHARACTER(len=*), INTENT(IN)           :: MinString      ! Minimum indicator ('>', '>=')
  REAL(r64), INTENT(IN), OPTIONAL             :: Maximum        ! Maximum desired value
  CHARACTER(len=*), INTENT(IN), OPTIONAL :: MaxString      ! Maximum indicator ('<', ',=')

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  REAL(r64) MinValue ! For total minimum
  REAL(r64) Maxvalue ! For total maximum
  LOGICAL :: MinValueOk
  LOGICAL :: MaxValueOk

  IF (ScheduleIndex == -1) THEN
    MinValue = 1.0d0
    MaxValue = 1.0d0
  ELSEIF (ScheduleIndex == 0) THEN
    MinValue = 0.0d0
    MaxValue = 0.0d0
  ELSEIF (ScheduleIndex < 1 .or. ScheduleIndex > NumDaySchedules) THEN
    CALL ShowFatalError('CheckDayScheduleValueMinMax called with ScheduleIndex out of range')
  ENDIF

  IF (ScheduleIndex > 0) THEN
    MinValue=MINVAL(DaySchedule(ScheduleIndex)%TSValue)
    MaxValue=MAXVAL(DaySchedule(ScheduleIndex)%TSValue)
  ENDIF

  !  Min/max for schedule has been set.  Test.
  MinValueOk=.true.
  MaxValueOk=.true.

  IF (MinString == '>') THEN
    MinValueOk=(MinValue > Minimum)
  ELSE
    MinValueOk=(MinValue >= Minimum)
  ENDIF

  IF (PRESENT(Maximum)) THEN
    IF (PRESENT(MaxString)) THEN
      IF (MaxString == '<') THEN
        MaxValueOk=(MaxValue < Maximum)
      ELSE
        MaxValueOk=(MaxValue <= Maximum)
      ENDIF
    ELSE
      MaxValueOk=(MaxValue <= Maximum)
    ENDIF
  ENDIF

  rCheckDayScheduleValueMinMax=(MinValueOk .and. MaxValueOk)

  RETURN

END FUNCTION rCheckDayScheduleValueMinMax

LOGICAL FUNCTION sCheckDayScheduleValueMinMax(ScheduleIndex,Minimum,MinString,Maximum,MaxString)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   February 2003
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function checks the indicated schedule values for validity.  Uses the ScheduleIndex
          ! from (GetScheduleIndex), a minimum and a maximum -- one or other optional to check "internals".

          ! METHODOLOGY EMPLOYED:
          ! Schedule data structure stores this on first validity check.  If there, then is returned else
          ! looks up minimum and maximum values for the schedule and then sets result of function based on
          ! requested minimum/maximum checks.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)                    :: ScheduleIndex  ! Which Day Schedule being tested
  REAL(r32), INTENT(IN)    :: Minimum        ! Minimum desired value
  CHARACTER(len=*), INTENT(IN)           :: MinString      ! Minimum indicator ('>', '>=')
  REAL(r32), INTENT(IN), OPTIONAL             :: Maximum        ! Maximum desired value
  CHARACTER(len=*), INTENT(IN), OPTIONAL :: MaxString      ! Maximum indicator ('<', ',=')

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  REAL(r64) MinValue ! For total minimum
  REAL(r64) Maxvalue ! For total maximum
  LOGICAL :: MinValueOk
  LOGICAL :: MaxValueOk

  IF (ScheduleIndex == -1) THEN
    MinValue = 1.0d0
    MaxValue = 1.0d0
  ELSEIF (ScheduleIndex == 0) THEN
    MinValue = 0.0d0
    MaxValue = 0.0d0
  ELSEIF (ScheduleIndex < 1 .or. ScheduleIndex > NumDaySchedules) THEN
    CALL ShowFatalError('CheckDayScheduleValueMinMax called with ScheduleIndex out of range')
  ENDIF

  IF (ScheduleIndex > 0) THEN
    MinValue=MINVAL(DaySchedule(ScheduleIndex)%TSValue)
    MaxValue=MAXVAL(DaySchedule(ScheduleIndex)%TSValue)
  ENDIF

  !  Min/max for schedule has been set.  Test.
  MinValueOk=.true.
  MaxValueOk=.true.
  IF (MinString == '>') THEN
    MinValueOk=(MinValue > Minimum)
  ELSE
    MinValueOk=(MinValue >= Minimum)
  ENDIF

  IF (PRESENT(Maximum)) THEN
    IF (PRESENT(MaxString)) THEN
      IF (MaxString == '<') THEN
        MaxValueOk=(MaxValue < Maximum)
      ELSE
        MaxValueOk=(MaxValue <= Maximum)
      ENDIF
    ELSE
      MaxValueOk=(MaxValue <= Maximum)
    ENDIF
  ENDIF

  sCheckDayScheduleValueMinMax=(MinValueOk .and. MaxValueOk)

  RETURN

END FUNCTION sCheckDayScheduleValueMinMax

FUNCTION HasFractionalScheduleValue(ScheduleIndex) RESULT(HasFractions)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   March 2008
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function returns true if the schedule contains fractional
          ! values [>0, <1].

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)  :: ScheduleIndex  ! Which Schedule being tested
  LOGICAL              :: HasFractions   ! True if the schedule has fractional values

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER WkSch
  INTEGER DayT
  INTEGER Loop
  INTEGER Hour
  INTEGER TStep

  IF (ScheduleIndex == -1 .or. ScheduleIndex == 0) THEN
    CONTINUE
  ELSEIF (ScheduleIndex < 1 .or. ScheduleIndex > NumSchedules) THEN
    CALL ShowFatalError('HasFractionalScheduleValue called with ScheduleIndex out of range')
  ENDIF

  HasFractions=.false.

  IF (ScheduleIndex > 0) THEN
    WkSch=Schedule(ScheduleIndex)%WeekSchedulePointer(1)
    DayTLoop: DO DayT=1,MaxDayTypes
      DO Hour=1,24
        DO TStep=1,NumOfTimeStepInHour
          IF (DaySchedule(WeekSchedule(WkSch)%DaySchedulePointer(DayT))%TSValue(Hour,TStep) > 0.0d0 .and.  &
              DaySchedule(WeekSchedule(WkSch)%DaySchedulePointer(DayT))%TSValue(Hour,TStep) < 1.0d0) THEN
            HasFractions=.true.
            EXIT DayTLoop
          ENDIF
        ENDDO
      ENDDO
    ENDDO DayTLoop
    IF (.not. HasFractions) THEN
      DO Loop=2,366
        WkSch=Schedule(ScheduleIndex)%WeekSchedulePointer(Loop)
        DayTLoop2: DO DayT=1,MaxDayTypes
          DO Hour=1,24
            DO TStep=1,NumOfTimeStepInHour
              IF (DaySchedule(WeekSchedule(WkSch)%DaySchedulePointer(DayT))%TSValue(Hour,TStep) > 0.0d0 .and.  &
                  DaySchedule(WeekSchedule(WkSch)%DaySchedulePointer(DayT))%TSValue(Hour,TStep) < 1.0d0) THEN
                HasFractions=.true.
                EXIT DayTLoop2
              ENDIF
            ENDDO
          ENDDO
        ENDDO DayTLoop2
      ENDDO
    ENDIF
  ENDIF

  RETURN

END FUNCTION HasFractionalScheduleValue

FUNCTION GetScheduleMinValue(ScheduleIndex) RESULT(MinimumValue)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   February 2004
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function returns the minimum value used by a schedule over
          ! the entire year.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)  :: ScheduleIndex  ! Which Schedule being tested
  REAL(r64)            :: MinimumValue   ! Minimum value for schedule

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  REAL(r64) MinValue
  REAL(r64) MaxValue
  INTEGER WkSch
  INTEGER DayT
  INTEGER Loop

  IF (ScheduleIndex == -1) THEN
    MinValue = 1.0d0
    MaxValue = 1.0d0
  ELSEIF (ScheduleIndex == 0) THEN
    MinValue = 0.0d0
    MaxValue = 0.0d0
  ELSEIF (ScheduleIndex < 1 .or. ScheduleIndex > NumSchedules) THEN
    CALL ShowFatalError('GetScheduleMinValue called with ScheduleIndex out of range')
  ENDIF

  IF (ScheduleIndex > 0) THEN
    IF (.not. Schedule(ScheduleIndex)%MaxMinSet) THEN  ! Set Minimum/Maximums for this schedule
      WkSch=Schedule(ScheduleIndex)%WeekSchedulePointer(1)
      MinValue=MINVAL(DaySchedule(WeekSchedule(WkSch)%DaySchedulePointer(1))%TSValue)
      MaxValue=MAXVAL(DaySchedule(WeekSchedule(WkSch)%DaySchedulePointer(1))%TSValue)
      DO DayT=2,MaxDayTypes
        MinValue=MIN(MinValue,MINVAL(DaySchedule(WeekSchedule(WkSch)%DaySchedulePointer(DayT))%TSValue))
        MaxValue=MAX(MaxValue,MAXVAL(DaySchedule(WeekSchedule(WkSch)%DaySchedulePointer(DayT))%TSValue))
      ENDDO
      DO Loop=2,366
        WkSch=Schedule(ScheduleIndex)%WeekSchedulePointer(Loop)
        DO DayT=1,MaxDayTypes
          MinValue=MIN(MinValue,MINVAL(DaySchedule(WeekSchedule(WkSch)%DaySchedulePointer(DayT))%TSValue))
          MaxValue=MAX(MaxValue,MAXVAL(DaySchedule(WeekSchedule(WkSch)%DaySchedulePointer(DayT))%TSValue))
        ENDDO
      ENDDO
      Schedule(ScheduleIndex)%MaxMinSet=.true.
      Schedule(ScheduleIndex)%MinValue=MinValue
      Schedule(ScheduleIndex)%MaxValue=MaxValue
    ENDIF

    !  Min/max for schedule has been set.
    MinimumValue=Schedule(ScheduleIndex)%MinValue
  ELSE
    MinimumValue=MinValue
  ENDIF

  RETURN

END FUNCTION GetScheduleMinValue

FUNCTION GetScheduleMaxValue(ScheduleIndex) RESULT(MaximumValue)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   February 2004
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function returns the maximum value used by a schedule over
          ! the entire year.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)  :: ScheduleIndex  ! Which Schedule being tested
  REAL(r64)            :: MaximumValue   ! Maximum value for schedule

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  REAL(r64) MinValue
  REAL(r64) MaxValue
  INTEGER WkSch
  INTEGER DayT
  INTEGER Loop

  IF (ScheduleIndex == -1) THEN
    MinValue = 1.0d0
    MaxValue = 1.0d0
  ELSEIF (ScheduleIndex == 0) THEN
    MinValue = 0.0d0
    MaxValue = 0.0d0
  ELSEIF (ScheduleIndex < 1 .or. ScheduleIndex > NumSchedules) THEN
    CALL ShowFatalError('CheckScheduleMaxValue called with ScheduleIndex out of range')
  ENDIF

  IF (ScheduleIndex > 0) THEN
    IF (.not. Schedule(ScheduleIndex)%MaxMinSet) THEN  ! Set Minimum/Maximums for this schedule
      WkSch=Schedule(ScheduleIndex)%WeekSchedulePointer(1)
      MinValue=MINVAL(DaySchedule(WeekSchedule(WkSch)%DaySchedulePointer(1))%TSValue)
      MaxValue=MAXVAL(DaySchedule(WeekSchedule(WkSch)%DaySchedulePointer(1))%TSValue)
      DO DayT=2,MaxDayTypes
        MinValue=MIN(MinValue,MINVAL(DaySchedule(WeekSchedule(WkSch)%DaySchedulePointer(DayT))%TSValue))
        MaxValue=MAX(MaxValue,MAXVAL(DaySchedule(WeekSchedule(WkSch)%DaySchedulePointer(DayT))%TSValue))
      ENDDO
      DO Loop=2,366
        WkSch=Schedule(ScheduleIndex)%WeekSchedulePointer(Loop)
        DO DayT=1,MaxDayTypes
          MinValue=MIN(MinValue,MINVAL(DaySchedule(WeekSchedule(WkSch)%DaySchedulePointer(DayT))%TSValue))
          MaxValue=MAX(MaxValue,MAXVAL(DaySchedule(WeekSchedule(WkSch)%DaySchedulePointer(DayT))%TSValue))
        ENDDO
      ENDDO
      Schedule(ScheduleIndex)%MaxMinSet=.true.
      Schedule(ScheduleIndex)%MinValue=MinValue
      Schedule(ScheduleIndex)%MaxValue=MaxValue
    ENDIF

    !  Min/max for schedule has been set.

    MaximumValue=Schedule(ScheduleIndex)%MaxValue
  ELSE
    MaximumValue=MaxValue
  ENDIF

  RETURN

END FUNCTION GetScheduleMaxValue


FUNCTION GetScheduleName(ScheduleIndex) RESULT(ScheduleName)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   February 2008
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function returns the schedule name from the Schedule Index.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: ScheduleIndex
  CHARACTER(len=MaxNameLength) :: ScheduleName

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
          ! na

  IF (.not. ScheduleInputProcessed) THEN
    CALL ProcessScheduleInput
    ScheduleInputProcessed=.True.
  ENDIF

  IF (ScheduleIndex > 0) THEN
    ScheduleName=Schedule(ScheduleIndex)%Name
  ELSEIF (ScheduleIndex == -1) THEN
    ScheduleName='Constant-1.0'
  ELSEIF (ScheduleIndex == 0) THEN
    ScheduleName='Constant-0.0'
  ELSE
    ScheduleName='N/A-Invalid'
  ENDIF

  RETURN

END FUNCTION GetScheduleName

SUBROUTINE ReportScheduleValues

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   February 2004
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine puts the proper current schedule values into the "reporting"
          ! slot for later reporting.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataEnvironment, ONLY: DayOfYear_Schedule


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
  INTEGER ScheduleIndex
  INTEGER WhichHour
  LOGICAL, SAVE :: DoScheduleReportingSetup=.true.
  INTEGER WeekSchedulePointer
  INTEGER DaySchedulePointer

  IF (.not. ScheduleInputProcessed) THEN
    CALL ProcessScheduleInput
    ScheduleInputProcessed=.true.
  ENDIF

  IF (DoScheduleReportingSetup) THEN  ! CurrentModuleObject='Any Schedule'
    DO ScheduleIndex=1,NumSchedules
       ! Set Up Reporting
      CALL SetupOutputVariable('Schedule Value []',Schedule(ScheduleIndex)%CurrentValue,'Zone','Average',  &
                                Schedule(ScheduleIndex)%Name)
    ENDDO
    DoScheduleReportingSetup=.false.
  ENDIF

  WhichHour=HourOfDay+DSTIndicator
  DO ScheduleIndex=1,NumSchedules
    ! Determine which Week Schedule is used
    !  Cant use stored day of year because of leap year inconsistency
    WeekSchedulePointer=Schedule(ScheduleIndex)%WeekSchedulePointer(DayOfYear_Schedule)

    ! Now, which day?
    IF (DayofWeek <= 7 .and. HolidayIndex > 0) THEN
      DaySchedulePointer=WeekSchedule(WeekSchedulePointer)%DaySchedulePointer(7+HolidayIndex)
    ELSE
      DaySchedulePointer=WeekSchedule(WeekSchedulePointer)%DaySchedulePointer(DayofWeek)
    ENDIF


    ! Hourly Value
    IF (WhichHour <= 24) THEN
      Schedule(ScheduleIndex)%CurrentValue=DaySchedule(DaySchedulePointer)%TSValue(WhichHour,TimeStep)
    ELSEIF (TimeStep <= NumOfTimeStepInHour) THEN
      Schedule(ScheduleIndex)%CurrentValue=DaySchedule(DaySchedulePointer)%TSValue(WhichHour-24,TimeStep)
    ELSE
      Schedule(ScheduleIndex)%CurrentValue=DaySchedule(DaySchedulePointer)%TSValue(WhichHour-24,NumOfTimeStepInHour)
    ENDIF

    IF (Schedule(ScheduleIndex)%EMSActuatedOn) THEN
      Schedule(ScheduleIndex)%CurrentValue = Schedule(ScheduleIndex)%EMSValue
    ENDIF

  ENDDO

  RETURN

END SUBROUTINE ReportScheduleValues

SUBROUTINE ReportOrphanSchedules

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   April 2008
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! In response to CR7498, report orphan (unused) schedule items.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataGlobals, ONLY: DisplayUnusedSchedules
  USE General, ONLY: RoundSigDigits

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
  LOGICAL :: NeedOrphanMessage
  LOGICAL :: NeedUseMessage
  INTEGER :: Item
  INTEGER :: NumCount

  NeedOrphanMessage=.true.
  NeedUseMessage=.false.
  NumCount=0

  DO Item=1,NumSchedules
    IF (Schedule(Item)%Used) CYCLE
    IF (NeedOrphanMessage .and. DisplayUnusedSchedules) THEN
      CALL ShowWarningError('The following schedule names are "Unused Schedules".  These schedules are in the idf')
      CALL ShowContinueError(' file but are never obtained by the simulation and therefore are NOT used.')
      NeedOrphanMessage=.false.
    ENDIF
    IF (DisplayUnusedSchedules) THEN
      CALL ShowMessage('Schedule:Year or Schedule:Compact or Schedule:File or Schedule:Constant='//TRIM(Schedule(Item)%Name))
    ELSE
      NumCount=NumCount+1
    ENDIF
  ENDDO

  IF (NumCount > 0) THEN
    CALL ShowMessage('There are '//trim(RoundSigDigits(NumCount))//' unused schedules in input.')
    NeedUseMessage=.true.
  ENDIF

  NeedOrphanMessage=.true.
  NumCount=0

  DO Item=1,NumWeekSchedules
    IF (WeekSchedule(Item)%Used) CYCLE
    IF (WeekSchedule(Item)%Name == Blank) CYCLE
    IF (NeedOrphanMessage .and. DisplayUnusedSchedules) THEN
      CALL ShowWarningError('The following week schedule names are "Unused Schedules".  These schedules are in the idf')
      CALL ShowContinueError(' file but are never obtained by the simulation and therefore are NOT used.')
      NeedOrphanMessage=.false.
    ENDIF
    IF (DisplayUnusedSchedules) THEN
      CALL ShowMessage('Schedule:Week:Daily or Schedule:Week:Compact='//TRIM(WeekSchedule(Item)%Name))
    ELSE
      NumCount=NumCount+1
    ENDIF
  ENDDO

  IF (NumCount > 0) THEN
    CALL ShowMessage('There are '//trim(RoundSigDigits(NumCount))//' unused week schedules in input.')
    NeedUseMessage=.true.
  ENDIF

  NeedOrphanMessage=.true.
  NumCount=0

  DO Item=1,NumDaySchedules
    IF (DaySchedule(Item)%Used) CYCLE
    IF (DaySchedule(Item)%Name == Blank) CYCLE
    IF (NeedOrphanMessage .and. DisplayUnusedSchedules) THEN
      CALL ShowWarningError('The following day schedule names are "Unused Schedules".  These schedules are in the idf')
      CALL ShowContinueError(' file but are never obtained by the simulation and therefore are NOT used.')
      NeedOrphanMessage=.false.
    ENDIF
    IF (DisplayUnusedSchedules) THEN
      CALL ShowMessage('Schedule:Day:Hourly or Schedule:Day:Interval or Schedule:Day:List='//TRIM(DaySchedule(Item)%Name))
    ELSE
      NumCount=NumCount+1
    ENDIF
  ENDDO

  IF (NumCount > 0) THEN
    CALL ShowMessage('There are '//trim(RoundSigDigits(NumCount))//' unused day schedules in input.')
    NeedUseMessage=.true.
  ENDIF

  IF (NeedUseMessage) CALL ShowMessage('Use Output:Diagnostics,DisplayUnusedSchedules; to see them.')

  RETURN

END SUBROUTINE ReportOrphanSchedules

FUNCTION ScheduleAverageHoursPerWeek(ScheduleIndex,StartDayOfWeek,isItLeapYear) RESULT(AverageHoursPerWeek)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   August 2006
          !       MODIFIED       September 2012; Glazer - CR8849
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function returns the "average" hours per week for a schedule over
          ! the entire year.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)  :: ScheduleIndex  ! Which Schedule being tested
  INTEGER, INTENT(IN)  :: StartDayOfWeek ! Day of week for start of year
  LOGICAL, INTENT(IN)  :: isItLeapYear   ! true if it is a leap year containing February 29
  REAL(r64)            :: AverageHoursPerWeek   ! Average Hours Per Week

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER    :: WkSch
  INTEGER    :: DayT
  INTEGER    :: Loop
  REAL(r64)  :: TotalHours
  REAL(r64)  :: WeeksInYear
  INTEGER    :: DaysInYear

  IF (isItLeapYear) THEN
    DaysInYear = 366
    WeeksInYear = 366.d0/7.d0
  ELSE
    DaysInYear =  365
    WeeksInYear = 365.d0/7.d0
  ENDIF

  IF (ScheduleIndex < -1 .or. ScheduleIndex > NumSchedules) THEN
    CALL ShowFatalError('ScheduleAverageHoursPerWeek called with ScheduleIndex out of range')
  ENDIF

  DayT=StartDayOfWeek
  AverageHoursPerWeek=0.0d0
  TotalHours=0.0d0

  IF (DayT == 0) RETURN

  DO Loop=1,DaysInYear
    WkSch=Schedule(ScheduleIndex)%WeekSchedulePointer(Loop)
    TotalHours=TotalHours+SUM(DaySchedule(WeekSchedule(WkSch)%DaySchedulePointer(DayT))%TSValue)/REAL(NumOfTimeStepInHour,r64)
    DayT=DayT+1
    IF (DayT > 7) DayT=1
  ENDDO

  !  Total hours for year have been set.

  AverageHoursPerWeek=TotalHours/WeeksInYear

  RETURN

END FUNCTION ScheduleAverageHoursPerWeek

FUNCTION GetNumberOfSchedules() RESULT(NumberOfSchedules)

    ! FUNCTION INFORMATION:
    !       AUTHOR         Greg Stark
    !       DATE WRITTEN   September 2008
    !       MODIFIED       na
    !       RE-ENGINEERED  na

    ! PURPOSE OF THIS FUNCTION:
    ! This function returns the number of schedules.

    ! METHODOLOGY EMPLOYED:
    ! na

    ! REFERENCES:
    ! na

    ! USE STATEMENTS:
    ! na

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER :: NumberOfSchedules

    ! FUNCTION PARAMETER DEFINITIONS:
    ! na

    ! INTERFACE BLOCK SPECIFICATIONS
    ! na

    ! DERIVED TYPE DEFINITIONS
    ! na

    ! FUNCTION LOCAL VARIABLE DECLARATIONS:
    ! na

    NumberOfSchedules = NumSchedules

    RETURN

END FUNCTION GetNumberOfSchedules

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

END MODULE ScheduleManager


