MODULE DemandManager

        ! MODULE INFORMATION:
        !       AUTHOR         Peter Graham Ellis
        !       DATE WRITTEN   July 2005
        !       MODIFIED       na
        !       RE-ENGINEERED  na

        ! PURPOSE OF THIS MODULE:
        ! This module provides controls for demand limiting various loads.

        ! METHODOLOGY EMPLOYED:
        ! ManageDemand is called from within the ManageHVAC routine after the first pass through SimHVAC, but
        ! _before_ any variables are reported or histories are updated.  If the metered demand is above the
        ! limit action is taken using the various demand managers to reduce loads.  Exterior energy use, zone
        ! heat balance, and HVAC system are then resimulated as necessary.  It is possible to iterate several
        ! times through ManageDemand before the final demand managers are established and the timestep can be
        ! completed.

        ! USE STATEMENTS:
USE DataPrecisionGlobals
USE DataGlobals, ONLY: MaxNameLength, NumOfTimeStepInHour, SecInHour, ScheduleAlwaysOn

IMPLICIT NONE ! Enforce explicit typing of all variables

PRIVATE ! Everything private unless explicitly made public

          ! MODULE PARAMETER DEFINITIONS:
INTEGER, PARAMETER :: ManagerTypeExtLights = 1
INTEGER, PARAMETER :: ManagerTypeLights = 2
INTEGER, PARAMETER :: ManagerTypeElecEquip = 3
INTEGER, PARAMETER :: ManagerTypeThermostats = 4

INTEGER, PARAMETER :: ManagerPrioritySequential = 1
INTEGER, PARAMETER :: ManagerPriorityOptimal = 2
INTEGER, PARAMETER :: ManagerPriorityAll = 3

INTEGER, PARAMETER :: ManagerLimitOff = 1
INTEGER, PARAMETER :: ManagerLimitFixed = 2
INTEGER, PARAMETER :: ManagerLimitVariable = 3

INTEGER, PARAMETER :: ManagerSelectionAll = 1
INTEGER, PARAMETER :: ManagerSelectionMany = 2
INTEGER, PARAMETER :: ManagerSelectionOne = 3

INTEGER, PARAMETER :: CheckCanReduce = 1
INTEGER, PARAMETER :: SetLimit = 2
INTEGER, PARAMETER :: ClearLimit = 3

          ! DERIVED TYPE DEFINITIONS:
TYPE DemandManagerListData
  CHARACTER(len=MaxNameLength)   :: Name = ''                     ! Name of DEMAND MANAGER LIST
  INTEGER                        :: Meter = 0                     ! Index to meter to demand limit
  INTEGER                        :: LimitSchedule = 0             ! Schedule index for demand limit
  REAL(r64)                      :: SafetyFraction = 1.0d0          ! Multiplier applied to demand limit schedule
  INTEGER                        :: BillingSchedule = 0           ! Schedule index for billing month periods
  REAL(r64)                      :: BillingPeriod = 0.0d0           ! Current billing period value
  INTEGER                        :: PeakSchedule = 0              ! Schedule index for billing month periods
  INTEGER                        :: AveragingWindow = 1           ! Number of timesteps for averaging demand window
  REAL(r64), ALLOCATABLE, DIMENSION(:)    :: History                   ! Demand window history
  INTEGER                        :: ManagerPriority = 0           ! Indicator for priority (SEQUENTIAL, OPTIMAL, ALL)
  INTEGER                        :: NumOfManager                  ! Number of DEMAND MANAGERs
  INTEGER, ALLOCATABLE, DIMENSION(:) :: Manager                   ! Indexes for DEMAND MANAGERs

  REAL(r64)                      :: MeterDemand = 0.0d0             ! Meter demand at this timestep
  REAL(r64)                      :: AverageDemand = 0.0d0           ! Current demand over the demand window
  REAL(r64)                      :: PeakDemand = 0.0d0              ! Peak demand in the billing month so far

  REAL(r64)                      :: ScheduledLimit = 0.0d0          ! Scheduled demand limit
  REAL(r64)                      :: DemandLimit = 0.0d0             ! Scheduled demand limit * Safety Fraction
  REAL(r64)                      :: AvoidedDemand = 0.0d0           ! Demand avoided by active DEMAND MANAGERs
  REAL(r64)                      :: OverLimit = 0.0d0               ! Amount that demand limit is exceeded
  REAL(r64)                      :: OverLimitDuration = 0.0d0       ! Number of hours that demand limit is exceeded

END TYPE DemandManagerListData

TYPE DemandManagerData
  CHARACTER(len=MaxNameLength)   :: Name = ''                     ! Name of DEMAND MANAGER
  INTEGER                        :: Type = 0                      ! Type of DEMAND MANAGER (:LIGHTS, :ELECTRICEQUIPMENT, etc.)
  INTEGER                        :: DemandManagerList = 0         ! Reference to parent DEMAND MANAGER LIST for error checking
  LOGICAL                        :: CanReduceDemand = .FALSE.     ! Flag to indicate whether manager can reduce demand

  INTEGER                        :: AvailSchedule = 0             ! Schedule index pointer for Availability Schedule
  LOGICAL                        :: Available = .FALSE.           ! Availability flag
  LOGICAL                        :: Activate = .FALSE.            ! Flag to activate the manager
  LOGICAL                        :: Active = .FALSE.              ! Flag to indicate that the manager is active

  INTEGER                        :: LimitControl = 0              !
  INTEGER                        :: SelectionControl = 0          !

  INTEGER                        :: LimitDuration = 0             ! Minimum duration of demand manager activity (min)
  INTEGER                        :: ElapsedTime = 0               ! Elapsed time for the demand manager activity (min)

  INTEGER                        :: RotationDuration = 0          ! Rotation duration (min)
  INTEGER                        :: ElapsedRotationTime = 0       ! Elapsed time for the current rotation (min)
  INTEGER                        :: RotatedLoadNum = 0            ! Index for rotated load

  REAL(r64)                      :: LowerLimit = 0.0d0              ! Lowest demand limit as fraction of design level
                                                                  ! Lowest heating setpoint for thermostats
  REAL(r64)                      :: UpperLimit = 0.0d0              ! Not used for demand limit
                                                                  ! Highest cooling setpoint for thermostats
  INTEGER                        :: NumOfLoads = 0                ! Number of load objects
  INTEGER, ALLOCATABLE, DIMENSION(:) :: Load                      ! Pointers to load objects
END TYPE DemandManagerData

          ! MODULE VARIABLE TYPE DECLARATIONS:
TYPE(DemandManagerListData), ALLOCATABLE, DIMENSION(:) :: DemandManagerList
TYPE(DemandManagerData), ALLOCATABLE, DIMENSION(:) :: DemandMgr

          ! MODULE VARIABLE DECLARATIONS:
INTEGER :: NumDemandManagerList=0
INTEGER :: NumDemandMgr=0
INTEGER, PUBLIC :: DemandManagerExtIterations=0
INTEGER, PUBLIC :: DemandManagerHBIterations=0
INTEGER, PUBLIC :: DemandManagerHVACIterations=0
LOGICAL, SAVE :: GetInput = .TRUE.  ! Flag to prevent input from being read multiple times

          ! SUBROUTINE SPECIFICATIONS:
PUBLIC ManageDemand
PUBLIC UpdateDemandManagers
PRIVATE GetDemandManagerListInput
PRIVATE SimulateDemandManagerList
PRIVATE ReportDemandManagerList
PRIVATE GetDemandManagerInput
PRIVATE SurveyDemandManagers
PRIVATE ActivateDemandManagers
PUBLIC  InitDemandManagers

CONTAINS

          ! MODULE SUBROUTINES:
SUBROUTINE ManageDemand

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Peter Graham Ellis
          !       DATE WRITTEN   July 2005
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !

          ! METHODOLOGY EMPLOYED:
          ! Standard EnergyPlus methodology.

          ! USE STATEMENTS:
  USE DataGlobals, ONLY: WarmupFlag, DoingSizing
  USE DataInterfaces, ONLY: ShowFatalError

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: ListNum                  !
  LOGICAL, SAVE :: FirstTime                ! Flag to allow Demand Manager List to simulate at least once
  LOGICAL, SAVE :: ResimExt                 ! Flag to resimulate the exterior energy use simulation
  LOGICAL, SAVE :: ResimHB                  ! Flag to resimulate the heat balance simulation (including HVAC)
  LOGICAL, SAVE :: ResimHVAC                ! Flag to resimulate the HVAC simulation
  LOGICAL, SAVE :: BeginDemandSim     ! TRUE in the first timestep after warmup of a new environment
  LOGICAL, SAVE :: ClearHistory       ! TRUE in the first timestep during warmup of a new environment

          ! FLOW:
  IF (GetInput .AND. .NOT. DoingSizing) THEN
    CALL GetDemandManagerInput
    CALL GetDemandManagerListInput
    GetInput = .FALSE.
  END IF

  IF (NumDemandManagerList > 0) THEN

    IF (WarmupFlag) THEN
      BeginDemandSim = .TRUE.
      IF (ClearHistory) THEN
        ! Clear historical variables
        DO ListNum = 1, NumDemandManagerList
          DemandManagerList(ListNum)%History = 0.0d0
          DemandManagerList(ListNum)%MeterDemand = 0.0d0
          DemandManagerList(ListNum)%AverageDemand = 0.0d0
          DemandManagerList(ListNum)%PeakDemand = 0.0d0
          DemandManagerList(ListNum)%ScheduledLimit = 0.0d0
          DemandManagerList(ListNum)%DemandLimit = 0.0d0
          DemandManagerList(ListNum)%AvoidedDemand = 0.0d0
          DemandManagerList(ListNum)%OverLimit = 0.0d0
          DemandManagerList(ListNum)%OverLimitDuration = 0.0d0
        END DO  ! ListNum

        ! Clear demand manager variables
        DemandMgr%Active = .FALSE.
        DemandMgr%ElapsedTime = 0
        DemandMgr%ElapsedRotationTime = 0
        DemandMgr%RotatedLoadNum = 0
      ENDIF
      ClearHistory=.false.
    ENDIF


    IF (.NOT. WarmupFlag .AND. .NOT. DoingSizing) THEN

      IF (BeginDemandSim) THEN
        BeginDemandSim = .FALSE.
        ClearHistory=.true.
      END IF

      DemandManagerExtIterations = 0
      DemandManagerHBIterations = 0
      DemandManagerHVACIterations = 0

      FirstTime = .TRUE.
      ResimExt = .FALSE.
      ResimHB = .FALSE.
      ResimHVAC = .FALSE.

      DO WHILE (FirstTime .OR. ResimExt .OR. ResimHB .OR. ResimHVAC)
        FirstTime = .FALSE.

        CALL Resimulate(ResimExt, ResimHB, ResimHVAC)
        ResimExt = .FALSE.
        ResimHB = .FALSE.
        ResimHVAC = .FALSE.

        CALL SurveyDemandManagers  ! Determines which Demand Managers can reduce demand

        DO ListNum = 1, NumDemandManagerList
          CALL SimulateDemandManagerList(ListNum, ResimExt, ResimHB, ResimHVAC)
        END DO  ! ListNum

        CALL ActivateDemandManagers  ! Sets limits on loads

        IF (DemandManagerExtIterations + DemandManagerHBIterations + DemandManagerHVACIterations > 500) THEN
          ! This error can only happen if there is a bug in the code
          CALL ShowFatalError('Too many DemandManager iterations. (>500)')
          EXIT
        END IF
      END DO

      DO ListNum = 1, NumDemandManagerList
        CALL ReportDemandManagerList(ListNum)
      END DO  ! ListNum

    END IF

  END IF

  RETURN

END SUBROUTINE ManageDemand


SUBROUTINE SimulateDemandManagerList(ListNum, ResimExt, ResimHB, ResimHVAC)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Peter Graham Ellis
          !       DATE WRITTEN   July 2005
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !

          ! METHODOLOGY EMPLOYED:
          !

          ! USE STATEMENTS:
  USE ScheduleManager, ONLY: GetCurrentScheduleValue
  USE DataGlobals, ONLY: TimeStepZone, SecInHour
  USE DataHVACGlobals, ONLY : TimeStepSys

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: ListNum
  LOGICAL, INTENT(INOUT) :: ResimExt   ! Flag to resimulate the exterior energy use simulation
  LOGICAL, INTENT(INOUT) :: ResimHB    ! Flag to resimulate the heat balance simulation (including HVAC)
  LOGICAL, INTENT(INOUT) :: ResimHVAC  ! Flag to resimulate the HVAC simulation

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER                :: MgrNum
  INTEGER                :: MgrPtr
  REAL(r64)              :: AverageDemand
  REAL(r64)              :: OverLimit
  LOGICAL                :: OnPeak
  REAL(r64), EXTERNAL         :: GetInstantMeterValue

          ! FLOW:
  DemandManagerList(ListNum)%ScheduledLimit = GetCurrentScheduleValue(DemandManagerList(ListNum)%LimitSchedule)
  DemandManagerList(ListNum)%DemandLimit = DemandManagerList(ListNum)%ScheduledLimit &
    * DemandManagerList(ListNum)%SafetyFraction

  DemandManagerList(ListNum)%MeterDemand = &
    GetInstantMeterValue(DemandManagerList(ListNum)%Meter, 1) / (TimeStepZone * SecInHour) &
    + GetInstantMeterValue(DemandManagerList(ListNum)%Meter, 2) / (TimeStepSys  * SecInHour)

  ! Calculate average demand over the averaging window including the current timestep meter demand
  AverageDemand = DemandManagerList(ListNum)%AverageDemand &
    + (DemandManagerList(ListNum)%MeterDemand - DemandManagerList(ListNum)%History(1)) &
    / DemandManagerList(ListNum)%AveragingWindow

  IF (DemandManagerList(ListNum)%PeakSchedule .EQ. 0) THEN
    OnPeak = .TRUE.
  ELSE
    IF (GetCurrentScheduleValue(DemandManagerList(ListNum)%PeakSchedule) .EQ. 1) THEN
      OnPeak = .TRUE.
    ELSE
      OnPeak = .FALSE.
    END IF
  END IF

  IF (OnPeak) THEN
    OverLimit = AverageDemand - DemandManagerList(ListNum)%DemandLimit

    IF (OverLimit > 0.0d0) THEN

      SELECT CASE (DemandManagerList(ListNum)%ManagerPriority)

        CASE (ManagerPrioritySequential)  ! Activate first Demand Manager that can reduce demand

          DO MgrNum = 1, DemandManagerList(ListNum)%NumOfManager
            MgrPtr = DemandManagerList(ListNum)%Manager(MgrNum)

            IF (DemandMgr(MgrPtr)%CanReduceDemand) THEN
              DemandMgr(MgrPtr)%Activate = .TRUE.

              SELECT CASE (DemandMgr(MgrPtr)%Type)
                CASE (ManagerTypeExtLights)
                  ResimExt = .TRUE.

                CASE (ManagerTypeLights, ManagerTypeElecEquip)
                  ResimHB = .TRUE.
                  ResimHVAC = .TRUE.

                CASE (ManagerTypeThermostats)
                  ResimHVAC = .TRUE.

              END SELECT

              EXIT  ! Leave the loop
            END IF
          END DO  ! MgrNum

        CASE (ManagerPriorityOptimal)
          ! Not yet implemented

        CASE (ManagerPriorityAll)  ! Activate ALL Demand Managers that can reduce demand

          DO MgrNum = 1, DemandManagerList(ListNum)%NumOfManager
            MgrPtr = DemandManagerList(ListNum)%Manager(MgrNum)

            IF (DemandMgr(MgrPtr)%CanReduceDemand) THEN
              DemandMgr(MgrPtr)%Activate = .TRUE.

              SELECT CASE (DemandMgr(MgrPtr)%Type)
                CASE (ManagerTypeExtLights)
                  ResimExt = .TRUE.

                CASE (ManagerTypeLights, ManagerTypeElecEquip)
                  ResimHB = .TRUE.
                  ResimHVAC = .TRUE.

                CASE (ManagerTypeThermostats)
                  ResimHVAC = .TRUE.

              END SELECT

            END IF
          END DO  ! MgrNum

      END SELECT

    END IF
  END IF

  RETURN

END SUBROUTINE SimulateDemandManagerList


SUBROUTINE GetDemandManagerListInput

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Peter Graham Ellis
          !       DATE WRITTEN   July 2005
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Gets the DEMAND MANAGER LIST input from the input file.

          ! METHODOLOGY EMPLOYED:
          ! Standard EnergyPlus methodology.

          ! USE STATEMENTS:
  USE DataGlobals, ONLY: MaxNameLength, MinutesPerTimeStep
  USE DataInterfaces, ONLY: ShowSevereError, ShowWarningError, ShowFatalError, &
                         SetupOutputVariable, ShowContinueError
  USE InputProcessor, ONLY: GetNumObjectsFound, GetObjectItem, VerifyName, FindItemInList, GetObjectDefMaxArgs
  USE DataIPShortCuts  ! Data for field names, blank numerics
  USE ScheduleManager, ONLY: GetScheduleIndex
  USE OutputProcessor, ONLY: EnergyMeters

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER :: Blank = ' '

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER                     :: ListNum
  INTEGER                     :: MgrNum
  INTEGER                     :: NumAlphas ! Number of elements in the alpha array
  INTEGER                     :: NumNums   ! Number of elements in the numeric array
  INTEGER                     :: IOStat    ! IO Status when calling get input subroutine
  CHARACTER(len=MaxNameLength),DIMENSION(:),ALLOCATABLE :: AlphArray  ! Character string data
  REAL(r64), DIMENSION(:),ALLOCATABLE          :: NumArray  ! Numeric data
  LOGICAL                     :: IsNotOK   ! Flag to verify name
  LOGICAL                     :: IsBlank   ! Flag for blank name
  CHARACTER(len=5)            :: Units     ! String for meter units
  INTEGER, EXTERNAL           :: GetMeterIndex
  LOGICAL                     :: ErrorsFound = .FALSE.
  CHARACTER(len=MaxNameLength) :: CurrentModuleObject  ! for ease in renaming.

          ! FLOW:
  CurrentModuleObject = 'DemandManagerAssignmentList'
  CALL GetObjectDefMaxArgs(CurrentModuleObject,ListNum,NumAlphas,NumNums)

  NumDemandManagerList = GetNumObjectsFound(CurrentModuleObject)

  IF (NumDemandManagerList > 0) THEN
    ALLOCATE(AlphArray(NumAlphas))
    AlphArray=Blank
    ALLOCATE(NumArray(NumNums))
    NumArray=0.0d0

    ALLOCATE(DemandManagerList(NumDemandManagerList))

    DO ListNum = 1, NumDemandManagerList

      CALL GetObjectItem(CurrentModuleObject,ListNum,AlphArray,NumAlphas,NumArray,NumNums,IOSTAT, &
                    AlphaBlank=lAlphaFieldBlanks, AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)


      IsNotOK = .FALSE.
      IsBlank = .FALSE.
      CALL VerifyName(AlphArray(1),DemandManagerList%Name,ListNum-1,IsNotOK,IsBlank,TRIM(CurrentModuleObject)//' Name')
      IF (IsNotOK) THEN
        ErrorsFound = .TRUE.
        IF (IsBlank) AlphArray(1) = 'xxxxx'
      END IF
      DemandManagerList(ListNum)%Name = AlphArray(1)

      DemandManagerList(ListNum)%Meter = GetMeterIndex(AlphArray(2))

      IF (DemandManagerList(ListNum)%Meter .EQ. 0) THEN
        CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(2))//'='//TRIM(AlphArray(2)))
        CALL ShowContinueError('Entered in '//TRIM(CurrentModuleObject)//'='//TRIM(AlphArray(1)))
        ErrorsFound = .TRUE.

      ELSE
        SELECT CASE (EnergyMeters(DemandManagerList(ListNum)%Meter)%ResourceType)
          CASE ('Electricity', 'ElectricityNet')
            Units = '[W]'  ! For setup of report variables

          CASE DEFAULT
            CALL ShowSevereError(trim(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid value '//  &
               trim(cAlphaFieldNames(2))//'="'//trim(AlphArray(2))//'".')
            CALL ShowContinueError('Only Electricity and ElectricityNet meters are currently allowed.')
            ErrorsFound = .TRUE.
        END SELECT
      END IF

! Further checking for conflicting DEMAND MANAGER LISTs



      IF ( .NOT. lAlphaFieldBlanks(3) ) THEN
        DemandManagerList(ListNum)%LimitSchedule = GetScheduleIndex(AlphArray(3))

        IF (DemandManagerList(ListNum)%LimitSchedule .EQ. 0) THEN
          CALL ShowSevereError(trim(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid '//  &
             trim(cAlphaFieldNames(3))//'="'//trim(AlphArray(3))//'" not found.')
          ErrorsFound = .TRUE.
        END IF
      END IF

      DemandManagerList(ListNum)%SafetyFraction = NumArray(1)

      IF ( .NOT. lAlphaFieldBlanks(4) ) THEN
        DemandManagerList(ListNum)%BillingSchedule = GetScheduleIndex(AlphArray(4))

        IF (DemandManagerList(ListNum)%BillingSchedule .EQ. 0) THEN
          CALL ShowSevereError(trim(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid '//  &
             trim(cAlphaFieldNames(4))//'="'//trim(AlphArray(4))//'" not found.')
          ErrorsFound = .TRUE.
        END IF
      END IF

      IF ( .NOT. lAlphaFieldBlanks(5) ) THEN
        DemandManagerList(ListNum)%PeakSchedule = GetScheduleIndex(AlphArray(5))

        IF (DemandManagerList(ListNum)%PeakSchedule .EQ. 0) THEN
          CALL ShowSevereError(trim(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid '//  &
             trim(cAlphaFieldNames(5))//'="'//trim(AlphArray(5))//'" not found.')
          ErrorsFound = .TRUE.
        END IF
      END IF

      DemandManagerList(ListNum)%AveragingWindow = MAX(INT(NumArray(2) / MinutesPerTimeStep), 1)
! Round to nearest timestep
! Can make this fancier to include windows that do not fit the timesteps
      ALLOCATE(DemandManagerList(ListNum)%History(DemandManagerList(ListNum)%AveragingWindow))
      DemandManagerList(ListNum)%History = 0.0d0

      ! Validate Demand Manager Priority
      SELECT CASE (AlphArray(6))
        CASE ('SEQUENTIAL')
          DemandManagerList(ListNum)%ManagerPriority = ManagerPrioritySequential

        CASE ('OPTIMAL')
          DemandManagerList(ListNum)%ManagerPriority = ManagerPriorityOptimal

        CASE ('ALL')
          DemandManagerList(ListNum)%ManagerPriority = ManagerPriorityAll

        CASE DEFAULT
          CALL ShowSevereError(trim(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid value '//  &
             trim(cAlphaFieldNames(6))//'="'//trim(AlphArray(6))//'" not found.')
          ErrorsFound = .TRUE.
      END SELECT

      ! Get DEMAND MANAGER Type and Name pairs
      DemandManagerList(ListNum)%NumOfManager = INT((NumAlphas - 6) / 2.0d0)

      IF (DemandManagerList(ListNum)%NumOfManager > 0) THEN
        ALLOCATE(DemandManagerList(ListNum)%Manager(DemandManagerList(ListNum)%NumOfManager))

        DO MgrNum = 1, DemandManagerList(ListNum)%NumOfManager

          ! Validate DEMAND MANAGER Type
          SELECT CASE (AlphArray(MgrNum*2 + 5))
            CASE ('DEMANDMANAGER:LIGHTS', &
                  'DEMANDMANAGER:EXTERIORLIGHTS', &
                  'DEMANDMANAGER:ELECTRICEQUIPMENT', &
                  'DEMANDMANAGER:THERMOSTATS')

              DemandManagerList(ListNum)%Manager(MgrNum) = FindItemInList(AlphArray(MgrNum*2 + 6),DemandMgr%Name,NumDemandMgr)

              IF (DemandManagerList(ListNum)%Manager(MgrNum) .EQ. 0) THEN
                CALL ShowSevereError(trim(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid '//  &
                         trim(cAlphaFieldNames(MgrNum*2 + 6))//'="'//trim(AlphArray(MgrNum*2 + 6))//'" not found.')
                ErrorsFound = .TRUE.
              END IF

            CASE DEFAULT
                CALL ShowSevereError(trim(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid value '//  &
                         trim(cAlphaFieldNames(MgrNum*2 + 5))//'="'//trim(AlphArray(MgrNum*2 + 5))//'".')
              ErrorsFound = .TRUE.
          END SELECT

          ! Check that each is not already referenced using %DemandManagerList field

        END DO  ! MgrNum
      END IF

      ! Setup report variables
      CALL SetupOutputVariable('Demand Manager Meter Demand Power [W]', &
        DemandManagerList(ListNum)%MeterDemand,'Zone','Average', &
        DemandManagerList(ListNum)%Name)

      CALL SetupOutputVariable('Demand Manager Average Demand Power [W]', &
        DemandManagerList(ListNum)%AverageDemand,'Zone','Average', &
        DemandManagerList(ListNum)%Name)

      CALL SetupOutputVariable('Demand Manager Peak Demand Power [W]', &
        DemandManagerList(ListNum)%PeakDemand,'Zone','Average', &
        DemandManagerList(ListNum)%Name)

      CALL SetupOutputVariable('Demand Manager Scheduled Limit Power [W]', &
        DemandManagerList(ListNum)%ScheduledLimit,'Zone','Average', &
        DemandManagerList(ListNum)%Name)

      CALL SetupOutputVariable('Demand Manager Demand Limit Power [W]', &
        DemandManagerList(ListNum)%DemandLimit,'Zone','Average', &
        DemandManagerList(ListNum)%Name)

      CALL SetupOutputVariable('Demand Manager Over Limit Power [W]', &
        DemandManagerList(ListNum)%OverLimit,'Zone','Average', &
        DemandManagerList(ListNum)%Name)

      CALL SetupOutputVariable('Demand Manager Over Limit Time [hr]', &
        DemandManagerList(ListNum)%OverLimitDuration,'Zone','Sum', &
        DemandManagerList(ListNum)%Name)

      IF (ErrorsFound) THEN
        CALL ShowFatalError('Errors found in processing input for '//TRIM(CurrentModuleObject) )
      END IF

    END DO ! ListNum

    DEALLOCATE(AlphArray)
    DEALLOCATE(NumArray)

    ! Iteration diagnostic reporting for all DEMAND MANAGER LISTs
    CALL SetupOutputVariable('Demand Manager Exterior Energy Iteration Count []', &
      DemandManagerExtIterations,'Zone','Sum','ManageDemand')

    CALL SetupOutputVariable('Demand Manager Heat Balance Iteration Count []', &
      DemandManagerHBIterations,'Zone','Sum','ManageDemand')

    CALL SetupOutputVariable('Demand Manager HVAC Iteration Count []', &
      DemandManagerHVACIterations,'Zone','Sum','ManageDemand')

  END IF

  RETURN

END SUBROUTINE GetDemandManagerListInput


SUBROUTINE GetDemandManagerInput

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Peter Graham Ellis
          !       DATE WRITTEN   July 2005
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Gets the DEMAND MANAGER input from the input file.

          ! METHODOLOGY EMPLOYED:
          ! Standard EnergyPlus methodology.

          ! USE STATEMENTS:
  USE DataGlobals, ONLY: MaxNameLength, MinutesPerTimeStep
  USE DataInterfaces, ONLY: ShowSevereError, ShowWarningError, ShowFatalError, &
                         SetupOutputVariable, ShowContinueError
  USE InputProcessor, ONLY: GetNumObjectsFound, GetObjectItem, VerifyName, FindItemInList, GetObjectDefMaxArgs
  USE DataIPShortCuts  ! Data for field names, blank numerics
  USE ScheduleManager, ONLY: GetScheduleIndex
  USE DataHeatBalance, ONLY: Lights, TotLights, ZoneElectric, TotElecEquip, LightsObjects, NumLightsStatements,  &
                             ZoneElectricObjects, NumZoneElectricStatements
  USE ExteriorEnergyUse, ONLY: ExteriorLights, NumExteriorLights
  USE DataZoneControls, ONLY: TempControlledZone, NumTempControlledZones, TStatObjects, NumTStatStatements
  USE General, ONLY: RoundSigDigits

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER :: Blank = ' '

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER                     :: NumDemandMgrExtLights
  INTEGER                     :: NumDemandMgrLights
  INTEGER                     :: NumDemandMgrElecEquip
  INTEGER                     :: NumDemandMgrThermostats
  INTEGER                     :: MgrNum
  INTEGER                     :: StartIndex
  INTEGER                     :: EndIndex
  INTEGER                     :: LoadNum
  INTEGER                     :: LoadPtr
  INTEGER                     :: NumAlphas ! Number of elements in the alpha array
  INTEGER                     :: NumNums   ! Number of elements in the numeric array
  INTEGER                     :: MaxAlphas ! Max number of elements in the alpha array
  INTEGER                     :: MaxNums   ! Max number of elements in the numeric array
  INTEGER                     :: NumParams ! Number of arguments total in an ObjectDef
  INTEGER                     :: IOStat    ! IO Status when calling get input subroutine
  CHARACTER(len=MaxNameLength),DIMENSION(:),ALLOCATABLE :: AlphArray  ! Character string data
  REAL(r64), DIMENSION(:),ALLOCATABLE          :: NumArray  ! Numeric data
  LOGICAL                     :: IsNotOK   ! Flag to verify name
  LOGICAL                     :: IsBlank   ! Flag for blank name
  LOGICAL                     :: ErrorsFound = .FALSE.
  CHARACTER(len=MaxNameLength) :: CurrentModuleObject  ! for ease in renaming.
  INTEGER :: Item
  INTEGER :: Item1

          ! FLOW:
  MaxAlphas=0
  MaxNums=0
  CurrentModuleObject='DemandManager:ExteriorLights'
  NumDemandMgrExtLights = GetNumObjectsFound(CurrentModuleObject)
  IF (NumDemandMgrExtLights > 0) THEN
    CALL GetObjectDefMaxArgs(CurrentModuleObject,NumParams,NumAlphas,NumNums)
    MaxAlphas=MAX(MaxAlphas,NumAlphas)
    MaxNums=MAX(MaxNums,NumNums)
  ENDIF
  CurrentModuleObject='DemandManager:Lights'
  NumDemandMgrLights = GetNumObjectsFound(CurrentModuleObject)
  IF (NumDemandMgrLights > 0) THEN
    CALL GetObjectDefMaxArgs(CurrentModuleObject,NumParams,NumAlphas,NumNums)
    MaxAlphas=MAX(MaxAlphas,NumAlphas)
    MaxNums=MAX(MaxNums,NumNums)
  ENDIF
  CurrentModuleObject='DemandManager:ElectricEquipment'
  NumDemandMgrElecEquip = GetNumObjectsFound(CurrentModuleObject)
  IF (NumDemandMgrElecEquip > 0) THEN
    CALL GetObjectDefMaxArgs(CurrentModuleObject,NumParams,NumAlphas,NumNums)
    MaxAlphas=MAX(MaxAlphas,NumAlphas)
    MaxNums=MAX(MaxNums,NumNums)
  ENDIF
  CurrentModuleObject='DemandManager:Thermostats'
  NumDemandMgrThermostats = GetNumObjectsFound(CurrentModuleObject)
  IF (NumDemandMgrThermostats > 0) THEN
    CALL GetObjectDefMaxArgs(CurrentModuleObject,NumParams,NumAlphas,NumNums)
    MaxAlphas=MAX(MaxAlphas,NumAlphas)
    MaxNums=MAX(MaxNums,NumNums)
  ENDIF

  NumDemandMgr = NumDemandMgrExtLights + NumDemandMgrLights + NumDemandMgrElecEquip + NumDemandMgrThermostats

  IF (NumDemandMgr > 0) THEN
    ALLOCATE(AlphArray(MaxAlphas))
    AlphArray=Blank
    ALLOCATE(NumArray(MaxNums))
    NumArray=0.0d0

    ALLOCATE(DemandMgr(NumDemandMgr))

    ! Get input for DemandManager:ExteriorLights
    StartIndex = 1
    EndIndex = NumDemandMgrExtLights

    CurrentModuleObject = 'DemandManager:ExteriorLights'

    DO MgrNum = StartIndex, EndIndex

      CALL GetObjectItem(CurrentModuleObject,MgrNum - StartIndex + 1,AlphArray,NumAlphas,NumArray,NumNums,IOSTAT, &
                    AlphaBlank=lAlphaFieldBlanks, AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

      IsNotOK = .FALSE.
      IsBlank = .FALSE.
      CALL VerifyName(AlphArray(1),DemandMgr%Name,MgrNum - StartIndex,IsNotOK,IsBlank,TRIM(CurrentModuleObject)//' Name')
      IF (IsNotOK) THEN
        ErrorsFound = .TRUE.
        IF (IsBlank) AlphArray(1) = 'xxxxx'
      END IF
      DemandMgr(MgrNum)%Name = AlphArray(1)

      DemandMgr(MgrNum)%Type = ManagerTypeExtLights

      IF (.NOT. lAlphaFieldBlanks(2) ) THEN
        DemandMgr(MgrNum)%AvailSchedule = GetScheduleIndex(AlphArray(2))

        IF (DemandMgr(MgrNum)%AvailSchedule .EQ. 0) THEN
          CALL ShowSevereError(trim(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid '//  &
             trim(cAlphaFieldNames(2))//'="'//trim(AlphArray(2))//'" not found.')
          ErrorsFound = .TRUE.
        END IF
      ELSE
        DemandMgr(MgrNum)%AvailSchedule = ScheduleAlwaysOn
      END IF

      ! Validate Limiting Control
      SELECT CASE (AlphArray(3))
        CASE ('OFF')
          DemandMgr(MgrNum)%LimitControl = ManagerLimitOff

        CASE ('FIXED')
          DemandMgr(MgrNum)%LimitControl = ManagerLimitFixed

        CASE ('VARIABLE')
          DemandMgr(MgrNum)%LimitControl = ManagerLimitVariable

        CASE DEFAULT
          CALL ShowSevereError(trim(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid value'//  &
             trim(cAlphaFieldNames(3))//'="'//trim(AlphArray(3))//'".')
          CALL ShowContinueError('...value must be one of Off, Fixed, or Variable.')
          ErrorsFound = .TRUE.
      END SELECT

      DemandMgr(MgrNum)%LimitDuration = NumArray(1)

      DemandMgr(MgrNum)%LowerLimit = NumArray(2)

      ! Validate Selection Control
      SELECT CASE (AlphArray(4))
        CASE ('ALL')
          DemandMgr(MgrNum)%SelectionControl = ManagerSelectionAll

        CASE ('ROTATEONE')
          DemandMgr(MgrNum)%SelectionControl = ManagerSelectionOne

        CASE ('ROTATEMANY')
          DemandMgr(MgrNum)%SelectionControl = ManagerSelectionMany

        CASE DEFAULT
          CALL ShowSevereError(trim(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid value'//  &
             trim(cAlphaFieldNames(4))//'="'//trim(AlphArray(4))//'".')
          CALL ShowContinueError('...value must be one of All, RotateOne, or RotateMany.')
          ErrorsFound = .TRUE.
      END SELECT

      DemandMgr(MgrNum)%RotationDuration = NumArray(4)

      DemandMgr(MgrNum)%NumOfLoads = NumAlphas - 4

      IF (DemandMgr(MgrNum)%NumOfLoads > 0) THEN
        ALLOCATE(DemandMgr(MgrNum)%Load(DemandMgr(MgrNum)%NumOfLoads))

        DO LoadNum = 1, DemandMgr(MgrNum)%NumOfLoads
          LoadPtr = FindItemInList(AlphArray(LoadNum + 4),ExteriorLights%Name,NumExteriorLights)

          IF (LoadPtr > 0) THEN
            DemandMgr(MgrNum)%Load(LoadNum) = LoadPtr

          ELSE
            CALL ShowSevereError(trim(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid '//  &
               trim(cAlphaFieldNames(LoadNum+4))//'="'//trim(AlphArray(LoadNum+4))//'" not found.')
            ErrorsFound = .TRUE.

          END IF
        END DO  ! LoadNum
      END IF

    END DO ! MgrNum


    ! Get input for DemandManager:Lights
    StartIndex = EndIndex + 1
    EndIndex = EndIndex + NumDemandMgrLights

    CurrentModuleObject = 'DemandManager:Lights'

    DO MgrNum = StartIndex, EndIndex

      CALL GetObjectItem(CurrentModuleObject,MgrNum - StartIndex + 1,AlphArray,NumAlphas,NumArray,NumNums,IOSTAT, &
                    AlphaBlank=lAlphaFieldBlanks, AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

      IsNotOK = .FALSE.
      IsBlank = .FALSE.
      CALL VerifyName(AlphArray(1),DemandMgr%Name,MgrNum - StartIndex,IsNotOK,IsBlank,TRIM(CurrentModuleObject)//' Name')
      IF (IsNotOK) THEN
        ErrorsFound = .TRUE.
        IF (IsBlank) AlphArray(1) = 'xxxxx'
      END IF
      DemandMgr(MgrNum)%Name = AlphArray(1)

      DemandMgr(MgrNum)%Type = ManagerTypeLights

      IF (.not. lAlphaFieldBlanks(2)) THEN
        DemandMgr(MgrNum)%AvailSchedule = GetScheduleIndex(AlphArray(2))

        IF (DemandMgr(MgrNum)%AvailSchedule .EQ. 0) THEN
          CALL ShowSevereError(trim(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid '//  &
             trim(cAlphaFieldNames(2))//'="'//trim(AlphArray(2))//'" not found.')
          ErrorsFound = .TRUE.
        END IF
      ELSE
        DemandMgr(MgrNum)%AvailSchedule = ScheduleAlwaysOn
      END IF

      ! Validate Limiting Control
      SELECT CASE (AlphArray(3))
        CASE ('OFF')
          DemandMgr(MgrNum)%LimitControl = ManagerLimitOff

        CASE ('FIXED')
          DemandMgr(MgrNum)%LimitControl = ManagerLimitFixed

        CASE ('VARIABLE')
          DemandMgr(MgrNum)%LimitControl = ManagerLimitVariable

        CASE DEFAULT
          CALL ShowSevereError(trim(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid value'//  &
             trim(cAlphaFieldNames(3))//'="'//trim(AlphArray(3))//'".')
          CALL ShowContinueError('...value must be one of Off, Fixed, or Variable.')
          ErrorsFound = .TRUE.
      END SELECT

      DemandMgr(MgrNum)%LimitDuration = NumArray(1)

      DemandMgr(MgrNum)%LowerLimit = NumArray(2)

      ! Validate Selection Control
      SELECT CASE (AlphArray(4))
        CASE ('ALL')
          DemandMgr(MgrNum)%SelectionControl = ManagerSelectionAll

        CASE ('ROTATEONE')
          DemandMgr(MgrNum)%SelectionControl = ManagerSelectionOne

        CASE ('ROTATEMANY')
          DemandMgr(MgrNum)%SelectionControl = ManagerSelectionMany

        CASE DEFAULT
          CALL ShowSevereError(trim(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid value'//  &
             trim(cAlphaFieldNames(4))//'="'//trim(AlphArray(4))//'".')
          CALL ShowContinueError('...value must be one of All, RotateOne, or RotateMany.')
          ErrorsFound = .TRUE.
      END SELECT

      DemandMgr(MgrNum)%RotationDuration = NumArray(4)

      ! Count actual pointers to controlled zones
      DemandMgr(MgrNum)%NumOfLoads = 0
      DO LoadNum=1,NumAlphas-4
        LoadPtr = FindItemInList(AlphArray(LoadNum+4),LightsObjects%Name,NumLightsStatements)
        IF (LoadPtr > 0) THEN
          DemandMgr(MgrNum)%NumOfLoads=DemandMgr(MgrNum)%NumOfLoads + LightsObjects(LoadPtr)%NumOfZones
        ELSE
          LoadPtr = FindItemInList(AlphArray(LoadNum+4),Lights%Name,TotLights)
          IF (LoadPtr > 0) THEN
            DemandMgr(MgrNum)%NumOfLoads=DemandMgr(MgrNum)%NumOfLoads + 1
          ELSE
            CALL ShowSevereError(trim(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid '//  &
               trim(cAlphaFieldNames(LoadNum+4))//'="'//trim(AlphArray(LoadNum+4))//'" not found.')
            ErrorsFound = .TRUE.
          ENDIF
        ENDIF
      ENDDO

!      DemandMgr(MgrNum)%NumOfLoads = NumAlphas - 4

      IF (DemandMgr(MgrNum)%NumOfLoads > 0) THEN
        ALLOCATE(DemandMgr(MgrNum)%Load(DemandMgr(MgrNum)%NumOfLoads))
        LoadNum=0
        DO Item = 1, NumAlphas-4
          LoadPtr = FindItemInList(AlphArray(Item+4),LightsObjects%Name,NumLightsStatements)
          IF (LoadPtr > 0) THEN
            DO Item1=1,LightsObjects(LoadPtr)%NumOfZones
              LoadNum=LoadNum+1
              DemandMgr(MgrNum)%Load(LoadNum) = LightsObjects(LoadPtr)%StartPtr+Item1-1
            ENDDO
          ELSE
            LoadPtr = FindItemInList(AlphArray(Item+4),Lights%Name,TotLights)
            IF (LoadPtr > 0) THEN
              LoadNum=LoadNum+1
              DemandMgr(MgrNum)%Load(LoadNum) = LoadPtr
            ENDIF
          END IF
        END DO  ! LoadNum
      END IF

    END DO ! MgrNum


    ! Get input for DemandManager:ElectricEquipment
    StartIndex = EndIndex + 1
    EndIndex = EndIndex + NumDemandMgrElecEquip

    CurrentModuleObject = 'DemandManager:ElectricEquipment'

    DO MgrNum = StartIndex, EndIndex

      CALL GetObjectItem(CurrentModuleObject, &
        MgrNum - StartIndex + 1,AlphArray,NumAlphas,NumArray,NumNums,IOSTAT, &
                    AlphaBlank=lAlphaFieldBlanks, AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

      IsNotOK = .FALSE.
      IsBlank = .FALSE.
      CALL VerifyName(AlphArray(1),DemandMgr%Name, &
        MgrNum - StartIndex,IsNotOK,IsBlank,TRIM(CurrentModuleObject)//' Name')
      IF (IsNotOK) THEN
        ErrorsFound = .TRUE.
        IF (IsBlank) AlphArray(1) = 'xxxxx'
      END IF
      DemandMgr(MgrNum)%Name = AlphArray(1)

      DemandMgr(MgrNum)%Type = ManagerTypeElecEquip

      IF (.not. lAlphaFieldBlanks(2)) THEN
        DemandMgr(MgrNum)%AvailSchedule = GetScheduleIndex(AlphArray(2))

        IF (DemandMgr(MgrNum)%AvailSchedule .EQ. 0) THEN
          CALL ShowSevereError(trim(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid '//  &
             trim(cAlphaFieldNames(2))//'="'//trim(AlphArray(2))//'" not found.')
          ErrorsFound = .TRUE.
        END IF
      ELSE
        DemandMgr(MgrNum)%AvailSchedule = ScheduleAlwaysOn
      END IF

      ! Validate Limiting Control
      SELECT CASE (AlphArray(3))
        CASE ('OFF')
          DemandMgr(MgrNum)%LimitControl = ManagerLimitOff

        CASE ('FIXED')
          DemandMgr(MgrNum)%LimitControl = ManagerLimitFixed

        CASE ('VARIABLE')
          DemandMgr(MgrNum)%LimitControl = ManagerLimitVariable

        CASE DEFAULT
          CALL ShowSevereError(trim(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid value'//  &
             trim(cAlphaFieldNames(3))//'="'//trim(AlphArray(3))//'".')
          CALL ShowContinueError('...value must be one of Off, Fixed, or Variable.')
          ErrorsFound = .TRUE.
      END SELECT

      DemandMgr(MgrNum)%LimitDuration = NumArray(1)

      DemandMgr(MgrNum)%LowerLimit = NumArray(2)

      ! Validate Selection Control
      SELECT CASE (AlphArray(4))
        CASE ('ALL')
          DemandMgr(MgrNum)%SelectionControl = ManagerSelectionAll

        CASE ('ROTATEONE')
          DemandMgr(MgrNum)%SelectionControl = ManagerSelectionOne

        CASE ('ROTATEMANY')
          DemandMgr(MgrNum)%SelectionControl = ManagerSelectionMany

        CASE DEFAULT
          CALL ShowSevereError(trim(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid value'//  &
             trim(cAlphaFieldNames(4))//'="'//trim(AlphArray(4))//'".')
          CALL ShowContinueError('...value must be one of All, RotateOne, or RotateMany.')
          ErrorsFound = .TRUE.
      END SELECT

      DemandMgr(MgrNum)%RotationDuration = NumArray(4)

      ! Count actual pointers to controlled zones
      DemandMgr(MgrNum)%NumOfLoads = 0
      DO LoadNum=1,NumAlphas-4
        LoadPtr = FindItemInList(AlphArray(LoadNum+4),ZoneElectricObjects%Name,NumZoneElectricStatements)
        IF (LoadPtr > 0) THEN
          DemandMgr(MgrNum)%NumOfLoads=DemandMgr(MgrNum)%NumOfLoads + ZoneElectricObjects(LoadPtr)%NumOfZones
        ELSE
          LoadPtr = FindItemInList(AlphArray(LoadNum+4),ZoneElectric%Name,TotElecEquip)
          IF (LoadPtr > 0) THEN
            DemandMgr(MgrNum)%NumOfLoads=DemandMgr(MgrNum)%NumOfLoads + 1
          ELSE
            CALL ShowSevereError(trim(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid '//  &
               trim(cAlphaFieldNames(LoadNum+4))//'="'//trim(AlphArray(LoadNum+4))//'" not found.')
            ErrorsFound = .TRUE.
          END IF
        ENDIF
      ENDDO

!      DemandMgr(MgrNum)%NumOfLoads = NumAlphas - 4

      IF (DemandMgr(MgrNum)%NumOfLoads > 0) THEN
        ALLOCATE(DemandMgr(MgrNum)%Load(DemandMgr(MgrNum)%NumOfLoads))
        LoadNum=0
        DO Item = 1, NumAlphas-4
          LoadPtr = FindItemInList(AlphArray(Item+4),ZoneElectricObjects%Name,NumZoneElectricStatements)
          IF (LoadPtr > 0) THEN
            DO Item1=1,ZoneElectricObjects(LoadPtr)%NumOfZones
              LoadNum=LoadNum+1
              DemandMgr(MgrNum)%Load(LoadNum) = ZoneElectricObjects(LoadPtr)%StartPtr+Item1-1
            ENDDO
          ELSE
            LoadPtr = FindItemInList(AlphArray(Item+4),ZoneElectric%Name,TotElecEquip)
            IF (LoadPtr > 0) THEN
              LoadNum=LoadNum+1
              DemandMgr(MgrNum)%Load(LoadNum) = LoadPtr
            ENDIF
          END IF
        END DO  ! LoadNum
      END IF

    END DO ! MgrNum


    ! Get input for DemandManager:Thermostats
    StartIndex = EndIndex + 1
    EndIndex = EndIndex + NumDemandMgrThermostats

    CurrentModuleObject = 'DemandManager:Thermostats'

    DO MgrNum = StartIndex, EndIndex

      CALL GetObjectItem(CurrentModuleObject, &
        MgrNum - StartIndex + 1,AlphArray,NumAlphas,NumArray,NumNums,IOSTAT, &
        AlphaBlank=lAlphaFieldBlanks, AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

      IsNotOK = .FALSE.
      IsBlank = .FALSE.
      CALL VerifyName(AlphArray(1),DemandMgr%Name, &
        MgrNum - StartIndex,IsNotOK,IsBlank,TRIM(CurrentModuleObject)//' Name')
      IF (IsNotOK) THEN
        ErrorsFound = .TRUE.
        IF (IsBlank) AlphArray(1) = 'xxxxx'
      END IF
      DemandMgr(MgrNum)%Name = AlphArray(1)

      DemandMgr(MgrNum)%Type = ManagerTypeThermostats

      IF (.not. lAlphaFieldBlanks(2)) THEN
        DemandMgr(MgrNum)%AvailSchedule = GetScheduleIndex(AlphArray(2))

        IF (DemandMgr(MgrNum)%AvailSchedule .EQ. 0) THEN
          CALL ShowSevereError(trim(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid '//  &
             trim(cAlphaFieldNames(2))//'="'//trim(AlphArray(2))//'" not found.')
          ErrorsFound = .TRUE.
        END IF
      ELSE
        DemandMgr(MgrNum)%AvailSchedule = ScheduleAlwaysOn
      END IF

      ! Validate Limiting Control
      SELECT CASE (AlphArray(3))
        CASE ('OFF')
          DemandMgr(MgrNum)%LimitControl = ManagerLimitOff

        CASE ('FIXED')
          DemandMgr(MgrNum)%LimitControl = ManagerLimitFixed

        CASE ('VARIABLE')
          DemandMgr(MgrNum)%LimitControl = ManagerLimitVariable

        CASE DEFAULT
          CALL ShowSevereError(trim(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid value'//  &
             trim(cAlphaFieldNames(3))//'="'//trim(AlphArray(3))//'".')
          CALL ShowContinueError('...value must be one of Off, Fixed, or Variable.')
          ErrorsFound = .TRUE.
      END SELECT

      DemandMgr(MgrNum)%LimitDuration = NumArray(1)

      DemandMgr(MgrNum)%LowerLimit = NumArray(2)
      DemandMgr(MgrNum)%UpperLimit = NumArray(3)

      IF (DemandMgr(MgrNum)%LowerLimit > DemandMgr(MgrNum)%UpperLimit) THEN
        CALL ShowSevereError('Invalid input for '//TRIM(CurrentModuleObject)//' = '//TRIM(AlphArray(1)))
        CALL ShowContinueError(TRIM(cNumericFieldNames(2))//' ['//TRIM(RoundSigDigits(NumArray(2),2))//'] > '//  &
                              TRIM(cNumericFieldNames(3))//' ['//TRIM(RoundSigDigits(NumArray(3),2))//']')
        CALL ShowContinueError(TRIM(cNumericFieldNames(2))//' cannot be greater than '//TRIM(cNumericFieldNames(3)) )
        ErrorsFound = .TRUE.
      END IF

      ! Validate Selection Control
      SELECT CASE (AlphArray(4))
        CASE ('ALL')
          DemandMgr(MgrNum)%SelectionControl = ManagerSelectionAll

        CASE ('ROTATEONE')
          DemandMgr(MgrNum)%SelectionControl = ManagerSelectionOne

        CASE ('ROTATEMANY')
          DemandMgr(MgrNum)%SelectionControl = ManagerSelectionMany

        CASE DEFAULT
          CALL ShowSevereError(trim(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid value'//  &
             trim(cAlphaFieldNames(4))//'="'//trim(AlphArray(4))//'".')
          CALL ShowContinueError('...value must be one of All, RotateOne, or RotateMany.')
          ErrorsFound = .TRUE.
      END SELECT

      DemandMgr(MgrNum)%RotationDuration = NumArray(5)

      ! Count actual pointers to controlled zones
      DemandMgr(MgrNum)%NumOfLoads = 0
      DO LoadNum=1,NumAlphas-4
        LoadPtr = FindItemInList(AlphArray(LoadNum+4),TStatObjects%Name,NumTStatStatements)
        IF (LoadPtr > 0) THEN
          DemandMgr(MgrNum)%NumOfLoads=DemandMgr(MgrNum)%NumOfLoads + TStatObjects(LoadPtr)%NumOfZones
        ELSE
          LoadPtr = FindItemInList(AlphArray(LoadNum+4),TempControlledZone%Name,NumTempControlledZones)
          IF (LoadPtr > 0) THEN
            DemandMgr(MgrNum)%NumOfLoads=DemandMgr(MgrNum)%NumOfLoads + 1
          ELSE
            CALL ShowSevereError(trim(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid '//  &
               trim(cAlphaFieldNames(LoadNum+4))//'="'//trim(AlphArray(LoadNum+4))//'" not found.')
            ErrorsFound = .TRUE.
          ENDIF
        ENDIF
      ENDDO

      IF (DemandMgr(MgrNum)%NumOfLoads > 0) THEN
        ALLOCATE(DemandMgr(MgrNum)%Load(DemandMgr(MgrNum)%NumOfLoads))
        LoadNum=0
        DO Item = 1, NumAlphas-4
          LoadPtr = FindItemInList(AlphArray(Item+4),TStatObjects%Name,NumTStatStatements)
          IF (LoadPtr > 0) THEN
            DO Item1=1,TStatObjects(LoadPtr)%NumOfZones
              LoadNum=LoadNum+1
              DemandMgr(MgrNum)%Load(LoadNum) = TStatObjects(LoadPtr)%TempControlledZoneStartPtr+Item1-1
            ENDDO
          ELSE
            LoadPtr = FindItemInList(AlphArray(Item+4),TempControlledZone%Name,NumTempControlledZones)
            IF (LoadPtr > 0) THEN
              LoadNum=LoadNum+1
              DemandMgr(MgrNum)%Load(LoadNum) = LoadPtr
            ENDIF
          END IF
        END DO  ! LoadNum
      END IF

    END DO ! MgrNum

    DEALLOCATE(AlphArray)
    DEALLOCATE(NumArray)

  END IF

  IF (ErrorsFound) THEN
    CALL ShowFatalError('Errors found in processing input for demand managers. Preceding condition causes termination.')
  END IF

  RETURN

END SUBROUTINE GetDemandManagerInput


SUBROUTINE SurveyDemandManagers

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Peter Graham Ellis
          !       DATE WRITTEN   July 2005
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Checks to see if any demand managers can reduce the load

          ! METHODOLOGY EMPLOYED:
          !

          ! USE STATEMENTS:


  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: MgrNum, LoadNum, LoadPtr
  LOGICAL :: CanReduceDemand

          ! FLOW:
  DO MgrNum = 1, NumDemandMgr

    DemandMgr(MgrNum)%CanReduceDemand = .FALSE.

    IF (.NOT. DemandMgr(MgrNum)%Available) CYCLE
    IF (DemandMgr(MgrNum)%LimitControl == ManagerLimitOff) CYCLE

    IF (DemandMgr(MgrNum)%Active) CYCLE  ! This works for FIXED control action, but not VARIABLE
    ! VARIABLE control could actually reduce demand farther, even if active already

    DO LoadNum = 1, DemandMgr(MgrNum)%NumOfLoads
      LoadPtr = DemandMgr(MgrNum)%Load(LoadNum)

      ! Check if this load can reduce demand
      ! Assume FIXED control action for now, needs more sophisticated check for VARIABLE control
      CALL LoadInterface(CheckCanReduce, MgrNum, LoadPtr, CanReduceDemand)

      IF (CanReduceDemand) THEN
        DemandMgr(MgrNum)%CanReduceDemand = .TRUE.
        EXIT  ! If any one load can reduce demand, then the whole demand manager can reduce demand
      END IF

    END DO  ! LoadNum

  END DO  ! MgrNum

  RETURN

END SUBROUTINE SurveyDemandManagers


SUBROUTINE ActivateDemandManagers

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Peter Graham Ellis
          !       DATE WRITTEN   July 2005
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !

          ! METHODOLOGY EMPLOYED:
          !

          ! USE STATEMENTS:


  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: MgrNum
  INTEGER :: LoadNum
  INTEGER :: LoadPtr
  INTEGER :: RotatedLoadNum
  LOGICAL :: CanReduceDemand

          ! FLOW:
  DO MgrNum = 1, NumDemandMgr

    IF (DemandMgr(MgrNum)%Activate) THEN
      DemandMgr(MgrNum)%Activate = .FALSE.
      DemandMgr(MgrNum)%Active = .TRUE.

      SELECT CASE (DemandMgr(MgrNum)%SelectionControl)

        CASE (ManagerSelectionAll)
          ! Turn ON limiting on all loads
          DO LoadNum = 1, DemandMgr(MgrNum)%NumOfLoads
            LoadPtr = DemandMgr(MgrNum)%Load(LoadNum)
            CALL LoadInterface(SetLimit, MgrNum, LoadPtr, CanReduceDemand)
          END DO  ! LoadNum

        CASE (ManagerSelectionMany)  ! All loads are limited except for one
          IF (DemandMgr(MgrNum)%NumOfLoads > 1) THEN

            ! Turn ON limiting on all loads
            DO LoadNum = 1, DemandMgr(MgrNum)%NumOfLoads
              LoadPtr = DemandMgr(MgrNum)%Load(LoadNum)
              CALL LoadInterface(SetLimit, MgrNum, LoadPtr, CanReduceDemand)
            END DO  ! LoadNum

            ! Set next rotated load (from last time it was active)
            RotatedLoadNum = DemandMgr(MgrNum)%RotatedLoadNum
            RotatedLoadNum = RotatedLoadNum + 1
            IF (RotatedLoadNum > DemandMgr(MgrNum)%NumOfLoads) RotatedLoadNum = 1
            DemandMgr(MgrNum)%RotatedLoadNum = RotatedLoadNum

            ! Turn OFF limiting for the new rotated load
            LoadPtr = DemandMgr(MgrNum)%Load(RotatedLoadNum)
            CALL LoadInterface(ClearLimit, MgrNum, LoadPtr, CanReduceDemand)
          ELSE
            ! Turn ON limiting for the one and only load
            LoadPtr = DemandMgr(MgrNum)%Load(1)
            CALL LoadInterface(SetLimit, MgrNum, LoadPtr, CanReduceDemand)
          END IF

        CASE (ManagerSelectionOne)  ! Only one load is limited
          IF (DemandMgr(MgrNum)%NumOfLoads > 1) THEN
            ! Turn OFF limiting on all loads
            DO LoadNum = 1, DemandMgr(MgrNum)%NumOfLoads
              LoadPtr = DemandMgr(MgrNum)%Load(LoadNum)
              CALL LoadInterface(ClearLimit, MgrNum, LoadPtr, CanReduceDemand)
            END DO  ! LoadNum

            ! Set next rotated load (from last time it was active)
            RotatedLoadNum = DemandMgr(MgrNum)%RotatedLoadNum
            RotatedLoadNum = RotatedLoadNum + 1
            IF (RotatedLoadNum > DemandMgr(MgrNum)%NumOfLoads) RotatedLoadNum = 1
            DemandMgr(MgrNum)%RotatedLoadNum = RotatedLoadNum

            ! Turn ON limiting for the new rotated load
            LoadPtr = DemandMgr(MgrNum)%Load(RotatedLoadNum)
            CALL LoadInterface(SetLimit, MgrNum, LoadPtr, CanReduceDemand)
          ELSE
            ! Turn ON limiting for the one and only load
            LoadPtr = DemandMgr(MgrNum)%Load(1)
            CALL LoadInterface(SetLimit, MgrNum, LoadPtr, CanReduceDemand)
          END IF

      END SELECT

    END IF

  END DO  ! MgrNum

  RETURN

END SUBROUTINE ActivateDemandManagers


SUBROUTINE UpdateDemandManagers

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Peter Graham Ellis
          !       DATE WRITTEN   July 2005
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Expires limits and rotates loads after specified time duration.
  ! It updates availability flags, expires managers that ended in the last timestep, etc.

          ! METHODOLOGY EMPLOYED:
          !

          ! USE STATEMENTS:
  USE DataGlobals, ONLY: MinutesPerTimeStep
  USE ExteriorEnergyUse, ONLY: ExteriorLights
  USE DataHeatBalance, ONLY: Lights, ZoneElectric
  USE DataZoneControls, ONLY: TempControlledZone
  USE ScheduleManager, ONLY: GetCurrentScheduleValue

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: MgrNum, LoadNum, LoadPtr
  LOGICAL :: Available
  LOGICAL :: CanReduceDemand
  INTEGER :: RotatedLoadNum

          ! FLOW:
  DO MgrNum = 1, NumDemandMgr

    ! Check availability
!    IF (DemandMgr(MgrNum)%AvailSchedule .EQ. 0) THEN
!      Available = .TRUE.  ! No schedule defaults to available
!    ELSE
     IF (GetCurrentScheduleValue(DemandMgr(MgrNum)%AvailSchedule) > 0.0d0) THEN
       Available = .TRUE.
     ELSE
       Available = .FALSE.
     END IF
!    END IF

    DemandMgr(MgrNum)%Available = Available

    ! Update demand manager status
    IF (Available) THEN

      IF (DemandMgr(MgrNum)%Active) THEN

        DemandMgr(MgrNum)%ElapsedTime = DemandMgr(MgrNum)%ElapsedTime + MinutesPerTimeStep

        ! Check for expiring limit duration
        IF (DemandMgr(MgrNum)%ElapsedTime >= DemandMgr(MgrNum)%LimitDuration) THEN
          DemandMgr(MgrNum)%ElapsedTime = 0
          DemandMgr(MgrNum)%ElapsedRotationTime = 0
          DemandMgr(MgrNum)%Active = .FALSE.

          ! Demand Manager is not available, remove demand limits from all loads
          DO LoadNum = 1, DemandMgr(MgrNum)%NumOfLoads
            LoadPtr = DemandMgr(MgrNum)%Load(LoadNum)
            CALL LoadInterface(ClearLimit, MgrNum, LoadPtr, CanReduceDemand)
          END DO  ! LoadNum

        ELSE

          SELECT CASE (DemandMgr(MgrNum)%SelectionControl)
            CASE (ManagerSelectionAll)
              ! Do nothing; limits remain on all loads

            CASE (ManagerSelectionMany)  ! All loads are limited except for one
              DemandMgr(MgrNum)%ElapsedRotationTime = DemandMgr(MgrNum)%ElapsedRotationTime + MinutesPerTimeStep

              IF (DemandMgr(MgrNum)%ElapsedRotationTime >= DemandMgr(MgrNum)%RotationDuration) THEN
                DemandMgr(MgrNum)%ElapsedRotationTime = 0

                IF (DemandMgr(MgrNum)%NumOfLoads > 1) THEN
                  ! Turn ON limiting for the old rotated load
                  RotatedLoadNum = DemandMgr(MgrNum)%RotatedLoadNum
                  LoadPtr = DemandMgr(MgrNum)%Load(RotatedLoadNum)
                  CALL LoadInterface(SetLimit, MgrNum, LoadPtr, CanReduceDemand)

                  ! Set next rotated load
                  RotatedLoadNum = RotatedLoadNum + 1
                  IF (RotatedLoadNum > DemandMgr(MgrNum)%NumOfLoads) RotatedLoadNum = 1
                  DemandMgr(MgrNum)%RotatedLoadNum = RotatedLoadNum

                  ! Turn OFF limiting for the new rotated load
                  LoadPtr = DemandMgr(MgrNum)%Load(RotatedLoadNum)
                  CALL LoadInterface(ClearLimit, MgrNum, LoadPtr, CanReduceDemand)
                END IF
              END IF

            CASE (ManagerSelectionOne)  ! Only one load is limited
              DemandMgr(MgrNum)%ElapsedRotationTime = DemandMgr(MgrNum)%ElapsedRotationTime + MinutesPerTimeStep

              IF (DemandMgr(MgrNum)%ElapsedRotationTime >= DemandMgr(MgrNum)%RotationDuration) THEN
                DemandMgr(MgrNum)%ElapsedRotationTime = 0

                IF (DemandMgr(MgrNum)%NumOfLoads > 1) THEN
                  ! Turn OFF limiting for the old rotated load
                  RotatedLoadNum = DemandMgr(MgrNum)%RotatedLoadNum
                  LoadPtr = DemandMgr(MgrNum)%Load(RotatedLoadNum)
                  CALL LoadInterface(ClearLimit, MgrNum, LoadPtr, CanReduceDemand)

                  ! Set next rotated load
                  RotatedLoadNum = RotatedLoadNum + 1
                  IF (RotatedLoadNum > DemandMgr(MgrNum)%NumOfLoads) RotatedLoadNum = 1
                  DemandMgr(MgrNum)%RotatedLoadNum = RotatedLoadNum

                  ! Turn ON limiting for the new rotated load
                  LoadPtr = DemandMgr(MgrNum)%Load(RotatedLoadNum)
                  CALL LoadInterface(SetLimit, MgrNum, LoadPtr, CanReduceDemand)
                END IF
              END IF

          END SELECT


        END IF
      END IF



    ELSE  ! Demand Manager is not available
      DemandMgr(MgrNum)%Active = .FALSE.

      ! Demand Manager is not available, remove demand limits from all loads
      DO LoadNum = 1, DemandMgr(MgrNum)%NumOfLoads
        LoadPtr = DemandMgr(MgrNum)%Load(LoadNum)
        CALL LoadInterface(ClearLimit, MgrNum, LoadPtr, CanReduceDemand)
      END DO  ! LoadNum

    END IF

  END DO  ! MgrNum

  RETURN

END SUBROUTINE UpdateDemandManagers

SUBROUTINE ReportDemandManagerList(ListNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Peter Graham Ellis
          !       DATE WRITTEN   July 2005
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Calculates report variables.

          ! METHODOLOGY EMPLOYED:
          ! Standard EnergyPlus methodology.

          ! USE STATEMENTS:
  USE DataGlobals, ONLY: MinutesPerTimeStep
  USE DataEnvironment, ONLY: Month
  USE ScheduleManager, ONLY: GetCurrentScheduleValue

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: ListNum

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64)           :: BillingPeriod
  INTEGER             :: Item
  INTEGER             :: AveragingWindow
  LOGICAL             :: OnPeak
  REAL(r64)           :: OverLimit

          ! FLOW:
  IF (DemandManagerList(ListNum)%BillingSchedule .EQ. 0) THEN
    BillingPeriod = Month
  ELSE
    BillingPeriod = GetCurrentScheduleValue(DemandManagerList(ListNum)%BillingSchedule)
  END IF

  IF (DemandManagerList(ListNum)%BillingPeriod .NE. BillingPeriod) THEN
    ! Reset variables for new billing period
    !DemandManagerList(ListNum)%History = 0.0        ! Don't reset--continue from previous billing period
    !DemandManagerList(ListNum)%AverageDemand = 0.0  ! Don't reset--continue from previous billing period
    DemandManagerList(ListNum)%PeakDemand = 0.0d0
    DemandManagerList(ListNum)%OverLimitDuration = 0.0d0

    DemandManagerList(ListNum)%BillingPeriod = BillingPeriod
  END IF

  ! Add new timestep to demand history and subtract oldest timestep
  AveragingWindow = DemandManagerList(ListNum)%AveragingWindow
  DemandManagerList(ListNum)%AverageDemand = DemandManagerList(ListNum)%AverageDemand &
    + (DemandManagerList(ListNum)%MeterDemand - DemandManagerList(ListNum)%History(1)) / AveragingWindow

  ! Update demand history
  DO Item = 1, AveragingWindow - 1
    DemandManagerList(ListNum)%History(Item) = DemandManagerList(ListNum)%History(Item + 1)
  END DO
  DemandManagerList(ListNum)%History(AveragingWindow) = DemandManagerList(ListNum)%MeterDemand

  IF (DemandManagerList(ListNum)%PeakSchedule .EQ. 0) THEN
    OnPeak = .TRUE.
  ELSE
    IF (GetCurrentScheduleValue(DemandManagerList(ListNum)%PeakSchedule) .EQ. 1) THEN
      OnPeak = .TRUE.
    ELSE
      OnPeak = .FALSE.
    END IF
  END IF

  IF (OnPeak) THEN
    DemandManagerList(ListNum)%PeakDemand = &
      MAX(DemandManagerList(ListNum)%AverageDemand,DemandManagerList(ListNum)%PeakDemand)

    OverLimit = DemandManagerList(ListNum)%AverageDemand - DemandManagerList(ListNum)%ScheduledLimit
    IF (OverLimit > 0.0d0) THEN
      DemandManagerList(ListNum)%OverLimit = OverLimit
      DemandManagerList(ListNum)%OverLimitDuration = DemandManagerList(ListNum)%OverLimitDuration + (MinutesPerTimeStep / 60.0d0)
    ELSE
      DemandManagerList(ListNum)%OverLimit = 0.0d0
    END IF

  ELSE
    DemandManagerList(ListNum)%OverLimit = 0.0d0
  END IF

  RETURN

END SUBROUTINE ReportDemandManagerList

SUBROUTINE LoadInterface(Action, MgrNum, LoadPtr, CanReduceDemand)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Peter Graham Ellis
          !       DATE WRITTEN   August 2005
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Provides a universal interface to handle all communication with the various load objects.
          ! Demand managers for new types of loads can be easily added with a new CASE statement in this subroutine
          ! and new GetInput code.

          ! METHODOLOGY EMPLOYED:
          !

          ! USE STATEMENTS:
  USE ExteriorEnergyUse, ONLY: ExteriorLights
  USE DataHeatBalance, ONLY: Lights, ZoneElectric
  USE DataZoneControls, ONLY: TempControlledZone,ComfortControlledZone,NumComfortControlledZones
  USE DataHeatBalFanSys, ONLY: ZoneThermostatSetPointHi, ZoneThermostatSetPointLo,ComfortControlType

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)           :: Action
  INTEGER, INTENT(IN)           :: MgrNum
  INTEGER, INTENT(IN)           :: LoadPtr
  LOGICAL, INTENT(OUT)          :: CanReduceDemand

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64)                     :: LowestPower

          ! FLOW:
  CanReduceDemand = .FALSE.

  SELECT CASE (DemandMgr(MgrNum)%Type)

    CASE (ManagerTypeExtLights)
      LowestPower = ExteriorLights(LoadPtr)%DesignLevel * DemandMgr(MgrNum)%LowerLimit
      IF (Action == CheckCanReduce) THEN
        IF (ExteriorLights(LoadPtr)%Power > LowestPower) CanReduceDemand = .TRUE.
      ELSE IF (Action == SetLimit) THEN
        ExteriorLights(LoadPtr)%ManageDemand = .TRUE.
        ExteriorLights(LoadPtr)%DemandLimit = LowestPower
      ELSE IF (Action == ClearLimit) THEN
        ExteriorLights(LoadPtr)%ManageDemand = .FALSE.
      END IF

    CASE (ManagerTypeLights)
      LowestPower = Lights(LoadPtr)%DesignLevel * DemandMgr(MgrNum)%LowerLimit
      IF (Action == CheckCanReduce) THEN
        IF (Lights(LoadPtr)%Power > LowestPower) CanReduceDemand = .TRUE.
      ELSE IF (Action == SetLimit) THEN
        Lights(LoadPtr)%ManageDemand = .TRUE.
        Lights(LoadPtr)%DemandLimit = LowestPower
      ELSE IF (Action == ClearLimit) THEN
        Lights(LoadPtr)%ManageDemand = .FALSE.
      END IF

    CASE (ManagerTypeElecEquip)
      LowestPower = ZoneElectric(LoadPtr)%DesignLevel * DemandMgr(MgrNum)%LowerLimit
      IF (Action == CheckCanReduce) THEN
        IF (ZoneElectric(LoadPtr)%Power > LowestPower) CanReduceDemand = .TRUE.
      ELSE IF (Action == SetLimit) THEN
        ZoneElectric(LoadPtr)%ManageDemand = .TRUE.
        ZoneElectric(LoadPtr)%DemandLimit = LowestPower
      ELSE IF (Action == ClearLimit) THEN
        ZoneElectric(LoadPtr)%ManageDemand = .FALSE.
      END IF

    CASE (ManagerTypeThermostats)
      IF (Action == CheckCanReduce) THEN
        IF (ZoneThermostatSetPointLo(TempControlledZone(LoadPtr)%ActualZoneNum) > DemandMgr(MgrNum)%LowerLimit &  ! Heating
          .OR. ZoneThermostatSetPointHi(TempControlledZone(LoadPtr)%ActualZoneNum) < DemandMgr(MgrNum)%UpperLimit) &  ! Cooling
          CanReduceDemand = .TRUE.
      ELSE IF (Action == SetLimit) THEN
        TempControlledZone(LoadPtr)%ManageDemand = .TRUE.
        TempControlledZone(LoadPtr)%HeatingResetLimit = DemandMgr(MgrNum)%LowerLimit
        TempControlledZone(LoadPtr)%CoolingResetLimit = DemandMgr(MgrNum)%UpperLimit
      ELSE IF (Action == ClearLimit) THEN
        TempControlledZone(LoadPtr)%ManageDemand = .FALSE.
      END IF
      IF (NumComfortControlledZones > 0) THEN
        IF (ComfortControlType(TempControlledZone(LoadPtr)%ActualZoneNum)>0) then
          IF (Action == CheckCanReduce) THEN
            IF (ZoneThermostatSetPointLo(ComfortControlledZone(LoadPtr)%ActualZoneNum) > DemandMgr(MgrNum)%LowerLimit &  !Heating
             .OR. ZoneThermostatSetPointHi(ComfortControlledZone(LoadPtr)%ActualZoneNum) < DemandMgr(MgrNum)%UpperLimit) &
              CanReduceDemand = .TRUE.
          ELSE IF (Action == SetLimit) THEN
            ComfortControlledZone(LoadPtr)%ManageDemand = .TRUE.
            ComfortControlledZone(LoadPtr)%HeatingResetLimit = DemandMgr(MgrNum)%LowerLimit
            ComfortControlledZone(LoadPtr)%CoolingResetLimit = DemandMgr(MgrNum)%UpperLimit
          ELSE IF (Action == ClearLimit) THEN
            ComfortControlledZone(LoadPtr)%ManageDemand = .FALSE.
          END IF
        END IF
      END IF

  END SELECT

  RETURN

END SUBROUTINE LoadInterface

SUBROUTINE InitDemandManagers

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   September 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Provide external call to get Demand manager input after
          ! appropriate initializations.

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

  IF (GetInput) THEN
    CALL GetDemandManagerInput
    CALL GetDemandManagerListInput
    GetInput = .FALSE.
  END IF

  RETURN

END SUBROUTINE InitDemandManagers

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

END MODULE DemandManager