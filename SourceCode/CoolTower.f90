! (ref: Object: COOLTOWER:SHOWER)
MODULE CoolTower
  ! Module containing the data for cooltower system

  ! MODULE INFORMATION:
  !       AUTHOR         Daeho Kang
  !       DATE WRITTEN   Aug 2008
  !       MODIFIED       na
  !       RE-ENGINEERED  na

  ! PURPOSE OF THIS MODULE:
  ! To encapsulate the data and algorithyms required to manage the cooltower component.

  ! METHODOLOGY EMPLOYED:
  ! na

  ! REFERENCES:
  ! Baruch Givoni. 1994. Passive and Low Energy Cooling of Buildings. Chapter 5: Evaporative Cooling Systems.
  !     John Wiley & Sons, Inc.
  !
  ! OTHER NOTES: none

  ! USE STATEMENTS:
USE DataPrecisionGlobals
USE DataGlobals
USE DataHeatBalance
USE DataInterfaces

          ! Use statements for access to subroutines in other modules

IMPLICIT NONE

  ! MODULE PARAMETER DEFINITIONS
INTEGER, PARAMETER :: WaterSupplyFromMains = 101
INTEGER, PARAMETER :: WaterSupplyFromTank  = 102
INTEGER, PARAMETER :: WaterFlowSchedule = 0
INTEGER, PARAMETER :: WindDrivenFlow    = 1

! DERIVED TYPE DEFINITIONS
TYPE CoolTowerParams
    CHARACTER(len=MaxNameLength) :: Name       =' '    ! The component name
    CHARACTER(len=MaxNameLength) :: CompType   =' '    ! Type of component
    CHARACTER(len=MaxNameLength) :: Schedule   =' '    ! Available schedule
    CHARACTER(len=MaxNameLength) :: ZoneName   =' '    ! Name of zone the component is serving
    CHARACTER(len=MaxNameLength) :: PumpSchedName =' ' ! Available schedule of the water pump
    INTEGER :: SchedPtr                  = 0 ! Index to schedule
    INTEGER :: ZonePtr                   = 0 ! Point to this zone
    INTEGER :: PumpSchedPtr              = 0 ! Index to schedule for water pump
    INTEGER :: FlowCtrlType              = 0 ! Type of cooltower operation
    INTEGER :: CoolTWaterSupplyMode      = WaterSupplyFromMains ! Type of water source
    CHARACTER(len=MaxNameLength) :: CoolTWaterSupplyName = ' '  ! Name of water source
    INTEGER :: CoolTWaterSupTankID       = 0 ! Index to water storage tank
    INTEGER :: CoolTWaterTankDemandARRID = 0 ! Index to water storage demand
    REAL(r64) :: TowerHeight             = 0.0d0 ! Effective cooltower height in m
    REAL(r64) :: OutletArea              = 0.0d0 ! Outlet area where conditioned air comes in m2
    REAL(r64) :: OutletVelocity          = 0.0d0 ! Outlet velocity of the cooltower in m/s
    REAL(r64) :: MaxAirVolFlowRate       = 0.0d0 ! Maximum allowable airflow in m3/s
    REAL(r64) :: AirMassFlowRate         = 0.0d0 ! Air mass flow rate in kg/s
    REAL(r64) :: CoolTAirMass            = 0.0d0 ! Air mass in kg
    REAL(r64) :: MinZoneTemp             = 0.0d0 ! Lower temperature limit to prevent over cooling in C
    REAL(r64) :: FracWaterLoss           = 0.0d0 ! Fraction of estimated blowdown and drift water
    REAL(r64) :: FracFlowSched           = 0.0d0 ! Fraction of airflow loss
    REAL(r64) :: MaxWaterFlowRate        = 0.0d0 ! Maximum limit of water flow rate in m3/s
    REAL(r64) :: ActualWaterFlowRate     = 0.0d0 ! Actual water mass flow rate in m3/s
    REAL(r64) :: RatedPumpPower          = 0.0d0 ! Rated power consumption for water pump serving the cooltower in watts
    REAL(r64) :: SenHeatLoss             = 0.0d0 ! Sensible heat loss in Joules
    REAL(r64) :: SenHeatPower            = 0.0d0 ! Sensible heat loss rate in watts
    REAL(r64) :: LatHeatLoss             = 0.0d0 ! Latent heat loss in Joules
    REAL(r64) :: LatHeatPower            = 0.0d0 ! Latent heat loss rate in watts
    REAL(r64) :: AirVolFlowRate          = 0.0d0 ! Air flow rate in m3/s
    REAL(r64) :: CoolTAirVol             = 0.0d0 ! Air volume in m3
    REAL(r64) :: ActualAirVolFlowRate    = 0.0d0 ! Actual air flow rate in m3/s
    REAL(r64) :: InletDBTemp             = 0.0d0 ! Outdoor dry bulb temperature in C
    REAL(r64) :: InletWBTemp             = 0.0d0 ! Outdoor wet bulb temperature in C
    REAL(r64) :: InletHumRat             = 0.0d0 ! Outdoor humidity ratio
    REAL(r64) :: OutletTemp              = 0.0d0 ! Dry bulb temperature at cooltower exit in C
    REAL(r64) :: OutletHumRat            = 0.0d0 ! Humidity ratio at cooltower exit
    REAL(r64) :: CoolTWaterConsumpRate   = 0.0d0 ! Total water consumption during the processes in m3/s
    REAL(r64) :: CoolTWaterStarvMakeupRate = 0.0d0 ! Water provided from the mains (m3/s)
    REAL(r64) :: CoolTWaterStarvMakeup   = 0.0d0 ! Water provided from the mains
    REAL(r64) :: CoolTWaterConsump       = 0.0d0 ! Total water consumption in m3
    REAL(r64) :: PumpElecPower           = 0.0d0 ! Pump power in watts
    REAL(r64) :: PumpElecConsump         = 0.0d0 ! Pump energy consumption in Joules

END TYPE CoolTowerParams

! MODULE VARIABLES DECLARATIONS:
TYPE (CoolTowerParams), ALLOCATABLE, DIMENSION(:) :: CoolTowerSys
INTEGER :: NumCoolTowers = 0                    ! Total cooltower statements in inputs

! Subroutine Specifications for the Heat Balance Module
PUBLIC  ManageCoolTower
PRIVATE GetCoolTower
PRIVATE CalcCoolTower
PRIVATE UpdateCoolTower
PRIVATE ReportCoolTower

CONTAINS


SUBROUTINE ManageCoolTower

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Daeho Kang
          !       DATE WRITTEN   Aug 2008
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine manages the simulation of Cooltower component.
          ! This driver manages the calls to all of the other drivers and simulation algorithms.

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

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  LOGICAL, SAVE :: GetInputFlag=.true.
!unused1208  LOGICAL :: ErrorsFound=.false.
!unused1208  INTEGER :: CoolTowerNum

        ! Obtains and allocates heat balance related parameters from input
  IF (GetInputFlag) THEN
    CALL GetCoolTower
    GetInputFlag=.false.
  ENDIF

  IF (NumCoolTowers == 0) RETURN

  CALL CalcCoolTower

  CALL UpdateCoolTower

  CALL ReportCoolTower

  RETURN

END SUBROUTINE ManageCoolTower


SUBROUTINE GetCooltower

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Daeho Kang
          !       DATE WRITTEN   Aug 2008
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine gets input data for cooltower components
          ! and stores it in the Cooltower data structure.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor,   ONLY: GetNumObjectsFound,GetObjectItem,FindItemInList,SameString,VerifyName,GetObjectDefMaxArgs
  USE ScheduleManager,  ONLY: GetScheduleIndex
  USE WaterManager,     ONLY: SetupTankDemandComponent
  USE General,          ONLY: RoundSigDigits

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER :: Blank = ' '
  CHARACTER(len=*), PARAMETER :: CurrentModuleObject='ZoneCoolTower:Shower'
  REAL(r64), PARAMETER :: MaximumWaterFlowRate = 0.016667d0  ! Maximum limit of water flow rate in m3/s (1000 l/min)
  REAL(r64), PARAMETER :: MinimumWaterFlowRate = 0.0d0       ! Minimum limit of water flow rate
  REAL(r64), PARAMETER :: MaxHeight = 30.0d0      ! Maximum effective tower height in m
  REAL(r64), PARAMETER :: MinHeight = 1.0d0       ! Minimum effective tower height in m
  REAL(r64), PARAMETER :: MaxValue = 100.0d0      ! Maximum limit of outlet area, airflow, and temperature
  REAL(r64), PARAMETER :: MinValue = 0.0d0        ! Minimum limit of outlet area, airflow, and temperature
  REAL(r64), PARAMETER :: MaxFrac = 1.0d0         ! Maximum fraction
  REAL(r64), PARAMETER :: MinFrac = 0.0d0         ! Minimum fraction

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    LOGICAL :: ErrorsFound = .false.    ! If errors detected in input
    LOGICAL :: IsNotOK                  ! Flag to verify name
    LOGICAL :: IsBlank                  ! Flag for blank name
    INTEGER :: CoolTowerNum             ! Cooltower number
    INTEGER :: NumAlphas                ! Number of Alphas for each GetobjectItem call
    INTEGER :: NumNumbers               ! Number of Numbers for each GetobjectItem call
    INTEGER :: NumArgs
    INTEGER :: IOStat
    CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: cAlphaArgs      ! Alpha input items for object
    CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: cAlphaFields   ! Alpha field names
    CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: cNumericFields ! Numeric field names
    REAL(r64), ALLOCATABLE, DIMENSION(:) :: rNumericArgs          ! Numeric input items for object
    LOGICAL, ALLOCATABLE, DIMENSION(:)   :: lAlphaBlanks      ! Logical array, alpha field input BLANK = .true.
    LOGICAL, ALLOCATABLE, DIMENSION(:)   :: lNumericBlanks    ! Logical array, numeric field input BLANK = .true.

            ! Initializations and allocations
   CALL GetObjectDefMaxArgs(CurrentModuleObject,NumArgs,NumAlphas,NumNumbers)
   ALLOCATE(cAlphaArgs(NumAlphas))
   cAlphaArgs=' '
   ALLOCATE(cAlphaFields(NumAlphas))
   cAlphaFields=' '
   ALLOCATE(cNumericFields(NumNumbers))
   cNumericFields=' '
   ALLOCATE(rNumericArgs(NumNumbers))
   rNumericArgs=0.0d0
   ALLOCATE(lAlphaBlanks(NumAlphas))
   lAlphaBlanks=.true.
   ALLOCATE(lNumericBlanks(NumNumbers))
   lNumericBlanks=.true.

NumCoolTowers = GetNumObjectsFound(CurrentModuleObject)

ALLOCATE (CoolTowerSys(NumCoolTowers))

            ! Obtain inputs
DO CoolTowerNum = 1, NumCoolTowers

    CALL GetObjectItem(CurrentModuleObject,CoolTowerNum,cAlphaArgs,NumAlphas,rNumericArgs,NumNumbers,IOStat,  &
                   AlphaBlank=lAlphaBlanks,NumBlank=lNumericBlanks,  &
                   AlphaFieldnames=cAlphaFields,NumericFieldNames=cNumericFields)
        IsNotOK=.false.
        IsBlank=.false.
        CALL VerifyName(cAlphaArgs(1),CoolTowerSys%Name,CoolTowerNum,IsNotOK,IsBlank,CurrentModuleObject//' Name')

        IF (IsNotOK) THEN
           ErrorsFound = .true.
        ENDIF

    CoolTowerSys(CoolTowerNum)%Name = cAlphaArgs(1)       ! Name of cooltower

    CoolTowerSys(CoolTowerNum)%Schedule = cAlphaArgs(2)   ! Get schedule
    IF (lAlphaBlanks(2)) THEN
      CoolTowerSys(CoolTowerNum)%SchedPtr  = ScheduleAlwaysOn
    ELSE
      CoolTowerSys(CoolTowerNum)%SchedPtr  = GetScheduleIndex(cAlphaArgs(2))
      IF (CoolTowerSys(CoolTowerNum)%SchedPtr == 0) THEN
        CALL ShowSevereError(trim(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid data')
        CALL ShowContinueError('Invalid-Schedule not found '//trim(cAlphaFields(2))//'="'//trim(cAlphaArgs(2))//'".')
        ErrorsFound = .TRUE.
      ENDIF
    ENDIF

    CoolTowerSys(CoolTowerNum)%ZoneName = cAlphaArgs(3)   ! Name of zone where cooltower is serving
    CoolTowerSys(CoolTowerNum)%ZonePtr = FindIteminList(cAlphaArgs(3),Zone%Name,NumOfZones)
    IF (CoolTowerSys(CoolTowerNum)%ZonePtr == 0) THEN
      IF (lAlphaBlanks(3)) THEN
        CALL ShowSevereError(trim(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid '//   &
           trim(cAlphaFields(3))//' is required but input is blank.')
      ELSE
        CALL ShowSevereError(trim(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid '//   &
           trim(cAlphaFields(3))//'="'//trim(cAlphaArgs(3))//'" not found.')
      END IF
      ErrorsFound = .TRUE.
    ENDIF

    CoolTowerSys(CoolTowerNum)%CoolTWaterSupplyName = cAlphaArgs(4) ! Name of water storage tank
    IF (lAlphaBlanks(4)) THEN
       CoolTowerSys(CoolTowerNum)%CoolTWaterSupplyMode = WaterSupplyFromMains
    ELSE IF (CoolTowerSys(CoolTowerNum)%CoolTWaterSupplyMode == WaterSupplyFromTank) THEN
       CALL SetupTankDemandComponent(CoolTowerSys(CoolTowerNum)%Name, CurrentModuleObject, &
                    CoolTowerSys(CoolTowerNum)%CoolTWaterSupplyName, ErrorsFound, &
                    CoolTowersys(CoolTowerNum)%CoolTWaterSupTankID, &
                    CoolTowerSys(CoolTowerNum)%CoolTWaterTankDemandARRID)
    ENDIF

    SELECT CASE (cAlphaArgs(5))    ! Type of flow control
      CASE ('WATERFLOWSCHEDULE')
        CoolTowerSys(CoolTowerNum)%FlowCtrlType = WaterFlowSchedule
      CASE ('WINDDRIVENFLOW','NONE',' ')
        CoolTowerSys(CoolTowerNum)%FlowCtrlType = WindDrivenFlow
      CASE DEFAULT
        CALL ShowSevereError(trim(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid '//   &
          trim(cAlphaFields(5))//'="'//trim(cAlphaArgs(5))//'".')
        ErrorsFound = .TRUE.
    END SELECT

    CoolTowerSys(CoolTowerNum)%PumpSchedName = cAlphaArgs(6)  !Get schedule for water pump
    CoolTowerSys(CoolTowerNum)%PumpSchedPtr  = GetScheduleIndex(cAlphaArgs(6))
    IF (CoolTowerSys(CoolTowerNum)%PumpSchedPtr == 0) THEN
      IF (lAlphaBlanks(6)) THEN
        CALL ShowSevereError(trim(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid '//   &
           trim(cAlphaFields(6))//' is required but input is blank.')
      ELSE
        CALL ShowSevereError(trim(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid '//   &
           trim(cAlphaFields(6))//'="'//trim(cAlphaArgs(6))//'" not found.')
      ENDIF
      ErrorsFound = .TRUE.
    ENDIF

    CoolTowerSys(CoolTowerNum)%MaxWaterFlowRate = rNumericArgs(1)   ! Maximum limit of water supply
    IF (CoolTowerSys(CoolTowerNum)%MaxWaterFlowRate > MaximumWaterFlowRate) THEN
        CoolTowerSys(CoolTowerNum)%MaxWaterFlowRate = MaximumWaterFlowRate
        CALL ShowWarningError(trim(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid '//   &
           trim(cNumericFields(1))//'=['//trim(RoundSigDigits(rNumericArgs(1),2))//'].')
        CALL ShowContinueError('...Maximum Allowable=['//trim(RoundSigDigits(MaximumWaterFlowRate,2))//'].')
    END IF
    IF (CoolTowerSys(CoolTowerNum)%MaxWaterFlowRate < MinimumWaterFlowRate) THEN
        CoolTowerSys(CoolTowerNum)%MaxWaterFlowRate = MinimumWaterFlowRate
        CALL ShowWarningError(trim(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid '//   &
           trim(cNumericFields(1))//'=['//trim(RoundSigDigits(rNumericArgs(1),2))//'].')
        CALL ShowContinueError('...Minimum Allowable=['//trim(RoundSigDigits(MinimumWaterFlowRate,2))//'].')
    END IF

    CoolTowerSys(CoolTowerNum)%TowerHeight = rNumericArgs(2)     ! Get effctive tower height
    IF (CoolTowerSys(CoolTowerNum)%TowerHeight > MaxHeight) THEN
        CoolTowerSys(CoolTowerNum)%TowerHeight = MaxHeight
        CALL ShowWarningError(trim(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid '//   &
           trim(cNumericFields(2))//'=['//trim(RoundSigDigits(rNumericArgs(2),2))//'].')
        CALL ShowContinueError('...Maximum Allowable=['//trim(RoundSigDigits(MaxHeight,2))//'].')
    END IF
    IF (CoolTowerSys(CoolTowerNum)%TowerHeight < MinHeight)  THEN
        CoolTowerSys(CoolTowerNum)%TowerHeight = MinHeight
        CALL ShowWarningError(trim(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid '//   &
           trim(cNumericFields(2))//'=['//trim(RoundSigDigits(rNumericArgs(2),2))//'].')
        CALL ShowContinueError('...Minimum Allowable=['//trim(RoundSigDigits(MinHeight,2))//'].')
    END IF

    CoolTowerSys(CoolTowerNum)%OutletArea = rNumericArgs(3)      ! Get outlet area
    IF (CoolTowerSys(CoolTowerNum)%OutletArea > MaxValue) THEN
        CoolTowerSys(CoolTowerNum)%OutletArea = MaxValue
        CALL ShowWarningError(trim(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid '//   &
           trim(cNumericFields(3))//'=['//trim(RoundSigDigits(rNumericArgs(3),2))//'].')
        CALL ShowContinueError('...Maximum Allowable=['//trim(RoundSigDigits(MaxValue,2))//'].')
    END IF
    IF (CoolTowerSys(CoolTowerNum)%OutletArea < MinValue) THEN
        CoolTowerSys(CoolTowerNum)%OutletArea = MinValue
        CALL ShowWarningError(trim(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid '//   &
           trim(cNumericFields(3))//'=['//trim(RoundSigDigits(rNumericArgs(3),2))//'].')
        CALL ShowContinueError('...Minimum Allowable=['//trim(RoundSigDigits(MinValue,2))//'].')
    END IF

    CoolTowerSys(CoolTowerNum)%MaxAirVolFlowRate = rNumericArgs(4)   ! Maximum limit of air flow to the space
    IF (CoolTowerSys(CoolTowerNum)%MaxAirVolFlowRate > MaxValue) THEN
        CoolTowerSys(CoolTowerNum)%MaxAirVolFlowRate = MaxValue
        CALL ShowWarningError(trim(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid '//   &
           trim(cNumericFields(4))//'=['//trim(RoundSigDigits(rNumericArgs(4),2))//'].')
        CALL ShowContinueError('...Maximum Allowable=['//trim(RoundSigDigits(MaxValue,2))//'].')
    END IF
    IF (CoolTowerSys(CoolTowerNum)%MaxAirVolFlowRate < MinValue) THEN
        CoolTowerSys(CoolTowerNum)%MaxAirVolFlowRate = MinValue
        CALL ShowWarningError(trim(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid '//   &
           trim(cNumericFields(4))//'=['//trim(RoundSigDigits(rNumericArgs(4),2))//'].')
        CALL ShowContinueError('...Minimum Allowable=['//trim(RoundSigDigits(MinValue,2))//'].')
    END IF

    CoolTowerSys(CoolTowerNum)%MinZoneTemp = rNumericArgs(5)     ! Get minimum temp limit which gets this cooltower off
    IF (CoolTowerSys(CoolTowerNum)%MinZoneTemp > MaxValue) THEN
        CoolTowerSys(CoolTowerNum)%MinZoneTemp = MaxValue
        CALL ShowWarningError(trim(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid '//   &
           trim(cNumericFields(5))//'=['//trim(RoundSigDigits(rNumericArgs(5),2))//'].')
        CALL ShowContinueError('...Maximum Allowable=['//trim(RoundSigDigits(MaxValue,2))//'].')
    END IF
    IF (CoolTowerSys(CoolTowerNum)%MinZoneTemp < MinValue) THEN
        CoolTowerSys(CoolTowerNum)%MinZoneTemp = MinValue
        CALL ShowWarningError(trim(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid '//   &
           trim(cNumericFields(5))//'=['//trim(RoundSigDigits(rNumericArgs(5),2))//'].')
        CALL ShowContinueError('...Minimum Allowable=['//trim(RoundSigDigits(MinValue,2))//'].')
    END IF

    CoolTowerSys(CoolTowerNum)%FracWaterLoss = rNumericArgs(6)   ! Fraction of water loss
    IF (CoolTowerSys(CoolTowerNum)%FracWaterLoss > MaxFrac) THEN
        CoolTowerSys(CoolTowerNum)%FracWaterLoss = MaxFrac
        CALL ShowWarningError(trim(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid '//   &
           trim(cNumericFields(6))//'=['//trim(RoundSigDigits(rNumericArgs(6),2))//'].')
        CALL ShowContinueError('...Maximum Allowable=['//trim(RoundSigDigits(MaxFrac,2))//'].')
    END IF
    IF (CoolTowerSys(CoolTowerNum)%FracWaterLoss < MinFrac) THEN
        CoolTowerSys(CoolTowerNum)%FracWaterLoss = MinFrac
        CALL ShowWarningError(trim(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid '//   &
           trim(cNumericFields(6))//'=['//trim(RoundSigDigits(rNumericArgs(6),2))//'].')
        CALL ShowContinueError('...Minimum Allowable=['//trim(RoundSigDigits(MinFrac,2))//'].')
    END IF

    CoolTowerSys(CoolTowerNum)%FracFlowSched = rNumericArgs(7)   ! Fraction of loss of air flow
    IF (CoolTowerSys(CoolTowerNum)%FracFlowSched > MaxFrac) THEN
        CoolTowerSys(CoolTowerNum)%FracFlowSched = MaxFrac
        CALL ShowWarningError(trim(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid '//   &
           trim(cNumericFields(7))//'=['//trim(RoundSigDigits(rNumericArgs(7),2))//'].')
        CALL ShowContinueError('...Maximum Allowable=['//trim(RoundSigDigits(MaxFrac,2))//'].')
    END IF
    IF (CoolTowerSys(CoolTowerNum)%FracFlowSched < MinFrac) THEN
        CoolTowerSys(CoolTowerNum)%FracFlowSched = MinFrac
        CALL ShowWarningError(trim(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid '//   &
           trim(cNumericFields(7))//'=['//trim(RoundSigDigits(rNumericArgs(7),5))//'].')
        CALL ShowContinueError('...Minimum Allowable=['//trim(RoundSigDigits(MinFrac,2))//'].')
    END IF

    CoolTowerSys(CoolTowerNum)%RatedPumpPower = rNumericArgs(8)  ! Get rated pump power

END DO

  DEALLOCATE(cAlphaArgs)
  DEALLOCATE(cAlphaFields)
  DEALLOCATE(cNumericFields)
  DEALLOCATE(rNumericArgs)
  DEALLOCATE(lAlphaBlanks)
  DEALLOCATE(lNumericBlanks)

  IF (ErrorsFound) Call ShowFatalError(CurrentModuleObject//' errors occurred in input.  Program terminates.')

DO CoolTowerNum = 1, NumCoolTowers
    CALL SetupOutputVariable('Zone Cooltower Sensible Heat Loss Energy [J]',CoolTowerSys(CoolTowerNum)%SenHeatLoss, &
                             'System','Sum',Zone(CoolTowerSys(CoolTowerNum)%ZonePtr)%Name)
    CALL SetupOutputVariable('Zone Cooltower Sensible Heat Loss Rate [W]',CoolTowerSys(CoolTowerNum)%SenHeatPower, &
                              'System','Average',Zone(CoolTowerSys(CoolTowerNum)%ZonePtr)%Name)
    CALL SetupOutputVariable('Zone Cooltower Latent Heat Loss Energy [J]',CoolTowerSys(CoolTowerNum)%LatHeatLoss, &
                             'System','Sum',Zone(CoolTowerSys(CoolTowerNum)%ZonePtr)%Name)
    CALL SetupOutputVariable('Zone Cooltower Latent Heat Loss Rate [W]',CoolTowerSys(CoolTowerNum)%LatHeatPower, &
                             'System','Average',Zone(CoolTowerSys(CoolTowerNum)%ZonePtr)%Name)
    CALL SetupOutputVariable('Zone Cooltower Air Volume [m3]',CoolTowerSys(CoolTowerNum)%CoolTAirVol, &
                             'System','Sum',Zone(CoolTowerSys(CoolTowerNum)%ZonePtr)%Name)
    CALL SetupOutputVariable('Zone Cooltower Air Volume Flow Rate [m3/s]',CoolTowerSys(CoolTowerNum)%AirVolFlowRate, &
                             'System','Average',Zone(CoolTowerSys(CoolTowerNum)%ZonePtr)%Name)
    CALL SetupOutputVariable('Zone Cooltower Air Mass [kg]',CoolTowerSys(CoolTowerNum)%CoolTAirMass, &
                             'System','Sum',Zone(CoolTowerSys(CoolTowerNum)%ZonePtr)%Name)
    CALL SetupOutputVariable('Zone Cooltower Air Mass Flow Rate [kg/s]',CoolTowerSys(CoolTowerNum)%AirMassFlowRate, &
                             'System','Average',Zone(CoolTowerSys(CoolTowerNum)%ZonePtr)%Name)
    CALL SetupOutputVariable('Zone Cooltower Air Inlet Temperature [C]',CoolTowerSys(CoolTowerNum)%InletDBTemp, &
                             'System','Average',Zone(CoolTowerSys(CoolTowerNum)%ZonePtr)%Name)
    CALL SetupOutputVariable('Zone Cooltower Air Inlet Humidity Ratio [kgWater/kgDryAir]',CoolTowerSys(CoolTowerNum)%InletHumRat, &
                             'System','Average',Zone(CoolTowerSys(CoolTowerNum)%ZonePtr)%Name)
    CALL SetupOutputVariable('Zone Cooltower Air Outlet Temperature [C]',CoolTowerSys(CoolTowerNum)%OutletTemp, &
                             'System','Average',Zone(CoolTowerSys(CoolTowerNum)%ZonePtr)%Name)
    CALL SetupOutputVariable('Zone Cooltower Air Outlet Humidity Ratio [kgWater/kgDryAir]',  &
                             CoolTowerSys(CoolTowerNum)%OutletHumRat, &
                             'System','Average',Zone(CoolTowerSys(CoolTowerNum)%ZonePtr)%Name)
    CALL SetupOutputVariable('Zone Cooltower Pump Electric Power [W]',CoolTowerSys(CoolTowerNum)%PumpElecPower, &
                             'System','Average',Zone(CoolTowerSys(CoolTowerNum)%ZonePtr)%Name)
    CALL SetupOutputVariable('Zone Cooltower Pump Electric Energy [J]',CoolTowerSys(CoolTowerNum)%PumpElecConsump, &
                             'System','Sum',Zone(CoolTowerSys(CoolTowerNum)%ZonePtr)%Name, &
                             ResourceTypeKey='Electric',EndUseKey='Cooling',GroupKey='System')
    IF (CoolTowerSys(CoolTowerNum)%CoolTWaterSupplyMode == WaterSupplyFromMains) THEN
        CALL SetupOutputVariable('Zone Cooltower Water Volume [m3]',CoolTowerSys(CoolTowerNum)%CoolTWaterConsump, &
                                  'System','Sum',Zone(CoolTowerSys(CoolTowerNum)%ZonePtr)%Name)
        CALL SetupOutputVariable('Zone Cooltower Mains Water Volume [m3]',CoolTowerSys(CoolTowerNum)%CoolTWaterConsump, &
                                 'System','Sum',Zone(CoolTowerSys(CoolTowerNum)%ZonePtr)%Name, &
                                  ResourceTypeKey='MainsWater',EndUseKey='Cooling',GroupKey='System')
    ELSE IF  (CoolTowerSys(CoolTowerNum)%CoolTWaterSupplyMode == WaterSupplyFromTank) THEN
        CALL SetupOutputVariable('Zone Cooltower Water Volume [m3]',CoolTowerSys(CoolTowerNum)%CoolTWaterConsump, &
                                  'System','Sum',Zone(CoolTowerSys(CoolTowerNum)%ZonePtr)%Name)
        CALL SetupOutputVariable('Zone Cooltower Storage Tank Water Volume [m3]',CoolTowerSys(CoolTowerNum)%CoolTWaterConsump, &
                                 'System','Sum',Zone(CoolTowerSys(CoolTowerNum)%ZonePtr)%Name)
        CALL SetupOutputVariable('Zone Cooltower Starved Mains Water Volume [m3]',  &
                                  CoolTowerSys(CoolTowerNum)%CoolTWaterStarvMakeup, &
                                 'System','Sum',Zone(CoolTowerSys(CoolTowerNum)%ZonePtr)%Name, &
                                  ResourceTypeKey='MainsWater',EndUseKey='Cooling',GroupKey='System')
    END IF
END DO

RETURN

END SUBROUTINE GetCoolTower


SUBROUTINE CalcCoolTower

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Daeho Kang
          !       DATE WRITTEN   Aug 2008
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! Baruch Givoni. 1994. Passive and Low Energy Cooling of Buildings. Chapter 5: Evaporative Cooling Systems.
          !     John Wiley & Sons, Inc.

          ! USE STATEMENTS:
USE DataHeatBalFanSys, ONLY: MAT, ZT, ZoneAirHumRat, MCPC, MCPTC, CTMFL
USE ScheduleManager,   ONLY: GetCurrentScheduleValue
USE Psychrometrics,    ONLY: PsyCpAirFnWTdb, PsyWFnTdbTwbPb, PsyWFnTdbH, PsyRhoAirFnPbTdbW, RhoH2O
USE DataEnvironment,   ONLY: OutBaroPress, OutDryBulbTemp, OutWetBulbTemp, OutHumRat, WindSpeed, OutEnthalpy

IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:

          ! SUBROUTINE PARAMETER DEFINITIONS:
REAL(r64), PARAMETER :: MinWindSpeed = 0.1d0    ! Minimum limit of outdoor air wind speed in m/s
REAL(r64), PARAMETER :: MaxWindSpeed = 30.0d0   ! Maximum limit of outdoor air wind speed in m/s
REAL(r64), PARAMETER :: UCFactor = 60000.0d0    ! Unit conversion factor m3/s to l/min

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
INTEGER :: ZoneNum              ! Number of zone being served
INTEGER :: CoolTowerNum         ! Number of coolter being served
REAL(r64), ALLOCATABLE, DIMENSION(:), SAVE :: CVF  ! Design flow rate in m3/s
REAL(r64) :: AirMassFlowRate    ! Actual air mass flow rate in kg/s
REAL(r64) :: AirSpecHeat        ! Specific heat of air
REAL(r64) :: AirDensity         ! Density of air
REAL(r64) :: RhoWater           ! Density of water
REAL(r64) :: PumpPartLoadRat    ! Pump part load ratio (based on user schedule, or 1.0 for no schedule)
REAL(r64) :: WaterFlowRate      ! Calculated water flow rate in m3/s
REAL(r64) :: AirVolFlowRate     ! Calculated air volume flow rate in m3/s
REAL(r64) :: InletHumRat        ! Humidity ratio of outdoor air
!unused1208REAL(r64) :: InletEnthalpy      ! Enthalpy of outdoor air
REAL(r64) :: OutletHumRat       ! Humidity ratio of air at the cooltower outlet
REAL(r64) :: OutletTemp         ! Dry bulb temperature of air at the cooltower outlet
REAL(r64) :: IntHumRat          ! Humidity ratio of initialized air

            ! Allocate the CVF array
IF (.NOT. ALLOCATED(CVF)) ALLOCATE(CVF(NumOfZones))
    CVF  = 0.0d0
    MCPTC = 0.0d0
    MCPC  = 0.0d0
    CTMFL = 0.0d0

DO CoolTowerNum = 1, NumCoolTowers
    ZoneNum = CoolTowerSys(CoolTowerNum)%ZonePtr

    IF (GetCurrentScheduleValue(CoolTowerSys(CoolTowerNum)%SchedPtr) > 0.0d0) THEN
            ! check component operation
        IF (WindSpeed < MinWindSpeed .OR. WindSpeed > MaxWindSpeed)  CYCLE
        IF (MAT(ZoneNum) < CoolTowerSys(CoolTowerNum)%MinZoneTemp)   CYCLE

            ! Unit is on and simulate this component
            ! Determine the temperature and air flow rate at the cooltower outlet
        IF (CoolTowerSys(CoolTowerNum)%FlowCtrlType == WindDrivenFlow) THEN
            CoolTowerSys(CoolTowerNum)%OutletVelocity = 0.7d0 * (CoolTowerSys(CoolTowerNum)%TowerHeight)**0.5d0 + &
                                                        0.47d0 * (WindSpeed - 1.d0)
            AirVolFlowRate = CoolTowerSys(CoolTowerNum)%OutletArea * CoolTowerSys(CoolTowerNum)%OutletVelocity
            AirVolFlowRate = Min(AirVolFlowRate,CoolTowerSys(CoolTowerNum)%MaxAirVolFlowRate)
            WaterFlowRate  = (AirVolFlowRate / (0.0125d0 * (CoolTowerSys(CoolTowerNum)%TowerHeight)**0.5d0))
            IF (WaterFlowRate > CoolTowerSys(CoolTowerNum)%MaxWaterFlowRate * UCFactor) THEN
                WaterFlowRate = CoolTowerSys(CoolTowerNum)%MaxWaterFlowRate * UCFactor
                AirVolFlowRate = 0.0125d0 * WaterFlowRate * (CoolTowerSys(CoolTowerNum)%TowerHeight)**0.5d0
                AirVolFlowRate = Min(AirVolFlowRate,CoolTowerSys(CoolTowerNum)%MaxAirVolFlowRate)
            END IF
            WaterFlowRate = MIN(WaterFlowRate,(CoolTowerSys(CoolTowerNum)%MaxWaterFlowRate * UCFactor))
            OutletTemp    = OutDryBulbTemp - (OutDryBulbTemp - OutWetBulbTemp) * &
                            (1.d0-exp(-0.8d0 * CoolTowerSys(CoolTowerNum)%TowerHeight)) * &
                            (1.d0-exp(-0.15d0 * WaterFlowRate))
        ELSE IF (CoolTowerSys(CoolTowerNum)%FlowCtrlType == WaterFlowSchedule) THEN
            WaterFlowRate  = CoolTowerSys(CoolTowerNum)%MaxWaterFlowRate * UCFactor
            AirVolFlowRate = 0.0125d0 * WaterFlowRate * (CoolTowerSys(CoolTowerNum)%TowerHeight)**0.5d0
            AirVolFlowRate = Min(AirVolFlowRate,CoolTowerSys(CoolTowerNum)%MaxAirVolFlowRate)
            OutletTemp     = OutDryBulbTemp - (OutDryBulbTemp - OutWetBulbTemp) * &
                            (1.d0-exp(-0.8d0 * CoolTowerSys(CoolTowerNum)%TowerHeight)) * &
                            (1.d0-exp(-0.15d0 * WaterFlowRate))
        END IF

        IF (OutletTemp < OutWetBulbTemp) THEN
            CALL ShowSevereError('Cooltower outlet temperature exceed the outdoor wet bulb temperature '//&
                            'reset to input values')
            CALL ShowContinueError('Occurs in Cooltower ='//TRIM(CoolTowerSys(CoolTowerNum)%Name))
        END IF

            WaterFlowRate = WaterFlowRate / UCFactor
            ! Determine actual water flow rate
        IF (CoolTowerSys(CoolTowerNum)%FracWaterLoss > 0.0d0) THEN
            CoolTowerSys(CoolTowerNum)%ActualWaterFlowRate = WaterFlowRate * &
                                                            (1.0d0 + CoolTowerSys(CoolTowerNum)%FracWaterLoss)
        ELSE
            CoolTowerSys(CoolTowerNum)%ActualWaterFlowRate = WaterFlowRate
        END IF

            ! Determine actual air flow rate
        IF (CoolTowerSys(CoolTowerNum)%FracFlowSched > 0.0d0) THEN
            CoolTowerSys(CoolTowerNum)%ActualAirVolFlowRate = AirVolFlowRate * &
                                                            (1.0d0 - CoolTowerSys(CoolTowerNum)%FracFlowSched)
        ELSE
            CoolTowerSys(CoolTowerNum)%ActualAirVolFlowRate = AirVolFlowRate
        END IF

                     ! Determine pump power
        IF (GetCurrentScheduleValue(CoolTowerSys(CoolTowerNum)%PumpSchedPtr) > 0) THEN
            PumpPartLoadRat = GetCurrentScheduleValue(CoolTowerSys(CoolTowerNum)%PumpSchedPtr)
        ELSE
            PumpPartLoadRat = 1.0d0
        END IF

            ! Determine air mass flow rate and volume flow rate
        InletHumRat   = PsyWFnTdbTwbPb(OutDryBulbTemp,OutWetBulbTemp,OutBaroPress)
            ! Assume no pressure drops and no changes in enthalpy between inlet and outlet air
        IntHumRat  = PsyWFnTdbH(OutletTemp,OutEnthalpy)     ! Initialized humidity ratio
        AirDensity = PsyRhoAirFnPbTdbW(OutBaroPress,OutletTemp,IntHumRat)
        AirMassFlowRate = AirDensity * CoolTowerSys(CoolTowerNum)%ActualAirVolFlowRate
            ! From the mass balance W_in*(m_air + m_water) = W_out*m_air
        RhoWater = RhoH2O(OutletTemp)   ! Assume T_water = T_outlet
        OutletHumRat = (InletHumRat*(AirMassFlowRate + (CoolTowerSys(CoolTowerNum)%ActualWaterFlowRate * RhoWater))) &
                        / AirMassFlowRate
        AirSpecHeat  = PsyCpAirFnWTdb(OutletHumRat,OutletTemp)
        AirDensity   = PsyRhoAirFnPbTdbW(OutBaroPress,OutletTemp,OutletHumRat)  ! Outlet air density
        CVF(ZoneNum) = CoolTowerSys(CoolTowerNum)%ActualAirVolFlowRate * &
                        GetCurrentScheduleValue(CoolTowerSys(CoolTowerNum)%SchedPtr)
        MCPC(ZoneNum)  = CVF(ZoneNum) * AirDensity * AirSpecHeat
        MCPTC(ZoneNum) = MCPC(ZoneNum) * OutletTemp
        CTMFL(ZoneNum) = MCPC(ZoneNum) / AirSpecHeat

        CoolTowerSys(CoolTowerNum)%SenHeatPower = MCPC(ZoneNum) * ABS(ZT(ZoneNum) - OutletTemp)
        CoolTowerSys(CoolTowerNum)%LatHeatPower = CVF(ZoneNum) * ABS(ZoneAirHumRat(ZoneNum) - OutletHumRat)
        CoolTowerSys(CoolTowerNum)%OutletTemp   = OutletTemp
        CoolTowerSys(CoolTowerNum)%OutletHumRat = OutletHumRat
        CoolTowerSys(CoolTowerNum)%AirVolFlowRate = CVF(ZoneNum)
        CoolTowerSys(CoolTowerNum)%AirMassFlowRate = CTMFL(ZoneNum)
        CoolTowerSys(CoolTowerNum)%InletDBTemp = Zone(ZoneNum)%OutDryBulbTemp
        CoolTowerSys(CoolTowerNum)%InletWBTemp = Zone(ZoneNum)%OutWetBulbTemp
        CoolTowerSys(CoolTowerNum)%InletHumRat = OutHumRat
        CoolTowerSys(CoolTowerNum)%CoolTWaterConsumpRate = (ABS(InletHumRat - OutletHumRat)*CTMFL(ZoneNum)) / RhoWater
        CoolTowerSys(CoolTowerNum)%CoolTWaterStarvMakeupRate = 0.0d0  ! initialize -- calc in update
        CoolTowerSys(CoolTowerNum)%PumpElecPower = CoolTowerSys(CoolTowerNum)%RatedPumpPower * PumpPartLoadRat
    ELSE    ! Unit is off
        CoolTowerSys(CoolTowerNum)%SenHeatPower = 0.0d0
        CoolTowerSys(CoolTowerNum)%LatHeatPower = 0.0d0
        CoolTowerSys(CoolTowerNum)%OutletTemp   = 0.0d0
        CoolTowerSys(CoolTowerNum)%OutletHumRat = 0.0d0
        CoolTowerSys(CoolTowerNum)%AirVolFlowRate = 0.0d0
        CoolTowerSys(CoolTowerNum)%AirMassFlowRate = 0.0d0
        CoolTowerSys(CoolTowerNum)%InletDBTemp = 0.0d0
        CoolTowerSys(CoolTowerNum)%InletHumRat = 0.0d0
        CoolTowerSys(CoolTowerNum)%PumpElecPower = 0.0d0
        CoolTowerSys(CoolTowerNum)%CoolTWaterConsumpRate = 0.0d0
        CoolTowerSys(CoolTowerNum)%CoolTWaterStarvMakeupRate = 0.0d0
    END IF

END DO

RETURN

END SUBROUTINE CalcCoolTower


SUBROUTINE UpdateCoolTower

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard J. Liesen
          !       DATE WRITTEN   October 2000
          !       MODIFIED       Aug 2008 Daeho Kang
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine needs a description.

          ! METHODOLOGY EMPLOYED:
          ! Needs description, as appropriate.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
USE DataWater

IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
INTEGER   :: CoolTowerNum
REAL(r64) :: AvailWaterRate

DO CoolTowerNum = 1, NumCoolTowers

       ! Set the demand request for supply water from water storage tank (if needed)
    IF (CoolTowerSys(CoolTowerNum)%CoolTWaterSupplyMode == WaterSupplyFromTank) THEN
      WaterStorage(CoolTowerSys(CoolTowerNum)%CoolTWaterSupTankID)% &
      VdotRequestDemand(CoolTowerSys(CoolTowerNum)%CoolTWaterTankDemandARRID) &
      = CoolTowerSys(CoolTowerNum)%CoolTWaterConsumpRate
    END IF

       !check if should be starved by restricted flow from tank
    IF (CoolTowerSys(CoolTowerNum)%CoolTWaterSupplyMode == WaterSupplyFromTank) THEN
      AvailWaterRate = WaterStorage(CoolTowerSys(CoolTowerNum)%CoolTWaterSupTankID)%VdotAvailDemand &
                    (CoolTowerSys(CoolTowerNum)%CoolTWaterTankDemandARRID)
      IF (AvailWaterRate < CoolTowerSys(CoolTowerNum)%CoolTWaterConsumpRate) THEN
        CoolTowerSys(CoolTowerNum)%CoolTWaterStarvMakeupRate = &
                      CoolTowerSys(CoolTowerNum)%CoolTWaterConsumpRate - AvailWaterRate
        CoolTowerSys(CoolTowerNum)%CoolTWaterConsumpRate = AvailWaterRate
      END IF
    END IF

END DO

RETURN

END Subroutine UpdateCoolTower


SUBROUTINE ReportCoolTower

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Daeho Kang
          !       DATE WRITTEN   Aut 2008
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine needs a description.

          ! METHODOLOGY EMPLOYED:
          ! Needs description, as appropriate.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
USE DataHVACGlobals, ONLY: TimeStepSys


IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
INTEGER :: CoolTowerNum
REAL(r64) :: TSMult

TSMult=TimeStepSys*SecInHour


DO CoolTowerNum = 1, NumCoolTowers

    CoolTowerSys(CoolTowerNum)%CoolTAirVol  = CoolTowerSys(CoolTowerNum)%AirVolFlowRate * TSMult
    CoolTowerSys(CoolTowerNum)%CoolTAirMass = CoolTowerSys(CoolTowerNum)%AirMassFlowRate * TSMult
    CoolTowerSys(CoolTowerNum)%SenHeatLoss      = CoolTowerSys(CoolTowerNum)%SenHeatPower * TSMult
    CoolTowerSys(CoolTowerNum)%LatHeatLoss      = CoolTowerSys(CoolTowerNum)%LatHeatPower * TSMult
    CoolTowerSys(CoolTowerNum)%PumpElecConsump   = CoolTowerSys(CoolTowerNum)%PumpElecPower * TSMult
    CoolTowerSys(CoolTowerNum)%CoolTWaterConsump = CoolTowerSys(CoolTowerNum)%CoolTWaterConsumpRate * TSMult
    CoolTowerSys(CoolTowerNum)%CoolTWaterStarvMakeup = CoolTowerSys(CoolTowerNum)%CoolTWaterStarvMakeupRate * TSMult
END DO

RETURN

END SUBROUTINE ReportCoolTower

!*****************************************************************************************
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

END MODULE CoolTower