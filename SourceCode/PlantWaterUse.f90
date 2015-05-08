MODULE WaterUse

          ! MODULE INFORMATION:
          !       AUTHOR         Peter Graham Ellis
          !       DATE WRITTEN   August 2006
          !       MODIFIED       Brent Griffith, plant upgrade
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS MODULE:
          !

          ! METHODOLOGY EMPLOYED:
          !

          ! USE STATEMENTS:
USE DataPrecisionGlobals
USE DataGlobals, ONLY: WarmupFlag, MaxNameLength, BeginEnvrnFlag, InitConvTemp, NumOfZones
USE DataInterfaces

IMPLICIT NONE ! Enforce explicit typing of all variables

PRIVATE ! Everything private unless explicitly made public

          ! MODULE PARAMETER DEFINITIONS:
INTEGER, PARAMETER :: HeatRecoveryHXIdeal = 1
INTEGER, PARAMETER :: HeatRecoveryHXCounterFlow = 2
INTEGER, PARAMETER :: HeatRecoveryHXCrossFlow = 3

INTEGER, PARAMETER :: HeatRecoveryConfigPlant = 1
INTEGER, PARAMETER :: HeatRecoveryConfigEquipment = 2
INTEGER, PARAMETER :: HeatRecoveryConfigPlantAndEquip= 3

          ! DERIVED TYPE DEFINITIONS:
TYPE WaterEquipmentType
  CHARACTER(len=MaxNameLength) :: Name = ''                ! Name of DHW
  CHARACTER(len=MaxNameLength) :: EndUseSubcatName = ''
  INTEGER                      :: Connections = 0          ! Index for WATER USE CONNECTIONS object

  REAL(r64)                    :: PeakVolFlowRate = 0.0d0    ! Peak volumetric flow rate, also water consumption rate (m3/s)
  INTEGER                      :: FlowRateFracSchedule = 0 ! Pointer to schedule object

  REAL(r64)                    :: ColdVolFlowRate = 0.0d0
  REAL(r64)                    :: HotVolFlowRate = 0.0d0
  REAL(r64)                    :: TotalVolFlowRate = 0.0d0        ! Volumetric flow rate, also water consumption rate (m3/s)

  REAL(r64)                    :: ColdMassFlowRate = 0.0d0
  REAL(r64)                    :: HotMassFlowRate = 0.0d0
  REAL(r64)                    :: TotalMassFlowRate = 0.0d0       ! Mass flow rate (kg/s)
  REAL(r64)                    :: DrainMassFlowRate = 0.0d0

  INTEGER                      :: ColdTempSchedule = 0     ! Index for schedule object
  INTEGER                      :: HotTempSchedule = 0      ! Index for schedule object
  INTEGER                      :: TargetTempSchedule = 0   ! Index for schedule object

  REAL(r64)                    :: ColdTemp = 0.0d0           ! Cold supply water temperature (C)
  REAL(r64)                    :: HotTemp = 0.0d0            ! Hot supply water temperature (C)
  REAL(r64)                    :: TargetTemp = 0.0d0         ! Target (mixed) water temperature (C)
  REAL(r64)                    :: MixedTemp = 0.0d0         ! Actual outlet (mixed) water temperature (C)
  REAL(r64)                    :: DrainTemp = 0.0d0

  INTEGER                      :: Zone = 0                 ! Index for zone object
  INTEGER                      :: SensibleFracSchedule = 0 ! Pointer to schedule object
  REAL(r64)                    :: SensibleRate = 0.0d0
  REAL(r64)                    :: SensibleEnergy = 0.0d0
  REAL(r64)                    :: SensibleRateNoMultiplier = 0.d0
  INTEGER                      :: LatentFracSchedule = 0   ! Pointer to schedule object
  REAL(r64)                    :: LatentRate = 0.0d0
  REAL(r64)                    :: LatentEnergy = 0.0d0
  REAL(r64)                    :: LatentRateNoMultiplier = 0.d0
  REAL(r64)                    :: MoistureRate = 0.0d0
  REAL(r64)                    :: MoistureMass = 0.0d0

  REAL(r64)                    :: ColdVolume = 0.0d0             ! Water consumption (m3)
  REAL(r64)                    :: HotVolume = 0.0d0             ! Water consumption (m3)
  REAL(r64)                    :: TotalVolume = 0.0d0             ! Water consumption (m3)

  REAL(r64)                    :: Power = 0.0d0              ! Heating rate required to meet the mixed water temperature (W)
  REAL(r64)                    :: Energy = 0.0d0             ! Heating energy required to meet the mixed water temperature (J)
END TYPE WaterEquipmentType

TYPE WaterConnectionsType
  CHARACTER(len=MaxNameLength) :: Name = ''                ! Name of DHW
  LOGICAL                      :: Init = .TRUE.            ! Flag for initialization:  TRUE means do the init
  LOGICAL                      :: InitSizing = .TRUE.      ! Flag for initialization of plant sizing
  LOGICAL                      :: StandAlone = .FALSE.     ! Flag for operation with no plant connections
  INTEGER                      :: InletNode = 0            ! Hot water demand node
  INTEGER                      :: OutletNode = 0           ! Cold water supply node
  INTEGER                      :: SupplyTankNum = 0
  INTEGER                      :: RecoveryTankNum = 0
  INTEGER                      :: TankDemandID = 0  ! array to request flow from supply tank
  INTEGER                      :: TankSupplyID = 0 ! array to send flow to recovery tank

  LOGICAL                      :: HeatRecovery = .FALSE.
  INTEGER                      :: HeatRecoveryHX = HeatRecoveryHXIdeal
  INTEGER                      :: HeatRecoveryConfig = HeatRecoveryConfigPlant
  REAL(r64)                    :: HXUA = 0.0d0
  REAL(r64)                    :: Effectiveness = 0.0d0
  REAL(r64)                    :: RecoveryRate = 0.0d0
  REAL(r64)                    :: RecoveryEnergy = 0.0d0

  REAL(r64)                    :: MainsMassFlowRate = 0.0d0       ! Mass flow rate (kg/s)
  REAL(r64)                    :: TankMassFlowRate = 0.0d0       ! Mass flow rate (kg/s)
  REAL(r64)                    :: ColdMassFlowRate = 0.0d0       ! Mass flow rate (kg/s)  cold = mains + tank
  REAL(r64)                    :: HotMassFlowRate = 0.0d0       ! Mass flow rate (kg/s)
  REAL(r64)                    :: TotalMassFlowRate = 0.0d0       ! Mass flow rate (kg/s) total = cold + hot
  REAL(r64)                    :: DrainMassFlowRate = 0.0d0
  REAL(r64)                    :: RecoveryMassFlowRate = 0.0d0

  REAL(r64)                    :: PeakVolFlowRate = 0.0d0        ! Volumetric flow rate, also water consumption rate (m3/s)
  REAL(r64)                    :: MainsVolFlowRate = 0.0d0        ! Volumetric flow rate, also water consumption rate (m3/s)
  REAL(r64)                    :: TankVolFlowRate = 0.0d0        ! Volumetric flow rate, also water consumption rate (m3/s)
  REAL(r64)                    :: ColdVolFlowRate = 0.0d0        ! Volumetric flow rate, also water consumption rate (m3/s)
  REAL(r64)                    :: HotVolFlowRate = 0.0d0        ! Volumetric flow rate, also water consumption rate (m3/s)
  REAL(r64)                    :: TotalVolFlowRate = 0.0d0        ! Volumetric flow rate, also water consumption rate (m3/s)
  REAL(r64)                    :: DrainVolFlowRate = 0.0d0
  REAL(r64)                    :: PeakMassFlowRate = 0.0D0 ! Peak Mass flow rate for MassFlowRateMax

  INTEGER                      :: ColdTempSchedule = 0     ! Index for schedule object
  INTEGER                      :: HotTempSchedule = 0      ! Index for schedule object

  REAL(r64)                    :: MainsTemp = 0.0d0           ! Cold supply water temperature (C)
  REAL(r64)                    :: TankTemp = 0.0d0           ! Cold supply water temperature (C)
  REAL(r64)                    :: ColdSupplyTemp = 0.0d0     ! cold from mains, schedule, or tank, depending
  REAL(r64)                    :: ColdTemp = 0.0d0           ! Cold supply water temperature (C)  actual cold (could be reheated)
  REAL(r64)                    :: HotTemp = 0.0d0            ! Hot supply water temperature (C)
  REAL(r64)                    :: DrainTemp = 0.0d0          !
  REAL(r64)                    :: RecoveryTemp = 0.0d0          !
  REAL(r64)                    :: ReturnTemp = 0.0d0         !
  REAL(r64)                    :: WasteTemp = 0.0d0         !

  REAL(r64)                    :: TempError = 0.0d0         !

  REAL(r64)                    :: MainsVolume = 0.0d0             ! Water consumption (m3)
  REAL(r64)                    :: TankVolume = 0.0d0             ! Water consumption (m3)
  REAL(r64)                    :: ColdVolume = 0.0d0             ! Water consumption (m3)
  REAL(r64)                    :: HotVolume = 0.0d0             ! Water consumption (m3)
  REAL(r64)                    :: TotalVolume = 0.0d0             ! Water consumption (m3)

  REAL(r64)                    :: Power = 0.0d0              ! Heating rate required to raise temperature from cold to hot (W)
  REAL(r64)                    :: Energy = 0.0d0             ! Heating energy required to raise temperature from cold to hot (J)

  INTEGER                      :: NumWaterEquipment = 0
  INTEGER                      :: MaxIterationsErrorIndex = 0   ! recurring error index
  INTEGER, DIMENSION(:), ALLOCATABLE :: WaterEquipment

  INTEGER                      :: PlantLoopNum       = 0
  INTEGER                      :: PlantLoopSide      = 0
  INTEGER                      :: PlantLoopBranchNum = 0
  INTEGER                      :: PlantLoopCompNum   = 0

END TYPE WaterConnectionsType

          ! MODULE VARIABLE TYPE DECLARATIONS:
TYPE (WaterEquipmentType), ALLOCATABLE, DIMENSION(:) :: WaterEquipment
TYPE (WaterConnectionsType), ALLOCATABLE, DIMENSION(:) :: WaterConnections

          ! MODULE VARIABLE DECLARATIONS:
INTEGER :: NumWaterEquipment       =0
INTEGER :: NumWaterConnections     =0
!INTEGER :: MaxIterationsErrorCount =0
LOGICAL :: GetWaterUseInputFlag = .TRUE.

LOGICAL, ALLOCATABLE, DIMENSION(:) :: CheckEquipName
LOGICAL, ALLOCATABLE, DIMENSION(:) :: CheckPlantLoop

          ! SUBROUTINE SPECIFICATIONS:
PUBLIC SimulateWaterUse
PUBLIC SimulateWaterUseConnection
PUBLIC CalcWaterUseZoneGains
PRIVATE GetWaterUseInput
PRIVATE CalcEquipmentFlowRates
PRIVATE CalcEquipmentDrainTemp
PRIVATE InitConnections
PRIVATE CalcConnectionsFlowRates
PRIVATE CalcConnectionsDrainTemp
PRIVATE CalcConnectionsHeatRecovery
PRIVATE UpdateWaterConnections
PRIVATE ReportWaterUse

CONTAINS

          ! MODULE SUBROUTINES:
SUBROUTINE SimulateWaterUse(FirstHVACIteration)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Peter Graham Ellis
          !       DATE WRITTEN   August 2006
          !       MODIFIED       Brent Griffith, March 2010, seperated plant connected to different sim routine
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This routine is called from non zone equipment manager and serves to call
          ! water use and connections that are not connected to a full plant loop


          ! METHODOLOGY EMPLOYED:
          !

          ! USE STATEMENTS:
  USE General, ONLY: RoundSigDigits

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE PARAMETER DEFINITIONS:
  INTEGER, PARAMETER :: MaxIterations = 100
  REAL(r64), PARAMETER    :: Tolerance = 0.1d0  ! Make input?

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  LOGICAL, INTENT(IN) :: FirstHVACIteration

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: WaterEquipNum
  INTEGER :: WaterConnNum
  INTEGER :: NumIteration
  INTEGER, SAVE :: MaxIterationsErrorCount
  LOGICAL, SAVE :: MyEnvrnFlag = .TRUE.

          ! FLOW:
  IF (GetWaterUseInputFlag) THEN
    CALL GetWaterUseInput
    GetWaterUseInputFlag = .FALSE.
  END IF

  IF (BeginEnvrnFlag .and. MyEnvrnFlag) THEN
    MaxIterationsErrorCount = 0
    IF (NumWaterEquipment > 0) THEN
      WaterEquipment%SensibleRate = 0.0d0
      WaterEquipment%SensibleEnergy = 0.0d0
      WaterEquipment%LatentRate = 0.0d0
      WaterEquipment%LatentEnergy = 0.0d0
      WaterEquipment%MixedTemp = 0.0d0
      WaterEquipment%TotalMassFlowRate = 0.0d0
      WaterEquipment%DrainTemp = 0.0d0
    ENDIF

    IF (NumWaterConnections > 0) THEN
      WaterConnections%TotalMassFlowRate = 0.0d0
    ENDIF

    MyEnvrnFlag = .false.
  END IF

  If ( .NOT. BeginEnvrnFlag) MyEnvrnFlag = .TRUE.

  ! Simulate all unconnected WATER USE EQUIPMENT objects
  DO WaterEquipNum = 1, NumWaterEquipment
    IF (WaterEquipment(WaterEquipNum)%Connections == 0) THEN
      CALL CalcEquipmentFlowRates(WaterEquipNum)
      CALL CalcEquipmentDrainTemp(WaterEquipNum)
    END IF
  END DO  ! WaterEquipNum

  CALL ReportStandAloneWaterUse

  ! Simulate WATER USE CONNECTIONS objects and connected WATER USE EQUIPMENT objects
  DO WaterConnNum = 1, NumWaterConnections

    IF (.NOT. WaterConnections(WaterConnNum)%StandAlone ) CYCLE ! only model non plant connections here

    CALL InitConnections(WaterConnNum)

    NumIteration = 0

    DO WHILE (.TRUE.)
      NumIteration = NumIteration + 1

      CALL CalcConnectionsFlowRates(WaterConnNum, FirstHVACIteration)
      CALL CalcConnectionsDrainTemp(WaterConnNum)
      CALL CalcConnectionsHeatRecovery(WaterConnNum)

      IF (WaterConnections(WaterConnNum)%TempError < Tolerance) THEN
        EXIT
      ELSE IF (NumIteration > MaxIterations) THEN
        IF (.NOT. WarmupFlag) THEN
          IF (WaterConnections(WaterConnNum)%MaxIterationsErrorIndex == 0) THEN
            CALL ShowWarningError('WaterUse:Connections = '//TRIM(WaterConnections(WaterConnNum)%Name)// &
              ':  Heat recovery temperature did not converge')
            CALL ShowContinueErrorTimeStamp(' ')
          ENDIF
          CALL ShowRecurringWarningErrorAtEnd('WaterUse:Connections = '//TRIM(WaterConnections(WaterConnNum)%Name)// &
              ':  Heat recovery temperature did not converge',  &
            WaterConnections(WaterConnNum)%MaxIterationsErrorIndex)
        END IF
        EXIT
      END IF

    END DO  ! WHILE

    CALL UpdateWaterConnections(WaterConnNum)

    CALL ReportWaterUse(WaterConnNum)

  END DO  ! WaterConnNum

  RETURN

END SUBROUTINE SimulateWaterUse

SUBROUTINE SimulateWaterUseConnection(EquipTypeNum, CompName, CompIndex, InitLoopEquip, FirstHVACIteration)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Brent Griffith March 2010, Demand Side Update
          !       DATE WRITTEN   August 2006
          !       MODIFIED
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Plant sim call for plant loop connected water use and connections
          ! (based on SimulateWaterUse by P. Ellis)

          ! METHODOLOGY EMPLOYED:
          !

          ! USE STATEMENTS:
  USE General, ONLY: RoundSigDigits, TrimSigDigits
  USE InputProcessor, ONLY: FindItemInList

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE PARAMETER DEFINITIONS:
  INTEGER, PARAMETER :: MaxIterations = 100
  REAL(r64), PARAMETER    :: Tolerance = 0.1d0  ! Make input?

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)          :: EquipTypeNum
  CHARACTER(len=*), INTENT(IN) :: CompName
  INTEGER, INTENT(INOUT)       :: CompIndex
  LOGICAL, INTENT(IN)          :: InitLoopEquip
  LOGICAL, INTENT(IN)          :: FirstHVACIteration

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
!  INTEGER :: WaterEquipNum
  INTEGER :: WaterConnNum
  INTEGER :: NumIteration
  INTEGER, SAVE :: MaxIterationsErrorCount
  LOGICAL, SAVE :: MyEnvrnFlag = .TRUE.

          ! FLOW:
  IF (GetWaterUseInputFlag) THEN
    CALL GetWaterUseInput
    GetWaterUseInputFlag = .FALSE.
  END IF

  IF (CompIndex == 0) THEN
    WaterConnNum = FindItemInList(CompName , WaterConnections%Name, NumWaterConnections )
    IF (WaterConnNum == 0) THEN
      CALL ShowFatalError('SimulateWaterUseConnection: Unit not found='//TRIM(CompName))
    ENDIF
    CompIndex = WaterConnNum
  ELSE
    WaterConnNum = CompIndex
    IF (WaterConnNum > NumWaterConnections .OR. WaterConnNum < 1) THEN
      Call ShowFatalError('SimulateWaterUseConnection: Invalid CompIndex passed='//  &
                          TRIM(TrimSigDigits(WaterConnNum))// &
                          ', Number of Units='//Trim(TrimSigDigits(NumWaterConnections))//  &
                          ', Entered Unit name='//Trim(CompName))
    ENDIF
    IF (CheckEquipName(WaterConnNum)) THEN
      IF (CompName /= WaterConnections(WaterConnNum)%Name) THEN
        CALL ShowFatalError('SimulateWaterUseConnection: Invalid CompIndex passed='//  &
                          TRIM(TrimSigDigits(WaterConnNum))// &
                          ', Unit name='//Trim(CompName )// ', stored Unit Name for that index='// &
                          TRIM(WaterConnections(WaterConnNum)%Name) )
      ENDIF
      CheckEquipName(WaterConnNum) = .FALSE.
    ENDIF
  ENDIF

  IF (InitLoopEquip) THEN
    RETURN
  ENDIF

  IF (BeginEnvrnFlag .and. MyEnvrnFlag) THEN
    MaxIterationsErrorCount = 0
    IF (NumWaterEquipment > 0) THEN
      WaterEquipment%SensibleRate = 0.0d0
      WaterEquipment%SensibleEnergy = 0.0d0
      WaterEquipment%LatentRate = 0.0d0
      WaterEquipment%LatentEnergy = 0.0d0
      WaterEquipment%MixedTemp = 0.0d0
      WaterEquipment%TotalMassFlowRate = 0.0d0
      WaterEquipment%DrainTemp = 0.0d0
    ENDIF

    IF (NumWaterConnections > 0) THEN
      WaterConnections%TotalMassFlowRate = 0.0d0
    ENDIF

    MyEnvrnFlag = .false.
  END IF

  IF ( .NOT. BeginEnvrnFlag) MyEnvrnFlag = .true.

  CALL InitConnections(WaterConnNum)

  NumIteration = 0

  DO WHILE (.TRUE.)
    NumIteration = NumIteration + 1

    CALL CalcConnectionsFlowRates(WaterConnNum, FirstHVACIteration)
    CALL CalcConnectionsDrainTemp(WaterConnNum)
    CALL CalcConnectionsHeatRecovery(WaterConnNum)

    IF (WaterConnections(WaterConnNum)%TempError < Tolerance) THEN
      EXIT
    ELSE IF (NumIteration > MaxIterations) THEN
      IF (.NOT. WarmupFlag) THEN
        IF (WaterConnections(WaterConnNum)%MaxIterationsErrorIndex == 0) THEN
          CALL ShowWarningError('WaterUse:Connections = '//TRIM(WaterConnections(WaterConnNum)%Name)// &
            ':  Heat recovery temperature did not converge')
          CALL ShowContinueErrorTimeStamp(' ')
        ENDIF
        CALL ShowRecurringWarningErrorAtEnd('WaterUse:Connections = '//TRIM(WaterConnections(WaterConnNum)%Name)// &
            ':  Heat recovery temperature did not converge',  &
          WaterConnections(WaterConnNum)%MaxIterationsErrorIndex)
      END IF
      EXIT
    END IF

  END DO  ! WHILE

  CALL UpdateWaterConnections(WaterConnNum)


  CALL ReportWaterUse(WaterConnNum)

  RETURN

END SUBROUTINE SimulateWaterUseConnection


SUBROUTINE GetWaterUseInput

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Peter Graham Ellis
          !       DATE WRITTEN   August 2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !

          ! METHODOLOGY EMPLOYED:
          ! Standard EnergyPlus methodology.

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: GetObjectDefMaxArgs, GetNumObjectsFound, FindItemInList, GetObjectItem, VerifyName
  USE DataIPShortCuts  ! Data for field names, blank numerics
  USE ScheduleManager, ONLY: GetScheduleIndex
  USE NodeInputManager, ONLY: GetOnlySingleNode
  USE BranchNodeConnections, ONLY: TestCompSet
  USE DataLoopNode
  USE DataHeatBalance
  USE WaterManager,   ONLY:SetupTankSupplyComponent,SetupTankDemandComponent
  USE Psychrometrics, ONLY: RhoH2O
  USE PlantUtilities, ONLY: RegisterPlantCompDesignFlow
  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER :: Blank = ' '

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  LOGICAL                        :: ErrorsFound = .FALSE. ! Set to true if errors in input, fatal at end of routine
  INTEGER                        :: IOStatus              ! Used in GetObjectItem
  LOGICAL                        :: IsBlank               ! TRUE if the name is blank
  LOGICAL                        :: IsNotOk               ! TRUE if there was a problem with a list name
  INTEGER                        :: NumAlphas             ! Number of Alphas for each GetObjectItem call
  INTEGER                        :: NumNumbers            ! Number of Numbers for each GetObjectItem call
!unused1208  INTEGER                        :: NumArgs
  INTEGER                        :: WaterEquipNum
  INTEGER                        :: WaterConnNum
  INTEGER                        :: AlphaNum
  INTEGER                        :: thisWaterEquipNum

          ! FLOW:

  cCurrentModuleObject = 'WaterUse:Equipment'
  NumWaterEquipment = GetNumObjectsFound(cCurrentModuleObject)

  IF (NumWaterEquipment > 0) THEN
    ALLOCATE(WaterEquipment(NumWaterEquipment))

    DO WaterEquipNum = 1, NumWaterEquipment
      CALL GetObjectItem( cCurrentModuleObject, WaterEquipNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, IOStatus, &
                    AlphaBlank=lAlphaFieldBlanks, AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

      IsNotOK = .FALSE.
      IsBlank = .FALSE.
      CALL VerifyName(cAlphaArgs(1), WaterEquipment%Name, WaterEquipNum-1, IsNotOK, IsBlank,  TRIM(cCurrentModuleObject)//' Name')
      IF (IsNotOK) THEN
        ErrorsFound = .TRUE.
        IF (IsBlank) cAlphaArgs(1) = 'xxxxx'
      END IF
      WaterEquipment(WaterEquipNum)%Name = cAlphaArgs(1)

      WaterEquipment(WaterEquipNum)%EndUseSubcatName = cAlphaArgs(2)

      WaterEquipment(WaterEquipNum)%PeakVolFlowRate = rNumericArgs(1)

      IF ((NumAlphas > 2) .AND. ( .NOT. lAlphaFieldBlanks(3) )) THEN
        WaterEquipment(WaterEquipNum)%FlowRateFracSchedule = GetScheduleIndex(cAlphaArgs(3))
        ! If no FlowRateFracSchedule, fraction defaults to 1.0

        IF (WaterEquipment(WaterEquipNum)%FlowRateFracSchedule == 0) THEN
          CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(3))//'='//TRIM(cAlphaArgs(3)))
          CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
          ErrorsFound = .TRUE.
        END IF
      END IF

      IF ((NumAlphas > 3) .AND. (.NOT. lAlphaFieldBlanks(4))) THEN
        WaterEquipment(WaterEquipNum)%TargetTempSchedule = GetScheduleIndex(cAlphaArgs(4))

        IF (WaterEquipment(WaterEquipNum)%TargetTempSchedule == 0) THEN
          CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(4))//'='//TRIM(cAlphaArgs(4)))
          CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
          ErrorsFound = .TRUE.
        END IF
      END IF

      IF ((NumAlphas > 4) .AND. (.NOT. lAlphaFieldBlanks(5))) THEN
        WaterEquipment(WaterEquipNum)%HotTempSchedule = GetScheduleIndex(cAlphaArgs(5))
        ! If no HotTempSchedule, there is no hot water.
        ! HotTempSchedule is ignored if connected to a plant loop via WATER USE CONNECTIONS

        IF (WaterEquipment(WaterEquipNum)%HotTempSchedule == 0) THEN
          CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(5))//'='//TRIM(cAlphaArgs(5)))
          CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
          ErrorsFound = .TRUE.
        END IF
      END IF

      IF ((NumAlphas > 5) .AND. ( .NOT. lAlphaFieldBlanks(6))) THEN
        WaterEquipment(WaterEquipNum)%ColdTempSchedule = GetScheduleIndex(cAlphaArgs(6))
        ! If no ColdTempSchedule, temperatures will be calculated by WATER MAINS TEMPERATURES object

        IF (WaterEquipment(WaterEquipNum)%ColdTempSchedule == 0) THEN
          CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(6))//'='//TRIM(cAlphaArgs(6)))
          CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
          ErrorsFound = .TRUE.
        END IF
      END IF

      IF ((NumAlphas > 6) .AND. (.NOT. lAlphaFieldBlanks(7) )) THEN
        WaterEquipment(WaterEquipNum)%Zone = FindItemInList(cAlphaArgs(7), Zone%Name, NumOfZones)

        IF (WaterEquipment(WaterEquipNum)%Zone == 0) THEN
          CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(7))//'='//TRIM(cAlphaArgs(7)))
          CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
          ErrorsFound = .TRUE.
        END IF
      END IF

      IF ((NumAlphas > 7) .AND. (.NOT. lAlphaFieldBlanks(8) )) THEN
        WaterEquipment(WaterEquipNum)%SensibleFracSchedule = GetScheduleIndex(cAlphaArgs(8))

        IF (WaterEquipment(WaterEquipNum)%SensibleFracSchedule == 0) THEN
          CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(8))//'='//TRIM(cAlphaArgs(8)))
          CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
          ErrorsFound = .TRUE.
        END IF
      END IF

      IF ((NumAlphas > 8) .AND. (.NOT. lAlphaFieldBlanks(9) )) THEN
        WaterEquipment(WaterEquipNum)%LatentFracSchedule = GetScheduleIndex(cAlphaArgs(9))

        IF (WaterEquipment(WaterEquipNum)%LatentFracSchedule == 0) THEN
          CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(9))//'='//TRIM(cAlphaArgs(9)))
          CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
          ErrorsFound = .TRUE.
        END IF
      END IF

    END DO ! WaterEquipNum

    IF (ErrorsFound) CALL ShowFatalError('Errors found in processing input for '//TRIM(cCurrentModuleObject))

  END IF

  cCurrentModuleObject = 'WaterUse:Connections'
  NumWaterConnections = GetNumObjectsFound(cCurrentModuleObject)

  IF (NumWaterConnections > 0) THEN
    ALLOCATE(WaterConnections(NumWaterConnections))

    DO WaterConnNum = 1, NumWaterConnections
      CALL GetObjectItem(cCurrentModuleObject, WaterConnNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, IOStatus, &
                    AlphaBlank=lAlphaFieldBlanks, AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

      IsNotOK = .FALSE.
      IsBlank = .FALSE.
      CALL VerifyName(cAlphaArgs(1), WaterConnections%Name, WaterConnNum-1, IsNotOK, IsBlank, TRIM(cCurrentModuleObject)//' Name')
      IF (IsNotOK) THEN
        ErrorsFound = .TRUE.
        IF (IsBlank) cAlphaArgs(1) = 'xxxxx'
      END IF
      WaterConnections(WaterConnNum)%Name = cAlphaArgs(1)

      IF ((.NOT. lAlphaFieldBlanks(2)) .OR. (.NOT. lAlphaFieldBlanks(3))) THEN
        WaterConnections(WaterConnNum)%InletNode = GetOnlySingleNode(cAlphaArgs(2), ErrorsFound, TRIM(cCurrentModuleObject), &
          cAlphaArgs(1), NodeType_Water, NodeConnectionType_Inlet, 1, ObjectIsNotParent)
        WaterConnections(WaterConnNum)%OutletNode = GetOnlySingleNode(cAlphaArgs(3), ErrorsFound, TRIM(cCurrentModuleObject), &
          cAlphaArgs(1), NodeType_Water,NodeConnectionType_Outlet, 1, ObjectIsNotParent)

        ! Check plant connections
        CALL TestCompSet(TRIM(cCurrentModuleObject),cAlphaArgs(1), cAlphaArgs(2), cAlphaArgs(3), 'DHW Nodes')
      ELSE
        ! If no plant nodes are connected, simulate in stand-alone mode.
        WaterConnections(WaterConnNum)%StandAlone = .TRUE.
      END IF

      IF (.NOT. lAlphaFieldBlanks(4) ) THEN
        CALL SetupTankDemandComponent(WaterConnections(WaterConnNum)%Name, TRIM(cCurrentModuleObject), cAlphaArgs(4), ErrorsFound, &
          WaterConnections(WaterConnNum)%SupplyTankNum, WaterConnections(WaterConnNum)%TankDemandID)
      END IF

      IF (.NOT. lAlphaFieldBlanks(5) ) THEN
        CALL SetupTankSupplyComponent(WaterConnections(WaterConnNum)%Name, TRIM(cCurrentModuleObject), cAlphaArgs(5), ErrorsFound, &
          WaterConnections(WaterConnNum)%RecoveryTankNum, WaterConnections(WaterConnNum)%TankSupplyID)
      END IF

      IF (.NOT. lAlphaFieldBlanks(6) ) THEN
        WaterConnections(WaterConnNum)%HotTempSchedule = GetScheduleIndex(cAlphaArgs(6))
        ! If no HotTempSchedule, there is no hot water.
        ! HotTempSchedule is ignored if connected to a plant loop via WATER USE CONNECTIONS

        IF (WaterConnections(WaterConnNum)%HotTempSchedule == 0) THEN
          CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(6))//'='//TRIM(cAlphaArgs(6)))
          CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
          ErrorsFound = .TRUE.
        END IF
      END IF

      IF (.NOT. lAlphaFieldBlanks(7) ) THEN
        WaterConnections(WaterConnNum)%ColdTempSchedule = GetScheduleIndex(cAlphaArgs(7))
        ! If no ColdTempSchedule, temperatures will be calculated by WATER MAINS TEMPERATURES object

        IF (WaterConnections(WaterConnNum)%ColdTempSchedule == 0) THEN
          CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(7))//'='//TRIM(cAlphaArgs(7)))
          CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
          ErrorsFound = .TRUE.
        END IF
      END IF

      IF ((.NOT. lAlphaFieldBlanks(8) ) .AND. (cAlphaArgs(8) /= 'NONE')) THEN
        WaterConnections(WaterConnNum)%HeatRecovery = .TRUE.

        SELECT CASE (cAlphaArgs(8))
          CASE ('IDEAL')
            WaterConnections(WaterConnNum)%HeatRecoveryHX = HeatRecoveryHXIdeal
          CASE ('COUNTERFLOW')
            WaterConnections(WaterConnNum)%HeatRecoveryHX = HeatRecoveryHXCounterFlow
          CASE ('CROSSFLOW')
            WaterConnections(WaterConnNum)%HeatRecoveryHX = HeatRecoveryHXCrossFlow
          CASE DEFAULT
            CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(8))//'='//TRIM(cAlphaArgs(8)))
            CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
            ErrorsFound = .TRUE.
        END SELECT

        SELECT CASE (cAlphaArgs(9))
          CASE ('PLANT')
            WaterConnections(WaterConnNum)%HeatRecoveryConfig = HeatRecoveryConfigPlant
          CASE ('EQUIPMENT')
            WaterConnections(WaterConnNum)%HeatRecoveryConfig = HeatRecoveryConfigEquipment
          CASE ('PLANTANDEQUIPMENT')
            WaterConnections(WaterConnNum)%HeatRecoveryConfig = HeatRecoveryConfigPlantAndEquip
          CASE DEFAULT
            CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(9))//'='//TRIM(cAlphaArgs(9)))
            CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
            ErrorsFound = .TRUE.
        END SELECT
      END IF

      WaterConnections(WaterConnNum)%HXUA = rNumericArgs(1)

      ALLOCATE(WaterConnections(WaterConnNum)%WaterEquipment(NumAlphas - 9))

      DO AlphaNum = 10, NumAlphas
        WaterEquipNum = FindItemInList(cAlphaArgs(AlphaNum), WaterEquipment%Name, NumWaterEquipment)

        IF (WaterEquipNum == 0) THEN
          CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(AlphaNum))//'='//TRIM(cAlphaArgs(AlphaNum)))
          CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
          ErrorsFound = .TRUE.
        ELSE
          IF (WaterEquipment(WaterEquipNum)%Connections > 0) THEN
            CALL ShowSevereError(TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1))// &
              ':  WaterUse:Equipment = '//TRIM(cAlphaArgs(AlphaNum))//' is already referenced by another object.')
            ErrorsFound = .TRUE.
          ELSE
            WaterEquipment(WaterEquipNum)%Connections = WaterConnNum

            WaterConnections(WaterConnNum)%NumWaterEquipment = WaterConnections(WaterConnNum)%NumWaterEquipment + 1
            WaterConnections(WaterConnNum)%WaterEquipment(WaterConnections(WaterConnNum)%NumWaterEquipment) = WaterEquipNum

            WaterConnections(WaterConnNum)%PeakVolFlowRate = WaterConnections(WaterConnNum)%PeakVolFlowRate &
              + WaterEquipment(WaterEquipNum)%PeakVolFlowRate ! this does not include possible multipliers
          END IF
        END IF
      END DO

    END DO ! WaterConnNum

    IF (ErrorsFound) CALL ShowFatalError('Errors found in processing input for '//TRIM(cCurrentModuleObject))

    IF (NumWaterConnections > 0) THEN
      ALLOCATE(CheckEquipName(NumWaterConnections) )
      ALLOCATE(CheckPlantLoop(NumWaterConnections) )
      CheckEquipName = .TRUE.
      CheckPlantLoop = .TRUE.
    ENDIF


  END IF

  ! determine connection's peak mass flow rates.
  IF (NumWaterConnections > 0) THEN
    DO WaterConnNum = 1, NumWaterConnections
      WaterConnections(WaterConnNum)%PeakMassFlowRate = 0.0D0
      Do WaterEquipNum = 1, WaterConnections(WaterConnNum)%NumWaterEquipment
        thisWaterEquipNum = WaterConnections(WaterConnNum)%WaterEquipment(WaterEquipNum)
        IF (WaterEquipment(thisWaterEquipNum)%zone > 0) then
          WaterConnections(WaterConnNum)%PeakMassFlowRate = WaterConnections(WaterConnNum)%PeakMassFlowRate &
           + WaterEquipment(thisWaterEquipNum)%PeakVolFlowRate * RhoH2O(InitConvTemp)                               &
            * Zone(WaterEquipment(thisWaterEquipNum)%zone)%Multiplier                                         &
            * Zone(WaterEquipment(thisWaterEquipNum)%zone)%ListMultiplier
        ELSE ! can't have multipliers
          WaterConnections(WaterConnNum)%PeakMassFlowRate = WaterConnections(WaterConnNum)%PeakMassFlowRate &
           + WaterEquipment(thisWaterEquipNum)%PeakVolFlowRate * RhoH2O(InitConvTemp)
        ENDIF
      ENDDO
      CALL RegisterPlantCompDesignFlow(WaterConnections(WaterConnNum)%InletNode, &
                                WaterConnections(WaterConnNum)%PeakMassFlowRate/RhoH2O(InitConvTemp) )
    ENDDO
  ENDIF

  ! Setup EQUIPMENT report variables (now that connections have been established)
  ! CurrentModuleObject='WaterUse:Equipment'
  DO WaterEquipNum = 1, NumWaterEquipment

    CALL SetupOutputVariable('Water Use Equipment Hot Water Mass Flow Rate [kg/s]', &
      WaterEquipment(WaterEquipNum)%HotMassFlowRate, 'System', 'Average', WaterEquipment(WaterEquipNum)%Name)

    CALL SetupOutputVariable('Water Use Equipment Cold Water Mass Flow Rate [kg/s]', &
      WaterEquipment(WaterEquipNum)%ColdMassFlowRate, 'System', 'Average', WaterEquipment(WaterEquipNum)%Name)

    CALL SetupOutputVariable('Water Use Equipment Total Mass Flow Rate [kg/s]', &
      WaterEquipment(WaterEquipNum)%TotalMassFlowRate, 'System', 'Average', WaterEquipment(WaterEquipNum)%Name)


    CALL SetupOutputVariable('Water Use Equipment Hot Water Volume Flow Rate [m3/s]', &
      WaterEquipment(WaterEquipNum)%HotVolFlowRate, 'System', 'Average', WaterEquipment(WaterEquipNum)%Name)

    CALL SetupOutputVariable('Water Use Equipment Cold Water Volume Flow Rate [m3/s]', &
      WaterEquipment(WaterEquipNum)%ColdVolFlowRate, 'System', 'Average', WaterEquipment(WaterEquipNum)%Name)

    CALL SetupOutputVariable('Water Use Equipment Total Volume Flow Rate [m3/s]', &
      WaterEquipment(WaterEquipNum)%TotalVolFlowRate, 'System', 'Average', WaterEquipment(WaterEquipNum)%Name)


    CALL SetupOutputVariable('Water Use Equipment Hot Water Volume [m3]', &
      WaterEquipment(WaterEquipNum)%HotVolume, 'System', 'Sum', WaterEquipment(WaterEquipNum)%Name)

    CALL SetupOutputVariable('Water Use Equipment Cold Water Volume [m3]', &
      WaterEquipment(WaterEquipNum)%ColdVolume, 'System', 'Sum', WaterEquipment(WaterEquipNum)%Name)

    CALL SetupOutputVariable('Water Use Equipment Total Volume [m3]', &
      WaterEquipment(WaterEquipNum)%TotalVolume, 'System', 'Sum', WaterEquipment(WaterEquipNum)%Name, &
      ResourceTypeKey='Water', EndUseKey='WATERSYSTEMS', EndUseSubKey=WaterEquipment(WaterEquipNum)%EndUseSubcatName, &
      GroupKey='Plant')
    CALL SetupOutputVariable('Water Use Equipment Mains Water Volume [m3]', &
      WaterEquipment(WaterEquipNum)%TotalVolume, 'System', 'Sum', WaterEquipment(WaterEquipNum)%Name, &
      ResourceTypeKey='MainsWater', EndUseKey='WATERSYSTEMS', EndUseSubKey=WaterEquipment(WaterEquipNum)%EndUseSubcatName, &
      GroupKey='Plant')


    CALL SetupOutputVariable('Water Use Equipment Hot Water Temperature [C]', &
      WaterEquipment(WaterEquipNum)%HotTemp, 'System', 'Average', WaterEquipment(WaterEquipNum)%Name)

    CALL SetupOutputVariable('Water Use Equipment Cold Water Temperature [C]', &
      WaterEquipment(WaterEquipNum)%ColdTemp, 'System', 'Average', WaterEquipment(WaterEquipNum)%Name)

    CALL SetupOutputVariable('Water Use Equipment Target Water Temperature [C]', &
      WaterEquipment(WaterEquipNum)%TargetTemp, 'System', 'Average', WaterEquipment(WaterEquipNum)%Name)

    CALL SetupOutputVariable('Water Use Equipment Mixed Water Temperature [C]', &
      WaterEquipment(WaterEquipNum)%MixedTemp, 'System', 'Average', WaterEquipment(WaterEquipNum)%Name)

    CALL SetupOutputVariable('Water Use Equipment Drain Water Temperature [C]', &
      WaterEquipment(WaterEquipNum)%DrainTemp, 'System', 'Average', WaterEquipment(WaterEquipNum)%Name)


    CALL SetupOutputVariable('Water Use Equipment Heating Rate [W]', &
      WaterEquipment(WaterEquipNum)%Power, 'System', 'Average', WaterEquipment(WaterEquipNum)%Name)

    IF (WaterEquipment(WaterEquipNum)%Connections == 0) THEN
      CALL SetupOutputVariable('Water Use Equipment Heating Energy [J]', &
        WaterEquipment(WaterEquipNum)%Energy, 'System', 'Sum', WaterEquipment(WaterEquipNum)%Name, &
        ResourceTypeKey='DISTRICTHEATING', EndUseKey='WATERSYSTEMS', &
        EndUseSubKey=WaterEquipment(WaterEquipNum)%EndUseSubcatName, GroupKey='Plant')

    ELSE IF (WaterConnections(WaterEquipment(WaterEquipNum)%Connections)%StandAlone) THEN
      CALL SetupOutputVariable('Water Use Equipment Heating Energy [J]', &
        WaterEquipment(WaterEquipNum)%Energy, 'System', 'Sum', WaterEquipment(WaterEquipNum)%Name, &
        ResourceTypeKey='DISTRICTHEATING', EndUseKey='WATERSYSTEMS', &
        EndUseSubKey=WaterEquipment(WaterEquipNum)%EndUseSubcatName, GroupKey='Plant')

    ELSE  ! The EQUIPMENT is coupled to a plant loop via a CONNECTIONS object
      CALL SetupOutputVariable('Water Use Equipment Heating Energy [J]', &
        WaterEquipment(WaterEquipNum)%Energy, 'System', 'Sum', WaterEquipment(WaterEquipNum)%Name, &
        ResourceTypeKey='ENERGYTRANSFER', EndUseKey='WATERSYSTEMS', &
        EndUseSubKey=WaterEquipment(WaterEquipNum)%EndUseSubcatName, GroupKey='Plant')
    END IF

    IF (WaterEquipment(WaterEquipNum)%Zone > 0) THEN
      CALL SetupOutputVariable('Water Use Equipment Zone Sensible Heat Gain Rate [W]', &
        WaterEquipment(WaterEquipNum)%SensibleRate, 'System', 'Average', WaterEquipment(WaterEquipNum)%Name)
      CALL SetupOutputVariable('Water Use Equipment Zone Sensible Heat Gain Energy [J]', &
        WaterEquipment(WaterEquipNum)%SensibleEnergy, 'System', 'Sum', WaterEquipment(WaterEquipNum)%Name)

      CALL SetupOutputVariable('Water Use Equipment Zone Latent Gain Rate [W]', &
        WaterEquipment(WaterEquipNum)%LatentRate, 'System', 'Average', WaterEquipment(WaterEquipNum)%Name)
      CALL SetupOutputVariable('Water Use Equipment Zone Latent Gain Energy [J]', &
        WaterEquipment(WaterEquipNum)%LatentEnergy, 'System', 'Sum', WaterEquipment(WaterEquipNum)%Name)

      CALL SetupOutputVariable('Water Use Equipment Zone Moisture Gain Mass Flow Rate [kg/s]', &
        WaterEquipment(WaterEquipNum)%MoistureRate, 'System', 'Average', WaterEquipment(WaterEquipNum)%Name)
      CALL SetupOutputVariable('Water Use Equipment Zone Moisture Gain Mass [kg]', &
        WaterEquipment(WaterEquipNum)%MoistureMass, 'System', 'Sum', WaterEquipment(WaterEquipNum)%Name)

      CALL SetupZoneInternalGain(WaterEquipment(WaterEquipNum)%Zone, &
                     'WaterUse:Equipment',  &
                     WaterEquipment(WaterEquipNum)%Name, &
                     IntGainTypeOf_WaterUseEquipment,    &
                     ConvectionGainRate    = WaterEquipment(WaterEquipNum)%SensibleRateNoMultiplier, &
                     LatentGainRate        = WaterEquipment(WaterEquipNum)%LatentRateNoMultiplier)

    END IF

  END DO ! WaterEquipNum


  ! Setup CONNECTIONS report variables (don't put any on meters; they are metered at WATER USE EQUIPMENT level)
  ! CurrentModuleObject='WaterUse:Connections'
  DO WaterConnNum = 1, NumWaterConnections

    CALL SetupOutputVariable('Water Use Connections Hot Water Mass Flow Rate [kg/s]', &
      WaterConnections(WaterConnNum)%HotMassFlowRate, 'System', 'Average', WaterConnections(WaterConnNum)%Name)

    CALL SetupOutputVariable('Water Use Connections Cold Water Mass Flow Rate [kg/s]', &
      WaterConnections(WaterConnNum)%ColdMassFlowRate, 'System', 'Average', WaterConnections(WaterConnNum)%Name)

    CALL SetupOutputVariable('Water Use Connections Total Mass Flow Rate [kg/s]', &
      WaterConnections(WaterConnNum)%TotalMassFlowRate, 'System', 'Average', WaterConnections(WaterConnNum)%Name)

    CALL SetupOutputVariable('Water Use Connections Drain Water Mass Flow Rate [kg/s]', &
      WaterConnections(WaterConnNum)%DrainMassFlowRate, 'System', 'Average', WaterConnections(WaterConnNum)%Name)

    CALL SetupOutputVariable('Water Use Connections Heat Recovery Mass Flow Rate [kg/s]', &
      WaterConnections(WaterConnNum)%RecoveryMassFlowRate, 'System', 'Average', WaterConnections(WaterConnNum)%Name)


    CALL SetupOutputVariable('Water Use Connections Hot Water Volume Flow Rate [m3/s]', &
      WaterConnections(WaterConnNum)%HotVolFlowRate, 'System', 'Average', WaterConnections(WaterConnNum)%Name)

    CALL SetupOutputVariable('Water Use Connections Cold Water Volume Flow Rate [m3/s]', &
      WaterConnections(WaterConnNum)%ColdVolFlowRate, 'System', 'Average', WaterConnections(WaterConnNum)%Name)

    CALL SetupOutputVariable('Water Use Connections Total Volume Flow Rate [m3/s]', &
      WaterConnections(WaterConnNum)%TotalVolFlowRate, 'System', 'Average', WaterConnections(WaterConnNum)%Name)


    CALL SetupOutputVariable('Water Use Connections Hot Water Volume [m3]', &
      WaterConnections(WaterConnNum)%HotVolume, 'System', 'Sum', WaterConnections(WaterConnNum)%Name)

    CALL SetupOutputVariable('Water Use Connections Cold Water Volume [m3]', &
      WaterConnections(WaterConnNum)%ColdVolume, 'System', 'Sum', WaterConnections(WaterConnNum)%Name)

    CALL SetupOutputVariable('Water Use Connections Total Volume [m3]', &
      WaterConnections(WaterConnNum)%TotalVolume, 'System', 'Sum', WaterConnections(WaterConnNum)%Name) !, &
     ! ResourceTypeKey='Water', EndUseKey='DHW', EndUseSubKey=EndUseSubcategoryName, GroupKey='Plant')
! tHIS WAS double counting

    CALL SetupOutputVariable('Water Use Connections Hot Water Temperature [C]', &
      WaterConnections(WaterConnNum)%HotTemp, 'System', 'Average', WaterConnections(WaterConnNum)%Name)

    CALL SetupOutputVariable('Water Use Connections Cold Water Temperature [C]', &
      WaterConnections(WaterConnNum)%ColdTemp, 'System', 'Average', WaterConnections(WaterConnNum)%Name)

    CALL SetupOutputVariable('Water Use Connections Drain Water Temperature [C]', &
      WaterConnections(WaterConnNum)%DrainTemp, 'System', 'Average', WaterConnections(WaterConnNum)%Name)

    CALL SetupOutputVariable('Water Use Connections Return Water Temperature [C]', &
      WaterConnections(WaterConnNum)%ReturnTemp, 'System', 'Average', WaterConnections(WaterConnNum)%Name)

    CALL SetupOutputVariable('Water Use Connections Waste Water Temperature [C]', &
      WaterConnections(WaterConnNum)%WasteTemp, 'System', 'Average', WaterConnections(WaterConnNum)%Name)


    CALL SetupOutputVariable('Water Use Connections Heat Recovery Water Temperature [C]', &
      WaterConnections(WaterConnNum)%RecoveryTemp, 'System', 'Average', WaterConnections(WaterConnNum)%Name)

    CALL SetupOutputVariable('Water Use Connections Heat Recovery Effectiveness []', &
      WaterConnections(WaterConnNum)%Effectiveness, 'System', 'Average', WaterConnections(WaterConnNum)%Name)

    CALL SetupOutputVariable('Water Use Connections Heat Recovery Rate [W]', &
      WaterConnections(WaterConnNum)%RecoveryRate, 'System', 'Average', WaterConnections(WaterConnNum)%Name)
    CALL SetupOutputVariable('Water Use Connections Heat Recovery Energy [J]', &
      WaterConnections(WaterConnNum)%RecoveryEnergy, 'System', 'Sum', WaterConnections(WaterConnNum)%Name)
      ! Does this go on a meter?

    ! To do:  Add report variable for starved flow when tank can't deliver?

    IF (.NOT. WaterConnections(WaterConnNum)%StandAlone) THEN
      CALL SetupOutputVariable('Water Use Connections Plant Hot Water Energy [J]', &
        WaterConnections(WaterConnNum)%Energy, 'System', 'Sum', WaterConnections(WaterConnNum)%Name, &
        ResourceTypeKey='PLANTLOOPHEATINGDEMAND', EndUseKey='WATERSYSTEMS', GroupKey='Plant')
    END IF

  END DO ! WaterConnNum

  RETURN

END SUBROUTINE GetWaterUseInput


SUBROUTINE CalcEquipmentFlowRates(WaterEquipNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Peter Graham Ellis
          !       DATE WRITTEN   August 2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Calculate desired hot and cold water flow rates

          ! METHODOLOGY EMPLOYED:
          !

          ! USE STATEMENTS:
  USE ScheduleManager, ONLY: GetCurrentScheduleValue
  USE Psychrometrics, ONLY: RhoH2O
  USE DataEnvironment, ONLY: WaterMainsTemp
  USE DataHeatBalance, ONLY: Zone

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: WaterEquipNum

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: WaterConnNum

          ! FLOW:
  WaterConnNum = WaterEquipment(WaterEquipNum)%Connections

  IF (WaterConnNum > 0) THEN
    ! Get water temperature conditions from the CONNECTIONS object
    WaterEquipment(WaterEquipNum)%ColdTemp = WaterConnections(WaterConnNum)%ColdTemp
    WaterEquipment(WaterEquipNum)%HotTemp = WaterConnections(WaterConnNum)%HotTemp

  ELSE
    ! Get water temperature conditions from the WATER USE EQUIPMENT schedules
    IF (WaterEquipment(WaterEquipNum)%ColdTempSchedule > 0) THEN
      WaterEquipment(WaterEquipNum)%ColdTemp = GetCurrentScheduleValue(WaterEquipment(WaterEquipNum)%ColdTempSchedule)
    ELSE  ! If no ColdTempSchedule, use the mains temperature
      WaterEquipment(WaterEquipNum)%ColdTemp = WaterMainsTemp
    END IF

    IF (WaterEquipment(WaterEquipNum)%HotTempSchedule > 0) THEN
      WaterEquipment(WaterEquipNum)%HotTemp = GetCurrentScheduleValue(WaterEquipment(WaterEquipNum)%HotTempSchedule)
    ELSE  ! If no HotTempSchedule, use all cold water
      WaterEquipment(WaterEquipNum)%HotTemp = WaterEquipment(WaterEquipNum)%ColdTemp
    END IF
  END IF

  IF (WaterEquipment(WaterEquipNum)%TargetTempSchedule > 0) THEN
    WaterEquipment(WaterEquipNum)%TargetTemp = GetCurrentScheduleValue(WaterEquipment(WaterEquipNum)%TargetTempSchedule)
  ELSE  ! If no TargetTempSchedule, use all hot water
    WaterEquipment(WaterEquipNum)%TargetTemp = WaterEquipment(WaterEquipNum)%HotTemp
  END IF

  ! Get the requested total flow rate
  ! 11-17-2006 BG Added multipliers in next block
  IF (WaterEquipment(WaterEquipNum)%zone > 0) THEN
    IF (WaterEquipment(WaterEquipNum)%FlowRateFracSchedule > 0) THEN
      WaterEquipment(WaterEquipNum)%TotalVolFlowRate = WaterEquipment(WaterEquipNum)%PeakVolFlowRate &
        * GetCurrentScheduleValue(WaterEquipment(WaterEquipNum)%FlowRateFracSchedule)                &
        * Zone(WaterEquipment(WaterEquipNum)%zone)%Multiplier                                        &
        * Zone(WaterEquipment(WaterEquipNum)%zone)%ListMultiplier
    ELSE
      WaterEquipment(WaterEquipNum)%TotalVolFlowRate = WaterEquipment(WaterEquipNum)%PeakVolFlowRate &
        * Zone(WaterEquipment(WaterEquipNum)%zone)%Multiplier                                        &
        * Zone(WaterEquipment(WaterEquipNum)%zone)%ListMultiplier
    END IF
  ELSE
    IF (WaterEquipment(WaterEquipNum)%FlowRateFracSchedule > 0) THEN
      WaterEquipment(WaterEquipNum)%TotalVolFlowRate = WaterEquipment(WaterEquipNum)%PeakVolFlowRate &
        * GetCurrentScheduleValue(WaterEquipment(WaterEquipNum)%FlowRateFracSchedule)
    ELSE
      WaterEquipment(WaterEquipNum)%TotalVolFlowRate = WaterEquipment(WaterEquipNum)%PeakVolFlowRate
    ENDIF
  ENDIF

  WaterEquipment(WaterEquipNum)%TotalMassFlowRate = WaterEquipment(WaterEquipNum)%TotalVolFlowRate * RhoH2O(InitConvTemp)

  ! Calculate hot and cold water mixing at the tap
  IF (WaterEquipment(WaterEquipNum)%TotalMassFlowRate > 0.0d0) THEN
    ! Calculate the flow rates needed to meet the target temperature
    IF (WaterEquipment(WaterEquipNum)%HotTemp == WaterEquipment(WaterEquipNum)%ColdTemp) THEN  ! Avoid divide by zero
      ! There is no hot water
      WaterEquipment(WaterEquipNum)%HotMassFlowRate = 0.0d0

! Need a special case for HotTemp < ColdTemp, due to bad user input  (but could happen in a plant loop accidentally)

    ELSE IF (WaterEquipment(WaterEquipNum)%TargetTemp > WaterEquipment(WaterEquipNum)%HotTemp) THEN
      WaterEquipment(WaterEquipNum)%HotMassFlowRate = WaterEquipment(WaterEquipNum)%TotalMassFlowRate

    ELSE
      WaterEquipment(WaterEquipNum)%HotMassFlowRate = WaterEquipment(WaterEquipNum)%TotalMassFlowRate &
        * (WaterEquipment(WaterEquipNum)%TargetTemp - WaterEquipment(WaterEquipNum)%ColdTemp) &
        / (WaterEquipment(WaterEquipNum)%HotTemp - WaterEquipment(WaterEquipNum)%ColdTemp)
    END IF

    IF (WaterEquipment(WaterEquipNum)%HotMassFlowRate < 0.0d0) THEN
      ! Target temp is colder than the cold water temp; don't allow colder
      WaterEquipment(WaterEquipNum)%HotMassFlowRate = 0.0d0
    END IF

    WaterEquipment(WaterEquipNum)%ColdMassFlowRate = WaterEquipment(WaterEquipNum)%TotalMassFlowRate &
      - WaterEquipment(WaterEquipNum)%HotMassFlowRate

    IF (WaterEquipment(WaterEquipNum)%ColdMassFlowRate < 0.0d0) WaterEquipment(WaterEquipNum)%ColdMassFlowRate = 0.0d0

    WaterEquipment(WaterEquipNum)%MixedTemp = &
      (WaterEquipment(WaterEquipNum)%ColdMassFlowRate * WaterEquipment(WaterEquipNum)%ColdTemp &
      + WaterEquipment(WaterEquipNum)%HotMassFlowRate * WaterEquipment(WaterEquipNum)%HotTemp) &
      / WaterEquipment(WaterEquipNum)%TotalMassFlowRate
  ELSE
    WaterEquipment(WaterEquipNum)%HotMassFlowRate = 0.0d0
    WaterEquipment(WaterEquipNum)%ColdMassFlowRate = 0.0d0
    WaterEquipment(WaterEquipNum)%MixedTemp = WaterEquipment(WaterEquipNum)%TargetTemp
  END IF

  RETURN

END SUBROUTINE CalcEquipmentFlowRates


SUBROUTINE CalcEquipmentDrainTemp(WaterEquipNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Peter Graham Ellis
          !       DATE WRITTEN   August 2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Calculate drainwater temperature and heat and moisture gains to zone.

          ! METHODOLOGY EMPLOYED:
          !

          ! USE STATEMENTS:
  USE ScheduleManager, ONLY: GetCurrentScheduleValue
  USE Psychrometrics, ONLY: CPHW, PsyWFnTdbRhPb, PsyRhoAirFnPbTdbW, PsyHfgAirFnWTdb
  USE DataHeatBalFanSys, ONLY: MAT, ZoneAirHumRat
  USE DataHeatBalance, ONLY: ZONE
  USE DataEnvironment, ONLY: OutBaroPress
  USE DataGlobals, ONLY: SecInHour
  Use DataHVACGlobals, ONLY: TimeStepSys

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: WaterEquipNum

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: ZoneNum
  REAL(r64) :: ZoneMAT
  REAL(r64) :: ZoneHumRat
  REAL(r64) :: ZoneHumRatSat
  REAL(r64) :: RhoAirDry
  REAL(r64) :: ZoneMassMax
  REAL(r64) :: FlowMassMax
  REAL(r64) :: MoistureMassMax

          ! FLOW:

  WaterEquipment(WaterEquipNum)%SensibleRate = 0.0d0
  WaterEquipment(WaterEquipNum)%SensibleEnergy = 0.0d0
  WaterEquipment(WaterEquipNum)%LatentRate = 0.0d0
  WaterEquipment(WaterEquipNum)%LatentEnergy = 0.0d0

  IF ((WaterEquipment(WaterEquipNum)%Zone == 0) .OR. (WaterEquipment(WaterEquipNum)%TotalMassFlowRate == 0.0d0)) THEN
    WaterEquipment(WaterEquipNum)%DrainTemp = WaterEquipment(WaterEquipNum)%MixedTemp
    WaterEquipment(WaterEquipNum)%DrainMassFlowRate = WaterEquipment(WaterEquipNum)%TotalMassFlowRate

  ELSE
    ZoneNum = WaterEquipment(WaterEquipNum)%Zone
    ZoneMAT = MAT(ZoneNum)

    IF (WaterEquipment(WaterEquipNum)%SensibleFracSchedule == 0) THEN
      WaterEquipment(WaterEquipNum)%SensibleRate = 0.0d0
      WaterEquipment(WaterEquipNum)%SensibleEnergy = 0.0d0
    ELSE
      WaterEquipment(WaterEquipNum)%SensibleRate = GetCurrentScheduleValue(WaterEquipment(WaterEquipNum)%SensibleFracSchedule) &
        * WaterEquipment(WaterEquipNum)%TotalMassFlowRate * CPHW(InitConvTemp) * (WaterEquipment(WaterEquipNum)%MixedTemp &
        - ZoneMAT)
      WaterEquipment(WaterEquipNum)%SensibleEnergy = WaterEquipment(WaterEquipNum)%SensibleRate * TimeStepSys * SecInHour
    END IF

    IF (WaterEquipment(WaterEquipNum)%LatentFracSchedule == 0) THEN
      WaterEquipment(WaterEquipNum)%LatentRate = 0.0d0
      WaterEquipment(WaterEquipNum)%LatentEnergy = 0.0d0
    ELSE
      ZoneHumRat = ZoneAirHumRat(ZoneNum)
      ZoneHumRatSat = PsyWFnTdbRhPb(ZoneMAT, 1.0d0, OutBaroPress, 'CalcEquipmentDrainTemp')  ! Humidratio at 100% relative humidity
      RhoAirDry = PsyRhoAirFnPbTdbW(OutBaroPress, ZoneMAT, 0.0d0)

      ZoneMassMax = (ZoneHumRatSat - ZoneHumRat) * RhoAirDry * Zone(ZoneNum)%Volume  ! Max water that can be evaporated to zone
      FlowMassMax = WaterEquipment(WaterEquipNum)%TotalMassFlowRate * TimeStepSys * SecInHour  ! Max water in flow
      MoistureMassMax = MIN(ZoneMassMax, FlowMassMax)

      WaterEquipment(WaterEquipNum)%MoistureMass = GetCurrentScheduleValue(WaterEquipment(WaterEquipNum)%LatentFracSchedule) &
        * MoistureMassMax
      WaterEquipment(WaterEquipNum)%MoistureRate = WaterEquipment(WaterEquipNum)%MoistureMass / (TimeStepSys * SecInHour)

      WaterEquipment(WaterEquipNum)%LatentRate = WaterEquipment(WaterEquipNum)%MoistureRate * PsyHfgAirFnWTdb(ZoneHumRat, ZoneMAT)
      WaterEquipment(WaterEquipNum)%LatentEnergy = WaterEquipment(WaterEquipNum)%LatentRate * TimeStepSys * SecInHour
    END IF

    WaterEquipment(WaterEquipNum)%DrainMassFlowRate = WaterEquipment(WaterEquipNum)%TotalMassFlowRate &
      - WaterEquipment(WaterEquipNum)%MoistureRate

    IF (WaterEquipment(WaterEquipNum)%DrainMassFlowRate == 0.0d0) THEN
      WaterEquipment(WaterEquipNum)%DrainTemp = WaterEquipment(WaterEquipNum)%MixedTemp
    ELSE
      WaterEquipment(WaterEquipNum)%DrainTemp = (WaterEquipment(WaterEquipNum)%TotalMassFlowRate * CPHW(InitConvTemp) &
        * WaterEquipment(WaterEquipNum)%MixedTemp - WaterEquipment(WaterEquipNum)%SensibleRate &
        - WaterEquipment(WaterEquipNum)%LatentRate) / (WaterEquipment(WaterEquipNum)%DrainMassFlowRate * CPHW(InitConvTemp))
    END IF
  END IF

  RETURN

END SUBROUTINE CalcEquipmentDrainTemp


SUBROUTINE InitConnections(WaterConnNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Peter Graham Ellis
          !       DATE WRITTEN   August 2006
          !       MODIFIED       Brent Griffith 2010, demand side update
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !

          ! METHODOLOGY EMPLOYED:
          !

          ! USE STATEMENTS:
  USE DataGlobals, ONLY: SysSizingCalc, DoingSizing
  USE ScheduleManager, ONLY: GetCurrentScheduleValue
  USE DataLoopNode, ONLY: Node
  USE DataEnvironment, ONLY: WaterMainsTemp
  USE DataWater,   ONLY: WaterStorage
  USE DataHeatBalance, ONLY: Zone
  USE DataPlant,       ONLY: PlantLoop, ScanPlantLoopsForObject, TypeOf_WaterUseConnection
  USE PlantUtilities,  ONLY: InitComponentNodes

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: WaterConnNum

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: InletNode
  INTEGER :: OutletNode
  LOGICAL, SAVE                            :: MyOneTimeFlag = .TRUE. ! one time flag                    !DSU
  LOGICAL, ALLOCATABLE, SAVE, DIMENSION(:) :: SetLoopIndexFlag       ! get loop number flag             !DSU
  LOGICAL :: errFlag

  IF (MyOneTimeFlag) THEN                                                                               !DSU
    ALLOCATE(SetLoopIndexFlag(NumWaterConnections))
    SetLoopIndexFlag = .TRUE.                                                                           !DSU
    MyOneTimeFlag    = .FALSE.                                                                          !DSU
  END IF                                                                                                !DSU

  IF(SetLoopIndexFlag(WaterConnNum))THEN                                                                !DSU
    IF(ALLOCATED(PlantLoop) .and. .NOT. WaterConnections(WaterConnNum)%StandAlone) THEN                 !DSU
      errFlag=.false.
      CALL ScanPlantLoopsForObject(WaterConnections(WaterConnNum)%Name, &                               !DSU
                                   TypeOf_WaterUseConnection, &                                         !DSU
                                   WaterConnections(WaterConnNum)%PlantLoopNum, &                       !DSU
                                   WaterConnections(WaterConnNum)%PlantLoopSide, &                      !DSU
                                   WaterConnections(WaterConnNum)%PlantLoopBranchNum, &                 !DSU
                                   WaterConnections(WaterConnNum)%PlantLoopCompNum,  &                  !DSU
                                   errFlag=errFlag)                                                     !DSU
      IF (errFlag) THEN                                                                                 !DSU
        CALL ShowFatalError('InitConnections: Program terminated due to previous condition(s).')        !DSU
      ENDIF                                                                                             !DSU
      SetLoopIndexFlag(WaterConnNum) = .FALSE.                                                          !DSU
    ENDIF                                                                                               !DSU
    IF (WaterConnections(WaterConnNum)%StandAlone) SetLoopIndexFlag(WaterConnNum) = .FALSE.
  ENDIF

  ! Set the cold water temperature
  IF (WaterConnections(WaterConnNum)%SupplyTankNum > 0) THEN
    WaterConnections(WaterConnNum)%ColdSupplyTemp = WaterStorage(WaterConnections(WaterConnNum)%SupplyTankNum)%Twater

  ELSE IF (WaterConnections(WaterConnNum)%ColdTempSchedule > 0) THEN
    WaterConnections(WaterConnNum)%ColdSupplyTemp = GetCurrentScheduleValue(WaterConnections(WaterConnNum)%ColdTempSchedule)

  ELSE
    WaterConnections(WaterConnNum)%ColdSupplyTemp = WaterMainsTemp
  END IF

  ! Initially set ColdTemp to the ColdSupplyTemp; with heat recovery, ColdTemp will change during iteration
  WaterConnections(WaterConnNum)%ColdTemp = WaterConnections(WaterConnNum)%ColdSupplyTemp

  ! Set the hot water temperature
  IF (WaterConnections(WaterConnNum)%StandAlone) THEN
    IF (WaterConnections(WaterConnNum)%HotTempSchedule > 0) THEN
      WaterConnections(WaterConnNum)%HotTemp = GetCurrentScheduleValue(WaterConnections(WaterConnNum)%HotTempSchedule)
    ELSE
      ! If no HotTempSchedule, use all cold water
      WaterConnections(WaterConnNum)%HotTemp = WaterConnections(WaterConnNum)%ColdTemp
    END IF

  ELSE
    InletNode  = WaterConnections(WaterConnNum)%InletNode
    OutletNode = WaterConnections(WaterConnNum)%OutletNode

    IF (BeginEnvrnFlag .AND. WaterConnections(WaterConnNum)%Init) THEN
      ! Clear node initial conditions
      IF (InletNode > 0 .AND. OutletNode > 0) THEN
        CALL InitComponentNodes(0.d0,WaterConnections(WaterConnNum)%PeakMassFlowRate, &
                                 InletNode,       &
                                 OutletNode,       &
                                 WaterConnections(WaterConnNum)%PlantLoopNum,         &
                                 WaterConnections(WaterConnNum)%PlantLoopSide,        &
                                 WaterConnections(WaterConnNum)%PlantLoopBranchNum,   &
                                 WaterConnections(WaterConnNum)%PlantLoopCompNum)

        WaterConnections(WaterConnNum)%ReturnTemp  = Node(InletNode)%Temp
      END IF

      WaterConnections(WaterConnNum)%Init = .FALSE.
    END IF

    IF (.NOT. BeginEnvrnFlag) WaterConnections(WaterConnNum)%Init = .TRUE.

    IF (InletNode > 0) THEN
      If (.not. DoingSizing) THEN
        WaterConnections(WaterConnNum)%HotTemp = Node(InletNode)%Temp
      ELSE
        ! plant loop will not be running so need a value here.
        ! should change to use tank setpoint but water use connections don't have knowledge of the tank they are fed by
        WaterConnections(WaterConnNum)%HotTemp = 60.0d0  !
      ENDIF
    END IF
  END IF

  RETURN

END SUBROUTINE InitConnections


SUBROUTINE CalcConnectionsFlowRates(WaterConnNum, FirstHVACIteration)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Peter Graham Ellis
          !       DATE WRITTEN   August 2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Calculate summed values for WATER USE CONNECTIONS (to prepare to request flow from plant, and for reporting).

          ! METHODOLOGY EMPLOYED:
          !

          ! USE STATEMENTS:
  USE DataLoopNode,   ONLY : Node
  USE Psychrometrics, ONLY : RhoH2O
  USE DataWater,      ONLY : WaterStorage
  USE PlantUtilities, ONLY : SetComponentFlowRate

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: WaterConnNum
  LOGICAL, INTENT(IN) :: FirstHVACIteration

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: WaterEquipNum, Loop
  INTEGER :: InletNode
  INTEGER :: OutletNode
  INTEGER :: LoopNum
  INTEGER :: LoopSideNum
  REAL(r64) :: AvailableFraction
  REAL(r64) :: DesiredHotWaterMassFlow ! store original request

          ! FLOW:
  WaterConnections(WaterConnNum)%ColdMassFlowRate = 0.0d0
  WaterConnections(WaterConnNum)%HotMassFlowRate = 0.0d0

  DO Loop = 1, WaterConnections(WaterConnNum)%NumWaterEquipment
    WaterEquipNum = WaterConnections(WaterConnNum)%WaterEquipment(Loop)

    CALL CalcEquipmentFlowRates(WaterEquipNum)

    WaterConnections(WaterConnNum)%ColdMassFlowRate = WaterConnections(WaterConnNum)%ColdMassFlowRate &
      + WaterEquipment(WaterEquipNum)%ColdMassFlowRate
    WaterConnections(WaterConnNum)%HotMassFlowRate = WaterConnections(WaterConnNum)%HotMassFlowRate &
      + WaterEquipment(WaterEquipNum)%HotMassFlowRate
  END DO  ! Loop

  WaterConnections(WaterConnNum)%TotalMassFlowRate = WaterConnections(WaterConnNum)%ColdMassFlowRate &
    + WaterConnections(WaterConnNum)%HotMassFlowRate


  IF (.NOT. WaterConnections(WaterConnNum)%StandAlone) THEN  ! Interact with the plant loop
    InletNode  = WaterConnections(WaterConnNum)%InletNode
    OutletNode = WaterConnections(WaterConnNum)%OutletNode
    LoopNum    = WaterConnections(WaterConnNum)%PlantLoopNum
    LoopSideNum = WaterConnections(WaterConnNum)%PlantLoopSide
    IF (InletNode > 0) THEN
      IF (FirstHVACIteration) THEN
        ! Request the mass flow rate from the demand side manager
!        Node(InletNode)%MassFlowRate = WaterConnections(WaterConnNum)%HotMassFlowRate
!        Node(InletNode)%MassFlowRateMaxAvail = WaterConnections(WaterConnNum)%PeakMassFlowRate
!        Node(InletNode)%MassFlowRateMinAvail = 0.0D0
        CALL  SetComponentFlowRate(WaterConnections(WaterConnNum)%HotMassFlowRate,InletNode,OutletNode,LoopNum,LoopSideNum, &
                                   WaterConnections(WaterConnNum)%PlantLoopBranchNum, &
                                   WaterConnections(WaterConnNum)%PlantLoopCompNum)

      ELSE
        DesiredHotWaterMassFlow = WaterConnections(WaterConnNum)%HotMassFlowRate
        CALL  SetComponentFlowRate(DesiredHotWaterMassFlow,InletNode,OutletNode,LoopNum,LoopSideNum, &
                                   WaterConnections(WaterConnNum)%PlantLoopBranchNum, &
                                   WaterConnections(WaterConnNum)%PlantLoopCompNum)
!DSU3   Node(InletNode)%MassFlowRate = Min(WaterConnections(WaterConnNum)%HotMassFlowRate, Node(InletNode)%MassFlowRateMaxAvail)
!DSU3   Node(InletNode)%MassFlowRate = Max(WaterConnections(WaterConnNum)%HotMassFlowRate, Node(InletNode)%MassFlowRateMinAvail)
        ! readjust if more than actual available mass flow rate determined by the demand side manager
        IF ((WaterConnections(WaterConnNum)%HotMassFlowRate /= DesiredHotWaterMassFlow) &
          .AND. (WaterConnections(WaterConnNum)%HotMassFlowRate > 0.0d0)) THEN ! plant didn't give what was asked for

       !DSU3   Node(InletNode)%MassFlowRate = Node(InletNode)%MassFlowRateMaxAvail

          AvailableFraction = DesiredHotWaterMassFlow / WaterConnections(WaterConnNum)%HotMassFlowRate

      !DSU3    WaterConnections(WaterConnNum)%HotMassFlowRate = Node(InletNode)%MassFlowRateMaxAvail
          WaterConnections(WaterConnNum)%ColdMassFlowRate = WaterConnections(WaterConnNum)%TotalMassFlowRate &
            - WaterConnections(WaterConnNum)%HotMassFlowRate  ! Preserve the total mass flow rate

          ! Proportionally reduce hot water and increase cold water for all WATER USE EQUIPMENT
          DO Loop = 1, WaterConnections(WaterConnNum)%NumWaterEquipment
            WaterEquipNum = WaterConnections(WaterConnNum)%WaterEquipment(Loop)

            ! Recalculate flow rates for water equipment within connection
            WaterEquipment(WaterEquipNum)%HotMassFlowRate = AvailableFraction * WaterEquipment(WaterEquipNum)%HotMassFlowRate
            WaterEquipment(WaterEquipNum)%ColdMassFlowRate = WaterEquipment(WaterEquipNum)%TotalMassFlowRate &
              - WaterEquipment(WaterEquipNum)%HotMassFlowRate

            ! Recalculate mixed water temperature
            IF (WaterEquipment(WaterEquipNum)%TotalMassFlowRate > 0.0d0) THEN
              WaterEquipment(WaterEquipNum)%MixedTemp = &
                (WaterEquipment(WaterEquipNum)%ColdMassFlowRate * WaterEquipment(WaterEquipNum)%ColdTemp &
                + WaterEquipment(WaterEquipNum)%HotMassFlowRate * WaterEquipment(WaterEquipNum)%HotTemp) &
                / WaterEquipment(WaterEquipNum)%TotalMassFlowRate
            ELSE
              WaterEquipment(WaterEquipNum)%MixedTemp = WaterEquipment(WaterEquipNum)%TargetTemp
            END IF
          END DO  ! Loop
        END IF
      END IF
    END IF
  END IF

  IF (WaterConnections(WaterConnNum)%SupplyTankNum > 0) THEN
    ! Set the demand request for supply water from water storage tank
    WaterConnections(WaterConnNum)%ColdVolFlowRate = WaterConnections(WaterConnNum)%ColdMassFlowRate / RhoH2O(InitConvTemp)
    WaterStorage(WaterConnections(WaterConnNum)%SupplyTankNum)%VdotRequestDemand(WaterConnections(WaterConnNum)%TankDemandID) = &
      WaterConnections(WaterConnNum)%ColdVolFlowRate

    ! Check if cold flow rate should be starved by restricted flow from tank
    ! Currently, the tank flow is not really starved--water continues to flow at the tank water temperature
    ! But the user can see the error by comparing report variables for TankVolFlowRate < ColdVolFlowRate
    WaterConnections(WaterConnNum)%TankVolFlowRate = &
      WaterStorage(WaterConnections(WaterConnNum)%SupplyTankNum)%VdotAvailDemand(WaterConnections(WaterConnNum)%TankDemandID)
    WaterConnections(WaterConnNum)%TankMassFlowRate = WaterConnections(WaterConnNum)%TankVolFlowRate * RhoH2O(InitConvTemp)
  END IF

  RETURN

END SUBROUTINE CalcConnectionsFlowRates


SUBROUTINE CalcConnectionsDrainTemp(WaterConnNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Peter Graham Ellis
          !       DATE WRITTEN   August 2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Calculate

          ! METHODOLOGY EMPLOYED:
          !

          ! USE STATEMENTS:
  USE Psychrometrics, ONLY: RhoH2O

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: WaterConnNum

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: WaterEquipNum, Loop
  REAL(r64) :: MassFlowTempSum

          ! FLOW:
  WaterConnections(WaterConnNum)%DrainMassFlowRate = 0.0d0
  MassFlowTempSum = 0.0d0

  DO Loop = 1, WaterConnections(WaterConnNum)%NumWaterEquipment
    WaterEquipNum = WaterConnections(WaterConnNum)%WaterEquipment(Loop)

    CALL CalcEquipmentDrainTemp(WaterEquipNum)

    WaterConnections(WaterConnNum)%DrainMassFlowRate = WaterConnections(WaterConnNum)%DrainMassFlowRate &
      + WaterEquipment(WaterEquipNum)%DrainMassFlowRate
    MassFlowTempSum = MassFlowTempSum + WaterEquipment(WaterEquipNum)%DrainMassFlowRate * WaterEquipment(WaterEquipNum)%DrainTemp
  END DO  ! Loop

  IF (WaterConnections(WaterConnNum)%DrainMassFlowRate > 0.0d0) THEN
    WaterConnections(WaterConnNum)%DrainTemp = MassFlowTempSum / WaterConnections(WaterConnNum)%DrainMassFlowRate
  ELSE
    WaterConnections(WaterConnNum)%DrainTemp = WaterConnections(WaterConnNum)%HotTemp
  END IF

  WaterConnections(WaterConnNum)%DrainVolFlowRate = WaterConnections(WaterConnNum)%DrainMassFlowRate * RhoH2O(InitConvTemp)

  RETURN

END SUBROUTINE CalcConnectionsDrainTemp


SUBROUTINE CalcConnectionsHeatRecovery(WaterConnNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Peter Graham Ellis
          !       DATE WRITTEN   August 2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Calculate drainwater heat recovery

          ! METHODOLOGY EMPLOYED:
          !

          ! USE STATEMENTS:
  USE Psychrometrics, ONLY: CPHW
!unused0909  USE DataEnvironment, ONLY: WaterMainsTemp
  USE DataWater,   ONLY: WaterStorage

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: WaterConnNum

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64) :: CapacityRatio, NTU, ExpVal, HXCapacityRate, DrainCapacityRate, MinCapacityRate

          ! FLOW:
  IF (.NOT. WaterConnections(WaterConnNum)%HeatRecovery) THEN
    WaterConnections(WaterConnNum)%RecoveryTemp = WaterConnections(WaterConnNum)%ColdSupplyTemp
    WaterConnections(WaterConnNum)%ReturnTemp = WaterConnections(WaterConnNum)%ColdSupplyTemp
    WaterConnections(WaterConnNum)%WasteTemp = WaterConnections(WaterConnNum)%DrainTemp

  ELSE IF (WaterConnections(WaterConnNum)%TotalMassFlowRate == 0.0d0) THEN
    WaterConnections(WaterConnNum)%Effectiveness = 0.0d0
    WaterConnections(WaterConnNum)%RecoveryRate = 0.0d0
    WaterConnections(WaterConnNum)%RecoveryTemp = WaterConnections(WaterConnNum)%ColdSupplyTemp
    WaterConnections(WaterConnNum)%ReturnTemp = WaterConnections(WaterConnNum)%ColdSupplyTemp
    WaterConnections(WaterConnNum)%WasteTemp = WaterConnections(WaterConnNum)%DrainTemp

  ELSE  ! WaterConnections(WaterConnNum)%TotalMassFlowRate > 0.0

    SELECT CASE (WaterConnections(WaterConnNum)%HeatRecoveryConfig)
      CASE (HeatRecoveryConfigPlant)
        WaterConnections(WaterConnNum)%RecoveryMassFlowRate = WaterConnections(WaterConnNum)%HotMassFlowRate
      CASE (HeatRecoveryConfigEquipment)
        WaterConnections(WaterConnNum)%RecoveryMassFlowRate = WaterConnections(WaterConnNum)%ColdMassFlowRate
      CASE (HeatRecoveryConfigPlantAndEquip)
        WaterConnections(WaterConnNum)%RecoveryMassFlowRate = WaterConnections(WaterConnNum)%TotalMassFlowRate
    END SELECT

    HXCapacityRate = CPHW(InitConvTemp) * WaterConnections(WaterConnNum)%RecoveryMassFlowRate
    DrainCapacityRate = CPHW(InitConvTemp) * WaterConnections(WaterConnNum)%DrainMassFlowRate
    MinCapacityRate = MIN(DrainCapacityRate, HXCapacityRate)

    SELECT CASE (WaterConnections(WaterConnNum)%HeatRecoveryHX)
      CASE (HeatRecoveryHXIdeal)
        WaterConnections(WaterConnNum)%Effectiveness = 1.0d0

      CASE (HeatRecoveryHXCounterFlow)  ! Unmixed
        CapacityRatio = MinCapacityRate / MAX(DrainCapacityRate, HXCapacityRate)
        NTU = WaterConnections(WaterConnNum)%HXUA / MinCapacityRate
        IF (CapacityRatio == 1.0d0) THEN
          WaterConnections(WaterConnNum)%Effectiveness = NTU / (1.0d0 + NTU)
        ELSE
          ExpVal = EXP(-NTU * (1.0d0 - CapacityRatio))
          WaterConnections(WaterConnNum)%Effectiveness = (1.0d0 - ExpVal) / (1.0d0 - CapacityRatio * ExpVal)
        END IF

      CASE (HeatRecoveryHXCrossFlow)  ! Unmixed
        CapacityRatio = MinCapacityRate / MAX(DrainCapacityRate, HXCapacityRate)
        NTU = WaterConnections(WaterConnNum)%HXUA / MinCapacityRate
        WaterConnections(WaterConnNum)%Effectiveness = 1.0d0 - EXP((NTU ** 0.22d0 / CapacityRatio) * &
          (EXP(-CapacityRatio * NTU ** 0.78d0) - 1.0d0))
    END SELECT

    WaterConnections(WaterConnNum)%RecoveryRate = WaterConnections(WaterConnNum)%Effectiveness * MinCapacityRate * &
      (WaterConnections(WaterConnNum)%DrainTemp - WaterConnections(WaterConnNum)%ColdSupplyTemp)

    WaterConnections(WaterConnNum)%RecoveryTemp = WaterConnections(WaterConnNum)%ColdSupplyTemp + &
      WaterConnections(WaterConnNum)%RecoveryRate / (CPHW(InitConvTemp) * WaterConnections(WaterConnNum)%TotalMassFlowRate)

    WaterConnections(WaterConnNum)%WasteTemp = WaterConnections(WaterConnNum)%DrainTemp - &
      WaterConnections(WaterConnNum)%RecoveryRate / (CPHW(InitConvTemp) * WaterConnections(WaterConnNum)%TotalMassFlowRate)

    IF (WaterConnections(WaterConnNum)%RecoveryTankNum > 0) THEN
      WaterStorage(WaterConnections(WaterConnNum)%RecoveryTankNum)%VdotAvailSupply(WaterConnections(WaterConnNum)%TankSupplyID) &
        = WaterConnections(WaterConnNum)%DrainVolFlowRate
      WaterStorage(WaterConnections(WaterConnNum)%RecoveryTankNum)%TwaterSupply(WaterConnections(WaterConnNum)%TankSupplyID) &
        = WaterConnections(WaterConnNum)%WasteTemp
    END IF

    SELECT CASE (WaterConnections(WaterConnNum)%HeatRecoveryConfig)
      CASE (HeatRecoveryConfigPlant)
        WaterConnections(WaterConnNum)%TempError = 0.0d0  ! No feedback back to the cold supply
        !WaterConnections(WaterConnNum)%ColdTemp = WaterConnections(WaterConnNum)%ColdSupplyTemp
        WaterConnections(WaterConnNum)%ReturnTemp = WaterConnections(WaterConnNum)%RecoveryTemp

      CASE (HeatRecoveryConfigEquipment)
        WaterConnections(WaterConnNum)%TempError = ABS(WaterConnections(WaterConnNum)%ColdTemp &
          - WaterConnections(WaterConnNum)%RecoveryTemp)

        WaterConnections(WaterConnNum)%ColdTemp = WaterConnections(WaterConnNum)%RecoveryTemp
        WaterConnections(WaterConnNum)%ReturnTemp = WaterConnections(WaterConnNum)%ColdSupplyTemp

      CASE (HeatRecoveryConfigPlantAndEquip)
        WaterConnections(WaterConnNum)%TempError = ABS(WaterConnections(WaterConnNum)%ColdTemp &
          - WaterConnections(WaterConnNum)%RecoveryTemp)

        WaterConnections(WaterConnNum)%ColdTemp = WaterConnections(WaterConnNum)%RecoveryTemp
        WaterConnections(WaterConnNum)%ReturnTemp = WaterConnections(WaterConnNum)%RecoveryTemp
    END SELECT
  END IF

  RETURN

END SUBROUTINE CalcConnectionsHeatRecovery


SUBROUTINE UpdateWaterConnections(WaterConnNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Peter Graham Ellis
          !       DATE WRITTEN   August 2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Updates the node variables with local variables.

          ! METHODOLOGY EMPLOYED:
          ! Standard EnergyPlus methodology.

          ! USE STATEMENTS:
  USE DataLoopNode, ONLY: Node
  USE PlantUtilities, ONLY: SafeCopyPlantNode

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: WaterConnNum

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: InletNode
  INTEGER :: OutletNode
  INTEGER :: LoopNum

          ! FLOW:
  InletNode  = WaterConnections(WaterConnNum)%InletNode
  OutletNode = WaterConnections(WaterConnNum)%OutletNode
  LoopNum    = WaterConnections(WaterConnNum)%PlantLoopNum

  IF (InletNode > 0 .AND. OutletNode > 0) THEN
    ! Pass all variables from inlet to outlet node
    CALL SafeCopyPlantNode( InletNode, OutletNode, LoopNum )
   ! DSU3 Node(OutletNode) = Node(InletNode)

    ! Set outlet node variables that are possibly changed
    Node(OutletNode)%Temp = WaterConnections(WaterConnNum)%ReturnTemp
    ! should add enthalpy update to return?
  END IF

  RETURN

END SUBROUTINE UpdateWaterConnections

SUBROUTINE ReportStandAloneWaterUse

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B. Griffith, Peter Graham Ellis
          !       DATE WRITTEN   Nov. 2011
          !       MODIFIED       Brent Griffith, March 2010 added argument
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Calculates report variables for stand alone water use

          ! METHODOLOGY EMPLOYED:
          ! Standard EnergyPlus methodology.

          ! USE STATEMENTS:
  USE DataGlobals, ONLY: SecInHour
  Use DataHVACGlobals, ONLY: TimeStepSys
  USE Psychrometrics, ONLY: RhoH2O, CPHW

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:


  INTEGER :: WaterEquipNum


          ! FLOW:
  DO WaterEquipNum = 1, NumWaterEquipment
    WaterEquipment(WaterEquipNum)%ColdVolFlowRate = WaterEquipment(WaterEquipNum)%ColdMassFlowRate / RhoH2O(InitConvTemp)
    WaterEquipment(WaterEquipNum)%HotVolFlowRate = WaterEquipment(WaterEquipNum)%HotMassFlowRate / RhoH2O(InitConvTemp)
    WaterEquipment(WaterEquipNum)%TotalVolFlowRate = WaterEquipment(WaterEquipNum)%ColdVolFlowRate &
      + WaterEquipment(WaterEquipNum)%HotVolFlowRate

    WaterEquipment(WaterEquipNum)%ColdVolume = WaterEquipment(WaterEquipNum)%ColdVolFlowRate * TimeStepSys * SecInHour
    WaterEquipment(WaterEquipNum)%HotVolume = WaterEquipment(WaterEquipNum)%HotVolFlowRate * TimeStepSys * SecInHour
    WaterEquipment(WaterEquipNum)%TotalVolume = WaterEquipment(WaterEquipNum)%TotalVolFlowRate * TimeStepSys * SecInHour

    IF (WaterEquipment(WaterEquipNum)%Connections == 0) THEN
      WaterEquipment(WaterEquipNum)%Power = WaterEquipment(WaterEquipNum)%HotMassFlowRate * CPHW(InitConvTemp) &
        * (WaterEquipment(WaterEquipNum)%HotTemp - WaterEquipment(WaterEquipNum)%ColdTemp)
    ELSE
      WaterEquipment(WaterEquipNum)%Power = WaterEquipment(WaterEquipNum)%HotMassFlowRate * CPHW(InitConvTemp) &
        * (WaterEquipment(WaterEquipNum)%HotTemp - WaterConnections(WaterEquipment(WaterEquipNum)%Connections)%ReturnTemp)
    END IF

    WaterEquipment(WaterEquipNum)%Energy = WaterEquipment(WaterEquipNum)%Power * TimeStepSys * SecInHour
  END DO

  RETURN

END SUBROUTINE ReportStandAloneWaterUse

SUBROUTINE ReportWaterUse(WaterConnNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Peter Graham Ellis
          !       DATE WRITTEN   August 2006
          !       MODIFIED       Brent Griffith, March 2010 added argument
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Calculates report variables.

          ! METHODOLOGY EMPLOYED:
          ! Standard EnergyPlus methodology.

          ! USE STATEMENTS:
  USE DataGlobals, ONLY: SecInHour
  Use DataHVACGlobals, ONLY: TimeStepSys
  USE Psychrometrics, ONLY: RhoH2O, CPHW

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: WaterConnNum

  INTEGER :: Loop
  INTEGER :: WaterEquipNum


          ! FLOW:
  DO Loop = 1, WaterConnections(WaterConnNum)%NumWaterEquipment
    WaterEquipNum  = WaterConnections(WaterConnNum)%WaterEquipment(Loop)
    WaterEquipment(WaterEquipNum)%ColdVolFlowRate = WaterEquipment(WaterEquipNum)%ColdMassFlowRate / RhoH2O(InitConvTemp)
    WaterEquipment(WaterEquipNum)%HotVolFlowRate = WaterEquipment(WaterEquipNum)%HotMassFlowRate / RhoH2O(InitConvTemp)
    WaterEquipment(WaterEquipNum)%TotalVolFlowRate = WaterEquipment(WaterEquipNum)%ColdVolFlowRate &
      + WaterEquipment(WaterEquipNum)%HotVolFlowRate

    WaterEquipment(WaterEquipNum)%ColdVolume = WaterEquipment(WaterEquipNum)%ColdVolFlowRate * TimeStepSys * SecInHour
    WaterEquipment(WaterEquipNum)%HotVolume = WaterEquipment(WaterEquipNum)%HotVolFlowRate * TimeStepSys * SecInHour
    WaterEquipment(WaterEquipNum)%TotalVolume = WaterEquipment(WaterEquipNum)%TotalVolFlowRate * TimeStepSys * SecInHour

    IF (WaterEquipment(WaterEquipNum)%Connections == 0) THEN
      WaterEquipment(WaterEquipNum)%Power = WaterEquipment(WaterEquipNum)%HotMassFlowRate * CPHW(InitConvTemp) &
        * (WaterEquipment(WaterEquipNum)%HotTemp - WaterEquipment(WaterEquipNum)%ColdTemp)
    ELSE
      WaterEquipment(WaterEquipNum)%Power = WaterEquipment(WaterEquipNum)%HotMassFlowRate * CPHW(InitConvTemp) &
        * (WaterEquipment(WaterEquipNum)%HotTemp - WaterConnections(WaterEquipment(WaterEquipNum)%Connections)%ReturnTemp)
    END IF

    WaterEquipment(WaterEquipNum)%Energy = WaterEquipment(WaterEquipNum)%Power * TimeStepSys * SecInHour
  END DO


  WaterConnections(WaterConnNum)%ColdVolFlowRate = WaterConnections(WaterConnNum)%ColdMassFlowRate / RhoH2O(InitConvTemp)
  WaterConnections(WaterConnNum)%HotVolFlowRate = WaterConnections(WaterConnNum)%HotMassFlowRate / RhoH2O(InitConvTemp)
  WaterConnections(WaterConnNum)%TotalVolFlowRate = WaterConnections(WaterConnNum)%ColdVolFlowRate &
    + WaterConnections(WaterConnNum)%HotVolFlowRate

  WaterConnections(WaterConnNum)%ColdVolume = WaterConnections(WaterConnNum)%ColdVolFlowRate * TimeStepSys * SecInHour
  WaterConnections(WaterConnNum)%HotVolume = WaterConnections(WaterConnNum)%HotVolFlowRate * TimeStepSys * SecInHour
  WaterConnections(WaterConnNum)%TotalVolume = WaterConnections(WaterConnNum)%TotalVolFlowRate * TimeStepSys * SecInHour

  WaterConnections(WaterConnNum)%Power = WaterConnections(WaterConnNum)%HotMassFlowRate * CPHW(InitConvTemp) &
    * (WaterConnections(WaterConnNum)%HotTemp - WaterConnections(WaterConnNum)%ReturnTemp)
  WaterConnections(WaterConnNum)%Energy = WaterConnections(WaterConnNum)%Power * TimeStepSys * SecInHour

  WaterConnections(WaterConnNum)%RecoveryEnergy = WaterConnections(WaterConnNum)%RecoveryRate * TimeStepSys * SecInHour


  RETURN

END SUBROUTINE ReportWaterUse


SUBROUTINE CalcWaterUseZoneGains

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Peter Graham Ellis
          !       DATE WRITTEN   August 2006
          !       MODIFIED
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Calculates the zone internal gains due to water use sensible and latent loads.

          ! METHODOLOGY EMPLOYED:
          !

          ! USE STATEMENTS:
  USE DataGlobals, ONLY: BeginEnvrnFlag
  USE DataHeatBalance, ONLY: ZoneIntGain, Zone

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: WaterEquipNum
  INTEGER :: ZoneNum
  LOGICAL, SAVE :: MyEnvrnFlag=.true.

          ! FLOW:
  IF (NumWaterEquipment == 0) RETURN

  IF (BeginEnvrnFlag .and. MyEnvrnFlag) THEN
    WaterEquipment%SensibleRate = 0.0d0
    WaterEquipment%SensibleEnergy = 0.0d0
    WaterEquipment%SensibleRateNoMultiplier = 0.d0
    WaterEquipment%LatentRate = 0.0d0
    WaterEquipment%LatentEnergy = 0.0d0
    WaterEquipment%LatentRateNoMultiplier = 0.d0
    WaterEquipment%MixedTemp = 0.0d0
    WaterEquipment%TotalMassFlowRate = 0.0d0
    WaterEquipment%DrainTemp = 0.0d0
    WaterEquipment%ColdVolFlowRate = 0.0d0
    WaterEquipment%HotVolFlowRate = 0.0d0
    WaterEquipment%TotalVolFlowRate = 0.0d0
    WaterEquipment%ColdMassFlowRate = 0.0d0
    WaterEquipment%HotMassFlowRate = 0.0d0
    MyEnvrnFlag=.false.
  ENDIF

  IF (.not. BeginEnvrnFlag) MyEnvrnFlag=.true.

  DO WaterEquipNum = 1, NumWaterEquipment
    IF (WaterEquipment(WaterEquipNum)%Zone == 0) CYCLE
    ZoneNum = WaterEquipment(WaterEquipNum)%Zone
    WaterEquipment(WaterEquipNum)%SensibleRateNoMultiplier =  WaterEquipment(WaterEquipNum)%SensibleRate  &
          / ( Zone(ZoneNum)%Multiplier                             & ! CR7401, back out multipliers
              * Zone(ZoneNum)%ListMultiplier)
    WaterEquipment(WaterEquipNum)%LatentRateNoMultiplier =  WaterEquipment(WaterEquipNum)%LatentRate  &
          / ( Zone(ZoneNum)%Multiplier                             & ! CR7401, back out multipliers
              * Zone(ZoneNum)%ListMultiplier)
  END DO

!  ! this routine needs to model approx zone gains for use during sizing
!  IF(DoingSizing)THEN
!    DO WaterEquipNum = 1, NumWaterEquipment
!      WaterEquipment(WaterEquipNum)%SensibleRateNoMultiplier =
!      WaterEquipment(WaterEquipNum)%LatentRateNoMultiplier   =
!    END DO
!  ENDIF

  RETURN

END SUBROUTINE CalcWaterUseZoneGains

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

END MODULE WaterUse

