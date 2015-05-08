MODULE ZoneEquipmentManager

  ! Module containing the routines dealing with the Zone Equipment Manager.

  ! MODULE INFORMATION:
  !       AUTHOR         Russ Taylor
  !       DATE WRITTEN   Unknown
  !       MODIFIED       na
  !       RE-ENGINEERED  na

  ! PURPOSE OF THIS MODULE:
  ! This module manages the zone equipment.

  ! METHODOLOGY EMPLOYED: none

  ! REFERENCES: none

  ! OTHER NOTES: none

  ! USE STATEMENTS:
  ! Use statements for data only modules
    USE DataPrecisionGlobals
    USE DataGlobals, ONLY: MaxNameLength, NumOfTimeStepInHour, NumOfZones, BeginEnvrnFlag, BeginDayFlag, &
                           BeginHourFlag, BeginTimeStepFlag, DayOfSim, ZoneSizingCalc, outputfiledebug
    USE DataInterfaces, ONLY: ShowFatalError, ShowSevereError, ShowContinueError, ShowWarningError, ShowContinueErrorTimeStamp
    USE DataSizing
    USE DataEnvironment, ONLY: TotDesDays, CurEnvirNum, EnvironmentName, TotRunDesPersDays, OutDryBulbTemp, OutHumRat
    USE DataZoneEquipment
  ! Use statements for access to subroutines in other modules
    USE Psychrometrics, ONLY:PsyHFnTdbW, PsyCpAirFnWTdb, PsyRhoAirFnPbTdbW, PsyHgAirFnWTdb, PsyWFnTdpPb

IMPLICIT NONE         ! Enforce explicit typing of all variables

PRIVATE ! Everything private unless explicitly made public

  !MODULE PARAMETER DEFINITIONS
  ! na

  ! DERIVED TYPE DEFINITIONS
    TYPE SimulationOrder
      CHARACTER(len=MaxNameLength) :: EquipType   = ' '
      INTEGER                      :: EquipType_Num = 0
      CHARACTER(len=MaxNameLength) :: EquipName   = ' '
      INTEGER                      :: EquipPtr = 0
      INTEGER                      :: CoolingPriority = 0
      INTEGER                      :: HeatingPriority = 0
    END TYPE SimulationOrder

  !MODULE VARIABLE DECLARATIONS:
    REAL(r64), ALLOCATABLE, DIMENSION(:) :: AvgData ! scratch array for storing averaged data
    TYPE(SimulationOrder), ALLOCATABLE, DIMENSION(:)  :: PrioritySimOrder
    INTEGER, ALLOCATABLE, DIMENSION(:) :: DefaultSimOrder
    INTEGER :: NumOfTimeStepInDay ! number of zone time steps in a day
    LOGICAL :: GetZoneEquipmentInputFlag = .TRUE.

  !SUBROUTINE SPECIFICATIONS FOR MODULE ZoneEquipmentManager
PUBLIC  ManageZoneEquipment
PRIVATE GetZoneEquipment
PRIVATE InitZoneEquipment
PRIVATE SimZoneEquipment
PRIVATE SetZoneEquipSimOrder
PRIVATE InitSystemOutputRequired
PRIVATE UpdateSystemOutputRequired
PRIVATE UpdateZoneEquipment
PRIVATE CalcZoneLeavingConditions
PRIVATE CalcZoneMassBalance
PRIVATE ReportZoneEquipment
PRIVATE SizeZoneEquipment
PRIVATE SetUpZoneSizingArrays
PUBLIC  UpdateZoneSizing
PUBLIC  RezeroZoneSizingArrays


CONTAINS

SUBROUTINE ManageZoneEquipment(FirstHVACIteration,SimZone,SimAir)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Russ Taylor
          !       DATE WRITTEN   May 1997
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Calls the zone thermal control simulations and the interfaces
          ! (water-air, refrigerant-air, steam-air, electric-electric,
          ! water-water, etc)

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
    LOGICAL FirstHVACIteration
    LOGICAL SimZone
    LOGICAL SimAir

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          ! na

  IF (GetZoneEquipmentInputFlag) THEN
    CALL GetZoneEquipment
    GetZoneEquipmentInputFlag = .FALSE.
    ZoneEquipInputsFilled=.true.
  END IF

  CALL InitZoneEquipment(FirstHVACIteration)

  IF (ZoneSizingCalc) THEN
    CALL SizeZoneEquipment
  ELSE
    CALL SimZoneEquipment(FirstHVACIteration, SimAir)
    ZoneEquipSimulatedOnce = .TRUE.
  END IF

  CALL UpdateZoneEquipment(SimAir)

  CALL ReportZoneEquipment

  SimZone = .False.

  RETURN

END SUBROUTINE ManageZoneEquipment

SUBROUTINE GetZoneEquipment

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Russ Taylor
          !       DATE WRITTEN   June 1997
          !       MODIFIED       Aug 2003, FCW: set ZoneEquipConfig number for each zone
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Get all the system related equipment which may be attached to
          ! a zone

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
INTEGER :: Counter
INTEGER :: MaxNumOfEquipTypes

IF (.not. ZoneEquipInputsFilled) THEN
  CALL GetZoneEquipmentData
END IF

NumOfTimeStepInDay = NumOfTimeStepInHour * 24

MaxNumOfEquipTypes=0
DO Counter=1,NumOfZones
  IF (.not. ZoneEquipConfig(Counter)%IsControlled) CYCLE
  MaxNumOfEquipTypes=MAX(MaxNumOfEquipTypes,ZoneEquipList(Counter)%NumOfEquipTypes)
ENDDO

ALLOCATE(PrioritySimOrder(MaxNumOfEquipTypes))
ALLOCATE(DefaultSimOrder(MaxNumOfEquipTypes))
DO Counter=1,MaxNumOfEquipTypes
  DefaultSimOrder(Counter)=Counter
ENDDO

RETURN

END SUBROUTINE GetZoneEquipment

SUBROUTINE InitZoneEquipment(FirstHVACIteration)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Russ Taylor
          !       DATE WRITTEN   Nov 1997
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine initializes the zone equipment prior to simulation.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataHVACGlobals, ONLY: NoAction, ZoneComp
  USE DataEnvironment, ONLY: OutBaroPress, OutHumRat
  USE DataLoopNode, ONLY: Node
  USE DataAirLoop, ONLY : AirLoopFlow
  USE DataContaminantBalance, ONLY: Contaminant, OutdoorCO2, OutdoorGC
  USE DataZoneEnergyDemands , ONLY: ZoneSysEnergyDemand, ZoneSysMoistureDemand

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  LOGICAL, INTENT(IN) :: FirstHVACIteration !unused 1208

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
 INTEGER :: ZoneNodeNum
 INTEGER :: InNodeNum
 INTEGER :: ExhNodeNum
 INTEGER :: ZoneInNode
 INTEGER :: ZoneExhNode
 INTEGER :: ControlledZoneNum
 INTEGER :: ZoneReturnAirNode
 LOGICAL,SAVE   :: MyOneTimeFlag = .true.
 LOGICAL,SAVE   :: MyEnvrnFlag   = .true.
 INTEGER :: ZoneEquipType   ! Type of zone equipment
 INTEGER :: TotalNumComp    ! Total number of zone components of ZoneEquipType
 INTEGER :: ZoneCompNum     ! Number/index of zone equipment component
 INTEGER :: ZoneEquipCount  !
      ! Flow

     IF (MyOneTimeFlag) THEN
       MyOneTimeFlag = .false.
       ALLOCATE(TermUnitSizing(NumOfZones))
       ALLOCATE(ZoneEqSizing(NumOfZones))
       ! setup zone equipment sequenced demand storage
       DO ControlledZoneNum = 1,  NumOfZones
         IF (.not. ZoneEquipConfig(ControlledZoneNum)%IsControlled) CYCLE
         IF (ZoneEquipConfig(ControlledZoneNum)%EquipListIndex == 0) cycle
         ZoneEquipCount = ZoneEquipList(ZoneEquipConfig(ControlledZoneNum)%EquipListIndex)%NumOfEquipTypes
         ZoneSysEnergyDemand(ControlledZoneNum)%NumZoneEquipment = ZoneEquipCount
         ALLOCATE(ZoneSysEnergyDemand(ControlledZoneNum)%SequencedOutputRequired(ZoneEquipCount))
         ALLOCATE(ZoneSysEnergyDemand(ControlledZoneNum)%SequencedOutputRequiredToHeatingSP(ZoneEquipCount))
         ALLOCATE(ZoneSysEnergyDemand(ControlledZoneNum)%SequencedOutputRequiredToCoolingSP(ZoneEquipCount))
         ZoneSysMoistureDemand(ControlledZoneNum)%NumZoneEquipment = ZoneEquipCount
         ALLOCATE(ZoneSysMoistureDemand(ControlledZoneNum)%SequencedOutputRequired(ZoneEquipCount))
         ALLOCATE(ZoneSysMoistureDemand(ControlledZoneNum)%SequencedOutputRequiredToHumidSP(ZoneEquipCount))
         ALLOCATE(ZoneSysMoistureDemand(ControlledZoneNum)%SequencedOutputRequiredToDehumidSP(ZoneEquipCount))
       ENDDO
     END IF

     ! Do the Begin Environment initializations
     IF (MyEnvrnFlag .and. BeginEnvrnFlag) THEN

       ZoneEquipAvail=NoAction

       IF(ALLOCATED(ZoneComp))THEN
         DO ZoneEquipType = 1, NumValidSysAvailZoneComponents
           IF(ALLOCATED(ZoneComp(ZoneEquipType)%ZoneCompAvailMgrs)) THEN
             TotalNumComp = ZoneComp(ZoneEquipType)%TotalNumComp
             DO ZoneCompNum = 1, TotalNumComp
               ZoneComp(ZoneEquipType)%ZoneCompAvailMgrs(ZoneCompNum)%AvailStatus = NoAction
               ZoneComp(ZoneEquipType)%ZoneCompAvailMgrs(ZoneCompNum)%StartTime   = 0
               ZoneComp(ZoneEquipType)%ZoneCompAvailMgrs(ZoneCompNum)%StopTime    = 0
             END DO
           ENDIF
         END DO
       ENDIF
       DO ControlledZoneNum = 1,  NumOfZones
         IF (.not. ZoneEquipConfig(ControlledZoneNum)%IsControlled) CYCLE

         ZoneNodeNum = ZoneEquipConfig(ControlledZoneNum)%ZoneNode
         Node(ZoneNodeNum)%Temp = 20.0d0
         Node(ZoneNodeNum)%MassFlowRate = 0.0d0
         Node(ZoneNodeNum)%Quality = 1.0d0
         Node(ZoneNodeNum)%Press = OutBaroPress
         Node(ZoneNodeNum)%HumRat = OutHumRat
         Node(ZoneNodeNum)%Enthalpy = PsyHFnTdbW(Node(ZoneNodeNum)%Temp,Node(ZoneNodeNum)%HumRat)
         IF (Contaminant%CO2Simulation) Then
           Node(ZoneNodeNum)%CO2 = OutdoorCO2
         End If
         IF (Contaminant%GenericContamSimulation) Then
           Node(ZoneNodeNum)%GenContam = OutdoorGC
         End If

         DO ZoneInNode = 1,  ZoneEquipConfig(ControlledZoneNum)%NumInletNodes

           InNodeNum = ZoneEquipConfig(ControlledZoneNum)%InletNode(ZoneInNode)
           Node(InNodeNum)%Temp = 20.0d0
           Node(InNodeNum)%MassFlowRate = 0.0d0
           Node(InNodeNum)%Quality = 1.0d0
           Node(InNodeNum)%Press = OutBaroPress
           Node(InNodeNum)%HumRat = OutHumRat
           Node(InNodeNum)%Enthalpy = PsyHFnTdbW(Node(InNodeNum)%Temp,Node(InNodeNum)%HumRat)
           IF (Contaminant%CO2Simulation) Then
             Node(InNodeNum)%CO2 = OutdoorCO2
           End If
           IF (Contaminant%GenericContamSimulation) Then
             Node(InNodeNum)%GenContam = OutdoorGC
           End If

         END DO

         DO ZoneExhNode = 1,  ZoneEquipConfig(ControlledZoneNum)%NumExhaustNodes

           ExhNodeNum = ZoneEquipConfig(ControlledZoneNum)%ExhaustNode(ZoneExhNode)
           Node(ExhNodeNum)%Temp = 20.0d0
           Node(ExhNodeNum)%MassFlowRate = 0.0d0
           Node(ExhNodeNum)%Quality = 1.0d0
           Node(ExhNodeNum)%Press = OutBaroPress
           Node(ExhNodeNum)%HumRat = OutHumRat
           Node(ExhNodeNum)%Enthalpy = PsyHFnTdbW(Node(ExhNodeNum)%Temp,Node(ExhNodeNum)%HumRat)
           IF (Contaminant%CO2Simulation) Then
             Node(ExhNodeNum)%CO2 = OutdoorCO2
           End If
           IF (Contaminant%GenericContamSimulation) Then
             Node(ExhNodeNum)%GenContam = OutdoorGC
           End If

         END DO

         ! BG CR 7122 following resets return air node.
         ZoneReturnAirNode = ZoneEquipConfig(ControlledZoneNum)%ReturnAirNode
         IF (ZoneReturnAirNode > 0) THEN
             Node(ZoneReturnAirNode)%Temp = 20.0d0
             Node(ZoneReturnAirNode)%MassFlowRate = 0.0d0
             Node(ZoneReturnAirNode)%Quality = 1.0d0
             Node(ZoneReturnAirNode)%Press = OutBaroPress
             Node(ZoneReturnAirNode)%HumRat = OutHumRat
             Node(ZoneReturnAirNode)%Enthalpy = PsyHFnTdbW(Node(ZoneReturnAirNode)%Temp,Node(ZoneReturnAirNode)%HumRat)
             IF (Contaminant%CO2Simulation) Then
               Node(ZoneReturnAirNode)%CO2 = OutdoorCO2
             End If
             IF (Contaminant%GenericContamSimulation) Then
               Node(ZoneReturnAirNode)%GenContam = OutdoorGC
             End If
         ENDIF
       END DO

       MyEnvrnFlag=.false.

     END IF

     IF (.not. BeginEnvrnFlag) THEN
       MyEnvrnFlag=.true.
     ENDIF

     ! do the  HVAC time step initializations

       DO ControlledZoneNum = 1,  NumOfZones
         IF (.not. ZoneEquipConfig(ControlledZoneNum)%IsControlled) CYCLE
         ZoneNodeNum = ZoneEquipConfig(ControlledZoneNum)%ZoneNode

         IF(FirstHVACIteration)THEN
           DO ZoneExhNode = 1,  ZoneEquipConfig(ControlledZoneNum)%NumExhaustNodes
             ExhNodeNum = ZoneEquipConfig(ControlledZoneNum)%ExhaustNode(ZoneExhNode)
             Node(ExhNodeNum)%Temp = Node(ZoneNodeNum)%Temp
             Node(ExhNodeNum)%HumRat = Node(ZoneNodeNum)%HumRat
             Node(ExhNodeNum)%Enthalpy = Node(ZoneNodeNum)%Enthalpy
             Node(ExhNodeNum)%Press = Node(ZoneNodeNum)%Press
             Node(ExhNodeNum)%Quality = Node(ZoneNodeNum)%Quality
             Node(ExhNodeNum)%MassFlowRate = 0.0d0
             Node(ExhNodeNum)%MassFlowRateMaxAvail = 0.0d0
             Node(ExhNodeNum)%MassFlowRateMinAvail = 0.0d0
             IF (Contaminant%CO2Simulation) Then
               Node(ExhNodeNum)%CO2 = Node(ZoneNodeNum)%CO2
             End If
             IF (Contaminant%GenericContamSimulation) Then
               Node(ExhNodeNum)%GenContam = Node(ZoneNodeNum)%GenContam
             End If
           END DO
         END IF

         IF (ZoneEquipConfig(ControlledZoneNum)%AirLoopNum > 0) THEN
           AirLoopFlow(ZoneEquipConfig(ControlledZoneNum)%AirLoopNum)%ZoneExhaust = 0.d0
           AirLoopFlow(ZoneEquipConfig(ControlledZoneNum)%AirLoopNum)%ZoneExhaustBalanced = 0.d0
           AirLoopFlow(ZoneEquipConfig(ControlledZoneNum)%AirLoopNum)%SupFlow = 0.d0
           AirLoopFlow(ZoneEquipConfig(ControlledZoneNum)%AirLoopNum)%RetFlow = 0.d0
           AirLoopFlow(ZoneEquipConfig(ControlledZoneNum)%AirLoopNum)%RetFlow0 = 0.d0
           AirLoopFlow(ZoneEquipConfig(ControlledZoneNum)%AirLoopNum)%RecircFlow = 0.d0
         END IF

       END DO
  RETURN

END SUBROUTINE InitZoneEquipment

SUBROUTINE SizeZoneEquipment

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   December 2000
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Performs the zone sizing calculations and fills the zone sizing
          ! data arrays with the results of the calculation.

          ! METHODOLOGY EMPLOYED:
          ! Using the input from Zone Sizing objects and the Zone Equipment input,
          ! for each controlled zone this subroutine performs a "purchased air" calculation
          ! and saves the results in the zone sizing data arrays.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataHeatBalFanSys, ONLY: NonAirSystemResponse, SysDepZoneLoads, TempZoneThermostatSetPoint
  USE DataZoneEnergyDemands, ONLY: ZoneSysEnergyDemand, DeadBandOrSetback
  USE DataLoopNode, ONLY: Node
  USE DataHVACGlobals, ONLY: SmallLoad, SmallTempDiff
  USE General, ONLY: RoundSigDigits

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
  LOGICAL,SAVE :: MyOneTimeFlag = .TRUE.
  INTEGER :: ControlledZoneNum ! controlled zone index
  INTEGER :: ActualZoneNum     ! index into Zone array (all zones)
  INTEGER :: SupplyAirNode     ! node number of zone supply air node
  INTEGER :: ZoneNode          ! node number of controlled zone
  INTEGER :: ReturnNode        ! node number of controlled zone return node
  REAL(r64)    :: DeltaTemp         ! difference between supply air temp and zone temp [C]
  REAL(r64)    :: CpAir             ! heat capacity of air [J/kg-C]
  REAL(r64)    :: SysOutputProvided ! system sensible output [W]
  REAL(r64)    :: LatOutputProvided ! system latent output [kg/s]
  REAL(r64)    :: Temp              ! inlet temperature [C]
  REAL(r64)    :: HumRat            ! inlet humidity ratio [kg water/kg dry air]
  REAL(r64)    :: Enthalpy          ! inlet specific enthalpy [J/kg]
  REAL(r64)    :: MassFlowRate      ! inlet mass flow rate [kg/s]
  REAL(r64)    :: RetTemp           ! zone return temperature [C]

  IF (MyOneTimeFlag) THEN
    CALL SetUpZoneSizingArrays
    MyOneTimeFlag = .FALSE.
  END IF

  DO ControlledZoneNum = 1, NumOfZones
    IF (.not. ZoneEquipConfig(ControlledZoneNum)%IsControlled) CYCLE
    ActualZoneNum = CalcZoneSizing(ControlledZoneNum,CurOverallSimDay)%ActualZoneNum
    NonAirSystemResponse(ActualZoneNum) = 0.0d0
    SysDepZoneLoads(ActualZoneNum) = 0.0d0

    CALL InitSystemOutputRequired(ActualZoneNum, SysOutputProvided, LatOutputProvided)

    SupplyAirNode = CalcZoneSizing(ControlledZoneNum,CurOverallSimDay)%SupplyAirNode
    ZoneNode = ZoneEquipConfig(ControlledZoneNum)%ZoneNode

    ! Sign convention: SysOutputProvided <0 Supply air is heated on entering zone (zone is cooled)
    !                  SysOutputProvided >0 Supply air is cooled on entering zone (zone is heated)
    IF ( .NOT. DeadBandOrSetback(ActualZoneNum) .AND. &
         ABS(ZoneSysEnergyDemand(ActualZoneNum)%RemainingOutputRequired) .GT. SmallLoad) THEN
      ! Determine design supply air temperture and design supply air temperature difference
      IF (ZoneSysEnergyDemand(ActualZoneNum)%RemainingOutputRequired < 0.0d0) THEN  ! Cooling case
        ! If the user specify the design cooling supply air temperature, then
        IF (CalcZoneSizing(ControlledZoneNum,CurOverallSimDay)%ZnCoolDgnSAMethod == SupplyAirTemperature) THEN
          Temp = CalcZoneSizing(ControlledZoneNum,CurOverallSimDay)%CoolDesTemp
          HumRat = CalcZoneSizing(ControlledZoneNum,CurOverallSimDay)%CoolDesHumRat
          DeltaTemp = Temp - Node(ZoneNode)%Temp
        ! If the user specify the design cooling supply air temperature difference, then
        ELSE
          DeltaTemp = -ABS(CalcZoneSizing(ControlledZoneNum,CurOverallSimDay)%CoolDesTempDiff)
          Temp = DeltaTemp + Node(ZoneNode)%Temp
          HumRat = CalcZoneSizing(ControlledZoneNum,CurOverallSimDay)%CoolDesHumRat
        END IF
      ELSE ! Heating Case
        ! If the user specify the design heating supply air temperature, then
        IF (CalcZoneSizing(ControlledZoneNum,CurOverallSimDay)%ZnHeatDgnSAMethod == SupplyAirTemperature) THEN
          Temp = CalcZoneSizing(ControlledZoneNum,CurOverallSimDay)%HeatDesTemp
          HumRat = CalcZoneSizing(ControlledZoneNum,CurOverallSimDay)%HeatDesHumRat
          DeltaTemp = Temp - Node(ZoneNode)%Temp
        ! If the user specify the design heating supply air temperature difference, then
        ELSE
          DeltaTemp = ABS(CalcZoneSizing(ControlledZoneNum,CurOverallSimDay)%HeatDesTempDiff)
          Temp = DeltaTemp + Node(ZoneNode)%Temp
          HumRat = CalcZoneSizing(ControlledZoneNum,CurOverallSimDay)%HeatDesHumRat
        END IF
      END IF

      Enthalpy = PsyHFnTdbW(Temp,HumRat)
      SysOutputProvided = ZoneSysEnergyDemand(ActualZoneNum)%RemainingOutputRequired
      CpAir = PsyCpAirFnWTdb(HumRat,Temp)
      IF ( ABS(DeltaTemp) > SmallTempDiff ) THEN
!!!PH/WFB/LKL (UCDV model)        MassFlowRate = SysOutputProvided / (CpAir*DeltaTemp)
        MassFlowRate = MAX( SysOutputProvided/(CpAir*DeltaTemp), 0.0d0)
      ELSE
        MassFlowRate = 0.0d0
      ENDIF

      IF (CalcZoneSizing(ControlledZoneNum,CurOverallSimDay)%SupplyAirAdjustFactor > 1.0d0) THEN
        MassFlowRate = MassFlowRate * CalcZoneSizing(ControlledZoneNum,CurOverallSimDay)%SupplyAirAdjustFactor
      ENDIF
    ELSE

      Temp = Node(ZoneNode)%Temp
      HumRat = Node(ZoneNode)%HumRat
      Enthalpy = Node(ZoneNode)%Enthalpy
      MassFlowRate = 0.0d0

    END IF

    CALL UpdateSystemOutputRequired(ActualZoneNum, SysOutputProvided, LatOutputProvided)

    IF (SysOutputProvided > 0.0d0) THEN
      CalcZoneSizing(ControlledZoneNum,CurOverallSimDay)%HeatLoad = SysOutputProvided
      CalcZoneSizing(ControlledZoneNum,CurOverallSimDay)%HeatMassFlow = MassFlowRate
      CalcZoneSizing(ControlledZoneNum,CurOverallSimDay)%HeatZoneTemp = Node(ZoneNode)%Temp
      CalcZoneSizing(ControlledZoneNum,CurOverallSimDay)%HeatZoneHumRat = Node(ZoneNode)%HumRat
      CalcZoneSizing(ControlledZoneNum,CurOverallSimDay)%HeatOutTemp = OutDryBulbTemp
      CalcZoneSizing(ControlledZoneNum,CurOverallSimDay)%HeatOutHumRat = OutHumRat
      CalcZoneSizing(ControlledZoneNum,CurOverallSimDay)%CoolLoad = 0.0d0
      CalcZoneSizing(ControlledZoneNum,CurOverallSimDay)%CoolMassFlow = 0.0d0
      CalcZoneSizing(ControlledZoneNum,CurOverallSimDay)%CoolZoneTemp = 0.0d0
      CalcZoneSizing(ControlledZoneNum,CurOverallSimDay)%CoolZoneHumRat = 0.0d0
    ELSE
      CalcZoneSizing(ControlledZoneNum,CurOverallSimDay)%CoolLoad = -SysOutputProvided
      CalcZoneSizing(ControlledZoneNum,CurOverallSimDay)%CoolMassFlow = MassFlowRate
      CalcZoneSizing(ControlledZoneNum,CurOverallSimDay)%CoolZoneTemp = Node(ZoneNode)%Temp
      CalcZoneSizing(ControlledZoneNum,CurOverallSimDay)%CoolZoneHumRat = Node(ZoneNode)%HumRat
      CalcZoneSizing(ControlledZoneNum,CurOverallSimDay)%CoolOutTemp = OutDryBulbTemp
      CalcZoneSizing(ControlledZoneNum,CurOverallSimDay)%CoolOutHumRat = OutHumRat
      CalcZoneSizing(ControlledZoneNum,CurOverallSimDay)%HeatLoad = 0.0d0
      CalcZoneSizing(ControlledZoneNum,CurOverallSimDay)%HeatMassFlow = 0.0d0
      CalcZoneSizing(ControlledZoneNum,CurOverallSimDay)%HeatZoneTemp = 0.0d0
      CalcZoneSizing(ControlledZoneNum,CurOverallSimDay)%HeatZoneHumRat = 0.0d0
    END IF

    IF (SupplyAirNode > 0) THEN
      Node(SupplyAirNode)%Temp = Temp
      Node(SupplyAirNode)%HumRat = HumRat
      Node(SupplyAirNode)%Enthalpy = Enthalpy
      Node(SupplyAirNode)%MassFlowRate = MassFlowRate
    ELSE
      NonAirSystemResponse(ActualZoneNum) = SysOutputProvided
    END IF

  END DO

  CALL CalcZoneMassBalance

  CALL CalcZoneLeavingConditions

  DO ControlledZoneNum = 1, NumOfZones
    IF (.not. ZoneEquipConfig(ControlledZoneNum)%IsControlled) CYCLE
    ReturnNode = ZoneEquipConfig(ControlledZoneNum)%ReturnAirNode
    ZoneNode = ZoneEquipConfig(ControlledZoneNum)%ZoneNode
    IF (ReturnNode > 0) THEN
      RetTemp = Node(ReturnNode)%Temp
    ELSE
      RetTemp = Node(ZoneNode)%Temp
    END IF
    IF (CalcZoneSizing(ControlledZoneNum,CurOverallSimDay)%HeatLoad > 0.0d0) THEN
      CalcZoneSizing(ControlledZoneNum,CurOverallSimDay)%HeatZoneRetTemp = RetTemp
      CalcZoneSizing(ControlledZoneNum,CurOverallSimDay)%HeatTstatTemp = TempZoneThermostatSetPoint(ActualZoneNum)
    ELSE
      CalcZoneSizing(ControlledZoneNum,CurOverallSimDay)%CoolZoneRetTemp = RetTemp
      CalcZoneSizing(ControlledZoneNum,CurOverallSimDay)%CoolTstatTemp = TempZoneThermostatSetPoint(ActualZoneNum)
    END IF
  END DO

  RETURN

END SUBROUTINE SizeZoneEquipment

SUBROUTINE SetUpZoneSizingArrays

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   December 2000
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Allocate and fill the ZoneSizing data array.

          ! METHODOLOGY EMPLOYED:
          ! Obtains data from Zone Sizing and Zone Equipment objects already input.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataGlobals ,   ONLY : OutputFileInits, AnyEnergyManagementSystemInModel,isPulseZoneSizing
  USE DataInterfaces, ONLY : SetupEMSActuator, SetupEMSInternalVariable
  USE InputProcessor, ONLY : FindItemInList
  USE DataHeatBalance, ONLY : People, TotPeople
  USE DataHeatBalance, ONLY: Zone
  USE ZoneTempPredictorCorrector, ONLY: VerifyThermostatInZone
  USE EMSManager, ONLY:  ManageEMS
  USE ScheduleManager, ONLY: GetScheduleMaxValue
  USE DataZoneEquipment, ONLY: CalcDesignSpecificationOutdoorAir

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
  INTEGER :: DesDayNum   ! design day index
!unused  INTEGER :: DesDayEnvrnNum   ! design day index
  INTEGER :: CtrlZoneNum ! controlled zone index
  INTEGER :: ZoneSizNum  ! zone sizing input index
  INTEGER :: NumOfTimeStepInDay ! number of zone time steps in a day
  INTEGER :: TimeStepIndex      ! zone time step index
  REAL(r64)    :: TotPeopleInZone    ! total (maximum) number of people in a zone
  INTEGER :: PeopleNum          ! index of People structure
  REAL(r64)    :: OAFromPeople = 0.0d0 ! min OA calculated from zone occupancy [m3/s]
  REAL(r64)    :: OAFromArea = 0.0d0   ! min OA calculated from zone area and OA flow per area [m3/s]
  INTEGER :: ZoneIndex          ! index of Zone Sizing zone name in zone array
  INTEGER :: ZoneSizIndex       ! zone sizing do loop index
  LOGICAL :: ErrorsFound=.false.! Set to true if errors in input, fatal at end of routine
  REAL (r64) :: SchMax   = 0.0d0  ! maximum people multiplier value
  REAL(r64) :: OAVolumeFlowRate ! outside air flow rate (m3/s)
  LOGICAL :: UseOccSchFlag      ! flag to use occupancy schedule when calculating OA
  LOGICAL :: UseMinOASchFlag    ! flag to use min OA schedule when calculating OA
  INTEGER :: DSOAPtr            ! index to DesignSpecification:OutdoorAir object

  DO ZoneSizIndex=1,NumZoneSizingInput
    ZoneIndex = FindItemInList(ZoneSizingInput(ZoneSizIndex)%ZoneName,Zone%Name,NumOfZones)
    IF (ZoneIndex == 0) THEN
      CALL ShowSevereError('SetUpZoneSizingArrays: Sizing:Zone="'//TRIM(ZoneSizingInput(ZoneSizIndex)%ZoneName)// &
                           '" references unknown zone')
      ErrorsFound = .TRUE.
    END IF
    IF (ANY(ZoneEquipConfig%IsControlled)) THEN
      ZoneIndex = FindItemInList(ZoneSizingInput(ZoneSizIndex)%ZoneName,ZoneEquipConfig%ZoneName,NumOfZones)
      IF (ZoneIndex == 0) THEN
        IF (.NOT. isPulseZoneSizing) THEN
          CALL ShowWarningError('SetUpZoneSizingArrays: Requested Sizing for Zone="'//  &
                              TRIM(ZoneSizingInput(ZoneSizIndex)%ZoneName)//  &
                              '", Zone is not found in the Controlled Zones List')
        ENDIF
      ELSE
        ZoneSizingInput(ZoneSizIndex)%ZoneNum = ZoneIndex
      ENDIF
      IF (ZoneSizingInput(ZoneSizIndex)%CoolAirDesMethod == FromDDCalc .or.   &
          ZoneSizingInput(ZoneSizIndex)%HeatAirDesMethod == FromDDCalc) THEN
        IF (.not. VerifyThermostatInZone(ZoneSizingInput(ZoneSizIndex)%ZoneName)) THEN
          IF (.NOT. isPulseZoneSizing) THEN
            CALL ShowWarningError('SetUpZoneSizingArrays: Requested Sizing for Zone="'//  &
                             TRIM(ZoneSizingInput(ZoneSizIndex)%ZoneName)//  &
                             '", Zone has no thermostat (ref: ZoneControl:Thermostat, et al)')
          ENDIF
        ENDIF
      ENDIF
    ELSE
      CALL ShowSevereError('SetUpZoneSizingArrays: Zone Sizing is requested '//  &
         'but there are no ZoneHVAC:EquipmentConnections statements.')
      ErrorsFound = .TRUE.
    ENDIF
  END DO
  IF (ErrorsFound) THEN
    CALL ShowFatalError('SetUpZoneSizingArrays: Errors found in Sizing:Zone input')
  END IF

  ALLOCATE(ZoneSizing(NumOfZones,TotDesDays+TotRunDesPersDays))
  ALLOCATE(FinalZoneSizing(NumOfZones))
  ALLOCATE(CalcZoneSizing(NumOfZones,TotDesDays+TotRunDesPersDays))
  ALLOCATE(CalcFinalZoneSizing(NumOfZones))
  ALLOCATE(TermUnitFinalZoneSizing(NumOfZones))
  ALLOCATE(DesDayWeath(TotDesDays+TotRunDesPersDays))
  NumOfTimeStepInDay = NumOfTimeStepInHour * 24
  ALLOCATE(AvgData(NumOfTimeStepInDay))
  ALLOCATE(CoolPeakDateHrMin(NumOfZones))
  ALLOCATE(HeatPeakDateHrMin(NumOfZones))
  ALLOCATE(ZoneSizThermSetPtHi(NumOfZones))
  ALLOCATE(ZoneSizThermSetPtLo(NumOfZones))

  CoolPeakDateHrMin = ' '
  HeatPeakDateHrMin = ' '

  ZoneSizThermSetPtHi = 0.0d0
  ZoneSizThermSetPtLo = 1000.d0

  DO DesDayNum=1,TotDesDays+TotRunDesPersDays
    ALLOCATE( DesDayWeath(DesDayNum)%Temp(NumOfTimeStepInHour*24) )
    ALLOCATE( DesDayWeath(DesDayNum)%HumRat(NumOfTimeStepInHour*24) )
    ALLOCATE( DesDayWeath(DesDayNum)%Press(NumOfTimeStepInHour*24) )
    DesDayWeath(DesDayNum)%Temp = 0.0d0
    DesDayWeath(DesDayNum)%HumRat = 0.0d0
    DesDayWeath(DesDayNum)%Press = 0.0d0
  END DO
  ! Fill zone sizing arrays from input array
  DO DesDayNum=1,TotDesDays+TotRunDesPersDays
    DO CtrlZoneNum = 1,NumOfZones
      IF (.not. ZoneEquipConfig(CtrlZoneNum)%IsControlled) CYCLE
      ZoneSizing(CtrlZoneNum,DesDayNum)%ZoneName = ZoneEquipConfig(CtrlZoneNum)%ZoneName
      ZoneSizing(CtrlZoneNum,DesDayNum)%ActualZoneNum = ZoneEquipConfig(CtrlZoneNum)%ActualZoneNum
      IF (ZoneEquipConfig(CtrlZoneNum)%NumInletNodes > 0) THEN
        ZoneSizing(CtrlZoneNum,DesDayNum)%SupplyAirNode = ZoneEquipConfig(CtrlZoneNum)%InletNode(1)
      END IF
      CalcZoneSizing(CtrlZoneNum,DesDayNum)%ZoneName = ZoneEquipConfig(CtrlZoneNum)%ZoneName
      CalcZoneSizing(CtrlZoneNum,DesDayNum)%ActualZoneNum = ZoneEquipConfig(CtrlZoneNum)%ActualZoneNum
      IF (ZoneEquipConfig(CtrlZoneNum)%NumInletNodes > 0) THEN
        CalcZoneSizing(CtrlZoneNum,DesDayNum)%SupplyAirNode = ZoneEquipConfig(CtrlZoneNum)%InletNode(1)
      END IF
      ! For each Zone Sizing object, find the corresponding controlled zone
      ZoneSizNum = FindItemInList(ZoneEquipConfig(CtrlZoneNum)%ZoneName,ZoneSizingInput%ZoneName, &
                                  NumZoneSizingInput)
      IF (ZoneSizNum > 0) THEN  ! move data from zone sizing input
        ZoneSizing(CtrlZoneNum,DesDayNum)%ZnCoolDgnSAMethod = ZoneSizingInput(ZoneSizNum)%ZnCoolDgnSAMethod
        ZoneSizing(CtrlZoneNum,DesDayNum)%ZnHeatDgnSAMethod = ZoneSizingInput(ZoneSizNum)%ZnHeatDgnSAMethod
        ZoneSizing(CtrlZoneNum,DesDayNum)%CoolDesTemp = ZoneSizingInput(ZoneSizNum)%CoolDesTemp
        ZoneSizing(CtrlZoneNum,DesDayNum)%HeatDesTemp = ZoneSizingInput(ZoneSizNum)%HeatDesTemp
        ZoneSizing(CtrlZoneNum,DesDayNum)%CoolDesTempDiff = ZoneSizingInput(ZoneSizNum)%CoolDesTempDiff
        ZoneSizing(CtrlZoneNum,DesDayNum)%HeatDesTempDiff = ZoneSizingInput(ZoneSizNum)%HeatDesTempDiff
        ZoneSizing(CtrlZoneNum,DesDayNum)%CoolDesHumRat = ZoneSizingInput(ZoneSizNum)%CoolDesHumRat
        ZoneSizing(CtrlZoneNum,DesDayNum)%HeatDesHumRat = ZoneSizingInput(ZoneSizNum)%HeatDesHumRat
        ZoneSizing(CtrlZoneNum,DesDayNum)%OADesMethod = ZoneSizingInput(ZoneSizNum)%OADesMethod
        ZoneSizing(CtrlZoneNum,DesDayNum)%DesOAFlowPPer = ZoneSizingInput(ZoneSizNum)%DesOAFlowPPer
        ZoneSizing(CtrlZoneNum,DesDayNum)%DesOAFlowPerArea = ZoneSizingInput(ZoneSizNum)%DesOAFlowPerArea
        ZoneSizing(CtrlZoneNum,DesDayNum)%DesOAFlow = ZoneSizingInput(ZoneSizNum)%DesOAFlow
        ZoneSizing(CtrlZoneNum,DesDayNum)%CoolAirDesMethod = ZoneSizingInput(ZoneSizNum)%CoolAirDesMethod
        ZoneSizing(CtrlZoneNum,DesDayNum)%HeatAirDesMethod = ZoneSizingInput(ZoneSizNum)%HeatAirDesMethod
        ZoneSizing(CtrlZoneNum,DesDayNum)%InpDesCoolAirFlow = ZoneSizingInput(ZoneSizNum)%DesCoolAirFlow
        ZoneSizing(CtrlZoneNum,DesDayNum)%DesCoolMinAirFlowPerArea = ZoneSizingInput(ZoneSizNum)%DesCoolMinAirFlowPerArea
        ZoneSizing(CtrlZoneNum,DesDayNum)%DesCoolMinAirFlow = ZoneSizingInput(ZoneSizNum)%DesCoolMinAirFlow
        ZoneSizing(CtrlZoneNum,DesDayNum)%DesCoolMinAirFlowFrac = ZoneSizingInput(ZoneSizNum)%DesCoolMinAirFlowFrac
        ZoneSizing(CtrlZoneNum,DesDayNum)%InpDesHeatAirFlow = ZoneSizingInput(ZoneSizNum)%DesHeatAirFlow
        ZoneSizing(CtrlZoneNum,DesDayNum)%DesHeatMaxAirFlowPerArea = ZoneSizingInput(ZoneSizNum)%DesHeatMaxAirFlowPerArea
        ZoneSizing(CtrlZoneNum,DesDayNum)%DesHeatMaxAirFlow = ZoneSizingInput(ZoneSizNum)%DesHeatMaxAirFlow
        ZoneSizing(CtrlZoneNum,DesDayNum)%DesHeatMaxAirFlowFrac = ZoneSizingInput(ZoneSizNum)%DesHeatMaxAirFlowFrac
        ZoneSizing(CtrlZoneNum,DesDayNum)%HeatSizingFactor = ZoneSizingInput(ZoneSizNum)%HeatSizingFactor
        ZoneSizing(CtrlZoneNum,DesDayNum)%CoolSizingFactor = ZoneSizingInput(ZoneSizNum)%CoolSizingFactor
        CalcZoneSizing(CtrlZoneNum,DesDayNum)%ZnCoolDgnSAMethod = ZoneSizingInput(ZoneSizNum)%ZnCoolDgnSAMethod
        CalcZoneSizing(CtrlZoneNum,DesDayNum)%ZnHeatDgnSAMethod = ZoneSizingInput(ZoneSizNum)%ZnHeatDgnSAMethod
        CalcZoneSizing(CtrlZoneNum,DesDayNum)%CoolDesTemp = ZoneSizingInput(ZoneSizNum)%CoolDesTemp
        CalcZoneSizing(CtrlZoneNum,DesDayNum)%HeatDesTemp = ZoneSizingInput(ZoneSizNum)%HeatDesTemp
        CalcZoneSizing(CtrlZoneNum,DesDayNum)%CoolDesTempDiff = ZoneSizingInput(ZoneSizNum)%CoolDesTempDiff
        CalcZoneSizing(CtrlZoneNum,DesDayNum)%HeatDesTempDiff = ZoneSizingInput(ZoneSizNum)%HeatDesTempDiff
        CalcZoneSizing(CtrlZoneNum,DesDayNum)%CoolDesHumRat = ZoneSizingInput(ZoneSizNum)%CoolDesHumRat
        CalcZoneSizing(CtrlZoneNum,DesDayNum)%HeatDesHumRat = ZoneSizingInput(ZoneSizNum)%HeatDesHumRat
        CalcZoneSizing(CtrlZoneNum,DesDayNum)%OADesMethod = ZoneSizingInput(ZoneSizNum)%OADesMethod
        CalcZoneSizing(CtrlZoneNum,DesDayNum)%DesOAFlowPPer = ZoneSizingInput(ZoneSizNum)%DesOAFlowPPer
        CalcZoneSizing(CtrlZoneNum,DesDayNum)%DesOAFlowPerArea = ZoneSizingInput(ZoneSizNum)%DesOAFlowPerArea
        CalcZoneSizing(CtrlZoneNum,DesDayNum)%DesOAFlow = ZoneSizingInput(ZoneSizNum)%DesOAFlow
        CalcZoneSizing(CtrlZoneNum,DesDayNum)%CoolAirDesMethod = ZoneSizingInput(ZoneSizNum)%CoolAirDesMethod
        CalcZoneSizing(CtrlZoneNum,DesDayNum)%HeatAirDesMethod = ZoneSizingInput(ZoneSizNum)%HeatAirDesMethod
        CalcZoneSizing(CtrlZoneNum,DesDayNum)%InpDesCoolAirFlow = ZoneSizingInput(ZoneSizNum)%DesCoolAirFlow
        CalcZoneSizing(CtrlZoneNum,DesDayNum)%DesCoolMinAirFlowPerArea = ZoneSizingInput(ZoneSizNum)%DesCoolMinAirFlowPerArea
        CalcZoneSizing(CtrlZoneNum,DesDayNum)%DesCoolMinAirFlow = ZoneSizingInput(ZoneSizNum)%DesCoolMinAirFlow
        CalcZoneSizing(CtrlZoneNum,DesDayNum)%DesCoolMinAirFlowFrac = ZoneSizingInput(ZoneSizNum)%DesCoolMinAirFlowFrac
        CalcZoneSizing(CtrlZoneNum,DesDayNum)%InpDesHeatAirFlow = ZoneSizingInput(ZoneSizNum)%DesHeatAirFlow
        CalcZoneSizing(CtrlZoneNum,DesDayNum)%DesHeatMaxAirFlowPerArea = ZoneSizingInput(ZoneSizNum)%DesHeatMaxAirFlowPerArea
        CalcZoneSizing(CtrlZoneNum,DesDayNum)%DesHeatMaxAirFlow = ZoneSizingInput(ZoneSizNum)%DesHeatMaxAirFlow
        CalcZoneSizing(CtrlZoneNum,DesDayNum)%DesHeatMaxAirFlowFrac = ZoneSizingInput(ZoneSizNum)%DesHeatMaxAirFlowFrac
        CalcZoneSizing(CtrlZoneNum,DesDayNum)%HeatSizingFactor = ZoneSizingInput(ZoneSizNum)%HeatSizingFactor
        CalcZoneSizing(CtrlZoneNum,DesDayNum)%CoolSizingFactor = ZoneSizingInput(ZoneSizNum)%CoolSizingFactor
      ELSE ! Every controlled zone must be simulated, so set missing inputs to the first
        !LKL I think this is sufficient for warning -- no need for array
        IF (DesDayNum == 1) THEN
          IF (.NOT. isPulseZoneSizing) THEN
            CALL ShowWarningError('SetUpZoneSizingArrays: Sizing for Zone="'//  &
              trim(ZoneEquipConfig(CtrlZoneNum)%ZoneName)//'" will use Sizing:Zone specifications listed for Zone="'//  &
              trim(ZoneSizingInput(1)%ZoneName)//'".')
          ENDIF
          ! Following needs to be implemented first:
!          CALL ShowContinueError('  A better option would be to set up global ZoneList objects for Sizing:Zone objects.')
        ENDIF
        ZoneSizing(CtrlZoneNum,DesDayNum)%ZnCoolDgnSAMethod = ZoneSizingInput(1)%ZnCoolDgnSAMethod
        ZoneSizing(CtrlZoneNum,DesDayNum)%ZnHeatDgnSAMethod = ZoneSizingInput(1)%ZnHeatDgnSAMethod
        ZoneSizing(CtrlZoneNum,DesDayNum)%CoolDesTemp = ZoneSizingInput(1)%CoolDesTemp
        ZoneSizing(CtrlZoneNum,DesDayNum)%HeatDesTemp = ZoneSizingInput(1)%HeatDesTemp
        ZoneSizing(CtrlZoneNum,DesDayNum)%CoolDesTempDiff = ZoneSizingInput(1)%CoolDesTempDiff
        ZoneSizing(CtrlZoneNum,DesDayNum)%HeatDesTempDiff = ZoneSizingInput(1)%HeatDesTempDiff
        ZoneSizing(CtrlZoneNum,DesDayNum)%CoolDesHumRat = ZoneSizingInput(1)%CoolDesHumRat
        ZoneSizing(CtrlZoneNum,DesDayNum)%HeatDesHumRat = ZoneSizingInput(1)%HeatDesHumRat
        ZoneSizing(CtrlZoneNum,DesDayNum)%OADesMethod = ZoneSizingInput(1)%OADesMethod
        ZoneSizing(CtrlZoneNum,DesDayNum)%DesOAFlowPPer = ZoneSizingInput(1)%DesOAFlowPPer
        ZoneSizing(CtrlZoneNum,DesDayNum)%DesOAFlowPerArea = ZoneSizingInput(1)%DesOAFlowPerArea
        ZoneSizing(CtrlZoneNum,DesDayNum)%DesOAFlow = ZoneSizingInput(1)%DesOAFlow
        ZoneSizing(CtrlZoneNum,DesDayNum)%CoolAirDesMethod = ZoneSizingInput(1)%CoolAirDesMethod
        ZoneSizing(CtrlZoneNum,DesDayNum)%HeatAirDesMethod = ZoneSizingInput(1)%HeatAirDesMethod
        ZoneSizing(CtrlZoneNum,DesDayNum)%InpDesCoolAirFlow = ZoneSizingInput(1)%DesCoolAirFlow
        ZoneSizing(CtrlZoneNum,DesDayNum)%DesCoolMinAirFlowPerArea = ZoneSizingInput(1)%DesCoolMinAirFlowPerArea
        ZoneSizing(CtrlZoneNum,DesDayNum)%DesCoolMinAirFlow = ZoneSizingInput(1)%DesCoolMinAirFlow
        ZoneSizing(CtrlZoneNum,DesDayNum)%DesCoolMinAirFlowFrac = ZoneSizingInput(1)%DesCoolMinAirFlowFrac
        ZoneSizing(CtrlZoneNum,DesDayNum)%InpDesHeatAirFlow = ZoneSizingInput(1)%DesHeatAirFlow
        ZoneSizing(CtrlZoneNum,DesDayNum)%DesHeatMaxAirFlowPerArea = ZoneSizingInput(1)%DesHeatMaxAirFlowPerArea
        ZoneSizing(CtrlZoneNum,DesDayNum)%DesHeatMaxAirFlow = ZoneSizingInput(1)%DesHeatMaxAirFlow
        ZoneSizing(CtrlZoneNum,DesDayNum)%DesHeatMaxAirFlowFrac = ZoneSizingInput(1)%DesHeatMaxAirFlowFrac
        ZoneSizing(CtrlZoneNum,DesDayNum)%HeatSizingFactor = ZoneSizingInput(1)%HeatSizingFactor
        ZoneSizing(CtrlZoneNum,DesDayNum)%CoolSizingFactor = ZoneSizingInput(1)%CoolSizingFactor
        CalcZoneSizing(CtrlZoneNum,DesDayNum)%ZnCoolDgnSAMethod = ZoneSizingInput(1)%ZnCoolDgnSAMethod
        CalcZoneSizing(CtrlZoneNum,DesDayNum)%ZnHeatDgnSAMethod = ZoneSizingInput(1)%ZnHeatDgnSAMethod
        CalcZoneSizing(CtrlZoneNum,DesDayNum)%CoolDesTemp = ZoneSizingInput(1)%CoolDesTemp
        CalcZoneSizing(CtrlZoneNum,DesDayNum)%HeatDesTemp = ZoneSizingInput(1)%HeatDesTemp
        CalcZoneSizing(CtrlZoneNum,DesDayNum)%CoolDesTempDiff = ZoneSizingInput(1)%CoolDesTempDiff
        CalcZoneSizing(CtrlZoneNum,DesDayNum)%HeatDesTempDiff = ZoneSizingInput(1)%HeatDesTempDiff
        CalcZoneSizing(CtrlZoneNum,DesDayNum)%CoolDesHumRat = ZoneSizingInput(1)%CoolDesHumRat
        CalcZoneSizing(CtrlZoneNum,DesDayNum)%HeatDesHumRat = ZoneSizingInput(1)%HeatDesHumRat
        CalcZoneSizing(CtrlZoneNum,DesDayNum)%OADesMethod = ZoneSizingInput(1)%OADesMethod
        CalcZoneSizing(CtrlZoneNum,DesDayNum)%DesOAFlowPPer = ZoneSizingInput(1)%DesOAFlowPPer
        CalcZoneSizing(CtrlZoneNum,DesDayNum)%DesOAFlowPerArea = ZoneSizingInput(1)%DesOAFlowPerArea
        CalcZoneSizing(CtrlZoneNum,DesDayNum)%DesOAFlow = ZoneSizingInput(1)%DesOAFlow
        CalcZoneSizing(CtrlZoneNum,DesDayNum)%CoolAirDesMethod = ZoneSizingInput(1)%CoolAirDesMethod
        CalcZoneSizing(CtrlZoneNum,DesDayNum)%HeatAirDesMethod = ZoneSizingInput(1)%HeatAirDesMethod
        CalcZoneSizing(CtrlZoneNum,DesDayNum)%InpDesCoolAirFlow = ZoneSizingInput(1)%DesCoolAirFlow
        CalcZoneSizing(CtrlZoneNum,DesDayNum)%DesCoolMinAirFlowPerArea = ZoneSizingInput(1)%DesCoolMinAirFlowPerArea
        CalcZoneSizing(CtrlZoneNum,DesDayNum)%DesCoolMinAirFlow = ZoneSizingInput(1)%DesCoolMinAirFlow
        CalcZoneSizing(CtrlZoneNum,DesDayNum)%DesCoolMinAirFlowFrac = ZoneSizingInput(1)%DesCoolMinAirFlowFrac
        CalcZoneSizing(CtrlZoneNum,DesDayNum)%InpDesHeatAirFlow = ZoneSizingInput(1)%DesHeatAirFlow
        CalcZoneSizing(CtrlZoneNum,DesDayNum)%DesHeatMaxAirFlowPerArea = ZoneSizingInput(1)%DesHeatMaxAirFlowPerArea
        CalcZoneSizing(CtrlZoneNum,DesDayNum)%DesHeatMaxAirFlow = ZoneSizingInput(1)%DesHeatMaxAirFlow
        CalcZoneSizing(CtrlZoneNum,DesDayNum)%DesHeatMaxAirFlowFrac = ZoneSizingInput(1)%DesHeatMaxAirFlowFrac
        CalcZoneSizing(CtrlZoneNum,DesDayNum)%HeatSizingFactor = ZoneSizingInput(1)%HeatSizingFactor
        CalcZoneSizing(CtrlZoneNum,DesDayNum)%CoolSizingFactor = ZoneSizingInput(1)%CoolSizingFactor
      END IF
      ALLOCATE(ZoneSizing(CtrlZoneNum,DesDayNum)%HeatFlowSeq(NumOfTimeStepInDay))
      ALLOCATE(ZoneSizing(CtrlZoneNum,DesDayNum)%CoolFlowSeq(NumOfTimeStepInDay))
      ALLOCATE(ZoneSizing(CtrlZoneNum,DesDayNum)%HeatLoadSeq(NumOfTimeStepInDay))
      ALLOCATE(ZoneSizing(CtrlZoneNum,DesDayNum)%CoolLoadSeq(NumOfTimeStepInDay))
      ALLOCATE(ZoneSizing(CtrlZoneNum,DesDayNum)%HeatZoneTempSeq(NumOfTimeStepInDay))
      ALLOCATE(ZoneSizing(CtrlZoneNum,DesDayNum)%DesHeatSetPtSeq(NumOfTimeStepInDay))
      ALLOCATE(ZoneSizing(CtrlZoneNum,DesDayNum)%CoolZoneTempSeq(NumOfTimeStepInDay))
      ALLOCATE(ZoneSizing(CtrlZoneNum,DesDayNum)%DesCoolSetPtSeq(NumOfTimeStepInDay))
      ALLOCATE(ZoneSizing(CtrlZoneNum,DesDayNum)%HeatOutTempSeq(NumOfTimeStepInDay))
      ALLOCATE(ZoneSizing(CtrlZoneNum,DesDayNum)%CoolOutTempSeq(NumOfTimeStepInDay))
      ALLOCATE(ZoneSizing(CtrlZoneNum,DesDayNum)%HeatZoneRetTempSeq(NumOfTimeStepInDay))
      ALLOCATE(ZoneSizing(CtrlZoneNum,DesDayNum)%HeatTstatTempSeq(NumOfTimeStepInDay))
      ALLOCATE(ZoneSizing(CtrlZoneNum,DesDayNum)%CoolZoneRetTempSeq(NumOfTimeStepInDay))
      ALLOCATE(ZoneSizing(CtrlZoneNum,DesDayNum)%CoolTstatTempSeq(NumOfTimeStepInDay))
      ALLOCATE(ZoneSizing(CtrlZoneNum,DesDayNum)%HeatZoneHumRatSeq(NumOfTimeStepInDay))
      ALLOCATE(ZoneSizing(CtrlZoneNum,DesDayNum)%CoolZoneHumRatSeq(NumOfTimeStepInDay))
      ALLOCATE(ZoneSizing(CtrlZoneNum,DesDayNum)%HeatOutHumRatSeq(NumOfTimeStepInDay))
      ALLOCATE(ZoneSizing(CtrlZoneNum,DesDayNum)%CoolOutHumRatSeq(NumOfTimeStepInDay))
      ALLOCATE(CalcZoneSizing(CtrlZoneNum,DesDayNum)%HeatFlowSeq(NumOfTimeStepInDay))
      ALLOCATE(CalcZoneSizing(CtrlZoneNum,DesDayNum)%CoolFlowSeq(NumOfTimeStepInDay))
      ALLOCATE(CalcZoneSizing(CtrlZoneNum,DesDayNum)%HeatLoadSeq(NumOfTimeStepInDay))
      ALLOCATE(CalcZoneSizing(CtrlZoneNum,DesDayNum)%CoolLoadSeq(NumOfTimeStepInDay))
      ALLOCATE(CalcZoneSizing(CtrlZoneNum,DesDayNum)%HeatZoneTempSeq(NumOfTimeStepInDay))
      ALLOCATE(CalcZoneSizing(CtrlZoneNum,DesDayNum)%CoolZoneTempSeq(NumOfTimeStepInDay))
      ALLOCATE(CalcZoneSizing(CtrlZoneNum,DesDayNum)%HeatOutTempSeq(NumOfTimeStepInDay))
      ALLOCATE(CalcZoneSizing(CtrlZoneNum,DesDayNum)%CoolOutTempSeq(NumOfTimeStepInDay))
      ALLOCATE(CalcZoneSizing(CtrlZoneNum,DesDayNum)%HeatZoneRetTempSeq(NumOfTimeStepInDay))
      ALLOCATE(CalcZoneSizing(CtrlZoneNum,DesDayNum)%HeatTstatTempSeq(NumOfTimeStepInDay))
      ALLOCATE(CalcZoneSizing(CtrlZoneNum,DesDayNum)%CoolZoneRetTempSeq(NumOfTimeStepInDay))
      ALLOCATE(CalcZoneSizing(CtrlZoneNum,DesDayNum)%CoolTstatTempSeq(NumOfTimeStepInDay))
      ALLOCATE(CalcZoneSizing(CtrlZoneNum,DesDayNum)%HeatZoneHumRatSeq(NumOfTimeStepInDay))
      ALLOCATE(CalcZoneSizing(CtrlZoneNum,DesDayNum)%CoolZoneHumRatSeq(NumOfTimeStepInDay))
      ALLOCATE(CalcZoneSizing(CtrlZoneNum,DesDayNum)%HeatOutHumRatSeq(NumOfTimeStepInDay))
      ALLOCATE(CalcZoneSizing(CtrlZoneNum,DesDayNum)%CoolOutHumRatSeq(NumOfTimeStepInDay))
      DO TimeStepIndex=1,NumOfTimeStepInDay
        ZoneSizing(CtrlZoneNum,DesDayNum)%HeatFlowSeq(TimeStepIndex) = 0.0d0
        ZoneSizing(CtrlZoneNum,DesDayNum)%CoolFlowSeq(TimeStepIndex) = 0.0d0
        ZoneSizing(CtrlZoneNum,DesDayNum)%HeatLoadSeq(TimeStepIndex) = 0.0d0
        ZoneSizing(CtrlZoneNum,DesDayNum)%CoolLoadSeq(TimeStepIndex) = 0.0d0
        ZoneSizing(CtrlZoneNum,DesDayNum)%HeatZoneTempSeq(TimeStepIndex) = 0.0d0
        ZoneSizing(CtrlZoneNum,DesDayNum)%DesHeatSetPtSeq(TimeStepIndex) = 0.0d0
        ZoneSizing(CtrlZoneNum,DesDayNum)%CoolZoneTempSeq(TimeStepIndex) = 0.0d0
        ZoneSizing(CtrlZoneNum,DesDayNum)%DesCoolSetPtSeq(TimeStepIndex) = 0.0d0
        ZoneSizing(CtrlZoneNum,DesDayNum)%HeatOutTempSeq(TimeStepIndex) = 0.0d0
        ZoneSizing(CtrlZoneNum,DesDayNum)%CoolOutTempSeq(TimeStepIndex) = 0.0d0
        ZoneSizing(CtrlZoneNum,DesDayNum)%HeatZoneRetTempSeq(TimeStepIndex) = 0.0d0
        ZoneSizing(CtrlZoneNum,DesDayNum)%HeatTstatTempSeq(TimeStepIndex) = 0.0d0
        ZoneSizing(CtrlZoneNum,DesDayNum)%CoolZoneRetTempSeq(TimeStepIndex) = 0.0d0
        ZoneSizing(CtrlZoneNum,DesDayNum)%CoolTstatTempSeq(TimeStepIndex) = 0.0d0
        ZoneSizing(CtrlZoneNum,DesDayNum)%HeatZoneHumRatSeq(TimeStepIndex) = 0.0d0
        ZoneSizing(CtrlZoneNum,DesDayNum)%CoolZoneHumRatSeq(TimeStepIndex) = 0.0d0
        ZoneSizing(CtrlZoneNum,DesDayNum)%HeatOutHumRatSeq(TimeStepIndex) = 0.0d0
        ZoneSizing(CtrlZoneNum,DesDayNum)%CoolOutHumRatSeq(TimeStepIndex) = 0.0d0
        CalcZoneSizing(CtrlZoneNum,DesDayNum)%HeatFlowSeq(TimeStepIndex) = 0.0d0
        CalcZoneSizing(CtrlZoneNum,DesDayNum)%CoolFlowSeq(TimeStepIndex) = 0.0d0
        CalcZoneSizing(CtrlZoneNum,DesDayNum)%HeatLoadSeq(TimeStepIndex) = 0.0d0
        CalcZoneSizing(CtrlZoneNum,DesDayNum)%CoolLoadSeq(TimeStepIndex) = 0.0d0
        CalcZoneSizing(CtrlZoneNum,DesDayNum)%HeatZoneTempSeq(TimeStepIndex) = 0.0d0
        CalcZoneSizing(CtrlZoneNum,DesDayNum)%CoolZoneTempSeq(TimeStepIndex) = 0.0d0
        CalcZoneSizing(CtrlZoneNum,DesDayNum)%HeatOutTempSeq(TimeStepIndex) = 0.0d0
        CalcZoneSizing(CtrlZoneNum,DesDayNum)%CoolOutTempSeq(TimeStepIndex) = 0.0d0
        CalcZoneSizing(CtrlZoneNum,DesDayNum)%HeatZoneRetTempSeq(TimeStepIndex) = 0.0d0
        CalcZoneSizing(CtrlZoneNum,DesDayNum)%HeatTstatTempSeq(TimeStepIndex) = 0.0d0
        CalcZoneSizing(CtrlZoneNum,DesDayNum)%CoolZoneRetTempSeq(TimeStepIndex) = 0.0d0
        CalcZoneSizing(CtrlZoneNum,DesDayNum)%CoolTstatTempSeq(TimeStepIndex) = 0.0d0
        CalcZoneSizing(CtrlZoneNum,DesDayNum)%HeatZoneHumRatSeq(TimeStepIndex) = 0.0d0
        CalcZoneSizing(CtrlZoneNum,DesDayNum)%CoolZoneHumRatSeq(TimeStepIndex) = 0.0d0
        CalcZoneSizing(CtrlZoneNum,DesDayNum)%HeatOutHumRatSeq(TimeStepIndex) = 0.0d0
        CalcZoneSizing(CtrlZoneNum,DesDayNum)%CoolOutHumRatSeq(TimeStepIndex) = 0.0d0
     END DO
    END DO
  END DO

  DO CtrlZoneNum = 1,NumOfZones
    IF (.not. ZoneEquipConfig(CtrlZoneNum)%IsControlled) CYCLE
    FinalZoneSizing(CtrlZoneNum)%ZoneName = ZoneEquipConfig(CtrlZoneNum)%ZoneName
    FinalZoneSizing(CtrlZoneNum)%ActualZoneNum = ZoneEquipConfig(CtrlZoneNum)%ActualZoneNum
    IF (ZoneEquipConfig(CtrlZoneNum)%NumInletNodes > 0) THEN
      FinalZoneSizing(CtrlZoneNum)%SupplyAirNode = ZoneEquipConfig(CtrlZoneNum)%InletNode(1)
    END IF
    CalcFinalZoneSizing(CtrlZoneNum)%ZoneName = ZoneEquipConfig(CtrlZoneNum)%ZoneName
    CalcFinalZoneSizing(CtrlZoneNum)%ActualZoneNum = ZoneEquipConfig(CtrlZoneNum)%ActualZoneNum
    IF (ZoneEquipConfig(CtrlZoneNum)%NumInletNodes > 0) THEN
      CalcFinalZoneSizing(CtrlZoneNum)%SupplyAirNode = ZoneEquipConfig(CtrlZoneNum)%InletNode(1)
    END IF
    ZoneSizNum = FindItemInList(ZoneEquipConfig(CtrlZoneNum)%ZoneName,ZoneSizingInput%ZoneName,NumZoneSizingInput)
    IF (ZoneSizNum > 0) THEN  ! move data from zone sizing input
      FinalZoneSizing(CtrlZoneNum)%ZnCoolDgnSAMethod = ZoneSizingInput(ZoneSizNum)%ZnCoolDgnSAMethod
      FinalZoneSizing(CtrlZoneNum)%ZnHeatDgnSAMethod = ZoneSizingInput(ZoneSizNum)%ZnHeatDgnSAMethod
      FinalZoneSizing(CtrlZoneNum)%CoolDesTemp = ZoneSizingInput(ZoneSizNum)%CoolDesTemp
      FinalZoneSizing(CtrlZoneNum)%HeatDesTemp = ZoneSizingInput(ZoneSizNum)%HeatDesTemp
      FinalZoneSizing(CtrlZoneNum)%CoolDesTempDiff = ZoneSizingInput(ZoneSizNum)%CoolDesTempDiff
      FinalZoneSizing(CtrlZoneNum)%HeatDesTempDiff = ZoneSizingInput(ZoneSizNum)%HeatDesTempDiff
      FinalZoneSizing(CtrlZoneNum)%CoolDesHumRat = ZoneSizingInput(ZoneSizNum)%CoolDesHumRat
      FinalZoneSizing(CtrlZoneNum)%HeatDesHumRat = ZoneSizingInput(ZoneSizNum)%HeatDesHumRat
      FinalZoneSizing(CtrlZoneNum)%ZoneDesignSpecOAIndex = ZoneSizingInput(ZoneSizNum)%ZoneDesignSpecOAIndex
      FinalZoneSizing(CtrlZoneNum)%OADesMethod = ZoneSizingInput(ZoneSizNum)%OADesMethod
      FinalZoneSizing(CtrlZoneNum)%DesOAFlowPPer = ZoneSizingInput(ZoneSizNum)%DesOAFlowPPer
      FinalZoneSizing(CtrlZoneNum)%DesOAFlowPerArea = ZoneSizingInput(ZoneSizNum)%DesOAFlowPerArea
      FinalZoneSizing(CtrlZoneNum)%DesOAFlow = ZoneSizingInput(ZoneSizNum)%DesOAFlow
      FinalZoneSizing(CtrlZoneNum)%CoolAirDesMethod = ZoneSizingInput(ZoneSizNum)%CoolAirDesMethod
      FinalZoneSizing(CtrlZoneNum)%HeatAirDesMethod = ZoneSizingInput(ZoneSizNum)%HeatAirDesMethod
      FinalZoneSizing(CtrlZoneNum)%InpDesCoolAirFlow = ZoneSizingInput(ZoneSizNum)%DesCoolAirFlow
      FinalZoneSizing(CtrlZoneNum)%DesCoolMinAirFlowPerArea = ZoneSizingInput(ZoneSizNum)%DesCoolMinAirFlowPerArea
      FinalZoneSizing(CtrlZoneNum)%DesCoolMinAirFlow = ZoneSizingInput(ZoneSizNum)%DesCoolMinAirFlow
      FinalZoneSizing(CtrlZoneNum)%DesCoolMinAirFlowFrac = ZoneSizingInput(ZoneSizNum)%DesCoolMinAirFlowFrac
      FinalZoneSizing(CtrlZoneNum)%InpDesHeatAirFlow = ZoneSizingInput(ZoneSizNum)%DesHeatAirFlow
      FinalZoneSizing(CtrlZoneNum)%DesHeatMaxAirFlowPerArea = ZoneSizingInput(ZoneSizNum)%DesHeatMaxAirFlowPerArea
      FinalZoneSizing(CtrlZoneNum)%DesHeatMaxAirFlow = ZoneSizingInput(ZoneSizNum)%DesHeatMaxAirFlow
      FinalZoneSizing(CtrlZoneNum)%DesHeatMaxAirFlowFrac = ZoneSizingInput(ZoneSizNum)%DesHeatMaxAirFlowFrac
      FinalZoneSizing(CtrlZoneNum)%HeatSizingFactor = ZoneSizingInput(ZoneSizNum)%HeatSizingFactor
      FinalZoneSizing(CtrlZoneNum)%CoolSizingFactor = ZoneSizingInput(ZoneSizNum)%CoolSizingFactor
      FinalZoneSizing(CtrlZoneNum)%ZoneADEffCooling = ZoneSizingInput(ZoneSizNum)%ZoneADEffCooling
      FinalZoneSizing(CtrlZoneNum)%ZoneADEffHeating = ZoneSizingInput(ZoneSizNum)%ZoneADEffHeating
      FinalZoneSizing(CtrlZoneNum)%ZoneSecondaryRecirculation = ZoneSizingInput(ZoneSizNum)%ZoneSecondaryRecirculation
      CalcFinalZoneSizing(CtrlZoneNum)%ZnCoolDgnSAMethod = ZoneSizingInput(ZoneSizNum)%ZnCoolDgnSAMethod
      CalcFinalZoneSizing(CtrlZoneNum)%ZnHeatDgnSAMethod = ZoneSizingInput(ZoneSizNum)%ZnHeatDgnSAMethod
      CalcFinalZoneSizing(CtrlZoneNum)%CoolDesTemp = ZoneSizingInput(ZoneSizNum)%CoolDesTemp
      CalcFinalZoneSizing(CtrlZoneNum)%HeatDesTemp = ZoneSizingInput(ZoneSizNum)%HeatDesTemp
      CalcFinalZoneSizing(CtrlZoneNum)%CoolDesTempDiff = ZoneSizingInput(ZoneSizNum)%CoolDesTempDiff
      CalcFinalZoneSizing(CtrlZoneNum)%HeatDesTempDiff = ZoneSizingInput(ZoneSizNum)%HeatDesTempDiff
      CalcFinalZoneSizing(CtrlZoneNum)%CoolDesHumRat = ZoneSizingInput(ZoneSizNum)%CoolDesHumRat
      CalcFinalZoneSizing(CtrlZoneNum)%HeatDesHumRat = ZoneSizingInput(ZoneSizNum)%HeatDesHumRat
      CalcFinalZoneSizing(CtrlZoneNum)%ZoneDesignSpecOAIndex = ZoneSizingInput(ZoneSizNum)%ZoneDesignSpecOAIndex
      CalcFinalZoneSizing(CtrlZoneNum)%OADesMethod = ZoneSizingInput(ZoneSizNum)%OADesMethod
      CalcFinalZoneSizing(CtrlZoneNum)%DesOAFlowPPer = ZoneSizingInput(ZoneSizNum)%DesOAFlowPPer
      CalcFinalZoneSizing(CtrlZoneNum)%DesOAFlowPerArea = ZoneSizingInput(ZoneSizNum)%DesOAFlowPerArea
      CalcFinalZoneSizing(CtrlZoneNum)%DesOAFlow = ZoneSizingInput(ZoneSizNum)%DesOAFlow
      CalcFinalZoneSizing(CtrlZoneNum)%CoolAirDesMethod = ZoneSizingInput(ZoneSizNum)%CoolAirDesMethod
      CalcFinalZoneSizing(CtrlZoneNum)%HeatAirDesMethod = ZoneSizingInput(ZoneSizNum)%HeatAirDesMethod
      CalcFinalZoneSizing(CtrlZoneNum)%InpDesCoolAirFlow = ZoneSizingInput(ZoneSizNum)%DesCoolAirFlow
      CalcFinalZoneSizing(CtrlZoneNum)%DesCoolMinAirFlowPerArea = ZoneSizingInput(ZoneSizNum)%DesCoolMinAirFlowPerArea
      CalcFinalZoneSizing(CtrlZoneNum)%DesCoolMinAirFlow = ZoneSizingInput(ZoneSizNum)%DesCoolMinAirFlow
      CalcFinalZoneSizing(CtrlZoneNum)%DesCoolMinAirFlowFrac = ZoneSizingInput(ZoneSizNum)%DesCoolMinAirFlowFrac
      CalcFinalZoneSizing(CtrlZoneNum)%InpDesHeatAirFlow = ZoneSizingInput(ZoneSizNum)%DesHeatAirFlow
      CalcFinalZoneSizing(CtrlZoneNum)%DesHeatMaxAirFlowPerArea = ZoneSizingInput(ZoneSizNum)%DesHeatMaxAirFlowPerArea
      CalcFinalZoneSizing(CtrlZoneNum)%DesHeatMaxAirFlow = ZoneSizingInput(ZoneSizNum)%DesHeatMaxAirFlow
      CalcFinalZoneSizing(CtrlZoneNum)%DesHeatMaxAirFlowFrac = ZoneSizingInput(ZoneSizNum)%DesHeatMaxAirFlowFrac
      CalcFinalZoneSizing(CtrlZoneNum)%HeatSizingFactor = ZoneSizingInput(ZoneSizNum)%HeatSizingFactor
      CalcFinalZoneSizing(CtrlZoneNum)%CoolSizingFactor = ZoneSizingInput(ZoneSizNum)%CoolSizingFactor
      CalcFinalZoneSizing(CtrlZoneNum)%ZoneADEffCooling = ZoneSizingInput(ZoneSizNum)%ZoneADEffCooling
      CalcFinalZoneSizing(CtrlZoneNum)%ZoneADEffHeating = ZoneSizingInput(ZoneSizNum)%ZoneADEffHeating
    ELSE ! Every controlled zone must be simulated, so set missing inputs to the first
      FinalZoneSizing(CtrlZoneNum)%ZnCoolDgnSAMethod = ZoneSizingInput(1)%ZnCoolDgnSAMethod
      FinalZoneSizing(CtrlZoneNum)%ZnHeatDgnSAMethod = ZoneSizingInput(1)%ZnHeatDgnSAMethod
      FinalZoneSizing(CtrlZoneNum)%CoolDesTemp = ZoneSizingInput(1)%CoolDesTemp
      FinalZoneSizing(CtrlZoneNum)%HeatDesTemp = ZoneSizingInput(1)%HeatDesTemp
      FinalZoneSizing(CtrlZoneNum)%CoolDesTempDiff = ZoneSizingInput(1)%CoolDesTempDiff
      FinalZoneSizing(CtrlZoneNum)%HeatDesTempDiff = ZoneSizingInput(1)%HeatDesTempDiff
      FinalZoneSizing(CtrlZoneNum)%CoolDesHumRat = ZoneSizingInput(1)%CoolDesHumRat
      FinalZoneSizing(CtrlZoneNum)%HeatDesHumRat = ZoneSizingInput(1)%HeatDesHumRat
      FinalZoneSizing(CtrlZoneNum)%ZoneDesignSpecOAIndex = ZoneSizingInput(1)%ZoneDesignSpecOAIndex
      FinalZoneSizing(CtrlZoneNum)%OADesMethod = ZoneSizingInput(1)%OADesMethod
      FinalZoneSizing(CtrlZoneNum)%DesOAFlowPPer = ZoneSizingInput(1)%DesOAFlowPPer
      FinalZoneSizing(CtrlZoneNum)%DesOAFlowPerArea = ZoneSizingInput(1)%DesOAFlowPerArea
      FinalZoneSizing(CtrlZoneNum)%DesOAFlow = ZoneSizingInput(1)%DesOAFlow
      FinalZoneSizing(CtrlZoneNum)%CoolAirDesMethod = ZoneSizingInput(1)%CoolAirDesMethod
      FinalZoneSizing(CtrlZoneNum)%HeatAirDesMethod = ZoneSizingInput(1)%HeatAirDesMethod
      FinalZoneSizing(CtrlZoneNum)%InpDesCoolAirFlow = ZoneSizingInput(1)%DesCoolAirFlow
      FinalZoneSizing(CtrlZoneNum)%DesCoolMinAirFlowPerArea = ZoneSizingInput(1)%DesCoolMinAirFlowPerArea
      FinalZoneSizing(CtrlZoneNum)%DesCoolMinAirFlow = ZoneSizingInput(1)%DesCoolMinAirFlow
      FinalZoneSizing(CtrlZoneNum)%DesCoolMinAirFlowFrac = ZoneSizingInput(1)%DesCoolMinAirFlowFrac
      FinalZoneSizing(CtrlZoneNum)%InpDesHeatAirFlow = ZoneSizingInput(1)%DesHeatAirFlow
      FinalZoneSizing(CtrlZoneNum)%DesHeatMaxAirFlowPerArea = ZoneSizingInput(1)%DesHeatMaxAirFlowPerArea
      FinalZoneSizing(CtrlZoneNum)%DesHeatMaxAirFlow = ZoneSizingInput(1)%DesHeatMaxAirFlow
      FinalZoneSizing(CtrlZoneNum)%DesHeatMaxAirFlowFrac = ZoneSizingInput(1)%DesHeatMaxAirFlowFrac
      FinalZoneSizing(CtrlZoneNum)%HeatSizingFactor = ZoneSizingInput(1)%HeatSizingFactor
      FinalZoneSizing(CtrlZoneNum)%CoolSizingFactor = ZoneSizingInput(1)%CoolSizingFactor
      FinalZoneSizing(CtrlZoneNum)%ZoneADEffCooling = ZoneSizingInput(1)%ZoneADEffCooling
      FinalZoneSizing(CtrlZoneNum)%ZoneADEffHeating = ZoneSizingInput(1)%ZoneADEffHeating
      FinalZoneSizing(CtrlZoneNum)%ZoneSecondaryRecirculation = ZoneSizingInput(1)%ZoneSecondaryRecirculation
      CalcFinalZoneSizing(CtrlZoneNum)%ZnCoolDgnSAMethod = ZoneSizingInput(1)%ZnCoolDgnSAMethod
      CalcFinalZoneSizing(CtrlZoneNum)%ZnHeatDgnSAMethod = ZoneSizingInput(1)%ZnHeatDgnSAMethod
      CalcFinalZoneSizing(CtrlZoneNum)%CoolDesTemp = ZoneSizingInput(1)%CoolDesTemp
      CalcFinalZoneSizing(CtrlZoneNum)%HeatDesTemp = ZoneSizingInput(1)%HeatDesTemp
      CalcFinalZoneSizing(CtrlZoneNum)%CoolDesTempDiff = ZoneSizingInput(1)%CoolDesTempDiff
      CalcFinalZoneSizing(CtrlZoneNum)%HeatDesTempDiff = ZoneSizingInput(1)%HeatDesTempDiff
      CalcFinalZoneSizing(CtrlZoneNum)%CoolDesHumRat = ZoneSizingInput(1)%CoolDesHumRat
      CalcFinalZoneSizing(CtrlZoneNum)%HeatDesHumRat = ZoneSizingInput(1)%HeatDesHumRat
      CalcFinalZoneSizing(CtrlZoneNum)%ZoneDesignSpecOAIndex = ZoneSizingInput(1)%ZoneDesignSpecOAIndex
      CalcFinalZoneSizing(CtrlZoneNum)%OADesMethod = ZoneSizingInput(1)%OADesMethod
      CalcFinalZoneSizing(CtrlZoneNum)%DesOAFlowPPer = ZoneSizingInput(1)%DesOAFlowPPer
      CalcFinalZoneSizing(CtrlZoneNum)%DesOAFlowPerArea = ZoneSizingInput(1)%DesOAFlowPerArea
      CalcFinalZoneSizing(CtrlZoneNum)%DesOAFlow = ZoneSizingInput(1)%DesOAFlow
      CalcFinalZoneSizing(CtrlZoneNum)%CoolAirDesMethod = ZoneSizingInput(1)%CoolAirDesMethod
      CalcFinalZoneSizing(CtrlZoneNum)%HeatAirDesMethod = ZoneSizingInput(1)%HeatAirDesMethod
      CalcFinalZoneSizing(CtrlZoneNum)%InpDesCoolAirFlow = ZoneSizingInput(1)%DesCoolAirFlow
      CalcFinalZoneSizing(CtrlZoneNum)%DesCoolMinAirFlowPerArea = ZoneSizingInput(1)%DesCoolMinAirFlowPerArea
      CalcFinalZoneSizing(CtrlZoneNum)%DesCoolMinAirFlow = ZoneSizingInput(1)%DesCoolMinAirFlow
      CalcFinalZoneSizing(CtrlZoneNum)%DesCoolMinAirFlowFrac = ZoneSizingInput(1)%DesCoolMinAirFlowFrac
      CalcFinalZoneSizing(CtrlZoneNum)%InpDesHeatAirFlow = ZoneSizingInput(1)%DesHeatAirFlow
      CalcFinalZoneSizing(CtrlZoneNum)%DesHeatMaxAirFlowPerArea = ZoneSizingInput(1)%DesHeatMaxAirFlowPerArea
      CalcFinalZoneSizing(CtrlZoneNum)%DesHeatMaxAirFlow = ZoneSizingInput(1)%DesHeatMaxAirFlow
      CalcFinalZoneSizing(CtrlZoneNum)%DesHeatMaxAirFlowFrac = ZoneSizingInput(1)%DesHeatMaxAirFlowFrac
      CalcFinalZoneSizing(CtrlZoneNum)%HeatSizingFactor = ZoneSizingInput(1)%HeatSizingFactor
      CalcFinalZoneSizing(CtrlZoneNum)%CoolSizingFactor = ZoneSizingInput(1)%CoolSizingFactor
      CalcFinalZoneSizing(CtrlZoneNum)%ZoneADEffCooling = ZoneSizingInput(1)%ZoneADEffCooling
      CalcFinalZoneSizing(CtrlZoneNum)%ZoneADEffHeating = ZoneSizingInput(1)%ZoneADEffHeating
    END IF
    ALLOCATE(FinalZoneSizing(CtrlZoneNum)%HeatFlowSeq(NumOfTimeStepInDay))
    ALLOCATE(FinalZoneSizing(CtrlZoneNum)%CoolFlowSeq(NumOfTimeStepInDay))
    ALLOCATE(FinalZoneSizing(CtrlZoneNum)%HeatLoadSeq(NumOfTimeStepInDay))
    ALLOCATE(FinalZoneSizing(CtrlZoneNum)%CoolLoadSeq(NumOfTimeStepInDay))
    ALLOCATE(FinalZoneSizing(CtrlZoneNum)%HeatZoneTempSeq(NumOfTimeStepInDay))
    ALLOCATE(FinalZoneSizing(CtrlZoneNum)%CoolZoneTempSeq(NumOfTimeStepInDay))
    ALLOCATE(FinalZoneSizing(CtrlZoneNum)%HeatOutTempSeq(NumOfTimeStepInDay))
    ALLOCATE(FinalZoneSizing(CtrlZoneNum)%CoolOutTempSeq(NumOfTimeStepInDay))
    ALLOCATE(FinalZoneSizing(CtrlZoneNum)%HeatZoneRetTempSeq(NumOfTimeStepInDay))
    ALLOCATE(FinalZoneSizing(CtrlZoneNum)%HeatTstatTempSeq(NumOfTimeStepInDay))
    ALLOCATE(FinalZoneSizing(CtrlZoneNum)%CoolZoneRetTempSeq(NumOfTimeStepInDay))
    ALLOCATE(FinalZoneSizing(CtrlZoneNum)%CoolTstatTempSeq(NumOfTimeStepInDay))
    ALLOCATE(FinalZoneSizing(CtrlZoneNum)%HeatZoneHumRatSeq(NumOfTimeStepInDay))
    ALLOCATE(FinalZoneSizing(CtrlZoneNum)%CoolZoneHumRatSeq(NumOfTimeStepInDay))
    ALLOCATE(FinalZoneSizing(CtrlZoneNum)%HeatOutHumRatSeq(NumOfTimeStepInDay))
    ALLOCATE(FinalZoneSizing(CtrlZoneNum)%CoolOutHumRatSeq(NumOfTimeStepInDay))
    ALLOCATE(CalcFinalZoneSizing(CtrlZoneNum)%HeatFlowSeq(NumOfTimeStepInDay))
    ALLOCATE(CalcFinalZoneSizing(CtrlZoneNum)%CoolFlowSeq(NumOfTimeStepInDay))
    ALLOCATE(CalcFinalZoneSizing(CtrlZoneNum)%HeatLoadSeq(NumOfTimeStepInDay))
    ALLOCATE(CalcFinalZoneSizing(CtrlZoneNum)%CoolLoadSeq(NumOfTimeStepInDay))
    ALLOCATE(CalcFinalZoneSizing(CtrlZoneNum)%HeatZoneTempSeq(NumOfTimeStepInDay))
    ALLOCATE(CalcFinalZoneSizing(CtrlZoneNum)%CoolZoneTempSeq(NumOfTimeStepInDay))
    ALLOCATE(CalcFinalZoneSizing(CtrlZoneNum)%HeatOutTempSeq(NumOfTimeStepInDay))
    ALLOCATE(CalcFinalZoneSizing(CtrlZoneNum)%CoolOutTempSeq(NumOfTimeStepInDay))
    ALLOCATE(CalcFinalZoneSizing(CtrlZoneNum)%HeatZoneRetTempSeq(NumOfTimeStepInDay))
    ALLOCATE(CalcFinalZoneSizing(CtrlZoneNum)%HeatTstatTempSeq(NumOfTimeStepInDay))
    ALLOCATE(CalcFinalZoneSizing(CtrlZoneNum)%CoolZoneRetTempSeq(NumOfTimeStepInDay))
    ALLOCATE(CalcFinalZoneSizing(CtrlZoneNum)%CoolTstatTempSeq(NumOfTimeStepInDay))
    ALLOCATE(CalcFinalZoneSizing(CtrlZoneNum)%HeatZoneHumRatSeq(NumOfTimeStepInDay))
    ALLOCATE(CalcFinalZoneSizing(CtrlZoneNum)%CoolZoneHumRatSeq(NumOfTimeStepInDay))
    ALLOCATE(CalcFinalZoneSizing(CtrlZoneNum)%HeatOutHumRatSeq(NumOfTimeStepInDay))
    ALLOCATE(CalcFinalZoneSizing(CtrlZoneNum)%CoolOutHumRatSeq(NumOfTimeStepInDay))
   DO TimeStepIndex=1,NumOfTimeStepInDay
      FinalZoneSizing(CtrlZoneNum)%HeatFlowSeq(TimeStepIndex) = 0.0d0
      FinalZoneSizing(CtrlZoneNum)%CoolFlowSeq(TimeStepIndex) = 0.0d0
      FinalZoneSizing(CtrlZoneNum)%HeatLoadSeq(TimeStepIndex) = 0.0d0
      FinalZoneSizing(CtrlZoneNum)%CoolLoadSeq(TimeStepIndex) = 0.0d0
      FinalZoneSizing(CtrlZoneNum)%HeatZoneTempSeq(TimeStepIndex) = 0.0d0
      FinalZoneSizing(CtrlZoneNum)%CoolZoneTempSeq(TimeStepIndex) = 0.0d0
      FinalZoneSizing(CtrlZoneNum)%HeatOutTempSeq(TimeStepIndex) = 0.0d0
      FinalZoneSizing(CtrlZoneNum)%CoolOutTempSeq(TimeStepIndex) = 0.0d0
      FinalZoneSizing(CtrlZoneNum)%HeatZoneRetTempSeq(TimeStepIndex) = 0.0d0
      FinalZoneSizing(CtrlZoneNum)%HeatTstatTempSeq(TimeStepIndex) = 0.0d0
      FinalZoneSizing(CtrlZoneNum)%CoolZoneRetTempSeq(TimeStepIndex) = 0.0d0
      FinalZoneSizing(CtrlZoneNum)%CoolTstatTempSeq(TimeStepIndex) = 0.0d0
      FinalZoneSizing(CtrlZoneNum)%HeatZoneHumRatSeq(TimeStepIndex) = 0.0d0
      FinalZoneSizing(CtrlZoneNum)%CoolZoneHumRatSeq(TimeStepIndex) = 0.0d0
      FinalZoneSizing(CtrlZoneNum)%HeatOutHumRatSeq(TimeStepIndex) = 0.0d0
      FinalZoneSizing(CtrlZoneNum)%CoolOutHumRatSeq(TimeStepIndex) = 0.0d0
      CalcFinalZoneSizing(CtrlZoneNum)%HeatFlowSeq(TimeStepIndex) = 0.0d0
      CalcFinalZoneSizing(CtrlZoneNum)%CoolFlowSeq(TimeStepIndex) = 0.0d0
      CalcFinalZoneSizing(CtrlZoneNum)%HeatLoadSeq(TimeStepIndex) = 0.0d0
      CalcFinalZoneSizing(CtrlZoneNum)%CoolLoadSeq(TimeStepIndex) = 0.0d0
      CalcFinalZoneSizing(CtrlZoneNum)%HeatZoneTempSeq(TimeStepIndex) = 0.0d0
      CalcFinalZoneSizing(CtrlZoneNum)%CoolZoneTempSeq(TimeStepIndex) = 0.0d0
      CalcFinalZoneSizing(CtrlZoneNum)%HeatOutTempSeq(TimeStepIndex) = 0.0d0
      CalcFinalZoneSizing(CtrlZoneNum)%CoolOutTempSeq(TimeStepIndex) = 0.0d0
      CalcFinalZoneSizing(CtrlZoneNum)%HeatZoneRetTempSeq(TimeStepIndex) = 0.0d0
      CalcFinalZoneSizing(CtrlZoneNum)%HeatTstatTempSeq(TimeStepIndex) = 0.0d0
      CalcFinalZoneSizing(CtrlZoneNum)%CoolZoneRetTempSeq(TimeStepIndex) = 0.0d0
      CalcFinalZoneSizing(CtrlZoneNum)%CoolTstatTempSeq(TimeStepIndex) = 0.0d0
      CalcFinalZoneSizing(CtrlZoneNum)%HeatZoneHumRatSeq(TimeStepIndex) = 0.0d0
      CalcFinalZoneSizing(CtrlZoneNum)%CoolZoneHumRatSeq(TimeStepIndex) = 0.0d0
      CalcFinalZoneSizing(CtrlZoneNum)%HeatOutHumRatSeq(TimeStepIndex) = 0.0d0
      CalcFinalZoneSizing(CtrlZoneNum)%CoolOutHumRatSeq(TimeStepIndex) = 0.0d0
    END DO

    ! setup CalcFinalZoneSizing structure for use with EMS, some as sensors, some as actuators
    IF (AnyEnergyManagementSystemInModel) Then !

      !actuate  REAL(r64)             :: DesHeatMassFlow          = 0.0d0   ! zone design heating air mass flow rate [kg/s]
      CALL SetupEMSInternalVariable('Final Zone Design Heating Air Mass Flow Rate', FinalZoneSizing(CtrlZoneNum)%ZoneName, &
                                    '[kg/s]', FinalZoneSizing(CtrlZoneNum)%DesHeatMassFlow )
      CALL SetupEMSInternalVariable('Intermediate Zone Design Heating Air Mass Flow Rate', &
                                     CalcFinalZoneSizing(CtrlZoneNum)%ZoneName, '[kg/s]', &
                                     CalcFinalZoneSizing(CtrlZoneNum)%DesHeatMassFlow )
      CALL SetupEMSActuator('Sizing:Zone', CalcFinalZoneSizing(CtrlZoneNum)%ZoneName, 'Zone Design Heating Air Mass Flow Rate', &
                            '[kg/s]', CalcFinalZoneSizing(CtrlZoneNum)%EMSOverrideDesHeatMassOn, &
                            CalcFinalZoneSizing(CtrlZoneNum)%EMSValueDesHeatMassFlow )

      !actuate  REAL(r64)             :: DesCoolMassFlow          = 0.0d0   ! zone design cooling air mass flow rate [kg/s]
      CALL SetupEMSInternalVariable('Final Zone Design Cooling Air Mass Flow Rate', FinalZoneSizing(CtrlZoneNum)%ZoneName, &
                                    '[kg/s]', FinalZoneSizing(CtrlZoneNum)%DesCoolMassFlow )
      CALL SetupEMSInternalVariable('Intermediate Zone Design Cooling Air Mass Flow Rate', &
                                     CalcFinalZoneSizing(CtrlZoneNum)%ZoneName, '[kg/s]', &
                                     CalcFinalZoneSizing(CtrlZoneNum)%DesCoolMassFlow )
      CALL SetupEMSActuator('Sizing:Zone', CalcFinalZoneSizing(CtrlZoneNum)%ZoneName, 'Zone Design Cooling Air Mass Flow Rate', &
                            '[kg/s]',  CalcFinalZoneSizing(CtrlZoneNum)%EMSOverrideDesCoolMassOn, &
                            CalcFinalZoneSizing(CtrlZoneNum)%EMSValueDesCoolMassFlow )

      !actuate  REAL(r64)             :: DesHeatLoad              = 0.0d0   ! zone design heating load [W]
      CALL SetupEMSInternalVariable('Final Zone Design Heating Load', FinalZoneSizing(CtrlZoneNum)%ZoneName, '[W]', &
                                    FinalZoneSizing(CtrlZoneNum)%DesHeatLoad )
      CALL SetupEMSInternalVariable('Intermediate Zone Design Heating Load', CalcFinalZoneSizing(CtrlZoneNum)%ZoneName, '[W]', &
                                    CalcFinalZoneSizing(CtrlZoneNum)%DesHeatLoad )
      CALL SetupEMSActuator('Sizing:Zone', CalcFinalZoneSizing(CtrlZoneNum)%ZoneName, 'Zone Design Heating Load', '[W]', &
                        CalcFinalZoneSizing(CtrlZoneNum)%EMSOverrideDesHeatLoadOn, &
                        CalcFinalZoneSizing(CtrlZoneNum)%EMSValueDesHeatLoad )

      !actuate  REAL(r64)             :: DesCoolLoad              = 0.0d0   ! zone design cooling load [W]
      CALL SetupEMSInternalVariable('Final Zone Design Cooling Load', FinalZoneSizing(CtrlZoneNum)%ZoneName, '[W]', &
                                    FinalZoneSizing(CtrlZoneNum)%DesCoolLoad )
      CALL SetupEMSInternalVariable('Intermediate Zone Design Cooling Load', CalcFinalZoneSizing(CtrlZoneNum)%ZoneName, '[W]', &
                                    CalcFinalZoneSizing(CtrlZoneNum)%DesCoolLoad )
      CALL SetupEMSActuator('Sizing:Zone', CalcFinalZoneSizing(CtrlZoneNum)%ZoneName, 'Zone Design Cooling Load', '[W]', &
                        CalcFinalZoneSizing(CtrlZoneNum)%EMSOverrideDesCoolLoadOn, &
                        CalcFinalZoneSizing(CtrlZoneNum)%EMSValueDesCoolLoad )

      !sensor?  REAL(r64)             :: DesHeatDens              = 0.0d0   ! zone design heating air density [kg/m3]
      CALL SetupEMSInternalVariable('Final Zone Design Heating Air Density', FinalZoneSizing(CtrlZoneNum)%ZoneName, '[kg/m3]', &
                                    FinalZoneSizing(CtrlZoneNum)%DesHeatDens )
      CALL SetupEMSInternalVariable('Intermediate Zone Design Heating Air Density', CalcFinalZoneSizing(CtrlZoneNum)%ZoneName, &
                                    '[kg/m3]', CalcFinalZoneSizing(CtrlZoneNum)%DesHeatDens )
      !sensor?  REAL(r64)             :: DesCoolDens              = 0.0d0   ! zone design cooling air density [kg/m3]
      CALL SetupEMSInternalVariable('Final Zone Design Cooling Air Density', FinalZoneSizing(CtrlZoneNum)%ZoneName, '[kg/m3]', &
                                    FinalZoneSizing(CtrlZoneNum)%DesCoolDens )
      CALL SetupEMSInternalVariable('Intermediate Zone Design Cooling Air Density', CalcFinalZoneSizing(CtrlZoneNum)%ZoneName, &
                                    '[kg/m3]', CalcFinalZoneSizing(CtrlZoneNum)%DesCoolDens )

      !actuate  REAL(r64)             :: DesHeatVolFlow           = 0.0d0   ! zone design heating air volume flow rate [m3/s]
      CALL SetupEMSInternalVariable('Final Zone Design Heating Volume Flow', FinalZoneSizing(CtrlZoneNum)%ZoneName, '[m3/s]', &
                                    FinalZoneSizing(CtrlZoneNum)%DesHeatVolFlow )
      CALL SetupEMSInternalVariable('Intermediate Zone Design Heating Volume Flow', CalcFinalZoneSizing(CtrlZoneNum)%ZoneName, &
                                    '[m3/s]', CalcFinalZoneSizing(CtrlZoneNum)%DesHeatVolFlow )
      CALL SetupEMSActuator('Sizing:Zone', CalcFinalZoneSizing(CtrlZoneNum)%ZoneName, 'Zone Design Heating Vol Flow', '[m3/s]', &
                            CalcFinalZoneSizing(CtrlZoneNum)%EMSOverrideDesHeatVolOn, &
                            CalcFinalZoneSizing(CtrlZoneNum)%EMSValueDesHeatVolFlow )

      !actuate  REAL(r64)             :: DesCoolVolFlow           = 0.0d0   ! zone design cooling air volume flow rate [m3/s]
      CALL SetupEMSInternalVariable('Final Zone Design Cooling Volume Flow', FinalZoneSizing(CtrlZoneNum)%ZoneName, '[m3/s]', &
                                    FinalZoneSizing(CtrlZoneNum)%DesCoolVolFlow )
      CALL SetupEMSInternalVariable('Intermediate Zone Design Cooling Volume Flow', CalcFinalZoneSizing(CtrlZoneNum)%ZoneName, &
                                    '[m3/s]', CalcFinalZoneSizing(CtrlZoneNum)%DesCoolVolFlow )
      CALL SetupEMSActuator('Sizing:Zone', CalcFinalZoneSizing(CtrlZoneNum)%ZoneName, 'Zone Design Cooling Vol Flow', '[m3/s]', &
                        CalcFinalZoneSizing(CtrlZoneNum)%EMSOverrideDesCoolVolOn, &
                        CalcFinalZoneSizing(CtrlZoneNum)%EMSValueDesCoolVolFlow )

      !actuate  REAL(r64)          :: DesHeatVolFlowMax        = 0.0d0   ! zone design heating maximum air volume flow rate [m3/s]
      !actuate  REAL(r64)          :: DesCoolVolFlowMin        = 0.0d0   ! zone design cooling minimum air volume flow rate [m3/s]

      CALL SetupEMSInternalVariable('Zone Outdoor Air Design Volume Flow Rate', CalcFinalZoneSizing(CtrlZoneNum)%ZoneName, &
                                    '[m3/s]', CalcFinalZoneSizing(CtrlZoneNum)%MinOA )

    ENDIF

  END DO
  ! Use the max occupancy data from the PEOPLE structure to calculate design min OA for each zone
  ! Calculate the zone design minimum outside air flow rate from the 3 Zone Sizing OA inputs and
  ! from the specified OA method
  DO CtrlZoneNum = 1,NumOfZones
    IF (.not. ZoneEquipConfig(CtrlZoneNum)%IsControlled) CYCLE
    ! Use the max occupancy data from the PEOPLE structure to calculate design min OA for each zone
    ! from the outside air flow per person input
    TotPeopleInZone = 0.0d0
    ZoneIndex = FinalZoneSizing(CtrlZoneNum)%ActualZoneNum
    DO PeopleNum=1,TotPeople
      IF (People(PeopleNum)%ZonePtr == FinalZoneSizing(CtrlZoneNum)%ActualZoneNum) THEN
        TotPeopleInZone = TotPeopleInZone + (People(PeopleNum)%NumberOfPeople &
                               * Zone(FinalZoneSizing(CtrlZoneNum)%ActualZoneNum)%Multiplier &
                               * Zone(FinalZoneSizing(CtrlZoneNum)%ActualZoneNum)%ListMultiplier)
        SchMax = GetScheduleMaxValue(People(PeopleNum)%NumberOfPeoplePtr)
        IF (SchMax > 0) THEN
          FinalZoneSizing(CtrlZoneNum)%ZonePeakOccupancy = TotPeopleInZone*SchMax
        ELSE
          FinalZoneSizing(CtrlZoneNum)%ZonePeakOccupancy = TotPeopleInZone
        ENDIF
      END IF
    END DO
    FinalZoneSizing(CtrlZoneNum)%TotalZoneFloorArea = (Zone(ZoneIndex)%FloorArea &
                               * Zone(FinalZoneSizing(CtrlZoneNum)%ActualZoneNum)%Multiplier &
                               * Zone(FinalZoneSizing(CtrlZoneNum)%ActualZoneNum)%ListMultiplier)
    OAFromPeople = FinalZoneSizing(CtrlZoneNum)%DesOAFlowPPer * TotPeopleInZone
    OAFromArea = FinalZoneSizing(CtrlZoneNum)%DesOAFlowPerArea * FinalZoneSizing(CtrlZoneNum)%TotalZoneFloorArea
    FinalZoneSizing(CtrlZoneNum)%TotPeopleInZone = TotPeopleInZone
    FinalZoneSizing(CtrlZoneNum)%TotalOAFromPeople = OAFromPeople
    FinalZoneSizing(CtrlZoneNum)%TotalOAFromArea = OAFromArea
    ! Calculate the design min OA flow rate for this zone
    UseOccSchFlag = .FALSE.
    UseMinOASchFlag = .FALSE.
    DSOAPtr = FinalZoneSizing(CtrlZoneNum)%ZoneDesignSpecOAIndex
    OAVolumeFlowRate = CalcDesignSpecificationOutdoorAir(DSOAPtr, ZoneIndex, UseOccSchFlag, UseMinOASchFlag)

    ! Zone(ZoneIndex)%Multiplier and Zone(ZoneIndex)%ListMultiplier applied in CalcDesignSpecificationOutdoorAir
    FinalZoneSizing(CtrlZoneNum)%MinOA = OAVolumeFlowRate
    CalcFinalZoneSizing(CtrlZoneNum)%MinOA = OAVolumeFlowRate
    IF (FinalZoneSizing(CtrlZoneNum)%ZoneADEffCooling > 0.0d0 .OR. FinalZoneSizing(CtrlZoneNum)%ZoneADEffHeating > 0.0d0) THEN
      FinalZoneSizing(CtrlZoneNum)%MinOA = FinalZoneSizing(CtrlZoneNum)%MinOA / Min(FinalZoneSizing(CtrlZoneNum)%ZoneADEffCooling, &
                                                 FinalZoneSizing(CtrlZoneNum)%ZoneADEffHeating)
      CalcFinalZoneSizing(CtrlZoneNum)%MinOA = FinalZoneSizing(CtrlZoneNum)%MinOA
    END IF
    ! calculated zone design flow rates automatically take into account zone multipliers, since the zone
    ! loads are multiplied (in ZoneTempPredictorCorrector.f90). Flow rates derived directly from
    ! user inputs need to be explicitly multiplied by the zone multipliers.
    FinalZoneSizing(CtrlZoneNum)%DesCoolMinAirFlow2 = FinalZoneSizing(CtrlZoneNum)%DesCoolMinAirFlowPerArea   &
                                                      * Zone(ZoneIndex)%FloorArea &
                                                      *  Zone(ZoneIndex)%Multiplier * Zone(ZoneIndex)%ListMultiplier
    CalcFinalZoneSizing(CtrlZoneNum)%DesCoolMinAirFlow2 = CalcFinalZoneSizing(CtrlZoneNum)%DesCoolMinAirFlowPerArea   &
                                                      * Zone(ZoneIndex)%FloorArea &
                                                      *  Zone(ZoneIndex)%Multiplier * Zone(ZoneIndex)%ListMultiplier
    FinalZoneSizing(CtrlZoneNum)%DesHeatMaxAirFlow2 = FinalZoneSizing(CtrlZoneNum)%DesHeatMaxAirFlowPerArea   &
                                                      * Zone(ZoneIndex)%FloorArea &
                                                      *  Zone(ZoneIndex)%Multiplier * Zone(ZoneIndex)%ListMultiplier
    CalcFinalZoneSizing(CtrlZoneNum)%DesHeatMaxAirFlow2 = CalcFinalZoneSizing(CtrlZoneNum)%DesHeatMaxAirFlowPerArea   &
                                                      * Zone(ZoneIndex)%FloorArea &
                                                      *  Zone(ZoneIndex)%Multiplier * Zone(ZoneIndex)%ListMultiplier
    FinalZoneSizing(CtrlZoneNum)%DesCoolMinAirFlow = FinalZoneSizing(CtrlZoneNum)%DesCoolMinAirFlow &
                                                      *  Zone(ZoneIndex)%Multiplier * Zone(ZoneIndex)%ListMultiplier
    CalcFinalZoneSizing(CtrlZoneNum)%DesCoolMinAirFlow = CalcFinalZoneSizing(CtrlZoneNum)%DesCoolMinAirFlow &
                                                      *  Zone(ZoneIndex)%Multiplier * Zone(ZoneIndex)%ListMultiplier
    FinalZoneSizing(CtrlZoneNum)%DesHeatMaxAirFlow = FinalZoneSizing(CtrlZoneNum)%DesHeatMaxAirFlow &
                                                      *  Zone(ZoneIndex)%Multiplier * Zone(ZoneIndex)%ListMultiplier
    CalcFinalZoneSizing(CtrlZoneNum)%DesHeatMaxAirFlow = CalcFinalZoneSizing(CtrlZoneNum)%DesHeatMaxAirFlow &
                                                      *  Zone(ZoneIndex)%Multiplier * Zone(ZoneIndex)%ListMultiplier
    FinalZoneSizing(CtrlZoneNum)%InpDesCoolAirFlow = FinalZoneSizing(CtrlZoneNum)%InpDesCoolAirFlow &
                                                      *  Zone(ZoneIndex)%Multiplier * Zone(ZoneIndex)%ListMultiplier
    CalcFinalZoneSizing(CtrlZoneNum)%InpDesCoolAirFlow = CalcFinalZoneSizing(CtrlZoneNum)%InpDesCoolAirFlow &
                                                      *  Zone(ZoneIndex)%Multiplier * Zone(ZoneIndex)%ListMultiplier
    FinalZoneSizing(CtrlZoneNum)%InpDesHeatAirFlow = FinalZoneSizing(CtrlZoneNum)%InpDesHeatAirFlow &
                                                      *  Zone(ZoneIndex)%Multiplier * Zone(ZoneIndex)%ListMultiplier
    CalcFinalZoneSizing(CtrlZoneNum)%InpDesHeatAirFlow = CalcFinalZoneSizing(CtrlZoneNum)%InpDesHeatAirFlow &
                                                      *  Zone(ZoneIndex)%Multiplier * Zone(ZoneIndex)%ListMultiplier

    DO DesDayNum=1,TotDesDays+TotRunDesPersDays
      ZoneSizing(CtrlZoneNum,DesDayNum)%MinOA = FinalZoneSizing(CtrlZoneNum)%MinOA
      CalcZoneSizing(CtrlZoneNum,DesDayNum)%MinOA = CalcFinalZoneSizing(CtrlZoneNum)%MinOA
      ZoneSizing(CtrlZoneNum,DesDayNum)%DesCoolMinAirFlow2 = FinalZoneSizing(CtrlZoneNum)%DesCoolMinAirFlow2
      CalcZoneSizing(CtrlZoneNum,DesDayNum)%DesCoolMinAirFlow2 = CalcFinalZoneSizing(CtrlZoneNum)%DesCoolMinAirFlow2
      ZoneSizing(CtrlZoneNum,DesDayNum)%DesCoolMinAirFlow = FinalZoneSizing(CtrlZoneNum)%DesCoolMinAirFlow
      CalcZoneSizing(CtrlZoneNum,DesDayNum)%DesCoolMinAirFlow = CalcFinalZoneSizing(CtrlZoneNum)%DesCoolMinAirFlow
      ZoneSizing(CtrlZoneNum,DesDayNum)%DesHeatMaxAirFlow2 = FinalZoneSizing(CtrlZoneNum)%DesHeatMaxAirFlow2
      CalcZoneSizing(CtrlZoneNum,DesDayNum)%DesHeatMaxAirFlow2 = CalcFinalZoneSizing(CtrlZoneNum)%DesHeatMaxAirFlow2
      ZoneSizing(CtrlZoneNum,DesDayNum)%DesHeatMaxAirFlow = FinalZoneSizing(CtrlZoneNum)%DesHeatMaxAirFlow
      CalcZoneSizing(CtrlZoneNum,DesDayNum)%DesHeatMaxAirFlow = CalcFinalZoneSizing(CtrlZoneNum)%DesHeatMaxAirFlow
    END DO
  END DO

  WRITE (OutputFileInits, 890)
  WRITE (OutputFileInits, 891) NumTimeStepsInAvg
  WRITE (OutputFileInits, 990)
  WRITE (OutputFileInits, 991) GlobalHeatSizingFactor
  DO CtrlZoneNum = 1,NumOfZones
    IF (.not. ZoneEquipConfig(CtrlZoneNum)%IsControlled) CYCLE
    IF (FinalZoneSizing(CtrlZoneNum)%HeatSizingFactor .NE. 1.0d0) THEN
      WRITE (OutputFileInits, 992) TRIM(FinalZoneSizing(CtrlZoneNum)%ZoneName), FinalZoneSizing(CtrlZoneNum)%HeatSizingFactor
    END IF
  END DO
  WRITE (OutputFileInits, 993)
  WRITE (OutputFileInits, 994) GlobalCoolSizingFactor
  DO CtrlZoneNum = 1,NumOfZones
    IF (.not. ZoneEquipConfig(CtrlZoneNum)%IsControlled) CYCLE
    IF (FinalZoneSizing(CtrlZoneNum)%CoolSizingFactor .NE. 1.0d0) THEN
      WRITE (OutputFileInits, 995) TRIM(FinalZoneSizing(CtrlZoneNum)%ZoneName), FinalZoneSizing(CtrlZoneNum)%CoolSizingFactor
    END IF
  END DO

  890 FORMAT('! <Load Timesteps in Zone Design Calculation Averaging Window>, Value')
  891 FORMAT(' Load Timesteps in Zone Design Calculation Averaging Window, ',I4)
  990 FORMAT('! <Heating Sizing Factor Information>, Sizing Factor ID, Value')
  991 FORMAT(' Heating Sizing Factor Information, Global, ',G12.5)
  992 FORMAT(' Heating Sizing Factor Information, Zone ',A,', ',G12.5)
  993 FORMAT('! <Cooling Sizing Factor Information>, Sizing Factor ID, Value')
  994 FORMAT(' Cooling Sizing Factor Information, Global, ',G12.5)
  995 FORMAT(' Cooling Sizing Factor Information, Zone ',A,', ',G12.5)

  RETURN

END SUBROUTINE SetUpZoneSizingArrays

SUBROUTINE RezeroZoneSizingArrays

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Jason Glazer
          !       DATE WRITTEN   August 2012 based on SetUpZoneSizingArrays
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Zero zone sizing arrays between the pulse and normal sizing.

          ! METHODOLOGY EMPLOYED:
          ! Based on SetUpZoneSizingArrays but remove allocates and other calculations.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:

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
  INTEGER :: DesDayNum   ! design day index
  INTEGER :: CtrlZoneNum ! controlled zone index
  INTEGER :: TimeStepIndex      ! zone time step index

  CALL DisplayString('Re-zeroing zone sizing arrays')

  DO DesDayNum=1,TotDesDays+TotRunDesPersDays
    DO CtrlZoneNum = 1,NumOfZones
      DO TimeStepIndex=1,NumOfTimeStepInDay
        IF (ALLOCATED(ZoneSizing(CtrlZoneNum,DesDayNum)%HeatFlowSeq)) THEN
          ZoneSizing(CtrlZoneNum,DesDayNum)%HeatFlowSeq(TimeStepIndex) = 0.0d0
          ZoneSizing(CtrlZoneNum,DesDayNum)%HeatLoadSeq(TimeStepIndex) = 0.0d0
          !not used directly in output report
          ZoneSizing(CtrlZoneNum,DesDayNum)%HeatZoneTempSeq(TimeStepIndex) = 0.0d0
          ZoneSizing(CtrlZoneNum,DesDayNum)%DesHeatSetPtSeq(TimeStepIndex) = 0.0d0
          ZoneSizing(CtrlZoneNum,DesDayNum)%HeatOutTempSeq(TimeStepIndex) = 0.0d0
          ZoneSizing(CtrlZoneNum,DesDayNum)%HeatZoneRetTempSeq(TimeStepIndex) = 0.0d0
          ZoneSizing(CtrlZoneNum,DesDayNum)%HeatTstatTempSeq(TimeStepIndex) = 0.0d0
          ZoneSizing(CtrlZoneNum,DesDayNum)%HeatZoneHumRatSeq(TimeStepIndex) = 0.0d0
          ZoneSizing(CtrlZoneNum,DesDayNum)%HeatOutHumRatSeq(TimeStepIndex) = 0.0d0
        END IF
        IF (ALLOCATED(CalcZoneSizing(CtrlZoneNum,DesDayNum)%HeatFlowSeq)) THEN
          CalcZoneSizing(CtrlZoneNum,DesDayNum)%HeatFlowSeq(TimeStepIndex) = 0.0d0
          CalcZoneSizing(CtrlZoneNum,DesDayNum)%HeatLoadSeq(TimeStepIndex) = 0.0d0
          CalcZoneSizing(CtrlZoneNum,DesDayNum)%HeatZoneTempSeq(TimeStepIndex) = 0.0d0
          CalcZoneSizing(CtrlZoneNum,DesDayNum)%HeatOutTempSeq(TimeStepIndex) = 0.0d0
          CalcZoneSizing(CtrlZoneNum,DesDayNum)%HeatZoneRetTempSeq(TimeStepIndex) = 0.0d0
          CalcZoneSizing(CtrlZoneNum,DesDayNum)%HeatTstatTempSeq(TimeStepIndex) = 0.0d0
          CalcZoneSizing(CtrlZoneNum,DesDayNum)%HeatZoneHumRatSeq(TimeStepIndex) = 0.0d0
          CalcZoneSizing(CtrlZoneNum,DesDayNum)%HeatOutHumRatSeq(TimeStepIndex) = 0.0d0
        END IF
        IF (ALLOCATED(ZoneSizing(CtrlZoneNum,DesDayNum)%CoolFlowSeq)) THEN
          ZoneSizing(CtrlZoneNum,DesDayNum)%CoolFlowSeq(TimeStepIndex) = 0.0d0
          ZoneSizing(CtrlZoneNum,DesDayNum)%CoolLoadSeq(TimeStepIndex) = 0.0d0
          !not used directly in output report
          ZoneSizing(CtrlZoneNum,DesDayNum)%CoolZoneTempSeq(TimeStepIndex) = 0.0d0
          ZoneSizing(CtrlZoneNum,DesDayNum)%DesCoolSetPtSeq(TimeStepIndex) = 0.0d0
          ZoneSizing(CtrlZoneNum,DesDayNum)%CoolOutTempSeq(TimeStepIndex) = 0.0d0
          ZoneSizing(CtrlZoneNum,DesDayNum)%CoolZoneRetTempSeq(TimeStepIndex) = 0.0d0
          ZoneSizing(CtrlZoneNum,DesDayNum)%CoolTstatTempSeq(TimeStepIndex) = 0.0d0
          ZoneSizing(CtrlZoneNum,DesDayNum)%CoolZoneHumRatSeq(TimeStepIndex) = 0.0d0
          ZoneSizing(CtrlZoneNum,DesDayNum)%CoolOutHumRatSeq(TimeStepIndex) = 0.0d0
        END IF
        IF (ALLOCATED(CalcZoneSizing(CtrlZoneNum,DesDayNum)%CoolFlowSeq)) THEN
          CalcZoneSizing(CtrlZoneNum,DesDayNum)%CoolFlowSeq(TimeStepIndex) = 0.0d0
          CalcZoneSizing(CtrlZoneNum,DesDayNum)%CoolLoadSeq(TimeStepIndex) = 0.0d0
          CalcZoneSizing(CtrlZoneNum,DesDayNum)%CoolZoneTempSeq(TimeStepIndex) = 0.0d0
          CalcZoneSizing(CtrlZoneNum,DesDayNum)%CoolOutTempSeq(TimeStepIndex) = 0.0d0
          CalcZoneSizing(CtrlZoneNum,DesDayNum)%CoolZoneRetTempSeq(TimeStepIndex) = 0.0d0
          CalcZoneSizing(CtrlZoneNum,DesDayNum)%CoolTstatTempSeq(TimeStepIndex) = 0.0d0
          CalcZoneSizing(CtrlZoneNum,DesDayNum)%CoolZoneHumRatSeq(TimeStepIndex) = 0.0d0
          CalcZoneSizing(CtrlZoneNum,DesDayNum)%CoolOutHumRatSeq(TimeStepIndex) = 0.0d0
        END IF
      END DO
      ZoneSizing(CtrlZoneNum,DesDayNum)%CoolDesDay               = ' '     ! name of a cooling design day
      ZoneSizing(CtrlZoneNum,DesDayNum)%HeatDesDay               = ' '     ! name of a heating design day

      ZoneSizing(CtrlZoneNum,DesDayNum)%DesHeatMassFlow          = 0.0d0   ! zone design heating air mass flow rate [kg/s]
      ZoneSizing(CtrlZoneNum,DesDayNum)%DesCoolMassFlow          = 0.0d0   ! zone design cooling air mass flow rate [kg/s]
      ZoneSizing(CtrlZoneNum,DesDayNum)%DesHeatLoad              = 0.0d0   ! zone design heating load [W]
      ZoneSizing(CtrlZoneNum,DesDayNum)%DesCoolLoad              = 0.0d0   ! zone design cooling load [W]
      ZoneSizing(CtrlZoneNum,DesDayNum)%DesHeatDens              = 0.0d0   ! zone design heating air density [kg/m3]
      ZoneSizing(CtrlZoneNum,DesDayNum)%DesCoolDens              = 0.0d0   ! zone design cooling air density [kg/m3]
      ZoneSizing(CtrlZoneNum,DesDayNum)%DesHeatVolFlow           = 0.0d0   ! zone design heating air volume flow rate [m3/s]
      ZoneSizing(CtrlZoneNum,DesDayNum)%DesCoolVolFlow           = 0.0d0   ! zone design cooling air volume flow rate [m3/s]
      ZoneSizing(CtrlZoneNum,DesDayNum)%DesHeatVolFlowMax        = 0.0d0   ! zone design heating maximum air volume flow rate [m3/s]
      ZoneSizing(CtrlZoneNum,DesDayNum)%DesCoolVolFlowMin        = 0.0d0   ! zone design cooling minimum air volume flow rate [m3/s]
      ZoneSizing(CtrlZoneNum,DesDayNum)%DesHeatCoilInTemp        = 0.0d0   ! zone heating coil design air inlet temperature [C]
      ZoneSizing(CtrlZoneNum,DesDayNum)%DesCoolCoilInTemp        = 0.0d0   ! zone cooling coil design air inlet temperature [C]
      ZoneSizing(CtrlZoneNum,DesDayNum)%DesHeatCoilInHumRat      = 0.0d0   ! zone heating coil design air inlet humidity ratio [kg/kg]
      ZoneSizing(CtrlZoneNum,DesDayNum)%DesCoolCoilInHumRat      = 0.0d0   ! zone cooling coil design air inlet humidity ratio [kg/kg]
      ZoneSizing(CtrlZoneNum,DesDayNum)%DesHeatCoilInTempTU      = 0.0d0   ! zone heating coil design air inlet temperature (supply air)([C]
      ZoneSizing(CtrlZoneNum,DesDayNum)%DesCoolCoilInTempTU      = 0.0d0   ! zone cooling coil design air inlet temperature (supply air)[C]
      ZoneSizing(CtrlZoneNum,DesDayNum)%DesHeatCoilInHumRatTU    = 0.0d0   ! zone heating coil design air inlet humidity ratio
      ZoneSizing(CtrlZoneNum,DesDayNum)%DesCoolCoilInHumRatTU    = 0.0d0   ! zone cooling coil design air inlet humidity ratio
      ZoneSizing(CtrlZoneNum,DesDayNum)%HeatMassFlow             = 0.0d0   ! current zone heating air mass flow rate (HVAC time step)
      ZoneSizing(CtrlZoneNum,DesDayNum)%CoolMassFlow             = 0.0d0   ! current zone cooling air mass flow rate (HVAC time step)
      ZoneSizing(CtrlZoneNum,DesDayNum)%HeatLoad                 = 0.0d0   ! current zone heating load (HVAC time step)
      ZoneSizing(CtrlZoneNum,DesDayNum)%CoolLoad                 = 0.0d0   ! current zone heating load (HVAC time step)
      ZoneSizing(CtrlZoneNum,DesDayNum)%HeatZoneTemp             = 0.0d0   ! current zone temperature (heating, time step)
      ZoneSizing(CtrlZoneNum,DesDayNum)%HeatOutTemp              = 0.0d0   ! current outdoor temperature (heating, time step)
      ZoneSizing(CtrlZoneNum,DesDayNum)%HeatZoneRetTemp          = 0.0d0   ! current zone return temperature (heating, time step)
      ZoneSizing(CtrlZoneNum,DesDayNum)%HeatTstatTemp            = 0.0d0   ! current zone thermostat temperature (heating, time step)
      ZoneSizing(CtrlZoneNum,DesDayNum)%CoolZoneTemp             = 0.0d0   ! current zone temperature (cooling, time step)
      ZoneSizing(CtrlZoneNum,DesDayNum)%CoolOutTemp              = 0.0d0   ! current Outdoor temperature (cooling, time step)
      ZoneSizing(CtrlZoneNum,DesDayNum)%CoolZoneRetTemp          = 0.0d0   ! current zone return temperature (cooling, time step)
      ZoneSizing(CtrlZoneNum,DesDayNum)%CoolTstatTemp            = 0.0d0   ! current zone thermostat temperature (cooling, time step)
      ZoneSizing(CtrlZoneNum,DesDayNum)%HeatZoneHumRat           = 0.0d0   ! current zone humidity ratio (heating, time step)
      ZoneSizing(CtrlZoneNum,DesDayNum)%CoolZoneHumRat           = 0.0d0   ! current zone humidity ratio (cooling, time step)
      ZoneSizing(CtrlZoneNum,DesDayNum)%HeatOutHumRat            = 0.0d0   ! current outdoor humidity ratio (heating, time step)
      ZoneSizing(CtrlZoneNum,DesDayNum)%CoolOutHumRat            = 0.0d0   ! current outdoor humidity ratio (cooling, time step)
      ZoneSizing(CtrlZoneNum,DesDayNum)%ZoneTempAtHeatPeak       = 0.0d0   ! zone temp at max heating [C]
      ZoneSizing(CtrlZoneNum,DesDayNum)%ZoneRetTempAtHeatPeak    = 0.0d0   ! zone return temp at max heating [C]
      ZoneSizing(CtrlZoneNum,DesDayNum)%OutTempAtHeatPeak        = 0.0d0   ! outdoor temperature at max heating [C]
      ZoneSizing(CtrlZoneNum,DesDayNum)%ZoneTempAtCoolPeak       = 0.0d0   ! zone temp at max cooling [C]
      ZoneSizing(CtrlZoneNum,DesDayNum)%ZoneRetTempAtCoolPeak    = 0.0d0   ! zone return temp at max cooling [C]
      ZoneSizing(CtrlZoneNum,DesDayNum)%OutTempAtCoolPeak        = 0.0d0   ! outdoor temperature at max cooling [C]
      ZoneSizing(CtrlZoneNum,DesDayNum)%ZoneHumRatAtHeatPeak     = 0.0d0   ! zone humidity ratio at max heating [kg/kg]
      ZoneSizing(CtrlZoneNum,DesDayNum)%ZoneHumRatAtCoolPeak     = 0.0d0   ! zone humidity ratio at max cooling [kg/kg]
      ZoneSizing(CtrlZoneNum,DesDayNum)%OutHumRatAtHeatPeak      = 0.0d0   ! outdoor humidity at max heating [kg/kg]
      ZoneSizing(CtrlZoneNum,DesDayNum)%OutHumRatAtCoolPeak      = 0.0d0   ! outdoor humidity at max cooling [kg/kg]
      ZoneSizing(CtrlZoneNum,DesDayNum)%TimeStepNumAtHeatMax     = 0       ! time step number (in day) at Heating peak
      ZoneSizing(CtrlZoneNum,DesDayNum)%TimeStepNumAtCoolMax     = 0       ! time step number (in day) at cooling peak
      ZoneSizing(CtrlZoneNum,DesDayNum)%HeatDDNum                = 0       ! design day index of design day causing heating peak
      ZoneSizing(CtrlZoneNum,DesDayNum)%CoolDDNum                = 0       ! design day index of design day causing heating peak
      ZoneSizing(CtrlZoneNum,DesDayNum)%cHeatDDDate              = ' '     ! date of design day causing heating peak
      ZoneSizing(CtrlZoneNum,DesDayNum)%cCoolDDDate              = ' '     ! date of design day causing cooling peak

      CalcZoneSizing(CtrlZoneNum,DesDayNum)%CoolDesDay               = ' '     ! name of a cooling design day
      CalcZoneSizing(CtrlZoneNum,DesDayNum)%HeatDesDay               = ' '     ! name of a heating design day

      CalcZoneSizing(CtrlZoneNum,DesDayNum)%DesHeatMassFlow          = 0.0d0   ! zone design heating air mass flow rate [kg/s]
      CalcZoneSizing(CtrlZoneNum,DesDayNum)%DesCoolMassFlow          = 0.0d0   ! zone design cooling air mass flow rate [kg/s]
      CalcZoneSizing(CtrlZoneNum,DesDayNum)%DesHeatLoad              = 0.0d0   ! zone design heating load [W]
      CalcZoneSizing(CtrlZoneNum,DesDayNum)%DesCoolLoad              = 0.0d0   ! zone design cooling load [W]
      CalcZoneSizing(CtrlZoneNum,DesDayNum)%DesHeatDens              = 0.0d0   ! zone design heating air density [kg/m3]
      CalcZoneSizing(CtrlZoneNum,DesDayNum)%DesCoolDens              = 0.0d0   ! zone design cooling air density [kg/m3]
      CalcZoneSizing(CtrlZoneNum,DesDayNum)%DesHeatVolFlow           = 0.0d0   ! zone design heating air volume flow rate [m3/s]
      CalcZoneSizing(CtrlZoneNum,DesDayNum)%DesCoolVolFlow           = 0.0d0   ! zone design cooling air volume flow rate [m3/s]
      CalcZoneSizing(CtrlZoneNum,DesDayNum)%DesHeatVolFlowMax        = 0.0d0   ! zone design heating maximum air volume flow rate [m3/s]
      CalcZoneSizing(CtrlZoneNum,DesDayNum)%DesCoolVolFlowMin        = 0.0d0   ! zone design cooling minimum air volume flow rate [m3/s]
      CalcZoneSizing(CtrlZoneNum,DesDayNum)%DesHeatCoilInTemp        = 0.0d0   ! zone heating coil design air inlet temperature [C]
      CalcZoneSizing(CtrlZoneNum,DesDayNum)%DesCoolCoilInTemp        = 0.0d0   ! zone cooling coil design air inlet temperature [C]
      CalcZoneSizing(CtrlZoneNum,DesDayNum)%DesHeatCoilInHumRat      = 0.0d0   ! zone heating coil design air inlet humidity ratio [kg/kg]
      CalcZoneSizing(CtrlZoneNum,DesDayNum)%DesCoolCoilInHumRat      = 0.0d0   ! zone cooling coil design air inlet humidity ratio [kg/kg]
      CalcZoneSizing(CtrlZoneNum,DesDayNum)%DesHeatCoilInTempTU      = 0.0d0   ! zone heating coil design air inlet temperature (supply air)([C]
      CalcZoneSizing(CtrlZoneNum,DesDayNum)%DesCoolCoilInTempTU      = 0.0d0   ! zone cooling coil design air inlet temperature (supply air)[C]
      CalcZoneSizing(CtrlZoneNum,DesDayNum)%DesHeatCoilInHumRatTU    = 0.0d0   ! zone heating coil design air inlet humidity ratio
      CalcZoneSizing(CtrlZoneNum,DesDayNum)%DesCoolCoilInHumRatTU    = 0.0d0   ! zone cooling coil design air inlet humidity ratio
      CalcZoneSizing(CtrlZoneNum,DesDayNum)%HeatMassFlow             = 0.0d0   ! current zone heating air mass flow rate (HVAC time step)
      CalcZoneSizing(CtrlZoneNum,DesDayNum)%CoolMassFlow             = 0.0d0   ! current zone cooling air mass flow rate (HVAC time step)
      CalcZoneSizing(CtrlZoneNum,DesDayNum)%HeatLoad                 = 0.0d0   ! current zone heating load (HVAC time step)
      CalcZoneSizing(CtrlZoneNum,DesDayNum)%CoolLoad                 = 0.0d0   ! current zone heating load (HVAC time step)
      CalcZoneSizing(CtrlZoneNum,DesDayNum)%HeatZoneTemp             = 0.0d0   ! current zone temperature (heating, time step)
      CalcZoneSizing(CtrlZoneNum,DesDayNum)%HeatOutTemp              = 0.0d0   ! current outdoor temperature (heating, time step)
      CalcZoneSizing(CtrlZoneNum,DesDayNum)%HeatZoneRetTemp          = 0.0d0   ! current zone return temperature (heating, time step)
      CalcZoneSizing(CtrlZoneNum,DesDayNum)%HeatTstatTemp            = 0.0d0   ! current zone thermostat temperature (heating, time step)
      CalcZoneSizing(CtrlZoneNum,DesDayNum)%CoolZoneTemp             = 0.0d0   ! current zone temperature (cooling, time step)
      CalcZoneSizing(CtrlZoneNum,DesDayNum)%CoolOutTemp              = 0.0d0   ! current Outdoor temperature (cooling, time step)
      CalcZoneSizing(CtrlZoneNum,DesDayNum)%CoolZoneRetTemp          = 0.0d0   ! current zone return temperature (cooling, time step)
      CalcZoneSizing(CtrlZoneNum,DesDayNum)%CoolTstatTemp            = 0.0d0   ! current zone Tstat temperature (cooling, time step)
      CalcZoneSizing(CtrlZoneNum,DesDayNum)%HeatZoneHumRat           = 0.0d0   ! current zone humidity ratio (heating, time step)
      CalcZoneSizing(CtrlZoneNum,DesDayNum)%CoolZoneHumRat           = 0.0d0   ! current zone humidity ratio (cooling, time step)
      CalcZoneSizing(CtrlZoneNum,DesDayNum)%HeatOutHumRat            = 0.0d0   ! current outdoor humidity ratio (heating, time step)
      CalcZoneSizing(CtrlZoneNum,DesDayNum)%CoolOutHumRat            = 0.0d0   ! current outdoor humidity ratio (cooling, time step)
      CalcZoneSizing(CtrlZoneNum,DesDayNum)%ZoneTempAtHeatPeak       = 0.0d0   ! zone temp at max heating [C]
      CalcZoneSizing(CtrlZoneNum,DesDayNum)%ZoneRetTempAtHeatPeak    = 0.0d0   ! zone return temp at max heating [C]
      CalcZoneSizing(CtrlZoneNum,DesDayNum)%OutTempAtHeatPeak        = 0.0d0   ! outdoor temperature at max heating [C]
      CalcZoneSizing(CtrlZoneNum,DesDayNum)%ZoneTempAtCoolPeak       = 0.0d0   ! zone temp at max cooling [C]
      CalcZoneSizing(CtrlZoneNum,DesDayNum)%ZoneRetTempAtCoolPeak    = 0.0d0   ! zone return temp at max cooling [C]
      CalcZoneSizing(CtrlZoneNum,DesDayNum)%OutTempAtCoolPeak        = 0.0d0   ! outdoor temperature at max cooling [C]
      CalcZoneSizing(CtrlZoneNum,DesDayNum)%ZoneHumRatAtHeatPeak     = 0.0d0   ! zone humidity ratio at max heating [kg/kg]
      CalcZoneSizing(CtrlZoneNum,DesDayNum)%ZoneHumRatAtCoolPeak     = 0.0d0   ! zone humidity ratio at max cooling [kg/kg]
      CalcZoneSizing(CtrlZoneNum,DesDayNum)%OutHumRatAtHeatPeak      = 0.0d0   ! outdoor humidity at max heating [kg/kg]
      CalcZoneSizing(CtrlZoneNum,DesDayNum)%OutHumRatAtCoolPeak      = 0.0d0   ! outdoor humidity at max cooling [kg/kg]
      CalcZoneSizing(CtrlZoneNum,DesDayNum)%TimeStepNumAtHeatMax     = 0       ! time step number (in day) at Heating peak
      CalcZoneSizing(CtrlZoneNum,DesDayNum)%TimeStepNumAtCoolMax     = 0       ! time step number (in day) at cooling peak
      CalcZoneSizing(CtrlZoneNum,DesDayNum)%HeatDDNum                = 0       ! design day index of design day causing heating peak
      CalcZoneSizing(CtrlZoneNum,DesDayNum)%CoolDDNum                = 0       ! design day index of design day causing heating peak
      CalcZoneSizing(CtrlZoneNum,DesDayNum)%cHeatDDDate              = ' '     ! date of design day causing heating peak
      CalcZoneSizing(CtrlZoneNum,DesDayNum)%cCoolDDDate              = ' '     ! date of design day causing cooling peak
    END DO
  END DO
  DO CtrlZoneNum = 1,NumOfZones
    DO TimeStepIndex=1,NumOfTimeStepInDay
      IF (ALLOCATED(FinalZoneSizing(CtrlZoneNum)%HeatFlowSeq)) THEN
        FinalZoneSizing(CtrlZoneNum)%HeatFlowSeq(TimeStepIndex) = 0.0d0
        FinalZoneSizing(CtrlZoneNum)%HeatLoadSeq(TimeStepIndex) = 0.0d0
        FinalZoneSizing(CtrlZoneNum)%HeatZoneTempSeq(TimeStepIndex) = 0.0d0
        FinalZoneSizing(CtrlZoneNum)%HeatOutTempSeq(TimeStepIndex) = 0.0d0
        FinalZoneSizing(CtrlZoneNum)%HeatZoneRetTempSeq(TimeStepIndex) = 0.0d0
        FinalZoneSizing(CtrlZoneNum)%HeatTstatTempSeq(TimeStepIndex) = 0.0d0
        FinalZoneSizing(CtrlZoneNum)%HeatZoneHumRatSeq(TimeStepIndex) = 0.0d0
        FinalZoneSizing(CtrlZoneNum)%HeatOutHumRatSeq(TimeStepIndex) = 0.0d0
      END IF
      IF (ALLOCATED(CalcFinalZoneSizing(CtrlZoneNum)%HeatFlowSeq)) THEN
        CalcFinalZoneSizing(CtrlZoneNum)%HeatFlowSeq(TimeStepIndex) = 0.0d0
        CalcFinalZoneSizing(CtrlZoneNum)%HeatLoadSeq(TimeStepIndex) = 0.0d0
        CalcFinalZoneSizing(CtrlZoneNum)%HeatZoneTempSeq(TimeStepIndex) = 0.0d0
        CalcFinalZoneSizing(CtrlZoneNum)%HeatOutTempSeq(TimeStepIndex) = 0.0d0
        CalcFinalZoneSizing(CtrlZoneNum)%HeatZoneRetTempSeq(TimeStepIndex) = 0.0d0
        CalcFinalZoneSizing(CtrlZoneNum)%HeatTstatTempSeq(TimeStepIndex) = 0.0d0
        CalcFinalZoneSizing(CtrlZoneNum)%HeatZoneHumRatSeq(TimeStepIndex) = 0.0d0
        CalcFinalZoneSizing(CtrlZoneNum)%HeatOutHumRatSeq(TimeStepIndex) = 0.0d0
      END IF
      IF (ALLOCATED(FinalZoneSizing(CtrlZoneNum)%CoolFlowSeq)) THEN
        FinalZoneSizing(CtrlZoneNum)%CoolFlowSeq(TimeStepIndex) = 0.0d0
        FinalZoneSizing(CtrlZoneNum)%CoolLoadSeq(TimeStepIndex) = 0.0d0
        FinalZoneSizing(CtrlZoneNum)%CoolZoneTempSeq(TimeStepIndex) = 0.0d0
        FinalZoneSizing(CtrlZoneNum)%CoolOutTempSeq(TimeStepIndex) = 0.0d0
        FinalZoneSizing(CtrlZoneNum)%CoolZoneRetTempSeq(TimeStepIndex) = 0.0d0
        FinalZoneSizing(CtrlZoneNum)%CoolTstatTempSeq(TimeStepIndex) = 0.0d0
        FinalZoneSizing(CtrlZoneNum)%CoolZoneHumRatSeq(TimeStepIndex) = 0.0d0
        FinalZoneSizing(CtrlZoneNum)%CoolOutHumRatSeq(TimeStepIndex) = 0.0d0
      END IF
      IF (ALLOCATED(CalcFinalZoneSizing(CtrlZoneNum)%CoolFlowSeq)) THEN
        CalcFinalZoneSizing(CtrlZoneNum)%CoolFlowSeq(TimeStepIndex) = 0.0d0
        CalcFinalZoneSizing(CtrlZoneNum)%CoolLoadSeq(TimeStepIndex) = 0.0d0
        CalcFinalZoneSizing(CtrlZoneNum)%CoolZoneTempSeq(TimeStepIndex) = 0.0d0
        CalcFinalZoneSizing(CtrlZoneNum)%CoolOutTempSeq(TimeStepIndex) = 0.0d0
        CalcFinalZoneSizing(CtrlZoneNum)%CoolZoneRetTempSeq(TimeStepIndex) = 0.0d0
        CalcFinalZoneSizing(CtrlZoneNum)%CoolTstatTempSeq(TimeStepIndex) = 0.0d0
        CalcFinalZoneSizing(CtrlZoneNum)%CoolZoneHumRatSeq(TimeStepIndex) = 0.0d0
        CalcFinalZoneSizing(CtrlZoneNum)%CoolOutHumRatSeq(TimeStepIndex) = 0.0d0
      END IF
    END DO
    FinalZoneSizing(CtrlZoneNum)%CoolDesDay               = ' '     ! name of a cooling design day
    FinalZoneSizing(CtrlZoneNum)%HeatDesDay               = ' '     ! name of a heating design day

    FinalZoneSizing(CtrlZoneNum)%DesHeatMassFlow          = 0.0d0   ! zone design heating air mass flow rate [kg/s]
    FinalZoneSizing(CtrlZoneNum)%DesCoolMassFlow          = 0.0d0   ! zone design cooling air mass flow rate [kg/s]
    FinalZoneSizing(CtrlZoneNum)%DesHeatLoad              = 0.0d0   ! zone design heating load [W]
    FinalZoneSizing(CtrlZoneNum)%DesCoolLoad              = 0.0d0   ! zone design cooling load [W]
    FinalZoneSizing(CtrlZoneNum)%DesHeatDens              = 0.0d0   ! zone design heating air density [kg/m3]
    FinalZoneSizing(CtrlZoneNum)%DesCoolDens              = 0.0d0   ! zone design cooling air density [kg/m3]
    FinalZoneSizing(CtrlZoneNum)%DesHeatVolFlow           = 0.0d0   ! zone design heating air volume flow rate [m3/s]
    FinalZoneSizing(CtrlZoneNum)%DesCoolVolFlow           = 0.0d0   ! zone design cooling air volume flow rate [m3/s]
    FinalZoneSizing(CtrlZoneNum)%DesHeatVolFlowMax        = 0.0d0   ! zone design heating maximum air volume flow rate [m3/s]
    FinalZoneSizing(CtrlZoneNum)%DesCoolVolFlowMin        = 0.0d0   ! zone design cooling minimum air volume flow rate [m3/s]
    FinalZoneSizing(CtrlZoneNum)%DesHeatCoilInTemp        = 0.0d0   ! zone heating coil design air inlet temperature [C]
    FinalZoneSizing(CtrlZoneNum)%DesCoolCoilInTemp        = 0.0d0   ! zone cooling coil design air inlet temperature [C]
    FinalZoneSizing(CtrlZoneNum)%DesHeatCoilInHumRat      = 0.0d0   ! zone heating coil design air inlet humidity ratio [kg/kg]
    FinalZoneSizing(CtrlZoneNum)%DesCoolCoilInHumRat      = 0.0d0   ! zone cooling coil design air inlet humidity ratio [kg/kg]
    FinalZoneSizing(CtrlZoneNum)%DesHeatCoilInTempTU      = 0.0d0   ! zone heating coil design air inlet temperature (supply air)([C]
    FinalZoneSizing(CtrlZoneNum)%DesCoolCoilInTempTU      = 0.0d0   ! zone cooling coil design air inlet temperature (supply air)[C]
    FinalZoneSizing(CtrlZoneNum)%DesHeatCoilInHumRatTU    = 0.0d0   ! zone heating coil design air inlet humidity ratio
    FinalZoneSizing(CtrlZoneNum)%DesCoolCoilInHumRatTU    = 0.0d0   ! zone cooling coil design air inlet humidity ratio
    FinalZoneSizing(CtrlZoneNum)%HeatMassFlow             = 0.0d0   ! current zone heating air mass flow rate (HVAC time step)
    FinalZoneSizing(CtrlZoneNum)%CoolMassFlow             = 0.0d0   ! current zone cooling air mass flow rate (HVAC time step)
    FinalZoneSizing(CtrlZoneNum)%HeatLoad                 = 0.0d0   ! current zone heating load (HVAC time step)
    FinalZoneSizing(CtrlZoneNum)%CoolLoad                 = 0.0d0   ! current zone heating load (HVAC time step)
    FinalZoneSizing(CtrlZoneNum)%HeatZoneTemp             = 0.0d0   ! current zone temperature (heating, time step)
    FinalZoneSizing(CtrlZoneNum)%HeatOutTemp              = 0.0d0   ! current outdoor temperature (heating, time step)
    FinalZoneSizing(CtrlZoneNum)%HeatZoneRetTemp          = 0.0d0   ! current zone return temperature (heating, time step)
    FinalZoneSizing(CtrlZoneNum)%HeatTstatTemp            = 0.0d0   ! current zone thermostat temperature (heating, time step)
    FinalZoneSizing(CtrlZoneNum)%CoolZoneTemp             = 0.0d0   ! current zone temperature (cooling, time step)
    FinalZoneSizing(CtrlZoneNum)%CoolOutTemp              = 0.0d0   ! current Outdoor temperature (cooling, time step)
    FinalZoneSizing(CtrlZoneNum)%CoolZoneRetTemp          = 0.0d0   ! current zone return temperature (cooling, time step)
    FinalZoneSizing(CtrlZoneNum)%CoolTstatTemp            = 0.0d0   ! current zone thermostat temperature (cooling, time step)
    FinalZoneSizing(CtrlZoneNum)%HeatZoneHumRat           = 0.0d0   ! current zone humidity ratio (heating, time step)
    FinalZoneSizing(CtrlZoneNum)%CoolZoneHumRat           = 0.0d0   ! current zone humidity ratio (cooling, time step)
    FinalZoneSizing(CtrlZoneNum)%HeatOutHumRat            = 0.0d0   ! current outdoor humidity ratio (heating, time step)
    FinalZoneSizing(CtrlZoneNum)%CoolOutHumRat            = 0.0d0   ! current outdoor humidity ratio (cooling, time step)
    FinalZoneSizing(CtrlZoneNum)%ZoneTempAtHeatPeak       = 0.0d0   ! zone temp at max heating [C]
    FinalZoneSizing(CtrlZoneNum)%ZoneRetTempAtHeatPeak    = 0.0d0   ! zone return temp at max heating [C]
    FinalZoneSizing(CtrlZoneNum)%OutTempAtHeatPeak        = 0.0d0   ! outdoor temperature at max heating [C]
    FinalZoneSizing(CtrlZoneNum)%ZoneTempAtCoolPeak       = 0.0d0   ! zone temp at max cooling [C]
    FinalZoneSizing(CtrlZoneNum)%ZoneRetTempAtCoolPeak    = 0.0d0   ! zone return temp at max cooling [C]
    FinalZoneSizing(CtrlZoneNum)%OutTempAtCoolPeak        = 0.0d0   ! outdoor temperature at max cooling [C]
    FinalZoneSizing(CtrlZoneNum)%ZoneHumRatAtHeatPeak     = 0.0d0   ! zone humidity ratio at max heating [kg/kg]
    FinalZoneSizing(CtrlZoneNum)%ZoneHumRatAtCoolPeak     = 0.0d0   ! zone humidity ratio at max cooling [kg/kg]
    FinalZoneSizing(CtrlZoneNum)%OutHumRatAtHeatPeak      = 0.0d0   ! outdoor humidity at max heating [kg/kg]
    FinalZoneSizing(CtrlZoneNum)%OutHumRatAtCoolPeak      = 0.0d0   ! outdoor humidity at max cooling [kg/kg]
    FinalZoneSizing(CtrlZoneNum)%TimeStepNumAtHeatMax     = 0       ! time step number (in day) at Heating peak
    FinalZoneSizing(CtrlZoneNum)%TimeStepNumAtCoolMax     = 0       ! time step number (in day) at cooling peak
    FinalZoneSizing(CtrlZoneNum)%HeatDDNum                = 0       ! design day index of design day causing heating peak
    FinalZoneSizing(CtrlZoneNum)%CoolDDNum                = 0       ! design day index of design day causing heating peak
    FinalZoneSizing(CtrlZoneNum)%cHeatDDDate              = ' '     ! date of design day causing heating peak
    FinalZoneSizing(CtrlZoneNum)%cCoolDDDate              = ' '     ! date of design day causing cooling peak

    CalcFinalZoneSizing(CtrlZoneNum)%CoolDesDay               = ' '     ! name of a cooling design day
    CalcFinalZoneSizing(CtrlZoneNum)%HeatDesDay               = ' '     ! name of a heating design day

    CalcFinalZoneSizing(CtrlZoneNum)%DesHeatMassFlow          = 0.0d0   ! zone design heating air mass flow rate [kg/s]
    CalcFinalZoneSizing(CtrlZoneNum)%DesCoolMassFlow          = 0.0d0   ! zone design cooling air mass flow rate [kg/s]
    CalcFinalZoneSizing(CtrlZoneNum)%DesHeatLoad              = 0.0d0   ! zone design heating load [W]
    CalcFinalZoneSizing(CtrlZoneNum)%DesCoolLoad              = 0.0d0   ! zone design cooling load [W]
    CalcFinalZoneSizing(CtrlZoneNum)%DesHeatDens              = 0.0d0   ! zone design heating air density [kg/m3]
    CalcFinalZoneSizing(CtrlZoneNum)%DesCoolDens              = 0.0d0   ! zone design cooling air density [kg/m3]
    CalcFinalZoneSizing(CtrlZoneNum)%DesHeatVolFlow           = 0.0d0   ! zone design heating air volume flow rate [m3/s]
    CalcFinalZoneSizing(CtrlZoneNum)%DesCoolVolFlow           = 0.0d0   ! zone design cooling air volume flow rate [m3/s]
    CalcFinalZoneSizing(CtrlZoneNum)%DesHeatVolFlowMax        = 0.0d0   ! zone design heating maximum air volume flow rate [m3/s]
    CalcFinalZoneSizing(CtrlZoneNum)%DesCoolVolFlowMin        = 0.0d0   ! zone design cooling minimum air volume flow rate [m3/s]
    CalcFinalZoneSizing(CtrlZoneNum)%DesHeatCoilInTemp        = 0.0d0   ! zone heating coil design air inlet temperature [C]
    CalcFinalZoneSizing(CtrlZoneNum)%DesCoolCoilInTemp        = 0.0d0   ! zone cooling coil design air inlet temperature [C]
    CalcFinalZoneSizing(CtrlZoneNum)%DesHeatCoilInHumRat      = 0.0d0   ! zone heating coil design air inlet humidity ratio [kg/kg]
    CalcFinalZoneSizing(CtrlZoneNum)%DesCoolCoilInHumRat      = 0.0d0   ! zone cooling coil design air inlet humidity ratio [kg/kg]
    CalcFinalZoneSizing(CtrlZoneNum)%DesHeatCoilInTempTU      = 0.0d0   ! zone heating coil design air inlet temperature (supply air)([C]
    CalcFinalZoneSizing(CtrlZoneNum)%DesCoolCoilInTempTU      = 0.0d0   ! zone cooling coil design air inlet temperature (supply air)[C]
    CalcFinalZoneSizing(CtrlZoneNum)%DesHeatCoilInHumRatTU    = 0.0d0   ! zone heating coil design air inlet humidity ratio
    CalcFinalZoneSizing(CtrlZoneNum)%DesCoolCoilInHumRatTU    = 0.0d0   ! zone cooling coil design air inlet humidity ratio
    CalcFinalZoneSizing(CtrlZoneNum)%HeatMassFlow             = 0.0d0   ! current zone heating air mass flow rate (HVAC time step)
    CalcFinalZoneSizing(CtrlZoneNum)%CoolMassFlow             = 0.0d0   ! current zone cooling air mass flow rate (HVAC time step)
    CalcFinalZoneSizing(CtrlZoneNum)%HeatLoad                 = 0.0d0   ! current zone heating load (HVAC time step)
    CalcFinalZoneSizing(CtrlZoneNum)%CoolLoad                 = 0.0d0   ! current zone heating load (HVAC time step)
    CalcFinalZoneSizing(CtrlZoneNum)%HeatZoneTemp             = 0.0d0   ! current zone temperature (heating, time step)
    CalcFinalZoneSizing(CtrlZoneNum)%HeatOutTemp              = 0.0d0   ! current outdoor temperature (heating, time step)
    CalcFinalZoneSizing(CtrlZoneNum)%HeatZoneRetTemp          = 0.0d0   ! current zone return temperature (heating, time step)
    CalcFinalZoneSizing(CtrlZoneNum)%HeatTstatTemp            = 0.0d0   ! current zone thermostat temperature (heating, time step)
    CalcFinalZoneSizing(CtrlZoneNum)%CoolZoneTemp             = 0.0d0   ! current zone temperature (cooling, time step)
    CalcFinalZoneSizing(CtrlZoneNum)%CoolOutTemp              = 0.0d0   ! current Outdoor temperature (cooling, time step)
    CalcFinalZoneSizing(CtrlZoneNum)%CoolZoneRetTemp          = 0.0d0   ! current zone return temperature (cooling, time step)
    CalcFinalZoneSizing(CtrlZoneNum)%CoolTstatTemp            = 0.0d0   ! current zone thermostat temperature (cooling, time step)
    CalcFinalZoneSizing(CtrlZoneNum)%HeatZoneHumRat           = 0.0d0   ! current zone humidity ratio (heating, time step)
    CalcFinalZoneSizing(CtrlZoneNum)%CoolZoneHumRat           = 0.0d0   ! current zone humidity ratio (cooling, time step)
    CalcFinalZoneSizing(CtrlZoneNum)%HeatOutHumRat            = 0.0d0   ! current outdoor humidity ratio (heating, time step)
    CalcFinalZoneSizing(CtrlZoneNum)%CoolOutHumRat            = 0.0d0   ! current outdoor humidity ratio (cooling, time step)
    CalcFinalZoneSizing(CtrlZoneNum)%ZoneTempAtHeatPeak       = 0.0d0   ! zone temp at max heating [C]
    CalcFinalZoneSizing(CtrlZoneNum)%ZoneRetTempAtHeatPeak    = 0.0d0   ! zone return temp at max heating [C]
    CalcFinalZoneSizing(CtrlZoneNum)%OutTempAtHeatPeak        = 0.0d0   ! outdoor temperature at max heating [C]
    CalcFinalZoneSizing(CtrlZoneNum)%ZoneTempAtCoolPeak       = 0.0d0   ! zone temp at max cooling [C]
    CalcFinalZoneSizing(CtrlZoneNum)%ZoneRetTempAtCoolPeak    = 0.0d0   ! zone return temp at max cooling [C]
    CalcFinalZoneSizing(CtrlZoneNum)%OutTempAtCoolPeak        = 0.0d0   ! outdoor temperature at max cooling [C]
    CalcFinalZoneSizing(CtrlZoneNum)%ZoneHumRatAtHeatPeak     = 0.0d0   ! zone humidity ratio at max heating [kg/kg]
    CalcFinalZoneSizing(CtrlZoneNum)%ZoneHumRatAtCoolPeak     = 0.0d0   ! zone humidity ratio at max cooling [kg/kg]
    CalcFinalZoneSizing(CtrlZoneNum)%OutHumRatAtHeatPeak      = 0.0d0   ! outdoor humidity at max heating [kg/kg]
    CalcFinalZoneSizing(CtrlZoneNum)%OutHumRatAtCoolPeak      = 0.0d0   ! outdoor humidity at max cooling [kg/kg]
    CalcFinalZoneSizing(CtrlZoneNum)%TimeStepNumAtHeatMax     = 0       ! time step number (in day) at Heating peak
    CalcFinalZoneSizing(CtrlZoneNum)%TimeStepNumAtCoolMax     = 0       ! time step number (in day) at cooling peak
    CalcFinalZoneSizing(CtrlZoneNum)%HeatDDNum                = 0       ! design day index of design day causing heating peak
    CalcFinalZoneSizing(CtrlZoneNum)%CoolDDNum                = 0       ! design day index of design day causing heating peak
    CalcFinalZoneSizing(CtrlZoneNum)%cHeatDDDate              = ' '     ! date of design day causing heating peak
    CalcFinalZoneSizing(CtrlZoneNum)%cCoolDDDate              = ' '     ! date of design day causing cooling peak
  END DO
END SUBROUTINE RezeroZoneSizingArrays

SUBROUTINE UpdateZoneSizing(CallIndicator)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   December 2000
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Update the result variables of the zone sizing calculation

          ! METHODOLOGY EMPLOYED:
          ! CallIndicator = 1 (BeginDay) zero the result arrays
          ! CallIndicator = 2 (DuringDay) fill arrays, averaging over 1 zone time step
          ! CallIndicator = 3 (EndDay) calculate daily maxima
          ! CallIndicator = 4 (EndZoneSizingCalc) write out results

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataGlobals, ONLY: HourOfDay, TimeStep, TimeStepZone, NumOfTimeStepInHour, &
                         BeginDay, DuringDay, EndDay, EndZoneSizingCalc, MinutesPerTimeStep, &
                         OutputFileZoneSizing, OutputFileDebug, emsCallFromZoneSizing, AnyEnergyManagementSystemInModel, &
                         isPulseZoneSizing, OutputFileZonePulse
  USE DataInterfaces, ONLY: SetupEMSActuator
  USE DataHVACGlobals, ONLY: FracTimeStepZone, SmallMassFlow, SmallTempDiff
  USE DataEnvironment, ONLY : StdBaroPress, StdRhoAir
  USE General, ONLY: MovingAvg, RoundSigDigits
  USE DataHeatBalFanSys, ONLY: ZoneThermostatSetPointHi, ZoneThermostatSetPointLo,TempZoneThermostatSetPoint
  USE EMSManager, ONLY: ManageEMS

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER :: CallIndicator

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER :: fmta="(A)"
  CHARACTER(len=*), PARAMETER :: ZSizeFmt10="('Time')"
  CHARACTER(len=*), PARAMETER :: ZSizeFmt11="(A1,A,':',A,A,A1,A,':',A,A,A1,A,':',A,A,A1,A,':',A,A )"
  CHARACTER(len=*), PARAMETER :: ZSizeFmt20="(I2.2,':',I2.2,':00')"
  CHARACTER(len=*), PARAMETER :: ZSizeFmt21="(A1,ES12.6,A1,ES12.6,A1,ES12.6,A1,ES12.6 )"
  CHARACTER(len=*), PARAMETER :: ZSizeFmt30="('Peak')"
  CHARACTER(len=*), PARAMETER :: ZSizeFmt31="(A1,ES12.6,A1,ES12.6,A1,ES12.6,A1,ES12.6)"
  CHARACTER(len=*), PARAMETER :: ZSizeFmt40="(/'Peak Vol Flow (m3/s)')"
  CHARACTER(len=*), PARAMETER :: ZSizeFmt41="(A1,A1,A1,ES12.6,A1,ES12.6)"

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: DesDayNum   ! design day index
  INTEGER :: TimeStepIndex      ! zone time step index
  INTEGER :: CtrlZoneNum ! controlled zone index
  INTEGER :: TimeStepInDay ! zone time step in day
  INTEGER :: I                  ! write statement index
!  REAL(r64)    :: HourFrac           ! fractional hour
  INTEGER :: HourCounter        ! Hour Counter
  INTEGER :: TimeStepCounter    ! Time Step Counter
  INTEGER :: Minutes            ! Current Minutes Counter
  INTEGER :: HourPrint          ! Hour to print (timestamp)
  REAL(r64)    :: OAFrac             ! outside air fraction
  INTEGER :: TimeStepAtPeak     ! time step number at heat or cool peak
  INTEGER :: TimeStepAtPeakF    ! time step number at heat or cool peak (final)
  INTEGER :: DDNum              ! Design Day index
  INTEGER :: DDNumF             ! Design Day index (final)
  REAL(r64)    :: TotCoolSizMult     ! combines user cooling design flow input with zone sizing multiplier
  REAL(r64)    :: TotHeatSizMult     ! combines user heating design flow input with zone sizing multiplier
  REAL(r64)    :: MinOAMass          ! zone minimum outside air mass flow rate kg/s
  REAL(r64)    :: MaxOfMinCoolVolFlow  ! max of the user specified design cooling minimum flows and min OA flow [m3/s]
  REAL(r64)    :: MaxOfMinCoolMassFlow ! max of the user specified design cooling minimum flows and min OA flow [kg/s]
  REAL(r64)    :: MaxHeatVolFlow     ! max of user specified design heating max flow [m3/s]
  CHARACTER(len=8) :: HrMinString   ! store hour/minute string before assigning to peak string array
  REAL(r64)    :: SupplyTemp        ! supply air temperature [C]
  REAL(r64)    :: DeltaTemp         ! supply air delta temperature [deltaC]

  SELECT CASE (CallIndicator)

    CASE (BeginDay)

      DO CtrlZoneNum = 1,NumOfZones

        IF (.not. ZoneEquipConfig(CtrlZoneNum)%IsControlled) CYCLE

        CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%CoolDesDay = EnvironmentName
        CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%HeatDesDay = EnvironmentName
        CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%DesHeatDens = StdRhoAir
        CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%DesCoolDens = StdRhoAir
        CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%HeatDDNum = CurOverallSimDay
        CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%CoolDDNum = CurOverallSimDay

      END DO

    CASE (DuringDay)

      TimeStepInDay = (HourOfDay-1)*NumOfTimeStepInHour + TimeStep

      DO CtrlZoneNum = 1,NumOfZones
        IF (.not. ZoneEquipConfig(CtrlZoneNum)%IsControlled) CYCLE
        IF (ZoneThermostatSetPointHi(CtrlZoneNum) > 0.0d0 .AND.   &
               ZoneThermostatSetPointHi(CtrlZoneNum) > ZoneSizThermSetPtHi(CtrlZoneNum)) THEN
          ZoneSizThermSetPtHi(CtrlZoneNum) = ZoneThermostatSetPointHi(CtrlZoneNum)
        END IF
        IF (ZoneThermostatSetPointLo(CtrlZoneNum) > 0.0d0 .AND.   &
               ZoneThermostatSetPointLo(CtrlZoneNum) < ZoneSizThermSetPtLo(CtrlZoneNum)) THEN
          ZoneSizThermSetPtLo(CtrlZoneNum) = ZoneThermostatSetPointLo(CtrlZoneNum)
        END IF
        ZoneSizing(CtrlZoneNum,CurOverallSimDay)%DesHeatSetPtSeq(TimeStepInDay) = ZoneThermostatSetPointLo(CtrlZoneNum)
        ZoneSizing(CtrlZoneNum,CurOverallSimDay)%HeatTstatTempSeq(TimeStepInDay) = TempZoneThermostatSetPoint(CtrlZoneNum)
        ZoneSizing(CtrlZoneNum,CurOverallSimDay)%DesCoolSetPtSeq(TimeStepInDay) = ZoneThermostatSetPointHi(CtrlZoneNum)
        ZoneSizing(CtrlZoneNum,CurOverallSimDay)%CoolTstatTempSeq(TimeStepInDay) = TempZoneThermostatSetPoint(CtrlZoneNum)
        CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%HeatFlowSeq(TimeStepInDay) = &
          CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%HeatFlowSeq(TimeStepInDay) + &
          CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%HeatMassFlow * FracTimeStepZone
        CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%HeatLoadSeq(TimeStepInDay) = &
          CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%HeatLoadSeq(TimeStepInDay) + &
          CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%HeatLoad * FracTimeStepZone
        CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%HeatZoneTempSeq(TimeStepInDay) = &
          CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%HeatZoneTempSeq(TimeStepInDay) + &
          CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%HeatZoneTemp * FracTimeStepZone
        CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%HeatOutTempSeq(TimeStepInDay) = &
          CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%HeatOutTempSeq(TimeStepInDay) + &
          CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%HeatOutTemp * FracTimeStepZone
        CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%HeatZoneRetTempSeq(TimeStepInDay) = &
          CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%HeatZoneRetTempSeq(TimeStepInDay) + &
          CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%HeatZoneRetTemp * FracTimeStepZone
        CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%HeatTstatTempSeq(TimeStepInDay) = &
          TempZoneThermostatSetPoint(CtrlZoneNum)
        CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%HeatZoneHumRatSeq(TimeStepInDay) = &
          CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%HeatZoneHumRatSeq(TimeStepInDay) + &
          CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%HeatZoneHumRat * FracTimeStepZone
        CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%HeatOutHumRatSeq(TimeStepInDay) = &
          CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%HeatOutHumRatSeq(TimeStepInDay) + &
          CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%HeatOutHumRat * FracTimeStepZone
        CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%CoolFlowSeq(TimeStepInDay) = &
          CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%CoolFlowSeq(TimeStepInDay) + &
          CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%CoolMassFlow * FracTimeStepZone
        CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%CoolLoadSeq(TimeStepInDay) = &
          CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%CoolLoadSeq(TimeStepInDay) + &
          CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%CoolLoad * FracTimeStepZone
        CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%CoolZoneTempSeq(TimeStepInDay) = &
          CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%CoolZoneTempSeq(TimeStepInDay) + &
          CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%CoolZoneTemp * FracTimeStepZone
        CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%CoolOutTempSeq(TimeStepInDay) = &
          CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%CoolOutTempSeq(TimeStepInDay) + &
          CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%CoolOutTemp * FracTimeStepZone
        CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%CoolZoneRetTempSeq(TimeStepInDay) = &
          CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%CoolZoneRetTempSeq(TimeStepInDay) + &
          CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%CoolZoneRetTemp * FracTimeStepZone
        CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%CoolTstatTempSeq(TimeStepInDay) = &
          TempZoneThermostatSetPoint(CtrlZoneNum)
        CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%CoolZoneHumRatSeq(TimeStepInDay) = &
          CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%CoolZoneHumRatSeq(TimeStepInDay) + &
          CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%CoolZoneHumRat * FracTimeStepZone
        CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%CoolOutHumRatSeq(TimeStepInDay) = &
          CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%CoolOutHumRatSeq(TimeStepInDay) + &
          CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%CoolOutHumRat * FracTimeStepZone
      END DO

    CASE (EndDay)

      DO CtrlZoneNum = 1,NumOfZones
        IF (.not. ZoneEquipConfig(CtrlZoneNum)%IsControlled) CYCLE
        AvgData = 0.0d0
        CALL MovingAvg(CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%CoolFlowSeq,NumOfTimeStepInDay,NumTimeStepsInAvg,AvgData)
        CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%CoolFlowSeq = AvgData
      END DO
      DO CtrlZoneNum = 1,NumOfZones
        IF (.not. ZoneEquipConfig(CtrlZoneNum)%IsControlled) CYCLE
        AvgData = 0.0d0
        CALL MovingAvg(CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%CoolLoadSeq,NumOfTimeStepInDay,NumTimeStepsInAvg,AvgData)
        CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%CoolLoadSeq = AvgData
      END DO
      DO CtrlZoneNum = 1,NumOfZones
        IF (.not. ZoneEquipConfig(CtrlZoneNum)%IsControlled) CYCLE
        AvgData = 0.0d0
        CALL MovingAvg(CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%HeatFlowSeq,NumOfTimeStepInDay,NumTimeStepsInAvg,AvgData)
        CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%HeatFlowSeq = AvgData
      END DO
      DO CtrlZoneNum = 1,NumOfZones
        IF (.not. ZoneEquipConfig(CtrlZoneNum)%IsControlled) CYCLE
        AvgData = 0.0d0
        CALL MovingAvg(CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%HeatLoadSeq,NumOfTimeStepInDay,NumTimeStepsInAvg,AvgData)
        CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%HeatLoadSeq = AvgData
      END DO
      DO CtrlZoneNum = 1,NumOfZones
        IF (.not. ZoneEquipConfig(CtrlZoneNum)%IsControlled) CYCLE
        AvgData = 0.0d0
        CALL MovingAvg(CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%CoolZoneRetTempSeq,NumOfTimeStepInDay,NumTimeStepsInAvg,AvgData)
        CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%CoolZoneRetTempSeq = AvgData
      END DO
      DO CtrlZoneNum = 1,NumOfZones
        IF (.not. ZoneEquipConfig(CtrlZoneNum)%IsControlled) CYCLE
        AvgData = 0.0d0
        CALL MovingAvg(CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%HeatZoneRetTempSeq,NumOfTimeStepInDay,NumTimeStepsInAvg,AvgData)
        CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%HeatZoneRetTempSeq = AvgData
      END DO

      DO CtrlZoneNum = 1,NumOfZones

        IF (.not. ZoneEquipConfig(CtrlZoneNum)%IsControlled) CYCLE

        DO TimeStepIndex = 1,NumOfTimeStepInDay
          IF ( CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%HeatLoadSeq(TimeStepIndex) > &
              CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%DesHeatLoad ) THEN
            CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%DesHeatLoad = &
              CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%HeatLoadSeq(TimeStepIndex)
            CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%DesHeatMassFlow = &
              CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%HeatFlowSeq(TimeStepIndex)
            CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%ZoneTempAtHeatPeak = &
              CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%HeatZoneTempSeq(TimeStepIndex)
            CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%OutTempAtHeatPeak = &
              CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%HeatOutTempSeq(TimeStepIndex)
            CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%ZoneRetTempAtHeatPeak = &
              CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%HeatZoneRetTempSeq(TimeStepIndex)
            CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%HeatTstatTemp = &
              CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%HeatTstatTempSeq(TimeStepIndex)
            CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%ZoneHumRatAtHeatPeak = &
              CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%HeatZoneHumRatSeq(TimeStepIndex)
            CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%OutHumRatAtHeatPeak = &
              CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%HeatOutHumRatSeq(TimeStepIndex)
            CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%TimeStepNumAtHeatMax = TimeStepIndex
          END IF
        END DO
        IF (CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%DesHeatMassFlow > 0.0d0) THEN
          CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%DesHeatVolFlow = &
              CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%DesHeatMassFlow &
              / CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%DesHeatDens
          OAFrac = CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%MinOA &
                     / MAX(CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%DesHeatVolFlow,SmallMassFlow)
          OAFrac = MIN(1.0d0,MAX(0.0d0,OAFrac))
          TimeStepAtPeak = CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%TimeStepNumAtHeatMax
          CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%DesHeatCoilInTemp = &
              OAFrac*DesDayWeath(CurOverallSimDay)%Temp(TimeStepAtPeak) + &
              (1.d0-OAFrac)*CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%ZoneTempAtHeatPeak
          CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%DesHeatCoilInHumRat = &
              OAFrac*DesDayWeath(CurOverallSimDay)%HumRat(TimeStepAtPeak) + &
              (1.d0-OAFrac)*CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%ZoneHumRatAtHeatPeak
        END IF
        DO  TimeStepIndex = 1,NumOfTimeStepInDay
          IF ( CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%CoolLoadSeq(TimeStepIndex) > &
              CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%DesCoolLoad ) THEN
            CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%DesCoolLoad = &
              CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%CoolLoadSeq(TimeStepIndex)
            CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%DesCoolMassFlow = &
              CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%CoolFlowSeq(TimeStepIndex)
            CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%ZoneTempAtCoolPeak = &
              CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%CoolZoneTempSeq(TimeStepIndex)
            CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%OutTempAtCoolPeak = &
              CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%CoolOutTempSeq(TimeStepIndex)
            CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%ZoneRetTempAtCoolPeak = &
              CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%CoolZoneRetTempSeq(TimeStepIndex)
            CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%CoolTstatTemp = &
              CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%CoolTstatTempSeq(TimeStepIndex)
            CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%ZoneHumRatAtCoolPeak = &
              CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%CoolZoneHumRatSeq(TimeStepIndex)
            CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%OutHumRatAtCoolPeak = &
              CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%CoolOutHumRatSeq(TimeStepIndex)
            CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%TimeStepNumAtCoolMax = TimeStepIndex
          END IF
        END DO
        IF (CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%DesCoolMassFlow > 0.0d0) THEN
          CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%DesCoolVolFlow = &
              CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%DesCoolMassFlow &
              / CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%DesCoolDens
          OAFrac = CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%MinOA &
                     / MAX(CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%DesCoolVolFlow,SmallMassFlow)
          OAFrac = MIN(1.0d0,MAX(0.0d0,OAFrac))
          TimeStepAtPeak = CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%TimeStepNumAtCoolMax
          CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%DesCoolCoilInTemp = &
              OAFrac*DesDayWeath(CurOverallSimDay)%Temp(TimeStepAtPeak) + &
              (1.d0-OAFrac)*CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%ZoneTempAtCoolPeak
          CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%DesCoolCoilInHumRat = &
              OAFrac*DesDayWeath(CurOverallSimDay)%HumRat(TimeStepAtPeak) + &
              (1.d0-OAFrac)*CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%ZoneHumRatAtCoolPeak
        END IF
        IF (CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%DesHeatVolFlow > CalcFinalZoneSizing(CtrlZoneNum)%DesHeatVolFlow) THEN
          CalcFinalZoneSizing(CtrlZoneNum)%DesHeatVolFlow = CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%DesHeatVolFlow
          CalcFinalZoneSizing(CtrlZoneNum)%DesHeatLoad = CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%DesHeatLoad
          CalcFinalZoneSizing(CtrlZoneNum)%DesHeatMassFlow = CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%DesHeatMassFlow
          CalcFinalZoneSizing(CtrlZoneNum)%HeatDesDay = CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%HeatDesDay
          CalcFinalZoneSizing(CtrlZoneNum)%DesHeatDens = CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%DesHeatDens
          CalcFinalZoneSizing(CtrlZoneNum)%HeatFlowSeq = CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%HeatFlowSeq
          CalcFinalZoneSizing(CtrlZoneNum)%HeatLoadSeq = CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%HeatLoadSeq
          CalcFinalZoneSizing(CtrlZoneNum)%HeatZoneTempSeq = CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%HeatZoneTempSeq
          CalcFinalZoneSizing(CtrlZoneNum)%HeatOutTempSeq = CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%HeatOutTempSeq
          CalcFinalZoneSizing(CtrlZoneNum)%HeatZoneRetTempSeq = CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%HeatZoneRetTempSeq
          CalcFinalZoneSizing(CtrlZoneNum)%HeatTstatTempSeq = CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%HeatTstatTempSeq
          CalcFinalZoneSizing(CtrlZoneNum)%HeatTstatTemp = CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%HeatTstatTemp
          CalcFinalZoneSizing(CtrlZoneNum)%HeatZoneHumRatSeq = CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%HeatZoneHumRatSeq
          CalcFinalZoneSizing(CtrlZoneNum)%HeatOutHumRatSeq = CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%HeatOutHumRatSeq
          CalcFinalZoneSizing(CtrlZoneNum)%ZoneTempAtHeatPeak = CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%ZoneTempAtHeatPeak
          CalcFinalZoneSizing(CtrlZoneNum)%OutTempAtHeatPeak = CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%OutTempAtHeatPeak
          CalcFinalZoneSizing(CtrlZoneNum)%ZoneRetTempAtHeatPeak = &
              CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%ZoneRetTempAtHeatPeak
          CalcFinalZoneSizing(CtrlZoneNum)%ZoneHumRatAtHeatPeak = &
              CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%ZoneHumRatAtHeatPeak
          CalcFinalZoneSizing(CtrlZoneNum)%OutHumRatAtHeatPeak = &
              CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%OutHumRatAtHeatPeak
          CalcFinalZoneSizing(CtrlZoneNum)%HeatDDNum = CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%HeatDDNum
          CalcFinalZoneSizing(CtrlZoneNum)%cHeatDDDate = DesDayWeath(CurOverallSimDay)%DateString
          CalcFinalZoneSizing(CtrlZoneNum)%TimeStepNumAtHeatMax = &
              CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%TimeStepNumAtHeatMax
          CalcFinalZoneSizing(CtrlZoneNum)%DesHeatCoilInTemp = CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%DesHeatCoilInTemp
          CalcFinalZoneSizing(CtrlZoneNum)%DesHeatCoilInHumRat = &
              CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%DesHeatCoilInHumRat
        ELSE
          CalcFinalZoneSizing(CtrlZoneNum)%DesHeatDens = StdRhoAir
        END IF
        IF (CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%DesCoolVolFlow > CalcFinalZoneSizing(CtrlZoneNum)%DesCoolVolFlow) THEN
          CalcFinalZoneSizing(CtrlZoneNum)%DesCoolVolFlow = CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%DesCoolVolFlow
          CalcFinalZoneSizing(CtrlZoneNum)%DesCoolLoad = CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%DesCoolLoad
          CalcFinalZoneSizing(CtrlZoneNum)%DesCoolMassFlow = CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%DesCoolMassFlow
          CalcFinalZoneSizing(CtrlZoneNum)%CoolDesDay = CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%CoolDesDay
          CalcFinalZoneSizing(CtrlZoneNum)%DesCoolDens = CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%DesCoolDens
          CalcFinalZoneSizing(CtrlZoneNum)%CoolFlowSeq = CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%CoolFlowSeq
          CalcFinalZoneSizing(CtrlZoneNum)%CoolLoadSeq = CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%CoolLoadSeq
          CalcFinalZoneSizing(CtrlZoneNum)%CoolZoneTempSeq = CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%CoolZoneTempSeq
          CalcFinalZoneSizing(CtrlZoneNum)%CoolOutTempSeq = CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%CoolOutTempSeq
          CalcFinalZoneSizing(CtrlZoneNum)%CoolZoneRetTempSeq = CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%CoolZoneRetTempSeq
          CalcFinalZoneSizing(CtrlZoneNum)%CoolTstatTempSeq = CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%CoolTstatTempSeq
          CalcFinalZoneSizing(CtrlZoneNum)%CoolTstatTemp = CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%CoolTstatTemp
          CalcFinalZoneSizing(CtrlZoneNum)%CoolZoneHumRatSeq = CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%CoolZoneHumRatSeq
          CalcFinalZoneSizing(CtrlZoneNum)%CoolOutHumRatSeq = CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%CoolOutHumRatSeq
          CalcFinalZoneSizing(CtrlZoneNum)%ZoneTempAtCoolPeak = CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%ZoneTempAtCoolPeak
          CalcFinalZoneSizing(CtrlZoneNum)%OutTempAtCoolPeak = CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%OutTempAtCoolPeak
          CalcFinalZoneSizing(CtrlZoneNum)%ZoneRetTempAtCoolPeak = &
              CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%ZoneRetTempAtCoolPeak
          CalcFinalZoneSizing(CtrlZoneNum)%ZoneHumRatAtCoolPeak = &
              CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%ZoneHumRatAtCoolPeak
          CalcFinalZoneSizing(CtrlZoneNum)%OutHumRatAtCoolPeak = &
              CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%OutHumRatAtCoolPeak
          CalcFinalZoneSizing(CtrlZoneNum)%CoolDDNum = CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%CoolDDNum
          CalcFinalZoneSizing(CtrlZoneNum)%cCoolDDDate = DesDayWeath(CurOverallSimDay)%DateString
          CalcFinalZoneSizing(CtrlZoneNum)%TimeStepNumAtCoolMax = &
              CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%TimeStepNumAtCoolMax
          CalcFinalZoneSizing(CtrlZoneNum)%DesCoolCoilInTemp = CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%DesCoolCoilInTemp
          CalcFinalZoneSizing(CtrlZoneNum)%DesCoolCoilInHumRat = &
              CalcZoneSizing(CtrlZoneNum,CurOverallSimDay)%DesCoolCoilInHumRat
        ELSE
          CalcFinalZoneSizing(CtrlZoneNum)%DesCoolDens = StdRhoAir
        END IF

      END DO

    CASE (EndZoneSizingCalc)

      ! candidate EMS calling point to customize CalcFinalZoneSizing
      Call ManageEMS(emsCallFromZoneSizing)

      ! now apply EMS overrides (if any)

      IF (AnyEnergyManagementSystemInModel) THEN
        DO CtrlZoneNum = 1,NumOfZones
          IF (CalcFinalZoneSizing(CtrlZoneNum)%EMSOverrideDesHeatMassOn ) THEN
            IF (CalcFinalZoneSizing(CtrlZoneNum)%DesHeatMassFlow > 0.0D0) &
              CalcFinalZoneSizing(CtrlZoneNum)%DesHeatMassFlow = CalcFinalZoneSizing(CtrlZoneNum)%EMSValueDesHeatMassFlow
          ENDIF
          IF (CalcFinalZoneSizing(CtrlZoneNum)%EMSOverrideDesCoolMassOn) THEN
            IF (CalcFinalZoneSizing(CtrlZoneNum)%DesCoolMassFlow > 0.0D0) &
              CalcFinalZoneSizing(CtrlZoneNum)%DesCoolMassFlow = CalcFinalZoneSizing(CtrlZoneNum)%EMSValueDesCoolMassFlow
          ENDIF
          IF (CalcFinalZoneSizing(CtrlZoneNum)%EMSOverrideDesHeatLoadOn) THEN
            IF (CalcFinalZoneSizing(CtrlZoneNum)%DesHeatLoad > 0.0D0) &
              CalcFinalZoneSizing(CtrlZoneNum)%DesHeatLoad = CalcFinalZoneSizing(CtrlZoneNum)%EMSValueDesHeatLoad
          ENDIF
          IF (CalcFinalZoneSizing(CtrlZoneNum)%EMSOverrideDesCoolLoadOn) THEN
            IF (CalcFinalZoneSizing(CtrlZoneNum)%DesCoolLoad > 0.0D0) &
              CalcFinalZoneSizing(CtrlZoneNum)%DesCoolLoad = CalcFinalZoneSizing(CtrlZoneNum)%EMSValueDesCoolLoad
          ENDIF
          IF (CalcFinalZoneSizing(CtrlZoneNum)%EMSOverrideDesHeatVolOn) THEN
            IF (CalcFinalZoneSizing(CtrlZoneNum)%DesHeatVolFlow > 0.0D0) &
              CalcFinalZoneSizing(CtrlZoneNum)%DesHeatVolFlow = CalcFinalZoneSizing(CtrlZoneNum)%EMSValueDesHeatVolFlow
          ENDIF
          IF (CalcFinalZoneSizing(CtrlZoneNum)%EMSOverrideDesCoolVolOn) THEN
            IF (CalcFinalZoneSizing(CtrlZoneNum)%DesCoolVolFlow > 0.0D0) &
              CalcFinalZoneSizing(CtrlZoneNum)%DesCoolVolFlow = CalcFinalZoneSizing(CtrlZoneNum)%EMSValueDesCoolVolFlow
          ENDIF
        ENDDO
      ENDIF


      IF (.NOT. isPulseZoneSizing) THEN

        DO CtrlZoneNum=1,NumOfZones
          IF (.not. ZoneEquipConfig(CtrlZoneNum)%IsControlled) CYCLE
          IF (ABS(CalcFinalZoneSizing(CtrlZoneNum)%DesCoolLoad) <= 1.d-8) THEN
            CALL ShowWarningError('Calculated design cooling load for zone='//  &
                              TRIM(CalcFinalZoneSizing(CtrlZoneNum)%ZoneName)//' is zero.')
            CALL ShowContinueError('Check Sizing:Zone and ZoneControl:Thermostat inputs.')
          ENDIF
          IF (ABS(CalcFinalZoneSizing(CtrlZoneNum)%DesHeatLoad) <= 1.d-8) THEN
            CALL ShowWarningError('Calculated design heating load for zone='//  &
                              TRIM(CalcFinalZoneSizing(CtrlZoneNum)%ZoneName)//' is zero.')
            CALL ShowContinueError('Check Sizing:Zone and ZoneControl:Thermostat inputs.')
          ENDIF
        ENDDO

        WRITE(OutputFileZoneSizing,ZSizeFmt10,ADVANCE='No')
        DO I=1,NumOfZones
          IF (.not. ZoneEquipConfig(I)%IsControlled) CYCLE
           WRITE(OutputFileZoneSizing,ZSizeFmt11,ADVANCE='No')   &
                      SizingFileColSep,TRIM(CalcFinalZoneSizing(I)%ZoneName),   &
                              TRIM(CalcFinalZoneSizing(I)%HeatDesDay),':Des Heat Load [W]',  &
                      SizingFileColSep,TRIM(CalcFinalZoneSizing(I)%ZoneName),   &
                              TRIM(CalcFinalZoneSizing(I)%CoolDesDay),':Des Sens Cool Load [W]',  &
                      SizingFileColSep,TRIM(CalcFinalZoneSizing(I)%ZoneName),   &
                              TRIM(CalcFinalZoneSizing(I)%HeatDesDay),':Des Heat Mass Flow [kg/s]',  &
                      SizingFileColSep,TRIM(CalcFinalZoneSizing(I)%ZoneName),   &
                              TRIM(CalcFinalZoneSizing(I)%CoolDesDay),':Des Cool Mass Flow [kg/s]'

! Should this be done only if there is a cooling load? Or would this message help dermine why there was no load?
        IF (ABS(CalcFinalZoneSizing(I)%DesCoolLoad) > 1.d-8) THEN
          ! check for low cooling delta T from supply to zone to see if air volume flow rate might be excessively high
          IF (CalcFinalZoneSizing(I)%ZnCoolDgnSAMethod == SupplyAirTemperature) THEN
            SupplyTemp = CalcFinalZoneSizing(I)%CoolDesTemp
            DeltaTemp = SupplyTemp - CalcFinalZoneSizing(I)%ZoneTempatCoolPeak
          ELSE
            DeltaTemp = -ABS(CalcFinalZoneSizing(I)%CoolDesTempDiff)
            SupplyTemp = DeltaTemp + CalcFinalZoneSizing(I)%ZoneTempatCoolPeak
          END IF

          ! check for low delta T to avoid very high flow rates
          IF(ABS(DeltaTemp) .LT. 5.0d0 .AND. ABS(DeltaTemp) > SmallTempDiff)THEN ! Vdot exceeds 1200 cfm/ton @ DT=5
            IF(ABS(DeltaTemp) .GE. 2.0d0)THEN ! Vdot exceeds 3000 cfm/ton @ DT=2
              CALL ShowWarningError('UpdateZoneSizing: Cooling supply air temperature (calculated) within 5C of'// &
                                    ' zone temperature')
            ELSE
              CALL ShowSevereError('UpdateZoneSizing: Cooling supply air temperature (calculated) within 2C of'// &
                                   ' zone temperature')
            END IF
            CALL ShowContinueError('...check zone thermostat set point and design supply air temperatures')
            CALL ShowContinueError('...zone name = '//TRIM(CalcFinalZoneSizing(I)%ZoneName))
            CALL ShowContinueError('...design sensible cooling load = '// &
                 TRIM(RoundSigDigits(CalcFinalZoneSizing(I)%DesCoolLoad,2))//' W')
            CALL ShowContinueError('...thermostat set point temp    = '// &
                 TRIM(RoundSigDigits(CalcFinalZoneSizing(I)%CoolTstatTemp,3))//' C')
            CALL ShowContinueError('...zone temperature             = '// &
                 TRIM(RoundSigDigits(CalcFinalZoneSizing(I)%ZoneTempatCoolPeak,3))//' C')
            CALL ShowContinueError('...supply air temperature       = '//TRIM(RoundSigDigits(SupplyTemp,3))//' C')
            CALL ShowContinueError('...temperature difference       = '//TRIM(RoundSigDigits(DeltaTemp,5))//' C')
            CALL ShowContinueError('...calculated volume flow rate  = '// &
                 TRIM(RoundSigDigits((CalcFinalZoneSizing(I)%DesCoolVolFlow),5))//' m3/s')
            CALL ShowContinueError('...calculated mass flow rate    = '// &
                 TRIM(RoundSigDigits((CalcFinalZoneSizing(I)%DesCoolMassFlow),5))//' kg/s')
            IF(SupplyTemp .GT. CalcFinalZoneSizing(I)%ZoneTempatCoolPeak) &
              CALL ShowContinueError('...Note: supply air temperature should be less than zone'// &
                                     ' temperature during cooling air flow calculations')
          ELSE IF(ABS(DeltaTemp) > SmallTempDiff .AND. SupplyTemp .GT. CalcFinalZoneSizing(I)%ZoneTempatCoolPeak)THEN
            CALL ShowSevereError('UpdateZoneSizing: Supply air temperature is greater than zone'// &
                                 ' temperature during cooling air flow calculations')
            CALL ShowContinueError('...zone temperature            = '// &
                 TRIM(RoundSigDigits(CalcFinalZoneSizing(I)%ZoneTempatCoolPeak,3))//' C')
            CALL ShowContinueError('...supply air temperature      = '//TRIM(RoundSigDigits(SupplyTemp,3))//' C')
            CALL ShowContinueError('...occurs in zone              = '//TRIM(CalcFinalZoneSizing(I)%ZoneName))
          END IF
        END IF
! Should this be done only if there is a heating load? Or would this message help dermine why there was no load?
        IF (ABS(CalcFinalZoneSizing(I)%DesHeatLoad) > 1.d-8) THEN ! ABS() ?
          ! check for low cooling delta T from supply to zone to see if air volume flow rate might be excessively high
          IF (CalcFinalZoneSizing(I)%ZnHeatDgnSAMethod == SupplyAirTemperature) THEN
            SupplyTemp = CalcFinalZoneSizing(I)%HeatDesTemp
            DeltaTemp = SupplyTemp - CalcFinalZoneSizing(I)%ZoneTempatHeatPeak
          ELSE
            DeltaTemp = CalcFinalZoneSizing(I)%HeatDesTempDiff
            SupplyTemp = DeltaTemp + CalcFinalZoneSizing(I)%ZoneTempatHeatPeak
          END IF

          IF(ABS(DeltaTemp) .LT. 5.0d0 .AND. ABS(DeltaTemp) > SmallTempDiff)THEN ! Vdot exceeds 1200 cfm/ton @ DT=5
            IF(ABS(DeltaTemp) .GE. 2.0d0)THEN ! Vdot exceeds 3000 cfm/ton @ DT=2
              CALL ShowWarningError('UpdateZoneSizing: Heating supply air temperature (calculated) within 5C of'// &
                                    ' zone temperature')
            ELSE
              CALL ShowSevereError('UpdateZoneSizing: Heating supply air temperature (calculated) within 2C of'// &
                                   ' zone temperature')
            END IF
            CALL ShowContinueError('...check zone thermostat set point and design supply air temperatures')
            CALL ShowContinueError('...zone name = '//TRIM(CalcFinalZoneSizing(I)%ZoneName))
            CALL ShowContinueError('...design heating load         = '// &
                 TRIM(RoundSigDigits(CalcFinalZoneSizing(I)%DesCoolLoad,2))//' W')
            CALL ShowContinueError('...thermostat set piont temp   = '// &
                 TRIM(RoundSigDigits(CalcFinalZoneSizing(I)%HeatTstatTemp,3))//' C')
            CALL ShowContinueError('...zone temperature            = '// &
                 TRIM(RoundSigDigits(CalcFinalZoneSizing(I)%ZoneTempatHeatPeak,3))//' C')
            CALL ShowContinueError('...supply air temperature      = '//TRIM(RoundSigDigits(SupplyTemp,3))//' C')
            CALL ShowContinueError('...temperature difference      = '//TRIM(RoundSigDigits(DeltaTemp,5))//' C')
            CALL ShowContinueError('...calculated volume flow rate = '// &
                 TRIM(RoundSigDigits((CalcFinalZoneSizing(I)%DesHeatVolFlow),5))//' m3/s')
            CALL ShowContinueError('...calculated mass flow rate   = '// &
                 TRIM(RoundSigDigits((CalcFinalZoneSizing(I)%DesHeatMassFlow),5))//' kg/s')
            IF(SupplyTemp .LT. CalcFinalZoneSizing(I)%ZoneTempatHeatPeak) &
              CALL ShowContinueError('...Note: supply air temperature should be greater than zone'// &
                                     ' temperature during heating air flow calculations')
          ELSE IF(ABS(DeltaTemp) > SmallTempDiff .AND. SupplyTemp .LT. CalcFinalZoneSizing(I)%ZoneTempatHeatPeak)THEN
            CALL ShowSevereError('UpdateZoneSizing: Supply air temperature is less than zone'// &
                                 ' temperature during heating air flow calculations')
            CALL ShowContinueError('...zone temperature            = '// &
                 TRIM(RoundSigDigits(CalcFinalZoneSizing(I)%ZoneTempatHeatPeak,3))//' C')
            CALL ShowContinueError('...supply air temperature      = '//TRIM(RoundSigDigits(SupplyTemp,3))//' C')
            CALL ShowContinueError('...occurs in zone              = '// &
                 TRIM(CalcFinalZoneSizing(I)%ZoneName))
          END IF
        END IF

        ENDDO

        WRITE(OutputFileZoneSizing,fmta) ' '
  !      HourFrac = 0.0
        Minutes=0
        TimeStepIndex=0
        DO HourCounter=1,24
          DO TimeStepCounter=1,NumOfTimeStepInHour
            TimeStepIndex=TimeStepIndex+1
            Minutes=Minutes+MinutesPerTimeStep
            IF (Minutes == 60) THEN
              Minutes=0
              HourPrint=HourCounter
            ELSE
              HourPrint=HourCounter-1
            ENDIF
            DO CtrlZoneNum = 1,NumOfZones
              IF (.not. ZoneEquipConfig(CtrlZoneNum)%IsControlled) CYCLE
              IF (TimeStepIndex == CalcFinalZoneSizing(CtrlZoneNum)%TimeStepNumAtHeatMax) THEN
                WRITE(HrMinString,PeakHrMinFmt) HourPrint, Minutes
                HeatPeakDateHrMin(CtrlZoneNum)=TRIM(CalcFinalZoneSizing(CtrlZoneNum)%cHeatDDDate)//' '//TRIM(HrMinString)
              END IF
              IF (TimeStepIndex == CalcFinalZoneSizing(CtrlZoneNum)%TimeStepNumAtCoolMax) THEN
                WRITE(HrMinString,PeakHrMinFmt) HourPrint, Minutes
                CoolPeakDateHrMin(CtrlZoneNum)=TRIM(CalcFinalZoneSizing(CtrlZoneNum)%cCoolDDDate)//' '//TRIM(HrMinString)
              END IF
            END DO
            WRITE(OutputFileZoneSizing,ZSizeFmt20,ADVANCE='No') HourPrint,Minutes
            DO I=1,NumOfZones
              IF (.not. ZoneEquipConfig(I)%IsControlled) CYCLE
              WRITE(OutputFileZoneSizing,ZSizeFmt21,ADVANCE='No')   &
                 SizingFileColSep,CalcFinalZoneSizing(I)%HeatLoadSeq(TimeStepIndex),  &
                 SizingFileColSep,CalcFinalZoneSizing(I)%CoolLoadSeq(TimeStepIndex),  &
                 SizingFileColSep,CalcFinalZoneSizing(I)%HeatFlowSeq(TimeStepIndex),  &
                 SizingFileColSep,CalcFinalZoneSizing(I)%CoolFlowSeq(TimeStepIndex)
            END DO
            WRITE(OutputFileZoneSizing,fmta) ' '
          END DO
        END DO
        WRITE(OutputFileZoneSizing,ZSizeFmt30,ADVANCE='No')
        DO I=1,NumOfZones
          IF (.not. ZoneEquipConfig(I)%IsControlled) CYCLE
          WRITE(OutputFileZoneSizing,ZSizeFmt31,ADVANCE='No') SizingFileColSep,CalcFinalZoneSizing(I)%DesHeatLoad,     &
                                        SizingFileColSep,CalcFinalZoneSizing(I)%DesCoolLoad,     &
                                        SizingFileColSep,CalcFinalZoneSizing(I)%DesHeatMassFlow, &
                                        SizingFileColSep,CalcFinalZoneSizing(I)%DesCoolMassFlow
        END DO
        WRITE(OutputFileZoneSizing,fmta) ' '
        WRITE(OutputFileZoneSizing,ZSizeFmt40,ADVANCE='No')
        DO I=1,NumOfZones
          IF (.not. ZoneEquipConfig(I)%IsControlled) CYCLE
          WRITE(OutputFileZoneSizing,ZSizeFmt41,ADVANCE='No') SizingFileColSep,SizingFileColSep,     &
                                        SizingFileColSep,CalcFinalZoneSizing(I)%DesHeatVolFlow,  &
                                        SizingFileColSep,CalcFinalZoneSizing(I)%DesCoolVolFlow
        END DO
        WRITE(OutputFileZoneSizing,fmta) ' '
        CLOSE (OutputFileZoneSizing)
      END IF

      ! Move data from Calc arrays to user modified arrays

      ZoneSizing%CoolDesDay = CalcZoneSizing%CoolDesDay
      ZoneSizing%HeatDesDay = CalcZoneSizing%HeatDesDay
      ZoneSizing%DesHeatDens = CalcZoneSizing%DesHeatDens
      ZoneSizing%DesCoolDens = CalcZoneSizing%DesCoolDens
      ZoneSizing%HeatDDNum = CalcZoneSizing%HeatDDNum
      ZoneSizing%CoolDDNum = CalcZoneSizing%CoolDDNum

      ZoneSizing%DesHeatLoad = CalcZoneSizing%DesHeatLoad
      ZoneSizing%DesHeatMassFlow = CalcZoneSizing%DesHeatMassFlow
      ZoneSizing%ZoneTempAtHeatPeak = CalcZoneSizing%ZoneTempAtHeatPeak
      ZoneSizing%OutTempAtHeatPeak = CalcZoneSizing%OutTempAtHeatPeak
      ZoneSizing%ZoneRetTempAtHeatPeak = CalcZoneSizing%ZoneRetTempAtHeatPeak
      ZoneSizing%ZoneHumRatAtHeatPeak = CalcZoneSizing%ZoneHumRatAtHeatPeak
      ZoneSizing%OutHumRatAtHeatPeak = CalcZoneSizing%OutHumRatAtHeatPeak
      ZoneSizing%TimeStepNumAtHeatMax = CalcZoneSizing%TimeStepNumAtHeatMax
      ZoneSizing%DesHeatVolFlow = CalcZoneSizing%DesHeatVolFlow
      ZoneSizing%DesHeatCoilInTemp = CalcZoneSizing%DesHeatCoilInTemp
      ZoneSizing%DesHeatCoilInHumRat = CalcZoneSizing%DesHeatCoilInHumRat

      ZoneSizing%DesCoolLoad = CalcZoneSizing%DesCoolLoad
      ZoneSizing%DesCoolMassFlow = CalcZoneSizing%DesCoolMassFlow
      ZoneSizing%ZoneTempAtCoolPeak = CalcZoneSizing%ZoneTempAtCoolPeak
      ZoneSizing%OutTempAtCoolPeak = CalcZoneSizing%OutTempAtCoolPeak
      ZoneSizing%ZoneRetTempAtCoolPeak = CalcZoneSizing%ZoneRetTempAtCoolPeak
      ZoneSizing%ZoneHumRatAtCoolPeak = CalcZoneSizing%ZoneHumRatAtCoolPeak
      ZoneSizing%OutHumRatAtCoolPeak = CalcZoneSizing%OutHumRatAtCoolPeak
      ZoneSizing%TimeStepNumAtCoolMax = CalcZoneSizing%TimeStepNumAtCoolMax
      ZoneSizing%DesCoolVolFlow = CalcZoneSizing%DesCoolVolFlow
      ZoneSizing%DesCoolCoilInTemp = CalcZoneSizing%DesCoolCoilInTemp
      ZoneSizing%DesCoolCoilInHumRat = CalcZoneSizing%DesCoolCoilInHumRat

      FinalZoneSizing%CoolDesDay = CalcFinalZoneSizing%CoolDesDay
      FinalZoneSizing%HeatDesDay = CalcFinalZoneSizing%HeatDesDay
      FinalZoneSizing%DesHeatDens = CalcFinalZoneSizing%DesHeatDens
      FinalZoneSizing%DesCoolDens = CalcFinalZoneSizing%DesCoolDens
      FinalZoneSizing%HeatDDNum = CalcFinalZoneSizing%HeatDDNum
      FinalZoneSizing%CoolDDNum = CalcFinalZoneSizing%CoolDDNum

      FinalZoneSizing%DesHeatLoad = CalcFinalZoneSizing%DesHeatLoad
      FinalZoneSizing%DesHeatMassFlow = CalcFinalZoneSizing%DesHeatMassFlow
      FinalZoneSizing%ZoneTempAtHeatPeak = CalcFinalZoneSizing%ZoneTempAtHeatPeak
      FinalZoneSizing%OutTempAtHeatPeak = CalcFinalZoneSizing%OutTempAtHeatPeak
      FinalZoneSizing%ZoneRetTempAtHeatPeak = CalcFinalZoneSizing%ZoneRetTempAtHeatPeak
      FinalZoneSizing%ZoneHumRatAtHeatPeak = CalcFinalZoneSizing%ZoneHumRatAtHeatPeak
      FinalZoneSizing%OutHumRatAtHeatPeak = CalcFinalZoneSizing%OutHumRatAtHeatPeak
      FinalZoneSizing%TimeStepNumAtHeatMax = CalcFinalZoneSizing%TimeStepNumAtHeatMax
      FinalZoneSizing%DesHeatVolFlow = CalcFinalZoneSizing%DesHeatVolFlow
      FinalZoneSizing%DesHeatCoilInTemp = CalcFinalZoneSizing%DesHeatCoilInTemp
      FinalZoneSizing%DesHeatCoilInHumRat = CalcFinalZoneSizing%DesHeatCoilInHumRat

      FinalZoneSizing%DesCoolLoad = CalcFinalZoneSizing%DesCoolLoad
      FinalZoneSizing%DesCoolMassFlow = CalcFinalZoneSizing%DesCoolMassFlow
      FinalZoneSizing%ZoneTempAtCoolPeak = CalcFinalZoneSizing%ZoneTempAtCoolPeak
      FinalZoneSizing%OutTempAtCoolPeak = CalcFinalZoneSizing%OutTempAtCoolPeak
      FinalZoneSizing%ZoneRetTempAtCoolPeak = CalcFinalZoneSizing%ZoneRetTempAtCoolPeak
      FinalZoneSizing%ZoneHumRatAtCoolPeak = CalcFinalZoneSizing%ZoneHumRatAtCoolPeak
      FinalZoneSizing%OutHumRatAtCoolPeak = CalcFinalZoneSizing%OutHumRatAtCoolPeak
      FinalZoneSizing%TimeStepNumAtCoolMax = CalcFinalZoneSizing%TimeStepNumAtCoolMax
      FinalZoneSizing%DesCoolVolFlow = CalcFinalZoneSizing%DesCoolVolFlow
      FinalZoneSizing%DesCoolCoilInTemp = CalcFinalZoneSizing%DesCoolCoilInTemp
      FinalZoneSizing%DesCoolCoilInHumRat = CalcFinalZoneSizing%DesCoolCoilInHumRat

      DO DesDayNum = 1,TotDesDays+TotRunDesPersDays
        DO CtrlZoneNum = 1,NumOfZones
          IF (.not. ZoneEquipConfig(CtrlZoneNum)%IsControlled) CYCLE
          DO TimeStepIndex = 1,NumOfTimeStepInDay
            ZoneSizing(CtrlZoneNum,DesDayNum)%HeatFlowSeq(TimeStepIndex) = &
              CalcZoneSizing(CtrlZoneNum,DesDayNum)%HeatFlowSeq(TimeStepIndex)
            ZoneSizing(CtrlZoneNum,DesDayNum)%HeatLoadSeq(TimeStepIndex) = &
              CalcZoneSizing(CtrlZoneNum,DesDayNum)%HeatLoadSeq(TimeStepIndex)
            ZoneSizing(CtrlZoneNum,DesDayNum)%CoolFlowSeq(TimeStepIndex) = &
              CalcZoneSizing(CtrlZoneNum,DesDayNum)%CoolFlowSeq(TimeStepIndex)
            ZoneSizing(CtrlZoneNum,DesDayNum)%CoolLoadSeq(TimeStepIndex) = &
              CalcZoneSizing(CtrlZoneNum,DesDayNum)%CoolLoadSeq(TimeStepIndex)
            ZoneSizing(CtrlZoneNum,DesDayNum)%HeatZoneTempSeq(TimeStepIndex) = &
              CalcZoneSizing(CtrlZoneNum,DesDayNum)%HeatZoneTempSeq(TimeStepIndex)
            ZoneSizing(CtrlZoneNum,DesDayNum)%HeatOutTempSeq(TimeStepIndex) = &
              CalcZoneSizing(CtrlZoneNum,DesDayNum)%HeatOutTempSeq(TimeStepIndex)
            ZoneSizing(CtrlZoneNum,DesDayNum)%HeatZoneRetTempSeq(TimeStepIndex) = &
              CalcZoneSizing(CtrlZoneNum,DesDayNum)%HeatZoneRetTempSeq(TimeStepIndex)
            ZoneSizing(CtrlZoneNum,DesDayNum)%HeatTstatTempSeq(TimeStepIndex) = &
              CalcZoneSizing(CtrlZoneNum,DesDayNum)%HeatTstatTempSeq(TimeStepIndex)
            ZoneSizing(CtrlZoneNum,DesDayNum)%HeatZoneHumRatSeq(TimeStepIndex) = &
              CalcZoneSizing(CtrlZoneNum,DesDayNum)%HeatZoneHumRatSeq(TimeStepIndex)
            ZoneSizing(CtrlZoneNum,DesDayNum)%HeatOutHumRatSeq(TimeStepIndex) = &
              CalcZoneSizing(CtrlZoneNum,DesDayNum)%HeatOutHumRatSeq(TimeStepIndex)
            ZoneSizing(CtrlZoneNum,DesDayNum)%CoolZoneTempSeq(TimeStepIndex) = &
              CalcZoneSizing(CtrlZoneNum,DesDayNum)%CoolZoneTempSeq(TimeStepIndex)
            ZoneSizing(CtrlZoneNum,DesDayNum)%CoolOutTempSeq(TimeStepIndex) = &
              CalcZoneSizing(CtrlZoneNum,DesDayNum)%CoolOutTempSeq(TimeStepIndex)
            ZoneSizing(CtrlZoneNum,DesDayNum)%CoolZoneRetTempSeq(TimeStepIndex) = &
              CalcZoneSizing(CtrlZoneNum,DesDayNum)%CoolZoneRetTempSeq(TimeStepIndex)
            ZoneSizing(CtrlZoneNum,DesDayNum)%CoolTstatTempSeq(TimeStepIndex) = &
              CalcZoneSizing(CtrlZoneNum,DesDayNum)%CoolTstatTempSeq(TimeStepIndex)
            ZoneSizing(CtrlZoneNum,DesDayNum)%CoolZoneHumRatSeq(TimeStepIndex) = &
              CalcZoneSizing(CtrlZoneNum,DesDayNum)%CoolZoneHumRatSeq(TimeStepIndex)
            ZoneSizing(CtrlZoneNum,DesDayNum)%CoolOutHumRatSeq(TimeStepIndex) = &
              CalcZoneSizing(CtrlZoneNum,DesDayNum)%CoolOutHumRatSeq(TimeStepIndex)
          END DO
        END DO
      END DO

      DO CtrlZoneNum = 1,NumOfZones
        IF (.not. ZoneEquipConfig(CtrlZoneNum)%IsControlled) CYCLE
        DO TimeStepIndex = 1,NumOfTimeStepInDay
          FinalZoneSizing(CtrlZoneNum)%HeatFlowSeq(TimeStepIndex) = &
            CalcFinalZoneSizing(CtrlZoneNum)%HeatFlowSeq(TimeStepIndex)
          FinalZoneSizing(CtrlZoneNum)%HeatLoadSeq(TimeStepIndex) = &
            CalcFinalZoneSizing(CtrlZoneNum)%HeatLoadSeq(TimeStepIndex)
          FinalZoneSizing(CtrlZoneNum)%CoolFlowSeq(TimeStepIndex) = &
            CalcFinalZoneSizing(CtrlZoneNum)%CoolFlowSeq(TimeStepIndex)
          FinalZoneSizing(CtrlZoneNum)%CoolLoadSeq(TimeStepIndex) = &
            CalcFinalZoneSizing(CtrlZoneNum)%CoolLoadSeq(TimeStepIndex)
          FinalZoneSizing(CtrlZoneNum)%HeatZoneTempSeq(TimeStepIndex) = &
            CalcFinalZoneSizing(CtrlZoneNum)%HeatZoneTempSeq(TimeStepIndex)
          FinalZoneSizing(CtrlZoneNum)%HeatOutTempSeq(TimeStepIndex) = &
            CalcFinalZoneSizing(CtrlZoneNum)%HeatOutTempSeq(TimeStepIndex)
          FinalZoneSizing(CtrlZoneNum)%HeatZoneRetTempSeq(TimeStepIndex) = &
            CalcFinalZoneSizing(CtrlZoneNum)%HeatZoneRetTempSeq(TimeStepIndex)
          FinalZoneSizing(CtrlZoneNum)%HeatTstatTempSeq(TimeStepIndex) = &
            CalcFinalZoneSizing(CtrlZoneNum)%HeatTstatTempSeq(TimeStepIndex)
          FinalZoneSizing(CtrlZoneNum)%HeatZoneHumRatSeq(TimeStepIndex) = &
            CalcFinalZoneSizing(CtrlZoneNum)%HeatZoneHumRatSeq(TimeStepIndex)
          FinalZoneSizing(CtrlZoneNum)%HeatOutHumRatSeq(TimeStepIndex) = &
            CalcFinalZoneSizing(CtrlZoneNum)%HeatOutHumRatSeq(TimeStepIndex)
          FinalZoneSizing(CtrlZoneNum)%CoolZoneTempSeq(TimeStepIndex) = &
            CalcFinalZoneSizing(CtrlZoneNum)%CoolZoneTempSeq(TimeStepIndex)
          FinalZoneSizing(CtrlZoneNum)%CoolOutTempSeq(TimeStepIndex) = &
            CalcFinalZoneSizing(CtrlZoneNum)%CoolOutTempSeq(TimeStepIndex)
          FinalZoneSizing(CtrlZoneNum)%CoolZoneRetTempSeq(TimeStepIndex) = &
            CalcFinalZoneSizing(CtrlZoneNum)%CoolZoneRetTempSeq(TimeStepIndex)
          FinalZoneSizing(CtrlZoneNum)%CoolTstatTempSeq(TimeStepIndex) = &
            CalcFinalZoneSizing(CtrlZoneNum)%CoolTstatTempSeq(TimeStepIndex)
          FinalZoneSizing(CtrlZoneNum)%CoolZoneHumRatSeq(TimeStepIndex) = &
            CalcFinalZoneSizing(CtrlZoneNum)%CoolZoneHumRatSeq(TimeStepIndex)
          FinalZoneSizing(CtrlZoneNum)%CoolOutHumRatSeq(TimeStepIndex) = &
            CalcFinalZoneSizing(CtrlZoneNum)%CoolOutHumRatSeq(TimeStepIndex)
        END DO
      END DO
      DO  CtrlZoneNum = 1,NumOfZones
        IF (.not. ZoneEquipConfig(CtrlZoneNum)%IsControlled) CYCLE
        ! Now take into account the user specified sizing factor and user specified cooling design air flow
        ! rate
        TotCoolSizMult = 0.0d0
        ! Calculate a sizing factor from the user specified cooling design air flow rate
        IF (FinalZoneSizing(CtrlZoneNum)%InpDesCoolAirFlow > 0.0d0 .AND. &
            FinalZoneSizing(CtrlZoneNum)%CoolAirDesMethod == InpDesAirFlow .AND. &
            FinalZoneSizing(CtrlZoneNum)%DesCoolVolFlow > 0.0d0) THEN
          TotCoolSizMult = (FinalZoneSizing(CtrlZoneNum)%InpDesCoolAirFlow / &
            FinalZoneSizing(CtrlZoneNum)%DesCoolVolFlow) * FinalZoneSizing(CtrlZoneNum)%CoolSizingFactor
        ! If no user specified cooling design air flow rate input, use the user specified szing factor
        ELSE
          TotCoolSizMult = FinalZoneSizing(CtrlZoneNum)%CoolSizingFactor
        END IF
        ! If the cooling sizing multiplier is not 1, adjust the cooling design data
        IF (ABS(TotCoolSizMult-1.0d0) > .00001d0) THEN
          IF (FinalZoneSizing(CtrlZoneNum)%DesCoolVolFlow > 0.0d0) THEN
            TimeStepAtPeak = FinalZoneSizing(CtrlZoneNum)%TimeStepNumAtCoolMax
            DDNum = FinalZoneSizing(CtrlZoneNum)%CoolDDNum
            FinalZoneSizing(CtrlZoneNum)%DesCoolVolFlow = CalcFinalZoneSizing(CtrlZoneNum)%DesCoolVolFlow * TotCoolSizMult
            FinalZoneSizing(CtrlZoneNum)%DesCoolMassFlow = CalcFinalZoneSizing(CtrlZoneNum)%DesCoolMassFlow &
                                                             * TotCoolSizMult
            FinalZoneSizing(CtrlZoneNum)%DesCoolLoad = CalcFinalZoneSizing(CtrlZoneNum)%DesCoolLoad * TotCoolSizMult
            FinalZoneSizing(CtrlZoneNum)%CoolFlowSeq = CalcFinalZoneSizing(CtrlZoneNum)%CoolFlowSeq * TotCoolSizMult
            FinalZoneSizing(CtrlZoneNum)%CoolLoadSeq = CalcFinalZoneSizing(CtrlZoneNum)%CoolLoadSeq * TotCoolSizMult
            OAFrac = FinalZoneSizing(CtrlZoneNum)%MinOA &
                       / FinalZoneSizing(CtrlZoneNum)%DesCoolVolFlow
            OAFrac = MIN(1.0d0,MAX(0.0d0,OAFrac))
            FinalZoneSizing(CtrlZoneNum)%DesCoolCoilInTemp = OAFrac*DesDayWeath(DDNum)%Temp(TimeStepAtPeak) + &
                                                      (1.0d0-OAFrac)*FinalZoneSizing(CtrlZoneNum)%ZoneTempAtCoolPeak
            FinalZoneSizing(CtrlZoneNum)%DesCoolCoilInHumRat = &
                                                 OAFrac*DesDayWeath(DDNum)%HumRat(TimeStepAtPeak) + &
                                                 (1.0d0-OAFrac)*FinalZoneSizing(CtrlZoneNum)%ZoneHumRatAtCoolPeak
          ELSE
            FinalZoneSizing(CtrlZoneNum)%DesCoolVolFlow = FinalZoneSizing(CtrlZoneNum)%InpDesCoolAirFlow
            FinalZoneSizing(CtrlZoneNum)%DesCoolMassFlow = FinalZoneSizing(CtrlZoneNum)%DesCoolVolFlow * &
              FinalZoneSizing(CtrlZoneNum)%DesCoolDens
          END IF
          DO DDNum = 1,TotDesDays+TotRunDesPersDays
            IF (ZoneSizing(CtrlZoneNum,DDNum)%DesCoolVolFlow > 0.0d0) THEN
              TimeStepAtPeak = ZoneSizing(CtrlZoneNum,DDNum)%TimeStepNumAtCoolMax
              ZoneSizing(CtrlZoneNum,DDNum)%DesCoolVolFlow = CalcZoneSizing(CtrlZoneNum,DDNum)%DesCoolVolFlow * &
                                                               TotCoolSizMult
              ZoneSizing(CtrlZoneNum,DDNum)%DesCoolMassFlow = CalcZoneSizing(CtrlZoneNum,DDNum)%DesCoolMassFlow &
                                                               * TotCoolSizMult
              ZoneSizing(CtrlZoneNum,DDNum)%DesCoolLoad = CalcZoneSizing(CtrlZoneNum,DDNum)%DesCoolLoad * TotCoolSizMult
              ZoneSizing(CtrlZoneNum,DDNum)%CoolFlowSeq = CalcZoneSizing(CtrlZoneNum,DDNum)%CoolFlowSeq * TotCoolSizMult
              ZoneSizing(CtrlZoneNum,DDNum)%CoolLoadSeq = CalcZoneSizing(CtrlZoneNum,DDNum)%CoolLoadSeq * TotCoolSizMult
              OAFrac = ZoneSizing(CtrlZoneNum,DDNum)%MinOA &
                         / ZoneSizing(CtrlZoneNum,DDNum)%DesCoolVolFlow
              OAFrac = MIN(1.0d0,MAX(0.0d0,OAFrac))
              ZoneSizing(CtrlZoneNum,DDNum)%DesCoolCoilInTemp = OAFrac*DesDayWeath(DDNum)%Temp(TimeStepAtPeak) + &
                                                        (1.0d0-OAFrac)*ZoneSizing(CtrlZoneNum,DDNum)%ZoneTempAtCoolPeak
              ZoneSizing(CtrlZoneNum,DDNum)%DesCoolCoilInHumRat = &
                                                   OAFrac*DesDayWeath(DDNum)%HumRat(TimeStepAtPeak) + &
                                                   (1.0d0-OAFrac)*ZoneSizing(CtrlZoneNum,DDNum)%ZoneHumRatAtCoolPeak
            ELSE
              ZoneSizing(CtrlZoneNum,DDNum)%DesCoolVolFlow = ZoneSizing(CtrlZoneNum,DDNum)%InpDesCoolAirFlow
              ZoneSizing(CtrlZoneNum,DDNum)%DesCoolMassFlow = ZoneSizing(CtrlZoneNum,DDNum)%DesCoolVolFlow * &
                ZoneSizing(CtrlZoneNum,DDNum)%DesCoolDens
            END IF
          END DO
        END IF
        ! Now make sure that the design cooling air flow rates are greater than or equal to the specified minimums
        IF (FinalZoneSizing(CtrlZoneNum)%CoolAirDesMethod == DesAirFlowWithLim) THEN
          MaxOfMinCoolVolFlow = MAX(FinalZoneSizing(CtrlZoneNum)%DesCoolMinAirFlow,   &
                                 FinalZoneSizing(CtrlZoneNum)%DesCoolMinAirFlow2, &
                                 FinalZoneSizing(CtrlZoneNum)%MinOA)
        ELSE
          MaxOfMinCoolVolFlow = FinalZoneSizing(CtrlZoneNum)%MinOA
        END IF
        MaxOfMinCoolMassFlow = MaxOfMinCoolVolFlow * FinalZoneSizing(CtrlZoneNum)%DesCoolDens
        IF (MaxOfMinCoolVolFlow > FinalZoneSizing(CtrlZoneNum)%DesCoolVolFlow) THEN
          FinalZoneSizing(CtrlZoneNum)%DesCoolVolFlow = MaxOfMinCoolVolFlow
          FinalZoneSizing(CtrlZoneNum)%DesCoolMassFlow = MaxOfMinCoolMassFlow
        END IF
        DO TimeStepIndex = 1,NumOfTimeStepInDay
          IF (MaxOfMinCoolMassFlow > FinalZoneSizing(CtrlZoneNum)%CoolFlowSeq(TimeStepIndex)) THEN
            FinalZoneSizing(CtrlZoneNum)%CoolFlowSeq(TimeStepIndex) = MaxOfMinCoolMassFlow
          END IF
        END DO
        DO DDNum = 1,TotDesDays+TotRunDesPersDays
          MaxOfMinCoolVolFlow = MAX(ZoneSizing(CtrlZoneNum,DDNum)%DesCoolMinAirFlow,   &
                                 ZoneSizing(CtrlZoneNum,DDNum)%DesCoolMinAirFlow, &
                                 ZoneSizing(CtrlZoneNum,DDNum)%MinOA)
          MaxOfMinCoolMassFlow =   MaxOfMinCoolVolFlow * ZoneSizing(CtrlZoneNum,DDNum)%DesCoolDens
          IF (MaxOfMinCoolVolFlow > ZoneSizing(CtrlZoneNum,DDNum)%DesCoolVolFlow) THEN
            ZoneSizing(CtrlZoneNum,DDNum)%DesCoolVolFlow = MaxOfMinCoolVolFlow
            ZoneSizing(CtrlZoneNum,DDNum)%DesCoolMassFlow = MaxOfMinCoolMassFlow
          END IF
          DO TimeStepIndex = 1,NumOfTimeStepInDay
            IF (MaxOfMinCoolMassFlow > ZoneSizing(CtrlZoneNum,DDNum)%CoolFlowSeq(TimeStepIndex)) THEN
              ZoneSizing(CtrlZoneNum,DDNum)%CoolFlowSeq(TimeStepIndex) = MaxOfMinCoolMassFlow
            END IF
          END DO
        END DO
! IF cooling flow rate is 0, this data may be used to size a HP so initialize DDNum, TimeStepatPeak, and sizing data (end of IF)
        ! check for flow rate having been set (by MinOA or other min) but no timestep at max
!        IF (FinalZoneSizing(CtrlZoneNum)%DesCoolMassFlow > 0.0d0 .AND. &
         IF ((FinalZoneSizing(CtrlZoneNum)%TimeStepNumAtCoolMax == 0 .OR. &
              FinalZoneSizing(CtrlZoneNum)%CoolDDNum == 0) ) THEN
          DO DDNum = 1,TotDesDays+TotRunDesPersDays
            ZoneSizing(CtrlZoneNum,DDNum)%TimeStepNumAtCoolMax = 1
            TimeStepAtPeak = ZoneSizing(CtrlZoneNum,DDNum)%TimeStepNumAtCoolMax
            DO TimeStepIndex = 1,NumOfTimeStepInDay
             IF (DesDayWeath(DDNum)%Temp(TimeStepIndex) > DesDayWeath(DDNum)%Temp(TimeStepAtPeak)) THEN
               TimeStepAtPeak = TimeStepIndex
             END IF
            END DO
            ZoneSizing(CtrlZoneNum,DDNum)%TimeStepNumAtCoolMax = TimeStepAtPeak
          END DO
          FinalZoneSizing(CtrlZoneNum)%TimeStepNumAtCoolMax = 1
          FinalZoneSizing(CtrlZoneNum)%CoolDDNum = 1
          TimeStepAtPeakF = FinalZoneSizing(CtrlZoneNum)%TimeStepNumAtCoolMax
          DDNumF = FinalZoneSizing(CtrlZoneNum)%CoolDDNum
          DO DDNum = 1,TotDesDays+TotRunDesPersDays
            TimeStepAtPeak = ZoneSizing(CtrlZoneNum,DDNum)%TimeStepNumAtCoolMax
            IF (DesDayWeath(DDNum)%Temp(TimeStepAtPeak) > DesDayWeath(DDNumF)%Temp(TimeStepAtPeakF)) THEN
              DDNumF = DDNum
              TimeStepAtPeakF = TimeStepAtPeak
            END IF
          END DO
          FinalZoneSizing(CtrlZoneNum)%TimeStepNumAtCoolMax = TimeStepAtPeakF
          FinalZoneSizing(CtrlZoneNum)%CoolDDNum = DDNumF
          FinalZoneSizing(CtrlZoneNum)%CoolDesDay = ZoneSizing(CtrlZoneNum,DDNumF)%CoolDesDay

          ! initialize sizing conditions if they have not been set (i.e., no corresponding load) to zone condition
          IF(FinalZoneSizing(CtrlZoneNum)%ZoneTempAtCoolPeak == 0.d0)THEN
            FinalZoneSizing(CtrlZoneNum)%ZoneTempAtCoolPeak = ZoneSizing(CtrlZoneNum,DDNumF)%DesCoolSetPtSeq(TimeStepAtPeakF)
            FinalZoneSizing(CtrlZoneNum)%ZoneHumRatAtCoolPeak = ZoneSizing(CtrlZoneNum,DDNumF)%CoolZoneHumRatSeq(TimeStepAtPeakF)
            IF(FinalZoneSizing(CtrlZoneNum)%ZoneHumRatAtCoolPeak .GT. 0.d0)THEN
              FinalZoneSizing(CtrlZoneNum)%ZoneHumRatAtCoolPeak = &
                 MIN(FinalZoneSizing(CtrlZoneNum)%ZoneHumRatAtCoolPeak, &
                   PsyWFnTdpPb(FinalZoneSizing(CtrlZoneNum)%ZoneTempAtCoolPeak,StdBaroPress,'UpdateZoneSizing'))

            ELSE
              FinalZoneSizing(CtrlZoneNum)%ZoneHumRatAtCoolPeak = ZoneSizing(CtrlZoneNum,DDNumF)%CoolDesHumRat
            END IF
            FinalZoneSizing(CtrlZoneNum)%DesCoolCoilInTemp = FinalZoneSizing(CtrlZoneNum)%ZoneTempAtCoolPeak
            FinalZoneSizing(CtrlZoneNum)%DesCoolCoilInHumRat = FinalZoneSizing(CtrlZoneNum)%ZoneHumRatAtCoolPeak
            FinalZoneSizing(CtrlZoneNum)%ZoneRetTempAtCoolPeak = FinalZoneSizing(CtrlZoneNum)%ZoneTempAtCoolPeak
          END IF
        END IF
        ! Now take into account the user specified sizing factor or user specified heating design air flow
        ! rate (which overrides the sizing factor)
        TotHeatSizMult = 0.0d0
        ! Calculate a sizing factor from the user specified heating design air flow rate
        IF (FinalZoneSizing(CtrlZoneNum)%InpDesHeatAirFlow > 0.0d0 .AND. &
            FinalZoneSizing(CtrlZoneNum)%HeatAirDesMethod == InpDesAirFlow .AND. &
            FinalZoneSizing(CtrlZoneNum)%DesHeatVolFlow > 0.0d0) THEN
          TotHeatSizMult = (FinalZoneSizing(CtrlZoneNum)%InpDesHeatAirFlow / &
            FinalZoneSizing(CtrlZoneNum)%DesHeatVolFlow) * FinalZoneSizing(CtrlZoneNum)%HeatSizingFactor
        ! Calculate a sizing factor from the user specified max heating design air flow rates
        ELSE IF ( FinalZoneSizing(CtrlZoneNum)%HeatAirDesMethod == DesAirFlowWithLim .AND. &
            FinalZoneSizing(CtrlZoneNum)%DesHeatVolFlow > 0.0d0) THEN
            MaxHeatVolFlow = MAX(FinalZoneSizing(CtrlZoneNum)%DesHeatMaxAirFlow, &
                                                             FinalZoneSizing(CtrlZoneNum)%DesHeatMaxAirFlow2, &
                                                             FinalZoneSizing(CtrlZoneNum)%DesCoolVolFlow * &
                                                             FinalZoneSizing(CtrlZoneNum)%DesHeatMaxAirFlowFrac)
          IF (MaxHeatVolFlow < FinalZoneSizing(CtrlZoneNum)%DesHeatVolFlow) THEN
            TotHeatSizMult = (MaxHeatVolFlow / FinalZoneSizing(CtrlZoneNum)%DesHeatVolFlow) *   &
               FinalZoneSizing(CtrlZoneNum)%HeatSizingFactor
          ELSE
            TotHeatSizMult = FinalZoneSizing(CtrlZoneNum)%HeatSizingFactor
          END IF
        ! If no user specified heating design air flow rate input, use the user specified sizing factor
        ELSE
          TotHeatSizMult = FinalZoneSizing(CtrlZoneNum)%HeatSizingFactor
        END IF

        IF (ABS(TotHeatSizMult-1.0d0) > .00001d0 ) THEN
         IF (FinalZoneSizing(CtrlZoneNum)%DesHeatVolFlow > 0.0d0) THEN
            TimeStepAtPeak = FinalZoneSizing(CtrlZoneNum)%TimeStepNumAtHeatMax
            DDNum = FinalZoneSizing(CtrlZoneNum)%HeatDDNum
            FinalZoneSizing(CtrlZoneNum)%DesHeatVolFlow = CalcFinalZoneSizing(CtrlZoneNum)%DesHeatVolFlow * TotHeatSizMult
            FinalZoneSizing(CtrlZoneNum)%DesHeatMassFlow = CalcFinalZoneSizing(CtrlZoneNum)%DesHeatMassFlow * TotHeatSizMult
            FinalZoneSizing(CtrlZoneNum)%DesHeatLoad = CalcFinalZoneSizing(CtrlZoneNum)%DesHeatLoad * TotHeatSizMult
            FinalZoneSizing(CtrlZoneNum)%HeatFlowSeq = CalcFinalZoneSizing(CtrlZoneNum)%HeatFlowSeq * TotHeatSizMult
            FinalZoneSizing(CtrlZoneNum)%HeatLoadSeq = CalcFinalZoneSizing(CtrlZoneNum)%HeatLoadSeq * TotHeatSizMult
            OAFrac = FinalZoneSizing(CtrlZoneNum)%MinOA &
                       / FinalZoneSizing(CtrlZoneNum)%DesHeatVolFlow
            OAFrac = MIN(1.0d0,MAX(0.0d0,OAFrac))
            FinalZoneSizing(CtrlZoneNum)%DesHeatCoilInTemp = OAFrac*DesDayWeath(DDNum)%Temp(TimeStepAtPeak) + &
                                                      (1.d0-OAFrac)*FinalZoneSizing(CtrlZoneNum)%ZoneTempAtHeatPeak
            FinalZoneSizing(CtrlZoneNum)%DesHeatCoilInHumRat = &
                                                 OAFrac*DesDayWeath(DDNum)%HumRat(TimeStepAtPeak) + &
                                                 (1.d0-OAFrac)*FinalZoneSizing(CtrlZoneNum)%ZoneHumRatAtHeatPeak
          ELSE
            FinalZoneSizing(CtrlZoneNum)%DesHeatVolFlow = FinalZoneSizing(CtrlZoneNum)%InpDesHeatAirFlow
            FinalZoneSizing(CtrlZoneNum)%DesHeatMassFlow = FinalZoneSizing(CtrlZoneNum)%DesHeatVolFlow * &
              FinalZoneSizing(CtrlZoneNum)%DesHeatDens
          END IF
          DO DDNum = 1,TotDesDays+TotRunDesPersDays
            IF (ZoneSizing(CtrlZoneNum,DDNum)%DesHeatVolFlow > 0.0d0) THEN
              TimeStepAtPeak = ZoneSizing(CtrlZoneNum,DDNum)%TimeStepNumAtHeatMax
              ZoneSizing(CtrlZoneNum,DDNum)%DesHeatVolFlow = CalcZoneSizing(CtrlZoneNum,DDNum)%DesHeatVolFlow * &
                                                               TotHeatSizMult
              ZoneSizing(CtrlZoneNum,DDNum)%DesHeatMassFlow = CalcZoneSizing(CtrlZoneNum,DDNum)%DesHeatMassFlow &
                                                               * TotHeatSizMult
              ZoneSizing(CtrlZoneNum,DDNum)%DesHeatLoad = CalcZoneSizing(CtrlZoneNum,DDNum)%DesHeatLoad * TotHeatSizMult
              ZoneSizing(CtrlZoneNum,DDNum)%HeatFlowSeq = CalcZoneSizing(CtrlZoneNum,DDNum)%HeatFlowSeq * TotHeatSizMult
              ZoneSizing(CtrlZoneNum,DDNum)%HeatLoadSeq = CalcZoneSizing(CtrlZoneNum,DDNum)%HeatLoadSeq * TotHeatSizMult
              OAFrac = ZoneSizing(CtrlZoneNum,DDNum)%MinOA &
                         / ZoneSizing(CtrlZoneNum,DDNum)%DesHeatVolFlow
              OAFrac = MIN(1.0d0,MAX(0.0d0,OAFrac))
              ZoneSizing(CtrlZoneNum,DDNum)%DesHeatCoilInTemp = OAFrac*DesDayWeath(DDNum)%Temp(TimeStepAtPeak) + &
                                                        (1.d0-OAFrac)*ZoneSizing(CtrlZoneNum,DDNum)%ZoneTempAtHeatPeak
              ZoneSizing(CtrlZoneNum,DDNum)%DesHeatCoilInHumRat = &
                                                   OAFrac*DesDayWeath(DDNum)%HumRat(TimeStepAtPeak) + &
                                                   (1.d0-OAFrac)*ZoneSizing(CtrlZoneNum,DDNum)%ZoneHumRatAtHeatPeak
            ELSE
              ZoneSizing(CtrlZoneNum,DDNum)%DesHeatVolFlow = ZoneSizing(CtrlZoneNum,DDNum)%InpDesHeatAirFlow
              ZoneSizing(CtrlZoneNum,DDNum)%DesHeatMassFlow = ZoneSizing(CtrlZoneNum,DDNum)%DesHeatVolFlow * &
                ZoneSizing(CtrlZoneNum,DDNum)%DesHeatDens
            END IF
          END DO
        END IF
        MinOAMass = FinalZoneSizing(CtrlZoneNum)%MinOA * FinalZoneSizing(CtrlZoneNum)%DesHeatDens
        IF (FinalZoneSizing(CtrlZoneNum)%MinOA > FinalZoneSizing(CtrlZoneNum)%DesHeatVolFlow) THEN
          FinalZoneSizing(CtrlZoneNum)%DesHeatVolFlow = FinalZoneSizing(CtrlZoneNum)%MinOA
          FinalZoneSizing(CtrlZoneNum)%DesHeatMassFlow = MinOAMass
        END IF
        DO TimeStepIndex = 1,NumOfTimeStepInDay
          IF (MinOAMass > FinalZoneSizing(CtrlZoneNum)%HeatFlowSeq(TimeStepIndex)) THEN
            FinalZoneSizing(CtrlZoneNum)%HeatFlowSeq(TimeStepIndex) = MinOAMass
          END IF
        END DO
        DO DDNum = 1,TotDesDays+TotRunDesPersDays
          MinOAMass = ZoneSizing(CtrlZoneNum,DDNum)%MinOA * ZoneSizing(CtrlZoneNum,DDNum)%DesHeatDens
          IF (ZoneSizing(CtrlZoneNum,DDNum)%MinOA > ZoneSizing(CtrlZoneNum,DDNum)%DesHeatVolFlow) THEN
            ZoneSizing(CtrlZoneNum,DDNum)%DesHeatVolFlow = ZoneSizing(CtrlZoneNum,DDNum)%MinOA
            ZoneSizing(CtrlZoneNum,DDNum)%DesHeatMassFlow = MinOAMass
          END IF
          DO TimeStepIndex = 1,NumOfTimeStepInDay
            IF (MinOAMass > ZoneSizing(CtrlZoneNum,DDNum)%HeatFlowSeq(TimeStepIndex)) THEN
              ZoneSizing(CtrlZoneNum,DDNum)%HeatFlowSeq(TimeStepIndex) = MinOAMass
            END IF
          END DO
        END DO
! IF heating flow rate is 0, this data may be used to size a HP so initialize DDNum, TimeStepatPeak, and sizing data (end of IF)
        ! check for flow rate having been set (by MinOA or other min) but no timestep at max
!        IF (FinalZoneSizing(CtrlZoneNum)%DesHeatMassFlow > 0.0d0 .AND. &
         IF ((FinalZoneSizing(CtrlZoneNum)%TimeStepNumAtHeatMax == 0 .OR. &
              FinalZoneSizing(CtrlZoneNum)%HeatDDNum == 0) ) THEN
          DO DDNum = 1,TotDesDays+TotRunDesPersDays
            ZoneSizing(CtrlZoneNum,DDNum)%TimeStepNumAtHeatMax = 1
            TimeStepAtPeak = ZoneSizing(CtrlZoneNum,DDNum)%TimeStepNumAtHeatMax
            DO TimeStepIndex = 1,NumOfTimeStepInDay
             IF (DesDayWeath(DDNum)%Temp(TimeStepIndex) < DesDayWeath(DDNum)%Temp(TimeStepAtPeak)) THEN
               TimeStepAtPeak = TimeStepIndex
             END IF
            END DO
            ZoneSizing(CtrlZoneNum,DDNum)%TimeStepNumAtHeatMax = TimeStepAtPeak
          END DO
          FinalZoneSizing(CtrlZoneNum)%TimeStepNumAtHeatMax = 1
          FinalZoneSizing(CtrlZoneNum)%HeatDDNum = 1
          TimeStepAtPeakF = FinalZoneSizing(CtrlZoneNum)%TimeStepNumAtHeatMax
          DDNumF = FinalZoneSizing(CtrlZoneNum)%HeatDDNum
          DO DDNum = 1,TotDesDays+TotRunDesPersDays
            TimeStepAtPeak = ZoneSizing(CtrlZoneNum,DDNum)%TimeStepNumAtHeatMax
            IF (DesDayWeath(DDNum)%Temp(TimeStepAtPeak) < DesDayWeath(DDNumF)%Temp(TimeStepAtPeakF)) THEN
              DDNumF = DDNum
              TimeStepAtPeakF = TimeStepAtPeak
            END IF
          END DO
          FinalZoneSizing(CtrlZoneNum)%TimeStepNumAtHeatMax = TimeStepAtPeakF
          FinalZoneSizing(CtrlZoneNum)%HeatDDNum = DDNumF
          FinalZoneSizing(CtrlZoneNum)%HeatDesDay = ZoneSizing(CtrlZoneNum,DDNumF)%HeatDesDay

          ! initialize sizing conditions if they have not been set (i.e., no corresponding load) to zone condition
          IF(FinalZoneSizing(CtrlZoneNum)%ZoneTempAtHeatPeak == 0.d0)THEN
            FinalZoneSizing(CtrlZoneNum)%ZoneTempAtHeatPeak = ZoneSizing(CtrlZoneNum,DDNumF)%DesHeatSetPtSeq(TimeStepAtPeakF)
            FinalZoneSizing(CtrlZoneNum)%ZoneHumRatAtHeatPeak = ZoneSizing(CtrlZoneNum,DDNumF)%HeatZoneHumRatSeq(TimeStepAtPeakF)
            IF(FinalZoneSizing(CtrlZoneNum)%ZoneHumRatAtHeatPeak .GT. 0.d0)THEN
              FinalZoneSizing(CtrlZoneNum)%ZoneHumRatAtHeatPeak = &
                 MIN(FinalZoneSizing(CtrlZoneNum)%ZoneHumRatAtHeatPeak, &
                   PsyWFnTdpPb(FinalZoneSizing(CtrlZoneNum)%ZoneTempAtHeatPeak,StdBaroPress,'UpdateZoneSizing'))
            ELSE
              FinalZoneSizing(CtrlZoneNum)%ZoneHumRatAtHeatPeak = ZoneSizing(CtrlZoneNum,DDNumF)%HeatDesHumRat
            END IF
            FinalZoneSizing(CtrlZoneNum)%DesHeatCoilInTemp = FinalZoneSizing(CtrlZoneNum)%ZoneTempAtHeatPeak
            FinalZoneSizing(CtrlZoneNum)%DesHeatCoilInHumRat = FinalZoneSizing(CtrlZoneNum)%ZoneHumRatAtHeatPeak
            FinalZoneSizing(CtrlZoneNum)%ZoneRetTempAtHeatPeak = FinalZoneSizing(CtrlZoneNum)%ZoneTempAtHeatPeak
          END IF
        END IF

        ! set the zone minimum cooling supply air flow rate. This will be used for autosizing VAV terminal unit
        ! minimum flow rates
        FinalZoneSizing(CtrlZoneNum)%DesCoolVolFlowMin = MAX(FinalZoneSizing(CtrlZoneNum)%DesCoolMinAirFlow, &
                                                             FinalZoneSizing(CtrlZoneNum)%DesCoolMinAirFlow2, &
                                                             FinalZoneSizing(CtrlZoneNum)%DesCoolVolFlow * &
                                                               FinalZoneSizing(CtrlZoneNum)%DesCoolMinAirFlowFrac)
        ! set the zone maximum heating supply air flow rate. This will be used for autosizing VAV terminal unit
        ! max heating flow rates
        FinalZoneSizing(CtrlZoneNum)%DesHeatVolFlowMax = MAX(FinalZoneSizing(CtrlZoneNum)%DesHeatMaxAirFlow, &
                                                             FinalZoneSizing(CtrlZoneNum)%DesHeatMaxAirFlow2, &
                                                             FinalZoneSizing(CtrlZoneNum)%DesHeatVolFlow * &
                                                               FinalZoneSizing(CtrlZoneNum)%DesHeatMaxAirFlowFrac)
        ! Determine the design cooling supply air temperature if the supply air temperature difference is specified by user.
        IF (FinalZoneSizing(CtrlZoneNum)%ZnCoolDgnSAMethod == TemperatureDifference) THEN
          FinalZoneSizing(CtrlZoneNum)%CoolDesTemp = FinalZoneSizing(CtrlZoneNum)%ZoneTempAtCoolPeak - &
                                                       ABS(FinalZoneSizing(CtrlZoneNum)%CoolDesTempDiff)
        END IF
        ! Determine the design heating supply air temperature if the supply air temperature difference is specified by user.
        IF (FinalZoneSizing(CtrlZoneNum)%ZnHeatDgnSAMethod == TemperatureDifference) THEN
          FinalZoneSizing(CtrlZoneNum)%HeatDesTemp = FinalZoneSizing(CtrlZoneNum)%ZoneTempAtHeatPeak + &
                                                       ABS(FinalZoneSizing(CtrlZoneNum)%HeatDesTempDiff)
        END IF
      END DO

  END SELECT

  TermUnitFinalZoneSizing = FinalZoneSizing

  RETURN

END SUBROUTINE UpdateZoneSizing

SUBROUTINE SimZoneEquipment(FirstHVACIteration, SimAir)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Russ Taylor
          !       DATE WRITTEN   May 1997
          !       MODIFIED       Raustad/Shirey, FSEC, June 2003
          !       MODIFIED       Gu, FSEC, Jan. 2004, Don Shirey, Aug 2009 (LatOutputProvided)
          !                      July 2012, Chandan Sharma - FSEC: Added zone sys avail managers
          !       RE-ENGINEERED  na

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
          ! example would be a VAV system with baseboard heaters

          ! METHODOLOGY EMPLOYED:
          ! 1.  Determine zone load - this is zone temperature dependent
          ! 2.  Determine balance point - the temperature at which the
          !     zone load is balanced by the system output. The way the
          !     balance point is determined will be different depending on
          !     the type of system being simulated.
          ! 3.  Calculate zone energy requirements

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataHVACGlobals
  USE DataHeatBalFanSys, ONLY: NonAirSystemResponse, SysDepZoneLoads
  USE ReturnAirPathManager, ONLY: SimReturnAirPath
  USE ZoneAirLoopEquipmentManager, ONLY: ManageZoneAirLoopEquipment
  USE PurchasedAirManager, ONLY: SimPurchasedAir
  USE DirectAirManager, ONLY: SimDirectAir
  USE HWBaseboardRadiator, ONLY: SimHWBaseboard
  USE SteamBaseboardRadiator, ONLY: SimSteamBaseboard
  USE BaseboardRadiator, ONLY: SimBaseboard
  USE BaseboardElectric, ONLY: SimElectricBaseboard
  USE SplitterComponent, ONLY: SimAirLoopSplitter
  USE FanCoilUnits, ONLY: SimFanCoilUnit
  USE Fans, ONLY: SimulateFanComponents
  USE WindowAC, ONLY: SimWindowAC
  USE PackagedTerminalHeatPump, ONLY: SimPackagedTerminalUnit
  USE ZoneDehumidifier, ONLY: SimZoneDehumidifier
  USE UnitVentilator, ONLY : SimUnitVentilator
  USE UnitHeater, ONLY : SimUnitHeater
  USE HeatRecovery, ONLY : SimHeatRecovery
  USE OutdoorAirUnit, ONLY : SimOutdoorAirUnit
  USE HVACStandAloneERV, ONLY: SimStandAloneERV
  USE LowTempRadiantSystem, ONLY : SimLowTempRadiantSystem
  USE HighTempRadiantSystem, ONLY : SimHighTempRadiantSystem
  USE VentilatedSlab, ONLY : SimVentilatedSlab
  USE ZonePlenum, ONLY : SimAirZonePlenum
  USE DataAirflowNetwork, ONLY: SimulateAirflowNetwork,AirflowNetworkFanActivated,AirflowNetworkControlMultizone
  USE WaterThermalTanks, ONLY: SimHeatPumpWaterHeater
  USE DataAirSystems, ONLY : PrimaryAirSystem
  USE DataAirLoop, ONLY : AirLoopControlInfo
  USE ElectricBaseboardRadiator, ONLY: SimElecBaseboard
  USE HVACVariableRefrigerantFlow, ONLY: SimulateVRF
  USE RefrigeratedCase, ONLY: SimAirChillerSet
  USE UserDefinedComponents, ONLY: SimZoneAirUserDefined
  USE SystemAvailabilityManager, ONLY: GetZoneEqAvailabilityManager
  USE DataGlobals, ONLY: isPulseZoneSizing
  USE EvaporativeCoolers, ONLY: SimZoneEvaporativeCoolerUnit
  USE HVACUnitarySystem, ONLY: SimUnitarySystem

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  LOGICAL FirstHVACIteration
  LOGICAL SimAir

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: ActualZoneNum
  INTEGER :: ControlledZoneNum
  INTEGER :: EquipTypeNum
  INTEGER :: SupplyAirPathNum
  INTEGER :: CompNum
  INTEGER :: EquipPtr
  INTEGER :: AirLoopNum
  INTEGER :: ZoneEquipTypeNum
  INTEGER :: ZoneCompNum

  LOGICAL :: SupPathInletChanged = .FALSE.
  LOGICAL,SAVE :: FirstCall  ! indicates first call to supply air path components
  LOGICAL,SAVE :: MyOneTimeFlag = .TRUE.
  LOGICAL      :: ErrorFlag
  LOGICAL      :: ValidSAMComp  = .FALSE.

  REAL(r64) :: SysOutputProvided ! sensible output delivered by zone equipment (W)
  REAL(r64) :: LatOutputProvided ! latent output delivered by zone equipment (kg/s)
  REAL(r64) :: AirSysOutput
  REAL(r64) :: NonAirSysOutput
  LOGICAL   :: ZoneHasAirLoopHVACTerminal = .FALSE. ! true if zone has an air loop terminal
  LOGICAL   :: ZoneHasAirLoopHVACDirectAir = .FALSE. ! true if zone has an uncontrolled air loop terminal
  LOGICAL, DIMENSION(:), ALLOCATABLE, SAVE :: DirectAirAndAirTerminalWarningIssued ! only warn once for each zone with problems

       ! Determine flow rate and temperature of supply air based on type of damper

     FirstCall = .TRUE.
     ErrorFlag = .FALSE.

     DO SupplyAirPathNum = 1, NumSupplyAirPaths

       DO CompNum = 1, SupplyAirPath(SupplyAirPathNum)%NumOfComponents
         SELECT CASE (SupplyAirPath(SupplyAirPathNum)%ComponentType_Num(CompNum))

           CASE (ZoneSplitter_Type) ! 'AirLoopHVAC:ZoneSplitter'

             if (.NOT. (AirflowNetworkFanActivated .AND. SimulateAirflowNetwork > AirflowNetworkControlMultizone)) then
               CALL SimAirLoopSplitter(SupplyAirPath(SupplyAirPathNum)%ComponentName(CompNum), &
                               FirstHVACIteration, FirstCall, SupPathInletChanged,             &
                               CompIndex=SupplyAirPath(SupplyAirPathNum)%ComponentIndex(CompNum))
             endif

           CASE (ZoneSupplyPlenum_Type)  ! 'AirLoopHVAC:SupplyPlenum'

             CALL SimAirZonePlenum(SupplyAirPath(SupplyAirPathNum)%ComponentName(CompNum),ZoneSupplyPlenum_Type, &
                               SupplyAirPath(SupplyAirPathNum)%ComponentIndex(CompNum),   &
                               FirstHVACIteration=FirstHVACIteration, FirstCall=FirstCall, &
                               PlenumInletChanged=SupPathInletChanged)

           CASE DEFAULT
             CALL ShowSevereError('Error found in Supply Air Path='//TRIM(SupplyAirPath(SupplyAirPathNum)%Name))
             CALL ShowContinueError('Invalid Supply Air Path Component='//  &
                  TRIM(SupplyAirPath(SupplyAirPathNum)%ComponentType(CompNum)))
             CALL ShowFatalError('Preceding condition causes termination.')

         END SELECT
       END DO

     END DO

     IF (FirstCall .AND. .NOT. ALLOCATED(DirectAirAndAirTerminalWarningIssued)) THEN
       ALLOCATE(DirectAirAndAirTerminalWarningIssued(NumOfZones) )
       DirectAirAndAirTerminalWarningIssued = .FALSE.
     ENDIF

     FirstCall = .FALSE.

     ! Loop over all the primary air loop; simulate their components (equipment)
     ! and controllers

     DO ControlledZoneNum = 1, NumOfZones

       IF (.not. ZoneEquipConfig(ControlledZoneNum)%IsControlled) CYCLE
       ActualZoneNum=ZoneEquipConfig(ControlledZoneNum)%ActualZoneNum

       NonAirSystemResponse(ActualZoneNum) = 0.d0
       SysDepZoneLoads(ActualZoneNum) = 0.d0
       ZoneEquipConfig(ControlledZoneNum)%ZoneExh = 0.d0
       ZoneEquipConfig(ControlledZoneNum)%ZoneExhBalanced = 0.d0
       ZoneEquipConfig(ControlledZoneNum)%PlenumMassFlow = 0.d0
       ZoneHasAirLoopHVACTerminal  = .FALSE.
       ZoneHasAirLoopHVACDirectAir = .FALSE.
       CurZoneEqNum = ControlledZoneNum

       CALL InitSystemOutputRequired(ActualZoneNum, SysOutputProvided, LatOutputProvided)

       CALL SetZoneEquipSimOrder(ControlledZoneNum, ActualZoneNum)

       ! Air loop system availability manager status only applies to PIU and exhaust fans
       ! Reset fan SAM operation flags for zone fans.
       TurnFansOn  = .FALSE.
       TurnFansOff = .FALSE.

       DO EquipTypeNum = 1, ZoneEquipList(ControlledZoneNum)%NumOfEquipTypes

         UnbalExhMassFlow = 0.d0
         BalancedExhMassFlow = 0.d0
         PlenumInducedMassFlow = 0.0d0
         EquipPtr=PrioritySimOrder(EquipTypeNum)%EquipPtr
         SysOutputProvided = 0.d0
         LatOutputProvided = 0.d0

         ZoneEquipTypeNum = PrioritySimOrder(EquipTypeNum)%EquipType_Num

         ZoneCompNum = ZoneEquipList(CurZoneEqNum)%EquipIndex(EquipPtr)

         ValidSAMComp = .FALSE.

         IF (ZoneEquipTypeNum .LE. NumValidSysAvailZoneComponents) ValidSAMComp = .TRUE.

         IF (ZoneCompNum .GT. 0 .AND. ValidSAMComp) THEN

           CALL GetZoneEqAvailabilityManager(ZoneEquipTypeNum, ZoneCompNum, ErrorFlag)

           IF(ZoneComp(ZoneEquipTypeNum)%ZoneCompAvailMgrs(ZoneCompNum)%AvailStatus .EQ. CycleOn) THEN
             ZoneCompTurnFansOn  = .TRUE.
             ZoneCompTurnFansOff = .FALSE.
           ELSEIF(ZoneComp(ZoneEquipTypeNum)%ZoneCompAvailMgrs(ZoneCompNum)%AvailStatus .EQ. ForceOff) THEN
             ZoneCompTurnFansOn  = .FALSE.
             ZoneCompTurnFansOff = .TRUE.
           ELSE
             ZoneCompTurnFansOn  = TurnFansOn
             ZoneCompTurnFansOff = TurnFansOff
           ENDIF
         ELSE
             ZoneCompTurnFansOn  = TurnFansOn
             ZoneCompTurnFansOff = TurnFansOff
         ENDIF

         SELECT CASE (ZoneEquipTypeNum)

            CASE(AirDistUnit_Num)  ! 'ZoneHVAC:AirDistributionUnit'

             ! Air loop system availability manager status only applies to PIU and exhaust fans
             ! Check to see if System Availability Managers are asking for fans to cycle on or shut off
             ! and set fan on/off flags accordingly.
             IF (ZoneEquipAvail(ControlledZoneNum).EQ.CycleOn .OR. &
                 ZoneEquipAvail(ControlledZoneNum).EQ.CycleOnZoneFansOnly) THEN
               TurnFansOn = .TRUE.
             END IF
             IF (ZoneEquipAvail(ControlledZoneNum).EQ.ForceOff) THEN
               TurnFansOff = .TRUE.
             END IF

             CALL ManageZoneAirLoopEquipment(PrioritySimOrder(EquipTypeNum)%EquipName, &
                                             FirstHVACIteration, AirSysOutput, NonAirSysOutput, &
                                             LatOutputProvided, ActualZoneNum, ControlledZoneNum, &
                                             ZoneEquipList(CurZoneEqNum)%EquipIndex(EquipPtr) )

!            reset status flags for other zone equipment
             TurnFansOn  = .FALSE.
             TurnFansOff = .FALSE.

             NonAirSystemResponse(ActualZoneNum) = NonAirSystemResponse(ActualZoneNum) + NonAirSysOutput
             SysOutputProvided = NonAirSysOutput + AirSysOutput
             ZoneHasAirLoopHVACTerminal = .TRUE.
           CASE(DirectAir_Num)  ! 'AirTerminal:SingleDuct:Uncontrolled'
             CALL SimDirectAir(PrioritySimOrder(EquipTypeNum)%EquipName, &
                                  ControlledZoneNum, FirstHVACIteration, &
                                  SysOutputProvided, LatOutputProvided, &
                                  ZoneEquipList(CurZoneEqNum)%EquipIndex(EquipPtr))
             ZoneHasAirLoopHVACDirectAir = .TRUE.
           CASE(VRFTerminalUnit_Num)  ! 'ZoneHVAC:TerminalUnit:VariableRefrigerantFlow'
             CALL SimulateVRF(PrioritySimOrder(EquipTypeNum)%EquipName, &
                                  ControlledZoneNum, FirstHVACIteration, &
                                  SysOutputProvided, LatOutputProvided, &
                                  ZoneEquipList(CurZoneEqNum)%EquipIndex(EquipPtr))

           CASE (WindowAC_Num)  ! 'ZoneHVAC:WindowAirConditioner'
             CALL SimWindowAC(PrioritySimOrder(EquipTypeNum)%EquipName, ActualZoneNum, &
                                 FirstHVACIteration, SysOutputProvided, LatOutputProvided, &
                                 ZoneEquipList(CurZoneEqNum)%EquipIndex(EquipPtr))

           CASE (PkgTermHPAirToAir_Num, PkgTermACAirToAir_Num, PkgTermHPWaterToAir_Num)  ! 'ZoneHVAC:PackagedTerminalHeatPump'
                                         ! 'ZoneHVAC:PackagedTerminalAirConditioner'
                                         ! 'ZoneHVAC:WaterToAirHeatPump'
             CALL SimPackagedTerminalUnit(PrioritySimOrder(EquipTypeNum)%EquipName, ActualZoneNum, &
                                 FirstHVACIteration, SysOutputProvided, LatOutputProvided, &
                                 ZoneEquipTypeNum, ZoneEquipList(CurZoneEqNum)%EquipIndex(EquipPtr))

           CASE (ZoneUnitarySystem_Num)  ! 'AirloopHVAC:UnitarySystem'
             CALL SimUnitarySystem(PrioritySimOrder(EquipTypeNum)%EquipName, FirstHVACIteration, &
                                 ActualZoneNum, ZoneEquipList(CurZoneEqNum)%EquipIndex(EquipPtr),ZoneEquipment=.TRUE.)

           CASE (ZoneDXDehumidifier_Num) ! 'ZoneHVAC:Dehumidifier:DX'
             CALL SimZoneDehumidifier(PrioritySimOrder(EquipTypeNum)%EquipName, ActualZoneNum, &
                                      FirstHVACIteration, SysOutputProvided, LatOutputProvided, &
                                      ZoneEquipList(CurZoneEqNum)%EquipIndex(EquipPtr))

             SysDepZoneLoads(ActualZoneNum) = SysDepZoneLoads(ActualZoneNum) + SysOutputProvided

             SysOutputProvided = 0.0d0 ! Reset to 0.0 since this equipment is controlled based on zone humidity level (not
                                       ! temperature) SysOutputProvided amount was already sent above to
                                       ! next Predict-Correct series of calcs via SysDepZoneLoads

           CASE (FanCoil4Pipe_Num) ! 'ZoneHVAC:FourPipeFanCoil'
             CALL SimFanCoilUnit(PrioritySimOrder(EquipTypeNum)%EquipName, ActualZoneNum, ControlledZoneNum, &
                                     FirstHVACIteration, SysOutputProvided, LatOutputProvided, &
                                     ZoneEquipList(CurZoneEqNum)%EquipIndex(EquipPtr))

           CASE (UnitVentilator_Num)  ! 'ZoneHVAC:UnitVentilator'
             CALL SimUnitVentilator(PrioritySimOrder(EquipTypeNum)%EquipName, ActualZoneNum, &
                                    FirstHVACIteration, SysOutputProvided, LatOutputProvided, &
                                    ZoneEquipList(CurZoneEqNum)%EquipIndex(EquipPtr))

           CASE (UnitHeater_Num)  ! 'ZoneHVAC:UnitHeater'
             CALL SimUnitHeater(PrioritySimOrder(EquipTypeNum)%EquipName, ActualZoneNum, &
                                FirstHVACIteration, SysOutputProvided, LatOutputProvided, &
                                ZoneEquipList(CurZoneEqNum)%EquipIndex(EquipPtr))

           CASE(PurchasedAir_Num)  ! 'ZoneHVAC:IdealLoadsAirSystem'
             CALL SimPurchasedAir(PrioritySimOrder(EquipTypeNum)%EquipName, &
                                  SysOutputProvided, LatOutputProvided, FirstHVACIteration, &
                                  ControlledZoneNum, ActualZoneNum, ZoneEquipList(CurZoneEqNum)%EquipIndex(EquipPtr))

           CASE(BBWater_Num) ! 'ZoneHVAC:Baseboard:RadiantConvective:Water'
             CALL SimHWBaseboard(PrioritySimOrder(EquipTypeNum)%EquipName, ActualZoneNum,       &
                                  ControlledZoneNum, FirstHVACIteration, SysOutputProvided,     &
                                  ZoneEquipList(CurZoneEqNum)%EquipIndex(EquipPtr))

             NonAirSystemResponse(ActualZoneNum) = NonAirSystemResponse(ActualZoneNum) + SysOutputProvided
             LatOutputProvided = 0.0d0 ! This baseboard does not add/remove any latent heat

           CASE(BBSteam_Num) ! 'ZoneHVAC:Baseboard:RadiantConvective:Steam'
             CALL SimSteamBaseboard(PrioritySimOrder(EquipTypeNum)%EquipName, ActualZoneNum,       &
                                  ControlledZoneNum, FirstHVACIteration, SysOutputProvided,     &
                                  ZoneEquipList(CurZoneEqNum)%EquipIndex(EquipPtr))

             NonAirSystemResponse(ActualZoneNum) = NonAirSystemResponse(ActualZoneNum) + SysOutputProvided
             LatOutputProvided = 0.0d0 ! This baseboard does not add/remove any latent heat

           CASE(BBWaterConvective_Num)  ! 'ZoneHVAC:Baseboard:Convective:Water'
             CALL SimBaseboard(PrioritySimOrder(EquipTypeNum)%EquipName, ActualZoneNum,       &
                                  ControlledZoneNum, FirstHVACIteration, SysOutputProvided,   &
                                  ZoneEquipList(CurZoneEqNum)%EquipIndex(EquipPtr))

             NonAirSystemResponse(ActualZoneNum) = NonAirSystemResponse(ActualZoneNum) + SysOutputProvided
             LatOutputProvided = 0.0d0 ! This baseboard does not add/remove any latent heat

           CASE(BBElectricConvective_Num)  ! 'ZoneHVAC:Baseboard:Convective:Electric'
             CALL SimElectricBaseBoard(PrioritySimOrder(EquipTypeNum)%EquipName, ActualZoneNum, &
                                  ControlledZoneNum, SysOutputProvided, ZoneEquipList(CurZoneEqNum)%EquipIndex(EquipPtr))

             NonAirSystemResponse(ActualZoneNum) = NonAirSystemResponse(ActualZoneNum) + SysOutputProvided
             LatOutputProvided = 0.0d0 ! This baseboard does not add/remove any latent heat

           CASE(HiTempRadiant_Num) ! 'ZoneHVAC:HighTemperatureRadiant'
             CALL SimHighTempRadiantSystem(PrioritySimOrder(EquipTypeNum)%EquipName, &
                                           FirstHVACIteration, SysOutputProvided,    &
                                           ZoneEquipList(CurZoneEqNum)%EquipIndex(EquipPtr))
             LatOutputProvided = 0.0d0 ! This baseboard currently sends its latent heat gain directly to predictor/corrector
                                       ! via SumLatentHTRadSys... so setting LatOutputProvided = 0.0

           CASE (LoTempRadiant_Num) ! 'ZoneHVAC:LowTemperatureRadiant:VariableFlow', 'ZoneHVAC:LowTemperatureRadiant:ConstantFlow'
                                    ! 'ZoneHVAC:LowTemperatureRadiant:Electric'
             CALL SimLowTempRadiantSystem(PrioritySimOrder(EquipTypeNum)%EquipName, &
                                          FirstHVACIteration, SysOutputProvided,    &
                                          ZoneEquipList(CurZoneEqNum)%EquipIndex(EquipPtr))
             LatOutputProvided = 0.0d0 ! This baseboard does not add/remove any latent heat

           CASE(ZoneExhaustFan_Num)  ! 'Fan:ZoneExhaust'

             ! Air loop system availability manager status only applies to PIU and exhaust fans
             ! Check to see if System Availability Managers are asking for fans to cycle on or shut off
             ! and set fan on/off flags accordingly.
             IF (ZoneEquipAvail(ControlledZoneNum).EQ.CycleOn .OR. &
                 ZoneEquipAvail(ControlledZoneNum).EQ.CycleOnZoneFansOnly) THEN
               TurnFansOn = .TRUE.
             END IF
             IF (ZoneEquipAvail(ControlledZoneNum).EQ.ForceOff) THEN
               TurnFansOff = .TRUE.
             END IF

             CALL SimulateFanComponents(PrioritySimOrder(EquipTypeNum)%EquipName, &
                                  FirstHVACIteration,ZoneEquipList(CurZoneEqNum)%EquipIndex(EquipPtr))

!            reset status flags for other zone equipment
             TurnFansOn  = .FALSE.
             TurnFansOff = .FALSE.

           CASE (HeatXchngr_Num)  ! 'HeatExchanger:AirToAir:FlatPlate'
             CALL SimHeatRecovery(PrioritySimOrder(EquipTypeNum)%EquipName,FirstHVACIteration,  &
                                  ZoneEquipList(ControlledZoneNum)%EquipIndex(EquipPtr), ContFanCycCoil)

           CASE (ERVStandAlone_Num)  ! 'ZoneHVAC:EnergyRecoveryVentilator'
             CALL SimStandAloneERV(PrioritySimOrder(EquipTypeNum)%EquipName, ActualZoneNum, &
                                 FirstHVACIteration, SysOutputProvided, LatOutputProvided, &
                                 ZoneEquipList(ControlledZoneNum)%EquipIndex(EquipPtr))

           CASE (HPWaterHeater_Num)  ! 'WaterHeater:HeatPump'
             CALL SimHeatPumpWaterHeater(PrioritySimOrder(EquipTypeNum)%EquipName, &
                                 FirstHVACIteration, SysOutputProvided, LatOutputProvided, &
                                 ZoneEquipList(ControlledZoneNum)%EquipIndex(EquipPtr))
           CASE (VentilatedSlab_Num)  ! 'ZoneHVAC:VentilatedSlab'
             CALL SimVentilatedSlab(PrioritySimOrder(EquipTypeNum)%EquipName, ActualZoneNum, &
                                    FirstHVACIteration, SysOutputProvided, LatOutputProvided, &
                                    ZoneEquipList(CurZoneEqNum)%EquipIndex(EquipPtr))
           CASE (OutdoorAirUnit_Num)  ! 'ZoneHVAC:OutdoorAirUnit'
             CALL SimOutdoorAirUnit(PrioritySimOrder(EquipTypeNum)%EquipName, ActualZoneNum, &
                                    FirstHVACIteration, SysOutputProvided, LatOutputProvided, &
                                    ZoneEquipList(CurZoneEqNum)%EquipIndex(EquipPtr))

           CASE(BBElectric_Num)  ! 'ZoneHVAC:Baseboard:RadiantConvective:Electric'
             CALL SimElecBaseBoard(PrioritySimOrder(EquipTypeNum)%EquipName, ActualZoneNum, &
                                  ControlledZoneNum, FirstHVACIteration, SysOutputProvided, &
                                  ZoneEquipList(CurZoneEqNum)%EquipIndex(EquipPtr))

             NonAirSystemResponse(ActualZoneNum) = NonAirSystemResponse(ActualZoneNum) + SysOutputProvided
             LatOutputProvided = 0.0d0 ! This baseboard does not add/remove any latent heat

           CASE(RefrigerationAirChillerSet_Num)  ! 'ZoneHVAC:RefrigerationChillerSet'
             CALL SimAirChillerSet(PrioritySimOrder(EquipTypeNum)%EquipName, ActualZoneNum, &
                                  FirstHVACIteration, SysOutputProvided,  LatOutputProvided,&
                                  ZoneEquipList(CurZoneEqNum)%EquipIndex(EquipPtr))

             NonAirSystemResponse(ActualZoneNum) = NonAirSystemResponse(ActualZoneNum) + SysOutputProvided

           CASE (UserDefinedZoneHVACForcedAir_Num)
             CALL SimZoneAirUserDefined(PrioritySimOrder(EquipTypeNum)%EquipName, ActualZoneNum, &
                                  SysOutputProvided,  LatOutputProvided,&
                                  ZoneEquipList(CurZoneEqNum)%EquipIndex(EquipPtr))

           CASE (ZoneEvaporativeCoolerUnit_Num)
             CALL SimZoneEvaporativeCoolerUnit(PrioritySimOrder(EquipTypeNum)%EquipName, ActualZoneNum, &
                                  SysOutputProvided,  LatOutputProvided,&
                                  ZoneEquipList(CurZoneEqNum)%EquipIndex(EquipPtr))

           CASE DEFAULT

         END SELECT

         ZoneEquipConfig(ControlledZoneNum)%ZoneExh = ZoneEquipConfig(ControlledZoneNum)%ZoneExh + UnbalExhMassFlow
         ZoneEquipConfig(ControlledZoneNum)%ZoneExhBalanced = ZoneEquipConfig(ControlledZoneNum)%ZoneExhBalanced &
                                                                + BalancedExhMassFlow
         ZoneEquipConfig(ControlledZoneNum)%PlenumMassFlow = ZoneEquipConfig(ControlledZoneNum)%PlenumMassFlow +   &
            PlenumInducedMassFlow

         CALL UpdateSystemOutputRequired(ActualZoneNum, SysOutputProvided, LatOutputProvided, EquipPriorityNum = EquipTypeNum)

         IF (ZoneHasAirLoopHVACTerminal .AND. ZoneHasAirLoopHVACDirectAir) THEN
           ! zone has both AirTerminal:SingleDuct:Uncontrolled and another kind of Air terminal unit which is not supported
           IF ( .NOT. DirectAirAndAirTerminalWarningIssued(ActualZoneNum)) THEN
             CALL ShowSevereError('In zone "' // TRIM(ZoneEquipConfig(ControlledZoneNum)%ZoneName) // &
                                  '" there are too many air terminals served by AirLoopHVAC systems.')
             CALL ShowContinueError('A single zone cannot have both an AirTerminal:SingleDuct:Uncontrolled ' &
                                  // 'and also a second AirTerminal:* object.')
             
             DirectAirAndAirTerminalWarningIssued(ActualZoneNum) = .TRUE.
             ErrorFlag = .TRUE.
           ENDIF
         ENDIF
       END DO ! zone loop

       AirLoopNum = ZoneEquipConfig(ControlledZoneNum)%AirLoopNum
       IF (AirLoopInit) THEN
         IF (AirLoopNum > 0) THEN
           IF ( .NOT. PrimaryAirSystem(AirLoopNum)%OASysExists) THEN
             IF (ZoneEquipConfig(ControlledZoneNum)%ZoneExh > 0.0d0 .AND. .NOT. ZoneEquipConfig(ControlledZoneNum)%FlowError .AND. &
                 AirLoopsSimOnce) THEN
               IF (.NOT. isPulseZoneSizing) THEN
                 CALL ShowWarningError('In zone ' // TRIM(ZoneEquipConfig(ControlledZoneNum)%ZoneName) // &
                                       ' there is unbalanced exhaust air flow.')
                 CALL ShowContinueErrorTimeStamp(' ')
                 CALL ShowContinueError('  Unless there is balancing infiltration / ventilation air flow, this will result in')
                 CALL ShowContinueError('  load due to induced outdoor air being neglected in the simulation.')
                 ZoneEquipConfig(ControlledZoneNum)%FlowError = .TRUE.
               END IF
             END IF
             ! ZoneEquipConfig(ControlledZoneNum)%ZoneExh = 0.0
           END IF
         ELSE
           IF (ZoneEquipConfig(ControlledZoneNum)%ZoneExh > 0.0d0 .AND. .NOT. ZoneEquipConfig(ControlledZoneNum)%FlowError .AND. &
               AirLoopsSimOnce) THEN
             IF (.NOT. isPulseZoneSizing) THEN
               CALL ShowWarningError('In zone ' // TRIM(ZoneEquipConfig(ControlledZoneNum)%ZoneName) // &
                                     ' there is unbalanced exhaust air flow.')
               CALL ShowContinueErrorTimeStamp(' ')
               CALL ShowContinueError('  Unless there is balancing infiltration / ventilation air flow, this will result in')
               CALL ShowContinueError('  load due to induced outdoor air being neglected in the simulation.')
               ZoneEquipConfig(ControlledZoneNum)%FlowError = .TRUE.
             END IF
           END IF
           ! ZoneEquipConfig(ControlledZoneNum)%ZoneExh = 0.0
         END IF
       END IF
     END DO  ! End of controlled zone loop
     CurZoneEqNum = 0

       !This is the call to the Supply Air Path after the components are simulated to update
       !  the path inlets

     ! Process supply air path components in reverse order
     DO SupplyAirPathNum = 1, NumSupplyAirPaths

       SupPathInletChanged = .FALSE.

       DO CompNum = SupplyAirPath(SupplyAirPathNum)%NumOfComponents, 1, -1
         SELECT CASE (SupplyAirPath(SupplyAirPathNum)%ComponentType_Num(CompNum))

           CASE (ZoneSplitter_Type)  ! 'AirLoopHVAC:ZoneSplitter'

             if (.NOT. (AirflowNetworkFanActivated .AND. SimulateAirflowNetwork > AirflowNetworkControlMultizone)) then
               CALL SimAirLoopSplitter(SupplyAirPath(SupplyAirPathNum)%ComponentName(CompNum), &
                               FirstHVACIteration, FirstCall, SupPathInletChanged,             &
                               CompIndex=SupplyAirPath(SupplyAirPathNum)%ComponentIndex(CompNum))
             endif

           CASE (ZoneSupplyPlenum_Type) ! 'AirLoopHVAC:SupplyPlenum'

             CALL SimAirZonePlenum(SupplyAirPath(SupplyAirPathNum)%ComponentName(CompNum),ZoneSupplyPlenum_Type, &
                               SupplyAirPath(SupplyAirPathNum)%ComponentIndex(CompNum),   &
                               FirstHVACIteration=FirstHVACIteration, FirstCall=FirstCall, &
                               PlenumInletChanged=SupPathInletChanged)

           CASE DEFAULT
             CALL ShowSevereError('Error found in Supply Air Path='//TRIM(SupplyAirPath(SupplyAirPathNum)%Name))
             CALL ShowContinueError('Invalid Supply Air Path Component='//  &
                  TRIM(SupplyAirPath(SupplyAirPathNum)%ComponentType(CompNum)))
             CALL ShowFatalError('Preceding condition causes termination.')

         END SELECT
       END DO

       IF (SupPathInletChanged) THEN
         ! If the supply air path inlet conditions have been changed, the Air Loop must be resimulated
         SimAir = .TRUE.
       END IF

     END DO ! end of the Supply Air Path DO Loop

     CALL CalcZoneMassBalance

     CALL CalcZoneLeavingConditions

     CALL SimReturnAirPath

     IF (MyOneTimeFlag) THEN
         DO ControlledZoneNum = 1, NumOfZones
           IF (.not. ZoneEquipConfig(ControlledZoneNum)%IsControlled) CYCLE
           IF (ZoneEquipConfig(ControlledZoneNum)%SupLeakToRetPlen .AND. &
               ZoneEquipConfig(ControlledZoneNum)%ReturnZonePlenumCondNum == 0) THEN
             CALL ShowSevereError('No return plenum for simple duct leakage model for Zone ' // &
                                  TRIM(ZoneEquipConfig(ControlledZoneNum)%ZoneName))
             CALL ShowContinueError('  The simple duct leakage model requires plenum return for all zone with leaks')
             ErrorFlag = .TRUE.
           END IF
         END DO
         IF (ErrorFlag) THEN
           CALL ShowFatalError('Preceding condition causes termination')
         END IF
       MyOneTimeFlag = .FALSE.
     END IF

  RETURN

END SUBROUTINE SimZoneEquipment

SUBROUTINE SetZoneEquipSimOrder(ControlledZoneNum, ActualZoneNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Russ Taylor
          !       DATE WRITTEN   May 1997
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Set simulation priorities based on user specified priorities and
          ! required conditions (heating or cooling).

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataZoneEnergyDemands, ONLY: ZoneSysEnergyDemand

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: ControlledZoneNum
  INTEGER, INTENT(IN) :: ActualZoneNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  CHARACTER(len=MaxNameLength) :: EquipTypeTemp
  CHARACTER(len=MaxNameLength) :: EquipNameTemp
  INTEGER             :: EquipTypeNum
  INTEGER             :: EquipPtrTemp
  INTEGER :: ComparedEquipTypeNum
  INTEGER :: TempNum
  INTEGER :: CurEqHeatingPriority  ! Used to make sure "optimization features" on compilers don't defeat purpose of this routine
  INTEGER :: CurEqCoolingPriority  ! Used to make sure "optimization features" on compilers don't defeat purpose of this routine
  INTEGER :: NumOfEquipTypes      ! For improved readability

     NumOfEquipTypes = ZoneEquipList(ControlledZoneNum)%NumOfEquipTypes
     PrioritySimOrder%EquipType=' '
     PrioritySimOrder%EquipType_Num=0
     PrioritySimOrder%EquipName=' '
     PrioritySimOrder%EquipPtr=0

     PrioritySimOrder(1:NumOfEquipTypes)%EquipType = ZoneEquipList(ControlledZoneNum)%EquipType(1:NumOfEquipTypes)
     PrioritySimOrder(1:NumOfEquipTypes)%EquipName = ZoneEquipList(ControlledZoneNum)%EquipName(1:NumOfEquipTypes)
     PrioritySimOrder(1:NumOfEquipTypes)%EquipType_Num = ZoneEquipList(ControlledZoneNum)%EquipType_Num(1:NumOfEquipTypes)
     PrioritySimOrder(1:NumOfEquipTypes)%CoolingPriority = ZoneEquipList(ControlledZoneNum)%CoolingPriority(1:NumOfEquipTypes)
     PrioritySimOrder(1:NumOfEquipTypes)%HeatingPriority = ZoneEquipList(ControlledZoneNum)%HeatingPriority(1:NumOfEquipTypes)

     PrioritySimOrder(1:NumOfEquipTypes)%EquipPtr = DefaultSimOrder(1:NumOfEquipTypes)

     DO EquipTypeNum = 1, NumOfEquipTypes

       CurEqHeatingPriority = PrioritySimOrder(EquipTypeNum)%HeatingPriority
       CurEqCoolingPriority = PrioritySimOrder(EquipTypeNum)%CoolingPriority

       DO ComparedEquipTypeNum = EquipTypeNum, NumOfEquipTypes

         IF (CurEqCoolingPriority > PrioritySimOrder(ComparedEquipTypeNum)%CoolingPriority .AND. &
             ZoneSysEnergyDemand(ActualZoneNum)%RemainingOutputRequired .LT. 0.0d0 &
                                          .OR. &
             CurEqHeatingPriority > PrioritySimOrder(ComparedEquipTypeNum)%HeatingPriority .AND. &
             ZoneSysEnergyDemand(ActualZoneNum)%RemainingOutputRequired .GE. 0.0d0) THEN

           EquipTypeTemp = PrioritySimOrder(EquipTypeNum)%EquipType
           PrioritySimOrder(EquipTypeNum)%EquipType = PrioritySimOrder(ComparedEquipTypeNum)%EquipType
           PrioritySimOrder(ComparedEquipTypeNum)%EquipType = EquipTypeTemp

           EquipNameTemp = PrioritySimOrder(EquipTypeNum)%EquipName
           PrioritySimOrder(EquipTypeNum)%EquipName = PrioritySimOrder(ComparedEquipTypeNum)%EquipName
           PrioritySimOrder(ComparedEquipTypeNum)%EquipName = EquipNameTemp

           EquipPtrTemp = PrioritySimOrder(EquipTypeNum)%EquipPtr
           PrioritySimOrder(EquipTypeNum)%EquipPtr = PrioritySimOrder(ComparedEquipTypeNum)%EquipPtr
           PrioritySimOrder(ComparedEquipTypeNum)%EquipPtr = EquipPtrTemp

           EquipPtrTemp = PrioritySimOrder(EquipTypeNum)%EquipType_Num
           PrioritySimOrder(EquipTypeNum)%EquipType_Num = PrioritySimOrder(ComparedEquipTypeNum)%EquipType_Num
           PrioritySimOrder(ComparedEquipTypeNum)%EquipType_Num = EquipPtrTemp

           TempNum = PrioritySimOrder(EquipTypeNum)%CoolingPriority
           PrioritySimOrder(EquipTypeNum)%CoolingPriority = PrioritySimOrder(ComparedEquipTypeNum)%CoolingPriority
           PrioritySimOrder(ComparedEquipTypeNum)%CoolingPriority = TempNum

           CurEqCoolingPriority = PrioritySimOrder(EquipTypeNum)%CoolingPriority

           TempNum = PrioritySimOrder(EquipTypeNum)%HeatingPriority
           PrioritySimOrder(EquipTypeNum)%HeatingPriority = PrioritySimOrder(ComparedEquipTypeNum)%HeatingPriority
           PrioritySimOrder(ComparedEquipTypeNum)%HeatingPriority = TempNum

           CurEqHeatingPriority = PrioritySimOrder(EquipTypeNum)%HeatingPriority

         END IF

       END DO

     END DO

  RETURN

END SUBROUTINE SetZoneEquipSimOrder

SUBROUTINE InitSystemOutputRequired(ZoneNum, SysOutputProvided, LatOutputProvided)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Russ Taylor
          !       DATE WRITTEN   May 1997
          !       MODIFIED       Don Shirey, Aug 2009 (latent/moisture additions)
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Initialize remaining output required variables

          ! METHODOLOGY EMPLOYED:
          ! Initialize remaining output variables using predictor calculations

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataZoneEnergyDemands, ONLY: ZoneSysEnergyDemand, DeadbandOrSetback, CurDeadbandOrSetback, &
                                   ZoneSysMoistureDemand

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)      :: ZoneNum
  REAL(r64), INTENT(INOUT) :: SysOutputProvided
  REAL(r64), INTENT(INOUT) :: LatOutputProvided

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          ! na

    ZoneSysEnergyDemand(ZoneNum)%RemainingOutputRequired = &
                          ZoneSysEnergyDemand(ZoneNum)%TotalOutputRequired
    ZoneSysEnergyDemand(ZoneNum)%RemainingOutputReqToHeatSP = &
                          ZoneSysEnergyDemand(ZoneNum)%OutputRequiredToHeatingSP
    ZoneSysEnergyDemand(ZoneNum)%RemainingOutputReqToCoolSP = &
                          ZoneSysEnergyDemand(ZoneNum)%OutputRequiredToCoolingSP
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

    ZoneSysMoistureDemand(ZoneNum)%RemainingOutputRequired = &
                          ZoneSysMoistureDemand(ZoneNum)%TotalOutputRequired
    ZoneSysMoistureDemand(ZoneNum)%RemainingOutputReqToHumidSP = &
                          ZoneSysMoistureDemand(ZoneNum)%OutputRequiredToHumidifyingSP
    ZoneSysMoistureDemand(ZoneNum)%RemainingOutputReqToDehumidSP = &
                          ZoneSysMoistureDemand(ZoneNum)%OutputRequiredToDehumidifyingSP
    !init each sequenced demand to the full output
    IF (ALLOCATED(ZoneSysMoistureDemand(ZoneNum)%SequencedOutputRequired)) &
      ZoneSysMoistureDemand(ZoneNum)%SequencedOutputRequired = & ! array assignment
                          ZoneSysMoistureDemand(ZoneNum)%TotalOutputRequired
    IF (ALLOCATED(ZoneSysMoistureDemand(ZoneNum)%SequencedOutputRequiredToHumidSP)) &
      ZoneSysMoistureDemand(ZoneNum)%SequencedOutputRequiredToHumidSP = & ! array assignment
                          ZoneSysMoistureDemand(ZoneNum)%OutputRequiredToHumidifyingSP
    IF (ALLOCATED(ZoneSysMoistureDemand(ZoneNum)%SequencedOutputRequiredToDehumidSP)) &
      ZoneSysMoistureDemand(ZoneNum)%SequencedOutputRequiredToDehumidSP = & ! array assignment
                          ZoneSysMoistureDemand(ZoneNum)%OutputRequiredToDehumidifyingSP

    SysOutputProvided = 0.0d0 ! sensible provided by a piece of zone equipment
    LatOutputProvided = 0.0d0 ! latent provided by a piece of zone equipment

    CurDeadbandOrSetback(ZoneNum) = DeadbandOrSetback(ZoneNum)

  RETURN

END SUBROUTINE InitSystemOutputRequired

SUBROUTINE UpdateSystemOutputRequired(ZoneNum, SysOutputProvided, LatOutputProvided, EquipPriorityNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Russ Taylor
          !       DATE WRITTEN   Unknown
          !       MODIFIED       B. Griffith Sept 2011, add storage of requirements by sequence
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine needs a description.

          ! METHODOLOGY EMPLOYED:
          ! Needs description, as appropriate.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataZoneEnergyDemands, ONLY: ZoneSysEnergyDemand, DeadbandOrSetback, CurDeadbandOrSetback, &
                                   ZoneSysMoistureDemand
  USE DataHVACGlobals, ONLY: SingleHeatingSetPoint, SingleCoolingSetPoint, SingleHeatCoolSetPoint, &
                             DualSetPointWithDeadBand
  USE DataHeatBalFanSys, ONLY: TempControlType

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: ZoneNum
  REAL(r64), INTENT(IN) :: SysOutputProvided ! sensible output provided by zone equipment (W)
  REAL(r64), INTENT(IN) :: LatOutputProvided ! latent output provided by zone equipment (kg/s)
  INTEGER , INTENT(IN), OPTIONAL  :: EquipPriorityNum  ! index in PrioritySimOrder for this update

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          ! na

           ! Determine flow rate and temperature of supply air based on type of damper

! Sensible output updates
    ZoneSysEnergyDemand(ZoneNum)%RemainingOutputRequired = ZoneSysEnergyDemand(ZoneNum)%RemainingOutputRequired &
                                                                  - SysOutputProvided
    ZoneSysEnergyDemand(ZoneNum)%RemainingOutputReqToHeatSP = ZoneSysEnergyDemand(ZoneNum)%RemainingOutputReqToHeatSP &
                                                                  - SysOutputProvided
    ZoneSysEnergyDemand(ZoneNum)%RemainingOutputReqToCoolSP = ZoneSysEnergyDemand(ZoneNum)%RemainingOutputReqToCoolSP &
                                                                  - SysOutputProvided
! Latent output updates
    ZoneSysMoistureDemand(ZoneNum)%RemainingOutputRequired = &
                          ZoneSysMoistureDemand(ZoneNum)%RemainingOutputRequired - LatOutputProvided
    ZoneSysMoistureDemand(ZoneNum)%RemainingOutputReqToHumidSP = &
                          ZoneSysMoistureDemand(ZoneNum)%RemainingOutputReqToHumidSP - LatOutputProvided
    ZoneSysMoistureDemand(ZoneNum)%RemainingOutputReqToDehumidSP = &
                          ZoneSysMoistureDemand(ZoneNum)%RemainingOutputReqToDehumidSP - LatOutputProvided

! re-evaluate if loads are now such that in dead band or set back
    SELECT CASE (TempControlType(ZoneNum))
      CASE (0) ! uncontrolled zone; shouldn't ever get here, but who knows
        CurDeadbandOrSetback(ZoneNum) = .FALSE.
      CASE (SingleHeatingSetPoint)
        IF ((ZoneSysEnergyDemand(ZoneNum)%RemainingOutputRequired - 1.0d0) .LT. 0.0d0) THEN
          CurDeadBandOrSetback(ZoneNum) = .TRUE.
        ELSE
          CurDeadBandOrSetback(ZoneNum) = .FALSE.
        END IF
      CASE (SingleCoolingSetPoint)
        IF ((ZoneSysEnergyDemand(ZoneNum)%RemainingOutputRequired + 1.0d0) .GT. 0.0d0) THEN
          CurDeadBandOrSetback(ZoneNum) = .TRUE.
        ELSE
          CurDeadBandOrSetback(ZoneNum) = .FALSE.
        END IF
      CASE (SingleHeatCoolSetPoint)
        IF (ZoneSysEnergyDemand(ZoneNum)%RemainingOutputReqToHeatSP .LT. 0.0d0 .AND. &
            ZoneSysEnergyDemand(ZoneNum)%RemainingOutputReqToCoolSP .GT. 0.0d0) THEN
          CurDeadBandOrSetback(ZoneNum) = .TRUE.
        ELSE
          CurDeadBandOrSetback(ZoneNum) = .FALSE.
        END IF
      CASE (DualSetPointWithDeadBand)
        IF (ZoneSysEnergyDemand(ZoneNum)%RemainingOutputReqToHeatSP .LT. 0.0d0 .AND. &
            ZoneSysEnergyDemand(ZoneNum)%RemainingOutputReqToCoolSP .GT. 0.0d0) THEN
          CurDeadBandOrSetback(ZoneNum) = .TRUE.
        ELSE
          CurDeadBandOrSetback(ZoneNum) = .FALSE.
        END IF
    END SELECT

    IF (PRESENT(EquipPriorityNum)) THEN
      !now store remaining load at the by sequence level
      IF (EquipPriorityNum +1 <= ZoneSysEnergyDemand(ZoneNum)%NumZoneEquipment) THEN
        ZoneSysEnergyDemand(ZoneNum)%SequencedOutputRequired(EquipPriorityNum +1) = &
            ZoneSysEnergyDemand(ZoneNum)%RemainingOutputRequired
        ZoneSysMoistureDemand(ZoneNum)%SequencedOutputRequired(EquipPriorityNum +1) = &
            ZoneSysMoistureDemand(ZoneNum)%RemainingOutputRequired
      ENDIF

      IF (PrioritySimOrder(EquipPriorityNum)%HeatingPriority +1 <= ZoneSysEnergyDemand(ZoneNum)%NumZoneEquipment) THEN
        ZoneSysEnergyDemand(ZoneNum)%SequencedOutputRequiredToHeatingSP(PrioritySimOrder(EquipPriorityNum)%HeatingPriority +1) = &
            ZoneSysEnergyDemand(ZoneNum)%RemainingOutputReqToHeatSP
        ZoneSysMoistureDemand(ZoneNum)%SequencedOutputRequiredToHumidSP(PrioritySimOrder(EquipPriorityNum)%HeatingPriority +1) = &
            ZoneSysMoistureDemand(ZoneNum)%RemainingOutputReqToHumidSP
      ENDIF
      IF (PrioritySimOrder(EquipPriorityNum)%CoolingPriority + 1 <= ZoneSysEnergyDemand(ZoneNum)%NumZoneEquipment) THEN
        ZoneSysEnergyDemand(ZoneNum)%SequencedOutputRequiredToCoolingSP(PrioritySimOrder(EquipPriorityNum)%CoolingPriority + 1) = &
            ZoneSysEnergyDemand(ZoneNum)%RemainingOutputReqToCoolSP
        ZoneSysMoistureDemand(ZoneNum)%SequencedOutputRequiredToDehumidSP(PrioritySimOrder(EquipPriorityNum)%CoolingPriority +1) = &
            ZoneSysMoistureDemand(ZoneNum)%RemainingOutputReqToDehumidSP
      ENDIF
    ENDIF

  RETURN

END SUBROUTINE UpdateSystemOutputRequired

SUBROUTINE CalcZoneMassBalance

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Russ Taylor
          !       DATE WRITTEN   May 1997
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Perform zone mass balance to get outlet air flow conditions.

          ! METHODOLOGY EMPLOYED:
          ! Mass continuity equation.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataLoopNode, ONLY : Node
  USE DataAirLoop, ONLY : AirLoopFlow
  USE DataRoomAirModel   ! UCSD
  USE DataHVACGlobals, ONLY : NumPrimaryAirSys, AirLoopsSimOnce
  USE DataAirSystems, ONLY : PrimaryAirSystem
  USE DataAirflowNetwork, ONLY : AirflowNetworkNumOfExhFan
  USE DataGlobals, ONLY: isPulseZoneSizing

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
  INTEGER :: ZoneNum
  INTEGER :: NodeNum
  INTEGER :: RetNode  ! return air node number
  INTEGER :: ZoneNode ! zone air node number
  INTEGER :: AirLoopNum
  REAL(r64) :: TotInletAirMassFlowRate
  REAL(r64) :: TotInletAirMassFlowRateMax
  REAL(r64) :: TotInletAirMassFlowRateMaxAvail
  REAL(r64) :: TotInletAirMassFlowRateMin
  REAL(r64) :: TotInletAirMassFlowRateMinAvail
  REAL(r64) :: TotExhaustAirMassFlowRate
  REAL(r64) :: TotSupplyAirMassFlowRate

   DO ZoneNum = 1, NumOfZones

       IF (.not. ZoneEquipConfig(ZoneNum)%IsControlled) CYCLE

       TotInletAirMassFlowRate = 0.d0
       TotInletAirMassFlowRateMax = 0.d0
       TotInletAirMassFlowRateMaxAvail = 0.d0
       TotInletAirMassFlowRateMin = 0.d0
       TotInletAirMassFlowRateMinAvail = 0.d0
       TotExhaustAirMassFlowRate = 0.d0

       DO NodeNum = 1, ZoneEquipConfig(ZoneNum)%NumInletNodes
         TotInletAirMassFlowRate = TotInletAirMassFlowRate + Node(ZoneEquipConfig(ZoneNum)%InletNode(NodeNum))%MassFlowRate
         TotInletAirMassFlowRateMax = TotInletAirMassFlowRateMax + &
                        Node(ZoneEquipConfig(ZoneNum)%InletNode(NodeNum))%MassFlowRateMax
         TotInletAirMassFlowRateMaxAvail = TotInletAirMassFlowRateMaxAvail + &
                        Node(ZoneEquipConfig(ZoneNum)%InletNode(NodeNum))%MassFlowRateMaxAvail
         TotInletAirMassFlowRateMin = TotInletAirMassFlowRateMin + &
                             Node(ZoneEquipConfig(ZoneNum)%InletNode(NodeNum))%MassFlowRateMin
         TotInletAirMassFlowRateMinAvail = TotInletAirMassFlowRateMinAvail + &
                        Node(ZoneEquipConfig(ZoneNum)%InletNode(NodeNum))%MassFlowRateMinAvail
       END DO

       DO NodeNum = 1, ZoneEquipConfig(ZoneNum)%NumExhaustNodes

         If (AirflowNetworkNumOfExhFan .EQ. 0) &
           TotExhaustAirMassFlowRate = TotExhaustAirMassFlowRate  &
                + Node(ZoneEquipConfig(ZoneNum)%ExhaustNode(NodeNum))%MassFlowRate

       END DO

       AirLoopNum = ZoneEquipConfig(ZoneNum)%AirLoopNum
       ZoneNode = ZoneEquipConfig(ZoneNum)%ZoneNode
       Node(ZoneNode)%MassFlowRate = TotInletAirMassFlowRate
       Node(ZoneNode)%MassFlowRateMax = TotInletAirMassFlowRateMax
       Node(ZoneNode)%MassFlowRateMaxAvail = TotInletAirMassFlowRateMaxAvail
       Node(ZoneNode)%MassFlowRateMin = TotInletAirMassFlowRateMin
       Node(ZoneNode)%MassFlowRateMinAvail = TotInletAirMassFlowRateMinAvail

       ! Update Return Air Node Conditions; If one Exists
       RetNode = ZoneEquipConfig(ZoneNum)%ReturnAirNode
       If(RetNode > 0) Then
          Node(RetNode)%MassFlowRate = &
            MAX(Node(ZoneNode)%MassFlowRate - (TotExhaustAirMassFlowRate - ZoneEquipConfig(ZoneNum)%ZoneExhBalanced), 0.0d0)
          IF (AirLoopNum > 0) THEN
            IF ( .NOT. PrimaryAirSystem(AirLoopNum)%OASysExists) THEN
              Node(RetNode)%MassFlowRate = &
                MAX(Node(ZoneNode)%MassFlowRate - (TotExhaustAirMassFlowRate - ZoneEquipConfig(ZoneNum)%ZoneExh), 0.0d0)
            END IF
          END IF
          Node(RetNode)%MassFlowRateMax = Node(ZoneNode)%MassFlowRateMax
          Node(RetNode)%MassFlowRateMin = Node(ZoneNode)%MassFlowRateMin
          Node(RetNode)%MassFlowRateMaxAvail = Node(ZoneNode)%MassFlowRateMaxAvail
          Node(RetNode)%MassFlowRateMinAvail = 0.0d0
       End If

       TotSupplyAirMassFlowRate = TotInletAirMassFlowRate - (TotExhaustAirMassFlowRate - ZoneEquipConfig(ZoneNum)%ZoneExh) &
                                  - ZoneEquipConfig(ZoneNum)%PlenumMassFlow


       IF (AirLoopNum > 0) THEN
         AirLoopFlow(AirLoopNum)%ZoneExhaust = AirLoopFlow(AirLoopNum)%ZoneExhaust + &
                                               ZoneEquipConfig(ZoneNum)%ZoneExh
         AirLoopFlow(AirLoopNum)%ZoneExhaustBalanced =  AirLoopFlow(AirLoopNum)%ZoneExhaustBalanced &
                                                        + ZoneEquipConfig(ZoneNum)%ZoneExhBalanced
         AirLoopFlow(AirLoopNum)%SupFlow = AirLoopFlow(AirLoopNum)%SupFlow + TotSupplyAirMassFlowRate
         AirLoopFlow(AirLoopNum)%RetFlow0 = AirLoopFlow(AirLoopNum)%RetFlow0 + Node(RetNode)%MassFlowRate
         AirLoopFlow(AirLoopNum)%RecircFlow = AirLoopFlow(AirLoopNum)%RecircFlow + ZoneEquipConfig(ZoneNum)%PlenumMassFlow
       END IF
     END DO
     ! Calculate an air loop return air flow rate
     DO AirLoopNum=1,NumPrimaryAirSys
       IF ( (AirLoopFlow(AirLoopNum)%ZoneExhaust > &
            (AirLoopFlow(AirLoopNum)%SupFlow + AirLoopFlow(AirLoopNum)%ZoneExhaustBalanced) .OR. &
           AirLoopFlow(AirLoopNum)%ZoneExhaust >   &
            (AirLoopFlow(AirLoopNum)%MaxOutAir + AirLoopFlow(AirLoopNum)%ZoneExhaustBalanced)) .AND. &
           .NOT. AirLoopFlow(AirLoopNum)%FlowError .AND. AirLoopsSimOnce) THEN
         IF (.NOT. isPulseZoneSizing) THEN
           CALL ShowWarningError('In AirLoopHVAC ' // TRIM(PrimaryAirSystem(AirLoopNum)%Name) // &
                                     ' there is unbalanced exhaust air flow.')
           CALL ShowContinueErrorTimeStamp(' ')
           CALL ShowContinueError('  Unless there is balancing infiltration / ventilation air flow, this will result in')
           CALL ShowContinueError('  load due to induced outdoor air being neglected in the simulation.')
           AirLoopFlow(AirLoopNum)%FlowError = .TRUE.
         END IF
       END IF
       AirLoopFlow(AirLoopNum)%ZoneExhaust = MIN(AirLoopFlow(AirLoopNum)%ZoneExhaust, &
                                                 (AirLoopFlow(AirLoopNum)%SupFlow + AirLoopFlow(AirLoopNum)%ZoneExhaustBalanced))
       AirLoopFlow(AirLoopNum)%ZoneExhaust = MIN(AirLoopFlow(AirLoopNum)%ZoneExhaust, &
                                                 (AirLoopFlow(AirLoopNum)%MaxOutAir + AirLoopFlow(AirLoopNum)%ZoneExhaustBalanced))
       AirLoopFlow(AirLoopNum)%RetFlow = AirLoopFlow(AirLoopNum)%SupFlow  &
                                           - (AirLoopFlow(AirLoopNum)%ZoneExhaust - AirLoopFlow(AirLoopNum)%ZoneExhaustBalanced) &
                                           + AirLoopFlow(AirLoopNum)%RecircFlow
     END DO
     ! adjust the zone return air flow rates to match the air loop return air flow rate
     DO ZoneNum = 1, NumOfZones
       IF (.not. ZoneEquipConfig(ZoneNum)%IsControlled) CYCLE
       RetNode = ZoneEquipConfig(ZoneNum)%ReturnAirNode
       AirLoopNum = ZoneEquipConfig(ZoneNum)%AirLoopNum
       IF (AirLoopNum > 0 .AND. RetNode > 0) THEN
         IF (PrimaryAirSystem(AirLoopNum)%OASysExists) THEN
           IF (AirLoopFlow(AirLoopNum)%RetFlow0 > 0.0d0) THEN
             Node(RetNode)%MassFlowRate = Node(RetNode)%MassFlowRate * &
               (AirLoopFlow(AirLoopNum)%RetFlow/AirLoopFlow(AirLoopNum)%RetFlow0)
           ELSE
             Node(RetNode)%MassFlowRate = 0.0d0
           END IF
         END IF
       END IF
!       IF (AirLoopNum == 0 .AND. RetNode > 0) THEN
!         ! sometimes models for ZoneHVAC have input a return node, but no air loop HVAC.
!         ! this block was tried but caused problems such as UA coil sizing issues and water coil controller problems
!         !  CR 7967, no air loop HVAC, but there is a return air node that never gets used or set
!         Node(RetNode)%MassFlowRate = 0.d0
!       ENDIF
     END DO

   RETURN

 END SUBROUTINE CalcZoneMassBalance

 SUBROUTINE CalcZoneLeavingConditions

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Liesen
          !       DATE WRITTEN   January 2001
          !       MODIFIED       June 2003, FCW: add heat from airflow window to return air
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Perform zone upate of the leaving conditions.

          ! METHODOLOGY EMPLOYED:
          ! Energy Balance.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataLoopNode, ONLY : Node
  USE DataHeatBalance, ONLY: ZoneIntGain, RefrigCaseCredit, Zone
  USE DataHeatBalFanSys, ONLY: SysDepZoneLoads, ZoneLatentGain
  USE DataSurfaces, ONLY: Surface, SurfaceWindow, AirFlowWindow_Destination_ReturnAir
  USE DataEnvironment, ONLY: OutBaroPress
  USE DataRoomAirModel,     ONLY: AirPatternZoneInfo
  USE DataZoneEnergyDemands, ONLY: ZoneSysEnergyDemand, DeadbandOrSetback, CurDeadbandOrSetback, ZoneSysMoistureDemand
  USE DataContaminantBalance, ONLY: Contaminant
  USE InternalHeatGains     , ONLY: SumAllReturnAirConvectionGains, SumAllReturnAirLatentGains
  USE DataHVACGlobals, ONLY: RetTempMax, RetTempMin

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
  REAL(r64)      :: qretair          ! Heat to return air from lights
  REAL(r64)      :: cpair            ! Air heat capacity [J/kg-K]
  REAL(r64)      :: TempRetAir       ! Return air temperature [C]
  REAL(r64)      :: TempZoneAir      ! Zone air temperature [C]
  INTEGER        :: ZoneNum          ! Controlled zone number
  INTEGER        :: ActualZoneNum    ! Zone number
  INTEGER        :: ZoneNode         ! Node number of controlled zone
  INTEGER        :: ReturnNode       ! Node number of controlled zone's return air
  INTEGER        :: SurfNum          ! Surface number
  REAL(r64)      :: MassFlowRA       ! Return air mass flow [kg/s]
  REAL(r64)      :: FlowThisTS       ! Window gap air mass flow [kg/s]
  REAL(r64)      :: WinGapFlowtoRA   ! Mass flow to return air from all airflow windows in zone [kg/s]
  REAL(r64)      :: WinGapFlowTtoRA  ! Sum of mass flow times outlet temp for all airflow windows in zone [(kg/s)-C]
  REAL(r64)      :: WinGapTtoRA      ! Temp of outlet flow mixture to return air from all airflow windows in zone [C]
  REAL(r64)      :: H2OHtOfVap       ! Heat of vaporization of water (W/kg)
  REAL(r64)      :: RhoAir           ! Density of air (Kg/m3)
  REAL(r64)      :: ZoneMult         ! zone multiplier
  REAL(r64)      :: SumRetAirLatentGainRate

   DO ZoneNum = 1, NumOfZones
    IF (.not. ZoneEquipConfig(ZoneNum)%IsControlled) CYCLE
    ActualZoneNum=ZoneEquipConfig(ZoneNum)%ActualZoneNum
     !A return air system may not exist for certain systems; Therefore when no return node exits
     ! there is no update.  OF course if there is no return air system then you cannot update
     ! the energy for the return air heat gain from the lights statements.
    ReturnNode = ZoneEquipConfig(ZoneNum)%ReturnAirNode
    ZoneNode = ZoneEquipConfig(ZoneNum)%ZoneNode
    ZoneMult = Zone(ActualZoneNum)%Multiplier * Zone(ActualZoneNum)%ListMultiplier
    IF(ReturnNode > 0) Then

     !RETURN AIR HEAT GAIN from the Lights statement; this heat gain is stored in
     ! Add sensible heat gain from refrigerated cases with under case returns
     CALL SumAllReturnAirConvectionGains(ActualZoneNum, QRetAir)


     CpAir   = PsyCpAirFnWTdb(Node(ZoneNode)%HumRat, Node(ZoneNode)%Temp)

     ! Need to add the energy to the return air from lights and from airflow windows. Where the heat
     ! is added depends on if there is system flow or not.  If there is system flow the heat is added
     ! to the Zone Return Node.  If there is no system flow then the heat is added back to the zone in the
     ! Correct step through the SysDepZoneLoads variable.

     MassFlowRA  = Node(ReturnNode)%MassFlowRate / ZoneMult

     ! user defined room air model may feed temp that differs from zone node
     IF (ALLOCATED(AirPatternZoneInfo)) THEN !
       IF ((AirPatternZoneInfo(ActualZoneNum)%IsUsed) .and. (.not. BeginEnvrnFlag)) THEN
         TempZoneAir =  AirPatternZoneInfo(ActualZoneNum)%Tleaving
         TempRetAir  =  TempZoneAir
       ELSE
         TempZoneAir = Node(ZoneNode)%Temp
         TempRetAir  = TempZoneAir
       ENDIF
     ELSE
       TempZoneAir = Node(ZoneNode)%Temp
       TempRetAir  = TempZoneAir
     ENDIF

     WinGapFlowtoRA = 0.0d0
     WinGapTtoRA = 0.0d0
     WinGapFlowTtoRA = 0.0d0

     DO SurfNum = Zone(ActualZoneNum)%SurfaceFirst,Zone(ActualZoneNum)%SurfaceLast
       IF(SurfaceWindow(SurfNum)%AirFlowThisTS > 0.0d0 .AND.   &
          SurfaceWindow(SurfNum)%AirflowDestination == AirFlowWindow_Destination_ReturnAir) THEN
         FlowThisTS = PsyRhoAirFnPbTdbW(OutBaroPress,SurfaceWindow(SurfNum)%TAirFlowGapOutlet, Node(ZoneNode)%HumRat) * &
           SurfaceWindow(SurfNum)%AirFlowThisTS * Surface(SurfNum)%Width
         WinGapFlowtoRA = WinGapFlowtoRA + FlowThisTS
         WinGapFlowTtoRA = WinGapFlowTtoRA + FlowThisTS * SurfaceWindow(SurfNum)%TAirFlowGapOutlet
       END IF
     END DO
     IF(WinGapFlowtoRA > 0.0d0) WinGapTtoRA = WinGapFlowTtoRA / WinGapFlowtoRA
     ! the flag NoHeatToReturnAir is TRUE if the system is zonal only or is central with on/off air flow. In these
     ! cases the heat to return air is treated as a zone heat gain and dealt with in CalcZoneSums in
     ! MODULE ZoneTempPredictorCorrector.
     IF (.NOT. Zone(ActualZoneNum)%NoHeatToReturnAir) THEN
       IF(MassFlowRA > 0.0d0) Then
         IF(WinGapFlowtoRA > 0.0d0) THEN
           ! Add heat-to-return from window gap airflow
           IF(MassFlowRA >= WinGapFlowtoRA) THEN
             TempRetAir = (WinGapFlowTtoRA + (MassFlowRA-WinGapFlowtoRA)*TempZoneAir)/MassFlowRA
           ELSE
             ! All of return air comes from flow through airflow windows
             TempRetAir = WinGapTtoRA
             ! Put heat from window airflow that exceeds return air flow into zone air
             SysDepZoneLoads(ActualZoneNum) = SysDepZoneLoads(ActualZoneNum) + &
               (WinGapFlowToRA - MassFlowRA) * CpAir*(WinGapTtoRA - TempZoneAir)
           END IF
         END IF
         ! Add heat-to-return from lights
         TempRetAir = TempRetAir + QRetAir/(MassFlowRA * CpAir)
         IF (TempRetAir > RetTempMax) THEN
           Node(ReturnNode)%Temp = RetTempMax
           IF (.not. ZoneSizingCalc) THEN
             SysDepZoneLoads(ActualZoneNum) = SysDepZoneLoads(ActualZoneNum) + CpAir*MassFlowRA*(TempRetAir-RetTempMax)
           END IF
         ELSE IF (TempRetAir < RetTempMin) THEN
           Node(ReturnNode)%Temp = RetTempMin
           IF (.not. ZoneSizingCalc) THEN
             SysDepZoneLoads(ActualZoneNum) = SysDepZoneLoads(ActualZoneNum) + CpAir*MassFlowRA*(TempRetAir-RetTempMin)
           END IF
         ELSE
           Node(ReturnNode)%Temp = TempRetAir
         END IF
       ELSE  ! No return air flow
         ! Assign all heat-to-return from window gap airflow to zone air
         IF(WinGapFlowToRA > 0.0d0) &
           SysDepZoneLoads(ActualZoneNum) = SysDepZoneLoads(ActualZoneNum) + &
             WinGapFlowToRA * CpAir * (WinGapTtoRA - TempZoneAir)
         ! Assign all heat-to-return from lights to zone air
         IF(QRetAir > 0.0d0) &
           SysDepZoneLoads(ActualZoneNum) = SysDepZoneLoads(ActualZoneNum) + QRetAir
         Node(ReturnNode)%Temp = Node(ZoneNode)%Temp
       END IF
     ELSE
       ! update the return air node for zonal and central on/off systems
       Node(ReturnNode)%Temp = Node(ZoneNode)%Temp
     END IF

     ! Update the rest of the Return Air Node conditions, if the return air system exists!
     Node(ReturnNode)%Press = Node(ZoneNode)%Press

     H2OHtOfVap = PsyHgAirFnWTdb(Node(ZoneNode)%HumRat,Node(ReturnNode)%Temp)
     RhoAir = PsyRhoAirFnPbTdbW(OutBaroPress,Node(ReturnNode)%Temp,Node(ZoneNode)%HumRat)

     ! Include impact of under case returns for refrigerated display case when updating the return air node humidity
     IF (.NOT. Zone(ActualZoneNum)%NoHeatToReturnAir) THEN
       IF (MassFlowRA > 0) THEN
         CALL SumAllReturnAirLatentGains(ZoneNum, SumRetAirLatentGainRate)
         Node(ReturnNode)%HumRat = Node(ZoneNode)%HumRat + (SumRetAirLatentGainRate / &
                                 (H2OHtOfVap * MassFlowRA))
       ELSE
       ! If no mass flow rate exists, include the latent HVAC case credit with the latent Zone case credit
         Node(ReturnNode)%HumRat = Node(ZoneNode)%HumRat
         RefrigCaseCredit(ActualZoneNum)%LatCaseCreditToZone = RefrigCaseCredit(ActualZoneNum)%LatCaseCreditToZone + &
                                                                 RefrigCaseCredit(ActualZoneNum)%LatCaseCreditToHVAC
         ! shouldn't the HVAC term be zeroed out then?
         CALL SumAllReturnAirLatentGains(ZoneNum, SumRetAirLatentGainRate)
         ZoneLatentGain(ActualZoneNum) = ZoneLatentGain(ActualZoneNum) + SumRetAirLatentGainRate

       END IF
     ELSE
       Node(ReturnNode)%HumRat = Node(ZoneNode)%HumRat
       RefrigCaseCredit(ActualZoneNum)%LatCaseCreditToZone = RefrigCaseCredit(ActualZoneNum)%LatCaseCreditToZone + &
                                                               RefrigCaseCredit(ActualZoneNum)%LatCaseCreditToHVAC
       ! shouldn't the HVAC term be zeroed out then?
       CALL SumAllReturnAirLatentGains(ZoneNum, SumRetAirLatentGainRate)
       ZoneLatentGain(ActualZoneNum) = ZoneLatentGain(ActualZoneNum) + SumRetAirLatentGainRate
     END IF

     Node(ReturnNode)%Enthalpy = PsyHFnTdbW(Node(ReturnNode)%Temp,Node(ReturnNode)%HumRat)

     IF (Contaminant%CO2Simulation) Node(ReturnNode)%CO2 = Node(ZoneNode)%CO2
     IF (Contaminant%GenericContamSimulation) Node(ReturnNode)%GenContam = Node(ZoneNode)%GenContam

    End If !End of check for a return air node, which implies a return air system.

    ! Reset current deadband flags, remaining output required, so no impact beyond zone equipment
    ZoneSysEnergyDemand(ActualZoneNum)%RemainingOutputRequired = &
                          ZoneSysEnergyDemand(ActualZoneNum)%TotalOutputRequired
    ZoneSysEnergyDemand(ActualZoneNum)%RemainingOutputReqToHeatSP = &
                          ZoneSysEnergyDemand(ActualZoneNum)%OutputRequiredToHeatingSP
    ZoneSysEnergyDemand(ActualZoneNum)%RemainingOutputReqToCoolSP = &
                          ZoneSysEnergyDemand(ActualZoneNum)%OutputRequiredToCoolingSP

    ZoneSysMoistureDemand(ActualZoneNum)%RemainingOutputRequired = &
                          ZoneSysMoistureDemand(ActualZoneNum)%TotalOutputRequired
    ZoneSysMoistureDemand(ActualZoneNum)%RemainingOutputReqToHumidSP = &
                          ZoneSysMoistureDemand(ActualZoneNum)%OutputRequiredToHumidifyingSP
    ZoneSysMoistureDemand(ActualZoneNum)%RemainingOutputReqToDehumidSP = &
                          ZoneSysMoistureDemand(ActualZoneNum)%OutputRequiredToDehumidifyingSP

    CurDeadbandOrSetback(ActualZoneNum) = DeadbandOrSetback(ActualZoneNum)

   END DO

   RETURN

 END SUBROUTINE CalcZoneLeavingConditions

 SUBROUTINE UpdateZoneEquipment(SimAir)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Russ Taylor
          !       DATE WRITTEN   Nov 1997
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine performs the update for Zone Equipment Management.
          ! Specifically, it transfers the conditions from the zone equipment return air nodes across
          ! to the air loop side, allowing for multiple return air nodes

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE HVACInterfaceManager, ONLY : UpdateHVACInterface
  USE DataAirLoop,         ONLY : AirToZoneNodeInfo
  USE DataHVACGlobals,      ONLY : NumPrimaryAirSys
  USE DataConvergParams,    ONLY : CalledFromAirSystemDemandSide

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  LOGICAL, INTENT(INOUT) :: SimAir

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: ZoneGroupNum
  INTEGER :: RetAirPathNum

             ! Transfer the conditions from the zone equipment return air nodes across
             ! to the air loop side, allowing for multiple return air nodes
     DO ZoneGroupNum = 1, NumPrimaryAirSys

       DO RetAirPathNum = 1, AirToZoneNodeInfo(ZoneGroupNum)%NumReturnNodes

         CALL UpdateHVACInterface(ZoneGroupNum, CalledFromAirSystemDemandSide,     &
            AirToZoneNodeInfo(ZoneGroupNum)%ZoneEquipReturnNodeNum(RetAirPathNum), &
            AirToZoneNodeInfo(ZoneGroupNum)%AirLoopReturnNodeNum(RetAirPathNum),   &
            SimAir)

       END DO

     END DO

  RETURN

END SUBROUTINE UpdateZoneEquipment

SUBROUTINE ReportZoneEquipment
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Russ Taylor
          !       DATE WRITTEN   <date_written>
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is left for Module format consistency -- not needed in this module.

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
          ! na

  RETURN
END SUBROUTINE ReportZoneEquipment

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

END MODULE ZoneEquipmentManager
