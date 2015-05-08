MODULE HVACMultiSpeedHeatPump

  ! Module containing the Multi Speed Heat Pump simulation routines

  ! MODULE INFORMATION:
  !       AUTHOR         Lixing Gu, Florida Solar Energy Center
  !       DATE WRITTEN   June 2007
  !       MODIFIED       Bereket Nigusse, FSEC, June 2010 - deprecated supply air flow fraction through controlled
  !                      zone from the furnace object input field. Now, the flow fraction is calculated internally
  !                      Brent Griffith, NREL, Dec 2010 -- upgrade to new plant for heat recovery, general fluid props.
  !                      Bereket Nigusse, FSEC, Jan. 2012 -- added hot water and steam heating coil

  !       RE-ENGINEERED  na

  ! PURPOSE OF THIS MODULE:
  ! To encapsulate the data and algorithms required to simulate Multi Speed Heat Pump in
  ! EnergyPlus.

  ! Module currently models air-cooled or evap-cooled direct expansion systems
  ! (split or packaged) with mulptiple speeds. Air-side performance is modeled to determine
  ! coil discharge air conditions. The module also determines the DX unit's energy
  ! usage. Neither the air-side performance nor the energy usage includes the effect
  ! of supply air fan heat/energy usage. The supply air fan is modeled by other modules.

  ! METHODOLOGY EMPLOYED:
  ! na

  ! REFERENCES:
  ! na

  ! OTHER NOTES:
  ! na

  ! USE STATEMENTS:
  ! Use statements for data only modules
USE DataPrecisionGlobals
USE DataLoopNode
USE DataGlobals
Use DataEnvironment, ONLY: StdBaroPress, StdRhoAir, OutBaroPress, OutDryBulbTemp, OutWetBulbTemp, OutHumRat, CurMnDy,   &
                           EnvironmentName,DayOfYear
USE DataHVACGlobals, ONLY: OnOffFanPartLoadFraction, SmallAirVolFLow, SmallMassFlow, SmallLoad, &
                           DXElecCoolingPower, DXElecHeatingPower, FanElecPower, ElecHeatingCoilPower, &
                           CycFanCycCoil, ContFanCycCoil, DrawThru, BlowThru, Coil_HeatingWater, Coil_HeatingSteam, &
                           Coil_HeatingGas, Coil_HeatingElectric, Coil_HeatingGas_MultiStage, Coil_HeatingElectric_MultiStage
USE DataZoneEnergyDemands, ONLY: ZoneSysEnergyDemand
USE DataInterfaces
USE Psychrometrics

  ! Use statements for access to subroutines in other modules
USE ScheduleManager

IMPLICIT NONE         ! Enforce explicit typing of all variables

PRIVATE ! Everything private unless explicitly made public

  !MODULE PARAMETER DEFINITIONS

! Heating coil types
INTEGER, PARAMETER :: MultiSpeedHeatingCoil = 1    ! COIL:DX:MultiSpeed:Heating
! Cooling coil types
INTEGER, PARAMETER :: MultiSpeedCoolingCoil = 2    ! COIL:DX:MultiSpeed:Cooling
! Supplymental heating coil types
INTEGER, PARAMETER :: SuppHeatingCoilGas  = 1     ! Supplymental heating coil type: COIL:GAS:HEATING
INTEGER, PARAMETER :: SuppHeatingCoilElec = 2     ! Supplymental heating coil type: COIL:ELECTRIC:HEATING
INTEGER, PARAMETER :: SuppHeatingCoilRec  = 3     ! Supplymental heating coil type: COIL:ENGINEHEATRECOVERY:HEATING

! Curve Types
INTEGER, PARAMETER :: Linear      = 1             ! Linear curve type
INTEGER, PARAMETER :: Bilinear    = 2             ! Bi-linear curve type
INTEGER, PARAMETER :: Quadratic   = 3             ! Quadratic curve type
INTEGER, PARAMETER :: Biquadratic = 4             ! Bi-quadratic curve type
INTEGER, PARAMETER :: Cubic       = 5             ! Cubic curve type

! Mode of operation
 INTEGER, PARAMETER :: CoolingMode         = 1    ! System operating mode is cooling
 INTEGER, PARAMETER :: HeatingMode         = 2    ! System operating mode is heating

! Airflow control for contant fan mode
INTEGER, PARAMETER :: UseCompressorOnFlow  = 1    ! set compressor OFF air flow rate equal to compressor ON air flow rate
INTEGER, PARAMETER :: UseCompressorOffFlow = 2    ! set compressor OFF air flow rate equal to user defined value
! Compressor operation
INTEGER, PARAMETER :: On               = 1        ! normal compressor operation
INTEGER, PARAMETER :: Off              = 0        ! signal DXCoil that compressor shouldn't run

  ! DERIVED TYPE DEFINITIONS
TYPE MSHeatPumpData
!          Some variables in this type are arrays (dimension=MaxSpeed) to support the number of speeds
  CHARACTER(len=MaxNameLength) :: Name           =' ' ! Name of the engine driven heat pump
  CHARACTER(len=MaxNameLength) :: AvaiSchedule   =' ' ! Availability Schedule name
  INTEGER  :: AvaiSchedPtr          = 0  ! Pointer to the correct schedule
  INTEGER  :: AirInletNodeNum       = 0  ! Node number of the heat pump air inlet
  INTEGER  :: AirOutletNodeNum      = 0  ! Node number of the heat pump air inlet
  CHARACTER(len=MaxNameLength) :: AirInletNodeName =' ' ! Node name of the heat pump air inlet
  CHARACTER(len=MaxNameLength) :: AirOutletNodeName =' ' ! Node name of the heat pump air outlet
  INTEGER  :: ControlZoneNum        = 0  ! Controlling zone or thermostat location
  INTEGER  :: ZoneSequenceCoolingNum =0 ! Index to cooling sequence/priority for this zone
  INTEGER  :: ZoneSequenceHeatingNum =0 ! Index to heating sequence/priority for this zone
  CHARACTER(len=MaxNameLength) :: ControlZoneName ! Controlled zone name
  INTEGER  :: NodeNumofControlledZone =0 ! Controlled zone node number
  REAL(r64)     :: FlowFraction          =0.0d0 ! Fraction of the total volume flow that goes through the controlling zone
  CHARACTER(len=MaxNameLength) :: FanName        =' ' ! Name of supply air fan
  INTEGER  :: FanType               = 0  ! Supply fan type
  INTEGER  :: FanNum                = 0  ! Supply fan number
  INTEGER  :: FanPlaceType          = 0  ! Supply air fan placement: 1 Blow through; 2 Draw through
  INTEGER  :: FanInletNode          = 0  ! Fan Inlet node
  INTEGER  :: FanOutletNode         = 0  ! Fan Outlet node
  REAL(r64)     :: FanVolFlow            =0.0d0 ! Supply fan volumetric flow rate
  CHARACTER(len=MaxNameLength) :: FanSchedule    =' ' ! Supply air fan operating mode schedule name
  INTEGER  :: FanSchedPtr           = 0  ! Pointer to the Supply air fan operating mode schedule
  INTEGER  :: OpMode                = 0  ! mode of operation; 1=cycling fan, cycling compressor; 2=continuous fan, cycling compresor
  CHARACTER(len=MaxNameLength) :: DXHeatCoilName  =' ' ! COIL:DX:MultiSpeed:Heating name
  INTEGER  :: HeatCoilType          = 0  ! Heating coil type: 1 COIL:DX:MultiSpeed:Heating only
  INTEGER  :: HeatCoilNum           = 0  ! Heating coil number
  INTEGER  :: DXHeatCoilIndex       = 0  ! DX heating coil index number
  REAL(r64)     :: MinOATCompressor      =0.0d0 ! Minimum outdoor dry-bulb temperature for compressor operation
  CHARACTER(len=MaxNameLength) :: HeatCoilName  =' ' ! Coil:Electric:MultiSpeed:Heating OR Coil:Gas:MultiSpeed:Heating name
  INTEGER  :: HeatCoilIndex       = 0  ! heating coil index number (Coil:Electric:MultiSpeed:Heating OR Coil:Gas:MultiSpeed:Heating)
  CHARACTER(len=MaxNameLength) :: DXCoolCoilName  =' ' ! COIL:DX:MultiSpeed:Cooling name
  INTEGER  :: CoolCoilType          = 0  ! Cooling coil type: 1 COIL:DX:MultiSpeed:Cooling only
  INTEGER  :: CoolCoilNum           = 0  ! Cooling coil number
  INTEGER  :: DXCoolCoilIndex       = 0  ! DX cooling coil index number
  CHARACTER(len=MaxNameLength) :: SuppHeatCoilName  =' ' ! Supplymental heating coil name
  INTEGER  :: SuppHeatCoilType      = 0  ! Supplymental heating coil type: 1 Gas; 2 Electric; 3 Recovery
  INTEGER  :: SuppHeatCoilNum       = 0  ! Supplymental heating coil number
  REAL(r64)     :: DesignSuppHeatingCapacity = 0.0d0 ! Supplemental heating coil design capacity
  REAL(r64)     :: SuppMaxAirTemp        = 0.0d0  ! Maximum supply air temperature from supplemental heater
  REAL(r64)     :: SuppMaxOATemp         = 0.0d0  ! Maximum outdoor dry-bulb temperature for supplemental heater operation
  REAL(r64)     :: AuxOnCyclePower       =0.0d0 ! Auxiliary On-Cycle Electric Power
  REAL(r64)     :: AuxOffCyclePower      =0.0d0 ! Auxiliary Off-Cycle Electric Power
  REAL(r64)     :: DesignHeatRecFlowRate =0.0d0 ! Design water volume flow rate through heat recovery loop [m3/s]
  LOGICAL  :: HeatRecActive    = .False. ! True when entered Heat Rec Vol Flow Rate > 0
  CHARACTER(len=MaxNameLength) :: HeatRecName  =' ' ! heat recovery water inlet name
  INTEGER  :: HeatRecInletNodeNum   = 0  ! Node number on heat recovery water inlet
  INTEGER  :: HeatRecOutletNodeNum  = 0  ! Node number on heat recovery water outlet
  REAL(r64)     :: MaxHeatRecOutletTemp  =0.0d0 ! Maximum outlet water temperature for heat recovery
  REAL(r64)     :: DesignHeatRecMassFlowRate = 0.0d0 ! Design water mass flow rate through heat recovery loop [kg/s]
  INTEGER       :: HRLoopNum        = 0  ! plant loop number for heat recovery
  INTEGER       :: HRLoopSideNum    = 0  ! Plant loop side for heat recovery
  INTEGER       :: HRBranchNum      = 0  ! plant loop branch for heat recovery
  INTEGER       :: HRCompNum        = 0  ! plant loop component for heat recovery
  REAL(r64)     :: AuxElecPower          =0.0d0 ! Auxiliary Electric Power
  REAL(r64)     :: IdleVolumeAirRate     =0.0d0 ! Supply air volumetric flow rate when no cooling or heating is needed
  REAL(r64)     :: IdleMassFlowRate      =0.0d0 ! Supply air mass flow rate when no cooling or heating is needed
  REAL(r64)     :: IdleSpeedRatio        =0.0d0 ! Fan speed ratio in idle mode
  INTEGER  :: NumOfSpeedCooling     =0   ! The number of speeds for cooling
  INTEGER  :: NumOfSpeedHeating     =0   ! The number of speeds for heating
  REAL(r64), DIMENSION(:), ALLOCATABLE :: HeatVolumeFlowRate ! Supply air volume flow rate during heating operation
  REAL(r64), DIMENSION(:), ALLOCATABLE :: HeatMassFlowRate ! Supply air mass flow rate during heating operation
  REAL(r64), DIMENSION(:), ALLOCATABLE :: CoolVolumeFlowRate ! Supply air volume flow rate during cooling operation
  REAL(r64), DIMENSION(:), ALLOCATABLE :: CoolMassFlowRate ! Supply air mass flow rate during cooling operation
  REAL(r64), DIMENSION(:), ALLOCATABLE :: HeatingSpeedRatio ! Fan speed ratio in heating mode
  REAL(r64), DIMENSION(:), ALLOCATABLE :: CoolingSpeedRatio ! Fan speed ratio in cooling mode
  LOGICAL  :: CheckFanFlow      = .TRUE. ! Supply airflow check
  INTEGER  :: LastMode              = 0  ! MSHP operation mode
  INTEGER  :: HeatCoolMode          = 0  ! System operating mode (0 = floating, 1 = cooling, 2 = heating)
  INTEGER  :: AirLoopNumber         = 0  ! Air loop served by the engine driven heat pump system
  INTEGER  :: NumControlledZones    = 0  ! Number of controlled zones for this system
  INTEGER  :: ZoneInletNode         = 0  ! Zone inlet node number in the controlled zone
  REAL(r64)     :: CompPartLoadRatio     =0.0d0 ! Compressor part load ratio
  REAL(r64)     :: FanPartLoadRatio      =0.0d0 ! Fan part load ratio
  REAL(r64)     :: TotCoolEnergyRate     =0.0d0 ! Total cooling enertgy rate
  REAL(r64)     :: TotHeatEnergyRate     =0.0d0 ! Total heating enertgy rate
  REAL(r64)     :: SensCoolEnergyRate    =0.0d0 ! Sensible cooling enertgy rate
  REAL(r64)     :: SensHeatEnergyRate    =0.0d0 ! Sensible heating enertgy rate
  REAL(r64)     :: LatCoolEnergyRate     =0.0d0 ! Latent cooling enertgy rate
  REAL(r64)     :: LatHeatEnergyRate     =0.0d0 ! Latent heating enertgy rate
  REAL(r64)     :: ElecPower             =0.0d0 ! Electric power (fan + supplemental electric coil)
  REAL(r64)     :: LoadMet               =0.0d0 ! met system load
  REAL(r64)     :: HeatRecoveryRate       =0.0d0 ! Heat recovery rate [W]
  REAL(r64)     :: HeatRecoveryInletTemp  =0.0d0 ! Inlet temperature for heat recovery rate [C]
  REAL(r64)     :: HeatRecoveryOutletTemp =0.0d0 ! Outlet temperature for heat recovery rate [C]
  REAL(r64)     :: HeatRecoveryMassFlowRate=0.0d0 ! Mass flow rate for heat recovery rate [kg/s]
  INTEGER  :: AirFlowControl       = 0   ! fan control mode, UseCompressorOnFlow or UseCompressorOffFlow
  INTEGER  :: ErrIndexCyc          = 0   ! Error index at low speed
  INTEGER  :: ErrIndexVar          = 0   ! Error index at high speed
  REAL(r64)     :: LoadLoss              =0.0d0 ! Air distribution system loss

  INTEGER      :: SuppCoilAirInletNode          =0  ! air inlet node number of supplemental heating coil
  INTEGER      :: SuppCoilAirOutletNode         =0  ! air outlet node number of supplemental heating coil
  INTEGER      :: SuppHeatCoilType_Num          =0  ! Numeric Equivalent for Supplemental Heat Coil Type
  INTEGER      :: SuppHeatCoilIndex             =0  ! Index to supplemental heater
  INTEGER      :: SuppCoilControlNode           =0  ! control node for simple water and steam heating coil
  REAL(r64)    :: MaxSuppCoilFluidFlow          =0.0d0  ! water or steam mass flow rate for supplemental heating coil [kg/s]
  INTEGER      :: SuppCoilOutletNode            =0  ! outlet node for hot water and steam supplemental heating coil
  INTEGER      :: CoilAirInletNode              =0  ! air inlet node number of supplemental heating coil
  INTEGER      :: CoilControlNode               =0  ! control node for simple water and steam heating coil
  REAL(r64)    :: MaxCoilFluidFlow              =0.0d0  ! water or steam mass flow rate for supplemental heating coil [kg/s]
  INTEGER      :: CoilOutletNode                =0  ! outlet node for hot water and steam supplemental heating coil
  INTEGER      :: HotWaterCoilControlNode       =0
  INTEGER      :: HotWaterCoilOutletNode        =0
  CHARACTER(len=MaxNameLength) :: HotWaterCoilName  =' '
  INTEGER      :: HotWaterCoilNum               =0
  INTEGER      :: LoopNum                       =0  ! plant loop index for hot water and steam heating coil
  INTEGER      :: LoopSide                      =0  ! plant loop side  index for hot water and steam heating coil
  INTEGER      :: BranchNum                     =0  ! plant loop branch index for water and steam heating coil
  INTEGER      :: CompNum                       =0  ! plant loop component index for hot water and steam heating coil
  INTEGER      :: SuppLoopNum                   =0  ! plant loop index for hot water and steam supplemental heating coil
  INTEGER      :: SuppLoopSide                  =0  ! plant loop side  index for hot water and steam supplemental heating coil
  INTEGER      :: SuppBranchNum                 =0  ! plant loop branch index for water and steam supplemental heating coil
  INTEGER      :: SuppCompNum                   =0  ! plant loop component index for hot water and steam supplemental heating coil
  INTEGER      :: HotWaterLoopNum               =0  ! plant loop index for hot water and steam heating coil
  INTEGER      :: HotWaterLoopSide              =0  ! plant loop side  index for hot water and steam heating coil
  INTEGER      :: HotWaterBranchNum             =0  ! plant loop branch index for water and steam heating coil
  INTEGER      :: HotWaterCompNum               =0  ! plant loop component index for hot water and steam heating coil
  Integer      :: HotWaterCoilMaxIterIndex      =0  ! Index to recurring warning message
  Integer      :: HotWaterCoilMaxIterIndex2     =0  ! Index to recurring warning message
  Integer      :: StageNum                      =0  ! Stage number specified by staged thermostat
  LOGICAL  :: Staged      = .FALSE.                 ! Using Staged thermostat
  INTEGER      :: CoolCountAvail                 =0 ! Counter used to minimize the occurrence of output warnings
  INTEGER      :: CoolIndexAvail                 =0 ! Index used to minimize the occurrence of output warnings
  INTEGER      :: HeatCountAvail                 =0 ! Counter used to minimize the occurrence of output warnings
  INTEGER      :: HeatIndexAvail                 =0 ! Index used to minimize the occurrence of output warnings

END TYPE MSHeatPumpData

TYPE MSHeatPumpReportData
  REAL(r64)     :: ElecPowerConsumption   =0.0d0 ! Electricity power comsumption: CondenserFan+CrankcaseHeater+Defroster+aux
  REAL(r64)     :: HeatRecoveryEnergy     =0.0d0 ! Heat recovery rate [J]
  REAL(r64)     :: CycRatio               =0.0d0 ! Cycle ratio
  REAL(r64)     :: SpeedRatio             =0.0d0 ! Speed ratio between two stages
  INTEGER       :: SpeedNum               =0   ! Speed number
  REAL(r64)     :: AuxElecCoolConsumption =0.0d0 ! Auxiliary electricity power consumption during cooling
  REAL(r64)     :: AuxElecHeatConsumption =0.0d0 ! Auxiliary electricity power consumption during heating
END TYPE MSHeatPumpReportData

  ! MODULE VARIABLE DECLARATIONS:
INTEGER    :: NumMSHeatPumps = 0            ! Number of multi speed heat pumps
INTEGER    :: AirLoopPass = 0               ! Number of air loop pass
REAL(r64)  :: TempSteamIn = 100.0d0           ! steam coil steam inlet temperature

TYPE (MSHeatPumpData), ALLOCATABLE, DIMENSION(:) :: MSHeatPump
TYPE (MSHeatPumpReportData), ALLOCATABLE, DIMENSION(:) :: MSHeatPumpReport

CHARACTER(len=MaxNameLength) :: CurrentModuleObject   ! Object type for getting and error messages
REAL(r64)    :: CompOnMassFlow         = 0.0d0  ! System air mass flow rate w/ compressor ON
REAL(r64)    :: CompOffMassFlow        = 0.0d0  ! System air mass flow rate w/ compressor OFF
REAL(r64)    :: CompOnFlowRatio        = 0.0d0  ! fan flow ratio when coil on
REAL(r64)    :: CompOffFlowRatio       = 0.0d0  ! fan flow ratio when coil off
REAL(r64)    :: FanSpeedRatio          = 0.0d0  ! fan speed ratio passed to on/off fan object
REAL(r64)    :: SupHeaterLoad          = 0.0d0  ! load to be met by supplemental heater [W]
REAL(r64)    :: SaveLoadResidual       = 0.0d0  ! Saved load residual used to check convergence
REAL(r64)    :: SaveCompressorPLR      = 0.0d0  ! holds compressor PLR from active DX coil
LOGICAL, ALLOCATABLE, DIMENSION(:) :: CheckEquipName

  ! SUBROUTINE SPECIFICATIONS FOR MODULE

PUBLIC  SimMSHeatPump
PRIVATE SimMSHP
PRIVATE GetMSHeatPumpInput
PRIVATE InitMSHeatPump
PRIVATE ControlMSHPOutput
PRIVATE MSHPCyclingResidual
PRIVATE MSHPVarSpeedResidual
PRIVATE CalcMSHeatPump
PRIVATE SizeMSHeatPump
PRIVATE MSHPHeatRecovery
PRIVATE UpdateMSHeatPump
PRIVATE ReportMSHeatPump
PRIVATE SetAverageAirFlow
PRIVATE CalcNonDXHeatingCoils


CONTAINS

SUBROUTINE SimMSHeatPump(CompName, FirstHVACIteration, AirLoopNum, CompIndex)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Lixing Gu, Florida Solar Energy Center
          !       DATE WRITTEN   June. 2007
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Manages the simulation of multispeed heat pump.

          ! METHODOLOGY EMPLOYED:
          ! NA

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: FindItemInList
  USE General,        ONLY: TrimSigDigits

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT    (IN) :: CompName            ! Name of the unitary engine driven heat pump system
  LOGICAL,          INTENT    (IN) :: FirstHVACIteration  ! TRUE if 1st HVAC simulation of system time step
  INTEGER,          INTENT    (IN) :: AirLoopNum          ! air loop index
  INTEGER,          INTENT (INOUT) :: CompIndex           ! Index to changeover-bypass VAV system
          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER       :: MSHeatPumpNum          ! index of fan coil unit being simulated
  LOGICAL, SAVE :: GetInputFlag = .TRUE.  ! Get input flag
  REAL(r64)     :: OnOffAirFlowRatio      ! Ratio of compressor ON airflow to average airflow over timestep
  REAL(r64)     :: QZnLoad                ! Zone load required by all zones served by this air loop system
  REAL(r64)     :: QSensUnitOut           ! MSHP sensible capacity output [W]
  ! FLOW

  ! First time SimMSHeatPump is called, get the input
  IF (GetInputFlag) THEN
    CALL GetMSHeatPumpInput
    GetInputFlag = .FALSE. ! Set GetInputFlag false so you don't get coil inputs again
  END IF

  IF (CompIndex == 0) THEN
    MSHeatPumpNum = FindItemInList(CompName,MSHeatPump%Name,NumMSheatPumps)
    IF (MSHeatPumpNum == 0) THEN
      CALL ShowFatalError('MultiSpeed Heat Pump is not found='//TRIM(CompName))
    ENDIF
    CompIndex=MSHeatPumpNum
  ELSE
    MSHeatPumpNum=CompIndex
    IF (MSHeatPumpNum > NumMSHeatPumps .or. MSHeatPumpNum < 1) THEN
      CALL ShowFatalError('SimMSHeatPump: Invalid CompIndex passed='//  &
                        TRIM(TrimSigDigits(MSHeatPumpNum))// &
                        ', Number of MultiSpeed Heat Pumps='//TRIM(TrimSigDigits(NumMSHeatPumps))//  &
                        ', Heat Pump name='//TRIM(CompName))
    ENDIF
    IF (CheckEquipName(MSHeatPumpNum)) THEN
      IF (CompName /= MSHeatPump(MSHeatPumpNum)%Name) THEN
        CALL ShowFatalError('SimMSHeatPump: Invalid CompIndex passed='//  &
                          TRIM(TrimSigDigits(MSHeatPumpNum))// &
                          ', Heat Pump name='//TRIM(CompName)// &
                          TRIM(MSHeatPump(MSHeatPumpNum)%Name))
      ENDIF
      CheckEquipName(MSHeatPumpNum)=.false.
    ENDIF
  ENDIF

  OnOffAirFlowRatio = 0.0d0

  ! Initialize the engine driven heat pump
  CALL InitMSHeatPump(MSHeatPumpNum, FirstHVACIteration, AirLoopNum, QZnLoad, OnOffAirFlowRatio)

  CALL SimMSHP(MSHeatPumpNum,FirstHVACIteration, QSensUnitOut, QZnLoad, OnOffAirFlowRatio)

  ! Update the unit outlet nodes
  CALL UpdateMSHeatPump(MSHeatPumpNum)

  ! Report the result of the simulation
  CALL ReportMSHeatPump(MSHeatPumpNum)


RETURN
END SUBROUTINE SimMSHeatPump

!******************************************************************************

SUBROUTINE SimMSHP(MSHeatPumpNum,FirstHVACIteration, QSensUnitOut, QZnReq, OnOffAirFlowRatio)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Lixing Gu
          !       DATE WRITTEN   June 2007
          !       MODIFIED       na
          !       RE-ENGINEERED  Revised based on SimPTHP

          ! PURPOSE OF THIS SUBROUTINE:
          ! Simulate a multispeed heat pump; adjust its output to match the
          ! required system load.

          ! METHODOLOGY EMPLOYED:
          ! Calls ControlMSHPOutput to obtain the desired unit output

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataZoneEnergyDemands
  USE DataHVACGlobals, ONLY: SmallMassFlow, SmallLoad
  USE DataZoneEquipment, ONLY: ZoneEquipConfig
  USE DataAirLoop, ONLY: AirLoopControlInfo

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  LOGICAL, INTENT (IN)    :: FirstHVACIteration ! TRUE if 1st HVAC simulation of system timestep
  INTEGER, INTENT (IN)    :: MSHeatPumpNum      ! number of the current engine driven Heat Pump being simulated
  REAL(r64),    INTENT (IN)    :: QZnReq             ! required zone load
  REAL(r64),    INTENT (INOUT) :: QSensUnitOut       ! cooling/heating deliveded to zones [W]
  REAL(r64),    INTENT (INOUT) :: OnOffAirFlowRatio  ! ratio of compressor ON airflow to AVERAGE airflow over timestep

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64)    :: PartLoadFrac      ! compressor part load fraction
  REAL(r64)    :: SpeedRatio        ! compressor speed ratio
  LOGICAL :: UnitOn            ! TRUE if unit is on
  INTEGER :: OutletNode        ! MSHP air outlet node
  INTEGER :: InletNode         ! MSHP air inlet node
  REAL(r64)    :: AirMassFlow       ! air mass flow rate [kg/s]
  INTEGER :: OpMode            ! operating mode (fan cycling or continious; DX coil always cycles)
  INTEGER :: ZoneNum           ! Controlled zone number
  REAL(r64)    :: QTotUnitOut
  INTEGER :: SpeedNum          ! Speed number
  REAL(r64)    :: SupHeaterLoad
  INTEGER :: CompOp            ! compressor operation; 1=on, 0=off
  INTEGER :: AirLoopNumber     ! Index to air loop
  REAL(r64)    :: SaveMassFlowRate  ! saved inlet air mass flow rate [kg/s]

  ! zero the fan, DX coils, and supplemental electric heater electricity consumption
  FanElecPower         = 0.0d0
  DXElecHeatingPower   = 0.0d0
  DXElecCoolingPower   = 0.0d0
  SaveCompressorPLR    = 0.0d0
  ElecHeatingCoilPower = 0.0d0

  ! initialize local variables
  UnitOn      = .TRUE.
  OutletNode  = MSHeatPump(MSHeatPumpNum)%AirOutletNodeNum
  InletNode   = MSHeatPump(MSHeatPumpNum)%AirInletNodeNum
  AirMassFlow = Node(InletNode)%MassFlowRate
  OpMode      = MSHeatPump(MSHeatPumpNum)%OpMode
  ZoneNum     = MSHeatPump(MSHeatPumpNum)%ControlZoneNum
  CompOp      = On

  ! set the on/off flags
  IF (MSHeatPump(MSHeatPumpNum)%OPMode == CycFanCycCoil) THEN
    ! cycling unit only runs if there is a cooling or heating load.
     IF (ABS(QZnReq) < SmallLoad .OR. AirMassFlow < SmallMassFlow .OR. CurDeadbandOrSetback(ZoneNum)) THEN
       UnitOn = .FALSE.
     END IF
  ELSE IF  (MSHeatPump(MSHeatPumpNum)%OPMode == ContFanCycCoil) THEN
    ! continuous unit: fan runs if scheduled on; coil runs only if there is a cooling or heating load
    IF (AirMassFlow.LT.SmallMassFlow) THEN
      UnitOn = .FALSE.
    END IF
  END IF

  OnOffFanPartLoadFraction = 1.0d0

  AirLoopNumber = ZoneEquipConfig(MSHeatPump(MSHeatPumpNum)%ControlZoneNum)%AirLoopNum
  SaveMassFlowRate = Node(InletNode)%MassFlowRate
  IF ( .NOT. FirstHVACIteration .AND. MSHeatPump(MSHeatPumpNum)%OPMode == CycFanCycCoil .AND. QZnReq < 0.0d0 &
       .AND. AirLoopControlInfo(AirLoopNumber)%EconoActive) THEN
       ! for cycling fan, cooling load, check whether furnace can meet load with compressor off
    CompOp = Off
    CALL ControlMSHPOutput(MSHeatPumpNum,FirstHVACIteration,CompOp,OpMode,QZnReq,ZoneNum,SpeedNum,SpeedRatio, &
                         PartLoadFrac,OnOffAirFlowRatio,SupHeaterLoad)
    IF (SpeedNum .EQ. MSHeatPump(MSHeatPumpNum)%NumOfSpeedCooling .AND. SpeedRatio .EQ. 1.0d0) THEN
      ! compressor on (reset inlet air mass flow rate to starting value)
      Node(InletNode)%MassFlowRate = SaveMassFlowRate
      CompOp = On
      CALL ControlMSHPOutput(MSHeatPumpNum,FirstHVACIteration,CompOp,OpMode,QZnReq,ZoneNum,SpeedNum,SpeedRatio, &
                         PartLoadFrac,OnOffAirFlowRatio,SupHeaterLoad)
    END IF
  ELSE
     ! compressor on
     CALL ControlMSHPOutput(MSHeatPumpNum,FirstHVACIteration,CompOp,OpMode,QZnReq,ZoneNum,SpeedNum,SpeedRatio, &
                         PartLoadFrac,OnOffAirFlowRatio,SupHeaterLoad)
  END IF

  IF (MSHeatPump(MSHeatPumpNum)%HeatCoilType .NE. MultiSpeedHeatingCoil) THEN
    SaveCompressorPLR = PartLoadFrac
  ELSE
    IF(SpeedNum > 1)  THEN
      SaveCompressorPLR = 1.0d0
    END IF

    If (PartLoadFrac .eq. 1.0d0 .and. SaveCompressorPLR < 1.0d0 .AND. (.NOT. MSHeatPump(MSHeatPumpNum)%Staged)) then
      PartLoadFrac = SaveCompressorPLR
    End If
  END IF

  CALL CalcMSHeatPump(MSHeatPumpNum,FirstHVACIteration,CompOp,SpeedNum,SpeedRatio,PartLoadFrac,QSensUnitOut, &
                      QZnReq,OnOffAirFlowRatio,SupHeaterLoad)

  ! calculate delivered capacity
  AirMassFlow = Node(InletNode)%MassFlowRate

  QTotUnitOut = AirMassFlow * (Node(OutletNode)%Enthalpy - Node(MSHeatPump(MSHeatPumpNum)%NodeNumofControlledZone)%Enthalpy)

  ! report variables
  MSHeatPump(MSHeatPumpNum)%CompPartLoadRatio = SaveCompressorPLR
  IF (MSHeatPump(MSHeatPumpNum)%OpMode .EQ. CycFanCycCoil) THEN
    If (SupHeaterLoad >0.0d0) Then
      MSHeatPump(MSHeatPumpNum)%FanPartLoadRatio = 1.0d0
    Else
      If (SpeedNum .LT. 2) Then
        MSHeatPump(MSHeatPumpNum)%FanPartLoadRatio = PartLoadFrac
      Else
        MSHeatPump(MSHeatPumpNum)%FanPartLoadRatio = 1.0d0
      End If
    End If
  ELSE
    IF (UnitOn) THEN
      MSHeatPump(MSHeatPumpNum)%FanPartLoadRatio = 1.0d0
    ELSE
      If (SpeedNum .LT. 2) Then
        MSHeatPump(MSHeatPumpNum)%FanPartLoadRatio = PartLoadFrac
      Else
        MSHeatPump(MSHeatPumpNum)%FanPartLoadRatio = 1.0d0
      End If
    END IF
  END IF

  If (MSHeatPump(MSHeatPumpNum)%HeatCoolMode == HeatingMode) Then
    MSHeatPump(MSHeatPumpNum)%TotHeatEnergyRate  = ABS(MAX(0.0d0, QTotUnitOut))
    MSHeatPump(MSHeatPumpNum)%SensHeatEnergyRate = ABS(MAX(0.0d0, QSensUnitOut))
    MSHeatPump(MSHeatPumpNum)%LatHeatEnergyRate  = ABS(MAX(0.0d0, (QTotUnitOut - QSensUnitOut)))
    MSHeatPump(MSHeatPumpNum)%TotCoolEnergyRate  = 0.0d0
    MSHeatPump(MSHeatPumpNum)%SensCoolEnergyRate = 0.0d0
    MSHeatPump(MSHeatPumpNum)%LatCoolEnergyRate  = 0.0d0
  End If
  If (MSHeatPump(MSHeatPumpNum)%HeatCoolMode == CoolingMode) Then
    MSHeatPump(MSHeatPumpNum)%TotCoolEnergyRate  = ABS(MIN(0.0d0, QTotUnitOut))
    MSHeatPump(MSHeatPumpNum)%SensCoolEnergyRate = ABS(MIN(0.0d0, QSensUnitOut))
    MSHeatPump(MSHeatPumpNum)%LatCoolEnergyRate  = ABS(MIN(0.0d0, (QTotUnitOut - QSensUnitOut)))
    MSHeatPump(MSHeatPumpNum)%TotHeatEnergyRate  = 0.0d0
    MSHeatPump(MSHeatPumpNum)%SensHeatEnergyRate = 0.0d0
    MSHeatPump(MSHeatPumpNum)%LatHeatEnergyRate  = 0.0d0
  End If

  MSHeatPump(MSHeatPumpNum)%AuxElecPower = MSHeatPump(MSHeatPumpNum)%AuxOnCyclePower*SaveCompressorPLR + &
                                           MSHeatPump(MSHeatPumpNum)%AuxOffCyclePower*(1.0d0-SaveCompressorPLR)
  IF (MSHeatPump(MSHeatPumpNum)%HeatCoilType .NE. MultiSpeedHeatingCoil) THEN
    SELECT CASE (MSHeatPump(MSHeatPumpNum)%HeatCoilType)
      CASE (Coil_HeatingGas_MultiStage, Coil_HeatingElectric_MultiStage)
        MSHeatPump(MSHeatPumpNum)%ElecPower = FanElecPower + DXElecCoolingPower + ElecHeatingCoilPower
      CASE (Coil_HeatingWater, Coil_HeatingSteam)
        MSHeatPump(MSHeatPumpNum)%ElecPower = FanElecPower + DXElecCoolingPower
      CASE DEFAULT
    END SELECT
  ELSE
    MSHeatPump(MSHeatPumpNum)%ElecPower = FanElecPower + DXElecCoolingPower + DXElecHeatingPower + ElecHeatingCoilPower + &
                                          MSHeatPump(MSHeatPumpNum)%AuxElecPower
  ENDIF

  RETURN
END SUBROUTINE SimMSHP

!******************************************************************************

SUBROUTINE GetMSHeatPumpInput
            ! SUBROUTINE INFORMATION:
            !       AUTHOR:          Lixing Gu, FSEC
            !       DATE WRITTEN:    July 2007
            !       MODIFIED         na
            !       RE-ENGINEERED    na

            ! PURPOSE OF THIS SUBROUTINE:
            !  This routine will get the input required by the multispeed heat pump model

            ! METHODOLOGY EMPLOYED:
            ! na

            ! REFERENCES: na

            ! USE STATEMENTS:
  USE DataInterfaces,        ONLY: ShowSevereError, ShowWarningError, ShowFatalError, SetupOutputVariable, ShowContinueError
  USE InputProcessor,        ONLY: GetNumObjectsFound, GetObjectItem, VerifyName, SameString, FindItemInList, GetObjectItemNum, &
                                   GetObjectDefMaxArgs
  USE BranchNodeConnections, ONLY: TestCompSet
  USE NodeInputManager,      ONLY: GetOnlySingleNode
  USE FluidProperties,       ONLY: FindGlycol
  USE CurveManager,          ONLY: CurveValue
  USE General,               ONLY: RoundSigDigits
  USE GlobalNames,           ONLY: VerifyUniqueChillerName
  USE DataSizing,            ONLY: Autosize
  USE Fans,                  ONLY: GetFanType, GetFanIndex, GetFanVolFlow, GetFanInletNode, GetFanOutletNode
  USE DataHeatBalance,       ONLY: Zone
  USE DataZoneEquipment,     ONLY: ZoneEquipConfig
  USE DataAirSystems,        ONLY : PrimaryAirSystem
  USE DataZoneControls,      ONLY: TempControlledZone, NumTempControlledZones, NumComfortControlledZones, &
                                        ComfortControlledZone
  USE DataHVACGlobals,       ONLY: FanType_SimpleOnOff, FanType_SimpleConstVolume

  USE CurveManager,          ONLY: GetCurveIndex, GetCurveType, CurveValue
  USE BranchNodeConnections, ONLY: SetUpCompSets
  USE DXCoils,               ONLY: GetDXCoilIndex, GetDXCoilInletNode=>GetCoilInletNode, &
                                   GetDXCoilOutletNode=>GetCoilOutletNode,GetDXCoilNumberOfSpeeds,GetDXCoilNumberOfSpeeds
  USE HeatingCoils,          ONLY: GetHeatingCoilInletNode=>GetCoilInletNode, GetHeatingCoilOutletNode=>GetCoilOutletNode, &
                                   GetHeatingCoilCapacity=>GetCoilCapacity, GetHeatingCoilIndex, GetCoilIndex, GetCoilInletNode, &
                                   GetCoilOutletNode, GetHeatingCoilNumberOfStages
  USE WaterCoils,            ONLY: GetCoilWaterInletNode, GetCoilMaxWaterFlowRate, &
                                   GetWaterCoilInletNode=>GetCoilInletNode,GetWaterCoilOutletNode=>GetCoilOutletNode
  USE SteamCoils,            ONLY: GetSteamCoilAirInletNode=>GetCoilAirInletNode, GetSteamCoilIndex, &
                                   GetSteamCoilAirOutletNode=>GetCoilAirOutletNode, &
                                   GetSteamCoilSteamInletNode=>GetCoilSteamInletNode, &
                                   GetCoilMaxSteamFlowRate=>GetCoilMaxSteamFlowRate, GetTypeOfCoil, ZoneLoadControl
  USE FluidProperties,       ONLY: GetSatDensityRefrig

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

            ! PARAMETERS
  CHARACTER(Len=*), PARAMETER :: RoutineName='GetMSHeatPumpInput: ' ! include trailing blank space


            ! LOCAL VARIABLES
  INTEGER                     :: MSHPNum                   ! Engine driven heat pump count
  INTEGER                     :: NumAlphas                 ! Number of elements in the alpha array
  INTEGER                     :: NumNumbers                ! Number of Numbers for each GetObjectItem call
  INTEGER                     :: IOStatus                  ! Used in GetObjectItem
  LOGICAL, SAVE               :: ErrorsFound=.false.       ! True when input errors found
  LOGICAL                     :: IsNotOK                   ! Flag to verify name
  LOGICAL                     :: IsBlank                   ! Flag for blank name
  LOGICAL, SAVE               :: AllocatedFlag =.FALSE.    ! True when arrays are allocated
  LOGICAL                     :: AirNodeFound              ! True when an air node is found
  LOGICAL                     :: AirLoopFound              ! True when an air loop is found
  INTEGER                     :: ControlledZoneNum         ! Controlled zone number
  INTEGER                     :: AirLoopNumber             ! Index to air loop
  INTEGER                     :: FanType                   ! Fan type
  INTEGER                     :: BranchNum                 ! Index to branch
  INTEGER                     :: CompNum                   ! Index to component
  INTEGER                     :: TstatZoneNum              ! Used to determine if control zone has a thermostat object
  INTEGER                     :: i                         ! Index to speeds
  INTEGER                     :: j                         ! Index to speeds
  LOGICAL                     :: Found                     ! Flag to find autosize
  INTEGER                     :: HeatingCoilInletNode      ! Heating coil inlet node number
  INTEGER                     :: HeatingCoilOutletNode     ! Heating coil outlet node number
  INTEGER                     :: CoolingCoilInletNode      ! Cooling coil inlet node number
  INTEGER                     :: CoolingCoilOutletNode     ! Cooling coil outlet node number
  INTEGER                     :: SuppHeatCoilInletNode     ! Supplemental heating coil inlet node number
  INTEGER                     :: SuppHeatCoilOutletNode    ! Supplemental heating coil outlet node number
  LOGICAL                     :: LocalError                ! Local error flag
  INTEGER                     :: SpeedInput                ! Status of number of speed input
  CHARACTER(Len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: Alphas         ! Alpha input items for object
  CHARACTER(Len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: cAlphaFields   ! Alpha field names
  CHARACTER(Len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: cNumericFields ! Numeric field names
  REAL(r64), ALLOCATABLE, DIMENSION(:) :: Numbers          ! Numeric input items for object
  LOGICAL, ALLOCATABLE, DIMENSION(:)   :: lAlphaBlanks     ! Logical array, alpha field input BLANK = .true.
  LOGICAL, ALLOCATABLE, DIMENSION(:)   :: lNumericBlanks   ! Logical array, numeric field input BLANK = .true.
  INTEGER                        :: MaxNums=0              ! Maximum number of numeric input fields
  INTEGER                        :: MaxAlphas=0            ! Maximum number of alpha input fields
  INTEGER                        :: TotalArgs=0            ! Total number of alpha and numeric arguments (max) for a
                                                           !  certain object in the input file
  LOGICAL                        :: errFlag
  INTEGER                        :: SteamIndex             ! steam coil steam inlet density
  REAL(r64)                      :: SteamDensity           ! density of steam at 100C

  ! FLOW

  If (AllocatedFlag) RETURN

  CurrentModuleObject = 'AirLoopHVAC:UnitaryHeatPump:AirToAir:MultiSpeed'   ! Object type for getting and error messages

  CALL GetObjectDefMaxArgs(CurrentModuleObject,TotalArgs,NumAlphas,NumNumbers)
  MaxNums=MAX(MaxNums,NumNumbers)
  MaxAlphas=MAX(MaxAlphas,NumAlphas)

  ALLOCATE(Alphas(MaxAlphas))
  Alphas=' '
  ALLOCATE(cAlphaFields(MaxAlphas))
  cAlphaFields=' '
  ALLOCATE(Numbers(MaxNums))
  Numbers=0.0d0
  ALLOCATE(cNumericFields(MaxNums))
  cNumericFields=' '
  ALLOCATE(lAlphaBlanks(MaxAlphas))
  lAlphaBlanks=.true.
  ALLOCATE(lNumericBlanks(MaxNums))
  lNumericBlanks=.true.

  NumMSHeatPumps = GetNumObjectsFound(CurrentModuleObject)


  IF (NumMSHeatPumps <= 0) THEN
    CALL ShowSevereError('No '//TRIM(CurrentModuleObject)//' objects specified in input file.')
    ErrorsFound=.true.
  END IF

  ! ALLOCATE ARRAYS
  ALLOCATE (MSHeatPump(NumMSHeatPumps))
  ALLOCATE (MSHeatPumpReport(NumMSHeatPumps))
  ALLOCATE(CheckEquipName(NumMSHeatPumps))
  CheckEquipName=.true.
  AllocatedFlag = .TRUE.

  ! Load arrays with reformulated electric EIR chiller data
  DO MSHPNum = 1 , NumMSHeatPumps

    HeatingCoilInletNode = 0
    HeatingCoilOutletNode = 0
    CoolingCoilInletNode = 0
    CoolingCoilOutletNode = 0
    SuppHeatCoilInletNode = 0
    SuppHeatCoilOutletNode = 0

    CALL GetObjectItem(CurrentModuleObject,MSHPNum,Alphas,NumAlphas, &
                    Numbers,NumNumbers,IOStatus, NumBlank=lNumericBlanks,AlphaBlank=lAlphaBlanks, &
                    AlphaFieldNames=cAlphaFields,NumericFieldNames=cNumericFields)
    IsNotOK=.false.
    IsBlank=.false.
    CALL VerifyName(Alphas(1),MSHeatPump%Name,MSHPNum-1,IsNotOK,IsBlank, TRIM(CurrentModuleObject)//' Name')
    IF (IsNotOK) THEN
      ErrorsFound=.true.
      IF (IsBlank) Alphas(1)='xxxxx'
    END IF
    MSHeatPump(MSHPNum)%Name                = Alphas(1)
    IF (lAlphaBlanks(2)) THEN
      MSHeatPump(MSHPNum)%AvaiSchedPtr        = ScheduleAlwaysOn
    ELSE
      MSHeatPump(MSHPNum)%AvaiSchedPtr        = GetScheduleIndex(Alphas(2))
      IF (MSHeatPump(MSHPNum)%AvaiSchedPtr == 0) THEN
        CALL ShowSevereError(TRIM(CurrentModuleObject)//', "'//TRIM(MSHeatPump(MSHPNum)%Name)//&
                             '" '//TRIM(cAlphaFields(2))//' not found: '//TRIM(Alphas(2)))
        ErrorsFound=.true.
      ENDIF
    ENDIF

    MSHeatPump(MSHPNum)%AirInletNodeName = Alphas(3)
    MSHeatPump(MSHPNum)%AirOutletNodeName = Alphas(4)
    MSHeatPump(MSHPNum)%AirInletNodeNum = &
               GetOnlySingleNode(Alphas(3),ErrorsFound,TRIM(CurrentModuleObject),Alphas(1), &
               NodeType_Air,NodeConnectionType_Inlet,1,ObjectIsParent)

    MSHeatPump(MSHPNum)%AirOutletNodeNum = &
               GetOnlySingleNode(Alphas(4),ErrorsFound,TRIM(CurrentModuleObject),Alphas(1), &
               NodeType_Air,NodeConnectionType_Outlet,1,ObjectIsParent)

    CALL TestCompSet(TRIM(CurrentModuleObject),Alphas(1),Alphas(3),Alphas(4),'Air Nodes')

    !Get the Controlling Zone or Location of the engine driven heat pump Thermostat
    MSHeatPump(MSHPNum)%ControlZoneNum = FindItemInList(Alphas(5),Zone%Name,NumOfZones)
    MSHeatPump(MSHPNum)%ControlZoneName = Alphas(5)
    IF (MSHeatPump(MSHPNum)%ControlZoneNum == 0) THEN
      CALL ShowSevereError(TRIM(CurrentModuleObject)//', "'//TRIM(MSHeatPump(MSHPNum)%Name)//  &
                           '" '//TRIM(cAlphaFields(5))//' not found: '//TRIM(MSHeatPump(MSHPNum)%ControlZoneName))
      ErrorsFound=.true.
    ENDIF

    ! Get the node number for the zone with the thermostat
    IF (MSHeatPump(MSHPNum)%ControlZoneNum >  0) THEN
      AirNodeFound=.FALSE.
      AirLoopFound=.FALSE.
      DO ControlledZoneNum = 1,NumOfZones
        IF (ZoneEquipConfig(ControlledZoneNum)%ActualZoneNum /= MSHeatPump(MSHPNum)%ControlZoneNum) CYCLE
        ! Find the controlled zone number for the specified thermostat location
        MSHeatPump(MSHPNum)%NodeNumofControlledZone=ZoneEquipConfig(ControlledZoneNum)%ZoneNode
        ! Determine if furnace is on air loop served by the thermostat location specified
        AirLoopNumber = ZoneEquipConfig(ControlledZoneNum)%AirLoopNum
        IF (AirLoopNumber .GT. 0)THEN
          DO BranchNum = 1, PrimaryAirSystem(AirLoopNumber)%NumBranches
            DO CompNum = 1, PrimaryAirSystem(AirLoopNumber)%Branch(BranchNum)%TotalComponents
              IF (.NOT. SameString(PrimaryAirSystem(AirLoopNumber)%Branch(BranchNum)%Comp(CompNum)%Name, &
                 MSHeatPump(MSHPNum)%Name) .OR. &
                 .NOT. SameString(PrimaryAirSystem(AirLoopNumber)%Branch(BranchNum)%Comp(CompNum)%TypeOf, &
                  CurrentModuleObject))CYCLE
              AirLoopFound=.TRUE.
              EXIT
            END DO
            IF(AirLoopFound)EXIT
          END DO
          DO TstatZoneNum = 1, NumTempControlledZones
            IF(TempControlledZone(TstatZoneNum)%ActualZoneNum .NE. MSHeatPump(MSHPNum)%ControlZoneNum)CYCLE
            AirNodeFound=.TRUE.
          END DO
          DO TstatZoneNum = 1, NumComfortControlledZones
            IF(ComfortControlledZone(TstatZoneNum)%ActualZoneNum .NE. MSHeatPump(MSHPNum)%ControlZoneNum)CYCLE
            AirNodeFound=.TRUE.
          END DO
        ELSE
          CALL ShowSevereError('Did not find a AirLoopHVAC for '//TRIM(CurrentModuleObject)//' = "'// &
                               '"'//TRIM(MSHeatPump(MSHPNum)%Name))
          CALL ShowContinueError('Specified '//TRIM(cAlphaFields(5))//' = '//TRIM(Alphas(5)))
          ErrorsFound=.TRUE.
        END IF
        EXIT
      ENDDO
      IF (.not. AirNodeFound) THEN
        CALL ShowSevereError('Did not find Air Node ('//TRIM(cAlphaFields(5))//'), '//TRIM(CurrentModuleObject)//' = "'// &
                             '"'//TRIM(MSHeatPump(MSHPNum)%Name))
        CALL ShowContinueError('Specified '//TRIM(cAlphaFields(5))//' = '//TRIM(Alphas(5)))
        ErrorsFound=.TRUE.
      ENDIF
      IF (.not. AirLoopFound) THEN
        CALL ShowSevereError('Did not find correct AirLoopHVAC for '//TRIM(CurrentModuleObject)//' = '// &
                             TRIM(MSHeatPump(MSHPNum)%Name))
        CALL ShowContinueError('The '//TRIM(cAlphaFields(5))//' = '//TRIM(Alphas(5))// &
                               ' is not served by this Primary Air Loop equipment.')
        ErrorsFound=.TRUE.
      ENDIF
    ENDIF

!    MSHeatPump(MSHPNum)%FlowFraction = Numbers(1)
!    IF (MSHeatPump(MSHPNum)%FlowFraction .LE. 0.0 .AND. MSHeatPump(MSHPNum)%FlowFraction /= AutoSize) THEN
!      CALL ShowSevereError(TRIM(CurrentModuleObject)//', "'//TRIM(MSHeatPump(MSHPNum)%Name)//&
!                           '", '//TRIM(cNumericFields(1))//' must greater than zero.')
!      ErrorsFound = .TRUE.
!    END IF
!    IF (MSHeatPump(MSHPNum)%FlowFraction .GT. 1.0 .AND. MSHeatPump(MSHPNum)%FlowFraction /= AutoSize) THEN
!      CALL ShowSevereError(TRIM(CurrentModuleObject)//', "'//TRIM(MSHeatPump(MSHPNum)%Name)//&
!                           '", '//TRIM(cNumericFields(1))//' cannot be greater than 1.0.')
!      ErrorsFound = .TRUE.
!    END IF

    !Get supply fan data
    MSHeatPump(MSHPNum)%FanName = Alphas(7)
    IF (SameString(Alphas(6),'Fan:OnOff') .OR. SameString(Alphas(6),'Fan:ConstantVolume')) THEN
      IF (SameString(Alphas(6),'Fan:OnOff')) then
        MSHeatPump(MSHPNum)%FanType = FanType_SimpleOnOff
        CALL SetUpCompSets(TRIM(CurrentModuleObject), MSHeatPump(MSHPNum)%Name, &
                           'Fan:OnOff',MSHeatPump(MSHPNum)%FanName,'UNDEFINED', 'UNDEFINED')
        MSHeatPump(MSHPNum)%FanInletNode  = GetFanInletNode('Fan:OnOff',MSHeatPump(MSHPNum)%FanName,ErrorsFound)
        MSHeatPump(MSHPNum)%FanOutletNode = GetFanOutletNode('Fan:OnOff',MSHeatPump(MSHPNum)%FanName,ErrorsFound)
      Else
        MSHeatPump(MSHPNum)%FanType = FanType_SimpleConstVolume
        CALL SetUpCompSets(TRIM(CurrentModuleObject), MSHeatPump(MSHPNum)%Name, &
                           'Fan:ConstantVolume',MSHeatPump(MSHPNum)%FanName,'UNDEFINED', 'UNDEFINED')
        MSHeatPump(MSHPNum)%FanInletNode = GetFanInletNode('Fan:ConstantVolume',MSHeatPump(MSHPNum)%FanName,ErrorsFound)
        MSHeatPump(MSHPNum)%FanOutletNode = GetFanOutletNode('Fan:ConstantVolume',MSHeatPump(MSHPNum)%FanName,ErrorsFound)
      End If
      CALL GetFanIndex(Alphas(7), MSHeatPump(MSHPNum)%FanNum, ErrorsFound, TRIM(CurrentModuleObject))
      CALL GetFanType(Alphas(7), FanType, ErrorsFound)
      If (FanType /= MSHeatPump(MSHPNum)%FanType) then
        CALL ShowSevereError(TRIM(CurrentModuleObject)//', "'//TRIM(MSHeatPump(MSHPNum)%Name)//&
                             '", '//TRIM(cAlphaFields(6))//' and '//TRIM(cAlphaFields(7))//' do not match in Fan objects.')
        CALL ShowContinueError('The entered '//TRIM(cAlphaFields(7))//' = '//TRIM(Alphas(7))//' and '// &
                               TRIM(cAlphaFields(6))//' = '//TRIM(Alphas(6)))
        ErrorsFound = .TRUE.
      End If
    ELSE
      CALL ShowSevereError(TRIM(CurrentModuleObject)//', "'//TRIM(MSHeatPump(MSHPNum)%Name)//&
                           '", '//TRIM(cAlphaFields(6))//' is not allowed = '//TRIM(Alphas(6)))
      CALL ShowContinueError('Valid choices are Fan:OnOff or Fan:ConstantVolume')
      ErrorsFound=.true.
    END IF

    !Get supply fan placement data
    IF (SameString(Alphas(8),'BlowThrough') .OR. SameString(Alphas(8),'DrawThrough')) THEN
      IF (SameString(Alphas(8),'BlowThrough')) then
        MSHeatPump(MSHPNum)%FanPlaceType = BlowThru
      Else
        MSHeatPump(MSHPNum)%FanPlaceType = DrawThru
      End If
    ELSE
      CALL ShowSevereError(TRIM(CurrentModuleObject)//', "'//TRIM(MSHeatPump(MSHPNum)%Name)//&
                           '", '//TRIM(cAlphaFields(8))//' is not allowed = '//TRIM(Alphas(8)))
      CALL ShowContinueError('Valid choices are BlowThrough or DrawThrough')
      ErrorsFound=.true.
    END IF

    MSHeatPump(MSHPNum)%FanSchedule = Alphas(9)
    MSHeatPump(MSHPNum)%FanSchedPtr = GetScheduleIndex(Alphas(9))
    IF (MSHeatPump(MSHPNum)%FanSchedPtr == 0) THEN
      CALL ShowSevereError(TRIM(CurrentModuleObject)//', "'//TRIM(MSHeatPump(MSHPNum)%Name)//&
                           '" '//TRIM(cAlphaFields(9))//' not found: '//TRIM(Alphas(9)))
      ErrorsFound=.true.
    ENDIF

    IF (MSHeatPump(MSHPNum)%FanSchedPtr .GT. 0 .AND. MSHeatPump(MSHPNum)%FanType == FanType_SimpleConstVolume) THEN
      IF (.NOT. CheckScheduleValueMinMax(MSHeatPump(MSHPNum)%FanSchedPtr,'>',0.0d0,'<=',1.0d0)) THEN
        CALL ShowSevereError(TRIM(CurrentModuleObject)//' "'//TRIM(MSHeatPump(MSHPNum)%Name)//'"')
        CALL ShowContinueError(TRIM(cAlphaFields(9))//' must be continuous (fan operating mode schedule values > 0)'//&
                               ' for '//TRIM(cAlphaFields(6))//' = Fan:ConstantVolume.')
        CALL ShowContinueError('Error found in '//TRIM(cAlphaFields(9))//' = '//TRIM(Alphas(9)))
        CALL ShowContinueError('schedule values must be (>0., <=1.)')
        ErrorsFound=.true.
      END IF
    END IF

    IF (SameString(Alphas(10),'Coil:Heating:DX:MultiSpeed')) THEN
      MSHeatPump(MSHPNum)%HeatCoilType = MultiSpeedHeatingCoil
      MSHeatPump(MSHPNum)%HeatCoilNum = GetObjectItemNum('Coil:Heating:DX:MultiSpeed',Alphas(11))
      MSHeatPump(MSHPNum)%DXHeatCoilName = Alphas(11)
      If (MSHeatPump(MSHPNum)%HeatCoilNum <= 0) then
        CALL ShowSevereError('Configuration error in '//TRIM(CurrentModuleObject)//' "'//TRIM(Alphas(1))//'"')
        CALL ShowContinueError(TRIM(cAlphaFields(11))//' "'//TRIM(Alphas(11))//'" not found.')
        CALL ShowContinueError(TRIM(cAlphaFields(10))//' must be Coil:Heating:DX:MultiSpeed ')
        CALL ShowFatalError(RoutineName//'Errors found in getting '//TRIM(CurrentModuleObject)//' input. '//&
                            'Preceding condition(s) causes termination.')
        ErrorsFound=.true.
      End If
      LocalError = .FALSE.
      CALL GetDXCoilIndex(MSHeatPump(MSHPNum)%DXHeatCoilName,MSHeatPump(MSHPNum)%DXHeatCoilIndex, &
                              LocalError, 'Coil:Heating:DX:MultiSpeed')
      IF(LocalError) Then
        CALL ShowSevereError('The index of '//TRIM(cAlphaFields(11))//' is not found "'//TRIM(Alphas(11))//'"')
        CALL ShowContinueError('...occurs in '//TRIM(CurrentModuleObject)//' "'//TRIM(Alphas(1))//'"')
        ErrorsFound=.true.
        LocalError = .FALSE.
      End If
      HeatingCoilInletNode = GetDXCoilInletNode(Alphas(10),Alphas(11),LocalError)
      IF(LocalError) Then
        CALL ShowSevereError('The inlet node number of '//TRIM(cAlphaFields(11))//' is not found "'//TRIM(Alphas(11))//'"')
        CALL ShowContinueError('...occurs in '//TRIM(CurrentModuleObject)//' "'//TRIM(Alphas(1))//'"')
        ErrorsFound=.true.
        LocalError = .FALSE.
      End If
      HeatingCoilOutletNode = GetDXCoilOutletNode(Alphas(10),Alphas(11),LocalError)
      IF(LocalError) Then
        CALL ShowSevereError('The outlet node number of '//TRIM(cAlphaFields(11))//' is not found "'//TRIM(Alphas(11))//'"')
        CALL ShowContinueError('...occurs in '//TRIM(CurrentModuleObject)//' "'//TRIM(Alphas(1))//'"')
        ErrorsFound=.true.
        LocalError = .FALSE.
      End If
      CALL SetUpCompSets(CurrentModuleObject, MSHeatPump(MSHPNum)%Name, &
                         'Coil:Heating:DX:MultiSpeed',MSHeatPump(MSHPNum)%DXHeatCoilName,'UNDEFINED', 'UNDEFINED')
    Else if (SameString(Alphas(10),'Coil:Heating:Electric:MultiStage') .OR. &
             SameString(Alphas(10),'Coil:Heating:Gas:MultiStage') ) THEN

      IF (SameString(Alphas(10),'Coil:Heating:Electric:MultiStage')) THEN
        MSHeatPump(MSHPNum)%HeatCoilType = Coil_HeatingElectric_MultiStage
        MSHeatPump(MSHPNum)%HeatCoilNum = GetObjectItemNum('Coil:Heating:Electric:MultiStage',Alphas(11))
        If (MSHeatPump(MSHPNum)%HeatCoilNum <= 0) then
          CALL ShowSevereError('Configuration error in '//TRIM(CurrentModuleObject)//' "'//TRIM(Alphas(1))//'"')
          CALL ShowContinueError(TRIM(cAlphaFields(11))//' "'//TRIM(Alphas(11))//'" not found.')
          CALL ShowContinueError(TRIM(cAlphaFields(10))//' must be Coil:Heating:Electric:MultiStage ')
          CALL ShowFatalError(RoutineName//'Errors found in getting '//TRIM(CurrentModuleObject)//' input. '//&
                              'Preceding condition(s) causes termination.')
          ErrorsFound=.true.
        End If
      ELSE
        MSHeatPump(MSHPNum)%HeatCoilType = Coil_HeatingGas_MultiStage
        MSHeatPump(MSHPNum)%HeatCoilNum = GetObjectItemNum('Coil:Heating:Gas:MultiStage',Alphas(11))
        If (MSHeatPump(MSHPNum)%HeatCoilNum <= 0) then
          CALL ShowSevereError('Configuration error in '//TRIM(CurrentModuleObject)//' "'//TRIM(Alphas(1))//'"')
          CALL ShowContinueError(TRIM(cAlphaFields(11))//' "'//TRIM(Alphas(11))//'" not found.')
          CALL ShowContinueError(TRIM(cAlphaFields(10))//' must be Coil:Heating:Gas:MultiStage ')
          CALL ShowFatalError(RoutineName//'Errors found in getting '//TRIM(CurrentModuleObject)//' input. '//&
                              'Preceding condition(s) causes termination.')
          ErrorsFound=.true.
        End If
      ENDIF
      MSHeatPump(MSHPNum)%HeatCoilName = Alphas(11)
      LocalError = .FALSE.
      IF (SameString(Alphas(10),'Coil:Heating:Electric:MultiStage')) THEN
        CALL GetCoilIndex(MSHeatPump(MSHPNum)%HeatCoilName,MSHeatPump(MSHPNum)%HeatCoilIndex, LocalError)
      ELSE
        CALL GetCoilIndex(MSHeatPump(MSHPNum)%HeatCoilName,MSHeatPump(MSHPNum)%HeatCoilIndex, LocalError)
      ENDIF
      IF(LocalError) Then
        CALL ShowSevereError('The index of '//TRIM(cAlphaFields(11))//' is not found "'//TRIM(Alphas(11))//'"')
        CALL ShowContinueError('...occurs in '//TRIM(CurrentModuleObject)//' "'//TRIM(Alphas(1))//'"')
        ErrorsFound=.true.
        LocalError = .FALSE.
      End If
      HeatingCoilInletNode = GetCoilInletNode(Alphas(10),Alphas(11),LocalError)
      IF(LocalError) Then
        CALL ShowSevereError('The inlet node number of '//TRIM(cAlphaFields(11))//' is not found "'//TRIM(Alphas(11))//'"')
        CALL ShowContinueError('...occurs in '//TRIM(CurrentModuleObject)//' "'//TRIM(Alphas(1))//'"')
        ErrorsFound=.true.
        LocalError = .FALSE.
      End If
      HeatingCoilOutletNode = GetCoilOutletNode(Alphas(10),Alphas(11),LocalError)
      IF(LocalError) Then
        CALL ShowSevereError('The outlet node number of '//TRIM(cAlphaFields(11))//' is not found "'//TRIM(Alphas(11))//'"')
        CALL ShowContinueError('...occurs in '//TRIM(CurrentModuleObject)//' "'//TRIM(Alphas(1))//'"')
        ErrorsFound=.true.
        LocalError = .FALSE.
      End If
      IF (SameString(Alphas(10),'Coil:Heating:Electric:MultiStage')) THEN
        CALL SetUpCompSets(CurrentModuleObject, MSHeatPump(MSHPNum)%Name, &
                           'Coil:Heating:Electric:MultiStage',MSHeatPump(MSHPNum)%HeatCoilName,'UNDEFINED', 'UNDEFINED')
      ELSE
        CALL SetUpCompSets(CurrentModuleObject, MSHeatPump(MSHPNum)%Name, &
                           'Coil:Heating:Gas:MultiStage',MSHeatPump(MSHPNum)%HeatCoilName,'UNDEFINED', 'UNDEFINED')
      ENDIF
    ELSEIF (SameString(Alphas(10),'Coil:Heating:Water')) THEN
        MSHeatPump(MSHPNum)%HeatCoilType = Coil_HeatingWater
        CALL ValidateComponent(Alphas(10),Alphas(11),IsNotOK,TRIM(CurrentModuleObject))
        IF (IsNotOK) THEN
            CALL ShowContinueError('...occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
            ErrorsFound=.TRUE.
        ELSE ! mine data from heating coil object

          MSHeatPump(MSHPNum)%HeatCoilName = Alphas(11)
          ! Get the Heating Coil water Inlet or control Node number
            ErrFlag = .FALSE.
            MSHeatPump(MSHPNum)%CoilControlNode = GetCoilWaterInletNode('Coil:Heating:Water', &
                                                           MSHeatPump(MSHPNum)%HeatCoilName,ErrFlag)
            IF(ErrFlag)THEN
                CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(MSHeatPump(MSHPNum)%Name))
                ErrorsFound = .TRUE.
            END IF

            ! Get the ReHeat Coil hot water max volume flow rate
            ErrFlag = .FALSE.
            MSHeatPump(MSHPNum)%MaxCoilFluidFlow = GetCoilMaxWaterFlowRate('Coil:Heating:Water',  &
                                                        MSHeatPump(MSHPNum)%HeatCoilName,ErrFlag)
            IF(ErrFlag)THEN
                CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(MSHeatPump(MSHPNum)%Name))
                ErrorsFound = .TRUE.
            END IF

            ! Get the lemental Heating Coil Inlet Node
            ErrFlag = .FALSE.
            HeatingCoilInletNode =  GetWaterCoilInletNode('Coil:Heating:Water',MSHeatPump(MSHPNum)%HeatCoilName,ErrFlag)
            MSHeatPump(MSHPNum)%CoilAirInletNode = HeatingCoilInletNode
            IF(ErrFlag)THEN
                CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(MSHeatPump(MSHPNum)%Name))
                ErrorsFound = .TRUE.
            END IF

            ! Get the lemental Heating Coil Outlet Node
            ErrFlag = .FALSE.
            HeatingCoilOutletNode = GetWaterCoilOutletNode('Coil:Heating:Water',MSHeatPump(MSHPNum)%HeatCoilName,ErrFlag)
            IF(ErrFlag)THEN
                CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(MSHeatPump(MSHPNum)%Name))
                ErrorsFound = .TRUE.
            END IF
            CALL SetUpCompSets(CurrentModuleObject, MSHeatPump(MSHPNum)%Name, &
                               'Coil:Heating:Water',MSHeatPump(MSHPNum)%HeatCoilName, &
                                NodeID(HeatingCoilInletNode), NodeID(HeatingCoilOutletNode))
        ENDIF
    ELSEIF (SameString(Alphas(10),'Coil:Heating:Steam')) THEN
        MSHeatPump(MSHPNum)%HeatCoilType = Coil_HeatingSteam
        CALL ValidateComponent(Alphas(10),Alphas(11),IsNotOK,TRIM(CurrentModuleObject))
        IF (IsNotOK) THEN
            CALL ShowContinueError('...occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(MSHeatPump(MSHPNum)%Name))
            ErrorsFound=.TRUE.
        ELSE ! mine data from heating coil object

            MSHeatPump(MSHPNum)%HeatCoilName = Alphas(11)
            ErrFlag = .FALSE.
            MSHeatPump(MSHPNum)%HeatCoilNum = GetSTeamCoilIndex(Alphas(10),MSHeatPump(MSHPNum)%HeatCoilName,ErrFlag)
            IF (MSHeatPump(MSHPNum)%HeatCoilNum .EQ. 0) THEN
                CALL ShowSevereError(TRIM(CurrentModuleObject)//' illegal '//TRIM(cAlphaFields(10))//' = ' &
                                //TRIM(MSHeatPump(MSHPNum)%HeatCoilName))
                CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(MSHeatPump(MSHPNum)%Name))
                ErrorsFound = .TRUE.
            END IF

            ! Get the lemental Heating Coil steam inlet node number
            ErrFlag = .FALSE.
            MSHeatPump(MSHPNum)%CoilControlNode = GetSteamCoilSteamInletNode('Coil:Heating:Steam', &
                                                      MSHeatPump(MSHPNum)%HeatCoilName,ErrFlag)
            IF(ErrFlag)THEN
                CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(MSHeatPump(MSHPNum)%Name))
                ErrorsFound = .TRUE.
            END IF

            ! Get the lemental Heating Coil steam max volume flow rate
            MSHeatPump(MSHPNum)%MaxCoilFluidFlow = GetCoilMaxSteamFlowRate(MSHeatPump(MSHPNum)%HeatCoilNum,ErrFlag)
            IF (MSHeatPump(MSHPNum)%MaxCoilFluidFlow .GT. 0.0d0)THEN
                SteamIndex = 0      ! Function GetSatDensityRefrig will look up steam index if 0 is passed
                SteamDensity=GetSatDensityRefrig("STEAM",TempSteamIn,1.0d0,SteamIndex,'GetMSHeatPumpInput')
                MSHeatPump(MSHPNum)%MaxCoilFluidFlow = MSHeatPump(MSHPNum)%MaxCoilFluidFlow * SteamDensity
            END IF

            ! Get the lemental Heating Coil Inlet Node
            ErrFlag = .FALSE.
            HeatingCoilInletNode = &
                GetSteamCoilAirInletNode(MSHeatPump(MSHPNum)%HeatCoilNum,MSHeatPump(MSHPNum)%HeatCoilName,ErrFlag)
            MSHeatPump(MSHPNum)%CoilAirInletNode = HeatingCoilInletNode
            IF(ErrFlag)THEN
                CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(MSHeatPump(MSHPNum)%Name))
                ErrorsFound = .TRUE.
            END IF

            ! Get the lemental Heating Coil Outlet Node
            ErrFlag = .FALSE.
            HeatingCoilOutletNode = &
                GetSteamCoilAirOutletNode(MSHeatPump(MSHPNum)%HeatCoilNum,MSHeatPump(MSHPNum)%HeatCoilName,ErrFlag)
            IF(ErrFlag)THEN
                CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(MSHeatPump(MSHPNum)%Name))
                ErrorsFound = .TRUE.
            END IF

            CALL SetUpCompSets(CurrentModuleObject, MSHeatPump(MSHPNum)%Name, &
                               'Coil:Heating:Steam',MSHeatPump(MSHPNum)%HeatCoilName, &
                                NodeID(HeatingCoilInletNode), NodeID(HeatingCoilOutletNode))

        ENDIF
    ELSE
      CALL ShowSevereError('The allowed '//TRIM(cAlphaFields(10))//' are Coil:Heating:DX:MultiSpeed, '// &
                           'Coil:Heating:Electric:MultiStage, and Coil:Heating:Gas:MultiStage  '// &
                           'in '//TRIM(CurrentModuleObject)//' "'//TRIM(Alphas(1))//'"')
      CALL ShowContinueError('The entered '//TRIM(cAlphaFields(10))//' = "'//TRIM(Alphas(10))//'".')
      ErrorsFound=.true.
    ENDIF

    MSHeatPump(MSHPNum)%MinOATCompressor = Numbers(1)
    If (Numbers(1) < -20.0d0) then
      CALL ShowSevereError(TRIM(CurrentModuleObject)//', "'//TRIM(MSHeatPump(MSHPNum)%Name)//&
                          '", '//TRIM(cNumericFields(1))//' is -20.0')
      CALL ShowContinueError('The input value is '//RoundSigDigits(Numbers(4),2))
      ErrorsFound=.true.
    End If

    IF (SameString(Alphas(12),'Coil:Cooling:DX:MultiSpeed')) THEN
      MSHeatPump(MSHPNum)%CoolCoilType = MultiSpeedCoolingCoil
      MSHeatPump(MSHPNum)%CoolCoilNum = GetObjectItemNum('Coil:Cooling:DX:MultiSpeed',Alphas(13))
      MSHeatPump(MSHPNum)%DXCoolCoilName = Alphas(13)
      If (MSHeatPump(MSHPNum)%CoolCoilNum <= 0) then
        CALL ShowSevereError('Configuration error in '//TRIM(CurrentModuleObject)//' "'//TRIM(Alphas(1))//'"')
        CALL ShowContinueError(TRIM(cAlphaFields(13))//' "'//TRIM(Alphas(13))//'" not found.')
        CALL ShowContinueError(TRIM(cAlphaFields(12))//' must be Coil:Cooling:DX:MultiSpeed ')
        CALL ShowFatalError(RoutineName//'Errors found in getting '//TRIM(CurrentModuleObject)//' input. '//&
                            'Preceding condition(s) causes termination.')
        ErrorsFound=.true.
      End If
      LocalError = .FALSE.
      CALL GetDXCoilIndex(MSHeatPump(MSHPNum)%DXCoolCoilName,MSHeatPump(MSHPNum)%DXCoolCoilIndex, &
                              LocalError, 'Coil:Cooling:DX:MultiSpeed')
      IF(LocalError) Then
        CALL ShowSevereError('The index of '//TRIM(cAlphaFields(13))//' is not found "'//TRIM(Alphas(13))//'"')
        CALL ShowContinueError('...occurs in '//TRIM(CurrentModuleObject)//' "'//TRIM(Alphas(1))//'"')
        ErrorsFound=.true.
        LocalError = .FALSE.
      End If
      CoolingCoilInletNode = GetDXCoilInletNode(Alphas(12),Alphas(13),LocalError)
      IF(LocalError) Then
        CALL ShowSevereError('The inlet node number of '//TRIM(cAlphaFields(13))//' is not found "'//TRIM(Alphas(13))//'"')
        CALL ShowContinueError('...occurs in '//TRIM(CurrentModuleObject)//' "'//TRIM(Alphas(1))//'"')
        ErrorsFound=.true.
        LocalError = .FALSE.
      End If
      CoolingCoilOutletNode = GetDXCoilOutletNode(Alphas(12),Alphas(13),LocalError)
      IF(LocalError) Then
        CALL ShowSevereError('The outlet node number of '//TRIM(cAlphaFields(13))//' is not found "'//TRIM(Alphas(13))//'"')
        CALL ShowContinueError('...occurs in '//TRIM(CurrentModuleObject)//' "'//TRIM(Alphas(1))//'"')
        ErrorsFound=.true.
        LocalError = .FALSE.
      End If
    Else
      CALL ShowSevereError('The allowed '//TRIM(cAlphaFields(12))//' is Coil:Cooling:DX:MultiSpeed '// &
                           'in '//TRIM(CurrentModuleObject)//' "'//TRIM(Alphas(1))//'"')
      CALL ShowContinueError('The entered '//TRIM(cAlphaFields(12))//' = "'//TRIM(Alphas(12))//'".')
      ErrorsFound=.true.
    End If
    CALL SetUpCompSets(CurrentModuleObject, MSHeatPump(MSHPNum)%Name, &
                       'Coil:Cooling:DX:MultiSpeed',MSHeatPump(MSHPNum)%DXCoolCoilName,'UNDEFINED', 'UNDEFINED')

    ! Get supplemental heating coil data
    MSHeatPump(MSHPNum)%SuppHeatCoilName = Alphas(15)
    IF (SameString(Alphas(14),'Coil:Heating:Gas')) THEN
      MSHeatPump(MSHPNum)%SuppHeatCoilType = SuppHeatingCoilGas
      errFlag=.false.
      MSHeatPump(MSHPNum)%SuppHeatCoilNum = GetHeatingCoilIndex('Coil:Heating:Gas',Alphas(15),errFlag)
      IF (MSHeatPump(MSHPNum)%SuppHeatCoilNum <= 0 .or. errFlag) then
        CALL ShowContinueError('Configuration error in '//TRIM(CurrentModuleObject)//' "'//TRIM(Alphas(1))//'"')
        CALL ShowContinueError(TRIM(cAlphaFields(15))//' of type Coil:Heating:Gas "'//TRIM(Alphas(15))//'" not found.')
        ErrorsFound=.true.
      End If

        ! Get the Supplemental Heating Coil Node Numbers
        LocalError = .FALSE.
        SuppHeatCoilInletNode = GetHeatingCoilInletNode(Alphas(14),Alphas(15),LocalError)
        IF(LocalError) Then
          CALL ShowSevereError('The inlet node number of '//TRIM(cAlphaFields(15))//' is not found "'//TRIM(Alphas(15))//'"')
          CALL ShowContinueError('...occurs in '//TRIM(CurrentModuleObject)//' "'//TRIM(Alphas(1))//'"')
          ErrorsFound=.true.
          LocalError = .FALSE.
        End If
        SuppHeatCoilOutletNode = GetHeatingCoilOutletNode(Alphas(14),Alphas(15),LocalError)
        IF(LocalError) Then
          CALL ShowSevereError('The outlet node number of '//TRIM(cAlphaFields(15))//' is not found "'//TRIM(Alphas(15))//'"')
          CALL ShowContinueError('...occurs in '//TRIM(CurrentModuleObject)//' "'//TRIM(Alphas(1))//'"')
          ErrorsFound=.true.
          LocalError = .FALSE.
        End If

        ! Get supplemental heating coil capacity to see if it is autosize
        MSHeatPump(MSHPNum)%DesignSuppHeatingCapacity = GetHeatingCoilCapacity(Alphas(14),Alphas(15),LocalError)
        IF(LocalError) Then
          CALL ShowSevereError('The capacity '//TRIM(cAlphaFields(15))//' is not found "'//TRIM(Alphas(15))//'"')
          CALL ShowContinueError('...occurs in '//TRIM(CurrentModuleObject)//' "'//TRIM(Alphas(1))//'"')
          ErrorsFound=.true.
          LocalError = .FALSE.
        End If
      CALL SetUpCompSets(CurrentModuleObject, MSHeatPump(MSHPNum)%Name, &
                         'Coil:Heating:Gas',MSHeatPump(MSHPNum)%SuppHeatCoilName,'UNDEFINED', 'UNDEFINED')
    End If
    IF (SameString(Alphas(14),'Coil:Heating:Electric')) THEN
      MSHeatPump(MSHPNum)%SuppHeatCoilType = SuppHeatingCoilElec
      errFlag=.false.
      MSHeatPump(MSHPNum)%SuppHeatCoilNum = GetHeatingCoilIndex('Coil:Heating:Electric',Alphas(15),errFlag)
      IF (MSHeatPump(MSHPNum)%SuppHeatCoilNum <= 0 .or. errFlag) then
        CALL ShowContinueError('Configuration error in '//TRIM(CurrentModuleObject)//' "'//TRIM(Alphas(1))//'"')
        CALL ShowContinueError(TRIM(cAlphaFields(15))//' of type Coil:Heating:Electric "'//TRIM(Alphas(15))//'" not found.')
        ErrorsFound=.true.
      End If

        ! Get the Supplemental Heating Coil Node Numbers
        LocalError = .FALSE.
        SuppHeatCoilInletNode = GetHeatingCoilInletNode(Alphas(14),Alphas(15),LocalError)
        IF(LocalError) Then
          CALL ShowSevereError('The inlet node number of '//TRIM(cAlphaFields(15))//' is not found "'//TRIM(Alphas(15))//'"')
          CALL ShowContinueError('...occurs in '//TRIM(CurrentModuleObject)//' "'//TRIM(Alphas(1))//'"')
          ErrorsFound=.true.
          LocalError = .FALSE.
        End If
        SuppHeatCoilOutletNode = GetHeatingCoilOutletNode(Alphas(14),Alphas(15),LocalError)
        IF(LocalError) Then
          CALL ShowSevereError('The outlet node number of '//TRIM(cAlphaFields(15))//' is not found "'//TRIM(Alphas(15))//'"')
          CALL ShowContinueError('...occurs in '//TRIM(CurrentModuleObject)//' "'//TRIM(Alphas(1))//'"')
          ErrorsFound=.true.
          LocalError = .FALSE.
        End If

        ! Get supplemental heating coil capacity to see if it is autosize
        MSHeatPump(MSHPNum)%DesignSuppHeatingCapacity = GetHeatingCoilCapacity(Alphas(14),Alphas(15),LocalError)
        IF(LocalError) Then
          CALL ShowSevereError('The capacity '//TRIM(cAlphaFields(15))//' is not found "'//TRIM(Alphas(15))//'"')
          CALL ShowContinueError('...occurs in '//TRIM(CurrentModuleObject)//' "'//TRIM(Alphas(1))//'"')
          ErrorsFound=.true.
          LocalError = .FALSE.
        End If

      CALL SetUpCompSets(CurrentModuleObject, MSHeatPump(MSHPNum)%Name, &
                         'Coil:Heating:Electric',MSHeatPump(MSHPNum)%SuppHeatCoilName,'UNDEFINED', 'UNDEFINED')
    End If

    IF (SameString(Alphas(14),'Coil:Heating:Water')) THEN
        MSHeatPump(MSHPNum)%SuppHeatCoilType = Coil_HeatingWater
        CALL ValidateComponent(Alphas(14),MSHeatPump(MSHPNum)%SuppHeatCoilName,IsNotOK,TRIM(CurrentModuleObject))
        IF (IsNotOK) THEN
            CALL ShowContinueError('...occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
            ErrorsFound=.TRUE.
        ELSE ! mine data from heating coil object

            ! Get the Heating Coil water Inlet or control Node number
            ErrFlag = .FALSE.
            MSHeatPump(MSHPNum)%SuppCoilControlNode = GetCoilWaterInletNode('Coil:Heating:Water', &
                                                           MSHeatPump(MSHPNum)%SuppHeatCoilName,ErrFlag)
            IF(ErrFlag)THEN
                CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(MSHeatPump(MSHPNum)%Name))
                ErrorsFound = .TRUE.
            END IF

            ! Get the ReHeat Coil hot water max volume flow rate
            ErrFlag = .FALSE.
            MSHeatPump(MSHPNum)%MaxSuppCoilFluidFlow = GetCoilMaxWaterFlowRate('Coil:Heating:Water',  &
                                                        MSHeatPump(MSHPNum)%SuppHeatCoilName,ErrFlag)
            IF(ErrFlag)THEN
                CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(MSHeatPump(MSHPNum)%Name))
                ErrorsFound = .TRUE.
            END IF

            ! Get the Supplemental Heating Coil Inlet Node
            ErrFlag = .FALSE.
            SuppHeatCoilInletNode =  GetWaterCoilInletNode('Coil:Heating:Water',MSHeatPump(MSHPNum)%SuppHeatCoilName,ErrFlag)
            MSHeatPump(MSHPNum)%SuppCoilAirInletNode = SuppHeatCoilInletNode
            IF(ErrFlag)THEN
                CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(MSHeatPump(MSHPNum)%Name))
                ErrorsFound = .TRUE.
            END IF

            ! Get the Supplemental Heating Coil Outlet Node
            ErrFlag = .FALSE.
            SuppHeatCoilOutletNode = GetWaterCoilOutletNode('Coil:Heating:Water',MSHeatPump(MSHPNum)%SuppHeatCoilName,ErrFlag)
            MSHeatPump(MSHPNum)%SuppCoilAirOutletNode = SuppHeatCoilOutletNode
            IF(ErrFlag)THEN
                CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(MSHeatPump(MSHPNum)%Name))
                ErrorsFound = .TRUE.
            END IF
            CALL SetUpCompSets(CurrentModuleObject, MSHeatPump(MSHPNum)%Name, &
                               'Coil:Heating:Water',MSHeatPump(MSHPNum)%SuppHeatCoilName, &
                                NodeID(SuppHeatCoilInletNode), NodeID(SuppHeatCoilOutletNode))
        ENDIF
    ENDIF
    IF (SameString(Alphas(14),'Coil:Heating:Steam')) THEN
        MSHeatPump(MSHPNum)%SuppHeatCoilType = Coil_HeatingSteam
        CALL ValidateComponent(Alphas(14),MSHeatPump(MSHPNum)%SuppHeatCoilName,IsNotOK,TRIM(CurrentModuleObject))
        IF (IsNotOK) THEN
            CALL ShowContinueError('...occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(MSHeatPump(MSHPNum)%Name))
            ErrorsFound=.TRUE.
        ELSE ! mine data from heating coil object

            ErrFlag = .FALSE.
            MSHeatPump(MSHPNum)%SuppHeatCoilNum = GetSTeamCoilIndex(Alphas(14),MSHeatPump(MSHPNum)%SuppHeatCoilName,ErrFlag)
            IF (MSHeatPump(MSHPNum)%SuppHeatCoilNum .EQ. 0) THEN
                CALL ShowSevereError(TRIM(CurrentModuleObject)//' illegal '//TRIM(cAlphaFields(14))//' = ' &
                                //TRIM(MSHeatPump(MSHPNum)%SuppHeatCoilName))
                CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(MSHeatPump(MSHPNum)%Name))
                ErrorsFound = .TRUE.
            END IF

            ! Get the Supplemental Heating Coil steam inlet node number
            ErrFlag = .FALSE.
            MSHeatPump(MSHPNum)%SuppCoilControlNode = GetSteamCoilSteamInletNode('Coil:Heating:Steam', &
                                                      MSHeatPump(MSHPNum)%SuppHeatCoilName,ErrFlag)
            IF(ErrFlag)THEN
                CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(MSHeatPump(MSHPNum)%Name))
                ErrorsFound = .TRUE.
            END IF

            ! Get the Supplemental Heating Coil steam max volume flow rate
            MSHeatPump(MSHPNum)%MaxSuppCoilFluidFlow = GetCoilMaxSteamFlowRate(MSHeatPump(MSHPNum)%SuppHeatCoilNum,ErrFlag)
            IF (MSHeatPump(MSHPNum)%MaxSuppCoilFluidFlow .GT. 0.0d0)THEN
                SteamIndex = 0      ! Function GetSatDensityRefrig will look up steam index if 0 is passed
                SteamDensity=GetSatDensityRefrig("STEAM",TempSteamIn,1.0d0,SteamIndex,'GetMSHeatPumpInput')
                MSHeatPump(MSHPNum)%MaxSuppCoilFluidFlow = MSHeatPump(MSHPNum)%MaxSuppCoilFluidFlow * SteamDensity
            END IF

            ! Get the Supplemental Heating Coil Inlet Node
            ErrFlag = .FALSE.
            SuppHeatCoilInletNode = &
                GetSteamCoilAirInletNode(MSHeatPump(MSHPNum)%SuppHeatCoilNum,MSHeatPump(MSHPNum)%SuppHeatCoilName,ErrFlag)
            MSHeatPump(MSHPNum)%SuppCoilAirInletNode = SuppHeatCoilInletNode
            IF(ErrFlag)THEN
                CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(MSHeatPump(MSHPNum)%Name))
                ErrorsFound = .TRUE.
            END IF

            ! Get the Supplemental Heating Coil Outlet Node
            ErrFlag = .FALSE.
            SuppHeatCoilOutletNode = &
                GetSteamCoilAirOutletNode(MSHeatPump(MSHPNum)%SuppHeatCoilNum,MSHeatPump(MSHPNum)%SuppHeatCoilName,ErrFlag)
            MSHeatPump(MSHPNum)%SuppCoilAirOutletNode = SuppHeatCoilOutletNode
            IF(ErrFlag)THEN
                CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(MSHeatPump(MSHPNum)%Name))
                ErrorsFound = .TRUE.
            END IF

            CALL SetUpCompSets(CurrentModuleObject, MSHeatPump(MSHPNum)%Name, &
                               'Coil:Heating:Steam',MSHeatPump(MSHPNum)%SuppHeatCoilName, &
                                NodeID(SuppHeatCoilInletNode), NodeID(SuppHeatCoilOutletNode))

        ENDIF
    ENDIF


    If (MSHeatPump(MSHPNum)%SuppHeatCoilType .EQ. 0) then
      CALL ShowSevereError(TRIM(CurrentModuleObject)//', "'//TRIM(MSHeatPump(MSHPNum)%Name)//&
                           '", '//TRIM(cAlphaFields(14))//' is not allowed = '//TRIM(Alphas(14)))
      CALL ShowContinueError('Valid choices are Coil:Heating:Gas,Coil:Heating:Electric,Coil:Heating:Steam,or Coil:Heating:Water')
      ErrorsFound=.true.
    End If

    MSHeatPump(MSHPNum)%SuppMaxAirTemp = Numbers(2)
    MSHeatPump(MSHPNum)%SuppMaxOATemp  = Numbers(3)
    If (MSHeatPump(MSHPNum)%SuppMaxOATemp > 21.0d0) then
      CALL ShowSevereError(TRIM(CurrentModuleObject)//', "'//TRIM(MSHeatPump(MSHPNum)%Name)//&
                           '", '//TRIM(cNumericFields(3))//' is greater than 21.0')
      CALL ShowContinueError('The input value is '//RoundSigDigits(Numbers(3),2))
      ErrorsFound=.true.
    End If

    MSHeatPump(MSHPNum)%AuxOnCyclePower = Numbers(4)
    MSHeatPump(MSHPNum)%AuxOffCyclePower = Numbers(5)
    If (MSHeatPump(MSHPNum)%AuxOnCyclePower .LT. 0.0d0) then
      CALL ShowSevereError(TRIM(CurrentModuleObject)//', "'//TRIM(MSHeatPump(MSHPNum)%Name)//&
                           '", A negative value for '//TRIM(cNumericFields(4))//' is not allowed ')
      ErrorsFound=.true.
    End If
    If (MSHeatPump(MSHPNum)%AuxOffCyclePower .LT. 0.0d0) then
      CALL ShowSevereError(TRIM(CurrentModuleObject)//', "'//TRIM(MSHeatPump(MSHPNum)%Name)//&
                           '", A negative value for '//TRIM(cNumericFields(5))//' is not allowed ')
      ErrorsFound=.true.
    End If

    ! Heat recovery
    MSHeatPump(MSHPNum)%DesignHeatRecFlowRate = Numbers(6)
    IF (MSHeatPump(MSHPNum)%DesignHeatRecFlowRate > 0.0d0) then
      MSHeatPump(MSHPNum)%HeatRecActive=.True.
      MSHeatPump(MSHPNum)%DesignHeatRecMassFlowRate = RhoH2O(InitConvTemp)*MSHeatPump(MSHPNum)%DesignHeatRecFlowRate
      MSHeatPump(MSHPNum)%HeatRecInletNodeNum = &
               GetOnlySingleNode(Alphas(16),ErrorsFound,'MSHP Heat receovery',Alphas(1), &
               NodeType_Water,NodeConnectionType_Inlet, 3, ObjectIsNotParent)
      IF (MSHeatPump(MSHPNum)%HeatRecInletNodeNum == 0) THEN
        CALL ShowSevereError(TRIM(CurrentModuleObject)//', "'//TRIM(MSHeatPump(MSHPNum)%Name)//&
                             '", Missing '//TRIM(cAlphaFields(16))//'.')
        ErrorsFound=.True.
      END IF
      MSHeatPump(MSHPNum)%HeatRecOutletNodeNum   = &
               GetOnlySingleNode(Alphas(17),ErrorsFound,'MSHP Heat receovery',Alphas(1), &
               NodeType_Water,NodeConnectionType_Outlet, 3, ObjectIsNotParent)
      IF (MSHeatPump(MSHPNum)%HeatRecOutletNodeNum == 0) THEN
        CALL ShowSevereError(TRIM(CurrentModuleObject)//', "'//TRIM(MSHeatPump(MSHPNum)%Name)//&
                             '", Missing '//TRIM(cAlphaFields(17))//'.')
        ErrorsFound=.True.
      END IF
      CALL TestCompSet(CurrentModuleObject,Alphas(1),Alphas(16),Alphas(17),'MSHP Heat receovery Nodes')
    ELSE
      MSHeatPump(MSHPNum)%HeatRecActive=.False.
      MSHeatPump(MSHPNum)%DesignHeatRecMassFlowRate = 0.0d0
      MSHeatPump(MSHPNum)%HeatRecInletNodeNum   = 0
      MSHeatPump(MSHPNum)%HeatRecOutletNodeNum   = 0
      IF (.NOT. lAlphaBlanks(16) .or. .NOT. lAlphaBlanks(17)) THEN
        CALL ShowWarningError('Since '//TRIM(cNumericFields(6))//' = 0.0, heat recovery is inactive for '// &
                              TRIM(CurrentModuleObject)// ' = '//TRIM(Alphas(1)))
        CALL ShowContinueError('However, '//TRIM(cAlphaFields(16))//' or '//TRIM(cAlphaFields(17))//' was specified.')
      END IF
    End If
    MSHeatPump(MSHPNum)%MaxHeatRecOutletTemp = Numbers(7)
    IF (MSHeatPump(MSHPNum)%MaxHeatRecOutletTemp .LT. 0.0d0) THEN
      CALL ShowSevereError(TRIM(CurrentModuleObject)//', "'//TRIM(MSHeatPump(MSHPNum)%Name)//&
                           '", The value for '//TRIM(cNumericFields(7))//' is below 0.0')
      ErrorsFound=.True.
    END IF
    IF (MSHeatPump(MSHPNum)%MaxHeatRecOutletTemp .GT. 100.0d0) THEN
      CALL ShowSevereError(TRIM(CurrentModuleObject)//', "'//TRIM(MSHeatPump(MSHPNum)%Name)//&
                           '", The value for '//TRIM(cNumericFields(7))//' is above 100.0')
      ErrorsFound=.True.
    END IF

    MSHeatPump(MSHPNum)%IdleVolumeAirRate = Numbers(8)
    IF (MSHeatPump(MSHPNum)%IdleVolumeAirRate .LT. 0.0d0 .AND. MSHeatPump(MSHPNum)%IdleVolumeAirRate /= AutoSize) THEN
      CALL ShowSevereError(TRIM(CurrentModuleObject)//', "'//TRIM(MSHeatPump(MSHPNum)%Name)//&
                           '", '//TRIM(cNumericFields(8))//' cannot be less than zero.')
      ErrorsFound = .TRUE.
    END IF

!     AirFlowControl only valid if fan opmode = ContFanCycCoil
    IF (MSHeatPump(MSHPNum)%IdleVolumeAirRate .EQ. 0.0d0) THEN
      MSHeatPump(MSHPNum)%AirFlowControl = UseCompressorOnFlow
    ELSE
      MSHeatPump(MSHPNum)%AirFlowControl = UseCompressorOffFlow
    END IF

!   Initialize last mode of compressor operation
    MSHeatPump(MSHPNum)%LastMode = HeatingMode

    MSHeatPump(MSHPNum)%NumOfSpeedHeating = Numbers(9)
    If (MSHeatPump(MSHPNum)%NumOfSpeedHeating .LT. 2 .OR. MSHeatPump(MSHPNum)%NumOfSpeedHeating .GT. 4) Then
      If (MSHeatPump(MSHPNum)%HeatCoilType .EQ. MultiSpeedHeatingCoil) THEN
        CALL ShowSevereError(TRIM(CurrentModuleObject)//', The maximum '//TRIM(cNumericFields(9))//' is 4, and ' &
                             //'the minimum number is 2')
        CALL ShowContinueError('The input value is '//RoundSigDigits(Numbers(9),0))
        ErrorsFound=.true.
      End If
    End If
    MSHeatPump(MSHPNum)%NumOfSpeedCooling = Numbers(10)
    If (MSHeatPump(MSHPNum)%NumOfSpeedCooling .LT. 2 .OR. MSHeatPump(MSHPNum)%NumOfSpeedCooling .GT. 4) Then
      CALL ShowSevereError(TRIM(CurrentModuleObject)//', The maximum '//TRIM(cNumericFields(10))//' is 4, and ' &
                           //'the minimum number is 2')
      CALL ShowContinueError('The input value is '//RoundSigDigits(Numbers(10),0))
      ErrorsFound=.true.
    End If

    ! Generate a dynamic array for heating
    If (MSHeatPump(MSHPNum)%NumOfSpeedHeating .GT. 0) Then
      ALLOCATE(MSHeatPump(MSHPNum)%HeatMassFlowRate(MSHeatPump(MSHPNum)%NumOfSpeedHeating))
      ALLOCATE(MSHeatPump(MSHPNum)%HeatVolumeFlowRate(MSHeatPump(MSHPNum)%NumOfSpeedHeating))
      ALLOCATE(MSHeatPump(MSHPNum)%HeatingSpeedRatio(MSHeatPump(MSHPNum)%NumOfSpeedHeating))
      MSHeatPump(MSHPNum)%HeatingSpeedRatio = 1.0d0
      Do i=1,MSHeatPump(MSHPNum)%NumOfSpeedHeating
        MSHeatPump(MSHPNum)%HeatVolumeFlowRate(i) = Numbers(10+i)
        If (MSHeatPump(MSHPNum)%HeatCoilType .EQ. MultiSpeedHeatingCoil) THEN
          IF (MSHeatPump(MSHPNum)%HeatVolumeFlowRate(i) .LE. 0.0d0 .AND. MSHeatPump(MSHPNum)%HeatVolumeFlowRate(i) /= AutoSize) THEN
            CALL ShowSevereError(TRIM(CurrentModuleObject)//', "'//TRIM(MSHeatPump(MSHPNum)%Name)//&
                                 '", '//TRIM(cNumericFields(10+i))//' must be greater than zero.')
            ErrorsFound = .TRUE.
          End If
        End If
      End Do
      ! Ensure flow rate at high speed should be greater or equal to the flow rate at low speed
      Do i=2,MSHeatPump(MSHPNum)%NumOfSpeedHeating
        If (MSHeatPump(MSHPNum)%HeatVolumeFlowRate(i) == AutoSize) Cycle
        found = .False.
        Do j=i-1,1,-1
          If (MSHeatPump(MSHPNum)%HeatVolumeFlowRate(i) /= AutoSize) Then
            Found = .True.
            Exit
          End If
        End Do
        If (Found) Then
          If (MSHeatPump(MSHPNum)%HeatVolumeFlowRate(i) .LT. MSHeatPump(MSHPNum)%HeatVolumeFlowRate(j)) Then
            CALL ShowSevereError(TRIM(CurrentModuleObject)//', "'//TRIM(MSHeatPump(MSHPNum)%Name)//&
                                 '", '//TRIM(cNumericFields(10+i)))
            CALL ShowContinueError(' cannot be less than '//TRIM(cNumericFields(10+j)))
            ErrorsFound = .TRUE.
          End If
        End If
      End Do
    End If

    ! Generate a dynamic array for cooling
    If (MSHeatPump(MSHPNum)%NumOfSpeedCooling .GT. 0) Then
      ALLOCATE(MSHeatPump(MSHPNum)%CoolMassFlowRate(MSHeatPump(MSHPNum)%NumOfSpeedCooling))
      ALLOCATE(MSHeatPump(MSHPNum)%CoolVolumeFlowRate(MSHeatPump(MSHPNum)%NumOfSpeedCooling))
      ALLOCATE(MSHeatPump(MSHPNum)%CoolingSpeedRatio(MSHeatPump(MSHPNum)%NumOfSpeedCooling))
      MSHeatPump(MSHPNum)%CoolingSpeedRatio = 1.0d0
      Do i=1,MSHeatPump(MSHPNum)%NumOfSpeedCooling
        MSHeatPump(MSHPNum)%CoolVolumeFlowRate(i) = Numbers(14+i)
        IF (MSHeatPump(MSHPNum)%CoolVolumeFlowRate(i) .LE. 0.0d0 .AND. MSHeatPump(MSHPNum)%CoolVolumeFlowRate(i) /= AutoSize) THEN
          CALL ShowSevereError(TRIM(CurrentModuleObject)//', "'//TRIM(MSHeatPump(MSHPNum)%Name)//&
                               '", '//TRIM(cNumericFields(14+i))//' must be greater than zero.')
          ErrorsFound = .TRUE.
        End If
      End Do
      ! Ensure flow rate at high speed should be greater or equal to the flow rate at low speed
      Do i=2,MSHeatPump(MSHPNum)%NumOfSpeedCooling
        If (MSHeatPump(MSHPNum)%CoolVolumeFlowRate(i) == AutoSize) Cycle
        found = .False.
        Do j=i-1,1,-1
          If (MSHeatPump(MSHPNum)%CoolVolumeFlowRate(i) /= AutoSize) Then
            Found = .True.
            Exit
          End If
        End Do
        If (Found) Then
          If (MSHeatPump(MSHPNum)%CoolVolumeFlowRate(i) .LT. MSHeatPump(MSHPNum)%CoolVolumeFlowRate(j)) Then
            CALL ShowSevereError(TRIM(CurrentModuleObject)//', "'//TRIM(MSHeatPump(MSHPNum)%Name)//&
                                 '", '//TRIM(cNumericFields(14+i)))
            CALL ShowContinueError(' cannot be less than '//TRIM(cNumericFields(14+j)))
            ErrorsFound = .TRUE.
          End If
        End If
      End Do
    End If

    ! Check node integrity
    If (MSHeatPump(MSHPNum)%FanPlaceType == BlowThru) Then
      IF (MSHeatPump(MSHPNum)%FanInletNode /= MSHeatPump(MSHPNum)%AirInletNodeNum) THEN
        CALL ShowSevereError('For '//TRIM(CurrentModuleObject)//' "'//TRIM(MSHeatPump(MSHPNum)%Name)//'"')
        CALL ShowContinueError('When a blow through fan is specified, the fan inlet node name must be '// &
                               'the same as the '//TRIM(cAlphaFields(3)))
        CALL ShowContinueError('...Fan inlet node name           = '//TRIM(NodeID(MSHeatPump(MSHPNum)%FanInletNode)))
        CALL ShowContinueError('...'//TRIM(cAlphaFields(3))//' = ' &
                               //TRIM(NodeID(MSHeatPump(MSHPNum)%AirInletNodeNum)))
        ErrorsFound=.true.
      END IF
      IF (MSHeatPump(MSHPNum)%FanOutletNode /= CoolingCoilInletNode) THEN
        CALL ShowSevereError('For '//TRIM(CurrentModuleObject)//' "'//TRIM(MSHeatPump(MSHPNum)%Name)//'"')
        CALL ShowContinueError('When a blow through fan is specified, the fan outlet node name must be '// &
                               'the same as the cooling coil inlet node name.')
        CALL ShowContinueError('...Fan outlet node name         = '//TRIM(NodeID(MSHeatPump(MSHPNum)%FanOutletNode)))
        CALL ShowContinueError('...Cooling coil inlet node name = '//TRIM(NodeID(CoolingCoilInletNode)))
        ErrorsFound=.true.
      END IF
      IF(CoolingCoilOutletNode /= HeatingCoilInletNode) THEN
        CALL ShowSevereError('For '//TRIM(CurrentModuleObject)//' "'//TRIM(MSHeatPump(MSHPNum)%Name)//'"')
        CALL ShowContinueError('The cooling coil outlet node name must be '// &
                               'the same as the heating coil inlet node name.')
        CALL ShowContinueError('...Cooling coil outlet node name = '//TRIM(NodeID(CoolingCoilOutletNode)))
        CALL ShowContinueError('...Heating coil inlet node name  = '//TRIM(NodeID(HeatingCoilInletNode)))
        ErrorsFound=.true.
      END IF
      IF(HeatingCoilOutletNode /= SuppHeatCoilInletNode) THEN
        CALL ShowSevereError('For '//TRIM(CurrentModuleObject)//' "'//TRIM(MSHeatPump(MSHPNum)%Name)//'"')
        CALL ShowContinueError('When a blow through fan is specified, the heating coil outlet node name must be '// &
                               'the same as the reheat coil inlet node name.')
        CALL ShowContinueError('...Heating coil outlet node name = '//TRIM(NodeID(HeatingCoilOutletNode)))
        CALL ShowContinueError('...Reheat coil inlet node name   = '//TRIM(NodeID(SuppHeatCoilInletNode)))
        ErrorsFound=.true.
      END IF
      IF(SuppHeatCoilOutletNode /= MSHeatPump(MSHPNum)%AirOutletNodeNum) THEN
        CALL ShowSevereError('For '//TRIM(CurrentModuleObject)//' "'//TRIM(MSHeatPump(MSHPNum)%Name)//'"')
        CALL ShowContinueError('The supplemental heating coil outlet node name must be '// &
                               'the same as the '//TRIM(cAlphaFields(4)))
        CALL ShowContinueError('...Supplemental heating coil outlet node name   = '//TRIM(NodeID(SuppHeatCoilOutletNode)))
        CALL ShowContinueError('...'//TRIM(cAlphaFields(4))//' = ' &
                               //TRIM(NodeID(MSHeatPump(MSHPNum)%AirOutletNodeNum)))
        ErrorsFound=.true.
      END IF
    Else
      IF(CoolingCoilInletNode /= MSHeatPump(MSHPNum)%AirInletNodeNum) THEN
        CALL ShowSevereError('For '//TRIM(CurrentModuleObject)//' "'//TRIM(MSHeatPump(MSHPNum)%Name)//'"')
        CALL ShowContinueError('When a draw through fan is specified, the cooling coil inlet node name must be '// &
                               'the same as the '//TRIM(cAlphaFields(3)))
        CALL ShowContinueError('...Cooling coil inlet node name  = '//TRIM(NodeID(CoolingCoilInletNode)))
        CALL ShowContinueError('...'//TRIM(cAlphaFields(3))//' = ' &
                               //TRIM(NodeID(MSHeatPump(MSHPNum)%AirInletNodeNum)))
        ErrorsFound=.true.
      END IF
      IF(CoolingCoilOutletNode /= HeatingCoilInletNode) THEN
        CALL ShowSevereError('For '//TRIM(CurrentModuleObject)//' "'//TRIM(MSHeatPump(MSHPNum)%Name)//'"')
        CALL ShowContinueError('The cooling coil outlet node name must be '// &
                               'the same as the heating coil inlet node name.')
        CALL ShowContinueError('...Cooling coil outlet node name = '//TRIM(NodeID(CoolingCoilOutletNode)))
        CALL ShowContinueError('...Heating coil inlet node name  = '//TRIM(NodeID(HeatingCoilInletNode)))
        ErrorsFound=.true.
      END IF
      IF(HeatingCoilOutletNode /= MSHeatPump(MSHPNum)%FanInletNode) THEN
        CALL ShowSevereError('For '//TRIM(CurrentModuleObject)//' "'//TRIM(MSHeatPump(MSHPNum)%Name)//'"')
        CALL ShowContinueError('When a draw through fan is specified, the heating coil outlet node name must be '// &
                               'the same as the fan inlet node name.')
        CALL ShowContinueError('...Heating coil outlet node name = '//TRIM(NodeID(HeatingCoilOutletNode)))
        CALL ShowContinueError('...Fan inlet node name           = '//TRIM(NodeID(MSHeatPump(MSHPNum)%FanInletNode)))
        ErrorsFound=.true.
      END IF
      IF(MSHeatPump(MSHPNum)%FanOutletNode /= SuppHeatCoilInletNode) THEN
        CALL ShowSevereError('For '//TRIM(CurrentModuleObject)//' "'//TRIM(MSHeatPump(MSHPNum)%Name)//'"')
        CALL ShowContinueError('When a draw through fan is specified, the fan outlet node name must be '// &
                               'the same as the reheat coil inlet node name.')
        CALL ShowContinueError('...Fan outlet node name        = '//TRIM(NodeID(MSHeatPump(MSHPNum)%FanOutletNode)))
        CALL ShowContinueError('...Reheat coil inlet node name = '//TRIM(NodeID(SuppheatCoilInletNode)))
        ErrorsFound=.true.
      END IF
      IF(SuppHeatCoilOutletNode /= MSHeatPump(MSHPNum)%AirOutletNodeNum) THEN
        CALL ShowSevereError('For '//TRIM(CurrentModuleObject)//' "'//TRIM(MSHeatPump(MSHPNum)%Name)//'"')
        CALL ShowContinueError('The reheat coil outlet node name must be '// &
                               'the same as the '//TRIM(cAlphaFields(4)))
        CALL ShowContinueError('...Reheat coil outlet node name   = '//TRIM(NodeID(SuppHeatCoilOutletNode)))
        CALL ShowContinueError('...'//TRIM(cAlphaFields(4))//' = ' &
                               //TRIM(NodeID(MSHeatPump(MSHPNum)%AirOutletNodeNum)))
        ErrorsFound=.true.
      END IF
    End If

    ! Ensure the numbers of speeds defined in the parent object are equal to the numbers defined in coil objects
    IF (MSHeatPump(MSHPNum)%HeatCoilType .EQ. MultiSpeedHeatingCoil) THEN
      I = GetDXCoilNumberOfSpeeds(Alphas(10),Alphas(11),ErrorsFound)
      If (MSHeatPump(MSHPNum)%NumOfSpeedHeating /= I) Then
        CALL ShowSevereError('For '//TRIM(CurrentModuleObject)//' "'//TRIM(MSHeatPump(MSHPNum)%Name)//'"')
        CALL ShowContinueError('The '//TRIM(cNumericFields(9))//' is not equal to the number defined in '// &
                               TRIM(cAlphaFields(11))//' = '//TRIM(Alphas(11)))
        ErrorsFound=.true.
      End If
    ELSEIF (MSHeatPump(MSHPNum)%HeatCoilType .EQ. Coil_HeatingElectric_MultiStage .OR. &
           MSHeatPump(MSHPNum)%HeatCoilType .EQ. Coil_HeatingGas_MultiStage) THEN
      I = GetHeatingCoilNumberOfStages(Alphas(10),Alphas(11),ErrorsFound)
      If (MSHeatPump(MSHPNum)%NumOfSpeedHeating /= I) Then
        CALL ShowSevereError('For '//TRIM(CurrentModuleObject)//' "'//TRIM(MSHeatPump(MSHPNum)%Name)//'"')
        CALL ShowContinueError('The '//TRIM(cNumericFields(9))//' is not equal to the number defined in '// &
                               TRIM(cAlphaFields(11))//' = '//TRIM(Alphas(11)))
        ErrorsFound=.true.
      End If
    ENDIF
    I = GetDXCoilNumberOfSpeeds(Alphas(12),Alphas(13),ErrorsFound)
    If (MSHeatPump(MSHPNum)%NumOfSpeedCooling /= I) Then
      CALL ShowSevereError('For '//TRIM(CurrentModuleObject)//' "'//TRIM(MSHeatPump(MSHPNum)%Name)//'"')
      CALL ShowContinueError('The '//TRIM(cNumericFields(10))//' is not equal to the number defined in '// &
                             TRIM(cAlphaFields(13))//' = '//TRIM(Alphas(13)))
      ErrorsFound=.true.
    End If
  END DO

  IF (ErrorsFound) THEN
    CALL ShowFatalError(RoutineName//'Errors found in getting '//TRIM(CurrentModuleObject)//' input.  '//&
                        'Preceding condition(s) causes termination.')
  END IF
  ! End of multispeed heat pump

  DO MSHPNum = 1 , NumMSHeatPumps
  ! Setup Report Variables for MSHP Equipment
    CALL SetupOutputVariable('Unitary System Ancillary Electric Power [W]',MSHeatPump(MSHPNum)%AuxElecPower, &
                             'System','Average',MSHeatPump(MSHPNum)%Name)
    CALL SetupOutputVariable('Unitary System Cooling Ancillary Electric Energy [J]', &
                             MSHeatPumpReport(MSHPNum)%AuxElecCoolConsumption,'System','Sum',MSHeatPump(MSHPNum)%Name, &
                              ResourceTypeKey='Electric',EndUseKey='Cooling',GroupKey='System')
    CALL SetupOutputVariable('Unitary System Heating Ancillary Electric Energy [J]', &
                             MSHeatPumpReport(MSHPNum)%AuxElecHeatConsumption,'System','Sum',MSHeatPump(MSHPNum)%Name, &
                              ResourceTypeKey='Electric',EndUseKey='Heating',GroupKey='System')
    CALL SetupOutputVariable('Unitary System Fan Part Load Ratio []',MSHeatPump(MSHPNum)%FanPartLoadRatio, &
                             'System','Average',MSHeatPump(MSHPNum)%Name)
    CALL SetupOutputVariable('Unitary System Compressor Part Load Ratio []',MSHeatPump(MSHPNum)%CompPartLoadRatio, &
                             'System','Average',MSHeatPump(MSHPNum)%Name)
    CALL SetupOutputVariable('Unitary System Electric Power [W]',MSHeatPump(MSHPNum)%ElecPower,&
                             'System','Average',MSHeatPump(MSHPNum)%Name)
    CALL SetupOutputVariable('Unitary System Electric Energy [J]',MSHeatPumpReport(MSHPNum)%ElecPowerConsumption, &
                             'System','Sum',MSHeatPump(MSHPNum)%Name)
    CALL SetupOutputVariable('Unitary System DX Coil Cycling Ratio []',MSHeatPumpReport(MSHPNum)%CycRatio, &
                             'System','Average',MSHeatPump(MSHPNum)%Name)
    CALL SetupOutputVariable('Unitary System DX Coil Speed Ratio []',MSHeatPumpReport(MSHPNum)%SpeedRatio, &
                             'System','Average',MSHeatPump(MSHPNum)%Name)
    CALL SetupOutputVariable('Unitary System DX Coil Speed Level []',MSHeatPumpReport(MSHPNum)%SpeedNum, &
                             'System','Average',MSHeatPump(MSHPNum)%Name)
    CALL SetupOutputVariable('Unitary System Total Cooling Rate [W]',MSHeatPump(MSHPNum)%TotCoolEnergyRate, &
                             'System','Average',MSHeatPump(MSHPNum)%Name)
    CALL SetupOutputVariable('Unitary System Total Heating Rate [W]',MSHeatPump(MSHPNum)%TotHeatEnergyRate, &
                             'System','Average',MSHeatPump(MSHPNum)%Name)
    CALL SetupOutputVariable('Unitary System Sensible Cooling Rate [W]',MSHeatPump(MSHPNum)%SensCoolEnergyRate, &
                             'System','Average',MSHeatPump(MSHPNum)%Name)
    CALL SetupOutputVariable('Unitary System Sensible Heating Rate [W]',MSHeatPump(MSHPNum)%SensHeatEnergyRate, &
                             'System','Average',MSHeatPump(MSHPNum)%Name)
    CALL SetupOutputVariable('Unitary System Latent Cooling Rate [W]',MSHeatPump(MSHPNum)%LatCoolEnergyRate, &
                             'System','Average',MSHeatPump(MSHPNum)%Name)
    CALL SetupOutputVariable('Unitary System Latent Heating Rate [W]',MSHeatPump(MSHPNum)%LatHeatEnergyRate, &
                             'System','Average',MSHeatPump(MSHPNum)%Name)
    If (MSHeatPump(MSHPNum)%HeatRecActive) then
      CALL SetupOutputVariable('Unitary System Heat Recovery Rate [W]',MSHeatPump(MSHPNum)%HeatRecoveryRate, &
                             'System','Average',MSHeatPump(MSHPNum)%Name)
      CALL SetupOutputVariable('Unitary System Heat Recovery Inlet Temperature [C]', &
                             MSHeatPump(MSHPNum)%HeatRecoveryInletTemp,'System','Average',MSHeatPump(MSHPNum)%Name)
      CALL SetupOutputVariable('Unitary System Heat Recovery Outlet Temperature [C]', &
                             MSHeatPump(MSHPNum)%HeatRecoveryOutletTemp,'System','Average',MSHeatPump(MSHPNum)%Name)
      CALL SetupOutputVariable('Unitary System Heat Recovery Fluid Mass Flow Rate [kg/s]', &
                             MSHeatPump(MSHPNum)%HeatRecoveryMassFlowRate,'System','Average',MSHeatPump(MSHPNum)%Name)
      CALL SetupOutputVariable('Unitary System Heat Recovery Energy [J]', &
                             MSHeatPumpReport(MSHPNum)%HeatRecoveryEnergy,'System','Sum',MSHeatPump(MSHPNum)%Name)
    End If
  END DO

RETURN
END SUBROUTINE GetMSHeatPumpInput

!******************************************************************************

SUBROUTINE InitMSHeatPump(MSHeatPumpNum,FirstHVACIteration,AirLoopNum,QZnReq,OnOffAirFlowRatio)

            ! SUBROUTINE INFORMATION:
            !       AUTHOR:          Lixing Gu, FSEC
            !       DATE WRITTEN:    July 2007
            !       MODIFIED         Bereket Nigusse, June 2010 - added a procedure to calculate supply air flow fraction
            !                        through controlled zone
            !       RE-ENGINEERED    na

            ! PURPOSE OF THIS SUBROUTINE:
            ! This subroutine is for initializations of the multispeed heat pump (MSHP) components.

            ! METHODOLOGY EMPLOYED:
            ! Uses the status flags to trigger initializations. The MSHP system is simulated with no load (coils off) to
            ! determine the outlet temperature. A setpoint temperature is calculated on FirstHVACIteration = TRUE.
            ! Once the setpoint is calculated, the inlet mass flow rate on FirstHVACIteration = FALSE is used to
            ! determine the bypass fraction. The simulation converges quickly on mass flow rate. If the zone
            ! temperatures float in the deadband, additional iterations are required to converge on mass flow rate.

            ! METHODOLOGY EMPLOYED:
            !

            ! REFERENCES: na

            ! USE STATEMENTS:
  USE Fans,                  ONLY: GetFanIndex, GetFanVolFlow
  USE General,               ONLY: TrimSigDigits, RoundSigDigits
  USE ReportSizingManager,   ONLY: ReportSizingOutput
  USE DataSizing,            ONLY: AutoSize
  USE DataEnvironment,       ONLY: StdBaroPress
  USE Psychrometrics,        ONLY: PsyRhoAirFnPbTdbW
  USE ScheduleManager,       ONLY: GetCurrentScheduleValue
  USE DataZoneEnergyDemands, ONLY: ZoneSysEnergyDemand, CurDeadbandOrSetback
  USE DataBranchNodeConnections, ONLY: NodeConnections, NumOfNodeConnections
  USE InputProcessor,        ONLY : SameString, MakeUPPERCase
  USE DataAirLoop,           ONLY: AirLoopControlInfo
  USE DataZoneEquipment,     ONLY: ZoneEquipConfig, ZONEEQUIPLIST, AirDistUnit_Num, DirectAir_Num
  USE DataAirLoop ,          ONLY: AirToZoneNodeInfo
  USE DataPlant,             ONLY: ScanPlantLoopsForObject, TypeOf_MultiSpeedHeatPumpRecovery, &
                                   PlantLoop, TypeOf_CoilSteamAirHeating, TypeOf_CoilWaterSimpleHeating
  USE PlantUtilities,        ONLY: InitComponentNodes, SetComponentFlowRate
  USE DataGlobals,           ONLY: AnyPlantInModel
  USE FluidProperties,       ONLY: GetDensityGlycol, GetSatDensityRefrig
  USE SteamCoils,            ONLY: SimulateSteamCoilComponents, GetCoilMaxSteamFlowRate=>GetCoilMaxSteamFlowRate, &
                                   GetSteamCoilCapacity=>GetCoilCapacity
  USE WaterCoils,            ONLY: GetCoilMaxWaterFlowRate, SimulateWaterCoilComponents
  USE DataZoneControls,      ONLY: StageZoneLogic
  USE DXCoils,               ONLY: GetDXCoilAvailSchPtr

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN)    :: MSHeatPumpNum      ! Engine driven heat pump number
  LOGICAL, INTENT (IN)    :: FirstHVACIteration ! TRUE if first HVAC iteration
  INTEGER, INTENT (IN)    :: AirLoopNum         ! air loop index
  REAL(r64),    INTENT (INOUT) :: QZnReq             ! Heating/Cooling load for all served zones
  REAL(r64),    INTENT (INOUT) :: OnOffAirFlowRatio  ! Ratio of compressor ON airflow to average airflow over timestep


          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER             :: InNode                           ! Inlet node number in MSHP loop
  INTEGER             :: OutNode                          ! Outlet node number in MSHP loop
  INTEGER             :: ZoneInNode                       ! Zone inlet node number in the controlled zone for MSHP
  INTEGER             :: HeatRecInNode                    ! Inlet node number of heat recovery
  INTEGER             :: HeatRecOutNode                   ! Outlet node number of heat recovery
  REAL(r64)           :: RhoAir                           ! Air density at InNode
  LOGICAL,SAVE        :: MyOneTimeFlag = .TRUE.           ! Initialization flag
  LOGICAL, ALLOCATABLE, SAVE, DIMENSION(:) :: MyEnvrnFlag ! Used for initializations each begin environment flag
  LOGICAL, ALLOCATABLE, SAVE, DIMENSION(:) :: MySizeFlag  ! Used for sizing MSHP inputs one time
  LOGICAL, ALLOCATABLE, SAVE, DIMENSION(:) :: MyCheckFlag ! Used to obtain the zone inlet node number in the controlled zone
  LOGICAL, ALLOCATABLE, SAVE, DIMENSION(:) :: MyFlowFracFlag ! Used for calculatig flow fraction once
  LOGICAL, ALLOCATABLE, SAVE, DIMENSION(:) :: MyPlantScantFlag ! used for finding on heat recovery plant loop
  LOGICAL, ALLOCATABLE, SAVE, DIMENSION(:) :: MyStagedFlag ! used for finding on staged thermostat

  REAL(r64)           :: QSensUnitOut                     ! Output of MSHP system with coils off
  REAL(r64)           :: PartLoadFrac                     ! Part-load ratio
  INTEGER             :: ZoneNum
  INTEGER             :: i                                ! Index to speed
  INTEGER             :: NumOfSpeedCooling                ! Number of speeds for cooling
  INTEGER             :: NumOfSpeedHeating                ! Number of speeds for heating
  INTEGER             :: j,k
  REAL(r64)           :: MinHumRat                        ! Minimum humidity ratio for sensible capacity calculation (kg/kg)
  REAL(r64)           :: DeltaMassRate                    ! Difference of mass flow rate between inlet node and system outlet node

  INTEGER :: ZoneInSysIndex = 0                        ! number of zone inlet nodes counter in an airloop
  INTEGER :: NumAirLoopZones = 0                       ! number of zone inlet nodes in an air loop
  INTEGER :: ZoneInletNodeNum = 0                      ! zone inlet nodes node number
  LOGICAL :: FlowFracFlagReady = .TRUE.                ! one time flag for calculating flow fraction through controlled zone
  REAL(r64) :: SumOfMassFlowRateMax = 0.0d0              ! the sum of mass flow rates at inlet to zones in an airloop
  REAL(r64) :: CntrlZoneTerminalUnitMassFlowRateMax = 0.0d0  ! Maximum mass flow rate through controlled zone terminal unit
  LOGICAL   :: errFlag
  REAL(r64) :: rho ! local fluid density
  REAL(r64) :: MdotHR ! local temporary for heat recovery fluid mass flow rate (kg/s)
  REAL(r64) :: ZoneLoadToCoolSPSequenced
  REAL(r64) :: ZoneLoadToHeatSPSequenced
  INTEGER :: EquipNum = 0  ! local do loop index for equipment listed for a zone

  LOGICAL             :: ErrorsFound        =.FALSE.   ! flag returned from mining call
  INTEGER             :: SteamIndex         =0         ! index of steam quality for steam heating coil
  REAL(r64)           :: mdot               =0.0d0     ! local temporary for mass flow rate (kg/s)
  REAL(r64)           :: SteamDensity       =0.0d0       ! density of steam at 100C, used for steam heating coils
  REAL(r64)           :: CoilMaxVolFlowRate =0.0d0     ! coil fluid maximum volume flow rate
  REAL(r64)           :: QACTUAL            =0.0d0     ! coil actual capacity
  INTEGER             :: CoilAvailSchPtr    =0         ! DX coil availability schedule pointer

  ! FLOW
  InNode  = MSHeatPump(MSHeatPumpNum)%AirInletNodeNum
  OutNode = MSHeatPump(MSHeatPumpNum)%AirOutletNodeNum
  NumOfSpeedCooling = MSHeatPump(MSHeatPumpNum)%NumOfSpeedCooling
  NumOfSpeedHeating = MSHeatPump(MSHeatPumpNum)%NumOfSpeedHeating

  AirLoopPass = AirLoopPass + 1
  If (AirLoopPass > 2) AirLoopPass = 1
! Do the one time initializations
  IF (MyOneTimeFlag) THEN

    ALLOCATE(MyEnvrnFlag(NumMSHeatPumps))
    ALLOCATE(MySizeFlag(NumMSHeatPumps))
    ALLOCATE(MyCheckFlag(NumMSHeatPumps))
    ALLOCATE(MyFlowFracFlag(NumMSHeatPumps))
    ALLOCATE(MyPlantScantFlag(NumMSHeatPumps))
    ALLOCATE(MyStagedFlag(NumMSHeatPumps))

    MyEnvrnFlag = .TRUE.
    MySizeFlag = .TRUE.
    MyOneTimeFlag = .FALSE.
    MyCheckFlag = .TRUE.
    MyFlowFracFlag = .TRUE.
    MyPlantScantFlag = .TRUE.
    MyStagedFlag = .TRUE.
  END IF

  IF (MyPlantScantFlag(MSHeatPumpNum) .AND. ALLOCATED(PlantLoop)) THEN
    IF (MSHeatPump(MSHeatPumpNum)%HeatRecActive) THEN
      errFlag=.false.
      CALL ScanPlantLoopsForObject(MSHeatPump(MSHeatPumpNum)%Name, &
                                   TypeOf_MultiSpeedHeatPumpRecovery, &
                                   MSHeatPump(MSHeatPumpNum)%HRLoopNum, &
                                   MSHeatPump(MSHeatPumpNum)%HRLoopSideNum, &
                                   MSHeatPump(MSHeatPumpNum)%HRBranchNum, &
                                   MSHeatPump(MSHeatPumpNum)%HRCompNum, &
                                   errFlag=errFlag)
      IF (errFlag) THEN
        CALL ShowFatalError('InitMSHeatPump: Program terminated for previous conditions.')
      ENDIF

      MyPlantScantFlag(MSHeatPumpNum) = .FALSE.
    ELSE
      MyPlantScantFlag(MSHeatPumpNum) = .FALSE.
    ENDIF
    IF (MSHeatPump(MSHeatPumpNum)%HeatCoilType == Coil_HeatingWater) THEN
        errFlag=.false.
        CALL ScanPlantLoopsForObject( MSHeatPump(MSHeatPumpNum)%HeatCoilName, &
                                      TypeOf_CoilWaterSimpleHeating , &
                                      MSHeatPump(MSHeatPumpNum)%LoopNum, &
                                      MSHeatPump(MSHeatPumpNum)%LoopSide, &
                                      MSHeatPump(MSHeatPumpNum)%BranchNum, &
                                      MSHeatPump(MSHeatPumpNum)%CompNum,   &
                                      errFlag=errFlag)
        IF (errFlag) THEN
          CALL ShowFatalError('InitMSHeatPump: Program terminated for previous conditions.')
        ENDIF
        MSHeatPump(MSHeatPumpNum)%MaxCoilFluidFlow = GetCoilMaxWaterFlowRate('Coil:Heating:Water',  &
                                                   MSHeatPump(MSHeatPumpNum)%HeatCoilName,ErrorsFound)

        IF(MSHeatPump(MSHeatPumpNum)%MaxCoilFluidFlow .GT. 0.0d0)THEN
            rho = GetDensityGlycol(PlantLoop(MSHeatPump(MSHeatPumpNum)%LoopNum)%FluidName, &
                                   InitConvTemp, &
                                   PlantLoop(MSHeatPump(MSHeatPumpNum)%LoopNum)%FluidIndex, &
                                   'InitMSHeatPump')
            MSHeatPump(MSHeatPumpNum)%MaxCoilFluidFlow = GetCoilMaxWaterFlowRate('Coil:Heating:Water',  &
                                    MSHeatPump(MSHeatPumpNum)%HeatCoilName,ErrorsFound) * rho
        END IF
        ! fill outlet node for coil
        MSHeatPump(MSHeatPumpNum)%CoilOutletNode =  &
            PlantLoop(MSHeatPump(MSHeatPumpNum)%LoopNum)%LoopSide(MSHeatPump(MSHeatPumpNum)%LoopSide) &
            %Branch(MSHeatPump(MSHeatPumpNum)%BranchNum)%Comp(MSHeatPump(MSHeatPumpNum)%CompNum)%NodeNumOut
        MyPlantScantFlag(MSHeatPumpNum) = .FALSE.

    ELSEIF (MSHeatPump(MSHeatPumpNum)%HeatCoilType == Coil_HeatingSteam) THEN
        errFlag=.false.
        CALL ScanPlantLoopsForObject(MSHeatPump(MSHeatPumpNum)%HeatCoilName, &
                                     TypeOf_CoilSteamAirHeating , &
                                     MSHeatPump(MSHeatPumpNum)%LoopNum, &
                                     MSHeatPump(MSHeatPumpNum)%LoopSide, &
                                     MSHeatPump(MSHeatPumpNum)%BranchNum, &
                                     MSHeatPump(MSHeatPumpNum)%CompNum,   &
                                     errFlag=errFlag)
        IF (errFlag) THEN
          CALL ShowFatalError('InitMSHeatPump: Program terminated for previous conditions.')
        ENDIF
        MSHeatPump(MSHeatPumpNum)%MaxCoilFluidFlow =   &
           GetCoilMaxSteamFlowRate(MSHeatPump(MSHeatPumpNum)%HeatCoilNum,ErrorsFound)
        IF(MSHeatPump(MSHeatPumpNum)%MaxCoilFluidFlow .GT. 0.0d0)THEN
          SteamIndex = 0 ! Function GetSatDensityRefrig will look up steam index if 0 is passed
          SteamDensity=GetSatDensityRefrig('STEAM',TempSteamIn,1.0d0,SteamIndex,'InitMSHeatPump')
          MSHeatPump(MSHeatPumpNum)%MaxCoilFluidFlow = MSHeatPump(MSHeatPumpNum)%MaxCoilFluidFlow * SteamDensity
        END IF


      ! fill outlet node for coil
      MSHeatPump(MSHeatPumpNum)%CoilOutletNode =  &
            PlantLoop(MSHeatPump(MSHeatPumpNum)%LoopNum)%LoopSide(MSHeatPump(MSHeatPumpNum)%LoopSide) &
            %Branch(MSHeatPump(MSHeatPumpNum)%BranchNum)%Comp(MSHeatPump(MSHeatPumpNum)%CompNum)%NodeNumOut
      MyPlantScantFlag(MSHeatPumpNum) = .FALSE.

    ENDIF
    IF (MSHeatPump(MSHeatPumpNum)%SuppHeatCoilType == Coil_HeatingWater) THEN
        errFlag=.false.
        CALL ScanPlantLoopsForObject( MSHeatPump(MSHeatPumpNum)%SuppHeatCoilName, &
                                      TypeOf_CoilWaterSimpleHeating , &
                                      MSHeatPump(MSHeatPumpNum)%SuppLoopNum, &
                                      MSHeatPump(MSHeatPumpNum)%SuppLoopSide, &
                                      MSHeatPump(MSHeatPumpNum)%SuppBranchNum, &
                                      MSHeatPump(MSHeatPumpNum)%SuppCompNum,   &
                                      errFlag=errFlag)
        IF (errFlag) THEN
          CALL ShowFatalError('InitMSHeatPump: Program terminated for previous conditions.')
        ENDIF
        MSHeatPump(MSHeatPumpNum)%MaxSuppCoilFluidFlow = GetCoilMaxWaterFlowRate('Coil:Heating:Water',  &
                                                   MSHeatPump(MSHeatPumpNum)%SuppHeatCoilName,ErrorsFound)

        IF(MSHeatPump(MSHeatPumpNum)%MaxSuppCoilFluidFlow .GT. 0.0d0)THEN
            rho = GetDensityGlycol(PlantLoop(MSHeatPump(MSHeatPumpNum)%SuppLoopNum)%FluidName, &
                                   InitConvTemp, &
                                   PlantLoop(MSHeatPump(MSHeatPumpNum)%SuppLoopNum)%FluidIndex, &
                                   'InitMSHeatPump')
            MSHeatPump(MSHeatPumpNum)%MaxSuppCoilFluidFlow = GetCoilMaxWaterFlowRate('Coil:Heating:Water',  &
                                    MSHeatPump(MSHeatPumpNum)%SuppHeatCoilName,ErrorsFound) * rho
        END IF
        ! fill outlet node for coil
        MSHeatPump(MSHeatPumpNum)%SuppCoilOutletNode =  &
            PlantLoop(MSHeatPump(MSHeatPumpNum)%SuppLoopNum)%LoopSide(MSHeatPump(MSHeatPumpNum)%SuppLoopSide) &
            %Branch(MSHeatPump(MSHeatPumpNum)%SuppBranchNum)%Comp(MSHeatPump(MSHeatPumpNum)%SuppCompNum)%NodeNumOut
        MyPlantScantFlag(MSHeatPumpNum) = .FALSE.

    ELSEIF (MSHeatPump(MSHeatPumpNum)%SuppHeatCoilType == Coil_HeatingSteam) THEN
        errFlag=.false.
        CALL ScanPlantLoopsForObject(MSHeatPump(MSHeatPumpNum)%SuppHeatCoilName, &
                                     TypeOf_CoilSteamAirHeating , &
                                     MSHeatPump(MSHeatPumpNum)%SuppLoopNum, &
                                     MSHeatPump(MSHeatPumpNum)%SuppLoopSide, &
                                     MSHeatPump(MSHeatPumpNum)%SuppBranchNum, &
                                     MSHeatPump(MSHeatPumpNum)%SuppCompNum,   &
                                     errFlag=errFlag)
        IF (errFlag) THEN
          CALL ShowFatalError('InitMSHeatPump: Program terminated for previous conditions.')
        ENDIF
        MSHeatPump(MSHeatPumpNum)%MaxSuppCoilFluidFlow =   &
           GetCoilMaxSteamFlowRate(MSHeatPump(MSHeatPumpNum)%SuppHeatCoilNum,ErrorsFound)
        IF(MSHeatPump(MSHeatPumpNum)%MaxSuppCoilFluidFlow .GT. 0.0d0)THEN
          SteamIndex = 0 ! Function GetSatDensityRefrig will look up steam index if 0 is passed
          SteamDensity=GetSatDensityRefrig('STEAM',TempSteamIn,1.0d0,SteamIndex,'InitMSHeatPump')
          MSHeatPump(MSHeatPumpNum)%MaxSuppCoilFluidFlow = MSHeatPump(MSHeatPumpNum)%MaxSuppCoilFluidFlow * SteamDensity
        END IF


      ! fill outlet node for coil
      MSHeatPump(MSHeatPumpNum)%SuppCoilOutletNode =  &
            PlantLoop(MSHeatPump(MSHeatPumpNum)%SuppLoopNum)%LoopSide(MSHeatPump(MSHeatPumpNum)%SuppLoopSide) &
            %Branch(MSHeatPump(MSHeatPumpNum)%SuppBranchNum)%Comp(MSHeatPump(MSHeatPumpNum)%SuppCompNum)%NodeNumOut
      MyPlantScantFlag(MSHeatPumpNum) = .FALSE.
    ENDIF
  ELSEIF (MyPlantScantFlag(MSHeatPumpNum) .AND. .NOT. AnyPlantInModel) THEN
    MyPlantScantFlag(MSHeatPumpNum) = .FALSE.
  ENDIF


  IF ( .NOT. SysSizingCalc .AND. MySizeFlag(MSHeatPumpNum) ) THEN
    CALL GetFanVolFlow(MSHeatPump(MSHeatPumpNum)%FanNum,MSHeatPump(MSHeatPumpNum)%FanVolFlow)
    CALL SizeMSHeatPump(MSHeatPumpNum)
    MSHeatPump(MSHeatPumpNum)%FlowFraction = 1.0d0
    MySizeFlag(MSHeatPumpNum) = .FALSE.
    ! Pass the fan cycling schedule index up to the air loop. Set the air loop unitary system flag.
    AirLoopControlInfo(AirLoopNum)%CycFanSchedPtr = MSHeatPump(MSHeatPumpNum)%FanSchedPtr
    AirLoopControlInfo(AirLoopNum)%UnitarySys = .TRUE.
    AirLoopControlInfo(AirLoopNum)%FanOpMode = MSHeatPump(MSHeatPumpNum)%OpMode
  END IF

  IF (ALLOCATED(ZoneEquipConfig) .AND. MyCheckFlag(MSHeatPumpNum)) THEN
    DO i=1,NumOfZones
      IF (AirLoopNum .NE. ZoneEquipConfig(i)%AirLoopNum) CYCLE
      IF (MSHeatPump(MSHeatPumpNum)%ControlZoneNum .EQ. ZoneEquipConfig(i)%ActualZoneNum) Then
        Do j=1, ZoneEquipConfig(i)%NumInletNodes
          If (MSHeatPump(MSHeatPumpNum)%ZoneInletNode .EQ. 0) Then
            Do k=1,ZoneEquipConfig(i)%NumInletNodes
              If (ZoneEquipConfig(i)%InletNode(j) .EQ. ZoneEquipConfig(i)%AirDistUnitCool(k)%OutNode) Then
                MSHeatPump(MSHeatPumpNum)%ZoneInletNode = ZoneEquipConfig(i)%InletNode(j)
                Exit
              End If
            end do
          End If
          If (MSHeatPump(MSHeatPumpNum)%ZoneInletNode .EQ. 0) Then
            Do k=1,ZoneEquipConfig(i)%NumInletNodes
              If (ZoneEquipConfig(i)%InletNode(j) .EQ. ZoneEquipConfig(i)%AirDistUnitHeat(k)%OutNode) Then
                MSHeatPump(MSHeatPumpNum)%ZoneInletNode = ZoneEquipConfig(i)%InletNode(j)
                Exit
              End If
            End do
          End If
        End Do
        !setup furnace zone equipment sequence information based on finding an air terminal
        IF (ZoneEquipConfig(i)%EquipListIndex > 0) THEN
          DO EquipNum = 1, ZoneEquipList(ZoneEquipConfig(i)%EquipListIndex)%NumOfEquipTypes
            IF ((ZoneEquipList(ZoneEquipConfig(i)%EquipListIndex)%EquipType_Num(EquipNum) == AirDistUnit_Num) &
                .OR. (ZoneEquipList(ZoneEquipConfig(i)%EquipListIndex)%EquipType_Num(EquipNum) == DirectAir_Num) ) THEN
              MSHeatPump(MSHeatPumpNum)%ZoneSequenceCoolingNum = &
                         ZoneEquipList(ZoneEquipConfig(i)%EquipListIndex)%CoolingPriority(EquipNum)
              MSHeatPump(MSHeatPumpNum)%ZoneSequenceHeatingNum = &
                         ZoneEquipList(ZoneEquipConfig(i)%EquipListIndex)%HeatingPriority(EquipNum)
            ENDIF
          ENDDO
        ENDIF
      END IF
    END DO
    MyCheckFlag(MSHeatPumpNum) = .FALSE.
    If (MSHeatPump(MSHeatPumpNum)%ZoneInletNode .EQ. 0) Then
      CALL ShowSevereError('AirLoopHVAC:UnitaryHeatPump:AirToAir:MultiSpeed'//', "'//TRIM(MSHeatPump(MSHeatPumpNum)%Name)// &
                             '", The zone inlet node in the controlled zone (' &
                            //Trim(MSHeatPump(MSHeatPumpNum)%ControlZoneName) //') is not found.')
      CALL ShowFatalError('Subroutine InitMSHeatPump: Errors found in getting '// &
           'AirLoopHVAC:UnitaryHeatPump:AirToAir:MultiSpeed'//' input.  '//'Preceding condition(s) causes termination.')
    End If
  END IF

  ! Find the number of zones (zone Inlet Nodes) attached to an air loop from the air loop number
  NumAirLoopZones = AirToZoneNodeInfo(AirLoopNum)%NumZonesCooled + AirToZoneNodeInfo(AirLoopNum)%NumZonesHeated
  IF (ALLOCATED(AirToZoneNodeInfo) .AND. MyFlowFracFlag(MSHeatPumpNum)) THEN
      FlowFracFlagReady = .TRUE.
      ZonesLoop: DO ZoneInSysIndex = 1, NumAirLoopZones
        ! zone inlet nodes for cooling
        IF (AirToZoneNodeInfo(AirLoopNum)%NumZonesCooled > 0) THEN
           IF( AirToZoneNodeInfo(AirLoopNum)%TermUnitCoolInletNodes(ZoneInSysIndex) == -999 )THEN
               ! the data structure for the zones inlet nodes has not been filled
               FlowFracFlagReady = .FALSE.
           ENDIF
        ENDIF
        ! zone inlet nodes for heating
        IF (AirToZoneNodeInfo(AirLoopNum)%NumZonesHeated  > 0) THEN
           IF( AirToZoneNodeInfo(AirLoopNum)%TermUnitHeatInletNodes(ZoneInSysIndex) == -999 ) THEN
               ! the data structure for the zones inlet nodes has not been filled
               FlowFracFlagReady = .FALSE.
           ENDIF
        ENDIF
      END DO ZonesLoop
  ENDIF
  IF (ALLOCATED(AirToZoneNodeInfo) .AND. FlowFracFlagReady ) THEN
      SumOfMassFlowRateMax = 0.0d0  ! initialize the sum of the maximum flows
      DO ZoneInSysIndex = 1, NumAirLoopZones
         ZoneInletNodeNum = AirToZoneNodeInfo(AirLoopNum)%TermUnitCoolInletNodes(ZoneInSysIndex)
         SumOfMassFlowRateMax = SumOfMassFlowRateMax + Node(ZoneInletNodeNum)%MassFlowRateMax
         IF(AirToZoneNodeInfo(AirLoopNum)%CoolCtrlZoneNums(ZoneInSysIndex) == MSHeatPump(MSHeatPumpNum)%ControlZoneNum )THEN
            CntrlZoneTerminalUnitMassFlowRateMax = Node(ZoneInletNodeNum)%MassFlowRateMax
         ENDIF
      END DO
      IF (SumOfMassFlowRateMax /= 0.0d0 .AND. MyFlowFracFlag(MSHeatPumpNum)) THEN
          IF (CntrlZoneTerminalUnitMassFlowRateMax >= SmallAirVolFlow ) THEN
              MSHeatPump(MSHeatPumpNum)%FlowFraction = CntrlZoneTerminalUnitMassFlowRateMax/SumOfMassFlowRateMax
          ELSE
              CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(MSHeatPump(MSHeatPumpNum)%Name))
              CALL ShowContinueError(' The Fraction of Supply Air Flow That Goes Through the Controlling Zone is set to 1.')
          END IF
          CALL ReportSizingOutput(TRIM(CurrentModuleObject), MSHeatPump(MSHeatPumpNum)%Name,            &
                                  'Fraction of Supply Air Flow That Goes Through the Controlling Zone', &
                                   MSHeatPump(MSHeatPumpNum)%FlowFraction)
          MyFlowFracFlag(MSHeatPumpNum) = .FALSE.
      ENDIF
  ENDIF

! Do the Begin Environment initializations
  IF (BeginEnvrnFlag .and. MyEnvrnFlag(MSHeatPumpNum)) THEN
    RhoAir = StdRhoAir
    ! set the mass flow rates from the input volume flow rates
    Do i=1,NumOfSpeedCooling
      MSHeatPump(MSHeatPumpNum)%CoolMassFlowRate(i) = RhoAir*MSHeatPump(MSHeatPumpNum)%CoolVolumeFlowRate(i)
    End Do
    Do i=1,NumOfSpeedHeating
      MSHeatPump(MSHeatPumpNum)%HeatMassFlowRate(i) = RhoAir*MSHeatPump(MSHeatPumpNum)%HeatVolumeFlowRate(i)
    End Do
    MSHeatPump(MSHeatPumpNum)%IdleMassFlowRate = RhoAir*MSHeatPump(MSHeatPumpNum)%IdleVolumeAirRate
    ! set the node max and min mass flow rates
    Node(InNode)%MassFlowRateMax = MAX(MSHeatPump(MSHeatPumpNum)%CoolMassFlowRate(NumOfSpeedCooling), &
                                       MSHeatPump(MSHeatPumpNum)%HeatMassFlowRate(NumOfSpeedHeating))
    Node(InNode)%MassFlowRateMaxAvail = MAX(MSHeatPump(MSHeatPumpNum)%CoolMassFlowRate(NumOfSpeedCooling), &
                                            MSHeatPump(MSHeatPumpNum)%HeatMassFlowRate(NumOfSpeedHeating))
    Node(InNode)%MassFlowRateMin = 0.0d0
    Node(InNode)%MassFlowRateMinAvail = 0.0d0
    Node(OutNode) = Node(InNode)
    MSHeatPump(MSHeatPumpNum)%LoadLoss = 0.0d0

    IF ((MSHeatPump(MSHeatPumpNum)%HeatRecActive) .AND. (.NOT. MyPlantScantFlag(MSHeatPumpNum)) ) THEN

      rho = GetDensityGlycol(PlantLoop(MSHeatPump(MSHeatPumpNum)%HRLoopNum)%FluidName, &
                             60.d0, &
                             PlantLoop(MSHeatPump(MSHeatPumpNum)%HRLoopNum)%FluidIndex, &
                             'InitMSHeatPump')

      MSHeatPump(MSHeatPumpNum)%DesignHeatRecMassFlowRate = MSHeatPump(MSHeatPumpNum)%DesignHeatRecFlowRate * rho

      CALL InitComponentNodes(0.d0, MSHeatPump(MSHeatPumpNum)%DesignHeatRecMassFlowRate, &
                              MSHeatPump(MSHeatPumpNum)%HeatRecInletNodeNum, &
                              MSHeatPump(MSHeatPumpNum)%HeatRecOutletNodeNum, &
                              MSHeatPump(MSHeatPumpNum)%HRLoopNum, &
                              MSHeatPump(MSHeatPumpNum)%HRLoopSideNum, &
                              MSHeatPump(MSHeatPumpNum)%HRBranchNum, &
                              MSHeatPump(MSHeatPumpNum)%HRCompNum )
    ENDIF
    IF(MSHeatPump(MSHeatPumpNum)%CoilControlNode .GT. 0)THEN
       IF(MSHeatPump(MSHeatPumpNum)%MaxCoilFluidFlow .EQ. Autosize)THEN
          IF (MSHeatPump(MSHeatPumpNum)%HeatCoilType == Coil_HeatingWater) THEN
              CALL SimulateWaterCoilComponents(MSHeatPump(MSHeatPumpNum)%HeatCoilName,FirstHVACIteration, &
                                               MSHeatPump(MSHeatPumpNum)%HeatCoilNum)

              CoilMaxVolFlowRate = GetCoilMaxWaterFlowRate('Coil:Heating:Water',  &
                                   MSHeatPump(MSHeatPumpNum)%HeatCoilName,ErrorsFound)
              IF(CoilMaxVolFlowRate .NE. Autosize) THEN
                 rho = GetDensityGlycol(PlantLoop(MSHeatPump(MSHeatPumpNum)%LoopNum)%fluidName, &
                                        InitConvTemp, &
                                        PlantLoop(MSHeatPump(MSHeatPumpNum)%LoopNum)%fluidIndex, &
                                        'InitMSHeatPump')
                 MSHeatPump(MSHeatPumpNum)%MaxCoilFluidFlow = CoilMaxVolFlowRate * rho
              ENDIF
              Call InitComponentNodes(0.d0, MSHeatPump(MSHeatPumpNum)%MaxCoilFluidFlow, &
                                            MSHeatPump(MSHeatPumpNum)%CoilControlNode, &
                                            MSHeatPump(MSHeatPumpNum)%CoilOutletNode, &
                                            MSHeatPump(MSHeatPumpNum)%LoopNum, &
                                            MSHeatPump(MSHeatPumpNum)%LoopSide, &
                                            MSHeatPump(MSHeatPumpNum)%BranchNum, &
                                            MSHeatPump(MSHeatPumpNum)%CompNum )
          ENDIF
          IF (MSHeatPump(MSHeatPumpNum)%HeatCoilType == Coil_HeatingSteam) THEN

             CALL SimulateSteamCoilComponents(MSHeatPump(MSHeatPumpNum)%HeatCoilName, &
                                              FirstHVACIteration,    &
                                              1.0d0, & !QCoilReq, simulate any load > 0 to get max capacity of steam coil
                                              MSHeatPump(MSHeatPumpNum)%HeatCoilNum, QActual)
             CoilMaxVolFlowRate = GetCoilMaxSteamFlowRate(MSHeatPump(MSHeatPumpNum)%HeatCoilNum,ErrorsFound)

             IF(CoilMaxVolFlowRate .NE. Autosize) THEN
                SteamIndex = 0 ! Function GetSatDensityRefrig will look up steam index if 0 is passed
                SteamDensity=GetSatDensityRefrig('STEAM',TempSteamIn,1.0d0,SteamIndex,'InitMSHeatPump')
                MSHeatPump(MSHeatPumpNum)%MaxCoilFluidFlow = CoilMaxVolFlowRate * SteamDensity
             ENDIF
             CALL InitComponentNodes(0.d0, MSHeatPump(MSHeatPumpNum)%MaxCoilFluidFlow, &
                                           MSHeatPump(MSHeatPumpNum)%CoilControlNode, &
                                           MSHeatPump(MSHeatPumpNum)%CoilOutletNode, &
                                           MSHeatPump(MSHeatPumpNum)%LoopNum, &
                                           MSHeatPump(MSHeatPumpNum)%LoopSide, &
                                           MSHeatPump(MSHeatPumpNum)%BranchNum, &
                                           MSHeatPump(MSHeatPumpNum)%CompNum )
          ENDIF
       ENDIF
    ENDIF
    IF(MSHeatPump(MSHeatPumpNum)%SuppCoilControlNode .GT. 0)THEN
       IF(MSHeatPump(MSHeatPumpNum)%MaxSuppCoilFluidFlow .EQ. Autosize)THEN
          IF (MSHeatPump(MSHeatPumpNum)%SuppHeatCoilType == Coil_HeatingWater) THEN
              CALL SimulateWaterCoilComponents(MSHeatPump(MSHeatPumpNum)%SuppHeatCoilName,FirstHVACIteration, &
                                               MSHeatPump(MSHeatPumpNum)%SuppHeatCoilNum)

              CoilMaxVolFlowRate = GetCoilMaxWaterFlowRate('Coil:Heating:Water',  &
                                   MSHeatPump(MSHeatPumpNum)%SuppHeatCoilName,ErrorsFound)
              IF(CoilMaxVolFlowRate .NE. Autosize) THEN
                 rho = GetDensityGlycol(PlantLoop(MSHeatPump(MSHeatPumpNum)%SuppLoopNum)%fluidName, &
                                        InitConvTemp, &
                                        PlantLoop(MSHeatPump(MSHeatPumpNum)%SuppLoopNum)%fluidIndex, &
                                        'InitMSHeatPump')
                 MSHeatPump(MSHeatPumpNum)%MaxSuppCoilFluidFlow = CoilMaxVolFlowRate * rho
              ENDIF
              Call InitComponentNodes(0.d0, MSHeatPump(MSHeatPumpNum)%MaxSuppCoilFluidFlow, &
                                            MSHeatPump(MSHeatPumpNum)%SuppCoilControlNode, &
                                            MSHeatPump(MSHeatPumpNum)%SuppCoilOutletNode, &
                                            MSHeatPump(MSHeatPumpNum)%SuppLoopNum, &
                                            MSHeatPump(MSHeatPumpNum)%SuppLoopSide, &
                                            MSHeatPump(MSHeatPumpNum)%SuppBranchNum, &
                                            MSHeatPump(MSHeatPumpNum)%SuppCompNum )
          ENDIF
          IF (MSHeatPump(MSHeatPumpNum)%SuppHeatCoilType == Coil_HeatingSteam) THEN

             CALL SimulateSteamCoilComponents(MSHeatPump(MSHeatPumpNum)%SuppHeatCoilName, &
                                              FirstHVACIteration,    &
                                              1.0d0, & !QCoilReq, simulate any load > 0 to get max capacity of steam coil
                                              MSHeatPump(MSHeatPumpNum)%SuppHeatCoilNum, QActual)
             CoilMaxVolFlowRate = GetCoilMaxSteamFlowRate(MSHeatPump(MSHeatPumpNum)%SuppHeatCoilNum,ErrorsFound)

             IF(CoilMaxVolFlowRate .NE. Autosize) THEN
                SteamIndex = 0 ! Function GetSatDensityRefrig will look up steam index if 0 is passed
                SteamDensity=GetSatDensityRefrig('STEAM',TempSteamIn,1.0d0,SteamIndex,'InitMSHeatPump')
                MSHeatPump(MSHeatPumpNum)%MaxSuppCoilFluidFlow = CoilMaxVolFlowRate * SteamDensity
             ENDIF
             CALL InitComponentNodes(0.d0, MSHeatPump(MSHeatPumpNum)%MaxSuppCoilFluidFlow, &
                                           MSHeatPump(MSHeatPumpNum)%SuppCoilControlNode, &
                                           MSHeatPump(MSHeatPumpNum)%SuppCoilOutletNode, &
                                           MSHeatPump(MSHeatPumpNum)%SuppLoopNum, &
                                           MSHeatPump(MSHeatPumpNum)%SuppLoopSide, &
                                           MSHeatPump(MSHeatPumpNum)%SuppBranchNum, &
                                           MSHeatPump(MSHeatPumpNum)%SuppCompNum )
          ENDIF
       ENDIF
    ENDIF
    MyEnvrnFlag(MSHeatPumpNum) = .FALSE.
  END IF ! end one time inits

  IF (.NOT. BeginEnvrnFlag) THEN
    MyEnvrnFlag(MSHeatPumpNum) = .TRUE.
  END IF

! IF MSHP system was not autosized and the fan is autosized, check that fan volumetric flow rate is greater than MSHP flow rates
  IF(.NOT. DoingSizing .AND. MSHeatPump(MSHeatPumpNum)%CheckFanFlow)THEN
    CurrentModuleObject = 'AirLoopHVAC:UnitaryHeatPump:AirToAir:MultiSpeed'
    CALL GetFanVolFlow(MSHeatPump(MSHeatPumpNum)%FanNum,MSHeatPump(MSHeatPumpNum)%FanVolFlow)
    IF(MSHeatPump(MSHeatPumpNum)%FanVolFlow .NE. AutoSize)THEN
!     Check fan versus system supply air flow rates
      IF(MSHeatPump(MSHeatPumpNum)%FanVolFlow .LT. &
         MSHeatPump(MSHeatPumpNum)%CoolVolumeFlowRate(NumOfSpeedCooling))THEN
        CALL ShowWarningError(TRIM(CurrentModuleObject)//' - air flow rate = ' &
                        //TRIM(TrimSigDigits(MSHeatPump(MSHeatPumpNum)%FanVolFlow,7))//' in fan object ' &
                        //TRIM(MSHeatPump(MSHeatPumpNum)%FanName)//' is less than the MSHP system air flow rate' &
                        //' when cooling is required ('// &
        TRIM(TrimSigDigits(MSHeatPump(MSHeatPumpNum)%CoolVolumeFlowRate(NumOfSpeedCooling),7))//').')
        CALL ShowContinueError(' The MSHP system flow rate when cooling is required is reset to the' &
                              //' fan flow rate and the simulation continues.')
        CALL ShowContinueError(' Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(MSHeatPump(MSHeatPumpNum)%Name))
        MSHeatPump(MSHeatPumpNum)%CoolVolumeFlowRate(NumOfSpeedCooling) = MSHeatPump(MSHeatPumpNum)%FanVolFlow
        ! Check flow rates in other speeds and ensure flow rates are not above the max flow rate
        Do i=NumOfSpeedCooling-1,1,-1
          If (MSHeatPump(MSHeatPumpNum)%CoolVolumeFlowRate(i) .GT. MSHeatPump(MSHeatPumpNum)%CoolVolumeFlowRate(i+1)) Then
            CALL ShowContinueError(' The MSHP system flow rate when cooling is required is reset to the' &
                 //' flow rate at higher speed and the simulation continues at Speed'//TrimSigDigits(i,0)//'.')
            CALL ShowContinueError(' Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(MSHeatPump(MSHeatPumpNum)%Name))
            MSHeatPump(MSHeatPumpNum)%CoolVolumeFlowRate(i) = MSHeatPump(MSHeatPumpNum)%CoolVolumeFlowRate(i+1)
          End If
        End Do
      END IF
      IF(MSHeatPump(MSHeatPumpNum)%FanVolFlow .LT. &
         MSHeatPump(MSHeatPumpNum)%HeatVolumeFlowRate(NumOfSpeedHeating))THEN
        CALL ShowWarningError(TRIM(CurrentModuleObject)//' - air flow rate = ' &
                        //TRIM(TrimSigDigits(MSHeatPump(MSHeatPumpNum)%FanVolFlow,7))//' in fan object ' &
                        //TRIM(MSHeatPump(MSHeatPumpNum)%FanName)//' is less than the MSHP system air flow rate' &
                        //' when heating is required ('// &
          TRIM(TrimSigDigits(MSHeatPump(MSHeatPumpNum)%HeatVolumeFlowRate(NumOfSpeedHeating),7))//').')
        CALL ShowContinueError(' The MSHP system flow rate when heating is required is reset to the' &
                              //' fan flow rate and the simulation continues.')
        CALL ShowContinueError(' Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(MSHeatPump(MSHeatPumpNum)%Name))
        MSHeatPump(MSHeatPumpNum)%HeatVolumeFlowRate(NumOfSpeedHeating) = MSHeatPump(MSHeatPumpNum)%FanVolFlow
        Do i=NumOfSpeedHeating-1,1,-1
          If (MSHeatPump(MSHeatPumpNum)%HeatVolumeFlowRate(i) .GT. MSHeatPump(MSHeatPumpNum)%HeatVolumeFlowRate(i+1)) Then
            CALL ShowContinueError(' The MSHP system flow rate when heating is required is reset to the' &
                 //' flow rate at higher speed and the simulation continues at Speed'//TrimSigDigits(i,0)//'.')
            CALL ShowContinueError(' Occurs in '//TRIM(CurrentModuleObject)//' system = '//TRIM(MSHeatPump(MSHeatPumpNum)%Name))
            MSHeatPump(MSHeatPumpNum)%HeatVolumeFlowRate(i) = MSHeatPump(MSHeatPumpNum)%HeatVolumeFlowRate(i+1)
          End If
        End Do
      END IF
      IF(MSHeatPump(MSHeatPumpNum)%FanVolFlow .LT. MSHeatPump(MSHeatPumpNum)%IdleVolumeAirRate .AND. &
         MSHeatPump(MSHeatPumpNum)%IdleVolumeAirRate .NE. 0.0d0)THEN
        CALL ShowWarningError(TRIM(CurrentModuleObject)//' - air flow rate = ' &
                   //TRIM(TrimSigDigits(MSHeatPump(MSHeatPumpNum)%FanVolFlow,7))//' in fan object ' &
                   //TRIM(MSHeatPump(MSHeatPumpNum)%FanName)//' is less than the MSHP system air flow rate when no ' &
                   //'heating or cooling is needed ('//TRIM(TrimSigDigits(MSHeatPump(MSHeatPumpNum)%IdleVolumeAirRate,7))//').')
        CALL ShowContinueError(' The MSHP system flow rate when no heating or cooling is needed is reset to the' &
                              //' fan flow rate and the simulation continues.')
        CALL ShowContinueError(' Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(MSHeatPump(MSHeatPumpNum)%Name))
        MSHeatPump(MSHeatPumpNum)%IdleVolumeAirRate = MSHeatPump(MSHeatPumpNum)%FanVolFlow
      END IF
      RhoAir = StdRhoAir
      ! set the mass flow rates from the reset volume flow rates
      Do I=1,NumOfSpeedCooling
        MSHeatPump(MSHeatPumpNum)%CoolMassFlowRate(i) = RhoAir*MSHeatPump(MSHeatPumpNum)%CoolVolumeFlowRate(i)
        IF(MSHeatPump(MSHeatPumpNum)%FanVolFlow .GT. 0.0d0)THEN
          MSHeatPump(MSHeatPumpNum)%CoolingSpeedRatio(i) = &
              MSHeatPump(MSHeatPumpNum)%CoolVolumeFlowRate(i)/MSHeatPump(MSHeatPumpNum)%FanVolFlow
        END IF
      End Do
      Do I=1,NumOfSpeedHeating
        MSHeatPump(MSHeatPumpNum)%HeatMassFlowRate(i) = RhoAir*MSHeatPump(MSHeatPumpNum)%HeatVolumeFlowRate(i)
        IF(MSHeatPump(MSHeatPumpNum)%FanVolFlow .GT. 0.0d0)THEN
          MSHeatPump(MSHeatPumpNum)%HeatingSpeedRatio(i) = &
              MSHeatPump(MSHeatPumpNum)%HeatVolumeFlowRate(i)/MSHeatPump(MSHeatPumpNum)%FanVolFlow
        END IF
      End Do
      MSHeatPump(MSHeatPumpNum)%IdleMassFlowRate = RhoAir*MSHeatPump(MSHeatPumpNum)%IdleVolumeAirRate
      IF(MSHeatPump(MSHeatPumpNum)%FanVolFlow .GT. 0.0d0)THEN
        MSHeatPump(MSHeatPumpNum)%IdleSpeedRatio = &
            MSHeatPump(MSHeatPumpNum)%IdleVolumeAirRate / MSHeatPump(MSHeatPumpNum)%FanVolFlow
      END IF
      ! set the node max and min mass flow rates based on reset volume flow rates
      Node(InNode)%MassFlowRateMax = MAX(MSHeatPump(MSHeatPumpNum)%CoolMassFlowRate(NumOfSpeedCooling), &
                                         MSHeatPump(MSHeatPumpNum)%HeatMassFlowRate(NumOfSpeedHeating))
      Node(InNode)%MassFlowRateMaxAvail = MAX(MSHeatPump(MSHeatPumpNum)%CoolMassFlowRate(NumOfSpeedCooling), &
                                              MSHeatPump(MSHeatPumpNum)%HeatMassFlowRate(NumOfSpeedHeating))
      Node(InNode)%MassFlowRateMin = 0.0d0
      Node(InNode)%MassFlowRateMinAvail = 0.0d0
      Node(OutNode) = Node(InNode)
      MSHeatPump(MSHeatPumpNum)%CheckFanFlow = .FALSE.
    END IF
  END IF

  IF(MSHeatPump(MSHeatPumpNum)%FanSchedPtr .GT. 0)THEN
    IF(GetCurrentScheduleValue(MSHeatPump(MSHeatPumpNum)%FanSchedPtr) .EQ. 0.0d0)THEN
      MSHeatPump(MSHeatPumpNum)%OpMode = CycFanCycCoil
    ELSE
      MSHeatPump(MSHeatPumpNum)%OpMode = ContFanCycCoil
    END IF
  END IF

  ! Calcuate air distribution losses
  IF (.NOT. FirstHVACIteration .AND. AirLoopPass .eq. 1) Then
    ZoneInNode = MSHeatPump(MSHeatPumpNum)%ZoneInletNode
    MinHumRat = Node(ZoneInNode)%HumRat
    IF(Node(OutNode)%Temp .LT. Node(MSHeatPump(MSHeatPumpNum)%NodeNumofControlledZone)%Temp ) &
      MinHumRat = Node(OutNode)%HumRat
    DeltaMassRate = Node(OutNode)%MassFlowrate-Node(ZoneInNode)%MassFlowrate/MSHeatPump(MSHeatPumpNum)%FlowFraction
    If (DeltaMassRate .LT. 0.0d0) DeltaMassRate = 0.0d0
    MSHeatPump(MSHeatPumpNum)%LoadLoss = Node(ZoneInNode)%MassFlowrate/MSHeatPump(MSHeatPumpNum)%FlowFraction * &
      (PsyHFnTdbW(Node(OutNode)%Temp,MinHumRat) - PsyHFnTdbW(Node(ZoneInNode)%Temp,MinHumRat)) + DeltaMassRate * &
      (PsyHFnTdbW(Node(OutNode)%Temp,MinHumRat) -   &
         PsyHFnTdbW(Node(MSHeatPump(MSHeatPumpNum)%NodeNumofControlledZone)%Temp,MinHumRat))
    If (ABS(MSHeatPump(MSHeatPumpNum)%LoadLoss) .LT. 1.0d-6) MSHeatPump(MSHeatPumpNum)%LoadLoss = 0.0d0
  End If

! Returns load only for zones requesting cooling (heating). If in deadband, Qzoneload = 0.
  ZoneNum = MSHeatPump(MSHeatPumpNum)%ControlZoneNum
  IF ((MSHeatPump(MSHeatPumpNum)%ZoneSequenceCoolingNum > 0) .and. (MSHeatPump(MSHeatPumpNum)%ZoneSequenceHeatingNum > 0)) THEN
    ZoneLoadToCoolSPSequenced = ZoneSysEnergyDemand(MSHeatPump(MSHeatPumpNum)%ControlZoneNum)%&
                  SequencedOutputRequiredToCoolingSP(MSHeatPump(MSHeatPumpNum)%ZoneSequenceCoolingNum)
    ZoneLoadToHeatSPSequenced = ZoneSysEnergyDemand(MSHeatPump(MSHeatPumpNum)%ControlZoneNum)%&
                  SequencedOutputRequiredToHeatingSP(MSHeatPump(MSHeatPumpNum)%ZoneSequenceHeatingNum)
    IF (ZoneLoadToHeatSPSequenced > SmallLoad .AND. ZoneLoadToCoolSPSequenced > SmallLoad) THEN
      QZnReq = ZoneLoadToHeatSPSequenced
    ELSEIF (ZoneLoadToHeatSPSequenced < (-1.d0*SmallLoad) .AND. ZoneLoadToCoolSPSequenced < (-1.d0*SmallLoad)) THEN
      QZnReq = ZoneLoadToCoolSPSequenced
    ELSEIF (ZoneLoadToHeatSPSequenced <= (-1.d0*SmallLoad) .AND. ZoneLoadToCoolSPSequenced >= SmallLoad) THEN
      QZnReq = 0.d0
    ENDIF
    QZnReq = QZnReq/MSHeatPump(MSHeatPumpNum)%FlowFraction
  ELSE
    QZnReq = ZoneSysEnergyDemand(ZoneNum)%RemainingOutputRequired/MSHeatPump(MSHeatPumpNum)%FlowFraction
  ENDIF
  If (CurDeadbandOrSetback(ZoneNum)) QZnReq = 0.0d0

  If (QZnReq > SmallLoad) then
    MSHeatPump(MSHeatPumpNum)%HeatCoolMode = HeatingMode
  Else If (QZnReq < (-1.d0*SmallLoad)) then
    MSHeatPump(MSHeatPumpNum)%HeatCoolMode = CoolingMode
  Else
    MSHeatPump(MSHeatPumpNum)%HeatCoolMode = 0
  End If

  ! Determine the staged status
  If (ALLOCATED(StageZoneLogic)) Then
    If (StageZoneLogic(ZoneNum)) Then
      MSHeatPump(MSHeatPumpNum)%Staged = .TRUE.
      MSHeatPump(MSHeatPumpNum)%StageNum = ZoneSysEnergyDemand(ZoneNum)%StageNum
    Else
      If (MyStagedFlag(MSHeatPumpNum)) Then
        CALL ShowWarningError('ZoneControl:Thermostat:StagedDualSetpoint is found, but is not applied to '// &
             'this AirLoopHVAC:UnitaryHeatPump:AirToAir:MultiSpeed object = ')
        CALL ShowContinueError(Trim(MSHeatPump(MSHeatPumpNum)%Name) //'. Please make correction. Simulation continues...')
        MyStagedFlag(MSHeatPumpNum) = .FALSE.
      End If
    End If
  End If
  ! Set the inlet node mass flow rate
  IF (MSHeatPump(MSHeatPumpNum)%OpMode .EQ. ContFanCycCoil) THEN
  ! constant fan mode
    IF (QZnReq > SmallLoad .AND. .NOT. CurDeadbandOrSetback(ZoneNum)) THEN
      CompOnMassFlow = MSHeatPump(MSHeatPumpNum)%HeatMassFlowRate(1)
      CompOnFlowRatio = MSHeatPump(MSHeatPumpNum)%HeatingSpeedRatio(1)
      MSHeatPump(MSHeatPumpNum)%LastMode = HeatingMode
    ELSE IF (QZnReq < (-1.d0*SmallLoad) .AND. .NOT. CurDeadbandOrSetback(ZoneNum)) THEN
      CompOnMassFlow = MSHeatPump(MSHeatPumpNum)%CoolMassFlowRate(1)
      CompOnFlowRatio = MSHeatPump(MSHeatPumpNum)%CoolingSpeedRatio(1)
      MSHeatPump(MSHeatPumpNum)%LastMode = CoolingMode
    ELSE
      CompOnMassFlow = MSHeatPump(MSHeatPumpNum)%IdleMassFlowRate
      CompOnFlowRatio = MSHeatPump(MSHeatPumpNum)%IdleSpeedRatio
    END IF
    CompOffMassFlow = MSHeatPump(MSHeatPumpNum)%IdleMassFlowRate
    CompOffFlowRatio = MSHeatPump(MSHeatPumpNum)%IdleSpeedRatio
  ELSE
  ! cycling fan mode
    IF (QZnReq > SmallLoad .AND. .NOT. CurDeadbandOrSetback(ZoneNum)) THEN
      CompOnMassFlow = MSHeatPump(MSHeatPumpNum)%HeatMassFlowRate(1)
      CompOnFlowRatio = MSHeatPump(MSHeatPumpNum)%HeatingSpeedRatio(1)
    ELSE IF (QZnReq < (-1.d0*SmallLoad) .AND. .NOT. CurDeadbandOrSetback(ZoneNum)) THEN
      CompOnMassFlow = MSHeatPump(MSHeatPumpNum)%CoolMassFlowRate(1)
      CompOnFlowRatio = MSHeatPump(MSHeatPumpNum)%CoolingSpeedRatio(1)
    ELSE
      CompOnMassFlow = 0.0d0
      CompOnFlowRatio = 0.0d0
    END IF
    CompOffMassFlow = 0.0d0
    CompOffFlowRatio = 0.0d0
  END IF

  ! Set the inlet node mass flow rate
  IF (GetCurrentScheduleValue(MSHeatPump(MSHeatPumpNum)%AvaiSchedPtr) .gt. 0.0d0 .AND. CompOnMassFlow .NE. 0.0d0) THEN
    OnOffAirFlowRatio = 1.0d0
    IF(FirstHVACIteration)THEN
      Node(MSHeatPump(MSHeatPumpNum)%AirInletNodeNum)%MassFlowRate = CompOnMassFlow
      PartLoadFrac           = 0.0d0
    ELSE
      IF (MSHeatPump(MSHeatPumpNum)%HeatCoolMode /= 0) THEN
        PartLoadFrac = 1.0d0
      ELSE
        PartLoadFrac = 0.0d0
      END IF
    END IF
  ELSE
    PartLoadFrac = 0.0d0
    Node(InNode)%MassFlowRate           = 0.0d0
    Node(OutNode)%MassFlowRate          = 0.0d0
    Node(OutNode)%MassFlowRateMaxAvail  = 0.0d0
    OnOffAirFlowRatio      = 1.0d0
  END IF

  ! Check availability of DX coils
  IF (GetCurrentScheduleValue(MSHeatPump(MSHeatPumpNum)%AvaiSchedPtr) .gt. 0.0d0) Then
    If (MSHeatPump(MSHeatPumpNum)%HeatCoolMode == CoolingMode) Then
      CoilAvailSchPtr=GetDXCoilAvailSchPtr('Coil:Cooling:DX:MultiSpeed',MSHeatPump(MSHeatPumpNum)%DXCoolCoilName, &
                      ErrorsFound,MSHeatPump(MSHeatPumpNum)%DXCoolCoilIndex)
      If (ErrorsFound) Then
        CALL ShowFatalError('InitMSHeatPump, The previous error causes termination.')
      End If
      IF (GetCurrentScheduleValue(CoilAvailSchPtr) .EQ. 0.d0) Then
        If (MSHeatPump(MSHeatPumpNum)%CoolCountAvail .eq. 0) THEN
          MSHeatPump(MSHeatPumpNum)%CoolCountAvail = MSHeatPump(MSHeatPumpNum)%CoolCountAvail+1
          CALL ShowWarningError(Trim(MSHeatPump(MSHeatPumpNum)%Name) //' is ready to perform cooling, but its DX cooling coil = ' &
            //TRIM(MSHeatPump(MSHeatPumpNum)%DXCoolCoilName)//' is not available at Available Schedule = ' &
            //Trim(GetScheduleName(CoilAvailSchPtr))//'.')
          CALL ShowContinueErrorTimeStamp('Availability schedule returned=' &
             //RoundSigDigits(GetCurrentScheduleValue(CoilAvailSchPtr),1))
        Else
          MSHeatPump(MSHeatPumpNum)%CoolCountAvail = MSHeatPump(MSHeatPumpNum)%CoolCountAvail+1
          CALL ShowRecurringWarningErrorAtEnd(TRIM(MSHeatPump(MSHeatPumpNum)%Name)//':'//&
            ' Cooling coil is still not available ...',MSHeatPump(MSHeatPumpNum)%CoolIndexAvail &
            , GetCurrentScheduleValue(CoilAvailSchPtr), GetCurrentScheduleValue(CoilAvailSchPtr))
        End If
      End If
    End If
    If (MSHeatPump(MSHeatPumpNum)%HeatCoolMode == HeatingMode .AND. &
       MSHeatPump(MSHeatPumpNum)%HeatCoilType == MultiSpeedHeatingCoil) Then
      CoilAvailSchPtr=GetDXCoilAvailSchPtr('Coil:Heating:DX:MultiSpeed',MSHeatPump(MSHeatPumpNum)%DXHeatCoilName, &
                      ErrorsFound,MSHeatPump(MSHeatPumpNum)%DXHeatCoilIndex)
      If (ErrorsFound) Then
        CALL ShowFatalError('InitMSHeatPump, The previous error causes termination.')
      End If
      IF (GetCurrentScheduleValue(CoilAvailSchPtr) .EQ. 0.d0) Then
        IF (MSHeatPump(MSHeatPumpNum)%HeatCountAvail .eq. 0) THEN
          MSHeatPump(MSHeatPumpNum)%HeatCountAvail = MSHeatPump(MSHeatPumpNum)%HeatCountAvail+1
          CALL ShowWarningError(Trim(MSHeatPump(MSHeatPumpNum)%Name) //' is ready to perform heating, but its DX heating coil = ' &
            //TRIM(MSHeatPump(MSHeatPumpNum)%DXCoolCoilName)//' is not available at Available Schedule = ' &
            //Trim(GetScheduleName(CoilAvailSchPtr))//'.')
          CALL ShowContinueErrorTimeStamp('Availability schedule returned=' &
             //RoundSigDigits(GetCurrentScheduleValue(CoilAvailSchPtr),1))
        Else
          MSHeatPump(MSHeatPumpNum)%HeatCountAvail = MSHeatPump(MSHeatPumpNum)%HeatCountAvail+1
          CALL ShowRecurringWarningErrorAtEnd(TRIM(MSHeatPump(MSHeatPumpNum)%Name)//':'//&
            ' Heating coil is still not available ...',MSHeatPump(MSHeatPumpNum)%HeatIndexAvail &
            , GetCurrentScheduleValue(CoilAvailSchPtr), GetCurrentScheduleValue(CoilAvailSchPtr))
        End If
      End If
    End If
  End If

  MSHeatPumpReport(MSHeatPumpNum)%CycRatio = 0.0d0
  MSHeatPumpReport(MSHeatPumpNum)%SpeedRatio = 0.0d0
  MSHeatPumpReport(MSHeatPumpNum)%SpeedNum = 0

  CALL CalcMSHeatPump(MSHeatPumpNum,FirstHVACIteration,On,1,0.0d0,PartLoadFrac,QSensUnitOut,  &
                       QZnReq,OnOffAirFlowRatio,SupHeaterLoad)

! If unit is scheduled OFF, setpoint is equal to inlet node temperature.
  MSHeatPump%TotHeatEnergyRate  = 0.0d0
  MSHeatPump%SensHeatEnergyRate = 0.0d0
  MSHeatPump%LatHeatEnergyRate  = 0.0d0
  MSHeatPump%TotCoolEnergyRate  = 0.0d0
  MSHeatPump%SensCoolEnergyRate = 0.0d0
  MSHeatPump%LatCoolEnergyRate  = 0.0d0

!!!LKL Discrepancy with < 0
  IF (GetCurrentScheduleValue(MSHeatPump(MSHeatPumpNum)%AvaiSchedPtr) .EQ. 0.0d0) THEN
    Node(OutNode)%Temp = Node(InNode)%Temp
    RETURN
  END IF

  IF(MSHeatPump(MSHeatPumpNum)%HeatCoolMode == 0 .AND. MSHeatPump(MSHeatPumpNum)%OpMode == CycFanCycCoil .OR.   &
     CompOnMassFlow .EQ. 0.0d0)THEN
    QZnReq = 0.0d0
    PartLoadFrac = 0.0d0
    Node(InNode)%MassFlowRate           = 0.0d0
    Node(OutNode)%MassFlowRateMaxAvail  = 0.0d0
  END IF
  MSHeatPump(MSHeatPumpNum)%LoadMet = 0.0d0
  CALL SetAverageAirFlow(MSHeatPumpNum, PartLoadFrac, OnOffAirFlowRatio)

  !Init maximum available Heat Recovery flow rate
  IF ((MSHeatPump(MSHeatPumpNum)%HeatRecActive) .AND. (.NOT. MyPlantScantFlag(MSHeatPumpNum)) ) THEN
    IF (PartLoadFrac > 0.d0) THEN
      IF (FirstHVACIteration) THEN
        MdotHR = MSHeatPump(MSHeatPumpNum)%DesignHeatRecMassFlowRate
      ELSE
        IF (MSHeatPump(MSHeatPumpNum)%HeatRecoveryMassFlowRate > 0.d0) THEN
          MdotHR = MSHeatPump(MSHeatPumpNum)%HeatRecoveryMassFlowRate
        ELSE
          MdotHR = MSHeatPump(MSHeatPumpNum)%DesignHeatRecMassFlowRate
        ENDIF
      ENDIF
    ELSE
      MdotHR = 0.d0
    ENDIF

    CALL SetComponentFlowRate(MdotHR, &
                              MSHeatPump(MSHeatPumpNum)%HeatRecInletNodeNum, &
                              MSHeatPump(MSHeatPumpNum)%HeatRecOutletNodeNum, &
                              MSHeatPump(MSHeatPumpNum)%HRLoopNum, &
                              MSHeatPump(MSHeatPumpNum)%HRLoopSideNum, &
                              MSHeatPump(MSHeatPumpNum)%HRBranchNum, &
                              MSHeatPump(MSHeatPumpNum)%HRCompNum )
  ENDIF

  ! get operating capacity of water and steam coil
  IF(FirstHVACIteration) THEN
    IF(MSHeatPump(MSHeatPumpNum)%HeatCoilType == Coil_HeatingWater) THEN
      !     set air-side and steam-side mass flow rates
      Node(MSHeatPump(MSHeatPumpNum)%CoilAirInletNode)%MassFlowRate = CompOnMassFlow
      mdot = MSHeatPump(MSHeatPumpNum)%MaxCoilFluidFlow
      CALL SetComponentFlowRate(mdot, &
                                   MSHeatPump(MSHeatPumpNum)%CoilControlNode, &
                                   MSHeatPump(MSHeatPumpNum)%CoilOutletNode, &
                                   MSHeatPump(MSHeatPumpNum)%LoopNum, &
                                   MSHeatPump(MSHeatPumpNum)%LoopSide, &
                                   MSHeatPump(MSHeatPumpNum)%BranchNum, &
                                   MSHeatPump(MSHeatPumpNum)%CompNum )
      !     simulate water coil to find operating capacity
      CALL SimulateWaterCoilComponents(MSHeatPump(MSHeatPumpNum)%HeatCoilName,FirstHVACIteration, &
                                       MSHeatPump(MSHeatPumpNum)%HeatCoilNum, QActual)
    END IF ! from IF(MSHeatPump(MSHeatPumpNum)%HeatCoilType == Coil_HeatingWater) THEN

    IF(MSHeatPump(MSHeatPumpNum)%HeatCoilType == Coil_HeatingSteam) THEN

      !     set air-side and steam-side mass flow rates
      Node(MSHeatPump(MSHeatPumpNum)%CoilAirInletNode)%MassFlowRate = CompOnMassFlow
      mdot = MSHeatPump(MSHeatPumpNum)%MaxCoilFluidFlow
      CALL SetComponentFlowRate(mdot, &
                                   MSHeatPump(MSHeatPumpNum)%CoilControlNode, &
                                   MSHeatPump(MSHeatPumpNum)%CoilOutletNode, &
                                   MSHeatPump(MSHeatPumpNum)%LoopNum, &
                                   MSHeatPump(MSHeatPumpNum)%LoopSide, &
                                   MSHeatPump(MSHeatPumpNum)%BranchNum, &
                                   MSHeatPump(MSHeatPumpNum)%CompNum )

!     simulate steam coil to find operating capacity
      CALL SimulateSteamCoilComponents(MSHeatPump(MSHeatPumpNum)%HeatCoilName, &
                                       FirstHVACIteration,    &
                                       1.0d0, & !QCoilReq, simulate any load > 0 to get max capacity of steam coil
                                       MSHeatPump(MSHeatPumpNum)%HeatCoilNum, QActual)

    END IF ! from IF(MSHeatPump(MSHeatPumpNum)%HeatCoilType == Coil_HeatingSteam) THEN
    IF(MSHeatPump(MSHeatPumpNum)%SuppHeatCoilType == Coil_HeatingWater) THEN
      !     set air-side and steam-side mass flow rates
      Node(MSHeatPump(MSHeatPumpNum)%SuppCoilAirInletNode)%MassFlowRate = CompOnMassFlow
      mdot = MSHeatPump(MSHeatPumpNum)%MaxSuppCoilFluidFlow
      CALL SetComponentFlowRate(mdot, &
                                   MSHeatPump(MSHeatPumpNum)%SuppCoilControlNode, &
                                   MSHeatPump(MSHeatPumpNum)%SuppCoilOutletNode, &
                                   MSHeatPump(MSHeatPumpNum)%SuppLoopNum, &
                                   MSHeatPump(MSHeatPumpNum)%SuppLoopSide, &
                                   MSHeatPump(MSHeatPumpNum)%SuppBranchNum, &
                                   MSHeatPump(MSHeatPumpNum)%SuppCompNum )
      !     simulate water coil to find operating capacity
      CALL SimulateWaterCoilComponents(MSHeatPump(MSHeatPumpNum)%SuppHeatCoilName,FirstHVACIteration, &
                                       MSHeatPump(MSHeatPumpNum)%SuppHeatCoilNum, QActual)
      MSHeatPump(MSHeatPumpNum)%DesignSuppHeatingCapacity = QActual

    END IF ! from IF(MSHeatPump(MSHeatPumpNum)%SuppHeatCoilType == Coil_HeatingWater) THEN

    IF(MSHeatPump(MSHeatPumpNum)%SuppHeatCoilType == Coil_HeatingSteam) THEN

      !     set air-side and steam-side mass flow rates
      Node(MSHeatPump(MSHeatPumpNum)%SuppCoilAirInletNode)%MassFlowRate = CompOnMassFlow
      mdot = MSHeatPump(MSHeatPumpNum)%MaxSuppCoilFluidFlow
      CALL SetComponentFlowRate(mdot, &
                                   MSHeatPump(MSHeatPumpNum)%SuppCoilControlNode, &
                                   MSHeatPump(MSHeatPumpNum)%SuppCoilOutletNode, &
                                   MSHeatPump(MSHeatPumpNum)%SuppLoopNum, &
                                   MSHeatPump(MSHeatPumpNum)%SuppLoopSide, &
                                   MSHeatPump(MSHeatPumpNum)%SuppBranchNum, &
                                   MSHeatPump(MSHeatPumpNum)%SuppCompNum )

!     simulate steam coil to find operating capacity
      CALL SimulateSteamCoilComponents(MSHeatPump(MSHeatPumpNum)%SuppHeatCoilName, &
                                       FirstHVACIteration,    &
                                       1.0d0, & !QCoilReq, simulate any load > 0 to get max capacity of steam coil
                                       MSHeatPump(MSHeatPumpNum)%SuppHeatCoilNum, QActual)
      MSHeatPump(MSHeatPumpNum)%DesignSuppHeatingCapacity = GetSteamCoilCapacity('Coil:Heating:Steam', &
                                                      MSHeatPump(MSHeatPumpNum)%SuppHeatCoilName,ErrorsFound)

    END IF ! from IF(MSHeatPump(MSHeatPumpNum)%SuppHeatCoilType == Coil_HeatingSteam) THEN
END IF ! from IF( FirstHVACIteration ) THEN

RETURN
END SUBROUTINE InitMSHeatPump

!******************************************************************************

SUBROUTINE SizeMSHeatPump(MSHeatPumpNum)
            ! SUBROUTINE INFORMATION:
            !       AUTHOR:          Lixing Gu, FSEC
            !       DATE WRITTEN:    June 2007
            !       MODIFIED         na
            !       RE-ENGINEERED    na

            ! PURPOSE OF THIS SUBROUTINE:
            ! This subroutine is for sizing multispeed heat pump airflow rates and flow fraction.

            ! METHODOLOGY EMPLOYED:
            !

            ! REFERENCES: na

            ! USE STATEMENTS:
  USE DataSizing
  USE InputProcessor
  USE DataZoneEquipment, ONLY: ZoneEquipConfig
  USE General,           ONLY: TrimSigDigits
  USE ReportSizingManager, ONLY: ReportSizingOutput
  USE PlantUtilities,    ONLY: RegisterPlantCompDesignFlow


  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER         , INTENT (IN)           :: MSHeatPumpNum      ! Engine driven heat pump number


          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER  :: ControlledZoneNum = 0        ! Index of Controllerd zone number
  INTEGER  :: ThisCtrlZoneNum = 0          ! Controllerd zone number
  REAL(r64)     :: ControlZoneVolFlow = 0.0d0     ! Controlled zone volumetric flow
  INTEGER  :: NumOfSpeedCooling            ! Number of speeds for cooling
  INTEGER  :: NumOfSpeedHeating            ! Number of speeds for heating
  INTEGER  :: i                            ! Index to speed

  ! FLOW
  NumOfSpeedCooling = MSHeatPump(MSHeatPumpNum)%NumOfSpeedCooling
  NumOfSpeedHeating = MSHeatPump(MSHeatPumpNum)%NumOfSpeedHeating

Do I=NumOfSpeedCooling,1,-1

  IF (MSHeatPump(MSHeatPumpNum)%CoolVolumeFlowRate(i) == AutoSize) THEN
    IF (CurSysNum > 0) THEN
      If (i == NumOfSpeedCooling) Then
        CALL CheckSysSizing(CurrentModuleObject, MSHeatPump(MSHeatPumpNum)%Name)
        MSHeatPump(MSHeatPumpNum)%CoolVolumeFlowRate(i) = FinalSysSizing(CurSysNum)%DesMainVolFlow
        IF (MSHeatPump(MSHeatPumpNum)%FanVolFlow .LT. MSHeatPump(MSHeatPumpNum)%CoolVolumeFlowRate(i) .AND. &
          MSHeatPump(MSHeatPumpNum)%FanVolFlow .NE. AutoSize)THEN
          MSHeatPump(MSHeatPumpNum)%CoolVolumeFlowRate(i) = MSHeatPump(MSHeatPumpNum)%FanVolFlow
          CALL ShowWarningError(TRIM(CurrentModuleObject)//' "'//TRIM(MSHeatPump(MSHeatPumpNum)%Name)//'"')
          CALL ShowContinueError('The supply air flow rate at high speed is less than the ' &
              //'autosized value for the supply air flow rate in cooling mode. Consider autosizing the fan for' &
              //' this simulation.')
          CALL ShowContinueError('The air flow rate at high speed in cooling mode ' &
                             //'is reset to the supply air flow rate and the simulation continues.')
        END IF
      Else
        MSHeatPump(MSHeatPumpNum)%CoolVolumeFlowRate(i) = MSHeatPump(MSHeatPumpNum)%CoolVolumeFlowRate(NumOfSpeedCooling)* &
                                                          i/NumOfSpeedCooling
      End If
      IF (MSHeatPump(MSHeatPumpNum)%CoolVolumeFlowRate(i) < SmallAirVolFlow) THEN
        MSHeatPump(MSHeatPumpNum)%CoolVolumeFlowRate = 0.0d0
      END IF
      ! Ensure the flow rate at lower speed has to be less or equal to the flow rate at higher speed
      If (i /= NumOfSpeedCooling) Then
        If (MSHeatPump(MSHeatPumpNum)%CoolVolumeFlowRate(i) > MSHeatPump(MSHeatPumpNum)%CoolVolumeFlowRate(i+1)) Then
          MSHeatPump(MSHeatPumpNum)%CoolVolumeFlowRate(i) = MSHeatPump(MSHeatPumpNum)%CoolVolumeFlowRate(i+1)
        End If
      End If
      CALL ReportSizingOutput(TRIM(CurrentModuleObject), MSHeatPump(MSHeatPumpNum)%Name, &
            'Speed '//Trim(TrimSigDigits(i))//' Supply Air Flow Rate During Cooling Operation [m3/s]',  &
               MSHeatPump(MSHeatPumpNum)%CoolVolumeFlowRate(i))
    END IF
  END IF

End Do

Do I=NumOfSpeedHeating,1,-1
  IF (MSHeatPump(MSHeatPumpNum)%HeatVolumeFlowRate(i) == AutoSize) THEN
    IF (CurSysNum > 0) THEN
      If (i == NumOfSpeedHeating) Then
        CALL CheckSysSizing(CurrentModuleObject, MSHeatPump(MSHeatPumpNum)%Name)
        MSHeatPump(MSHeatPumpNum)%HeatVolumeFlowRate(i)   = FinalSysSizing(CurSysNum)%DesMainVolFlow
        IF (MSHeatPump(MSHeatPumpNum)%FanVolFlow .LT. MSHeatPump(MSHeatPumpNum)%HeatVolumeFlowRate(i) .AND. &
          MSHeatPump(MSHeatPumpNum)%FanVolFlow .NE. AutoSize) THEN
          MSHeatPump(MSHeatPumpNum)%HeatVolumeFlowRate(i) = MSHeatPump(MSHeatPumpNum)%FanVolFlow
          CALL ShowWarningError(TRIM(CurrentModuleObject)//' "'//TRIM(MSHeatPump(MSHeatPumpNum)%Name)//'"')
          CALL ShowContinueError('The supply air flow rate at high speed is less than the ' &
              //'autosized value for the maximum air flow rate in heating mode. Consider autosizing the fan for' &
              //' this simulation.')
          CALL ShowContinueError('The maximum air flow rate at high speed in heating mode ' &
                             //'is reset to the supply air flow rate and the simulation continues.')
        END IF
      Else
        MSHeatPump(MSHeatPumpNum)%HeatVolumeFlowRate(i) = MSHeatPump(MSHeatPumpNum)%HeatVolumeFlowRate(NumOfSpeedHeating)* &
                                                          i/NumOfSpeedHeating
      End If
      IF (MSHeatPump(MSHeatPumpNum)%HeatVolumeFlowRate(i) < SmallAirVolFlow) THEN
        MSHeatPump(MSHeatPumpNum)%HeatVolumeFlowRate(i) = 0.0d0
      END IF
      ! Ensure the flow rate at lower speed has to be less or equal to the flow rate at higher speed
      If (i /= NumOfSpeedHeating) Then
        If (MSHeatPump(MSHeatPumpNum)%HeatVolumeFlowRate(i) > MSHeatPump(MSHeatPumpNum)%HeatVolumeFlowRate(i+1)) Then
          MSHeatPump(MSHeatPumpNum)%HeatVolumeFlowRate(i) = MSHeatPump(MSHeatPumpNum)%HeatVolumeFlowRate(i+1)
        End If
      End If
      CALL ReportSizingOutput(TRIM(CurrentModuleObject), MSHeatPump(MSHeatPumpNum)%Name, &
           'Speed'//Trim(TrimSigDigits(i))//'Supply Air Flow Rate During Heating Operation [m3/s]',   &
              MSHeatPump(MSHeatPumpNum)%HeatVolumeFlowRate(i))
    END IF
  END IF
End Do

  IF (MSHeatPump(MSHeatPumpNum)%IdleVolumeAirRate == AutoSize) THEN
    IF (CurSysNum > 0) THEN
      CALL CheckSysSizing(TRIM(CurrentModuleObject), MSHeatPump(MSHeatPumpNum)%Name)
      MSHeatPump(MSHeatPumpNum)%IdleVolumeAirRate = FinalSysSizing(CurSysNum)%DesMainVolFlow
      IF (MSHeatPump(MSHeatPumpNum)%FanVolFlow .LT. MSHeatPump(MSHeatPumpNum)%IdleVolumeAirRate .AND. &
         MSHeatPump(MSHeatPumpNum)%FanVolFlow .NE. AutoSize)THEN
        MSHeatPump(MSHeatPumpNum)%IdleVolumeAirRate = MSHeatPump(MSHeatPumpNum)%FanVolFlow
        CALL ShowWarningError(TRIM(CurrentModuleObject)//' "'//TRIM(MSHeatPump(MSHeatPumpNum)%Name)//'"')
        CALL ShowContinueError('The supply air flow rate is less than the autosized value' &
                             //' for the maximum air flow rate when no heating or cooling is needed. Consider' &
                             //' autosizing the fan for this simulation.')
        CALL ShowContinueError('The maximum air flow rate when no heating or cooling is needed ' &
                             //'is reset to the supply air flow rate and the simulation continues.')
      END IF
      IF (MSHeatPump(MSHeatPumpNum)%IdleVolumeAirRate < SmallAirVolFlow) THEN
        MSHeatPump(MSHeatPumpNum)%IdleVolumeAirRate = 0.0d0
      END IF

      CALL ReportSizingOutput(TRIM(CurrentModuleObject), MSHeatPump(MSHeatPumpNum)%Name, &
                    'Supply Air Flow Rate When No Cooling or Heating is Needed [m3/s]',  &
                     MSHeatPump(MSHeatPumpNum)%IdleVolumeAirRate)
    END IF
  END IF

  IF (MSHeatPump(MSHeatPumpNum)%SuppMaxAirTemp == AutoSize) THEN
    IF (CurSysNum > 0) THEN
      If (MSHeatPump(MSHeatPumpNum)%SuppHeatCoilType == 1) Then ! Gas
        CALL CheckZoneSizing('Coil:Heating:Gas', MSHeatPump(MSHeatPumpNum)%Name)
      Else
        CALL CheckZoneSizing('Coil:Heating:Electric', MSHeatPump(MSHeatPumpNum)%Name)
      End If
      MSHeatPump(MSHeatPumpNum)%SuppMaxAirTemp = FinalSysSizing(CurSysNum)%HeatSupTemp
      CALL ReportSizingOutput(TRIM(CurrentModuleObject), MSHeatPump(MSHeatPumpNum)%Name, &
                              'Maximum Supply Air Temperature from Supplemental Heater [C]', &
                              MSHeatPump(MSHeatPumpNum)%SuppMaxAirTemp)
    END IF
  END IF

  IF (MSHeatPump(MSHeatPumpNum)%DesignSuppHeatingCapacity == AutoSize) THEN
    IF (CurSysNum > 0) THEN
      If (MSHeatPump(MSHeatPumpNum)%SuppHeatCoilType == 1) Then ! Gas
        CALL CheckSysSizing('Coil:Heating:Gas', MSHeatPump(MSHeatPumpNum)%Name)
      Else
        CALL CheckSysSizing('Coil:Heating:Electric', MSHeatPump(MSHeatPumpNum)%Name)
      End If
      MSHeatPump(MSHeatPumpNum)%DesignSuppHeatingCapacity = FinalSysSizing(CurSysNum)%HeatCap
    ELSE
      MSHeatPump(MSHeatPumpNum)%DesignSuppHeatingCapacity = 0.0d0
    END IF
    CALL ReportSizingOutput(TRIM(CurrentModuleObject), MSHeatPump(MSHeatPumpNum)%Name, &
                              'Supplemental Heating Coil Nominal Capacity [W]', &
                              MSHeatPump(MSHeatPumpNum)%DesignSuppHeatingCapacity)
  END IF
  SuppHeatCap = MSHeatPump(MSHeatPumpNum)%DesignSuppHeatingCapacity

  IF (MSHeatPump(MSHeatPumpNum)%HeatRecActive) THEN
    CALL RegisterPlantCompDesignFlow(MSHeatPump(MSHeatPumpNum)%HeatRecInletNodeNum, &
                              MSHeatPump(MSHeatPumpNum)%DesignHeatRecFlowRate )
  ENDIF

RETURN
END SUBROUTINE SizeMSHeatPump

!******************************************************************************

SUBROUTINE ControlMSHPOutput(MSHeatPumpNum,FirstHVACIteration,CompOp,OpMode,QZnReq,ZoneNum,SpeedNum,SpeedRatio,PartLoadFrac, &
                             OnOffAirFlowRatio,SupHeaterLoad)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Lixing Gu
          !       DATE WRITTEN   June 2007
          !       MODIFIED       na
          !       RE-ENGINEERED  Revised for multispeed heat pump use based on ControlPTHPOutput

          ! PURPOSE OF THIS SUBROUTINE:
          ! Determine the part load fraction at low speed, and speed ratio at high speed for this time step.

          ! METHODOLOGY EMPLOYED:
          ! Use RegulaFalsi technique to iterate on part-load ratio until convergence is achieved.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE General,                   ONLY: SolveRegulaFalsi, RoundSigDigits, TrimSigDigits
  USE DataGlobals,               ONLY: WarmUpFlag,CurrentTime
  USE HeatingCoils,              ONLY: SimulateHeatingCoilComponents
  USE Psychrometrics,            ONLY: PsyCpAirFnWTdb

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT   (IN)  :: MSHeatPumpNum      ! Unit index of engine driven heat pump
  LOGICAL, INTENT   (IN)  :: FirstHVACIteration ! flag for 1st HVAC iteration in the time step
  INTEGER, INTENT   (IN)  :: CompOp             ! compressor operation; 1=on, 0=off
  INTEGER, INTENT   (IN)  :: OpMode             ! operating mode: CycFanCycCoil | ContFanCycCoil
  INTEGER, INTENT   (OUT) :: SpeedNum           ! Speed number
  REAL(r64)   , INTENT   (IN)  :: QZnReq             ! cooling or heating output needed by zone [W]
  INTEGER, INTENT   (IN)  :: ZoneNum            ! Index to zone number
  REAL(r64)   , INTENT   (OUT) :: SpeedRatio         ! unit speed ratio for DX coils
  REAL(r64)   , INTENT   (OUT) :: PartLoadFrac       ! unit part load fraction
  REAL(r64)   , INTENT (INOUT) :: OnOffAirFlowRatio  ! ratio of compressor ON airflow to AVERAGE airflow over timestep
  REAL(r64)   , INTENT (INOUT) :: SupHeaterLoad      ! Supplemental heater load [W]

          ! SUBROUTINE PARAMETER DEFINITIONS:
          !
  INTEGER, PARAMETER :: MaxIte   = 500          ! maximum number of iterations
  REAL(r64), PARAMETER    :: MinPLF   = 0.0d0          ! minimum part load factor allowed

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64)          :: FullOutput    ! unit full output when compressor is operating [W]
  REAL(r64)          :: LowOutput     ! unit full output at low speed [W]
  REAL(r64)          :: TempOutput    ! unit output when iteration limit exceeded [W]
  REAL(r64)          :: NoCompOutput  ! output when no active compressor [W]
  REAL(r64)   :: ErrorToler    ! error tolerance
  INTEGER            :: SolFla        ! Flag of RegulaFalsi solver
  REAL(r64), DIMENSION(9) :: Par           ! Parameters passed to RegulaFalsi
  REAL(r64)          :: CpAir         ! air specific heat
  REAL(r64)          :: OutsideDryBulbTemp ! Outside air temperature at external node height
  REAL(r64)          :: QCoilActual   ! coil load actually delivered returned to calling component
  INTEGER            :: i             ! Speed index
  INTEGER,SAVE       :: ErrCountCyc=0 ! Counter used to minimize the occurrence of output warnings
  INTEGER,SAVE       :: ErrCountVar=0 ! Counter used to minimize the occurrence of output warnings

  ! FLOW
  SupHeaterLoad = 0.0d0
  PartLoadFrac  = 0.0d0
  SpeedRatio    = 0.0d0
  SpeedNum = 1

  OutsideDryBulbTemp = OutDryBulbTemp

!!!LKL Discrepancy with < 0
  IF (GetCurrentScheduleValue(MSHeatPump(MSHeatPumpNum)%AvaiSchedPtr) .EQ. 0.0d0) RETURN

  ! Get result when DX coil is off
  CALL CalcMSHeatPump(MSHeatPumpNum,FirstHVACIteration,CompOp,SpeedNum,SpeedRatio,PartLoadFrac,NoCompOutput, &
                      QZnReq,OnOffAirFlowRatio,SupHeaterLoad)

  ! If cooling and NoCompOutput < QZnReq, the coil needs to be off
  ! If heating and NoCompOutput > QZnReq, the coil needs to be off
  IF ((QZnReq < (-1.d0*SmallLoad) .AND. NoCompOutput < QZnReq) .OR. (QZnReq > SmallLoad .AND. NoCompOutput > QZnReq) &
       .OR. ABS(QZnReq) <= SmallLoad) THEN
    RETURN
  END IF

  ! Get full load result
  PartLoadFrac  = 1.0d0
  SpeedRatio    = 1.0d0
  If (MSHeatPump(MSHeatPumpNum)%HeatCoolMode == HeatingMode) Then
    SpeedNum = MSHeatPump(MSHeatPumpNum)%NumOfSpeedHeating
    If (MSHeatPump(MSHeatPumpNum)%Staged .AND. ABS(MSHeatPump(MSHeatPumpNum)%StageNum) .LT. SpeedNum) Then
      SpeedNum = ABS(MSHeatPump(MSHeatPumpNum)%StageNum)
      If (SpeedNum == 1) SpeedRatio = 0.0d0
    End If
  End If
  If (MSHeatPump(MSHeatPumpNum)%HeatCoolMode == CoolingMode) Then
    SpeedNum = MSHeatPump(MSHeatPumpNum)%NumOfSpeedCooling
    If (MSHeatPump(MSHeatPumpNum)%Staged .AND. ABS(MSHeatPump(MSHeatPumpNum)%StageNum) .LT. SpeedNum) Then
      SpeedNum = ABS(MSHeatPump(MSHeatPumpNum)%StageNum)
      If (SpeedNum == 1) SpeedRatio = 0.0d0
    End If
  End If

  CALL CalcMSHeatPump(MSHeatPumpNum,FirstHVACIteration,CompOp,SpeedNum,SpeedRatio,PartLoadFrac,FullOutput,QZnReq,  &
                      OnOffAirFlowRatio,SupHeaterLoad)

  IF (QZnReq .LT. (-1.d0*SmallLoad)) THEN
  ! Since we are cooling, we expect FullOutput to be < 0 and FullOutput < NoCompOutput
  ! Check that this is the case; if not set PartLoadFrac = 0.0 (off) and return
    IF (FullOutput >= 0.0d0 .OR. FullOutput >= NoCompOutput) THEN
      PartLoadFrac = 0.0d0
      SpeedRatio   = 0.0d0
      SpeedNum = 0
      RETURN
    END IF
!  ! If the QZnReq <= FullOutput the unit needs to run full out
    IF (QZnReq <= FullOutput) THEN
      PartLoadFrac = 1.0d0
      SpeedRatio   = 1.0d0
      If (MSHeatPump(MSHeatPumpNum)%Staged .AND. SpeedNum == 1) SpeedRatio = 0.0d0
      MSHeatPumpReport(MSHeatPumpNum)%CycRatio = PartLoadFrac
      MSHeatPumpReport(MSHeatPumpNum)%SpeedRatio = SpeedRatio
      MSHeatPumpReport(MSHeatPumpNum)%SpeedNum = SpeedNum
      RETURN
    END IF
    ErrorToler = 0.001d0 !Error tolerance for convergence from input deck
  ELSE
  ! Since we are heating, we expect FullOutput to be > 0 and FullOutput > NoCompOutput
  ! Check that this is the case; if not set PartLoadFrac = 0.0 (off)
    IF (FullOutput <= 0.0d0 .OR. FullOutput <= NoCompOutput) THEN
      PartLoadFrac = 0.0d0
      SpeedRatio   = 0.0d0
  ! may need supplemental heating so don't return in heating mode
    END IF
    IF (QZnReq  >=  FullOutput) THEN
      PartLoadFrac = 1.0d0
      SpeedRatio   = 1.0d0
  ! may need supplemental heating so don't return in heating mode
    END IF
    ErrorToler = 0.001d0 !Error tolerance for convergence from input deck
  END IF

  ! Calculate the part load fraction
  IF (((QZnReq .GT. SmallLoad .AND. QZnReq < FullOutput) .OR. (QZnReq .LT. (-1.d0*SmallLoad) .AND. QZnReq > FullOutput)) &
      .AND. (.NOT. MSHeatPump(MSHeatPumpNum)%Staged)) THEN

    Par(1) = MSHeatPumpNum
    Par(2) = ZoneNum
    IF (FirstHVACIteration) THEN
      Par(3) = 1.0d0
    ELSE
      Par(3) = 0.0d0
    END IF
    Par(4) = OpMode
    Par(5) = QZnReq
    Par(6) = OnOffAirFlowRatio
    Par(7) = SupHeaterLoad
    Par(9) = CompOp
    ! Check whether the low speed coil can meet the load or not
    CALL CalcMSHeatPump(MSHeatPumpNum,FirstHVACIteration,CompOp,1,0.0d0,1.0d0,LowOutput,  &
                        QZnReq,OnOffAirFlowRatio,SupHeaterLoad)
    IF ((QZnReq .GT. 0.0d0 .AND. QZnReq <= LowOutput) .OR. (QZnReq .LT. 0.0d0 .AND. QZnReq >= LowOutput)) THEN
      SpeedRatio = 0.0d0
      SpeedNum = 1
      CALL SolveRegulaFalsi(ErrorToler, MaxIte, SolFla, PartLoadFrac, MSHPCyclingResidual, 0.0d0, 1.0d0, Par)
      IF (SolFla == -1) THEN
        If ( .NOT. WarmupFlag) Then
          IF (ErrCountCyc .eq. 0) THEN
            ErrCountCyc = ErrCountCyc+1
            CALL ShowWarningError('Iteration limit exceeded calculating DX unit cycling ratio, for unit='// &
                            TRIM(MSHeatPump(MSHeatPumpNum)%Name))
            CALL ShowContinueErrorTimeStamp('Cycling ratio returned='//RoundSigDigits(PartLoadFrac,2))
          Else
            ErrCountCyc = ErrCountCyc+1
            CALL ShowRecurringWarningErrorAtEnd(TRIM(MSHeatPump(MSHeatPumpNum)%Name)//'":'//&
          ' Iteration limit warning exceeding calculating DX unit cycling ratio  continues...' &
          ,MSHeatPump(MSHeatPumpNum)%ErrIndexCyc , PartLoadFrac, PartLoadFrac)
          End If
        End If
      ELSE IF (SolFla == -2) THEN
        CALL ShowFatalError('DX unit cycling ratio calculation failed: cycling limits exceeded, for unit='// &
                           TRIM(MSHeatPump(MSHeatPumpNum)%DXCoolCoilName))
      END IF
    Else
      ! Check to see which speed to meet the load
      PartLoadFrac = 1.0d0
      SpeedRatio = 1.0d0
      If (QZnReq .LT. (-1.d0*SmallLoad)) Then ! Cooling
        DO I=2,MSHeatPump(MSHeatPumpNum)%NumOfSpeedCooling
          CALL CalcMSHeatPump(MSHeatPumpNum,FirstHVACIteration,CompOp,I,SpeedRatio,PartLoadFrac,TempOutput, &
                              QZnReq,OnOffAirFlowRatio,SupHeaterLoad)
          If (QZnReq >= TempOutput) Then
            SpeedNum = I
            Exit
          End If
        END DO
      ELSE
        DO I=2,MSHeatPump(MSHeatPumpNum)%NumOfSpeedHeating
          CALL CalcMSHeatPump(MSHeatPumpNum,FirstHVACIteration,CompOp,I,SpeedRatio,PartLoadFrac,TempOutput, &
               QZnReq,OnOffAirFlowRatio,SupHeaterLoad)
          If (QZnReq <= TempOutput) Then
            SpeedNum = I
            Exit
          End If
        END DO
      END IF
      Par(8) = SpeedNum
      CALL SolveRegulaFalsi(ErrorToler, MaxIte, SolFla, SpeedRatio, MSHPVarSpeedResidual, 0.0d0, 1.0d0, Par)
      IF (SolFla == -1) THEN
        If ( .NOT. WarmupFlag) Then
          IF (ErrCountVar .eq. 0) THEN
            ErrCountVar = ErrCountVar+1
            CALL ShowWarningError('Iteration limit exceeded calculating DX unit speed ratio, for unit='// &
                            TRIM(MSHeatPump(MSHeatPumpNum)%Name))
            CALL ShowContinueErrorTimeStamp('Speed ratio returned=['//trim(RoundSigDigits(SpeedRatio,2))//'], Speed number =' &
                                        //trim(RoundSigDigits(SpeedNum,0)))
          Else
            ErrCountVar = ErrCountVar+1
            CALL ShowRecurringWarningErrorAtEnd(TRIM(MSHeatPump(MSHeatPumpNum)%Name)//'":'//&
          ' Iteration limit warning exceeding calculating DX unit speed ratio continues...' &
          ,MSHeatPump(MSHeatPumpNum)%ErrIndexVar, SpeedRatio, SpeedRatio)
          End If
        End If
      ELSE IF (SolFla == -2) THEN
        CALL ShowFatalError('DX unit compressor speed calculation failed: speed limits exceeded, for unit='// &
                           TRIM(MSHeatPump(MSHeatPumpNum)%DXCoolCoilName))
      END IF
    End If
  Else
    ! Staged thermostat performance
    If (MSHeatPump(MSHeatPumpNum)%StageNum .NE. 0) Then
      Par(1) = MSHeatPumpNum
      Par(2) = ZoneNum
      IF (FirstHVACIteration) THEN
        Par(3) = 1.0d0
      ELSE
        Par(3) = 0.0d0
      END IF
      Par(4) = OpMode
      Par(5) = QZnReq
      Par(6) = OnOffAirFlowRatio
      Par(7) = SupHeaterLoad
      Par(9) = CompOp
      SpeedNum = ABS(MSHeatPump(MSHeatPumpNum)%StageNum)
      Par(8) = SpeedNum
      If (SpeedNum == 1) Then
        CALL CalcMSHeatPump(MSHeatPumpNum,FirstHVACIteration,CompOp,1,0.0d0,1.0d0,LowOutput,  &
                      QZnReq,OnOffAirFlowRatio,SupHeaterLoad)
        SpeedRatio = 0.0d0
        IF ((QZnReq .GT. 0.0 .AND. QZnReq <= LowOutput) .OR. (QZnReq .LT. 0.0 .AND. QZnReq >= LowOutput)) THEN
          CALL SolveRegulaFalsi(ErrorToler, MaxIte, SolFla, PartLoadFrac, MSHPCyclingResidual, 0.0d0, 1.0d0, Par)
          IF (SolFla == -1) THEN
            If ( .NOT. WarmupFlag) Then
              IF (ErrCountCyc .eq. 0) THEN
                ErrCountCyc = ErrCountCyc+1
                CALL ShowWarningError('Iteration limit exceeded calculating DX unit cycling ratio, for unit='// &
                          TRIM(MSHeatPump(MSHeatPumpNum)%Name))
                CALL ShowContinueErrorTimeStamp('Cycling ratio returned='//RoundSigDigits(PartLoadFrac,2))
              Else
                ErrCountCyc = ErrCountCyc+1
                CALL ShowRecurringWarningErrorAtEnd(TRIM(MSHeatPump(MSHeatPumpNum)%Name)//'":'//&
                ' Iteration limit warning exceeding calculating DX unit cycling ratio  continues...' &
                ,MSHeatPump(MSHeatPumpNum)%ErrIndexCyc , PartLoadFrac, PartLoadFrac)
              End If
            End If
          ELSE IF (SolFla == -2) THEN
            CALL ShowFatalError('DX unit cycling ratio calculation failed: cycling limits exceeded, for unit='// &
                         TRIM(MSHeatPump(MSHeatPumpNum)%DXCoolCoilName))
          END IF
        Else
          FullOutput = LowOutput
          PartLoadFrac = 1.0d0
        End If
      Else
        If (MSHeatPump(MSHeatPumpNum)%StageNum < 0) Then
          SpeedNum = MIN(MSHeatPump(MSHeatPumpNum)%NumOfSpeedCooling,ABS(MSHeatPump(MSHeatPumpNum)%StageNum))
        Else
          SpeedNum = MIN(MSHeatPump(MSHeatPumpNum)%NumOfSpeedHeating,ABS(MSHeatPump(MSHeatPumpNum)%StageNum))
        End If
        CALL CalcMSHeatPump(MSHeatPumpNum,FirstHVACIteration,CompOp,SpeedNum,0.0d0,1.0d0,LowOutput,  &
                      QZnReq,OnOffAirFlowRatio,SupHeaterLoad)
        IF ((QZnReq .GT. 0.0 .AND. QZnReq >= LowOutput) .OR. (QZnReq .LT. 0.0 .AND. QZnReq <= LowOutput)) THEN
          CALL CalcMSHeatPump(MSHeatPumpNum,FirstHVACIteration,CompOp,SpeedNum,1.0d0,1.0d0,FullOutput,  &
                      QZnReq,OnOffAirFlowRatio,SupHeaterLoad)
          IF ((QZnReq .GT. 0.0 .AND. QZnReq <= FullOutput) .OR. (QZnReq .LT. 0.0 .AND. QZnReq >= FullOutput)) THEN
            Par(8) = SpeedNum
            CALL SolveRegulaFalsi(ErrorToler, MaxIte, SolFla, SpeedRatio, MSHPVarSpeedResidual, 0.0d0, 1.0d0, Par)
            IF (SolFla == -1) THEN
              If ( .NOT. WarmupFlag) Then
                IF (ErrCountVar .eq. 0) THEN
                  ErrCountVar = ErrCountVar+1
                  CALL ShowWarningError('Iteration limit exceeded calculating DX unit speed ratio, for unit='// &
                              TRIM(MSHeatPump(MSHeatPumpNum)%Name))
                  CALL ShowContinueErrorTimeStamp('Speed ratio returned=['//trim(RoundSigDigits(SpeedRatio,2))//  &
                     '], Speed number ='//trim(RoundSigDigits(SpeedNum,0)))
                Else
                  ErrCountVar = ErrCountVar+1
                  CALL ShowRecurringWarningErrorAtEnd(TRIM(MSHeatPump(MSHeatPumpNum)%Name)//'":'//&
                  ' Iteration limit warning exceeding calculating DX unit speed ratio continues...' &
                  ,MSHeatPump(MSHeatPumpNum)%ErrIndexVar, SpeedRatio, SpeedRatio)
                End If
              End If
            ELSE IF (SolFla == -2) THEN
              CALL ShowFatalError('DX unit compressor speed calculation failed: speed limits exceeded, for unit='// &
                           TRIM(MSHeatPump(MSHeatPumpNum)%DXCoolCoilName))
            END IF
          Else
            SpeedRatio = 1.0d0
          End If
        Else ! lowOutput provides a larger capacity than needed
          SpeedRatio = 0.0d0
        End If
      End If
    End If
  End If

  ! if the DX heating coil cannot meet the load, trim with supplemental heater
  ! occurs with constant fan mode when compressor is on or off
  ! occurs with cycling fan mode when compressor PLR is equal to 1
  IF ((QZnReq .GT. SmallLoad .AND. QZnReq .GT. FullOutput))THEN
    PartLoadFrac  = 1.0d0
    SpeedRatio  = 1.0d0
    If (MSHeatPump(MSHeatPumpNum)%Staged .AND. SpeedNum == 1) SpeedRatio = 0.0d0
    IF (OutsideDryBulbTemp .LE. MSHeatPump(MSHeatPumpNum)%SuppMaxAirTemp) THEN
      SupHeaterLoad = QZnReq - FullOutput
    ELSE
      SupHeaterLoad = 0.0d0
    END IF
    CALL CalcMSHeatPump(MSHeatPumpNum,FirstHVACIteration,CompOp,SpeedNum,SpeedRatio,PartLoadFrac,TempOutput,QZnReq,  &
                        OnOffAirFlowRatio,SupHeaterLoad)
  END IF

! check the outlet of the supplemental heater to be lower than the maximum supplemental heater supply air temperature
  IF (Node(MSHeatPump(MSHeatPumpNum)%AirOutletNodeNum)%Temp .GT. MSHeatPump(MSHeatPumpNum)%SuppMaxAirTemp .AND. &
                                                             SupHeaterLoad .GT. 0.0d0) THEN

!   If the supply air temperature is to high, turn off the supplemental heater to recalculate the outlet temperature
    SupHeaterLoad = 0.0d0
    CALL CalcNonDXHeatingCoils(MSHeatPumpNum,FirstHVACIteration, SupHeaterLoad, OpMode, QCoilActual)

!   If the outlet temperature is below the maximum supplemental heater supply air temperature, reduce the load passed to
!   the supplemental heater, otherwise leave the supplemental heater off. If the supplemental heater is to be turned on,
!   use the outlet conditions when the supplemental heater was off (CALL above) as the inlet conditions for the calculation
!   of supplemental heater load to just meet the maximum supply air temperature from the supplemental heater.
    IF (Node(MSHeatPump(MSHeatPumpNum)%AirOutletNodeNum)%Temp .LT. MSHeatPump(MSHeatPumpNum)%SuppMaxAirTemp) THEN
      CpAir = PsyCpAirFnWTdb(Node(MSHeatPump(MSHeatPumpNum)%AirOutletNodeNum)%HumRat, &
                             Node(MSHeatPump(MSHeatPumpNum)%AirOutletNodeNum)%Temp)
      SupHeaterLoad = Node(MSHeatPump(MSHeatPumpNum)%AirInletNodeNum)%MassFlowRate * CpAir * &
                      (MSHeatPump(MSHeatPumpNum)%SuppMaxAirTemp - Node(MSHeatPump(MSHeatPumpNum)%AirOutletNodeNum)%Temp)

    ELSE
      SupHeaterLoad = 0.0d0
    END IF
  END IF

  MSHeatPumpReport(MSHeatPumpNum)%CycRatio = PartLoadFrac
  MSHeatPumpReport(MSHeatPumpNum)%SpeedRatio = SpeedRatio
  MSHeatPumpReport(MSHeatPumpNum)%SpeedNum = SpeedNum

  RETURN
END SUBROUTINE ControlMSHPOutput

!******************************************************************************

SUBROUTINE CalcMSHeatPump(MSHeatPumpNum,FirstHVACIteration,CompOp,SpeedNum,SpeedRatio,PartLoadFrac,LoadMet, &
                          QZnReq,OnOffAirFlowRatio,SupHeaterLoad)
            ! SUBROUTINE INFORMATION:
            !       AUTHOR:          Lixing Gu, FSEC
            !       DATE WRITTEN:    June 2007
            !       MODIFIED         na
            !       RE-ENGINEERED    na

            ! PURPOSE OF THIS SUBROUTINE:
            !  This routine will calcultes MSHP performance based on given system load

            ! METHODOLOGY EMPLOYED:
            ! na

            ! REFERENCES: na

            ! USE STATEMENTS:
USE Fans,                    ONLY: SimulateFanComponents
USE DataEnvironment,         ONLY: OutDryBulbTemp
USE HeatingCoils,            ONLY: SimulateHeatingCoilComponents
USE DXCoils,                 ONLY: SimDXCoilMultiSpeed,DXCoilPartLoadRatio

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN)    :: MSHeatPumpNum        ! Engine driven heat pump number
  LOGICAL, INTENT (IN)    :: FirstHVACIteration   ! Flag for 1st HVAC iteration
  INTEGER, INTENT (IN)    :: SpeedNum             ! Speed number
  REAL(r64)   , INTENT (IN)    :: SpeedRatio           ! Compressor speed ratio
  REAL(r64)   , INTENT (IN)    :: PartLoadFrac         ! Compressor part load fraction
  REAL(r64)   , INTENT (OUT)   :: LoadMet              ! Load met by unit (W)
  REAL(r64)   , INTENT (IN)    :: QZnReq               ! Zone load (W)
  REAL(r64)   , INTENT (INOUT) :: OnOffAirFlowRatio    ! Ratio of compressor ON airflow to AVERAGE airflow over timestep
  REAL(r64)   , INTENT (INOUT) :: SupHeaterLoad        ! supplemental heater load (W)
  INTEGER, INTENT(IN)     :: CompOp               ! Compressor on/off; 1=on, 0=off

          ! SUBROUTINE PARAMETER DEFINITIONS:
!  INTEGER, PARAMETER  ::   On  = 1           ! Compressor on flag
!  INTEGER, PARAMETER  ::   Off = 2           ! Compressor off flag

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVMS TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: OutletNode        ! MSHP air outlet node
  INTEGER :: InletNode         ! MSHP air inlet node
  REAL(r64)    :: MinHumRat         ! Minimum humidity ratio for sensible capacity calculation (kg/kg)
  REAL(r64)    :: OutsideDryBulbTemp ! Outdoor dry bulb temperature [C]
  REAL(r64)    :: AirMassFlow       ! Air mass flow rate [kg/s]
  INTEGER :: FanInletNode      ! MSHP air outlet node
  INTEGER :: FanOutletNode     ! MSHP air inlet node
  REAL(r64)    :: SavePartloadRatio !
  REAL(r64)    :: SaveSpeedRatio
  REAL(r64)    :: QCoilActual   ! coil load actually delivered returned to calling component
  REAL(r64)    :: MdotSupp      ! suppleental coil hot water or steam flow rate
  REAL(r64)    :: MinWaterFlow  ! minimum water flow rate
  REAL(r64)    :: ErrorToler    ! supplemental heating coil convergence tollerance

  ! FLOW
  OutletNode = MSHeatPump(MSHeatPumpNum)%AirOutletNodeNum
  InletNode  = MSHeatPump(MSHeatPumpNum)%AirInletNodeNum
  OutsideDryBulbTemp = OutDryBulbTemp
  FanOutletNode = MSHeatPump(MSHeatPumpNum)%FanOutletNode
  FanInletNode  = MSHeatPump(MSHeatPumpNum)%FanInletNode

  SaveCompressorPLR = 0.0d0
  SavePartloadRatio = 0.0d0
  MinWaterFlow = 0.0d0
  ErrorToler = 0.001d0
  ! Set inlet air mass flow rate based on PLR and compressor on/off air flow rates
  CALL SetAverageAirFlow(MSHeatPumpNum, PartLoadFrac, OnOffAirFlowRatio, SpeedNum, SpeedRatio)

  AirMassFlow = Node(InletNode)%MassFlowRate
  ! if blow through, simulate fan then coils
  IF (MSHeatPump(MSHeatPumpNum)%FanPlaceType .EQ. BlowThru) THEN
    CALL SimulateFanComponents(MSHeatPump(MSHeatPumpNum)%FanName,FirstHVACIteration,MSHeatPump(MSHeatPumpNum)%FanNum,FanSpeedRatio)
    IF (QZnReq .LT. (-1.d0*SmallLoad)) THEN
      IF(OutsideDryBulbTemp .GT. MSHeatPump(MSHeatPumpNum)%MinOATCompressor) THEN
        CALL SimDXCoilMultiSpeed(MSHeatPump(MSHeatPumpNum)%DXCoolCoilName,SpeedRatio,PartLoadFrac,&
                                 MSHeatPump(MSHeatPumpNum)%DXCoolCoilIndex,SpeedNum,MSHeatPump(MSHeatPumpNum)%OpMode,CompOp)
        SavePartloadRatio = PartLoadFrac
        SaveSpeedRatio = SpeedRatio
      ELSE
        CALL SimDXCoilMultiSpeed(MSHeatPump(MSHeatPumpNum)%DXCoolCoilName,0.0d0,0.0d0,&
                                 MSHeatPump(MSHeatPumpNum)%DXCoolCoilIndex,SpeedNum,MSHeatPump(MSHeatPumpNum)%OpMode,CompOp)
      END IF
      SaveCompressorPLR = DXCoilPartLoadRatio(MSHeatPump(MSHeatPumpNum)%DXCoolCoilIndex)
    ELSE
      CALL SimDXCoilMultiSpeed(MSHeatPump(MSHeatPumpNum)%DXCoolCoilName,0.0d0,0.0d0,&
                               MSHeatPump(MSHeatPumpNum)%DXCoolCoilIndex,SpeedNum,MSHeatPump(MSHeatPumpNum)%OpMode,CompOp)
    END IF
    IF (MSHeatPump(MSHeatPumpNum)%HeatCoilType .EQ. MultiSpeedHeatingCoil)THEN
      IF (QZnReq .GT. SmallLoad) THEN
        IF(OutsideDryBulbTemp .GT. MSHeatPump(MSHeatPumpNum)%MinOATCompressor)THEN
          CALL SimDXCoilMultiSpeed(MSHeatPump(MSHeatPumpNum)%DXHeatCoilName,SpeedRatio,PartLoadFrac,&
                                   MSHeatPump(MSHeatPumpNum)%DXHeatCoilIndex,SpeedNum,MSHeatPump(MSHeatPumpNum)%OpMode,CompOp)
          SavePartloadRatio = PartLoadFrac
          SaveSpeedRatio = SpeedRatio
        ELSE
          CALL SimDXCoilMultiSpeed(MSHeatPump(MSHeatPumpNum)%DXHeatCoilName,0.0d0,0.0d0,&
                                   MSHeatPump(MSHeatPumpNum)%DXHeatCoilIndex,SpeedNum,MSHeatPump(MSHeatPumpNum)%OpMode,CompOp)
        END IF
        SaveCompressorPLR = DXCoilPartLoadRatio(MSHeatPump(MSHeatPumpNum)%DXHeatCoilIndex)
      ELSE
        CALL SimDXCoilMultiSpeed(MSHeatPump(MSHeatPumpNum)%DXHeatCoilName,0.0d0,0.0d0,&
                                 MSHeatPump(MSHeatPumpNum)%DXHeatCoilIndex,SpeedNum,MSHeatPump(MSHeatPumpNum)%OpMode,CompOp)
      ENDIF
    ELSEIF(MSHeatPump(MSHeatPumpNum)%HeatCoilType .EQ. Coil_HeatingElectric_MultiStage .OR. &
           MSHeatPump(MSHeatPumpNum)%HeatCoilType .EQ. Coil_HeatingGas_MultiStage)THEN
      IF (QZnReq .GT. SmallLoad) THEN
        CALL SimulateHeatingCoilComponents(MSHeatPump(MSHeatPumpNum)%HeatCoilName,FirstHVACIteration, &
                                           CompIndex     = 0, &
                                           FanOpMode     = MSHeatPump(MSHeatPumpNum)%OpMode,      &
                                           PartLoadRatio = PartLoadFrac, &
                                           StageNum      = SpeedNum,     &
                                           SpeedRatio    = SpeedRatio)
      ELSE
        CALL SimulateHeatingCoilComponents(MSHeatPump(MSHeatPumpNum)%HeatCoilName,FirstHVACIteration, &
                                           CompIndex     = 0, &
                                           FanOpMode     = MSHeatPump(MSHeatPumpNum)%OpMode,      &
                                           PartLoadRatio = 0.0d0,        &
                                           StageNum      = SpeedNum,     &
                                           SpeedRatio    = 0.0d0)
      ENDIF
    ELSE
      CALL CalcNonDXHeatingCoils(MSHeatPumpNum,FirstHVACIteration,QZnReq,MSHeatPump(MSHeatPumpNum)%OpMode,QCoilActual, PartLoadFrac)
    END IF
    ! Call twice to ensure the fan outlet conditions are updated
    CALL SimulateFanComponents(MSHeatPump(MSHeatPumpNum)%FanName,FirstHVACIteration,MSHeatPump(MSHeatPumpNum)%FanNum,FanSpeedRatio)
    IF (QZnReq .LT. (-1.d0*SmallLoad)) THEN
      IF(OutsideDryBulbTemp .GT. MSHeatPump(MSHeatPumpNum)%MinOATCompressor) THEN
        CALL SimDXCoilMultiSpeed(MSHeatPump(MSHeatPumpNum)%DXCoolCoilName,SpeedRatio,PartLoadFrac,&
                                 MSHeatPump(MSHeatPumpNum)%DXCoolCoilIndex,SpeedNum,MSHeatPump(MSHeatPumpNum)%OpMode,CompOp)
        SavePartloadRatio = PartLoadFrac
        SaveSpeedRatio = SpeedRatio
      ELSE
        CALL SimDXCoilMultiSpeed(MSHeatPump(MSHeatPumpNum)%DXCoolCoilName,0.0d0,0.0d0,&
                                 MSHeatPump(MSHeatPumpNum)%DXCoolCoilIndex,SpeedNum,MSHeatPump(MSHeatPumpNum)%OpMode,CompOp)
      END IF
      SaveCompressorPLR = DXCoilPartLoadRatio(MSHeatPump(MSHeatPumpNum)%DXCoolCoilIndex)
    ELSE
      CALL SimDXCoilMultiSpeed(MSHeatPump(MSHeatPumpNum)%DXCoolCoilName,0.0d0,0.0d0,&
                               MSHeatPump(MSHeatPumpNum)%DXCoolCoilIndex,SpeedNum,MSHeatPump(MSHeatPumpNum)%OpMode,CompOp)
    END IF
    IF (MSHeatPump(MSHeatPumpNum)%HeatCoilType .EQ. MultiSpeedHeatingCoil)THEN
      IF (QZnReq .GT. SmallLoad) THEN
        IF(OutsideDryBulbTemp .GT. MSHeatPump(MSHeatPumpNum)%MinOATCompressor)THEN
          CALL SimDXCoilMultiSpeed(MSHeatPump(MSHeatPumpNum)%DXHeatCoilName,SpeedRatio,PartLoadFrac,&
                                   MSHeatPump(MSHeatPumpNum)%DXHeatCoilIndex,SpeedNum,MSHeatPump(MSHeatPumpNum)%OpMode,CompOp)
          SavePartloadRatio = PartLoadFrac
          SaveSpeedRatio = SpeedRatio
        ELSE
          CALL SimDXCoilMultiSpeed(MSHeatPump(MSHeatPumpNum)%DXHeatCoilName,0.0d0,0.0d0,&
                                   MSHeatPump(MSHeatPumpNum)%DXHeatCoilIndex,SpeedNum,MSHeatPump(MSHeatPumpNum)%OpMode,CompOp)
        END IF
        SaveCompressorPLR = DXCoilPartLoadRatio(MSHeatPump(MSHeatPumpNum)%DXHeatCoilIndex)
      ELSE
        CALL SimDXCoilMultiSpeed(MSHeatPump(MSHeatPumpNum)%DXHeatCoilName,0.0d0,0.0d0,&
                                 MSHeatPump(MSHeatPumpNum)%DXHeatCoilIndex,SpeedNum,MSHeatPump(MSHeatPumpNum)%OpMode,CompOp)
      END IF
    ELSEIF(MSHeatPump(MSHeatPumpNum)%HeatCoilType .EQ. Coil_HeatingElectric_MultiStage .OR. &
           MSHeatPump(MSHeatPumpNum)%HeatCoilType .EQ. Coil_HeatingGas_MultiStage)THEN
      IF (QZnReq .GT. SmallLoad) THEN
        CALL SimulateHeatingCoilComponents(MSHeatPump(MSHeatPumpNum)%HeatCoilName,FirstHVACIteration, &
                                           CompIndex     = 0, &
                                           FanOpMode     = MSHeatPump(MSHeatPumpNum)%OpMode,      &
                                           PartLoadRatio = PartLoadFrac, &
                                           StageNum      = SpeedNum,     &
                                           SpeedRatio    = SpeedRatio)
      ELSE
        CALL SimulateHeatingCoilComponents(MSHeatPump(MSHeatPumpNum)%HeatCoilName,FirstHVACIteration, &
                                           CompIndex     = 0, &
                                           FanOpMode     = MSHeatPump(MSHeatPumpNum)%OpMode,      &
                                           PartLoadRatio = 0.0d0,        &
                                           StageNum      = SpeedNum,     &
                                           SpeedRatio    = 0.0d0)
      ENDIF
    ELSE
      CALL CalcNonDXHeatingCoils(MSHeatPumpNum,FirstHVACIteration,QZnReq,MSHeatPump(MSHeatPumpNum)%OpMode,QCoilActual, PartLoadFrac)
    END IF
    !  Simulate supplemental heating coil for blow through fan
    IF(MSHeatPump(MSHeatPumpNum)%SuppHeatCoilNum .GT. 0)THEN
      CALL CalcNonDXHeatingCoils(MSHeatPumpNum,FirstHVACIteration,SupHeaterLoad,MSHeatPump(MSHeatPumpNum)%OpMode,QCoilActual)
    ENDIF
  ELSE ! otherwise simulate DX coils then fan then supplemental heater
    IF(QZnReq .LT. (-1.d0*SmallLoad))THEN
      IF(OutsideDryBulbTemp .GT. MSHeatPump(MSHeatPumpNum)%MinOATCompressor)THEN
        CALL SimDXCoilMultiSpeed(MSHeatPump(MSHeatPumpNum)%DXCoolCoilName,SpeedRatio,PartLoadFrac,&
                                 MSHeatPump(MSHeatPumpNum)%DXCoolCoilIndex,SpeedNum,MSHeatPump(MSHeatPumpNum)%OpMode,CompOp)
        SavePartloadRatio = PartLoadFrac
        SaveSpeedRatio = SpeedRatio
      ELSE
        CALL SimDXCoilMultiSpeed(MSHeatPump(MSHeatPumpNum)%DXCoolCoilName,0.0d0,0.0d0,&
                                 MSHeatPump(MSHeatPumpNum)%DXCoolCoilIndex,SpeedNum,MSHeatPump(MSHeatPumpNum)%OpMode,CompOp)
      END IF
      SaveCompressorPLR = DXCoilPartLoadRatio(MSHeatPump(MSHeatPumpNum)%DXCoolCoilIndex)
    ELSE
      CALL SimDXCoilMultiSpeed(MSHeatPump(MSHeatPumpNum)%DXCoolCoilName,0.0d0,0.0d0,&
                               MSHeatPump(MSHeatPumpNum)%DXCoolCoilIndex,SpeedNum,MSHeatPump(MSHeatPumpNum)%OpMode,CompOp)
    END IF
    IF (MSHeatPump(MSHeatPumpNum)%HeatCoilType .EQ. MultiSpeedHeatingCoil)THEN
      IF (QZnReq .GT. SmallLoad) THEN
        IF(OutsideDryBulbTemp .GT. MSHeatPump(MSHeatPumpNum)%MinOATCompressor)THEN
          CALL SimDXCoilMultiSpeed(MSHeatPump(MSHeatPumpNum)%DXHeatCoilName,SpeedRatio,PartLoadFrac,&
                                   MSHeatPump(MSHeatPumpNum)%DXHeatCoilIndex,SpeedNum,MSHeatPump(MSHeatPumpNum)%OpMode,CompOp)
          SavePartloadRatio = PartLoadFrac
          SaveSpeedRatio = SpeedRatio
        ELSE
          CALL SimDXCoilMultiSpeed(MSHeatPump(MSHeatPumpNum)%DXHeatCoilName,0.0d0,0.0d0,&
                                   MSHeatPump(MSHeatPumpNum)%DXHeatCoilIndex,SpeedNum,MSHeatPump(MSHeatPumpNum)%OpMode,CompOp)
        END IF
        SaveCompressorPLR = DXCoilPartLoadRatio(MSHeatPump(MSHeatPumpNum)%DXHeatCoilIndex)
      ELSE
        CALL SimDXCoilMultiSpeed(MSHeatPump(MSHeatPumpNum)%DXHeatCoilName,0.0d0,0.0d0,&
                                   MSHeatPump(MSHeatPumpNum)%DXHeatCoilIndex,SpeedNum,MSHeatPump(MSHeatPumpNum)%OpMode,CompOp)
      END IF
    ELSEIF (MSHeatPump(MSHeatPumpNum)%HeatCoilType .EQ. Coil_HeatingElectric_MultiStage .OR. &
           MSHeatPump(MSHeatPumpNum)%HeatCoilType .EQ. Coil_HeatingGas_MultiStage)THEN
      IF (QZnReq .GT. SmallLoad) THEN
        CALL SimulateHeatingCoilComponents(MSHeatPump(MSHeatPumpNum)%HeatCoilName,FirstHVACIteration, &
                                           CompIndex     = 0, &
                                           FanOpMode     = MSHeatPump(MSHeatPumpNum)%OpMode,      &
                                           PartLoadRatio = PartLoadFrac, &
                                           StageNum      = SpeedNum,     &
                                           SpeedRatio    = SpeedRatio)
      ELSE
        CALL SimulateHeatingCoilComponents(MSHeatPump(MSHeatPumpNum)%HeatCoilName,FirstHVACIteration, &
                                           CompIndex     = 0, &
                                           FanOpMode     = MSHeatPump(MSHeatPumpNum)%OpMode,      &
                                           PartLoadRatio = 0.0d0,        &
                                           StageNum      = SpeedNum,     &
                                           SpeedRatio    = 0.0d0)
      ENDIF
    ELSE
      CALL CalcNonDXHeatingCoils(MSHeatPumpNum,FirstHVACIteration,QZnReq,MSHeatPump(MSHeatPumpNum)%OpMode,QCoilActual, PartLoadFrac)
    END IF
    CALL SimulateFanComponents(MSHeatPump(MSHeatPumpNum)%FanName,FirstHVACIteration,MSHeatPump(MSHeatPumpNum)%FanNum,FanSpeedRatio)
    !  Simulate supplemental heating coil for draw through fan
    IF(MSHeatPump(MSHeatPumpNum)%SuppHeatCoilNum .GT. 0)THEN
      CALL CalcNonDXHeatingCoils(MSHeatPumpNum,FirstHVACIteration,SupHeaterLoad,MSHeatPump(MSHeatPumpNum)%OpMode,QCoilActual)
    ENDIF
  END IF

! calculate sensible load met using delta enthalpy at a constant (minimum) humidity ratio)
  MinHumRat = Node(MSHeatPump(MSHeatPumpNum)%NodeNumofControlledZone)%HumRat
  IF(Node(OutletNode)%Temp .LT. Node(MSHeatPump(MSHeatPumpNum)%NodeNumofControlledZone)%Temp ) &
    MinHumRat = Node(OutletNode)%HumRat
  LoadMet = AirMassFlow * (PsyHFnTdbW(Node(OutletNode)%Temp,MinHumRat)  &
         - PsyHFnTdbW(Node(MSHeatPump(MSHeatPumpNum)%NodeNumofControlledZone)%Temp,MinHumRat))-MSHeatPump(MSHeatPumpNum)%LoadLoss

  MSHeatPump(MSHeatPumpNum)%LoadMet = LoadMet

RETURN
END SUBROUTINE CalcMSHeatPump


!******************************************************************************

REAL(r64) FUNCTION MSHPCyclingResidual(PartLoadFrac,Par)
          ! FUNCTION INFORMATION:
          !       AUTHOR         Lixing Gu
          !       DATE WRITTEN   June 2007
          !       MODIFIED       na
          !       RE-ENGINEERED  Revised for multispeed heat pump use based on DXCoilCyclingResidual

          ! PURPOSE OF THIS FUNCTION:
          !  Calculates residual function ((ActualOutput - QZnReq)/QZnReq)
          !  MSHP output depends on the part load ratio which is being varied to zero the residual.

          ! METHODOLOGY EMPLOYED:
          !  Calls CalcMSHeatPump to get ActualOutput at the given part load ratio
          !  and calculates the residual as defined above

          ! REFERENCES:

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  REAL(r64), INTENT(IN)   :: PartLoadFrac ! compressor cycling ratio (1.0 is continuous, 0.0 is off)
  REAL(r64), INTENT(IN), DIMENSION(:), OPTIONAL :: Par ! par(1) = MSHPNum
                                                  ! par(2) = Zone Num
                                                  ! par(3) = FirstHVACIteration
                                                  ! par(4) = OpMode
                                                  ! par(5) = QZnReq
                                                  ! par(6) = OnOffAirFlowRatio
                                                  ! par(7) = SupHeaterLoad
                                                  ! par(9) = CompOp

          ! FUNCTION PARAMETER DEFINITIONS:
          !  na

          ! INTERFACE BLOCK SPECIFICATIONS
          !  na

          ! DERIVED TYPE DEFINITIONS
          !  na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER :: MSHeatPumpNum      ! MSHP index
  INTEGER :: ZoneNum            ! Zone index
  LOGICAL :: FirstHVACIteration ! FirstHVACIteration flag
  INTEGER :: OpMode             ! Compressor operating mode
  REAL(r64)    :: QZnReq             ! zone load (W)
  REAL(r64)    :: OnOffAirFlowRatio  ! ratio of compressor ON airflow to average airflow over timestep
  REAL(r64)    :: ActualOutput       ! delivered capacity of MSHP
  REAL(r64)    :: SupHeaterLoad      ! Supplemental heater load
  INTEGER :: CompOp             ! compressor operation; 1=on, 0=off

  MSHeatPumpNum = INT(Par(1))
  ZoneNum = INT(Par(2))
  ! FirstHVACIteration is a logical, Par is REAL(r64), so make 1.0=TRUE and 0.0=FALSE
  IF(Par(3) .EQ. 1.0d0)THEN
    FirstHVACIteration = .TRUE.
  ELSE
    FirstHVACIteration = .FALSE.
  END IF
  OpMode = INT(Par(4))
  QZnReq = Par(5)
  OnOffAirFlowRatio = Par(6)
  SupHeaterLoad     = Par(7)
  CompOp = INT(Par(9))

  CALL CalcMSHeatPump(MSHeatPumpNum,FirstHVACIteration,CompOp,1,0.0d0,PartLoadFrac,  &
                       ActualOutput,QZnReq,OnOffAirFlowRatio,SupHeaterLoad)

  MSHPCyclingResidual = (ActualOutput - QZnReq)/QZnReq
  RETURN

END FUNCTION MSHPCyclingResidual

!******************************************************************************

REAL(r64) FUNCTION MSHPVarSpeedResidual(SpeedRatio,Par)
          ! FUNCTION INFORMATION:
          !       AUTHOR         Lixing Gu
          !       DATE WRITTEN   June 2007
          !       MODIFIED       na L. Gu, Oct. 2006, revised for multispeed heat pump use
          !       RE-ENGINEERED  Revised for multispeed heat pump use based on DXCoilVarSpeedResidual

          ! PURPOSE OF THIS FUNCTION:
          !  Calculates residual function ((ActualOutput - QZnReq)/QZnReq)
          !  MSHP output depends on the part load ratio which is being varied to zero the residual.

          ! METHODOLOGY EMPLOYED:
          !  Calls CalcMSHeatPump to get ActualOutput at the given speed ratio (partload ratio for high speed)
          !  and calculates the residual as defined above

          ! REFERENCES:

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  REAL(r64), INTENT(IN)   :: SpeedRatio ! compressor cycling ratio (1.0 is continuous, 0.0 is off)
  REAL(r64), INTENT(IN), DIMENSION(:), OPTIONAL :: Par ! par(1) = MSHPNum
                                                  ! par(2) = Zone Num
                                                  ! par(3) = FirstHVACIteration
                                                  ! par(4) = OpMode
                                                  ! par(5) = QZnReq
                                                  ! par(6) = OnOffAirFlowRatio
                                                  ! par(7) = SupHeaterLoad
                                                  ! par(8) = SpeedNum
                                                  ! par(9) = CompOp

          ! FUNCTION PARAMETER DEFINITIONS:
          !  na

          ! INTERFACE BLOCK SPECIFICATIONS
          !  na

          ! DERIVED TYPE DEFINITIONS
          !  na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER :: MSHeatPumpNum      ! MSHP index
  INTEGER :: ZoneNum            ! Zone index
  LOGICAL :: FirstHVACIteration ! FirstHVACIteration flag
  INTEGER :: OpMode             ! Compressor operating mode
  REAL(r64)    :: QZnReq             ! zone load (W)
  REAL(r64)    :: OnOffAirFlowRatio  ! ratio of compressor ON airflow to average airflow over timestep
  REAL(r64)    :: ActualOutput       ! delivered capacity of MSHP
  REAL(r64)    :: SupHeaterLoad      ! Supplemental heater load
  INTEGER :: SpeedNum           ! Speed number
  INTEGER :: CompOp             ! compressor operation; 1=on, 0=off

  MSHeatPumpNum = INT(Par(1))
  ZoneNum = INT(Par(2))
  ! FirstHVACIteration is a logical, Par is REAL(r64), so make 1.0=TRUE and 0.0=FALSE
  IF(Par(3) .EQ. 1.0d0)THEN
    FirstHVACIteration = .TRUE.
  ELSE
    FirstHVACIteration = .FALSE.
  END IF
  OpMode = INT(Par(4))
  QZnReq = Par(5)
  OnOffAirFlowRatio = Par(6)
  SupHeaterLoad     = Par(7)
  SpeedNum     = INT(Par(8))
  CompOp = INT(Par(9))

  CALL CalcMSHeatPump(MSHeatPumpNum,FirstHVACIteration,CompOp,SpeedNum,SpeedRatio,1.0d0,  &
                       ActualOutput,QZnReq,OnOffAirFlowRatio,SupHeaterLoad)

  MSHPVarSpeedResidual = (ActualOutput - QZnReq)/QZnReq
  RETURN

END FUNCTION MSHPVarSpeedResidual

!******************************************************************************

SUBROUTINE UpdateMSHeatPump(MSHeatPumpNum)
            ! SUBROUTINE INFORMATION:
            !       AUTHOR:          Lixing Gu, FSEC
            !       DATE WRITTEN:    June 2007
            !       MODIFIED         na
            !       RE-ENGINEERED    na

            ! PURPOSE OF THIS SUBROUTINE:
            !  This routine will update MSHP performance and calculate heat recovery rate and crankcase heater power

            ! METHODOLOGY EMPLOYED:
            ! na

            ! REFERENCES: na

            ! USE STATEMENTS:
  USE DataAirLoop, ONLY: LoopSystemOnMassFlowrate,LoopSystemOffMassFlowrate,LoopFanOperationMode,LoopOnOffFanPartLoadRatio, &
                         LoopCompCycRatio

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER         , INTENT (IN)           :: MSHeatPumpNum      ! Engine driven heat pump number

  ! Calculate heat recovery
  If (MSHeatPump(MSHeatPumpNum)%HeatRecActive) then
    Call MSHPHeatRecovery(MSHeatPumpNum)
  End If

  LoopSystemOnMassFlowrate = CompOnMassFlow
  LoopSystemOffMassFlowrate = CompOffMassFlow
  LoopFanOperationMode = MSHeatPump(MSHeatPumpNum)%OpMode
  LoopOnOffFanPartLoadRatio = MSHeatPump(MSHeatPumpNum)%FanPartLoadRatio
  LoopCompCycRatio = MSHeatPumpReport(MSHeatPumpNum)%CycRatio

RETURN
END SUBROUTINE UpdateMSHeatPump
!******************************************************************************

SUBROUTINE ReportMSHeatPump(MSHeatPumpNum)
            ! SUBROUTINE INFORMATION:
            !       AUTHOR:          Lixing Gu, FSEC
            !       DATE WRITTEN:    June 2007
            !       MODIFIED         na
            !       RE-ENGINEERED    na

            ! PURPOSE OF THIS SUBROUTINE:
            !  This routine will write values to output variables in MSHP

            ! METHODOLOGY EMPLOYED:
            !

            ! REFERENCES: na

            ! USE STATEMENTS:
  Use DataHVACGlobals, ONLY: TimeStepSys

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER         , INTENT (IN)           :: MSHeatPumpNum      ! Engine driven heat pump number
          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64) :: ReportingConstant

  ReportingConstant = TimeStepSys*SecInHour
  MSHeatPumpReport(MSHeatPumpNum)%ElecPowerConsumption = MSHeatPump(MSHeatPumpNum)%ElecPower*ReportingConstant ! + &
  MSHeatPumpReport(MSHeatPumpNum)%HeatRecoveryEnergy = MSHeatPump(MSHeatPumpNum)%HeatRecoveryRate*ReportingConstant

  MSHeatPumpReport(MSHeatPumpNum)%AuxElecHeatConsumption = 0.0d0
  MSHeatPumpReport(MSHeatPumpNum)%AuxElecCoolConsumption = 0.0d0

  MSHeatPump(MSHeatPumpNum)%AuxElecPower = MSHeatPump(MSHeatPumpNum)%AuxOnCyclePower*SaveCompressorPLR + &
                                           MSHeatPump(MSHeatPumpNum)%AuxOffCyclePower*(1.0d0-SaveCompressorPLR)
  If (MSHeatPump(MSHeatPumpNum)%HeatCoolMode == CoolingMode) Then
    MSHeatPumpReport(MSHeatPumpNum)%AuxElecCoolConsumption = MSHeatPump(MSHeatPumpNum)%AuxOnCyclePower*SaveCompressorPLR &
                                                             *ReportingConstant
  End If
  If (MSHeatPump(MSHeatPumpNum)%HeatCoolMode == HeatingMode) Then
    MSHeatPumpReport(MSHeatPumpNum)%AuxElecHeatConsumption = MSHeatPump(MSHeatPumpNum)%AuxOnCyclePower*SaveCompressorPLR &
                                                             *ReportingConstant
  End If
  If (MSHeatPump(MSHeatPumpNum)%LastMode == HeatingMode) Then
    MSHeatPumpReport(MSHeatPumpNum)%AuxElecHeatConsumption = MSHeatPumpReport(MSHeatPumpNum)%AuxElecHeatConsumption+ &
                                MSHeatPump(MSHeatPumpNum)%AuxOffCyclePower*(1.0d0-SaveCompressorPLR)*ReportingConstant
  Else
    MSHeatPumpReport(MSHeatPumpNum)%AuxElecCoolConsumption = MSHeatPumpReport(MSHeatPumpNum)%AuxElecCoolConsumption+ &
                                MSHeatPump(MSHeatPumpNum)%AuxOffCyclePower*(1.0d0-SaveCompressorPLR)*ReportingConstant
  End If

RETURN
END SUBROUTINE ReportMSHeatPump

SUBROUTINE MSHPHeatRecovery(MSHeatPumpNum)
            ! SUBROUTINE INFORMATION:
            !       AUTHOR:          Lixing Gu
            !       DATE WRITTEN:    June 2007
            !       MODIFIED:        na
            !       RE-ENGINEERED    Revised to calculate MSHP heat recovery rate based on EIR Chiller heat recovery subroutine
            ! PURPOSE OF THIS SUBROUTINE:
            !  Calculate the heat recovered from MSHP

            ! METHODOLOGY EMPLOYED:
            !  na

            ! REFERENCES:
            !  na

            ! USE STATEMENTS:
  USE DataHVACGlobals,     ONLY: MSHPWasteHeat
  USE FluidProperties,     ONLY: GetSpecificHeatGlycol
  USE DataPlant,           ONLY: PlantLoop
  USE PlantUtilities,      ONLY: SafeCopyPlantNode

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN)     :: MSHeatPumpNum ! Number of the current electric MSHP being simulated

          ! SUBROUTINE PARAMETER DEFINITIONS:
          !  na

          ! DERIVMS TYPE DEFINITIONS:
          !  na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: HeatRecInNode       ! Node number of heat recovery water inlet node
  INTEGER :: HeatRecOutNode      ! Node number of heat recovery water outlet node
  REAL(r64)    :: QHeatRec              ! Total heat recovered [W]
  REAL(r64)    :: HeatRecInletTemp    ! Heat reclaim inlet temp [C]
  REAL(r64)    :: HeatRecOutletTemp   ! Heat reclaim outlet temp [C]
  REAL(r64)    :: HeatRecMassFlowRate ! Heat reclaim mass flow rate [m3/s]
  REAL(r64)    :: CpHeatRec           ! Heat reclaim water inlet specific heat [J/kg-K]
  REAL(r64)    :: HeatRecInletEnth    ! Heat reclaim water inlet enthalpy [J/kg]

  ! Begin routine
  HeatRecInNode  = MSHeatPump(MSHeatPumpNum)%HeatRecInletNodeNum
  HeatRecOutNode = MSHeatPump(MSHeatPumpNum)%HeatRecOutletNodeNum

   ! Inlet node to the heat recovery heat exchanger
  HeatRecInletTemp  = Node(HeatRecInNode)%Temp
  HeatRecInletEnth  = Node(HeatRecInNode)%Enthalpy

   ! Set heat recovery mass flow rates
  HeatRecMassFlowRate = Node(HeatRecInNode)%MassFlowRate

  QHeatRec = MSHPWasteHeat

  IF (HeatRecMassFlowRate > 0.0d0) THEN

    CpHeatRec = GetSpecificHeatGlycol(PlantLoop(MSHeatPump(MSHeatPumpNum)%HRLoopNum)%FluidName, &
                                      HeatRecInletTemp, &
                                      PlantLoop(MSHeatPump(MSHeatPumpNum)%HRLoopNum)%FluidIndex, &
                                      'MSHPHeatRecovery')

    HeatRecOutletTemp = QHeatRec/(HeatRecMassFlowRate*CpHeatRec) + HeatRecInletTemp
    IF (HeatRecOutletTemp .GT. MSHeatPump(MSHeatPumpNum)%MaxHeatRecOutletTemp) &
        HeatRecOutletTemp = MSHeatPump(MSHeatPumpNum)%MaxHeatRecOutletTemp
  ELSE
    HeatRecOutletTemp = HeatRecInletTemp
  END IF

  CALL SafeCopyPlantNode(HeatRecInNode, HeatRecOutNode)
  ! changed outputs
  Node(HeatRecOutNode)%Temp         = HeatRecOutletTemp

  MSHeatPump(MSHeatPumpNum)%HeatRecoveryRate = QHeatRec
  MSHeatPump(MSHeatPumpNum)%HeatRecoveryInletTemp  = HeatRecInletTemp
  MSHeatPump(MSHeatPumpNum)%HeatRecoveryOutletTemp = HeatRecOutletTemp
  MSHeatPump(MSHeatPumpNum)%HeatRecoveryMassFlowRate = HeatRecMassFlowRate


  RETURN

END SUBROUTINE MSHPHeatRecovery

SUBROUTINE SetAverageAirFlow(MSHeatPumpNum,PartLoadRatio,OnOffAirFlowRatio,SpeedNum,SpeedRatio)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Lixing
          !       DATE WRITTEN   June 2007
          !       MODIFIED       na
          !       RE-ENGINEERED  Resived to meet requirements of multispeed heat pump based on the same subroutine
          !                      in PTHP module

          ! PURPOSE OF THIS SUBROUTINE:
          ! Set the average air mass flow rates using the part load fraction of the heat pump for this time step
          ! Set OnOffAirFlowRatio to be used by DX coils

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataZoneEnergyDemands,      ONLY: CurDeadBandOrSetback
  USE DataHVACGlobals,            ONLY: MSHPMassFlowRateLow, MSHPMassFlowRateHigh

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN)    :: MSHeatPumpNum      ! Unit index
  REAL(r64)   , INTENT (IN)    :: PartLoadRatio      ! unit part load ratio
  REAL(r64)   , INTENT (INOUT) :: OnOffAirFlowRatio  ! ratio of compressor ON airflow to average airflow over timestep
  INTEGER, INTENT (IN),OPTIONAL :: SpeedNum     ! Speed number
  REAL(r64),    INTENT (IN),OPTIONAL :: SpeedRatio   ! Speed ratio

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVMS TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER             :: InletNode           ! inlet node number for PTHPNum
  REAL(r64)           :: AverageUnitMassFlow ! average supply air mass flow rate over time step

  MSHPMassFlowRateLow = 0.0d0             ! Mass flow rate at low speed
  MSHPMassFlowRateHigh = 0.0d0            ! Mass flow rate at high speed


  If (.NOT. CurDeadbandOrSetback(MSHeatPump(MSHeatPumpNum)%ControlZoneNum) .AND. Present(SpeedNum) ) Then
    If (MSHeatPump(MSHeatPumpNum)%HeatCoolMode == HeatingMode) Then
      If (SpeedNum .eq. 1) Then
        CompOnMassFlow = MSHeatPump(MSHeatPumpNum)%HeatMassFlowRate(SpeedNum)
        CompOnFlowRatio = MSHeatPump(MSHeatPumpNum)%HeatingSpeedRatio(SpeedNum)
        MSHPMassFlowRateLow = MSHeatPump(MSHeatPumpNum)%HeatMassFlowRate(1)
        MSHPMassFlowRateHigh = MSHeatPump(MSHeatPumpNum)%HeatMassFlowRate(1)
      Else If (SpeedNum .GT. 1) Then
        CompOnMassFlow = SpeedRatio*MSHeatPump(MSHeatPumpNum)%HeatMassFlowRate(SpeedNum) + &
                         (1.0d0-SpeedRatio)*MSHeatPump(MSHeatPumpNum)%HeatMassFlowRate(SpeedNum-1)
        CompOnFlowRatio = SpeedRatio*MSHeatPump(MSHeatPumpNum)%HeatingSpeedRatio(SpeedNum) + &
                         (1.0d0-SpeedRatio)*MSHeatPump(MSHeatPumpNum)%HeatingSpeedRatio(SpeedNum-1)
        MSHPMassFlowRateLow = MSHeatPump(MSHeatPumpNum)%HeatMassFlowRate(SpeedNum-1)
        MSHPMassFlowRateHigh = MSHeatPump(MSHeatPumpNum)%HeatMassFlowRate(SpeedNum)
      End If
    Else If (MSHeatPump(MSHeatPumpNum)%HeatCoolMode == CoolingMode) Then
      If (SpeedNum .eq. 1) Then
        CompOnMassFlow = MSHeatPump(MSHeatPumpNum)%CoolMassFlowRate(SpeedNum)
        CompOnFlowRatio = MSHeatPump(MSHeatPumpNum)%CoolingSpeedRatio(SpeedNum)
        MSHPMassFlowRateLow = MSHeatPump(MSHeatPumpNum)%CoolMassFlowRate(1)
        MSHPMassFlowRateHigh = MSHeatPump(MSHeatPumpNum)%CoolMassFlowRate(1)
      Else If (SpeedNum .GT. 1) Then
        CompOnMassFlow = SpeedRatio*MSHeatPump(MSHeatPumpNum)%CoolMassFlowRate(SpeedNum) + &
                         (1.0d0-SpeedRatio)*MSHeatPump(MSHeatPumpNum)%CoolMassFlowRate(SpeedNum-1)
        CompOnFlowRatio = SpeedRatio*MSHeatPump(MSHeatPumpNum)%CoolingSpeedRatio(SpeedNum) + &
                         (1.0d0-SpeedRatio)*MSHeatPump(MSHeatPumpNum)%CoolingSpeedRatio(SpeedNum-1)
        MSHPMassFlowRateLow = MSHeatPump(MSHeatPumpNum)%CoolMassFlowRate(SpeedNum-1)
        MSHPMassFlowRateHigh = MSHeatPump(MSHeatPumpNum)%CoolMassFlowRate(SpeedNum)
      End If
    End If
  End If
  InletNode      = MSHeatPump(MSHeatPumpNum)%AirInletNodeNum

  ! Set up fan flow rate during compressor off time
  If (MSHeatPump(MSHeatPumpNum)%OpMode .EQ. ContFanCycCoil .AND. Present(SpeedNum)) Then
    IF (MSHeatPump(MSHeatPumpNum)%AirFlowControl .EQ. UseCompressorOnFlow .AND. CompOnMassFlow > 0.0d0) THEN
      IF (MSHeatPump(MSHeatPumpNum)%LastMode .EQ. HeatingMode) THEN
        CompOffMassFlow = MSHeatPump(MSHeatPumpNum)%HeatMassFlowRate(SpeedNum)
        CompOffFlowRatio = MSHeatPump(MSHeatPumpNum)%HeatingSpeedRatio(SpeedNum)
      ELSE
        CompOffMassFlow = MSHeatPump(MSHeatPumpNum)%CoolMassFlowRate(SpeedNum)
        CompOffFlowRatio = MSHeatPump(MSHeatPumpNum)%CoolingSpeedRatio(SpeedNum)
      END IF
    END IF
  End If

  If (Present(SpeedNum)) Then
    If (SpeedNum > 1) Then
      AverageUnitMassFlow = CompOnMassFlow
      FanSpeedRatio       = CompOnFlowRatio
    Else
      AverageUnitMassFlow = (PartLoadRatio * CompOnMassFlow) + ((1-PartLoadRatio) * CompOffMassFlow)
      IF(CompOffFlowRatio .GT. 0.0d0)THEN
        FanSpeedRatio = (PartLoadRatio * CompOnFlowRatio) + ((1-PartLoadRatio) * CompOffFlowRatio)
      ELSE
        FanSpeedRatio     = CompOnFlowRatio
      END IF
    End If
  Else
    AverageUnitMassFlow = (PartLoadRatio * CompOnMassFlow) + ((1-PartLoadRatio) * CompOffMassFlow)
    IF(CompOffFlowRatio .GT. 0.0d0)THEN
      FanSpeedRatio = (PartLoadRatio * CompOnFlowRatio) + ((1-PartLoadRatio) * CompOffFlowRatio)
    ELSE
      FanSpeedRatio       = CompOnFlowRatio
    END IF
  End If

!!!LKL Discrepancy with > 0
  IF (GetCurrentScheduleValue(MSHeatPump(MSHeatPumpNum)%AvaiSchedPtr) .EQ. 0.0d0) THEN
    Node(InletNode)%MassFlowRate              = 0.0d0
    OnOffAirFlowRatio                         = 0.0d0
  ELSE
    Node(InletNode)%MassFlowRate              = AverageUnitMassFlow
    Node(InletNode)%MassFlowRateMaxAvail      = AverageUnitMassFlow
    IF (AverageUnitMassFlow .GT. 0.0d0) THEN
      OnOffAirFlowRatio                       = CompOnMassFlow / AverageUnitMassFlow
    ELSE
      OnOffAirFlowRatio                       = 0.0d0
    END IF
  END IF

  RETURN
END SUBROUTINE SetAverageAirFlow

SUBROUTINE CalcNonDXHeatingCoils(MSHeatPumpNum,FirstHVACIteration,HeatingLoad,FanMode,HeatCoilLoadmet,PartLoadFrac)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Bereket Nigusse, FSEC/UCF
          !       DATE WRITTEN   January 2012
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine simulates the four non dx heating coil types: Gas, Electric, hot water and steam.

          ! METHODOLOGY EMPLOYED:
          ! Simply calls the different heating coil component.  The hot water flow rate matching the coil load
          ! is calculated iteratively.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:

  USE HeatingCoils,              ONLY: SimulateHeatingCoilComponents
  USE WaterCoils,                ONLY: SimulateWaterCoilComponents
  USE SteamCoils,                ONLY: SimulateSteamCoilComponents
  USE PlantUtilities,            ONLY: SetComponentFlowRate
  USE General,                   ONLY: SolveRegulaFalsi,RoundSigDigits
  USE DataHVACGlobals,           ONLY: SmallLoad

  IMPLICIT NONE     ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER,      INTENT(IN)    :: MSHeatPumpNum             ! multispeed heatpump index
  LOGICAL,      INTENT(IN)    :: FirstHVACIteration        ! flag for first HVAC iteration in the time step
  REAL(r64),    INTENT(IN)    :: HeatingLoad             ! supplemental coil load to be met by unit (watts)
  REAL(r64),    INTENT(OUT)   :: HeatCoilLoadmet           ! Heating Load Met
  INTEGER,      INTENT(IN)    :: FanMode                   ! fan operation mode
  REAL(r64),    INTENT(IN), OPTIONAL    :: PartLoadFrac

          ! SUBROUTINE PARAMETER DEFINITIONS:
  REAL(r64), PARAMETER :: ErrTolerance = 0.001d0    ! convergence limit for hotwater coil
  INTEGER, PARAMETER :: SolveMaxIter=50
  CHARACTER(len=*), PARAMETER :: CurrentModuleObject = 'AirLoopHVAC:UnitaryHeatPump:AirToAir:MultiSpeed'

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64)      :: QCoilRequired   ! heat addition required from an electric, gas, steam, or hot water coil
  REAL(r64)      :: QCoilActual     ! actual heating load met
  REAL(r64)      :: mdot            ! heating coil steam or hot water mass flow rate
  REAL(r64)      :: MinWaterFlow    ! coil minimum hot water mass flow rate, kg/s
  REAL(r64)      :: MaxHotWaterFlow ! coil maximum hot water mass flow rate, kg/s
  REAL(r64)      :: HotWaterMdot    ! actual hot water mass flow rate
  REAL(r64), DIMENSION(3) :: Par    !
  INTEGER        :: SolFlag

  CHARACTER(len=MaxNameLength) :: HeatCoilName  =' '
  INTEGER        :: HeatCoilType
  INTEGER        :: HeatCoilNum
  REAL(r64)      :: MaxCoilFluidFlow
  REAL(r64)      :: SteamCoilHeatingLoad
  INTEGER        :: CoilControlNode
  INTEGER        :: CoilOutletNode
  INTEGER        :: LoopNum
  INTEGER        :: LoopSide
  INTEGER        :: BranchNum
  INTEGER        :: CompNum

  QCoilActual=0.0d0

  IF (PRESENT(PartLoadFrac)) THEN
    HeatCoilType     = MSHeatPump(MSHeatPumpNum)%HeatCoilType
    HeatCoilName     = MSHeatPump(MSHeatPumpNum)%HeatCoilName
    HeatCoilNum      = MSHeatPump(MSHeatPumpNum)%HeatCoilNum
    MaxCoilFluidFlow = MSHeatPump(MSHeatPumpNum)%MaxCoilFluidFlow
    CoilControlNode  = MSHeatPump(MSHeatPumpNum)%CoilControlNode
    CoilOutletNode   = MSHeatPump(MSHeatPumpNum)%CoilOutletNode
    LoopNum          = MSHeatPump(MSHeatPumpNum)%LoopNum
    LoopSide         = MSHeatPump(MSHeatPumpNum)%LoopSide
    BranchNum        = MSHeatPump(MSHeatPumpNum)%BranchNum
    CompNum          = MSHeatPump(MSHeatPumpNum)%CompNum
  ELSE
    HeatCoilType     = MSHeatPump(MSHeatPumpNum)%SuppHeatCoilType
    HeatCoilName     = MSHeatPump(MSHeatPumpNum)%SuppHeatCoilName
    HeatCoilNum      = MSHeatPump(MSHeatPumpNum)%SuppHeatCoilNum
    MaxCoilFluidFlow = MSHeatPump(MSHeatPumpNum)%MaxSuppCoilFluidFlow
    CoilControlNode  = MSHeatPump(MSHeatPumpNum)%SuppCoilControlNode
    CoilOutletNode   = MSHeatPump(MSHeatPumpNum)%SuppCoilOutletNode
    LoopNum          = MSHeatPump(MSHeatPumpNum)%SuppLoopNum
    LoopSide         = MSHeatPump(MSHeatPumpNum)%SuppLoopSide
    BranchNum        = MSHeatPump(MSHeatPumpNum)%SuppBranchNum
    CompNum          = MSHeatPump(MSHeatPumpNum)%SuppCompNum
  ENDIF

  MSHeatPump(MSHeatPumpNum)%HotWaterLoopNum          = LoopNum
  MSHeatPump(MSHeatPumpNum)%HotWaterLoopSide         = LoopSide
  MSHeatPump(MSHeatPumpNum)%HotWaterBranchNum        = BranchNum
  MSHeatPump(MSHeatPumpNum)%HotWaterCompNum          = CompNum
  MSHeatPump(MSHeatPumpNum)%HotWaterCoilControlNode  = CoilControlNode
  MSHeatPump(MSHeatPumpNum)%HotWaterCoilOutletNode   = CoilOutletNode
  MSHeatPump(MSHeatPumpNum)%HotWaterCoilName         = HeatCoilName
  MSHeatPump(MSHeatPumpNum)%HotWaterCoilNum          = HeatCoilNum

  IF (HeatingLoad > SmallLoad) THEN

   Select Case (HeatCoilType)
    Case (SuppHeatingCoilGas, SuppHeatingCoilElec)
      CALL SimulateHeatingCoilComponents(HeatCoilName,FirstHVACIteration, &
                                         HeatingLoad, HeatCoilNum, &
                                         QCoilActual, .TRUE., FanMode)
    Case (Coil_HeatingWater)
       If (PRESENT(PartLoadFrac)) THEN
         MaxHotWaterFlow = MaxCoilFluidFlow * PartLoadFrac
         Call SetComponentFlowRate( MaxHotWaterFlow , &
                                    CoilControlNode, &
                                    CoilOutletNode, &
                                    LoopNum, &
                                    LoopSide, &
                                    BranchNum, &
                                    CompNum)
         CALL SimulateWaterCoilComponents(HeatCoilName,FirstHVACIteration, &
                                          HeatCoilNum, QCoilActual, FanMode)
       Else
         MaxHotWaterFlow = MaxCoilFluidFlow
         Call SetComponentFlowRate( MaxHotWaterFlow , &
                                    CoilControlNode, &
                                    CoilOutletNode, &
                                    LoopNum, &
                                    LoopSide, &
                                    BranchNum, &
                                    CompNum)
         CALL SimulateWaterCoilComponents(HeatCoilName,FirstHVACIteration, &
                                          HeatCoilNum, QCoilActual, FanMode)
         IF (QCoilActual > (HeatingLoad + SmallLoad)) THEN
            ! control water flow to obtain output matching HeatingLoad
            SolFlag = 0
            MinWaterFlow = 0.0d0
            Par(1) = REAL(MSHeatPumpNum,r64)
            IF (FirstHVACIteration) THEN
              Par(2) = 1.d0
            ELSE
              Par(2) = 0.0d0
            END IF
            Par(3) = HeatingLoad
            CALL SolveRegulaFalsi(ErrTolerance, SolveMaxIter, SolFlag, HotWaterMdot, HotWaterCoilResidual, &
                                  MinWaterFlow, MaxHotWaterFlow, Par)
            IF (SolFlag == -1) THEN
              IF (MSHeatPump(MSHeatPumpNum)%HotWaterCoilMaxIterIndex == 0) THEN
                CALL ShowWarningMessage('CalcNonDXHeatingCoils: Hot water coil control failed for '//  &
                trim(CurrentModuleObject)//'="'//  &
                TRIM(MSHeatPump(MSHeatPumpNum)%Name)//'"')
                CALL ShowContinueErrorTimeStamp(' ')
                CALL ShowContinueError('  Iteration limit ['//trim(RoundSigDigits(SolveMaxIter))//  &
                      '] exceeded in calculating hot water mass flow rate')
              ENDIF
              CALL ShowRecurringWarningErrorAtEnd('CalcNonDXHeatingCoils: Hot water coil control failed (iteration limit ['//  &
                    trim(RoundSigDigits(SolveMaxIter))//']) for '//trim(CurrentModuleObject)//'="'// &
                    TRIM(MSHeatPump(MSHeatPumpNum)%Name),MSHeatPump(MSHeatPumpNum)%HotWaterCoilMaxIterIndex)
            ELSE IF (SolFlag == -2) THEN
              IF (MSHeatPump(MSHeatPumpNum)%HotWaterCoilMaxIterIndex2 == 0) THEN
                CALL ShowWarningMessage('CalcNonDXHeatingCoils: Hot water coil control failed (maximum flow limits) for '//  &
                      trim(CurrentModuleObject)//'="'// &
                      TRIM(MSHeatPump(MSHeatPumpNum)%Name)//'"')
                CALL ShowContinueErrorTimeStamp(' ')
                CALL ShowContinueError('...Bad hot water maximum flow rate limits')
                CALL ShowContinueError('...Given minimum water flow rate='//trim(RoundSigDigits(MinWaterFlow,3))//' kg/s')
                CALL ShowContinueError('...Given maximum water flow rate='//trim(RoundSigDigits(MaxHotWaterFlow,3))//' kg/s')
              ENDIF
              CALL ShowRecurringWarningErrorAtEnd('CalcNonDXHeatingCoils: Hot water coil control failed (flow limits) for '//  &
                    trim(CurrentModuleObject)//'="'// &
                    TRIM(MSHeatPump(MSHeatPumpNum)%Name)//'"', &
                    MSHeatPump(MSHeatPumpNum)%HotWaterCoilMaxIterIndex2,  &
                    ReportMinOf=MinWaterFlow,ReportMaxOf=MaxHotWaterFlow,ReportMinUnits='[kg/s]',ReportMaxUnits='[kg/s]')

            END IF
          ! simulate hot water supplemental heating coil
            CALL SimulateWaterCoilComponents(HeatCoilName,FirstHVACIteration, &
                                           HeatCoilNum, QCoilActual, FanMode)
         ENDIF
       Endif
    Case (Coil_HeatingSteam)
       If (PRESENT(PartLoadFrac)) THEN
         mdot = MSHeatPump(MSHeatPumpNum)%MaxCoilFluidFlow * PartLoadFrac
         SteamCoilHeatingLoad = HeatingLoad * PartLoadFrac
       Else
         mdot = MSHeatPump(MSHeatPumpNum)%MaxCoilFluidFlow
         SteamCoilHeatingLoad = HeatingLoad
       Endif
       Call SetComponentFlowRate( mdot , &
                                 CoilControlNode, &
                                 CoilOutletNode, &
                                 LoopNum, &
                                 LoopSide, &
                                 BranchNum, &
                                 CompNum)
       ! simulate steam supplemental heating coil
       CALL SimulateSteamCoilComponents(HeatCoilName, &
                                       FirstHVACIteration,  SteamCoilHeatingLoad,      &
                                       HeatCoilNum,   &
                                       QCoilActual, FanMode)
    END Select

  ELSE  ! end of IF (HeatingLoad > SmallLoad) THEN

    Select Case (HeatCoilType)
     Case (SuppHeatingCoilGas, SuppHeatingCoilElec)
       CALL SimulateHeatingCoilComponents(HeatCoilName,FirstHVACIteration, &
                                          HeatingLoad, HeatCoilNum, &
                                          QCoilActual, .TRUE., FanMode)
     Case (Coil_HeatingWater)
       mdot = 0.0d0
       Call SetComponentFlowRate( mdot , &
                                  CoilControlNode, &
                                  CoilOutletNode, &
                                  LoopNum, &
                                  LoopSide, &
                                  BranchNum, &
                                  CompNum)
       CALL SimulateWaterCoilComponents(HeatCoilName,FirstHVACIteration, &
                                         HeatCoilNum, QCoilActual, FanMode)
     Case (Coil_HeatingSteam)
       mdot = 0.0d0
       Call SetComponentFlowRate( mdot , &
                                  CoilControlNode, &
                                  CoilOutletNode, &
                                  LoopNum, &
                                  LoopSide, &
                                  BranchNum, &
                                  CompNum)
       ! simulate the steam supplemental heating coil
       CALL SimulateSteamCoilComponents(HeatCoilName, &
                                        FirstHVACIteration,  HeatingLoad,      &
                                        HeatCoilNum,   &
                                        QCoilActual, FanMode)
    END Select
  ENDIF
  HeatCoilLoadmet = QCoilActual

 RETURN
END SUBROUTINE CalcNonDXHeatingCoils

FUNCTION HotWaterCoilResidual(HWFlow, Par) RESULT (Residuum)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Bereket Nigusse, FSEC/UCF
          !       DATE WRITTEN   November 2011
          !       MODIFIED
          !       RE-ENGINEERED

          ! PURPOSE OF THIS FUNCTION:
          ! Calculates residual function (QCoilActual - SupHeatCoilLoad) / SupHeatCoilLoad
          ! coil actual output depends on the hot water flow rate which is varied to minimize the
          ! residual.

          ! METHODOLOGY EMPLOYED:
          ! Calls HotWaterCoilResidual, and calculates the residual as defined above.
          !

          ! REFERENCES:

          ! USE STATEMENTS:
  USE WaterCoils,     ONLY: SimulateWaterCoilComponents
  USE PlantUtilities, ONLY: SetComponentFlowRate


  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  REAL(r64), INTENT(IN)                         :: HWFlow   ! hot water flow rate in kg/s
  REAL(r64), INTENT(IN), DIMENSION(:), OPTIONAL :: Par      ! Par(5) is the requested coil load
  REAL(r64)                                     :: Residuum ! residual to be minimized to zero

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER               :: MSHeatPumpNum
  LOGICAL               :: FirstHVACSoln
  REAL(r64)             :: QCoilActual             ! delivered coild load, W
  REAL(r64)             :: HeatCoilLoad         ! requested coild load, W
  REAL(r64)             :: mdot

  MSHeatPumpNum = INT(Par(1))
  IF (Par(2) > 0.0d0) THEN
    FirstHVACSoln = .TRUE.
  ELSE
    FirstHVACSoln = .FALSE.
  END IF
  HeatCoilLoad =  Par(3)
  QCoilActual = HeatCoilLoad
  mdot = HWFlow
  Call SetComponentFlowRate( mdot , &
                             MSHeatPump(MSHeatPumpNum)%HotWaterCoilControlNode, &
                             MSHeatPump(MSHeatPumpNum)%HotWaterCoilOutletNode, &
                             MSHeatPump(MSHeatPumpNum)%HotWaterLoopNum, &
                             MSHeatPump(MSHeatPumpNum)%HotWaterLoopSide, &
                             MSHeatPump(MSHeatPumpNum)%HotWaterBranchNum, &
                             MSHeatPump(MSHeatPumpNum)%HotWaterCompNum)
  ! simulate the hot water supplemental heating coil
  CALL SimulateWaterCoilComponents(MSHeatPump(MSHeatPumpNum)%HotWaterCoilName,FirstHVACSoln, &
                                   MSHeatPump(MSHeatPumpNum)%HotWaterCoilNum, QCoilActual, &
                                   MSHeatPump(MSHeatPumpNum)%OpMode)
  IF (HeatCoilLoad /= 0.0d0) THEN
    Residuum = (QCoilActual - HeatCoilLoad)/ HeatCoilLoad
  ELSE !Objexx:Return Condition added to assure return value is set
    Residuum = 0.0d0
  ENDIF
  RETURN
END FUNCTION HotWaterCoilResidual

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

END MODULE HVACMultiSpeedHeatPump
