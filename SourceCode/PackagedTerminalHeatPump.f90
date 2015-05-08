MODULE PackagedTerminalHeatPump

  ! Module containing the routines for modeling packaged terminal air conditioners and heat pumps

  ! MODULE INFORMATION:
  !       AUTHOR         Richard Raustad
  !       DATE WRITTEN   July 2005
  !       MODIFIED       B. Griffith Dec. 2006 added Function call for OA node and moved get input flag up to Module
  !                      B. Griffith, Sept 2010, plant upgrades, fluid properties, for heating coils
  !                      B. Nigusse, Jan 2012, added hot water and steam heating coils to PTHP and WSHP
  !                      Bo Shen, ORNL, March 2012, added variable-speed water-source heat pump
  !       RE-ENGINEERED  na

  ! PURPOSE OF THIS MODULE:
  ! To encapsulate the data and algorithms needed to simulate packaged
  ! terminal units, which are considered "Zone Equipment" in EnergyPlus

  ! METHODOLOGY EMPLOYED:
  ! Units are modeled as a collection of components: outside air mixer, supply air fan, DX cooling coils,
  ! DX heating coil (PTHP) or gas/elec/water/steam heating coil (PTAC), and supplemental heater if necessary.
  ! Control is by means of cycling: either continuous air flow with the DX compressor
  ! cycling on/off or the entire unit - fan and compressor cycling on/off. Cycling behavior
  ! is not explicitly modeled - instead cycling inefficiencies must be included in
  ! the efficiency curves of the DX coil module.

  ! REFERENCES: None

  ! OTHER NOTES: None

  ! USE STATEMENTS:
  ! Use statements for data only modules
USE DataPrecisionGlobals
USE DataLoopNode
USE DataSizing
USE DataGlobals,     ONLY: BeginEnvrnFlag, MaxNameLength, SysSizingCalc, SecInHour, InitConvTemp, NumOfZones, ScheduleAlwaysOn, &
                           DisplayExtraWarnings
USE DataInterfaces
USE DataHVACGlobals
USE DXCoils, ONLY: DXCoilPartLoadRatio
USE VariableSpeedCoils, ONLY:MaxSpedLevels

  ! Use statements for access to subroutines in other modules
USE ScheduleManager

IMPLICIT NONE         ! Enforce explicit typing of all variables

PRIVATE ! Everything private unless explicitly made public

  ! MODULE PARAMETER DEFINITIONS
! Compressor operation
INTEGER, PARAMETER :: On             = 1   ! normal compressor operation
INTEGER, PARAMETER :: Off            = 0   ! signal DXCoil that compressor shouldn't run

! Last mode of operation
INTEGER, PARAMETER :: CoolingMode                   = 1 ! last compressor operating mode was in cooling
INTEGER, PARAMETER :: HeatingMode                   = 2 ! last compressor operating mode was in heating

! Airflow control for contant fan mode
INTEGER, PARAMETER :: UseCompressorOnFlow           = 1 ! set compressor OFF air flow rate equal to compressor ON air flow rate
INTEGER, PARAMETER :: UseCompressorOffFlow          = 2 ! set compressor OFF air flow rate equal to user defined value

! Unit type
INTEGER, PARAMETER :: PTHPUnit   = 1   ! equivalent to PackagedTerminal:HeatPump:AirToAir
INTEGER, PARAMETER :: PTACUnit   = 2   ! equivalent to PackagedTerminal:AirConditioner
INTEGER, PARAMETER :: PTWSHPUnit = 3   ! equivalent to WaterToAirHeatPump

  ! DERIVED TYPE DEFINITIONS
TYPE PTUnitData
  ! input data
 CHARACTER(len=MaxNameLength) :: Name             =' '  ! name of unit
 CHARACTER(len=MaxNameLength) :: UnitType         =' '  ! type of unit
 INTEGER                      :: UnitType_Num     = 0   ! paramter equivalent to type of unit
 INTEGER                      :: ZoneEquipType    = 0  ! Type of PT unit
 INTEGER                      :: SchedPtr         = 0   ! index number to availability schedule
 REAL(r64)                    :: MaxCoolAirVolFlow        = 0.0d0 ! supply air volumetric flow rate during cooling operation [m3/s]
 REAL(r64)                    :: MaxHeatAirVolFlow        = 0.0d0 ! supply air volumetric flow rate during heating operation [m3/s]
 REAL(r64)                    :: MaxNoCoolHeatAirVolFlow  = 0.0d0 ! supply air volumetric flow rate when no cooling or heating [m3/s]
 REAL(r64)                    :: MaxCoolAirMassFlow       = 0.0d0 ! supply air mass flow rate during cooling operation [kg/s]
 REAL(r64)                    :: MaxHeatAirMassFlow       = 0.0d0 ! supply air mass flow rate during heating operation [kg/s]
 REAL(r64)                    :: MaxNoCoolHeatAirMassFlow = 0.0d0 ! supply air mass flow rate when no cooling or heating [kg/s]
 REAL(r64)                    :: CoolOutAirVolFlow        = 0.0d0 ! OA volumetric flow rate during cooling operation [m3/s]
 REAL(r64)                    :: HeatOutAirVolFlow        = 0.0d0 ! OA volumetric flow rate during heating operation [m3/s]
 REAL(r64)                    :: NoCoolHeatOutAirVolFlow  = 0.0d0 ! OA volumetric flow rate when no cooling or heating [m3/s]
 REAL(r64)                    :: CoolOutAirMassFlow       = 0.0d0 ! OA mass flow rate during cooling operation [kg/s]
 REAL(r64)                    :: HeatOutAirMassFlow       = 0.0d0 ! OA mass flow rate during heating operation [kg/s]
 REAL(r64)                    :: NoCoolHeatOutAirMassFlow = 0.0d0 ! OA mass flow rate when no cooling or heating [kg/s]
 INTEGER                      :: AirInNode        = 0    ! inlet air node number
 INTEGER                      :: AirOutNode       = 0    ! outlet air node number
 INTEGER                      :: OutsideAirNode   = 0    ! OAmixer outside air node number
 INTEGER                      :: AirReliefNode    = 0    ! OAmixer relief air node number
 CHARACTER(len=MaxNameLength) :: OAMixType        = ' '  ! type of outside air mixer
 CHARACTER(len=MaxNameLength) :: OAMixName        = ' '  ! name of OAmixer
 INTEGER                      :: OAMixIndex       = 0
 CHARACTER(len=MaxNameLength) :: FanName          = ' '  ! name of fan
 CHARACTER(len=MaxNameLength) :: FanType          = ' '  ! type of fan
 INTEGER                      :: FanType_Num      = 0    ! fan type number (see DataHVACGlobals)
 INTEGER                      :: FanIndex         = 0    ! index number to fan
 INTEGER                      :: FanSchedPtr      = 0    ! index number to fan operating mode schedule
 INTEGER                      :: FanAvailSchedPtr = 0    ! index to fan availability schedule
 CHARACTER(len=MaxNameLength) :: DXCoolCoilName   = ' '  ! name of DX cooling coil
 CHARACTER(len=MaxNameLength) :: DXCoolCoilType   = ' '  ! type of DX cooling coil,Coil:DX:CoolingBypassFactorEmpirical or
                                                         !                        'CoilSystem:Cooling:DX:HeatExchangerAssisted'
 INTEGER                      :: DXCoolCoilType_Num   = 0   ! numeric equivalent for DX cooling coil type
 INTEGER                      :: CoolCoilCompIndex    = 0   ! cooling coil index number (index for DX coil or HX Assisted object)
 INTEGER                      :: DXCoolCoilIndexNum   = 0   ! actual DX cooling coil index number
 INTEGER                      :: CondenserNodeNum     = 0   ! DX cooling coil condenser node number
 INTEGER                      :: DXHeatCoilIndexNum   = 0   ! actual DX heating coil index number
 CHARACTER(len=MaxNameLength) :: DXHeatCoilName       = ' ' ! name of DX heating coil
 CHARACTER(len=MaxNameLength) :: DXHeatCoilType       = ' ' ! type of DX heating coil,Coil:DX:HeatingEmpirical
 INTEGER                      :: DXHeatCoilType_Num   = 0   ! numeric equivalent for DX heating coil type
 INTEGER                      :: DXHeatCoilIndex      = 0   ! DX heating coil index number
 CHARACTER(len=MaxNameLength) :: ACHeatCoilName       = ' ' ! name of heating coil for PTAC
 CHARACTER(len=MaxNameLength) :: ACHeatCoilType       = ' ' ! type of heating coil for PTAC
 REAL(r64)                    :: ACHeatCoilCap        = 0.0d0 ! heating coil capacity for PTAC
 INTEGER                      :: ACHeatCoilIndex      = 0   ! heating coil index number for PTAC
 INTEGER                      :: HWCoilAirInletNode   = 0   ! air outlet node number of HW coil for PTAC
 INTEGER                      :: HWCoilSteamInletNode = 0   ! steam inlet node number of HW coil for PTAC and HP
 INTEGER                      :: HWCoilSteamOutletNode = 0  ! steam inlet node number of HW coil for PTAC and HP
 CHARACTER(len=MaxNameLength) :: SuppHeatCoilName     = ' ' ! name of supplemental heating coil
 INTEGER                      :: SuppHeatCoilType_Num = 0   ! numeric equivalent for supplemental heating coil type
 INTEGER                      :: ACHeatCoilType_Num   = 0   ! numeric equivalent for PTAC heating coil type
 INTEGER                      :: SuppHeatCoilIndex    = 0   ! supplemental heater index number
 INTEGER                      :: SupHeatCoilCap       = 0   ! supplemental heater coil capacity [W]
 INTEGER                      :: SupCoilAirInletNode  = 0   ! air inlet node for supplemental coil for HP
 CHARACTER(len=MaxNameLength) :: SuppHeatCoilType     = ' ' ! supplemental heater coil type
 REAL(r64)                    :: MaxSATSupHeat        = 0.0d0 ! maximum supply air temperature from supplemental heater [C]
 REAL(r64)                    :: MaxOATSupHeat        = 0.0d0 ! maximum outdoor air temp for supplemental heater operation [C]
 INTEGER :: OpMode            =0 ! mode of operation; 1=cycling fan, cycling compressor, 2=continuous fan, cycling compresor
 INTEGER :: FanPlace          =0 ! fan placement;     1=blow through, 2=draw through
 REAL(r64)                    :: CoolConvergenceTol   = 0.0d0 ! Convergence tolerance, fraction (ZoneLoad - Equip Output)/ZoneLoad
 REAL(r64)                    :: HeatConvergenceTol   = 0.0d0 ! Convergence tolerance, fraction (ZoneLoad - Equip Output)/ZoneLoad
 REAL(r64)                    :: MinOATCompressor     = 0.0d0 ! Minimum OAT for compressor operation [C]
 INTEGER                      :: IterErrIndex         = 0   ! index for recurring warnings
 CHARACTER(len=MaxNameLength) :: AvailManagerListName = ' ' ! Name of an availability manager list object
 INTEGER   :: WaterCyclingMode                    = 0  ! Heat Pump Coil water flow mode; See definitions in DataHVACGlobals,
                                                       ! 1=water cycling, 2=water constant, 3=water constant on demand (old mode)
 INTEGER                      :: PTObjectIndex         = 0   ! index for PT unit

! Water source HP specific variables
 REAL(r64) :: MaxONOFFCyclesperHour            =0.0d0 ! Maximum ON/OFF Cycling Rate [cycles/hr]
 REAL(r64) :: HPTimeConstant                   =0.0d0 ! Heat Pump Time Constant [s]
 REAL(r64) :: OnCyclePowerFraction             =0.0d0 ! Fraction of on-cycle power use [~]
                                                        ! supplemental heating coil operation
 REAL(r64) :: FanDelayTime                     =0.0d0 ! Fan delay time, time delay for the HP's fan to
                                                        ! shut off after compressor cycle off  [s]
 REAL(r64) :: DesignHeatingCapacity            =0.0d0 ! Nominal Capacity of Heating Coil [W]
 REAL(r64) :: DesignCoolingCapacity            =0.0d0 ! Nominal Capacity of Cooling Coil [W]
 REAL(r64) :: DesignSuppHeatingCapacity        =0.0d0 ! Nominal Capacity of Supplemental Heating Coil [W]

 ! Report data
 REAL(r64) :: TotHeatEnergyRate    = 0.0d0 ! total heating output [W]
 REAL(r64) :: TotHeatEnergy        = 0.0d0 ! total heating output [J]
 REAL(r64) :: TotCoolEnergyRate    = 0.0d0 ! total cooling output [W]
 REAL(r64) :: TotCoolEnergy        = 0.0d0 ! total cooling output [J]
 REAL(r64) :: SensHeatEnergyRate   = 0.0d0 ! sensible heating output [W]
 REAL(r64) :: SensHeatEnergy       = 0.0d0 ! sensible heating output [J]
 REAL(r64) :: SensCoolEnergyRate   = 0.0d0 ! sensible cooling output [W]
 REAL(r64) :: SensCoolEnergy       = 0.0d0 ! sensible cooling output [J]
 REAL(r64) :: LatHeatEnergyRate    = 0.0d0 ! latent heating output [W]
 REAL(r64) :: LatHeatEnergy        = 0.0d0 ! latent heating output [J]
 REAL(r64) :: LatCoolEnergyRate    = 0.0d0 ! latent cooling output [W]
 REAL(r64) :: LatCoolEnergy        = 0.0d0 ! latent cooling output [J]
 REAL(r64) :: ElecPower            = 0.0d0 ! electricity consumed [W]
 REAL(r64) :: ElecConsumption      = 0.0d0 ! electricity consumed [J]
 REAL(r64) :: FanPartLoadRatio     = 0.0d0 ! fan part-load ratio for time step
 REAL(r64) :: CompPartLoadRatio    = 0.0d0 ! compressor part-load ratio for time step
 INTEGER   :: LastMode             = 0   ! last mode of operation, coolingmode or heatingmode
 INTEGER   :: AirFlowControl       = 0   ! fan control mode, UseCompressorOnFlow or UseCompressorOffFlow
 REAL(r64) :: CompPartLoadFrac     = 0.0d0 ! compressor part load ratio
 INTEGER   :: HotWaterControlNode  = 0   ! control node for simple water heating coil
 INTEGER   :: PlantCoilOutletNode  = 0   ! outlet node for water coil
 INTEGER   :: LoopNum              = 0   ! plant loop index for water heating coil
 INTEGER   :: LoopSide             = 0   ! plant loop side  index for water heating coil
 INTEGER   :: BranchNum            = 0   ! plant loop branch index for water heating coil
 INTEGER   :: CompNum              = 0   ! plant loop component index for water heating coil
 REAL(r64) :: MaxHeatCoilFluidFlow = 0.0d0 ! water or steam mass flow rate for heating coil [kg/s]
 REAL(r64) :: MaxSuppCoilFluidFlow = 0.0d0 ! water or steam mass flow rate supp. heating coil [kg/s]
 Integer   :: HotWaterCoilMaxIterIndex   = 0  ! Index to recurring warning message
 Integer   :: HotWaterCoilMaxIterIndex2  = 0  ! Index to recurring warning message
 REAL(r64) :: ActualFanVolFlowRate = 0.0d0 ! Volumetric flow rate from fan object
 REAL(r64) :: HeatingSpeedRatio    = 1.0d0 ! Fan speed ratio in heating mode
 REAL(r64) :: CoolingSpeedRatio    = 1.0d0 ! Fan speed ratio in cooling mode
 REAL(r64) :: NoHeatCoolSpeedRatio = 1.0d0 ! Fan speed ratio when no cooling or heating
 INTEGER   :: AvailStatus          = 0
! starting added varibles for variable speed water source heat pump, Bo Shen, ORNL, March 2012
  INTEGER  :: HeatCoolMode          = 0  ! System operating mode (0 = floating, 1 = cooling, 2 = heating)
  INTEGER  :: NumOfSpeedCooling     =0   ! The number of speeds for cooling
  INTEGER  :: NumOfSpeedHeating     =0   ! The number of speeds for heating
  REAL(r64):: IdleSpeedRatio        = 0.0d0  !idle air fan ratio
  REAL(r64):: IdleVolumeAirRate = 0.0d0  ! idle air flow rate
  REAL(r64):: IdleMassFlowRate      = 0.0d0  ! idle air flow rate
  REAL(r64):: FanVolFlow            = 0.0d0  ! fan volumetric flow rate
  LOGICAL  :: CheckFanFlow      = .TRUE. ! Supply airflow check
  REAL(r64):: HeatVolumeFlowRate(MaxSpedLevels) = 0.0d0 ! Supply air volume flow rate during heating operation
  REAL(r64):: HeatMassFlowRate(MaxSpedLevels) = 0.0d0 ! Supply air mass flow rate during heating operation
  REAL(r64):: CoolVolumeFlowRate(MaxSpedLevels) = 0.0d0  ! Supply air volume flow rate during cooling operation
  REAL(r64):: CoolMassFlowRate(MaxSpedLevels) = 0.0d0  ! Supply air mass flow rate during cooling operation
  REAL(r64):: MSHeatingSpeedRatio(MaxSpedLevels) = 0.0d0  ! Fan speed ratio in heating mode
  REAL(r64):: MSCoolingSpeedRatio(MaxSpedLevels) = 0.0d0  ! Fan speed ratio in cooling mode
  INTEGER :: CompSpeedNum                        = 0
  REAL(r64):: CompSpeedRatio                     = 0.0d0
  INTEGER :: ErrIndexCyc                         = 0
  INTEGER :: ErrIndexVar                         = 0
 ! end of the additional variables for variable speed water source heat pump
END TYPE PTUnitData

  ! MODULE VARIABLE DECLARATIONS:
TYPE (PTUnitData), ALLOCATABLE, DIMENSION(:) :: PTUnit
LOGICAL, ALLOCATABLE, DIMENSION(:) :: CheckEquipName

REAL(r64) :: SupHeaterLoad      = 0.0d0     ! load to be met by supplemental heater [W]
INTEGER   :: NumPTHP            = 0       ! total number of PTHP's
INTEGER   :: NumPTAC            = 0       ! total number of PTAC's
INTEGER   :: NumPTWSHP          = 0       ! total number of PTWSHP's
INTEGER   :: NumPTUs            = 0       ! total number of PTHP and PTAC units
REAL(r64) :: CompOnMassFlow     = 0.0d0     ! Supply air mass flow rate w/ compressor ON
REAL(r64) :: OACompOnMassFlow   = 0.0d0     ! OA mass flow rate w/ compressor ON
REAL(r64) :: CompOffMassFlow    = 0.0d0     ! Supply air mass flow rate w/ compressor OFF
REAL(r64) :: OACompOffMassFlow  = 0.0d0     ! OA mass flow rate w/ compressor OFF
REAL(r64) :: CompOnFlowRatio    = 0.0d0     ! fan flow ratio when coil on
REAL(r64) :: CompOffFlowRatio   = 0.0d0     ! fan flow ratio when coil off
REAL(r64) :: FanSpeedRatio      = 0.0d0     ! ratio of air flow ratio passed to fan object
LOGICAL   :: GetPTUnitInputFlag = .TRUE.  ! First time, input is "gotten"
REAL(r64) :: SaveCompressorPLR  = 0.0d0     ! holds compressor PLR from active DX coil
REAL(r64) :: SteamDensity       = 0.0d0     ! density of steam at 100C, used for steam heating coils
LOGICAL   :: HeatingLoad        = .FALSE. ! defines a heating load on PTUnit
LOGICAL   :: CoolingLoad        = .FALSE. ! defines a cooling load on PTUnit
REAL(r64) :: MinWaterFlow       = 0.0d0     ! minimum water flow for heating [kg/s]
REAL(r64) :: TempSteamIn        = 100.0d0   ! steam coil steam inlet temperature

  ! SUBROUTINE SPECIFICATIONS FOR MODULE

PUBLIC SimPackagedTerminalUnit
PRIVATE SimPTUnit
PRIVATE GetPTUnit
PRIVATE InitPTUnit
PRIVATE SizePTUnit
PRIVATE ControlPTUnitOutput
PRIVATE CalcPTUnit
PRIVATE SetAverageAirFlow
PRIVATE ReportPTUnit
PRIVATE HeatPumpRunFrac
PRIVATE HotWaterCoilResidual

PUBLIC  GetPTUnitOutAirNode
PUBLIC  GetPTUnitReturnAirNode
PUBLIC  GetPTUnitMixedAirNode
PUBLIC  GetPTUnitZoneInletAirNode

        ! modules for variable speed heat pump
Private SimVariableSpeedHP
Private SetOnOffMassFlowRateVSCoil
Private CalcVarSpeedHeatPump
Private SetVSHPAirFlow
Private VSHPSpeedResidual
Private VSHPCyclingResidual
Private ControlVSHPOutput

CONTAINS

SUBROUTINE SimPackagedTerminalUnit(CompName,ZoneNum,FirstHVACIteration,QUnitOut,LatOutputProvided,PTUnitType,CompIndex)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Raustad
          !       DATE WRITTEN   July 2005
          !       MODIFIED       D. Shirey, Aug 2009 (LatOutputProvided)
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Manages the simulation of a packaged terminal heat pump. Called from SimZoneEquipment.

          ! METHODOLOGY EMPLOYED:
          ! NA

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE General,        ONLY: TrimSigDigits
  USE InputProcessor, ONLY: FindItemInList
  USE DataZoneEnergyDemands
  USE DataHeatBalFanSys, ONLY: TempControlType
  USE DataZoneEquipment, ONLY: PkgTermHPAirToAir_Num, PkgTermHPWaterToAir_Num, PkgTermACAirToAir_Num

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT    (IN) :: CompName            ! name of the packaged terminal heat pump
  INTEGER,          INTENT    (IN) :: ZoneNum             ! number of zone being served
  LOGICAL,          INTENT    (IN) :: FirstHVACIteration  ! TRUE if 1st HVAC simulation of system timestep
  REAL(r64),        INTENT   (OUT) :: QUnitOut            ! sensible capacity delivered to zone
  REAL(r64),        INTENT   (OUT) :: LatOutputProvided   ! Latent add/removal by packaged terminal unit (kg/s), dehumid = negative
  INTEGER,          INTENT    (IN) :: PTUnitType          ! indicates whether PTAC, PTHP or PTWSHP
  INTEGER,          INTENT (INOUT) :: CompIndex           ! index to Packaged Terminal Heat Pump

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER      :: PTUnitNum              ! index of packaged terminal heat pump being simulated

  REAL(r64)    :: OnOffAirFlowRatio      ! ratio of compressor ON airflow to average airflow over timestep
  REAL(r64)    :: QZnReq                 ! load to be met by zone equipment
  REAL(r64)    :: RemainingOutputToHeatingSP ! remaining load to heating setpoint
  REAL(r64)    :: RemainingOutputToCoolingSP ! remaining load to cooling setpoint

          ! FLOW

  ! First time SimPackagedTerminalHeatPump is called, get the input for all the PTUnits
  IF (GetPTUnitInputFlag) THEN
    CALL GetPTUnit
    GetPTUnitInputFlag = .FALSE.
  END IF

  ! Find the correct packaged terminal heat pump
  IF (CompIndex == 0) THEN
    PTUnitNum = FindItemInList(CompName,PTUnit%Name,NumPTUs)
    IF (PTUnitNum == 0) THEN
      CALL ShowFatalError('SimPackagedTerminalUnit: Unit not found='//TRIM(CompName))
    END IF
    CompIndex=PTUnit(PTUnitNum)%PTObjectIndex
  ELSE
    SELECT CASE (PTUnitType)
      CASE (PkgTermHPAirToAir_Num)
        PTUnitNum = CompIndex
      CASE (PkgTermACAirToAir_Num)
        PTUnitNum = CompIndex + NumPTHP
      CASE (PkgTermHPWaterToAir_Num)
        PTUnitNum = CompIndex + NumPTHP + NumPTAC
      CASE DEFAULT
    END SELECT
    IF (PTUnitNum > NumPTUs .or. PTUnitNum < 1) THEN
      CALL ShowFatalError('SimPackagedTerminalUnit:  Invalid CompIndex passed='//  &
                          TRIM(TrimSigDigits(PTUnitNum))// &
                          ', Number of Units='//TRIM(TrimSigDigits(NumPTUs))//  &
                          ', Entered Unit name='//TRIM(CompName))
    END IF
    IF (CheckEquipName(PTUnitNum)) THEN
      IF (CompName /= PTUnit(PTUnitNum)%Name) THEN
        CALL ShowFatalError('SimPackagedTerminalUnit: Invalid CompIndex passed='//  &
                            TRIM(TrimSigDigits(PTUnitNum))// &
                            ', Unit name='//TRIM(CompName)//', stored Unit Name for that index='//  &
                            TRIM(PTUnit(PTUnitNum)%Name))
      END IF
      CheckEquipName(PTUnitNum)=.false.
    ENDIF
  END IF

  OnOffAirFlowRatio = 0.0d0

  RemainingOutputToHeatingSP = ZoneSysEnergyDemand(ZoneNum)%RemainingOutputReqToHeatSP
  RemainingOutputToCoolingSP = ZoneSysEnergyDemand(ZoneNum)%RemainingOutputReqToCoolSP

  IF(RemainingOutputToCoolingSP .LT. 0.0D0 .and. TempControlType(ZoneNum) .NE. SingleHeatingSetPoint)THEN
    QZnReq = RemainingOutputToCoolingSP
  ELSE IF(RemainingOutputToHeatingSP .GT. 0.0D0 .and. TempControlType(ZoneNum) .NE. SingleCoolingSetPoint)THEN
    QZnReq = RemainingOutputToHeatingSP
  ELSE
    QZnReq = 0.0D0
  END IF

  ZoneEqDXCoil = .TRUE.

  ! Initialize the packaged terminal heat pump
  CALL InitPTUnit(PTUnitNum,ZoneNum,FirstHVACIteration,OnOffAirFlowRatio,QZnReq)

  CALL SimPTUnit(PTUnitNum,ZoneNum,FirstHVACIteration,QUnitOut,OnOffAirFlowRatio,QZnReq,LatOutputProvided)

  ! Report the result of the simulation
  CALL ReportPTUnit(PTUnitNum)

  ZoneEqDXCoil = .FALSE.

  RETURN
END SUBROUTINE SimPackagedTerminalUnit

SUBROUTINE SimPTUnit(PTUnitNum,ZoneNum,FirstHVACIteration,QSensUnitOut,OnOffAirFlowRatio,QZnReq,QLatUnitOut)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Raustad
          !       DATE WRITTEN   July 2005
          !       MODIFIED       D. Shirey, Aug 2009 (QLatUnitOut)
          !       MODIFIED       Bo Shen, March 2012, added switch to variable-speed water-source heat pump
          !       MODIFIED       Bo Shen, July 2012, added variable-speed air-source heat pump
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Simulate a packaged terminal heat pump; adjust its output to match the
          ! remaining zone load.

          ! METHODOLOGY EMPLOYED:
          ! Calls ControlPTUnitOutput to obtain the desired unit output

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na
  USE Psychrometrics,            ONLY: PsyHFnTdbW

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  LOGICAL, INTENT (IN)    :: FirstHVACIteration ! TRUE if 1st HVAC simulation of system timestep
  INTEGER, INTENT (IN)    :: PTUnitNum          ! number of the current Packaged Terminal Heat Pump being simulated
  INTEGER, INTENT (IN)    :: ZoneNum            ! number of zone being served
  REAL(r64),    INTENT (OUT)   :: QSensUnitOut       ! sensible delivered capacity [W]
  REAL(r64),    INTENT (OUT)   :: QLatUnitOut        ! Latent delivered capacity [kg/s], dehumidification = negative
  REAL(r64),    INTENT (INOUT) :: OnOffAirFlowRatio  ! ratio of compressor ON airflow to AVERAGE airflow over timestep
  REAL(r64), INTENT (IN)    :: QZnReq                ! cooling/heating needed by zone [W]

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64) :: PartLoadFrac      ! compressor part load fraction
  LOGICAL   :: UnitOn            ! TRUE if unit is on
  INTEGER   :: OutletNode        ! PTUnit air outlet node
  INTEGER   :: InletNode         ! PTUnit air inlet node
  REAL(r64) :: QTotUnitOut       ! total delivered capacity [W]
  REAL(r64) :: AirMassFlow       ! air mass flow rate [kg/s]
  REAL(r64) :: SpecHumOut        ! Specific humidity ratio of outlet air (kg moisture / kg moist air)
  REAL(r64) :: SpecHumIn         ! Specific humidity ratio of inlet air (kg moisture / kg moist air)
  INTEGER   :: OpMode            ! operating mode (fan cycling or continious; DX coil always cycles)
  LOGICAL   :: HXUnitOn          ! flag to enable heat exchanger
  REAL(r64) :: QLatReq           ! latent cooling output needed by zone [W], now is zero
  REAL(r64) :: QSensUnitOutMul   ! sensible output for the variable speed HP
  REAL(r64) :: QLatUnitOutMul    ! latent output for the variable speed HP
  REAL(r64) :: MinHumRat         ! min humidity for calculating sensible capacity of VS WSHP

  ! zero the fan, DX coils, and supplemental electric heater electricity consumption
  FanElecPower         = 0.0d0
  DXElecCoolingPower   = 0.0d0
  DXElecHeatingPower   = 0.0d0
  ElecHeatingCoilPower = 0.0d0
  SaveCompressorPLR    = 0.0d0
  QLatReq               = 0.0d0

  ! initialize local variables
  UnitOn       = .TRUE.
  HXUnitOn     = .TRUE.
  QSensUnitOut = 0.0d0
  QLatUnitOut  = 0.0d0
  OutletNode   = PTUnit(PTUnitNum)%AirOutNode
  InletNode    = PTUnit(PTUnitNum)%AirInNode
  AirMassFlow  = Node(InletNode)%MassFlowRate
  OpMode       = PTUnit(PTUnitNum)%OpMode

  ! reset operation flag if unit is off
  IF (PTUnit(PTUnitNum)%OPMode == CycFanCycCoil) THEN
    ! cycling unit: only runs if there is a cooling or heating load.
     IF ((.NOT. CoolingLoad .AND. .NOT. HeatingLoad) .OR. AirMassFlow < SmallMassFlow) THEN
       UnitOn = .FALSE.
     END IF
  ELSE IF  (PTUnit(PTUnitNum)%OPMode == ContFanCycCoil) THEN
    ! continuous unit: fan runs if scheduled on; coil runs only if there is a cooling or heating load
    IF (AirMassFlow.LT.SmallMassFlow) THEN
      UnitOn = .FALSE.
    END IF
  END IF

  OnOffFanPartLoadFraction = 1.0d0

  IF(UnitOn)THEN
   IF(PTUnit(PTUnitNum)%NumOfSpeedCooling > 0) THEN
     CALL SimVariableSpeedHP(PTUnitNum,ZoneNum, FirstHVACIteration, QZnReq, QLatReq, OnOffAirFlowRatio, OpMode, HXUnitOn )
   ELSE
     CALL ControlPTUnitOutput(PTUnitNum,FirstHVACIteration,OpMode,QZnReq,ZoneNum,PartLoadFrac,OnOffAirFlowRatio, &
                             SupHeaterLoad,HXUnitOn)
   END IF
  ELSE
    PartLoadFrac      = 0.0d0
    OnOffAirFlowRatio = 1.0d0
    SupHeaterLoad     = 0.0d0
    IF(PTUnit(PTUnitNum)%NumOfSpeedCooling > 0) THEN
      CALL CalcVarSpeedHeatPump(PTUnitNum, ZoneNum, FirstHVACIteration,0,1,0.0d0,PartLoadFrac,QSensUnitOutMul, QLatUnitOutMul, &
                      0.0d0,0.0d0,OnOffAirFlowRatio,SupHeaterLoad,HXUnitOn )
    END IF
  END IF

  ! calculate delivered capacity
  AirMassFlow = Node(InletNode)%MassFlowRate

  IF(PTUnit(PTUnitNum)%NumOfSpeedCooling == 0) THEN
    CALL CalcPTUnit(PTUnitNum,FirstHVACIteration,PartLoadFrac,QSensUnitOut,QZnReq,OnOffAirFlowRatio,SupHeaterLoad, HXUnitOn)
  ELSE
    ! calculate delivered capacity
    MinHumRat = MIN(Node(InletNode)%HumRat,Node(OutletNode)%HumRat)
    QSensUnitOut   = AirMassFlow * (PsyHFnTdbW(Node(OutletNode)%Temp,MinHumRat) - PsyHFnTdbW(Node(InletNode)%Temp,MinHumRat))
  END IF

! CR9155 Remove specific humidity calculations
  SpecHumOut = Node(OutletNode)%HumRat
  SpecHumIn  = Node(InletNode)%HumRat
  QLatUnitOut = AirMassFlow * (SpecHumOut - SpecHumIn) ! Latent rate, kg/s (dehumid = negative)

  QTotUnitOut = AirMassFlow * (Node(OutletNode)%Enthalpy - Node(InletNode)%Enthalpy)

  IF(PTUnit(PTUnitNum)%NumOfSpeedCooling == 0) THEN
      ! report variables
      IF(PTUnit(PTUnitNum)%UnitType_Num == PTACUnit)THEN
        PTUnit(PTUnitNum)%CompPartLoadRatio = PartLoadFrac
      ELSE
        PTUnit(PTUnitNum)%CompPartLoadRatio = SaveCompressorPLR
      END IF

      IF (PTUnit(PTUnitNum)%OpMode .EQ. CycFanCycCoil) THEN
        PTUnit(PTUnitNum)%FanPartLoadRatio = PartLoadFrac
      ELSE
        IF (UnitOn) THEN
          PTUnit(PTUnitNum)%FanPartLoadRatio = 1.0d0
        ELSE
          PTUnit(PTUnitNum)%FanPartLoadRatio = 0.0d0
        END IF
      END IF
  END IF

  PTUnit(PTUnitNum)%TotCoolEnergyRate  = ABS(MIN(0.0d0, QTotUnitOut))
  PTUnit(PTUnitNum)%TotHeatEnergyRate  = ABS(MAX(0.0d0, QTotUnitOut))
  PTUnit(PTUnitNum)%SensCoolEnergyRate = ABS(MIN(0.0d0, QSensUnitOut))
  PTUnit(PTUnitNum)%SensHeatEnergyRate = ABS(MAX(0.0d0, QSensUnitOut))
  PTUnit(PTUnitNum)%LatCoolEnergyRate  = ABS(MIN(0.0d0, (QTotUnitOut - QSensUnitOut)))
  PTUnit(PTUnitNum)%LatHeatEnergyRate  = ABS(MAX(0.0d0, (QTotUnitOut - QSensUnitOut)))

  IF(PTUnit(PTUnitNum)%UnitType_Num == PTACUnit)THEN
    SELECT CASE (PTUnit(PTUnitNum)%ACHeatCoilType_Num)
      CASE (Coil_HeatingGas, Coil_HeatingElectric)
        PTUnit(PTUnitNum)%ElecPower    = FanElecPower + DXElecCoolingPower + ElecHeatingCoilPower
      CASE (Coil_HeatingWater, Coil_HeatingSteam)
        PTUnit(PTUnitNum)%ElecPower    = FanElecPower + DXElecCoolingPower
      CASE DEFAULT
    END SELECT
  ELSE
    PTUnit(PTUnitNum)%ElecPower        = FanElecPower + DXElecCoolingPower + DXElecHeatingPower + ElecHeatingCoilPower
  END IF

  RETURN
END SUBROUTINE SimPTUnit

SUBROUTINE GetPTUnit

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Raustad
          !       DATE WRITTEN   July 2005
          !       MODIFIED       Chandan Sharma, FSEC, March 2011: Added ZoneHVAC sys avail manager
          !                      Bereket Nigusse, FSEC, April 2011: added OA Mixer object type
          !       MODIFIED       Bo Shen, ORNL, March 2012, added variable-speed water-source heat pump
          !       MODIFIED       Bo Shen, July 2012, added variable-speed air-source heat pump
          !
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Obtains input data for packaged terminal units and stores it in PTUnit data structures

          ! METHODOLOGY EMPLOYED:
          ! Uses "Get" routines to read in data.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE Fans,                  ONLY: GetFanType, GetFanIndex, GetFanVolFlow, GetFanInletNode, GetFanOutletNode, GetFanAvailSchPtr
  USE MixedAir,              ONLY: GetOAMixerNodeNumbers
  USE General,               ONLY: TrimSigDigits, RoundSigDigits
  USE DXCoils,               ONLY: GetDXCoolCoilIndex=>GetDXCoilIndex, &
                                   GetDXCoilInletNode=>GetCoilInletNode, GetDXCoilOutletNode=>GetCoilOutletNode, &
                                   GetCoilCondenserInletNode
  USE HVACHXAssistedCoolingCoil,  ONLY: GetHXDXCoilName, GetHXDXCoilInletNode=>GetCoilInletNode, &
                                        GetHXDXCoilOutletNode=>GetCoilOutletNode
  USE HeatingCoils,          ONLY: GetHeatingCoilIndex=>GetCoilIndex, HeatingCoil, SimulateHeatingCoilComponents, &
                                   GetHeatingCoilInletNode=>GetCoilInletNode, GetHeatingCoilOutletNode=>GetCoilOutletNode, &
                                   GetHeatingCoilCapacity=>GetCoilCapacity, GetHeatingCoilTypeNum
  USE SteamCoils,            ONLY: GetSteamCoilAirInletNode=>GetCoilAirInletNode, GetSteamCoilIndex, &
                                   GetSteamCoilAirOutletNode=>GetCoilAirOutletNode, &
                                   GetSteamCoilSteamInletNode=>GetCoilSteamInletNode, &
                                   GetCoilMaxSteamFlowRate=>GetCoilMaxSteamFlowRate, GetTypeOfCoil, ZoneLoadControl
  USE WaterCoils,            ONLY: GetCoilWaterInletNode, GetCoilMaxWaterFlowRate, &
                                   GetWaterCoilInletNode=>GetCoilInletNode,GetWaterCoilOutletNode=>GetCoilOutletNode
  USE InputProcessor,        ONLY: GetNumObjectsFound, GetObjectItem, VerifyName, SameString, GetObjectDefMaxArgs
  USE NodeInputManager,      ONLY: GetOnlySingleNode
  USE BranchNodeConnections, ONLY: SetUpCompSets
  USE FluidProperties,       ONLY: GetSatDensityRefrig
  USE WaterToAirHeatPump,    ONLY: GetWtoAHPCoilCapacity=>GetCoilCapacity, &
                                   GetWtoAHPCoilInletNode=>GetCoilInletNode, &
                                   GetWtoAHPCoilOutletNode=>GetCoilOutletNode,GetWtoAHPCoilIndex=>GetCoilIndex
  USE WaterToAirHeatPumpSimple, ONLY: GetWtoAHPSimpleCoilCapacity=>GetCoilCapacity,  &
                                      GetWtoAHPSimpleCoilInletNode=>GetCoilInletNode, &
                                      GetWtoAHPSimpleCoilOutletNode=>GetCoilOutletNode,GetWtoAHPSimpleCoilIndex=>GetCoilIndex, &
                                      SetSimpleWSHPData
  USE VariableSpeedCoils,    ONLY: GetCoilCapacityVariableSpeed, &
                                          GetCoilInletNodeVariableSpeed, &
                                          GetCoilOutletNodeVariableSpeed, &
                                          GetCoilIndexVariableSpeed, &
                                          SetVarSpeedCoilData, GetVSCoilCondenserInletNode
  USE OutAirNodeManager, ONLY: CheckOutAirNodeNumber
  USE DataZoneEquipment, ONLY: ZoneEquipConfig, PkgTermHPAirToAir_Num, PkgTermHPWaterToAir_Num, PkgTermACAirToAir_Num
  USE DataHVACGlobals,   ONLY: WaterCycling, WaterConstant, WaterConstantOnDemand

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER :: RoutineName='GetPTUnit: ' ! include trailing blank space


          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER                        :: PTUnitIndex  ! loop index
  INTEGER                        :: PTUnitNum    ! current packaged terminal unit number
  CHARACTER(len=MaxNameLength), &
                   ALLOCATABLE, DIMENSION(:) :: Alphas     ! Alpha items for object
  REAL(r64), ALLOCATABLE, DIMENSION(:)       :: Numbers    ! Numeric items for object
  INTEGER, DIMENSION(4)          :: OANodeNums  ! Node numbers of OA mixer (OA, EA, RA, MA)
  INTEGER                        :: FanInletNodeNum ! Fan inlet node number
  INTEGER                        :: FanOutletNodeNum ! Fan outlet node number
  INTEGER                        :: SuppHeatInletNodeNum !Supplemental heating coil inlet node number
  INTEGER                        :: SuppHeatOutletNodeNum !Supplemental heating coil outlet node number
  INTEGER                        :: CoolCoilInletNodeNum ! cooling coil inlet node number
  INTEGER                        :: CoolCoilOutletNodeNum ! cooling coil outlet node number
  CHARACTER(len=MaxNameLength)   :: ACHeatCoilName ! name of heating coil
  INTEGER                        :: HeatCoilInletNodeNum ! heating coil inlet node number
  INTEGER                        :: HeatCoilOutletNodeNum ! heating coil outlet node number
  INTEGER                        :: SuppHeatHWInletNodeNum  !Supplemental heating coil Hot Water inlet node number
  INTEGER                        :: SuppHeatHWOutletNodeNum !Supplemental heating coil Hot Water outlet node number
  CHARACTER(len=MaxNameLength)   :: CompSetFanInlet, CompSetCoolInlet, CompSetFanOutlet, CompSetCoolOutlet
  CHARACTER(len=MaxNameLength)   :: CompSetHeatInlet, CompSetHeatOutlet, CompSetSupHeatInlet, CompSetSupHeatOutlet
  INTEGER                        :: NumAlphas  ! Number of Alphas for each GetObjectItem call
  INTEGER                        :: NumNumbers ! Number of Numbers for each GetObjectItem call
  INTEGER                        :: MaxAlphas     ! Maximum number of alpha fields in all objects
  INTEGER                        :: MaxNumbers    ! Maximum number of numeric fields in all objects
  INTEGER                        :: NumFields     ! Total number of fields in object
  INTEGER                        :: IOStatus   ! Used in GetObjectItem
  LOGICAL                        :: ErrorsFound=.FALSE.  ! Set to true if errors in input, fatal at end of routine
  LOGICAL                        :: IsNotOK    ! Flag to verify name
  LOGICAL                        :: IsBlank    ! Flag for blank name
  CHARACTER(len=MaxNameLength)   :: CurrentModuleObject ! Object type for getting and error messages
  LOGICAL                        :: ErrFlag = .FALSE. ! Error flag returned during CALL to mining functions
  REAL(r64)                      :: FanVolFlow ! maximum supply air volumetric flow rate of fan
  INTEGER                        :: TempNodeNum ! dummy variable to set up HW coil water inlet node
  INTEGER                        :: SteamIndex  ! dummy variable to set up steam coil steam inlet density
  CHARACTER(len=MaxNameLength)   :: SuppHeatCoilType ! type of supplemental heating coil
  CHARACTER(len=MaxNameLength)   :: SuppHeatCoilName ! name of supplemental heating coil
  INTEGER                        :: CtrlZone    ! index to loop counter
  INTEGER                        :: NodeNum     ! index to loop counter
  LOGICAL                        :: ZoneNodeNotFound ! used in error checking

  CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: cAlphaFields   ! Alpha field names
  CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: cNumericFields ! Numeric field names
  LOGICAL, ALLOCATABLE, DIMENSION(:) :: lAlphaBlanks     ! Logical array, alpha field input BLANK = .true.
  LOGICAL, ALLOCATABLE, DIMENSION(:) :: lNumericBlanks   ! Logical array, numeric field input BLANK = .true.

  MaxNumbers = 0
  MaxAlphas  = 0

  ! find the number of each type of packaged terminal unit
  CurrentModuleObject = 'ZoneHVAC:PackagedTerminalHeatPump'
  NumPTHP = GetNumObjectsFound(CurrentModuleObject)
  CALL GetObjectDefMaxArgs(CurrentModuleObject,NumFields,NumAlphas,NumNumbers)
  MaxNumbers=MAX(MaxNumbers,NumNumbers)
  MaxAlphas=MAX(MaxAlphas,NumAlphas)

  CurrentModuleObject = 'ZoneHVAC:PackagedTerminalAirConditioner'
  NumPTAC = GetNumObjectsFound(CurrentModuleObject)
  CALL GetObjectDefMaxArgs(CurrentModuleObject,NumFields,NumAlphas,NumNumbers)
  MaxNumbers=MAX(MaxNumbers,NumNumbers)
  MaxAlphas=MAX(MaxAlphas,NumAlphas)

  CurrentModuleObject = 'ZoneHVAC:WaterToAirHeatPump'
  NumPTWSHP = GetNumObjectsFound(CurrentModuleObject)
  CALL GetObjectDefMaxArgs(CurrentModuleObject,NumFields,NumAlphas,NumNumbers)
  MaxNumbers=MAX(MaxNumbers,NumNumbers)
  MaxAlphas=MAX(MaxAlphas,NumAlphas)

  ALLOCATE(Alphas(MaxAlphas))
  Alphas=' '
  ALLOCATE(Numbers(MaxNumbers))
  Numbers=0.0d0
  ALLOCATE(cAlphaFields(MaxAlphas))
  cAlphaFields=' '
  ALLOCATE(cNumericFields(MaxNumbers))
  cNumericFields=' '
  ALLOCATE(lAlphaBlanks(MaxAlphas))
  lAlphaBlanks=.TRUE.
  ALLOCATE(lNumericBlanks(MaxNumbers))
  lNumericBlanks=.TRUE.
  NumPTUs = NumPTHP + NumPTAC + NumPTWSHP

  ! allocate the data structures
  IF (NumPTUs .GT. 0) THEN
    ALLOCATE(PTUnit(NumPTUs))
    ALLOCATE(CheckEquipName(NumPTUs))
  ENDIF
  CheckEquipName=.true.

  ! loop over PTHP units; get and load the input data
  DO PTUnitIndex = 1,NumPTHP

    FanInletNodeNum       = 0
    FanOutletNodeNum      = 0
    SuppHeatInletNodeNum   = 0
    SuppHeatOutletNodeNum  = 0
    CoolCoilInletNodeNum  = 0
    CoolCoilOutletNodeNum = 0
    HeatCoilInletNodeNum  = 0
    HeatCoilOutletNodeNum = 0
    SuppHeatHWInletNodeNum = 0
    SuppHeatHWOutletNodeNum = 0
    OANodeNums = 0

    CurrentModuleObject = 'ZoneHVAC:PackagedTerminalHeatPump'
    CALL GetObjectItem(CurrentModuleObject,PTUnitIndex,Alphas,NumAlphas,Numbers,NumNumbers,IOStatus, &
                       NumBlank=lNumericBlanks,AlphaBlank=lAlphaBlanks, &
                       AlphaFieldNames=cAlphaFields,NumericFieldNames=cNumericFields)

    PTUnitNum = PTUnitIndex
    IsNotOK=.FALSE.
    IsBlank=.FALSE.
    CALL VerifyName(Alphas(1),PTUnit%Name,PTUnitNum-1,IsNotOK,IsBlank,TRIM(CurrentModuleObject)//' Name')
    PTUnit(PTUnitNum)%PTObjectIndex = PTUnitIndex
    IF (IsNotOK) THEN
      ErrorsFound=.TRUE.
      IF (IsBlank) Alphas(1)='xxxxx'
    ENDIF
    PTUnit(PTUnitNum)%Name = Alphas(1)
    PTUnit(PTUnitNum)%UnitType = CurrentModuleObject
    PTUnit(PTUnitNum)%UnitType_Num = PTHPUnit
    PTUnit(PTUnitNum)%ZoneEquipType = PkgTermHPAirToAir_Num
    IF (lAlphaBlanks(2)) THEN
      PTUnit(PTUnitNum)%SchedPtr = ScheduleAlwaysOn
    ELSE
      PTUnit(PTUnitNum)%SchedPtr = GetScheduleIndex(Alphas(2))  ! convert schedule name to pointer (index number)
      IF (PTUnit(PTUnitNum)%SchedPtr .EQ. 0) THEN
        CALL ShowSevereError(TRIM(CurrentModuleObject)//'="'//TRIM(PTUnit(PTUnitNum)%Name)//'" invalid data.')
        CALL ShowContinueError('invalid-not found '//TRIM(cAlphaFields(2))//'="'//TRIM(Alphas(2))//'".')
        ErrorsFound=.TRUE.
      ENDIF
    END IF

    PTUnit(PTUnitNum)%AirInNode = &
               GetOnlySingleNode(Alphas(3),ErrorsFound,TRIM(CurrentModuleObject),Alphas(1), &
                            NodeType_Air,NodeConnectionType_Inlet,1,ObjectIsParent)

    PTUnit(PTUnitNum)%AirOutNode = &
               GetOnlySingleNode(Alphas(4),ErrorsFound,TRIM(CurrentModuleObject),Alphas(1), &
                            NodeType_Air,NodeConnectionType_Outlet,1,ObjectIsParent)

    PTUnit(PTUnitNum)%OAMixType = Alphas(5)
    PTUnit(PTUnitNum)%OAMixName = Alphas(6)

    ErrFlag = .false.
    CALL ValidateComponent(PTUnit(PTUnitNum)%OAMixType,PTUnit(PTUnitNum)%OAMixName,ErrFlag,TRIM(CurrentModuleObject))
    IF (ErrFlag) THEN
       CALL ShowContinueError('specified in '//TRIM(CurrentModuleObject)//' = "'//TRIM(PTUnit(PTUnitNum)%Name)//'".')
       ErrorsFound = .TRUE.
    ELSE
       ! OANodeNums = outside air mixer node numbers, OANodeNums(4) = outside air mixer mixed air node
       OANodeNums = GetOAMixerNodeNumbers(PTUnit(PTUnitNum)%OAMixName, ErrFlag)
       IF(ErrFlag) THEN
         CALL ShowContinueError('that was specified in '//TRIM(CurrentModuleObject)//' = '//TRIM(PTUnit(PTUnitNum)%Name))
         CALL ShowContinueError('..OutdoorAir:Mixer is required. Enter an OutdoorAir:Mixer object with this name.')
         ErrorsFound=.true.
       ELSE
         !  Set connection type to 'Inlet', because this is not necessarily directly come from
         !  outside air.  Outside Air Inlet Node List will set the connection to outside air
         PTUnit(PTUnitNum)%OutsideAirNode = OANodeNums(1)
         PTUnit(PTUnitNum)%AirReliefNode = OANodeNums(2)
      ENDIF
    ENDIF

    PTUnit(PTUnitNum)%MaxCoolAirVolFlow       = Numbers(1)
    IF (PTUnit(PTUnitNum)%MaxCoolAirVolFlow .LE. 0 .AND. PTUnit(PTUnitNum)%MaxCoolAirVolFlow .NE. AutoSize) THEN
      CALL ShowSevereError(TRIM(CurrentModuleObject)//' illegal '//TRIM(cNumericFields(1))//' = ' &
                           //TRIM(TrimSigDigits(Numbers(1),7)))
      CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(PTUnit(PTUnitNum)%Name))
      ErrorsFound = .TRUE.
    END IF

    PTUnit(PTUnitNum)%MaxHeatAirVolFlow       = Numbers(2)
    IF (PTUnit(PTUnitNum)%MaxHeatAirVolFlow .LE. 0 .AND. PTUnit(PTUnitNum)%MaxHeatAirVolFlow .NE. AutoSize) THEN
      CALL ShowSevereError(TRIM(CurrentModuleObject)//' illegal '//TRIM(cNumericFields(2))//' = ' &
                           //TRIM(TrimSigDigits(Numbers(2),7)))
      CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(PTUnit(PTUnitNum)%Name))
      ErrorsFound = .TRUE.
    END IF

    PTUnit(PTUnitNum)%MaxNoCoolHeatAirVolFlow = Numbers(3)
    IF (PTUnit(PTUnitNum)%MaxNoCoolHeatAirVolFlow .LT. 0 .AND. PTUnit(PTUnitNum)%MaxNoCoolHeatAirVolFlow .NE. AutoSize) THEN
      CALL ShowSevereError(TRIM(CurrentModuleObject)//' illegal '//TRIM(cNumericFields(3))//' = ' &
                           //TRIM(TrimSigDigits(Numbers(3),7)))
      CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(PTUnit(PTUnitNum)%Name))
      ErrorsFound = .TRUE.
    END IF

    PTUnit(PTUnitNum)%CoolOutAirVolFlow       = Numbers(4)
    IF (PTUnit(PTUnitNum)%CoolOutAirVolFlow .LT. 0 .AND. PTUnit(PTUnitNum)%CoolOutAirVolFlow .NE. AutoSize) THEN
      CALL ShowSevereError(TRIM(CurrentModuleObject)//' illegal '//TRIM(cNumericFields(4))//' = ' &
                           //TRIM(TrimSigDigits(Numbers(4),7)))
      CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(PTUnit(PTUnitNum)%Name))
      ErrorsFound = .TRUE.
    END IF

!   only check that SA flow in cooling is >= OA flow in cooling when either or both are not autosized
    IF (PTUnit(PTUnitNum)%CoolOutAirVolFlow .GT. PTUnit(PTUnitNum)%MaxCoolAirVolFlow .AND. &
        PTUnit(PTUnitNum)%CoolOutAirVolFlow .NE. AutoSize .AND. PTUnit(PTUnitNum)%MaxCoolAirVolFlow .NE. AutoSize) THEN
      CALL ShowSevereError(TRIM(CurrentModuleObject)//' '//TRIM(cNumericFields(4))//' cannot be greater than '// &
                           TRIM(cNumericFields(1)))
      CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(PTUnit(PTUnitNum)%Name))
      ErrorsFound = .TRUE.
    END IF

    PTUnit(PTUnitNum)%HeatOutAirVolFlow       = Numbers(5)
    IF (PTUnit(PTUnitNum)%HeatOutAirVolFlow .LT. 0 .AND. PTUnit(PTUnitNum)%HeatOutAirVolFlow.NE. AutoSize) THEN
      CALL ShowSevereError(TRIM(CurrentModuleObject)//' illegal '//TRIM(cNumericFields(5))//' = ' &
                           //TRIM(TrimSigDigits(Numbers(5),7)))
      CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(PTUnit(PTUnitNum)%Name))
      ErrorsFound = .TRUE.
    END IF

!   only check that SA flow in heating is >= OA flow in heating when either or both are not autosized
    IF (PTUnit(PTUnitNum)%HeatOutAirVolFlow .GT. PTUnit(PTUnitNum)%MaxHeatAirVolFlow .AND. &
        PTUnit(PTUnitNum)%HeatOutAirVolFlow .NE. AutoSize .AND. PTUnit(PTUnitNum)%MaxHeatAirVolFlow .NE. AutoSize) THEN
      CALL ShowSevereError(TRIM(CurrentModuleObject)//' '//TRIM(cNumericFields(5))//' cannot be greater than '// &
                           TRIM(cNumericFields(2)))
      CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(PTUnit(PTUnitNum)%Name))
      ErrorsFound = .TRUE.
    END IF

    PTUnit(PTUnitNum)%NoCoolHeatOutAirVolFlow = Numbers(6)
    IF (PTUnit(PTUnitNum)%NoCoolHeatOutAirVolFlow .LT. 0 .AND. PTUnit(PTUnitNum)%NoCoolHeatOutAirVolFlow .NE. AutoSize) THEN
      CALL ShowSevereError(TRIM(CurrentModuleObject)//' illegal '//TRIM(cNumericFields(6))//' = ' &
                           //TRIM(TrimSigDigits(Numbers(6),7)))
      CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(PTUnit(PTUnitNum)%Name))
      ErrorsFound = .TRUE.
    END IF
!   only check that SA flow when compressor is OFF is >= OA flow when compressor is OFF after fan mode is read in

    PTUnit(PTUnitNum)%FanType = Alphas(7)
    PTUnit(PTUnitNum)%FanName = Alphas(8)
    ErrFlag = .FALSE.
    CALL GetFanType(PTUnit(PTUnitNum)%FanName,PTUnit(PTUnitNum)%FanType_Num,ErrFlag,CurrentModuleObject,PTUnit(PTUnitNum)%Name)
    FanVolFlow = 0.d0
    IF(ErrFlag)THEN
      CALL ShowContinueError('specified in '//TRIM(CurrentModuleObject)//' = '//TRIM(PTUnit(PTUnitNum)%Name))
      ErrorsFound = .TRUE.
    ELSE
      CALL GetFanIndex(PTUnit(PTUnitNum)%FanName,PTUnit(PTUnitNum)%FanIndex,ErrFlag,CurrentModuleObject)
      FanInletNodeNum = GetFanInletNode(PTUnit(PTUnitNum)%FanType,PTUnit(PTUnitNum)%FanName,ErrFlag)
      FanOutletNodeNum = GetFanOutletNode(PTUnit(PTUnitNum)%FanType,PTUnit(PTUnitNum)%FanName,ErrFlag)
      CALL GetFanVolFlow(PTUnit(PTUnitNum)%FanIndex,FanVolFlow)
      PTUnit(PTUnitNum)%ActualFanVolFlowRate = FanVolFlow
      ! Get the fan's availability schedule
      PTUnit(PTUnitNum)%FanAvailSchedPtr = GetFanAvailSchPtr(PTUnit(PTUnitNum)%FanType,PTUnit(PTUnitNum)%FanName,ErrFlag)
      IF (ErrFlag) THEN
        CALL ShowContinueError('...specified in '//TRIM(CurrentModuleObject)//' = '//TRIM(PTUnit(PTUnitNum)%Name))
        ErrorsFound=.TRUE.
      ENDIF
    ENDIF

    IF(FanVolFlow .NE. AutoSize)THEN
      IF(FanVolFlow .LT. MAX(PTUnit(PTUnitNum)%MaxCoolAirVolFlow, &
                             PTUnit(PTUnitNum)%MaxHeatAirVolFlow, &
                             PTUnit(PTUnitNum)%MaxNoCoolHeatAirVolFlow))THEN
        CALL ShowSevereError(TRIM(CurrentModuleObject)//'="'//TRIM(PTUnit(PTUnitNum)%Name)//'", invalid air flow rate')
        CALL ShowContinueError('air flow rate = '//TRIM(TrimSigDigits(FanVolFlow,7))// &
                 ' in fan object '//TRIM(PTUnit(PTUnitNum)%FanName)//' is less than the maximum PTHP supply air flow rate.')
        CALL ShowContinueError(' The fan flow rate must be greater than the PTHP maximum supply air flow rate.')
        ErrorsFound = .TRUE.
      END IF
    END IF

    PTUnit(PTUnitNum)%DXHeatCoilName = Alphas(10)
    IF(SameString(Alphas(9),'Coil:Heating:DX:SingleSpeed')) THEN
       PTUnit(PTUnitNum)%DXHeatCoilType = Alphas(9)
!       PTUnit(PTUnitNum)%DXHeatCoilType_Num = CoilDX_HeatingEmpirical
       ErrFlag = .FALSE.
       CALL GetDXCoolCoilIndex(PTUnit(PTUnitNum)%DXHeatCoilName,PTUnit(PTUnitNum)%DXHeatCoilIndexNum, &
                               ErrFlag, PTUnit(PTUnitNum)%DXHeatCoilType)
       HeatCoilInletNodeNum = GetDXCoilInletNode(PTUnit(PTUnitNum)%DXHeatCoilType,PTUnit(PTUnitNum)%DXHeatCoilName,ErrFlag)
       HeatCoilOutletNodeNum = GetDXCoilOutletNode(PTUnit(PTUnitNum)%DXHeatCoilType,PTUnit(PTUnitNum)%DXHeatCoilName,ErrFlag)
       IF(ErrFlag)CALL ShowContinueError('...occurs in '//TRIM(PTUnit(PTUnitNum)%UnitType)// &
                                         ' "'//TRIM(PTUnit(PTUnitNum)%Name)//'"')
    ELSE IF (SameString(Alphas(9),'COIL:HEATING:DX:VARIABLESPEED' ))THEN
      PTUnit(PTUnitNum)%DXHeatCoilType = Alphas(9)
      PTUnit(PTUnitNum)%DXHeatCoilType_Num = Coil_HeatingAirToAirVariableSpeed
      PTUnit(PTUnitNum)%DXHeatCoilName = Alphas(10)
      CALL ValidateComponent(PTUnit(PTUnitNum)%DXHeatCoilType,PTUnit(PTUnitNum)%DXHeatCoilName,IsNotOK,  &
                             TRIM(CurrentModuleObject))
      IF (IsNotOK) THEN
        CALL ShowContinueError('...specified in '//TRIM(CurrentModuleObject)//'="'//TRIM(Alphas(1))//'".')
        ErrorsFound=.TRUE.
      ELSE
        ErrFlag = .FALSE.
        PTUnit(PTUnitNum)%DXHeatCoilIndex = GetCoilIndexVariableSpeed(PTUnit(PTUnitNum)%DXHeatCoilType, &
                                                                     PTUnit(PTUnitNum)%DXHeatCoilName,ErrFlag)
        IF(ErrFlag)THEN
          CALL ShowContinueError('...specified in '//TRIM(CurrentModuleObject)//'="'//TRIM(Alphas(1))//'".')
          ErrorsFound=.TRUE.
        END IF
        HeatCoilInletNodeNum=GetCoilInletNodeVariableSpeed(PTUnit(PTUnitNum)%DXHeatCoilType, &
                                                          PTUnit(PTUnitNum)%DXHeatCoilName,ErrFlag)
        HeatCoilOutletNodeNum=GetCoilOutletNodeVariableSpeed(PTUnit(PTUnitNum)%DXHeatCoilType, &
                                                            PTUnit(PTUnitNum)%DXHeatCoilName,ErrFlag)
      ENDIF
    ELSE
      CALL ShowSevereError(TRIM(CurrentModuleObject)//'="'//TRIM(PTUnit(PTUnitNum)%Name)//'", invalid field')
      CALL ShowContinueError(' illegal '//TRIM(cAlphaFields(9))//' = '//TRIM(Alphas(9)))
      ErrorsFound = .TRUE.
    END IF

    PTUnit(PTUnitNum)%HeatConvergenceTol      = Numbers(7)
    PTUnit(PTUnitNum)%MinOATCompressor        = Numbers(8)
    PTUnit(PTUnitNum)%DXCoolCoilName          = Alphas(12)
    PTUnit(PTUnitNum)%CoolConvergenceTol      = Numbers(9)

    IF(SameString(Alphas(11),'Coil:Cooling:DX:SingleSpeed') .OR. &
       SameString(Alphas(11),'CoilSystem:Cooling:DX:HeatExchangerAssisted')) THEN
       PTUnit(PTUnitNum)%DXCoolCoilType = Alphas(11)
       IF (SameString(Alphas(11),'Coil:Cooling:DX:SingleSpeed')) THEN
         PTUnit(PTUnitNum)%DXCoolCoilType_Num = CoilDX_CoolingSingleSpeed
         ErrFlag = .FALSE.
         CALL GetDXCoolCoilIndex(PTUnit(PTUnitNum)%DXCoolCoilName,PTUnit(PTUnitNum)%DXCoolCoilIndexNum, &
                                 ErrFlag, PTUnit(PTUnitNum)%DXCoolCoilType)
         CoolCoilInletNodeNum = GetDXCoilInletNode(PTUnit(PTUnitNum)%DXCoolCoilType,PTUnit(PTUnitNum)%DXCoolCoilName,ErrFlag)
         CoolCoilOutletNodeNum = GetDXCoilOutletNode(PTUnit(PTUnitNum)%DXCoolCoilType,PTUnit(PTUnitNum)%DXCoolCoilName,ErrFlag)
         PTUnit(PTUnitNum)%CondenserNodeNum = &
                         GetCoilCondenserInletNode(PTUnit(PTUnitNum)%DXCoolCoilType,PTUnit(PTUnitNum)%DXCoolCoilName,ErrFlag)
         IF(ErrFlag)CALL ShowContinueError('...occurs in '//TRIM(PTUnit(PTUnitNum)%UnitType)// &
                                           ' "'//TRIM(PTUnit(PTUnitNum)%Name)//'"')
       ELSEIF (SameString(Alphas(11),'CoilSystem:Cooling:DX:HeatExchangerAssisted')) THEN
         PTUnit(PTUnitNum)%DXCoolCoilType_Num = CoilDX_CoolingHXAssisted
         ErrFlag = .FALSE.
         CALL GetDXCoolCoilIndex( &
               GetHXDXCoilName(PTUnit(PTUnitNum)%DXCoolCoilType,PTUnit(PTUnitNum)%DXCoolCoilName,ErrFlag), &
               PTUnit(PTUnitNum)%DXCoolCoilIndexNum, ErrFlag, 'Coil:Cooling:DX:SingleSpeed')
         CoolCoilInletNodeNum = GetHXDXCoilInletNode(PTUnit(PTUnitNum)%DXCoolCoilType,PTUnit(PTUnitNum)%DXCoolCoilName,ErrFlag)
         CoolCoilOutletNodeNum = GetHXDXCoilOutletNode(PTUnit(PTUnitNum)%DXCoolCoilType,PTUnit(PTUnitNum)%DXCoolCoilName,ErrFlag)
         PTUnit(PTUnitNum)%CondenserNodeNum = GetCoilCondenserInletNode('Coil:Cooling:DX:SingleSpeed', &
                        GetHXDXCoilName(PTUnit(PTUnitNum)%DXCoolCoilType,PTUnit(PTUnitNum)%DXCoolCoilName,ErrFlag),ErrFlag)
         IF(ErrFlag)CALL ShowContinueError('...occurs in '//TRIM(PTUnit(PTUnitNum)%UnitType)// &
                                           ' "'//TRIM(PTUnit(PTUnitNum)%Name)//'"')
       END IF
    ELSE IF (SameString(Alphas(11), 'COIL:COOLING:DX:VARIABLESPEED') )THEN
      PTUnit(PTUnitNum)%DXCoolCoilType = Alphas(11)
      PTUnit(PTUnitNum)%DXCoolCoilType_Num = Coil_CoolingAirToAirVariableSpeed
      PTUnit(PTUnitNum)%DXCoolCoilName = Alphas(12)
      CALL ValidateComponent(PTUnit(PTUnitNum)%DXCoolCoilType,PTUnit(PTUnitNum)%DXCoolCoilName,IsNotOK,  &
                             TRIM(CurrentModuleObject))
      IF (IsNotOK) THEN
        CALL ShowContinueError('...specified in '//TRIM(CurrentModuleObject)//'="'//TRIM(Alphas(1))//'".')
        ErrorsFound=.TRUE.
      ELSE
        ErrFlag = .FALSE.
        PTUnit(PTUnitNum)%DXCoolCoilIndexNum = GetCoilIndexVariableSpeed(PTUnit(PTUnitNum)%DXCoolCoilType, &
                                                                        PTUnit(PTUnitNum)%DXCoolCoilName,ErrFlag)
        IF(ErrFlag)THEN
          CALL ShowContinueError('...specified in '//TRIM(CurrentModuleObject)//'="'//TRIM(Alphas(1))//'".')
          ErrorsFound=.TRUE.
        END IF
        CoolCoilInletNodeNum=GetCoilInletNodeVariableSpeed(PTUnit(PTUnitNum)%DXCoolCoilType,  &
                                                          PTUnit(PTUnitNum)%DXCoolCoilName,ErrFlag)
        CoolCoilOutletNodeNum=GetCoilOutletNodeVariableSpeed(PTUnit(PTUnitNum)%DXCoolCoilType,  &
                                                            PTUnit(PTUnitNum)%DXCoolCoilName,ErrFlag)
        PTUnit(PTUnitNum)%CondenserNodeNum = GetVSCoilCondenserInletNode(PTUnit(PTUnitNum)%DXCoolCoilName,ErrFlag)

        IF(ErrFlag)CALL ShowContinueError('...occurs in '//TRIM(PTUnit(PTUnitNum)%UnitType)// &
                                           ' "'//TRIM(PTUnit(PTUnitNum)%Name)//'"')

      ENDIF
    ELSE
      CALL ShowWarningError(TRIM(CurrentModuleObject)//' illegal '//TRIM(cAlphaFields(11))//' = '//TRIM(Alphas(11)))
      CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(PTUnit(PTUnitNum)%Name))
      ErrorsFound = .TRUE.
    END IF

    IF (Alphas(9) == 'COIL:HEATING:DX:VARIABLESPEED' .AND.   &
        Alphas(11) == 'COIL:COOLING:DX:VARIABLESPEED') THEN
      IF(PTUnit(PTUnitNum)%DXHeatCoilIndex .GT. 0 .AND. PTUnit(PTUnitNum)%DXCoolCoilIndexNum .GT. 0)THEN
         CALL SetVarSpeedCoilData(PTUnit(PTUnitNum)%DXCoolCoilIndexNum,ErrorsFound, &
                                CompanionHeatingCoilNum=PTUnit(PTUnitNum)%DXHeatCoilIndex)
      END IF
    END IF

    SuppHeatCoilType                           = Alphas(13)
    SuppHeatCoilName                           = Alphas(14)
    PTUnit(PTUnitNum)%SuppHeatCoilName         = SuppHeatCoilName
    IF (SameString(Alphas(13),'Coil:Heating:Gas')      .OR. &
        SameString(Alphas(13),'Coil:Heating:Electric') .OR. &
        SameString(Alphas(13),'Coil:Heating:Water')    .OR. &
        SameString(Alphas(13),'Coil:Heating:Steam')) THEN
        PTUnit(PTUnitNum)%SuppHeatCoilType=SuppHeatCoilType
       IF (SameString(Alphas(13),'Coil:Heating:Gas') .OR. SameString(Alphas(13),'Coil:Heating:Electric')) THEN
         IF (SameString(Alphas(13),'Coil:Heating:Gas')) THEN
            PTUnit(PTUnitNum)%SuppHeatCoilType_Num = Coil_HeatingGas
         ELSEIF (SameString(Alphas(13),'Coil:Heating:Electric')) THEN
            PTUnit(PTUnitNum)%SuppHeatCoilType_Num = Coil_HeatingElectric
         ENDIF
         ErrFlag = .FALSE.
         CALL ValidateComponent(SuppHeatCoilType,SuppHeatCoilName,ErrFlag,  &
                                TRIM(CurrentModuleObject))
         IF (ErrFlag) THEN
           CALL ShowContinueError('...specified in '//TRIM(CurrentModuleObject)//'="'//TRIM(PTUnit(PTUnitNum)%Name)//'".')
           ErrorsFound=.TRUE.
         ELSE
           CALL GetHeatingCoilIndex(SuppHeatCoilName,PTUnit(PTUnitNum)%SuppHeatCoilIndex,ErrFlag)
           ! Get the Supplemental Heating Coil Node Numbers
           SuppHeatInletNodeNum = &
               GetHeatingCoilInletNode(SuppHeatCoilType,SuppHeatCoilName,ErrFlag)
           SuppHeatOutletNodeNum = &
               GetHeatingCoilOutletNode(SuppHeatCoilType,SuppHeatCoilName,ErrFlag)
           IF (ErrFlag) THEN
             CALL ShowContinueError('...specified in '//TRIM(CurrentModuleObject)//'="'//TRIM(PTUnit(PTUnitNum)%Name)//'".')
             ErrorsFound=.TRUE.
           ENDIF
        ENDIF
       ELSEIF (SameString(Alphas(13),'Coil:Heating:Water')) THEN
         PTUnit(PTUnitNum)%SuppHeatCoilType_Num = Coil_HeatingWater
         ErrFlag = .FALSE.
         SuppHeatHWInletNodeNum = GetCoilWaterInletNode(SuppHeatCoilType,PTUnit(PTUnitNum)%SuppHeatCoilName,ErrFlag)
         PTUnit(PTUnitNum)%HotWaterControlNode = SuppHeatHWInletNodeNum
         IF(ErrFlag)THEN
           CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(PTUnit(PTUnitNum)%Name))
           ErrorsFound = .TRUE.
         END IF
         PTUnit(PTUnitNum)%MaxSuppCoilFluidFlow = GetCoilMaxWaterFlowRate(SuppHeatCoilType,  &
                                                  PTUnit(PTUnitNum)%SuppHeatCoilName,ErrFlag)
         IF(PTUnit(PTUnitNum)%MaxSuppCoilFluidFlow .GT. 0.0d0)THEN
            PTUnit(PTUnitNum)%MaxSuppCoilFluidFlow = GetCoilMaxWaterFlowRate(SuppHeatCoilType,  &
                                                     PTUnit(PTUnitNum)%SuppHeatCoilName,ErrFlag)
         END IF
         ErrFlag = .FALSE.
         SuppHeatInletNodeNum =  GetWaterCoilInletNode('Coil:Heating:Water',PTUnit(PTUnitNum)%SuppHeatCoilName,ErrFlag)
         PTUnit(PTUnitNum)%SupCoilAirInletNode = SuppHeatInletNodeNum
         SuppHeatOutletNodeNum = GetWaterCoilOutletNode('Coil:Heating:Water', &
                                                        PTUnit(PTUnitNum)%SuppHeatCoilName,ErrFlag)
         IF(ErrFlag)THEN
           CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(PTUnit(PTUnitNum)%Name))
           ErrorsFound = .TRUE.
         END IF

       ELSEIF (SameString(Alphas(13),'Coil:Heating:Steam')) THEN
         PTUnit(PTUnitNum)%SuppHeatCoilType_Num = Coil_HeatingSteam
         ErrFlag = .FALSE.
         PTUnit(PTUnitNum)%SuppHeatCoilIndex = GetSTeamCoilIndex(SuppHeatCoilType,PTUnit(PTUnitNum)%SuppHeatCoilName,ErrFlag)
         IF (PTUnit(PTUnitNum)%SuppHeatCoilIndex .EQ. 0) THEN
             CALL ShowSevereError(TRIM(CurrentModuleObject)//' illegal '//TRIM(cAlphaFields(14))//' = ' &
                           //TRIM(PTUnit(PTUnitNum)%SuppHeatCoilName))
             CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(PTUnit(PTUnitNum)%Name))
             ErrorsFound = .TRUE.
         END IF
         !IF(ErrFlag)CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(PTUnit(PTUnitNum)%Name))
         ErrFlag = .FALSE.
         SuppHeatHWInletNodeNum = GetSteamCoilSteamInletNode(SuppHeatCoilType,PTUnit(PTUnitNum)%SuppHeatCoilName,ErrFlag)
         PTUnit(PTUnitNum)%HWCoilSteamInletNode = SuppHeatHWInletNodeNum
         IF(ErrFlag)THEN
           CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(PTUnit(PTUnitNum)%Name))
           ErrorsFound = .TRUE.
         END IF
         PTUnit(PTUnitNum)%MaxSuppCoilFluidFlow = GetCoilMaxSteamFlowRate(PTUnit(PTUnitNum)%SuppHeatCoilIndex,ErrFlag)
         IF(PTUnit(PTUnitNum)%MaxSuppCoilFluidFlow .GT. 0.0d0)THEN
            SteamIndex = 0      ! Function GetSatDensityRefrig will look up steam index if 0 is passed
            SteamDensity=GetSatDensityRefrig("STEAM",TempSteamIn,1.0d0,SteamIndex,'GetPackagedTerminalHeatPumpInput')
            PTUnit(PTUnitNum)%MaxSuppCoilFluidFlow = &
                              GetCoilMaxSteamFlowRate(PTUnit(PTUnitNum)%SuppHeatCoilIndex,ErrFlag) * SteamDensity
         END IF
         ErrFlag = .FALSE.
         SuppHeatInletNodeNum = &
            GetSteamCoilAirInletNode(PTUnit(PTUnitNum)%SuppHeatCoilIndex,PTUnit(PTUnitNum)%SuppHeatCoilName,ErrFlag)
         PTUnit(PTUnitNum)%SupCoilAirInletNode = SuppHeatInletNodeNum
         SuppHeatOutletNodeNum = GetSteamCoilAirOutletNode(SuppHeatCoilType,PTUnit(PTUnitNum)%SuppHeatCoilName,ErrFlag)
         IF(ErrFlag)THEN
           CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(PTUnit(PTUnitNum)%Name))
           ErrorsFound = .TRUE.
         END IF
       END IF

    ELSE
      CALL ShowSevereError(TRIM(CurrentModuleObject)//' illegal '//TRIM(cAlphaFields(13))//' = '//TRIM(Alphas(13)))
      CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(PTUnit(PTUnitNum)%Name))
      ErrorsFound = .TRUE.
    END IF

    PTUnit(PTUnitNum)%MaxSATSupHeat           = Numbers(10)
    PTUnit(PTUnitNum)%MaxOATSupHeat           = Numbers(11)
    IF(PTUnit(PTUnitNum)%MaxOATSupHeat .GT. 21.0d0) THEN
      CALL ShowWarningError(TRIM(CurrentModuleObject)//' = '//TRIM(PTUnit(PTUnitNum)%Name)//&
                            ': '//TRIM(cNumericFields(11))//' should be <= to 21.')
      CALL ShowContinueError('...'//TRIM(cNumericFields(11))//' = '//TRIM(TrimSigDigits(Numbers(11),1)))
    END IF

    IF (SameString(Alphas(15),'BlowThrough'))  PTUnit(PTUnitNum)%FanPlace = BlowThru
    IF (SameString(Alphas(15),'DrawThrough'))  PTUnit(PTUnitNum)%FanPlace = DrawThru
    IF (PTUnit(PTUnitNum)%FanPlace .EQ. 0) THEN
      CALL ShowSevereError(TRIM(CurrentModuleObject)//' illegal '//TRIM(cAlphaFields(15))//' = '//TRIM(Alphas(15)))
      CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(PTUnit(PTUnitNum)%Name))
      ErrorsFound = .TRUE.
    END IF

    ! Check component placement
    IF (PTUnit(PTUnitNum)%FanPlace == BlowThru) THEN
      ! PTUnit inlet node must be the same as a zone exhaust node and the OA Mixer return node
      ! check that PTUnit inlet node is a zone exhaust node.
      ZoneNodeNotFound = .TRUE.
      DO CtrlZone = 1,NumOfZones
        IF (.not. ZoneEquipConfig(CtrlZone)%IsControlled) CYCLE
        DO NodeNum = 1,ZoneEquipConfig(CtrlZone)%NumExhaustNodes
          IF (PTUnit(PTUnitNum)%AirInNode .EQ. ZoneEquipConfig(CtrlZone)%ExhaustNode(NodeNum)) THEN
            ZoneNodeNotFound = .FALSE.
            EXIT
          END IF
        END DO
      END DO
      IF(ZoneNodeNotFound)THEN
        CALL ShowSevereError(TRIM(CurrentModuleObject)//' "'//TRIM(PTUnit(PTUnitNum)%Name)//&
                           '" Heat Pumps air inlet node name must be the same as a zone exhaust node name.')
        CALL ShowContinueError('..Zone exhaust node name is specified in ZoneHVAC:EquipmentConnections object.')
        CALL ShowContinueError('..Heat pumps inlet node name = '//TRIM(NodeID(PTUnit(PTUnitNum)%AirInNode)))
        ErrorsFound=.TRUE.
      END IF
      ! check OA Mixer return node
      IF(PTUnit(PTUnitNum)%AirInNode /= OANodeNums(3))THEN
        CALL ShowSevereError(TRIM(CurrentModuleObject)//' "'//TRIM(PTUnit(PTUnitNum)%Name)//&
                             '" PTUnit air inlet node name must be the same as the OutdoorAir:Mixer return air node name.')
        CALL ShowContinueError('..PTUnit air inlet node name            = '//TRIM(NodeID(PTUnit(PTUnitNum)%AirInNode)))
        CALL ShowContinueError('..OutdoorAir:Mixer return air node name = '//TRIM(NodeID(OANodeNums(3))))
        ErrorsFound=.TRUE.
      END IF
      ! Fan inlet node name must be the same as the heat pump's OA mixer mixed air node name
      IF (OANodeNums(4) /= FanInletNodeNum) THEN
        CALL ShowSevereError(TRIM(CurrentModuleObject)//' "'//TRIM(PTUnit(PTUnitNum)%Name)//&
                             '" Fan inlet node name must be the same as the heat pumps')
        CALL ShowContinueError('OutdoorAir:Mixer mixed air node name when blow through '// &
                                TRIM(cAlphaFields(15))//' is specified.')
        CALL ShowContinueError('..Fan inlet node name                   = '//TRIM(NodeID(FanInletNodeNum)))
        CALL ShowContinueError('..OutdoorAir:Mixer mixed air node name = '//TRIM(NodeID(OANodeNums(4))))
        ErrorsFound=.TRUE.
      END IF
      IF(CoolCoilInletNodeNum /= FanOutletNodeNum)THEN
        CALL ShowSevereError(TRIM(CurrentModuleObject)//' "'//TRIM(PTUnit(PTUnitNum)%Name)//&
                             '" Fan outlet node name must be the same as the cooling coil')
        CALL ShowContinueError(' inlet node name when blow through '//TRIM(cAlphaFields(15))//' is specified.')
        CALL ShowContinueError('..Fan outlet node name         = '//TRIM(NodeID(FanOutletNodeNum)))
        CALL ShowContinueError('..Cooling coil inlet node name = '//TRIM(NodeID(CoolCoilInletNodeNum)))
        ErrorsFound=.TRUE.
      END IF
      IF(CoolCoilOutletNodeNum /= HeatCoilInletNodeNum)THEN
        CALL ShowSevereError(TRIM(CurrentModuleObject)//' "'//TRIM(PTUnit(PTUnitNum)%Name)//&
                           '" Cooling coil outlet node name must be the same as the heating coil inlet node name.')
        CALL ShowContinueError('..Cooling coil outlet node name = '//TRIM(NodeID(CoolCoilOutletNodeNum)))
        CALL ShowContinueError('..Heating coil inlet node name  = '//TRIM(NodeID(HeatCoilInletNodeNum)))
        ErrorsFound=.TRUE.
      END IF
      IF(HeatCoilOutletNodeNum /= SuppHeatInletNodeNum)THEN
        CALL ShowSevereError(TRIM(CurrentModuleObject)//' "'//TRIM(PTUnit(PTUnitNum)%Name)//&
                           '" Heating coil outlet node name must be the same as the supplemental heating coil inlet')
        CALL ShowContinueError(' node name when blow through '//TRIM(cAlphaFields(14))//' is specified.')
        CALL ShowContinueError('..Heating coil outlet node name              = '//TRIM(NodeID(HeatCoilOutletNodeNum)))
        CALL ShowContinueError('..Supplemental heating coil inlet node name  = '//TRIM(NodeID(SuppHeatInletNodeNum)))
        ErrorsFound=.TRUE.
      END IF
      IF(SuppHeatOutletNodeNum /= PTUnit(PTUnitNum)%AirOutNode)THEN
        CALL ShowSevereError(TRIM(CurrentModuleObject)//' "'//TRIM(PTUnit(PTUnitNum)%Name)//&
                           '" Supplemental heating coil outlet node name must be the same as the heat pumps outlet node name.')
        CALL ShowContinueError('..Supplemental heating coil outlet node name = '//TRIM(NodeID(SuppHeatOutletNodeNum)))
        CALL ShowContinueError('..Heat pumps outlet node name                   = '//TRIM(NodeID(PTUnit(PTUnitNum)%AirOutNode)))
        ErrorsFound=.TRUE.
      END IF
      ! check that PTUnit outlet node is a zone inlet node.
      ZoneNodeNotFound = .TRUE.
      DO CtrlZone = 1,NumOfZones
        IF (.not. ZoneEquipConfig(CtrlZone)%IsControlled) CYCLE
        DO NodeNum = 1,ZoneEquipConfig(CtrlZone)%NumInletNodes
          IF (PTUnit(PTUnitNum)%AirOutNode .EQ. ZoneEquipConfig(CtrlZone)%InletNode(NodeNum)) THEN
            ZoneNodeNotFound = .FALSE.
            EXIT
          END IF
        END DO
      END DO
      IF(ZoneNodeNotFound)THEN
        CALL ShowSevereError(TRIM(CurrentModuleObject)//' "'//TRIM(PTUnit(PTUnitNum)%Name)//&
                           '" Heat Pumps air outlet node name must be the same as a zone inlet node name.')
        CALL ShowContinueError('..Zone inlet node name is specified in ZoneHVAC:EquipmentConnections object.')
        CALL ShowContinueError('..Heat pumps outlet node name = '//TRIM(NodeID(PTUnit(PTUnitNum)%AirOutNode)))
        ErrorsFound=.TRUE.
      END IF
    ELSE ! draw through fan from IF (PTUnit(PTUnitNum)%FanPlace == BlowThru) THEN
      ! check that PTUnit inlet node is a zone exhaust node.
      ZoneNodeNotFound = .TRUE.
      DO CtrlZone = 1,NumOfZones
        IF (.not. ZoneEquipConfig(CtrlZone)%IsControlled) CYCLE
        DO NodeNum = 1,ZoneEquipConfig(CtrlZone)%NumExhaustNodes
          IF (PTUnit(PTUnitNum)%AirInNode .EQ. ZoneEquipConfig(CtrlZone)%ExhaustNode(NodeNum)) THEN
            ZoneNodeNotFound = .FALSE.
            EXIT
          END IF
        END DO
      END DO
      IF(ZoneNodeNotFound)THEN
        CALL ShowSevereError(TRIM(CurrentModuleObject)//' "'//TRIM(PTUnit(PTUnitNum)%Name)//&
                           '" Heat Pumps air inlet node name must be the same as a zone exhaust node name.')
        CALL ShowContinueError('..Zone exhaust node name is specified in ZoneHVAC:EquipmentConnections object.')
        CALL ShowContinueError('..Heat pumps inlet node name = '//TRIM(NodeID(PTUnit(PTUnitNum)%AirInNode)))
        ErrorsFound=.TRUE.
      END IF
      ! check OA Mixer return node
      IF(PTUnit(PTUnitNum)%AirInNode /= OANodeNums(3))THEN
        CALL ShowSevereError(TRIM(CurrentModuleObject)//' "'//TRIM(PTUnit(PTUnitNum)%Name)//&
                             '" PTUnit air inlet node name must be the same as the OutdoorAir:Mixer return air node name.')
        CALL ShowContinueError('..PTUnit air inlet node name            = '//TRIM(NodeID(PTUnit(PTUnitNum)%AirInNode)))
        CALL ShowContinueError('..OutdoorAir:Mixer return air node name = '//TRIM(NodeID(OANodeNums(3))))
        ErrorsFound=.TRUE.
      END IF
      ! Fan outlet node name must be the same as the supplemental heating coil inlet node name
      IF(CoolCoilInletNodeNum /= OANodeNums(4))THEN
        CALL ShowSevereError(TRIM(CurrentModuleObject)//' "'//TRIM(PTUnit(PTUnitNum)%Name)//&
                           '" OutdoorAir:Mixer mixed air node name must be the same as the cooling coil')
        CALL ShowContinueError(' inlet node name when draw through '//TRIM(cAlphaFields(15))//' is specified.')
        CALL ShowContinueError('..OutdoorAir:Mixer mixed air name = '//TRIM(NodeID(OANodeNums(4))))
        CALL ShowContinueError('..Cooling coil inlet node name     = '//TRIM(NodeID(CoolCoilInletNodeNum)))
        ErrorsFound=.TRUE.
      END IF
      IF(CoolCoilOutletNodeNum /= HeatCoilInletNodeNum)THEN
        CALL ShowSevereError(TRIM(CurrentModuleObject)//' "'//TRIM(PTUnit(PTUnitNum)%Name)//&
                           '" Cooling coil outlet node name must be the same as the heating coil inlet node name.')
        CALL ShowContinueError('..Cooling coil outlet node name = '//TRIM(NodeID(CoolCoilOutletNodeNum)))
        CALL ShowContinueError('..Heating coil inlet node name  = '//TRIM(NodeID(HeatCoilInletNodeNum)))
        ErrorsFound=.TRUE.
      END IF
      IF(HeatCoilOutletNodeNum /= FanInletNodeNum)THEN
        CALL ShowSevereError(TRIM(CurrentModuleObject)//' "'//TRIM(PTUnit(PTUnitNum)%Name)//&
                           '" Heating coil outlet node name must be the same as the fan inlet node name')
        CALL ShowContinueError(' when draw through '//TRIM(cAlphaFields(15))//' is specified.')
        CALL ShowContinueError('..Heating coil outlet node name = '//TRIM(NodeID(HeatCoilOutletNodeNum)))
        CALL ShowContinueError('..Fan inlet node name           = '//TRIM(NodeID(FanInletNodeNum)))
        ErrorsFound=.TRUE.
      END IF
      IF (SuppHeatInletNodeNum /= FanOutletNodeNum) THEN
        CALL ShowSevereError(TRIM(CurrentModuleObject)//' "'//TRIM(PTUnit(PTUnitNum)%Name)//&
                             '" Fan outlet node name must be the same')
        CALL ShowContinueError('as the supplemental heating coil inlet node name when draw through '// &
                                TRIM(cAlphaFields(15))//' is specified.')
        CALL ShowContinueError('..Fan outlet node = '//TRIM(NodeID(FanOutletNodeNum)))
        CALL ShowContinueError('..Supplemental heating coil inlet node = '//TRIM(NodeID(SuppHeatInletNodeNum)))
        ErrorsFound=.TRUE.
      END IF
      IF(SuppHeatOutletNodeNum /= PTUnit(PTUnitNum)%AirOutNode)THEN
        CALL ShowSevereError(TRIM(CurrentModuleObject)//' "'//TRIM(PTUnit(PTUnitNum)%Name)//&
                           '" Supplemental heating coil outlet node name must be the same as the heat pumps outlet node name.')
        CALL ShowContinueError('..Supplemental heating coil outlet node name = '//TRIM(NodeID(SuppHeatOutletNodeNum)))
        CALL ShowContinueError('..Heat pumps outlet node name                = '//TRIM(NodeID(PTUnit(PTUnitNum)%AirOutNode)))
        ErrorsFound=.TRUE.
      END IF
      ! check that PTUnit outlet node is a zone inlet node.
      ZoneNodeNotFound = .TRUE.
      DO CtrlZone = 1,NumOfZones
        IF (.not. ZoneEquipConfig(CtrlZone)%IsControlled) CYCLE
        DO NodeNum = 1,ZoneEquipConfig(CtrlZone)%NumInletNodes
          IF (PTUnit(PTUnitNum)%AirOutNode .EQ. ZoneEquipConfig(CtrlZone)%InletNode(NodeNum)) THEN
            ZoneNodeNotFound = .FALSE.
            EXIT
          END IF
        END DO
      END DO
      IF(ZoneNodeNotFound)THEN
        CALL ShowSevereError(TRIM(CurrentModuleObject)//' "'//TRIM(PTUnit(PTUnitNum)%Name)//&
                           '" Heat Pumps air outlet node name must be the same as a zone inlet node name.')
        CALL ShowContinueError('..Zone inlet node name is specified in ZoneHVAC:EquipmentConnections object.')
        CALL ShowContinueError('..Heat pumps outlet node name = '//TRIM(NodeID(PTUnit(PTUnitNum)%AirOutNode)))
        ErrorsFound=.TRUE.
      END IF
    ENDIF ! IF (PTUnit(PTUnitNum)%FanPlace == BlowThru) THEN

    PTUnit(PTUnitNum)%FanSchedPtr     = GetScheduleIndex(Alphas(16))
    IF (.NOT. lAlphaBlanks(16) .AND. PTUnit(PTUnitNum)%FanSchedPtr == 0) THEN
      CALL ShowSevereError(TRIM(CurrentModuleObject)//' "'//TRIM(PTUnit(PTUnitNum)%Name)//&
                           '" '//TRIM(cAlphaFields(16))//' not found: '//TRIM(Alphas(16)))
      ErrorsFound=.TRUE.
    ELSEIF (lAlphaBlanks(16)) THEN
!     default to cycling fan if not specified in input
      PTUnit(PTUnitNum)%OpMode = CycFanCycCoil
    ENDIF

    IF (.NOT. lAlphaBlanks(17)) THEN
      PTUnit(PTUnitNum)%AvailManagerListName = Alphas(17)
      ZoneComp(PkgTermHPAirToAir_Num)%ZoneCompAvailMgrs(PTUnitNum)%AvailManagerListName  = Alphas(17)
    ENDIF

!   set air flow control mode, UseCompressorOnFlow = operate at last cooling or heating air flow requested when compressor is off
!                              UseCompressorOffFlow = operate at value specified by user
!   AirFlowControl only valid if fan opmode = ContFanCycCoil
    IF (PTUnit(PTUnitNum)%MaxNoCoolHeatAirVolFlow .EQ. 0.0d0) THEN
      PTUnit(PTUnitNum)%AirFlowControl = UseCompressorOnFlow
    ELSE
      PTUnit(PTUnitNum)%AirFlowControl = UseCompressorOffFlow
    END IF

!   Initialize last mode of compressor operation
    PTUnit(PTUnitNum)%LastMode = HeatingMode

    IF (SameString(PTUnit(PTUnitNum)%FanType, 'Fan:OnOff') .OR. &
        SameString(PTUnit(PTUnitNum)%FanType, 'Fan:ConstantVolume'))THEN
      IF(PTUnit(PTUnitNum)%FanSchedPtr .GT. 0 .AND. SameString(PTUnit(PTUnitNum)%FanType,'Fan:ConstantVolume'))THEN
        IF (.NOT. CheckScheduleValueMinMax(PTUnit(PTUnitNum)%FanSchedPtr,'>',0.0d0,'<=',1.0d0)) THEN
          CALL ShowSevereError(TRIM(CurrentModuleObject)//' "'//TRIM(PTUnit(PTUnitNum)%Name)//'"')
          CALL ShowContinueError('Fan operating mode must be continuous (fan operating mode schedule values > 0)'//&
                                 ' for supply fan type Fan:ConstantVolume.')
          CALL ShowContinueError('Error found in '//TRIM(cAlphaFields(16))//' = '//TRIM(Alphas(16)))
          CALL ShowContinueError('schedule values must be (>0., <=1.)')
          ErrorsFound=.TRUE.
        ELSEIF(PTUnit(PTUnitNum)%NoCoolHeatOutAirVolFlow .GT. PTUnit(PTUnitNum)%MaxNoCoolHeatAirVolFlow .AND. &
          PTUnit(PTUnitNum)%NoCoolHeatOutAirVolFlow .NE. AutoSize .AND.   &
          PTUnit(PTUnitNum)%MaxNoCoolHeatAirVolFlow .NE. AutoSize .AND. &
          PTUnit(PTUnitNum)%MaxNoCoolHeatAirVolFlow .NE. 0.0d0) THEN
          CALL ShowSevereError(TRIM(CurrentModuleObject)//' "'//TRIM(PTUnit(PTUnitNum)%Name)//'"')
          CALL ShowContinueError('Outdoor air flow rate when compressor is off cannot be greater than ' &
                                 //'supply air flow rate when compressor is off')
          ErrorsFound = .TRUE.
        END IF
      END IF
    ELSE
      CALL ShowSevereError(TRIM(CurrentModuleObject)//' "'//TRIM(PTUnit(PTUnitNum)%Name)//'"')
      CALL ShowContinueError(TRIM(cAlphaFields(8))//' "'//TRIM(PTUnit(PTUnitNum)%FanName)//&
                             '" must be type Fan:OnOff or Fan:ConstantVolume.')
      ErrorsFound=.TRUE.
    END IF

    IF (PTUnit(PTUnitNum)%DXHeatCoilType_Num == Coil_HeatingAirToAirVariableSpeed) THEN
      ErrFlag=.FALSE.
      PTUnit(PTUnitNum)%DesignHeatingCapacity =   &
         GetCoilCapacityVariableSpeed(PTUnit(PTUnitNum)%DXHeatCoilType, &
                                                                        PTUnit(PTUnitNum)%DXHeatCoilName,ErrFlag)
      IF (ErrFlag) THEN
        CALL ShowContinueError('...occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
        ErrorsFound=.TRUE.
      ENDIF
    ENDIF

    IF (PTUnit(PTUnitNum)%DXCoolCoilType_Num == Coil_CoolingAirToAirVariableSpeed) THEN
      ErrFlag=.FALSE.
      PTUnit(PTUnitNum)%DesignCoolingCapacity =   &
         GetCoilCapacityVariableSpeed(PTUnit(PTUnitNum)%DXCoolCoilType, &
                                                                        PTUnit(PTUnitNum)%DXCoolCoilName,ErrFlag)
      IF (ErrFlag) THEN
        CALL ShowContinueError('...occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
        ErrorsFound=.TRUE.
      ENDIF
    ENDIF

    CompSetFanInlet   = NodeID(FanInletNodeNum)
    CompSetFanOutlet  = NodeID(FanOutletNodeNum)
    CompSetCoolInlet  = NodeID(CoolCoilInletNodeNum)
    CompSetCoolOutlet = NodeID(CoolCoilOutletNodeNum)
    CompSetHeatInlet  = NodeID(HeatCoilInletNodeNum)
    CompSetHeatOutlet = NodeID(HeatCoilOutletNodeNum)
    CompSetSupHeatInlet  = NodeID(SuppHeatInletNodeNum)
    CompSetSupHeatOutlet = NodeID(SuppHeatOutletNodeNum)

    ! Add fan to component sets array
    CALL SetUpCompSets(PTUnit(PTUnitNum)%UnitType, PTUnit(PTUnitNum)%Name, &
                   PTUnit(PTUnitNum)%FanType,PTUnit(PTUnitNum)%FanName,CompSetFanInlet,CompSetFanOutlet)

    ! Add cooling coil to component sets array
    CALL SetUpCompSets(PTUnit(PTUnitNum)%UnitType, PTUnit(PTUnitNum)%Name, &
                   PTUnit(PTUnitNum)%DXCoolCoilType,PTUnit(PTUnitNum)%DXCoolCoilName,CompSetCoolInlet,CompSetCoolOutlet)

    ! Add heating coil to component sets array
    CALL SetUpCompSets(PTUnit(PTUnitNum)%UnitType, PTUnit(PTUnitNum)%Name, &
                   PTUnit(PTUnitNum)%DXHeatCoilType,PTUnit(PTUnitNum)%DXHeatCoilName,CompSetHeatInlet,CompSetHeatOutlet)

    ! Add supplemental heating coil to component sets array
    CALL SetUpCompSets(PTUnit(PTUnitNum)%UnitType, PTUnit(PTUnitNum)%Name, &
                   SuppHeatCoilType,PTUnit(PTUnitNum)%SuppHeatCoilName,CompSetSupHeatInlet,CompSetSupHeatOutlet)

    IF(PTUnit(PTUnitNum)%UnitType_Num .EQ. PTHPUnit)THEN
      IF (PTUnit(PTUnitNum)%SuppHeatCoilType_Num == Coil_HeatingWater) THEN
        ! Add heating coil water inlet node as actuator node for coil
        TempNodeNum  = GetOnlySingleNode(NodeID(PTUnit(PTUnitNum)%HotWaterControlNode),ErrorsFound,PTUnit(PTUnitNum)%UnitType, &
                              PTUnit(PTUnitNum)%Name,NodeType_Water,NodeConnectionType_Actuator,1,ObjectIsParent)
      ELSEIF (PTUnit(PTUnitNum)%SuppHeatCoilType_Num == Coil_HeatingSteam) THEN
        ! Add heating coil steam inlet node as actualtor node for coil
        TempNodeNum  = GetOnlySingleNode(NodeID(PTUnit(PTUnitNum)%HWCoilSteamInletNode),ErrorsFound,PTUnit(PTUnitNum)%UnitType, &
                             PTUnit(PTUnitNum)%Name, NodeType_Steam,NodeConnectionType_Actuator,1,ObjectIsParent)
      END IF
    END IF
    ! Set up component set for OA mixer - use OA node and Mixed air node
    CALL SetUpCompSets(PTUnit(PTUnitNum)%UnitType, PTUnit(PTUnitNum)%Name, &
                       PTUnit(PTUnitNum)%OAMixType, PTUnit(PTUnitNum)%OAMixName,NodeID(OANodeNums(1)),NodeID(OANodeNums(4)))
  END DO

  ! loop over PTAC units; get and load the input data
  DO PTUnitIndex = 1,NumPTAC

    FanInletNodeNum       = 0
    FanOutletNodeNum      = 0
    CoolCoilInletNodeNum  = 0
    CoolCoilOutletNodeNum = 0
    HeatCoilInletNodeNum  = 0
    HeatCoilOutletNodeNum = 0
    SuppHeatInletNodeNum   = 0

    CurrentModuleObject = 'ZoneHVAC:PackagedTerminalAirConditioner'
    CALL GetObjectItem(CurrentModuleObject,PTUnitIndex,Alphas,NumAlphas,Numbers,NumNumbers,IOStatus, &
                       NumBlank=lNumericBlanks,AlphaBlank=lAlphaBlanks, &
                       AlphaFieldNames=cAlphaFields,NumericFieldNames=cNumericFields)

    PTUnitNum = PTUnitIndex + NumPTHP
    IsNotOK=.FALSE.
    IsBlank=.FALSE.
    CALL VerifyName(Alphas(1),PTUnit%Name,PTUnitNum-1,IsNotOK,IsBlank,TRIM(CurrentModuleObject)//' Name')
    PTUnit(PTUnitNum)%PTObjectIndex = PTUnitIndex
    IF (IsNotOK) THEN
      ErrorsFound=.TRUE.
      IF (IsBlank) Alphas(1)='xxxxx'
    ENDIF
    PTUnit(PTUnitNum)%Name = Alphas(1)
    PTUnit(PTUnitNum)%UnitType = CurrentModuleObject
    PTUnit(PTUnitNum)%UnitType_Num = PTACUnit
    PTUnit(PTUnitNum)%ZoneEquipType = PkgTermACAirToAir_Num
    IF (lAlphaBlanks(2)) THEN
      PTUnit(PTUnitNum)%SchedPtr = ScheduleAlwaysOn
    ELSE
      PTUnit(PTUnitNum)%SchedPtr = GetScheduleIndex(Alphas(2))  ! convert schedule name to pointer (index number)
      IF (PTUnit(PTUnitNum)%SchedPtr .EQ. 0) THEN
        CALL ShowSevereError(TRIM(CurrentModuleObject)//'="'//TRIM(PTUnit(PTUnitNum)%Name)//'" invalid data.')
        CALL ShowContinueError('invalid-not found '//TRIM(cAlphaFields(2))//'="'//TRIM(Alphas(2))//'".')
        ErrorsFound=.TRUE.
      ENDIF
    END IF

    PTUnit(PTUnitNum)%AirInNode = &
               GetOnlySingleNode(Alphas(3),ErrorsFound,TRIM(CurrentModuleObject),Alphas(1), &
                            NodeType_Air,NodeConnectionType_Inlet,1,ObjectIsParent)

    PTUnit(PTUnitNum)%AirOutNode = &
               GetOnlySingleNode(Alphas(4),ErrorsFound,TRIM(CurrentModuleObject),Alphas(1), &
                            NodeType_Air,NodeConnectionType_Outlet,1,ObjectIsParent)

    PTUnit(PTUnitNum)%OAMixType = Alphas(5)
    PTUnit(PTUnitNum)%OAMixName = Alphas(6)

    ErrFlag = .false.
    CALL ValidateComponent(PTUnit(PTUnitNum)%OAMixType,PTUnit(PTUnitNum)%OAMixName,ErrFlag,TRIM(CurrentModuleObject))
    IF (ErrFlag) THEN
       CALL ShowContinueError('specified in '//TRIM(CurrentModuleObject)//' = "'//TRIM(PTUnit(PTUnitNum)%Name)//'".')
       ErrorsFound = .TRUE.
    ELSE
       ! OANodeNums = outside air mixer node numbers, OANodeNums(4) = outside air mixer mixed air node
       OANodeNums = GetOAMixerNodeNumbers(PTUnit(PTUnitNum)%OAMixName, ErrFlag)
       IF(ErrFlag) THEN
         CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(PTUnit(PTUnitNum)%Name))
         CALL ShowContinueError('..OutdoorAir:Mixer is required. Enter an OutdoorAir:Mixer object with this name.')
         ErrorsFound=.true.
       ELSE
         !  Set connection type to 'Inlet', because this is not necessarily directly come from
         !  outside air.  Outside Air Inlet Node List will set the connection to outside air
         PTUnit(PTUnitNum)%OutsideAirNode = OANodeNums(1)
         PTUnit(PTUnitNum)%AirReliefNode = OANodeNums(2)
       ENDIF
    ENDIF

    PTUnit(PTUnitNum)%MaxCoolAirVolFlow       = Numbers(1)
    IF (PTUnit(PTUnitNum)%MaxCoolAirVolFlow .LE. 0 .AND. PTUnit(PTUnitNum)%MaxCoolAirVolFlow .NE. AutoSize) THEN
      CALL ShowSevereError(TRIM(CurrentModuleObject)//' illegal '//TRIM(cNumericFields(1))//' = ' &
                           //TRIM(TrimSigDigits(Numbers(1),7)))
      CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(PTUnit(PTUnitNum)%Name))
      ErrorsFound = .TRUE.
    END IF

    PTUnit(PTUnitNum)%MaxHeatAirVolFlow       = Numbers(2)
    IF (PTUnit(PTUnitNum)%MaxHeatAirVolFlow .LE. 0 .AND. PTUnit(PTUnitNum)%MaxHeatAirVolFlow .NE. AutoSize) THEN
      CALL ShowSevereError(TRIM(CurrentModuleObject)//' illegal '//TRIM(cNumericFields(2))//' = ' &
                           //TRIM(TrimSigDigits(Numbers(2),7)))
      CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(PTUnit(PTUnitNum)%Name))
      ErrorsFound = .TRUE.
    END IF

    PTUnit(PTUnitNum)%MaxNoCoolHeatAirVolFlow = Numbers(3)
    IF (PTUnit(PTUnitNum)%MaxNoCoolHeatAirVolFlow .LT. 0 .AND. PTUnit(PTUnitNum)%MaxNoCoolHeatAirVolFlow .NE. AutoSize) THEN
      CALL ShowSevereError(TRIM(CurrentModuleObject)//' illegal '//TRIM(cNumericFields(3))//' = ' &
                           //TRIM(TrimSigDigits(Numbers(3),7)))
      CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(PTUnit(PTUnitNum)%Name))
      ErrorsFound = .TRUE.
    END IF

    PTUnit(PTUnitNum)%CoolOutAirVolFlow       = Numbers(4)
    IF (PTUnit(PTUnitNum)%CoolOutAirVolFlow .LT. 0 .AND. PTUnit(PTUnitNum)%CoolOutAirVolFlow .NE. AutoSize) THEN
      CALL ShowSevereError(TRIM(CurrentModuleObject)//' illegal '//TRIM(cNumericFields(4))//' = ' &
                           //TRIM(TrimSigDigits(Numbers(4),7)))
      CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(PTUnit(PTUnitNum)%Name))
      ErrorsFound = .TRUE.
    END IF

!   only check that SA flow in cooling is >= OA flow in cooling when either or both are not autosized
    IF (PTUnit(PTUnitNum)%CoolOutAirVolFlow .GT. PTUnit(PTUnitNum)%MaxCoolAirVolFlow .AND. &
        PTUnit(PTUnitNum)%CoolOutAirVolFlow .NE. AutoSize .AND. PTUnit(PTUnitNum)%MaxCoolAirVolFlow .NE. AutoSize) THEN
      CALL ShowSevereError(TRIM(CurrentModuleObject)//' '//TRIM(cNumericFields(4))//' cannot be greater than '// &
                           TRIM(cNumericFields(1)))
      CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(PTUnit(PTUnitNum)%Name))
      ErrorsFound = .TRUE.
    END IF

    PTUnit(PTUnitNum)%HeatOutAirVolFlow       = Numbers(5)
    IF (PTUnit(PTUnitNum)%HeatOutAirVolFlow .LT. 0 .AND. PTUnit(PTUnitNum)%HeatOutAirVolFlow.NE. AutoSize) THEN
      CALL ShowSevereError(TRIM(CurrentModuleObject)//' illegal '//TRIM(cNumericFields(5))//' = ' &
                           //TRIM(TrimSigDigits(Numbers(5),7)))
      CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(PTUnit(PTUnitNum)%Name))
      ErrorsFound = .TRUE.
    END IF

!   only check that SA flow in heating is >= OA flow in heating when either or both are not autosized
    IF (PTUnit(PTUnitNum)%HeatOutAirVolFlow .GT. PTUnit(PTUnitNum)%MaxHeatAirVolFlow .AND. &
        PTUnit(PTUnitNum)%HeatOutAirVolFlow .NE. AutoSize .AND. PTUnit(PTUnitNum)%MaxHeatAirVolFlow .NE. AutoSize) THEN
      CALL ShowSevereError(TRIM(CurrentModuleObject)//' '//TRIM(cNumericFields(5))//' cannot be greater than '// &
                           TRIM(cNumericFields(2)))
      CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(PTUnit(PTUnitNum)%Name))
      ErrorsFound = .TRUE.
    END IF

    PTUnit(PTUnitNum)%NoCoolHeatOutAirVolFlow = Numbers(6)
    IF (PTUnit(PTUnitNum)%NoCoolHeatOutAirVolFlow .LT. 0 .AND. PTUnit(PTUnitNum)%NoCoolHeatOutAirVolFlow .NE. AutoSize) THEN
      CALL ShowSevereError(TRIM(CurrentModuleObject)//' illegal '//TRIM(cNumericFields(6))//' = ' &
                           //TRIM(TrimSigDigits(Numbers(6),7)))
      CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(PTUnit(PTUnitNum)%Name))
      ErrorsFound = .TRUE.
    END IF

    !set minimum OA to something low because its not an input for PTACs
    PTUnit(PTUnitNum)%MinOATCompressor = -100.0D0

!   only check that SA flow when compressor is OFF is >= OA flow when compressor is OFF after fan mode is read in

    PTUnit(PTUnitNum)%FanType                 = Alphas(7)
    PTUnit(PTUnitNum)%FanName                 = Alphas(8)

    ! Get the fan's availabitlity schedule
    ErrFlag=.FALSE.
    PTUnit(PTUnitNum)%FanAvailSchedPtr = GetFanAvailSchPtr(PTUnit(PTUnitNum)%FanType,PTUnit(PTUnitNum)%FanName,ErrFlag)
    IF (ErrFlag) THEN
      CALL ShowContinueError('...specified in '//TRIM(CurrentModuleObject)//' = '//TRIM(PTUnit(PTUnitNum)%Name))
      ErrorsFound=.TRUE.
    ENDIF

    CALL ValidateComponent(PTUnit(PTUnitNum)%FanType,PTUnit(PTUnitNum)%FanName,IsNotOK,TRIM(CurrentModuleObject))
    IF (IsNotOK) THEN
      CALL ShowContinueError('In '//TRIM(CurrentModuleObject)//' = '//TRIM(PTUnit(PTUnitNum)%Name))
      ErrorsFound=.TRUE.
    ENDIF
    ErrFlag = .FALSE.
    CALL GetFanType(PTUnit(PTUnitNum)%FanName,PTUnit(PTUnitNum)%FanType_Num,ErrFlag,CurrentModuleObject,PTUnit(PTUnitNum)%Name)
    FanVolFlow = 0.d0
    IF(ErrFlag)THEN
      CALL ShowContinueError('...specified in '//TRIM(PTUnit(PTUnitNum)%UnitType)// &
                                        ' "'//TRIM(PTUnit(PTUnitNum)%Name)//'"')
      ErrorsFound = .TRUE.
    ELSE
      CALL GetFanIndex(PTUnit(PTUnitNum)%FanName,PTUnit(PTUnitNum)%FanIndex,ErrFlag,CurrentModuleObject)
      FanInletNodeNum = GetFanInletNode(PTUnit(PTUnitNum)%FanType,PTUnit(PTUnitNum)%FanName,ErrFlag)
      FanOutletNodeNum = GetFanOutletNode(PTUnit(PTUnitNum)%FanType,PTUnit(PTUnitNum)%FanName,ErrFlag)
      CALL GetFanVolFlow(PTUnit(PTUnitNum)%FanIndex,FanVolFlow)
      PTUnit(PTUnitNum)%ActualFanVolFlowRate = FanVolFlow
    END IF

    IF(FanVolFlow .NE. AutoSize)THEN
      IF(FanVolFlow .LT. MAX(PTUnit(PTUnitNum)%MaxCoolAirVolFlow, &
                             PTUnit(PTUnitNum)%MaxHeatAirVolFlow, &
                             PTUnit(PTUnitNum)%MaxNoCoolHeatAirVolFlow))THEN
        CALL ShowWarningError(TRIM(CurrentModuleObject)//' - air flow rate = '//TRIM(TrimSigDigits(FanVolFlow,7))// &
              ' in fan object '//TRIM(PTUnit(PTUnitNum)%FanName)//' is less than the maximum PTHP supply air flow rate.')
        CALL ShowContinueError(' The fan flow rate must be greater than the PTHP maximum supply air flow rate.')
        CALL ShowContinueError(' Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(PTUnit(PTUnitNum)%Name))
        ErrorsFound = .TRUE.
      END IF
    END IF

!   Name is currently used in CALL to Sim routines, can't get rid of the character string at this time.
    PTUnit(PTUnitNum)%ACHeatCoilName = Alphas(10)
    ACHeatCoilName                   = Alphas(10)


    IF (SameString(Alphas(9),'Coil:Heating:Gas') .OR. &
       SameString(Alphas(9),'Coil:Heating:Electric') .OR. &
       SameString(Alphas(9),'Coil:Heating:Water') .OR. &
       SameString(Alphas(9),'Coil:Heating:Steam') ) THEN
       PTUnit(PTUnitNum)%ACHeatCoilType = Alphas(9)
       IF (SameString(Alphas(9),'Coil:Heating:Gas') .OR. SameString(Alphas(9),'Coil:Heating:Electric')) THEN
         IF(SameString(Alphas(9),'Coil:Heating:Gas'))PTUnit(PTUnitNum)%ACHeatCoilType_Num = Coil_HeatingGas
         IF(SameString(Alphas(9),'Coil:Heating:Electric'))PTUnit(PTUnitNum)%ACHeatCoilType_Num = Coil_HeatingElectric
         PTUnit(PTUnitNum)%ACHeatCoilCap = GetHeatingCoilCapacity(PTUnit(PTUnitNum)%ACHeatCoilType,ACHeatCoilName,ErrorsFound)
         ErrFlag = .FALSE.
         HeatCoilInletNodeNum = GetHeatingCoilInletNode(PTUnit(PTUnitNum)%ACHeatCoilType,ACHeatCoilName,ErrFlag)
         HeatCoilOutletNodeNum = GetHeatingCoilOutletNode(PTUnit(PTUnitNum)%ACHeatCoilType,ACHeatCoilName,ErrFlag)
         IF(ErrFlag)THEN
           CALL ShowContinueError('...occurs in '//TRIM(PTUnit(PTUnitNum)%UnitType)// &
                                             ' "'//TRIM(PTUnit(PTUnitNum)%Name)//'"')
           ErrorsFound = .TRUE.
         END IF
       ELSEIF (SameString(Alphas(9),'Coil:Heating:Water')) THEN
         PTUnit(PTUnitNum)%ACHeatCoilType_Num = Coil_HeatingWater
         ErrFlag = .FALSE.
         PTUnit(PTUnitNum)%HotWaterControlNode = GetCoilWaterInletNode('Coil:Heating:Water',  &
                                                 ACHeatCoilName,ErrFlag)
         PTUnit(PTUnitNum)%MaxHeatCoilFluidFlow = GetCoilMaxWaterFlowRate('Coil:Heating:Water',  &
                                                 ACHeatCoilName,ErrFlag)
         IF(PTUnit(PTUnitNum)%MaxHeatCoilFluidFlow .GT. 0.0d0)THEN
           PTUnit(PTUnitNum)%MaxHeatCoilFluidFlow = GetCoilMaxWaterFlowRate('Coil:Heating:Water',  &
                                                    ACHeatCoilName,ErrFlag)
         END IF
         HeatCoilInletNodeNum = GetWaterCoilInletNode('Coil:Heating:Water',ACHeatCoilName,ErrFlag)
         PTUnit(PTUnitNum)%HWCoilAirInletNode = HeatCoilInletNodeNum
         HeatCoilOutletNodeNum = GetWaterCoilOutletNode('Coil:Heating:Water', &
                                                         PTUnit(PTUnitNum)%ACHeatCoilName,ErrFlag)
         IF(ErrFlag)THEN
           CALL ShowContinueError('...occurs in '//TRIM(PTUnit(PTUnitNum)%UnitType)// &
                                             ' "'//TRIM(PTUnit(PTUnitNum)%Name)//'"')
           ErrorsFound = .TRUE.
         END IF
       ELSEIF (SameString(Alphas(9),'Coil:Heating:Steam')) THEN
         PTUnit(PTUnitNum)%ACHeatCoilType_Num = Coil_HeatingSteam
         ErrFlag = .FALSE.
         PTUnit(PTUnitNum)%ACHeatCoilIndex      = GetSTeamCoilIndex(Alphas(9),ACHeatCoilName,ErrFlag)
         PTUnit(PTUnitNum)%HWCoilAirInletNode   = GetSteamCoilAirInletNode(PTUnit(PTUnitNum)%ACHeatCoilIndex,ACHeatCoilName,ErrFlag)
         PTUnit(PTUnitNum)%HWCoilSteamInletNode = GetSteamCoilSteamInletNode(PTUnit(PTUnitNum)%ACHeatCoilIndex,ACHeatCoilName, &
                                                  ErrFlag)
         PTUnit(PTUnitNum)%MaxHeatCoilFluidFlow = GetCoilMaxSteamFlowRate(PTUnit(PTUnitNum)%ACHeatCoilIndex,ErrFlag)
         SteamIndex = 0 ! Function GetSatDensityRefrig will look up steam index if 0 is passed
         SteamDensity=GetSatDensityRefrig("STEAM",TempSteamIn,1.0d0,SteamIndex,'GetPackagedTerminalHeatPumpInput')
         IF(PTUnit(PTUnitNum)%MaxHeatCoilFluidFlow .GT. 0.0d0)THEN
           PTUnit(PTUnitNum)%MaxHeatCoilFluidFlow = &
                              GetCoilMaxSteamFlowRate(PTUnit(PTUnitNum)%ACHeatCoilIndex,ErrFlag) * SteamDensity
         END IF
         HeatCoilInletNodeNum = PTUnit(PTUnitNum)%HWCoilAirInletNode
         HeatCoilOutletNodeNum = GetSteamCoilAirOutletNode(PTUnit(PTUnitNum)%ACHeatCoilIndex,ACHeatCoilName,ErrFlag)
         IF(ErrFlag)THEN
           CALL ShowContinueError('...occurs in '//TRIM(PTUnit(PTUnitNum)%UnitType)// &
                                             ' "'//TRIM(PTUnit(PTUnitNum)%Name)//'"')
           ErrorsFound = .TRUE.
         END IF
         IF(GetTypeOfCoil(PTUnit(PTUnitNum)%ACHeatCoilIndex,ACHeatCoilName,ErrFlag) /= ZoneLoadControl)THEN
           IF(ErrFlag)THEN
             CALL ShowContinueError('...occurs in '//TRIM(PTUnit(PTUnitNum)%UnitType)// &
                                             ' "'//TRIM(PTUnit(PTUnitNum)%Name)//'"')
             ErrorsFound = .TRUE.
           END IF
           CALL ShowSevereError(TRIM(CurrentModuleObject)//TRIM(PTUnit(PTUnitNum)%Name)//&
                           '" Steam coil type of control must be set to ZoneLoadControl in the heating coil = ' &
                           //'Coil:Heating:Steam "'//TRIM(ACHeatCoilName)//'"')
           ErrorsFound=.TRUE.
         END IF
       END IF
    ELSE
      CALL ShowWarningError(TRIM(CurrentModuleObject)//' illegal '//TRIM(cAlphaFields(9))//' = '//TRIM(Alphas(9)))
      CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(PTUnit(PTUnitNum)%Name))
      ErrorsFound = .TRUE.
    END IF

    PTUnit(PTUnitNum)%HeatConvergenceTol      = 0.001d0
    PTUnit(PTUnitNum)%DXCoolCoilName          = Alphas(12)

    IF(SameString(Alphas(11),'Coil:Cooling:DX:SingleSpeed') .OR. &
       SameString(Alphas(11),'CoilSystem:Cooling:DX:HeatExchangerAssisted')) THEN
       PTUnit(PTUnitNum)%DXCoolCoilType = Alphas(11)
       IF (SameString(Alphas(11),'Coil:Cooling:DX:SingleSpeed')) THEN
         PTUnit(PTUnitNum)%DXCoolCoilType_Num = CoilDX_CoolingSingleSpeed
         ErrFlag = .FALSE.
         CALL GetDXCoolCoilIndex(PTUnit(PTUnitNum)%DXCoolCoilName,PTUnit(PTUnitNum)%DXCoolCoilIndexNum, &
                              ErrFlag, PTUnit(PTUnitNum)%DXCoolCoilType)
         CoolCoilInletNodeNum = GetDXCoilInletNode(PTUnit(PTUnitNum)%DXCoolCoilType,PTUnit(PTUnitNum)%DXCoolCoilName,ErrFlag)
         CoolCoilOutletNodeNum = GetDXCoilOutletNode(PTUnit(PTUnitNum)%DXCoolCoilType,PTUnit(PTUnitNum)%DXCoolCoilName,ErrFlag)
         PTUnit(PTUnitNum)%CondenserNodeNum = &
                         GetCoilCondenserInletNode(PTUnit(PTUnitNum)%DXCoolCoilType,PTUnit(PTUnitNum)%DXCoolCoilName,ErrFlag)
         IF(ErrFlag)THEN
           CALL ShowContinueError('...occurs in '//TRIM(PTUnit(PTUnitNum)%UnitType)// &
                                             ' "'//TRIM(PTUnit(PTUnitNum)%Name)//'"')
           ErrorsFound = .TRUE.
         END IF
       ELSEIF (SameString(Alphas(11),'CoilSystem:Cooling:DX:HeatExchangerAssisted')) THEN
         PTUnit(PTUnitNum)%DXCoolCoilType_Num = CoilDX_CoolingHXAssisted
         ErrFlag = .FALSE.
         CALL GetDXCoolCoilIndex( &
               GetHXDXCoilName(PTUnit(PTUnitNum)%DXCoolCoilType,PTUnit(PTUnitNum)%DXCoolCoilName,ErrFlag), &
               PTUnit(PTUnitNum)%DXCoolCoilIndexNum, ErrFlag, 'Coil:Cooling:DX:SingleSpeed')
         CoolCoilInletNodeNum = GetHXDXCoilInletNode(PTUnit(PTUnitNum)%DXCoolCoilType,PTUnit(PTUnitNum)%DXCoolCoilName,ErrFlag)
         CoolCoilOutletNodeNum = GetHXDXCoilOutletNode(PTUnit(PTUnitNum)%DXCoolCoilType,PTUnit(PTUnitNum)%DXCoolCoilName,ErrFlag)
         PTUnit(PTUnitNum)%CondenserNodeNum = GetCoilCondenserInletNode('Coil:Cooling:DX:SingleSpeed', &
                        GetHXDXCoilName(PTUnit(PTUnitNum)%DXCoolCoilType,PTUnit(PTUnitNum)%DXCoolCoilName,ErrFlag),ErrFlag)
         IF(ErrFlag)THEN
           CALL ShowContinueError('...occurs in '//TRIM(PTUnit(PTUnitNum)%UnitType)// &
                                             ' "'//TRIM(PTUnit(PTUnitNum)%Name)//'"')
           ErrorsFound = .TRUE.
         END IF
       END IF
   ELSE IF (SameString(Alphas(11), 'COIL:COOLING:DX:VARIABLESPEED') )THEN
      PTUnit(PTUnitNum)%DXCoolCoilType = Alphas(11)
      PTUnit(PTUnitNum)%DXCoolCoilType_Num = Coil_CoolingAirToAirVariableSpeed
      PTUnit(PTUnitNum)%DXCoolCoilName = Alphas(12)
      CALL ValidateComponent(PTUnit(PTUnitNum)%DXCoolCoilType,PTUnit(PTUnitNum)%DXCoolCoilName,IsNotOK,  &
                             TRIM(CurrentModuleObject))
      IF (IsNotOK) THEN
        CALL ShowContinueError('...specified in '//TRIM(CurrentModuleObject)//'="'//TRIM(Alphas(1))//'".')
        ErrorsFound=.TRUE.
      ELSE
        ErrFlag = .FALSE.
        PTUnit(PTUnitNum)%DXCoolCoilIndexNum = GetCoilIndexVariableSpeed(PTUnit(PTUnitNum)%DXCoolCoilType, &
                                                                        PTUnit(PTUnitNum)%DXCoolCoilName,ErrFlag)
        IF(ErrFlag)THEN
          CALL ShowContinueError('...specified in '//TRIM(CurrentModuleObject)//'="'//TRIM(Alphas(1))//'".')
          ErrorsFound=.TRUE.
        END IF
        CoolCoilInletNodeNum=GetCoilInletNodeVariableSpeed(PTUnit(PTUnitNum)%DXCoolCoilType,  &
                                                          PTUnit(PTUnitNum)%DXCoolCoilName,ErrFlag)
        CoolCoilOutletNodeNum=GetCoilOutletNodeVariableSpeed(PTUnit(PTUnitNum)%DXCoolCoilType,  &
                                                            PTUnit(PTUnitNum)%DXCoolCoilName,ErrFlag)
        PTUnit(PTUnitNum)%CondenserNodeNum = GetVSCoilCondenserInletNode(PTUnit(PTUnitNum)%DXCoolCoilName,ErrFlag)

        IF(ErrFlag)CALL ShowContinueError('...occurs in '//TRIM(PTUnit(PTUnitNum)%UnitType)// &
                                           ' "'//TRIM(PTUnit(PTUnitNum)%Name)//'"')
      ENDIF
    ELSE
      CALL ShowWarningError(TRIM(CurrentModuleObject)//' illegal '//TRIM(cAlphaFields(11))//' = '//TRIM(Alphas(11)))
      CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(PTUnit(PTUnitNum)%Name))
      ErrorsFound = .TRUE.
    END IF

    IF (SameString(Alphas(13),'BlowThrough'))  PTUnit(PTUnitNum)%FanPlace = BlowThru
    IF (SameString(Alphas(13),'DrawThrough'))  PTUnit(PTUnitNum)%FanPlace = DrawThru
!   default to draw through if not specified in input
    IF (lAlphaBlanks(13))           PTUnit(PTUnitNum)%FanPlace = DrawThru
    IF (PTUnit(PTUnitNum)%FanPlace .EQ. 0) THEN
      CALL ShowSevereError(TRIM(CurrentModuleObject)//' illegal '//TRIM(cAlphaFields(13))//' = '//TRIM(Alphas(13)))
      CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(PTUnit(PTUnitNum)%Name))
      ErrorsFound = .TRUE.
    END IF

    ! Check component placement
    IF (PTUnit(PTUnitNum)%FanPlace == BlowThru) THEN
      ! PTUnit inlet node must be the same as a zone exhaust node and the OA Mixer return node
      ! check that PTUnit inlet node is a zone exhaust node.
      ZoneNodeNotFound = .TRUE.
      DO CtrlZone = 1,NumOfZones
        IF (.not. ZoneEquipConfig(CtrlZone)%IsControlled) CYCLE
        DO NodeNum = 1,ZoneEquipConfig(CtrlZone)%NumExhaustNodes
          IF (PTUnit(PTUnitNum)%AirInNode .EQ. ZoneEquipConfig(CtrlZone)%ExhaustNode(NodeNum)) THEN
            ZoneNodeNotFound = .FALSE.
            EXIT
          END IF
        END DO
      END DO
      IF(ZoneNodeNotFound)THEN
        CALL ShowSevereError(TRIM(CurrentModuleObject)//' "'//TRIM(PTUnit(PTUnitNum)%Name)//&
                           '" Air Conditioners air inlet node name must be the same as a zone exhaust node name.')
        CALL ShowContinueError('..Zone exhaust node name is specified in ZoneHVAC:EquipmentConnections object.')
        CALL ShowContinueError('..Air Conditioners inlet node name = '//TRIM(NodeID(PTUnit(PTUnitNum)%AirInNode)))
        ErrorsFound=.TRUE.
      END IF
      ! check OA Mixer return node
      IF(PTUnit(PTUnitNum)%AirInNode /= OANodeNums(3))THEN
        CALL ShowSevereError(TRIM(CurrentModuleObject)//' "'//TRIM(PTUnit(PTUnitNum)%Name)//&
                       '" Air Conditioners air inlet node name must be the same as the OutdoorAir:Mixer return air node name.')
        CALL ShowContinueError('..PTUnit air inlet node name            = '//TRIM(NodeID(PTUnit(PTUnitNum)%AirInNode)))
        CALL ShowContinueError('..OutdoorAir:Mixer return air node name = '//TRIM(NodeID(OANodeNums(3))))
        ErrorsFound=.TRUE.
      END IF
      ! Fan inlet node name must be the same as the heat pump's OA mixer mixed air node name
      IF (OANodeNums(4) /= FanInletNodeNum) THEN
        CALL ShowSevereError(TRIM(CurrentModuleObject)//' "'//TRIM(PTUnit(PTUnitNum)%Name)//&
                             '" Fan inlet node name must be the same as the air conditioners')
        CALL ShowContinueError('OutdoorAir:Mixer mixed air node name when blow through '// &
                               TRIM(cAlphaFields(13))//' is specified.')
        CALL ShowContinueError('..Fan inlet node name                   = '//TRIM(NodeID(FanInletNodeNum)))
        CALL ShowContinueError('..OutdoorAir:Mixer mixed air node name = '//TRIM(NodeID(OANodeNums(4))))
        ErrorsFound=.TRUE.
      END IF
      IF(CoolCoilInletNodeNum /= FanOutletNodeNum)THEN
        CALL ShowSevereError(TRIM(CurrentModuleObject)//' "'//TRIM(PTUnit(PTUnitNum)%Name)//&
                           '" Fan outlet node name must be the same as the cooling coil')
        CALL ShowContinueError(' inlet node name when blow through '//TRIM(cAlphaFields(12))//' is specified.')
        CALL ShowContinueError('..Fan outlet node name         = '//TRIM(NodeID(FanOutletNodeNum)))
        CALL ShowContinueError('..Cooling coil inlet node name = '//TRIM(NodeID(CoolCoilInletNodeNum)))
        ErrorsFound=.TRUE.
      END IF
      IF(CoolCoilOutletNodeNum /= HeatCoilInletNodeNum)THEN
        CALL ShowSevereError(TRIM(CurrentModuleObject)//' "'//TRIM(PTUnit(PTUnitNum)%Name)//&
                           '" Cooling coil outlet node name must be the same as the heating coil inlet node name.')
        CALL ShowContinueError('..Cooling coil outlet node name = '//TRIM(NodeID(CoolCoilOutletNodeNum)))
        CALL ShowContinueError('..Heating coil inlet node name  = '//TRIM(NodeID(HeatCoilInletNodeNum)))
        ErrorsFound=.TRUE.
      END IF
      IF(HeatCoilOutletNodeNum /= PTUnit(PTUnitNum)%AirOutNode)THEN
        CALL ShowSevereError(TRIM(CurrentModuleObject)//' "'//TRIM(PTUnit(PTUnitNum)%Name)//&
                           '" Heating coil outlet node name must be the same as the air conditioners outlet')
        CALL ShowContinueError(' node name when blow through '//TRIM(cAlphaFields(12))//' is specified.')
        CALL ShowContinueError('..Heating coil outlet node name      = '//TRIM(NodeID(HeatCoilOutletNodeNum)))
        CALL ShowContinueError('..Air conditioners outlet node name  = '//TRIM(NodeID(SuppHeatInletNodeNum)))
        ErrorsFound=.TRUE.
      END IF
      ! check that PTUnit outlet node is a zone inlet node.
      ZoneNodeNotFound = .TRUE.
      DO CtrlZone = 1,NumOfZones
        IF (.not. ZoneEquipConfig(CtrlZone)%IsControlled) CYCLE
        DO NodeNum = 1,ZoneEquipConfig(CtrlZone)%NumInletNodes
          IF (PTUnit(PTUnitNum)%AirOutNode .EQ. ZoneEquipConfig(CtrlZone)%InletNode(NodeNum)) THEN
            ZoneNodeNotFound = .FALSE.
            EXIT
          END IF
        END DO
      END DO
      IF(ZoneNodeNotFound)THEN
        CALL ShowSevereError(TRIM(CurrentModuleObject)//' "'//TRIM(PTUnit(PTUnitNum)%Name)//&
                           '" Air Conditioners air outlet node name must be the same as a zone inlet node name.')
        CALL ShowContinueError('..Zone inlet node name is specified in ZoneHVAC:EquipmentConnections object.')
        CALL ShowContinueError('..Air Conditioners outlet node name = '//TRIM(NodeID(PTUnit(PTUnitNum)%AirOutNode)))
        ErrorsFound=.TRUE.
      END IF
    ELSE ! draw through fan from IF (PTUnit(PTUnitNum)%FanPlace == BlowThru) THEN
      ! PTUnit inlet node must be the same as a zone exhaust node and the OA Mixer return node
      ! check that PTUnit inlet node is a zone exhaust node.
      ZoneNodeNotFound = .TRUE.
      DO CtrlZone = 1,NumOfZones
        IF (.not. ZoneEquipConfig(CtrlZone)%IsControlled) CYCLE
        DO NodeNum = 1,ZoneEquipConfig(CtrlZone)%NumExhaustNodes
          IF (PTUnit(PTUnitNum)%AirInNode .EQ. ZoneEquipConfig(CtrlZone)%ExhaustNode(NodeNum)) THEN
            ZoneNodeNotFound = .FALSE.
            EXIT
          END IF
        END DO
      END DO
      IF(ZoneNodeNotFound)THEN
        CALL ShowSevereError(TRIM(CurrentModuleObject)//' "'//TRIM(PTUnit(PTUnitNum)%Name)//&
                           '" Air Conditioners air inlet node name must be the same as a zone exhaust node name.')
        CALL ShowContinueError('..Zone exhaust node name is specified in ZoneHVAC:EquipmentConnections object.')
        CALL ShowContinueError('..Air Conditioners inlet node name = '//TRIM(NodeID(PTUnit(PTUnitNum)%AirInNode)))
        ErrorsFound=.TRUE.
      END IF
      ! check OA Mixer return node
      IF(PTUnit(PTUnitNum)%AirInNode /= OANodeNums(3))THEN
        CALL ShowSevereError(TRIM(CurrentModuleObject)//' "'//TRIM(PTUnit(PTUnitNum)%Name)//&
                      '" Air Conditioners air inlet node name must be the same as the OutdoorAir:Mixer return air node name.')
        CALL ShowContinueError('..Air Conditioner air inlet node name   = '//TRIM(NodeID(PTUnit(PTUnitNum)%AirInNode)))
        CALL ShowContinueError('..OutdoorAir:Mixer return air node name = '//TRIM(NodeID(OANodeNums(3))))
        ErrorsFound=.TRUE.
      END IF
      ! cooling coil inlet node name must be the same as the OA mixers mixed air node name
      IF(CoolCoilInletNodeNum /= OANodeNums(4))THEN
        CALL ShowSevereError(TRIM(CurrentModuleObject)//' "'//TRIM(PTUnit(PTUnitNum)%Name)//&
                           '" OutdoorAir:Mixer mixed air node name must be the same as the cooling coil')
        CALL ShowContinueError(' inlet node name when draw through '//TRIM(cAlphaFields(13))//' is specified.')
        CALL ShowContinueError('..OutdoorAir:Mixer mixed air name = '//TRIM(NodeID(OANodeNums(4))))
        CALL ShowContinueError('..Cooling coil inlet node name     = '//TRIM(NodeID(CoolCoilInletNodeNum)))
        ErrorsFound=.TRUE.
      END IF
      IF(CoolCoilOutletNodeNum /= HeatCoilInletNodeNum)THEN
        CALL ShowSevereError(TRIM(CurrentModuleObject)//' "'//TRIM(PTUnit(PTUnitNum)%Name)//&
                           '" Cooling coil outlet node name must be the same as the heating coil inlet node name.')
        CALL ShowContinueError('..Cooling coil outlet node name = '//TRIM(NodeID(CoolCoilOutletNodeNum)))
        CALL ShowContinueError('..Heating coil inlet node name  = '//TRIM(NodeID(HeatCoilInletNodeNum)))
        ErrorsFound=.TRUE.
      END IF
      IF(HeatCoilOutletNodeNum /= FanInletNodeNum)THEN
        CALL ShowSevereError(TRIM(CurrentModuleObject)//' "'//TRIM(PTUnit(PTUnitNum)%Name)//&
                           '" Heating coil outlet node name must be the same as the fan inlet node name')
        CALL ShowContinueError(' when blow through '//TRIM(cAlphaFields(13))//' is specified.')
        CALL ShowContinueError('..Heating coil outlet node name = '//TRIM(NodeID(HeatCoilOutletNodeNum)))
        CALL ShowContinueError('..Fan inlet node name           = '//TRIM(NodeID(FanInletNodeNum)))
        ErrorsFound=.TRUE.
      END IF
      IF (FanOutletNodeNum /= PTUnit(PTUnitNum)%AirOutNode) THEN
        CALL ShowSevereError(TRIM(CurrentModuleObject)//' "'//TRIM(PTUnit(PTUnitNum)%Name)//&
                           '" Fan outlet node name must be the same')
        CALL ShowContinueError('as the air conditioners outlet node name when draw through '// &
                               TRIM(cAlphaFields(13))//' is specified.')
        CALL ShowContinueError('..Fan outlet node  name             = '//TRIM(NodeID(FanOutletNodeNum)))
        CALL ShowContinueError('..Air conditioners outlet node name = '//TRIM(NodeID(SuppHeatInletNodeNum)))
        ErrorsFound=.TRUE.
      END IF
      ! check that PTUnit outlet node is a zone inlet node.
      ZoneNodeNotFound = .TRUE.
      DO CtrlZone = 1,NumOfZones
        IF (.not. ZoneEquipConfig(CtrlZone)%IsControlled) CYCLE
        DO NodeNum = 1,ZoneEquipConfig(CtrlZone)%NumInletNodes
          IF (PTUnit(PTUnitNum)%AirOutNode .EQ. ZoneEquipConfig(CtrlZone)%InletNode(NodeNum)) THEN
            ZoneNodeNotFound = .FALSE.
            EXIT
          END IF
        END DO
      END DO
      IF(ZoneNodeNotFound)THEN
        CALL ShowSevereError(TRIM(CurrentModuleObject)//' "'//TRIM(PTUnit(PTUnitNum)%Name)//&
                           '" Air Conditionerss air outlet node name must be the same as a zone inlet node name.')
        CALL ShowContinueError('..Zone inlet node name is specified in ZoneHVAC:EquipmentConnections object.')
        CALL ShowContinueError('..Air Conditioners outlet node name = '//TRIM(NodeID(PTUnit(PTUnitNum)%AirOutNode)))
        ErrorsFound=.TRUE.
      END IF
    ENDIF ! IF (PTUnit(PTUnitNum)%FanPlace == BlowThru) THEN

    PTUnit(PTUnitNum)%FanSchedPtr     = GetScheduleIndex(Alphas(14))
    IF (.NOT. lAlphaBlanks(14) .AND. PTUnit(PTUnitNum)%FanSchedPtr == 0) THEN
      CALL ShowSevereError(TRIM(CurrentModuleObject)//' "'//TRIM(PTUnit(PTUnitNum)%Name)//&
                           '" '//TRIM(cAlphaFields(14))//' not found: '//TRIM(Alphas(14)))
      ErrorsFound=.TRUE.
    ELSEIF (lAlphaBlanks(14)) THEN
!     default to cycling fan if not specified in input
      PTUnit(PTUnitNum)%OpMode = CycFanCycCoil
    ENDIF

    IF (.NOT. lAlphaBlanks(15)) THEN
      PTUnit(PTUnitNum)%AvailManagerListName = Alphas(15)
      ZoneComp(PkgTermACAirToAir_Num)%ZoneCompAvailMgrs(PTUnitNum)%AvailManagerListName  = Alphas(15)
    ENDIF
!   set air flow control mode, UseCompressorOnFlow = operate at last cooling or heating air flow requested when compressor is off
!                              UseCompressorOffFlow = operate at value specified by user
!   AirFlowControl only valid if fan opmode = ContFanCycCoil
    IF (PTUnit(PTUnitNum)%MaxNoCoolHeatAirVolFlow .EQ. 0.0d0) THEN
      PTUnit(PTUnitNum)%AirFlowControl = UseCompressorOnFlow
    ELSE
      PTUnit(PTUnitNum)%AirFlowControl = UseCompressorOffFlow
    END IF

!   Initialize last mode of compressor operation
    PTUnit(PTUnitNum)%LastMode = HeatingMode

    IF (SameString(PTUnit(PTUnitNum)%FanType, 'Fan:OnOff') .OR. &
        SameString(PTUnit(PTUnitNum)%FanType, 'Fan:ConstantVolume'))THEN
      IF(PTUnit(PTUnitNum)%FanSchedPtr .GT. 0 .AND. SameString(PTUnit(PTUnitNum)%FanType,'Fan:ConstantVolume'))THEN
        IF (.NOT. CheckScheduleValueMinMax(PTUnit(PTUnitNum)%FanSchedPtr,'>',0.0d0,'<=',1.0d0)) THEN
          CALL ShowSevereError(TRIM(CurrentModuleObject)//' "'//TRIM(PTUnit(PTUnitNum)%Name)//'"')
          CALL ShowContinueError('Fan operating mode must be continuous (fan operating mode schedule values > 0)'//&
                                 ' for supply fan type Fan:ConstantVolume.')
          CALL ShowContinueError('Error found in '//TRIM(cAlphaFields(14))//' = '//TRIM(Alphas(14)))
          CALL ShowContinueError('schedule values must be (>0., <=1.)')
          ErrorsFound=.TRUE.
        ELSEIF(PTUnit(PTUnitNum)%NoCoolHeatOutAirVolFlow .GT. PTUnit(PTUnitNum)%MaxNoCoolHeatAirVolFlow .AND. &
          PTUnit(PTUnitNum)%NoCoolHeatOutAirVolFlow .NE. AutoSize .AND.   &
          PTUnit(PTUnitNum)%MaxNoCoolHeatAirVolFlow .NE. AutoSize .AND. &
          PTUnit(PTUnitNum)%MaxNoCoolHeatAirVolFlow .NE. 0.0d0) THEN
          CALL ShowSevereError(TRIM(CurrentModuleObject)//' "'//TRIM(PTUnit(PTUnitNum)%Name)//'"')
          CALL ShowContinueError('Outdoor air flow rate when compressor is off cannot be greater than ' &
                         //'supply air flow rate when compressor is off')
          ErrorsFound = .TRUE.
        END IF
      END IF
    ELSE
      CALL ShowSevereError(TRIM(CurrentModuleObject)//' "'//TRIM(PTUnit(PTUnitNum)%Name)//'"')
      CALL ShowContinueError(TRIM(cAlphaFields(8))//' "'//TRIM(PTUnit(PTUnitNum)%FanName)//&
                    '" must be type Fan:OnOff or Fan:ConstantVolume.')
      ErrorsFound=.TRUE.
    END IF

    IF (PTUnit(PTUnitNum)%DXCoolCoilType_Num == Coil_CoolingAirToAirVariableSpeed) THEN
      ErrFlag=.FALSE.
      PTUnit(PTUnitNum)%DesignCoolingCapacity =   &
         GetCoilCapacityVariableSpeed(PTUnit(PTUnitNum)%DXCoolCoilType, &
                                                                        PTUnit(PTUnitNum)%DXCoolCoilName,ErrFlag)
      IF (ErrFlag) THEN
        CALL ShowContinueError('...occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
        ErrorsFound=.TRUE.
      ENDIF
    ENDIF

    CompSetFanInlet   = NodeID(FanInletNodeNum)
    CompSetFanOutlet  = NodeID(FanOutletNodeNum)
    CompSetCoolInlet  = NodeID(CoolCoilInletNodeNum)
    CompSetCoolOutlet = NodeID(CoolCoilOutletNodeNum)
    CompSetHeatInlet  = NodeID(HeatCoilInletNodeNum)
    CompSetHeatOutlet = NodeID(HeatCoilOutletNodeNum)

    ! Add fan to component sets array
    CALL SetUpCompSets(PTUnit(PTUnitNum)%UnitType, PTUnit(PTUnitNum)%Name, &
                       PTUnit(PTUnitNum)%FanType,PTUnit(PTUnitNum)%FanName, &
                       NodeID(FanInletNodeNum),NodeID(FanOutletNodeNum))

    ! Add cooling coil to component sets array
    CALL SetUpCompSets(PTUnit(PTUnitNum)%UnitType, PTUnit(PTUnitNum)%Name, &
                       PTUnit(PTUnitNum)%DXCoolCoilType,PTUnit(PTUnitNum)%DXCoolCoilName, &
                       NodeID(CoolCoilInletNodeNum),NodeID(CoolCoilOutletNodeNum))

    ! Add heating coil to component sets array
    CALL SetUpCompSets(PTUnit(PTUnitNum)%UnitType, PTUnit(PTUnitNum)%Name, &
                       PTUnit(PTUnitNum)%ACHeatCoilType,ACHeatCoilName, &
                       NodeID(HeatCoilInletNodeNum),NodeID(HeatCoilOutletNodeNum))

    IF(PTUnit(PTUnitNum)%UnitType_Num .EQ. PTACUnit)THEN
      IF (PTUnit(PTUnitNum)%ACHeatCoilType_Num == Coil_HeatingWater) THEN
        ! Add heating coil water inlet node as actuator node for coil
        TempNodeNum  = GetOnlySingleNode(NodeID(PTUnit(PTUnitNum)%HotWaterControlNode),ErrorsFound,PTUnit(PTUnitNum)%UnitType, &
                              PTUnit(PTUnitNum)%Name,NodeType_Water,NodeConnectionType_Actuator,1,ObjectIsParent)
      ELSEIF (PTUnit(PTUnitNum)%ACHeatCoilType_Num == Coil_HeatingSteam) THEN
        ! Add heating coil steam inlet node as actualtor node for coil
        TempNodeNum  = GetOnlySingleNode(NodeID(PTUnit(PTUnitNum)%HWCoilSteamInletNode),ErrorsFound,PTUnit(PTUnitNum)%UnitType, &
                             PTUnit(PTUnitNum)%Name, NodeType_Steam,NodeConnectionType_Actuator,1,ObjectIsParent)
      END IF
    END IF

    ! Set up component set for OA mixer - use OA node and Mixed air node
    CALL SetUpCompSets(PTUnit(PTUnitNum)%UnitType, PTUnit(PTUnitNum)%Name, &
                       PTUnit(PTUnitNum)%OAMixType,PTUnit(PTUnitNum)%OAMixName,NodeID(OANodeNums(1)),NodeID(OANodeNums(4)))
  END DO




!***********************************************************************************


  DO PTUnitIndex = 1,NumPTWSHP

    FanInletNodeNum       = 0
    FanOutletNodeNum      = 0
    CoolCoilInletNodeNum  = 0
    CoolCoilOutletNodeNum = 0
    HeatCoilInletNodeNum  = 0
    HeatCoilOutletNodeNum = 0
    SuppHeatInletNodeNum  = 0
    SuppHeatOutletNodeNum = 0
    SuppHeatHWInletNodeNum = 0
    SuppHeatHWOutletNodeNum = 0
    OANodeNums = 0

    CurrentModuleObject = 'ZoneHVAC:WaterToAirHeatPump'
    CALL GetObjectItem(CurrentModuleObject,PTUnitIndex,Alphas,NumAlphas,Numbers,NumNumbers,IOStatus, &
                       NumBlank=lNumericBlanks,AlphaBlank=lAlphaBlanks, &
                       AlphaFieldNames=cAlphaFields,NumericFieldNames=cNumericFields)

    PTUnitNum = PTUnitIndex + NumPTHP + NumPTAC
    IsNotOK=.FALSE.
    IsBlank=.FALSE.
    CALL VerifyName(Alphas(1),PTUnit%Name,PTUnitNum-1,IsNotOK,IsBlank,TRIM(CurrentModuleObject)//' Name')
    PTUnit(PTUnitNum)%PTObjectIndex = PTUnitIndex
    IF (IsNotOK) THEN
      ErrorsFound=.TRUE.
      IF (IsBlank) Alphas(1)='xxxxx'
    ENDIF
    PTUnit(PTUnitNum)%Name = Alphas(1)
    PTUnit(PTUnitNum)%UnitType = CurrentModuleObject
    PTUnit(PTUnitNum)%UnitType_Num = PTWSHPUnit
    PTUnit(PTUnitNum)%ZoneEquipType = PkgTermHPWaterToAir_Num
    IF (lAlphaBlanks(2)) THEN
      PTUnit(PTUnitNum)%SchedPtr     = ScheduleAlwaysOn
    ELSE
      PTUnit(PTUnitNum)%SchedPtr     = GetScheduleIndex(Alphas(2))
      IF (PTUnit(PTUnitNum)%SchedPtr == 0) THEN
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(PTUnit(PTUnitNum)%Name)//'" invalid data.')
        CALL ShowContinueError('invalid-not found '//TRIM(cAlphaFields(2))//'="'//TRIM(Alphas(2))//'".')
        ErrorsFound=.TRUE.
      ENDIF
    ENDIF


    PTUnit(PTUnitNum)%AirInNode = &
               GetOnlySingleNode(Alphas(3),ErrorsFound,TRIM(CurrentModuleObject),Alphas(1), &
                            NodeType_Air,NodeConnectionType_Inlet,1,ObjectIsParent)

    PTUnit(PTUnitNum)%AirOutNode = &
               GetOnlySingleNode(Alphas(4),ErrorsFound,TRIM(CurrentModuleObject),Alphas(1), &
                            NodeType_Air,NodeConnectionType_Outlet,1,ObjectIsParent)

    PTUnit(PTUnitNum)%OAMixType = Alphas(5)
    PTUnit(PTUnitNum)%OAMixName  = Alphas(6)

    ErrFlag = .false.
    CALL ValidateComponent(PTUnit(PTUnitNum)%OAMixType,PTUnit(PTUnitNum)%OAMixName,ErrFlag,TRIM(CurrentModuleObject))
    IF (ErrFlag) THEN
       CALL ShowContinueError('specified in '//TRIM(CurrentModuleObject)//' = "'//TRIM(PTUnit(PTUnitNum)%Name)//'".')
       ErrorsFound = .TRUE.
    ELSE
      ! OANodeNums = outside air mixer node numbers, OANodeNums(4) = outside air mixer mixed air node
      OANodeNums = GetOAMixerNodeNumbers(PTUnit(PTUnitNum)%OAMixName, ErrFlag)
      IF(ErrFlag) THEN
        CALL ShowContinueError('that was specified in '//TRIM(CurrentModuleObject)//' = '//TRIM(PTUnit(PTUnitNum)%Name))
        CALL ShowContinueError('..OutdoorAir:Mixer is required. Enter an OutdoorAir:Mixer object with this name.')
        ErrorsFound=.true.
      ELSE
           !  Set connection type to 'Inlet', because this is not necessarily directly come from
           !  outside air.  Outside Air Inlet Node List will set the connection to outside air
        PTUnit(PTUnitNum)%OutsideAirNode = OANodeNums(1)
        PTUnit(PTUnitNum)%AirReliefNode = OANodeNums(2)
      ENDIF
    END IF

    !Get fan data
    PTUnit(PTUnitNum)%FanType = Alphas(7)
    PTUnit(PTUnitNum)%FanName = Alphas(8)
    ErrFlag=.FALSE.
    CALL GetFanType(TRIM(PTUnit(PTUnitNum)%FanName), PTUnit(PTUnitNum)%FanType_Num, ErrFlag, CurrentModuleObject,Alphas(1))
    FanVolFlow = 0.d0
    IF (ErrFlag) THEN
      CALL ShowContinueError('...specified in '//TRIM(CurrentModuleObject)//'="'//TRIM(PTUnit(PTUnitNum)%Name)//'".')
      ErrorsFound=.TRUE.
    END IF

    IF (PTUnit(PTUnitNum)%FanType_Num == FanType_SimpleOnOff)THEN
      CALL ValidateComponent(PTUnit(PTUnitNum)%FanType,PTUnit(PTUnitNum)%FanName,IsNotOK, TRIM(CurrentModuleObject))
      IF (IsNotOK) THEN
        CALL ShowContinueError('...specified in '//TRIM(CurrentModuleObject)//'="'//TRIM(Alphas(1))//'".')
        ErrorsFound=.TRUE.
      ELSE
        ErrFlag=.FALSE.
        CALL GetFanIndex(PTUnit(PTUnitNum)%FanName, PTUnit(PTUnitNum)%FanIndex, ErrFlag)
        IF (ErrFlag) THEN
          CALL ShowContinueError('...specified in '//TRIM(CurrentModuleObject)//'="'//TRIM(Alphas(1))//'".')
          ErrorsFound=.TRUE.
        ENDIF
        ErrFlag=.FALSE.
        FanInletNodeNum  = GetFanInletNode(PTUnit(PTUnitNum)%FanType,PTUnit(PTUnitNum)%FanName,ErrFlag)
        IF (ErrFlag) THEN
          CALL ShowContinueError('...specified in '//TRIM(CurrentModuleObject)//'="'//TRIM(Alphas(1))//'".')
          ErrorsFound=.TRUE.
        ENDIF
        ErrFlag=.FALSE.
        FanOutletNodeNum = GetFanOutletNode(PTUnit(PTUnitNum)%FanType,PTUnit(PTUnitNum)%FanName,ErrFlag)
        IF (ErrFlag) THEN
          CALL ShowContinueError('...specified in '//TRIM(CurrentModuleObject)//'="'//TRIM(Alphas(1))//'".')
          ErrorsFound=.TRUE.
        ELSE
          CALL GetFanVolFlow(PTUnit(PTUnitNum)%FanIndex,FanVolFlow)
          PTUnit(PTUnitNum)%ActualFanVolFlowRate = FanVolFlow
        ENDIF
        ErrFlag=.FALSE.
        PTUnit(PTUnitNum)%FanAvailSchedPtr = GetFanAvailSchPtr(PTUnit(PTUnitNum)%FanType,PTUnit(PTUnitNum)%FanName,ErrFlag)
        IF (ErrFlag) THEN
          CALL ShowContinueError('...specified in '//TRIM(CurrentModuleObject)//'="'//TRIM(Alphas(1))//'".')
          ErrorsFound=.TRUE.
        ENDIF
      ENDIF
    ELSE
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(Alphas(1))//'"')
      CALL ShowContinueError('Illegal '//TRIM(cAlphaFields(7))//'="'//TRIM(Alphas(7))//'".')
      ErrorsFound=.TRUE.
    END IF


    !Get heating coil type and name data
    IF (Alphas(9) == 'COIL:HEATING:WATERTOAIRHEATPUMP:EQUATIONFIT' )THEN
      PTUnit(PTUnitNum)%DXHeatCoilType = Alphas(9)
      PTUnit(PTUnitNum)%DXHeatCoilType_Num = Coil_HeatingWaterToAirHPSimple
      PTUnit(PTUnitNum)%DXHeatCoilName = Alphas(10)
      CALL ValidateComponent(PTUnit(PTUnitNum)%DXHeatCoilType,PTUnit(PTUnitNum)%DXHeatCoilName,IsNotOK,  &
                             TRIM(CurrentModuleObject))
      IF (IsNotOK) THEN
        CALL ShowContinueError('...specified in '//TRIM(CurrentModuleObject)//'="'//TRIM(Alphas(1))//'".')
        ErrorsFound=.TRUE.
      ELSE
        ErrFlag = .FALSE.
        PTUnit(PTUnitNum)%DXHeatCoilIndex = GetWtoAHPSimpleCoilIndex(PTUnit(PTUnitNum)%DXHeatCoilType, &
                                                                     PTUnit(PTUnitNum)%DXHeatCoilName,ErrFlag)
        IF(ErrFlag)THEN
          CALL ShowContinueError('...specified in '//TRIM(CurrentModuleObject)//'="'//TRIM(Alphas(1))//'".')
          ErrorsFound=.TRUE.
        END IF
        HeatCoilInletNodeNum=GetWtoAHPSimpleCoilInletNode(PTUnit(PTUnitNum)%DXHeatCoilType, &
                                                          PTUnit(PTUnitNum)%DXHeatCoilName,ErrFlag)
        HeatCoilOutletNodeNum=GetWtoAHPSimpleCoilOutletNode(PTUnit(PTUnitNum)%DXHeatCoilType, &
                                                            PTUnit(PTUnitNum)%DXHeatCoilName,ErrFlag)
      ENDIF
    ELSE IF (Alphas(9) == 'COIL:HEATING:WATERTOAIRHEATPUMP:VARIABLESPEEDEQUATIONFIT' )THEN
      PTUnit(PTUnitNum)%DXHeatCoilType = Alphas(9)
      PTUnit(PTUnitNum)%DXHeatCoilType_Num = Coil_HeatingWaterToAirHPVSEquationFit
      PTUnit(PTUnitNum)%DXHeatCoilName = Alphas(10)
      CALL ValidateComponent(PTUnit(PTUnitNum)%DXHeatCoilType,PTUnit(PTUnitNum)%DXHeatCoilName,IsNotOK,  &
                             TRIM(CurrentModuleObject))
      IF (IsNotOK) THEN
        CALL ShowContinueError('...specified in '//TRIM(CurrentModuleObject)//'="'//TRIM(Alphas(1))//'".')
        ErrorsFound=.TRUE.
      ELSE
        ErrFlag = .FALSE.
        PTUnit(PTUnitNum)%DXHeatCoilIndex = GetCoilIndexVariableSpeed(PTUnit(PTUnitNum)%DXHeatCoilType, &
                                                                     PTUnit(PTUnitNum)%DXHeatCoilName,ErrFlag)
        IF(ErrFlag)THEN
          CALL ShowContinueError('...specified in '//TRIM(CurrentModuleObject)//'="'//TRIM(Alphas(1))//'".')
          ErrorsFound=.TRUE.
        END IF
        HeatCoilInletNodeNum=GetCoilInletNodeVariableSpeed(PTUnit(PTUnitNum)%DXHeatCoilType, &
                                                          PTUnit(PTUnitNum)%DXHeatCoilName,ErrFlag)
        HeatCoilOutletNodeNum=GetCoilOutletNodeVariableSpeed(PTUnit(PTUnitNum)%DXHeatCoilType, &
                                                            PTUnit(PTUnitNum)%DXHeatCoilName,ErrFlag)
      ENDIF
    ELSE
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(Alphas(1))//'"')
      CALL ShowContinueError('Illegal '//TRIM(cAlphaFields(9))//' = '//TRIM(Alphas(9)))
      ErrorsFound=.TRUE.
    END IF

    ! Get Cooling Coil Information if available
    IF (Alphas(11) == 'COIL:COOLING:WATERTOAIRHEATPUMP:EQUATIONFIT' )THEN
      PTUnit(PTUnitNum)%DXCoolCoilType = Alphas(11)
      PTUnit(PTUnitNum)%DXCoolCoilType_Num = Coil_CoolingWaterToAirHPSimple
      PTUnit(PTUnitNum)%DXCoolCoilName = Alphas(12)
      CALL ValidateComponent(PTUnit(PTUnitNum)%DXCoolCoilType,PTUnit(PTUnitNum)%DXCoolCoilName,IsNotOK,  &
                             TRIM(CurrentModuleObject))
      IF (IsNotOK) THEN
        CALL ShowContinueError('...specified in '//TRIM(CurrentModuleObject)//'="'//TRIM(Alphas(1))//'".')
        ErrorsFound=.TRUE.
      ELSE
        ErrFlag = .FALSE.
        PTUnit(PTUnitNum)%DXCoolCoilIndexNum = GetWtoAHPSimpleCoilIndex(PTUnit(PTUnitNum)%DXCoolCoilType, &
                                                                        PTUnit(PTUnitNum)%DXCoolCoilName,ErrFlag)
        IF(ErrFlag)THEN
          CALL ShowContinueError('...specified in '//TRIM(CurrentModuleObject)//'="'//TRIM(Alphas(1))//'".')
          ErrorsFound=.TRUE.
        END IF
        CoolCoilInletNodeNum=GetWtoAHPSimpleCoilInletNode(PTUnit(PTUnitNum)%DXCoolCoilType,  &
                                                          PTUnit(PTUnitNum)%DXCoolCoilName,ErrFlag)
        CoolCoilOutletNodeNum=GetWtoAHPSimpleCoilOutletNode(PTUnit(PTUnitNum)%DXCoolCoilType,  &
                                                            PTUnit(PTUnitNum)%DXCoolCoilName,ErrFlag)
      ENDIF
    ELSE IF (Alphas(11) == 'COIL:COOLING:WATERTOAIRHEATPUMP:VARIABLESPEEDEQUATIONFIT' )THEN
      PTUnit(PTUnitNum)%DXCoolCoilType = Alphas(11)
      PTUnit(PTUnitNum)%DXCoolCoilType_Num = Coil_CoolingWaterToAirHPVSEquationFit
      PTUnit(PTUnitNum)%DXCoolCoilName = Alphas(12)
      CALL ValidateComponent(PTUnit(PTUnitNum)%DXCoolCoilType,PTUnit(PTUnitNum)%DXCoolCoilName,IsNotOK,  &
                             TRIM(CurrentModuleObject))
      IF (IsNotOK) THEN
        CALL ShowContinueError('...specified in '//TRIM(CurrentModuleObject)//'="'//TRIM(Alphas(1))//'".')
        ErrorsFound=.TRUE.
      ELSE
        ErrFlag = .FALSE.
        PTUnit(PTUnitNum)%DXCoolCoilIndexNum = GetCoilIndexVariableSpeed(PTUnit(PTUnitNum)%DXCoolCoilType, &
                                                                        PTUnit(PTUnitNum)%DXCoolCoilName,ErrFlag)
        IF(ErrFlag)THEN
          CALL ShowContinueError('...specified in '//TRIM(CurrentModuleObject)//'="'//TRIM(Alphas(1))//'".')
          ErrorsFound=.TRUE.
        END IF
        CoolCoilInletNodeNum=GetCoilInletNodeVariableSpeed(PTUnit(PTUnitNum)%DXCoolCoilType,  &
                                                          PTUnit(PTUnitNum)%DXCoolCoilName,ErrFlag)
        CoolCoilOutletNodeNum=GetCoilOutletNodeVariableSpeed(PTUnit(PTUnitNum)%DXCoolCoilType,  &
                                                            PTUnit(PTUnitNum)%DXCoolCoilName,ErrFlag)
      ENDIF
    ELSE
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(Alphas(1))//'"')
      CALL ShowContinueError('Illegal '//TRIM(cAlphaFields(11))//'="'//TRIM(Alphas(11))//'".')
      ErrorsFound=.TRUE.
    END IF

    IF (NumAlphas >= 19) THEN
      ! get water flow mode info before calling SetSimpleWSHPData
      IF (SameString(Alphas(19),'Constant')) PTUnit(PTUnitNum)%WaterCyclingMode = WaterConstant
      IF (SameString(Alphas(19),'Cycling')) PTUnit(PTUnitNum)%WaterCyclingMode = WaterCycling
      IF (SameString(Alphas(19),'ConstantOnDemand'))   PTUnit(PTUnitNum)%WaterCyclingMode = WaterConstantOnDemand
         !default to draw through if not specified in input
      IF (lAlphaBlanks(19))           PTUnit(PTUnitNum)%WaterCyclingMode = WaterCycling
      IF (PTUnit(PTUnitNum)%WaterCyclingMode .EQ. 0) THEN
        CALL ShowSevereError(TRIM(CurrentModuleObject)//' illegal '//TRIM(cAlphaFields(19))//' = '//TRIM(Alphas(19)))
        CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(PTUnit(PTUnitNum)%Name))
        ErrorsFound = .TRUE.
      END IF
    ELSE
      PTUnit(PTUnitNum)%WaterCyclingMode = WaterCycling
    ENDIF

      ! end get water flow mode info
    IF (Alphas(9) == 'COIL:HEATING:WATERTOAIRHEATPUMP:EQUATIONFIT' .AND.   &
        Alphas(11) == 'COIL:COOLING:WATERTOAIRHEATPUMP:EQUATIONFIT') THEN
      IF(PTUnit(PTUnitNum)%DXHeatCoilIndex .GT. 0 .AND. PTUnit(PTUnitNum)%DXCoolCoilIndexNum .GT. 0)THEN
         CALL SetSimpleWSHPData(PTUnit(PTUnitNum)%DXCoolCoilIndexNum,ErrorsFound,PTUnit(PTUnitNum)%WaterCyclingMode,  &
                                CompanionHeatingCoilNum=PTUnit(PTUnitNum)%DXHeatCoilIndex)
!         CALL SetSimpleWSHPData(PTUnit(PTUnitNum)%WaterCyclingMode, PTUnit(PTUnitNum)%DXHeatCoilIndex,ErrorsFound, &
!                                CompanionCoolingCoilNum=PTUnit(PTUnitNum)%DXCoolCoilIndexNum)
      END IF
    ELSE IF (Alphas(9) == 'COIL:HEATING:WATERTOAIRHEATPUMP:VARIABLESPEEDEQUATIONFIT' .AND.   &
        Alphas(11) == 'COIL:COOLING:WATERTOAIRHEATPUMP:VARIABLESPEEDEQUATIONFIT') THEN
      IF(PTUnit(PTUnitNum)%DXHeatCoilIndex .GT. 0 .AND. PTUnit(PTUnitNum)%DXCoolCoilIndexNum .GT. 0)THEN
         CALL SetVarSpeedCoilData(PTUnit(PTUnitNum)%DXCoolCoilIndexNum,ErrorsFound, &
                                CompanionHeatingCoilNum=PTUnit(PTUnitNum)%DXHeatCoilIndex)
      END IF
    ELSE
      CALL ShowContinueError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(Alphas(1))//'"')
      CALL ShowContinueError('Cooling coil and heating coil should use the equation fit model and be of same general type')
      ErrorsFound = .TRUE.
    END IF

    ! Get supplemental heating coil information

    SuppHeatCoilType = Alphas(13)
    SuppHeatCoilName = Alphas(14)
    PTUnit(PTUnitNum)%SuppHeatCoilName     = SuppHeatCoilName
    IF (SameString(Alphas(13),'Coil:Heating:Gas')      .OR. &
        SameString(Alphas(13),'Coil:Heating:Electric') .OR. &
        SameString(Alphas(13),'Coil:Heating:Water')    .OR. &
        SameString(Alphas(13),'Coil:Heating:Steam')) THEN
        PTUnit(PTUnitNum)%SuppHeatCoilType=SuppHeatCoilType
       IF (SameString(Alphas(13),'Coil:Heating:Gas') .OR. SameString(Alphas(13),'Coil:Heating:Electric')) THEN
         IF (SameString(Alphas(13),'Coil:Heating:Gas')) THEN
            PTUnit(PTUnitNum)%SuppHeatCoilType_Num = Coil_HeatingGas
         ELSEIF (SameString(Alphas(13),'Coil:Heating:Electric')) THEN
            PTUnit(PTUnitNum)%SuppHeatCoilType_Num = Coil_HeatingElectric
         ENDIF
         ErrFlag = .FALSE.
         CALL ValidateComponent(SuppHeatCoilType,SuppHeatCoilName,ErrFlag,  &
                                TRIM(CurrentModuleObject))
         IF (ErrFlag) THEN
           CALL ShowContinueError('...specified in '//TRIM(CurrentModuleObject)//'="'//TRIM(PTUnit(PTUnitNum)%Name)//'".')
           ErrorsFound=.TRUE.
         ELSE
           CALL GetHeatingCoilIndex(SuppHeatCoilName,PTUnit(PTUnitNum)%SuppHeatCoilIndex,ErrFlag)
           ! Get the Supplemental Heating Coil Node Numbers
           SuppHeatInletNodeNum = &
               GetHeatingCoilInletNode(SuppHeatCoilType,SuppHeatCoilName,ErrFlag)
           SuppHeatOutletNodeNum = &
               GetHeatingCoilOutletNode(SuppHeatCoilType,SuppHeatCoilName,ErrFlag)
           IF (ErrFlag) THEN
             CALL ShowContinueError('...specified in '//TRIM(CurrentModuleObject)//'="'//TRIM(PTUnit(PTUnitNum)%Name)//'".')
             ErrorsFound=.TRUE.
           ENDIF
        ENDIF
       ELSEIF (SameString(Alphas(13),'Coil:Heating:Water')) THEN
         PTUnit(PTUnitNum)%SuppHeatCoilType_Num = Coil_HeatingWater
         ErrFlag = .FALSE.
         SuppHeatHWInletNodeNum = GetCoilWaterInletNode(SuppHeatCoilType,PTUnit(PTUnitNum)%SuppHeatCoilName,ErrFlag)
         PTUnit(PTUnitNum)%HotWaterControlNode = SuppHeatHWInletNodeNum
         IF(ErrFlag)THEN
           CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(PTUnit(PTUnitNum)%Name))
           ErrorsFound = .TRUE.
         END IF
         PTUnit(PTUnitNum)%MaxSuppCoilFluidFlow = GetCoilMaxWaterFlowRate(SuppHeatCoilType,  &
                                                  PTUnit(PTUnitNum)%SuppHeatCoilName,ErrFlag)
         IF(PTUnit(PTUnitNum)%MaxSuppCoilFluidFlow .GT. 0.0d0)THEN
            PTUnit(PTUnitNum)%MaxSuppCoilFluidFlow = GetCoilMaxWaterFlowRate(SuppHeatCoilType,  &
                                                     PTUnit(PTUnitNum)%SuppHeatCoilName,ErrFlag)
         END IF
         ErrFlag = .FALSE.
         SuppHeatInletNodeNum =  GetWaterCoilInletNode('Coil:Heating:Water',PTUnit(PTUnitNum)%SuppHeatCoilName,ErrFlag)
         PTUnit(PTUnitNum)%SupCoilAirInletNode = SuppHeatInletNodeNum
         SuppHeatOutletNodeNum = GetWaterCoilOutletNode('Coil:Heating:Water', &
                                                        PTUnit(PTUnitNum)%SuppHeatCoilName,ErrFlag)
         IF(ErrFlag)THEN
           CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(PTUnit(PTUnitNum)%Name))
           ErrorsFound = .TRUE.
         END IF

       ELSEIF (SameString(Alphas(13),'Coil:Heating:Steam')) THEN
         PTUnit(PTUnitNum)%SuppHeatCoilType_Num = Coil_HeatingSteam
         ErrFlag = .FALSE.
         PTUnit(PTUnitNum)%SuppHeatCoilIndex = GetSTeamCoilIndex(SuppHeatCoilType,PTUnit(PTUnitNum)%SuppHeatCoilName,ErrFlag)
         IF (PTUnit(PTUnitNum)%SuppHeatCoilIndex .EQ. 0) THEN
             CALL ShowSevereError(TRIM(CurrentModuleObject)//' illegal '//TRIM(cAlphaFields(14))//' = ' &
                           //TRIM(PTUnit(PTUnitNum)%SuppHeatCoilName))
             CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(PTUnit(PTUnitNum)%Name))
             ErrorsFound = .TRUE.
         END IF
         ErrFlag = .FALSE.
         SuppHeatHWInletNodeNum = GetSteamCoilSteamInletNode(SuppHeatCoilType,PTUnit(PTUnitNum)%SuppHeatCoilName,ErrFlag)
         PTUnit(PTUnitNum)%HWCoilSteamInletNode = SuppHeatHWInletNodeNum
         IF(ErrFlag)THEN
           CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(PTUnit(PTUnitNum)%Name))
           ErrorsFound = .TRUE.
         END IF
         PTUnit(PTUnitNum)%MaxSuppCoilFluidFlow = GetCoilMaxSteamFlowRate(PTUnit(PTUnitNum)%SuppHeatCoilIndex,ErrFlag)
         IF(PTUnit(PTUnitNum)%MaxSuppCoilFluidFlow .GT. 0.0d0)THEN
            SteamIndex = 0      ! Function GetSatDensityRefrig will look up steam index if 0 is passed
            SteamDensity=GetSatDensityRefrig("STEAM",TempSteamIn,1.0d0,SteamIndex,'GetPackagedTerminalHeatPumpInput')
            PTUnit(PTUnitNum)%MaxSuppCoilFluidFlow = &
                              GetCoilMaxSteamFlowRate(PTUnit(PTUnitNum)%SuppHeatCoilIndex,ErrFlag) * SteamDensity
         END IF
         ErrFlag = .FALSE.
         SuppHeatInletNodeNum = &
            GetSteamCoilAirInletNode(PTUnit(PTUnitNum)%SuppHeatCoilIndex,PTUnit(PTUnitNum)%SuppHeatCoilName,ErrFlag)
         PTUnit(PTUnitNum)%SupCoilAirInletNode = SuppHeatInletNodeNum
         SuppHeatOutletNodeNum = GetSteamCoilAirOutletNode(SuppHeatCoilType,PTUnit(PTUnitNum)%SuppHeatCoilName,ErrFlag)
         IF(ErrFlag)THEN
           CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(PTUnit(PTUnitNum)%Name))
           ErrorsFound = .TRUE.
         END IF
       END IF
    ELSE
      CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
      CALL ShowContinueError('Illegal '//TRIM(cAlphaFields(13))//' = '//TRIM(Alphas(13)))
      ErrorsFound=.TRUE.
    END IF

    IF (lAlphaBlanks(15)) THEN
      PTUnit(PTUnitNum)%CondenserNodeNum = 0
    ELSE
      PTUnit(PTUnitNum)%CondenserNodeNum = &
              GetOnlySingleNode(Alphas(15),ErrorsFound,TRIM(CurrentModuleObject),Alphas(1), &
                                NodeType_Air,NodeConnectionType_OutsideAirReference,1,ObjectIsNotParent)
     ! need better verification.
     IF (.not. CheckOutAirNodeNumber(PTUnit(PTUnitNum)%CondenserNodeNum)) THEN
       CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(Alphas(1))//'"')
       CALL ShowContinueError(' Node name of outdoor dry-bulb temperature sensor not valid outdoor air node="'//  &
          TRIM(Alphas(15))//'"')
       CALL ShowContinueError('...does not appear in an OutdoorAir:NodeList or as an OutdoorAir:Node.')
       ErrorsFound=.TRUE.
     END IF
    ENDIF

    IF (SameString(Alphas(16),'BlowThrough'))  PTUnit(PTUnitNum)%FanPlace = BlowThru
    IF (SameString(Alphas(16),'DrawThrough'))  PTUnit(PTUnitNum)%FanPlace = DrawThru
    IF (PTUnit(PTUnitNum)%FanPlace .EQ.0) THEN
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(Alphas(1))//'"')
      CALL ShowContinueError('Illegal '//TRIM(cAlphaFields(16))//'="'//TRIM(Alphas(16))//'".')
      ErrorsFound = .TRUE.
    END IF

    PTUnit(PTUnitNum)%FanSchedPtr     = GetScheduleIndex(Alphas(17))
    IF (.NOT. lAlphaBlanks(17) .AND. PTUnit(PTUnitNum)%FanSchedPtr == 0) THEN
      CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
      CALL ShowContinueError('Illegal '//TRIM(cAlphaFields(17))//' = '//TRIM(Alphas(17)))
      ErrorsFound=.TRUE.
    ELSEIF (lAlphaBlanks(17)) THEN
      PTUnit(PTUnitNum)%OpMode = CycFanCycCoil
    ENDIF

    IF (.NOT. lAlphaBlanks(18)) THEN
      PTUnit(PTUnitNum)%AvailManagerListName = Alphas(18)
      ZoneComp(PkgTermHPWaterToAir_Num)%ZoneCompAvailMgrs(PTUnitNum)%AvailManagerListName  = Alphas(18)
    ENDIF

    ! Check component placement
    IF (PTUnit(PTUnitNum)%FanPlace == BlowThru) THEN
      ! check that PTUnit inlet node is a zone exhaust node.
      ZoneNodeNotFound = .TRUE.
      DO CtrlZone = 1,NumOfZones
        IF (.not. ZoneEquipConfig(CtrlZone)%IsControlled) CYCLE
        DO NodeNum = 1,ZoneEquipConfig(CtrlZone)%NumExhaustNodes
          IF (PTUnit(PTUnitNum)%AirInNode .EQ. ZoneEquipConfig(CtrlZone)%ExhaustNode(NodeNum)) THEN
            ZoneNodeNotFound = .FALSE.
            EXIT
          END IF
        END DO
      END DO
      IF(ZoneNodeNotFound)THEN
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(PTUnit(PTUnitNum)%Name)//'"')
        CALL ShowContinueError('..Heat Pumps air inlet node name must be the same as a zone exhaust node name.')
        CALL ShowContinueError('..Zone exhaust node name is specified in ZoneHVAC:EquipmentConnections object.')
        CALL ShowContinueError('..Heat pumps inlet node name = '//TRIM(NodeID(PTUnit(PTUnitNum)%AirInNode)))
        ErrorsFound=.TRUE.
      END IF
      IF(OANodeNums(4) == 0)THEN
       ! Fan inlet node name must be the same as the heat pump's inlet air node name
        IF (PTUnit(PTUnitNum)%AirInNode /= FanInletNodeNum) THEN
          CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(PTUnit(PTUnitNum)%Name)//'"')
          CALL ShowContinueError('..Fan inlet node name must be the same as the heat pumps inlet air node name')
          CALL ShowContinueError('..when blow through '// &
                                TRIM(cAlphaFields(16))//' is specified and an outdoor air mixer is not used.')
          CALL ShowContinueError('..Fan inlet node name           = '//TRIM(NodeID(FanInletNodeNum)))
          CALL ShowContinueError('..Heat pump air inlet node name = '//TRIM(NodeID(PTUnit(PTUnitNum)%AirInNode)))
          ErrorsFound=.TRUE.
        END IF
      ELSE
        ! Fan inlet node name must be the same as the heat pump's OA mixer mixed air node name
        IF (OANodeNums(4) /= FanInletNodeNum) THEN
          CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(PTUnit(PTUnitNum)%Name)//'"')
          CALL ShowContinueError('..Fan inlet node name must be the same as the heat pumps OutdoorAir:Mixer mixed air node name')
          CALL ShowContinueError('..when blow through '// &
                                TRIM(cAlphaFields(16))//' is specified.')
          CALL ShowContinueError('..Fan inlet node name                   = '//TRIM(NodeID(FanInletNodeNum)))
          CALL ShowContinueError('..OutdoorAir:Mixer mixed air node name = '//TRIM(NodeID(OANodeNums(4))))
          ErrorsFound=.TRUE.
        END IF
        ! check OA Mixer return node
        IF(PTUnit(PTUnitNum)%AirInNode /= OANodeNums(3))THEN
          CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(PTUnit(PTUnitNum)%Name)//'"')
          CALL ShowContinueError('..Heat Pump air inlet node name must be the same as the OutdoorAir:Mixer return air node name.')
          CALL ShowContinueError('..Heat Pump air inlet node name         = '//TRIM(NodeID(PTUnit(PTUnitNum)%AirInNode)))
          CALL ShowContinueError('..OutdoorAir:Mixer return air node name = '//TRIM(NodeID(OANodeNums(3))))
          ErrorsFound=.TRUE.
        END IF
      END IF
      IF(CoolCoilInletNodeNum /= FanOutletNodeNum)THEN
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(PTUnit(PTUnitNum)%Name)//'"')
        CALL ShowContinueError('..Fan outlet node name must be the same as the cooling coil inlet node name')
        CALL ShowContinueError('..when blow through '//TRIM(cAlphaFields(16))//' is specified.')
        CALL ShowContinueError('..Fan outlet node name         = '//TRIM(NodeID(FanOutletNodeNum)))
        CALL ShowContinueError('..Cooling coil inlet node name = '//TRIM(NodeID(CoolCoilInletNodeNum)))
        ErrorsFound=.TRUE.
      END IF
      IF(CoolCoilOutletNodeNum /= HeatCoilInletNodeNum)THEN
        CALL ShowSevereError(TRIM(CurrentModuleObject)//' "'//TRIM(PTUnit(PTUnitNum)%Name)//&
                           '" Cooling coil outlet node name must be the same as the heating coil inlet node name.')
        CALL ShowContinueError('..Cooling coil outlet node name = '//TRIM(NodeID(CoolCoilOutletNodeNum)))
        CALL ShowContinueError('..Heating coil inlet node name  = '//TRIM(NodeID(HeatCoilInletNodeNum)))
        ErrorsFound=.TRUE.
      END IF
      IF(HeatCoilOutletNodeNum /= SuppHeatInletNodeNum)THEN
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(PTUnit(PTUnitNum)%Name)//'"')
        CALL ShowContinueError('..Heating coil outlet node name must be the same as the supplemental heating coil inlet node name')
        CALL ShowContinueError('..when blow through '//TRIM(cAlphaFields(16))//' is specified.')
        CALL ShowContinueError('..Heating coil outlet node name              = '//TRIM(NodeID(HeatCoilOutletNodeNum)))
        CALL ShowContinueError('..Supplemental heating coil inlet node name  = '//TRIM(NodeID(SuppHeatInletNodeNum)))
        ErrorsFound=.TRUE.
      END IF
      IF(SuppHeatOutletNodeNum /= PTUnit(PTUnitNum)%AirOutNode)THEN
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(PTUnit(PTUnitNum)%Name)//'"')
        CALL ShowContinueError('..Supplemental heating coil outlet node name must be the same as the heat pumps outlet node name.')
        CALL ShowContinueError('..Supplemental heating coil outlet node name = '//TRIM(NodeID(SuppHeatOutletNodeNum)))
        CALL ShowContinueError('..Heat pumps outlet node name                   = '//TRIM(NodeID(PTUnit(PTUnitNum)%AirOutNode)))
        ErrorsFound=.TRUE.
      END IF
      ! check that PTUnit outlet node is a zone inlet node.
      ZoneNodeNotFound = .TRUE.
      DO CtrlZone = 1,NumOfZones
        IF (.not. ZoneEquipConfig(CtrlZone)%IsControlled) CYCLE
        DO NodeNum = 1,ZoneEquipConfig(CtrlZone)%NumInletNodes
          IF (PTUnit(PTUnitNum)%AirOutNode .EQ. ZoneEquipConfig(CtrlZone)%InletNode(NodeNum)) THEN
            ZoneNodeNotFound = .FALSE.
            EXIT
          END IF
        END DO
      END DO
      IF(ZoneNodeNotFound)THEN
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(PTUnit(PTUnitNum)%Name)//'"')
        CALL ShowContinueError('..Heat Pumps air outlet node name must be the same as a zone inlet node name.')
        CALL ShowContinueError('..Zone inlet node name is specified in ZoneHVAC:EquipmentConnections object.')
        CALL ShowContinueError('..Heat pumps outlet node name = '//TRIM(NodeID(PTUnit(PTUnitNum)%AirOutNode)))
        ErrorsFound=.TRUE.
      END IF
    ELSE ! draw through fan from IF (PTUnit(PTUnitNum)%FanPlace == BlowThru) THEN
      ! check that PTUnit inlet node is a zone exhaust node.
      ZoneNodeNotFound = .TRUE.
      DO CtrlZone = 1,NumOfZones
        IF (.not. ZoneEquipConfig(CtrlZone)%IsControlled) CYCLE
        DO NodeNum = 1,ZoneEquipConfig(CtrlZone)%NumExhaustNodes
          IF (PTUnit(PTUnitNum)%AirInNode .EQ. ZoneEquipConfig(CtrlZone)%ExhaustNode(NodeNum)) THEN
            ZoneNodeNotFound = .FALSE.
            EXIT
          END IF
        END DO
      END DO
      IF(ZoneNodeNotFound)THEN
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(PTUnit(PTUnitNum)%Name)//'"')
        CALL ShowContinueError('..Heat Pumps air inlet node name must be the same as a zone exhaust node name.')
        CALL ShowContinueError('..Zone exhaust node name is specified in ZoneHVAC:EquipmentConnections object.')
        CALL ShowContinueError('..Heat pumps inlet node name = '//TRIM(NodeID(PTUnit(PTUnitNum)%AirInNode)))
        ErrorsFound=.TRUE.
      END IF
      IF(OANodeNums(4) == 0)THEN
        ! Cooling coil inlet node name must be the same as heat pump's air inlet node name
        IF(CoolCoilInletNodeNum /= PTUnit(PTUnitNum)%AirInNode)THEN
          CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(PTUnit(PTUnitNum)%Name)//'"')
          CALL ShowContinueError('..Heat pump air inlet node name must be the same as the cooling coil inlet node name')
          CALL ShowContinueError('..when draw through '//TRIM(cAlphaFields(16))// &
                                 ' is specified and an outdoor air mixer is not used.')
          CALL ShowContinueError('..Heat pump air inlet node name = '//TRIM(NodeID(PTUnit(PTUnitNum)%AirInNode)))
          CALL ShowContinueError('..Cooling coil inlet node name  = '//TRIM(NodeID(CoolCoilInletNodeNum)))
          ErrorsFound=.TRUE.
        END IF
      ELSE
        ! Cooling coil inlet node name must be the same as the OA mixers mixed air node name
        IF(CoolCoilInletNodeNum /= OANodeNums(4))THEN
          CALL ShowSevereError(TRIM(CurrentModuleObject)//' "'//TRIM(PTUnit(PTUnitNum)%Name)//&
                           '" OutdoorAir:Mixer mixed air node name must be the same as the cooling coil')
          CALL ShowContinueError(' inlet node name when draw through '//TRIM(cAlphaFields(16))//' is specified.')
          CALL ShowContinueError('..OutdoorAir:Mixer mixed air name = '//TRIM(NodeID(OANodeNums(4))))
          CALL ShowContinueError('..Cooling coil inlet node name    = '//TRIM(NodeID(CoolCoilInletNodeNum)))
          ErrorsFound=.TRUE.
        END IF
      ! check OA Mixer return node
        IF(PTUnit(PTUnitNum)%AirInNode /= OANodeNums(3))THEN
          CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(PTUnit(PTUnitNum)%Name)//'"')
          CALL ShowContinueError('..Heat Pump air inlet node name must be the same as the OutdoorAir:Mixer return air node name.')
          CALL ShowContinueError('..Heat Pump air inlet node name         = '//TRIM(NodeID(PTUnit(PTUnitNum)%AirInNode)))
          CALL ShowContinueError('..OutdoorAir:Mixer return air node name = '//TRIM(NodeID(OANodeNums(3))))
          ErrorsFound=.TRUE.
        END IF
      END IF
      IF(CoolCoilOutletNodeNum /= HeatCoilInletNodeNum)THEN
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(PTUnit(PTUnitNum)%Name)//'"')
        CALL ShowContinueError('..Cooling coil outlet node name must be the same as the heating coil inlet node name.')
        CALL ShowContinueError('..Cooling coil outlet node name = '//TRIM(NodeID(CoolCoilOutletNodeNum)))
        CALL ShowContinueError('..Heating coil inlet node name  = '//TRIM(NodeID(HeatCoilInletNodeNum)))
        ErrorsFound=.TRUE.
      END IF
      IF(HeatCoilOutletNodeNum /= FanInletNodeNum)THEN
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(PTUnit(PTUnitNum)%Name)//'"')
        CALL ShowContinueError('..Heating coil outlet node name must be the same as the fan inlet node name')
        CALL ShowContinueError('..when draw through '//TRIM(cAlphaFields(16))//' is specified.')
        CALL ShowContinueError('..Heating coil outlet node name = '//TRIM(NodeID(HeatCoilOutletNodeNum)))
        CALL ShowContinueError('..Fan inlet node name           = '//TRIM(NodeID(FanInletNodeNum)))
        ErrorsFound=.TRUE.
      END IF
      IF (SuppHeatInletNodeNum /= FanOutletNodeNum) THEN
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(PTUnit(PTUnitNum)%Name)//'"')
        CALL ShowContinueError('..Fan outlet node name must be the same as the supplemental heating coil inlet node name ')
        CALL ShowContinueError('..when draw through '//TRIM(cAlphaFields(16))//' is specified.')
        CALL ShowContinueError('..Fan outlet node = '//TRIM(NodeID(FanOutletNodeNum)))
        CALL ShowContinueError('..Supplemental heating coil inlet node = '//TRIM(NodeID(SuppHeatInletNodeNum)))
        ErrorsFound=.TRUE.
      END IF
      IF(SuppHeatOutletNodeNum /= PTUnit(PTUnitNum)%AirOutNode)THEN
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(PTUnit(PTUnitNum)%Name)//'"')
        CALL ShowContinueError('..Supplemental heating coil outlet node name must be the same as the heat pumps outlet node name.')
        CALL ShowContinueError('..Supplemental heating coil outlet node name = '//TRIM(NodeID(SuppHeatOutletNodeNum)))
        CALL ShowContinueError('..Heat pumps outlet node name                = '//TRIM(NodeID(PTUnit(PTUnitNum)%AirOutNode)))
        ErrorsFound=.TRUE.
      END IF
      ! check that PTUnit outlet node is a zone inlet node.
      ZoneNodeNotFound = .TRUE.
      DO CtrlZone = 1,NumOfZones
        IF (.not. ZoneEquipConfig(CtrlZone)%IsControlled) CYCLE
        DO NodeNum = 1,ZoneEquipConfig(CtrlZone)%NumInletNodes
          IF (PTUnit(PTUnitNum)%AirOutNode .EQ. ZoneEquipConfig(CtrlZone)%InletNode(NodeNum)) THEN
            ZoneNodeNotFound = .FALSE.
            EXIT
          END IF
        END DO
      END DO
      IF(ZoneNodeNotFound)THEN
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(PTUnit(PTUnitNum)%Name)//'"')
        CALL ShowContinueError('..Heat Pumps air outlet node name must be the same as a zone inlet node name.')
        CALL ShowContinueError('..Zone inlet node name is specified in ZoneHVAC:EquipmentConnections object.')
        CALL ShowContinueError('..Heat pumps outlet node name = '//TRIM(NodeID(PTUnit(PTUnitNum)%AirOutNode)))
        ErrorsFound=.TRUE.
      END IF
    ENDIF ! IF (PTUnit(PTUnitNum)%FanPlace == BlowThru) THEN

    CompSetFanInlet   = NodeID(FanInletNodeNum)
    CompSetFanOutlet  = NodeID(FanOutletNodeNum)
    CompSetCoolInlet  = NodeID(CoolCoilInletNodeNum)
    CompSetCoolOutlet = NodeID(CoolCoilOutletNodeNum)
    CompSetHeatInlet  = NodeID(HeatCoilInletNodeNum)
    CompSetHeatOutlet = NodeID(HeatCoilOutletNodeNum)
    CompSetSupHeatInlet  = NodeID(SuppHeatInletNodeNum)
    CompSetSupHeatOutlet = NodeID(SuppHeatOutletNodeNum)

    ! Add fan to component sets array
    CALL SetUpCompSets(PTUnit(PTUnitNum)%UnitType, PTUnit(PTUnitNum)%Name, &
                   PTUnit(PTUnitNum)%FanType,PTUnit(PTUnitNum)%FanName,CompSetFanInlet,CompSetFanOutlet)

    ! Add cooling coil to component sets array
    CALL SetUpCompSets(PTUnit(PTUnitNum)%UnitType, PTUnit(PTUnitNum)%Name, &
                   PTUnit(PTUnitNum)%DXCoolCoilType,PTUnit(PTUnitNum)%DXCoolCoilName,CompSetCoolInlet,CompSetCoolOutlet)

    ! Add heating coil to component sets array
    CALL SetUpCompSets(PTUnit(PTUnitNum)%UnitType, PTUnit(PTUnitNum)%Name, &
                   PTUnit(PTUnitNum)%DXHeatCoilType,PTUnit(PTUnitNum)%DXHeatCoilName,CompSetHeatInlet,CompSetHeatOutlet)

    ! Add supplemental heating coil to component sets array
    CALL SetUpCompSets(PTUnit(PTUnitNum)%UnitType, PTUnit(PTUnitNum)%Name, &
                   SuppHeatCoilType,SuppHeatCoilName,CompSetSupHeatInlet,CompSetSupHeatOutlet)

    IF(PTUnit(PTUnitNum)%UnitType_Num .EQ. PTWSHPUnit)THEN
      IF (PTUnit(PTUnitNum)%SuppHeatCoilType_Num == Coil_HeatingWater) THEN
        ! Add heating coil water inlet node as actuator node for coil
        TempNodeNum  = GetOnlySingleNode(NodeID(PTUnit(PTUnitNum)%HotWaterControlNode),ErrorsFound,PTUnit(PTUnitNum)%UnitType, &
                              PTUnit(PTUnitNum)%Name,NodeType_Water,NodeConnectionType_Actuator,1,ObjectIsParent)
      ELSEIF (PTUnit(PTUnitNum)%SuppHeatCoilType_Num == Coil_HeatingSteam) THEN
        ! Add heating coil steam inlet node as actualtor node for coil
        TempNodeNum  = GetOnlySingleNode(NodeID(PTUnit(PTUnitNum)%HWCoilSteamInletNode),ErrorsFound,PTUnit(PTUnitNum)%UnitType, &
                             PTUnit(PTUnitNum)%Name, NodeType_Steam,NodeConnectionType_Actuator,1,ObjectIsParent)
      END IF
    END IF
    IF(OANodeNums(1) .GT. 0)THEN
    ! Set up component set for OA mixer - use OA node and Mixed air node
      CALL SetUpCompSets(PTUnit(PTUnitNum)%UnitType, PTUnit(PTUnitNum)%Name, &
                        PTUnit(PTUnitNum)%OAMixType,PTUnit(PTUnitNum)%OAMixName,NodeID(OANodeNums(1)),NodeID(OANodeNums(4)))
    END IF

     !Set the Design Fan Volume Flow Rate
     ErrFlag=.FALSE.
     CALL GetFanVolFlow(PTUnit(PTUnitNum)%FanIndex,FanVolFlow)
     PTUnit(PTUnitNum)%ActualFanVolFlowRate = FanVolFlow
     IF (ErrFlag) THEN
       CALL ShowContinueError('...occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
       ErrorsFound=.TRUE.
     ENDIF
!     PTUnit(PTUnitNum)%ActualFanVolFlowRate = MAX(Numbers(1),Numbers(2),Numbers(3))
     IF (FanVolFlow /= AutoSize .and. PTUnit(PTUnitNum)%ActualFanVolFlowRate /= AutoSize) THEN
       IF (PTUnit(PTUnitNum)%ActualFanVolFlowRate > FanVolFlow) THEN
         CALL ShowContinueError('...occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
         CALL ShowContinueError('... has a Design Fan Flow Rate > Max Fan Volume Flow Rate, should be <=.')
         CALL ShowContinueError('... Entered value='//TRIM(RoundSigDigits(PTUnit(PTUnitNum)%ActualFanVolFlowRate,2))//  &
             '... Fan ['//TRIM(PTUnit(PTUnitNum)%FanType)//':'//TRIM(PTUnit(PTUnitNum)%FanName)//  &
             '] Max Value='//TRIM(RoundSigDigits(FanVolFlow,2)))
       ENDIF
       IF (PTUnit(PTUnitNum)%ActualFanVolFlowRate <= 0.0d0) THEN
         CALL ShowContinueError('...occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
         CALL ShowContinueError('... has a Design Fan Flow Rate <= 0.0, it must be >0.0')
         CALL ShowContinueError('... Entered value='//TRIM(RoundSigDigits(PTUnit(PTUnitNum)%ActualFanVolFlowRate,2)))
         ErrorsFound=.TRUE.
       ENDIF
     ENDIF

    PTUnit(PTUnitNum)%MaxCoolAirVolFlow       = Numbers(1)
    IF (PTUnit(PTUnitNum)%MaxCoolAirVolFlow .LE. 0 .AND. PTUnit(PTUnitNum)%MaxCoolAirVolFlow .NE. AutoSize) THEN
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(PTUnit(PTUnitNum)%Name)//'"')
      CALL ShowContinueError(' illegal value '//TRIM(cNumericFields(1))//' = ' &
                           //TRIM(TrimSigDigits(Numbers(1),7)))
      ErrorsFound = .TRUE.
    END IF

    PTUnit(PTUnitNum)%MaxHeatAirVolFlow       = Numbers(2)
    IF (PTUnit(PTUnitNum)%MaxHeatAirVolFlow .LE. 0 .AND. PTUnit(PTUnitNum)%MaxHeatAirVolFlow .NE. AutoSize) THEN
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(PTUnit(PTUnitNum)%Name)//'"')
      CALL ShowContinueError(' illegal '//TRIM(cNumericFields(2))//' = ' &
                           //TRIM(TrimSigDigits(Numbers(2),7)))
      ErrorsFound = .TRUE.
    END IF

    PTUnit(PTUnitNum)%MaxNoCoolHeatAirVolFlow = Numbers(3)
    IF (PTUnit(PTUnitNum)%MaxNoCoolHeatAirVolFlow .LT. 0 .AND. PTUnit(PTUnitNum)%MaxNoCoolHeatAirVolFlow .NE. AutoSize) THEN
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(PTUnit(PTUnitNum)%Name)//'"')
      CALL ShowContinueError(' illegal '//TRIM(cNumericFields(3))//' = ' &
                           //TRIM(TrimSigDigits(Numbers(3),7)))
      ErrorsFound = .TRUE.
    END IF

!   AirFlowControl only valid if fan opmode = ContFanCycCoil
    IF (PTUnit(PTUnitNum)%MaxNoCoolHeatAirVolFlow .EQ. 0.0d0) THEN
      PTUnit(PTUnitNum)%AirFlowControl = UseCompressorOnFlow
    ELSE
      PTUnit(PTUnitNum)%AirFlowControl = UseCompressorOffFlow
    END IF

    IF(OANodeNums(1) .GT. 0)THEN
      PTUnit(PTUnitNum)%CoolOutAirVolFlow       = Numbers(4)
      IF (PTUnit(PTUnitNum)%CoolOutAirVolFlow .LT. 0 .AND. PTUnit(PTUnitNum)%CoolOutAirVolFlow .NE. AutoSize) THEN
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(PTUnit(PTUnitNum)%Name)//'"')
        CALL ShowContinueError(' illegal '//TRIM(cNumericFields(4))//' = ' &
                           //TRIM(TrimSigDigits(Numbers(4),7)))
        ErrorsFound = .TRUE.
      END IF

!     only check that SA flow in cooling is >= OA flow in cooling when either or both are not autosized
      IF (PTUnit(PTUnitNum)%CoolOutAirVolFlow .GT. PTUnit(PTUnitNum)%MaxCoolAirVolFlow .AND. &
          PTUnit(PTUnitNum)%CoolOutAirVolFlow .NE. AutoSize .AND. PTUnit(PTUnitNum)%MaxCoolAirVolFlow .NE. AutoSize) THEN
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(PTUnit(PTUnitNum)%Name)//'"')
        CALL ShowContinueError('..'//TRIM(cNumericFields(4))//' cannot be greater than '// &
                           TRIM(cNumericFields(1)))
        CALL ShowContinueError('..'//TRIM(cNumericFields(1))//' = ' &
                           //TRIM(TrimSigDigits(Numbers(1),7)))
        CALL ShowContinueError('..'//TRIM(cNumericFields(4))//' = ' &
                           //TRIM(TrimSigDigits(Numbers(4),7)))
        ErrorsFound = .TRUE.
      END IF

      PTUnit(PTUnitNum)%HeatOutAirVolFlow       = Numbers(5)
      IF (PTUnit(PTUnitNum)%HeatOutAirVolFlow .LT. 0 .AND. PTUnit(PTUnitNum)%HeatOutAirVolFlow.NE. AutoSize) THEN
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(PTUnit(PTUnitNum)%Name)//'"')
        CALL ShowContinueError(' illegal '//TRIM(cNumericFields(5))//' = ' &
                           //TRIM(TrimSigDigits(Numbers(5),7)))
        ErrorsFound = .TRUE.
      END IF

!     only check that SA flow in heating is >= OA flow in heating when either or both are not autosized
      IF (PTUnit(PTUnitNum)%HeatOutAirVolFlow .GT. PTUnit(PTUnitNum)%MaxHeatAirVolFlow .AND. &
          PTUnit(PTUnitNum)%HeatOutAirVolFlow .NE. AutoSize .AND. PTUnit(PTUnitNum)%MaxHeatAirVolFlow .NE. AutoSize) THEN
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(PTUnit(PTUnitNum)%Name)//'"')
        CALL ShowContinueError('..'//TRIM(cNumericFields(5))//' cannot be greater than '// &
                             TRIM(cNumericFields(2)))
        CALL ShowContinueError('..'//TRIM(cNumericFields(2))//' = ' &
                           //TRIM(TrimSigDigits(Numbers(2),7)))
        CALL ShowContinueError('..'//TRIM(cNumericFields(5))//' = ' &
                           //TRIM(TrimSigDigits(Numbers(5),7)))
        ErrorsFound = .TRUE.
      END IF

      PTUnit(PTUnitNum)%NoCoolHeatOutAirVolFlow = Numbers(6)
      IF (PTUnit(PTUnitNum)%NoCoolHeatOutAirVolFlow .LT. 0 .AND. PTUnit(PTUnitNum)%NoCoolHeatOutAirVolFlow .NE. AutoSize) THEN
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(PTUnit(PTUnitNum)%Name)//'"')
        CALL ShowContinueError(' illegal '//TRIM(cNumericFields(6))//' = ' &
                           //TRIM(TrimSigDigits(Numbers(6),7)))
        ErrorsFound = .TRUE.
      END IF
    ELSE
      PTUnit(PTUnitNum)%CoolOutAirVolFlow = 0.0d0
      PTUnit(PTUnitNum)%HeatOutAirVolFlow = 0.0d0
      PTUnit(PTUnitNum)%NoCoolHeatOutAirVolFlow = 0.0d0
      IF(.NOT. lNumericBlanks(4) .OR. .NOT. lNumericBlanks(5) .OR. .NOT. lNumericBlanks(6))THEN
      ! user entered values for OA with no outdoor air mixer name specified
        PTUnit(PTUnitNum)%CoolOutAirVolFlow = 0.0d0
      END IF
    END IF

     !Set the heat pump heating coil capacity
     !  Get from coil module.
     IF (PTUnit(PTUnitNum)%DXHeatCoilType_Num  == Coil_HeatingWaterToAirHP) THEN
       ErrFlag=.FALSE.
       PTUnit(PTUnitNum)%DesignHeatingCapacity =   &
          GetWtoAHPCoilCapacity(PTUnit(PTUnitNum)%DXHeatCoilType,PTUnit(PTUnitNum)%DXHeatCoilName,ErrFlag)

       IF (ErrFlag) THEN
         CALL ShowContinueError('...occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
         ErrorsFound=.TRUE.
       ENDIF
     ELSEIF (PTUnit(PTUnitNum)%DXHeatCoilType_Num == Coil_HeatingWaterToAirHPSimple) THEN
       ErrFlag=.FALSE.
       PTUnit(PTUnitNum)%DesignHeatingCapacity =   &
             GetWtoAHPSimpleCoilCapacity(PTUnit(PTUnitNum)%DXHeatCoilType,PTUnit(PTUnitNum)%DXHeatCoilName,ErrFlag)
       IF (ErrFlag) THEN
         CALL ShowContinueError('...occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
            ErrorsFound=.TRUE.
       ENDIF
    ELSEIF (PTUnit(PTUnitNum)%DXHeatCoilType_Num == Coil_HeatingWaterToAirHPVSEquationFit) THEN
       ErrFlag=.FALSE.
       PTUnit(PTUnitNum)%DesignHeatingCapacity =   &
             GetCoilCapacityVariableSpeed(PTUnit(PTUnitNum)%DXHeatCoilType,PTUnit(PTUnitNum)%DXHeatCoilName,ErrFlag)
       IF (ErrFlag) THEN
         CALL ShowContinueError('...occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
            ErrorsFound=.TRUE.
       ENDIF
     ENDIF
     !Set the heat pump heating coil convergence
     PTUnit(PTUnitNum)%HeatConvergenceTol = 0.001d0
     !Set the heat pump cooling coil capacity (Total capacity)
     !  Get from coil module.
     IF (PTUnit(PTUnitNum)%DXCoolCoilType_Num  == Coil_CoolingWaterToAirHP) THEN
       ErrFlag=.FALSE.
       PTUnit(PTUnitNum)%DesignCoolingCapacity =   &
          GetWtoAHPCoilCapacity(PTUnit(PTUnitNum)%DXCoolCoilType,PTUnit(PTUnitNum)%DXCoolCoilName,ErrFlag)
       IF (ErrFlag) THEN
         CALL ShowContinueError('...occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
         ErrorsFound=.TRUE.
       ENDIF
     ELSEIF (PTUnit(PTUnitNum)%DXCoolCoilType_Num == Coil_CoolingWaterToAirHPSimple) THEN
       ErrFlag=.FALSE.
       PTUnit(PTUnitNum)%DesignCoolingCapacity =   &
          GetWtoAHPSimpleCoilCapacity(PTUnit(PTUnitNum)%DXCoolCoilType,PTUnit(PTUnitNum)%DXCoolCoilName,ErrFlag)
       IF (ErrFlag) THEN
         CALL ShowContinueError('...occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
         ErrorsFound=.TRUE.
       ENDIF
     ELSEIF (PTUnit(PTUnitNum)%DXCoolCoilType_Num == Coil_CoolingWaterToAirHPVSEquationFit) THEN
       ErrFlag=.FALSE.
       PTUnit(PTUnitNum)%DesignCoolingCapacity =   &
          GetCoilCapacityVariableSpeed(PTUnit(PTUnitNum)%DXCoolCoilType,PTUnit(PTUnitNum)%DXCoolCoilName,ErrFlag)
       IF (ErrFlag) THEN
         CALL ShowContinueError('...occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
         ErrorsFound=.TRUE.
       ENDIF
     ENDIF
     !Set the heat pump cooling coil convergence
     PTUnit(PTUnitNum)%CoolConvergenceTol = 0.001d0
     !Set the heatpump cycling rate
     PTUnit(PTUnitNum)%MaxONOFFCyclesperHour = Numbers(7)

     !Set the heat pump time constant
     PTUnit(PTUnitNum)%HPTimeConstant = Numbers(8)

     !Set the heat pump on-cycle power use fraction
     PTUnit(PTUnitNum)%OnCyclePowerFraction = Numbers (9)

     !Set the heat pump fan delay time
     PTUnit(PTUnitNum)%FanDelayTime = Numbers(10)

     !Set the heatpump design supplemental heating capacity
     !  Get from coil module.
     IF(PTUnit(PTUnitNum)%SuppHeatCoilType_Num == Coil_HeatingGas .OR. &
        PTUnit(PTUnitNum)%SuppHeatCoilType_Num == Coil_HeatingElectric)THEN
         ErrFlag=.FALSE.
         PTUnit(PTUnitNum)%DesignSuppHeatingCapacity =   &
            GetHeatingCoilCapacity(SuppHeatCoilType,SuppHeatCoilName,ErrFlag)
         IF (ErrFlag) THEN
           CALL ShowContinueError('...occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
           ErrorsFound=.TRUE.
         ENDIF
     ENDIF

     !Set the max outlet temperature for supplemental heating coil
     PTUnit(PTUnitNum)%MaxSATSupHeat = Numbers(11)

     !Set maximum supply air temperature for supplemental heating coil
     PTUnit(PTUnitNum)%MaxOATSupHeat = Numbers(12)

     !set minimum OA temp for WSHP compressor to large negative number (field not used for a WSHP)
     PTUnit(PTUnitNum)%MinOATCompressor = -99999.0d0

   END DO  !End of the WatertoAirHeatPump Loop




!***********************************************************************************


  DEALLOCATE(Alphas)
  DEALLOCATE(Numbers)
  DEALLOCATE(cAlphaFields)
  DEALLOCATE(cNumericFields)
  DEALLOCATE(lAlphaBlanks)
  DEALLOCATE(lNumericBlanks)


  IF (ErrorsFound) THEN
    CALL ShowFatalError(RoutineName//'Errors found in getting input.')
    CALL ShowContinueError('... Preceding condition causes termination.')
  END IF

  DO PTUnitNum=1,NumPTHP
    ! Setup Report variables for the Packaged Terminal Heat Psmps,   CurrentModuleObject = 'ZoneHVAC:PackagedTerminalHeatPump'
    CALL SetupOutputVariable('Zone Packaged Terminal Heat Pump Total Heating Rate [W]',PTUnit(PTUnitNum)%TotHeatEnergyRate,&
                             'System','Average',PTUnit(PTUnitNum)%Name)
    CALL SetupOutputVariable('Zone Packaged Terminal Heat Pump Total Heating Energy [J]',PTUnit(PTUnitNum)%TotHeatEnergy,&
                             'System','Sum',PTUnit(PTUnitNum)%Name)
    CALL SetupOutputVariable('Zone Packaged Terminal Heat Pump Total Cooling Rate [W]',PTUnit(PTUnitNum)%TotCoolEnergyRate,&
                             'System','Average',PTUnit(PTUnitNum)%Name)
    CALL SetupOutputVariable('Zone Packaged Terminal Heat Pump Total Cooling Energy [J]',PTUnit(PTUnitNum)%TotCoolEnergy,&
                             'System','Sum',PTUnit(PTUnitNum)%Name)
    CALL SetupOutputVariable('Zone Packaged Terminal Heat Pump Sensible Heating Rate [W]',PTUnit(PTUnitNum)%SensHeatEnergyRate,&
                             'System','Average',PTUnit(PTUnitNum)%Name)
    CALL SetupOutputVariable('Zone Packaged Terminal Heat Pump Sensible Heating Energy [J]',PTUnit(PTUnitNum)%SensHeatEnergy,&
                             'System','Sum',PTUnit(PTUnitNum)%Name)
    CALL SetupOutputVariable('Zone Packaged Terminal Heat Pump Sensible Cooling Rate [W]',PTUnit(PTUnitNum)%SensCoolEnergyRate,&
                             'System','Average',PTUnit(PTUnitNum)%Name)
    CALL SetupOutputVariable('Zone Packaged Terminal Heat Pump Sensible Cooling Energy [J]',PTUnit(PTUnitNum)%SensCoolEnergy,&
                             'System','Sum',PTUnit(PTUnitNum)%Name)
    CALL SetupOutputVariable('Zone Packaged Terminal Heat Pump Latent Heating Rate [W]',PTUnit(PTUnitNum)%LatHeatEnergyRate,&
                             'System','Average',PTUnit(PTUnitNum)%Name)
    CALL SetupOutputVariable('Zone Packaged Terminal Heat Pump Latent Heating Energy [J]',PTUnit(PTUnitNum)%LatHeatEnergy,&
                             'System','Sum',PTUnit(PTUnitNum)%Name)
    CALL SetupOutputVariable('Zone Packaged Terminal Heat Pump Latent Cooling Rate [W]',PTUnit(PTUnitNum)%LatCoolEnergyRate,&
                             'System','Average',PTUnit(PTUnitNum)%Name)
    CALL SetupOutputVariable('Zone Packaged Terminal Heat Pump Latent Cooling Energy [J]',PTUnit(PTUnitNum)%LatCoolEnergy,&
                             'System','Sum',PTUnit(PTUnitNum)%Name)
    CALL SetupOutputVariable('Zone Packaged Terminal Heat Pump Electric Power [W]',PTUnit(PTUnitNum)%ElecPower,&
                             'System','Average',PTUnit(PTUnitNum)%Name)
    CALL SetupOutputVariable('Zone Packaged Terminal Heat Pump Electric Energy [J]',PTUnit(PTUnitNum)%ElecConsumption,&
                             'System','Sum',PTUnit(PTUnitNum)%Name)
    CALL SetupOutputVariable('Zone Packaged Terminal Heat Pump Fan Part Load Ratio []',PTUnit(PTUnitNum)%FanPartLoadRatio,&
                             'System','Average',PTUnit(PTUnitNum)%Name)
    CALL SetupOutputVariable('Zone Packaged Terminal Heat Pump Compressor Part Load Ratio []',PTUnit(PTUnitNum)%CompPartLoadRatio,&
                             'System','Average',PTUnit(PTUnitNum)%Name)
    CALL SetupOutputVariable('Zone Packaged Terminal Heat Pump Fan Availability Status []',PTUnit(PTUnitNum)%AvailStatus,&
                               'System','Average',PTUnit(PTUnitNum)%Name)
  END DO


  DO PTUnitNum=1+NumPTHP,NumPTHP+NumPTAC
    ! Setup Report variables for the Packaged Terminal Air Conditioners,
    ! CurrentModuleObject = 'ZoneHVAC:PackagedTerminalAirConditioner'
    CALL SetupOutputVariable('Zone Packaged Terminal Air Conditioner Total Heating Rate [W]',PTUnit(PTUnitNum)%TotHeatEnergyRate,&
                             'System','Average',PTUnit(PTUnitNum)%Name)
    CALL SetupOutputVariable('Zone Packaged Terminal Air Conditioner Total Heating Energy [J]',PTUnit(PTUnitNum)%TotHeatEnergy,&
                             'System','Sum',PTUnit(PTUnitNum)%Name)
    CALL SetupOutputVariable('Zone Packaged Terminal Air Conditioner Total Cooling Rate [W]',PTUnit(PTUnitNum)%TotCoolEnergyRate,&
                             'System','Average',PTUnit(PTUnitNum)%Name)
    CALL SetupOutputVariable('Zone Packaged Terminal Air Conditioner Total Cooling Energy [J]',PTUnit(PTUnitNum)%TotCoolEnergy,&
                             'System','Sum',PTUnit(PTUnitNum)%Name)
    CALL SetupOutputVariable('Zone Packaged Terminal Air Conditioner Sensible Heating Rate [W]',  &
                              PTUnit(PTUnitNum)%SensHeatEnergyRate,&
                             'System','Average',PTUnit(PTUnitNum)%Name)
    CALL SetupOutputVariable('Zone Packaged Terminal Air Conditioner Sensible Heating Energy [J]',PTUnit(PTUnitNum)%SensHeatEnergy,&
                             'System','Sum',PTUnit(PTUnitNum)%Name)
    CALL SetupOutputVariable('Zone Packaged Terminal Air Conditioner Sensible Cooling Rate [W]',  &
                              PTUnit(PTUnitNum)%SensCoolEnergyRate,&
                             'System','Average',PTUnit(PTUnitNum)%Name)
    CALL SetupOutputVariable('Zone Packaged Terminal Air Conditioner Sensible Cooling Energy [J]',PTUnit(PTUnitNum)%SensCoolEnergy,&
                             'System','Sum',PTUnit(PTUnitNum)%Name)
    CALL SetupOutputVariable('Zone Packaged Terminal Air Conditioner Latent Heating Rate [W]',PTUnit(PTUnitNum)%LatHeatEnergyRate,&
                             'System','Average',PTUnit(PTUnitNum)%Name)
    CALL SetupOutputVariable('Zone Packaged Terminal Air Conditioner Latent Heating Energy [J]',PTUnit(PTUnitNum)%LatHeatEnergy,&
                             'System','Sum',PTUnit(PTUnitNum)%Name)
    CALL SetupOutputVariable('Zone Packaged Terminal Air Conditioner Latent Cooling Rate [W]',PTUnit(PTUnitNum)%LatCoolEnergyRate,&
                             'System','Average',PTUnit(PTUnitNum)%Name)
    CALL SetupOutputVariable('Zone Packaged Terminal Air Conditioner Latent Cooling Energy [J]',PTUnit(PTUnitNum)%LatCoolEnergy,&
                             'System','Sum',PTUnit(PTUnitNum)%Name)
    CALL SetupOutputVariable('Zone Packaged Terminal Air Conditioner Electric Power [W]',PTUnit(PTUnitNum)%ElecPower,&
                             'System','Average',PTUnit(PTUnitNum)%Name)
    CALL SetupOutputVariable('Zone Packaged Terminal Air Conditioner Electric Energy [J]',PTUnit(PTUnitNum)%ElecConsumption,&
                             'System','Sum',PTUnit(PTUnitNum)%Name)
    CALL SetupOutputVariable('Zone Packaged Terminal Air Conditioner Fan Part Load Ratio []',PTUnit(PTUnitNum)%FanPartLoadRatio,&
                             'System','Average',PTUnit(PTUnitNum)%Name)
    CALL SetupOutputVariable('Zone Packaged Terminal Air Conditioner Compressor Part Load Ratio []',&
                              PTUnit(PTUnitNum)%CompPartLoadRatio,&
                             'System','Average',PTUnit(PTUnitNum)%Name)
    CALL SetupOutputVariable('Zone Packaged Terminal Air Conditioner Fan Availability Status []',PTUnit(PTUnitNum)%AvailStatus,&
                               'System','Average',PTUnit(PTUnitNum)%Name)
  END DO

  DO PTUnitNum=1+NumPTHP+NumPTAC,NumPTUs
    ! Setup Report variables for the Zone Water Source Heat Pumps, CurrentModuleObject='ZoneHVAC:WaterToAirHeatPump'
    CALL SetupOutputVariable('Zone Water to Air Heat Pump Total Heating Rate [W]',PTUnit(PTUnitNum)%TotHeatEnergyRate,&
                             'System','Average',PTUnit(PTUnitNum)%Name)
    CALL SetupOutputVariable('Zone Water to Air Heat Pump Total Heating Energy [J]',PTUnit(PTUnitNum)%TotHeatEnergy,&
                             'System','Sum',PTUnit(PTUnitNum)%Name)
    CALL SetupOutputVariable('Zone Water to Air Heat Pump Total Cooling Rate [W]',PTUnit(PTUnitNum)%TotCoolEnergyRate,&
                             'System','Average',PTUnit(PTUnitNum)%Name)
    CALL SetupOutputVariable('Zone Water to Air Heat Pump Total Cooling Energy [J]',PTUnit(PTUnitNum)%TotCoolEnergy,&
                             'System','Sum',PTUnit(PTUnitNum)%Name)
    CALL SetupOutputVariable('Zone Water to Air Heat Pump Sensible Heating Rate [W]',PTUnit(PTUnitNum)%SensHeatEnergyRate,&
                             'System','Average',PTUnit(PTUnitNum)%Name)
    CALL SetupOutputVariable('Zone Water to Air Heat Pump Sensible Heating Energy [J]',PTUnit(PTUnitNum)%SensHeatEnergy,&
                             'System','Sum',PTUnit(PTUnitNum)%Name)
    CALL SetupOutputVariable('Zone Water to Air Heat Pump Sensible Cooling Rate [W]',PTUnit(PTUnitNum)%SensCoolEnergyRate,&
                             'System','Average',PTUnit(PTUnitNum)%Name)
    CALL SetupOutputVariable('Zone Water to Air Heat Pump Sensible Cooling Energy [J]',PTUnit(PTUnitNum)%SensCoolEnergy,&
                             'System','Sum',PTUnit(PTUnitNum)%Name)
    CALL SetupOutputVariable('Zone Water to Air Heat Pump Latent Heating Rate [W]',PTUnit(PTUnitNum)%LatHeatEnergyRate,&
                             'System','Average',PTUnit(PTUnitNum)%Name)
    CALL SetupOutputVariable('Zone Water to Air Heat Pump Latent Heating Energy [J]',PTUnit(PTUnitNum)%LatHeatEnergy,&
                             'System','Sum',PTUnit(PTUnitNum)%Name)
    CALL SetupOutputVariable('Zone Water to Air Heat Pump Latent Cooling Rate [W]',PTUnit(PTUnitNum)%LatCoolEnergyRate,&
                             'System','Average',PTUnit(PTUnitNum)%Name)
    CALL SetupOutputVariable('Zone Water to Air Heat Pump Latent Cooling Energy [J]',PTUnit(PTUnitNum)%LatCoolEnergy,&
                             'System','Sum',PTUnit(PTUnitNum)%Name)
    CALL SetupOutputVariable('Zone Water to Air Heat Pump Electric Power [W]',PTUnit(PTUnitNum)%ElecPower,&
                             'System','Average',PTUnit(PTUnitNum)%Name)
    CALL SetupOutputVariable('Zone Water to Air Heat Pump Electric Energy [J]',PTUnit(PTUnitNum)%ElecConsumption,&
                             'System','Sum',PTUnit(PTUnitNum)%Name)
    CALL SetupOutputVariable('Zone Water to Air Heat Pump Fan Part Load Ratio []',PTUnit(PTUnitNum)%FanPartLoadRatio,&
                             'System','Average',PTUnit(PTUnitNum)%Name)
    CALL SetupOutputVariable('Zone Water to Air Heat Pump Compressor Part Load Ratio []',PTUnit(PTUnitNum)%CompPartLoadRatio,&
                             'System','Average',PTUnit(PTUnitNum)%Name)
    CALL SetupOutputVariable('Zone Water to Air Heat Pump Fan Availability Status []',PTUnit(PTUnitNum)%AvailStatus,&
                             'System','Average',PTUnit(PTUnitNum)%Name)
  END DO
  RETURN
END SUBROUTINE GetPTUnit

SUBROUTINE InitPTUnit(PTUnitNum,ZoneNum,FirstHVACIteration,OnOffAirFlowRatio,ZoneLoad)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Raustad
          !       DATE WRITTEN   July 2005
          !       MODIFIED       Chandan Sharma, FSEC, March 2011: Added ZoneHVAC sys avail manager
          !       MODIFIED       Bo Shen, ORNL, March 2012, added variable-speed water-source heat pump
          !       MODIFIED       Bo Shen, ORNL, July 2012, added variable-speed air-source heat pump
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is for initializations of the packaged terminal heat pump components.

          ! METHODOLOGY EMPLOYED:
          ! Uses the status flags to trigger initializations.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataZoneEnergyDemands
  USE DataGlobals,          ONLY: InitConvTemp, AnyPlantInModel
  USE DataEnvironment,      ONLY: StdBaroPress, StdRhoAir
  USE Psychrometrics,       ONLY: PsyRhoAirFnPbTdbW
  USE DataZoneEquipment,    ONLY: ZoneEquipInputsFilled,CheckZoneEquipmentList
  USE HeatingCoils,         ONLY: GetHeatingCoilCapacity=>GetCoilCapacity
  USE SteamCoils,           ONLY: SimulateSteamCoilComponents, GetCoilMaxSteamFlowRate=>GetCoilMaxSteamFlowRate, &
                                  GetSteamCoilCapacity=>GetCoilCapacity
  USE WaterCoils,           ONLY: GetCoilMaxWaterFlowRate, SimulateWaterCoilComponents
!unused-12/12/08  USE FluidProperties,      ONLY: GetSatDensityRefrig !, FindRefrigerant, FindGlycol
  USE DataHeatBalFanSys,    ONLY: TempControlType
  USE Fans,                 ONLY: GetFanVolFlow
  USE DataPlant,            ONLY: TypeOf_CoilSteamAirHeating, ScanPlantLoopsForObject, TypeOf_CoilWaterSimpleHeating, &
                                  PlantLoop
  USE FluidProperties,      ONLY: GetDensityGlycol, GetSatDensityRefrig
  USE PlantUtilities,       ONLY: SetComponentFlowRate, InitComponentNodes
  USE General,              ONLY: TrimSigDigits
  USE DataZoneEquipment,    ONLY: ZoneEquipConfig
  USE VariableSpeedCoils,   ONLY: SimVariableSpeedCoils, VarSpeedCoil

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN)  :: PTUnitNum          ! number of the current PTHP unit being simulated
  INTEGER, INTENT (IN)  :: ZoneNum            ! zone number where the current PTHP unit is located
  LOGICAL, INTENT (IN)  :: FirstHVACIteration ! TRUE on first HVAC iteration
  REAL(r64), INTENT (OUT) :: OnOffAirFlowRatio  ! ratio of compressor ON airflow to average airflow over timestep
  REAL(r64), INTENT (INOUT) :: ZoneLoad         ! cooling or heating needed by zone [watts]

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER             :: InNode         ! inlet node number in PTHP loop
  INTEGER             :: OutNode        ! outlet node number in PTHP loop
  INTEGER             :: OutsideAirNode ! outside air node number in PTHP loop
  REAL(r64)           :: QZnReq         ! cooling or heating needed by zone [watts]
  REAL(r64)           :: RhoAir         ! air density at InNode
  REAL(r64)           :: PartLoadFrac   ! compressor part load fraction
  REAL(r64)           :: CoilMaxVolFlowRate   ! water or steam max volumetric water flow rate
  LOGICAL,SAVE        :: MyOneTimeFlag = .TRUE. ! initialization flag
  LOGICAL,SAVE        :: ZoneEquipmentListChecked = .FALSE.  ! True after the Zone Equipment List has been checked for items
  Integer             :: Loop
  LOGICAL, ALLOCATABLE, SAVE, DIMENSION(:) :: MyEnvrnFlag ! used for initializations each begin environment flag
  LOGICAL, ALLOCATABLE, SAVE, DIMENSION(:) :: MySizeFlag  ! used for sizing PTHP inputs one time
  LOGICAL, ALLOCATABLE, SAVE, DIMENSION(:) :: MyFanFlag  ! used for sizing PTHP fan inputs one time
  LOGICAL, ALLOCATABLE, SAVE, DIMENSION(:) :: MyPlantScanFlag
  REAL(r64)    :: QActual           ! actual PTAC steam heating coil load met (W)
  LOGICAL :: ErrorsFound       ! flag returned from mining call
  REAL(r64)           :: QToCoolSetPt
  REAL(r64)           :: QToHeatSetPt
  REAL(r64)           :: NoCompOutput
  REAL(r64)           :: SupHeaterLoad
  REAL(r64)           :: mdot ! local temporary for mass flow rate (kg/s)
  REAL(r64)           :: rho  ! local for fluid density
  INTEGER             :: SteamIndex
  LOGICAL             :: errFlag
  REAL(r64)           :: LatentOutput                     ! no load latent output (coils off) (W)
  INTEGER             :: NumOfSpeedCooling                ! Number of speeds for cooling
  INTEGER             :: NumOfSpeedHeating                ! Number of speeds for heating
  CHARACTER(len=MaxNameLength) :: CurrentModuleObject     ! Object type for getting and error messages
  INTEGER             ::I                                 ! Loop index
  INTEGER             :: Iter !speed iteration count
  INTEGER             :: PTObjectIndex
  REAL(r64)           :: MulSpeedFlowScale !variable speed air flow scaling factor

  InNode  = PTUnit(PTUnitNum)%AirInNode
  OutNode = PTUnit(PTUnitNum)%AirOutNode

! Do the one time initializations
  IF (MyOneTimeFlag) THEN

    ALLOCATE(MyEnvrnFlag(NumPTUs))
    ALLOCATE(MySizeFlag(NumPTUs))
    ALLOCATE(MyFanFlag(NumPTUs))
    ALLOCATE(MyPlantScanFlag(NumPTUs))
    MyEnvrnFlag = .TRUE.
    MySizeFlag  = .TRUE.
    MyFanFlag   = .TRUE.
    MyPlantScanFlag = .TRUE.
    MyOneTimeFlag = .FALSE.

  END IF

  IF (ALLOCATED(ZoneComp)) THEN
    PTObjectIndex = PTUnit(PTUnitNum)%PTObjectIndex
    ZoneComp(PTUnit(PTUnitNum)%ZoneEquipType)%ZoneCompAvailMgrs( PTObjectIndex)%ZoneNum = ZoneNum
    PTUnit(PTUnitNum)%AvailStatus = ZoneComp(PTUnit(PTUnitNum)%ZoneEquipType)%ZoneCompAvailMgrs( PTObjectIndex )%AvailStatus
  ENDIF

  IF (MyPlantScanFlag(PTUnitNum) .AND. ALLOCATED(PlantLoop)) THEN
    IF ( (PTUnit(PTUnitNum)%ACHeatCoilType_Num == Coil_HeatingWater) .OR. &
         (PTUnit(PTUnitNum)%ACHeatCoilType_Num == Coil_HeatingSteam) ) THEN
      IF (PTUnit(PTUnitNum)%ACHeatCoilType_Num == Coil_HeatingWater) THEN

        errFlag=.false.
        CALL ScanPlantLoopsForObject( PTUnit(PTUnitNum)%ACHeatCoilName, &
                                          TypeOf_CoilWaterSimpleHeating , &
                                          PTUnit(PTUnitNum)%LoopNum, &
                                          PTUnit(PTUnitNum)%LoopSide, &
                                          PTUnit(PTUnitNum)%BranchNum, &
                                          PTUnit(PTUnitNum)%CompNum,   &
                                          errFlag=errFlag)
        IF (errFlag) THEN
          CALL ShowContinueError('Reference Unit="'//trim(PTUnit(PTUnitNum)%Name)//'", type='//trim(PTUnit(PTUnitNum)%UnitType))
          CALL ShowFatalError('InitPTUnit: Program terminated for previous conditions.')
        ENDIF

        PTUnit(PTUnitNum)%MaxHeatCoilFluidFlow = GetCoilMaxWaterFlowRate('Coil:Heating:Water',  &
                                               PTUnit(PTUnitNum)%ACHeatCoilName,ErrorsFound)

        IF(PTUnit(PTUnitNum)%MaxHeatCoilFluidFlow .GT. 0.0d0)THEN
          rho = GetDensityGlycol(PlantLoop(PTUnit(PTUnitNum)%LoopNum)%FluidName, &
                                InitConvTemp, &
                                PlantLoop(PTUnit(PTUnitNum)%LoopNum)%FluidIndex, &
                                'InitPTUnit')

          PTUnit(PTUnitNum)%MaxHeatCoilFluidFlow = GetCoilMaxWaterFlowRate('Coil:Heating:Water',  &
                                                       PTUnit(PTUnitNum)%ACHeatCoilName,ErrorsFound) * rho
        END IF

      ELSEIF (PTUnit(PTUnitNum)%ACHeatCoilType_Num == Coil_HeatingSteam) THEN

        errFlag=.false.
        CALL ScanPlantLoopsForObject( PTUnit(PTUnitNum)%ACHeatCoilName, &
                                          TypeOf_CoilSteamAirHeating , &
                                          PTUnit(PTUnitNum)%LoopNum, &
                                          PTUnit(PTUnitNum)%LoopSide, &
                                          PTUnit(PTUnitNum)%BranchNum, &
                                          PTUnit(PTUnitNum)%CompNum,   &
                                          errFlag=errFlag)
        IF (errFlag) THEN
          CALL ShowContinueError('Reference Unit="'//trim(PTUnit(PTUnitNum)%Name)//'", type='//trim(PTUnit(PTUnitNum)%UnitType))
          CALL ShowFatalError('InitPTUnit: Program terminated for previous conditions.')
        ENDIF

        PTUnit(PTUnitNum)%MaxHeatCoilFluidFlow = GetCoilMaxSteamFlowRate(PTUnit(PTUnitNum)%ACHeatCoilIndex,ErrorsFound)

        IF(PTUnit(PTUnitNum)%MaxHeatCoilFluidFlow .GT. 0.0d0)THEN
          SteamIndex = 0 ! Function GetSatDensityRefrig will look up steam index if 0 is passed
          SteamDensity=GetSatDensityRefrig('STEAM',TempSteamIn,1.0d0,SteamIndex,'InitPTUnit')
          PTUnit(PTUnitNum)%MaxHeatCoilFluidFlow =   &
             GetCoilMaxSteamFlowRate(PTUnit(PTUnitNum)%ACHeatCoilIndex,ErrorsFound) * SteamDensity
        END IF

      ENDIF

         !fill outlet node for coil
      PTUnit(PTUnitNum)%PlantCoilOutletNode =  &
            PlantLoop(PTUnit(PTUnitNum)%LoopNum)%LoopSide(PTUnit(PTUnitNum)%LoopSide) &
                       %Branch(PTUnit(PTUnitNum)%BranchNum)%Comp(PTUnit(PTUnitNum)%CompNum)%NodeNumOut
      MyPlantScanFlag(PTUnitNum) = .FALSE.

    ELSEIF ( (PTUnit(PTUnitNum)%SuppHeatCoilType_Num == Coil_HeatingWater) .OR. &
             (PTUnit(PTUnitNum)%SuppHeatCoilType_Num == Coil_HeatingSteam) ) THEN
      IF (PTUnit(PTUnitNum)%SuppHeatCoilType_Num == Coil_HeatingWater) THEN
        errFlag=.false.
        CALL ScanPlantLoopsForObject( PTUnit(PTUnitNum)%SuppHeatCoilName, &
                                          TypeOf_CoilWaterSimpleHeating , &
                                          PTUnit(PTUnitNum)%LoopNum, &
                                          PTUnit(PTUnitNum)%LoopSide, &
                                          PTUnit(PTUnitNum)%BranchNum, &
                                          PTUnit(PTUnitNum)%CompNum,   &
                                          errFlag=errFlag)
        IF (errFlag) THEN
          CALL ShowFatalError('InitPTUnit: Program terminated for previous conditions.')
        ENDIF
        PTUnit(PTUnitNum)%MaxSuppCoilFluidFlow = GetCoilMaxWaterFlowRate('Coil:Heating:Water',  &
                                                 PTUnit(PTUnitNum)%SuppHeatCoilName,ErrorsFound)

        IF(PTUnit(PTUnitNum)%MaxSuppCoilFluidFlow .GT. 0.0d0)THEN
            rho = GetDensityGlycol(PlantLoop(PTUnit(PTUnitNum)%LoopNum)%FluidName, &
                                   InitConvTemp, &
                                   PlantLoop(PTUnit(PTUnitNum)%LoopNum)%FluidIndex, &
                                   'InitPTUnit')
            PTUnit(PTUnitNum)%MaxSuppCoilFluidFlow = GetCoilMaxWaterFlowRate('Coil:Heating:Water',  &
                                                     PTUnit(PTUnitNum)%SuppHeatCoilName,ErrorsFound) * rho
        END IF
      ELSEIF (PTUnit(PTUnitNum)%SuppHeatCoilType_Num == Coil_HeatingSteam) THEN
        errFlag=.false.
        CALL ScanPlantLoopsForObject( PTUnit(PTUnitNum)%SuppHeatCoilName, &
                                          TypeOf_CoilSteamAirHeating , &
                                          PTUnit(PTUnitNum)%LoopNum, &
                                          PTUnit(PTUnitNum)%LoopSide, &
                                          PTUnit(PTUnitNum)%BranchNum, &
                                          PTUnit(PTUnitNum)%CompNum,   &
                                          errFlag=errFlag)
        IF (errFlag) THEN
          CALL ShowFatalError('InitPTUnit: Program terminated for previous conditions.')
        ENDIF
        PTUnit(PTUnitNum)%MaxSuppCoilFluidFlow = GetCoilMaxSteamFlowRate(PTUnit(PTUnitNum)%SuppHeatCoilIndex,ErrorsFound)
        IF(PTUnit(PTUnitNum)%MaxSuppCoilFluidFlow .GT. 0.0d0)THEN
          SteamIndex = 0 ! Function GetSatDensityRefrig will look up steam index if 0 is passed
          SteamDensity=GetSatDensityRefrig('STEAM',TempSteamIn,1.0d0,SteamIndex,'InitPTUnit')
          PTUnit(PTUnitNum)%MaxSuppCoilFluidFlow = &
                   GetCoilMaxSteamFlowRate(PTUnit(PTUnitNum)%SuppHeatCoilIndex,ErrorsFound) * SteamDensity
        END IF
      ENDIF
         !fill outlet node for coil
      PTUnit(PTUnitNum)%PlantCoilOutletNode =  &
            PlantLoop(PTUnit(PTUnitNum)%LoopNum)%LoopSide(PTUnit(PTUnitNum)%LoopSide) &
                       %Branch(PTUnit(PTUnitNum)%BranchNum)%Comp(PTUnit(PTUnitNum)%CompNum)%NodeNumOut
      MyPlantScanFlag(PTUnitNum) = .FALSE.
    ELSE ! pthp not connected to plant
      MyPlantScanFlag(PTUnitNum) = .FALSE.
    ENDIF
  ELSEIF (MyPlantScanFlag(PTUnitNum) .AND. .NOT. AnyPlantInModel) THEN
    MyPlantScanFlag(PTUnitNum) = .FALSE.
  ENDIF

  IF (.NOT. ZoneEquipmentListChecked .AND. ZoneEquipInputsFilled) THEN
    ZoneEquipmentListChecked=.TRUE.
    DO Loop=1,NumPTUs
      IF (CheckZoneEquipmentList(PTUnit(Loop)%UnitType,PTUnit(Loop)%Name)) CYCLE
      CALL ShowSevereError('InitPTHP: Packaged Terminal Unit=['//TRIM(PTUnit(Loop)%UnitType)//','//  &
         TRIM(PTUnit(Loop)%Name)//  &
           '] is not on any ZoneHVAC:EquipmentList.  It will not be simulated.')
    ENDDO
  ENDIF

  IF ( .NOT. SysSizingCalc .AND. MySizeFlag(PTUnitNum) ) THEN
    CALL SizePTUnit(PTUnitNum)
    MySizeFlag(PTUnitNum) = .FALSE.
  END IF

  IF ((PTUnit(PTUnitNum)%DXCoolCoilType_Num == Coil_CoolingWaterToAirHPVSEquationFit &
    .OR. PTUnit(PTUnitNum)%DXCoolCoilType_Num == Coil_CoolingAirToAirVariableSpeed) .AND. &
      (0 == PTUnit(PTUnitNum)%NumOfSpeedCooling)) THEN

    CALL SimVariableSpeedCoils('',PTUnit(PTUnitNum)%DXCoolCoilIndexNum,&
           0,PTUnit(PTUnitNum)%MaxONOFFCyclesperHour, &
           PTUnit(PTUnitNum)%HPTimeConstant,PTUnit(PTUnitNum)%FanDelayTime,&
           0, 0.0d0, 0.0d0,1, 0.0d0,0.0d0, 0.0d0 )     !conduct the sizing operation in the VS WSHP
    PTUnit(PTUnitNum)%NumOfSpeedCooling = VarSpeedCoil(PTUnit(PTUnitNum)%DXCoolCoilIndexNum)%NumOfSpeeds

    MulSpeedFlowScale = VarSpeedCoil(PTUnit(PTUnitNum)%DXCoolCoilIndexNum)%RatedAirVolFlowRate/ &
    VarSpeedCoil(PTUnit(PTUnitNum)%DXCoolCoilIndexNum)%MSRatedAirVolFlowRate&
                (VarSpeedCoil(PTUnit(PTUnitNum)%DXCoolCoilIndexNum)%NormSpedLevel)
    Do Iter = 1,PTUnit(PTUnitNum)%NumOfSpeedCooling
      PTUnit(PTUnitNum)%CoolVolumeFlowRate(Iter) = &
            VarSpeedCoil(PTUnit(PTUnitNum)%DXCoolCoilIndexNum)%MSRatedAirVolFlowRate(Iter) * &
            MulSpeedFlowScale
      PTUnit(PTUnitNum)%CoolMassFlowRate(Iter) = &
            VarSpeedCoil(PTUnit(PTUnitNum)%DXCoolCoilIndexNum)%MSRatedAirMassFlowRate(Iter) * &
            MulSpeedFlowScale
      PTUnit(PTUnitNum)%MSCoolingSpeedRatio(Iter) = &
            VarSpeedCoil(PTUnit(PTUnitNum)%DXCoolCoilIndexNum)%MSRatedAirVolFlowRate(Iter)/ &
            VarSpeedCoil(PTUnit(PTUnitNum)%DXCoolCoilIndexNum)%MSRatedAirVolFlowRate&
            (PTUnit(PTUnitNum)%NumOfSpeedCooling)
    End Do

    IF (PTUnit(PTUnitNum)%DXHeatCoilType_Num == Coil_HeatingWaterToAirHPVSEquationFit &
        .OR. PTUnit(PTUnitNum)%DXHeatCoilType_Num == Coil_HeatingAirToAirVariableSpeed ) THEN

        CALL SimVariableSpeedCoils('',PTUnit(PTUnitNum)%DXHeatCoilIndex,&
               0,PTUnit(PTUnitNum)%MaxONOFFCyclesperHour, &
               PTUnit(PTUnitNum)%HPTimeConstant,PTUnit(PTUnitNum)%FanDelayTime,&
               0, 0.0d0, 0.0d0,1, 0.0d0,0.0d0, 0.0d0 ) !conduct the sizing operation in the VS WSHP

        PTUnit(PTUnitNum)%NumOfSpeedHeating = VarSpeedCoil(PTUnit(PTUnitNum)%DXHeatCoilIndex)%NumOfSpeeds

        MulSpeedFlowScale = VarSpeedCoil(PTUnit(PTUnitNum)%DXHeatCoilIndex)%RatedAirVolFlowRate/ &
        VarSpeedCoil(PTUnit(PTUnitNum)%DXHeatCoilIndex)%MSRatedAirVolFlowRate &
                    (VarSpeedCoil(PTUnit(PTUnitNum)%DXHeatCoilIndex)%NormSpedLevel)
        Do Iter = 1,PTUnit(PTUnitNum)%NumOfSpeedHeating
          PTUnit(PTUnitNum)%HeatVolumeFlowRate(Iter) = &
                VarSpeedCoil(PTUnit(PTUnitNum)%DXHeatCoilIndex)%MSRatedAirVolFlowRate(Iter) * &
                MulSpeedFlowScale
          PTUnit(PTUnitNum)%HeatMassFlowRate(Iter) = &
                VarSpeedCoil(PTUnit(PTUnitNum)%DXHeatCoilIndex)%MSRatedAirMassFlowRate(Iter) * &
                MulSpeedFlowScale
          PTUnit(PTUnitNum)%MSHeatingSpeedRatio(Iter) = &
                VarSpeedCoil(PTUnit(PTUnitNum)%DXHeatCoilIndex)%MSRatedAirVolFlowRate(Iter)/ &
                VarSpeedCoil(PTUnit(PTUnitNum)%DXHeatCoilIndex)%MSRatedAirVolFlowRate&
                (PTUnit(PTUnitNum)%NumOfSpeedHeating)
        End Do
   END IF
    ! intialize idle flow

   IF(PTUnit(PTUnitNum)%NumOfSpeedHeating > 0) THEN
       PTUnit(PTUnitNum)%IdleMassFlowRate = &
            min(PTUnit(PTUnitNum)%HeatMassFlowRate(1), PTUnit(PTUnitNum)%CoolMassFlowRate(1))
       PTUnit(PTUnitNum)%IdleSpeedRatio = &
            min(PTUnit(PTUnitNum)%MSHeatingSpeedRatio(1), PTUnit(PTUnitNum)%MSCoolingSpeedRatio(1))
       PTUnit(PTUnitNum)%IdleVolumeAirRate = &
            min(PTUnit(PTUnitNum)%HeatVolumeFlowRate(1), PTUnit(PTUnitNum)%CoolVolumeFlowRate(1))
   ELSE
       PTUnit(PTUnitNum)%IdleMassFlowRate = PTUnit(PTUnitNum)%CoolMassFlowRate(1)
       PTUnit(PTUnitNum)%IdleSpeedRatio =   PTUnit(PTUnitNum)%MSCoolingSpeedRatio(1)
       PTUnit(PTUnitNum)%IdleVolumeAirRate = PTUnit(PTUnitNum)%CoolVolumeFlowRate(1)
   END IF

   IF (PTUnit(PTUnitNum)%OpMode .EQ. ContFanCycCoil) THEN
        PTUnit(PTUnitNum)%MaxNoCoolHeatAirVolFlow = PTUnit(PTUnitNum)%IdleVolumeAirRate
        PTUnit(PTUnitNum)%MaxNoCoolHeatAirMassFlow = PTUnit(PTUnitNum)%IdleMassFlowRate
        PTUnit(PTUnitNum)%NoHeatCoolSpeedRatio= PTUnit(PTUnitNum)%IdleSpeedRatio
   END IF
  END IF

  IF(MyFanFlag(PTUnitNum))THEN
    IF(PTUnit(PTUnitNum)%ActualFanVolFlowRate /= Autosize)THEN
      IF(PTUnit(PTUnitNum)%ActualFanVolFlowRate .GT. 0.0d0)THEN
        PTUnit(PTUnitNum)%HeatingSpeedRatio = PTUnit(PTUnitNum)%MaxHeatAirVolFlow/PTUnit(PTUnitNum)%ActualFanVolFlowRate
        PTUnit(PTUnitNum)%CoolingSpeedRatio = PTUnit(PTUnitNum)%MaxCoolAirVolFlow/PTUnit(PTUnitNum)%ActualFanVolFlowRate
        PTUnit(PTUnitNum)%NoHeatCoolSpeedRatio = PTUnit(PTUnitNum)%MaxNoCoolHeatAirVolFlow/PTUnit(PTUnitNum)%ActualFanVolFlowRate
      END IF
      MyFanFlag(PTUnitNum) = .FALSE.
    ELSE
      CALL GetFanVolFlow(PTUnit(PTUnitNum)%FanIndex,PTUnit(PTUnitNum)%ActualFanVolFlowRate)
    END IF
  END IF

  IF (PTUnit(PTUnitNum)%FanSchedPtr .GT. 0) THEN
    IF (GetCurrentScheduleValue(PTUnit(PTUnitNum)%FanSchedPtr) .EQ. 0.0d0) THEN
      PTUnit(PTUnitNum)%OpMode = CycFanCycCoil
    ELSE
      PTUnit(PTUnitNum)%OpMode = ContFanCycCoil
    END IF
  END IF

  QZnReq = ZoneLoad

! Original thermostat control logic
! Sets initial control based on load - works only for cycling fan systems
! Constant fan systems will further test the load including the impacts of OA
! OA can change the load to be met by the PTUnit (this is done later in Init)
  IF(QZnReq .GT. SmallLoad)THEN
    HeatingLoad = .TRUE.
    CoolingLoad = .FALSE.
  ELSE IF(ABS(QZnReq) .GT. SmallLoad)THEN
    HeatingLoad = .FALSE.
    CoolingLoad = .TRUE.
  ELSE
    HeatingLoad = .FALSE.
    CoolingLoad = .FALSE.
  END IF

  ! Initialize the operating PLR (turn coils on if needed, otherwise turn coils off)
  IF (GetCurrentScheduleValue(PTUnit(PTUnitNum)%SchedPtr) .gt. 0.0d0) THEN
    IF (HeatingLoad .OR. CoolingLoad) THEN
      PartLoadFrac = 1.0d0
    ELSE
      PartLoadFrac = 0.0d0
    END IF
  ELSE
    PartLoadFrac = 0.0d0
  END IF

  IF(PTUnit(PTUnitNum)%NumOfSpeedCooling > 0 ) THEN !BoS, variable-speed water source hp
    !PTUnit(PTUnitNum)%IdleMassFlowRate = RhoAir*PTUnit(PTUnitNum)%IdleVolumeAirRate
      NumOfSpeedCooling = PTUnit(PTUnitNum)%NumOfSpeedCooling
      NumOfSpeedHeating = PTUnit(PTUnitNum)%NumOfSpeedHeating
    ! IF MSHP system was not autosized and the fan is autosized, check that fan volumetric flow rate is greater than MSHP flow rates
      IF(PTUnit(PTUnitNum)%CheckFanFlow)THEN
        CurrentModuleObject = 'ZoneHVAC:PackagedTerminalHeatPump'
        CALL GetFanVolFlow(PTUnit(PTUnitNum)%FanIndex,PTUnit(PTUnitNum)%FanVolFlow)
        IF(PTUnit(PTUnitNum)%FanVolFlow .NE. AutoSize)THEN
    !     Check fan versus system supply air flow rates
          IF(PTUnit(PTUnitNum)%FanVolFlow  + 1d-10 .LT. &
             PTUnit(PTUnitNum)%CoolVolumeFlowRate(NumOfSpeedCooling))THEN
            CALL ShowWarningError(TRIM(CurrentModuleObject)//' - air flow rate = ' &
                            //TRIM(TrimSigDigits(PTUnit(PTUnitNum)%FanVolFlow,7))//' in fan object ' &
                            //' is less than the MSHP system air flow rate' &
                            //' when cooling is required ('// &
            TRIM(TrimSigDigits(PTUnit(PTUnitNum)%CoolVolumeFlowRate(NumOfSpeedCooling),7))//').')
            CALL ShowContinueError(' The MSHP system flow rate when cooling is required is reset to the' &
                                  //' fan flow rate and the simulation continues.')
            CALL ShowContinueError(' Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(PTUnit(PTUnitNum)%Name))
            PTUnit(PTUnitNum)%CoolVolumeFlowRate(NumOfSpeedCooling) = PTUnit(PTUnitNum)%FanVolFlow
            ! Check flow rates in other speeds and ensure flow rates are not above the max flow rate
            Do i=NumOfSpeedCooling-1,1,-1
              If (PTUnit(PTUnitNum)%CoolVolumeFlowRate(i) .GT. PTUnit(PTUnitNum)%CoolVolumeFlowRate(i+1)) Then
                CALL ShowContinueError(' The MSHP system flow rate when cooling is required is reset to the' &
                     //' flow rate at higher speed and the simulation continues at Speed'//TrimSigDigits(i,0)//'.')
                CALL ShowContinueError(' Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(PTUnit(PTUnitNum)%Name))
                PTUnit(PTUnitNum)%CoolVolumeFlowRate(i) = PTUnit(PTUnitNum)%CoolVolumeFlowRate(i+1)
              End If
            End Do
          END IF

          IF(PTUnit(PTUnitNum)%NumOfSpeedHeating > 0) THEN
              IF(PTUnit(PTUnitNum)%FanVolFlow  + 1d-10 .LT. &
                 PTUnit(PTUnitNum)%HeatVolumeFlowRate(NumOfSpeedHeating))THEN
                CALL ShowWarningError(TRIM(CurrentModuleObject)//' - air flow rate = ' &
                                //TRIM(TrimSigDigits(PTUnit(PTUnitNum)%FanVolFlow,7))//' in fan object ' &
                                //' is less than the MSHP system air flow rate' &
                                //' when heating is required ('// &
                  TRIM(TrimSigDigits(PTUnit(PTUnitNum)%HeatVolumeFlowRate(NumOfSpeedHeating),7))//').')
                CALL ShowContinueError(' The MSHP system flow rate when heating is required is reset to the' &
                                      //' fan flow rate and the simulation continues.')
                CALL ShowContinueError(' Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(PTUnit(PTUnitNum)%Name))
                PTUnit(PTUnitNum)%HeatVolumeFlowRate(NumOfSpeedHeating) = PTUnit(PTUnitNum)%FanVolFlow
                Do i=NumOfSpeedHeating-1,1,-1
                  If (PTUnit(PTUnitNum)%HeatVolumeFlowRate(i) .GT. PTUnit(PTUnitNum)%HeatVolumeFlowRate(i+1)) Then
                    CALL ShowContinueError(' The MSHP system flow rate when heating is required is reset to the' &
                         //' flow rate at higher speed and the simulation continues at Speed'//TrimSigDigits(i,0)//'.')
                    CALL ShowContinueError(' Occurs in '//TRIM(CurrentModuleObject)//' system = '//TRIM(PTUnit(PTUnitNum)%Name))
                    PTUnit(PTUnitNum)%HeatVolumeFlowRate(i) = PTUnit(PTUnitNum)%HeatVolumeFlowRate(i+1)
                  End If
                End Do
              END IF
          END IF

          IF(PTUnit(PTUnitNum)%FanVolFlow .LT. PTUnit(PTUnitNum)%IdleVolumeAirRate .AND. &
             PTUnit(PTUnitNum)%IdleVolumeAirRate .NE. 0.0d0)THEN
            CALL ShowWarningError(TRIM(CurrentModuleObject)//' - air flow rate = ' &
                       //TRIM(TrimSigDigits(PTUnit(PTUnitNum)%FanVolFlow,7))//' in fan object ' &
                       //' is less than the MSHP system air flow rate when no ' &
                       //'heating or cooling is needed ('//TRIM(TrimSigDigits(PTUnit(PTUnitNum)%IdleVolumeAirRate,7))//').')
            CALL ShowContinueError(' The MSHP system flow rate when no heating or cooling is needed is reset to the' &
                                  //' fan flow rate and the simulation continues.')
            CALL ShowContinueError(' Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(PTUnit(PTUnitNum)%Name))
            PTUnit(PTUnitNum)%IdleVolumeAirRate = PTUnit(PTUnitNum)%FanVolFlow
          END IF
          RhoAir = StdRhoAir
          ! set the mass flow rates from the reset volume flow rates
          Do I=1,NumOfSpeedCooling
            PTUnit(PTUnitNum)%CoolMassFlowRate(i) = RhoAir*PTUnit(PTUnitNum)%CoolVolumeFlowRate(i)
            IF(PTUnit(PTUnitNum)%FanVolFlow .GT. 0.0d0)THEN
              PTUnit(PTUnitNum)%MSCoolingSpeedRatio(i) = &
                  PTUnit(PTUnitNum)%CoolVolumeFlowRate(i)/PTUnit(PTUnitNum)%FanVolFlow
            END IF
          End Do
          Do I=1,NumOfSpeedHeating
            PTUnit(PTUnitNum)%HeatMassFlowRate(i) = RhoAir*PTUnit(PTUnitNum)%HeatVolumeFlowRate(i)
            IF(PTUnit(PTUnitNum)%FanVolFlow .GT. 0.0d0)THEN
              PTUnit(PTUnitNum)%MSHeatingSpeedRatio(i) = &
                  PTUnit(PTUnitNum)%HeatVolumeFlowRate(i)/PTUnit(PTUnitNum)%FanVolFlow
            END IF
          End Do
          PTUnit(PTUnitNum)%IdleMassFlowRate = RhoAir*PTUnit(PTUnitNum)%IdleVolumeAirRate
          IF(PTUnit(PTUnitNum)%FanVolFlow .GT. 0.0d0)THEN
            PTUnit(PTUnitNum)%IdleSpeedRatio = &
                PTUnit(PTUnitNum)%IdleVolumeAirRate / PTUnit(PTUnitNum)%FanVolFlow
          END IF
          ! set the node max and min mass flow rates based on reset volume flow rates
          IF(PTUnit(PTUnitNum)%NumOfSpeedHeating > 0) THEN
              Node(InNode)%MassFlowRateMax = MAX(PTUnit(PTUnitNum)%CoolMassFlowRate(NumOfSpeedCooling), &
                                                 PTUnit(PTUnitNum)%HeatMassFlowRate(NumOfSpeedHeating))
              Node(InNode)%MassFlowRateMaxAvail = MAX(PTUnit(PTUnitNum)%CoolMassFlowRate(NumOfSpeedCooling), &
                                                      PTUnit(PTUnitNum)%HeatMassFlowRate(NumOfSpeedHeating))
          ELSE
              Node(InNode)%MassFlowRateMax = PTUnit(PTUnitNum)%CoolMassFlowRate(NumOfSpeedCooling)
              Node(InNode)%MassFlowRateMaxAvail = PTUnit(PTUnitNum)%CoolMassFlowRate(NumOfSpeedCooling)
          END IF

          Node(InNode)%MassFlowRateMin = 0.0d0
          Node(InNode)%MassFlowRateMinAvail = 0.0d0
          Node(OutNode) = Node(InNode)
        END IF
      END IF

      PTUnit(PTUnitNum)%CheckFanFlow = .FALSE.

      CALL SetOnOffMassFlowRate(PTUnitNum, PartLoadFrac, OnOffAirFlowRatio)

     !CALL SetOnOffMassFlowRateVSCoil(PTUnitNum,ZoneNum, FirstHVACIteration, &
     !               ZoneEquipConfig(ZoneNum)%AirLoopNum, OnOffAirFlowRatio, PTUnit(PTUnitNum)%OpMode, QZnReq, 0.0d0, PartLoadFrac)
  ELSE
    CALL SetOnOffMassFlowRate(PTUnitNum, PartLoadFrac, OnOffAirFlowRatio)
  END IF

! Do the Begin Environment initializations
  IF (BeginEnvrnFlag .and. MyEnvrnFlag(PTUnitNum)) THEN
    InNode  = PTUnit(PTUnitNum)%AirInNode
    OutNode = PTUnit(PTUnitNum)%AirOutNode
    OutsideAirNode = PTUnit(PTUnitNum)%OutsideAirNode
    RhoAir = StdRhoAir
    ! set the mass flow rates from the input volume flow rates
    PTUnit(PTUnitNum)%MaxCoolAirMassFlow = RhoAir*PTUnit(PTUnitNum)%MaxCoolAirVolFlow
    PTUnit(PTUnitNum)%CoolOutAirMassFlow = RhoAir*PTUnit(PTUnitNum)%CoolOutAirVolFlow
    PTUnit(PTUnitNum)%MaxHeatAirMassFlow = RhoAir*PTUnit(PTUnitNum)%MaxHeatAirVolFlow
    PTUnit(PTUnitNum)%HeatOutAirMassFlow = RhoAir*PTUnit(PTUnitNum)%HeatOutAirVolFlow
    PTUnit(PTUnitNum)%MaxNoCoolHeatAirMassFlow = RhoAir*PTUnit(PTUnitNum)%MaxNoCoolHeatAirVolFlow
    PTUnit(PTUnitNum)%NoCoolHeatOutAirMassFlow = RhoAir*PTUnit(PTUnitNum)%NoCoolHeatOutAirVolFlow
    ! set the node max and min mass flow rates
    ! outside air mixer is optional, check that node num > 0
    IF(OutsideAirNode .GT. 0)THEN
      Node(OutsideAirNode)%MassFlowRateMax = MAX(PTUnit(PTUnitNum)%CoolOutAirMassFlow,PTUnit(PTUnitNum)%HeatOutAirMassFlow)
      Node(OutsideAirNode)%MassFlowRateMin = 0.0d0
      Node(OutsideAirNode)%MassFlowRateMinAvail = 0.0d0
    END IF
    Node(OutNode)%MassFlowRateMax = MAX(PTUnit(PTUnitNum)%MaxCoolAirMassFlow,PTUnit(PTUnitNum)%MaxHeatAirMassFlow)
    Node(OutNode)%MassFlowRateMin = 0.0d0
    Node(OutNode)%MassFlowRateMinAvail = 0.0d0
    Node(InNode)%MassFlowRateMax = MAX(PTUnit(PTUnitNum)%MaxCoolAirMassFlow,PTUnit(PTUnitNum)%MaxHeatAirMassFlow)
    Node(InNode)%MassFlowRateMin = 0.0d0
    Node(InNode)%MassFlowRateMinAvail = 0.0d0
    IF(PTUnit(PTUnitNum)%AirReliefNode .GT. 0)THEN
      Node(PTUnit(PTUnitNum)%AirReliefNode)%MassFlowRateMinAvail = 0.0d0
    END IF
    MyEnvrnFlag(PTUnitNum) = .FALSE.
    PTUnit(PTUnitNum)%LastMode = HeatingMode

!   set fluid-side hardware limits
    IF(PTUnit(PTUnitNum)%HotWaterControlNode .GT. 0)THEN
       ! If water coil max water flow rate is autosized, simulate once in order to mine max water flow rate
      IF(PTUnit(PTUnitNum)%MaxHeatCoilFluidFlow .EQ. Autosize)THEN
        CALL SimulateWaterCoilComponents(PTUnit(PTUnitNum)%ACHeatCoilName,FirstHVACIteration, &
                                         PTUnit(PTUnitNum)%ACHeatCoilIndex)
        CoilMaxVolFlowRate = GetCoilMaxWaterFlowRate('Coil:Heating:Water',  &
                                          PTUnit(PTUnitNum)%ACHeatCoilName,ErrorsFound)
        IF(CoilMaxVolFlowRate .NE. Autosize) THEN

          rho = GetDensityGlycol(PlantLoop(PTUnit(PTUnitNum)%LoopNum)%fluidName, &
                                 InitConvTemp, &
                                 PlantLoop(PTUnit(PTUnitNum)%LoopNum)%fluidIndex, &
                                 ' InitPTUnit')
          PTUnit(PTUnitNum)%MaxHeatCoilFluidFlow = CoilMaxVolFlowRate * rho

        ENDIF
      ENDIF

      Call InitComponentNodes(0.d0, PTUnit(PTUnitNum)%MaxHeatCoilFluidFlow, &
                                    PTUnit(PTUnitNum)%HotWaterControlNode, &
                                    PTUnit(PTUnitNum)%PlantCoilOutletNode, &
                                    PTUnit(PTUnitNum)%LoopNum, &
                                    PTUnit(PTUnitNum)%LoopSide, &
                                    PTUnit(PTUnitNum)%BranchNum, &
                                    PTUnit(PTUnitNum)%CompNum )

      IF(PTUnit(PTUnitNum)%MaxSuppCoilFluidFlow .EQ. Autosize)THEN
         CALL SimulateWaterCoilComponents(PTUnit(PTUnitNum)%SuppHeatCoilName,FirstHVACIteration, &
                                          PTUnit(PTUnitNum)%SuppHeatCoilIndex)
              CoilMaxVolFlowRate = GetCoilMaxWaterFlowRate('Coil:Heating:Water',  &
                                                            PTUnit(PTUnitNum)%SuppHeatCoilName,ErrorsFound)
         IF(CoilMaxVolFlowRate .NE. Autosize) THEN
           rho = GetDensityGlycol(PlantLoop(PTUnit(PTUnitNum)%LoopNum)%fluidName, &
                                 InitConvTemp, &
                                 PlantLoop(PTUnit(PTUnitNum)%LoopNum)%fluidIndex, &
                                 ' InitPTUnit')
           PTUnit(PTUnitNum)%MaxSuppCoilFluidFlow = CoilMaxVolFlowRate * rho
         ENDIF
      END IF
      Call InitComponentNodes(0.d0, PTUnit(PTUnitNum)%MaxSuppCoilFluidFlow, &
                                    PTUnit(PTUnitNum)%HotWaterControlNode, &
                                    PTUnit(PTUnitNum)%PlantCoilOutletNode, &
                                    PTUnit(PTUnitNum)%LoopNum, &
                                    PTUnit(PTUnitNum)%LoopSide, &
                                    PTUnit(PTUnitNum)%BranchNum, &
                                    PTUnit(PTUnitNum)%CompNum )

    ENDIF
    IF(PTUnit(PTUnitNum)%HWCoilSteamInletNode .GT. 0)THEN
    !     If steam coil max steam flow rate is autosized, simulate once in order to mine max steam flow rate
      IF(PTUnit(PTUnitNum)%MaxHeatCoilFluidFlow .EQ. Autosize)THEN
          CALL SimulateSteamCoilComponents(PTUnit(PTUnitNum)%ACHeatCoilName, &
                                           FirstHVACIteration,    &
                                           1.0d0, & !QCoilReq, simulate any load > 0 to get max capacity of steam coil
                                           PTUnit(PTUnitNum)%ACHeatCoilIndex, QActual)
          CoilMaxVolFlowRate = GetCoilMaxSteamFlowRate(PTUnit(PTUnitNum)%ACHeatCoilIndex,ErrorsFound)

          IF(CoilMaxVolFlowRate .NE. Autosize) THEN
            SteamIndex = 0 ! Function GetSatDensityRefrig will look up steam index if 0 is passed
            SteamDensity=GetSatDensityRefrig('STEAM',TempSteamIn,1.0d0,SteamIndex,'InitPTUnit')
            PTUnit(PTUnitNum)%MaxHeatCoilFluidFlow = CoilMaxVolFlowRate * SteamDensity
          ENDIF
          CALL InitComponentNodes(0.d0, PTUnit(PTUnitNum)%MaxHeatCoilFluidFlow, &
                                        PTUnit(PTUnitNum)%HWCoilSteamInletNode, &
                                        PTUnit(PTUnitNum)%PlantCoilOutletNode, &
                                        PTUnit(PTUnitNum)%LoopNum, &
                                        PTUnit(PTUnitNum)%LoopSide, &
                                        PTUnit(PTUnitNum)%BranchNum, &
                                        PTUnit(PTUnitNum)%CompNum )
      END IF

      IF(PTUnit(PTUnitNum)%MaxSuppCoilFluidFlow .EQ. Autosize)THEN
         CALL SimulateSteamCoilComponents(PTUnit(PTUnitNum)%SuppHeatCoilName, &
                                          FirstHVACIteration,    &
                                          1.0d0, & !QCoilReq, simulate any load > 0 to get max capacity of steam coil
                                          PTUnit(PTUnitNum)%SuppHeatCoilIndex, QActual)
               CoilMaxVolFlowRate = GetCoilMaxSteamFlowRate(PTUnit(PTUnitNum)%SuppHeatCoilIndex,ErrorsFound)

          IF(CoilMaxVolFlowRate .NE. Autosize) THEN
            SteamIndex = 0 ! Function GetSatDensityRefrig will look up steam index if 0 is passed
            SteamDensity=GetSatDensityRefrig('STEAM',TempSteamIn,1.0d0,SteamIndex,'InitPTUnit')
            PTUnit(PTUnitNum)%MaxSuppCoilFluidFlow = CoilMaxVolFlowRate * SteamDensity
          ENDIF
          CALL InitComponentNodes(0.d0, PTUnit(PTUnitNum)%MaxSuppCoilFluidFlow, &
                                        PTUnit(PTUnitNum)%HWCoilSteamInletNode, &
                                        PTUnit(PTUnitNum)%PlantCoilOutletNode, &
                                        PTUnit(PTUnitNum)%LoopNum, &
                                        PTUnit(PTUnitNum)%LoopSide, &
                                        PTUnit(PTUnitNum)%BranchNum, &
                                        PTUnit(PTUnitNum)%CompNum )
      END IF
    ENDIF
  END IF ! end one time inits

  IF (.NOT. BeginEnvrnFlag) THEN
    MyEnvrnFlag(PTUnitNum) = .TRUE.
  END IF

  IF(PTUnit(PTUnitNum)%ACHeatCoilCap .EQ. AutoSize)THEN
    IF (PTUnit(PTUnitNum)%ACHeatCoilType_Num == Coil_HeatingGas) THEN
      PTUnit(PTUnitNum)%ACHeatCoilCap = GetHeatingCoilCapacity(PTUnit(PTUnitNum)%ACHeatCoilType,  &
                                                        PTUnit(PTUnitNum)%ACHeatCoilName,ErrorsFound)
    ELSEIF (PTUnit(PTUnitNum)%ACHeatCoilType_Num == Coil_HeatingElectric) THEN
      PTUnit(PTUnitNum)%ACHeatCoilCap = GetHeatingCoilCapacity(PTUnit(PTUnitNum)%ACHeatCoilType,  &
                                                        PTUnit(PTUnitNum)%ACHeatCoilName,ErrorsFound)
    END IF
  END IF

! Constant fan systems are tested for ventilation load to determine if load to be met changes.

  IF(PTUnit(PTUnitNum)%OpMode .EQ. ContFanCycCoil .AND. GetCurrentScheduleValue(PTUnit(PTUnitNum)%SchedPtr) .GT. 0.0d0 &
    .AND. ((GetCurrentScheduleValue(PTUnit(PTUnitNum)%FanAvailSchedPtr) .GT. 0.0d0 .OR. &
    ZoneCompTurnFansOn) .AND. .NOT. ZoneCompTurnFansOff))THEN

    SupHeaterLoad = 0.0d0
    IF(PTUnit(PTUnitNum)%NumOfSpeedCooling > 1) THEN
     CALL CalcVarSpeedHeatPump(PTUnitNum,ZoneNum, FirstHVACIteration,off,1,0.0d0,0.0d0,NoCompOutput, LatentOutput, &
                          QZnReq, 0.0d0, OnOffAirFlowRatio,SupHeaterLoad, .FALSE.)
    ELSE
     CALL CalcPTUnit(PTUnitNum, FirstHVACIteration, 0.0d0, NoCompOutput, QZnReq, OnOffAirFlowRatio, SupHeaterLoad, .FALSE.)
    END IF

    QToCoolSetPt=ZoneSysEnergyDemand(ZoneNum)%RemainingOutputReqToCoolSP
    QToHeatSetPt=ZoneSysEnergyDemand(ZoneNum)%RemainingOutputReqToHeatSP

!   If the PTUnit has a net cooling capacity (NoCompOutput < 0) and
!   the zone temp is above the Tstat heating setpoint (QToHeatSetPt < 0)
    IF(NoCompOutput .LT. 0.0d0 .AND. QToHeatSetPt .LT. 0.0d0)THEN
      IF(NoCompOutput .LT. QToHeatSetPt)THEN
!       If the net cooling capacity overshoots the heating setpoint, change mode
        QZnReq       = QToHeatSetPt
        CoolingLoad  = .FALSE.
!       Don't set mode TRUE unless mode is allowed. Also check for floating zone.
        IF(TempControlType(ZoneNum) .EQ. SingleCoolingSetPoint .OR. &
           TempControlType(ZoneNum) .EQ. 0)THEN
          HeatingLoad = .FALSE.
        ELSE
          HeatingLoad = .TRUE.
        END IF
        PartLoadFrac = 1.0d0
        IF(PTUnit(PTUnitNum)%NumOfSpeedCooling > 1) THEN
          !CALL SetOnOffMassFlowRateVSCoil(PTUnitNum,ZoneNum, FirstHVACIteration, &
          !      ZoneEquipConfig(ZoneNum)%AirLoopNum, OnOffAirFlowRatio, PTUnit(PTUnitNum)%OpMode, QZnReq, 0.0d0, PartLoadFrac)
          CALL SetOnOffMassFlowRate(PTUnitNum, PartLoadFrac, OnOffAirFlowRatio)
          CALL CalcVarSpeedHeatPump(PTUnitNum,ZoneNum, FirstHVACIteration,off,1,0.0d0,0.0d0,NoCompOutput, LatentOutput, &
                          QZnReq, 0.0d0, OnOffAirFlowRatio,SupHeaterLoad, .FALSE.)
        ELSE
          CALL SetOnOffMassFlowRate(PTUnitNum, PartLoadFrac, OnOffAirFlowRatio)
          CALL CalcPTUnit(PTUnitNum,FirstHVACIteration,0.0d0,NoCompOutput,QZnReq,OnOffAirFlowRatio,SupHeaterLoad,.FALSE.)
        END IF
        IF(NoCompOutput .GT. QToHeatSetPt)THEN
!         If changing operating mode (flow rates) does not overshoot heating setpoint, turn off coil
          QZnReq       = 0.0d0
          HeatingLoad  = .FALSE.
          PartLoadFrac = 0.0d0
          IF(PTUnit(PTUnitNum)%NumOfSpeedCooling > 1) THEN
              CALL SetOnOffMassFlowRate(PTUnitNum, PartLoadFrac, OnOffAirFlowRatio)
             !CALL SetOnOffMassFlowRateVSCoil(PTUnitNum,ZoneNum, FirstHVACIteration, ZoneEquipConfig(ZoneNum)%AirLoopNum, &
             !       OnOffAirFlowRatio, PTUnit(PTUnitNum)%OpMode, QZnReq, 0.0d0, PartLoadFrac)
          ELSE
             CALL SetOnOffMassFlowRate(PTUnitNum, PartLoadFrac, OnOffAirFlowRatio)
          END IF
        END IF
      ELSE IF(NoCompOutput .LT. QZnReq)THEN
!       If the net cooling capacity meets the zone cooling load but does not overshoot heating set piont, turn off coil
        QZnReq       = 0.0d0
        CoolingLoad  = .FALSE.
        PartLoadFrac = 0.0d0
        IF(PTUnit(PTUnitNum)%NumOfSpeedCooling > 1) THEN
            CALL SetOnOffMassFlowRate(PTUnitNum, PartLoadFrac, OnOffAirFlowRatio)
           !CALL SetOnOffMassFlowRateVSCoil(PTUnitNum,ZoneNum, FirstHVACIteration, ZoneEquipConfig(ZoneNum)%AirLoopNum, &
           !         OnOffAirFlowRatio, PTUnit(PTUnitNum)%OpMode, QZnReq, 0.0d0, PartLoadFrac)
        ELSE
           CALL SetOnOffMassFlowRate(PTUnitNum, PartLoadFrac, OnOffAirFlowRatio)
        END IF
      END IF
    END IF
!   If the furnace has a net heating capacity and the zone temp is below the Tstat cooling setpoint
    IF(NoCompOutput .GT. 0.0d0 .AND. QToCoolSetPt .GT. 0.0d0)THEN
      IF(NoCompOutput .GT. QToCoolSetPt)THEN
        QZnReq       = QToCoolSetPt
!       Don't set mode TRUE unless mode is allowed. Also check for floating zone.
        IF(TempControlType(ZoneNum) .EQ. SingleHeatingSetPoint .OR. &
           TempControlType(ZoneNum) .EQ. 0)THEN
          CoolingLoad = .FALSE.
        ELSE
          CoolingLoad = .TRUE.
        END IF
        HeatingLoad  = .FALSE.
        PartLoadFrac = 1.0d0
        IF(PTUnit(PTUnitNum)%NumOfSpeedCooling > 1) THEN
           CALL SetOnOffMassFlowRate(PTUnitNum, PartLoadFrac, OnOffAirFlowRatio)
          ! CALL SetOnOffMassFlowRateVSCoil(PTUnitNum,ZoneNum, FirstHVACIteration, ZoneEquipConfig(ZoneNum)%AirLoopNum, &
           !     OnOffAirFlowRatio, PTUnit(PTUnitNum)%OpMode, QZnReq, 0.0d0, PartLoadFrac)
           CALL CalcVarSpeedHeatPump(PTUnitNum,ZoneNum, FirstHVACIteration,off,1,0.0d0,0.0d0,NoCompOutput, LatentOutput, &
                          QZnReq, 0.0d0, OnOffAirFlowRatio,SupHeaterLoad, .FALSE.)
        ELSE
          CALL SetOnOffMassFlowRate(PTUnitNum, PartLoadFrac, OnOffAirFlowRatio)
          CALL CalcPTUnit(PTUnitNum, FirstHVACIteration, 0.0d0, NoCompOutput, QZnReq, OnOffAirFlowRatio, SupHeaterLoad, .FALSE.)
        END IF

        IF(NoCompOutput .LT. QToCoolSetPt)THEN
!         If changing operating mode (flow rates) does not overshoot cooling setpoint, turn off coil
          QZnReq       = 0.0d0
          CoolingLoad  = .FALSE.
          PartLoadFrac = 0.0d0
          IF(PTUnit(PTUnitNum)%NumOfSpeedCooling > 1) THEN
            CALL SetOnOffMassFlowRate(PTUnitNum, PartLoadFrac, OnOffAirFlowRatio)
             !CALL SetOnOffMassFlowRateVSCoil(PTUnitNum,ZoneNum, FirstHVACIteration, ZoneEquipConfig(ZoneNum)%AirLoopNum, &
              !      OnOffAirFlowRatio, PTUnit(PTUnitNum)%OpMode, QZnReq, 0.0d0, PartLoadFrac)
          ELSE
             CALL SetOnOffMassFlowRate(PTUnitNum, PartLoadFrac, OnOffAirFlowRatio)
          END IF
        END IF
      ELSE IF(NoCompOutput .GT. QZnReq)THEN
!       If the net heating capacity meets the zone heating load but does not overshoot, turn off coil
        QZnReq       = 0.0d0
        HeatingLoad = .FALSE.
        PartLoadFrac = 0.0d0
        IF(PTUnit(PTUnitNum)%NumOfSpeedCooling > 1) THEN
            CALL SetOnOffMassFlowRate(PTUnitNum, PartLoadFrac, OnOffAirFlowRatio)
           !CALL SetOnOffMassFlowRateVSCoil(PTUnitNum,ZoneNum, FirstHVACIteration, ZoneEquipConfig(ZoneNum)%AirLoopNum, &
           !     OnOffAirFlowRatio, PTUnit(PTUnitNum)%OpMode, QZnReq, 0.0d0, PartLoadFrac)
        ELSE
           CALL SetOnOffMassFlowRate(PTUnitNum, PartLoadFrac, OnOffAirFlowRatio)
        END IF
      END IF
    END IF
    ZoneLoad = QZnReq
  END IF

! get operating capacity of water and steam coil (dependent on entering water/steam temperature)
  IF(FirstHVACIteration .AND. PartLoadFrac > 0.0d0) THEN

    IF(PTUnit(PTUnitNum)%ACHeatCoilType_Num == Coil_HeatingWater) THEN

!     set water-side mass flow rates
      Node(PTUnit(PTUnitNum)%HWCoilAirInletNode)%MassFlowRate = CompOnMassFlow

      mdot = PTUnit(PTUnitNum)%MaxHeatCoilFluidFlow

      CALL SetComponentFlowRate(mdot, &
                                   PTUnit(PTUnitNum)%HotWaterControlNode, &
                                   PTUnit(PTUnitNum)%PlantCoilOutletNode, &
                                   PTUnit(PTUnitNum)%LoopNum, &
                                   PTUnit(PTUnitNum)%LoopSide, &
                                   PTUnit(PTUnitNum)%BranchNum, &
                                   PTUnit(PTUnitNum)%CompNum )

!     simulate water coil to find operating capacity
      CALL SimulateWaterCoilComponents(PTUnit(PTUnitNum)%ACHeatCoilName,FirstHVACIteration, &
                                       PTUnit(PTUnitNum)%ACHeatCoilIndex, QActual)
      PTUnit(PTUnitNum)%ACHeatCoilCap = QActual

    END IF ! from IF(PTUnit(PTUnitNum)%ACHeatCoilType_Num == Coil_HeatingWater) THEN

    IF(PTUnit(PTUnitNum)%ACHeatCoilType_Num == Coil_HeatingSteam) THEN

!     set air-side and steam-side mass flow rates
      Node(PTUnit(PTUnitNum)%HWCoilAirInletNode)%MassFlowRate = CompOnMassFlow

      mdot = PTUnit(PTUnitNum)%MaxHeatCoilFluidFlow
      CALL SetComponentFlowRate(mdot, &
                                   PTUnit(PTUnitNum)%HWCoilSteamInletNode, &
                                   PTUnit(PTUnitNum)%PlantCoilOutletNode, &
                                   PTUnit(PTUnitNum)%LoopNum, &
                                   PTUnit(PTUnitNum)%LoopSide, &
                                   PTUnit(PTUnitNum)%BranchNum, &
                                   PTUnit(PTUnitNum)%CompNum )

!     simulate steam coil to find operating capacity
      CALL SimulateSteamCoilComponents(PTUnit(PTUnitNum)%ACHeatCoilName, &
                                       FirstHVACIteration,    &
                                       1.0d0, & !QCoilReq, simulate any load > 0 to get max capacity of steam coil
                                       PTUnit(PTUnitNum)%ACHeatCoilIndex, QActual)
      PTUnit(PTUnitNum)%ACHeatCoilCap = GetSteamCoilCapacity(PTUnit(PTUnitNum)%ACHeatCoilType, &
                                                                    PTUnit(PTUnitNum)%ACHeatCoilName,ErrorsFound)

    END IF ! from IF(PTUnit(PTUnitNum)%ACHeatCoilType_Num == Coil_HeatingSteam) THEN

    IF(PTUnit(PTUnitNum)%SuppHeatCoilType_Num == Coil_HeatingWater) THEN

      !     set air-side and steam-side mass flow rates
      Node(PTUnit(PTUnitNum)%SupCoilAirInletNode)%MassFlowRate = CompOnMassFlow
      mdot = PTUnit(PTUnitNum)%MaxSuppCoilFluidFlow
      CALL SetComponentFlowRate(mdot, &
                                   PTUnit(PTUnitNum)%HotWaterControlNode, &
                                   PTUnit(PTUnitNum)%PlantCoilOutletNode, &
                                   PTUnit(PTUnitNum)%LoopNum, &
                                   PTUnit(PTUnitNum)%LoopSide, &
                                   PTUnit(PTUnitNum)%BranchNum, &
                                   PTUnit(PTUnitNum)%CompNum )

      !     simulate water coil to find operating capacity
      CALL SimulateWaterCoilComponents(PTUnit(PTUnitNum)%SuppHeatCoilName,FirstHVACIteration, &
                                       PTUnit(PTUnitNum)%SuppHeatCoilIndex, QActual)
      PTUnit(PTUnitNum)%SupHeatCoilCap = QActual

    END IF ! from IF(PTUnit(PTUnitNum)%SuppHeatCoilType_Num == Coil_HeatingWater) THEN
    IF(PTUnit(PTUnitNum)%SuppHeatCoilType_Num == Coil_HeatingSteam) THEN

      !     set air-side and steam-side mass flow rates
      Node(PTUnit(PTUnitNum)%SupCoilAirInletNode)%MassFlowRate = CompOnMassFlow
      mdot = PTUnit(PTUnitNum)%MaxSuppCoilFluidFlow
      CALL SetComponentFlowRate(mdot, &
                                   PTUnit(PTUnitNum)%HWCoilSteamInletNode, &
                                   PTUnit(PTUnitNum)%PlantCoilOutletNode, &
                                   PTUnit(PTUnitNum)%LoopNum, &
                                   PTUnit(PTUnitNum)%LoopSide, &
                                   PTUnit(PTUnitNum)%BranchNum, &
                                   PTUnit(PTUnitNum)%CompNum )

!     simulate steam coil to find operating capacity
      CALL SimulateSteamCoilComponents(PTUnit(PTUnitNum)%SuppHeatCoilName, &
                                       FirstHVACIteration,    &
                                       1.0d0, & !QCoilReq, simulate any load > 0 to get max capacity of steam coil
                                       PTUnit(PTUnitNum)%SuppHeatCoilIndex, QActual)
      PTUnit(PTUnitNum)%SupHeatCoilCap = GetSteamCoilCapacity(PTUnit(PTUnitNum)%SuppHeatCoilType, &
                                                              PTUnit(PTUnitNum)%SuppHeatCoilName,ErrorsFound)

    END IF ! from IF(PTUnit(PTUnitNum)%SuppHeatCoilType_Num == Coil_HeatingSteam) THEN
  END IF ! from IF(FirstHVACIteration .AND. PartLoadFrac > 0.0) THEN

  CALL SetAverageAirFlow(PTUnitNum, PartLoadFrac, OnOffAirFlowRatio)

  RETURN
END SUBROUTINE InitPTUnit

SUBROUTINE SetOnOffMassFlowRate(PTUnitNum, PartLoadFrac, OnOffAirFlowRatio)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Raustad
          !       DATE WRITTEN   November 2008
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is for initializations of the operating flow rates.

          ! METHODOLOGY EMPLOYED:
          ! Set cooling or heating and no cooling or heating flow rate.
          ! Set mass flow rate using PLR and call to Subroutine SetAverageAirFlow.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER,   INTENT (IN)    :: PTUnitNum          ! number of the current PTHP unit being simulated
  REAL(r64), INTENT (IN)    :: PartLoadFrac       ! coil operating part-load ratio
  REAL(r64), INTENT (INOUT) :: OnOffAirFlowRatio  ! ratio of coil on to coil off air flow rate

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          ! na

  ! Set the operating air mass flow rate
  IF (PTUnit(PTUnitNum)%OpMode .EQ. ContFanCycCoil) THEN
  ! constant fan mode
    IF (HeatingLoad) THEN
      CompOnMassFlow = PTUnit(PTUnitNum)%MaxHeatAirMassFlow
      CompOnFlowRatio = PTUnit(PTUnitNum)%HeatingSpeedRatio
      OACompOnMassFlow   = PTUnit(PTUnitNum)%HeatOutAirMassFlow
      PTUnit(PTUnitNum)%LastMode = HeatingMode
    ELSE IF (CoolingLoad) THEN
      CompOnMassFlow = PTUnit(PTUnitNum)%MaxCoolAirMassFlow
      CompOnFlowRatio = PTUnit(PTUnitNum)%CoolingSpeedRatio
      OACompOnMassFlow   = PTUnit(PTUnitNum)%CoolOutAirMassFlow
      PTUnit(PTUnitNum)%LastMode = CoolingMode
    ELSE
      CompOnMassFlow = PTUnit(PTUnitNum)%MaxNoCoolHeatAirMassFlow
      CompOnFlowRatio = PTUnit(PTUnitNum)%NoHeatCoolSpeedRatio
      OACompOnMassFlow   = PTUnit(PTUnitNum)%NoCoolHeatOutAirMassFlow
    END IF

    IF (PTUnit(PTUnitNum)%AirFlowControl .EQ. UseCompressorOnFlow) THEN
      IF (PTUnit(PTUnitNum)%LastMode .EQ. HeatingMode) THEN
        CompOffMassFlow = PTUnit(PTUnitNum)%MaxHeatAirMassFlow
        CompOffFlowRatio = PTUnit(PTUnitNum)%HeatingSpeedRatio
        OACompOffMassFlow   = PTUnit(PTUnitNum)%HeatOutAirMassFlow
      ELSE
        CompOffMassFlow = PTUnit(PTUnitNum)%MaxCoolAirMassFlow
        CompOffFlowRatio = PTUnit(PTUnitNum)%CoolingSpeedRatio
        OACompOffMassFlow   = PTUnit(PTUnitNum)%CoolOutAirMassFlow
      END IF
    ELSE
      CompOffMassFlow = PTUnit(PTUnitNum)%MaxNoCoolHeatAirMassFlow
      CompOffFlowRatio = PTUnit(PTUnitNum)%NoHeatCoolSpeedRatio
      OACompOffMassFlow   = PTUnit(PTUnitNum)%NoCoolHeatOutAirMassFlow
    END IF
  ELSE
  ! cycling fan mode
    IF (HeatingLoad) THEN
      CompOnMassFlow = PTUnit(PTUnitNum)%MaxHeatAirMassFlow
      CompOnFlowRatio = PTUnit(PTUnitNum)%HeatingSpeedRatio
      OACompOnMassFlow   = PTUnit(PTUnitNum)%HeatOutAirMassFlow
    ELSE IF (CoolingLoad) THEN
      CompOnMassFlow = PTUnit(PTUnitNum)%MaxCoolAirMassFlow
      CompOnFlowRatio = PTUnit(PTUnitNum)%CoolingSpeedRatio
      OACompOnMassFlow   = PTUnit(PTUnitNum)%CoolOutAirMassFlow
    ELSE
      CompOnMassFlow = 0.0d0
      CompOnFlowRatio = 0.0d0
      OACompOnMassFlow = 0.0d0
    END IF
    CompOffMassFlow = 0.0d0
    CompOffFlowRatio = 0.0d0
    OACompOffMassFlow = 0.0d0
  END IF

  CALL SetAverageAirFlow(PTUnitNum, PartLoadFrac, OnOffAirFlowRatio)

  RETURN
END SUBROUTINE SetOnOffMassFlowRate


SUBROUTINE SizePTUnit(PTUnitNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Raustad
          !       DATE WRITTEN   July 2005
          !       MODIFIED       Bo Shen, ORNL, March 2012, added variable-speed water-source heat pump
          !                      August 2013 Daeho Kang, add component sizing table entries
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is for sizing packaged terminal heat pump components for which flow rates have not been
          ! specified in the input.

          ! METHODOLOGY EMPLOYED:
          ! Obtains flow rates from the zone sizing arrays. ParentCoolAirFlowSizing and ParentHeatAirFlowSizing
          ! arrays are used to pass volumetric flow rates to child objects when zone sizing array values are overridden.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE WaterCoils,     ONLY: SetCoilDesFlow
  USE ReportSizingManager, ONLY: ReportSizingOutput
  USE General,             ONLY: RoundSigDigits

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine
  LOGICAL :: IsAutosize                    ! Indicator to autosize
  REAL(r64) :: MaxCoolAirVolFlowDes        ! Autosized cooling air flow for reporting
  REAL(r64) :: MaxCoolAirVolFlowUser       ! Hardsizedcooling air flow for reporting
  REAL(r64) :: MaxHeatAirVolFlowDes        ! Autosized heating air flow for reporting
  REAL(r64) :: MaxHeatAirVolFlowUser       ! Hardsized heating air flow for reporting
  REAL(r64) :: MaxNoCoolHeatAirVolFlowDes  ! Autosized maximum air flow when unconditioned for reporting
  REAL(r64) :: MaxNoCoolHeatAirVolFlowUser ! Hardsized maximum air flow when unconditioned for reporting
  REAL(r64) :: CoolOutAirVolFlowDes        ! Autosized cooling outdoor air flow for reporting
  REAL(r64) :: CoolOutAirVolFlowUser       ! Hardsized cooling outdoor air flow for reporting
  REAL(r64) :: HeatOutAirVolFlowDes        ! Autosized heating outdoor air flow for reporting
  REAL(r64) :: HeatOutAirVolFlowUser       ! Hardsized heating outdoor air flow for reporting
  REAL(r64) :: NoCoolHeatOutAirVolFlowDes  ! Autosized outdoor air flow when unconditioned for reporting
  REAL(r64) :: NoCoolHeatOutAirVolFlowUser ! Hardsized outdoor air flow when unconditioned for reporting
  REAL(r64) :: MaxSATSupHeatDes            ! Autosized supply air temperature of supplemental heater for reporting
  REAL(r64) :: MaxSATSupHeatUser           ! Hardsized supply air temperature of supplemental heater for reporting

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: PTUnitNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  LOGICAL :: ErrorsFound
  LOGICAL :: SizingDesRunThisZone              ! true if a particular zone had a Sizing:Zone object and zone sizing was done

  ErrorsFound=.false.
  IsAutosize = .FALSE.
  MaxCoolAirVolFlowDes = 0.0d0
  MaxCoolAirVolFlowUser = 0.0d0
  MaxHeatAirVolFlowDes = 0.0d0
  MaxHeatAirVolFlowUser = 0.0d0
  MaxNoCoolHeatAirVolFlowDes = 0.0d0
  MaxNoCoolHeatAirVolFlowUser = 0.0d0
  CoolOutAirVolFlowDes = 0.0d0
  CoolOutAirVolFlowUser = 0.0d0
  HeatOutAirVolFlowDes = 0.0d0
  HeatOutAirVolFlowUser = 0.0d0
  NoCoolHeatOutAirVolFlowDes = 0.0d0
  NoCoolHeatOutAirVolFlowUser = 0.0d0
  MaxSATSupHeatDes = 0.0d0
  MaxSATSupHeatUser = 0.0d0

  IF (PTUnit(PTUnitNum)%MaxCoolAirVolFlow == AutoSize) THEN
    IsAutosize = .TRUE.
  END IF
  IF (CurZoneEqNum > 0) THEN
    CALL CheckThisZoneForSizing(CurZoneEqNum, SizingDesRunThisZone)
    IF (.NOT. IsAutosize .AND. .NOT. SizingDesRunThisZone) THEN ! Simulation continue
      IF (PTUnit(PTUnitNum)%MaxCoolAirVolFlow > 0.0d0) THEN
        CALL ReportSizingOutput(PTUnit(PTUnitNum)%UnitType, PTUnit(PTUnitNum)%Name, &
                     'User-Specified Supply Air Flow Rate During Cooling Operation [m3/s]',  &
                     PTUnit(PTUnitNum)%MaxCoolAirVolFlow)
      END IF
    ELSE
      CALL CheckZoneSizing(PTUnit(PTUnitNum)%UnitType, PTUnit(PTUnitNum)%Name)
      MaxCoolAirVolFlowDes = MAX(FinalZoneSizing(CurZoneEqNum)%DesCoolVolFlow, &
                             FinalZoneSizing(CurZoneEqNum)%DesHeatVolFlow)
      IF (MaxCoolAirVolFlowDes < SmallAirVolFlow) THEN
        MaxCoolAirVolFlowDes = 0.0d0
      END IF
      IF (IsAutosize) THEN
        PTUnit(PTUnitNum)%MaxCoolAirVolFlow = MaxCoolAirVolFlowDes
        CALL ReportSizingOutput(PTUnit(PTUnitNum)%UnitType, PTUnit(PTUnitNum)%Name, &
                     'Design Size Supply Air Flow Rate During Cooling Operation [m3/s]', MaxCoolAirVolFlowDes)
      ELSE
        IF (PTUnit(PTUnitNum)%MaxCoolAirVolFlow > 0.0d0 .AND. MaxCoolAirVolFlowDes > 0.0d0 .AND. SizingDesRunThisZone) THEN
          MaxCoolAirVolFlowUser = PTUnit(PTUnitNum)%MaxCoolAirVolFlow
          CALL ReportSizingOutput(PTUnit(PTUnitNum)%UnitType, PTUnit(PTUnitNum)%Name, &
                     'Design Size Supply Air Flow Rate During Cooling Operation [m3/s]', MaxCoolAirVolFlowDes, &
                     'User-Specified Supply Air Flow Rate During Cooling Operation [m3/s]', MaxCoolAirVolFlowUser)
          IF (DisplayExtraWarnings) THEN
          IF ((ABS(MaxCoolAirVolFlowDes - MaxCoolAirVolFlowUser)/MaxCoolAirVolFlowUser) > AutoVsHardSizingThreshold) THEN
            CALL ShowMessage('SizePTUnit: Potential issue with equipment sizing for '&
                                  //TRIM(PTUnit(PTUnitNum)%UnitType)//' '//TRIM(PTUnit(PTUnitNum)%Name))
            CALL ShowContinueError('User-Specified Supply Air Flow Rate During Cooling Operation of '// &
                                    TRIM(RoundSigDigits(MaxCoolAirVolFlowUser,5))// ' [m3/s]')
            CALL ShowContinueError('differs from Design Size Supply Air Flow Rate During Cooling Operation of ' // &
                                    TRIM(RoundSigDigits(MaxCoolAirVolFlowDes,5))// ' [m3/s]')
            CALL ShowContinueError('This may, or may not, indicate mismatched component sizes.')
            CALL ShowContinueError('Verify that the value entered is intended and is consistent with other components.')
          END IF
          ENDIF
        END IF
      END IF
    END IF
  END IF

  IsAutosize = .FALSE.
  IF (PTUnit(PTUnitNum)%MaxHeatAirVolFlow == AutoSize) THEN
    IsAutosize = .TRUE.
  END IF
  IF (CurZoneEqNum > 0) THEN
    CALL CheckThisZoneForSizing(CurZoneEqNum, SizingDesRunThisZone)
    IF (.NOT. IsAutosize .AND. .NOT. SizingDesRunThisZone) THEN ! Simulation continue
      IF (PTUnit(PTUnitNum)%MaxHeatAirVolFlow > 0.0d0) THEN
        CALL ReportSizingOutput(PTUnit(PTUnitNum)%UnitType, PTUnit(PTUnitNum)%Name, &
                     'User-Specified Supply Air Flow Rate During Heating Operation [m3/s]',  &
                     PTUnit(PTUnitNum)%MaxHeatAirVolFlow)
      END IF
    ELSE
      CALL CheckZoneSizing(PTUnit(PTUnitNum)%UnitType, PTUnit(PTUnitNum)%Name)
      MaxHeatAirVolFlowDes = MAX(FinalZoneSizing(CurZoneEqNum)%DesCoolVolFlow, &
                                            FinalZoneSizing(CurZoneEqNum)%DesHeatVolFlow)
      IF (MaxHeatAirVolFlowDes < SmallAirVolFlow) THEN
        MaxHeatAirVolFlowDes = 0.0d0
      END IF
      IF (IsAutosize) THEN
        PTUnit(PTUnitNum)%MaxHeatAirVolFlow = MaxHeatAirVolFlowDes
        CALL ReportSizingOutput(PTUnit(PTUnitNum)%UnitType, PTUnit(PTUnitNum)%Name, &
                     'Design Size Supply Air Flow Rate During Heating Operation [m3/s]', MaxHeatAirVolFlowDes)
      ELSE
        IF (PTUnit(PTUnitNum)%MaxHeatAirVolFlow > 0.0d0 .AND. MaxHeatAirVolFlowDes > 0.0d0 .AND. SizingDesRunThisZone) THEN
          MaxHeatAirVolFlowUser = PTUnit(PTUnitNum)%MaxHeatAirVolFlow
          CALL ReportSizingOutput(PTUnit(PTUnitNum)%UnitType, PTUnit(PTUnitNum)%Name, &
                     'Design Size Supply Air Flow Rate During Heating Operation [m3/s]', MaxHeatAirVolFlowDes, &
                     'User-Specified Supply Air Flow Rate During Heating Operation [m3/s]', MaxHeatAirVolFlowUser)
          IF (DisplayExtraWarnings) THEN
            IF ((ABS(MaxHeatAirVolFlowDes - MaxHeatAirVolFlowUser)/ MaxHeatAirVolFlowUser) > AutoVsHardSizingThreshold) THEN
              CALL ShowMessage('SizePTUnit: Potential issue with equipment sizing for ' &
                                   //TRIM(PTUnit(PTUnitNum)%UnitType)//' '//TRIM(PTUnit(PTUnitNum)%Name))
              CALL ShowContinueError('User-Specified Supply Air Flow Rate During Heating Operation of '// &
                                      TRIM(RoundSigDigits(MaxHeatAirVolFlowUser,5))// ' [m3/s]')
              CALL ShowContinueError('differs from Design Size Supply Air Flow Rate During Heating Operation of ' // &
                                      TRIM(RoundSigDigits(MaxHeatAirVolFlowDes,5))// ' [m3/s]')
              CALL ShowContinueError('This may, or may not, indicate mismatched component sizes.')
              CALL ShowContinueError('Verify that the value entered is intended and is consistent with other components.')
            END IF
          ENDIF
        END IF
      END IF
    END IF
  END IF

  IsAutosize = .FALSE.
  IF (PTUnit(PTUnitNum)%MaxNoCoolHeatAirVolFlow == AutoSize) THEN
    IsAutosize = .TRUE.
  END IF

  IF (CurZoneEqNum > 0) THEN
    CALL CheckThisZoneForSizing(CurZoneEqNum, SizingDesRunThisZone)
    IF (.NOT. IsAutosize .AND. .NOT. SizingDesRunThisZone) THEN ! Simulation continue
      IF (PTUnit(PTUnitNum)%MaxNoCoolHeatAirVolFlow > 0.0d0) THEN
        CALL ReportSizingOutput(PTUnit(PTUnitNum)%UnitType, PTUnit(PTUnitNum)%Name, &
                     'User-Specified Supply Air Flow Rate When No Cooling or Heating is Needed [m3/s]',  &
                     PTUnit(PTUnitNum)%MaxNoCoolHeatAirVolFlow)
      END IF
    ELSE
      CALL CheckZoneSizing(PTUnit(PTUnitNum)%UnitType, PTUnit(PTUnitNum)%Name)
      MaxNoCoolHeatAirVolFlowDes = MIN(PTUnit(PTUnitNum)%MaxCoolAirVolFlow, &
                                                  PTUnit(PTUnitNum)%MaxHeatAirVolFlow)
      IF (MaxNoCoolHeatAirVolFlowDes < SmallAirVolFlow) THEN
        MaxNoCoolHeatAirVolFlowDes = 0.0d0
      END IF
      IF (IsAutosize) THEN
        PTUnit(PTUnitNum)%MaxNoCoolHeatAirVolFlow = MaxNoCoolHeatAirVolFlowDes
        CALL ReportSizingOutput(PTUnit(PTUnitNum)%UnitType, PTUnit(PTUnitNum)%Name, &
                        'Design Size Supply Air Flow Rate When No Cooling or Heating is Needed [m3/s]', &
                        MaxNoCoolHeatAirVolFlowDes)
      ELSE
        IF (PTUnit(PTUnitNum)%MaxNoCoolHeatAirVolFlow > 0.0d0 .AND. MaxNoCoolHeatAirVolFlowDes > 0.0d0 &
                      .AND. SizingDesRunThisZone) THEN
          MaxNoCoolHeatAirVolFlowUser = PTUnit(PTUnitNum)%MaxNoCoolHeatAirVolFlow
          CALL ReportSizingOutput(PTUnit(PTUnitNum)%UnitType, PTUnit(PTUnitNum)%Name, &
                        'Design Size Supply Air Flow Rate When No Cooling or Heating is Needed [m3/s]', &
                        MaxNoCoolHeatAirVolFlowDes, &
                        'User-Specified Supply Air Flow Rate When No Cooling or Heating is Needed [m3/s]', &
                        MaxNoCoolHeatAirVolFlowUser)
          IF (DisplayExtraWarnings) THEN
            IF ((ABS(MaxNoCoolHeatAirVolFlowDes - MaxNoCoolHeatAirVolFlowUser)/MaxNoCoolHeatAirVolFlowUser) &
                                             > AutoVsHardSizingThreshold) THEN
              CALL ShowMessage('SizePTUnit: Potential issue with equipment sizing for ' &
                                    //TRIM(PTUnit(PTUnitNum)%UnitType)//' '//TRIM(PTUnit(PTUnitNum)%Name))
              CALL ShowContinueError('User-Specified Supply Air Flow Rate When No Cooling or Heating is Needed of '// &
                                      TRIM(RoundSigDigits(MaxNoCoolHeatAirVolFlowUser,5))// ' [m3/s]')
              CALL ShowContinueError('differs from Design Size Supply Air Flow Rate When No Cooling or Heating is Needed of ' // &
                                      TRIM(RoundSigDigits(MaxNoCoolHeatAirVolFlowDes,5))// ' [m3/s]')
              CALL ShowContinueError('This may, or may not, indicate mismatched component sizes.')
              CALL ShowContinueError('Verify that the value entered is intended and is consistent with other components.')
            END IF
          ENDIF
        END IF
      END IF
    END IF
  END IF

  IsAutosize = .FALSE.
  IF (PTUnit(PTUnitNum)%CoolOutAirVolFlow == AutoSize) THEN
    IsAutosize = .TRUE.
  END IF
  IF (CurZoneEqNum > 0) THEN
    CALL CheckThisZoneForSizing(CurZoneEqNum, SizingDesRunThisZone)
    IF (.NOT. IsAutosize .AND. .NOT. SizingDesRunThisZone) THEN ! Simulation continue
      IF (PTUnit(PTUnitNum)%CoolOutAirVolFlow > 0.0d0) THEN
        CALL ReportSizingOutput(PTUnit(PTUnitNum)%UnitType, PTUnit(PTUnitNum)%Name, &
                     'User-Specified Outdoor Air Flow Rate During Cooling Operation [m3/s]',  &
                     PTUnit(PTUnitNum)%CoolOutAirVolFlow)
      END IF
    ELSE
      CALL CheckZoneSizing(PTUnit(PTUnitNum)%UnitType, PTUnit(PTUnitNum)%Name)
      CoolOutAirVolFlowDes = MIN(FinalZoneSizing(CurZoneEqNum)%MinOA,PTUnit(PTUnitNum)%MaxCoolAirVolFlow)
      IF (CoolOutAirVolFlowDes < SmallAirVolFlow) THEN
        CoolOutAirVolFlowDes = 0.0d0
      END IF
      IF (IsAutosize) THEN
        PTUnit(PTUnitNum)%CoolOutAirVolFlow = CoolOutAirVolFlowDes
        CALL ReportSizingOutput(PTUnit(PTUnitNum)%UnitType, PTUnit(PTUnitNum)%Name, &
                        'Design Size Outdoor Air Flow Rate During Cooling Operation [m3/s]', &
                        CoolOutAirVolFlowDes)
      ELSE
        IF (PTUnit(PTUnitNum)%CoolOutAirVolFlow > 0.0d0 .AND. CoolOutAirVolFlowDes > 0.0d0 .AND. SizingDesRunThisZone) THEN
          CoolOutAirVolFlowUser = PTUnit(PTUnitNum)%CoolOutAirVolFlow
          CALL ReportSizingOutput(PTUnit(PTUnitNum)%UnitType, PTUnit(PTUnitNum)%Name, &
                        'Design Size Outdoor Air Flow Rate During Cooling Operation [m3/s]', &
                        CoolOutAirVolFlowDes, &
                        'User-Specified Outdoor Air Flow Rate During Cooling Operation [m3/s]', &
                        CoolOutAirVolFlowUser)
          IF (DisplayExtraWarnings) THEN
            IF ((ABS(CoolOutAirVolFlowDes - CoolOutAirVolFlowUser)/CoolOutAirVolFlowUser) > AutoVsHardSizingThreshold) THEN
              CALL ShowMessage('SizePTUnit: Potential issue with equipment sizing for ' &
                                    //TRIM(PTUnit(PTUnitNum)%UnitType)//' '//TRIM(PTUnit(PTUnitNum)%Name))
              CALL ShowContinueError('User-Specified Outdoor Air Flow Rate During Cooling Operation of '// &
                                      TRIM(RoundSigDigits(CoolOutAirVolFlowUser,5))// ' [m3/s]')
              CALL ShowContinueError('differs from Design Size Outdoor Air Flow Rate During Cooling Operation of ' // &
                                      TRIM(RoundSigDigits(CoolOutAirVolFlowDes,5))// ' [m3/s]')
              CALL ShowContinueError('This may, or may not, indicate mismatched component sizes.')
              CALL ShowContinueError('Verify that the value entered is intended and is consistent with other components.')
            END IF
          ENDIF
        END IF
      END IF
    END IF
  END IF

  IsAutosize = .FALSE.
  IF (PTUnit(PTUnitNum)%HeatOutAirVolFlow == AutoSize) THEN
    IsAutosize = .TRUE.
  END IF
  IF (CurZoneEqNum > 0) THEN
    CALL CheckThisZoneForSizing(CurZoneEqNum, SizingDesRunThisZone)
    IF (.NOT. IsAutosize .AND. .NOT. SizingDesRunThisZone) THEN ! Simulation continue
      IF (PTUnit(PTUnitNum)%HeatOutAirVolFlow > 0.0d0) THEN
        CALL ReportSizingOutput(PTUnit(PTUnitNum)%UnitType, PTUnit(PTUnitNum)%Name, &
                     'User-Specified Supply Air Flow Rate During Heating Operation [m3/s]',  &
                     PTUnit(PTUnitNum)%HeatOutAirVolFlow)
      END IF
    ELSE
      CALL CheckZoneSizing(PTUnit(PTUnitNum)%UnitType, PTUnit(PTUnitNum)%Name)
      HeatOutAirVolFlowDes = MIN(FinalZoneSizing(CurZoneEqNum)%MinOA,PTUnit(PTUnitNum)%MaxHeatAirVolFlow)
      IF (HeatOutAirVolFlowDes < SmallAirVolFlow) THEN
        HeatOutAirVolFlowDes = 0.0d0
      END IF
      IF (IsAutosize) THEN
        PTUnit(PTUnitNum)%HeatOutAirVolFlow = HeatOutAirVolFlowDes
        CALL ReportSizingOutput(PTUnit(PTUnitNum)%UnitType, PTUnit(PTUnitNum)%Name, &
                        'Design Size Outdoor Air Flow Rate During Heating Operation [m3/s]', &
                        HeatOutAirVolFlowDes)
      ELSE
        IF (PTUnit(PTUnitNum)%HeatOutAirVolFlow > 0.0d0 .AND. HeatOutAirVolFlowDes > 0.0d0 .AND. SizingDesRunThisZone) THEN
          HeatOutAirVolFlowUser = PTUnit(PTUnitNum)%HeatOutAirVolFlow
          CALL ReportSizingOutput(PTUnit(PTUnitNum)%UnitType, PTUnit(PTUnitNum)%Name, &
                        'Design Size Outdoor Air Flow Rate During Heating Operation [m3/s]', &
                        HeatOutAirVolFlowDes, &
                        'User-Specified Outdoor Air Flow Rate During Heating Operation [m3/s]', &
                        HeatOutAirVolFlowUser)
          IF (DisplayExtraWarnings) THEN
            IF ((ABS(HeatOutAirVolFlowDes - HeatOutAirVolFlowUser)/HeatOutAirVolFlowUser) > AutoVsHardSizingThreshold) THEN
              CALL ShowMessage('SizePTUnit: Potential issue with equipment sizing for ' &
                                     //TRIM(PTUnit(PTUnitNum)%UnitType)//' '//TRIM(PTUnit(PTUnitNum)%Name))
              CALL ShowContinueError('User-Specified Outdoor Air Flow Rate During Heating Operation of '// &
                                      TRIM(RoundSigDigits(HeatOutAirVolFlowUser,5))// ' [m3/s]')
              CALL ShowContinueError('differs from Design Size Outdoor Air Flow Rate During Heating Operation of ' // &
                                      TRIM(RoundSigDigits(HeatOutAirVolFlowDes,5))// ' [m3/s]')
              CALL ShowContinueError('This may, or may not, indicate mismatched component sizes.')
              CALL ShowContinueError('Verify that the value entered is intended and is consistent with other components.')
            END IF
          ENDIF
        END IF
      END IF
    END IF
  END IF

  IsAutosize = .FALSE.
  IF (PTUnit(PTUnitNum)%NoCoolHeatOutAirVolFlow == AutoSize) THEN
    IsAutosize = .TRUE.
  END IF
  IF (CurZoneEqNum > 0) THEN
    CALL CheckThisZoneForSizing(CurZoneEqNum, SizingDesRunThisZone)
    IF (.NOT. IsAutosize .AND. .NOT. SizingDesRunThisZone) THEN ! Simulation continue
      IF (PTUnit(PTUnitNum)%NoCoolHeatOutAirVolFlow > 0.0d0) THEN
        CALL ReportSizingOutput(PTUnit(PTUnitNum)%UnitType, PTUnit(PTUnitNum)%Name, &
                     'User-Specified Outdoor Air Flow Rate When No Cooling or Heating is Needed [m3/s]',  &
                     PTUnit(PTUnitNum)%NoCoolHeatOutAirVolFlow)
      END IF
    ELSE
      CALL CheckZoneSizing(PTUnit(PTUnitNum)%UnitType, PTUnit(PTUnitNum)%Name)
      NoCoolHeatOutAirVolFlowDes = MIN(FinalZoneSizing(CurZoneEqNum)%MinOA,PTUnit(PTUnitNum)%MaxNoCoolHeatAirVolFlow)
      IF (NoCoolHeatOutAirVolFlowDes < SmallAirVolFlow) THEN
        NoCoolHeatOutAirVolFlowDes = 0.0d0
      END IF
      IF (IsAutosize) THEN
        PTUnit(PTUnitNum)%NoCoolHeatOutAirVolFlow = NoCoolHeatOutAirVolFlowDes
        CALL ReportSizingOutput(PTUnit(PTUnitNum)%UnitType, PTUnit(PTUnitNum)%Name, &
                        'Design Size Outdoor Air Flow Rate When No Cooling or Heating is Needed [m3/s]', &
                        NoCoolHeatOutAirVolFlowDes)
      ELSE
        IF (PTUnit(PTUnitNum)%NoCoolHeatOutAirVolFlow > 0.0d0 .AND. NoCoolHeatOutAirVolFlowDes > 0.0d0 &
                       .AND. SizingDesRunThisZone) THEN
          NoCoolHeatOutAirVolFlowUser = PTUnit(PTUnitNum)%NoCoolHeatOutAirVolFlow
          CALL ReportSizingOutput(PTUnit(PTUnitNum)%UnitType, PTUnit(PTUnitNum)%Name, &
                        'Design Size Outdoor Air Flow Rate When No Cooling or Heating is Needed [m3/s]', &
                        NoCoolHeatOutAirVolFlowDes, &
                        'User-Specified Outdoor Air Flow Rate When No Cooling or Heating is Needed [m3/s]', &
                        NoCoolHeatOutAirVolFlowUser)
          IF (DisplayExtraWarnings) THEN
            IF ((ABS(NoCoolHeatOutAirVolFlowDes - NoCoolHeatOutAirVolFlowUser)/NoCoolHeatOutAirVolFlowUser) &
                           > AutoVsHardSizingThreshold) THEN
              CALL ShowMessage('SizePTUnit: Potential issue with equipment sizing for ' &
                                      //TRIM(PTUnit(PTUnitNum)%UnitType)//' '//TRIM(PTUnit(PTUnitNum)%Name))
              CALL ShowContinueError('User-Specified Outdoor Air Flow Rate When No Cooling or Heating is Needed of '// &
                                      TRIM(RoundSigDigits(NoCoolHeatOutAirVolFlowUser,5))// ' [m3/s]')
              CALL ShowContinueError('differs from Design Size Outdoor Air Flow Rate When No Cooling or Heating is Needed of '// &
                                      TRIM(RoundSigDigits(NoCoolHeatOutAirVolFlowDes,5))// ' [m3/s]')
              CALL ShowContinueError('This may, or may not, indicate mismatched component sizes.')
              CALL ShowContinueError('Verify that the value entered is intended and is consistent with other components.')
            END IF
          ENDIF
        END IF
      END IF
    END IF
  END IF

  IsAutosize = .FALSE.
  IF (PTUnit(PTUnitNum)%MaxSATSupHeat == AutoSize) THEN
    IsAutosize = .TRUE.
  END IF
  IF (CurZoneEqNum > 0) THEN
    CALL CheckThisZoneForSizing(CurZoneEqNum, SizingDesRunThisZone)
    IF (.NOT. IsAutosize .AND. .NOT. SizingDesRunThisZone) THEN ! Simulation continue
      IF (PTUnit(PTUnitNum)%MaxSATSupHeat > 0.0d0) THEN
        CALL ReportSizingOutput(PTUnit(PTUnitNum)%UnitType, PTUnit(PTUnitNum)%Name, &
                     'User-Specified Maximum Supply Air Temperature from Supplemental Heater [C]',  &
                     PTUnit(PTUnitNum)%MaxSATSupHeat)
      END IF
    ELSE
      CALL CheckZoneSizing(PTUnit(PTUnitNum)%UnitType, PTUnit(PTUnitNum)%Name)
      MaxSATSupHeatDes = FinalZoneSizing(CurZoneEqNum)%HeatDesTemp
      IF (IsAutosize) THEN
        PTUnit(PTUnitNum)%MaxSATSupHeat = MaxSATSupHeatDes
        CALL ReportSizingOutput(PTUnit(PTUnitNum)%UnitType, PTUnit(PTUnitNum)%Name, &
                        'Design Size Maximum Supply Air Temperature from Supplemental Heater [C]', &
                        MaxSATSupHeatDes)
      ELSE
        IF (PTUnit(PTUnitNum)%MaxSATSupHeat > 0.0d0 .AND. MaxSATSupHeatDes > 0.0d0 &
            .AND. SizingDesRunThisZone) THEN
          MaxSATSupHeatUser = PTUnit(PTUnitNum)%MaxSATSupHeat
          CALL ReportSizingOutput(PTUnit(PTUnitNum)%UnitType, PTUnit(PTUnitNum)%Name, &
                        'Design Size Maximum Supply Air Temperature from Supplemental Heater [C]', &
                        MaxSATSupHeatDes, &
                        'User-Specified Maximum Supply Air Temperature from Supplemental Heater [C]', &
                        MaxSATSupHeatUser)
          IF (DisplayExtraWarnings) THEN
            IF (ABS(MaxSATSupHeatDes - MaxSATSupHeatUser) > (4.0d0*AutoVsHardSizingDeltaTempThreshold)) THEN
              CALL ShowMessage('SizePTUnit: Potential issue with equipment sizing for ' &
                                  //TRIM(PTUnit(PTUnitNum)%UnitType)//' '//TRIM(PTUnit(PTUnitNum)%Name))
              CALL ShowContinueError('User-Specified Maximum Supply Air Temperature from Supplemental Heater of '// &
                                      TRIM(RoundSigDigits(MaxSATSupHeatUser,2))// ' [C]')
              CALL ShowContinueError('differs from Design Size Maximum Supply Air Temperature from Supplemental Heater of ' // &
                                      TRIM(RoundSigDigits(MaxSATSupHeatDes,2))// ' [C]')
              CALL ShowContinueError('This may, or may not, indicate mismatched component sizes.')
              CALL ShowContinueError('Verify that the value entered is intended and is consistent with other components.')
            END IF
          ENDIF
        END IF
      END IF
    END IF
  END IF

  CALL SetCoilDesFlow(PTUnit(PTUnitNum)%ACHeatCoilType,PTUnit(PTUnitNum)%ACHeatCoilName,PTUnit(PTUnitNum)%MaxHeatAirVolFlow,&
                       ErrorsFound)

  IF (CurZoneEqNum > 0) THEN
    ZoneEqSizing(CurZoneEqNum)%OAVolFlow = MAX(PTUnit(PTUnitNum)%CoolOutAirVolFlow,PTUnit(PTUnitNum)%HeatOutAirVolFlow)
    ZoneEqSizing(CurZoneEqNum)%AirVolFlow = MAX(PTUnit(PTUnitNum)%MaxCoolAirVolFlow,PTUnit(PTUnitNum)%MaxHeatAirVolFlow)
  END IF

  IF (ErrorsFound) THEN
    CALL ShowFatalError('Preceding sizing errors cause program termination')
  END IF

  RETURN
END SUBROUTINE SizePTUnit

SUBROUTINE ControlPTUnitOutput(PTUnitNum,FirstHVACIteration,OpMode,QZnReq,ZoneNum,PartLoadFrac,OnOffAirFlowRatio,SupHeaterLoad, &
                             HXUnitOn)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Raustad
          !       DATE WRITTEN   July 2005
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Determine the part load fraction of the heat pump for this time step.

          ! METHODOLOGY EMPLOYED:
          ! Use RegulaFalsi technique to iterate on part-load ratio until convergence is achieved.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE General,                   ONLY: SolveRegulaFalsi, RoundSigDigits, TrimSigDigits
  USE DataGlobals,               ONLY: WarmUpFlag
  USE HeatingCoils,              ONLY: SimulateHeatingCoilComponents
  USE Psychrometrics,            ONLY: PsyCpAirFnWTdb
  USE DataEnvironment,           ONLY: OutDryBulbTemp
  USE SteamCoils,                ONLY: SimulateSteamCoilComponents
  USE WaterCoils,                ONLY: SimulateWaterCoilComponents
  USE PlantUtilities,            ONLY: SetComponentFlowRate

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER,   INTENT (IN)    :: PTUnitNum          ! Unit index in fan coil array
  LOGICAL,   INTENT (IN)    :: FirstHVACIteration ! flag for 1st HVAC iteration in the time step
  INTEGER,   INTENT (IN)    :: OpMode             ! operating mode: CycFanCycCoil | ContFanCycCoil
  REAL(r64), INTENT (IN)    :: QZnReq             ! cooling or heating output needed by zone [W]
  INTEGER,   INTENT (IN)    :: ZoneNum            ! Index to zone number
  REAL(r64), INTENT (OUT)   :: PartLoadFrac       ! unit part load fraction
  REAL(r64), INTENT (INOUT) :: OnOffAirFlowRatio  ! ratio of compressor ON airflow to AVERAGE airflow over timestep
  REAL(r64), INTENT (INOUT) :: SupHeaterLoad      ! Supplemental heater load [W]
  LOGICAL,   INTENT (INOUT) :: HXUnitOn           ! flag to enable heat exchanger
  REAL(r64)                 :: mdot               ! coil fluid mass flow rate (kg/s)

          ! SUBROUTINE PARAMETER DEFINITIONS:
          !
  INTEGER, PARAMETER   :: MaxIte   = 500          ! maximum number of iterations
  REAL(r64), PARAMETER :: MinPLF   = 0.0d0          ! minimum part load factor allowed

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64)          :: FullOutput    ! unit full output when compressor is operating [W]
  REAL(r64)          :: TempOutput    ! unit output when iteration limit exceeded [W]
  REAL(r64)          :: NoCompOutput  ! output when no active compressor [W]
  REAL(r64)   :: ErrorToler    ! error tolerance
  INTEGER            :: SolFla        ! Flag of RegulaFalsi solver
  REAL(r64), DIMENSION(8) :: Par           ! Parameters passed to RegulaFalsi
  CHARACTER(len=20)  :: IterNum       ! Max number of iterations for warning message
  REAL(r64)          :: CpAir         ! air specific heat
  REAL(r64)          :: OutsideDryBulbTemp ! Outside air temperature at external node height
!unused1208  REAL(r64)          :: UpperLimitPLR ! used when RegulaFalsi exceeds iteration limit
  REAL(r64)          :: TempMinPLR
  REAL(r64)          :: TempMaxPLR
  LOGICAL            :: ContinueIter

  SupHeaterLoad = 0.0d0
  PartLoadFrac  = 0.0d0

  IF(PTUnit(PTUnitNum)%CondenserNodeNum .EQ. 0)THEN
    OutsideDryBulbTemp = OutDryBulbTemp
  ELSE
    OutsideDryBulbTemp = Node(PTUnit(PTUnitNum)%CondenserNodeNum)%Temp
  END IF

  IF (GetCurrentScheduleValue(PTUnit(PTUnitNum)%SchedPtr) .EQ. 0.0d0) RETURN

  ! If no heating or cooling required the coils needs to be off
  IF (.NOT. HeatingLoad .AND. .NOT. CoolingLoad) THEN
    RETURN
  END IF

  ! Get result when DX coil is off
  CALL CalcPTUnit(PTUnitNum, FirstHVACIteration, PartLoadFrac, NoCompOutput, QZnReq, OnOffAirFlowRatio, SupHeaterLoad, HXUnitOn)

  ! Get full load result
  PartLoadFrac  = 1.0d0
  CALL CalcPTUnit(PTUnitNum, FirstHVACIteration, PartLoadFrac, FullOutput, QZnReq, OnOffAirFlowRatio, SupHeaterLoad, HXUnitOn)

  IF (CoolingLoad) THEN
  ! Since we are cooling, we expect FullOutput < NoCompOutput
  ! Check that this is the case; if not set PartLoadFrac = 0.0 (off) and return
    IF (FullOutput >= NoCompOutput) THEN
      PartLoadFrac = 0.0d0
      RETURN
    END IF
  ! If the QZnReq <= FullOutput the unit needs to run full out
    IF (QZnReq  <=  FullOutput) THEN
      PartLoadFrac = 1.0d0
      RETURN
    END IF
    IF(PTUnit(PTUnitNum)%UnitType_Num .EQ. PTACUnit)THEN
      ErrorToler = 0.001d0
    ELSE
      ErrorToler = PTUnit(PTUnitNum)%CoolConvergenceTol !Error tolerance for convergence from input deck
    END IF
  ELSE
  ! Since we are heating, we expect FullOutput > NoCompOutput
  ! Check that this is the case; if not set PartLoadFrac = 0.0 (off)
    IF (FullOutput <= NoCompOutput) THEN
      PartLoadFrac = 0.0d0
  ! may need supplemental heating so don't return in heating mode
  !    RETURN
    END IF
  ! If the QZnReq >= FullOutput the unit needs to run full out
    IF (QZnReq  >=  FullOutput .AND. PTUnit(PTUnitNum)%SuppHeatCoilIndex .GT. 0) THEN
      PartLoadFrac = 1.0d0
  ! may need supplemental heating so don't return in heating mode
  !    RETURN
    END IF
    ErrorToler = PTUnit(PTUnitNum)%HeatConvergenceTol !Error tolerance for convergence from input deck
  END IF

  ! Calculate the part load fraction

  IF ((HeatingLoad .AND. QZnReq < FullOutput) .OR. (CoolingLoad .AND. QZnReq > FullOutput)) THEN

    Par(1) = PTUnitNum
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
    IF(HXUnitOn)THEN
      Par(8) = 1.0d0
    ELSE
      Par(8) = 0.0d0
    END IF
    CALL SolveRegulaFalsi(ErrorToler, MaxIte, SolFla, PartLoadFrac, PLRResidual,   &
                              0.0d0, 1.0d0, Par)
    IF (SolFla == -1) THEN
!     Very low loads may not converge quickly. Tighten PLR boundary and try again.
      TempMaxPLR = -0.1d0
      ContinueIter = .TRUE.
      DO WHILE(ContinueIter .AND. TempMaxPLR .LT. 1.0d0)
        TempMaxPLR = TempMaxPLR + 0.1d0
        CALL CalcPTUnit(PTUnitNum,FirstHVACIteration,TempMaxPLR,TempOutput,QZnReq,OnOffAirFlowRatio,SupHeaterLoad, HXUnitOn)
        IF(HeatingLoad .AND. TempOutput .GT. QZnReq)ContinueIter = .FALSE.
        IF(CoolingLoad .AND. TempOutput .LT. QZnReq)ContinueIter = .FALSE.
      END DO
      TempMinPLR = TempMaxPLR
      ContinueIter = .TRUE.
      DO WHILE(ContinueIter .AND. TempMinPLR .GT. 0.0d0)
        TempMinPLR = TempMinPLR - 0.01d0
        CALL CalcPTUnit(PTUnitNum,FirstHVACIteration,TempMinPLR,TempOutput,QZnReq,OnOffAirFlowRatio,SupHeaterLoad, HXUnitOn)
        IF(HeatingLoad .AND. TempOutput .LT. QZnReq)ContinueIter = .FALSE.
        IF(CoolingLoad .AND. TempOutput .GT. QZnReq)ContinueIter = .FALSE.
      END DO
      CALL SolveRegulaFalsi(ErrorToler, MaxIte, SolFla, PartLoadFrac, PLRResidual,   &
                              TempMinPLR, TempMaxPLR, Par)
      IF (SolFla == -1) THEN
        IF (.NOT. FirstHVACIteration .AND. .NOT. WarmupFlag) THEN
          CALL CalcPTUnit(PTUnitNum,FirstHVACIteration,PartLoadFrac,TempOutput,QZnReq,OnOffAirFlowRatio,SupHeaterLoad, HXUnitOn)
          IF(PTUnit(PTUnitNum)%IterErrIndex .EQ. 0)THEN
            WRITE(IterNum,*) MaxIte
            IterNum=ADJUSTL(IterNum)
            Call ShowWarningError(TRIM(PTUnit(PTUnitNum)%UnitType)//' "'//TRIM(PTUnit(PTUnitNum)%Name)//'"')
            CALL ShowContinueError(' Iteration limit exceeded calculating packaged terminal unit part-load ratio, '// &
                                'maximum iterations = '//TRIM(IterNum))
            CALL ShowContinueErrorTimeStamp(' Part-load ratio returned = '//TRIM(RoundSigDigits(PartLoadFrac,3)))
            CALL ShowContinueError(' Load requested = '//TRIM(TrimSigDigits(QZnReq,5))//', Load delivered = ' &
                                 //TRIM(TrimSigDigits(TempOutput,5)))
          END IF
          CALL ShowRecurringWarningErrorAtEnd(TRIM(PTUnit(PTUnitNum)%UnitType)//' "'//TRIM(PTUnit(PTUnitNum)%Name)//&
            '" - Iteration limit exceeded error continues...',   &
           PTUnit(PTUnitNum)%IterErrIndex,ReportMinOf=TempOutput,ReportMaxOf=TempOutput,  &
                                             ReportMaxUnits='{W}',ReportMinUnits='{W}')

        END IF
      ELSE IF (SolFla == -2) THEN
        IF (.NOT. FirstHVACIteration) THEN
          Call ShowWarningError(TRIM(PTUnit(PTUnitNum)%UnitType)//' "'//TRIM(PTUnit(PTUnitNum)%Name)//'"')
          CALL ShowContinueError('Packaged terminal unit part-load ratio calculation failed: ' &
                           //'PLR limits of 0 to 1 exceeded')
          CALL ShowContinueError('Please fill out a bug report and forward to the EnergyPlus support group.')
          CALL ShowContinueErrorTimeStamp(' ')
          IF (WarmupFlag) CALL ShowContinueError ('Error occurred during warmup days.')
        END IF
        PartLoadFrac = MAX(MinPLF, ABS(QZnReq - NoCompOutput) / ABS(FullOutput - NoCompOutput))
      END IF
    ELSE IF (SolFla == -2) THEN
      IF (.NOT. FirstHVACIteration) THEN
        Call ShowWarningError(TRIM(PTUnit(PTUnitNum)%UnitType)//' "'//TRIM(PTUnit(PTUnitNum)%Name)//'"')
        CALL ShowContinueError('Packaged terminal unit part-load ratio calculation failed: ' &
                           //'PLR limits of 0 to 1 exceeded')
        CALL ShowContinueError('Please fill out a bug report and forward to the EnergyPlus support group.')
        CALL ShowContinueErrorTimeStamp(' ')
        IF (WarmupFlag) CALL ShowContinueError ('Error occurred during warmup days.')
      END IF
      PartLoadFrac = MAX(MinPLF, ABS(QZnReq - NoCompOutput) / ABS(FullOutput - NoCompOutput))
    END IF

  END IF

  ! if the DX heating coil cannot meet the load, trim with supplemental heater
  ! occurs with constant fan mode when compressor is on or off
  ! occurs with cycling fan mode when compressor PLR is equal to 1
  IF (HeatingLoad .AND. QZnReq .GT. FullOutput .AND. PTUnit(PTUnitNum)%SuppHeatCoilIndex .GT. 0)THEN
    PartLoadFrac  = 1.0d0
    IF (OutsideDryBulbTemp .LE. PTUnit(PTUnitNum)%MaxOATSupHeat) THEN
      SupHeaterLoad = QZnReq - FullOutput
    ELSE
      SupHeaterLoad = 0.0d0
    END IF
    CALL CalcPTUnit(PTUnitNum, FirstHVACIteration, PartLoadFrac, TempOutput, QZnReq, OnOffAirFlowRatio, SupHeaterLoad, HXUnitOn)
  END IF

! check the outlet of the supplemental heater to be lower than the maximum supplemental heater supply air temperature
  IF(PTUnit(PTUnitNum)%SuppHeatCoilIndex .GT. 0)THEN
    IF (Node(PTUnit(PTUnitNum)%AirOutNode)%Temp .GT. PTUnit(PTUnitNum)%MaxSATSupHeat .AND. SupHeaterLoad .GT. 0.0d0) THEN

       ! If supply air temperature is to high, turn off the supplemental heater to recalculate the outlet temperature
       SupHeaterLoad = 0.0d0
       Select Case (PTUnit(PTUnitNum)%SuppHeatCoilType_Num)
         Case (Coil_HeatingGas,Coil_HeatingElectric )
             CALL SimulateHeatingCoilComponents(PTUnit(PTUnitNum)%SuppHeatCoilName,FirstHVACIteration, &
                                                SupHeaterLoad, PTUnit(PTUnitNum)%SuppHeatCoilIndex)
         Case (Coil_HeatingWater)
             mdot = 0.d0
             Call SetComponentFlowRate( mdot , &
                                        PTUnit(PTUnitNum)%HotWaterControlNode, &
                                        PTUnit(PTUnitNum)%PlantCoilOutletNode, &
                                        PTUnit(PTUnitNum)%LoopNum, &
                                        PTUnit(PTUnitNum)%LoopSide, &
                                        PTUnit(PTUnitNum)%BranchNum, &
                                        PTUnit(PTUnitNum)%CompNum)
             CALL SimulateWaterCoilComponents(PTUnit(PTUnitNum)%SuppHeatCoilName,FirstHVACIteration, &
                                              PTUnit(PTUnitNum)%SuppHeatCoilIndex, SupHeaterLoad, &
                                              PTUnit(PTUnitNum)%OpMode, PartLoadFrac)
         Case (Coil_HeatingSteam)
             mdot = 0.d0
             CALL SetComponentFlowRate( mdot , &
                                        PTUnit(PTUnitNum)%HWCoilSteamInletNode, &
                                        PTUnit(PTUnitNum)%PlantCoilOutletNode, &
                                        PTUnit(PTUnitNum)%LoopNum, &
                                        PTUnit(PTUnitNum)%LoopSide, &
                                        PTUnit(PTUnitNum)%BranchNum, &
                                        PTUnit(PTUnitNum)%CompNum)
             CALL SimulateSteamCoilComponents(PTUnit(PTUnitNum)%SuppHeatCoilName, &
                                              FirstHVACIteration, SupHeaterLoad,  &
                                              PTUnit(PTUnitNum)%SuppHeatCoilIndex)
       END Select

!     If the outlet temperature is below the maximum supplemental heater supply air temperature, reduce the load passed to
!     the supplemental heater, otherwise leave the supplemental heater off. If the supplemental heater is to be turned on,
!     use the outlet conditions when the supplemental heater was off (CALL above) as the inlet conditions for the calculation
!     of supplemental heater load to just meet the maximum supply air temperature from the supplemental heater.
      IF (Node(PTUnit(PTUnitNum)%AirOutNode)%Temp .LT. PTUnit(PTUnitNum)%MaxSATSupHeat) THEN
        CpAir = PsyCpAirFnWTdb(Node(PTUnit(PTUnitNum)%AirOutNode)%HumRat,Node(PTUnit(PTUnitNum)%AirOutNode)%Temp)
        SupHeaterLoad = Node(PTUnit(PTUnitNum)%AirInNode)%MassFlowRate * CpAir * &
                      (PTUnit(PTUnitNum)%MaxSATSupHeat - Node(PTUnit(PTUnitNum)%AirOutNode)%Temp)

      ELSE
        SupHeaterLoad = 0.0d0
      END IF
    END IF
  END IF

  RETURN
END SUBROUTINE ControlPTUnitOutput

SUBROUTINE CalcPTUnit(PTUnitNum,FirstHVACIteration,PartLoadFrac,LoadMet,QZnReq,OnOffAirFlowRatio,SupHeaterLoad, HXUnitOn)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Raustad
          !       DATE WRITTEN   July 2005
          !       MODIFIED        B. Nigusse, Jan 2012, added hot water and steam heating coils to PTHP and WSHP
          !                       Chandan Sharma, July 2012 : Added zone sys avail managers
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Simulate the components making up the packaged terminal heat pump.

          ! METHODOLOGY EMPLOYED:
          ! Simulates the unit components sequentially in the air flow direction.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE Fans,                      ONLY: SimulateFanComponents
  USE DXCoils,                   ONLY: SimDXCoil
  USE MixedAir,                  ONLY: SimOAMixer
  USE HeatingCoils,              ONLY: SimulateHeatingCoilComponents
  USE SteamCoils,                ONLY: SimulateSteamCoilComponents
  USE WaterCoils,                ONLY: SimulateWaterCoilComponents
  USE InputProcessor,            ONLY: SameString
  USE HVACHXAssistedCoolingCoil, ONLY: SimHXAssistedCoolingCoil
  USE Psychrometrics,            ONLY: PsyHFnTdbW, PsyCpAirFnWTdb
  USE DataEnvironment,           ONLY: OutDryBulbTemp
  USE WatertoAirheatPumpSimple,  ONLY: SimWatertoAirHPSimple
  USE PlantUtilities,            ONLY: SetComponentFlowRate
  USE General,                   ONLY: SolveRegulaFalsi, RoundSigDigits

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER,   INTENT    (IN) :: PTUnitNum              ! Unit index in fan coil array
  LOGICAL,   INTENT    (IN) :: FirstHVACIteration   ! flag for 1st HVAC iteration in the time step
  REAL(r64), INTENT    (IN) :: PartLoadFrac         ! compressor part load fraction
  REAL(r64), INTENT   (OUT) :: LoadMet              ! load met by unit (W)
  REAL(r64), INTENT    (IN) :: QZnReq               ! Zone load (W) unused1208
  REAL(r64), INTENT (INOUT) :: OnOffAirFlowRatio    ! ratio of compressor ON airflow to AVERAGE airflow over timestep
  REAL(r64), INTENT (INOUT) :: SupHeaterLoad        ! supplemental heater load (W)
  LOGICAL,   INTENT    (IN) :: HXUnitOn             ! flag to enable heat exchanger

          ! SUBROUTINE PARAMETER DEFINITIONS:
  INTEGER, PARAMETER          :: MaxIte = 500       ! maximum number of iterations
  CHARACTER(len=*), PARAMETER :: Blank = ' '        ! subroutine argument when coil index is known
  REAL(r64), PARAMETER :: ErrTolerance = 0.001d0    ! convergence limit for hotwater coil
  INTEGER, PARAMETER :: SolveMaxIter=50

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na


          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER  :: OutletNode         ! PTHP air outlet node
  INTEGER  :: InletNode          ! PTHP air inlet node
  REAL(r64):: AirMassFlow        ! total supply air mass flow through the PTHP [m3/s]
  REAL(r64):: MinHumRat          ! minimum humidity ratio for sensible capacity calculation (kg/kg)
  REAL(r64):: OutsideDryBulbTemp ! Outdoor air temperature at external node height
  REAL(r64):: QCoilReq           ! load passed to heating coil (W)
  REAL(r64):: QActual            ! actual heating coil output (W)
  INTEGER  :: OpMode             ! fan operating mode, CycFanCycCoil or ContFanCycCoil
  LOGICAL  :: errflag            ! subroutine error flag
  REAL(r64):: WSHPRuntimeFrac    ! RTF variable for WSHP's
  REAL(r64) :: mdot              !local temporary for mass flow rate
  REAL(r64) :: MinWaterFlow      ! minimum water mass flow rate
  REAL(r64) :: PartLoadFraction  ! heating or cooling part load fraction
  REAL(r64) :: MaxHotWaterFlow   ! coil maximum hot water mass flow rate, kg/s
  REAL(r64) :: HotWaterMdot      ! actual hot water mass flow rate
  REAL(r64), DIMENSION(3) :: Par
  INTEGER   :: SolFlag
  REAL(r64) :: MinFlow            ! minimum fluid flow rate, kg/s
  INTEGER   :: ControlCompTypeNum ! temporary component index number
          ! FLOW

  OutletNode = PTUnit(PTUnitNum)%AirOutNode
  InletNode = PTUnit(PTUnitNum)%AirInNode
  OpMode = PTUnit(PTUnitNum)%OpMode
  IF(PTUnit(PTUnitNum)%CondenserNodeNum .EQ. 0)THEN
    OutsideDryBulbTemp = OutDryBulbTemp
  ELSE
    OutsideDryBulbTemp = Node(PTUnit(PTUnitNum)%CondenserNodeNum)%Temp
  END IF

  SaveCompressorPLR = 0.0d0
  ! Set inlet air mass flow rate based on PLR and compressor on/off air flow rates
  CALL SetAverageAirFlow(PTUnitNum, PartLoadFrac, OnOffAirFlowRatio)

  AirMassFlow = Node(InletNode)%MassFlowRate
  IF(PTUnit(PTUnitNum)%OutsideAirNode .GT. 0)&
  CALL SimOAMixer(PTUnit(PTUnitNum)%OAMixName,FirstHVACIteration,PTUnit(PTUnitNum)%OAMixIndex)

  ! if blow through, simulate fan then coils
  IF (PTUnit(PTUnitNum)%FanPlace .EQ. BlowThru) THEN
    CALL SimulateFanComponents(PTUnit(PTUnitNum)%FanName,FirstHVACIteration,PTUnit(PTUnitNum)%FanIndex,FanSpeedRatio,&
                                 ZoneCompTurnFansOn,ZoneCompTurnFansOff)
  END IF

  IF (CoolingLoad .AND. OutsideDryBulbTemp .GT. PTUnit(PTUnitNum)%MinOATCompressor)THEN
    SELECT CASE(PTUnit(PTUnitNum)%UnitType_Num)
      CASE(PTACUnit, PTHPUnit)
        IF (PTUnit(PTUnitNum)%DXCoolCoilType_Num == CoilDX_CoolingHXAssisted) THEN
          CALL SimHXAssistedCoolingCoil(PTUnit(PTUnitNum)%DXCoolCoilName,FirstHVACIteration,On,PartLoadFrac,&
                                        PTUnit(PTUnitNum)%CoolCoilCompIndex, PTUnit(PTUnitNum)%OpMode, &
                                        HXUnitEnable=HXUnitOn)
        ELSE
          CALL SimDXCoil(PTUnit(PTUnitNum)%DXCoolCoilName,On,FirstHVACIteration,PartLoadFrac,PTUnit(PTUnitNum)%CoolCoilCompIndex,  &
             PTUnit(PTUnitNum)%OpMode,OnOffAirFlowRatio)
        END IF
        SaveCompressorPLR = DXCoilPartLoadRatio(PTUnit(PTUnitNum)%DXCoolCoilIndexNum)
      CASE(PTWSHPUnit)
        CALL HeatPumpRunFrac(PTUnitNum,PartLoadFrac,errflag,WSHPRuntimeFrac)
        CALL SimWatertoAirHPSimple(Blank, &
        PTUnit(PTUnitNum)%DXCoolCoilIndexNum, &
                          QZnReq, 1.0d0, &
                          OpMode,WSHPRuntimeFrac, PTUnit(PTUnitNum)%MaxONOFFCyclesperHour, &
                          PTUnit(PTUnitNum)%HPTimeConstant, PTUnit(PTUnitNum)%FanDelayTime, 1, PartLoadFrac, FirstHVACIteration, &
                          OnOffAirFlowRat=OnOffAirFlowRatio)
        SaveCompressorPLR = PartLoadFrac
      CASE DEFAULT
    END SELECT
  ELSE ! cooling coil is off
    SELECT CASE(PTUnit(PTUnitNum)%UnitType_Num)
      CASE(PTACUnit, PTHPUnit)
        IF (PTUnit(PTUnitNum)%DXCoolCoilType_Num == CoilDX_CoolingHXAssisted) THEN
          CALL SimHXAssistedCoolingCoil(PTUnit(PTUnitNum)%DXCoolCoilName,FirstHVACIteration,Off,0.0d0, &
                                        PTUnit(PTUnitNum)%CoolCoilCompIndex, PTUnit(PTUnitNum)%OpMode, &
                                        HXUnitEnable=HXUnitOn)
        ELSE
          CALL SimDXCoil(PTUnit(PTUnitNum)%DXCoolCoilName,Off,FirstHVACIteration,0.0d0,  &
                         PTUnit(PTUnitNum)%CoolCoilCompIndex,PTUnit(PTUnitNum)%OpMode,OnOffAirFlowRatio)
        END IF
      CASE(PTWSHPUnit)
        CALL SimWatertoAirHPSimple(Blank, &
        PTUnit(PTUnitNum)%DXCoolCoilIndexNum, &
                         0.0d0, 0.0d0, &
                         OpMode,0.0d0, PTUnit(PTUnitNum)%MaxONOFFCyclesperHour, &
                         PTUnit(PTUnitNum)%HPTimeConstant, PTUnit(PTUnitNum)%FanDelayTime, 0, 0.0d0, FirstHVACIteration )
      CASE DEFAULT
    END SELECT
  END IF
  IF (HeatingLoad)THEN
    IF(PTUnit(PTUnitNum)%UnitType_Num .EQ. PTACUnit)THEN
      QCoilReq = PTUnit(PTUnitNum)%ACHeatCoilCap * PartLoadFrac
      IF(PTUnit(PTUnitNum)%ACHeatCoilType_Num == Coil_HeatingGas .OR.   &
         PTUnit(PTUnitNum)%ACHeatCoilType_Num == Coil_HeatingElectric)THEN
        CALL SimulateHeatingCoilComponents(PTUnit(PTUnitNum)%ACHeatCoilName,FirstHVACIteration,QCoilReq,  &
                                     PTUnit(PTUnitNum)%ACHeatCoilIndex, QActual, .FALSE., OpMode, PartLoadFrac)
      ELSE IF(PTUnit(PTUnitNum)%ACHeatCoilType_Num == Coil_HeatingWater)THEN
!       set water inlet node mass flow rate proportional to PLR. Limit water flow rate based on "available" upper limit.
        mdot = PTUnit(PTUnitNum)%MaxHeatCoilFluidFlow * PartLoadFrac

        CALL SetComponentFlowRate( mdot , &
                                   PTUnit(PTUnitNum)%HotWaterControlNode, &
                                   PTUnit(PTUnitNum)%PlantCoilOutletNode, &
                                   PTUnit(PTUnitNum)%LoopNum, &
                                   PTUnit(PTUnitNum)%LoopSide, &
                                   PTUnit(PTUnitNum)%BranchNum, &
                                   PTUnit(PTUnitNum)%CompNum)

        CALL SimulateWaterCoilComponents(PTUnit(PTUnitNum)%ACHeatCoilName,FirstHVACIteration, &
                                       PTUnit(PTUnitNum)%ACHeatCoilIndex, QActual, PTUnit(PTUnitNum)%OpMode, PartLoadFrac)
      ELSE IF(PTUnit(PTUnitNum)%ACHeatCoilType_Num == Coil_HeatingSteam)THEN
!       set steam inlet node mass flow rate proportional to PLR. Limit steam flow rate based on "available" upper limit.
        mdot = PTUnit(PTUnitNum)%MaxHeatCoilFluidFlow *  PartLoadFrac
        CALL SetComponentFlowRate( mdot , &
                                   PTUnit(PTUnitNum)%HWCoilSteamInletNode, &
                                   PTUnit(PTUnitNum)%PlantCoilOutletNode, &
                                   PTUnit(PTUnitNum)%LoopNum, &
                                   PTUnit(PTUnitNum)%LoopSide, &
                                   PTUnit(PTUnitNum)%BranchNum, &
                                   PTUnit(PTUnitNum)%CompNum)

        CALL SimulateSteamCoilComponents(PTUnit(PTUnitNum)%ACHeatCoilName, FirstHVACIteration, QCoilReq,  &
                                         PTUnit(PTUnitNum)%ACHeatCoilIndex, QActual, PTUnit(PTUnitNum)%OpMode, PartLoadFrac)
      END IF
    ELSE
      IF(OutsideDryBulbTemp .GT. PTUnit(PTUnitNum)%MinOATCompressor)THEN
        SELECT CASE(PTUnit(PTUnitNum)%UnitType_Num)
          CASE(PTHPUnit)
            CALL SimDXCoil(PTUnit(PTUnitNum)%DXHeatCoilName,On,FirstHVACIteration,PartLoadFrac,PTUnit(PTUnitNum)%DXHeatCoilIndex,  &
               PTUnit(PTUnitNum)%OpMode,OnOffAirFlowRatio)
            SaveCompressorPLR = DXCoilPartLoadRatio(PTUnit(PTUnitNum)%DXHeatCoilIndexNum)
          CASE(PTWSHPUnit)
            CALL HeatPumpRunFrac(PTUnitNum,PartLoadFrac,errflag,WSHPRuntimeFrac)
            CALL SimWatertoAirHPSimple(Blank, &
                     PTUnit(PTUnitNum)%DXHeatCoilIndex, &
                     QZnReq, 0.0d0, &
                     OpMode,WSHPRuntimeFrac, PTUnit(PTUnitNum)%MaxONOFFCyclesperHour, &
                     PTUnit(PTUnitNum)%HPTimeConstant, PTUnit(PTUnitNum)%FanDelayTime, 1, PartLoadFrac, FirstHVACIteration, &
                     OnOffAirFlowRat=OnOffAirFlowRatio)
            SaveCompressorPLR = PartLoadFrac
          CASE DEFAULT
        END SELECT
      ELSE
        SELECT CASE(PTUnit(PTUnitNum)%UnitType_Num)
          CASE(PTHPUnit)
            CALL SimDXCoil(PTUnit(PTUnitNum)%DXHeatCoilName,Off,FirstHVACIteration,0.0d0,PTUnit(PTUnitNum)%DXHeatCoilIndex,  &
                           PTUnit(PTUnitNum)%OpMode,OnOffAirFlowRatio)
          CASE(PTWSHPUnit)
            CALL SimWatertoAirHPSimple(Blank, &
                   PTUnit(PTUnitNum)%DXHeatCoilIndex, &
                   0.0d0, 0.0d0, &
                   OpMode, 0.0d0, PTUnit(PTUnitNum)%MaxONOFFCyclesperHour, &
                   PTUnit(PTUnitNum)%HPTimeConstant, PTUnit(PTUnitNum)%FanDelayTime, 0, 0.0d0, FirstHVACIteration)

          CASE DEFAULT
        END SELECT
      END IF
    END IF
  ELSE
!   heating coil is off
    IF(PTUnit(PTUnitNum)%UnitType_Num .EQ. PTACUnit)THEN
      QCoilReq = 0.0d0
      IF(PTUnit(PTUnitNum)%ACHeatCoilType_Num == Coil_HeatingGas .OR.   &
         PTUnit(PTUnitNum)%ACHeatCoilType_Num == Coil_HeatingElectric)THEN
        CALL SimulateHeatingCoilComponents(PTUnit(PTUnitNum)%ACHeatCoilName,FirstHVACIteration,QCoilReq,  &
                                     PTUnit(PTUnitNum)%ACHeatCoilIndex)
      ELSE IF(PTUnit(PTUnitNum)%ACHeatCoilType_Num == Coil_HeatingWater)THEN
        mdot = 0.d0
        Call SetComponentFlowRate( mdot , &
                                   PTUnit(PTUnitNum)%HotWaterControlNode, &
                                   PTUnit(PTUnitNum)%PlantCoilOutletNode, &
                                   PTUnit(PTUnitNum)%LoopNum, &
                                   PTUnit(PTUnitNum)%LoopSide, &
                                   PTUnit(PTUnitNum)%BranchNum, &
                                   PTUnit(PTUnitNum)%CompNum)

        CALL SimulateWaterCoilComponents(PTUnit(PTUnitNum)%ACHeatCoilName,FirstHVACIteration, &
                                       PTUnit(PTUnitNum)%ACHeatCoilIndex)
      ELSE IF(PTUnit(PTUnitNum)%ACHeatCoilType_Num == Coil_HeatingSteam)THEN
        mdot = 0.d0
        Call SetComponentFlowRate( mdot , &
                                   PTUnit(PTUnitNum)%HWCoilSteamInletNode, &
                                   PTUnit(PTUnitNum)%PlantCoilOutletNode, &
                                   PTUnit(PTUnitNum)%LoopNum, &
                                   PTUnit(PTUnitNum)%LoopSide, &
                                   PTUnit(PTUnitNum)%BranchNum, &
                                   PTUnit(PTUnitNum)%CompNum)

        CALL SimulateSteamCoilComponents(PTUnit(PTUnitNum)%ACHeatCoilName, &
                                         FirstHVACIteration,    &
                                         QCoilReq,                        &
                                         PTUnit(PTUnitNum)%ACHeatCoilIndex, &
                                         QActual, PTUnit(PTUnitNum)%OpMode, PartLoadFrac)
      END IF
    ELSE
      SELECT CASE(PTUnit(PTUnitNum)%UnitType_Num)
        CASE(PTHPUnit)
          CALL SimDXCoil(PTUnit(PTUnitNum)%DXHeatCoilName,Off,FirstHVACIteration,0.0d0,PTUnit(PTUnitNum)%DXHeatCoilIndex,  &
                         PTUnit(PTUnitNum)%OpMode,OnOffAirFlowRatio)
        CASE(PTWSHPUnit)
          CALL SimWatertoAirHPSimple(Blank, &
                   PTUnit(PTUnitNum)%DXHeatCoilIndex, &
                   0.0d0, 0.0d0, &
                   OpMode, 0.0d0, PTUnit(PTUnitNum)%MaxONOFFCyclesperHour, &
                   PTUnit(PTUnitNum)%HPTimeConstant, PTUnit(PTUnitNum)%FanDelayTime, 0, 0.0d0, FirstHVACIteration )

        CASE DEFAULT
      END SELECT
    END IF
  END IF

  ! if draw through, simulate coils then fan
  IF (PTUnit(PTUnitNum)%FanPlace .EQ. DrawThru) THEN
    CALL SimulateFanComponents(PTUnit(PTUnitNum)%FanName,FirstHVACIteration,PTUnit(PTUnitNum)%FanIndex,FanSpeedRatio,&
                                 ZoneCompTurnFansOn,ZoneCompTurnFansOff)
  END IF
  IF(PTUnit(PTUnitNum)%SuppHeatCoilIndex .GT. 0)THEN
     IF ( SupHeaterLoad < SmallLoad ) THEN
         Select Case (PTUnit(PTUnitNum)%SuppHeatCoilType_Num)
           Case (Coil_HeatingGas,Coil_HeatingElectric )
                 CALL SimulateHeatingCoilComponents(PTUnit(PTUnitNum)%SuppHeatCoilName,FirstHVACIteration, &
                                                    SupHeaterLoad, PTUnit(PTUnitNum)%SuppHeatCoilIndex,  &
                                                    QActual, .TRUE., PTUnit(PTUnitNum)%OpMode)
           Case (Coil_HeatingWater)
               mdot = 0.0d0
               Call SetComponentFlowRate( mdot , &
                                          PTUnit(PTUnitNum)%HotWaterControlNode, &
                                          PTUnit(PTUnitNum)%PlantCoilOutletNode, &
                                          PTUnit(PTUnitNum)%LoopNum, &
                                          PTUnit(PTUnitNum)%LoopSide, &
                                          PTUnit(PTUnitNum)%BranchNum, &
                                          PTUnit(PTUnitNum)%CompNum)
               CALL SimulateWaterCoilComponents(PTUnit(PTUnitNum)%SuppHeatCoilName,FirstHVACIteration, &
                                          PTUnit(PTUnitNum)%SuppHeatCoilIndex, SupHeaterLoad, &
                                          PTUnit(PTUnitNum)%OpMode)
           Case (Coil_HeatingSteam)
               mdot = 0.0d0
               Call SetComponentFlowRate( mdot , &
                                          PTUnit(PTUnitNum)%HWCoilSteamInletNode, &
                                          PTUnit(PTUnitNum)%PlantCoilOutletNode, &
                                          PTUnit(PTUnitNum)%LoopNum, &
                                          PTUnit(PTUnitNum)%LoopSide, &
                                          PTUnit(PTUnitNum)%BranchNum, &
                                          PTUnit(PTUnitNum)%CompNum)
               CALL SimulateSteamCoilComponents(PTUnit(PTUnitNum)%SuppHeatCoilName, &
                                          FirstHVACIteration, SupHeaterLoad, &
                                          PTUnit(PTUnitNum)%SuppHeatCoilIndex, &
                                          QActual, PTUnit(PTUnitNum)%OpMode)
         END Select
     ELSE
        Select Case (PTUnit(PTUnitNum)%SuppHeatCoilType_Num)
           Case (Coil_HeatingGas,Coil_HeatingElectric )
              CALL SimulateHeatingCoilComponents(PTUnit(PTUnitNum)%SuppHeatCoilName,FirstHVACIteration, &
                                                 SupHeaterLoad, PTUnit(PTUnitNum)%SuppHeatCoilIndex, &
                                                 QActual, .TRUE., PTUnit(PTUnitNum)%OpMode)
           Case (Coil_HeatingWater)
               MaxHotWaterFlow = PTUnit(PTUnitNum)%MaxSuppCoilFluidFlow
               Call SetComponentFlowRate( MaxHotWaterFlow , &
                                          PTUnit(PTUnitNum)%HotWaterControlNode, &
                                          PTUnit(PTUnitNum)%PlantCoilOutletNode, &
                                          PTUnit(PTUnitNum)%LoopNum, &
                                          PTUnit(PTUnitNum)%LoopSide, &
                                          PTUnit(PTUnitNum)%BranchNum, &
                                          PTUnit(PTUnitNum)%CompNum)
              QActual = SupHeaterLoad
              ! simulate the hot water supplemental heating coil
              CALL SimulateWaterCoilComponents(PTUnit(PTUnitNum)%SuppHeatCoilName,FirstHVACIteration, &
                                               PTUnit(PTUnitNum)%SuppHeatCoilIndex, QActual, &
                                               PTUnit(PTUnitNum)%OpMode)
              IF( QActual > (SupHeaterLoad + SmallLoad)) THEN
                  ! control water flow to obtain output matching SupHeaterLoad
                  SolFlag = 0
                  MinWaterFlow = 0.0d0
                  Par(1) = REAL(PTUnitNum,r64)
                  IF (FirstHVACIteration) THEN
                    Par(2) = 1.d0
                  ELSE
                    Par(2) = 0.0d0
                  END IF
                  Par(3) = SupHeaterLoad
                  MaxHotWaterFlow = PTUnit(PTUnitNum)%MaxSuppCoilFluidFlow
                  CALL SolveRegulaFalsi(ErrTolerance, SolveMaxIter, SolFlag, HotWaterMdot, HotWaterCoilResidual, &
                                        MinWaterFlow, MaxHotWaterFlow, Par)
                  IF (SolFlag == -1) THEN
                    IF (PTUnit(PTUnitNum)%HotWaterCoilMaxIterIndex == 0) THEN
                      CALL ShowWarningMessage('CalcPTUnit: Hot water coil control failed for '//  &
                         trim(PTUnit(PTUnitNum)%UnitType)//'="'//  &
                         TRIM(PTUnit(PTUnitNum)%Name)//'"')
                      CALL ShowContinueErrorTimeStamp(' ')
                      CALL ShowContinueError('  Iteration limit ['//trim(RoundSigDigits(SolveMaxIter))//  &
                          '] exceeded in calculating hot water mass flow rate')
                    ENDIF
                    CALL ShowRecurringWarningErrorAtEnd('CalcPTUnit: Hot water coil control failed (iteration limit ['//  &
                        trim(RoundSigDigits(SolveMaxIter))//']) for '//trim(PTUnit(PTUnitNum)%UnitType)//'="'// &
                        TRIM(PTUnit(PTUnitNum)%Name),PTUnit(PTUnitNum)%HotWaterCoilMaxIterIndex)
                  ELSE IF (SolFlag == -2) THEN
                    IF (PTUnit(PTUnitNum)%HotWaterCoilMaxIterIndex2 == 0) THEN
                      CALL ShowWarningMessage('CalcPTUnit: Hot water coil control failed (maximum flow limits) for '//  &
                          trim(PTUnit(PTUnitNum)%UnitType)//'="'// &
                          TRIM(PTUnit(PTUnitNum)%Name)//'"')
                      CALL ShowContinueErrorTimeStamp(' ')
                      CALL ShowContinueError('...Bad hot water maximum flow rate limits')
                      CALL ShowContinueError('...Given minimum water flow rate='//trim(RoundSigDigits(MinWaterFlow,3))//' kg/s')
                      CALL ShowContinueError('...Given maximum water flow rate='//trim(RoundSigDigits(MaxHotWaterFlow,3))//' kg/s')
                    ENDIF
                    CALL ShowRecurringWarningErrorAtEnd('CalcPTUnit: Hot water coil control failed (flow limits) for '//  &
                       trim(PTUnit(PTUnitNum)%UnitType)//'="'// &
                       TRIM(PTUnit(PTUnitNum)%Name)//'"', &
                       PTUnit(PTUnitNum)%HotWaterCoilMaxIterIndex2,  &
                       ReportMinOf=MinWaterFlow,ReportMaxOf=MaxHotWaterFlow,ReportMinUnits='[kg/s]',ReportMaxUnits='[kg/s]')
                  END IF
                  QActual = SupHeaterLoad
                  ! simulate the hot water supplemental heating coil
                  CALL SimulateWaterCoilComponents(PTUnit(PTUnitNum)%SuppHeatCoilName,FirstHVACIteration, &
                                                   PTUnit(PTUnitNum)%SuppHeatCoilIndex, QActual, &
                                                   PTUnit(PTUnitNum)%OpMode)
              ENDIF
           Case (Coil_HeatingSteam)
               mdot = PTUnit(PTUnitNum)%MaxSuppCoilFluidFlow
               Call SetComponentFlowRate( mdot , &
                                          PTUnit(PTUnitNum)%HWCoilSteamInletNode, &
                                          PTUnit(PTUnitNum)%PlantCoilOutletNode, &
                                          PTUnit(PTUnitNum)%LoopNum, &
                                          PTUnit(PTUnitNum)%LoopSide, &
                                          PTUnit(PTUnitNum)%BranchNum, &
                                          PTUnit(PTUnitNum)%CompNum)

               ! simulate the steam supplemental heating coil
               CALL SimulateSteamCoilComponents(PTUnit(PTUnitNum)%SuppHeatCoilName, &
                                          FirstHVACIteration, SupHeaterLoad, &
                                          PTUnit(PTUnitNum)%SuppHeatCoilIndex, &
                                          QActual, PTUnit(PTUnitNum)%OpMode)
        END Select
     ENDIF
  ENDIF
! calculate sensible load met using delta enthalpy at a constant (minimum) humidity ratio)
  MinHumRat = MIN(Node(InletNode)%HumRat,Node(OutletNode)%HumRat)
  LoadMet   = AirMassFlow * (PsyHFnTdbW(Node(OutletNode)%Temp,MinHumRat) - PsyHFnTdbW(Node(InletNode)%Temp,MinHumRat))

RETURN
END SUBROUTINE CalcPTUnit

SUBROUTINE HeatPumpRunFrac(PTUnitNum,PLR,errflag,RuntimeFrac)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         R. Raustad (based on subroutine by Kenneth Tang)
          !       DATE WRITTEN   June 2009
          !       MODIFIED       na
          !       RE-ENGINEERED  na


          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine calculates the PLF based on the PLR. Parameters required are
          ! thermostat cycling rate (Nmax), heat pump time constant (tau), and the fraction
          ! of on-cycle power use (pr)

          ! METHODOLOGY EMPLOYED:
          ! NA


          ! REFERENCES:
          ! (1) Henderson, H. I., K. Rengarajan.1996. A Model to predict the latent capacity
          ! of air conditioners and heat pumps at part-load conditions with constant fan
          ! operation. ASHRAE Transactions 102 (1): 266-274

          ! (2) Henderson, H.I. Jr., Y.J. Huang and Danny Parker. 1999. Residential Equipment
          ! Part Load Curves for Use in DOE-2.  Environmental Energy Technologies Division,
          ! Ernest Orlando Lawrence Berkeley National Laboratory.


          ! USE STATEMENTS:


  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine


          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER,   INTENT(IN)           :: PTUnitNum        ! PTAC Index Number
  REAL(r64), INTENT(IN)           :: PLR              ! part load ratio
  REAL(r64), INTENT(OUT)          :: RuntimeFrac      ! the required run time fraction to meet part load
  LOGICAL,   INTENT(INOUT)        :: errflag          ! part load factor out of range flag



          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na


          ! INTERFACE BLOCK SPECIFICATIONS
          ! na


          ! DERIVED TYPE DEFINITIONS
          ! na


          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64) :: PartLoadFactor ! Part load factor
  REAL(r64) :: Nmax           ! Maximum cycling rate [cycles/hr]
  REAL(r64) :: tau            ! Heat pump time constant [s]
  REAL(r64) :: pr             ! On-cycle power use fraction [~]
  REAL(r64) :: error          ! Calculation error
  REAL(r64) :: PLF1           ! ith term of part load factor
  REAL(r64) :: PLF2           ! (i+1)th term of part load factor
  REAL(r64) :: A              ! Variable for simplify equation
  INTEGER   :: NumIteration   ! Iteration Counter

  Nmax=PTUnit(PTUnitNum)%MaxONOFFCyclesperHour
  tau=PTUnit(PTUnitNum)%HPTimeConstant
  pr=PTUnit(PTUnitNum)%OnCyclePowerFraction

  !Initialize
  errflag = .FALSE.
  error = 1.0d0
  NumIteration = 0

  !Initial guess for part load fraction
  PLF1 = 1.0d0

  !Calculate PLF using successive substitution until convergence
  !is achieved
  LOOPPLF: DO
    NumIteration=NumIteration + 1

    IF (PLR.EQ.1) THEN
        ! Set part load fraction, PLF1=1.0 if PLR=1.0 and exit loop
        PLF1 = 1.0d0
        EXIT LOOPPLF
    END IF

    IF (NumIteration.GT.100)THEN
        ! Exit loop if interation exceed 100
        errflag = .TRUE.
        PLF1 = 1.0d0
        EXIT LOOPPLF
    END IF

    IF (error.LT.0.00001d0)THEN
        ! Exit loop if convergence is achieved
        EXIT LOOPPLF

    ELSE
        ! Calculate PLF
        A = 4.d0 * tau * (Nmax/3600.d0) * (1 - PLR / PLF1)
        IF (A.LT.1.5d-3) THEN
            ! A safety check to prevent PLF2 = 1 - A * (1 - Exp(-1 / A))
            ! from "float underflow error". Occurs when PLR is very close to 1.0,
            ! small A value, thus Exp(-1/A) = 0
            PLF2 = 1 - A
        ELSE
            PLF2 = 1.0d0 - A * (1 - Exp(-1.d0 / A))
        END IF
        error = ABS((PLF2 - PLF1) / PLF1)
        PLF1 = PLF2
     END IF
  END DO LOOPPLF

  !Adjust PLF for the off cycle power consumption if
  !on-cycle power use is specified by the user
  IF (pr>0.0d0) THEN
    PartLoadFactor = PLR / ((PLR / PLF1) + (1 - PLR / PLF1) * pr)
  ELSE
    PartLoadFactor=PLF1
  END IF

  IF (PartLoadFactor <= 0.0d0)THEN
     PartLoadFactor = 0.0d0
     RuntimeFrac = 0.0d0
     errflag = .TRUE.
  ELSE
     RuntimeFrac = PLR / PartLoadFactor
  ENDIF

  IF (RuntimeFrac > 1.0d0 ) THEN
     RuntimeFrac = 1.0d0
  END IF

 RETURN

END SUBROUTINE HeatPumpRunFrac

FUNCTION HotWaterCoilResidual(HWFlow, Par) RESULT (Residuum)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Bereket Nigusse
          !       DATE WRITTEN   January 2012
          !       MODIFIED
          !       RE-ENGINEERED

          ! PURPOSE OF THIS FUNCTION:
          ! Calculates residual function (Actual Coil Output - SupHeaterLoad) / SupHeaterLoad
          ! the actual coil output depends on the hot water flow rate which is being varied to
          ! minimize the residual to the tolerance limit specified.

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
  INTEGER               :: PTUnitNum
  LOGICAL               :: FirstHVACSoln
  REAL(r64)             :: QCoilActual             ! delivered coild load, W
  REAL(r64)             :: SupHeaterLoad           ! requested coild load, W
  REAL(r64)             :: mdot

  PTUnitNum = INT(Par(1))
  IF (Par(2) > 0.0d0) THEN
    FirstHVACSoln = .TRUE.
  ELSE
    FirstHVACSoln = .FALSE.
  END IF
  SupHeaterLoad =  Par(3)
  QCoilActual = SupHeaterLoad
  mdot = HWFlow

  Call SetComponentFlowRate( mdot , &
                             PTUnit(PTUnitNum)%HotWaterControlNode, &
                             PTUnit(PTUnitNum)%PlantCoilOutletNode, &
                             PTUnit(PTUnitNum)%LoopNum, &
                             PTUnit(PTUnitNum)%LoopSide, &
                             PTUnit(PTUnitNum)%BranchNum, &
                             PTUnit(PTUnitNum)%CompNum)
  ! simulate the hot water supplemental heating coil
  CALL SimulateWaterCoilComponents(PTUnit(PTUnitNum)%SuppHeatCoilName,FirstHVACSoln, &
                                   PTUnit(PTUnitNum)%SuppHeatCoilIndex, QCoilActual, &
                                   PTUnit(PTUnitNum)%OpMode)

  IF (SupHeaterLoad /= 0.0d0) THEN
    Residuum = (QCoilActual - SupHeaterLoad)/ SupHeaterLoad
  ELSE !Objexx:Return ELSE added to assure return value is set
    Residuum = 0.0d0
  ENDIF
  RETURN
END FUNCTION HotWaterCoilResidual

REAL(r64) FUNCTION SupSATResidual(TempSupHeater,Par)
          ! FUNCTION INFORMATION:
          !       AUTHOR         Richard Raustad
          !       DATE WRITTEN   July 2005
          !       MODIFIED
          !       RE-ENGINEERED

          ! PURPOSE OF THIS FUNCTION:
          !  Calculates residual function (outlet temp - maximum supplemental heater SAT)
          !  Outlet temperature depends on the supplemental heater load which is being varied to zero the residual.

          ! METHODOLOGY EMPLOYED:
          !  Calls SimulateHeatingCoilComponents to get outlet temperature minus the maximum supplemental heater SAT
          !  at the given supplemental heater load and calculates the residual as defined above

          ! REFERENCES:

          ! USE STATEMENTS:
  USE HeatingCoils, ONLY: SimulateHeatingCoilComponents

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  REAL(r64), INTENT(INOUT)     :: TempSupHeater ! supplemental heater load at maximum SAT
  REAL(r64), INTENT(IN), DIMENSION(:), OPTIONAL :: Par ! par(1) = PTUnitNum
                                                  ! par(2) = FirstHVACIteration

          ! FUNCTION PARAMETER DEFINITIONS:
          !  na

          ! INTERFACE BLOCK SPECIFICATIONS
          !  na

          ! DERIVED TYPE DEFINITIONS
          !  na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER :: PTUnitNum            ! PTHP index
  LOGICAL :: FirstHVACIteration ! FirstHVACIteration flag

  PTUnitNum = INT(Par(1))
  ! FirstHVACIteration is a logical, Par is real, so make 1.0=TRUE and 0.0=FALSE
  IF(Par(2) .EQ. 1.0d0)THEN
    FirstHVACIteration = .TRUE.
  ELSE
    FirstHVACIteration = .FALSE.
  END IF
  CALL SimulateHeatingCoilComponents(PTUnit(PTUnitNum)%SuppHeatCoilName,FirstHVACIteration,TempSupHeater,  &
                                     PTUnit(PTUnitNum)%SuppHeatCoilIndex)
  SupSATResidual = Node(PTUnit(PTUnitNum)%AirOutNode)%Temp - PTUnit(PTUnitNum)%MaxSATSupHeat

  RETURN
END FUNCTION SupSATResidual

REAL(r64) FUNCTION PLRResidual(PartLoadFrac,Par)
          ! FUNCTION INFORMATION:
          !       AUTHOR         Richard Raustad
          !       DATE WRITTEN   July 2005
          !       MODIFIED
          !       RE-ENGINEERED

          ! PURPOSE OF THIS FUNCTION:
          !  Calculates residual function ((ActualOutput - QZnReq)/QZnReq)
          !  PTHP output depends on the part load ratio which is being varied to zero the residual.

          ! METHODOLOGY EMPLOYED:
          !  Calls CalcPTHP to get ActualOutput at the given part load ratio
          !  and calculates the residual as defined above

          ! REFERENCES:

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  REAL(r64), INTENT(IN)     :: PartLoadFrac ! compressor cycling ratio (1.0 is continuous, 0.0 is off)
  REAL(r64), INTENT(IN), DIMENSION(:), OPTIONAL :: Par ! par(1) = PTUnitNum
                                                  ! par(2) = Zone Num
                                                  ! par(3) = FirstHVACIteration
                                                  ! par(4) = OpMode
                                                  ! par(5) = QZnReq
                                                  ! par(6) = OnOffAirFlowRatio
                                                  ! par(7) = SupHeaterLoad

          ! FUNCTION PARAMETER DEFINITIONS:
          !  na

          ! INTERFACE BLOCK SPECIFICATIONS
          !  na

          ! DERIVED TYPE DEFINITIONS
          !  na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER :: PTUnitNum            ! PTHP index
  INTEGER :: ZoneNum            ! Zone index
  LOGICAL :: FirstHVACIteration ! FirstHVACIteration flag
  INTEGER :: OpMode             ! Compressor operating mode
  REAL(r64)    :: QZnReq             ! zone load (W)
  REAL(r64)    :: QZnReqTemp         ! denominator representing zone load (W)
  REAL(r64)    :: OnOffAirFlowRatio  ! ratio of compressor ON airflow to average airflow over timestep
  REAL(r64)    :: ActualOutput       ! delivered capacity of PTHP
  REAL(r64)    :: SupHeaterLoad      ! load passed to supplemental heater (W)
  LOGICAL :: HXUnitOn           ! flag to enable heat exchanger

  PTUnitNum = INT(Par(1))
  ZoneNum = INT(Par(2))
  ! FirstHVACIteration is a logical, Par is real, so make 1.0=TRUE and 0.0=FALSE
  IF(Par(3) .EQ. 1.0d0)THEN
    FirstHVACIteration = .TRUE.
  ELSE
    FirstHVACIteration = .FALSE.
  END IF
  OpMode = INT(Par(4))
  QZnReq = Par(5)
  QZnReqTemp = QZnReq
  IF(ABS(QZnReq) .LT. 100.d0)QZnReqTemp=SIGN(100.d0,QZnReq)
  OnOffAirFlowRatio = Par(6)
  SupHeaterLoad     = Par(7) * PartLoadFrac
  IF(Par(8) .EQ. 1.0d0)THEN
    HXUnitOn = .TRUE.
  ELSE
    HXUnitOn = .FALSE.
  END IF

  CALL CalcPTUnit(PTUnitNum,FirstHVACIteration,PartLoadFrac,ActualOutput,QZnReq,OnOffAirFlowRatio,SupHeaterLoad, HXUnitOn)
  PLRResidual = (ActualOutput - QZnReq)/QZnReqTemp

  RETURN
END FUNCTION PLRResidual

SUBROUTINE SetAverageAirFlow(PTUnitNum,PartLoadRatio,OnOffAirFlowRatio)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Raustad
          !       DATE WRITTEN   July 2005
          !       MODIFIED       Chandan Sharma, FSEC, March 2011: Added ZoneHVAC sys avail manager
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Set the average air mass flow rates using the part load fraction of the heat pump for this time step
          ! Set OnOffAirFlowRatio to be used by DX coils

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataZoneEnergyDemands

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN)    :: PTUnitNum            ! Unit index
  REAL(r64)   , INTENT (IN)    :: PartLoadRatio      ! unit part load ratio
  REAL(r64)   , INTENT (INOUT) :: OnOffAirFlowRatio  ! ratio of compressor ON airflow to average airflow over timestep

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER             :: InletNode           ! inlet node number for PTUnitNum
  INTEGER             :: OutsideAirNode      ! outside air node number in PTHP loop
  INTEGER             :: AirRelNode          ! relief air node number in PTHP loop
  REAL(r64)           :: AverageUnitMassFlow ! average supply air mass flow rate over time step
  REAL(r64)           :: AverageOAMassFlow   ! average outdoor air mass flow rate over time step

  InletNode      = PTUnit(PTUnitNum)%AirInNode
  OutsideAirNode = PTUnit(PTUnitNum)%OutsideAirNode
  AirRelNode     = PTUnit(PTUnitNum)%AirReliefNode

  AverageUnitMassFlow = (PartLoadRatio * CompOnMassFlow) + ((1-PartLoadRatio) * CompOffMassFlow)
  AverageOAMassFlow   = (PartLoadRatio * OACompOnMassFlow) + ((1-PartLoadRatio) * OACompOffMassFlow)
  IF(CompOffFlowRatio .GT. 0.0d0)THEN
    FanSpeedRatio     = (PartLoadRatio * CompOnFlowRatio) + ((1-PartLoadRatio) * CompOffFlowRatio)
  ELSE
    FanSpeedRatio     = CompOnFlowRatio
  END IF

  IF ( GetCurrentScheduleValue(PTUnit(PTUnitNum)%SchedPtr) .GT. 0.0d0 &
     .AND. ((GetCurrentScheduleValue(PTUnit(PTUnitNum)%FanAvailSchedPtr) .GT. 0.0d0 .OR. &
      ZoneCompTurnFansOn) .AND. .NOT. ZoneCompTurnFansOff))THEN

    Node(InletNode)%MassFlowRate              = AverageUnitMassFlow
    Node(InletNode)%MassFlowRateMaxAvail      = AverageUnitMassFlow
    IF(OutsideAirNode .GT. 0)THEN
      Node(OutsideAirNode)%MassFlowRate         = AverageOAMassFlow
      Node(OutsideAirNode)%MassFlowRateMaxAvail = AverageOAMassFlow
      Node(AirRelNode)%MassFlowRate             = AverageOAMassFlow
      Node(AirRelNode)%MassFlowRateMaxAvail     = AverageOAMassFlow
    END IF
    IF (AverageUnitMassFlow .GT. 0.0d0) THEN
      OnOffAirFlowRatio                       = CompOnMassFlow / AverageUnitMassFlow
    ELSE
      OnOffAirFlowRatio                       = 0.0d0
    END IF

  ELSE

    Node(InletNode)%MassFlowRate              = 0.0d0
    IF(OutsideAirNode .GT. 0)THEN
      Node(OutsideAirNode)%MassFlowRate       = 0.0d0
      Node(AirRelNode)%MassFlowRate           = 0.0d0
    END IF
    OnOffAirFlowRatio                         = 0.0d0

  END IF

  RETURN
END SUBROUTINE SetAverageAirFlow

SUBROUTINE ReportPTUnit(PTUnitNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Raustad
          !       DATE WRITTEN   July 2005
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Fills some of the report variables for the packaged terminal heat pump

          ! METHODOLOGY EMPLOYED:
          ! NA

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! NA

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN) :: PTUnitNum ! number of the current AC unit being simulated

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64) :: ReportingConstant

          ! FLOW

  ReportingConstant = TimeStepSys*SecInHour
  PTUnit(PTUnitNum)%TotCoolEnergy   = PTUnit(PTUnitNum)%TotCoolEnergyRate * ReportingConstant
  PTUnit(PTUnitNum)%TotHeatEnergy   = PTUnit(PTUnitNum)%TotHeatEnergyRate * ReportingConstant
  PTUnit(PTUnitNum)%SensCoolEnergy  = PTUnit(PTUnitNum)%SensCoolEnergyRate * ReportingConstant
  PTUnit(PTUnitNum)%SensHeatEnergy  = PTUnit(PTUnitNum)%SensHeatEnergyRate * ReportingConstant
  PTUnit(PTUnitNum)%LatCoolEnergy   = PTUnit(PTUnitNum)%LatCoolEnergyRate * ReportingConstant
  PTUnit(PTUnitNum)%LatHeatEnergy   = PTUnit(PTUnitNum)%LatHeatEnergyRate * ReportingConstant
  PTUnit(PTUnitNum)%ElecConsumption = PTUnit(PTUnitNum)%ElecPower * ReportingConstant

  RETURN
END SUBROUTINE ReportPTUnit

INTEGER FUNCTION GetPTUnitZoneInletAirNode(PTUnitCompIndex, PTUnitType)

          ! FUNCTION INFORMATION:
          !       AUTHOR         B Griffith
          !       DATE WRITTEN   Dec  2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! lookup function for zone air inlet node

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataZoneEquipment, ONLY: PkgTermHPAirToAir_Num, PkgTermHPWaterToAir_Num, PkgTermACAirToAir_Num

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN)  :: PTUnitCompIndex
  INTEGER, INTENT (IN)  :: PTUnitType

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER :: PTUnitNum

  IF (GetPTUnitInputFlag) THEN
    CALL GetPTUnit
    GetPTUnitInputFlag = .FALSE.
  END IF

  GetPTUnitZoneInletAirNode = 0

  ! PTHP, PTAC and PTWSHP share the same data structure which was allocated to total number of all three
  ! objects in the input file. Whereas zone availability manager associated with each type (i.e. PTAC, PTHP and PTWSHP)
  ! is allocated to total number of objects of each type. This led to crash for the case when PTAC, PTHP and PTWSHP
  ! objects were mixed together. To address this, CompIndex in SimPackagedTerminalUnit is calculated for each object type.
  ! So ZoneEquipList()%EquipIndex() passed to this subroutine (which is actually CompIndex), is based on each object type
  ! which was recalculated for total number of all three object type for use in PT data structure.

  SELECT CASE (PTUnitType)
    CASE (PkgTermHPAirToAir_Num)
      PTUnitNum = PTUnitCompIndex
    CASE (PkgTermACAirToAir_Num)
      PTUnitNum = PTUnitCompIndex + NumPTHP
    CASE (PkgTermHPWaterToAir_Num)
      PTUnitNum = PTUnitCompIndex + NumPTHP + NumPTAC
    CASE DEFAULT
  END SELECT

  IF (PTUnitNum > 0 .and. PTUnitNum <= NumPTUs) THEN
    GetPTUnitZoneInletAirNode = PTUnit(PTUnitNum)%AirOutNode
  ENDIF

  RETURN

END FUNCTION GetPTUnitZoneInletAirNode

INTEGER FUNCTION GetPTUnitOutAirNode(PTUnitCompIndex, PTUnitType)

          ! FUNCTION INFORMATION:
          !       AUTHOR         B Griffith
          !       DATE WRITTEN   Dec  2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! lookup function for OA inlet node

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataZoneEquipment, ONLY: PkgTermHPAirToAir_Num, PkgTermHPWaterToAir_Num, PkgTermACAirToAir_Num

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN)  :: PTUnitCompIndex
  INTEGER, INTENT (IN)  :: PTUnitType

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER :: PTUnitNum

  IF (GetPTUnitInputFlag) THEN
    CALL GetPTUnit
    GetPTUnitInputFlag = .FALSE.
  END IF

  GetPTUnitOutAirNode = 0

  ! PTHP, PTAC and PTWSHP share the same data structure which was allocated to total number of all three
  ! objects in the input file. Whereas zone availability manager associated with each type (i.e. PTAC, PTHP and PTWSHP)
  ! is allocated to total number of objects of each type. This led to crash for the case when PTAC, PTHP and PTWSHP
  ! objects were mixed together. To address this, CompIndex in SimPackagedTerminalUnit is calculated for each object type.
  ! So ZoneEquipList()%EquipIndex() passed to this subroutine (which is actually CompIndex), is based on each object type
  ! which was recalculated for total number of all three object type for use in PT data structure.

  SELECT CASE (PTUnitType)
    CASE (PkgTermHPAirToAir_Num)
      PTUnitNum = PTUnitCompIndex
    CASE (PkgTermACAirToAir_Num)
      PTUnitNum = PTUnitCompIndex + NumPTHP
    CASE (PkgTermHPWaterToAir_Num)
      PTUnitNum = PTUnitCompIndex + NumPTHP + NumPTAC
    CASE DEFAULT
  END SELECT

  IF (PTUnitNum > 0 .and. PTUnitNum <= NumPTUs) THEN

    GetPTUnitOutAirNode = PTUnit(PTUnitNum)%OutsideAirNode
  ENDIF

  RETURN

END FUNCTION GetPTUnitOutAirNode

INTEGER FUNCTION GetPTUnitReturnAirNode(PTUnitCompIndex, PTUnitType)

          ! FUNCTION INFORMATION:
          !       AUTHOR         B Griffith
          !       DATE WRITTEN   Dec  2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! lookup function for mixer return air node

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE MixedAir, ONLY: GetOAMixerReturnNodeNumber
  USE DataZoneEquipment, ONLY: PkgTermHPAirToAir_Num, PkgTermHPWaterToAir_Num, PkgTermACAirToAir_Num

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN)  :: PTUnitCompIndex
  INTEGER, INTENT (IN)  :: PTUnitType

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER :: PTUnitNum

  IF (GetPTUnitInputFlag) THEN
    CALL GetPTUnit
    GetPTUnitInputFlag = .FALSE.
  END IF

  ! PTHP, PTAC and PTWSHP share the same data structure which was allocated to total number of all three
  ! objects in the input file. Whereas zone availability manager associated with each type (i.e. PTAC, PTHP and PTWSHP)
  ! is allocated to total number of objects of each type. This led to crash for the case when PTAC, PTHP and PTWSHP
  ! objects were mixed together. To address this, CompIndex in SimPackagedTerminalUnit is calculated for each object type.
  ! So ZoneEquipList()%EquipIndex() passed to this subroutine (which is actually CompIndex), is based on each object type
  ! which was recalculated for total number of all three object type for use in PT data structure.

  SELECT CASE (PTUnitType)
    CASE (PkgTermHPAirToAir_Num)
      PTUnitNum = PTUnitCompIndex
    CASE (PkgTermACAirToAir_Num)
      PTUnitNum = PTUnitCompIndex + NumPTHP
    CASE (PkgTermHPWaterToAir_Num)
      PTUnitNum = PTUnitCompIndex + NumPTHP + NumPTAC
    CASE DEFAULT
  END SELECT

  IF (PTUnitNum > 0 .and. PTUnitNum <= NumPTUs) THEN
    IF (PTUnit(PTUnitNum)%OAMixIndex > 0)  THEN
      GetPTUnitReturnAirNode = GetOAMixerReturnNodeNumber( PTUnit(PTUnitNum)%OAMixIndex )
    ELSE
      GetPTUnitReturnAirNode = 0
    END IF
  ELSE
    GetPTUnitReturnAirNode = 0
  END IF

  RETURN

END FUNCTION GetPTUnitReturnAirNode

INTEGER FUNCTION GetPTUnitMixedAirNode(PTUnitCompIndex, PTUnitType)

          ! FUNCTION INFORMATION:
          !       AUTHOR         B Griffith
          !       DATE WRITTEN   Dec  2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! lookup function for mixed air node

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE MixedAir, ONLY: GetOAMixerMixedNodeNumber
  USE DataZoneEquipment, ONLY: PkgTermHPAirToAir_Num, PkgTermHPWaterToAir_Num, PkgTermACAirToAir_Num

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN)  :: PTUnitCompIndex
  INTEGER, INTENT (IN)  :: PTUnitType

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER :: PTUnitNum

  IF (GetPTUnitInputFlag) THEN
    CALL GetPTUnit
    GetPTUnitInputFlag = .FALSE.
  END IF

  ! PTHP, PTAC and PTWSHP share the same data structure which was allocated to total number of all three
  ! objects in the input file. Whereas zone availability manager associated with each type (i.e. PTAC, PTHP and PTWSHP)
  ! is allocated to total number of objects of each type. This led to crash for the case when PTAC, PTHP and PTWSHP
  ! objects were mixed together. To address this, CompIndex in SimPackagedTerminalUnit is calculated for each object type.
  ! So ZoneEquipList()%EquipIndex() passed to this subroutine (which is actually CompIndex), is based on each object type
  ! which was recalculated for total number of all three object type for use in PT data structure.

  SELECT CASE (PTUnitType)
    CASE (PkgTermHPAirToAir_Num)
      PTUnitNum = PTUnitCompIndex
    CASE (PkgTermACAirToAir_Num)
      PTUnitNum = PTUnitCompIndex + NumPTHP
    CASE (PkgTermHPWaterToAir_Num)
      PTUnitNum = PTUnitCompIndex + NumPTHP + NumPTAC
    CASE DEFAULT
  END SELECT

  IF (PTUnitNum > 0 .and. PTUnitNum <= NumPTUs) THEN
    IF (PTUnit(PTUnitNum)%OAMixIndex > 0)  THEN
      GetPTUnitMixedAirNode = GetOAMixerMixedNodeNumber( PTUnit(PTUnitNum)%OAMixIndex )
    ELSE
      GetPTUnitMixedAirNode = 0
    END IF
  ELSE
    GetPTUnitMixedAirNode = 0
  END IF

  RETURN

END FUNCTION GetPTUnitMixedAirNode

!******************************************************************************

SUBROUTINE SimVariableSpeedHP(PTUnitNum,ZoneNum, FirstHVACIteration, QZnReq, QLatReq, OnOffAirFlowRatio, OpMode, HXUnitOn )

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Bo Shen, based on HVACMultiSpeedHeatPump:CalcMSHeatPump
          !       DATE WRITTEN   March, 2012
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Simulate a multispeed heat pump; adjust its output to match the
          ! required system load.

          ! METHODOLOGY EMPLOYED:
          ! Calls ControlMSHPOutput to obtain the desired unit output

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataZoneEnergyDemands
  USE DataZoneEquipment, ONLY: ZoneEquipConfig
  USE DataAirLoop, ONLY: AirLoopControlInfo,  AirToZoneNodeInfo
  USE DataAirSystems,  ONLY: PrimaryAirSystem

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  LOGICAL, INTENT (IN)    :: FirstHVACIteration ! TRUE if 1st HVAC simulation of system timestep
  INTEGER, INTENT (IN)    :: PTUnitNum      ! number of the current engine driven Heat Pump being simulated
  REAL(r64),    INTENT (IN)    :: QZnReq             ! required zone load
  REAL(r64),    INTENT (IN)    :: QLatReq            ! required latent load
  INTEGER,      INTENT (IN)    :: ZoneNum           ! Controlled zone number
  REAL(r64),    INTENT (INOUT) :: OnOffAirFlowRatio  ! ratio of compressor ON airflow to AVERAGE airflow over timestep
  INTEGER, INTENT   (IN)  :: OpMode             ! operating mode: CycFanCycCoil | ContFanCycCoil
  LOGICAL,      INTENT (IN)    :: HXUnitOn          ! flag to enable heat exchanger
          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64)    :: PartLoadFrac = 0.0d0     ! compressor part load fraction
  REAL(r64)    :: SpeedRatio = 0.0d0       ! compressor speed ratio
  LOGICAL :: UnitOn            ! TRUE if unit is on
  INTEGER :: OutletNode        ! MSHP air outlet node
  INTEGER :: InletNode         ! MSHP air inlet node
  REAL(r64)    :: AirMassFlow       ! air mass flow rate [kg/s]
  REAL(r64)    :: QTotUnitOut  ! capacity output
  INTEGER :: SpeedNum   = 1       ! Speed number
  REAL(r64)    :: SupHeaterLoad ! supplement heater load
  INTEGER :: AirLoopNumber     ! Index to air loop
  REAL(r64)    :: SaveMassFlowRate  ! saved inlet air mass flow rate [kg/s]
  REAL(r64)    :: QSensUnitOut    ! sensible capacity output
  REAL(r64)    :: QLatUnitOut     ! latent capacity output
  INTEGER      :: CompOp             ! compressor operation; 1=on, 0=off
  REAL(r64), SAVE        :: TotalZoneLatentLoad    ! Total ZONE heating load (not including outside air)
  INTEGER           ::  TotBranchNum ! total outlet branch number
  INTEGER           ::  ZoneSideNodeNum ! zone equip supply node
  LOGICAL           ::  EconoActive        ! TRUE if Economizer is active

  ! zero the fan, DX coils, and supplemental electric heater electricity consumption
  FanElecPower         = 0.0d0
  DXElecHeatingPower   = 0.0d0
  DXElecCoolingPower   = 0.0d0
  SaveCompressorPLR    = 0.0d0
  ElecHeatingCoilPower = 0.0d0

  ! initialize local variables
  UnitOn      = .TRUE.
  CompOp      = 1
  OutletNode  = PTUnit(PTUnitNum)%AirOutNode
  InletNode   = PTUnit(PTUnitNum)%AirInNode
  AirMassFlow = PTUnit(PTUnitNum)%MaxCoolAirMassFlow

   !Set latent load for heating
  IF(HeatingLoad)THEN
      TotalZoneLatentLoad = 0.0d0
      PTUnit(PTUnitNum)%HeatCoolMode = HeatingMode
 !Set latent load for cooling and no sensible load condition
  ELSE
      TotalZoneLatentLoad = QLatReq
      PTUnit(PTUnitNum)%HeatCoolMode = CoolingMode
  ENDIF

  If (HeatingLoad) then
    PTUnit(PTUnitNum)%HeatCoolMode = HeatingMode
  Else If (CoolingLoad) then
    PTUnit(PTUnitNum)%HeatCoolMode = CoolingMode
  Else
    PTUnit(PTUnitNum)%HeatCoolMode = 0
  End If

  ! set the on/off flags
  IF (PTUnit(PTUnitNum)%OPMode == CycFanCycCoil) THEN
    ! cycling unit only runs if there is a cooling or heating load.
     IF (ABS(QZnReq) < SmallLoad .OR. AirMassFlow < SmallMassFlow .OR. CurDeadbandOrSetback(ZoneNum)) THEN
       UnitOn = .FALSE.
     END IF
  ELSE IF  (PTUnit(PTUnitNum)%OPMode == ContFanCycCoil) THEN
    ! continuous unit: fan runs if scheduled on; coil runs only if there is a cooling or heating load
    IF (AirMassFlow.LT.SmallMassFlow) THEN
      UnitOn = .FALSE.
    END IF
  END IF

  OnOffFanPartLoadFraction = 1.0d0

  AirLoopNumber = ZoneEquipConfig(ZoneNum)%AirLoopNum

  IF(AirLoopNumber /= 0) THEN
    EconoActive = AirLoopControlInfo(AirLoopNumber)%EconoActive
  ELSE
    EconoActive = .FALSE.
  END IF

  SaveMassFlowRate = Node(InletNode)%MassFlowRate
  IF ( .NOT. FirstHVACIteration .AND. PTUnit(PTUnitNum)%OPMode == CycFanCycCoil .AND. &
        (QZnReq < (-1.d0*SmallLoad) .OR. TotalZoneLatentLoad > SmallLoad  ) &
       .AND. EconoActive ) THEN
       ! for cycling fan, cooling load, check whether furnace can meet load with compressor off
    CompOp = Off
    CALL ControlVSHPOutput(PTUnitNum,FirstHVACIteration,CompOp,OpMode,QZnReq, &
                            TotalZoneLatentLoad,ZoneNum,SpeedNum,SpeedRatio, &
                         PartLoadFrac,OnOffAirFlowRatio,SupHeaterLoad, HXUnitOn )

    IF (SpeedNum .EQ. PTUnit(PTUnitNum)%NumOfSpeedCooling .AND. SpeedRatio .EQ. 1.0d0) THEN
      ! compressor on (reset inlet air mass flow rate to starting value)
      Node(InletNode)%MassFlowRate = SaveMassFlowRate
      CompOp = On
      CALL ControlVSHPOutput(PTUnitNum,FirstHVACIteration,CompOp,OpMode,QZnReq,TotalZoneLatentLoad,&
                            ZoneNum,SpeedNum,SpeedRatio, &
                         PartLoadFrac,OnOffAirFlowRatio,SupHeaterLoad, HXUnitOn )
    END IF
  ELSE
     ! compressor on
     CompOp      = On
     CALL ControlVSHPOutput(PTUnitNum,FirstHVACIteration,CompOp,OpMode,QZnReq,TotalZoneLatentLoad,&
                            ZoneNum,SpeedNum,SpeedRatio, &
                         PartLoadFrac,OnOffAirFlowRatio,SupHeaterLoad, HXUnitOn )
  END IF

  IF (PTUnit(PTUnitNum)%UnitType_Num .EQ. PTACUnit) THEN
    SaveCompressorPLR = PartLoadFrac
  ELSE
    IF(SpeedNum > 1)  THEN
      SaveCompressorPLR = 1.0d0
    END IF

    If (PartLoadFrac .eq. 1.0d0 .and. SaveCompressorPLR < 1.0d0) then
      PartLoadFrac = SaveCompressorPLR
    End If
  END IF

  CALL CalcVarSpeedHeatPump(PTUnitNum, ZoneNum, FirstHVACIteration,CompOp,SpeedNum,SpeedRatio,PartLoadFrac,&
                        QSensUnitOut, QLatUnitOut, &
                      QZnReq,TotalZoneLatentLoad,OnOffAirFlowRatio,SupHeaterLoad, HXUnitOn )

  ! calculate delivered capacity
  AirMassFlow = Node(InletNode)%MassFlowRate

 QTotUnitOut = AirMassFlow * (Node(OutletNode)%Enthalpy - Node(InletNode)%Enthalpy)

  Node(InletNode)%MassFlowRateMaxAvail = AirMassFlow
  Node(OutletNode)%MassFlowRateMaxAvail = AirMassFlow

  IF(.NOT. FirstHVACIteration .AND. AirMassFlow > 0.0d0 .AND. AirLoopNumber > 0 ) THEN
      TotBranchNum = PrimaryAirSystem(AirLoopNumber)%NumOutletBranches
      IF(TotBranchNum .EQ. 1) THEN
         ZoneSideNodeNum = AirToZoneNodeInfo(AirLoopNumber)%ZoneEquipSupplyNodeNum(1)
! THE MASS FLOW PRECISION of the system solver is not enough for some small air flow rate iterations , BY DEBUGGING
! it may cause mass flow rate occilations between airloop and zoneequip
! specify the air flow rate directly for one-to-one system, when the iteration deviation is closing the solver precision level
         IF(ABS(AirMassFlow - Node(ZoneSideNodeNum)%MassFlowRate) < 0.02d0) &
         ! 0.02 is 2 * HVACFlowRateToler, in order to accomodate the system solver precision level
           Node(ZoneSideNodeNum)%MassFlowRateMaxAvail = AirMassFlow
           Node(ZoneSideNodeNum)%MassFlowRate = AirMassFlow
      END IF

     ! the below might be useful if more divergences occur
     ! Node(PrimaryAirSystem(AirLoopNumber)%Branch(1)%NodeNumIn)%MassFlowRateMaxAvail = AirMassFlow
     ! Node(PrimaryAirSystem(AirLoopNumber)%Branch(1)%NodeNumIn)%MassFlowRate = AirMassFlow
  END IF


  ! report variables
  PTUnit(PTUnitNum)%CompPartLoadRatio = SaveCompressorPLR
  IF (PTUnit(PTUnitNum)%OpMode .EQ. CycFanCycCoil) THEN
    If (SupHeaterLoad >0.0d0) Then
      PTUnit(PTUnitNum)%FanPartLoadRatio = 1.0d0
    Else
      If (SpeedNum .LT. 2) Then
        PTUnit(PTUnitNum)%FanPartLoadRatio = PartLoadFrac
      Else
        PTUnit(PTUnitNum)%FanPartLoadRatio = 1.0d0
      End If
    End If
  ELSE
    IF (UnitOn) THEN
      PTUnit(PTUnitNum)%FanPartLoadRatio = 1.0d0
    ELSE
      If (SpeedNum .LT. 2) Then
        PTUnit(PTUnitNum)%FanPartLoadRatio = PartLoadFrac
      Else
        PTUnit(PTUnitNum)%FanPartLoadRatio = 1.0d0
      End If
    END IF
  END IF

  RETURN
END SUBROUTINE SimVariableSpeedHP
!******************************************************************************
!******************************************************************************

SUBROUTINE ControlVSHPOutput(PTUnitNum, FirstHVACIteration,CompOp,OpMode,&
                QZnReq, QLatReq, ZoneNum,SpeedNum,SpeedRatio,PartLoadFrac, &
                             OnOffAirFlowRatio,SupHeaterLoad, HXUnitOn )

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Bo Shen, based on HVACMultiSpeedHeatPump:ControlMSHPOutput
          !       DATE WRITTEN   March,  2012
          !       MODIFIED       na
          !       RE-ENGINEERED

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
  USE SteamCoils,                ONLY: SimulateSteamCoilComponents
  USE WaterCoils,                ONLY: SimulateWaterCoilComponents
  USE PlantUtilities,            ONLY: SetComponentFlowRate
  USE DataEnvironment,           ONLY: OutDryBulbTemp
  USE DataZoneEnergyDemands,     ONLY: CurDeadbandOrSetback

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER,   INTENT    (IN) :: PTUnitNum              ! Unit index in fan coil array
  LOGICAL, INTENT   (IN)  :: FirstHVACIteration ! flag for 1st HVAC iteration in the time step
  INTEGER, INTENT   (IN)  :: CompOp             ! compressor operation; 1=on, 0=off
  INTEGER, INTENT   (IN)  :: OpMode             ! operating mode: CycFanCycCoil | ContFanCycCoil
  INTEGER, INTENT   (OUT) :: SpeedNum           ! Speed number
  REAL(r64)   , INTENT   (IN)  :: QZnReq        ! cooling or heating output needed by zone [W]
  REAL(r64)   , INTENT   (IN)  :: QLatReq       ! latent cooling output needed by zone [W]
  INTEGER, INTENT   (IN)  :: ZoneNum            ! Index to zone number
  REAL(r64)   , INTENT   (OUT) :: SpeedRatio         ! unit speed ratio for DX coils
  REAL(r64)   , INTENT   (OUT) :: PartLoadFrac       ! unit part load fraction
  REAL(r64)   , INTENT (INOUT) :: OnOffAirFlowRatio  ! ratio of compressor ON airflow to AVERAGE airflow over timestep
  REAL(r64)   , INTENT (INOUT) :: SupHeaterLoad      ! Supplemental heater load [W]
  LOGICAL,   INTENT    (IN) :: HXUnitOn             ! flag to enable heat exchanger

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
  REAL(r64)          :: LatOutput     ! latent capacity output
  REAL(r64)          :: ErrorToler    ! error tolerance
  INTEGER            :: SolFla        ! Flag of RegulaFalsi solver
  REAL(r64), DIMENSION(11) :: Par           ! Parameters passed to RegulaFalsi
  REAL(r64)          :: CpAir         ! air specific heat
  REAL(r64)          :: QCoilActual   ! coil load actually delivered returned to calling component
  INTEGER            :: i             ! Speed index
  INTEGER,SAVE       :: ErrCountCyc=0 ! Counter used to minimize the occurrence of output warnings
  INTEGER,SAVE       :: ErrCountVar=0 ! Counter used to minimize the occurrence of output warnings
  REAL(r64)                 :: mdot               ! coil fluid mass flow rate (kg/s)

  ! FLOW
  SupHeaterLoad = 0.0d0
  PartLoadFrac  = 0.0d0
  SpeedRatio    = 0.0d0
  SpeedNum = 1
  LatOutput = 0.0d0
  ErrorToler = 0.001d0 !Error tolerance for convergence from input deck

  IF (GetCurrentScheduleValue(PTUnit(PTUnitNum)%SchedPtr) .EQ. 0.0d0) RETURN

  ! Get result when DX coil is off
  CALL CalcVarSpeedHeatPump(PTUnitNum,ZoneNum, FirstHVACIteration,CompOp,SpeedNum,SpeedRatio,PartLoadFrac,NoCompOutput, LatOutput, &
                          QZnReq, QLatReq, OnOffAirFlowRatio,SupHeaterLoad, HXUnitOn)

  ! If cooling and NoCompOutput < QZnReq, the coil needs to be off
  ! If heating and NoCompOutput > QZnReq, the coil needs to be off
  IF ((QZnReq < (-1.d0*SmallLoad) .AND. NoCompOutput < QZnReq) .OR. (QZnReq > SmallLoad .AND. NoCompOutput > QZnReq) &
    .OR. (ABS(QZnReq) <= SmallLoad .AND. ABS(QLatReq) <= SmallLoad ) .OR. CurDeadbandOrSetback(ZoneNum)) THEN
    RETURN
  END IF

  ! Get full load result
  PartLoadFrac  = 1.0d0
  SpeedRatio    = 1.0d0
  If (PTUnit(PTUnitNum)%HeatCoolMode == HeatingMode) Then
    IF (PTUnit(PTUnitNum)%UnitType_Num .EQ. PTACUnit) THEN
        SpeedNum = PTUnit(PTUnitNum)%NumOfSpeedCooling
    ELSE
        SpeedNum = PTUnit(PTUnitNum)%NumOfSpeedHeating
    END IF
  Else If (PTUnit(PTUnitNum)%HeatCoolMode == CoolingMode) Then
    SpeedNum = PTUnit(PTUnitNum)%NumOfSpeedCooling
  ELSE
    SpeedNum = 1
    PartLoadFrac  = 0.0d0
  End If

  CALL CalcVarSpeedHeatPump(PTUnitNum,ZoneNum, FirstHVACIteration,CompOp,SpeedNum,SpeedRatio,PartLoadFrac,FullOutput, LatOutput, &
                          QZnReq, QLatReq, OnOffAirFlowRatio,SupHeaterLoad, HXUnitOn)

  IF(QLatReq < 0.0d0 ) THEN !dehumidification mode
  !  ! If the QLatReq <= LatOutput the unit needs to run full out
    IF (QLatReq <= LatOutput) THEN
      PartLoadFrac = 1.0d0
      SpeedRatio   = 1.0d0
      PTUnit(PTUnitNum)%CompPartLoadRatio = PartLoadFrac
      PTUnit(PTUnitNum)%CompSpeedRatio = SpeedRatio
      PTUnit(PTUnitNum)%CompSpeedNum = SpeedNum
      RETURN
    END IF
    ErrorToler = 0.001d0 !Error tolerance for convergence from input deck
  ELSE IF (QZnReq .LT. (-1.d0*SmallLoad) .AND. .NOT. CurDeadbandorSetback(ZoneNum)) THEN
  ! Since we are cooling, we expect FullOutput to be < 0 and FullOutput < NoCompOutput
  ! Check that this is the case; if not set PartLoadFrac = 0.0 (off) and return
    IF (FullOutput >= 0.0d0 .OR. FullOutput >= NoCompOutput) THEN
      PartLoadFrac = 0.0d0
      SpeedRatio   = 0.0d0
      SpeedNum = 1
      RETURN
    END IF
!  ! If the QZnReq <= FullOutput the unit needs to run full out
    IF (QZnReq <= FullOutput) THEN
      PartLoadFrac = 1.0d0
      SpeedRatio   = 1.0d0
      PTUnit(PTUnitNum)%CompPartLoadRatio = PartLoadFrac
      PTUnit(PTUnitNum)%CompSpeedRatio = SpeedRatio
      PTUnit(PTUnitNum)%CompSpeedNum = SpeedNum
      RETURN
    END IF
    ErrorToler = 0.001d0 !Error tolerance for convergence from input deck
  ELSEIF (QZnReq > SmallLoad .AND. .NOT. CurDeadbandorSetback(ZoneNum)) THEN
  ! Since we are heating, we expect FullOutput to be > 0 and FullOutput > NoCompOutput
  ! Check that this is the case; if not set PartLoadFrac = 0.0 (off)
    IF (FullOutput <= 0.0d0 .OR. FullOutput <= NoCompOutput) THEN
      PartLoadFrac = 0.0d0
      SpeedRatio   = 0.0d0
      SpeedNum = 1
  ! may need supplemental heating so don't return in heating mode
    END IF
    IF (QZnReq  >=  FullOutput) THEN
      PartLoadFrac = 1.0d0
      SpeedRatio   = 1.0d0
  ! may need supplemental heating so don't return in heating mode
    END IF
    ErrorToler = 0.001d0 !Error tolerance for convergence from input deck
  ELSE ! no load
    PartLoadFrac = 0.0d0
    SpeedRatio   = 0.0d0
    SpeedNum = 1
  END IF

  ! Calculate the part load fraction
  IF (((QZnReq .GT. SmallLoad .AND. QZnReq < FullOutput) .OR. (QZnReq .LT. (-1.d0*SmallLoad) .AND. QZnReq > FullOutput) &
     .OR. (QLatReq < (-1.d0*SmallLoad))) .AND. .NOT. CurDeadbandorSetback(ZoneNum)) THEN

    Par(1) = PTUnitNum
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
    Par(10) = 1.0d0
    IF(HXUnitOn)THEN
      Par(11) = 1.0d0
    ELSE
      Par(11) = 0.0d0
    END IF
    ! Check whether the low speed coil can meet the load or not
    CALL CalcVarSpeedHeatPump(PTUnitNum, ZoneNum, FirstHVACIteration,CompOp,1, 0.0d0, 1.0d0,LowOutput, LatOutput,  &
                        QZnReq, QLatReq, OnOffAirFlowRatio,SupHeaterLoad, HXUnitOn)
    IF (((QZnReq .GT. SmallLoad .AND. QZnReq <= LowOutput) .OR. (QZnReq .LT. (-1.d0*SmallLoad) .AND. QZnReq >= LowOutput) &
              .OR. (QLatReq < (-1.d0*SmallLoad) .AND. QLatReq > LatOutput)) .AND. .NOT. CurDeadbandorSetback(ZoneNum)) THEN
      SpeedRatio = 0.0d0
      SpeedNum = 1

      IF(QLatReq < 0.0d0) THEN !calculate latent heat residual
        Par(10) = 0.0d0
        Par(5) = QLatReq
      END IF

      CALL SolveRegulaFalsi(ErrorToler, MaxIte, SolFla, PartLoadFrac, VSHPCyclingResidual, 0.0d0, 1.0d0, Par)
      IF (SolFla == -1) THEN
        If ( .NOT. WarmupFlag) Then
          IF (ErrCountCyc .eq. 0) THEN
            ErrCountCyc = ErrCountCyc+1
            CALL ShowWarningError('Iteration limit exceeded calculating VS WSHP unit cycling ratio, for unit='// &
                            TRIM(PTUnit(PTUnitNum)%Name))
            CALL ShowContinueErrorTimeStamp('Cycling ratio returned='//RoundSigDigits(PartLoadFrac,2))
          Else
            ErrCountCyc = ErrCountCyc+1
            CALL ShowRecurringWarningErrorAtEnd(TRIM(PTUnit(PTUnitNum)%Name)//'":'//&
          ' Iteration limit warning exceeding calculating DX unit cycling ratio  continues...' &
          ,PTUnit(PTUnitNum)%ErrIndexCyc , PartLoadFrac, PartLoadFrac)
          End If
        End If
      ELSE IF (SolFla == -2) THEN
        CALL ShowFatalError('VS WSHP unit cycling ratio calculation failed: cycling limits exceeded, for unit='// &
                           TRIM(PTUnit(PTUnitNum)%Name))
      END IF
    Else
      ! Check to see which speed to meet the load
      PartLoadFrac = 1.0d0
      SpeedRatio = 1.0d0
       ! Cooling
      If (((QZnReq .LT. (-1.d0*SmallLoad)) .OR. (QLatReq < (-1.d0*SmallLoad))).AND. .NOT. CurDeadbandorSetback(ZoneNum)) Then
        DO I=2,PTUnit(PTUnitNum)%NumOfSpeedCooling
          CALL CalcVarSpeedHeatPump(PTUnitNum,ZoneNum, FirstHVACIteration,CompOp,I,SpeedRatio,PartLoadFrac,TempOutput, LatOutput,&
                              QZnReq,QLatReq, OnOffAirFlowRatio,SupHeaterLoad, HXUnitOn )

          IF(QLatReq < 0.0d0) THEN
            IF(QLatReq > LatOutput) THEN
             SpeedNum = I
             Exit
            END IF
          ELSE IF (QZnReq >= TempOutput) THEN
            SpeedNum = I
            Exit
          END IF

        END DO
      ELSE
        DO I=2,PTUnit(PTUnitNum)%NumOfSpeedHeating
          CALL CalcVarSpeedHeatPump(PTUnitNum,ZoneNum, FirstHVACIteration,CompOp,I,SpeedRatio,PartLoadFrac,TempOutput, LatOutput,&
               QZnReq,QLatReq,OnOffAirFlowRatio,SupHeaterLoad, HXUnitOn )
          If (QZnReq <= TempOutput) Then
            SpeedNum = I
            Exit
          End If
        END DO
      END IF
      Par(8) = SpeedNum

      IF(QLatReq < (-1.d0*SmallLoad)) THEN !calculate latent heat residual
        Par(10) = 0.0d0
        Par(5) = QLatReq
      END IF

      CALL SolveRegulaFalsi(ErrorToler, MaxIte, SolFla, SpeedRatio, VSHPSpeedResidual, 1.0d-10, 1.0d0, Par)
      IF (SolFla == -1) THEN
        If ( .NOT. WarmupFlag) Then
          IF (ErrCountVar .eq. 0) THEN
            ErrCountVar = ErrCountVar+1
            CALL ShowWarningError('Iteration limit exceeded calculating VS WSHP unit speed ratio, for unit='// &
                            TRIM(PTUnit(PTUnitNum)%Name))
            CALL ShowContinueErrorTimeStamp('Speed ratio returned=['//trim(RoundSigDigits(SpeedRatio,2))//'], Speed number =' &
                                        //trim(RoundSigDigits(SpeedNum,0)))
          Else
            ErrCountVar = ErrCountVar+1
            CALL ShowRecurringWarningErrorAtEnd(TRIM(PTUnit(PTUnitNum)%Name)//'":'//&
          ' Iteration limit warning exceeding calculating DX unit speed ratio continues...' &
          ,PTUnit(PTUnitNum)%ErrIndexVar, SpeedRatio, SpeedRatio)
          End If
        End If
      ELSE IF (SolFla == -2) THEN
        CALL ShowFatalError('VS WSHP unit compressor speed calculation failed: speed limits exceeded, for unit='// &
                           TRIM(PTUnit(PTUnitNum)%Name))
      END IF
    End If
  End If

  ! if the DX heating coil cannot meet the load, trim with supplemental heater
  ! occurs with constant fan mode when compressor is on or off
  ! occurs with cycling fan mode when compressor PLR is equal to 1
  IF (HeatingLoad .AND. QZnReq .GT. FullOutput .AND. PTUnit(PTUnitNum)%SuppHeatCoilIndex .GT. 0)THEN
    PartLoadFrac  = 1.0d0
    SpeedRatio  = 1.0d0

    IF(PTUnit(PTUnitNum)%NumOfSpeedHeating > 0) &
        SpeedNum = PTUnit(PTUnitNum)%NumOfSpeedHeating !maximum heating speed, avoid zero

    IF (OutDryBulbTemp .LE. PTUnit(PTUnitNum)%MaxOATSupHeat) THEN
      SupHeaterLoad = QZnReq - FullOutput
    ELSE
      SupHeaterLoad = 0.0d0
    END IF
    CALL CalcVarSpeedHeatPump(PTUnitNum, ZoneNum, FirstHVACIteration,CompOp,&
                SpeedNum,SpeedRatio,PartLoadFrac,TempOutput, LatOutput,QZnReq,  &
                        QLatReq, OnOffAirFlowRatio,SupHeaterLoad, HXUnitOn)
  END IF

! check the outlet of the supplemental heater to be lower than the maximum supplemental heater supply air temperature
  IF(PTUnit(PTUnitNum)%SuppHeatCoilIndex .GT. 0)THEN
    IF (Node(PTUnit(PTUnitNum)%AirOutNode)%Temp .GT. PTUnit(PTUnitNum)%MaxSATSupHeat .AND. SupHeaterLoad .GT. 0.0d0) THEN

       ! If supply air temperature is to high, turn off the supplemental heater to recalculate the outlet temperature
       SupHeaterLoad = 0.0d0
       Select Case (PTUnit(PTUnitNum)%SuppHeatCoilType_Num)
         Case (Coil_HeatingGas,Coil_HeatingElectric )
             CALL SimulateHeatingCoilComponents(PTUnit(PTUnitNum)%SuppHeatCoilName,FirstHVACIteration, &
                                                SupHeaterLoad, PTUnit(PTUnitNum)%SuppHeatCoilIndex)
         Case (Coil_HeatingWater)
             mdot = 0.d0
             Call SetComponentFlowRate( mdot , &
                                        PTUnit(PTUnitNum)%HotWaterControlNode, &
                                        PTUnit(PTUnitNum)%PlantCoilOutletNode, &
                                        PTUnit(PTUnitNum)%LoopNum, &
                                        PTUnit(PTUnitNum)%LoopSide, &
                                        PTUnit(PTUnitNum)%BranchNum, &
                                        PTUnit(PTUnitNum)%CompNum)
             CALL SimulateWaterCoilComponents(PTUnit(PTUnitNum)%SuppHeatCoilName,FirstHVACIteration, &
                                              PTUnit(PTUnitNum)%SuppHeatCoilIndex, SupHeaterLoad, &
                                              PTUnit(PTUnitNum)%OpMode, PartLoadFrac)
         Case (Coil_HeatingSteam)
             mdot = 0.d0
             CALL SetComponentFlowRate( mdot , &
                                        PTUnit(PTUnitNum)%HWCoilSteamInletNode, &
                                        PTUnit(PTUnitNum)%PlantCoilOutletNode, &
                                        PTUnit(PTUnitNum)%LoopNum, &
                                        PTUnit(PTUnitNum)%LoopSide, &
                                        PTUnit(PTUnitNum)%BranchNum, &
                                        PTUnit(PTUnitNum)%CompNum)
             CALL SimulateSteamCoilComponents(PTUnit(PTUnitNum)%SuppHeatCoilName, &
                                              FirstHVACIteration, SupHeaterLoad,  &
                                              PTUnit(PTUnitNum)%SuppHeatCoilIndex )
       END Select

!     If the outlet temperature is below the maximum supplemental heater supply air temperature, reduce the load passed to
!     the supplemental heater, otherwise leave the supplemental heater off. If the supplemental heater is to be turned on,
!     use the outlet conditions when the supplemental heater was off (CALL above) as the inlet conditions for the calculation
!     of supplemental heater load to just meet the maximum supply air temperature from the supplemental heater.
      IF (Node(PTUnit(PTUnitNum)%AirOutNode)%Temp .LT. PTUnit(PTUnitNum)%MaxSATSupHeat) THEN
        CpAir = PsyCpAirFnWTdb(Node(PTUnit(PTUnitNum)%AirOutNode)%HumRat,Node(PTUnit(PTUnitNum)%AirOutNode)%Temp)
        SupHeaterLoad = Node(PTUnit(PTUnitNum)%AirInNode)%MassFlowRate * CpAir * &
                      (PTUnit(PTUnitNum)%MaxSATSupHeat - Node(PTUnit(PTUnitNum)%AirOutNode)%Temp)

      ELSE
        SupHeaterLoad = 0.0d0
      END IF
    END IF
  END IF

  PTUnit(PTUnitNum)%CompPartLoadRatio = PartLoadFrac
  PTUnit(PTUnitNum)%CompSpeedRatio = SpeedRatio
  PTUnit(PTUnitNum)%CompSpeedNum = SpeedNum

  RETURN
END SUBROUTINE ControlVSHPOutput

!******************************************************************************

!******************************************************************************

REAL(r64) FUNCTION VSHPCyclingResidual(PartLoadFrac,Par)
          ! FUNCTION INFORMATION:
          !       AUTHOR         Bo Shen, based on HVACMultiSpeedHeatPump:MSHPCyclingResidual
          !       DATE WRITTEN   March, 2012
          !       MODIFIED       na
          !       RE-ENGINEERED  na

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
  REAL(r64), INTENT(IN), DIMENSION(:), OPTIONAL :: Par ! par(1) = FurnaceNum
                                                  ! par(2) = Zone Num
                                                  ! par(3) = FirstHVACIteration
                                                  ! par(4) = OpMode
                                                  ! par(5) = QZnReq, load to be met
                                                  ! par(6) = OnOffAirFlowRatio
                                                  ! par(7) = SupHeaterLoad

                                                  ! par(9) = CompOp
                                                  ! par(10) = 1.0 to meet sensible load


          ! FUNCTION PARAMETER DEFINITIONS:
          !  na

          ! INTERFACE BLOCK SPECIFICATIONS
          !  na

          ! DERIVED TYPE DEFINITIONS
          !  na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  LOGICAL :: HXUnitOn             ! flag to enable heat exchanger
  INTEGER :: PTUnitNum       ! MSHP index
  INTEGER :: ZoneNum            ! Zone index
  LOGICAL :: FirstHVACIteration ! FirstHVACIteration flag
  INTEGER :: OpMode             ! Compressor operating mode
  REAL(r64)    :: QZnReq             ! zone sensible load (W)
  REAL(r64)    :: QZnLat             ! zone latent load (W)
  REAL(r64)    :: OnOffAirFlowRatio  ! ratio of compressor ON airflow to average airflow over timestep
  REAL(r64)    :: ZoneSensLoadMet       ! delivered sensible capacity of MSHP
  REAL(r64)    :: ZoneLatLoadMet       ! delivered latent capacity of MSHP
  REAL(r64)    :: LoadToBeMet      ! sensible or latent load to be met
  REAL(r64)    :: SupHeaterLoad      ! Supplemental heater load
  REAL(r64)    :: ResScale        ! Residual scale
  INTEGER :: CompOp             ! compressor operation; 1=on, 0=off

  PTUnitNum  = INT(Par(1))
  ZoneNum = INT(Par(2))
  ! FirstHVACIteration is a logical, Par is REAL(r64), so make 1.0=TRUE and 0.0=FALSE
  IF(Par(3) .EQ. 1.0d0)THEN
    FirstHVACIteration = .TRUE.
  ELSE
    FirstHVACIteration = .FALSE.
  END IF
  OpMode = INT(Par(4))

  QZnReq = 0.0d0
  QZnLat = 0.0d0

  LoadToBeMet = Par(5)
  IF(Par(10) .EQ. 1.0d0) THEN
    QZnReq = Par(5)
  ELSE
    QZnLat = Par(5)
  END IF

  OnOffAirFlowRatio = Par(6)
  SupHeaterLoad     = Par(7)
  CompOp = INT(Par(9))

  IF(Par(11) > 0.0d0)THEN
    HXUnitOn = .TRUE.
  ELSE
   HXUnitOn = .FALSE.
  END IF

  CALL CalcVarSpeedHeatPump(PTUnitNum, ZoneNum, FirstHVACIteration,CompOp,1,0.0d0,PartLoadFrac,ZoneSensLoadMet, ZoneLatLoadMet,&
                          QZnReq, QZnLat, OnOffAirFlowRatio,SupHeaterLoad, HXUnitOn)

  ResScale = abs(LoadToBeMet)
  IF (ResScale < 100.0d0) THEN
    ResScale = 100.0d0
  END IF

  ! Calculate residual based on output calculation flag
  IF(Par(10) .EQ. 1.0d0) THEN
    VSHPCyclingResidual = (ZoneSensLoadMet - LoadToBeMet)/ResScale
  ELSE
    VSHPCyclingResidual = (ZoneLatLoadMet - LoadToBeMet)/ResScale
  END IF

  RETURN

END FUNCTION VSHPCyclingResidual


!******************************************************************************

REAL(r64) FUNCTION VSHPSpeedResidual(SpeedRatio,Par)
          ! FUNCTION INFORMATION:
          !       AUTHOR         Bo Shen, , based on HVACMultiSpeedHeatPump:MSHPVarSpeedgResidual
          !       DATE WRITTEN   March, 2012
          !       MODIFIED       na
          !       RE-ENGINEERED  na

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
                                                  ! par(10) = 1.0 to meet sensible load

          ! FUNCTION PARAMETER DEFINITIONS:
          !  na

          ! INTERFACE BLOCK SPECIFICATIONS
          !  na

          ! DERIVED TYPE DEFINITIONS
          !  na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  LOGICAL :: HXUnitOn             ! flag to enable heat exchanger
  INTEGER :: PTUnitNum      ! MSHP index
  INTEGER :: ZoneNum            ! Zone index
  LOGICAL :: FirstHVACIteration ! FirstHVACIteration flag
  INTEGER :: OpMode             ! Compressor operating mode
  REAL(r64)    :: QZnReq             ! zone load (W)
  REAL(r64)    :: QZnLat             ! zone latent load (W)
  REAL(r64)    :: OnOffAirFlowRatio  ! ratio of compressor ON airflow to average airflow over timestep
  REAL(r64)    :: ZoneSensLoadMet       ! delivered sensible capacity of MSHP
  REAL(r64)    :: ZoneLatLoadMet       ! delivered latent capacity of MSHP
  REAL(r64)    :: LoadToBeMet      ! sensible or latent load to be met
  REAL(r64)    :: SupHeaterLoad      ! Supplemental heater load
  REAL(r64)    :: ResScale        ! Residual scale
  INTEGER :: SpeedNum           ! Speed number
  INTEGER :: CompOp             ! compressor operation; 1=on, 0=off

  PTUnitNum = INT(Par(1))
  ZoneNum = INT(Par(2))
  ! FirstHVACIteration is a logical, Par is REAL(r64), so make 1.0=TRUE and 0.0=FALSE
  IF(Par(3) .EQ. 1.0d0)THEN
    FirstHVACIteration = .TRUE.
  ELSE
    FirstHVACIteration = .FALSE.
  END IF
  OpMode = INT(Par(4))

  QZnReq = 0.0d0
  QZnLat = 0.0d0

  LoadToBeMet = Par(5)
  IF(Par(10) .EQ. 1.0d0) THEN
    QZnReq = Par(5)
  ELSE
    QZnLat = Par(5)
  END IF

  OnOffAirFlowRatio = Par(6)
  SupHeaterLoad     = Par(7)
  SpeedNum     = INT(Par(8))
  CompOp = INT(Par(9))

  IF(Par(11) > 0.0d0)THEN
    HXUnitOn = .TRUE.
  ELSE
   HXUnitOn = .FALSE.
  END IF


  Call CalcVarSpeedHeatPump(PTUnitNum, ZoneNum, FirstHVACIteration,CompOp,SpeedNum,  &
                          SpeedRatio,1.0d0,ZoneSensLoadMet, ZoneLatLoadMet, &
                          QZnReq, QZnLat, OnOffAirFlowRatio,SupHeaterLoad, HXUnitOn)

  ResScale = abs(LoadToBeMet)
  IF (ResScale < 100.0d0) THEN
    ResScale = 100.0d0
  END IF

    ! Calculate residual based on output calculation flag
  IF(Par(10) .EQ. 1.0d0) THEN
    VSHPSpeedResidual = (ZoneSensLoadMet - LoadToBeMet)/ResScale
  ELSE
    VSHPSpeedResidual = (ZoneLatLoadMet - LoadToBeMet)/ResScale
  END IF

  RETURN

END FUNCTION VSHPSpeedResidual
!******************************************************************************

SUBROUTINE CalcVarSpeedHeatPump(PTUnitNum,ZoneNum, FirstHVACIteration,CompOp,SpeedNum,SpeedRatio,PartLoadFrac,LoadMet,  &
                          LatentLoadMet,QZnReq, QLatReq, OnOffAirFlowRatio,SupHeaterLoad, HXUnitOn)
            ! SUBROUTINE INFORMATION:
            !       AUTHOR:          Bo Shen, based on HVACMultiSpeedHeatPump:CalcMSHeatPump
            !       DATE WRITTEN:    March 2012
            !       MODIFIED         July 2012, Chandan Sharma - FSEC: Added zone sys avail managers
            !       RE-ENGINEERED    na

            ! PURPOSE OF THIS SUBROUTINE:
            !  This routine will calcultes MSHP performance based on given system load

            ! METHODOLOGY EMPLOYED:
            ! na

            ! REFERENCES: na

            ! USE STATEMENTS:
  USE Fans,                      ONLY: SimulateFanComponents
  USE DXCoils,                   ONLY: SimDXCoil
  USE MixedAir,                  ONLY: SimOAMixer
  USE HeatingCoils,              ONLY: SimulateHeatingCoilComponents
  USE SteamCoils,                ONLY: SimulateSteamCoilComponents
  USE WaterCoils,                ONLY: SimulateWaterCoilComponents
  USE InputProcessor,            ONLY: SameString
  USE HVACHXAssistedCoolingCoil, ONLY: SimHXAssistedCoolingCoil
  USE Psychrometrics,            ONLY: PsyHFnTdbW, PsyCpAirFnWTdb
  USE DataEnvironment,           ONLY: OutDryBulbTemp
  USE WatertoAirheatPumpSimple,  ONLY: SimWatertoAirHPSimple
  USE PlantUtilities,            ONLY: SetComponentFlowRate
  USE General,                   ONLY: SolveRegulaFalsi, RoundSigDigits
  USE VariableSpeedCoils,        ONLY: SimVariableSpeedCoils, VarSpeedCoil

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER,   INTENT    (IN) :: PTUnitNum              ! Unit index in fan coil array
  INTEGER, INTENT (IN)    :: ZoneNum                 ! Zone index
  LOGICAL,   INTENT    (IN) :: FirstHVACIteration   ! flag for 1st HVAC iteration in the time step
  REAL(r64), INTENT    (IN) :: PartLoadFrac         ! compressor part load fraction
  REAL(r64), INTENT   (OUT) :: LoadMet              ! load met by unit (W)
  REAL(r64), INTENT    (IN) :: QZnReq               ! Zone load (W) unused1208
  REAL(r64), INTENT (INOUT) :: OnOffAirFlowRatio    ! ratio of compressor ON airflow to AVERAGE airflow over timestep
  REAL(r64), INTENT (INOUT) :: SupHeaterLoad        ! supplemental heater load (W)
  LOGICAL,   INTENT    (IN) :: HXUnitOn             ! flag to enable heat exchanger
  INTEGER, INTENT (IN)    :: SpeedNum             ! Speed number
  REAL(r64)   , INTENT (IN)    :: SpeedRatio           ! Compressor speed ratio
  INTEGER, INTENT(IN)     :: CompOp               ! Compressor on/off; 1=on, 0=off
  REAL(r64), Intent(OUT)   :: LatentLoadMet ! Latent cooling load met (furnace outlet with respect to control zone humidity ratio)
  REAL(r64)   , INTENT (IN)    :: QLatReq              ! Zone latent load []

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER :: Blank = ' '        ! subroutine argument when coil index is known
  CHARACTER(len=*), PARAMETER :: RoutineName='CalcVarSpeedHeatPump: '  ! for error messages
  REAL(r64), PARAMETER :: ErrTolerance = 0.001d0    ! convergence limit for hotwater coil
  INTEGER, PARAMETER :: SolveMaxIter=50

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na


          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER  :: OutletNode         ! PTHP air outlet node
  INTEGER  :: InletNode          ! PTHP air inlet node
  REAL(r64):: AirMassFlow        ! total supply air mass flow through the PTHP [m3/s]
  REAL(r64):: MinHumRat          ! minimum humidity ratio for sensible capacity calculation (kg/kg)
  REAL(r64):: OutsideDryBulbTemp ! Outdoor air temperature at external node height
  REAL(r64):: QCoilReq           ! load passed to heating coil (W)
  REAL(r64):: QActual            ! actual heating coil output (W)
  INTEGER  :: OpMode             ! fan operating mode, CycFanCycCoil or ContFanCycCoil
  LOGICAL  :: errflag            ! subroutine error flag
  REAL(r64) :: mdot              !local temporary for mass flow rate
  REAL(r64) :: MaxHotWaterFlow   ! coil maximum hot water mass flow rate, kg/s
  REAL(r64) :: HotWaterMdot      ! actual hot water mass flow rate
  REAL(r64), DIMENSION(3) :: Par
  INTEGER   :: SolFlag
          ! FLOW

  OutletNode = PTUnit(PTUnitNum)%AirOutNode
  InletNode = PTUnit(PTUnitNum)%AirInNode
  OpMode = PTUnit(PTUnitNum)%OpMode
!  IF(PTUnit(PTUnitNum)%CondenserNodeNum .EQ. 0)THEN
!    OutsideDryBulbTemp = OutDryBulbTemp
!  ELSE
!    OutsideDryBulbTemp = Node(PTUnit(PTUnitNum)%CondenserNodeNum)%Temp
!  END IF

  OutsideDryBulbTemp = OutDryBulbTemp

  SaveCompressorPLR = 0.0d0
  ! Set inlet air mass flow rate based on PLR and compressor on/off air flow rates
  CALL SetVSHPAirFlow(PTUnitNum, ZoneNum, PartLoadFrac,OnOffAirFlowRatio,SpeedNum,SpeedRatio)

  AirMassFlow = Node(InletNode)%MassFlowRate
  IF(PTUnit(PTUnitNum)%OutsideAirNode .GT. 0)&
  CALL SimOAMixer(PTUnit(PTUnitNum)%OAMixName,FirstHVACIteration,PTUnit(PTUnitNum)%OAMixIndex)

  ! if blow through, simulate fan then coils
  IF (PTUnit(PTUnitNum)%FanPlace .EQ. BlowThru) THEN
    CALL SimulateFanComponents(PTUnit(PTUnitNum)%FanName,FirstHVACIteration,PTUnit(PTUnitNum)%FanIndex,FanSpeedRatio,&
                                 ZoneCompTurnFansOn,ZoneCompTurnFansOff)
  END IF

  IF (CoolingLoad .AND. OutsideDryBulbTemp .GT. PTUnit(PTUnitNum)%MinOATCompressor )THEN
        Call SimVariableSpeedCoils(Blank,PTUnit(PTUnitNum)%DXCoolCoilIndexNum ,&
           PTUnit(PTUnitNum)%OpMode,PTUnit(PTUnitNum)%MaxONOFFCyclesperHour, &
           PTUnit(PTUnitNum)%HPTimeConstant,PTUnit(PTUnitNum)%FanDelayTime,&
           CompOp, PartLoadFrac, OnOffAirFlowRatio,SpeedNum, SpeedRatio,QZnReq, QLatReq )

        SaveCompressorPLR = PartLoadFrac
  ELSE ! cooling coil is off
        Call SimVariableSpeedCoils(Blank,PTUnit(PTUnitNum)%DXCoolCoilIndexNum ,&
           PTUnit(PTUnitNum)%OpMode,PTUnit(PTUnitNum)%MaxONOFFCyclesperHour, &
           PTUnit(PTUnitNum)%HPTimeConstant,PTUnit(PTUnitNum)%FanDelayTime,&
           CompOp, 0.0d0, OnOffAirFlowRatio,1, 0.0d0,0.0d0, 0.0d0 )
  END IF

  IF(PTUnit(PTUnitNum)%UnitType_Num /= PTACUnit) THEN ! PTHP
      IF (HeatingLoad)THEN
           Call SimVariableSpeedCoils(Blank,PTUnit(PTUnitNum)%DXHeatCoilIndex ,&
               PTUnit(PTUnitNum)%OpMode,PTUnit(PTUnitNum)%MaxONOFFCyclesperHour, &
               PTUnit(PTUnitNum)%HPTimeConstant,PTUnit(PTUnitNum)%FanDelayTime,&
               CompOp, PartLoadFrac, OnOffAirFlowRatio,SpeedNum, SpeedRatio,QZnReq, QLatReq )

           SaveCompressorPLR = PartLoadFrac
      ELSE
    !   heating coil is off
          Call SimVariableSpeedCoils(Blank,PTUnit(PTUnitNum)%DXHeatCoilIndex,&
               PTUnit(PTUnitNum)%OpMode,PTUnit(PTUnitNum)%MaxONOFFCyclesperHour, &
               PTUnit(PTUnitNum)%HPTimeConstant,PTUnit(PTUnitNum)%FanDelayTime,&
               CompOp, 0.0d0, OnOffAirFlowRatio,1, 0.0d0,0.0d0, 0.0d0 )
      END IF
  ELSE !PTAC
     IF (HeatingLoad)THEN
        IF(PTUnit(PTUnitNum)%UnitType_Num .EQ. PTACUnit)THEN
          QCoilReq = PTUnit(PTUnitNum)%ACHeatCoilCap * PartLoadFrac
          IF(PTUnit(PTUnitNum)%ACHeatCoilType_Num == Coil_HeatingGas .OR.   &
             PTUnit(PTUnitNum)%ACHeatCoilType_Num == Coil_HeatingElectric)THEN
            CALL SimulateHeatingCoilComponents(PTUnit(PTUnitNum)%ACHeatCoilName,FirstHVACIteration,QCoilReq,  &
                                         PTUnit(PTUnitNum)%ACHeatCoilIndex, QActual, .FALSE., OpMode, PartLoadFrac)
          ELSE IF(PTUnit(PTUnitNum)%ACHeatCoilType_Num == Coil_HeatingWater)THEN
    !       set water inlet node mass flow rate proportional to PLR. Limit water flow rate based on "available" upper limit.
            mdot = PTUnit(PTUnitNum)%MaxHeatCoilFluidFlow * PartLoadFrac

            CALL SetComponentFlowRate( mdot , &
                                       PTUnit(PTUnitNum)%HotWaterControlNode, &
                                       PTUnit(PTUnitNum)%PlantCoilOutletNode, &
                                       PTUnit(PTUnitNum)%LoopNum, &
                                       PTUnit(PTUnitNum)%LoopSide, &
                                       PTUnit(PTUnitNum)%BranchNum, &
                                       PTUnit(PTUnitNum)%CompNum)

            CALL SimulateWaterCoilComponents(PTUnit(PTUnitNum)%ACHeatCoilName,FirstHVACIteration, &
                                           PTUnit(PTUnitNum)%ACHeatCoilIndex, QActual, PTUnit(PTUnitNum)%OpMode, PartLoadFrac)
          ELSE IF(PTUnit(PTUnitNum)%ACHeatCoilType_Num == Coil_HeatingSteam)THEN
    !       set steam inlet node mass flow rate proportional to PLR. Limit steam flow rate based on "available" upper limit.
            mdot = PTUnit(PTUnitNum)%MaxHeatCoilFluidFlow *  PartLoadFrac
            CALL SetComponentFlowRate( mdot , &
                                       PTUnit(PTUnitNum)%HWCoilSteamInletNode, &
                                       PTUnit(PTUnitNum)%PlantCoilOutletNode, &
                                       PTUnit(PTUnitNum)%LoopNum, &
                                       PTUnit(PTUnitNum)%LoopSide, &
                                       PTUnit(PTUnitNum)%BranchNum, &
                                       PTUnit(PTUnitNum)%CompNum)

            CALL SimulateSteamCoilComponents(PTUnit(PTUnitNum)%ACHeatCoilName, FirstHVACIteration, QCoilReq,  &
                                             PTUnit(PTUnitNum)%ACHeatCoilIndex, QActual, PTUnit(PTUnitNum)%OpMode, PartLoadFrac)
          END IF
        END IF
     ELSE
!   heating coil is off
       IF(PTUnit(PTUnitNum)%UnitType_Num .EQ. PTACUnit)THEN
          QCoilReq = 0.0d0
          IF(PTUnit(PTUnitNum)%ACHeatCoilType_Num == Coil_HeatingGas .OR.   &
             PTUnit(PTUnitNum)%ACHeatCoilType_Num == Coil_HeatingElectric)THEN
            CALL SimulateHeatingCoilComponents(PTUnit(PTUnitNum)%ACHeatCoilName,FirstHVACIteration,QCoilReq,  &
                                         PTUnit(PTUnitNum)%ACHeatCoilIndex)
          ELSE IF(PTUnit(PTUnitNum)%ACHeatCoilType_Num == Coil_HeatingWater)THEN
            mdot = 0.d0
            Call SetComponentFlowRate( mdot , &
                                       PTUnit(PTUnitNum)%HotWaterControlNode, &
                                       PTUnit(PTUnitNum)%PlantCoilOutletNode, &
                                       PTUnit(PTUnitNum)%LoopNum, &
                                       PTUnit(PTUnitNum)%LoopSide, &
                                       PTUnit(PTUnitNum)%BranchNum, &
                                       PTUnit(PTUnitNum)%CompNum)

            CALL SimulateWaterCoilComponents(PTUnit(PTUnitNum)%ACHeatCoilName,FirstHVACIteration, &
                                           PTUnit(PTUnitNum)%ACHeatCoilIndex)
          ELSE IF(PTUnit(PTUnitNum)%ACHeatCoilType_Num == Coil_HeatingSteam)THEN
            mdot = 0.d0
            Call SetComponentFlowRate( mdot , &
                                       PTUnit(PTUnitNum)%HWCoilSteamInletNode, &
                                       PTUnit(PTUnitNum)%PlantCoilOutletNode, &
                                       PTUnit(PTUnitNum)%LoopNum, &
                                       PTUnit(PTUnitNum)%LoopSide, &
                                       PTUnit(PTUnitNum)%BranchNum, &
                                       PTUnit(PTUnitNum)%CompNum)

            CALL SimulateSteamCoilComponents(PTUnit(PTUnitNum)%ACHeatCoilName, &
                                             FirstHVACIteration,    &
                                             QCoilReq,                        &
                                             PTUnit(PTUnitNum)%ACHeatCoilIndex, &
                                             QActual, PTUnit(PTUnitNum)%OpMode, PartLoadFrac)
          END IF
       END IF
    END IF
 END IF

  ! if draw through, simulate coils then fan
  IF (PTUnit(PTUnitNum)%FanPlace .EQ. DrawThru) THEN
    CALL SimulateFanComponents(PTUnit(PTUnitNum)%FanName,FirstHVACIteration,PTUnit(PTUnitNum)%FanIndex,FanSpeedRatio,&
                                 ZoneCompTurnFansOn,ZoneCompTurnFansOff)
  END IF

  IF(PTUnit(PTUnitNum)%SuppHeatCoilIndex .GT. 0)THEN
     IF ( SupHeaterLoad < SmallLoad ) THEN
         Select Case (PTUnit(PTUnitNum)%SuppHeatCoilType_Num)
           Case (Coil_HeatingGas,Coil_HeatingElectric )
                 CALL SimulateHeatingCoilComponents(PTUnit(PTUnitNum)%SuppHeatCoilName,FirstHVACIteration, &
                                                    SupHeaterLoad, PTUnit(PTUnitNum)%SuppHeatCoilIndex,  &
                                                    QActual, .TRUE., PTUnit(PTUnitNum)%OpMode)
           Case (Coil_HeatingWater)
               mdot = 0.0d0
               Call SetComponentFlowRate( mdot , &
                                          PTUnit(PTUnitNum)%HotWaterControlNode, &
                                          PTUnit(PTUnitNum)%PlantCoilOutletNode, &
                                          PTUnit(PTUnitNum)%LoopNum, &
                                          PTUnit(PTUnitNum)%LoopSide, &
                                          PTUnit(PTUnitNum)%BranchNum, &
                                          PTUnit(PTUnitNum)%CompNum)
               CALL SimulateWaterCoilComponents(PTUnit(PTUnitNum)%SuppHeatCoilName,FirstHVACIteration, &
                                          PTUnit(PTUnitNum)%SuppHeatCoilIndex, SupHeaterLoad, &
                                          PTUnit(PTUnitNum)%OpMode)
           Case (Coil_HeatingSteam)
               mdot = 0.0d0
               Call SetComponentFlowRate( mdot , &
                                          PTUnit(PTUnitNum)%HWCoilSteamInletNode, &
                                          PTUnit(PTUnitNum)%PlantCoilOutletNode, &
                                          PTUnit(PTUnitNum)%LoopNum, &
                                          PTUnit(PTUnitNum)%LoopSide, &
                                          PTUnit(PTUnitNum)%BranchNum, &
                                          PTUnit(PTUnitNum)%CompNum)
               CALL SimulateSteamCoilComponents(PTUnit(PTUnitNum)%SuppHeatCoilName, &
                                          FirstHVACIteration, SupHeaterLoad, &
                                          PTUnit(PTUnitNum)%SuppHeatCoilIndex, &
                                          QActual, PTUnit(PTUnitNum)%OpMode)
         END Select
     ELSE
        Select Case (PTUnit(PTUnitNum)%SuppHeatCoilType_Num)
           Case (Coil_HeatingGas,Coil_HeatingElectric )
              CALL SimulateHeatingCoilComponents(PTUnit(PTUnitNum)%SuppHeatCoilName,FirstHVACIteration, &
                                                 SupHeaterLoad, PTUnit(PTUnitNum)%SuppHeatCoilIndex, &
                                                 QActual, .TRUE., PTUnit(PTUnitNum)%OpMode)
           Case (Coil_HeatingWater)
               MaxHotWaterFlow = PTUnit(PTUnitNum)%MaxSuppCoilFluidFlow
               Call SetComponentFlowRate( MaxHotWaterFlow , &
                                          PTUnit(PTUnitNum)%HotWaterControlNode, &
                                          PTUnit(PTUnitNum)%PlantCoilOutletNode, &
                                          PTUnit(PTUnitNum)%LoopNum, &
                                          PTUnit(PTUnitNum)%LoopSide, &
                                          PTUnit(PTUnitNum)%BranchNum, &
                                          PTUnit(PTUnitNum)%CompNum)
              QActual = SupHeaterLoad
              ! simulate the hot water supplemental heating coil
              CALL SimulateWaterCoilComponents(PTUnit(PTUnitNum)%SuppHeatCoilName,FirstHVACIteration, &
                                               PTUnit(PTUnitNum)%SuppHeatCoilIndex, QActual, &
                                               PTUnit(PTUnitNum)%OpMode)
              IF( QActual > (SupHeaterLoad + SmallLoad)) THEN
                  ! control water flow to obtain output matching SupHeaterLoad
                  SolFlag = 0
                  MinWaterFlow = 0.0d0
                  Par(1) = REAL(PTUnitNum,r64)
                  IF (FirstHVACIteration) THEN
                    Par(2) = 1.d0
                  ELSE
                    Par(2) = 0.0d0
                  END IF
                  Par(3) = SupHeaterLoad
                  MaxHotWaterFlow = PTUnit(PTUnitNum)%MaxSuppCoilFluidFlow
                  CALL SolveRegulaFalsi(ErrTolerance, SolveMaxIter, SolFlag, HotWaterMdot, HotWaterCoilResidual, &
                                        MinWaterFlow, MaxHotWaterFlow, Par)
                  IF (SolFlag == -1) THEN
                    IF (PTUnit(PTUnitNum)%HotWaterCoilMaxIterIndex == 0) THEN
                      CALL ShowWarningMessage('RoutineName//Hot water coil control failed for '//  &
                         trim(PTUnit(PTUnitNum)%UnitType)//'="'//  &
                         TRIM(PTUnit(PTUnitNum)%Name)//'"')
                      CALL ShowContinueErrorTimeStamp(' ')
                      CALL ShowContinueError('  Iteration limit ['//trim(RoundSigDigits(SolveMaxIter))//  &
                          '] exceeded in calculating hot water mass flow rate')
                    ENDIF
                    CALL ShowRecurringWarningErrorAtEnd('RoutineName//Hot water coil control failed (iteration limit ['//  &
                        trim(RoundSigDigits(SolveMaxIter))//']) for '//trim(PTUnit(PTUnitNum)%UnitType)//'="'// &
                        TRIM(PTUnit(PTUnitNum)%Name),PTUnit(PTUnitNum)%HotWaterCoilMaxIterIndex)
                  ELSE IF (SolFlag == -2) THEN
                    IF (PTUnit(PTUnitNum)%HotWaterCoilMaxIterIndex2 == 0) THEN
                      CALL ShowWarningMessage('RoutineName//Hot water coil control failed (maximum flow limits) for '//  &
                          trim(PTUnit(PTUnitNum)%UnitType)//'="'// &
                          TRIM(PTUnit(PTUnitNum)%Name)//'"')
                      CALL ShowContinueErrorTimeStamp(' ')
                      CALL ShowContinueError('...Bad hot water maximum flow rate limits')
                      CALL ShowContinueError('...Given minimum water flow rate='//trim(RoundSigDigits(MinWaterFlow,3))//' kg/s')
                      CALL ShowContinueError('...Given maximum water flow rate='//trim(RoundSigDigits(MaxHotWaterFlow,3))//' kg/s')
                    ENDIF
                    CALL ShowRecurringWarningErrorAtEnd('RoutineName//Hot water coil control failed (flow limits) for '//&
                       trim(PTUnit(PTUnitNum)%UnitType)//'="'// &
                       TRIM(PTUnit(PTUnitNum)%Name)//'"', &
                       PTUnit(PTUnitNum)%HotWaterCoilMaxIterIndex2,  &
                       ReportMinOf=MinWaterFlow,ReportMaxOf=MaxHotWaterFlow,ReportMinUnits='[kg/s]',ReportMaxUnits='[kg/s]')
                  END IF
                  QActual = SupHeaterLoad
                  ! simulate the hot water supplemental heating coil
                  CALL SimulateWaterCoilComponents(PTUnit(PTUnitNum)%SuppHeatCoilName,FirstHVACIteration, &
                                                   PTUnit(PTUnitNum)%SuppHeatCoilIndex, QActual, &
                                                   PTUnit(PTUnitNum)%OpMode)
              ENDIF
           Case (Coil_HeatingSteam)
               mdot = PTUnit(PTUnitNum)%MaxSuppCoilFluidFlow
               Call SetComponentFlowRate( mdot , &
                                          PTUnit(PTUnitNum)%HWCoilSteamInletNode, &
                                          PTUnit(PTUnitNum)%PlantCoilOutletNode, &
                                          PTUnit(PTUnitNum)%LoopNum, &
                                          PTUnit(PTUnitNum)%LoopSide, &
                                          PTUnit(PTUnitNum)%BranchNum, &
                                          PTUnit(PTUnitNum)%CompNum)

               ! simulate the steam supplemental heating coil
               CALL SimulateSteamCoilComponents(PTUnit(PTUnitNum)%SuppHeatCoilName, &
                                          FirstHVACIteration, SupHeaterLoad, &
                                          PTUnit(PTUnitNum)%SuppHeatCoilIndex, &
                                          QActual, PTUnit(PTUnitNum)%OpMode)
        END Select
     ENDIF
  ENDIF
! calculate sensible load met using delta enthalpy at a constant (minimum) humidity ratio)
  MinHumRat = MIN(Node(InletNode)%HumRat,Node(OutletNode)%HumRat)
  LoadMet   = AirMassFlow * (PsyHFnTdbW(Node(OutletNode)%Temp,MinHumRat) - PsyHFnTdbW(Node(InletNode)%Temp,MinHumRat))
  LatentLoadMet = 0.0d0
RETURN
END SUBROUTINE CalcVarSpeedHeatPump


SUBROUTINE SetVSHPAirFlow(PTUnitNum,ZoneNum, PartLoadRatio,OnOffAirFlowRatio,SpeedNum,SpeedRatio)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Bo Shen, based on HVACMultiSpeedHeatPump:SetAverageAirFlow
          !       DATE WRITTEN   March, 2012
          !       MODIFIED       na
          !       RE-ENGINEERED  na
          ! PURPOSE OF THIS SUBROUTINE:
          ! Set the average air mass flow rates using the part load fraction of the heat pump for this time step
          ! Set OnOffAirFlowRatio to be used by DX coils

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataZoneEnergyDemands,      ONLY: CurDeadBandOrSetback

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN)    :: PTUnitNum      ! Unit index
  INTEGER, INTENT (IN)    :: ZoneNum        ! Zone index
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
  INTEGER             :: InletNode           ! inlet node number for PTUnitNum
  INTEGER             :: OutsideAirNode      ! outside air node number in PTHP loop
  INTEGER             :: AirRelNode          ! relief air node number in PTHP loop
  REAL(r64)           :: AverageUnitMassFlow = 0.0d0 ! average supply air mass flow rate over time step
  REAL(r64)           :: AverageOAMassFlow = 0.0d0  ! average outdoor air mass flow rate over time step

  MSHPMassFlowRateLow = 0.0d0             ! Mass flow rate at low speed
  MSHPMassFlowRateHigh = 0.0d0            ! Mass flow rate at high speed

  InletNode      = PTUnit(PTUnitNum)%AirInNode
  OutsideAirNode = PTUnit(PTUnitNum)%OutsideAirNode
  AirRelNode     = PTUnit(PTUnitNum)%AirReliefNode

  AverageOAMassFlow   = (PartLoadRatio * OACompOnMassFlow) + ((1-PartLoadRatio) * OACompOffMassFlow)

  IF (PTUnit(PTUnitNum)%OpMode .EQ. ContFanCycCoil) THEN
   CompOffMassFlow = PTUnit(PTUnitNum)%IdleMassFlowRate
   CompOffFlowRatio = PTUnit(PTUnitNum)%IdleSpeedRatio
  ELSE
   CompOffMassFlow = 0.0d0
   CompOffFlowRatio = 0.0d0
  END IF

  IF (HeatingLoad .AND. (PTUnit(PTUnitNum)%UnitType_Num .EQ. PTACUnit))THEN
      CompOnMassFlow = PTUnit(PTUnitNum)%CoolMassFlowRate(PTUnit(PTUnitNum)%NumOfSpeedCooling)
      CompOnFlowRatio = PTUnit(PTUnitNum)%MSCoolingSpeedRatio(PTUnit(PTUnitNum)%NumOfSpeedCooling)
      MSHPMassFlowRateLow = PTUnit(PTUnitNum)%CoolMassFlowRate(PTUnit(PTUnitNum)%NumOfSpeedCooling)
      MSHPMassFlowRateHigh = PTUnit(PTUnitNum)%CoolMassFlowRate(PTUnit(PTUnitNum)%NumOfSpeedCooling)
      AverageUnitMassFlow = (PartLoadRatio * CompOnMassFlow) + ((1-PartLoadRatio) * CompOffMassFlow)
      IF(CompOffFlowRatio .GT. 0.0d0)THEN
        FanSpeedRatio = (PartLoadRatio * CompOnFlowRatio) + ((1-PartLoadRatio) * CompOffFlowRatio)
      ELSE
        FanSpeedRatio     = CompOnFlowRatio
      END IF
  ELSE
     If (Present(SpeedNum)) Then
          If (HeatingLoad) Then
            If (SpeedNum .eq. 1) Then
              CompOnMassFlow = PTUnit(PTUnitNum)%HeatMassFlowRate(SpeedNum)
              CompOnFlowRatio = PTUnit(PTUnitNum)%MSHeatingSpeedRatio(SpeedNum)
              MSHPMassFlowRateLow = PTUnit(PTUnitNum)%HeatMassFlowRate(1)
              MSHPMassFlowRateHigh = PTUnit(PTUnitNum)%HeatMassFlowRate(1)
            Else If (SpeedNum .GT. 1) Then
              CompOnMassFlow = SpeedRatio*PTUnit(PTUnitNum)%HeatMassFlowRate(SpeedNum) + &
                               (1.0-SpeedRatio)*PTUnit(PTUnitNum)%HeatMassFlowRate(SpeedNum-1)
              CompOnFlowRatio = SpeedRatio*PTUnit(PTUnitNum)%MSHeatingSpeedRatio(SpeedNum) + &
                               (1.0-SpeedRatio)*PTUnit(PTUnitNum)%MSHeatingSpeedRatio(SpeedNum-1)
              MSHPMassFlowRateLow = PTUnit(PTUnitNum)%HeatMassFlowRate(SpeedNum-1)
              MSHPMassFlowRateHigh = PTUnit(PTUnitNum)%HeatMassFlowRate(SpeedNum)
            End If
          Else If (PTUnit(PTUnitNum)%HeatCoolMode == CoolingMode) Then
            If (SpeedNum .eq. 1) Then
              CompOnMassFlow = PTUnit(PTUnitNum)%CoolMassFlowRate(SpeedNum)
              CompOnFlowRatio = PTUnit(PTUnitNum)%MSCoolingSpeedRatio(SpeedNum)
              MSHPMassFlowRateLow = PTUnit(PTUnitNum)%CoolMassFlowRate(1)
              MSHPMassFlowRateHigh = PTUnit(PTUnitNum)%CoolMassFlowRate(1)
            Else If (SpeedNum .GT. 1) Then
              CompOnMassFlow = SpeedRatio*PTUnit(PTUnitNum)%CoolMassFlowRate(SpeedNum) + &
                               (1.0-SpeedRatio)*PTUnit(PTUnitNum)%CoolMassFlowRate(SpeedNum-1)
              CompOnFlowRatio = SpeedRatio*PTUnit(PTUnitNum)%MSCoolingSpeedRatio(SpeedNum) + &
                               (1.0-SpeedRatio)*PTUnit(PTUnitNum)%MSCoolingSpeedRatio(SpeedNum-1)
              MSHPMassFlowRateLow = PTUnit(PTUnitNum)%CoolMassFlowRate(SpeedNum-1)
              MSHPMassFlowRateHigh = PTUnit(PTUnitNum)%CoolMassFlowRate(SpeedNum)
            End If
          End If
      END IF

      ! Set up fan flow rate during compressor off time
      If (PTUnit(PTUnitNum)%OpMode .EQ. ContFanCycCoil .AND. Present(SpeedNum)) Then
        IF (PTUnit(PTUnitNum)%AirFlowControl .EQ. UseCompressorOnFlow .AND. CompOnMassFlow > 0.0d0) THEN
          IF(SpeedNum == 1) THEN  !LOWEST SPEED USE IDLE FLOW
            CompOffMassFlow = PTUnit(PTUnitNum)%IdleMassFlowRate
            CompOffFlowRatio = PTUnit(PTUnitNum)%IdleSpeedRatio
          Else IF (PTUnit(PTUnitNum)%LastMode .EQ. HeatingMode) THEN
            CompOffMassFlow = PTUnit(PTUnitNum)%HeatMassFlowRate(SpeedNum)
            CompOffFlowRatio = PTUnit(PTUnitNum)%MSHeatingSpeedRatio(SpeedNum)
          ELSE
            CompOffMassFlow = PTUnit(PTUnitNum)%CoolMassFlowRate(SpeedNum)
            CompOffFlowRatio = PTUnit(PTUnitNum)%MSCoolingSpeedRatio(SpeedNum)
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
  END IF

  IF ( GetCurrentScheduleValue(PTUnit(PTUnitNum)%SchedPtr) .GT. 0.0d0 &
     .AND. ((GetCurrentScheduleValue(PTUnit(PTUnitNum)%FanAvailSchedPtr) .GT. 0.0d0 .OR. &
      ZoneCompTurnFansOn) .AND. .NOT. ZoneCompTurnFansOff))THEN

    Node(InletNode)%MassFlowRate              = AverageUnitMassFlow
    Node(InletNode)%MassFlowRateMaxAvail      = AverageUnitMassFlow
    IF(OutsideAirNode .GT. 0)THEN
      Node(OutsideAirNode)%MassFlowRate         = AverageOAMassFlow
      Node(OutsideAirNode)%MassFlowRateMaxAvail = AverageOAMassFlow
      Node(AirRelNode)%MassFlowRate             = AverageOAMassFlow
      Node(AirRelNode)%MassFlowRateMaxAvail     = AverageOAMassFlow
    END IF
    IF (AverageUnitMassFlow .GT. 0.0d0) THEN
      OnOffAirFlowRatio                       = CompOnMassFlow / AverageUnitMassFlow
    ELSE
      OnOffAirFlowRatio                       = 0.0d0
    END IF

  ELSE

    Node(InletNode)%MassFlowRate              = 0.0d0
    IF(OutsideAirNode .GT. 0)THEN
      Node(OutsideAirNode)%MassFlowRate       = 0.0d0
      Node(AirRelNode)%MassFlowRate           = 0.0d0
    END IF
    OnOffAirFlowRatio                         = 0.0d0

  END IF
  RETURN
END SUBROUTINE SetVSHPAirFlow

SUBROUTINE SetOnOffMassFlowRateVSCoil(PTUnitNum, ZoneNum, FirstHVACIteration, &
        AirLoopNum, OnOffAirFlowRatio, OpMode, QZnReq, MoistureLoad, PartLoadRatio)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Bo Shen
          !       DATE WRITTEN   March 2012
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is for initializations of the Furnace Components.

          ! METHODOLOGY EMPLOYED:
          ! The HeatCool furnace/unitarysystem and air-to-air heat pump may have alternate air flow rates
          ! in cooling, heating, and when no cooling or heating is needed. Set up the coil (comp) ON and OFF
          ! air flow rates. Use these flow rates during the Calc routines to set the average mass flow rates
          ! based on PLR.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE General,     ONLY: RoundSigDigits
  USE DataZoneEnergyDemands, ONLY: CurDeadbandOrSetback

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER,   INTENT(IN)    :: PTUnitNum        ! index to furnace
  INTEGER,   INTENT(IN)    :: ZoneNum           ! index to zone
  INTEGER,   INTENT(IN)    :: AirLoopNum        ! index to air loop !unused1208
  REAL(r64), INTENT(INOUT) :: OnOffAirFlowRatio ! ratio of coil on to coil off air flow rate
  INTEGER,   INTENT(IN)    :: OpMode            ! fan operating mode
  REAL(r64), INTENT(IN)    :: QZnReq          ! sensible load to be met (W) !unused1208
  REAL(r64), INTENT(IN)    :: MoistureLoad      ! moisture load to be met (W)
  REAL(r64), INTENT(INOUT)    :: PartLoadRatio     ! coil part-load ratio
  LOGICAL, INTENT (IN)    :: FirstHVACIteration   ! Flag for 1st HVAC iteration

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER             :: InNode                           ! Inlet node number in MSHP loop
  INTEGER             :: OutNode                          ! Outlet node number in MSHP loop

  InNode  = PTUnit(PTUnitNum)%AirInNode
  OutNode = PTUnit(PTUnitNum)%AirOutNode

  CALL SetOnOffMassFlowRate(PTUnitNum, PartLoadRatio, OnOffAirFlowRatio)
  !INTIALIZE FIXED SPEED FIRST, AND OVER-WRITE USING MUL-SPEED

          ! FLOW:

  If (CoolingLoad) then
    PTUnit(PTUnitNum)%HeatCoolMode = CoolingMode
  ELSE If (HeatingLoad) then
    PTUnit(PTUnitNum)%HeatCoolMode = HeatingMode
  Else
    PTUnit(PTUnitNum)%HeatCoolMode = 0
  End If

  ! Set the inlet node mass flow rate
  IF (PTUnit(PTUnitNum)%OpMode .EQ. ContFanCycCoil) THEN
  ! constant fan mode
    IF ((PTUnit(PTUnitNum)%HeatCoolMode == HeatingMode) .AND. .NOT. CurDeadbandOrSetback(ZoneNum)) THEN
      CompOnMassFlow = PTUnit(PTUnitNum)%HeatMassFlowRate(1)
      CompOnFlowRatio = PTUnit(PTUnitNum)%MSHeatingSpeedRatio(1)
      PTUnit(PTUnitNum)%LastMode = HeatingMode
    ELSE IF ((PTUnit(PTUnitNum)%HeatCoolMode == CoolingMode) .AND. .NOT. CurDeadbandOrSetback(ZoneNum)) THEN
      CompOnMassFlow = PTUnit(PTUnitNum)%CoolMassFlowRate(1)
      CompOnFlowRatio = PTUnit(PTUnitNum)%MSCoolingSpeedRatio(1)
      PTUnit(PTUnitNum)%LastMode = CoolingMode
    ELSE
      CompOnMassFlow = PTUnit(PTUnitNum)%IdleMassFlowRate
      CompOnFlowRatio = PTUnit(PTUnitNum)%IdleSpeedRatio
    END IF
    CompOffMassFlow = PTUnit(PTUnitNum)%IdleMassFlowRate
    CompOffFlowRatio = PTUnit(PTUnitNum)%IdleSpeedRatio
  ELSE
  ! cycling fan mode
    IF ((PTUnit(PTUnitNum)%HeatCoolMode == HeatingMode).AND. .NOT. CurDeadbandOrSetback(ZoneNum)) THEN
      CompOnMassFlow = PTUnit(PTUnitNum)%HeatMassFlowRate(1)
      CompOnFlowRatio = PTUnit(PTUnitNum)%MSHeatingSpeedRatio(1)
    ELSE IF ((PTUnit(PTUnitNum)%HeatCoolMode == CoolingMode).AND. .NOT. CurDeadbandOrSetback(ZoneNum)) THEN
      CompOnMassFlow = PTUnit(PTUnitNum)%CoolMassFlowRate(1)
      CompOnFlowRatio = PTUnit(PTUnitNum)%MSCoolingSpeedRatio(1)
    ELSE
      CompOnMassFlow = 0.0d0
      CompOnFlowRatio = 0.0d0
    END IF
    CompOffMassFlow = 0.0d0
    CompOffFlowRatio = 0.0d0
  END IF

  ! Set the inlet node mass flow rate
  IF (GetCurrentScheduleValue(PTUnit(PTUnitNum)%FanAvailSchedPtr) .gt. 0.0d0 .AND. CompOnMassFlow .NE. 0.0d0) THEN
    OnOffAirFlowRatio = 1.0d0
    IF(FirstHVACIteration)THEN
      Node(InNode)%MassFlowRate = CompOnMassFlow
      PartLoadRatio            = 0.0d0
    ELSE
      IF (PTUnit(PTUnitNum)%HeatCoolMode /= 0) THEN
        PartLoadRatio  = 1.0d0
      ELSE
        PartLoadRatio  = 0.0d0
      END IF
    END IF
  ELSE
    PartLoadRatio  = 0.0d0
    Node(InNode)%MassFlowRate           = 0.0d0
    Node(OutNode)%MassFlowRate          = 0.0d0
    Node(OutNode)%MassFlowRateMaxAvail  = 0.0d0
    OnOffAirFlowRatio      = 1.0d0
  END IF

! Set the system mass flow rates
  CALL SetVSHPAirFlow(PTUnitNum, ZoneNum, PartLoadRatio,OnOffAirFlowRatio)

END SUBROUTINE SetOnOffMassFlowRateVSCoil
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

END MODULE PackagedTerminalHeatPump
