Module Furnaces
  ! Module containing the Furnace and Unitary System simulation routines


  ! MODULE INFORMATION:
  !       AUTHOR         Dan Fisher
  !       DATE WRITTEN   Jan 2001
  !       MODIFIED       Richard Liesen, Feb 2001; Don Shirey, Mar/Oct 2001, Oct 2003
  !                      Richard Raustad, Nov 2006 - allow draw through fan and alternate air flow in cooling,
  !                      heating, and when no cooling or heating is required.
  !                      Bereket Nigusse, FSEC, June 2010 - deprecated supply air flow fraction through controlled
  !                      zone from the furnace object input field. Now, the flow fraction is calculated internally
  !                      B. Nigusse, FSEC, Jan 2012 - added steam and hot water heating coils as an option
  !                      Bo Shen, ORNL, March 2012 - added variable-speed water source heat pump cooling and heating coils, using curve-fits
  !                      Bo Shen, ORNL, July 2012 - added variable-speed air source heat pump cooling and heating coils, using curve-fits
  !
  !       RE-ENGINEERED  na


  ! PURPOSE OF THIS MODULE:
  ! To encapsulate the data and algorithms required to
  ! manage the Furnace/Unitary System Compound Component


  ! METHODOLOGY EMPLOYED:
  ! Calculates the part-load ratio of the HVAC system to meet the zone sensible load. For non-heat pump HVAC systems,
  ! if humidity control is specified and the latent capacity at the sensible PLR is insufficient to meet the latent load,
  ! calculate a latent part-load ratio to meet the zone sensible load (MultiMode dehumidificaiton control) or the zone
  ! latent load (CoolReheat dehumidification control). Use the greater of the sensible PLR and latent PLR to control
  ! the HVAC system.
  !
  ! Subroutines:
  !
  ! SimFurnace - Top level simulate routine CALLed by other modules. Each child object is simulated a final time after
  !              the part-load ratio for child components has been determined.
  !
  !  Note: A supplemental heater augments the heating capacity for both air-to-air and water-to-air heat pump systems.
  !        A reheat coil is used for the HeatCool furnace/unitarysystem to offset the sensible cooling when the
  !        dehumidification control type is COOLREHEAT. Both the supplemental and reheat heating coil load is calculated
  !        in the Calc routines. The actual simulation of these coils is performed in the SimFurnace routine (i.e. the
  !        supplemental and reheat coil loads are passed as 0 to CalcFurnaceOutput).
  !
  ! CalcNewZoneHeatOnlyFlowRates - HeatOnly furnace/unitarysystem routine.
  !                                Calculates a part-load ratio to meet the sensible load.
  !
  ! CalcNewZoneHeatCoolFlowRates - HeatCool furnace/unitarysystem and air-to-air HeatPump routine.
  !                                Calculates a part-load ratio for the system (sensible and/or latent).
  !                                For dehumidification control type COOLREHEAT, both a sensible and latent PLR
  !                                may exist for a single time step (heating and dehumidificaiton can occur). For all
  !                                other system types, only a single PLR is allowed for any given time step.
  !                                Order of simulation depends on dehumidification control option as described below.
  !
  ! Dehumidificaiton control options (non-heat pump versions):
  !
  ! Dehumidification Control NONE:   Cooling performance is simulated first and then heating performance. If a HX
  !                                  assisted cooling coil is selected, the HX is always active (cooling).
  !
  ! Dehumidification Control COOLREHEAT: For cooling operation, the sensible capacity is calculated to
  !                                      meet the thermostat setpoint. If a HX assisted cooling coil is selected,
  !                                      the HX is always active. If the latent load is not met by operating the
  !                                      system at the sensible PLR, a new PLR is calculated to meet the humidistat
  !                                      setpoint. The reheat coil load is then calculated to meet the HEATING
  !                                      setpoint temperature.
  !
  ! Dehumidification Control MULTIMODE: For cooling operation, the sensible capacity is calculated to
  !                                     meet the thermostat setpoint. If a HX assisted cooling coil is selected,
  !                                     the HX is off for this calculation. If the latent load is not met by operating
  !                                     the system at the sensible PLR, a new PLR is calculated with the HX operating
  !                                     and the target is the zone SENSIBLE load (thermostat setpoint). Humidity is not
  !                                     controlled in this mode. No reheat coil is used in this configuration.
  !
  ! CalcWaterToAirHeatPump - Water-to-air HeatPump routine.
  !                          Calculates a part-load ratio to meet the sensible load.
  !
  !
  ! CalcFurnaceOutput - Simulates each child component in order.
  !                     For cooling operation, the heating coil is off.
  !                     For heating operation, the cooling coil is off.
  !                     Reheat or supplemental heating coil is simulated here just to pass the inlet node conditions
  !                     to the output node (actual simulation of these coils is done on the final simulation of the
  !                     child components in SimFurnace).
  !                     Fan is simulated based on placement (drawthru or blowthru).
  !
  ! REFERENCES:



  ! OTHER NOTES:
  !


  ! USE STATEMENTS:
  ! Use statements for data only modules
USE DataPrecisionGlobals
USE DataLoopNode
USE DataGlobals
USE DataHVACGlobals
USE DataEnvironment, ONLY: StdBaroPress, OutDryBulbTemp, StdRhoAir

USE DataZoneEquipment
USE DataInterfaces
USE Psychrometrics, ONLY:PsyHfgAirFnWTdb, PsyCpAirFnWTdb, PsyHFnTdbW, PsyTdbFnHW,PsyRhoAirFnPbTdbW


  ! Use statements for access to subroutines in other modules
USE ScheduleManager
USE Fans,    ONLY: SimulateFanComponents
USE DXCoils, ONLY: SimDXCoil
USE VariableSpeedCoils, ONLY:MaxSpedLevels


IMPLICIT NONE         ! Enforce explicit typing of all variables


PRIVATE ! Everything private unless explicitly made public


  !MODULE PARAMETER DEFINITIONS
CHARACTER(len=*), PARAMETER :: Blank = ' '

! Last mode of operation
INTEGER, PARAMETER :: CoolingMode = 1 ! last compressor operating mode was in cooling
INTEGER, PARAMETER :: HeatingMode = 2 ! last compressor operating mode was in heating
! Airflow control for contant fan mode
INTEGER, PARAMETER :: UseCompressorOnFlow = 1 ! set compressor OFF air flow rate equal to compressor ON air flow rate
INTEGER, PARAMETER :: UseCompressorOffFlow = 2 ! set compressor OFF air flow rate equal to user defined value
! Compressor operation
INTEGER, PARAMETER :: On =  1              ! normal compressor operation
INTEGER, PARAMETER :: Off = 0              ! signal DXCoil that compressor shouldn't run


! Dehumidification control modes (DehumidControlMode)
INTEGER, PARAMETER :: DehumidControl_None       = 0
INTEGER, PARAMETER :: DehumidControl_Multimode  = 1
INTEGER, PARAMETER :: DehumidControl_CoolReheat = 2

  ! DERIVED TYPE DEFINITIONS
TYPE FurnaceEquipConditions
  CHARACTER(len=MaxNameLength) :: Name             =' ' ! Name of the Furnace
  INTEGER      :: FurnaceType_Num                  =0 ! Numeric Equivalent for Furnace Type
  INTEGER      :: FurnaceIndex                     =0 ! Index to furnace
  INTEGER      :: SchedPtr                         =0 ! Index to furnace operating schedule
  INTEGER      :: FanSchedPtr                      =0 ! Index to fan operating mode schedule
  INTEGER      :: FanAvailSchedPtr                 =0 ! Index to fan availability schedule
  INTEGER      :: ControlZoneNum                   =0 ! Index to controlled zone
  INTEGER      :: ZoneSequenceCoolingNum           =0 ! Index to cooling sequence/priority for this zone
  INTEGER      :: ZoneSequenceHeatingNum           =0 ! Index to heating sequence/priority for this zone
  INTEGER      :: CoolingCoilType_Num              =0 ! Numeric Equivalent for Cooling Coil Type
  INTEGER      :: CoolingCoilIndex                 =0 ! Index to cooling coil
  INTEGER      :: ActualDXCoilIndexforHXAssisted   =0 ! Index to DX cooling coil when HX assisted
  LOGICAL      :: CoolingCoilUpstream         =.TRUE. ! Indicates if cooling coil is upstream of heating coil
  INTEGER      :: HeatingCoilType_Num              =0 ! Numeric Equivalent for Heating Coil Type
  INTEGER      :: HeatingCoilIndex                 =0 ! Index to heating coil
  INTEGER      :: ReheatingCoilType_Num            =0 ! Numeric Equivalent for Reheat Coil Type
  INTEGER      :: ReheatingCoilIndex               =0 ! Index to reheat coil

  CHARACTER(len=MaxNameLength) :: HeatingCoilName  =' ' ! name of heating coil
  CHARACTER(len=MaxNameLength) :: HeatingCoilType  =' ' ! type of heating coil
  INTEGER      :: CoilControlNode                  =0 ! control node for hot water and steam heating coils
  INTEGER      :: HWCoilAirInletNode               =0 ! air inlet node number of HW coil for PTAC, PTHP, HeatCool, HeatOnly
  INTEGER      :: HWCoilAirOutletNode              =0 ! air outlet node number of HW coil for PTAC, PTHP, HeatCool, HeatOnly
  INTEGER      :: SuppCoilAirInletNode             =0 ! air inlet node number of HW coil for HeatCool Reheat Coil
  INTEGER      :: SuppCoilAirOutletNode            =0 ! air outlet node number of HW coil for HeatCool Reheat Coil
  INTEGER      :: SuppHeatCoilType_Num             =0 ! Numeric Equivalent for Supplemental Heat Coil Type
  INTEGER      :: SuppHeatCoilIndex                =0 ! Index to supplemental heater
  INTEGER      :: SuppCoilControlNode              =0 ! control node for steam and hot water heating coil
  CHARACTER(len=MaxNameLength) :: SuppHeatCoilName=' ' ! name of supplemental heating coil
  CHARACTER(len=MaxNameLength) :: SuppHeatCoilType=' ' ! type of supplemental heating coil

  INTEGER      :: FanType_Num                      =0 ! Integer equivalent of fan type (1=OnOff, 2 = ConstVolume)
  INTEGER      :: FanIndex                         =0 ! Index to fan object
  INTEGER      :: FurnaceInletNodeNum              =0 ! Furnace inlet node number
  INTEGER      :: FurnaceOutletNodeNum             =0 ! Furnace inlet node number
  INTEGER      :: OpMode                           =0 ! operation mode: 1 = cycling fan, cycling coils
                                                      !                 2 = continuous fan, cycling coils
  INTEGER      :: LastMode                         =0 ! last mode of operation, coolingmode or heatingmode
  INTEGER      :: AirFlowControl                   =0 ! fan control mode, UseCompressorOnFlow or UseCompressorOffFlow
  INTEGER      :: FanPlace                         =0 ! fan placement; 1=blow through, 2=draw through
  INTEGER      :: NodeNumofControlledZone          =0 ! Node number of controlled zone air node
  INTEGER      :: WatertoAirHPType                 =0 !Type of water to air heat pump model used
  REAL(r64)    :: CoolingConvergenceTolerance      =0.0d0 ! Convergence tolerance for cooling,
                                                        !   ratio (CoolingCoilLoad - FurnaceCoolingOutput)/CoolingCoilLoad
  REAL(r64)    :: HeatingConvergenceTolerance      =0.0d0 ! Convergence tolerance for heating,
                                                        !   ratio (HeatingCoilLoad - HeatPumpheatingOutput)/HeatingCoilLoad
  REAL(r64)    :: DesignHeatingCapacity            =0.0d0 ! Nominal Capacity of Heating Coil [W]
  REAL(r64)    :: DesignCoolingCapacity            =0.0d0 ! Nominal Capacity of Cooling Coil [W]
  REAL(r64)    :: CoolingCoilSensDemand            =0.0d0 ! Sensible demand on Cooling Coil [W]
  REAL(r64)    :: HeatingCoilSensDemand            =0.0d0 ! Sensible demand on Heating Coil [W]
  REAL(r64)    :: CoolingCoilLatentDemand          =0.0d0 ! Latent demand on Cooling Coil [W]
  REAL(r64)    :: DesignSuppHeatingCapacity        =0.0d0 ! Nominal Capacity of Supplemental Heating Coil [W]
  REAL(r64)    :: DesignFanVolFlowRate             =0.0d0 ! Vol Flow through the Furnace being Simulated [m**3/Sec]
  LOGICAL      :: DesignFanVolFlowRateEMSOverrideOn=.FALSE. ! if true, then EMS is calling to override autosize fan flow
  REAL(r64)    :: DesignFanVolFlowRateEMSOverrideValue=0.d0 ! EMS value for override of fan flow rate autosize [m3/s]
  REAL(r64)    :: DesignMassFlowRate               =0.0d0 ! Design mass flow rate through furnace [kg/s]
  REAL(r64)    :: MaxCoolAirVolFlow                =0.0d0 ! supply air volumetric flow rate during cooling operation [m3/s]
  LOGICAL      :: MaxCoolAirVolFlowEMSOverrideOn   =.FALSE. !if true, EMS is calling to override autosize flow during cooling
  REAL(r64)    :: MaxCoolAirVolFlowEMSOverrideValue=0.d0 ! EMS value for override of flow during cooling [m3/s]
  REAL(r64)    :: MaxHeatAirVolFlow                =0.0d0 ! supply air volumetric flow rate during cooling operation [m3/s]
  LOGICAL      :: MaxHeatAirVolFlowEMSOverrideOn   =.FALSE. ! if true, EMS is calling to override autosize flow during heating
  REAL(r64)    :: MaxHeatAirVolFlowEMSOverrideValue=0.d0  ! EMS value for override of flow during heating operation [m3/s]
  REAL(r64)    :: MaxNoCoolHeatAirVolFlow          =0.0d0 ! supply air volumetric flow rate when no cooling or heating [m3/s]
  LOGICAL      :: MaxNoCoolHeatAirVolFlowEMSOverrideOn = .FALSE. ! if true, EMS is calling to override autosize no heatcool rate
  REAL(r64)    :: MaxNoCoolHeatAirVolFlowEMSOverrideValue=0.d0 ! EMS value for override of flow during no heat cool [m3/s]
  REAL(r64)    :: MaxCoolAirMassFlow               =0.0d0 ! supply air mass flow rate during cooling operation [kg/s]
  REAL(r64)    :: MaxHeatAirMassFlow               =0.0d0 ! supply air mass flow rate during heating operation [kg/s]
  REAL(r64)    :: MaxNoCoolHeatAirMassFlow         =0.0d0 ! supply air mass flow rate when no cooling or heating [kg/s]
  REAL(r64)    :: MaxHeatCoilFluidFlow             =0.0d0 ! water or steam mass flow rate for heating coil [kg/s]
  REAL(r64)    :: MaxSuppCoilFluidFlow             =0.0d0 ! water or steam mass flow rate for supplemental heating coil [kg/s]

  REAL(r64)    :: ControlZoneMassFlowFrac          =0.0d0 ! Fraction of furnace flow to control zone
  REAL(r64)    :: DesignMaxOutletTemp              =9999.0d0 ! Maximum supply air temperature from furnace heater [C]
  REAL(r64)    :: MdotFurnace                      =0.0d0 ! Mass flow rate through furnace [kg/s]
  REAL(r64)    :: FanPartLoadRatio                 =0.0d0 ! Part load ratio of furnace fan (mdot actual/mdot design)
  REAL(r64)    :: CompPartLoadRatio                =0.0d0 ! Part load ratio of furnace compressor (load / steady-state output)
  REAL(r64)    :: WSHPRuntimeFrac                  =0.0d0 ! Runtime fraction of water source heat pump
  REAL(r64)    :: CoolPartLoadRatio                =0.0d0 ! Cooling part load ratio
  REAL(r64)    :: HeatPartLoadRatio                =0.0d0 ! Heating part load ratio
  REAL(r64)    :: MinOATCompressor                 =0.0d0 ! Minimum outdoor operating temperature for heat pump compressor
  REAL(r64)    :: MaxOATSuppHeat                   =0.0d0 ! Maximum outdoor dry-bulb temperature for
  INTEGER      :: CondenserNodeNum                 =0   ! Node number of outdoor condenser/compressor
  REAL(r64)    :: MaxONOFFCyclesperHour            =0.0d0 ! Maximum ON/OFF Cycling Rate [cycles/hr]
  REAL(r64)    :: HPTimeConstant                   =0.0d0 ! Heat Pump Time Constant [s]
  REAL(r64)    :: OnCyclePowerFraction             =0.0d0 ! Fraction of on-cycle power use [~]
                                                        ! supplemental heating coil operation
  REAL(r64)    :: FanDelayTime                     =0.0d0 ! Fan delay time, time delay for the HP's fan to
                                                        ! shut off after compressor cycle off  [s]
  Logical      :: Humidistat                       =.FALSE. ! Humidistat control (heatcool units only and not heatpump)
  Logical      :: InitHeatPump                     =.FALSE. ! Heat pump initialization flag (for error reporting)
  Integer      :: DehumidControlType_Num           =0   ! 0 = None, 1=MultiMode, 2=CoolReheat
  Integer      :: LatentMaxIterIndex               =0   ! Index to recurring warning message
  Integer      :: LatentRegulaFalsiFailedIndex     =0   ! Index to recurring warning message
  Integer      :: LatentRegulaFalsiFailedIndex2    =0   ! Index to recurring warning message
  Integer      :: SensibleMaxIterIndex             =0   ! Index to recurring warning message
  Integer      :: SensibleRegulaFalsiFailedIndex   =0   ! Index to recurring warning message
  Integer      :: WSHPHeatMaxIterIndex             =0   ! Index to recurring warning message
  Integer      :: WSHPHeatRegulaFalsiFailedIndex   =0   ! Index to recurring warning message
  Integer      :: DXHeatingMaxIterIndex            =0   ! Index to recurring warning message
  Integer      :: DXHeatingRegulaFalsiFailedIndex  =0   ! Index to recurring warning messages
  Integer      :: HeatingMaxIterIndex              =0   ! Index to recurring warning message
  Integer      :: HeatingMaxIterIndex2             =0   ! Index to recurring warning message
  Integer      :: HeatingRegulaFalsiFailedIndex    =0   ! Index to recurring warning messages
  REAL(r64)    :: ActualFanVolFlowRate             =0.0d0 ! Volumetric flow rate from fan object
  REAL(r64)    :: HeatingSpeedRatio                =1.0d0 ! Fan speed ratio in heating mode
  REAL(r64)    :: CoolingSpeedRatio                =1.0d0 ! Fan speed ratio in cooling mode
  REAL(r64)    :: NoHeatCoolSpeedRatio             =1.0d0 ! Fan speed ratio when no cooling or heating
  INTEGER      :: ZoneInletNode                    =0   ! Zone inlet node number in the controlled zone
  REAL(r64)    :: SenLoadLoss                      =0.0d0 ! Air distribution system sensible loss
  REAL(r64)    :: LatLoadLoss                      =0.0d0 ! Air distribution system latent loss
  REAL(r64)    :: SensibleLoadMet                  =0.0d0 ! System sensible load
  REAL(r64)    :: LatentLoadMet                    =0.0d0 ! System latent load
  REAL(r64)    :: DehumidInducedHeatingDemandRate  =0.0d0 ! Additional heating demand on supplemental heater
                                                        ! when heat pumps operate on dehumidification mode
  INTEGER      :: CoilOutletNode                   = 0  ! outlet node for hot water and steam heating coil
  INTEGER      :: LoopNum                          = 0  ! plant loop index for water and steam heating coil
  INTEGER      :: LoopSide                         = 0  ! plant loop side  index for water and steam heating coil
  INTEGER      :: BranchNum                        = 0  ! plant loop branch index for water and steam heating coil
  INTEGER      :: CompNum                          = 0  ! plant loop component index for water and steam heating coil

  INTEGER      :: SuppCoilOutletNode          = 0  ! outlet node for hot water and steam supplemental heating coil
  INTEGER      :: LoopNumSupp                      = 0  ! plant loop index for water and steam supplemental heating coil
  INTEGER      :: LoopSideSupp                     = 0  ! plant loop side  index for  water and steam supplemental heating coil
  INTEGER      :: BranchNumSupp                    = 0  ! plant loop branch index for water and steam supplemental heating coil
  INTEGER      :: CompNumSupp                      = 0  ! plant loop component index for water and steam supplemental heating coil

  Integer      :: HotWaterCoilMaxIterIndex         = 0  ! Index to recurring warning message
  Integer      :: HotWaterCoilMaxIterIndex2        = 0  ! Index to recurring warning message
  LOGICAL      :: EMSOverrideSensZoneLoadRequest   = .FALSE. ! if true, then EMS is calling to override zone load
  REAL(r64)    :: EMSSensibleZoneLoadValue         = 0.0D0 ! Value EMS is directing to use
  LOGICAL      :: EMSOverrideMoistZoneLoadRequest  = .FALSE. ! if true, then EMS is calling to override zone load
  REAL(r64)    :: EMSMoistureZoneLoadValue         = 0.0D0 ! Value EMS is directing to use

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
  INTEGER :: CompSpeedNum=0
  REAL(r64) :: CompSpeedRatio=0.0d0
  INTEGER :: ErrIndexCyc=0
  INTEGER :: ErrIndexVar=0
 ! end of the additional variables for variable speed water source heat pump
  INTEGER :: WaterCyclingMode = 0    ! Heat Pump Coil water flow mode; See definitions in DataHVACGlobals,
                                     ! 1=water cycling, 2=water constant, 3=water constant on demand (old mode)
END TYPE FurnaceEquipConditions

  !MODULE VARIABLE DECLARATIONS:
  INTEGER :: NumFurnaces=0                          ! The number of furnaces found in the input data file
  TYPE (FurnaceEquipConditions), ALLOCATABLE, DIMENSION(:) :: Furnace
  LOGICAL, ALLOCATABLE, DIMENSION(:) :: MySizeFlag
  LOGICAL, ALLOCATABLE, DIMENSION(:) :: CheckEquipName
  REAL(r64)    :: ModifiedHeatCoilLoad=0.0d0 ! used to adjust heating coil capacity if outlet temp > DesignMaxOutletTemp,
                                      ! used for Coil:Gas:Heating and Coil:Electric:Heating coils only.
  REAL(r64)    :: OnOffAirFlowRatioSave = 0.0d0 ! Saves the OnOffAirFlowRatio calculated in RegulaFalsi CALLs.
  REAL(r64)    :: OnOffFanPartLoadFractionSave = 0.0d0 ! Global part-load fraction passed to fan object
  REAL(r64)    :: CompOnMassFlow    = 0.0d0 ! Supply air mass flow rate w/ compressor ON [kg/s]
  REAL(r64)    :: CompOffMassFlow   = 0.0d0 ! Supply air mass flow rate w/ compressor OFF [kg/s]
  REAL(r64)    :: CompOnFlowRatio   = 0.0d0 ! fan flow ratio when coil on
  REAL(r64)    :: CompOffFlowRatio  = 0.0d0 ! fan flow ratio when coil off
  REAL(r64)    :: FanSpeedRatio     = 0.0d0 ! ratio of air flow ratio passed to fan object
  REAL(r64)    :: CoolHeatPLRRat    = 1.0d0 ! ratio of cooling to heating PLR, used for cycling fan RH control
  LOGICAL      :: HeatingLoad       = .FALSE.
  LOGICAL      :: CoolingLoad       = .FALSE.
  LOGICAL      :: EconomizerFlag    = .FALSE. ! holds air loop economizer status
  INTEGER      :: AirLoopPass = 0             ! Number of air loop pass
  LOGICAL      :: HPDehumidificationLoadFlag = .FALSE. ! true if there is dehumidification load (heat pumps only)
  REAL(r64)    :: TempSteamIn       = 100.0d0   !  steam coil steam inlet temperature
  !starting add variables for variable speed water source heat pump
  REAL(r64)    :: SaveCompressorPLR      = 0.0d0  ! holds compressor PLR from active DX coil
  CHARACTER(len=MaxNameLength) :: CurrentModuleObject   ! Object type for getting and error messages
  !ending varibles for variable speed water source heat pump

! Subroutine Specifications for the Module
          ! Driver/Manager Routines
Public  SimFurnace


          ! Get Input routines for module
PRIVATE GetFurnaceInput


          ! Initialization routines for module
PRIVATE InitFurnace
PRIVATE SizeFurnace
PRIVATE SetOnOffMassFlowRate


          ! Calculate routines to check convergence
Private CalcNewZoneHeatOnlyFlowRates
Private CalcNewZoneHeatCoolFlowRates
Private CalcWaterToAirHeatPump


          ! Supporting routines for module
Private CalcFurnaceOutput
Private CalcFurnaceResidual
Private CalcWaterToAirResidual
Private SetAverageAirFlow
Private HeatPumpRunFrac
Private CalcNonDXHeatingCoils
Private HotWaterCoilResidual

        ! modules for variable speed heat pump
Private SimVariableSpeedHP
Private SetOnOffMassFlowRateVSCoil
Private CalcVarSpeedHeatPump
Private SetVSHPAirFlow
Private VSHPSpeedResidual
Private VSHPCyclingResidual
Private ControlVSHPOutput

          ! Reporting routines for module
Private ReportFurnace


          ! Utility routines for module
          ! na

CONTAINS


! MODULE SUBROUTINES:
!*************************************************************************
SUBROUTINE SimFurnace(FurnaceName,FirstHVACIteration, AirLoopNum, CompIndex)


          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Dan Fisher
          !       DATE WRITTEN   Jan 2001
          !       MODIFIED       Richard Liesen, Oct 2001 - Richard Raustad; Bo Shen, March 2012, for VS WSHP
          !       RE-ENGINEERED  Feb 2001


          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine manages Furnace component simulation.


          ! METHODOLOGY EMPLOYED:
          ! CALL the calc routine to determine an operating PLR. Resimulate child components at this PLR.
          ! A supplemental heater augments the heating capacity for both air-to-air and water-to-air heat pump systems.
          ! A reheat coil is used for the HeatCool furnace/unitarysystem to offset the sensible cooling when the
          ! dehumidification control type is COOLREHEAT. Both the supplemental and reheat heating coil load is calculated
          ! in the Calc routines and returned here through subroutine arguments. The actual simulation of these coils is
          ! performed here (i.e. the supplemental and reheat coil loads are passed as 0 to CalcFurnaceOutput).


          ! REFERENCES:
          ! na


          ! USE STATEMENTS:
  USE InputProcessor,   ONLY: FindItemInList
  USE HeatingCoils,     Only: SimulateHeatingCoilComponents
  USE HVACHXAssistedCoolingCoil, ONLY: SimHXAssistedCoolingCoil
  USE DataAirLoop, ONLY: AirLoopControlInfo, AirLoopFlow
  USE DataZoneEnergyDemands
  USE General, ONLY: TrimSigDigits
  USE WatertoAirheatPumpSimple, ONLY: SimWatertoAirHPSimple
  USE DataHeatBalFanSys, ONLY: TempControlType
  !USE WaterCoils,               ONLY: SimulateWaterCoilComponents
  !USE PlantUtilities,           ONLY: SetComponentFlowRate
  !USE SteamCoils,               ONLY: SimulateSteamCoilComponents


  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine


          ! SUBROUTINE ARGUMENT DEFINITIONS:
  LOGICAL, INTENT (IN):: FirstHVACIteration
  CHARACTER(len=*), INTENT(IN) :: FurnaceName
  INTEGER, INTENT (IN) ::  AirLoopNum         ! Primary air loop number
  INTEGER, INTENT (INOUT) :: CompIndex        ! Pointer to which furnace


          ! SUBROUTINE PARAMETER DEFINITIONS:
  INTEGER, PARAMETER :: MaxIter = 25          ! maximum number of iterations for controlling output
  INTEGER, PARAMETER :: MaxIterCycl = 100


          ! INTERFACE BLOCK SPECIFICATIONS
          ! na


          ! DERIVED TYPE DEFINITIONS
          ! na


          ! SUBROUTINE LOCAL VARIABLE DEFINITIONS:
          ! na


          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER                       :: FurnaceNum             ! Furnace number
  LOGICAL,SAVE                  :: GetInputFlag = .True.  ! Logical to allow "GetInput" only once per simulation
  REAL(r64)                     :: HeatCoilLoad           ! Zone heating coil load
  REAL(r64)                     :: ReheatCoilLoad         ! Load to be met by the reheat coil (if high humidity control)
  REAL(r64)                     :: ZoneLoad               ! Control zone sensible load
  REAL(r64)                     :: MoistureLoad           ! Control zone latent load
  REAL(r64)                     :: H2OHtOfVap             ! Heat of vaporization of air
  INTEGER                       :: FurnaceInletNode       ! Inlet node to furnace or unitary system
  REAL(r64)                     :: FurnaceSavMdot         ! saved furnace inlet air mass flow rate [m3/s]
  REAL(r64)                     :: Dummy = 0.0d0
  INTEGER                       :: CompOp                 ! compressor operation; 1=on, 0=off
  REAL(r64)                     :: OnOffAirFlowRatio      ! Ratio of compressor ON air flow to AVERAGE air flow over time step
  INTEGER                       :: FanOpMode              ! Fan operating mode (1=CycFanCycCoil, 2=ContFanCycCoil)
  LOGICAL                       :: HXUnitOn               ! flag to control HX assisted cooling coil
  REAL(r64)                     :: ZoneLoadToCoolSPSequenced
  REAL(r64)                     :: ZoneLoadToHeatSPSequenced

  REAL(r64)                     :: QCoilReq                 ! load passed to heating coil (W)
  REAL(r64)                     :: QActual                  ! actual heating coil output (W)
  REAL(r64)                     :: mdot                     ! local temporary for mass flow rate
  REAL(r64)                     :: QCoilMax                 ! heating coil maximum capacity (W)
  REAL(r64)                     :: MinWaterFlow             ! minimum fluid flow rates
  REAL(r64)                     :: PartLoadRatioHeatingCoil ! Heating Coil Part Load Ratio
  LOGICAL                       :: SuppHeatingCoilFlag      ! true if supplemental heating coil


    ! Obtains and Allocates Furnace related parameters from input file
  IF (GetInputFlag) THEN  !First time subroutine has been entered
        !Get the furnace input
    CALL GetFurnaceInput
    GetInputFlag=.FALSE.
  End If

    ! Find the correct Furnace
  IF (CompIndex == 0) THEN
    FurnaceNum = FindItemInList(FurnaceName,Furnace%Name,NumFurnaces)
    IF (FurnaceNum == 0) THEN
      CALL ShowFatalError('SimFurnace: Unit not found='//TRIM(FurnaceName))
    ENDIF
    CompIndex=FurnaceNum
  ELSE
    FurnaceNum=CompIndex
    IF (FurnaceNum > NumFurnaces .or. FurnaceNum < 1) THEN
      CALL ShowFatalError('SimFurnace:  Invalid CompIndex passed='//  &
                          TRIM(TrimSigDigits(FurnaceNum))// &
                          ', Number of Units='//TRIM(TrimSigDigits(NumFurnaces))//  &
                          ', Entered Unit name='//TRIM(FurnaceName))
    ENDIF
    IF (CheckEquipName(FurnaceNum)) THEN
      IF (FurnaceName /= Furnace(FurnaceNum)%Name) THEN
        CALL ShowFatalError('SimFurnace: Invalid CompIndex passed='//  &
                            TRIM(TrimSigDigits(FurnaceNum))// &
                            ', Unit name='//TRIM(FurnaceName)//', stored Unit Name for that index='//  &
                            TRIM(Furnace(FurnaceNum)%Name))
      ENDIF
      CheckEquipName(FurnaceNum)=.false.
    ENDIF
  ENDIF

  HXUnitOn   = .FALSE.
  OnOffAirFlowRatio = 0.0d0
  ! here we need to deal with sequenced zone equip
  IF (Furnace(FurnaceNum)%ZoneSequenceCoolingNum > 0 .and. Furnace(FurnaceNum)%ZoneSequenceHeatingNum > 0) THEN
    ZoneLoadToCoolSPSequenced = ZoneSysEnergyDemand(Furnace(FurnaceNum)%ControlZoneNum)%&
                  SequencedOutputRequiredToCoolingSP(Furnace(FurnaceNum)%ZoneSequenceCoolingNum)
    ZoneLoadToHeatSPSequenced = ZoneSysEnergyDemand(Furnace(FurnaceNum)%ControlZoneNum)%&
                  SequencedOutputRequiredToHeatingSP(Furnace(FurnaceNum)%ZoneSequenceHeatingNum)
    IF (ZoneLoadToHeatSPSequenced > 0.d0 .AND. ZoneLoadToCoolSPSequenced > 0.d0 .AND. &
        TempControlType(Furnace(FurnaceNum)%ControlZoneNum) .NE. SingleCoolingSetPoint) THEN
      ZoneLoad = ZoneLoadToHeatSPSequenced
    ELSEIF (ZoneLoadToHeatSPSequenced > 0.d0 .AND. ZoneLoadToCoolSPSequenced > 0.d0 .AND. &
        TempControlType(Furnace(FurnaceNum)%ControlZoneNum) .EQ. SingleCoolingSetPoint) THEN
      ZoneLoad = 0.d0
    ELSEIF (ZoneLoadToHeatSPSequenced < 0.d0 .AND. ZoneLoadToCoolSPSequenced < 0.d0 .AND. &
        TempControlType(Furnace(FurnaceNum)%ControlZoneNum) .NE. SingleHeatingSetPoint) THEN
      ZoneLoad = ZoneLoadToCoolSPSequenced
    ELSEIF (ZoneLoadToHeatSPSequenced < 0.d0 .AND. ZoneLoadToCoolSPSequenced < 0.d0 .AND. &
        TempControlType(Furnace(FurnaceNum)%ControlZoneNum) .EQ. SingleHeatingSetPoint) THEN
      ZoneLoad = 0.d0
    ELSEIF (ZoneLoadToHeatSPSequenced <= 0.d0 .AND. ZoneLoadToCoolSPSequenced >= 0.d0) THEN
      ZoneLoad = 0.d0
    ENDIF
    MoistureLoad = ZoneSysMoistureDemand(Furnace(FurnaceNum)%ControlZoneNum)% &
                     SequencedOutputRequiredToDehumidSP(Furnace(FurnaceNum)%ZoneSequenceCoolingNum)
  ELSE
    ZoneLoad= ZoneSysEnergyDemand(Furnace(FurnaceNum)%ControlZoneNum)%RemainingOutputRequired
    MoistureLoad = ZoneSysMoistureDemand(Furnace(FurnaceNum)%ControlZoneNum)%OutputRequiredToDehumidifyingSP
  ENDIF

  H2OHtOfVap = PsyHfgAirFnWTdb(Node(Furnace(FurnaceNum)%NodeNumofControlledZone)%HumRat,&
                        Node(Furnace(FurnaceNum)%NodeNumofControlledZone)%Temp,'SimFurnace')

  MoistureLoad = MoistureLoad * H2OHtOfVap

    ! Initialize Furnace Flows
  Call InitFurnace(FurnaceNum, AirLoopNum, OnOffAirFlowRatio, FanOpMode, ZoneLoad, MoistureLoad, FirstHVACIteration)

  FurnaceInletNode = Furnace(FurnaceNum)%FurnaceInletNodeNum
  FurnaceSavMdot = Node(FurnaceInletNode)%MassFlowRate
  CompOp = On
  CoolHeatPLRRat = 1.0d0

  ! Simulate correct system type (1 of 4 choices)
  SELECT CASE(Furnace(FurnaceNum)%FurnaceType_Num)

  ! Simulate HeatOnly systems:
  CASE(Furnace_HeatOnly, UnitarySys_HeatOnly)


     ! Update the furnace flow rates
     Call CalcNewZoneHeatOnlyFlowRates(FurnaceNum,FirstHVACIteration,ZoneLoad,HeatCoilLoad,OnOffAirFlowRatio)

     IF (Furnace(FurnaceNum)%FanPlace .EQ. BlowThru) THEN
       ! simulate fan
       CALL SimulateFanComponents(Blank,FirstHVACIteration,Furnace(FurnaceNum)%FanIndex,FanSpeedRatio)
     END IF

       ! simulate furnace heating coil
     SuppHeatingCoilFlag = .FALSE.       ! if true simulates supplemental heating coil
     CALL CalcNonDXHeatingCoils(FurnaceNum,SuppHeatingCoilFlag,FirstHVACIteration,HeatCoilLoad,FanOpMode,QActual)

     IF (Furnace(FurnaceNum)%FanPlace .EQ. DrawThru) THEN
       ! simulate fan
       CALL SimulateFanComponents(Blank,FirstHVACIteration,Furnace(FurnaceNum)%FanIndex,FanSpeedRatio)
     END IF

  ! Simulate HeatCool sytems:
  CASE(Furnace_HeatCool, UnitarySys_HeatCool)

   IF(Furnace(FurnaceNum)%CoolingCoilType_Num == Coil_CoolingAirToAirVariableSpeed) THEN
       ! variable speed cooling coil
       HeatCoilLoad=0.0d0
       Call SimVariableSpeedHP(FurnaceNum,FirstHVACIteration, ZoneLoad,MoistureLoad, OnOffAirFlowRatio)
   ELSE
     ! calculate the system flow rate
     IF ( .NOT. FirstHVACIteration .AND. Furnace(FurnaceNum)%OpMode == CycFanCycCoil .AND. CoolingLoad &
          .AND. AirLoopControlInfo(AirLoopNum)%EconoActive) THEN
       ! for cycling fan, cooling load, check whether furnace can meet load with compressor off
       CompOp = Off
       Call CalcNewZoneHeatCoolFlowRates(FurnaceNum,FirstHVACIteration,CompOp,ZoneLoad,MoistureLoad, &
                                         HeatCoilLoad, ReheatCoilLoad, OnOffAirFlowRatio, HXUnitOn)
       IF (Furnace(FurnaceNum)%CoolPartLoadRatio >= 1.0d0 .OR. Furnace(FurnaceNum)%HeatPartLoadRatio >= 1.0d0 &
           .OR. (Furnace(FurnaceNum)%CoolPartLoadRatio <= 0.0d0 .AND. Furnace(FurnaceNum)%HeatPartLoadRatio <= 0.0d0)) THEN
         ! compressor on (reset inlet air mass flow rate to starting value)
         Node(FurnaceInletNode)%MassFlowRate = FurnaceSavMdot
         CompOp = On
         Call CalcNewZoneHeatCoolFlowRates(FurnaceNum,FirstHVACIteration,CompOp,ZoneLoad,MoistureLoad, &
                                           HeatCoilLoad, ReheatCoilLoad, OnOffAirFlowRatio, HXUnitOn)
        END IF
     ELSE
       ! compressor on
       Call CalcNewZoneHeatCoolFlowRates(FurnaceNum,FirstHVACIteration,CompOp,ZoneLoad,MoistureLoad, &
                                         HeatCoilLoad, ReheatCoilLoad, OnOffAirFlowRatio, HXUnitOn)
     END IF

     IF (Furnace(FurnaceNum)%FanPlace .EQ. BlowThru) THEN
       ! simulate fan
       CALL SimulateFanComponents(Blank,FirstHVACIteration,Furnace(FurnaceNum)%FanIndex,FanSpeedRatio)
     END IF

     IF(.NOT. Furnace(FurnaceNum)%CoolingCoilUpstream)THEN
       ! simulate furnace heating coil
        SuppHeatingCoilFlag = .FALSE.       ! if true simulates supplemental heating coil
        CALL CalcNonDXHeatingCoils(FurnaceNum,SuppHeatingCoilFlag,FirstHVACIteration,HeatCoilLoad,FanOpMode,QActual)
     END IF

     ! simulate furnace DX cooling coil
     IF (Furnace(FurnaceNum)%CoolingCoilType_Num == CoilDX_CoolingHXAssisted) THEN
      CALL SimHXAssistedCoolingCoil(Blank,FirstHVACIteration,CompOp,Furnace(FurnaceNum)%CoolPartLoadRatio,  &
                                    Furnace(FurnaceNum)%CoolingCoilIndex, FanOpMode, &
                                    HXUnitEnable=HXUnitOn, OnOffAFR = OnOffAirFlowRatio, EconomizerFlag=EconomizerFlag)
     ELSE
       CALL SimDXCoil(Blank,CompOp,FirstHVACIteration,Furnace(FurnaceNum)%CoolPartLoadRatio,Furnace(FurnaceNum)%CoolingCoilIndex, &
                      FanOpMode, OnOffAFR = OnOffAirFlowRatio, CoilCoolingHeatingPLRRatio = CoolHeatPLRRat)
     END IF


     IF(Furnace(FurnaceNum)%CoolingCoilUpstream)THEN
       ! simulate furnace heating coil
        SuppHeatingCoilFlag = .FALSE.       ! if true simulates supplemental heating coil
        CALL CalcNonDXHeatingCoils(FurnaceNum,SuppHeatingCoilFlag,FirstHVACIteration,HeatCoilLoad,FanOpMode,QActual)
     END IF

     IF (Furnace(FurnaceNum)%FanPlace .EQ. DrawThru) THEN
       ! simulate fan
       CALL SimulateFanComponents(Blank,FirstHVACIteration,Furnace(FurnaceNum)%FanIndex,FanSpeedRatio)
     END IF

     ! Simulate furnace reheat coil if a humidistat is used or if the reheat coil is present
     IF(Furnace(FurnaceNum)%DehumidControlType_Num == DehumidControl_CoolReheat .OR.   &
        Furnace(FurnaceNum)%SuppHeatCoilIndex .GT. 0)THEN
        SuppHeatingCoilFlag = .TRUE.       ! if truee simulates supplemental heating coil
        CALL CalcNonDXHeatingCoils(FurnaceNum,SuppHeatingCoilFlag,FirstHVACIteration,ReheatCoilLoad,FanOpMode,QActual)
     END IF
   END IF

  ! Simulate air-to-air heat pumps:
  CASE(UnitarySys_HeatPump_AirToAir)
   IF(Furnace(FurnaceNum)%HeatingCoilType_Num == Coil_HeatingAirToAirVariableSpeed) THEN
       ! variable speed heat pump
       HeatCoilLoad=0.0d0
       Call SimVariableSpeedHP(FurnaceNum,FirstHVACIteration, ZoneLoad,MoistureLoad, OnOffAirFlowRatio)
   ELSE
     ! Update the furnace flow rates
     IF ( .NOT. FirstHVACIteration .AND. Furnace(FurnaceNum)%OpMode == CycFanCycCoil .AND. CoolingLoad &
          .AND. AirLoopControlInfo(AirLoopNum)%EconoActive) THEN
       ! for cycling fan, cooling load, check whether furnace can meet load with compressor off
       CompOp = Off
       Call CalcNewZoneHeatCoolFlowRates(FurnaceNum,FirstHVACIteration,CompOp,ZoneLoad,MoistureLoad, &
                                         HeatCoilLoad, ReheatCoilLoad, OnOffAirFlowRatio, HXUnitOn)
       IF (Furnace(FurnaceNum)%CoolPartLoadRatio >= 1.0d0 .OR. Furnace(FurnaceNum)%HeatPartLoadRatio >= 1.0d0 &
           .OR. (Furnace(FurnaceNum)%CoolPartLoadRatio <= 0.0d0 .AND. Furnace(FurnaceNum)%HeatPartLoadRatio <= 0.0d0)) THEN
         ! compressor on (reset inlet air mass flow rate to starting value)
         CompOp = On
         Node(FurnaceInletNode)%MassFlowRate = FurnaceSavMdot
         Call CalcNewZoneHeatCoolFlowRates(FurnaceNum,FirstHVACIteration,CompOp,ZoneLoad,MoistureLoad, &
                                           HeatCoilLoad, ReheatCoilLoad, OnOffAirFlowRatio, HXUnitOn)
        END IF
     ELSE
       ! compressor on
       Call CalcNewZoneHeatCoolFlowRates(FurnaceNum,FirstHVACIteration,CompOp,ZoneLoad,MoistureLoad, &
                                         HeatCoilLoad, ReheatCoilLoad, OnOffAirFlowRatio, HXUnitOn)
     END IF


     IF (Furnace(FurnaceNum)%FanPlace .EQ. BlowThru) THEN
       CALL SimulateFanComponents(Blank,FirstHVACIteration,Furnace(FurnaceNum)%FanIndex,FanSpeedRatio)
     END IF

     IF (Furnace(FurnaceNum)%CoolingCoilType_Num == CoilDX_CoolingHXAssisted) THEN
      CALL SimHXAssistedCoolingCoil(Blank,FirstHVACIteration,CompOp,Furnace(FurnaceNum)%CoolPartLoadRatio,  &
                                    Furnace(FurnaceNum)%CoolingCoilIndex, FanOpMode, &
                                    HXUnitEnable=HXUnitOn, OnOffAFR = OnOffAirFlowRatio, EconomizerFlag=EconomizerFlag)
     ELSE
      CALL SimDXCoil(Blank,CompOp,FirstHVACIteration,Furnace(FurnaceNum)%CoolPartLoadRatio,Furnace(FurnaceNum)%CoolingCoilIndex, &
                     FanOpMode, OnOffAFR = OnOffAirFlowRatio)
     END IF
     CALL SimDXCoil(Blank,CompOp,FirstHVACIteration,Furnace(FurnaceNum)%HeatPartLoadRatio,  &
                   Furnace(FurnaceNum)%HeatingCoilIndex, FanOpMode, OnOffAFR = OnOffAirFlowRatio)
     IF (Furnace(FurnaceNum)%FanPlace .EQ. DrawThru) THEN
       CALL SimulateFanComponents(Blank,FirstHVACIteration,Furnace(FurnaceNum)%FanIndex,FanSpeedRatio)
     END IF

     ! Simulate furnace reheat coil if a humidistat is present, the dehumidification type of coolreheat and
     ! reheat coil load exists
     IF(Furnace(FurnaceNum)%DehumidControlType_Num == DehumidControl_CoolReheat .AND. ReheatCoilLoad .GT. 0.d0)THEN
        SuppHeatingCoilFlag = .TRUE.       ! if truee simulates supplemental heating coil
        CALL CalcNonDXHeatingCoils(FurnaceNum,SuppHeatingCoilFlag,FirstHVACIteration,ReheatCoilLoad,FanOpMode,QActual)
     ELSE
        SuppHeatingCoilFlag = .TRUE.       ! if true simulates supplemental heating coil
        CALL CalcNonDXHeatingCoils(FurnaceNum,SuppHeatingCoilFlag,FirstHVACIteration,HeatCoilLoad,FanOpMode,QActual)
     ENDIF

   END IF
  ! Simulate water-to-air systems:
  CASE(UnitarySys_HeatPump_WaterToAir)

     IF(Furnace(FurnaceNum)%WatertoAirHPType == WatertoAir_Simple) THEN
     ! Update the furnace flow rates
!
!   When CompOp logic is added to the child cooling coil (COIL:WaterToAirHP:EquationFit:Cooling), then this logic
!   needs to be reinstated.. to align with Unitary/Furnace HeatCool and Unitary Air-to-Air Heat Pump (see above).
!
     IF ( .NOT. FirstHVACIteration .AND. Furnace(FurnaceNum)%OpMode == CycFanCycCoil .AND. CoolingLoad &
          .AND. AirLoopControlInfo(AirLoopNum)%EconoActive) THEN
       ! for cycling fan, cooling load, check whether furnace can meet load with compressor off
       CompOp = Off
       Call CalcNewZoneHeatCoolFlowRates(FurnaceNum,FirstHVACIteration,CompOp,ZoneLoad,MoistureLoad, &
                                         HeatCoilLoad, ReheatCoilLoad, OnOffAirFlowRatio, HXUnitOn)
       IF (Furnace(FurnaceNum)%CoolPartLoadRatio >= 1.0d0 .OR. Furnace(FurnaceNum)%HeatPartLoadRatio >= 1.0d0 &
           .OR. (Furnace(FurnaceNum)%CoolPartLoadRatio <= 0.0d0 .AND. Furnace(FurnaceNum)%HeatPartLoadRatio <= 0.0d0)) THEN
         ! compressor on (reset inlet air mass flow rate to starting value)
         CompOp = On
         Node(FurnaceInletNode)%MassFlowRate = FurnaceSavMdot
         Call CalcNewZoneHeatCoolFlowRates(FurnaceNum,FirstHVACIteration,CompOp,ZoneLoad,MoistureLoad, &
                                           HeatCoilLoad, ReheatCoilLoad, OnOffAirFlowRatio, HXUnitOn)
        END IF
      ELSE
       ! compressor on
       Call CalcNewZoneHeatCoolFlowRates(FurnaceNum,FirstHVACIteration,CompOp,ZoneLoad,MoistureLoad, &
                                         HeatCoilLoad, ReheatCoilLoad, OnOffAirFlowRatio, HXUnitOn)
      END IF
      IF (Furnace(FurnaceNum)%FanPlace .EQ. BlowThru) THEN
        CALL SimulateFanComponents(Blank,FirstHVACIteration,Furnace(FurnaceNum)%FanIndex,FanSpeedRatio)
      END IF

      CALL SimWatertoAirHPSimple(Blank, Furnace(FurnaceNum)%CoolingCoilIndex, &
              Furnace(FurnaceNum)%CoolingCoilSensDemand, Furnace(FurnaceNum)%CoolingCoilLatentDemand, &
              Furnace(FurnaceNum)%OpMode,Furnace(FurnaceNum)%WSHPRuntimeFrac, Furnace(FurnaceNum)%MaxONOFFCyclesperHour, &
              Furnace(FurnaceNum)%HPTimeConstant, Furnace(FurnaceNum)%FanDelayTime, CompOp, &
              Furnace(FurnaceNum)%CoolPartLoadRatio, FirstHVACIteration)
      CALL SimWatertoAirHPSimple(Blank, Furnace(FurnaceNum)%HeatingCoilIndex, &
              Furnace(FurnaceNum)%HeatingCoilSensDemand, dummy, Furnace(FurnaceNum)%OpMode,  &
              Furnace(FurnaceNum)%WSHPRuntimeFrac, &
              Furnace(FurnaceNum)%MaxONOFFCyclesperHour, Furnace(FurnaceNum)%HPTimeConstant, &
              Furnace(FurnaceNum)%FanDelayTime, CompOp, &
              Furnace(FurnaceNum)%HeatPartLoadRatio, FirstHVACIteration)

      IF (Furnace(FurnaceNum)%FanPlace .EQ. DrawThru) THEN
        CALL SimulateFanComponents(Blank,FirstHVACIteration,Furnace(FurnaceNum)%FanIndex,FanSpeedRatio)
      END IF
      IF(Furnace(FurnaceNum)%DehumidControlType_Num == DehumidControl_CoolReheat .AND. ReheatCoilLoad .GT. 0.d0)THEN
        SuppHeatingCoilFlag = .TRUE.       ! if true simulates supplemental heating coil
        CALL CalcNonDXHeatingCoils(FurnaceNum,SuppHeatingCoilFlag,FirstHVACIteration,ReheatCoilLoad,FanOpMode,QActual)
      ELSE
        SuppHeatingCoilFlag = .TRUE.       ! if true simulates supplemental heating coil
        CALL CalcNonDXHeatingCoils(FurnaceNum,SuppHeatingCoilFlag,FirstHVACIteration,HeatCoilLoad,FanOpMode,QActual)
      ENDIF
     ELSE IF(Furnace(FurnaceNum)%WatertoAirHPType == WatertoAir_ParEst) THEN

      ! simulate the heat pump
       HeatCoilLoad=0.0d0
       CALL CalcWaterToAirHeatpump(AirLoopNum, FurnaceNum,FirstHVACIteration,CompOp,ZoneLoad,MoistureLoad)
     ELSE IF(Furnace(FurnaceNum)%WatertoAirHPType == WatertoAir_VarSpeedEquationFit) THEN
       ! simulate the heat pump
       HeatCoilLoad=0.0d0
       Call SimVariableSpeedHP(FurnaceNum,FirstHVACIteration, ZoneLoad,MoistureLoad, OnOffAirFlowRatio)

     ELSE IF(Furnace(FurnaceNum)%WatertoAirHPType == WatertoAir_VarSpeedLooUpTable) THEN

     END IF

  CASE DEFAULT
  ! will never get here, all system types are simulated above

  END SELECT

  ! set the econo lockout flags
  IF (Furnace(FurnaceNum)%CompPartLoadRatio > 0.0d0 .AND. &
      AirLoopControlInfo(AirLoopNum)%CanLockoutEconoWithCompressor) THEN
    AirLoopControlInfo(AirLoopNum)%ReqstEconoLockoutWithCompressor = .TRUE.
  ELSE
    AirLoopControlInfo(AirLoopNum)%ReqstEconoLockoutWithCompressor = .FALSE.
  END IF

  IF ((HeatCoilLoad > 0.0d0 .OR. Furnace(FurnaceNum)%HeatPartLoadRatio > 0.0d0) .AND. &
      (AirLoopControlInfo(AirLoopNum)%CanLockoutEconoWithCompressor .OR. &
       AirLoopControlInfo(AirLoopNum)%CanLockoutEconoWithHeating)) THEN
    AirLoopControlInfo(AirLoopNum)%ReqstEconoLockoutWithHeating = .TRUE.
  ELSE
    AirLoopControlInfo(AirLoopNum)%ReqstEconoLockoutWithHeating = .FALSE.
  END IF

  IF(Furnace(FurnaceNum)%OpMode == CycFanCycCoil)THEN
    AirLoopflow(AirLoopNum)%FanPLR = Furnace(FurnaceNum)%FanPartLoadRatio
  END IF

  ! Report the current Furnace output
  Call ReportFurnace(FurnaceNum)

  ! Reset OnOffFanPartLoadFraction to 1 in case another on/off fan is called without a part-load curve
  OnOffFanPartLoadFraction = 1.0d0

  RETURN


END SUBROUTINE SimFurnace


! Get Input Section of the Module
!******************************************************************************
SUBROUTINE GetFurnaceInput


          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Liesen
          !       DATE WRITTEN   Feb 2001
          !       MODIFIED       Don Shirey and Rich Raustad, Mar/Oct 2001, Mar 2003
          !                      Bereket Nigusse, April 2010 - deprecated supply air flow fraction through
          !                      controlled zone from the input field.
          !                      Bo Shen, March 2012, add inputs for VS WSHP,
          !                      Bo Shen, ORNL, July 2012 - added variable-speed air source heat pump cooling and heating coils, using curve-fits
          !       RE-ENGINEERED  na


          ! PURPOSE OF THIS SUBROUTINE:
          ! Obtains input data for fans and coils and stores it in the Furnace data structures


          ! METHODOLOGY EMPLOYED:
          ! Uses "Get" routines to read in data.


          ! REFERENCES:


          ! USE STATEMENTS:
    USE InputProcessor,             ONLY: GetNumObjectsFound, GetObjectItem, VerifyName, SameString, FindItemInList,   &
                                          GetObjectDefMaxArgs
    USE NodeInputManager,           ONLY: GetOnlySingleNode
    USE DataLoopNode,               ONLY: NodeID
    USE DataHeatBalance,            ONLY: Zone
    USE BranchNodeConnections,      ONLY: SetUpCompSets, TestCompSet
    USE DataAirSystems,             ONLY: PrimaryAirSystem
    USE DataZoneControls,           ONLY: TempControlledZone, NumTempControlledZones, HumidityControlZone,   &
                                          NumHumidityControlZones, ComfortControlledZone, NumComfortControlledZones
    USE WaterToAirHeatPumpSimple,   ONLY: GetWtoAHPSimpleCoilCapacity=>GetCoilCapacity,  &
                                          GetWtoAHPSimpleCoilInletNode=>GetCoilInletNode, &
                                          GetWtoAHPSimpleCoilOutletNode=>GetCoilOutletNode, &
                                          GetWtoAHPSimpleCoilIndex=>GetCoilIndex, &
                                          SetSimpleWSHPData, GetWtoAHPSimpleCoilAirFlow=>GetCoilAirFlowRate
    USE VariableSpeedCoils,      ONLY: GetCoilCapacityVariableSpeed, &
                                          GetCoilInletNodeVariableSpeed, &
                                          GetCoilOutletNodeVariableSpeed, &
                                          GetCoilIndexVariableSpeed, &
                                          GetCoilAirFlowRateVariableSpeed,&
                                          SetVarSpeedCoilData, GetVSCoilCondenserInletNode, &
                                          GetVSCoilMinOATCompressor
    USE WaterToAirHeatPump,         ONLY: GetWtoAHPCoilCapacity=>GetCoilCapacity, &
                                          GetWtoAHPCoilInletNode=>GetCoilInletNode, &
                                          GetWtoAHPCoilOutletNode=>GetCoilOutletNode,GetWtoAHPCoilIndex=>GetCoilIndex
    USE HeatingCoils,               ONLY: GetHeatingCoilCapacity=>GetCoilCapacity,GetHeatingCoilInletNode=>GetCoilInletNode, &
                                          GetHeatingCoilOutletNode=>GetCoilOutletNode, GetHeatingCoilIndex=>GetCoilIndex, &
                                          GetHeatingCoilTypeNum, GetHeatingCoilPLFCurveIndex
    USE DXCoils,                    ONLY: GetDXCoilCapacity=>GetCoilCapacity,GetMinOATDXCoilCompressor=>GetMinOATCompressor,  &
                                          GetDXCoilInletNode=>GetCoilInletNode, GetDXCoilOutletNode=>GetCoilOutletNode, &
                                          GetDXCoilCondenserInletNode=>GetCoilCondenserInletNode,GetDXCoilIndex, &
                                          GetDXCoilTypeNum=>GetCoilTypeNum, SetDXCoolingCoilData
    USE HVACHXAssistedCoolingCoil,  ONLY: GetDXHXAsstdCoilCapacity=>GetCoilCapacity,GetDXHXAsstdCoilInletNode=>GetCoilInletNode, &
                                          GetDXHXAsstdCoilOutletNode=>GetCoilOutletNode,GetHXDXCoilName,GetHXDXCoilIndex, &
                                          GetHXAssistedCoilTypeNum=>GetCoilGroupTypeNum, GetActualDXCoilIndex
    USE WaterCoils,                 ONLY: GetCoilWaterInletNode, GetCoilMaxWaterFlowRate, &
                                          GetWaterCoilInletNode=>GetCoilInletNode,GetWaterCoilOutletNode=>GetCoilOutletNode
    USE SteamCoils,                 ONLY: GetSteamCoilAirInletNode=>GetCoilAirInletNode, GetSteamCoilIndex, &
                                          GetSteamCoilAirOutletNode=>GetCoilAirOutletNode, &
                                          GetSteamCoilSteamInletNode=>GetCoilSteamInletNode, &
                                          GetCoilMaxSteamFlowRate=>GetCoilMaxSteamFlowRate, GetTypeOfCoil, ZoneLoadControl
    USE Fans,                       ONLY: GetFanDesignVolumeFlowRate,GetFanInletNode,GetFanOutletNode,GetFanIndex, &
                                          GetFanAvailSchPtr, GetFanType
    USE FluidProperties,            ONLY: GetSatDensityRefrig
    USE General,                    ONLY: RoundSigDigits, TrimSigDigits
    USE DataSizing,                 ONLY: AutoSize
    USE OutAirNodeManager,          ONLY: CheckOutAirNodeNumber
    USE DataIPShortCuts
    USE EMSManager,                 ONLY: ManageEMS
    USE HVACControllers,            ONLY: CheckCoilWaterInletNode
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
    INTEGER :: FurnaceNum      ! The Furnace that you are currently loading input into
    INTEGER :: GetObjectNum    ! The index to each specific object name
    INTEGER :: NumFields       ! Total number of fields in object
    INTEGER :: NumAlphas       ! Total number of alpha fields in object
    INTEGER :: MaxAlphas       ! Maximum number of alpha fields in all objects
    INTEGER :: NumNumbers      ! Total number of numeric fields in object
    INTEGER :: MaxNumbers      ! Maximum number of numeric fields in all objects
    INTEGER :: IOStatus        ! Function call status
    REAL(r64), ALLOCATABLE, DIMENSION(:)                    :: Numbers        ! Numeric data
    CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: Alphas         ! Alpha data
    CHARACTER(Len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: cAlphaFields   ! Alpha field names
    CHARACTER(Len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: cNumericFields ! Numeric field names
    LOGICAL, ALLOCATABLE, DIMENSION(:)   :: lAlphaBlanks      ! Logical array, alpha field input BLANK = .TRUE.
    LOGICAL, ALLOCATABLE, DIMENSION(:)   :: lNumericBlanks    ! Logical array, numeric field input BLANK = .TRUE.
    CHARACTER(len=MaxNameLength) :: CompSetFanInlet, CompSetFanOutlet, CompSetCoolInlet, CompSetHeatInlet, CompSetHeatOutlet
    CHARACTER(len=MaxNameLength)   :: CurrentModuleObject ! Object type for getting and error messages
    LOGICAL :: ErrorsFound = .FALSE.   ! If errors detected in input
    LOGICAL :: IsNotOK               ! Flag to verify name
    LOGICAL :: IsBlank               ! Flag for blank name
    Integer :: NumHeatOnly           ! Number of heat only furnaces
    Integer :: NumHeatCool           ! Number of heat/cool furnaces
    Integer :: HeatOnlyNum           ! Index to heat only furnaces
    Integer :: HeatCoolNum           ! Index to heat/cool furnaces
    Integer :: NumUnitaryHeatOnly    ! Number of heat only unitary systems
    Integer :: NumUnitaryHeatCool    ! Number of heat/cool unitary systems
    Integer :: UnitaryHeatOnlyNum    ! Index to heat only furnaces
    Integer :: UnitaryHeatCoolNum    ! Index to heat/cool unitary systems
    Integer :: NumWaterToAirHeatPump ! Number of water-to-air heat pumps
    Integer :: NumHeatPump           ! Number of air-to-air or water-to-air heat pumps
    Integer :: HeatPumpNum           ! Index to air-to-air heat pumps
    Integer :: ControlledZoneNum     ! Index to controlled zones
    LOGICAL :: AirNodeFound          ! Used to determine if control zone is valid
    LOGICAL :: AirLoopFound          ! Used to determine if control zone is served by furnace air loop
    INTEGER :: AirLoopNumber         ! Used to determine if control zone is served by furnace air loop
    INTEGER :: BranchNum             ! Used to determine if control zone is served by furnace air loop
    INTEGER :: CompNum               ! Used to determine if control zone is served by furnace air loop
    INTEGER :: TstatZoneNum          ! Used to determine if control zone has a thermostat object
    INTEGER :: HstatZoneNum          ! Used to determine if control zone has a humidistat object
    LOGICAL :: ErrFlag               ! Mining function error flag
    INTEGER :: FanInletNode          ! Used for node checking warning messages
    INTEGER :: FanOutletNode         ! Used for node checking warning messages
    INTEGER :: CoolingCoilInletNode  ! Used for node checking warning messages
    INTEGER :: CoolingCoilOutletNode ! Used for node checking warning messages
    INTEGER :: HeatingCoilInletNode  ! Used for node checking warning messages
    INTEGER :: HeatingCoilOutletNode ! Used for node checking warning messages
    INTEGER :: SupHeatCoilInletNode  ! Used for node checking warning messages
    INTEGER :: SupHeatCoilOutletNode ! Used for node checking warning messages
    INTEGER :: ReHeatCoilInletNode   ! Used for node checking warning messages
    INTEGER :: ReHeatCoilOutletNode  ! Used for node checking warning messages
    REAL(r64)    :: FanVolFlowRate        ! Fan Max Flow Rate from Fan object (for comparisons to validity)
    INTEGER :: FurnaceType_Num       ! Integer equivalent of Furnace or UnitarySystem "type"
    CHARACTER(len=MaxNameLength)   :: CoolingCoilType     ! Used in mining function CALLS
    CHARACTER(len=MaxNameLength)   :: CoolingCoilName     ! Used in mining function CALLS
    CHARACTER(len=MaxNameLength)   :: HeatingCoilType     ! Used in mining function CALLS
    CHARACTER(len=MaxNameLength)   :: HeatingCoilName     ! Used in mining function CALLS
    CHARACTER(len=MaxNameLength)   :: ReheatingCoilType   ! Used in mining function CALLS
    CHARACTER(len=MaxNameLength)   :: ReheatingCoilName   ! Used in mining function CALLS
    CHARACTER(len=MaxNameLength)   :: SuppHeatCoilType    ! Used in mining function CALLS
    CHARACTER(len=MaxNameLength)   :: SuppHeatCoilName    ! Used in mining function CALLS
    CHARACTER(len=MaxNameLength)   :: FanType             ! Used in mining function CALLS
    CHARACTER(len=MaxNameLength)   :: FanName             ! Used in mining function CALLS
    LOGICAL                        :: PrintMessage        ! Used in mining function CALLS
    INTEGER                        :: TotalZonesOnAirLoop ! number of zones attached to air loop
    INTEGER                        :: HeatingCoilPLFCurveIndex ! index of heating coil PLF curve
    INTEGER                        :: SteamIndex          !  steam coil index
    REAL(r64)                      :: SteamDensity        !  density of steam at 100C
    INTEGER                        :: DXCoilIndex         ! Index to DX coil in HXAssited object


          ! Flow
    MaxNumbers=0
    MaxAlphas=0

    CurrentModuleObject  = 'AirLoopHVAC:Unitary:Furnace:HeatOnly'
    NumHeatOnly          = GetNumObjectsFound(CurrentModuleObject)
    CALL GetObjectDefMaxArgs(CurrentModuleObject,NumFields,NumAlphas,NumNumbers)
    MaxNumbers=MAX(MaxNumbers,NumNumbers)
    MaxAlphas=MAX(MaxAlphas,NumAlphas)

    CurrentModuleObject  = 'AirLoopHVAC:Unitary:Furnace:HeatCool'
    NumHeatCool          = GetNumObjectsFound(CurrentModuleObject)
    CALL GetObjectDefMaxArgs(CurrentModuleObject,NumFields,NumAlphas,NumNumbers)
    MaxNumbers=MAX(MaxNumbers,NumNumbers)
    MaxAlphas=MAX(MaxAlphas,NumAlphas)

    CurrentModuleObject  = 'AirLoopHVAC:UnitaryHeatOnly'
    NumUnitaryHeatOnly   = GetNumObjectsFound(CurrentModuleObject)
    CALL GetObjectDefMaxArgs(CurrentModuleObject,NumFields,NumAlphas,NumNumbers)
    MaxNumbers=MAX(MaxNumbers,NumNumbers)
    MaxAlphas=MAX(MaxAlphas,NumAlphas)

    CurrentModuleObject  = 'AirLoopHVAC:UnitaryHeatCool'
    NumUnitaryHeatCool   = GetNumObjectsFound(CurrentModuleObject)
    CALL GetObjectDefMaxArgs(CurrentModuleObject,NumFields,NumAlphas,NumNumbers)
    MaxNumbers=MAX(MaxNumbers,NumNumbers)
    MaxAlphas=MAX(MaxAlphas,NumAlphas)

    CurrentModuleObject  = 'AirLoopHVAC:UnitaryHeatPump:AirToAir'
    NumHeatPump   = GetNumObjectsFound(CurrentModuleObject)
    CALL GetObjectDefMaxArgs(CurrentModuleObject,NumFields,NumAlphas,NumNumbers)
    MaxNumbers=MAX(MaxNumbers,NumNumbers)
    MaxAlphas=MAX(MaxAlphas,NumAlphas)

    CurrentModuleObject  = 'AirLoopHVAC:UnitaryHeatPump:WaterToAir'
    NumWaterToAirHeatPump = GetNumObjectsFound(CurrentModuleObject)
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

    NumFurnaces = NumHeatOnly + NumHeatCool + NumUnitaryHeatOnly + NumUnitaryHeatCool + NumHeatPump + NumWaterToAirHeatPump

    IF (NumFurnaces.GT.0) THEN
      ALLOCATE(Furnace(NumFurnaces))
    ENDIF
    ALLOCATE(CheckEquipName(NumFurnaces))
    CheckEquipName=.true.

      ! Get the data for the HeatOnly Furnace
      DO HeatOnlyNum = 1,  NumHeatOnly + NumUnitaryHeatOnly

        FanInletNode          = 0
        FanOutletNode         = 0
        FanVolFlowRate        = 0.0d0
        HeatingCoilInletNode  = 0
        HeatingCoilOutletNode = 0

!       Furnace and UnitarySystem objects are both read in here.
!       Will still have 2 differently named objects for the user, but read in with 1 DO loop.
        IF(HeatOnlyNum .LE. NumHeatOnly) THEN
          CurrentModuleObject  = 'AirLoopHVAC:Unitary:Furnace:HeatOnly'
          FurnaceType_Num = Furnace_HeatOnly
          GetObjectNum    = HeatOnlyNum
        ELSE
          CurrentModuleObject  = 'AirLoopHVAC:UnitaryHeatOnly'
          FurnaceType_Num = UnitarySys_HeatOnly
          GetObjectNum    = HeatOnlyNum - NumHeatOnly
        END IF

        FurnaceNum  = HeatOnlyNum
        Furnace(FurnaceNum)%FurnaceType_Num = FurnaceType_Num

        CALL GetObjectItem(CurrentModuleObject,GetObjectNum,Alphas,NumAlphas,Numbers,NumNumbers,IOStatus, &
                           NumBlank=lNumericBlanks,AlphaBlank=lAlphaBlanks, &
                           AlphaFieldNames=cAlphaFields,NumericFieldNames=cNumericFields)

        IsNotOK=.FALSE.
        IsBlank=.FALSE.
        CALL VerifyName(Alphas(1),Furnace%Name,FurnaceNum-1,IsNotOK,IsBlank,TRIM(CurrentModuleObject)//' Name')
        IF (IsNotOK) THEN
          ErrorsFound=.TRUE.
          IF (IsBlank) Alphas(1)='xxxxx'
        ENDIF

        Furnace(FurnaceNum)%Name            = Alphas(1)
        IF (lAlphaBlanks(2)) THEN
          Furnace(FurnaceNum)%SchedPtr        = ScheduleAlwaysOn
        ELSE
          Furnace(FurnaceNum)%SchedPtr        = GetScheduleIndex(Alphas(2))
          IF (Furnace(FurnaceNum)%SchedPtr == 0) THEN
            CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
            CALL ShowContinueError('Illegal '//TRIM(cAlphaFields(2))//' = '//TRIM(Alphas(2)))
            ErrorsFound=.TRUE.
          ENDIF
        ENDIF


        Furnace(FurnaceNum)%FurnaceInletNodeNum = &
               GetOnlySingleNode(Alphas(3),ErrorsFound,TRIM(CurrentModuleObject),Alphas(1), &
                            NodeType_Air,NodeConnectionType_Inlet,1,ObjectIsParent)
        Furnace(FurnaceNum)%FurnaceOutletNodeNum = &
               GetOnlySingleNode(Alphas(4),ErrorsFound,TRIM(CurrentModuleObject),Alphas(1), &
                            NodeType_Air,NodeConnectionType_Outlet,1,ObjectIsParent)


        CALL TestCompSet(CurrentModuleObject,Alphas(1),Alphas(3),Alphas(4),'Air Nodes')


        Furnace(FurnaceNum)%FanSchedPtr     = GetScheduleIndex(Alphas(5))
        IF (.NOT. lAlphaBlanks(5) .AND. Furnace(FurnaceNum)%FanSchedPtr == 0) THEN
          CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
          CALL ShowContinueError('Illegal '//TRIM(cAlphaFields(5))//' = '//TRIM(Alphas(5)))
          ErrorsFound=.TRUE.
        ELSEIF (lAlphaBlanks(5)) THEN
          Furnace(FurnaceNum)%OpMode = CycFanCycCoil
        ENDIF

         !Get the Controlling Zone or Location of the Furnace Thermostat
        Furnace(FurnaceNum)%ControlZoneNum = FindItemInList(Alphas(6),Zone%Name,NumOfZones)
        IF (Furnace(FurnaceNum)%ControlZoneNum == 0) THEN
          CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
          CALL ShowContinueError('Illegal '//TRIM(cAlphaFields(6))//' = '//TRIM(Alphas(6)))
          ErrorsFound=.TRUE.
        ENDIF

        TotalZonesOnAirLoop = 0

        ! Get the node number for the zone with the thermostat
        IF (Furnace(FurnaceNum)%ControlZoneNum >  0) THEN
          AirNodeFound=.FALSE.
          AirLoopFound=.FALSE.
          DO ControlledZoneNum = 1,NumOfZones
            IF (ZoneEquipConfig(ControlledZoneNum)%ActualZoneNum /= Furnace(FurnaceNum)%ControlZoneNum) CYCLE
!             Find the controlled zone number for the specified thermostat location
              Furnace(FurnaceNum)%NodeNumofControlledZone=ZoneEquipConfig(ControlledZoneNum)%ZoneNode
!             Determine if furnace is on air loop served by the thermostat location specified
              AirLoopNumber = ZoneEquipConfig(ControlledZoneNum)%AirLoopNum
              IF(AirLoopNumber .GT. 0)THEN
                DO BranchNum = 1, PrimaryAirSystem(AirLoopNumber)%NumBranches
                  DO CompNum = 1, PrimaryAirSystem(AirLoopNumber)%Branch(BranchNum)%TotalComponents
                    IF(.NOT. SameString(PrimaryAirSystem(AirLoopNumber)%Branch(BranchNum)%Comp(CompNum)%Name, &
                             Furnace(FurnaceNum)%Name) .OR. &
                       .NOT. SameString(PrimaryAirSystem(AirLoopNumber)%Branch(BranchNum)%Comp(CompNum)%TypeOf, &
                             CurrentModuleObject))CYCLE
                    AirLoopFound=.TRUE.
                    EXIT
                  END DO
                  IF(AirLoopFound)EXIT
                END DO
                DO TstatZoneNum = 1, NumTempControlledZones
                  IF(TempControlledZone(TstatZoneNum)%ActualZoneNum .NE. Furnace(FurnaceNum)%ControlZoneNum)CYCLE
                  AirNodeFound=.TRUE.
                END DO
                DO TstatZoneNum = 1, NumComfortControlledZones
                  IF(ComfortControlledZone(TstatZoneNum)%ActualZoneNum .NE. Furnace(FurnaceNum)%ControlZoneNum)CYCLE
                  AirNodeFound=.TRUE.
                END DO
              ELSE
                CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
                CALL ShowContinueError('Did not find an AirLoopHVAC.')
                CALL ShowContinueError('Specified '//TRIM(cAlphaFields(6))//' = '//TRIM(Alphas(6)))
                ErrorsFound=.TRUE.
              END IF
            EXIT
          ENDDO
          IF (.not. AirNodeFound) THEN
            CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
            CALL ShowSevereError('Did not find Air Node (Zone with Thermostat).')
            CALL ShowContinueError('Specified '//TRIM(cAlphaFields(6))//' = '//TRIM(Alphas(6)))
            CALL ShowContinueError('Both a ZoneHVAC:EquipmentConnections object and a ZoneControl:Thermostat object' &
                                //' must be specified for this zone.')
            ErrorsFound=.TRUE.
          ENDIF
          IF (.not. AirLoopFound) THEN
            CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
            CALL ShowSevereError('Did not find correct Primary Air Loop.')
            CALL ShowContinueError('Specified '//TRIM(cAlphaFields(6))//' = '//TRIM(Alphas(6))//&
                                   ' is not served by this AirLoopHVAC equipment.')
            ErrorsFound=.TRUE.
          ENDIF
          IF(AirLoopNumber .GT. 0)THEN
            DO ControlledZoneNum = 1,NumOfZones
              IF (ZoneEquipConfig(ControlledZoneNum)%AirLoopNum == AirLoopNumber) THEN
                TotalZonesOnAirLoop = TotalZonesOnAirLoop + 1
              END IF
            END DO
          END IF
        ENDIF


         !Get fan data
        FanType = Alphas(7)
        FanName = Alphas(8)
        ErrFlag=.FALSE.
        CALL GetFanType(FanName, Furnace(FurnaceNum)%FanType_Num, ErrFlag,  &
           CurrentModuleObject,Alphas(1))
        IF (ErrFlag) THEN
          ErrorsFound=.TRUE.
        END IF
        IF (Furnace(FurnaceNum)%FanType_Num == FanType_SimpleOnOff .OR. &
            Furnace(FurnaceNum)%FanType_Num == FanType_SimpleConstVolume)THEN

          CALL ValidateComponent(FanType,FanName,IsNotOK,TRIM(CurrentModuleObject))
          IF (IsNotOK) THEN
            CALL ShowContinueError('...occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
            ErrorsFound=.TRUE.

          ELSE ! mine data from fan object

            ! Get the fan index
            ErrFlag=.FALSE.
            CALL GetFanIndex(FanName, Furnace(FurnaceNum)%FanIndex, ErrFlag)
            IF (ErrFlag) THEN
              CALL ShowContinueError('...occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
              ErrorsFound=.TRUE.
            ENDIF

            !Set the Design Fan Volume Flow Rate
            ErrFlag=.FALSE.
            FanVolFlowRate = GetFanDesignVolumeFlowRate(FanType,FanName,ErrFlag)
            Furnace(FurnaceNum)%ActualFanVolFlowRate    = FanVolFlowRate

            IF (ErrFlag) THEN
              CALL ShowContinueError('...occurs in '//TRIM(CurrentModuleObject)//' ='//TRIM(Alphas(1)))
              ErrorsFound=.TRUE.
            ENDIF

            ! Get the Fan Inlet Node
            ErrFlag=.FALSE.
            FanInletNode = GetFanInletNode(FanType,FanName,ErrFlag)
            IF (ErrFlag) THEN
              CALL ShowContinueError('...occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
              ErrorsFound=.TRUE.
            ENDIF

            ! Get the Fan Outlet Node
            ErrFlag=.FALSE.
            FanOutletNode = GetFanOutletNode(FanType,FanName,ErrFlag)
            IF (ErrFlag) THEN
              CALL ShowContinueError('...occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
              ErrorsFound=.TRUE.
            ENDIF

            ! Get the fan's availabitlity schedule
            ErrFlag=.FALSE.
            Furnace(FurnaceNum)%FanAvailSchedPtr = GetFanAvailSchPtr(FanType,FanName,ErrFlag)
            IF (ErrFlag) THEN
              CALL ShowContinueError('...occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
              ErrorsFound=.TRUE.
            ENDIF

            ! Check fan's schedule for cycling fan operation if constant volume fan is used
            IF(Furnace(FurnaceNum)%FanSchedPtr .GT. 0 .AND. Furnace(FurnaceNum)%FanType_Num == FanType_SimpleConstVolume)THEN
              IF (.NOT. CheckScheduleValueMinMax(Furnace(FurnaceNum)%FanSchedPtr,'>',0.0d0,'<=',1.0d0)) THEN
                CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
                CALL ShowContinueError('For '//TRIM(cAlphaFields(7))//' = '//TRIM(Alphas(7)))
                CALL ShowContinueError('Fan operating mode must be continuous (fan operating mode schedule values > 0).')
                CALL ShowContinueError('Error found in '//TRIM(cAlphaFields(5))//' = '//TRIM(Alphas(5)))
                CALL ShowContinueError('...schedule values must be (>0., <=1.)')
                ErrorsFound=.TRUE.
              END IF
            ELSE IF(lAlphaBlanks(5) .AND. Furnace(FurnaceNum)%FanType_Num /= FanType_SimpleOnOff)THEN
              CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(Furnace(FurnaceNum)%Name))
              CALL ShowContinueError(TRIM(cAlphaFields(7))//' = '//TRIM(Alphas(7)))
              CALL ShowContinueError('Fan type must be Fan:OnOff when '//  &
                                   TRIM(cAlphaFields(5))//' = Blank.')
              ErrorsFound=.TRUE.
            END IF

          ENDIF ! IF (IsNotOK) THEN

        ELSE ! wrong fan type
          CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
          CALL ShowContinueError('Illegal '//TRIM(cAlphaFields(7))//' = '//TRIM(Alphas(7)))
          ErrorsFound=.TRUE.
        END IF ! IF (Furnace(FurnaceNum)%FanType_Num == FanType_SimpleOnOff .OR. &

        IF (SameString(Alphas(9),'BlowThrough') )      &
               Furnace(FurnaceNum)%FanPlace = BlowThru
        IF (SameString(Alphas(9),'DrawThrough') )      &
              Furnace(FurnaceNum)%FanPlace = DrawThru
        IF (Furnace(FurnaceNum)%FanPlace .EQ.0) THEN
          CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
          CALL ShowContinueError('Illegal '//TRIM(cAlphaFields(9))//' = '//TRIM(Alphas(9)))
          ErrorsFound = .TRUE.
        END IF

            !Get coil data
        HeatingCoilType = Alphas(10)
        HeatingCoilName = Alphas(11)
        Furnace(FurnaceNum)%HeatingCoilType = HeatingCoilType
        Furnace(FurnaceNum)%HeatingCoilName = HeatingCoilName
        IF (SameString(HeatingCoilType,'Coil:Heating:Gas')      .OR. &
            SameString(HeatingCoilType,'Coil:Heating:Electric')) THEN
            ErrFlag = .FALSE.
            Furnace(FurnaceNum)%HeatingCoilType_Num = GetHeatingCoilTypeNum(HeatingCoilType,HeatingCoilName,ErrFlag)
            IF (ErrFlag) THEN
              CALL ShowContinueError('...occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
              ErrorsFound=.TRUE.
            ELSE
              CALL ValidateComponent(HeatingCoilType,HeatingCoilName,IsNotOK,TRIM(CurrentModuleObject))
              IF (IsNotOK) THEN
                CALL ShowContinueError('...occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
                ErrorsFound=.TRUE.

              ELSE ! mine data from heating coil object

                ! Get index to Heating Coil
                ErrFlag=.FALSE.
                CALL GetHeatingCoilIndex(HeatingCoilName,Furnace(FurnaceNum)%HeatingCoilIndex,ErrFlag)
                IF (ErrFlag) THEN
                  CALL ShowContinueError('...occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
                  ErrorsFound=.TRUE.
                ENDIF

                ! Get the furnace design capacity
                ErrFlag=.FALSE.
                Furnace(FurnaceNum)%DesignHeatingCapacity  =   &
                   GetHeatingCoilCapacity(HeatingCoilType,HeatingCoilName,ErrFlag)
                IF (ErrFlag) THEN
                  CALL ShowContinueError('...occurs in '//TRIM(CurrentModuleObject)//' ='//TRIM(Alphas(1)))
                  ErrorsFound=.TRUE.
                ENDIF

                ! Get the Heating Coil Inlet Node
                ErrFlag=.FALSE.
                HeatingCoilInletNode = &
                   GetHeatingCoilInletNode(HeatingCoilType,HeatingCoilName,ErrFlag)
                Furnace(FurnaceNum)%HWCoilAirInletNode = HeatingCoilInletNode
                IF (ErrFlag) THEN
                  CALL ShowContinueError('...occurs in '//TRIM(CurrentModuleObject)//' ='//TRIM(Alphas(1)))
                  ErrorsFound=.TRUE.
                ENDIF

                ! Get the Heating Coil Outlet Node
                ErrFlag=.FALSE.
                HeatingCoilOutletNode = &
                   GetHeatingCoilOutletNode(HeatingCoilType,HeatingCoilName,ErrFlag)
                IF (ErrFlag) THEN
                  CALL ShowContinueError('...occurs in '//TRIM(CurrentModuleObject)//' ='//TRIM(Alphas(1)))
                  ErrorsFound=.TRUE.
                ENDIF

              ENDIF           ! IF (IsNotOK) THEN

            ENDIF

        ELSEIF (SameString(HeatingCoilType,'Coil:Heating:Water')) THEN
            Furnace(FurnaceNum)%HeatingCoilType_Num = Coil_HeatingWater
            CALL ValidateComponent(HeatingCoilType,HeatingCoilName,IsNotOK,TRIM(CurrentModuleObject))
            IF (IsNotOK) THEN
                CALL ShowContinueError('...occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
                ErrorsFound=.TRUE.
            ELSE ! mine data from heating coil object

                ! Get the Heating Coil water Inlet or control Node number
                ErrFlag = .FALSE.
                Furnace(FurnaceNum)%CoilControlNode = GetCoilWaterInletNode('Coil:Heating:Water',HeatingCoilName,ErrFlag)
                IF(ErrFlag)THEN
                  CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Furnace(FurnaceNum)%Name))
                  ErrorsFound = .TRUE.
                END IF

                ! Get the Heating Coil hot water max volume flow rate
                ErrFlag = .FALSE.
                Furnace(FurnaceNum)%MaxHeatCoilFluidFlow = GetCoilMaxWaterFlowRate('Coil:Heating:Water',  &
                                                           HeatingCoilName,ErrFlag)
                IF(ErrFlag)THEN
                  CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Furnace(FurnaceNum)%Name))
                  ErrorsFound = .TRUE.
                END IF

                ! Get the Heating Coil Inlet Node
                ErrFlag = .FALSE.
                HeatingCoilInletNode =  GetWaterCoilInletNode('Coil:Heating:Water',HeatingCoilName,ErrFlag)
                Furnace(FurnaceNum)%HWCoilAirInletNode = HeatingCoilInletNode
                IF(ErrFlag)THEN
                  CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Furnace(FurnaceNum)%Name))
                  ErrorsFound = .TRUE.
                END IF

                ! Get the Heating Coil Outlet Node
                ErrFlag = .FALSE.
                HeatingCoilOutletNode = GetWaterCoilOutletNode('Coil:Heating:Water',HeatingCoilName,ErrFlag)
                Furnace(FurnaceNum)%HWCoilAirOutletNode = HeatingCoilOutletNode
                IF(ErrFlag)THEN
                  CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Furnace(FurnaceNum)%Name))
                  ErrorsFound = .TRUE.
                END IF

                ! check if user has also used a water coil controller, which they should not do
                ErrFlag = .FALSE.
                CALL CheckCoilWaterInletNode(Furnace(FurnaceNum)%CoilControlNode, ErrFlag)
                IF (.NOT. ErrFlag) THEN ! then did find a controller so that is bad
                  CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(Furnace(FurnaceNum)%Name) &
                                      //' has a conflicting Controller:WaterCoil object' )
                  CALL ShowContinueError('Hot water coils are controlled directly by unitary and furnace systems.')
                  CALL ShowContinueError('No water coil controller should be input for the coil.')
                  ErrorsFound = .TRUE.
                ENDIF

            ENDIF

        ELSEIF (SameString(HeatingCoilType,'Coil:Heating:Steam')) THEN
            Furnace(FurnaceNum)%HeatingCoilType_Num = Coil_HeatingSteam
            CALL ValidateComponent(HeatingCoilType,HeatingCoilName,IsNotOK,TRIM(CurrentModuleObject))
            IF (IsNotOK) THEN
                CALL ShowContinueError('...occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
                ErrorsFound=.TRUE.
            ELSE ! mine data from heating coil object

                ErrFlag = .FALSE.
                Furnace(FurnaceNum)%HeatingCoilIndex = GetSTeamCoilIndex('COIL:HEATING:STEAM',HeatingCoilName,ErrFlag)
                IF (Furnace(FurnaceNum)%HeatingCoilIndex .EQ. 0) THEN
                    CALL ShowSevereError(TRIM(CurrentModuleObject)//' illegal '//TRIM(cAlphaFields(11))//' = ' &
                                  //TRIM(HeatingCoilName))
                    CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Furnace(FurnaceNum)%Name))
                    ErrorsFound = .TRUE.
                END IF

                ! Get the Heating Coil steam inlet node number
                ErrFlag = .FALSE.
                Furnace(FurnaceNum)%CoilControlNode = GetSteamCoilSteamInletNode('COIL:HEATING:STEAM',HeatingCoilName,ErrFlag)
                IF(ErrFlag)THEN
                  CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Furnace(FurnaceNum)%Name))
                  ErrorsFound = .TRUE.
                END IF

                ! Get the Heating Coil steam max volume flow rate
                Furnace(FurnaceNum)%MaxHeatCoilFluidFlow = GetCoilMaxSteamFlowRate(Furnace(FurnaceNum)%HeatingCoilIndex,ErrFlag)
                IF (Furnace(FurnaceNum)%MaxHeatCoilFluidFlow .GT. 0.0d0)THEN
                   SteamIndex = 0      ! Function GetSatDensityRefrig will look up steam index if 0 is passed
                   SteamDensity=GetSatDensityRefrig("STEAM",TempSteamIn,1.0d0,SteamIndex,'GetUnitaryHeatOnly')
                   Furnace(FurnaceNum)%MaxHeatCoilFluidFlow = Furnace(FurnaceNum)%MaxHeatCoilFluidFlow * SteamDensity
                END IF

                ! Get the Heating Coil Inlet Node
                ErrFlag = .FALSE.
                HeatingCoilInletNode = &
                         GetSteamCoilAirInletNode(Furnace(FurnaceNum)%HeatingCoilIndex,HeatingCoilName,ErrFlag)
                Furnace(FurnaceNum)%HWCoilAirInletNode = HeatingCoilInletNode
                IF(ErrFlag)THEN
                  CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Furnace(FurnaceNum)%Name))
                  ErrorsFound = .TRUE.
                END IF

                ! Get the Heating Coil Outlet Node
                ErrFlag = .FALSE.
                HeatingCoilOutletNode = &
                         GetSteamCoilAirOutletNode(Furnace(FurnaceNum)%HeatingCoilIndex,HeatingCoilName,ErrFlag)
                Furnace(FurnaceNum)%HWCoilAirOutletNode = HeatingCoilOutletNode
                IF(ErrFlag)THEN
                  CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Furnace(FurnaceNum)%Name))
                  ErrorsFound = .TRUE.
                END IF

            ENDIF

        ELSE
          CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
          CALL ShowContinueError('Illegal '//TRIM(cAlphaFields(11))//' = '//TRIM(Alphas(11)))
          ErrorsFound=.TRUE.
        END IF ! IF (Furnace(FurnaceNum)%HeatingCoilType_Num == Coil_HeatingGas .OR. &, etc.

        ! Add component sets array
        IF (Furnace(FurnaceNum)%FanPlace == BlowThru) THEN
          CompSetFanInlet = Alphas(3)
          CompSetFanOutlet = NodeID(FanOutletNode)
          CompSetHeatInlet = NodeID(FanOutletNode)
          CompSetHeatOutlet = Alphas(4)
          ! Fan inlet node name must not be the same as the furnace inlet node name
          IF (FanInletNode /= Furnace(FurnaceNum)%FurnaceInletNodeNum) THEN
            CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
            IF(FurnaceType_Num == Furnace_HeatOnly)THEN
              CALL ShowContinueError('When a blow through fan is specified, '//&
                                 'the fan inlet node name must be the same as the furnace inlet node name.')
              CALL ShowContinueError('...Fan inlet node name     = '//TRIM(NodeID(FanInletNode)))
              CALL ShowContinueError('...Furnace inlet node name = '//TRIM(NodeID(Furnace(FurnaceNum)%FurnaceInletNodeNum)))
            ELSE
              CALL ShowContinueError('When a blow through fan is specified, '//&
                                 'the fan inlet node name must be the same as the unitary system inlet node name.')
              CALL ShowContinueError('...Fan inlet node name            = '//TRIM(NodeID(FanInletNode)))
              CALL ShowContinueError('...Unitary System inlet node name = '//TRIM(NodeID(Furnace(FurnaceNum)%FurnaceInletNodeNum)))
            END IF
            ErrorsFound=.TRUE.
          END IF
          ! Fan outlet node name must be the same as the heating coil inlet node name
          IF (FanOutletNode /= HeatingCoilInletNode) THEN
            CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
            CALL ShowContinueError('When a blow through fan is specified, '//&
                                 'the fan outlet node name must be the same as the heating coil inlet node name.')
            CALL ShowContinueError('...Fan outlet node name         = '//TRIM(NodeID(FanOutletNode)))
            CALL ShowContinueError('...Heating coil inlet node name = '//TRIM(NodeID(HeatingCoilInletNode)))
            ErrorsFound=.TRUE.
          END IF
          ! Heating coil outlet node name must be the same as the furnace outlet node name
          IF (HeatingCoilOutletNode /= Furnace(FurnaceNum)%FurnaceOutletNodeNum) THEN
            CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
            IF(FurnaceType_Num == Furnace_HeatOnly)THEN
              CALL ShowContinueError('When a blow through fan is specified, '//&
                           'the heating coil outlet node name must be the same as the furnace outlet node name.')
              CALL ShowContinueError('...Heating coil outlet node name = '//TRIM(NodeID(HeatingCoilOutletNode)))
              CALL ShowContinueError('...Furnace outlet node name      = ' &
                                                                //TRIM(NodeID(Furnace(FurnaceNum)%FurnaceOutletNodeNum)))
            ELSE
              CALL ShowContinueError('When a blow through fan is specified, '//&
                           'the heating coil outlet node name must be the same as the unitary system outlet node name.')
              CALL ShowContinueError('...Heating coil outlet node name  = '//TRIM(NodeID(HeatingCoilOutletNode)))
              CALL ShowContinueError('...UnitarySystem outlet node name = ' &
                                                                //TRIM(NodeID(Furnace(FurnaceNum)%FurnaceOutletNodeNum)))
            END IF
            ErrorsFound=.TRUE.
          END IF
        ELSE ! draw through fan
          CompSetHeatInlet = Alphas(3)
          CompSetHeatOutlet = NodeID(FanInletNode)
          CompSetFanInlet = NodeID(FanInletNode)
          CompSetFanOutlet = Alphas(4)
          ! Heating coil inlet node name must not be the same as the furnace inlet node name
          IF (HeatingCoilInletNode /= Furnace(FurnaceNum)%FurnaceInletNodeNum) THEN
            CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
            IF(FurnaceType_Num == Furnace_HeatOnly)THEN
              CALL ShowContinueError('When a draw through fan is specified, '//&
                          'the heating coil inlet node name must be the same as the furnace inlet node name.')
              CALL ShowContinueError('...Heating coil inlet node name = '//TRIM(NodeID(HeatingCoilInletNode)))
              CALL ShowContinueError('...Furnace inlet node name      = ' &
                                                                 //TRIM(NodeID(Furnace(FurnaceNum)%FurnaceInletNodeNum)))
            ELSE
              CALL ShowContinueError('When a draw through fan is specified, '//&
                          'the heating coil inlet node name must be the same as the unitary system inlet node name.')
              CALL ShowContinueError('...Heating coil inlet node name  = '//TRIM(NodeID(HeatingCoilInletNode)))
              CALL ShowContinueError('...UnitarySystem inlet node name = ' &
                                                                 //TRIM(NodeID(Furnace(FurnaceNum)%FurnaceInletNodeNum)))
            END IF
            ErrorsFound=.TRUE.
          END IF
          ! Heating coil outlet node name must be the same as the fan inlet node name
          IF (HeatingCoilOutletNode /= FanInletNode) THEN
            CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
            CALL ShowContinueError('When a draw through fan is specified, '//&
                                 'the heating coil outlet node name must be the same as the fan inlet node name.')
            CALL ShowContinueError('...Heating coil outlet node name = '//TRIM(NodeID(HeatingCoilOutletNode)))
            CALL ShowContinueError('...Fan inlet node name           = '//TRIM(NodeID(FanInletNode)))
            ErrorsFound=.TRUE.
          END IF
          ! Fan coil outlet node name must be the same as the furnace outlet node name
          IF (FanOutletNode /= Furnace(FurnaceNum)%FurnaceOutletNodeNum) THEN
            CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
            IF(FurnaceType_Num == Furnace_HeatOnly)THEN
              CALL ShowContinueError('When a draw through fan is specified, '//&
                                 'the fan outlet node name must be the same as the furnace outlet node name.')
              CALL ShowContinueError('...Fan outlet node name     = '//TRIM(NodeID(FanOutletNode)))
              CALL ShowContinueError('...Furnace outlet node name = ' &
                                                          //TRIM(NodeID(Furnace(FurnaceNum)%FurnaceOutletNodeNum)))
            ELSE
              CALL ShowContinueError('When a draw through fan is specified, '//&
                                 'the fan outlet node name must be the same as the unitary system outlet node name.')
              CALL ShowContinueError('...Fan outlet node name           = '//TRIM(NodeID(FanOutletNode)))
              CALL ShowContinueError('...UnitarySystem outlet node name = ' &
                                                          //TRIM(NodeID(Furnace(FurnaceNum)%FurnaceOutletNodeNum)))
            END IF
            ErrorsFound=.TRUE.
          END IF
        ENDIF

        ! Add fan to component sets array
        CALL SetUpCompSets(CurrentModuleObject, Furnace(FurnaceNum)%Name, &
                           Alphas(7),Alphas(8),CompSetFanInlet,CompSetFanOutlet)
        ! Add heating coil to component sets array
        CALL SetUpCompSets(CurrentModuleObject, Furnace(FurnaceNum)%Name, &
                           Alphas(10),Alphas(11),CompSetHeatInlet,CompSetHeatOutlet)

        ! Set the furnace max outlet temperature
        Furnace(FurnaceNum)%DesignMaxOutletTemp = Numbers(1)

        ! Set the furnace design fan volumetric flow rate
        Furnace(FurnaceNum)%DesignFanVolFlowRate    = Numbers(2)

        ! Compare the flow rates.
        IF (FanVolFlowRate /= AutoSize .and. Furnace(FurnaceNum)%DesignFanVolFlowRate /= AutoSize) THEN
          IF (Furnace(FurnaceNum)%DesignFanVolFlowRate > FanVolFlowRate) THEN
            CALL ShowWarningError(TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
            CALL ShowContinueError('... The '//TRIM(cNumericFields(2))//' > Max Volume Flow Rate defined '// &
                                   'in the associated fan object, should be <=.')
            CALL ShowContinueError('... Entered value = '//TRIM(RoundSigDigits(Furnace(FurnaceNum)%DesignFanVolFlowRate,4))// &
                '... Fan ['//TRIM(FanType)//' = '//TRIM(FanName)//  &
                '] Max Value = '//TRIM(RoundSigDigits(FanVolFlowRate,4)))
            CALL ShowContinueError(' The HVAC system  flow rate is reset to the' &
                                 //' fan flow rate and the simulation continues.')
            Furnace(FurnaceNum)%DesignFanVolFlowRate = FanVolFlowRate
          ENDIF
        ENDIF
        IF (Furnace(FurnaceNum)%DesignFanVolFlowRate /= AutoSize) THEN
          IF (Furnace(FurnaceNum)%DesignFanVolFlowRate <= 0.0d0) THEN
            CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
            CALL ShowContinueError('... The '//TRIM(cNumericFields(2))//' <= 0.0, it must be > 0.0.')
            CALL ShowContinueError('... Entered value = '//TRIM(RoundSigDigits(Furnace(FurnaceNum)%DesignFanVolFlowRate,2)))
            ErrorsFound=.TRUE.
          ENDIF
        ENDIF

!       HeatOnly furnace has only 1 flow rate, initialize other variables used in this module
        Furnace(FurnaceNum)%MaxHeatAirVolFlow       = Furnace(FurnaceNum)%DesignFanVolFlowRate
        Furnace(FurnaceNum)%MaxCoolAirVolFlow       = Furnace(FurnaceNum)%DesignFanVolFlowRate
        Furnace(FurnaceNum)%MaxNoCoolHeatAirVolFlow = Furnace(FurnaceNum)%DesignFanVolFlowRate
        Furnace(FurnaceNum)%AirFlowControl          = UseCompressorOnFlow

        !Set heating convergence tolerance
        Furnace(FurnaceNum)%HeatingConvergenceTolerance = 0.001d0

       END DO  !End of the HeatOnly Furnace Loop



      ! Get the data for the HeatCool Furnace or UnitarySystem
      DO HeatCoolNum = 1,  NumHeatCool + NumUnitaryHeatCool

        FanInletNode          = 0
        FanOutletNode         = 0
        FanVolFlowRate        = 0.0d0
        CoolingCoilInletNode  = 0
        CoolingCoilOutletNode = 0
        HeatingCoilInletNode  = 0
        HeatingCoilOutletNode = 0
        ReHeatCoilInletNode   = 0
        ReHeatCoilOutletNode  = 0

!      Furnace and UnitarySystem objects are both read in here.
!      Will still have 2 differently named objects for the user, but read in with 1 DO loop.
        IF(HeatCoolNum .LE. NumHeatCool) THEN
          CurrentModuleObject  = 'AirLoopHVAC:Unitary:Furnace:HeatCool'
          FurnaceType_Num = Furnace_HeatCool
          GetObjectNum    = HeatCoolNum
        ELSE
          CurrentModuleObject  = 'AirLoopHVAC:UnitaryHeatCool'
          FurnaceType_Num = UnitarySys_HeatCool
          GetObjectNum    = HeatCoolNum - NumHeatCool
        END IF

        FurnaceNum  = HeatCoolNum + NumHeatOnly + NumUnitaryHeatOnly
        Furnace(FurnaceNum)%FurnaceType_Num = FurnaceType_Num

        CALL GetObjectItem(CurrentModuleObject,GetObjectNum,Alphas,NumAlphas,Numbers,NumNumbers,IOStatus, &
                           NumBlank=lNumericBlanks,AlphaBlank=lAlphaBlanks, &
                           AlphaFieldNames=cAlphaFields,NumericFieldNames=cNumericFields)

        IsNotOK=.FALSE.
        IsBlank=.FALSE.
        CALL VerifyName(Alphas(1),Furnace%Name,FurnaceNum-1,IsNotOK,IsBlank,TRIM(CurrentModuleObject)//' Name')
        IF (IsNotOK) THEN
          ErrorsFound=.TRUE.
          IF (IsBlank) Alphas(1)='xxxxx'
        ENDIF

        Furnace(FurnaceNum)%Name            = Alphas(1)
        IF (lAlphaBlanks(2)) THEN
          Furnace(FurnaceNum)%SchedPtr        = ScheduleAlwaysOn
        ELSE
          Furnace(FurnaceNum)%SchedPtr        = GetScheduleIndex(Alphas(2))
          IF (Furnace(FurnaceNum)%SchedPtr == 0) THEN
            CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
            CALL ShowContinueError('Illegal '//TRIM(cAlphaFields(2))//' = '//TRIM(Alphas(2)))
            ErrorsFound=.TRUE.
          ENDIF
        ENDIF

        Furnace(FurnaceNum)%FurnaceInletNodeNum = &
               GetOnlySingleNode(Alphas(3),ErrorsFound,CurrentModuleObject,Alphas(1), &
                            NodeType_Air,NodeConnectionType_Inlet,1,ObjectIsParent)
        Furnace(FurnaceNum)%FurnaceOutletNodeNum = &
               GetOnlySingleNode(Alphas(4),ErrorsFound,CurrentModuleObject,Alphas(1), &
                            NodeType_Air,NodeConnectionType_Outlet,1,ObjectIsParent)


        CALL TestCompSet(CurrentModuleObject,Alphas(1),Alphas(3),Alphas(4),'Air Nodes')


        Furnace(FurnaceNum)%FanSchedPtr     = GetScheduleIndex(Alphas(5))
        IF (.NOT. lAlphaBlanks(5) .AND. Furnace(FurnaceNum)%FanSchedPtr == 0) THEN
          CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
          CALL ShowContinueError('Illegal '//TRIM(cAlphaFields(5))//' = '//TRIM(Alphas(5)))
          ErrorsFound=.TRUE.
        ELSEIF (lAlphaBlanks(5)) THEN
          Furnace(FurnaceNum)%OpMode = CycFanCycCoil
        ENDIF

         !Get the Controlling Zone or Location of the Furnace Thermostat
        Furnace(FurnaceNum)%ControlZoneNum = FindItemInList(Alphas(6),Zone%Name,NumOfZones)
        IF (Furnace(FurnaceNum)%ControlZoneNum == 0) THEN
          CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
          CALL ShowContinueError('Illegal '//TRIM(cAlphaFields(6))//' = '//TRIM(Alphas(6)))
          ErrorsFound=.TRUE.
        ENDIF

        TotalZonesOnAirLoop = 0

        ! Get the node number for the zone with the thermostat
        IF (Furnace(FurnaceNum)%ControlZoneNum >  0) THEN
          AirNodeFound=.FALSE.
          AirLoopFound=.FALSE.
          DO ControlledZoneNum = 1,NumOfZones
            IF (ZoneEquipConfig(ControlledZoneNum)%ActualZoneNum /= Furnace(FurnaceNum)%ControlZoneNum) CYCLE
!             Find the controlled zone number for the specified thermostat location
              Furnace(FurnaceNum)%NodeNumofControlledZone=ZoneEquipConfig(ControlledZoneNum)%ZoneNode
!             Determine if system is on air loop served by the thermostat location specified
              AirLoopNumber = ZoneEquipConfig(ControlledZoneNum)%AirLoopNum
              IF(AirLoopNumber .GT. 0)THEN
                DO BranchNum = 1, PrimaryAirSystem(AirLoopNumber)%NumBranches
                  DO CompNum = 1, PrimaryAirSystem(AirLoopNumber)%Branch(BranchNum)%TotalComponents
                    IF(.NOT. SameString(PrimaryAirSystem(AirLoopNumber)%Branch(BranchNum)%Comp(CompNum)%Name, &
                             Alphas(1)) .OR. &
                       .NOT. SameString(PrimaryAirSystem(AirLoopNumber)%Branch(BranchNum)%Comp(CompNum)%TypeOf, &
                             CurrentModuleObject))CYCLE
                    AirLoopFound=.TRUE.
                    EXIT
                  END DO
                  IF(AirLoopFound)EXIT
                END DO
                DO TstatZoneNum = 1, NumTempControlledZones
                  IF(TempControlledZone(TstatZoneNum)%ActualZoneNum .NE. Furnace(FurnaceNum)%ControlZoneNum)CYCLE
                  AirNodeFound=.TRUE.
                END DO
                DO TstatZoneNum = 1, NumComfortControlledZones
                  IF(ComfortControlledZone(TstatZoneNum)%ActualZoneNum .NE. Furnace(FurnaceNum)%ControlZoneNum)CYCLE
                  AirNodeFound=.TRUE.
                END DO
              ELSE
                CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
                CALL ShowContinueError('Did not find an AirLoopHVAC.')
                CALL ShowContinueError('Specified '//TRIM(cAlphaFields(6))//' = '//TRIM(Alphas(6)))
                ErrorsFound=.TRUE.
              END IF
            EXIT
          ENDDO
          IF (.not. AirNodeFound) THEN
            CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
            CALL ShowContinueError('Did not find air node (zone with thermostat).')
            CALL ShowContinueError('Specified '//TRIM(cAlphaFields(6))//' = '//TRIM(Alphas(6)))
            CALL ShowContinueError('Both a ZoneHVAC:EquipmentConnections object and a ZoneControl:Thermostat object' &
                                //' must be specified for this zone.')
            ErrorsFound=.TRUE.
          ENDIF
          IF (.not. AirLoopFound) THEN
            CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
            CALL ShowSevereError('Did not find correct AirLoopHVAC.')
            CALL ShowContinueError('Specified '//TRIM(cAlphaFields(6))//' = '//TRIM(Alphas(6)))
            ErrorsFound=.TRUE.
          ENDIF
          IF(AirLoopNumber .GT. 0)THEN
            DO ControlledZoneNum = 1,NumOfZones
              IF (ZoneEquipConfig(ControlledZoneNum)%AirLoopNum == AirLoopNumber) THEN
                TotalZonesOnAirLoop = TotalZonesOnAirLoop + 1
              END IF
            END DO
          END IF
        ENDIF

         !Get fan data
        FanType = Alphas(7)
        FanName = Alphas(8)

        ErrFlag=.FALSE.
        CALL GetFanType(FanName, Furnace(FurnaceNum)%FanType_Num, ErrFlag,   &
           CurrentModuleObject,Alphas(1))
        IF (ErrFlag) THEN
          ErrorsFound=.TRUE.
        END IF

        IF (Furnace(FurnaceNum)%FanType_Num == FanType_SimpleOnOff .OR. &
            Furnace(FurnaceNum)%FanType_Num == FanType_SimpleConstVolume)THEN
          CALL ValidateComponent(FanType,FanName,IsNotOK,TRIM(CurrentModuleObject))
          IF (IsNotOK) THEN
            CALL ShowContinueError('In Furnace='//TRIM(Alphas(1)))
            ErrorsFound=.TRUE.

          ELSE ! mine data from fan object

            ! Get the fan index
            ErrFlag=.FALSE.
            CALL GetFanIndex(FanName, Furnace(FurnaceNum)%FanIndex, ErrFlag)
            IF (ErrFlag) THEN
              CALL ShowContinueError('...occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
              ErrorsFound=.TRUE.
            ENDIF

            ! Get the Design Fan Volume Flow Rate
            ErrFlag=.FALSE.
            FanVolFlowRate = GetFanDesignVolumeFlowRate(FanType,FanName,ErrFlag)
            Furnace(FurnaceNum)%ActualFanVolFlowRate = FanVolFlowRate
            IF (ErrFlag) THEN
              CALL ShowContinueError('...occurs in '//TRIM(CurrentModuleObject)//' "'//TRIM(Alphas(1))//'"')
              ErrorsFound=.TRUE.
            ENDIF

            ! Get the Fan Inlet Node
            ErrFlag=.FALSE.
            FanInletNode = GetFanInletNode(FanType,FanName,ErrFlag)
            IF (ErrFlag) THEN
              CALL ShowContinueError('...occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
              ErrorsFound=.TRUE.
            ENDIF

            ! Get the Fan Outlet Node
            ErrFlag=.FALSE.
            FanOutletNode = GetFanOutletNode(FanType,FanName,ErrFlag)
            IF (ErrFlag) THEN
              CALL ShowContinueError('...occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
              ErrorsFound=.TRUE.
            ENDIF

            ! Get the fan's availability schedule
            ErrFlag=.FALSE.
            Furnace(FurnaceNum)%FanAvailSchedPtr = GetFanAvailSchPtr(FanType,FanName,ErrFlag)
            IF (ErrFlag) THEN
              CALL ShowContinueError('...occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
              ErrorsFound=.TRUE.
            ENDIF

            ! Check fan's schedule for cycling fan operation if constant volume fan is used
            IF(Furnace(FurnaceNum)%FanSchedPtr .GT. 0 .AND. Furnace(FurnaceNum)%FanType_Num == FanType_SimpleConstVolume)THEN
              IF (.NOT. CheckScheduleValueMinMax(Furnace(FurnaceNum)%FanSchedPtr,'>',0.0d0,'<=',1.0d0)) THEN
                CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
                CALL ShowContinueError('For '//TRIM(cAlphaFields(7))//' = '//TRIM(Alphas(7)))
                CALL ShowContinueError('Fan operating mode must be continuous (fan operating mode schedule values > 0).')
                CALL ShowContinueError('Error found in '//TRIM(cAlphaFields(5))//' = '//TRIM(Alphas(5)))
                CALL ShowContinueError('...schedule values must be (>0., <=1.)')
                ErrorsFound=.TRUE.
              END IF
            ELSE IF(lAlphaBlanks(5) .AND. Furnace(FurnaceNum)%FanType_Num /= FanType_SimpleOnOff)THEN
              CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(Furnace(FurnaceNum)%Name))
              CALL ShowContinueError(TRIM(cAlphaFields(7))//' = '//TRIM(Alphas(7)))
              CALL ShowContinueError('Fan type must be Fan:OnOff when '//  &
                                   TRIM(cAlphaFields(5))//' = Blank.')
              ErrorsFound=.TRUE.
            END IF

          ENDIF ! IF (IsNotOK) THEN

        ELSE
          CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
          CALL ShowContinueError('Illegal '//TRIM(cAlphaFields(7))//' = '//TRIM(Alphas(7)))
          ErrorsFound=.TRUE.
        END IF !  IF (TFurnace(FurnaceNum)%FanType_Num == FanType_SimpleOnOff .OR. &, etc.

        IF (SameString(Alphas(9),'BlowThrough') )      &
               Furnace(FurnaceNum)%FanPlace = BlowThru
        IF (SameString(Alphas(9),'DrawThrough') )      &
              Furnace(FurnaceNum)%FanPlace = DrawThru
        IF (Furnace(FurnaceNum)%FanPlace .EQ.0) THEN
          CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
          CALL ShowContinueError('Illegal '//TRIM(cAlphaFields(9))//' = '//TRIM(Alphas(9)))
          ErrorsFound = .TRUE.
        END IF

            !Get coil data
        HeatingCoilType = Alphas(10)
        HeatingCoilName = Alphas(11)
        HeatingCoilPLFCurveIndex = 0
        Furnace(FurnaceNum)%HeatingCoilType = HeatingCoilType
        Furnace(FurnaceNum)%HeatingCoilName = HeatingCoilName
        IF (SameString(HeatingCoilType,'Coil:Heating:Gas')      .OR. &
            SameString(HeatingCoilType,'Coil:Heating:Electric')) THEN
            ErrFlag = .FALSE.
            Furnace(FurnaceNum)%HeatingCoilType_Num = GetHeatingCoilTypeNum(HeatingCoilType,HeatingCoilName,ErrFlag)
            IF (ErrFlag) THEN
              CALL ShowContinueError('...occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
              ErrorsFound=.TRUE.
            ELSE

              CALL ValidateComponent(HeatingCoilType,HeatingCoilName,IsNotOK,  &
                                     TRIM(CurrentModuleObject))
              IF (IsNotOK) THEN
                CALL ShowContinueError('...occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
                ErrorsFound=.TRUE.

              ELSE ! mine data from heating coil

                ! Get heating coil index
                ErrFlag=.FALSE.
                CALL GetHeatingCoilIndex(HeatingCoilName,Furnace(FurnaceNum)%HeatingCoilIndex,ErrFlag)
                IF (ErrFlag) THEN
                  CALL ShowContinueError('...occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
                  ErrorsFound=.TRUE.
                ENDIF

                ! Get the design heating capacity
                ErrFlag=.FALSE.
                Furnace(FurnaceNum)%DesignHeatingCapacity =   &
                   GetHeatingCoilCapacity(HeatingCoilType,HeatingCoilName,ErrFlag)
                IF (ErrFlag) THEN
                    CALL ShowContinueError('...occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
                  ErrorsFound=.TRUE.
                ENDIF

                ! Get the Heating Coil Inlet Node
                ErrFlag=.FALSE.
                HeatingCoilInletNode = &
                       GetHeatingCoilInletNode(HeatingCoilType,HeatingCoilName,ErrFlag)
                IF (ErrFlag) THEN
                  CALL ShowContinueError('...occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
                  ErrorsFound=.TRUE.
                ENDIF

                ! Get the Heating Coil Outlet Node
                ErrFlag=.FALSE.
                HeatingCoilOutletNode = &
                       GetHeatingCoilOutletNode(HeatingCoilType,HeatingCoilName,ErrFlag)
                IF (ErrFlag) THEN
                  CALL ShowContinueError('...occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
                  ErrorsFound=.TRUE.
                ENDIF

                ! Get the Heating Coil PLF Curve Index
                ErrFlag=.FALSE.
                HeatingCoilPLFCurveIndex = &
                       GetHeatingCoilPLFCurveIndex(HeatingCoilType,HeatingCoilName,ErrFlag)
                IF (ErrFlag) THEN
                  CALL ShowContinueError('...occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
                  ErrorsFound=.TRUE.
                ENDIF

              ENDIF ! IF (IsNotOK) THEN

            ENDIF

        ELSEIF (SameString(HeatingCoilType,'Coil:Heating:Water')) THEN
            Furnace(FurnaceNum)%HeatingCoilType_Num = Coil_HeatingWater
            CALL ValidateComponent(HeatingCoilType,HeatingCoilName,IsNotOK,TRIM(CurrentModuleObject))
            IF (IsNotOK) THEN
                CALL ShowContinueError('...occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
                ErrorsFound=.TRUE.
            ELSE ! mine data from heating coil object

                ! Get the Heating Coil water Inlet or control Node number
                ErrFlag = .FALSE.
                Furnace(FurnaceNum)%CoilControlNode = GetCoilWaterInletNode('Coil:Heating:Water',HeatingCoilName,ErrFlag)
                IF(ErrFlag)THEN
                  CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Furnace(FurnaceNum)%Name))
                  ErrorsFound = .TRUE.
                END IF

                ! Get the Heating Coil hot water max volume flow rate
                ErrFlag = .FALSE.
                Furnace(FurnaceNum)%MaxHeatCoilFluidFlow = GetCoilMaxWaterFlowRate('Coil:Heating:Water',  &
                                                           HeatingCoilName,ErrFlag)
                IF(ErrFlag)THEN
                  CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Furnace(FurnaceNum)%Name))
                  ErrorsFound = .TRUE.
                END IF

                ! Get the Heating Coil Inlet Node
                ErrFlag = .FALSE.
                HeatingCoilInletNode =  GetWaterCoilInletNode('Coil:Heating:Water',HeatingCoilName,ErrFlag)
                Furnace(FurnaceNum)%HWCoilAirInletNode = HeatingCoilInletNode
                IF(ErrFlag)THEN
                  CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Furnace(FurnaceNum)%Name))
                  ErrorsFound = .TRUE.
                END IF

                ! Get the Heating Coil Outlet Node
                ErrFlag = .FALSE.
                HeatingCoilOutletNode = GetWaterCoilOutletNode('Coil:Heating:Water',HeatingCoilName,ErrFlag)
                Furnace(FurnaceNum)%HWCoilAirOutletNode = HeatingCoilOutletNode
                IF(ErrFlag)THEN
                  CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Furnace(FurnaceNum)%Name))
                  ErrorsFound = .TRUE.
                END IF

                ! check if user has also used a water coil controller, which they should not do
                ErrFlag = .FALSE.
                CALL CheckCoilWaterInletNode(Furnace(FurnaceNum)%CoilControlNode, ErrFlag)
                IF (.NOT. ErrFlag) THEN ! then did find a controller so that is bad
                  CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(Furnace(FurnaceNum)%Name) &
                                      //' has a conflicting Controller:WaterCoil object' )
                  CALL ShowContinueError('Hot water coils are controlled directly by unitary and furnace systems.')
                  CALL ShowContinueError('No water coil controller should be input for the coil.')
                  ErrorsFound = .TRUE.
                ENDIF
            ENDIF

        ELSEIF (SameString(HeatingCoilType,'Coil:Heating:Steam')) THEN
            Furnace(FurnaceNum)%HeatingCoilType_Num = Coil_HeatingSteam
            CALL ValidateComponent(HeatingCoilType,HeatingCoilName,IsNotOK,TRIM(CurrentModuleObject))
            IF (IsNotOK) THEN
                CALL ShowContinueError('...occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
                ErrorsFound=.TRUE.
            ELSE ! mine data from heating coil object

                ErrFlag = .FALSE.
                Furnace(FurnaceNum)%HeatingCoilIndex = GetSTeamCoilIndex('COIL:HEATING:STEAM',HeatingCoilName,ErrFlag)
                IF (Furnace(FurnaceNum)%HeatingCoilIndex .EQ. 0) THEN
                    CALL ShowSevereError(TRIM(CurrentModuleObject)//' illegal '//TRIM(cAlphaFields(11))//' = ' &
                                  //TRIM(HeatingCoilName))
                    CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Furnace(FurnaceNum)%Name))
                    ErrorsFound = .TRUE.
                END IF

                ! Get the Heating Coil steam inlet node number
                ErrFlag = .FALSE.
                Furnace(FurnaceNum)%CoilControlNode = GetSteamCoilSteamInletNode('Coil:Heating:Steam',HeatingCoilName,ErrFlag)
                IF(ErrFlag)THEN
                  CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Furnace(FurnaceNum)%Name))
                  ErrorsFound = .TRUE.
                END IF

                ! Get the Heating Coil steam max volume flow rate
                Furnace(FurnaceNum)%MaxHeatCoilFluidFlow = GetCoilMaxSteamFlowRate(Furnace(FurnaceNum)%HeatingCoilIndex,ErrFlag)
                IF (Furnace(FurnaceNum)%MaxHeatCoilFluidFlow .GT. 0.0d0)THEN
                   SteamIndex = 0      ! Function GetSatDensityRefrig will look up steam index if 0 is passed
                   SteamDensity=GetSatDensityRefrig("STEAM",TempSteamIn,1.0d0,SteamIndex,'GetAirLoopHVACHeatCoolInput')
                   Furnace(FurnaceNum)%MaxHeatCoilFluidFlow = Furnace(FurnaceNum)%MaxHeatCoilFluidFlow * SteamDensity
                END IF

                ! Get the Heating Coil Inlet Node
                ErrFlag = .FALSE.
                HeatingCoilInletNode = &
                         GetSteamCoilAirInletNode(Furnace(FurnaceNum)%HeatingCoilIndex,HeatingCoilName,ErrFlag)
                Furnace(FurnaceNum)%HWCoilAirInletNode = HeatingCoilInletNode
                IF(ErrFlag)THEN
                  CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Furnace(FurnaceNum)%Name))
                  ErrorsFound = .TRUE.
                END IF

                ! Get the Heating Coil Outlet Node
                ErrFlag = .FALSE.
                HeatingCoilOutletNode = &
                         GetSteamCoilAirOutletNode(Furnace(FurnaceNum)%HeatingCoilIndex,HeatingCoilName,ErrFlag)
                Furnace(FurnaceNum)%HWCoilAirOutletNode = HeatingCoilOutletNode
                IF(ErrFlag)THEN
                  CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Furnace(FurnaceNum)%Name))
                  ErrorsFound = .TRUE.
                END IF

            ENDIF

        ELSE
          CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
          CALL ShowContinueError('Illegal '//TRIM(cAlphaFields(11))//' = '//TRIM(Alphas(11)))
          ErrorsFound=.TRUE.
        END IF ! IF (Furnace(FurnaceNum)%HeatingCoilType_Num == Coil_HeatingGas .OR. &, etc.


        ! Get Cooling Coil Information if available
        CoolingCoilType = Alphas(12)
        CoolingCoilName = Alphas(13)
!       Find the type of coil. Do not print message since this may not be the correct coil type.
        ErrFlag = .FALSE.
        PrintMessage = .FALSE.

        IF (SameString(CoolingCoilType, 'COIL:COOLING:DX:VARIABLESPEED') )THEN
          Furnace(FurnaceNum)%CoolingCoilType_Num = Coil_CoolingAirToAirVariableSpeed
        ELSE
          Furnace(FurnaceNum)%CoolingCoilType_Num = &
                 GetDXCoilTypeNum(CoolingCoilType,CoolingCoilName,ErrFlag,PrintMessage)
        END IF


        ! If coil type not found, check to see if a HX assisted cooling coil is used.
        IF(Furnace(FurnaceNum)%CoolingCoilType_Num .EQ. 0)THEN
          ErrFlag = .FALSE.
          Furnace(FurnaceNum)%CoolingCoilType_Num = &
                 GetHXAssistedCoilTypeNum(CoolingCoilType,CoolingCoilName,ErrFlag,PrintMessage)
        END IF

        IF (Furnace(FurnaceNum)%CoolingCoilType_Num == CoilDX_CoolingSingleSpeed)THEN
          CALL ValidateComponent(CoolingCoilType,CoolingCoilName,IsNotOK,  &
                                 TRIM(CurrentModuleObject))
          IF (IsNotOK) THEN
            CALL ShowContinueError('...occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
            ErrorsFound=.TRUE.

          ELSE ! mine data from DX cooling coil

            ! Get DX cooling coil index
            CALL GetDXCoilIndex(CoolingCoilName,Furnace(FurnaceNum)%CoolingCoilIndex,IsNotOK)
            IF(IsNotOK)THEN
              CALL ShowContinueError('...occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
              ErrorsFound=.TRUE.
            END IF

            ! Get DX cooling coil capacity
            ErrFlag = .FALSE.
            Furnace(FurnaceNum)%DesignCoolingCapacity =    &
               GetDXCoilCapacity(CoolingCoilType,CoolingCoilName,ErrFlag)
            IF (ErrFlag) THEN
              CALL ShowContinueError('...occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
              ErrorsFound=.TRUE.
            ENDIF

            ! Get the Cooling Coil Nodes
            ErrFlag=.FALSE.
            CoolingCoilInletNode = GetDXCoilInletNode(CoolingCoilType,CoolingCoilName,ErrFlag)
            CoolingCoilOutletNode = GetDXCoilOutletNode(CoolingCoilType,CoolingCoilName,ErrFlag)
            IF (ErrFlag) THEN
              CALL ShowContinueError('...occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
              ErrorsFound=.TRUE.
            ENDIF

            ! Get outdoor condenser node from DX coil object
            ErrFlag=.FALSE.
            IF (Furnace(FurnaceNum)%CoolingCoilType_Num == Coil_CoolingAirToAirVariableSpeed) THEN
                Furnace(FurnaceNum)%CondenserNodeNum = GetVSCoilCondenserInletNode(CoolingCoilName,ErrFlag)
            ELSE
                Furnace(FurnaceNum)%CondenserNodeNum = &
                    GetDXCoilCondenserInletNode(CoolingCoilType,CoolingCoilName,ErrFlag)
            END IF
            IF (ErrFlag) THEN
              CALL ShowContinueError('...occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
              ErrorsFound=.TRUE.
            ENDIF

          ENDIF ! IF (IsNotOK) THEN

          ! Push heating coil PLF curve index to DX coil
          IF(HeatingCoilPLFCurveIndex .GT. 0)THEN
            CALL SetDXCoolingCoilData(Furnace(FurnaceNum)%CoolingCoilIndex,ErrorsFound, &
                                      HeatingCoilPLFCurvePTR=HeatingCoilPLFCurveIndex)
          END IF

        ELSEIF (Furnace(FurnaceNum)%CoolingCoilType_Num == CoilDX_CoolingHXAssisted)THEN
          CALL ValidateComponent(CoolingCoilType,CoolingCoilName,IsNotOK,  &
                                 TRIM(CurrentModuleObject))
          IF (IsNotOK) THEN
            CALL ShowContinueError('...occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
            ErrorsFound=.TRUE.

          ELSE ! mine data from heat exchanger assisted cooling coil

            ! Get DX heat exchanger assisted cooling coil index
            CALL GetHXDXCoilIndex(CoolingCoilName,Furnace(FurnaceNum)%CoolingCoilIndex,IsNotOK)
            IF(IsNotOK)THEN
              CALL ShowContinueError('...occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
              ErrorsFound=.TRUE.
            END IF

            ! Get DX cooling coil capacity
            Furnace(FurnaceNum)%DesignCoolingCapacity =    &
               GetDXHXAsstdCoilCapacity(CoolingCoilType,CoolingCoilName,ErrFlag)
            ErrFlag = .FALSE.
            IF (ErrFlag) THEN
              CALL ShowContinueError('...occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
              ErrorsFound=.TRUE.
            ENDIF

            ! Get the Cooling Coil Nodes
            ErrFlag=.FALSE.
            CoolingCoilInletNode = GetDXHXAsstdCoilInletNode(CoolingCoilType,CoolingCoilName,ErrFlag)
            CoolingCoilOutletNode = GetDXHXAsstdCoilOutletNode(CoolingCoilType,CoolingCoilName,ErrFlag)
            IF (ErrFlag) THEN
              CALL ShowContinueError('...occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
              ErrorsFound=.TRUE.
            ENDIF

            ! Get outdoor condenser node from heat exchanger assisted DX coil object
            ErrFlag=.FALSE.
           IF (Furnace(FurnaceNum)%CoolingCoilType_Num == Coil_CoolingAirToAirVariableSpeed) THEN
                Furnace(FurnaceNum)%CondenserNodeNum = GetVSCoilCondenserInletNode(CoolingCoilName,ErrFlag)
           ELSE
            Furnace(FurnaceNum)%CondenserNodeNum = &
                GetDXCoilCondenserInletNode('COIL:COOLING:DX:SINGLESPEED', &
                GetHXDXCoilName(CoolingCoilType,CoolingCoilName,ErrFlag), ErrFlag)
           END IF

            IF (ErrFlag) THEN
              CALL ShowContinueError('...occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
              ErrorsFound=.TRUE.
            ENDIF

            ! Push heating coil PLF curve index to DX coil
            IF(HeatingCoilPLFCurveIndex .GT. 0)THEN
              ! get the actual index to the DX cooling coil object
              DXCoilIndex = GetActualDXCoilIndex(CoolingCoilType,CoolingCoilName,ErrorsFound)
              Furnace(FurnaceNum)%ActualDXCoilIndexforHXAssisted = DXCoilIndex
              CALL SetDXCoolingCoilData(DXCoilIndex,ErrorsFound, &
                                        HeatingCoilPLFCurvePTR=HeatingCoilPLFCurveIndex)
            END IF

          ENDIF ! IF (IsNotOK) THEN
        ELSE IF (Furnace(FurnaceNum)%CoolingCoilType_Num == Coil_CoolingAirToAirVariableSpeed )THEN
        ! BOS ADDED, AUG/2012, VARIIABLE SPEED DX COOLING COIL
        !  Furnace(FurnaceNum)%DXCoolCoilType = 'COIL:COOLING:DX:VARIABLESPEED'
        !  Furnace(FurnaceNum)%DXCoolCoilName = CoolingCoilName
          CALL ValidateComponent(CoolingCoilType,CoolingCoilName,IsNotOK,  &
                                 TRIM(CurrentModuleObject))
          IF (IsNotOK) THEN
            CALL ShowContinueError('...specified in '//TRIM(CurrentModuleObject)//'="'//TRIM(Alphas(1))//'".')
            ErrorsFound=.TRUE.
          ELSE
            ErrFlag = .FALSE.
            Furnace(FurnaceNum)%CoolingCoilIndex = GetCoilIndexVariableSpeed(CoolingCoilType, &
                        CoolingCoilName,ErrFlag)
            IF(ErrFlag)THEN
              CALL ShowContinueError('...specified in '//TRIM(CurrentModuleObject)//'="'//TRIM(Alphas(1))//'".')
              ErrorsFound=.TRUE.
            END IF
            CoolingCoilInletNode =GetCoilInletNodeVariableSpeed(CoolingCoilType,CoolingCoilName,ErrFlag)
            CoolingCoilOutletNode =GetCoilOutletNodeVariableSpeed(CoolingCoilType,CoolingCoilName,ErrFlag)
            Furnace(FurnaceNum)%CondenserNodeNum = GetVSCoilCondenserInletNode(CoolingCoilName,ErrFlag)

            IF (ErrFlag) THEN
              CALL ShowContinueError('...occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
              ErrorsFound=.TRUE.
            ENDIF
          ENDIF
        ELSE
          CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
          CALL ShowContinueError('Illegal '//TRIM(cAlphaFields(12))//' = '//TRIM(Alphas(12)))
          ErrorsFound=.TRUE.
        END IF

        IF (SameString(Alphas(14),'None') .OR. SameString(Alphas(14),'Multimode') .OR. &
            SameString(Alphas(14),'CoolReheat'))THEN
          AirNodeFound=.FALSE.
          If(SameString(Alphas(14),'Multimode'))THEN
            Furnace(FurnaceNum)%DehumidControlType_Num = DehumidControl_Multimode
            Furnace(FurnaceNum)%Humidistat = .TRUE.
            IF(Furnace(FurnaceNum)%CoolingCoilType_Num /= CoilDX_CoolingHXAssisted) THEN
              CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
              CALL ShowContinueError('Illegal '//TRIM(cAlphaFields(14))//' = '//TRIM(Alphas(14)))
              CALL ShowContinueError('Multimode control must be used with a Heat Exchanger Assisted Cooling Coil.')
              IF(lAlphaBlanks(15))THEN
              CALL ShowContinueError('Dehumidification control type is assumed to be None since a reheat coil has not been '// &
                                     'specified and the simulation continues.')
                Furnace(FurnaceNum)%Humidistat = .FALSE.
                Furnace(FurnaceNum)%DehumidControlType_Num = DehumidControl_None
              ELSE
                CALL ShowContinueError('Dehumidification control type is assumed to be CoolReheat and the simulation continues.')
                Furnace(FurnaceNum)%DehumidControlType_Num = DehumidControl_CoolReheat
              END IF
            END IF
          END IF
          IF(SameString(Alphas(14),'CoolReheat'))THEN
            Furnace(FurnaceNum)%DehumidControlType_Num = DehumidControl_CoolReheat
            Furnace(FurnaceNum)%Humidistat = .TRUE.
            IF(lAlphaBlanks(15))THEN
              CALL ShowWarningError(TRIM(CurrentModuleObject)//' "'//TRIM(Alphas(1))//'"')
              CALL ShowContinueError('Dehumidification control type is assumed to be None since a reheat coil has not been '// &
                                     'specified and the simulation continues.')
              Furnace(FurnaceNum)%Humidistat = .FALSE.
              Furnace(FurnaceNum)%DehumidControlType_Num = DehumidControl_None
            END IF
          END IF
          IF(SameString(Alphas(14),'None'))THEN
            Furnace(FurnaceNum)%DehumidControlType_Num = DehumidControl_None
            Furnace(FurnaceNum)%Humidistat = .FALSE.
          END IF
          IF(Furnace(FurnaceNum)%Humidistat)THEN
            DO HstatZoneNum = 1, NumHumidityControlZones
              IF(HumidityControlZone(HstatZoneNum)%ActualZoneNum .NE. Furnace(FurnaceNum)%ControlZoneNum)CYCLE
              AirNodeFound=.TRUE.
            END DO
            IF (.not. AirNodeFound) THEN
              CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
              CALL ShowContinueError('Did not find Air Node (Zone with Humidistat).')
              CALL ShowContinueError('Specified '//TRIM(cAlphaFields(6))//' = '//TRIM(Alphas(6)))
              ErrorsFound=.TRUE.
            ENDIF
          END IF
        ELSE ! invalid input
          CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
          CALL ShowContinueError('Illegal '//TRIM(cAlphaFields(14))//' = '//TRIM(Alphas(14)))
          Furnace(FurnaceNum)%Humidistat = .FALSE.
          ErrorsFound=.TRUE.
        END IF

!       Check placement of cooling coil with respect to fan placement and dehumidification control type
        IF(Furnace(FurnaceNum)%FanPlace == BlowThru)THEN
          IF(FanOutletNode == HeatingCoilInletNode .AND. &
             Furnace(FurnaceNum)%DehumidControlType_Num /= DehumidControl_CoolReheat)THEN
            Furnace(FurnaceNum)%CoolingCoilUpstream = .FALSE.
          END IF
        ELSE
          IF(HeatingCoilOutletNode == CoolingCoilInletNode .AND. &
             Furnace(FurnaceNum)%DehumidControlType_Num /= DehumidControl_CoolReheat)THEN
            Furnace(FurnaceNum)%CoolingCoilUpstream = .FALSE.
          END IF
        END IF


        !Get reheat coil data if humidistat is used
        ReheatingCoilType = Alphas(15)
        ReheatingCoilName = Alphas(16)
        Furnace(FurnaceNum)%SuppHeatCoilType = ReheatingCoilType
        Furnace(FurnaceNum)%SuppHeatCoilName = ReheatingCoilName
        ErrFlag = .FALSE.
        IF(.NOT. lAlphaBlanks(15))THEN
          IF (SameString(ReheatingCoilType,'Coil:Heating:Gas')      .OR. &
              SameString(ReheatingCoilType,'Coil:Heating:Electric') .OR. &
              SameString(ReheatingCoilType,'Coil:Heating:Desuperheater')) THEN

            Furnace(FurnaceNum)%SuppHeatCoilType_Num = GetHeatingCoilTypeNum(ReheatingCoilType,ReheatingCoilName,ErrFlag)
            IF (ErrFlag) THEN
               CALL ShowContinueError('...occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
               ErrorsFound=.TRUE.
            ELSE

              CALL ValidateComponent(ReHeatingCoilType,ReHeatingCoilName,IsNotOK,  &
                                   TRIM(CurrentModuleObject))
              IF (IsNotOK) THEN
                CALL ShowContinueError('In '//TRIM(CurrentModuleObject)//' "'//TRIM(Alphas(1))//'"')
                ErrorsFound=.TRUE.

              ELSE ! mine data from reheat coil

                ! Get the heating coil index
                CALL GetHeatingCoilIndex(ReheatingCoilName,Furnace(FurnaceNum)%SuppHeatCoilIndex,IsNotOK)
                IF (IsNotOK) THEN
                  CALL ShowContinueError('...occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
                  ErrorsFound=.TRUE.
                ENDIF

                ! Get the design supplemental heating capacity
                ErrFlag=.FALSE.
                Furnace(FurnaceNum)%DesignSuppHeatingCapacity =   &
                    GetHeatingCoilCapacity(ReHeatingCoilType,ReHeatingCoilName,ErrFlag)
                IF (ErrFlag) THEN
                  CALL ShowContinueError('...occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
                  ErrorsFound=.TRUE.
                ENDIF

                ! Get the Reheat Coil Inlet Node
                ErrFlag=.FALSE.
                ReheatCoilInletNode = GetHeatingCoilInletNode(ReheatingCoilType,ReheatingCoilName,ErrFlag)
                IF (ErrFlag) THEN
                  CALL ShowContinueError('...occurs in '//TRIM(CurrentModuleObject)//' "'//TRIM(Alphas(1))//'"')
                  ErrorsFound=.TRUE.
                ENDIF

                ! Get the Reheat Coil Outlet Node
                ErrFlag=.FALSE.
                ReheatCoilOutletNode = GetHeatingCoilOutletNode(ReheatingCoilType,ReheatingCoilName,ErrFlag)
                IF (ErrFlag) THEN
                  CALL ShowContinueError('...occurs in '//TRIM(CurrentModuleObject)//' "'//TRIM(Alphas(1))//'"')
                  ErrorsFound=.TRUE.
                ENDIF

              ENDIF  ! IF (IsNotOK) THEN
            ENDIF

          ELSEIF (SameString(ReheatingCoilType,'Coil:Heating:Water')) THEN
            Furnace(FurnaceNum)%SuppHeatCoilType_Num = Coil_HeatingWater
            CALL ValidateComponent(ReheatingCoilType,ReheatingCoilName,IsNotOK,TRIM(CurrentModuleObject))
            IF (IsNotOK) THEN
                CALL ShowContinueError('...occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
                ErrorsFound=.TRUE.
            ELSE ! mine data from heating coil object

                ! Get the Heating Coil water Inlet or control Node number
                ErrFlag = .FALSE.
                Furnace(FurnaceNum)%SuppCoilControlNode = GetCoilWaterInletNode('Coil:Heating:Water',ReheatingCoilName,ErrFlag)
                IF(ErrFlag)THEN
                  CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Furnace(FurnaceNum)%Name))
                  ErrorsFound = .TRUE.
                END IF

                ! Get the ReHeat Coil hot water max volume flow rate
                ErrFlag = .FALSE.
                Furnace(FurnaceNum)%MaxSuppCoilFluidFlow = GetCoilMaxWaterFlowRate('Coil:Heating:Water',  &
                                                           ReheatingCoilName,ErrFlag)
                IF(ErrFlag)THEN
                  CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Furnace(FurnaceNum)%Name))
                  ErrorsFound = .TRUE.
                END IF

                ! Get the ReHeat Coil Inlet Node
                ErrFlag = .FALSE.
                ReheatCoilInletNode =  GetWaterCoilInletNode('Coil:Heating:Water',ReheatingCoilName,ErrFlag)
                Furnace(FurnaceNum)%SuppCoilAirInletNode = ReheatCoilInletNode
                IF(ErrFlag)THEN
                  CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Furnace(FurnaceNum)%Name))
                  ErrorsFound = .TRUE.
                END IF

                ! Get the ReHeat Coil Outlet Node
                ErrFlag = .FALSE.
                ReheatCoilOutletNode = GetWaterCoilOutletNode('Coil:Heating:Water',ReheatingCoilName,ErrFlag)
                Furnace(FurnaceNum)%SuppCoilAirOutletNode = ReheatCoilOutletNode
                IF(ErrFlag)THEN
                  CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Furnace(FurnaceNum)%Name))
                  ErrorsFound = .TRUE.
                END IF

                ! check if user has also used a water coil controller, which they should not do
                ErrFlag = .FALSE.
                CALL CheckCoilWaterInletNode(Furnace(FurnaceNum)%CoilControlNode, ErrFlag)
                IF (.NOT. ErrFlag) THEN ! then did find a controller so that is bad
                  CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(Furnace(FurnaceNum)%Name) &
                                      //' has a conflicting Controller:WaterCoil object' )
                  CALL ShowContinueError('Hot water coils are controlled directly by unitary and furnace systems.')
                  CALL ShowContinueError('No water coil controller should be input for the coil.')
                  ErrorsFound = .TRUE.
                ENDIF

            ENDIF

          ELSEIF (SameString(ReheatingCoilType,'Coil:Heating:Steam')) THEN
            Furnace(FurnaceNum)%SuppHeatCoilType_Num = Coil_HeatingSteam
            CALL ValidateComponent(ReheatingCoilType,ReheatingCoilName,IsNotOK,TRIM(CurrentModuleObject))
            IF (IsNotOK) THEN
                CALL ShowContinueError('...occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
                ErrorsFound=.TRUE.
            ELSE ! mine data from heating coil object

                ErrFlag = .FALSE.
                Furnace(FurnaceNum)%SuppHeatCoilIndex = GetSTeamCoilIndex('COIL:HEATING:STEAM',ReheatingCoilName,ErrFlag)
                IF (Furnace(FurnaceNum)%SuppHeatCoilIndex .EQ. 0) THEN
                    CALL ShowSevereError(TRIM(CurrentModuleObject)//' illegal '//TRIM(cAlphaFields(11))//' = ' &
                                  //TRIM(ReheatingCoilName))
                    CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Furnace(FurnaceNum)%Name))
                    ErrorsFound = .TRUE.
                END IF

                ! Get the Heating Coil steam inlet node number
                ErrFlag = .FALSE.
                Furnace(FurnaceNum)%SuppCoilControlNode = GetSteamCoilSteamInletNode('Coil:Heating:Steam',ReheatingCoilName,ErrFlag)
                IF(ErrFlag)THEN
                  CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Furnace(FurnaceNum)%Name))
                  ErrorsFound = .TRUE.
                END IF

                ! Get the Heating Coil steam max volume flow rate
                Furnace(FurnaceNum)%MaxSuppCoilFluidFlow = GetCoilMaxSteamFlowRate(Furnace(FurnaceNum)%SuppHeatCoilIndex,ErrFlag)
                IF (Furnace(FurnaceNum)%MaxSuppCoilFluidFlow .GT. 0.0d0)THEN
                   SteamIndex = 0      ! Function GetSatDensityRefrig will look up steam index if 0 is passed
                   SteamDensity=GetSatDensityRefrig("STEAM",TempSteamIn,1.0d0,SteamIndex,'GetAirLoopHVACHeatCoolInput')
                   Furnace(FurnaceNum)%MaxSuppCoilFluidFlow = &
                                     GetCoilMaxSteamFlowRate(Furnace(FurnaceNum)%SuppHeatCoilIndex,ErrFlag) * SteamDensity
                END IF

                ! Get the Heating Coil Inlet Node
                ErrFlag = .FALSE.
                ReheatCoilInletNode = &
                         GetSteamCoilAirInletNode(Furnace(FurnaceNum)%SuppHeatCoilIndex,ReheatingCoilName,ErrFlag)
                Furnace(FurnaceNum)%SuppCoilAirInletNode = ReheatCoilInletNode
                IF(ErrFlag)THEN
                  CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Furnace(FurnaceNum)%Name))
                  ErrorsFound = .TRUE.
                END IF

                ! Get the Heating Coil Outlet Node
                ErrFlag = .FALSE.
                ReheatCoilOutletNode = &
                         GetSteamCoilAirOutletNode(Furnace(FurnaceNum)%SuppHeatCoilIndex,ReheatingCoilName,ErrFlag)
                Furnace(FurnaceNum)%SuppCoilAirOutletNode = ReheatCoilOutletNode
                IF(ErrFlag)THEN
                  CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Furnace(FurnaceNum)%Name))
                  ErrorsFound = .TRUE.
                END IF

            ENDIF

          ELSE  ! Illeagal heating coil
            CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
            CALL ShowContinueError('Illegal '//TRIM(cAlphaFields(15))//' = '//TRIM(Alphas(15)))
            ErrorsFound=.TRUE.
          END IF ! IF (Furnace(FurnaceNum)%SuppHeatCoilType_Num == Coil_HeatingGas .OR. &, etc.

        END IF ! IF(.NOT. lAlphaBlanks(15))THEN

        IF(Furnace(FurnaceNum)%FanPlace .EQ. BlowThru)THEN

          IF(FanInletNode /= Furnace(FurnaceNum)%FurnaceInletNodeNum) THEN
             CALL ShowSevereError('For '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
             IF(FurnaceType_Num == Furnace_HeatCool)THEN
               CALL ShowContinueError('When a blow through fan is specified, the fan inlet node name must be '// &
                                      'the same as the furnace inlet node name.')
               CALL ShowContinueError('...Fan inlet node name     = '//TRIM(NodeID(FanInletNode)))
               CALL ShowContinueError('...Furnace inlet node name = ' &
                                                      //TRIM(NodeID(Furnace(FurnaceNum)%FurnaceInletNodeNum)))
             ELSE
               CALL ShowContinueError('When a blow through fan is specified, the fan inlet node name must be '// &
                                      'the same as the unitary system inlet node name.')
               CALL ShowContinueError('...Fan inlet node name           = '//TRIM(NodeID(FanInletNode)))
               CALL ShowContinueError('...UnitarySystem inlet node name = ' &
                                                      //TRIM(NodeID(Furnace(FurnaceNum)%FurnaceInletNodeNum)))
             END IF
             ErrorsFound=.TRUE.
          END IF
          IF(Furnace(FurnaceNum)%CoolingCoilUpstream)THEN
            IF(FanOutletNode /= CoolingCoilInletNode) THEN
               CALL ShowSevereError('For '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
               CALL ShowContinueError('When a blow through fan is specified, the fan outlet node name must be '// &
                                      'the same as the cooling coil inlet node name.')
               CALL ShowContinueError('...Fan outlet node name         = '//TRIM(NodeID(FanOutletNode)))
               CALL ShowContinueError('...Cooling coil inlet node name = '//TRIM(NodeID(CoolingCoilInletNode)))
               ErrorsFound=.TRUE.
            END IF
            IF(CoolingCoilOutletNode /= HeatingCoilInletNode) THEN
               CALL ShowSevereError('For '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
               CALL ShowContinueError('The cooling coil outlet node name must be '// &
                                      'the same as the heating coil inlet node name.')
               CALL ShowContinueError('...Cooling coil outlet node name = '//TRIM(NodeID(CoolingCoilOutletNode)))
               CALL ShowContinueError('...Heating coil inlet node name  = '//TRIM(NodeID(HeatingCoilInletNode)))
               ErrorsFound=.TRUE.
            END IF
            IF((Furnace(FurnaceNum)%Humidistat .AND. Furnace(FurnaceNum)%DehumidControlType_Num == DehumidControl_CoolReheat) .OR. &
               ReHeatCoilInletNode .GT. 0)THEN
              IF(HeatingCoilOutletNode /= ReHeatCoilInletNode) THEN
                CALL ShowSevereError('For '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
                CALL ShowContinueError('When a blow through fan is specified, the heating coil outlet node name must be '// &
                                       'the same as the reheat coil inlet node name.')
                CALL ShowContinueError('...Heating coil outlet node name = '//TRIM(NodeID(HeatingCoilOutletNode)))
                CALL ShowContinueError('...Reheat coil inlet node name   = '//TRIM(NodeID(ReHeatCoilInletNode)))
                ErrorsFound=.TRUE.
              END IF
              IF(ReHeatCoilOutletNode /= Furnace(FurnaceNum)%FurnaceOutletNodeNum) THEN
                CALL ShowSevereError('For '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
                IF(FurnaceType_Num == Furnace_HeatCool)THEN
                  CALL ShowContinueError('The reheat coil outlet node name must be '// &
                                         'the same as the furnace outlet node name.')
                  CALL ShowContinueError('...Reheat coil outlet node name = '//TRIM(NodeID(ReHeatCoilOutletNode)))
                  CALL ShowContinueError('...Furnace outlet node name     = ' &
                                        //TRIM(NodeID(Furnace(FurnaceNum)%FurnaceOutletNodeNum)))
                ELSE
                  CALL ShowContinueError('The reheat coil outlet node name must be '// &
                                         'the same as the unitary system outlet node name.')
                  CALL ShowContinueError('...Reheat coil outlet node name   = '//TRIM(NodeID(ReHeatCoilOutletNode)))
                  CALL ShowContinueError('...UnitarySystem outlet node name = ' &
                                        //TRIM(NodeID(Furnace(FurnaceNum)%FurnaceOutletNodeNum)))
                END IF
                ErrorsFound=.TRUE.
              END IF
            ELSE ! IF((Furnace(FurnaceNum)%Humidistat ...
              ! Heating coil outlet node name must be the same as the furnace outlet node name
              IF (HeatingCoilOutletNode /= Furnace(FurnaceNum)%FurnaceOutletNodeNum) THEN
                CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
                IF(FurnaceType_Num == Furnace_HeatOnly)THEN
                  CALL ShowContinueError('When a blow through fan is specified, '//&
                               'the heating coil outlet node name must be the same as the furnace outlet node name.')
                  CALL ShowContinueError('...Heating coil outlet node name = '//TRIM(NodeID(HeatingCoilOutletNode)))
                  CALL ShowContinueError('...Furnace outlet node name      = ' &
                                                                    //TRIM(NodeID(Furnace(FurnaceNum)%FurnaceOutletNodeNum)))
                ELSE
                  CALL ShowContinueError('When a blow through fan is specified, '//&
                               'the heating coil outlet node name must be the same as the unitary system outlet node name.')
                  CALL ShowContinueError('...Heating coil outlet node name  = '//TRIM(NodeID(HeatingCoilOutletNode)))
                  CALL ShowContinueError('...UnitarySystem outlet node name = ' &
                                                                    //TRIM(NodeID(Furnace(FurnaceNum)%FurnaceOutletNodeNum)))
                END IF
                ErrorsFound=.TRUE.
              END IF
            END IF
          ELSE ! IF(Furnace(FurnaceNum)%CoolingCoilUpstream)THEN
            IF(FanOutletNode /= HeatingCoilInletNode) THEN
               CALL ShowSevereError('For '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
               CALL ShowContinueError('When a blow through fan is specified, the fan outlet node name must be '// &
                                      'the same as the heating coil inlet node name.')
               CALL ShowContinueError('...Fan outlet node name         = '//TRIM(NodeID(FanOutletNode)))
               CALL ShowContinueError('...Heating coil inlet node name = '//TRIM(NodeID(HeatingCoilInletNode)))
               ErrorsFound=.TRUE.
            END IF
            IF(HeatingCoilOutletNode /= CoolingCoilInletNode) THEN
               CALL ShowSevereError('For '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
               CALL ShowContinueError('The heating coil outlet node name must be '// &
                                      'the same as the cooling coil inlet node name.')
               CALL ShowContinueError('...Heating coil outlet node name = '//TRIM(NodeID(HeatingCoilOutletNode)))
               CALL ShowContinueError('...Cooling coil inlet node name  = '//TRIM(NodeID(CoolingCoilInletNode)))
               ErrorsFound=.TRUE.
            END IF
            IF(CoolingCoilOutletNode /= Furnace(FurnaceNum)%FurnaceOutletNodeNum) THEN
               CALL ShowSevereError('For '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
               IF(FurnaceType_Num == Furnace_HeatCool)THEN
                 CALL ShowContinueError('When a blow through fan is specified, the cooling coil outlet node name must be '// &
                                        'the same as the furnace outlet node name.')
                 CALL ShowContinueError('...Cooling coil outlet node name = '//TRIM(NodeID(CoolingCoilOutletNode)))
                 CALL ShowContinueError('...Furnace outlet node name      = ' &
                                                                  //TRIM(NodeID(Furnace(FurnaceNum)%FurnaceOutletNodeNum)))
               ELSE
                 CALL ShowContinueError('When a blow through fan is specified, the cooling coil outlet node name must be '// &
                                        'the same as the unitary system outlet node name.')
                 CALL ShowContinueError('...Cooling coil outlet node name   = '//TRIM(NodeID(CoolingCoilOutletNode)))
                 CALL ShowContinueError('...UnitarySystem outlet node name  = ' &
                                                                  //TRIM(NodeID(Furnace(FurnaceNum)%FurnaceOutletNodeNum)))
               END IF
               ErrorsFound=.TRUE.
            END IF
          END IF

        ELSE ! ELSE from IF(Furnace(FurnaceNum)%FanPlace .EQ. BlowThru)THEN

          IF(Furnace(FurnaceNum)%CoolingCoilUpstream)THEN
            IF(CoolingCoilInletNode /= Furnace(FurnaceNum)%FurnaceInletNodeNum) THEN
              CALL ShowSevereError('For '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
              IF(FurnaceType_Num == Furnace_HeatCool)THEN
                CALL ShowContinueError('When a draw through fan is specified, the cooling coil inlet node name must be '// &
                                       'the same as the furnace inlet node name.')
                CALL ShowContinueError('...Cooling coil inlet node name = '//TRIM(NodeID(CoolingCoilInletNode)))
                CALL ShowContinueError('...Furnace inlet node name      = ' &
                                                       //TRIM(NodeID(Furnace(FurnaceNum)%FurnaceInletNodeNum)))
              ELSE
                CALL ShowContinueError('When a draw through fan is specified, the cooling coil inlet node name must be '// &
                                       'the same as the unitary system inlet node name.')
                CALL ShowContinueError('...Cooling coil inlet node name  = '//TRIM(NodeID(CoolingCoilInletNode)))
                CALL ShowContinueError('...UnitarySystem inlet node name = ' &
                                                       //TRIM(NodeID(Furnace(FurnaceNum)%FurnaceInletNodeNum)))
              END IF
              ErrorsFound=.TRUE.
            END IF
            IF(CoolingCoilOutletNode /= HeatingCoilInletNode) THEN
              CALL ShowSevereError('For '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
              CALL ShowContinueError('The cooling coil outlet node name must be '// &
                                     'the same as the heating coil inlet node name.')
              CALL ShowContinueError('...Cooling coil outlet node name = '//TRIM(NodeID(CoolingCoilOutletNode)))
              CALL ShowContinueError('...Heating coil inlet node name  = '//TRIM(NodeID(HeatingCoilInletNode)))
              ErrorsFound=.TRUE.
            END IF
            IF(HeatingCoilOutletNode /= FanInletNode) THEN
              CALL ShowSevereError('For '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
              CALL ShowContinueError('When a draw through fan is specified, the heating coil outlet node name must be '// &
                                     'the same as the fan inlet node name.')
              CALL ShowContinueError('...Heating coil outlet node name = '//TRIM(NodeID(HeatingCoilOutletNode)))
              CALL ShowContinueError('...Fan inlet node name           = '//TRIM(NodeID(FanInletNode)))
              ErrorsFound=.TRUE.
            END IF
            IF((Furnace(FurnaceNum)%Humidistat .AND. Furnace(FurnaceNum)%DehumidControlType_Num == DehumidControl_CoolReheat) .OR. &
               ReHeatCoilInletNode .GT. 0)THEN
              IF(FanOutletNode /= ReHeatCoilInletNode) THEN
                CALL ShowSevereError('For '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
                CALL ShowContinueError('When a draw through fan is specified, the fan outlet node name must be '// &
                                       'the same as the reheat coil inlet node name.')
                CALL ShowContinueError('...Fan outlet node name        = '//TRIM(NodeID(FanOutletNode)))
                CALL ShowContinueError('...Reheat coil inlet node name = '//TRIM(NodeID(ReheatCoilInletNode)))
                ErrorsFound=.TRUE.
              END IF
              IF(ReHeatCoilOutletNode /= Furnace(FurnaceNum)%FurnaceOutletNodeNum) THEN
                CALL ShowSevereError('For '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
                IF(FurnaceType_Num == Furnace_HeatCool)THEN
                  CALL ShowContinueError('The reheat coil outlet node name must be '// &
                                         'the same as the furnace outlet node name.')
                  CALL ShowContinueError('...Reheat coil outlet node name = '//TRIM(NodeID(ReHeatCoilOutletNode)))
                  CALL ShowContinueError('...Furnace outlet node name     = ' &
                                                //TRIM(NodeID(Furnace(FurnaceNum)%FurnaceOutletNodeNum)))
                ELSE
                  CALL ShowContinueError('The reheat coil outlet node name must be '// &
                                         'the same as the unitary system outlet node name.')
                  CALL ShowContinueError('...Reheat coil outlet node name   = '//TRIM(NodeID(ReHeatCoilOutletNode)))
                  CALL ShowContinueError('...UnitarySystem outlet node name = ' &
                                                  //TRIM(NodeID(Furnace(FurnaceNum)%FurnaceOutletNodeNum)))
                END IF
                ErrorsFound=.TRUE.
              END IF
            ELSE
              IF(FanOutletNode /= Furnace(FurnaceNum)%FurnaceOutletNodeNum) THEN
                CALL ShowSevereError('For '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
                CALL ShowContinueError('When a draw through fan is specified, the fan outlet node name must be '// &
                                       'the same as the unitary system outlet node name.')
                CALL ShowContinueError('...Fan outlet node name        = '//TRIM(NodeID(FanOutletNode)))
                CALL ShowContinueError('...Unitary system outlet node name = '// &
                                       TRIM(NodeID(Furnace(FurnaceNum)%FurnaceOutletNodeNum)))
                ErrorsFound=.TRUE.
              END IF
            END IF
          ELSE ! IF(Furnace(FurnaceNum)%CoolingCoilUpstream)THEN
            IF(HeatingCoilInletNode /= Furnace(FurnaceNum)%FurnaceInletNodeNum) THEN
               CALL ShowSevereError('For '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
               IF(FurnaceType_Num == Furnace_HeatCool)THEN
                 CALL ShowContinueError('When a draw through fan is specified, the heating coil inlet node name must be '// &
                                        'the same as the furnace inlet node name.')
                 CALL ShowContinueError('...Heating coil inlet node name = '//TRIM(NodeID(HeatingCoilInletNode)))
                 CALL ShowContinueError('...Furnace inlet node name      = ' &
                                                                //TRIM(NodeID(Furnace(FurnaceNum)%FurnaceInletNodeNum)))
               ELSE
                 CALL ShowContinueError('When a draw through fan is specified, the heating coil inlet node name must be '// &
                                        'the same as the unitary system inlet node name.')
                 CALL ShowContinueError('...Heating coil inlet node name  = '//TRIM(NodeID(HeatingCoilInletNode)))
                 CALL ShowContinueError('...UnitarySystem inlet node name = ' &
                                                                //TRIM(NodeID(Furnace(FurnaceNum)%FurnaceInletNodeNum)))
               END IF
               ErrorsFound=.TRUE.
            END IF
            IF(HeatingCoilOutletNode /= CoolingCoilInletNode) THEN
               CALL ShowSevereError('For '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
               CALL ShowContinueError('The heating coil outlet node name must be '// &
                                      'the same as the cooling coil inlet node name.')
               CALL ShowContinueError('...Heating coil outlet node name = '//TRIM(NodeID(HeatingCoilOutletNode)))
               CALL ShowContinueError('...Cooling coil inlet node name  = '//TRIM(NodeID(CoolingCoilInletNode)))
               ErrorsFound=.TRUE.
            END IF
            IF(CoolingCoilOutletNode /= FanInletNode) THEN
               CALL ShowSevereError('For '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
               CALL ShowContinueError('When a draw through fan is specified, the cooling coil outlet node name must be '// &
                                      'the same as the fan inlet node name.')
               CALL ShowContinueError('...Cooling coil outlet node name = '//TRIM(NodeID(CoolingCoilOutletNode)))
               CALL ShowContinueError('...Fan inlet node name           = '//TRIM(NodeID(FanInletNode)))
               ErrorsFound=.TRUE.
            END IF
            IF(FanOutletNode /= Furnace(FurnaceNum)%FurnaceOutletNodeNum) THEN
               CALL ShowSevereError('For '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
               IF(FurnaceType_Num == Furnace_HeatCool)THEN
                 CALL ShowContinueError('When a draw through fan is specified, the fan outlet node name must be '// &
                                        'the same as the furnace outlet node name.')
                 CALL ShowContinueError('...Fan outlet node name     = '//TRIM(NodeID(FanOutletNode)))
                 CALL ShowContinueError('...Furnace outlet node name = ' &
                                                         //TRIM(NodeID(Furnace(FurnaceNum)%FurnaceOutletNodeNum)))
               ELSE
                 CALL ShowContinueError('When a draw through fan is specified, the fan outlet node name must be '// &
                                        'the same as the unitary system outlet node name.')
                 CALL ShowContinueError('...Fan outlet node name           = '//TRIM(NodeID(FanOutletNode)))
                 CALL ShowContinueError('...UnitarySystem outlet node name = ' &
                                                         //TRIM(NodeID(Furnace(FurnaceNum)%FurnaceOutletNodeNum)))
               END IF
               ErrorsFound=.TRUE.
            END IF
          END IF
        END IF ! ELSE from IF(Furnace(FurnaceNum)%FanPlace .EQ. BlowThru)THEN

        ! Add fan to component sets array
        CALL SetUpCompSets(CurrentModuleObject, Alphas(1), &
                           Alphas(7),Alphas(8),NodeID(FanInletNode),NodeID(FanOutletNode))

        ! Add DX cooling coil to component sets array
        CALL SetUpCompSets(CurrentModuleObject, Alphas(1), &
                           Alphas(12),Alphas(13),NodeID(CoolingCoilInletNode), NodeID(CoolingCoilOutletNode))

        ! Add heating coil to component sets array
        CALL SetUpCompSets(CurrentModuleObject, Alphas(1), &
                           Alphas(10),Alphas(11),NodeID(HeatingCoilInletNode), NodeID(HeatingCoilOutletNode))

        IF(ReheatCoilInletNode .GT. 0)THEN

          ! Add reheating coil to component sets array
          CALL SetUpCompSets(CurrentModuleObject, Alphas(1), &
                             Alphas(15),Alphas(16),NodeID(ReheatCoilInletNode),NodeID(ReheatCoilOutletNode))

        END IF

        !Set the furnace max outlet temperature
        Furnace(FurnaceNum)%DesignMaxOutletTemp = Numbers(1)

        Furnace(FurnaceNum)%MaxCoolAirVolFlow       = Numbers(2)
        IF (Furnace(FurnaceNum)%MaxCoolAirVolFlow .LE. 0 .AND. Furnace(FurnaceNum)%MaxCoolAirVolFlow .NE. AutoSize) THEN
          CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
          CALL ShowContinueError('Illegal '//TRIM(cNumericFields(2))//' = '//TRIM(TrimSigDigits(Numbers(2),7)))
          ErrorsFound = .TRUE.
        END IF

        Furnace(FurnaceNum)%MaxHeatAirVolFlow       = Numbers(3)
        IF (Furnace(FurnaceNum)%MaxHeatAirVolFlow .LE. 0 .AND. Furnace(FurnaceNum)%MaxHeatAirVolFlow .NE. AutoSize) THEN
          CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
          CALL ShowContinueError('Illegal '//TRIM(cNumericFields(3))//' = '//TRIM(TrimSigDigits(Numbers(3),7)))
          ErrorsFound = .TRUE.
        END IF

        Furnace(FurnaceNum)%MaxNoCoolHeatAirVolFlow = Numbers(4)
        IF (Furnace(FurnaceNum)%MaxNoCoolHeatAirVolFlow .LT. 0 .AND.Furnace(FurnaceNum)%MaxNoCoolHeatAirVolFlow .NE. AutoSize) THEN
          CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
          CALL ShowContinueError('Illegal '//TRIM(cNumericFields(4))//' = '//TRIM(TrimSigDigits(Numbers(4),7)))
          ErrorsFound = .TRUE.
        END IF

        IF(Numbers(2) .NE. Autosize .AND. Numbers(3) .NE. Autosize .AND. Numbers(4) .NE. Autosize)THEN
          Furnace(FurnaceNum)%DesignFanVolFlowRate = MAX(Numbers(2),Numbers(3),Numbers(4))
        ELSE
          Furnace(FurnaceNum)%DesignFanVolFlowRate = Autosize
        END IF

        IF (Furnace(FurnaceNum)%CoolingCoilType_Num == Coil_CoolingAirToAirVariableSpeed) THEN
             ErrFlag=.FALSE.
             Furnace(FurnaceNum)%MaxCoolAirVolFlow       &
                = GetCoilAirFlowRateVariableSpeed(CoolingCoilType,CoolingCoilName,ErrFlag)
             IF (ErrFlag) THEN
                CALL ShowContinueError('...occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
                ErrorsFound=.TRUE.
             ENDIF

            Furnace(FurnaceNum)%MaxNoCoolHeatAirVolFlow = &
                 MIN(Furnace(FurnaceNum)%MaxHeatAirVolFlow,Furnace(FurnaceNum)%MaxCoolAirVolFlow)
            IF(Furnace(FurnaceNum)%MaxHeatAirVolFlow /= Autosize .AND. &
               Furnace(FurnaceNum)%MaxCoolAirVolFlow /= Autosize)THEN
              Furnace(FurnaceNum)%DesignFanVolFlowRate = &
                 MAX(Furnace(FurnaceNum)%MaxHeatAirVolFlow,Furnace(FurnaceNum)%MaxCoolAirVolFlow)
            ELSE
              Furnace(FurnaceNum)%DesignFanVolFlowRate = Autosize
            END IF
        END IF

        IF(FanVolFlowRate .NE. AutoSize)THEN
          IF(FanVolFlowRate .LT. Furnace(FurnaceNum)%MaxCoolAirVolFlow .AND. &
               Furnace(FurnaceNum)%MaxCoolAirVolFlow .NE. AutoSize)THEN
            CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
            CALL ShowContinueError('... air flow rate = '//  &
               TRIM(TrimSigDigits(FanVolFlowRate,7))//&
              ' in fan object '//TRIM(FanName)//' is less than the maximum HVAC system air flow rate in ' &
                              //'cooling mode.')
            CALL ShowContinueError(' The '//TRIM(cNumericFields(2))//' is reset to the' &
                                 //' fan flow rate and the simulation continues.')
            Furnace(FurnaceNum)%MaxCoolAirVolFlow = FanVolFlowRate
            Furnace(FurnaceNum)%DesignFanVolFlowRate = FanVolFlowRate
          END IF
          IF(FanVolFlowRate .LT. Furnace(FurnaceNum)%MaxHeatAirVolFlow .AND. &
               Furnace(FurnaceNum)%MaxHeatAirVolFlow .NE. AutoSize)THEN
            CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
            CALL ShowContinueError('... air flow rate = '//  &
               TRIM(TrimSigDigits(FanVolFlowRate,7))//&
              ' in fan object '//TRIM(FanName)//' is less than the maximum HVAC system air flow rate in ' &
                              //'heating mode.')
            CALL ShowContinueError(' The '//TRIM(cNumericFields(3))//' is reset to the' &
                                 //' fan flow rate and the simulation continues.')
            Furnace(FurnaceNum)%MaxHeatAirVolFlow = FanVolFlowRate
            Furnace(FurnaceNum)%DesignFanVolFlowRate = FanVolFlowRate
          END IF
        END IF

        IF(Furnace(FurnaceNum)%FanSchedPtr .GT. 0)THEN
          IF (.NOT. CheckScheduleValueMinMax(Furnace(FurnaceNum)%FanSchedPtr,'>=',0.0d0,'<=',0.0d0)) THEN
!           set air flow control mode:
!             UseCompressorOnFlow = operate at last cooling or heating air flow requested when compressor is off
!             UseCompressorOffFlow = operate at value specified by user
!           AirFlowControl only valid if fan opmode = ContFanCycComp
            IF (Furnace(FurnaceNum)%MaxNoCoolHeatAirVolFlow .EQ. 0.0d0) THEN
              Furnace(FurnaceNum)%AirFlowControl = UseCompressorOnFlow
            ELSE
              Furnace(FurnaceNum)%AirFlowControl = UseCompressorOffFlow
            END IF
          END IF
        END IF

        IF (Furnace(FurnaceNum)%CoolingCoilType_Num == Coil_CoolingAirToAirVariableSpeed) THEN
          ErrFlag=.FALSE.
          Furnace(FurnaceNum)%DesignCoolingCapacity =   &
             GetCoilCapacityVariableSpeed(CoolingCoilType,CoolingCoilName,ErrFlag)
          IF (ErrFlag) THEN
            CALL ShowContinueError('...occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
            ErrorsFound=.TRUE.
          ENDIF
        ENDIF

        !Set heating convergence tolerance
        Furnace(FurnaceNum)%HeatingConvergenceTolerance = 0.001d0

        !Set cooling convergence tolerance
        Furnace(FurnaceNum)%CoolingConvergenceTolerance = 0.001d0

      END DO  !End of the HeatCool Furnace Loop



      ! Get the data for the Unitary System HeatPump AirToAir (UnitarySystem:HeatPump:AirToAir)
      DO HeatPumpNum = 1,  NumHeatPump

        CurrentModuleObject = 'AirLoopHVAC:UnitaryHeatPump:AirToAir'
        FanInletNode  = 0
        FanOutletNode = 0
        CoolingCoilInletNode  = 0
        CoolingCoilOutletNode = 0
        HeatingCoilInletNode  = 0
        HeatingCoilOutletNode = 0
        SupHeatCoilInletNode  = 0
        SupHeatCoilOutletNode = 0

        FurnaceNum= NumHeatOnly + NumHeatCool + NumUnitaryHeatOnly + NumUnitaryHeatCool + HeatPumpNum


        CALL GetObjectItem(CurrentModuleObject,HeatPumpNum,Alphas,NumAlphas,Numbers,NumNumbers,IOStatus, &
                           NumBlank=lNumericBlanks,AlphaBlank=lAlphaBlanks, &
                           AlphaFieldNames=cAlphaFields,NumericFieldNames=cNumericFields)

        IsNotOK=.FALSE.
        IsBlank=.FALSE.
        CALL VerifyName(Alphas(1),Furnace%Name,FurnaceNum-1,IsNotOK,IsBlank,TRIM(CurrentModuleObject)//' Name')
        IF (IsNotOK) THEN
          ErrorsFound=.TRUE.
          IF (IsBlank) Alphas(1)='xxxxx'
        ENDIF
        Furnace(FurnaceNum)%FurnaceType_Num = UnitarySys_HeatPump_AirToAir
        Furnace(FurnaceNum)%Name            = Alphas(1)
        IF (lAlphaBlanks(2)) THEN
          Furnace(FurnaceNum)%SchedPtr        = ScheduleAlwaysOn
        ELSE
          Furnace(FurnaceNum)%SchedPtr        = GetScheduleIndex(Alphas(2))
          IF (Furnace(FurnaceNum)%SchedPtr == 0) THEN
            CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
            CALL ShowContinueError('Illegal '//TRIM(cAlphaFields(2))//' = '//TRIM(Alphas(2)))
            ErrorsFound=.TRUE.
          ENDIF
        ENDIF


        Furnace(FurnaceNum)%FurnaceInletNodeNum = &
               GetOnlySingleNode(Alphas(3),ErrorsFound,TRIM(CurrentModuleObject),Alphas(1), &
                            NodeType_Air,NodeConnectionType_Inlet,1,ObjectIsParent)


        Furnace(FurnaceNum)%FurnaceOutletNodeNum = &
               GetOnlySingleNode(Alphas(4),ErrorsFound,TRIM(CurrentModuleObject),Alphas(1), &
                            NodeType_Air,NodeConnectionType_Outlet,1,ObjectIsParent)


        CALL TestCompSet(CurrentModuleObject,Alphas(1),Alphas(3),Alphas(4),'Air Nodes')


        !Get the Controlling Zone or Location of the Furnace Thermostat
        Furnace(FurnaceNum)%ControlZoneNum = FindItemInList(Alphas(5),Zone%Name,NumOfZones)
        IF (Furnace(FurnaceNum)%ControlZoneNum == 0) THEN
          CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
          CALL ShowContinueError('Illegal '//TRIM(cAlphaFields(5))//' = '//TRIM(Alphas(5)))
          ErrorsFound=.TRUE.
        ENDIF

        TotalZonesOnAirLoop = 0

        ! Get the node number for the zone with the thermostat
        IF (Furnace(FurnaceNum)%ControlZoneNum >  0) THEN
          AirNodeFound=.FALSE.
          AirLoopFound=.FALSE.
          DO ControlledZoneNum = 1,NumOfZones
            IF (ZoneEquipConfig(ControlledZoneNum)%ActualZoneNum /= Furnace(FurnaceNum)%ControlZoneNum) CYCLE
!             Find the controlled zone number for the specified thermostat location
              Furnace(FurnaceNum)%NodeNumofControlledZone=ZoneEquipConfig(ControlledZoneNum)%ZoneNode
!             Determine if furnace is on air loop served by the thermostat location specified
              AirLoopNumber = ZoneEquipConfig(ControlledZoneNum)%AirLoopNum
              IF(AirLoopNumber .GT. 0)THEN
                DO BranchNum = 1, PrimaryAirSystem(AirLoopNumber)%NumBranches
                  DO CompNum = 1, PrimaryAirSystem(AirLoopNumber)%Branch(BranchNum)%TotalComponents
                    IF(.NOT. SameString(PrimaryAirSystem(AirLoopNumber)%Branch(BranchNum)%Comp(CompNum)%Name, &
                             Alphas(1)) .OR. &
                       .NOT. SameString(PrimaryAirSystem(AirLoopNumber)%Branch(BranchNum)%Comp(CompNum)%TypeOf, &
                             CurrentModuleObject))CYCLE
                    AirLoopFound=.TRUE.
                    EXIT
                  END DO
                  IF(AirLoopFound)EXIT
                END DO
                DO TstatZoneNum = 1, NumTempControlledZones
                  IF(TempControlledZone(TstatZoneNum)%ActualZoneNum .NE. Furnace(FurnaceNum)%ControlZoneNum)CYCLE
                  AirNodeFound=.TRUE.
                END DO
                DO TstatZoneNum = 1, NumComfortControlledZones
                  IF(ComfortControlledZone(TstatZoneNum)%ActualZoneNum .NE. Furnace(FurnaceNum)%ControlZoneNum)CYCLE
                  AirNodeFound=.TRUE.
                END DO
              ELSE
                CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
                CALL ShowContinueError('Did not find an AirLoopHVAC.')
                CALL ShowContinueError('Specified '//TRIM(cAlphaFields(5))//' = '//TRIM(Alphas(5)))
                ErrorsFound=.TRUE.
              END IF
            EXIT
          ENDDO
          IF (.not. AirNodeFound) THEN
            CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
            CALL ShowContinueError('Did not find air node (zone with thermostat).')
            CALL ShowContinueError('Specified '//TRIM(cAlphaFields(5))//' = '//TRIM(Alphas(5)))
            CALL ShowContinueError('Both a ZoneHVAC:EquipmentConnections object and a ZoneControl:Thermostat object' &
                                //' must be specified for this zone.')
            ErrorsFound=.TRUE.
          ENDIF
          IF (.not. AirLoopFound) THEN
            CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
            CALL ShowSevereError('Did not find correct AirLoopHVAC.')
            CALL ShowContinueError('Specified '//TRIM(cAlphaFields(5))//' = '//TRIM(Alphas(5)))
            ErrorsFound=.TRUE.
          ENDIF
          IF(AirLoopNumber .GT. 0)THEN
            DO ControlledZoneNum = 1,NumOfZones
              IF (ZoneEquipConfig(ControlledZoneNum)%AirLoopNum == AirLoopNumber) THEN
                TotalZonesOnAirLoop = TotalZonesOnAirLoop + 1
              END IF
            END DO
          END IF
        ENDIF

         !Get fan data
        FanType = Alphas(6)
        FanName = Alphas(7)

        ErrFlag=.FALSE.
        CALL GetFanType(FanName, Furnace(FurnaceNum)%FanType_Num, ErrFlag,   &
           CurrentModuleObject,Alphas(1))
        IF (ErrFlag) THEN
          ErrorsFound=.TRUE.
        END IF

        IF (Furnace(FurnaceNum)%FanType_Num == FanType_SimpleOnOff .OR. &
            Furnace(FurnaceNum)%FanType_Num == FanType_SimpleConstVolume)THEN
          CALL ValidateComponent(FanType,FanName,IsNotOK, TRIM(CurrentModuleObject))
          IF (IsNotOK) THEN
            CALL ShowContinueError('...occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
            ErrorsFound=.TRUE.

          ELSE ! mine data from fan object

            ! Get the fan index
            ErrFlag=.FALSE.
            CALL GetFanIndex(FanName, Furnace(FurnaceNum)%FanIndex, ErrFlag)
            IF (ErrFlag) THEN
              CALL ShowContinueError('...occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
              ErrorsFound=.TRUE.
            ENDIF

            ! Get the fan inlet node number
            ErrFlag=.FALSE.
            FanInletNode  = GetFanInletNode(FanType,FanName,ErrFlag)
            IF (ErrFlag) THEN
              CALL ShowContinueError('...occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
              ErrorsFound=.TRUE.
            ENDIF

            ! Get the fan outlet node number
            ErrFlag=.FALSE.
            FanOutletNode = GetFanOutletNode(FanType,FanName,ErrFlag)
            IF (ErrFlag) THEN
              CALL ShowContinueError('...occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
              ErrorsFound=.TRUE.
            ENDIF

            ! Get the fan availability schedule
            ErrFlag=.FALSE.
            Furnace(FurnaceNum)%FanAvailSchedPtr = GetFanAvailSchPtr(FanType,FanName,ErrFlag)
            IF (ErrFlag) THEN
              CALL ShowContinueError('...occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
              ErrorsFound=.TRUE.
            ENDIF

            ! Get the Design Fan Volume Flow Rate
            ErrFlag=.FALSE.
            FanVolFlowRate = GetFanDesignVolumeFlowRate(FanType,FanName,ErrFlag)
            Furnace(FurnaceNum)%ActualFanVolFlowRate = FanVolFlowRate
            IF (ErrFlag) THEN
              CALL ShowContinueError('...occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
              ErrorsFound=.TRUE.
            ENDIF

          ENDIF ! IF (IsNotOK) THEN

        ELSE
          CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
          CALL ShowContinueError('Illegal '//TRIM(cAlphaFields(6))//' = '//TRIM(Alphas(6)))
          ErrorsFound=.TRUE.
        END IF


            !Get heating coil type and name data
        HeatingCoilType = Alphas(8)
        HeatingCoilName = Alphas(9)

        ErrFlag = .FALSE.

        IF (SameString(HeatingCoilType, 'COIL:HEATING:DX:VARIABLESPEED') )THEN
           Furnace(FurnaceNum)%HeatingCoilType_Num = Coil_HeatingAirToAirVariableSpeed
        ELSE
           Furnace(FurnaceNum)%HeatingCoilType_Num = GetDXCoilTypeNum(HeatingCoilType,HeatingCoilName,ErrFlag)
        END IF

        IF (ErrFlag) THEN
          CALL ShowContinueError('...occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
          ErrorsFound=.TRUE.
        END IF

        IF (Furnace(FurnaceNum)%HeatingCoilType_Num == CoilDX_HeatingEmpirical)THEN
          CALL ValidateComponent(HeatingCoilType,HeatingCoilName,IsNotOK,  &
                                 TRIM(CurrentModuleObject))
          IF (IsNotOK) THEN
            CALL ShowContinueError('...occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
            ErrorsFound=.TRUE.

          ELSE ! mine data from DX heating coil

            CALL GetDXCoilIndex(HeatingCoilName,Furnace(FurnaceNum)%HeatingCoilIndex,IsNotOK)
            IF(IsNotOK)THEN
              CALL ShowContinueError('...occurs '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
              ErrorsFound=.TRUE.
            END IF

            ! Get the Heating Coil Node Names
            ErrFlag=.FALSE.
            HeatingCoilInletNode = GetDXCoilInletNode(HeatingCoilType,HeatingCoilName,ErrFlag)
            HeatingCoilOutletNode = GetDXCoilOutletNode(HeatingCoilType,HeatingCoilName,ErrFlag)
            IF (ErrFlag) THEN
              CALL ShowContinueError('...occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
              ErrorsFound=.TRUE.
            ENDIF

            ! Get the design heating capacity
            ErrFlag=.FALSE.
            Furnace(FurnaceNum)%DesignHeatingCapacity = GetDXCoilCapacity(HeatingCoilType,HeatingCoilName,ErrFlag)
            IF (ErrFlag) THEN
              CALL ShowContinueError('...occurs in '//TRIM(CurrentModuleObject)//' ='//TRIM(Alphas(1)))
              ErrorsFound=.TRUE.
            ENDIF

          ENDIF ! IF (IsNotOK) THEN
        ELSE IF(Furnace(FurnaceNum)%HeatingCoilType_Num == Coil_HeatingAirToAirVariableSpeed) THEN
          CALL ValidateComponent(HeatingCoilType,HeatingCoilName,IsNotOK,  &
                                 TRIM(CurrentModuleObject))
          IF (IsNotOK) THEN
            CALL ShowContinueError('...occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
            ErrorsFound=.TRUE.
          ELSE
            Furnace(FurnaceNum)%HeatingCoilIndex = GetCoilIndexVariableSpeed(HeatingCoilType, HeatingCoilName,ErrFlag)
            HeatingCoilInletNode=GetCoilInletNodeVariableSpeed(HeatingCoilType, HeatingCoilName,ErrFlag)
            HeatingCoilOutletNode=GetCoilOutletNodeVariableSpeed(HeatingCoilType, HeatingCoilName,ErrFlag)
          ENDIF
        ELSE
          CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
          CALL ShowContinueError('Illegal '//TRIM(cAlphaFields(8))//' = '//TRIM(Alphas(8)))
          ErrorsFound=.TRUE.
        END IF


        ! Get Cooling Coil Information if available
        CoolingCoilType = Alphas(10)
        CoolingCoilName = Alphas(11)

        IF (SameString(CoolingCoilType, 'COIL:COOLING:DX:VARIABLESPEED') )THEN
          Furnace(FurnaceNum)%CoolingCoilType_Num = Coil_CoolingAirToAirVariableSpeed
        END IF

        CALL ValidateComponent(CoolingCoilType,CoolingCoilName,IsNotOK,  &
                               TRIM(CurrentModuleObject))

        IF (IsNotOK) THEN
          CALL ShowContinueError('...occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
          ErrorsFound=.TRUE.

        ELSE ! mine data from DX cooling coil

          ErrFlag = .FALSE.
          PrintMessage = .FALSE.

          IF(Furnace(FurnaceNum)%CoolingCoilType_Num /= Coil_CoolingAirToAirVariableSpeed) THEN
              Furnace(FurnaceNum)%CoolingCoilType_Num = &
                     GetDXCoilTypeNum(CoolingCoilType,CoolingCoilName,ErrFlag,PrintMessage)
          END IF

          ! If coil type not found, check to see if a HX assisted cooling coil is used.
          IF(Furnace(FurnaceNum)%CoolingCoilType_Num .EQ. 0)THEN
            ErrFlag = .FALSE.
            PrintMessage = .FALSE.
            Furnace(FurnaceNum)%CoolingCoilType_Num = &
                 GetHXAssistedCoilTypeNum(CoolingCoilType,CoolingCoilName,ErrFlag,PrintMessage)
          END IF

          IF (Furnace(FurnaceNum)%CoolingCoilType_Num == CoilDX_CoolingSingleSpeed)THEN

            ! Get the cooling coil node numbers
            ErrFlag=.FALSE.
            CALL GetDXCoilIndex(CoolingCoilName,Furnace(FurnaceNum)%CoolingCoilIndex,ErrFlag)
            CoolingCoilInletNode = GetDXCoilInletNode(CoolingCoilType,CoolingCoilName,ErrFlag)
            CoolingCoilOutletNode = GetDXCoilOutletNode(CoolingCoilType,CoolingCoilName,ErrFlag)
            IF (ErrFlag) THEN
              CALL ShowContinueError('...occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
              ErrorsFound=.TRUE.
            ENDIF

            ! Get the DX cooling coil design capacity
            ErrFlag=.FALSE.
            Furnace(FurnaceNum)%DesignCoolingCapacity = GetDXCoilCapacity(CoolingCoilType,CoolingCoilName,ErrFlag)
            IF (ErrFlag) THEN
              CALL ShowContinueError('...occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
              ErrorsFound=.TRUE.
            ENDIF

          ELSEIF (Furnace(FurnaceNum)%CoolingCoilType_Num == CoilDX_CoolingHXAssisted)THEN

            ! Get the cooling coil node numbers
            ErrFlag=.FALSE.
            CALL GetHXDXCoilIndex(CoolingCoilName,Furnace(FurnaceNum)%CoolingCoilIndex,ErrFlag)
            CoolingCoilInletNode = GetDXHXAsstdCoilInletNode(CoolingCoilType,CoolingCoilName,ErrFlag)
            CoolingCoilOutletNode = GetDXHXAsstdCoilOutletNode(CoolingCoilType,CoolingCoilName,ErrFlag)
            IF (ErrFlag) THEN
              CALL ShowContinueError('...occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
              ErrorsFound=.TRUE.
            ENDIF

            ! Get the heat exchanger assisted cooling coil design capacity
            ErrFlag=.FALSE.
            Furnace(FurnaceNum)%DesignCoolingCapacity = GetDXHXAsstdCoilCapacity(CoolingCoilType,CoolingCoilName,ErrFlag)
            IF (ErrFlag) THEN
              CALL ShowContinueError('...occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
              ErrorsFound=.TRUE.
            ENDIF

            ! get the actual index to the DX cooling coil object
            DXCoilIndex = GetActualDXCoilIndex(CoolingCoilType,CoolingCoilName,ErrorsFound)
            Furnace(FurnaceNum)%ActualDXCoilIndexforHXAssisted = DXCoilIndex

          ELSE IF (Furnace(FurnaceNum)%CoolingCoilType_Num == Coil_CoolingAirToAirVariableSpeed )THEN
            ! BOS ADDED, AUG/2012, VARIIABLE SPEED DX COOLING COIL
            !  Furnace(FurnaceNum)%DXCoolCoilType = 'COIL:COOLING:DX:VARIABLESPEED'
            !  Furnace(FurnaceNum)%DXCoolCoilName = CoolingCoilName
              CALL ValidateComponent(CoolingCoilType,CoolingCoilName,IsNotOK,  &
                                     TRIM(CurrentModuleObject))
              IF (IsNotOK) THEN
                CALL ShowContinueError('...specified in '//TRIM(CurrentModuleObject)//'="'//TRIM(Alphas(1))//'".')
                ErrorsFound=.TRUE.
              ELSE
                ErrFlag = .FALSE.
                Furnace(FurnaceNum)%CoolingCoilIndex = GetCoilIndexVariableSpeed(CoolingCoilType, &
                            CoolingCoilName,ErrFlag)
                IF(ErrFlag)THEN
                  CALL ShowContinueError('...specified in '//TRIM(CurrentModuleObject)//'="'//TRIM(Alphas(1))//'".')
                  ErrorsFound=.TRUE.
                END IF
                CoolingCoilInletNode =GetCoilInletNodeVariableSpeed(CoolingCoilType,CoolingCoilName,ErrFlag)
                CoolingCoilOutletNode =GetCoilOutletNodeVariableSpeed(CoolingCoilType,CoolingCoilName,ErrFlag)
                Furnace(FurnaceNum)%CondenserNodeNum = GetVSCoilCondenserInletNode(CoolingCoilName,ErrFlag)

                IF (ErrFlag) THEN
                  CALL ShowContinueError('...occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
                  ErrorsFound=.TRUE.
                ENDIF
              ENDIF
          ELSE
          CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
          CALL ShowContinueError('Illegal '//TRIM(cAlphaFields(10))//' = '//TRIM(Alphas(10)))
            ErrorsFound=.TRUE.
          END IF

        ENDIF

        IF(Furnace(FurnaceNum)%CoolingCoilType_Num == Coil_CoolingAirToAirVariableSpeed .AND. &
                Furnace(FurnaceNum)%HeatingCoilType_Num == Coil_HeatingAirToAirVariableSpeed) THEN
          !Furnace(FurnaceNum)%WatertoAirHPType = WatertoAir_VarSpeedEquationFit
          CALL SetVarSpeedCoilData(Furnace(FurnaceNum)%CoolingCoilIndex,ErrorsFound, &
                                CompanionHeatingCoilNum=Furnace(FurnaceNum)%HeatingCoilIndex)
        END IF

        ! Get supplemental heating coil information
        SuppHeatCoilType = Alphas(12)
        SuppHeatCoilName = Alphas(13)
        Furnace(FurnaceNum)%SuppHeatCoilType = SuppHeatCoilType
        Furnace(FurnaceNum)%SuppHeatCoilName = SuppHeatCoilName
        ErrFlag = .FALSE.
        IF (SameString(SuppHeatCoilType,'Coil:Heating:Gas')      .OR. &
              SameString(SuppHeatCoilType,'Coil:Heating:Electric') ) THEN

            Furnace(FurnaceNum)%SuppHeatCoilType_Num = GetHeatingCoilTypeNum(SuppHeatCoilType,SuppHeatCoilName,ErrFlag)
            IF (ErrFlag) THEN
               CALL ShowContinueError('...occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
               ErrorsFound=.TRUE.
            ELSE
              IsNotOK = .FALSE.
              CALL ValidateComponent(SuppHeatCoilType,SuppHeatCoilName,IsNotOK,  &
                                   TRIM(CurrentModuleObject))
              IF (IsNotOK) THEN
                CALL ShowContinueError('In '//TRIM(CurrentModuleObject)//' "'//TRIM(Alphas(1))//'"')
                ErrorsFound=.TRUE.

               ELSE ! mine data from the supplemental heating coil

                  CALL GetHeatingCoilIndex(SuppHeatCoilName,Furnace(FurnaceNum)%SuppHeatCoilIndex,IsNotOK)
                  IF (IsNotOK) THEN
                    CALL ShowContinueError('...occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
                    ErrorsFound=.TRUE.
                  ENDIF

                  ! Get the Supplemental Heating Coil Inlet Node Number
                  ErrFlag=.FALSE.
                  SupHeatCoilInletNode = GetHeatingCoilInletNode(SuppHeatCoilType,SuppHeatCoilName,ErrFlag)
                  IF (ErrFlag) THEN
                     CALL ShowContinueError('...occurs in '//TRIM(CurrentModuleObject)//' "'//TRIM(Alphas(1))//'"')
                     ErrorsFound=.TRUE.
                  ENDIF

                  ! Get the Supplemental Heating Coil Outlet Node Number
                  ErrFlag=.FALSE.
                  SupHeatCoilOutletNode = GetHeatingCoilOutletNode(SuppHeatCoilType,SuppHeatCoilName,ErrFlag)

                  IF (ErrFlag) THEN
                    CALL ShowContinueError('...occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
                    ErrorsFound=.TRUE.
                  ENDIF

                  ! Get the supplemental heating coil design capacity
                  ErrFlag=.FALSE.
                  Furnace(FurnaceNum)%DesignSuppHeatingCapacity =   &
                     GetHeatingCoilCapacity(SuppHeatCoilType,SuppHeatCoilName,ErrFlag)
                  IF (ErrFlag) THEN
                    CALL ShowContinueError('...occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
                    ErrorsFound=.TRUE.
                  ENDIF

               ENDIF ! IF (IsNotOK) THEN
            ENDIF
        ELSEIF (SameString(SuppHeatCoilType,'Coil:Heating:Water')) THEN
            Furnace(FurnaceNum)%SuppHeatCoilType_Num = Coil_HeatingWater
            CALL ValidateComponent(SuppHeatCoilType,SuppHeatCoilName,IsNotOK,TRIM(CurrentModuleObject))
            IF (IsNotOK) THEN
                CALL ShowContinueError('...occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
                ErrorsFound=.TRUE.
            ELSE ! mine data from heating coil object

                ! Get the Heating Coil water Inlet or control Node number
                ErrFlag = .FALSE.
                Furnace(FurnaceNum)%SuppCoilControlNode = GetCoilWaterInletNode('Coil:Heating:Water',SuppHeatCoilName,ErrFlag)
                IF(ErrFlag)THEN
                  CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Furnace(FurnaceNum)%Name))
                  ErrorsFound = .TRUE.
                END IF

                ! Get the ReHeat Coil hot water max volume flow rate
                ErrFlag = .FALSE.
                Furnace(FurnaceNum)%MaxSuppCoilFluidFlow = GetCoilMaxWaterFlowRate('Coil:Heating:Water',  &
                                                           SuppHeatCoilName,ErrFlag)
                IF(ErrFlag)THEN
                  CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Furnace(FurnaceNum)%Name))
                  ErrorsFound = .TRUE.
                END IF

                ! Get the ReHeat Coil Inlet Node
                ErrFlag = .FALSE.
                SupHeatCoilInletNode =  GetWaterCoilInletNode('Coil:Heating:Water',SuppHeatCoilName,ErrFlag)
                Furnace(FurnaceNum)%SuppCoilAirInletNode = SupHeatCoilInletNode
                IF(ErrFlag)THEN
                  CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Furnace(FurnaceNum)%Name))
                  ErrorsFound = .TRUE.
                END IF

                ! Get the ReHeat Coil Outlet Node
                ErrFlag = .FALSE.
                SupHeatCoilOutletNode = GetWaterCoilOutletNode('Coil:Heating:Water',SuppHeatCoilName,ErrFlag)
                Furnace(FurnaceNum)%SuppCoilAirOutletNode = SupHeatCoilOutletNode
                IF(ErrFlag)THEN
                  CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Furnace(FurnaceNum)%Name))
                  ErrorsFound = .TRUE.
                END IF

                ! check if user has also used a water coil controller, which they should not do
                ErrFlag = .FALSE.
                CALL CheckCoilWaterInletNode(Furnace(FurnaceNum)%CoilControlNode, ErrFlag)
                IF (.NOT. ErrFlag) THEN ! then did find a controller so that is bad
                  CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(Furnace(FurnaceNum)%Name) &
                                      //' has a conflicting Controller:WaterCoil object' )
                  CALL ShowContinueError('Hot water coils are controlled directly by unitary and furnace systems.')
                  CALL ShowContinueError('No water coil controller should be input for the coil.')
                  ErrorsFound = .TRUE.
                ENDIF

            ENDIF

        ELSEIF (SameString(SuppHeatCoilType,'Coil:Heating:Steam')) THEN
            Furnace(FurnaceNum)%SuppHeatCoilType_Num = Coil_HeatingSteam
            CALL ValidateComponent(SuppHeatCoilType,SuppHeatCoilName,IsNotOK,TRIM(CurrentModuleObject))
            IF (IsNotOK) THEN
                CALL ShowContinueError('...occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
                ErrorsFound=.TRUE.
            ELSE ! mine data from heating coil object

                ErrFlag = .FALSE.
                Furnace(FurnaceNum)%SuppHeatCoilIndex = GetSTeamCoilIndex('COIL:HEATING:STEAM',SuppHeatCoilName,ErrFlag)
                IF (Furnace(FurnaceNum)%SuppHeatCoilIndex .EQ. 0) THEN
                    CALL ShowSevereError(TRIM(CurrentModuleObject)//' illegal '//TRIM(cAlphaFields(12))//' = ' &
                                  //TRIM(SuppHeatCoilName))
                    CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Furnace(FurnaceNum)%Name))
                    ErrorsFound = .TRUE.
                END IF

                ! Get the Heating Coil steam inlet node number
                ErrFlag = .FALSE.
                Furnace(FurnaceNum)%SuppCoilControlNode = GetSteamCoilSteamInletNode('Coil:Heating:Steam',SuppHeatCoilName,ErrFlag)
                IF(ErrFlag)THEN
                  CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Furnace(FurnaceNum)%Name))
                  ErrorsFound = .TRUE.
                END IF

                ! Get the Heating Coil steam max volume flow rate
                Furnace(FurnaceNum)%MaxSuppCoilFluidFlow = GetCoilMaxSteamFlowRate(Furnace(FurnaceNum)%SuppHeatCoilIndex,ErrFlag)
                IF (Furnace(FurnaceNum)%MaxSuppCoilFluidFlow .GT. 0.0d0)THEN
                   SteamIndex = 0      ! Function GetSatDensityRefrig will look up steam index if 0 is passed
                   SteamDensity=GetSatDensityRefrig("STEAM",TempSteamIn,1.0d0,SteamIndex,'GetAirLoopHVACHeatCoolInput')
                   Furnace(FurnaceNum)%MaxSuppCoilFluidFlow = &
                                     GetCoilMaxSteamFlowRate(Furnace(FurnaceNum)%SuppHeatCoilIndex,ErrFlag) * SteamDensity
                END IF

                ! Get the Heating Coil Inlet Node
                ErrFlag = .FALSE.
                SupHeatCoilInletNode = &
                         GetSteamCoilAirInletNode(Furnace(FurnaceNum)%SuppHeatCoilIndex,SuppHeatCoilName,ErrFlag)
                Furnace(FurnaceNum)%SuppCoilAirInletNode = SupHeatCoilInletNode
                IF(ErrFlag)THEN
                  CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Furnace(FurnaceNum)%Name))
                  ErrorsFound = .TRUE.
                END IF

                ! Get the Heating Coil Outlet Node
                ErrFlag = .FALSE.
                SupHeatCoilOutletNode = &
                         GetSteamCoilAirOutletNode(Furnace(FurnaceNum)%SuppHeatCoilIndex,SuppHeatCoilName,ErrFlag)
                Furnace(FurnaceNum)%SuppCoilAirOutletNode = SupHeatCoilOutletNode
                IF(ErrFlag)THEN
                  CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Furnace(FurnaceNum)%Name))
                  ErrorsFound = .TRUE.
                END IF

            ENDIF

        ELSE
          CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
          CALL ShowContinueError('Illegal '//TRIM(cAlphaFields(12))//' = '//TRIM(Alphas(12)))
          ErrorsFound=.TRUE.
        END IF ! IF (Furnace(FurnaceNum)%HeatingCoilType_Num == Coil_HeatingGas .OR. &, etc.


        IF (SameString(Alphas(14),'BlowThrough')  )      &
               Furnace(FurnaceNum)%FanPlace = BlowThru
        IF (SameString(Alphas(14),'DrawThrough')  )      &
              Furnace(FurnaceNum)%FanPlace = DrawThru
        IF (Furnace(FurnaceNum)%FanPlace .EQ. 0) THEN
          CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
          CALL ShowContinueError('Illegal '//TRIM(cAlphaFields(14))//' = '//TRIM(Alphas(14)))
          ErrorsFound = .TRUE.
        END IF

        Furnace(FurnaceNum)%FanSchedPtr     = GetScheduleIndex(Alphas(15))
        IF (.NOT. lAlphaBlanks(15) .AND. Furnace(FurnaceNum)%FanSchedPtr == 0) THEN
          CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
          CALL ShowContinueError('Illegal '//TRIM(cAlphaFields(15))//' = '//TRIM(Alphas(15)))
          ErrorsFound=.TRUE.
        ELSEIF (lAlphaBlanks(15)) THEN
          Furnace(FurnaceNum)%OpMode = CycFanCycCoil
          IF(Furnace(FurnaceNum)%FanType_Num /= FanType_SimpleOnOff)THEN
            CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(Furnace(FurnaceNum)%Name))
            CALL ShowContinueError(TRIM(cAlphaFields(6))//' = '//TRIM(Alphas(6)))
            CALL ShowContinueError('Fan type must be Fan:OnOff when '//  &
                                   TRIM(cAlphaFields(15))//' = Blank.')
            ErrorsFound=.TRUE.
          END IF
        ENDIF

        IF (Furnace(FurnaceNum)%FanType_Num == FanType_SimpleConstVolume)THEN
          IF(Furnace(FurnaceNum)%FanSchedPtr .GT. 0)THEN
            IF (.NOT. CheckScheduleValueMinMax(Furnace(FurnaceNum)%FanSchedPtr,'>',0.0d0,'<=',1.0d0)) THEN
              CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
              CALL ShowContinueError('For '//TRIM(cAlphaFields(7))//' = '//TRIM(Alphas(7)))
              CALL ShowContinueError('Fan operating mode must be continuous (fan operating mode schedule values > 0).')
              CALL ShowContinueError('Error found in '//TRIM(cAlphaFields(15))//' = '//TRIM(Alphas(15)))
              CALL ShowContinueError('...schedule values must be (>0., <=1.)')
              ErrorsFound=.TRUE.
            END IF
          END IF
        END IF

        ! Dehumidification Control Type
        IF (SameString(Alphas(16),'None') .OR. SameString(Alphas(16),'Multimode') .OR. &
            SameString(Alphas(16),'CoolReheat'))THEN
          AirNodeFound=.FALSE.
          If(SameString(Alphas(16),'Multimode'))THEN
            Furnace(FurnaceNum)%DehumidControlType_Num = DehumidControl_Multimode
            Furnace(FurnaceNum)%Humidistat = .TRUE.
            IF(Furnace(FurnaceNum)%CoolingCoilType_Num /= CoilDX_CoolingHXAssisted) THEN
              CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
              CALL ShowContinueError('Illegal '//TRIM(cAlphaFields(16))//' = '//TRIM(Alphas(16)))
              CALL ShowContinueError('Multimode control must be used with a Heat Exchanger Assisted Cooling Coil.')
              ErrorsFound=.TRUE.
            END IF
          END IF
          IF(SameString(Alphas(16),'CoolReheat'))THEN
            Furnace(FurnaceNum)%DehumidControlType_Num = DehumidControl_CoolReheat
            Furnace(FurnaceNum)%Humidistat = .TRUE.
          END IF
          IF(SameString(Alphas(16),'None'))THEN
            Furnace(FurnaceNum)%DehumidControlType_Num = DehumidControl_None
            Furnace(FurnaceNum)%Humidistat = .FALSE.
          END IF
          IF(Furnace(FurnaceNum)%Humidistat)THEN
            DO HstatZoneNum = 1, NumHumidityControlZones
              IF(HumidityControlZone(HstatZoneNum)%ActualZoneNum .NE. Furnace(FurnaceNum)%ControlZoneNum)CYCLE
              AirNodeFound=.TRUE.
            END DO
            IF (.not. AirNodeFound) THEN
              CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
              CALL ShowContinueError('Did not find Air Node (Zone with Humidistat).')
              CALL ShowContinueError('Specified '//TRIM(cAlphaFields(5))//' = '//TRIM(Alphas(5)))
              ErrorsFound=.TRUE.
            ENDIF
          END IF
        ELSE ! invalid input or blank
          IF (.NOT. lAlphaBlanks(16)) THEN
            CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
            CALL ShowContinueError('Illegal '//TRIM(cAlphaFields(16))//' = '//TRIM(Alphas(16)))
            ErrorsFound=.TRUE.
          ELSE
            Furnace(FurnaceNum)%Humidistat = .FALSE.
            Furnace(FurnaceNum)%DehumidControlType_Num = DehumidControl_None
          ENDIF
        END IF

        ! Check node names for child components
        IF (Furnace(FurnaceNum)%FanPlace == BlowThru) THEN
          IF(FanInletNode /= Furnace(FurnaceNum)%FurnaceInletNodeNum) THEN
              CALL ShowSevereError('For '//TRIM(CurrentModuleObject)//' "'//TRIM(Alphas(1))//'"')
              CALL ShowContinueError('When a blow through fan is specified, the fan inlet node name must be '// &
                                     'the same as the unitary system inlet node name.')
              CALL ShowContinueError('...Fan inlet node name            = '//TRIM(NodeID(FanInletNode)))
              CALL ShowContinueError('...Unitary system inlet node name = '//TRIM(NodeID(Furnace(FurnaceNum)%FurnaceInletNodeNum)))
              ErrorsFound=.TRUE.
          END IF
          IF(FanOutletNode /= CoolingCoilInletNode) THEN
              CALL ShowSevereError('For '//TRIM(CurrentModuleObject)//' "'//TRIM(Alphas(1))//'"')
              CALL ShowContinueError('When a blow through fan is specified, the fan outlet node name must be '// &
                                     'the same as the cooling coil inlet node name.')
              CALL ShowContinueError('...Fan outlet node name         = '//TRIM(NodeID(FanOutletNode)))
              CALL ShowContinueError('...Cooling coil inlet node name = '//TRIM(NodeID(CoolingCoilInletNode)))
              ErrorsFound=.TRUE.
          END IF
          IF(CoolingCoilOutletNode /= HeatingCoilInletNode) THEN
              CALL ShowSevereError('For '//TRIM(CurrentModuleObject)//' "'//TRIM(Alphas(1))//'"')
              CALL ShowContinueError('The cooling coil outlet node name must be '// &
                                     'the same as the heating coil inlet node name.')
              CALL ShowContinueError('...Cooling coil outlet node name = '//TRIM(NodeID(CoolingCoilOutletNode)))
              CALL ShowContinueError('...Heating coil inlet node name  = '//TRIM(NodeID(HeatingCoilInletNode)))
              ErrorsFound=.TRUE.
          END IF
          IF(HeatingCoilOutletNode /= SupHeatCoilInletNode) THEN
              CALL ShowSevereError('For '//TRIM(CurrentModuleObject)//' "'//TRIM(Alphas(1))//'"')
              CALL ShowContinueError('When a blow through fan is specified, the heating coil outlet node name must be '// &
                                     'the same as the supplemental heating coil inlet node name.')
              CALL ShowContinueError('...Heating coil outlet node name              = '//TRIM(NodeID(HeatingCoilOutletNode)))
              CALL ShowContinueError('...Supplemental heating coil inlet node name  = '//TRIM(NodeID(SupHeatCoilInletNode)))
              ErrorsFound=.TRUE.
          END IF
          IF(SupHeatCoilOutletNode /= Furnace(FurnaceNum)%FurnaceOutletNodeNum) THEN
              CALL ShowSevereError('For '//TRIM(CurrentModuleObject)//' "'//TRIM(Alphas(1))//'"')
              CALL ShowContinueError('The supplemental heating coil outlet node name must be '// &
                                     'the same as the unitary system outlet node name.')
              CALL ShowContinueError('...Supplemental heating coil outlet node name = '//TRIM(NodeID(SupHeatCoilOutletNode)))
              CALL ShowContinueError('...Unitary system outlet node name            = ' &
                                     //TRIM(NodeID(Furnace(FurnaceNum)%FurnaceOutletNodeNum)))
              ErrorsFound=.TRUE.
          END IF
        ELSE
          IF(CoolingCoilInletNode /= Furnace(FurnaceNum)%FurnaceInletNodeNum) THEN
              CALL ShowSevereError('For '//TRIM(CurrentModuleObject)//' "'//TRIM(Alphas(1))//'"')
              CALL ShowContinueError('When a draw through fan is specified, the cooling coil inlet node name must be '// &
                                     'the same as the unitary system inlet node name.')
              CALL ShowContinueError('...Cooling coil inlet node name   = '//TRIM(NodeID(CoolingCoilInletNode)))
              CALL ShowContinueError('...Unitary system inlet node name = '//TRIM(NodeID(Furnace(FurnaceNum)%FurnaceInletNodeNum)))
              ErrorsFound=.TRUE.
          END IF
          IF(CoolingCoilOutletNode /= HeatingCoilInletNode) THEN
              CALL ShowSevereError('For '//TRIM(CurrentModuleObject)//' "'//TRIM(Alphas(1))//'"')
              CALL ShowContinueError('The cooling coil outlet node name must be '// &
                                     'the same as the heating coil inlet node name.')
              CALL ShowContinueError('...Cooling coil outlet node name = '//TRIM(NodeID(CoolingCoilOutletNode)))
              CALL ShowContinueError('...Heating coil inlet node name  = '//TRIM(NodeID(HeatingCoilInletNode)))
              ErrorsFound=.TRUE.
          END IF
          IF(HeatingCoilOutletNode /= FanInletNode) THEN
              CALL ShowSevereError('For '//TRIM(CurrentModuleObject)//' "'//TRIM(Alphas(1))//'"')
              CALL ShowContinueError('When a draw through fan is specified, the heating coil outlet node name must be '// &
                                     'the same as the fan inlet node name.')
              CALL ShowContinueError('...Heating coil outlet node name = '//TRIM(NodeID(HeatingCoilOutletNode)))
              CALL ShowContinueError('...Fan inlet node name           = '//TRIM(NodeID(FanInletNode)))
              ErrorsFound=.TRUE.
          END IF
          IF(FanOutletNode /= SupHeatCoilInletNode) THEN
              CALL ShowSevereError('For '//TRIM(CurrentModuleObject)//' "'//TRIM(Alphas(1))//'"')
              CALL ShowContinueError('When a draw through fan is specified, the fan outlet node name must be '// &
                                     'the same as the supplemental heating coil inlet node name.')
              CALL ShowContinueError('...Fan outlet node name                       = '//TRIM(NodeID(FanOutletNode)))
              CALL ShowContinueError('...Supplemental heating coil inlet node name  = '//TRIM(NodeID(SupHeatCoilInletNode)))
              ErrorsFound=.TRUE.
          END IF
          IF(SupHeatCoilOutletNode /= Furnace(FurnaceNum)%FurnaceOutletNodeNum) THEN
              CALL ShowSevereError('For '//TRIM(CurrentModuleObject)//' "'//TRIM(Alphas(1))//'"')
              CALL ShowContinueError('The supplemental heating coil outlet node name must be '// &
                                     'the same as the unitary system outlet node name.')
              CALL ShowContinueError('...Supplemental heating coil outlet node name = '//TRIM(NodeID(SupHeatCoilOutletNode)))
              CALL ShowContinueError('...Unitary system outlet node name            = ' &
                                     //TRIM(NodeID(Furnace(FurnaceNum)%FurnaceOutletNodeNum)))
              ErrorsFound=.TRUE.
          END IF
        END IF

        ! Add component sets array
        IF (Furnace(FurnaceNum)%FanPlace == BlowThru) THEN
          CompSetFanInlet = Alphas(3)
          CompSetCoolInlet = 'UNDEFINED'
        ELSE
          CompSetFanInlet = 'UNDEFINED'
          CompSetCoolInlet = Alphas(3)
        ENDIF
        CALL SetUpCompSets(CurrentModuleObject, Alphas(1), &
                           Alphas(6),Alphas(7),CompSetFanInlet,'UNDEFINED')


        ! Add DX cooling coil to component sets array
        CALL SetUpCompSets(CurrentModuleObject, Alphas(1), &
                           Alphas(10),Alphas(11),CompSetCoolInlet,'UNDEFINED')


        ! Add DX heating coil to component sets array
        CALL SetUpCompSets(CurrentModuleObject, Alphas(1), &
                           Alphas(8),Alphas(9),'UNDEFINED','UNDEFINED')


        ! Add supplemental heating coil to component sets array
        CALL SetUpCompSets(CurrentModuleObject, Alphas(1), &
                           Alphas(12),Alphas(13),'UNDEFINED',Alphas(4))


        Furnace(FurnaceNum)%MaxCoolAirVolFlow       = Numbers(1)
        IF (Furnace(FurnaceNum)%MaxCoolAirVolFlow .LE. 0 .AND. Furnace(FurnaceNum)%MaxCoolAirVolFlow .NE. AutoSize) THEN
          CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
          CALL ShowContinueError('Illegal '//TRIM(cNumericFields(1))//' = '//TRIM(TrimSigDigits(Numbers(1),7)))
          ErrorsFound = .TRUE.
        END IF

        Furnace(FurnaceNum)%MaxHeatAirVolFlow       = Numbers(2)
        IF (Furnace(FurnaceNum)%MaxHeatAirVolFlow .LE. 0 .AND. Furnace(FurnaceNum)%MaxHeatAirVolFlow .NE. AutoSize) THEN
          CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
          CALL ShowContinueError('Illegal '//TRIM(cNumericFields(2))//' = '//TRIM(TrimSigDigits(Numbers(2),7)))
          ErrorsFound = .TRUE.
        END IF

        Furnace(FurnaceNum)%MaxNoCoolHeatAirVolFlow = Numbers(3)
        IF (Furnace(FurnaceNum)%MaxNoCoolHeatAirVolFlow .LT. 0 .AND.Furnace(FurnaceNum)%MaxNoCoolHeatAirVolFlow .NE. AutoSize) THEN
          CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
          CALL ShowContinueError('Illegal '//TRIM(cNumericFields(3))//' = '//TRIM(TrimSigDigits(Numbers(3),7)))
          ErrorsFound = .TRUE.
        END IF

        IF(Furnace(FurnaceNum)%FanSchedPtr .GT. 0)THEN
          IF (.NOT. CheckScheduleValueMinMax(Furnace(FurnaceNum)%FanSchedPtr,'>=',0.0d0,'<=',0.0d0)) THEN !Objexx Range is 0 to 0?
!           set air flow control mode:
!             UseCompressorOnFlow = operate at last cooling or heating air flow requested when compressor is off
!             UseCompressorOffFlow = operate at value specified by user
!           AirFlowControl only valid if fan opmode = ContFanCycComp
            IF (Furnace(FurnaceNum)%MaxNoCoolHeatAirVolFlow .EQ. 0.0d0) THEN
              Furnace(FurnaceNum)%AirFlowControl = UseCompressorOnFlow
            ELSE
              Furnace(FurnaceNum)%AirFlowControl = UseCompressorOffFlow
            END IF
          END IF
        END IF

        IF(Numbers(1) .NE. Autosize .AND. Numbers(2) .NE. Autosize .AND. Numbers(3) .NE. Autosize)THEN
          Furnace(FurnaceNum)%DesignFanVolFlowRate = MAX(Numbers(1),Numbers(2),Numbers(3))
        ELSE
          Furnace(FurnaceNum)%DesignFanVolFlowRate = Autosize
        END IF

        IF (Furnace(FurnaceNum)%HeatingCoilType_Num == Coil_HeatingAirToAirVariableSpeed) THEN
             ErrFlag=.FALSE.
             Furnace(FurnaceNum)%MaxHeatAirVolFlow       &
                = GetCoilAirFlowRateVariableSpeed(HeatingCoilType,HeatingCoilName,ErrFlag)
             Furnace(FurnaceNum)%MaxCoolAirVolFlow       &
                = GetCoilAirFlowRateVariableSpeed(CoolingCoilType,CoolingCoilName,ErrFlag)
             IF (ErrFlag) THEN
                CALL ShowContinueError('...occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
                ErrorsFound=.TRUE.
             ENDIF

            Furnace(FurnaceNum)%MaxNoCoolHeatAirVolFlow = &
                 MIN(Furnace(FurnaceNum)%MaxHeatAirVolFlow,Furnace(FurnaceNum)%MaxCoolAirVolFlow)
            IF(Furnace(FurnaceNum)%MaxHeatAirVolFlow /= Autosize .AND. &
               Furnace(FurnaceNum)%MaxCoolAirVolFlow /= Autosize)THEN
              Furnace(FurnaceNum)%DesignFanVolFlowRate = &
                 MAX(Furnace(FurnaceNum)%MaxHeatAirVolFlow,Furnace(FurnaceNum)%MaxCoolAirVolFlow)
            ELSE
              Furnace(FurnaceNum)%DesignFanVolFlowRate = Autosize
            END IF
        END IF

        IF(FanVolFlowRate .NE. AutoSize)THEN
          IF(FanVolFlowRate .LT. Furnace(FurnaceNum)%MaxCoolAirVolFlow .AND. &
               Furnace(FurnaceNum)%MaxCoolAirVolFlow .NE. AutoSize)THEN
            CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
            CALL ShowContinueError('... air flow rate = '//  &
               TRIM(TrimSigDigits(FanVolFlowRate,7))//&
              ' in fan object '//TRIM(FanName)//' is less than the maximum HVAC system air flow rate in ' &
                              //'cooling mode.')
            CALL ShowContinueError(' The '//TRIM(cNumericFields(1))//' is reset to the' &
                                 //' fan flow rate and the simulation continues.')
            Furnace(FurnaceNum)%MaxCoolAirVolFlow = FanVolFlowRate
            Furnace(FurnaceNum)%DesignFanVolFlowRate = FanVolFlowRate
          END IF
          IF(FanVolFlowRate .LT. Furnace(FurnaceNum)%MaxHeatAirVolFlow .AND. &
               Furnace(FurnaceNum)%MaxHeatAirVolFlow .NE. AutoSize)THEN
            CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
            CALL ShowContinueError('... air flow rate = '//  &
               TRIM(TrimSigDigits(FanVolFlowRate,7))//&
              ' in fan object '//TRIM(FanName)//' is less than the maximum HVAC system air flow rate in ' &
                              //'heating mode.')
            CALL ShowContinueError(' The '//TRIM(cNumericFields(2))//' is reset to the' &
                                 //' fan flow rate and the simulation continues.')
            Furnace(FurnaceNum)%MaxHeatAirVolFlow = FanVolFlowRate
            Furnace(FurnaceNum)%DesignFanVolFlowRate = FanVolFlowRate
          END IF
        END IF

        !Set heating convergence tolerance
        Furnace(FurnaceNum)%HeatingConvergenceTolerance = 0.001d0


        !Set minimum OAT for heat pump compressor operation
        ! get from coil module
        ErrFlag=.FALSE.
        IF (Furnace(FurnaceNum)%HeatingCoilType_Num == Coil_HeatingAirToAirVariableSpeed) THEN
            Furnace(FurnaceNum)%MinOATCompressor = GetVSCoilMinOATCompressor(HeatingCoilName,ErrFlag)
        ELSE
            Furnace(FurnaceNum)%MinOATCompressor =    &
               GetMinOATDXCoilCompressor(HeatingCoilType,HeatingCoilName,ErrFlag)
        END IF
        IF (ErrFlag) THEN
          CALL ShowContinueError('...occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
          ErrorsFound=.TRUE.
        ENDIF


!       Mine heatpump outdoor condenser node from DX coil object
        ErrFlag=.FALSE.
        IF (Furnace(FurnaceNum)%CoolingCoilType_Num == CoilDX_CoolingSingleSpeed) THEN
          Furnace(FurnaceNum)%CondenserNodeNum = &
            GetDXCoilCondenserInletNode(CoolingCoilType,CoolingCoilName,ErrFlag)
        ELSE IF (Furnace(FurnaceNum)%CoolingCoilType_Num == Coil_CoolingAirToAirVariableSpeed) THEN
          Furnace(FurnaceNum)%CondenserNodeNum = GetVSCoilCondenserInletNode(CoolingCoilName,ErrFlag)
        ELSE
          Furnace(FurnaceNum)%CondenserNodeNum = &
            GetDXCoilCondenserInletNode('Coil:Cooling:DX:SingleSpeed', &
            GetHXDXCoilName(CoolingCoilType,CoolingCoilName,ErrFlag), &
            ErrFlag)
        END IF
        IF (ErrFlag) THEN
          CALL ShowContinueError('...occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
          ErrorsFound=.TRUE.
        ENDIF


        IF (Furnace(FurnaceNum)%HeatingCoilType_Num == Coil_HeatingAirToAirVariableSpeed) THEN
          ErrFlag=.FALSE.
          Furnace(FurnaceNum)%DesignHeatingCapacity =   &
             GetCoilCapacityVariableSpeed(HeatingCoilType,HeatingCoilName,ErrFlag)
          IF (ErrFlag) THEN
            CALL ShowContinueError('...occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
            ErrorsFound=.TRUE.
          ENDIF
        ENDIF

        IF (Furnace(FurnaceNum)%CoolingCoilType_Num == Coil_CoolingAirToAirVariableSpeed) THEN
          ErrFlag=.FALSE.
          Furnace(FurnaceNum)%DesignCoolingCapacity =   &
             GetCoilCapacityVariableSpeed(CoolingCoilType,CoolingCoilName,ErrFlag)
          IF (ErrFlag) THEN
            CALL ShowContinueError('...occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
            ErrorsFound=.TRUE.
          ENDIF
        ENDIF

        !Set cooling convergence tolerance
        Furnace(FurnaceNum)%CoolingConvergenceTolerance = 0.001d0

        !Set the furnace max outlet temperature
        Furnace(FurnaceNum)%DesignMaxOutletTemp = Numbers(4)


        !Set maximum supply air temperature for supplemental heating coil
        Furnace(FurnaceNum)%MaxOATSuppHeat = Numbers(5)

      END DO  !End of the Unitary System HeatPump Loop


            !Get the Input for the Water to Air Heat Pump (UnitarySystem:HeatPump:WaterToAir)
      DO HeatPumpNum = 1,  NumWatertoAirHeatPump

        CurrentModuleObject = 'AirLoopHVAC:UnitaryHeatPump:WaterToAir'
        FanInletNode  = 0
        FanOutletNode = 0
        CoolingCoilInletNode  = 0
        CoolingCoilOutletNode = 0
        HeatingCoilInletNode  = 0
        HeatingCoilOutletNode = 0
        SupHeatCoilInletNode  = 0
        SupHeatCoilOutletNode = 0

        FurnaceNum= NumHeatOnly + NumHeatCool + NumUnitaryHeatOnly + NumUnitaryHeatCool + NumHeatPump + HeatPumpNum

        CALL GetObjectItem(CurrentModuleObject,HeatPumpNum,Alphas,NumAlphas,Numbers,NumNumbers,IOStatus, &
                           NumBlank=lNumericBlanks,AlphaBlank=lAlphaBlanks, &
                           AlphaFieldNames=cAlphaFields,NumericFieldNames=cNumericFields)

        IsNotOK=.FALSE.
        IsBlank=.FALSE.
        CALL VerifyName(Alphas(1),Furnace%Name,FurnaceNum-1,IsNotOK,IsBlank,TRIM(CurrentModuleObject)//' Name')
        IF (IsNotOK) THEN
          ErrorsFound=.TRUE.
          IF (IsBlank) Alphas(1)='xxxxx'
        ENDIF
        Furnace(FurnaceNum)%FurnaceType_Num = UnitarySys_HeatPump_WaterToAir
        Furnace(FurnaceNum)%Name            = Alphas(1)
        IF (lAlphaBlanks(2)) THEN
          Furnace(FurnaceNum)%SchedPtr        = ScheduleAlwaysOn
        ELSE
          Furnace(FurnaceNum)%SchedPtr        = GetScheduleIndex(Alphas(2))
          IF (Furnace(FurnaceNum)%SchedPtr == 0) THEN
            CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
            CALL ShowContinueError('Illegal '//TRIM(cAlphaFields(2))//' = '//TRIM(Alphas(2)))
            ErrorsFound=.TRUE.
          ENDIF
        ENDIF

        Furnace(FurnaceNum)%FurnaceInletNodeNum = &
               GetOnlySingleNode(Alphas(3),ErrorsFound,TRIM(CurrentModuleObject),Alphas(1), &
                            NodeType_Air,NodeConnectionType_Inlet,1,ObjectIsParent)


        Furnace(FurnaceNum)%FurnaceOutletNodeNum = &
               GetOnlySingleNode(Alphas(4),ErrorsFound,TRIM(CurrentModuleObject),Alphas(1), &
                            NodeType_Air,NodeConnectionType_Outlet,1,ObjectIsParent)


        CALL TestCompSet(CurrentModuleObject,Alphas(1),Alphas(3),Alphas(4),'Air Nodes')


        !Get the Controlling Zone or Location of the Furnace Thermostat
        Furnace(FurnaceNum)%ControlZoneNum = FindItemInList(Alphas(5),Zone%Name,NumOfZones)
        IF (Furnace(FurnaceNum)%ControlZoneNum == 0) THEN
          CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
          CALL ShowContinueError('Illegal '//TRIM(cAlphaFields(5))//' = '//TRIM(Alphas(5)))
          ErrorsFound=.TRUE.
        ENDIF

        TotalZonesOnAirLoop = 0

        ! Get the node number for the zone with the thermostat
        IF (Furnace(FurnaceNum)%ControlZoneNum >  0) THEN
          AirNodeFound=.FALSE.
          AirLoopFound=.FALSE.
          DO ControlledZoneNum = 1,NumOfZones
            IF (ZoneEquipConfig(ControlledZoneNum)%ActualZoneNum /= Furnace(FurnaceNum)%ControlZoneNum) CYCLE
!             Find the controlled zone number for the specified thermostat location
              Furnace(FurnaceNum)%NodeNumofControlledZone=ZoneEquipConfig(ControlledZoneNum)%ZoneNode
!             Determine if furnace is on air loop served by the thermostat location specified
              AirLoopNumber = ZoneEquipConfig(ControlledZoneNum)%AirLoopNum
              IF(AirLoopNumber .GT. 0)THEN
                DO BranchNum = 1, PrimaryAirSystem(AirLoopNumber)%NumBranches
                  DO CompNum = 1, PrimaryAirSystem(AirLoopNumber)%Branch(BranchNum)%TotalComponents
                    IF(.NOT. SameString(PrimaryAirSystem(AirLoopNumber)%Branch(BranchNum)%Comp(CompNum)%Name, &
                             Alphas(1)) .OR. &
                       .NOT. SameString(PrimaryAirSystem(AirLoopNumber)%Branch(BranchNum)%Comp(CompNum)%TypeOf, &
                             CurrentModuleObject))CYCLE
                    AirLoopFound=.TRUE.
                    EXIT
                  END DO
                  IF(AirLoopFound)EXIT
                END DO
                DO TstatZoneNum = 1, NumTempControlledZones
                  IF(TempControlledZone(TstatZoneNum)%ActualZoneNum .NE. Furnace(FurnaceNum)%ControlZoneNum)CYCLE
                  AirNodeFound=.TRUE.
                END DO
                DO TstatZoneNum = 1, NumComfortControlledZones
                  IF(ComfortControlledZone(TstatZoneNum)%ActualZoneNum .NE. Furnace(FurnaceNum)%ControlZoneNum)CYCLE
                  AirNodeFound=.TRUE.
                END DO
              ELSE
                CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
                CALL ShowContinueError('Did not find an AirLoopHVAC.')
                CALL ShowContinueError('Specified '//TRIM(cAlphaFields(5))//' = '//TRIM(Alphas(5)))
                ErrorsFound=.TRUE.
              END IF
            EXIT
          ENDDO
          IF (.not. AirNodeFound) THEN
            CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
            CALL ShowContinueError('Did not find air node (zone with thermostat).')
            CALL ShowContinueError('Specified '//TRIM(cAlphaFields(5))//' = '//TRIM(Alphas(5)))
            CALL ShowContinueError('Both a ZoneHVAC:EquipmentConnections object and a ZoneControl:Thermostat object' &
                                //' must be specified for this zone.')
            ErrorsFound=.TRUE.
          ENDIF
          IF (.not. AirLoopFound) THEN
            CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
            CALL ShowSevereError('Did not find correct AirLoopHVAC.')
            CALL ShowContinueError('Specified '//TRIM(cAlphaFields(5))//' = '//TRIM(Alphas(5)))
            ErrorsFound=.TRUE.
          ENDIF
          IF(AirLoopNumber .GT. 0)THEN
            DO ControlledZoneNum = 1,NumOfZones
              IF (ZoneEquipConfig(ControlledZoneNum)%AirLoopNum == AirLoopNumber) THEN
                TotalZonesOnAirLoop = TotalZonesOnAirLoop + 1
              END IF
            END DO
          END IF
        ENDIF


         !Get fan data
        FanType = Alphas(6)
        FanName = Alphas(7)
        ErrFlag=.FALSE.
        CALL GetFanType(FanName, Furnace(FurnaceNum)%FanType_Num, ErrFlag,   &
           CurrentModuleObject,Alphas(1))
        IF (ErrFlag) THEN
          ErrorsFound=.TRUE.
        END IF

        IF (Furnace(FurnaceNum)%FanType_Num == FanType_SimpleOnOff)THEN
          CALL ValidateComponent(FanType,FanName,IsNotOK, TRIM(CurrentModuleObject))
          IF (IsNotOK) THEN
            CALL ShowContinueError('...occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
            ErrorsFound=.TRUE.
          ELSE
            ErrFlag=.FALSE.
            CALL GetFanIndex(FanName, Furnace(FurnaceNum)%FanIndex, ErrFlag)
            IF (ErrFlag) THEN
              CALL ShowContinueError('...occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
              ErrorsFound=.TRUE.
            ENDIF
            ErrFlag=.FALSE.
            FanInletNode  = GetFanInletNode(FanType,FanName,ErrFlag)
            IF (ErrFlag) THEN
              CALL ShowContinueError('...occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
              ErrorsFound=.TRUE.
            ENDIF
            ErrFlag=.FALSE.
            FanOutletNode = GetFanOutletNode(FanType,FanName,ErrFlag)
            IF (ErrFlag) THEN
              CALL ShowContinueError('...occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
              ErrorsFound=.TRUE.
            ENDIF
            ErrFlag=.FALSE.
            Furnace(FurnaceNum)%FanAvailSchedPtr = GetFanAvailSchPtr(FanType,FanName,ErrFlag)
            IF (ErrFlag) THEN
              CALL ShowContinueError('...occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
              ErrorsFound=.TRUE.
            ENDIF
          ENDIF
        ELSE
          CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
          CALL ShowContinueError('Illegal '//TRIM(cAlphaFields(6))//' = '//TRIM(Alphas(6)))
          ErrorsFound=.TRUE.
        END IF


            !Get heating coil type and name data
        IF (Alphas(8) == 'COIL:HEATING:WATERTOAIRHEATPUMP:PARAMETERESTIMATION' )THEN
          HeatingCoilType = Alphas(8)
          Furnace(FurnaceNum)%HeatingCoilType_Num = Coil_HeatingWaterToAirHP
          HeatingCoilName = Alphas(9)
          CALL ValidateComponent(HeatingCoilType,HeatingCoilName,IsNotOK, &
                                 TRIM(CurrentModuleObject))
          IF (IsNotOK) THEN
            CALL ShowContinueError('...occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
            ErrorsFound=.TRUE.
          ELSE
            Furnace(FurnaceNum)%HeatingCoilIndex = GetWtoAHPCoilIndex(HeatingCoilType, HeatingCoilName,ErrFlag)
            HeatingCoilInletNode=GetWtoAHPCoilInletNode(HeatingCoilType, HeatingCoilName,ErrFlag)
            HeatingCoilOutletNode=GetWtoAHPCoilOutletNode(HeatingCoilType, HeatingCoilName,ErrFlag)
          ENDIF
        ELSEIF (Alphas(8) == 'COIL:HEATING:WATERTOAIRHEATPUMP:EQUATIONFIT' )THEN
          HeatingCoilType = Alphas(8)
          Furnace(FurnaceNum)%HeatingCoilType_Num = Coil_HeatingWaterToAirHPSimple
          HeatingCoilName = Alphas(9)
          CALL ValidateComponent(HeatingCoilType,HeatingCoilName,IsNotOK,  &
                                 TRIM(CurrentModuleObject))
          IF (IsNotOK) THEN
            CALL ShowContinueError('...occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
            ErrorsFound=.TRUE.
          ELSE
            Furnace(FurnaceNum)%HeatingCoilIndex = GetWtoAHPSimpleCoilIndex(HeatingCoilType, HeatingCoilName,ErrFlag)
            HeatingCoilInletNode=GetWtoAHPSimpleCoilInletNode(HeatingCoilType, HeatingCoilName,ErrFlag)
            HeatingCoilOutletNode=GetWtoAHPSimpleCoilOutletNode(HeatingCoilType, HeatingCoilName,ErrFlag)
          ENDIF
        ELSEIF (Alphas(8) == 'COIL:HEATING:WATERTOAIRHEATPUMP:VARIABLESPEEDEQUATIONFIT' )THEN
          HeatingCoilType = Alphas(8)
          Furnace(FurnaceNum)%HeatingCoilType_Num = Coil_HeatingWaterToAirHPVSEquationFit
          HeatingCoilName = Alphas(9)
          CALL ValidateComponent(HeatingCoilType,HeatingCoilName,IsNotOK,  &
                                 TRIM(CurrentModuleObject))
          IF (IsNotOK) THEN
            CALL ShowContinueError('...occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
            ErrorsFound=.TRUE.
          ELSE
            Furnace(FurnaceNum)%HeatingCoilIndex = GetCoilIndexVariableSpeed(HeatingCoilType, HeatingCoilName,ErrFlag)
            HeatingCoilInletNode=GetCoilInletNodeVariableSpeed(HeatingCoilType, HeatingCoilName,ErrFlag)
            HeatingCoilOutletNode=GetCoilOutletNodeVariableSpeed(HeatingCoilType, HeatingCoilName,ErrFlag)
          ENDIF
        ELSE
          CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
          CALL ShowContinueError('Illegal '//TRIM(cAlphaFields(8))//' = '//TRIM(Alphas(8)))
          ErrorsFound=.TRUE.
        END IF


        ! Get Cooling Coil Information if available
         IF (Alphas(10) == 'COIL:COOLING:WATERTOAIRHEATPUMP:PARAMETERESTIMATION' )THEN
          CoolingCoilType = Alphas(10)
          Furnace(FurnaceNum)%CoolingCoilType_Num = Coil_CoolingWaterToAirHP
          CoolingCoilName = Alphas(11)
          CALL ValidateComponent(CoolingCoilType,CoolingCoilName,IsNotOK,  &
                                 TRIM(CurrentModuleObject))
          IF (IsNotOK) THEN
            CALL ShowContinueError('...occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
            ErrorsFound=.TRUE.
          ELSE
            Furnace(FurnaceNum)%CoolingCoilIndex = GetWtoAHPCoilIndex(CoolingCoilType, CoolingCoilName,ErrFlag)
            CoolingCoilInletNode=GetWtoAHPCoilInletNode(CoolingCoilType,  &
                                                          CoolingCoilName,ErrFlag)
            CoolingCoilOutletNode=GetWtoAHPCoilOutletNode(CoolingCoilType,  &
                                                          CoolingCoilName,ErrFlag)
          ENDIF
        ELSEIF (Alphas(10) == 'COIL:COOLING:WATERTOAIRHEATPUMP:EQUATIONFIT' )THEN
          CoolingCoilType = Alphas(10)
          Furnace(FurnaceNum)%CoolingCoilType_Num = Coil_CoolingWaterToAirHPSimple
          CoolingCoilName = Alphas(11)
          CALL ValidateComponent(CoolingCoilType,CoolingCoilName,IsNotOK,  &
                                 TRIM(CurrentModuleObject))
          IF (IsNotOK) THEN
            CALL ShowContinueError('...occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
            ErrorsFound=.TRUE.
          ELSE
            Furnace(FurnaceNum)%CoolingCoilIndex = GetWtoAHPSimpleCoilIndex(CoolingCoilType, CoolingCoilName,ErrFlag)
            CoolingCoilInletNode=GetWtoAHPSimpleCoilInletNode(CoolingCoilType,  &
                                                                CoolingCoilName,ErrFlag)
            CoolingCoilOutletNode=GetWtoAHPSimpleCoilOutletNode(CoolingCoilType,  &
                                                                CoolingCoilName,ErrFlag)
          ENDIF
        ELSEIF (Alphas(10) == 'COIL:COOLING:WATERTOAIRHEATPUMP:VARIABLESPEEDEQUATIONFIT' )THEN
          CoolingCoilType = Alphas(10)
          Furnace(FurnaceNum)%CoolingCoilType_Num = Coil_CoolingWaterToAirHPVSEquationFit
          CoolingCoilName = Alphas(11)
          CALL ValidateComponent(CoolingCoilType,CoolingCoilName,IsNotOK,  &
                                 TRIM(CurrentModuleObject))
          IF (IsNotOK) THEN
            CALL ShowContinueError('...occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
            ErrorsFound=.TRUE.
          ELSE
            Furnace(FurnaceNum)%CoolingCoilIndex = GetCoilIndexVariableSpeed(CoolingCoilType, CoolingCoilName,ErrFlag)
            CoolingCoilInletNode=GetCoilInletNodeVariableSpeed(CoolingCoilType,  &
                                                                CoolingCoilName,ErrFlag)
            CoolingCoilOutletNode=GetCoilOutletNodeVariableSpeed(CoolingCoilType,  &
                                                                CoolingCoilName,ErrFlag)
          ENDIF
        ELSE
          CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
          CALL ShowContinueError('Illegal '//TRIM(cAlphaFields(10))//' = '//TRIM(Alphas(10)))
          ErrorsFound=.TRUE.
        END IF

        IF (NumAlphas >= 18) THEN
          ! get water flow mode info before CALL SetSimpleWSHPData
          IF (SameString(Alphas(18),'Constant'))   Furnace(FurnaceNum)%WaterCyclingMode = WaterConstant
          IF (SameString(Alphas(18),'Cycling'))   Furnace(FurnaceNum)%WaterCyclingMode = WaterCycling
          IF (SameString(Alphas(18),'ConstantOnDemand'))   Furnace(FurnaceNum)%WaterCyclingMode = WaterConstantOnDemand
             !default to draw through if not specified in input
          IF (lAlphaBlanks(18))           Furnace(FurnaceNum)%WaterCyclingMode = WaterCycling
        ELSE
          Furnace(FurnaceNum)%WaterCyclingMode = WaterCycling
        ENDIF
        IF (Furnace(FurnaceNum)%WaterCyclingMode .EQ. 0) THEN
          CALL ShowSevereError(TRIM(CurrentModuleObject)//' illegal '//TRIM(cAlphaFields(18))//' = '//TRIM(Alphas(18)))
          CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Furnace(FurnaceNum)%Name))
          ErrorsFound = .TRUE.
        END IF

        ! end get water flow mode info
        IF (Alphas(8) == 'COIL:HEATING:WATERTOAIRHEATPUMP:EQUATIONFIT' .AND.   &
            Alphas(10) == 'COIL:COOLING:WATERTOAIRHEATPUMP:EQUATIONFIT') THEN
          Furnace(FurnaceNum)%WatertoAirHPType = WatertoAir_Simple
          CALL SetSimpleWSHPData(Furnace(FurnaceNum)%CoolingCoilIndex,ErrorsFound,Furnace(FurnaceNum)%WaterCyclingMode, &
                                CompanionHeatingCoilNum=Furnace(FurnaceNum)%HeatingCoilIndex)
        ELSE IF(Alphas(8) == 'COIL:HEATING:WATERTOAIRHEATPUMP:PARAMETERESTIMATION' .AND. &
                Alphas(10) == 'COIL:COOLING:WATERTOAIRHEATPUMP:PARAMETERESTIMATION') THEN
          Furnace(FurnaceNum)%WatertoAirHPType = WatertoAir_ParEst
        ELSE IF(Alphas(8) == 'COIL:HEATING:WATERTOAIRHEATPUMP:VARIABLESPEEDEQUATIONFIT' .AND. &
                Alphas(10) == 'COIL:COOLING:WATERTOAIRHEATPUMP:VARIABLESPEEDEQUATIONFIT') THEN
          Furnace(FurnaceNum)%WatertoAirHPType = WatertoAir_VarSpeedEquationFit
          CALL SetVarSpeedCoilData(Furnace(FurnaceNum)%CoolingCoilIndex,ErrorsFound, &
                                CompanionHeatingCoilNum=Furnace(FurnaceNum)%HeatingCoilIndex)
        ELSE
          CALL ShowContinueError('For '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
          CALL ShowContinueError('Cooling coil and heating coil should be of same general type')
          ErrorsFound = .TRUE.
        END IF

        ! Get supplemental heating coil information

        SuppHeatCoilType = Alphas(12)
        SuppHeatCoilName = Alphas(13)
        Furnace(FurnaceNum)%SuppHeatCoilType = SuppHeatCoilType
        Furnace(FurnaceNum)%SuppHeatCoilName = SuppHeatCoilName
        ErrFlag = .FALSE.
        IF (SameString(SuppHeatCoilType,'Coil:Heating:Gas')      .OR. &
              SameString(SuppHeatCoilType,'Coil:Heating:Electric') ) THEN

            Furnace(FurnaceNum)%SuppHeatCoilType_Num = GetHeatingCoilTypeNum(SuppHeatCoilType,SuppHeatCoilName,ErrFlag)
            IF (ErrFlag) THEN
               CALL ShowContinueError('...occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
               ErrorsFound=.TRUE.
            ELSE
              IsNotOK = .FALSE.
              CALL ValidateComponent(SuppHeatCoilType,SuppHeatCoilName,IsNotOK,TRIM(CurrentModuleObject))
              IF (IsNotOK) THEN
                CALL ShowContinueError('In '//TRIM(CurrentModuleObject)//' "'//TRIM(Alphas(1))//'"')
                ErrorsFound=.TRUE.

               ELSE ! mine data from the supplemental heating coil

                  CALL GetHeatingCoilIndex(SuppHeatCoilName,Furnace(FurnaceNum)%SuppHeatCoilIndex,IsNotOK)
                  IF (IsNotOK) THEN
                    CALL ShowContinueError('...occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
                    ErrorsFound=.TRUE.
                  ENDIF

                  ! Get the Supplemental Heating Coil Inlet Node Number
                  ErrFlag=.FALSE.
                  SupHeatCoilInletNode = GetHeatingCoilInletNode(SuppHeatCoilType,SuppHeatCoilName,ErrFlag)
                  IF (ErrFlag) THEN
                     CALL ShowContinueError('...occurs in '//TRIM(CurrentModuleObject)//' "'//TRIM(Alphas(1))//'"')
                     ErrorsFound=.TRUE.
                  ENDIF

                  ! Get the Supplemental Heating Coil Outlet Node Number
                  ErrFlag=.FALSE.
                  SupHeatCoilOutletNode = GetHeatingCoilOutletNode(SuppHeatCoilType,SuppHeatCoilName,ErrFlag)
                  IF (ErrFlag) THEN
                    CALL ShowContinueError('...occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
                    ErrorsFound=.TRUE.
                  ENDIF

                  ! Get the supplemental heating coil design capacity
                  ErrFlag=.FALSE.
                  Furnace(FurnaceNum)%DesignSuppHeatingCapacity =   &
                     GetHeatingCoilCapacity(SuppHeatCoilType,SuppHeatCoilName,ErrFlag)
                  IF (ErrFlag) THEN
                    CALL ShowContinueError('...occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
                    ErrorsFound=.TRUE.
                  ENDIF

               ENDIF ! IF (IsNotOK) THEN
            ENDIF
        ELSEIF (SameString(SuppHeatCoilType,'Coil:Heating:Water')) THEN
            Furnace(FurnaceNum)%SuppHeatCoilType_Num = Coil_HeatingWater
            CALL ValidateComponent(SuppHeatCoilType,SuppHeatCoilName,IsNotOK,TRIM(CurrentModuleObject))
            IF (IsNotOK) THEN
                CALL ShowContinueError('...occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
                ErrorsFound=.TRUE.
            ELSE ! mine data from heating coil object

                ! Get the Heating Coil water Inlet or control Node number
                ErrFlag = .FALSE.
                Furnace(FurnaceNum)%SuppCoilControlNode = GetCoilWaterInletNode('Coil:Heating:Water',SuppHeatCoilName,ErrFlag)
                IF(ErrFlag)THEN
                  CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Furnace(FurnaceNum)%Name))
                  ErrorsFound = .TRUE.
                END IF

                ! Get the ReHeat Coil hot water max volume flow rate
                ErrFlag = .FALSE.
                Furnace(FurnaceNum)%MaxSuppCoilFluidFlow = GetCoilMaxWaterFlowRate('Coil:Heating:Water',  &
                                                           SuppHeatCoilName,ErrFlag)
                IF(ErrFlag)THEN
                  CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Furnace(FurnaceNum)%Name))
                  ErrorsFound = .TRUE.
                END IF

                ! Get the ReHeat Coil Inlet Node
                ErrFlag = .FALSE.
                SupHeatCoilInletNode =  GetWaterCoilInletNode('Coil:Heating:Water',SuppHeatCoilName,ErrFlag)
                Furnace(FurnaceNum)%SuppCoilAirInletNode = SupHeatCoilInletNode
                IF(ErrFlag)THEN
                  CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Furnace(FurnaceNum)%Name))
                  ErrorsFound = .TRUE.
                END IF

                ! Get the ReHeat Coil Outlet Node
                ErrFlag = .FALSE.
                SupHeatCoilOutletNode = GetWaterCoilOutletNode('Coil:Heating:Water',SuppHeatCoilName,ErrFlag)
                Furnace(FurnaceNum)%SuppCoilAirOutletNode = SupHeatCoilOutletNode
                IF(ErrFlag)THEN
                  CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Furnace(FurnaceNum)%Name))
                  ErrorsFound = .TRUE.
                END IF
                ! check if user has also used a water coil controller, which they should not do
                ErrFlag = .FALSE.
                CALL CheckCoilWaterInletNode(Furnace(FurnaceNum)%CoilControlNode, ErrFlag)
                IF (.NOT. ErrFlag) THEN ! then did find a controller so that is bad
                  CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(Furnace(FurnaceNum)%Name) &
                                      //' has a conflicting Controller:WaterCoil object' )
                  CALL ShowContinueError('Hot water coils are controlled directly by unitary and furnace systems.')
                  CALL ShowContinueError('No water coil controller should be input for the coil.')
                  ErrorsFound = .TRUE.
                ENDIF


            ENDIF

        ELSEIF (SameString(SuppHeatCoilType,'Coil:Heating:Steam')) THEN
            Furnace(FurnaceNum)%SuppHeatCoilType_Num = Coil_HeatingSteam
            CALL ValidateComponent(SuppHeatCoilType,SuppHeatCoilName,IsNotOK,TRIM(CurrentModuleObject))
            IF (IsNotOK) THEN
                CALL ShowContinueError('...occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
                ErrorsFound=.TRUE.
            ELSE ! mine data from heating coil object

                ErrFlag = .FALSE.
                Furnace(FurnaceNum)%SuppHeatCoilIndex = GetSTeamCoilIndex(SuppHeatCoilType,SuppHeatCoilName,ErrFlag)
                IF (Furnace(FurnaceNum)%SuppHeatCoilIndex .EQ. 0) THEN
                    CALL ShowSevereError(TRIM(CurrentModuleObject)//' illegal '//TRIM(cAlphaFields(12))//' = ' &
                                  //TRIM(SuppHeatCoilName))
                    CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Furnace(FurnaceNum)%Name))
                    ErrorsFound = .TRUE.
                END IF

                ! Get the Heating Coil steam inlet node number
                ErrFlag = .FALSE.
                Furnace(FurnaceNum)%SuppCoilControlNode = GetSteamCoilSteamInletNode('Coil:Heating:Steam',SuppHeatCoilName,ErrFlag)
                IF(ErrFlag)THEN
                  CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Furnace(FurnaceNum)%Name))
                  ErrorsFound = .TRUE.
                END IF

                ! Get the Heating Coil steam max volume flow rate
                Furnace(FurnaceNum)%MaxSuppCoilFluidFlow = GetCoilMaxSteamFlowRate(Furnace(FurnaceNum)%SuppHeatCoilIndex,ErrFlag)
                IF (Furnace(FurnaceNum)%MaxSuppCoilFluidFlow .GT. 0.0d0)THEN
                   SteamIndex = 0      ! Function GetSatDensityRefrig will look up steam index if 0 is passed
                   SteamDensity=GetSatDensityRefrig("STEAM",TempSteamIn,1.0d0,SteamIndex,'GetAirLoopHVACHeatCoolInput')
                   Furnace(FurnaceNum)%MaxSuppCoilFluidFlow = &
                                     GetCoilMaxSteamFlowRate(Furnace(FurnaceNum)%SuppHeatCoilIndex,ErrFlag) * SteamDensity
                END IF

                ! Get the Heating Coil Inlet Node
                ErrFlag = .FALSE.
                SupHeatCoilInletNode = &
                         GetSteamCoilAirInletNode(Furnace(FurnaceNum)%SuppHeatCoilIndex,SuppHeatCoilName,ErrFlag)
                Furnace(FurnaceNum)%SuppCoilAirInletNode = SupHeatCoilInletNode
                IF(ErrFlag)THEN
                  CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Furnace(FurnaceNum)%Name))
                  ErrorsFound = .TRUE.
                END IF

                ! Get the Heating Coil Outlet Node
                ErrFlag = .FALSE.
                SupHeatCoilOutletNode = &
                         GetSteamCoilAirOutletNode(Furnace(FurnaceNum)%SuppHeatCoilIndex,SuppHeatCoilName,ErrFlag)
                Furnace(FurnaceNum)%SuppCoilAirOutletNode = SupHeatCoilOutletNode
                IF(ErrFlag)THEN
                  CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Furnace(FurnaceNum)%Name))
                  ErrorsFound = .TRUE.
                END IF

            ENDIF

        ELSE
          CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
          CALL ShowContinueError('Illegal '//TRIM(cAlphaFields(12))//' = '//TRIM(Alphas(12)))
          ErrorsFound=.TRUE.
        END IF ! IF (Furnace(FurnaceNum)%HeatingCoilType_Num == Coil_HeatingGas .OR. &, etc.

        IF (lAlphaBlanks(14)) THEN
          Furnace(FurnaceNum)%CondenserNodeNum = 0
        ELSE
          Furnace(FurnaceNum)%CondenserNodeNum = &
              GetOnlySingleNode(Alphas(14),ErrorsFound,TRIM(CurrentModuleObject),Alphas(1), &
                                NodeType_Air,NodeConnectionType_OutsideAirReference,1,ObjectIsNotParent)
         ! need better verification.
         IF (.not. CheckOutAirNodeNumber(Furnace(FurnaceNum)%CondenserNodeNum)) THEN
            CALL ShowSevereError('For '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
            CALL ShowContinueError(' Node name of outdoor dry-bulb temperature sensor not valid outdoor air node= '//  &
              TRIM(Alphas(14)))
            CALL ShowContinueError('...does not appear in an OutdoorAir:NodeList or as an OutdoorAir:Node.')
           ErrorsFound=.TRUE.
         END IF
        ENDIF

        IF (SameString(Alphas(15),'BlowThrough'))  Furnace(FurnaceNum)%FanPlace = BlowThru
        IF (SameString(Alphas(15),'DrawThrough'))  Furnace(FurnaceNum)%FanPlace = DrawThru
        IF (Furnace(FurnaceNum)%FanPlace .EQ.0) THEN
          CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
          CALL ShowContinueError('Illegal '//TRIM(cAlphaFields(15))//' = '//TRIM(Alphas(15)))
          ErrorsFound = .TRUE.
        END IF

        Furnace(FurnaceNum)%FanSchedPtr     = GetScheduleIndex(Alphas(16))
        IF (.NOT. lAlphaBlanks(16) .AND. Furnace(FurnaceNum)%FanSchedPtr == 0) THEN
          CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
          CALL ShowContinueError('Illegal '//TRIM(cAlphaFields(16))//' = '//TRIM(Alphas(16)))
          ErrorsFound=.TRUE.
        ELSEIF (lAlphaBlanks(16)) THEN
          Furnace(FurnaceNum)%OpMode = CycFanCycCoil
          IF(Furnace(FurnaceNum)%FanType_Num /= FanType_SimpleOnOff)THEN
            CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(Furnace(FurnaceNum)%Name))
            CALL ShowContinueError(TRIM(cAlphaFields(6))//' = '//TRIM(Alphas(6)))
            CALL ShowContinueError('Fan type must be Fan:OnOff when '//  &
                                   TRIM(cAlphaFields(16))//' = Blank.')
            ErrorsFound=.TRUE.
          END IF
        ENDIF

        ! add the Dehumidification Type
        IF (SameString(Alphas(17),'None') .OR. SameString(Alphas(17),'CoolReheat'))THEN
          AirNodeFound=.FALSE.
          IF(SameString(Alphas(17),'CoolReheat'))THEN
            Furnace(FurnaceNum)%DehumidControlType_Num = DehumidControl_CoolReheat
            Furnace(FurnaceNum)%Humidistat = .TRUE.
            IF(lAlphaBlanks(17))THEN
              CALL ShowWarningError(TRIM(CurrentModuleObject)//' "'//TRIM(Alphas(1))//'"')
              CALL ShowContinueError('Dehumidification control type is assumed to be None since a supplemental reheat coil '// &
                                     'has not been specified and the simulation continues.')
              Furnace(FurnaceNum)%Humidistat = .FALSE.
              Furnace(FurnaceNum)%DehumidControlType_Num = DehumidControl_None
            END IF
          END IF
          IF(SameString(Alphas(17),'None'))THEN
            Furnace(FurnaceNum)%DehumidControlType_Num = DehumidControl_None
            Furnace(FurnaceNum)%Humidistat = .FALSE.
          END IF
          IF(Furnace(FurnaceNum)%Humidistat)THEN
            DO HstatZoneNum = 1, NumHumidityControlZones
              IF(HumidityControlZone(HstatZoneNum)%ActualZoneNum .NE. Furnace(FurnaceNum)%ControlZoneNum)CYCLE
              AirNodeFound=.TRUE.
            END DO
            IF (.not. AirNodeFound) THEN
              CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
              CALL ShowContinueError('Did not find Air Node (Zone with Humidistat).')
              CALL ShowContinueError('Specified '//TRIM(cAlphaFields(5))//' = '//TRIM(Alphas(5)))
              ErrorsFound=.TRUE.
            ENDIF
          END IF
        ELSE ! invalid input or blank
          IF (.NOT. lAlphaBlanks(17)) THEN
            CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
            CALL ShowContinueError('Illegal '//TRIM(cAlphaFields(17))//' = '//TRIM(Alphas(17)))
            ErrorsFound=.TRUE.
          ELSE
            Furnace(FurnaceNum)%Humidistat = .FALSE.
            Furnace(FurnaceNum)%DehumidControlType_Num = DehumidControl_None
          ENDIF
        END IF

        ! Add fan to component sets array

        IF (Furnace(FurnaceNum)%FanPlace == BlowThru) THEN
          CompSetFanInlet = Alphas(3)
          CompSetCoolInlet = 'UNDEFINED'
          IF (FanInletNode /= Furnace(FurnaceNum)%FurnaceInletNodeNum) THEN
            CALL ShowSevereError('For '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1))//&
                                ', Mismatch between unitary system inlet node and fan inlet node.')
            CALL ShowContinueError('..For "BlowThrough" fan, the inlet node name for the HeatPump should match the '// &
                                   'fan inlet node name.')
            CALL ShowContinueError('..HeatPump Inlet Node = '//TRIM(NodeID(Furnace(FurnaceNum)%FurnaceInletNodeNum)))
            CALL ShowContinueError('..Fan Inlet Node      = '//TRIM(NodeID(FanInletNode)))
            ErrorsFound=.TRUE.
          ENDIF
          IF (FanOutletNode /= CoolingCoilInletNode) THEN
            CALL ShowSevereError('For '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1))//&
                                ', Mismatch between fan outlet node and cooling coil inlet node.')
            CALL ShowContinueError('..For "BlowThrough" fan, the fan outlet node name must match the cooling coil '// &
                                   'inlet node name.')
            CALL ShowContinueError('..Fan outlet node         = '//TRIM(NodeID(FanOutletNode)))
            CALL ShowContinueError('..Cooling coil inlet node = '//TRIM(NodeID(CoolingCoilInletNode)))
            ErrorsFound=.TRUE.
          ENDIF
          IF (CoolingCoilOutletNode /= HeatingCoilInletNode) THEN
            CALL ShowSevereError('For '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1))//&
                                ', Mismatch between cooling coil outlet node and heating coil inlet node.')
            CALL ShowContinueError('..The cooling coil outlet node name must match the heating coil inlet node name.')
            CALL ShowContinueError('..Cooling coil outlet node = '//TRIM(NodeID(CoolingCoilOutletNode)))
            CALL ShowContinueError('..Heating coil inlet node  = '//TRIM(NodeID(HeatingCoilInletNode)))
            ErrorsFound=.TRUE.
          ENDIF
          IF (HeatingCoilOutletNode /= SupHeatCoilInletNode) THEN
            CALL ShowSevereError('For '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1))//&
                                ', Mismatch between heating coil outlet node and supplemental heating coil inlet node.')
            CALL ShowContinueError('..For "BlowThrough" fan, the heating coil outlet node name must match the supplemental '// &
                                   'heating coil inlet node name.')
            CALL ShowContinueError('..Heating coil outlet node             = '//TRIM(NodeID(HeatingCoilOutletNode)))
            CALL ShowContinueError('..Supplemental heating coil inlet node = '//TRIM(NodeID(SupHeatCoilInletNode)))
            ErrorsFound=.TRUE.
          ENDIF
          IF (SupHeatCoilOutletNode /= Furnace(FurnaceNum)%FurnaceOutletNodeNum) THEN
            CALL ShowSevereError('For '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1))//&
                                ', Mismatch between supplemental heating coil outlet node and HeatPump outlet node.')
            CALL ShowContinueError('..The supplemental heating coil outlet node name must match the '// &
                                   'HeatPump outlet node name.')
            CALL ShowContinueError('..Supplemental heating coil outlet node = '//TRIM(NodeID(SupHeatCoilOutletNode)))
            CALL ShowContinueError('..HeatPump outlet node                  = ' &
                                                            //TRIM(NodeID(Furnace(FurnaceNum)%FurnaceOutletNodeNum)))
            ErrorsFound=.TRUE.
          ENDIF
        ELSE
          CompSetFanInlet = 'UNDEFINED'
          CompSetCoolInlet = Alphas(3)
          IF (CoolingCoilInletNode /= Furnace(FurnaceNum)%FurnaceInletNodeNum) THEN
            CALL ShowSevereError('For '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1))//&
                                ', Mismatch between unitary system inlet node and cooling coil inlet node.')
            CALL ShowContinueError('..For "DrawThrough" fan, the inlet node name for the HeatPump should match'//  &
                                   ' the cooling coil inlet node name.')
            CALL ShowContinueError('..HeatPump inlet node     = '//TRIM(NodeID(Furnace(FurnaceNum)%FurnaceInletNodeNum)))
            CALL ShowContinueError('..Cooling coil inlet node = '//TRIM(NodeID(CoolingCoilInletNode)))
            ErrorsFound=.TRUE.
          ENDIF
          IF (CoolingCoilOutletNode /= HeatingCoilInletNode) THEN
            CALL ShowSevereError('For '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1))//&
                                ', Mismatch between cooling coil outlet node and heating coil inlet node.')
            CALL ShowContinueError('..The outlet node name for the cooling coil should match'//  &
                                   ' the heating coil inlet node name.')
            CALL ShowContinueError('..Cooling coil outlet node = '//TRIM(NodeID(CoolingCoilOutletNode)))
            CALL ShowContinueError('..Heating coil inlet node  = '//TRIM(NodeID(HeatingCoilInletNode)))
            ErrorsFound=.TRUE.
          ENDIF
          IF (HeatingCoilOutletNode /= FanInletNode) THEN
            CALL ShowSevereError('For '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1))//&
                                ', Mismatch between heating coil outlet node and fan inlet node.')
            CALL ShowContinueError('..For "DrawThrough" fan, the outlet node name for the heating coil should match'//  &
                                   ' the fan inlet node name.')
            CALL ShowContinueError('..Heating coil outlet node = '//TRIM(NodeID(HeatingCoilOutletNode)))
            CALL ShowContinueError('..Fan inlet node           = '//TRIM(NodeID(FanInletNode)))
            ErrorsFound=.TRUE.
          ENDIF
          IF (FanOutletNode /= SupHeatCoilInletNode) THEN
            CALL ShowSevereError('For '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1))//&
                                ', Mismatch between fan outlet node and supplemental heating coil inlet node.')
            CALL ShowContinueError('..For "DrawThrough" fan, the outlet node name for the fan should match the '// &
                                   'supplemental heating coil inlet node name.')
            CALL ShowContinueError('..Fan outlet node                      = '//TRIM(NodeID(FanOutletNode)))
            CALL ShowContinueError('..Supplemental heating coil inlet node = '//TRIM(NodeID(SupHeatCoilInletNode)))
            ErrorsFound=.TRUE.
          ENDIF
          IF (SupHeatCoilOutletNode /= Furnace(FurnaceNum)%FurnaceOutletNodeNum) THEN
            CALL ShowSevereError('For '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1))//&
                                ', Mismatch between supplemental heating coil outlet node and HeatPump outlet node.')
            CALL ShowContinueError('..The supplemental heating coil outlet node name must match the '// &
                                   'HeatPump outlet node name.')
            CALL ShowContinueError('..Supplemental heating coil outlet node = '//TRIM(NodeID(SupHeatCoilOutletNode)))
            CALL ShowContinueError('..HeatPump outlet node                  = ' &
                                                            //TRIM(NodeID(Furnace(FurnaceNum)%FurnaceOutletNodeNum)))
            ErrorsFound=.TRUE.
          ENDIF
        ENDIF
        !  (Set up validation here for the fan or cooling coil inlet?)
        CALL SetUpCompSets(CurrentModuleObject, Alphas(1), &
                           Alphas(6),Alphas(7),CompSetFanInlet,'UNDEFINED')

        ! Add DX heating coil to component sets array
        CALL SetUpCompSets(CurrentModuleObject, Alphas(1), &
                           Alphas(8),Alphas(9),'UNDEFINED','UNDEFINED')

        ! Add DX cooling coil to component sets array
        CALL SetUpCompSets(CurrentModuleObject, Alphas(1), &
                           Alphas(10),Alphas(11),CompSetCoolInlet,'UNDEFINED')

        ! Add supplemental heating coil to component sets array
        CALL SetUpCompSets(CurrentModuleObject, Alphas(1), &
                           Alphas(12),Alphas(13),'UNDEFINED',Alphas(4))



        !Set the Design Fan Volume Flow Rate
        ErrFlag=.FALSE.
        FanVolFlowRate = GetFanDesignVolumeFlowRate(FanType,FanName,ErrFlag)
        Furnace(FurnaceNum)%ActualFanVolFlowRate = FanVolFlowRate
        IF (ErrFlag) THEN
          CALL ShowContinueError('...occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
          ErrorsFound=.TRUE.
        ENDIF

! CR8094 - simple water to air heat pump MUST operate at the same flow rate specified in the coil objects
!        Furnace(FurnaceNum)%DesignFanVolFlowRate = Numbers(1)
!        Furnace(FurnaceNum)%MaxHeatAirVolFlow    = Furnace(FurnaceNum)%DesignFanVolFlowRate
!        Furnace(FurnaceNum)%MaxCoolAirVolFlow    = Furnace(FurnaceNum)%DesignFanVolFlowRate

        ! parameter estimate model only specifies air flow rate in parent object
        IF (Furnace(FurnaceNum)%HeatingCoilType_Num  == Coil_HeatingWaterToAirHP) THEN
          Furnace(FurnaceNum)%MaxHeatAirVolFlow    = Numbers(1)
          Furnace(FurnaceNum)%MaxCoolAirVolFlow    = Numbers(1)
        ! simple HP model specifies air flow rate in both the parent and child coils. Use coil air flow rates.
        ! simple HP model air flow rate input will not be used.
        ELSEIF (Furnace(FurnaceNum)%HeatingCoilType_Num == Coil_HeatingWaterToAirHPSimple) THEN
          ErrFlag=.FALSE.
          Furnace(FurnaceNum)%MaxHeatAirVolFlow       = GetWtoAHPSimpleCoilAirFlow(HeatingCoilType,HeatingCoilName,ErrFlag)
          Furnace(FurnaceNum)%MaxCoolAirVolFlow       = GetWtoAHPSimpleCoilAirFlow(CoolingCoilType,CoolingCoilName,ErrFlag)
         IF (ErrFlag) THEN
            CALL ShowContinueError('...occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
            ErrorsFound=.TRUE.
          ENDIF
        ELSEIF (Furnace(FurnaceNum)%HeatingCoilType_Num == Coil_HeatingWaterToAirHPVSEquationFit) THEN
          ErrFlag=.FALSE.
          Furnace(FurnaceNum)%MaxHeatAirVolFlow       = GetCoilAirFlowRateVariableSpeed(HeatingCoilType,HeatingCoilName,ErrFlag)
          Furnace(FurnaceNum)%MaxCoolAirVolFlow       = GetCoilAirFlowRateVariableSpeed(CoolingCoilType,CoolingCoilName,ErrFlag)
         IF (ErrFlag) THEN
            CALL ShowContinueError('...occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
            ErrorsFound=.TRUE.
          ENDIF
        ENDIF

        Furnace(FurnaceNum)%MaxNoCoolHeatAirVolFlow = &
             MIN(Furnace(FurnaceNum)%MaxHeatAirVolFlow,Furnace(FurnaceNum)%MaxCoolAirVolFlow)
        IF(Furnace(FurnaceNum)%MaxHeatAirVolFlow /= Autosize .AND. &
           Furnace(FurnaceNum)%MaxCoolAirVolFlow /= Autosize)THEN
          Furnace(FurnaceNum)%DesignFanVolFlowRate = &
             MAX(Furnace(FurnaceNum)%MaxHeatAirVolFlow,Furnace(FurnaceNum)%MaxCoolAirVolFlow)
        ELSE
          Furnace(FurnaceNum)%DesignFanVolFlowRate = Autosize
        END IF

        Furnace(FurnaceNum)%AirFlowControl          = UseCompressorOnFlow

        IF (FanVolFlowRate /= AutoSize .and. Furnace(FurnaceNum)%DesignFanVolFlowRate /= AutoSize) THEN
          IF (Furnace(FurnaceNum)%DesignFanVolFlowRate > FanVolFlowRate) THEN
            CALL ShowContinueError('...occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
            CALL ShowContinueError('... has a Cooling or Heating Air Flow Rate > Max Fan Volume Flow Rate, should be <=.')
            CALL ShowContinueError('... Entered value='//TRIM(RoundSigDigits(Furnace(FurnaceNum)%DesignFanVolFlowRate,2))//  &
                '... Fan ['//TRIM(FanType)//':'//TRIM(FanName)//  &
                '] Max Value='//TRIM(RoundSigDigits(FanVolFlowRate,2)))
          ENDIF
        ENDIF
        IF (FanVolFlowRate /= AutoSize .and. Furnace(FurnaceNum)%DesignFanVolFlowRate /= AutoSize) THEN
          IF (Furnace(FurnaceNum)%DesignFanVolFlowRate <= 0.0d0) THEN
            CALL ShowContinueError('...occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
            CALL ShowContinueError('... has a Design Fan Flow Rate <= 0.0, it must be >0.0')
            CALL ShowContinueError('... Entered value='//TRIM(RoundSigDigits(Furnace(FurnaceNum)%DesignFanVolFlowRate,2)))
            ErrorsFound=.TRUE.
          ENDIF
        ENDIF

        !Set the heat pump heating coil capacity
        !  Get from coil module.
        IF (Furnace(FurnaceNum)%HeatingCoilType_Num  == Coil_HeatingWaterToAirHP) THEN
          ErrFlag=.FALSE.
          Furnace(FurnaceNum)%DesignHeatingCapacity =   &
             GetWtoAHPCoilCapacity(HeatingCoilType,HeatingCoilName,ErrFlag)
          IF (ErrFlag) THEN
            CALL ShowContinueError('...occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
            ErrorsFound=.TRUE.
          ENDIF
        ELSEIF (Furnace(FurnaceNum)%HeatingCoilType_Num == Coil_HeatingWaterToAirHPSimple) THEN
          ErrFlag=.FALSE.
          Furnace(FurnaceNum)%DesignHeatingCapacity =   &
             GetWtoAHPSimpleCoilCapacity(HeatingCoilType,HeatingCoilName,ErrFlag)
          IF (ErrFlag) THEN
            CALL ShowContinueError('...occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
            ErrorsFound=.TRUE.
          ENDIF
        ELSEIF (Furnace(FurnaceNum)%HeatingCoilType_Num == Coil_HeatingWaterToAirHPVSEquationFit) THEN
          ErrFlag=.FALSE.
          Furnace(FurnaceNum)%DesignHeatingCapacity =   &
             GetCoilCapacityVariableSpeed(HeatingCoilType,HeatingCoilName,ErrFlag)
          IF (ErrFlag) THEN
            CALL ShowContinueError('...occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
            ErrorsFound=.TRUE.
          ENDIF
        ENDIF
        !Set the heat pump heating coil convergence
        Furnace(FurnaceNum)%HeatingConvergenceTolerance = Numbers(2)
        !Set the heat pump cooling coil capacity (Total capacity)
        !  Get from coil module.
        IF (Furnace(FurnaceNum)%CoolingCoilType_Num  == Coil_CoolingWaterToAirHP) THEN
          ErrFlag=.FALSE.
          Furnace(FurnaceNum)%DesignCoolingCapacity =   &
             GetWtoAHPCoilCapacity(CoolingCoilType,CoolingCoilName,ErrFlag)
          IF (ErrFlag) THEN
            CALL ShowContinueError('...occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
            ErrorsFound=.TRUE.
          ENDIF
        ELSEIF (Furnace(FurnaceNum)%CoolingCoilType_Num == Coil_CoolingWaterToAirHPSimple) THEN
          ErrFlag=.FALSE.
          Furnace(FurnaceNum)%DesignCoolingCapacity =   &
             GetWtoAHPSimpleCoilCapacity(CoolingCoilType,CoolingCoilName,ErrFlag)
          IF (ErrFlag) THEN
            CALL ShowContinueError('...occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
            ErrorsFound=.TRUE.
          ENDIF
       ELSEIF (Furnace(FurnaceNum)%CoolingCoilType_Num == Coil_CoolingWaterToAirHPVSEquationFit) THEN
          ErrFlag=.FALSE.
          Furnace(FurnaceNum)%DesignCoolingCapacity =   &
             GetCoilCapacityVariableSpeed(CoolingCoilType,CoolingCoilName,ErrFlag)
          IF (ErrFlag) THEN
            CALL ShowContinueError('...occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
            ErrorsFound=.TRUE.
          ENDIF
        ENDIF
        !Set the heat pump cooling coil convergence
        Furnace(FurnaceNum)%CoolingConvergenceTolerance = Numbers(3)
        !Set the heatpump cycling rate
        Furnace(FurnaceNum)%MaxONOFFCyclesperHour = Numbers(4)

        !Set the heat pump time constant
        Furnace(FurnaceNum)%HPTimeConstant = Numbers(5)

        !Set the heat pump on-cycle power use fraction
        Furnace(FurnaceNum)%OnCyclePowerFraction = Numbers (6)

        !Set the heat pump fan delay time
        Furnace(FurnaceNum)%FanDelayTime = Numbers(7)

        !Set the heatpump design supplemental heating capacity
        !  Get from coil module.


        !Set the heatpump max outlet temperature
        Furnace(FurnaceNum)%DesignMaxOutletTemp = Numbers(8)


        !Set maximum supply air temperature for supplemental heating coil
        Furnace(FurnaceNum)%MaxOATSuppHeat = Numbers(9)

      END DO  !End of the Unitary System WatertoAirHeatPump Loop


      DEALLOCATE(Alphas)
      DEALLOCATE(Numbers)

      IF (ErrorsFound) THEN
        CALL ShowFatalError('Errors found in getting Furnace or Unitary System input.')
      ENDIF


      DO HeatOnlyNum = 1,  NumHeatOnly
        FurnaceNum= HeatOnlyNum
        ! Setup Report variables for the Furnace that are not reported in the components themselves
        CALL SetupOutputVariable('Unitary System Fan Part Load Ratio []',Furnace(FurnaceNum)%FanPartLoadRatio, &
                              'System','Average',Furnace(FurnaceNum)%Name)
        IF (AnyEnergyManagementSystemInModel) THEN
          CALL SetupEMSActuator('AirLoopHVAC:Unitary:Furnace:HeatOnly', Furnace(FurnaceNum)%Name,             &
                                'Autosized Supply Air Flow Rate', '[m3/s]', &
                                Furnace(FurnaceNum)%DesignFanVolFlowRateEMSOverrideOn,                  &
                                Furnace(FurnaceNum)%DesignFanVolFlowRateEMSOverrideValue )
        ENDIF
      END DO


      DO UnitaryHeatOnlyNum = NumHeatOnly + 1, NumHeatOnly + NumUnitaryHeatOnly
        FurnaceNum= UnitaryHeatOnlyNum
        ! Setup Report variables for Unitary System that are not reported in the components themselves
        CALL SetupOutputVariable('Unitary System Fan Part Load Ratio []',Furnace(FurnaceNum)%FanPartLoadRatio, &
                              'System','Average',Furnace(FurnaceNum)%Name)
        IF (AnyEnergyManagementSystemInModel) THEN
          CALL SetupEMSActuator('AirLoopHVAC:UnitaryHeatOnly', Furnace(FurnaceNum)%Name,             &
                                'Autosized Supply Air Flow Rate', '[m3/s]', &
                                Furnace(FurnaceNum)%DesignFanVolFlowRateEMSOverrideOn,                  &
                                Furnace(FurnaceNum)%DesignFanVolFlowRateEMSOverrideValue )
        ENDIF
      END DO


      DO HeatCoolNum = NumHeatOnly + NumUnitaryHeatOnly + 1,  NumHeatOnly + NumUnitaryHeatOnly + NumHeatCool
        FurnaceNum= HeatCoolNum
        ! Setup Report variables for the Furnace that are not reported in the components themselves
        CALL SetupOutputVariable('Unitary System Fan Part Load Ratio []',Furnace(FurnaceNum)%FanPartLoadRatio, &
                              'System','Average',Furnace(FurnaceNum)%Name)
        CALL SetupOutputVariable('Unitary System Compressor Part Load Ratio []',Furnace(FurnaceNum)%CompPartLoadRatio, &
                              'System','Average',Furnace(FurnaceNum)%Name)

        IF (AnyEnergyManagementSystemInModel) THEN
          CALL SetupEMSActuator('AirLoopHVAC:Unitary:Furnace:HeatCool', Furnace(FurnaceNum)%Name,   &
                                'Autosized Supply Air Flow Rate', '[m3/s]',                         &
                                Furnace(FurnaceNum)%DesignFanVolFlowRateEMSOverrideOn,              &
                                Furnace(FurnaceNum)%DesignFanVolFlowRateEMSOverrideValue )
          CALL SetupEMSActuator('AirLoopHVAC:Unitary:Furnace:HeatCool', Furnace(FurnaceNum)%Name,    &
                                'Autosized Supply Air Flow Rate During Cooling Operation', '[m3/s]', &
                                Furnace(FurnaceNum)%MaxCoolAirVolFlowEMSOverrideOn,                  &
                                Furnace(FurnaceNum)%MaxCoolAirVolFlowEMSOverrideValue )
          CALL SetupEMSActuator('AirLoopHVAC:Unitary:Furnace:HeatCool', Furnace(FurnaceNum)%Name,    &
                                'Autosized Supply Air Flow Rate During Heating Operation', '[m3/s]', &
                                Furnace(FurnaceNum)%MaxHeatAirVolFlowEMSOverrideOn,                  &
                                Furnace(FurnaceNum)%MaxHeatAirVolFlowEMSOverrideValue )
          CALL SetupEMSActuator('AirLoopHVAC:Unitary:Furnace:HeatCool', Furnace(FurnaceNum)%Name,             &
                                'Autosized Supply Air Flow Rate During No Heating or Cooling Operation', '[m3/s]', &
                                Furnace(FurnaceNum)%MaxNoCoolHeatAirVolFlowEMSOverrideOn,                  &
                                Furnace(FurnaceNum)%MaxNoCoolHeatAirVolFlowEMSOverrideValue )
        ENDIF
      END DO


      DO UnitaryHeatCoolNum = NumHeatOnly+NumHeatCool+NumUnitaryHeatOnly+1, &
                              NumHeatOnly+NumHeatCool+NumUnitaryHeatOnly+NumUnitaryHeatCool
        FurnaceNum= UnitaryHeatCoolNum
        ! Setup Report variables for Unitary System that are not reported in the components themselves
        CALL SetupOutputVariable('Unitary System Fan Part Load Ratio []',Furnace(FurnaceNum)%FanPartLoadRatio, &
                              'System','Average',Furnace(FurnaceNum)%Name)
        CALL SetupOutputVariable('Unitary System Compressor Part Load Ratio []',Furnace(FurnaceNum)%CompPartLoadRatio, &
                              'System','Average',Furnace(FurnaceNum)%Name)
        IF (AnyEnergyManagementSystemInModel) THEN
          CALL SetupEMSActuator('AirLoopHVAC:UnitaryHeatCool', Furnace(FurnaceNum)%Name,   &
                                'Autosized Supply Air Flow Rate', '[m3/s]',                         &
                                Furnace(FurnaceNum)%DesignFanVolFlowRateEMSOverrideOn,              &
                                Furnace(FurnaceNum)%DesignFanVolFlowRateEMSOverrideValue )
          CALL SetupEMSActuator('AirLoopHVAC:UnitaryHeatCool', Furnace(FurnaceNum)%Name,             &
                                'Autosized Supply Air Flow Rate During Cooling Operation', '[m3/s]', &
                                Furnace(FurnaceNum)%MaxCoolAirVolFlowEMSOverrideOn,                  &
                                Furnace(FurnaceNum)%MaxCoolAirVolFlowEMSOverrideValue )
          CALL SetupEMSActuator('AirLoopHVAC:UnitaryHeatCool', Furnace(FurnaceNum)%Name,             &
                                'Autosized Supply Air Flow Rate During Heating Operation', '[m3/s]', &
                                Furnace(FurnaceNum)%MaxHeatAirVolFlowEMSOverrideOn,                  &
                                Furnace(FurnaceNum)%MaxHeatAirVolFlowEMSOverrideValue )
          CALL SetupEMSActuator('AirLoopHVAC:UnitaryHeatCool', Furnace(FurnaceNum)%Name,             &
                                'Autosized Supply Air Flow Rate During No Heating or Cooling Operation', '[m3/s]', &
                                Furnace(FurnaceNum)%MaxNoCoolHeatAirVolFlowEMSOverrideOn,                  &
                                Furnace(FurnaceNum)%MaxNoCoolHeatAirVolFlowEMSOverrideValue )
        ENDIF

      END DO


      DO HeatPumpNum = NumHeatOnly+NumHeatCool+NumUnitaryHeatOnly+NumUnitaryHeatCool+1, NumFurnaces-NumWaterToAirHeatPump
        FurnaceNum= HeatPumpNum
        ! Setup Report variables for Unitary System that are not reported in the components themselves
        CALL SetupOutputVariable('Unitary System Fan Part Load Ratio []',Furnace(FurnaceNum)%FanPartLoadRatio, &
                              'System','Average',Furnace(FurnaceNum)%Name)
        CALL SetupOutputVariable('Unitary System Compressor Part Load Ratio []',Furnace(FurnaceNum)%CompPartLoadRatio, &
                              'System','Average',Furnace(FurnaceNum)%Name)
        CALL SetupOutputVariable('Unitary System Dehumidification Induced Heating Demand Rate [W]', &
                                 Furnace(FurnaceNum)%DehumidInducedHeatingDemandRate, 'System','Average', &
                                 Furnace(FurnaceNum)%Name)

        IF (AnyEnergyManagementSystemInModel) THEN
          CALL SetupEMSActuator('AirLoopHVAC:UnitaryHeatPump:AirToAir', Furnace(FurnaceNum)%Name,             &
                                'Autosized Supply Air Flow Rate', '[m3/s]', &
                                Furnace(FurnaceNum)%DesignFanVolFlowRateEMSOverrideOn,                  &
                                Furnace(FurnaceNum)%DesignFanVolFlowRateEMSOverrideValue )
        ENDIF
      END DO

      DO HeatPumpNum = NumHeatOnly+NumHeatCool+NumUnitaryHeatOnly+NumUnitaryHeatCool+NumHeatPump+1, NumFurnaces
        FurnaceNum= HeatPumpNum
        ! Setup Report variables for Unitary System that are not reported in the components themselves
        CALL SetupOutputVariable('Unitary System Fan Part Load Ratio []',Furnace(FurnaceNum)%FanPartLoadRatio, &
                              'System','Average',Furnace(FurnaceNum)%Name)
        CALL SetupOutputVariable('Unitary System Compressor Part Load Ratio []',Furnace(FurnaceNum)%CompPartLoadRatio, &
                              'System','Average',Furnace(FurnaceNum)%Name)
        CALL SetupOutputVariable('Unitary System Requested Sensible Cooling Rate [W]',Furnace(FurnaceNum)%CoolingCoilSensDemand, &
                              'System','Average',Furnace(FurnaceNum)%Name)
        CALL SetupOutputVariable('Unitary System Requested Latent Cooling Rate [W]',Furnace(FurnaceNum)%CoolingCoilLatentDemand, &
                              'System','Average',Furnace(FurnaceNum)%Name)
        CALL SetupOutputVariable('Unitary System Requested Heating Rate [W]',Furnace(FurnaceNum)%HeatingCoilSensDemand, &
                              'System','Average',Furnace(FurnaceNum)%Name)
        CALL SetupOutputVariable('Unitary System Dehumidification Induced Heating Demand Rate [W]', &
                                 Furnace(FurnaceNum)%DehumidInducedHeatingDemandRate, 'System','Average', &
                                 Furnace(FurnaceNum)%Name)

        IF (AnyEnergyManagementSystemInModel) THEN
          CALL SetupEMSActuator('AirLoopHVAC:UnitaryHeatPump:WaterToAir', Furnace(FurnaceNum)%Name,             &
                                'Autosized Supply Air Flow Rate', '[m3/s]', &
                                Furnace(FurnaceNum)%DesignFanVolFlowRateEMSOverrideOn,                  &
                                Furnace(FurnaceNum)%DesignFanVolFlowRateEMSOverrideValue )
        ENDIF
      END DO

      IF (AnyEnergyManagementSystemInModel) THEN
        DO FurnaceNum = 1, NumFurnaces
          CALL SetupEMSInternalVariable('Unitary HVAC Design Heating Capacity', Furnace(FurnaceNum)%Name, '[W]', &
                        Furnace(FurnaceNum)%DesignHeatingCapacity  )
          CALL SetupEMSInternalVariable('Unitary HVAC Design Cooling Capacity', Furnace(FurnaceNum)%Name, '[W]', &
                        Furnace(FurnaceNum)%DesignCoolingCapacity  )
          CALL SetupEMSActuator('Unitary HVAC', Furnace(FurnaceNum)%Name, 'Sensible Load Request' , '[W]', &
                        Furnace(FurnaceNum)%EMSOverrideSensZoneLoadRequest, Furnace(FurnaceNum)%EMSSensibleZoneLoadValue )
          CALL SetupEMSActuator('Unitary HVAC', Furnace(FurnaceNum)%Name, 'Moisture Load Request' , '[W]', &
                        Furnace(FurnaceNum)%EMSOverrideMoistZoneLoadRequest, Furnace(FurnaceNum)%EMSMoistureZoneLoadValue )

        ENDDO
      ENDIF

      CALL ManageEMS(emsCallFromComponentGetInput)
  RETURN


END SUBROUTINE GetFurnaceInput



! End of Get Input subroutines for this Module
!******************************************************************************



 ! Beginning Initialization Section of the Module
!******************************************************************************


SUBROUTINE InitFurnace(FurnaceNum, AirLoopNum, OnOffAirFlowRatio, OpMode, ZoneLoad, MoistureLoad, FirstHVACIteration)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard J. Liesen
          !       DATE WRITTEN   Feb 2001
          !       MODIFIED       Oct 2001, Richard Raustad
          !                      Sep 2008, R. Raustad - revised logic to determine load to be met
          !                      Bereket Nigusse, June 2010 - added a procedure to calculate supply air flow fraction
          !                      through controlled zone
          !                      Bo Shen, March 2012 - for VS WSHP
          !                      Bo Shen, ORNL, July 2012 - added variable-speed air source heat pump cooling and heating coils, using curve-fits

          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is for initializations of the Furnace Components.

          ! METHODOLOGY EMPLOYED:
          ! Uses the status flags to trigger initializations.
          ! The HeatCool furnace/unitarysystem and air-to-air heat pump may have alternate air flow rates
          ! in cooling, heating, and when no cooling or heating is needed. Set up the coil (comp) ON and OFF
          ! air flow rates during InitFurnace. Use these flow rates during the Calc routines to set the
          ! average mass flow rates based on PLR.

          ! REFERENCES:

          ! USE STATEMENTS:
  USE Fans,                  ONLY: GetFanDesignVolumeFlowRate, GetFanSpeedRatioCurveIndex
  USE General,               ONLY: RoundSigDigits, TrimSigDigits
  USE ReportSizingManager,   ONLY: ReportSizingOutput
  USE DataSizing,            ONLY: AutoSize
  USE DataAirLoop,           ONLY: LoopHeatingCoilMaxRTF, AirLoopControlInfo
  USE DataZoneEnergyDemands, ONLY: ZoneSysEnergyDemand, ZoneSysMoistureDemand, CurDeadbandOrSetback
  USE DataHeatBalFanSys,     ONLY: TempControlType
  USE DataHeatBalance,       ONLY: Zone
  USE DataAirflowNetwork,    ONLY: SimulateAirflowNetwork, AirflowNetworkControlMultizone
  USE DataAirLoop ,          ONLY: AirToZoneNodeInfo
  USE DataPlant,             ONLY: TypeOf_CoilSteamAirHeating, ScanPlantLoopsForObject, TypeOf_CoilWaterSimpleHeating, &
                                   PlantLoop
  USE SteamCoils,            ONLY: SimulateSteamCoilComponents, GetCoilMaxSteamFlowRate=>GetCoilMaxSteamFlowRate, &
                                   GetSteamCoilCapacity=>GetCoilCapacity
  USE WaterCoils,            ONLY: GetCoilMaxWaterFlowRate, SimulateWaterCoilComponents
  USE FluidProperties,       ONLY: GetDensityGlycol, GetSatDensityRefrig
  USE PlantUtilities,        ONLY: SetComponentFlowRate, InitComponentNodes
  USE Fans,                  ONLY: GetFanVolFlow

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER,   INTENT(IN)    :: FurnaceNum        ! index to Furnace
  INTEGER,   INTENT(IN)    :: AirLoopNum        ! index to air loop
  REAL(r64), INTENT(INOUT) :: OnOffAirFlowRatio ! ratio of on to off air mass flow rate
  INTEGER,   INTENT(INOUT) :: OpMode            ! fan operating mode
  REAL(r64), INTENT(INOUT) :: ZoneLoad          ! zone sensible load to be met (modified here as needed) (W)
  REAL(r64), INTENT(INOUT) :: MoistureLoad      ! zone moisture load (W)
  LOGICAL,   INTENT (IN)   :: FirstHVACIteration ! TRUE if first HVAC iteration

          ! SUBROUTINE PARAMETER DEFINITIONS:
  REAL(r64), PARAMETER :: Small5WLoad = 5.0D0

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  LOGICAL, SAVE                           :: MyOneTimeFlag = .TRUE.  ! one time allocation flag
  LOGICAL, ALLOCATABLE,SAVE, DIMENSION(:) :: MyEnvrnFlag             ! environment flag
  LOGICAL, ALLOCATABLE,SAVE, DIMENSION(:) :: MySecondOneTimeFlag     ! additional one time flag
  LOGICAL, ALLOCATABLE,SAVE, DIMENSION(:) :: MyFanFlag               ! used for sizing fan inputs one time
  LOGICAL, ALLOCATABLE,SAVE, DIMENSION(:) :: MyCheckFlag ! Used to obtain the zone inlet node number in the controlled zone
  LOGICAL, ALLOCATABLE,SAVE, DIMENSION(:) :: MyFlowFracFlag          ! Used for calculatig flow fraction once
  LOGICAL, ALLOCATABLE,SAVE, DIMENSION(:) :: MyPlantScanFlag ! used to initializa plant comp for water and steam heating coils
  LOGICAL, ALLOCATABLE,SAVE, DIMENSION(:) :: MySuppCoilPlantScanFlag ! used to initialize plant comp for water and steam heating coils
  LOGICAL                                 :: ErrFlag                 ! error flag for mining functions
  REAL(r64)                               :: FanVolFlowRate          ! fan volumetric flow rate (m3/s)
  REAL(r64)                               :: QZnReq                  ! furnace load based on control zone frac (W)
  REAL(r64)                               :: PartLoadRatio           ! furnace part-load ratio
  REAL(r64)                               :: SensibleOutput          ! no load sensible output (coils off) (W)
  REAL(r64)                               :: LatentOutput            ! no load latent output (coils off) (W)
  REAL(r64)                               :: QToCoolSetPt            ! sensible load to cooling setpoint (W)
  REAL(r64)                               :: QToHeatSetPt            ! sensible load to heating setpoint (W)
  INTEGER                                 :: ZoneInNode              ! Zone inlet node number in the controlled zone
  REAL(r64)                               :: MinHumRat               ! Minimum humidity ratio for sensible capacity
                                                                     ! calculation (kg/kg)
  REAL(r64)                               :: DeltaMassRate           ! Difference of mass flow rate between
                                                                     ! inlet node and system outlet node
  INTEGER                                 :: i,j,k                   ! index to get the zone inlet node
  REAL(r64)                               :: MassFlowRate            ! mass flow rate to calculate loss
  REAL(r64)                               :: MaxTemp                 ! Maximum temperature used in latent loss calculation
  CHARACTER(len=MaxNameLength)            :: FanType                 ! used in warning messages
  CHARACTER(len=MaxNameLength)            :: FanName                 ! used in warning messages

  INTEGER :: ZoneInSysIndex = 0                        ! number of zone inlet nodes counter in an airloop
  INTEGER :: NumAirLoopZones = 0                       ! number of zone inlet nodes in an air loop
  INTEGER :: ZoneInletNodeNum = 0                      ! zone inlet nodes node number
  LOGICAL :: FlowFracFlagReady = .TRUE.                ! one time flag for calculating flow fraction through controlled zone
  REAL(r64) :: SumOfMassFlowRateMax = 0.0d0              ! the sum of mass flow rates at inlet to zones in an airloop
  REAL(r64) :: CntrlZoneTerminalUnitMassFlowRateMax = 0.0d0  ! Maximum mass flow rate through controlled zone terminal unit
  INTEGER :: EquipNum = 0  ! local do loop index for equipment listed for a zone
  INTEGER :: InletNodeNum = 0 ! local do loop index for inlet nodes to a zone

  LOGICAL             :: ErrorsFound        =.FALSE.   ! flag returned from mining call
  REAL(r64)           :: mdot               =0.0d0     ! local temporary for mass flow rate (kg/s)
  REAL(r64)           :: rho                =0.0d0     ! local for fluid density
  INTEGER             :: SteamIndex         =0         ! index of steam quality for steam heating coil
  REAL(r64)           :: SteamDensity       =0.0d0       ! density of steam at 100C, used for steam heating coils
  REAL(r64)           :: CoilMaxVolFlowRate =0.0d0     ! coil fluid maximum volume flow rate
  REAL(r64)           :: QACTUAL            =0.0d0     ! coil actual capacity
  REAL(r64)           :: SUPHEATERLOAD      =0.0d0     ! SUPPLEMENTAL HEATER LOAD
  INTEGER             :: NumOfSpeedCooling                ! Number of speeds for cooling
  INTEGER             :: NumOfSpeedHeating                ! Number of speeds for heating
  INTEGER             :: InNode                           ! Inlet node number in MSHP loop
  INTEGER             :: OutNode                          ! Outlet node number in MSHP loop
  REAL(r64)           :: RhoAir                           ! Air density at InNode
  LOGICAL, SAVE       :: MyAirLoopPass = .TRUE.  ! one time allocation flag

  InNode  = Furnace(FurnaceNum)%FurnaceInletNodeNum
  OutNode = Furnace(FurnaceNum)%FurnaceOutletNodeNum

          ! FLOW:
  IF (MyOneTimeFlag) THEN
    ! initialize the environment and sizing flags
    ALLOCATE(MyEnvrnFlag(NumFurnaces))
    ALLOCATE(MySizeFlag(NumFurnaces))
    ALLOCATE(MySecondOneTimeFlag(NumFurnaces))
    ALLOCATE(MyFanFlag(NumFurnaces))
    ALLOCATE(MyCheckFlag(NumFurnaces))
    ALLOCATE(MyFlowFracFlag(NumFurnaces))
    ALLOCATE(MyPlantScanFlag(NumFurnaces))
    ALLOCATE(MySuppCoilPlantScanFlag(NumFurnaces))
    MyEnvrnFlag = .TRUE.
    MySizeFlag = .TRUE.
    MySecondOneTimeFlag=.TRUE.
    MyFanFlag = .TRUE.
    MyCheckFlag = .TRUE.
    MyFlowFracFlag = .TRUE.
    MyOneTimeFlag = .FALSE.
    MyPlantScanFlag = .TRUE.
    MySuppCoilPlantScanFlag = .TRUE.
  END IF

  IF (BeginEnvrnFlag .and. MyAirLoopPass) THEN
    AirLoopPass = 0
    MyAirLoopPass = .FALSE.
  End If
  IF (.not. BeginEnvrnFlag) THEN
    MyAirLoopPass = .TRUE.
  End If

  AirLoopPass = AirLoopPass + 1
  If (AirLoopPass > 2) AirLoopPass = 1

  IF ( .NOT. SysSizingCalc .AND. MySizeFlag(FurnaceNum)) THEN
    ! for each furnace, do the sizing once.
    CALL SizeFurnace(FurnaceNum,FirstHVACIteration)
    Furnace(FurnaceNum)%ControlZoneMassFlowFrac = 1.0d0

    MySizeFlag(FurnaceNum) = .FALSE.
    ! Pass the fan cycling schedule index up to the air loop. Set the air loop unitary system flag.
    AirLoopControlInfo(AirLoopNum)%CycFanSchedPtr = Furnace(FurnaceNum)%FanSchedPtr
    AirLoopControlInfo(AirLoopNum)%UnitarySys = .TRUE.
    AirLoopControlInfo(AirLoopNum)%FanOpMode = Furnace(FurnaceNum)%OpMode

    !Check that heat pump heating capacity is within 20% of cooling capacity
    IF (Furnace(FurnaceNum)%FurnaceType_Num == UnitarySys_HeatPump_AirToAir) THEN
      IF(ABS(Furnace(FurnaceNum)%DesignCoolingCapacity-&
             Furnace(FurnaceNum)%DesignHeatingCapacity)/&
             Furnace(FurnaceNum)%DesignCoolingCapacity .GT. 0.2d0) THEN
        CALL ShowWarningError(TRIM(cFurnaceTypes(Furnace(FurnaceNum)%FurnaceType_Num))//' "'//TRIM(Furnace(FurnaceNum)%Name)//&
                              '" heating capacity is disproportionate (> 20% different) to total cooling capacity')
      END IF
    END IF

  END IF

  IF (.not. DoingSizing .and. MySecondOneTimeFlag(FurnaceNum)) THEN
    ! sizing all done.  check fan air flow rates
    ErrFlag=.FALSE.
    FanVolFlowRate =    &
        GetFanDesignVolumeFlowRate(Blank,Blank,ErrFlag,Furnace(FurnaceNum)%FanIndex)
    Furnace(FurnaceNum)%ActualFanVolFlowRate = FanVolFlowRate
    IF (ErrFlag) THEN
      CALL ShowContinueError('...occurs in '//TRIM(cFurnaceTypes(Furnace(FurnaceNum)%FurnaceType_Num))//  &
                ' ='//TRIM(Furnace(FurnaceNum)%Name))
    ENDIF
    IF (FanVolFlowRate /= AutoSize) THEN
      IF (Furnace(FurnaceNum)%DesignFanVolFlowRate > FanVolFlowRate) THEN
        CALL ShowWarningError(TRIM(cFurnaceTypes(Furnace(FurnaceNum)%FurnaceType_Num))//'='//TRIM(Furnace(FurnaceNum)%Name)//  &
             ' has a Design Fan Volume Flow Rate > Max Fan Volume Flow Rate, should be <=')
        CALL ShowContinueError('... Entered value='//TRIM(RoundSigDigits(Furnace(FurnaceNum)%DesignFanVolFlowRate,2))//  &
             '... Fan ['//TRIM(cFanTypes(Furnace(FurnaceNum)%FanType_Num))//  &
             '] Max Value='//TRIM(RoundSigDigits(FanVolFlowRate,2)))
      ENDIF
      IF (Furnace(FurnaceNum)%DesignFanVolFlowRate <= 0.0d0) THEN
        CALL ShowSevereError(TRIM(cFurnaceTypes(Furnace(FurnaceNum)%FurnaceType_Num))//'='//TRIM(Furnace(FurnaceNum)%Name)//  &
             ' has a Design Fan Volume Flow Rate <= 0.0, it must be >0.0')
        CALL ShowContinueError('... Entered value='//TRIM(RoundSigDigits(Furnace(FurnaceNum)%DesignFanVolFlowRate,2)))
      ENDIF

      MySecondOneTimeFlag(FurnaceNum)=.FALSE.
    ENDIF
  ENDIF

  ! Scan hot water and steam heating coil plant components for one time initializations
  IF (MyPlantScanFlag(FurnaceNum) .AND. ALLOCATED(PlantLoop)) THEN
    IF ( (Furnace(FurnaceNum)%HeatingCoilType_Num == Coil_HeatingWater) .OR. &
         (Furnace(FurnaceNum)%HeatingCoilType_Num == Coil_HeatingSteam) ) THEN

      IF (Furnace(FurnaceNum)%HeatingCoilType_Num == Coil_HeatingWater) THEN

        ERRFlag=.false.
        CALL ScanPlantLoopsForObject( Furnace(FurnaceNum)%HeatingCoilName, &
                                          TypeOf_CoilWaterSimpleHeating , &
                                          Furnace(FurnaceNum)%LoopNum, &
                                          Furnace(FurnaceNum)%LoopSide, &
                                          Furnace(FurnaceNum)%BranchNum, &
                                          Furnace(FurnaceNum)%CompNum,   &
                                          ERRFlag=errFlag)
        IF (errFlag) THEN
          CALL ShowFatalError('InitFurnace: Program terminated for previous conditions.')
        ENDIF
        Furnace(FurnaceNum)%MaxHeatCoilFluidFlow = GetCoilMaxWaterFlowRate('Coil:Heating:Water',  &
                                                   Furnace(FurnaceNum)%HeatingCoilName,ErrorsFound)
        IF(Furnace(FurnaceNum)%MaxHeatCoilFluidFlow .GT. 0.0d0)THEN
          rho = GetDensityGlycol(PlantLoop(Furnace(FurnaceNum)%LoopNum)%FluidName, &
                                 InitConvTemp, &
                                 PlantLoop(Furnace(FurnaceNum)%LoopNum)%FluidIndex, &
                                'InitFurnace')
          Furnace(FurnaceNum)%MaxHeatCoilFluidFlow = Furnace(FurnaceNum)%MaxHeatCoilFluidFlow * rho
        END IF
      ELSEIF (Furnace(FurnaceNum)%HeatingCoilType_Num == Coil_HeatingSteam) THEN

        errFlag=.false.
        CALL ScanPlantLoopsForObject( Furnace(FurnaceNum)%HeatingCoilName, &
                                          TypeOf_CoilSteamAirHeating , &
                                          Furnace(FurnaceNum)%LoopNum, &
                                          Furnace(FurnaceNum)%LoopSide, &
                                          Furnace(FurnaceNum)%BranchNum, &
                                          Furnace(FurnaceNum)%CompNum,   &
                                          errFlag=errFlag)
        IF (errFlag) THEN
          CALL ShowFatalError('InitFurnace: Program terminated for previous conditions.')
        ENDIF
        Furnace(FurnaceNum)%MaxHeatCoilFluidFlow = &
                   GetCoilMaxSteamFlowRate(Furnace(FurnaceNum)%HeatingCoilIndex,ErrorsFound)
        IF(Furnace(FurnaceNum)%MaxHeatCoilFluidFlow .GT. 0.0d0)THEN
          SteamIndex = 0 ! Function GetSatDensityRefrig will look up steam index if 0 is passed
          SteamDensity=GetSatDensityRefrig('STEAM',TempSteamIn,1.0d0,SteamIndex,'InitFurnace')
          Furnace(FurnaceNum)%MaxHeatCoilFluidFlow = Furnace(FurnaceNum)%MaxHeatCoilFluidFlow * SteamDensity
        END IF

      ENDIF
      ! fill outlet node for coil
      Furnace(FurnaceNum)%CoilOutletNode =  &
            PlantLoop(Furnace(FurnaceNum)%LoopNum)%LoopSide(Furnace(FurnaceNum)%LoopSide) &
                       %Branch(Furnace(FurnaceNum)%BranchNum)%Comp(Furnace(FurnaceNum)%CompNum)%NodeNumOut
      MyPlantScanFlag(FurnaceNum) = .FALSE.
    ELSE ! pthp not connected to plant
      MyPlantScanFlag(FurnaceNum) = .FALSE.
    ENDIF
  ELSEIF (MyPlantScanFlag(FurnaceNum) .AND. .NOT. AnyPlantInModel) THEN
    MyPlantScanFlag(FurnaceNum) = .FALSE.
  ENDIF

  ! Scan Supplemental hot water and steam heating coil plant components for one time initializations
  IF (MySuppCoilPlantScanFlag(FurnaceNum) .AND. ALLOCATED(PlantLoop)) THEN
    IF ( (Furnace(FurnaceNum)%SuppHeatCoilType_Num == Coil_HeatingWater) .OR. &
             (Furnace(FurnaceNum)%SuppHeatCoilType_Num == Coil_HeatingSteam) ) THEN

      IF (Furnace(FurnaceNum)%SuppHeatCoilType_Num == Coil_HeatingWater) THEN
        errFlag=.false.
        CALL ScanPlantLoopsForObject( Furnace(FurnaceNum)%SuppHeatCoilName, &
                                          TypeOf_CoilWaterSimpleHeating , &
                                          Furnace(FurnaceNum)%LoopNumSupp, &
                                          Furnace(FurnaceNum)%LoopSideSupp, &
                                          Furnace(FurnaceNum)%BranchNumSupp, &
                                          Furnace(FurnaceNum)%CompNumSupp,   &
                                          errFlag=errFlag)
        IF (errFlag) THEN
          CALL ShowFatalError('InitFurnace: Program terminated for previous conditions.')
        ENDIF
        Furnace(FurnaceNum)%MaxSuppCoilFluidFlow = GetCoilMaxWaterFlowRate('Coil:Heating:Water',  &
                                                   Furnace(FurnaceNum)%SuppHeatCoilName,ErrorsFound)
        IF(Furnace(FurnaceNum)%MaxSuppCoilFluidFlow .GT. 0.0d0)THEN
            rho = GetDensityGlycol(PlantLoop(Furnace(FurnaceNum)%LoopNumSupp)%FluidName, &
                                   InitConvTemp, &
                                   PlantLoop(Furnace(FurnaceNum)%LoopNumSupp)%FluidIndex, &
                                   'InitFurnace')
            Furnace(FurnaceNum)%MaxSuppCoilFluidFlow = Furnace(FurnaceNum)%MaxSuppCoilFluidFlow * rho
        END IF
      ELSEIF (Furnace(FurnaceNum)%SuppHeatCoilType_Num == Coil_HeatingSteam) THEN
        errFlag=.false.
        CALL ScanPlantLoopsForObject(Furnace(FurnaceNum)%SuppHeatCoilName, &
                                          TypeOf_CoilSteamAirHeating , &
                                          Furnace(FurnaceNum)%LoopNumSupp, &
                                          Furnace(FurnaceNum)%LoopSideSupp, &
                                          Furnace(FurnaceNum)%BranchNumSupp, &
                                          Furnace(FurnaceNum)%CompNumSupp,   &
                                          errFlag=errFlag)
        IF (errFlag) THEN
          CALL ShowFatalError('InitFurnace: Program terminated for previous conditions.')
        ENDIF
        Furnace(FurnaceNum)%MaxSuppCoilFluidFlow = GetCoilMaxSteamFlowRate(Furnace(FurnaceNum)%SuppHeatCoilIndex,ErrorsFound)
        IF(Furnace(FurnaceNum)%MaxSuppCoilFluidFlow .GT. 0.0d0)THEN
          SteamIndex = 0 ! Function GetSatDensityRefrig will look up steam index if 0 is passed
          SteamDensity=GetSatDensityRefrig('STEAM',TempSteamIn,1.0d0,SteamIndex,'InitFurnace')
          Furnace(FurnaceNum)%MaxSuppCoilFluidFlow = Furnace(FurnaceNum)%MaxSuppCoilFluidFlow * SteamDensity
        END IF

      ENDIF
      ! fill outlet node for coil
      Furnace(FurnaceNum)%SuppCoilOutletNode =  &
            PlantLoop(Furnace(FurnaceNum)%LoopNumSupp)%LoopSide(Furnace(FurnaceNum)%LoopSideSupp) &
                       %Branch(Furnace(FurnaceNum)%BranchNumSupp)%Comp(Furnace(FurnaceNum)%CompNumSupp)%NodeNumOut
      MySuppCoilPlantScanFlag(FurnaceNum) = .FALSE.
    ELSE ! pthp not connected to plant
      MySuppCoilPlantScanFlag(FurnaceNum) = .FALSE.
    ENDIF

  ELSEIF (MySuppCoilPlantScanFlag(FurnaceNum) .AND. .NOT. AnyPlantInModel) THEN
    MySuppCoilPlantScanFlag(FurnaceNum) = .FALSE.
  ENDIF

! Do the Begin Environment initializations
  IF (BeginEnvrnFlag .and. MyEnvrnFlag(FurnaceNum)) THEN
    !Change the Volume Flow Rates to Mass Flow Rates
    Furnace(FurnaceNum)%DesignMassFlowRate = Furnace(FurnaceNum)%DesignFanVolFlowRate * StdRhoAir
    Furnace(FurnaceNum)%MaxCoolAirMassFlow = Furnace(FurnaceNum)%MaxCoolAirVolFlow * StdRhoAir
    Furnace(FurnaceNum)%MaxHeatAirMassFlow = Furnace(FurnaceNum)%MaxHeatAirVolFlow *  StdRhoAir
    Furnace(FurnaceNum)%MaxNoCoolHeatAirMassFlow = Furnace(FurnaceNum)%MaxNoCoolHeatAirVolFlow * StdRhoAir
    Furnace(FurnaceNum)%SenLoadLoss = 0.0d0
    IF (Furnace(FurnaceNum)%Humidistat) Then
      Furnace(FurnaceNum)%LatLoadLoss = 0.0d0
    END IF

!   set fluid-side hardware limits
    IF(Furnace(FurnaceNum)%CoilControlNode .GT. 0)THEN

        IF(Furnace(FurnaceNum)%MaxHeatCoilFluidFlow .EQ. Autosize)THEN
            ! If water coil max water flow rate is autosized, simulate once in order to mine max flow rate
            IF(Furnace(FurnaceNum)%HeatingCoilType_Num == Coil_HeatingWater) THEN
                CALL SimulateWaterCoilComponents(Furnace(FurnaceNum)%HeatingCoilName,FirstHVACIteration, &
                                                Furnace(FurnaceNum)%HeatingCoilIndex)
                CoilMaxVolFlowRate = GetCoilMaxWaterFlowRate('Coil:Heating:Water',  &
                                                             Furnace(FurnaceNum)%HeatingCoilName,ErrorsFound)
                IF(CoilMaxVolFlowRate .NE. Autosize) THEN
                    rho = GetDensityGlycol(PlantLoop(Furnace(FurnaceNum)%LoopNum)%fluidName, &
                                            InitConvTemp, &
                                            PlantLoop(Furnace(FurnaceNum)%LoopNum)%fluidIndex, &
                                            'InitFurnace')
                    Furnace(FurnaceNum)%MaxHeatCoilFluidFlow = CoilMaxVolFlowRate * rho
                ENDIF
            ENDIF
            ! If steam coil max steam flow rate is autosized, simulate once in order to mine max flow rate
            IF(Furnace(FurnaceNum)%HeatingCoilType_Num == Coil_HeatingSteam) THEN
                CALL SimulateSteamCoilComponents(Furnace(FurnaceNum)%HeatingCoilName, &
                                                FirstHVACIteration,    &
                                                1.0d0, & !QCoilReq, simulate any load > 0 to get max capacity
                                                Furnace(FurnaceNum)%HeatingCoilIndex, QActual)
                CoilMaxVolFlowRate = GetCoilMaxSteamFlowRate(Furnace(FurnaceNum)%HeatingCoilIndex,ErrorsFound)
                IF(CoilMaxVolFlowRate .NE. Autosize) THEN
                   SteamIndex = 0 ! Function GetSatDensityRefrig will look up steam index if 0 is passed
                   SteamDensity=GetSatDensityRefrig('STEAM',TempSteamIn,1.0d0,SteamIndex,'InitFurnace')
                   Furnace(FurnaceNum)%MaxHeatCoilFluidFlow = CoilMaxVolFlowRate * SteamDensity
                ENDIF
            ENDIF
        ENDIF

        Call InitComponentNodes(0.d0, Furnace(FurnaceNum)%MaxHeatCoilFluidFlow, &
                                        Furnace(FurnaceNum)%CoilControlNode, &
                                        Furnace(FurnaceNum)%CoilOutletNode, &
                                        Furnace(FurnaceNum)%LoopNum, &
                                        Furnace(FurnaceNum)%LoopSide, &
                                        Furnace(FurnaceNum)%BranchNum, &
                                        Furnace(FurnaceNum)%CompNum )
    ENDIF
    IF(Furnace(FurnaceNum)%SuppCoilControlNode .GT. 0)THEN
        IF(Furnace(FurnaceNum)%MaxSuppCoilFluidFlow .EQ. Autosize)THEN
            IF(Furnace(FurnaceNum)%SuppHeatCoilType_Num == Coil_HeatingWater) THEN
                ! If water coil max water flow rate is autosized, simulate once in order to mine max flow rate
                CALL SimulateWaterCoilComponents(Furnace(FurnaceNum)%SuppHeatCoilName,FirstHVACIteration, &
                                                Furnace(FurnaceNum)%SuppHeatCoilIndex)
                CoilMaxVolFlowRate = GetCoilMaxWaterFlowRate('Coil:Heating:Water',  &
                                                            Furnace(FurnaceNum)%SuppHeatCoilName,ErrorsFound)
                IF(CoilMaxVolFlowRate .NE. Autosize) THEN
                    rho = GetDensityGlycol(PlantLoop(Furnace(FurnaceNum)%LoopNumSupp)%fluidName, &
                                        InitConvTemp, &
                                        PlantLoop(Furnace(FurnaceNum)%LoopNumSupp)%fluidIndex, &
                                        'InitFurnace')
                    Furnace(FurnaceNum)%MaxSuppCoilFluidFlow = CoilMaxVolFlowRate * rho
                ENDIF
            ENDIF
            IF(Furnace(FurnaceNum)%SuppHeatCoilType_Num == Coil_HeatingSteam) THEN
                CALL SimulateSteamCoilComponents(Furnace(FurnaceNum)%SuppHeatCoilName, &
                                                FirstHVACIteration,    &
                                                1.0d0, & !QCoilReq, simulate any load > 0 to get max capacity
                                                Furnace(FurnaceNum)%SuppHeatCoilIndex, QActual)
                CoilMaxVolFlowRate = GetCoilMaxSteamFlowRate(Furnace(FurnaceNum)%SuppHeatCoilIndex,ErrorsFound)
                IF(CoilMaxVolFlowRate .NE. Autosize) THEN
                SteamIndex = 0 ! Function GetSatDensityRefrig will look up steam index if 0 is passed
                SteamDensity=GetSatDensityRefrig('STEAM',TempSteamIn,1.0d0,SteamIndex,'InitFurnace')
                Furnace(FurnaceNum)%MaxSuppCoilFluidFlow = CoilMaxVolFlowRate * SteamDensity
                ENDIF
            ENDIF
            Call InitComponentNodes(0.d0, Furnace(FurnaceNum)%MaxSuppCoilFluidFlow, &
                                        Furnace(FurnaceNum)%SuppCoilControlNode, &
                                        Furnace(FurnaceNum)%SuppCoilOutletNode, &
                                        Furnace(FurnaceNum)%LoopNumSupp, &
                                        Furnace(FurnaceNum)%LoopSideSupp, &
                                        Furnace(FurnaceNum)%BranchNumSupp, &
                                        Furnace(FurnaceNum)%CompNumSupp )
        ENDIF
    ENDIF
    MyEnvrnFlag(FurnaceNum) = .FALSE.
  END IF

  IF (.not. BeginEnvrnFlag) THEN
    MyEnvrnFlag(FurnaceNum) = .TRUE.
  ENDIF

  IF(MyFanFlag(FurnaceNum))THEN
    IF(Furnace(FurnaceNum)%ActualFanVolFlowRate /= Autosize)THEN
      IF(Furnace(FurnaceNum)%ActualFanVolFlowRate .GT. 0.0d0)THEN
        Furnace(FurnaceNum)%HeatingSpeedRatio = Furnace(FurnaceNum)%MaxHeatAirVolFlow/Furnace(FurnaceNum)%ActualFanVolFlowRate
        Furnace(FurnaceNum)%CoolingSpeedRatio = Furnace(FurnaceNum)%MaxCoolAirVolFlow/Furnace(FurnaceNum)%ActualFanVolFlowRate
        Furnace(FurnaceNum)%NoHeatCoolSpeedRatio = &
                                        Furnace(FurnaceNum)%MaxNoCoolHeatAirVolFlow/Furnace(FurnaceNum)%ActualFanVolFlowRate
      END IF
      IF(GetFanSpeedRatioCurveIndex(FanType,FanName,Furnace(FurnaceNum)%FanIndex) .GT. 0)THEN
        IF(Furnace(FurnaceNum)%ActualFanVolFlowRate .EQ. Furnace(FurnaceNum)%MaxHeatAirVolFlow .AND. &
           Furnace(FurnaceNum)%ActualFanVolFlowRate .EQ. Furnace(FurnaceNum)%MaxCoolAirVolFlow .AND. &
           Furnace(FurnaceNum)%ActualFanVolFlowRate .EQ. Furnace(FurnaceNum)%MaxNoCoolHeatAirVolFlow)THEN
          CALL ShowWarningError(TRIM(cFurnaceTypes(Furnace(FurnaceNum)%FurnaceType_Num))// &
                                ' "'//TRIM(Furnace(FurnaceNum)%Name)//'"')
          CALL ShowContinueError('...For fan type and name = '//TRIM(FanType)//' "'//TRIM(FanName)//'"')
          CALL ShowContinueError('...Fan power ratio function of speed ratio curve has no impact if fan volumetric '// &
                                 'flow rate is the same as the unitary system volumetric flow rate.')
          CALL ShowContinueError('...Fan volumetric flow rate            = '// &
                                 TRIM(RoundSigDigits(Furnace(FurnaceNum)%ActualFanVolFlowRate,5))//' m3/s.')
          CALL ShowContinueError('...Unitary system volumetric flow rate = '// &
                                 TRIM(RoundSigDigits(Furnace(FurnaceNum)%MaxHeatAirVolFlow,5))//' m3/s.')
        END IF
      END IF
      MyFanFlag(FurnaceNum) = .FALSE.
    ELSE
      Furnace(FurnaceNum)%ActualFanVolFlowRate = &
        GetFanDesignVolumeFlowRate(Blank,Blank,ErrFlag,Furnace(FurnaceNum)%FanIndex)
    END IF
  END IF

  ! Get the zone inlet node
  IF (ALLOCATED(ZoneEquipConfig) .AND. MyCheckFlag(FurnaceNum)) THEN
    DO i=1,NumOfZones
      IF (AirLoopNum .NE. ZoneEquipConfig(i)%AirLoopNum) CYCLE
      IF (Furnace(FurnaceNum)%ControlZoneNum .EQ. ZoneEquipConfig(i)%ActualZoneNum) Then
        Do j=1, ZoneEquipConfig(i)%NumInletNodes
          If (Furnace(FurnaceNum)%ZoneInletNode .EQ. 0) Then
            Do k=1,ZoneEquipConfig(i)%NumInletNodes
              If (ZoneEquipConfig(i)%InletNode(j) .EQ. ZoneEquipConfig(i)%AirDistUnitCool(k)%OutNode) Then
                Furnace(FurnaceNum)%ZoneInletNode = ZoneEquipConfig(i)%InletNode(j)
                Exit
              End If
            end do
          End If
          If (Furnace(FurnaceNum)%ZoneInletNode .EQ. 0) Then
            Do k=1,ZoneEquipConfig(i)%NumInletNodes
              If (ZoneEquipConfig(i)%InletNode(j) .EQ. ZoneEquipConfig(i)%AirDistUnitHeat(k)%OutNode) Then
                Furnace(FurnaceNum)%ZoneInletNode = ZoneEquipConfig(i)%InletNode(j)
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
              Furnace(FurnaceNum)%ZoneSequenceCoolingNum = &
                         ZoneEquipList(ZoneEquipConfig(i)%EquipListIndex)%CoolingPriority(EquipNum)
              Furnace(FurnaceNum)%ZoneSequenceHeatingNum = &
                         ZoneEquipList(ZoneEquipConfig(i)%EquipListIndex)%HeatingPriority(EquipNum)
            ENDIF
          ENDDO
        ENDIF
      END IF
    END DO
    MyCheckFlag(FurnaceNum) = .FALSE.
    If (Furnace(FurnaceNum)%ZoneInletNode .EQ. 0) Then
      CALL ShowSevereError(TRIM(cFurnaceTypes(Furnace(FurnaceNum)%FurnaceType_Num))//' "'//TRIM(Furnace(FurnaceNum)%Name)// &
                             '": The zone inlet node in the controlled zone (' &
                            //Trim(Zone(Furnace(FurnaceNum)%ControlZoneNum)%Name) //') is not found.')
      CALL ShowFatalError('Subroutine InitFurnace: '//'Errors found in getting '// &
           TRIM(cFurnaceTypes(Furnace(FurnaceNum)%FurnaceType_Num))//' input.  '//'Preceding condition(s) causes termination.')
    End If
  END IF

  ! Find the number of zones (zone Inlet Nodes) attached to an air loop from the air loop number
  NumAirLoopZones = AirToZoneNodeInfo(AirLoopNum)%NumZonesCooled + AirToZoneNodeInfo(AirLoopNum)%NumZonesHeated
  IF (ALLOCATED(AirToZoneNodeInfo) .AND. MyFlowFracFlag(FurnaceNum)) THEN
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
         IF(AirToZoneNodeInfo(AirLoopNum)%CoolCtrlZoneNums(ZoneInSysIndex) == Furnace(FurnaceNum)%ControlZoneNum )THEN
            CntrlZoneTerminalUnitMassFlowRateMax = Node(ZoneInletNodeNum)%MassFlowRateMax
         ENDIF
      END DO
      IF (SumOfMassFlowRateMax /= 0.0d0 .AND. MyFlowFracFlag(FurnaceNum)) THEN
          IF (CntrlZoneTerminalUnitMassFlowRateMax >= SmallAirVolFlow ) THEN
              Furnace(FurnaceNum)%ControlZoneMassFlowFrac = CntrlZoneTerminalUnitMassFlowRateMax/SumOfMassFlowRateMax
          ELSE
              CALL ShowSevereError(TRIM(cFurnaceTypes(Furnace(FurnaceNum)%FurnaceType_Num))//' = '//TRIM(Furnace(FurnaceNum)%Name))
              CALL ShowContinueError(' The Fraction of Supply Air Flow That Goes Through the Controlling Zone is set to 1.')
          END IF
          CALL ReportSizingOutput(cFurnaceTypes(Furnace(FurnaceNum)%FurnaceType_Num),Furnace(FurnaceNum)%Name, &
                                  'Fraction of Supply Air Flow That Goes Through the Controlling Zone', &
                                  Furnace(FurnaceNum)%ControlZoneMassFlowFrac)
          MyFlowFracFlag(FurnaceNum) = .FALSE.
      ENDIF
  ENDIF

  ! Calcuate air distribution losses
  IF (.NOT. FirstHVACIteration .AND. AirLoopPass .eq. 1) Then
    ZoneInNode = Furnace(FurnaceNum)%ZoneInletNode
    MinHumRat = Node(ZoneInNode)%HumRat
    MassFlowRate = Node(ZoneInNode)%MassFlowrate/Furnace(FurnaceNum)%ControlZoneMassFlowFrac
    IF(Node(Furnace(FurnaceNum)%FurnaceOutletNodeNum)%Temp .LT. Node(Furnace(FurnaceNum)%NodeNumofControlledZone)%Temp ) &
      MinHumRat = Node(Furnace(FurnaceNum)%FurnaceOutletNodeNum)%HumRat
    If (SimulateAirflowNetwork > AirflowNetworkControlMultizone) Then
      DeltaMassRate = Node(Furnace(FurnaceNum)%FurnaceOutletNodeNum)%MassFlowrate - &
                    Node(ZoneInNode)%MassFlowrate/Furnace(FurnaceNum)%ControlZoneMassFlowFrac
      If (DeltaMassRate .LT. 0.0d0) DeltaMassRate = 0.0d0
    Else
      MassFlowRate = Node(Furnace(FurnaceNum)%FurnaceOutletNodeNum)%MassFlowrate
      DeltaMassRate = 0.0d0
    End If
    Furnace(FurnaceNum)%SenLoadLoss = MassFlowRate * (PsyHFnTdbW(Node(Furnace(FurnaceNum)%FurnaceOutletNodeNum)%Temp,MinHumRat)- &
       PsyHFnTdbW(Node(ZoneInNode)%Temp,MinHumRat)) + DeltaMassRate * &
      (PsyHFnTdbW(Node(Furnace(FurnaceNum)%FurnaceOutletNodeNum)%Temp,MinHumRat) - &
       PsyHFnTdbW(Node(Furnace(FurnaceNum)%NodeNumofControlledZone)%Temp,MinHumRat))
    If (ABS(Furnace(FurnaceNum)%SensibleLoadMet) > 0.0d0) Then
      If (ABS(Furnace(FurnaceNum)%SenLoadLoss/Furnace(FurnaceNum)%SensibleLoadMet) < 0.001d0)   &
         Furnace(FurnaceNum)%SenLoadLoss = 0.0d0
    End If
    IF(Furnace(FurnaceNum)%Humidistat)THEN
      MaxTemp = Node(Furnace(FurnaceNum)%NodeNumofControlledZone)%Temp
      Furnace(FurnaceNum)%LatLoadLoss = MassFlowRate*(PsyHFnTdbW(MaxTemp,Node(Furnace(FurnaceNum)%FurnaceOutletNodeNum)%HumRat)- &
        PsyHFnTdbW(MaxTemp,Node(ZoneInNode)%HumRat)) + DeltaMassRate * &
        (PsyHFnTdbW(MaxTemp,Node(Furnace(FurnaceNum)%FurnaceOutletNodeNum)%HumRat) - &
        PsyHFnTdbW(MaxTemp,Node(Furnace(FurnaceNum)%NodeNumofControlledZone)%HumRat))
      If (ABS(Furnace(FurnaceNum)%LatentLoadMet) > 0.0d0) Then
        If (ABS(Furnace(FurnaceNum)%LatLoadLoss/Furnace(FurnaceNum)%LatentLoadMet) < 0.001d0)   &
           Furnace(FurnaceNum)%LatLoadLoss = 0.0d0
      End If
    End If
  End If

  IF (Furnace(FurnaceNum)%FanSchedPtr .GT. 0) THEN
    IF (GetCurrentScheduleValue(Furnace(FurnaceNum)%FanSchedPtr) .EQ. 0.0d0) THEN
      Furnace(FurnaceNum)%OpMode = CycFanCycCoil
    ELSE
      Furnace(FurnaceNum)%OpMode = ContFanCycCoil
    END IF
  END IF

  OpMode = Furnace(FurnaceNum)%OpMode
  EconomizerFlag = AirLoopControlInfo(AirLoopNum)%EconoActive

  IF(Furnace(FurnaceNum)%ControlZoneMassFlowFrac .GT. 0.0d0)THEN
    QZnReq = ZoneLoad/Furnace(FurnaceNum)%ControlZoneMassFlowFrac
    MoistureLoad = MoistureLoad/Furnace(FurnaceNum)%ControlZoneMassFlowFrac
    ZoneLoad = QZnReq
  ELSE
    QZnReq = ZoneLoad
  END IF

! Original thermostat control logic (works only for cycling fan systems)
  IF(QZnReq .GT. SmallLoad .AND. QZnReq .GT. (Small5WLoad/Furnace(FurnaceNum)%ControlZoneMassFlowFrac) &
     .AND. .NOT. CurDeadbandOrSetback(Furnace(FurnaceNum)%ControlZoneNum))THEN
    HeatingLoad = .TRUE.
    CoolingLoad = .FALSE.
  ELSE IF(QZnReq .LT. (-1.d0 * SmallLoad) .AND. ABS(QZnReq) .GT. (Small5WLoad/Furnace(FurnaceNum)%ControlZoneMassFlowFrac) &
    .AND. .NOT. CurDeadbandOrSetback(Furnace(FurnaceNum)%ControlZoneNum))THEN
    HeatingLoad = .FALSE.
    CoolingLoad = .TRUE.
  ELSE
    HeatingLoad = .FALSE.
    CoolingLoad = .FALSE.
  END IF

  IF (Furnace(FurnaceNum)%FurnaceType_Num == UnitarySys_HeatPump_AirToAir   .OR.  &
     (Furnace(FurnaceNum)%FurnaceType_Num == UnitarySys_HeatPump_WaterToAir .AND. &
      (Furnace(FurnaceNum)%WatertoAirHPType == WatertoAir_Simple .OR.   &
       Furnace(FurnaceNum)%WatertoAirHPType == WatertoAir_VarSpeedEquationFit) )) THEN
      IF (MoistureLoad .LT. 0.d0 .AND. Furnace(FurnaceNum)%DehumidControlType_Num .EQ. DehumidControl_CoolReheat) THEN
         HPDehumidificationLoadFlag = .TRUE.
         HeatingLoad = .FALSE.
         CoolingLoad = .TRUE.
      ELSE
         HPDehumidificationLoadFlag = .FALSE.
      ENDIF
  ENDIF

! Check for heat only furnace
  IF(Furnace(FurnaceNum)%FurnaceType_Num .NE. Furnace_HeatOnly .AND. &
     Furnace(FurnaceNum)%FurnaceType_Num .NE. UnitarySys_HeatOnly)THEN

    IF (GetCurrentScheduleValue(Furnace(FurnaceNum)%SchedPtr) .gt. 0.0d0) THEN
      IF ( (HeatingLoad .OR. CoolingLoad) .OR. &
           (Furnace(FurnaceNum)%Humidistat .AND. MoistureLoad .LT. 0.0d0) ) THEN
        PartLoadRatio = 1.0d0
      ELSE
        PartLoadRatio = 0.0d0
      END IF
    ELSE
      PartLoadRatio = 0.0d0
    END IF
  ELSE
    PartLoadRatio   = 1.0d0
  END IF

  ! get current time step operating capacity of water and steam coils
  ! (dependent on entering water and steam temperature)
  IF(FirstHVACIteration) THEN
    IF(Furnace(FurnaceNum)%HeatingCoilType_Num == Coil_HeatingWater) THEN
      ! set water-side mass flow rates
      Node(Furnace(FurnaceNum)%HWCoilAirInletNode)%MassFlowRate = CompOnMassFlow
      mdot = Furnace(FurnaceNum)%MaxHeatCoilFluidFlow
      CALL SetComponentFlowRate(mdot, &
                                   Furnace(FurnaceNum)%CoilControlNode, &
                                   Furnace(FurnaceNum)%CoilOutletNode, &
                                   Furnace(FurnaceNum)%LoopNum, &
                                   Furnace(FurnaceNum)%LoopSide, &
                                   Furnace(FurnaceNum)%BranchNum, &
                                   Furnace(FurnaceNum)%CompNum )
      !     simulate water coil to find operating capacity
      CALL SimulateWaterCoilComponents(Furnace(FurnaceNum)%HeatingCoilName,FirstHVACIteration, &
                                       Furnace(FurnaceNum)%HeatingCoilIndex, QActual)
      Furnace(FurnaceNum)%DesignHeatingCapacity = QActual

    END IF ! from IF(Furnace(FurnaceNum)%HeatingCoilType_Num == Coil_HeatingWater) THEN

    IF(Furnace(FurnaceNum)%HeatingCoilType_Num == Coil_HeatingSteam) THEN
      ! set air-side and steam-side mass flow rates
      Node(Furnace(FurnaceNum)%HWCoilAirInletNode)%MassFlowRate = CompOnMassFlow
      mdot = Furnace(FurnaceNum)%MaxHeatCoilFluidFlow
      CALL SetComponentFlowRate(mdot, &
                                   Furnace(FurnaceNum)%CoilControlNode, &
                                   Furnace(FurnaceNum)%CoilOutletNode, &
                                   Furnace(FurnaceNum)%LoopNum, &
                                   Furnace(FurnaceNum)%LoopSide, &
                                   Furnace(FurnaceNum)%BranchNum, &
                                   Furnace(FurnaceNum)%CompNum )

      !     simulate steam coil to find operating capacity
      CALL SimulateSteamCoilComponents(Furnace(FurnaceNum)%HeatingCoilName, &
                                       FirstHVACIteration,    &
                                       1.0d0, & !QCoilReq, simulate any load > 0 to get max capacity of steam coil
                                       Furnace(FurnaceNum)%HeatingCoilIndex, QActual)
      Furnace(FurnaceNum)%DesignHeatingCapacity = GetSteamCoilCapacity(Furnace(FurnaceNum)%HeatingCoilType, &
                                                Furnace(FurnaceNum)%HeatingCoilName,ErrorsFound)

    END IF ! from IF(Furnace(FurnaceNum)%HeatingCoilType_Num == Coil_HeatingSteam) THEN

    IF(Furnace(FurnaceNum)%SuppHeatCoilType_Num == Coil_HeatingWater) THEN

      !     set air-side and steam-side mass flow rates
      Node(Furnace(FurnaceNum)%SuppCoilAirInletNode)%MassFlowRate = CompOnMassFlow
      mdot = Furnace(FurnaceNum)%MaxSuppCoilFluidFlow
      CALL SetComponentFlowRate(mdot, &
                                   Furnace(FurnaceNum)%SuppCoilControlNode, &
                                   Furnace(FurnaceNum)%SuppCoilOutletNode, &
                                   Furnace(FurnaceNum)%LoopNumSupp, &
                                   Furnace(FurnaceNum)%LoopSideSupp, &
                                   Furnace(FurnaceNum)%BranchNumSupp, &
                                   Furnace(FurnaceNum)%CompNumSupp )

      !     simulate water coil to find operating capacity
      CALL SimulateWaterCoilComponents(Furnace(FurnaceNum)%SuppHeatCoilName,FirstHVACIteration, &
                                       Furnace(FurnaceNum)%SuppHeatCoilIndex, QActual)
      Furnace(FurnaceNum)%DesignSuppHeatingCapacity = QActual

    END IF ! from IF(Furnace(FurnaceNum)%SuppHeatCoilType_Num == Coil_HeatingWater) THEN
    !
    IF(Furnace(FurnaceNum)%SuppHeatCoilType_Num == Coil_HeatingSteam) THEN
      !     set air-side and steam-side mass flow rates
      Node(Furnace(FurnaceNum)%SuppCoilAirInletNode)%MassFlowRate = CompOnMassFlow
      mdot = Furnace(FurnaceNum)%MaxSuppCoilFluidFlow
      CALL SetComponentFlowRate(mdot, &
                                Furnace(FurnaceNum)%SuppCoilControlNode, &
                                Furnace(FurnaceNum)%SuppCoilOutletNode, &
                                Furnace(FurnaceNum)%LoopNumSupp, &
                                Furnace(FurnaceNum)%LoopSideSupp, &
                                Furnace(FurnaceNum)%BranchNumSupp, &
                                Furnace(FurnaceNum)%CompNumSupp )

!     simulate steam coil to find operating capacity
      CALL SimulateSteamCoilComponents(Furnace(FurnaceNum)%SuppHeatCoilName, &
                                       FirstHVACIteration,    &
                                       1.0d0, & !QCoilReq, simulate any load > 0 to get max capacity of steam coil
                                       Furnace(FurnaceNum)%SuppHeatCoilIndex, QActual)
      Furnace(FurnaceNum)%DesignSuppHeatingCapacity = &
          GetSteamCoilCapacity(Furnace(FurnaceNum)%SuppHeatCoilType,Furnace(FurnaceNum)%SuppHeatCoilName,ErrorsFound)

    END IF ! from IF(Furnace(FurnaceNum)%SuppHeatCoilType_Num == Coil_HeatingSteam) THEN
  END IF ! from IF( FirstHVACIteration ) THEN

  IF( Furnace(FurnaceNum)%NumOfSpeedCooling > 0  ) THEN !BoS, variable-speed water source hp
    !Furnace(FurnaceNum)%IdleMassFlowRate = RhoAir*Furnace(FurnaceNum)%IdleVolumeAirRate
      NumOfSpeedCooling = Furnace(FurnaceNum)%NumOfSpeedCooling
      NumOfSpeedHeating = Furnace(FurnaceNum)%NumOfSpeedHeating
    ! IF MSHP system was not autosized and the fan is autosized, check that fan volumetric flow rate is greater than MSHP flow rates
      IF(Furnace(FurnaceNum)%CheckFanFlow)THEN
        CurrentModuleObject = 'AirLoopHVAC:UnitaryHeatPump:VariableSpeed'
        CALL GetFanVolFlow(Furnace(FurnaceNum)%FanIndex,Furnace(FurnaceNum)%FanVolFlow)
        IF(Furnace(FurnaceNum)%FanVolFlow .NE. AutoSize)THEN
    !     Check fan versus system supply air flow rates
          IF(Furnace(FurnaceNum)%FanVolFlow + 1d-10 .LT. &
             Furnace(FurnaceNum)%CoolVolumeFlowRate(NumOfSpeedCooling))THEN
            CALL ShowWarningError(TRIM(CurrentModuleObject)//' - air flow rate = ' &
                            //TRIM(TrimSigDigits(Furnace(FurnaceNum)%FanVolFlow,7))//' in fan object ' &
                            //' is less than the MSHP system air flow rate' &
                            //' when cooling is required ('// &
            TRIM(TrimSigDigits(Furnace(FurnaceNum)%CoolVolumeFlowRate(NumOfSpeedCooling),7))//').')
            CALL ShowContinueError(' The MSHP system flow rate when cooling is required is reset to the' &
                                  //' fan flow rate and the simulation continues.')
            CALL ShowContinueError(' Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Furnace(FurnaceNum)%Name))
            Furnace(FurnaceNum)%CoolVolumeFlowRate(NumOfSpeedCooling) = Furnace(FurnaceNum)%FanVolFlow
            ! Check flow rates in other speeds and ensure flow rates are not above the max flow rate
            Do i=NumOfSpeedCooling-1,1,-1
              If (Furnace(FurnaceNum)%CoolVolumeFlowRate(i) .GT. Furnace(FurnaceNum)%CoolVolumeFlowRate(i+1)) Then
                CALL ShowContinueError(' The MSHP system flow rate when cooling is required is reset to the' &
                     //' flow rate at higher speed and the simulation continues at Speed'//TrimSigDigits(i,0)//'.')
                CALL ShowContinueError(' Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Furnace(FurnaceNum)%Name))
                Furnace(FurnaceNum)%CoolVolumeFlowRate(i) = Furnace(FurnaceNum)%CoolVolumeFlowRate(i+1)
              End If
            End Do
          END IF
          IF(Furnace(FurnaceNum)%FanVolFlow + 1d-10 .LT. &
             Furnace(FurnaceNum)%HeatVolumeFlowRate(NumOfSpeedHeating))THEN
            CALL ShowWarningError(TRIM(CurrentModuleObject)//' - air flow rate = ' &
                            //TRIM(TrimSigDigits(Furnace(FurnaceNum)%FanVolFlow,7))//' in fan object ' &
                            //' is less than the MSHP system air flow rate' &
                            //' when heating is required ('// &
              TRIM(TrimSigDigits(Furnace(FurnaceNum)%HeatVolumeFlowRate(NumOfSpeedHeating),7))//').')
            CALL ShowContinueError(' The MSHP system flow rate when heating is required is reset to the' &
                                  //' fan flow rate and the simulation continues.')
            CALL ShowContinueError(' Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Furnace(FurnaceNum)%Name))
            Furnace(FurnaceNum)%HeatVolumeFlowRate(NumOfSpeedHeating) = Furnace(FurnaceNum)%FanVolFlow
            Do i=NumOfSpeedHeating-1,1,-1
              If (Furnace(FurnaceNum)%HeatVolumeFlowRate(i) .GT. Furnace(FurnaceNum)%HeatVolumeFlowRate(i+1)) Then
                CALL ShowContinueError(' The MSHP system flow rate when heating is required is reset to the' &
                     //' flow rate at higher speed and the simulation continues at Speed'//TrimSigDigits(i,0)//'.')
                CALL ShowContinueError(' Occurs in '//TRIM(CurrentModuleObject)//' system = '//TRIM(Furnace(FurnaceNum)%Name))
                Furnace(FurnaceNum)%HeatVolumeFlowRate(i) = Furnace(FurnaceNum)%HeatVolumeFlowRate(i+1)
              End If
            End Do
          END IF
          IF(Furnace(FurnaceNum)%FanVolFlow .LT. Furnace(FurnaceNum)%IdleVolumeAirRate .AND. &
             Furnace(FurnaceNum)%IdleVolumeAirRate .NE. 0.0d0)THEN
            CALL ShowWarningError(TRIM(CurrentModuleObject)//' - air flow rate = ' &
                       //TRIM(TrimSigDigits(Furnace(FurnaceNum)%FanVolFlow,7))//' in fan object ' &
                       //' is less than the MSHP system air flow rate when no ' &
                       //'heating or cooling is needed ('//TRIM(TrimSigDigits(Furnace(FurnaceNum)%IdleVolumeAirRate,7))//').')
            CALL ShowContinueError(' The MSHP system flow rate when no heating or cooling is needed is reset to the' &
                                  //' fan flow rate and the simulation continues.')
            CALL ShowContinueError(' Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Furnace(FurnaceNum)%Name))
            Furnace(FurnaceNum)%IdleVolumeAirRate = Furnace(FurnaceNum)%FanVolFlow
          END IF
          RhoAir = StdRhoAir
          ! set the mass flow rates from the reset volume flow rates
          Do I=1,NumOfSpeedCooling
            Furnace(FurnaceNum)%CoolMassFlowRate(i) = RhoAir*Furnace(FurnaceNum)%CoolVolumeFlowRate(i)
            IF(Furnace(FurnaceNum)%FanVolFlow .GT. 0.0d0)THEN
              Furnace(FurnaceNum)%MSCoolingSpeedRatio(i) = &
                  Furnace(FurnaceNum)%CoolVolumeFlowRate(i)/Furnace(FurnaceNum)%FanVolFlow
            END IF
          End Do
          Do I=1,NumOfSpeedHeating
            Furnace(FurnaceNum)%HeatMassFlowRate(i) = RhoAir*Furnace(FurnaceNum)%HeatVolumeFlowRate(i)
            IF(Furnace(FurnaceNum)%FanVolFlow .GT. 0.0d0)THEN
              Furnace(FurnaceNum)%MSHeatingSpeedRatio(i) = &
                  Furnace(FurnaceNum)%HeatVolumeFlowRate(i)/Furnace(FurnaceNum)%FanVolFlow
            END IF
          End Do
          Furnace(FurnaceNum)%IdleMassFlowRate = RhoAir*Furnace(FurnaceNum)%IdleVolumeAirRate
          IF(Furnace(FurnaceNum)%FanVolFlow .GT. 0.0d0)THEN
            Furnace(FurnaceNum)%IdleSpeedRatio = &
                Furnace(FurnaceNum)%IdleVolumeAirRate / Furnace(FurnaceNum)%FanVolFlow
          END IF
          ! set the node max and min mass flow rates based on reset volume flow rates
          Node(InNode)%MassFlowRateMax = MAX(Furnace(FurnaceNum)%CoolMassFlowRate(NumOfSpeedCooling), &
                                             Furnace(FurnaceNum)%HeatMassFlowRate(NumOfSpeedHeating))
          Node(InNode)%MassFlowRateMaxAvail = MAX(Furnace(FurnaceNum)%CoolMassFlowRate(NumOfSpeedCooling), &
                                                  Furnace(FurnaceNum)%HeatMassFlowRate(NumOfSpeedHeating))
          Node(InNode)%MassFlowRateMin = 0.0d0
          Node(InNode)%MassFlowRateMinAvail = 0.0d0
          Node(OutNode) = Node(InNode)
        END IF
      END IF

      Furnace(FurnaceNum)%CheckFanFlow = .FALSE.

    ! CALL SetOnOffMassFlowRateVSCoil(FurnaceNum, Furnace(FurnaceNum)%ControlZoneNum, FirstHVACIteration, &
    !        AirLoopNum, OnOffAirFlowRatio, OpMode, QZnReq, MoistureLoad, PartLoadRatio)
    CALL SetOnOffMassFlowRate(FurnaceNum, AirLoopNum, OnOffAirFlowRatio, OpMode, QZnReq, MoistureLoad, PartLoadRatio)
  ELSE
    CALL SetOnOffMassFlowRate(FurnaceNum, AirLoopNum, OnOffAirFlowRatio, OpMode, QZnReq, MoistureLoad, PartLoadRatio)
  END IF


! Check ventilation/fan load for constant fan systems to see if load to be met changes
! Same IF logic used in Subroutine SetAverageAirFlow to determine if unit is ON or OFF

  IF(OpMode .EQ. ContFanCycCoil .AND. GetCurrentScheduleValue(Furnace(FurnaceNum)%SchedPtr) .GT. 0.0d0 .AND. &
     ( (GetCurrentScheduleValue(Furnace(FurnaceNum)%FanAvailSchedPtr) .GT. 0.0d0 .OR. TurnFansOn) &
          .AND. .NOT. TurnFansOff) )THEN

    IF(Furnace(FurnaceNum)%NumOfSpeedCooling > 0) THEN
        CALL CalcVarSpeedHeatPump(FurnaceNum,.FALSE.,off,1,0.0d0,0.0d0,SensibleOutput,LatentOutput, &
                          0.0d0, 0.0d0, OnOffAirFlowRatio, SUPHEATERLOAD)
    ELSE
        CALL CalcFurnaceOutput(FurnaceNum,.FALSE.,0,Off,0.0d0,0.0d0,0.0d0,0.0d0,SensibleOutput,  &
           LatentOutput,OnOffAirFlowRatio,.FALSE.)
    END IF

    IF(Furnace(FurnaceNum)%ControlZoneMassFlowFrac .GT. 0.0d0)THEN
      IF (Furnace(FurnaceNum)%ZoneSequenceCoolingNum > 0 .and. Furnace(FurnaceNum)%ZoneSequenceHeatingNum > 0) THEN
        QToCoolSetPt=ZoneSysEnergyDemand(Furnace(FurnaceNum)%ControlZoneNum)% &
                       SequencedOutputRequiredToCoolingSP(Furnace(FurnaceNum)%ZoneSequenceCoolingNum)/ &
                       Furnace(FurnaceNum)%ControlZoneMassFlowFrac
        QToHeatSetPt=ZoneSysEnergyDemand(Furnace(FurnaceNum)%ControlZoneNum)% &
                       SequencedOutputRequiredToHeatingSP(Furnace(FurnaceNum)%ZoneSequenceHeatingNum)/ &
                       Furnace(FurnaceNum)%ControlZoneMassFlowFrac
      ELSE
        QToCoolSetPt=ZoneSysEnergyDemand(Furnace(FurnaceNum)%ControlZoneNum)%OutputRequiredToCoolingSP/ &
                   Furnace(FurnaceNum)%ControlZoneMassFlowFrac
        QToHeatSetPt=ZoneSysEnergyDemand(Furnace(FurnaceNum)%ControlZoneNum)%OutputRequiredToHeatingSP/ &
                   Furnace(FurnaceNum)%ControlZoneMassFlowFrac
      ENDIF
!     If the furnace has a net cooling capacity (SensibleOutput < 0) and
!     the zone temp is above the Tstat heating setpoint (QToHeatSetPt < 0) and
!     the net cooling capacity does not just offset the cooling load
      IF(SensibleOutput .LT. 0.0d0 .AND. QToHeatSetPt .LT. 0.0d0 .AND.  &
         ABS(QToCoolSetPt - SensibleOutput) .GT. (Small5WLoad/Furnace(FurnaceNum)%ControlZoneMassFlowFrac))THEN
!       Only switch modes when humidistat is not used or no moisture load exists, otherwise let
!       reheat coil pick up load
!        IF((SensibleOutput .LT. QToHeatSetPt .AND. .NOT. Furnace(FurnaceNum)%Humidistat) .OR. &
!           (SensibleOutput .LT. QToHeatSetPt .AND. Furnace(FurnaceNum)%Humidistat .AND. MoistureLoad .GE. 0.0))THEN
        IF((SensibleOutput .LT. QToHeatSetPt .AND. .NOT. Furnace(FurnaceNum)%Humidistat) .OR. &
           (SensibleOutput .LT. QToHeatSetPt .AND. Furnace(FurnaceNum)%Humidistat .AND. MoistureLoad .GE. 0.0d0))THEN
          QZnReq = QToHeatSetPt
          CoolingLoad = .FALSE.
!         Don't set mode TRUE unless mode is allowed. Also check for floating zone.
          IF(TempControlType(Furnace(FurnaceNum)%ControlZoneNum) .EQ. SingleCoolingSetPoint .OR. &
             TempControlType(Furnace(FurnaceNum)%ControlZoneNum) .EQ. 0)THEN
            HeatingLoad = .FALSE.
          ELSE
            HeatingLoad = .TRUE.
          END IF

         IF(Furnace(FurnaceNum)%NumOfSpeedCooling > 0) THEN
           !CALL SetOnOffMassFlowRateVSCoil(FurnaceNum, Furnace(FurnaceNum)%ControlZoneNum, FirstHVACIteration, &
           !         AirLoopNum, OnOffAirFlowRatio, OpMode, QZnReq, MoistureLoad, PartLoadRatio)
           CALL SetOnOffMassFlowRate(FurnaceNum, AirLoopNum, OnOffAirFlowRatio, OpMode, QZnReq, MoistureLoad, PartLoadRatio)
           CALL CalcVarSpeedHeatPump(FurnaceNum,.FALSE.,off,1,0.0d0,0.0d0,SensibleOutput,LatentOutput, &
                          0.0d0, 0.0d0, OnOffAirFlowRatio, SUPHEATERLOAD)
         ELSE
          CALL SetOnOffMassFlowRate(FurnaceNum, AirLoopNum, OnOffAirFlowRatio, OpMode, QZnReq, MoistureLoad, PartLoadRatio)
          CALL CalcFurnaceOutput(FurnaceNum,.FALSE.,0,Off,0.0d0,0.0d0,0.0d0,0.0d0,SensibleOutput,LatentOutput, &
                                 OnOffAirFlowRatio,.FALSE.)
         END IF
          IF(SensibleOutput .GT. QToHeatSetPt)THEN
!           If changing operating mode (flow rates) does not overshoot heating setpoint, turn off heating
            QZnReq       = 0.0d0
            HeatingLoad  = .FALSE.
            IF(Furnace(FurnaceNum)%NumOfSpeedCooling > 0) THEN
                CALL SetOnOffMassFlowRate(FurnaceNum, AirLoopNum, OnOffAirFlowRatio, OpMode, QZnReq, MoistureLoad, PartLoadRatio)
!               CALL SetOnOffMassFlowRateVSCoil(FurnaceNum, Furnace(FurnaceNum)%ControlZoneNum, FirstHVACIteration, &
!                    AirLoopNum, OnOffAirFlowRatio, OpMode, QZnReq, MoistureLoad, PartLoadRatio)
            ELSE
              CALL SetOnOffMassFlowRate(FurnaceNum, AirLoopNum, OnOffAirFlowRatio, OpMode, QZnReq, MoistureLoad, PartLoadRatio)
            END IF
          END IF
        ELSE IF(SensibleOutput .LT. QZnReq)THEN
!         If the net cooling capacity meets the zone cooling load but does not overshoot heating setpoint, turn off cooling
!         (dehumidification may still occur)
          QZnReq       = 0.0d0
          CoolingLoad  = .FALSE.
          IF (HPDehumidificationLoadFlag) THEN
             CoolingLoad = .TRUE.
             HeatingLoad = .FALSE.
           ENDIF
           IF(Furnace(FurnaceNum)%NumOfSpeedCooling > 0) THEN
                CALL SetOnOffMassFlowRate(FurnaceNum, AirLoopNum, OnOffAirFlowRatio, OpMode, QZnReq, &
                    MoistureLoad, PartLoadRatio)
!               CALL SetOnOffMassFlowRateVSCoil(FurnaceNum, Furnace(FurnaceNum)%ControlZoneNum, FirstHVACIteration, &
!                    AirLoopNum, OnOffAirFlowRatio, OpMode, QZnReq, MoistureLoad, PartLoadRatio)
           ELSE
               CALL SetOnOffMassFlowRate(FurnaceNum, AirLoopNum, OnOffAirFlowRatio, OpMode, QZnReq, MoistureLoad,PartLoadRatio)
           END IF
        END IF
!     the net cooling capacity just offsets the cooling load, turn off cooling
      ELSE IF(SensibleOutput .LT. 0.0d0 .AND. QToCoolSetPt .LT. 0.0d0 .AND.  &
              ABS(QToCoolSetPt - SensibleOutput) .LT. (Small5WLoad/Furnace(FurnaceNum)%ControlZoneMassFlowFrac))THEN
        CoolingLoad = .FALSE.
        IF (HPDehumidificationLoadFlag) THEN
            CoolingLoad = .TRUE.
            HeatingLoad = .FALSE.
         ENDIF
      END IF ! SensibleOutput .LT. 0.0d0 .AND. QToHeatSetPt .LT. 0.0d0

!     If the furnace has a net heating capacity and the zone temp is below the Tstat cooling setpoint and
!     the net heating capacity does not just offset the heating load
      IF(SensibleOutput .GT. 0.0d0 .AND. QToCoolSetPt .GT. 0.0d0 .AND.  &
         ABS(SensibleOutput - QToHeatSetPt) .GT. (Small5WLoad/Furnace(FurnaceNum)%ControlZoneMassFlowFrac))THEN
        IF(SensibleOutput .GT. QToCoolSetPt)THEN
          QZnReq = QToCoolSetPt
!         Don't set mode TRUE unless mode is allowed. Also check for floating zone.
          IF(TempControlType(Furnace(FurnaceNum)%ControlZoneNum) .EQ. SingleHeatingSetPoint .OR. &
             TempControlType(Furnace(FurnaceNum)%ControlZoneNum) .EQ. 0)THEN
            CoolingLoad = .FALSE.
          ELSE
            CoolingLoad = .TRUE.
          END IF
          HeatingLoad = .FALSE.

         IF(Furnace(FurnaceNum)%NumOfSpeedCooling > 0) THEN
            CALL SetOnOffMassFlowRate(FurnaceNum, AirLoopNum, OnOffAirFlowRatio, OpMode, QZnReq, &
             MoistureLoad, PartLoadRatio)
!           CALL SetOnOffMassFlowRateVSCoil(FurnaceNum, Furnace(FurnaceNum)%ControlZoneNum, FirstHVACIteration, &
!                    AirLoopNum, OnOffAirFlowRatio, OpMode, QZnReq, MoistureLoad, PartLoadRatio)
           CALL CalcVarSpeedHeatPump(FurnaceNum,.FALSE.,off,1,0.0d0,0.0d0,SensibleOutput,LatentOutput, &
                          0.0d0, 0.0d0, OnOffAirFlowRatio, SUPHEATERLOAD)
         ELSE
          CALL SetOnOffMassFlowRate(FurnaceNum, AirLoopNum, OnOffAirFlowRatio, OpMode, QZnReq, MoistureLoad,PartLoadRatio)
          CALL CalcFurnaceOutput(FurnaceNum,.FALSE.,0,Off,0.0d0,0.0d0,0.0d0,0.0d0,SensibleOutput,LatentOutput, &
                                 OnOffAirFlowRatio,.FALSE.)
         END IF
          IF(SensibleOutput .LT. QToCoolSetPt)THEN
!           If changing operating mode (flow rates) does not overshoot cooling setpoint, turn off cooling
            IF (HPDehumidificationLoadFlag) THEN
                CoolingLoad = .TRUE.
                HeatingLoad = .FALSE.
            ELSE
                QZnReq       = 0.0d0
                CoolingLoad  = .FALSE.
            ENDIF
            IF(Furnace(FurnaceNum)%NumOfSpeedCooling > 0) THEN
                CALL SetOnOffMassFlowRate(FurnaceNum, AirLoopNum, OnOffAirFlowRatio, OpMode, &
                    QZnReq, MoistureLoad, PartLoadRatio)
!               CALL SetOnOffMassFlowRateVSCoil(FurnaceNum, Furnace(FurnaceNum)%ControlZoneNum, FirstHVACIteration, &
!                     AirLoopNum, OnOffAirFlowRatio, OpMode, QZnReq, MoistureLoad, PartLoadRatio)
            ELSE
             CALL SetOnOffMassFlowRate(FurnaceNum, AirLoopNum, OnOffAirFlowRatio, OpMode, QZnReq, MoistureLoad,PartLoadRatio)
            END IF
          END IF
        ELSE IF(SensibleOutput .GT. QZnReq)THEN
!         If the net heating capacity meets the zone heating load but does not overshoot, turn off heating
          QZnReq       = 0.0d0
          HeatingLoad = .FALSE.
          IF(Furnace(FurnaceNum)%NumOfSpeedCooling > 0) THEN
!            CALL SetOnOffMassFlowRateVSCoil(FurnaceNum, Furnace(FurnaceNum)%ControlZoneNum, FirstHVACIteration, &
!                        AirLoopNum, OnOffAirFlowRatio, OpMode, QZnReq, MoistureLoad, PartLoadRatio)
            CALL SetOnOffMassFlowRate(FurnaceNum, AirLoopNum, OnOffAirFlowRatio, &
                 OpMode, QZnReq, MoistureLoad, PartLoadRatio)
          ELSE
            CALL SetOnOffMassFlowRate(FurnaceNum, AirLoopNum, OnOffAirFlowRatio, OpMode, QZnReq, MoistureLoad,PartLoadRatio)
          END IF
        END IF
!     the net heating capacity just offsets the heating load, turn off heating
      ELSE IF (SensibleOutput .GT. 0.0d0 .AND. QToHeatSetPt .GT. 0.0d0 .AND.  &
               ABS(SensibleOutput - QToHeatSetPt) .LT. (Small5WLoad/Furnace(FurnaceNum)%ControlZoneMassFlowFrac))THEN
        HeatingLoad = .FALSE.
      END IF ! SensibleOutput .GT. 0.0d0 .AND. QToCoolSetPt .GT. 0.0d0
    END IF ! Furnace(FurnaceNum)%ControlZoneMassFlowFrac .GT. 0.0d0
    ZoneLoad = QZnReq
  END IF ! OpMode .EQ. ContFanCycCoil

  ! EMS override point
  IF (Furnace(FurnaceNum)%EMSOverrideSensZoneLoadRequest) ZoneLoad = Furnace(FurnaceNum)%EMSSensibleZoneLoadValue
  IF (Furnace(FurnaceNum)%EMSOverrideMoistZoneLoadRequest) MoistureLoad = Furnace(FurnaceNum)%EMSMoistureZoneLoadValue
  IF (Furnace(FurnaceNum)%EMSOverrideSensZoneLoadRequest .OR. Furnace(FurnaceNum)%EMSOverrideMoistZoneLoadRequest) THEN
    IF ((ZoneLoad /= 0.0D0) .AND. (Furnace(FurnaceNum)%EMSOverrideSensZoneLoadRequest)) THEN
      PartLoadRatio = 1.0D0
    ELSEIF ((MoistureLoad /= 0.0D0) .AND. (Furnace(FurnaceNum)%EMSOverrideMoistZoneLoadRequest)) THEN
      PartLoadRatio = 1.0D0
    ELSE
      PartLoadRatio = 0.0D0
    ENDIF
    IF(Furnace(FurnaceNum)%NumOfSpeedCooling > 0) THEN
        CALL SetOnOffMassFlowRate(FurnaceNum, AirLoopNum, OnOffAirFlowRatio, OpMode, &
         QZnReq, MoistureLoad, PartLoadRatio)
!       CALL SetOnOffMassFlowRateVSCoil(FurnaceNum, Furnace(FurnaceNum)%ControlZoneNum, FirstHVACIteration, &
!                AirLoopNum, OnOffAirFlowRatio, OpMode, QZnReq, MoistureLoad, PartLoadRatio)
    ELSE
       CALL SetOnOffMassFlowRate(FurnaceNum, AirLoopNum, OnOffAirFlowRatio, OpMode, ZoneLoad, MoistureLoad,PartLoadRatio)
    END IF
  ENDIF


! AirflowNetwork global variable
  LoopHeatingCoilMaxRTF = 0.0d0
  RETURN

END SUBROUTINE InitFurnace

SUBROUTINE SetOnOffMassFlowRate(FurnaceNum, AirLoopNum, OnOffAirFlowRatio, OpMode, ZoneLoad, MoistureLoad, PartLoadRatio)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Raustad
          !       DATE WRITTEN   Sep 2008
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

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER,   INTENT(IN)    :: FurnaceNum        ! index to furnace
  INTEGER,   INTENT(IN)    :: AirLoopNum        ! index to air loop !unused1208
  REAL(r64), INTENT(INOUT) :: OnOffAirFlowRatio ! ratio of coil on to coil off air flow rate
  INTEGER,   INTENT(IN)    :: OpMode            ! fan operating mode
  REAL(r64), INTENT(IN)    :: ZoneLoad          ! sensible load to be met (W) !unused1208
  REAL(r64), INTENT(IN)    :: MoistureLoad      ! moisture load to be met (W)
  REAL(r64), INTENT(IN)    :: PartLoadRatio     ! coil part-load ratio

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          ! na

          ! FLOW:
! Check for heat only furnace
  IF(Furnace(FurnaceNum)%FurnaceType_Num .NE. Furnace_HeatOnly .AND. &
     Furnace(FurnaceNum)%FurnaceType_Num .NE. UnitarySys_HeatOnly)THEN

    ! Set the system mass flow rates
    IF (OpMode .EQ. ContFanCycCoil) THEN
    ! Set the compressor or coil ON mass flow rate
    ! constant fan mode
      IF ( HeatingLoad ) THEN
!       IF a heating and moisture load exists, operate at the cooling mass flow rate ELSE operate at the heating flow rate
        IF(MoistureLoad .LT. 0.0d0 .AND. Furnace(FurnaceNum)%Humidistat .AND. &
           Furnace(FurnaceNum)%DehumidControlType_Num == DehumidControl_CoolReheat)THEN
          CompOnMassFlow = Furnace(FurnaceNum)%MaxCoolAirMassFlow
          CompOnFlowRatio = Furnace(FurnaceNum)%CoolingSpeedRatio
        ELSE
          CompOnMassFlow = Furnace(FurnaceNum)%MaxHeatAirMassFlow
          CompOnFlowRatio = Furnace(FurnaceNum)%HeatingSpeedRatio
        END IF
        Furnace(FurnaceNum)%LastMode = HeatingMode
!     IF a cooling load exists, operate at the cooling mass flow rate
      ELSE IF ( CoolingLoad ) THEN
        CompOnMassFlow = Furnace(FurnaceNum)%MaxCoolAirMassFlow
        CompOnFlowRatio = Furnace(FurnaceNum)%CoolingSpeedRatio
        Furnace(FurnaceNum)%LastMode = CoolingMode
!     If no load exists, set the compressor on mass flow rate.
!     Set equal the mass flow rate when no heating or cooling is needed if no moisture load exists.
!     If the user has set the off mass flow rate to 0, set according to the last operating mode.
      ELSE
        IF(MoistureLoad .LT. 0.0d0 .AND. Furnace(FurnaceNum)%Humidistat .AND. &
           Furnace(FurnaceNum)%DehumidControlType_Num == DehumidControl_CoolReheat)THEN
          CompOnMassFlow = Furnace(FurnaceNum)%MaxCoolAirMassFlow
          CompOnFlowRatio = Furnace(FurnaceNum)%CoolingSpeedRatio
        ELSE
          CompOnMassFlow = Furnace(FurnaceNum)%MaxNoCoolHeatAirMassFlow
          CompOnFlowRatio = Furnace(FurnaceNum)%HeatingSpeedRatio
!         User may have entered a 0 for MaxNoCoolHeatAirMassFlow
          IF(CompOnMassFlow .EQ. 0.0d0)THEN
            IF(Furnace(FurnaceNum)%LastMode .EQ. HeatingMode)THEN
              CompOnMassFlow = Furnace(FurnaceNum)%MaxHeatAirMassFlow
              CompOnFlowRatio = Furnace(FurnaceNum)%HeatingSpeedRatio
            ELSE
              CompOnMassFlow = Furnace(FurnaceNum)%MaxCoolAirMassFlow
              CompOnFlowRatio = Furnace(FurnaceNum)%CoolingSpeedRatio
            END IF
          END IF
        END IF
      END IF

!     Set the compressor or coil OFF mass flow rate based on LOGICAL flag
!     UseCompressorOnFlow is used when the user does not enter a value for no cooling or heating flow rate
      IF (Furnace(FurnaceNum)%AirFlowControl .EQ. UseCompressorOnFlow) THEN
        IF (Furnace(FurnaceNum)%LastMode .EQ. HeatingMode) THEN
          IF(MoistureLoad .LT. 0.0d0 .AND. Furnace(FurnaceNum)%Humidistat .AND. &
           Furnace(FurnaceNum)%DehumidControlType_Num == DehumidControl_CoolReheat)THEN
            CompOffMassFlow = Furnace(FurnaceNum)%MaxCoolAirMassFlow
            CompOffFlowRatio = Furnace(FurnaceNum)%CoolingSpeedRatio
          ELSE
            CompOffMassFlow = Furnace(FurnaceNum)%MaxHeatAirMassFlow
            CompOffFlowRatio = Furnace(FurnaceNum)%HeatingSpeedRatio
          END IF
        ELSE
          CompOffMassFlow = Furnace(FurnaceNum)%MaxCoolAirMassFlow
          CompOffFlowRatio = Furnace(FurnaceNum)%CoolingSpeedRatio
        END IF
!     ELSE use the user specified value
      ELSE
        CompOffMassFlow = Furnace(FurnaceNum)%MaxNoCoolHeatAirMassFlow
        CompOffFlowRatio = Furnace(FurnaceNum)%NoHeatCoolSpeedRatio
      END IF
    ELSE
!     cycling fan mode
      IF ( HeatingLoad .OR. &
           (Furnace(FurnaceNum)%Humidistat .AND. MoistureLoad .LT. 0.0d0 .AND. &
           Furnace(FurnaceNum)%DehumidControlType_Num == DehumidControl_CoolReheat ) ) THEN

        IF(Furnace(FurnaceNum)%Humidistat .AND. MoistureLoad .LT. 0.0d0 .AND. &
           Furnace(FurnaceNum)%DehumidControlType_Num == DehumidControl_CoolReheat)THEN
          CompOnMassFlow = Furnace(FurnaceNum)%MaxCoolAirMassFlow
          CompOnFlowRatio = Furnace(FurnaceNum)%CoolingSpeedRatio
          Furnace(FurnaceNum)%LastMode = CoolingMode
        ELSE
          CompOnMassFlow = Furnace(FurnaceNum)%MaxHeatAirMassFlow
          CompOnFlowRatio = Furnace(FurnaceNum)%HeatingSpeedRatio
          Furnace(FurnaceNum)%LastMode = HeatingMode
        END IF
      ELSE IF ( CoolingLoad ) THEN
        CompOnMassFlow = Furnace(FurnaceNum)%MaxCoolAirMassFlow
        CompOnFlowRatio = Furnace(FurnaceNum)%CoolingSpeedRatio
      ELSE
        CompOnMassFlow = 0.0d0
        CompOnFlowRatio = 0.0d0
      END IF
      CompOffMassFlow = 0.0d0
      CompOffFlowRatio = 0.0d0
    END IF
  ELSE !  Is a HeatOnly furnace

    CompOnMassFlow  = Furnace(FurnaceNum)%DesignMassFlowRate
    CompOnFlowRatio = Furnace(FurnaceNum)%HeatingSpeedRatio
    IF(OpMode .EQ. ContFanCycCoil)THEN
      CompOffMassFlow = Furnace(FurnaceNum)%MaxNoCoolHeatAirMassFlow
      CompOffFlowRatio = Furnace(FurnaceNum)%HeatingSpeedRatio
    ELSE
      CompOffMassFlow = 0.0d0
      CompOffFlowRatio = 0.0d0
    END IF

  END IF ! End check for heat only furnace or water-to-air heat pump


! Set the system mass flow rates
  CALL SetAverageAirFlow(FurnaceNum, PartLoadRatio, OnOffAirFlowRatio)

END SUBROUTINE SetOnOffMassFlowRate

SUBROUTINE SizeFurnace(FurnaceNum,FirstHVACIteration)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   January 2002
          !       MODIFIED       Bereket Nigusse, May 2010, removed the autosize option for the input field supply air
          !                                                 flow fraction through controlled zone.
          !                      Bo Shen, March 2012, size the air flow rates at individual speed levels for VS WSHP
          !                      Bo Shen, ORNL, July 2012 - added variable-speed air source heat pump cooling and heating coils, using curve-fits
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is for sizing Furnace Components for which nominal cpacities
          ! and flow rates have not been specified in the input

          ! METHODOLOGY EMPLOYED:
          ! Obtains heating capacities and flow rates from the zone or system sizing arrays.
          !
          ! NOTE: In UNITARYSYSTEM:HEATPUMP:AIRTOAIR we are sizing the heating capacity to be
          ! equal to the cooling capacity.  Thus the cooling and
          ! and heating capacities of a DX heat pump system will be identical. In real life the ARI
          ! heating and cooling capacities are close but not identical.



          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataSizing
  USE General,                   ONLY: TrimSigDigits
  USE BranchInputManager,        ONLY: CheckSystemBranchFlow
  USE HVACHXAssistedCoolingCoil, ONLY: SimHXAssistedCoolingCoil
  USE WatertoAirheatPumpSimple,  ONLY: SimWatertoAirHPSimple
  USE VariableSpeedCoils,        ONLY: SimVariableSpeedCoils, VarSpeedCoil
  USE ReportSizingManager,       ONLY: ReportSizingOutput
  USE EMSManager,                ONLY: ManageEMS
  USE DataGlobals,               ONLY: emsCallFromUnitarySystemSizing

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  Integer, Intent(IN) :: FurnaceNum
  LOGICAL, Intent(IN) :: FirstHVACIteration

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64) :: ControlZoneVolFlow
  INTEGER :: ThisCtrlZoneNum ! the controlled zone number of the control zone !!!
  INTEGER :: ControlledZoneNum
  INTEGER :: Iter !iteration count
  REAL(r64) :: MulSpeedFlowScale ! variable speed air flow scaling factor
  LOGICAL :: ErrFound ! flag returned from mining functions
  REAL(r64) :: BranchFlow ! branch volumetric flow rate [m3/s]

  CALL ManageEMS(emsCallFromUnitarySystemSizing) ! calling point

  ThisCtrlZoneNum = 0
  DXCoolCap = 0.0d0
  UnitaryHeatCap = 0.0d0
  SuppHeatCap = 0.0d0
  IF (Furnace(FurnaceNum)%CoolingCoilType_Num == CoilDX_CoolingSingleSpeed) THEN
    CALL SimDXCoil(Blank,On,.TRUE.,0.0d0,Furnace(FurnaceNum)%CoolingCoilIndex, 1)
  ELSE IF (Furnace(FurnaceNum)%CoolingCoilType_Num == CoilDX_CoolingHXAssisted) THEN
    CALL SimHXAssistedCoolingCoil(Blank,.TRUE.,On, &
                                  0.0d0, Furnace(FurnaceNum)%CoolingCoilIndex, 1, &
                                  HXUnitEnable=.FALSE., OnOffAFR = 1.0d0, EconomizerFlag=.FALSE.)
  ELSE IF (Furnace(FurnaceNum)%CoolingCoilType_Num == Coil_CoolingWaterToAirHPSimple) THEN
    CALL SimWatertoAirHPSimple(Blank, &
           Furnace(FurnaceNum)%CoolingCoilIndex, &
           Furnace(FurnaceNum)%CoolingCoilSensDemand, Furnace(FurnaceNum)%CoolingCoilLatentDemand, &
           0,0.0d0, Furnace(FurnaceNum)%MaxONOFFCyclesperHour, &  !CoolPartLoadRatio
           Furnace(FurnaceNum)%HPTimeConstant, Furnace(FurnaceNum)%FanDelayTime, 0, 0.0d0, FirstHVACIteration)
  ELSE IF (Furnace(FurnaceNum)%CoolingCoilType_Num == Coil_CoolingWaterToAirHPVSEquationFit .OR. &
            Furnace(FurnaceNum)%CoolingCoilType_Num == Coil_CoolingAirToAirVariableSpeed ) THEN
    CALL SimVariableSpeedCoils(Blank,Furnace(FurnaceNum)%CoolingCoilIndex,&
           0,Furnace(FurnaceNum)%MaxONOFFCyclesperHour, &
           Furnace(FurnaceNum)%HPTimeConstant,Furnace(FurnaceNum)%FanDelayTime,&
           0, 0.0d0, 0.0d0,1, 0.0d0,0.0d0, 0.0d0 )     !conduct the sizing operation in the VS WSHP
    Furnace(FurnaceNum)%NumOfSpeedCooling = VarSpeedCoil(Furnace(FurnaceNum)%CoolingCoilIndex)%NumOfSpeeds

    MulSpeedFlowScale = VarSpeedCoil(Furnace(FurnaceNum)%CoolingCoilIndex)%RatedAirVolFlowRate/ &
       VarSpeedCoil(Furnace(FurnaceNum)%CoolingCoilIndex)%MSRatedAirVolFlowRate(  &
          VarSpeedCoil(Furnace(FurnaceNum)%CoolingCoilIndex)%NormSpedLevel)
    Do Iter = 1,Furnace(FurnaceNum)%NumOfSpeedCooling
      Furnace(FurnaceNum)%CoolVolumeFlowRate(Iter) =   &
         VarSpeedCoil(Furnace(FurnaceNum)%CoolingCoilIndex)%MSRatedAirVolFlowRate(Iter) * MulSpeedFlowScale
      Furnace(FurnaceNum)%CoolMassFlowRate(Iter) =   &
         VarSpeedCoil(Furnace(FurnaceNum)%CoolingCoilIndex)%MSRatedAirMassFlowRate(Iter) * MulSpeedFlowScale
      Furnace(FurnaceNum)%MSCoolingSpeedRatio(Iter) =   &
         VarSpeedCoil(Furnace(FurnaceNum)%CoolingCoilIndex)%MSRatedAirVolFlowRate(Iter)/ &
                   VarSpeedCoil(Furnace(FurnaceNum)%CoolingCoilIndex)%MSRatedAirVolFlowRate(Furnace(FurnaceNum)%NumOfSpeedCooling)
    End Do

    IF (Furnace(FurnaceNum)%HeatingCoilType_Num == Coil_HeatingWaterToAirHPVSEquationFit .OR. &
            Furnace(FurnaceNum)%HeatingCoilType_Num == Coil_HeatingAirToAirVariableSpeed) THEN
        CALL SimVariableSpeedCoils(Blank,Furnace(FurnaceNum)%HeatingCoilIndex,&
               0,Furnace(FurnaceNum)%MaxONOFFCyclesperHour, &
               Furnace(FurnaceNum)%HPTimeConstant,Furnace(FurnaceNum)%FanDelayTime,&
               0, 0.0d0, 0.0d0,1, 0.0d0,0.0d0, 0.0d0 ) !conduct the sizing operation in the VS WSHP

        Furnace(FurnaceNum)%NumOfSpeedHeating = VarSpeedCoil(Furnace(FurnaceNum)%HeatingCoilIndex)%NumOfSpeeds

        MulSpeedFlowScale = VarSpeedCoil(Furnace(FurnaceNum)%HeatingCoilIndex)%RatedAirVolFlowRate/ &
                        VarSpeedCoil(Furnace(FurnaceNum)%HeatingCoilIndex)%MSRatedAirVolFlowRate(  &
                           VarSpeedCoil(Furnace(FurnaceNum)%HeatingCoilIndex)%NormSpedLevel)
        Do Iter = 1,Furnace(FurnaceNum)%NumOfSpeedHeating
          Furnace(FurnaceNum)%HeatVolumeFlowRate(Iter) =   &
             VarSpeedCoil(Furnace(FurnaceNum)%HeatingCoilIndex)%MSRatedAirVolFlowRate(Iter) * MulSpeedFlowScale
          Furnace(FurnaceNum)%HeatMassFlowRate(Iter) =   &
             VarSpeedCoil(Furnace(FurnaceNum)%HeatingCoilIndex)%MSRatedAirMassFlowRate(Iter) * MulSpeedFlowScale
          Furnace(FurnaceNum)%MSHeatingSpeedRatio(Iter) =   &
             VarSpeedCoil(Furnace(FurnaceNum)%HeatingCoilIndex)%MSRatedAirVolFlowRate(Iter)/ &
                VarSpeedCoil(Furnace(FurnaceNum)%HeatingCoilIndex)%MSRatedAirVolFlowRate&
                (Furnace(FurnaceNum)%NumOfSpeedHeating)
        End Do
    END IF

    IF(Furnace(FurnaceNum)%NumOfSpeedHeating > 0) THEN
       Furnace(FurnaceNum)%IdleMassFlowRate = &
            min(Furnace(FurnaceNum)%HeatMassFlowRate(1), Furnace(FurnaceNum)%CoolMassFlowRate(1))
       Furnace(FurnaceNum)%IdleSpeedRatio = &
            min(Furnace(FurnaceNum)%MSHeatingSpeedRatio(1), Furnace(FurnaceNum)%MSCoolingSpeedRatio(1))
       Furnace(FurnaceNum)%IdleVolumeAirRate = &
            min(Furnace(FurnaceNum)%HeatVolumeFlowRate(1), Furnace(FurnaceNum)%CoolVolumeFlowRate(1))
    ELSE
       Furnace(FurnaceNum)%IdleMassFlowRate = Furnace(FurnaceNum)%CoolMassFlowRate(1)
       Furnace(FurnaceNum)%IdleSpeedRatio =   Furnace(FurnaceNum)%MSCoolingSpeedRatio(1)
       Furnace(FurnaceNum)%IdleVolumeAirRate = Furnace(FurnaceNum)%CoolVolumeFlowRate(1)
    END IF

    IF (Furnace(FurnaceNum)%OpMode .EQ. ContFanCycCoil) THEN
        Furnace(FurnaceNum)%MaxNoCoolHeatAirVolFlow = Furnace(FurnaceNum)%IdleVolumeAirRate
        Furnace(FurnaceNum)%MaxNoCoolHeatAirMassFlow = Furnace(FurnaceNum)%IdleMassFlowRate
        Furnace(FurnaceNum)%NoHeatCoolSpeedRatio= Furnace(FurnaceNum)%IdleSpeedRatio
    END IF

  END IF

  IF (Furnace(FurnaceNum)%DesignFanVolFlowRate == AutoSize) THEN

    IF (CurSysNum > 0) THEN

      CALL CheckSysSizing(cFurnaceTypes(Furnace(FurnaceNum)%FurnaceType_Num), Furnace(FurnaceNum)%Name)
      IF (FinalSysSizing(CurSysNum)%DesMainVolFlow >= SmallAirVolFlow) THEN
        Furnace(FurnaceNum)%DesignFanVolFlowRate = FinalSysSizing(CurSysNum)%DesMainVolFlow
      ELSE
        Furnace(FurnaceNum)%DesignFanVolFlowRate = 0.0d0
      END IF

      IF (Furnace(FurnaceNum)%DesignFanVolFlowRateEMSOverrideOn) THEN
        Furnace(FurnaceNum)%DesignFanVolFlowRate = Furnace(FurnaceNum)%DesignFanVolFlowRateEMSOverrideValue
      ENDIF

      CALL ReportSizingOutput(cFurnaceTypes(Furnace(FurnaceNum)%FurnaceType_Num), Furnace(FurnaceNum)%Name, &
                              'Supply Air Flow Rate [m3/s]', &
                              Furnace(FurnaceNum)%DesignFanVolFlowRate)

    END IF

  END IF

  IF (Furnace(FurnaceNum)%MaxHeatAirVolFlow == AutoSize) THEN

    IF (CurSysNum > 0) THEN

      CALL CheckSysSizing(cFurnaceTypes(Furnace(FurnaceNum)%FurnaceType_Num), Furnace(FurnaceNum)%Name)
      IF (FinalSysSizing(CurSysNum)%DesMainVolFlow >= SmallAirVolFlow) THEN
        Furnace(FurnaceNum)%MaxHeatAirVolFlow = FinalSysSizing(CurSysNum)%DesMainVolFlow
      ELSE
        Furnace(FurnaceNum)%MaxHeatAirVolFlow = 0.0d0
      END IF

      IF (Furnace(FurnaceNum)%MaxHeatAirVolFlowEMSOverrideOn) THEN
        Furnace(FurnaceNum)%MaxHeatAirVolFlow = Furnace(FurnaceNum)%MaxHeatAirVolFlowEMSOverrideValue
      ENDIF
      CALL ReportSizingOutput(cFurnaceTypes(Furnace(FurnaceNum)%FurnaceType_Num), Furnace(FurnaceNum)%Name, &
                               'Supply Air Flow Rate During Heating Operation [m3/s]', &
                                Furnace(FurnaceNum)%MaxHeatAirVolFlow)

    END IF

  END IF

  IF (Furnace(FurnaceNum)%MaxCoolAirVolFlow == AutoSize) THEN

    IF (CurSysNum > 0) THEN

      CALL CheckSysSizing(cFurnaceTypes(Furnace(FurnaceNum)%FurnaceType_Num), Furnace(FurnaceNum)%Name)
      IF (FinalSysSizing(CurSysNum)%DesMainVolFlow >= SmallAirVolFlow) THEN
        Furnace(FurnaceNum)%MaxCoolAirVolFlow = FinalSysSizing(CurSysNum)%DesMainVolFlow
      ELSE
        Furnace(FurnaceNum)%MaxCoolAirVolFlow = 0.0d0
      END IF

      IF (Furnace(FurnaceNum)%MaxCoolAirVolFlowEMSOverrideOn) THEN
        Furnace(FurnaceNum)%MaxCoolAirVolFlow = Furnace(FurnaceNum)%MaxCoolAirVolFlowEMSOverrideValue
      ENDIF

      CALL ReportSizingOutput(cFurnaceTypes(Furnace(FurnaceNum)%FurnaceType_Num), Furnace(FurnaceNum)%Name, &
                               'Supply Air Flow Rate During Cooling Operation [m3/s]', &
                                Furnace(FurnaceNum)%MaxCoolAirVolFlow)

    END IF

  END IF

  IF (Furnace(FurnaceNum)%MaxNoCoolHeatAirVolFlow == AutoSize) THEN

    IF (CurSysNum > 0) THEN

      CALL CheckSysSizing(cFurnaceTypes(Furnace(FurnaceNum)%FurnaceType_Num), Furnace(FurnaceNum)%Name)
      IF (FinalSysSizing(CurSysNum)%DesMainVolFlow >= SmallAirVolFlow) THEN
        Furnace(FurnaceNum)%MaxNoCoolHeatAirVolFlow = FinalSysSizing(CurSysNum)%DesMainVolFlow
      ELSE
        Furnace(FurnaceNum)%MaxNoCoolHeatAirVolFlow = 0.0d0
      END IF

      IF (Furnace(FurnaceNum)%MaxNoCoolHeatAirVolFlowEMSOverrideOn) THEN
        Furnace(FurnaceNum)%MaxNoCoolHeatAirVolFlow = Furnace(FurnaceNum)%MaxNoCoolHeatAirVolFlowEMSOverrideValue
      ENDIF

      CALL ReportSizingOutput(cFurnaceTypes(Furnace(FurnaceNum)%FurnaceType_Num), Furnace(FurnaceNum)%Name, &
                               'Supply Air Flow Rate When No Cooling or Heating is Needed [m3/s]', &
                                Furnace(FurnaceNum)%MaxNoCoolHeatAirVolFlow)

    END IF

  END IF

  IF (Furnace(FurnaceNum)%DesignHeatingCapacity == AutoSize) THEN

    IF (CurSysNum > 0) THEN

      IF (Furnace(FurnaceNum)%FurnaceType_Num == UnitarySys_HeatPump_AirToAir .OR. &
          Furnace(FurnaceNum)%FurnaceType_Num == UnitarySys_HeatPump_WaterToAir) THEN

        CALL CheckSysSizing(cFurnaceTypes(Furnace(FurnaceNum)%FurnaceType_Num), Furnace(FurnaceNum)%Name)
        Furnace(FurnaceNum)%DesignHeatingCapacity = DXCoolCap

      ELSE

        CALL CheckSysSizing(cFurnaceTypes(Furnace(FurnaceNum)%FurnaceType_Num), Furnace(FurnaceNum)%Name)

        Furnace(FurnaceNum)%DesignHeatingCapacity = FinalSysSizing(CurSysNum)%HeatCap

      END IF

      IF (Furnace(FurnaceNum)%DesignHeatingCapacity < SmallLoad) THEN
        Furnace(FurnaceNum)%DesignHeatingCapacity = 0.0d0
      END IF

      CALL ReportSizingOutput(cFurnaceTypes(Furnace(FurnaceNum)%FurnaceType_Num), Furnace(FurnaceNum)%Name, &
                              'Nominal Heating Capacity [W]', &
                              Furnace(FurnaceNum)%DesignHeatingCapacity)

    END IF

  END IF

  IF (Furnace(FurnaceNum)%DesignCoolingCapacity == AutoSize) THEN

    IF (CurSysNum > 0) THEN

      CALL CheckSysSizing(cFurnaceTypes(Furnace(FurnaceNum)%FurnaceType_Num), Furnace(FurnaceNum)%Name)
      IF (DXCoolCap >= SmallLoad) THEN
        Furnace(FurnaceNum)%DesignCoolingCapacity = DXCoolCap
      ELSE
        Furnace(FurnaceNum)%DesignCoolingCapacity = 0.0d0
      END IF
      CALL ReportSizingOutput(cFurnaceTypes(Furnace(FurnaceNum)%FurnaceType_Num), Furnace(FurnaceNum)%Name, &
                              'Nominal Cooling Capacity [W]', &
                              Furnace(FurnaceNum)%DesigncoolingCapacity)

    END IF

  END IF

  IF (Furnace(FurnaceNum)%DesignMaxOutletTemp == AutoSize) THEN

    IF (CurSysNum > 0) THEN

      CALL CheckSysSizing(cFurnaceTypes(Furnace(FurnaceNum)%FurnaceType_Num), Furnace(FurnaceNum)%Name)
      Furnace(FurnaceNum)%DesignMaxOutletTemp = FinalSysSizing(CurSysNum)%HeatSupTemp
      CALL ReportSizingOutput(cFurnaceTypes(Furnace(FurnaceNum)%FurnaceType_Num), Furnace(FurnaceNum)%Name, &
                              'Maximum Supply Air Temperature from Supplemental Heater [C]', &
                              Furnace(FurnaceNum)%DesignMaxOutletTemp)

    END IF

  END IF

  IF (Furnace(FurnaceNum)%DesignSuppHeatingCapacity == AutoSize) THEN

    IF (CurSysNum > 0) THEN

      CALL CheckSysSizing(cFurnaceTypes(Furnace(FurnaceNum)%FurnaceType_Num), Furnace(FurnaceNum)%Name)
      IF (Furnace(FurnaceNum)%FurnaceType_Num == UnitarySys_HeatPump_AirToAir .or. &
          Furnace(FurnaceNum)%FurnaceType_Num == UnitarySys_HeatPump_WaterToAir) THEN
        ! set the supplemental heating capacity to the actual heating load
        Furnace(FurnaceNum)%DesignSuppHeatingCapacity = FinalSysSizing(CurSysNum)%HeatCap
        ! if reheat needed for humidity control, make sure supplemental heating is at least as big
        ! as the cooling capacity
        IF (Furnace(FurnaceNum)%Humidistat .AND. Furnace(FurnaceNum)%DehumidControlType_Num == DehumidControl_CoolReheat) THEN
          Furnace(FurnaceNum)%DesignSuppHeatingCapacity = MAX(Furnace(FurnaceNum)%DesignSuppHeatingCapacity, &
                                                              Furnace(FurnaceNum)%DesignCoolingCapacity)
          IF (Furnace(FurnaceNum)%DesignSuppHeatingCapacity < SmallLoad) THEN
            Furnace(FurnaceNum)%DesignSuppHeatingCapacity = 0.0d0
          ENDIF
        END IF

      ELSE

        IF (Furnace(FurnaceNum)%Humidistat .AND. Furnace(FurnaceNum)%DehumidControlType_Num == DehumidControl_CoolReheat) THEN
          Furnace(FurnaceNum)%DesignSuppHeatingCapacity = Furnace(FurnaceNum)%DesignCoolingCapacity
        ELSE
          Furnace(FurnaceNum)%DesignSuppHeatingCapacity = 0.0d0
        END IF

      ENDIF

      CALL ReportSizingOutput(cFurnaceTypes(Furnace(FurnaceNum)%FurnaceType_Num), Furnace(FurnaceNum)%Name, &
                              'Supplemental Heating Coil Nominal Capacity [W]', &
                              Furnace(FurnaceNum)%DesignSuppHeatingCapacity)

    END IF

  END IF

  UnitaryHeatCap = Furnace(FurnaceNum)%DesignHeatingCapacity
  SuppHeatCap = Furnace(FurnaceNum)%DesignSuppHeatingCapacity

  BranchFlow = 0.0d0
  ErrFound  = .FALSE.
  ! check branch flow rate vs system flow rate. Branch must match system of OA system is present
  CALL CheckSystemBranchFlow(TRIM(cFurnaceTypes(Furnace(FurnaceNum)%FurnaceType_Num)), &
                  Furnace(FurnaceNum)%Name,BranchFlow,Furnace(FurnaceNum)%DesignFanVolFlowRate, ErrFound)
  IF(ErrFound)CALL ShowContinueError('...occurs in '//TRIM(cFurnaceTypes(Furnace(FurnaceNum)%FurnaceType_Num))// &
                                     ' "'//TRIM(Furnace(FurnaceNum)%Name))

  RETURN

END SUBROUTINE SizeFurnace

 ! End Initialization Section of the Module
!******************************************************************************



! Beginning of Update subroutines for the Furnace Module
! *****************************************************************************


SUBROUTINE CalcNewZoneHeatOnlyFlowRates(FurnaceNum,FirstHVACIteration,ZoneLoad,HeatCoilLoad, OnOffAirFlowRatio)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Liesen
          !       DATE WRITTEN   Feb 2001
          !       MODIFIED       Don Shirey and R. Raustad, Mar 2001 & Mar 2003
          !       RE-ENGINEERED  na


          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine updates the coil outlet nodes by simulating a heat-only
          ! furnace or unitary system.


          ! METHODOLOGY EMPLOYED:
          ! Determine the operating PLR to meet the zone sensible load.


          ! REFERENCES:
          ! na


          ! USE STATEMENTS:
  USE HeatingCoils,         ONLY: SimulateHeatingCoilComponents
  USE ScheduleManager
  USE DataHeatBalFanSys,    ONLY: MAT
  USE General, ONLY: TrimSigDigits


  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine


          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER,   INTENT(IN)    :: FurnaceNum          ! Index to furnace
  LOGICAL,   INTENT(IN)    :: FirstHVACIteration  ! Iteration flag
  REAL(r64), INTENT(IN)    :: ZoneLoad            ! load to be met by furnace (W)
  REAL(r64), INTENT(INOUT) :: HeatCoilLoad        ! actual load passed to heating coil (W)
  REAL(r64), INTENT(INOUT) :: OnOffAirFlowRatio   ! ratio of coil on to coil off air flow rate


          ! SUBROUTINE PARAMETER DEFINITIONS:
  INTEGER,   PARAMETER ::  MaxIter = 15           ! maximum number of iterations
  REAL(r64), PARAMETER ::  MinPLR = 0.0d0           ! minimum part load ratio allowed


          ! INTERFACE BLOCK SPECIFICATIONS
          ! na


          ! DERIVED TYPE DEFINITIONS
          ! na


          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64)         :: cpair
  REAL(r64)         :: Error = 1.0d0
  REAL(r64)         :: SystemSensibleLoad   ! Sensible load to be met by furnace (W)
  REAL(r64)         :: FullSensibleOutput   ! Full sensible output of furnace (W)
  REAL(r64)         :: FullLatentOutput     ! Full latent output of furnace = 0 (W)
  REAL(r64)         :: NoSensibleOutput     ! Sensible output of furnace with no heating allowed (W)
  REAL(r64)         :: NoLatentOutput       ! Latent output of furnace = 0 (W)
  REAL(r64)         :: PartLoadRatio        ! Part load ratio of furnace
  REAL(r64)         :: HeatErrorToler       ! Error tolerance in heating mode
  REAL(r64)         :: IterRelax            ! Relaxation factor for iterations
  REAL(r64)         :: ActualSensibleOutput ! Actual furnace sensible capacity
  REAL(r64)         :: ActualLatentOutput   ! Actual furnace latent capacity = 0
  REAL(r64)         :: DeltaT               ! Heater outlet temp minus design heater outlet temp
!  CHARACTER(len=20) :: ErrNum = ' '         ! For displaying error message in cooling
!  INTEGER,SAVE      :: ErrCount = 0
  INTEGER           :: Iter = 0             ! Iteration counter
  INTEGER           :: FurnaceInletNode     ! Node number of furnace inlet
  INTEGER           :: FurnaceOutletNode    ! Node number of furnace outlet
  INTEGER           :: OpMode               ! Mode of Operation (fan cycling or fan continuous)
      ! Set local variables


      ! Retrieve the load on the controlled zone
  FurnaceOutletNode = Furnace(FurnaceNum)%FurnaceOutletNodeNum
  FurnaceInletNode = Furnace(FurnaceNum)%FurnaceInletNodeNum
  OpMode = Furnace(FurnaceNum)%OpMode
  Furnace(FurnaceNum)%MdotFurnace = Furnace(FurnaceNum)%DesignMassFlowRate
  Furnace(FurnaceNum)%CoolPartLoadRatio = 0.0d0
!  OnOffAirFlowRatio = 1.0


  !Calculate the Cp Air
  cpair = PsyCpAirFnWTdb(Node(FurnaceInletNode)%HumRat,Node(FurnaceInletNode)%Temp)


  IF(FirstHVACIteration) THEN
    HeatCoilLoad = ZoneLoad
    OnOffFanPartLoadFraction = 1.0d0
    Node(FurnaceInletNode)%MassFlowRate = Furnace(FurnaceNum)%MdotFurnace
  ELSE
    ! If Furnace runs then set HeatCoilLoad on Heating Coil and the Mass Flow
    IF((GetCurrentScheduleValue(Furnace(FurnaceNum)%SchedPtr) .gt. 0.0d0) .and. &
         (Node(FurnaceInletNode)%MassFlowRate .gt. 0.0d0) .and. &
          (HeatingLoad)) THEN


      Node(FurnaceInletNode)%MassFlowRate=Furnace(FurnaceNum)%MdotFurnace
      HeatCoilLoad = Furnace(FurnaceNum)%DesignHeatingCapacity
      SystemSensibleLoad = ZoneLoad


      ! Get no load result
      IF (OpMode .EQ. CycFanCycCoil) THEN
        Node(FurnaceInletNode)%MassFlowRate = 0.0d0
      END IF
      IF (OpMode .EQ. ContFanCycCoil) THEN
        OnOffFanPartLoadFraction = 1.0d0 ! The on/off fan will not cycle, so set part-load fraction = 1
      END IF

!     Set the inlet mass flow rate based on user specified coil OFF flow rate
      PartLoadRatio = 0.0d0
      CALL SetAverageAirFlow(FurnaceNum, PartLoadRatio, OnOffAirFlowRatio)

      CALL CalcFurnaceOutput(FurnaceNum,FirstHVACIteration,OpMode,On,0.0d0,0.0d0,0.0d0,0.0d0,NoSensibleOutput,NoLatentOutput, &
                             OnOffAirFlowRatio, .FALSE.)


      Node(FurnaceInletNode)%MassFlowRate=Furnace(FurnaceNum)%MdotFurnace


      ! Set fan part-load fraction equal to 1 while getting full load result
      OnOffFanPartLoadFraction = 1.0d0
      OnOffAirFlowRatio        = 1.0d0

      ! Get full load result
      CALL CalcFurnaceOutput(FurnaceNum,FirstHVACIteration,OpMode,On,0.0d0,1.0d0,HeatCoilLoad,0.0d0,FullSensibleOutput, &
                             FullLatentOutput, OnOffAirFlowRatio, .FALSE.)


      ! Since we are heating, we expect FullSensibleOutput to be > 0 and FullSensibleOutput > NoSensibleOutput
      ! Check that this is the case; if not set PartLoadRatio = 0.0d0 (off) and return


      IF(FullSensibleOutput.GT.NoSensibleOutput)THEN
        PartLoadRatio = MAX(MinPLR, MIN(1.0d0, ABS(SystemSensibleLoad-NoSensibleOutput) &
                                           / ABS(FullSensibleOutput-NoSensibleOutput)))
        IF (OpMode .EQ. CycFanCycCoil) THEN
          Node(FurnaceInletNode)%MassFlowRate = Furnace(FurnaceNum)%MdotFurnace * PartLoadRatio
          HeatCoilLoad = Furnace(FurnaceNum)%DesignHeatingCapacity * PartLoadRatio
        ELSE ! ContFanCycCoil
          IF(Node(FurnaceOutletNode)%Temp .GT. Furnace(FurnaceNum)%DesignMaxOutletTemp) THEN
            deltaT = Node(FurnaceOutletNode)%Temp-Furnace(FurnaceNum)%DesignMaxOutletTemp
            IF(HeatCoilLoad .GT. Furnace(FurnaceNum)%DesignHeatingCapacity) &
              HeatCoilLoad = Furnace(FurnaceNum)%DesignHeatingCapacity
              HeatCoilLoad = HeatCoilLoad - Node(FurnaceInletNode)%MassFlowRate * cpair * deltaT
          ELSE
          HeatCoilLoad = SystemSensibleLoad - NoSensibleOutput
          END IF
        END IF


        ! Calculate the part load ratio through iteration
        HeatErrorToler = Furnace(FurnaceNum)%HeatingConvergenceTolerance !Error tolerance for convergence from input deck
        Error = 1.0d0             ! initialize error value for comparison against tolerance
        Iter = 0                  ! initialize iteration counter
        IterRelax = 0.9d0         ! relaxation factor for iterations
        DO WHILE (Iter .LE. MaxIter)

         IF (OpMode .EQ. CycFanCycCoil) Node(FurnaceInletNode)%MassFlowRate = Furnace(FurnaceNum)%MdotFurnace * PartLoadRatio
         CALL CalcFurnaceOutput(FurnaceNum,FirstHVACIteration,OpMode,On,0.0d0,PartLoadRatio, &
                                HeatCoilLoad,0.0d0,ActualSensibleOutput,ActualLatentOutput, OnOffAirFlowRatio, .FALSE.)


         IF(SystemSensibleLoad .NE. 0.0d0) Error=(SystemSensibleLoad-ActualSensibleOutput)/(SystemSensibleLoad)
         IF(ABS(Error) .LE. HeatErrorToler) EXIT
         PartLoadRatio = MAX(MinPLR,MIN(1.0d0,PartLoadRatio + IterRelax*&
                       (SystemSensibleLoad-ActualSensibleOutput)/(FullSensibleOutput-NoSensibleOutput)))


!        limit the heating coil outlet air temperature to DesignMaxOutletTemp
         IF(Node(FurnaceOutletNode)%Temp .GT. Furnace(FurnaceNum)%DesignMaxOutletTemp) THEN
           deltaT = Node(FurnaceOutletNode)%Temp-Furnace(FurnaceNum)%DesignMaxOutletTemp
           IF(HeatCoilLoad .GT. Furnace(FurnaceNum)%DesignHeatingCapacity) &
              HeatCoilLoad = Furnace(FurnaceNum)%DesignHeatingCapacity
           HeatCoilLoad = HeatCoilLoad - Node(FurnaceInletNode)%MassFlowRate * cpair * deltaT
           CALL CalcFurnaceOutput(FurnaceNum,FirstHVACIteration,OpMode,On,0.0d0,PartLoadRatio, &
                                HeatCoilLoad,0.0d0,ActualSensibleOutput,ActualLatentOutput, OnOffAirFlowRatio, .FALSE.)


           IF(SystemSensibleLoad .NE. 0.0d0) Error=(SystemSensibleLoad-ActualSensibleOutput)/(SystemSensibleLoad)
           PartLoadRatio = MAX(MinPLR,MIN(1.0d0,PartLoadRatio + IterRelax* &
                       (SystemSensibleLoad-ActualSensibleOutput)/(FullSensibleOutput-NoSensibleOutput)))
         ELSE
           HeatCoilLoad = Furnace(FurnaceNum)%DesignHeatingCapacity * PartLoadRatio
         END IF


         IF(PartLoadRatio.eq.MinPLR)EXIT
         IF(PartLoadRatio.eq.1.0d0)EXIT
         Iter = Iter + 1
         IF(Iter.eq.7)IterRelax=0.7d0
         IF(Iter.eq.15)IterRelax=0.4d0


        END DO


        IF (Iter .GT. MaxIter) THEN
          IF (Furnace(FurnaceNum)%HeatingMaxIterIndex2 == 0) THEN
            CALL ShowWarningMessage(TRIM(cFurnaceTypes(Furnace(FurnaceNum)%FurnaceType_Num))//' "'&
                 //TRIM(Furnace(FurnaceNum)%Name)//'" -- Exceeded max heating iterations ('//  &
                    TRIM(TrimSigDigits(MaxIter))//  &
                  ') while adjusting furnace runtime.')
            CALL ShowContinueErrorTimeStamp(' ')
          ENDIF
          CALL ShowRecurringWarningErrorAtEnd(TRIM(cFurnaceTypes(Furnace(FurnaceNum)%FurnaceType_Num))//' "'//  &
                 TRIM(Furnace(FurnaceNum)%Name)//'" -- Exceeded max heating iterations error continues...',  &
                 Furnace(FurnaceNum)%HeatingMaxIterIndex2)
        END IF


      ELSE  !ELSE from IF(FullSensibleOutput.GT.NoSensibleOutput)THEN above
        ! Set part load ratio to 1 and run heater at design heating capacity
        PartLoadRatio = 1.0d0
        HeatCoilLoad = Furnace(FurnaceNum)%DesignHeatingCapacity
      END IF
      ! Set the final results
!      IF (OpMode .EQ. CycFanCycCoil) THEN
!        Furnace(FurnaceNum)%MdotFurnace = Furnace(FurnaceNum)%MdotFurnace * PartLoadRatio
!      END IF
       Furnace(FurnaceNum)%MdotFurnace = Node(FurnaceInletNode)%MassFlowRate


    ELSEIF((GetCurrentScheduleValue(Furnace(FurnaceNum)%SchedPtr) .gt. 0.0d0) .and. &
       (Node(FurnaceInletNode)%MassFlowRate .gt. 0.0d0) .and. (OpMode .EQ. ContFanCycCoil)) THEN
      HeatCoilLoad = 0.0d0
    ELSE ! no heating and no flow
      Furnace(FurnaceNum)%MdotFurnace = 0.0d0
      HeatCoilLoad = 0.0d0
    END IF   ! End of the Scheduled Furnace If block


  END IF   ! End of the FirstHVACIteration control of the mass flow If block


        ! Set the fan inlet node flow rates
  Node(FurnaceInletNode)%MassFlowRateMaxAvail = Furnace(FurnaceNum)%MdotFurnace
  Node(FurnaceInletNode)%MassFlowRate         = Furnace(FurnaceNum)%MdotFurnace


RETURN
END Subroutine CalcNewZoneHeatOnlyFlowRates


SUBROUTINE CalcNewZoneHeatCoolFlowRates(FurnaceNum,FirstHVACIteration,CompOp,ZoneLoad,MoistureLoad,HeatCoilLoad,ReheatCoilLoad, &
                                        OnOffAirFlowRatio, HXUnitOn)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Liesen
          !       DATE WRITTEN   Feb 2001
          !       MODIFIED       R. Raustad and D. Shirey, Feb/Mar/Sept/Oct/Dec 2001, Jan/Oct 2002
          !       RE-ENGINEERED  R. Raustad, Feb. 2005 (added RegulaFalsi for iteration technique)


          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine updates the coil outlet nodes.


          ! METHODOLOGY EMPLOYED:
          ! Determine the operating PLR to meet the zone sensible load. If a humidistat is specified, determine
          ! the operating PLR (greater of the sensible and latent PLR) to meet the zone SENSIBLE load
          ! (Multimode dehumidification control) or zone LATENT load (CoolReheat dehumidification control).
          !
          ! For dehumidification control type COOLREHEAT, both a sensible and latent PLR may exist for a
          ! single time step (heating and dehumidificaiton can occur). For all other sytem types,
          ! only a single PLR is allowed for any given time step.
          !
          ! Order of simulation depends on dehumidification control option as described below.
          !
          ! Dehumidificaiton control options:
          !
          ! Dehumidification Control NONE:   Cooling performance is simulated first and then heating performance. If a HX
          !                                  assisted cooling coil is selected, the HX is always active.
          !
          ! Dehumidification Control COOLREHEAT: Continuous Fan Operation:
          !                                      For cooling operation, the sensible and latent capacities are calculated to
          !                                      meet the thermostat setpoint. If a HX assisted cooling coil is selected,
          !                                      the HX is always active. If the latent load is not met by operating the
          !                                      system at the sensible PLR, a new PLR is calculated to meet the humidistat
          !                                      setpoint. The reheat coil load is then calculated to meet the HEATING
          !                                      setpoint temperature.
          !
          !                                      Cycling Fan Operation:
          !                                      The heating part-load ratio is calculated first. Since the fan will be
          !                                      controlled at the higher of the heating or cooling PLR's, a ratio of the
          !                                      cooling to heating PLR is used to pass to the cooling coil (MAX=1). This allows
          !                                      the cooling coil to operate at the heating PLR when the heating PLR is
          !                                      higher than the cooling PLR. The sensible and latent capacities are then
          !                                      calculated to meet the thermostat setpoint.
          !                                      If a HX assisted cooling coil is selected, the HX is always active.
          !                                      If the latent load is not met by operating the system at the sensible PLR,
          !                                      a new PLR is calculated to meet the humidistat setpoint.
          !                                      The reheat coil load is then calculated to meet the HEATING setpoint temperature.
          !
          ! Dehumidification Control MULTIMODE: For cooling operation, the sensible and latent capacities are calculated to
          !                                     meet the thermostat setpoint. If a HX assisted cooling coil is selected,
          !                                     the HX is off for this calculation. If the latent load is not met by operating
          !                                     the system at the sensible PLR, a new PLR is calculated with the HX operating
          !                                     and the target is the thermostat setpoint. Humidity is not controlled in this
          !                                     mode. No reheat coil is used in this configuration.
          !
          !  Note: A supplemental heater augments the heating capacity for air-to-air heat pumps.
          !        A reheat coil is used for the HeatCool furnace/unitarysystem to offset the sensible cooling when the
          !        dehumidification control type is COOLREHEAT. Both the supplemental and reheat heating coil load is calculated
          !        in the Calc routines. The actual simulation of these coils is performed in the SimFurnace routine (i.e. the
          !        supplemental and reheat coil loads are passed as 0 to CalcFurnaceOutput).

          ! REFERENCES:
          ! na


          ! USE STATEMENTS:
  USE HeatingCoils,       ONLY: SimulateHeatingCoilComponents
  USE DataHeatBalFanSys,  ONLY: MAT
  USE ScheduleManager
  USE DataZoneEnergyDemands
  USE General,            ONLY: SolveRegulaFalsi, TrimSigDigits
  USE DXCoils,            ONLY: DXCoilPartLoadRatio

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine


          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER,   Intent(IN)    :: FurnaceNum
  LOGICAL,   Intent(IN)    :: FirstHVACIteration
  INTEGER,   Intent(IN)    :: CompOp             ! compressor operation flag (1=On, 0=Off)
  REAL(r64), Intent(IN)    :: ZoneLoad           ! the control zone load (watts)
  REAL(r64), Intent(IN)    :: MoistureLoad       ! the control zone latent load (watts)
  REAL(r64), Intent(INOUT) :: HeatCoilLoad       ! Heating load to be met by heating coil ( excluding heat pump DX coil)
  REAL(r64), Intent(INOUT) :: ReheatCoilLoad     ! Heating load to be met by reheat coil using hstat (excluding HP DX coil)
  REAL(r64), Intent(INOUT) :: OnOffAirFlowRatio  ! Ratio of compressor ON air flow to AVERAGE air flow over time step
  LOGICAL,   Intent(INOUT) :: HXUnitOn           ! flag to control HX based on zone moisture load

          ! SUBROUTINE PARAMETER DEFINITIONS:
  INTEGER,   PARAMETER  ::  MaxIter = 100        ! maximum number of iterations
  REAL(r64), PARAMETER  ::  MinPLR  = 0.0d0        ! minimum part load ratio allowed

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na


          ! DERIVED TYPE DEFINITIONS
          ! na


          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64)         :: SystemMoistureLoad   ! Total latent load to be removed by furnace/unitary system
  REAL(r64)         :: cpair                ! Heat capacity of air
  REAL(r64)         :: deltaT               ! Temperature rise across heating coil (C)
  REAL(r64)         :: TempOutHeatingCoil   ! Temperature leaving heating coil (C)
  REAL(r64)         :: FullSensibleOutput   ! Full sensible output of AC (W)
  REAL(r64)         :: FullLatentOutput     ! Full latent output of AC (W)
  REAL(r64)         :: NoCoolOutput         ! Sensible output of AC with no cooling allowed (W)
  REAL(r64)         :: NoHeatOutput         ! Sensible output of heater with no heating allowed (W)
  REAL(r64)         :: NoLatentOutput       ! Latent output of AC with no cooling allowed (W)
  INTEGER           :: FurnaceInletNode     ! Inlet node to furnace or unitary system
  INTEGER           :: FurnaceOutletNode    ! Outlet node of furnace or unitary system
  INTEGER           :: OpMode               ! Mode of Operation (fan cycling = 1 or fan continuous = 2)
  REAL(r64), SAVE   :: CoolCoilLoad         ! Negative value means cooling required
  REAL(r64), SAVE   :: SystemSensibleLoad   ! Positive value means heating required
  REAL(r64)         :: CoolErrorToler       ! Error tolerance in cooling mode
  REAL(r64)         :: HeatErrorToler       ! Error tolerance in heating mode
  REAL(r64)         :: ActualSensibleOutput ! Actual furnace sensible capacity
  REAL(r64)         :: ActualLatentOutput   ! Actual furnace latent capacity
  REAL(r64)         :: PartLoadRatio        ! Part load ratio (greater of sensible or latent part load ratio for cooling,
                                            ! or heating PLR)
  REAL(r64)         :: LatentPartLoadRatio  ! Part load ratio to meet dehumidification load
  REAL(r64)         :: TempCoolOutput       ! Temporary Sensible output of AC while iterating on PLR (W)
  REAL(r64)         :: TempHeatOutput       ! Temporary Sensible output of heating coil while iterating on PLR (W)
  REAL(r64)         :: TempLatentOutput     ! Temporary Latent output of AC at increasing PLR (W)
!                                           ! (Temp variables are used to find min PLR for positive latent removal)
  LOGICAL           :: HumControl = .FALSE. ! Logical flag signaling when dehumidification is required
  REAL(r64), DIMENSION(10) :: Par           ! parameters passed to RegulaFalsi function
  INTEGER           :: SolFlag              ! return flag from RegulaFalsi
  REAL(r64)         :: TempMinPLR           ! Temporary min latent PLR when hum control is required and iter is exceeded
  REAL(r64)         :: TempMinPLR2          ! Temporary min latent PLR when cyc fan hum control is required and iter is exceeded
  REAL(r64)         :: TempMaxPLR           ! Temporary max latent PLR when hum control is required and iter is exceeded
  REAL(r64)         :: QToHeatSetPt         ! Load required to meet heating setpoint temp (>0 is a heating load)
  REAL(r64)         :: CoolingHeatingPLRRatio ! ratio of cooling to heating PLR (MAX=1). Used in heating mode.
  REAL(r64)         :: HeatingSensibleOutput
  REAL(r64)         :: HeatingLatentOutput


  ! Set local variables
  FurnaceOutletNode = Furnace(FurnaceNum)%FurnaceOutletNodeNum
  FurnaceInletNode = Furnace(FurnaceNum)%FurnaceInletNodeNum
  OpMode = Furnace(FurnaceNum)%OpMode
!  Furnace(FurnaceNum)%MdotFurnace = Furnace(FurnaceNum)%DesignMassFlowRate
  HumControl = .FALSE.
  !Calculate the Cp Air for all conditions
  cpair = PsyCpAirFnWTdb(Node(FurnaceInletNode)%HumRat,Node(FurnaceInletNode)%Temp)
  NoHeatOutput       = 0.0d0
  SystemSensibleLoad = 0.0d0
  ReHeatCoilLoad     = 0.0d0
  HeatCoilLoad       = 0.0d0
  ReheatCoilLoad     = 0.0d0
  PartLoadRatio      = 0.0d0

  IF(FirstHVACIteration) THEN
  ! Set selected values during first HVAC iteration


    !Init for heating
    IF(HeatingLoad)THEN
      IF (Furnace(FurnaceNum)%FurnaceType_Num == UnitarySys_HeatPump_AirToAir .OR. &
          (Furnace(FurnaceNum)%FurnaceType_Num == UnitarySys_HeatPump_WatertoAir .AND. &
           Furnace(FurnaceNum)%WatertoAirHPType == WatertoAir_Simple))THEN
        Furnace(FurnaceNum)%HeatPartLoadRatio = 1.0d0
        HeatCoilLoad = 0.0d0
        Furnace(FurnaceNum)%HeatingCoilSensDemand=0.0d0
        Furnace(FurnaceNum)%CoolingCoilSensDemand=0.0d0
        Furnace(FurnaceNum)%CoolingCoilLatentDemand=0.0d0
      ELSE !for furnaces
        Furnace(FurnaceNum)%HeatPartLoadRatio = 0.0d0
        HeatCoilLoad = ZoneLoad
        Node(FurnaceInletNode)%MassFlowRate = Furnace(FurnaceNum)%MdotFurnace
        Furnace(FurnaceNum)%HeatingCoilSensDemand=0.0d0
        Furnace(FurnaceNum)%CoolingCoilSensDemand=0.0d0
        Furnace(FurnaceNum)%CoolingCoilLatentDemand=0.0d0
      ENDIF
      ReheatCoilLoad = 0.0d0
      Furnace(FurnaceNum)%CoolPartLoadRatio = 0.0d0


    !Init for cooling
    ELSEIF(CoolingLoad)THEN
       !air to air heat pumps
        Furnace(FurnaceNum)%CoolPartLoadRatio = 1.0d0
        Furnace(FurnaceNum)%HeatPartLoadRatio = 0.0d0
        HeatCoilLoad = 0.0d0
        ReheatCoilLoad = 0.0d0


    !Init for moisture load only
    ELSE
      Furnace(FurnaceNum)%CoolPartLoadRatio = 0.0d0
      Furnace(FurnaceNum)%HeatPartLoadRatio = 0.0d0
      HeatCoilLoad = 0.0d0
      ReheatCoilLoad = 0.0d0
      Furnace(FurnaceNum)%HeatingCoilSensDemand=0.0d0
      Furnace(FurnaceNum)%CoolingCoilSensDemand=0.0d0
      Furnace(FurnaceNum)%CoolingCoilLatentDemand=0.0d0
    ENDIF

    CALL SetAverageAirFlow(FurnaceNum, &
        MAX(Furnace(FurnaceNum)%HeatPartLoadRatio,Furnace(FurnaceNum)%CoolPartLoadRatio), &
        OnOffAirFlowRatio)
    !  if dehumidification load exists (for heat pumps) turn on the supplmental heater
    IF (HPDehumidificationLoadFlag) HumControl = .TRUE.
  ELSE  !not FirstHVACIteration
    !Init for heating
    IF(HeatingLoad)THEN
      CoolCoilLoad = 0.0d0
      IF (Furnace(FurnaceNum)%FurnaceType_Num == UnitarySys_HeatPump_AirToAir .OR. &
          (Furnace(FurnaceNum)%FurnaceType_Num == UnitarySys_HeatPump_WatertoAir .AND. &
           Furnace(FurnaceNum)%WatertoAirHPType == WatertoAir_Simple))THEN
        SystemSensibleLoad = ZoneLoad
        SystemMoistureLoad = 0.0d0
        HeatCoilLoad = 0.0d0
        Furnace(FurnaceNum)%HeatingCoilSensDemand=SystemSensibleLoad
        Furnace(FurnaceNum)%CoolingCoilSensDemand=0.0d0
        Furnace(FurnaceNum)%CoolingCoilLatentDemand=0.0d0
      ELSE
        SystemMoistureLoad = MoistureLoad
        HeatCoilLoad = ZoneLoad
      END IF


    !Init for cooling
    ELSEIF(CoolingLoad)THEN
      CoolCoilLoad = ZoneLoad
      SystemMoistureLoad = MoistureLoad
      HeatCoilLoad = 0.0d0
      Furnace(FurnaceNum)%CoolingCoilSensDemand=ABS(CoolCoilLoad)
      Furnace(FurnaceNum)%CoolingCoilLatentDemand=ABS(SystemMoistureLoad)
      Furnace(FurnaceNum)%HeatingCoilSensDemand=0.0d0


    !Init for latent
    ELSE
      SystemMoistureLoad = MoistureLoad
      CoolCoilLoad = 0.0d0
      HeatCoilLoad = 0.0d0
      !set report variables
      Furnace(FurnaceNum)%CoolingCoilSensDemand=0.0d0
      Furnace(FurnaceNum)%CoolingCoilLatentDemand=SystemMoistureLoad
      Furnace(FurnaceNum)%HeatingCoilSensDemand=0.0d0
    ENDIF
    HeatingSensibleOutput                 = 0.0d0
    HeatingLatentOutput                   = 0.0d0
    ReheatCoilLoad                        = 0.0d0
    Furnace(FurnaceNum)%CoolPartLoadRatio = 0.0d0
    Furnace(FurnaceNum)%HeatPartLoadRatio = 0.0d0
    Furnace(FurnaceNum)%CompPartLoadRatio = 0.0d0
    Furnace(FurnaceNum)%DehumidInducedHeatingDemandRate = 0.0d0

! When humidity control is used with cycling fan control and a heating load exists, if a moisture load
! also exists, the heating PLR must be available for the cooling coil calculations.
!*********** Heating Section ************
    ! If Furnace runs with a heating load then set HeatCoilLoad on Heating Coil and the Mass Flow
    IF((GetCurrentScheduleValue(Furnace(FurnaceNum)%SchedPtr) .gt. 0.0d0) .and. &
!         (Node(FurnaceInletNode)%MassFlowRate .gt. 0.0) .and. &
          (HeatingLoad)) THEN

!    Heat pumps only calculate a single PLR each time step (i.e. only cooling or heating allowed in a single time step)
     IF (Furnace(FurnaceNum)%FurnaceType_Num == UnitarySys_HeatPump_AirToAir .OR. &
        (Furnace(FurnaceNum)%FurnaceType_Num == UnitarySys_HeatPump_WaterToAir .AND. &
          Furnace(FurnaceNum)%WatertoAirHPType == WatertoAir_Simple )) THEN


      Node(FurnaceInletNode)%MassFlowRate=Furnace(FurnaceNum)%MdotFurnace


      ! Get no load result
      IF (OpMode .EQ. CycFanCycCoil) THEN
        Node(FurnaceInletNode)%MassFlowRate = 0.0d0
      END IF

!     Set the inlet mass flow rate based on user specified coil OFF flow rate
      PartLoadRatio = 0.0d0

      CALL SetAverageAirFlow(FurnaceNum, PartLoadRatio, OnOffAirFlowRatio)

         !Set the input parameters for CalcFurnaceOutput
      Furnace(FurnaceNum)%CompPartLoadRatio = 0.0d0    !compressor off
      Furnace(FurnaceNum)%WSHPRuntimeFrac = 0.0d0

      CALL CalcFurnaceOutput(FurnaceNum,FirstHVACIteration,OpMode,CompOp,0.0d0,minPLR,0.0d0,0.0d0,NoHeatOutput, NoLatentOutput, &
                             OnOffAirFlowRatio, .FALSE.)

      PartLoadRatio = 1.0d0
      Node(FurnaceInletNode)%MassFlowRate=Furnace(FurnaceNum)%MdotFurnace

      Furnace(FurnaceNum)%CompPartLoadRatio = 1.0d0    !compressor ON
      Furnace(FurnaceNum)%WSHPRuntimeFrac   = 1.0d0

      ! Set fan part-load fraction equal to 1 while getting full load result
      OnOffFanPartLoadFraction = 1.0d0
      OnOffAirFlowRatio = 1.0d0

      ! Get full load result
      CALL CalcFurnaceOutput(FurnaceNum,FirstHVACIteration,OpMode,CompOp,  &
                             0.0d0,PartLoadRatio,0.0d0,0.0d0,FullSensibleOutput, FullLatentOutput, &
                             OnOffAirFlowRatio, .FALSE.)

      ! Check that SystemSensibleLoad is between FullSensibleOutput and NoHeatOutput
      ! If so then calculate PartLoadRatio for the DX Heating coil
      IF(SystemSensibleLoad .LT. FullSensibleOutput .AND. SystemSensibleLoad .GT. NoHeatOutput)THEN

!       check bounds on sensible output prior to iteration using RegulaFalsi
        IF(FullSensibleOutput .LT. SystemSensibleLoad)THEN
          PartLoadRatio = 1.0d0
        ELSEIF(NoHeatOutput .GT. SystemSensibleLoad)THEN
          PartLoadRatio = 0.0d0
        ELSE

          ! Calculate the part load ratio through iteration
          HeatErrorToler = Furnace(FurnaceNum)%HeatingConvergenceTolerance !Error tolerance for convergence from input deck

          SolFlag = 0    ! # of iterations if positive, -1 means failed to converge, -2 means bounds are incorrect
          Par(1)  = REAL(FurnaceNum,r64)
          Par(2)  = 0.0d0  ! FLAG, if 1.0 then FirstHVACIteration equals TRUE, if 0.0 then FirstHVACIteration equals false
          IF(FirstHVACIteration)Par(2)=1.0d0
          Par(3)  = REAL(OpMode,r64)
          Par(4)  = REAL(CompOp,r64)
          Par(5)  = SystemSensibleLoad
          Par(6)  = 0.0d0 ! FLAG, 0.0 if heating load, 1.0 if cooling or moisture load
          Par(7)  = 1.0d0 ! FLAG, 0.0 if latent load, 1.0 if sensible load to be met
          Par(8)  = OnOffAirFlowRatio ! Ratio of compressor ON mass flow rate to AVERAGE mass flow rate over time step
          Par(9) = 0.0d0 ! HXUnitOn is always false for HX
          Par(10) = 0.0d0
!         HeatErrorToler is in fraction of load, MaxIter = 30, SolFalg = # of iterations or error as appropriate
          CALL SolveRegulaFalsi(HeatErrorToler, MaxIter, SolFlag, PartLoadRatio, CalcFurnaceResidual, 0.0d0, 1.0d0, Par)
!         OnOffAirFlowRatio is updated during the above iteration. Reset to correct value based on PLR.
          OnOffAirFlowRatio = OnOffAirFlowRatioSave
          IF (SolFlag == -1) THEN
            CALL CalcFurnaceOutput(FurnaceNum,FirstHVACIteration,OpMode,CompOp,0.0d0, &
                                   PartLoadRatio,0.0d0,0.0d0, TempHeatOutput, TempLatentOutput, OnOffAirFlowRatio, .FALSE.)
            IF(ABS(SystemSensibleLoad - TempHeatOutput) .GT. SmallLoad)THEN
              IF(Furnace(FurnaceNum)%DXHeatingMaxIterIndex == 0)THEN
                CALL ShowWarningMessage('Heating coil control failed to converge for ' &
                       //TRIM(cFurnaceTypes(Furnace(FurnaceNum)%FurnaceType_Num))//':'//TRIM(Furnace(FurnaceNum)%Name))
                CALL ShowContinueError('  Iteration limit exceeded in calculating DX heating coil sensible part-load ratio.')
                CALL ShowContinueErrorTimeStamp('Sensible load to be met by DX heating coil = ' &
                     //TRIM(TrimSigDigits(SystemSensibleLoad,2))//' (watts), sensible output of DX heating coil = ' &
                     //TRIM(TrimSigDigits(TempHeatOutput,2))//' (watts), and the simulation continues.')
              END IF
              CALL ShowRecurringWarningErrorAtEnd(TRIM(cFurnaceTypes(Furnace(FurnaceNum)%FurnaceType_Num))//' "'&
                  //TRIM(Furnace(FurnaceNum)%Name)//'" - Iteration limit exceeded in calculating'// &
                  ' DX sensible heating part-load ratio error continues. Sensible load statistics:' &
                  ,Furnace(FurnaceNum)%DXHeatingMaxIterIndex,SystemSensibleLoad,SystemSensibleLoad)
            END IF
          ELSE IF (SolFlag == -2) THEN
            IF(Furnace(FurnaceNum)%DXHeatingRegulaFalsiFailedIndex == 0)THEN
              CALL ShowWarningMessage('Heating coil control failed for ' &
                      //TRIM(cFurnaceTypes(Furnace(FurnaceNum)%FurnaceType_Num))//':'//TRIM(Furnace(FurnaceNum)%Name))
              CALL ShowContinueError('  DX sensible heating part-load ratio determined to be outside the range of 0-1.')
              CALL ShowContinueErrorTimeStamp('Sensible load to be met by DX heating coil = ' &
                   //TRIM(TrimSigDigits(SystemSensibleLoad,2))//' (watts), and the simulation continues.')
            END IF
            CALL ShowRecurringWarningErrorAtEnd(TRIM(cFurnaceTypes(Furnace(FurnaceNum)%FurnaceType_Num))//' "'&
                //TRIM(Furnace(FurnaceNum)%Name)//'" - '// &
                ' DX sensible heating part-load ratio out of range error continues. Sensible load statistics:' &
                ,Furnace(FurnaceNum)%DXHeatingRegulaFalsiFailedIndex,SystemSensibleLoad,SystemSensibleLoad)
          END IF
        END IF

        Furnace(FurnaceNum)%HeatPartLoadRatio = PartLoadRatio
!       Check if Heat Pump compressor is allowed to run based on outdoor temperature
        IF (Furnace(FurnaceNum)%CondenserNodeNum > 0) THEN
          IF (Node(Furnace(FurnaceNum)%CondenserNodeNum)%Temp .GT. Furnace(FurnaceNum)%MinOATCompressor) THEN
            Furnace(FurnaceNum)%CompPartLoadRatio = PartLoadRatio
          ELSE
            Furnace(FurnaceNum)%CompPartLoadRatio = 0.0d0
          END IF
        ELSE
          IF (OutDryBulbTemp .GT. Furnace(FurnaceNum)%MinOATCompressor) THEN
            Furnace(FurnaceNum)%CompPartLoadRatio = PartLoadRatio
          ELSE
            Furnace(FurnaceNum)%CompPartLoadRatio = 0.0d0
          END IF
        ENDIF
      ELSEIF(SystemSensibleLoad .GT. FullSensibleOutput) THEN
!       SystemSensibleLoad is greater than full DX Heating coil output so heat pump runs entire
!       timestep and additional supplemental heating is required
        Furnace(FurnaceNum)%HeatPartLoadRatio = 1.0d0
        IF (Furnace(FurnaceNum)%CondenserNodeNum > 0) THEN
          IF(Node(Furnace(FurnaceNum)%CondenserNodeNum)%Temp .GT. Furnace(FurnaceNum)%MinOATCompressor) THEN
  !       Check to see if Heat Pump compressor was allowed to run based on outdoor temperature
            Furnace(FurnaceNum)%CompPartLoadRatio = 1.0d0
          ELSE
            Furnace(FurnaceNum)%CompPartLoadRatio = 0.0d0
          END IF
        ELSE
          IF(OutDryBulbTemp .GT. Furnace(FurnaceNum)%MinOATCompressor) THEN
  !       Check to see if Heat Pump compressor was allowed to run based on outdoor temperature
            Furnace(FurnaceNum)%CompPartLoadRatio = 1.0d0
          ELSE
            Furnace(FurnaceNum)%CompPartLoadRatio = 0.0d0
          END IF
        ENDIF
      ELSEIF(SystemSensibleLoad .LT. NoHeatOutput) THEN
!       SystemSensibleLoad is less than minimum DX Heating coil output so heat pump does not run and
!       the load will be met by the supplemental heater
        Furnace(FurnaceNum)%CompPartLoadRatio = 0.0d0
        Furnace(FurnaceNum)%HeatPartLoadRatio = 1.0d0
      ENDIF
      IF (Furnace(FurnaceNum)%HeatPartLoadRatio .EQ. 1.0d0 ) THEN
!       Determine the load on the supplemental heating coil
        IF((SystemSensibleLoad - FullSensibleOutput) .GT. Furnace(FurnaceNum)%DesignSuppHeatingCapacity) THEN
          HeatCoilLoad = Furnace(FurnaceNum)%DesignSuppHeatingCapacity
          TempOutHeatingCoil = Node(FurnaceOutletNode)%Temp + HeatCoilLoad/(cpair*Furnace(FurnaceNum)%MdotFurnace)
        ELSEIF(SystemSensibleLoad .LT. NoHeatOutput) THEN
          HeatCoilLoad = MAX(0.0d0,SystemSensibleLoad)   !BG 10/22/2008 need a case for when its all suppl heat
          TempOutHeatingCoil = Node(FurnaceInletNode)%Temp + HeatCoilLoad/(cpair*Furnace(FurnaceNum)%MdotFurnace)
        ELSE
          HeatCoilLoad = MAX(0.0d0,(SystemSensibleLoad - FullSensibleOutput))
          TempOutHeatingCoil = Node(FurnaceOutletNode)%Temp + HeatCoilLoad/(cpair*Furnace(FurnaceNum)%MdotFurnace)
        END IF
        IF (Furnace(FurnaceNum)%CondenserNodeNum > 0) THEN
          IF(Node(Furnace(FurnaceNum)%CondenserNodeNum)%Temp .GT. Furnace(FurnaceNum)%MaxOATSuppHeat) THEN
             HeatCoilLoad = 0.0d0
             IF(SystemSensibleLoad .LT. NoHeatOutput) THEN
               TempOutHeatingCoil = Node(FurnaceInletNode)%Temp
             ELSE
               TempOutHeatingCoil = Node(FurnaceOutletNode)%Temp
             ENDIF
          ENDIF
        ELSE
          IF(OutDryBulbTemp .GT. Furnace(FurnaceNum)%MaxOATSuppHeat) THEN
            HeatCoilLoad = 0.0d0
             IF(SystemSensibleLoad .LT. NoHeatOutput) THEN
               TempOutHeatingCoil = Node(FurnaceInletNode)%Temp
             ELSE
               TempOutHeatingCoil = Node(FurnaceOutletNode)%Temp
             ENDIF
          ENDIF
        ENDIF
        cpair = PsyCpAirFnWTdb(Node(FurnaceInletNode)%HumRat,Node(FurnaceOutletNode)%Temp)
       ! TempOutHeatingCoil = Node(FurnaceOutletNode)%Temp + HeatCoilLoad/(cpair*Furnace(FurnaceNum)%MdotFurnace)
        IF((TempOutHeatingCoil .GT. Furnace(FurnaceNum)%DesignMaxOutletTemp) .AND. (HeatCoilLoad > 0.0D0) ) THEN
         ! deltaT = Furnace(FurnaceNum)%DesignMaxOutletTemp - Node(FurnaceOutletNode)%Temp
            !BG 10/22/2008 above made no sense if DX heat is off and its all supplemental,
            !  because Node(FurnaceOutletNode)%Temp will have been calc'd with full DX heat in last faux call to CalcFurnaceOutput

          deltaT = (Furnace(FurnaceNum)%DesignMaxOutletTemp - TempOutHeatingCoil)
          HeatCoilLoad = HeatCoilLoad - (Node(FurnaceInletNode)%MassFlowRate * cpair * deltaT)
          HeatCoilLoad = MAX(0.0D0, HeatCoilLoad)
        END IF
      ELSE
        HeatCoilLoad = 0.0d0
      END IF
      PartLoadRatio = 0.0d0

!   HeatCool systems can have both a sensible and latent PLR in a single time step
!   (i.e. both cooling and heating can occur in a single time step)
    ELSE  ! else not a heatpump DX coil

      Node(FurnaceInletNode)%MassFlowRate = Furnace(FurnaceNum)%MdotFurnace
      HeatCoilLoad = Furnace(FurnaceNum)%DesignHeatingCapacity
      SystemSensibleLoad = ZoneLoad


      ! Get no load result
      IF (OpMode .EQ. CycFanCycCoil) THEN
        Node(FurnaceInletNode)%MassFlowRate = 0.0d0
      END IF
      IF (OpMode .EQ. ContFanCycCoil) THEN
        OnOffFanPartLoadFraction = 1.0d0 ! The on/off fan will not cycle, so set part-load fraction = 1
      END IF

!     Set the inlet mass flow rate based on user specified coil OFF flow rate
      PartLoadRatio = 0.0d0
      CALL SetAverageAirFlow(FurnaceNum, PartLoadRatio, OnOffAirFlowRatio)

      CALL CalcFurnaceOutput(FurnaceNum,FirstHVACIteration,OpMode,CompOp,0.0d0,MinPLR,0.0d0,0.0d0,NoHeatOutput,NoLatentOutput, &
                             OnOffAirFlowRatio, .FALSE.)

      IF(NoHeatOutput .LT. SystemSensibleLoad)THEN
        Node(FurnaceInletNode)%MassFlowRate = Furnace(FurnaceNum)%MdotFurnace

        ! Set fan part-load fraction equal to 1 while getting full load result
        OnOffFanPartLoadFraction = 1.0d0
        OnOffAirFlowRatio        = 1.0d0

        ! Get full load result
        CALL CalcFurnaceOutput(FurnaceNum,FirstHVACIteration,OpMode,CompOp,0.0d0,1.0d0,HeatCoilLoad,0.0d0,  &
                               FullSensibleOutput,FullLatentOutput, OnOffAirFlowRatio, .FALSE.)
      ELSE
        FullSensibleOutput = NoHeatOutput + 0.000000001d0
      END IF

      ! Since we are heating, we expect FullSensibleOutput to be > 0 and FullSensibleOutput > NoSensibleOutput
      ! Check that this is the case; if not set PartLoadRatio = 0.0 (off) and return


      IF(FullSensibleOutput.GT.NoHeatOutput)THEN

!       check bounds on sensible output prior to iteration using RegulaFalsi
        IF(FullSensibleOutput .LE. SystemSensibleLoad)THEN
          PartLoadRatio = 1.0d0
!         save modified HeatCoilLoad in case it was reset because outlet temp > DesignMaxOutletTemp
          IF(ModifiedHeatCoilLoad .GT. 0.0d0)THEN
            HeatCoilLoad = ModifiedHeatCoilLoad
          ELSE
            HeatCoilLoad = Furnace(FurnaceNum)%DesignHeatingCapacity
          END IF
        ELSEIF(NoHeatOutput .GE. SystemSensibleLoad)THEN
          PartLoadRatio = 0.0d0
          HeatCoilLoad  = 0.0d0
        ELSE

          ! Calculate the part load ratio through iteration
          HeatErrorToler = Furnace(FurnaceNum)%HeatingConvergenceTolerance !Error tolerance for convergence from input deck

          SolFlag = 0    ! # of iterations if positive, -1 means failed to converge, -2 means bounds are incorrect
          Par(1)  = REAL(FurnaceNum,r64)
          Par(2)  = 0.0d0  ! FLAG, if 1.0 then FirstHVACIteration equals TRUE, if 0.0 then FirstHVACIteration equals false
          IF(FirstHVACIteration)Par(2)=1.0d0
          Par(3)  = REAL(OpMode,r64)
          Par(4)  = REAL(CompOp,r64)
          Par(5)  = SystemSensibleLoad
          Par(6)  = 0.0d0 ! FLAG, 0.0 if heating load, 1.0 if cooling or moisture load
          Par(7)  = 1.0d0 ! FLAG, 0.0 if latent load, 1.0 if sensible load to be met
          Par(8)  = OnOffAirFlowRatio ! Ratio of compressor ON mass flow rate to AVERAGE mass flow rate over time step
          Par(9)  = 0.0d0 ! HXUnitOn is always false for HX
          Par(10) = 0.0d0
!         HeatErrorToler is in fraction load, MaxIter = 30, SolFalg = # of iterations or error as appropriate
          CALL SolveRegulaFalsi(HeatErrorToler, MaxIter, SolFlag, PartLoadRatio, CalcFurnaceResidual, 0.0d0, 1.0d0, Par)
!         OnOffAirFlowRatio is updated during the above iteration. Reset to correct value based on PLR.
          OnOffAirFlowRatio = OnOffAirFlowRatioSave
!         Reset HeatCoilLoad calculated in CalcFurnaceResidual (in case it was reset because output temp > DesignMaxOutletTemp)
          IF(ModifiedHeatCoilLoad .GT. 0.0d0)THEN
            HeatCoilLoad = ModifiedHeatCoilLoad
          ELSE
            HeatCoilLoad = Furnace(FurnaceNum)%DesignHeatingCapacity * PartLoadRatio
          END IF
          IF (SolFlag == -1) THEN

!           RegulaFalsi may not find heating PLR when the maximum supply air temperature is exceeded.
!           If iteration limit is exceeded, find tighter boundary of solution and repeat RegulaFalsi
            TempMaxPLR = -0.1d0
            TempHeatOutput = NoHeatOutput
            DO WHILE((TempHeatOutput - SystemSensibleLoad) .LT. 0.0d0 .AND. TempMaxPLR .LT. 1.0d0)
!             find upper limit of HeatingPLR
              TempMaxPLR = TempMaxPLR + 0.1d0
              HeatCoilLoad = Furnace(FurnaceNum)%DesignHeatingCapacity * TempMaxPLR
              CALL CalcFurnaceOutput(FurnaceNum,FirstHVACIteration,OpMode,CompOp, &
                   0.0d0, TempMaxPLR, HeatCoilLoad,0.0d0, TempHeatOutput, TempLatentOutput, OnOffAirFlowRatio, .FALSE.)
            END DO
            TempMinPLR = TempMaxPLR
            DO WHILE((TempHeatOutput - SystemSensibleLoad) .GT. 0.0d0 .AND. TempMinPLR .GT. 0.0d0)
!             pull upper limit of HeatingPLR down to last valid limit (i.e. heat output still exceeds SystemSensibleLoad)
              TempMaxPLR = TempMinPLR
!             find minimum limit of HeatingPLR
              TempMinPLR = TempMinPLR - 0.01d0

              HeatCoilLoad = Furnace(FurnaceNum)%DesignHeatingCapacity * TempMinPLR
              CALL CalcFurnaceOutput(FurnaceNum,FirstHVACIteration,OpMode,CompOp, &
                   0.0d0, TempMinPLR, HeatCoilLoad,0.0d0, TempHeatOutput, TempLatentOutput, OnOffAirFlowRatio, .FALSE.)
            END DO
!           Now solve again with tighter PLR limits
            CALL SolveRegulaFalsi(HeatErrorToler,MaxIter,SolFlag,PartLoadRatio,CalcFurnaceResidual,TempMinPLR,TempMaxPLR,Par)
            IF(ModifiedHeatCoilLoad .GT. 0.0d0)THEN
              HeatCoilLoad = ModifiedHeatCoilLoad
            ELSE
              HeatCoilLoad = Furnace(FurnaceNum)%DesignHeatingCapacity * PartLoadRatio
            END IF
            CALL CalcFurnaceOutput(FurnaceNum,FirstHVACIteration,OpMode,CompOp, &
                    0.0d0, PartLoadRatio, HeatCoilLoad,0.0d0, TempHeatOutput, TempLatentOutput, OnOffAirFlowRatio, .FALSE.)

!           After iterating with tighter boundaries, if still out of tolerance, show warning.
            IF(SolFlag == -1 .AND. ABS(SystemSensibleLoad - TempHeatOutput) .GT. SmallLoad)THEN
              IF(Furnace(FurnaceNum)%HeatingMaxIterIndex == 0)THEN
                CALL ShowWarningMessage('Heating coil control failed to converge for ' &
                      //TRIM(cFurnaceTypes(Furnace(FurnaceNum)%FurnaceType_Num))//':'//TRIM(Furnace(FurnaceNum)%Name))
                CALL ShowContinueError('  Iteration limit exceeded in calculating heating coil sensible part-load ratio.')
                CALL ShowContinueErrorTimeStamp('Sensible load to be met by heating coil = ' &
                     //TRIM(TrimSigDigits(SystemSensibleLoad,2))//' (watts), sensible output of heating coil = ' &
                     //TRIM(TrimSigDigits(TempHeatOutput,2))//' (watts), and the simulation continues.')
              END IF
              CALL ShowRecurringWarningErrorAtEnd(TRIM(cFurnaceTypes(Furnace(FurnaceNum)%FurnaceType_Num))//' "'&
                  //TRIM(Furnace(FurnaceNum)%Name)//'" - Iteration limit exceeded in calculating'// &
                  ' sensible heating part-load ratio error continues. Sensible load statistics:' &
                  ,Furnace(FurnaceNum)%HeatingMaxIterIndex,SystemSensibleLoad,SystemSensibleLoad)
            END IF
          ELSE IF (SolFlag == -2) THEN
            IF(Furnace(FurnaceNum)%HeatingRegulaFalsiFailedIndex == 0)THEN
              CALL ShowWarningMessage('Heating coil control failed for ' &
                  //TRIM(cFurnaceTypes(Furnace(FurnaceNum)%FurnaceType_Num))//':'//TRIM(Furnace(FurnaceNum)%Name))
              CALL ShowContinueError('  Sensible heating part-load ratio determined to be outside the range of 0-1.')
              CALL ShowContinueErrorTimeStamp('Sensible load to be met by heating coil = ' &
                   //TRIM(TrimSigDigits(SystemSensibleLoad,2))//' (watts), and the simulation continues.')
            END IF
            CALL ShowRecurringWarningErrorAtEnd(TRIM(cFurnaceTypes(Furnace(FurnaceNum)%FurnaceType_Num))//' "'&
                //TRIM(Furnace(FurnaceNum)%Name)//'" - '// &
                ' Sensible heating part-load ratio out of range error continues. Sensible load statistics:' &
                ,Furnace(FurnaceNum)%HeatingRegulaFalsiFailedIndex,SystemSensibleLoad,SystemSensibleLoad)
          END IF
        END IF

      ELSE  !ELSE from IF(FullSensibleOutput.GT.NoSensibleOutput)THEN above
        ! Set part load ratio to 1 and run heater at design heating capacity
        PartLoadRatio = 1.0d0
        HeatCoilLoad = Furnace(FurnaceNum)%DesignHeatingCapacity
      END IF

    END IF !End of IF HeatPump

  END IF !End of IF for heating

! Non-heat pump systems do not set a heating PLR, set it here for use with the DX cooling coil calculations.
! Set this variable back to 0 for non-heat pump systems at the end of this routine.
  Furnace(FurnaceNum)%HeatPartLoadRatio = MAX(PartLoadRatio,Furnace(FurnaceNum)%HeatPartLoadRatio)
  CALL CalcFurnaceOutput(FurnaceNum,FirstHVACIteration,OpMode,CompOp,0.0d0, &
                         Furnace(FurnaceNum)%HeatPartLoadRatio,HeatCoilLoad,0.0d0,  &
                         HeatingSensibleOutput,HeatingLatentOutput, OnOffAirFlowRatio, .FALSE.)

  IF (Furnace(FurnaceNum)%FurnaceType_Num == UnitarySys_HeatPump_AirToAir  .OR. &
     (Furnace(FurnaceNum)%FurnaceType_Num == UnitarySys_HeatPump_WaterToAir  .AND. &
      Furnace(FurnaceNum)%WatertoAirHPType == WatertoAir_Simple ) .AND. CoolingLoad) THEN
      HeatingSensibleOutput = 0.0d0
      HeatingLatentOutput = 0.0d0
  ENDIF
!***********Cooling Section*****************
! Simulate if scheduled ON and cooling load or if a moisture load exists when using a humidistat
! Check of HeatingLatentOutput is used to reduce overshoot during simultaneous heating and cooling
! Setback flag is used to avoid continued RH control when Tstat is setback (RH should float down)
    IF((GetCurrentScheduleValue(Furnace(FurnaceNum)%SchedPtr) .gt. 0.0d0 .and. CoolingLoad) .or. &
       (Furnace(FurnaceNum)%Humidistat .AND. &
        Furnace(FurnaceNum)%DehumidControlType_Num == DehumidControl_CoolReheat .and. &
       (SystemMoistureLoad.lt.0.0d0 .OR. (SystemMoistureLoad .GE. 0.0d0 .AND. &
       HeatingLatentOutput .GT. SystemMoistureLoad .AND. .NOT. Setback(Furnace(FurnaceNum)%ControlZoneNum))))) THEN


!     For cooling operation, the first step is to set the HX operation flag in case a HX assisted coil is used.
!      (if a HX assisted coil is not used, this flag is not used. It's only used in the CALL to SimHXAssistedCoolingCoil)
!     Check the dehumidification control type:
!           For dehumidification control options CoolReheat and None, the HX is always active (locked ON).
!           For dehumidification control option Multimode, the system is operated first with the HX off.
!           If the moisture load is not met, the HX will then be turned on and the system is re-simulated.

      IF(Furnace(FurnaceNum)%DehumidControlType_Num .EQ. DehumidControl_CoolReheat .OR. &
         Furnace(FurnaceNum)%DehumidControlType_Num .EQ. DehumidControl_None) THEN
        HXUnitOn = .TRUE.
      ELSE
        HXUnitOn = .FALSE.
      END IF

!     The next step is to determine the system output at no load (PLR=0) and full load (PLR=1)

!     Set the inlet mass flow rate based on user specified coil OFF flow rate
      PartLoadRatio = 0.0d0

      Furnace(FurnaceNum)%CompPartLoadRatio = 0.0d0    !compressor off
      Furnace(FurnaceNum)%WSHPRuntimeFrac = 0.0d0

!     SetAverageAirFlow calculates the operating mass flow rate based on PLR and the user specified inputs
!     for MaxCoolAirMassFlow and MaxNoCoolHeatAirMassFlow.
!     Air flow rate is set according to max of cooling and heating PLR if heating and latent load exists.
      IF(OpMode .EQ. CycFanCycCoil .AND. &
         Furnace(FurnaceNum)%HeatPartLoadRatio .GT. 0.0d0 .AND. &
         Furnace(FurnaceNum)%Humidistat .AND. &
         Furnace(FurnaceNum)%DehumidControlType_Num == DehumidControl_CoolReheat .AND. &
        (SystemMoistureLoad.lt.0.0d0 .OR. (SystemMoistureLoad .GE. 0.0d0 .AND. &
         HeatingLatentOutput .GT. SystemMoistureLoad .AND. .NOT. Setback(Furnace(FurnaceNum)%ControlZoneNum)))) THEN
         CoolingHeatingPLRRatio = MIN(1.0d0,PartLoadRatio/Furnace(FurnaceNum)%HeatPartLoadRatio)
        CALL SetAverageAirFlow(FurnaceNum, MAX(PartLoadRatio,Furnace(FurnaceNum)%HeatPartLoadRatio), OnOffAirFlowRatio)

      ELSE
        CoolingHeatingPLRRatio = 1.0d0
        CALL SetAverageAirFlow(FurnaceNum, PartLoadRatio, OnOffAirFlowRatio)

      END IF

      ! Get no load result (coils simulated OFF)
      CALL CalcFurnaceOutput(FurnaceNum,FirstHVACIteration,OpMode,CompOp,MinPLR,PartLoadRatio,0.0d0,0.0d0, &
                             NoCoolOutput, NoLatentOutput, OnOffAirFlowRatio, HXUnitOn, CoolingHeatingPLRRatio)

!     Don't calculate full load output if no load output can meet sensible load
      IF(NoCoolOutput .GE. CoolCoilLoad .AND. (CoolCoilLoad .NE. 0.0d0 .OR. HPDehumidificationLoadFlag))THEN
!       Set full mass flow rate for full load calculation
        Node(FurnaceInletNode)%MassFlowRate = Furnace(FurnaceNum)%MdotFurnace


        ! Set fan part-load fraction equal to 1 while getting full load result
        OnOffFanPartLoadFraction = 1.0d0
        OnOffAirFlowRatio        = 1.0d0
        PartLoadRatio            = 1.0d0
        Furnace(FurnaceNum)%CompPartLoadRatio = 1.0d0    !compressor ON
        Furnace(FurnaceNum)%WSHPRuntimeFrac   = 1.0d0

        ! Get full load result (coils simulated full ON)
        CALL CalcFurnaceOutput(FurnaceNum,FirstHVACIteration,OpMode,CompOp,PartLoadRatio,0.0d0,0.0d0,0.0d0, &
                               FullSensibleOutput, FullLatentOutput, OnOffAirFlowRatio, HXUnitOn)
      ELSE
        FullSensibleOutput = NoCoolOutput - 0.00000001d0
      END IF

!     The next step is to compare the results of the full load and no load results
!
!     1) Since we are cooling, we expect FullSensibleOutput < NoCoolOutput
!        Check that this is the case; if not set PartLoadRatio = 0.0 (off)
!     2) Verify that the load to be met is within the range of available output
!        (i.e. between FullSensibleOutput and NoCoolOutput)
!     3) Set PLR if load is out of range or RegulaFalsi on PLR if system can meet the load
!
      IF(FullSensibleOutput.LT.NoCoolOutput)THEN
        IF(CoolCoilLoad .NE. 0.0d0 .OR. HPDehumidificationLoadFlag)THEN

!           check bounds on sensible output prior to iteration using RegulaFalsi
!           Negative value represents cooling load, IF FullSensibleOutput .GT. CoolCoilLoad, load is greater than capacity
            IF(FullSensibleOutput .GE. CoolCoilLoad)THEN
              PartLoadRatio = 1.0d0
!           Likewise IF NoCoolOutput .LT. CoolCoilLoad, then load can be met using only the fan (constant fan mode only)
            ELSEIF(NoCoolOutput .LE. CoolCoilLoad)THEN
              PartLoadRatio = 0.0d0
!           ELSE load is between NoCoolOutput and FullSensibleOuput, find PLR required to meet load
            ELSE

            ! Calculate the sensible part load ratio through iteration
              CoolErrorToler = Furnace(FurnaceNum)%CoolingConvergenceTolerance !Error tolerance for convergence from input deck
              SolFlag = 0    ! # of iterations if positive, -1 means failed to converge, -2 means bounds are incorrect
              Par(1)  = REAL(FurnaceNum,r64)
              Par(2)  = 0.0d0  ! FLAG, if 1.0 then FirstHVACIteration equals TRUE, if 0.0 then FirstHVACIteration equals false
              IF(FirstHVACIteration)Par(2)=1.0d0
              Par(3)  = REAL(OpMode,r64)
              Par(4)  = REAL(CompOp,r64)
              Par(5)  = CoolCoilLoad
              Par(6)  = 1.0d0 ! FLAG, 0.0 if heating load, 1.0 if cooling or moisture load
              Par(7)  = 1.0d0 ! FLAG, 0.0 if latent load, 1.0 if sensible load to be met
              Par(8)  = OnOffAirFlowRatio ! Ratio of compressor ON mass flow rate to AVERAGE mass flow rate over time step
              IF(HXUnitOn)THEN
                Par(9) = 1.0d0
              ELSE
                Par(9) = 0.0d0
              END IF
!             Par(10) is the heating coil PLR, set this value to 0 for sensible PLR calculations.
              Par(10) = 0.0d0
!             CoolErrorToler is in fraction of load, MaxIter = 30, SolFalg = # of iterations or error as appropriate
              CALL SolveRegulaFalsi(CoolErrorToler, MaxIter, SolFlag, PartLoadRatio, CalcFurnaceResidual, 0.0d0, 1.0d0, Par)
!             OnOffAirFlowRatio is updated during the above iteration. Reset to correct value based on PLR.
              OnOffAirFlowRatio = OnOffAirFlowRatioSave
              IF (SolFlag == -1) THEN
                CALL CalcFurnaceOutput(FurnaceNum,FirstHVACIteration,OpMode,CompOp,PartLoadRatio, &
                               0.0d0,0.0d0,0.0d0, TempCoolOutput, TempLatentOutput, OnOffAirFlowRatio, HXUnitOn)
                IF(.NOT. WarmupFlag)THEN
                  IF(ABS(CoolCoilLoad - TempCoolOutput) .GT. SmallLoad)THEN
                    IF(Furnace(FurnaceNum)%SensibleMaxIterIndex == 0)THEN
                      CALL ShowWarningMessage('Cooling coil control failed to converge for ' &
                                //TRIM(cFurnaceTypes(Furnace(FurnaceNum)%FurnaceType_Num))//':'//TRIM(Furnace(FurnaceNum)%Name))
                      CALL ShowContinueError('  Iteration limit exceeded in calculating DX cooling coil sensible part-load ratio.')
                      CALL ShowContinueErrorTimeStamp('Sensible load to be met by DX coil = ' &
                           //TRIM(TrimSigDigits(CoolCoilLoad,2))//' (watts), sensible output of DX coil = ' &
                           //TRIM(TrimSigDigits(TempCoolOutput,2))//' (watts), and the simulation continues.')
                    END IF
                    CALL ShowRecurringWarningErrorAtEnd(TRIM(cFurnaceTypes(Furnace(FurnaceNum)%FurnaceType_Num))//' "'&
                        //TRIM(Furnace(FurnaceNum)%Name)//'" - Iteration limit exceeded in calculating'// &
                        ' sensible cooling part-load ratio error continues. Sensible load statistics:' &
                        ,Furnace(FurnaceNum)%SensibleMaxIterIndex,CoolCoilLoad,CoolCoilLoad)
                  END IF
                END IF
              ELSE IF (SolFlag == -2) THEN
                IF(.NOT. WarmupFlag)THEN
                  IF(Furnace(FurnaceNum)%SensibleRegulaFalsiFailedIndex == 0)THEN
                    CALL ShowWarningMessage('Cooling coil control failed for ' &
                            //TRIM(cFurnaceTypes(Furnace(FurnaceNum)%FurnaceType_Num))//':'//TRIM(Furnace(FurnaceNum)%Name))
                    CALL ShowContinueError('  Cooling sensible part-load ratio determined to be outside the range of 0-1.')
                    CALL ShowContinueErrorTimeStamp('  Cooling sensible load = '//TRIM(TrimSigDigits(CoolCoilLoad,2)))
                  END IF
                  CALL ShowRecurringWarningErrorAtEnd(TRIM(cFurnaceTypes(Furnace(FurnaceNum)%FurnaceType_Num))//' "'&
                      //TRIM(Furnace(FurnaceNum)%Name)//'" - Cooling sensible part-load ratio out of range'// &
                      ' error continues. Sensible cooling load statistics:' &
                      ,Furnace(FurnaceNum)%SensibleRegulaFalsiFailedIndex,CoolCoilLoad,CoolCoilLoad)
                END IF
              END IF
            END IF

        ELSE
          PartLoadRatio = 0.0d0
        END IF ! EndIf for IF(CoolCoilLoad.NE.0.0)

!       Calculate the delivered capacity from the PLR caculated above
        CALL CalcFurnaceOutput(FurnaceNum,FirstHVACIteration,OpMode,CompOp,PartLoadRatio,Furnace(FurnaceNum)%HeatPartLoadRatio, &
                               0.0d0,0.0d0, TempCoolOutput, TempLatentOutput, OnOffAirFlowRatio, HXUnitOn)

!       Calculate the latent part load ratio through iteration
!       Negative SystemMoistureLoad means dehumidification load is present
!
!       IF this furnace uses MultiMode control AND there is a moisture load AND the moisture load met by the furnace in
!       cooling only mode above is sufficient to meet the moisture demand OR there is no sensible load (PLR=0 from above)
!       then set LatentPartLoadRatio to 0 (no additional dehumidification is required).
        IF(Furnace(FurnaceNum)%DehumidControlType_Num == DehumidControl_Multimode .AND. ((SystemMoistureLoad .LT. 0.0d0 .AND. &
           TempLatentOutput .LT. SystemMoistureLoad) .OR. PartLoadRatio .EQ. 0.0d0)) THEN
          LatentPartLoadRatio = 0.0d0
!       ELSE calculate a new PLR for valid dehumidification control types if a moisture load exists.
        ELSE IF(Furnace(FurnaceNum)%DehumidControlType_Num /= DehumidControl_None .AND. &
             (SystemMoistureLoad .LT. 0.0d0 .OR. (SystemMoistureLoad .GE. 0.0d0 .AND. TempLatentOutput .GT. SystemMoistureLoad &
             .AND. .NOT. Setback(Furnace(FurnaceNum)%ControlZoneNum))))THEN

!         IF the furnace uses dehumidification control MultiMode, turn on the HX and calculate the latent output with
!         the HX ON to compare to the moisture load predicted by the humidistat.
          IF(Furnace(FurnaceNum)%DehumidControlType_Num == DehumidControl_Multimode)THEN
            HXUnitOn = .TRUE.
            Node(FurnaceInletNode)%MassFlowRate = Furnace(FurnaceNum)%MdotFurnace
            ! Set fan part-load fraction equal to 1 while getting full load result
            OnOffFanPartLoadFraction = 1.0d0
            OnOffAirFlowRatio        = 1.0d0
            ! Get full load result
            CALL CalcFurnaceOutput(FurnaceNum,FirstHVACIteration,OpMode,CompOp,1.0d0,0.0d0,0.0d0,0.0d0, TempCoolOutput, &
                             TempLatentOutput, OnOffAirFlowRatio, HXUnitOn)
          END IF

!         Set the global cooling to heating PLR ratio. CoolHeatPLRRat = MIN(1,CoolingPLR/HeatingPLR)
          CoolHeatPLRRat = 1.0d0 ! means cooling dominated operation (applies to cycling fan mode)

          IF(TempLatentOutput .GT. SystemMoistureLoad)THEN
!           Set full mass flow rate for full load calculation
            Node(FurnaceInletNode)%MassFlowRate = Furnace(FurnaceNum)%MdotFurnace


            ! Set fan part-load fraction equal to 1 while getting full load result
            OnOffFanPartLoadFraction = 1.0d0
            OnOffAirFlowRatio        = 1.0d0
            Furnace(FurnaceNum)%CompPartLoadRatio = 1.0d0    !compressor ON
            Furnace(FurnaceNum)%WSHPRuntimeFrac   = 1.0d0

            ! Get full load result (coils simulated full ON)
            CALL CalcFurnaceOutput(FurnaceNum,FirstHVACIteration,OpMode,CompOp,1.0d0,0.0d0,0.0d0,0.0d0, &
                                   TempCoolOutput, TempLatentOutput, OnOffAirFlowRatio, HXUnitOn)
          END IF

!         check bounds on latent output prior to iteration using RegulaFalsi
          IF(TempLatentOutput .GT. SystemMoistureLoad .OR. &
            (Furnace(FurnaceNum)%DehumidControlType_Num == DehumidControl_Multimode .AND. TempCoolOutput.GT.CoolCoilLoad))THEN
            LatentPartLoadRatio = 1.0d0
          ELSEIF(NoLatentOutput .LT. SystemMoistureLoad .OR. HeatingLatentOutput .LT. SystemMoistureLoad)THEN
            LatentPartLoadRatio = 0.0d0
          ELSE

            CoolErrorToler = Furnace(FurnaceNum)%CoolingConvergenceTolerance !Error tolerance for convergence

            SolFlag = 0    ! # of iterations if positive, -1 means failed to converge, -2 means bounds are incorrect
            Par(1)  = REAL(FurnaceNum,r64)
            Par(2)  = 0.0d0  ! FLAG, if 1.0 then FirstHVACIteration equals TRUE, if 0.0 then FirstHVACIteration equals false
            IF(FirstHVACIteration)Par(2)=1.0d0
            Par(3)  = REAL(OpMode,r64)
            Par(4)  = REAL(CompOp,r64)
!           Multimode always controls to meet the SENSIBLE load (however, HXUnitOn is now TRUE)
            IF(Furnace(FurnaceNum)%DehumidControlType_Num == DehumidControl_Multimode)THEN
              Par(5) = CoolCoilLoad
            ELSE
              Par(5) = SystemMoistureLoad
            END IF
            Par(6)  = 1.0d0 ! FLAG, 0.0 if heating load, 1.0 if cooling or moisture load
!           Multimode always controls to meet the SENSIBLE load (however, HXUnitOn is now TRUE)
            IF(Furnace(FurnaceNum)%DehumidControlType_Num == DehumidControl_Multimode)THEN
              Par(7) = 1.0d0 ! FLAG, 0.0 if latent load, 1.0 if sensible load to be met
            ELSE
              Par(7) = 0.0d0
            END IF
            Par(8)  = OnOffAirFlowRatio ! Ratio of compressor ON mass flow rate to AVERAGE mass flow rate over time step
            IF(HXUnitOn)THEN
              Par(9) = 1.0d0
            ELSE
              Par(9) = 0.0d0
            END IF
!           Par(10) used only with cycling fan.
!           Par(10) is the heating coil PLR, set this value only if there is a heating load (heating PLR > 0)
!           and the latent PLR is being calculated. Otherwise set Par(10) to 0.
            IF(OpMode .EQ. CycFanCycCoil .AND. Furnace(FurnaceNum)%HeatPartLoadRatio .GT. 0.0d0 .AND. Par(7) .EQ. 0.0d0)THEN
              Par(10) = Furnace(FurnaceNum)%HeatPartLoadRatio
            ELSE
              Par(10) = 0.0d0
            END IF
!           CoolErrorToler is in fraction of load, MaxIter = 30, SolFalg = # of iterations or error as appropriate
            CALL SolveRegulaFalsi(CoolErrorToler, MaxIter, SolFlag, LatentPartLoadRatio, CalcFurnaceResidual, 0.0d0, 1.0d0, Par)
!           OnOffAirFlowRatio is updated during the above iteration. Reset to correct value based on PLR.
            OnOffAirFlowRatio = OnOffAirFlowRatioSave
            IF (SolFlag == -1) THEN
!             RegulaFalsi may not find latent PLR when the latent degradation model is used.
!             If iteration limit is exceeded, find tighter boundary of solution and repeat RegulaFalsi
              TempMaxPLR = -0.1d0
              TempLatentOutput = NoLatentOutput
              DO WHILE((TempLatentOutput - SystemMoistureLoad) .GT. 0.0d0 .AND. TempMaxPLR .LT. 1.0d0)
!               find upper limit of LatentPLR
                TempMaxPLR = TempMaxPLR + 0.1d0

!               Same calculation as is done in Function CalcFurnaceResidual for latent PLR calculation.
!               Set cooling to heating PLR for use with Subroutine CalcFurnaceOutput. IF Par(10) = 0,
!               heating PLR = 0 so set the CoolingHeatingPLRRatio to 1 so the cooling PLR is used in the
!               DX cooling coil calculations.
                IF(Par(10) .GT. 0.0d0)THEN
!                 Par(10) = Furnace(FurnaceNum)%HeatPartLoadRatio
!                 OpMode = CycFan and Furnace(FurnaceNum)%HeatPartLoadRatio must be > 0 for Part(10) to be greater than 0
                  CoolingHeatingPLRRatio = MIN(1.0d0,TempMaxPLR/Furnace(FurnaceNum)%HeatPartLoadRatio)
                ELSE
                  CoolingHeatingPLRRatio = 1.0d0
                END IF

                CALL CalcFurnaceOutput(FurnaceNum,FirstHVACIteration,OpMode,CompOp,TempMaxPLR, &
                                       0.0d0,0.0d0,0.0d0, TempCoolOutput, TempLatentOutput, OnOffAirFlowRatio, &
                                       HXUnitOn, CoolingHeatingPLRRatio)
              END DO
              TempMinPLR = TempMaxPLR
              DO WHILE((TempLatentOutput - SystemMoistureLoad) .LT. 0.0d0 .AND. TempMinPLR .GT. 0.0d0)
!               pull upper limit of LatentPLR down to last valid limit (i.e. latent output still exceeds SystemMoisuterLoad)
!               CR7558 - relax final limits to allow HX assisted coils to converge
                TempMaxPLR = TempMinPLR + 0.001d0
!               find minimum limit of Latent PLR
                TempMinPLR = TempMinPLR - 0.001d0

!               Set cooling to heating PLR for use with Subroutine CalcFurnaceOutput.
                IF(Par(10) .GT. 0.0d0)THEN
!                 Par(10) = Furnace(FurnaceNum)%HeatPartLoadRatio
!                 OpMode = CycFan and Furnace(FurnaceNum)%HeatPartLoadRatio must be > 0 for Part(10) to be greater than 0
!                 Since the latent output of cycling fan systems is 0 at PLR=0, do not allow the PLR to be 0,
!                 otherwise RegulaFalsi can fail when a heating and moisture load exists and heating PLR > latent PLR.
                  TempMinPLR2 = MAX(0.0000000001d0,TempMinPLR)
                  CoolingHeatingPLRRatio = MIN(1.0d0,TempMinPLR2/Furnace(FurnaceNum)%HeatPartLoadRatio)
                ELSE
                  TempMinPLR2 = TempMinPLR
                  CoolingHeatingPLRRatio = 1.0d0
                END IF

                CALL CalcFurnaceOutput(FurnaceNum,FirstHVACIteration,OpMode,CompOp,TempMinPLR2, &
                                       0.0d0,0.0d0,0.0d0, TempCoolOutput, TempLatentOutput, OnOffAirFlowRatio, &
                                       HXUnitOn, CoolingHeatingPLRRatio)
              END DO
!             tighter boundary of solution has been found, call RegulaFalsi a second time
              CALL SolveRegulaFalsi(CoolErrorToler,MaxIter,SolFlag,LatentPartLoadRatio, &
                                    CalcFurnaceResidual,TempMinPLR2,TempMaxPLR,Par)
!             OnOffAirFlowRatio is updated during the above iteration. Reset to correct value based on PLR.
              OnOffAirFlowRatio = OnOffAirFlowRatioSave
              IF (SolFlag == -1) THEN

!               Set cooling to heating PLR for use with Subroutine CalcFurnaceOutput.
                IF(Par(10) .GT. 0.0d0)THEN
!                 Par(10) = Furnace(FurnaceNum)%HeatPartLoadRatio
!                 OpMode = CycFan and Furnace(FurnaceNum)%HeatPartLoadRatio must be > 0 for Part(10) to be greater than 0
                  CoolingHeatingPLRRatio = MIN(1.0d0,LatentPartLoadRatio/Furnace(FurnaceNum)%HeatPartLoadRatio)
                ELSE
                  CoolingHeatingPLRRatio = 1.0d0
                END IF

                CALL CalcFurnaceOutput(FurnaceNum,FirstHVACIteration,OpMode,CompOp,LatentPartLoadRatio, &
                                       0.0d0,0.0d0,0.0d0, TempCoolOutput, TempLatentOutput, OnOffAirFlowRatio, &
                                       HXUnitOn, CoolingHeatingPLRRatio)
                IF(ABS((SystemMoistureLoad - TempLatentOutput)/SystemMoistureLoad) .GT. CoolErrorToler .AND. &
                   ABS(SystemMoistureLoad - TempLatentOutput).GT. 10.0d0)THEN
                  IF(.NOT. WarmupFlag)THEN
                    IF(Furnace(FurnaceNum)%LatentMaxIterIndex == 0)THEN
                      CALL ShowWarningMessage('Cooling coil control failed to converge for ' &
                             //TRIM(cFurnaceTypes(Furnace(FurnaceNum)%FurnaceType_Num))//':'//TRIM(Furnace(FurnaceNum)%Name))
                      CALL ShowContinueError('  Iteration limit exceeded in calculating cooling coil latent part-load ratio.')
                      CALL ShowContinueError('  Latent load convergence error (percent) = '// &
                                  TRIM(TrimSigDigits(100.0d0*ABS((SystemMoistureLoad - TempLatentOutput)/SystemMoistureLoad),2)))
                      CALL ShowContinueErrorTimeStamp('Moisture load to be met by DX coil = ' &
                           //TRIM(TrimSigDigits(SystemMoistureLoad,2))//' (watts), Latent output of DX coil = ' &
                           //TRIM(TrimSigDigits(TempLatentOutput,2))//' (watts), and the simulation continues.')
                    ENDIF
                    CALL ShowRecurringWarningErrorAtEnd(TRIM(cFurnaceTypes(Furnace(FurnaceNum)%FurnaceType_Num))//' "'&
                        //TRIM(Furnace(FurnaceNum)%Name)//'" - Iteration limit exceeded in calculating'// &
                        ' latent part-load ratio error continues. Latent load convergence error (percent) statistics follow.' &
                        ,Furnace(FurnaceNum)%LatentMaxIterIndex, &
                            100.0d0*ABS((SystemMoistureLoad - TempLatentOutput)/SystemMoistureLoad), &
                            100.0d0*ABS((SystemMoistureLoad - TempLatentOutput)/SystemMoistureLoad))
                  END IF
                END IF
              ELSE IF (SolFlag == -2) THEN
                IF(Furnace(FurnaceNum)%LatentRegulaFalsiFailedIndex2 == 0)THEN
                  CALL ShowWarningMessage('Cooling coil control failed for ' &
                        //TRIM(cFurnaceTypes(Furnace(FurnaceNum)%FurnaceType_Num))//':'//TRIM(Furnace(FurnaceNum)%Name))
                  CALL ShowContinueError('  Latent part-load ratio determined to be outside the range of ' &
                                       //TrimSigDigits(TempMinPLR,3)//' to '//TrimSigDigits(TempMaxPLR,3)//'.')
                  CALL ShowContinueErrorTimeStamp('A PLR of '//TRIM(TrimSigDigits(TempMinPLR,3))//' will be used and the ' &
                                                //'simulation continues.')
                ENDIF
                CALL ShowRecurringWarningErrorAtEnd(TRIM(cFurnaceTypes(Furnace(FurnaceNum)%FurnaceType_Num))//' "'&
                    //TRIM(Furnace(FurnaceNum)%Name)//'" - Cooling sensible part-load ratio out of range'// &
                    ' error continues. System moisture load statistics:' &
                    ,Furnace(FurnaceNum)%LatentRegulaFalsiFailedIndex2,SystemMoistureLoad,SystemMoistureLoad)
                LatentPartLoadRatio = TempMinPLR
              END IF
            ELSE IF (SolFlag == -2) THEN
              IF(Furnace(FurnaceNum)%LatentRegulaFalsiFailedIndex == 0)THEN
                CALL ShowWarningMessage('Cooling coil control failed for ' &
                      //TRIM(cFurnaceTypes(Furnace(FurnaceNum)%FurnaceType_Num))//':'//TRIM(Furnace(FurnaceNum)%Name))
                CALL ShowContinueError('  Latent part-load ratio determined to be outside the range of 0-1.')
                CALL ShowContinueErrorTimeStamp('A PLR of 0 will be used and the simulation continues.')
              ENDIF
              CALL ShowRecurringWarningErrorAtEnd(TRIM(cFurnaceTypes(Furnace(FurnaceNum)%FurnaceType_Num))//' "'&
                  //TRIM(Furnace(FurnaceNum)%Name)//'" - Latent part-load ratio out of range or 0-1'// &
                  ' error continues. System moisture load statistics:' &
                  ,Furnace(FurnaceNum)%LatentRegulaFalsiFailedIndex,SystemMoistureLoad,SystemMoistureLoad)
              LatentPartLoadRatio = 0.0d0
            END IF
          END IF

!         Cooling to heating PLR ratio is now known as CoolHeatPLRRat (Module level global set in CalcFurnaceOutput
!         This same variable is use in Subroutine SimFurnace for final calculations.
!         Get the actual output in case reheat needs to be calculated (HumControl=TRUE [latent PLR > sensible PLR])
          CALL CalcFurnaceOutput(FurnaceNum,FirstHVACIteration,OpMode,CompOp,LatentPartLoadRatio, &
                                0.0d0,0.0d0,0.0d0,ActualSensibleOutput, ActualLatentOutput, OnOffAirFlowRatio, &
                                HXUnitOn, CoolHeatPLRRat)

        ELSE
          LatentPartLoadRatio = 0.0d0
        END IF !ENDIF for valid dehumidification control types

!       IF a humidistat is used and there is a moisture load, check if the latent PLR is greater than the (sensible) PLR
!        IF(LatentPartLoadRatio .GT. PartLoadRatio .and. SystemMoistureLoad .lt. 0.0 .and. Furnace(FurnaceNum)%Humidistat) THEN
        IF(LatentPartLoadRatio .GT. PartLoadRatio .and. Furnace(FurnaceNum)%Humidistat) THEN
!         For dehumidification mode CoolReheat, compare the Sensible and Latent PLR values, if latentPLR is greater
!         than PLR (sensible), then overcooling is required and reheat will be activated using the HumControl flag.
          IF(Furnace(FurnaceNum)%DehumidControlType_Num .EQ. DehumidControl_CoolReheat)THEN
            PartLoadRatio = LatentPartLoadRatio
            HumControl = .TRUE.
          END IF
!         For dehumidification mode MultiMode, compare the Sensible and Latent PLR values, if latentPLR is
!         greater than PLR (sensible), then use the latent PLR to control the unit.
!         For MultiMode control, the latent PLR is found by enabling the HX and calculating a PLR required to meet the sensible
!         load. Overcooling is not required, and reheat will not be activated using the HumControl flag.
          IF(Furnace(FurnaceNum)%DehumidControlType_Num .EQ. DehumidControl_Multimode)THEN
            PartLoadRatio = LatentPartLoadRatio
          END IF
        END IF

        Furnace(FurnaceNum)%CoolPartLoadRatio = PartLoadRatio
        IF (CompOp == Off) THEN
          Furnace(FurnaceNum)%CompPartLoadRatio = 0.0d0
        ELSE
          Furnace(FurnaceNum)%CompPartLoadRatio = PartLoadRatio
        END IF


      ELSE  !ELSE from IF(FullSensibleOutput.LT.NoCoolOutput)THEN above
      ! CR8679 - Unitary Heat Cool control problem, will not run to meeting cooling load
      ! underlying problem is that FullSensibleOutput is greater than 0 due to very high inlet temp, so the system should be on
      ! NoCoolOutput was 0 since the defect file is a cycling fan system and the system was turned off

      ! if FullSensibleOutput > NoCoolOutput, it means the system cannot meet the load and will run full out
      ! this same logic for WSHP does not seem to work (only the Unitary Heat Pump Compressor Part-Load Ratio report
      ! variable was affected in the HeatPumpWaterToAirRHControl.idf file while other variables showed very small diffs).
      ! The defect files meter.csv showed 2% diffs so this IF test is used to keep the results the same in that file.
      ! Additional logic is used here to make sure the coil actually turned on, e.g., if DX coil PLR > 0 then set to 1,
      ! otherwise 0 (to make sure coil is actually ON and not off due to schedule, OAT, or other reason).
      ! The global variable DXCoilPartLoadRatio(DXCoilNum) is not yet used for the WSHP to make the same check.
        IF(Furnace(FurnaceNum)%FurnaceType_Num == UnitarySys_HeatPump_WaterToAir) THEN
          Furnace(FurnaceNum)%CoolPartLoadRatio = 0.0d0
          Furnace(FurnaceNum)%CompPartLoadRatio = 0.0d0
        ELSE
          IF(Furnace(FurnaceNum)%CoolingCoilType_Num == CoilDX_CoolingHXAssisted)THEN
            IF(DXCoilPartLoadRatio(Furnace(FurnaceNum)%ActualDXCoilIndexforHXAssisted) .GT. 0.d0)THEN
              Furnace(FurnaceNum)%CoolPartLoadRatio = 1.0d0
              Furnace(FurnaceNum)%CompPartLoadRatio = 1.0d0
            ELSE
              Furnace(FurnaceNum)%CoolPartLoadRatio = 0.0d0
              Furnace(FurnaceNum)%CompPartLoadRatio = 0.0d0
            END IF
          ELSE
            IF(DXCoilPartLoadRatio(Furnace(FurnaceNum)%CoolingCoilIndex) .GT. 0.d0)THEN
              Furnace(FurnaceNum)%CoolPartLoadRatio = 1.0d0
              Furnace(FurnaceNum)%CompPartLoadRatio = 1.0d0
            ELSE
              Furnace(FurnaceNum)%CoolPartLoadRatio = 0.0d0
              Furnace(FurnaceNum)%CompPartLoadRatio = 0.0d0
            END IF
          END IF
        END IF
      ENDIF

!     Calculate the reheat coil output
      IF(HumControl)THEN ! HumControl = .TRUE. if a Humidistat is installed and dehumdification control type is CoolReheat
        IF (Furnace(FurnaceNum)%ZoneSequenceHeatingNum > 0) THEN
          QToHeatSetPt=(ZoneSysEnergyDemand(Furnace(FurnaceNum)%ControlZoneNum)% &
                         SequencedOutputRequiredToHeatingSP(Furnace(FurnaceNum)%ZoneSequenceHeatingNum) / &
                         Furnace(FurnaceNum)%ControlZoneMassFlowFrac)
        ELSE
          QToHeatSetPt=(ZoneSysEnergyDemand(Furnace(FurnaceNum)%ControlZoneNum)%OutputRequiredToHeatingSP / &
                      Furnace(FurnaceNum)%ControlZoneMassFlowFrac)
        ENDIF
!       Cooling mode or floating condition and dehumidification is required
        IF(QToHeatSetPt .LT. 0.0d0)THEN
!         Calculate the reheat coil load wrt the heating setpoint temperature. Reheat coil picks up
!         the entire excess sensible cooling (DX cooling coil and impact of outdoor air).
          ReheatCoilLoad = MAX(0.0d0,(QToHeatSetPt-ActualSensibleOutput))
          Furnace(FurnaceNum)%DehumidInducedHeatingDemandRate = ReheatCoilLoad
!       Heating mode and dehumidification is required
        ELSEIF(QToHeatSetPt .GE. 0.0d0)THEN
!         Calculate the reheat coil load as the sensible capacity of the DX cooling coil only. Let
!         the heating coil pick up the load due to outdoor air.
          ReheatCoilLoad = MAX(0.0d0,(ActualSensibleOutput-NoCoolOutput)*(-1.0d0))
!         Dehumidification is not required
          IF (Furnace(FurnaceNum)%FurnaceType_Num == UnitarySys_HeatPump_AirToAir .OR. &
             (Furnace(FurnaceNum)%FurnaceType_Num == UnitarySys_HeatPump_WaterToAir .AND. &
              Furnace(FurnaceNum)%WatertoAirHPType == WatertoAir_Simple )) THEN
              ReheatCoilLoad = MAX(QToHeatSetPt,QToHeatSetPt-ActualSensibleOutput)
          ENDIF
          Furnace(FurnaceNum)%DehumidInducedHeatingDemandRate = MAX(0.0d0,ActualSensibleOutput*(-1.0d0))
        ELSE
          ReheatCoilLoad = 0.0d0
        END IF
      ELSE
!       No humidistat installed
        ReheatCoilLoad = 0.0d0
      END IF
    END IF !End of cooling section IF statement

    IF(NoHeatOutput .GT. SystemSensibleLoad .AND. ReHeatCoilLoad .GT. 0.0d0) THEN
    ! Reduce reheat coil load if you are controlling high humidity but outside air
    ! and/or the supply air fan is providing enough heat to meet the system sensible load.
    ! This will bring the zone temp closer to the heating setpoint temp.
      ReHeatCoilLoad = MAX(0.0d0,ReHeatCoilLoad-(NoHeatOutput-SystemSensibleLoad))
    END IF

    ! Set the final air flow. MdotFurnace will be used to set the fan part-load ratio in ReportFurnace
    IF(HumControl .AND. SystemMoistureLoad .LT. 0.0d0)THEN
      IF(OpMode .EQ. CycFanCycCoil)THEN
!       set the flow rate at the maximum of the cooling and heating PLR's
        CALL SetAverageAirFlow(FurnaceNum, &
          MAX(Furnace(FurnaceNum)%CoolPartLoadRatio,Furnace(FurnaceNum)%HeatPartLoadRatio), OnOffAirFlowRatio)
      ELSE
!       ELSE set the flow rate at the cooling PLR
        CALL SetAverageAirFlow(FurnaceNum, Furnace(FurnaceNum)%CoolPartLoadRatio, OnOffAirFlowRatio)
      END IF
    ELSE
      CALL SetAverageAirFlow(FurnaceNum, &
        MAX(Furnace(FurnaceNum)%CoolPartLoadRatio,Furnace(FurnaceNum)%HeatPartLoadRatio), OnOffAirFlowRatio)
    END IF
    Furnace(FurnaceNum)%MdotFurnace = Node(FurnaceInletNode)%MassFlowRate

    IF (Furnace(FurnaceNum)%FurnaceType_Num == UnitarySys_HeatPump_AirToAir .OR. &
       (Furnace(FurnaceNum)%FurnaceType_Num == UnitarySys_HeatPump_WatertoAir .AND. &
        Furnace(FurnaceNum)%WatertoAirHPType == WatertoAir_Simple))THEN
    ELSE
    ! Non-HeatPump (non-DX) heating coils do not set PLR, reset to 0 here. This variable was set for non-DX
    ! coils to allow the SetAverageAirFlow CALL above to set the correct air mass flow rate. See this
    ! IF block above in heating section. HeatPLR is not set in the ELSE part of the IF (only HeatCoilLoad is set).
      Furnace(FurnaceNum)%HeatPartLoadRatio = 0.0d0
    END IF

!*********HVAC Scheduled OFF*************
    ! No heating or cooling or dehumidification
!!!LKL discrepancy with < 0?
    IF(GetCurrentScheduleValue(Furnace(FurnaceNum)%SchedPtr) .eq. 0.0d0 .OR. &
       Node(FurnaceInletNode)%MassFlowRate .eq. 0.0d0) THEN
      Furnace(FurnaceNum)%MdotFurnace = 0.0d0
      CoolCoilLoad = 0.0d0
      HeatCoilLoad = 0.0d0
      ReheatCoilLoad = 0.0d0
      OnOffFanPartLoadFraction = 1.0d0 ! System off, so set on/off fan part-load fraction = 1
      Furnace(FurnaceNum)%CoolPartLoadRatio = 0.0d0
      Furnace(FurnaceNum)%HeatPartLoadRatio = 0.0d0
      Furnace(FurnaceNum)%CompPartLoadRatio = 0.0d0
    !set report variables
      Furnace(FurnaceNum)%CoolingCoilSensDemand=0.0d0
      Furnace(FurnaceNum)%CoolingCoilLatentDemand=0.0d0
      Furnace(FurnaceNum)%HeatingCoilSensDemand=0.0d0
    END IF


  END IF   ! End of the FirstHVACIteration control of the mass flow If block


  ! Set the fan inlet node flow rates
    Node(FurnaceInletNode)%MassFlowRateMaxAvail = Furnace(FurnaceNum)%MdotFurnace
    Node(FurnaceInletNode)%MassFlowRate         = Furnace(FurnaceNum)%MdotFurnace
RETURN
END Subroutine CalcNewZoneHeatCoolFlowRates




SUBROUTINE CalcWaterToAirHeatpump(AirLoopNum, FurnaceNum,FirstHVACIteration,CompOp,ZoneLoad,MoistureLoad)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Dan Fisher
          !       DATE WRITTEN   Feb 2004
          !       MODIFIED       R. Raustad (Oct 2006) Revised iteration technique
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine manages the heat pump simulation

          ! METHODOLOGY EMPLOYED:
          ! Calculate the part-load ratio required to meet the zone sensible load.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE HeatingCoils,       ONLY: SimulateHeatingCoilComponents
  USE InputProcessor,     ONLY: FindItemInList
  USE DataHeatBalFanSys,  ONLY: MAT
  USE DataAirLoop,        ONLY: AirToOANodeInfo
  USE General,            ONLY: SolveRegulaFalsi, TrimSigDigits

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER,   Intent(IN) :: FurnaceNum          ! index to Furnace
  INTEGER,   Intent(IN) :: AirLoopNum          ! index to air loop
  LOGICAL,   Intent(IN) :: FirstHVACIteration  ! TRUE on first HVAC iteration
  INTEGER,   Intent(IN) :: CompOp              ! compressor operation flag (1=On, 0=Off)
  REAL(r64), Intent(IN) :: ZoneLoad            ! the control zone load (watts)
  REAL(r64), Intent(IN) :: MoistureLoad        ! the control zone latent load (watts)


          ! SUBROUTINE PARAMETER DEFINITIONS:
  INTEGER,   PARAMETER    ::  MaxIter = 600    ! maximum number of iterations
  REAL(r64), PARAMETER    ::  MinPLR = 0.0d0     ! minimum part load ratio allowed

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64)               :: OnOffAirFlowRatio   ! Ratio of compressor ON air mass flow to AVERAGE air mass flow over time step
  REAL(r64), SAVE        :: TotalZoneLatentLoad  ! Total ZONE latent load (not including outside air)
                                            ! to be removed by furnace/unitary system
  REAL(r64), SAVE        :: TotalZoneSensLoad    ! Total ZONE heating load (not including outside air)
                                            ! to be removed by furnace/unitary system
  REAL(r64)         :: cpair                ! Heat capacity of air
  REAL(r64)         :: ZoneSensLoadMet      ! Actual zone sensible load met by heat pump (W)
  REAL(r64)         :: ZoneLatLoadMet       ! Actual zone latent load met by heat pump (W)
  REAL(r64)         :: ZoneSensLoadMetFanONCompON ! Max Zone sensible load heat pump can meet (W)
  REAL(r64)         :: ZoneLatLoadMetFanONCompON  !Max Zone latentload heat pump can meet (W)
  REAL(r64)         :: ZoneSensLoadMetFanONCompOFF ! control zone sensible load met using only outside air
                                                   ! and fan heat (no coil output) (W)
  REAL(r64)         :: ZoneLatLoadMetFanONCompOFF  ! control zone Latent   load met using only outside air
                                                   ! and fan heat (no coil output) (W)
  REAL(r64)         :: HPCoilSensDemand     ! Heat pump sensible demand
  REAL(r64)         :: HPCoilSensCapacity   ! Heat pump sensible capacity
  INTEGER           :: FurnaceInletNode     ! heat pump Inlet node
  INTEGER           :: FurnaceOutletNode    ! heat pump Outlet node

  INTEGER           :: OASysInletNode       ! node number of return air inlet to OA sys
  INTEGER           :: OASysOutletNode      ! node number of mixed air outlet of OA sys
  INTEGER           :: OpMode               ! Mode of Operation (fan cycling = 1 or fan continuous = 2)
  REAL(r64),SAVE         :: CoolPartLoadRatio    ! Part load ratio (greater of sensible or latent part load ratio for cooling)
  REAL(r64),SAVE         :: HeatPartLoadRatio    ! Part load ratio (greater of sensible or latent part load ratio for cooling)
  REAL(r64),SAVE         :: Dummy=0.0d0            ! Dummy var. for generic calc. furnace output arg. (n/a for heat pump)
  LOGICAL           :: HumControl           ! Logical flag signaling when dehumidification is required
  REAL(r64)         :: SuppHeatCoilLoad     ! Load passed to supplemental heater (W)
  REAL(r64)  :: CoolErrorToler       ! convergence tolerance used in cooling mode
  REAL(r64)  :: HeatErrorToler       ! convergence tolerance used in heating mode
  INTEGER           :: SolFlag              ! flag returned from iteration routine to denote problems
  REAL(r64)         :: Par(9)               ! parameters passed to iteration routine

  ! Set local variables
  Dummy = 0.0d0
  OnOffAirFlowRatio = 1.0d0
  FurnaceOutletNode = Furnace(FurnaceNum)%FurnaceOutletNodeNum
  FurnaceInletNode  = Furnace(FurnaceNum)%FurnaceInletNodeNum
  IF(AirToOANodeInfo(AirLoopNum)%OASysExists)THEN
    OASysOutletNode = AirToOANodeInfo(AirLoopNum)%OASysOutletNodeNum
    OASysInletNode = AirToOANodeInfo(AirLoopNum)%OASysInletNodeNum
  ENDIF
  OpMode = Furnace(FurnaceNum)%OpMode
  Furnace(FurnaceNum)%MdotFurnace = Furnace(FurnaceNum)%DesignMassFlowRate
  HumControl = .FALSE.


!*********INITIAL CALCULATIONS****************
  !Calculate the Cp Air for all conditions
  cpair = PsyCpAirFnWTdb(Node(FurnaceInletNode)%HumRat,Node(FurnaceInletNode)%Temp)

  !set the fan part load fraction
  ! Note: OnOffFanPartLoadFraction is passed to the
  !       fan module by DataHVACGlobals.  It should be
  !     set =1 for all cases except cycling fan/cycling
  !     coil. For this case it is set to the part load
  !     factor.  In SimOnOffFan, the part load ratio is
  !     divided by the part load factor (OnOffFanPartLoadFraction)
  !     in order to match the run time fraction of the cycling
  !     fan with the run time fraction of the cycling compressor
 IF(FirstHVACIteration) OnOffFanPartLoadFraction = 1.0d0

 !Calc Zone sensible loads for heating (+) and cooling (-)
 TotalZoneSensLoad = ZoneLoad

 !Set latent load for heating
 IF(HeatingLoad)THEN
   TotalZoneLatentLoad = 0.0d0

 !Set latent load for cooling and no sensible load condition
 ELSE
   TotalZoneLatentLoad = MoistureLoad
 ENDIF

!*********COOLING CALCULATIONS****************
            !IF scheduled on...
    IF((GetCurrentScheduleValue(Furnace(FurnaceNum)%SchedPtr) .gt. 0.0d0                    .AND. &
            !AND air flow rate is greater than zero...
        Node(FurnaceInletNode)%MassFlowRate .gt. 0.0d0)                                     .AND. &
            !AND the air system has a cooling load and is not set back or in the deadband...
       ((CoolingLoad) .OR.  &
            !OR the system is controlled by a humidistat and there is a latent load
       (Furnace(FurnaceNum)%Humidistat.AND.Furnace(FurnaceNum)%CoolingCoilLatentDemand.lt.0.0d0))) THEN

         !Set the air flow rate to the design flow rate and set the fan operation fraction to 1 (continuous operation)
         Node(FurnaceInletNode)%MassFlowRate=Furnace(FurnaceNum)%DesignMassFlowRate
         OnOffFanPartLoadFraction = 1.0d0 !see 'Note' under INITIAL CALCULATIONS

!         !Set the operation flag to run the fan continuously
!         OpMode = ContFanCycCoil

         !Set the input parameters for CalcFurnaceOutput
             Furnace(FurnaceNum)%HeatingCoilSensDemand   = 0.0d0
             Furnace(FurnaceNum)%CoolingCoilLatentDemand = 0.0d0
             Furnace(FurnaceNum)%CoolingCoilSensDemand   = 0.0d0
             Furnace(FurnaceNum)%CompPartLoadRatio = 0.0d0    !compressor off
             Furnace(FurnaceNum)%InitHeatPump = .TRUE.      !initialization call to Calc Furnace
             Furnace(FurnaceNum)%WSHPRuntimeFrac = 0.0d0
             CoolPartLoadRatio = 0.0d0

        !Get no load result in order to calculate the effect of the fan and the mixed air equipment
         CALL CalcFurnaceOutput(FurnaceNum,FirstHVACIteration,OpMode,CompOp,CoolPartLoadRatio,HeatPartLoadRatio, &
                 Dummy, Dummy, ZoneSensLoadMetFanONCompOFF, ZoneLatLoadMetFanONCompOFF, OnOffAirFlowRatio, .FALSE.)


        !Set the input parameters for CalcFurnaceOutput
             Furnace(FurnaceNum)%CoolingCoilSensDemand   = 1.0d0
             Furnace(FurnaceNum)%CompPartLoadRatio = 1.0d0    !compressor ON
             Furnace(FurnaceNum)%WSHPRuntimeFrac   = 1.0d0
             CoolPartLoadRatio = 1.0d0

        !Get full load result in order to estimate the operating part load ratio for continuous fan operation
         CALL CalcFurnaceOutput(FurnaceNum,FirstHVACIteration,OpMode,CompOp,CoolPartLoadRatio,HeatPartLoadRatio, &
                     Dummy, Dummy, ZoneSensLoadMetFanONCompON, ZoneLatLoadMetFanONCompON, OnOffAirFlowRatio, .FALSE.)

        !Calculate the heating coil demand for continuous fan operation as:
        !    (the zone sensible load - the zone sensible load met by fan heat and mixed air)
        !Note; The sensible zone load met by fan heat and mixed air is calculated as:
        !     mdotsys(control zone inlet enthalpy - control zone outlet enthalpy)
        !This accounts for the negative sign in the equation.
         HPCoilSensDemand   = TotalZoneSensLoad  - ZoneSensLoadMetFanONCompOFF


        !Calculate the heating coil capacity for continuous fan operation as:
        !    (the zone sensible load met by fan heat and mixed air and coil
        !   - the zone sensible load met by fan heat and mixed air)
        HPCoilSensCapacity = ZoneSensLoadMetFanONCompON - ZoneSensLoadMetFanONCompOFF

        !Calculate the part load ratio for continuous fan operation with cycling coil
        IF(HPCoilSensCapacity == 0.0d0) Then
           CoolPartLoadRatio = 0.0d0
        Else
           CoolPartLoadRatio = MAX(MinPLR,MIN(1.0d0,&
                   ABS(HPCoilSensDemand) / ABS(HPCoilSensCapacity)))
        End If

        Furnace(FurnaceNum)%InitHeatPump = .FALSE.

!       check bounds on sensible output prior to iteration using RegulaFalsi
        IF(ZoneSensLoadMetFanONCompON .GT. TotalZoneSensLoad)THEN
          CoolPartLoadRatio = 1.0d0
          HPCoilSensDemand = ABS(ZoneSensLoadMetFanONCompON - ZoneSensLoadMetFanONCompOFF)
          Furnace(FurnaceNum)%CoolingCoilSensDemand = HPCoilSensDemand
        ELSEIF(ZoneSensLoadMetFanONCompOFF .LT. TotalZoneSensLoad)THEN
          CoolPartLoadRatio = 0.0d0
          Furnace(FurnaceNum)%CompPartLoadRatio = 0.0d0    !compressor OFF
          Furnace(FurnaceNum)%WSHPRuntimeFrac   = 0.0d0
          Furnace(FurnaceNum)%CoolingCoilSensDemand = 0.0d0
          CALL CalcFurnaceOutput(FurnaceNum,FirstHVACIteration,OpMode,CompOp,CoolPartLoadRatio,HeatPartLoadRatio, &
               Dummy, Dummy, ZoneSensLoadMetFanONCompOFF, ZoneLatLoadMetFanONCompOFF, OnOffAirFlowRatio, .FALSE.)
        ELSE
!         Calculate the sensible part load ratio through iteration
          CoolErrorToler = Furnace(FurnaceNum)%CoolingConvergenceTolerance
          SolFlag = 0    ! # of iterations if positive, -1 means failed to converge, -2 means bounds are incorrect
          Par(1)  = REAL(FurnaceNum,r64)
          Par(2)  = 0.0d0  ! FLAG, if 1.0 then FirstHVACIteration equals TRUE, if 0.0 then FirstHVACIteration equals false
          IF(FirstHVACIteration)Par(2)=1.0d0
          Par(3)  = REAL(OpMode,r64)
          Par(4)  = REAL(CompOp,r64)
          Par(5)  = TotalZoneSensLoad
          Par(6)  = 1.0d0 ! FLAG, 0.0 if heating load, 1.0 if cooling or moisture load
          Par(7)  = 1.0d0 ! FLAG, 0.0 if latent load, 1.0 if sensible load to be met
          Par(8)  = ZoneSensLoadMetFanONCompOFF ! Output with fan ON compressor OFF
          Par(9)  = 0.0d0 ! HX is off for water-to-air HP
!         CoolErrorToler is in fraction of load, MaxIter = 600, SolFalg = # of iterations or error as appropriate
          CALL SolveRegulaFalsi(CoolErrorToler, MaxIter, SolFlag, CoolPartLoadRatio, CalcWaterToAirResidual, 0.0d0, 1.0d0, Par)
          IF (SolFlag == -1 .AND. .NOT. WarmupFlag .AND. .NOT. FirstHVACIteration) THEN
            OnOffFanPartLoadFraction = OnOffFanPartLoadFractionSave
            CALL CalcFurnaceOutput(FurnaceNum,FirstHVACIteration,OpMode,CompOp,CoolPartLoadRatio, &
                                   0.0d0,0.0d0,0.0d0, ZoneSensLoadMet, ZoneLatLoadMet, OnOffAirFlowRatio, .FALSE.)
            IF(ABS(ZoneSensLoadMet - TotalZoneSensLoad)/TotalZoneSensLoad .GT. CoolErrorToler)THEN
              IF(Furnace(FurnaceNum)%SensibleMaxIterIndex == 0)THEN
                CALL ShowWarningMessage('Cooling coil control failed to converge for ' &
                    //TRIM(cFurnaceTypes(Furnace(FurnaceNum)%FurnaceType_Num))//':'//TRIM(Furnace(FurnaceNum)%Name))
                CALL ShowContinueError('  Iteration limit exceeded in calculating DX cooling coil sensible part-load ratio.')
                CALL ShowContinueErrorTimeStamp('Sensible load to be met by DX coil = ' &
                     //TRIM(TrimSigDigits(TotalZoneSensLoad,2))//' (watts), sensible output of DX coil = ' &
                     //TRIM(TrimSigDigits(ZoneSensLoadMet,2))//' (watts), and the simulation continues.')
              END IF
              CALL ShowRecurringWarningErrorAtEnd(TRIM(cFurnaceTypes(Furnace(FurnaceNum)%FurnaceType_Num))//' "'&
                  //TRIM(Furnace(FurnaceNum)%Name)//'" - Iteration limit exceeded in calculating'// &
                  ' sensible cooling part-load ratio error continues. Sensible load statistics:' &
                  ,Furnace(FurnaceNum)%SensibleMaxIterIndex,TotalZoneSensLoad,TotalZoneSensLoad)
            END IF
          ELSE IF (SolFlag == -2 .AND. .NOT. WarmupFlag .AND. .NOT. FirstHVACIteration) THEN
            CoolPartLoadRatio = MAX(MinPLR,MIN(1.0d0,ABS(HPCoilSensDemand ) / ABS(HPCoilSensCapacity)))
            OnOffFanPartLoadFraction = 1.0d0
            CALL CalcFurnaceOutput(FurnaceNum,FirstHVACIteration,OpMode,CompOp,CoolPartLoadRatio, &
                                   0.0d0,0.0d0,0.0d0, ZoneSensLoadMet, ZoneLatLoadMet, OnOffAirFlowRatio, .FALSE.)
            IF((ZoneSensLoadMet - TotalZoneSensLoad)/TotalZoneSensLoad .GT. CoolErrorToler)THEN
              IF(Furnace(FurnaceNum)%SensibleRegulaFalsiFailedIndex == 0)THEN
                CALL ShowWarningMessage('Cooling coil control failed for ' &
                     //TRIM(cFurnaceTypes(Furnace(FurnaceNum)%FurnaceType_Num))//':'//TRIM(Furnace(FurnaceNum)%Name))
                CALL ShowContinueError('  Cooling sensible part-load ratio determined to be outside the range of 0-1.')
                CALL ShowContinueError('  An estimated part-load ratio = '//TRIM(TrimSigDigits(CoolPartLoadRatio,2))// &
                                       ' will be used and the simulation continues.')
                CALL ShowContinueError('  The estimated part-load ratio provides a cooling sensible capacity = '// &
                                       TRIM(TrimSigDigits(ZoneSensLoadMet,2)))
                CALL ShowContinueErrorTimeStamp('  Cooling sensible load required = ' &
                                                //TRIM(TrimSigDigits(TotalZoneSensLoad,2)))
              ENDIF
              CALL ShowRecurringWarningErrorAtEnd(TRIM(cFurnaceTypes(Furnace(FurnaceNum)%FurnaceType_Num))//' "'&
                  //TRIM(Furnace(FurnaceNum)%Name)//'" - Cooling sensible part-load ratio out of range'// &
                  ' error continues. Sensible cooling load statistics:' &
                  ,Furnace(FurnaceNum)%SensibleRegulaFalsiFailedIndex,TotalZoneSensLoad,TotalZoneSensLoad)
            END IF
          END IF
       END IF

       IF (OpMode .EQ. CycFanCycCoil) THEN
         Furnace(FurnaceNum)%MdotFurnace = Furnace(FurnaceNum)%MdotFurnace * CoolPartLoadRatio
       END IF


!*********HEATING CALCULATIONS****************
    ! If Furnace runs with a heating load then set HeatCoilLoad on Heating Coil and the Mass Flow
    ELSEIF((GetCurrentScheduleValue(Furnace(FurnaceNum)%SchedPtr) .gt. 0.0d0) .and. &
          (Node(FurnaceInletNode)%MassFlowRate .gt. 0.0d0) .and. &
          HeatingLoad) THEN

         !Set the air flow rate to the design flow rate and set the fan operation fraction to 1 (continuous operation)
         Node(FurnaceInletNode)%MassFlowRate=Furnace(FurnaceNum)%DesignMassFlowRate
         OnOffFanPartLoadFraction = 1.0d0 !see 'Note' under INITIAL CALCULATIONS

!         !Set the operation flag to run the fan continuously
!         OpMode = ContFanCycCoil

         !Set the input parameters for CalcFurnaceOutput
             Furnace(FurnaceNum)%HeatingCoilSensDemand   = 0.0d0
             Furnace(FurnaceNum)%CoolingCoilLatentDemand = 0.0d0
             Furnace(FurnaceNum)%CoolingCoilSensDemand   = 0.0d0
             Furnace(FurnaceNum)%CompPartLoadRatio = 0.0d0    !compressor off
             Furnace(FurnaceNum)%InitHeatPump = .TRUE.      !initialization call to Calc Furnace
             Furnace(FurnaceNum)%WSHPRuntimeFrac = 0.0d0
             HeatPartLoadRatio = 0.0d0

        !Get no load result in order to calculate the effect of the fan and the mixed air equipment
         CALL CalcFurnaceOutput(FurnaceNum,FirstHVACIteration,OpMode,CompOp,CoolPartLoadRatio,HeatPartLoadRatio, &
                 Dummy, Dummy, ZoneSensLoadMetFanONCompOFF, ZoneLatLoadMetFanONCompOFF, OnOffAirFlowRatio, .FALSE.)


        !Set the input parameters for CalcFurnaceOutput
             Furnace(FurnaceNum)%HeatingCoilSensDemand   = 1.0d0
             Furnace(FurnaceNum)%CompPartLoadRatio = 1.0d0    !compressor ON
             Furnace(FurnaceNum)%WSHPRuntimeFrac   = 1.0d0
             HeatPartLoadRatio = 1.0d0

        !Get full load result in order to estimate the operating part load ratio for continuous fan operation

         CALL CalcFurnaceOutput(FurnaceNum,FirstHVACIteration,OpMode,CompOp,CoolPartLoadRatio,HeatPartLoadRatio, &
                     Dummy, Dummy, ZoneSensLoadMetFanONCompON, ZoneLatLoadMetFanONCompON, OnOffAirFlowRatio, .FALSE.)

        !Calculate the heating coil demand for continuous fan operation as:
        !    (the zone sensible load - the zone sensible load met by fan heat and mixed air)
        !Note; The sensible zone load met by fan heat and mixed air is calculated as:
        !     mdotsys(control zone inlet enthalpy - control zone outlet enthalpy)
        !This accounts for the negative sign in the equation.
         HPCoilSensDemand   = TotalZoneSensLoad  - ZoneSensLoadMetFanONCompOFF


        !Calculate the heating coil capacity for continuous fan operation as:
        !    (the zone sensible load met by fan heat and mixed air and coil
        !   - the zone sensible load met by fan heat and mixed air)
        HPCoilSensCapacity = ZoneSensLoadMetFanONCompON - ZoneSensLoadMetFanONCompOFF

        !Calculate the part load ratio for continuous fan operation with cycling coil
        If(HPCoilSensCapacity == 0.0d0) Then
           HeatPartLoadRatio = 0.0d0
        Else
           HeatPartLoadRatio = MAX(MinPLR,MIN(1.0d0,&
                                           ABS(HPCoilSensDemand) / ABS(HPCoilSensCapacity)))
        End If

        Furnace(FurnaceNum)%InitHeatPump = .FALSE.

!       check bounds on sensible output prior to iteration using RegulaFalsi
        IF(ZoneSensLoadMetFanONCompON .LT. TotalZoneSensLoad)THEN
          HeatPartLoadRatio = 1.0d0
          ZoneSensLoadMet = ZoneSensLoadMetFanONCompON
          HPCoilSensDemand = ABS(ZoneSensLoadMetFanONCompON - ZoneSensLoadMetFanONCompOFF)
          Furnace(FurnaceNum)%HeatingCoilSensDemand = HPCoilSensDemand
        ELSEIF(ZoneSensLoadMetFanONCompOFF .GT. TotalZoneSensLoad)THEN
          HeatPartLoadRatio = 0.0d0
          ZoneSensLoadMet = ZoneSensLoadMetFanONCompOFF
          Furnace(FurnaceNum)%CompPartLoadRatio = 0.0d0    !compressor ON
          Furnace(FurnaceNum)%WSHPRuntimeFrac   = 0.0d0
          CALL CalcFurnaceOutput(FurnaceNum,FirstHVACIteration,OpMode,CompOp,CoolPartLoadRatio,&
                 HeatPartLoadRatio, Dummy, Dummy, ZoneSensLoadMet,ZoneLatLoadMet, OnOffAirFlowRatio, .FALSE.)
         ELSE
!         Calculate the sensible part load ratio through iteration
          HeatErrorToler = Furnace(FurnaceNum)%HeatingConvergenceTolerance
          SolFlag = 0    ! # of iterations if positive, -1 means failed to converge, -2 means bounds are incorrect
          Par(1)  = REAL(FurnaceNum,r64)
          Par(2)  = 0.0d0  ! FLAG, if 1.0 then FirstHVACIteration equals TRUE, if 0.0 then FirstHVACIteration equals false
          IF(FirstHVACIteration)Par(2)=1.0d0
          Par(3)  = REAL(OpMode,r64)
          Par(4)  = REAL(CompOp,r64)
          Par(5)  = TotalZoneSensLoad
          Par(6)  = 0.0d0 ! FLAG, 0.0 if heating load, 1.0 if cooling or moisture load
          Par(7)  = 1.0d0 ! FLAG, 0.0 if latent load, 1.0 if sensible load to be met
          Par(8)  = ZoneSensLoadMetFanONCompOFF ! Output with fan ON compressor OFF
          Par(9)  = 0.0d0 ! HX is OFF for water-to-air HP
!         HeatErrorToler is in fraction of load, MaxIter = 600, SolFalg = # of iterations or error as appropriate
          CALL SolveRegulaFalsi(HeatErrorToler, MaxIter, SolFlag, HeatPartLoadRatio, CalcWaterToAirResidual, 0.0d0, 1.0d0, Par)
          OnOffFanPartLoadFraction = OnOffFanPartLoadFractionSave
          CALL CalcFurnaceOutput(FurnaceNum,FirstHVACIteration,OpMode,CompOp,CoolPartLoadRatio,HeatPartLoadRatio, &
                                 Dummy, Dummy, ZoneSensLoadMet,ZoneLatLoadMet, OnOffAirFlowRatio, .FALSE.)
          IF (SolFlag == -1 .AND. .NOT. WarmupFlag .AND. .NOT. FirstHVACIteration) THEN
            IF(ABS(ZoneSensLoadMet - TotalZoneSensLoad)/TotalZoneSensLoad .GT. HeatErrorToler)THEN
              IF(Furnace(FurnaceNum)%WSHPHeatMaxIterIndex == 0)THEN
                CALL ShowWarningMessage('Heating coil control failed to converge for ' &
                    //TRIM(cFurnaceTypes(Furnace(FurnaceNum)%FurnaceType_Num))//':'//TRIM(Furnace(FurnaceNum)%Name))
                CALL ShowContinueError('  Iteration limit exceeded in calculating DX heating coil sensible part-load ratio.')
                CALL ShowContinueErrorTimeStamp('Sensible load to be met by DX coil = ' &
                     //TRIM(TrimSigDigits(TotalZoneSensLoad,2))//' (watts), sensible output of DX coil = ' &
                     //TRIM(TrimSigDigits(ZoneSensLoadMet,2))//' (watts), and the simulation continues.')
              END IF
              CALL ShowRecurringWarningErrorAtEnd(TRIM(cFurnaceTypes(Furnace(FurnaceNum)%FurnaceType_Num))//' "'&
                  //TRIM(Furnace(FurnaceNum)%Name)//'" - Iteration limit exceeded in calculating'// &
                  ' sensible heating part-load ratio error continues.' &
                  ,Furnace(FurnaceNum)%WSHPHeatMaxIterIndex,TotalZoneSensLoad,TotalZoneSensLoad)
            END IF
          ELSE IF (SolFlag == -2) THEN
            HeatPartLoadRatio = MAX(MinPLR,MIN(1.0d0,ABS(HPCoilSensDemand ) / ABS(HPCoilSensCapacity)))
            CALL CalcFurnaceOutput(FurnaceNum,FirstHVACIteration,OpMode,CompOp,0.0d0,HeatPartLoadRatio, &
                                   0.0d0,0.0d0, ZoneSensLoadMet,ZoneLatLoadMet, OnOffAirFlowRatio, .FALSE.)
            IF((ZoneSensLoadMet - TotalZoneSensLoad)/TotalZoneSensLoad .GT. HeatErrorToler)THEN
              IF(Furnace(FurnaceNum)%WSHPHeatRegulaFalsiFailedIndex == 0)THEN
                CALL ShowWarningError('Heating coil control failed for ' &
                    //TRIM(cFurnaceTypes(Furnace(FurnaceNum)%FurnaceType_Num))//':'//TRIM(Furnace(FurnaceNum)%Name))
                CALL ShowContinueError('  Heating sensible part-load ratio determined to be outside the range of 0-1.')
                CALL ShowContinueError('  An estimated part-load ratio = '//TRIM(TrimSigDigits(HeatPartLoadRatio,2))// &
                                       ' will be used and the simulation continues.')
                CALL ShowContinueError('  The estimated part-load ratio provides a heating sensible capacity = '// &
                                       TRIM(TrimSigDigits(ZoneSensLoadMet,2)))
                CALL ShowContinueErrorTimeStamp('  Heating sensible load required = '//TRIM(TrimSigDigits(TotalZoneSensLoad,2)))
              END IF
              CALL ShowRecurringWarningErrorAtEnd(TRIM(cFurnaceTypes(Furnace(FurnaceNum)%FurnaceType_Num))//' "'&
                  //TRIM(Furnace(FurnaceNum)%Name)//'" - Heating sensible part-load ratio out of range'// &
                  ' error continues.' &
                  ,Furnace(FurnaceNum)%WSHPHeatRegulaFalsiFailedIndex,TotalZoneSensLoad,TotalZoneSensLoad)
            END IF
          END IF
        END IF

!       CALL supplemental heater if required
        IF((TotalZoneSensLoad - ZoneSensLoadMet) .GT. SmallLoad .AND. HeatPartLoadRatio .GE. 1.0d0)THEN
          SuppHeatCoilLoad = TotalZoneSensLoad - ZoneSensLoadMet
          CALL CalcFurnaceOutput(FurnaceNum,FirstHVACIteration,OpMode,CompOp,CoolPartLoadRatio,&
               HeatPartLoadRatio, SuppHeatCoilLoad, Dummy, ZoneSensLoadMet,ZoneLatLoadMet, OnOffAirFlowRatio, .FALSE.)
        END IF

        IF (OpMode .EQ. CycFanCycCoil) THEN
          Furnace(FurnaceNum)%MdotFurnace = Furnace(FurnaceNum)%MdotFurnace * HeatPartLoadRatio
        END IF

!**********HVAC Scheduled ON, but no cooling, dehumidification or heating load*********
    ELSEIF(GetCurrentScheduleValue(Furnace(FurnaceNum)%SchedPtr) .gt. 0.0d0) THEN
      Furnace(FurnaceNum)%InitHeatPump = .TRUE.      !initialization call to Calc Furnace
      HeatPartLoadRatio  = 0.0d0
      CoolPartLoadRatio = 0.0d0
      OnOffFanPartLoadFraction = 1.0d0 !!see 'Note' under INITIAL CALCULATIONS
    !set report variables
      Furnace(FurnaceNum)%CompPartLoadRatio = 0.0d0
      Furnace(FurnaceNum)%CoolingCoilSensDemand=0.0d0
      Furnace(FurnaceNum)%CoolingCoilLatentDemand=0.0d0
      Furnace(FurnaceNum)%HeatingCoilSensDemand=0.0d0
      IF(OpMode .EQ. CycFanCycCoil)THEN
        Furnace(FurnaceNum)%MdotFurnace = 0.0d0
        OnOffFanPartLoadFraction        = 1.0d0 !see 'Note' under INITIAL CALCULATIONS
        CALL CalcFurnaceOutput(FurnaceNum,FirstHVACIteration,OpMode,CompOp,CoolPartLoadRatio,&
         HeatPartLoadRatio, Dummy, Dummy, ZoneSensLoadMet,ZoneLatLoadMet, OnOffAirFlowRatio, .FALSE.)
         Furnace(FurnaceNum)%MdotFurnace = 0.0d0
      ElSE !continuous fan, cycling coil
        CALL CalcFurnaceOutput(FurnaceNum,FirstHVACIteration,OpMode,CompOp,CoolPartLoadRatio,&
         HeatPartLoadRatio, Dummy, Dummy, ZoneSensLoadMet,ZoneLatLoadMet, OnOffAirFlowRatio, .FALSE.)
      ENDIF
!*********No heating or cooling or dehumidification*********
    ELSE
      Furnace(FurnaceNum)%InitHeatPump = .TRUE.      !initialization call to Calc Furnace
      Furnace(FurnaceNum)%MdotFurnace = 0.0d0
      HeatPartLoadRatio   = 0.0d0
      CoolPartLoadRatio   = 0.0d0
      OnOffFanPartLoadFraction = 1.0d0 !see 'Note' under INITIAL CALCULATIONS
      Furnace(FurnaceNum)%CompPartLoadRatio = 0.0d0
      Furnace(FurnaceNum)%CoolingCoilSensDemand  = 0.0d0
      Furnace(FurnaceNum)%CoolingCoilLatentDemand= 0.0d0
      Furnace(FurnaceNum)%HeatingCoilSensDemand  = 0.0d0
      CALL CalcFurnaceOutput(FurnaceNum,FirstHVACIteration,OpMode,CompOp,CoolPartLoadRatio,&
         HeatPartLoadRatio, Dummy, Dummy, ZoneSensLoadMet,ZoneLatLoadMet, OnOffAirFlowRatio, .FALSE.)
      Furnace(FurnaceNum)%MdotFurnace = 0.0d0
  END IF


  ! Set the fan inlet node flow rates
    Node(FurnaceInletNode)%MassFlowRateMaxAvail = Furnace(FurnaceNum)%MdotFurnace
    Node(FurnaceInletNode)%MassFlowRate         = Furnace(FurnaceNum)%MdotFurnace
RETURN
END SUBROUTINE CalcWaterToAirHeatpump

SUBROUTINE CalcFurnaceOutput(FurnaceNum,FirstHVACIteration,FanOpMode,CompOp,CoolPartLoadRatio,HeatPartLoadRatio, &
                     HeatCoilLoad, ReHeatCoilLoad, SensibleLoadMet, LatentLoadMet, OnOffAirFlowRatio, HXUnitOn, &
                     CoolingHeatingPLRRat)


          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Raustad
          !       DATE WRITTEN   Sept 2001
          !       MODIFIED       Dec 2001
          !       RE-ENGINEERED  na


          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine calculates to sensible and latent loads met by the DX coils
          ! specified.  Load met is the outlet node with respect to the control zone's
          ! temperature and humidity ratio.


          ! METHODOLOGY EMPLOYED:
          ! Simulate each child object in the correct order for each system type. This routine is used in the
          ! RegulaFalsi function CALL. Air mass flow rate is set each iteration based on PLR.


          ! REFERENCES:
          ! na


          ! USE STATEMENTS:
  USE HeatingCoils,              ONLY: SimulateHeatingCoilComponents
  USE WatertoAirHeatPump,        ONLY: SimWaterToAirHP
  USE WatertoAirheatPumpSimple,  ONLY: SimWatertoAirHPSimple
  USE HVACHXAssistedCoolingCoil, ONLY: SimHXAssistedCoolingCoil

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine


          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER,   Intent(IN)    :: FurnaceNum
  LOGICAL,   Intent(IN)    :: FirstHVACIteration
  INTEGER,   Intent(IN)    :: FanOpMode          ! Cycling fan or constant fan
  INTEGER,   Intent(IN)    :: CompOp             ! Compressor on/off; 1=on, 0=off
  REAL(r64), Intent(IN)    :: CoolPartLoadRatio  ! DX cooling coil part load ratio
  REAL(r64), Intent(IN)    :: HeatPartLoadRatio  ! DX heating coil part load ratio (0 for other heating coil types)
  REAL(r64), Intent(IN)    :: HeatCoilLoad       ! Heating coil load for gas heater
  REAL(r64), Intent(IN)    :: ReHeatCoilLoad     ! Reheating coil load for gas heater
  REAL(r64), Intent(OUT)   :: SensibleLoadMet    ! Sensible cooling load met (furnace outlet with respect to control zone temp)
  REAL(r64), Intent(OUT)   :: LatentLoadMet ! Latent cooling load met (furnace outlet with respect to control zone humidity ratio)
  REAL(r64), Intent(INOUT) :: OnOffAirFlowRatio  ! Ratio of compressor ON mass flow rate to AVERAGE
  LOGICAL,   Intent(IN)    :: HXUnitOn           ! flag to enable HX based on zone moisture load
  REAL(r64), Intent(IN), OPTIONAL  :: CoolingHeatingPLRRat  ! cooling PLR to heating PLR ratio, used for cycling fan RH control


          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na


          ! INTERFACE BLOCK SPECIFICATIONS
          ! na


          ! DERIVED TYPE DEFINITIONS
          ! na


          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER          :: FurnaceInletNode    ! Furnace inlet node number
  INTEGER          :: FurnaceOutletNode   ! Furnace outlet node number
  REAL(r64)        :: AirMassFlow         ! Furnace inlet node temperature
  REAL(r64)        :: WSHPRuntimeFrac     ! Compressor runtime fraction
  REAL(r64)        :: CompPartLoadRatio   ! Compressor part load ratio
  REAL(r64)        :: MinHumRatio         ! Minimum humidity ratio for calculating sensible load at a constant humidity ratio
  REAL(r64)        :: MaxTemp             ! Maximum temperature for calculating latent load at a constant temperature
  REAL(r64)        :: dummy               ! dummy variable
  REAL(r64)        :: Tout                ! Temporary variable used when outlet temp > DesignMaxOutletTemp
  REAL(r64)        :: Wout                ! Temporary variable used when outlet temp > DesignMaxOutletTemp
  INTEGER          :: CoolingCoilType_Num ! Numeric Equivalent for CoolingCoilType
  INTEGER          :: HeatingCoilType_Num ! Numeric Equivalent for HeatingCoilType
  REAL(r64)        :: mdot                ! hot water or steam heating coil fluid mass flow rates
  REAL(r64)        :: QCoilReq            ! heating coil load
  REAL(r64)        :: QActual             ! heating coil load met or delivered
  REAL(r64)        :: MinWaterFlow   = 0.0d0  ! minimum fluid flow rates
  INTEGER          :: LoopNumber          ! plant loop index for water and steam supplemental heating coil
  INTEGER          :: LoopSideNumber      ! plant loop side  index for  water and steam supp. heating coil
  INTEGER          :: BranchNumber        ! plant loop branch index for water and steam supp. heating coil
  INTEGER          :: CompNumber          ! plant loop comp. index for water and steam supp. heating coil
  LOGICAL          :: SuppHeatingCoilFlag ! .true. if supplemental heating coil

  FurnaceOutletNode     = Furnace(FurnaceNum)%FurnaceOutletNodeNum
  FurnaceInletNode      = Furnace(FurnaceNum)%FurnaceInletNodeNum
  CoolingCoilType_Num   = Furnace(FurnaceNum)%CoolingCoilType_Num
  HeatingCoilType_Num   = Furnace(FurnaceNum)%HeatingCoilType_Num
  WSHPRuntimeFrac       = Furnace(FurnaceNum)%WSHPRuntimeFrac
  CompPartLoadRatio     = Furnace(FurnaceNum)%CompPartLoadRatio
  ModifiedHeatCoilLoad  = 0.0d0

  IF (PRESENT(CoolingHeatingPLRRat)) THEN
    CoolHeatPLRRat = CoolingHeatingPLRRat
  ELSE
    CoolHeatPLRRat = 1.0d0
  END IF

! Cooling to Heating PLR Ratio (CoolHeatPLRRat) is used to track the air mass flow rate of both the heating
! and cooling coils when RH control is used and the heating coil operates longer than the cooling coil.
! When CoolPartLoadRatio/CoolHeatPLRRat is used, the PLR calculated is acutally the PLR for the heating
! coil (heating PLR is greater than cooling PLR), it is this PLR that determines the air mass flow rate.
!
! When MAX(HeatPartLoadRatio,CoolPartLoadRatio) is used, only one of these values is non-zero.
!
  IF (FanOpMode.EQ.CycFanCycCoil) THEN
    IF(CoolHeatPLRRat .LT. 1.0d0) THEN
      IF(CoolHeatPLRRat .GT. 0.0d0) THEN
        Node(FurnaceInletNode)%MassFlowRate = CompOnMassFlow * CoolPartLoadRatio/CoolHeatPLRRat
        IF(Furnace(FurnaceNum)%FurnaceType_Num .NE. UnitarySys_HeatPump_WaterToAir)THEN
          CALL SetAverageAirFlow(FurnaceNum, CoolPartLoadRatio/CoolHeatPLRRat, OnOffAirFlowRatio)
        END IF
      ELSE
        Node(FurnaceInletNode)%MassFlowRate = CompOnMassFlow * CoolPartLoadRatio
        IF(Furnace(FurnaceNum)%FurnaceType_Num .NE. UnitarySys_HeatPump_WaterToAir)THEN
          CALL SetAverageAirFlow(FurnaceNum, MAX(HeatPartLoadRatio,CoolPartLoadRatio), OnOffAirFlowRatio)
        END IF
      END IF
    ELSE
      Node(FurnaceInletNode)%MassFlowRate = CompOnMassFlow * MAX(HeatPartLoadRatio,CoolPartLoadRatio)
      IF(Furnace(FurnaceNum)%FurnaceType_Num .NE. UnitarySys_HeatPump_WaterToAir)THEN
        CALL SetAverageAirFlow(FurnaceNum, MAX(HeatPartLoadRatio,CoolPartLoadRatio), OnOffAirFlowRatio)
      END IF
    END IF
  ELSE
    IF(Furnace(FurnaceNum)%FurnaceType_Num .NE. UnitarySys_HeatPump_WaterToAir)THEN
      CALL SetAverageAirFlow(FurnaceNum, MAX(HeatPartLoadRatio,CoolPartLoadRatio), OnOffAirFlowRatio)
    END IF
  END IF

  AirMassFlow = Node(FurnaceInletNode)%MassFlowRate
  Node(FurnaceInletNode)%MassFlowRateMaxAvail = AirMassFlow

! Simulate the air-to-air heat pump
  IF(Furnace(FurnaceNum)%FurnaceType_Num == UnitarySys_HeatPump_AirToAir)THEN
!   Simulate blow-thru fan and non-linear coils twice to update PLF used by the ONOFF Fan
    IF (Furnace(FurnaceNum)%FanPlace .EQ. BlowThru) THEN
      CALL SimulateFanComponents(Blank,FirstHVACIteration,Furnace(FurnaceNum)%FanIndex,FanSpeedRatio)
      IF (CoolingCoilType_Num == CoilDX_CoolingHXAssisted)THEN
        CALL SimHXAssistedCoolingCoil(Blank,FirstHVACIteration,CompOp,CoolPartLoadRatio,  &
                                      Furnace(FurnaceNum)%CoolingCoilIndex, FanOpMode, &
                                      HXUnitEnable=HXUnitOn, OnOffAFR = OnOffAirFlowRatio, EconomizerFlag=EconomizerFlag)
      ELSE
        CALL SimDXCoil(Blank,CompOp,FirstHVACIteration,CoolPartLoadRatio,Furnace(FurnaceNum)%CoolingCoilIndex, &
                       FanOpMode, OnOffAirFlowRatio)
      END IF
      CALL SimDXCoil(Blank,CompOp,FirstHVACIteration,HeatPartLoadRatio,Furnace(FurnaceNum)%HeatingCoilIndex, &
                     FanOpMode, OnOffAirFlowRatio)
      CALL SimulateFanComponents(Blank,FirstHVACIteration,Furnace(FurnaceNum)%FanIndex,FanSpeedRatio)
    END IF
!   Simulate cooling and heating coils
    IF (CoolingCoilType_Num == CoilDX_CoolingHXAssisted)THEN
      CALL SimHXAssistedCoolingCoil(Blank,FirstHVACIteration,CompOp,CoolPartLoadRatio,  &
                                    Furnace(FurnaceNum)%CoolingCoilIndex, FanOpMode, &
                                    HXUnitEnable=HXUnitOn, OnOffAFR = OnOffAirFlowRatio, EconomizerFlag=EconomizerFlag)
    ELSE
      CALL SimDXCoil(Blank,CompOp,FirstHVACIteration,CoolPartLoadRatio,Furnace(FurnaceNum)%CoolingCoilIndex, &
                     FanOpMode, OnOffAirFlowRatio)
    END IF
    CALL SimDXCoil(Blank,CompOp,FirstHVACIteration,HeatPartLoadRatio,Furnace(FurnaceNum)%HeatingCoilIndex, &
                   FanOpMode, OnOffAirFlowRatio)
!   Simulate the draw-thru fan
    IF (Furnace(FurnaceNum)%FanPlace .EQ. DrawThru) THEN
      CALL SimulateFanComponents(Blank,FirstHVACIteration,Furnace(FurnaceNum)%FanIndex,FanSpeedRatio)
    END IF
!   Simulate the supplemental heating coil
    IF(Furnace(FurnaceNum)%DehumidControlType_Num .EQ. DehumidControl_CoolReheat .AND. ReHeatCoilLoad .GT. 0.0d0)THEN
      SuppHeatingCoilFlag = .TRUE.
      CALL CalcNonDXHeatingCoils(FurnaceNum,SuppHeatingCoilFlag,FirstHVACIteration,ReHeatCoilLoad,FanOpMode,QActual)
    ELSE
      ! equivalent to QCoilReq=0.0d0 or ReHeatCoilLoad = 0.0d0
      SuppHeatingCoilFlag = .TRUE.
      CALL CalcNonDXHeatingCoils(FurnaceNum,SuppHeatingCoilFlag,FirstHVACIteration,ReHeatCoilLoad,FanOpMode,QActual)
    ENDIF
! Simulate the parameter estimate water-to-air heat pump
  ELSEIF (Furnace(FurnaceNum)%FurnaceType_Num == UnitarySys_HeatPump_WaterToAir .AND. &
          Furnace(FurnaceNum)%WatertoAirHPType == WatertoAir_Simple) Then
!    Simulate blow-thru fan and non-linear coils twice to update PLF used by the ONOFF Fan
     IF (Furnace(FurnaceNum)%FanPlace .EQ. BlowThru) THEN
       CALL SimulateFanComponents(Blank,FirstHVACIteration,Furnace(FurnaceNum)%FanIndex,FanSpeedRatio)
       !COIL:WATERTOAIRHPSIMPLE:COOLING
       CALL SimWatertoAirHPSimple(Blank, &
              Furnace(FurnaceNum)%CoolingCoilIndex, &
              Furnace(FurnaceNum)%CoolingCoilSensDemand, Furnace(FurnaceNum)%CoolingCoilLatentDemand, &
              FanOpMode,WSHPRuntimeFrac, Furnace(FurnaceNum)%MaxONOFFCyclesperHour, &  !CoolPartLoadRatio
              Furnace(FurnaceNum)%HPTimeConstant, Furnace(FurnaceNum)%FanDelayTime, CompOp,   &
              CoolPartLoadRatio, FirstHVACIteration)
       Dummy=0.0d0
       !COIL:WATERTOAIRHPSIMPLE:HEATING
       CALL SimWatertoAirHPSimple(Blank, &
              Furnace(FurnaceNum)%HeatingCoilIndex, &
              Furnace(FurnaceNum)%HeatingCoilSensDemand, dummy, &
              FanOpMode,WSHPRuntimeFrac, Furnace(FurnaceNum)%MaxONOFFCyclesperHour, &  !HeatPartLoadRatio
              Furnace(FurnaceNum)%HPTimeConstant, Furnace(FurnaceNum)%FanDelayTime, CompOp,   &
              HeatPartLoadRatio, FirstHVACIteration)
!      Simulate the whole thing a second time so that the correct PLF required by the coils is used by the Fan. *******
       CALL SimulateFanComponents(Blank,FirstHVACIteration,Furnace(FurnaceNum)%FanIndex,FanSpeedRatio)
     END IF
!    Simulate the cooling and heating coils
     !COIL:WATERTOAIRHPSIMPLE:COOLING
      CALL SimWatertoAirHPSimple(Blank, &
              Furnace(FurnaceNum)%CoolingCoilIndex, &
              Furnace(FurnaceNum)%CoolingCoilSensDemand, Furnace(FurnaceNum)%CoolingCoilLatentDemand, &
              FanOpMode,WSHPRuntimeFrac, Furnace(FurnaceNum)%MaxONOFFCyclesperHour, &  !CoolPartLoadRatio
              Furnace(FurnaceNum)%HPTimeConstant, Furnace(FurnaceNum)%FanDelayTime, CompOp,   &
              CoolPartLoadRatio, FirstHVACIteration)
      Dummy=0.0d0
     !COIL:WATERTOAIRHPSIMPLE:HEATING
      CALL SimWatertoAirHPSimple(Blank, &
              Furnace(FurnaceNum)%HeatingCoilIndex, &
              Furnace(FurnaceNum)%HeatingCoilSensDemand, dummy, &
              FanOpMode,WSHPRuntimeFrac, Furnace(FurnaceNum)%MaxONOFFCyclesperHour, &  !HeatPartLoadRatio
              Furnace(FurnaceNum)%HPTimeConstant, Furnace(FurnaceNum)%FanDelayTime, CompOp,   &
              HeatPartLoadRatio, FirstHVACIteration)
!     Simulate the draw-thru fan
      IF (Furnace(FurnaceNum)%FanPlace .EQ. BlowThru) THEN
        CALL SimulateFanComponents(Blank,FirstHVACIteration,Furnace(FurnaceNum)%FanIndex,FanSpeedRatio)
      END IF
!     Simulate the supplemental heating coil
      IF(Furnace(FurnaceNum)%DehumidControlType_Num .EQ. DehumidControl_CoolReheat .AND. ReHeatCoilLoad .GT. 0.0d0)THEN
         SuppHeatingCoilFlag = .TRUE. ! if true simulates supplemental heating coil
         CALL CalcNonDXHeatingCoils(FurnaceNum,SuppHeatingCoilFlag,FirstHVACIteration,ReheatCoilLoad,FanOpMode, QActual)
       ELSE
         SuppHeatingCoilFlag = .TRUE. ! if true simulates supplemental heating coil
         CALL CalcNonDXHeatingCoils(FurnaceNum,SuppHeatingCoilFlag,FirstHVACIteration,HeatCoilLoad,FanOpMode,QActual)
      ENDIF
! Simulate the detailed water-to-air heat pump
  ELSE IF(Furnace(FurnaceNum)%FurnaceType_Num == UnitarySys_HeatPump_WaterToAir .AND. &
          Furnace(FurnaceNum)%WatertoAirHPType == WatertoAir_ParEst) Then
!    Simulate the draw-thru fan
     IF (Furnace(FurnaceNum)%FanPlace .EQ. BlowThru) THEN
       CALL SimulateFanComponents(Blank,FirstHVACIteration,Furnace(FurnaceNum)%FanIndex,FanSpeedRatio)
     END IF
!    Simulate the cooling and heating coils
     CALL SimWatertoAirHP(Blank, &
              Furnace(FurnaceNum)%CoolingCoilIndex,  &
              Furnace(FurnaceNum)%DesignMassFlowRate,FanOpMode,FirstHVACIteration,WSHPRuntimeFrac,&
              Furnace(FurnaceNum)%MaxONOFFCyclesperHour, &
              Furnace(FurnaceNum)%HPTimeConstant, &
              Furnace(FurnaceNum)%FanDelayTime, &
              Furnace(FurnaceNum)%InitHeatPump, &
              Furnace(FurnaceNum)%CoolingCoilSensDemand, &
              Furnace(FurnaceNum)%CoolingCoilLatentDemand,CompOp, CoolPartLoadRatio)
     Dummy=0.0d0
     CALL SimWatertoAirHP(Blank, &
              Furnace(FurnaceNum)%HeatingCoilIndex,  &
              Furnace(FurnaceNum)%DesignMassFlowRate,FanOpMode,FirstHVACIteration,WSHPRuntimeFrac,&
              Furnace(FurnaceNum)%MaxONOFFCyclesperHour, &
              Furnace(FurnaceNum)%HPTimeConstant, &
              Furnace(FurnaceNum)%FanDelayTime, &
              Furnace(FurnaceNum)%InitHeatPump, &
              Furnace(FurnaceNum)%HeatingCoilSensDemand, &
              Dummy,CompOp,HeatPartLoadRatio)
!    Simulate the draw-thru fan
     IF (Furnace(FurnaceNum)%FanPlace .EQ. DrawThru) THEN
       CALL SimulateFanComponents(Blank,FirstHVACIteration,Furnace(FurnaceNum)%FanIndex,FanSpeedRatio)
     END IF
!    Simulate the supplemental heating coil
     CALL SimulateHeatingCoilComponents(Blank, FirstHVACIteration, QCoilReq=HeatCoilLoad, &
                                        CompIndex=Furnace(FurnaceNum)%SuppHeatCoilIndex,  &
                                        SuppHeat=.TRUE., FanOpMode=FanOpMode)


  ELSE ! ELSE it's not a heat pump
!   Simulate blow-thru fan
    IF (Furnace(FurnaceNum)%FanPlace .EQ. BlowThru) THEN

      CALL SimulateFanComponents(Blank,FirstHVACIteration,Furnace(FurnaceNum)%FanIndex,FanSpeedRatio)

!     For non-linear coils, simulate coil to update PLF used by the ONOFF Fan
      IF(Furnace(FurnaceNum)%FanType_Num == FanType_SimpleOnOff)THEN
        IF(Furnace(FurnaceNum)%FurnaceType_Num /= UnitarySys_HeatOnly .AND. &
            Furnace(FurnaceNum)%FurnaceType_Num /= Furnace_HeatOnly)THEN

          IF(.NOT. Furnace(FurnaceNum)%CoolingCoilUpstream)THEN
            SuppHeatingCoilFlag = .FALSE. ! if false simulates heating coil
            CALL CalcNonDXHeatingCoils(FurnaceNum,SuppHeatingCoilFlag,FirstHVACIteration,HeatCoilLoad,FanOpMode,QActual)
          END IF

          IF (CoolingCoilType_Num == CoilDX_CoolingHXAssisted)THEN
            CALL SimHXAssistedCoolingCoil(Blank,FirstHVACIteration,CompOp,CoolPartLoadRatio,  &
                                          Furnace(FurnaceNum)%CoolingCoilIndex, FanOpMode, &
                                          HXUnitEnable=HXUnitOn, OnOffAFR = OnOffAirFlowRatio, EconomizerFlag=EconomizerFlag)
          ELSE
            CALL SimDXCoil(Blank,CompOp,FirstHVACIteration,CoolPartLoadRatio,Furnace(FurnaceNum)%CoolingCoilIndex,  &
                           FanOpMode, OnOffAirFlowRatio, CoilCoolingHeatingPLRRatio = CoolHeatPLRRat)
          END IF
        END IF

        IF(Furnace(FurnaceNum)%CoolingCoilUpstream)THEN
           SuppHeatingCoilFlag = .FALSE.      ! if false simulates heating coil
           CALL CalcNonDXHeatingCoils(FurnaceNum,SuppHeatingCoilFlag,FirstHVACIteration,HeatCoilLoad,FanOpMode,QActual)
        END IF
        CALL SimulateFanComponents(Blank,FirstHVACIteration,Furnace(FurnaceNum)%FanIndex,FanSpeedRatio)
      END IF ! Simple OnOff fan

    END IF ! Blow thru fan

!   Simulate the cooling and heating coils
    IF(Furnace(FurnaceNum)%FurnaceType_Num /= UnitarySys_HeatOnly .AND. &
       Furnace(FurnaceNum)%FurnaceType_Num /= Furnace_HeatOnly)THEN

      IF(.NOT. Furnace(FurnaceNum)%CoolingCoilUpstream)THEN
         SuppHeatingCoilFlag = .FALSE.      ! if false simulates heating coil
         CALL CalcNonDXHeatingCoils(FurnaceNum,SuppHeatingCoilFlag,FirstHVACIteration,HeatCoilLoad,FanOpMode,QActual)
      END IF

      IF (CoolingCoilType_Num == CoilDX_CoolingHXAssisted)THEN
        CALL SimHXAssistedCoolingCoil(Blank,FirstHVACIteration,CompOp,CoolPartLoadRatio,  &
                                      Furnace(FurnaceNum)%CoolingCoilIndex, FanOpMode, &
                                      HXUnitEnable=HXUnitOn, OnOffAFR = OnOffAirFlowRatio, EconomizerFlag=EconomizerFlag)
      ELSE
        CALL SimDXCoil(Blank,CompOp,FirstHVACIteration,CoolPartLoadRatio,Furnace(FurnaceNum)%CoolingCoilIndex,  &
                       FanOpMode, OnOffAirFlowRatio, CoilCoolingHeatingPLRRatio = CoolHeatPLRRat)
      END IF

    END IF

    IF(Furnace(FurnaceNum)%CoolingCoilUpstream)THEN
       SuppHeatingCoilFlag = .FALSE.      ! if false simulates heating coil
       CALL CalcNonDXHeatingCoils(FurnaceNum,SuppHeatingCoilFlag,FirstHVACIteration,HeatCoilLoad,FanOpMode,QActual)
    END IF
!   Simulate the draw-thru fan
    IF (Furnace(FurnaceNum)%FanPlace .EQ. DrawThru) THEN
      CALL SimulateFanComponents(Blank,FirstHVACIteration,Furnace(FurnaceNum)%FanIndex,FanSpeedRatio)
    END IF
    IF(Furnace(FurnaceNum)%DehumidControlType_Num .EQ. DehumidControl_CoolReheat .OR. &
        Furnace(FurnaceNum)%SuppHeatCoilIndex .GT. 0)THEN
        SuppHeatingCoilFlag = .TRUE.       ! if truee simulates supplemental heating coil
        CALL CalcNonDXHeatingCoils(FurnaceNum,SuppHeatingCoilFlag,FirstHVACIteration,ReheatCoilLoad,FanOpMode,QActual)
    END IF
  END IF ! IF(Furnace(FurnaceNum)%FurnaceType_Num == UnitarySys_HeatPump_AirToAir)THEN

! check the DesignMaxOutletTemp and reset if necessary (for Coil:Gas:Heating or Coil:Electric:Heating only)
  IF(Node(Furnace(FurnaceNum)%FurnaceOutletNodeNum)%Temp .GT. Furnace(FurnaceNum)%DesignMaxOutletTemp)THEN
    Wout = Node(FurnaceOutletNode)%HumRat
    Tout = Furnace(FurnaceNum)%DesignMaxOutletTemp
    ModifiedHeatCoilLoad = HeatCoilLoad - &
                         (AirMassFlow * PsyCpAirFnWTdb(Wout,Tout) * (Node(FurnaceOutletNode)%Temp-Tout))
    Node(FurnaceOutletNode)%Temp = Tout
  ENDIF


  ! If the fan runs continually do not allow coils to set OnOffFanPartLoadRatio.
  IF (FanOpMode.EQ.ContFanCycCoil)OnOffFanPartLoadFraction = 1.0d0


 ! Check delta T (outlet to space), if positive
 ! use space HumRat (next line), else outlet humrat (IF) so psyc routine gives good result
  MinHumRatio = Node(Furnace(FurnaceNum)%NodeNumofControlledZone)%HumRat
  IF(Node(FurnaceOutletNode)%Temp .LT. Node(Furnace(FurnaceNum)%NodeNumofControlledZone)%Temp ) &
    MinHumRatio = Node(FurnaceOutletNode)%HumRat

 ! Calculate sensible load met (at constant humidity ratio)
  SensibleLoadMet = AirMassFlow * (PsyHFnTdbW(Node(FurnaceOutletNode)%Temp,MinHumRatio)  &
         - PsyHFnTdbW(Node(Furnace(FurnaceNum)%NodeNumofControlledZone)%Temp,MinHumRatio)) &
         - Furnace(FurnaceNum)%SenLoadLoss
  Furnace(FurnaceNum)%SensibleLoadMet = SensibleLoadMet

  IF(Furnace(FurnaceNum)%Humidistat)THEN
    MaxTemp = Node(Furnace(FurnaceNum)%NodeNumofControlledZone)%Temp
! modified, why does switching between furnace outlet and control zone temp
! cause latent load to change when latent capacity is 0 ?
!    IF(Node(FurnaceOutletNode)%Temp .GT. Node(Furnace(FurnaceNum)%NodeNumofControlledZone)%Temp ) &
!       MaxTemp = Node(FurnaceOutletNode)%Temp


!   Calculate latent load met (at constant temperature)
    LatentLoadMet = AirMassFlow * (PsyHFnTdbW(MaxTemp,Node(FurnaceOutletNode)%HumRat)  &
         - PsyHFnTdbW(MaxTemp,Node(Furnace(FurnaceNum)%NodeNumofControlledZone)%HumRat)) &
         - Furnace(FurnaceNum)%LatLoadLoss
  ELSE
    LatentLoadMet = 0.0d0
  END IF
  Furnace(FurnaceNum)%LatentLoadMet = LatentLoadMet


RETURN
END Subroutine CalcFurnaceOutput
!        End of Update subroutines for the Furnace Module
! *****************************************************************************




FUNCTION CalcFurnaceResidual(PartLoadRatio, Par) RESULT (Residuum)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Richard Raustad
          !       DATE WRITTEN   Feb 2005
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! To calculate the part-load ratio for cooling and heating coils

          ! METHODOLOGY EMPLOYED:
          ! Use SolveRegulaFalsi to call this Function to converge on a solution

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  REAL(r64), INTENT(IN)                         :: PartLoadRatio  ! DX cooling coil part load ratio
  REAL(r64), INTENT(IN), DIMENSION(:), OPTIONAL :: Par            ! Function parameters
  REAL(r64)                                     :: Residuum       ! Result (force to 0)

!   Parameter description example:
!       Par(1)  = REAL(FurnaceNum,r64) ! Index to furnace
!       Par(2)  = 0.0                  ! FirstHVACIteration FLAG, if 1.0 then TRUE, if 0.0 then FALSE
!       Par(3)  = REAL(OpMode,r64)     ! Fan control, if 1.0 then cycling fan, if 0.0 then continuous fan
!       Par(4)  = REAL(CompOp,r64)     ! Compressor control, if 1.0 then compressor ON, if 0.0 then compressor OFF
!       Par(5)  = CoolCoilLoad         ! Sensible or Latent load to be met by furnace
!       Par(6)  = 1.0                  ! Type of load FLAG, 0.0 if heating load, 1.0 if cooling or moisture load
!       Par(7)  = 1.0                  ! Output calculation FLAG, 0.0 for latent capacity, 1.0 for sensible capacity
!       Par(8)  = OnOffAirFlowRatio    ! Ratio of compressor ON air mass flow to AVERAGE air mass flow over time step
!       Par(9)  = HXUnitOn             ! flag to enable HX, 1=ON and 2=OFF
!       Par(10) = HeatingCoilPLR       ! used to calculate latent degradation for cycling fan RH control

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER    :: FurnaceNum         ! Index to furnace
  LOGICAL    :: FirstHVACIteration ! FirstHVACIteration flag
  INTEGER    :: FanOpMode          ! Cycling fan or constant fan
  INTEGER    :: CompOp             ! Compressor on/off; 1=on, 0=off
  REAL(r64)  :: CoolPartLoadRatio  ! DX cooling coil part load ratio
  REAL(r64)  :: HeatPartLoadRatio  ! DX heating coil part load ratio (0 for other heating coil types)
  REAL(r64)  :: HeatCoilLoad       ! Heating coil load for gas heater
  REAL(r64)  :: SensibleLoadMet    ! Sensible cooling load met (furnace outlet with respect to control zone temp)
  REAL(r64)  :: LatentLoadMet      ! Latent cooling load met (furnace outlet with respect to control zone humidity ratio)
  REAL(r64)  :: LoadToBeMet        ! Sensible or Latent load to be met by furnace
  REAL(r64)  :: OnOffAirFlowRatio  ! Ratio of compressor ON air mass flow to AVERAGE air mass flow over time step
  REAL(r64)  :: RuntimeFrac        ! heat pump runtime fraction
  REAL(r64)  :: CoolingHeatingPLRRatio ! ratio of cooling PLR to heating PLR, used for cycling fan RH control
  LOGICAL    :: HXUnitOn           ! flag to enable HX based on zone moisture load
  LOGICAL    :: errflag            ! flag denoting error in runtime calculation

! Convert parameters to usable variables
  FurnaceNum           = INT(Par(1))
  IF(Par(2) .EQ. 1.0d0)THEN
    FirstHVACIteration = .TRUE.
  ELSE
    FirstHVACIteration = .FALSE.
  END IF
  FanOpMode            = INT(Par(3))
  CompOp               = INT(Par(4))
  LoadToBeMet          = Par(5)

  IF(Par(6) .EQ. 1.0d0)THEN
   CoolPartLoadRatio   = PartLoadRatio
   HeatPartLoadRatio   = 0.0d0
   HeatCoilLoad        = 0.0d0
  ELSE
   CoolPartLoadRatio   = 0.0d0
   HeatPartLoadRatio   = PartLoadRatio

   IF(Furnace(FurnaceNum)%HeatingCoilType_Num .EQ. Coil_HeatingGas .OR. &
      Furnace(FurnaceNum)%HeatingCoilType_Num .EQ. Coil_HeatingElectric .OR. &
      Furnace(FurnaceNum)%HeatingCoilType_Num .EQ. Coil_HeatingWater .OR.    &
      Furnace(FurnaceNum)%HeatingCoilType_Num .EQ. Coil_HeatingSteam )THEN
      HeatCoilLoad      = Furnace(FurnaceNum)%DesignHeatingCapacity * PartLoadRatio
   ELSE
      HeatCoilLoad      = 0.0d0
   END IF
  END IF

!  OnOffAirFlowRatio = Par(8)
  IF(Furnace(FurnaceNum)%FurnaceType_Num == UnitarySys_HeatPump_WaterToAir) THEN
   CALL HeatPumpRunFrac(FurnaceNum,PartLoadRatio,errflag,RuntimeFrac)
   Furnace(FurnaceNum)%CompPartLoadRatio = PartLoadRatio
   Furnace(FurnaceNum)%WSHPRuntimeFrac   = RuntimeFrac
  END IF

  IF(Par(9) .EQ. 1.0d0)THEN
    HXUnitOn = .TRUE.
  ELSE
    HXUnitOn = .FALSE.
  END IF

  IF(Par(10) .GT. 0.0d0)THEN
!    Par(10) = Furnace(FurnaceNum)%HeatPartLoadRatio
!    FanOpMode = CycFan and Furnace(FurnaceNum)%HeatPartLoadRatio must be > 0 for Part(10) to be greater than 0
!    This variable used when in heating mode and dehumidification (cooling) is required.
    CoolingHeatingPLRRatio = MIN(1.0d0,CoolPartLoadRatio/Furnace(FurnaceNum)%HeatPartLoadRatio)
  ELSE
    CoolingHeatingPLRRatio = 1.0d0
  END IF

! Subroutine arguments
! CALL CalcFurnaceOutput(FurnaceNum,FirstHVACIteration,FanOpMode,CompOp,CoolPartLoadRatio,HeatPartLoadRatio, &
!                    HeatCoilLoad, ReHeatCoilLoad, SensibleLoadMet, LatentLoadMet, OnOffAirFlowRatio, HXUnitOn)
  CALL CalcFurnaceOutput(FurnaceNum,FirstHVACIteration,FanOpMode,CompOp,CoolPartLoadRatio,HeatPartLoadRatio, &
                         HeatCoilLoad, 0.0d0, SensibleLoadMet, LatentLoadMet, OnOffAirFlowRatio, HXUnitOn, &
                         CoolingHeatingPLRRatio)

! Calculate residual based on output calculation flag
  IF(Par(7) .EQ. 1.0d0) THEN
    IF(LoadToBeMet .EQ. 0.0d0)THEN
      Residuum = (SensibleLoadMet - LoadToBeMet)/100.0d0
    ELSE
      Residuum = (SensibleLoadMet - LoadToBeMet)/LoadToBeMet
    END IF
  ELSE
    IF(LoadToBeMet .EQ. 0.0d0)THEN
      Residuum = (LatentLoadMet - LoadToBeMet)/100.0d0
    ELSE
      Residuum = (LatentLoadMet - LoadToBeMet)/LoadToBeMet
    END IF
  END IF

RETURN
END FUNCTION CalcFurnaceResidual


FUNCTION CalcWaterToAirResidual(PartLoadRatio, Par) RESULT (Residuum)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Richard Raustad
          !       DATE WRITTEN   October 2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! To calculate the part-load ratio for water to air HP's
          ! this is used for parameter estimation WAHPs but not equation fit WAHPs

          ! METHODOLOGY EMPLOYED:
          ! Use SolveRegulaFalsi to call this Function to converge on a solution

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  REAL(r64), INTENT(IN)                         :: PartLoadRatio  ! DX cooling coil part load ratio
  REAL(r64), INTENT(IN), DIMENSION(:), OPTIONAL :: Par            ! Function parameters
  REAL(r64)                                     :: Residuum       ! Result (force to 0)

!   Parameter description example:
!     Par(1)  = REAL(FurnaceNum,r64) ! Index to furnace
!     Par(2)  = 0.0                  ! FirstHVACIteration FLAG, if 1.0 then TRUE, if 0.0 then FALSE
!     Par(3)  = REAL(OpMode,r64)     ! Fan control, if 1.0 then cycling fan, if 0.0 then continuous fan
!     Par(4)  = REAL(CompOp,r64)     ! Compressor control, if 1.0 then compressor ON, if 0.0 then compressor OFF
!     Par(5)  = CoolCoilLoad         ! Sensible or Latent load to be met by furnace
!     Par(6)  = 1.0                  ! Type of load FLAG, 0.0 if heating load, 1.0 if cooling or moisture load
!     Par(7)  = 1.0                  ! Output calculation FLAG, 0.0 for latent capacity, 1.0 for sensible capacity
!     Par(8)  = ZoneSensLoadMetFanONCompOFF  ! Output with fan ON compressor OFF

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER    :: FurnaceNum         ! Index to furnace
  LOGICAL    :: FirstHVACIteration ! FirstHVACIteration flag
  INTEGER    :: FanOpMode          ! Cycling fan or constant fan
  INTEGER    :: CompOp             ! Compressor on/off; 1=on, 0=off
  REAL(r64)  :: CoolPartLoadRatio  ! DX cooling coil part load ratio
  REAL(r64)  :: HeatPartLoadRatio  ! DX heating coil part load ratio (0 for other heating coil types)
  REAL(r64)  :: HeatCoilLoad       ! Heating coil load for gas heater
  REAL(r64)  :: ZoneSensLoadMet    ! Sensible cooling load met (furnace outlet with respect to control zone temp)
  REAL(r64)  :: ZoneLatLoadMet     ! Latent cooling load met (furnace outlet with respect to control zone humidity ratio)
  REAL(r64)  :: LoadToBeMet        ! Sensible or Latent load to be met by furnace
  LOGICAL    :: errflag
  REAL(r64)  :: RuntimeFrac
  REAL(r64)  :: Dummy
  REAL(r64)  :: HPCoilSensDemand
  REAL(r64)  :: ZoneSensLoadMetFanONCompOFF
  REAL(r64)  :: OnOffAirFlowRatio
  LOGICAL    :: HXUnitOn           ! flag to enable HX based on zone moisture load (not valid for water-to-air HP's

! Convert parameters to usable variables
  FurnaceNum           = INT(Par(1))
  IF(Par(2) .EQ. 1.0d0)THEN
    FirstHVACIteration = .TRUE.
  ELSE
    FirstHVACIteration = .FALSE.
  END IF
  FanOpMode            = INT(Par(3))
  CompOp               = INT(Par(4))
  LoadToBeMet          = Par(5)

  IF(Par(6) .EQ. 1.0d0)THEN
   CoolPartLoadRatio   = PartLoadRatio
   HeatPartLoadRatio   = 0.0d0
   HeatCoilLoad        = 0.0d0
  ELSE
   CoolPartLoadRatio   = 0.0d0
   HeatPartLoadRatio   = PartLoadRatio
  END IF
  ZoneSensLoadMetFanONCompOFF = Par(8)
       !calculate the run time fraction
   CALL HeatPumpRunFrac(FurnaceNum,PartLoadRatio,errflag,RuntimeFrac)

       !update the fan part load factor
       !see 'Note' under INITIAL CALCULATIONS
  IF(Par(6) .EQ. 1.0d0)THEN
   IF(RuntimeFrac > 0.0d0)THEN
     OnOffFanPartLoadFraction = CoolPartLoadRatio/RuntimeFrac
   ELSE
     OnOffFanPartLoadFraction = 1.0d0
   ENDIF
  ELSE
   IF(RuntimeFrac > 0.0d0)THEN
     OnOffFanPartLoadFraction = PartLoadRatio/RuntimeFrac
!   Else IF(RuntimeFrac == 0.0d0)THEN
!     OnOffFanPartLoadFraction = 0.0
   ELSE
     OnOffFanPartLoadFraction = 1.0d0
   ENDIF
  END IF
  OnOffFanPartLoadFractionSave = OnOffFanPartLoadFraction
       !update fan and compressor run times
  Furnace(FurnaceNum)%CompPartLoadRatio = PartLoadRatio
  Furnace(FurnaceNum)%WSHPRuntimeFrac   = RuntimeFrac

       !Calculate the heating coil demand as (the zone sensible load - load met by fan heat and mixed air)
       !Note; The load met by fan heat and mixed air is calculated as mdot(zoneinletenthalpy-zoneoutletenthalpy)
       !This accounts for the negative sign in the equation.

       !Calculate the heat coil sensible capacity as the load met by the system with the fan and compressor on less
       !the load met by the system with the compressor off.
!  HPCoilSensCapacity = ZoneSensLoadMetFanONCompON - ZoneSensLoadMetFanONCompOFF

    !Set input parameters for heat pump coil model
  HPCoilSensDemand = LoadToBeMet  - RuntimeFrac*ZoneSensLoadMetFanONCompOFF
!  HPCoilSensDemand = LoadToBeMet  - PartLoadRatio*ZoneSensLoadMetFanONCompOFF
  IF(Par(6) .EQ. 1.0d0)THEN
    Furnace(FurnaceNum)%HeatingCoilSensDemand   = 0.0d0
    Furnace(FurnaceNum)%CoolingCoilSensDemand   = ABS(HPCoilSensDemand)
  ELSE
    Furnace(FurnaceNum)%HeatingCoilSensDemand   = HPCoilSensDemand
    Furnace(FurnaceNum)%CoolingCoilSensDemand   = 0.0d0
  END IF
  Furnace(FurnaceNum)%InitHeatPump = .FALSE.      !initialization call to Calc Furnace

    !Calculate the zone loads met and the new part load ratio and for the specified run time
   Dummy=0.0d0
   OnOffAirFlowRatio = 1.0d0
   IF(Par(9) .EQ. 1.0d0)THEN
     HXUnitOn = .TRUE.
   ELSE
     HXUnitOn = .FALSE.
   END IF

!  Subroutine arguments
!  CALL CalcFurnaceOutput(FurnaceNum,FirstHVACIteration,FanOpMode,CompOp,CoolPartLoadRatio,&
!                         HeatPartLoadRatio, HeatCoilLoad, ReHeatCoilLoad, SensibleLoadMet, LatentLoadMet, HXUnitOn)
   CALL CalcFurnaceOutput(FurnaceNum,FirstHVACIteration,FanOpMode,CompOp,CoolPartLoadRatio,&
                          HeatPartLoadRatio, Dummy, Dummy, ZoneSensLoadMet,ZoneLatLoadMet, OnOffAirFlowRatio, HXUnitOn)

! Calculate residual based on output calculation flag
  IF(Par(7) .EQ. 1.0d0) THEN
    Residuum = (ZoneSensLoadMet - LoadToBeMet)/LoadToBeMet
  ELSE
    Residuum = (ZoneLatLoadMet - LoadToBeMet)/LoadToBeMet
  END IF

RETURN
END FUNCTION CalcWaterToAirResidual


SUBROUTINE SetAverageAirFlow(FurnaceNum,PartLoadRatio,OnOffAirFlowRatio)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Raustad
          !       DATE WRITTEN   July 2005
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Set the average air mass flow rates using the part-load fraction of the HVAC system for this time step
          ! Set OnOffAirFlowRatio to be used by DX coils

          ! METHODOLOGY EMPLOYED:
          ! The air flow rate in cooling, heating, and no cooling or heating can be different.
          ! Calculate the air flow rate based on initializations made in InitFurnace.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataZoneEnergyDemands

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER,   INTENT (IN)    :: FurnaceNum         ! Unit index
  REAL(r64), INTENT (IN)    :: PartLoadRatio      ! unit part load ratio
  REAL(r64), INTENT (INOUT) :: OnOffAirFlowRatio  ! ratio of compressor ON airflow to AVERAGE airflow over timestep

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER             :: InletNode           ! inlet node number for furnace
  REAL(r64)           :: AverageUnitMassFlow ! average supply air mass flow rate over time step

  InletNode      = Furnace(FurnaceNum)%FurnaceInletNodeNum

  AverageUnitMassFlow = (PartLoadRatio * CompOnMassFlow) + ((1-PartLoadRatio) * CompOffMassFlow)
  IF(CompOffFlowRatio .GT. 0.0d0)THEN
    FanSpeedRatio     = (PartLoadRatio * CompOnFlowRatio) + ((1-PartLoadRatio) * CompOffFlowRatio)
  ELSE
    FanSpeedRatio     = CompOnFlowRatio
  END IF
! IF the furnace is scheduled on or nightime cycle overrides fan schedule. Uses same logic as fan.
  IF ( GetCurrentScheduleValue(Furnace(FurnaceNum)%SchedPtr) .GT. 0.0d0 .AND. &
     ( (GetCurrentScheduleValue(Furnace(FurnaceNum)%FanAvailSchedPtr) .GT. 0.0d0 .OR. TurnFansOn) &
          .AND. .NOT. TurnFansOff) ) THEN
    Node(InletNode)%MassFlowRate              = AverageUnitMassFlow
    Node(InletNode)%MassFlowRateMaxAvail      = AverageUnitMassFlow
    IF (AverageUnitMassFlow .GT. 0.0d0) THEN
      OnOffAirFlowRatio                       = CompOnMassFlow / AverageUnitMassFlow
    ELSE
      OnOffAirFlowRatio                       = 0.0d0
    END IF
  ELSE
    Node(InletNode)%MassFlowRate              = 0.0d0
    OnOffAirFlowRatio                         = 1.0d0
  END IF

  Furnace(FurnaceNum)%MdotFurnace = CompOnMassFlow
  OnOffAirFlowRatioSave = OnOffAirFlowRatio

  RETURN
END SUBROUTINE SetAverageAirFlow


SUBROUTINE HeatPumpRunFrac(FurnaceNum,PLR,errflag,RuntimeFrac)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Kenneth Tang
          !       DATE WRITTEN   Apr 2004
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
  INTEGER,   INTENT(IN)           :: FurnaceNum       ! Furnace Index Number
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

  Nmax=Furnace(FurnaceNum)%MaxONOFFCyclesperHour
  tau=Furnace(FurnaceNum)%HPTimeConstant
  pr=Furnace(FurnaceNum)%OnCyclePowerFraction

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
            PLF2 = 1.0d0 - A * (1.0d0 - Exp(-1.0d0 / A))
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



! Beginning of Reporting subroutines for the Furnace Module
! *****************************************************************************


SUBROUTINE ReportFurnace(FurnaceNum)


          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Liesen
          !       DATE WRITTEN   Feb 2001
          !       MODIFIED       na
          !       RE-ENGINEERED  na


          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine updates the report variable for the coils.


          ! METHODOLOGY EMPLOYED:
          ! Update fan part-load ratio based on mass flow rate ratio.
          ! Update global variables used by AirflowNetwork module.


          ! REFERENCES:
          ! na


          ! USE STATEMENTS:
  USE DataAirLoop, ONLY: LoopSystemOnMassFlowrate,LoopSystemOffMassFlowrate,LoopFanOperationMode,LoopOnOffFanPartLoadRatio


  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine


          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: FurnaceNum


          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na


          ! INTERFACE BLOCK SPECIFICATIONS
          ! na


          ! DERIVED TYPE DEFINITIONS
          ! na


          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          ! na


  ! Report the Furnace Fan Part Load Ratio
  IF (Furnace(FurnaceNum)%NumOfSpeedCooling < 1) THEN
      IF(Furnace(FurnaceNum)%DesignMassFlowRate .GT. 0.0d0)THEN
        Furnace(FurnaceNum)%FanPartLoadRatio = Furnace(FurnaceNum)%MdotFurnace/Furnace(FurnaceNum)%DesignMassFlowRate
      ELSE
        Furnace(FurnaceNum)%FanPartLoadRatio = 0.0d0
      END IF
  END IF

  ! Set mass flow rates during on and off cylce using an OnOff fan
  LoopSystemOnMassFlowrate = CompOnMassFlow
  LoopSystemOffMassFlowrate = CompOffMassFlow
  LoopFanOperationMode = Furnace(FurnaceNum)%OpMode
  LoopOnOffFanPartLoadRatio = Furnace(FurnaceNum)%FanPartLoadRatio

  RETURN
END Subroutine ReportFurnace

SUBROUTINE CalcNonDXHeatingCoils(FurnaceNum,SuppHeatingCoilFlag,FirstHVACIteration,QCoilLoad,FanMode,HeatCoilLoadmet)
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
  INTEGER,      INTENT(IN)    :: FurnaceNum                ! Furnace Index
  LOGICAL,      INTENT(IN)    :: FirstHVACIteration        ! flag for first HVAC iteration in the time step
  REAL(r64),    INTENT(IN)    :: QCoilLoad                 ! load met by unit (watts)
  REAL(r64),    INTENT(OUT)   :: HeatCoilLoadmet           ! Heating Load Met
  LOGICAL,      INTENT(IN)    :: SuppHeatingCoilFlag       ! .true. if supplemental heating coil
  INTEGER,      INTENT(IN)    :: FanMode                   ! fan operation mode

          ! SUBROUTINE PARAMETER DEFINITIONS:
  REAL(r64), PARAMETER :: ErrTolerance = 0.001d0    ! convergence limit for hotwater coil
  INTEGER, PARAMETER :: SolveMaxIter=50

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64)      :: QCoilReq        ! Heat addition required from an electric, gas, steam , or hot water heating coil
  REAL(r64)      :: QActual         ! actual heating load
  INTEGER        :: FanOpMode       ! cycling fan or constant fan
  REAL(r64)      :: mdot            ! heating coil steam or hot water mass flow rate
  REAL(r64)      :: MinWaterFlow    ! coil minimum hot water mass flow rate, kg/s
  REAL(r64)      :: MaxHotWaterFlow ! coil maximum hot water mass flow rate, kg/s
  REAL(r64)      :: HotWaterMdot    ! actual hot water mass flow rate
  REAL(r64), DIMENSION(4) :: Par
  INTEGER        :: SolFlag
  CHARACTER(len=MaxNameLength) :: HeatingCoilName  =' ' ! name of heating coil
  CHARACTER(len=MaxNameLength) :: HeatingCoilType  =' ' ! type of heating coil
  INTEGER      :: CoilTypeNum                      =0   ! heating coil type number
  INTEGER      :: HeatingCoilIndex                 =0   ! heating coil index
  INTEGER      :: CoilControlNode                  =0   ! control node for hot water and steam heating coils
  INTEGER      :: CoilOutletNode                   =0   ! air outlet node of the heatiing coils
  INTEGER      :: LoopNum                          =0   ! plant loop number
  INTEGER      :: LoopSideNum                      =0   ! plant loop side number
  INTEGER      :: BranchNum                        =0   ! plant branch number
  INTEGER      :: CompNum                          =0   ! Numeric Equivalent for Supplemental Heat Coil Type

 QActual=0.0d0

  IF ( SuppHeatingCoilFlag ) Then
   HeatingCoilName = Furnace(FurnaceNum)%SuppHeatCoilName
   HeatingCoilIndex = Furnace(FurnaceNum)%SuppHeatCoilIndex
   CoilControlNode = Furnace(FurnaceNum)%SuppCoilControlNode
   CoilOutletNode = Furnace(FurnaceNum)%SuppCoilOutletNode
   CoilTypeNum = Furnace(FurnaceNum)%SuppHeatCoilType_Num
   LoopNum = Furnace(FurnaceNum)%LoopNumSupp
   LoopSideNum =  Furnace(FurnaceNum)%LoopSideSupp
   BranchNum =  Furnace(FurnaceNum)%BranchNumSupp
   CompNum = Furnace(FurnaceNum)%CompNumSupp
   MaxHotWaterFlow = Furnace(FurnaceNum)%MaxSuppCoilFluidFlow
ELSE
   HeatingCoilName = Furnace(FurnaceNum)%HeatingCoilName
   HeatingCoilIndex = Furnace(FurnaceNum)%HeatingCoilIndex
   CoilControlNode = Furnace(FurnaceNum)%CoilControlNode
   CoilOutletNode =  Furnace(FurnaceNum)%CoilOutletNode
   CoilTypeNum = Furnace(FurnaceNum)%HeatingCoilType_Num
   LoopNum = Furnace(FurnaceNum)%LoopNum
   LoopSideNum =  Furnace(FurnaceNum)%LoopSide
   BranchNum =  Furnace(FurnaceNum)%BranchNum
   CompNum = Furnace(FurnaceNum)%CompNum
   MaxHotWaterFlow = Furnace(FurnaceNum)%MaxHeatCoilFluidFlow
END IF

Select Case (CoilTypeNum)
     Case (Coil_HeatingGas,Coil_HeatingElectric,Coil_HeatingDesuperheater )
         CALL SimulateHeatingCoilComponents(HeatingCoilName,FirstHVACIteration, &
                                QCoilReq=QCoilLoad, CompIndex=HeatingCoilIndex, &
                                SuppHeat=SuppHeatingCoilFlag,FanOpMode=FanMode)
     Case (Coil_HeatingWater)
       IF ( QCoilLoad > SmallLoad) Then
          Call SetComponentFlowRate( MaxHotWaterFlow, CoilControlNode,CoilOutletNode,LoopNum, &
                                     LoopSideNum, BranchNum, CompNum)
          CALL SimulateWaterCoilComponents(HeatingCoilName,FirstHVACIteration,HeatingCoilIndex, QActual,FanMode)

          IF ( QActual > (QCoilLoad + SmallLoad)) THEN
             ! control water flow to obtain output matching QCoilLoad
             MinWaterFlow = 0.0d0
             Par(1) = REAL(FurnaceNum,r64)
             IF (FirstHVACIteration) THEN
               Par(2) = 1.d0
             ELSE
               Par(2) = 0.0d0
             END IF
             Par(3) = QCoilLoad
             IF (SuppHeatingCoilFlag) THEN
               Par(4) = 1.d0
             ELSE
               Par(4) = 0.0d0
             END IF
             CALL SolveRegulaFalsi(ErrTolerance, SolveMaxIter, SolFlag, HotWaterMdot, HotWaterCoilResidual, &
                                   MinWaterFlow, MaxHotWaterFlow, Par)
             IF (SolFlag == -1) THEN
               IF (Furnace(FurnaceNum)%HotWaterCoilMaxIterIndex == 0) THEN
                 CALL ShowWarningMessage('CalcNonDXHeatingCoils: Hot water coil control failed for '//  &
                    trim(cFurnaceTypes(Furnace(FurnaceNum)%FurnaceType_Num))//'="'//  &
                    TRIM(Furnace(FurnaceNum)%Name)//'"')
                 CALL ShowContinueErrorTimeStamp(' ')
                 CALL ShowContinueError('  Iteration limit ['//trim(RoundSigDigits(SolveMaxIter))//  &
                    '] exceeded in calculating hot water mass flow rate')
               ENDIF
               CALL ShowRecurringWarningErrorAtEnd('CalcNonDXHeatingCoils: Hot water coil control failed (iteration limit ['//  &
                  trim(RoundSigDigits(SolveMaxIter))//']) for '//trim(cFurnaceTypes(Furnace(FurnaceNum)%FurnaceType_Num))//'="'// &
                  TRIM(Furnace(FurnaceNum)%Name),Furnace(FurnaceNum)%HotWaterCoilMaxIterIndex)
             ELSE IF (SolFlag == -2) THEN
               IF (Furnace(FurnaceNum)%HotWaterCoilMaxIterIndex2 == 0) THEN
                 CALL ShowWarningMessage('CalcNonDXHeatingCoils: Hot water coil control failed (maximum flow limits) for '//  &
                    trim(cFurnaceTypes(Furnace(FurnaceNum)%FurnaceType_Num))//'="'// &
                    TRIM(Furnace(FurnaceNum)%Name)//'"')
                 CALL ShowContinueErrorTimeStamp(' ')
                 CALL ShowContinueError('...Bad hot water maximum flow rate limits')
                 CALL ShowContinueError('...Given minimum water flow rate='//trim(RoundSigDigits(MinWaterFlow,3))//' kg/s')
                 CALL ShowContinueError('...Given maximum water flow rate='//trim(RoundSigDigits(MaxHotWaterFlow,3))//' kg/s')
               ENDIF
               CALL ShowRecurringWarningErrorAtEnd('CalcNonDXHeatingCoils: Hot water coil control failed (flow limits) for '//  &
                  trim(cFurnaceTypes(Furnace(FurnaceNum)%FurnaceType_Num))//'="'// &
                  TRIM(Furnace(FurnaceNum)%Name)//'"', &
                  Furnace(FurnaceNum)%HotWaterCoilMaxIterIndex2,  &
                  ReportMinOf=MinWaterFlow,ReportMaxOf=MaxHotWaterFlow,ReportMinUnits='[kg/s]',ReportMaxUnits='[kg/s]')
             END IF
          ENDIF
       ELSE
          mdot = 0.0d0
          Call SetComponentFlowRate( mdot, CoilControlNode,CoilOutletNode,LoopNum, &
                                     LoopSideNum, BranchNum, CompNum)
       ENDIF
       ! simulate the hot water heating coil
       CALL SimulateWaterCoilComponents(HeatingCoilName,FirstHVACIteration,HeatingCoilIndex, QActual,FanMode)
     Case (Coil_HeatingSteam)
         IF ( QCoilLoad > SmallLoad) Then
             Call SetComponentFlowRate( MaxHotWaterFlow, CoilControlNode,CoilOutletNode,LoopNum, &
                                        LoopSideNum, BranchNum, CompNum)
             ! simulate the steam heating coil
             CALL SimulateSteamCoilComponents(HeatingCoilName, FirstHVACIteration, QCoilLoad, HeatingCoilIndex, &
                                              QActual, FanMode)
         ELSE
             mdot = 0.0d0
             Call SetComponentFlowRate( mdot, CoilControlNode,CoilOutletNode,LoopNum, &
                                        LoopSideNum, BranchNum, CompNum)
             ! simulate the steam heating coil
             CALL SimulateSteamCoilComponents(HeatingCoilName, FirstHVACIteration, QCoilLoad, HeatingCoilIndex, &
                                              QActual, FanMode)
         ENDIF

END Select

HeatCoilLoadmet = QActual

RETURN
END SUBROUTINE CalcNonDXHeatingCoils

FUNCTION HotWaterCoilResidual(HWFlow, Par) RESULT (Residuum)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Bereket Nigusse, FSEC/UCF
          !       DATE WRITTEN   January 2011
          !       MODIFIED
          !       RE-ENGINEERED

          ! PURPOSE OF THIS FUNCTION:
          ! Calculates residual function (QCoilActual - QCoilRequested) / QCoilRequested
          ! the coil actual output depends on the hot water flow rate which is being varied
          ! to minimize the residual.

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
  INTEGER               :: FurnaceNum
  LOGICAL               :: FirstHVACIteration
  REAL(r64)             :: QCoilActual             ! delivered coild load, W
  REAL(r64)             :: QCoilRequested          ! requested coild load, W
  REAL(r64)             :: mdot
  LOGICAL               :: SuppHeatingCoilFlag       ! .true. if supplemental heating coil

  FurnaceNum = INT(Par(1))
  IF (Par(2) > 0.0d0) THEN
    FirstHVACIteration = .TRUE.
  ELSE
    FirstHVACIteration = .FALSE.
  END IF
  QCoilRequested =  Par(3)
  IF (Par(4) > 0.0d0) THEN
    SuppHeatingCoilFlag = .TRUE.
  ELSE
    SuppHeatingCoilFlag = .FALSE.
  END IF
  QCoilActual = QCoilRequested
  mdot = HWFlow
  IF (.NOT. SuppHeatingCoilFlag ) Then

      Call SetComponentFlowRate( mdot , &
                                Furnace(FurnaceNum)%CoilControlNode, &
                                Furnace(FurnaceNum)%CoilOutletNode, &
                                Furnace(FurnaceNum)%LoopNum, &
                                Furnace(FurnaceNum)%LoopSide, &
                                Furnace(FurnaceNum)%BranchNum, &
                                Furnace(FurnaceNum)%CompNum)
      CALL SimulateWaterCoilComponents(Furnace(FurnaceNum)%HeatingCoilName,FirstHVACIteration, &
                                Furnace(FurnaceNum)%HeatingCoilIndex, QCoilActual, &
                                Furnace(FurnaceNum)%OpMode)
  ELSE
      ! supplemental coil
      Call SetComponentFlowRate( mdot , &
                                 Furnace(FurnaceNum)%SuppCoilControlNode, &
                                 Furnace(FurnaceNum)%SuppCoilOutletNode, &
                                 Furnace(FurnaceNum)%LoopNumSupp, &
                                 Furnace(FurnaceNum)%LoopSideSupp, &
                                 Furnace(FurnaceNum)%BranchNumSupp, &
                                 Furnace(FurnaceNum)%CompNumSupp)
      ! simulate the hot water supplemental heating coil
      CALL SimulateWaterCoilComponents(Furnace(FurnaceNum)%SuppHeatCoilName,FirstHVACIteration, &
                                 Furnace(FurnaceNum)%SuppHeatCoilIndex, QCoilActual, &
                                 Furnace(FurnaceNum)%OpMode)
  ENDIF
  IF (QCoilRequested /= 0.0d0) THEN
    Residuum = (QCoilActual - QCoilRequested)/ QCoilRequested
  ELSE !Objexx:Return ELSE added to assure return value is set
    Residuum = 0.0d0
  ENDIF
  RETURN
END FUNCTION HotWaterCoilResidual

!        End of Reporting subroutines for the Furnace Module

!******************************************************************************

SUBROUTINE SimVariableSpeedHP(FurnaceNum,FirstHVACIteration, QZnReq, QLatReq, OnOffAirFlowRatio)

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
  USE DataHVACGlobals, ONLY: SmallMassFlow, SmallLoad
  USE DataZoneEquipment, ONLY: ZoneEquipConfig
  USE DataAirLoop, ONLY: AirLoopControlInfo,  AirToZoneNodeInfo
  USE DataAirSystems,  ONLY: PrimaryAirSystem
  ! USE DataConvergParams, ONLY: HVACFlowRateToler

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  LOGICAL, INTENT (IN)    :: FirstHVACIteration ! TRUE if 1st HVAC simulation of system timestep
  INTEGER, INTENT (IN)    :: FurnaceNum      ! number of the current engine driven Heat Pump being simulated
  REAL(r64),    INTENT (IN)    :: QZnReq             ! required zone load
  REAL(r64),    INTENT (IN)    :: QLatReq            ! required latent load
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
  REAL(r64)    :: QTotUnitOut  ! capacity output
  INTEGER :: SpeedNum  = 1         ! Speed number
  REAL(r64)    :: SupHeaterLoad = 0.0d0 ! supplement heater load
  INTEGER :: CompOp            ! compressor operation; 1=on, 0=off
  INTEGER :: AirLoopNumber     ! Index to air loop
  REAL(r64)    :: SaveMassFlowRate  ! saved inlet air mass flow rate [kg/s]
  REAL(r64)    :: QSensUnitOut    ! sensible capacity output
  REAL(r64)    :: QLatUnitOut     ! latent capacity output
  REAL(r64), SAVE        :: TotalZoneLatentLoad    ! Total ZONE latent load
  REAL(r64), SAVE        :: TotalZoneSensibleLoad    ! Total ZONE sensible load
  REAL(r64)          :: ActualSensibleOutput ! Actual furnace sensible capacity
  REAL(r64)          :: ReheatCoilLoad ! reheat coil load due to dehumidification
  REAL(r64), SAVE   ::  SystemSensibleLoad   ! Positive value means heating required
  REAL(r64)         ::  QToHeatSetPt         ! Load required to meet heating setpoint temp (>0 is a heating load)
  REAL(r64)          :: NoCompOutput  ! output when no active compressor [W]
  INTEGER           ::  TotBranchNum ! total exit branch number
  INTEGER           ::  ZoneSideNodeNum ! zone equip supply node
  LOGICAL           ::  EconoActive        ! TRUE if Economizer is active

                                            ! to be removed by furnace/unitary system

  ! zero the fan, DX coils, and supplemental electric heater electricity consumption
  FanElecPower         = 0.0d0
  DXElecHeatingPower   = 0.0d0
  DXElecCoolingPower   = 0.0d0
  SaveCompressorPLR    = 0.0d0
  ElecHeatingCoilPower = 0.0d0

  SystemSensibleLoad = QZnReq
  TotalZoneSensibleLoad = QZnReq
  TotalZoneLatentLoad = QLatReq

  ! initialize local variables
  UnitOn      = .TRUE.
  OutletNode  = Furnace(FurnaceNum)%FurnaceOutletNodeNum
  InletNode   = Furnace(FurnaceNum)%FurnaceInletNodeNum
  AirMassFlow = Furnace(FurnaceNum)%DesignMassFlowRate
  OpMode      = Furnace(FurnaceNum)%OpMode
  ZoneNum     = Furnace(FurnaceNum)%ControlZoneNum
  CompOp      = On

   !Set latent load for heating
  IF(HeatingLoad)THEN
      Furnace(FurnaceNum)%HeatCoolMode = HeatingMode
 !Set latent load for cooling and no sensible load condition
  ELSE IF(CoolingLoad) THEN
      Furnace(FurnaceNum)%HeatCoolMode = CoolingMode
  ELSE
      Furnace(FurnaceNum)%HeatCoolMode = 0
  ENDIF


  ! set the on/off flags
  IF (Furnace(FurnaceNum)%OPMode == CycFanCycCoil) THEN
    ! cycling unit only runs if there is a cooling or heating load.
     IF (ABS(QZnReq) < SmallLoad .OR. AirMassFlow < SmallMassFlow .OR. CurDeadbandOrSetback(ZoneNum)) THEN
       UnitOn = .FALSE.
     END IF
  ELSE IF  (Furnace(FurnaceNum)%OPMode == ContFanCycCoil) THEN
    ! continuous unit: fan runs if scheduled on; coil runs only if there is a cooling or heating load
    IF (AirMassFlow.LT.SmallMassFlow) THEN
      UnitOn = .FALSE.
    END IF
  END IF

  OnOffFanPartLoadFraction = 1.0d0

  AirLoopNumber = ZoneEquipConfig(Furnace(FurnaceNum)%ControlZoneNum)%AirLoopNum

  IF(AirLoopNumber /= 0) THEN
    EconoActive = AirLoopControlInfo(AirLoopNumber)%EconoActive
  ELSE
    EconoActive = .FALSE.
  END IF

  SaveMassFlowRate = Node(InletNode)%MassFlowRate
  IF ( .NOT. FirstHVACIteration .AND. Furnace(FurnaceNum)%OPMode == CycFanCycCoil .AND. &
        (QZnReq < (-1.d0*SmallLoad) .OR. TotalZoneLatentLoad < (-1.d0*SmallLoad)  ) &
       .AND. EconoActive) THEN
       ! for cycling fan, cooling load, check whether furnace can meet load with compressor off
    CompOp = Off
    CALL ControlVSHPOutput(FurnaceNum,FirstHVACIteration,CompOp,OpMode,TotalZoneSensibleLoad, TotalZoneLatentLoad,ZoneNum,  &
       SpeedNum,SpeedRatio,PartLoadFrac,OnOffAirFlowRatio,SupHeaterLoad)

     TotalZoneSensibleLoad = QZnReq
     TotalZoneLatentLoad = QLatReq

    IF (SpeedNum .EQ. Furnace(FurnaceNum)%NumOfSpeedCooling .AND. SpeedRatio .EQ. 1.0d0) THEN
      ! compressor on (reset inlet air mass flow rate to starting value)
      Node(InletNode)%MassFlowRate = SaveMassFlowRate
      CompOp = On
      CALL ControlVSHPOutput(FurnaceNum,FirstHVACIteration,CompOp,OpMode,TotalZoneSensibleLoad,TotalZoneLatentLoad,ZoneNum,  &
         SpeedNum,SpeedRatio,PartLoadFrac,OnOffAirFlowRatio,SupHeaterLoad)
    END IF
  ELSE
     ! compressor on
     CompOp      = On

!     if(QZnReq < -1000.0 .AND. FurnaceNum == 1 ) then
!       CompOp      = On
!     end if
     CALL ControlVSHPOutput(FurnaceNum,FirstHVACIteration,CompOp,OpMode,TotalZoneSensibleLoad,TotalZoneLatentLoad,ZoneNum,  &
        SpeedNum,SpeedRatio,PartLoadFrac,OnOffAirFlowRatio,SupHeaterLoad)
  END IF

  IF (Furnace(FurnaceNum)%FurnaceType_Num == UnitarySys_HeatCool) THEN
    SaveCompressorPLR = PartLoadFrac
  ELSE
     IF(SpeedNum > 1)  THEN
       SaveCompressorPLR = 1.0d0
     END IF

     IF (PartLoadFrac .eq. 1.0d0 .and. SaveCompressorPLR < 1.0d0) then
       PartLoadFrac = SaveCompressorPLR
     End IF
  END IF

  ReheatCoilLoad = 0.0d0
  TotalZoneSensibleLoad = QZnReq
  TotalZoneLatentLoad = QLatReq
  !     Calculate the reheat coil output
  IF((GetCurrentScheduleValue(Furnace(FurnaceNum)%SchedPtr) .gt. 0.0d0 .and. CoolingLoad) .AND. &
       (Furnace(FurnaceNum)%Humidistat .AND. &
        Furnace(FurnaceNum)%DehumidControlType_Num == DehumidControl_CoolReheat .and. &
       (QLatReq.lt.0.0d0))) THEN ! if a Humidistat is installed and dehumdification control type is CoolReheat
    CALL CalcVarSpeedHeatPump(FurnaceNum,FirstHVACIteration,CompOp,SpeedNum,SpeedRatio,PartLoadFrac,ActualSensibleOutput, &
                    QLatUnitOut, TotalZoneSensibleLoad,TotalZoneLatentLoad, OnOffAirFlowRatio,ReheatCoilLoad)
    IF (Furnace(FurnaceNum)%ZoneSequenceHeatingNum > 0) THEN
      QToHeatSetPt=(ZoneSysEnergyDemand(Furnace(FurnaceNum)%ControlZoneNum)% &
                     SequencedOutputRequiredToHeatingSP(Furnace(FurnaceNum)%ZoneSequenceHeatingNum) / &
                     Furnace(FurnaceNum)%ControlZoneMassFlowFrac)
    ELSE
      QToHeatSetPt=(ZoneSysEnergyDemand(Furnace(FurnaceNum)%ControlZoneNum)%OutputRequiredToHeatingSP / &
                  Furnace(FurnaceNum)%ControlZoneMassFlowFrac)
    ENDIF
!       Cooling mode or floating condition and dehumidification is required
    IF(QToHeatSetPt .LT. 0.0d0)THEN
!         Calculate the reheat coil load wrt the heating setpoint temperature. Reheat coil picks up
!         the entire excess sensible cooling (DX cooling coil and impact of outdoor air).
      ReheatCoilLoad = MAX(0.0d0,(QToHeatSetPt-ActualSensibleOutput))
      Furnace(FurnaceNum)%DehumidInducedHeatingDemandRate = ReheatCoilLoad
!       Heating mode and dehumidification is required
    ELSEIF(QToHeatSetPt .GE. 0.0d0)THEN
      ReheatCoilLoad = MAX(QToHeatSetPt,QToHeatSetPt-ActualSensibleOutput)
      Furnace(FurnaceNum)%DehumidInducedHeatingDemandRate = MAX(0.0d0,ActualSensibleOutput*(-1.0d0))
    ELSE
      ReheatCoilLoad = 0.0d0
    END IF

   SupHeaterLoad = 0.0d0
   CALL CalcVarSpeedHeatPump(FurnaceNum,FirstHVACIteration,CompOp,1,0.0d0,0.0d0,NoCompOutput, QLatUnitOut, &
                          0.0d0, 0.0d0, OnOffAirFlowRatio,SupHeaterLoad)

    IF(NoCompOutput > SystemSensibleLoad .AND. SystemSensibleLoad > 0.0d0 .AND. ReHeatCoilLoad > 0.0d0) THEN
     !Reduce reheat coil load if you are controlling high humidity but outside air
! and/or the supply air fan is providing enough heat to meet the system sensible load.
! This will bring the zone temp closer to the heating setpoint temp.
      ReHeatCoilLoad = MAX(0.0d0,ReHeatCoilLoad-(NoCompOutput-SystemSensibleLoad))
    END IF
  ELSE
!       No humidistat installed
    ReheatCoilLoad = 0.0d0
  END IF

  TotalZoneSensibleLoad = QZnReq
  TotalZoneLatentLoad = QLatReq
  IF(ReHeatCoilLoad > 0.0d0) THEN
     CALL CalcVarSpeedHeatPump(FurnaceNum,FirstHVACIteration,CompOp,SpeedNum,SpeedRatio,PartLoadFrac,QSensUnitOut, QLatUnitOut, &
                TotalZoneSensibleLoad,TotalZoneLatentLoad,OnOffAirFlowRatio,ReHeatCoilLoad)
  ELSE
     CALL CalcVarSpeedHeatPump(FurnaceNum,FirstHVACIteration,CompOp,SpeedNum,SpeedRatio,PartLoadFrac,QSensUnitOut, QLatUnitOut, &
                      TotalZoneSensibleLoad,TotalZoneLatentLoad,OnOffAirFlowRatio,SupHeaterLoad)
  END IF


  ! calculate delivered capacity
  AirMassFlow = Node(InletNode)%MassFlowRate

  Furnace(FurnaceNum)%MdotFurnace = AirMassFlow

  QTotUnitOut = AirMassFlow * (Node(OutletNode)%Enthalpy - Node(Furnace(FurnaceNum)%NodeNumofControlledZone)%Enthalpy)

  Node(InletNode)%MassFlowRateMaxAvail = AirMassFlow
  Node(OutletNode)%MassFlowRateMaxAvail = AirMassFlow

  If(.NOT. FirstHVACIteration .AND. AirMassFlow > 0.0d0 .AND. AirLoopNumber > 0 ) THEN
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
  Furnace(FurnaceNum)%DehumidInducedHeatingDemandRate = ReheatCoilLoad
  IF(QZnReq > SmallLoad ) THEN !HEATING LOAD
     Furnace(FurnaceNum)%CoolingCoilSensDemand = 0.0d0
     Furnace(FurnaceNum)%HeatingCoilSensDemand = QZnReq
  ELSE
     Furnace(FurnaceNum)%CoolingCoilSensDemand = ABS(QZnReq)
     Furnace(FurnaceNum)%HeatingCoilSensDemand = 0.0d0
  END IF

  Furnace(FurnaceNum)%CompPartLoadRatio = SaveCompressorPLR
  IF (Furnace(FurnaceNum)%OpMode .EQ. CycFanCycCoil) THEN
    If (SupHeaterLoad >0.0d0) Then
      Furnace(FurnaceNum)%FanPartLoadRatio = 1.0d0
    Else
      If (SpeedNum .LT. 2) Then
        Furnace(FurnaceNum)%FanPartLoadRatio = PartLoadFrac
      Else
        Furnace(FurnaceNum)%FanPartLoadRatio = 1.0d0
      End If
    End If
  ELSE
    IF (UnitOn) THEN
      Furnace(FurnaceNum)%FanPartLoadRatio = 1.0d0
    ELSE
      If (SpeedNum .LT. 2) Then
        Furnace(FurnaceNum)%FanPartLoadRatio = PartLoadFrac
      Else
        Furnace(FurnaceNum)%FanPartLoadRatio = 1.0d0
      End If
    END IF
  END IF


  RETURN
END SUBROUTINE SimVariableSpeedHP
!******************************************************************************

SUBROUTINE ControlVSHPOutput(FurnaceNum,FirstHVACIteration,CompOp,OpMode,QZnReq,QLatReq,ZoneNum,SpeedNum,  &
                             SpeedRatio,PartLoadFrac,OnOffAirFlowRatio,SupHeaterLoad)

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
  USE DataEnvironment,           ONLY: OutDryBulbTemp

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT   (IN)  :: FurnaceNum         ! Unit index of engine driven heat pump
  LOGICAL, INTENT   (IN)  :: FirstHVACIteration ! flag for 1st HVAC iteration in the time step
  INTEGER, INTENT   (IN)  :: CompOp             ! compressor operation; 1=on, 0=off
  INTEGER, INTENT   (IN)  :: OpMode             ! operating mode: CycFanCycCoil | ContFanCycCoil
  INTEGER, INTENT   (OUT) :: SpeedNum           ! Speed number
  REAL(r64)   , INTENT   (INOUT)  :: QZnReq        ! cooling or heating output needed by zone [W]
  REAL(r64)   , INTENT   (INOUT)  :: QLatReq       ! latent cooling output needed by zone [W]
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
  REAL(r64)          :: LatOutput     ! latent capacity output
  REAL(r64)          :: ErrorToler    ! error tolerance
  INTEGER            :: SolFla        ! Flag of RegulaFalsi solver
  REAL(r64), DIMENSION(10) :: Par           ! Parameters passed to RegulaFalsi
  REAL(r64)          :: CpAir         ! air specific heat
  REAL(r64)          :: QCoilActual   ! coil load actually delivered returned to calling component
  INTEGER            :: i             ! Speed index
  INTEGER,SAVE       :: ErrCountCyc=0 ! Counter used to minimize the occurrence of output warnings
  INTEGER,SAVE       :: ErrCountVar=0 ! Counter used to minimize the occurrence of output warnings

  ! FLOW
  SupHeaterLoad = 0.0d0
  PartLoadFrac  = 0.0d0
  SpeedRatio    = 0.0d0
  SpeedNum = 1
  LatOutput = 0.0d0
  ErrorToler = 0.001d0 !Error tolerance for convergence from input deck

  !dehumidification load has the priority
 IF((GetCurrentScheduleValue(Furnace(FurnaceNum)%SchedPtr) .gt. 0.0d0 .and. CoolingLoad) .AND. &
       (Furnace(FurnaceNum)%Humidistat .AND. &
        Furnace(FurnaceNum)%DehumidControlType_Num == DehumidControl_CoolReheat .and. &
       (QLatReq.lt.0.0d0))) THEN
    QZnReq = 0.0d0
  ELSE
    QLatReq = 0.0d0
  END IF

  IF (GetCurrentScheduleValue(Furnace(FurnaceNum)%SchedPtr) .EQ. 0.0d0) RETURN

  ! Get result when DX coil is off
  SupHeaterLoad = 0.0d0
  CALL CalcVarSpeedHeatPump(FurnaceNum,FirstHVACIteration,CompOp,SpeedNum,SpeedRatio,PartLoadFrac,NoCompOutput, LatOutput, &
                          0.0d0, 0.0d0, OnOffAirFlowRatio,SupHeaterLoad)

  ! If cooling and NoCompOutput < QZnReq, the coil needs to be off
  ! If heating and NoCompOutput > QZnReq, the coil needs to be off
  IF ((QZnReq < (-1.d0*SmallLoad) .AND. NoCompOutput < QZnReq) .OR. (QZnReq > SmallLoad .AND. NoCompOutput > QZnReq) &
    .OR. ( (ABS(QZnReq) <=SmallLoad) .AND. (ABS(QLatReq) <= SmallLoad) ) &
    .OR. (QLatReq < (-1.d0*SmallLoad) .AND. LatOutput < QLatReq)) THEN
    RETURN
  END IF

  ! Get full load result
  PartLoadFrac  = 1.0d0
  SpeedRatio    = 1.0d0
  If (Furnace(FurnaceNum)%HeatCoolMode == HeatingMode) Then
    IF (Furnace(FurnaceNum)%FurnaceType_Num == UnitarySys_HeatCool) THEN
        SpeedNum = Furnace(FurnaceNum)%NumOfSpeedCooling
    ELSE
        SpeedNum = Furnace(FurnaceNum)%NumOfSpeedHeating
    END IF
  Else If (Furnace(FurnaceNum)%HeatCoolMode == CoolingMode) Then
    SpeedNum = Furnace(FurnaceNum)%NumOfSpeedCooling
  ELSE
    SpeedNum = 1
    PartLoadFrac  = 0.0d0
  End If

  CALL CalcVarSpeedHeatPump(FurnaceNum,FirstHVACIteration,CompOp,SpeedNum,SpeedRatio,PartLoadFrac,FullOutput, LatOutput, &
                          QZnReq, QLatReq, OnOffAirFlowRatio,SupHeaterLoad)

  IF(QLatReq < (-1.d0*SmallLoad) ) THEN !dehumidification mode
  !  ! If the QLatReq <= LatOutput the unit needs to run full out
    IF (QLatReq <= LatOutput) THEN
      PartLoadFrac = 1.0d0
      SpeedRatio   = 1.0d0
      Furnace(FurnaceNum)%CompPartLoadRatio = PartLoadFrac
      Furnace(FurnaceNum)%CompSpeedRatio = SpeedRatio
      Furnace(FurnaceNum)%CompSpeedNum = SpeedNum
      RETURN
    END IF
    ErrorToler = 0.001d0 !Error tolerance for convergence from input deck
  ELSE IF (QZnReq .LT. (-1.d0*SmallLoad)) THEN
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
      Furnace(FurnaceNum)%CompPartLoadRatio = PartLoadFrac
      Furnace(FurnaceNum)%CompSpeedRatio = SpeedRatio
      Furnace(FurnaceNum)%CompSpeedNum = SpeedNum
      RETURN
    END IF
    ErrorToler = 0.001d0 !Error tolerance for convergence from input deck
  ELSE
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
  END IF

  IF ((QZnReq .GT. SmallLoad .AND. QZnReq < FullOutput) .OR. (QZnReq .LT. (-1.d0*SmallLoad) .AND. QZnReq > FullOutput) &
     .OR. (QLatReq < (-1.d0*SmallLoad)) ) THEN

    Par(1) = FurnaceNum
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
    ! Check whether the low speed coil can meet the load or not
    CALL CalcVarSpeedHeatPump(FurnaceNum, FirstHVACIteration,CompOp,1, 0.0d0, 1.0d0,LowOutput, LatOutput,  &
                        QZnReq, QLatReq, OnOffAirFlowRatio,SupHeaterLoad)
    IF ((QZnReq .GT. SmallLoad .AND. QZnReq <= LowOutput) .OR. (QZnReq .LT. (-1.d0*SmallLoad) .AND. QZnReq >= LowOutput) &
              .OR. (QLatReq < (-1.d0*SmallLoad) .AND. QLatReq > LatOutput)) THEN
        ! Calculate the part load fraction
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
                            TRIM(Furnace(FurnaceNum)%Name))
            CALL ShowContinueErrorTimeStamp('Cycling ratio returned='//RoundSigDigits(PartLoadFrac,2))
          Else
            ErrCountCyc = ErrCountCyc+1
            CALL ShowRecurringWarningErrorAtEnd(TRIM(Furnace(FurnaceNum)%Name)//'":'//&
          ' Iteration limit warning exceeding calculating DX unit cycling ratio  continues...' &
          ,Furnace(FurnaceNum)%ErrIndexCyc , PartLoadFrac, PartLoadFrac)
          End If
        End If
      ELSE IF (SolFla == -2) THEN
        CALL ShowFatalError('VS WSHP unit cycling ratio calculation failed: cycling limits exceeded, for unit='// &
                           TRIM(Furnace(FurnaceNum)%Name))
      END IF
    Else
      ! Check to see which speed to meet the load
      PartLoadFrac = 1.0d0
      SpeedRatio = 1.0d0
      If ((QZnReq .LT. (-1.d0*SmallLoad)) .OR. (QLatReq < (-1.d0*SmallLoad))) Then ! Cooling
        DO I=2,Furnace(FurnaceNum)%NumOfSpeedCooling
          CALL CalcVarSpeedHeatPump(FurnaceNum,FirstHVACIteration,CompOp,I,SpeedRatio,PartLoadFrac,TempOutput, LatOutput,&
                              QZnReq,QLatReq, OnOffAirFlowRatio,SupHeaterLoad)

          IF(QLatReq < (-1.d0*SmallLoad)) THEN
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
        DO I=2,Furnace(FurnaceNum)%NumOfSpeedHeating
          CALL CalcVarSpeedHeatPump(FurnaceNum,FirstHVACIteration,CompOp,I,SpeedRatio,PartLoadFrac,TempOutput, LatOutput,&
               QZnReq,QLatReq,OnOffAirFlowRatio,SupHeaterLoad)
          If (QZnReq <= TempOutput) Then
            SpeedNum = I
            Exit
          End If
        END DO
      END IF
      Par(8) = SpeedNum

      IF(QLatReq < (-1.d0*SmallLoad) ) THEN !calculate latent heat residual
        Par(10) = 0.0d0
        Par(5) = QLatReq
      END IF

      CALL SolveRegulaFalsi(ErrorToler, MaxIte, SolFla, SpeedRatio, VSHPSpeedResidual, 1.0d-10, 1.0d0, Par)
      IF (SolFla == -1) THEN
        If ( .NOT. WarmupFlag) Then
          IF (ErrCountVar .eq. 0) THEN
            ErrCountVar = ErrCountVar+1
            CALL ShowWarningError('Iteration limit exceeded calculating VS WSHP unit speed ratio, for unit='// &
                            TRIM(Furnace(FurnaceNum)%Name))
            CALL ShowContinueErrorTimeStamp('Speed ratio returned=['//trim(RoundSigDigits(SpeedRatio,2))//'], Speed number =' &
                                        //trim(RoundSigDigits(SpeedNum,0)))
          Else
            ErrCountVar = ErrCountVar+1
            CALL ShowRecurringWarningErrorAtEnd(TRIM(Furnace(FurnaceNum)%Name)//'":'//&
          ' Iteration limit warning exceeding calculating DX unit speed ratio continues...' &
          ,Furnace(FurnaceNum)%ErrIndexVar, SpeedRatio, SpeedRatio)
          End If
        End If
      ELSE IF (SolFla == -2) THEN
        CALL ShowFatalError('VS WSHP unit compressor speed calculation failed: speed limits exceeded, for unit='// &
                           TRIM(Furnace(FurnaceNum)%Name))
      END IF
    End If
  End If

  ! if the heating coil cannot meet the load, trim with supplemental heater
  ! occurs with constant fan mode when compressor is on or off
  ! occurs with cycling fan mode when compressor PLR is equal to 1
  IF ((QZnReq .GT. SmallLoad .AND. QZnReq .GT. FullOutput) .AND. (Furnace(FurnaceNum)%SuppHeatCoilIndex /= 0))THEN
    PartLoadFrac  = 1.0d0
    SpeedRatio  = 1.0d0
    IF(Furnace(FurnaceNum)%NumOfSpeedHeating > 0) &
            SpeedNum = Furnace(FurnaceNum)%NumOfSpeedHeating !maximum heating speed, avoid zero for cooling only mode

    IF(OutDryBulbTemp .LE. Furnace(FurnaceNum)%MaxOATSuppHeat)THEN
      SupHeaterLoad = QZnReq - FullOutput
    ELSE
      SupHeaterLoad = 0.0d0
    END IF
    CALL CalcVarSpeedHeatPump(FurnaceNum,FirstHVACIteration,CompOp,SpeedNum,SpeedRatio,PartLoadFrac,TempOutput, LatOutput,QZnReq,  &
                        QLatReq, OnOffAirFlowRatio,SupHeaterLoad)
  END IF

! check the outlet of the supplemental heater to be lower than the maximum supplemental heater supply air temperature
  IF (Node(Furnace(FurnaceNum)%FurnaceOutletNodeNum)%Temp .GT. Furnace(FurnaceNum)%DesignMaxOutletTemp .AND. &
                                                             SupHeaterLoad .GT. 0.0d0) THEN

!   If the supply air temperature is to high, turn off the supplemental heater to recalculate the outlet temperature
    CALL CalcNonDXHeatingCoils(FurnaceNum,.TRUE.,FirstHVACIteration,0.0d0,OpMode,QCoilActual)
!   If the outlet temperature is below the maximum supplemental heater supply air temperature, reduce the load passed to
!   the supplemental heater, otherwise leave the supplemental heater off. If the supplemental heater is to be turned on,
!   use the outlet conditions when the supplemental heater was off (CALL above) as the inlet conditions for the calculation
!   of supplemental heater load to just meet the maximum supply air temperature from the supplemental heater.
    IF (Node(Furnace(FurnaceNum)%FurnaceOutletNodeNum)%Temp .LT. Furnace(FurnaceNum)%DesignMaxOutletTemp) THEN
      CpAir = PsyCpAirFnWTdb(Node(Furnace(FurnaceNum)%FurnaceOutletNodeNum)%HumRat, &
                             Node(Furnace(FurnaceNum)%FurnaceOutletNodeNum)%Temp)
      SupHeaterLoad = Node(Furnace(FurnaceNum)%FurnaceInletNodeNum)%MassFlowRate * CpAir * &
                      (Furnace(FurnaceNum)%DesignMaxOutletTemp - Node(Furnace(FurnaceNum)%FurnaceOutletNodeNum)%Temp)

    ELSE
      SupHeaterLoad = 0.0d0
    END IF
  END IF

!prepare module level output
  Furnace(FurnaceNum)%CompPartLoadRatio = PartLoadFrac
  Furnace(FurnaceNum)%CompSpeedRatio = SpeedRatio
  Furnace(FurnaceNum)%CompSpeedNum = SpeedNum
  Furnace(FurnaceNum)%CoolingCoilLatentDemand = ABS(QLatReq)

  IF (Furnace(FurnaceNum)%OpMode .EQ. ContFanCycCoil) THEN
    Furnace(FurnaceNum)%FanPartLoadRatio = 1.0d0
  ELSE
    Furnace(FurnaceNum)%FanPartLoadRatio = PartLoadFrac
  END IF

  RETURN
END SUBROUTINE ControlVSHPOutput

!******************************************************************************

SUBROUTINE CalcVarSpeedHeatPump(FurnaceNum,FirstHVACIteration,CompOp,SpeedNum,SpeedRatio,  &
                          PartLoadFrac,SensibleLoadMet, LatentLoadMet, &
                          QZnReq, QLatReq, OnOffAirFlowRatio,SupHeaterLoad)
            ! SUBROUTINE INFORMATION:
            !       AUTHOR:          Bo Shen, based on HVACMultiSpeedHeatPump:CalcMSHeatPump
            !       DATE WRITTEN:    March 2012
            !       MODIFIED         na
            !       RE-ENGINEERED    na

            ! PURPOSE OF THIS SUBROUTINE:
            !  This routine will calcultes MSHP performance based on given system load

            ! METHODOLOGY EMPLOYED:
            ! na

            ! REFERENCES: na

            ! USE STATEMENTS:
USE Fans,                    ONLY: SimulateFanComponents
USE VariableSpeedCoils,   ONLY: SimVariableSpeedCoils, VarSpeedCoil
USE DataEnvironment,           ONLY: OutDryBulbTemp

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN)    :: FurnaceNum        ! Variable speed heat pump number
  LOGICAL, INTENT (IN)    :: FirstHVACIteration   ! Flag for 1st HVAC iteration
  INTEGER, INTENT (IN)    :: SpeedNum             ! Speed number
  REAL(r64)   , INTENT (IN)    :: SpeedRatio           ! Compressor speed ratio
  REAL(r64)   , INTENT (IN)    :: PartLoadFrac         ! Compressor part load fraction
  REAL(r64), Intent(OUT)   :: SensibleLoadMet    ! Sensible cooling load met (furnace outlet with respect to control zone temp)
  REAL(r64), Intent(OUT)   :: LatentLoadMet ! Latent cooling load met (furnace outlet with respect to control zone humidity ratio)
  REAL(r64)   , INTENT (IN)    :: QZnReq               ! Zone load (W)
  REAL(r64)   , INTENT (IN)    :: QLatReq              ! Zone latent load []
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
  REAL(r64)    :: AirMassFlow       ! Air mass flow rate [kg/s]
  REAL(r64)    :: SavePartloadRatio !part-load ratio
  REAL(r64)    :: SaveSpeedRatio !speed ratio
  REAL(r64)    :: QCoilActual   ! coil load actually delivered returned to calling component
  REAL(r64)    :: ErrorToler    ! supplemental heating coil convergence tollerance
  Logical      :: SuppHeatingCoilFlag !whether to turn on the supplemental heater
  REAL(r64)        :: MaxTemp             ! Maximum temperature for calculating latent load at a constant temperature
  REAL(r64)        ::HeatCoilLoad         ! REQUIRED HEAT COIL LOAD

  ! FLOW
  InletNode  = Furnace(FurnaceNum)%FurnaceInletNodeNum
  OutletNode = Furnace(FurnaceNum)%FurnaceOutletNodeNum

  HeatCoilLoad = 0.0d0
  SaveCompressorPLR = 0.0d0
  SavePartloadRatio = 0.0d0
  ErrorToler = 0.001d0
  ! Set inlet air mass flow rate based on PLR and compressor on/off air flow rates
  CALL SetVSHPAirFlow(FurnaceNum,PartLoadFrac,OnOffAirFlowRatio,SpeedNum,SpeedRatio)

  IF ((SupHeaterLoad .GT. 1.0d-10) .AND. &
        (Furnace(FurnaceNum)%FurnaceType_Num == UnitarySys_HeatCool) .AND. &
        (Furnace(FurnaceNum)%SuppHeatCoilIndex .EQ. 0))THEN
        ! ONLY HEATING COIL, NO SUPPLEMENTAL COIL, USED FOR REHEAT DURING DUHMI
    HeatCoilLoad  = Furnace(FurnaceNum)%DesignHeatingCapacity * PartLoadFrac !REHEAT IN FAN ON TIME

    IF( HeatCoilLoad  > SupHeaterLoad) HeatCoilLoad = SupHeaterLoad !HEATING COIL RUN TIME < FAN ON TIME

  ELSE IF ((QZnReq .GT. SmallLoad) .AND. &
        (Furnace(FurnaceNum)%FurnaceType_Num == UnitarySys_HeatCool))THEN
    HeatCoilLoad      = Furnace(FurnaceNum)%DesignHeatingCapacity * PartLoadFrac
  ELSE
    HeatCoilLoad      = 0.0d0
  END IF

  AirMassFlow = Node(InletNode)%MassFlowRate
  ! if blow through, simulate fan then coils
  IF (Furnace(FurnaceNum)%FanPlace .EQ. BlowThru) THEN
    CALL SimulateFanComponents(Blank,FirstHVACIteration,Furnace(FurnaceNum)%FanIndex,FanSpeedRatio)

     IF((.NOT. Furnace(FurnaceNum)%CoolingCoilUpstream) .AND. &
                (Furnace(FurnaceNum)%FurnaceType_Num == UnitarySys_HeatCool))THEN
       ! simulate furnace heating coil
        SuppHeatingCoilFlag = .FALSE.       ! if true simulates supplemental heating coil
        CALL CalcNonDXHeatingCoils(FurnaceNum,SuppHeatingCoilFlag,FirstHVACIteration,HeatCoilLoad,&
            Furnace(FurnaceNum)%OpMode,QCoilActual)
     END IF


    IF (((QZnReq .LT. (-1.d0*SmallLoad)) .AND. (OutDryBulbTemp .GT. Furnace(FurnaceNum)%MinOATCompressor)) &
        .OR. (QLatReq < (-1.d0*SmallLoad))) THEN !COOLING MODE or dehumidification mode
        Call SimVariableSpeedCoils(Blank,Furnace(FurnaceNum)%CoolingCoilIndex,&
           Furnace(FurnaceNum)%OpMode,Furnace(FurnaceNum)%MaxONOFFCyclesperHour, &
           Furnace(FurnaceNum)%HPTimeConstant,Furnace(FurnaceNum)%FanDelayTime,&
           CompOp, PartLoadFrac, OnOffAirFlowRatio,SpeedNum, SpeedRatio,QZnReq, QLatReq )

        SavePartloadRatio = PartLoadFrac
        SaveSpeedRatio = SpeedRatio

      SaveCompressorPLR = VarSpeedCoil(Furnace(FurnaceNum)%CoolingCoilIndex)%PartLoadRatio
    ELSE
        Call SimVariableSpeedCoils(Blank,Furnace(FurnaceNum)%CoolingCoilIndex,&
           Furnace(FurnaceNum)%OpMode,Furnace(FurnaceNum)%MaxONOFFCyclesperHour, &
           Furnace(FurnaceNum)%HPTimeConstant,Furnace(FurnaceNum)%FanDelayTime,&
           CompOp, 0.0d0, OnOffAirFlowRatio,1, 0.0d0,0.0d0, 0.0d0 )
    END IF

    IF(Furnace(FurnaceNum)%FurnaceType_Num /= UnitarySys_HeatCool) THEN
        IF (QZnReq .GT. SmallLoad)THEN
            Call SimVariableSpeedCoils(Blank,Furnace(FurnaceNum)%HeatingCoilIndex,&
               Furnace(FurnaceNum)%OpMode,Furnace(FurnaceNum)%MaxONOFFCyclesperHour, &
               Furnace(FurnaceNum)%HPTimeConstant,Furnace(FurnaceNum)%FanDelayTime,&
               CompOp, PartLoadFrac, OnOffAirFlowRatio,SpeedNum, SpeedRatio,QZnReq, QLatReq )

            SavePartloadRatio = PartLoadFrac
            SaveSpeedRatio = SpeedRatio

           SaveCompressorPLR = VarSpeedCoil(Furnace(FurnaceNum)%HeatingCoilIndex)%PartLoadRatio
        ELSE
            Call SimVariableSpeedCoils(Blank,Furnace(FurnaceNum)%HeatingCoilIndex,&
               Furnace(FurnaceNum)%OpMode,Furnace(FurnaceNum)%MaxONOFFCyclesperHour, &
               Furnace(FurnaceNum)%HPTimeConstant,Furnace(FurnaceNum)%FanDelayTime,&
               CompOp, 0.0d0, OnOffAirFlowRatio,1, 0.0d0,0.0d0, 0.0d0 )
        END IF
    ELSE IF( Furnace(FurnaceNum)%CoolingCoilUpstream .AND. &
                (Furnace(FurnaceNum)%FurnaceType_Num == UnitarySys_HeatCool))THEN
       ! simulate furnace heating coil
        SuppHeatingCoilFlag = .FALSE.       ! if true simulates supplemental heating coil
        CALL CalcNonDXHeatingCoils(FurnaceNum,SuppHeatingCoilFlag,FirstHVACIteration,HeatCoilLoad,&
            Furnace(FurnaceNum)%OpMode,QCoilActual)
    END IF

    ! Call twice to ensure the fan outlet conditions are updated
    CALL SimulateFanComponents(Blank,FirstHVACIteration,Furnace(FurnaceNum)%FanIndex,FanSpeedRatio)

    IF((.NOT. Furnace(FurnaceNum)%CoolingCoilUpstream) .AND. &
                (Furnace(FurnaceNum)%FurnaceType_Num == UnitarySys_HeatCool))THEN
       ! simulate furnace heating coil
        SuppHeatingCoilFlag = .FALSE.       ! if true simulates supplemental heating coil
        CALL CalcNonDXHeatingCoils(FurnaceNum,SuppHeatingCoilFlag,FirstHVACIteration,HeatCoilLoad,&
            Furnace(FurnaceNum)%OpMode,QCoilActual)
    END IF

    IF (((QZnReq .LT. (-1.d0*SmallLoad)) .AND. (OutDryBulbTemp .GT. Furnace(FurnaceNum)%MinOATCompressor)) &
           .OR. (QLatReq < (-1.d0*SmallLoad))) THEN
        Call SimVariableSpeedCoils(Blank,Furnace(FurnaceNum)%CoolingCoilIndex,&
           Furnace(FurnaceNum)%OpMode,Furnace(FurnaceNum)%MaxONOFFCyclesperHour, &
           Furnace(FurnaceNum)%HPTimeConstant,Furnace(FurnaceNum)%FanDelayTime,&
           CompOp, PartLoadFrac, OnOffAirFlowRatio,SpeedNum, SpeedRatio,QZnReq, QLatReq )

        SavePartloadRatio = PartLoadFrac
        SaveSpeedRatio = SpeedRatio
     SaveCompressorPLR = VarSpeedCoil(Furnace(FurnaceNum)%CoolingCoilIndex)%PartLoadRatio
    ELSE
        Call SimVariableSpeedCoils(Blank,Furnace(FurnaceNum)%CoolingCoilIndex,&
           Furnace(FurnaceNum)%OpMode,Furnace(FurnaceNum)%MaxONOFFCyclesperHour, &
           Furnace(FurnaceNum)%HPTimeConstant,Furnace(FurnaceNum)%FanDelayTime,&
           CompOp, 0.0d0, OnOffAirFlowRatio,1, 0.0d0,0.0d0, 0.0d0 )
    END IF

    IF(Furnace(FurnaceNum)%FurnaceType_Num /= UnitarySys_HeatCool) THEN
        IF (QZnReq .GT. SmallLoad)THEN
            Call SimVariableSpeedCoils(Blank,Furnace(FurnaceNum)%HeatingCoilIndex,&
               Furnace(FurnaceNum)%OpMode,Furnace(FurnaceNum)%MaxONOFFCyclesperHour, &
               Furnace(FurnaceNum)%HPTimeConstant,Furnace(FurnaceNum)%FanDelayTime,&
               CompOp, PartLoadFrac, OnOffAirFlowRatio,SpeedNum, SpeedRatio,QZnReq, QLatReq )

            SavePartloadRatio = PartLoadFrac
            SaveSpeedRatio = SpeedRatio
         SaveCompressorPLR = VarSpeedCoil(Furnace(FurnaceNum)%HeatingCoilIndex)%PartLoadRatio
        ELSE
            Call SimVariableSpeedCoils(Blank,Furnace(FurnaceNum)%HeatingCoilIndex,&
               Furnace(FurnaceNum)%OpMode,Furnace(FurnaceNum)%MaxONOFFCyclesperHour, &
               Furnace(FurnaceNum)%HPTimeConstant,Furnace(FurnaceNum)%FanDelayTime,&
               CompOp, 0.0d0, OnOffAirFlowRatio,1, 0.0d0,0.0d0, 0.0d0 )
        END IF
    ELSE IF( Furnace(FurnaceNum)%CoolingCoilUpstream .AND. &
                (Furnace(FurnaceNum)%FurnaceType_Num == UnitarySys_HeatCool))THEN
       ! simulate furnace heating coil
        SuppHeatingCoilFlag = .FALSE.       ! if true simulates supplemental heating coil
        CALL CalcNonDXHeatingCoils(FurnaceNum,SuppHeatingCoilFlag,FirstHVACIteration,HeatCoilLoad,&
                Furnace(FurnaceNum)%OpMode,QCoilActual)
    END IF

    !  Simulate supplemental heating coil for blow through fan
    IF(Furnace(FurnaceNum)%SuppHeatCoilIndex .GT. 0)THEN
      SuppHeatingCoilFlag = .TRUE.       ! if true simulates supplemental heating coil
      CALL CalcNonDXHeatingCoils(FurnaceNum,SuppHeatingCoilFlag,FirstHVACIteration,SupHeaterLoad,  &
         Furnace(FurnaceNum)%OpMode,QCoilActual)
    ENDIF
  ELSE ! otherwise simulate DX coils then fan then supplemental heater

     IF((.NOT. Furnace(FurnaceNum)%CoolingCoilUpstream) .AND. &
                (Furnace(FurnaceNum)%FurnaceType_Num == UnitarySys_HeatCool))THEN
       ! simulate furnace heating coil
        SuppHeatingCoilFlag = .FALSE.       ! if true simulates supplemental heating coil
        CALL CalcNonDXHeatingCoils(FurnaceNum,SuppHeatingCoilFlag,FirstHVACIteration,HeatCoilLoad,&
                Furnace(FurnaceNum)%OpMode,QCoilActual)
     END IF

    IF(((QZnReq .LT. (-1.d0*SmallLoad)) .AND. (OutDryBulbTemp .GT. Furnace(FurnaceNum)%MinOATCompressor)) &
        .OR. (QLatReq < (-1.d0*SmallLoad)))THEN
        Call SimVariableSpeedCoils(Blank,Furnace(FurnaceNum)%CoolingCoilIndex,&
           Furnace(FurnaceNum)%OpMode,Furnace(FurnaceNum)%MaxONOFFCyclesperHour, &
           Furnace(FurnaceNum)%HPTimeConstant,Furnace(FurnaceNum)%FanDelayTime,&
           CompOp, PartLoadFrac, OnOffAirFlowRatio,SpeedNum, SpeedRatio,QZnReq, QLatReq )

        SavePartloadRatio = PartLoadFrac
        SaveSpeedRatio = SpeedRatio

      SaveCompressorPLR = VarSpeedCoil(Furnace(FurnaceNum)%CoolingCoilIndex)%PartLoadRatio
    ELSE
        Call SimVariableSpeedCoils(Blank,Furnace(FurnaceNum)%CoolingCoilIndex,&
           Furnace(FurnaceNum)%OpMode,Furnace(FurnaceNum)%MaxONOFFCyclesperHour, &
           Furnace(FurnaceNum)%HPTimeConstant,Furnace(FurnaceNum)%FanDelayTime,&
           CompOp, 0.0d0, OnOffAirFlowRatio,1, 0.0d0,0.0d0, 0.0d0 )
    END IF

    IF(Furnace(FurnaceNum)%FurnaceType_Num /= UnitarySys_HeatCool) THEN
        IF(QZnReq .GT. SmallLoad)THEN
             Call SimVariableSpeedCoils(Blank,Furnace(FurnaceNum)%HeatingCoilIndex,&
               Furnace(FurnaceNum)%OpMode,Furnace(FurnaceNum)%MaxONOFFCyclesperHour, &
               Furnace(FurnaceNum)%HPTimeConstant,Furnace(FurnaceNum)%FanDelayTime,&
               CompOp, PartLoadFrac, OnOffAirFlowRatio,SpeedNum, SpeedRatio,QZnReq, QLatReq )

            SavePartloadRatio = PartLoadFrac
            SaveSpeedRatio = SpeedRatio
          SaveCompressorPLR = VarSpeedCoil(Furnace(FurnaceNum)%HeatingCoilIndex)%PartLoadRatio
        ELSE
            Call SimVariableSpeedCoils(Blank,Furnace(FurnaceNum)%HeatingCoilIndex,&
               Furnace(FurnaceNum)%OpMode,Furnace(FurnaceNum)%MaxONOFFCyclesperHour, &
               Furnace(FurnaceNum)%HPTimeConstant,Furnace(FurnaceNum)%FanDelayTime,&
               CompOp, 0.0d0, OnOffAirFlowRatio,1, 0.0d0,0.0d0, 0.0d0 )
        END IF
    ELSE IF( Furnace(FurnaceNum)%CoolingCoilUpstream .AND. &
                (Furnace(FurnaceNum)%FurnaceType_Num == UnitarySys_HeatCool))THEN
       ! simulate furnace heating coil
        SuppHeatingCoilFlag = .FALSE.       ! if true simulates supplemental heating coil
        CALL CalcNonDXHeatingCoils(FurnaceNum,SuppHeatingCoilFlag,FirstHVACIteration,HeatCoilLoad,&
                Furnace(FurnaceNum)%OpMode,QCoilActual)
    END IF

    CALL SimulateFanComponents(Blank,FirstHVACIteration,Furnace(FurnaceNum)%FanIndex,FanSpeedRatio)
    !  Simulate supplemental heating coil for draw through fan
    IF(Furnace(FurnaceNum)%SuppHeatCoilIndex .GT. 0)THEN
      SuppHeatingCoilFlag = .TRUE.       ! if true simulates supplemental heating coil
      CALL CalcNonDXHeatingCoils(FurnaceNum,SuppHeatingCoilFlag,FirstHVACIteration,SupHeaterLoad,  &
         Furnace(FurnaceNum)%OpMode,QCoilActual)
    ENDIF
  END IF

  ! If the fan runs continually do not allow coils to set OnOffFanPartLoadRatio.
  IF (Furnace(FurnaceNum)%OpMode.EQ.ContFanCycCoil)OnOffFanPartLoadFraction = 1.0d0

! Check delta T (outlet to space), if positive
 ! use space HumRat (next line), else outlet humrat (IF) so psyc routine gives good result
  MinHumRat = Node(Furnace(FurnaceNum)%NodeNumofControlledZone)%HumRat
  IF(Node(OutletNode)%Temp .LT. Node(Furnace(FurnaceNum)%NodeNumofControlledZone)%Temp ) &
    MinHumRat = Node(OutletNode)%HumRat

 ! Calculate sensible load met (at constant humidity ratio)
  SensibleLoadMet = AirMassFlow * (PsyHFnTdbW(Node(OutletNode)%Temp,MinHumRat)  &
         - PsyHFnTdbW(Node(Furnace(FurnaceNum)%NodeNumofControlledZone)%Temp,MinHumRat)) &
         - Furnace(FurnaceNum)%SenLoadLoss
  Furnace(FurnaceNum)%SensibleLoadMet = SensibleLoadMet

  IF(Furnace(FurnaceNum)%Humidistat)THEN
    MaxTemp = Node(Furnace(FurnaceNum)%NodeNumofControlledZone)%Temp
! modified, why does switching between furnace outlet and control zone temp
! cause latent load to change when latent capacity is 0 ?
!    IF(Node(FurnaceOutletNode)%Temp .GT. Node(Furnace(FurnaceNum)%NodeNumofControlledZone)%Temp ) &
!       MaxTemp = Node(FurnaceOutletNode)%Temp
!   Calculate latent load met (at constant temperature)
    LatentLoadMet = AirMassFlow * (PsyHFnTdbW(MaxTemp,Node(OutletNode)%HumRat)  &
         - PsyHFnTdbW(MaxTemp,Node(Furnace(FurnaceNum)%NodeNumofControlledZone)%HumRat)) &
         - Furnace(FurnaceNum)%LatLoadLoss
  ELSE
    LatentLoadMet = 0.0d0
  END IF
  Furnace(FurnaceNum)%LatentLoadMet = LatentLoadMet

RETURN
END SUBROUTINE CalcVarSpeedHeatPump


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
  INTEGER :: FurnaceNum       ! MSHP index
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

  FurnaceNum  = INT(Par(1))
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

  CALL CalcVarSpeedHeatPump(FurnaceNum,FirstHVACIteration,CompOp,1,0.0d0,PartLoadFrac,ZoneSensLoadMet, ZoneLatLoadMet,&
                          QZnReq, QZnLat, OnOffAirFlowRatio,SupHeaterLoad)

  ResScale = abs(LoadToBeMet)
  IF (ResScale < 100.0d0) THEN
    ResScale = 100.0d0
  ELSE
    ResScale =  LoadToBeMet
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
  INTEGER :: FurnaceNum      ! MSHP index
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

  FurnaceNum = INT(Par(1))
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

  Call CalcVarSpeedHeatPump(FurnaceNum,FirstHVACIteration,CompOp,SpeedNum,SpeedRatio,1.0d0,ZoneSensLoadMet, ZoneLatLoadMet, &
                          QZnReq, QZnLat, OnOffAirFlowRatio,SupHeaterLoad)

  ResScale = abs(LoadToBeMet)
  IF (ResScale < 100.0d0) THEN
    ResScale = 100.0d0
  ELSE
    ResScale =  LoadToBeMet
  END IF

    ! Calculate residual based on output calculation flag
  IF(Par(10) .EQ. 1.0d0) THEN
    VSHPSpeedResidual = (ZoneSensLoadMet - LoadToBeMet)/ResScale
  ELSE
    VSHPSpeedResidual = (ZoneLatLoadMet - LoadToBeMet)/ResScale
  END IF

  RETURN

END FUNCTION VSHPSpeedResidual

SUBROUTINE SetVSHPAirFlow(FurnaceNum,PartLoadRatio,OnOffAirFlowRatio,SpeedNum,SpeedRatio)

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
  USE DataHVACGlobals,            ONLY: MSHPMassFlowRateLow, MSHPMassFlowRateHigh

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN)    :: FurnaceNum      ! Unit index
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
  INTEGER             :: OutNode                          ! Outlet node number in MSHP loop

  InletNode      = Furnace(FurnaceNum)%FurnaceInletNodeNum
  OutNode = Furnace(FurnaceNum)%FurnaceOutletNodeNum

  MSHPMassFlowRateLow = 0.0d0             ! Mass flow rate at low speed
  MSHPMassFlowRateHigh = 0.0d0            ! Mass flow rate at high speed


  IF (Furnace(FurnaceNum)%OpMode .EQ. ContFanCycCoil) THEN
   CompOffMassFlow = Furnace(FurnaceNum)%IdleMassFlowRate
   CompOffFlowRatio = Furnace(FurnaceNum)%IdleSpeedRatio
  ELSE
   CompOffMassFlow = 0.0d0
   CompOffFlowRatio = 0.0d0
  END IF

  IF (HeatingLoad .AND. (Furnace(FurnaceNum)%FurnaceType_Num == UnitarySys_HeatCool))THEN
      CompOnMassFlow = Furnace(FurnaceNum)%CoolMassFlowRate(Furnace(FurnaceNum)%NumOfSpeedCooling)
      CompOnFlowRatio = Furnace(FurnaceNum)%MSCoolingSpeedRatio(Furnace(FurnaceNum)%NumOfSpeedCooling)
      MSHPMassFlowRateLow = Furnace(FurnaceNum)%CoolMassFlowRate(Furnace(FurnaceNum)%NumOfSpeedCooling)
      MSHPMassFlowRateHigh = Furnace(FurnaceNum)%CoolMassFlowRate(Furnace(FurnaceNum)%NumOfSpeedCooling)
      AverageUnitMassFlow = (PartLoadRatio * CompOnMassFlow) + ((1-PartLoadRatio) * CompOffMassFlow)
      IF(CompOffFlowRatio .GT. 0.0d0)THEN
        FanSpeedRatio = (PartLoadRatio * CompOnFlowRatio) + ((1-PartLoadRatio) * CompOffFlowRatio)
      ELSE
        FanSpeedRatio     = CompOnFlowRatio
      END IF
  ELSE
     If (.NOT. CurDeadbandOrSetback(Furnace(FurnaceNum)%ControlZoneNum) .AND. Present(SpeedNum) ) Then
        If (Furnace(FurnaceNum)%HeatCoolMode == HeatingMode) Then
          If (SpeedNum .eq. 1) Then
            CompOnMassFlow = Furnace(FurnaceNum)%HeatMassFlowRate(SpeedNum)
            CompOnFlowRatio = Furnace(FurnaceNum)%MSHeatingSpeedRatio(SpeedNum)
            MSHPMassFlowRateLow = Furnace(FurnaceNum)%HeatMassFlowRate(1)
            MSHPMassFlowRateHigh = Furnace(FurnaceNum)%HeatMassFlowRate(1)
          Else If (SpeedNum .GT. 1) Then
            CompOnMassFlow = SpeedRatio*Furnace(FurnaceNum)%HeatMassFlowRate(SpeedNum) + &
                             (1.0-SpeedRatio)*Furnace(FurnaceNum)%HeatMassFlowRate(SpeedNum-1)
            CompOnFlowRatio = SpeedRatio*Furnace(FurnaceNum)%MSHeatingSpeedRatio(SpeedNum) + &
                             (1.0-SpeedRatio)*Furnace(FurnaceNum)%MSHeatingSpeedRatio(SpeedNum-1)
            MSHPMassFlowRateLow = Furnace(FurnaceNum)%HeatMassFlowRate(SpeedNum-1)
            MSHPMassFlowRateHigh = Furnace(FurnaceNum)%HeatMassFlowRate(SpeedNum)
          End If
        Else If (Furnace(FurnaceNum)%HeatCoolMode == CoolingMode) Then
          If (SpeedNum .eq. 1) Then
            CompOnMassFlow = Furnace(FurnaceNum)%CoolMassFlowRate(SpeedNum)
            CompOnFlowRatio = Furnace(FurnaceNum)%MSCoolingSpeedRatio(SpeedNum)
            MSHPMassFlowRateLow = Furnace(FurnaceNum)%CoolMassFlowRate(1)
            MSHPMassFlowRateHigh = Furnace(FurnaceNum)%CoolMassFlowRate(1)
          Else If (SpeedNum .GT. 1) Then
            CompOnMassFlow = SpeedRatio*Furnace(FurnaceNum)%CoolMassFlowRate(SpeedNum) + &
                             (1.0-SpeedRatio)*Furnace(FurnaceNum)%CoolMassFlowRate(SpeedNum-1)
            CompOnFlowRatio = SpeedRatio*Furnace(FurnaceNum)%MSCoolingSpeedRatio(SpeedNum) + &
                             (1.0-SpeedRatio)*Furnace(FurnaceNum)%MSCoolingSpeedRatio(SpeedNum-1)
            MSHPMassFlowRateLow = Furnace(FurnaceNum)%CoolMassFlowRate(SpeedNum-1)
            MSHPMassFlowRateHigh = Furnace(FurnaceNum)%CoolMassFlowRate(SpeedNum)
          End If
        End If
      End If

      ! Set up fan flow rate during compressor off time
      If (Furnace(FurnaceNum)%OpMode .EQ. ContFanCycCoil .AND. Present(SpeedNum)) Then
        IF (Furnace(FurnaceNum)%AirFlowControl .EQ. UseCompressorOnFlow .AND. CompOnMassFlow > 0.0d0) THEN
          IF(SpeedNum == 1) THEN  !LOWEST SPEED USE IDLE FLOW
            CompOffMassFlow = Furnace(FurnaceNum)%IdleMassFlowRate
            CompOffFlowRatio = Furnace(FurnaceNum)%IdleSpeedRatio
          ELSE IF (Furnace(FurnaceNum)%LastMode .EQ. HeatingMode) THEN
            CompOffMassFlow = Furnace(FurnaceNum)%HeatMassFlowRate(SpeedNum)
            CompOffFlowRatio = Furnace(FurnaceNum)%MSHeatingSpeedRatio(SpeedNum)
          ELSE
            CompOffMassFlow = Furnace(FurnaceNum)%CoolMassFlowRate(SpeedNum)
            CompOffFlowRatio = Furnace(FurnaceNum)%MSCoolingSpeedRatio(SpeedNum)
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

  IF (GetCurrentScheduleValue(Furnace(FurnaceNum)%SchedPtr) .EQ. 0.0d0) THEN
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

  Node(OutNode)%MassFlowRate  = Node(InletNode)%MassFlowRate

!  IF(abs(Node(OutNode)%MassFlowRate - 0.435)  < 0.001) THEN
!    Node(OutNode)%MassFlowRate  = Node(InletNode)%MassFlowRate
!  END IF

  RETURN
END SUBROUTINE SetVSHPAirFlow

SUBROUTINE SetOnOffMassFlowRateVSCoil(FurnaceNum, ZoneNum, FirstHVACIteration, AirLoopNum, OnOffAirFlowRatio, OpMode,   &
                                        QZnReq, MoistureLoad, PartLoadRatio)

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
  INTEGER,   INTENT(IN)    :: FurnaceNum        ! index to furnace
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

  InNode  = Furnace(FurnaceNum)%FurnaceInletNodeNum
  OutNode = Furnace(FurnaceNum)%FurnaceOutletNodeNum

          ! FLOW:

  If (CoolingLoad) then
    Furnace(FurnaceNum)%HeatCoolMode = CoolingMode
  ELSE If (HeatingLoad) then
    Furnace(FurnaceNum)%HeatCoolMode = HeatingMode
  Else
    Furnace(FurnaceNum)%HeatCoolMode = 0
  End If

  ! Set the inlet node mass flow rate
  IF (Furnace(FurnaceNum)%OpMode .EQ. ContFanCycCoil) THEN
  ! constant fan mode
    IF ((Furnace(FurnaceNum)%HeatCoolMode == HeatingMode) .AND. .NOT. CurDeadbandOrSetback(ZoneNum)) THEN
      CompOnMassFlow = Furnace(FurnaceNum)%HeatMassFlowRate(1)
      CompOnFlowRatio = Furnace(FurnaceNum)%MSHeatingSpeedRatio(1)
      Furnace(FurnaceNum)%LastMode = HeatingMode
    ELSE IF ((Furnace(FurnaceNum)%HeatCoolMode == CoolingMode) .AND. .NOT. CurDeadbandOrSetback(ZoneNum)) THEN
      CompOnMassFlow = Furnace(FurnaceNum)%CoolMassFlowRate(1)
      CompOnFlowRatio = Furnace(FurnaceNum)%MSCoolingSpeedRatio(1)
      Furnace(FurnaceNum)%LastMode = CoolingMode
    ELSE
      CompOnMassFlow = Furnace(FurnaceNum)%IdleMassFlowRate
      CompOnFlowRatio = Furnace(FurnaceNum)%IdleSpeedRatio
    END IF
    CompOffMassFlow = Furnace(FurnaceNum)%IdleMassFlowRate
    CompOffFlowRatio = Furnace(FurnaceNum)%IdleSpeedRatio
  ELSE
  ! cycling fan mode
    IF ((Furnace(FurnaceNum)%HeatCoolMode == HeatingMode).AND. .NOT. CurDeadbandOrSetback(ZoneNum)) THEN
      CompOnMassFlow = Furnace(FurnaceNum)%HeatMassFlowRate(1)
      CompOnFlowRatio = Furnace(FurnaceNum)%MSHeatingSpeedRatio(1)
    ELSE IF ((Furnace(FurnaceNum)%HeatCoolMode == CoolingMode).AND. .NOT. CurDeadbandOrSetback(ZoneNum)) THEN
      CompOnMassFlow = Furnace(FurnaceNum)%CoolMassFlowRate(1)
      CompOnFlowRatio = Furnace(FurnaceNum)%MSCoolingSpeedRatio(1)
    ELSE
      CompOnMassFlow = 0.0d0
      CompOnFlowRatio = 0.0d0
    END IF
    CompOffMassFlow = 0.0d0
    CompOffFlowRatio = 0.0d0
  END IF

  ! Set the inlet node mass flow rate
  IF (GetCurrentScheduleValue(Furnace(FurnaceNum)%FanAvailSchedPtr) .gt. 0.0d0 .AND. CompOnMassFlow .NE. 0.0d0) THEN
    OnOffAirFlowRatio = 1.0d0
    IF(FirstHVACIteration)THEN
      Node(InNode)%MassFlowRate = CompOnMassFlow
      PartLoadRatio            = 0.0d0
    ELSE
      IF (Furnace(FurnaceNum)%HeatCoolMode /= 0) THEN
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
  CALL SetVSHPAirFlow(FurnaceNum,PartLoadRatio,OnOffAirFlowRatio)

END SUBROUTINE SetOnOffMassFlowRateVSCoil

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


End Module Furnaces