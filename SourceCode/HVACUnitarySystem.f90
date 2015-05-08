MODULE HVACUnitarySystem
  ! Module containing the Unitary System simulation routines
  ! AirloopHVAC:UnitarySystem
  ! Unitary System allows any coil type with fan and coils optional
  ! Unitary System model can be placed anywhere in the simulation:
  !   (air loops, outside air systems, Outdoor air units, zone equipment)
  !   ( not fully tested for all configurations)
  !
  ! Routine calling order:
  !
  !  SimUnitarySystem
  !    GetUnitarySystemInput
  !    InitUnitarySystems
  !    IF(SetPointBased Control)THEN
  !      ControlUnitarySystemToSP   ---->  SimFan (if exists and blowthru)
  !                                        UpdateUnitarySystemControl (cooling coil if exists)
  !                                        ControlCoolingSystem ---> Sim*CoolingCoil
  !                                        CalcUnitaryCoolingSystem
  !                                        UpdateUnitarySystemControl (heating coil if exists)
  !                                        ControlHeatingSystem ---> Sim*HeatingCoil
  !                                        CalcUnitaryHeatingSystem
  !                                        SimFan (if exists and drawthru)
  !                                        UpdateUnitarySystemControl (supp heating coil if exists)
  !                                        ControlSuppHeatingSystem ---> Sim*HeatingCoil
  !                                        CalcUnitarySuppSystemToSP
  !    ELSEIF(LoadBased Control)THEN
  !      ControlUnitarySystemToLoad ---->  UpdateUnitarySystemControl
  !                                        ControlUnitarySystemOutput ---> CalcUnitarySystemToLoad(PLR)
  !                                        CalcUnitarySystemToLoad(FinalPLR w/ supp heater operating)
  !    END IF
  !
  !  ReportUnitarySystem(UnitarySysNum)
  !
  ! MODULE INFORMATION:
  !       AUTHOR         Richard Raustad, FSEC
  !       DATE WRITTEN   February 2013
  !       RE-ENGINEERED  na

  ! PURPOSE OF THIS MODULE:
  ! To encapsulate the data and algorithms required to
  ! manage the Unitary System Component

  ! METHODOLOGY EMPLOYED:
  ! Calculates the part-load ratio of the HVAC system to meet the zone sensible load.
  ! IF humidity control is specified and the latent capacity at the sensible PLR is insufficient to meet the latent load,
  ! enable multimode operation and calculate a part-load ratio to meet the zone sensible load (MultiMode dehumidification control)
  ! or the zone latent load (CoolReheat dehumidification control).
  !
  ! Subroutines:
  !
  ! SimUnitarySystem - Top level simulate routine CALLed by other modules. Each child object is simulated a final time after
  !                    the part-load ratio for child components has been determined.
  !
  !  Note: A supplemental heater augments the heating capacity for both air-to-air and water-to-air heat pump systems.
  !        The supplemental heating coil may be present even if the system is not a heat pump.
  !        The supplemental heating coil is used in the unitarysystem to meet the sensible load when the
  !        primary heating coil is unable to meet the zone load.
  !
  !
  ! Dehumidificaiton control options:
  !
  ! Dehumidification Control NONE:   If a HX assisted cooling coil is selected, the HX is always active (cooling).
  !
  ! Dehumidification Control COOLREHEAT: For cooling operation, the sensible capacity is calculated to
  !                                      meet the thermostat setpoint. If a HX assisted cooling coil is selected,
  !                                      the HX is always active. If the latent load is not met by operating the
  !                                      system at the sensible PLR, a new PLR is calculated to meet the humidistat
  !                                      setpoint. The supplemental heating coil load is then calculated to meet the
  !                                      HEATING setpoint temperature.
  !
  ! Dehumidification Control MULTIMODE: For cooling operation, the sensible capacity is calculated to
  !                                     meet the thermostat setpoint. If a HX assisted cooling coil is selected,
  !                                     the HX is off for this calculation. If the latent load is not met by operating
  !                                     the system at the sensible PLR, a new PLR is calculated with the HX operating
  !                                     and the target is the zone SENSIBLE load (thermostat setpoint). Humidity is not
  !                                     controlled in this mode. No reheat coil is needed in this configuration.
  !
  !
  ! REFERENCES:

  ! OTHER NOTES:  This module is intended to allow any configuration of coil types and location. All possible configurations
  !               have not been fully tested, however, the methodology is to treat all configurations the same. No special
  !               treatment is desired (e.g., IF coiltype == X THEN do this ELSE do something else). DX coils have not been
  !               included as supplemental heating coils, however, there is no reason other than the DX coil module may not
  !               allow a heating only DX system.
  !
  ! USE STATEMENTS:
  ! Use statements for data only modules
USE DataPrecisionGlobals
USE DataLoopNode
USE DataAirloop
USE DataGlobals
USE DataHVACGlobals
USE DataInterfaces
USE DataSizing
USE DataZoneEquipment
USE DataEnvironment, ONLY: StdBaroPress,EnvironmentName,CurMnDy,OutDryBulbTemp,OutHumRat,OutBaroPress,OutWetBulbTemp,StdRhoAir

  ! Use statements for access to subroutines in other modules
USE VariableSpeedCoils, ONLY:MaxSpedLevels
USE ScheduleManager

IMPLICIT NONE         ! Enforce explicit typing of all variables

PRIVATE ! Everything private unless explicitly made public

  !MODULE PARAMETER DEFINITIONS
REAL(r64), PARAMETER :: MinAirMassFlow = 0.001d0

! Last mode of operation
INTEGER, PARAMETER :: CoolingMode = 1 ! last compressor operating mode was in cooling
INTEGER, PARAMETER :: HeatingMode = 2 ! last compressor operating mode was in heating

! Compressor operation
INTEGER, PARAMETER :: On  = 1              ! normal compressor operation
INTEGER, PARAMETER :: Off = 0              ! signal DXCoil that compressor shouldn't run

! Dehumidification control modes (DehumidControlMode)
INTEGER, PARAMETER :: DehumidControl_None       = 0
INTEGER, PARAMETER :: DehumidControl_Multimode  = 1
INTEGER, PARAMETER :: DehumidControl_CoolReheat = 2

! Coil type for SimWater and SimSteamCoil
INTEGER, PARAMETER :: CoolingCoil               = 0
INTEGER, PARAMETER :: HeatingCoil               = 1
INTEGER, PARAMETER :: SuppHeatCoil              = 2

! Supply Air Sizing Option
INTEGER, PARAMETER :: None                            = 1
INTEGER, PARAMETER :: SupplyAirFlowRate               = 2
INTEGER, PARAMETER :: FlowPerFloorArea                = 3
INTEGER, PARAMETER :: FractionOfAutosizedCoolingValue = 4
INTEGER, PARAMETER :: FractionOfAutosizedHeatingValue = 5
INTEGER, PARAMETER :: FlowPerCoolingCapacity          = 6
INTEGER, PARAMETER :: FlowPerHeatingCapacity          = 7

! Airflow control for contant fan mode
INTEGER, PARAMETER :: UseCompressorOnFlow  = 1 ! set compressor OFF air flow rate equal to compressor ON air flow rate
INTEGER, PARAMETER :: UseCompressorOffFlow = 2 ! set compressor OFF air flow rate equal to user defined value

! System Control Type
INTEGER, PARAMETER :: LoadBased     = 1 ! control system based on zone load
INTEGER, PARAMETER :: SetPointBased = 2 ! control system based on coil set point manager

CHARACTER(len=*), PARAMETER :: Blank = ' '

  ! DERIVED TYPE DEFINITIONS
TYPE DesignSpecMSHPData
  CHARACTER(len=MaxNameLength) :: Name =' '                    ! Name of the design specification MSHP
  INTEGER  :: NumOfSpeedCooling         = 0                    ! The number of speeds for cooling
  INTEGER  :: NumOfSpeedHeating         = 0                    ! The number of speeds for heating
  REAL(R64), DIMENSION(:), ALLOCATABLE :: CoolingVolFlowRatio  ! The ratio of flow to max for this speed
  REAL(R64), DIMENSION(:), ALLOCATABLE :: HeatingVolFlowRatio  ! The ratio of flow to max for this speed
END TYPE DesignSpecMSHPData

TYPE UnitarySystemData
! HVAC system specific data
  CHARACTER(len=MaxNameLength) :: UnitarySystemType  = ' '     ! Type of Unitary System
  INTEGER    :: UnitarySystemType_Num                = 0       ! integer type of Unitary System
  CHARACTER(len=MaxNameLength) :: Name               = ' '     ! Name of the Unitary System
  LOGICAL    :: HeatPump                             = .FALSE. ! TRUE if both cooling and heating coils are DX
  INTEGER    :: SysAvailSchedPtr                     = 0       ! System Availability schedule
  INTEGER    :: UnitarySystemInletNodeNum            = 0       ! Parent inlet node number
  INTEGER    :: UnitarySystemOutletNodeNum           = 0       ! Parent outlet node number
  INTEGER    :: CondenserType                        = 0       ! type of condenser (AirCooled, EvapCooled, WaterCooled)
  LOGICAL    :: AirLoopEquipment                     = .TRUE.  ! identifies that this system is part of an air loop
  INTEGER    :: ControlZoneNum                       = 0       ! Index to controlled zone
  INTEGER    :: ZoneSequenceCoolingNum               = 0       ! Index to cooling sequence/priority for this zone
  INTEGER    :: ZoneSequenceHeatingNum               = 0       ! Index to heating sequence/priority for this zone
  INTEGER    :: NodeNumofControlledZone              = 0       ! Node number of controlled zone
  INTEGER    :: ZoneInletNode                        = 0       ! Zone inlet node number in the controlled zone
  REAL(r64)  :: ControlZoneMassFlowFrac              = 0.0d0   ! Fraction of flow to control zone
  LOGICAL    :: Humidistat                           = .FALSE. ! Set to True if dehumidification control mode is set to
  REAL(r64)  :: DesignMaxOutletTemp                  = 80.0d0  ! Maximum supply air temperature from heater [C]
  INTEGER    :: CondenserNodeNum                     = 0       ! index to condenser air inlet node
  INTEGER    :: DehumidControlType_Num               = 0       ! Set to Dehumid Control None, CoolReheat or MultiMode
  INTEGER    :: AirFlowControl                       = 1       ! UseCompressorOnFlow or UseCompressorOffFlow
  INTEGER    :: ControlType                          = 0       ! Setpoint or Load based control
  LOGICAL    :: RequestAutosize                      = .FALSE. ! determines if inputs need autosizing
  LOGICAL    :: RunOnSensibleLoad                    = .TRUE.  ! logical determines if this system will run to
  LOGICAL    :: RunOnLatentLoad                      = .FALSE. ! logical determines if this system will run to
  LOGICAL    :: RunOnLatentOnlyWithSensible          = .FALSE. ! allow latent dehumidification only if sensible load exists
  INTEGER    :: DehumidificationMode                 = 0       ! Dehumidification mode for multimode coil,
                                                               ! 0=normal, 1+=enhanced dehumidification mode
  INTEGER    :: FanOpMode                            = 0       ! Fan operating mode (see parameter above)
  INTEGER    :: LastMode                             = 0       ! last mode of operation, coolingmode or heatingmode
  REAL(r64)  :: AncillaryOnPower                    = 0.0d0    ! Ancillary On-Cycle Electric Power [W]
  REAL(r64)  :: AncillaryOffPower                   = 0.0d0    ! Ancillary Off-Cycle Electric Power [W]

  CHARACTER(len=MaxNameLength) :: DesignSpecMultispeedHPType = ' '  ! Object type for specifying multispeed flow rates
  CHARACTER(len=MaxNameLength) :: DesignSpecMultispeedHPName = ' '  ! Object name for specifying multispeed flow rates

! Cooling coil specific data
  CHARACTER(len=MaxNameLength) :: CoolingCoilName   = ' '      ! coil name (eliminate after blank is accepted in CALL)
  INTEGER    :: CoolingCoilType_Num                 = 0        ! numeric coil type
  INTEGER    :: CoolingCoilIndex                    = 0        ! index to specific cooling coil
  REAL(r64)  :: DesignCoolingCapacity               = 0.0d0    ! cooling coil capacity (W)
  INTEGER    :: CoolingCoilAvailSchPtr              = 0        ! cooling coil availability schedule index
  INTEGER    :: ActualDXCoilIndexForHXAssisted      = 0        ! index to DX coil used in HX assisted object
  REAL(r64)  :: DOASDXCoolingCoilMinTout            = 0.0d0    ! DOAS DX Cooling coil outlet air minimum temperature
  LOGICAL    :: ISHundredPercentDOASDXCoil          = .FALSE.  ! logical determines if this system will run as 100% DOAS
  LOGICAL    :: CoolCoilExists                      = .FALSE.  ! True if a cooling coil is specified in the unitary system
  INTEGER    :: FrostControlStatus                  = 0        ! DOAS coil system frost control status
  INTEGER    :: CoolCoilInletNodeNum                = 0        ! Cooling coil air inlet node number
  INTEGER    :: CoolCoilOutletNodeNum               = 0        ! Cooling coil air outlet node number
  INTEGER    :: CoolCoilFluidOutletNodeNum          = 0        ! Cooling coil fluid outlet node number (from Plant Loop data)
  INTEGER    :: CoolCoilLoopNum                     = 0        ! Plant loop num of chilled water coil
  INTEGER    :: CoolCoilLoopSide                    = 0        ! Supply side or demand side
  INTEGER    :: CoolCoilBranchNum                   = 0        ! Branch of number of the cooling coil in the plant loop
  INTEGER    :: CoolCoilCompNum                     = 0        ! Comp num of the cooling coil in the plant loop
  INTEGER    :: CoolCoilFluidInletNode              = 0        ! Cooling coil fluid inlet node
  REAL(r64)  :: MaxCoolCoilFluidFlow                = Autosize ! Maximum cooling coil fluid flow for chilled water coil
  LOGICAL    :: CoolingCoilUpStream                 = .TRUE.   ! Set to true when coolign coil is upstream in the unitary system
  REAL(R64)  :: CoolCompPartLoadRatio               = 0.0d0    ! Unitary system compressor part load ratio in cooling

! Heating coil specific data
  CHARACTER(len=MaxNameLength) :: HeatingCoilName   =' '       ! coil name (eliminate after blank is accepted in CALL)
  INTEGER    :: HeatingCoilType_Num                 = 0        ! numeric coil type
  INTEGER    :: HeatingCoilIndex                    = 0        ! index to specific heating coil
  REAL(r64)  :: DesignHeatingCapacity               = 0.0d0    ! heating coil capacity (W)
  REAL(r64)  :: HeatingSizingRatio                  = 1.0d0    ! ratio of heating coil to cooling coil size
  LOGICAL    :: DXHeatingCoil                       = .FALSE.  ! specifies if heating coil is DX
  INTEGER    :: HeatCoilInletNodeNum                = 0        ! Heating coil air inlet node number
  INTEGER    :: HeatCoilOutletNodeNum               = 0        ! Heating coil air outlet node number
  INTEGER    :: HeatCoilFluidOutletNodeNum          = 0        ! Heating coil fluid outlet node number (from Plant Loop data)
  INTEGER    :: HeatingCoilPLFCurveIndex            = 0        ! PLF curve index (not used yet?)
  INTEGER    :: HeatingCoilAvailSchPtr              = 0        ! heating coil availability schedule index
  LOGICAL    :: HeatCoilExists                      = .FALSE.  ! True if a heating coil is specified in the unitary system
  INTEGER    :: HeatCoilLoopNum                     = 0        ! Plant loop num of hot water or steam coil
  INTEGER    :: HeatCoilLoopSide                    = 0        ! Supply side or demand side
  INTEGER    :: HeatCoilBranchNum                   = 0        ! Branch of number of the heating coil in the plant loop
  INTEGER    :: HeatCoilCompNum                     = 0        ! Comp num of the heating coil in the plant loop
  INTEGER    :: HeatCoilFluidInletNode              = 0        ! Heating coil fluid inlet node
  REAL(r64)  :: MaxHeatCoilFluidFlow                = Autosize ! Maximum heating coil fluid flow for hot water or steam coil
  REAL(R64)  :: HeatCompPartLoadRatio               = 0.0d0    ! Unitary system compressor part load ratio in heating

! Supplemental heating coil specific data
  CHARACTER(len=MaxNameLength) :: SuppHeatCoilName  = ' '      ! coil name (eliminate after blank is accepted in CALL)
  INTEGER    :: SuppHeatCoilType_Num                = 0        ! numeric coil type
  INTEGER    :: SuppHeatCoilIndex                   = 0        ! index to specific supplemental heating coil
  REAL(r64)  :: DesignSuppHeatingCapacity           = 0.0d0    ! supplemental heating coil capacity (W)
  INTEGER    :: SuppCoilFluidInletNode              = 0        ! supplemental heating coil water/steam inlet node
  INTEGER    :: SuppCoilFluidOutletNodeNum          = 0        ! Supplemental coil fluid outlet node number (from Plant Loop data)
  INTEGER    :: SuppCoilAirInletNode                = 0        ! supplemental heating coil air inlet node
  INTEGER    :: SuppCoilAirOutletNode               = 0        ! supplemental heating coil air outlet node
  INTEGER    :: SuppCoilAvailSchPtr                 = 0        ! supplemental heating coil availability schedule index
  REAL(r64)  :: MaxSuppCoilFluidFlow                = Autosize ! supplemental heating coil maximum water/steam flow rate (m3/s)
  REAL(r64)  :: MaxOATSuppHeat                      = 21.0d0   ! Maximum outdoor dry-bulb temperature for supplemental coil [C]
  LOGICAL    :: SuppCoilExists                      = .FALSE.  ! True if a supp coil is specified in the unitary system
  INTEGER    :: SuppCoilLoopNum                     = 0        ! Plant loop num of supplemental coil
  INTEGER    :: SuppCoilLoopSide                    = 0        ! Supply side or demand side
  INTEGER    :: SuppCoilBranchNum                   = 0        ! Branch of number of the supplemental coil in the plant loop
  INTEGER    :: SuppCoilCompNum                     = 0        ! Comp num of the supplemental coil in the plant loop

! fan specific data
  INTEGER    :: FanType_Num                         = 0        ! Fan type num i.e. OnOff, ConstVol, VAV
  INTEGER    :: FanIndex                            = 0        ! index of fan of a particular type
  REAL(r64)  :: ActualFanVolFlowRate                = 0.0d0    ! Actual or design fan volume flow rate
  INTEGER    :: FanOpModeSchedPtr                   = 0        ! fan operating mode schedule pointer
  INTEGER    :: FanAvailSchedPtr                    = 0        ! fan availability schedule pointer
  INTEGER    :: FanPlace                            = 0        ! Blow through or DrawThrough Fan
  LOGICAL    :: FanExists                           = .FALSE.  ! True if a fan is specified in the unitary system
  REAL(r64)  :: FanDelayTime                        = 0.0d0    ! Fan delay time, time delay for the HP's fan to
                                                               ! shut off after compressor cycle off [s]
! air flow variables
  REAL(r64)  :: MaxCoolAirVolFlow                   = 0.0d0    ! Maximum coil air volumetric flow for cooling [m3/s]
  REAL(r64)  :: MaxHeatAirVolFlow                   = 0.0d0    ! Maximum coil air volumetric flow for heating [m3/s]
  REAL(r64)  :: MaxNoCoolHeatAirVolFlow             = 0.0d0    ! Maximum coil air volumetric flow for no cooling or heating [m3/s]
  REAL(r64)  :: DesignFanVolFlowRate                = 0.0d0    ! Design fan volume flow rate [m3/s]
  REAL(r64)  :: DesignMassFlowRate                  = 0.0d0    ! Design mass flow rate [m3/s]
  REAL(r64)  :: MaxCoolAirMassFlow                  = 0.0d0    ! Maximum coil air mass flow for cooling [kg/s]
  REAL(r64)  :: MaxHeatAirMassFlow                  = 0.0d0    ! Maximum coil air mass flow for heating [kg/s]
  REAL(r64)  :: MaxNoCoolHeatAirMassFlow            = 0.0d0    ! Maximum coil air mass flow for no cooling or heating [kg/s]
  INTEGER    :: CoolingSAFMethod                    = 0        ! Supply air flow method for cooling
  INTEGER    :: HeatingSAFMethod                    = 0        ! Supply air flow method for heating
  INTEGER    :: NoCoolHeatSAFMethod                 = 0        ! Supply air flow method for no cooling or heating

! Heat pump related specific data
  REAL(r64)  :: MinOATCompressor                    = 0.0d0    ! Minimum outdoor temperature below which compressor if off
  REAL(r64)  :: MaxONOFFCyclesperHour               = 0.0d0    ! Maximum cycling rate of unitary system [cycles/hr]
  REAL(r64)  :: HPTimeConstant                      = 0.0d0    ! Heat pump time constant [s]
  REAL(r64)  :: OnCyclePowerFraction                = 0.0d0    ! Fraction of on-cycle power use [~]
  REAL(r64)  :: DesignHRWaterVolumeFlow             = 0.0d0    ! Design water volume flow rate through heat recovery loop [m3/s]
  REAL(R64)  :: WSHPRuntimeFrac                     = 0.0d0    ! Runtime fraction of water source heat pump
  REAL(R64)  :: HeatingCoilSensDemand               = 0.0d0         ! Sensible demand on Heating Coil [W]
  REAL(R64)  :: CoolingCoilSensDemand               = 0.0d0         ! Sensible demand on Cooling Coil [W]
  REAL(R64)  :: CoolingCoilLatentDemand             = 0.0d0         ! Latent demand on Cooling Coil [W]

! Heat recovery related specific data
  INTEGER    :: HeatRecoveryInletNodeNum            = 0        ! Node number on heat recovery water inlet
  INTEGER    :: HeatRecoveryOutletNodeNum           = 0        ! Node number on heat recovery water outlet
  LOGICAL    :: HeatRecActive                       = .FALSE.  ! True when entered Heat Rec Vol Flow Rate > 0
  REAL(r64)  :: DesignHeatRecMassFlowRate           = 0.0d0    ! Design water mass flow rate through heat recovery loop [kg/s]
  REAL(r64)  :: MaxHROutletWaterTemp                = 0.0d0    ! Maximum outlet water temperature for heat recovery [C]
  INTEGER    :: HRLoopNum                           = 0        ! plant loop number for heat recovery
  INTEGER    :: HRLoopSideNum                       = 0        ! Plant loop side (supply or demand) for heat recovery
  INTEGER    :: HRBranchNum                         = 0        ! plant loop branch for heat recovery
  INTEGER    :: HRCompNum                           = 0        ! plant loop component for heat recovery

! set point based control varibles
  INTEGER    :: SystemHeatControlNodeNum            = 0        ! the node number of the node with the setpoint
  INTEGER    :: SystemCoolControlNodeNum            = 0        ! the node number of the node with the setpoint
  INTEGER    :: SuppHeatControlNodeNum              = 0        ! the node number of the node with the setpoint
  REAL(r64)  :: DesiredOutletTemp                   = 0.0d0    ! the setpoint temperature at the unit outlet node
  REAL(r64)  :: DesiredOutletHumRat                 = 1.0d0    ! the setpoint humidity ratio at the unit outlet node

! operational system variables
  REAL(r64)  :: CoolingPartLoadFrac                 = 0.0d0    ! part load cooling fraction for current timestep
  REAL(r64)  :: HeatingPartLoadFrac                 = 0.0d0    ! part load heating fraction for current timestep
  REAL(r64)  :: SuppHeatPartLoadFrac                = 0.0d0    ! part load supp heating fraction for current timestep
  REAL(r64)  :: SupHeaterLoad                       = 0.0d0    ! Supplemental Heat Load for current timestep
  REAL(r64)  :: SenLoadLoss                         = 0.0d0    ! Air distribution system sensible loss [W]
  REAL(r64)  :: LatLoadLoss                         = 0.0d0    ! Air distribution system latent loss [W]
  REAL(r64)  :: SensibleLoadMet                     = 0.0d0    ! System sensible load [W]
  REAL(r64)  :: LatentLoadMet                       = 0.0d0    ! System latent load [W]
  LOGICAL    :: InitHeatPump                        = .TRUE.   ! Heat pump initialization flag (for error reporting)
  INTEGER    :: WaterCyclingMode                    = 0        ! Heat Pump Coil water flow mode; See def in DataHVACGlobals,
                                                               ! 1=water cycling, 2=water constant, 3=water constant on demand

! start of additional varibles for variable speed water source heat pump
  INTEGER   :: HeatCoolMode                         = 0        ! System operating mode (0 = floating, 1 = cooling, 2 = heating)
  INTEGER   :: NumOfSpeedCooling                    = 0        ! The number of speeds for cooling
  INTEGER   :: NumOfSpeedHeating                    = 0        ! The number of speeds for heating
  REAL(r64) :: IdleSpeedRatio                       = 0        ! idle air fan ratio
  REAL(r64) :: IdleVolumeAirRate                    = 0        ! idle air flow rate [m3/s]
  REAL(r64) :: IdleMassFlowRate                     = 0        ! idle air flow rate [kg/s]
  LOGICAL   :: CheckFanFlow                         = .TRUE.   ! Supply airflow check
  REAL(r64), DIMENSION(:), ALLOCATABLE :: HeatVolumeFlowRate   ! Supply air volume flow rate during heating operation
  REAL(r64), DIMENSION(:), ALLOCATABLE :: HeatMassFlowRate     ! Supply air mass flow rate during heating operation
  REAL(r64), DIMENSION(:), ALLOCATABLE :: CoolVolumeFlowRate   ! Supply air volume flow rate during cooling operation
  REAL(r64), DIMENSION(:), ALLOCATABLE :: CoolMassFlowRate     ! Supply air mass flow rate during cooling operation
  REAL(r64), DIMENSION(:), ALLOCATABLE :: MSHeatingSpeedRatio  ! Fan speed ratio in heating mode
  REAL(r64), DIMENSION(:), ALLOCATABLE :: MSCoolingSpeedRatio  ! Fan speed ratio in cooling mode
  REAL(r64) :: NoHeatCoolSpeedRatio                 = 1.0d0    ! Fan speed ratio when no cooling or heating
  INTEGER   :: DesignSpecMSHPIndex                  = 0        ! Index to design specification multispeed heat pump object
  LOGICAL   :: MultiSpeedCoolingCoil                = .FALSE.  ! TRUE when cooling coil multispeed
  LOGICAL   :: MultiSpeedHeatingCoil                = .FALSE.  ! TRUE when heating coil multispeed
  LOGICAL   :: VarSpeedCoolingCoil                  = .FALSE.  ! TRUE when cooling coil variable speed
  LOGICAL   :: VarSpeedHeatingCoil                  = .FALSE.  ! TRUE when heating coil variable speed
  INTEGER   :: CoolingSpeedNum                      = 0        ! speed number for multispeed cooling coils types
  INTEGER   :: HeatingSpeedNum                      = 0        ! speed number for multispeed heating coils types
  REAL(r64) :: CoolingSpeedRatio                    = 1.0d0    ! current compressor speed ratio (variable speed)
  REAL(r64) :: CoolingFanSpeedRatio                 = 1.0d0    ! current fan speed ratio
  REAL(r64) :: HeatingSpeedRatio                    = 1.0d0    ! current compressor speed ratio (variable speed)
  REAL(r64) :: HeatingFanSpeedRatio                 = 1.0d0    ! current fan speed ratio
  REAL(r64) :: CoolingCycRatio                      = 0.0d0    ! cycling part load ratio (variable speed)
  REAL(r64) :: HeatingCycRatio                      = 0.0d0    ! cycling part load ratio (variable speed)
 ! end of additional variables for variable speed water source heat pump

! Report Varibles
  REAL(r64)    :: PartLoadFrac                     = 0.0d0     ! part load fraction for current time step (single speed)
  REAL(R64)    :: FanPartLoadRatio                 = 0.0d0     ! Unitary system fan part load ratio
  REAL(R64)    :: CompPartLoadRatio                = 0.0d0     ! Unitary system compressor part load ratio
  REAL(R64)    :: ElecPower                        = 0.0d0     ! Unitary System Electric Power
  REAL(R64)    :: ElecPowerConsumption             = 0.0d0     ! Electricity power comsumption: CondenserFan+CCHeater+Defrost+aux
  REAL(r64)    :: TotCoolEnergyRate                = 0.0d0     ! Unitary System Total Cooling Rate [W]
  REAL(r64)    :: SensCoolEnergyRate               = 0.0d0     ! Unitary System Sensible Cooling Rate [W]
  REAL(r64)    :: LatCoolEnergyRate                = 0.0d0     ! Unitary System Latent Cooling Rate [W]
  REAL(r64)    :: TotHeatEnergyRate                = 0.0d0     ! Unitary System Total Heating Rate [W]
  REAL(r64)    :: SensHeatEnergyRate               = 0.0d0     ! Unitary System Sensible Heating Rate [W]
  REAL(r64)    :: LatHeatEnergyRate                = 0.0d0     ! Unitary System Latent Heating Rate [W]
  REAL(r64)    :: TotalAuxElecPower                = 0.0d0     ! Unitary System Ancillary Electric Power [W]
  REAL(r64)    :: HeatingAuxElecConsumption        = 0.0d0     ! Unitary System Heating Ancillary Electric Energy [J]
  REAL(r64)    :: CoolingAuxElecConsumption        = 0.0d0     ! Unitary System Cooling Ancillary Electric Energy [J]
  REAL(r64)    :: HeatRecoveryRate                 = 0.0d0     ! Unitary System Heat Recovery Rate [W]
  REAL(r64)    :: HeatRecoveryEnergy               = 0.0d0     ! Unitary System Heat Recovery Energy [J]
  REAL(r64)    :: HeatRecoveryInletTemp            = 0.0d0     ! Unitary System Heat Recovery Inlet Temperature [C]
  REAL(r64)    :: HeatRecoveryOutletTemp           = 0.0d0     ! Unitary System Heat Recovery Outlet Temperature [C]
  REAL(r64)    :: HeatRecoveryMassFlowRate         = 0.0d0     ! Unitary System Heat Recovery Fluid Mass Flow Rate [kg/s]
  REAL(r64)    :: DehumidInducedHeatingDemandRate  = 0.0d0     ! Unitary System
  REAL(r64)    :: EMSSensibleZoneLoadValue         = 0.0d0     ! Value EMS is directing to use
  REAL(r64)    :: EMSMoistureZoneLoadValue         = 0.0d0     ! Value EMS is directing to use
  INTEGER      :: SpeedNum                         = 0         ! speed number of active multi- or variable-speed coil
  REAL(r64)    :: SpeedRatio                       = 0.0d0     ! current compressor speed ratio (variable speed)
  REAL(r64)    :: CycRatio                         = 0.0d0     ! cycling part load ratio (variable speed)

! Warning message variables
  INTEGER      :: HXAssistedSensPLRIter                = 0     ! used in HX Assisted calculations
  INTEGER      :: HXAssistedSensPLRIterIndex           = 0     ! used in HX Assisted calculations
  INTEGER      :: HXAssistedSensPLRFail                = 0     ! used in HX Assisted calculations
  INTEGER      :: HXAssistedSensPLRFailIndex           = 0     ! used in HX Assisted calculations
  INTEGER      :: HXAssistedSensPLRFail2               = 0     ! used in HX Assisted calculations
  INTEGER      :: HXAssistedSensPLRFailIndex2          = 0     ! used in HX Assisted calculations
  INTEGER      :: HXAssistedLatPLRIter                 = 0     ! used in HX Assisted calculations
  INTEGER      :: HXAssistedLatPLRIterIndex            = 0     ! used in HX Assisted calculations
  INTEGER      :: HXAssistedLatPLRFail                 = 0     ! used in HX Assisted calculations
  INTEGER      :: HXAssistedLatPLRFailIndex            = 0     ! used in HX Assisted calculations

  INTEGER      :: HXAssistedCRLatPLRIter               = 0     ! used in HX Assisted calculations
  INTEGER      :: HXAssistedCRLatPLRIterIndex          = 0     ! used in HX Assisted calculations
  INTEGER      :: HXAssistedCRLatPLRFail               = 0     ! used in HX Assisted calculations
  INTEGER      :: HXAssistedCRLatPLRFailIndex          = 0     ! used in HX Assisted calculations
  INTEGER      :: HXAssistedCRLatPLRFail2              = 0     ! used in HX Assisted calculations
  INTEGER      :: HXAssistedCRLatPLRFailIndex2         = 0     ! used in HX Assisted calculations

  INTEGER      :: SensPLRIter                          = 0     ! used in cool coil calculations
  INTEGER      :: SensPLRIterIndex                     = 0     ! used in cool coil calculations
  INTEGER      :: SensPLRFail                          = 0     ! used in cool coil calculations
  INTEGER      :: SensPLRFailIndex                     = 0     ! used in cool coil calculations
  INTEGER      :: LatPLRIter                           = 0     ! used in cool coil calculations
  INTEGER      :: LatPLRIterIndex                      = 0     ! used in cool coil calculations
  INTEGER      :: LatPLRFail                           = 0     ! used in cool coil calculations
  INTEGER      :: LatPLRFailIndex                      = 0     ! used in cool coil calculations

  INTEGER      :: HeatCoilSensPLRIter                  = 0     ! used in heat coil calculations
  INTEGER      :: HeatCoilSensPLRIterIndex             = 0     ! used in heat coil calculations
  INTEGER      :: HeatCoilSensPLRFail                  = 0     ! used in heat coil calculations
  INTEGER      :: HeatCoilSensPLRFailIndex             = 0     ! used in heat coil calculations

  INTEGER      :: SuppHeatCoilSensPLRIter              = 0     ! used in supp heat coil calculations
  INTEGER      :: SuppHeatCoilSensPLRIterIndex         = 0     ! used in supp heat coil calculations
  INTEGER      :: SuppHeatCoilSensPLRFail              = 0     ! used in supp heat coil calculations
  INTEGER      :: SuppHeatCoilSensPLRFailIndex         = 0     ! used in supp heat coil calculations

  INTEGER      :: DXCoilSensPLRIter                    = 0     ! used in DXCoil calculations
  INTEGER      :: DXCoilSensPLRIterIndex               = 0     ! used in DXCoil calculations
  INTEGER      :: DXCoilSensPLRFail                    = 0     ! used in DXCoil calculations
  INTEGER      :: DXCoilSensPLRFailIndex               = 0     ! used in DXCoil calculations

  INTEGER      :: MSpdSensPLRIter                      = 0     ! used in MultiSpeed calculations
  INTEGER      :: MSpdSensPLRIterIndex                 = 0     ! used in MultiSpeed calculations
  INTEGER      :: MSpdCycSensPLRIter                   = 0     ! used in MultiSpeed calculations
  INTEGER      :: MSpdCycSensPLRIterIndex              = 0     ! used in MultiSpeed calculations
  INTEGER      :: MSpdLatPLRIter                       = 0     ! used in MultiSpeed calculations
  INTEGER      :: MSpdLatPLRIterIndex                  = 0     ! used in MultiSpeed calculations
  INTEGER      :: MSpdCycLatPLRIter                    = 0     ! used in MultiSpeed calculations
  INTEGER      :: MSpdCycLatPLRIterIndex               = 0     ! used in MultiSpeed calculations

  INTEGER      :: MaxIterIndex                         = 0     ! used in PLR calculations for sensible load
  INTEGER      :: RegulaFalsIFailedIndex               = 0     ! used in PLR calculations for sensible load
  INTEGER      :: LatMaxIterIndex                      = 0     ! used in PLR calculations for moisture load
  INTEGER      :: LatRegulaFalsIFailedIndex            = 0     ! used in PLR calculations for moisture load

! EMS variables
  LOGICAL      :: DesignFanVolFlowRateEMSOverrideOn       = .FALSE. ! If true, then EMS is calling to override autosize fan flow
  LOGICAL      :: MaxHeatAirVolFlowEMSOverrideOn          = .FALSE. ! If true, then EMS is calling to override autosize fan flow
  LOGICAL      :: MaxCoolAirVolFlowEMSOverrideOn          = .FALSE. ! If true, then EMS is calling to override autosize fan flow
  LOGICAL      :: MaxNoCoolHeatAirVolFlowEMSOverrideOn    = .FALSE. ! If true, then EMS is calling to override autosize fan flow
  REAL(r64)    :: DesignFanVolFlowRateEMSOverrideValue    = 0.0d0   ! EMS value for override of fan flow rate autosize [m3/s]
  REAL(r64)    :: MaxHeatAirVolFlowEMSOverrideValue       = 0.0d0   ! EMS value for override of fan flow rate autosize [m3/s]
  REAL(r64)    :: MaxCoolAirVolFlowEMSOverrideValue       = 0.0d0   ! EMS value for override of fan flow rate autosize [m3/s]
  REAL(r64)    :: MaxNoCoolHeatAirVolFlowEMSOverrideValue = 0.0d0   ! EMS value for override of fan flow rate autosize [m3/s]
  LOGICAL      :: EMSOverrideSensZoneLoadRequest          = .FALSE. ! If true, then EMS is calling to override zone load
  LOGICAL      :: EMSOverrideMoistZoneLoadRequest         = .FALSE. ! If true, then EMS is calling to override zone load

! Staged thermostat control
  Integer      :: StageNum                             = 0     ! Stage number specified by staged thermostat
  LOGICAL      :: Staged                               = .FALSE.  ! Using Staged thermostat
  INTEGER      :: CoolCountAvail                       = 0     ! Counter used to minimize the occurrence of output warnings
  INTEGER      :: CoolIndexAvail                       = 0     ! Index used to minimize the occurrence of output warnings
  INTEGER      :: HeatCountAvail                       = 0     ! Counter used to minimize the occurrence of output warnings
  INTEGER      :: HeatIndexAvail                       = 0     ! Index used to minimize the occurrence of output warnings

END TYPE UnitarySystemData

!MODULE VARIABLE DECLARATIONS:
LOGICAL,SAVE :: GetInputFlag              = .TRUE.     ! Flag to get input only once
LOGICAL      :: EconomizerFlag            = .FALSE.    ! holds air loop economizer status
LOGICAL      :: HeatingLoad               = .FALSE.    ! True when zone needs heating
LOGICAL      :: CoolingLoad               = .FALSE.    ! True when zone needs cooling
REAL(r64)    :: MoistureLoad              = 0.0d0      ! Dehumidification Load (W)
LOGICAL      :: SuppHeatingCoilFlag       = .FALSE.    ! set to TRUE when simulating supplemental heating coil
INTEGER      :: NumUnitarySystem          = 0          ! The Number of Unitary Systems found in the Input
INTEGER      :: NumDesignSpecMultiSpeedHP = 0          ! The number of design specification objects for MSHP
REAL(r64)    :: CompOnMassFlow            = 0.0d0      ! Supply air mass flow rate w/ compressor ON [kg/s]
REAL(r64)    :: CompOffMassFlow           = 0.0d0      ! Supply air mass flow rate w/ compressor OFF [kg/s]
REAL(r64)    :: CompOnFlowRatio           = 0.0d0      ! fan flow ratio when coil on
REAL(r64)    :: CompOffFlowRatio          = 0.0d0      ! fan flow ratio when coil off
REAL(r64)    :: FanSpeedRatio             = 0.0d0      ! ratio of air flow ratio passed to fan object
REAL(r64)    :: CoolHeatPLRRat            = 1.0d0      ! ratio of cooling to heating PLR, used for cycling fan RH control
REAL(r64)    :: OnOffAirFlowRatioSave     = 0.0d0      ! Saves the OnOffAirFlowRatio calculated in RegulaFalsi calls.
REAL(R64)    :: QToCoolSetPt              = 0.0d0      ! load to cooling set point {W}
REAL(R64)    :: QToHeatSetPt              = 0.0d0      ! load to heating set point {W}
REAL(r64)    :: TempSteamIn               = 100.0d0    !  steam coil steam inlet temperature

! Allocatable types
TYPE (DesignSpecMSHPData), ALLOCATABLE, DIMENSION(:) :: DesignSpecMSHP
TYPE (UnitarySystemData),  ALLOCATABLE, DIMENSION(:) :: UnitarySystem
LOGICAL, ALLOCATABLE, DIMENSION(:)                   :: CheckEquipName
LOGICAL, ALLOCATABLE, DIMENSION(:)   :: MultiOrVarSpeedHeatCoil
LOGICAL, ALLOCATABLE, DIMENSION(:)   :: MultiOrVarSpeedCoolCoil

! Subroutine Specifications for the Module
          ! Driver/Manager Routines
PUBLIC  SimUnitarySystem

          ! Initialization routines
PRIVATE InitUnitarySystems
PRIVATE UpdateUnitarySystemControl
PRIVATE InitLoadBasedControl
PRIVATE SizeUnitarySystem

          ! Get Input routines
PRIVATE GetUnitarySystemInput

          ! Control routines to find PLR, check convergence and update nodes
PRIVATE ControlUnitarySystemtoSP   ! simulates set point based control
PRIVATE ControlUnitarySystemtoLoad ! simulates load based control
PRIVATE ControlUnitarySystemOutput ! simulate system to find operating output
PRIVATE ControlCoolingSystem       ! control cooling system to set point
PRIVATE ControlHeatingSystem       ! control heating system to set point
PRIVATE ControlSuppHeatSystem      ! control supplemental heating system to set point

          ! Calc routines to simulate each child component in order

          ! set point based calc routine
PRIVATE CalcUnitarySuppSystemtoSP
          ! Load based calc routine
PRIVATE CalcUnitarySystemToLoad

PRIVATE CalcUnitaryCoolingSystem
PRIVATE CalcUnitaryHeatingSystem
PRIVATE CalcUnitarySuppHeatingSystem
PRIVATE CalcPassiveSystem

          ! Airflow control routines
PRIVATE SetOnOffMassFlowRate  ! sets CompOnMassFlow and CompOffMassFlow
PRIVATE SetAverageAirFlow     ! sets inlet node mass flow rate based on PLR

PRIVATE SetSpeedVariables ! sets speed ratio, cycling ratio, RTF etc for different models

          ! Verify set point exists for SetPointBased control
PRIVATE CheckNodeSetPoint
          ! Heat recovery subroutine
PRIVATE UnitarySystemHeatRecovery
          ! Reporting routines for module
PRIVATE ReportUnitarySystem

          ! RegulaFalsi routines
! ** RAR I'd rather see a SELECT CASE in 1 or 2 generic routines instead of one for each coil type
PRIVATE DXCoilVarSpeedResidual         ! converges on outlet air set point temperature
PRIVATE DXCoilVarSpeedHumRatResidual   ! converges on outlet air set point humidity ratio
PRIVATE DXCoilCyclingResidual          ! converges on outlet air set point temperature
PRIVATE DXCoilCyclingHumRatResidual    ! converges on outlet air set point humidity ratio
PRIVATE DOE2DXCoilResidual             ! converges on outlet air set point temperature
PRIVATE DOE2DXCoilHumRatResidual       ! converges on outlet air set point humidity ratio
PRIVATE DXHeatingCoilResidual          ! converges on outlet air set point temperature
PRIVATE HeatingCoilVarSpeedResidual    ! converges on outlet air set point temperature
PRIVATE HeatingCoilVarSpeedCycResidual ! converges on outlet air set point temperature
PRIVATE MultiModeDXCoilResidual        ! converges on outlet air set point temperature
PRIVATE MultiModeDXCoilHumRatResidual  ! converges on outlet air set point humidity ratio
PRIVATE HXAssistedCoolCoilTempResidual ! converges on outlet air set point temperature
PRIVATE HXAssistedCoolCoilHRResidual   ! converges on outlet air set point humidity ratio
PRIVATE HotWaterHeatingCoilResidual    ! converges on outlet air set point temperature
PRIVATE GasElecHeatingCoilResidual     ! converges on outlet air set point temperature
PRIVATE SteamHeatingCoilResidual       ! converges on outlet air set point temperature
PRIVATE CoolWaterTempResidual          ! converges on outlet air set point temperature
PRIVATE CoolWaterHumRatResidual        ! converges on outlet air set point temperature

PRIVATE CalcUnitarySystemLoadResidual  ! converges on load to be met

PRIVATE CoolWatertoAirHPTempResidual   ! converges on outlet air set point temperature
PRIVATE HeatWatertoAirHPTempResidual   ! converges on outlet air set point temperature
PRIVATE HeatPumpRunFrac                ! calculates RTF based on PLR
PRIVATE SimMultiSpeedCoils             ! simulates multispeed coil types
PRIVATE SimWaterCoils                  ! simulates water coils
PRIVATE SimSteamCoils                  ! simulates steam coils
PRIVATE FrostControlSetPointLimit      ! frost control routine for 100% OA DX coils
PRIVATE GetUnitarySystemDXCoolingCoilIndex ! finds cool coil index in other UnitarySystem objects
PUBLIC  CheckUnitarySysCoilInOASysExists   ! check if UnitarySystem is on OA system
PUBLIC  GetUnitarySystemOAHeatCoolCoil     ! Logical returns if coil is in OA system

CONTAINS

! MODULE SUBROUTINES:
!*************************************************************************
SUBROUTINE SimUnitarySystem(UnitarySystemName,FirstHVACIteration,AirLoopNum,CompIndex, &
                            HeatActive,CoolActive,OAUnitNum,OAUCoilOutTemp,ZoneEquipment)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Raustad, FSEC
          !       DATE WRITTEN   February 2013
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine manages unitary system component simulation.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE General,          ONLY: TrimSigDigits
  USE DataAirLoop,      ONLY: AirLoopControlInfo
  USE InputProcessor,   ONLY: FindItemInList

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN)       :: UnitarySystemName  ! Name of Unitary System object
  LOGICAL,   INTENT(IN)              :: FirstHVACIteration ! True when first HVAC iteration
  INTEGER,   INTENT(IN)              :: AirLoopNum         ! Primary air loop number
  INTEGER,   INTENT(INOUT)           :: CompIndex          ! Index to Unitary System object
  LOGICAL,   INTENT(OUT),   OPTIONAL :: HeatActive         ! True if heat coil active
  LOGICAL,   INTENT(OUT),   OPTIONAL :: CoolActive         ! True if cool coil active
  INTEGER,   INTENT(IN),    OPTIONAL :: OAUnitNum          ! If the system is an equipment of OutdoorAirUnit
  REAL(r64), INTENT(IN),    OPTIONAL :: OAUCoilOutTemp     ! the coil inlet temperature of OutdoorAirUnit
  LOGICAL,   INTENT(IN),    OPTIONAL :: ZoneEquipment      ! TRUE if called as zone equipment

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER       :: UnitarySysNum         ! Index to AirloopHVAC:UnitarySystem object
  LOGICAL       :: HXUnitOn              ! Flag to control HX for HXAssisted Cooling Coil
  INTEGER       :: CompOn                ! Determines if compressor is on or off

  ! Obtains and Allocates unitary system related parameters from input file
  IF (GetInputFlag) THEN
    ! Get the unitary system input
    CALL GetUnitarySystemInput
    GetInputFlag=.false.
  END IF

  ! Find the correct unitary system Number
  IF (CompIndex == 0) THEN
    UnitarySysNum = FindItemInList(UnitarySystemName,UnitarySystem%Name,NumUnitarySystem)
    IF (UnitarySysNum == 0) THEN
      CALL ShowFatalError('SimDXCoolingSystem: DXUnit not found='//TRIM(UnitarySystemName))
    END IF
    CompIndex=UnitarySysNum
  ELSE
    UnitarySysNum=CompIndex
    IF (UnitarySysNum > NumUnitarySystem .or. UnitarySysNum < 1) THEN
      CALL ShowFatalError('SimUnitarySystem:  Invalid CompIndex passed='//  &
                          TRIM(TrimSigDigits(UnitarySysNum))// &
                          ', Number of Unit Systems='//TRIM(TrimSigDigits(NumUnitarySystem))//  &
                          ', Unitary System name='//TRIM(UnitarySystemName))
    END IF
    IF (CheckEquipName(UnitarySysNum)) THEN
      IF (UnitarySystemName /= UnitarySystem(UnitarySysNum)%Name) THEN
        CALL ShowFatalError('SimUnitarySystem: Invalid CompIndex passed='//  &
                            TRIM(TrimSigDigits(UnitarySysNum))// &
                            ', Unitary System name='//TRIM(UnitarySystemName)//', stored Unit Name for that index='//  &
                            TRIM(UnitarySystem(UnitarySysNum)%Name))
      END IF
      CheckEquipName(UnitarySysNum)=.false.
    END IF
  END IF

  IF(PRESENT(HeatActive))HeatActive = .FALSE.
  IF(PRESENT(CoolActive))CoolActive = .FALSE.

  FanSpeedRatio = 1.0d0
  IF(PRESENT(ZoneEquipment))THEN
    CALL InitUnitarySystems(UnitarySysNum,0,OAUnitNUm,OAUCoilOutTemp, FirstHVACIteration)
  ELSE
    CALL InitUnitarySystems(UnitarySysNum,AirLoopNum,OAUnitNUm,OAUCoilOutTemp, FirstHVACIteration)
  END IF


  HXUnitOn = .FALSE.
  SELECT CASE(UnitarySystem(UnitarySysNum)%ControlType)
    CASE(SetPointBased)
      CompOn = 1
      IF(PRESENT(ZoneEquipment))THEN
        CALL ControlUnitarySystemtoSP(UnitarySysNum,FirstHVACIteration,0,OAUCoilOutTemp,HXUnitOn)
      ELSE
        CALL ControlUnitarySystemtoSP(UnitarySysNum,FirstHVACIteration,AirLoopNum,OAUCoilOutTemp,HXUnitOn)
      END IF
    CASE(LoadBased)
      IF(PRESENT(ZoneEquipment))THEN
        CALL ControlUnitarySystemtoLoad(UnitarySysNum,FirstHVACIteration,0,CompOn,OAUCoilOutTemp,HXUnitOn)
      ELSE
        CALL ControlUnitarySystemtoLoad(UnitarySysNum,FirstHVACIteration,AirLoopNum,CompOn,OAUCoilOutTemp,HXUnitOn)
      END IF
  END SELECT

  ! Report the current output
  IF(PRESENT(ZoneEquipment))THEN
    CALL ReportUnitarySystem(UnitarySysNum, 0)
  ELSE
    CALL ReportUnitarySystem(UnitarySysNum, AirLoopNum)
  END IF

  IF(PRESENT(CoolActive))THEN
    IF(UnitarySystem(UnitarySysNum)%CoolingPartLoadFrac*REAL(CompOn,r64) > 0.0d0)CoolActive = .TRUE.
  END IF
  IF(PRESENT(HeatActive))THEN
    IF(UnitarySystem(UnitarySysNum)%HeatingPartLoadFrac*REAL(CompOn,r64) > 0.0d0 .OR. &
     UnitarySystem(UnitarySysNum)%SuppHeatPartLoadFrac*REAL(CompOn,r64) > 0.0d0)HeatActive = .TRUE.
  END IF

  ! set econo lockout flag
  ! If the sysem is not an equipment of Outdoor air unit
!  IF (AirLoopNum /=-1 .AND. ALLOCATED(AirLoopControlInfo) .AND. UnitarySystem(UnitarySysNum)%AirLoopEquipment) THEN
  IF (AirLoopNum > 0 .AND. ALLOCATED(AirLoopControlInfo) .AND. UnitarySystem(UnitarySysNum)%AirLoopEquipment) THEN

    IF ( (UnitarySystem(UnitarySysNum)%HeatCompPartLoadRatio > 0.0d0 .OR. &
          UnitarySystem(UnitarySysNum)%SpeedRatio > 0.0d0 .OR. &
          UnitarySystem(UnitarySysNum)%CycRatio > 0.0d0) .AND. &
          AirLoopControlInfo(AirLoopNum)%CanLockoutEconoWithCompressor) THEN
            AirLoopControlInfo(AirLoopNum)%ReqstEconoLockoutWithCompressor = .TRUE.
    ELSE
      AirLoopControlInfo(AirLoopNum)%ReqstEconoLockoutWithCompressor = .FALSE.
    END IF

    IF ((HeatActive) .AND. &
       (AirLoopControlInfo(AirLoopNum)%CanLockoutEconoWithCompressor .OR. &
        AirLoopControlInfo(AirLoopNum)%CanLockoutEconoWithHeating)) THEN
      AirLoopControlInfo(AirLoopNum)%ReqstEconoLockoutWithHeating = .TRUE.
    ELSE
      AirLoopControlInfo(AirLoopNum)%ReqstEconoLockoutWithHeating = .FALSE.
    END IF

  END IF

  ! Calculate heat recovery
  IF (UnitarySystem(UnitarySysNum)%HeatRecActive) THEN
    CALL UnitarySystemHeatRecovery(UnitarySysNum)
  END IF

  ! Coils should have been sized by now. Set this flag to false in case other equipment is downstream of Unitary System.
! can't do this since there are other checks that need this flag (e.g., HVACManager, line 3577)
!  AirLoopControlInfo(AirLoopNum)%UnitarySys = .FALSE.

  RETURN

END SUBROUTINE SimUnitarySystem

! Beginning of Initialization subroutines for the Module
! *****************************************************************************

SUBROUTINE InitUnitarySystems(UnitarySysNum,AirLoopNum,OAUnitNum,OAUCoilOutTemp,FirstHVACIteration)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Raustad, FSEC
          !       DATE WRITTEN   February 2013
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is for initializations of the unitary systems.

          ! METHODOLOGY EMPLOYED:
          ! Uses the status flags to trigger initializations.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataAirLoop,     ONLY: AirLoopControlInfo
  USE DataAirflowNetwork, ONLY: AirflowNetworkUnitarySystem
  USE DataPlant,       ONLY: ScanPlantLoopsForObject, TypeOf_UnitarySystemRecovery, &
                             PlantLoop, TypeOf_CoilSteamAirHeating, TypeOf_CoilWaterSimpleHeating, &
                             TypeOf_CoilWaterCooling, TypeOf_CoilWaterDetailedFlatCooling
  USE FluidProperties, ONLY: GetDensityGlycol, GetSatDensityRefrig
  USE HeatingCoils,    ONLY: SimulateHeatingCoilComponents, GetHeatingCoilCapacity=>GetCoilCapacity
  USE WaterCoils,      ONLY: GetCoilMaxWaterFlowRate, SimulateWaterCoilComponents, SetCoilDesFlow
  USE HVACHXAssistedCoolingCoil,  ONLY: GetHXDXCoilName,GetHXDXCoilIndex, GetCoilObjectTypeNum
  USE SteamCoils,      ONLY: GetCoilMaxSteamFlowRate, SimulateSteamCoilComponents, &
                             GetSteamCoilCapacity=>GetCoilCapacity
  USE PlantUtilities,  ONLY: SetComponentFlowRate, InitComponentNodes

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN)            :: UnitarySysNum       ! number of the current DX Sys being simulated
  INTEGER, INTENT (IN)            :: AirLoopNum          ! number of the current air loop being simulated
  INTEGER, INTENT (IN), OPTIONAL  :: OAUnitNum           ! number of the current Outdoor air unit being simulated
  REAL(r64), INTENT(IN), OPTIONAL :: OAUCoilOutTemp      ! the coil inlet temperature of OutdoorAirUnit
  LOGICAL, INTENT(IN)             :: FirstHVACIteration  ! True when first HVAC iteration

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  LOGICAL, ALLOCATABLE, SAVE, DIMENSION(:) :: MyEnvrnFlag              ! environment flag
  LOGICAL, ALLOCATABLE, SAVE, DIMENSION(:) :: MyPlantScanFlag          ! used for finding on heat recovery plant loop
  LOGICAL, ALLOCATABLE, SAVE, DIMENSION(:) :: MySuppCoilPlantScanFlag  ! used for finding on heat recovery plant loop
  LOGICAL, ALLOCATABLE, SAVE, DIMENSION(:) :: MySetPointCheckFlag      ! tests for set point
  LOGICAL, ALLOCATABLE, SAVE, DIMENSION(:) :: MySizingCheckFlag        ! tests for sizing
  LOGICAL,SAVE                             :: MyOneTimeFlag  = .TRUE.  ! one time flag
  CHARACTER(len=MaxNameLength) :: CoolingCoilType    =' '    ! Coil:Cooling:Water or Coil:Cooling:Water:DetailedGeometry
  CHARACTER(len=MaxNameLength) :: CoolingCoilName    =' '    ! Coil:Cooling:Water or Coil:Cooling:Water:DetailedGeometry
  CHARACTER(len=MaxNameLength) :: HeatingCoilType    =' '    ! Coil:Heating:Water or Coil:Heating:Steam
  LOGICAL             :: errFlag                 = .FALSE.   ! error flag for mining functions
  LOGICAL             :: ErrorsFound             = .FALSE.   ! error flag for mining functions
  INTEGER             :: ControlNode                         ! control node number
  INTEGER             :: OutdoorAirUnitNum                   ! "ONLY" for ZoneHVAC:OutdoorAirUnit
  INTEGER             :: SteamIndex              = 0         ! index of steam quality for steam heating coil
  INTEGER             :: TypeOfCoilWaterCooling  = 0         ! Used for simple water cool coil or detailed geometry
  INTEGER             :: TypeOfCoilWaterHeating  = 0         ! Used for simple water heat coil or steam coil
  REAL(r64)           :: OAUCoilOutletTemp       = 0.0d0     ! "ONLY" for zoneHVAC:OutdoorAirUnit [C]
  REAL(r64)           :: mdot                    = 0.0d0     ! local temporary for mass flow rate (kg/s)
  REAL(r64)           :: SteamDensity            = 0.0d0     ! density of steam at 100C, used for steam heating coils [kg/m3]
  REAL(r64)           :: CoilMaxVolFlowRate      = 0.0d0     ! coil fluid maximum volume flow rate [m3/s]
  REAL(r64)           :: QACTUAL                 = 0.0d0     ! coil actual capacity [W]
  REAL(r64)           :: rho                     = 0.0d0     ! local fluid density [kg/m3]
  REAL(r64)           :: mdotHR                  = 0.0d0     ! heat recovery mass flow rate [kg/s]
!  REAL(r64)           :: SaveMassFlow            = 0.0d0     ! saves node flow rate when checking heat coil capacity [m3/s]

IF (MyOneTimeFlag) THEN

  ALLOCATE(MyEnvrnFlag(NumUnitarySystem))
  ALLOCATE(MyPlantScanFlag(NumUnitarySystem))
  ALLOCATE(MySuppCoilPlantScanFlag(NumUnitarySystem))
  ALLOCATE(MySetPointCheckFlag(NumUnitarySystem))
  ALLOCATE(MySizingCheckFlag(NumUnitarySystem))

  MyEnvrnFlag = .TRUE.
  MyPlantScanFlag = .TRUE.
  MySuppCoilPlantScanFlag = .TRUE.
  MySetPointCheckFlag = .TRUE.
  MySizingCheckFlag = .TRUE.

  MyOneTimeFlag = .FALSE.
  AirflowNetworkUnitarySystem = .TRUE.
END IF

IF ( .NOT. SysSizingCalc .AND. MySizingCheckFlag(UnitarySysNum)) THEN
  IF(UnitarySystem(UnitarySysNum)%FanExists .AND. &
     (UnitarySystem(UnitarySysNum)%CoolCoilExists .AND. &
      (UnitarySystem(UnitarySysNum)%HeatCoilExists .OR. UnitarySystem(UnitarySysNum)%SuppCoilExists))) &
        AirLoopControlInfo(AirLoopNum)%UnitarySys = .TRUE.
  AirLoopControlInfo(AirLoopNum)%UnitarySysSimulating = .TRUE.
  CALL SizeUnitarySystem(UnitarySysNum, FirstHVACIteration, AirLoopNum)
  MySizingCheckFlag(UnitarySysNum) = .FALSE.
  IF (AirLoopNum > 0) THEN
    AirLoopControlInfo(AirLoopNum)%FanOpMode = UnitarySystem(UnitarySysNum)%FanOpMode
    AirLoopControlInfo(AirLoopNum)%CycFanSchedPtr = UnitarySystem(UnitarySysNum)%FanOpModeSchedPtr
  END IF
END IF

IF (AirLoopNum .EQ.-1) THEN ! This DX system is component of ZoneHVAC:OutdoorAirUnit
   OutdoorAirUnitNum=OAUnitNum
   OAUCoilOutletTemp=OAUCoilOutTemp
END IF

  ! Scan hot water and steam heating coil plant components for one time initializations
  IF (MyPlantScanFlag(UnitarySysNum) .AND. ALLOCATED(PlantLoop)) THEN
    IF (UnitarySystem(UnitarySysNum)%HeatRecActive) THEN
      errFlag=.FALSE.
      CALL ScanPlantLoopsForObject(UnitarySystem(UnitarySysNum)%Name, &
                                   TypeOf_UnitarySystemRecovery, &
                                   UnitarySystem(UnitarySysNum)%HRLoopNum, &
                                   UnitarySystem(UnitarySysNum)%HRLoopSideNum, &
                                   UnitarySystem(UnitarySysNum)%HRBranchNum, &
                                   UnitarySystem(UnitarySysNum)%HRCompNum, &
                                   errFlag=errFlag)
      IF (errFlag) THEN
        CALL ShowFatalError('InitUnitarySystems: Program terminated for previous conditions.')
      END IF
    END IF
    IF (UnitarySystem(UnitarySysNum)%CoolingCoilType_Num == Coil_CoolingWater .OR. &
        UnitarySystem(UnitarySysNum)%CoolingCoilType_Num == Coil_CoolingWaterDetailed .OR. &
        UnitarySystem(UnitarySysNum)%CoolingCoilType_Num == CoilWater_CoolingHXAssisted) THEN
        IF (UnitarySystem(UnitarySysNum)%CoolingCoilType_Num == Coil_CoolingWater) THEN
          TypeOfCoilWaterCooling = TypeOf_CoilWaterCooling
          CoolingCoilType = 'Coil:Cooling:Water'
          CoolingCoilName = UnitarySystem(UnitarySysNum)%CoolingCoilName
        ELSE IF(UnitarySystem(UnitarySysNum)%CoolingCoilType_Num == Coil_CoolingWaterDetailed)THEN
          TypeOfCoilWaterCooling = TypeOf_CoilWaterDetailedFlatCooling
          CoolingCoilType = 'Coil:Cooling:Water:DetailedGeometry'
          CoolingCoilName = UnitarySystem(UnitarySysNum)%CoolingCoilName
        ELSE
          TypeOfCoilWaterCooling = GetCoilObjectTypeNum(cAllCoilTypes(UnitarySystem(UnitarySysNum)%CoolingCoilType_Num), &
                                                        UnitarySystem(UnitarySysNum)%CoolingCoilName,ErrFlag,.TRUE.)
          IF(TypeOfCoilWaterCooling == Coil_CoolingWater)THEN
            TypeOfCoilWaterCooling = TypeOf_CoilWaterCooling
            CoolingCoilType = 'Coil:Cooling:Water'
          ELSE IF(TypeOfCoilWaterCooling == Coil_CoolingWaterDetailed)THEN
            TypeOfCoilWaterCooling = TypeOf_CoilWaterDetailedFlatCooling
            CoolingCoilType = 'Coil:Cooling:Water:DetailedGeometry'
          END IF
          CoolingCoilName = GetHXDXCoilName(cAllCoilTypes(UnitarySystem(UnitarySysNum)%CoolingCoilType_Num), &
                                            UnitarySystem(UnitarySysNum)%CoolingCoilName,ErrFlag)
        END IF
        errFlag=.false.
        CALL ScanPlantLoopsForObject( TRIM(CoolingCoilName), &
                                      TypeOfCoilWaterCooling , &
                                      UnitarySystem(UnitarySysNum)%CoolCoilLoopNum, &
                                      UnitarySystem(UnitarySysNum)%CoolCoilLoopSide, &
                                      UnitarySystem(UnitarySysNum)%CoolCoilBranchNum, &
                                      UnitarySystem(UnitarySysNum)%CoolCoilCompNum,   &
                                      errFlag=errFlag)
        IF (errFlag) THEN
          CALL ShowFatalError('InitUnitarySystem: Program terminated for previous conditions.')
        END IF
        UnitarySystem(UnitarySysNum)%MaxCoolCoilFluidFlow = GetCoilMaxWaterFlowRate(CoolingCoilType,  &
                                                   CoolingCoilName,ErrorsFound)

        IF(UnitarySystem(UnitarySysNum)%MaxCoolCoilFluidFlow .GT. 0.0d0)THEN
            rho = GetDensityGlycol(PlantLoop(UnitarySystem(UnitarySysNum)%CoolCoilLoopNum)%FluidName, &
                                   InitConvTemp, &
                                   PlantLoop(UnitarySystem(UnitarySysNum)%CoolCoilLoopNum)%FluidIndex, &
                                   'InitUnitarySystem')
            UnitarySystem(UnitarySysNum)%MaxCoolCoilFluidFlow = UnitarySystem(UnitarySysNum)%MaxCoolCoilFluidFlow * rho
        END IF
        ! fill outlet node for coil
        UnitarySystem(UnitarySysNum)%CoolCoilFluidOutletNodeNum =  &
            PlantLoop(UnitarySystem(UnitarySysNum)%CoolCoilLoopNum)%LoopSide(UnitarySystem(UnitarySysNum)%CoolCoilLoopSide) &
            %Branch(UnitarySystem(UnitarySysNum)%CoolCoilBranchNum)%Comp(UnitarySystem(UnitarySysNum)%CoolCoilCompNum)%NodeNumOut

    END IF
    IF (UnitarySystem(UnitarySysNum)%HeatingCoilType_Num == Coil_HeatingWater .OR. &
        UnitarySystem(UnitarySysNum)%HeatingCoilType_Num == Coil_HeatingSteam) THEN
        IF (UnitarySystem(UnitarySysNum)%HeatingCoilType_Num == Coil_HeatingWater) THEN
          TypeOfCoilWaterHeating = TypeOf_CoilWaterSimpleHeating
          HeatingCoilType = 'Coil:Heating:Water'
          CALL SetCoilDesFlow(CALLCoilTypes(UnitarySystem(UnitarySysNum)%HeatingCoilType_Num), &
                              UnitarySystem(UnitarySysNum)%HeatingCoilName, &
                              UnitarySystem(UnitarySysNum)%MaxHeatAirVolFlow,&
                              ErrorsFound)
        ELSE
          TypeOfCoilWaterHeating = TypeOf_CoilSteamAirHeating
          HeatingCoilType = 'Coil:Heating:Steam'
        END IF
        errFlag=.false.
        CALL ScanPlantLoopsForObject( UnitarySystem(UnitarySysNum)%HeatingCoilName, &
                                      TypeOfCoilWaterHeating , &
                                      UnitarySystem(UnitarySysNum)%HeatCoilLoopNum, &
                                      UnitarySystem(UnitarySysNum)%HeatCoilLoopSide, &
                                      UnitarySystem(UnitarySysNum)%HeatCoilBranchNum, &
                                      UnitarySystem(UnitarySysNum)%HeatCoilCompNum,   &
                                      errFlag=errFlag)
        IF (errFlag) THEN
          CALL ShowFatalError('InitUnitarySystem: Program terminated for previous conditions.')
        END IF
        IF (UnitarySystem(UnitarySysNum)%HeatingCoilType_Num == Coil_HeatingWater) THEN
          UnitarySystem(UnitarySysNum)%MaxHeatCoilFluidFlow = GetCoilMaxWaterFlowRate(HeatingCoilType,  &
                                                     UnitarySystem(UnitarySysNum)%HeatingCoilName,ErrorsFound)

          IF(UnitarySystem(UnitarySysNum)%MaxHeatCoilFluidFlow .GT. 0.0d0)THEN
              rho = GetDensityGlycol(PlantLoop(UnitarySystem(UnitarySysNum)%HeatCoilLoopNum)%FluidName, &
                                     InitConvTemp, &
                                     PlantLoop(UnitarySystem(UnitarySysNum)%HeatCoilLoopNum)%FluidIndex, &
                                     'InitUnitarySystem')
              UnitarySystem(UnitarySysNum)%MaxHeatCoilFluidFlow = GetCoilMaxWaterFlowRate(HeatingCoilType,  &
                                      UnitarySystem(UnitarySysNum)%HeatingCoilName,ErrorsFound) * rho
          END IF
        ELSE
          UnitarySystem(UnitarySysNum)%MaxHeatCoilFluidFlow =   &
             GetCoilMaxSteamFlowRate(UnitarySystem(UnitarySysNum)%HeatingCoilIndex,ErrorsFound)
          IF(UnitarySystem(UnitarySysNum)%MaxHeatCoilFluidFlow .GT. 0.0d0)THEN
            SteamIndex = 0 ! Function GetSatDensityRefrig will look up steam index if 0 is passed
            SteamDensity=GetSatDensityRefrig('STEAM',TempSteamIn,1.0d0,SteamIndex,'InitUnitarySystem')
            UnitarySystem(UnitarySysNum)%MaxHeatCoilFluidFlow = UnitarySystem(UnitarySysNum)%MaxHeatCoilFluidFlow * SteamDensity
          END IF
        END IF
        ! fill outlet node for coil
        UnitarySystem(UnitarySysNum)%HeatCoilFluidOutletNodeNum =  &
            PlantLoop(UnitarySystem(UnitarySysNum)%HeatCoilLoopNum)%LoopSide(UnitarySystem(UnitarySysNum)%HeatCoilLoopSide) &
            %Branch(UnitarySystem(UnitarySysNum)%HeatCoilBranchNum)%Comp(UnitarySystem(UnitarySysNum)%HeatCoilCompNum)%NodeNumOut
    END IF

    MyPlantScanFlag(UnitarySysNum) = .FALSE.

  ELSEIF (MyPlantScanFlag(UnitarySysNum) .AND. .NOT. AnyPlantInModel) THEN
    MyPlantScanFlag(UnitarySysNum) = .FALSE.
  END IF

  ! Scan Supplemental hot water and steam heating coil plant components for one time initializations
  IF (MySuppCoilPlantScanFlag(UnitarySysNum) .AND. ALLOCATED(PlantLoop)) THEN
    IF (UnitarySystem(UnitarySysNum)%SuppHeatCoilType_Num == Coil_HeatingWater) THEN
        errFlag=.false.
        CALL ScanPlantLoopsForObject( UnitarySystem(UnitarySysNum)%SuppHeatCoilName, &
                                      TypeOf_CoilWaterSimpleHeating , &
                                      UnitarySystem(UnitarySysNum)%SuppCoilLoopNum, &
                                      UnitarySystem(UnitarySysNum)%SuppCoilLoopSide, &
                                      UnitarySystem(UnitarySysNum)%SuppCoilBranchNum, &
                                      UnitarySystem(UnitarySysNum)%SuppCoilCompNum,   &
                                      errFlag=errFlag)
        CALL SetCoilDesFlow(CALLCoilTypes(UnitarySystem(UnitarySysNum)%SuppHeatCoilType_Num), &
                            UnitarySystem(UnitarySysNum)%SuppHeatCoilName, &
                            UnitarySystem(UnitarySysNum)%MaxHeatAirVolFlow,&
                            ErrorsFound)

        IF (errFlag) THEN
          CALL ShowFatalError('InitUnitarySystems: Program terminated for previous conditions.')
        END IF
        UnitarySystem(UnitarySysNum)%MaxSuppCoilFluidFlow = GetCoilMaxWaterFlowRate('Coil:Heating:Water',  &
                                                   UnitarySystem(UnitarySysNum)%SuppHeatCoilName,ErrorsFound)

        IF(UnitarySystem(UnitarySysNum)%MaxSuppCoilFluidFlow .GT. 0.0d0)THEN
            rho = GetDensityGlycol(PlantLoop(UnitarySystem(UnitarySysNum)%SuppCoilLoopNum)%FluidName, &
                                   InitConvTemp, &
                                   PlantLoop(UnitarySystem(UnitarySysNum)%SuppCoilLoopNum)%FluidIndex, &
                                   'InitUnitarySystems')
            UnitarySystem(UnitarySysNum)%MaxSuppCoilFluidFlow = GetCoilMaxWaterFlowRate('Coil:Heating:Water',  &
                                    UnitarySystem(UnitarySysNum)%SuppHeatCoilName,ErrorsFound) * rho
        END IF
        ! fill outlet node for coil
        UnitarySystem(UnitarySysNum)%SuppCoilFluidOutletNodeNum =  &
            PlantLoop(UnitarySystem(UnitarySysNum)%SuppCoilLoopNum)%LoopSide(UnitarySystem(UnitarySysNum)%SuppCoilLoopSide) &
            %Branch(UnitarySystem(UnitarySysNum)%SuppCoilBranchNum)%Comp(UnitarySystem(UnitarySysNum)%SuppCoilCompNum)%NodeNumOut

    ELSEIF (UnitarySystem(UnitarySysNum)%SuppHeatCoilType_Num == Coil_HeatingSteam) THEN
        errFlag=.false.
        CALL ScanPlantLoopsForObject(UnitarySystem(UnitarySysNum)%SuppHeatCoilName, &
                                     TypeOf_CoilSteamAirHeating , &
                                     UnitarySystem(UnitarySysNum)%SuppCoilLoopNum, &
                                     UnitarySystem(UnitarySysNum)%SuppCoilLoopSide, &
                                     UnitarySystem(UnitarySysNum)%SuppCoilBranchNum, &
                                     UnitarySystem(UnitarySysNum)%SuppCoilCompNum,   &
                                     errFlag=errFlag)
        IF (errFlag) THEN
          CALL ShowFatalError('InitUnitarySystems: Program terminated for previous conditions.')
        END IF
        UnitarySystem(UnitarySysNum)%MaxSuppCoilFluidFlow =   &
           GetCoilMaxSteamFlowRate(UnitarySystem(UnitarySysNum)%SuppHeatCoilIndex,ErrorsFound)
        IF(UnitarySystem(UnitarySysNum)%MaxSuppCoilFluidFlow .GT. 0.0d0)THEN
          SteamIndex = 0 ! Function GetSatDensityRefrig will look up steam index if 0 is passed
          SteamDensity=GetSatDensityRefrig('STEAM',TempSteamIn,1.0d0,SteamIndex,'InitUnitarySystems')
          UnitarySystem(UnitarySysNum)%MaxSuppCoilFluidFlow = UnitarySystem(UnitarySysNum)%MaxSuppCoilFluidFlow * SteamDensity
        END IF

      ! fill outlet node for coil
      UnitarySystem(UnitarySysNum)%SuppCoilFluidOutletNodeNum =  &
            PlantLoop(UnitarySystem(UnitarySysNum)%SuppCoilLoopNum)%LoopSide(UnitarySystem(UnitarySysNum)%SuppCoilLoopSide) &
            %Branch(UnitarySystem(UnitarySysNum)%SuppCoilBranchNum)%Comp(UnitarySystem(UnitarySysNum)%SuppCoilCompNum)%NodeNumOut
    END IF

    MySuppCoilPlantScanFlag(UnitarySysNum) = .FALSE.

  ELSEIF (MySuppCoilPlantScanFlag(UnitarySysNum) .AND. .NOT. AnyPlantInModel) THEN
    MySuppCoilPlantScanFlag(UnitarySysNum) = .FALSE.
  END IF

! do the Begin Environment initializations
  IF (BeginEnvrnFlag .and. MyEnvrnFlag(UnitarySysNum)) THEN
    UnitarySystem(UnitarySysNum)%DesignMassFlowRate = UnitarySystem(UnitarySysNum)%DesignFanVolFlowRate * StdRhoAir
    UnitarySystem(UnitarySysNum)%MaxCoolAirMassFlow = UnitarySystem(UnitarySysNum)%MaxCoolAirVolFlow * StdRhoAir
    UnitarySystem(UnitarySysNum)%MaxHeatAirMassFlow = UnitarySystem(UnitarySysNum)%MaxHeatAirVolFlow *  StdRhoAir
    UnitarySystem(UnitarySysNum)%MaxNoCoolHeatAirMassFlow = UnitarySystem(UnitarySysNum)%MaxNoCoolHeatAirVolFlow * StdRhoAir
    UnitarySystem(UnitarySysNum)%SenLoadLoss = 0.0d0
    IF (UnitarySystem(UnitarySysNum)%Humidistat) THEN
      UnitarySystem(UnitarySysNum)%LatLoadLoss = 0.0d0
    END IF

    IF ((UnitarySystem(UnitarySysNum)%HeatRecActive) .AND. (.NOT. MyPlantScanFlag(UnitarySysNum)) ) THEN

      rho = GetDensityGlycol(PlantLoop(UnitarySystem(UnitarySysNum)%HRLoopNum)%FluidName, &
                             60.0d0, &
                             PlantLoop(UnitarySystem(UnitarySysNum)%HRLoopNum)%FluidIndex, &
                             'InitUnitarySystems')

      UnitarySystem(UnitarySysNum)%DesignHeatRecMassFlowRate = UnitarySystem(UnitarySysNum)%DesignHRWaterVolumeFlow * rho

      CALL InitComponentNodes(0.0d0, UnitarySystem(UnitarySysNum)%DesignHeatRecMassFlowRate, &
                              UnitarySystem(UnitarySysNum)%HeatRecoveryInletNodeNum, &
                              UnitarySystem(UnitarySysNum)%HeatRecoveryOutletNodeNum, &
                              UnitarySystem(UnitarySysNum)%HRLoopNum, &
                              UnitarySystem(UnitarySysNum)%HRLoopSideNum, &
                              UnitarySystem(UnitarySysNum)%HRBranchNum, &
                              UnitarySystem(UnitarySysNum)%HRCompNum )
    END IF
!   set fluid-side hardware limits
    IF(UnitarySystem(UnitarySysNum)%CoolCoilFluidInletNode .GT. 0)THEN

      IF(UnitarySystem(UnitarySysNum)%MaxCoolCoilFluidFlow .EQ. Autosize)THEN
          ! If water coil max water flow rate is autosized, simulate once in order to mine max flow rate
        IF(UnitarySystem(UnitarySysNum)%CoolingCoilType_Num == Coil_CoolingWater) THEN
            CoolingCoilType = 'Coil:Cooling:Water'
        ELSE
            CoolingCoilType = 'Coil:Cooling:Water:DetailedGeometry'
        END IF
        CALL SimulateWaterCoilComponents(UnitarySystem(UnitarySysNum)%CoolingCoilName,FirstHVACIteration, &
                                         UnitarySystem(UnitarySysNum)%CoolingCoilIndex)
        CoilMaxVolFlowRate = GetCoilMaxWaterFlowRate(CoolingCoilType, UnitarySystem(UnitarySysNum)%CoolingCoilName, &
                                                     ErrorsFound)
        IF(CoilMaxVolFlowRate .NE. Autosize) THEN
          rho = GetDensityGlycol(PlantLoop(UnitarySystem(UnitarySysNum)%CoolCoilLoopNum)%fluidName, &
                                 InitConvTemp, &
                                 PlantLoop(UnitarySystem(UnitarySysNum)%CoolCoilLoopNum)%fluidIndex, &
                                 'InitUnitarySystem')
          UnitarySystem(UnitarySysNum)%MaxCoolCoilFluidFlow = CoilMaxVolFlowRate * rho
        END IF
      END IF

      CALL InitComponentNodes(0.0d0, UnitarySystem(UnitarySysNum)%MaxCoolCoilFluidFlow, &
                                    UnitarySystem(UnitarySysNum)%CoolCoilFluidInletNode, &
                                    UnitarySystem(UnitarySysNum)%CoolCoilFluidOutletNodeNum, &
                                    UnitarySystem(UnitarySysNum)%CoolCoilLoopNum, &
                                    UnitarySystem(UnitarySysNum)%CoolCoilLoopSide, &
                                    UnitarySystem(UnitarySysNum)%CoolCoilBranchNum, &
                                    UnitarySystem(UnitarySysNum)%CoolCoilCompNum )
    END IF
    IF(UnitarySystem(UnitarySysNum)%HeatCoilFluidInletNode .GT. 0)THEN

        IF(UnitarySystem(UnitarySysNum)%MaxHeatCoilFluidFlow .EQ. Autosize)THEN
            ! IF water coil max water flow rate is autosized, simulate once in order to mine max flow rate
            IF(UnitarySystem(UnitarySysNum)%HeatingCoilType_Num == Coil_HeatingWater) THEN
                CALL SimulateWaterCoilComponents(UnitarySystem(UnitarySysNum)%HeatingCoilName,FirstHVACIteration, &
                                                UnitarySystem(UnitarySysNum)%HeatingCoilIndex)
                CoilMaxVolFlowRate = GetCoilMaxWaterFlowRate('Coil:Heating:Water',  &
                                                             UnitarySystem(UnitarySysNum)%HeatingCoilName,ErrorsFound)
                IF(CoilMaxVolFlowRate .NE. Autosize) THEN
                    rho = GetDensityGlycol(PlantLoop(UnitarySystem(UnitarySysNum)%HeatCoilLoopNum)%fluidName, &
                                            InitConvTemp, &
                                            PlantLoop(UnitarySystem(UnitarySysNum)%HeatCoilLoopNum)%fluidIndex, &
                                            'InitUnitarySystems')
                    UnitarySystem(UnitarySysNum)%MaxHeatCoilFluidFlow = CoilMaxVolFlowRate * rho
                END IF
            END IF
            ! If steam coil max steam flow rate is autosized, simulate once in order to mine max flow rate
            IF(UnitarySystem(UnitarySysNum)%HeatingCoilType_Num == Coil_HeatingSteam) THEN
                CALL SimulateSteamCoilComponents(UnitarySystem(UnitarySysNum)%HeatingCoilName, &
                                                FirstHVACIteration,    &
                                                1.0d0, & !QCoilReq, simulate any load > 0 to get max capacity
                                                UnitarySystem(UnitarySysNum)%HeatingCoilIndex, QActual)
                CoilMaxVolFlowRate = GetCoilMaxSteamFlowRate(UnitarySystem(UnitarySysNum)%HeatingCoilIndex,ErrorsFound)
                IF(CoilMaxVolFlowRate .NE. Autosize) THEN
                   SteamIndex = 0 ! Function GetSatDensityRefrig will look up steam index if 0 is passed
                   SteamDensity=GetSatDensityRefrig('STEAM',TempSteamIn,1.0d0,SteamIndex,'InitUnitarySystems')
                   UnitarySystem(UnitarySysNum)%MaxHeatCoilFluidFlow = CoilMaxVolFlowRate * SteamDensity
                END IF
            END IF
        END IF

        CALL InitComponentNodes(0.0d0, UnitarySystem(UnitarySysNum)%MaxHeatCoilFluidFlow, &
                                        UnitarySystem(UnitarySysNum)%HeatCoilFluidInletNode, &
                                        UnitarySystem(UnitarySysNum)%HeatCoilFluidOutletNodeNum, &
                                        UnitarySystem(UnitarySysNum)%HeatCoilLoopNum, &
                                        UnitarySystem(UnitarySysNum)%HeatCoilLoopSide, &
                                        UnitarySystem(UnitarySysNum)%HeatCoilBranchNum, &
                                        UnitarySystem(UnitarySysNum)%HeatCoilCompNum )
    END IF
    IF(UnitarySystem(UnitarySysNum)%SuppCoilFluidInletNode .GT. 0)THEN
        IF(UnitarySystem(UnitarySysNum)%MaxSuppCoilFluidFlow .EQ. Autosize)THEN
            IF(UnitarySystem(UnitarySysNum)%SuppHeatCoilType_Num == Coil_HeatingWater) THEN
                ! If water coil max water flow rate is autosized, simulate once in order to mine max flow rate
                CALL SimulateWaterCoilComponents(UnitarySystem(UnitarySysNum)%SuppHeatCoilName,FirstHVACIteration, &
                                                UnitarySystem(UnitarySysNum)%SuppHeatCoilIndex)
                CoilMaxVolFlowRate = GetCoilMaxWaterFlowRate('Coil:Heating:Water',  &
                                                            UnitarySystem(UnitarySysNum)%SuppHeatCoilName,ErrorsFound)
                IF(CoilMaxVolFlowRate .NE. Autosize) THEN
                    rho = GetDensityGlycol(PlantLoop(UnitarySystem(UnitarySysNum)%SuppCoilLoopNum)%fluidName, &
                                        InitConvTemp, &
                                        PlantLoop(UnitarySystem(UnitarySysNum)%SuppCoilLoopNum)%fluidIndex, &
                                        'InitUnitarySystems')
                    UnitarySystem(UnitarySysNum)%MaxSuppCoilFluidFlow = CoilMaxVolFlowRate * rho
                END IF
            END IF
            IF(UnitarySystem(UnitarySysNum)%SuppHeatCoilType_Num == Coil_HeatingSteam) THEN
                CALL SimulateSteamCoilComponents(UnitarySystem(UnitarySysNum)%SuppHeatCoilName, &
                                                FirstHVACIteration,    &
                                                1.0d0, & !QCoilReq, simulate any load > 0 to get max capacity
                                                UnitarySystem(UnitarySysNum)%SuppHeatCoilIndex, QActual)
                CoilMaxVolFlowRate = GetCoilMaxSteamFlowRate(UnitarySystem(UnitarySysNum)%SuppHeatCoilIndex,ErrorsFound)
                IF(CoilMaxVolFlowRate .NE. Autosize) THEN
                  SteamIndex = 0 ! Function GetSatDensityRefrig will look up steam index if 0 is passed
                  SteamDensity=GetSatDensityRefrig('STEAM',TempSteamIn,1.0d0,SteamIndex,'InitUnitarySystems')
                  UnitarySystem(UnitarySysNum)%MaxSuppCoilFluidFlow = CoilMaxVolFlowRate * SteamDensity
                END IF
            END IF
            CALL InitComponentNodes(0.0d0, UnitarySystem(UnitarySysNum)%MaxSuppCoilFluidFlow, &
                                        UnitarySystem(UnitarySysNum)%SuppCoilFluidInletNode, &
                                        UnitarySystem(UnitarySysNum)%SuppCoilFluidOutletNodeNum, &
                                        UnitarySystem(UnitarySysNum)%SuppCoilLoopNum, &
                                        UnitarySystem(UnitarySysNum)%SuppCoilLoopSide, &
                                        UnitarySystem(UnitarySysNum)%SuppCoilBranchNum, &
                                        UnitarySystem(UnitarySysNum)%SuppCoilCompNum )
        END IF
    END IF
    IF(UnitarySystem(UnitarySysNum)%HeatingCoilType_Num == Coil_HeatingGas .OR. &
       UnitarySystem(UnitarySysNum)%HeatingCoilType_Num == Coil_HeatingElectric)THEN
      CALL SimulateHeatingCoilComponents(UnitarySystem(UnitarySysNum)%HeatingCoilName,FirstHVACIteration, &
                                CompIndex=UnitarySystem(UnitarySysNum)%HeatingCoilIndex, &
                                QCoilReq=1.0d0, &
                                FanOpMode=UnitarySystem(UnitarySysNum)%FanOpMode, &
                                PartLoadRatio=1.0d0)
      UnitarySystem(UnitarySysNum)%DesignHeatingCapacity =   &
             GetHeatingCoilCapacity(CALLCoilTypes(UnitarySystem(UnitarySysNum)%HeatingCoilType_Num), &
                                    UnitarySystem(UnitarySysNum)%HeatingCoilName,ErrFlag)
    END IF
    MyEnvrnFlag(UnitarySysNum) = .FALSE.
  END IF

  IF (.not. BeginEnvrnFlag) THEN
    MyEnvrnFlag(UnitarySysNum) = .TRUE.
  END IF

  !Init maximum available Heat Recovery flow rate
  IF ((UnitarySystem(UnitarySysNum)%HeatRecActive) .AND. (.NOT. MyPlantScanFlag(UnitarySysNum)) ) THEN
    IF (GetCurrentScheduleValue(UnitarySystem(UnitarySysNum)%SysAvailSchedPtr) .GT. 0.0d0) THEN
      IF (FirstHVACIteration) THEN
        mdotHR = UnitarySystem(UnitarySysNum)%DesignHeatRecMassFlowRate
      ELSE
        IF (UnitarySystem(UnitarySysNum)%HeatRecoveryMassFlowRate > 0.0d0) THEN
          mdotHR = UnitarySystem(UnitarySysNum)%HeatRecoveryMassFlowRate
        ELSE
          mdotHR = UnitarySystem(UnitarySysNum)%DesignHeatRecMassFlowRate
        END IF
      END IF
    ELSE
      mdotHR = 0.0d0
    END IF

    mdotHR = MIN(Node(UnitarySystem(UnitarySysNum)%HeatRecoveryOutletNodeNum)%MassFlowRateMaxAvail,mdotHR)
    Node(UnitarySystem(UnitarySysNum)%HeatRecoveryInletNodeNum)%MassFlowRate = mdotHR
  END IF

  ! get operating capacity of water and steam coil
  IF(FirstHVACIteration .OR. UnitarySystem(UnitarySysNum)%DehumidControlType_Num == DehumidControl_CoolReheat) THEN
    IF(UnitarySystem(UnitarySysNum)%CoolingCoilType_Num == Coil_CoolingWater .OR. &
       UnitarySystem(UnitarySysNum)%CoolingCoilType_Num == Coil_CoolingWaterDetailed) THEN
      !     set air-side and steam-side mass flow rates
      mdot = MIN(Node(UnitarySystem(UnitarySysNum)%CoolCoilFluidOutletNodeNum)%MassFlowRateMaxAvail, &
             UnitarySystem(UnitarySysNum)%MaxCoolCoilFluidFlow)
      Node(UnitarySystem(UnitarySysNum)%CoolCoilFluidInletNode)%MassFlowRate = mdot
      !     simulate water coil to find operating capacity
      CALL SimulateWaterCoilComponents(UnitarySystem(UnitarySysNum)%CoolingCoilName,FirstHVACIteration, &
                                                UnitarySystem(UnitarySysNum)%CoolingCoilIndex, QActual)
      UnitarySystem(UnitarySysNum)%DesignCoolingCapacity = QActual
    END IF ! from IF(UnitarySystem(UnitarySysNum)%CoolingCoilType_Num == Coil_CoolingWater .OR. Coil_CoolingWaterDetailed
    IF(UnitarySystem(UnitarySysNum)%HeatingCoilType_Num == Coil_HeatingWater) THEN
      !     set air-side and steam-side mass flow rates
      mdot = MIN(Node(UnitarySystem(UnitarySysNum)%HeatCoilFluidOutletNodeNum)%MassFlowRateMaxAvail, &
             UnitarySystem(UnitarySysNum)%MaxHeatCoilFluidFlow)
      Node(UnitarySystem(UnitarySysNum)%HeatCoilFluidInletNode)%MassFlowRate = mdot
      !     simulate water coil to find operating capacity
!      SaveMassFlow = Node(UnitarySystem(UnitarySysNum)%HeatCoilInletNodeNum)%MassFlowRate
!      Node(UnitarySystem(UnitarySysNum)%HeatCoilInletNodeNum)%MassFlowRate = UnitarySystem(UnitarySysNum)%MaxHeatAirMassFlow
      CALL SimulateWaterCoilComponents(UnitarySystem(UnitarySysNum)%HeatingCoilName,FirstHVACIteration, &
                                                UnitarySystem(UnitarySysNum)%HeatingCoilIndex, QActual)
      UnitarySystem(UnitarySysNum)%DesignHeatingCapacity = QActual
!      Node(UnitarySystem(UnitarySysNum)%HeatCoilInletNodeNum)%MassFlowRate = SaveMassFlow
    END IF ! from IF(UnitarySystem(UnitarySysNum)%HeatingCoilType_Num == Coil_HeatingWater) THEN

    IF(UnitarySystem(UnitarySysNum)%HeatingCoilType_Num == Coil_HeatingSteam) THEN

      !     set air-side and steam-side mass flow rates
      mdot = MIN(Node(UnitarySystem(UnitarySysNum)%HeatCoilFluidOutletNodeNum)%MassFlowRateMaxAvail, &
             UnitarySystem(UnitarySysNum)%MaxHeatCoilFluidFlow)
      Node(UnitarySystem(UnitarySysNum)%HeatCoilFluidInletNode)%MassFlowRate = mdot

!     simulate steam coil to find operating capacity
      CALL SimulateSteamCoilComponents(UnitarySystem(UnitarySysNum)%HeatingCoilName, &
                                       FirstHVACIteration,    &
                                       1.0d0, & !QCoilReq, simulate any load > 0 to get max capacity of steam coil
                                       UnitarySystem(UnitarySysNum)%HeatingCoilIndex, QActual)

      UnitarySystem(UnitarySysNum)%DesignHeatingCapacity = &
             GetSteamCoilCapacity(CALLCoilTypes(UnitarySystem(UnitarySysNum)%HeatingCoilType_Num), &
                                                UnitarySystem(UnitarySysNum)%HeatingCoilName,ErrorsFound)
    END IF ! from IF(UnitarySystem(UnitarySysNum)%HeatingCoilType_Num == Coil_HeatingSteam) THEN
    IF(UnitarySystem(UnitarySysNum)%SuppHeatCoilType_Num == Coil_HeatingWater) THEN
      !     set air-side and steam-side mass flow rates
      mdot = MIN(Node(UnitarySystem(UnitarySysNum)%SuppCoilFluidOutletNodeNum)%MassFlowRateMaxAvail, &
             UnitarySystem(UnitarySysNum)%MaxSuppCoilFluidFlow)
      Node(UnitarySystem(UnitarySysNum)%SuppCoilFluidInletNode)%MassFlowRate = mdot
!      SaveMassFlow = Node(UnitarySystem(UnitarySysNum)%SuppCoilAirInletNode)%MassFlowRate
!      Node(UnitarySystem(UnitarySysNum)%SuppCoilAirInletNode)%MassFlowRate = UnitarySystem(UnitarySysNum)%MaxHeatAirMassFlow
      !     simulate water coil to find operating capacity
      IF(mdot > 0.0d0)THEN
!        Node(UnitarySystem(UnitarySysNum)%SuppCoilAirInletNode)%MassFlowRate = &
!          UnitarySystem(UnitarySysNum)%MaxHeatAirMassFlow
        CALL SimulateWaterCoilComponents(UnitarySystem(UnitarySysNum)%SuppHeatCoilName,FirstHVACIteration, &
                                                UnitarySystem(UnitarySysNum)%SuppHeatCoilIndex, QActual)
        UnitarySystem(UnitarySysNum)%DesignSuppHeatingCapacity = QActual
      ELSE
        UnitarySystem(UnitarySysNum)%DesignSuppHeatingCapacity = 0.0d0
      END IF
!      Node(UnitarySystem(UnitarySysNum)%SuppCoilAirInletNode)%MassFlowRate = SaveMassFlow

    END IF ! from IF(UnitarySystem(UnitarySysNum)%SuppHeatCoilType_Num == Coil_HeatingWater) THEN

    IF(UnitarySystem(UnitarySysNum)%SuppHeatCoilType_Num == Coil_HeatingSteam) THEN

      !     set air-side and steam-side mass flow rates
      mdot = MIN(Node(UnitarySystem(UnitarySysNum)%SuppCoilFluidOutletNodeNum)%MassFlowRateMaxAvail, &
             UnitarySystem(UnitarySysNum)%MaxSuppCoilFluidFlow)
      Node(UnitarySystem(UnitarySysNum)%SuppCoilFluidInletNode)%MassFlowRate = mdot

!     simulate steam coil to find operating capacity
      CALL SimulateSteamCoilComponents(UnitarySystem(UnitarySysNum)%SuppHeatCoilName, &
                                       FirstHVACIteration,    &
                                       1.0d0, & !QCoilReq, simulate any load > 0 to get max capacity of steam coil
                                       UnitarySystem(UnitarySysNum)%SuppHeatCoilIndex, QActual)
      UnitarySystem(UnitarySysNum)%DesignSuppHeatingCapacity = GetSteamCoilCapacity('Coil:Heating:Steam', &
                                                      UnitarySystem(UnitarySysNum)%SuppHeatCoilName,ErrorsFound)

    END IF ! from IF(UnitarySystem(UnitarySysNum)%SuppHeatCoilType_Num == Coil_HeatingSteam) THEN
END IF ! from IF( FirstHVACIteration ) THEN

IF(MySetPointCheckFlag(UnitarySysNum))THEN
  IF ( .NOT. SysSizingCalc .AND. DOSetPointTest) THEN

    IF(UnitarySystem(UnitarySysNum)%CoolCoilExists)THEN
      ControlNode = UnitarySystem(UnitarySysNum)%SystemCoolControlNodeNum
      IF (ControlNode > 0) THEN
        CALL CheckNodeSetPoint(UnitarySysNum,AirLoopNum, ControlNode, CoolingCoil,OAUCoilOutTemp)
      END IF
    END IF

    IF(UnitarySystem(UnitarySysNum)%HeatCoilExists)THEN
      ControlNode = UnitarySystem(UnitarySysNum)%SystemHeatControlNodeNum
      IF (ControlNode > 0) THEN
        CALL CheckNodeSetPoint(UnitarySysNum,AirLoopNum, ControlNode,HeatingCoil,OAUCoilOutTemp)
      END IF
    END IF

    IF(UnitarySystem(UnitarySysNum)%SuppCoilExists)THEN
      ControlNode = UnitarySystem(UnitarySysNum)%SuppHeatControlNodeNum
      IF (ControlNode > 0) THEN
        CALL CheckNodeSetPoint(UnitarySysNum,AirLoopNum, ControlNode,SuppHeatCoil,OAUCoilOutTemp)
      END IF
    END IF

    MySetPointCheckFlag(UnitarySysNum) = .FALSE.
  END IF
END IF

UnitarySystem(UnitarySysNum)%CoolingPartLoadFrac  = 0.0d0
UnitarySystem(UnitarySysNum)%HeatingPartLoadFrac  = 0.0d0
UnitarySystem(UnitarySysNum)%SuppHeatPartLoadFrac = 0.0d0
UnitarySystem(UnitarySysNum)%CoolingCycRatio      = 0.0d0
UnitarySystem(UnitarySysNum)%CoolingSpeedRatio    = 0.0d0
UnitarySystem(UnitarySysNum)%CoolingSpeedNum      = 0.0d0
UnitarySystem(UnitarySysNum)%HeatingCycRatio      = 0.0d0
UnitarySystem(UnitarySysNum)%HeatingSpeedRatio    = 0.0d0
UnitarySystem(UnitarySysNum)%HeatingSpeedNum      = 0.0d0
UnitarySystem(UnitarySysNum)%HeatingCoilSensDemand   = 0.0d0
UnitarySystem(UnitarySysNum)%CoolingCoilSensDemand   = 0.0d0
UnitarySystem(UnitarySysNum)%CoolingCoilLatentDemand = 0.0d0
UnitarySystem(UnitarySysNum)%DehumidInducedHeatingDemandRate = 0.0d0

UnitarySystem(UnitarySysNum)%InitHeatPump = .TRUE.

RETURN
END SUBROUTINE InitUnitarySystems

SUBROUTINE CheckNodeSetPoint(UnitarySysNum,AirLoopNum, ControlNode, CoilType,OAUCoilOutTemp)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Raustad
          !       DATE WRITTEN   March 2013
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine checks for proper set point at control node.

          ! METHODOLOGY EMPLOYED:
          ! Uses the control node to test for set point.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE EMSManager,      ONLY: iTemperatureSetpoint, CheckIfNodeSetpointManagedByEMS, iHumidityRatioMaxSetpoint

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN) :: UnitarySysNum               ! number of the current DX Sys being simulated
  INTEGER, INTENT (IN) :: AirLoopNum                  ! number of the current air loop being simulated
  INTEGER, INTENT (IN) :: ControlNode                 ! Node to test for set point
  INTEGER, INTENT (IN) :: CoilType                    ! True if cooling coil, then test for HumRatMax set point
  REAL(r64), INTENT(IN), OPTIONAL :: OAUCoilOutTemp   ! the coil inlet temperature of OutdoorAirUnit

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          ! na

IF (AirLoopNum .EQ.-1) THEN                           ! Outdoor Air Unit
  Node(ControlNode)%TempSetPoint = OAUCoilOutTemp     ! Set the coil outlet temperature
  IF(UnitarySystem(UnitarySysNum)%ISHundredPercentDOASDXCoil) THEN
    CALL FrostControlSetPointLimit(UnitarySysNum,UnitarySystem(UnitarySysNum)%DesiredOutletTemp,Node(ControlNode)%HumRatMax, &
                                   OutBaroPress, UnitarySystem(UnitarySysNum)%DOASDXCoolingCoilMinTout,1)
  END IF
ELSEIF (AirLoopNum /= -1) THEN ! Not an Outdoor air unit

  IF (Node(ControlNode)%TempSetPoint == SensedNodeFlagValue .AND. &
      UnitarySystem(UnitarySysNum)%ControlType == SetPointBased) THEN
    IF (.NOT. AnyEnergyManagementSystemInModel) THEN
      CALL ShowSevereError(TRIM(UnitarySystem(UnitarySysNum)%UnitarySystemType)//&
                       ': Missing temperature setpoint for unitary system = ' //TRIM(UnitarySystem(UnitarySysNum)%Name))
      CALL ShowContinueError('  use a Setpoint Manager to establish a setpoint at the coil control node.')
      SetPointErrorFlag = .TRUE.
    ELSE
      CALL CheckIfNodeSetpointManagedByEMS(ControlNode,iTemperatureSetpoint, SetpointErrorFlag)
      IF (SetpointErrorFlag) THEN
        CALL ShowSevereError(TRIM(UnitarySystem(UnitarySysNum)%UnitarySystemType)//&
                       ': Missing temperature setpoint for unitary system = ' //TRIM(UnitarySystem(UnitarySysNum)%Name))
        CALL ShowContinueError('  use a Setpoint Manager to establish a setpoint at the coil control node.')
        CALL ShowContinueError('  or use an EMS actuator to establish a temperature setpoint at the coil control node.')
      END IF
    END IF
  END IF
  IF ((UnitarySystem(UnitarySysNum)%DehumidControlType_Num .NE. DehumidControl_None) .AND. &
      (Node(ControlNode)%HumRatMax == SensedNodeFlagValue) .AND. &
      UnitarySystem(UnitarySysNum)%ControlType == SetPointBased .AND. CoilType .EQ. CoolingCoil) THEN
    IF (.NOT. AnyEnergyManagementSystemInModel) THEN
      CALL ShowSevereError(TRIM(UnitarySystem(UnitarySysNum)%UnitarySystemType)//&
                       ': Missing humidity ratio setpoint (HUMRATMAX) for unitary system = ' &
                                  //TRIM(UnitarySystem(UnitarySysNum)%Name))
      CALL ShowContinueError('  use a Setpoint Manager to establish a setpoint at the coil control node.')
      SetPointErrorFlag = .TRUE.
    ELSE
      CALL CheckIfNodeSetpointManagedByEMS(ControlNode,iHumidityRatioMaxSetpoint, SetpointErrorFlag)
      IF (SetpointErrorFlag) THEN
        CALL ShowSevereError(TRIM(UnitarySystem(UnitarySysNum)%UnitarySystemType)//&
                       ': Missing maximum humidity ratio setpoint (HUMRATMAX) for unitary system = ' &
                                 //TRIM(UnitarySystem(UnitarySysNum)%Name))
        CALL ShowContinueError('  use a Setpoint Manager to establish a setpoint at the coil control node.')
        CALL ShowContinueError('  or use an EMS actuator to establish a maximum humidity ratio setpoint.')
      END IF
    END IF

  END IF
END IF


RETURN
END SUBROUTINE CheckNodeSetPoint

SUBROUTINE UpdateUnitarySystemControl(UnitarySysNum,AirLoopNum,OutNode,ControlNode,OnOffAirFlowRatio, &
                                    OAUCoilOutletTemp,FirstHVACIteration, ZoneLoad,MaxOutletTemp)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Raustad, FSEC
          !       DATE WRITTEN   February 2013
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is for sizing unitary systems.

          ! METHODOLOGY EMPLOYED:
          ! Either CALL the coil model to get the size or size coil.
          ! Current method is to use same methodology as is used in coil objects.
          ! Future changes will include a common sizing algorithm and push the calculated
          ! size to the coil object prior to first call (so the coil will not autosize).

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataAirLoop,           ONLY: AirLoopControlInfo
  USE Psychrometrics,        ONLY: PsyHfgAirFnWTdb
  USE DataHeatBalFanSys,     ONLY: TempControlType
  USE DataZoneEnergyDemands

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN)   :: UnitarySysNum                ! number of the current DX Sys being simulated
  INTEGER, INTENT (IN)   :: AirLoopNum                   ! number of the current air loop being simulated
  INTEGER, INTENT (IN)   :: OutNode                      ! coil outlet node number
  INTEGER, INTENT (IN)   :: ControlNode                  ! control node number
  LOGICAL, INTENT (IN)   :: FirstHVACIteration
  REAL(r64), INTENT (IN) :: OAUCoilOutletTemp            ! "ONLY" for zoneHVAC:OutdoorAirUnit
  REAL(R64), INTENT (INOUT)           :: OnOffAirFlowRatio
  REAL(R64), INTENT (INOUT), OPTIONAL :: ZoneLoad
  REAL(R64), INTENT (IN), OPTIONAL    :: MaxOutletTemp   ! limits heating coil outlet temp [C]

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
INTEGER   :: ControlType
REAL(r64) :: H2OHtOfVap                  ! Heat of vaporization of air

ControlType = UnitarySystem(UnitarySysNum)%ControlType
! These initializations are done every iteration

SELECT CASE(ControlType)
  CASE(LoadBased)
    IF (AirLoopNum .EQ.-1) THEN ! This IF-THEN routine is just for ZoneHVAC:OutdoorAirUnit
      CALL ShowWarningError(TRIM(UnitarySystem(UnitarySysNum)%UnitarySystemType)// &
                            ' "'//TRIM(UnitarySystem(UnitarySysNum)%Name)//'"')
      CALL ShowFatalError('...Load based control is not allowed when used with ZoneHVAC:OutdoorAirUnit')
    END IF

  ! here we need to deal with sequenced zone equip
    HeatingLoad = .FALSE.
    CoolingLoad = .FALSE.
    IF (UnitarySystem(UnitarySysNum)%ZoneSequenceCoolingNum > 0 .AND. UnitarySystem(UnitarySysNum)%ZoneSequenceHeatingNum > 0)THEN
      QToCoolSetPt = ZoneSysEnergyDemand(UnitarySystem(UnitarySysNum)%ControlZoneNum)%&
                  SequencedOutputRequiredToCoolingSP(UnitarySystem(UnitarySysNum)%ZoneSequenceCoolingNum)
      QToHeatSetPt = ZoneSysEnergyDemand(UnitarySystem(UnitarySysNum)%ControlZoneNum)%&
                  SequencedOutputRequiredToHeatingSP(UnitarySystem(UnitarySysNum)%ZoneSequenceHeatingNum)
      IF (QToHeatSetPt > 0.0d0 .AND. QToCoolSetPt > 0.0d0 .AND. &
          TempControlType(UnitarySystem(UnitarySysNum)%ControlZoneNum) .NE. SingleCoolingSetPoint) THEN
        ZoneLoad = QToHeatSetPt
        HeatingLoad = .TRUE.
      ELSEIF (QToHeatSetPt > 0.0d0 .AND. QToCoolSetPt > 0.0d0 .AND. &
          TempControlType(UnitarySystem(UnitarySysNum)%ControlZoneNum) .EQ. SingleCoolingSetPoint) THEN
        ZoneLoad = 0.0d0
      ELSEIF (QToHeatSetPt < 0.0d0 .AND. QToCoolSetPt < 0.0d0 .AND. &
          TempControlType(UnitarySystem(UnitarySysNum)%ControlZoneNum) .NE. SingleHeatingSetPoint) THEN
        ZoneLoad = QToCoolSetPt
        CoolingLoad = .TRUE.
      ELSEIF (QToHeatSetPt < 0.0d0 .AND. QToCoolSetPt < 0.0d0 .AND. &
          TempControlType(UnitarySystem(UnitarySysNum)%ControlZoneNum) .EQ. SingleHeatingSetPoint) THEN
        ZoneLoad = 0.0d0
      ELSEIF (QToHeatSetPt <= 0.0d0 .AND. QToCoolSetPt >= 0.0d0) THEN
        ZoneLoad = 0.0d0
      END IF
      MoistureLoad = ZoneSysMoistureDemand(UnitarySystem(UnitarySysNum)%ControlZoneNum)% &
                     SequencedOutputRequiredToDehumidSP(UnitarySystem(UnitarySysNum)%ZoneSequenceCoolingNum)
    ELSE
      ZoneLoad= ZoneSysEnergyDemand(UnitarySystem(UnitarySysNum)%ControlZoneNum)%RemainingOutputRequired
      QToCoolSetPt=ZoneSysEnergyDemand(UnitarySystem(UnitarySysNum)%ControlZoneNum)%OutputRequiredToCoolingSP
      QToHeatSetPt=ZoneSysEnergyDemand(UnitarySystem(UnitarySysNum)%ControlZoneNum)%OutputRequiredToHeatingSP
      MoistureLoad = ZoneSysMoistureDemand(UnitarySystem(UnitarySysNum)%ControlZoneNum)%OutputRequiredToDehumidIFyingSP
    END IF

    IF ( UnitarySystem(UnitarySysNum)%DehumidControlType_Num .NE. DehumidControl_None)THEN
      H2OHtOfVap = PsyHfgAirFnWTdb(Node(UnitarySystem(UnitarySysNum)%NodeNumofControlledZone)%HumRat,&
                        Node(UnitarySystem(UnitarySysNum)%NodeNumofControlledZone)%Temp,'UpdateUnitarySystemControl')

      ! positive MoistureLoad means no dehumidification load
      MoistureLoad = MIN(0.0d0,MoistureLoad * H2OHtOfVap)
    ELSE
      MoistureLoad = 0.0d0
    END IF

    CALL InitLoadBasedControl(UnitarySysNum,AirLoopNum,FirstHVACIteration,OnOffAirFlowRatio,ZoneLoad)

    ! EMS override point
    IF (UnitarySystem(UnitarySysNum)%EMSOverrideSensZoneLoadRequest) &
      ZoneLoad = UnitarySystem(UnitarySysNum)%EMSSensibleZoneLoadValue
    IF (UnitarySystem(UnitarySysNum)%EMSOverrideMoistZoneLoadRequest) &
      MoistureLoad = UnitarySystem(UnitarySysNum)%EMSMoistureZoneLoadValue

  CASE(SetPointBased)
    IF (AirLoopNum .EQ.-1) THEN ! This IF-THEN routine is just for ZoneHVAC:OutdoorAIRUNIT

      IF (ControlNode.EQ.0) THEN
        UnitarySystem(UnitarySysNum)%DesiredOutletTemp = OAUCoilOutletTemp
        UnitarySystem(UnitarySysNum)%DesiredOutletHumRat = 1.0d0
      ELSEIF (ControlNode.EQ.OutNode) THEN
        UnitarySystem(UnitarySysNum)%DesiredOutletTemp =OAUCoilOutletTemp
      END IF
      ! If the unitary system is an equipment of Outdoor Air Unit, the desired coil outlet humidity level is set to zero
      UnitarySystem(UnitarySysNum)%DesiredOutletHumRat = 1.0d0

    ELSE ! Not Outdoor Air Unit or zone equipment
      IF(AirLoopNum .GT. 0)EconomizerFlag = AirLoopControlInfo(AirLoopNum)%EconoActive
      IF (ControlNode.EQ.0) THEN
        UnitarySystem(UnitarySysNum)%DesiredOutletTemp = 0.0d0
        UnitarySystem(UnitarySysNum)%DesiredOutletHumRat = 1.0d0
      ELSEIF (ControlNode.EQ.OutNode) THEN
        IF (UnitarySystem(UnitarySysNum)%ISHundredPercentDOASDXCoil .AND. UnitarySystem(UnitarySysNum)%RunOnSensibleLoad) THEN
          CALL FrostControlSetPointLimit(UnitarySysNum, Node(ControlNode)%TempSetPoint,Node(ControlNode)%HumRatMax,OutBaroPress, &
                                         UnitarySystem(UnitarySysNum)%DOASDXCoolingCoilMinTout,1)
        END IF
        UnitarySystem(UnitarySysNum)%DesiredOutletTemp = Node(ControlNode)%TempSetPoint
        !  IF HumRatMax is zero, then there is no request from SetpointManager:SingleZone:Humidity:Maximum
        IF ((UnitarySystem(UnitarySysNum)%DehumidControlType_Num .NE. DehumidControl_None) .AND. &
            (Node(ControlNode)%HumRatMax .GT. 0.0d0)) THEN
          IF (UnitarySystem(UnitarySysNum)%ISHundredPercentDOASDXCoil .AND. UnitarySystem(UnitarySysNum)%RunOnLatentLoad) THEN
            CALL FrostControlSetPointLimit(UnitarySysNum,Node(ControlNode)%TempSetPoint,Node(ControlNode)%HumRatMax,OutBaroPress, &
                                           UnitarySystem(UnitarySysNum)%DOASDXCoolingCoilMinTout,2)
          END IF
          UnitarySystem(UnitarySysNum)%DesiredOutletHumRat = Node(ControlNode)%HumRatMax
        ELSE
          UnitarySystem(UnitarySysNum)%DesiredOutletHumRat = 1.0d0
        END IF
      ELSE
        IF (UnitarySystem(UnitarySysNum)%ISHundredPercentDOASDXCoil .AND. UnitarySystem(UnitarySysNum)%RunOnSensibleLoad) THEN
          CALL FrostControlSetPointLimit(UnitarySysNum,Node(ControlNode)%TempSetPoint,Node(ControlNode)%HumRatMax,OutBaroPress, &
                                         UnitarySystem(UnitarySysNum)%DOASDXCoolingCoilMinTout,1)
        END IF
        UnitarySystem(UnitarySysNum)%DesiredOutletTemp = Node(ControlNode)%TempSetPoint - &
          (Node(ControlNode)%Temp - Node(OutNode)%Temp)
        IF (UnitarySystem(UnitarySysNum)%DehumidControlType_Num .NE. DehumidControl_None) THEN
          IF (UnitarySystem(UnitarySysNum)%ISHundredPercentDOASDXCoil .AND. UnitarySystem(UnitarySysNum)%RunOnLatentLoad) THEN
            CALL FrostControlSetPointLimit(UnitarySysNum,Node(ControlNode)%TempSetPoint,Node(ControlNode)%HumRatMax,OutBaroPress, &
                                           UnitarySystem(UnitarySysNum)%DOASDXCoolingCoilMinTout,2)
          END IF
          UnitarySystem(UnitarySysNum)%DesiredOutletHumRat = Node(ControlNode)%HumRatMax - &
            (Node(ControlNode)%HumRat - Node(OutNode)%HumRat)
        ELSE
          UnitarySystem(UnitarySysNum)%DesiredOutletHumRat = 1.0d0
        END IF
      END IF
    END IF
    IF(PRESENT(MaxOutletTemp))UnitarySystem(UnitarySysNum)%DesiredOutletTemp = &
        MIN(UnitarySystem(UnitarySysNum)%DesiredOutletTemp,MaxOutletTemp)

  CASE DEFAULT

END SELECT

RETURN
END SUBROUTINE UpdateUnitarySystemControl

SUBROUTINE InitLoadBasedControl(UnitarySysNum,AirLoopNum,FirstHVACIteration,OnOffAirFlowRatio,ZoneLoad)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Raustad, FSEC
          !       DATE WRITTEN   February 2013
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is for initializations of the load controlled Unitary Systems.

          ! METHODOLOGY EMPLOYED:
          ! Initialize mass flow rates and speed ratios. Calculate loads and adjust if necessary when using constant fan.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataAirLoop,           ONLY: AirLoopControlInfo, AirToZoneNodeInfo
  USE Fans,                  ONLY: GetFanDesignVolumeFlowRate, GetFanSpeedRatioCurveIndex
  USE General,               ONLY: RoundSigDigits, TrimSigDigits
  USE DataHeatBalance,       ONLY: Zone
  USE DataHeatBalFanSys,     ONLY: TempControlType
  USE DataAirflowNetwork,    ONLY: SimulateAirflowNetwork, AirflowNetworkControlMultizone, AirflowNetworkFanActivated
  USE Psychrometrics,        ONLY: PsyHFnTdbW
  USE ReportSizingManager,   ONLY: ReportSizingOutput
  USE WaterCoils,            ONLY: GetCoilMaxWaterFlowRate, SimulateWaterCoilComponents
  USE SteamCoils,            ONLY: GetCoilMaxSteamFlowRate, SimulateSteamCoilComponents
  USE DataPlant,             ONLY: PlantLoop
  USE FluidProperties,       ONLY: GetDensityGlycol, GetSatDensityRefrig
  USE PlantUtilities,        ONLY: SetComponentFlowRate, InitComponentNodes
  USE DataZoneEnergyDemands, ONLY: ZoneSysEnergyDemand, CurDeadbandOrSetback, SetBack
  USE DataZoneControls,      ONLY: StageZoneLogic
  USE BranchInputManager,    ONLY: CheckSystemBranchFlow

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN)      :: UnitarySysNum       ! number of the current DX Sys being simulated
  INTEGER, INTENT (IN)      :: AirLoopNum          ! number of the current air loop being simulated
  LOGICAL, INTENT (IN)      :: FirstHVACIteration
  REAL(R64), INTENT (INOUT) :: OnOffAirFlowRatio
  REAL(r64), INTENT (INOUT) :: ZoneLoad

          ! SUBROUTINE PARAMETER DEFINITIONS:
  REAL(r64), PARAMETER :: Small5WLoad = 5.0d0

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  LOGICAL, ALLOCATABLE,SAVE, DIMENSION(:) :: MyEnvrnFlag             ! environment flag
  LOGICAL, ALLOCATABLE,SAVE, DIMENSION(:) :: MyFanFlag               ! used for sizing fan inputs one time
  LOGICAL, ALLOCATABLE,SAVE, DIMENSION(:) :: MyCheckFlag             ! Used to obtain the zone inlet node number
                                                                     ! in the controlled zone
  LOGICAL, ALLOCATABLE,SAVE, DIMENSION(:) :: MyFlowFracFlag          ! Used for calculatig flow fraction once
  LOGICAL, ALLOCATABLE,SAVE, DIMENSION(:) :: MyStagedFlag            ! used for finding on staged thermostat
  LOGICAL, SAVE                 :: MyOneTimeFlag = .TRUE.  ! one time allocation flag
  LOGICAL, SAVE                 :: MyAirLoopPass = .TRUE.  ! one time allocation flag
  INTEGER                       :: AirLoopPass   = 0       ! Number of air loop pass
  CHARACTER(len=MaxNameLength)  :: FanType                 ! used in warning messages
  CHARACTER(len=MaxNameLength)  :: FanName                 ! used in warning messages
  LOGICAL                       :: ErrFlag                 ! error flag for mining functions
  LOGICAL                       :: ErrorsFound             ! error flag for mining functions
  INTEGER                       :: ZoneInNode              ! Zone inlet node number in the controlled zone
  REAL(r64)                     :: MinHumRat               ! Minimum humidity ratio for sensible capacity calculation (kg/kg)
  REAL(r64)                     :: DeltaMassRate           ! DIFference of mass flow rate between
                                                           ! inlet node and system outlet node
  INTEGER                       :: i,j,k                   ! index to get the zone inlet node
  REAL(r64)                     :: MassFlowRate            ! mass flow rate to calculate loss
  REAL(r64)                     :: MaxTemp                 ! Maximum temperature used in latent loss calculation
  INTEGER                       :: EquipNum              = 0                    ! local DO loop index for zone equipment
  INTEGER                       :: ZoneInSysIndex        = 0                    ! number of zone inlet nodes counter in an airloop
  INTEGER                       :: NumAirLoopZones       = 0                    ! number of zone inlet nodes in an air loop
  INTEGER                       :: ZoneInletNodeNum      = 0                    ! zone inlet nodes node number
  LOGICAL                       :: FlowFracFlagReady     = .TRUE.               ! one time flag for calculating flow fraction
  REAL(r64)                     :: SumOfMassFlowRateMax  = 0.0d0                ! the sum of zone inlet mass flow rates
  REAL(r64)                     :: CntrlZoneTerminalUnitMassFlowRateMax = 0.0d0 ! Maximum mass flow rate through controlled zone
  REAL(R64)                     :: rho
  REAL(R64)                     :: QZnReq
  REAL(R64)                     :: QActual
  REAL(R64)                     :: CoilMaxVolFlowRate
  INTEGER                       :: SteamIndex
  REAL(R64)                     :: SteamDensity
  REAL(R64)                     :: SensOutputOff
  REAL(R64)                     :: LatOutputOff
  LOGICAL                       :: HXUnitOn

  IF (MyOneTimeFlag) THEN

    ! initialize the environment and sizing flags
    ALLOCATE(MyEnvrnFlag(NumUnitarySystem))
    ALLOCATE(MyFanFlag(NumUnitarySystem))
    ALLOCATE(MyCheckFlag(NumUnitarySystem))
    ALLOCATE(MyFlowFracFlag(NumUnitarySystem))
    ALLOCATE(MyStagedFlag(NumUnitarySystem))

    MyEnvrnFlag = .TRUE.
    MyFanFlag = .TRUE.
    MyCheckFlag = .TRUE.
    MyFlowFracFlag = .TRUE.
    MyOneTimeFlag = .FALSE.
    MyStagedFlag = .TRUE.

  END IF

! do the Begin Environment initializations
  IF (BeginEnvrnFlag .and. MyEnvrnFlag(UnitarySysNum)) THEN

!   set fluid-side hardware limits
    IF(UnitarySystem(UnitarySysNum)%HeatCoilFluidInletNode .GT. 0)THEN

        IF(UnitarySystem(UnitarySysNum)%MaxHeatCoilFluidFlow .EQ. Autosize)THEN
            ! IF water coil max water flow rate is autosized, simulate once in order to mine max flow rate
            IF(UnitarySystem(UnitarySysNum)%HeatingCoilType_Num == Coil_HeatingWater) THEN
                CALL SimulateWaterCoilComponents(UnitarySystem(UnitarySysNum)%HeatingCoilName,FirstHVACIteration, &
                                                UnitarySystem(UnitarySysNum)%HeatingCoilIndex)
                CoilMaxVolFlowRate = GetCoilMaxWaterFlowRate('Coil:Heating:Water',  &
                                                             UnitarySystem(UnitarySysNum)%HeatingCoilName,ErrorsFound)
                IF(CoilMaxVolFlowRate .NE. Autosize) THEN
                    rho = GetDensityGlycol(PlantLoop(UnitarySystem(UnitarySysNum)%HeatCoilLoopNum)%fluidName, &
                                            InitConvTemp, &
                                            PlantLoop(UnitarySystem(UnitarySysNum)%HeatCoilLoopNum)%fluidIndex, &
                                            'InitUnitarySystems')
                    UnitarySystem(UnitarySysNum)%MaxHeatCoilFluidFlow = CoilMaxVolFlowRate * rho
                END IF
            END IF
            ! IF steam coil max steam flow rate is autosized, simulate once in order to mine max flow rate
            IF(UnitarySystem(UnitarySysNum)%HeatingCoilType_Num == Coil_HeatingSteam) THEN
                CALL SimulateSteamCoilComponents(UnitarySystem(UnitarySysNum)%HeatingCoilName, &
                                                FirstHVACIteration,    &
                                                1.0d0, & !QCoilReq, simulate any load > 0 to get max capacity
                                                UnitarySystem(UnitarySysNum)%HeatingCoilIndex, QActual)
                CoilMaxVolFlowRate = GetCoilMaxSteamFlowRate(UnitarySystem(UnitarySysNum)%HeatingCoilIndex,ErrorsFound)
                IF(CoilMaxVolFlowRate .NE. Autosize) THEN
                   SteamIndex = 0 ! Function GetSatDensityRefrig will look up steam index if 0 is passed
                   SteamDensity=GetSatDensityRefrig('STEAM',TempSteamIn,1.0d0,SteamIndex,'InitUnitarySystems')
                   UnitarySystem(UnitarySysNum)%MaxHeatCoilFluidFlow = CoilMaxVolFlowRate * SteamDensity
                END IF
            END IF
        END IF

        CALL InitComponentNodes(0.0d0, UnitarySystem(UnitarySysNum)%MaxHeatCoilFluidFlow, &
                                        UnitarySystem(UnitarySysNum)%HeatCoilFluidInletNode, &
                                        UnitarySystem(UnitarySysNum)%HeatCoilFluidOutletNodeNum, &
                                        UnitarySystem(UnitarySysNum)%HeatCoilLoopNum, &
                                        UnitarySystem(UnitarySysNum)%HeatCoilLoopSide, &
                                        UnitarySystem(UnitarySysNum)%HeatCoilBranchNum, &
                                        UnitarySystem(UnitarySysNum)%HeatCoilCompNum )
    END IF
    IF(UnitarySystem(UnitarySysNum)%SuppCoilFluidInletNode .GT. 0)THEN
        IF(UnitarySystem(UnitarySysNum)%MaxSuppCoilFluidFlow .EQ. Autosize)THEN
            IF(UnitarySystem(UnitarySysNum)%SuppHeatCoilType_Num == Coil_HeatingWater) THEN
                ! IF water coil max water flow rate is autosized, simulate once in order to mine max flow rate
                CALL SimulateWaterCoilComponents(UnitarySystem(UnitarySysNum)%SuppHeatCoilName,FirstHVACIteration, &
                                                UnitarySystem(UnitarySysNum)%SuppHeatCoilIndex)
                CoilMaxVolFlowRate = GetCoilMaxWaterFlowRate('Coil:Heating:Water',  &
                                                            UnitarySystem(UnitarySysNum)%SuppHeatCoilName,ErrorsFound)
                IF(CoilMaxVolFlowRate .NE. Autosize) THEN
                    rho = GetDensityGlycol(PlantLoop(UnitarySystem(UnitarySysNum)%SuppCoilLoopNum)%fluidName, &
                                        InitConvTemp, &
                                        PlantLoop(UnitarySystem(UnitarySysNum)%SuppCoilLoopNum)%fluidIndex, &
                                        'InitUnitarySystems')
                    UnitarySystem(UnitarySysNum)%MaxSuppCoilFluidFlow = CoilMaxVolFlowRate * rho
                END IF
            END IF
            IF(UnitarySystem(UnitarySysNum)%SuppHeatCoilType_Num == Coil_HeatingSteam) THEN
                CALL SimulateSteamCoilComponents(UnitarySystem(UnitarySysNum)%SuppHeatCoilName, &
                                                FirstHVACIteration,    &
                                                1.0d0, & !QCoilReq, simulate any load > 0 to get max capacity
                                                UnitarySystem(UnitarySysNum)%SuppHeatCoilIndex, QActual)
                CoilMaxVolFlowRate = GetCoilMaxSteamFlowRate(UnitarySystem(UnitarySysNum)%SuppHeatCoilIndex,ErrorsFound)
                IF(CoilMaxVolFlowRate .NE. Autosize) THEN
                SteamIndex = 0 ! Function GetSatDensityRefrig will look up steam index if 0 is passed
                SteamDensity=GetSatDensityRefrig('STEAM',TempSteamIn,1.0d0,SteamIndex,'InitUnitarySystems')
                UnitarySystem(UnitarySysNum)%MaxSuppCoilFluidFlow = CoilMaxVolFlowRate * SteamDensity
                END IF
            END IF
            CALL InitComponentNodes(0.0d0, UnitarySystem(UnitarySysNum)%MaxSuppCoilFluidFlow, &
                                        UnitarySystem(UnitarySysNum)%SuppCoilFluidInletNode, &
                                        UnitarySystem(UnitarySysNum)%SuppCoilFluidOutletNodeNum, &
                                        UnitarySystem(UnitarySysNum)%SuppCoilLoopNum, &
                                        UnitarySystem(UnitarySysNum)%SuppCoilLoopSide, &
                                        UnitarySystem(UnitarySysNum)%SuppCoilBranchNum, &
                                        UnitarySystem(UnitarySysNum)%SuppCoilCompNum )
        END IF
    END IF
    MyEnvrnFlag(UnitarySysNum) = .FALSE.
  END IF

  IF (.not. BeginEnvrnFlag) THEN
    MyEnvrnFlag(UnitarySysNum) = .TRUE.
  END IF

  IF(MyFanFlag(UnitarySysNum))THEN
    IF(UnitarySystem(UnitarySysNum)%ActualFanVolFlowRate /= Autosize)THEN
      IF(UnitarySystem(UnitarySysNum)%ActualFanVolFlowRate .GT. 0.0d0)THEN
        UnitarySystem(UnitarySysNum)%HeatingFanSpeedRatio = &
                  UnitarySystem(UnitarySysNum)%MaxHeatAirVolFlow/UnitarySystem(UnitarySysNum)%ActualFanVolFlowRate
        UnitarySystem(UnitarySysNum)%CoolingFanSpeedRatio = &
                  UnitarySystem(UnitarySysNum)%MaxCoolAirVolFlow/UnitarySystem(UnitarySysNum)%ActualFanVolFlowRate
        UnitarySystem(UnitarySysNum)%NoHeatCoolSpeedRatio = &
                  UnitarySystem(UnitarySysNum)%MaxNoCoolHeatAirVolFlow/UnitarySystem(UnitarySysNum)%ActualFanVolFlowRate
        IF(UnitarySystem(UnitarySysNum)%FanExists)THEN
          IF(GetFanSpeedRatioCurveIndex(FanType,FanName,UnitarySystem(UnitarySysNum)%FanIndex) .GT. 0)THEN
            IF(UnitarySystem(UnitarySysNum)%ActualFanVolFlowRate .EQ. UnitarySystem(UnitarySysNum)%MaxHeatAirVolFlow .AND. &
               UnitarySystem(UnitarySysNum)%ActualFanVolFlowRate .EQ. UnitarySystem(UnitarySysNum)%MaxCoolAirVolFlow .AND. &
               UnitarySystem(UnitarySysNum)%ActualFanVolFlowRate .EQ. UnitarySystem(UnitarySysNum)%MaxNoCoolHeatAirVolFlow)THEN
              CALL ShowWarningError(TRIM(UnitarySystem(UnitarySysNum)%UnitarySystemType)// &
                                ' "'//TRIM(UnitarySystem(UnitarySysNum)%Name)//'"')
              CALL ShowContinueError('...For fan type and name = '//TRIM(FanType)//' "'//TRIM(FanName)//'"')
              CALL ShowContinueError('...Fan power ratio function of speed ratio curve has no impact IF fan volumetric '// &
                                 'flow rate is the same as the unitary system volumetric flow rate.')
              CALL ShowContinueError('...Fan volumetric flow rate            = '// &
                                 TRIM(RoundSigDigits(UnitarySystem(UnitarySysNum)%ActualFanVolFlowRate,5))//' m3/s.')
              CALL ShowContinueError('...Unitary system volumetric flow rate = '// &
                                 TRIM(RoundSigDigits(UnitarySystem(UnitarySysNum)%MaxHeatAirVolFlow,5))//' m3/s.')
            END IF
          END IF
        ELSE
          CALL CheckSystemBranchFlow(TRIM(UnitarySystem(UnitarySysNum)%UnitarySystemType),UnitarySystem(UnitarySysNum)%Name, &
                               UnitarySystem(UnitarySysNum)%ActualFanVolFlowRate,0.0d0,ErrFlag)
        END IF
      END IF
      MyFanFlag(UnitarySysNum) = .FALSE.
    ELSE
      IF(UnitarySystem(UnitarySysNum)%FanExists)THEN
        UnitarySystem(UnitarySysNum)%ActualFanVolFlowRate = &
          GetFanDesignVolumeFlowRate(Blank,Blank,ErrFlag,UnitarySystem(UnitarySysNum)%FanIndex)
      ELSE
        CALL CheckSystemBranchFlow(TRIM(UnitarySystem(UnitarySysNum)%UnitarySystemType),UnitarySystem(UnitarySysNum)%Name, &
                               UnitarySystem(UnitarySysNum)%ActualFanVolFlowRate,0.0d0,ErrFlag)
      END IF
    END IF
  END IF

  ! Get the zone inlet node
  IF (ALLOCATED(ZoneEquipConfig) .AND. MyCheckFlag(UnitarySysNum)) THEN
    DO i=1,NumOfZones
      IF(UnitarySystem(UnitarySysNum)%AirLoopEquipment)THEN
        IF (AirLoopNum .NE. ZoneEquipConfig(i)%AirLoopNum) CYCLE
        IF (UnitarySystem(UnitarySysNum)%ControlZoneNum .EQ. ZoneEquipConfig(i)%ActualZoneNum) THEN
          DO j=1, ZoneEquipConfig(i)%NumInletNodes
            IF (UnitarySystem(UnitarySysNum)%ZoneInletNode .EQ. 0) THEN
              DO k=1,ZoneEquipConfig(i)%NumInletNodes
                IF (ZoneEquipConfig(i)%InletNode(j) .EQ. ZoneEquipConfig(i)%AirDistUnitCool(k)%OutNode) THEN
                  UnitarySystem(UnitarySysNum)%ZoneInletNode = ZoneEquipConfig(i)%InletNode(j)
                  EXIT
                ELSEIF (ZoneEquipConfig(i)%InletNode(j) .EQ. ZoneEquipConfig(i)%AirDistUnitHeat(k)%OutNode) THEN
                  UnitarySystem(UnitarySysNum)%ZoneInletNode = ZoneEquipConfig(i)%InletNode(j)
                  EXIT
                END IF
              END DO
            END IF
          END DO
          !setup unitary system zone equipment sequence information based on finding an air terminal
          IF (ZoneEquipConfig(i)%EquipListIndex > 0) THEN
            DO EquipNum = 1, ZoneEquipList(ZoneEquipConfig(i)%EquipListIndex)%NumOfEquipTypes
              IF ((ZoneEquipList(ZoneEquipConfig(i)%EquipListIndex)%EquipType_Num(EquipNum) == AirDistUnit_Num) &
                  .OR. (ZoneEquipList(ZoneEquipConfig(i)%EquipListIndex)%EquipType_Num(EquipNum) == DirectAir_Num) ) THEN
                UnitarySystem(UnitarySysNum)%ZoneSequenceCoolingNum = &
                         ZoneEquipList(ZoneEquipConfig(i)%EquipListIndex)%CoolingPriority(EquipNum)
                UnitarySystem(UnitarySysNum)%ZoneSequenceHeatingNum = &
                         ZoneEquipList(ZoneEquipConfig(i)%EquipListIndex)%HeatingPriority(EquipNum)
              END IF
            END DO
          END IF
        END IF
      END IF
    END DO
    MyCheckFlag(UnitarySysNum) = .FALSE.
    IF (UnitarySystem(UnitarySysNum)%ZoneInletNode .EQ. 0) THEN
      CALL ShowSevereError(TRIM(UnitarySystem(UnitarySysNum)%UnitarySystemType)//' "'//TRIM(UnitarySystem(UnitarySysNum)%Name)// &
                             '": The zone inlet node in the controlled zone (' &
                            //Trim(Zone(UnitarySystem(UnitarySysNum)%ControlZoneNum)%Name) //') is not found.')
      CALL ShowFatalError('Subroutine InitLoadBasedControl: '//'Errors found in getting '// &
           TRIM(UnitarySystem(UnitarySysNum)%UnitarySystemType)//' input.  '//'Preceding condition(s) causes termination.')
    END IF
  END IF

  ! Find the number of zones (zone Inlet Nodes) attached to an air loop from the air loop number
  IF(MyFlowFracFlag(UnitarySysNum))THEN
    IF(UnitarySystem(UnitarySysNum)%AirLoopEquipment)THEN
      IF(ALLOCATED(AirToZoneNodeInfo))NumAirLoopZones = &
        AirToZoneNodeInfo(AirLoopNum)%NumZonesCooled + AirToZoneNodeInfo(AirLoopNum)%NumZonesHeated
      IF (ALLOCATED(AirToZoneNodeInfo) .AND. MyFlowFracFlag(UnitarySysNum)) THEN
        FlowFracFlagReady = .TRUE.
        ZonesLoop: DO ZoneInSysIndex = 1, NumAirLoopZones
          ! zone inlet nodes for cooling
          IF (AirToZoneNodeInfo(AirLoopNum)%NumZonesCooled > 0) THEN
            IF( AirToZoneNodeInfo(AirLoopNum)%TermUnitCoolInletNodes(ZoneInSysIndex) == -999 )THEN
              ! the data structure for the zones inlet nodes has not been filled
              FlowFracFlagReady = .FALSE.
            END IF
          END IF
          ! zone inlet nodes for heating
          IF (AirToZoneNodeInfo(AirLoopNum)%NumZonesHeated  > 0) THEN
            IF( AirToZoneNodeInfo(AirLoopNum)%TermUnitHeatInletNodes(ZoneInSysIndex) == -999 ) THEN
              ! the data structure for the zones inlet nodes has not been filled
              FlowFracFlagReady = .FALSE.
            END IF
          END IF
        END DO ZonesLoop
      END IF
      IF (ALLOCATED(AirToZoneNodeInfo) .AND. FlowFracFlagReady ) THEN
        SumOfMassFlowRateMax = 0.0d0  ! initialize the sum of the maximum flows
        DO ZoneInSysIndex = 1, NumAirLoopZones
          ZoneInletNodeNum = AirToZoneNodeInfo(AirLoopNum)%TermUnitCoolInletNodes(ZoneInSysIndex)
          SumOfMassFlowRateMax = SumOfMassFlowRateMax + Node(ZoneInletNodeNum)%MassFlowRateMax
          IF(AirToZoneNodeInfo(AirLoopNum)%CoolCtrlZoneNums(ZoneInSysIndex) == UnitarySystem(UnitarySysNum)%ControlZoneNum )THEN
            CntrlZoneTerminalUnitMassFlowRateMax = Node(ZoneInletNodeNum)%MassFlowRateMax
          END IF
        END DO
        IF (SumOfMassFlowRateMax /= 0.0d0 .AND. MyFlowFracFlag(UnitarySysNum)) THEN
          IF (CntrlZoneTerminalUnitMassFlowRateMax >= SmallAirVolFlow ) THEN
              UnitarySystem(UnitarySysNum)%ControlZoneMassFlowFrac = CntrlZoneTerminalUnitMassFlowRateMax/SumOfMassFlowRateMax
          ELSE
            CALL ShowSevereError(TRIM(UnitarySystem(UnitarySysNum)%UnitarySystemType)//' = '// &
                                 TRIM(UnitarySystem(UnitarySysNum)%Name))
            CALL ShowContinueError(' The Fraction of Supply Air Flow That Goes Through the Controlling Zone is set to 1.')
          END IF
          CALL ReportSizingOutput(UnitarySystem(UnitarySysNum)%UnitarySystemType,UnitarySystem(UnitarySysNum)%Name, &
                                  'Fraction of Supply Air Flow That Goes Through the Controlling Zone', &
                                  UnitarySystem(UnitarySysNum)%ControlZoneMassFlowFrac)
          MyFlowFracFlag(UnitarySysNum) = .FALSE.
        END IF
      END IF
    ELSE
      UnitarySystem(UnitarySysNum)%ControlZoneMassFlowFrac = 1.0d0
    END IF
  END IF ! IF(MyFlowFracFlag(UnitarySysNum))THEN

! What type of logic is this? Is the point to go through the main IF once? or every other time?
  IF (BeginEnvrnFlag .and. MyAirLoopPass) THEN
    AirLoopPass = 0
    MyAirLoopPass = .FALSE.
  END IF
  IF (.not. BeginEnvrnFlag) THEN
    MyAirLoopPass = .TRUE.
  END IF

  AirLoopPass = AirLoopPass + 1
  IF (AirLoopPass > 2) AirLoopPass = 1

  ! reset duct losses from previous iteration
  IF (FirstHVACIteration) THEN
    UnitarySystem(UnitarySysNum)%SenLoadLoss = 0.0d0
    UnitarySystem(UnitarySysNum)%LatLoadLoss = 0.0d0
  END IF

  ! Calcuate air distribution losses
!  IF (.NOT. FirstHVACIteration .AND. AirLoopPass .EQ. 1 .AND. AirflowNetworkFanActivated) THEN
  IF (.NOT. FirstHVACIteration .AND. AirflowNetworkFanActivated) THEN
    ZoneInNode = UnitarySystem(UnitarySysNum)%ZoneInletNode
    MinHumRat = Node(ZoneInNode)%HumRat
    MassFlowRate = Node(ZoneInNode)%MassFlowrate/UnitarySystem(UnitarySysNum)%ControlZoneMassFlowFrac
    IF(Node(UnitarySystem(UnitarySysNum)%UnitarySystemOutletNodeNum)%Temp .LT. &
       Node(UnitarySystem(UnitarySysNum)%NodeNumofControlledZone)%Temp ) &
      MinHumRat = Node(UnitarySystem(UnitarySysNum)%UnitarySystemOutletNodeNum)%HumRat
    IF (SimulateAirflowNetwork > AirflowNetworkControlMultizone) THEN
      DeltaMassRate = Node(UnitarySystem(UnitarySysNum)%UnitarySystemOutletNodeNum)%MassFlowrate - &
                    Node(ZoneInNode)%MassFlowrate/UnitarySystem(UnitarySysNum)%ControlZoneMassFlowFrac
      IF (DeltaMassRate .LT. 0.0d0) DeltaMassRate = 0.0d0
    ELSE
      MassFlowRate = Node(UnitarySystem(UnitarySysNum)%UnitarySystemOutletNodeNum)%MassFlowrate
      DeltaMassRate = 0.0d0
    END IF
    UnitarySystem(UnitarySysNum)%SenLoadLoss = &
       MassFlowRate * (PsyHFnTdbW(Node(UnitarySystem(UnitarySysNum)%UnitarySystemOutletNodeNum)%Temp,MinHumRat)- &
       PsyHFnTdbW(Node(ZoneInNode)%Temp,MinHumRat)) + DeltaMassRate * &
      (PsyHFnTdbW(Node(UnitarySystem(UnitarySysNum)%UnitarySystemOutletNodeNum)%Temp,MinHumRat) - &
       PsyHFnTdbW(Node(UnitarySystem(UnitarySysNum)%NodeNumofControlledZone)%Temp,MinHumRat))
    IF (ABS(UnitarySystem(UnitarySysNum)%SensibleLoadMet) > 0.0d0) THEN
      IF (ABS(UnitarySystem(UnitarySysNum)%SenLoadLoss/UnitarySystem(UnitarySysNum)%SensibleLoadMet) < 0.001d0) &
           UnitarySystem(UnitarySysNum)%SenLoadLoss = 0.0d0
    END IF
    IF(UnitarySystem(UnitarySysNum)%Humidistat)THEN
      MaxTemp = Node(UnitarySystem(UnitarySysNum)%NodeNumofControlledZone)%Temp
      UnitarySystem(UnitarySysNum)%LatLoadLoss = &
        MassFlowRate*(PsyHFnTdbW(MaxTemp,Node(UnitarySystem(UnitarySysNum)%UnitarySystemOutletNodeNum)%HumRat)- &
        PsyHFnTdbW(MaxTemp,Node(ZoneInNode)%HumRat)) + DeltaMassRate * &
        (PsyHFnTdbW(MaxTemp,Node(UnitarySystem(UnitarySysNum)%UnitarySystemOutletNodeNum)%HumRat) - &
        PsyHFnTdbW(MaxTemp,Node(UnitarySystem(UnitarySysNum)%NodeNumofControlledZone)%HumRat))
      IF (ABS(UnitarySystem(UnitarySysNum)%LatentLoadMet) > 0.0d0) THEN
        IF (ABS(UnitarySystem(UnitarySysNum)%LatLoadLoss/UnitarySystem(UnitarySysNum)%LatentLoadMet) < 0.001d0) &
             UnitarySystem(UnitarySysNum)%LatLoadLoss = 0.0d0
      END IF
    END IF
  END IF

  IF (UnitarySystem(UnitarySysNum)%FanOpModeSchedPtr .GT. 0) THEN
    IF (GetCurrentScheduleValue(UnitarySystem(UnitarySysNum)%FanOpModeSchedPtr) .EQ. 0.0d0) THEN
      UnitarySystem(UnitarySysNum)%FanOpMode = CycFanCycCoil
    ELSE
      UnitarySystem(UnitarySysNum)%FanOpMode = ContFanCycCoil
      OnOffFanPartLoadFraction = 1.0d0
    END IF
  END IF

!  OpMode = UnitarySystem(UnitarySysNum)%FanOpMode
  IF(ALLOCATED(AirLoopControlInfo) .AND. UnitarySystem(UnitarySysNum)%AirLoopEquipment)THEN
    EconomizerFlag = AirLoopControlInfo(AirLoopNum)%EconoActive
  ELSE
    EconomizerFlag = .FALSE.
  END IF

! System load calculation for cycling fan systems
  IF(UnitarySystem(UnitarySysNum)%ControlZoneMassFlowFrac .GT. 0.0d0)THEN
    QZnReq = ZoneLoad/UnitarySystem(UnitarySysNum)%ControlZoneMassFlowFrac
    MoistureLoad = MoistureLoad/UnitarySystem(UnitarySysNum)%ControlZoneMassFlowFrac
    QToCoolSetPt=QToCoolSetPt/UnitarySystem(UnitarySysNum)%ControlZoneMassFlowFrac
    QToHeatSetPt=QToHeatSetPt/UnitarySystem(UnitarySysNum)%ControlZoneMassFlowFrac
    ZoneLoad = QZnReq
  ELSE
    QZnReq = ZoneLoad
    UnitarySystem(UnitarySysNum)%ControlZoneMassFlowFrac = 1.0d0
  END IF

  CoolingLoad = .FALSE.
  HeatingLoad = .FALSE.

  IF(QZnReq .GT.  Small5WLoad/UnitarySystem(UnitarySysNum)%ControlZoneMassFlowFrac .AND. &
     .NOT. CurDeadbandOrSetback(UnitarySystem(UnitarySysNum)%ControlZoneNum))THEN
    IF(TempControlType(UnitarySystem(UnitarySysNum)%ControlZoneNum) .NE. SingleCoolingSetPoint)THEN
      HeatingLoad = .TRUE.
    END IF
  ELSEIF(QZnReq .LT. -Small5WLoad/UnitarySystem(UnitarySysNum)%ControlZoneMassFlowFrac .AND. &
     .NOT. CurDeadbandOrSetback(UnitarySystem(UnitarySysNum)%ControlZoneNum))THEN
    IF(TempControlType(UnitarySystem(UnitarySysNum)%ControlZoneNum) .NE. SingleHeatingSetPoint)THEN
      CoolingLoad = .TRUE.
    END IF
  END IF

! System load calculation for constant fan systems
  IF(UnitarySystem(UnitarySysNum)%FanOpMode == ContFanCycCoil)THEN
    HXUnitOn = .FALSE.
    CALL CalcUnitarySystemToLoad(UnitarySysNum,FirstHVACIteration,0.0d0,0.0d0, &
                                 OnOffAirFlowRatio,SensOutputOff,LatOutputOff,HXUnitOn)
    SELECT CASE(TempControlType(UnitarySystem(UnitarySysNum)%ControlZoneNum))
    CASE(SingleHeatingSetPoint)
      CoolingLoad = .FALSE.
      ! No heating load and constant fan pushes zone below heating set point
      IF(SensOutputOff .LT. 0.0d0 .AND. QToHeatSetPt .LT. 0.0d0 .AND. &
         SensOutputOff-QToHeatSetPt .LT. -SmallLoad)THEN
        HeatingLoad = .TRUE.
        CoolingLoad = .FALSE.
        ZoneLoad = QToHeatSetPt
      END IF
    CASE(SingleCoolingSetPoint)
      HeatingLoad = .FALSE.
      ! No heating load and constant fan pushes zone above cooling set point
      IF(SensOutputOff .GT. 0.0d0 .AND. QToCoolSetPt .GT. 0.0d0 .AND. &
         SensOutputOff-QToCoolSetPt .GT. SmallLoad)THEN
        HeatingLoad = .FALSE.
        CoolingLoad = .TRUE.
        ZoneLoad = QToCoolSetPt
      END IF
    CASE(SingleHeatCoolSetPoint)
      ! zone temp above cooling and heating set point temps
      IF(QToHeatSetPt .LT. 0.0d0 .AND. QToCoolSetPt .LT. 0.0d0)THEN
        ! zone pushed below heating set point
        IF(SensOutputOff .LT. 0.0d0 .AND. &
           QToHeatSetPt-SensOutputOff .GT. SmallLoad)THEN
          HeatingLoad = .TRUE.
          CoolingLoad = .FALSE.
          ZoneLoad = QToHeatSetPt
        END IF
      ! zone temp below heating set point temp
      ELSEIF(QToHeatSetPt .GT. 0.0d0 .AND. QToCoolSetPt .GT. 0.0d0)THEN
        ! zone pushed above cooling set point
        IF(SensOutputOff .GT. 0.0d0 .AND. &
           QToCoolSetPt-SensOutputOff .GT. SmallLoad)THEN
          HeatingLoad = .FALSE.
          CoolingLoad = .TRUE.
          ZoneLoad = QToCoolSetPt
        END IF
      END IF
    CASE(DualSetPointWithDeadBand)
      ! zone temp above cooling and heating set point temps
      IF(QToHeatSetPt .LT. 0.0d0 .AND. QToCoolSetPt .LT. 0.0d0)THEN
        ! zone pushed into deadband
        IF(SensOutputOff .LT. 0.0d0 .AND. &
           QToCoolSetPt-SensOutputOff .GT. SmallLoad)THEN
          HeatingLoad = .FALSE.
          CoolingLoad = .FALSE.
          ZoneLoad = 0.0d0
        END IF
        ! zone pushed below heating set point
        IF(SensOutputOff .LT. 0.0d0 .AND. &
           QToHeatSetPt-SensOutputOff .GT. SmallLoad)THEN
          HeatingLoad = .TRUE.
          CoolingLoad = .FALSE.
          ZoneLoad = QToHeatSetPt
        END IF
      ! zone temp below heating set point temp
      ELSEIF(QToHeatSetPt .GT. 0.0d0 .AND. QToCoolSetPt .GT. 0.0d0)THEN
        ! zone pushed into deadband
        IF(SensOutputOff .GT. 0.0d0 .AND. &
           SensOutputOff-QToHeatSetPt .GT. SmallLoad)THEN
          HeatingLoad = .FALSE.
          CoolingLoad = .FALSE.
          ZoneLoad = 0.0d0
        END IF
        ! zone pushed above cooling set point
        IF(SensOutputOff .GT. 0.0d0 .AND. &
           SensOutputOff-QToCoolSetPt .GT. SmallLoad)THEN
          HeatingLoad = .FALSE.
          CoolingLoad = .TRUE.
          ZoneLoad = QToCoolSetPt
        END IF
      ! zone temp between set point temps
      ELSEIF(QToHeatSetPt .LT. 0.0d0 .AND. QToCoolSetPt .GT. 0.0d0)THEN
        ! zone pushed below heating set point
        IF(SensOutputOff .LT. 0.0d0 .AND. &
           SensOutputOff-QToHeatSetPt .LT. -SmallLoad)THEN
          HeatingLoad = .TRUE.
          CoolingLoad = .FALSE.
          ZoneLoad = QToHeatSetPt
        ! zone pushed above cooling set point
        ELSEIF(SensOutputOff .GT. 0.0d0 .AND. &
                SensOutputOff-QToCoolSetPt .GT. SmallLoad)THEN
          HeatingLoad = .FALSE.
          CoolingLoad = .TRUE.
          ZoneLoad = QToCoolSetPt
        END IF
      END IF
    CASE DEFAULT
    END SELECT

    ! IF small loads to meet, just shut down unit
    IF(ABS(ZoneLoad) < Small5WLoad)THEN
      ZoneLoad = 0.0d0
      CoolingLoad = .FALSE.
      HeatingLoad = .FALSE.
    END IF

  END IF

  ! Determine the staged status
  IF (ALLOCATED(StageZoneLogic) .AND.  UnitarySystem(UnitarySysNum)%DesignSpecMSHPIndex > 0) THEN
    IF (StageZoneLogic(UnitarySystem(UnitarySysNum)%ControlZoneNum)) THEN
      UnitarySystem(UnitarySysNum)%Staged = .TRUE.
      UnitarySystem(UnitarySysNum)%StageNum = ZoneSysEnergyDemand(UnitarySystem(UnitarySysNum)%ControlZoneNum)%StageNum
    ELSE
      IF (MyStagedFlag(UnitarySysNum)) THEN
        CALL ShowWarningError('ZoneControl:Thermostat:StagedDualSetpoint is found, but is not applied to '// &
             'this AirLoopHVAC:UnitarySystem object with UnitarySystemPerformance:HeatPump:Multispeed type = ')
        CALL ShowContinueError(Trim(UnitarySystem(UnitarySysNum)%Name) //'. Please make correction. Simulation continues...')
        MyStagedFlag(UnitarySysNum) = .FALSE.
      END IF
    END IF
  END IF

  ! Staged control
  IF (UnitarySystem(UnitarySysNum)%Staged) THEN
    IF (UnitarySystem(UnitarySysNum)%StageNum == 0) THEN
      HeatingLoad = .FALSE.
      CoolingLoad = .FALSE.
      QZnReq = 0.0d0
    ELSE
      QZnReq = ZoneSysEnergyDemand(UnitarySystem(UnitarySysNum)%ControlZoneNum)%RemainingOutputRequired/ &
               UnitarySystem(UnitarySysNum)%ControlZoneMassFlowFrac
      IF (UnitarySystem(UnitarySysNum)%StageNum > 0) THEN
        HeatingLoad = .TRUE.
        CoolingLoad = .FALSE.
      ELSE
        HeatingLoad = .FALSE.
        CoolingLoad = .TRUE.
      END IF
    END IF
  END IF

  IF(UnitarySystem(UnitarySysNum)%DehumidControlType_Num == DehumidControl_MultiMode) THEN
    IF(HeatingLoad)MoistureLoad = 0.0d0
  END IF

  ! Check load control
  IF(UnitarySystem(UnitarySysNum)%RunOnLatentOnlyWithSensible .AND. ZoneLoad == 0.0d0)MoistureLoad = 0.0d0
  IF(.NOT. UnitarySystem(UnitarySysNum)%RunOnSensibleLoad)THEN
    ZoneLoad = 0.0d0
    CoolingLoad = .FALSE.
    HeatingLoad = .FALSE.
  END IF
  IF(.NOT. UnitarySystem(UnitarySysNum)%RunOnLatentLoad)MoistureLoad = 0.0d0

  ! Testing heat pump air to air with RH control with CoolReheat dehumidifaction control showed that when there was heating
  ! and moisture load, the cooling coil was turning on to meet the moisture load and reheat was then turning on to meet both
  ! heating load and excess cooling load caused by cooling coil. Adding the logic below caused the zone temperature,
  ! relative humidity, cooling/heating rate to line up for both the orignal and new file with unitary system object.

   IF(UnitarySystem(UnitarySysNum)%SuppCoilExists)THEN
     IF(UnitarySystem(UnitarySysNum)%DehumidControlType_Num == DehumidControl_CoolReheat) THEN
       IF (MoistureLoad < 0.0d0 .AND. UnitarySystem(UnitarySysNum)%HeatPump) THEN
         HeatingLoad = .FALSE.
         CoolingLoad = .TRUE.
       END IF
     END IF
   END IF

RETURN
END SUBROUTINE InitLoadBasedControl

! End of Initialization subroutines for the Module
! *****************************************************************************

SUBROUTINE SizeUnitarySystem(UnitarySysNum, FirstHVACIteration, AirLoopNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Raustad, FSEC
          !       DATE WRITTEN   February 2013
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is for sizing unitary system components for which nominal cpacities
          ! and flow rates have not been specified in the input. Coil sizing is preformed in the coil module.
          ! Future modifications will size coils here and "push" this info to the specific coil.

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
  USE BranchInputManager,        ONLY: CheckSystemBranchFlow, GetAirBranchIndex, GetBranchFanTypeName, GetBranchFlow
  USE DataAirSystems,            ONLY: PrimaryAirSystem
  USE CurveManager,              ONLY: CurveValue
  USE DXCoils,                   ONLY: SimDXCoil, SimDXCoilMultiSpeed, GetCoilCapacityByIndexType, &
                                       GetDXCoilCapFTCurveIndex !, SetDXCoolingCoilData
  USE Fans,                      ONLY: GetFanDesignVolumeFlowRate
  USE HVACHXAssistedCoolingCoil, ONLY: SimHXAssistedCoolingCoil,GetCoilCapacity,GetHXDXCoilName,GetCoilObjectTypeNum
  USE ReportSizingManager,       ONLY: ReportSizingOutput
  USE VariableSpeedCoils,        ONLY: SimVariableSpeedCoils, VarSpeedCoil, GetCoilCapacityVariableSpeed
  USE WatertoAirHeatPump,        ONLY: SimWaterToAirHP,GetWAHPCoilCapacity=>GetCoilCapacity
  USE WatertoAirHeatPumpSimple,  ONLY: SimWatertoAirHPSimple,GetSimpleCoilCapacity=>GetCoilCapacity
  USE PlantUtilities,            ONLY: RegisterPlantCompDesignFlow
  USE InputProcessor,            ONLY: SameString, FindItemInList,MakeUPPERCase
  USE General,                   ONLY: TrimSigDigits
  USE WaterCoils,                ONLY: SetCoilDesFlow,GetWaterCoilCapacity
  USE Psychrometrics
  USE HVACDXSystem,              ONLY: GetCoolingCoilTypeNameAndIndex
  USE EMSManager,                ONLY: ManageEMS
  USE DataGlobals,               ONLY: emsCallFromUnitarySystemSizing

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: UnitarySysNum
  LOGICAL, INTENT(IN) :: FirstHVACIteration
  INTEGER, INTENT(IN) :: AirLoopNum ! does this need to be optional?

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER ::  RoutineName='SizeUnitarySystem'
  REAL(r64), PARAMETER ::    MaxRatedVolFlowPerRatedTotCap1 = 0.00006041d0 ! m3/s per watt = 450 cfm/ton
  REAL(r64), PARAMETER ::    MinRatedVolFlowPerRatedTotCap1 = 0.00004027d0 ! m3/s per watt = 300 cfm/ton

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER   :: ThisCtrlZoneNum    ! the controlled zone number of the control zone !!!
  INTEGER   :: Iter               ! iteration count
  REAL(r64) :: MulSpeedFlowScale  ! variable speed air flow scaling factor
  INTEGER   :: MSHPIndex          ! Index to design Specification object
  INTEGER   :: BranchNum          ! Index to branch on air loop
  INTEGER   :: CompNum            ! Index to component on branch
  REAL(r64) :: BranchFlow         ! branch flow rate [m3/s]
  REAL(r64) :: BranchFanFlow      ! branch fan flow rate [m3/s]
  LOGICAL   :: ErrFound           ! logical error flag
  CHARACTER(len=MaxNameLength) :: FanType  ! fan type
  CHARACTER(len=MaxNameLength) :: FanName  ! fan name
  INTEGER   :: DXHeatCoilBranch   ! branch where DX heating coil is located
  INTEGER   :: DXHeatCoilCompNum  ! comp number of DX heating coil on branch
  INTEGER   :: CoolUnitarySystemNum ! index to unitary system with DX cooling coil
  INTEGER   :: CoolCoilIndex      ! index to DX cooling coil
  INTEGER   :: CoolCoilType       ! type of DX cooling coil
  CHARACTER(len=MaxNameLength) :: CoolCoilName ! name of DX cooling coil
  REAL(r64) :: rhoair             ! air density [kg/m3]
  REAL(r64) :: CpAirStd           ! air specific heat [J/kg-k]
  REAL(r64) :: MixTemp            ! mixed air temp used for sizing [C]
  REAL(r64) :: MixHumRat          ! mixed air humidity ratio used for sizing [kg/kg]
  REAL(r64) :: MixEnth            ! mixed air enthalpy used for sizing [J/kg]
  REAL(r64) :: MixWetBulb         ! mixed air wet bulb temp used for sizing [C]
  REAL(r64) :: SupTemp            ! supply air temp used for sizing [C]
  REAL(r64) :: SupHumRat          ! supply air humidity ratio used for sizing [kg/kg]
  REAL(r64) :: SupEnth            ! supply air enthalpy used for sizing [J/kg]
  REAL(r64) :: OutTemp            ! outdoor air temp used for sizing [C]
  REAL(r64) :: OutAirFrac         ! fraction of outdoor air flow
  REAL(r64) :: VolFlowRate        ! volumetric flow rate [m3/s]
  REAL(r64) :: CoolCapAtPeak      ! cooling capacity at peak [W]
  REAL(r64) :: HeatCapAtPeak      ! heating capacity at peak [W]
  INTEGER   :: TimeStepNumAtMax   ! time step at peak
  INTEGER   :: DDNum              ! design day index at peak
  REAL(r64) :: TotCapTempModFac   ! capacity modifier used for sizing
  REAL(r64) :: RatedVolFlowPerRatedTotCap !Rated Air Volume Flow Rate divided by Rated Total Capacity[m3/s-W)
  CHARACTER(len=MaxNameLength) :: SystemType ! type of air loop equipment
  REAL(r64) :: OnOffAirFlowRatio  ! used to pass to cooling coil for sizing
  REAL(r64) :: PartLoadRatio      ! used to pass to cooling coil for sizing
  INTEGER   :: CapFTCurve         ! index to DX coil Capacity as a function of Temperature curve
  LOGICAL   :: TempCoolingLoad    ! size cooling coils with a cooling load, save actual load
  LOGICAL   :: TempHeatingLoad    ! save actual load
  REAL(r64) :: SysCoolingFlow     ! individually sized cooling flow rate [m3/s]
  REAL(r64) :: SysHeatingFlow     ! individually sized heating flow rate [m3/s]
  CHARACTER(len=MaxNameLength) :: HXCoilName         ! cooling coil name in HXAssisted parent
  INTEGER :: ActualCoolCoilType ! cooling coil type in HXAssisted parent

  CALL ManageEMS(emsCallFromUnitarySystemSizing) ! calling point

  ThisCtrlZoneNum = 0
  DXCoolCap = 0.0d0
  UnitaryHeatCap = 0.0d0
  SuppHeatCap = 0.0d0
  TempCoolingLoad = CoolingLoad
  TempHeatingLoad = HeatingLoad
  CoolingLoad = .TRUE.
  HeatingLoad = .FALSE.
  ZoneCoolingOnlyFan = .FALSE.
  ZoneHeatingOnlyFan = .FALSE.
  SysCoolingFlow = 0.0d0
  SysHeatingFlow = 0.0d0
  CoolCapAtPeak = 0.0d0
  HeatCapAtPeak = 0.0d0

!  IF(UnitarySystem(UnitarySysNum)%CoolCoilExists)THEN
!    IF(
!    UnitarySystem(UnitarySysNum)%DesignCoolingCapacity =    &
!     GetWaterHXAssistedCoilCapacity(CoolingCoilType,CoolingCoilName,ErrFlag)
!    IF(UnitarySystem(UnitarySysNum)%DesignCoolingCapacity == Autosize) &
!        UnitarySystem(UnitarySysNum)%RequestAutosize = .TRUE.
!    IF (ErrFlag) THEN
!      CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
!      ErrorsFound=.TRUE.
!    END IF
!  END IF

  IF ((CurOASysNum > 0 .OR. CurSysNum > 0) .AND. UnitarySystem(UnitarySysNum)%RequestAutosize) THEN
    CALL CheckSysSizing(UnitarySystem(UnitarySysNum)%UnitarySystemType, UnitarySystem(UnitarySysNum)%Name)
  ELSE IF(CurZoneEqNum > 0)THEN
    CALL CheckZoneSizing(UnitarySystem(UnitarySysNum)%UnitarySystemType, UnitarySystem(UnitarySysNum)%Name)
  END IF

  IF(UnitarySystem(UnitarySysNum)%MaxCoolAirVolFlow /= AutoSize) &
    SysCoolingFlow = UnitarySystem(UnitarySysNum)%MaxCoolAirVolFlow
  IF(UnitarySystem(UnitarySysNum)%MaxHeatAirVolFlow /= AutoSize) &
    SysHeatingFlow = UnitarySystem(UnitarySysNum)%MaxHeatAirVolFlow

  IF(UnitarySystem(UnitarySysNum)%RequestAutosize)THEN
  IF (CurOASysNum > 0) THEN
!    CALL CheckSysSizing(UnitarySystem(UnitarySysNum)%UnitarySystemType, UnitarySystem(UnitarySysNum)%Name)
    IF(UnitarySystem(UnitarySysNum)%CoolCoilExists .AND. .NOT. UnitarySystem(UnitarySysNum)%HeatCoilExists)THEN
      SELECT CASE(UnitarySystem(UnitarySysNum)%CoolingSAFMethod)
        CASE (SupplyAirFlowRate, None)
          IF(UnitarySystem(UnitarySysNum)%MaxCoolAirVolFlow == Autosize)THEN
            SysCoolingFlow = FinalSysSizing(CurSysNum)%DesOutAirVolFlow
          ELSE
            SysCoolingFlow = UnitarySystem(UnitarySysNum)%MaxCoolAirVolFlow
          END IF
          OASysEqSizing(CurOASysNum)%AirFlow = .TRUE.
          OASysEqSizing(CurOASysNum)%AirVolFlow = SysCoolingFlow
        CASE (FlowPerFloorArea)
          SysCoolingFlow = UnitarySystem(UnitarySysNum)%MaxCoolAirVolFlow
          UnitarySystem(UnitarySysNum)%MaxCoolAirVolFlow = Autosize
          OASysEqSizing(CurOASysNum)%AirFlow = .TRUE.
          OASysEqSizing(CurOASysNum)%AirVolFlow = SysCoolingFlow
        CASE (FractionOfAutosizedCoolingValue)
          SysCoolingFlow = FinalSysSizing(CurSysNum)%DesOutAirVolFlow * &
                           UnitarySystem(UnitarySysNum)%MaxCoolAirVolFlow
          UnitarySystem(UnitarySysNum)%MaxCoolAirVolFlow = Autosize
          OASysEqSizing(CurOASysNum)%AirFlow = .TRUE.
          OASysEqSizing(CurOASysNum)%AirVolFlow = SysCoolingFlow
        CASE (FlowPerCoolingCapacity)
          VolFlowRate = FinalSysSizing(CurSysNum)%DesOutAirVolFlow
          MixTemp = FinalSysSizing(CurSysNum)%CoolOutTemp
          MixHumRat = FinalSysSizing(CurSysNum)%CoolOutHumRat
          SupTemp = FinalSysSizing(CurSysNum)%PrecoolTemp
          SupHumRat = FinalSysSizing(CurSysNum)%PrecoolHumRat
          OutTemp = FinalSysSizing(CurSysNum)%CoolOutTemp
          rhoair = PsyRhoAirFnPbTdbW(StdBaroPress,MixTemp,MixHumRat,RoutineName)
          MixEnth = PsyHFnTdbW(MixTemp,MixHumRat,RoutineName)
          MixWetBulb = PsyTwbFnTdbWPb(MixTemp,MixHumRat,StdBaroPress,RoutineName)
          SupEnth = PsyHFnTdbW(SupTemp,SupHumRat,RoutineName)
          IF(UnitarySystem(UnitarySysNum)%CoolingCoilType_Num == CoilDX_CoolingSingleSpeed .OR. &
             UnitarySystem(UnitarySysNum)%CoolingCoilType_Num == CoilDX_MultiSpeedCooling .OR. &
             UnitarySystem(UnitarySysNum)%CoolingCoilType_Num == CoilDX_CoolingTwoSpeed .OR. &
             UnitarySystem(UnitarySysNum)%CoolingCoilType_Num == CoilDX_CoolingTwoStageWHumControl)THEN
              CapFTCurve = GetDXCoilCapFTCurveIndex(UnitarySystem(UnitarySysNum)%CoolingCoilIndex,ErrFound)
            TotCapTempModFac = CurveValue(CapFTCurve,MixWetBulb,OutTemp)
          ELSE
            TotCapTempModFac = 1.0d0
          END IF
          CoolCapAtPeak = MAX(0.d0, (rhoair * VolFlowRate * (MixEnth-SupEnth)))
          IF(TotCapTempModFac .GT. 0.d0)THEN
            CoolCapAtPeak = CoolCapAtPeak / TotCapTempModFac
          END IF
          SysCoolingFlow = CoolCapAtPeak * UnitarySystem(UnitarySysNum)%MaxCoolAirVolFlow
          UnitarySystem(UnitarySysNum)%MaxCoolAirVolFlow = Autosize
          OASysEqSizing(CurOASysNum)%AirFlow = .TRUE.
          OASysEqSizing(CurOASysNum)%AirVolFlow = SysCoolingFlow
          OASysEqSizing(CurOASysNum)%Capacity = .TRUE.
          OASysEqSizing(CurOASysNum)%DesCoolingLoad = CoolCapAtPeak
        CASE DEFAULT
          IF(UnitarySystem(UnitarySysNum)%MaxCoolAirVolFlow == Autosize)THEN
            SysCoolingFlow = FinalSysSizing(CurSysNum)%DesOutAirVolFlow
          ELSE
            SysCoolingFlow = UnitarySystem(UnitarySysNum)%MaxCoolAirVolFlow
          END IF
      END SELECT
    ELSE IF(UnitarySystem(UnitarySysNum)%HeatCoilExists .AND. .NOT. UnitarySystem(UnitarySysNum)%CoolCoilExists)THEN
      SELECT CASE(UnitarySystem(UnitarySysNum)%HeatingSAFMethod)
        CASE (SupplyAirFlowRate, None)
          IF(UnitarySystem(UnitarySysNum)%MaxHeatAirVolFlow == Autosize)THEN
            SysHeatingFlow = FinalSysSizing(CurSysNum)%DesOutAirVolFlow
          ELSE
            SysHeatingFlow = UnitarySystem(UnitarySysNum)%MaxHeatAirVolFlow
          END IF
          OASysEqSizing(CurOASysNum)%AirFlow = .TRUE.
          OASysEqSizing(CurOASysNum)%AirVolFlow = SysHeatingFlow
        CASE (FlowPerFloorArea)
          SysHeatingFlow = UnitarySystem(UnitarySysNum)%MaxHeatAirVolFlow
          UnitarySystem(UnitarySysNum)%MaxHeatAirVolFlow = Autosize
          OASysEqSizing(CurOASysNum)%AirFlow = .TRUE.
          OASysEqSizing(CurOASysNum)%AirVolFlow = SysHeatingFlow
        CASE (FractionOfAutosizedCoolingValue)
          SysHeatingFlow = FinalSysSizing(CurSysNum)%DesOutAirVolFlow * &
                           UnitarySystem(UnitarySysNum)%MaxHeatAirVolFlow
          UnitarySystem(UnitarySysNum)%MaxHeatAirVolFlow = Autosize
          OASysEqSizing(CurOASysNum)%AirFlow = .TRUE.
          OASysEqSizing(CurOASysNum)%AirVolFlow = SysHeatingFlow
        CASE (FlowPerHeatingCapacity)
          VolFlowRate = FinalSysSizing(CurSysNum)%DesOutAirVolFlow
          MixTemp = FinalSysSizing(CurSysNum)%HeatOutTemp
          MixHumRat = FinalSysSizing(CurSysNum)%HeatOutHumRat
          SupTemp = FinalSysSizing(CurSysNum)%PrecoolTemp
          SupHumRat = FinalSysSizing(CurSysNum)%PrecoolHumRat
          OutTemp = FinalSysSizing(CurSysNum)%HeatOutTemp
          rhoair = PsyRhoAirFnPbTdbW(StdBaroPress,MixTemp,MixHumRat,RoutineName)
          MixEnth = PsyHFnTdbW(MixTemp,MixHumRat,RoutineName)
          MixWetBulb = PsyTwbFnTdbWPb(MixTemp,MixHumRat,StdBaroPress,RoutineName)
          SupEnth = PsyHFnTdbW(SupTemp,SupHumRat,RoutineName)
          IF(UnitarySystem(UnitarySysNum)%HeatingCoilType_Num == CoilDX_MultiSpeedHeating .OR. &
             UnitarySystem(UnitarySysNum)%HeatingCoilType_Num == CoilDX_HeatingEmpirical)THEN
              CapFTCurve = GetDXCoilCapFTCurveIndex(UnitarySystem(UnitarySysNum)%HeatingCoilIndex,ErrFound)
            TotCapTempModFac = CurveValue(CapFTCurve,MixWetBulb,OutTemp)
          ELSE
            TotCapTempModFac = 1.0d0
          END IF
          HeatCapAtPeak = MAX(0.d0, (rhoair * VolFlowRate * (SupEnth-MixEnth)))
          IF(TotCapTempModFac .GT. 0.d0)THEN
            HeatCapAtPeak = HeatCapAtPeak / TotCapTempModFac
          END IF
          SysHeatingFlow = HeatCapAtPeak * UnitarySystem(UnitarySysNum)%MaxHeatAirVolFlow
          UnitarySystem(UnitarySysNum)%MaxHeatAirVolFlow = Autosize
          OASysEqSizing(CurOASysNum)%AirFlow = .TRUE.
          OASysEqSizing(CurOASysNum)%AirVolFlow = SysCoolingFlow
          OASysEqSizing(CurOASysNum)%Capacity = .TRUE.
          OASysEqSizing(CurOASysNum)%DesCoolingLoad = HeatCapAtPeak
        CASE DEFAULT
          IF(UnitarySystem(UnitarySysNum)%MaxHeatAirVolFlow == Autosize)THEN
            SysHeatingFlow = FinalSysSizing(CurSysNum)%DesOutAirVolFlow
          ELSE
            SysHeatingFlow = UnitarySystem(UnitarySysNum)%MaxHeatAirVolFlow
          END IF
          OASysEqSizing(CurOASysNum)%AirFlow = .TRUE.
          OASysEqSizing(CurOASysNum)%AirVolFlow = SysCoolingFlow
      END SELECT
    ELSE ! Cooling and Heating coil are present
      SELECT CASE(UnitarySystem(UnitarySysNum)%CoolingSAFMethod)
        CASE (SupplyAirFlowRate, None)
          IF(UnitarySystem(UnitarySysNum)%MaxCoolAirVolFlow == Autosize)THEN
            SysCoolingFlow = FinalSysSizing(CurSysNum)%DesOutAirVolFlow
          ELSE
            SysCoolingFlow = UnitarySystem(UnitarySysNum)%MaxCoolAirVolFlow
          END IF
          OASysEqSizing(CurOASysNum)%AirFlow = .TRUE.
        CASE (FlowPerFloorArea)
          SysCoolingFlow = MAX(UnitarySystem(UnitarySysNum)%MaxCoolAirVolFlow, &
                               UnitarySystem(UnitarySysNum)%MaxHeatAirVolFlow)
          UnitarySystem(UnitarySysNum)%MaxCoolAirVolFlow = Autosize
          OASysEqSizing(CurOASysNum)%AirFlow = .TRUE.
        CASE (FractionOfAutosizedCoolingValue)
          SysCoolingFlow = FinalSysSizing(CurSysNum)%DesOutAirVolFlow * &
              MAX(UnitarySystem(UnitarySysNum)%MaxCoolAirVolFlow, UnitarySystem(UnitarySysNum)%MaxHeatAirVolFlow)
          UnitarySystem(UnitarySysNum)%MaxCoolAirVolFlow = SysCoolingFlow
          OASysEqSizing(CurOASysNum)%AirFlow = .TRUE.
        CASE (FlowPerCoolingCapacity)
          VolFlowRate = FinalSysSizing(CurSysNum)%DesOutAirVolFlow
          MixTemp = FinalSysSizing(CurSysNum)%CoolOutTemp
          MixHumRat = FinalSysSizing(CurSysNum)%CoolOutHumRat
          SupTemp = FinalSysSizing(CurSysNum)%PrecoolTemp
          SupHumRat = FinalSysSizing(CurSysNum)%PrecoolHumRat
          OutTemp = FinalSysSizing(CurSysNum)%CoolOutTemp
          rhoair = PsyRhoAirFnPbTdbW(StdBaroPress,MixTemp,MixHumRat,RoutineName)
          MixEnth = PsyHFnTdbW(MixTemp,MixHumRat,RoutineName)
          MixWetBulb = PsyTwbFnTdbWPb(MixTemp,MixHumRat,StdBaroPress,RoutineName)
          SupEnth = PsyHFnTdbW(SupTemp,SupHumRat,RoutineName)
          IF(UnitarySystem(UnitarySysNum)%CoolingCoilType_Num == CoilDX_CoolingSingleSpeed .OR. &
             UnitarySystem(UnitarySysNum)%CoolingCoilType_Num == CoilDX_MultiSpeedCooling .OR. &
             UnitarySystem(UnitarySysNum)%CoolingCoilType_Num == CoilDX_CoolingTwoSpeed .OR. &
             UnitarySystem(UnitarySysNum)%CoolingCoilType_Num == CoilDX_CoolingTwoStageWHumControl)THEN
              CapFTCurve = GetDXCoilCapFTCurveIndex(UnitarySystem(UnitarySysNum)%CoolingCoilIndex,ErrFound)
            TotCapTempModFac = CurveValue(CapFTCurve,MixWetBulb,OutTemp)
          ELSE
            TotCapTempModFac = 1.0d0
          END IF
          CoolCapAtPeak = MAX(0.d0, (rhoair * VolFlowRate * (MixEnth-SupEnth)))
          IF(TotCapTempModFac .GT. 0.d0)THEN
            CoolCapAtPeak = CoolCapAtPeak / TotCapTempModFac
          END IF
          SysCoolingFlow = CoolCapAtPeak * UnitarySystem(UnitarySysNum)%MaxCoolAirVolFlow
          UnitarySystem(UnitarySysNum)%MaxCoolAirVolFlow = Autosize
          OASysEqSizing(CurOASysNum)%AirFlow = .TRUE.
          OASysEqSizing(CurOASysNum)%Capacity = .TRUE.
        CASE DEFAULT
          IF(UnitarySystem(UnitarySysNum)%MaxCoolAirVolFlow == Autosize)THEN
            SysCoolingFlow = FinalSysSizing(CurSysNum)%DesOutAirVolFlow
          ELSE
            SysCoolingFlow = UnitarySystem(UnitarySysNum)%MaxCoolAirVolFlow
          END IF
          OASysEqSizing(CurOASysNum)%AirFlow = .TRUE.
      END SELECT
      SELECT CASE(UnitarySystem(UnitarySysNum)%HeatingSAFMethod)
        CASE (SupplyAirFlowRate, None)
          IF(UnitarySystem(UnitarySysNum)%MaxHeatAirVolFlow == Autosize)THEN
            SysHeatingFlow = FinalSysSizing(CurSysNum)%DesOutAirVolFlow
          ELSE
            SysHeatingFlow = UnitarySystem(UnitarySysNum)%MaxHeatAirVolFlow
          END IF
          OASysEqSizing(CurOASysNum)%AirFlow = .TRUE.
        CASE (FlowPerFloorArea)
          SysHeatingFlow = UnitarySystem(UnitarySysNum)%MaxHeatAirVolFlow
          UnitarySystem(UnitarySysNum)%MaxHeatAirVolFlow = Autosize
          OASysEqSizing(CurOASysNum)%AirFlow = .TRUE.
        CASE (FractionOfAutosizedCoolingValue)
          SysHeatingFlow = FinalSysSizing(CurSysNum)%DesOutAirVolFlow * &
              UnitarySystem(UnitarySysNum)%MaxHeatAirVolFlow
          UnitarySystem(UnitarySysNum)%MaxHeatAirVolFlow = SysHeatingFlow
          OASysEqSizing(CurOASysNum)%AirFlow = .TRUE.
        CASE (FlowPerHeatingCapacity)
          VolFlowRate = FinalSysSizing(CurSysNum)%DesOutAirVolFlow
          MixTemp = FinalSysSizing(CurSysNum)%HeatOutTemp
          MixHumRat = FinalSysSizing(CurSysNum)%HeatOutHumRat
          SupTemp = FinalSysSizing(CurSysNum)%PrecoolTemp
          SupHumRat = FinalSysSizing(CurSysNum)%PrecoolHumRat
          OutTemp = FinalSysSizing(CurSysNum)%HeatOutTemp
          rhoair = PsyRhoAirFnPbTdbW(StdBaroPress,MixTemp,MixHumRat,RoutineName)
          MixEnth = PsyHFnTdbW(MixTemp,MixHumRat,RoutineName)
          MixWetBulb = PsyTwbFnTdbWPb(MixTemp,MixHumRat,StdBaroPress,RoutineName)
          SupEnth = PsyHFnTdbW(SupTemp,SupHumRat,RoutineName)
          IF(UnitarySystem(UnitarySysNum)%HeatingCoilType_Num == CoilDX_MultiSpeedHeating .OR. &
             UnitarySystem(UnitarySysNum)%HeatingCoilType_Num == CoilDX_HeatingEmpirical)THEN
              CapFTCurve = GetDXCoilCapFTCurveIndex(UnitarySystem(UnitarySysNum)%HeatingCoilIndex,ErrFound)
            TotCapTempModFac = CurveValue(CapFTCurve,MixWetBulb,OutTemp)
          ELSE
            TotCapTempModFac = 1.0d0
          END IF
          HeatCapAtPeak = MAX(0.d0, (rhoair * VolFlowRate * (SupEnth-MixEnth)))
          IF(TotCapTempModFac .GT. 0.d0)THEN
            HeatCapAtPeak = HeatCapAtPeak / TotCapTempModFac
          END IF
          SysHeatingFlow = HeatCapAtPeak * UnitarySystem(UnitarySysNum)%MaxHeatAirVolFlow
          UnitarySystem(UnitarySysNum)%MaxHeatAirVolFlow = Autosize
          OASysEqSizing(CurOASysNum)%AirFlow = .TRUE.
          OASysEqSizing(CurOASysNum)%Capacity = .TRUE.
        CASE DEFAULT
          IF(UnitarySystem(UnitarySysNum)%MaxHeatAirVolFlow == Autosize)THEN
            SysHeatingFlow = FinalSysSizing(CurSysNum)%DesOutAirVolFlow
          ELSE
            SysHeatingFlow = UnitarySystem(UnitarySysNum)%MaxHeatAirVolFlow
          END IF
          OASysEqSizing(CurOASysNum)%AirFlow = .TRUE.
      END SELECT
      OASysEqSizing(CurOASysNum)%AirVolFlow = MAX(SysCoolingFlow,SysHeatingFlow)
      OASysEqSizing(CurOASysNum)%DesHeatingLoad = MAX(CoolCapAtPeak,HeatCapAtPeak)
    END IF
  ELSE

    IF (CurSysNum > 0) THEN

!      CALL CheckSysSizing(UnitarySystem(UnitarySysNum)%UnitarySystemType, UnitarySystem(UnitarySysNum)%Name)

      IF(UnitarySystem(UnitarySysNum)%CoolCoilExists .AND. .NOT. UnitarySystem(UnitarySysNum)%HeatCoilExists)THEN
        SELECT CASE(UnitarySystem(UnitarySysNum)%CoolingSAFMethod)
          CASE (SupplyAirFlowRate, None)
            IF(UnitarySystem(UnitarySysNum)%MaxCoolAirVolFlow == Autosize)THEN
              SysCoolingFlow = FinalSysSizing(CurSysNum)%DesMainVolFlow
            ELSE
              SysCoolingFlow = UnitarySystem(UnitarySysNum)%MaxCoolAirVolFlow
            END IF
            UnitarySysEqSizing(CurSysNum)%AirFlow = .TRUE.
            UnitarySysEqSizing(CurSysNum)%AirVolFlow = SysCoolingFlow
          CASE (FlowPerFloorArea)
            SysCoolingFlow = UnitarySystem(UnitarySysNum)%MaxCoolAirVolFlow
            UnitarySystem(UnitarySysNum)%MaxCoolAirVolFlow = Autosize
            UnitarySysEqSizing(CurSysNum)%AirFlow = .TRUE.
            UnitarySysEqSizing(CurSysNum)%AirVolFlow = SysCoolingFlow
          CASE (FractionOfAutosizedCoolingValue)
            SysCoolingFlow = FinalSysSizing(CurSysNum)%DesMainVolFlow * &
                             UnitarySystem(UnitarySysNum)%MaxCoolAirVolFlow
            UnitarySystem(UnitarySysNum)%MaxCoolAirVolFlow = Autosize
            UnitarySysEqSizing(CurSysNum)%AirFlow = .TRUE.
            UnitarySysEqSizing(CurSysNum)%AirVolFlow = SysCoolingFlow
          CASE (FlowPerCoolingCapacity)
            VolFlowRate = FinalSysSizing(CurSysNum)%DesMainVolFlow
            SupTemp = FinalSysSizing(CurSysNum)%CoolSupTemp
            SupHumRat = FinalSysSizing(CurSysNum)%CoolSupHumRat
            IF (PrimaryAirSystem(CurSysNum)%NumOACoolCoils == 0) THEN ! there is no precooling of the OA stream
              MixTemp = FinalSysSizing(CurSysNum)%CoolMixTemp
              MixHumRat = FinalSysSizing(CurSysNum)%CoolMixHumRat
            ELSE ! there is precooling of OA stream
              IF (VolFlowRate > 0.0d0) THEN
                OutAirFrac = FinalSysSizing(CurSysNum)%DesOutAirVolFlow / VolFlowRate
              ELSE
                OutAirFrac = 1.0d0
              END IF
              OutAirFrac = MIN(1.0d0,MAX(0.0d0,OutAirFrac))
              MixTemp = OutAirFrac*FinalSysSizing(CurSysNum)%PrecoolTemp + &
                          (1.0d0-OutAirFrac)*FinalSysSizing(CurSysNum)%CoolRetTemp
              MixHumRat = OutAirFrac*FinalSysSizing(CurSysNum)%PrecoolHumRat + &
                            (1.0d0-OutAirFrac)*FinalSysSizing(CurSysNum)%CoolRetHumRat
            END IF
            OutTemp = FinalSysSizing(CurSysNum)%CoolOutTemp
            rhoair = PsyRhoAirFnPbTdbW(StdBaroPress,MixTemp,MixHumRat,RoutineName)
            MixEnth = PsyHFnTdbW(MixTemp,MixHumRat,RoutineName)
            MixWetBulb = PsyTwbFnTdbWPb(MixTemp,MixHumRat,StdBaroPress,RoutineName)
            SupEnth = PsyHFnTdbW(SupTemp,SupHumRat,RoutineName)
            IF(UnitarySystem(UnitarySysNum)%CoolingCoilType_Num == CoilDX_CoolingSingleSpeed .OR. &
               UnitarySystem(UnitarySysNum)%CoolingCoilType_Num == CoilDX_MultiSpeedCooling .OR. &
               UnitarySystem(UnitarySysNum)%CoolingCoilType_Num == CoilDX_CoolingTwoSpeed .OR. &
               UnitarySystem(UnitarySysNum)%CoolingCoilType_Num == CoilDX_CoolingTwoStageWHumControl)THEN
                CapFTCurve = GetDXCoilCapFTCurveIndex(UnitarySystem(UnitarySysNum)%CoolingCoilIndex,ErrFound)
              TotCapTempModFac = CurveValue(CapFTCurve,MixWetBulb,OutTemp)
            ELSE
              TotCapTempModFac = 1.0d0
            END IF
            CoolCapAtPeak = MAX(0.d0, (rhoair * VolFlowRate * (MixEnth-SupEnth)))
            IF(TotCapTempModFac .GT. 0.d0)THEN
              CoolCapAtPeak = CoolCapAtPeak / TotCapTempModFac
            END IF
            SysCoolingFlow = CoolCapAtPeak * UnitarySystem(UnitarySysNum)%MaxCoolAirVolFlow
            UnitarySystem(UnitarySysNum)%MaxCoolAirVolFlow = Autosize
            UnitarySysEqSizing(CurSysNum)%AirFlow = .TRUE.
            UnitarySysEqSizing(CurSysNum)%AirVolFlow = SysCoolingFlow
            UnitarySysEqSizing(CurSysNum)%Capacity = .TRUE.
            UnitarySysEqSizing(CurSysNum)%DesCoolingLoad = CoolCapAtPeak
          CASE DEFAULT
            IF(UnitarySystem(UnitarySysNum)%MaxCoolAirVolFlow == Autosize)THEN
              SysCoolingFlow = FinalSysSizing(CurSysNum)%DesMainVolFlow
            ELSE
              SysCoolingFlow = UnitarySystem(UnitarySysNum)%MaxCoolAirVolFlow
            END IF
            UnitarySysEqSizing(CurSysNum)%AirFlow = .TRUE.
            UnitarySysEqSizing(CurSysNum)%AirVolFlow = SysCoolingFlow
        END SELECT
      ELSE IF(UnitarySystem(UnitarySysNum)%HeatCoilExists .AND. .NOT. UnitarySystem(UnitarySysNum)%CoolCoilExists)THEN
        SELECT CASE(UnitarySystem(UnitarySysNum)%HeatingSAFMethod)
          CASE (SupplyAirFlowRate)
            IF(UnitarySystem(UnitarySysNum)%MaxHeatAirVolFlow == Autosize)THEN
              SysHeatingFlow = FinalSysSizing(CurSysNum)%DesMainVolFlow
            ELSE
              SysHeatingFlow = UnitarySystem(UnitarySysNum)%MaxHeatAirVolFlow
            END IF
            UnitarySysEqSizing(CurSysNum)%AirFlow = .TRUE.
            UnitarySysEqSizing(CurSysNum)%AirVolFlow = SysHeatingFlow
          CASE (FlowPerFloorArea)
            SysHeatingFlow = UnitarySystem(UnitarySysNum)%MaxHeatAirVolFlow
            UnitarySystem(UnitarySysNum)%MaxHeatAirVolFlow = Autosize
            UnitarySysEqSizing(CurSysNum)%AirFlow = .TRUE.
            UnitarySysEqSizing(CurSysNum)%AirVolFlow = SysHeatingFlow
          CASE (FractionOfAutosizedCoolingValue)
            SysHeatingFlow = FinalSysSizing(CurSysNum)%DesMainVolFlow * &
                             UnitarySystem(UnitarySysNum)%MaxHeatAirVolFlow
            UnitarySystem(UnitarySysNum)%MaxHeatAirVolFlow = Autosize
            UnitarySysEqSizing(CurSysNum)%AirFlow = .TRUE.
            UnitarySysEqSizing(CurSysNum)%AirVolFlow = SysHeatingFlow
          CASE (FlowPerHeatingCapacity)
            VolFlowRate = FinalSysSizing(CurSysNum)%DesMainVolFlow
            SupTemp = FinalSysSizing(CurSysNum)%HeatSupTemp
            SupHumRat = FinalSysSizing(CurSysNum)%HeatSupHumRat
            IF (PrimaryAirSystem(CurSysNum)%NumOACoolCoils == 0) THEN ! there is no precooling of the OA stream
              MixTemp = FinalSysSizing(CurSysNum)%HeatMixTemp
              MixHumRat = FinalSysSizing(CurSysNum)%HeatMixHumRat
            ELSE ! there is precooling of OA stream
              IF (VolFlowRate > 0.0d0) THEN
                OutAirFrac = FinalSysSizing(CurSysNum)%DesOutAirVolFlow / VolFlowRate
              ELSE
                OutAirFrac = 1.0d0
              END IF
              OutAirFrac = MIN(1.0d0,MAX(0.0d0,OutAirFrac))
              MixTemp = OutAirFrac*FinalSysSizing(CurSysNum)%PrecoolTemp + &
                          (1.0d0-OutAirFrac)*FinalSysSizing(CurSysNum)%HeatRetTemp
              MixHumRat = OutAirFrac*FinalSysSizing(CurSysNum)%PrecoolHumRat + &
                            (1.0d0-OutAirFrac)*FinalSysSizing(CurSysNum)%HeatRetHumRat
            END IF
            OutTemp = FinalSysSizing(CurSysNum)%HeatOutTemp
            rhoair = PsyRhoAirFnPbTdbW(StdBaroPress,MixTemp,MixHumRat,RoutineName)
            MixEnth = PsyHFnTdbW(MixTemp,MixHumRat,RoutineName)
            MixWetBulb = PsyTwbFnTdbWPb(MixTemp,MixHumRat,StdBaroPress,RoutineName)
            SupEnth = PsyHFnTdbW(SupTemp,SupHumRat,RoutineName)
            IF(UnitarySystem(UnitarySysNum)%HeatingCoilType_Num == CoilDX_MultiSpeedHeating .OR. &
               UnitarySystem(UnitarySysNum)%HeatingCoilType_Num == CoilDX_HeatingEmpirical)THEN
                CapFTCurve = GetDXCoilCapFTCurveIndex(UnitarySystem(UnitarySysNum)%HeatingCoilIndex,ErrFound)
              TotCapTempModFac = CurveValue(CapFTCurve,MixWetBulb,OutTemp)
            ELSE
              TotCapTempModFac = 1.0d0
            END IF
            HeatCapAtPeak = MAX(0.d0, (rhoair * VolFlowRate * (SupEnth-MixEnth)))
            IF(TotCapTempModFac .GT. 0.d0)THEN
              HeatCapAtPeak = HeatCapAtPeak / TotCapTempModFac
            END IF
            SysHeatingFlow = HeatCapAtPeak * UnitarySystem(UnitarySysNum)%MaxCoolAirVolFlow
            UnitarySystem(UnitarySysNum)%MaxCoolAirVolFlow = Autosize
            UnitarySysEqSizing(CurSysNum)%AirFlow = .TRUE.
            UnitarySysEqSizing(CurSysNum)%AirVolFlow = SysHeatingFlow
            UnitarySysEqSizing(CurSysNum)%Capacity = .TRUE.
            UnitarySysEqSizing(CurSysNum)%DesHeatingLoad = HeatCapAtPeak
          CASE DEFAULT
            IF(UnitarySystem(UnitarySysNum)%MaxHeatAirVolFlow == Autosize)THEN
              SysHeatingFlow = FinalSysSizing(CurSysNum)%DesMainVolFlow
            ELSE
              SysHeatingFlow = UnitarySystem(UnitarySysNum)%MaxHeatAirVolFlow
            END IF
            UnitarySysEqSizing(CurSysNum)%AirFlow = .TRUE.
            UnitarySysEqSizing(CurSysNum)%AirVolFlow = SysHeatingFlow
        END SELECT
      ELSE ! Cooling and Heating coil are present
        SELECT CASE(UnitarySystem(UnitarySysNum)%CoolingSAFMethod)
          CASE (SupplyAirFlowRate, None)
            IF(UnitarySystem(UnitarySysNum)%MaxCoolAirVolFlow == Autosize)THEN
              SysCoolingFlow = FinalSysSizing(CurSysNum)%DesMainVolFlow
            ELSE
              SysCoolingFlow = UnitarySystem(UnitarySysNum)%MaxCoolAirVolFlow
            END IF
            UnitarySysEqSizing(CurSysNum)%AirFlow = .TRUE.
          CASE (FlowPerFloorArea)
            SysCoolingFlow = MAX(UnitarySystem(UnitarySysNum)%MaxCoolAirVolFlow, &
                                 UnitarySystem(UnitarySysNum)%MaxHeatAirVolFlow)
            UnitarySystem(UnitarySysNum)%MaxCoolAirVolFlow = Autosize
            UnitarySystem(UnitarySysNum)%MaxHeatAirVolFlow = Autosize
            UnitarySysEqSizing(CurSysNum)%AirFlow = .TRUE.
          CASE (FractionOfAutosizedCoolingValue)
            SysCoolingFlow = FinalSysSizing(CurSysNum)%DesMainVolFlow * &
                MAX(UnitarySystem(UnitarySysNum)%MaxCoolAirVolFlow, UnitarySystem(UnitarySysNum)%MaxHeatAirVolFlow)
            UnitarySystem(UnitarySysNum)%MaxCoolAirVolFlow = Autosize
            UnitarySystem(UnitarySysNum)%MaxHeatAirVolFlow = Autosize
            UnitarySysEqSizing(CurSysNum)%AirFlow = .TRUE.
          CASE (FlowPerCoolingCapacity)
            VolFlowRate = FinalSysSizing(CurSysNum)%DesMainVolFlow
            SupTemp = FinalSysSizing(CurSysNum)%CoolSupTemp
            SupHumRat = FinalSysSizing(CurSysNum)%CoolSupHumRat
            IF (PrimaryAirSystem(CurSysNum)%NumOACoolCoils == 0) THEN ! there is no precooling of the OA stream
              MixTemp = FinalSysSizing(CurSysNum)%CoolMixTemp
              MixHumRat = FinalSysSizing(CurSysNum)%CoolMixHumRat
            ELSE ! there is precooling of OA stream
              IF (VolFlowRate > 0.0d0) THEN
                OutAirFrac = FinalSysSizing(CurSysNum)%DesMainVolFlow / VolFlowRate
              ELSE
                OutAirFrac = 1.0d0
              END IF
              OutAirFrac = MIN(1.0d0,MAX(0.0d0,OutAirFrac))
              MixTemp = OutAirFrac*FinalSysSizing(CurSysNum)%PrecoolTemp + &
                          (1.0d0-OutAirFrac)*FinalSysSizing(CurSysNum)%CoolRetTemp
              MixHumRat = OutAirFrac*FinalSysSizing(CurSysNum)%PrecoolHumRat + &
                            (1.0d0-OutAirFrac)*FinalSysSizing(CurSysNum)%CoolRetHumRat
            END IF
            OutTemp = FinalSysSizing(CurSysNum)%CoolOutTemp
            rhoair = PsyRhoAirFnPbTdbW(StdBaroPress,MixTemp,MixHumRat,RoutineName)
            MixEnth = PsyHFnTdbW(MixTemp,MixHumRat,RoutineName)
            MixWetBulb = PsyTwbFnTdbWPb(MixTemp,MixHumRat,StdBaroPress,RoutineName)
            SupEnth = PsyHFnTdbW(SupTemp,SupHumRat,RoutineName)
            IF(UnitarySystem(UnitarySysNum)%CoolingCoilType_Num == CoilDX_CoolingSingleSpeed .OR. &
               UnitarySystem(UnitarySysNum)%CoolingCoilType_Num == CoilDX_MultiSpeedCooling .OR. &
               UnitarySystem(UnitarySysNum)%CoolingCoilType_Num == CoilDX_CoolingTwoSpeed .OR. &
               UnitarySystem(UnitarySysNum)%CoolingCoilType_Num == CoilDX_CoolingTwoStageWHumControl)THEN
              CapFTCurve = GetDXCoilCapFTCurveIndex(UnitarySystem(UnitarySysNum)%CoolingCoilIndex,ErrFound)
              TotCapTempModFac = CurveValue(CapFTCurve,MixWetBulb,OutTemp)
            ELSE
              TotCapTempModFac = 1.0d0
            END IF
            CoolCapAtPeak = MAX(0.d0, (rhoair * VolFlowRate * (MixEnth-SupEnth)))
            IF(TotCapTempModFac .GT. 0.d0)THEN
              CoolCapAtPeak = CoolCapAtPeak / TotCapTempModFac
            END IF
            SysCoolingFlow = CoolCapAtPeak * UnitarySystem(UnitarySysNum)%MaxCoolAirVolFlow
            UnitarySystem(UnitarySysNum)%MaxCoolAirVolFlow = Autosize
            UnitarySysEqSizing(CurSysNum)%AirFlow = .TRUE.
            UnitarySysEqSizing(CurSysNum)%Capacity = .TRUE.
          CASE DEFAULT
            IF(UnitarySystem(UnitarySysNum)%MaxCoolAirVolFlow == Autosize)THEN
              SysCoolingFlow = FinalSysSizing(CurSysNum)%DesMainVolFlow
            ELSE
              SysCoolingFlow = UnitarySystem(UnitarySysNum)%MaxCoolAirVolFlow
            END IF
            UnitarySysEqSizing(CurSysNum)%AirFlow = .TRUE.
        END SELECT
        SELECT CASE(UnitarySystem(UnitarySysNum)%HeatingSAFMethod)
          CASE (SupplyAirFlowRate)
            IF(UnitarySystem(UnitarySysNum)%MaxHeatAirVolFlow == Autosize)THEN
              SysHeatingFlow = FinalSysSizing(CurSysNum)%DesMainVolFlow
            ELSE
              SysHeatingFlow = UnitarySystem(UnitarySysNum)%MaxHeatAirVolFlow
            END IF
            UnitarySysEqSizing(CurSysNum)%AirFlow = .TRUE.
          CASE (FlowPerFloorArea)
            SysHeatingFlow = UnitarySystem(UnitarySysNum)%MaxHeatAirVolFlow
            UnitarySystem(UnitarySysNum)%MaxHeatAirVolFlow = Autosize
            UnitarySysEqSizing(CurSysNum)%AirFlow = .TRUE.
          CASE (FractionOfAutosizedCoolingValue)
            SysHeatingFlow = FinalSysSizing(CurSysNum)%DesMainVolFlow * &
                             UnitarySystem(UnitarySysNum)%MaxHeatAirVolFlow
            UnitarySystem(UnitarySysNum)%MaxHeatAirVolFlow = Autosize
            UnitarySysEqSizing(CurSysNum)%AirFlow = .TRUE.
          CASE (FlowPerHeatingCapacity)
            VolFlowRate = FinalSysSizing(CurSysNum)%DesMainVolFlow
            SupTemp = FinalSysSizing(CurSysNum)%HeatSupTemp
            SupHumRat = FinalSysSizing(CurSysNum)%HeatSupHumRat
            IF (PrimaryAirSystem(CurSysNum)%NumOACoolCoils == 0) THEN ! there is no precooling of the OA stream
              MixTemp = FinalSysSizing(CurSysNum)%HeatMixTemp
              MixHumRat = FinalSysSizing(CurSysNum)%HeatMixHumRat
            ELSE ! there is precooling of OA stream
              IF (VolFlowRate > 0.0d0) THEN
                OutAirFrac = FinalSysSizing(CurSysNum)%DesMainVolFlow / VolFlowRate
              ELSE
                OutAirFrac = 1.0d0
              END IF
              OutAirFrac = MIN(1.0d0,MAX(0.0d0,OutAirFrac))
              MixTemp = OutAirFrac*FinalSysSizing(CurSysNum)%PrecoolTemp + &
                          (1.0d0-OutAirFrac)*FinalSysSizing(CurSysNum)%HeatRetTemp
              MixHumRat = OutAirFrac*FinalSysSizing(CurSysNum)%PrecoolHumRat + &
                            (1.0d0-OutAirFrac)*FinalSysSizing(CurSysNum)%HeatRetHumRat
            END IF
            OutTemp = FinalSysSizing(CurSysNum)%HeatOutTemp
            rhoair = PsyRhoAirFnPbTdbW(StdBaroPress,MixTemp,MixHumRat,RoutineName)
            MixEnth = PsyHFnTdbW(MixTemp,MixHumRat,RoutineName)
            MixWetBulb = PsyTwbFnTdbWPb(MixTemp,MixHumRat,StdBaroPress,RoutineName)
            SupEnth = PsyHFnTdbW(SupTemp,SupHumRat,RoutineName)
            IF(UnitarySystem(UnitarySysNum)%HeatingCoilType_Num == CoilDX_MultiSpeedHeating .OR. &
               UnitarySystem(UnitarySysNum)%HeatingCoilType_Num == CoilDX_HeatingEmpirical)THEN
              CapFTCurve = GetDXCoilCapFTCurveIndex(UnitarySystem(UnitarySysNum)%HeatingCoilIndex,ErrFound)
              TotCapTempModFac = CurveValue(CapFTCurve,MixWetBulb,OutTemp)
            ELSE
              TotCapTempModFac = 1.0d0
            END IF
            HeatCapAtPeak = MAX(0.d0, (rhoair * VolFlowRate * (SupEnth-MixEnth)))
            IF(TotCapTempModFac .GT. 0.d0)THEN
              HeatCapAtPeak = HeatCapAtPeak / TotCapTempModFac
            END IF
            SysHeatingFlow = HeatCapAtPeak * UnitarySystem(UnitarySysNum)%MaxHeatAirVolFlow
            UnitarySystem(UnitarySysNum)%MaxHeatAirVolFlow = Autosize
            UnitarySysEqSizing(CurSysNum)%AirFlow = .TRUE.
            UnitarySysEqSizing(CurSysNum)%Capacity = .TRUE.
          CASE DEFAULT
            IF(UnitarySystem(UnitarySysNum)%MaxHeatAirVolFlow == Autosize)THEN
              SysHeatingFlow = FinalSysSizing(CurSysNum)%DesMainVolFlow
            ELSE
              SysHeatingFlow = UnitarySystem(UnitarySysNum)%MaxHeatAirVolFlow
            END IF
            UnitarySysEqSizing(CurSysNum)%AirFlow = .TRUE.
        END SELECT
        UnitarySysEqSizing(CurSysNum)%AirVolFlow = MAX(SysCoolingFlow,SysHeatingFlow)
        UnitarySysEqSizing(CurSysNum)%DesCoolingLoad = MAX(CoolCapAtPeak,HeatCapAtPeak)
      END IF
    ELSEIF (CurZoneEqNum > 0) THEN
      ! if we keep this (zone equipment) then we should probably check to see IF a cooling/heating coil is present
      ! and use just cooling if only a cooling coil, or just heating if only a heating coil
!      CALL CheckZoneSizing(UnitarySystem(UnitarySysNum)%UnitarySystemType, UnitarySystem(UnitarySysNum)%Name)

       IF(UnitarySystem(UnitarySysNum)%CoolCoilExists .AND. .NOT. UnitarySystem(UnitarySysNum)%HeatCoilExists)THEN
         ZoneCoolingOnlyFan = .TRUE.
         SELECT CASE(UnitarySystem(UnitarySysNum)%CoolingSAFMethod)
           CASE (SupplyAirFlowRate)
            IF(UnitarySystem(UnitarySysNum)%MaxCoolAirVolFlow == Autosize)THEN
              SysCoolingFlow = FinalZoneSizing(CurZoneEqNum)%DesCoolVolFlow
            ELSE
              SysCoolingFlow = UnitarySystem(UnitarySysNum)%MaxCoolAirVolFlow
            END IF
            ZoneEqSizing(CurZoneEqNum)%AirFlow = .TRUE.
            ZoneEqSizing(CurZoneEqNum)%AirVolFlow = SysCoolingFlow
           CASE (FlowPerFloorArea)
             SysCoolingFlow = UnitarySystem(UnitarySysNum)%MaxCoolAirVolFlow
             UnitarySystem(UnitarySysNum)%MaxCoolAirVolFlow = Autosize
             ZoneEqSizing(CurZoneEqNum)%AirFlow = .TRUE.
             ZoneEqSizing(CurZoneEqNum)%AirVolFlow = SysCoolingFlow
           CASE (FractionOfAutosizedCoolingValue)
             SysCoolingFlow = FinalZoneSizing(CurZoneEqNum)%DesCoolVolFlow * &
                              UnitarySystem(UnitarySysNum)%MaxCoolAirVolFlow
             UnitarySystem(UnitarySysNum)%MaxCoolAirVolFlow = Autosize
             ZoneEqSizing(CurZoneEqNum)%AirFlow = .TRUE.
             ZoneEqSizing(CurZoneEqNum)%AirVolFlow = SysCoolingFlow
           CASE (FlowPerCoolingCapacity)
             VolFlowRate = FinalZoneSizing(CurZoneEqNum)%DesCoolVolFlow
             IF(ZoneEqDXCoil)THEN
               IF (ZoneEqSizing(CurZoneEqNum)%OAVolFlow > 0.0d0) THEN
                 MixTemp = FinalZoneSizing(CurZoneEqNum)%DesCoolCoilInTemp
                 MixHumRat = FinalZoneSizing(CurZoneEqNum)%DesCoolCoilInHumRat
               ELSE
                 MixTemp = FinalZoneSizing(CurZoneEqNum)%ZoneRetTempAtCoolPeak
                 MixHumRat = FinalZoneSizing(CurZoneEqNum)%ZoneHumRatAtCoolPeak
               END IF
             ELSE
               MixTemp = FinalZoneSizing(CurZoneEqNum)%DesCoolCoilInTemp
               MixHumRat = FinalZoneSizing(CurZoneEqNum)%DesCoolCoilInHumRat
             END IF
             SupTemp = FinalZoneSizing(CurZoneEqNum)%CoolDesTemp
             SupHumRat = FinalZoneSizing(CurZoneEqNum)%CoolDesHumRat
             TimeStepNumAtMax = FinalZoneSizing(CurZoneEqNum)%TimeStepNumAtCoolMax
             DDNum = FinalZoneSizing(CurZoneEqNum)%CoolDDNum
             IF (DDNum > 0 .and. TimeStepNumAtMax > 0) THEN
               OutTemp = DesDayWeath(DDNum)%Temp(TimeStepNumAtMax)
             ELSE
               OutTemp = 0.0d0
             END IF
             rhoair = PsyRhoAirFnPbTdbW(StdBaroPress,MixTemp,MixHumRat,RoutineName)
             MixEnth = PsyHFnTdbW(MixTemp,MixHumRat,RoutineName)
             MixWetBulb = PsyTwbFnTdbWPb(MixTemp,MixHumRat,StdBaroPress,RoutineName)
             SupEnth = PsyHFnTdbW(SupTemp,SupHumRat,RoutineName)
             IF(UnitarySystem(UnitarySysNum)%CoolingCoilType_Num == CoilDX_CoolingSingleSpeed .OR. &
                UnitarySystem(UnitarySysNum)%CoolingCoilType_Num == CoilDX_MultiSpeedCooling .OR. &
                UnitarySystem(UnitarySysNum)%CoolingCoilType_Num == CoilDX_CoolingTwoSpeed .OR. &
                UnitarySystem(UnitarySysNum)%CoolingCoilType_Num == CoilDX_CoolingTwoStageWHumControl)THEN
               CapFTCurve = GetDXCoilCapFTCurveIndex(UnitarySystem(UnitarySysNum)%CoolingCoilIndex,ErrFound)
               TotCapTempModFac = CurveValue(CapFTCurve,MixWetBulb,OutTemp)
             ELSE
               TotCapTempModFac = 1.0d0
             END IF
             CoolCapAtPeak = MAX(0.d0, (rhoair * VolFlowRate * (MixEnth-SupEnth)))
             IF(TotCapTempModFac .GT. 0.d0)THEN
               CoolCapAtPeak = CoolCapAtPeak / TotCapTempModFac
             END IF
             SysCoolingFlow = CoolCapAtPeak * UnitarySystem(UnitarySysNum)%MaxCoolAirVolFlow
             UnitarySystem(UnitarySysNum)%MaxCoolAirVolFlow = Autosize
             ZoneEqSizing(CurZoneEqNum)%AirFlow = .TRUE.
             ZoneEqSizing(CurZoneEqNum)%AirVolFlow = SysCoolingFlow
             ZoneEqSizing(CurZoneEqNum)%Capacity = .TRUE.
             ZoneEqSizing(CurZoneEqNum)%DesCoolingLoad = CoolCapAtPeak
           CASE DEFAULT
            IF(UnitarySystem(UnitarySysNum)%MaxCoolAirVolFlow == Autosize)THEN
              SysCoolingFlow = FinalZoneSizing(CurZoneEqNum)%DesCoolVolFlow
            ELSE
              SysCoolingFlow = UnitarySystem(UnitarySysNum)%MaxCoolAirVolFlow
            END IF
            ZoneEqSizing(CurZoneEqNum)%AirFlow = .TRUE.
            ZoneEqSizing(CurZoneEqNum)%AirVolFlow = SysCoolingFlow
         END SELECT
       ELSE IF(UnitarySystem(UnitarySysNum)%HeatCoilExists .AND. .NOT. UnitarySystem(UnitarySysNum)%CoolCoilExists)THEN
         ZoneHeatingOnlyFan = .TRUE.
        SELECT CASE(UnitarySystem(UnitarySysNum)%HeatingSAFMethod)
          CASE (SupplyAirFlowRate)
            IF(UnitarySystem(UnitarySysNum)%MaxHeatAirVolFlow == Autosize)THEN
              SysHeatingFlow = FinalZoneSizing(CurZoneEqNum)%DesHeatVolFlow
            ELSE
              SysHeatingFlow = UnitarySystem(UnitarySysNum)%MaxHeatAirVolFlow
            END IF
            ZoneEqSizing(CurZoneEqNum)%AirFlow = .TRUE.
            ZoneEqSizing(CurZoneEqNum)%AirVolFlow = SysHeatingFlow
          CASE (FlowPerFloorArea)
            SysHeatingFlow = UnitarySystem(UnitarySysNum)%MaxHeatAirVolFlow
            UnitarySystem(UnitarySysNum)%MaxHeatAirVolFlow = Autosize
            ZoneEqSizing(CurZoneEqNum)%AirFlow = .TRUE.
            ZoneEqSizing(CurZoneEqNum)%AirVolFlow = SysHeatingFlow
          CASE (FractionOfAutosizedCoolingValue)
            SysHeatingFlow = FinalZoneSizing(CurZoneEqNum)%DesHeatVolFlow * &
                             UnitarySystem(UnitarySysNum)%MaxHeatAirVolFlow
            UnitarySystem(UnitarySysNum)%MaxHeatAirVolFlow = Autosize
            ZoneEqSizing(CurZoneEqNum)%AirFlow = .TRUE.
            ZoneEqSizing(CurZoneEqNum)%AirVolFlow = SysHeatingFlow
          CASE (FlowPerHeatingCapacity)
            VolFlowRate = FinalZoneSizing(CurZoneEqNum)%DesHeatVolFlow
            IF(ZoneEqDXCoil)THEN
              IF (ZoneEqSizing(CurZoneEqNum)%OAVolFlow > 0.0d0) THEN
                MixTemp = FinalZoneSizing(CurZoneEqNum)%DesHeatCoilInTemp
                MixHumRat = FinalZoneSizing(CurZoneEqNum)%DesHeatCoilInHumRat
              ELSE
                MixTemp = FinalZoneSizing(CurZoneEqNum)%ZoneRetTempAtHeatPeak
                MixHumRat = FinalZoneSizing(CurZoneEqNum)%ZoneHumRatAtHeatPeak
              END IF
            ELSE
              MixTemp = FinalZoneSizing(CurZoneEqNum)%DesHeatCoilInTemp
              MixHumRat = FinalZoneSizing(CurZoneEqNum)%DesHeatCoilInHumRat
            END IF
            SupTemp = FinalZoneSizing(CurZoneEqNum)%HeatDesTemp
            SupHumRat = FinalZoneSizing(CurZoneEqNum)%HeatDesHumRat
            TimeStepNumAtMax = FinalZoneSizing(CurZoneEqNum)%TimeStepNumAtHeatMax
            DDNum = FinalZoneSizing(CurZoneEqNum)%HeatDDNum
            IF (DDNum > 0 .and. TimeStepNumAtMax > 0) THEN
              OutTemp = DesDayWeath(DDNum)%Temp(TimeStepNumAtMax)
            ELSE
              OutTemp = 0.0d0
            END IF
            rhoair = PsyRhoAirFnPbTdbW(StdBaroPress,MixTemp,MixHumRat,RoutineName)
            MixEnth = PsyHFnTdbW(MixTemp,MixHumRat,RoutineName)
            MixWetBulb = PsyTwbFnTdbWPb(MixTemp,MixHumRat,StdBaroPress,RoutineName)
            SupEnth = PsyHFnTdbW(SupTemp,SupHumRat,RoutineName)
            IF(UnitarySystem(UnitarySysNum)%HeatingCoilType_Num == CoilDX_MultiSpeedHeating .OR. &
               UnitarySystem(UnitarySysNum)%HeatingCoilType_Num == CoilDX_HeatingEmpirical)THEN
              CapFTCurve = GetDXCoilCapFTCurveIndex(UnitarySystem(UnitarySysNum)%HeatingCoilIndex,ErrFound)
              TotCapTempModFac = CurveValue(CapFTCurve,MixWetBulb,OutTemp)
            ELSE
              TotCapTempModFac = 1.0d0
            END IF
            HeatCapAtPeak = MAX(0.d0, (rhoair * VolFlowRate * (SupEnth-MixEnth)))
            IF(TotCapTempModFac .GT. 0.d0)THEN
              HeatCapAtPeak = HeatCapAtPeak / TotCapTempModFac
            END IF
            SysHeatingFlow = HeatCapAtPeak * UnitarySystem(UnitarySysNum)%MaxHeatAirVolFlow
            UnitarySystem(UnitarySysNum)%MaxHeatAirVolFlow = Autosize
            ZoneEqSizing(CurZoneEqNum)%AirFlow = .TRUE.
            ZoneEqSizing(CurZoneEqNum)%AirVolFlow = SysHeatingFlow
            ZoneEqSizing(CurZoneEqNum)%Capacity = .TRUE.
            ZoneEqSizing(CurZoneEqNum)%DesHeatingLoad = HeatCapAtPeak
          CASE DEFAULT
            IF(UnitarySystem(UnitarySysNum)%MaxHeatAirVolFlow == Autosize)THEN
              SysHeatingFlow = FinalZoneSizing(CurZoneEqNum)%DesHeatVolFlow
            ELSE
              SysHeatingFlow = UnitarySystem(UnitarySysNum)%MaxHeatAirVolFlow
            END IF
            ZoneEqSizing(CurZoneEqNum)%AirFlow = .TRUE.
            ZoneEqSizing(CurZoneEqNum)%AirVolFlow = SysHeatingFlow
        END SELECT
      ELSE
        SELECT CASE(UnitarySystem(UnitarySysNum)%CoolingSAFMethod)
          CASE (SupplyAirFlowRate, None)
            IF(UnitarySystem(UnitarySysNum)%MaxCoolAirVolFlow == Autosize)THEN
              SysCoolingFlow = FinalZoneSizing(CurZoneEqNum)%DesCoolVolFlow
            ELSE
              SysCoolingFlow = UnitarySystem(UnitarySysNum)%MaxCoolAirVolFlow
            END IF
            ZoneEqSizing(CurZoneEqNum)%AirFlow = .TRUE.
          CASE (FlowPerFloorArea)
            SysCoolingFlow = UnitarySystem(UnitarySysNum)%MaxCoolAirVolFlow
            UnitarySystem(UnitarySysNum)%MaxCoolAirVolFlow = Autosize ! allow reporting for sizing
            ZoneEqSizing(CurZoneEqNum)%AirFlow = .TRUE.
          CASE (FractionOfAutosizedCoolingValue)
            SysCoolingFlow = UnitarySystem(UnitarySysNum)%MaxCoolAirVolFlow * FinalZoneSizing(CurZoneEqNum)%DesCoolVolFlow
            UnitarySystem(UnitarySysNum)%MaxCoolAirVolFlow = Autosize
            ZoneEqSizing(CurZoneEqNum)%AirFlow = .TRUE.
          CASE (FlowPerCoolingCapacity)
            VolFlowRate = FinalZoneSizing(CurZoneEqNum)%DesCoolVolFlow
            IF(ZoneEqDXCoil)THEN
              IF (ZoneEqSizing(CurZoneEqNum)%OAVolFlow > 0.0d0) THEN
                MixTemp = FinalZoneSizing(CurZoneEqNum)%DesCoolCoilInTemp
                MixHumRat = FinalZoneSizing(CurZoneEqNum)%DesCoolCoilInHumRat
              ELSE
                MixTemp = FinalZoneSizing(CurZoneEqNum)%ZoneRetTempAtCoolPeak
                MixHumRat = FinalZoneSizing(CurZoneEqNum)%ZoneHumRatAtCoolPeak
              END IF
            ELSE
              MixTemp = FinalZoneSizing(CurZoneEqNum)%DesCoolCoilInTemp
              MixHumRat = FinalZoneSizing(CurZoneEqNum)%DesCoolCoilInHumRat
            END IF
            SupTemp = FinalZoneSizing(CurZoneEqNum)%CoolDesTemp
            SupHumRat = FinalZoneSizing(CurZoneEqNum)%CoolDesHumRat
            TimeStepNumAtMax = FinalZoneSizing(CurZoneEqNum)%TimeStepNumAtCoolMax
            DDNum = FinalZoneSizing(CurZoneEqNum)%CoolDDNum
            IF (DDNum > 0 .and. TimeStepNumAtMax > 0) THEN
              OutTemp = DesDayWeath(DDNum)%Temp(TimeStepNumAtMax)
            ELSE
              OutTemp = 0.0d0
            END IF
            rhoair = PsyRhoAirFnPbTdbW(StdBaroPress,MixTemp,MixHumRat,RoutineName)
            MixEnth = PsyHFnTdbW(MixTemp,MixHumRat,RoutineName)
            MixWetBulb = PsyTwbFnTdbWPb(MixTemp,MixHumRat,StdBaroPress,RoutineName)
            SupEnth = PsyHFnTdbW(SupTemp,SupHumRat,RoutineName)
            IF(UnitarySystem(UnitarySysNum)%CoolingCoilType_Num == CoilDX_CoolingSingleSpeed .OR. &
               UnitarySystem(UnitarySysNum)%CoolingCoilType_Num == CoilDX_MultiSpeedCooling .OR. &
               UnitarySystem(UnitarySysNum)%CoolingCoilType_Num == CoilDX_CoolingTwoSpeed .OR. &
               UnitarySystem(UnitarySysNum)%CoolingCoilType_Num == CoilDX_CoolingTwoStageWHumControl)THEN
              CapFTCurve = GetDXCoilCapFTCurveIndex(UnitarySystem(UnitarySysNum)%CoolingCoilIndex,ErrFound)
              TotCapTempModFac = CurveValue(CapFTCurve,MixWetBulb,OutTemp)
            ELSE
              TotCapTempModFac = 1.0d0
            END IF
            CoolCapAtPeak = MAX(0.d0, (rhoair * VolFlowRate * (MixEnth-SupEnth)))
            IF(TotCapTempModFac .GT. 0.d0)THEN
              CoolCapAtPeak = CoolCapAtPeak / TotCapTempModFac
            END IF
            SysCoolingFlow = CoolCapAtPeak * UnitarySystem(UnitarySysNum)%MaxCoolAirVolFlow
            UnitarySystem(UnitarySysNum)%MaxCoolAirVolFlow = Autosize
            ZoneEqSizing(CurZoneEqNum)%AirFlow = .TRUE.
            ZoneEqSizing(CurZoneEqNum)%Capacity = .TRUE.
          CASE DEFAULT
            IF(UnitarySystem(UnitarySysNum)%MaxCoolAirVolFlow == Autosize)THEN
              SysCoolingFlow = FinalZoneSizing(CurZoneEqNum)%DesCoolVolFlow
            ELSE
              SysCoolingFlow = UnitarySystem(UnitarySysNum)%MaxCoolAirVolFlow
            END IF
            ZoneEqSizing(CurZoneEqNum)%AirFlow = .TRUE.
        END SELECT
        SELECT CASE(UnitarySystem(UnitarySysNum)%HeatingSAFMethod)
          CASE (SupplyAirFlowRate)
            IF(UnitarySystem(UnitarySysNum)%MaxHeatAirVolFlow == Autosize)THEN
              SysHeatingFlow = FinalZoneSizing(CurZoneEqNum)%DesHeatVolFlow
            ELSE
              SysHeatingFlow = UnitarySystem(UnitarySysNum)%MaxHeatAirVolFlow
            END IF
            ZoneEqSizing(CurZoneEqNum)%AirFlow = .TRUE.
          CASE (FlowPerFloorArea)
            SysHeatingFlow = UnitarySystem(UnitarySysNum)%MaxHeatAirVolFlow
            UnitarySystem(UnitarySysNum)%MaxHeatAirVolFlow = Autosize
            ZoneEqSizing(CurZoneEqNum)%AirFlow = .TRUE.
          CASE (FractionOfAutosizedCoolingValue)
            SysHeatingFlow = FinalZoneSizing(CurZoneEqNum)%DesHeatVolFlow * &
                             UnitarySystem(UnitarySysNum)%MaxHeatAirVolFlow
            UnitarySystem(UnitarySysNum)%MaxHeatAirVolFlow = Autosize
            ZoneEqSizing(CurZoneEqNum)%AirFlow = .TRUE.
          CASE (FlowPerHeatingCapacity)
            VolFlowRate = FinalZoneSizing(CurZoneEqNum)%DesHeatVolFlow
            IF(ZoneEqDXCoil)THEN
              IF (ZoneEqSizing(CurZoneEqNum)%OAVolFlow > 0.0d0) THEN
                MixTemp = FinalZoneSizing(CurZoneEqNum)%DesHeatCoilInTemp
                MixHumRat = FinalZoneSizing(CurZoneEqNum)%DesHeatCoilInHumRat
              ELSE
                MixTemp = FinalZoneSizing(CurZoneEqNum)%ZoneRetTempAtHeatPeak
                MixHumRat = FinalZoneSizing(CurZoneEqNum)%ZoneHumRatAtHeatPeak
              END IF
            ELSE
              MixTemp = FinalZoneSizing(CurZoneEqNum)%DesHeatCoilInTemp
              MixHumRat = FinalZoneSizing(CurZoneEqNum)%DesHeatCoilInHumRat
            END IF
            SupTemp = FinalZoneSizing(CurZoneEqNum)%HeatDesTemp
            SupHumRat = FinalZoneSizing(CurZoneEqNum)%HeatDesHumRat
            TimeStepNumAtMax = FinalZoneSizing(CurZoneEqNum)%TimeStepNumAtHeatMax
            DDNum = FinalZoneSizing(CurZoneEqNum)%HeatDDNum
            IF (DDNum > 0 .and. TimeStepNumAtMax > 0) THEN
              OutTemp = DesDayWeath(DDNum)%Temp(TimeStepNumAtMax)
            ELSE
              OutTemp = 0.0d0
            END IF
            rhoair = PsyRhoAirFnPbTdbW(StdBaroPress,MixTemp,MixHumRat,RoutineName)
            MixEnth = PsyHFnTdbW(MixTemp,MixHumRat,RoutineName)
            MixWetBulb = PsyTwbFnTdbWPb(MixTemp,MixHumRat,StdBaroPress,RoutineName)
            SupEnth = PsyHFnTdbW(SupTemp,SupHumRat,RoutineName)
            IF(UnitarySystem(UnitarySysNum)%HeatingCoilType_Num == CoilDX_MultiSpeedHeating .OR. &
               UnitarySystem(UnitarySysNum)%HeatingCoilType_Num == CoilDX_HeatingEmpirical)THEN
              CapFTCurve = GetDXCoilCapFTCurveIndex(UnitarySystem(UnitarySysNum)%HeatingCoilIndex,ErrFound)
              TotCapTempModFac = CurveValue(CapFTCurve,MixWetBulb,OutTemp)
            ELSE
              TotCapTempModFac = 1.0d0
            END IF
            HeatCapAtPeak = MAX(0.d0, (rhoair * VolFlowRate * (SupEnth-MixEnth)))
            IF(TotCapTempModFac .GT. 0.d0)THEN
              HeatCapAtPeak = HeatCapAtPeak / TotCapTempModFac
            END IF
            SysHeatingFlow = HeatCapAtPeak * UnitarySystem(UnitarySysNum)%MaxHeatAirVolFlow
            UnitarySystem(UnitarySysNum)%MaxHeatAirVolFlow = Autosize
            ZoneEqSizing(CurZoneEqNum)%AirFlow = .TRUE.
            ZoneEqSizing(CurZoneEqNum)%Capacity = .TRUE.
          CASE DEFAULT
            IF(UnitarySystem(UnitarySysNum)%MaxHeatAirVolFlow == Autosize)THEN
              SysHeatingFlow = FinalZoneSizing(CurZoneEqNum)%DesHeatVolFlow
            ELSE
              SysHeatingFlow = UnitarySystem(UnitarySysNum)%MaxHeatAirVolFlow
            END IF
            ZoneEqSizing(CurZoneEqNum)%AirFlow = .TRUE.
        END SELECT
        ZoneEqSizing(CurZoneEqNum)%AirVolFlow = MAX(SysCoolingFlow,SysHeatingFlow)
        ZoneEqSizing(CurZoneEqNum)%DesCoolingLoad = MAX(CoolCapAtPeak,HeatCapAtPeak)
      END IF
    END IF
  END IF
  END IF ! IF(UnitarySystem(UnitarySysNum)%RequestAutosize)THEN

! Not sure yet how to enforce heat pump cooling/heating air flow and capacity limits.
! Other checks are in place (e.g., DXCoolCap) to already account for heat pump DX coils but apply to all coil types.
! This method would actually size the HP to the larger of cooling or heating but not yet ready to implement.
!  IF(UnitarySystem(UnitarySysNum)%HeatPump .AND. UnitarySystem(UnitarySysNum)%RequestAutosize)THEN
!    IF(UnitarySystem(UnitarySysNum)%MaxCoolAirVolFlow == Autosize .AND. &
!       UnitarySystem(UnitarySysNum)%MaxHeatAirVolFlow == Autosize)THEN
!      IF(CurOASysNum > 0)THEN
!        IF(OASysEqSizing(CurOASysNum)%AirFlow .AND. .NOT. OASysEqSizing(CurOASysNum)%Capacity)THEN
!          SysCoolingFlow = MAX(SysCoolingFlow,SysHeatingFlow)
!          SysHeatingFlow = SysCoolingFlow
!          OASysEqSizing(CurOASysNum)%AirVolFlow = SysCoolingFlow
!        ELSE IF(OASysEqSizing(CurOASysNum)%Capacity)THEN
!          SysCoolingFlow = MAX(SysCoolingFlow,SysHeatingFlow)
!          SysHeatingFlow = SysCoolingFlow
!          OASysEqSizing(CurOASysNum)%AirVolFlow = SysCoolingFlow
!          CoolCapAtPeak = MAX(CoolCapAtPeak,HeatCapAtPeak)
!          HeatCapAtPeak = CoolCapAtPeak
!          OASysEqSizing(CurOASysNum)%DesCoolingLoad = CoolCapAtPeak
!          OASysEqSizing(CurOASysNum)%DesHeatingLoad = CoolCapAtPeak * UnitarySystem(UnitarySysNum)%HeatingSizingRatio
!        END IF
!      ELSE IF(CurSysNum > 0)THEN
!        IF(UnitarySysEqSizing(CurSysNum)%AirFlow .AND. .NOT. UnitarySysEqSizing(CurSysNum)%Capacity)THEN
!          SysCoolingFlow = MAX(SysCoolingFlow,SysHeatingFlow)
!          SysHeatingFlow = SysCoolingFlow
!          UnitarySysEqSizing(CurSysNum)%AirVolFlow = SysCoolingFlow
!        ELSE IF(UnitarySysEqSizing(CurSysNum)%Capacity)THEN
!          SysCoolingFlow = MAX(SysCoolingFlow,SysHeatingFlow)
!          SysHeatingFlow = SysCoolingFlow
!          UnitarySysEqSizing(CurSysNum)%AirVolFlow = SysCoolingFlow
!          CoolCapAtPeak = MAX(CoolCapAtPeak,HeatCapAtPeak)
!          HeatCapAtPeak = CoolCapAtPeak
!          UnitarySysEqSizing(CurSysNum)%DesCoolingLoad = CoolCapAtPeak
!          UnitarySysEqSizing(CurSysNum)%DesHeatingLoad = CoolCapAtPeak * UnitarySystem(UnitarySysNum)%HeatingSizingRatio
!        END IF
!      ELSE IF(CurZoneEqNum > 0)THEN
!        IF(ZoneEqSizing(CurZoneEqNum)%AirFlow .AND. .NOT. ZoneEqSizing(CurZoneEqNum)%Capacity)THEN
!          SysCoolingFlow = MAX(SysCoolingFlow,SysHeatingFlow)
!          SysHeatingFlow = SysCoolingFlow
!          ZoneEqSizing(CurZoneEqNum)%AirVolFlow = SysCoolingFlow
!        ELSE IF(ZoneEqSizing(CurZoneEqNum)%Capacity)THEN
!          SysCoolingFlow = MAX(SysCoolingFlow,SysHeatingFlow)
!          SysHeatingFlow = SysCoolingFlow
!          ZoneEqSizing(CurZoneEqNum)%AirVolFlow = SysCoolingFlow
!          CoolCapAtPeak = MAX(CoolCapAtPeak,HeatCapAtPeak)
!          HeatCapAtPeak = CoolCapAtPeak
!          ZoneEqSizing(CurZoneEqNum)%DesCoolingLoad = CoolCapAtPeak
!          ZoneEqSizing(CurZoneEqNum)%DesHeatingLoad = CoolCapAtPeak * UnitarySystem(UnitarySysNum)%HeatingSizingRatio
!        END IF
!      END IF
!    END IF
!  END IF

  IF (UnitarySystem(UnitarySysNum)%DesignFanVolFlowRate == AutoSize .AND. &
      UnitarySystem(UnitarySysNum)%FanExists) THEN

    UnitarySystem(UnitarySysNum)%DesignFanVolFlowRate = MAX(SysCoolingFlow,SysHeatingFlow)

    IF (UnitarySystem(UnitarySysNum)%DesignFanVolFlowRateEMSOverrideOn) THEN
      UnitarySystem(UnitarySysNum)%DesignFanVolFlowRate = UnitarySystem(UnitarySysNum)%DesignFanVolFlowRateEMSOverrideValue
    END IF

    IF(UnitarySystem(UnitarySysNum)%DesignFanVolFlowRate < SmallAirVolFlow) THEN
      UnitarySystem(UnitarySysNum)%DesignFanVolFlowRate = 0.0d0
    END IF

    CALL ReportSizingOutput(UnitarySystem(UnitarySysNum)%UnitarySystemType, UnitarySystem(UnitarySysNum)%Name, &
                            'Supply Air Flow Rate [m3/s]', &
                             UnitarySystem(UnitarySysNum)%DesignFanVolFlowRate)

  END IF

  ! not sure what to do if UnitarySystem has only 1 coil type and flow needs to occur when present coil is off
  ! how does constant fan operating mode pertain here?
  IF(UnitarySystem(UnitarySysNum)%HeatCoilExists .AND. .NOT. UnitarySystem(UnitarySysNum)%CoolCoilExists)THEN
    UnitarySystem(UnitarySysNum)%MaxCoolAirVolFlow = SysHeatingFlow
  ELSE IF(UnitarySystem(UnitarySysNum)%CoolCoilExists .AND. .NOT. UnitarySystem(UnitarySysNum)%HeatCoilExists)THEN
    UnitarySystem(UnitarySysNum)%MaxHeatAirVolFlow = SysCoolingFlow
  END IF

    IF (UnitarySystem(UnitarySysNum)%MaxHeatAirVolFlow == AutoSize .AND. &
      UnitarySystem(UnitarySysNum)%HeatCoilExists) THEN

    IF (CurOASysNum > 0) THEN
      IF(OASysEqSizing(CurOASysNum)%AirFlow)THEN
        UnitarySystem(UnitarySysNum)%MaxHeatAirVolFlow = OASysEqSizing(CurOASysNum)%AirVolFlow
      ELSE
        UnitarySystem(UnitarySysNum)%MaxHeatAirVolFlow = FinalSysSizing(CurSysNum)%DesOutAirVolFlow
      END IF
      IF (UnitarySystem(UnitarySysNum)%MaxHeatAirVolFlow < SmallAirVolFlow) THEN
        UnitarySystem(UnitarySysNum)%MaxHeatAirVolFlow = 0.0d0
      END IF
    ELSE
      IF (CurSysNum > 0) THEN

        IF(UnitarySysEqSizing(CurSysNum)%AirFlow)THEN
          UnitarySystem(UnitarySysNum)%MaxHeatAirVolFlow = UnitarySysEqSizing(CurSysNum)%AirVolFlow
        ELSE
          UnitarySystem(UnitarySysNum)%MaxHeatAirVolFlow = FinalSysSizing(CurSysNum)%DesMainVolFlow
        END IF
        IF (FinalSysSizing(CurSysNum)%DesMainVolFlow < SmallAirVolFlow) THEN
          UnitarySystem(UnitarySysNum)%MaxHeatAirVolFlow = 0.0d0
        END IF

        IF (UnitarySystem(UnitarySysNum)%MaxHeatAirVolFlowEMSOverrideOn) THEN
          UnitarySystem(UnitarySysNum)%MaxHeatAirVolFlow = UnitarySystem(UnitarySysNum)%MaxHeatAirVolFlowEMSOverrideValue
        END IF

      ELSEIF (CurZoneEqNum > 0) THEN
        ! if we keep this (zone equipment) then we should probably check to see if a cooling/heating coil is present
        ! and use just cooling if only a cooling coil, or just heating if only a heating coil
        IF(ZoneEqSizing(CurZoneEqNum)%AirFlow)THEN
          UnitarySystem(UnitarySysNum)%MaxHeatAirVolFlow = ZoneEqSizing(CurZoneEqNum)%AirVolFlow
        ELSE
          UnitarySystem(UnitarySysNum)%MaxHeatAirVolFlow =   &
                 MAX(FinalZoneSizing(CurZoneEqNum)%DesCoolVolFlow,FinalZoneSizing(CurZoneEqNum)%DesHeatVolFlow)
        END IF
        IF (UnitarySystem(UnitarySysNum)%MaxHeatAirVolFlow < SmallAirVolFlow) THEN
          UnitarySystem(UnitarySysNum)%MaxHeatAirVolFlow = 0.0d0
        END IF

        IF (UnitarySystem(UnitarySysNum)%MaxHeatAirVolFlowEMSOverrideOn) THEN
          UnitarySystem(UnitarySysNum)%MaxHeatAirVolFlow = UnitarySystem(UnitarySysNum)%MaxHeatAirVolFlowEMSOverrideValue
        END IF

      END IF
    END IF

    CALL ReportSizingOutput(UnitarySystem(UnitarySysNum)%UnitarySystemType, UnitarySystem(UnitarySysNum)%Name, &
                            'Supply Air Flow Rate During Heating Operation [m3/s]', &
                            UnitarySystem(UnitarySysNum)%MaxHeatAirVolFlow)
  END IF

  IF (UnitarySystem(UnitarySysNum)%MaxCoolAirVolFlow == AutoSize .AND. &
      UnitarySystem(UnitarySysNum)%CoolCoilExists) THEN

    IF (CurOASysNum > 0) THEN
      IF(OASysEqSizing(CurOASysNum)%AirFlow)THEN
        UnitarySystem(UnitarySysNum)%MaxCoolAirVolFlow = OASysEqSizing(CurOASysNum)%AirVolFlow
      ELSE
        UnitarySystem(UnitarySysNum)%MaxCoolAirVolFlow = FinalSysSizing(CurSysNum)%DesOutAirVolFlow
      END IF
    ELSE
      IF (CurSysNum > 0) THEN

        IF(UnitarySysEqSizing(CurSysNum)%AirFlow)THEN
          UnitarySystem(UnitarySysNum)%MaxCoolAirVolFlow = UnitarySysEqSizing(CurSysNum)%AirVolFlow
        ELSE
          UnitarySystem(UnitarySysNum)%MaxCoolAirVolFlow = FinalSysSizing(CurSysNum)%DesMainVolFlow
        END IF
        IF (UnitarySystem(UnitarySysNum)%MaxCoolAirVolFlow < SmallAirVolFlow) THEN
          UnitarySystem(UnitarySysNum)%MaxCoolAirVolFlow = 0.0d0
        END IF

        IF (UnitarySystem(UnitarySysNum)%MaxCoolAirVolFlowEMSOverrideOn) THEN
          UnitarySystem(UnitarySysNum)%MaxCoolAirVolFlow = UnitarySystem(UnitarySysNum)%MaxCoolAirVolFlowEMSOverrideValue
        END IF

      ELSEIF (CurZoneEqNum > 0) THEN
        ! if we keep this (zone equipment) then we should probably check to see if a cooling/heating coil is present
        ! and use just cooling if only a cooling coil, or just heating if only a heating coil
        IF(ZoneEqSizing(CurZoneEqNum)%AirFlow)THEN
          UnitarySystem(UnitarySysNum)%MaxCoolAirVolFlow = ZoneEqSizing(CurZoneEqNum)%AirVolFlow
        ELSE
          UnitarySystem(UnitarySysNum)%MaxCoolAirVolFlow =   &
                 MAX(FinalZoneSizing(CurZoneEqNum)%DesCoolVolFlow,FinalZoneSizing(CurZoneEqNum)%DesHeatVolFlow)
        END IF
        IF (UnitarySystem(UnitarySysNum)%MaxCoolAirVolFlow < SmallAirVolFlow) THEN
          UnitarySystem(UnitarySysNum)%MaxCoolAirVolFlow = 0.0d0
        END IF

        IF (UnitarySystem(UnitarySysNum)%MaxCoolAirVolFlowEMSOverrideOn) THEN
          UnitarySystem(UnitarySysNum)%MaxCoolAirVolFlow = UnitarySystem(UnitarySysNum)%MaxCoolAirVolFlowEMSOverrideValue
        END IF

      END IF
    END IF

    CALL ReportSizingOutput(UnitarySystem(UnitarySysNum)%UnitarySystemType, UnitarySystem(UnitarySysNum)%Name, &
                            'Supply Air Flow Rate During Cooling Operation [m3/s]', &
                             UnitarySystem(UnitarySysNum)%MaxCoolAirVolFlow)

  END IF

  IF (UnitarySystem(UnitarySysNum)%MaxNoCoolHeatAirVolFlow == AutoSize .AND. &
      (UnitarySystem(UnitarySysNum)%CoolCoilExists .OR. &
       (UnitarySystem(UnitarySysNum)%HeatCoilExists .OR. UnitarySystem(UnitarySysNum)%SuppCoilExists))) THEN

    IF (CurOASysNum > 0) THEN
      IF(OASysEqSizing(CurOASysNum)%AirFlow)THEN
        UnitarySystem(UnitarySysNum)%MaxNoCoolHeatAirVolFlow = OASysEqSizing(CurOASysNum)%AirVolFlow
      ELSE
        UnitarySystem(UnitarySysNum)%MaxNoCoolHeatAirVolFlow = FinalSysSizing(CurSysNum)%DesOutAirVolFlow
      END IF
    ELSE
      IF (CurSysNum > 0) THEN

        IF(UnitarySysEqSizing(CurSysNum)%AirFlow)THEN
          UnitarySystem(UnitarySysNum)%MaxNoCoolHeatAirVolFlow = UnitarySysEqSizing(CurSysNum)%AirVolFlow
        ELSE
          UnitarySystem(UnitarySysNum)%MaxNoCoolHeatAirVolFlow = FinalSysSizing(CurSysNum)%DesMainVolFlow
        END IF
        IF (UnitarySystem(UnitarySysNum)%MaxNoCoolHeatAirVolFlow < SmallAirVolFlow) THEN
          UnitarySystem(UnitarySysNum)%MaxNoCoolHeatAirVolFlow = 0.0d0
        END IF

        IF (UnitarySystem(UnitarySysNum)%MaxNoCoolHeatAirVolFlowEMSOverrideOn) THEN
          UnitarySystem(UnitarySysNum)%MaxNoCoolHeatAirVolFlow=UnitarySystem(UnitarySysNum)%MaxNoCoolHeatAirVolFlowEMSOverrideValue
        END IF

        CALL ReportSizingOutput(UnitarySystem(UnitarySysNum)%UnitarySystemType, UnitarySystem(UnitarySysNum)%Name, &
                               'Supply Air Flow Rate When No Cooling or Heating is Needed [m3/s]', &
                                UnitarySystem(UnitarySysNum)%MaxNoCoolHeatAirVolFlow)

      ELSEIF (CurZoneEqNum > 0) THEN
        ! if we keep this (zone equipment) then we should probably check to see if a cooling/heating coil is present
        ! and use just cooling if only a cooling coil, or just heating if only a heating coil
!        CALL CheckZoneSizing(UnitarySystem(UnitarySysNum)%UnitarySystemType, UnitarySystem(UnitarySysNum)%Name)
!  would use this to size cooling or heating only systems except for the case where dual UnitarySystems are on the branch
!  and each has only 1 cooling or heating coil ??? How to handle?
!  Use same logic for other flow rates in this sizing module.
        IF(UnitarySystem(UnitarySysNum)%CoolCoilExists .AND. UnitarySystem(UnitarySysNum)%HeatCoilExists)THEN
          UnitarySystem(UnitarySysNum)%MaxNoCoolHeatAirVolFlow =   &
                 MAX(FinalZoneSizing(CurZoneEqNum)%DesCoolVolFlow,FinalZoneSizing(CurZoneEqNum)%DesHeatVolFlow)
        ELSE IF(UnitarySystem(UnitarySysNum)%CoolCoilExists)THEN
          UnitarySystem(UnitarySysNum)%MaxNoCoolHeatAirVolFlow =   &
                 FinalZoneSizing(CurZoneEqNum)%DesCoolVolFlow
        ELSE IF(UnitarySystem(UnitarySysNum)%HeatCoilExists .OR. UnitarySystem(UnitarySysNum)%SuppCoilExists)THEN
          UnitarySystem(UnitarySysNum)%MaxNoCoolHeatAirVolFlow =   &
                 FinalZoneSizing(CurZoneEqNum)%DesHeatVolFlow
        ELSE
          UnitarySystem(UnitarySysNum)%MaxNoCoolHeatAirVolFlow = 0.0d0 ! ?
        END IF

        IF (UnitarySystem(UnitarySysNum)%MaxNoCoolHeatAirVolFlow < SmallAirVolFlow) THEN
          UnitarySystem(UnitarySysNum)%MaxNoCoolHeatAirVolFlow = 0.0d0
        END IF

        IF (UnitarySystem(UnitarySysNum)%MaxNoCoolHeatAirVolFlowEMSOverrideOn) THEN
          UnitarySystem(UnitarySysNum)%MaxNoCoolHeatAirVolFlow=UnitarySystem(UnitarySysNum)%MaxNoCoolHeatAirVolFlowEMSOverrideValue
        END IF

        CALL ReportSizingOutput(UnitarySystem(UnitarySysNum)%UnitarySystemType, UnitarySystem(UnitarySysNum)%Name, &
                               'Supply Air Flow Rate When No Cooling or Heating is Needed [m3/s]', &
                                UnitarySystem(UnitarySysNum)%MaxNoCoolHeatAirVolFlow)

      END IF
    END IF
  ELSE IF (UnitarySystem(UnitarySysNum)%CoolCoilExists .OR. UnitarySystem(UnitarySysNum)%HeatCoilExists) THEN
    SELECT CASE(UnitarySystem(UnitarySysNum)%NoCoolHeatSAFMethod)
      CASE (SupplyAirFlowRate)
        IF(UnitarySystem(UnitarySysNum)%MaxNoCoolHeatAirVolFlow == Autosize)THEN
          IF (CurOASysNum > 0) THEN
            UnitarySystem(UnitarySysNum)%MaxNoCoolHeatAirVolFlow = FinalSysSizing(CurSysNum)%DesOutAirVolFlow
          ELSE IF (CurSysNum > 0) THEN
            UnitarySystem(UnitarySysNum)%MaxNoCoolHeatAirVolFlow = FinalSysSizing(CurSysNum)%DesMainVolFlow
          ELSE IF (CurZoneEqNum > 0) THEN
            IF(UnitarySystem(UnitarySysNum)%CoolCoilExists .AND. UnitarySystem(UnitarySysNum)%HeatCoilExists)THEN
              UnitarySystem(UnitarySysNum)%MaxNoCoolHeatAirVolFlow = &
                MAX(FinalZoneSizing(CurZoneEqNum)%DesCoolVolFlow, FinalZoneSizing(CurZoneEqNum)%DesHeatVolFlow)
            ELSE IF(UnitarySystem(UnitarySysNum)%CoolCoilExists)THEN
              UnitarySystem(UnitarySysNum)%MaxNoCoolHeatAirVolFlow = FinalZoneSizing(CurZoneEqNum)%DesCoolVolFlow
            ELSE IF(UnitarySystem(UnitarySysNum)%HeatCoilExists)THEN
              UnitarySystem(UnitarySysNum)%MaxNoCoolHeatAirVolFlow = FinalZoneSizing(CurZoneEqNum)%DesHeatVolFlow
            ELSE
              ! just guessing here what this should be
              UnitarySystem(UnitarySysNum)%MaxNoCoolHeatAirVolFlow = 0.0d0
            END IF
          END IF
        END IF
      CASE (FlowPerFloorArea)
        ! already calculated in GetInput
      CASE (FractionOfAutosizedCoolingValue, FractionOfAutosizedHeatingValue)
        IF (CurOASysNum > 0) THEN
          UnitarySystem(UnitarySysNum)%MaxNoCoolHeatAirVolFlow = &
          UnitarySystem(UnitarySysNum)%MaxNoCoolHeatAirVolFlow * FinalSysSizing(CurSysNum)%DesOutAirVolFlow
        ELSE IF (CurSysNum > 0) THEN
          UnitarySystem(UnitarySysNum)%MaxNoCoolHeatAirVolFlow = &
          UnitarySystem(UnitarySysNum)%MaxNoCoolHeatAirVolFlow * FinalSysSizing(CurSysNum)%DesMainVolFlow
        ELSE IF (CurZoneEqNum > 0) THEN
          IF(UnitarySystem(UnitarySysNum)%NoCoolHeatSAFMethod == FractionOfAutosizedCoolingValue)THEN
            UnitarySystem(UnitarySysNum)%MaxNoCoolHeatAirVolFlow = &
              UnitarySystem(UnitarySysNum)%MaxNoCoolHeatAirVolFlow * FinalZoneSizing(CurZoneEqNum)%DesCoolVolFlow
          ELSE
            UnitarySystem(UnitarySysNum)%MaxNoCoolHeatAirVolFlow = &
              UnitarySystem(UnitarySysNum)%MaxNoCoolHeatAirVolFlow * FinalZoneSizing(CurZoneEqNum)%DesHeatVolFlow
          END IF
        END IF
      CASE (FlowPerCoolingCapacity) ! these aren't going to work if the user doesn't choose the same method for all cases
        IF(DXCoolCap .EQ. 0.0d0)THEN
          UnitarySystem(UnitarySysNum)%MaxNoCoolHeatAirVolFlow = &
            UnitarySystem(UnitarySysNum)%MaxNoCoolHeatAirVolFlow * &
              ZoneEqSizing(CurZoneEqNum)%DesCoolingLoad
        ELSE
          UnitarySystem(UnitarySysNum)%MaxNoCoolHeatAirVolFlow = &
            UnitarySystem(UnitarySysNum)%MaxNoCoolHeatAirVolFlow * DXCoolCap
        END IF
      CASE (FlowPerHeatingCapacity)
        UnitarySystem(UnitarySysNum)%MaxNoCoolHeatAirVolFlow = &
          UnitarySystem(UnitarySysNum)%MaxNoCoolHeatAirVolFlow * &
            ZoneEqSizing(CurZoneEqNum)%DesHeatingLoad
      CASE (None)
        ! what does this mean?
      CASE DEFAULT
      ! wait for CR's
    END SELECT

  ELSE
    UnitarySystem(UnitarySysNum)%MaxNoCoolHeatAirVolFlow = 0.0d0
  END IF


  SELECT CASE(UnitarySystem(UnitarySysNum)%CoolingCoilType_Num)
  CASE(CoilDX_CoolingSingleSpeed)
    CALL SimDXCoil(Blank,On,.TRUE.,0.0d0,UnitarySystem(UnitarySysNum)%CoolingCoilIndex, 1)
    DXCoolCap = GetCoilCapacityByIndexType(UnitarySystem(UnitarySysNum)%CoolingCoilIndex, &
                                           UnitarySystem(UnitarySysNum)%CoolingCoilType_Num,ErrFound)
  CASE(CoilDX_CoolingHXAssisted,CoilWater_CoolingHXAssisted)
    Node(UnitarySystem(UnitarySysNum)%CoolCoilInletNodeNum)%MassFlowRate = SysCoolingFlow
    IF(UnitarySystem(UnitarySysNum)%CoolingCoilType_Num == CoilWater_CoolingHXAssisted)THEN
      HXCoilName = GetHXDXCoilName(cAllCoilTypes(UnitarySystem(UnitarySysNum)%CoolingCoilType_Num), &
                                   UnitarySystem(UnitarySysNum)%CoolingCoilName,ErrFound)
      ActualCoolCoilType = GetCoilObjectTypeNum(cAllCoilTypes(UnitarySystem(UnitarySysNum)%CoolingCoilType_Num), &
                           UnitarySystem(UnitarySysNum)%CoolingCoilName,ErrFound,.TRUE.)
      CALL SetCoilDesFlow(cAllCoilTypes(ActualCoolCoilType),HXCoilName,SysCoolingFlow,ErrFound)
    END IF
    CALL SimHXAssistedCoolingCoil(Blank,.TRUE.,On, &
                                  1.0d0, UnitarySystem(UnitarySysNum)%CoolingCoilIndex, 1, &
                                  HXUnitEnable=.FALSE., OnOffAFR = 1.0d0, EconomizerFlag=.FALSE.)
    IF(UnitarySystem(UnitarySysNum)%CoolingCoilType_Num == CoilWater_CoolingHXAssisted)THEN
      DXCoolCap = GetWaterCoilCapacity(MakeUPPERCase(cAllCoilTypes(ActualCoolCoilType)),HXCoilName,ErrFound)
    ELSE
      DXCoolCap = GetCoilCapacity(cAllCoilTypes(UnitarySystem(UnitarySysNum)%CoolingCoilType_Num), &
                                UnitarySystem(UnitarySysNum)%CoolingCoilName,ErrFound)
    END IF
  CASE(Coil_CoolingWaterDetailed)
    CALL SetCoilDesFlow(cAllCoilTypes(UnitarySystem(UnitarySysNum)%CoolingCoilType_Num), &
                        UnitarySystem(UnitarySysNum)%CoolingCoilName,SysCoolingFlow,ErrFound)
  CASE(Coil_CoolingWaterToAirHPSimple)
    CALL SimWatertoAirHPSimple(Blank, &
           UnitarySystem(UnitarySysNum)%CoolingCoilIndex, &
           UnitarySystem(UnitarySysNum)%CoolingCoilSensDemand, UnitarySystem(UnitarySysNum)%CoolingCoilLatentDemand, &
           0,0.0d0, UnitarySystem(UnitarySysNum)%MaxONOFFCyclesperHour, &
           UnitarySystem(UnitarySysNum)%HPTimeConstant, UnitarySystem(UnitarySysNum)%FanDelayTime, &
           0,0.0d0,FirstHVACIteration)
    DXCoolCap = GetSimpleCoilCapacity(cAllCoilTypes(UnitarySystem(UnitarySysNum)%CoolingCoilType_Num), &
                                      UnitarySystem(UnitarySysNum)%CoolingCoilName,ErrFound)
  CASE(Coil_CoolingWaterToAirHP)
    CALL SimWatertoAirHP(Blank, UnitarySystem(UnitarySysNum)%CoolingCoilIndex, &
           UnitarySystem(UnitarySysNum)%MaxCoolAirMassFlow,UnitarySystem(UnitarySysNum)%FanOpMode,&
           FirstHVACIteration,0.0d0,&
           UnitarySystem(UnitarySysNum)%MaxONOFFCyclesperHour, &
           UnitarySystem(UnitarySysNum)%HPTimeConstant, &
           UnitarySystem(UnitarySysNum)%FanDelayTime, &
           UnitarySystem(UnitarySysNum)%InitHeatPump, &
           0.0d0,0.0d0,0, 0.0d0)
    DXCoolCap = GetWAHPCoilCapacity(cAllCoilTypes(UnitarySystem(UnitarySysNum)%CoolingCoilType_Num), &
                                    UnitarySystem(UnitarySysNum)%CoolingCoilName,ErrFound)
  CASE(Coil_CoolingWaterToAirHPVSEquationFit, Coil_CoolingAirToAirVariableSpeed)

    IF (UnitarySystem(UnitarySysNum)%NumOfSpeedCooling .GT. 0)THEN
      IF(.NOT. ALLOCATED(UnitarySystem(UnitarySysNum)%CoolVolumeFlowRate)) &
        ALLOCATE(UnitarySystem(UnitarySysNum)%CoolVolumeFlowRate(UnitarySystem(UnitarySysNum)%NumOfSpeedCooling))

      IF(.NOT. ALLOCATED(UnitarySystem(UnitarySysNum)%CoolMassFlowRate)) &
        ALLOCATE(UnitarySystem(UnitarySysNum)%CoolMassFlowRate(UnitarySystem(UnitarySysNum)%NumOfSpeedCooling))

      IF(.NOT. ALLOCATED(UnitarySystem(UnitarySysNum)%MSCoolingSpeedRatio)) &
        ALLOCATE(UnitarySystem(UnitarySysNum)%MSCoolingSpeedRatio(UnitarySystem(UnitarySysNum)%NumOfSpeedCooling))
    END IF

    CALL SimVariableSpeedCoils(Blank,UnitarySystem(UnitarySysNum)%CoolingCoilIndex,&
           0,UnitarySystem(UnitarySysNum)%MaxONOFFCyclesperHour, &
           UnitarySystem(UnitarySysNum)%HPTimeConstant,UnitarySystem(UnitarySysNum)%FanDelayTime,&
           0,0.0d0,0.0d0,1,0.0d0,0.0d0,0.0d0 )     !conduct the sizing operation in the VS WSHP
    UnitarySystem(UnitarySysNum)%NumOfSpeedCooling = VarSpeedCoil(UnitarySystem(UnitarySysNum)%CoolingCoilIndex)%NumOfSpeeds
    DXCoolCap = GetCoilCapacityVariableSpeed(cAllCoilTypes(UnitarySystem(UnitarySysNum)%CoolingCoilType_Num), &
                                      UnitarySystem(UnitarySysNum)%CoolingCoilName,ErrFound)
    MulSpeedFlowScale = VarSpeedCoil(UnitarySystem(UnitarySysNum)%CoolingCoilIndex)%RatedAirVolFlowRate/ &
       VarSpeedCoil(UnitarySystem(UnitarySysNum)%CoolingCoilIndex)%MSRatedAirVolFlowRate(  &
          VarSpeedCoil(UnitarySystem(UnitarySysNum)%CoolingCoilIndex)%NormSpedLevel)
    DO Iter = 1,UnitarySystem(UnitarySysNum)%NumOfSpeedCooling
      UnitarySystem(UnitarySysNum)%CoolVolumeFlowRate(Iter) =   &
         VarSpeedCoil(UnitarySystem(UnitarySysNum)%CoolingCoilIndex)%MSRatedAirVolFlowRate(Iter) * MulSpeedFlowScale
      UnitarySystem(UnitarySysNum)%CoolMassFlowRate(Iter) = UnitarySystem(UnitarySysNum)%CoolVolumeFlowRate(Iter)* StdRhoAir
! it seems the ratio should reference the actual flow rates, not the fan flow ???
      IF(UnitarySystem(UnitarySysNum)%DesignFanVolFlowRate .GT. 0.0d0 .AND. UnitarySystem(UnitarySysNum)%FanExists)THEN
        UnitarySystem(UnitarySysNum)%MSCoolingSpeedRatio(Iter) = UnitarySystem(UnitarySysNum)%CoolVolumeFlowRate(Iter)/ &
!             UnitarySystem(UnitarySysNum)%CoolVolumeFlowRate(UnitarySystem(UnitarySysNum)%NumOfSpeedCooling)
              UnitarySystem(UnitarySysNum)%DesignFanVolFlowRate
      ELSE
        UnitarySystem(UnitarySysNum)%MSCoolingSpeedRatio(Iter) = UnitarySystem(UnitarySysNum)%CoolVolumeFlowRate(Iter)/ &
             UnitarySystem(UnitarySysNum)%CoolVolumeFlowRate(UnitarySystem(UnitarySysNum)%NumOfSpeedCooling)
      END IF
    END DO

    UnitarySystem(UnitarySysNum)%IdleVolumeAirRate = UnitarySystem(UnitarySysNum)%CoolVolumeFlowRate(1)
    UnitarySystem(UnitarySysNum)%IdleMassFlowRate  = UnitarySystem(UnitarySysNum)%CoolMassFlowRate(1)
    UnitarySystem(UnitarySysNum)%IdleSpeedRatio    = UnitarySystem(UnitarySysNum)%MSCoolingSpeedRatio(1)

  CASE(CoilDX_MultiSpeedCooling)

    IF (UnitarySystem(UnitarySysNum)%NumOfSpeedCooling .GT. 0)THEN
      IF(.NOT. ALLOCATED(UnitarySystem(UnitarySysNum)%CoolVolumeFlowRate)) &
        ALLOCATE(UnitarySystem(UnitarySysNum)%CoolVolumeFlowRate(UnitarySystem(UnitarySysNum)%NumOfSpeedCooling))

      IF(.NOT. ALLOCATED(UnitarySystem(UnitarySysNum)%CoolMassFlowRate)) &
        ALLOCATE(UnitarySystem(UnitarySysNum)%CoolMassFlowRate(UnitarySystem(UnitarySysNum)%NumOfSpeedCooling))

      IF(.NOT. ALLOCATED(UnitarySystem(UnitarySysNum)%MSCoolingSpeedRatio)) &
        ALLOCATE(UnitarySystem(UnitarySysNum)%MSCoolingSpeedRatio(UnitarySystem(UnitarySysNum)%NumOfSpeedCooling))
    END IF

    OnOffAirFlowRatio = 1.0d0
    PartLoadRatio = 1.0d0
    CALL SetOnOffMassFlowRate(UnitarySysNum, OnOffAirFlowRatio, PartLoadRatio)
    CALL SimDXCoilMultiSpeed(Blank,1.0d0,1.0d0,UnitarySystem(UnitarySysNum)%CoolingCoilIndex,0,0,0)
    DXCoolCap = GetCoilCapacityByIndexType(UnitarySystem(UnitarySysNum)%CoolingCoilIndex, &
                                           UnitarySystem(UnitarySysNum)%CoolingCoilType_Num,ErrFound)

    MSHPIndex = UnitarySystem(UnitarySysNum)%DesignSpecMSHPIndex

    IF(MSHPIndex .GT. 0)THEN
      DO Iter = DesignSpecMSHP(MSHPIndex)%NumOfSpeedCooling, 1 , -1 ! use reverse order since we divide by CoolVolumeFlowRate(max)
        IF(DesignSpecMSHP(MSHPIndex)%CoolingVolFlowRatio(Iter) == Autosize) &
             DesignSpecMSHP(MSHPIndex)%CoolingVolFlowRatio(Iter) = &
               REAL(Iter,r64)/REAL(DesignSpecMSHP(MSHPIndex)%NumOfSpeedCooling,r64)
        UnitarySystem(UnitarySysNum)%CoolVolumeFlowRate(Iter) =   UnitarySystem(UnitarySysNum)%MaxCoolAirVolFlow * &
                                                           DesignSpecMSHP(MSHPIndex)%CoolingVolFlowRatio(Iter)
        UnitarySystem(UnitarySysNum)%CoolMassFlowRate(Iter) = UnitarySystem(UnitarySysNum)%CoolVolumeFlowRate(Iter)* StdRhoAir
        UnitarySystem(UnitarySysNum)%MSCoolingSpeedRatio(Iter) = UnitarySystem(UnitarySysNum)%CoolVolumeFlowRate(Iter)/ &
                              UnitarySystem(UnitarySysNum)%CoolVolumeFlowRate(DesignSpecMSHP(MSHPIndex)%NumOfSpeedCooling)

      END DO
      UnitarySystem(UnitarySysNum)%IdleVolumeAirRate = UnitarySystem(UnitarySysNum)%CoolVolumeFlowRate(1)
      UnitarySystem(UnitarySysNum)%IdleMassFlowRate = UnitarySystem(UnitarySysNum)%CoolMassFlowRate(1)
      UnitarySystem(UnitarySysNum)%IdleSpeedRatio   = UnitarySystem(UnitarySysNum)%MSCoolingSpeedRatio(1)
    END IF

  CASE DEFAULT
  END SELECT


  IF (UnitarySystem(UnitarySysNum)%HeatingCoilType_Num == CoilDX_MultiSpeedHeating .OR. &
      UnitarySystem(UnitarySysNum)%HeatingCoilType_Num == Coil_HeatingElectric_MultiStage .OR. &
      UnitarySystem(UnitarySysNum)%HeatingCoilType_Num == Coil_HeatingGas_MultiStage) THEN

    IF (UnitarySystem(UnitarySysNum)%NumOfSpeedHeating .GT. 0)THEN
      IF(.NOT. ALLOCATED(UnitarySystem(UnitarySysNum)%HeatVolumeFlowRate)) &
        ALLOCATE(UnitarySystem(UnitarySysNum)%HeatVolumeFlowRate(UnitarySystem(UnitarySysNum)%NumOfSpeedHeating))

      IF(.NOT. ALLOCATED(UnitarySystem(UnitarySysNum)%HeatMassFlowRate)) &
        ALLOCATE(UnitarySystem(UnitarySysNum)%HeatMassFlowRate(UnitarySystem(UnitarySysNum)%NumOfSpeedHeating))

      IF(.NOT. ALLOCATED(UnitarySystem(UnitarySysNum)%MSHeatingSpeedRatio)) &
        ALLOCATE(UnitarySystem(UnitarySysNum)%MSHeatingSpeedRatio(UnitarySystem(UnitarySysNum)%NumOfSpeedHeating))
    END IF

    MSHPIndex = UnitarySystem(UnitarySysNum)%DesignSpecMSHPIndex

    IF(MSHPIndex .GT. 0)THEN
      DO Iter = DesignSpecMSHP(MSHPIndex)%NumOfSpeedHeating, 1 , -1 ! use reverse order since we divide by HeatVolumeFlowRate(max)
        IF(DesignSpecMSHP(MSHPIndex)%HeatingVolFlowRatio(Iter) == Autosize) &
             DesignSpecMSHP(MSHPIndex)%HeatingVolFlowRatio(Iter) = &
                REAL(Iter,r64)/REAL(DesignSpecMSHP(MSHPIndex)%NumOfSpeedHeating,r64)
        UnitarySystem(UnitarySysNum)%HeatVolumeFlowRate(Iter) =   UnitarySystem(UnitarySysNum)%MaxHeatAirVolFlow * &
                                                                  DesignSpecMSHP(MSHPIndex)%HeatingVolFlowRatio(Iter)
        UnitarySystem(UnitarySysNum)%HeatMassFlowRate(Iter) = UnitarySystem(UnitarySysNum)%HeatVolumeFlowRate(Iter)* StdRhoAir
        UnitarySystem(UnitarySysNum)%MSHeatingSpeedRatio(Iter) = UnitarySystem(UnitarySysNum)%HeatVolumeFlowRate(Iter)/ &
                                      UnitarySystem(UnitarySysNum)%HeatVolumeFlowRate(DesignSpecMSHP(MSHPIndex)%NumOfSpeedHeating)
      END DO
      UnitarySystem(UnitarySysNum)%IdleVolumeAirRate = UnitarySystem(UnitarySysNum)%HeatVolumeFlowRate(1)
      UnitarySystem(UnitarySysNum)%IdleMassFlowRate = UnitarySystem(UnitarySysNum)%HeatMassFlowRate(1)
      UnitarySystem(UnitarySysNum)%IdleSpeedRatio   = UnitarySystem(UnitarySysNum)%MSHeatingSpeedRatio(1)
    END IF
  ELSEIF (UnitarySystem(UnitarySysNum)%HeatingCoilType_Num == Coil_HeatingWaterToAirHPVSEquationFit .OR. &
            UnitarySystem(UnitarySysNum)%HeatingCoilType_Num == Coil_HeatingAirToAirVariableSpeed) THEN
    CALL SimVariableSpeedCoils(Blank,UnitarySystem(UnitarySysNum)%HeatingCoilIndex,&
           0,UnitarySystem(UnitarySysNum)%MaxONOFFCyclesperHour, &
           UnitarySystem(UnitarySysNum)%HPTimeConstant,UnitarySystem(UnitarySysNum)%FanDelayTime,&
           0,0.0d0,0.0d0,1,0.0d0,0.0d0,0.0d0 ) !conduct the sizing operation in the VS WSHP

    UnitarySystem(UnitarySysNum)%NumOfSpeedHeating = VarSpeedCoil(UnitarySystem(UnitarySysNum)%HeatingCoilIndex)%NumOfSpeeds

    IF (UnitarySystem(UnitarySysNum)%NumOfSpeedHeating .GT. 0)THEN
      IF(.NOT. ALLOCATED(UnitarySystem(UnitarySysNum)%HeatVolumeFlowRate)) &
        ALLOCATE(UnitarySystem(UnitarySysNum)%HeatVolumeFlowRate(UnitarySystem(UnitarySysNum)%NumOfSpeedHeating))

      IF(.NOT. ALLOCATED(UnitarySystem(UnitarySysNum)%HeatMassFlowRate)) &
        ALLOCATE(UnitarySystem(UnitarySysNum)%HeatMassFlowRate(UnitarySystem(UnitarySysNum)%NumOfSpeedHeating))

      IF(.NOT. ALLOCATED(UnitarySystem(UnitarySysNum)%MSHeatingSpeedRatio)) &
        ALLOCATE(UnitarySystem(UnitarySysNum)%MSHeatingSpeedRatio(UnitarySystem(UnitarySysNum)%NumOfSpeedHeating))
    END IF

    MulSpeedFlowScale = VarSpeedCoil(UnitarySystem(UnitarySysNum)%HeatingCoilIndex)%RatedAirVolFlowRate/ &
                        VarSpeedCoil(UnitarySystem(UnitarySysNum)%HeatingCoilIndex)%MSRatedAirVolFlowRate(  &
                           VarSpeedCoil(UnitarySystem(UnitarySysNum)%HeatingCoilIndex)%NormSpedLevel)
    DO Iter = 1,UnitarySystem(UnitarySysNum)%NumOfSpeedHeating
      UnitarySystem(UnitarySysNum)%HeatVolumeFlowRate(Iter) =   &
         VarSpeedCoil(UnitarySystem(UnitarySysNum)%HeatingCoilIndex)%MSRatedAirVolFlowRate(Iter) * MulSpeedFlowScale
      UnitarySystem(UnitarySysNum)%HeatMassFlowRate(Iter) = UnitarySystem(UnitarySysNum)%HeatVolumeFlowRate(Iter)* StdRhoAir
      IF(UnitarySystem(UnitarySysNum)%DesignFanVolFlowRate .GT. 0.0d0 .AND. UnitarySystem(UnitarySysNum)%FanExists)THEN
        UnitarySystem(UnitarySysNum)%MSHeatingSpeedRatio(Iter) = UnitarySystem(UnitarySysNum)%HeatVolumeFlowRate(Iter)/ &
          UnitarySystem(UnitarySysNum)%DesignFanVolFlowRate
      ELSE
        UnitarySystem(UnitarySysNum)%MSHeatingSpeedRatio(Iter) = UnitarySystem(UnitarySysNum)%HeatVolumeFlowRate(Iter)/ &
          UnitarySystem(UnitarySysNum)%HeatVolumeFlowRate(UnitarySystem(UnitarySysNum)%NumOfSpeedHeating)
      END IF

    END DO

    IF(UnitarySystem(UnitarySysNum)%CoolCoilExists)THEN
      IF(ALLOCATED(UnitarySystem(UnitarySysNum)%CoolVolumeFlowRate))THEN
        UnitarySystem(UnitarySysNum)%IdleVolumeAirRate = &
          MIN(UnitarySystem(UnitarySysNum)%IdleVolumeAirRate,UnitarySystem(UnitarySysNum)%HeatVolumeFlowRate(1))
        UnitarySystem(UnitarySysNum)%IdleMassFlowRate  = &
          MIN(UnitarySystem(UnitarySysNum)%IdleMassFlowRate,UnitarySystem(UnitarySysNum)%HeatMassFlowRate(1))
        UnitarySystem(UnitarySysNum)%IdleSpeedRatio    = &
          MIN(UnitarySystem(UnitarySysNum)%IdleSpeedRatio,UnitarySystem(UnitarySysNum)%MSHeatingSpeedRatio(1))
      ELSE
        UnitarySystem(UnitarySysNum)%IdleVolumeAirRate = UnitarySystem(UnitarySysNum)%HeatVolumeFlowRate(1)
        UnitarySystem(UnitarySysNum)%IdleMassFlowRate  = UnitarySystem(UnitarySysNum)%HeatMassFlowRate(1)
        UnitarySystem(UnitarySysNum)%IdleSpeedRatio    = UnitarySystem(UnitarySysNum)%MSHeatingSpeedRatio(1)
      END IF
    ELSE
      UnitarySystem(UnitarySysNum)%IdleVolumeAirRate = UnitarySystem(UnitarySysNum)%HeatVolumeFlowRate(1)
      UnitarySystem(UnitarySysNum)%IdleMassFlowRate  = UnitarySystem(UnitarySysNum)%HeatMassFlowRate(1)
      UnitarySystem(UnitarySysNum)%IdleSpeedRatio    = UnitarySystem(UnitarySysNum)%MSHeatingSpeedRatio(1)
    END IF
  END IF

  IF(UnitarySystem(UnitarySysNum)%CoolCoilExists .AND. UnitarySystem(UnitarySysNum)%MaxCoolAirVolFlow < 0.0d0)THEN
    IF ( .NOT. SysSizingRunDone) THEN
      BranchNum = GetAirBranchIndex('AirloopHVAC:UnitarySystem',UnitarySystem(UnitarySysNum)%Name)
      FanType = ' '
      FanName = ' '
      BranchFanFlow = 0.0d0
      IF(BranchNum .GT. 0.0d0)CALL GetBranchFanTypeName(BranchNum, FanType, FanName, ErrFound)
      IF(.NOT. ErrFound .AND. BranchNum .GT. 0)BranchFanFlow = GetFanDesignVolumeFlowRate(FanType,FanName,ErrFound)
      IF(BranchNum .GT. 0.0d0)BranchFlow = GetBranchFlow(BranchNum)
      IF(BranchFanFlow > 0.0d0)THEN
        UnitarySystem(UnitarySysNum)%MaxCoolAirVolFlow = BranchFanFlow
      ELSEIF(BranchFlow > 0.0d0)THEN
        UnitarySystem(UnitarySysNum)%MaxCoolAirVolFlow = BranchFlow
      ELSEIF(BranchFlow == Autosize)THEN
        ! what do I do?
      END IF
    END IF
  END IF

    !Change the Volume Flow Rates to Mass Flow Rates
    UnitarySystem(UnitarySysNum)%DesignMassFlowRate = UnitarySystem(UnitarySysNum)%DesignFanVolFlowRate * StdRhoAir
    UnitarySystem(UnitarySysNum)%MaxCoolAirMassFlow = UnitarySystem(UnitarySysNum)%MaxCoolAirVolFlow * StdRhoAir
    UnitarySystem(UnitarySysNum)%MaxHeatAirMassFlow = UnitarySystem(UnitarySysNum)%MaxHeatAirVolFlow* StdRhoAir
    UnitarySystem(UnitarySysNum)%MaxNoCoolHeatAirMassFlow = UnitarySystem(UnitarySysNum)%MaxNoCoolHeatAirVolFlow* StdRhoAir

    UnitarySystem(UnitarySysNum)%SenLoadLoss = 0.0d0
    IF (UnitarySystem(UnitarySysNum)%Humidistat) THEN
      UnitarySystem(UnitarySysNum)%LatLoadLoss = 0.0d0
    END IF

  IF (UnitarySystem(UnitarySysNum)%DesignHeatingCapacity == AutoSize) THEN

    IF (CurOASysNum > 0) THEN
      IF(OASysEqSizing(CurOASysNum)%Capacity)THEN
        IF(OASysEqSizing(CurOASysNum)%DesHeatingLoad .GT. 0.0d0)THEN
          UnitarySystem(UnitarySysNum)%DesignHeatingCapacity = OASysEqSizing(CurOASysNum)%DesHeatingLoad
        ELSE
          UnitarySystem(UnitarySysNum)%DesignHeatingCapacity = OASysEqSizing(CurOASysNum)%DesCoolingLoad * &
                                                               UnitarySystem(UnitarySysNum)%HeatingSizingRatio
        END IF
      ELSE
        IF (DXCoolCap >= SmallLoad) THEN
          UnitarySystem(UnitarySysNum)%DesignHeatingCapacity = DXCoolCap * UnitarySystem(UnitarySysNum)%HeatingSizingRatio
        ELSE
          UnitarySystem(UnitarySysNum)%DesignHeatingCapacity = 0.0d0
        END IF
      END IF
    ELSE IF (CurZoneEqNum > 0) THEN
      IF(ZoneEqSizing(CurZoneEqNum)%Capacity)THEN
        IF(ZoneEqSizing(CurZoneEqNum)%DesHeatingLoad .GT. 0.0d0)THEN
          UnitarySystem(UnitarySysNum)%DesignHeatingCapacity = ZoneEqSizing(CurZoneEqNum)%DesHeatingLoad
        ELSE
          UnitarySystem(UnitarySysNum)%DesignHeatingCapacity = ZoneEqSizing(CurZoneEqNum)%DesCoolingLoad * &
                                                               UnitarySystem(UnitarySysNum)%HeatingSizingRatio
        END IF
      ELSE
        IF (DXCoolCap >= SmallLoad) THEN
          UnitarySystem(UnitarySysNum)%DesignHeatingCapacity = DXCoolCap * UnitarySystem(UnitarySysNum)%HeatingSizingRatio
        ELSE
          UnitarySystem(UnitarySysNum)%DesignHeatingCapacity = 0.0d0
        END IF
      END IF
    ELSE IF(DXCoolCap > 0 .AND. UnitarySystem(UnitarySysNum)%HeatPump) THEN
      IF (DXCoolCap >= SmallLoad) THEN
        UnitarySystem(UnitarySysNum)%DesignHeatingCapacity = DXCoolCap * UnitarySystem(UnitarySysNum)%HeatingSizingRatio
      ELSE
        UnitarySystem(UnitarySysNum)%DesignHeatingCapacity = 0.0d0
      END IF
    ELSE IF (CurSysNum > 0) THEN

      IF (UnitarySystem(UnitarySysNum)%HeatingCoilType_Num == Coil_HeatingAirToAirVariableSpeed .OR. &
          UnitarySystem(UnitarySysNum)%HeatingCoilType_Num == CoilDX_MultiSpeedHeating .OR. &
          UnitarySystem(UnitarySysNum)%HeatingCoilType_Num == Coil_HeatingWaterToAirHPSimple .OR. &
          UnitarySystem(UnitarySysNum)%HeatingCoilType_Num == Coil_HeatingWaterToAirHP .OR. &
          UnitarySystem(UnitarySysNum)%HeatingCoilType_Num == Coil_HeatingWaterToAirHPVSEquationFit .OR. &
          UnitarySystem(UnitarySysNum)%HeatingCoilType_Num == CoilDX_HeatingEmpirical)  THEN
        ! for now, find the associated DX cooling coil to identically size heating coil
        DXHeatCoilBranch = 0
        DXHeatCoilCompNum = 0
        IF(UnitarySystem(UnitarySysNum)%DXHeatingCoil .AND. .NOT. UnitarySystem(UnitarySysNum)%CoolCoilExists)THEN
          BRANCHLoop: DO BranchNum = 1, PrimaryAirSystem(AirLoopNum)%NumBranches
            DO CompNum = 1, PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%TotalComponents
              IF(.NOT. SameString(PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%TypeOf, &
                           UnitarySystem(UnitarySysNum)%UnitarySystemType)) CYCLE
              IF(.NOT. SameString(PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%Name, &
                           UnitarySystem(UnitarySysNum)%Name)) CYCLE
              DXHeatCoilBranch = BranchNum
              DXHeatCoilCompNum = CompNum
              EXIT BRANCHLoop
            END DO
          END DO BRANCHLoop
        END IF
        IF(DXHeatCoilCompNum .GT. 0)THEN
          DO CompNum = PrimaryAirSystem(AirLoopNum)%Branch(DXHeatCoilBranch)%TotalComponents, 1, -1
            IF(SameString(PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%TypeOf, &
                        UnitarySystem(UnitarySysNum)%UnitarySystemType)) THEN
              IF(SameString(PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%Name, &
                        UnitarySystem(UnitarySysNum)%Name))CYCLE
              CoolCoilIndex = &
                GetUnitarySystemDXCoolingCoilIndex(PrimaryAirSystem(AirLoopNum)%Branch(DXHeatCoilBranch)%Comp(CompNum)%Name)
              IF(CoolCoilIndex .GT. 0)THEN
                CoolUnitarySystemNum = FinditemInList(PrimaryAirSystem(AirLoopNum)%Branch(DXHeatCoilBranch)%Comp(CompNum)%Name, &
                                                    UnitarySystem%Name,NumUnitarySystem)
                CoolCoilType = UnitarySystem(CoolUnitarySystemNum)%CoolingCoilType_Num
                DXCoolCap = GetCoilCapacityByIndexType(CoolCoilIndex,CoolCoilType,ErrFound)
                UnitarySystem(UnitarySysNum)%DesignHeatingCapacity = DXCoolCap*UnitarySystem(UnitarySysNum)%HeatingSizingRatio
                EXIT
              END IF
            ELSE IF(SameString(PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%TypeOf, &
                        'CoilSystem:Cooling:DX'))THEN
              CoolCoilType  = 0
              CoolCoilIndex = 0
              CoolCoilName =' '
              CALL GetCoolingCoilTypeNameAndIndex(PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%Name, &
                                              CoolCoilType,CoolCoilIndex,CoolCoilName,ErrFound)
              SELECT CASE(CoolCoilType)
                CASE(CoilDX_CoolingSingleSpeed,CoilDX_CoolingTwoSpeed,CoilDX_CoolingTwoStageWHumControl)
                  DXCoolCap = GetCoilCapacityByIndexType(CoolCoilIndex,CoolCoilType,ErrFound)
                CASE(CoilDX_CoolingHXAssisted)
                  DXCoolCap = GetCoilCapacity(cAllCoilTypes(CoolCoilType), CoolCoilName,ErrFound)
                CASE(Coil_CoolingAirToAirVariableSpeed)
                  DXCoolCap = GetCoilCapacityByIndexType(CoolCoilIndex,CoolCoilType,ErrFound)
                ! the following CASE's are not allowed in CoilSystem:Cooling:DX
                CASE(Coil_CoolingWaterToAirHPSimple)
                  DXCoolCap = GetSimpleCoilCapacity(cAllCoilTypes(CoolCoilType), CoolCoilName,ErrFound)
                CASE(Coil_CoolingWaterToAirHP)
                  DXCoolCap = GetWAHPCoilCapacity(cAllCoilTypes(CoolCoilType), CoolCoilName,ErrFound)
                CASE(Coil_CoolingWaterToAirHPVSEquationFit)
                  DXCoolCap = GetCoilCapacityVariableSpeed(cAllCoilTypes(CoolCoilType), CoolCoilName,ErrFound)
                CASE(CoilDX_MultiSpeedCooling)
                  DXCoolCap = GetCoilCapacityByIndexType(CoolCoilIndex,CoolCoilType,ErrFound)
                CASE DEFAULT
              END SELECT

              UnitarySystem(UnitarySysNum)%CoolingCoilIndex = CoolCoilIndex
              IF(UnitarySystem(UnitarySysNum)%CoolingCoilIndex .GT. 0)THEN
                IF (DXCoolCap > 0.0d0) UnitarySystem(UnitarySysNum)%DesignHeatingCapacity = &
                   DXCoolCap*UnitarySystem(UnitarySysNum)%HeatingSizingRatio
!                EXIT - don't exit, see if there is a UnitarySystem on the branch more upstream of the CoilSystem
              END IF
            END IF
          END DO
          IF(DXCoolCap == 0.0d0)THEN
            IF(UnitarySystem(UnitarySysNum)%DesignHeatingCapacity == Autosize)THEN
              VolFlowRate = UnitarySystem(UnitarySysNum)%MaxHeatAirVolFlow
              IF (VolFlowRate >= SmallAirVolFlow) THEN
                IF (CurOASysNum > 0) THEN ! coil is in the OA stream
                  MixTemp = FinalSysSizing(CurSysNum)%HeatOutTemp
                  MixHumRat = FinalSysSizing(CurSysNum)%HeatOutHumRat
                  SupTemp = FinalSysSizing(CurSysNum)%PrecoolTemp
                  SupHumRat = FinalSysSizing(CurSysNum)%PrecoolHumRat
                  OutTemp = FinalSysSizing(CurSysNum)%HeatOutTemp
                ELSE IF(CurSysNum > 0)THEN ! coil is on the main air loop
                  SupTemp = FinalSysSizing(CurSysNum)%HeatSupTemp
                  SupHumRat = FinalSysSizing(CurSysNum)%HeatSupHumRat
                  MixTemp = FinalSysSizing(CurSysNum)%HeatMixTemp
                  MixHumRat = FinalSysSizing(CurSysNum)%HeatMixHumRat
                  OutTemp = FinalSysSizing(CurSysNum)%HeatOutTemp
                ELSE ! coil is zone equipment
                  IF (ZoneEqSizing(CurZoneEqNum)%OAVolFlow > 0.0d0) THEN
                    MixTemp = FinalZoneSizing(CurZoneEqNum)%DesHeatCoilInTemp
                    MixHumRat = FinalZoneSizing(CurZoneEqNum)%DesHeatCoilInHumRat
                  ELSE
                    MixTemp = FinalZoneSizing(CurZoneEqNum)%ZoneRetTempAtCoolPeak
                    MixHumRat = FinalZoneSizing(CurZoneEqNum)%ZoneHumRatAtCoolPeak
                  END IF
                  SupTemp = FinalZoneSizing(CurZoneEqNum)%HeatDesTemp
                  SupHumRat = FinalZoneSizing(CurZoneEqNum)%HeatDesHumRat
                  TimeStepNumAtMax = FinalZoneSizing(CurZoneEqNum)%TimeStepNumAtCoolMax
                  DDNum = FinalZoneSizing(CurZoneEqNum)%CoolDDNum
                  IF (DDNum > 0 .and. TimeStepNumAtMax > 0) THEN
                    OutTemp = DesDayWeath(DDNum)%Temp(TimeStepNumAtMax)
                  ELSE
                    OutTemp = 0.0d0
                  ENDIF
                END IF
                rhoair = PsyRhoAirFnPbTdbW(StdBaroPress,MixTemp,MixHumRat,RoutineName)
                MixEnth = PsyHFnTdbW(MixTemp,MixHumRat,RoutineName)
                MixWetBulb = PsyTwbFnTdbWPb(MixTemp,MixHumRat,StdBaroPress,RoutineName)
                SupEnth = PsyHFnTdbW(SupTemp,SupHumRat,RoutineName)
                IF(UnitarySystem(UnitarySysNum)%HeatingCoilType_Num == CoilDX_MultiSpeedHeating .OR. &
                   UnitarySystem(UnitarySysNum)%HeatingCoilType_Num == CoilDX_HeatingEmpirical)THEN
                  CapFTCurve = GetDXCoilCapFTCurveIndex(UnitarySystem(UnitarySysNum)%HeatingCoilIndex,ErrFound)
                  TotCapTempModFac = CurveValue(CapFTCurve,MixWetBulb,OutTemp)
                ELSE
                  TotCapTempModFac = 1.0d0
                END IF
                HeatCapAtPeak = MAX(0.0d0, (rhoair * VolFlowRate * (SupEnth-MixEnth)))
                IF(TotCapTempModFac .GT. 0.0d0)THEN
                  UnitarySystem(UnitarySysNum)%DesignHeatingCapacity = HeatCapAtPeak / TotCapTempModFac
                ELSE
                  UnitarySystem(UnitarySysNum)%DesignHeatingCapacity = HeatCapAtPeak
                END IF
                IF(UnitarySystem(UnitarySysNum)%DesignHeatingCapacity .GT. 0.d0)THEN
                  RatedVolFlowPerRatedTotCap = UnitarySystem(UnitarySysNum)%MaxHeatAirVolFlow /   &
                     UnitarySystem(UnitarySysNum)%DesignHeatingCapacity
                ELSE
                  RatedVolFlowPerRatedTotCap = 0.0d0
                END IF
                ! check capacity to make sure design volume flow per total capacity is within range
                IF (RatedVolFlowPerRatedTotCap .LT. MinRatedVolFlowPerRatedTotCap1) THEN
                  HeatCapAtPeak = UnitarySystem(UnitarySysNum)%MaxHeatAirVolFlow &
                                                   / MinRatedVolFlowPerRatedTotCap1
                  UnitarySystem(UnitarySysNum)%DesignHeatingCapacity = HeatCapAtPeak
                  DXCoolCap = HeatCapAtPeak
                ELSEIF (RatedVolFlowPerRatedTotCap .GT. MaxRatedVolFlowPerRatedTotCap1) THEN
                  HeatCapAtPeak = UnitarySystem(UnitarySysNum)%MaxHeatAirVolFlow &
                                                    / MaxRatedVolFlowPerRatedTotCap1
                  UnitarySystem(UnitarySysNum)%DesignHeatingCapacity = HeatCapAtPeak
                  DXCoolCap = HeatCapAtPeak
                END IF
              END IF ! IF (VolFlowRate >= SmallAirVolFlow) THEN
            END IF ! IF(UnitarySystem(UnitarySysNum)%DesignHeatingCapacity == Autosize)THEN
          END IF ! IF(DXCoolCap == 0.0d0)THEN
        ELSE
          IF(UnitarySystem(UnitarySysNum)%DesignHeatingCapacity == Autosize)THEN
            VolFlowRate = UnitarySystem(UnitarySysNum)%MaxHeatAirVolFlow
            IF (VolFlowRate >= SmallAirVolFlow) THEN
              IF (CurOASysNum > 0) THEN ! coil is in the OA stream
                MixTemp = FinalSysSizing(CurSysNum)%HeatOutTemp
                MixHumRat = FinalSysSizing(CurSysNum)%HeatOutHumRat
                SupTemp = FinalSysSizing(CurSysNum)%PrecoolTemp
                SupHumRat = FinalSysSizing(CurSysNum)%PrecoolHumRat
                OutTemp = FinalSysSizing(CurSysNum)%HeatOutTemp
              ELSE IF(CurSysNum > 0)THEN ! coil is on the main air loop
                SupTemp = FinalSysSizing(CurSysNum)%HeatSupTemp
                SupHumRat = FinalSysSizing(CurSysNum)%HeatSupHumRat
                MixTemp = FinalSysSizing(CurSysNum)%HeatMixTemp
                MixHumRat = FinalSysSizing(CurSysNum)%HeatMixHumRat
                OutTemp = FinalSysSizing(CurSysNum)%HeatOutTemp
              ELSE ! coil is zone equipment
                IF (ZoneEqSizing(CurZoneEqNum)%OAVolFlow > 0.0d0) THEN
                  MixTemp = FinalZoneSizing(CurZoneEqNum)%DesHeatCoilInTemp
                  MixHumRat = FinalZoneSizing(CurZoneEqNum)%DesHeatCoilInHumRat
                ELSE
                  MixTemp = FinalZoneSizing(CurZoneEqNum)%ZoneRetTempAtCoolPeak
                  MixHumRat = FinalZoneSizing(CurZoneEqNum)%ZoneHumRatAtCoolPeak
                END IF
                SupTemp = FinalZoneSizing(CurZoneEqNum)%HeatDesTemp
                SupHumRat = FinalZoneSizing(CurZoneEqNum)%HeatDesHumRat
                TimeStepNumAtMax = FinalZoneSizing(CurZoneEqNum)%TimeStepNumAtHeatMax
                DDNum = FinalZoneSizing(CurZoneEqNum)%HeatDDNum
                IF (DDNum > 0 .and. TimeStepNumAtMax > 0) THEN
                  OutTemp = DesDayWeath(DDNum)%Temp(TimeStepNumAtMax)
                ELSE
                  OutTemp = 0.0d0
                ENDIF
              END IF
              rhoair = PsyRhoAirFnPbTdbW(StdBaroPress,MixTemp,MixHumRat,RoutineName)
              MixEnth = PsyHFnTdbW(MixTemp,MixHumRat,RoutineName)
              MixWetBulb = PsyTwbFnTdbWPb(MixTemp,MixHumRat,StdBaroPress,RoutineName)
              SupEnth = PsyHFnTdbW(SupTemp,SupHumRat,RoutineName)
              IF(UnitarySystem(UnitarySysNum)%HeatingCoilType_Num == CoilDX_MultiSpeedHeating .OR. &
                 UnitarySystem(UnitarySysNum)%HeatingCoilType_Num == CoilDX_HeatingEmpirical)THEN
                CapFTCurve = GetDXCoilCapFTCurveIndex(UnitarySystem(UnitarySysNum)%HeatingCoilIndex,ErrFound)
                TotCapTempModFac = CurveValue(CapFTCurve,MixWetBulb,OutTemp)
              ELSE
                TotCapTempModFac = 1.0d0
              END IF
              HeatCapAtPeak = MAX(0.0d0, (rhoair * VolFlowRate * (SupEnth-MixEnth)))
              IF(TotCapTempModFac .GT. 0.0d0)THEN
                UnitarySystem(UnitarySysNum)%DesignHeatingCapacity = HeatCapAtPeak / TotCapTempModFac
              ELSE
                UnitarySystem(UnitarySysNum)%DesignHeatingCapacity = HeatCapAtPeak
              END IF
              DXCoolCap = UnitarySystem(UnitarySysNum)%DesignHeatingCapacity
              IF(UnitarySystem(UnitarySysNum)%DesignHeatingCapacity .GT. 0.d0)THEN
                RatedVolFlowPerRatedTotCap = UnitarySystem(UnitarySysNum)%MaxHeatAirVolFlow /   &
                   UnitarySystem(UnitarySysNum)%DesignHeatingCapacity
              ELSE
                RatedVolFlowPerRatedTotCap = 0.0d0
              END IF
              ! check capacity to make sure design volume flow per total capacity is within range
              IF (RatedVolFlowPerRatedTotCap .LT. MinRatedVolFlowPerRatedTotCap1) THEN
                HeatCapAtPeak = UnitarySystem(UnitarySysNum)%MaxHeatAirVolFlow &
                                                 / MinRatedVolFlowPerRatedTotCap1
                UnitarySystem(UnitarySysNum)%DesignHeatingCapacity = HeatCapAtPeak
                DXCoolCap = HeatCapAtPeak
              ELSEIF (RatedVolFlowPerRatedTotCap .GT. MaxRatedVolFlowPerRatedTotCap1) THEN
                HeatCapAtPeak = UnitarySystem(UnitarySysNum)%MaxHeatAirVolFlow &
                                                  / MaxRatedVolFlowPerRatedTotCap1
                UnitarySystem(UnitarySysNum)%DesignHeatingCapacity = HeatCapAtPeak
                DXCoolCap = HeatCapAtPeak
              END IF
            END IF ! IF (VolFlowRate >= SmallAirVolFlow) THEN
          END IF ! IF(UnitarySystem(UnitarySysNum)%DesignHeatingCapacity == Autosize)THEN
        END IF ! IF(DXHeatCoilCompNum .GT. 0)THEN

      ELSE ! else not a HP heating coil
        IF (CurOASysNum > 0) THEN
          IF(OASysEqSizing(CurOASysNum)%Capacity) &
            UnitarySystem(UnitarySysNum)%DesignHeatingCapacity = OASysEqSizing(CurOASysNum)%DesHeatingLoad
        ELSE IF (CurSysNum > 0) THEN
          IF(UnitarySysEqSizing(CurSysNum)%Capacity) &
            UnitarySystem(UnitarySysNum)%DesignHeatingCapacity = UnitarySysEqSizing(CurSysNum)%DesHeatingLoad
        ELSE IF (CurZoneEqNum > 0) THEN
          IF(ZoneEqSizing(CurZoneEqNum)%Capacity) &
            UnitarySystem(UnitarySysNum)%DesignHeatingCapacity = ZoneEqSizing(CurZoneEqNum)%DesHeatingLoad
        END IF
        IF (UnitarySystem(UnitarySysNum)%DesignHeatingCapacity == Autosize) THEN
          IF (CurOASysNum > 0) THEN
            IF(OASysEqSizing(CurOASysNum)%AirFlow)THEN
              VolFlowRate = OASysEqSizing(CurOASysNum)%AirVolFlow
            ELSE
              VolFlowRate = FinalSysSizing(CurSysNum)%DesOutAirVolFlow
            END IF
          ELSE
            IF(CurZoneEqNum > 0)THEN
              IF(ZoneEqSizing(CurZoneEqNum)%AirFlow)THEN
                VolFlowRate = ZoneEqSizing(CurZoneEqNum)%AirVolFlow
              ELSE
                VolFlowRate = FinalZoneSizing(CurZoneEqNum)%DesHeatVolFlow
              END IF
            ELSE
              IF(UnitarySysEqSizing(CurSysNum)%AirFlow)THEN
                VolFlowRate = UnitarySysEqSizing(CurSysNum)%AirVolFlow
              ELSE
                SELECT CASE(CurDuctType)
                  CASE(Main)
                    VolFlowRate = FinalSysSizing(CurSysNum)%SysAirMinFlowRat*FinalSysSizing(CurSysNum)%DesMainVolFlow
                  CASE(Cooling)
                    VolFlowRate = FinalSysSizing(CurSysNum)%SysAirMinFlowRat*FinalSysSizing(CurSysNum)%DesCoolVolFlow
                  CASE(Heating)
                    VolFlowRate = FinalSysSizing(CurSysNum)%DesHeatVolFlow
                  CASE(Other)
                    VolFlowRate = FinalSysSizing(CurSysNum)%DesMainVolFlow
                  CASE DEFAULT
                    VolFlowRate = FinalSysSizing(CurSysNum)%DesMainVolFlow
                END SELECT
              END IF
            END IF
          END IF
          ! get the outside air fraction
          IF (CurOASysNum > 0) THEN
            OutAirFrac = 1.0d0
          ELSE IF (CurSysNum > 0) THEN
            IF (FinalSysSizing(CurSysNum)%HeatOAOption == MinOA) THEN
              IF (VolFlowRate > 0.0d0) THEN
                OutAirFrac = FinalSysSizing(CurSysNum)%DesOutAirVolFlow / VolFlowRate
              ELSE
                OutAirFrac = 1.0d0
              END IF
            ELSE
              OutAirFrac = 1.0d0
            END IF
            OutAirFrac = MIN(1.0d0,MAX(0.0d0,OutAirFrac))
          ELSE
            OutAirFrac = 1.0d0
          END IF
          ! coil inlet temperature
          IF (CurZoneEqNum > 0) THEN
            MixTemp = FinalZoneSizing(CurZoneEqNum)%DesHeatCoilInTemp
            CpAirStd = PsyCpAirFnWTdb(0.0d0,20.0d0)
            HeatCapAtPeak = CpAirStd*StdRhoAir*VolFlowRate*(FinalZoneSizing(CurZoneEqNum)%HeatDesTemp - MixTemp)
            ZoneEqSizing(CurZoneEqNum)%Capacity = .TRUE.
            ZoneEqSizing(CurZoneEqNum)%DesHeatingLoad = HeatCapAtPeak
          ELSE
            IF (CurOASysNum == 0 .AND. PrimaryAirSystem(CurSysNum)%NumOAHeatCoils > 0) THEN
              MixTemp = OutAirFrac*FinalSysSizing(CurSysNum)%PreheatTemp + &
                         (1.0d0-OutAirFrac)*FinalSysSizing(CurSysNum)%HeatRetTemp
            ELSE
              MixTemp = OutAirFrac*FinalSysSizing(CurSysNum)%HeatOutTemp + &
                         (1.0d0-OutAirFrac)*FinalSysSizing(CurSysNum)%HeatRetTemp
            END IF
            ! coil load
            IF (CurOASysNum > 0) THEN
              IF(OASysEqSizing(CurOASysNum)%Capacity)THEN
                HeatCapAtPeak = OASysEqSizing(CurOASysNum)%DesHeatingLoad
              ELSE
                CpAirStd = PsyCpAirFnWTdb(0.0d0,20.0d0)
                HeatCapAtPeak = CpAirStd*StdRhoAir*VolFlowRate*(FinalSysSizing(CurSysNum)%PreheatTemp - MixTemp)
              END IF
            ELSE
              IF(UnitarySysEqSizing(CurSysNum)%Capacity)THEN
                HeatCapAtPeak = UnitarySysEqSizing(CurSysNum)%DesHeatingLoad
              ELSE
                CpAirStd = PsyCpAirFnWTdb(0.0d0,20.0d0)
                HeatCapAtPeak = CpAirStd*StdRhoAir*VolFlowRate*(FinalSysSizing(CurSysNum)%HeatSupTemp - MixTemp)
              END IF
            END IF
            UnitaryHeatCap = HeatCapAtPeak
          END IF
          UnitarySystem(UnitarySysNum)%DesignHeatingCapacity = HeatCapAtPeak
        END IF
      END IF

      IF (UnitarySystem(UnitarySysNum)%DesignHeatingCapacity < SmallLoad) THEN
        UnitarySystem(UnitarySysNum)%DesignHeatingCapacity = 0.0d0
      END IF

    END IF

    CALL ReportSizingOutput(UnitarySystem(UnitarySysNum)%UnitarySystemType, UnitarySystem(UnitarySysNum)%Name, &
                            'Nominal Heating Capacity [W]', &
                            UnitarySystem(UnitarySysNum)%DesignHeatingCapacity)

  END IF

  UnitaryHeatCap = UnitarySystem(UnitarySysNum)%DesignHeatingCapacity

  IF (UnitarySystem(UnitarySysNum)%DesignCoolingCapacity == AutoSize) THEN

    IF (CurOASysNum > 0) THEN
      IF (DXCoolCap >= SmallLoad) THEN
        UnitarySystem(UnitarySysNum)%DesignCoolingCapacity = DXCoolCap
      ELSE
        UnitarySystem(UnitarySysNum)%DesignCoolingCapacity = 0.0d0
      END IF
    ELSE IF (CurZoneEqNum > 0) THEN

      IF (DXCoolCap >= SmallLoad) THEN
        UnitarySystem(UnitarySysNum)%DesignCoolingCapacity = DXCoolCap
      ELSE
        UnitarySystem(UnitarySysNum)%DesignCoolingCapacity = 0.0d0
      END IF
    ELSE IF (CurSysNum > 0) THEN

      IF (DXCoolCap >= SmallLoad) THEN
        UnitarySystem(UnitarySysNum)%DesignCoolingCapacity = DXCoolCap
      ELSE
        UnitarySystem(UnitarySysNum)%DesignCoolingCapacity = 0.0d0
      END IF
    END IF

    CALL ReportSizingOutput(UnitarySystem(UnitarySysNum)%UnitarySystemType, UnitarySystem(UnitarySysNum)%Name, &
                            'Nominal Cooling Capacity [W]', &
                            UnitarySystem(UnitarySysNum)%DesigncoolingCapacity)

  END IF

  IF (UnitarySystem(UnitarySysNum)%DesignMaxOutletTemp == AutoSize .AND. &
      (UnitarySystem(UnitarySysNum)%HeatCoilExists .OR. UnitarySystem(UnitarySysNum)%SuppCoilExists)) THEN

    IF (CurOASysNum > 0) THEN
      UnitarySystem(UnitarySysNum)%DesignMaxOutletTemp = FinalSysSizing(CurSysNum)%HeatSupTemp
    ELSE IF (CurZoneEqNum > 0) THEN
      UnitarySystem(UnitarySysNum)%DesignMaxOutletTemp = FinalZoneSizing(CurZoneEqNum)%HeatDesTemp
    ELSE IF (CurSysNum > 0) THEN
      UnitarySystem(UnitarySysNum)%DesignMaxOutletTemp = FinalSysSizing(CurSysNum)%HeatSupTemp
    END IF

    CALL ReportSizingOutput(UnitarySystem(UnitarySysNum)%UnitarySystemType, UnitarySystem(UnitarySysNum)%Name, &
                            'Maximum Supply Air Temperature from Unitary Heater [C]', &
                            UnitarySystem(UnitarySysNum)%DesignMaxOutletTemp)

  END IF

  IF (UnitarySystem(UnitarySysNum)%DesignSuppHeatingCapacity == AutoSize) THEN

    IF (CurOASysNum > 0) THEN
      UnitarySystem(UnitarySysNum)%DesignSuppHeatingCapacity = FinalSysSizing(CurSysNum)%HeatCap
    ELSE IF (CurZoneEqNum > 0) THEN
      UnitarySystem(UnitarySysNum)%DesignSuppHeatingCapacity = FinalZoneSizing(CurZoneEqNum)%DesHeatLoad
    ELSE IF (CurSysNum > 0) THEN

      ! set the supplemental heating capacity to the actual heating load
      UnitarySystem(UnitarySysNum)%DesignSuppHeatingCapacity = FinalSysSizing(CurSysNum)%HeatCap
      ! If reheat needed for humidity control, make sure supplemental heating is at least as big
      ! as the cooling capacity
      IF (UnitarySystem(UnitarySysNum)%Humidistat .AND. &
          UnitarySystem(UnitarySysNum)%DehumidControlType_Num == DehumidControl_CoolReheat) THEN
        UnitarySystem(UnitarySysNum)%DesignSuppHeatingCapacity = MAX(UnitarySystem(UnitarySysNum)%DesignSuppHeatingCapacity, &
                                                            UnitarySystem(UnitarySysNum)%DesignCoolingCapacity)
        IF (UnitarySystem(UnitarySysNum)%DesignSuppHeatingCapacity < SmallLoad) THEN
          UnitarySystem(UnitarySysNum)%DesignSuppHeatingCapacity = 0.0d0
        END IF
      END IF

    END IF

    CALL ReportSizingOutput(UnitarySystem(UnitarySysNum)%UnitarySystemType, UnitarySystem(UnitarySysNum)%Name, &
                            'Supplemental Heating Coil Nominal Capacity [W]', &
                            UnitarySystem(UnitarySysNum)%DesignSuppHeatingCapacity)

  END IF

  SuppHeatCap = UnitarySystem(UnitarySysNum)%DesignSuppHeatingCapacity

  IF (UnitarySystem(UnitarySysNum)%HeatRecActive) THEN
    CALL RegisterPlantCompDesignFlow(UnitarySystem(UnitarySysNum)%HeatRecoveryInletNodeNum, &
                              UnitarySystem(UnitarySysNum)%DesignHRWaterVolumeFlow )
  END IF

  IF(CurOASysNum .EQ. 0 .AND. CurZoneEqNum .EQ. 0)THEN
    BranchFlow = 0.0d0
    SystemType = cFurnaceTypes(UnitarySystem(UnitarySysNum)%UnitarySystemType_Num)
    ErrFound  = .FALSE.
    ! check branch flow rate vs system flow rate. Branch must match system of OA system is present
    CALL CheckSystemBranchFlow(TRIM(SystemType),UnitarySystem(UnitarySysNum)%Name, &
                               BranchFlow,UnitarySystem(UnitarySysNum)%DesignFanVolFlowRate,ErrFound)
    IF(ErrFound)CALL ShowContinueError('...occurs in '//TRIM(SystemType)//' "'//TRIM(UnitarySystem(UnitarySysNum)%Name))
    IF(UnitarySystem(UnitarySysNum)%DesignFanVolFlowRate .LE. 0.0d0)THEN
      IF(BranchFlow .NE. Autosize)THEN
        UnitarySystem(UnitarySysNum)%DesignFanVolFlowRate = BranchFlow
      ELSE
        UnitarySystem(UnitarySysNum)%DesignFanVolFlowRate = MAX(UnitarySystem(UnitarySysNum)%MaxCoolAirVolFlow, &
                                                                UnitarySystem(UnitarySysNum)%MaxHeatAirVolFlow)
      END IF
      UnitarySystem(UnitarySysNum)%DesignMassFlowRate = UnitarySystem(UnitarySysNum)%DesignFanVolFlowRate * StdRhoAir
    END IF
  END IF

  CoolingLoad = TempCoolingLoad
  HeatingLoad = TempHeatingLoad

  RETURN

END SUBROUTINE SizeUnitarySystem


! Get Input Section of the Module
!******************************************************************************
SUBROUTINE GetUnitarySystemInput

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Raustad
          !       DATE WRITTEN   February 2013
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Obtains input data for system and stores it in System data structures

          ! METHODOLOGY EMPLOYED:
          ! Uses "Get" routines to read in data.

          ! REFERENCES:

          ! USE STATEMENTS:
    USE InputProcessor
    USE NodeInputManager,           ONLY: GetOnlySingleNode
    USE DataHeatBalance,            ONLY: Zone
    USE BranchNodeConnections,      ONLY: SetUpCompSets, TestCompSet
    USE HVACHXAssistedCoolingCoil,  ONLY: GetHXDXCoilName
    USE DataIPShortCuts
    USE General,                    ONLY: TrimSigDigits
    USE DataHVACControllers,        ONLY: ControllerTypes, ControllerSimple_Type
    USE DXCoils,                    ONLY: SetCoilSystemCoolingData, GetDXCoilAvailSchPtr,SetDXCoolingCoilData
    USE DataAirSystems,             ONLY: PrimaryAirSystem
    USE DataZoneControls,           ONLY: TempControlledZone, NumTempControlledZones, HumidityControlZone,   &
                                          NumHumidityControlZones, ComfortControlledZone, NumComfortControlledZones
    USE WaterToAirHeatPumpSimple,   ONLY: GetWtoAHPSimpleCoilCapacity=>GetCoilCapacity,  &
                                          GetWtoAHPSimpleCoilInletNode=>GetCoilInletNode, &
                                          GetWtoAHPSimpleCoilOutletNode=>GetCoilOutletNode, &
                                          GetWtoAHPSimpleCoilIndex=>GetCoilIndex, &
                                          SetSimpleWSHPData, GetWtoAHPSimpleCoilAirFlow=>GetCoilAirFlowRate
    USE VariableSpeedCoils,         ONLY: GetCoilCapacityVariableSpeed, &
                                          GetCoilInletNodeVariableSpeed, &
                                          GetCoilOutletNodeVariableSpeed, &
                                          GetCoilIndexVariableSpeed, &
                                          GetCoilAirFlowRateVariableSpeed,&
                                          SetVarSpeedCoilData, GetVSCoilCondenserInletNode, &
                                          GetVSCoilMinOATCompressor, GetVSCoilNumOfSpeeds
    USE WaterToAirHeatPump,         ONLY: GetWtoAHPCoilCapacity=>GetCoilCapacity, &
                                          GetWtoAHPCoilInletNode=>GetCoilInletNode, &
                                          GetWtoAHPCoilOutletNode=>GetCoilOutletNode,GetWtoAHPCoilIndex=>GetCoilIndex
    USE HeatingCoils,               ONLY: GetHeatingCoilCapacity=>GetCoilCapacity,GetHeatingCoilInletNode=>GetCoilInletNode, &
                                          GetHeatingCoilOutletNode=>GetCoilOutletNode, GetHeatingCoilIndex=>GetCoilIndex, &
                                          GetHeatingCoilTypeNum, GetHeatingCoilPLFCurveIndex, GetCoilAvailScheduleIndex
    USE DXCoils,                    ONLY: GetDXCoilCapacity=>GetCoilCapacity,GetMinOATDXCoilCompressor=>GetMinOATCompressor,  &
                                          GetDXCoilInletNode=>GetCoilInletNode, GetDXCoilOutletNode=>GetCoilOutletNode, &
                                          GetDXCoilCondenserInletNode=>GetCoilCondenserInletNode,GetDXCoilIndex, &
                                          GetDXCoilTypeNum=>GetCoilTypeNum,SetDXCoolingCoilData,GetDXCoilAirFlow,SetDXCoilTypeData
    USE HVACHXAssistedCoolingCoil,  ONLY: GetDXHXAsstdCoilCapacity=>GetCoilCapacity,GetDXHXAsstdCoilInletNode=>GetCoilInletNode, &
                                          GetDXHXAsstdCoilOutletNode=>GetCoilOutletNode,GetHXDXCoilName,GetHXDXCoilIndex, &
                                          GetHXAssistedCoilTypeNum=>GetCoilGroupTypeNum, GetActualDXCoilIndex, &
                                          GetWaterHXAssistedCoilCapacity=>GetCoilCapacity, GetHXCoilAirFlowRate,   &
                                          GetCoilObjectTypeNum
    USE WaterCoils,                 ONLY: GetCoilWaterInletNode, GetCoilWaterOutletNode, GetCoilMaxWaterFlowRate, &
                                          GetWaterCoilInletNode=>GetCoilInletNode,GetWaterCoilOutletNode=>GetCoilOutletNode, &
                                          GetWaterCoilAvailScheduleIndex, GetWaterCoilIndex
    USE SteamCoils,                 ONLY: GetSteamCoilAirInletNode=>GetCoilAirInletNode, GetSteamCoilIndex, &
                                          GetSteamCoilAirOutletNode=>GetCoilAirOutletNode, &
                                          GetSteamCoilSteamInletNode=>GetCoilSteamInletNode, &
                                          GetCoilMaxSteamFlowRate=>GetCoilMaxSteamFlowRate, ZoneLoadControl, &
                                          GetSteamCoilAvailScheduleIndex
    USE Fans,                       ONLY: GetFanDesignVolumeFlowRate,GetFanInletNode,GetFanOutletNode,GetFanIndex, &
                                          GetFanAvailSchPtr, GetFanType, GetFanIndex
    USE FluidProperties,            ONLY: GetSatDensityRefrig
    USE EMSManager,                 ONLY: ManageEMS
    USE SetPointManager,            ONLY: NodeHasSPMCtrlVarType, iCtrlVarType_Temp, iCtrlVarType_HumRat

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER :: RoutineName='GetUnitarySystemInput: ' ! include trailing blank space

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: Alphas         ! Alpha input items for object
    CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: cAlphaFields   ! Alpha field names
    CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: cNumericFields ! Numeric field names
    CHARACTER(len=MaxNameLength) :: CurrentModuleObject ! for ease in getting objects
    CHARACTER(len=MaxNameLength) :: CoolingCoilType     ! Used in mining function calls
    CHARACTER(len=MaxNameLength) :: CoolingCoilName     ! Used in mining function calls
    CHARACTER(len=MaxNameLength) :: HeatingCoilType     ! Used in mining function calls
    CHARACTER(len=MaxNameLength) :: HeatingCoilName     ! Used in mining function calls
    REAL(r64)                    :: HeatingSizingRatio  ! Used to size DX heating coil wrt DX cooling coil
    CHARACTER(len=MaxNameLength) :: SuppHeatCoilType    ! Used in mining function calls
    CHARACTER(len=MaxNameLength) :: SuppHeatCoilName    ! Used in mining function calls
    CHARACTER(len=MaxNameLength) :: FanType             ! Used in mining function calls
    CHARACTER(len=MaxNameLength) :: FanName             ! Used in mining function calls
    INTEGER                      :: FanIndex            ! Used in mining function calls
    LOGICAL, ALLOCATABLE, DIMENSION(:)   :: lAlphaBlanks      ! Logical array, alpha field input BLANK = .true.
    LOGICAL, ALLOCATABLE, DIMENSION(:)   :: lNumericBlanks    ! Logical array, numeric field input BLANK = .true.
    REAL(r64), ALLOCATABLE, DIMENSION(:) :: Numbers     ! Numeric input items for object
    REAL(r64)  :: FanVolFlowRate                        ! Fan Max Flow Rate from Fan object (for comparisons to validity)
    REAL(r64)  :: SteamDensity                          ! steam density
    REAL(r64)  :: TotalFloorAreaOnAirLoop               ! AirloopHVAC total floor area served
    LOGICAL    :: ErrorsFound = .FALSE.                 ! If errors detected in input
    LOGICAL    :: IsNotOK                               ! Flag to verify name
    LOGICAL    :: IsBlank                               ! Flag for blank name
    LOGICAL    :: AirNodeFound                          ! used in error checking
    LOGICAL    :: AirLoopFound                          ! used in error checking
    LOGICAL    :: ErrFlag                               ! Mining function error flag
    LOGICAL    :: PrintMessage                          ! flag to print or not print message
    LOGICAL    :: InletNodeNotControlled                ! True if using controller on water coil
    INTEGER    :: UnitarySysNum                         ! The Unitary System object currently loading
    INTEGER    :: DesignSpecNum                         ! The design Specification object (multispeed coils) currently loading
    INTEGER    :: NumAlphas                             ! The number of alpha arguments in each object
    INTEGER    :: NumNumbers                            ! The number of numeric arguments in each object
    INTEGER    :: IOStatus                              ! The status of the GetObjectItem call
    INTEGER    :: TotalArgs=0                           ! Total number of alpha and numeric arguments (max) for a
                                                        ! certain object in the input file
    INTEGER    :: FanInletNode                          ! Used for node checking warning messages
    INTEGER    :: FanOutletNode                         ! Used for node checking warning messages
    INTEGER    :: CoolingCoilInletNode                  ! Used for node checking warning messages
    INTEGER    :: CoolingCoilOutletNode                 ! Used for node checking warning messages
    INTEGER    :: HeatingCoilInletNode                  ! Used for node checking warning messages
    INTEGER    :: HeatingCoilOutletNode                 ! Used for node checking warning messages
    INTEGER    :: SupHeatCoilInletNode                  ! Used for node checking warning messages
    INTEGER    :: SupHeatCoilOutletNode                 ! Used for node checking warning messages
    INTEGER    :: TotalZonesOnAirLoop                   ! number of zones connected to air loop
    LOGICAL    :: ZoneEquipmentFound                    ! TRUE if Unitary System found connected to zone exhaust node
    INTEGER    :: ActualCoolCoilType                    ! Coil type number for HX assisted coils
    INTEGER    :: ControlledZoneNum                     ! loop counter
    INTEGER    :: ZoneExhNum                            ! loop counter
    INTEGER    :: EquipNum                              ! loop counter
    INTEGER    :: ZoneInletNum                          ! loop counter
    INTEGER    :: AirLoopNumber                         ! loop counter
    INTEGER    :: BranchNum                             ! loop counter
    INTEGER    :: CompNum                               ! loop counter
    INTEGER    :: OACompNum                             ! loop counter
    INTEGER    :: TstatZoneNum                          ! loop counter
    INTEGER    :: HeatingCoilPLFCurveIndex              ! index of heating coil PLF curve
    INTEGER    :: SteamIndex                            ! steam index
    INTEGER    :: DXCoilIndex                           ! index to DX coil
    INTEGER    :: HSTATZoneNum                          ! zone index where humidistat is located
    INTEGER    :: TempAlphas                            ! temp alpha array
    INTEGER    :: TempNumbers                           ! temp number array
    INTEGER    :: Index                                 ! index to multispeed coil speed data
    INTEGER    :: SpeedNum                              ! multispeed coil speed
    INTEGER    :: NumOfSpeedCooling                     ! number of cooling speeds for multispeed coil
    INTEGER    :: NumOfSpeedHeating                     ! number of heating speeds for multispeed coil
    INTEGER    :: StartingSpeedRatioInput               ! field where speed ratio inputs start
    INTEGER    :: MaxSpeedNum                           ! maximum number of numeric inputs in design specification object
    INTEGER    :: OASysNum                              ! loop counter index to outside air system
    INTEGER    :: AirLoopNum                            ! loop counter index to primary air system
    CHARACTER(len=MaxNameLength)   :: HXCoilName        ! Cooling coil name within HX assembly

    ! local integer representation of input field numbers (i.e., alpha=A1 or numeric=N1)
    INTEGER    :: iNameAlphaNum                         ! get input index to unitary system name
    INTEGER    :: iControlTypeAlphaNum                  ! get input index to unitary system control type
    INTEGER    :: iControlZoneAlphaNum                  ! get input index to unitary system control zone
    INTEGER    :: iDehumidControlAlphaNum               ! get input index to unitary system dehumidification control
    INTEGER    :: iSysAvailSchedAlphaNum                ! get input index to unitary system avail schedule
    INTEGER    :: iFanTypeAlphaNum                      ! get input index to unitary system fan type
    INTEGER    :: iFanNameAlphaNum                      ! get input index to unitary system fan name
    INTEGER    :: iFanPlaceAlphaNum                     ! get input index to unitary system fan placement
    INTEGER    :: iFanSchedAlphaNum                     ! get input index to unitary system fan mode op schedule
    INTEGER    :: iHeatingCoilTypeAlphaNum              ! get input index to unitary system heating coil type
    INTEGER    :: iHeatingCoilNameAlphaNum              ! get input index to unitary system heating coil name
    INTEGER    :: iHeatingCoilSizeRatioNumericNum       ! get input index to unitary system heating coil sizing ratio
    INTEGER    :: iCoolingCoilTypeAlphaNum              ! get input index to unitary system cooling coil type
    INTEGER    :: iCoolingCoilNameAlphaNum              ! get input index to unitary system cooling coil name
    INTEGER    :: iDOASDXCoilAlphaNum                   ! get input index to unitary system DX coil DOAS specified
    INTEGER    :: iRunOnLatentLoadAlphaNum              ! get input index to unitary system run on latent load
    INTEGER    :: iSuppHeatCoilTypeAlphaNum             ! get input index to unitary system supp heat coil type
    INTEGER    :: iSuppHeatCoilNameAlphaNum             ! get input index to unitary system supp heat coil name
    INTEGER    :: iHeatSAFMAlphaNum                     ! get input index to unitary system heat supp air flow method
    INTEGER    :: iCoolSAFMAlphaNum                     ! get input index to unitary system cool supp air flow method
    INTEGER    :: iMaxCoolAirVolFlowNumericNum          ! get input index to unitary system cool supply air flow
    INTEGER    :: iMaxHeatAirVolFlowNumericNum          ! get input index to unitary system heat supply air flow
    INTEGER    :: iNoCoolHeatSAFMAlphaNum               ! get input index to unitary system no cool/heat supply air flow
    INTEGER    :: iMaxNoCoolHeatAirVolFlowNumericNum    ! get input index to unitary system no cool/heat supply air flow
    INTEGER    :: iDesignSpecMSHPTypeAlphaNum           ! get input index to unitary system MSHP coil type
    INTEGER    :: iDesignSpecMSHPNameAlphaNum           ! get input index to unitary system MSHP coil name
    INTEGER    :: iAirInletNodeNameAlphaNum             ! get input index to unitary system air inlet node
    INTEGER    :: iAirOutletNodeNameAlphaNum            ! get input index to unitary system air outlet node
    INTEGER    :: iDOASDXMinTempNumericNum              ! get input index to unitary system DOAS DX coil min outlet temp
    INTEGER    :: iCoolFlowPerFloorAreaNumericNum       ! get input index to unitary system cool flow per floor area
    INTEGER    :: iCoolFlowPerFracCoolNumericNum        ! get input index to unitary system cool flow per fraction cool
    INTEGER    :: iCoolFlowPerCoolCapNumericNum         ! get input index to unitary system cool flow per cooling cap
    INTEGER    :: iHeatFlowPerFloorAreaNumericNum       ! get input index to unitary system heat flow per floor area
    INTEGER    :: iHeatFlowPerFracCoolNumericNum        ! get input index to unitary system heat flow per fraction heat
    INTEGER    :: iHeatFlowPerHeatCapNumericNum         ! get input index to unitary system heat flow per heating cap
    INTEGER    :: iNoCoolHeatFlowPerFloorAreaNumericNum ! get input index to unitary system no cool/heat FPA
    INTEGER    :: iNoCoolHeatFlowPerFracCoolNumericNum  ! get input index to unitary system no cool/heat FPFC
    INTEGER    :: iNoCoolHeatFlowPerFracHeatNumericNum  ! get input index to unitary system no cool/heat FPFH
    INTEGER    :: iNoCoolHeatFlowPerCoolCapNumericNum   ! get input index to unitary system no cool/heat FPCC
    INTEGER    :: iNoCoolHeatFlowPerHeatCapNumericNum   ! get input index to unitary system no cool/heat FPHC
    INTEGER    :: iDesignMaxOutletTempNumericNum        ! get input index to unitary system design max outlet temp
    INTEGER    :: iMaxOATSuppHeatNumericNum             ! get input index to unitary system maximum OAT for supp operation
    INTEGER    :: iCondenserNodeAlphaNum                ! get input index to unitary system condenser node
    INTEGER    :: iMaxONOFFCycPerHourNumericNum         ! get input index to unitary system WSHP max cycle per hour
    INTEGER    :: iHPTimeConstantNumericNum             ! get input index to unitary system WSHP time constant
    INTEGER    :: iOnCyclePowerFracNumericNum           ! get input index to unitary system WSHP on cycle power
    INTEGER    :: iFanDelayTimeNumericNum               ! get input index to unitary system WSHP off cycle power
    INTEGER    :: iAncillaryOnPowerNumericNum           ! get input index to unitary system ancillary on power
    INTEGER    :: iAncillaryOffPowerNumericNum          ! get input index to unitary system ancillary off power
    INTEGER    :: iDesignHRWaterVolFlowNumericNum       ! get input index to unitary system design HR water flow
    INTEGER    :: iMaxHROutletWaterTempNumericNum       ! get input index to unitary system max HR outlet temp
    INTEGER    :: iHRWaterInletNodeAlphaNum             ! get input index to unitary system HR water inlet node
    INTEGER    :: iHRWaterOutletNodeAlphaNum            ! get input index to unitary system HR water outlet node

    CurrentModuleObject='AirloopHVAC:UnitarySystem'
    NumUnitarySystem = GetNumObjectsFound(TRIM(CurrentModuleObject))

    ALLOCATE(UnitarySystem(NumUnitarySystem))
    ALLOCATE(CheckEquipName(NumUnitarySystem))
    ALLOCATE(MultiOrVarSpeedHeatCoil(NumUnitarySystem))
    ALLOCATE(MultiOrVarSpeedCoolCoil(NumUnitarySystem))
    CheckEquipName = .TRUE.
    MultiOrVarSpeedHeatCoil = .FALSE.
    MultiOrVarSpeedCoolCoil = .FALSE.


    CALL GetObjectDefMaxArgs(TRIM(CurrentModuleObject),TotalArgs,NumAlphas,NumNumbers)
    TempAlphas = NumAlphas
    TempNumbers = NumNumbers

    CurrentModuleObject='UnitarySystemPerformance:HeatPump:Multispeed'
    NumDesignSpecMultiSpeedHP = GetNumObjectsFound(TRIM(CurrentModuleObject))
    CALL GetObjectDefMaxArgs(TRIM(CurrentModuleObject),TotalArgs,NumAlphas,NumNumbers)

    NumAlphas = MAX(TempAlphas, NumAlphas)
    NumNumbers = MAX(TempNumbers, NumNumbers)

    ALLOCATE(Alphas(NumAlphas))
    Alphas=' '
    ALLOCATE(cAlphaFields(NumAlphas))
    cAlphaFields=' '
    ALLOCATE(cNumericFields(NumNumbers))
    cNumericFields=' '
    ALLOCATE(Numbers(NumNumbers))
    Numbers=0.0d0
    ALLOCATE(lAlphaBlanks(NumAlphas))
    lAlphaBlanks=.TRUE.
    ALLOCATE(lNumericBlanks(NumNumbers))
    lNumericBlanks=.TRUE.

      ! Get the data for the DesignSpecification object
      CurrentModuleObject='UnitarySystemPerformance:HeatPump:Multispeed'
      ALLOCATE(DesignSpecMSHP(NumDesignSpecMultiSpeedHP))

      DO DesignSpecNum = 1, NumDesignSpecMultiSpeedHP

        CALL GetObjectItem(TRIM(CurrentModuleObject),DesignSpecNum,Alphas,NumAlphas,Numbers,NumNumbers,IOStatus, &
                           NumBlank=lNumericBlanks,AlphaBlank=lAlphaBlanks, &
                           AlphaFieldNames=cAlphaFields,NumericFieldNames=cNumericFields)

        IsNotOK=.FALSE.
        IsBlank=.FALSE.
        CALL VerifyName(Alphas(1),DesignSpecMSHP%Name,DesignSpecNum-1,IsNotOK,IsBlank,TRIM(CurrentModuleObject)//' Name')
        IF (IsNotOK) THEN
          ErrorsFound=.TRUE.
          IF (IsBlank) Alphas(1)='xxxxx'
        END IF

        DesignSpecMSHP(DesignSpecNum)%Name = Alphas(1)

        DesignSpecMSHP(DesignSpecNum)%NumOfSpeedHeating = Numbers(1)
        NumOfSpeedHeating = DesignSpecMSHP(DesignSpecNum)%NumOfSpeedHeating

        DesignSpecMSHP(DesignSpecNum)%NumOfSpeedCooling = Numbers(2)
        NumOfSpeedCooling = DesignSpecMSHP(DesignSpecNum)%NumOfSpeedCooling

        ! initialize number of speeds (MIN = 4 or greater of cooling and heating speeds x 2)
        MaxSpeedNum = 0 ! set to 0 so we know if the inputs are valid

        IF(MOD(REAL(NumNumbers-2,r64),2.0d0) .GT. 0.0d0)THEN
          ! check that the number of numeric speed input fields match the number of speeds
          CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(DesignSpecMSHP(DesignSpecNum)%Name))
          CALL ShowContinueError('Illegal number of entries in Supply Air Flow Rate fields.')
          CALL ShowContinueError('..number of required Supply Air Flow Rate fields = '// &
               TRIM(TrimSigDigits(2*(MAX(NumOfSpeedCooling,NumOfSpeedHeating)),0)))
          CALL ShowContinueError('..number of actual Supply Air Flow Rate fields   = '//TRIM(TrimSigDigits((NumNumbers-2),0)))
          ErrorsFound=.TRUE.
        ELSE
          MaxSpeedNum = (NumNumbers-2)/2 ! Extensible fields included (>4) for cooling and heating
        END IF

        IF(MaxSpeedNum .LT. MAX(NumOfSpeedHeating,NumOfSpeedCooling))THEN
          CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(DesignSpecMSHP(DesignSpecNum)%Name))
          CALL ShowContinueError('Illegal number of entries in Supply Air Flow Rate fields.')
          CALL ShowContinueError('..number of required Supply Air Flow Rate fields = '// &
               TRIM(TrimSigDigits(2*(MAX(NumOfSpeedCooling,NumOfSpeedHeating)),0)))
          CALL ShowContinueError('..number of actual Supply Air Flow Rate fields   = '//TRIM(TrimSigDigits((NumNumbers-2),0)))
          ErrorsFound=.TRUE.
        END IF

        StartingSpeedRatioInput = 3 ! start the index counter at the first input for heating (e.g. 3+0*2)
        IF(NumOfSpeedHeating .GT. 0 .AND. MaxSpeedNum > 0) THEN
          ALLOCATE(DesignSpecMSHP(DesignSpecNum)%HeatingVolFlowRatio(MaxSpeedNum))
          DO SpeedNum = 1, NumOfSpeedHeating
            DesignSpecMSHP(DesignSpecNum)%HeatingVolFlowRatio(SpeedNum) = Numbers(StartingSpeedRatioInput+((SpeedNum-1)*2))
          END DO
        END IF

        StartingSpeedRatioInput = 4 ! start the index counter at the first input for heating (e.g. 4+0*2)
        IF(NumOfSpeedCooling .GT. 0 .AND. MaxSpeedNum > 0) THEN
          ALLOCATE(DesignSpecMSHP(DesignSpecNum)%CoolingVolFlowRatio(MaxSpeedNum))
          DO SpeedNum = 1, NumOfSpeedCooling
            DesignSpecMSHP(DesignSpecNum)%CoolingVolFlowRatio(SpeedNum) = Numbers(StartingSpeedRatioInput+((SpeedNum-1)*2))
          END DO
        END IF

      END DO

      ! AirLoopHVAC:UnitarySystem,
      iNameAlphaNum                   = 1  ! A1,  \field Name - \required-field
      iControlTypeAlphaNum            = 2  ! A2,  \field Control Type
      iControlZoneAlphaNum            = 3  ! A3,  \field Controlling Zone or Thermostat Location
      iDehumidControlAlphaNum         = 4  ! A4,  \field dehumidification Control Type
      iSysAvailSchedAlphaNum          = 5  ! A5,  \field Availability Schedule Name
      iAirInletNodeNameAlphaNum       = 6  ! A6,  \field Air Inlet Node Name - \required-field
      iAirOutletNodeNameAlphaNum      = 7  ! A7,  \field Air Outlet Node Name - \required-field
      iFanTypeAlphaNum                = 8  ! A8,  \field Supply Fan Object Type
      iFanNameAlphaNum                = 9  ! A9,  \field Supply Fan Name
      iFanPlaceAlphaNum               = 10 ! A10, \field Fan Placement
      iFanSchedAlphaNum               = 11 ! A11, \field Supply Air Fan Operating Mode Schedule Name
      iHeatingCoilTypeAlphaNum        = 12 ! A12, \field Heating Coil Object Type
      iHeatingCoilNameAlphaNum        = 13 ! A13, \field Heating Coil Name
      iHeatingCoilSizeRatioNumericNum = 1  ! N1,  \field DX Heating Coil Sizing Ratio
      iCoolingCoilTypeAlphaNum        = 14 ! A14, \field Cooling Coil Object Type
      iCoolingCoilNameAlphaNum        = 15 ! A15, \field Cooling Coil Name
      iDOASDXCoilAlphaNum             = 16 ! A16, \field Use DOAS DX Cooling Coil
      iDOASDXMinTempNumericNum        = 2  ! N2 , \field DOAS DX Cooling Coil Leaving Minimum Air Temperature
      iRunOnLatentLoadAlphaNum        = 17 ! A17, \field Run on Latent Load
      iSuppHeatCoilTypeAlphaNum       = 18 ! A18, \field Supplemental Heating Coil Object Type
      iSuppHeatCoilNameAlphaNum       = 19 ! A19, \field Supplemental Heating Coil Name
      iCoolSAFMAlphaNum               = 20 ! A20, \field Supply air Flow Rate Method During Cooling Operation
      iMaxCoolAirVolFlowNumericNum    = 3  ! N3,  \field Supply Air Flow Rate During Cooling Operation
      iCoolFlowPerFloorAreaNumericNum = 4  ! N4,  \field Supply Air Flow Rate Per Floor Area During Cooling Operation
      iCoolFlowPerFracCoolNumericNum  = 5  ! N5,  \field Fraction of Autosized Design Cooling Supply Air Flow Rate
      iCoolFlowPerCoolCapNumericNum   = 6  ! N6,  \field Design Supply Air Flow Rate Per Unit of Capacity During Cooling Operation
      iHeatSAFMAlphaNum               = 21 ! A21, \field Supply air Flow Rate Method During Heating Operation
      iMaxHeatAirVolFlowNumericNum    = 7  ! N7,  \field Supply Air Flow Rate During Heating Operation
      iHeatFlowPerFloorAreaNumericNum = 8  ! N8,  \field Supply Air Flow Rate Per Floor Area during Heating Operation
      iHeatFlowPerFracCoolNumericNum  = 9  ! N9,  \field Fraction of Autosized Design Heating Supply Air Flow Rate
      iHeatFlowPerHeatCapNumericNum   = 10 ! N10, \field Design Supply Air Flow Rate Per Unit of Capacity During Heating Operation
      iNoCoolHeatSAFMAlphaNum         = 22 ! A22, \field Supply air Flow Rate Method When No Cooling or Heating is Required
      iMaxNoCoolHeatAirVolFlowNumericNum = 11 ! N11, \field Supply Air Flow Rate When No Cooling or Heating is Required
      iNoCoolHeatFlowPerFloorAreaNumericNum = 12 ! N12, \field Supply Air Flow Rate Per Floor Area When No Cooling/Heating is Req
      iNoCoolHeatFlowPerFracCoolNumericNum = 13 ! N13, \field Fraction of Autosized Design Cooling Supply Air Flow Rate
      iNoCoolHeatFlowPerFracHeatNumericNum = 14 ! N14, \field Fraction of Autosized Design Heating Supply Air Flow Rate
      iNoCoolHeatFlowPerCoolCapNumericNum = 15 ! N15, \field Design Supply Air Flow Rate Per Unit of Capacity During Cooling Op
      iNoCoolHeatFlowPerHeatCapNumericNum = 16 ! N16, \field Design Supply Air Flow Rate Per Unit of Capacity During Heating Op
      iDesignMaxOutletTempNumericNum  = 17 ! N17, \field Maximum Supply Air Temperature
      iMaxOATSuppHeatNumericNum       = 18 ! N18, \field Maximum Outdoor Dry-Bulb Temperature for Supplemental Heater Op
      iCondenserNodeAlphaNum          = 23 ! A23, \field Outdoor Dry-Bulb Temperature Sensor Node Name
      iMaxONOFFCycPerHourNumericNum   = 19 ! N19, \field Maximum Cycling Rate
      iHPTimeConstantNumericNum       = 20 ! N20, \field Heat Pump Time Constant
      iOnCyclePowerFracNumericNum     = 21 ! N21, \field Fraction of On-Cycle Power Use
      iFanDelayTimeNumericNum         = 22 ! N22, \field Heat Pump Fan Delay Time
      iAncillaryOnPowerNumericNum     = 23 ! N23, \field Ancilliary On-Cycle Electric Power
      iAncillaryOffPowerNumericNum    = 24 ! N24, \field Ancilliary Off-Cycle Electric Power
      iDesignHRWaterVolFlowNumericNum = 25 ! N25, \field Design Heat Recovery Water Flow Rate
      iMaxHROutletWaterTempNumericNum = 26 ! N26, \field Maximum Temperature for Heat Recovery
      iHRWaterInletNodeAlphaNum       = 24 ! A24, \field Heat Recovery Water Inlet Node Name
      iHRWaterOutletNodeAlphaNum      = 25 ! A25, \field Heat Recovery Water Outlet Node Name

      iDesignSpecMSHPTypeAlphaNum     = 26 ! A26, \field design Specification Multispeed Heat Pump Object Type
      iDesignSpecMSHPNameAlphaNum     = 27 ! A27; \field design Specification Multispeed Heat Pump Object Name


      ! Get the data for the Unitary System
      CurrentModuleObject='AirloopHVAC:UnitarySystem'
      DO UnitarySysNum = 1,  NumUnitarySystem

        FanInletNode          = 0
        FanOutletNode         = 0
        FanVolFlowRate        = 0.0d0
        CoolingCoilInletNode  = 0
        CoolingCoilOutletNode = 0
        HeatingCoilInletNode  = 0
        HeatingCoilOutletNode = 0
        SupHeatCoilInletNode  = 0
        SupHeatCoilOutletNode = 0

        CurrentModuleObject  = 'AirLoopHVAC:UnitarySystem'
        UnitarySystem(UnitarySysNum)%UnitarySystemType = CurrentModuleObject
        UnitarySystem(UnitarySysNum)%UnitarySystemType_Num = UnitarySystem_AnyCoilType

        CALL GetObjectItem(TRIM(CurrentModuleObject),UnitarySysNum,Alphas,NumAlphas,Numbers,NumNumbers,IOStatus, &
                           NumBlank=lNumericBlanks,AlphaBlank=lAlphaBlanks, &
                           AlphaFieldNames=cAlphaFields,NumericFieldNames=cNumericFields)

        IsNotOK=.FALSE.
        IsBlank=.FALSE.
        CALL VerifyName(Alphas(iNameAlphaNum),UnitarySystem%Name,UnitarySysNum-1, &
                        IsNotOK,IsBlank,TRIM(CurrentModuleObject)//' Name')
        IF (IsNotOK) THEN
          ErrorsFound=.TRUE.
          IF (IsBlank) Alphas(iNameAlphaNum)='xxxxx'
        END IF

        UnitarySystem(UnitarySysNum)%Name            = Alphas(iNameAlphaNum)

        IF(SameString(Alphas(iControlTypeAlphaNum),'Load'))THEN
          UnitarySystem(UnitarySysNum)%ControlType = LoadBased
        ELSEIF(SameString(Alphas(iControlTypeAlphaNum),'SetPoint'))THEN
          UnitarySystem(UnitarySysNum)%ControlType = SetPointBased
        ELSE
          CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
          CALL ShowContinueError('Illegal '//TRIM(cAlphaFields(iControlTypeAlphaNum))//' = '//TRIM(Alphas(iControlTypeAlphaNum)))
          ErrorsFound=.TRUE.
        END IF

         !Get the Controlling Zone or Location of the Thermostat
        UnitarySystem(UnitarySysNum)%ControlZoneNum = FindItemInList(Alphas(iControlZoneAlphaNum),Zone%Name,NumOfZones)

        IF(.NOT. lAlphaBlanks(iDehumidControlAlphaNum))THEN
          IF (SameString(Alphas(iDehumidControlAlphaNum),'None') .OR. &
              SameString(Alphas(iDehumidControlAlphaNum),'Multimode') .OR. &
              SameString(Alphas(iDehumidControlAlphaNum),'CoolReheat'))THEN
            AirNodeFound=.FALSE.
            IF(SameString(Alphas(iDehumidControlAlphaNum),'Multimode'))THEN
              UnitarySystem(UnitarySysNum)%DehumidControlType_Num = DehumidControl_Multimode
              UnitarySystem(UnitarySysNum)%Humidistat = .TRUE.
            END IF
            IF(SameString(Alphas(iDehumidControlAlphaNum),'CoolReheat'))THEN
              UnitarySystem(UnitarySysNum)%DehumidControlType_Num = DehumidControl_CoolReheat
              UnitarySystem(UnitarySysNum)%Humidistat = .TRUE.
              IF(lAlphaBlanks(iSuppHeatCoilNameAlphaNum))THEN
              END IF
            END IF
            IF(SameString(Alphas(iDehumidControlAlphaNum),'None'))THEN
              UnitarySystem(UnitarySysNum)%DehumidControlType_Num = DehumidControl_None
              UnitarySystem(UnitarySysNum)%Humidistat = .FALSE.
            END IF
            IF(UnitarySystem(UnitarySysNum)%Humidistat .AND. &
               UnitarySystem(UnitarySysNum)%ControlType == LoadBased)THEN
              DO HstatZoneNum = 1, NumHumidityControlZones
                IF(HumidityControlZone(HstatZoneNum)%ActualZoneNum .NE. UnitarySystem(UnitarySysNum)%ControlZoneNum)CYCLE
                AirNodeFound=.TRUE.
              END DO
              IF (.not. AirNodeFound) THEN
                CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
                CALL ShowContinueError('Did not find Air Node (Zone with Humidistat).')
                CALL ShowContinueError('specified '//TRIM(cAlphaFields(iControlZoneAlphaNum))//' = '// &
                                       TRIM(Alphas(iControlZoneAlphaNum)))
                ErrorsFound=.TRUE.
             END IF
            END IF
          ELSE ! invalid input
            CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
            CALL ShowContinueError('Illegal '//TRIM(cAlphaFields(iDehumidControlAlphaNum))//' = '// &
                                   TRIM(Alphas(iDehumidControlAlphaNum)))
            UnitarySystem(UnitarySysNum)%Humidistat = .FALSE.
            ErrorsFound=.TRUE.
          END IF
        END IF

        IF (lAlphaBlanks(iSysAvailSchedAlphaNum)) THEN
          UnitarySystem(UnitarySysNum)%SysAvailSchedPtr        = ScheduleAlwaysOn
        ELSE
          UnitarySystem(UnitarySysNum)%SysAvailSchedPtr        = GetScheduleIndex(Alphas(iSysAvailSchedAlphaNum))
          IF (UnitarySystem(UnitarySysNum)%SysAvailSchedPtr == 0) THEN
            CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
            CALL ShowContinueError('Illegal '//TRIM(cAlphaFields(iSysAvailSchedAlphaNum))//' = '// &
                                   TRIM(Alphas(iSysAvailSchedAlphaNum)))
            ErrorsFound=.TRUE.
          END IF
        END IF

        UnitarySystem(UnitarySysNum)%UnitarySystemInletNodeNum = &
            GetOnlySingleNode(Alphas(iAirInletNodeNameAlphaNum),ErrorsFound,CurrentModuleObject,Alphas(iNameAlphaNum), &
                     NodeType_Air,NodeConnectionType_Inlet,1,ObjectIsParent)
        UnitarySystem(UnitarySysNum)%UnitarySystemOutletNodeNum = &
            GetOnlySingleNode(Alphas(iAirOutletNodeNameAlphaNum),ErrorsFound,CurrentModuleObject,Alphas(iNameAlphaNum), &
                     NodeType_Air,NodeConnectionType_Outlet,1,ObjectIsParent)

         !Get fan data
        FanType = Alphas(iFanTypeAlphaNum)
        FanName = Alphas(iFanNameAlphaNum)

        IF(.NOT. lAlphaBlanks(iFanTypeAlphaNum))THEN
          IsNotOK=.FALSE.
          CALL GetFanType(FanName, UnitarySystem(UnitarySysNum)%FanType_Num, IsNotOK,   &
               CurrentModuleObject,Alphas(iNameAlphaNum))
          IF (IsNotOK) THEN
            ErrorsFound=.TRUE.
          END IF
          UnitarySystem(UnitarySysNum)%FanExists = .TRUE.
        END IF

        IF (UnitarySystem(UnitarySysNum)%FanType_Num == FanType_SimpleOnOff .OR. &
            UnitarySystem(UnitarySysNum)%FanType_Num == FanType_SimpleConstVolume .OR. &
            UnitarySystem(UnitarySysNum)%FanType_Num == FanType_SimpleVAV)THEN
          IsNotOK=.FALSE.
          CALL ValidateComponent(FanType,FanName,IsNotOK,TRIM(CurrentModuleObject))
          IF (IsNotOK) THEN
            CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
            ErrorsFound=.TRUE.

          ELSE ! mine data from fan object

            ! Get the fan index
            ErrFlag=.FALSE.
            CALL GetFanIndex(FanName, UnitarySystem(UnitarySysNum)%FanIndex, ErrFlag)
            IF (ErrFlag) THEN
              CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
              ErrorsFound=.TRUE.
            END IF

            ! Get the Design Fan Volume Flow Rate
            ErrFlag=.FALSE.
            FanVolFlowRate = GetFanDesignVolumeFlowRate(FanType,FanName,ErrFlag)
            IF(FanVolFlowRate == Autosize)UnitarySystem(UnitarySysNum)%RequestAutosize = .TRUE.
            UnitarySystem(UnitarySysNum)%ActualFanVolFlowRate = FanVolFlowRate
            UnitarySystem(UnitarySysNum)%DesignFanVolFlowRate = FanVolFlowRate
            IF (ErrFlag) THEN
              CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
              ErrorsFound=.TRUE.
            END IF

            ! Get the Fan Inlet Node
            ErrFlag=.FALSE.
            FanInletNode = GetFanInletNode(FanType,FanName,ErrFlag)
            IF (ErrFlag) THEN
              CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
              ErrorsFound=.TRUE.
            END IF

            ! Get the Fan Outlet Node
            ErrFlag=.FALSE.
            FanOutletNode = GetFanOutletNode(FanType,FanName,ErrFlag)
            IF (ErrFlag) THEN
              CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
              ErrorsFound=.TRUE.
            END IF

            ! Get the fan's availability schedule
            ErrFlag=.FALSE.
            UnitarySystem(UnitarySysNum)%FanAvailSchedPtr = GetFanAvailSchPtr(FanType,FanName,ErrFlag)
            IF (ErrFlag) THEN
              CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
              ErrorsFound=.TRUE.
            END IF

          END IF ! IF (IsNotOK) THEN

        ELSEIF(UnitarySystem(UnitarySysNum)%FanExists)THEN
          CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
          CALL ShowContinueError('Illegal '//TRIM(cAlphaFields(iFanTypeAlphaNum))//' = '//TRIM(Alphas(iFanTypeAlphaNum)))
          ErrorsFound=.TRUE.
        END IF ! IF (UnitarySystem(UnitarySysNum)%FanType_Num...

        ! Add fan to component sets array
        IF(UnitarySystem(UnitarySysNum)%FanExists)CALL SetUpCompSets(CurrentModuleObject, Alphas(iNameAlphaNum), &
                           Alphas(iFanTypeAlphaNum),Alphas(iFanNameAlphaNum),NodeID(FanInletNode),NodeID(FanOutletNode))

        IF (SameString(Alphas(iFanPlaceAlphaNum),'BlowThrough') )      &
               UnitarySystem(UnitarySysNum)%FanPlace = BlowThru
        IF (SameString(Alphas(iFanPlaceAlphaNum),'DrawThrough') )      &
              UnitarySystem(UnitarySysNum)%FanPlace = DrawThru
        IF (UnitarySystem(UnitarySysNum)%FanPlace .EQ.0 .AND. UnitarySystem(UnitarySysNum)%FanExists) THEN
          CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
          CALL ShowContinueError('Illegal '//TRIM(cAlphaFields(iFanPlaceAlphaNum))//' = '//TRIM(Alphas(iFanPlaceAlphaNum)))
          ErrorsFound = .TRUE.
        END IF

        UnitarySystem(UnitarySysNum)%FanOpModeSchedPtr     = GetScheduleIndex(Alphas(iFanSchedAlphaNum))
        IF (.NOT. lAlphaBlanks(iFanSchedAlphaNum) .AND. UnitarySystem(UnitarySysNum)%FanOpModeSchedPtr == 0) THEN
          CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
          CALL ShowContinueError('Illegal '//TRIM(cAlphaFields(iFanSchedAlphaNum))//' = '//TRIM(Alphas(iFanSchedAlphaNum)))
          ErrorsFound=.TRUE.
        ELSEIF (lAlphaBlanks(iFanSchedAlphaNum)) THEN
          IF(UnitarySystem(UnitarySysNum)%ControlType == SetPointBased)THEN
            ! Fan operating mode must be constant fan so that the coil outlet temp is proportional to PLR
            ! Cycling fan always outputs the full load outlet air temp so should not be used with set point based control
            UnitarySystem(UnitarySysNum)%FanOpMode = ContFanCycCoil
          ELSE
            UnitarySystem(UnitarySysNum)%FanOpMode = CycFanCycCoil
            IF(UnitarySystem(UnitarySysNum)%FanType_Num /= FanType_SimpleOnOff)THEN
              CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
              CALL ShowContinueError(TRIM(cAlphaFields(iFanTypeAlphaNum))//' = '//TRIM(Alphas(iFanTypeAlphaNum)))
              CALL ShowContinueError('Fan type must be Fan:OnOff when '//  &
                                     TRIM(cAlphaFields(iFanSchedAlphaNum))//' = Blank.')
              ErrorsFound=.TRUE.
            END IF
          END IF
        ELSE IF(.NOT. lAlphaBlanks(iFanSchedAlphaNum) .AND. UnitarySystem(UnitarySysNum)%FanOpModeSchedPtr .GT. 0 .AND. &
                UnitarySystem(UnitarySysNum)%ControlType == SetPointBased)THEN
          IF (.NOT. CheckScheduleValueMinMax(UnitarySystem(UnitarySysNum)%FanOpModeSchedPtr,'>',0.0d0,'<=',1.0d0)) THEN
            CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
            CALL ShowContinueError('For '//TRIM(cAlphaFields(iFanTypeAlphaNum))//' = '//TRIM(Alphas(iFanTypeAlphaNum)))
            CALL ShowContinueError('Fan operating mode must be continuous (fan operating mode schedule values > 0).')
            CALL ShowContinueError('Error found in '//TRIM(cAlphaFields(iFanSchedAlphaNum))//' = '//TRIM(Alphas(iFanSchedAlphaNum)))
            CALL ShowContinueError('...schedule values must be (>0., <=1.)')
            ErrorsFound=.TRUE.
          END IF
        END IF

        ! Check fan's schedule for cycling fan operation IF constant volume fan is used
        IF(UnitarySystem(UnitarySysNum)%FanOpModeSchedPtr .GT. 0 .AND. &
           UnitarySystem(UnitarySysNum)%FanType_Num == FanType_SimpleConstVolume)THEN
          IF (.NOT. CheckScheduleValueMinMax(UnitarySystem(UnitarySysNum)%FanOpModeSchedPtr,'>',0.0d0,'<=',1.0d0)) THEN
            CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
            CALL ShowContinueError('For '//TRIM(cAlphaFields(iFanTypeAlphaNum))//' = '//TRIM(Alphas(iFanTypeAlphaNum)))
            CALL ShowContinueError('Fan operating mode must be continuous (fan operating mode schedule values > 0).')
            CALL ShowContinueError('Error found in '//TRIM(cAlphaFields(iFanSchedAlphaNum))//' = '// &
                                   TRIM(Alphas(iFanSchedAlphaNum)))
            CALL ShowContinueError('...schedule values must be (>0., <=1.)')
            ErrorsFound=.TRUE.
          END IF
        END IF

            !Get coil data
        HeatingCoilType = Alphas(iHeatingCoilTypeAlphaNum)
        HeatingCoilName = Alphas(iHeatingCoilNameAlphaNum)
        IF(NumNumbers .GT. (iHeatingCoilSizeRatioNumericNum-1))THEN
          HeatingSizingRatio = Numbers(iHeatingCoilSizeRatioNumericNum)
          UnitarySystem(UnitarySysNum)%HeatingSizingRatio = HeatingSizingRatio
        ELSE
          HeatingSizingRatio = 1.0d0
        END IF
        HeatingCoilPLFCurveIndex = 0
        UnitarySystem(UnitarySysNum)%HeatingCoilName = HeatingCoilName
        IF(.NOT. lAlphaBlanks(iHeatingCoilTypeAlphaNum))THEN
          UnitarySystem(UnitarySysNum)%HeatCoilExists = .TRUE.
          PrintMessage = .FALSE.
        END IF

        IF (SameString(HeatingCoilType, 'Coil:Heating:DX:VariableSpeed') )THEN
          UnitarySystem(UnitarySysNum)%HeatingCoilType_Num = Coil_HeatingAirToAirVariableSpeed
        ELSEIF (SameString(HeatingCoilType,'Coil:Heating:DX:MultiSpeed')) THEN
          UnitarySystem(UnitarySysNum)%HeatingCoilType_Num = CoilDX_MultiSpeedHeating
        ELSEIF (SameString(HeatingCoilType,'Coil:Heating:Water')) THEN
          UnitarySystem(UnitarySysNum)%HeatingCoilType_Num = Coil_HeatingWater
        ELSEIF (SameString(HeatingCoilType,'Coil:Heating:Steam')) THEN
          UnitarySystem(UnitarySysNum)%HeatingCoilType_Num = Coil_HeatingSteam
        ELSEIF (SameString(HeatingCoilType,'Coil:Heating:WaterToAirHeatPump:EquationFit')) THEN
            UnitarySystem(UnitarySysNum)%HeatingCoilType_Num = Coil_HeatingWaterToAirHPSimple
        ELSEIF (SameString(HeatingCoilType,'Coil:Heating:WaterToAirHeatPump:ParameterEstimation')) THEN
            UnitarySystem(UnitarySysNum)%HeatingCoilType_Num = Coil_HeatingWaterToAirHP
        ELSEIF (SameString(HeatingCoilType,'Coil:Heating:WaterToAirHeatPump:VariableSpeedEquationFit')) THEN
            UnitarySystem(UnitarySysNum)%HeatingCoilType_Num = Coil_HeatingWaterToAirHPVSEquationFit
        ELSEIF (SameString(HeatingCoilType,'Coil:Heating:Electric:MultiStage')) THEN
            UnitarySystem(UnitarySysNum)%HeatingCoilType_Num = Coil_HeatingElectric_MultiStage
        ELSEIF (SameString(HeatingCoilType,'Coil:Heating:Gas:MultiStage')) THEN
            UnitarySystem(UnitarySysNum)%HeatingCoilType_Num = Coil_HeatingGas_MultiStage
        ELSEIF (SameString(HeatingCoilType,'Coil:Heating:Gas')          .OR. &
                SameString(HeatingCoilType,'Coil:Heating:Electric')     .OR. &
                SameString(HeatingCoilType,'Coil:Heating:Desuperheater')) THEN
            UnitarySystem(UnitarySysNum)%HeatingCoilType_Num = GetHeatingCoilTypeNum(HeatingCoilType,HeatingCoilName,ErrFlag)
        ELSE IF(UnitarySystem(UnitarySysNum)%HeatCoilExists)THEN
          UnitarySystem(UnitarySysNum)%HeatingCoilType_Num = &
                 GetDXCoilTypeNum(HeatingCoilType,HeatingCoilName,ErrFlag,PrintMessage)
        END IF

        IF (UnitarySystem(UnitarySysNum)%HeatingCoilType_Num == CoilDX_HeatingEmpirical)  THEN

          UnitarySystem(UnitarySysNum)%DXHeatingCoil = .TRUE.
          ErrFlag = .FALSE.

          CALL ValidateComponent(HeatingCoilType,HeatingCoilName,IsNotOK,  &
                                 TRIM(CurrentModuleObject))
          IF (IsNotOK) THEN
            CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
            ErrorsFound=.TRUE.

          ELSE ! mine data from DX heating coil

            ! Get DX heating coil index
            CALL GetDXCoilIndex(HeatingCoilName,UnitarySystem(UnitarySysNum)%HeatingCoilIndex,ErrFlag)
            IF(ErrFlag)THEN
              CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
              ErrorsFound=.TRUE.
              ErrFlag = .FALSE.
            END IF

            UnitarySystem(UnitarySysNum)%HeatingCoilAvailSchPtr = GetDXCoilAvailSchPtr(HeatingCoilType,HeatingCoilName,ErrFlag)
            IF (ErrFlag) THEN
              CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
              ErrorsFound=.TRUE.
              ErrFlag = .FALSE.
            END IF

            ! Get DX heating coil capacity
            UnitarySystem(UnitarySysNum)%DesignHeatingCapacity =    &
               GetDXCoilCapacity(HeatingCoilType,HeatingCoilName,ErrFlag)
            IF(UnitarySystem(UnitarySysNum)%DesignHeatingCapacity == Autosize) &
                 UnitarySystem(UnitarySysNum)%RequestAutosize = .TRUE.
            IF (ErrFlag) THEN
              CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
              ErrorsFound=.TRUE.
              ErrFlag = .FALSE.
            END IF

            ! Get DX coil air flow rate.
            UnitarySystem(UnitarySysNum)%MaxHeatAirVolFlow   = GetDXCoilAirFlow(HeatingCoilType,HeatingCoilName,ErrFlag)
            IF(UnitarySystem(UnitarySysNum)%MaxHeatAirVolFlow == Autosize) &
                 UnitarySystem(UnitarySysNum)%RequestAutosize = .TRUE.
            ! Get the Heating Coil Nodes
            HeatingCoilInletNode = GetDXCoilInletNode(HeatingCoilType,HeatingCoilName,ErrFlag)
            HeatingCoilOutletNode = GetDXCoilOutletNode(HeatingCoilType,HeatingCoilName,ErrFlag)
            IF (ErrFlag) THEN
              CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
              ErrorsFound=.TRUE.
              ErrFlag = .FALSE.
            END IF

            CALL SetDXCoolingCoilData(UnitarySystem(UnitarySysNum)%HeatingCoilIndex,ErrorsFound, &
                                      HeatSizeRatio=HeatingSizingRatio)
          END IF

        ELSEIF(UnitarySystem(UnitarySysNum)%HeatingCoilType_Num == Coil_HeatingAirToAirVariableSpeed .OR. &
               UnitarySystem(UnitarySysNum)%HeatingCoilType_Num == Coil_HeatingWaterToAirHPVSEquationFit) THEN

          UnitarySystem(UnitarySysNum)%DXHeatingCoil = .TRUE.

          CALL ValidateComponent(HeatingCoilType,HeatingCoilName,IsNotOK,  &
                                 TRIM(CurrentModuleObject))
          IF (IsNotOK) THEN
            CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
            ErrorsFound=.TRUE.
          ELSE

            UnitarySystem(UnitarySysNum)%HeatingCoilIndex = GetCoilIndexVariableSpeed(HeatingCoilType, HeatingCoilName,ErrFlag)
            IF (ErrFlag) THEN
              CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
              ErrorsFound=.TRUE.
              ErrFlag = .FALSE.
            END IF

            UnitarySystem(UnitarySysNum)%NumOfSpeedHeating = GetVSCoilNumOfSpeeds(HeatingCoilName,ErrFlag)

            UnitarySystem(UnitarySysNum)%HeatingCoilAvailSchPtr = ScheduleAlwaysOn

            HeatingCoilInletNode=GetCoilInletNodeVariableSpeed(HeatingCoilType, HeatingCoilName,ErrFlag)
            IF (ErrFlag) THEN
              CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
              ErrorsFound=.TRUE.
              ErrFlag = .FALSE.
            END IF

            HeatingCoilOutletNode=GetCoilOutletNodeVariableSpeed(HeatingCoilType, HeatingCoilName,ErrFlag)
            IF (ErrFlag) THEN
              CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
              ErrorsFound=.TRUE.
              ErrFlag = .FALSE.
            END IF

          END IF
        ELSEIF (UnitarySystem(UnitarySysNum)%HeatingCoilType_Num == CoilDX_MultiSpeedHeating) THEN
          UnitarySystem(UnitarySysNum)%DXHeatingCoil = .TRUE.
          ErrFlag = .FALSE.
          CALL GetDXCoilIndex(HeatingCoilName,UnitarySystem(UnitarySysNum)%HeatingCoilIndex, &
                              ErrFlag, TRIM(HeatingCoilType))
          IF(ErrFlag) THEN
            CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
            ErrorsFound=.true.
            ErrFlag = .FALSE.
          END IF

          UnitarySystem(UnitarySysNum)%HeatingCoilAvailSchPtr = GetDXCoilAvailSchPtr(HeatingCoilType,HeatingCoilName,ErrFlag)

          HeatingCoilInletNode = GetDXCoilInletNode(HeatingCoilType,HeatingCoilName,ErrFlag)
          IF(ErrFlag)THEN
            CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
            ErrorsFound = .TRUE.
            ErrFlag = .FALSE.
          END IF
          HeatingCoilOutletNode = GetDXCoilOutletNode(HeatingCoilType,HeatingCoilName,ErrFlag)
          IF(ErrFlag)THEN
            CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
            ErrorsFound = .TRUE.
            ErrFlag = .FALSE.
          END IF

!            IF(UnitarySystem(UnitarySysNum)%MaxHeatAirVolFlow == Autosize) &
!                 UnitarySystem(UnitarySysNum)%RequestAutosize = .TRUE.
!            IF(UnitarySystem(UnitarySysNum)%DesignHeatingCapacity == Autosize) &
!                 UnitarySystem(UnitarySysNum)%RequestAutosize = .TRUE.

        ELSEIF (UnitarySystem(UnitarySysNum)%HeatingCoilType_Num == Coil_HeatingElectric_MultiStage .OR. &
                UnitarySystem(UnitarySysNum)%HeatingCoilType_Num == Coil_HeatingGas_MultiStage ) THEN

          ErrFlag = .FALSE.
          CALL GetHeatingCoilIndex(HeatingCoilName,UnitarySystem(UnitarySysNum)%HeatingCoilIndex, ErrFlag)
          IF (ErrFlag) THEN
            CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
            ErrorsFound=.TRUE.
            ErrFlag = .FALSE.
          END IF
          HeatingCoilInletNode = GetHeatingCoilInletNode(HeatingCoilType,HeatingCoilName,ErrFlag)
          IF (ErrFlag) THEN
            CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
            ErrorsFound=.TRUE.
            ErrFlag = .FALSE.
          END IF
          HeatingCoilOutletNode = GetHeatingCoilOutletNode(HeatingCoilType,HeatingCoilName,ErrFlag)
          IF (ErrFlag) THEN
            CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
            ErrorsFound=.TRUE.
            ErrFlag = .FALSE.
          END IF

          UnitarySystem(UnitarySysNum)%DesignHeatingCapacity =    &
            GetHeatingCoilCapacity(HeatingCoilType,HeatingCoilName,ErrFlag)

!            IF(UnitarySystem(UnitarySysNum)%MaxHeatAirVolFlow == Autosize) &
!                 UnitarySystem(UnitarySysNum)%RequestAutosize = .TRUE.
          IF(UnitarySystem(UnitarySysNum)%DesignHeatingCapacity == Autosize) &
               UnitarySystem(UnitarySysNum)%RequestAutosize = .TRUE.

        ELSEIF (UnitarySystem(UnitarySysNum)%HeatingCoilType_Num == Coil_HeatingGas          .OR. &
                UnitarySystem(UnitarySysNum)%HeatingCoilType_Num == Coil_HeatingElectric     .OR. &
                UnitarySystem(UnitarySysNum)%HeatingCoilType_Num == Coil_HeatingDesuperheater) THEN
            ErrFlag = .FALSE.
            IF (ErrFlag) THEN
              CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
              ErrorsFound=.TRUE.
              ErrFlag = .FALSE.
            ELSE

              CALL ValidateComponent(HeatingCoilType,HeatingCoilName,IsNotOK,  &
                                     TRIM(CurrentModuleObject))
              IF (IsNotOK) THEN
                CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
                ErrorsFound=.TRUE.

              ELSE ! mine data from heating coil

                ! Get heating coil index
                ErrFlag=.FALSE.
                CALL GetHeatingCoilIndex(HeatingCoilName,UnitarySystem(UnitarySysNum)%HeatingCoilIndex,ErrFlag)
                IF (ErrFlag) THEN
                  CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
                  ErrorsFound=.TRUE.
                  ErrFlag=.FALSE.
                END IF

                ! Get the design heating capacity
                UnitarySystem(UnitarySysNum)%DesignHeatingCapacity =   &
                   GetHeatingCoilCapacity(HeatingCoilType,HeatingCoilName,ErrFlag)
                IF(UnitarySystem(UnitarySysNum)%DesignHeatingCapacity == Autosize) &
                   UnitarySystem(UnitarySysNum)%RequestAutosize = .TRUE.
                IF (ErrFlag) THEN
                  CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
                  ErrorsFound=.TRUE.
                  ErrFlag=.FALSE.
                END IF

                UnitarySystem(UnitarySysNum)%HeatingCoilAvailSchPtr = &
                       GetCoilAvailScheduleIndex(HeatingCoilType,HeatingCoilName,ErrFlag)

                ! Get the Heating Coil Inlet Node
                HeatingCoilInletNode = &
                       GetHeatingCoilInletNode(HeatingCoilType,HeatingCoilName,ErrFlag)
                IF (ErrFlag) THEN
                  CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
                  ErrorsFound=.TRUE.
                  ErrFlag=.FALSE.
                END IF

                ! Get the Heating Coil Outlet Node
                HeatingCoilOutletNode = &
                       GetHeatingCoilOutletNode(HeatingCoilType,HeatingCoilName,ErrFlag)
                IF (ErrFlag) THEN
                  CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
                  ErrorsFound=.TRUE.
                  ErrFlag=.FALSE.
                END IF

                ! Get the Heating Coil PLF Curve Index
                HeatingCoilPLFCurveIndex = &
                       GetHeatingCoilPLFCurveIndex(HeatingCoilType,HeatingCoilName,ErrFlag)
                IF (ErrFlag) THEN
                  CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
                  ErrorsFound=.TRUE.
                  ErrFlag=.FALSE.
                END IF
               ! These heating coil types do not have an air flow input field
               IF(NumNumbers < iMaxHeatAirVolFlowNumericNum)THEN
                 UnitarySystem(UnitarySysNum)%MaxHeatAirVolFlow = Autosize
                 UnitarySystem(UnitarySysNum)%RequestAutosize = .TRUE.
               END IF
              END IF ! IF (IsNotOK) THEN

            END IF

        ELSEIF (UnitarySystem(UnitarySysNum)%HeatingCoilType_Num == Coil_HeatingWater) THEN
            CALL ValidateComponent(HeatingCoilType,HeatingCoilName,IsNotOK,TRIM(CurrentModuleObject))
            IF (IsNotOK) THEN
                CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
                ErrorsFound=.TRUE.
            ELSE ! mine data from heating coil object

                ErrFlag = .FALSE.
                UnitarySystem(UnitarySysNum)%HeatingCoilAvailSchPtr = &
                       GetWaterCoilAvailScheduleIndex(HeatingCoilType,HeatingCoilName,ErrFlag)
                IF(ErrFlag)THEN
                  CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
                  ErrorsFound = .TRUE.
                  ErrFlag = .FALSE.
                END IF

                UnitarySystem(UnitarySysNum)%HeatingCoilIndex = GetWaterCoilIndex('COIL:HEATING:WATER',HeatingCoilName,ErrFlag)
                IF (UnitarySystem(UnitarySysNum)%HeatingCoilIndex .EQ. 0) THEN
                  CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
                  CALL ShowContinueError('Illegal '//TRIM(cAlphaFields(iHeatingCoilNameAlphaNum))//' = ' &
                                  //TRIM(HeatingCoilName))
                  ErrorsFound = .TRUE.
                  ErrFlag = .FALSE.
                END IF

                ! Get the Heating Coil water Inlet or control Node number
                UnitarySystem(UnitarySysNum)%HeatCoilFluidInletNode = &
                    GetCoilWaterInletNode('Coil:Heating:Water',HeatingCoilName,ErrFlag)
                IF(ErrFlag)THEN
                  CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
                  ErrorsFound = .TRUE.
                  ErrFlag = .FALSE.
                END IF

                ! Get the Heating Coil hot water max volume flow rate
                UnitarySystem(UnitarySysNum)%MaxHeatCoilFluidFlow = GetCoilMaxWaterFlowRate('Coil:Heating:Water',  &
                                                           HeatingCoilName,ErrFlag)
                IF(UnitarySystem(UnitarySysNum)%MaxHeatCoilFluidFlow == Autosize) &
                   UnitarySystem(UnitarySysNum)%RequestAutosize = .TRUE.

                IF(ErrFlag)THEN
                  CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
                  ErrorsFound = .TRUE.
                  ErrFlag = .FALSE.
                END IF

                ! Get the Heating Coil Inlet Node
                HeatingCoilInletNode =  GetWaterCoilInletNode('Coil:Heating:Water',HeatingCoilName,ErrFlag)
                IF(ErrFlag)THEN
                  CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
                  ErrorsFound = .TRUE.
                  ErrFlag = .FALSE.
                END IF

                ! Get the Heating Coil Outlet Node
                HeatingCoilOutletNode = GetWaterCoilOutletNode('Coil:Heating:Water',HeatingCoilName,ErrFlag)
                IF(ErrFlag)THEN
                  CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
                  ErrorsFound = .TRUE.
                  ErrFlag = .FALSE.
                END IF

            END IF

        ELSEIF (UnitarySystem(UnitarySysNum)%HeatingCoilType_Num == Coil_HeatingSteam) THEN
            CALL ValidateComponent(HeatingCoilType,HeatingCoilName,IsNotOK,TRIM(CurrentModuleObject))
            IF (IsNotOK) THEN
                CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
                ErrorsFound=.TRUE.
            ELSE ! mine data from heating coil object

                ErrFlag = .FALSE.
                UnitarySystem(UnitarySysNum)%HeatingCoilAvailSchPtr = &
                       GetSteamCoilAvailScheduleIndex(HeatingCoilType,HeatingCoilName,ErrFlag)
                IF(ErrFlag)THEN
                  CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
                  ErrorsFound = .TRUE.
                  ErrFlag = .FALSE.
                END IF

                UnitarySystem(UnitarySysNum)%HeatingCoilIndex = GetSteamCoilIndex('COIL:HEATING:STEAM',HeatingCoilName,ErrFlag)
                IF (UnitarySystem(UnitarySysNum)%HeatingCoilIndex .EQ. 0) THEN
                  CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
                  CALL ShowContinueError('Illegal '//TRIM(cAlphaFields(iHeatingCoilNameAlphaNum))//' = ' &
                                  //TRIM(HeatingCoilName))
                  ErrorsFound = .TRUE.
                  ErrFlag = .FALSE.
                END IF

                ! Get the Heating Coil steam inlet node number
                ErrFlag = .FALSE.
                UnitarySystem(UnitarySysNum)%HeatCoilFluidInletNode = &
                    GetSteamCoilSteamInletNode('Coil:Heating:Steam',HeatingCoilName,ErrFlag)
                IF(ErrFlag)THEN
                  CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
                  ErrorsFound = .TRUE.
                  ErrFlag = .FALSE.
                END IF

                ! Get the Heating Coil steam max volume flow rate
                UnitarySystem(UnitarySysNum)%MaxHeatCoilFluidFlow = &
                    GetCoilMaxSteamFlowRate(UnitarySystem(UnitarySysNum)%HeatingCoilIndex,ErrFlag)
                IF(UnitarySystem(UnitarySysNum)%MaxHeatCoilFluidFlow == Autosize) &
                   UnitarySystem(UnitarySysNum)%RequestAutosize = .TRUE.

                IF (UnitarySystem(UnitarySysNum)%MaxHeatCoilFluidFlow .GT. 0.0d0)THEN
                  SteamIndex = 0      ! Function GetSatDensityRefrig will look up steam index if 0 is passed
                  SteamDensity=GetSatDensityRefrig("STEAM",TempSteamIn,1.0d0,SteamIndex,'GetAirLoopHVACHeatCoolInput')
                  UnitarySystem(UnitarySysNum)%MaxHeatCoilFluidFlow = &
                                UnitarySystem(UnitarySysNum)%MaxHeatCoilFluidFlow * SteamDensity
                  ErrFlag = .FALSE.
                END IF

                ! Get the Heating Coil Inlet Node
                ErrFlag = .FALSE.
                HeatingCoilInletNode = &
                         GetSteamCoilAirInletNode(UnitarySystem(UnitarySysNum)%HeatingCoilIndex,HeatingCoilName,ErrFlag)
                IF(ErrFlag)THEN
                  CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
                  ErrorsFound = .TRUE.
                  ErrFlag = .FALSE.
                END IF

                ! Get the Heating Coil Outlet Node
                HeatingCoilOutletNode = &
                         GetSteamCoilAirOutletNode(UnitarySystem(UnitarySysNum)%HeatingCoilIndex,HeatingCoilName,ErrFlag)
                IF(ErrFlag)THEN
                  CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
                  ErrorsFound = .TRUE.
                  ErrFlag = .FALSE.
                END IF

               IF(NumNumbers < iMaxHeatAirVolFlowNumericNum)THEN
                 UnitarySystem(UnitarySysNum)%MaxHeatAirVolFlow = Autosize
                 UnitarySystem(UnitarySysNum)%RequestAutosize = .TRUE.
               END IF

            END IF

        ELSEIF (UnitarySystem(UnitarySysNum)%HeatingCoilType_Num == Coil_HeatingWaterToAirHPSimple) THEN
            UnitarySystem(UnitarySysNum)%DXHeatingCoil = .TRUE.
            CALL ValidateComponent(HeatingCoilType,HeatingCoilName,IsNotOK,TRIM(CurrentModuleObject))
            IF (IsNotOK) THEN
                CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
                ErrorsFound=.TRUE.
            ELSE ! mine data from heating coil object

                ErrFlag = .FALSE.
                UnitarySystem(UnitarySysNum)%HeatingCoilAvailSchPtr = ScheduleAlwaysOn
                IF(ErrFlag)THEN
                  CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
                  ErrorsFound = .TRUE.
                  ErrFlag = .FALSE.
                END IF

                UnitarySystem(UnitarySysNum)%HeatingCoilIndex = GetWtoAHPSimpleCoilIndex(HeatingCoilType,HeatingCoilName,ErrFlag)
                IF (UnitarySystem(UnitarySysNum)%HeatingCoilIndex .EQ. 0) THEN
                  CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
                  CALL ShowContinueError('Illegal '//TRIM(cAlphaFields(iHeatingCoilNameAlphaNum))//' = ' &
                                  //TRIM(HeatingCoilName))
                  ErrorsFound = .TRUE.
                  ErrFlag = .FALSE.
                END IF

                ! Get the Heating Coil Inlet Node
                ErrFlag = .FALSE.
                HeatingCoilInletNode = GetWtoAHPSimpleCoilInletNode(HeatingCoilType, HeatingCoilName,ErrFlag)
                IF(ErrFlag)THEN
                  CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
                  ErrorsFound = .TRUE.
                  ErrFlag = .FALSE.
                END IF

                ! Get the Heating Coil Outlet Node
                HeatingCoilOutletNode = GetWtoAHPSimpleCoilOutletNode(HeatingCoilType, HeatingCoilName,ErrFlag)
                IF(ErrFlag)THEN
                  CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
                  ErrorsFound = .TRUE.
                  ErrFlag = .FALSE.
                END IF

!                IF(UnitarySystem(UnitarySysNum)%MaxHeatCoilFluidFlow == Autosize) &
!                   UnitarySystem(UnitarySysNum)%RequestAutosize = .TRUE.
!                IF(UnitarySystem(UnitarySysNum)%DesignHeatingCapacity == Autosize) &
!                   UnitarySystem(UnitarySysNum)%RequestAutosize = .TRUE.
!                IF(UnitarySystem(UnitarySysNum)%MaxHeatCoilFluidFlow == Autosize) &
!                   UnitarySystem(UnitarySysNum)%RequestAutosize = .TRUE.
            END IF

        ELSEIF (UnitarySystem(UnitarySysNum)%HeatingCoilType_Num == Coil_HeatingWaterToAirHP) THEN
            UnitarySystem(UnitarySysNum)%DXHeatingCoil = .TRUE.
            CALL ValidateComponent(HeatingCoilType,HeatingCoilName,IsNotOK,TRIM(CurrentModuleObject))
            IF (IsNotOK) THEN
                  CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
                ErrorsFound=.TRUE.
            ELSE ! mine data from heating coil object

                ErrFlag = .FALSE.
                UnitarySystem(UnitarySysNum)%HeatingCoilAvailSchPtr = ScheduleAlwaysOn
                IF(ErrFlag)THEN
                  CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
                  ErrorsFound = .TRUE.
                  ErrFlag = .FALSE.
                END IF

                UnitarySystem(UnitarySysNum)%HeatingCoilIndex =  GetWtoAHPCoilIndex(HeatingCoilType, HeatingCoilName,ErrFlag)
                IF (UnitarySystem(UnitarySysNum)%HeatingCoilIndex .EQ. 0) THEN
                  CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
                  CALL ShowContinueError('Illegal '//TRIM(cAlphaFields(iHeatingCoilNameAlphaNum))//' = ' &
                                  //TRIM(HeatingCoilName))
                  ErrorsFound = .TRUE.
                  ErrFlag = .FALSE.
                END IF

                ! Get the Heating Coil Inlet Node
                ErrFlag = .FALSE.
                HeatingCoilInletNode = GetWtoAHPCoilInletNode(HeatingCoilType, HeatingCoilName,ErrFlag)
                IF(ErrFlag)THEN
                  CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
                  ErrorsFound = .TRUE.
                  ErrFlag = .FALSE.
                END IF

                ! Get the Heating Coil Outlet Node
                HeatingCoilOutletNode = GetWtoAHPCoilOutletNode(HeatingCoilType, HeatingCoilName,ErrFlag)
                IF(ErrFlag)THEN
                  CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
                  ErrorsFound = .TRUE.
                  ErrFlag = .FALSE.
                END IF

!                IF(UnitarySystem(UnitarySysNum)%MaxHeatCoilFluidFlow == Autosize) &
!                   UnitarySystem(UnitarySysNum)%RequestAutosize = .TRUE.
!                IF(UnitarySystem(UnitarySysNum)%DesignHeatingCapacity == Autosize) &
!                   UnitarySystem(UnitarySysNum)%RequestAutosize = .TRUE.
!                IF(UnitarySystem(UnitarySysNum)%MaxHeatCoilFluidFlow == Autosize) &
!                   UnitarySystem(UnitarySysNum)%RequestAutosize = .TRUE.

            END IF

        ELSEIF(UnitarySystem(UnitarySysNum)%HeatCoilExists)THEN
          CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
          CALL ShowContinueError('Illegal '//TRIM(cAlphaFields(iHeatingCoilTypeAlphaNum))//' = '// &
                                 TRIM(Alphas(iHeatingCoilTypeAlphaNum)))
          ErrorsFound=.TRUE.
        END IF ! IF (UnitarySystem(UnitarySysNum)%HeatingCoilType_Num == Coil_HeatingGas .OR. &, etc.

        IF(UnitarySystem(UnitarySysNum)%HeatingCoilType_Num == CoilDX_MultiSpeedHeating .OR. &
           UnitarySystem(UnitarySysNum)%HeatingCoilType_Num == Coil_HeatingElectric_MultiStage .OR. &
           UnitarySystem(UnitarySysNum)%HeatingCoilType_Num == Coil_HeatingGas_MultiStage) THEN
          UnitarySystem(UnitarySysNum)%MultiSpeedHeatingCoil = .TRUE.
        ELSEIF(UnitarySystem(UnitarySysNum)%HeatingCoilType_Num == Coil_HeatingWaterToAirHPVSEquationFit .OR. &
           UnitarySystem(UnitarySysNum)%HeatingCoilType_Num == Coil_HeatingAirToAirVariableSpeed ) THEN
          UnitarySystem(UnitarySysNum)%VarSpeedHeatingCoil = .TRUE.
        END IF

        ! coil outlet node set point has priority, IF not exist, then use system outlet node
        IF(NodeHasSPMCtrlVarType(UnitarySystem(UnitarySysNum)%UnitarySystemOutletNodeNum, iCtrlVarType_Temp)) &
            UnitarySystem(UnitarySysNum)%SystemHeatControlNodeNum = UnitarySystem(UnitarySysNum)%UnitarySystemOutletNodeNum
        IF(NodeHasSPMCtrlVarType(HeatingCoilOutletNode, iCtrlVarType_Temp)) &
          UnitarySystem(UnitarySysNum)%SystemHeatControlNodeNum = HeatingCoilOutletNode

        UnitarySystem(UnitarySysNum)%HeatCoilInletNodeNum  = HeatingCoilInletNode
        UnitarySystem(UnitarySysNum)%HeatCoilOutletNodeNum = HeatingCoilOutletNode
        UnitarySystem(UnitarySysNum)%HeatingCoilName       = HeatingCoilName

        ! Add heating coil to component sets array
        IF(UnitarySystem(UnitarySysNum)%HeatCoilExists) THEN
          IF (UnitarySystem(UnitarySysNum)%HeatingCoilType_Num .NE. CoilDX_MultiSpeedHeating) THEN
            CALL SetUpCompSets(CurrentModuleObject, Alphas(iNameAlphaNum), &
                               Alphas(iHeatingCoilTypeAlphaNum),Alphas(iHeatingCoilNameAlphaNum), &
                               NodeID(HeatingCoilInletNode), NodeID(HeatingCoilOutletNode))
          ELSE
            CALL SetUpCompSets(CurrentModuleObject, Alphas(iNameAlphaNum), &
                               Alphas(iHeatingCoilTypeAlphaNum),Alphas(iHeatingCoilNameAlphaNum), &
                               'UNDEFINED', 'UNDEFINED')
          END IF
        END IF
        ! Get Cooling Coil Information IF available
        CoolingCoilType = Alphas(iCoolingCoilTypeAlphaNum)
        CoolingCoilName = Alphas(iCoolingCoilNameAlphaNum)
        IF(.NOT. lAlphaBlanks(iCoolingCoilTypeAlphaNum))THEN
          UnitarySystem(UnitarySysNum)%CoolCoilExists = .TRUE.

!       Find the type of coil. do not print message since this may not be the correct coil type.
        ErrFlag = .FALSE.
        IF (SameString(CoolingCoilType, 'Coil:Cooling:DX:VariableSpeed') )THEN
          UnitarySystem(UnitarySysNum)%CoolingCoilType_Num = Coil_CoolingAirToAirVariableSpeed
        ELSEIF (SameString(CoolingCoilType,'Coil:Cooling:DX:MultiSpeed')) THEN
          UnitarySystem(UnitarySysNum)%CoolingCoilType_Num = CoilDX_MultiSpeedCooling
        ELSEIF (SameString(CoolingCoilType,'Coil:Cooling:Water')) THEN
          UnitarySystem(UnitarySysNum)%CoolingCoilType_Num = Coil_CoolingWater
        ELSEIF (SameString(CoolingCoilType,'Coil:Cooling:Water:DetailedGeometry')) THEN
          UnitarySystem(UnitarySysNum)%CoolingCoilType_Num = Coil_CoolingWaterDetailed
        ELSEIF (SameString(CoolingCoilType,'Coil:Cooling:DX:TwoStageWithHumidityControlMode')) THEN
          UnitarySystem(UnitarySysNum)%CoolingCoilType_Num = CoilDX_CoolingTwoStageWHumControl
        ELSEIF (SameString(CoolingCoilType,'CoilSystem:Cooling:DX:HeatExchangerAssisted')) THEN
          UnitarySystem(UnitarySysNum)%CoolingCoilType_Num = &
                 GetHXAssistedCoilTypeNum(CoolingCoilType,CoolingCoilName,ErrFlag,PrintMessage)
        ELSEIF (SameString(CoolingCoilType,'CoilSystem:Cooling:Water:HeatExchangerAssisted')) THEN
          ! why use a mining function, why not just set it using the integer representation?
          UnitarySystem(UnitarySysNum)%CoolingCoilType_Num = &
                 GetHXAssistedCoilTypeNum(CoolingCoilType,CoolingCoilName,ErrFlag,PrintMessage)
        ELSEIF (SameString(CoolingCoilType,'Coil:Cooling:WaterToAirHeatPump:EquationFit')) THEN
            UnitarySystem(UnitarySysNum)%CoolingCoilType_Num = Coil_CoolingWaterToAirHPSimple
        ELSEIF (SameString(CoolingCoilType,'Coil:Cooling:WaterToAirHeatPump:ParameterEstimation')) THEN
            UnitarySystem(UnitarySysNum)%CoolingCoilType_Num = Coil_CoolingWaterToAirHP
        ELSEIF (SameString(CoolingCoilType,'Coil:Cooling:WaterToAirHeatPump:VariableSpeedEquationFit')) THEN
            UnitarySystem(UnitarySysNum)%CoolingCoilType_Num = Coil_CoolingWaterToAirHPVSEquationFit
        ELSEIF (SameString(CoolingCoilType,'Coil:Cooling:DX:SingleSpeed')) THEN
          UnitarySystem(UnitarySysNum)%CoolingCoilType_Num = &
                 GetDXCoilTypeNum(CoolingCoilType,CoolingCoilName,ErrFlag,PrintMessage)
        ELSEIF (SameString(CoolingCoilType,'Coil:Cooling:DX:TwoSpeed')) THEN
          UnitarySystem(UnitarySysNum)%CoolingCoilType_Num = &
                 GetDXCoilTypeNum(CoolingCoilType,CoolingCoilName,ErrFlag,PrintMessage)
        ELSE
          CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
          CALL ShowContinueError('Illegal '//TRIM(cAlphaFields(iCoolingCoilTypeAlphaNum))//' = '// &
                                 TRIM(Alphas(iCoolingCoilTypeAlphaNum)))
        END IF

        IF (UnitarySystem(UnitarySysNum)%CoolingCoilType_Num == CoilDX_CoolingSingleSpeed .OR. &
            UnitarySystem(UnitarySysNum)%CoolingCoilType_Num == CoilDX_CoolingTwoSpeed)THEN
          CALL ValidateComponent(CoolingCoilType,CoolingCoilName,IsNotOK,  &
                                 TRIM(CurrentModuleObject))
          IF (IsNotOK) THEN
            CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
            ErrorsFound=.TRUE.

          ELSE ! mine data from DX cooling coil

            IF(UnitarySystem(UnitarySysNum)%CoolingCoilType_Num == CoilDX_CoolingTwoSpeed) &
                 UnitarySystem(UnitarySysNum)%NumOfSpeedCooling = 2

            ! Get DX cooling coil index
            CALL GetDXCoilIndex(CoolingCoilName,UnitarySystem(UnitarySysNum)%CoolingCoilIndex,IsNotOK)
            IF(IsNotOK)THEN
              CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
              ErrorsFound=.TRUE.
            END IF

            UnitarySystem(UnitarySysNum)%CoolingCoilAvailSchPtr = GetDXCoilAvailSchPtr(CoolingCoilType,CoolingCoilName,ErrFlag)

            ! Get DX cooling coil capacity
            ErrFlag = .FALSE.
            UnitarySystem(UnitarySysNum)%DesignCoolingCapacity =    &
                 GetDXCoilCapacity(CoolingCoilType,CoolingCoilName,ErrFlag)
            IF(UnitarySystem(UnitarySysNum)%DesignCoolingCapacity == Autosize) &
                 UnitarySystem(UnitarySysNum)%RequestAutosize = .TRUE.
            IF (ErrFlag) THEN
              CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
              ErrorsFound=.TRUE.
            END IF

            ! Get DX coil air flow rate. Latter fields will overwrite this IF input field is present
            UnitarySystem(UnitarySysNum)%MaxCoolAirVolFlow   = GetDXCoilAirFlow(CoolingCoilType,CoolingCoilName,ErrFlag)
            IF(UnitarySystem(UnitarySysNum)%MaxCoolAirVolFlow == Autosize) &
                 UnitarySystem(UnitarySysNum)%RequestAutosize = .TRUE.

            ! Get the Cooling Coil Nodes
            ErrFlag=.FALSE.
            CoolingCoilInletNode = GetDXCoilInletNode(CoolingCoilType,CoolingCoilName,ErrFlag)
            CoolingCoilOutletNode = GetDXCoilOutletNode(CoolingCoilType,CoolingCoilName,ErrFlag)
            IF (ErrFlag) THEN
              CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
              ErrorsFound=.TRUE.
            END IF

            ! Get Outdoor condenser node from DX coil object
            ErrFlag=.FALSE.
            UnitarySystem(UnitarySysNum)%CondenserNodeNum = &
                    GetDXCoilCondenserInletNode(CoolingCoilType,CoolingCoilName,ErrFlag)
            IF (ErrFlag) THEN
              CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
              ErrorsFound=.TRUE.
            END IF

            IF(UnitarySystem(UnitarySysNum)%FanExists)THEN
              ErrFlag = .FALSE.
              CALL GetFanIndex(FanName,FanIndex,ErrFlag,CurrentModuleObject)
              CALL SetDXCoolingCoilData(UnitarySystem(UnitarySysNum)%CoolingCoilIndex,ErrFlag, &
                                        SupplyFanName=FanName)
              CALL SetDXCoolingCoilData(UnitarySystem(UnitarySysNum)%CoolingCoilIndex,ErrFlag, &
                                        SupplyFanIndex=FanIndex)
              IF (ErrFlag) THEN
                CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
                ErrorsFound=.TRUE.
              END IF
            END IF
            IF(UnitarySystem(UnitarySysNum)%HeatCoilExists)THEN
              IF(UnitarySystem(UnitarySysNum)%HeatingCoilType_Num == Coil_HeatingAirToAirVariableSpeed .OR. &
                 UnitarySystem(UnitarySysNum)%HeatingCoilType_Num == Coil_HeatingWaterToAirHPVSEquationFit .OR. &
                 UnitarySystem(UnitarySysNum)%HeatingCoilType_Num == Coil_HeatingWaterToAirHPSimple .OR. &
                 UnitarySystem(UnitarySysNum)%HeatingCoilType_Num == CoilDX_MultiSpeedHeating .OR. &
                 UnitarySystem(UnitarySysNum)%HeatingCoilType_Num == CoilDX_HeatingEmpirical)THEN
                UnitarySystem(UnitarySysNum)%HeatPump = .TRUE.
              END IF
            END IF

          END IF ! IF (IsNotOK) THEN

          ! Push heating coil PLF curve index to DX coil
          IF(HeatingCoilPLFCurveIndex .GT. 0)THEN
            CALL SetDXCoolingCoilData(UnitarySystem(UnitarySysNum)%CoolingCoilIndex,ErrorsFound, &
                                      HeatingCoilPLFCurvePTR=HeatingCoilPLFCurveIndex)
          END IF

        ELSEIF (UnitarySystem(UnitarySysNum)%CoolingCoilType_Num == CoilDX_CoolingTwoStageWHumControl)THEN
          CALL ValidateComponent(CoolingCoilType,CoolingCoilName,IsNotOK,  &
                                 TRIM(CurrentModuleObject))
          IF (IsNotOK) THEN
            CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
            ErrorsFound=.TRUE.

          ELSE ! mine data from DX cooling coil

            ! Get DX cooling coil index
            CALL GetDXCoilIndex(CoolingCoilName,UnitarySystem(UnitarySysNum)%CoolingCoilIndex,IsNotOK)
            IF(IsNotOK)THEN
              CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
              ErrorsFound=.TRUE.
            END IF

            UnitarySystem(UnitarySysNum)%CoolingCoilAvailSchPtr = GetDXCoilAvailSchPtr(CoolingCoilType,CoolingCoilName,ErrFlag)

            ! Get DX cooling coil capacity
            ErrFlag = .FALSE.
            UnitarySystem(UnitarySysNum)%DesignCoolingCapacity =    &
                 GetDXCoilCapacity(CoolingCoilType,CoolingCoilName,ErrFlag)
            IF(UnitarySystem(UnitarySysNum)%DesignCoolingCapacity == Autosize) &
                 UnitarySystem(UnitarySysNum)%RequestAutosize = .TRUE.
            IF (ErrFlag) THEN
              CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
              ErrorsFound=.TRUE.
            END IF

            ! Get DX coil air flow rate. Later fields will overwrite this IF input field is present
            UnitarySystem(UnitarySysNum)%MaxCoolAirVolFlow   = GetDXCoilAirFlow(CoolingCoilType,CoolingCoilName,ErrFlag)
            IF(UnitarySystem(UnitarySysNum)%MaxCoolAirVolFlow == Autosize) &
                 UnitarySystem(UnitarySysNum)%RequestAutosize = .TRUE.

            ! Get the Cooling Coil Nodes
            ErrFlag=.FALSE.
            CoolingCoilInletNode = GetDXCoilInletNode(CoolingCoilType,CoolingCoilName,ErrFlag)
            CoolingCoilOutletNode = GetDXCoilOutletNode(CoolingCoilType,CoolingCoilName,ErrFlag)
            IF (ErrFlag) THEN
              CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
              ErrorsFound=.TRUE.
            END IF

            ! Get Outdoor condenser node from DX coil object
            ErrFlag=.FALSE.
            UnitarySystem(UnitarySysNum)%CondenserNodeNum = &
                    GetDXCoilCondenserInletNode(CoolingCoilType,CoolingCoilName,ErrFlag)
            IF (ErrFlag) THEN
              CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
              ErrorsFound=.TRUE.
            END IF

          END IF ! IF (IsNotOK) THEN

          ! Push heating coil PLF curve index to DX coil
          IF(HeatingCoilPLFCurveIndex .GT. 0)THEN
            CALL SetDXCoolingCoilData(UnitarySystem(UnitarySysNum)%CoolingCoilIndex,ErrorsFound, &
                                      HeatingCoilPLFCurvePTR=HeatingCoilPLFCurveIndex)
          END IF

            IF(UnitarySystem(UnitarySysNum)%HeatCoilExists)THEN
              IF(UnitarySystem(UnitarySysNum)%HeatingCoilType_Num == Coil_HeatingAirToAirVariableSpeed .OR. &
                 UnitarySystem(UnitarySysNum)%HeatingCoilType_Num == Coil_HeatingWaterToAirHPVSEquationFit .OR. &
                 UnitarySystem(UnitarySysNum)%HeatingCoilType_Num == Coil_HeatingWaterToAirHPSimple .OR. &
                 UnitarySystem(UnitarySysNum)%HeatingCoilType_Num == CoilDX_MultiSpeedHeating .OR. &
                 UnitarySystem(UnitarySysNum)%HeatingCoilType_Num == CoilDX_HeatingEmpirical)THEN
                UnitarySystem(UnitarySysNum)%HeatPump = .TRUE.
              END IF
            END IF

        ELSEIF (UnitarySystem(UnitarySysNum)%CoolingCoilType_Num == CoilDX_CoolingHXAssisted)THEN
          CALL ValidateComponent(CoolingCoilType,CoolingCoilName,IsNotOK,  &
                                 TRIM(CurrentModuleObject))
          IF (IsNotOK) THEN
            CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
            ErrorsFound=.TRUE.

          ELSE ! mine data from heat exchanger assisted cooling coil

            ! Get DX heat exchanger assisted cooling coil index
            CALL GetHXDXCoilIndex(CoolingCoilName,UnitarySystem(UnitarySysNum)%CoolingCoilIndex,IsNotOK)
            IF(IsNotOK)THEN
              CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
              ErrorsFound=.TRUE.
            END IF

            UnitarySystem(UnitarySysNum)%CoolingCoilAvailSchPtr = &
              GetDXCoilAvailSchPtr(CALLCoilTypes(UnitarySystem(UnitarySysNum)%CoolingCoilIndex), &
                                   GetHXDXCoilName(CoolingCoilType,CoolingCoilName,IsNotOK),ErrFlag)

            ! Get DX cooling coil capacity
            ErrFlag = .FALSE.
            UnitarySystem(UnitarySysNum)%DesignCoolingCapacity =    &
               GetDXHXAsstdCoilCapacity(CoolingCoilType,CoolingCoilName,ErrFlag)
            IF(UnitarySystem(UnitarySysNum)%DesignCoolingCapacity == Autosize) &
                 UnitarySystem(UnitarySysNum)%RequestAutosize = .TRUE.
            IF (ErrFlag) THEN
              CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
              ErrorsFound=.TRUE.
            END IF

            ! Get the Cooling Coil Nodes
            ErrFlag=.FALSE.
            CoolingCoilInletNode = GetDXHXAsstdCoilInletNode(CoolingCoilType,CoolingCoilName,ErrFlag)
            CoolingCoilOutletNode = GetDXHXAsstdCoilOutletNode(CoolingCoilType,CoolingCoilName,ErrFlag)
            IF (ErrFlag) THEN
              CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
              ErrorsFound=.TRUE.
            END IF

            ! Get Outdoor condenser node from heat exchanger assisted DX coil object
            ErrFlag=.FALSE.
            UnitarySystem(UnitarySysNum)%CondenserNodeNum = &
                GetDXCoilCondenserInletNode('COIL:COOLING:DX:SINGLESPEED', &
                GetHXDXCoilName(CoolingCoilType,CoolingCoilName,ErrFlag), ErrFlag)

            IF (ErrFlag) THEN
              CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
              ErrorsFound=.TRUE.
            END IF

            ! Push heating coil PLF curve index to DX coil
            IF(HeatingCoilPLFCurveIndex .GT. 0)THEN
              ! get the actual index to the DX cooling coil object
              DXCoilIndex = GetActualDXCoilIndex(CoolingCoilType,CoolingCoilName,ErrorsFound)
              UnitarySystem(UnitarySysNum)%ActualDXCoilIndexforHXAssisted = DXCoilIndex
              CALL SetDXCoolingCoilData(DXCoilIndex,ErrorsFound, &
                                        HeatingCoilPLFCurvePTR=HeatingCoilPLFCurveIndex)
            END IF

            IF(UnitarySystem(UnitarySysNum)%HeatCoilExists)THEN
              IF(UnitarySystem(UnitarySysNum)%HeatingCoilType_Num == Coil_HeatingAirToAirVariableSpeed .OR. &
                 UnitarySystem(UnitarySysNum)%HeatingCoilType_Num == Coil_HeatingWaterToAirHPVSEquationFit .OR. &
                 UnitarySystem(UnitarySysNum)%HeatingCoilType_Num == Coil_HeatingWaterToAirHPSimple .OR. &
                 UnitarySystem(UnitarySysNum)%HeatingCoilType_Num == CoilDX_MultiSpeedHeating .OR. &
                 UnitarySystem(UnitarySysNum)%HeatingCoilType_Num == CoilDX_HeatingEmpirical)THEN
                UnitarySystem(UnitarySysNum)%HeatPump = .TRUE.
              END IF
            END IF

          END IF ! IF (IsNotOK) THEN
        ELSEIF (UnitarySystem(UnitarySysNum)%CoolingCoilType_Num == CoilWater_CoolingHXAssisted)THEN
          CALL ValidateComponent(CoolingCoilType,CoolingCoilName,IsNotOK,  &
                                 TRIM(CurrentModuleObject))
          IF (IsNotOK) THEN
            CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
            ErrorsFound=.TRUE.

          ELSE ! mine data from heat exchanger assisted cooling coil

            ErrFlag = .FALSE.
            ActualCoolCoilType = GetCoilObjectTypeNum(CoolingCoilType,CoolingCoilName,ErrFlag,.TRUE.)
            HXCoilName = GetHXDXCoilName(CoolingCoilType,CoolingCoilName,ErrFlag)

            IF(ErrFlag)THEN
              CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
              ErrorsFound=.TRUE.
            END IF

            ! Get DX heat exchanger assisted cooling coil index
            ErrFlag = .FALSE.
            CALL GetHXDXCoilIndex(CoolingCoilName,UnitarySystem(UnitarySysNum)%CoolingCoilIndex,ErrFlag)
            IF(ErrFlag)THEN
              CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
              ErrorsFound=.TRUE.
            END IF

            ErrFlag = .FALSE.
            UnitarySystem(UnitarySysNum)%CoolingCoilAvailSchPtr = &
              GetWaterCoilAvailScheduleIndex(cAllCoilTypes(ActualCoolCoilType),HXCoilName,ErrFlag)
            UnitarySystem(UnitarySysNum)%MaxCoolCoilFluidFlow = &
              GetCoilMaxWaterFlowRate(cAllCoilTypes(ActualCoolCoilType),HXCoilName,ErrFlag)
            ! Get the Cooling Coil water Inlet Node number
            UnitarySystem(UnitarySysNum)%CoolCoilFluidInletNode = &
              GetCoilWaterInletNode(cAllCoilTypes(ActualCoolCoilType),HXCoilName,ErrFlag)
!            UnitarySystem(UnitarySysNum)%CoolCoilFluidOutletNodeNum = &
!              GetCoilWaterOutletNode(cAllCoilTypes(ActualCoolCoilType),HXCoilName,ErrFlag)
            IF(ErrFlag)THEN
              CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
              ErrorsFound=.TRUE.
            END IF

   ! this needs to be the coil index, not the parent
   ! CAN't do this here, we need to know the air flow rate first
!            UnitarySystem(UnitarySysNum)%DesignCoolingCapacity =    &
!               GetDXHXAsstdCoilCapacity(CoolingCoilType,CoolingCoilName,ErrFlag)
!            UnitarySystem(UnitarySysNum)%CoolingCoilAvailSchPtr = GetDXCoilAvailSchPtr(CoolingCoilType,CoolingCoilName,ErrFlag)

            ! Get the Cooling Coil Nodes
            ErrFlag=.FALSE.
            CoolingCoilInletNode = GetDXHXAsstdCoilInletNode(CoolingCoilType,CoolingCoilName,ErrFlag)
            CoolingCoilOutletNode = GetDXHXAsstdCoilOutletNode(CoolingCoilType,CoolingCoilName,ErrFlag)
            IF (ErrFlag) THEN
              CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
              ErrorsFound=.TRUE.
            END IF

            UnitarySystem(UnitarySysNum)%MaxCoolAirVolFlow = GetHXCoilAirFlowRate(CoolingCoilType,CoolingCoilName,ErrFlag)
            IF(UnitarySystem(UnitarySysNum)%MaxCoolAirVolFlow == Autosize) &
                 UnitarySystem(UnitarySysNum)%RequestAutosize = .TRUE.

            UnitarySystem(UnitarySysNum)%CondenserNodeNum = 0
           IF (ErrFlag) THEN
              CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
              ErrorsFound=.TRUE.
            END IF

            ! Push heating coil PLF curve index to DX coil
            IF(HeatingCoilPLFCurveIndex .GT. 0)THEN
              ! get the actual index to the DX cooling coil object
              DXCoilIndex = GetActualDXCoilIndex(CoolingCoilType,CoolingCoilName,ErrorsFound)
              UnitarySystem(UnitarySysNum)%ActualDXCoilIndexforHXAssisted = DXCoilIndex
              CALL SetDXCoolingCoilData(DXCoilIndex,ErrorsFound, &
                                        HeatingCoilPLFCurvePTR=HeatingCoilPLFCurveIndex)
            END IF

            IF(UnitarySystem(UnitarySysNum)%HeatCoilExists)THEN
              IF(UnitarySystem(UnitarySysNum)%HeatingCoilType_Num == Coil_HeatingAirToAirVariableSpeed .OR. &
                 UnitarySystem(UnitarySysNum)%HeatingCoilType_Num == Coil_HeatingWaterToAirHPVSEquationFit .OR. &
                 UnitarySystem(UnitarySysNum)%HeatingCoilType_Num == Coil_HeatingWaterToAirHPSimple .OR. &
                 UnitarySystem(UnitarySysNum)%HeatingCoilType_Num == CoilDX_MultiSpeedHeating .OR. &
                 UnitarySystem(UnitarySysNum)%HeatingCoilType_Num == CoilDX_HeatingEmpirical)THEN
                UnitarySystem(UnitarySysNum)%HeatPump = .TRUE.
              END IF
            END IF

          END IF ! IF (IsNotOK) THEN
        ELSEIF (UnitarySystem(UnitarySysNum)%CoolingCoilType_Num == Coil_CoolingAirToAirVariableSpeed .OR. &
                 UnitarySystem(UnitarySysNum)%CoolingCoilType_Num == Coil_CoolingWaterToAirHPVSEquationFit)THEN
          CALL ValidateComponent(CoolingCoilType,CoolingCoilName,IsNotOK,  &
                                 TRIM(CurrentModuleObject))
          IF (IsNotOK) THEN
              CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
            ErrorsFound=.TRUE.
          ELSE
            ErrFlag = .FALSE.
            UnitarySystem(UnitarySysNum)%CoolingCoilIndex = GetCoilIndexVariableSpeed(CoolingCoilType, &
                        CoolingCoilName,ErrFlag)
            IF(ErrFlag)THEN
              CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
              ErrorsFound=.TRUE.
              ErrFlag = .FALSE.
            END IF

            CoolingCoilInletNode =GetCoilInletNodeVariableSpeed(CoolingCoilType,CoolingCoilName,ErrFlag)
            IF(ErrFlag)THEN
              CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
              ErrorsFound=.TRUE.
              ErrFlag = .FALSE.
            END IF

            CoolingCoilOutletNode =GetCoilOutletNodeVariableSpeed(CoolingCoilType,CoolingCoilName,ErrFlag)
            IF(ErrFlag)THEN
              CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
              ErrorsFound=.TRUE.
              ErrFlag = .FALSE.
            END IF

            UnitarySystem(UnitarySysNum)%CondenserNodeNum = GetVSCoilCondenserInletNode(CoolingCoilName,ErrFlag)
            IF(ErrFlag)THEN
              CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
              ErrorsFound=.TRUE.
              ErrFlag = .FALSE.
            END IF

            UnitarySystem(UnitarySysNum)%CoolingCoilAvailSchPtr = ScheduleAlwaysOn

            UnitarySystem(UnitarySysNum)%NumOfSpeedCooling = GetVSCoilNumOfSpeeds(CoolingCoilName,ErrFlag)
            IF(ErrFlag)THEN
              CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
              ErrorsFound=.TRUE.
              ErrFlag = .FALSE.
            END IF

            ErrFlag=.FALSE.
            UnitarySystem(UnitarySysNum)%DesignCoolingCapacity =   &
              GetCoilCapacityVariableSpeed(CoolingCoilType,CoolingCoilName,ErrFlag)
            IF(UnitarySystem(UnitarySysNum)%DesignCoolingCapacity == Autosize) &
                 UnitarySystem(UnitarySysNum)%RequestAutosize = .TRUE.
            IF (ErrFlag) THEN
              CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
              ErrorsFound=.TRUE.
            END IF
          END IF

          ! why was this done?
          IF (UnitarySystem(UnitarySysNum)%CoolingCoilType_Num == Coil_CoolingAirToAirVariableSpeed) THEN
             ErrFlag=.FALSE.
             UnitarySystem(UnitarySysNum)%MaxCoolAirVolFlow       &
                = GetCoilAirFlowRateVariableSpeed(CoolingCoilType,CoolingCoilName,ErrFlag)
             IF(UnitarySystem(UnitarySysNum)%MaxCoolAirVolFlow == Autosize) &
                  UnitarySystem(UnitarySysNum)%RequestAutosize = .TRUE.

             IF (ErrFlag) THEN
                CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
                ErrorsFound=.TRUE.
             END IF

!            UnitarySystem(UnitarySysNum)%MaxNoCoolHeatAirVolFlow = &
!                 MIN(UnitarySystem(UnitarySysNum)%MaxHeatAirVolFlow,UnitarySystem(UnitarySysNum)%MaxCoolAirVolFlow)
!            IF(UnitarySystem(UnitarySysNum)%FanExists)THEN
!              IF(UnitarySystem(UnitarySysNum)%MaxHeatAirVolFlow /= Autosize .AND. &
!                 UnitarySystem(UnitarySysNum)%MaxCoolAirVolFlow /= Autosize .AND. &
!                 .NOT. UnitarySystem(UnitarySysNum)%RequestAutosize)THEN
!                 UnitarySystem(UnitarySysNum)%DesignFanVolFlowRate = &
!                   MAX(UnitarySystem(UnitarySysNum)%MaxHeatAirVolFlow,UnitarySystem(UnitarySysNum)%MaxCoolAirVolFlow)
!              ELSE
!                UnitarySystem(UnitarySysNum)%DesignFanVolFlowRate = Autosize
!              END IF
!            END IF
          END IF

          IF(UnitarySystem(UnitarySysNum)%HeatCoilExists)THEN
            IF(UnitarySystem(UnitarySysNum)%HeatingCoilType_Num == Coil_HeatingAirToAirVariableSpeed .OR. &
               UnitarySystem(UnitarySysNum)%HeatingCoilType_Num == Coil_HeatingWaterToAirHPVSEquationFit .OR. &
               UnitarySystem(UnitarySysNum)%HeatingCoilType_Num == Coil_HeatingWaterToAirHPSimple .OR. &
               UnitarySystem(UnitarySysNum)%HeatingCoilType_Num == CoilDX_MultiSpeedHeating .OR. &
               UnitarySystem(UnitarySysNum)%HeatingCoilType_Num == CoilDX_HeatingEmpirical)THEN
              UnitarySystem(UnitarySysNum)%HeatPump = .TRUE.
            END IF
          END IF

        ELSEIF (UnitarySystem(UnitarySysNum)%CoolingCoilType_Num == CoilDX_MultiSpeedCooling) THEN
          ErrFlag = .FALSE.
          CALL GetDXCoilIndex(CoolingCoilName,UnitarySystem(UnitarySysNum)%CoolingCoilIndex, &
                              ErrFlag, TRIM(CoolingCoilType))
          IF(ErrFlag) THEN
              CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
            ErrorsFound=.true.
            ErrFlag = .FALSE.
          END IF

          UnitarySystem(UnitarySysNum)%CoolingCoilAvailSchPtr = GetDXCoilAvailSchPtr(CoolingCoilType,CoolingCoilName,ErrFlag)

          CoolingCoilInletNode = GetDXCoilInletNode(CoolingCoilType,CoolingCoilName,ErrFlag)
          IF(ErrFlag) THEN
              CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
            ErrorsFound=.true.
            ErrFlag = .FALSE.
          END IF
          CoolingCoilOutletNode = GetDXCoilOutletNode(CoolingCoilType,CoolingCoilName,ErrFlag)
          IF(ErrFlag) THEN
              CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
            ErrorsFound=.true.
            ErrFlag = .FALSE.
          END IF

!          IF(UnitarySystem(UnitarySysNum)%DesignCoolingCapacity == Autosize) &
!               UnitarySystem(UnitarySysNum)%RequestAutosize = .TRUE.

          IF(UnitarySystem(UnitarySysNum)%HeatCoilExists)THEN
            IF(UnitarySystem(UnitarySysNum)%HeatingCoilType_Num == Coil_HeatingAirToAirVariableSpeed .OR. &
               UnitarySystem(UnitarySysNum)%HeatingCoilType_Num == Coil_HeatingWaterToAirHPVSEquationFit .OR. &
               UnitarySystem(UnitarySysNum)%HeatingCoilType_Num == Coil_HeatingWaterToAirHPSimple .OR. &
               UnitarySystem(UnitarySysNum)%HeatingCoilType_Num == CoilDX_MultiSpeedHeating .OR. &
               UnitarySystem(UnitarySysNum)%HeatingCoilType_Num == CoilDX_HeatingEmpirical)THEN
              UnitarySystem(UnitarySysNum)%HeatPump = .TRUE.
            END IF
          END IF

        ELSEIF (UnitarySystem(UnitarySysNum)%CoolingCoilType_Num == Coil_CoolingWater .OR. &
                UnitarySystem(UnitarySysNum)%CoolingCoilType_Num == Coil_CoolingWaterDetailed) THEN

          CALL ValidateComponent(CoolingCoilType,CoolingCoilName,IsNotOK,TRIM(CurrentModuleObject))
          IF (IsNotOK) THEN
              CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
              ErrorsFound=.TRUE.
          ELSE ! mine data from Cooling coil object

            ErrFlag = .FALSE.
            UnitarySystem(UnitarySysNum)%CoolingCoilAvailSchPtr = &
                 GetWaterCoilAvailScheduleIndex(CoolingCoilType,CoolingCoilName,ErrFlag)
            IF(ErrFlag)THEN
              CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
              ErrorsFound = .TRUE.
              ErrFlag = .FALSE.
            END IF

            UnitarySystem(UnitarySysNum)%CoolingCoilIndex = GetWaterCoilIndex(CoolingCoilType,CoolingCoilName,ErrFlag)
            IF (UnitarySystem(UnitarySysNum)%CoolingCoilIndex .EQ. 0) THEN
              CALL ShowSevereError(TRIM(CurrentModuleObject)//' illegal '//TRIM(cAlphaFields(iCoolingCoilNameAlphaNum))//' = ' &
                              //TRIM(HeatingCoilName))
              CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
              ErrorsFound = .TRUE.
              ErrFlag = .FALSE.
            END IF

            ! Get the Cooling Coil water Inlet Node number
            UnitarySystem(UnitarySysNum)%CoolCoilFluidInletNode = GetCoilWaterInletNode(CoolingCoilType,CoolingCoilName,ErrFlag)
            IF(ErrFlag)THEN
              CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
              ErrorsFound = .TRUE.
              ErrFlag = .FALSE.
            END IF

            InletNodeNotControlled = .TRUE.
          !  CALL CheckCoilWaterInletNode(UnitarySystem(UnitarySysNum)%CoolCoilFluidInletNode,InletNodeNotControlled)
            IF(.NOT. InletNodeNotControlled)THEN
              CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
              CALL ShowContinueError(TRIM(ControllerTypes(ControllerSimple_Type))//' found for '// &
                                     TRIM(CoolingCoilType)//' = "'//TRIM(CoolingCoilName)//'."')
              CALL ShowContinueError('...water coil controllers are not used with '// &
                                      TRIM(UnitarySystem(UnitarySysNum)%UnitarySystemType))
              ErrorsFound = .TRUE.
            END IF

            ! Get the Cooling Coil chilled water max volume flow rate
            ErrFlag = .FALSE.
            UnitarySystem(UnitarySysNum)%MaxCoolCoilFluidFlow = GetCoilMaxWaterFlowRate(CoolingCoilType,  &
                                           CoolingCoilName,ErrFlag)
            IF(UnitarySystem(UnitarySysNum)%MaxCoolCoilFluidFlow == Autosize) &
                 UnitarySystem(UnitarySysNum)%RequestAutosize = .TRUE.
            IF(ErrFlag)THEN
              CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
              ErrorsFound = .TRUE.
              ErrFlag = .FALSE.
            END IF

            ! Get the Cooling Coil Inlet Node
            CoolingCoilInletNode =  GetWaterCoilInletNode(CoolingCoilType,CoolingCoilName,ErrFlag)
            IF(ErrFlag)THEN
              CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
              ErrorsFound = .TRUE.
              ErrFlag = .FALSE.
            END IF

            ! Get the Cooling Coil Outlet Node
            CoolingCoilOutletNode = GetWaterCoilOutletNode(CoolingCoilType,CoolingCoilName,ErrFlag)
            IF(ErrFlag)THEN
              CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
              ErrorsFound = .TRUE.
              ErrFlag = .FALSE.
            END IF
          END IF
        ELSEIF (UnitarySystem(UnitarySysNum)%CoolingCoilType_Num == Coil_CoolingWaterToAirHPSimple) THEN
            CALL ValidateComponent(CoolingCoilType,CoolingCoilName,IsNotOK,TRIM(CurrentModuleObject))
            IF (IsNotOK) THEN
              CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
              ErrorsFound=.TRUE.
            ELSE ! mine data from Cooling coil object

                ErrFlag = .FALSE.
                UnitarySystem(UnitarySysNum)%CoolingCoilAvailSchPtr = ScheduleAlwaysOn
                IF(ErrFlag)THEN
                  CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
                  ErrorsFound = .TRUE.
                  ErrFlag = .FALSE.
                END IF

                UnitarySystem(UnitarySysNum)%CoolingCoilIndex =  GetWtoAHPSimpleCoilIndex(CoolingCoilType,CoolingCoilName,ErrFlag)
                IF (UnitarySystem(UnitarySysNum)%CoolingCoilIndex .EQ. 0) THEN
                  CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
                  CALL ShowContinueError('Illegal '//TRIM(cAlphaFields(iCoolingCoilNameAlphaNum))//' = ' &
                                  //TRIM(CoolingCoilName))
                  ErrorsFound = .TRUE.
                  ErrFlag = .FALSE.
                END IF

                ! Get the Cooling Coil Inlet Node
                ErrFlag = .FALSE.
                CoolingCoilInletNode = GetWtoAHPSimpleCoilInletNode(CoolingCoilType, CoolingCoilName,ErrFlag)
                IF(ErrFlag)THEN
                  CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
                  ErrorsFound = .TRUE.
                  ErrFlag = .FALSE.
                END IF

                ! Get the Cooling Coil Outlet Node
                CoolingCoilOutletNode = GetWtoAHPSimpleCoilOutletNode(CoolingCoilType, CoolingCoilName,ErrFlag)
                IF(ErrFlag)THEN
                  CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
                  ErrorsFound = .TRUE.
                  ErrFlag = .FALSE.
                END IF

!                IF(UnitarySystem(UnitarySysNum)%MaxCoolCoilFluidFlow == Autosize) &
!                     UnitarySystem(UnitarySysNum)%RequestAutosize = .TRUE.

            END IF

            IF(UnitarySystem(UnitarySysNum)%HeatCoilExists)THEN
              IF(UnitarySystem(UnitarySysNum)%HeatingCoilType_Num == Coil_HeatingAirToAirVariableSpeed .OR. &
                 UnitarySystem(UnitarySysNum)%HeatingCoilType_Num == Coil_HeatingWaterToAirHPVSEquationFit .OR. &
                 UnitarySystem(UnitarySysNum)%HeatingCoilType_Num == Coil_HeatingWaterToAirHPSimple .OR. &
                 UnitarySystem(UnitarySysNum)%HeatingCoilType_Num == CoilDX_MultiSpeedHeating .OR. &
                 UnitarySystem(UnitarySysNum)%HeatingCoilType_Num == CoilDX_HeatingEmpirical)THEN
                UnitarySystem(UnitarySysNum)%HeatPump = .TRUE.
              END IF
            END IF

        ELSEIF (UnitarySystem(UnitarySysNum)%CoolingCoilType_Num == Coil_CoolingWaterToAirHP) THEN
            CALL ValidateComponent(CoolingCoilType,CoolingCoilName,IsNotOK,TRIM(CurrentModuleObject))
            IF (IsNotOK) THEN
                CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
                ErrorsFound=.TRUE.
            ELSE ! mine data from Cooling coil object

                ErrFlag = .FALSE.
                UnitarySystem(UnitarySysNum)%CoolingCoilAvailSchPtr = ScheduleAlwaysOn
                IF(ErrFlag)THEN
                  CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
                  ErrorsFound = .TRUE.
                  ErrFlag = .FALSE.
                END IF

                UnitarySystem(UnitarySysNum)%CoolingCoilIndex =  GetWtoAHPCoilIndex(CoolingCoilType, CoolingCoilName,ErrFlag)
                IF (UnitarySystem(UnitarySysNum)%CoolingCoilIndex .EQ. 0) THEN
                  CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
                  CALL ShowContinueError('Illegal '//TRIM(cAlphaFields(iCoolingCoilNameAlphaNum))//' = ' &
                                  //TRIM(CoolingCoilName))
                  ErrorsFound = .TRUE.
                  ErrFlag = .FALSE.
                END IF

                ! Get the Cooling Coil Inlet Node
                ErrFlag = .FALSE.
                CoolingCoilInletNode = GetWtoAHPCoilInletNode(CoolingCoilType, CoolingCoilName,ErrFlag)
                IF(ErrFlag)THEN
                  CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
                  ErrorsFound = .TRUE.
                  ErrFlag = .FALSE.
                END IF

                ! Get the Cooling Coil Outlet Node
                CoolingCoilOutletNode = GetWtoAHPCoilOutletNode(CoolingCoilType, CoolingCoilName,ErrFlag)
                IF(ErrFlag)THEN
                  CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
                  ErrorsFound = .TRUE.
                  ErrFlag = .FALSE.
                END IF

!                IF(UnitarySystem(UnitarySysNum)%MaxCoolCoilFluidFlow == Autosize) &
!                     UnitarySystem(UnitarySysNum)%RequestAutosize = .TRUE.

            END IF

            IF(UnitarySystem(UnitarySysNum)%HeatCoilExists)THEN
              IF(UnitarySystem(UnitarySysNum)%HeatingCoilType_Num == Coil_HeatingAirToAirVariableSpeed .OR. &
                 UnitarySystem(UnitarySysNum)%HeatingCoilType_Num == Coil_HeatingWaterToAirHPVSEquationFit .OR. &
                 UnitarySystem(UnitarySysNum)%HeatingCoilType_Num == Coil_HeatingWaterToAirHPSimple .OR. &
                 UnitarySystem(UnitarySysNum)%HeatingCoilType_Num == CoilDX_MultiSpeedHeating .OR. &
                 UnitarySystem(UnitarySysNum)%HeatingCoilType_Num == CoilDX_HeatingEmpirical)THEN
                UnitarySystem(UnitarySysNum)%HeatPump = .TRUE.
              END IF
            END IF

        ELSE ! IF(.NOT. lAlphaBlanks(16))THEN
          CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
          CALL ShowContinueError('Illegal '//TRIM(cAlphaFields(iCoolingCoilTypeAlphaNum))//' = '// &
                                 TRIM(Alphas(iCoolingCoilTypeAlphaNum)))
          ErrorsFound=.TRUE.
        END IF

        IF(UnitarySystem(UnitarySysNum)%CoolingCoilType_Num == CoilDX_MultiSpeedCooling) THEN
          UnitarySystem(UnitarySysNum)%MultiSpeedCoolingCoil = .TRUE.
        ELSEIF(UnitarySystem(UnitarySysNum)%CoolingCoilType_Num == Coil_CoolingWaterToAirHPVSEquationFit .OR. &
           UnitarySystem(UnitarySysNum)%CoolingCoilType_Num == Coil_CoolingAirToAirVariableSpeed) THEN
          UnitarySystem(UnitarySysNum)%VarSpeedCoolingCoil = .TRUE.
        END IF

        IF(NodeHasSPMCtrlVarType(UnitarySystem(UnitarySysNum)%UnitarySystemOutletNodeNum, iCtrlVarType_Temp)) &
            UnitarySystem(UnitarySysNum)%SystemCoolControlNodeNum = UnitarySystem(UnitarySysNum)%UnitarySystemOutletNodeNum
        IF(NodeHasSPMCtrlVarType(CoolingCoilOutletNode, iCtrlVarType_Temp)) &
          UnitarySystem(UnitarySysNum)%SystemCoolControlNodeNum = CoolingCoilOutletNode

        UnitarySystem(UnitarySysNum)%CoolCoilInletNodeNum  = CoolingCoilInletNode
        UnitarySystem(UnitarySysNum)%CoolCoilOutletNodeNum = CoolingCoilOutletNode
        UnitarySystem(UnitarySysNum)%CoolingCoilName       = CoolingCoilName

        END IF

        IF (UnitarySystem(UnitarySysNum)%HeatingCoilType_Num == Coil_HeatingWaterToAirHPSimple .AND. &
            UnitarySystem(UnitarySysNum)%CoolingCoilType_Num == Coil_CoolingWaterToAirHPSimple) THEN
          UnitarySystem(UnitarySysNum)%WaterCyclingMode = WaterCycling
          CALL SetSimpleWSHPData(UnitarySystem(UnitarySysNum)%CoolingCoilIndex,ErrorsFound, &
                                 UnitarySystem(UnitarySysNum)%WaterCyclingMode, &
                                 CompanionHeatingCoilNum=UnitarySystem(UnitarySysNum)%HeatingCoilIndex)
        END IF

        IF (UnitarySystem(UnitarySysNum)%HeatingCoilType_Num == Coil_HeatingWaterToAirHPVSEquationFit .AND. &
            UnitarySystem(UnitarySysNum)%CoolingCoilType_Num == Coil_CoolingWaterToAirHPVSEquationFit) THEN
          CALL SetVarSpeedCoilData(UnitarySystem(UnitarySysNum)%CoolingCoilIndex,ErrorsFound, &
                                CompanionHeatingCoilNum=UnitarySystem(UnitarySysNum)%HeatingCoilIndex)
        END IF

        ! Add cooling coil to component sets array
        IF(UnitarySystem(UnitarySysNum)%CoolCoilExists) THEN
          IF(UnitarySystem(UnitarySysNum)%CoolingCoilType_Num .NE. CoilDX_MultiSpeedCooling) THEN
            CALL SetUpCompSets(CurrentModuleObject, Alphas(iNameAlphaNum), &
                               Alphas(iCoolingCoilTypeAlphaNum), Alphas(iCoolingCoilNameAlphaNum), &
                               NodeID(CoolingCoilInletNode), NodeID(CoolingCoilOutletNode))
          ELSE
            CALL SetUpCompSets(CurrentModuleObject, Alphas(iNameAlphaNum), &
                               Alphas(iCoolingCoilTypeAlphaNum), Alphas(iCoolingCoilNameAlphaNum), &
                               'UNDEFINED', 'UNDEFINED')
          END IF
        END IF
        ! Run as 100% DOAS DX coil
        IF (lAlphaBlanks(iDOASDXCoilAlphaNum) .AND. NumAlphas < iDOASDXCoilAlphaNum)THEN
            UnitarySystem(UnitarySysNum)%ISHundredPercentDOASDXCoil= .FALSE.
        ELSE
            IF (SameString(Alphas(iDOASDXCoilAlphaNum),'Yes')) THEN
              UnitarySystem(UnitarySysNum)%ISHundredPercentDOASDXCoil= .TRUE.
            ELSEIF (SameString(Alphas(iDOASDXCoilAlphaNum),' ')) THEN
              UnitarySystem(UnitarySysNum)%ISHundredPercentDOASDXCoil= .FALSE.
            ELSEIF (SameString(Alphas(iDOASDXCoilAlphaNum),'No')) THEN
              UnitarySystem(UnitarySysNum)%ISHundredPercentDOASDXCoil= .FALSE.
            ELSE
              CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
              CALL ShowContinueError('Invalid entry for '//TRIM(cAlphaFields(iDOASDXCoilAlphaNum))//' :'// &
                                     TRIM(Alphas(iDOASDXCoilAlphaNum)))
              CALL ShowContinueError('Must be Yes or No.')
            END IF
        END IF

        ! considered as as 100% DOAS DX cooling coil
        IF (UnitarySystem(UnitarySysNum)%ISHundredPercentDOASDXCoil) THEN
           ! set the system DX Coil application type to the child DX coil
           CALL SetDXCoilTypeData(UnitarySystem(UnitarySysNum)%CoolingCoilName)
        END IF
        ! DOAS DX Cooling Coil Leaving Minimum Air Temperature
        IF (NumNumbers > 0)THEN
          IF (.NOT. lNumericBlanks(iDOASDXMinTempNumericNum))THEN
             UnitarySystem(UnitarySysNum)%DOASDXCoolingCoilMinTout = Numbers(iDOASDXMinTempNumericNum)
          END IF
        END IF

        !Get Latent Load Control flag
        IF(.NOT. lAlphaBlanks(iRunOnLatentLoadAlphaNum))THEN
          IF(SameString(Alphas(iRunOnLatentLoadAlphaNum),'SensibleOnlyLoadControl')) THEN
            UnitarySystem(UnitarySysNum)%RunOnSensibleLoad = .TRUE.
            UnitarySystem(UnitarySysNum)%RunOnLatentLoad = .FALSE.
          ELSE IF(SameString(Alphas(iRunOnLatentLoadAlphaNum),'LatentOnlyLoadControl'))THEN
            UnitarySystem(UnitarySysNum)%RunOnSensibleLoad = .FALSE.
            UnitarySystem(UnitarySysNum)%RunOnLatentLoad = .TRUE.
          ELSE IF(SameString(Alphas(iRunOnLatentLoadAlphaNum),'LatentOrSensibleLoadControl'))THEN
            UnitarySystem(UnitarySysNum)%RunOnSensibleLoad = .TRUE.
            UnitarySystem(UnitarySysNum)%RunOnLatentLoad = .TRUE.
          ELSE IF(SameString(Alphas(iRunOnLatentLoadAlphaNum),'LatentWithSensibleLoadControl'))THEN
            UnitarySystem(UnitarySysNum)%RunOnSensibleLoad = .TRUE.
            UnitarySystem(UnitarySysNum)%RunOnLatentLoad = .TRUE.
            UnitarySystem(UnitarySysNum)%RunOnLatentOnlyWithSensible = .TRUE.
          ELSE
            CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
            CALL ShowContinueError('Invalid entry for '//TRIM(cAlphaFields(iRunOnLatentLoadAlphaNum))//' :'// &
                                   TRIM(Alphas(iRunOnLatentLoadAlphaNum)))
            CALL ShowContinueError('Must be SensibleOnlyLoadControl, LatentOnlyLoadControl,'// &
                                   ' LatentOrSensibleLoadControl, or LatentWithSensibleLoadControl.')
          END IF
        END IF

        !Get reheat coil data if humidistat is used
        SuppHeatCoilType = Alphas(iSuppHeatCoilTypeAlphaNum)
        SuppHeatCoilName = Alphas(iSuppHeatCoilNameAlphaNum)
        UnitarySystem(UnitarySysNum)%SuppHeatCoilName = SuppHeatCoilName
        ErrFlag = .FALSE.

        IF (SameString(SuppHeatCoilType,'Coil:Heating:Water')) THEN
          UnitarySystem(UnitarySysNum)%SuppHeatCoilType_Num = Coil_HeatingWater
        ELSEIF (SameString(SuppHeatCoilType,'Coil:Heating:Steam')) THEN
          UnitarySystem(UnitarySysNum)%SuppHeatCoilType_Num = Coil_HeatingSteam
        ELSEIF (SameString(SuppHeatCoilType,'Coil:Heating:Gas')          .OR. &
                SameString(SuppHeatCoilType,'Coil:Heating:Electric')     .OR. &
                SameString(SuppHeatCoilType,'Coil:Heating:DesuperHeater')) THEN
            UnitarySystem(UnitarySysNum)%SuppHeatCoilType_Num = GetHeatingCoilTypeNum(SuppHeatCoilType,SuppHeatCoilName,ErrFlag)
        END IF

        IF(.NOT. lAlphaBlanks(iSuppHeatCoilTypeAlphaNum))THEN
          UnitarySystem(UnitarySysNum)%SuppCoilExists = .TRUE.

          IF (UnitarySystem(UnitarySysNum)%SuppHeatCoilType_Num == Coil_HeatingGas          .OR. &
              UnitarySystem(UnitarySysNum)%SuppHeatCoilType_Num == Coil_HeatingElectric     .OR. &
              UnitarySystem(UnitarySysNum)%SuppHeatCoilType_Num == Coil_HeatingDesuperheater) THEN

            UnitarySystem(UnitarySysNum)%SuppHeatCoilType_Num = GetHeatingCoilTypeNum(SuppHeatCoilType,SuppHeatCoilName,ErrFlag)
            IF (ErrFlag) THEN
               CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
               ErrorsFound=.TRUE.
            ELSE

              CALL ValidateComponent(SuppHeatCoilType,SuppHeatCoilName,IsNotOK,  &
                                   TRIM(CurrentModuleObject))
              IF (IsNotOK) THEN
                  CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
                ErrorsFound=.TRUE.

              ELSE ! mine data from reheat coil

                ! Get the heating coil index
                CALL GetHeatingCoilIndex(SuppHeatCoilName,UnitarySystem(UnitarySysNum)%SuppHeatCoilIndex,IsNotOK)
                IF (IsNotOK) THEN
                  CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
                  ErrorsFound=.TRUE.
                END IF

                ! Get the design supplemental heating capacity
                ErrFlag=.FALSE.
                UnitarySystem(UnitarySysNum)%DesignSuppHeatingCapacity =   &
                    GetHeatingCoilCapacity(SuppHeatCoilType,SuppHeatCoilName,ErrFlag)
                IF(UnitarySystem(UnitarySysNum)%DesignSuppHeatingCapacity == Autosize) &
                     UnitarySystem(UnitarySysNum)%RequestAutosize = .TRUE.

                IF (ErrFlag) THEN
                  CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
                  ErrorsFound=.TRUE.
                END IF

                ! Get the Reheat Coil Inlet Node
                ErrFlag=.FALSE.
                SupHeatCoilInletNode = GetHeatingCoilInletNode(SuppHeatCoilType,SuppHeatCoilName,ErrFlag)
                IF (ErrFlag) THEN
                  CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
                  ErrorsFound=.TRUE.
                END IF

                ! Get the Reheat Coil Outlet Node
                ErrFlag=.FALSE.
                SupHeatCoilOutletNode = GetHeatingCoilOutletNode(SuppHeatCoilType,SuppHeatCoilName,ErrFlag)
                IF (ErrFlag) THEN
                  CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
                  ErrorsFound=.TRUE.
                END IF

              END IF  ! IF (IsNotOK) THEN
            END IF

            UnitarySystem(UnitarySysNum)%SuppCoilAirInletNode = SupHeatCoilInletNode
            UnitarySystem(UnitarySysNum)%SuppCoilAirOutletNode = SupHeatCoilOutletNode

          ELSEIF (UnitarySystem(UnitarySysNum)%SuppHeatCoilType_Num == Coil_HeatingWater) THEN

            CALL ValidateComponent(SuppHeatCoilType,SuppHeatCoilName,IsNotOK,TRIM(CurrentModuleObject))
            IF (IsNotOK) THEN
              CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
              ErrorsFound=.TRUE.
            ELSE ! mine data from heating coil object

                ! Get the Heating Coil water Inlet or control Node number
                ErrFlag = .FALSE.
                UnitarySystem(UnitarySysNum)%SuppCoilFluidInletNode = &
                      GetCoilWaterInletNode('Coil:Heating:Water',SuppHeatCoilName,ErrFlag)
                IF(ErrFlag)THEN
                  CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
                  ErrorsFound = .TRUE.
                END IF

                ! Get the ReHeat Coil hot water max volume flow rate
                ErrFlag = .FALSE.
                UnitarySystem(UnitarySysNum)%MaxSuppCoilFluidFlow = GetCoilMaxWaterFlowRate('Coil:Heating:Water',  &
                                                           SuppHeatCoilName,ErrFlag)
                IF(UnitarySystem(UnitarySysNum)%MaxSuppCoilFluidFlow == Autosize) &
                     UnitarySystem(UnitarySysNum)%RequestAutosize = .TRUE.

                IF(ErrFlag)THEN
                  CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
                  ErrorsFound = .TRUE.
                END IF

                ! Get the ReHeat Coil Inlet Node
                ErrFlag = .FALSE.
                SupHeatCoilInletNode =  GetWaterCoilInletNode('Coil:Heating:Water',SuppHeatCoilName,ErrFlag)
                UnitarySystem(UnitarySysNum)%SuppCoilAirInletNode = SupHeatCoilInletNode
                IF(ErrFlag)THEN
                  CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
                  ErrorsFound = .TRUE.
                END IF

                ! Get the ReHeat Coil Outlet Node
                ErrFlag = .FALSE.
                SupHeatCoilOutletNode = GetWaterCoilOutletNode('Coil:Heating:Water',SuppHeatCoilName,ErrFlag)
                UnitarySystem(UnitarySysNum)%SuppCoilAirOutletNode = SupHeatCoilOutletNode
                IF(ErrFlag)THEN
                  CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
                  ErrorsFound = .TRUE.
                END IF

!                IF(UnitarySystem(UnitarySysNum)%MaxSuppCoilFluidFlow == Autosize) &
!                   UnitarySystem(UnitarySysNum)%RequestAutosize = .TRUE.
!                IF(UnitarySystem(UnitarySysNum)%DesignSuppHeatingCapacity == Autosize) &
!                   UnitarySystem(UnitarySysNum)%RequestAutosize = .TRUE.

            END IF

          ELSEIF (UnitarySystem(UnitarySysNum)%SuppHeatCoilType_Num == Coil_HeatingSteam) THEN

            CALL ValidateComponent(SuppHeatCoilType,SuppHeatCoilName,IsNotOK,TRIM(CurrentModuleObject))
            IF (IsNotOK) THEN
              CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
              ErrorsFound=.TRUE.
            ELSE ! mine data from heating coil object

                ErrFlag = .FALSE.
                UnitarySystem(UnitarySysNum)%SuppHeatCoilIndex = GetSTeamCoilIndex('COIL:HEATING:STEAM',SuppHeatCoilName,ErrFlag)
                IF (UnitarySystem(UnitarySysNum)%SuppHeatCoilIndex .EQ. 0) THEN
                    CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
                    CALL ShowSevereError('Illegal '//TRIM(cAlphaFields(iSuppHeatCoilNameAlphaNum))//' = ' &
                                  //TRIM(SuppHeatCoilName))
                    ErrorsFound = .TRUE.
                END IF

                ! Get the Heating Coil steam inlet node number
                ErrFlag = .FALSE.
                UnitarySystem(UnitarySysNum)%SuppCoilFluidInletNode = &
                    GetSteamCoilSteamInletNode('Coil:Heating:Steam',SuppHeatCoilName,ErrFlag)
                IF(ErrFlag)THEN
                  CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
                  ErrorsFound = .TRUE.
                END IF

                ! Get the Heating Coil steam max volume flow rate
                UnitarySystem(UnitarySysNum)%MaxSuppCoilFluidFlow = &
                    GetCoilMaxSteamFlowRate(UnitarySystem(UnitarySysNum)%SuppHeatCoilIndex,ErrFlag)
                IF(UnitarySystem(UnitarySysNum)%MaxSuppCoilFluidFlow == Autosize) &
                   UnitarySystem(UnitarySysNum)%RequestAutosize = .TRUE.

                IF (UnitarySystem(UnitarySysNum)%MaxSuppCoilFluidFlow .GT. 0.0d0)THEN
                   SteamIndex = 0      ! Function GetSatDensityRefrig will look up steam index if 0 is passed
                   SteamDensity=GetSatDensityRefrig("STEAM",TempSteamIn,1.0d0,SteamIndex,'GetAirLoopHVACHeatCoolInput')
                   UnitarySystem(UnitarySysNum)%MaxSuppCoilFluidFlow = &
                                GetCoilMaxSteamFlowRate(UnitarySystem(UnitarySysNum)%SuppHeatCoilIndex,ErrFlag) * SteamDensity
                END IF

                ! Get the Heating Coil Inlet Node
                ErrFlag = .FALSE.
                SupHeatCoilInletNode = &
                         GetSteamCoilAirInletNode(UnitarySystem(UnitarySysNum)%SuppHeatCoilIndex,SuppHeatCoilName,ErrFlag)
                UnitarySystem(UnitarySysNum)%SuppCoilAirInletNode = SupHeatCoilInletNode
                IF(ErrFlag)THEN
                  CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
                  ErrorsFound = .TRUE.
                END IF

                ! Get the Heating Coil Outlet Node
                ErrFlag = .FALSE.
                SupHeatCoilOutletNode = &
                         GetSteamCoilAirOutletNode(UnitarySystem(UnitarySysNum)%SuppHeatCoilIndex,SuppHeatCoilName,ErrFlag)
                UnitarySystem(UnitarySysNum)%SuppCoilAirOutletNode = SupHeatCoilOutletNode
                IF(ErrFlag)THEN
                  CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
                  ErrorsFound = .TRUE.
                END IF

!                IF(UnitarySystem(UnitarySysNum)%DesignSuppHeatingCapacity == Autosize) &
!                   UnitarySystem(UnitarySysNum)%RequestAutosize = .TRUE.

            END IF

          ELSE  ! Illegal reheating coil type
            CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
            CALL ShowContinueError('Illegal '//TRIM(cAlphaFields(iSuppHeatCoilTypeAlphaNum))//' = '// &
                                   TRIM(Alphas(iSuppHeatCoilTypeAlphaNum)))
            ErrorsFound=.TRUE.
          END IF ! IF (UnitarySystem(UnitarySysNum)%SuppHeatCoilType_Num == Coil_HeatingGas .OR. &, etc.

        END IF ! IF(.NOT. lAlphaBlanks(iSuppHeatCoilTypeAlphaNum))THEN

        IF(NodeHasSPMCtrlVarType(UnitarySystem(UnitarySysNum)%UnitarySystemOutletNodeNum, iCtrlVarType_Temp)) &
            UnitarySystem(UnitarySysNum)%SuppHeatControlNodeNum = UnitarySystem(UnitarySysNum)%UnitarySystemOutletNodeNum
        IF(NodeHasSPMCtrlVarType(SupHeatCoilOutletNode, iCtrlVarType_Temp)) &
          UnitarySystem(UnitarySysNum)%SuppHeatControlNodeNum = SupHeatCoilOutletNode

        ! Add supplemental heating coil to component sets array
        IF(UnitarySystem(UnitarySysNum)%SuppCoilExists)CALL SetUpCompSets(CurrentModuleObject, Alphas(iNameAlphaNum), &
                             Alphas(iSuppHeatCoilTypeAlphaNum),Alphas(iSuppHeatCoilNameAlphaNum), &
                             NodeID(SupHeatCoilInletNode),NodeID(SupHeatCoilOutletNode))

        TotalZonesOnAirLoop = 0
        TotalFloorAreaOnAirLoop = 0.0d0
        AirLoopNumber = 0

 !***... only need to do this for load based control?
        ! Get the node number for the zone with the thermostat ! what if ControlZoneNum does = 0 if it's an OA sys?
        IF (UnitarySystem(UnitarySysNum)%ControlZoneNum >  0) THEN
          AirNodeFound=.FALSE.
          AirLoopFound=.FALSE.
          DO ControlledZoneNum = 1,NumOfZones
            IF (ZoneEquipConfig(ControlledZoneNum)%ActualZoneNum /= UnitarySystem(UnitarySysNum)%ControlZoneNum) CYCLE
!             Find the controlled zone number for the specified thermostat location
              UnitarySystem(UnitarySysNum)%NodeNumofControlledZone=ZoneEquipConfig(ControlledZoneNum)%ZoneNode
!             Determine if system is on air loop served by the thermostat location specified
              AirLoopNumber = ZoneEquipConfig(ControlledZoneNum)%AirLoopNum
              IF(AirLoopNumber .GT. 0)THEN
                DO BranchNum = 1, PrimaryAirSystem(AirLoopNumber)%NumBranches
                  DO CompNum = 1, PrimaryAirSystem(AirLoopNumber)%Branch(BranchNum)%TotalComponents
                    IF(.NOT. SameString(PrimaryAirSystem(AirLoopNumber)%Branch(BranchNum)%Comp(CompNum)%Name, &
                             Alphas(iNameAlphaNum)) .OR. &
                       .NOT. SameString(PrimaryAirSystem(AirLoopNumber)%Branch(BranchNum)%Comp(CompNum)%TypeOf, &
                             CurrentModuleObject))CYCLE
                    AirLoopFound=.TRUE.
                    EXIT
                  END DO
                  IF(AirLoopFound)EXIT
                END DO
                DO TstatZoneNum = 1, NumTempControlledZones
                  IF(TempControlledZone(TstatZoneNum)%ActualZoneNum .NE. UnitarySystem(UnitarySysNum)%ControlZoneNum)CYCLE
                  AirNodeFound=.TRUE.
                END DO
                DO TstatZoneNum = 1, NumComfortControlledZones
                  IF(ComfortControlledZone(TstatZoneNum)%ActualZoneNum .NE. UnitarySystem(UnitarySysNum)%ControlZoneNum)CYCLE
                  AirNodeFound=.TRUE.
                END DO
              END IF
            EXIT
          END DO
          IF(AirLoopNumber .GT. 0)THEN
            DO ControlledZoneNum = 1,NumOfZones
              IF (ZoneEquipConfig(ControlledZoneNum)%AirLoopNum == AirLoopNumber) THEN
                TotalZonesOnAirLoop = TotalZonesOnAirLoop + 1
                TotalFloorAreaOnAirLoop = TotalFloorAreaOnAirLoop+Zone(ZoneEquipConfig(ControlledZoneNum)%ActualZoneNum)%FloorArea
              END IF
            END DO
          ELSE
            IF (CurOASysNum > 0) THEN
!            IF(ALLOCATED(OutsideAirSys))THEN
              DO OASysNum = 1, NumOASystems
                DO OACompNum = 1, OutsideAirSys(OASysNum)%NumComponents
                  IF(.NOT. SameString(OutsideAirSys(OASysNum)%ComponentName(OACompNum), &
                           Alphas(iNameAlphaNum)) .OR. &
                     .NOT. SameString(OutsideAirSys(OASysNum)%ComponentType(OACompNum), &
                           CurrentModuleObject))CYCLE
                  AirLoopFound=.TRUE.
                  AirLoopNumber = OASysNum
                  UnitarySystem(UnitarySysNum)%AirLoopEquipment = .FALSE.
                  EXIT
                END DO
                IF(AirLoopFound)EXIT
              END DO
            END IF
          END IF
        ELSE ! this works IF it's zone equipment, but what IF it's air loop equipment?
          AirLoopScan: DO AirLoopNum = 1, NumPrimaryAirSys
            BranchScan: DO BranchNum = 1, PrimaryAirSystem(AirLoopNum)%NumBranches
              CompScan: DO CompNum = 1, PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%TotalComponents
                IF(SameString(PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%Name, Alphas(iNameAlphaNum)) .OR. &
                   SameString(PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%TypeOf, CurrentModuleObject))THEN
                  AirLoopNumber = AirLoopNum
                  AirLoopFound=.TRUE.
                ELSE IF(PrimaryAirSystem(AirLoopNum)%OASysExists)THEN
                  IF(ALLOCATED(OutsideAirSys))THEN
                    OASysScan: DO OASysNum = 1, NumOASystems
                      IF(.NOT. SameString(OutsideAirSys(OASysNum)%Name, &
                               PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%Name) .OR. &
                         .NOT. SameString('AirloopHVAC:OutdoorAirSystem', &
                               PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%TypeOf))CYCLE
                      DO OACompNum = 1, OutsideAirSys(OASysNum)%NumComponents
                        IF(.NOT. SameString(OutsideAirSys(OASysNum)%ComponentName(OACompNum), &
                                 Alphas(iNameAlphaNum)) .OR. &
                           .NOT. SameString(OutsideAirSys(OASysNum)%ComponentType(OACompNum), &
                                 CurrentModuleObject))CYCLE
                        AirLoopFound=.TRUE.
                        AirLoopNumber = AirLoopNum
                        UnitarySystem(UnitarySysNum)%AirLoopEquipment = .FALSE.
                        EXIT
                      END DO
!                      IF(AirLoopFound)EXIT OASysScan  WHY aren't these working? I get a break here in the debugger
                    END DO OASysScan
                  END IF
                END IF
!                IF(AirLoopFound)EXIT CompScan
              END DO CompScan
!              IF(AirLoopFound)EXIT BranchScan
            END DO BranchScan
!            IF(AirLoopFound)EXIT AirLoopScan
          END DO AirLoopScan
          IF(.NOT. AirLoopFound)THEN
            DO ControlledZoneNum = 1,NumOfZones
              DO ZoneExhNum = 1, ZoneEquipConfig(ControlledZoneNum)%NumExhaustNodes
                IF (ZoneEquipConfig(ControlledZoneNum)%ExhaustNode(ZoneExhNum) /= &
                    UnitarySystem(UnitarySysNum)%UnitarySystemInletNodeNum) CYCLE
!               Find the controlled zone number for the specified thermostat location
                UnitarySystem(UnitarySysNum)%NodeNumofControlledZone=ZoneEquipConfig(ControlledZoneNum)%ZoneNode
                UnitarySystem(UnitarySysNum)%ControlZoneNum = ControlledZoneNum
                TotalFloorAreaOnAirLoop = Zone(ZoneEquipConfig(ControlledZoneNum)%ActualZoneNum)%FloorArea
                UnitarySystem(UnitarySysNum)%AirLoopEquipment = .FALSE.
                UnitarySystem(UnitarySysNum)%ZoneInletNode = ZoneEquipConfig(ControlledZoneNum)%ExhaustNode(ZoneExhNum)
                IF (ZoneEquipConfig(ControlledZoneNum)%EquipListIndex > 0) THEN
                  DO EquipNum = 1, ZoneEquipList(ZoneEquipConfig(ControlledZoneNum)%EquipListIndex)%NumOfEquipTypes
                    IF ((ZoneEquipList(ZoneEquipConfig(ControlledZoneNum)%EquipListIndex)%EquipType_Num(EquipNum) /= &
                         ZoneUnitarySystem_Num).OR. &
                       ZoneEquipList(ZoneEquipConfig(ControlledZoneNum)%EquipListIndex)%EquipName(EquipNum) /= &
                         UnitarySystem(UnitarySysNum)%Name)CYCLE
                      UnitarySystem(UnitarySysNum)%ZoneSequenceCoolingNum = &
                            ZoneEquipList(ZoneEquipConfig(ControlledZoneNum)%EquipListIndex)%CoolingPriority(EquipNum)
                      UnitarySystem(UnitarySysNum)%ZoneSequenceHeatingNum = &
                            ZoneEquipList(ZoneEquipConfig(ControlledZoneNum)%EquipListIndex)%HeatingPriority(EquipNum)
                  END DO
                END IF
                EXIT
              END DO
              DO ZoneInletNum = 1, ZoneEquipConfig(ControlledZoneNum)%NumInletNodes
                IF (ZoneEquipConfig(ControlledZoneNum)%InletNode(ZoneInletNum) /= &
                      UnitarySystem(UnitarySysNum)%UnitarySystemOutletNodeNum) CYCLE
                UnitarySystem(UnitarySysNum)%AirLoopEquipment = .FALSE.
                EXIT
              END DO
            END DO
          ELSE
            DO ControlledZoneNum = 1,NumOfZones
              IF (ZoneEquipConfig(ControlledZoneNum)%AirLoopNum == AirLoopNumber) THEN
                TotalZonesOnAirLoop = TotalZonesOnAirLoop + 1
                TotalFloorAreaOnAirLoop = TotalFloorAreaOnAirLoop+Zone(ZoneEquipConfig(ControlledZoneNum)%ActualZoneNum)%FloorArea
              END IF
            END DO
          END IF
        END IF

        IF (UnitarySystem(UnitarySysNum)%ControlZoneNum == 0 .AND. &
            UnitarySystem(UnitarySysNum)%ControlType == LoadBased) THEN
          CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
          CALL ShowContinueError('Illegal '//TRIM(cAlphaFields(iControlZoneAlphaNum))//' = '//TRIM(Alphas(iControlZoneAlphaNum)))
          ErrorsFound=.TRUE.
        END IF

        ! if a user connects the Unitary System as zone equipment, try to find the connection
        ZoneEquipmentFound=.FALSE.
        IF(UnitarySystem(UnitarySysNum)%AirLoopEquipment .AND. .NOT. AirLoopFound)THEN
          IF(UnitarySystem(UnitarySysNum)%ControlZoneNum > 0)THEN
            DO ControlledZoneNum = 1,NumOfZones
              DO ZoneExhNum = 1, ZoneEquipConfig(ControlledZoneNum)%NumExhaustNodes
                IF (ZoneEquipConfig(ControlledZoneNum)%ExhaustNode(ZoneExhNum) /= &
                    UnitarySystem(UnitarySysNum)%UnitarySystemInletNodeNum) CYCLE
                IF(UnitarySystem(UnitarySysNum)%ControlZoneNum /= ControlledZoneNum)THEN
                  CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
                  CALL ShowContinueError('Did not find Air Node (Zone with Thermostat).')
                  CALL ShowContinueError('specified '//TRIM(cAlphaFields(iControlZoneAlphaNum))//' = '// &
                                       TRIM(Alphas(iControlZoneAlphaNum)))
                  ErrorsFound=.TRUE.
                END IF
                IF(UnitarySystem(UnitarySysNum)%ControlZoneNum == ControlledZoneNum)THEN
                  ZoneEquipmentFound = .TRUE.
                  UnitarySystem(UnitarySysNum)%AirLoopEquipment = .FALSE.
                  UnitarySystem(UnitarySysNum)%ZoneInletNode = ZoneEquipConfig(ControlledZoneNum)%ExhaustNode(ZoneExhNum)
                END IF
              END DO
            END DO
          END IF
          IF(AirLoopNumber == 0 .AND. .NOT. ZoneEquipmentFound .AND. UnitarySystem(UnitarySysNum)%ControlType == LoadBased)THEN
            CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
            CALL ShowContinueError('Did not find an AirLoopHVAC.')
            CALL ShowContinueError('specified '//TRIM(cAlphaFields(iControlZoneAlphaNum))//' = '// &
                                   TRIM(Alphas(iControlZoneAlphaNum)))
            IF (.not. AirNodeFound .AND. .NOT. ZoneEquipmentFound .AND. UnitarySystem(UnitarySysNum)%ControlType == LoadBased)THEN
              CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
              CALL ShowContinueError('Did not find air node (zone with thermostat).')
              CALL ShowContinueError('specified '//TRIM(cAlphaFields(iControlZoneAlphaNum))//' = '// &
                                   TRIM(Alphas(iControlZoneAlphaNum)))
              CALL ShowContinueError('Both a ZoneHVAC:EquipmentConnections object and a ZoneControl:Thermostat object' &
                                //' must be specified for this zone.')
            END IF
            IF (.not. AirLoopFound .AND. .NOT. ZoneEquipmentFound .AND. UnitarySystem(UnitarySysNum)%ControlType == LoadBased)THEN
              CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
              CALL ShowSevereError('Did not find correct AirLoopHVAC.')
              CALL ShowContinueError('specified '//TRIM(cAlphaFields(iControlZoneAlphaNum))//' = '// &
                                   TRIM(Alphas(iControlZoneAlphaNum)))
            END IF
            IF(UnitarySystem(UnitarySysNum)%ControlType == LoadBased)ErrorsFound=.TRUE.
          END IF
        END IF

        IF(.NOT. ZoneEquipmentFound)CALL TestCompSet(CurrentModuleObject,Alphas(iNameAlphaNum), &
              Alphas(iAirInletNodeNameAlphaNum),Alphas(iAirOutletNodeNameAlphaNum),'Air Nodes')

        ! Determine supply air flow rate sizing method for cooling mode
        IF(SameString(Alphas(iCoolSAFMAlphaNum),'SupplyAirFlowRate'))THEN
          UnitarySystem(UnitarySysNum)%CoolingSAFMethod    = SupplyAirFlowRate

          IF (.NOT. lNumericBlanks(iMaxCoolAirVolFlowNumericNum)) THEN
            UnitarySystem(UnitarySysNum)%MaxCoolAirVolFlow   = Numbers(iMaxCoolAirVolFlowNumericNum)
            IF(UnitarySystem(UnitarySysNum)%MaxCoolAirVolFlow == Autosize) &
                 UnitarySystem(UnitarySysNum)%RequestAutosize = .TRUE.

            IF (UnitarySystem(UnitarySysNum)%MaxCoolAirVolFlow .LE. 0.0d0 .AND. &
                UnitarySystem(UnitarySysNum)%MaxCoolAirVolFlow .NE. AutoSize) THEN
              CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
              CALL ShowContinueError('Illegal '//TRIM(cNumericFields(iMaxCoolAirVolFlowNumericNum))//' = '// &
                                     TRIM(TrimSigDigits(Numbers(iMaxCoolAirVolFlowNumericNum),7)))
              ErrorsFound = .TRUE.
            END IF
          ELSE
            CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
            CALL ShowContinueError('Input for '//TRIM(cAlphaFields(iCoolSAFMAlphaNum))//' = '//TRIM(Alphas(iCoolSAFMAlphaNum)))
            CALL ShowContinueError('Blank field not allowed for '//TRIM(cNumericFields(iMaxCoolAirVolFlowNumericNum)))
            ErrorsFound = .TRUE.
          END IF
        ELSEIF(SameString(Alphas(iCoolSAFMAlphaNum),'FlowPerFloorArea'))THEN

          UnitarySystem(UnitarySysNum)%CoolingSAFMethod    = FlowPerFloorArea
          IF (.NOT. lNumericBlanks(iCoolFlowPerFloorAreaNumericNum)) THEN
            UnitarySystem(UnitarySysNum)%MaxCoolAirVolFlow   = Numbers(iCoolFlowPerFloorAreaNumericNum)
            IF (UnitarySystem(UnitarySysNum)%MaxCoolAirVolFlow .LE. 0.0d0 .AND. &
                UnitarySystem(UnitarySysNum)%MaxCoolAirVolFlow .NE. AutoSize) THEN
              CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
              CALL ShowContinueError('Input for '//TRIM(cAlphaFields(iCoolSAFMAlphaNum))//' = '//TRIM(Alphas(iCoolSAFMAlphaNum)))
              CALL ShowContinueError('Illegal '//TRIM(cNumericFields(iCoolFlowPerFloorAreaNumericNum))//' = '// &
                                     TRIM(TrimSigDigits(Numbers(iCoolFlowPerFloorAreaNumericNum),7)))
              ErrorsFound = .TRUE.
            ! Autosized input is not allowed
            ELSEIF(UnitarySystem(UnitarySysNum)%MaxCoolAirVolFlow .EQ. AutoSize) THEN
              CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
              CALL ShowContinueError('Input for '//TRIM(cAlphaFields(iCoolSAFMAlphaNum))//' = '//TRIM(Alphas(iCoolSAFMAlphaNum)))
              CALL ShowContinueError('Illegal '//TRIM(cNumericFields(iCoolFlowPerFloorAreaNumericNum))//' = Autosize')
              ErrorsFound = .TRUE.
            ELSE
              UnitarySystem(UnitarySysNum)%MaxCoolAirVolFlow = &
                UnitarySystem(UnitarySysNum)%MaxCoolAirVolFlow * TotalFloorAreaOnAirLoop
              UnitarySystem(UnitarySysNum)%RequestAutosize = .TRUE.
            END IF
          ELSE
            CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
            CALL ShowContinueError('Input for '//TRIM(cAlphaFields(iCoolSAFMAlphaNum))//' = '//TRIM(Alphas(iCoolSAFMAlphaNum)))
            CALL ShowContinueError('Blank field not allowed for '//TRIM(cNumericFields(iCoolFlowPerFloorAreaNumericNum)))
            ErrorsFound = .TRUE.
          END IF
        ELSEIF(SameString(Alphas(iCoolSAFMAlphaNum),'FractionOfAutosizedCoolingValue'))THEN

          UnitarySystem(UnitarySysNum)%CoolingSAFMethod  = FractionOfAutosizedCoolingValue
          IF (.NOT. lNumericBlanks(iCoolFlowPerFracCoolNumericNum)) THEN
            UnitarySystem(UnitarySysNum)%MaxCoolAirVolFlow = Numbers(iCoolFlowPerFracCoolNumericNum)
            IF (UnitarySystem(UnitarySysNum)%MaxCoolAirVolFlow .LE. 0.0d0 .AND. &
                UnitarySystem(UnitarySysNum)%MaxCoolAirVolFlow .NE. AutoSize) THEN
              CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
              CALL ShowContinueError('Input for '//TRIM(cAlphaFields(iCoolSAFMAlphaNum))//' = '//TRIM(Alphas(iCoolSAFMAlphaNum)))
              CALL ShowContinueError('Illegal '//TRIM(cNumericFields(iCoolFlowPerFracCoolNumericNum))//' = '// &
                                     TRIM(TrimSigDigits(Numbers(iCoolFlowPerFracCoolNumericNum),7)))
              ErrorsFound = .TRUE.
            ! Autosized input is not allowed
            ELSEIF(UnitarySystem(UnitarySysNum)%MaxCoolAirVolFlow .EQ. AutoSize) THEN
              CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
              CALL ShowContinueError('Input for '//TRIM(cAlphaFields(iCoolSAFMAlphaNum))//' = '//TRIM(Alphas(iCoolSAFMAlphaNum)))
              CALL ShowContinueError('Illegal '//TRIM(cNumericFields(iCoolFlowPerFracCoolNumericNum))//' = Autosize')
              ErrorsFound = .TRUE.
            ELSE
              UnitarySystem(UnitarySysNum)%RequestAutosize = .TRUE.
            END IF
          ELSE
            CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
            CALL ShowContinueError('Input for '//TRIM(cAlphaFields(iCoolSAFMAlphaNum))//' = '//TRIM(Alphas(iCoolSAFMAlphaNum)))
            CALL ShowContinueError('Blank field not allowed for '//TRIM(cNumericFields(iCoolFlowPerFracCoolNumericNum)))
            ErrorsFound = .TRUE.
          END IF
        ELSEIF(SameString(Alphas(iCoolSAFMAlphaNum),'FlowPerCoolingCapacity'))THEN

          UnitarySystem(UnitarySysNum)%CoolingSAFMethod  = FlowPerCoolingCapacity
          IF (.NOT. lNumericBlanks(iCoolFlowPerCoolCapNumericNum)) THEN
            UnitarySystem(UnitarySysNum)%MaxCoolAirVolFlow = Numbers(iCoolFlowPerCoolCapNumericNum)
            IF (UnitarySystem(UnitarySysNum)%MaxCoolAirVolFlow .LE. 0.d0 .AND. &
                UnitarySystem(UnitarySysNum)%MaxCoolAirVolFlow .NE. AutoSize) THEN
              CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
              CALL ShowContinueError('Input for '//TRIM(cAlphaFields(iCoolSAFMAlphaNum))//' = '//TRIM(Alphas(iCoolSAFMAlphaNum)))
              CALL ShowContinueError('Illegal '//TRIM(cNumericFields(iCoolFlowPerCoolCapNumericNum))//' = '// &
                                     TRIM(TrimSigDigits(Numbers(iCoolFlowPerCoolCapNumericNum),7)))
              ErrorsFound = .TRUE.
            ! Autosized input is not allowed
            ELSEIF(UnitarySystem(UnitarySysNum)%MaxCoolAirVolFlow .EQ. AutoSize) THEN
              CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
              CALL ShowContinueError('Input for '//TRIM(cAlphaFields(iCoolSAFMAlphaNum))//' = '//TRIM(Alphas(iCoolSAFMAlphaNum)))
              CALL ShowContinueError('Illegal '//TRIM(cNumericFields(iCoolFlowPerCoolCapNumericNum))//' = Autosize')
              ErrorsFound = .TRUE.
            ELSE
              UnitarySystem(UnitarySysNum)%RequestAutosize = .TRUE.
            END IF
          ELSE
            CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
            CALL ShowContinueError('Input for '//TRIM(cAlphaFields(iCoolSAFMAlphaNum))//' = '//TRIM(Alphas(iCoolSAFMAlphaNum)))
            CALL ShowContinueError('Blank field not allowed for '//TRIM(cNumericFields(iCoolFlowPerCoolCapNumericNum)))
            ErrorsFound = .TRUE.
          END IF
        ELSEIF(SameString(Alphas(iCoolSAFMAlphaNum),'None') .OR. lAlphaBlanks(iCoolSAFMAlphaNum))THEN
          UnitarySystem(UnitarySysNum)%CoolingSAFMethod  = None
!          UnitarySystem(UnitarySysNum)%RequestAutosize = .TRUE. ! ??
        ELSE
          CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
          CALL ShowContinueError('Illegal '//TRIM(cAlphaFields(iCoolSAFMAlphaNum))//' = '//TRIM(Alphas(iCoolSAFMAlphaNum)))
          ErrorsFound=.TRUE.
        END IF

        ! Determine supply air flow rate sizing method for heating mode
        IF(SameString(Alphas(iHeatSAFMAlphaNum),'SupplyAirFlowRate'))THEN
          UnitarySystem(UnitarySysNum)%HeatingSAFMethod    = SupplyAirFlowRate
          IF (.NOT. lNumericBlanks(iMaxHeatAirVolFlowNumericNum)) THEN
            UnitarySystem(UnitarySysNum)%MaxHeatAirVolFlow   = Numbers(iMaxHeatAirVolFlowNumericNum)
            IF(UnitarySystem(UnitarySysNum)%MaxHeatAirVolFlow == Autosize) &
                 UnitarySystem(UnitarySysNum)%RequestAutosize = .TRUE.

            IF (UnitarySystem(UnitarySysNum)%MaxHeatAirVolFlow .LE. 0.0d0 .AND. &
                UnitarySystem(UnitarySysNum)%MaxHeatAirVolFlow .NE. AutoSize) THEN
              CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
              CALL ShowContinueError('Illegal '//TRIM(cNumericFields(iMaxHeatAirVolFlowNumericNum))//' = '// &
                                     TRIM(TrimSigDigits(Numbers(iMaxHeatAirVolFlowNumericNum),7)))
              ErrorsFound = .TRUE.
            END IF
          ELSE
            CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
            CALL ShowContinueError('Input for '//TRIM(cAlphaFields(iHeatSAFMAlphaNum))//' = '//TRIM(Alphas(iHeatSAFMAlphaNum)))
            CALL ShowContinueError('Blank field not allowed for '//TRIM(cNumericFields(iMaxHeatAirVolFlowNumericNum)))
            ErrorsFound = .TRUE.
          END IF
        ELSEIF(SameString(Alphas(iHeatSAFMAlphaNum),'FlowPerFloorArea'))THEN
          UnitarySystem(UnitarySysNum)%HeatingSAFMethod    = FlowPerFloorArea
          IF (.NOT. lNumericBlanks(iHeatFlowPerFloorAreaNumericNum)) THEN
            UnitarySystem(UnitarySysNum)%MaxHeatAirVolFlow   = Numbers(iHeatFlowPerFloorAreaNumericNum)
            IF (UnitarySystem(UnitarySysNum)%MaxHeatAirVolFlow .LE. 0.0d0 .AND. &
                UnitarySystem(UnitarySysNum)%MaxHeatAirVolFlow .NE. AutoSize) THEN
              CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
              CALL ShowContinueError('Input for '//TRIM(cAlphaFields(iHeatSAFMAlphaNum))//' = '//TRIM(Alphas(iHeatSAFMAlphaNum)))
              CALL ShowContinueError('Illegal '//TRIM(cNumericFields(iHeatFlowPerFloorAreaNumericNum))//' = '// &
                                     TRIM(TrimSigDigits(Numbers(iHeatFlowPerFloorAreaNumericNum),7)))
              ErrorsFound = .TRUE.
            ! Autosized input is not allowed
            ELSEIF(UnitarySystem(UnitarySysNum)%MaxHeatAirVolFlow .EQ. AutoSize) THEN
              CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
              CALL ShowContinueError('Input for '//TRIM(cAlphaFields(iHeatSAFMAlphaNum))//' = '//TRIM(Alphas(iHeatSAFMAlphaNum)))
              CALL ShowContinueError('Illegal '//TRIM(cNumericFields(iHeatFlowPerFloorAreaNumericNum))//' = Autosize')
              ErrorsFound = .TRUE.
            ELSE
              UnitarySystem(UnitarySysNum)%MaxHeatAirVolFlow = &
                UnitarySystem(UnitarySysNum)%MaxHeatAirVolFlow * TotalFloorAreaOnAirLoop
              UnitarySystem(UnitarySysNum)%RequestAutosize = .TRUE.
            END IF
          ELSE
            CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
            CALL ShowContinueError('Input for '//TRIM(cAlphaFields(iHeatSAFMAlphaNum))//' = '//TRIM(Alphas(iHeatSAFMAlphaNum)))
            CALL ShowContinueError('Blank field not allowed for '//TRIM(cNumericFields(iHeatFlowPerFloorAreaNumericNum)))
            ErrorsFound = .TRUE.
          END IF
        ELSEIF(SameString(Alphas(iHeatSAFMAlphaNum),'FractionOfAutosizedHeatingValue'))THEN
          UnitarySystem(UnitarySysNum)%HeatingSAFMethod  = FractionOfAutosizedHeatingValue
          IF (.NOT. lNumericBlanks(iHeatFlowPerFracCoolNumericNum)) THEN
            UnitarySystem(UnitarySysNum)%MaxHeatAirVolFlow = Numbers(iHeatFlowPerFracCoolNumericNum)
            IF (UnitarySystem(UnitarySysNum)%MaxHeatAirVolFlow .LE. 0.0d0 .AND. &
                UnitarySystem(UnitarySysNum)%MaxHeatAirVolFlow .NE. AutoSize) THEN
              CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
              CALL ShowContinueError('Input for '//TRIM(cAlphaFields(iHeatSAFMAlphaNum))//' = '//TRIM(Alphas(iHeatSAFMAlphaNum)))
              CALL ShowContinueError('Illegal '//TRIM(cNumericFields(iHeatFlowPerFracCoolNumericNum))//' = '// &
                                     TRIM(TrimSigDigits(Numbers(iHeatFlowPerFracCoolNumericNum),7)))
              ErrorsFound = .TRUE.
            ! Autosized input is not allowed
            ELSEIF(UnitarySystem(UnitarySysNum)%MaxHeatAirVolFlow .EQ. AutoSize) THEN
              CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
              CALL ShowContinueError('Input for '//TRIM(cAlphaFields(iHeatSAFMAlphaNum))//' = '//TRIM(Alphas(iHeatSAFMAlphaNum)))
              CALL ShowContinueError('Illegal '//TRIM(cNumericFields(iHeatFlowPerFracCoolNumericNum))//' = Autosize')
              ErrorsFound = .TRUE.
            ELSE
              UnitarySystem(UnitarySysNum)%RequestAutosize = .TRUE.
            END IF
          ELSE
            CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
            CALL ShowContinueError('Input for '//TRIM(cAlphaFields(iHeatSAFMAlphaNum))//' = '//TRIM(Alphas(iHeatSAFMAlphaNum)))
            CALL ShowContinueError('Blank field not allowed for '//TRIM(cNumericFields(iHeatFlowPerFracCoolNumericNum)))
            ErrorsFound = .TRUE.
          END IF
        ELSEIF(SameString(Alphas(iHeatSAFMAlphaNum),'FlowPerHeatingCapacity'))THEN
          UnitarySystem(UnitarySysNum)%HeatingSAFMethod  = FlowPerHeatingCapacity
          IF (.NOT. lNumericBlanks(iHeatFlowPerHeatCapNumericNum)) THEN
            UnitarySystem(UnitarySysNum)%MaxHeatAirVolFlow = Numbers(iHeatFlowPerHeatCapNumericNum)
            IF (UnitarySystem(UnitarySysNum)%MaxHeatAirVolFlow .LE. 0.0d0 .AND. &
                UnitarySystem(UnitarySysNum)%MaxHeatAirVolFlow .NE. AutoSize) THEN
              CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
              CALL ShowContinueError('Input for '//TRIM(cAlphaFields(iHeatSAFMAlphaNum))//' = '//TRIM(Alphas(iHeatSAFMAlphaNum)))
              CALL ShowContinueError('Illegal '//TRIM(cNumericFields(iHeatFlowPerHeatCapNumericNum))//' = '// &
                                     TRIM(TrimSigDigits(Numbers(iHeatFlowPerHeatCapNumericNum),7)))
              ErrorsFound = .TRUE.
            ! Autosized input is not allowed
            ELSEIF(UnitarySystem(UnitarySysNum)%MaxHeatAirVolFlow .EQ. AutoSize) THEN
              CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
              CALL ShowContinueError('Input for '//TRIM(cAlphaFields(iHeatSAFMAlphaNum))//' = '//TRIM(Alphas(iHeatSAFMAlphaNum)))
              CALL ShowContinueError('Illegal '//TRIM(cNumericFields(iHeatFlowPerHeatCapNumericNum))//' = Autosize')
              ErrorsFound = .TRUE.
            ELSE
              UnitarySystem(UnitarySysNum)%RequestAutosize = .TRUE.
            END IF
          ELSE
            CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
            CALL ShowContinueError('Input for '//TRIM(cAlphaFields(iHeatSAFMAlphaNum))//' = '//TRIM(Alphas(iHeatSAFMAlphaNum)))
            CALL ShowContinueError('Blank field not allowed for '//TRIM(cNumericFields(iHeatFlowPerHeatCapNumericNum)))
            ErrorsFound = .TRUE.
          END IF
        ELSEIF(SameString(Alphas(iHeatSAFMAlphaNum),'None') .OR. lAlphaBlanks(iHeatSAFMAlphaNum))THEN
          UnitarySystem(UnitarySysNum)%HeatingSAFMethod  = None
!          UnitarySystem(UnitarySysNum)%RequestAutosize = .TRUE. ! ??
        ELSE
          CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
          CALL ShowContinueError('Illegal '//TRIM(cAlphaFields(iHeatSAFMAlphaNum))//' = '//TRIM(Alphas(iHeatSAFMAlphaNum)))
          ErrorsFound=.TRUE.
        END IF

        ! Determine supply air flow rate sizing method when cooling or heating is not needed
        IF(SameString(Alphas(iNoCoolHeatSAFMAlphaNum),'SupplyAirFlowRate'))THEN
          UnitarySystem(UnitarySysNum)%NoCoolHeatSAFMethod    = SupplyAirFlowRate
          IF (.NOT. lNumericBlanks(iMaxNoCoolHeatAirVolFlowNumericNum)) THEN
            UnitarySystem(UnitarySysNum)%MaxNoCoolHeatAirVolFlow = Numbers(iMaxNoCoolHeatAirVolFlowNumericNum)
            IF(UnitarySystem(UnitarySysNum)%MaxNoCoolHeatAirVolFlow == Autosize) &
                 UnitarySystem(UnitarySysNum)%RequestAutosize = .TRUE.

            IF (UnitarySystem(UnitarySysNum)%MaxNoCoolHeatAirVolFlow .LT. 0.0d0 .AND. &
                UnitarySystem(UnitarySysNum)%MaxNoCoolHeatAirVolFlow .NE. AutoSize) THEN
              CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
              CALL ShowContinueError('Illegal '//TRIM(cNumericFields(iMaxNoCoolHeatAirVolFlowNumericNum))//' = '// &
                                     TRIM(TrimSigDigits(Numbers(iMaxNoCoolHeatAirVolFlowNumericNum),7)))
              ErrorsFound = .TRUE.
            END IF
          ELSE
            CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
            CALL ShowContinueError('Input for '//TRIM(cAlphaFields(iNoCoolHeatSAFMAlphaNum))//' = '// &
                                   TRIM(Alphas(iNoCoolHeatSAFMAlphaNum)))
            CALL ShowContinueError('Blank field not allowed for '//TRIM(cNumericFields(iMaxNoCoolHeatAirVolFlowNumericNum)))
            ErrorsFound = .TRUE.
          END IF
        ELSEIF(SameString(Alphas(iNoCoolHeatSAFMAlphaNum),'FlowPerFloorArea'))THEN
          UnitarySystem(UnitarySysNum)%NoCoolHeatSAFMethod    = FlowPerFloorArea
          IF (.NOT. lNumericBlanks(iNoCoolHeatFlowPerFloorAreaNumericNum)) THEN
            UnitarySystem(UnitarySysNum)%MaxNoCoolHeatAirVolFlow   = Numbers(iNoCoolHeatFlowPerFloorAreaNumericNum)
            IF (UnitarySystem(UnitarySysNum)%MaxNoCoolHeatAirVolFlow .LT. 0.0d0 .AND. &
                UnitarySystem(UnitarySysNum)%MaxNoCoolHeatAirVolFlow .NE. AutoSize) THEN
              CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
              CALL ShowContinueError('Input for '//TRIM(cAlphaFields(iNoCoolHeatSAFMAlphaNum))//' = '// &
                                     TRIM(Alphas(iNoCoolHeatSAFMAlphaNum)))
              CALL ShowContinueError('Illegal '//TRIM(cNumericFields(iNoCoolHeatFlowPerFloorAreaNumericNum))//' = '// &
                                      TRIM(TrimSigDigits(Numbers(iNoCoolHeatFlowPerFloorAreaNumericNum),7)))
              ErrorsFound = .TRUE.
            ! Autosized input is not allowed
            ELSEIF(UnitarySystem(UnitarySysNum)%MaxNoCoolHeatAirVolFlow .EQ. AutoSize) THEN
              CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
              CALL ShowContinueError('Input for '//TRIM(cAlphaFields(iNoCoolHeatSAFMAlphaNum))//' = '// &
                                     TRIM(Alphas(iNoCoolHeatSAFMAlphaNum)))
              CALL ShowContinueError('Illegal '//TRIM(cNumericFields(iNoCoolHeatFlowPerFloorAreaNumericNum))//' = Autosize')
              ErrorsFound = .TRUE.
            ELSE
              UnitarySystem(UnitarySysNum)%MaxNoCoolHeatAirVolFlow = &
                UnitarySystem(UnitarySysNum)%MaxNoCoolHeatAirVolFlow * TotalFloorAreaOnAirLoop
              UnitarySystem(UnitarySysNum)%RequestAutosize = .TRUE.
            END IF
          ELSE
            CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
            CALL ShowContinueError('Input for '//TRIM(cAlphaFields(iNoCoolHeatSAFMAlphaNum))//' = '// &
                                   TRIM(Alphas(iNoCoolHeatSAFMAlphaNum)))
            CALL ShowContinueError('Blank field not allowed for '//TRIM(cNumericFields(iNoCoolHeatFlowPerFloorAreaNumericNum)))
            ErrorsFound = .TRUE.
          END IF
        ELSEIF(SameString(Alphas(iNoCoolHeatSAFMAlphaNum),'FractionOfAutosizedCoolingValue'))THEN
          UnitarySystem(UnitarySysNum)%NoCoolHeatSAFMethod  = FractionOfAutosizedCoolingValue
          IF (.NOT. lNumericBlanks(iNoCoolHeatFlowPerFracCoolNumericNum)) THEN
            UnitarySystem(UnitarySysNum)%MaxNoCoolHeatAirVolFlow = Numbers(iNoCoolHeatFlowPerFracCoolNumericNum)
            IF (UnitarySystem(UnitarySysNum)%MaxNoCoolHeatAirVolFlow .LT. 0.0d0 .AND. &
                UnitarySystem(UnitarySysNum)%MaxNoCoolHeatAirVolFlow .NE. AutoSize) THEN
              CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
              CALL ShowContinueError('Input for '//TRIM(cAlphaFields(iNoCoolHeatSAFMAlphaNum))//' = '// &
                                     TRIM(Alphas(iNoCoolHeatSAFMAlphaNum)))
              CALL ShowContinueError('Illegal '//TRIM(cNumericFields(iNoCoolHeatFlowPerFracCoolNumericNum))//' = '// &
                                     TRIM(TrimSigDigits(Numbers(iNoCoolHeatFlowPerFracCoolNumericNum),7)))
              ErrorsFound = .TRUE.
            ! Autosized input is not allowed
            ELSEIF(UnitarySystem(UnitarySysNum)%MaxNoCoolHeatAirVolFlow .EQ. AutoSize) THEN
              CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
              CALL ShowContinueError('Input for '//TRIM(cAlphaFields(iNoCoolHeatSAFMAlphaNum))//' = '// &
                                     TRIM(Alphas(iNoCoolHeatSAFMAlphaNum)))
              CALL ShowContinueError('Illegal '//TRIM(cNumericFields(iNoCoolHeatFlowPerFracCoolNumericNum))//' = Autosize')
              ErrorsFound = .TRUE.
            ELSE
              UnitarySystem(UnitarySysNum)%RequestAutosize = .TRUE.
            END IF
          ELSE
            CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
            CALL ShowContinueError('Input for '//TRIM(cAlphaFields(iNoCoolHeatSAFMAlphaNum))//' = '// &
                                   TRIM(Alphas(iNoCoolHeatSAFMAlphaNum)))
            CALL ShowContinueError('Blank field not allowed for '//TRIM(cNumericFields(iNoCoolHeatFlowPerFracCoolNumericNum)))
            ErrorsFound = .TRUE.
          END IF
        ELSEIF(SameString(Alphas(iNoCoolHeatSAFMAlphaNum),'FractionOfAutosizedHeatingValue'))THEN
          UnitarySystem(UnitarySysNum)%NoCoolHeatSAFMethod  = FractionOfAutosizedHeatingValue
          IF (.NOT. lNumericBlanks(iNoCoolHeatFlowPerFracHeatNumericNum)) THEN
            UnitarySystem(UnitarySysNum)%MaxNoCoolHeatAirVolFlow = Numbers(iNoCoolHeatFlowPerFracHeatNumericNum)
            IF (UnitarySystem(UnitarySysNum)%MaxNoCoolHeatAirVolFlow .LT. 0.0d0 .AND. &
                UnitarySystem(UnitarySysNum)%MaxNoCoolHeatAirVolFlow .NE. AutoSize) THEN
              CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
              CALL ShowContinueError('Input for '//TRIM(cAlphaFields(iNoCoolHeatSAFMAlphaNum))//' = '// &
                                     TRIM(Alphas(iNoCoolHeatSAFMAlphaNum)))
              CALL ShowContinueError('Illegal '//TRIM(cNumericFields(iNoCoolHeatFlowPerFracHeatNumericNum))//' = '// &
                                     TRIM(TrimSigDigits(Numbers(iNoCoolHeatFlowPerFracHeatNumericNum),7)))
              ErrorsFound = .TRUE.
            ! Autosized input is not allowed
            ELSEIF(UnitarySystem(UnitarySysNum)%MaxNoCoolHeatAirVolFlow .EQ. AutoSize) THEN
              CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
              CALL ShowContinueError('Input for '//TRIM(cAlphaFields(iNoCoolHeatSAFMAlphaNum))//' = '// &
                                     TRIM(Alphas(iNoCoolHeatSAFMAlphaNum)))
              CALL ShowContinueError('Illegal '//TRIM(cNumericFields(iNoCoolHeatFlowPerFracHeatNumericNum))//' = Autosize')
              ErrorsFound = .TRUE.
            ELSE
              UnitarySystem(UnitarySysNum)%RequestAutosize = .TRUE.
            END IF
          ELSE
            CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
            CALL ShowContinueError('Input for '//TRIM(cAlphaFields(iNoCoolHeatSAFMAlphaNum))//' = '// &
                                   TRIM(Alphas(iNoCoolHeatSAFMAlphaNum)))
            CALL ShowContinueError('Blank field not allowed for '//TRIM(cNumericFields(iNoCoolHeatFlowPerFracHeatNumericNum)))
            ErrorsFound = .TRUE.
          END IF
        ELSEIF(SameString(Alphas(iNoCoolHeatSAFMAlphaNum),'FlowPerCoolingCapacity'))THEN
          UnitarySystem(UnitarySysNum)%NoCoolHeatSAFMethod  = FlowPerCoolingCapacity
          IF (.NOT. lNumericBlanks(iNoCoolHeatFlowPerCoolCapNumericNum)) THEN
            UnitarySystem(UnitarySysNum)%MaxNoCoolHeatAirVolFlow = Numbers(iNoCoolHeatFlowPerCoolCapNumericNum)
            IF (UnitarySystem(UnitarySysNum)%MaxNoCoolHeatAirVolFlow .LT. 0.0d0 .AND. &
                UnitarySystem(UnitarySysNum)%MaxNoCoolHeatAirVolFlow .NE. AutoSize) THEN
              CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
              CALL ShowContinueError('Input for '//TRIM(cAlphaFields(iNoCoolHeatSAFMAlphaNum))//' = '// &
                                     TRIM(Alphas(iNoCoolHeatSAFMAlphaNum)))
              CALL ShowContinueError('Illegal '//TRIM(cNumericFields(iNoCoolHeatFlowPerCoolCapNumericNum))//' = '// &
                                     TRIM(TrimSigDigits(Numbers(iNoCoolHeatFlowPerCoolCapNumericNum),7)))
              ErrorsFound = .TRUE.
            ! Autosized input is not allowed
            ELSEIF(UnitarySystem(UnitarySysNum)%MaxNoCoolHeatAirVolFlow .EQ. AutoSize) THEN
              CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
              CALL ShowContinueError('Input for '//TRIM(cAlphaFields(iNoCoolHeatSAFMAlphaNum))//' = '// &
                                     TRIM(Alphas(iNoCoolHeatSAFMAlphaNum)))
              CALL ShowContinueError('Illegal '//TRIM(cNumericFields(iNoCoolHeatFlowPerCoolCapNumericNum))//' = Autosize')
              ErrorsFound = .TRUE.
            ELSE
              UnitarySystem(UnitarySysNum)%RequestAutosize = .TRUE.
            END IF
          ELSE
            CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
            CALL ShowContinueError('Input for '//TRIM(cAlphaFields(iNoCoolHeatSAFMAlphaNum))//' = '// &
                                   TRIM(Alphas(iNoCoolHeatSAFMAlphaNum)))
            CALL ShowContinueError('Blank field not allowed for '//TRIM(cNumericFields(iNoCoolHeatFlowPerCoolCapNumericNum)))
            ErrorsFound = .TRUE.
          END IF
        ELSEIF(SameString(Alphas(iNoCoolHeatSAFMAlphaNum),'FlowPerHeatingCapacity'))THEN
          UnitarySystem(UnitarySysNum)%NoCoolHeatSAFMethod  = FlowPerHeatingCapacity
          IF (.NOT. lNumericBlanks(iNoCoolHeatFlowPerHeatCapNumericNum)) THEN
            UnitarySystem(UnitarySysNum)%MaxNoCoolHeatAirVolFlow = Numbers(iNoCoolHeatFlowPerHeatCapNumericNum)
            IF (UnitarySystem(UnitarySysNum)%MaxNoCoolHeatAirVolFlow .LT. 0.0d0 .AND. &
                UnitarySystem(UnitarySysNum)%MaxNoCoolHeatAirVolFlow .NE. AutoSize) THEN
              CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
              CALL ShowContinueError('Input for '//TRIM(cAlphaFields(iNoCoolHeatSAFMAlphaNum))//' = '// &
                                     TRIM(Alphas(iNoCoolHeatSAFMAlphaNum)))
              CALL ShowContinueError('Illegal '//TRIM(cNumericFields(iNoCoolHeatFlowPerHeatCapNumericNum))//' = '// &
                                     TRIM(TrimSigDigits(Numbers(iNoCoolHeatFlowPerHeatCapNumericNum),7)))
              ErrorsFound = .TRUE.
            ! Autosized input is not allowed
            ELSEIF(UnitarySystem(UnitarySysNum)%MaxNoCoolHeatAirVolFlow .EQ. AutoSize) THEN
              CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
              CALL ShowContinueError('Input for '//TRIM(cAlphaFields(iNoCoolHeatSAFMAlphaNum))//' = '// &
                                     TRIM(Alphas(iNoCoolHeatSAFMAlphaNum)))
              CALL ShowContinueError('Illegal '//TRIM(cNumericFields(iNoCoolHeatFlowPerHeatCapNumericNum))//' = Autosize')
              ErrorsFound = .TRUE.
            ELSE
              UnitarySystem(UnitarySysNum)%RequestAutosize = .TRUE.
            END IF
          ELSE
            CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
            CALL ShowContinueError('Input for '//TRIM(cAlphaFields(iNoCoolHeatSAFMAlphaNum))//' = '// &
                                   TRIM(Alphas(iNoCoolHeatSAFMAlphaNum)))
            CALL ShowContinueError('Blank field not allowed for '//TRIM(cNumericFields(iNoCoolHeatFlowPerHeatCapNumericNum)))
            ErrorsFound = .TRUE.
          END IF
        ELSEIF(SameString(Alphas(iNoCoolHeatSAFMAlphaNum),'None') .OR. lAlphaBlanks(iNoCoolHeatSAFMAlphaNum))THEN
          UnitarySystem(UnitarySysNum)%NoCoolHeatSAFMethod  = None
!          UnitarySystem(UnitarySysNum)%RequestAutosize = .TRUE. ! ??
        ELSE
          CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
          CALL ShowContinueError('Illegal '//TRIM(cAlphaFields(iNoCoolHeatSAFMAlphaNum))//' = '// &
                                 TRIM(Alphas(iNoCoolHeatSAFMAlphaNum)))
          ErrorsFound=.TRUE.
        END IF

!!       Check that the same air flow method is used if cooling and heating coil present
!        IF(UnitarySystem(UnitarySysNum)%CoolCoilExists .AND. UnitarySystem(UnitarySysNum)%HeatCoilExists)THEN
!          IF(UnitarySystem(UnitarySysNum)%CoolingSAFMethod /= UnitarySystem(UnitarySysNum)%HeatingSAFMethod .OR. &
!             UnitarySystem(UnitarySysNum)%CoolingSAFMethod /= UnitarySystem(UnitarySysNum)%NoCoolHeatSAFMethod)THEN
!          CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
!          CALL ShowContinueError('Illegal supply air flow method.')
!          CALL ShowContinueError(TRIM(cAlphaFields(iCoolSAFMAlphaNum))//' = '//TRIM(Alphas(iCoolSAFMAlphaNum)))
!          CALL ShowContinueError(TRIM(cAlphaFields(iHeatSAFMAlphaNum))//' = '//TRIM(Alphas(iHeatSAFMAlphaNum)))
!          CALL ShowContinueError(TRIM(cAlphaFields(iNoCoolHeatSAFMAlphaNum))//' = '//TRIM(Alphas(iNoCoolHeatSAFMAlphaNum)))
!          CALL ShowContinueError('...Supply air flow methods must be the same when both a cooling and heating coil are present.')
!          ErrorsFound=.TRUE.
!          END IF
!        END IF

!       Fan operating mode (cycling or constant) schedule. IF constant fan, then set AirFlowControl
        IF(UnitarySystem(UnitarySysNum)%FanOpModeSchedPtr .GT. 0)THEN
          IF (.NOT. CheckScheduleValueMinMax(UnitarySystem(UnitarySysNum)%FanOpModeSchedPtr,'>=',0.0d0,'<=',0.0d0)) THEN
!           set fan operating mode to continuous so sizing can set VS coil data
            UnitarySystem(UnitarySysNum)%FanOpMode = ContFanCycCoil
!           set air flow control mode:
!             UseCompressorOnFlow = operate at last cooling or heating air flow requested when compressor is off
!             UseCompressorOffFlow = operate at value specified by user
!           AirFlowControl only valid if fan opmode = ContFanCycComp
            IF (UnitarySystem(UnitarySysNum)%MaxNoCoolHeatAirVolFlow .EQ. 0.0d0) THEN
              UnitarySystem(UnitarySysNum)%AirFlowControl = UseCompressorOnFlow
            ELSE
              UnitarySystem(UnitarySysNum)%AirFlowControl = UseCompressorOffFlow
            END IF
          END IF
        END IF

       IF(UnitarySystem(UnitarySysNum)%CoolingCoilType_Num /= CoilDX_CoolingHXAssisted .AND. &
          UnitarySystem(UnitarySysNum)%CoolingCoilType_Num /= CoilDX_CoolingTwoStageWHumControl .AND. &
          UnitarySystem(UnitarySysNum)%DehumidControlType_Num == DehumidControl_Multimode) THEN
         CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
         CALL ShowContinueError('Illegal '//TRIM(cAlphaFields(iDehumidControlAlphaNum))//' = '// &
                                TRIM(Alphas(iDehumidControlAlphaNum)))
         CALL ShowContinueError('Multimode control must be used with a Heat Exchanger Assisted or Multimode Cooling Coil.')
         IF(lAlphaBlanks(iSuppHeatCoilNameAlphaNum))THEN
         ELSE
           CALL ShowContinueError('Dehumidification control type is assumed to be CoolReheat'// &
                                  ' and the simulation continues.')
           UnitarySystem(UnitarySysNum)%DehumidControlType_Num = DehumidControl_CoolReheat
         END IF
       END IF

!       Check placement of cooling coil with respect to fan placement and dehumidification control type

       IF (UnitarySystem(UnitarySysNum)%FanExists) THEN
         IF(UnitarySystem(UnitarySysNum)%FanPlace == BlowThru)THEN
           IF(FanOutletNode == HeatingCoilInletNode .AND. &
              UnitarySystem(UnitarySysNum)%DehumidControlType_Num /= DehumidControl_CoolReheat)THEN
             UnitarySystem(UnitarySysNum)%CoolingCoilUpstream = .FALSE.
           END IF
         ELSEIF(UnitarySystem(UnitarySysNum)%FanPlace == DrawThru)THEN
           IF(HeatingCoilOutletNode == CoolingCoilInletNode .AND. &
              UnitarySystem(UnitarySysNum)%DehumidControlType_Num /= DehumidControl_CoolReheat)THEN
             UnitarySystem(UnitarySysNum)%CoolingCoilUpstream = .FALSE.
           END IF
         END IF
       ELSE
         IF(HeatingCoilOutletNode == CoolingCoilInletNode .AND. &
            UnitarySystem(UnitarySysNum)%DehumidControlType_Num /= DehumidControl_CoolReheat)THEN
           UnitarySystem(UnitarySysNum)%CoolingCoilUpstream = .FALSE.
         END IF
       END IF

       ! check node connections
       IF(UnitarySystem(UnitarySysNum)%FanPlace .EQ. BlowThru)THEN

          IF(FanInletNode /= UnitarySystem(UnitarySysNum)%UnitarySystemInletNodeNum) THEN
             CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
             CALL ShowContinueError('When a blow through fan is specified, the fan inlet node name must be '// &
                                    'the same as the unitary system inlet node name.')
             CALL ShowContinueError('...Fan inlet node name           = '//TRIM(NodeID(FanInletNode)))
             CALL ShowContinueError('...UnitarySystem inlet node name = ' &
                                    //TRIM(NodeID(UnitarySystem(UnitarySysNum)%UnitarySystemInletNodeNum)))
             ErrorsFound=.TRUE.
          END IF
          IF(UnitarySystem(UnitarySysNum)%CoolingCoilUpstream)THEN
            IF(FanOutletNode /= CoolingCoilInletNode .AND. &
               UnitarySystem(UnitarySysNum)%CoolCoilExists .AND. UnitarySystem(UnitarySysNum)%FanExists) THEN
               CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
               CALL ShowContinueError('When a blow through fan is specified, the fan outlet node name must be '// &
                                      'the same as the cooling coil inlet node name.')
               CALL ShowContinueError('...Fan outlet node name         = '//TRIM(NodeID(FanOutletNode)))
               CALL ShowContinueError('...Cooling coil inlet node name = '//TRIM(NodeID(CoolingCoilInletNode)))
               ErrorsFound=.TRUE.
            END IF
            IF(CoolingCoilOutletNode /= HeatingCoilInletNode .AND. &
               UnitarySystem(UnitarySysNum)%CoolCoilExists .AND. UnitarySystem(UnitarySysNum)%HeatCoilExists) THEN
               CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
               CALL ShowContinueError('The cooling coil outlet node name must be '// &
                                      'the same as the heating coil inlet node name.')
               CALL ShowContinueError('...Cooling coil outlet node name = '//TRIM(NodeID(CoolingCoilOutletNode)))
               CALL ShowContinueError('...Heating coil inlet node name  = '//TRIM(NodeID(HeatingCoilInletNode)))
               ErrorsFound=.TRUE.
            END IF
            IF(UnitarySystem(UnitarySysNum)%SuppCoilExists)THEN
              IF(SupHeatCoilOutletNode /= UnitarySystem(UnitarySysNum)%UnitarySystemOutletNodeNum) THEN
                CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
                CALL ShowContinueError('The reheat coil outlet node name must be '// &
                                       'the same as the unitary system outlet node name.')
                CALL ShowContinueError('...Reheat coil outlet node name   = '//TRIM(NodeID(SupHeatCoilOutletNode)))
                CALL ShowContinueError('...UnitarySystem outlet node name = ' &
                                      //TRIM(NodeID(UnitarySystem(UnitarySysNum)%UnitarySystemOutletNodeNum)))
!                ErrorsFound=.TRUE.
              END IF
            ELSE ! IF((UnitarySystem(UnitarySysNum)%Humidistat ...
              ! Heating coil outlet node name must be the same as the Unitary system outlet node name
              IF (UnitarySystem(UnitarySysNum)%HeatCoilExists .AND. &
                  HeatingCoilOutletNode /= UnitarySystem(UnitarySysNum)%UnitarySystemOutletNodeNum) THEN
                CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
                CALL ShowContinueError('When a blow through fan is specified, '//&
                               'the heating coil outlet node name must be the same as the unitary system outlet node name.')
                CALL ShowContinueError('...Heating coil outlet node name  = '//TRIM(NodeID(HeatingCoilOutletNode)))
                CALL ShowContinueError('...Unitary system outlet node name = ' &
                                         //TRIM(NodeID(UnitarySystem(UnitarySysNum)%UnitarySystemOutletNodeNum)))
                ErrorsFound=.TRUE.
              END IF
            END IF
          ELSE ! IF(UnitarySystem(UnitarySysNum)%CoolingCoilUpstream)THEN
            IF(FanOutletNode /= HeatingCoilInletNode .AND. UnitarySystem(UnitarySysNum)%FanExists .AND. &
               UnitarySystem(UnitarySysNum)%HeatCoilExists)THEN
               CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
               CALL ShowContinueError('When a blow through fan is specified, the fan outlet node name must be '// &
                                      'the same as the heating coil inlet node name.')
               CALL ShowContinueError('...Fan outlet node name         = '//TRIM(NodeID(FanOutletNode)))
               CALL ShowContinueError('...Heating coil inlet node name = '//TRIM(NodeID(HeatingCoilInletNode)))
               ErrorsFound=.TRUE.
            END IF
            IF(HeatingCoilOutletNode /= CoolingCoilInletNode .AND. UnitarySystem(UnitarySysNum)%CoolCoilExists .AND. &
               UnitarySystem(UnitarySysNum)%HeatCoilExists) THEN
               CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
               CALL ShowContinueError('The heating coil outlet node name must be '// &
                                      'the same as the cooling coil inlet node name.')
               CALL ShowContinueError('...Heating coil outlet node name = '//TRIM(NodeID(HeatingCoilOutletNode)))
               CALL ShowContinueError('...Cooling coil inlet node name  = '//TRIM(NodeID(CoolingCoilInletNode)))
               ErrorsFound=.TRUE.
            END IF
            IF(CoolingCoilOutletNode /= UnitarySystem(UnitarySysNum)%UnitarySystemOutletNodeNum .AND. &
               UnitarySystem(UnitarySysNum)%CoolCoilExists) THEN
               CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
               CALL ShowContinueError('When a blow through fan is specified, the cooling coil outlet node name must be '// &
                                      'the same as the unitary system outlet node name.')
               CALL ShowContinueError('...Cooling coil outlet node name   = '//TRIM(NodeID(CoolingCoilOutletNode)))
               CALL ShowContinueError('...UnitarySystem outlet node name  = ' &
                                      //TRIM(NodeID(UnitarySystem(UnitarySysNum)%UnitarySystemOutletNodeNum)))
               ErrorsFound=.TRUE.
            END IF
          END IF

        ELSE ! ELSE from IF(UnitarySystem(UnitarySysNum)%FanPlace .EQ. BlowThru)THEN

          IF(UnitarySystem(UnitarySysNum)%CoolingCoilUpstream)THEN
            IF(CoolingCoilInletNode /= UnitarySystem(UnitarySysNum)%UnitarySystemInletNodeNum .AND. &
               CoolingCoilInletNode /= 0 .AND. UnitarySystem(UnitarySysNum)%FanExists) THEN
              CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
              CALL ShowContinueError('When a draw through fan is specified, the cooling coil inlet node name must be '// &
                                     'the same as the unitary system inlet node name.')
              CALL ShowContinueError('...Cooling coil inlet node name  = '//TRIM(NodeID(CoolingCoilInletNode)))
              CALL ShowContinueError('...UnitarySystem inlet node name = ' &
                                     //TRIM(NodeID(UnitarySystem(UnitarySysNum)%UnitarySystemInletNodeNum)))
                ErrorsFound=.TRUE.
            END IF
            IF(CoolingCoilOutletNode /= HeatingCoilInletNode .AND. &
               UnitarySystem(UnitarySysNum)%CoolCoilExists .AND. UnitarySystem(UnitarySysNum)%HeatCoilExists) THEN
              CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
              CALL ShowContinueError('The cooling coil outlet node name must be '// &
                                     'the same as the heating coil inlet node name.')
              CALL ShowContinueError('...Cooling coil outlet node name = '//TRIM(NodeID(CoolingCoilOutletNode)))
              CALL ShowContinueError('...Heating coil inlet node name  = '//TRIM(NodeID(HeatingCoilInletNode)))
              ErrorsFound=.TRUE.
            END IF
            IF(HeatingCoilOutletNode /= FanInletNode .AND. &
               UnitarySystem(UnitarySysNum)%HeatCoilExists .AND. UnitarySystem(UnitarySysNum)%FanExists) THEN
              CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
              CALL ShowContinueError('When a draw through fan is specified, the heating coil outlet node name must be '// &
                                     'the same as the fan inlet node name.')
              CALL ShowContinueError('...Heating coil outlet node name = '//TRIM(NodeID(HeatingCoilOutletNode)))
              CALL ShowContinueError('...Fan inlet node name           = '//TRIM(NodeID(FanInletNode)))
              ErrorsFound=.TRUE.
            END IF
            IF(UnitarySystem(UnitarySysNum)%SuppCoilExists)THEN
              IF(FanOutletNode /= SupHeatCoilInletNode .AND. UnitarySystem(UnitarySysNum)%FanExists) THEN
                CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
                CALL ShowContinueError('When a draw through fan is specified, the fan outlet node name must be '// &
                                       'the same as the reheat coil inlet node name.')
                CALL ShowContinueError('...Fan outlet node name        = '//TRIM(NodeID(FanOutletNode)))
                CALL ShowContinueError('...Reheat coil inlet node name = '//TRIM(NodeID(SupHeatCoilInletNode)))
!                ErrorsFound=.TRUE.
              END IF
              IF(SupHeatCoilOutletNode /= UnitarySystem(UnitarySysNum)%UnitarySystemOutletNodeNum) THEN
                CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
                CALL ShowContinueError('The reheat coil outlet node name must be '// &
                                       'the same as the unitary system outlet node name.')
                CALL ShowContinueError('...Reheat coil outlet node name   = '//TRIM(NodeID(SupHeatCoilOutletNode)))
                CALL ShowContinueError('...UnitarySystem outlet node name = ' &
                                                  //TRIM(NodeID(UnitarySystem(UnitarySysNum)%UnitarySystemOutletNodeNum)))
              END IF
            ELSE
              IF(FanOutletNode /= UnitarySystem(UnitarySysNum)%UnitarySystemOutletNodeNum .AND. &
                 UnitarySystem(UnitarySysNum)%FanExists) THEN
                CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
                CALL ShowContinueError('When a draw through fan is specified, the fan outlet node name must be '// &
                                       'the same as the unitary system outlet node name.')
                CALL ShowContinueError('...Fan outlet node name        = '//TRIM(NodeID(FanOutletNode)))
                CALL ShowContinueError('...Unitary system outlet node name = '// &
                                       TRIM(NodeID(UnitarySystem(UnitarySysNum)%UnitarySystemOutletNodeNum)))
                ErrorsFound=.TRUE.
              END IF
            END IF
          ELSE ! IF(UnitarySystem(UnitarySysNum)%CoolingCoilUpstream)THEN
            IF(HeatingCoilInletNode /= UnitarySystem(UnitarySysNum)%UnitarySystemInletNodeNum .AND. &
               HeatingCoilInletNode /= 0 .AND. UnitarySystem(UnitarySysNum)%FanExists) THEN
               CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
               CALL ShowContinueError('When a draw through fan is specified, the heating coil inlet node name must be '// &
                                      'the same as the unitary system inlet node name.')
               CALL ShowContinueError('...Heating coil inlet node name  = '//TRIM(NodeID(HeatingCoilInletNode)))
               CALL ShowContinueError('...UnitarySystem inlet node name = ' &
                                      //TRIM(NodeID(UnitarySystem(UnitarySysNum)%UnitarySystemInletNodeNum)))
               ErrorsFound=.TRUE.
            END IF
            IF(HeatingCoilOutletNode /= CoolingCoilInletNode .AND. &
               UnitarySystem(UnitarySysNum)%HeatCoilExists .AND. UnitarySystem(UnitarySysNum)%CoolCoilExists) THEN
               CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
               CALL ShowContinueError('The heating coil outlet node name must be '// &
                                      'the same as the cooling coil inlet node name.')
               CALL ShowContinueError('...Heating coil outlet node name = '//TRIM(NodeID(HeatingCoilOutletNode)))
               CALL ShowContinueError('...Cooling coil inlet node name  = '//TRIM(NodeID(CoolingCoilInletNode)))
               ErrorsFound=.TRUE.
            END IF
            IF(CoolingCoilOutletNode /= FanInletNode .AND. &
               UnitarySystem(UnitarySysNum)%CoolCoilExists .AND. UnitarySystem(UnitarySysNum)%FanExists) THEN
               CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
               CALL ShowContinueError('When a draw through fan is specified, the cooling coil outlet node name must be '// &
                                      'the same as the fan inlet node name.')
               CALL ShowContinueError('...Cooling coil outlet node name = '//TRIM(NodeID(CoolingCoilOutletNode)))
               CALL ShowContinueError('...Fan inlet node name           = '//TRIM(NodeID(FanInletNode)))
               ErrorsFound=.TRUE.
            END IF
            IF(FanOutletNode /= UnitarySystem(UnitarySysNum)%UnitarySystemOutletNodeNum .AND. &
               UnitarySystem(UnitarySysNum)%FanExists) THEN
               CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
               CALL ShowContinueError('When a draw through fan is specified, the fan outlet node name must be '// &
                                      'the same as the unitary system outlet node name.')
               CALL ShowContinueError('...Fan outlet node name           = '//TRIM(NodeID(FanOutletNode)))
               CALL ShowContinueError('...UnitarySystem outlet node name = ' &
                                       //TRIM(NodeID(UnitarySystem(UnitarySysNum)%UnitarySystemOutletNodeNum)))
               ErrorsFound=.TRUE.
            END IF
          END IF
        END IF ! ELSE from IF(UnitarySystem(UnitarySysNum)%FanPlace .EQ. BlowThru)THEN

        !Set the unitary system supplemental heater max outlet temperature
        ! this field will be 0 if the input is not specified (included) in the input file
        ! someone may use a default other than what we intended, allow it to be used
        ! so if this field is blank, and the input field is included, read the default, otherwise use 80
        IF (.NOT. lNumericBlanks(iDesignMaxOutletTempNumericNum) .AND. NumNumbers .GT. (iDesignMaxOutletTempNumericNum-1)) THEN
          UnitarySystem(UnitarySysNum)%DesignMaxOutletTemp = Numbers(iDesignMaxOutletTempNumericNum)
          IF(UnitarySystem(UnitarySysNum)%DesignMaxOutletTemp == Autosize) &
               UnitarySystem(UnitarySysNum)%RequestAutosize = .TRUE.
        END IF

        !Set maximum Outdoor air temperature for supplemental heating coil operation
        ! this field will be 0 if the input is not specified (included) in the input file
        ! someone may use a default other than what we intended, allow it to be used
        ! so if this field is blank, and the input field is included, read the default, otherwise use 9999
        IF (.NOT. lNumericBlanks(iMaxOATSuppHeatNumericNum) .AND. NumNumbers .GT. (iMaxOATSuppHeatNumericNum-1)) THEN
          UnitarySystem(UnitarySysNum)%MaxOATSuppHeat = Numbers(iMaxOATSuppHeatNumericNum)
        ! Can't let MaxOATSuppHeat default to 21C if using cool reheat since it would shut off supp heater when dehumidifying
        ! this may also allow supplemental heater to operate when in heating mode when it should not
        ELSE IF(NumNumbers .LT. iMaxOATSuppHeatNumericNum .AND. &
                UnitarySystem(UnitarySysNum)%DehumidControlType_Num == DehumidControl_CoolReheat)THEN
          UnitarySystem(UnitarySysNum)%MaxOATSuppHeat = 999.0d0
        END IF

        IF(UnitarySystem(UnitarySysNum)%MaxCoolAirVolFlow .NE. Autosize .AND. &
           UnitarySystem(UnitarySysNum)%MaxHeatAirVolFlow .NE. Autosize .AND. &
           UnitarySystem(UnitarySysNum)%MaxNoCoolHeatAirVolFlow .NE. Autosize .AND. &
           .NOT. UnitarySystem(UnitarySysNum)%RequestAutosize)THEN
!           (UnitarySystem(UnitarySysNum)%CoolingSAFMethod .LE. SupplyAirFlowRate .OR. &
!            UnitarySystem(UnitarySysNum)%HeatingSAFMethod .LE. SupplyAirFlowRate))THEN
          UnitarySystem(UnitarySysNum)%DesignFanVolFlowRate = MAX(UnitarySystem(UnitarySysNum)%MaxCoolAirVolFlow, &
                                                                    UnitarySystem(UnitarySysNum)%MaxHeatAirVolFlow, &
                                                                    UnitarySystem(UnitarySysNum)%MaxNoCoolHeatAirVolFlow)
        ELSE
          IF(UnitarySystem(UnitarySysNum)%FanExists)THEN
            UnitarySystem(UnitarySysNum)%DesignFanVolFlowRate = Autosize
          END IF
  ! need more of this type of warning when flow cannot be determined
          IF(UnitarySystem(UnitarySysNum)%MaxHeatAirVolFlow .EQ. 0.0d0 .AND. &
             UnitarySystem(UnitarySysNum)%HeatCoilExists)THEN
             IF(UnitarySystem(UnitarySysNum)%FanExists)THEN
               IF(UnitarySystem(UnitarySysNum)%CoolCoilExists .AND. &
                  UnitarySystem(UnitarySysNum)%MaxCoolAirVolFlow /= Autosize)THEN
                 IF(UnitarySystem(UnitarySysNum)%MaxCoolAirVolFlow == 0.0d0)THEN
                   UnitarySystem(UnitarySysNum)%MaxHeatAirVolFlow = UnitarySystem(UnitarySysNum)%DesignFanVolFlowRate
                 END IF
               END IF
             ELSEIF(UnitarySystem(UnitarySysNum)%CoolCoilExists)THEN
               UnitarySystem(UnitarySysNum)%MaxHeatAirVolFlow = UnitarySystem(UnitarySysNum)%MaxCoolAirVolFlow
             ELSE
               IF(UnitarySystem(UnitarySysNum)%HeatingCoilType_Num /= CoilDX_HeatingEmpirical .AND. &
                  UnitarySystem(UnitarySysNum)%HeatingCoilType_Num /= CoilDX_MultiSpeedHeating .AND. &
                  UnitarySystem(UnitarySysNum)%HeatingCoilType_Num /= Coil_HeatingAirToAirVariableSpeed)THEN
                 CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
                 CALL ShowContinueError('When non-DX heating coils are specified, the heating air flow rate must be '// &
                                        'entered in '//TRIM(cAlphaFields(iHeatSAFMAlphaNum)))
                 ErrorsFound=.TRUE.
               END IF
             END IF
          ELSEIF(UnitarySystem(UnitarySysNum)%MaxHeatAirVolFlow .EQ. 0.0d0 .AND. &
             .NOT. UnitarySystem(UnitarySysNum)%FanExists .AND. &
             .NOT. UnitarySystem(UnitarySysNum)%CoolCoilExists)THEN
                 CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
                 CALL ShowContinueError('When non-DX heating coils are specified, the heating air flow rate must be '// &
                                        'entered in '//TRIM(cAlphaFields(iHeatSAFMAlphaNum)))
          END IF
        END IF

        IF(FanVolFlowRate .NE. AutoSize .AND. UnitarySystem(UnitarySysNum)%FanExists)THEN
          IF(FanVolFlowRate .LT. UnitarySystem(UnitarySysNum)%MaxCoolAirVolFlow .AND. &
               UnitarySystem(UnitarySysNum)%MaxCoolAirVolFlow .NE. AutoSize .AND. &
               UnitarySystem(UnitarySysNum)%CoolCoilExists)THEN
            CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
            CALL ShowContinueError('... air flow rate = '//TRIM(TrimSigDigits(FanVolFlowRate,7))//' in fan object '// &
                 TRIM(FanName)//' is less than the maximum HVAC system air flow rate in cooling mode.')
            CALL ShowContinueError(' The '//TRIM(cNumericFields(iMaxCoolAirVolFlowNumericNum))//' is reset to the' &
                                 //' fan flow rate and the simulation continues.')
            UnitarySystem(UnitarySysNum)%MaxCoolAirVolFlow = FanVolFlowRate
            UnitarySystem(UnitarySysNum)%DesignFanVolFlowRate = FanVolFlowRate
          END IF
          IF(FanVolFlowRate .LT. UnitarySystem(UnitarySysNum)%MaxHeatAirVolFlow .AND. &
               UnitarySystem(UnitarySysNum)%MaxHeatAirVolFlow .NE. AutoSize .AND. &
               UnitarySystem(UnitarySysNum)%HeatCoilExists)THEN
            CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
            CALL ShowContinueError('... air flow rate = '//TRIM(TrimSigDigits(FanVolFlowRate,7))//' in fan object '// &
                 TRIM(FanName)//' is less than the maximum HVAC system air flow rate in heating mode.')
            CALL ShowContinueError(' The '//TRIM(cNumericFields(3))//' is reset to the' &
                                 //' fan flow rate and the simulation continues.')
            UnitarySystem(UnitarySysNum)%MaxHeatAirVolFlow = FanVolFlowRate
            UnitarySystem(UnitarySysNum)%DesignFanVolFlowRate = FanVolFlowRate
          END IF
        END IF

        IF(UnitarySystem(UnitarySysNum)%FanOpModeSchedPtr .GT. 0)THEN
          IF (.NOT. CheckScheduleValueMinMax(UnitarySystem(UnitarySysNum)%FanOpModeSchedPtr,'>=',0.0d0,'<=',0.0d0)) THEN
!           set air flow control mode:
!             UseCompressorOnFlow = operate at last cooling or heating air flow requested when compressor is off
!             UseCompressorOffFlow = operate at value specified by user
!           AirFlowControl only valid IF fan opmode = ContFanCycComp
            IF (UnitarySystem(UnitarySysNum)%MaxNoCoolHeatAirVolFlow .EQ. 0.0d0) THEN
              UnitarySystem(UnitarySysNum)%AirFlowControl = UseCompressorOnFlow
            ELSE
              UnitarySystem(UnitarySysNum)%AirFlowControl = UseCompressorOffFlow
            END IF
          END IF
        END IF

        !Set minimum OAT for heat pump compressor operation
        ! get from coil module
        ErrFlag=.FALSE.
        IF (UnitarySystem(UnitarySysNum)%HeatingCoilType_Num == Coil_HeatingAirToAirVariableSpeed) THEN
            UnitarySystem(UnitarySysNum)%MinOATCompressor = GetVSCoilMinOATCompressor(HeatingCoilName,ErrFlag)
        ELSEIF(UnitarySystem(UnitarySysNum)%HeatingCoilType_Num == CoilDX_HeatingEmpirical .OR. &
                UnitarySystem(UnitarySysNum)%HeatingCoilType_Num == CoilDX_MultiSpeedHeating) THEN
            UnitarySystem(UnitarySysNum)%MinOATCompressor =    &
               GetMinOATDXCoilCompressor(HeatingCoilType,HeatingCoilName,ErrFlag)
!       ELSEIF  ***... make sure we catch all possbile coil types here ...***
        ELSE
            UnitarySystem(UnitarySysNum)%MinOATCompressor = -1000.0d0
        END IF
        IF (ErrFlag) THEN
          CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
          ErrorsFound=.TRUE.
        END IF


!       Mine heatpump Outdoor condenser node from DX coil object
        ErrFlag=.FALSE.
        IF (UnitarySystem(UnitarySysNum)%CoolingCoilType_Num == CoilDX_CoolingSingleSpeed) THEN
          UnitarySystem(UnitarySysNum)%CondenserNodeNum = &
            GetDXCoilCondenserInletNode(CoolingCoilType,CoolingCoilName,ErrFlag)
        ELSEIF (UnitarySystem(UnitarySysNum)%CoolingCoilType_Num == Coil_CoolingAirToAirVariableSpeed) THEN
          UnitarySystem(UnitarySysNum)%CondenserNodeNum = GetVSCoilCondenserInletNode(CoolingCoilName,ErrFlag)
        ELSEIF (UnitarySystem(UnitarySysNum)%CoolingCoilType_Num == CoilDX_CoolingHXAssisted) THEN
          UnitarySystem(UnitarySysNum)%CondenserNodeNum = &
            GetDXCoilCondenserInletNode('Coil:Cooling:DX:SingleSpeed', &
            GetHXDXCoilName(CoolingCoilType,CoolingCoilName,ErrFlag), &
            ErrFlag)
        ELSE
          IF(.NOT. lAlphaBlanks(iCondenserNodeAlphaNum))THEN
            UnitarySystem(UnitarySysNum)%CondenserNodeNum = &
               GetOnlySingleNode(Alphas(iCondenserNodeAlphaNum),ErrFlag,CurrentModuleObject,Alphas(1), &
                            NodeType_Air,NodeConnectionType_Inlet,1,ObjectIsParent)
          ELSE
            ! do nothing?
          END IF
        END IF
        IF (ErrFlag) THEN
          CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
          ErrorsFound=.TRUE.
        END IF

        !Set the heatpump cycling rate
        UnitarySystem(UnitarySysNum)%MaxONOFFCyclesperHour = Numbers(iMaxONOFFCycPerHourNumericNum)
        IF(NumNumbers .LT. iMaxONOFFCycPerHourNumericNum)THEN
          UnitarySystem(UnitarySysNum)%MaxONOFFCyclesperHour = 2.5d0
        END IF

        !Set the heat pump time constant
        UnitarySystem(UnitarySysNum)%HPTimeConstant = Numbers(iHPTimeConstantNumericNum)
        IF(NumNumbers .LT. iHPTimeConstantNumericNum)THEN
          UnitarySystem(UnitarySysNum)%HPTimeConstant = 60.0d0
        END IF

        !Set the heat pump on-cycle power use fraction
        UnitarySystem(UnitarySysNum)%OnCyclePowerFraction = Numbers (iOnCyclePowerFracNumericNum)
        IF(NumNumbers .LT. iOnCyclePowerFracNumericNum)THEN
          UnitarySystem(UnitarySysNum)%OnCyclePowerFraction = 0.01d0
        END IF

        !Set the heat pump fan delay time
        UnitarySystem(UnitarySysNum)%FanDelayTime = Numbers(iFanDelayTimeNumericNum)
        IF(NumNumbers .LT. iFanDelayTimeNumericNum)THEN
          UnitarySystem(UnitarySysNum)%FanDelayTime = 60.0d0
        END IF

        UnitarySystem(UnitarySysNum)%AncillaryOnPower  = Numbers(iAncillaryOnPowerNumericNum)
        UnitarySystem(UnitarySysNum)%AncillaryOffPower = Numbers(iAncillaryOffPowerNumericNum)

        UnitarySystem(UnitarySysNum)%DesignHRWaterVolumeFlow = Numbers(iDesignHRWaterVolFlowNumericNum)
        UnitarySystem(UnitarySysNum)%MaxHROutletWaterTemp = Numbers(iMaxHROutletWaterTempNumericNum)

        IF(UnitarySystem(UnitarySysNum)%DesignHRWaterVolumeFlow .GT. 0.0d0)THEN
          UnitarySystem(UnitarySysNum)%HeatRecActive = .TRUE.
          ErrFlag=.FALSE.
          IF(.NOT. lAlphaBlanks(iHRWaterInletNodeAlphaNum) .AND. .NOT. lAlphaBlanks(iHRWaterOutletNodeAlphaNum))THEN
            UnitarySystem(UnitarySysNum)%HeatRecoveryInletNodeNum = &
              GetOnlySingleNode(Alphas(iHRWaterInletNodeAlphaNum),ErrFlag,'Unitary System Heat receovery',Alphas(iNameAlphaNum), &
                NodeType_Water,NodeConnectionType_Inlet,3,ObjectIsParent)
            UnitarySystem(UnitarySysNum)%HeatRecoveryOutletNodeNum = &
              GetOnlySingleNode(Alphas(iHRWaterOutletNodeAlphaNum),ErrFlag,'Unitary System Heat receovery',Alphas(iNameAlphaNum), &
                NodeType_Water,NodeConnectionType_Inlet,3,ObjectIsParent)

            CALL TestCompSet(CurrentModuleObject,Alphas(iNameAlphaNum),Alphas(iHRWaterInletNodeAlphaNum), &
                             Alphas(iHRWaterOutletNodeAlphaNum), 'Unitary System Heat receovery Nodes')

            IF (ErrFlag) THEN
              CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
              ErrorsFound=.TRUE.
            END IF
          ELSE
            CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
            CALL ShowContinueError('Illegal '//TRIM(cAlphaFields(iHRWaterInletNodeAlphaNum))//' = '// &
                                   TRIM(Alphas(iHRWaterInletNodeAlphaNum)))
            CALL ShowContinueError('Illegal '//TRIM(cAlphaFields(iHRWaterOutletNodeAlphaNum))//' = '// &
                                   TRIM(Alphas(iHRWaterOutletNodeAlphaNum)))
            CALL ShowContinueError('... heat recovery nodes must be specified when ' &
                                   //TRIM(cNumericFields(iDesignHRWaterVolFlowNumericNum))//' is greater than 0.')
            CALL ShowContinueError('... '//TRIM(cNumericFields(iDesignHRWaterVolFlowNumericNum))//' = '// &
                                   TRIM(TrimSigDigits(UnitarySystem(UnitarySysNum)%DesignHRWaterVolumeFlow,7)))
            ErrorsFound=.TRUE.
          END IF
        END IF

        IF(.NOT. lAlphaBlanks(iDesignSpecMSHPTypeAlphaNum) .AND. .NOT. lAlphaBlanks(iDesignSpecMSHPNameAlphaNum))THEN
          UnitarySystem(UnitarySysNum)%DesignSpecMultispeedHPType = Alphas(iDesignSpecMSHPTypeAlphaNum)
          UnitarySystem(UnitarySysNum)%DesignSpecMultispeedHPName = Alphas(iDesignSpecMSHPNameAlphaNum)

          UnitarySystem(UnitarySysNum)%DesignSpecMSHPIndex = &
            FindItemInList(UnitarySystem(UnitarySysNum)%DesignSpecMultispeedHPName,DesignSpecMSHP%Name,NumDesignSpecMultiSpeedHP)
          DesignSpecNum = UnitarySystem(UnitarySysNum)%DesignSpecMSHPIndex

          IF(DesignSpecNum .GT. 0)THEN

            IF (UnitarySystem(UnitarySysNum)%HeatingCoilType_Num == CoilDX_MultiSpeedHeating .OR. &
                UnitarySystem(UnitarySysNum)%HeatingCoilType_Num == Coil_HeatingElectric_MultiStage .OR. &
                UnitarySystem(UnitarySysNum)%HeatingCoilType_Num == Coil_HeatingGas_MultiStage) THEN
              UnitarySystem(UnitarySysNum)%NumOfSpeedHeating = DesignSpecMSHP(DesignSpecNum)%NumOfSpeedHeating
              ALLOCATE(UnitarySystem(UnitarySysNum)%HeatMassFlowRate(DesignSpecMSHP(DesignSpecNum)%NumOfSpeedHeating))
              ALLOCATE(UnitarySystem(UnitarySysNum)%HeatVolumeFlowRate(DesignSpecMSHP(DesignSpecNum)%NumOfSpeedHeating))
              ALLOCATE(UnitarySystem(UnitarySysNum)%MSHeatingSpeedRatio(DesignSpecMSHP(DesignSpecNum)%NumOfSpeedHeating))
              UnitarySystem(UnitarySysNum)%MSHeatingSpeedRatio = 1.0d0
            END IF

            IF (UnitarySystem(UnitarySysNum)%CoolingCoilType_Num == CoilDX_MultiSpeedCooling) THEN
              UnitarySystem(UnitarySysNum)%NumOfSpeedCooling = DesignSpecMSHP(DesignSpecNum)%NumOfSpeedCooling
              ALLOCATE(UnitarySystem(UnitarySysNum)%CoolMassFlowRate(DesignSpecMSHP(DesignSpecNum)%NumOfSpeedCooling))
              ALLOCATE(UnitarySystem(UnitarySysNum)%CoolVolumeFlowRate(DesignSpecMSHP(DesignSpecNum)%NumOfSpeedCooling))
              ALLOCATE(UnitarySystem(UnitarySysNum)%MSCoolingSpeedRatio(DesignSpecMSHP(DesignSpecNum)%NumOfSpeedCooling))
              UnitarySystem(UnitarySysNum)%MSCoolingSpeedRatio = 1.0d0
            END IF
          ELSE
            CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
            CALL ShowContinueError('... one or both of the following inputs are invalid.')
            CALL ShowContinueError('Field '//TRIM(cAlphaFields(iDesignSpecMSHPTypeAlphaNum))//' = '// &
                                   TRIM(Alphas(iDesignSpecMSHPTypeAlphaNum)))
            CALL ShowContinueError('Field '//TRIM(cAlphaFields(iDesignSpecMSHPNameAlphaNum))//' = '// &
                                   TRIM(Alphas(iDesignSpecMSHPNameAlphaNum)))
            ErrorsFound = .TRUE.
          END IF
        ELSEIF(lAlphaBlanks(iDesignSpecMSHPTypeAlphaNum) .AND. .NOT. lAlphaBlanks(iDesignSpecMSHPNameAlphaNum) .OR. &
                .NOT. lAlphaBlanks(iDesignSpecMSHPTypeAlphaNum) .AND. lAlphaBlanks(iDesignSpecMSHPNameAlphaNum))THEN
            CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
            CALL ShowContinueError('... one or both of the following inputs are invalid.')
            CALL ShowContinueError('Field '//TRIM(cAlphaFields(iDesignSpecMSHPTypeAlphaNum))//' = '// &
                                   TRIM(Alphas(iDesignSpecMSHPTypeAlphaNum)))
            CALL ShowContinueError('Field '//TRIM(cAlphaFields(iDesignSpecMSHPNameAlphaNum))//' = '// &
                                   TRIM(Alphas(iDesignSpecMSHPNameAlphaNum)))
            ErrorsFound = .TRUE.
        ELSEIF(UnitarySystem(UnitarySysNum)%NumOfSpeedHeating .GT. 0)THEN
          NumOfSpeedHeating = UnitarySystem(UnitarySysNum)%NumOfSpeedHeating

          ALLOCATE(UnitarySystem(UnitarySysNum)%HeatMassFlowRate(NumOfSpeedHeating))
          ALLOCATE(UnitarySystem(UnitarySysNum)%HeatVolumeFlowRate(NumOfSpeedHeating))
          ALLOCATE(UnitarySystem(UnitarySysNum)%MSHeatingSpeedRatio(NumOfSpeedHeating))
          UnitarySystem(UnitarySysNum)%MSHeatingSpeedRatio = 1.0d0

        ELSEIF(UnitarySystem(UnitarySysNum)%NumOfSpeedCooling .GT. 0)THEN
          NumOfSpeedCooling = UnitarySystem(UnitarySysNum)%NumOfSpeedCooling

          ALLOCATE(UnitarySystem(UnitarySysNum)%CoolMassFlowRate(NumOfSpeedCooling))
          ALLOCATE(UnitarySystem(UnitarySysNum)%CoolVolumeFlowRate(NumOfSpeedCooling))
          ALLOCATE(UnitarySystem(UnitarySysNum)%MSCoolingSpeedRatio(NumOfSpeedCooling))
          UnitarySystem(UnitarySysNum)%MSCoolingSpeedRatio = 1.0d0

        END IF

        IF(UnitarySystem(UnitarySysNum)%MultiSpeedCoolingCoil) THEN

           Index = UnitarySystem(UnitarySysNum)%DesignSpecMSHPIndex
           IF(Index .GT. 0) &
             UnitarySystem(UnitarySysNum)%NumOfSpeedCooling = DesignSpecMSHP(Index)%NumOfSpeedCooling

           IF (UnitarySystem(UnitarySysNum)%NumOfSpeedCooling == 0) THEN
             CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
             CALL ShowContinueError('... Cooling coil object type requires valid number of speeds for cooling to be specified')
            ErrorsFound = .TRUE.
           END IF
        END IF
        IF(UnitarySystem(UnitarySysNum)%MultiSpeedHeatingCoil) THEN

           Index = UnitarySystem(UnitarySysNum)%DesignSpecMSHPIndex
           IF(Index .GT. 0) &
             UnitarySystem(UnitarySysNum)%NumOfSpeedHeating = DesignSpecMSHP(Index)%NumOfSpeedHeating

           IF (UnitarySystem(UnitarySysNum)%NumOfSpeedHeating == 0) THEN
             CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
             CALL ShowContinueError('... Heating coil object type requires valid number of speeds for heating to be specified')
            ErrorsFound = .TRUE.
           END IF
        END IF

        ! set global logicals that denote coil type
        IF (UnitarySystem(UnitarySysNum)%MultiSpeedHeatingCoil .OR. UnitarySystem(UnitarySysNum)%VarSpeedHeatingCoil) THEN
          MultiOrVarSpeedHeatCoil(UnitarySysNum) = .TRUE.
        END IF
        IF (UnitarySystem(UnitarySysNum)%MultiSpeedCoolingCoil .OR. UnitarySystem(UnitarySysNum)%VarSpeedCoolingCoil) THEN
          MultiOrVarSpeedCoolCoil(UnitarySysNum) = .TRUE.
        END IF

      END DO  !End of the Unitary System Loop

      IF (ErrorsFound) THEN
          CALL ShowFatalError(RoutineName//'Errors found in input.  Program terminates.')
      END IF

      ! Setup Report variables for the Unitary System that are not reported in the components themselves
      DO UnitarySysNum=1,NumUnitarySystem
        CALL SetupOutputVariable('Unitary System Part Load Ratio []',UnitarySystem(UnitarySysNum)%PartLoadFrac, &
                             'System','Average',UnitarySystem(UnitarySysNum)%Name)
        CALL SetupOutputVariable('Unitary System Total Cooling Rate [W]',UnitarySystem(UnitarySysNum)%TotCoolEnergyRate, &
                             'System','Average',UnitarySystem(UnitarySysNum)%Name)
        CALL SetupOutputVariable('Unitary System Sensible Cooling Rate [W]',UnitarySystem(UnitarySysNum)%SensCoolEnergyRate, &
                             'System','Average',UnitarySystem(UnitarySysNum)%Name)
        CALL SetupOutputVariable('Unitary System Latent Cooling Rate [W]',UnitarySystem(UnitarySysNum)%LatCoolEnergyRate, &
                             'System','Average',UnitarySystem(UnitarySysNum)%Name)
        CALL SetupOutputVariable('Unitary System Total Heating Rate [W]',UnitarySystem(UnitarySysNum)%TotHeatEnergyRate, &
                             'System','Average',UnitarySystem(UnitarySysNum)%Name)
        CALL SetupOutputVariable('Unitary System Sensible Heating Rate [W]',UnitarySystem(UnitarySysNum)%SensHeatEnergyRate, &
                             'System','Average',UnitarySystem(UnitarySysNum)%Name)
        CALL SetupOutputVariable('Unitary System Latent Heating Rate [W]',UnitarySystem(UnitarySysNum)%LatHeatEnergyRate, &
                             'System','Average',UnitarySystem(UnitarySysNum)%Name)
        CALL SetupOutputVariable('Unitary System Ancillary Electric Power [W]', &
                              UnitarySystem(UnitarySysNum)%TotalAuxElecPower, &
                             'System','Average',UnitarySystem(UnitarySysNum)%Name)

!        IF(UnitarySystem(UnitarySysNum)%DehumidControlType_Num .EQ. DehumidControl_CoolReheat)THEN
          CALL SetupOutputVariable('Unitary System Dehumidification Induced Heating Demand Rate [W]', &
                                   UnitarySystem(UnitarySysNum)%DehumidInducedHeatingDemandRate, 'System','Average', &
                                   UnitarySystem(UnitarySysNum)%Name)
!        END IF

        IF(UnitarySystem(UnitarySysNum)%FanExists)THEN
          CALL SetupOutputVariable('Unitary System Fan Part Load Ratio []',UnitarySystem(UnitarySysNum)%FanPartLoadRatio, &
                             'System','Average',UnitarySystem(UnitarySysNum)%Name)
        END IF

        CALL SetupOutputVariable('Unitary System Compressor Part Load Ratio []', &
                                 UnitarySystem(UnitarySysNum)%CompPartLoadRatio, &
                                 'System','Average',UnitarySystem(UnitarySysNum)%Name)

        CALL SetupOutputVariable('Unitary System Frost Control Status []',UnitarySystem(UnitarySysNum)%FrostControlStatus, &
                                 'System','Average',UnitarySystem(UnitarySysNum)%Name)

        IF(UnitarySystem(UnitarySysNum)%HeatCoilExists)THEN
          CALL SetupOutputVariable('Unitary System Heating Ancillary Electric Energy [J]', &
                  UnitarySystem(UnitarySysNum)%HeatingAuxElecConsumption,'System','Sum',UnitarySystem(UnitarySysNum)%Name, &
                  ResourceTypeKey='Electric',EndUseKey='Heating',GroupKey='System')
        END IF

        SELECT CASE(UnitarySystem(UnitarySysNum)%CoolingCoilType_Num)
          CASE(CoilDX_CoolingTwoSpeed)
            CALL SetupOutputVariable('Unitary System Cycling Ratio []',UnitarySystem(UnitarySysNum)%CycRatio, &
                                'System','Average',UnitarySystem(UnitarySysNum)%Name)
            CALL SetupOutputVariable('Unitary System Compressor Speed Ratio []',UnitarySystem(UnitarySysNum)%SpeedRatio, &
                                'System','Average',UnitarySystem(UnitarySysNum)%Name)
          CASE(CoilDX_MultiSpeedCooling)
            CALL SetupOutputVariable('Unitary System Cooling Ancillary Electric Energy [J]', &
                     UnitarySystem(UnitarySysNum)%CoolingAuxElecConsumption,'System','Sum',UnitarySystem(UnitarySysNum)%Name, &
                     ResourceTypeKey='Electric',EndUseKey='Cooling',GroupKey='System')
            CALL SetupOutputVariable('Unitary System Electric Power [W]',UnitarySystem(UnitarySysNum)%ElecPower,&
                                     'System','Average',UnitarySystem(UnitarySysNum)%Name)
            CALL SetupOutputVariable('Unitary System Electric Energy [J]',UnitarySystem(UnitarySysNum)%ElecPowerConsumption, &
                                     'System','Sum',UnitarySystem(UnitarySysNum)%Name)
!            CALL SetupOutputVariable('Unitary System Cooling Coil Cycling Ratio []',UnitarySystem(UnitarySysNum)%CoolingCycRatio, &
!                                     'System','Average',UnitarySystem(UnitarySysNum)%Name)
!            CALL SetupOutputVariable('Unitary System Cooling Coil Speed Ratio []',UnitarySystem(UnitarySysNum)%CoolingSpeedRatio, &
!                                     'System','Average',UnitarySystem(UnitarySysNum)%Name)
!            CALL SetupOutputVariable('Unitary System Cooling Coil Speed Level []',UnitarySystem(UnitarySysNum)%CoolingSpeedNum, &
!                                     'System','Average',UnitarySystem(UnitarySysNum)%Name)
            IF (UnitarySystem(UnitarySysNum)%HeatRecActive) THEN
              CALL SetupOutputVariable('Unitary System Heat Recovery Rate [W]',UnitarySystem(UnitarySysNum)%HeatRecoveryRate, &
                      'System','Average',UnitarySystem(UnitarySysNum)%Name)
              CALL SetupOutputVariable('Unitary System Heat Recovery Inlet Temperature [C]', &
                      UnitarySystem(UnitarySysNum)%HeatRecoveryInletTemp,'System','Average',UnitarySystem(UnitarySysNum)%Name)
              CALL SetupOutputVariable('Unitary System Heat Recovery Outlet Temperature [C]', &
                      UnitarySystem(UnitarySysNum)%HeatRecoveryOutletTemp,'System','Average',UnitarySystem(UnitarySysNum)%Name)
              CALL SetupOutputVariable('Unitary System Heat Recovery Fluid Mass Flow Rate [kg/s]', &
                      UnitarySystem(UnitarySysNum)%HeatRecoveryMassFlowRate,'System','Average',UnitarySystem(UnitarySysNum)%Name)
              CALL SetupOutputVariable('Unitary System Heat Recovery Energy [J]', &
                      UnitarySystem(UnitarySysNum)%HeatRecoveryEnergy,'System','Sum',UnitarySystem(UnitarySysNum)%Name)
            END IF
          CASE (Coil_CoolingAirToAirVariableSpeed, Coil_CoolingWaterToAirHPVSEquationFit, &
                Coil_CoolingWaterToAirHPSimple, Coil_CoolingWaterToAirHP)
            CALL SetupOutputVariable('Unitary System Requested Sensible Cooling Rate [W]', &
                                  UnitarySystem(UnitarySysNum)%CoolingCoilSensDemand, &
                                  'System','Average',UnitarySystem(UnitarySysNum)%Name)
            CALL SetupOutputVariable('Unitary System Requested Latent Cooling Rate [W]', &
                                  UnitarySystem(UnitarySysNum)%CoolingCoilLatentDemand, &
                                  'System','Average',UnitarySystem(UnitarySysNum)%Name)
          CASE DEFAULT
        END SELECT

        SELECT CASE(UnitarySystem(UnitarySysNum)%HeatingCoilType_Num)
          CASE(CoilDX_MultiSpeedHeating, Coil_HeatingElectric_MultiStage, Coil_HeatingGas_MultiStage)
!            CALL SetupOutputVariable('Unitary System Heating Coil Cycling Ratio []', &
!                                      UnitarySystem(UnitarySysNum)%HeatingCycRatio, &
!                                     'System','Average',UnitarySystem(UnitarySysNum)%Name)
!            CALL SetupOutputVariable('Unitary System Heating Coil Speed Ratio []', &
!                                      UnitarySystem(UnitarySysNum)%HeatingSpeedRatio, &
!                                     'System','Average',UnitarySystem(UnitarySysNum)%Name)
!            CALL SetupOutputVariable('Unitary System Heating Coil Speed Level []', &
!                                      UnitarySystem(UnitarySysNum)%HeatingSpeedNum, &
!                                     'System','Average',UnitarySystem(UnitarySysNum)%Name)
          CASE (Coil_HeatingAirToAirVariableSpeed, Coil_HeatingWaterToAirHPVSEquationFit, &
                Coil_HeatingWaterToAirHPSimple, Coil_HeatingWaterToAirHP)
            CALL SetupOutputVariable('Unitary System Requested Heating Rate [W]', &
                                      UnitarySystem(UnitarySysNum)%HeatingCoilSensDemand, &
                                     'System','Average',UnitarySystem(UnitarySysNum)%Name)
          CASE DEFAULT
        END SELECT

        IF(UnitarySystem(UnitarySysNum)%CoolingCoilType_Num == CoilDX_MultiSpeedCooling .OR. &
           UnitarySystem(UnitarySysNum)%HeatingCoilType_Num == CoilDX_MultiSpeedHeating .OR. &
           UnitarySystem(UnitarySysNum)%HeatingCoilType_Num == Coil_HeatingElectric_MultiStage .OR. &
           UnitarySystem(UnitarySysNum)%HeatingCoilType_Num == Coil_HeatingGas_MultiStage) THEN
            CALL SetupOutputVariable('Unitary System DX Coil Cycling Ratio []',UnitarySystem(UnitarySysNum)%CycRatio, &
                                     'System','Average',UnitarySystem(UnitarySysNum)%Name)
            CALL SetupOutputVariable('Unitary System DX Coil Speed Ratio []',UnitarySystem(UnitarySysNum)%SpeedRatio, &
                                     'System','Average',UnitarySystem(UnitarySysNum)%Name)
            CALL SetupOutputVariable('Unitary System DX Coil Speed Level []',UnitarySystem(UnitarySysNum)%SpeedNum, &
                                     'System','Average',UnitarySystem(UnitarySysNum)%Name)
        END IF

        IF (AnyEnergyManagementSystemInModel) THEN
          CALL SetupEMSActuator('AirLoopHVAC:UnitarySystem', UnitarySystem(UnitarySysNum)%Name,   &
                                'Autosized Supply Air Flow Rate', '[m3/s]',                          &
                                UnitarySystem(UnitarySysNum)%DesignFanVolFlowRateEMSOverrideOn,   &
                                UnitarySystem(UnitarySysNum)%DesignFanVolFlowRateEMSOverrideValue )
          CALL SetupEMSActuator('AirLoopHVAC:UnitarySystem', UnitarySystem(UnitarySysNum)%Name,   &
                                'Autosized Supply Air Flow Rate During Cooling Operation', '[m3/s]', &
                                UnitarySystem(UnitarySysNum)%MaxCoolAirVolFlowEMSOverrideOn,      &
                                UnitarySystem(UnitarySysNum)%MaxCoolAirVolFlowEMSOverrideValue )
          CALL SetupEMSActuator('AirLoopHVAC:UnitarySystem', UnitarySystem(UnitarySysNum)%Name,   &
                                'Autosized Supply Air Flow Rate During Heating Operation', '[m3/s]', &
                                UnitarySystem(UnitarySysNum)%MaxHeatAirVolFlowEMSOverrideOn,      &
                                UnitarySystem(UnitarySysNum)%MaxHeatAirVolFlowEMSOverrideValue )
          CALL SetupEMSActuator('AirLoopHVAC:UnitarySystem', UnitarySystem(UnitarySysNum)%Name,   &
                                'Autosized Supply Air Flow Rate During No Heating or Cooling Operation', '[m3/s]', &
                                UnitarySystem(UnitarySysNum)%MaxNoCoolHeatAirVolFlowEMSOverrideOn, &
                                UnitarySystem(UnitarySysNum)%MaxNoCoolHeatAirVolFlowEMSOverrideValue )
        END IF

      END DO

      DEALLOCATE(Alphas)
      DEALLOCATE(cAlphaFields)
      DEALLOCATE(cNumericFields)
      DEALLOCATE(Numbers)
      DEALLOCATE(lAlphaBlanks)
      DEALLOCATE(lNumericBlanks)

      IF (AnyEnergyManagementSystemInModel) THEN
        DO UnitarySysNum = 1, NumUnitarySystem
          CALL SetupEMSInternalVariable('Unitary HVAC Design Heating Capacity', UnitarySystem(UnitarySysNum)%Name, '[W]', &
                        UnitarySystem(UnitarySysNum)%DesignHeatingCapacity  )
          CALL SetupEMSInternalVariable('Unitary HVAC Design Cooling Capacity', UnitarySystem(UnitarySysNum)%Name, '[W]', &
                        UnitarySystem(UnitarySysNum)%DesignCoolingCapacity  )
          CALL SetupEMSActuator('Unitary HVAC', UnitarySystem(UnitarySysNum)%Name, 'Sensible Load Request' , '[W]', &
                        UnitarySystem(UnitarySysNum)%EMSOverrideSensZoneLoadRequest, &
                        UnitarySystem(UnitarySysNum)%EMSSensibleZoneLoadValue )
          CALL SetupEMSActuator('Unitary HVAC', UnitarySystem(UnitarySysNum)%Name, 'Moisture Load Request' , '[W]', &
                        UnitarySystem(UnitarySysNum)%EMSOverrideMoistZoneLoadRequest, &
                        UnitarySystem(UnitarySysNum)%EMSMoistureZoneLoadValue )

        END DO
      END IF

      CALL ManageEMS(emsCALLFromComponentGetInput)


  RETURN

END SUBROUTINE GetUnitarySystemInput


! End of Get Input subroutines for the Module
!******************************************************************************


! Beginning of Calculation subroutines for the DXCoolingSystem Module
! *****************************************************************************

SUBROUTINE ControlUnitarySystemtoSP(UnitarySysNum, FirstHVACIteration, AirLoopNum,OAUCoilOutTemp,HXUnitOn)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Raustad, FSEC
          !       DATE WRITTEN   February 2013
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine manages DXCoolingSystem component simulation.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE Fans,             ONLY: SimulateFanComponents

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)          :: UnitarySysNum       ! Index of AirloopHVAC:UnitarySystem object
  LOGICAL, INTENT(IN)          :: FirstHVACIteration  ! True when first HVAC iteration
  INTEGER, INTENT(IN)          :: AirLoopNum          ! Primary air loop number
  REAL(r64), INTENT(IN), OPTIONAL :: OAUCoilOutTemp   ! the coil inlet temperature of OutdoorAirUnit
  LOGICAL, INTENT(INOUT), OPTIONAL :: HXUnitOn        ! Flag to control HX for HXAssisted Cooling Coil

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(R64)                     :: PartLoadRatio         ! coil operating part-load ratio
  REAL(R64)                     :: OnOffAirFlowRatio     ! Setpoint based coil control does not use this variable
  REAL(R64)                     :: CoilCoolHeatRat       ! ratio of cooling to heating PLR for cycling fan RH control
  INTEGER                       :: CompOn                ! compressor control (0=off, 1=on)

  OnOffAirFlowRatio = 1.0d0
  CoilCoolHeatRat   = 1.0d0

  !CALL the series of components that simulate a Unitary System
  IF (UnitarySystem(UnitarySysNum)%FanExists .AND. UnitarySystem(UnitarySysNum)%FanPlace .EQ. BlowThru) THEN
    CALL SimulateFanComponents(Blank,FirstHVACIteration,UnitarySystem(UnitarySysNum)%FanIndex,FanSpeedRatio)
  END IF

  IF(UnitarySystem(UnitarySysNum)%CoolingCoilUpstream)THEN

    IF (UnitarySystem(UnitarySysNum)%CoolCoilExists)THEN
      CALL UpdateUnitarySystemControl(UnitarySysNum,AirLoopNum, &
                            UnitarySystem(UnitarySysNum)%CoolCoilOutletNodeNum, &
                            UnitarySystem(UnitarySysNum)%SystemCoolControlNodeNum, &
                            OnOffAirFlowRatio,OAUCoilOutTemp,FirstHVACIteration)
      CALL ControlCoolingSystem(UnitarySysNum, FirstHVACIteration, HXUnitOn)
      PartLoadRatio = UnitarySystem(UnitarySysNum)%CoolingPartLoadFrac
      CompOn = 0
      IF(PartLoadRatio > 0.0d0)CompOn = 1
      CALL CalcUnitaryCoolingSystem(UnitarySysNum,FirstHVACIteration,PartLoadRatio, &
                                    CompOn,OnOffAirFlowRatio,CoilCoolHeatRat,HXUnitOn)
    END IF
    IF (UnitarySystem(UnitarySysNum)%HeatCoilExists)THEN
      CALL UpdateUnitarySystemControl(UnitarySysNum,AirLoopNum, &
                            UnitarySystem(UnitarySysNum)%HeatCoilOutletNodeNum, &
                            UnitarySystem(UnitarySysNum)%SystemHeatControlNodeNum, &
                            OnOffAirFlowRatio,OAUCoilOutTemp,FirstHVACIteration, &
                            MaxOutletTemp=UnitarySystem(UnitarySysNum)%DesignMaxOutletTemp)
      CALL ControlHeatingSystem(UnitarySysNum, FirstHVACIteration)
      PartLoadRatio = UnitarySystem(UnitarySysNum)%HeatingPartLoadFrac
      CompOn = 0
      IF(PartLoadRatio > 0.0d0)CompOn = 1
      CALL CalcUnitaryHeatingSystem(UnitarySysNum,FirstHVACIteration,PartLoadRatio, CompOn, OnOffAirFlowRatio)
    END IF

  ELSE

    IF (UnitarySystem(UnitarySysNum)%HeatCoilExists)THEN
      CALL UpdateUnitarySystemControl(UnitarySysNum,AirLoopNum, &
                            UnitarySystem(UnitarySysNum)%HeatCoilOutletNodeNum, &
                            UnitarySystem(UnitarySysNum)%SystemHeatControlNodeNum, &
                            OnOffAirFlowRatio,OAUCoilOutTemp,FirstHVACIteration, &
                            MaxOutletTemp=UnitarySystem(UnitarySysNum)%DesignMaxOutletTemp)
      CALL ControlHeatingSystem(UnitarySysNum, FirstHVACIteration)
      PartLoadRatio = UnitarySystem(UnitarySysNum)%HeatingPartLoadFrac
      CompOn = 0
      IF(PartLoadRatio > 0.0d0)CompOn = 1
      CALL CalcUnitaryHeatingSystem(UnitarySysNum,FirstHVACIteration,PartLoadRatio, CompOn, OnOffAirFlowRatio)
    END IF
    IF (UnitarySystem(UnitarySysNum)%CoolCoilExists)THEN
      CALL UpdateUnitarySystemControl(UnitarySysNum,AirLoopNum, &
                            UnitarySystem(UnitarySysNum)%CoolCoilOutletNodeNum, &
                            UnitarySystem(UnitarySysNum)%SystemCoolControlNodeNum, &
                            OnOffAirFlowRatio,OAUCoilOutTemp,FirstHVACIteration)
      CALL ControlCoolingSystem(UnitarySysNum, FirstHVACIteration, HXUnitOn)
      PartLoadRatio = UnitarySystem(UnitarySysNum)%CoolingPartLoadFrac
      CompOn = 0
      IF(PartLoadRatio > 0.0d0)CompOn = 1
      CALL CalcUnitaryCoolingSystem(UnitarySysNum,FirstHVACIteration,PartLoadRatio, &
                                    CompOn,OnOffAirFlowRatio,CoilCoolHeatRat,HXUnitOn)
    END IF

  END IF

  IF (UnitarySystem(UnitarySysNum)%FanExists .AND. UnitarySystem(UnitarySysNum)%FanPlace .EQ. DrawThru) THEN
    CALL SimulateFanComponents(Blank,FirstHVACIteration,UnitarySystem(UnitarySysNum)%FanIndex,FanSpeedRatio)
  END IF

  IF (UnitarySystem(UnitarySysNum)%SuppCoilExists)THEN
    SuppHeatingCoilFlag = .TRUE.
    CALL UpdateUnitarySystemControl(UnitarySysNum,AirLoopNum, &
                          UnitarySystem(UnitarySysNum)%SuppCoilAirOutletNode, &
                          UnitarySystem(UnitarySysNum)%SuppHeatControlNodeNum, &
                          OnOffAirFlowRatio,OAUCoilOutTemp,FirstHVACIteration, &
                          MaxOutletTemp=UnitarySystem(UnitarySysNum)%DesignMaxOutletTemp)
    CALL ControlSuppHeatSystem(UnitarySysNum, FirstHVACIteration)
    CALL CalcUnitarySuppSystemToSP(UnitarySysNum,FirstHVACIteration)
    SuppHeatingCoilFlag = .FALSE.
  END IF


  UnitarySystem(UnitarySysNum)%InitHeatPump = .FALSE.
  RETURN

END SUBROUTINE ControlUnitarySystemtoSP

SUBROUTINE ControlUnitarySystemtoLoad(UnitarySysNum, FirstHVACIteration, AirLoopNum,CompOn,OAUCoilOutTemp,HXUnitOn)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Raustad, FSEC
          !       DATE WRITTEN   February 2013
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine manages DXCoolingSystem component simulation.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataAirLoop, ONLY: AirLoopControlInfo
  USE PlantUtilities,    ONLY: SetComponentFlowRate

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)              :: UnitarySysNum    ! Index of AirloopHVAC:UnitarySystem object
  LOGICAL, INTENT(IN)              :: FirstHVACIteration  ! True when first HVAC iteration
  INTEGER, INTENT(IN)              :: AirLoopNum          ! Primary air loop number
  INTEGER, INTENT(INOUT)           :: CompOn              ! Determines if compressor is on or off
  REAL(r64), INTENT(IN),  OPTIONAL :: OAUCoilOutTemp      ! the coil inlet temperature of OutdoorAirUnit
  LOGICAL, INTENT(INOUT), OPTIONAL :: HXUnitOn            ! Flag to control HX for HXAssisted Cooling Coil

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(R64)                     :: CoolPLR          ! cooling part-load ratio
  REAL(R64)                     :: HeatPLR          ! heating part-load ratio
  REAL(R64)                     :: SuppPLR          ! supplemental heating part-load ratio
  REAL(R64)                     :: SensOutput       ! Sensible output of Unitary System (W)
  REAL(R64)                     :: LatOutput        ! Latent output of Unitary System (W)
  REAL(R64)                     :: ZoneLoad           ! zone load (W)
  REAL(R64)                     :: OnOffAirFlowRatio  ! ratio of on to off air flow
  REAL(R64)                     :: QZnReq             ! zone load (W)
  REAL(R64)                     :: FullSensibleOutput ! sensible output of Unitary System (W)
!  REAL(R64)                     :: FullLatentOutput   ! latent output of Unitary System (W)
  REAL(R64)                     :: SupHeaterLoad    ! additional heating required by supplemental heater (W)
  REAL(r64)                     :: HeatCoilLoad     ! load pass to heating coil (W)

  OnOffAirFlowRatio = 1.0d0
  QZnReq = 0.0d0
  CALL UpdateUnitarySystemControl(UnitarySysNum,AirLoopNum, &
                            UnitarySystem(UnitarySysNum)%CoolCoilOutletNodeNum, &
                            UnitarySystem(UnitarySysNum)%SystemCoolControlNodeNum, OnOffAirFlowRatio, &
                            OAUCoilOutTemp, FirstHVACIteration, ZoneLoad)

! will not be running supplemental heater on this CALL (simulate with supplemental heater off)
  FullSensibleOutput = 0.0d0
  ! using furnace module logic
  ! first check to see if cycling fan with economizer can meet the load
  IF(AirLoopNum > 0)THEN
    IF ( UnitarySystem(UnitarySysNum)%CoolCoilExists .AND. UnitarySystem(UnitarySysNum)%HeatCoilExists .AND. &
        UnitarySystem(UnitarySysNum)%CoolingCoilType_Num /= Coil_CoolingAirToAirVariableSpeed .AND. &
        UnitarySystem(UnitarySysNum)%HeatingCoilType_Num /= Coil_HeatingAirToAirVariableSpeed .AND. &
        .NOT. FirstHVACIteration .AND. UnitarySystem(UnitarySysNum)%FanOpMode == CycFanCycCoil .AND. CoolingLoad &
         .AND. AirLoopControlInfo(AirLoopNum)%EconoActive) THEN
      CompOn=0
      CALL ControlUnitarySystemOutput(UnitarySysNum, FirstHVACIteration, OnOffAirFlowRatio, &
                                    HXUnitOn, ZoneLoad, FullSensibleOutput, CompOn)
      IF (UnitarySystem(UnitarySysNum)%CoolingPartLoadFrac >= 1.0d0 .OR. UnitarySystem(UnitarySysNum)%HeatingPartLoadFrac >= 1.0d0 &
           .OR. (UnitarySystem(UnitarySysNum)%CoolingPartLoadFrac <= 0.0d0 .AND. &
                 UnitarySystem(UnitarySysNum)%HeatingPartLoadFrac <= 0.0d0)) THEN
        CompOn=1
        CALL ControlUnitarySystemOutput(UnitarySysNum, FirstHVACIteration, OnOffAirFlowRatio, &
                                      HXUnitOn, ZoneLoad, FullSensibleOutput, CompOn)
      END IF
    ELSE
      CompOn = 1
      CALL ControlUnitarySystemOutput(UnitarySysNum, FirstHVACIteration, OnOffAirFlowRatio, &
                                  HXUnitOn, ZoneLoad, FullSensibleOutput, CompOn)
    END IF
  ELSE
    CompOn = 1
    CALL ControlUnitarySystemOutput(UnitarySysNum, FirstHVACIteration, OnOffAirFlowRatio, &
                                  HXUnitOn, ZoneLoad, FullSensibleOutput, CompOn)
  END IF
  CoolPLR = UnitarySystem(UnitarySysNum)%CoolingPartLoadFrac
  HeatPLR = UnitarySystem(UnitarySysNum)%HeatingPartLoadFrac
  HeatCoilLoad = HeatPLR * UnitarySystem(UnitarySysNum)%DesignHeatingCapacity
  SensOutput = 0.0d0
  LatOutput  = 0.0d0

  IF(UnitarySystem(UnitarySysNum)%CoolCoilFluidInletNode .GT. 0)THEN
    CALL SetComponentFlowRate( Node(UnitarySystem(UnitarySysNum)%CoolCoilFluidInletNode)%MassFlowRate, &
                               UnitarySystem(UnitarySysNum)%CoolCoilFluidInletNode,  &
                               UnitarySystem(UnitarySysNum)%CoolCoilFluidOutletNodeNum,&
                               UnitarySystem(UnitarySysNum)%CoolCoilLoopNum, &
                               UnitarySystem(UnitarySysNum)%CoolCoilLoopSide, &
                               UnitarySystem(UnitarySysNum)%CoolCoilBranchNum, &
                               UnitarySystem(UnitarySysNum)%CoolCoilCompNum )
  END IF
  IF(UnitarySystem(UnitarySysNum)%HeatCoilFluidInletNode .GT. 0)THEN
    CALL SetComponentFlowRate( Node(UnitarySystem(UnitarySysNum)%HeatCoilFluidInletNode)%MassFlowRate, &
                               UnitarySystem(UnitarySysNum)%HeatCoilFluidInletNode,  &
                               UnitarySystem(UnitarySysNum)%HeatCoilFluidOutletNodeNum,&
                               UnitarySystem(UnitarySysNum)%HeatCoilLoopNum, &
                               UnitarySystem(UnitarySysNum)%HeatCoilLoopSide, &
                               UnitarySystem(UnitarySysNum)%HeatCoilBranchNum, &
                               UnitarySystem(UnitarySysNum)%HeatCoilCompNum )
  END IF

  IF (UnitarySystem(UnitarySysNum)%SuppCoilExists .AND. (HeatingLoad .OR. CoolingLoad .OR. MoistureLoad<0.0d0)) THEN
    IF((FullSensibleOutput < (QToHeatSetPt - SmallLoad)) .AND. .NOT. FirstHVACIteration)THEN
      SupHeaterLoad = MAX(0.0d0,QToHeatSetPt - FullSensibleOutput)
      UnitarySystem(UnitarySysNum)%SupHeaterLoad = 0.0d0
! what does this line even do? I know we want the supplemental heater on only if there is a dehum load,
! but for HP's the supp heater should also run if the heating coil can't turn on
! (i.e., this line calc's a supp heater load, then next line also calc's it?)
      IF (MoistureLoad < 0.0d0) &
        UnitarySystem(UnitarySysNum)%SupHeaterLoad = SupHeaterLoad
! so it look's like this next line should only be valid for HP's.
      IF(UnitarySystem(UnitarySysNum)%DesignSuppHeatingCapacity .GT. 0.0d0)THEN
        UnitarySystem(UnitarySysNum)%SuppHeatPartLoadFrac = &
             MIN(1.0d0,SupHeaterLoad/UnitarySystem(UnitarySysNum)%DesignSuppHeatingCapacity)
      END IF
    ELSE
      SupHeaterLoad = 0.0d0
      UnitarySystem(UnitarySysNum)%SuppHeatPartLoadFrac = 0.0d0
    END IF
  ELSE
    SupHeaterLoad = 0.0d0
    UnitarySystem(UnitarySysNum)%SuppHeatPartLoadFrac = 0.0d0
  END IF

  CALL CalcUnitarySystemToLoad(UnitarySysNum,FirstHVACIteration,CoolPLR,HeatPLR, &
         OnOffAirFlowRatio,SensOutput,LatOutput,HXUnitOn,HeatCoilLoad=HeatCoilLoad, &
         SuppCoilLoad=SupHeaterLoad,CompOn=CompOn)

  ! check supplemental heating coil outlet temp based on maximum allowed
  IF(UnitarySystem(UnitarySysNum)%SuppCoilExists)THEN
    SuppPLR = UnitarySystem(UnitarySysNum)%SuppHeatPartLoadFrac
    ! only need to test for high supply air temp if supplemental coil is operating
    IF(SuppPLR > 0.0d0)THEN
      CALL CalcUnitarySystemToLoad(UnitarySysNum,FirstHVACIteration,CoolPLR,HeatPLR,OnOffAirFlowRatio, &
             SensOutput,LatOutput,HXUnitOn,HeatCoilLoad=HeatCoilLoad,SuppCoilLoad=SupHeaterLoad, CompOn=CompOn)
      IF(UnitarySystem(UnitarySysNum)%DesignSuppHeatingCapacity .GT. 0.0d0)THEN
        UnitarySystem(UnitarySysNum)%SuppHeatPartLoadFrac = SupHeaterLoad/UnitarySystem(UnitarySysNum)%DesignSuppHeatingCapacity
      ELSE
        UnitarySystem(UnitarySysNum)%SuppHeatPartLoadFrac = 0.0d0
      END IF
    END IF
  END IF

  IF(UnitarySystem(UnitarySysNum)%SuppCoilFluidInletNode .GT. 0)THEN
    CALL SetComponentFlowRate( Node(UnitarySystem(UnitarySysNum)%SuppCoilFluidInletNode)%MassFlowRate, &
                               UnitarySystem(UnitarySysNum)%SuppCoilFluidInletNode,  &
                               UnitarySystem(UnitarySysNum)%SuppCoilFluidOutletNodeNum,&
                               UnitarySystem(UnitarySysNum)%SuppCoilLoopNum, &
                               UnitarySystem(UnitarySysNum)%SuppCoilLoopSide, &
                               UnitarySystem(UnitarySysNum)%SuppCoilBranchNum, &
                               UnitarySystem(UnitarySysNum)%SuppCoilCompNum )
  END IF

  IF (UnitarySystem(UnitarySysNum)%HeatRecActive) THEN
    CALL SetComponentFlowRate(Node(UnitarySystem(UnitarySysNum)%HeatRecoveryInletNodeNum)%MassFlowRate, &
                              UnitarySystem(UnitarySysNum)%HeatRecoveryInletNodeNum, &
                              UnitarySystem(UnitarySysNum)%HeatRecoveryOutletNodeNum, &
                              UnitarySystem(UnitarySysNum)%HRLoopNum, &
                              UnitarySystem(UnitarySysNum)%HRLoopSideNum, &
                              UnitarySystem(UnitarySysNum)%HRBranchNum, &
                              UnitarySystem(UnitarySysNum)%HRCompNum )
  END IF

  RETURN

END SUBROUTINE ControlUnitarySystemtoLoad

SUBROUTINE ControlUnitarySystemOutput(UnitarySysNum, FirstHVACIteration, OnOffAirFlowRatio, HXUnitOn, &
                                      ZoneLoad, FullSensibleOutput, CompOn)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Raustad, FSEC
          !       DATE WRITTEN   February 2013
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine determines operating PLR and calculates the load based system output.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE General,           ONLY: SolveRegulaFalsi, TrimSigDigits
  USE DataHeatBalFanSys, ONLY: TempControlType
  USE Psychrometrics,    ONLY: PsyCpAirFnWTdb

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)              :: UnitarySysNum    ! Index of AirloopHVAC:UnitarySystem object
  LOGICAL, INTENT(IN)              :: FirstHVACIteration  ! True when first HVAC iteration
  REAL(R64), INTENT(INOUT)         :: OnOffAirFlowRatio   ! ratio of heating PLR to cooling PLR (is this correct?)
  LOGICAL, INTENT(INOUT), OPTIONAL :: HXUnitOn            ! Flag to control HX for HXAssisted Cooling Coil
  REAL(R64), INTENT(IN)            :: ZoneLoad
  REAL(R64), INTENT(INOUT)         :: FullSensibleOutput
  INTEGER, OPTIONAL, INTENT(INOUT) :: CompOn

          ! SUBROUTINE PARAMETER DEFINITIONS:
  INTEGER,   PARAMETER  ::  MaxIter = 100        ! maximum number of iterations
  REAL(r64), PARAMETER  ::  MinPLR  = 0.0d0      ! minimum part load ratio allowed

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  CHARACTER(len=MaxNameLength)  :: CompName           ! Name of Unitary System object
  INTEGER                       :: InletNode          ! DX System inlet node number
  INTEGER                       :: OutletNode         ! DX System outlet node number
  INTEGER                       :: OpMode             ! fan operating mode (cycling or constant)
  REAL(R64)                     :: PartLoadRatio      ! operating part-load ratio
  REAL(R64)                     :: SensOutputOff      ! sensible output at PLR = 0 [W]
  REAL(R64)                     :: LatOutputOff       ! latent output at PLR = 0 [W]
  REAL(R64)                     :: SensOutputOn       ! sensible output at PLR = 1 [W]
  REAL(R64)                     :: LatOutputOn        ! latent output at PLR = 1 [W]
  REAL(R64)                     :: CoolPLR            ! cooing part load ratio
  REAL(R64)                     :: HeatPLR            ! heating part load ratio
  REAL(r64), DIMENSION(10)      :: Par                ! parameters passed to RegulaFalsi function
  INTEGER                       :: SolFlag            ! return flag from RegulaFalsi for sensible load
  INTEGER                       :: SolFlagLat         ! return flag from RegulaFalsi for latent load
  REAL(r64)                     :: TempLoad           ! represents either a sensible or latent load [W]
  REAL(r64)                     :: TempSysOutput      ! represents either a sensible or latent capacity [W]
  REAL(R64)                     :: TempSensOutput     ! iterative sensible capacity [W]
  REAL(R64)                     :: TempLatOutput      ! iterative latent capacity [W]
  REAL(R64)                     :: TempMinPLR         ! iterative minimum PLR
  REAL(R64)                     :: TempMaxPLR         ! iterative maximum PLR
  INTEGER                       :: SpeedNum           ! multi-speed coil speed number
  INTEGER                       :: CompressorONFlag   ! 0= compressor off, 1= compressor off
  REAL(r64)                     :: CoolingOnlySensibleOutput ! use to calculate dehumidification induced heating [W]
  REAL(r64)                     :: CpAir                     ! specific heat of air [J/kg_C]

  CompName = UnitarySystem(UnitarySysNum)%Name
  InletNode = UnitarySystem(UnitarySysNum)%UnitarySystemInletNodeNum
  OutletNode = UnitarySystem(UnitarySysNum)%UnitarySystemOutletNodeNum
  OpMode = UnitarySystem(UnitarySysNum)%FanOpMode
  ! Pass full mass flow rate on FirstHVACIteration to set MassFlowRateMax
  IF(FirstHVACIteration .AND. UnitarySystem(UnitarySysNum)%AirLoopEquipment)THEN
    IF (UnitarySystem(UnitarySysNum)%CoolingCoilType_Num == CoilDX_CoolingSingleSpeed .OR. &
        UnitarySystem(UnitarySysNum)%CoolingCoilType_Num == CoilDX_CoolingHXAssisted .OR. &
        UnitarySystem(UnitarySysNum)%HeatingCoilType_Num == CoilDX_HeatingEmpirical .OR. &
        UnitarySystem(UnitarySysNum)%HeatingCoilType_Num == Coil_HeatingAirToAirVariableSpeed .OR. &
        UnitarySystem(UnitarySysNum)%CoolingCoilType_Num == Coil_CoolingAirToAirVariableSpeed .OR. &
        UnitarySystem(UnitarySysNum)%CoolingCoilType_Num == Coil_CoolingWaterToAirHPSimple .OR. &
        UnitarySystem(UnitarySysNum)%HeatingCoilType_Num == Coil_HeatingWaterToAirHPSimple) THEN
      PartLoadRatio = 1.0d0
      IF(HeatingLoad)THEN
        UnitarySystem(UnitarySysNum)%HeatingSpeedNum = UnitarySystem(UnitarySysNum)%NumOfSpeedHeating
        UnitarySystem(UnitarySysNum)%HeatingSpeedRatio = 1.0d0
      ELSE
!        CoolingLoad = .TRUE.
        UnitarySystem(UnitarySysNum)%CoolingSpeedNum = UnitarySystem(UnitarySysNum)%NumOfSpeedCooling
        UnitarySystem(UnitarySysNum)%CoolingSpeedRatio = 1.0d0
      END IF
    ELSE
      IF(HeatingLoad)THEN
        PartLoadRatio = 0.0d0
        UnitarySystem(UnitarySysNum)%HeatingSpeedNum = UnitarySystem(UnitarySysNum)%NumOfSpeedHeating
        UnitarySystem(UnitarySysNum)%HeatingSpeedRatio = 1.0d0
        IF (UnitarySystem(UnitarySysNum)%Staged .AND. ABS(UnitarySystem(UnitarySysNum)%StageNum) .LT. &
            UnitarySystem(UnitarySysNum)%NumOfSpeedHeating) THEN
          UnitarySystem(UnitarySysNum)%HeatingSpeedNum = ABS(UnitarySystem(UnitarySysNum)%StageNum)
          IF (UnitarySystem(UnitarySysNum)%HeatingSpeedNum == 1) UnitarySystem(UnitarySysNum)%HeatingSpeedRatio = 0.0d0
        END IF
      ELSE
!        CoolingLoad = .TRUE.
        PartLoadRatio = 1.0d0
        UnitarySystem(UnitarySysNum)%CoolingSpeedNum = UnitarySystem(UnitarySysNum)%NumOfSpeedCooling
        UnitarySystem(UnitarySysNum)%CoolingSpeedRatio = 1.0d0
        IF (UnitarySystem(UnitarySysNum)%Staged .AND. ABS(UnitarySystem(UnitarySysNum)%StageNum) .LT. &
            UnitarySystem(UnitarySysNum)%NumOfSpeedCooling) THEN
          UnitarySystem(UnitarySysNum)%CoolingSpeedNum = ABS(UnitarySystem(UnitarySysNum)%StageNum)
          IF (UnitarySystem(UnitarySysNum)%CoolingSpeedNum == 1) UnitarySystem(UnitarySysNum)%CoolingSpeedRatio = 0.0d0
        END IF
      END IF
    END IF
    CALL SetOnOffMassFlowRate(UnitarySysNum, OnOffAirFlowRatio, PartLoadRatio)
    IF(HeatingLoad)THEN
      UnitarySystem(UnitarySysNum)%HeatingPartLoadFrac=1.0d0
    ELSE
      UnitarySystem(UnitarySysNum)%CoolingPartLoadFrac=1.0d0
    END IF
    CompOn=0
    RETURN
  END IF

  IF(GetCurrentScheduleValue(UnitarySystem(UnitarySysNum)%SysAvailSchedPtr) .LE. 0.0d0)THEN
    RETURN
  END IF

  PartLoadRatio = 0.0d0   ! Get no load result
  SolFlag = 0    ! # of iterations IF positive, -1 means failed to converge, -2 means bounds are incorrect
  SolFlagLat = 0    ! # of iterations IF positive, -1 means failed to converge, -2 means bounds are incorrect

  SensOutputOff = 0.0d0
  LatOutputOff  = 0.0d0
  CoolPLR       = 0.0d0
  HeatPLR       = 0.0d0
  CompressorONFlag = 0

  UnitarySystem(UnitarySysNum)%WSHPRuntimeFrac   = 0.0d0

  CALL SetOnOffMassFlowRate(UnitarySysNum, OnOffAirFlowRatio, PartLoadRatio)

  IF (HeatingLoad) THEN
    UnitarySystem(UnitarySysNum)%HeatingCoilSensDemand   = ZoneLoad
    UnitarySystem(UnitarySysNum)%CoolingCoilSensDemand   = 0.0d0
    UnitarySystem(UnitarySysNum)%CoolingCoilLatentDemand = 0.0d0
  ELSE IF(CoolingLoad .OR. MoistureLoad < 0.0d0)THEN
    UnitarySystem(UnitarySysNum)%HeatingCoilSensDemand   = 0.0d0
    IF(CoolingLoad)THEN
      UnitarySystem(UnitarySysNum)%CoolingCoilSensDemand   = ABS(ZoneLoad)
    ELSE
      UnitarySystem(UnitarySysNum)%CoolingCoilSensDemand   = 0.0d0
    END IF
    UnitarySystem(UnitarySysNum)%CoolingCoilLatentDemand = ABS(MoistureLoad)
  ELSE
    RETURN
  END IF

  CALL CalcUnitarySystemToLoad(UnitarySysNum,FirstHVACIteration,CoolPLR,HeatPLR, &
                               OnOffAirFlowRatio,SensOutputOff,LatOutputOff,HXUnitOn,CompOn=CompressorONFlag)
  FullSensibleOutput = SensOutputOff

  IF(.NOT. HeatingLoad .AND. .NOT. CoolingLoad) THEN
    ! no load
    IF(MoistureLoad >= 0.0d0 .OR. MoistureLoad > LatOutputOff) RETURN
    ! Dehumcontrol_Multimode only controls RH if there is a sensible load
    IF(UnitarySystem(UnitarySysNum)%DehumidControlType_Num .EQ. DehumidControl_Multimode) RETURN
  END IF

! determine if PLR=0 meets the load
  SELECT CASE (TempControlType(UnitarySystem(UnitarySysNum)%ControlZoneNum))
    CASE(SingleHeatingSetPoint)
      IF(HeatingLoad .AND. SensOutputOff .GT. ZoneLoad .AND. (MoistureLoad >= 0.0d0 .OR. MoistureLoad .GT. LatOutputOff))RETURN
      IF(.NOT. HeatingLoad .AND. (MoistureLoad >= 0.0d0 .OR. MoistureLoad .GT. LatOutputOff))RETURN
    CASE(SingleCoolingSetPoint)
      IF(CoolingLoad .AND. SensOutputOff .LT. ZoneLoad .AND. (MoistureLoad >= 0.0d0 .OR. MoistureLoad .GT. LatOutputOff))RETURN
      IF(.NOT. CoolingLoad .AND. (MoistureLoad >= 0.0d0 .OR. MoistureLoad .GT. LatOutputOff))RETURN
    CASE(SingleHeatCoolSetPoint, DualSetPointWithDeadBand)
      IF(HeatingLoad .AND. SensOutputOff .GT. ZoneLoad .AND. (MoistureLoad >= 0.0d0 .OR. MoistureLoad .GT. LatOutputOff))RETURN
      IF(CoolingLoad .AND. SensOutputOff .LT. ZoneLoad .AND. (MoistureLoad >= 0.0d0 .OR. MoistureLoad .GT. LatOutputOff))RETURN
      IF(.NOT. HeatingLoad .AND. .NOT. CoolingLoad .AND. (MoistureLoad >= 0.0d0 .OR. MoistureLoad .GT. LatOutputOff))RETURN
    CASE DEFAULT
      ! should never get here
  END SELECT

! if a variable speed unit, the SensOutputOff at SpeedNum=1 must be checked to see if it exceeds the ZoneLoad
! This is still no load but at the first speed above idle
  IF(HeatingLoad .AND. UnitarySystem(UnitarySysNum)%NumOfSpeedHeating > 0 .OR. &
     CoolingLoad .AND. UnitarySystem(UnitarySysNum)%NumOfSpeedCooling > 0)THEN
    IF(UnitarySystem(UnitarySysNum)%Staged)THEN
      IF(HeatingLoad)THEN
        UnitarySystem(UnitarySysNum)%HeatingSpeedNum   = UnitarySystem(UnitarySysNum)%StageNum
      ELSE
        UnitarySystem(UnitarySysNum)%CoolingSpeedNum   = ABS(UnitarySystem(UnitarySysNum)%StageNum)
      END IF
    ELSE
      IF(HeatingLoad)THEN
        UnitarySystem(UnitarySysNum)%HeatingSpeedNum   = 1
      ELSE
        UnitarySystem(UnitarySysNum)%CoolingSpeedNum   = 1
      END IF
    END IF
    CALL SetOnOffMassFlowRate(UnitarySysNum, OnOffAirFlowRatio, PartLoadRatio)
    CALL CalcUnitarySystemToLoad(UnitarySysNum,FirstHVACIteration,CoolPLR,HeatPLR, &
                                 OnOffAirFlowRatio,SensOutputOff,LatOutputOff,HXUnitOn,CompOn=CompressorONFlag)
    FullSensibleOutput = SensOutputOff

    SELECT CASE (TempControlType(UnitarySystem(UnitarySysNum)%ControlZoneNum))
      CASE(SingleHeatingSetPoint)
        IF(HeatingLoad .AND. SensOutputOff .GT. ZoneLoad .AND. (MoistureLoad >= 0.0d0 .OR. MoistureLoad .GT. LatOutputOff))RETURN
        IF(.NOT. HeatingLoad .AND. (MoistureLoad >= 0.0d0 .OR. MoistureLoad .GT. LatOutputOff))RETURN
      CASE(SingleCoolingSetPoint)
        IF(CoolingLoad .AND. SensOutputOff .LT. ZoneLoad .AND. (MoistureLoad >= 0.0d0 .OR. MoistureLoad .GT. LatOutputOff))RETURN
        IF(.NOT. CoolingLoad .AND. (MoistureLoad >= 0.0d0 .OR. MoistureLoad .GT. LatOutputOff))RETURN
      CASE(SingleHeatCoolSetPoint, DualSetPointWithDeadBand)
        IF(HeatingLoad .AND. SensOutputOff .GT. ZoneLoad .AND. (MoistureLoad >= 0.0d0 .OR. MoistureLoad .GT. LatOutputOff))RETURN
        IF(CoolingLoad .AND. SensOutputOff .LT. ZoneLoad .AND. (MoistureLoad >= 0.0d0 .OR. MoistureLoad .GT. LatOutputOff))RETURN
        IF(.NOT. HeatingLoad .AND. .NOT. CoolingLoad .AND. (MoistureLoad >= 0.0d0 .OR. MoistureLoad .GT. LatOutputOff))RETURN
      CASE DEFAULT
      ! should never get here
    END SELECT
  END IF

  PartLoadRatio = 1.0d0  ! Get full load result
  IF(PRESENT(CompOn))THEN
    CompressorONFlag = CompOn
  ELSE
    CompressorONFlag = 1
  END IF

  IF(HeatingLoad)THEN
    CoolPLR       = 0.0d0
    HeatPLR       = 1.0d0
    UnitarySystem(UnitarySysNum)%HeatingSpeedRatio = 1.0d0
    UnitarySystem(UnitarySysNum)%HeatingCycRatio = 1.0d0
    UnitarySystem(UnitarySysNum)%WSHPRuntimeFrac   = HeatPLR
    UnitarySystem(UnitarySysNum)%HeatingSpeedNum = UnitarySystem(UnitarySysNum)%NumOfSpeedHeating
    IF (UnitarySystem(UnitarySysNum)%Staged .AND. UnitarySystem(UnitarySysNum)%StageNum > 0) THEN
      UnitarySystem(UnitarySysNum)%HeatingSpeedNum = MIN(UnitarySystem(UnitarySysNum)%StageNum, &
                                                         UnitarySystem(UnitarySysNum)%NumOfSpeedHeating)
      CALL SetOnOffMassFlowRate(UnitarySysNum, OnOffAirFlowRatio, PartLoadRatio)
      UnitarySystem(UnitarySysNum)%HeatingSpeedRatio = 0.0d0
      CALL CalcUnitarySystemToLoad(UnitarySysNum,FirstHVACIteration,CoolPLR,HeatPLR, &
                         OnOffAirFlowRatio,SensOutputOff,LatOutputOff,HXUnitOn,CompOn=CompressorONFlag)
      IF(SensOutputOff .GT. ZoneLoad)RETURN
      UnitarySystem(UnitarySysNum)%HeatingSpeedRatio = 1.0d0
    END IF
  ELSEIF(CoolingLoad .OR. MoistureLoad < LatOutputOff)THEN
    CoolPLR       = 1.0d0
    HeatPLR       = 0.0d0
    UnitarySystem(UnitarySysNum)%CoolingSpeedRatio = 1.0d0
    UnitarySystem(UnitarySysNum)%CoolingCycRatio   = 1.0d0
    UnitarySystem(UnitarySysNum)%WSHPRuntimeFrac   = CoolPLR
    UnitarySystem(UnitarySysNum)%CoolingSpeedNum = UnitarySystem(UnitarySysNum)%NumOfSpeedCooling
    IF (UnitarySystem(UnitarySysNum)%Staged .AND. UnitarySystem(UnitarySysNum)%StageNum < 0) THEN
      UnitarySystem(UnitarySysNum)%CoolingSpeedNum = MIN(ABS(UnitarySystem(UnitarySysNum)%StageNum), &
                                                         UnitarySystem(UnitarySysNum)%NumOfSpeedCooling)
      CALL SetOnOffMassFlowRate(UnitarySysNum, OnOffAirFlowRatio, PartLoadRatio)
      UnitarySystem(UnitarySysNum)%CoolingSpeedRatio = 0.0d0
      CALL CalcUnitarySystemToLoad(UnitarySysNum,FirstHVACIteration,CoolPLR,HeatPLR, &
                         OnOffAirFlowRatio,SensOutputOff,LatOutputOff,HXUnitOn,CompOn=CompressorONFlag)
      IF(SensOutputOff .LT. ZoneLoad)RETURN
      UnitarySystem(UnitarySysNum)%CoolingSpeedRatio = 1.0d0
    END IF
  ELSE
    ! will return here when no cooling or heating load and MoistureLoad > LatOutputOff (i.e., PLR=0)
    RETURN
  END IF

  CALL SetOnOffMassFlowRate(UnitarySysNum, OnOffAirFlowRatio, PartLoadRatio)

  CALL CalcUnitarySystemToLoad(UnitarySysNum,FirstHVACIteration,CoolPLR,HeatPLR, &
                         OnOffAirFlowRatio,SensOutputOn,LatOutputOn,HXUnitOn,CompOn=CompressorONFlag)
  FullSensibleOutput = SensOutputOn

  ! turn on HX if DehumidControl_Multimode
  IF(UnitarySystem(UnitarySysNum)%DehumidControlType_Num .EQ. DehumidControl_Multimode .AND. &
     MoistureLoad < 0.0d0 .AND. MoistureLoad < LatOutputOn .AND. CoolingLoad)THEN
    HXUnitOn = .TRUE.
    CALL CalcUnitarySystemToLoad(UnitarySysNum,FirstHVACIteration,CoolPLR,HeatPLR, &
                         OnOffAirFlowRatio,SensOutputOn,LatOutputOn,HXUnitOn,CompOn=CompressorONFlag)
    FullSensibleOutput = SensOutputOn
  END IF

  ! test to see if full capacity is less than load, if so set to PLR=1 and RETURN if no moisture load
  IF(HeatingLoad .AND. UnitarySystem(UnitarySysNum)%NumOfSpeedHeating .LE. 1 .OR. &
     CoolingLoad .AND. UnitarySystem(UnitarySysNum)%NumOfSpeedCooling .LE. 1)THEN
  SELECT CASE (TempControlType(UnitarySystem(UnitarySysNum)%ControlZoneNum))
    CASE(SingleHeatingSetPoint)
      IF(HeatingLoad .AND. SensOutputOn .LT. ZoneLoad)THEN
        UnitarySystem(UnitarySysNum)%HeatingPartLoadFrac = 1.0d0
        UnitarySystem(UnitarySysNum)%WSHPRuntimeFrac     = 1.0d0
        IF(MoistureLoad >= 0.0d0 .OR. (MoistureLoad < 0.0d0 .AND. MoistureLoad < LatOutputOn))RETURN
      END IF
      IF(.NOT. HeatingLoad .AND. &
         (MoistureLoad >= 0.0d0 .OR. (MoistureLoad < 0.0d0 .AND. MoistureLoad < LatOutputOn)))RETURN
    CASE(SingleCoolingSetPoint)
      IF(CoolingLoad .AND. SensOutputOn .GT. ZoneLoad)THEN
        UnitarySystem(UnitarySysNum)%CoolingPartLoadFrac = 1.0d0
        UnitarySystem(UnitarySysNum)%WSHPRuntimeFrac     = 1.0d0
        IF(MoistureLoad >= 0.0d0 .OR. (MoistureLoad < 0.0d0 .AND. MoistureLoad < LatOutputOn))RETURN
      END IF
      IF(.NOT. CoolingLoad .AND. &
        (MoistureLoad >= 0.0d0 .OR. (MoistureLoad < 0.0d0 .AND. MoistureLoad < LatOutputOn)))RETURN
    CASE(SingleHeatCoolSetPoint, DualSetPointWithDeadBand)
      IF(HeatingLoad .AND. SensOutputOn .LT. ZoneLoad)THEN
        UnitarySystem(UnitarySysNum)%HeatingPartLoadFrac = 1.0d0
        UnitarySystem(UnitarySysNum)%WSHPRuntimeFrac     = 1.0d0
        IF(MoistureLoad >= 0.0d0 .OR. (MoistureLoad < 0.0d0 .AND. MoistureLoad > LatOutputOn))THEN
          RETURN
        END IF
      END IF
      IF(CoolingLoad .AND. SensOutputOn .GT. ZoneLoad)THEN
        UnitarySystem(UnitarySysNum)%CoolingPartLoadFrac = 1.0d0
        UnitarySystem(UnitarySysNum)%WSHPRuntimeFrac     = 1.0d0
        RETURN
      END IF
      IF(.NOT. HeatingLoad .AND. .NOT. CoolingLoad .AND. &
        (MoistureLoad >= 0.0d0 .OR. (MoistureLoad < 0.0d0 .AND. MoistureLoad < LatOutputOn)))THEN
        RETURN
      END IF
  CASE DEFAULT
    ! no other choices for thermostat control
  END SELECT
  END IF
! will find speed for multispeed coils here and then RegulaFalsi on PLR at a fixed speed

! Do the non-variable or non-multispeed coils have a NumOfSpeed = 0 ? We don't need to do this for single speed coils.
! Check to see which speed to meet the load
  UnitarySystem(UnitarySysNum)%HeatingSpeedNum = 0
  UnitarySystem(UnitarySysNum)%CoolingSpeedNum = 0
  IF (.NOT. UnitarySystem(UnitarySysNum)%Staged) THEN
    IF(HeatingLoad)THEN
      DO SpeedNum=1,UnitarySystem(UnitarySysNum)%NumOfSpeedHeating
        CoolPLR       = 0.0d0
        HeatPLR       = 1.0d0
        IF(SpeedNum == 1)THEN
          UnitarySystem(UnitarySysNum)%HeatingSpeedRatio = 0.0d0
        ELSE
          UnitarySystem(UnitarySysNum)%HeatingSpeedRatio = 1.0d0
        END IF
        UnitarySystem(UnitarySysNum)%HeatingCycRatio = 1.0d0
        UnitarySystem(UnitarySysNum)%HeatingSpeedNum = SpeedNum
        CALL CalcUnitarySystemToLoad(UnitarySysNum,FirstHVACIteration,CoolPLR,HeatPLR, &
                         OnOffAirFlowRatio,SensOutputOn,LatOutputOn,HXUnitOn,CompOn=CompressorONFlag)
        IF(UnitarySystem(UnitarySysNum)%HeatingCoilType_Num /= Coil_HeatingWaterToAirHPVSEquationFit) THEN
          UnitarySystem(UnitarySysNum)%HeatingSpeedRatio = 0.0d0
          UnitarySystem(UnitarySysNum)%HeatingSpeedNum = SpeedNum-1
          IF(UnitarySystem(UnitarySysNum)%HeatingSpeedNum .EQ. 0)THEN
            UnitarySystem(UnitarySysNum)%HeatingCycRatio = 0.0d0
            HeatPLR = 0.0d0
          ELSE
            UnitarySystem(UnitarySysNum)%HeatingCycRatio = 1.0d0
            HeatPLR = 1.0d0
          END IF
          CALL CalcUnitarySystemToLoad(UnitarySysNum,FirstHVACIteration,CoolPLR,HeatPLR, &
                       OnOffAirFlowRatio,SensOutputOff,LatOutputOff,HXUnitOn,CompOn=CompressorONFlag)
          UnitarySystem(UnitarySysNum)%HeatingSpeedNum = SpeedNum
        END IF
        IF (ZoneLoad <= SensOutputOn) THEN
          EXIT
        END IF
      END DO
    ELSE ! Cooling or moisture load
      DO SpeedNum=1,UnitarySystem(UnitarySysNum)%NumOfSpeedCooling
        CoolPLR       = 1.0d0
        HeatPLR       = 0.0d0
        IF(SpeedNum == 1)THEN
          UnitarySystem(UnitarySysNum)%CoolingSpeedRatio = 0.0d0
        ELSE
          UnitarySystem(UnitarySysNum)%CoolingSpeedRatio = 1.0d0
        END IF
        UnitarySystem(UnitarySysNum)%CoolingCycRatio = 1.0d0
        UnitarySystem(UnitarySysNum)%CoolingSpeedNum = SpeedNum
        CALL CalcUnitarySystemToLoad(UnitarySysNum,FirstHVACIteration,CoolPLR,HeatPLR,OnOffAirFlowRatio, &
                                     SensOutputOn,LatOutputOn,HXUnitOn,CompOn=CompressorONFlag)

        IF(UnitarySystem(UnitarySysNum)%CoolingCoilType_Num /= Coil_CoolingWaterToAirHPVSEquationFit) THEN
          UnitarySystem(UnitarySysNum)%CoolingSpeedRatio = 0.0d0
          UnitarySystem(UnitarySysNum)%CoolingSpeedNum = SpeedNum-1
          IF(UnitarySystem(UnitarySysNum)%CoolingSpeedNum .EQ. 0)THEN
            UnitarySystem(UnitarySysNum)%CoolingCycRatio = 0.0d0
            CoolPLR = 0.0d0
          ELSE
            UnitarySystem(UnitarySysNum)%CoolingCycRatio = 1.0d0
            CoolPLR = 1.0d0
          END IF
          CALL CalcUnitarySystemToLoad(UnitarySysNum,FirstHVACIteration,CoolPLR,HeatPLR,OnOffAirFlowRatio, &
                                       SensOutputOff,LatOutputOff,HXUnitOn,CompOn=CompressorONFlag)
          UnitarySystem(UnitarySysNum)%CoolingSpeedNum = SpeedNum
          END IF
        IF (ZoneLoad >= SensOutputOn) THEN
          EXIT
        END IF
      END DO
    END IF
  ELSE ! IF (.NOT. UnitarySystem(UnitarySysNum)%Staged) THEN
  ! Staged control
   IF(HeatingLoad)THEN
      CoolPLR       = 0.0d0
      HeatPLR       = 1.0d0
      SpeedNum = UnitarySystem(UnitarySysNum)%StageNum
      IF(SpeedNum == 1)THEN
        UnitarySystem(UnitarySysNum)%HeatingSpeedRatio = 0.0d0
      ELSE
        UnitarySystem(UnitarySysNum)%HeatingSpeedRatio = 1.0d0
        SpeedNum = MIN(UnitarySystem(UnitarySysNum)%StageNum,UnitarySystem(UnitarySysNum)%NumOfSpeedHeating)
      END IF
      UnitarySystem(UnitarySysNum)%HeatingCycRatio = 1.0d0
      UnitarySystem(UnitarySysNum)%HeatingSpeedNum = SpeedNum
      CALL CalcUnitarySystemToLoad(UnitarySysNum,FirstHVACIteration,CoolPLR,HeatPLR, &
                         OnOffAirFlowRatio,SensOutputOn,LatOutputOn,HXUnitOn,CompOn=CompressorONFlag)
      IF(UnitarySystem(UnitarySysNum)%HeatingCoilType_Num /= Coil_HeatingWaterToAirHPVSEquationFit) THEN
        UnitarySystem(UnitarySysNum)%HeatingSpeedRatio = 0.0d0
        UnitarySystem(UnitarySysNum)%HeatingSpeedNum = SpeedNum-1
        IF(UnitarySystem(UnitarySysNum)%HeatingSpeedNum .EQ. 0)THEN
          UnitarySystem(UnitarySysNum)%HeatingCycRatio = 0.0d0
          HeatPLR = 0.0d0
        ELSE
          UnitarySystem(UnitarySysNum)%HeatingCycRatio = 1.0d0
          HeatPLR = 1.0d0
        END IF
        CALL CalcUnitarySystemToLoad(UnitarySysNum,FirstHVACIteration,CoolPLR,HeatPLR, &
                       OnOffAirFlowRatio,SensOutputOff,LatOutputOff,HXUnitOn,CompOn=CompressorONFlag)
        UnitarySystem(UnitarySysNum)%HeatingSpeedNum = SpeedNum
      END IF
      IF (ZoneLoad <= SensOutputOn) THEN
!        EXIT ????????????
      END IF
   ELSE ! Cooling or moisture load
      CoolPLR       = 1.0d0
      HeatPLR       = 0.0d0
      SpeedNum = ABS(UnitarySystem(UnitarySysNum)%StageNum)
      IF(SpeedNum == 1)THEN
        UnitarySystem(UnitarySysNum)%CoolingSpeedRatio = 0.0d0
      ELSE
        UnitarySystem(UnitarySysNum)%CoolingSpeedRatio = 1.0d0
        SpeedNum = MIN(ABS(UnitarySystem(UnitarySysNum)%StageNum),UnitarySystem(UnitarySysNum)%NumOfSpeedCooling)
      END IF
      UnitarySystem(UnitarySysNum)%CoolingCycRatio = 1.0d0
      UnitarySystem(UnitarySysNum)%CoolingSpeedNum = SpeedNum
      CALL CalcUnitarySystemToLoad(UnitarySysNum,FirstHVACIteration,CoolPLR,HeatPLR,OnOffAirFlowRatio, &
                                   SensOutputOn,LatOutputOn,HXUnitOn,CompOn=CompressorONFlag)

      IF(UnitarySystem(UnitarySysNum)%CoolingCoilType_Num /= Coil_CoolingWaterToAirHPVSEquationFit) THEN
        UnitarySystem(UnitarySysNum)%CoolingSpeedRatio = 0.0d0
        UnitarySystem(UnitarySysNum)%CoolingSpeedNum = SpeedNum-1
        IF(UnitarySystem(UnitarySysNum)%CoolingSpeedNum .EQ. 0)THEN
          UnitarySystem(UnitarySysNum)%CoolingCycRatio = 0.0d0
          CoolPLR = 0.0d0
        ELSE
          UnitarySystem(UnitarySysNum)%CoolingCycRatio = 1.0d0
          CoolPLR = 1.0d0
        END IF
        CALL CalcUnitarySystemToLoad(UnitarySysNum,FirstHVACIteration,CoolPLR,HeatPLR,OnOffAirFlowRatio, &
                                     SensOutputOff,LatOutputOff,HXUnitOn,CompOn=CompressorONFlag)
        UnitarySystem(UnitarySysNum)%CoolingSpeedNum = SpeedNum
      END IF
      IF (ZoneLoad >= SensOutputOn) THEN
!        EXIT ???????????
      END IF
   END IF
  END IF ! IF (.NOT. UnitarySystem(UnitarySysNum)%Staged) THEN

  FullSensibleOutput = SensOutputOn

  IF(.NOT. HeatingLoad .AND. .NOT. CoolingLoad .AND. (MoistureLoad >= 0.0d0 .OR. MoistureLoad < LatOutputOn))THEN
    ! if no load, or only a moisture load which can't be met at PLR=1, RETURN
    RETURN
  END IF
  ! must test to see if load is bounded by capacity before calling RegulaFalsi
  IF((HeatingLoad .AND. ZoneLoad < SensOutputOn) .OR. (CoolingLoad .AND. ZoneLoad > SensOutputOn))THEN
    IF((HeatingLoad .AND. ZoneLoad > SensOutputOff) .OR. (CoolingLoad .AND. ZoneLoad < SensOutputOff))THEN
      Par(1)  = REAL(UnitarySysNum,r64)
      Par(2)  = 0.0d0  ! FLAG, IF 1.0 then FirstHVACIteration equals TRUE, if 0.0 then FirstHVACIteration equals false
      IF(FirstHVACIteration)Par(2)=1.0d0
      Par(3)  = REAL(UnitarySystem(UnitarySysNum)%FanOpMode,r64)
      Par(4)  = CompressorONFlag ! CompOp
      Par(5)  = ZoneLoad
      Par(6)  = 0.0d0 ! FLAG, 0.0 if heating load, 1.0 IF cooling or moisture load
      IF(CoolingLoad)Par(6)  = 1.0d0
      Par(7)  = 1.0d0 ! FLAG, 0.0 if latent load, 1.0 if sensible load to be met
      Par(8)  = OnOffAirFlowRatio ! Ratio of compressor ON mass flow rate to AVERAGE mass flow rate over time step
      Par(9)  = 0.0d0 ! HXUnitOn is always false for HX
      Par(10) = UnitarySystem(UnitarySysNum)%HeatingPartLoadFrac
!     Tolerance is in fraction of load, MaxIter = 30, SolFalg = # of iterations or error as appropriate
      CALL SolveRegulaFalsi(0.001d0, MaxIter, SolFlag, PartLoadRatio, CalcUnitarySystemLoadResidual, 0.0d0, 1.0d0, Par)

      IF (SolFlag == -1) THEN
        IF(HeatingLoad)THEN
          ! IF iteration limit is exceeded, find tighter boundary of solution and repeat RegulaFalsi
          ! This does cause a problem when coil cannot turn on when OAT < min allowed or scheduled off
          ! If max iteration limit is exceeded, how do we know if the heating coil is operating?
          TempMaxPLR = -0.1d0
          TempSensOutput = SensOutputOff
          DO WHILE((TempSensOutput - ZoneLoad) .LT. 0.0d0 .AND. TempMaxPLR .LT. 1.0d0)
            ! find upper limit of HeatingPLR
            TempMaxPLR = TempMaxPLR + 0.1d0

            ! SUBROUTINE SetSpeedVariables(UnitarySysNum, SensibleLoad, PartLoadRatio)
            CALL SetSpeedVariables(UnitarySysNum, .TRUE., TempMaxPLR)
            CALL CalcUnitarySystemToLoad(UnitarySysNum,FirstHVACIteration,CoolPLR,TempMaxPLR, &
                                         OnOffAirFlowRatio,TempSensOutput,TempLatOutput,HXUnitOn,CompOn=CompressorONFlag)
          END DO
          TempMinPLR = TempMaxPLR
          DO WHILE((TempSensOutput - ZoneLoad) .GT. 0.0d0 .AND. TempMinPLR .GT. 0.0d0)
            ! pull upper limit of HeatingPLR down to last valid limit (i.e. heat output still exceeds SystemSensibleLoad)
            TempMaxPLR = TempMinPLR
            ! find minimum limit of HeatingPLR
            TempMinPLR = TempMinPLR - 0.01d0
            CALL SetSpeedVariables(UnitarySysNum, .TRUE., TempMinPLR)
            CALL CalcUnitarySystemToLoad(UnitarySysNum,FirstHVACIteration,CoolPLR,TempMinPLR, &
                                         OnOffAirFlowRatio,TempSensOutput,TempLatOutput,HXUnitOn,CompOn=CompressorONFlag)
          END DO
          ! Now solve again with tighter PLR limits
          CALL SolveRegulaFalsi(0.001d0, MaxIter, SolFlag, HeatPLR, CalcUnitarySystemLoadResidual, TempMinPLR, TempMaxPLR, Par)
          CALL CalcUnitarySystemToLoad(UnitarySysNum,FirstHVACIteration,CoolPLR,HeatPLR, &
                                       OnOffAirFlowRatio,TempSensOutput,TempLatOutput,HXUnitOn,CompOn=CompressorONFlag)
        ELSEIF(CoolingLoad)THEN
          ! RegulaFalsi may not find cooling PLR when the latent degradation model is used.
          ! IF iteration limit is exceeded, find tighter boundary of solution and repeat RegulaFalsi
          TempMaxPLR = -0.1d0
          TempSysOutput = SensOutputOff
          TempLoad = ZoneLoad
          DO WHILE((TempSysOutput - TempLoad) .GT. 0.0d0 .AND. TempMaxPLR .LT. 1.0d0)
            ! find upper limit of HeatingPLR
            TempMaxPLR = TempMaxPLR + 0.1d0
            CALL SetSpeedVariables(UnitarySysNum, .TRUE., TempMaxPLR)
            CALL CalcUnitarySystemToLoad(UnitarySysNum,FirstHVACIteration,TempMaxPLR,HeatPLR, &
                                       OnOffAirFlowRatio,TempSensOutput,TempLatOutput,HXUnitOn,CompOn=CompressorONFlag)
            TempSysOutput = TempSensOutput
          END DO
          TempMinPLR = TempMaxPLR
          DO WHILE((TempSysOutput - TempLoad) .LT. 0.0d0 .AND. TempMinPLR .GT. 0.0d0)
            ! pull upper limit of HeatingPLR DOwn to last valid limit (i.e. heat output still exceeds SystemSensibleLoad)
            TempMaxPLR = TempMinPLR
            ! find minimum limit of HeatingPLR
            TempMinPLR = TempMinPLR - 0.01d0
            CALL SetSpeedVariables(UnitarySysNum, .TRUE., TempMinPLR)
            CALL CalcUnitarySystemToLoad(UnitarySysNum,FirstHVACIteration,TempMinPLR,HeatPLR, &
                                         OnOffAirFlowRatio,TempSensOutput,TempLatOutput,HXUnitOn,CompOn=CompressorONFlag)
            TempSysOutput = TempSensOutput
          END DO
          ! Now solve again with tighter PLR limits
          CALL SolveRegulaFalsi(0.001d0, MaxIter, SolFlag, CoolPLR, CalcUnitarySystemLoadResidual, TempMinPLR, TempMaxPLR, Par)
          CALL CalcUnitarySystemToLoad(UnitarySysNum,FirstHVACIteration,CoolPLR,HeatPLR, &
                                       OnOffAirFlowRatio,TempSensOutput,TempLatOutput,HXUnitOn,CompOn=CompressorONFlag)
        END IF ! IF(HeatingLoad)THEN
        IF (SolFlag == -1) THEN
          IF(ABS(ZoneLoad - TempSensOutput) .GT. SmallLoad)THEN
            IF(UnitarySystem(UnitarySysNum)%MaxIterIndex == 0)THEN
              CALL ShowWarningMessage('Coil control failed to converge for ' &
               //TRIM(UnitarySystem(UnitarySysNum)%UnitarySystemType)//':'//TRIM(UnitarySystem(UnitarySysNum)%Name))
              CALL ShowContinueError('  Iteration limit exceeded in calculating system sensible part-load ratio.')
              CALL ShowContinueErrorTimeStamp('Sensible load to be met = ' &
               //TRIM(TrimSigDigits(ZoneLoad,2))//' (watts), sensible output = ' &
               //TRIM(TrimSigDigits(TempSensOutput,2))//' (watts), and the simulation continues.')
            END IF
            CALL ShowRecurringWarningErrorAtEnd(TRIM(UnitarySystem(UnitarySysNum)%UnitarySystemType)//' "'&
             //TRIM(UnitarySystem(UnitarySysNum)%Name)//'" - Iteration limit exceeded in calculating'// &
             ' sensible part-load ratio error continues. Sensible load statistics:' &
             ,UnitarySystem(UnitarySysNum)%MaxIterIndex,ZoneLoad,ZoneLoad)
          END IF
        ELSEIF (SolFlag == -2) THEN
          IF(UnitarySystem(UnitarySysNum)%RegulaFalsIFailedIndex == 0)THEN
            CALL ShowWarningMessage('Coil control failed for ' &
             //TRIM(UnitarySystem(UnitarySysNum)%UnitarySystemType)//':'//TRIM(UnitarySystem(UnitarySysNum)%Name))
            CALL ShowContinueError('  sensible part-load ratio determined to be outside the range of 0-1.')
            CALL ShowContinueErrorTimeStamp('Sensible load to be met = ' &
            //TRIM(TrimSigDigits(ZoneLoad,2))//' (watts), and the simulation continues.')
          END IF
          CALL ShowRecurringWarningErrorAtEnd(TRIM(UnitarySystem(UnitarySysNum)%UnitarySystemType)//' "'&
           //TRIM(UnitarySystem(UnitarySysNum)%Name)//'" - '// &
           ' sensible part-load ratio out of range error continues. Sensible load statistics:' &
           ,UnitarySystem(UnitarySysNum)%RegulaFalsIFailedIndex,ZoneLoad,ZoneLoad)
        END IF
      ELSEIF (SolFlag == -2) THEN
        IF(UnitarySystem(UnitarySysNum)%RegulaFalsIFailedIndex == 0)THEN
          CALL ShowWarningMessage('Coil control failed for ' &
            //TRIM(UnitarySystem(UnitarySysNum)%UnitarySystemType)//':'//TRIM(UnitarySystem(UnitarySysNum)%Name))
          CALL ShowContinueError('  sensible part-load ratio determined to be outside the range of 0-1.')
          CALL ShowContinueErrorTimeStamp('Sensible load to be met = ' &
           //TRIM(TrimSigDigits(ZoneLoad,2))//' (watts), and the simulation continues.')
        END IF
        CALL ShowRecurringWarningErrorAtEnd(TRIM(UnitarySystem(UnitarySysNum)%UnitarySystemType)//' "'&
        //TRIM(UnitarySystem(UnitarySysNum)%Name)//'" - '// &
        ' sensible part-load ratio out of range error continues. Sensible load statistics:' &
        ,UnitarySystem(UnitarySysNum)%RegulaFalsIFailedIndex,ZoneLoad,ZoneLoad)
      END IF ! IF (SolFlag == -1) THEN
    ELSE ! load is not bounded by capacity. Leave PLR=1 or turn off unit?
      UnitarySystem(UnitarySysNum)%CoolingPartLoadFrac = 0.0d0
      UnitarySystem(UnitarySysNum)%HeatingPartLoadFrac = 0.0d0
      CoolPLR = 0.0d0
      HeatPLR = 0.0d0
      PartLoadRatio = 0.0d0
    END IF ! IF((HeatingLoad .AND. ZoneLoad > SensOutputOff) .OR. (CoolingLoad .AND. ZoneLoad < SensOutputOff))THEN
  END IF ! IF((HeatingLoad .AND. ZoneLoad < SensOutputOn) .OR. (CoolingLoad .AND. ZoneLoad > SensOutputOn))THEN

  IF(HeatingLoad .AND. (UnitarySystem(UnitarySysNum)%MultiSpeedHeatingCoil .OR. &
                        UnitarySystem(UnitarySysNum)%VarSpeedHeatingCoil)) THEN
    IF(UnitarySystem(UnitarySysNum)%HeatingSpeedNum == 1)THEN
      UnitarySystem(UnitarySysNum)%HeatingCycRatio   = PartLoadRatio
      UnitarySystem(UnitarySysNum)%HeatingSpeedRatio = 0.0d0
    ELSE
      UnitarySystem(UnitarySysNum)%HeatingCycRatio   = 1.0d0
      UnitarySystem(UnitarySysNum)%HeatingSpeedRatio = PartLoadRatio
    END IF
    HeatPLR = PartLoadRatio
    CoolPLR = 0.0d0
    UnitarySystem(UnitarySysNum)%CoolingCycRatio   = 0.0d0
    UnitarySystem(UnitarySysNum)%CoolingSpeedRatio = 0.0d0
  ELSEIF(CoolingLoad .AND. (UnitarySystem(UnitarySysNum)%MultiSpeedCoolingCoil .OR. &
                            UnitarySystem(UnitarySysNum)%VarSpeedCoolingCoil))THEN
    IF(UnitarySystem(UnitarySysNum)%CoolingSpeedNum == 1)THEN
      UnitarySystem(UnitarySysNum)%CoolingCycRatio   = PartLoadRatio
      UnitarySystem(UnitarySysNum)%CoolingSpeedRatio = 0.0d0
    ELSE
      UnitarySystem(UnitarySysNum)%CoolingCycRatio   = 1.0d0
      UnitarySystem(UnitarySysNum)%CoolingSpeedRatio = PartLoadRatio
    END IF
    UnitarySystem(UnitarySysNum)%HeatingCycRatio   = 0.0d0
    UnitarySystem(UnitarySysNum)%HeatingSpeedRatio = 0.0d0
    HeatPLR = 0.0d0
    CoolPLR = PartLoadRatio
  ELSE
    HeatPLR = UnitarySystem(UnitarySysNum)%HeatingPartLoadFrac
    CoolPLR = UnitarySystem(UnitarySysNum)%CoolingPartLoadFrac
  END IF

  CALL CalcUnitarySystemToLoad(UnitarySysNum,FirstHVACIteration,CoolPLR,HeatPLR, &
                               OnOffAirFlowRatio,TempSensOutput,TempLatOutput,HXUnitOn,CompOn=CompressorONFlag)

! FullSensibleOutput is used to set supplemental heater PLR in calling routine
! OnOffAirFlowRatio is used to average air flow between ON and OFF state
  FullSensibleOutput = TempSensOutput

  ! RETURN of the moisture load is met
  IF(MoistureLoad >= 0.0d0 .OR. (MoistureLoad < 0.0d0 .AND. MoistureLoad >= TempLatOutput))RETURN
  ! Multimode does not meet the latent load, only the sensible load with or without HX active
  IF(.NOT. CoolingLoad .AND. UnitarySystem(UnitarySysNum)%DehumidControlType_Num .EQ. DehumidControl_Multimode)RETURN
!  IF(HeatingLoad .AND. UnitarySystem(UnitarySysNum)%DehumidControlType_Num .EQ. DehumidControl_CoolReheat)RETURN

  IF (( UnitarySystem(UnitarySysNum)%DehumidControlType_Num .EQ. DehumidControl_CoolReheat .OR. &
          UnitarySystem(UnitarySysNum)%DehumidControlType_Num .EQ. DehumidControl_Multimode))THEN

    ! find maximum latent output IF not already calculated
    IF(HeatingLoad)THEN
      CoolPLR = 1.0d0
      UnitarySystem(UnitarySysNum)%CoolingPartLoadFrac = 1.0d0
      UnitarySystem(UnitarySysNum)%CoolingSpeedNum = UnitarySystem(UnitarySysNum)%NumOfSpeedCooling
      UnitarySystem(UnitarySysNum)%CoolingSpeedRatio = 1.0d0
      UnitarySystem(UnitarySysNum)%CoolingCycRatio   = 1.0d0
      UnitarySystem(UnitarySysNum)%WSHPRuntimeFrac   = CoolPLR
      IF(UnitarySystem(UnitarySysNum)%CoolingSpeedNum .GT. 0)THEN
        UnitarySystem(UnitarySysNum)%HeatingPartLoadFrac = 0.0d0
        UnitarySystem(UnitarySysNum)%HeatingSpeedNum = 0
        HeatPLR = 0.0d0
        CoolingLoad = .TRUE.
        HeatingLoad = .FALSE.
        UnitarySystem(UnitarySysNum)%HeatingCoilSensDemand = 0.0d0
        UnitarySystem(UnitarySysNum)%CoolingCoilLatentDemand = MoistureLoad
        CALL CalcUnitarySystemToLoad(UnitarySysNum,FirstHVACIteration,0.0d0,0.0d0, &
                                   OnOffAirFlowRatio,TempSensOutput,TempLatOutput,HXUnitOn,CompOn=CompressorONFlag)
        CALL CalcUnitarySystemToLoad(UnitarySysNum,FirstHVACIteration,CoolPLR,HeatPLR, &
                                   OnOffAirFlowRatio,TempSensOutput,LatOutputOn,HXUnitOn,CompOn=CompressorONFlag)
      ELSE
        UnitarySystem(UnitarySysNum)%HeatingCoilSensDemand = 0.0d0
        UnitarySystem(UnitarySysNum)%CoolingCoilLatentDemand = 0.0d0
        CALL CalcUnitarySystemToLoad(UnitarySysNum,FirstHVACIteration,0.0d0,0.0d0, &
                                   OnOffAirFlowRatio,TempSensOutput,TempLatOutput,HXUnitOn,CompOn=CompressorONFlag)
        UnitarySystem(UnitarySysNum)%CoolingCoilLatentDemand = MoistureLoad
        CALL CalcUnitarySystemToLoad(UnitarySysNum,FirstHVACIteration,CoolPLR,HeatPLR, &
                                   OnOffAirFlowRatio,TempSensOutput,LatOutputOn,HXUnitOn,CompOn=CompressorONFlag)
      END IF
    END IF

    IF(UnitarySystem(UnitarySysNum)%DehumidControlType_Num .EQ. DehumidControl_Multimode .AND. &
       MoistureLoad < LatOutputOn)THEN
      HXUnitOn = .TRUE.
      CALL CalcUnitarySystemToLoad(UnitarySysNum,FirstHVACIteration,CoolPLR,HeatPLR, &
                                   OnOffAirFlowRatio,TempSensOutput,LatOutputOn,HXUnitOn,CompOn=CompressorONFlag)
      FullSensibleOutput = TempSensOutput
    END IF

!    IF ((HeatingLoad .AND. MoistureLoad < TempLatOutput) .OR. &
!        (CoolingLoad .AND. MoistureLoad < TempLatOutput .AND. MoistureLoad > LatOutputOn) .OR. &
!        ((.NOT. HeatingLoad) .AND. (.NOT. CoolingLoad) .AND. MoistureLoad > LatOutputOn)) THEN
    IF ((MoistureLoad < TempLatOutput) .AND. (MoistureLoad > LatOutputOn)) THEN ! bounds check for RegulaFalsi

      ! save heating PLR
      HeatPLR = UnitarySystem(UnitarySysNum)%HeatingPartLoadFrac
      Par(1)  = REAL(UnitarySysNum,r64)
      Par(2)  = 0.0d0  ! FLAG, if 1.0 then FirstHVACIteration equals TRUE, if 0.0 then FirstHVACIteration equals false
      IF(FirstHVACIteration)Par(2)=1.0d0
      Par(3)  = REAL(UnitarySystem(UnitarySysNum)%FanOpMode,r64)
      Par(4)  = CompressorONFlag ! CompOp
      IF(UnitarySystem(UnitarySysNum)%DehumidControlType_Num .EQ. DehumidControl_Multimode)THEN
        Par(5)  = ZoneLoad
        Par(7)  = 1.0d0 ! FLAG, 0.0 if latent load, 1.0 if sensible load to be met
      ELSE
        Par(5)  = MoistureLoad
        Par(7)  = 0.0d0 ! FLAG, 0.0 if latent load, 1.0 if sensible load to be met
      END IF
      Par(6)  = 1.0d0 ! FLAG, 0.0 if heating load, 1.0 if cooling or moisture load
!      IF(HeatingLoad)Par(6)  = 0.0d0
      Par(8)  = OnOffAirFlowRatio ! Ratio of compressor ON mass flow rate to AVERAGE mass flow rate over time step
      IF (HXUnitOn) THEN
        Par(9) = 1.0d0
      ELSE
        Par(9) = 0.0d0
      END IF
      Par(10) = UnitarySystem(UnitarySysNum)%HeatingPartLoadFrac
    ! Tolerance is fraction of load, MaxIter = 30, SolFalg = # of iterations or error as appropriate
      CALL SolveRegulaFalsi(0.001d0, MaxIter, SolFlagLat, PartLoadRatio, CalcUnitarySystemLoadResidual, 0.0d0, 1.0d0, Par)
!      IF (HeatingLoad) THEN
!        UnitarySystem(UnitarySysNum)%HeatingPartLoadFrac = PartLoadRatio
!      ELSE
        UnitarySystem(UnitarySysNum)%CoolingPartLoadFrac = PartLoadRatio
!      END IF
      UnitarySystem(UnitarySysNum)%HeatingPartLoadFrac = HeatPLR
    ELSEIF(MoistureLoad < LatOutputOn .AND. CoolingLoad)THEN
!     Logic below needs further look...what to do if the bounds check for RegulaFalsi fail?
!     I'm not even sure if this should be done.
!     It's wrong anyway, since there won't be a cooling load if multimode (see RETURN about 80 lines up).
      IF(UnitarySystem(UnitarySysNum)%DehumidControlType_Num .NE. DehumidControl_Multimode) THEN
        UnitarySystem(UnitarySysNum)%CoolingPartLoadFrac = 1.0d0
      END IF
    END IF
  END IF

  CoolPLR = UnitarySystem(UnitarySysNum)%CoolingPartLoadFrac
  HeatPLR = UnitarySystem(UnitarySysNum)%HeatingPartLoadFrac

  CALL CalcUnitarySystemToLoad(UnitarySysNum,FirstHVACIteration,CoolPLR,HeatPLR, &
                               OnOffAirFlowRatio,TempSensOutput,TempLatOutput,HXUnitOn,CompOn=CompressorONFlag)

  IF (SolFlagLat == -1) THEN
    ! RegulaFalsi may not find cooling PLR when the latent degradation model is used.
    ! IF iteration limit is exceeded, find tighter boundary of solution and repeat RegulaFalsi
    TempMaxPLR = -0.1d0
    TempLatOutput = LatOutputOff
    DO WHILE((TempLatOutput - MoistureLoad) .GT. 0.0d0 .AND. TempMaxPLR .LT. 1.0d0)
      ! find upper limit of HeatingPLR
      TempMaxPLR = TempMaxPLR + 0.1d0
      CALL CalcUnitarySystemToLoad(UnitarySysNum,FirstHVACIteration,TempMaxPLR,HeatPLR, &
                                   OnOffAirFlowRatio,TempSensOutput,TempLatOutput,HXUnitOn,CompOn=CompressorONFlag)
    END DO
    TempMinPLR = TempMaxPLR
    DO WHILE((TempLatOutput - MoistureLoad) .LT. 0.0 .AND. TempMinPLR .GT. 0.0d0)
      ! pull upper limit of HeatingPLR DOwn to last valid limit (i.e. heat output still exceeds SystemSensibleLoad)
      TempMaxPLR = TempMinPLR
      ! find minimum limit of HeatingPLR
      TempMinPLR = TempMinPLR - 0.01d0
      CALL CalcUnitarySystemToLoad(UnitarySysNum,FirstHVACIteration,TempMinPLR,HeatPLR, &
                                   OnOffAirFlowRatio,TempSensOutput,TempLatOutput,HXUnitOn,CompOn=CompressorONFlag)
    END DO
    ! Now solve again with tighter PLR limits
    CALL SolveRegulaFalsi(0.001d0, MaxIter, SolFlagLat, CoolPLR, CalcUnitarySystemLoadResidual, TempMinPLR, TempMaxPLR, Par)
    CALL CalcUnitarySystemToLoad(UnitarySysNum,FirstHVACIteration,CoolPLR,HeatPLR, &
                                 OnOffAirFlowRatio,TempSensOutput,TempLatOutput,HXUnitOn,CompOn=CompressorONFlag)
    IF (SolFlagLat == -1) THEN
      IF(ABS(MoistureLoad - TempLatOutput) .GT. SmallLoad)THEN
        IF(UnitarySystem(UnitarySysNum)%LatMaxIterIndex == 0)THEN
          CALL ShowWarningMessage('Coil control failed to converge for ' &
               //TRIM(UnitarySystem(UnitarySysNum)%UnitarySystemType)//':'//TRIM(UnitarySystem(UnitarySysNum)%Name))
          CALL ShowContinueError('  Iteration limit exceeded in calculating system Latent part-load ratio.')
          CALL ShowContinueErrorTimeStamp('Latent load to be met = ' &
             //TRIM(TrimSigDigits(MoistureLoad,2))//' (watts), Latent output = ' &
             //TRIM(TrimSigDigits(TempLatOutput,2))//' (watts), and the simulation continues.')
        END IF
        CALL ShowRecurringWarningErrorAtEnd(TRIM(UnitarySystem(UnitarySysNum)%UnitarySystemType)//' "'&
          //TRIM(UnitarySystem(UnitarySysNum)%Name)//'" - Iteration limit exceeded in calculating'// &
          ' Latent part-load ratio error continues. Latent load statistics:' &
          ,UnitarySystem(UnitarySysNum)%LatMaxIterIndex,MoistureLoad,MoistureLoad)
      END IF
    ELSEIF (SolFlagLat == -2) THEN
      IF(UnitarySystem(UnitarySysNum)%LatRegulaFalsIFailedIndex == 0)THEN
        CALL ShowWarningMessage('Coil control failed for ' &
              //TRIM(UnitarySystem(UnitarySysNum)%UnitarySystemType)//':'//TRIM(UnitarySystem(UnitarySysNum)%Name))
        CALL ShowContinueError('  Latent part-load ratio determined to be outside the range of 0-1.')
        CALL ShowContinueErrorTimeStamp('Latent load to be met = ' &
           //TRIM(TrimSigDigits(MoistureLoad,2))//' (watts), and the simulation continues.')
      END IF
      CALL ShowRecurringWarningErrorAtEnd(TRIM(UnitarySystem(UnitarySysNum)%UnitarySystemType)//' "'&
        //TRIM(UnitarySystem(UnitarySysNum)%Name)//'" - '// &
        ' Latent part-load ratio out of range error continues. Latent load statistics:' &
        ,UnitarySystem(UnitarySysNum)%LatRegulaFalsIFailedIndex,MoistureLoad,MoistureLoad)
    END IF
  ELSEIF (SolFlagLat == -2) THEN
    IF(UnitarySystem(UnitarySysNum)%LatRegulaFalsIFailedIndex == 0)THEN
      CALL ShowWarningMessage('Coil control failed for ' &
              //TRIM(UnitarySystem(UnitarySysNum)%UnitarySystemType)//':'//TRIM(UnitarySystem(UnitarySysNum)%Name))
      CALL ShowContinueError('  Latent part-load ratio determined to be outside the range of 0-1.')
      CALL ShowContinueErrorTimeStamp('Latent load to be met = ' &
           //TRIM(TrimSigDigits(MoistureLoad,2))//' (watts), and the simulation continues.')
    END IF
    CALL ShowRecurringWarningErrorAtEnd(TRIM(UnitarySystem(UnitarySysNum)%UnitarySystemType)//' "'&
        //TRIM(UnitarySystem(UnitarySysNum)%Name)//'" - '// &
        ' Latent part-load ratio out of range error continues. Latent load statistics:' &
        ,UnitarySystem(UnitarySysNum)%LatRegulaFalsIFailedIndex,MoistureLoad,MoistureLoad)
  END IF

  FullSensibleOutput = TempSensOutput

  CpAir = PsyCpAirFnWTdb(Node(UnitarySystem(UnitarySysNum)%CoolCoilInletNodeNum)%HumRat, &
                         Node(UnitarySystem(UnitarySysNum)%CoolCoilInletNodeNum)%Temp)
  CoolingOnlySensibleOutput = Node(UnitarySystem(UnitarySysNum)%CoolCoilInletNodeNum)%MassFlowRate * CpAir * &
     ((Node(UnitarySystem(UnitarySysNum)%NodeNumOfControlledZone)%Temp-&
      Node(UnitarySystem(UnitarySysNum)%CoolCoilOutletNodeNum)%Temp)- &
      (Node(UnitarySystem(UnitarySysNum)%HeatCoilOutletNodeNum)%Temp-&
      Node(UnitarySystem(UnitarySysNum)%HeatCoilInletNodeNum)%Temp))
  IF(QToHeatSetPt .LT. 0.0d0)THEN
!   Calculate the reheat coil load wrt the heating setpoint temperature. Reheat coil picks up
!   the entire excess sensible cooling (DX cooling coil and impact of outdoor air).
    UnitarySystem(UnitarySysNum)%DehumidInducedHeatingDemandRate = MAX(0.0d0,(CoolingOnlySensibleOutput+QToHeatSetPt))
!   Heating mode and dehumidification is required
  ELSEIF(QToHeatSetPt .GE. 0.0d0)THEN
!   Calculate the reheat coil load as the sensible capacity of the DX cooling coil only. Let
!   the heating coil pick up the load due to outdoor air.
    UnitarySystem(UnitarySysNum)%DehumidInducedHeatingDemandRate = MAX(0.0d0,CoolingOnlySensibleOutput)
  END IF

  RETURN

END SUBROUTINE ControlUnitarySystemOutput

SUBROUTINE SetSpeedVariables(UnitarySysNum, SensibleLoad, PartLoadRatio)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Raustad, FSEC
          !       DATE WRITTEN   February 2013
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine determines operating PLR and calculates the load based system output.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE General,           ONLY: SolveRegulaFalsi, TrimSigDigits
  USE DataHeatBalFanSys, ONLY: TempControlType

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)              :: UnitarySysNum    ! Index of AirloopHVAC:UnitarySystem object
  LOGICAL, INTENT(IN)              :: SensibleLoad     ! True when meeting a sensible load (not a moisture load)
  REAL(R64), INTENT(IN)            :: PartLoadRatio    ! operating PLR

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  LOGICAL :: errflag         ! error flag returned from subroutine
  REAL(r64) :: RunTimeFrac   ! heat pump runtime fraction


  IF(HeatingLoad .AND. SensibleLoad)THEN
    UnitarySystem(UnitarySysNum)%CoolingSpeedRatio = 0.0d0
    UnitarySystem(UnitarySysNum)%CoolingCycRatio = 0.0d0
    IF(UnitarySystem(UnitarySysNum)%MultiSpeedHeatingCoil .OR. UnitarySystem(UnitarySysNum)%VarSpeedHeatingCoil) THEN
      IF(UnitarySystem(UnitarySysNum)%HeatingSpeedNum == 1)THEN
        UnitarySystem(UnitarySysNum)%HeatingSpeedRatio = 0.0d0
        UnitarySystem(UnitarySysNum)%HeatingCycRatio   = PartLoadRatio
      ELSE
        UnitarySystem(UnitarySysNum)%HeatingSpeedRatio = PartLoadRatio
        UnitarySystem(UnitarySysNum)%HeatingCycRatio   = 1.0d0
      END IF
    ELSEIF(UnitarySystem(UnitarySysNum)%HeatingCoilType_Num == Coil_HeatingWaterToAirHPSimple .OR. &
            UnitarySystem(UnitarySysNum)%HeatingCoilType_Num == Coil_HeatingWaterToAirHP)THEN
      CALL HeatPumpRunFrac(UnitarySysNum,PartLoadRatio,errflag,RuntimeFrac)
      IF(RuntimeFrac > 0.0d0 .AND. UnitarySystem(UnitarySysNum)%FanOpMode == ContFanCycCoil)THEN
        OnOffFanPartLoadFraction = PartLoadRatio/RuntimeFrac
      ELSE
        OnOffFanPartLoadFraction = 1
      END IF
      UnitarySystem(UnitarySysNum)%CompPartLoadRatio = PartLoadRatio
      UnitarySystem(UnitarySysNum)%WSHPRuntimeFrac   = RuntimeFrac
    END IF
  ELSE
    UnitarySystem(UnitarySysNum)%HeatingSpeedRatio = 0.0d0
    UnitarySystem(UnitarySysNum)%HeatingCycRatio = 0.0d0
    IF(UnitarySystem(UnitarySysNum)%MultiSpeedCoolingCoil .OR. &
       UnitarySystem(UnitarySysNum)%VarSpeedCoolingCoil) THEN
      IF(UnitarySystem(UnitarySysNum)%CoolingSpeedNum == 1)THEN
        UnitarySystem(UnitarySysNum)%CoolingSpeedRatio = 0.0d0
        UnitarySystem(UnitarySysNum)%CoolingCycRatio   = PartLoadRatio
      ELSE
        UnitarySystem(UnitarySysNum)%CoolingSpeedRatio = PartLoadRatio
        UnitarySystem(UnitarySysNum)%CoolingCycRatio   = 1.0d0
      END IF
    ELSEIF(UnitarySystem(UnitarySysNum)%CoolingCoilType_Num == Coil_CoolingWaterToAirHPSimple .OR. &
            UnitarySystem(UnitarySysNum)%CoolingCoilType_Num == Coil_CoolingWaterToAirHP)THEN
      CALL HeatPumpRunFrac(UnitarySysNum,PartLoadRatio,errflag,RuntimeFrac)
      IF(RuntimeFrac > 0.0d0 .AND. UnitarySystem(UnitarySysNum)%FanOpMode == ContFanCycCoil)THEN
        OnOffFanPartLoadFraction = PartLoadRatio/RuntimeFrac
      ELSE
        OnOffFanPartLoadFraction = 1.0d0
      END IF
      UnitarySystem(UnitarySysNum)%CompPartLoadRatio = PartLoadRatio
      UnitarySystem(UnitarySysNum)%WSHPRuntimeFrac   = RuntimeFrac
    ELSEIF(UnitarySystem(UnitarySysNum)%CoolingCoilType_Num == CoilDX_CoolingTwoSpeed) THEN
      IF(UnitarySystem(UnitarySysNum)%CoolingSpeedNum == 1)THEN
        UnitarySystem(UnitarySysNum)%CoolingSpeedRatio = 0.0d0
        UnitarySystem(UnitarySysNum)%CoolingCycRatio   = PartLoadRatio
      ELSE
        UnitarySystem(UnitarySysNum)%CoolingSpeedRatio = PartLoadRatio
        UnitarySystem(UnitarySysNum)%CoolingCycRatio   = 1.0d0
      END IF
    END IF
  END IF


  RETURN

END SUBROUTINE SetSpeedVariables

FUNCTION CalcUnitarySystemLoadResidual(PartLoadRatio, Par) RESULT (Residuum)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Richard Raustad, FSEC
          !       DATE WRITTEN   February 2013
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! To calculate the part-load ratio for the unitary system

          ! METHODOLOGY EMPLOYED:
          ! Use SolveRegulaFalsi to CALL this Function to converge on a solution

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
!       Par(1)  = REAL(UnitarySysNum,r64) ! Index to Unitary System
!       Par(2)  = 0.0                  ! FirstHVACIteration FLAG, IF 1.0 then TRUE, if 0.0 then FALSE
!       Par(3)  = REAL(OpMode,r64)     ! Fan control, IF 1.0 then cycling fan, if 0.0 then continuous fan
!       Par(4)  = REAL(CompOp,r64)     ! Compressor control, IF 1.0 then compressor ON, if 0.0 then compressor OFF
!       Par(5)  = SensLoad or MoistureLoad   ! Sensible or Latent load to be met by unitary system
!       Par(6)  = HeatingLoad or CoolingLoad ! Type of load FLAG, 0.0 IF heating load, 1.0 IF cooling or moisture load
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
  INTEGER    :: UnitarySysNum   ! Index to this unitary system
  LOGICAL    :: FirstHVACIteration ! FirstHVACIteration flag
  INTEGER    :: FanOpMode          ! Cycling fan or constant fan
  INTEGER    :: CompOp             ! Compressor on/off; 1=on, 0=off
  REAL(r64)  :: LoadToBeMet        ! Sensible or Latent load to be met
  REAL(r64)  :: OnOffAirFlowRatio  ! Ratio of compressor ON air mass flow to AVERAGE air mass flow over time step
  LOGICAL    :: HXUnitOn           ! flag to enable HX based on zone moisture load
  REAL(r64)  :: HeatPLR            ! heating coil part load ratio
  REAL(r64)  :: CoolPLR            ! cooling coil part load ratio
  LOGICAL    :: SensibleLoad       ! sensible load
  REAL(r64)  :: SensOutput         ! sensible output of system
  REAL(r64)  :: LatOutput          ! latent output of system

! Convert parameters to usable variables
  UnitarySysNum     = INT(Par(1))
  IF(Par(2) .EQ. 1.0d0)THEN
    FirstHVACIteration = .TRUE.
  ELSE
    FirstHVACIteration = .FALSE.
  END IF
  FanOpMode            = INT(Par(3))
  CompOp               = INT(Par(4))
  LoadToBeMet          = Par(5)
  OnOffAirFlowRatio    = Par(8)

  IF(Par(6) .EQ. 1.0d0)THEN
   CoolPLR   = PartLoadRatio
   HeatPLR   = 0.0d0
  ELSE
   CoolPLR   = 0.0d0
   HeatPLR   = PartLoadRatio
  END IF

  SensibleLoad = .FALSE.
  IF(Par(7) .EQ. 1.0d0) SensibleLoad = .TRUE.

  IF(Par(9) .EQ. 1.0d0)THEN
    HXUnitOn = .TRUE.
  ELSE
    HXUnitOn = .FALSE.
  END IF

 CALL SetSpeedVariables(UnitarySysNum,SensibleLoad,PartLoadRatio)

 CALL CalcUnitarySystemToLoad(UnitarySysNum,FirstHVACIteration,CoolPLR,HeatPLR, &
                    OnOffAirFlowRatio,SensOutput,LatOutput,HXUnitOn=HXUnitOn,CompOn=CompOp)

! Calculate residual based on output calculation flag
  IF(SensibleLoad) THEN
    IF(ABS(LoadToBeMet) .EQ. 0.0d0)THEN
      Residuum = (SensOutput - LoadToBeMet)/100.0d0
    ELSE
      Residuum = (SensOutput - LoadToBeMet)/LoadToBeMet
    END IF
  ELSE
    IF(ABS(LoadToBeMet) .EQ. 0.0d0)THEN
      Residuum = (LatOutput - LoadToBeMet)/100.0d0
    ELSE
      Residuum = (LatOutput - LoadToBeMet)/LoadToBeMet
    END IF
  END IF

RETURN
END FUNCTION CalcUnitarySystemLoadResidual

SUBROUTINE CalcUnitarySystemToLoad(UnitarySysNum,FirstHVACIteration,CoolPLR,HeatPLR, &
             OnOffAirFlowRatio,SensOutput,LatOutput,HXUnitOn,HeatCoilLoad,SuppCoilLoad,CompOn)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Raustad, FSEC
          !       DATE WRITTEN   February 2013
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine calculates the resulting performance of the unitary system given
          ! the operating PLR. System output is calculated with respect to zone condition.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE Fans,             ONLY: SimulateFanComponents
  USE Psychrometrics,   ONLY: PsyHFnTdbW, PsyCpAirFnWTdb

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER,   INTENT(IN)        :: UnitarySysNum    ! Index of AirloopHVAC:UnitarySystem object
  LOGICAL,   INTENT(IN)        :: FirstHVACIteration  ! True when first HVAC iteration
  REAL(R64), INTENT(IN)        :: CoolPLR             ! operating cooling part-load ratio []
  REAL(R64), INTENT(IN)        :: HeatPLR             ! operating cooling part-load ratio []
  REAL(R64), INTENT(INOUT)     :: OnOffAirFlowRatio   ! ratio of heating PLR to cooling PLR (is this correct?)
  REAL(R64), INTENT(INOUT)     :: SensOutput          ! sensible capacity (W)
  REAL(R64), INTENT(INOUT)     :: LatOutput           ! latent capacity (W)
  LOGICAL, INTENT(INOUT), OPTIONAL :: HXUnitOn        ! Flag to control HX for HXAssisted Cooling Coil
  REAL(r64), INTENT(INOUT), OPTIONAL :: HeatCoilLoad  ! Adjusted load to heating coil when SAT exceeds max limit (W)
  REAL(r64), INTENT(INOUT), OPTIONAL :: SuppCoilLoad  ! Adjusted load to supp heating coil when SAT exceeds max limit (W)
  INTEGER, OPTIONAL, INTENT(IN)   :: CompOn           ! Determines if compressor is on or off

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER                       :: OutletNode         ! DX System outlet node number
  REAL(R64)                     :: SuppPLR            ! supplemental heating coil operating part-load ratio
  REAL(R64)                     :: MinHumRatio        ! used to calculate delivered capacity
  REAL(R64)                     :: AirMassFlow        ! operating mass flow rate through unitary system (kg/s)
  REAL(R64)                     :: ZoneTemp           ! zone node temperature (C)
  REAL(R64)                     :: ZoneHumRat         ! zone node humidity ratio (kg-water/kg-dryair)
  REAL(R64)                     :: CoilCoolHeatRat    ! ratio of cooling to heating PLR for cycling fan RH control
  INTEGER                       :: CoolingCompOn      ! Compressor control (0=off, 1=on1)
  INTEGER                       :: HeatingCompOn      ! Compressor control (0=off, 1=on1)
  REAL(r64)                     :: MDotAir            ! inlet air mass flow rate [kg/s]
  REAL(r64)                     :: CpAir              ! average specific heat [J/kg-C]
  REAL(r64)                     :: CpAirIn            ! inlet air Cp  [J/kg-C]
  REAL(r64)                     :: CpAirOut           ! outlet air Cp [J/kg-C]
  REAL(r64)                     :: HCDeltaT           ! heating coil delta temperture [deltaC]
  REAL(r64)                     :: MaxHeatCoilLoad    ! maximum allowed coil load so max temp is not exceeded [W]

  CoolingCompOn=0
  IF(CoolPLR>0)THEN
    CoolingCompOn=1
    ! let logical override compressor status if present (tests if economizer can meet load without compressor)
    IF(PRESENT(CompOn))CoolingCompOn=CompOn
  ! for multispeed coils, comp is on IF speed > 1
  ELSEIF(UnitarySystem(UnitarySysNum)%CoolingSpeedNum .GT. 1)THEN
    CoolingCompOn=1
  END IF

  HeatingCompOn=0
  IF(HeatPLR>0)THEN
    HeatingCompOn=1
    ! let logical override compressor status if present (tests if economizer can meet load without compressor)
    ! probably don't need this for heating mode
    IF(PRESENT(CompOn))HeatingCompOn=CompOn
    CoilCoolHeatRat = MIN(1.0d0,CoolPLR/HeatPLR)
  ELSE
    CoilCoolHeatRat = 1.0d0
  END IF
  ! for multispeed coils, comp is on at PLR=0 IF speed > 1
  IF(UnitarySystem(UnitarySysNum)%HeatingSpeedNum .GT. 1)HeatingCompOn=1

  ! set the operating flow rate
  IF(UnitarySystem(UnitarySysNum)%NumOfSpeedCooling > 0 .OR. &
     UnitarySystem(UnitarySysNum)%NumOfSpeedHeating > 0)THEN
     CALL SetOnOffMassFlowRate(UnitarySysNum, OnOffAirFlowRatio, MAX(CoolPLR,HeatPLR))
  ELSE
    CALL SetAverageAirFlow(UnitarySysNum, MAX(CoolPLR,HeatPLR), OnOffAirFlowRatio)
  END IF

  ! Call the series of components that simulate a Unitary System
  IF (UnitarySystem(UnitarySysNum)%FanExists .AND. UnitarySystem(UnitarySysNum)%FanPlace .EQ. BlowThru) THEN
    CALL SimulateFanComponents(Blank,FirstHVACIteration,UnitarySystem(UnitarySysNum)%FanIndex,FanSpeedRatio)
  END IF

  IF(UnitarySystem(UnitarySysNum)%CoolingCoilUpstream)THEN

    IF (UnitarySystem(UnitarySysNum)%CoolCoilExists)THEN
      CALL CalcUnitaryCoolingSystem(UnitarySysNum, FirstHVACIteration, CoolPLR, CoolingCompOn, OnOffAirFlowRatio, &
                                    CoilCoolHeatRat, HXUnitOn)
    END IF
    IF (UnitarySystem(UnitarySysNum)%HeatCoilExists)THEN
      ! operate the heating coil without regard to coil outlet temperature
      CALL CalcUnitaryHeatingSystem(UnitarySysNum, FirstHVACIteration, HeatPLR, HeatingCompOn, &
                                    OnOffAirFlowRatio, HeatCoilLoad)
      IF(Node(UnitarySystem(UnitarySysNum)%HeatCoilOutletNodeNum)%Temp .GT. &
           UnitarySystem(UnitarySysNum)%DesignMaxOutletTemp)THEN
        MDotAir = Node(UnitarySystem(UnitarySysNum)%HeatCoilInletNodeNum)%MassFlowRate
        CpAirIn  = PsyCpAirFnWTdb(Node(UnitarySystem(UnitarySysNum)%HeatCoilInletNodeNum)%HumRat, &
                                  Node(UnitarySystem(UnitarySysNum)%HeatCoilInletNodeNum)%Temp)
        CpAirOut = PsyCpAirFnWTdb(Node(UnitarySystem(UnitarySysNum)%HeatCoilOutletNodeNum)%HumRat, &
                                  Node(UnitarySystem(UnitarySysNum)%HeatCoilOutletNodeNum)%Temp)
        CpAir = (CpAirIn + CpAirout)/2
        HCDeltaT = UnitarySystem(UnitarySysNum)%DesignMaxOutletTemp - &
                   Node(UnitarySystem(UnitarySysNum)%HeatCoilInletNodeNum)%Temp
        MaxHeatCoilLoad = MDotAir * CpAir * HCDeltaT
        CALL CalcUnitaryHeatingSystem(UnitarySysNum, FirstHVACIteration, HeatPLR, HeatingCompOn, &
                                      OnOffAirFlowRatio, MaxHeatCoilLoad)
        IF(PRESENT(HeatCoilLoad))HeatCoilLoad = MaxHeatCoilLoad
      END IF
    END IF

    ! If blow thru fan is used, the fan must be simulated after coil sets OnOffFanPartLoadFraction
    IF (UnitarySystem(UnitarySysNum)%FanExists .AND. UnitarySystem(UnitarySysNum)%FanPlace .EQ. BlowThru .AND. &
        OnOffFanPartLoadFraction < 1.0d0) THEN
      CALL SimulateFanComponents(Blank,FirstHVACIteration,UnitarySystem(UnitarySysNum)%FanIndex,FanSpeedRatio)

      IF (UnitarySystem(UnitarySysNum)%CoolCoilExists)THEN
        CALL CalcUnitaryCoolingSystem(UnitarySysNum, FirstHVACIteration, CoolPLR, CoolingCompOn, OnOffAirFlowRatio, &
                                    CoilCoolHeatRat, HXUnitOn)
      END IF
      IF (UnitarySystem(UnitarySysNum)%HeatCoilExists)THEN
        CALL CalcUnitaryHeatingSystem(UnitarySysNum, FirstHVACIteration, HeatPLR, HeatingCompOn, OnOffAirFlowRatio, HeatCoilLoad)
        IF(Node(UnitarySystem(UnitarySysNum)%HeatCoilOutletNodeNum)%Temp .GT. &
           UnitarySystem(UnitarySysNum)%DesignMaxOutletTemp)THEN
          MDotAir = Node(UnitarySystem(UnitarySysNum)%HeatCoilInletNodeNum)%MassFlowRate
          CpAirIn  = PsyCpAirFnWTdb(Node(UnitarySystem(UnitarySysNum)%HeatCoilInletNodeNum)%HumRat, &
                                    Node(UnitarySystem(UnitarySysNum)%HeatCoilInletNodeNum)%Temp)
          CpAirOut = PsyCpAirFnWTdb(Node(UnitarySystem(UnitarySysNum)%HeatCoilOutletNodeNum)%HumRat, &
                                    Node(UnitarySystem(UnitarySysNum)%HeatCoilOutletNodeNum)%Temp)
          CpAir = (CpAirIn + CpAirout)/2
          HCDeltaT = UnitarySystem(UnitarySysNum)%DesignMaxOutletTemp - &
                     Node(UnitarySystem(UnitarySysNum)%HeatCoilInletNodeNum)%Temp
          MaxHeatCoilLoad = MDotAir * CpAir * HCDeltaT
          CALL CalcUnitaryHeatingSystem(UnitarySysNum, FirstHVACIteration, HeatPLR, HeatingCompOn, &
                                        OnOffAirFlowRatio, MaxHeatCoilLoad)
        END IF
      END IF
    END IF

  ELSE

    IF (UnitarySystem(UnitarySysNum)%HeatCoilExists)THEN
      CALL CalcUnitaryHeatingSystem(UnitarySysNum, FirstHVACIteration, HeatPLR, HeatingCompOn, OnOffAirFlowRatio, HeatCoilLoad)
      IF(Node(UnitarySystem(UnitarySysNum)%HeatCoilOutletNodeNum)%Temp .GT. &
           UnitarySystem(UnitarySysNum)%DesignMaxOutletTemp)THEN
        MDotAir = Node(UnitarySystem(UnitarySysNum)%HeatCoilInletNodeNum)%MassFlowRate
        CpAirIn  = PsyCpAirFnWTdb(Node(UnitarySystem(UnitarySysNum)%HeatCoilInletNodeNum)%HumRat, &
                                  Node(UnitarySystem(UnitarySysNum)%HeatCoilInletNodeNum)%Temp)
        CpAirOut = PsyCpAirFnWTdb(Node(UnitarySystem(UnitarySysNum)%HeatCoilOutletNodeNum)%HumRat, &
                                  Node(UnitarySystem(UnitarySysNum)%HeatCoilOutletNodeNum)%Temp)
        CpAir = (CpAirIn + CpAirout)/2
        HCDeltaT = UnitarySystem(UnitarySysNum)%DesignMaxOutletTemp - &
                   Node(UnitarySystem(UnitarySysNum)%HeatCoilInletNodeNum)%Temp
        MaxHeatCoilLoad = MDotAir * CpAir * HCDeltaT
        CALL CalcUnitaryHeatingSystem(UnitarySysNum, FirstHVACIteration, HeatPLR, HeatingCompOn, &
                                      OnOffAirFlowRatio, MaxHeatCoilLoad)
      END IF
    END IF
    IF (UnitarySystem(UnitarySysNum)%CoolCoilExists)THEN
      CALL CalcUnitaryCoolingSystem(UnitarySysNum, FirstHVACIteration, CoolPLR, CoolingCompOn, OnOffAirFlowRatio, &
                                    CoilCoolHeatRat, HXUnitOn)
    END IF

    ! If blow thru fan is used, the fan must be simulated after coil sets OnOffFanPartLoadFraction
    IF (UnitarySystem(UnitarySysNum)%FanExists .AND. UnitarySystem(UnitarySysNum)%FanPlace .EQ. BlowThru .AND. &
        OnOffFanPartLoadFraction < 1.0d0) THEN
      CALL SimulateFanComponents(Blank,FirstHVACIteration,UnitarySystem(UnitarySysNum)%FanIndex,FanSpeedRatio)

      IF (UnitarySystem(UnitarySysNum)%HeatCoilExists)THEN
        CALL CalcUnitaryHeatingSystem(UnitarySysNum, FirstHVACIteration, HeatPLR, HeatingCompOn, OnOffAirFlowRatio, HeatCoilLoad)
        IF(Node(UnitarySystem(UnitarySysNum)%HeatCoilOutletNodeNum)%Temp .GT. &
           UnitarySystem(UnitarySysNum)%DesignMaxOutletTemp)THEN
          MDotAir = Node(UnitarySystem(UnitarySysNum)%HeatCoilInletNodeNum)%MassFlowRate
          CpAirIn  = PsyCpAirFnWTdb(Node(UnitarySystem(UnitarySysNum)%HeatCoilInletNodeNum)%HumRat, &
                                    Node(UnitarySystem(UnitarySysNum)%HeatCoilInletNodeNum)%Temp)
          CpAirOut = PsyCpAirFnWTdb(Node(UnitarySystem(UnitarySysNum)%HeatCoilOutletNodeNum)%HumRat, &
                                    Node(UnitarySystem(UnitarySysNum)%HeatCoilOutletNodeNum)%Temp)
          CpAir = (CpAirIn + CpAirout)/2
          HCDeltaT = UnitarySystem(UnitarySysNum)%DesignMaxOutletTemp - &
                     Node(UnitarySystem(UnitarySysNum)%HeatCoilInletNodeNum)%Temp
          MaxHeatCoilLoad = MDotAir * CpAir * HCDeltaT
          CALL CalcUnitaryHeatingSystem(UnitarySysNum, FirstHVACIteration, HeatPLR, HeatingCompOn, &
                                        OnOffAirFlowRatio, MaxHeatCoilLoad)
        END IF
      END IF
      IF (UnitarySystem(UnitarySysNum)%CoolCoilExists)THEN
        CALL CalcUnitaryCoolingSystem(UnitarySysNum, FirstHVACIteration, CoolPLR, CoolingCompOn, OnOffAirFlowRatio, &
                                    CoilCoolHeatRat, HXUnitOn)
      END IF
    END IF

  END IF

  IF (UnitarySystem(UnitarySysNum)%FanExists .AND. UnitarySystem(UnitarySysNum)%FanPlace .EQ. DrawThru) THEN
    CALL SimulateFanComponents(Blank,FirstHVACIteration,UnitarySystem(UnitarySysNum)%FanIndex,FanSpeedRatio)
  END IF

  SuppPLR = UnitarySystem(UnitarySysNum)%SuppHeatPartLoadFrac
  IF (UnitarySystem(UnitarySysNum)%SuppCoilExists)THEN
    CALL CalcUnitarySuppHeatingSystem(UnitarySysNum, FirstHVACIteration, SuppPLR, SuppCoilLoad)
    IF((Node(UnitarySystem(UnitarySysNum)%SuppCoilAirOutletNode)%Temp .GT. &
        UnitarySystem(UnitarySysNum)%DesignMaxOutletTemp) .AND. SuppPLR .GT. 0.0d0)THEN
      MDotAir = Node(UnitarySystem(UnitarySysNum)%SuppCoilAirInletNode)%MassFlowRate
      CpAirIn  = PsyCpAirFnWTdb(Node(UnitarySystem(UnitarySysNum)%SuppCoilAirInletNode)%HumRat, &
                                Node(UnitarySystem(UnitarySysNum)%SuppCoilAirInletNode)%Temp)
      CpAirOut = PsyCpAirFnWTdb(Node(UnitarySystem(UnitarySysNum)%SuppCoilAirOutletNode)%HumRat, &
                                Node(UnitarySystem(UnitarySysNum)%SuppCoilAirOutletNode)%Temp)
      CpAir = (CpAirIn + CpAirout)/2
      HCDeltaT = MAX(0.0d0,UnitarySystem(UnitarySysNum)%DesignMaxOutletTemp - &
                           Node(UnitarySystem(UnitarySysNum)%SuppCoilAirInletNode)%Temp)
      MaxHeatCoilLoad = MDotAir * CpAir * HCDeltaT
      CALL CalcUnitarySuppHeatingSystem(UnitarySysNum, FirstHVACIteration, SuppPLR, MaxHeatCoilLoad)
      IF(PRESENT(SuppCoilLoad))SuppCoilLoad = MaxHeatCoilLoad
    END IF
  END IF

 ! Check delta T (outlet to space), IF positive
 ! use space HumRat (next line), ELSE outlet humrat (IF) so psyc routine gives good result
  MinHumRatio = Node(UnitarySystem(UnitarySysNum)%NodeNumofControlledZone)%HumRat
  OutletNode = UnitarySystem(UnitarySysNum)%UnitarySystemOutletNodeNum
  AirMassFlow = Node(OutletNode)%MassFlowRate
  ZoneTemp = Node(UnitarySystem(UnitarySysNum)%NodeNumofControlledZone)%Temp
  ZoneHumRat = Node(UnitarySystem(UnitarySysNum)%NodeNumofControlledZone)%HumRat
  IF(Node(OutletNode)%Temp .LT. ZoneTemp ) MinHumRatio = Node(OutletNode)%HumRat

 ! Calculate sensible load met (at constant humidity ratio)
  SensOutput = AirMassFlow * (PsyHFnTdbW(Node(OutletNode)%Temp,MinHumRatio)  &
         - PsyHFnTdbW(ZoneTemp,MinHumRatio)) &
         - UnitarySystem(UnitarySysNum)%SenLoadLoss
  UnitarySystem(UnitarySysNum)%SensibleLoadMet = SensOutput

  IF(UnitarySystem(UnitarySysNum)%Humidistat)THEN

!   Calculate latent load met (at constant temperature)
    LatOutput = AirMassFlow * (PsyHFnTdbW(ZoneTemp,Node(OutletNode)%HumRat)  &
         - PsyHFnTdbW(ZoneTemp,ZoneHumRat)) - UnitarySystem(UnitarySysNum)%LatLoadLoss
  ELSE
    LatOutput = 0.0d0
  END IF
  UnitarySystem(UnitarySysNum)%LatentLoadMet = LatOutput

  RETURN

END SUBROUTINE CalcUnitarySystemToLoad

SUBROUTINE CalcUnitaryCoolingSystem(UnitarySysNum, FirstHVACIteration, PartLoadRatio, &
                                    CompOn, OnOffAirFlowRatio, CoilCoolHeatRat, HXUnitOn)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Raustad, FSEC
          !       DATE WRITTEN   February 2013
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine manages unitary cooling system component simulation.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DXCoils,                   ONLY: SimDXCoil, SimDXCoilMultiSpeed, SimDXCoilMultiMode
  USE HVACHXAssistedCoolingCoil, ONLY: SimHXAssistedCoolingCoil
  USE Fans,                      ONLY: SimulateFanComponents
  USE WaterCoils,                ONLY: SimulateWaterCoilComponents
  USE VariableSpeedCoils,        ONLY: SimVariableSpeedCoils
  USE WatertoAirHeatPumpSimple,  ONLY: SimWatertoAirHPSimple
  USE WatertoAirHeatPump,        ONLY: SimWaterToAirHP

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)          :: UnitarySysNum       ! Index of AirloopHVAC:UnitarySystem object
  LOGICAL, INTENT(IN)          :: FirstHVACIteration  ! True when first HVAC iteration
  REAL(R64), INTENT(IN)        :: PartLoadRatio       ! coil operating part-load ratio
  INTEGER, INTENT(IN)          :: CompOn              ! compressor control (0=off, 1=on)
  REAL(R64), INTENT(IN)        :: OnOffAirFlowRatio   !
  REAL(R64), INTENT(IN)        :: CoilCoolHeatRat     ! ratio of cooling to heating PLR for cycling fan RH control
  LOGICAL, INTENT(INOUT), OPTIONAL :: HXUnitOn        ! Flag to control HX for HXAssisted Cooling Coil

          ! SUBROUTINE PARAMETER DEFINITIONS:

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  CHARACTER(len=MaxNameLength)  :: CompName            ! Name of Unitary System object
  INTEGER                       :: CompIndex           ! index to cooling coil
  REAL(R64)                     :: OutsideDryBulbTemp  ! outdoor temperature (C)
  REAL(r64)                     :: mdot                ! water side flow rate (kg/s)
  REAL(r64)                     :: QActual             ! actual coil output (W)
  REAL(r64)                     :: OutdoorPressure     ! Outdoor barometric pressure at condenser (Pa)
  LOGICAL                       :: errflag             ! returned flag from called routine

! Simulate the coil component
  CompName = UnitarySystem(UnitarySysNum)%CoolingCoilName
  CompIndex = UnitarySystem(UnitarySysNum)%CoolingCoilIndex
  IF (UnitarySystem(UnitarySysNum)%CondenserNodeNum /= 0) THEN
    OutdoorPressure = Node(UnitarySystem(UnitarySysNum)%CondenserNodeNum)%Press
    ! IF node is not connected to anything, pressure = default, use weather data
    IF(OutdoorPressure == DefaultNodeValues%Press)THEN
      OutsideDryBulbTemp  = OutDryBulbTemp
!      OutdoorHumRat   = OutHumRat
!      OutdoorPressure = OutBaroPress
!      OutdoorWetBulb  = OutWetBulbTemp
    ELSE
      OutsideDryBulbTemp  = Node(UnitarySystem(UnitarySysNum)%CondenserNodeNum)%Temp
!      OutdoorHumRat   = Node(UnitarySystem(UnitarySysNum)%CondenserNodeNum)%HumRat
!      OutdoorWetBulb  = PsyTwbFnTdbWPb(OutdoorDryBulb,OutdoorHumRat,OutdoorPressure,RoutineName)
    END IF
  ELSE
    OutsideDryBulbTemp  = OutDryBulbTemp
!    OutdoorHumRat   = OutHumRat
!    OutdoorPressure = OutBaroPress
!    OutdoorWetBulb  = OutWetBulbTemp
  END IF
!  PartLoadRatio = UnitarySystem(UnitarySysNum)%CoolingPartLoadFrac

  SELECT CASE(UnitarySystem(UnitarySysNum)%CoolingCoilType_Num)

    CASE (CoilDX_CoolingSingleSpeed) ! Coil:Cooling:DX:SingleSpeed

      CALL SimDXCoil(Blank,CompOn,FirstHVACIteration, PartLoadRatio,  &
         CompIndex, &
         UnitarySystem(UnitarySysNum)%FanOpMode, &
         CoilCoolingHeatingPLRRatio = CoilCoolHeatRat)
      UnitarySystem(UnitarySysNum)%CoolCompPartLoadRatio = PartLoadRatio*REAL(CompOn,r64)

    CASE (CoilDX_CoolingHXAssisted,CoilWater_CoolingHXAssisted)  ! CoilSystem:Cooling:*:HeatExchangerAssisted

      IF(UnitarySystem(UnitarySysNum)%CoolingCoilType_Num==CoilWater_CoolingHXAssisted)THEN
        mdot = MIN(Node(UnitarySystem(UnitarySysNum)%CoolCoilFluidOutletNodeNum)%MassFlowRateMaxAvail, &
                 UnitarySystem(UnitarySysNum)%MaxCoolCoilFluidFlow * PartLoadRatio)
        Node(UnitarySystem(UnitarySysNum)%CoolCoilFluidInletNode)%MassFlowRate = mdot
      END IF
      CALL SimHXAssistedCoolingCoil(Blank,FirstHVACIteration,CompOn,PartLoadRatio,  &
                                CompIndex, UnitarySystem(UnitarySysNum)%FanOpMode, &
                                HXUnitEnable=HXUnitOn,EconomizerFlag=EconomizerFlag)
      IF(UnitarySystem(UnitarySysNum)%CoolingCoilType_Num==CoilDX_CoolingHXAssisted) &
           UnitarySystem(UnitarySysNum)%CoolCompPartLoadRatio = PartLoadRatio*REAL(CompOn,r64)

    CASE (CoilDX_CoolingTwoSpeed)  ! Coil:Cooling:DX:TwoSpeed
                                   ! formerly (v3 and beyond)COIL:DX:MULTISPEED:COOLINGEMPIRICAL

      CALL SimDXCoilMultiSpeed(Blank,UnitarySystem(UnitarySysNum)%CoolingSpeedRatio,&
                     UnitarySystem(UnitarySysNum)%CoolingCycRatio,CompIndex)
      IF(UnitarySystem(UnitarySysNum)%CoolingSpeedRatio .GT. 0.0d0)THEN
        UnitarySystem(UnitarySysNum)%CoolCompPartLoadRatio = UnitarySystem(UnitarySysNum)%CoolingSpeedRatio*REAL(CompOn,r64)
      ELSE
        UnitarySystem(UnitarySysNum)%CoolCompPartLoadRatio = UnitarySystem(UnitarySysNum)%CoolingCycRatio*REAL(CompOn,r64)
      END IF

    CASE (CoilDX_MultiSpeedCooling) ! Coil:Cooling:DX:Multispeed

      IF(OutsideDryBulbTemp .GT. UnitarySystem(UnitarySysNum)%MinOATCompressor) THEN
        CALL SimDXCoilMultiSpeed(CompName,UnitarySystem(UnitarySysNum)%CoolingSpeedRatio, &
                                 UnitarySystem(UnitarySysNum)%CoolingCycRatio,&
                                 CompIndex, &
                                 UnitarySystem(UnitarySysNum)%CoolingSpeedNum, &
                                 UnitarySystem(UnitarySysNum)%FanOpMode,CompOn)
        IF(UnitarySystem(UnitarySysNum)%CoolingSpeedNum .GT. 1)THEN
          UnitarySystem(UnitarySysNum)%CoolCompPartLoadRatio = REAL(CompOn,r64)
        ELSE
          UnitarySystem(UnitarySysNum)%CoolCompPartLoadRatio = UnitarySystem(UnitarySysNum)%CoolingCycRatio*REAL(CompOn,r64)
        END IF
      ELSE
        CALL SimDXCoilMultiSpeed(CompName,0.0d0,0.0d0,&
                                 CompIndex, &
                                 UnitarySystem(UnitarySysNum)%CoolingSpeedNum, &
                                 UnitarySystem(UnitarySysNum)%FanOpMode,CompOn)
        UnitarySystem(UnitarySysNum)%CoolCompPartLoadRatio = 0.0d0
      END IF

    CASE (CoilDX_CoolingTwoStageWHumControl)  ! Coil:Cooling:DX:TwoStageWithHumidityControlMode
                                     ! formerly (v3 and beyond) COIL:DX:MULTIMODE:COOLINGEMPIRICAL

      CALL SimDXCoilMultiMode(CompName,CompOn,FirstHVACIteration,PartLoadRatio, &
                     UnitarySystem(UnitarySysNum)%dehumidificationMode, &
                     CompIndex, &
                     UnitarySystem(UnitarySysNum)%FanOpMode)
      UnitarySystem(UnitarySysNum)%CoolCompPartLoadRatio = PartLoadRatio*REAL(CompOn,r64)

    CASE (Coil_CoolingWater, Coil_CoolingWaterDetailed)
      mdot = MIN(Node(UnitarySystem(UnitarySysNum)%CoolCoilFluidOutletNodeNum)%MassFlowRateMaxAvail, &
                 UnitarySystem(UnitarySysNum)%MaxCoolCoilFluidFlow * PartLoadRatio)
      Node(UnitarySystem(UnitarySysNum)%CoolCoilFluidInletNode)%MassFlowRate = mdot
      CALL SimulateWaterCoilComponents(CompName,FirstHVACIteration, &
                                           UnitarySystem(UnitarySysNum)%CoolingCoilIndex,  &
                                           QActual,UnitarySystem(UnitarySysNum)%FanOpMode, &
                                           PartLoadRatio)

    CASE (Coil_CoolingAirToAirVariableSpeed, Coil_CoolingWaterToAirHPVSEquationFit)
      CALL SimVariableSpeedCoils(CompName,CompIndex,&
           UnitarySystem(UnitarySysNum)%FanOpMode,UnitarySystem(UnitarySysNum)%MaxONOFFCyclesperHour, &
           UnitarySystem(UnitarySysNum)%HPTimeConstant,UnitarySystem(UnitarySysNum)%FanDelayTime,&
           CompOn, PartLoadRatio, OnOffAirFlowRatio, &
           UnitarySystem(UnitarySysNum)%CoolingSpeedNum, &
           UnitarySystem(UnitarySysNum)%CoolingSpeedRatio,&
           UnitarySystem(UnitarySysNum)%CoolingCoilSensDemand, &
           UnitarySystem(UnitarySysNum)%CoolingCoilLatentDemand )


    CASE(Coil_CoolingWaterToAirHPSimple)

        IF(PartLoadRatio > 0.0d0 .AND. UnitarySystem(UnitarySysNum)%WSHPRuntimeFrac > 0.0d0 .AND. &
           UnitarySystem(UnitarySysNum)%FanOpMode == CycFanCycCoil)THEN
          OnOffFanPartLoadFraction = PartLoadRatio/UnitarySystem(UnitarySysNum)%WSHPRuntimeFrac
        END IF

        CALL SimWatertoAirHPSimple(Blank, UnitarySystem(UnitarySysNum)%CoolingCoilIndex, &
                                   UnitarySystem(UnitarySysNum)%CoolingCoilSensDemand, &
                                   UnitarySystem(UnitarySysNum)%CoolingCoilLatentDemand, &
                                   UnitarySystem(UnitarySysNum)%FanOpMode,UnitarySystem(UnitarySysNum)%WSHPRuntimeFrac, &
                                   UnitarySystem(UnitarySysNum)%MaxONOFFCyclesperHour, &
                                   UnitarySystem(UnitarySysNum)%HPTimeConstant, UnitarySystem(UnitarySysNum)%FanDelayTime, &
                                   CompOn, PartLoadRatio, FirstHVACIteration)

        UnitarySystem(UnitarySysNum)%CoolCompPartLoadRatio = PartLoadRatio*REAL(CompOn,r64)

    CASE(Coil_CoolingWaterToAirHP)

        CALL HeatPumpRunFrac(UnitarySysNum,PartLoadRatio,errflag,UnitarySystem(UnitarySysNum)%WSHPRuntimeFrac)

        IF(PartLoadRatio > 0.0d0 .AND. UnitarySystem(UnitarySysNum)%WSHPRuntimeFrac > 0.0d0 .AND. &
           UnitarySystem(UnitarySysNum)%FanOpMode == CycFanCycCoil)THEN
          OnOffFanPartLoadFraction = PartLoadRatio/UnitarySystem(UnitarySysNum)%WSHPRuntimeFrac
        END IF

        CALL SimWatertoAirHP(Blank, UnitarySystem(UnitarySysNum)%CoolingCoilIndex, &
              UnitarySystem(UnitarySysNum)%MaxCoolAirMassFlow,UnitarySystem(UnitarySysNum)%FanOpMode,&
              FirstHVACIteration,UnitarySystem(UnitarySysNum)%WSHPRuntimeFrac,&
              UnitarySystem(UnitarySysNum)%MaxONOFFCyclesperHour, &
              UnitarySystem(UnitarySysNum)%HPTimeConstant, &
              UnitarySystem(UnitarySysNum)%FanDelayTime, &
              UnitarySystem(UnitarySysNum)%InitHeatPump, &
              UnitarySystem(UnitarySysNum)%CoolingCoilSensDemand, &
              UnitarySystem(UnitarySysNum)%CoolingCoilLatentDemand,CompOn, PartLoadRatio)

        UnitarySystem(UnitarySysNum)%CoolCompPartLoadRatio = PartLoadRatio*REAL(CompOn,r64)

  END SELECT

  UnitarySystem(UnitarySysNum)%CoolingPartLoadFrac = PartLoadRatio

  RETURN

END SUBROUTINE CalcUnitaryCoolingSystem

SUBROUTINE CalcUnitaryHeatingSystem(UnitarySysNum, FirstHVACIteration, PartLoadRatio, CompOn, OnOffAirFlowRatio, HeatCoilLoad)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Raustad, FSEC
          !       DATE WRITTEN   February 2013
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine manages unitary heating system component simulation.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DXCoils,                   ONLY: SimDXCoil, SimDXCoilMultiSpeed
  USE HeatingCoils,              ONLY: SimulateHeatingCoilComponents
  USE WaterCoils,                ONLY: SimulateWaterCoilComponents
  USE SteamCoils,                ONLY: SimulateSteamCoilComponents
  USE VariableSpeedCoils,        ONLY: SimVariableSpeedCoils
  USE WatertoAirHeatPumpSimple,  ONLY: SimWatertoAirHPSimple
  USE WatertoAirHeatPump,        ONLY: SimWaterToAirHP

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)          :: UnitarySysNum    ! Index of AirloopHVAC:UnitarySystem object
  LOGICAL, INTENT(IN)          :: FirstHVACIteration  ! True when first HVAC iteration
  REAL(R64), INTENT(IN)        :: PartLoadRatio       ! coil operating part-load ratio
  INTEGER, INTENT(IN)          :: CompOn              ! comrpressor control (0=off, 1=on)
  REAL(R64), INTENT(IN)        :: OnOffAirFlowRatio   ! ratio of on to off flow rate
  REAL(r64), OPTIONAL, INTENT(IN) :: HeatCoilLoad     ! adjusted heating coil load if outlet temp exceeds max (W)

          ! SUBROUTINE PARAMETER DEFINITIONS:

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  CHARACTER(len=MaxNameLength)  :: CompName            ! Name of Unitary System object
  REAL(R64)                     :: OutsideDryBulbTemp  ! outdoor temperature (C)
  REAL(r64)                     :: mdot                ! water side flow rate (kg/s)
  REAL(r64)                     :: QActual             ! actual output of coil (W)
  REAL(r64)                     :: dummy               ! used when sub argument is not needed
  REAL(r64)                     :: OutdoorPressure     ! Outdoor barometric pressure at condenser (Pa)
  LOGICAL                       :: errflag             ! returned flag from called routine

  CompName = UnitarySystem(UnitarySysNum)%HeatingCoilName
  dummy = 0.0d0
  IF (UnitarySystem(UnitarySysNum)%CondenserNodeNum /= 0) THEN
    OutdoorPressure = Node(UnitarySystem(UnitarySysNum)%CondenserNodeNum)%Press
    ! IF node is not connected to anything, pressure = default, use weather data
    IF(OutdoorPressure == DefaultNodeValues%Press)THEN
      OutsideDryBulbTemp  = OutDryBulbTemp
!      OutdoorHumRat   = OutHumRat
!      OutdoorPressure = OutBaroPress
!      OutdoorWetBulb  = OutWetBulbTemp
    ELSE
      OutsideDryBulbTemp  = Node(UnitarySystem(UnitarySysNum)%CondenserNodeNum)%Temp
!      OutdoorHumRat   = Node(UnitarySystem(UnitarySysNum)%CondenserNodeNum)%HumRat
!      OutdoorWetBulb  = PsyTwbFnTdbWPb(OutdoorDryBulb,OutdoorHumRat,OutdoorPressure,RoutineName)
    END IF
  ELSE
    OutsideDryBulbTemp  = OutDryBulbTemp
!    OutdoorHumRat   = OutHumRat
!    OutdoorPressure = OutBaroPress
!    OutdoorWetBulb  = OutWetBulbTemp
  END IF

  SELECT CASE(UnitarySystem(UnitarySysNum)%HeatingCoilType_Num)

    CASE (CoilDX_HeatingEmpirical) ! COIL:HEATING:DX:SINGLESPEED

        CALL SimDXCoil(CompName,CompOn,FirstHVACIteration, PartLoadRatio,  &
                       UnitarySystem(UnitarySysNum)%HeatingCoilIndex, &
                       UnitarySystem(UnitarySysNum)%FanOpMode)
        UnitarySystem(UnitarySysNum)%HeatCompPartLoadRatio = PartLoadRatio*REAL(CompOn,r64)

    CASE (Coil_HeatingGas,Coil_HeatingElectric )
      IF(PRESENT(HeatCoilLoad))THEN
          CALL SimulateHeatingCoilComponents(CompName,FirstHVACIteration, &
                                CompIndex=UnitarySystem(UnitarySysNum)%HeatingCoilIndex, &
                                SuppHeat=.FALSE., &
                                QCoilReq=HeatCoilLoad, &
                                FanOpMode=UnitarySystem(UnitarySysNum)%FanOpMode, &
                                PartLoadRatio=PartLoadRatio)
      ELSE
          CALL SimulateHeatingCoilComponents(CompName,FirstHVACIteration, &
                                CompIndex=UnitarySystem(UnitarySysNum)%HeatingCoilIndex, &
                                SuppHeat=.FALSE., &
                                QCoilReq=UnitarySystem(UnitarySysNum)%DesignHeatingCapacity*PartLoadRatio, &
                                FanOpMode=UnitarySystem(UnitarySysNum)%FanOpMode, &
                                PartLoadRatio=PartLoadRatio)
      END IF
    CASE (Coil_HeatingDesuperheater )
      IF(PRESENT(HeatCoilLoad))THEN
          CALL SimulateHeatingCoilComponents(CompName,FirstHVACIteration, &
                                CompIndex=UnitarySystem(UnitarySysNum)%HeatingCoilIndex, &
                                SuppHeat=.FALSE., &
                                QCoilReq=HeatCoilLoad, &
                                FanOpMode=UnitarySystem(UnitarySysNum)%FanOpMode, &
                                PartLoadRatio=PartLoadRatio)
      ELSE
          CALL SimulateHeatingCoilComponents(CompName,FirstHVACIteration, &
                                CompIndex=UnitarySystem(UnitarySysNum)%HeatingCoilIndex, &
                                SuppHeat=.FALSE., &
                                QCoilReq=UnitarySystem(UnitarySysNum)%DesignHeatingCapacity*PartLoadRatio, &
                                FanOpMode=UnitarySystem(UnitarySysNum)%FanOpMode, &
                                PartLoadRatio=PartLoadRatio)
      END IF

    CASE(CoilDX_MultiSpeedHeating)

        IF(OutsideDryBulbTemp .GT. UnitarySystem(UnitarySysNum)%MinOATCompressor)THEN
          CALL SimDXCoilMultiSpeed(CompName,UnitarySystem(UnitarySysNum)%HeatingSpeedRatio, &
                                   UnitarySystem(UnitarySysNum)%HeatingCycRatio,&
                                   UnitarySystem(UnitarySysNum)%HeatingCoilIndex, &
                                   UnitarySystem(UnitarySysNum)%HeatingSpeedNum, &
                                   UnitarySystem(UnitarySysNum)%FanOpMode,CompOn)
          UnitarySystem(UnitarySysNum)%HeatCompPartLoadRatio = PartLoadRatio*REAL(CompOn,r64)
        ELSE
          CALL SimDXCoilMultiSpeed(CompName,0.0d0,0.0d0,&
                                   UnitarySystem(UnitarySysNum)%HeatingCoilIndex, &
                                   UnitarySystem(UnitarySysNum)%HeatingSpeedNum, &
                                   UnitarySystem(UnitarySysNum)%FanOpMode,CompOn)
          UnitarySystem(UnitarySysNum)%HeatCompPartLoadRatio = 0.0d0
        END IF

    CASE(Coil_HeatingElectric_MultiStage, Coil_HeatingGas_MultiStage)
        CALL SimulateHeatingCoilComponents(CompName,FirstHVACIteration, &
                                           CompIndex     = 0, &
                                           FanOpMode     = UnitarySystem(UnitarySysNum)%FanOpMode,      &
                                           PartLoadRatio = UnitarySystem(UnitarySysNum)%HeatingCycRatio, &
                                           StageNum      = UnitarySystem(UnitarySysNum)%HeatingSpeedNum,     &
                                           SpeedRatio    = UnitarySystem(UnitarySysNum)%HeatingSpeedRatio)

    CASE (Coil_HeatingWater)
          mdot = MIN(Node(UnitarySystem(UnitarySysNum)%HeatCoilFluidOutletNodeNum)%MassFlowRateMaxAvail, &
                 UnitarySystem(UnitarySysNum)%MaxHeatCoilFluidFlow * PartLoadRatio)
          Node(UnitarySystem(UnitarySysNum)%HeatCoilFluidInletNode)%MassFlowRate = mdot
          CALL SimulateWaterCoilComponents(CompName,FirstHVACIteration, &
                                           UnitarySystem(UnitarySysNum)%HeatingCoilIndex,  &
                                           QActual,UnitarySystem(UnitarySysNum)%FanOpMode, &
                                           PartLoadRatio)

    CASE (Coil_HeatingSteam)
! this same CALL is made in the steam coil calc routine
          mdot = MIN(Node(UnitarySystem(UnitarySysNum)%HeatCoilFluidOutletNodeNum)%MassFlowRateMaxAvail, &
                 UnitarySystem(UnitarySysNum)%MaxHeatCoilFluidFlow * PartLoadRatio)
          Node(UnitarySystem(UnitarySysNum)%HeatCoilFluidInletNode)%MassFlowRate = mdot
          CALL SimulateSteamCoilComponents(CompName, FirstHVACIteration, &
                                           UnitarySystem(UnitarySysNum)%DesignHeatingCapacity*PartLoadRatio, &
                                           UnitarySystem(UnitarySysNum)%HeatingCoilIndex, &
                                           FanOpMode = UnitarySystem(UnitarySysNum)%FanOpMode, &
                                           PartLoadRatio = PartLoadRatio)

    CASE(Coil_HeatingAirToAirVariableSpeed, Coil_HeatingWaterToAirHPVSEquationFit)

        CALL SimVariableSpeedCoils(CompName,UnitarySystem(UnitarySysNum)%HeatingCoilIndex, &
             UnitarySystem(UnitarySysNum)%FanOpMode,UnitarySystem(UnitarySysNum)%MaxONOFFCyclesperHour, &
             UnitarySystem(UnitarySysNum)%HPTimeConstant,UnitarySystem(UnitarySysNum)%FanDelayTime, &
             CompOn, PartLoadRatio, OnOffAirFlowRatio, &
             UnitarySystem(UnitarySysNum)%HeatingSpeedNum, UnitarySystem(UnitarySysNum)%HeatingSpeedRatio, &
             UnitarySystem(UnitarySysNum)%HeatingCoilSensDemand, dummy)
        IF(UnitarySystem(UnitarySysNum)%HeatingSpeedNum .GT. 1.0d0)THEN
          UnitarySystem(UnitarySysNum)%HeatCompPartLoadRatio = 1.0d0
        ELSE
          UnitarySystem(UnitarySysNum)%HeatCompPartLoadRatio = PartLoadRatio*REAL(CompOn,r64)
        END IF
    CASE(Coil_HeatingWaterToAirHPSimple)

        IF(PartLoadRatio > 0.0d0 .AND. UnitarySystem(UnitarySysNum)%WSHPRuntimeFrac > 0.0d0 .AND. &
           UnitarySystem(UnitarySysNum)%FanOpMode == CycFanCycCoil)THEN
          OnOffFanPartLoadFraction = PartLoadRatio/UnitarySystem(UnitarySysNum)%WSHPRuntimeFrac
        END IF

        CALL SimWatertoAirHPSimple(Blank, UnitarySystem(UnitarySysNum)%HeatingCoilIndex, &
                                   UnitarySystem(UnitarySysNum)%HeatingCoilSensDemand, dummy, &
                                   UnitarySystem(UnitarySysNum)%FanOpMode,UnitarySystem(UnitarySysNum)%WSHPRuntimeFrac, &
                                   UnitarySystem(UnitarySysNum)%MaxONOFFCyclesperHour, &
                                   UnitarySystem(UnitarySysNum)%HPTimeConstant, UnitarySystem(UnitarySysNum)%FanDelayTime, &
                                   CompOn, PartLoadRatio, FirstHVACIteration)
        UnitarySystem(UnitarySysNum)%HeatCompPartLoadRatio = PartLoadRatio*REAL(CompOn,r64)

    CASE(Coil_HeatingWaterToAirHP)

        CALL HeatPumpRunFrac(UnitarySysNum,PartLoadRatio,errflag,UnitarySystem(UnitarySysNum)%WSHPRuntimeFrac)

        IF(PartLoadRatio > 0.0d0 .AND. UnitarySystem(UnitarySysNum)%WSHPRuntimeFrac > 0.0d0 .AND. &
           UnitarySystem(UnitarySysNum)%FanOpMode == CycFanCycCoil)THEN
          OnOffFanPartLoadFraction = PartLoadRatio/UnitarySystem(UnitarySysNum)%WSHPRuntimeFrac
        END IF

        CALL SimWatertoAirHP(Blank, UnitarySystem(UnitarySysNum)%HeatingCoilIndex, &
              UnitarySystem(UnitarySysNum)%MaxHeatAirMassFlow,UnitarySystem(UnitarySysNum)%FanOpMode,&
              FirstHVACIteration,UnitarySystem(UnitarySysNum)%WSHPRuntimeFrac,&
              UnitarySystem(UnitarySysNum)%MaxONOFFCyclesperHour, &
              UnitarySystem(UnitarySysNum)%HPTimeConstant, &
              UnitarySystem(UnitarySysNum)%FanDelayTime, &
              UnitarySystem(UnitarySysNum)%InitHeatPump, &
              UnitarySystem(UnitarySysNum)%HeatingCoilSensDemand, &
              dummy,CompOn, PartLoadRatio)
        UnitarySystem(UnitarySysNum)%HeatCompPartLoadRatio = PartLoadRatio*REAL(CompOn,r64)

    CASE DEFAULT
          CALL ShowFatalError('CalcUnitaryHeatingSystem: Invalid Unitary System coil type = '//  &
                              TRIM(cAllCoilTypes(UnitarySystem(UnitarySysNum)%HeatingCoilType_Num)))

  END SELECT

  UnitarySystem(UnitarySysNum)%HeatingPartLoadFrac = PartLoadRatio

  RETURN

END SUBROUTINE CalcUnitaryHeatingSystem

SUBROUTINE CalcUnitarySuppHeatingSystem(UnitarySysNum, FirstHVACIteration, PartLoadRatio, SuppCoilLoad)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Raustad, FSEC
          !       DATE WRITTEN   February 2013
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine manages supplemental heater simulation.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE HeatingCoils,     ONLY: SimulateHeatingCoilComponents
  USE WaterCoils,       ONLY: SimulateWaterCoilComponents
  USE SteamCoils,       ONLY: SimulateSteamCoilComponents
  USE General,          ONLY: SolveRegulaFalsi

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)          :: UnitarySysNum    ! Index of AirloopHVAC:UnitarySystem object
  LOGICAL, INTENT(IN)          :: FirstHVACIteration  ! True when first HVAC iteration
  REAL(R64), INTENT(IN)        :: PartLoadRatio       ! coil operating part-load ratio
  REAL(r64), OPTIONAL, INTENT(IN) :: SuppCoilLoad     ! adjusted supp coil load when outlet temp exceeds max (W)

          ! SUBROUTINE PARAMETER DEFINITIONS:
  INTEGER, PARAMETER   :: MaxIte    = 500     ! Maximum number of iterations for solver
  REAL(r64), PARAMETER :: Acc       = 1.d-3   ! Accuracy of solver result

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  CHARACTER(len=MaxNameLength)  :: CompName              ! Name of Unitary System object
  REAL(r64)                     :: SuppHeatCoilLoad      ! load passed to supplemental heating coil (W)
  REAL(r64)                     :: QActual               ! actual coil output (W)
  REAL(r64)                     :: mdot                  ! water coil water mass flow rate (kg/s)
  REAL(r64), DIMENSION(5)       :: Par                   ! Parameter array passed to solver
  INTEGER                       :: SolFla                ! Flag of solver, num iterations if >0, else error index
  REAL(r64)                     :: PartLoadFrac          ! temporary PLR variable


! work is needed to figure out how to adjust other coil types if outlet temp exceeds maximum
! this works for gas and electric heating coils
  CompName = UnitarySystem(UnitarySysNum)%SuppHeatCoilName
  IF(OutDryBulbTemp .LE. UnitarySystem(UnitarySysNum)%MaxOATSuppHeat .OR. &
     (MoistureLoad < 0.0d0 .AND. UnitarySystem(UnitarySysNum)%CoolingPartLoadFrac > 0.0d0))THEN
    IF(PRESENT(SuppCoilLoad))THEN
      SuppHeatCoilLoad = SuppCoilLoad
    ELSE
      SuppHeatCoilLoad = UnitarySystem(UnitarySysNum)%DesignSuppHeatingCapacity*PartLoadRatio
    END IF
  ELSE
    SuppHeatCoilLoad = 0.0d0
  END IF

  SELECT CASE(UnitarySystem(UnitarySysNum)%SuppHeatCoilType_Num)

    CASE (Coil_HeatingGas,Coil_HeatingElectric)
      SELECT CASE(UnitarySystem(UnitarySysNum)%ControlType)
        CASE(SetPointBased)
          CALL SimulateHeatingCoilComponents(CompName,FirstHVACIteration, &
                                CompIndex=UnitarySystem(UnitarySysNum)%SuppHeatCoilIndex, &
                                SuppHeat=.TRUE., &
                                FanOpMode=UnitarySystem(UnitarySysNum)%FanOpMode, &
                                PartLoadRatio=PartLoadRatio)
        CASE DEFAULT
          CALL SimulateHeatingCoilComponents(CompName,FirstHVACIteration, &
                                CompIndex=UnitarySystem(UnitarySysNum)%SuppHeatCoilIndex, &
                                SuppHeat=.TRUE., &
                                QCoilReq=SuppHeatCoilLoad, &
                                FanOpMode=UnitarySystem(UnitarySysNum)%FanOpMode, &
                                PartLoadRatio=PartLoadRatio)
      END SELECT
    CASE (Coil_HeatingDesuperheater)
        CALL SimulateHeatingCoilComponents(CompName,FirstHVACIteration, &
                                CompIndex=UnitarySystem(UnitarySysNum)%SuppHeatCoilIndex, &
                                SuppHeat=.TRUE., &
                                QCoilReq=SuppHeatCoilLoad, &
                                FanOpMode=UnitarySystem(UnitarySysNum)%FanOpMode, &
                                PartLoadRatio=PartLoadRatio)

    CASE (Coil_HeatingWater)
      IF(PRESENT(SuppCoilLoad))THEN
        IF(SuppHeatCoilLoad > 0.0d0)THEN
          ! see if HW coil has enough capacity to meet the load
          mdot = MIN(Node(UnitarySystem(UnitarySysNum)%SuppCoilFluidOutletNodeNum)%MassFlowRateMaxAvail, &
                 UnitarySystem(UnitarySysNum)%MaxSuppCoilFluidFlow)
          Node(UnitarySystem(UnitarySysNum)%SuppCoilFluidInletNode)%MassFlowRate = mdot
          !     simulate water coil to find operating capacity
          CALL SimulateWaterCoilComponents(UnitarySystem(UnitarySysNum)%SuppHeatCoilName,FirstHVACIteration, &
                                           UnitarySystem(UnitarySysNum)%SuppHeatCoilIndex, QActual, &
                                           FanOpMode=UnitarySystem(UnitarySysNum)%FanOpMode, &
                                           PartLoadRatio=PartLoadRatio)
          IF(QActual > SuppHeatCoilLoad)THEN
            Par(1) = REAL(UnitarySysNum,r64)
            IF (FirstHVACIteration) THEN
              Par(2) = 1.0d0
            ELSE
              Par(2) = 0.0d0
            END IF
            Par(3) = SuppHeatCoilLoad
            Par(4) = 1.0d0 ! SuppHeatingCoilFlag
            Par(5) = 1.0d0 ! Load based control
            CALL SolveRegulaFalsi(Acc, MaxIte, SolFla, PartLoadFrac, HotWaterHeatingCoilResidual, 0.0d0,   &
                                        1.0d0, Par)
            UnitarySystem(UnitarySysNum)%SuppHeatPartLoadFrac = PartLoadFrac
          ELSE
            UnitarySystem(UnitarySysNum)%SuppHeatPartLoadFrac = 1.0d0
          END IF
        END IF
      ELSE
        mdot = MIN(Node(UnitarySystem(UnitarySysNum)%SuppCoilFluidOutletNodeNum)%MassFlowRateMaxAvail, &
                 UnitarySystem(UnitarySysNum)%MaxSuppCoilFluidFlow * PartLoadRatio)
        Node(UnitarySystem(UnitarySysNum)%SuppCoilFluidInletNode)%MassFlowRate = mdot

        CALL SimulateWaterCoilComponents(CompName,FirstHVACIteration, &
                                           UnitarySystem(UnitarySysNum)%SuppHeatCoilIndex,  &
                                           QActual, UnitarySystem(UnitarySysNum)%FanOpMode, &
                                           PartLoadRatio)
      END IF
    CASE (Coil_HeatingSteam)
        mdot = MIN(Node(UnitarySystem(UnitarySysNum)%SuppCoilFluidOutletNodeNum)%MassFlowRateMaxAvail, &
                 UnitarySystem(UnitarySysNum)%MaxSuppCoilFluidFlow * PartLoadRatio)
        Node(UnitarySystem(UnitarySysNum)%SuppCoilFluidInletNode)%MassFlowRate = mdot
        CALL SimulateSteamCoilComponents(CompName, FirstHVACIteration, &
                                           SuppHeatCoilLoad, &
                                           UnitarySystem(UnitarySysNum)%SuppHeatCoilIndex, &
                                           FanOpMode = UnitarySystem(UnitarySysNum)%FanOpMode, &
                                           PartLoadRatio = PartLoadRatio)

    CASE DEFAULT

  END SELECT

!  UnitarySystem(UnitarySysNum)%SuppHeatPartLoadFrac = PartLoadRatio

  RETURN

END SUBROUTINE CalcUnitarySuppHeatingSystem

SUBROUTINE CalcUnitarySuppSystemtoSP(UnitarySysNum, FirstHVACIteration)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Raustad, FSEC
          !       DATE WRITTEN   February 2013
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine manages supplemental heater component simulation for setpoint based operation scheme.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE HeatingCoils,    ONLY: SimulateHeatingCoilComponents
  USE WaterCoils,      ONLY: SimulateWaterCoilComponents
  USE SteamCoils,      ONLY: SimulateSteamCoilComponents

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)          :: UnitarySysNum    ! Index of AirloopHVAC:UnitarySystem object
  LOGICAL, INTENT(IN)          :: FirstHVACIteration  ! True when first HVAC iteration

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  CHARACTER(len=MaxNameLength)  :: CompName              ! Name of Unitary System object
  REAL(r64)                     :: QActual

  CompName = UnitarySystem(UnitarySysNum)%SuppHeatCoilName

  SELECT CASE(UnitarySystem(UnitarySysNum)%SuppHeatCoilType_Num)

    CASE (Coil_HeatingGas,Coil_HeatingElectric)
      CALL SimulateHeatingCoilComponents(CompName,FirstHVACIteration, &
                                CompIndex=UnitarySystem(UnitarySysNum)%SuppHeatCoilIndex, &
!                                QCoilReq=(UnitarySystem(UnitarySysNum)%DesignSuppHeatingCapacity* &
!                                          UnitarySystem(UnitarySysNum)%SuppHeatPartLoadFrac), &
                                FanOpMode=UnitarySystem(UnitarySysNum)%FanOpMode, &
                                PartLoadRatio=UnitarySystem(UnitarySysNum)%SuppHeatPartLoadFrac)

    CASE (Coil_HeatingDesuperheater )
      CALL SimulateHeatingCoilComponents(CompName,FirstHVACIteration, &
                                CompIndex=UnitarySystem(UnitarySysNum)%SuppHeatCoilIndex, &
                                FanOpMode=UnitarySystem(UnitarySysNum)%FanOpMode, &
                                PartLoadRatio=UnitarySystem(UnitarySysNum)%SuppHeatPartLoadFrac)

    CASE (Coil_HeatingWater)
      CALL SimulateWaterCoilComponents(CompName,FirstHVACIteration, &
                                       UnitarySystem(UnitarySysNum)%SuppHeatCoilIndex,  &
                                       QActual, UnitarySystem(UnitarySysNum)%FanOpMode, &
                                       UnitarySystem(UnitarySysNum)%SuppHeatPartLoadFrac)

    CASE (Coil_HeatingSteam)
      CALL SimulateSteamCoilComponents(CompName, FirstHVACIteration, &
                                       QCoilReq=(UnitarySystem(UnitarySysNum)%DesignSuppHeatingCapacity* &
                                       UnitarySystem(UnitarySysNum)%SuppHeatPartLoadFrac), &
                                       CompIndex = UnitarySystem(UnitarySysNum)%SuppHeatCoilIndex, &
                                       FanOpMode = UnitarySystem(UnitarySysNum)%FanOpMode, &
                                       PartLoadRatio = UnitarySystem(UnitarySysNum)%SuppHeatPartLoadFrac)

    CASE DEFAULT

  END SELECT

  RETURN

END SUBROUTINE CalcUnitarySuppSystemtoSP

SUBROUTINE ControlCoolingSystem(UnitarySysNum, FirstHVACIteration, HXUnitOn)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Raustad, FSEC
          !       DATE WRITTEN   February 2013
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !  Simulate the coil object at the required PLR.

          ! METHODOLOGY EMPLOYED:
          !  Calculate operating PLR and adjust speed when using multispeed coils.
          !  Meet moisture load if required to do so.

          ! REFERENCES:
          !  na

          ! USE STATEMENTS:
  USE DataAirLoop,     ONLY: LoopDXCoilRTF
  USE Psychrometrics , ONLY: PsyHFnTdbW, PsyTdpFnWPb
  USE General,         ONLY: SolveRegulaFalsi, RoundSigDigits
  USE DXCoils,         ONLY: SimDXCoil, SimDXCoilMultiSpeed, DXCoilOutletTemp, SimDXCoilMultiMode, DXCoilOutletHumRat
  USE HVACHXAssistedCoolingCoil, ONLY: SimHXAssistedCoolingCoil, HXAssistedCoilOutletTemp, HXAssistedCoilOutletHumRat
  USE WaterCoils,      ONLY: SimulateWaterCoilComponents
  USE PlantUtilities,  ONLY: SetComponentFlowRate
  USE WatertoAirHeatPumpSimple,  ONLY: SimWatertoAirHPSimple
  USE WatertoAirHeatPump,        ONLY: SimWaterToAirHP
  USE VariableSpeedCoils,        ONLY: SimVariableSpeedCoils, VarSpeedCoil

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER,  Intent(In)    :: UnitarySysNum           ! index to Unitary System
  LOGICAL,  Intent(In)    :: FirstHVACIteration      ! First HVAC iteration flag
  LOGICAL,  Intent(InOut) :: HXUnitOn                ! flag to enable heat exchanger heat recovery

          ! SUBROUTINE PARAMETER DEFINITIONS:
  INTEGER, PARAMETER   :: MaxIte    = 500     ! Maximum number of iterations for solver
  REAL(r64), PARAMETER :: Acc       = 1.d-3   ! Accuracy of solver result
  REAL(r64), PARAMETER :: HumRatAcc = 1.d-6   ! Accuracy of solver result

          ! INTERFACE BLOCK SPECIFICATIONS
          !  na

          ! DERIVED TYPE DEFINITIONS
          !  na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  CHARACTER(len=MaxNameLength)  :: CompName  ! Name of the DX cooling coil
!  REAL(r64)           :: NoOutput            ! Sensible capacity (outlet - inlet) when the compressor is off
  REAL(r64)           :: FullOutput          ! Sensible capacity (outlet - inlet) when the compressor is on
  REAL(r64)           :: ReqOutput           ! Sensible capacity (outlet - inlet) required to meet load or setpoint temperature
  Integer             :: InletNode           ! Inlet node number of the DX cooling coil
  Integer             :: OutletNode          ! Outlet node number of the DX cooling coil
  Integer             :: ControlNode         ! The node number where a setpoint is placed to control the DX cooling coil
  REAL(r64)           :: PartLoadFrac        ! The part-load fraction of the compressor
  REAL(r64)           :: SpeedRatio          ! SpeedRatio = (CompressorSpeed - CompressorSpeedMin) /
                                             !              (CompressorSpeedMax - CompressorSpeedMin)
                                             ! for variable speed or 2 speed compressors
  REAL(r64)           :: CycRatio            ! Cycling part-load ratio for variable speed or 2 speed compressors
  REAL(r64)           :: DesOutTemp          ! Desired outlet temperature of the DX cooling coil
  REAL(r64)           :: DesOutHumRat        ! Desired outlet humidity ratio of the DX cooling coil
  REAL(r64)           :: OutletTempDXCoil    ! Actual outlet temperature of the DX cooling coil
  REAL(r64)           :: OutletHumRatLS      ! Actual outlet humrat of the variable speed DX cooling coil at low speed
  REAL(r64)           :: OutletHumRatHS      ! Actual outlet humrat of the variable speed DX cooling coil at high speed
  REAL(r64)           :: OutletHumRatDXCoil  ! Actual outlet humidity ratio of the DX cooling coil
  INTEGER             :: SolFla              ! Flag of solver, num iterations if >0, else error index
  INTEGER             :: SolFlaLat           ! Flag of solver for dehumid calculations
  REAL(r64), DIMENSION(8)  :: Par                 ! Parameter array passed to solver
  LOGICAL             :: SensibleLoad        ! True if there is a sensible cooling load on this system
  LOGICAL             :: LatentLoad          ! True if there is a latent   cooling load on this system
  INTEGER             :: DehumidMode         ! dehumidification mode (0=normal, 1=enhanced)
  INTEGER             :: FanOpMode           ! Supply air fan operating mode
  REAL(r64)           :: TempMinPLR          ! Used to find latent PLR when max iterations exceeded
  REAL(r64)           :: TempMaxPLR          ! Used to find latent PLR when max iterations exceeded
  REAL(r64)           :: TempOutletTempDXCoil   ! Used to find latent PLR when max iterations exceeded
  REAL(r64)           :: TempOutletHumRatDXCoil ! Used to find latent PLR when max iterations exceeded
  REAL(r64)           :: NoLoadHumRatOut     ! DX coil outlet air humidity ratio with comprssor off
  REAL(r64)           :: FullLoadHumRatOut   ! DX coil outlet air humidity ratio with comprssor full on
  REAL(r64)           :: WSHPRuntimeFrac            ! Run time fraction of water to air hp
  REAL(r64)           :: dummy               ! dummy variable for heating latent demand
  REAL(R64)           :: OnOffAirFlowRatio
  REAL(R64)           :: OutletTemp
  INTEGER             :: SpeedNum
  REAL(R64)           :: LoopDXCoilMaxRTFSave ! Used to find RTF of DX heating coils without overwriting globabl variable
  REAL(r64)           :: NoLoadTempOut       ! saves coil off outlet temp

  ! Set local variables
  ! Retrieve the load on the controlled zone
  OutletNode   = UnitarySystem(UnitarySysNum)%CoolCoilOutletNodeNum
  InletNode    = UnitarySystem(UnitarySysNum)%CoolCoilInletNodeNum
  ControlNode  = UnitarySystem(UnitarySysNum)%SystemCoolControlNodeNum
  DesOutTemp   = UnitarySystem(UnitarySysNum)%DesiredOutletTemp
  DesOutHumRat = UnitarySystem(UnitarySysNum)%DesiredOutletHumRat
  LoopDXCoilMaxRTFSave=LoopDXCoilRTF
  LoopDXCoilRTF=0.0d0

  CompName     = UnitarySystem(UnitarySysNum)%CoolingCoilName
  FanOpMode    = UnitarySystem(UnitarySysNum)%FanOpMode
  SpeedRatio   = 0.0d0
  CycRatio     = 0.0d0
  PartLoadFrac = 0.0d0
  DehumidMode  = 0
  SensibleLoad = .FALSE.
  LatentLoad   = .FALSE.
  WSHPRuntimeFrac = 0.0d0
  Dummy = 0.0d0
  SolFla = 0.0d0
  SolFlaLat = 0.0d0
  NoLoadTempOut = 0.0d0
  NoLoadHumRatOut = 0.0d0

! Check the dehumidification control type. IF it's multimode, turn off the HX to find the sensible PLR. Then check to
! see if the humidity load is met without the use of the HX. Always run the HX for the other modes.
  IF (UnitarySystem(UnitarySysNum)%DehumidControlType_Num .NE. DehumidControl_Multimode)THEN
    HXUnitOn = .TRUE.
  ELSE
    HXUnitOn = .FALSE.
  END IF

  ! IF DXCoolingSystem is scheduled on and there is flow
  IF((GetCurrentScheduleValue(UnitarySystem(UnitarySysNum)%SysAvailSchedPtr) .gt. 0.0d0) .and. &
      GetCurrentScheduleValue(UnitarySystem(UnitarySysNum)%CoolingCoilAvailSchPtr) > 0.0d0 .AND. &
     (Node(InletNode)%MassFlowRate .gt. MinAirMassFlow) .AND. UnitarySystem(UnitarySysNum)%HeatingPartLoadFrac == 0.0d0) THEN

    ! Determine if there is a sensible load on this system (aren't the first 2 tests redundant?)
    IF((Node(InletNode)%Temp > DesOutTemp) .and. &
       (ABS(Node(InletNode)%Temp - DesOutTemp) .gt. TempControlTol) ) SensibleLoad = .TRUE.

    ! Determine if there is a latent load on this system - for future use to serve latent-only loads
    IF(Node(InletNode)%HumRat > DesOutHumRat) LatentLoad = .TRUE.

    ! disable latent dehumidification if there is no sensible load and latent only is not allowed
    IF(UnitarySystem(UnitarySysNum)%RunOnLatentOnlyWithSensible .AND. .NOT. SensibleLoad)LatentLoad = .FALSE.

    ! IF DXCoolingSystem runs with a cooling load then set PartLoadFrac on Cooling System and the Mass Flow
    ! Multimode coil will switch to enhanced dehumidification IF available and needed, but it
    ! still runs to meet the sensible load. Multimode applies to Multimode or HXAssistedCooling coils.
    IF ((SensibleLoad .and. UnitarySystem(UnitarySysNum)%RunOnSensibleLoad) .OR. &
        (LatentLoad .and. UnitarySystem(UnitarySysNum)%RunOnLatentLoad)) THEN
      ! calculate sensible PLR, don't care IF latent is true here but need to gaurd for
      ! when LatentLoad=TRUE and SensibleLoad=FALSE
      ReqOutput = Node(InletNode)%MassFlowRate *  &
                    (PsyHFnTdbW(DesOutTemp,Node(OutletNode)%HumRat) - &
                     PsyHFnTdbW(Node(InletNode)%Temp,Node(OutletNode)%HumRat))

      PartLoadFrac = 0.0d0

      SELECT CASE(UnitarySystem(UnitarySysNum)%CoolingCoilType_Num)

        CASE (CoilDX_CoolingSingleSpeed)  ! COIL:DX:COOLINGBYPASSFACTOREMPIRICAL
          UnitarySystem(UnitarySysNum)%CompPartLoadRatio = PartLoadFrac

          CALL SimDXCoil(CompName,On,FirstHVACIteration,PartLoadFrac,UnitarySystem(UnitarySysNum)%CoolingCoilIndex,FanOpMode)

        CASE (CoilDX_CoolingHXAssisted, CoilWater_CoolingHXAssisted)  ! CoilSystem:Cooling:DX:HeatExchangerAssisted

          IF(UnitarySystem(UnitarySysNum)%CoolCoilFluidInletNode .GT. 0) &
               Node(UnitarySystem(UnitarySysNum)%CoolCoilFluidInletNode)%MassFlowRate = 0.0d0

          CALL SimHXAssistedCoolingCoil(CompName,FirstHVACIteration,On,PartLoadFrac, &
                                        UnitarySystem(UnitarySysNum)%CoolingCoilIndex, FanOpMode, &
                                        HXUnitEnable=HXUnitOn,EconomizerFlag=EconomizerFlag)
          IF(UnitarySystem(UnitarySysNum)%CoolingCoilType_Num==CoilDX_CoolingHXAssisted) &
               UnitarySystem(UnitarySysNum)%CompPartLoadRatio = PartLoadFrac
        CASE (CoilDX_CoolingTwoSpeed)

          CALL SimDXCoilMultiSpeed(CompName,0.0d0,PartLoadFrac,UnitarySystem(UnitarySysNum)%CoolingCoilIndex)

        CASE (CoilDX_MultiSpeedCooling, Coil_CoolingAirToAirVariableSpeed, Coil_CoolingWaterToAirHPVSEquationFit)

          CALL SimMultiSpeedCoils(UnitarySysNum, FirstHVACIteration, SensibleLoad, LatentLoad, PartLoadFrac, &
                                  CoolingCoil)

        CASE (CoilDX_CoolingTwoStageWHumControl) ! Coil:Cooling:DX:TwoStageWithHumidityControlMode

          CALL SimDXCoilMultiMode(CompName,On,FirstHVACIteration,PartLoadFrac,DehumidMode,  &
                                  UnitarySystem(UnitarySysNum)%CoolingCoilIndex,FanOpMode)
          UnitarySystem(UnitarySysNum)%CompPartLoadRatio = PartLoadFrac
        CASE (Coil_CoolingWater, Coil_CoolingWaterDetailed)  ! COIL:COOLING:WATER

          CALL SimWaterCoils(UnitarySysNum, FirstHVACIteration, PartLoadFrac, CoolingCoil)

        CASE(Coil_CoolingWaterToAirHPSimple)

          CALL SimWatertoAirHPSimple(Blank, UnitarySystem(UnitarySysNum)%CoolingCoilIndex, ReqOutput, Dummy, &
                                       FanOpMode,WSHPRuntimeFrac, UnitarySystem(UnitarySysNum)%MaxONOFFCyclesperHour, &
                                       UnitarySystem(UnitarySysNum)%HPTimeConstant, UnitarySystem(UnitarySysNum)%FanDelayTime, &
                                       0, PartLoadFrac, FirstHVACIteration)

        CASE(Coil_CoolingWaterToAirHP)

          CALL SimWatertoAirHP(Blank, UnitarySystem(UnitarySysNum)%CoolingCoilIndex, &
                                 UnitarySystem(UnitarySysNum)%MaxCoolAirMassFlow,FanOpMode, &
                                 FirstHVACIteration,WSHPRuntimeFrac,UnitarySystem(UnitarySysNum)%MaxONOFFCyclesperHour, &
                                 UnitarySystem(UnitarySysNum)%HPTimeConstant, UnitarySystem(UnitarySysNum)%FanDelayTime, &
                                 UnitarySystem(UnitarySysNum)%InitHeatPump, ReqOutput,Dummy,0, PartLoadFrac)
        CASE DEFAULT


      END SELECT

!      NoOutput = Node(InletNode)%MassFlowRate *  &
!                   (PsyHFnTdbW(Node(OutletNode)%Temp,Node(OutletNode)%HumRat)  &
!                    - PsyHFnTdbW(Node(InletNode)%Temp,Node(OutletNode)%HumRat))
      NoLoadTempOut = Node(OutletNode)%Temp
      NoLoadHumRatOut = Node(OutletNode)%HumRat

!     Changed logic to use temperature instead of load. The Psyc calcs can cause slight errors.
!     For example it's possible that (NoOutput-ReqOutput) > Acc while (Node(OutletNode)%Temp-DesOutTemp) is not
!     This can (and did) lead to RegulaFalsi errors

!      IF ((NoOutput-ReqOutput) .LT. Acc) THEN
!     IF outlet temp at no load is lower than DesOutTemp (set point), do not operate the coil
      IF ((NoLoadTempOut-DesOutTemp) .LT. Acc) THEN
        PartLoadFrac = 0.0d0
      ELSE IF(SensibleLoad)THEN ! need to turn on compressor to see if load is met
        PartLoadFrac = 1.0d0
        WSHPRuntimeFrac = 1.0d0
        SELECT CASE(UnitarySystem(UnitarySysNum)%CoolingCoilType_Num)

          CASE (CoilDX_CoolingSingleSpeed)  ! COIL:DX:COOLINGBYPASSFACTOREMPIRICAL

            CALL SimDXCoil(CompName,On,FirstHVACIteration,PartLoadFrac,UnitarySystem(UnitarySysNum)%CoolingCoilIndex,FanOpMode)
            UnitarySystem(UnitarySysNum)%CompPartLoadRatio = PartLoadFrac

          CASE (CoilDX_CoolingHXAssisted,CoilWater_CoolingHXAssisted)  ! CoilSystem:Cooling:DX:HeatExchangerAssisted

            IF(UnitarySystem(UnitarySysNum)%CoolCoilFluidInletNode .GT. 0) &
                 Node(UnitarySystem(UnitarySysNum)%CoolCoilFluidInletNode)%MassFlowRate = &
                    MAX(0.0d0,UnitarySystem(UnitarySysNum)%MaxCoolCoilFluidFlow)
            CALL SimHXAssistedCoolingCoil(CompName,FirstHVACIteration,On,PartLoadFrac, &
                                          UnitarySystem(UnitarySysNum)%CoolingCoilIndex, FanOpMode, &
                                          HXUnitEnable=HXUnitOn,EconomizerFlag=EconomizerFlag)

            IF(UnitarySystem(UnitarySysNum)%CoolingCoilType_Num==CoilDX_CoolingHXAssisted) &
                 UnitarySystem(UnitarySysNum)%CompPartLoadRatio = PartLoadFrac

          CASE (CoilDX_CoolingTwoSpeed)

            CycRatio = 1.0d0
            DO SpeedNum = 1, UnitarySystem(UnitarySysNum)%NumOfSpeedCooling
              SpeedRatio = REAL(SpeedNum,r64) - 1.0d0
              CALL SimDXCoilMultiSpeed(CompName,SpeedRatio,CycRatio,UnitarySystem(UnitarySysNum)%CoolingCoilIndex)
              OutletTemp = DXCoilOutletTemp(UnitarySystem(UnitarySysNum)%CoolingCoilIndex)
              IF (OutletTemp < DesOutTemp .AND. SensibleLoad) EXIT ! this isn't going to work IF dehumidIFying
            END DO

          CASE (CoilDX_MultiSpeedCooling, Coil_CoolingAirToAirVariableSpeed, Coil_CoolingWaterToAirHPVSEquationFit)

            CycRatio = 1.0d0
            DO SpeedNum = 1, UnitarySystem(UnitarySysNum)%NumOfSpeedCooling
              CALL SimMultiSpeedCoils(UnitarySysNum, FirstHVACIteration, SensibleLoad, LatentLoad, PartLoadFrac, &
                                      CoolingCoil, SpeedNum)
              OutletTemp = Node(OutletNode)%Temp
              SpeedRatio = REAL(SpeedNum,r64) - 1.0d0
              IF (OutletTemp < DesOutTemp .AND. SensibleLoad) EXIT
            END DO

          CASE (CoilDX_CoolingTwoStageWHumControl) ! Coil:Cooling:DX:TwoStageWithHumidityControlMode

            CALL SimDXCoilMultiMode(CompName,On,FirstHVACIteration,PartLoadFrac,DehumidMode,  &
                                    UnitarySystem(UnitarySysNum)%CoolingCoilIndex,FanOpMode)
            UnitarySystem(UnitarySysNum)%CompPartLoadRatio = PartLoadFrac

          CASE (Coil_CoolingWater, Coil_CoolingWaterDetailed)  ! COIL:COOLING:WATER

            CALL SimWaterCoils(UnitarySysNum, FirstHVACIteration, PartLoadFrac, CoolingCoil)

          CASE(Coil_CoolingWaterToAirHPSimple)

            CALL SimWatertoAirHPSimple(Blank, UnitarySystem(UnitarySysNum)%CoolingCoilIndex, ReqOutput, Dummy, &
                                       FanOpMode,WSHPRuntimeFrac, UnitarySystem(UnitarySysNum)%MaxONOFFCyclesperHour, &
                                       UnitarySystem(UnitarySysNum)%HPTimeConstant, UnitarySystem(UnitarySysNum)%FanDelayTime, &
                                       0, PartLoadFrac, FirstHVACIteration)

          CASE(Coil_CoolingWaterToAirHP)

            CALL SimWatertoAirHP(Blank, UnitarySystem(UnitarySysNum)%CoolingCoilIndex, &
                                   UnitarySystem(UnitarySysNum)%MaxCoolAirMassFlow,FanOpMode, &
                                   FirstHVACIteration,WSHPRuntimeFrac,UnitarySystem(UnitarySysNum)%MaxONOFFCyclesperHour, &
                                   UnitarySystem(UnitarySysNum)%HPTimeConstant, UnitarySystem(UnitarySysNum)%FanDelayTime, &
                                   UnitarySystem(UnitarySysNum)%InitHeatPump, ReqOutput,Dummy,0, PartLoadFrac)
          CASE DEFAULT


        END SELECT

        FullOutput = Node(InletNode)%MassFlowRate *  &
                   (PsyHFnTdbW(Node(OutletNode)%Temp,Node(OutletNode)%HumRat)  &
                    - PsyHFnTdbW(Node(InletNode)%Temp,Node(OutletNode)%HumRat))

        FullLoadHumRatOut = Node(OutletNode)%HumRat


!        IF ((FullOutput - ReqOutput) .GT. Acc) THEN ! old method
!        IF ((Node(OutletNode)%Temp-DesOutTemp) .GT. Acc) THEN ! new method gets caught when temps are very close
        IF (Node(OutletNode)%Temp .GT. DesOutTemp-Acc) THEN
          PartLoadFrac = 1.0d0

        ELSE
          SELECT CASE(UnitarySystem(UnitarySysNum)%CoolingCoilType_Num)

            CASE (CoilDX_CoolingSingleSpeed)  ! COIL:DX:COOLINGBYPASSFACTOREMPIRICAL

              Par(1) = REAL(UnitarySystem(UnitarySysNum)%CoolingCoilIndex,r64)
              Par(2) = DesOutTemp
              Par(5) = REAL(FanOpMode,r64)
              CALL SolveRegulaFalsi(Acc, MaxIte, SolFla, PartLoadFrac, DOE2DXCoilResidual, 0.0d0,   &
                                    1.0d0, Par)
              UnitarySystem(UnitarySysNum)%CompPartLoadRatio = PartLoadFrac

            CASE (CoilDX_CoolingHXAssisted,CoilWater_CoolingHXAssisted)  ! CoilSystem:Cooling:DX:HeatExchangerAssisted

                Par(1) = REAL(UnitarySystem(UnitarySysNum)%CoolingCoilIndex,r64)
                Par(2) = DesOutTemp
              ! FirstHVACIteration is a logical, Par is REAL(r64), so make TRUE = 1 and FALSE = 0
                IF(FirstHVACIteration)THEN
                  Par(3) = 1.0d0
                ELSE
                  Par(3) = 0.0d0
                END IF
                IF(HXUnitOn)THEN
                  Par(4) = 1.0d0
                ELSE
                  Par(4) = 0.0d0
                END IF
                Par(5) = REAL(FanOpMode,r64)
                Par(6) = REAL(UnitarySysNum,r64)
                CALL SolveRegulaFalsi(Acc, MaxIte, SolFla, PartLoadFrac, HXAssistedCoolCoilTempResidual, 0.0d0,   &
                                      1.0d0, Par)
                IF (SolFla == -1) THEN

!                 RegulaFalsi may not find sensible PLR when the latent degradation model is used.
!                 IF iteration limit is exceeded, find tighter boundary of solution and repeat RegulaFalsi
                  TempMaxPLR = -0.1d0
                  TempOutletTempDXCoil = Node(InletNode)%Temp
                  DO WHILE((TempOutletTempDXCoil-DesOutTemp) .GT. 0.0d0 .AND. TempMaxPLR .LE. 1.0d0)
!                   find upper limit of PLR
                    TempMaxPLR = TempMaxPLR + 0.1d0
                    CALL SimHXAssistedCoolingCoil(CompName,FirstHVACIteration,On,TempMaxPLR, &
                                                  UnitarySystem(UnitarySysNum)%CoolingCoilIndex, FanOpMOde, &
                                                  HXUnitEnable=HXUnitOn, EconomizerFlag=EconomizerFlag)
                    TempOutletTempDXCoil = HXAssistedCoilOutletTemp(UnitarySystem(UnitarySysNum)%CoolingCoilIndex)
                  END DO
                  TempMinPLR = TempMaxPLR
                  DO WHILE((TempOutletTempDXCoil-DesOutTemp) .LT. 0.0d0 .AND. TempMinPLR .GE. 0.0d0)
!                  pull upper limit of PLR DOwn to last valid limit (i.e. outlet temp still exceeds DesOutTemp)
                    TempMaxPLR = TempMinPLR
!                   find minimum limit of PLR
                    TempMinPLR = TempMinPLR - 0.01d0
                    CALL SimHXAssistedCoolingCoil(CompName,FirstHVACIteration,On,TempMinPLR, &
                                                  UnitarySystem(UnitarySysNum)%CoolingCoilIndex, FanOpMode, &
                                                  HXUnitEnable=HXUnitOn, EconomizerFlag=EconomizerFlag)
                    TempOutletTempDXCoil = HXAssistedCoilOutletTemp(UnitarySystem(UnitarySysNum)%CoolingCoilIndex)
                  END DO
!                 Relax boundary slightly to assure a solution can be found using RegulaFalsi (i.e. one boundary may be
!                 very near the desired result)
                  TempMinPLR = MAX(0.0d0,(TempMinPLR - 0.01d0))
                  TempMaxPLR = MIN(1.0d0,(TempMaxPLR + 0.01d0))
!                 tighter boundary of solution has been found, CALL RegulaFalsi a second time
                  CALL SolveRegulaFalsi(Acc,MaxIte,SolFla,PartLoadFrac,HXAssistedCoolCoilTempResidual,TempMinPLR,TempMaxPLR,Par)
                  IF (SolFla == -1) THEN
                    IF(.NOT. WarmupFlag)THEN
                      IF(UnitarySystem(UnitarySysNum)%HXAssistedSensPLRIter .LT. 1)THEN
                        UnitarySystem(UnitarySysNum)%HXAssistedSensPLRIter = UnitarySystem(UnitarySysNum)%HXAssistedSensPLRIter+1
                        CALL ShowWarningError(TRIM(UnitarySystem(UnitarySysNum)%UnitarySystemType)//' - Iteration limit'// &
                                              ' exceeded calculating DX unit sensible part-load ratio for unit = '// &
                                              TRIM(UnitarySystem(UnitarySysNum)%Name))
                        CALL ShowContinueError('Estimated part-load ratio   = '//RoundSigDigits((ReqOutput/FullOutput),3))
                        CALL ShowContinueError('Calculated part-load ratio = '//RoundSigDigits(PartLoadFrac,3))
                        CALL ShowContinueErrorTimeStamp('The calculated part-load ratio will be used and the simulation'// &
                                                        ' continues. Occurrence info: ')
                      END IF
                      CALL ShowRecurringWarningErrorAtEnd(TRIM(UnitarySystem(UnitarySysNum)%UnitarySystemType)//' "'&
                          //TRIM(UnitarySystem(UnitarySysNum)%Name)//'" - Iteration limit exceeded calculating'// &
                          ' sensible part-load ratio error continues. Sensible PLR statistics follow.' &
                          ,UnitarySystem(UnitarySysNum)%HXAssistedSensPLRIterIndex,PartLoadFrac,PartLoadFrac)
                    END IF
                  ELSEIF (SolFla == -2) THEN
                    PartLoadFrac = ReqOutput/FullOutput
                    IF(.NOT. WarmupFlag)THEN
                      IF(UnitarySystem(UnitarySysNum)%HXAssistedSensPLRFail .LT. 1)THEN
                        UnitarySystem(UnitarySysNum)%HXAssistedSensPLRFail = UnitarySystem(UnitarySysNum)%HXAssistedSensPLRFail+1
                        CALL ShowWarningError(TRIM(UnitarySystem(UnitarySysNum)%UnitarySystemType)// &
                                            ' - DX unit sensible part-load ratio calculation unexpectedly failed: part-load '// &
                                            'ratio limits exceeded, for unit = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
                        CALL ShowContinueError('Estimated part-load ratio = '//RoundSigDigits(PartLoadFrac,3))
                        CALL ShowContinueErrorTimeStamp('The estimated part-load ratio will be used and the simulation'// &
                                                        ' continues. Occurrence info: ')
                      END IF
                      CALL ShowRecurringWarningErrorAtEnd(TRIM(UnitarySystem(UnitarySysNum)%UnitarySystemType)//' "'&
                          //TRIM(UnitarySystem(UnitarySysNum)%Name)//'" - DX unit sensible part-load ratio calculation'// &
                          ' unexpectedly failed error continues. Sensible PLR statistics follow.' &
                          ,UnitarySystem(UnitarySysNum)%HXAssistedSensPLRFailIndex,PartLoadFrac,PartLoadFrac)
                    END IF
                  END IF
                ELSEIF (SolFla == -2) THEN
                  PartLoadFrac = ReqOutput/FullOutput
                  IF(.NOT. WarmupFlag)THEN
                    IF(UnitarySystem(UnitarySysNum)%HXAssistedSensPLRFail2 .LT. 1)THEN
                      UnitarySystem(UnitarySysNum)%HXAssistedSensPLRFail2 = UnitarySystem(UnitarySysNum)%HXAssistedSensPLRFail2+1
                      CALL ShowWarningError(TRIM(UnitarySystem(UnitarySysNum)%UnitarySystemType)// &
                                            ' - DX unit sensible part-load ratio calculation failed: part-load '// &
                                            'ratio limits exceeded, for unit = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
                      CALL ShowContinueError('Estimated part-load ratio = '//RoundSigDigits(PartLoadFrac,3))
                      CALL ShowContinueErrorTimeStamp('The estimated part-load ratio will be used and the simulation'// &
                                                      ' continues. Occurrence info: ')
                    END IF
                    CALL ShowRecurringWarningErrorAtEnd(TRIM(UnitarySystem(UnitarySysNum)%UnitarySystemType)//' "'&
                        //TRIM(UnitarySystem(UnitarySysNum)%Name)//'" - DX unit sensible part-load ratio calculation'// &
                        ' failed error continues. Sensible PLR statistics follow.' &
                        ,UnitarySystem(UnitarySysNum)%HXAssistedSensPLRFailIndex2,PartLoadFrac,PartLoadFrac)
                  END IF
                END IF
              IF(UnitarySystem(UnitarySysNum)%CoolingCoilType_Num==CoilDX_CoolingHXAssisted) &
                   UnitarySystem(UnitarySysNum)%CompPartLoadRatio = PartLoadFrac

            CASE (CoilDX_CoolingTwoSpeed)
              Par(1) = REAL(UnitarySystem(UnitarySysNum)%CoolingCoilIndex,r64)
              Par(2) = DesOutTemp
              ! Par(3) is only needed for variable speed coils (see DXCoilVarSpeedResidual and DXCoilCyclingResidual)
              Par(3) = UnitarySysNum
              IF(SpeedRatio == 1.0d0)THEN
                CALL SolveRegulaFalsi(Acc, MaxIte, SolFla, SpeedRatio, DXCoilVarSpeedResidual, 0.0d0,   &
                                        1.0d0, Par)
                PartLoadFrac = SpeedRatio
              ELSE
                CALL SolveRegulaFalsi(Acc, MaxIte, SolFla, CycRatio, DXCoilCyclingResidual, 0.0d0,   &
                                      1.0d0, Par)
                PartLoadFrac = CycRatio
              END IF

            CASE (CoilDX_MultiSpeedCooling, Coil_CoolingAirToAirVariableSpeed, Coil_CoolingWaterToAirHPVSEquationFit)

                Par(1) = REAL(UnitarySystem(UnitarySysNum)%CoolingCoilIndex,r64)
                Par(2) = DesOutTemp
                Par(3) = UnitarySysNum
                ! Par(4) = CycRatio or SpeedRatio
                Par(5) = UnitarySystem(UnitarySysNum)%CoolingSpeedNum
                Par(6) = 1.0d0 ! UnitarySystem(UnitarySysNum)%FanOpMode
                Par(7) = 1.0d0 ! CompOp
                Par(8) = ReqOutput

              IF (SpeedRatio == 1.0d0) THEN
                Par(4) = CycRatio
                CALL SolveRegulaFalsi(Acc, MaxIte, SolFla, SpeedRatio, DXCoilVarSpeedResidual, 0.0d0,   &
                                              1.0d0, Par)
                UnitarySystem(UnitarySysNum)%CoolingCycRatio = SpeedRatio
                UnitarySystem(UnitarySysNum)%CoolingPartLoadFrac = SpeedRatio
                CALL CalcPassiveSystem(UnitarySysNum, FirstHVACIteration)
                PartLoadFrac = SpeedRatio
              ELSE
                SpeedRatio = 0.0d0
                UnitarySystem(UnitarySysNum)%CoolingSpeedRatio = SpeedRatio
                Par(4) = SpeedRatio

                CALL SolveRegulaFalsi(Acc, MaxIte, SolFla, CycRatio, DXCoilCyclingResidual, 0.0d0,   &
                                                1.0d0, Par)
                UnitarySystem(UnitarySysNum)%CoolingCycRatio = CycRatio
                UnitarySystem(UnitarySysNum)%CoolingPartLoadFrac = CycRatio
                CALL CalcPassiveSystem(UnitarySysNum, FirstHVACIteration)
                PartLoadFrac = CycRatio
              END IF

            CASE (CoilDX_CoolingTwoStageWHumControl) ! Coil:Cooling:DX:TwoStageWithHumidityControlMode

              Par(1) = REAL(UnitarySystem(UnitarySysNum)%CoolingCoilIndex,r64)
              Par(2) = DesOutTemp
              ! dehumidification mode = 0 for normal mode, 1+ for enhanced mode
              Par(3) = REAL(DehumidMode,r64)
              Par(4) = REAL(FanOpMode,r64)
              CALL SolveRegulaFalsi(Acc, MaxIte, SolFla, PartLoadFrac, MultiModeDXCoilResidual, 0.0d0,   &
                                    1.0d0, Par)
              UnitarySystem(UnitarySysNum)%CompPartLoadRatio = PartLoadFrac

            CASE (Coil_CoolingWater, Coil_CoolingWaterDetailed)  ! COIL:COOLING:WATER

              Par(1) = REAL(UnitarySysNum,r64)
              IF (FirstHVACIteration) THEN
                Par(2) = 1.0d0
              ELSE
                Par(2) = 0.0d0
              END IF
              Par(3) = DesOutTemp
              CALL SolveRegulaFalsi(Acc, MaxIte, SolFla, PartLoadFrac, CoolWaterTempResidual, 0.0d0,   &
                                            1.0d0, Par)

            CASE(Coil_CoolingWaterToAirHPSimple, Coil_CoolingWaterToAirHP)
              Par(1) = REAL(UnitarySysNum,r64)
              IF (FirstHVACIteration) THEN
                Par(2) = 1.0d0
              ELSE
                Par(2) = 0.0d0
              END IF
              Par(3) = DesOutTemp
              Par(4) = ReqOutput

              CALL SolveRegulaFalsi(Acc, MaxIte, SolFla, PartLoadFrac, CoolWatertoAirHPTempResidual, 0.0d0,   &
                                            1.0d0, Par)

            CASE DEFAULT
              CALL ShowMessage(' For :'//TRIM(UnitarySystem(UnitarySysNum)%UnitarySystemType)//'="'// &
                               TRIM(UnitarySystem(UnitarySysNum)%Name)//'"')
              CALL ShowFatalError('ControlCoolingSystem: Invalid cooling coil type = '//  &
                               TRIM(CALLCoilTypes(UnitarySystem(UnitarySysNum)%CoolingCoilType_Num)))

          END SELECT

        END IF
      END IF

!     IF system does not operate to meet sensible load, use no load humidity ratio to test against humidity setpoint,
!     ELSE use operating humidity ratio to test against humidity setpoint
      IF (PartLoadFrac .EQ. 0.0d0)THEN
        OutletHumRatDXCoil = NoLoadHumRatOut
      ELSE
        OutletHumRatDXCoil = Node(OutletNode)%HumRat
      END IF

      ! IF humidity setpoint is not satisfied and humidity control type is MultiMode,
      ! then enable heat exchanger and run to meet sensible load

      IF (( OutletHumRatDXCoil > (DesOutHumRat + HumRatAcc)) .AND. (PartLoadFrac .LT. 1.0d0) .AND. &
          (UnitarySystem(UnitarySysNum)%DehumidControlType_Num .EQ. DehumidControl_Multimode)) THEN

        SELECT CASE(UnitarySystem(UnitarySysNum)%CoolingCoilType_Num)

          CASE (CoilDX_CoolingHXAssisted)  ! CoilSystem:Cooling:DX:HeatExchangerAssisted
            ! Determine required part load when heat exchanger is ON
            HXUnitOn = .TRUE.
            PartLoadFrac = 1.0d0
            CALL SimHXAssistedCoolingCoil(CompName,FirstHVACIteration,On,PartLoadFrac, &
                                            UnitarySystem(UnitarySysNum)%CoolingCoilIndex, FanOpMode, &
                                            HXUnitEnable=HXUnitOn, EconomizerFlag=EconomizerFlag)

            OutletTempDXCoil = HXAssistedCoilOutletTemp(UnitarySystem(UnitarySysNum)%CoolingCoilIndex)

        !               FullOutput will be different than the FullOutput determined above during sensible PLR calculations
            FullOutput = Node(InletNode)%MassFlowRate *  &
                        (PsyHFnTdbW(Node(OutletNode)%Temp,Node(OutletNode)%HumRat)  &
                        - PsyHFnTdbW(Node(InletNode)%Temp,Node(OutletNode)%HumRat))

        !   Check to see if the system can meet the load with the compressor off
        !   If NoOutput is lower than (more cooling than required) or very near the ReqOutput, do not run the compressor
            IF ((NoLoadTempOut-DesOutTemp) .LT. Acc) THEN
                PartLoadFrac = 0.0d0
        !          OutletTempDXCoil is the full capacity outlet temperature at PartLoadFrac = 1 from the CALL above.
        !          if this temp is greater than or very near the desired outlet temp, then run the compressor at PartLoadFrac = 1.
!            ELSEIF ((OutletTempDXCoil > DesOutTemp) .OR. ABS(OutletTempDXCoil - DesOutTemp) .LE. (Acc*2.0d0)) THEN
            ELSEIF (OutletTempDXCoil > DesOutTemp - (Acc*2.0d0)) THEN
                PartLoadFrac = 1.0d0
            ELSE
                Par(1) = REAL(UnitarySystem(UnitarySysNum)%CoolingCoilIndex,r64)
                Par(2) = DesOutTemp
                ! FirstHVACIteration is a logical, Par is REAL(r64), so make TRUE = 1.0 and FALSE = 0.0
                IF(FirstHVACIteration)THEN
                Par(3) = 1.0d0
                ELSE
                Par(3) = 0.0d0
                END IF
                IF(HXUnitOn)THEN
                Par(4) = 1.0d0
                ELSE
                Par(4) = 0.0d0
                END IF
                Par(5) = REAL(FanOpMode,r64)
                CALL SolveRegulaFalsi(Acc, MaxIte, SolFla, PartLoadFrac, HXAssistedCoolCoilTempResidual, 0.0d0,   &
                                            1.0d0, Par)
            END IF
            UnitarySystem(UnitarySysNum)%CompPartLoadRatio = PartLoadFrac

          CASE (CoilDX_CoolingTwoStageWHumControl) ! Coil:Cooling:DX:TwoStageWithHumidityControlMode
                                        ! formerly (v3 and beyond) COIL:DX:MULTIMODE:COOLINGEMPIRICAL)

               ! Get full load result
                PartLoadFrac = 1.0d0
                DehumidMode  = 1
                UnitarySystem(UnitarySysNum)%dehumidificationMode = DehumidMode
                CALL SimDXCoilMultiMode(CompName,On,FirstHVACIteration,PartLoadFrac,DehumidMode,  &
                                        UnitarySystem(UnitarySysNum)%CoolingCoilIndex,FanOpMode)
                FullOutput = Node(InletNode)%MassFlowRate *  &
                             (PsyHFnTdbW(Node(OutletNode)%Temp,Node(InletNode)%HumRat)  &
                              - PsyHFnTdbW(Node(InletNode)%Temp,Node(InletNode)%HumRat))

                ! Since we are cooling, we expect FullOutput to be < 0 and FullOutput < NoCoolOutput
                ! Check that this is the case; IF not set PartLoadFrac = 0.0 (off) and return
                ! Calculate the part load fraction
                IF (FullOutput .GE. 0) THEN
                  PartLoadFrac = 0.0d0
                ELSE
                  OutletTempDXCoil = DXCoilOutletTemp(UnitarySystem(UnitarySysNum)%CoolingCoilIndex)
                  OutletHumRatDXCoil = DXCoilOutletHumRat(UnitarySystem(UnitarySysNum)%CoolingCoilIndex)
                  ! If sensible load and setpoint cannot be met, set PLR = 1. if no sensible load and
                  ! latent load exists and setpoint cannot be met, set PLR = 1.
! why is our logic different? Did we figure something out that reduced the logic?
!                  IF ((SensibleLoad .and. LatentLoad .AND. .NOT. UnitarySystem(UnitarySysNum)%RunOnLatentLoad .AND. &
!                       OutletHumRatDXCoil >= DesOutHumRat)) THEN
                  IF ((OutletTempDXCoil > (DesOutTemp-(Acc*2.0d0)) .AND. SensibleLoad .and.   &
                       UnitarySystem(UnitarySysNum)%RunOnSensibleLoad) .OR. &
                      (OutletHumRatDXCoil > (DesOutHumRat-(HumRatAcc*2.0d0)) .AND. &
                      .NOT. SensibleLoad .AND. LatentLoad .AND. UnitarySystem(UnitarySysNum)%RunOnLatentLoad)) THEN
                    PartLoadFrac = 1.0d0
!                  ELSEIF ((SensibleLoad .and. LatentLoad .AND. .NOT. UnitarySystem(UnitarySysNum)%RunOnLatentLoad .AND. &
!                       OutletHumRatDXCoil < DesOutHumRat)) THEN
                  ELSE IF (.NOT. SensibleLoad .AND. &
                      (OutletHumRatDXCoil < DesOutHumRat .AND. LatentLoad .AND. UnitarySystem(UnitarySysNum)%RunOnLatentLoad)) THEN
                    PartLoadFrac = ReqOutput/FullOutput
                    Par(1) = REAL(UnitarySystem(UnitarySysNum)%CoolingCoilIndex,r64)
                    Par(2) = DesOutHumRat
                    ! dehumidification mode = 0 for normal mode, 1+ for enhanced mode
                    Par(3) = REAL(DehumidMode,r64)
                    Par(4) = REAL(FanOpMode,r64)
                    CALL SolveRegulaFalsi(Acc, MaxIte, SolFla, PartLoadFrac, MultiModeDXCoilHumRatResidual, &
                                          0.0d0, 1.0d0, Par)
                  ELSE ! must be a sensible load so find PLR
                    PartLoadFrac = ReqOutput/FullOutput
                    Par(1) = REAL(UnitarySystem(UnitarySysNum)%CoolingCoilIndex,r64)
                    Par(2) = DesOutTemp
                    ! Dehumidification mode = 0 for normal mode, 1+ for enhanced mode
                    Par(3) = REAL(DehumidMode,r64)
                    Par(4) = REAL(FanOpMode,r64)
                    CALL SolveRegulaFalsi(Acc, MaxIte, SolFla, PartLoadFrac, MultiModeDXCoilResidual, &
                                          0.0d0, 1.0d0, Par)
                  END IF
                END IF
            UnitarySystem(UnitarySysNum)%CompPartLoadRatio = PartLoadFrac

          CASE DEFAULT

        END SELECT
      END IF ! END IF humidity ratio setpoint not met - Multimode humidity control

        ! IF humidity setpoint is not satisfied and humidity control type is CoolReheat,
        ! then overcool to meet moisture load

      IF (( OutletHumRatDXCoil > DesOutHumRat) .AND. (PartLoadFrac .LT. 1.0d0) .AND. LatentLoad .AND. &
            (UnitarySystem(UnitarySysNum)%DehumidControlType_Num .EQ. DehumidControl_CoolReheat)) THEN

!           IF NoLoadHumRatOut is lower than (more dehumidification than required) or very near the DesOutHumRat,
!           do not run the compressor
        IF ((NoLoadHumRatOut-DesOutHumRat) .LT. HumRatAcc) THEN
            PartLoadFrac = PartLoadFrac  ! keep part-load fraction from sensible calculation
!           If the FullLoadHumRatOut is greater than (insufficient dehumidification) or very near the DesOutHumRat,
!           run the compressor at PartLoadFrac = 1.
!        ELSEIF ((DesOutHumRat-FullLoadHumRatOut) .LT. HumRatAcc) THEN
        ELSEIF (FullLoadHumRatOut .GT. (DesOutHumRat-HumRatAcc)) THEN
            PartLoadFrac = 1.0d0
!           ELSE find the PLR to meet the load

        ELSE

          SELECT CASE(UnitarySystem(UnitarySysNum)%CoolingCoilType_Num)

            CASE (CoilDX_CoolingSingleSpeed)  ! COIL:DX:COOLINGBYPASSFACTOREMPIRICAL

                Par(1) = REAL(UnitarySystem(UnitarySysNum)%CoolingCoilIndex,r64)
                Par(2) = DesOutHumRat
                Par(5) = REAL(FanOpMode,r64)
                CALL SolveRegulaFalsi(HumRatAcc, MaxIte, SolFlaLat, PartLoadFrac, DOE2DXCoilHumRatResidual, 0.0d0,   &
                                            1.0d0, Par)
              UnitarySystem(UnitarySysNum)%CompPartLoadRatio = PartLoadFrac

            CASE (CoilDX_CoolingHXAssisted)  ! CoilSystem:Cooling:DX:HeatExchangerAssisted

!               IF NoLoadHumRatOut is lower than (more dehumidification than required) or very near the DesOutHumRat,
!               do not run the compressor
                IF ((NoLoadHumRatOut-DesOutHumRat) .LT. HumRatAcc*2.0d0) THEN
                  PartLoadFrac = PartLoadFrac  ! keep part-load fraction from sensible calculation
!                If the FullLoadHumRatOut is greater than (insufficient dehumidification) or very near the DesOutHumRat,
!                run the compressor at PartLoadFrac = 1.
                ELSEIF ((DesOutHumRat-FullLoadHumRatOut) .LT. HumRatAcc*2.0d0) THEN
                  PartLoadFrac = 1.0d0
!               ELSE find the PLR to meet the load
                ELSE
                  Par(1) = REAL(UnitarySystem(UnitarySysNum)%CoolingCoilIndex,r64)
                  Par(2) = DesOutHumRat
                  ! FirstHVACIteration is a logical, Par is REAL(r64), so make TRUE = 1 and FALSE = 0
                  IF(FirstHVACIteration)THEN
                    Par(3) = 1.0d0
                  ELSE
                    Par(3) = 0.0d0
                  END IF
                  IF(HXUnitOn)THEN
                    Par(4) = 1.0d0
                  ELSE
                    Par(4) = 0.0d0
                  END IF
                  Par(5) = REAL(FanOpMode,r64)
                  CALL SolveRegulaFalsi(HumRatAcc, MaxIte, SolFla, PartLoadFrac, HXAssistedCoolCoilHRResidual, 0.0d0,   &
                                        1.0d0, Par)
                  IF (SolFla == -1) THEN

!                   RegulaFalsi may not find latent PLR when the latent degradation model is used.
!                   IF iteration limit is exceeded, find tighter boundary of solution and repeat RegulaFalsi
                    TempMaxPLR = -0.1d0
                    TempOutletHumRatDXCoil = OutletHumRatDXCoil
                    DO WHILE((OutletHumRatDXCoil - TempOutletHumRatDXCoil) .GE. 0.0d0 .AND. TempMaxPLR .LE. 1.0d0)
!                     find upper limit of LatentPLR
                      TempMaxPLR = TempMaxPLR + 0.1d0
                      CALL SimHXAssistedCoolingCoil(CompName,FirstHVACIteration,On,TempMaxPLR, &
                                                    UnitarySystem(UnitarySysNum)%CoolingCoilIndex, FanOpMode, &
                                                    HXUnitEnable=HXUnitOn, EconomizerFlag=EconomizerFlag)
                      OutletHumRatDXCoil = HXAssistedCoilOutletHumRat(UnitarySystem(UnitarySysNum)%CoolingCoilIndex)
                    END DO
                    TempMinPLR = TempMaxPLR
                    DO WHILE((OutletHumRatDXCoil - TempOutletHumRatDXCoil) .LE. 0.0d0 .AND. TempMinPLR .GE. 0.0d0)
!                     pull upper limit of LatentPLR DOwn to last valid limit (i.e. latent output still exceeds SystemMoisuterLoad)
                      TempMaxPLR = TempMinPLR
!                     find minimum limit of Latent PLR
                      TempMinPLR = TempMinPLR - 0.01d0
                      CALL SimHXAssistedCoolingCoil(CompName,FirstHVACIteration,On,TempMaxPLR, &
                                                    UnitarySystem(UnitarySysNum)%CoolingCoilIndex, FanOpMode, &
                                                    HXUnitEnable=HXUnitOn, EconomizerFlag=EconomizerFlag)
                      OutletHumRatDXCoil = HXAssistedCoilOutletHumRat(UnitarySystem(UnitarySysNum)%CoolingCoilIndex)
                    END DO
!                   tighter boundary of solution has been found, CALL RegulaFalsi a second time
                    CALL SolveRegulaFalsi(HumRatAcc,MaxIte,SolFla,PartLoadFrac,HXAssistedCoolCoilHRResidual, &
                                          TempMinPLR,TempMaxPLR,Par)
                    IF (SolFla == -1) THEN
                      IF(.NOT. WarmupFlag)THEN
                        IF(UnitarySystem(UnitarySysNum)%HXAssistedCRLatPLRIter .LT. 1)THEN
                          UnitarySystem(UnitarySysNum)%HXAssistedCRLatPLRIter = &
                            UnitarySystem(UnitarySysNum)%HXAssistedCRLatPLRIter+1
                          CALL ShowWarningError(TRIM(UnitarySystem(UnitarySysNum)%UnitarySystemType)// &
                                                ' - Iteration limit exceeded calculating DX unit latent'// &
                                                ' part-load ratio for unit = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
                          CALL ShowContinueError('Estimated latent part-load ratio  = '//RoundSigDigits((ReqOutput/FullOutput),3))
                          CALL ShowContinueError('Calculated latent part-load ratio = '//RoundSigDigits(PartLoadFrac,3))
                          CALL ShowContinueErrorTimeStamp('The calculated latent part-load ratio will be used and the'// &
                                                          ' simulation continues. Occurrence info: ')
                        END IF
                        CALL ShowRecurringWarningErrorAtEnd(TRIM(UnitarySystem(UnitarySysNum)%UnitarySystemType)//' "'&
                            //TRIM(UnitarySystem(UnitarySysNum)%Name)//'" - Iteration limit exceeded calculating'// &
                            ' latent part-load ratio error continues. Latent PLR statistics follow.' &
                            ,UnitarySystem(UnitarySysNum)%HXAssistedCRLatPLRIterIndex,PartLoadFrac,PartLoadFrac)
                      END IF

                    ELSEIF (SolFla == -2) THEN

                      PartLoadFrac = ReqOutput/FullOutput
                      IF(.NOT. WarmupFlag)THEN
                        IF(UnitarySystem(UnitarySysNum)%HXAssistedCRLatPLRFail .LT. 1)THEN
                          UnitarySystem(UnitarySysNum)%HXAssistedCRLatPLRFail = &
                            UnitarySystem(UnitarySysNum)%HXAssistedCRLatPLRFail+1
                          CALL ShowWarningError(TRIM(UnitarySystem(UnitarySysNum)%UnitarySystemType)// &
                                         ' - DX unit latent part-load ratio calculation failed unexpectedly:'// &
                                         ' part-load ratio limits exceeded, for unit = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
                          CALL ShowContinueError('Estimated part-load ratio = '//RoundSigDigits(PartLoadFrac,3))
                          CALL ShowContinueErrorTimeStamp('The estimated part-load ratio will be used and the simulation'// &
                                                          ' continues. Occurrence info: ')
                        END IF
                        CALL ShowRecurringWarningErrorAtEnd(TRIM(UnitarySystem(UnitarySysNum)%UnitarySystemType)//' "'&
                            //TRIM(UnitarySystem(UnitarySysNum)%Name)//'" - DX unit latent part-load ratio calculation'// &
                            ' failed unexpectedly error continues. Latent PLR statistics follow.' &
                            ,UnitarySystem(UnitarySysNum)%HXAssistedCRLatPLRFailIndex,PartLoadFrac,PartLoadFrac)
                      END IF
                    END IF
                  ELSEIF (SolFla == -2) THEN
                    PartLoadFrac = ReqOutput/FullOutput
                    IF(.NOT. WarmupFlag)THEN
                      IF(UnitarySystem(UnitarySysNum)%HXAssistedCRLatPLRFail2 .LT. 1)THEN
                        UnitarySystem(UnitarySysNum)%HXAssistedCRLatPLRFail2 = &
                          UnitarySystem(UnitarySysNum)%HXAssistedCRLatPLRFail2+1
                        CALL ShowWarningError(TRIM(UnitarySystem(UnitarySysNum)%UnitarySystemType)// &
                                            ' - DX unit latent part-load ratio calculation failed: part-load '// &
                                             'ratio limits exceeded, for unit = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
                        CALL ShowContinueError('Estimated part-load ratio = '//RoundSigDigits(PartLoadFrac,3))
                        CALL ShowContinueErrorTimeStamp('The estimated part-load ratio will be used and the simulation'// &
                                                        ' continues. Occurrence info: ')
                      END IF
                      CALL ShowRecurringWarningErrorAtEnd(TRIM(UnitarySystem(UnitarySysNum)%UnitarySystemType)//' "'&
                          //TRIM(UnitarySystem(UnitarySysNum)%Name)//'" - DX unit latent part-load ratio calculation'// &
                          ' failed error continues. Latent PLR statistics follow.' &
                          ,UnitarySystem(UnitarySysNum)%HXAssistedCRLatPLRFailIndex2,PartLoadFrac,PartLoadFrac)
                    END IF
                  END IF
                END IF
              UnitarySystem(UnitarySysNum)%CompPartLoadRatio = PartLoadFrac

            CASE (CoilDX_CoolingTwoSpeed)

!               Simulate MultiSpeed DX coil at sensible result
                CALL SimDXCoilMultiSpeed(CompName,SpeedRatio,CycRatio,UnitarySystem(UnitarySysNum)%CoolingCoilIndex)

                OutletHumRatDXCoil = DXCoilOutletHumRat(UnitarySystem(UnitarySysNum)%CoolingCoilIndex)
                ! IF humidity setpoint is not satisfied and humidity control type is CoolReheat,
                ! then overcool to meet moisture load

                IF (OutletHumRatDXCoil > DesOutHumRat) THEN

                  CycRatio = 0.0d0
                  SpeedRatio = 0.0d0

                  CALL SimDXCoilMultiSpeed(CompName,0.0d0,1.0d0,UnitarySystem(UnitarySysNum)%CoolingCoilIndex)
                  OutletHumRatLS = DXCoilOutletHumRat(UnitarySystem(UnitarySysNum)%CoolingCoilIndex)
                  IF (OutletHumRatLS > DesOutHumRat) THEN
                    CycRatio = 1.0d0
                    CALL SimDXCoilMultiSpeed(CompName,1.0d0,1.0d0,UnitarySystem(UnitarySysNum)%CoolingCoilIndex)
                    OutletHumRatHS = DXCoilOutletHumRat(UnitarySystem(UnitarySysNum)%CoolingCoilIndex)
                    IF (OutletHumRatHS < DesOutHumRat) THEN
                      Par(1) = REAL(UnitarySystem(UnitarySysNum)%CoolingCoilIndex,r64)
                      Par(2) = DesOutHumRat
                      CALL SolveRegulaFalsi(HumRatAcc, MaxIte, SolFla, SpeedRatio, DXCoilVarSpeedHumRatResidual, 0.0d0, &
                                            1.0d0, Par)
                    ELSE
                      SpeedRatio = 1.0d0
                    END IF
                  ELSE
                    SpeedRatio = 0.0d0
                    Par(1) = REAL(UnitarySystem(UnitarySysNum)%CoolingCoilIndex,r64)
                    Par(2) = DesOutHumRat
                    CALL SolveRegulaFalsi(HumRatAcc, MaxIte, SolFla, CycRatio, DXCoilCyclingHumRatResidual, 0.0d0, &
                                          1.0d0, Par)
                  END IF

                END IF

            CASE (CoilDX_MultiSpeedCooling, Coil_CoolingAirToAirVariableSpeed, Coil_CoolingWaterToAirHPVSEquationFit)

                IF (UnitarySystem(UnitarySysNum)%CoolingCoilType_Num == CoilDX_MultiSpeedCooling) THEN
                  CALL SimDXCoilMultiSpeed(CompName,SpeedRatio,CycRatio,UnitarySystem(UnitarySysNum)%CoolingCoilIndex)
                ELSE
                  CALL SimVariableSpeedCoils(CompName,UnitarySystem(UnitarySysNum)%CoolingCoilIndex, &
                                    UnitarySystem(UnitarySysNum)%FanOpMode,UnitarySystem(UnitarySysNum)%MaxONOFFCyclesperHour, &
                                    UnitarySystem(UnitarySysNum)%HPTimeConstant,UnitarySystem(UnitarySysNum)%FanDelayTime, &
                                    1, CycRatio, OnOffAirFlowRatio,SpeedNum, SpeedRatio,ReqOutput,Dummy )
                END IF

                OutletHumRatDXCoil = DXCoilOutletHumRat(UnitarySystem(UnitarySysNum)%CoolingCoilIndex)
                ! IF humidity setpoint is not satisfied and humidity control type is CoolReheat,
                ! then overcool to meet moisture load

                IF (OutletHumRatDXCoil > DesOutHumRat) THEN

                  CycRatio = 0.0d0
                  SpeedRatio = 0.0d0

                  IF (UnitarySystem(UnitarySysNum)%CoolingCoilType_Num == CoilDX_MultiSpeedCooling) THEN
                    CALL SimDXCoilMultiSpeed(CompName,0.0d0,1.0d0,UnitarySystem(UnitarySysNum)%CoolingCoilIndex)
                  ELSE
                    CALL SimVariableSpeedCoils(CompName,UnitarySystem(UnitarySysNum)%CoolingCoilIndex, &
                                 UnitarySystem(UnitarySysNum)%FanOpMode,UnitarySystem(UnitarySysNum)%MaxONOFFCyclesperHour, &
                                 UnitarySystem(UnitarySysNum)%HPTimeConstant,UnitarySystem(UnitarySysNum)%FanDelayTime, &
                                 1, 1.0d0, OnOffAirFlowRatio,SpeedNum, 1.0d0,ReqOutput,Dummy )
                  END IF

                  OutletHumRatLS = DXCoilOutletHumRat(UnitarySystem(UnitarySysNum)%CoolingCoilIndex)

                  IF (OutletHumRatLS > DesOutHumRat) THEN
                    CycRatio = 1.0d0
                    IF (UnitarySystem(UnitarySysNum)%CoolingCoilType_Num == CoilDX_MultiSpeedCooling) THEN
                      CALL SimDXCoilMultiSpeed(CompName,1.0d0,1.0d0,UnitarySystem(UnitarySysNum)%CoolingCoilIndex)
                    ELSE
                      CALL SimVariableSpeedCoils(CompName,UnitarySystem(UnitarySysNum)%CoolingCoilIndex, &
                                   UnitarySystem(UnitarySysNum)%FanOpMode,UnitarySystem(UnitarySysNum)%MaxONOFFCyclesperHour, &
                                   UnitarySystem(UnitarySysNum)%HPTimeConstant,UnitarySystem(UnitarySysNum)%FanDelayTime, &
                                   1, 1.0d0, OnOffAirFlowRatio,SpeedNum, 1.0d0,ReqOutput,Dummy )
                    END IF
                    OutletHumRatHS = DXCoilOutletHumRat(UnitarySystem(UnitarySysNum)%CoolingCoilIndex)
                    IF (OutletHumRatHS < DesOutHumRat) THEN
                      Par(1) = REAL(UnitarySystem(UnitarySysNum)%CoolingCoilIndex,r64)
                      Par(2) = DesOutHumRat
                      Par(3) = ReqOutput
                      CALL SolveRegulaFalsi(HumRatAcc, MaxIte, SolFla, SpeedRatio, DXCoilVarSpeedHumRatResidual, 0.0d0,   &
                                                1.0d0, Par)
                    ELSE
                      SpeedRatio = 1.0d0
                    END IF
                  ELSE
                    SpeedRatio = 0.0d0
                    Par(1) = REAL(UnitarySystem(UnitarySysNum)%CoolingCoilIndex,r64)
                    Par(2) = DesOutHumRat
                    Par(3) = ReqOutput
                      CALL SolveRegulaFalsi(HumRatAcc, MaxIte, SolFla, CycRatio, DXCoilCyclingHumRatResidual, 0.0d0,   &
                                                1.0d0, Par)
                  END IF

                END IF

            CASE (CoilDX_CoolingTwoStageWHumControl) ! Coil:Cooling:DX:TwoStageWithHumidityControlMode

                Par(1) = REAL(UnitarySystem(UnitarySysNum)%CoolingCoilIndex,r64)
                Par(2) = DesOutHumRat
                ! dehumidification mode = 0 for normal mode, 1+ for enhanced mode
                Par(3) = REAL(DehumidMode,r64)
                Par(4) = REAL(FanOpMode,r64)
                CALL SolveRegulaFalsi(Acc, MaxIte, SolFlaLat, PartLoadFrac, MultiModeDXCoilHumRatResidual, 0.0d0,   &
                                            1.0d0, Par)
                UnitarySystem(UnitarySysNum)%CompPartLoadRatio = PartLoadFrac

            CASE (Coil_CoolingWater, Coil_CoolingWaterDetailed)  ! COIL:COOLING:WATER

                Par(1) = REAL(UnitarySysNum,r64)
                IF (FirstHVACIteration) THEN
                Par(2) = 1.0d0
                ELSE
                Par(2) = 0.0d0
                END IF
                Par(3) = DesOutHumRat

                CALL SolveRegulaFalsi(HumRatAcc, MaxIte, SolFlaLat, PartLoadFrac, CoolWaterHumRatResidual, 0.0d0,   &
                                            1.0d0, Par)

            CASE(Coil_CoolingWaterToAirHPSimple, Coil_CoolingWaterToAirHP)

                Par(1) = REAL(UnitarySysNum,r64)
                IF (FirstHVACIteration) THEN
                  Par(2) = 1.0d0
                ELSE
                  Par(2) = 0.0d0
                END IF
                Par(3) = DesOutHumRat
                Par(4) = ReqOutput

                CALL SolveRegulaFalsi(HumRatAcc, MaxIte, SolFlaLat, PartLoadFrac, CoolWatertoAirHPHumRatResidual, 0.0d0,   &
                                                1.0d0, Par)

              CASE DEFAULT

            END SELECT
        END IF
      END IF
    END IF
  END IF

  IF (SolFla == -1) THEN
    IF(.NOT. WarmupFlag)THEN
      IF(UnitarySystem(UnitarySysNum)%SensPLRIter .LT. 1)THEN
        UnitarySystem(UnitarySysNum)%SensPLRIter = UnitarySystem(UnitarySysNum)%SensPLRIter+1
        CALL ShowWarningError(TRIM(UnitarySystem(UnitarySysNum)%UnitarySystemType)// &
                                ' - Iteration limit exceeded calculating '// &
                                'part-load ratio for unit = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
        CALL ShowContinueError('Estimated part-load ratio  = '//RoundSigDigits((ReqOutput/FullOutput),3))
        CALL ShowContinueError('Calculated part-load ratio = '//RoundSigDigits(PartLoadFrac,3))
        CALL ShowContinueErrorTimeStamp('The calculated part-load ratio will be used and the simulation'// &
                                        ' continues. Occurrence info: ')
      ELSE
        CALL ShowRecurringWarningErrorAtEnd(TRIM(UnitarySystem(UnitarySysNum)%UnitarySystemType)//' "'&
            //TRIM(UnitarySystem(UnitarySysNum)%Name)//'" - Iteration limit exceeded calculating'// &
            ' sensible part-load ratio error continues. Sensible PLR statistics follow.' &
            ,UnitarySystem(UnitarySysNum)%SensPLRIterIndex,PartLoadFrac,PartLoadFrac)
      END IF
    END IF
  ELSEIF (SolFla == -2) THEN
    PartLoadFrac = ReqOutput/FullOutput
    IF(.NOT. WarmupFlag)THEN
      IF(UnitarySystem(UnitarySysNum)%SensPLRFail .LT. 1)THEN
        UnitarySystem(UnitarySysNum)%SensPLRFail = UnitarySystem(UnitarySysNum)%SensPLRFail+1
        CALL ShowWarningError(TRIM(UnitarySystem(UnitarySysNum)%UnitarySystemType)//' - sensible part-'// &
                        'load ratio calculation failed: part-load ratio limits exceeded, for unit = '// &
                            TRIM(UnitarySystem(UnitarySysNum)%Name))
        CALL ShowContinueError('Estimated part-load ratio = '//RoundSigDigits(PartLoadFrac,3))
        CALL ShowContinueErrorTimeStamp('The estimated part-load ratio will be used and the simulation'// &
                                        ' continues. Occurrence info: ')
      ELSE
        CALL ShowRecurringWarningErrorAtEnd(TRIM(UnitarySystem(UnitarySysNum)%UnitarySystemType)//' "'&
            //TRIM(UnitarySystem(UnitarySysNum)%Name)//'" - sensible part-load ratio calculation'// &
            ' failed error continues. Sensible PLR statistics follow.' &
            ,UnitarySystem(UnitarySysNum)%SensPLRFailIndex,PartLoadFrac,PartLoadFrac)
      END IF
    END IF
  END IF

  IF (SolFlaLat == -1 .AND. SolFla .NE. -1) THEN
    IF(.NOT. WarmupFlag)THEN
      IF(UnitarySystem(UnitarySysNum)%LatPLRIter .LT. 1)THEN
        UnitarySystem(UnitarySysNum)%LatPLRIter = UnitarySystem(UnitarySysNum)%LatPLRIter+1
        CALL ShowWarningError(TRIM(UnitarySystem(UnitarySysNum)%UnitarySystemType)// &
                                ' - Iteration limit exceeded calculating latent part-load'// &
                                ' ratio for unit = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
        CALL ShowContinueError('Estimated part-load ratio   = '//RoundSigDigits((ReqOutput/FullOutput),3))
        CALL ShowContinueError('Calculated part-load ratio = '//RoundSigDigits(PartLoadFrac,3))
        CALL ShowContinueErrorTimeStamp('The calculated part-load ratio will be used and the simulation'// &
                                        ' continues. Occurrence info: ')
      END IF
        CALL ShowRecurringWarningErrorAtEnd(TRIM(UnitarySystem(UnitarySysNum)%UnitarySystemType)//' "'&
            //TRIM(UnitarySystem(UnitarySysNum)%Name)//'" - Iteration limit exceeded calculating'// &
            ' latent part-load ratio error continues. Latent PLR statistics follow.' &
            ,UnitarySystem(UnitarySysNum)%LatPLRIterIndex,PartLoadFrac,PartLoadFrac)
    END IF
  ELSEIF (SolFlaLat == -2 .AND. SolFla .NE. -2) THEN
!               RegulaFalsi returns PLR = minPLR when a solution cannot be found, recalculate PartLoadFrac.
    IF(NoLoadHumRatOut-FullLoadHumRatOut .NE. 0.0d0)THEN
        PartLoadFrac = (NoLoadHumRatOut-DesOutHumRat)/(NoLoadHumRatOut-FullLoadHumRatOut)
    ELSE
        PartLoadFrac = 1.0d0
    END IF
    IF(.NOT. WarmupFlag)THEN
      IF(UnitarySystem(UnitarySysNum)%LatPLRFail .LT. 1)THEN
        UnitarySystem(UnitarySysNum)%LatPLRFail = UnitarySystem(UnitarySysNum)%LatPLRFail+1
        CALL ShowWarningError(TRIM(UnitarySystem(UnitarySysNum)%UnitarySystemType)//' - latent part-'// &
                        'load ratio calculation failed: part-load ratio limits exceeded, for unit = '//&
                        TRIM(UnitarySystem(UnitarySysNum)%Name))
        CALL ShowContinueError('Estimated part-load ratio = '//RoundSigDigits(PartLoadFrac,3))
        CALL ShowContinueErrorTimeStamp('The estimated part-load ratio will be used and the simulation'// &
                                        ' continues. Occurrence info: ')
      END IF
        CALL ShowRecurringWarningErrorAtEnd(TRIM(UnitarySystem(UnitarySysNum)%UnitarySystemType)//' "'&
            //TRIM(UnitarySystem(UnitarySysNum)%Name)//'" - latent part-load ratio calculation'// &
            ' failed error continues. Latent PLR statistics follow.' &
            ,UnitarySystem(UnitarySysNum)%LatPLRFailIndex,PartLoadFrac,PartLoadFrac)
    END IF
  END IF
  !Set the final results

  IF(PartLoadFrac .GT. 1.0d0) THEN
    PartLoadFrac = 1.0d0
  ELSEIF(PartLoadFrac < 0.0d0) THEN
    PartLoadFrac = 0.0d0
  END IF

  UnitarySystem(UnitarySysNum)%CoolingPartLoadFrac = PartLoadFrac
  UnitarySystem(UnitarySysNum)%CoolingSpeedRatio = SpeedRatio
  UnitarySystem(UnitarySysNum)%CoolingCycRatio = CycRatio
  UnitarySystem(UnitarySysNum)%dehumidificationMode = DehumidMode

  LoopDXCoilRTF      = MAX(LoopDXCoilRTF, LoopDXCoilMaxRTFSave)

RETURN
END SUBROUTINE ControlCoolingSystem

SUBROUTINE ControlHeatingSystem(UnitarySysNum, FirstHVACIteration)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Raustad, FSEC
          !       DATE WRITTEN   February 2013
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !  Simulate the coil object at the required PLR.

          ! METHODOLOGY EMPLOYED:
          !  Calculate operating PLR and adjust speed when using multispeed coils.

          ! REFERENCES:
          !  na

          ! USE STATEMENTS:
  USE Psychrometrics , ONLY: PsyHFnTdbW, PsyTdpFnWPb
  USE General,         ONLY: SolveRegulaFalsi, RoundSigDigits
  USE DXCoils,         ONLY: SimDXCoil, SimDXCoilMultiSpeed, DXCoilOutletTemp, DXCoilOutletHumRat
  USE HeatingCoils,    ONLY: SimulateHeatingCoilComponents
  USE DataAirLoop,     ONLY: LoopHeatingCoilMaxRTF, LoopDXCoilRTF
  USE WaterCoils,      ONLY: SimulateWaterCoilComponents
  USE SteamCoils,      ONLY: SimulateSteamCoilComponents
  USE PlantUtilities,  ONLY: SetComponentFlowRate
  USE WatertoAirHeatPumpSimple,  ONLY: SimWatertoAirHPSimple
  USE WatertoAirHeatPump,        ONLY: SimWaterToAirHP
  USE VariableSpeedCoils,        ONLY: SimVariableSpeedCoils, VarSpeedCoil

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER,  INTENT(In)    :: UnitarySysNum             ! index to Unitary System
  LOGICAL,  INTENT(In)    :: FirstHVACIteration        ! First HVAC iteration flag

          ! SUBROUTINE PARAMETER DEFINITIONS:
  INTEGER, PARAMETER   :: MaxIte    = 500     ! Maximum number of iterations for solver
  REAL(r64), PARAMETER :: Acc       = 1.0d-3   ! Accuracy of solver result
  REAL(r64), PARAMETER :: HumRatAcc = 1.0d-6   ! Accuracy of solver result
  LOGICAL, PARAMETER   :: SuppHeatingCoilFlag = .FALSE.

          ! INTERFACE BLOCK SPECIFICATIONS
          !  na

          ! DERIVED TYPE DEFINITIONS
          !  na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  CHARACTER(len=MaxNameLength)  :: CompName  ! Name of the heating coil
  INTEGER             :: CompIndex           ! Index to the heating coil
!  REAL(r64)           :: NoOutput            ! Sensible capacity (outlet - inlet) when the compressor is off
  REAL(r64)           :: FullOutput          ! Sensible capacity (outlet - inlet) when the compressor is on
  REAL(r64)           :: ReqOutput           ! Sensible capacity (outlet - inlet) required to meet load or set point temperature
  Integer             :: InletNode           ! Inlet node number of the DX cooling coil
  Integer             :: OutletNode          ! Outlet node number of the DX cooling coil
  Integer             :: ControlNode         ! The node number where a set point is placed to control the DX cooling coil
  REAL(r64)           :: PartLoadFrac        ! The part-load fraction of the compressor

  REAL(r64)           :: SpeedRatio          ! SpeedRatio = (CompressorSpeed - CompressorSpeedMin) /
                                             !              (CompressorSpeedMax - CompressorSpeedMin)
                                             ! for variable speed or 2 speed compressors
  INTEGER             :: SpeedNum
  REAL(r64)           :: CycRatio            ! Cycling part-load ratio for variable speed or 2 speed compressors
  REAL(r64)           :: DesOutTemp          ! Desired outlet temperature of the DX cooling coil

  INTEGER             :: SolFla              ! Flag of solver, num iterations if >0, else error index
  REAL(r64), DIMENSION(8)  :: Par                 ! Parameter array passed to solver
  LOGICAL             :: SensibleLoad        ! True if there is a sensible cooling load on this system
  LOGICAL             :: LatentLoad          ! True if there is a latent   cooling load on this system
  INTEGER             :: FanOpMode           ! Supply air fan operating mode
  REAL(R64)           :: LoopHeatingCoilMaxRTFSave ! Used to find RTF of heating coils without overwriting globabl variable
  REAL(R64)           :: LoopDXCoilMaxRTFSave ! Used to find RTF of DX heating coils without overwriting globabl variable
  REAL(r64)           :: dummy               ! dummy variable for heating latent demand
  REAL(r64)           :: WSHPRuntimeFrac            ! Run time fraction of water to air hp
  REAL(R64)           :: OutdoorDryBulb     ! local variable for OutDryBulbTemp
  REAL(R64)           :: OutdoorHumRat      ! local variable for OutHumRat
  REAL(R64)           :: OutdoorPressure    ! local variable for OutBaroPress
  REAL(R64)           :: OutdoorWetBulb     ! local variable for OutWetBulbTemp
  REAL(R64)           :: OutletTemp

      ! Set local variables
      ! Retrieve the load on the controlled zone
  InletNode    = UnitarySystem(UnitarySysNum)%HeatCoilInletNodeNum
  OutletNode   = UnitarySystem(UnitarySysNum)%HeatCoilOutletNodeNum
  ControlNode  = UnitarySystem(UnitarySysNum)%SystemHeatControlNodeNum
  DesOutTemp   = UnitarySystem(UnitarySysNum)%DesiredOutletTemp
  LoopHeatingCoilMaxRTFSave=LoopHeatingCoilMaxRTF
  LoopHeatingCoilMaxRTF = 0.0d0
  LoopDXCoilMaxRTFSave=LoopDXCoilRTF
  LoopDXCoilRTF=0.0d0

  CompName     = UnitarySystem(UnitarySysNum)%HeatingCoilName
  CompIndex    = UnitarySystem(UnitarySysNum)%HeatingCoilIndex
  FanOpMode    = UnitarySystem(UnitarySysNum)%FanOpMode

  PartLoadFrac = 0.0d0
  SpeedRatio   = 0.0d0
  CycRatio     = 0.0d0
  Dummy        = 0.0d0
  SolFla       = 0.0d0
  SensibleLoad = .FALSE.
  LatentLoad   = .FALSE.

  IF (UnitarySystem(UnitarySysNum)%CondenserNodeNum /= 0) THEN
    OutdoorDryBulb  = Node(UnitarySystem(UnitarySysNum)%CondenserNodeNum)%Temp
    IF(UnitarySystem(UnitarySysNum)%CondenserType == WaterCooled)THEN
      OutdoorHumRat   = OutHumRat
      OutdoorPressure = OutBaroPress
      OutdoorWetBulb  = OutWetBulbTemp
    ELSE
      OutdoorPressure = Node(UnitarySystem(UnitarySysNum)%CondenserNodeNum)%Press
      ! IF node is not connected to anything, pressure = default, use weather data
      IF(OutdoorPressure == DefaultNodeValues%Press)THEN
        OutdoorDryBulb  = OutDryBulbTemp
        OutdoorHumRat   = OutHumRat
        OutdoorPressure = OutBaroPress
        OutdoorWetBulb  = OutWetBulbTemp
      ELSE
        OutdoorHumRat   = Node(UnitarySystem(UnitarySysNum)%CondenserNodeNum)%HumRat
  !     this should use Node%WetBulbTemp or a PSYC function, not OAWB
        OutdoorWetBulb  = Node(UnitarySystem(UnitarySysNum)%CondenserNodeNum)%OutAirWetBulb
      END IF
    END IF
  ELSE
    OutdoorDryBulb  = OutDryBulbTemp
    OutdoorHumRat   = OutHumRat
    OutdoorPressure = OutBaroPress
    OutdoorWetBulb  = OutWetBulbTemp
  END IF

  ! IF DXHeatingSystem is scheduled on and there is flow
  IF(GetCurrentScheduleValue(UnitarySystem(UnitarySysNum)%SysAvailSchedPtr) > 0.0d0 .AND. &
      GetCurrentScheduleValue(UnitarySystem(UnitarySysNum)%HeatingCoilAvailSchPtr) > 0.0d0 .AND. &
      Node(InletNode)%MassFlowRate .gt. MinAirMassFlow .AND. UnitarySystem(UnitarySysNum)%CoolingPartLoadFrac == 0.0d0) THEN

    ! Determine if there is a sensible load on this system
!    IF((Node(InletNode)%Temp < Node(ControlNode)%TempSetPoint) .AND. &
    IF((Node(InletNode)%Temp < DesOutTemp) .AND. &
       (ABS(Node(InletNode)%Temp - DesOutTemp) .gt. TempControlTol) ) SensibleLoad = .TRUE.


    ! IF DXHeatingSystem runs with a heating load then set PartLoadFrac on Heating System
    IF (SensibleLoad ) THEN

      ReqOutput = Node(InletNode)%MassFlowRate *  &
                      (PsyHFnTdbW(UnitarySystem(UnitarySysNum)%DesiredOutletTemp,Node(InletNode)%HumRat) - &
                       PsyHFnTdbW(Node(InletNode)%Temp,Node(InletNode)%HumRat))

      ! Get no load result
      PartLoadFrac = 0.0d0
      WSHPRuntimeFrac = 0.0d0

      SELECT CASE(UnitarySystem(UnitarySysNum)%HeatingCoilType_Num)

        CASE (CoilDX_HeatingEmpirical)  ! Coil:Heating:DX:SingleSpeed

          CALL SimDXCoil(CompName,On,FirstHVACIteration,PartLoadFrac,UnitarySystem(UnitarySysNum)%HeatingCoilIndex,FanOpMode)
          UnitarySystem(UnitarySysNum)%CompPartLoadRatio = PartLoadFrac

        CASE (CoilDX_MultiSpeedHeating, Coil_HeatingAirToAirVariableSpeed, Coil_HeatingWaterToAirHPVSEquationFit, &
              Coil_HeatingElectric_MultiStage, Coil_HeatingGas_MultiStage)

          CALL SimMultiSpeedCoils(UnitarySysNum, FirstHVACIteration, SensibleLoad, LatentLoad, PartLoadFrac, &
                                  HeatingCoil)

        CASE (Coil_HeatingGas,Coil_HeatingElectric,Coil_HeatingDesuperheater )

          CALL SimulateHeatingCoilComponents(CompName,FirstHVACIteration, &
                                CompIndex=CompIndex, &
                                FanOpMode=FanOpMode, QCoilReq = 0.0d0)

        CASE (Coil_HeatingWater)

          CALL SimWaterCoils(UnitarySysNum, FirstHVACIteration, PartLoadFrac, HeatingCoil)

        CASE (Coil_HeatingSteam)

          CALL SimSteamCoils(UnitarySysNum, FirstHVACIteration, PartLoadFrac, HeatingCoil)
          IF(UnitarySystem(UnitarySysNum)%MaxHeatCoilFluidFlow .GT. 0.0d0)PartLoadFrac = &
                     MIN(1.0d0,Node(UnitarySystem(UnitarySysNum)%HeatCoilFluidOutletNodeNum)%MassFlowRate/ &
                              UnitarySystem(UnitarySysNum)%MaxHeatCoilFluidFlow)

        CASE(Coil_HeatingWaterToAirHPSimple)

          CALL SimWatertoAirHPSimple(Blank, UnitarySystem(UnitarySysNum)%HeatingCoilIndex, ReqOutput, Dummy, &
                                     FanOpMode,WSHPRuntimeFrac, UnitarySystem(UnitarySysNum)%MaxONOFFCyclesperHour, &
                                     UnitarySystem(UnitarySysNum)%HPTimeConstant, UnitarySystem(UnitarySysNum)%FanDelayTime, &
                                     0, PartLoadFrac, FirstHVACIteration)
          UnitarySystem(UnitarySysNum)%CompPartLoadRatio = PartLoadFrac

        CASE(Coil_HeatingWaterToAirHP)

          CALL SimWatertoAirHP(Blank, UnitarySystem(UnitarySysNum)%HeatingCoilIndex, &
                                 UnitarySystem(UnitarySysNum)%MaxHeatAirMassFlow,FanOpMode, &
                                 FirstHVACIteration,WSHPRuntimeFrac,UnitarySystem(UnitarySysNum)%MaxONOFFCyclesperHour, &
                                 UnitarySystem(UnitarySysNum)%HPTimeConstant, UnitarySystem(UnitarySysNum)%FanDelayTime, &
                                 UnitarySystem(UnitarySysNum)%InitHeatPump, ReqOutput,Dummy,0, PartLoadFrac)
          UnitarySystem(UnitarySysNum)%CompPartLoadRatio = PartLoadFrac

        CASE DEFAULT

      END SELECT

!     IF outlet temp at no load is within ACC of set point, do not run the coil
      IF(ABS(Node(OutletNode)%Temp-DesOutTemp) < Acc)THEN
       ! do nothing, coil is at the set point.
      ELSE IF ((Node(OutletNode)%Temp-DesOutTemp) > Acc) THEN ! IF outlet temp is above set point turn off coil
        PartLoadFrac = 0.0d0
      ELSE ! ELSE get full load result

        ! Get full load result
        PartLoadFrac = 1.0d0
        WSHPRuntimeFrac = 1.0d0

        SELECT CASE(UnitarySystem(UnitarySysNum)%HeatingCoilType_Num)

          CASE (CoilDX_HeatingEmpirical)  ! Coil:Heating:DX:SingleSpeed

            CALL SimDXCoil(CompName,On,FirstHVACIteration,PartLoadFrac,UnitarySystem(UnitarySysNum)%HeatingCoilIndex,FanOpMode)
            UnitarySystem(UnitarySysNum)%CompPartLoadRatio = PartLoadFrac

          CASE (CoilDX_MultiSpeedHeating, Coil_HeatingAirToAirVariableSpeed, Coil_HeatingWaterToAirHPVSEquationFit, &
                Coil_HeatingElectric_MultiStage, Coil_HeatingGas_MultiStage)

            CycRatio = 1.0d0
            DO SpeedNum = 1, UnitarySystem(UnitarySysNum)%NumOfSpeedHeating
              CALL SimMultiSpeedCoils(UnitarySysNum, FirstHVACIteration, SensibleLoad, LatentLoad, PartLoadFrac, &
                                      HeatingCoil, SpeedNum)
              OutletTemp = Node(OutletNode)%Temp
              SpeedRatio = REAL(SpeedNum,r64) - 1.0d0
              IF (OutletTemp > DesOutTemp .AND. SensibleLoad) EXIT
            END DO

          CASE (Coil_HeatingGas,Coil_HeatingElectric)

            CALL SimulateHeatingCoilComponents(CompName,FirstHVACIteration, &
                                CompIndex=CompIndex, &
                                FanOpMode=FanOpMode, QCoilReq = UnitarySystem(UnitarySysNum)%DesignHeatingCapacity)

          CASE (Coil_HeatingDesuperheater )

              CALL SimulateHeatingCoilComponents(CompName,FirstHVACIteration, &
                                CompIndex=CompIndex, &
                                FanOpMode=FanOpMode, QCoilReq = ReqOutput)

          CASE (Coil_HeatingWater)

            CALL SimWaterCoils(UnitarySysNum, FirstHVACIteration, PartLoadFrac, HeatingCoil)

          CASE (Coil_HeatingSteam)

            CALL SimSteamCoils(UnitarySysNum, FirstHVACIteration, PartLoadFrac, HeatingCoil)
            IF(UnitarySystem(UnitarySysNum)%MaxHeatCoilFluidFlow .GT. 0.0d0)PartLoadFrac = &
                     MIN(1.0d0,Node(UnitarySystem(UnitarySysNum)%HeatCoilFluidOutletNodeNum)%MassFlowRate/ &
                              UnitarySystem(UnitarySysNum)%MaxHeatCoilFluidFlow)


          CASE(Coil_HeatingWaterToAirHPSimple)

            CALL SimWatertoAirHPSimple(Blank, UnitarySystem(UnitarySysNum)%HeatingCoilIndex, ReqOutput, Dummy, &
                                     FanOpMode,WSHPRuntimeFrac, UnitarySystem(UnitarySysNum)%MaxONOFFCyclesperHour, &
                                     UnitarySystem(UnitarySysNum)%HPTimeConstant, UnitarySystem(UnitarySysNum)%FanDelayTime, &
                                     0, PartLoadFrac, FirstHVACIteration)
            UnitarySystem(UnitarySysNum)%CompPartLoadRatio = PartLoadFrac

          CASE(Coil_HeatingWaterToAirHP)
            CALL SimWatertoAirHP(Blank, UnitarySystem(UnitarySysNum)%HeatingCoilIndex, &
                                 UnitarySystem(UnitarySysNum)%MaxHeatAirMassFlow,FanOpMode, &
                                 FirstHVACIteration,WSHPRuntimeFrac,UnitarySystem(UnitarySysNum)%MaxONOFFCyclesperHour, &
                                 UnitarySystem(UnitarySysNum)%HPTimeConstant, UnitarySystem(UnitarySysNum)%FanDelayTime, &
                                 UnitarySystem(UnitarySysNum)%InitHeatPump, ReqOutput,Dummy,0, PartLoadFrac)
            UnitarySystem(UnitarySysNum)%CompPartLoadRatio = PartLoadFrac


          CASE DEFAULT

        END SELECT

        FullOutput = Node(InletNode)%MassFlowRate *  &
                       (PsyHFnTdbW(Node(OutletNode)%Temp,Node(InletNode)%HumRat)  &
                        - PsyHFnTdbW(Node(InletNode)%Temp,Node(InletNode)%HumRat))

!       If the outlet temp is within ACC of set point,
        IF(ABS(Node(OutletNode)%Temp-DesOutTemp) < Acc)THEN
          ! do nothing, coil is at set point
        ELSE IF (Node(OutletNode)%Temp .LT. (DesOutTemp-Acc)) THEN ! IF outlet temp is below set point coil must be on
          PartLoadFrac = 1.0d0
        ELSE  ! ELSE find the PLR to meet the set point

          SELECT CASE(UnitarySystem(UnitarySysNum)%HeatingCoilType_Num)

            CASE (CoilDX_HeatingEmpirical)  ! Coil:Heating:DX:SingleSpeed

              Par(1) = REAL(UnitarySystem(UnitarySysNum)%HeatingCoilIndex,r64)
              Par(2) = DesOutTemp
              Par(3) = 1.0d0  !OnOffAirFlowFrac assume = 1.0 for continuous fan dx system
              Par(5) = REAL(FanOpMode,r64) ! this does nothing since set point based control requires constant fan
              CALL SolveRegulaFalsi(Acc, MaxIte, SolFla, PartLoadFrac, DXHeatingCoilResidual, 0.0d0,   &
                                            1.0d0, Par)
              UnitarySystem(UnitarySysNum)%CompPartLoadRatio = PartLoadFrac

            CASE (CoilDX_MultiSpeedHeating, Coil_HeatingAirToAirVariableSpeed, Coil_HeatingWaterToAirHPVSEquationFit, &
                  Coil_HeatingElectric_MultiStage, Coil_HeatingGas_MultiStage)

              Par(1) = REAL(UnitarySystem(UnitarySysNum)%HeatingCoilIndex,r64)
              Par(2) = DesOutTemp
              Par(3) = UnitarySysNum
              ! Par(4) = CycRatio or SpeedRatio
              Par(5) = UnitarySystem(UnitarySysNum)%HeatingSpeedNum
              Par(6) = 1.0d0 ! UnitarySystem(UnitarySysNum)%FanOpMode
              Par(7) = 1.0d0 ! UnitarySystem(UnitarySysNum)%CompOp
              Par(8) = ReqOutput ! UnitarySystem(UnitarySysNum)%FanOpMode
              IF (SpeedRatio == 1.0d0) THEN
                Par(4) = CycRatio
                CALL SolveRegulaFalsi(Acc, MaxIte, SolFla, SpeedRatio, HeatingCoilVarSpeedResidual, 0.0d0,   &
                                                1.0d0, Par)
                UnitarySystem(UnitarySysNum)%HeatingCycRatio = CycRatio
                UnitarySystem(UnitarySysNum)%HeatingSpeedRatio = SpeedRatio
                UnitarySystem(UnitarySysNum)%HeatingPartLoadFrac = SpeedRatio
                CALL CalcPassiveSystem(UnitarySysNum, FirstHVACIteration)
                PartLoadFrac = SpeedRatio
              ELSE
                SpeedRatio = 0.0d0
                UnitarySystem(UnitarySysNum)%HeatingSpeedRatio = SpeedRatio
                Par(4) = SpeedRatio
                CALL SolveRegulaFalsi(Acc, MaxIte, SolFla, CycRatio, HeatingCoilVarSpeedCycResidual, 0.0d0,   &
                                                1.0d0, Par)
                UnitarySystem(UnitarySysNum)%HeatingCycRatio = CycRatio
                UnitarySystem(UnitarySysNum)%HeatingPartLoadFrac = CycRatio
                CALL CalcPassiveSystem(UnitarySysNum, FirstHVACIteration)
                PartLoadFrac = CycRatio
              END IF

            CASE (Coil_HeatingGas,Coil_HeatingElectric, Coil_HeatingDesuperheater)

              Par(1) = REAL(UnitarySysNum,r64)
              IF (FirstHVACIteration) THEN
                Par(2) = 1.0d0
              ELSE
                Par(2) = 0.0d0
              END IF
              Par(3) = DesOutTemp
              IF (SuppHeatingCoilFlag) THEN
                Par(4) = 1.0d0
              ELSE
                Par(4) = 0.0d0
              END IF
              Par(5) = FanOpMode
              CALL SolveRegulaFalsi(Acc, MaxIte, SolFla, PartLoadFrac, GasElecHeatingCoilResidual, 0.0d0,   &
                                                1.0d0, Par)

            CASE (Coil_HeatingWater)

              Par(1) = REAL(UnitarySysNum,r64)
              IF (FirstHVACIteration) THEN
                Par(2) = 1.0d0
              ELSE
                Par(2) = 0.0d0
              END IF
              Par(3) = DesOutTemp
              IF (SuppHeatingCoilFlag) THEN
                Par(4) = 1.0d0
              ELSE
                Par(4) = 0.0d0
              END IF
              Par(5) = 0.0d0
              CALL SolveRegulaFalsi(Acc, MaxIte, SolFla, PartLoadFrac, HotWaterHeatingCoilResidual, 0.0d0,   &
                                                1.0d0, Par)

            CASE (Coil_HeatingSteam)

              Par(1) = REAL(UnitarySysNum,r64)
              IF (FirstHVACIteration) THEN
                Par(2) = 1.0d0
              ELSE
                Par(2) = 0.0d0
              END IF
              Par(3) = DesOutTemp
              IF (SuppHeatingCoilFlag) THEN
                Par(4) = 1.0d0
              ELSE
                Par(4) = 0.0d0
              END IF

              CALL SolveRegulaFalsi(Acc, MaxIte, SolFla, PartLoadFrac, SteamHeatingCoilResidual, 0.0d0,   &
                                                1.0d0, Par)

            CASE(Coil_HeatingWaterToAirHPSimple, Coil_HeatingWaterToAirHP)

              Par(1) = REAL(UnitarySysNum,r64)
              IF (FirstHVACIteration) THEN
                Par(2) = 1.0d0
              ELSE
                Par(2) = 0.0d0
              END IF
              Par(3) = DesOutTemp
              Par(4) = ReqOutput

              CALL SolveRegulaFalsi(Acc, MaxIte, SolFla, PartLoadFrac, HeatWatertoAirHPTempResidual, 0.0d0,   &
                                            1.0d0, Par)

            CASE DEFAULT
              CALL ShowMessage(' For :'//TRIM(UnitarySystem(UnitarySysNum)%UnitarySystemType)//'="'// &
                                       TRIM(UnitarySystem(UnitarySysNum)%Name)//'"')
              CALL ShowFatalError('ControlHeatingSystem: Invalid heating coil type = '//  &
                                  TRIM(CALLCoilTypes(UnitarySystem(UnitarySysNum)%HeatingCoilType_Num)))

          END SELECT
        END IF
      END IF
    END IF
  END IF

  IF(PartLoadFrac .GT. 1.0d0) THEN
    PartLoadFrac = 1.0d0
  ELSEIF(PartLoadFrac < 0.0d0) THEN
    PartLoadFrac = 0.0d0
  END IF

  IF (SolFla == -1) THEN
    IF(.NOT. WarmupFlag)THEN
      IF(UnitarySystem(UnitarySysNum)%HeatCoilSensPLRIter .LT. 1)THEN
        UnitarySystem(UnitarySysNum)%HeatCoilSensPLRIter = UnitarySystem(UnitarySysNum)%HeatCoilSensPLRIter+1
        CALL ShowWarningError(TRIM(UnitarySystem(UnitarySysNum)%UnitarySystemType)// &
                                ' - Iteration limit exceeded calculating sensible '// &
                                'part-load ratio for unit = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
        CALL ShowContinueError('Estimated part-load ratio  = '//RoundSigDigits((ReqOutput/FullOutput),3))
        CALL ShowContinueError('Calculated part-load ratio = '//RoundSigDigits(PartLoadFrac,3))
        CALL ShowContinueErrorTimeStamp('The calculated part-load ratio will be used and the simulation'// &
                                        ' continues. Occurrence info: ')
      ELSE
        CALL ShowRecurringWarningErrorAtEnd(TRIM(UnitarySystem(UnitarySysNum)%UnitarySystemType)//' "'&
            //TRIM(UnitarySystem(UnitarySysNum)%Name)//'" - Iteration limit exceeded calculating'// &
            ' sensible part-load ratio error continues. Sensible PLR statistics follow.' &
            ,UnitarySystem(UnitarySysNum)%HeatCoilSensPLRIterIndex,PartLoadFrac,PartLoadFrac)
      END IF
    END IF
  ELSEIF (SolFla == -2) THEN
    PartLoadFrac = ReqOutput/FullOutput
    IF(.NOT. WarmupFlag)THEN
      IF(UnitarySystem(UnitarySysNum)%HeatCoilSensPLRFail .LT. 1)THEN
        UnitarySystem(UnitarySysNum)%HeatCoilSensPLRFail = UnitarySystem(UnitarySysNum)%HeatCoilSensPLRFail+1
        CALL ShowWarningError(TRIM(UnitarySystem(UnitarySysNum)%UnitarySystemType)//' - sensible part-'// &
                        'load ratio calculation failed: part-load ratio limits exceeded, for unit = '// &
                            TRIM(UnitarySystem(UnitarySysNum)%Name))
        CALL ShowContinueError('Estimated part-load ratio = '//RoundSigDigits(PartLoadFrac,3))
        CALL ShowContinueErrorTimeStamp('The estimated part-load ratio will be used and the simulation'// &
                                        ' continues. Occurrence info: ')
      ELSE
        CALL ShowRecurringWarningErrorAtEnd(TRIM(UnitarySystem(UnitarySysNum)%UnitarySystemType)//' "'&
            //TRIM(UnitarySystem(UnitarySysNum)%Name)//'" - sensible part-load ratio calculation'// &
            ' failed error continues. Sensible PLR statistics follow.' &
            ,UnitarySystem(UnitarySysNum)%HeatCoilSensPLRFailIndex,PartLoadFrac,PartLoadFrac)
      END IF
    END IF
  END IF

  !Set the final results
  UnitarySystem(UnitarySysNum)%HeatingPartLoadFrac = PartLoadFrac
  UnitarySystem(UnitarySysNum)%HeatingSpeedRatio = SpeedRatio
  UnitarySystem(UnitarySysNum)%HeatingCycRatio = CycRatio

  LoopHeatingCoilMaxRTF = MAX(LoopHeatingCoilMaxRTF, LoopHeatingCoilMaxRTFSave)
  LoopDXCoilRTF      = MAX(LoopDXCoilRTF, LoopDXCoilMaxRTFSave)

RETURN
END SUBROUTINE ControlHeatingSystem

SUBROUTINE ControlSuppHeatSystem(UnitarySysNum, FirstHVACIteration )
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Raustad, FSEC
          !       DATE WRITTEN   February 2013
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !  This subroutine updates the System outlet nodes.

          ! METHODOLOGY EMPLOYED:
          !  Data is moved from the System data structure to the System outlet nodes.

          ! REFERENCES:
          !  na

          ! USE STATEMENTS:
  USE DataAirLoop,     ONLY: LoopHeatingCoilMaxRTF, LoopDXCoilRTF
  USE Psychrometrics,  ONLY: PsyHFnTdbW, PsyTdpFnWPb
  USE General,         ONLY: SolveRegulaFalsi, RoundSigDigits
  USE HeatingCoils,    ONLY: SimulateHeatingCoilComponents
  USE WaterCoils,      ONLY: SimulateWaterCoilComponents
  USE SteamCoils,      ONLY: SimulateSteamCoilComponents
  USE PlantUtilities,  ONLY: SetComponentFlowRate


  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER,  INTENT(In)    :: UnitarySysNum           ! index to Unitary System
  LOGICAL,  INTENT(In)    :: FirstHVACIteration      ! First HVAC iteration flag

          ! SUBROUTINE PARAMETER DEFINITIONS:
  INTEGER, PARAMETER   :: MaxIte    = 500     ! Maximum number of iterations for solver
  REAL(r64), PARAMETER :: Acc       = 1.0d-3  ! Accuracy of solver result
  REAL(r64), PARAMETER :: HumRatAcc = 1.0d-6  ! Accuracy of solver result
  INTEGER, PARAMETER   :: SolveMaxIter=50

          ! INTERFACE BLOCK SPECIFICATIONS
          !  na

          ! DERIVED TYPE DEFINITIONS
          !  na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  CHARACTER(len=MaxNameLength)  :: CompName  ! Name of the heating coil
  INTEGER             :: CompIndex           ! Index to the heating coil
!  REAL(r64)           :: NoOutput            ! Sensible capacity (outlet - inlet) when the compressor is off
  REAL(r64)           :: FullOutput          ! Sensible capacity (outlet - inlet) when the compressor is on
  REAL(r64)           :: ReqOutput           ! Sensible capacity (outlet - inlet) required to meet load or set point temperature
  Integer             :: InletNode           ! Inlet node number of the DX cooling coil
  Integer             :: OutletNode          ! Outlet node number of the DX cooling coil
  Integer             :: ControlNode         ! The node number where a set point is placed to control the DX cooling coil
  REAL(r64)           :: PartLoadFrac        ! The part-load fraction of the compressor

  REAL(r64)           :: DesOutTemp          ! Desired outlet temperature of the DX cooling coil
  REAL(r64)           :: QCoilActual         ! Heating coil operating capacity [W]

  INTEGER             :: SolFla              ! Flag of solver, num iterations if >0, else error index
  REAL(r64), DIMENSION(5)  :: Par            ! Parameter array passed to solver
  LOGICAL             :: SensibleLoad        ! True if there is a sensible cooling load on this system
  INTEGER             :: FanOpMode           ! Supply air fan operating mode
  REAL(R64)           :: LoopHeatingCoilMaxRTFSave ! Used to find RTF of heating coils without overwriting globabl variable
  REAL(R64)           :: LoopDXCoilMaxRTFSave ! Used to find RTF of DX heating coils without overwriting globabl variable
  LOGICAL             :: SuppHeatingCoilFlag = .TRUE.
  REAL(r64)           :: NoLoadTempOut       ! save outlet temp when coil is off (C)

      ! Set local variables
      ! Retrieve the load on the controlled zone

  OutletNode   = UnitarySystem(UnitarySysNum)%SuppCoilAirOutletNode
  InletNode    = UnitarySystem(UnitarySysNum)%SuppCoilAirInletNode
  ControlNode  = UnitarySystem(UnitarySysNum)%SuppCoilAirOutletNode
  DesOutTemp   = UnitarySystem(UnitarySysNum)%DesiredOutletTemp
  CompName     = UnitarySystem(UnitarySysNum)%SuppHeatCoilName
  CompIndex    = UnitarySystem(UnitarySysNum)%SuppHeatCoilIndex
  FanOpMode    = UnitarySystem(UnitarySysNum)%FanOpMode
  SolFla       = 0.0d0

  PartLoadFrac = 0.0d0

  SensibleLoad = .FALSE.

  LoopHeatingCoilMaxRTFSave=LoopHeatingCoilMaxRTF
  LoopHeatingCoilMaxRTF = 0.0d0
  LoopDXCoilMaxRTFSave=LoopDXCoilRTF
  LoopDXCoilRTF=0.0d0

  IF((GetCurrentScheduleValue(UnitarySystem(UnitarySysNum)%SysAvailSchedPtr) > 0.0d0) .AND. &
     (Node(InletNode)%MassFlowRate .gt. MinAirMassFlow)) THEN

    ! Determine if there is a sensible load on this system
    IF((Node(InletNode)%Temp < DesOutTemp) .AND. &
       (ABS(Node(InletNode)%Temp - DesOutTemp) .gt. TempControlTol) ) SensibleLoad = .TRUE.

    IF (SensibleLoad ) THEN

      ReqOutput = Node(InletNode)%MassFlowRate *  &
                       (PsyHFnTdbW(UnitarySystem(UnitarySysNum)%DesiredOutletTemp,Node(InletNode)%HumRat) - &
                        PsyHFnTdbW(Node(InletNode)%Temp,Node(InletNode)%HumRat))

      ! Get no load result
      PartLoadFrac = 0.0d0

      SELECT CASE(UnitarySystem(UnitarySysNum)%SuppHeatCoilType_Num)

        CASE (Coil_HeatingGas,Coil_HeatingElectric,Coil_HeatingDesuperheater )
          CALL SimulateHeatingCoilComponents(CompName,FirstHVACIteration, &
                                CompIndex=CompIndex, PartLoadRatio = PartLoadFrac, & ! QCoilReq= 0.0d0,  &
                                SuppHeat=SuppHeatingCoilFlag,FanOpMode=FanOpMode,QCoilActual=QCoilActual)
          PartLoadFrac=QCoilActual/UnitarySystem(UnitarySysNum)%DesignSuppHeatingCapacity

        CASE (Coil_HeatingWater)

          CALL SimWaterCoils(UnitarySysNum, FirstHVACIteration, PartLoadFrac, SuppHeatCoil)

        CASE (Coil_HeatingSteam)

          CALL SimSteamCoils(UnitarySysNum, FirstHVACIteration, PartLoadFrac, SuppHeatCoil)

        CASE DEFAULT

      END SELECT


      NoLoadTempOut = Node(OutletNode)%Temp
!      NoOutput = Node(InletNode)%MassFlowRate *  &
!                       (PsyHFnTdbW(NoLoadTempOut,Node(OutletNode)%HumRat)  &
!                        - PsyHFnTdbW(Node(InletNode)%Temp,Node(OutletNode)%HumRat))

!     If OutletTemp is within ACC of set point, either coil operated or is not needed
      IF(ABS(Node(OutletNode)%Temp-DesOutTemp) < Acc)THEN
        ! do nothing, coil is at set point (i.e., gas/elec/steam coil will try to hit set point
      ELSE IF(PartLoadFrac .GT. 0.0d0)THEN
        ! do nothing, coil tried to hit set point (i.e., gas/elec/steam coil tried to hit set point but missed
      ELSE IF (NoLoadTempOut .GT. (DesOutTemp - Acc)) THEN
        PartLoadFrac = 0.0d0 ! outlet temp > set point, coil is not needed
      ELSE ! outlet temp too low, turn on coil

        ! Get full load result
        PartLoadFrac = 1.0d0

        SELECT CASE(UnitarySystem(UnitarySysNum)%SuppHeatCoilType_Num)

          CASE (Coil_HeatingGas,Coil_HeatingElectric)

            CALL SimulateHeatingCoilComponents(CompName,FirstHVACIteration, &
!                                  CompIndex=CompIndex, QCoilReq= UnitarySystem(UnitarySysNum)%DesignSuppHeatingCapacity,  &
                                  CompIndex=CompIndex, PartLoadRatio = PartLoadFrac, &
                                  SuppHeat=SuppHeatingCoilFlag, FanOpMode=FanOpMode,QCoilActual=QCoilActual)
            PartLoadFrac=QCoilActual/UnitarySystem(UnitarySysNum)%DesignSuppHeatingCapacity

          CASE (Coil_HeatingDesuperheater )

            CALL SimulateHeatingCoilComponents(CompName,FirstHVACIteration, &
                                  CompIndex=CompIndex, QCoilReq= ReqOutput,  &
                                  SuppHeat=SuppHeatingCoilFlag, FanOpMode=FanOpMode)

          CASE (Coil_HeatingWater)

            CALL SimWaterCoils(UnitarySysNum, FirstHVACIteration, PartLoadFrac, SuppHeatCoil)

          CASE (Coil_HeatingSteam)

            CALL SimSteamCoils(UnitarySysNum, FirstHVACIteration, PartLoadFrac, SuppHeatCoil)

          CASE DEFAULT

        END SELECT

        FullOutput = Node(InletNode)%MassFlowRate *  &
                       (PsyHFnTdbW(Node(OutletNode)%Temp,Node(InletNode)%HumRat)  &
                        - PsyHFnTdbW(Node(InletNode)%Temp,Node(InletNode)%HumRat))

!         If the FullOutput outlet temp is less than (insufficient heating) or very near set point,
!         run the coil at PartLoadFrac = 1.
        IF (Node(OutletNode)%Temp .LT. (DesOutTemp+Acc)) THEN
          PartLoadFrac = 1.0d0
        ELSE

          SELECT CASE(UnitarySystem(UnitarySysNum)%SuppHeatCoilType_Num)

            CASE (Coil_HeatingGas,Coil_HeatingElectric, Coil_HeatingDesuperheater)

              Par(1) = REAL(UnitarySysNum,r64)
              IF (FirstHVACIteration) THEN
                Par(2) = 1.0d0
              ELSE
                Par(2) = 0.0d0
              END IF
              Par(3) = DesOutTemp
              IF (SuppHeatingCoilFlag) THEN
                Par(4) = 1.0d0
              ELSE
                Par(4) = 0.0d0
              END IF
              Par(5) = FanOpMode
              CALL SolveRegulaFalsi(Acc, MaxIte, SolFla, PartLoadFrac, GasElecHeatingCoilResidual, 0.0d0,   &
                                                1.0d0, Par)

            CASE (Coil_HeatingWater)

               Par(1) = REAL(UnitarySysNum,r64)
               IF (FirstHVACIteration) THEN
                 Par(2) = 1.0d0
               ELSE
                 Par(2) = 0.0d0
               END IF
               Par(3) = DesOutTemp
               IF (SuppHeatingCoilFlag) THEN
                 Par(4) = 1.0d0
               ELSE
                 Par(4) = 0.0d0
               END IF
               Par(5)=0.0d0
               CALL SolveRegulaFalsi(Acc, SolveMaxIter, SolFla, PartLoadFrac, HotWaterHeatingCoilResidual, &
                                     0.0d0, 1.0d0, Par)

            CASE (Coil_HeatingSteam)

              Par(1) = REAL(UnitarySysNum,r64)
              IF (FirstHVACIteration) THEN
                Par(2) = 1.0d0
              ELSE
                Par(2) = 0.0d0
              END IF
              Par(3) = DesOutTemp
              IF (SuppHeatingCoilFlag) THEN
                Par(4) = 1.0d0
              ELSE
                Par(4) = 0.0d0
              END IF

              CALL SolveRegulaFalsi(Acc, MaxIte, SolFla, PartLoadFrac, SteamHeatingCoilResidual, 0.0d0,   &
                                                1.0d0, Par)

            CASE DEFAULT

          END SELECT

        END IF   ! IF ((FullOutput - ReqOutput) < Acc) THEN
      END IF     ! IF ((NoOutput-ReqOutput) > Acc) THEN
    END IF       ! IF (SensibleLoad ) THEN
  END IF         ! IF((GetCurrentScheduleValue(UnitarySystem(UnitarySysNum)%SysAvailSchedPtr) > 0.0d0) .AND. &

  IF(PartLoadFrac .GT. 1.0d0) THEN
    PartLoadFrac = 1.0d0
  ELSEIF(PartLoadFrac < 0.0d0) THEN
    PartLoadFrac = 0.0d0
  END IF

  IF (SolFla == -1) THEN
    IF(.NOT. WarmupFlag)THEN
      IF(UnitarySystem(UnitarySysNum)%SuppHeatCoilSensPLRIter .LT. 1)THEN
        UnitarySystem(UnitarySysNum)%SuppHeatCoilSensPLRIter = UnitarySystem(UnitarySysNum)%SuppHeatCoilSensPLRIter+1
        CALL ShowWarningError(TRIM(UnitarySystem(UnitarySysNum)%UnitarySystemType)// &
                              ' - Iteration limit exceeded calculating sensible '// &
                              'part-load ratio for unit = '//TRIM(UnitarySystem(UnitarySysNum)%Name))
        CALL ShowContinueError('Estimated part-load ratio  = '//RoundSigDigits((ReqOutput/FullOutput),3))
        CALL ShowContinueError('Calculated part-load ratio = '//RoundSigDigits(PartLoadFrac,3))
        CALL ShowContinueErrorTimeStamp('The calculated part-load ratio will be used and the simulation'// &
                                      ' continues. Occurrence info: ')
      ELSE
        CALL ShowRecurringWarningErrorAtEnd(TRIM(UnitarySystem(UnitarySysNum)%UnitarySystemType)//' "'&
        //TRIM(UnitarySystem(UnitarySysNum)%Name)//'" - Iteration limit exceeded calculating'// &
        ' sensible part-load ratio error continues. Sensible PLR statistics follow.' &
        ,UnitarySystem(UnitarySysNum)%SuppHeatCoilSensPLRIterIndex,PartLoadFrac,PartLoadFrac)
      END IF
    END IF  ! IF(.NOT. WarmupFlag)THEN
  ELSEIF (SolFla == -2) THEN
    PartLoadFrac = ReqOutput/FullOutput
    IF(.NOT. WarmupFlag)THEN
      IF(UnitarySystem(UnitarySysNum)%SuppHeatCoilSensPLRFail .LT. 1)THEN
        UnitarySystem(UnitarySysNum)%SuppHeatCoilSensPLRFail = UnitarySystem(UnitarySysNum)%SuppHeatCoilSensPLRFail+1
        CALL ShowWarningError(TRIM(UnitarySystem(UnitarySysNum)%UnitarySystemType)//' - sensible part-'// &
                              'load ratio calculation failed: part-load ratio limits exceeded, for unit = '// &
                              TRIM(UnitarySystem(UnitarySysNum)%Name))
        CALL ShowContinueError('Estimated part-load ratio = '//RoundSigDigits(PartLoadFrac,3))
        CALL ShowContinueErrorTimeStamp('The estimated part-load ratio will be used and the simulation'// &
                                        ' continues. Occurrence info: ')
      ELSE
        CALL ShowRecurringWarningErrorAtEnd(TRIM(UnitarySystem(UnitarySysNum)%UnitarySystemType)//' "'&
             //TRIM(UnitarySystem(UnitarySysNum)%Name)//'" - sensible part-load ratio calculation'// &
             ' failed error continues. Sensible PLR statistics follow.' &
             ,UnitarySystem(UnitarySysNum)%SuppHeatCoilSensPLRFailIndex,PartLoadFrac,PartLoadFrac)
      END IF
    END IF  ! IF(.NOT. WarmupFlag)THEN
  END IF  ! IF (SolFla == -1) THEN

  UnitarySystem(UnitarySysNum)%SuppHeatPartLoadFrac = PartLoadFrac

  LoopHeatingCoilMaxRTF = MAX(LoopHeatingCoilMaxRTF, LoopHeatingCoilMaxRTFSave)
  LoopDXCoilRTF      = MAX(LoopDXCoilRTF, LoopDXCoilMaxRTFSave)

RETURN
END SUBROUTINE ControlSuppHeatSystem

SUBROUTINE SimWaterCoils(UnitarySysNum, FirstHVACIteration,PartLoadFrac, CoilType)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Chandan Sharma, FSEC
          !       DATE WRITTEN   March 2013
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine manages water cooling/heating coil simulation.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE WaterCoils,       ONLY: SimulateWaterCoilComponents

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)          :: UnitarySysNum    ! Index of AirloopHVAC:UnitarySystem object
  INTEGER, INTENT(IN)          :: CoilType
  LOGICAL, INTENT(IN)          :: FirstHVACIteration  ! True when first HVAC iteration
  REAL(R64), INTENT(IN)        :: PartLoadFrac
          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  CHARACTER(len=MaxNameLength)  :: CompName              ! Name of Unitary System object
  REAL(R64)                     :: mdot

 IF (CoilType == CoolingCoil) THEN
    CompName     = UnitarySystem(UnitarySysNum)%CoolingCoilName

    mdot = MIN(Node(UnitarySystem(UnitarySysNum)%CoolCoilFluidOutletNodeNum)%MassFlowRateMaxAvail, &
           UnitarySystem(UnitarySysNum)%MaxCoolCoilFluidFlow * PartLoadFrac)
    Node(UnitarySystem(UnitarySysNum)%CoolCoilFluidInletNode)%MassFlowRate = mdot

    CALL SimulateWaterCoilComponents(CompName,FirstHVACIteration,UnitarySystem(UnitarySysNum)%CoolingCoilIndex, &
                                    FanOpMode = UnitarySystem(UnitarySysNum)%FanOpMode, &
                                    PartLoadRatio = PartLoadFrac)

 ELSEIF (CoilType == HeatingCoil) THEN
    CompName     = UnitarySystem(UnitarySysNum)%HeatingCoilName

    mdot = MIN(Node(UnitarySystem(UnitarySysNum)%HeatCoilFluidOutletNodeNum)%MassFlowRateMaxAvail, &
           UnitarySystem(UnitarySysNum)%MaxHeatCoilFluidFlow * PartLoadFrac)
    Node(UnitarySystem(UnitarySysNum)%HeatCoilFluidInletNode)%MassFlowRate = mdot

    CALL SimulateWaterCoilComponents(CompName,FirstHVACIteration,UnitarySystem(UnitarySysNum)%HeatingCoilIndex, &
                                    FanOpMode = UnitarySystem(UnitarySysNum)%FanOpMode, &
                                    PartLoadRatio = PartLoadFrac)

 ELSE
    CompName     = UnitarySystem(UnitarySysNum)%SuppHeatCoilName

    mdot = MIN(Node(UnitarySystem(UnitarySysNum)%SuppCoilFluidOutletNodeNum)%MassFlowRateMaxAvail, &
           UnitarySystem(UnitarySysNum)%MaxSuppCoilFluidFlow * PartLoadFrac)
    Node(UnitarySystem(UnitarySysNum)%SuppCoilFluidInletNode)%MassFlowRate = mdot

    CALL SimulateWaterCoilComponents(CompName,FirstHVACIteration,UnitarySystem(UnitarySysNum)%SuppHeatCoilIndex, &
                                    FanOpMode = UnitarySystem(UnitarySysNum)%FanOpMode, &
                                    PartLoadRatio = PartLoadFrac)
 END IF

END SUBROUTINE SimWaterCoils

SUBROUTINE SimSteamCoils(UnitarySysNum, FirstHVACIteration,PartLoadFrac, CoilType)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Chandan Sharma, FSEC
          !       DATE WRITTEN   March 2013
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine manages steam heating coil simulation.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE SteamCoils,       ONLY: SimulateSteamCoilComponents

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)          :: UnitarySysNum    ! Index of AirloopHVAC:UnitarySystem object
  INTEGER, INTENT(IN)          :: CoilType
  LOGICAL, INTENT(IN)          :: FirstHVACIteration  ! True when first HVAC iteration
  REAL(R64), INTENT(IN)        :: PartLoadFrac
          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  CHARACTER(len=MaxNameLength)  :: CompName              ! Name of Unitary System object
  REAL(R64)                     :: mdot

  IF (CoilType == HeatingCoil) THEN

    CompName     = UnitarySystem(UnitarySysNum)%HeatingCoilName

    mdot = MIN(Node(UnitarySystem(UnitarySysNum)%HeatCoilFluidOutletNodeNum)%MassFlowRateMaxAvail, &
           UnitarySystem(UnitarySysNum)%MaxHeatCoilFluidFlow * PartLoadFrac)
    Node(UnitarySystem(UnitarySysNum)%HeatCoilFluidInletNode)%MassFlowRate = mdot

    CALL SimulateSteamCoilComponents(CompName, FirstHVACIteration, &
                                           1.0d0, & !QCoilReq, simulate any load > 0 to get max capacity
                                           UnitarySystem(UnitarySysNum)%HeatingCoilIndex, &
                                           FanOpMode = UnitarySystem(UnitarySysNum)%FanOpMode, &
                                           PartLoadRatio = PartLoadFrac)

  ELSE

    CompName     = UnitarySystem(UnitarySysNum)%SuppHeatCoilName

    mdot = MIN(Node(UnitarySystem(UnitarySysNum)%SuppCoilFluidOutletNodeNum)%MassFlowRateMaxAvail, &
           UnitarySystem(UnitarySysNum)%MaxSuppCoilFluidFlow * PartLoadFrac)
    Node(UnitarySystem(UnitarySysNum)%SuppCoilFluidInletNode)%MassFlowRate = mdot

    CALL SimulateSteamCoilComponents(CompName, FirstHVACIteration, &
                                           1.0d0, & !QCoilReq, simulate any load > 0 to get max capacity
                                           UnitarySystem(UnitarySysNum)%SuppHeatCoilIndex, &
                                           FanOpMode = UnitarySystem(UnitarySysNum)%FanOpMode, &
                                           PartLoadRatio = PartLoadFrac)

  END IF
RETURN

END SUBROUTINE SimSteamCoils

SUBROUTINE SimMultiSpeedCoils(UnitarySysNum, FirstHVACIteration, SensibleLoad, LatentLoad, PartLoadFrac, CoilType, SpeedNumber)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Chandan Sharma, FSEC
          !       DATE WRITTEN   March 2013
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine manages multispeed and variable speed cooling coil simulation.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DXCoils,            ONLY: SimDXCoilMultiSpeed, DXCoilOutletTemp
  USE VariableSpeedCoils, ONLY: SimVariableSpeedCoils
  USE HeatingCoils,       ONLY: SimulateHeatingCoilComponents

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)           :: UnitarySysNum    ! Index of AirloopHVAC:UnitarySystem object
  LOGICAL, INTENT(IN)           :: FirstHVACIteration  ! True when first HVAC iteration
  LOGICAL, INTENT(IN)           :: SensibleLoad
  LOGICAL, INTENT(IN)           :: LatentLoad
  INTEGER, INTENT(IN)           :: CoilType
  REAL(R64), INTENT(IN)         :: PartLoadFrac
  INTEGER, OPTIONAL,INTENT(IN)  :: SpeedNumber

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  CHARACTER(len=MaxNameLength)  :: CompName              ! Name of Unitary System object
  REAL(R64)                     :: Dummy
  REAL(R64)                     :: SensLoad
  REAL(R64)                     :: LatLoad
  REAL(R64)                     :: OnOffAirFlowRatio
  INTEGER                       :: CoilTypeNum
  INTEGER                       :: SpeedNum
  INTEGER                       :: CoilOutletNodeNum
  INTEGER                       :: CompIndex
  LOGICAL                       :: CoolLoad
  LOGICAL                       :: HeatLoad

  Dummy = 0.0d0

  IF(PRESENT(SpeedNumber))THEN
    SpeedNum = SpeedNumber
  ELSE
    SpeedNum = 1
  END IF

  IF (CoilType == CoolingCoil) THEN

    CompName     = UnitarySystem(UnitarySysNum)%CoolingCoilName
    CompIndex    = UnitarySystem(UnitarySysNum)%CoolingCoilIndex
    CoilTypeNum  = UnitarySystem(UnitarySysNum)%CoolingCoilType_Num
    CoilOutletNodeNum = UnitarySystem(UnitarySysNum)%CoolCoilOutletNodeNum
    IF(SensibleLoad)THEN
      SensLoad = -1.0d0
      CoolLoad = .TRUE.
      HeatLoad = .FALSE.
    END IF
    IF(LatentLoad)LatLoad = -1.0d0

  ELSE

    CompName     = UnitarySystem(UnitarySysNum)%HeatingCoilName
    CompIndex    = UnitarySystem(UnitarySysNum)%HeatingCoilIndex
    CoilTypeNum  = UnitarySystem(UnitarySysNum)%HeatingCoilType_Num
    CoilOutletNodeNum = UnitarySystem(UnitarySysNum)%HeatCoilOutletNodeNum

    IF(SensibleLoad)THEN
      SensLoad = 1.0d0
      CoolLoad = .FALSE.
      HeatLoad = .TRUE.
    ELSE
      SensLoad = 0.0d0
      HeatLoad = .FALSE.
    END IF
    LatLoad = 0.0d0
    UnitarySystem(UnitarySysNum)%FanOpMode = 1

  END IF

  OnOffAirFlowRatio = 1.0d0
  CALL SetOnOffMassFlowRate(UnitarySysNum, OnOffAirFlowRatio, PartLoadFrac) !1.0d0 = PartLoadRatio

  CALL CalcPassiveSystem(UnitarySysNum, FirstHVACIteration)

  SELECT CASE(CoilTypeNum)

    CASE(CoilDX_MultiSpeedCooling, CoilDX_MultiSpeedHeating)

      CALL SimDXCoilMultiSpeed(CompName,0.0d0,PartLoadFrac,CompIndex,SpeedNum,UnitarySystem(UnitarySysNum)%FanOpMode,1)

    CASE(Coil_CoolingAirToAirVariableSpeed)

       CALL SimVariableSpeedCoils(CompName,CompIndex, &
                   UnitarySystem(UnitarySysNum)%FanOpMode,UnitarySystem(UnitarySysNum)%MaxONOFFCyclesperHour, &
                   UnitarySystem(UnitarySysNum)%HPTimeConstant,UnitarySystem(UnitarySysNum)%FanDelayTime, &
                   1, PartLoadFrac, OnOffAirFlowRatio,SpeedNum, 0.0d0,UnitarySystem(UnitarySysNum)%CoolingCoilSensDemand,Dummy )

    CASE(Coil_HeatingAirToAirVariableSpeed)

       CALL SimVariableSpeedCoils(CompName,CompIndex, &
                   UnitarySystem(UnitarySysNum)%FanOpMode,UnitarySystem(UnitarySysNum)%MaxONOFFCyclesperHour, &
                   UnitarySystem(UnitarySysNum)%HPTimeConstant,UnitarySystem(UnitarySysNum)%FanDelayTime, &
                   1, PartLoadFrac, OnOffAirFlowRatio,SpeedNum, 0.0d0,UnitarySystem(UnitarySysNum)%HeatingCoilSensDemand,Dummy )

    CASE(Coil_CoolingWaterToAirHPVSEquationFit)

      CALL SimVariableSpeedCoils(CompName,CompIndex, &
                   UnitarySystem(UnitarySysNum)%FanOpMode,UnitarySystem(UnitarySysNum)%MaxONOFFCyclesperHour, &
                   UnitarySystem(UnitarySysNum)%HPTimeConstant,UnitarySystem(UnitarySysNum)%FanDelayTime, &
                   1, PartLoadFrac, OnOffAirFlowRatio,SpeedNum, 0.0d0,UnitarySystem(UnitarySysNum)%CoolingCoilSensDemand,Dummy )

    CASE(Coil_HeatingWaterToAirHPVSEquationFit)

      CALL SimVariableSpeedCoils(CompName,CompIndex, &
                   UnitarySystem(UnitarySysNum)%FanOpMode,UnitarySystem(UnitarySysNum)%MaxONOFFCyclesperHour, &
                   UnitarySystem(UnitarySysNum)%HPTimeConstant,UnitarySystem(UnitarySysNum)%FanDelayTime, &
                   1, PartLoadFrac, OnOffAirFlowRatio,SpeedNum, 0.0d0,UnitarySystem(UnitarySysNum)%HeatingCoilSensDemand,Dummy )

    CASE(Coil_HeatingElectric_MultiStage, Coil_HeatingGas_MultiStage)

      CALL SimulateHeatingCoilComponents(CompName,FirstHVACIteration, &
                                         CompIndex     = CompIndex, &
                                         FanOpMode     = UnitarySystem(UnitarySysNum)%FanOpMode,      &
                                         PartLoadRatio = PartLoadFrac,  &
                                         StageNum      = SpeedNum,     &
                                         SpeedRatio    = 0.0d0)
    CASE DEFAULT

  END SELECT

  RETURN

END SUBROUTINE SimMultiSpeedCoils

SUBROUTINE CalcPassiveSystem(UnitarySysNum, FirstHVACIteration)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Raustad, FSEC
          !       DATE WRITTEN   February 2013
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine calculates the set point based output of the unitary system.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE Fans,             ONLY: SimulateFanComponents

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)          :: UnitarySysNum       ! Index of AirloopHVAC:UnitarySystem object
  LOGICAL, INTENT(IN)          :: FirstHVACIteration  ! True when first HVAC iteration
          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(R64)                     :: PartLoadRatio         ! coil operating part-load ratio
  REAL(R64)                     :: OnOffAirFlowRatio     ! Setpoint based coil control does not use this variable
  REAL(R64)                     :: CoilCoolHeatRat       ! ratio of cooling to heating PLR for cycling fan RH control
  REAL(R64)                     :: QZnReq
  INTEGER                       :: CompOn                ! compressor control (0=off, 1=on)
  LOGICAL                       :: HXUnitOn

  OnOffAirFlowRatio = 1.0d0
  CoilCoolHeatRat = 1.0d0
  QZnReq = 0.0d0
  !CALL the series of components that simulate a Unitary System
  IF (UnitarySystem(UnitarySysNum)%FanExists .AND. UnitarySystem(UnitarySysNum)%FanPlace .EQ. BlowThru) THEN
    CALL SimulateFanComponents(Blank,FirstHVACIteration,UnitarySystem(UnitarySysNum)%FanIndex,FanSpeedRatio)
  END IF

  IF(UnitarySystem(UnitarySysNum)%CoolingCoilUpstream)THEN

    IF (UnitarySystem(UnitarySysNum)%CoolCoilExists)THEN
      PartLoadRatio = UnitarySystem(UnitarySysNum)%CoolingPartLoadFrac
      CompOn = 0
      IF(PartLoadRatio > 0.0d0)CompOn = 1
      HXUnitOn = .FALSE.
      CALL CalcUnitaryCoolingSystem(UnitarySysNum,FirstHVACIteration,PartLoadRatio, &
                                    CompOn,OnOffAirFlowRatio,CoilCoolHeatRat,HXUnitOn)
    END IF
    IF (UnitarySystem(UnitarySysNum)%HeatCoilExists)THEN
      PartLoadRatio = UnitarySystem(UnitarySysNum)%HeatingPartLoadFrac
      CompOn = 0
      IF(PartLoadRatio > 0.0d0)CompOn = 1
      CALL CalcUnitaryHeatingSystem(UnitarySysNum,FirstHVACIteration,PartLoadRatio, CompOn, OnOffAirFlowRatio)
    END IF

  ELSE

    IF (UnitarySystem(UnitarySysNum)%HeatCoilExists)THEN
      PartLoadRatio = UnitarySystem(UnitarySysNum)%HeatingPartLoadFrac
      CompOn = 0
      IF(PartLoadRatio > 0.0d0)CompOn = 1
      CALL CalcUnitaryHeatingSystem(UnitarySysNum,FirstHVACIteration,PartLoadRatio, CompOn, OnOffAirFlowRatio)
    END IF
    IF (UnitarySystem(UnitarySysNum)%CoolCoilExists)THEN
      PartLoadRatio = UnitarySystem(UnitarySysNum)%CoolingPartLoadFrac
      CompOn = 0
      IF(PartLoadRatio > 0.0d0)CompOn = 1
      HXUnitOn = .FALSE.
      CALL CalcUnitaryCoolingSystem(UnitarySysNum,FirstHVACIteration,PartLoadRatio, &
                                    CompOn,OnOffAirFlowRatio,CoilCoolHeatRat,HXUnitOn)
    END IF

  END IF

  IF (UnitarySystem(UnitarySysNum)%FanExists .AND. UnitarySystem(UnitarySysNum)%FanPlace .EQ. DrawThru) THEN
    CALL SimulateFanComponents(Blank,FirstHVACIteration,UnitarySystem(UnitarySysNum)%FanIndex,FanSpeedRatio)
  END IF

! CALL reheat coils next
  IF (UnitarySystem(UnitarySysNum)%SuppCoilExists)THEN
    SuppHeatingCoilFlag = .TRUE.
    CALL CalcUnitarySuppSystemToSP(UnitarySysNum,FirstHVACIteration)
    SuppHeatingCoilFlag = .FALSE.
  END IF

  RETURN

END SUBROUTINE CalcPassiveSystem

SUBROUTINE SetOnOffMassFlowRate(UnitarySysNum, OnOffAirFlowRatio, PartLoadRatio)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Chandan Sharma
          !       DATE WRITTEN   May 2013
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is for initializations of the components.

          ! METHODOLOGY EMPLOYED:
          ! The unitarysystem may have alternate air flow rates
          ! in cooling, heating, and when no cooling or heating is needed. Set up the coil (comp) ON and OFF
          ! air flow rates. Use these flow rates during the Calc routines to set the average mass flow rates
          ! based on PLR.

          ! REFERENCES:
          ! Based on SetOnOffMassFlowRate by Richard Raustad

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER,   INTENT(IN)    :: UnitarySysNum     ! index to unitary system
  REAL(r64), INTENT(INOUT) :: OnOffAirFlowRatio ! ratio of coil on to coil off air flow rate
  REAL(r64), INTENT(IN)    :: PartLoadRatio     ! coil part-load ratio

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
 INTEGER   :: HeatSpeedNum
 INTEGER   :: CoolSpeedNum
 REAL(R64) :: SpeedRatio

CompOffMassFlow  = 0.0d0
CompOffFlowRatio = 0.0d0

! Set the compressor or coil ON mass flow rate
IF ( HeatingLoad ) THEN

  UnitarySystem(UnitarySysNum)%LastMode = HeatingMode

  IF(MultiOrVarSpeedHeatCoil(UnitarySysNum)) THEN

    HeatSpeedNum = UnitarySystem(UnitarySysNum)%HeatingSpeedNum

    IF (HeatSpeedNum .EQ. 0) THEN
      CompOnMassFlow  = UnitarySystem(UnitarySysNum)%IdleMassFlowRate
      CompOnFlowRatio = UnitarySystem(UnitarySysNum)%IdleSpeedRatio
    ELSE IF (HeatSpeedNum .EQ. 1) THEN
      CompOnMassFlow  = UnitarySystem(UnitarySysNum)%HeatMassFlowRate(1)
      CompOnFlowRatio = UnitarySystem(UnitarySysNum)%MSHeatingSpeedRatio(1)
      MSHPMassFlowRateLow  = UnitarySystem(UnitarySysNum)%HeatMassFlowRate(1)
      MSHPMassFlowRateHigh = UnitarySystem(UnitarySysNum)%HeatMassFlowRate(1)
    ELSEIF (HeatSpeedNum .GT. 1) THEN
      SpeedRatio       = UnitarySystem(UnitarySysNum)%HeatingSpeedRatio

      CompOnMassFlow   = SpeedRatio*UnitarySystem(UnitarySysNum)%HeatMassFlowRate(HeatSpeedNum) + &
                         (1.0d0-SpeedRatio)*UnitarySystem(UnitarySysNum)%HeatMassFlowRate(HeatSpeedNum-1)
      CompOnFlowRatio  = SpeedRatio*UnitarySystem(UnitarySysNum)%MSHeatingSpeedRatio(HeatSpeedNum) + &
                         (1.0d0-SpeedRatio)*UnitarySystem(UnitarySysNum)%MSHeatingSpeedRatio(HeatSpeedNum-1)
      MSHPMassFlowRateHigh = UnitarySystem(UnitarySysNum)%HeatMassFlowRate(HeatSpeedNum)
      MSHPMassFlowRateLow  = UnitarySystem(UnitarySysNum)%HeatMassFlowRate(HeatSpeedNum-1)
    END IF
    ! Set the compressor or coil OFF mass flow rate based on LOGICAL flag
    ! UseCompressorOnFlow is used when the user does not enter a value for no cooling or heating flow rate
    IF(UnitarySystem(UnitarySysNum)%FanOpMode == ContFanCycCoil)THEN
      IF(MoistureLoad .LT. 0.0d0 .AND. UnitarySystem(UnitarySysNum)%Humidistat .AND. &
         UnitarySystem(UnitarySysNum)%DehumidControlType_Num == DehumidControl_CoolReheat)THEN
        IF (MultiOrVarSpeedCoolCoil(UnitarySysNum)) THEN
          CoolSpeedNum = UnitarySystem(UnitarySysNum)%CoolingSpeedNum
          IF(CoolSpeedNum .LT. 1)THEN
            CompOffMassFlow  = UnitarySystem(UnitarySysNum)%IdleMassFlowRate
            CompOffFlowRatio = UnitarySystem(UnitarySysNum)%IdleSpeedRatio
          ELSEIF(CoolSpeedNum == 1) THEN
            CompOffMassFlow  = UnitarySystem(UnitarySysNum)%CoolMassFlowRate(1)
            CompOffFlowRatio = UnitarySystem(UnitarySysNum)%MSCoolingSpeedRatio(1)
          ELSEIF(CoolSpeedNum > 1) THEN
            CompOffMassFlow  = UnitarySystem(UnitarySysNum)%CoolMassFlowRate(CoolSpeedNum-1)
            CompOffFlowRatio = UnitarySystem(UnitarySysNum)%MSCoolingSpeedRatio(CoolSpeedNum-1)
          END IF
        ELSE
          CompOffMassFlow  = UnitarySystem(UnitarySysNum)%MaxCoolAirMassFlow
          CompOffFlowRatio = UnitarySystem(UnitarySysNum)%CoolingFanSpeedRatio
        END IF
      ELSE
!        IF (UnitarySystem(UnitarySysNum)%AirFlowControl .EQ. UseCompressorOnFlow) THEN
          IF(HeatSpeedNum <= 1)THEN
            CompOffMassFlow  = UnitarySystem(UnitarySysNum)%IdleMassFlowRate
            CompOffFlowRatio = UnitarySystem(UnitarySysNum)%IdleSpeedRatio
          ELSE
            CompOffMassFlow  = UnitarySystem(UnitarySysNum)%HeatMassFlowRate(HeatSpeedNum-1)
            CompOffFlowRatio = UnitarySystem(UnitarySysNum)%MSHeatingSpeedRatio(HeatSpeedNum-1)
          END IF
      END IF
    END IF
  ELSE   ! IF(MultiOrVarSpeedHeatCoil) THEN
!   If a heating and moisture load exists, operate at the cooling mass flow rate ELSE operate at the heating flow rate
    IF(MoistureLoad .LT. 0.0d0 .AND. UnitarySystem(UnitarySysNum)%Humidistat .AND. &
       UnitarySystem(UnitarySysNum)%DehumidControlType_Num == DehumidControl_CoolReheat .AND. &
       .NOT. UnitarySystem(UnitarySysNum)%DXHeatingCoil)THEN
      IF (MultiOrVarSpeedCoolCoil(UnitarySysNum)) THEN
        CoolSpeedNum = UnitarySystem(UnitarySysNum)%CoolingSpeedNum
        IF(CoolSpeedNum .LT. 1)THEN
          CompOnMassFlow   = UnitarySystem(UnitarySysNum)%IdleMassFlowRate
          CompOnFlowRatio  = UnitarySystem(UnitarySysNum)%IdleSpeedRatio
        ELSEIF(CoolSpeedNum .EQ. 1)THEN
          CompOnMassFlow  = UnitarySystem(UnitarySysNum)%CoolMassFlowRate(1)
          CompOnFlowRatio = UnitarySystem(UnitarySysNum)%MSCoolingSpeedRatio(1)
        ELSE
!          SpeedRatio           = UnitarySystem(UnitarySysNum)%MSCoolingSpeedRatio(CoolSpeedNum)
          SpeedRatio           = UnitarySystem(UnitarySysNum)%CoolingSpeedRatio

          CompOnMassFlow       = SpeedRatio*UnitarySystem(UnitarySysNum)%CoolMassFlowRate(CoolSpeedNum) + &
                                 (1.0d0-SpeedRatio)*UnitarySystem(UnitarySysNum)%CoolMassFlowRate(CoolSpeedNum-1)
          CompOnFlowRatio      = SpeedRatio*UnitarySystem(UnitarySysNum)%MSCoolingSpeedRatio(CoolSpeedNum) + &
                                 (1.0d0-SpeedRatio)*UnitarySystem(UnitarySysNum)%MSCoolingSpeedRatio(CoolSpeedNum-1)
          MSHPMassFlowRateHigh = UnitarySystem(UnitarySysNum)%CoolMassFlowRate(CoolSpeedNum)
          MSHPMassFlowRateLow  = UnitarySystem(UnitarySysNum)%CoolMassFlowRate(CoolSpeedNum-1)
        END IF
      ELSE ! IF (MultiOrVarSpeedCoolCoil) THEN
        CompOnMassFlow   = UnitarySystem(UnitarySysNum)%MaxCoolAirMassFlow
        CompOnFlowRatio  = UnitarySystem(UnitarySysNum)%CoolingFanSpeedRatio
        IF(UnitarySystem(UnitarySysNum)%FanOpMode == ContFanCycCoil)THEN
          CompOffMassFlow  = UnitarySystem(UnitarySysNum)%MaxNoCoolHeatAirMassFlow
          CompOffFlowRatio = UnitarySystem(UnitarySysNum)%CoolingFanSpeedRatio
        END IF
      END IF
    ELSE  ! Heating load but no moisture load
      CompOnMassFlow  = UnitarySystem(UnitarySysNum)%MaxHeatAirMassFlow
      CompOnFlowRatio = UnitarySystem(UnitarySysNum)%HeatingFanSpeedRatio
      IF(UnitarySystem(UnitarySysNum)%FanOpMode == ContFanCycCoil)THEN
        IF (UnitarySystem(UnitarySysNum)%AirFlowControl .EQ. UseCompressorOnFlow) THEN
          CompOffMassFlow  = UnitarySystem(UnitarySysNum)%MaxHeatAirMassFlow
          CompOffFlowRatio = UnitarySystem(UnitarySysNum)%HeatingFanSpeedRatio
        ELSE
          CompOffMassFlow  = UnitarySystem(UnitarySysNum)%MaxNoCoolHeatAirMassFlow
          CompOffFlowRatio = UnitarySystem(UnitarySysNum)%HeatingFanSpeedRatio
        END IF
      END IF
    END IF
  END IF

! If a cooling load exists, operate at the cooling mass flow rate
ELSEIF ( CoolingLoad ) THEN

  UnitarySystem(UnitarySysNum)%LastMode = CoolingMode

  IF(MultiOrVarSpeedCoolCoil(UnitarySysNum)) THEN

    CoolSpeedNum = UnitarySystem(UnitarySysNum)%CoolingSpeedNum

    IF (CoolSpeedNum .EQ. 0) THEN
      CompOnMassFlow  = UnitarySystem(UnitarySysNum)%IdleMassFlowRate
      CompOnFlowRatio = UnitarySystem(UnitarySysNum)%IdleSpeedRatio
    ELSE IF (CoolSpeedNum .EQ. 1) THEN
      CompOnMassFlow       = UnitarySystem(UnitarySysNum)%CoolMassFlowRate(1)
      CompOnFlowRatio      = UnitarySystem(UnitarySysNum)%MSCoolingSpeedRatio(1)
      MSHPMassFlowRateLow  = UnitarySystem(UnitarySysNum)%CoolMassFlowRate(1)
      MSHPMassFlowRateHigh = UnitarySystem(UnitarySysNum)%CoolMassFlowRate(1)
    ELSEIF (CoolSpeedNum .GT. 1) THEN
!      SpeedRatio           = UnitarySystem(UnitarySysNum)%MSCoolingSpeedRatio(CoolSpeedNum)
      SpeedRatio           = UnitarySystem(UnitarySysNum)%CoolingSpeedRatio

      CompOnMassFlow       = SpeedRatio * UnitarySystem(UnitarySysNum)%CoolMassFlowRate(CoolSpeedNum) + &
                             (1.0d0-SpeedRatio) * UnitarySystem(UnitarySysNum)%CoolMassFlowRate(CoolSpeedNum-1)
      CompOnFlowRatio      = SpeedRatio * UnitarySystem(UnitarySysNum)%MSCoolingSpeedRatio(CoolSpeedNum) + &
                             (1.0d0-SpeedRatio) * UnitarySystem(UnitarySysNum)%MSCoolingSpeedRatio(CoolSpeedNum-1)
      MSHPMassFlowRateHigh = UnitarySystem(UnitarySysNum)%CoolMassFlowRate(CoolSpeedNum)
      MSHPMassFlowRateLow  = UnitarySystem(UnitarySysNum)%CoolMassFlowRate(CoolSpeedNum-1)
    END IF
    ! Set the compressor or coil OFF mass flow rate based on LOGICAL flag
    ! UseCompressorOnFlow is used when the user does not enter a value for no cooling or heating flow rate
!    IF(UnitarySystem(UnitarySysNum)%FanOpMode == ContFanCycCoil)THEN
!      IF (UnitarySystem(UnitarySysNum)%AirFlowControl .EQ. UseCompressorOnFlow) THEN
        IF(CoolSpeedNum <= 1)THEN
          IF(UnitarySystem(UnitarySysNum)%FanOpMode == ContFanCycCoil)THEN
            CompOffMassFlow  = UnitarySystem(UnitarySysNum)%IdleMassFlowRate
            CompOffFlowRatio = UnitarySystem(UnitarySysNum)%IdleSpeedRatio
          END IF
        ELSE
          CompOffMassFlow = UnitarySystem(UnitarySysNum)%CoolMassFlowRate(CoolSpeedNum-1)
          CompOffFlowRatio = UnitarySystem(UnitarySysNum)%MSCoolingSpeedRatio(CoolSpeedNum-1)
        END IF
!      ELSE
!        CompOffMassFlow  = UnitarySystem(UnitarySysNum)%IdleMassFlowRate
!        CompOffFlowRatio = UnitarySystem(UnitarySysNum)%IdleSpeedRatio
!      END IF
!    END IF
  ELSE ! IF(MultiOrVarSpeedCoolCoil(UnitarySysNum)) THEN
    CompOnMassFlow = UnitarySystem(UnitarySysNum)%MaxCoolAirMassFlow
    CompOnFlowRatio = UnitarySystem(UnitarySysNum)%CoolingSpeedRatio
    IF(UnitarySystem(UnitarySysNum)%FanOpMode == ContFanCycCoil)THEN
      IF (UnitarySystem(UnitarySysNum)%AirFlowControl .EQ. UseCompressorOnFlow) THEN
        CompOffMassFlow = UnitarySystem(UnitarySysNum)%MaxCoolAirMassFlow
        CompOffFlowRatio = UnitarySystem(UnitarySysNum)%CoolingFanSpeedRatio
      ELSE
        CompOffMassFlow = UnitarySystem(UnitarySysNum)%MaxNoCoolHeatAirMassFlow
        CompOffFlowRatio = UnitarySystem(UnitarySysNum)%CoolingFanSpeedRatio
      END IF
    END IF
  END IF

ELSE  ! No load
! If no load exists, set the compressor on mass flow rate.
! Set equal the mass flow rate when no heating or cooling is needed If no moisture load exists.
! If the user has set the off mass flow rate to 0, set according to the last operating mode.

  IF(MoistureLoad .LT. 0.0d0 .AND. UnitarySystem(UnitarySysNum)%Humidistat .AND. &
     UnitarySystem(UnitarySysNum)%DehumidControlType_Num == DehumidControl_CoolReheat)THEN
    IF (MultiOrVarSpeedCoolCoil(UnitarySysNum)) THEN
      CoolSpeedNum = UnitarySystem(UnitarySysNum)%CoolingSpeedNum
      IF(CoolSpeedNum .LT. 1)THEN
        CompOnMassFlow   = UnitarySystem(UnitarySysNum)%IdleMassFlowRate
        CompOnFlowRatio  = UnitarySystem(UnitarySysNum)%IdleSpeedRatio
      ELSEIF(CoolSpeedNum .EQ. 1)THEN
        CompOnMassFlow  = UnitarySystem(UnitarySysNum)%CoolMassFlowRate(1)
        CompOnFlowRatio = UnitarySystem(UnitarySysNum)%MSCoolingSpeedRatio(1)
      ELSE
!        SpeedRatio           = UnitarySystem(UnitarySysNum)%MSCoolingSpeedRatio(CoolSpeedNum)
        SpeedRatio           = UnitarySystem(UnitarySysNum)%CoolingSpeedRatio

        CompOnMassFlow       = SpeedRatio * UnitarySystem(UnitarySysNum)%CoolMassFlowRate(CoolSpeedNum) + &
                               (1.0d0-SpeedRatio) * UnitarySystem(UnitarySysNum)%CoolMassFlowRate(CoolSpeedNum-1)
        CompOnFlowRatio      = SpeedRatio * UnitarySystem(UnitarySysNum)%MSCoolingSpeedRatio(CoolSpeedNum) + &
                               (1.0d0-SpeedRatio) * UnitarySystem(UnitarySysNum)%MSCoolingSpeedRatio(CoolSpeedNum-1)
        MSHPMassFlowRateHigh = UnitarySystem(UnitarySysNum)%CoolMassFlowRate(CoolSpeedNum)
        MSHPMassFlowRateLow  = UnitarySystem(UnitarySysNum)%CoolMassFlowRate(CoolSpeedNum-1)
      END IF

      IF(UnitarySystem(UnitarySysNum)%FanOpMode == ContFanCycCoil)THEN
        IF (UnitarySystem(UnitarySysNum)%AirFlowControl .EQ. UseCompressorOnFlow) THEN
          IF(CoolSpeedNum <= 1)THEN
            CompOffMassFlow  = UnitarySystem(UnitarySysNum)%IdleMassFlowRate
            CompOffFlowRatio = UnitarySystem(UnitarySysNum)%IdleSpeedRatio
          ELSE
            CompOffMassFlow = UnitarySystem(UnitarySysNum)%CoolMassFlowRate(CoolSpeedNum-1)
            CompOffFlowRatio = UnitarySystem(UnitarySysNum)%MSCoolingSpeedRatio(CoolSpeedNum-1)
          END IF
        ELSE
          CompOffMassFlow  = UnitarySystem(UnitarySysNum)%IdleMassFlowRate
          CompOffFlowRatio = UnitarySystem(UnitarySysNum)%IdleSpeedRatio
        END IF
      END IF

    ELSE  ! IF (MultiOrVarSpeedCoolCoil(UnitarySysNum)) THEN
      CompOnMassFlow = UnitarySystem(UnitarySysNum)%MaxCoolAirMassFlow
      CompOnFlowRatio = UnitarySystem(UnitarySysNum)%CoolingFanSpeedRatio
      IF(UnitarySystem(UnitarySysNum)%FanOpMode == ContFanCycCoil)THEN
        IF (UnitarySystem(UnitarySysNum)%AirFlowControl .EQ. UseCompressorOnFlow) THEN
          CompOffMassFlow  = UnitarySystem(UnitarySysNum)%MaxCoolAirMassFlow
          CompOffFlowRatio = UnitarySystem(UnitarySysNum)%CoolingFanSpeedRatio
        ELSE
          CompOffMassFlow  = UnitarySystem(UnitarySysNum)%MaxNoCoolHeatAirMassFlow
          CompOffFlowRatio = UnitarySystem(UnitarySysNum)%CoolingFanSpeedRatio
        END IF
      END IF
    END IF

  ELSE ! No Moisture Load

    IF(UnitarySystem(UnitarySysNum)%LastMode .EQ. HeatingMode)THEN
      IF(MultiOrVarSpeedHeatCoil(UnitarySysNum)) THEN
        CompOnMassFlow  = UnitarySystem(UnitarySysNum)%IdleMassFlowRate
        CompOnFlowRatio = UnitarySystem(UnitarySysNum)%IdleSpeedRatio
      ELSE
        CompOnMassFlow  = UnitarySystem(UnitarySysNum)%MaxNoCoolHeatAirMassFlow
        CompOnFlowRatio = 1.0d0
      END IF
    ELSE
      IF(MultiOrVarSpeedCoolCoil(UnitarySysNum)) THEN
        CompOnMassFlow  = UnitarySystem(UnitarySysNum)%IdleMassFlowRate
        CompOnFlowRatio = UnitarySystem(UnitarySysNum)%IdleSpeedRatio
      ELSE
        CompOnMassFlow  = UnitarySystem(UnitarySysNum)%MaxNoCoolHeatAirMassFlow
        CompOnFlowRatio = 1.0d0
      END IF
    END IF
    IF(CompOnMassFlow .EQ. 0.0d0)THEN
      IF(UnitarySystem(UnitarySysNum)%LastMode .EQ. HeatingMode)THEN
        IF(MultiOrVarSpeedHeatCoil(UnitarySysNum)) THEN
          HeatSpeedNum = UnitarySystem(UnitarySysNum)%HeatingSpeedNum
          IF (HeatSpeedNum .EQ. 0) THEN
            CompOnMassFlow  = UnitarySystem(UnitarySysNum)%IdleMassFlowRate
            CompOnFlowRatio = UnitarySystem(UnitarySysNum)%IdleSpeedRatio
          ELSE IF (HeatSpeedNum .EQ. 1) THEN
            CompOnMassFlow   = UnitarySystem(UnitarySysNum)%HeatMassFlowRate(1)
            CompOnFlowRatio  = UnitarySystem(UnitarySysNum)%MSHeatingSpeedRatio(1)
!            CompOffMassFlow  = UnitarySystem(UnitarySysNum)%IdleMassFlowRate
!            CompOffFlowRatio = UnitarySystem(UnitarySysNum)%IdleMassFlowRate
          ELSEIF (HeatSpeedNum .GT. 1) THEN
            CompOnMassFlow   = UnitarySystem(UnitarySysNum)%HeatMassFlowRate(HeatSpeedNum)
            CompOnFlowRatio  = UnitarySystem(UnitarySysNum)%MSHeatingSpeedRatio(HeatSpeedNum)
!            CompOffMassFlow  = UnitarySystem(UnitarySysNum)%HeatMassFlowRate(HeatSpeedNum-1)
!            CompOffFlowRatio = UnitarySystem(UnitarySysNum)%MSHeatingSpeedRatio(HeatSpeedNum-1)
          END IF
        ELSE  ! IF(MultiOrVarSpeedHeatCoil) THEN
          CompOnMassFlow  = UnitarySystem(UnitarySysNum)%MaxHeatAirMassFlow
          CompOnFlowRatio = UnitarySystem(UnitarySysNum)%HeatingFanSpeedRatio
        END IF
      ELSE   ! IF(UnitarySystem(UnitarySysNum)%LastMode .EQ. HeatingMode)THEN
        IF(MultiOrVarSpeedCoolCoil(UnitarySysNum)) THEN
          CoolSpeedNum = UnitarySystem(UnitarySysNum)%CoolingSpeedNum
          IF (CoolSpeedNum .EQ. 0) THEN
            CompOnMassFlow  = UnitarySystem(UnitarySysNum)%IdleMassFlowRate
            CompOnFlowRatio = UnitarySystem(UnitarySysNum)%IdleSpeedRatio
          ELSE IF (CoolSpeedNum .EQ. 1) THEN
            CompOnMassFlow   = UnitarySystem(UnitarySysNum)%CoolMassFlowRate(1)
            CompOnFlowRatio  = UnitarySystem(UnitarySysNum)%MSCoolingSpeedRatio(1)
!            CompOffMassFlow  = UnitarySystem(UnitarySysNum)%IdleMassFlowRate
!            CompOffFlowRatio = UnitarySystem(UnitarySysNum)%IdleMassFlowRate
          ELSEIF (CoolSpeedNum .GT. 1) THEN
            CompOnMassFlow   = UnitarySystem(UnitarySysNum)%CoolMassFlowRate(CoolSpeedNum)
            CompOnFlowRatio  = UnitarySystem(UnitarySysNum)%MSCoolingSpeedRatio(CoolSpeedNum)
!            CompOffMassFlow  = UnitarySystem(UnitarySysNum)%CoolMassFlowRate(CoolSpeedNum-1)
!            CompOffFlowRatio = UnitarySystem(UnitarySysNum)%MSCoolingSpeedRatio(CoolSpeedNum-1)
          END IF
        ELSE  ! IF(MultiOrVarSpeedCoolCoil) THEN
          CompOnMassFlow  = UnitarySystem(UnitarySysNum)%MaxCoolAirMassFlow
          CompOnFlowRatio = UnitarySystem(UnitarySysNum)%CoolingFanSpeedRatio
        END IF ! IF(MultiOrVarSpeedCoolCoil) THEN
      END IF   ! IF(UnitarySystem(UnitarySysNum)%LastMode .EQ. HeatingMode)THEN
    END IF ! IF(CompOnMassFlow .EQ. 0.0d0)THEN

    IF(UnitarySystem(UnitarySysNum)%FanOpMode == ContFanCycCoil)THEN
      IF (UnitarySystem(UnitarySysNum)%AirFlowControl .EQ. UseCompressorOnFlow) THEN
        IF(UnitarySystem(UnitarySysNum)%LastMode .EQ. HeatingMode)THEN
          IF(MultiOrVarSpeedHeatCoil(UnitarySysNum)) THEN
            HeatSpeedNum = UnitarySystem(UnitarySysNum)%HeatingSpeedNum
            IF(HeatSpeedNum<1)THEN
              CompOffMassFlow  = UnitarySystem(UnitarySysNum)%IdleMassFlowRate
              CompOffFlowRatio = UnitarySystem(UnitarySysNum)%IdleSpeedRatio
            ELSE IF(HeatSpeedNum==1)THEN
              CompOffMassFlow  = UnitarySystem(UnitarySysNum)%HeatMassFlowRate(1)
              CompOffFlowRatio = UnitarySystem(UnitarySysNum)%MSHeatingSpeedRatio(1)
            ELSE
              CompOffMassFlow  = UnitarySystem(UnitarySysNum)%HeatMassFlowRate(HeatSpeedNum-1)
              CompOffFlowRatio = UnitarySystem(UnitarySysNum)%MSHeatingSpeedRatio(HeatSpeedNum-1)
            END IF
          ELSE
            CompOffMassFlow  = UnitarySystem(UnitarySysNum)%MaxHeatAirMassFlow
            CompOffFlowRatio = UnitarySystem(UnitarySysNum)%HeatingFanSpeedRatio
          END IF
        ELSE   ! IF(UnitarySystem(UnitarySysNum)%LastMode .EQ. HeatingMode)THEN
          IF(MultiOrVarSpeedCoolCoil(UnitarySysNum)) THEN
            CoolSpeedNum = UnitarySystem(UnitarySysNum)%CoolingSpeedNum
            IF(CoolSpeedNum<1)THEN
              CompOffMassFlow  = UnitarySystem(UnitarySysNum)%IdleMassFlowRate
              CompOffFlowRatio = UnitarySystem(UnitarySysNum)%IdleSpeedRatio
            ELSE IF(CoolSpeedNum==1)THEN
              CompOffMassFlow  = UnitarySystem(UnitarySysNum)%CoolMassFlowRate(1)
              CompOffFlowRatio = UnitarySystem(UnitarySysNum)%MSCoolingSpeedRatio(1)
            ELSE
              CompOffMassFlow  = UnitarySystem(UnitarySysNum)%CoolMassFlowRate(CoolSpeedNum-1)
              CompOffFlowRatio = UnitarySystem(UnitarySysNum)%MSCoolingSpeedRatio(CoolSpeedNum-1)
            END IF
          ELSE
            CompOffMassFlow  = UnitarySystem(UnitarySysNum)%MaxCoolAirMassFlow
            CompOffFlowRatio = UnitarySystem(UnitarySysNum)%CoolingFanSpeedRatio
          END IF
        END IF   ! IF(UnitarySystem(UnitarySysNum)%LastMode .EQ. HeatingMode)THEN
      ELSE  ! IF (UnitarySystem(UnitarySysNum)%AirFlowControl .EQ. UseCompressorOnFlow) THEN
        CompOffMassFlow  = UnitarySystem(UnitarySysNum)%MaxNoCoolHeatAirMassFlow
        IF(UnitarySystem(UnitarySysNum)%LastMode .EQ. HeatingMode)THEN
          IF(MultiOrVarSpeedHeatCoil(UnitarySysNum)) THEN
            CompOffFlowRatio = UnitarySystem(UnitarySysNum)%MSHeatingSpeedRatio(1)
          ELSE
            CompOffFlowRatio = UnitarySystem(UnitarySysNum)%HeatingFanSpeedRatio
          END IF
        ELSE
          IF(MultiOrVarSpeedCoolCoil(UnitarySysNum)) THEN
            CompOffFlowRatio = UnitarySystem(UnitarySysNum)%MSCoolingSpeedRatio(1)
          ELSE
            CompOffFlowRatio = UnitarySystem(UnitarySysNum)%CoolingFanSpeedRatio
          END IF
        END IF
      END IF ! IF (UnitarySystem(UnitarySysNum)%AirFlowControl .EQ. UseCompressorOnFlow) THEN
    END IF   ! IF(UnitarySystem(UnitarySysNum)%FanOpMode == ContFanCycCoil)THEN
  END IF     ! ELSE ! No Moisture Load
END IF       ! No Heating/Cooling Load

! Set the system mass flow rates
CALL SetAverageAirFlow(UnitarySysNum, PartLoadRatio, OnOffAirFlowRatio)

END SUBROUTINE SetOnOffMassFlowRate

SUBROUTINE SetAverageAirFlow(UnitarySysNum,PartLoadRatio,OnOffAirFlowRatio)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Raustad
          !       DATE WRITTEN   July 2005
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Set the average air mass flow rates using the part-load fraction of the HVAC system for this time step
          ! Set OnOffAirFlowRatio to be used by DX coils

          ! METHODOLOGY EMPLOYED:
          ! The air flow rate in cooling, heating, and no cooling or heating can be dIFferent.
          ! Calculate the air flow rate based on initializations.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataZoneEnergyDemands

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER,   INTENT (IN)    :: UnitarySysNum   ! Unit index
  REAL(r64), INTENT (IN)    :: PartLoadRatio      ! unit part load ratio
  REAL(r64), INTENT (INOUT) :: OnOffAirFlowRatio  ! ratio of compressor ON airflow to AVERAGE airflow over timestep

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER             :: InletNode           ! inlet node number
  REAL(r64)           :: AverageUnitMassFlow ! average supply air mass flow rate over time step
  INTEGER             :: SpeedNum            ! speed for multi-speed or variable-speed coils
  LOGICAL             :: FanOn

  SpeedNum  = MAX(UnitarySystem(UnitarySysNum)%CoolingSpeedNum,UnitarySystem(UnitarySysNum)%HeatingSpeedNum)
  InletNode = UnitarySystem(UnitarySysNum)%UnitarySystemInletNodeNum

  IF(SpeedNum > 1)THEN
    AverageUnitMassFlow = CompOnMassFlow
  ELSE
    AverageUnitMassFlow = (PartLoadRatio * CompOnMassFlow) + ((1.0d0-PartLoadRatio) * CompOffMassFlow)
  END IF
  IF(CompOffFlowRatio .GT. 0.0d0)THEN
    IF(SpeedNum > 1)THEN
      FanSpeedRatio = CompOnFlowRatio
    ELSE
      FanSpeedRatio = (PartLoadRatio * CompOnFlowRatio) + ((1.0d0-PartLoadRatio) * CompOffFlowRatio)
    END IF
  ELSE
    FanSpeedRatio     = CompOnFlowRatio
  END IF
! If the unitary system is scheduled on or nightime cycle overrides fan schedule. Uses same logic as fan.
  IF(UnitarySystem(UnitarySysNum)%FanExists)THEN
    FanOn=.FALSE.
    IF(GetCurrentScheduleValue(UnitarySystem(UnitarySysNum)%FanAvailSchedPtr) .GT. 0)FanOn=.TRUE.
  ELSE
    FanOn=.TRUE.
  END IF
  IF ( GetCurrentScheduleValue(UnitarySystem(UnitarySysNum)%SysAvailSchedPtr) .GT. 0.0d0 .AND. &
     ( (FanOn .OR. TurnFansOn) .AND. .NOT. TurnFansOff) ) THEN
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

  RETURN
END SUBROUTINE SetAverageAirFlow

SUBROUTINE ReportUnitarySystem(UnitarySysNum, AirLoopNum)


          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Chandan Sharma
          !       DATE WRITTEN   July 2013
          !       MODIFIED       na
          !       RE-ENGINEERED  na


          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine updates the report variable for the coils.


          ! METHODOLOGY EMPLOYED:
          !

          ! REFERENCES:
          ! na


          ! USE STATEMENTS:
  USE Psychrometrics,        ONLY: PsyHFnTdbW
  USE DataAirLoop, ONLY: LoopSystemOnMassFlowrate,LoopSystemOffMassFlowrate,LoopFanOperationMode,LoopOnOffFanPartLoadRatio, &
                         LoopCompCycRatio, AirLoopFlow

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine


          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: UnitarySysNum
  INTEGER, INTENT(IN) :: AirLoopNum


          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na


          ! INTERFACE BLOCK SPECIFICATIONS
          ! na


          ! DERIVED TYPE DEFINITIONS
          ! na


          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
      INTEGER      :: OutletNode
      INTEGER      :: InletNode
      REAL(r64)    :: QSensUnitOut
      REAL(r64)    :: QTotUnitOut
      REAL(r64)    :: AirMassFlow
      REAL(r64)    :: MinHumRatio
      REAL(r64)    :: CompPartLoadFrac
      REAL(r64)    :: ReportingConstant

      ReportingConstant = TimeStepSys*SecInHour

      QTotUnitOut = 0.0d0
      QSensUnitOut = 0.0d0
      UnitarySystem(UnitarySysNum)%PartLoadFrac              = 0.0d0
      UnitarySystem(UnitarySysNum)%CompPartLoadRatio         = 0.0d0
      UnitarySystem(UnitarySysNum)%CycRatio                  = 0.0d0
      UnitarySystem(UnitarySysNum)%SpeedRatio                = 0.0d0
      UnitarySystem(UnitarySysNum)%FanPartLoadRatio          = 0.0d0
      UnitarySystem(UnitarySysNum)%TotalAuxElecPower         = 0.0d0
      UnitarySystem(UnitarySysNum)%HeatingAuxElecConsumption = 0.0d0
      UnitarySystem(UnitarySysNum)%CoolingAuxElecConsumption = 0.0d0
      UnitarySystem(UnitarySysNum)%ElecPower                 = 0.0d0
      UnitarySystem(UnitarySysNum)%ElecPowerConsumption      = 0.0d0

      OutletNode  = UnitarySystem(UnitarySysNum)%UnitarySystemOutletNodeNum
      AirMassFlow = Node(OutletNode)%MassFlowRate
      SELECT CASE(UnitarySystem(UnitarySysNum)%ControlType)
        CASE(SetPointBased)
          InletNode  = UnitarySystem(UnitarySysNum)%UnitarySystemInletNodeNum
          MinHumRatio  = Node(InletNode)%HumRat
          QSensUnitOut = AirMassFlow * (PsyHFnTdbW(Node(OutletNode)%Temp,MinHumRatio)  &
                            - PsyHFnTdbW(Node(InletNode)%Temp,MinHumRatio)) &
                            - UnitarySystem(UnitarySysNum)%SenLoadLoss
          QTotUnitOut  = AirMassFlow * (Node(OutletNode)%Enthalpy - &
                         Node(InletNode)%Enthalpy)

        CASE(LoadBased)
          MinHumRatio  = Node(UnitarySystem(UnitarySysNum)%NodeNumofControlledZone)%HumRat
          QSensUnitOut = AirMassFlow * (PsyHFnTdbW(Node(OutletNode)%Temp,MinHumRatio)  &
                           - PsyHFnTdbW(Node(UnitarySystem(UnitarySysNum)%NodeNumofControlledZone)%Temp,MinHumRatio)) &
                           - UnitarySystem(UnitarySysNum)%SenLoadLoss
          QTotUnitOut  = AirMassFlow * (Node(OutletNode)%Enthalpy - &
                         Node(UnitarySystem(UnitarySysNum)%NodeNumofControlledZone)%Enthalpy)

        CASE DEFAULT

      END SELECT

      ! set the system part-load ratio report variable
      UnitarySystem(UnitarySysNum)%PartLoadFrac = MAX(UnitarySystem(UnitarySysNum)%CoolingPartLoadFrac, &
                                                      UnitarySystem(UnitarySysNum)%HeatingPartLoadFrac)
      ! set the compressor part-load ratio report variable
      SELECT CASE(UnitarySystem(UnitarySysNum)%HeatingCoilType_Num)
        CASE(CoilDX_HeatingEmpirical, CoilDX_MultiSpeedHeating, Coil_HeatingWaterToAirHP, &
             Coil_HeatingWaterToAirHPSimple, Coil_HeatingWaterToAirHPVSEquationFit, Coil_HeatingAirToAirVariableSpeed)
! wasn't this already set in the calc routine?
! they look wrong anyway since the compressor can be off when the fan is on
!          UnitarySystem(UnitarySysNum)%CompPartLoadRatio = UnitarySystem(UnitarySysNum)%PartLoadFrac
        CASE DEFAULT
!          UnitarySystem(UnitarySysNum)%CompPartLoadRatio = UnitarySystem(UnitarySysNum)%CoolingPartLoadFrac
      END SELECT
      UnitarySystem(UnitarySysNum)%CompPartLoadRatio = MAX(UnitarySystem(UnitarySysNum)%CoolCompPartLoadRatio, &
                                                           UnitarySystem(UnitarySysNum)%HeatCompPartLoadRatio)

      IF (HeatingLoad) THEN
        UnitarySystem(UnitarySysNum)%TotCoolEnergyRate  = 0.0d0
        UnitarySystem(UnitarySysNum)%SensCoolEnergyRate = 0.0d0
        UnitarySystem(UnitarySysNum)%LatCoolEnergyRate  = 0.0d0
        UnitarySystem(UnitarySysNum)%TotHeatEnergyRate  = ABS(MAX(0.0d0, QTotUnitOut))
        UnitarySystem(UnitarySysNum)%SensHeatEnergyRate = ABS(MAX(0.0d0, QSensUnitOut))
        UnitarySystem(UnitarySysNum)%LatHeatEnergyRate  = ABS(MAX(0.0d0, (QTotUnitOut - QSensUnitOut)))
      ELSE
        UnitarySystem(UnitarySysNum)%TotCoolEnergyRate  = ABS(MIN(0.0d0, QTotUnitOut))
        UnitarySystem(UnitarySysNum)%SensCoolEnergyRate = ABS(MIN(0.0d0, QSensUnitOut))
        UnitarySystem(UnitarySysNum)%LatCoolEnergyRate  = ABS(MIN(0.0d0, (QTotUnitOut - QSensUnitOut)))
        UnitarySystem(UnitarySysNum)%TotHeatEnergyRate  = 0.0d0
        UnitarySystem(UnitarySysNum)%SensHeatEnergyRate = 0.0d0
        UnitarySystem(UnitarySysNum)%LatHeatEnergyRate  = 0.0d0
      END IF

      IF (UnitarySystem(UnitarySysNum)%FanExists) THEN
        IF (CompOnMassFlow .GT. 0.0d0) THEN
          UnitarySystem(UnitarySysNum)%FanPartLoadRatio = Node(OutletNode)%MassFlowRate / CompOnMassFlow
        ELSE
          UnitarySystem(UnitarySysNum)%FanPartLoadRatio = 0.0d0
        END IF
        IF(AirLoopNum .GT. 0)THEN
          IF(UnitarySystem(UnitarySysNum)%FanOpMode == CycFanCycCoil)THEN
            AirLoopflow(AirLoopNum)%FanPLR = UnitarySystem(UnitarySysNum)%FanPartLoadRatio
          ELSE
            AirLoopflow(AirLoopNum)%FanPLR = 0.0d0
          END IF
        END IF
      END IF

      SELECT CASE(UnitarySystem(UnitarySysNum)%CoolingCoilType_Num)

        CASE(CoilDX_CoolingTwoSpeed)
          ! need to make sure these are 0 for non-variable speed coils (or not report these variables)
          UnitarySystem(UnitarySysNum)%CycRatio     = MAX(UnitarySystem(UnitarySysNum)%CoolingCycRatio,     &
                                                          UnitarySystem(UnitarySysNum)%HeatingCycRatio)
          UnitarySystem(UnitarySysNum)%SpeedRatio   = MAX(UnitarySystem(UnitarySysNum)%CoolingSpeedRatio,   &
                                                          UnitarySystem(UnitarySysNum)%HeatingSpeedRatio)
          UnitarySystem(UnitarySysNum)%SpeedNum     = MAX(UnitarySystem(UnitarySysNum)%CoolingSpeedNum,   &
                                                          UnitarySystem(UnitarySysNum)%HeatingSpeedNum)

        CASE(CoilDX_MultiSpeedCooling)
          UnitarySystem(UnitarySysNum)%CycRatio     = MAX(UnitarySystem(UnitarySysNum)%CoolingCycRatio,     &
                                                          UnitarySystem(UnitarySysNum)%HeatingCycRatio)
          UnitarySystem(UnitarySysNum)%SpeedRatio   = MAX(UnitarySystem(UnitarySysNum)%CoolingSpeedRatio,   &
                                                          UnitarySystem(UnitarySysNum)%HeatingSpeedRatio)
          UnitarySystem(UnitarySysNum)%SpeedNum     = MAX(UnitarySystem(UnitarySysNum)%CoolingSpeedNum,   &
                                                          UnitarySystem(UnitarySysNum)%HeatingSpeedNum)

          CompPartLoadFrac = UnitarySystem(UnitarySysNum)%CompPartLoadRatio
          IF (CoolingLoad) THEN

            UnitarySystem(UnitarySysNum)%TotalAuxElecPower  = UnitarySystem(UnitarySysNum)%AncillaryOnPower * CompPartLoadFrac + &
                                                         UnitarySystem(UnitarySysNum)%AncillaryOffPower * (1.0d0-CompPartLoadFrac)
            UnitarySystem(UnitarySysNum)%CoolingAuxElecConsumption = UnitarySystem(UnitarySysNum)%AncillaryOnPower * &
                                                                   CompPartLoadFrac * ReportingConstant
          END IF
          IF (UnitarySystem(UnitarySysNum)%LastMode .EQ. CoolingMode) THEN
            UnitarySystem(UnitarySysNum)%CoolingAuxElecConsumption = UnitarySystem(UnitarySysNum)%CoolingAuxElecConsumption + &
                              UnitarySystem(UnitarySysNum)%AncillaryOffPower*(1.0d0-CompPartLoadFrac)*ReportingConstant

          END IF
          UnitarySystem(UnitarySysNum)%ElecPower = FanElecPower + DXElecCoolingPower + DXElecHeatingPower + &
                                                   ElecHeatingCoilPower + UnitarySystem(UnitarySysNum)%TotalAuxElecPower
          UnitarySystem(UnitarySysNum)%ElecPowerConsumption = UnitarySystem(UnitarySysNum)%ElecPower * ReportingConstant

        CASE DEFAULT

      END SELECT

      SELECT CASE(UnitarySystem(UnitarySysNum)%HeatingCoilType_Num)

        CASE(CoilDX_MultiSpeedHeating)
          UnitarySystem(UnitarySysNum)%CycRatio     = MAX(UnitarySystem(UnitarySysNum)%CoolingCycRatio,     &
                                                          UnitarySystem(UnitarySysNum)%HeatingCycRatio)
          UnitarySystem(UnitarySysNum)%SpeedRatio   = MAX(UnitarySystem(UnitarySysNum)%CoolingSpeedRatio,   &
                                                          UnitarySystem(UnitarySysNum)%HeatingSpeedRatio)

          CompPartLoadFrac = UnitarySystem(UnitarySysNum)%CompPartLoadRatio
          IF (HeatingLoad) THEN

            UnitarySystem(UnitarySysNum)%TotalAuxElecPower  = UnitarySystem(UnitarySysNum)%AncillaryOnPower * CompPartLoadFrac + &
                                                         UnitarySystem(UnitarySysNum)%AncillaryOffPower * (1.0d0-CompPartLoadFrac)
            UnitarySystem(UnitarySysNum)%HeatingAuxElecConsumption = UnitarySystem(UnitarySysNum)%AncillaryOnPower * &
                                                                     CompPartLoadFrac * ReportingConstant
          END IF
          IF (UnitarySystem(UnitarySysNum)%LastMode .EQ. HeatingMode) THEN
            UnitarySystem(UnitarySysNum)%HeatingAuxElecConsumption = UnitarySystem(UnitarySysNum)%HeatingAuxElecConsumption + &
                                UnitarySystem(UnitarySysNum)%AncillaryOffPower*(1.0d0-CompPartLoadFrac)*ReportingConstant

          END IF
          UnitarySystem(UnitarySysNum)%ElecPower = FanElecPower + DXElecCoolingPower + DXElecHeatingPower + &
                                                   UnitarySystem(UnitarySysNum)%TotalAuxElecPower
          UnitarySystem(UnitarySysNum)%ElecPowerConsumption = UnitarySystem(UnitarySysNum)%ElecPower * ReportingConstant

        CASE(Coil_HeatingGas_MultiStage, Coil_HeatingElectric_MultiStage)
          UnitarySystem(UnitarySysNum)%CycRatio     = MAX(UnitarySystem(UnitarySysNum)%CoolingCycRatio,     &
                                                          UnitarySystem(UnitarySysNum)%HeatingCycRatio)
          UnitarySystem(UnitarySysNum)%SpeedRatio   = MAX(UnitarySystem(UnitarySysNum)%CoolingSpeedRatio,   &
                                                          UnitarySystem(UnitarySysNum)%HeatingSpeedRatio)

          UnitarySystem(UnitarySysNum)%ElecPower = FanElecPower + DXElecCoolingPower + &
                                                   ElecHeatingCoilPower + UnitarySystem(UnitarySysNum)%TotalAuxElecPower
          UnitarySystem(UnitarySysNum)%ElecPowerConsumption = UnitarySystem(UnitarySysNum)%ElecPower * ReportingConstant

        CASE DEFAULT

      END SELECT

  LoopSystemOnMassFlowrate = CompOnMassFlow
  LoopSystemOffMassFlowrate = CompOffMassFlow
  LoopFanOperationMode = UnitarySystem(UnitarySysNum)%FanOpMode
  LoopOnOffFanPartLoadRatio = UnitarySystem(UnitarySysNum)%FanPartLoadRatio
  LoopCompCycRatio = UnitarySystem(UnitarySysNum)%CycRatio

  IF(CurOASysNum > 0)THEN
    OASysEqSizing(CurOASysNum)%AirFlow = .FALSE.
    OASysEqSizing(CurOASysNum)%Capacity = .FALSE.
  ELSE IF(CurSysNum > 0)THEN
    UnitarySysEqSizing(CurSysNum)%AirFlow = .FALSE.
    UnitarySysEqSizing(CurSysNum)%Capacity = .FALSE.
    AirLoopControlInfo(CurSysNum)%UnitarySysSimulating = .FALSE.
  ELSE IF(CurZoneEqNum > 0)THEN
    ZoneEqSizing(CurZoneEqNum)%AirFlow = .FALSE.
    ZoneEqSizing(CurZoneEqNum)%Capacity = .FALSE.
  END IF

RETURN
END SUBROUTINE ReportUnitarySystem

SUBROUTINE UnitarySystemHeatRecovery(UnitarySysNum)

            ! SUBROUTINE INFORMATION:
            !       AUTHOR:          Chandan Sharma
            !       DATE WRITTEN:    May 2013
            !       MODIFIED:        na
            !       RE-ENGINEERED    na

            ! PURPOSE OF THIS SUBROUTINE:
            !  Calculate the heat recovered from UnitarySystem

            ! METHODOLOGY EMPLOYED:
            !  na

            ! REFERENCES:
            !  na

            ! USE STATEMENTS:
  USE FluidProperties,     ONLY: GetSpecificHeatGlycol
  USE DataPlant,           ONLY: PlantLoop
  USE PlantUtilities,      ONLY: SafeCopyPlantNode

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN)     :: UnitarySysNum ! Number of the current electric UnitarySystem being simulated

          ! SUBROUTINE PARAMETER DEFINITIONS:
          !  na

          ! DERIVMS TYPE DEFINITIONS:
          !  na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER      :: HeatRecInNode       ! Node number of heat recovery water inlet node
  INTEGER      :: HeatRecOutNode      ! Node number of heat recovery water outlet node
  REAL(r64)    :: QHeatRec            ! Total heat recovered [W]
  REAL(r64)    :: HeatRecInletTemp    ! Heat reclaim inlet temp [C]
  REAL(r64)    :: HeatRecOutletTemp   ! Heat reclaim outlet temp [C]
  REAL(r64)    :: HeatRecMassFlowRate ! Heat reclaim mass flow rate [m3/s]
  REAL(r64)    :: CpHeatRec           ! Heat reclaim water inlet specIFic heat [J/kg-K]
  REAL(r64)    :: HeatRecInletEnth    ! Heat reclaim water inlet enthalpy [J/kg]
  REAL(r64)    :: ReportingConstant

  ReportingConstant = TimeStepSys*SecInHour

  ! Begin routine
  HeatRecInNode  = UnitarySystem(UnitarySysNum)%HeatRecoveryInletNodeNum
  HeatRecOutNode = UnitarySystem(UnitarySysNum)%HeatRecoveryOutletNodeNum

   ! Inlet node to the heat recovery heat exchanger
  HeatRecInletTemp  = Node(HeatRecInNode)%Temp
  HeatRecInletEnth  = Node(HeatRecInNode)%Enthalpy

   ! Set heat recovery mass flow rates
  HeatRecMassFlowRate = Node(HeatRecInNode)%MassFlowRate

  QHeatRec = MSHPWasteHeat

  IF (HeatRecMassFlowRate > 0.0d0) THEN

    CpHeatRec = GetSpecificHeatGlycol(PlantLoop(UnitarySystem(UnitarySysNum)%HRLoopNum)%FluidName, &
                                      HeatRecInletTemp, &
                                      PlantLoop(UnitarySystem(UnitarySysNum)%HRLoopNum)%FluidIndex, &
                                      'UnitarySystemHeatRecovery')

    HeatRecOutletTemp = QHeatRec/(HeatRecMassFlowRate*CpHeatRec) + HeatRecInletTemp
    IF (HeatRecOutletTemp .GT. UnitarySystem(UnitarySysNum)%MaxHROutletWaterTemp) &
        HeatRecOutletTemp = UnitarySystem(UnitarySysNum)%MaxHROutletWaterTemp
  ELSE
    HeatRecOutletTemp = HeatRecInletTemp
  END IF

  CALL SafeCopyPlantNode(HeatRecInNode, HeatRecOutNode)
  ! changed outputs
  Node(HeatRecOutNode)%Temp         = HeatRecOutletTemp

  UnitarySystem(UnitarySysNum)%HeatRecoveryRate         = QHeatRec
  UnitarySystem(UnitarySysNum)%HeatRecoveryEnergy       = UnitarySystem(UnitarySysNum)%HeatRecoveryRate*ReportingConstant
  UnitarySystem(UnitarySysNum)%HeatRecoveryInletTemp    = HeatRecInletTemp
  UnitarySystem(UnitarySysNum)%HeatRecoveryOutletTemp   = HeatRecOutletTemp
  UnitarySystem(UnitarySysNum)%HeatRecoveryMassFlowRate = HeatRecMassFlowRate

  RETURN

END SUBROUTINE UnitarySystemHeatRecovery

FUNCTION DXHeatingCoilResidual(PartLoadFrac, Par) RESULT (Residuum)
          ! FUNCTION INFORMATION:
          !       AUTHOR         Richard Raustad, FSEC
          !       DATE WRITTEN   June 2006
          !       MODIFIED
          !       RE-ENGINEERED

          ! PURPOSE OF THIS FUNCTION:
          ! Calculates residual function (desired outlet temp - actual outlet temp)
          ! DX Coil output depends on the part load ratio which is being varied to zero the residual.

          ! METHODOLOGY EMPLOYED:
          ! Calls CalcDOe2DXCoil to get outlet temperature at the given cycling ratio
          ! and calculates the residual as defined above

          ! REFERENCES:

          ! USE STATEMENTS:
  USE DXCoils, ONLY: DXCoilOutletTemp, CalcDXHeatingCoil

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
    REAL(r64), INTENT(IN)  :: PartLoadFrac               ! Compressor cycling ratio (1.0 is continuous, 0.0 is off)
    REAL(r64), INTENT(IN), DIMENSION(:), OPTIONAL :: Par ! Par(1) = DX coil number
                                                    ! Par(2) = desired air outlet temperature [C]
    REAL(r64)         :: Residuum                   ! Residual to be minimized to zero

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER      :: CoilIndex        ! Index of this coil
  REAL(r64)    :: OutletAirTemp    ! Outlet air temperature [C]
  REAL(r64)    :: OnOffAirFlowFrac ! Ratio of compressor ON to compressor OFF air mass flow rate

  CoilIndex        = INT(Par(1))
  OnOffAirFlowFrac = Par(3)

  CALL CalcDXHeatingCoil(CoilIndex,PartLoadFrac,ContFanCycCoil,OnOffAirFlowFrac)

  OutletAirTemp = DXCoilOutletTemp(CoilIndex)
  Residuum      = Par(2) - OutletAirTemp

  RETURN
END FUNCTION DXHeatingCoilResidual

FUNCTION DXCoilVarSpeedResidual(SpeedRatio, Par) RESULT (Residuum)
          ! FUNCTION INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   September 2002
          !       MODIFIED
          !       RE-ENGINEERED

          ! PURPOSE OF THIS FUNCTION:
          ! Calculates residual function (desired outlet temp - actual outlet temp).
          ! DX Coil output depends on the compressor speed which is being varied to zero the residual.

          ! METHODOLOGY EMPLOYED:
          ! Calls CalcMultiSpeedDXCoil to get outlet temperature at the given compressor speed
          ! and calculates the residual as defined above

          ! REFERENCES:

          ! USE STATEMENTS:
  USE DXCoils,                   ONLY: DXCoilOutletTemp, CalcMultiSpeedDXCoil, CalcMultiSpeedDXCoilCooling
  USE VariableSpeedCoils,        ONLY: CalcVarSpeedCoilCooling

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
    REAL(r64), INTENT(IN)  :: SpeedRatio ! compressor speed ratio (1.0 is max, 0.0 is min)
    REAL(r64), INTENT(IN), DIMENSION(:), OPTIONAL :: Par ! par(1) = DX coil number
                                                    ! par(2) = desired air outlet temperature [C]
    REAL(r64)         :: Residuum ! residual to be minimized to zero

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER   :: CoilIndex        ! index of this coil
  INTEGER   :: UnitarySysNum ! index to Unitary System
  REAL(r64) :: OutletAirTemp   ! outlet air temperature [C]
  REAL(r64) :: CycRatio
  INTEGER   :: SpeedNum
  INTEGER   :: FanOpMode
  INTEGER   :: CompOp
  REAL(r64) :: ReqOutput
  REAL(r64) :: dummy
  REAL(r64) :: RuntimeFrac
  REAL(r64) :: OnOffAirFlowRatio


  CoilIndex     = INT(Par(1))
  UnitarySysNum = INT(Par(3))

  SELECT CASE(UnitarySystem(UnitarySysNum)%CoolingCoilType_Num)

    CASE (CoilDX_CoolingTwoSpeed)

      CALL CalcMultiSpeedDXCoil(CoilIndex,SpeedRatio,1.0d0)
      OutletAirTemp = DXCoilOutletTemp(CoilIndex)

    CASE (CoilDX_MultiSpeedCooling)

      CycRatio = Par(4)
      SpeedNum   = INT(Par(5))
      FanOpMode  = INT(Par(6))
      CompOp     = INT(Par(7))

      CALL CalcMultiSpeedDXCoilCooling(CoilIndex,SpeedRatio,CycRatio,SpeedNum,FanOpMode,CompOp)
      OutletAirTemp = DXCoilOutletTemp(CoilIndex)

    CASE (Coil_CoolingAirToAirVariableSpeed, Coil_CoolingWaterToAirHPVSEquationFit)

      CycRatio   = Par(4)
      SpeedNum   = INT(Par(5))
      FanOpMode  = INT(Par(6))
      CompOp     = INT(Par(7))
      ReqOutput  = Par(8)
      dummy      = 0.0d0
      RuntimeFrac = 1.0d0
      OnOffAirFlowRatio = 1.0d0

      CALL CalcVarSpeedCoilCooling(CoilIndex,FanOpMode,RuntimeFrac,&
                ReqOutput,dummy,CompOp,CycRatio,OnOffAirFlowRatio, SpeedRatio, SpeedNum)

      OutletAirTemp = Node(UnitarySystem(UnitarySysNum)%CoolCoilOutletNodeNum)%Temp

    CASE DEFAULT

  END SELECT

  Residuum = Par(2) - OutletAirTemp

  RETURN
END FUNCTION DXCoilVarSpeedResidual

FUNCTION HeatingCoilVarSpeedResidual(SpeedRatio, Par) RESULT (Residuum)
          ! FUNCTION INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   September 2002
          !       MODIFIED
          !       RE-ENGINEERED

          ! PURPOSE OF THIS FUNCTION:
          ! Calculates residual function (desired outlet temp - actual outlet temp).
          ! DX Coil output depends on the compressor speed which is being varied to zero the residual.

          ! METHODOLOGY EMPLOYED:
          ! Calls calc routines of  multi Speed or variable Coil to get outlet temperature at the given compressor speed
          ! and calculates the residual as defined above

          ! REFERENCES:

          ! USE STATEMENTS:
  USE DXCoils,               ONLY: DXCoilOutletTemp, CalcMultiSpeedDXCoil, CalcMultiSpeedDXCoilHeating
  USE VariableSpeedCoils,    ONLY: CalcVarSpeedCoilHeating
  USE HeatingCoils,          ONLY: CalcMultiStageElectricHeatingCoil, CalcMultiStageGasHeatingCoil

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
    REAL(r64), INTENT(IN)  :: SpeedRatio ! compressor speed ratio (1.0 is max, 0.0 is min)
    REAL(r64), INTENT(IN), DIMENSION(:), OPTIONAL :: Par ! par(1) = DX coil number
                                                    ! par(2) = desired air outlet temperature [C]
    REAL(r64)         :: Residuum ! residual to be minimized to zero

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER   :: CoilIndex        ! index of this coil
  INTEGER   :: UnitarySysNum ! index to Unitary System
  REAL(r64) :: OutletAirTemp   ! outlet air temperature [C]
  REAL(r64) :: CycRatio
  INTEGER   :: SpeedNum
  INTEGER   :: FanOpMode
  INTEGER   :: CompOp
  REAL(r64) :: ReqOutput
  REAL(r64) :: RuntimeFrac
  REAL(r64) :: OnOffAirFlowRatio

  CoilIndex = INT(Par(1))
  UnitarySysNum = INT(Par(3))

  SELECT CASE(UnitarySystem(UnitarySysNum)%HeatingCoilType_Num)

    CASE (CoilDX_MultiSpeedHeating)

      CycRatio   = Par(4)
      SpeedNum   = INT(Par(5))
      FanOpMode  = INT(Par(6))

      CALL CalcMultiSpeedDXCoilHeating(CoilIndex,SpeedRatio,CycRatio,SpeedNum,FanOpMode)

      OutletAirTemp = DXCoilOutletTemp(CoilIndex)

    CASE (Coil_HeatingAirToAirVariableSpeed, Coil_HeatingWaterToAirHPVSEquationFit)

      CycRatio   = Par(4)
      SpeedNum   = INT(Par(5))
      FanOpMode  = INT(Par(6))
      CompOp     = INT(Par(7))
      ReqOutput  = Par(8)
      RuntimeFrac = 1.0d0
      OnOffAirFlowRatio = 1.0d0

      CALL CalcVarSpeedCoilHeating(CoilIndex,FanOpMode,RuntimeFrac,&
                ReqOutput,CompOp,CycRatio,OnOffAirFlowRatio, SpeedRatio, SpeedNum)

      OutletAirTemp = Node(UnitarySystem(UnitarySysNum)%HeatCoilOutletNodeNum)%Temp

    CASE (Coil_HeatingElectric_MultiStage)

      CycRatio   = Par(4)
      SpeedNum   = INT(Par(5))
      FanOpMode  = INT(Par(6))

      CALL CalcMultiStageElectricHeatingCoil(CoilIndex,SpeedRatio,CycRatio,SpeedNum,FanOpMode)

      OutletAirTemp = Node(UnitarySystem(UnitarySysNum)%HeatCoilOutletNodeNum)%Temp

    CASE (Coil_HeatingGas_MultiStage)

      CycRatio   = Par(4)
      SpeedNum   = INT(Par(5))
      FanOpMode  = INT(Par(6))

      CALL CalcMultiStageElectricHeatingCoil(CoilIndex,SpeedRatio,CycRatio,SpeedNum,FanOpMode)

      OutletAirTemp = Node(UnitarySystem(UnitarySysNum)%HeatCoilOutletNodeNum)%Temp

    CASE DEFAULT

  END SELECT

  Residuum = Par(2) - OutletAirTemp

  RETURN
END FUNCTION HeatingCoilVarSpeedResidual

FUNCTION DXCoilVarSpeedHumRatResidual(SpeedRatio, Par) RESULT (Residuum)
          ! FUNCTION INFORMATION:
          !       AUTHOR         Richard Raustad
          !       DATE WRITTEN   January 2008
          !       MODIFIED
          !       RE-ENGINEERED

          ! PURPOSE OF THIS FUNCTION:
          ! Calculates residual function (desired outlet humrat - actual outlet humrat).
          ! DX Coil output depends on the compressor speed which is being varied to zero the residual.

          ! METHODOLOGY EMPLOYED:
          ! Calls calc routine sof multi speed or variable speed coils to get outlet humidity ratio at the given compressor speed
          ! and calculates the residual as defined above

          ! REFERENCES:

          ! USE STATEMENTS:
  USE DXCoils,                   ONLY: DXCoilOutletHumRat, CalcMultiSpeedDXCoil, CalcMultiSpeedDXCoilCooling
  USE VariableSpeedCoils,        ONLY: CalcVarSpeedCoilCooling

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
    REAL(r64), INTENT(IN)  :: SpeedRatio ! compressor speed ratio (1.0 is max, 0.0 is min)
    REAL(r64), INTENT(IN), DIMENSION(:), OPTIONAL :: Par ! par(1) = DX coil number
                                                    ! par(2) = desired air outlet humidity ratio [kg/kg]
    REAL(r64)         :: Residuum ! residual to be minimized to zero

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER   :: CoilIndex         ! index of this coil
  INTEGER   :: UnitarySysNum     ! index to Unitary System
  REAL(r64) :: OutletAirHumRat   ! outlet air humidity ratio
  REAL(r64) :: CycRatio
  INTEGER   :: SpeedNum
  INTEGER   :: FanOpMode
  INTEGER   :: CompOp
  REAL(r64) :: ReqOutput
  REAL(r64) :: dummy
  REAL(r64) :: RuntimeFrac
  REAL(r64) :: OnOffAirFlowRatio

  CoilIndex = INT(Par(1))
  UnitarySysNum = INT(Par(3))
  SELECT CASE(UnitarySystem(UnitarySysNum)%CoolingCoilType_Num)

    CASE (CoilDX_CoolingTwoSpeed)

      CALL CalcMultiSpeedDXCoil(CoilIndex,SpeedRatio,1.0d0)
      OutletAirHumRat = DXCoilOutletHumRat(CoilIndex)

    CASE (CoilDX_MultiSpeedCooling)

      CycRatio = Par(4)
      SpeedNum   = INT(Par(5))
      FanOpMode  = INT(Par(6))
      CompOp     = INT(Par(7))

      CALL CalcMultiSpeedDXCoilCooling(CoilIndex,SpeedRatio,CycRatio,SpeedNum,FanOpMode,CompOp)
      OutletAirHumRat = DXCoilOutletHumRat(CoilIndex)

    CASE (Coil_CoolingAirToAirVariableSpeed, Coil_CoolingWaterToAirHPVSEquationFit)

      CycRatio   = Par(4)
      SpeedNum   = INT(Par(5))
      FanOpMode  = INT(Par(6))
      CompOp     = INT(Par(7))
      ReqOutput  = Par(8)
      dummy      = 0.0d0
      RuntimeFrac = 1.0d0
      OnOffAirFlowRatio = 1.0d0

      CALL CalcVarSpeedCoilCooling(CoilIndex,FanOpMode,RuntimeFrac,&
                ReqOutput,dummy,CompOp,CycRatio,OnOffAirFlowRatio, SpeedRatio, SpeedNum)

      OutletAirHumRat = Node(UnitarySystem(UnitarySysNum)%CoolCoilOutletNodeNum)%HumRat

    CASE DEFAULT

  END SELECT

  Residuum = Par(2) - OutletAirHumRat

  RETURN
END FUNCTION DXCoilVarSpeedHumRatResidual

FUNCTION DXCoilCyclingResidual(CycRatio, Par) RESULT (Residuum)
          ! FUNCTION INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   September 2002
          !       MODIFIED
          !       RE-ENGINEERED

          ! PURPOSE OF THIS FUNCTION:
          ! Calculates residual function (desired outlet temp - actual outlet temp)
          ! DX Coil output depends on the cycling ratio which is being varied to zero the residual.

          ! METHODOLOGY EMPLOYED:
          ! Calls multi or variable speed coil to get outlet temperature at the given cycling ratio
          ! and calculates the residual as defined above

          ! REFERENCES:

          ! USE STATEMENTS:
  USE DXCoils,             ONLY: DXCoilOutletTemp, CalcMultiSpeedDXCoil, CalcMultiSpeedDXCoilCooling
  USE VariableSpeedCoils,  ONLY: CalcVarSpeedCoilCooling

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
    REAL(r64), INTENT(IN)  :: CycRatio ! compressor cycling ratio (1.0 is continuous, 0.0 is off)
    REAL(r64), INTENT(IN), DIMENSION(:), OPTIONAL :: Par ! par(1) = DX coil number
                                                    ! par(2) = desired air outlet temperature [C]
    REAL(r64)         :: Residuum ! residual to be minimized to zero

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER   :: CoilIndex       ! index of this coil
  REAL(r64) :: OutletAirTemp   ! outlet air temperature [C]
  REAL(R64) :: SpeedRatio
  INTEGER   :: SpeedNum
  INTEGER   :: FanOpMode
  INTEGER   :: CompOp
  INTEGER   :: UnitarySysNum
  REAL(R64) :: ReqOutput
  REAL(R64) :: dummy
  REAL(r64) :: RuntimeFrac
  REAL(r64) :: OnOffAirFlowRatio

!            Par(1) = REAL(UnitarySystem(UnitarySysNum)%CoolingCoilIndex,r64)
!            Par(2) = DesOutTemp
!            Par(3) = UnitarySysNum
!            Par(4) = SpeedRatio
!            Par(5) = UnitarySystem(UnitarySysNum)%CoolingSpeedNum
!            Par(6) = UnitarySystem(UnitarySysNum)%FanOpMode
!            Par(7) = 1.0d0 ! CompOp

  CoilIndex  = INT(Par(1))
  UnitarySysNum = INT(Par(3))

SELECT CASE(UnitarySystem(UnitarySysNum)%CoolingCoilType_Num)

  CASE (CoilDX_CoolingTwoSpeed)

    CALL CalcMultiSpeedDXCoil(CoilIndex,0.0d0,CycRatio)

    OutletAirTemp = DXCoilOutletTemp(CoilIndex)
  CASE (CoilDX_MultiSpeedCooling)

    SpeedRatio = INT(Par(4))
    SpeedNum   = INT(Par(5))
    FanOpMode  = INT(Par(6))
    CompOp     = INT(Par(7))

    CALL CalcMultiSpeedDXCoilCooling(CoilIndex,SpeedRatio,CycRatio,SpeedNum,FanOpMode,CompOp)
    OutletAirTemp = DXCoilOutletTemp(CoilIndex)

  CASE (Coil_CoolingAirToAirVariableSpeed, Coil_CoolingWaterToAirHPVSEquationFit)

      SpeedNum   = INT(Par(5))
      FanOpMode  = INT(Par(6))
      CompOp     = INT(Par(7))
      ReqOutput  = Par(8)
      dummy      = 0.0d0
      RuntimeFrac = 1.0d0
      OnOffAirFlowRatio = 1.0d0

      CALL CalcVarSpeedCoilCooling(CoilIndex,FanOpMode,RuntimeFrac,&
                ReqOutput,dummy,CompOp,CycRatio,OnOffAirFlowRatio, SpeedRatio, SpeedNum)

      OutletAirTemp = Node(UnitarySystem(UnitarySysNum)%CoolCoilOutletNodeNum)%Temp

  CASE DEFAULT

END SELECT

  Residuum = Par(2) - OutletAirTemp

  RETURN
END FUNCTION DXCoilCyclingResidual

FUNCTION HeatingCoilVarSpeedCycResidual(CycRatio, Par) RESULT (Residuum)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   September 2002
          !       MODIFIED
          !       RE-ENGINEERED

          ! PURPOSE OF THIS FUNCTION:
          ! Calculates residual function (desired outlet temp - actual outlet temp)
          ! DX Coil output depends on the cycling ratio which is being varied to zero the residual.

          ! METHODOLOGY EMPLOYED:
          ! Calls multi or variable speed coil to get outlet temperature at the given cycling ratio
          ! and calculates the residual as defined above

          ! REFERENCES:

          ! USE STATEMENTS:
  USE DXCoils,               ONLY: DXCoilOutletTemp, CalcMultiSpeedDXCoil, CalcMultiSpeedDXCoilHeating
  USE VariableSpeedCoils,    ONLY: CalcVarSpeedCoilHeating
  USE HeatingCoils,          ONLY: CalcMultiStageElectricHeatingCoil, CalcMultiStageGasHeatingCoil

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
    REAL(r64), INTENT(IN)  :: CycRatio ! compressor cycling ratio (1.0 is continuous, 0.0 is off)
    REAL(r64), INTENT(IN), DIMENSION(:), OPTIONAL :: Par ! par(1) = DX coil number
                                                    ! par(2) = desired air outlet temperature [C]
    REAL(r64)         :: Residuum ! residual to be minimized to zero

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER   :: CoilIndex       ! index of this coil
  REAL(r64) :: OutletAirTemp   ! outlet air temperature [C]
  REAL(R64) :: SpeedRatio
  INTEGER   :: SpeedNum
  INTEGER   :: FanOpMode
  INTEGER   :: CompOp
  INTEGER   :: UnitarySysNum
  REAL(r64) :: RuntimeFrac
  REAL(r64) :: ReqOutput
  REAL(r64) :: dummy
  REAL(r64) :: OnOffAirFlowRatio

!            Par(1) = REAL(UnitarySystem(UnitarySysNum)%CoolingCoilIndex,r64)
!            Par(2) = DesOutTemp
!            Par(3) = UnitarySysNum
!            Par(4) = SpeedRatio
!            Par(5) = UnitarySystem(UnitarySysNum)%CoolingSpeedNum
!            Par(6) = UnitarySystem(UnitarySysNum)%FanOpMode
!            Par(7) = 1.0d0 ! CompOp

  CoilIndex  = INT(Par(1))
  UnitarySysNum = INT(Par(3))


SELECT CASE(UnitarySystem(UnitarySysNum)%HeatingCoilType_Num)

  CASE (CoilDX_MultiSpeedHeating)

    SpeedRatio = INT(Par(4))
    SpeedNum   = INT(Par(5))
    FanOpMode  = INT(Par(6))
    CompOp     = INT(Par(7))

    CALL CalcMultiSpeedDXCoilHeating(CoilIndex,SpeedRatio,CycRatio,SpeedNum,FanOpMode)
    OutletAirTemp = DXCoilOutletTemp(CoilIndex)

  CASE (Coil_HeatingAirToAirVariableSpeed, Coil_HeatingWaterToAirHPVSEquationFit)

      SpeedNum   = INT(Par(5))
      FanOpMode  = INT(Par(6))
      CompOp     = INT(Par(7))
      ReqOutput  = Par(8)
      dummy      = 0.0d0
      RuntimeFrac = 1.0d0
      OnOffAirFlowRatio = 1.0d0

      CALL CalcVarSpeedCoilHeating(CoilIndex,FanOpMode,RuntimeFrac,&
                ReqOutput,CompOp,CycRatio,OnOffAirFlowRatio, SpeedRatio, SpeedNum)

      OutletAirTemp = Node(UnitarySystem(UnitarySysNum)%HeatCoilOutletNodeNum)%Temp

  CASE (Coil_HeatingElectric_MultiStage)

      SpeedRatio = INT(Par(4))
      SpeedNum   = INT(Par(5))
      FanOpMode  = INT(Par(6))

      CALL CalcMultiStageElectricHeatingCoil(CoilIndex,SpeedRatio,CycRatio,SpeedNum,FanOpMode)

      OutletAirTemp = Node(UnitarySystem(UnitarySysNum)%HeatCoilOutletNodeNum)%Temp

  CASE (Coil_HeatingGas_MultiStage)

      SpeedRatio = INT(Par(4))
      SpeedNum   = INT(Par(5))
      FanOpMode  = INT(Par(6))

      CALL CalcMultiStageElectricHeatingCoil(CoilIndex,SpeedRatio,CycRatio,SpeedNum,FanOpMode)

      OutletAirTemp = Node(UnitarySystem(UnitarySysNum)%HeatCoilOutletNodeNum)%Temp

  CASE DEFAULT


END SELECT

  Residuum = Par(2) - OutletAirTemp

  RETURN
END FUNCTION HeatingCoilVarSpeedCycResidual

FUNCTION DXCoilCyclingHumRatResidual(CycRatio, Par) RESULT (Residuum)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   September 2002
          !       MODIFIED
          !       RE-ENGINEERED

          ! PURPOSE OF THIS FUNCTION:
          ! Calculates residual function (desired outlet temp - actual outlet temp)
          ! DX Coil output depends on the cycling ratio which is being varied to zero the residual.

          ! METHODOLOGY EMPLOYED:
          ! Calls CalcMultiSpeedDXCoil to get outlet temperature at the given cycling ratio
          ! and calculates the residual as defined above

          ! REFERENCES:

          ! USE STATEMENTS:
  USE DXCoils,             ONLY: DXCoilOutletHumRat, CalcMultiSpeedDXCoil, CalcMultiSpeedDXCoilCooling
  USE VariableSpeedCoils,  ONLY: CalcVarSpeedCoilCooling

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
    REAL(r64), INTENT(IN)  :: CycRatio ! compressor cycling ratio (1.0 is continuous, 0.0 is off)
    REAL(r64), INTENT(IN), DIMENSION(:), OPTIONAL :: Par ! par(1) = DX coil number
                                                    ! par(2) = desired air outlet humidity ratio [kg/kg]
    REAL(r64)         :: Residuum ! residual to be minimized to zero

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER   :: CoilIndex       ! index of this coil
  REAL(r64) :: OutletAirHumRat  ! outlet air humidity ratio [kg/kg]
  REAL(R64) :: SpeedRatio
  INTEGER   :: SpeedNum
  INTEGER   :: FanOpMode
  INTEGER   :: CompOp
  INTEGER   :: UnitarySysNum
  REAL(R64) :: ReqOutput
  REAL(R64) :: dummy
  REAL(r64) :: RuntimeFrac
  REAL(r64) :: OnOffAirFlowRatio

  CoilIndex     = INT(Par(1))
  UnitarySysNum = INT(Par(3))

SELECT CASE(UnitarySystem(UnitarySysNum)%CoolingCoilType_Num)

  CASE (CoilDX_CoolingTwoSpeed)

    CALL CalcMultiSpeedDXCoil(CoilIndex,0.0d0,CycRatio)

    OutletAirHumRat = DXCoilOutletHumRat(CoilIndex)
  CASE (CoilDX_MultiSpeedCooling)

    SpeedRatio = INT(Par(4))
    SpeedNum   = INT(Par(5))
    FanOpMode  = INT(Par(6))
    CompOp     = INT(Par(7))

    CALL CalcMultiSpeedDXCoilCooling(CoilIndex,SpeedRatio,CycRatio,SpeedNum,FanOpMode,CompOp)
    OutletAirHumRat = DXCoilOutletHumRat(CoilIndex)

  CASE (Coil_CoolingAirToAirVariableSpeed, Coil_CoolingWaterToAirHPVSEquationFit)

      SpeedNum   = INT(Par(5))
      FanOpMode  = INT(Par(6))
      CompOp     = INT(Par(7))
      ReqOutput  = Par(8)
      dummy      = 0.0d0
      RuntimeFrac = 1.0d0
      OnOffAirFlowRatio = 1.0d0

      CALL CalcVarSpeedCoilCooling(CoilIndex,FanOpMode,RuntimeFrac,&
                ReqOutput,dummy,CompOp,CycRatio,OnOffAirFlowRatio, SpeedRatio, SpeedNum)

      OutletAirHumRat = Node(UnitarySystem(UnitarySysNum)%CoolCoilOutletNodeNum)%HumRat

  CASE DEFAULT

END SELECT

  Residuum = Par(2) - OutletAirHumRat

  RETURN
END FUNCTION DXCoilCyclingHumRatResidual

FUNCTION DOE2DXCoilResidual(PartLoadRatio, Par) RESULT (Residuum)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Richard Raustad, FSEC
          !       DATE WRITTEN   November 2003
          !       MODIFIED
          !       RE-ENGINEERED

          ! PURPOSE OF THIS FUNCTION:
          ! Calculates residual function (desired outlet temp - actual outlet temp)
          ! DX Coil output depends on the part load ratio which is being varied to zero the residual.

          ! METHODOLOGY EMPLOYED:
          ! Calls CalcDOe2DXCoil to get outlet temperature at the given cycling ratio
          ! and calculates the residual as defined above

          ! REFERENCES:

          ! USE STATEMENTS:
  USE DXCoils, ONLY: DXCoilOutletTemp, CalcDOe2DXCoil

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
    REAL(r64), INTENT(IN)  :: PartLoadRatio ! compressor cycling ratio (1.0 is continuous, 0.0 is off)
    REAL(r64), INTENT(IN), DIMENSION(:), OPTIONAL :: Par ! par(1) = DX coil number
                                                    ! par(2) = desired air outlet temperature [C]
                                                    ! par(5) = supply air fan operating mode (ContFanCycCoil)
    REAL(r64)         :: Residuum ! residual to be minimized to zero

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER   :: CoilIndex       ! index of this coil
  REAL(r64) :: OutletAirTemp   ! outlet air temperature [C]
  INTEGER   :: FanOpMode       ! Supply air fan operating mode

  CoilIndex = INT(Par(1))
  FanOpMode = INT(Par(5))
  CALL CalcDOe2DXCoil(CoilIndex,On,.TRUE., PartLoadRatio,FanOpMode)
  OutletAirTemp = DXCoilOutletTemp(CoilIndex)
  Residuum = Par(2) - OutletAirTemp

  RETURN
END FUNCTION DOE2DXCoilResidual

FUNCTION DOE2DXCoilHumRatResidual(PartLoadRatio, Par) RESULT (Residuum)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Richard Raustad, FSEC
          !       DATE WRITTEN   January 2008
          !       MODIFIED
          !       RE-ENGINEERED

          ! PURPOSE OF THIS FUNCTION:
          ! Calculates residual function (desired outlet humrat - actual outlet humrat)
          ! DX Coil output depends on the part load ratio which is being varied to zero the residual.

          ! METHODOLOGY EMPLOYED:
          ! Calls CalcDOe2DXCoil to get outlet humidity ratio at the given cycling ratio
          ! and calculates the residual as defined above

          ! REFERENCES:

          ! USE STATEMENTS:
  USE DXCoils, ONLY: DXCoilOutletHumRat, CalcDOe2DXCoil

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
    REAL(r64), INTENT(IN)  :: PartLoadRatio ! compressor cycling ratio (1.0 is continuous, 0.0 is off)
    REAL(r64), INTENT(IN), DIMENSION(:), OPTIONAL :: Par ! par(1) = DX coil number
                                                    ! par(2) = desired air outlet humidity ratio [kg/kg]
                                                    ! par(5) = supply air fan operating mode (ContFanCycCoil)
    REAL(r64)         :: Residuum ! residual to be minimized to zero

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER   :: CoilIndex       ! index of this coil
  REAL(r64) :: OutletAirHumRat ! outlet air humidity ratio [kg/kg]
  INTEGER   :: FanOpMode       ! Supply air fan operating mode

  CoilIndex = INT(Par(1))
  FanOpMode = INT(Par(5))
  CALL CalcDOe2DXCoil(CoilIndex,On,.TRUE., PartLoadRatio,FanOpMode)
  OutletAirHumRat = DXCoilOutletHumRat(CoilIndex)
  Residuum = Par(2) - OutletAirHumRat

  RETURN
END FUNCTION DOE2DXCoilHumRatResidual

FUNCTION CoolWaterHumRatResidual(PartLoadRatio, Par) RESULT (Residuum)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Chandan Sharma, FSEC
          !       DATE WRITTEN   January 2013
          !       MODIFIED
          !       RE-ENGINEERED

          ! PURPOSE OF THIS FUNCTION:
          ! Calculates residual function (desired outlet humrat - actual outlet humrat)
          ! Cool water coil output depends on the part load ratio which is being varied to zero the residual.

          ! METHODOLOGY EMPLOYED:
          ! Calls SimulateWaterCoilComponents to get outlet temp at the given cycling ratio
          ! and calculates the residual as defined above

          ! REFERENCES:

          ! USE STATEMENTS:
  USE WaterCoils,     ONLY: SimulateWaterCoilComponents

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
    REAL(r64), INTENT(IN)  :: PartLoadRatio ! compressor cycling ratio (1.0 is continuous, 0.0 is off)
    REAL(r64), INTENT(IN), DIMENSION(:), OPTIONAL :: Par ! par(1) = CoolWater coil number
                                                    ! par(2) = desired air outlet humidity ratio [kg/kg]
                                                    ! par(5) = supply air fan operating mode (ContFanCycCoil)
    REAL(r64)         :: Residuum ! residual to be minimized to zero

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER   :: UnitarySysNum       ! index of this coil
  REAL(r64) :: OutletAirHumRat        ! outlet air humidity ratio [kg/kg]
  REAL(r64) :: mdot
  LOGICAL   :: FirstHVACIteration


  UnitarySysNum = INT(Par(1))
  IF (Par(2) > 0.0d0) THEN
    FirstHVACIteration = .TRUE.
  ELSE
    FirstHVACIteration = .FALSE.
  END IF

  mdot = MIN(Node(UnitarySystem(UnitarySysNum)%CoolCoilFluidOutletNodeNum)%MassFlowRateMaxAvail, &
         UnitarySystem(UnitarySysNum)%MaxCoolCoilFluidFlow * PartLoadRatio)
  Node(UnitarySystem(UnitarySysNum)%CoolCoilFluidInletNode)%MassFlowRate = mdot
  CALL SimulateWaterCoilComponents(UnitarySystem(UnitarySysNum)%CoolingCoilName,FirstHVACIteration, &
                                   UnitarySystem(UnitarySysNum)%CoolingCoilIndex, &
                                   PartLoadRatio = PartLoadRatio)

  OutletAirHumRat = Node(UnitarySystem(UnitarySysNum)%CoolCoilOutletNodeNum)%HumRat
  Residuum      = Par(3) - OutletAirHumRat

  RETURN
END FUNCTION CoolWaterHumRatResidual

FUNCTION CoolWaterTempResidual(PartLoadRatio, Par) RESULT (Residuum)
          ! FUNCTION INFORMATION:
          !       AUTHOR         Chandan Sharma, FSEC
          !       DATE WRITTEN   January 2013
          !       MODIFIED
          !       RE-ENGINEERED

          ! PURPOSE OF THIS FUNCTION:
          ! Calculates residual function (desired outlet temp - actual outlet temp)
          ! Cool water coil output depends on the part load ratio which is being varied to zero the residual.

          ! METHODOLOGY EMPLOYED:
          ! Calls SimulateWaterCoilComponents to get outlet temp at the given cycling ratio
          ! and calculates the residual as defined above

          ! REFERENCES:

          ! USE STATEMENTS:
  USE WaterCoils,     ONLY: SimulateWaterCoilComponents

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
    REAL(r64), INTENT(IN)  :: PartLoadRatio ! compressor cycling ratio (1.0 is continuous, 0.0 is off)
    REAL(r64), INTENT(IN), DIMENSION(:), OPTIONAL :: Par ! par(1) = CoolWater coil number
                                                    ! par(2) = desired air outlet humidity ratio [kg/kg]
                                                    ! par(5) = supply air fan operating mode (ContFanCycCoil)
    REAL(r64)         :: Residuum ! residual to be minimized to zero

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER   :: UnitarySysNum       ! index of this coil
  REAL(r64) :: OutletAirTemp        ! outlet air humidity ratio [kg/kg]
  REAL(r64) :: mdot
  LOGICAL   :: FirstHVACIteration


  UnitarySysNum = INT(Par(1))
  IF (Par(2) > 0.0d0) THEN
    FirstHVACIteration = .TRUE.
  ELSE
    FirstHVACIteration = .FALSE.
  END IF

  mdot = MIN(Node(UnitarySystem(UnitarySysNum)%CoolCoilFluidOutletNodeNum)%MassFlowRateMaxAvail, &
         UnitarySystem(UnitarySysNum)%MaxCoolCoilFluidFlow * PartLoadRatio)
  Node(UnitarySystem(UnitarySysNum)%CoolCoilFluidInletNode)%MassFlowRate = mdot
  CALL SimulateWaterCoilComponents(UnitarySystem(UnitarySysNum)%CoolingCoilName,FirstHVACIteration, &
                                   UnitarySystem(UnitarySysNum)%CoolingCoilIndex, &
                                   PartLoadRatio = PartLoadRatio)

  OutletAirTemp = Node(UnitarySystem(UnitarySysNum)%CoolCoilOutletNodeNum)%Temp
  Residuum      = Par(3) - OutletAirTemp

  RETURN
END FUNCTION CoolWaterTempResidual

FUNCTION CoolWatertoAirHPHumRatResidual(PartLoadRatio, Par) RESULT (Residuum)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Chandan Sharma, FSEC
          !       DATE WRITTEN   January 2013
          !       MODIFIED
          !       RE-ENGINEERED

          ! PURPOSE OF THIS FUNCTION:
          ! Calculates residual function (desired outlet humrat - actual outlet humrat)
          ! Cool water coil output depends on the part load ratio which is being varied to zero the residual.

          ! METHODOLOGY EMPLOYED:
          ! Calls SimWatertoAirHP or SimWatertoAirHPSimple to get outlet humidity ratio at the given cycling ratio
          ! and calculates the residual as defined above

          ! REFERENCES:

          ! USE STATEMENTS:
  USE WatertoAirHeatPumpSimple,  ONLY: SimWatertoAirHPSimple
  USE WatertoAirHeatPump,        ONLY: SimWaterToAirHP

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
    REAL(r64), INTENT(IN)  :: PartLoadRatio ! compressor cycling ratio (1.0 is continuous, 0.0 is off)
    REAL(r64), INTENT(IN), DIMENSION(:), OPTIONAL :: Par ! par(1) = CoolWatertoAirHP coil number
                                                    ! par(2) = desired air outlet humidity ratio [kg/kg]
                                                    ! par(5) = supply air fan operating mode (ContFanCycCoil)
    REAL(r64)         :: Residuum ! residual to be minimized to zero

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER   :: UnitarySysNum       ! index of this coil
  REAL(r64) :: OutletAirHumRat        ! outlet air humidity ratio [kg/kg]
  REAL(r64) :: ReqOutput
  LOGICAL   :: FirstHVACIteration
  LOGICAL   :: errflag
  REAL(r64)  :: RuntimeFrac        ! heat pump runtime fraction
  REAL(r64)  :: Dummy


  UnitarySysNum = INT(Par(1))
  IF (Par(2) > 0.0d0) THEN
    FirstHVACIteration = .TRUE.
  ELSE
    FirstHVACIteration = .FALSE.
  END IF
  ReqOutput = Par(4)

  CALL HeatPumpRunFrac(UnitarySysNum,PartLoadRatio,errflag,RuntimeFrac)

  UnitarySystem(UnitarySysNum)%CompPartLoadRatio = PartLoadRatio
  UnitarySystem(UnitarySysNum)%WSHPRuntimeFrac   = RuntimeFrac

  Dummy = 0.0d0
  IF (UnitarySystem(UnitarySysNum)%CoolingCoilType_Num == Coil_CoolingWaterToAirHPSimple) THEN
    CALL SimWatertoAirHPSimple(Blank, UnitarySystem(UnitarySysNum)%CoolingCoilIndex, ReqOutput, Dummy, &
                               UnitarySystem(UnitarySysNum)%FanOpMode,RuntimeFrac, &
                               UnitarySystem(UnitarySysNum)%MaxONOFFCyclesperHour, &
                               UnitarySystem(UnitarySysNum)%HPTimeConstant, UnitarySystem(UnitarySysNum)%FanDelayTime, &
                               0, PartLoadRatio, FirstHVACIteration)
  ELSE
    CALL SimWatertoAirHP(Blank, UnitarySystem(UnitarySysNum)%CoolingCoilIndex, &
                         UnitarySystem(UnitarySysNum)%MaxCoolAirMassFlow,UnitarySystem(UnitarySysNum)%FanOpMode, &
                         FirstHVACIteration,RuntimeFrac,UnitarySystem(UnitarySysNum)%MaxONOFFCyclesperHour, &
                         UnitarySystem(UnitarySysNum)%HPTimeConstant, UnitarySystem(UnitarySysNum)%FanDelayTime, &
                         UnitarySystem(UnitarySysNum)%InitHeatPump, ReqOutput,Dummy,0, PartLoadRatio)
  END IF

  OutletAirHumRat = Node(UnitarySystem(UnitarySysNum)%CoolCoilOutletNodeNum)%HumRat
  Residuum      = Par(3) - OutletAirHumRat

  RETURN
END FUNCTION CoolWatertoAirHPHumRatResidual

FUNCTION CoolWatertoAirHPTempResidual(PartLoadRatio, Par) RESULT (Residuum)

          ! FUNCTION INFORMATION:
          !       AUTHOR        Chandan Sharma, FSEC
          !       DATE WRITTEN   January 2013
          !       MODIFIED
          !       RE-ENGINEERED

          ! PURPOSE OF THIS FUNCTION:
          ! Calculates residual function (desired outlet temp - actual outlet temp)
          ! Cool water coil output depends on the part load ratio which is being varied to zero the residual.

          ! METHODOLOGY EMPLOYED:
          ! Calls SimWatertoAirHP or SimWatertoAirHPSimple to get outlet humidity ratio at the given cycling ratio
          ! and calculates the residual as defined above

          ! REFERENCES:

          ! USE STATEMENTS:
  USE WatertoAirHeatPumpSimple,  ONLY: SimWatertoAirHPSimple
  USE WatertoAirHeatPump,        ONLY: SimWaterToAirHP

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
    REAL(r64), INTENT(IN)  :: PartLoadRatio ! compressor cycling ratio (1.0 is continuous, 0.0 is off)
    REAL(r64), INTENT(IN), DIMENSION(:), OPTIONAL :: Par ! par(1) = CoolWatertoAirHP coil number
                                                    ! par(2) = desired air outlet humidity ratio [kg/kg]
                                                    ! par(5) = supply air fan operating mode (ContFanCycCoil)
    REAL(r64)         :: Residuum ! residual to be minimized to zero

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER   :: UnitarySysNum       ! index of this coil
  REAL(r64) :: OutletAirTemp        ! outlet air humidity ratio [kg/kg]
  REAL(r64) :: ReqOutput
  LOGICAL   :: FirstHVACIteration
  LOGICAL   :: errflag
  REAL(r64) :: RuntimeFrac
  REAL(r64) :: Dummy

  UnitarySysNum = INT(Par(1))
  IF (Par(2) > 0.0d0) THEN
    FirstHVACIteration = .TRUE.
  ELSE
    FirstHVACIteration = .FALSE.
  END IF
  ReqOutput = Par(4)

  CALL HeatPumpRunFrac(UnitarySysNum,PartLoadRatio,errflag,RuntimeFrac)

  IF(RuntimeFrac > 0.0d0 .AND. UnitarySystem(UnitarySysNum)%FanOpMode == CycFanCycCoil)THEN
    OnOffFanPartLoadFraction = PartLoadRatio/RuntimeFrac
  ELSE
    OnOffFanPartLoadFraction = 1
  END IF

  UnitarySystem(UnitarySysNum)%CompPartLoadRatio = PartLoadRatio
  UnitarySystem(UnitarySysNum)%WSHPRuntimeFrac   = RuntimeFrac

  Dummy = 0.0d0
  IF (UnitarySystem(UnitarySysNum)%CoolingCoilType_Num == Coil_CoolingWaterToAirHPSimple) THEN
    CALL SimWatertoAirHPSimple(Blank, UnitarySystem(UnitarySysNum)%CoolingCoilIndex, ReqOutput, Dummy, &
                               UnitarySystem(UnitarySysNum)%FanOpMode,RuntimeFrac, &
                               UnitarySystem(UnitarySysNum)%MaxONOFFCyclesperHour, &
                               UnitarySystem(UnitarySysNum)%HPTimeConstant, UnitarySystem(UnitarySysNum)%FanDelayTime, &
                               0, PartLoadRatio, FirstHVACIteration)
  ELSE
    CALL SimWatertoAirHP(Blank, UnitarySystem(UnitarySysNum)%CoolingCoilIndex, &
                         UnitarySystem(UnitarySysNum)%MaxCoolAirMassFlow,UnitarySystem(UnitarySysNum)%FanOpMode, &
                         FirstHVACIteration,RuntimeFrac,UnitarySystem(UnitarySysNum)%MaxONOFFCyclesperHour, &
                         UnitarySystem(UnitarySysNum)%HPTimeConstant, UnitarySystem(UnitarySysNum)%FanDelayTime, &
                         UnitarySystem(UnitarySysNum)%InitHeatPump, ReqOutput,Dummy,0, PartLoadRatio)
  END IF

  OutletAirTemp = Node(UnitarySystem(UnitarySysNum)%CoolCoilOutletNodeNum)%Temp
  Residuum      = Par(3) - OutletAirTemp

  RETURN
END FUNCTION CoolWatertoAirHPTempResidual

FUNCTION HeatWatertoAirHPTempResidual(PartLoadRatio, Par) RESULT (Residuum)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Chandan Sharma, FSEC
          !       DATE WRITTEN   January 2013
          !       MODIFIED
          !       RE-ENGINEERED

          ! PURPOSE OF THIS FUNCTION:
          ! Calculates residual function (desired outlet temp - actual outlet temp)
          ! Heat water coil output depends on the part load ratio which is being varied to zero the residual.

          ! METHODOLOGY EMPLOYED:
          ! Calls SimWatertoAirHP or SimWatertoAirHPSimple to get outlet humidity ratio at the given cycling ratio
          ! and calculates the residual as defined above

          ! REFERENCES:

          ! USE STATEMENTS:
  USE WatertoAirHeatPumpSimple,  ONLY: SimWatertoAirHPSimple
  USE WatertoAirHeatPump,        ONLY: SimWaterToAirHP

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
    REAL(r64), INTENT(IN)  :: PartLoadRatio ! compressor cycling ratio (1.0 is continuous, 0.0 is off)
    REAL(r64), INTENT(IN), DIMENSION(:), OPTIONAL :: Par ! par(1) = HeatWatertoAirHP coil number
                                                    ! par(2) = desired air outlet humidity ratio [kg/kg]
                                                    ! par(5) = supply air fan operating mode (ContFanCycCoil)
    REAL(r64)         :: Residuum ! residual to be minimized to zero

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER   :: UnitarySysNum       ! index of this coil
  REAL(r64) :: OutletAirTemp        ! outlet air humidity ratio [kg/kg]
  REAL(r64) :: ReqOutput
  LOGICAL   :: FirstHVACIteration
  LOGICAL   :: errflag
  REAL(r64) :: RuntimeFrac
  REAL(r64) :: Dummy

  UnitarySysNum = INT(Par(1))
  IF (Par(2) > 0.0d0) THEN
    FirstHVACIteration = .TRUE.
  ELSE
    FirstHVACIteration = .FALSE.
  END IF
  ReqOutput = Par(1)

  CALL HeatPumpRunFrac(UnitarySysNum,PartLoadRatio,errflag,RuntimeFrac)

  IF(RuntimeFrac > 0.0d0 .AND. UnitarySystem(UnitarySysNum)%FanOpMode == CycFanCycCoil)THEN
    OnOffFanPartLoadFraction = PartLoadRatio/RuntimeFrac
  ELSE
    OnOffFanPartLoadFraction = 1
  END IF

  UnitarySystem(UnitarySysNum)%CompPartLoadRatio = PartLoadRatio
  UnitarySystem(UnitarySysNum)%WSHPRuntimeFrac   = RuntimeFrac

  Dummy = 0.0d0
  IF (UnitarySystem(UnitarySysNum)%HeatingCoilType_Num == Coil_HeatingWaterToAirHPSimple) THEN
    CALL SimWatertoAirHPSimple(Blank, UnitarySystem(UnitarySysNum)%HeatingCoilIndex, ReqOutput, Dummy, &
                               UnitarySystem(UnitarySysNum)%FanOpMode,RuntimeFrac, &
                               UnitarySystem(UnitarySysNum)%MaxONOFFCyclesperHour, &
                               UnitarySystem(UnitarySysNum)%HPTimeConstant, UnitarySystem(UnitarySysNum)%FanDelayTime, &
                               0, PartLoadRatio, FirstHVACIteration)
  ELSE
    CALL SimWatertoAirHP(Blank, UnitarySystem(UnitarySysNum)%HeatingCoilIndex, &
                         UnitarySystem(UnitarySysNum)%MaxHeatAirMassFlow,UnitarySystem(UnitarySysNum)%FanOpMode, &
                         FirstHVACIteration,RuntimeFrac,UnitarySystem(UnitarySysNum)%MaxONOFFCyclesperHour, &
                         UnitarySystem(UnitarySysNum)%HPTimeConstant, UnitarySystem(UnitarySysNum)%FanDelayTime, &
                         UnitarySystem(UnitarySysNum)%InitHeatPump, ReqOutput,Dummy,0, PartLoadRatio)
  END IF

  OutletAirTemp = Node(UnitarySystem(UnitarySysNum)%HeatCoilOutletNodeNum)%Temp
  Residuum      = Par(3) - OutletAirTemp

  RETURN
END FUNCTION HeatWatertoAirHPTempResidual

SUBROUTINE HeatPumpRunFrac(UnitarySysNum,PLR,errflag,RuntimeFrac)

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
          ! Ernest OrlanDO Lawrence Berkeley National Laboratory.


          ! USE STATEMENTS:


  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine


          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER,   INTENT(IN)           :: UnitarySysNum       ! UnitarySystem Index Number
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
  REAL(r64) :: A              ! Variable for simplIFy equation
  INTEGER   :: NumIteration   ! Iteration Counter

  Nmax=UnitarySystem(UnitarySysNum)%MaxONOFFCyclesperHour
  tau=UnitarySystem(UnitarySysNum)%HPTimeConstant
  pr=UnitarySystem(UnitarySysNum)%OnCyclePowerFraction

  !Initialize
  errflag = .FALSE.
  error = 1
  NumIteration = 0

  !Initial guess for part load fraction
  PLF1 = 1

  !Calculate PLF using successive substitution until convergence
  !is achieved
  LOOPPLF: DO
    NumIteration=NumIteration + 1

    IF (PLR.EQ.1) THEN
        ! Set part load fraction, PLF1=1.0 IF PLR=1.0 and EXIT loop
        PLF1 = 1
        EXIT LOOPPLF
    END IF

    IF (NumIteration.GT.100)THEN
        ! EXIT loop IF interation exceed 100
        errflag = .TRUE.
        PLF1 = 1
        EXIT LOOPPLF
    END IF

    IF (error.LT.0.00001d0)THEN
        ! EXIT loop IF convergence is achieved
        EXIT LOOPPLF

    ELSE
        ! Calculate PLF
        A = 4.d0 * tau * (Nmax/3600.0d0) * (1 - PLR / PLF1)
        IF (A.LT.1.5d-3) THEN
            ! A safety check to prevent PLF2 = 1 - A * (1 - Exp(-1 / A))
            ! from "float underflow error". Occurs when PLR is very close to 1.0,
            ! small A value, thus Exp(-1/A) = 0
            PLF2 = 1.0d0 - A
        ELSE
            PLF2 = 1.0d0 - A * (1.0d0 - Exp(-1.0d0 / A))
        END IF
        error = ABS((PLF2 - PLF1) / PLF1)
        PLF1 = PLF2
     END IF
  END DO LOOPPLF

  !Adjust PLF for the off cycle power consumption IF
  !on-cycle power use is specified by the user
  IF (pr>0.0d0) THEN
    PartLoadFactor = PLR / ((PLR / PLF1) + (1.0d0 - PLR / PLF1) * pr)
  ELSE
    PartLoadFactor=PLF1
  END IF

  IF (PartLoadFactor <= 0.0d0)THEN
     PartLoadFactor = 0
     RuntimeFrac = 0
     errflag = .TRUE.
  ELSE
     RuntimeFrac = PLR / PartLoadFactor
  END IF

  IF (RuntimeFrac > 1.0d0 ) THEN
     RuntimeFrac = 1.0d0
  END IF

 RETURN

END SUBROUTINE HeatPumpRunFrac

FUNCTION MultiModeDXCoilResidual(PartLoadRatio, Par) RESULT (Residuum)

          ! FUNCTION INFORMATION:
          !       AUTHOR         M. J. Witte, GARD Analytics, Inc.
          !       DATE WRITTEN   February 2005
          !                      (based on DOE2DXCoilResidual by Richard Raustad, FSEC)
          !       MODIFIED
          !       RE-ENGINEERED

          ! PURPOSE OF THIS FUNCTION:
          ! Calculates residual function (desired outlet temp - actual outlet temp)
          ! DX Coil output depends on the part load ratio which is being varied to zero the residual.

          ! METHODOLOGY EMPLOYED:
          ! Calls SimDXCoilMultiMode to get outlet temperature at the given cycling ratio
          ! and calculates the residual as defined above

          ! REFERENCES:

          ! USE STATEMENTS:
  USE DXCoils, ONLY: DXCoilOutletTemp, SimDXCoilMultiMode

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
    REAL(r64), INTENT(IN)  :: PartLoadRatio ! compressor cycling ratio (1.0 is continuous, 0.0 is off)
    REAL(r64), INTENT(IN), DIMENSION(:), OPTIONAL :: Par ! par(1) = DX coil number
                                                    ! par(2) = desired air outlet temperature [C]
                                                    ! par(3) = dehumidification mode (0=normal, 1=enhanced)
                                                    ! par(4) = supply air fan operating mode (ContFanCycCoil)
    REAL(r64)         :: Residuum ! residual to be minimized to zero

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER   :: CoilIndex       ! index of this coil
  REAL(r64) :: OutletAirTemp   ! outlet air temperature [C]
  INTEGER   :: DehumidMode     ! dehumidification mode (par3)
  INTEGER   :: FanOpMode       ! supply air fan operating mode

  CoilIndex   = INT(Par(1))
  DehumidMode = INT(Par(3))
  FanOpMode   = INT(Par(4))
  CALL SimDXCoilMultiMode('',On,.FALSE.,PartLoadRatio,DehumidMode,CoilIndex,FanOpMode)
  OutletAirTemp = DXCoilOutletTemp(CoilIndex)
  Residuum = Par(2) - OutletAirTemp

  RETURN
END FUNCTION MultiModeDXCoilResidual

FUNCTION MultiModeDXCoilHumRatResidual(PartLoadRatio, Par) RESULT (Residuum)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Richard Raustad, FSEC
          !       DATE WRITTEN   January 2008
          !       MODIFIED
          !       RE-ENGINEERED

          ! PURPOSE OF THIS FUNCTION:
          ! Calculates residual function (desired outlet humrat - actual outlet humrat)
          ! DX Coil output depends on the part load ratio which is being varied to zero the residual.

          ! METHODOLOGY EMPLOYED:
          ! Calls SimDXCoilMultiMode to get outlet humidity ratio at the given cycling ratio
          ! and calculates the residual as defined above

          ! REFERENCES:

          ! USE STATEMENTS:
  USE DXCoils, ONLY: DXCoilOutletHumRat, SimDXCoilMultiMode

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
    REAL(r64), INTENT(IN)  :: PartLoadRatio ! compressor cycling ratio (1.0 is continuous, 0.0 is off)
    REAL(r64), INTENT(IN), DIMENSION(:), OPTIONAL :: Par ! par(1) = DX coil number
                                                    ! par(2) = desired air outlet humidity ratio [kg/kg]
                                                    ! par(3) = dehumidification mode (0=normal, 1=enhanced)
                                                    ! par(4) = supply air fan operating mode (ContFanCycCoil)
    REAL(r64)         :: Residuum ! residual to be minimized to zero

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER   :: CoilIndex       ! index of this coil
  REAL(r64) :: OutletAirHumRat ! outlet air humidity ratio [kg/kg]
  INTEGER   :: DehumidMode     ! dehumidification mode (par3)
  INTEGER   :: FanOpMode       ! supply air fan operating mode

  CoilIndex   = INT(Par(1))
  DehumidMode = INT(Par(3))
  FanOpMode   = INT(Par(4))
  CALL SimDXCoilMultiMode('',On,.FALSE.,PartLoadRatio,DehumidMode,CoilIndex,FanOpMode)
  OutletAirHumRat = DXCoilOutletHumRat(CoilIndex)
  Residuum = Par(2) - OutletAirHumRat

  RETURN
END FUNCTION MultiModeDXCoilHumRatResidual

FUNCTION HXAssistedCoolCoilTempResidual(PartLoadRatio, Par) RESULT (Residuum)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Richard Raustad, FSEC
          !       DATE WRITTEN   November 2003
          !       MODIFIED
          !       RE-ENGINEERED

          ! PURPOSE OF THIS FUNCTION:
          !  Calculates residual function (desired outlet temp - actual outlet temp)
          !  DX Coil output depends on the part load ratio which is being varied to zero the residual.

          ! METHODOLOGY EMPLOYED:
          !  Calls CalcHXAssistedCoolingCoil to get outlet temperature at the given part load ratio
          !  and calculates the residual as defined above

          ! REFERENCES:

          ! USE STATEMENTS:
  USE HVACHXAssistedCoolingCoil, ONLY: HXAssistedCoilOutletTemp, CalcHXAssistedCoolingCoil

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  REAL(r64), INTENT(IN)     :: PartLoadRatio ! compressor cycling ratio (1.0 is continuous, 0.0 is off)
  REAL(r64), INTENT(IN), DIMENSION(:), OPTIONAL :: Par ! par(1) = DX coil number
                                                  ! par(2) = desired air outlet temperature [C]
                                                  ! par(3) = FirstHVACIteration logical converted to numeric (1=TRUE,0=FALSE)
                                                  ! par(4) = HX control (On/Off)
                                                  ! par(5) = supply air fan operating mode (ContFanCycCoil)
  REAL(r64)         :: Residuum  ! residual to be minimized to zero

          ! FUNCTION PARAMETER DEFINITIONS:
          !  na

          ! INTERFACE BLOCK SPECIFICATIONS
          !  na

          ! DERIVED TYPE DEFINITIONS
          !  na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER   :: CoilIndex          ! index of this coil
  REAL(r64) :: OutletAirTemp      ! outlet air temperature [C]
  LOGICAL   :: FirstHVACIteration ! FirstHVACIteration flag
  LOGICAL   :: HXUnitOn           ! flag to enable heat exchanger heat recovery
  INTEGER   :: FanOpMode          ! Supply air fan operating mode
  INTEGER   :: UnitarySysNum      ! index to unitary system

  CoilIndex = INT(Par(1))
  ! FirstHVACIteration is a logical, Par is REAL(r64), so make 1=TRUE and 0=FALSE
  IF(Par(3) .EQ. 1.0d0)THEN
    FirstHVACIteration = .TRUE.
  ELSE
    FirstHVACIteration = .FALSE.
  END IF
  IF(Par(4) .EQ. 1.0d0)THEN
    HXUnitOn = .TRUE.
  ELSE
    HXUnitOn = .FALSE.
  END IF
  FanOpMode = INT(Par(5))
  UnitarySysNum = INT(Par(6))
  IF(UnitarySystem(UnitarySysNum)%CoolCoilFluidInletNode .GT. 0)THEN
    Node(UnitarySystem(UnitarySysNum)%CoolCoilFluidInletNode)%MassFlowRate = &
      UnitarySystem(UnitarySysNum)%MaxCoolCoilFluidFlow * PartLoadRatio
  END IF
  CALL CalcHXAssistedCoolingCoil(CoilIndex,FirstHVACIteration,On,PartLoadRatio, HXUnitOn, FanOpMode)
  OutletAirTemp = HXAssistedCoilOutletTemp(CoilIndex)
  Residuum = Par(2) - OutletAirTemp
  RETURN

END FUNCTION HXAssistedCoolCoilTempResidual

FUNCTION HXAssistedCoolCoilHRResidual(PartLoadRatio, Par) RESULT (Residuum)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Richard Raustad, FSEC
          !       DATE WRITTEN   January 2008
          !       MODIFIED
          !       RE-ENGINEERED

          ! PURPOSE OF THIS FUNCTION:
          !  Calculates residual function (desired outlet humrat - actual outlet humrat)
          !  DX Coil output depends on the part load ratio which is being varied to zero the residual.

          ! METHODOLOGY EMPLOYED:
          !  Calls CalcHXAssistedCoolingCoil to get outlet humidity ratio at the given part load ratio
          !  and calculates the residual as defined above

          ! REFERENCES:

          ! USE STATEMENTS:
  USE HVACHXAssistedCoolingCoil, ONLY: HXAssistedCoilOutletHumRat, CalcHXAssistedCoolingCoil

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  REAL(r64), INTENT(IN)     :: PartLoadRatio ! compressor cycling ratio (1.0 is continuous, 0.0 is off)
  REAL(r64), INTENT(IN), DIMENSION(:), OPTIONAL :: Par ! par(1) = DX coil number
                                                  ! par(2) = desired air outlet humidity ratio [kg/kg]
                                                  ! par(3) = FirstHVACIteration logical converted to numeric (1=TRUE,0=FALSE)
                                                  ! par(4) = HX control (On/Off)
                                                  ! par(5) = supply air fan operating mode (ContFanCycCoil)
  REAL(r64)         :: Residuum  ! residual to be minimized to zero

          ! FUNCTION PARAMETER DEFINITIONS:
          !  na

          ! INTERFACE BLOCK SPECIFICATIONS
          !  na

          ! DERIVED TYPE DEFINITIONS
          !  na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER   :: CoilIndex          ! index of this coil
  REAL(r64) :: OutletAirHumRat    ! outlet air humidity ratio [kg/kg]
  LOGICAL   :: FirstHVACIteration ! FirstHVACIteration flag
  LOGICAL   :: HXUnitOn           ! flag to enable heat exchanger heat recovery
  INTEGER   :: FanOpMode          ! Supply air fan operating mode

  CoilIndex = INT(Par(1))
  ! FirstHVACIteration is a logical, Par is REAL(r64), so make 1=TRUE and 0=FALSE
  IF(Par(3) .EQ. 1.0d0)THEN
    FirstHVACIteration = .TRUE.
  ELSE
    FirstHVACIteration = .FALSE.
  END IF
  IF(Par(4) .EQ. 1.0d0)THEN
    HXUnitOn = .TRUE.
  ELSE
    HXUnitOn = .FALSE.
  END IF
  FanOpMode = INT(Par(5))
  CALL CalcHXAssistedCoolingCoil(CoilIndex,FirstHVACIteration,On,PartLoadRatio, HXUnitOn, FanOpMode, &
                                 EconomizerFlag=EconomizerFlag)
  OutletAirHumRat = HXAssistedCoilOutletHumRat(CoilIndex)
  Residuum = Par(2) - OutletAirHumRat
  RETURN

END FUNCTION HXAssistedCoolCoilHRResidual

FUNCTION GasElecHeatingCoilResidual(PartLoadFrac, Par) RESULT (Residuum)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Chandan Sharma, FSEC
          !       DATE WRITTEN   February 2013
          !       MODIFIED
          !       RE-ENGINEERED

          ! PURPOSE OF THIS FUNCTION:
          ! Calculates residual function (desired outlet temp - actual outlet temp)
          ! hot water Coil output depends on the part load ratio which is being varied to zero the residual.

          ! METHODOLOGY EMPLOYED:
          ! Calls SimulateHeatingCoilComponents to get outlet temperature at the given part load ratio
          ! and calculates the residual as defined above

          ! REFERENCES:

          ! USE STATEMENTS:
  USE HeatingCoils,    ONLY: SimulateHeatingCoilComponents

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
    REAL(r64), INTENT(IN)  :: PartLoadFrac               ! Compressor cycling ratio (1.0 is continuous, 0.0 is off)
    REAL(r64), INTENT(IN), DIMENSION(:), OPTIONAL :: Par ! Par(1) = DX coil number
                                                    ! Par(2) = desired air outlet temperature [C]
    REAL(r64)         :: Residuum                   ! Residual to be minimized to zero

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  REAL(r64)    :: OutletAirTemp    ! Outlet air temperature [C]
  INTEGER      :: UnitarySysNum
  INTEGER      :: FanOpMode              =0   ! Fan operating mode (see parameter above)
  LOGICAL      :: FirstHVACIteration
  LOGICAL      :: SuppHeatingCoilFlag

  UnitarySysNum = INT(Par(1))
  IF (Par(2) > 0.0d0) THEN
    FirstHVACIteration = .TRUE.
  ELSE
    FirstHVACIteration = .FALSE.
  END IF
  IF (Par(4) > 0.0d0) THEN
    SuppHeatingCoilFlag = .TRUE.
  ELSE
    SuppHeatingCoilFlag = .FALSE.
  END IF
  FanOpMode = Par(4)
  IF (.NOT. SuppHeatingCoilFlag) THEN
    CALL SimulateHeatingCoilComponents(UnitarySystem(UnitarySysNum)%HeatingCoilName,FirstHVACIteration, &
                                       CompIndex=UnitarySystem(UnitarySysNum)%HeatingCoilIndex, &
                                       FanOpMode=FanOpMode, &
                                       PartLoadRatio = PartLoadFrac)
    OutletAirTemp = Node(UnitarySystem(UnitarySysNum)%HeatCoilOutletNodeNum)%Temp
  ELSE
    CALL SimulateHeatingCoilComponents(UnitarySystem(UnitarySysNum)%SuppHeatCoilName,FirstHVACIteration, &
                                       CompIndex=UnitarySystem(UnitarySysNum)%SuppHeatCoilIndex, &
                                       FanOpMode=FanOpMode, &
                                       PartLoadRatio = PartLoadFrac, SuppHeat=.TRUE.)
    OutletAirTemp = Node(UnitarySystem(UnitarySysNum)%SuppCoilAirOutletNode)%Temp
  END IF
  Residuum      = Par(3) - OutletAirTemp

  RETURN
END FUNCTION GasElecHeatingCoilResidual

FUNCTION HotWaterHeatingCoilResidual(PartLoadFrac, Par) RESULT (Residuum)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Chandan Sharma, FSEC
          !       DATE WRITTEN   February 2013
          !       MODIFIED
          !       RE-ENGINEERED

          ! PURPOSE OF THIS FUNCTION:
          ! Calculates residual function (desired outlet temp - actual outlet temp)
          ! hot water Coil output depends on the part load ratio which is being varied to zero the residual.

          ! METHODOLOGY EMPLOYED:
          ! Calls SimulateWaterCoilComponents to get outlet temperature at the given part load ratio
          ! and calculates the residual as defined above

          ! REFERENCES:

          ! USE STATEMENTS:
  USE WaterCoils,       ONLY: SimulateWaterCoilComponents

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
    REAL(r64), INTENT(IN)  :: PartLoadFrac               ! Compressor cycling ratio (1.0 is continuous, 0.0 is off)
    REAL(r64), INTENT(IN), DIMENSION(:), OPTIONAL :: Par ! Par(1) = DX coil number
                                                    ! Par(2) = desired air outlet temperature [C]
    REAL(r64)         :: Residuum                   ! Residual to be minimized to zero

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  REAL(r64)    :: OutletAirTemp    ! Outlet air temperature [C]
  REAL(r64)    :: mdot             ! water-side flow rate of HW coil [kg/s]
  REAL(r64)    :: QActual          ! heating capacity of HW coil [W]
  INTEGER      :: UnitarySysNum    ! index to unitary system
  LOGICAL      :: FirstHVACIteration ! iteration flag
  LOGICAL      :: SuppHeatingCoilFlag ! TRUE if supplemental heating coil
  LOGICAL      :: LoadBased        ! TRUE if controlling to load, else control to temperature

  UnitarySysNum = INT(Par(1))
  IF (Par(2) > 0.0d0) THEN
    FirstHVACIteration = .TRUE.
  ELSE
    FirstHVACIteration = .FALSE.
  END IF
  IF (Par(4) > 0.0d0) THEN
    SuppHeatingCoilFlag = .TRUE.
  ELSE
    SuppHeatingCoilFlag = .FALSE.
  END IF
  IF(Par(5) > 0.0d0)THEN
    LoadBased = .TRUE.
  ELSE
    LoadBased = .FALSE.
  END IF
  QActual = 0.0d0
  IF (.NOT. SuppHeatingCoilFlag) THEN
    mdot = MIN(Node(UnitarySystem(UnitarySysNum)%HeatCoilFluidOutletNodeNum)%MassFlowRateMaxAvail, &
           UnitarySystem(UnitarySysNum)%MaxHeatCoilFluidFlow * PartLoadFrac)
    Node(UnitarySystem(UnitarySysNum)%HeatCoilFluidInletNode)%MassFlowRate = mdot
    CALL SimulateWaterCoilComponents(UnitarySystem(UnitarySysNum)%HeatingCoilName,FirstHVACIteration, &
                                     UnitarySystem(UnitarySysNum)%HeatingCoilIndex,QActual=QActual, &
                                     FanOpMode=UnitarySystem(UnitarySysNum)%FanOpMode,PartLoadRatio = PartLoadFrac)
    OutletAirTemp = Node(UnitarySystem(UnitarySysNum)%HeatCoilOutletNodeNum)%Temp
  ELSE
    mdot = MIN(Node(UnitarySystem(UnitarySysNum)%SuppCoilFluidOutletNodeNum)%MassFlowRateMaxAvail, &
           UnitarySystem(UnitarySysNum)%MaxSuppCoilFluidFlow * PartLoadFrac)
    Node(UnitarySystem(UnitarySysNum)%SuppCoilFluidInletNode)%MassFlowRate = mdot
    CALL SimulateWaterCoilComponents(UnitarySystem(UnitarySysNum)%SuppHeatCoilName,FirstHVACIteration, &
                                     UnitarySystem(UnitarySysNum)%SuppHeatCoilIndex,QActual=QActual, &
                                     FanOpMode=UnitarySystem(UnitarySysNum)%FanOpMode,PartLoadRatio = PartLoadFrac)
    OutletAirTemp = Node(UnitarySystem(UnitarySysNum)%SuppCoilAirOutletNode)%Temp
  END IF
  IF(LoadBased)THEN
    Residuum      = Par(3) - QActual
  ELSE
    Residuum      = Par(3) - OutletAirTemp
  END IF

  RETURN
END FUNCTION HotWaterHeatingCoilResidual

FUNCTION SteamHeatingCoilResidual(PartLoadFrac, Par) RESULT (Residuum)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Chandan Sharma, FSEC
          !       DATE WRITTEN   February 2013
          !       MODIFIED
          !       RE-ENGINEERED

          ! PURPOSE OF THIS FUNCTION:
          ! Calculates residual function (desired outlet temp - actual outlet temp)
          ! hot Steam Coil output depends on the part load ratio which is being varied to zero the residual.

          ! METHODOLOGY EMPLOYED:
          ! Calls SimulateSteamCoilComponents to get outlet temperature at the given part load ratio
          ! and calculates the residual as defined above

          ! REFERENCES:

          ! USE STATEMENTS:
  USE SteamCoils,       ONLY: SimulateSteamCoilComponents

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
    REAL(r64), INTENT(IN)  :: PartLoadFrac               ! Compressor cycling ratio (1.0 is continuous, 0.0 is off)
    REAL(r64), INTENT(IN), DIMENSION(:), OPTIONAL :: Par ! Par(1) = DX coil number
                                                    ! Par(2) = desired air outlet temperature [C]
    REAL(r64)         :: Residuum                   ! Residual to be minimized to zero

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  REAL(r64)    :: OutletAirTemp    ! Outlet air temperature [C]
  REAL(r64)    :: mdot
  INTEGER      :: UnitarySysNum
  LOGICAL      :: FirstHVACIteration
  LOGICAL      :: SuppHeatingCoilFlag

  UnitarySysNum = INT(Par(1))
  IF (Par(2) > 0.0d0) THEN
    FirstHVACIteration = .TRUE.
  ELSE
    FirstHVACIteration = .FALSE.
  END IF
  IF (Par(4) > 0.0d0) THEN
    SuppHeatingCoilFlag = .TRUE.
  ELSE
    SuppHeatingCoilFlag = .FALSE.
  END IF

  IF (.NOT. SuppHeatingCoilFlag) THEN
    mdot = MIN(Node(UnitarySystem(UnitarySysNum)%HeatCoilFluidOutletNodeNum)%MassFlowRateMaxAvail, &
           UnitarySystem(UnitarySysNum)%MaxHeatCoilFluidFlow * PartLoadFrac)
    Node(UnitarySystem(UnitarySysNum)%HeatCoilFluidInletNode)%MassFlowRate = mdot
    CALL SimulateSteamCoilComponents(UnitarySystem(UnitarySysNum)%HeatingCoilName, FirstHVACIteration, &
                                     1.0d0, &
                                     UnitarySystem(UnitarySysNum)%HeatingCoilIndex, &
                                     FanOpMode = UnitarySystem(UnitarySysNum)%FanOpMode, &
                                     PartLoadRatio = PartLoadFrac)
  ELSE
    mdot = MIN(Node(UnitarySystem(UnitarySysNum)%SuppCoilFluidOutletNodeNum)%MassFlowRateMaxAvail, &
           UnitarySystem(UnitarySysNum)%MaxSuppCoilFluidFlow * PartLoadFrac)
    Node(UnitarySystem(UnitarySysNum)%SuppCoilFluidInletNode)%MassFlowRate = mdot
    CALL SimulateSteamCoilComponents(UnitarySystem(UnitarySysNum)%SuppHeatCoilName, FirstHVACIteration, &
                                     1.0d0, &
                                     UnitarySystem(UnitarySysNum)%SuppHeatCoilIndex, &
                                     FanOpMode = UnitarySystem(UnitarySysNum)%FanOpMode, &
                                     PartLoadRatio = PartLoadFrac)
  END IF
  OutletAirTemp = Node(UnitarySystem(UnitarySysNum)%HeatCoilOutletNodeNum)%Temp
  Residuum      = Par(3) - OutletAirTemp

  RETURN
END FUNCTION SteamHeatingCoilResidual

SUBROUTINE FrostControlSetPointLimit(UnitarySysNum,TempSetPoint,HumRatSetPoint,BaroPress,TfrostControl,ControlMode)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Bereket Nigusse, FSEC
          !       DATE WRITTEN   January 2013
          !       MODIFIED       na
          !       RE-ENGINEERED  na
          !
          ! PURPOSE OF THIS SUBROUTINE:
          ! Controls the forst formation condition based on user specified minimum DX coil outlet
          ! air temperature. Resets the cooling setpoint based on the user specified limiting
          ! temperature for frost control.
          !
          ! METHODOLOGY EMPLOYED:
          ! Based on FrostControlSetPointLimit by Bereket Nigusse in HVACDXSystem
          !
          ! REFERENCES:
          !  na
          ! USE STATEMENTS:
  USE Psychrometrics,     ONLY: PsyWFnTdpPb
          !
  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine
          !
          ! SUBROUTINE ARGUMENT DEFINITIONS:
    INTEGER, INTENT(IN)       :: UnitarySysNum           ! dx cooling coil system index
    REAL(r64), INTENT(INOUT)  :: TempSetPoint          ! temperature setpoint of the sensor node
    REAL(r64), INTENT(INOUT)  :: HumRatSetPoint        ! humidity ratio setpoint of the sensor node
    REAL(r64), INTENT(IN)     :: BaroPress             ! baromtric pressure, Pa [N/m^2]
    REAL(r64), INTENT(IN)     :: TfrostControl         ! minimum temperature limit for forst control
    INTEGER, INTENT(IN)       :: ControlMode           ! temperature or humidity control mode

          ! SUBROUTINE PARAMETER DEFINITIONS:
    INTEGER, PARAMETER        :: RunOnSensible = 1     ! identifier for temperature (sensible load) control
    INTEGER, PARAMETER        :: RunOnLatent = 2       ! identifier for humidity (latent load) control

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na
          !
          ! DERIVED TYPE DEFINITIONS
          ! na
          !
          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64)                   :: HumRatioSat     ! saturation humidity ratio at forst control temperature
  REAL(r64)                   :: AirMassFlow     ! air masss flow rate through the DX coil
  !
  AirMassFlow = Node(UnitarySystem(UnitarySysNum)%CoolCoilInletNodeNum)%MassFlowRate
  IF (ControlMode == RunOnSensible .AND. AirMassFlow > MinAirMassFlow .AND. &
      TempSetPoint .LT. Node(UnitarySystem(UnitarySysNum)%CoolCoilInletNodeNum)%Temp) THEN
    IF (TempSetPoint .lt. TfrostControl) THEN
        TempSetPoint = TfrostControl
        UnitarySystem(UnitarySysNum)%FrostControlStatus = 1
    END IF
  ELSEIF(ControlMode == RunOnLatent .AND. AirMassFlow > MinAirMassFlow .AND. &
         HumRatSetPoint .LT. Node(UnitarySystem(UnitarySysNum)%CoolCoilInletNodeNum)%HumRat) THEN
    HumRatioSat = PsyWFnTdpPb(TfrostControl,BaroPress,'FrostControlSetPointLimit')
    IF (HumRatioSat .gt. HumRatSetPoint) THEN
        HumRatSetPoint = HumRatioSat
        UnitarySystem(UnitarySysNum)%FrostControlStatus = 2
    END IF
  ELSE
    UnitarySystem(UnitarySysNum)%FrostControlStatus = 0
  END IF
  RETURN
END SUBROUTINE FrostControlSetPointLimit

SUBROUTINE CheckUnitarySysCoilInOASysExists(UnitarySysName)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Chandan Sharma
          !       DATE WRITTEN   April 2013
          !       MODIFIED       na
          !       RE-ENGINEERED  na
          !
          ! PURPOSE OF THIS SUBROUTINE:
          ! After making sure get input is done, checks if the Coil System DX coil is in the
          ! OA System.  IF exists then the DX cooling coil is 100% DOAS DX coil.
          !
          ! METHODOLOGY EMPLOYED:
          ! Based on CheckDXCoolingCoilInOASysExists by Bereket Nigusse in HVACDXSystem
          !
          ! REFERENCES:
          ! na
          !
          ! USE STATEMENTS:
  USE InputProcessor, ONLY: FindItemInList
  USE DXCoils,        ONLY: SetDXCoilTypeData

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=MaxNameLength), INTENT(IN) :: UnitarySysName

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER :: RoutineName='CheckUnitarySysCoilInOASysExists: ' ! include trailing blank space

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER                       :: UnitarySysNum

  IF (GetInputFlag) THEN
    CALL GetUnitarySystemInput
    GetInputFlag=.false.
  END IF

  UnitarySysNum=0
  IF (NumUnitarySystem > 0) THEN
     UnitarySysNum=FindItemInList(UnitarySysName,UnitarySystem%Name,NumUnitarySystem)
     IF (UnitarySysNum > 0)THEN
       IF(UnitarySystem(UnitarySysNum)%ISHundredPercentDOASDXCoil) THEN
         CALL SetDXCoilTypeData(UnitarySystem(UnitarySysNum)%CoolingCoilName)
       END IF
     ELSE
       CALL ShowSevereError(TRIM(RoutineName)//'System not found = AirloopHVAC:UnitarySystem "'//TRIM(UnitarySysName)//'"')
     END IF
  ELSE
    CALL ShowSevereError(TRIM(RoutineName)//'System not found = AirloopHVAC:UnitarySystem "'//TRIM(UnitarySysName)//'"')
  END IF


  RETURN

END SUBROUTINE CheckUnitarySysCoilInOASysExists

SUBROUTINE GetUnitarySystemOAHeatCoolCoil(UnitarySystemName, OACoolingCoil, OAHeatingCoil)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Chandan Sharma
          !       DATE WRITTEN   April 2013
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! Determined weather Unitary system in OA stream has heating or cooling coils

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor,   ONLY: SameString

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN)         :: UnitarySystemName  ! Name of Unitary System object
  LOGICAL,   INTENT(INOUT),   OPTIONAL :: OACoolingCoil      ! Cooling coil in OA stream
  LOGICAL,   INTENT(INOUT),   OPTIONAL :: OAHeatingCoil      ! Heating coil in OA stream

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER :: UnitarySysNum

  IF (GetInputFlag) THEN             !First time subroutine has been entered
    CALL GetUnitarySystemInput
    GetInputFlag=.false.
  END IF

  DO UnitarySysNum = 1,  NumUnitarySystem
    IF(SameString(UnitarySystemName,UnitarySystem(UnitarySysNum)%Name))THEN
      IF (UnitarySystem(UnitarySysNum)%CoolCoilExists) THEN
        OACoolingCoil = .TRUE.
      END IF
      IF (UnitarySystem(UnitarySysNum)%HeatCoilExists .OR. &
          UnitarySystem(UnitarySysNum)%SuppCoilExists) THEN
        OAHeatingCoil = .TRUE.
      END IF
    END IF
  END DO

  RETURN

END SUBROUTINE GetUnitarySystemOAHeatCoolCoil

FUNCTION GetUnitarySystemDXCoolingCoilIndex(UnitarySystemName)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Richard Raustad
          !       DATE WRITTEN   July 2013
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! Find the DX cooling coil in this Unitary System

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor,   ONLY: SameString

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN)         :: UnitarySystemName  ! Name of Unitary System object

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER :: UnitarySysNum
  INTEGER :: GetUnitarySystemDXCoolingCoilIndex

  IF (GetInputFlag) THEN             !First time subroutine has been entered
    CALL GetUnitarySystemInput
    GetInputFlag=.false.
  END IF

  GetUnitarySystemDXCoolingCoilIndex = 0
  DO UnitarySysNum = 1,  NumUnitarySystem
    IF(SameString(UnitarySystemName,UnitarySystem(UnitarySysNum)%Name))THEN
      IF (UnitarySystem(UnitarySysNum)%CoolCoilExists) THEN
        GetUnitarySystemDXCoolingCoilIndex = UnitarySystem(UnitarySysNum)%CoolingCoilIndex
      END IF
    END IF
  END DO

  RETURN

END FUNCTION GetUnitarySystemDXCoolingCoilIndex

END MODULE HVACUnitarySystem

! *****************************************************************************
!
!     NOTICE
!
!     Copyright © 1996-2013 The Board of Trustees of the University of Illinois
!     and The Regents of the University of CalIFornia through Ernest OrlanDO Lawrence
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
!     permit others to DO so.
!
!     TRADEMARKS: EnergyPlus is a trademark of the US Department of Energy.
!


