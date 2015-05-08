Module HVACVariableRefrigerantFlow
  ! Module containing the Variable Refrigerant Flow (VRF or VRV) simulation routines

  ! MODULE INFORMATION:
  !       AUTHOR         Richard Raustad, FSEC
  !       DATE WRITTEN   August 2010
  !       MODIFIED       na
  !       RE-ENGINEERED  na

  ! PURPOSE OF THIS MODULE:
  ! To encapsulate the data and algorithms required to
  ! manage the VRF System Component

  ! REFERENCES: none

  ! OTHER NOTES: none

  ! USE STATEMENTS:
  ! Use statements for data only modules
USE DataGlobals
USE DataLoopNode
USE DataInterfaces
USE DataHVACGlobals
USE DataPrecisionGlobals
USE DataZoneEnergyDemands
USE Psychrometrics

  ! Use statements for access to subroutines in other modules
  ! na

IMPLICIT NONE   ! Enforce explicit typing of all variables

PRIVATE         ! Everything private unless explicitly made public

  !MODULE PARAMETER DEFINITIONS
! Compressor operation
INTEGER, PARAMETER :: On  = 1   ! normal compressor operation
INTEGER, PARAMETER :: Off = 0   ! signal DXCoil that compressor shouldn't run

!Heat Recovery System used
INTEGER, PARAMETER :: No  = 1 ! Heat Pump mode only
INTEGER, PARAMETER :: Yes = 2 ! Heat Pump or Heat Recovery Mode (not available at this time)

! Defrost strategy
INTEGER, PARAMETER :: ReverseCycle = 1 ! uses reverse cycle defrost strategy
INTEGER, PARAMETER :: Resistive    = 2 ! uses electric resistance heater for defrost

! Defrost control
INTEGER, PARAMETER :: Timed    = 1 ! defrost cycle is timed
INTEGER, PARAMETER :: OnDemand = 2 ! defrost cycle occurs only when required

!Condenser Type
INTEGER, PARAMETER :: AirCooled        = 1 ! Air-cooled condenser
!INTEGER, PARAMETER :: WaterCooled      = 2 ! Water-cooled condenser (not yet implemented)
INTEGER, PARAMETER :: EvapCooled       = 3 ! Evaporatively-cooled condenser

! Thermostat Priority Control Type
INTEGER, PARAMETER :: LoadPriority = 1 ! total of zone loads dictate operation in cooling or heating
INTEGER, PARAMETER :: ZonePriority = 2 ! # of zones requireing cooling or heating dictate operation in cooling or heating
INTEGER, PARAMETER :: ThermostatOffsetPriority = 3 ! zone with largest deviation from setpoint dictates operation
INTEGER, PARAMETER :: ScheduledPriority = 4 ! cooling and heating modes are scheduled
INTEGER, PARAMETER :: MasterThermostatPriority = 5 ! Master zone thermostat dictates operation
INTEGER, PARAMETER :: FirstOnPriority = 6  ! first unit to respond dictates operation (not used at this time)

!Water Systems
INTEGER, PARAMETER :: CondensateDiscarded = 1001 ! default mode where water is "lost"
INTEGER, PARAMETER :: CondensateToTank    = 1002 ! collect coil condensate from air and store in water storage tank

INTEGER, PARAMETER :: WaterSupplyFromMains = 101 ! mains water line used as water source
INTEGER, PARAMETER :: WaterSupplyFromTank  = 102 ! storage tank used as water source

REAL(r64), PARAMETER :: MaxCap = 1.0d+20         ! limit of zone terminal unit capacity

! VRF System Types (strings used in integer conversions)
INTEGER, PARAMETER :: NumVRFSystemTypes = 1
INTEGER, PARAMETER :: VRF_HeatPump      = 1
CHARACTER(len=*), PARAMETER, DIMENSION(NumVRFSystemTypes) :: cVRFTypes=  &
       (/'AirConditioner:VariableRefrigerantFlow'/)

INTEGER, PARAMETER :: NumValidFuelTypes=7
CHARACTER(len=*), PARAMETER, DIMENSION(NumValidFuelTypes) :: cValidFuelTypes=    &
                 (/'Electric  ',  &
                   'NaturalGas',  &
                   'PropaneGas',  &
                   'Diesel    ',  &
                   'Gasoline  ',  &
                   'FuelOil#1 ',  &
                   'FuelOil#2 '/)

! Fuel Types
INTEGER, PARAMETER :: FuelTypeElectric    = 1     ! Fuel type for electricity
INTEGER, PARAMETER :: FuelTypeNaturalGas  = 2     ! Fuel type for natural gas
INTEGER, PARAMETER :: FuelTypePropaneGas  = 3     ! Fuel type for propane gas
INTEGER, PARAMETER :: FuelTypeDiesel      = 4     ! Fuel type for diesel
INTEGER, PARAMETER :: FuelTypeGasoline    = 5     ! Fuel type for gasoline
INTEGER, PARAMETER :: FuelTypeFuelOil1    = 6     ! Fuel type for fuel oil #1
INTEGER, PARAMETER :: FuelTypeFuelOil2    = 7     ! Fuel type for fuel oil #2

! curve type for equivalent piping losses (not necessarily the same value used in CurveManager)
INTEGER, PARAMETER :: BiQuadratic     = 4

  ! DERIVED TYPE DEFINITIONS
TYPE VRFCondenserEquipment
  CHARACTER(len=MaxNameLength) :: Name       =' ' ! Name of the VRF Terminal Unit
  INTEGER      :: VRFSystemTypeNum           =0   ! integer equivalent of system type
  INTEGER      :: SchedPtr                   =-1  ! Pointer to the correct schedule
  REAL(r64)    :: CoolingCapacity            =0.0 ! Nominal VRF heat pump cooling capacity (W)
  REAL(r64)    :: TotalCoolingCapacity       =0.0 ! Nominal VRF heat pump cooling capacity (W)
  REAL(r64)    :: CoolingCombinationRatio    =1.0 ! Ratio or terminal unit cooling capacity to VRF condenser capacity
  REAL(r64)    :: VRFCondPLR                 =0.0 ! Condenser part-load ratio wrt total capacity
  REAL(r64)    :: VRFCondRTF                 =0.0 ! Condenser runtime fraction
  REAL(r64)    :: VRFCondCyclingRatio        =0.0 ! Condenser cycling ratio below MinPLR
  REAL(r64)    :: CondenserInletTemp         =0.0 ! Condenser entering air temperature (C)
  REAL(r64)    :: CoolingCOP                 =0.0 ! Nominal VRF heat pump cooling COP
  REAL(r64)    :: OperatingCoolingCOP        =0.0 ! Operating VRF heat pump cooling COP
  REAL(r64)    :: RatedCoolingPower          =0.0 ! Rated cooling power = Rated Cooling Capacity / Rated COP (W)
  REAL(r64)    :: HeatingCapacity            =0.0 ! Nominal VRF heat pump heating capacity (W)
  REAL(r64)    :: TotalHeatingCapacity       =0.0 ! Nominal VRF heat pump heating capacity (W)
  REAL(r64)    :: HeatingCombinationRatio    =1.0 ! Ratio or terminal unit heating capacity to VRF condenser capacity
  REAL(r64)    :: HeatingCOP                 =0.0 ! Nominal VRF heat pump heating COP
  REAL(r64)    :: OperatingHeatingCOP        =0.0 ! Operating VRF heat pump heating COP
  REAL(r64)    :: RatedHeatingPower          =0.0 ! Rated heating power = Rated Heating Capacity / Rated COP (W)
  REAL(r64)    :: MinOATCooling              =0.0 ! Minimum outdoor air dry-bulb temp in cooling mode (C)
  REAL(r64)    :: MaxOATCooling              =0.0 ! Maximum outdoor air dry-bulb temp in cooling mode (C)
  REAL(r64)    :: MinOATHeating              =0.0 ! Minimum outdoor air dry-bulb temp in heating mode (C)
  REAL(r64)    :: MaxOATHeating              =0.0 ! Maximum outdoor air dry-bulb temp in heating mode (C)
  INTEGER      :: CoolCapFT                  =0   ! index to cooling capacity function of temperature curve
  INTEGER      :: CoolEIRFT                  =0   ! index to cooling EIR function of temperature curve
  INTEGER      :: HeatCapFT                  =0   ! index to heating capacity function of temperature curve
  INTEGER      :: HeatEIRFT                  =0   ! index to heating EIR function of temperature curve
  INTEGER      :: CoolBoundaryCurvePtr       =0   ! index to cooling capacity boundary curve
  INTEGER      :: HeatBoundaryCurvePtr       =0   ! index to cooling capacity boundary curve
  INTEGER      :: EIRCoolBoundaryCurvePtr    =0   ! index to cooling EIR boundary curve
  INTEGER      :: CoolEIRFPLR1               =0   ! index to cooling EIR function of PLR curve < 1
  INTEGER      :: CoolEIRFPLR2               =0   ! index to cooling EIR function of PLR curve >= 1
  INTEGER      :: CoolCapFTHi                =0   ! index to cooling capacity function of temperature curve
  INTEGER      :: CoolEIRFTHi                =0   ! index to cooling EIR function of temperature curve
  INTEGER      :: HeatCapFTHi                =0   ! index to heating capacity function of temperature curve
  INTEGER      :: HeatEIRFTHi                =0   ! index to heating EIR function of temperature curve
  INTEGER      :: EIRHeatBoundaryCurvePtr    =0   ! index to heating EIR boundary curve
  INTEGER      :: HeatEIRFPLR1               =0   ! index to heating EIR function of PLR curve < 1
  INTEGER      :: HeatEIRFPLR2               =0   ! index to heating EIR function of PLR curve >= 1
  INTEGER      :: CoolPLFFPLR                =0   ! index to cooling PLF function of PLR curve
  INTEGER      :: HeatPLFFPLR                =0   ! index to heating PLF function of PLR curve
  INTEGER      :: HeatingPerformanceOATType  =0   ! Temperature type for heating performance curves
  REAL(r64)    :: MinPLR                     =0.0 ! minimum PLR before cycling occurs
  INTEGER      :: MasterZonePtr              =0   ! index to master thermostat zone
  INTEGER      :: MasterZoneTUIndex          =0   ! index to TU in master thermostat zone
  INTEGER      :: ThermostatPriority         =0   ! VRF priority control (1=LoadPriority, 2=ZonePriority, etc)
  INTEGER      :: SchedPriorityPtr           =0   ! VRF priority control schedule pointer
  INTEGER      :: ZoneTUListPtr              =0   ! index to zone terminal unit list
  LOGICAL      :: HeatRecoveryUsed           =.FALSE. ! .TRUE. = heat recovery used
  REAL(r64)    :: VertPipeLngth              =0.0 ! vertical piping length (m)
  INTEGER      :: PCFLengthCoolPtr           =0   ! piping correction factor for length in cooling mode curve index
  INTEGER      :: PCFLengthCoolPtrType       =0   ! PCF for length curve type
  REAL(r64)    :: PCFHeightCool              =0.0 ! piping correction factor for height in cooling mode
  REAL(r64)    :: EquivPipeLngthCool         =0.0 ! equivalent piping length for cooling
  REAL(r64)    :: PipingCorrectionCooling    =1.0 ! piping correction factor for cooling
  INTEGER      :: PCFLengthHeatPtr           =0   ! piping correction factor for length in heating mode curve index
  INTEGER      :: PCFLengthHeatPtrType       =0   ! PCF for length curve type
  REAL(r64)    :: PCFHeightHeat              =0.0 ! piping correction factor for height in heating mode
  REAL(r64)    :: EquivPipeLngthHeat         =0.0 ! equivalent piping length for heating
  REAL(r64)    :: PipingCorrectionHeating    =1.0 ! piping correction factor for heating
  REAL(r64)    :: CCHeaterPower              =0.0 ! crankcase heater power per compressor (W)
  REAL(r64)    :: CompressorSizeRatio        =0.0 ! ratio of min compressor size to total capacity
  INTEGER      :: NumCompressors             =0   ! number of compressors in VRF condenser
  REAL(r64)    :: MaxOATCCHeater             =0.0 ! maximum outdoor air dry-bulb temp for crankcase heater operation (C)
  INTEGER      :: DefrostEIRPtr              =0   ! index to defrost EIR curve
  REAL(r64)    :: DefrostFraction            =0.0 ! defrost time period fraction (hr)
  INTEGER      :: DefrostStrategy            =0   ! Type of defrost (reversecycle or resistive)
  INTEGER      :: DefrostControl             =0   ! type of defrost control (timed or ondemand)
  REAL(r64)    :: DefrostCapacity            =0.0 ! capacity of resistive defrost heating element (W)
  REAL(r64)    :: DefrostPower               =0.0 ! power used during defrost (W)
  REAL(r64)    :: DefrostConsumption         =0.0 ! energy used during defrost (J)
  REAL(r64)    :: MaxOATDefrost              =0.0 ! maximum outdoor air dry-bulb temp for defrost operation (C)
  INTEGER      :: CondenserType              =0   ! condenser type, evap- or air-cooled
  INTEGER      :: CondenserNodeNum           =0   ! condenser inlet node number
  REAL(r64)    :: EvapCondEffectiveness      =0.0 ! evaporative condenser effectiveness
  REAL(r64)    :: EvapCondAirVolFlowRate     =0.0 ! air volume flow rate through condenser (m3/s)
  REAL(r64)    :: EvapCondPumpPower          =0.0 ! evaporative condenser water pump power (W)
  INTEGER      :: CoolCombRatioPTR           = 0  ! index to cooling combination ratio curve pointer
  INTEGER      :: HeatCombRatioPTR           = 0  ! index to heating combination ratio curve pointer
  INTEGER      :: OperatingMode              = 0  ! VRF Condenser operating mode, 0=off, 1=cooling, 2=heating, 3=HR
  REAL(r64)    :: ElecPower                  =0.0 ! VRF Condenser power (W)
  REAL(r64)    :: ElecCoolingPower           =0.0 ! VRF Condenser power in cooling mode (W)
  REAL(r64)    :: ElecHeatingPower           =0.0 ! VRF Condenser power in heating mode (W)
  REAL(r64)    :: CoolElecConsumption        =0.0 ! VRF Condenser cooling energy (J)
  REAL(r64)    :: HeatElecConsumption        =0.0 ! VRF Condenser heating energy (J)
  REAL(r64)    :: CrankCaseHeaterPower       =0.0 ! VRF Condenser crankcase heater power (W)
  REAL(r64)    :: CrankCaseHeaterElecConsumption =0.0 ! VRF Condenser crankcase heater energy (J)
  REAL(r64)    :: EvapCondPumpElecPower      =0.0 ! VRF Condenser evaporatively cooled condenser pump power (W)
  REAL(r64)    :: EvapCondPumpElecConsumption=0.0 ! VRF Condenser evaporatively cooled condenser pump elec consumption (J)
  REAL(R64)    :: EvapWaterConsumpRate       =0.0 ! VRF Condenser evaporatively cooled condenser water consumption (m3/s)
  INTEGER      :: CoolingMaxTempLimitIndex   =0   ! Warning message recurring error index
  INTEGER      :: HeatingMaxTempLimitIndex   =0   ! Warning message recurring error index
  INTEGER      :: FuelType                   =0   ! Fuel type

  ! begin variables for Water System interactions
  INTEGER ::EvapWaterSupplyMode              = WaterSupplyFromMains !  where does water come from
  CHARACTER(len=MaxNameLength) :: EvapWaterSupplyName = ' ' ! name of water source e.g. water storage tank
  INTEGER ::EvapWaterSupTankID               = 0 !
  INTEGER ::EvapWaterTankDemandARRID         = 0 !
  CHARACTER(len=MaxNameLength) :: CondensateCollectName = ' ' ! name of water source e.g. water storage tank
  INTEGER ::CondensateTankID                 = 0 !
  INTEGER ::CondensateTankSupplyARRID        = 0 !
  REAL(r64)   :: CondensateVdot = 0.0 ! rate of water condensation from air stream [m3/s]
  REAL(r64)   :: CondensateVol  = 0.0 ! amount of water condensed from air stream [m3]
  !end variables for water system interactions

  ! begin variables for Basin Heater interactions
  REAL(r64)   :: BasinHeaterPowerFTempDiff = 0.d0 ! Basin heater capacity per degree C below setpoint (W/C)
  REAL(r64)   :: BasinHeaterSetPointTemp   = 0.d0 ! setpoint temperature for basin heater operation (C)
  REAL(r64)   :: BasinHeaterPower          = 0.d0 ! Basin heater power (W)
  REAL(r64)   :: BasinHeaterConsumption    = 0.d0 ! Basin heater energy consumption (J)
  INTEGER     :: BasinHeaterSchedulePtr    = 0    ! Pointer to basin heater schedule
  !end variables for Basin Heater interactions

  Logical     :: EMSOverrideHPOperatingMode = .FALSE.
  REAL(r64)   :: EMSValueForHPOperatingMode = 0.d0 !
  INTEGER     :: HPOperatingModeErrorIndex = 0

END TYPE VRFCondenserEquipment

TYPE TerminalUnitListData
  CHARACTER(len=MaxNameLength) :: Name       =' ' ! Name of the VRF Terminal Unit List
  INTEGER :: NumTUInList                     =0   ! Number of VRF Terminal Units in List
  INTEGER, ALLOCATABLE :: ZoneTUPtr(:)            ! index to VRF Terminal Unit
  CHARACTER(len=MaxNameLength), ALLOCATABLE :: ZoneTUName(:) ! Name of the VRF Terminal Unit
  LOGICAL, ALLOCATABLE :: IsSimulated(:)          ! TRUE if TU has been simulated
  REAL(r64), ALLOCATABLE :: TotalCoolLoad(:)      ! Total zone cooling coil load met by TU
  REAL(r64), ALLOCATABLE :: TotalHeatLoad(:)      ! Total zone heating coil load met by TU
  LOGICAL, ALLOCATABLE :: CoolingCoilPresent(:)   ! FALSE if coil not present
  LOGICAL, ALLOCATABLE :: HeatingCoilPresent(:)   ! FALSE if coil not present
  LOGICAL, ALLOCATABLE :: TerminalUnitNotSizedYet(:) ! TRUE if terminal unit not sized
END TYPE TerminalUnitListData

TYPE VRFTerminalUnitEquipment
  CHARACTER(len=MaxNameLength) :: Name       =' ' ! Name of the VRF Terminal Unit
  INTEGER      :: VRFTUType_Num              =0   ! DataHVACGlobals VRF Terminal Unit type
  INTEGER      :: SchedPtr                   =-1  ! Pointer to the correct schedule
  INTEGER      :: VRFSysNum                  =0   ! index to VRF Condenser
  INTEGER      :: TUListNum                  =0   ! index to VRF Terminal Unit List
  INTEGER      :: NumTUInList                =0   ! index to pointer in VRF Terminal Unit List
  INTEGER      :: ZoneNum                    =0   ! index to zone where VRF Terminal Unit resides
  INTEGER      :: VRFTUInletNodeNum          =0   ! VRF Terminal Unit inlet node number
  INTEGER      :: VRFTUOutletNodeNum         =0   ! VRF Terminal Unit outlet node number
  INTEGER      :: VRFTUOAMixerOANodeNum      =0   ! OA node number for this TU's OA mixer
  INTEGER      :: VRFTUOAMixerRelNodeNum     =0   ! Relief node number for this TU's OA mixer
  INTEGER      :: VRFTUOAMixerRetNodeNum     =0   ! Return node number for this TU's OA mixer
  REAL(r64)    :: MaxCoolAirVolFlow          =0.0 ! supply air volumetric flow rate during cooling operation [m3/s]
  REAL(r64)    :: MaxHeatAirVolFlow          =0.0 ! supply air volumetric flow rate during heating operation [m3/s]
  REAL(r64)    :: MaxNoCoolAirVolFlow        =0.0 ! supply air volumetric flow rate when no cooling [m3/s]
  REAL(r64)    :: MaxNoHeatAirVolFlow        =0.0 ! supply air volumetric flow rate when no heating [m3/s]
  REAL(r64)    :: MaxCoolAirMassFlow         =0.0 ! supply air mass flow rate during cooling operation [kg/s]
  REAL(r64)    :: MaxHeatAirMassFlow         =0.0 ! supply air mass flow rate during heating operation [kg/s]
  REAL(r64)    :: MaxNoCoolAirMassFlow       =0.0 ! supply air mass flow rate when no cooling [kg/s]
  REAL(r64)    :: MaxNoHeatAirMassFlow       =0.0 ! supply air mass flow rate when no heating [kg/s]
  REAL(r64)    :: CoolOutAirVolFlow          =0.0 ! OA volumetric flow rate during cooling operation [m3/s]
  REAL(r64)    :: HeatOutAirVolFlow          =0.0 ! OA volumetric flow rate during heating operation [m3/s]
  REAL(r64)    :: NoCoolHeatOutAirVolFlow    =0.0 ! OA volumetric flow rate when no cooling or heating [m3/s]
  REAL(r64)    :: CoolOutAirMassFlow         =0.0 ! OA mass flow rate during cooling operation [kg/s]
  REAL(r64)    :: HeatOutAirMassFlow         =0.0 ! OA mass flow rate during heating operation [kg/s]
  REAL(r64)    :: NoCoolHeatOutAirMassFlow   =0.0 ! OA mass flow rate when no cooling or heating [kg/s]
  INTEGER      :: FanOpModeSchedPtr          =0   ! Pointer to the correct fan operating mode schedule
  INTEGER      :: FanAvailSchedPtr           =0   ! Pointer to the correct fan availability schedule
  INTEGER      :: FanIndex                   =0   ! Index to fan object
  INTEGER      :: OpMode                     =0   ! operation mode: 1 = cycling fan, cycling coil 2 = constant fan, cycling coil
  INTEGER      :: FanPlace                   =0   ! fan placement; 1=blow through, 2=draw through
  REAL(r64)    :: ActualFanVolFlowRate       =0.0 ! volumetric flow rate from fan object
  CHARACTER(len=MaxNameLength) :: OAMixerName=' ' ! name of outside air mixer
  INTEGER      :: OAMixerIndex               =0   ! index to outside air mixer
  LOGICAL      :: OAMixerUsed                =.FALSE. ! true if OA Mixer object is used
  INTEGER      :: CoolCoilIndex              =0   ! index to terminal unit cooling coil
  INTEGER      :: HeatCoilIndex              =0   ! index to terminal unit heating coil
  INTEGER      :: DXCoolCoilType_Num         =0   ! type of VRF cooling coil
  INTEGER      :: DXHeatCoilType_Num         =0   ! type of VRF cooling coil
  REAL(r64)    :: ParasiticElec              =0.0 ! parasitic electric for VRF terminal unit
  REAL(r64)    :: ParasiticOffElec           =0.0 ! parasitic electric for VRF terminal unit when off
  REAL(r64)    :: HeatingSpeedRatio          = 1.d0 ! Fan speed ratio in heating mode
  REAL(r64)    :: CoolingSpeedRatio          = 1.d0 ! Fan speed ratio in cooling mode
  REAL(r64)    :: ParasiticCoolElecPower       = 0.d0 ! Terminal unit cooling parasitic electric power [W]
  REAL(r64)    :: ParasiticHeatElecPower       = 0.d0 ! Terminal unit heating parasitic electric power [W]
  REAL(r64)    :: ParasiticElecCoolConsumption = 0.d0 ! Terminal unit parasitic electric consumption in cooling [J]
  REAL(r64)    :: ParasiticElecHeatConsumption = 0.d0 ! Terminal unit parasitic electric consumption in heating [J]
  LOGICAL      :: CoolingCoilPresent         =.TRUE. ! FALSE if coil not present
  LOGICAL      :: HeatingCoilPresent         =.TRUE. ! FALSE if coil not present

  REAL(r64)    :: TerminalUnitSensibleRate   = 0.0d0 ! sensible cooling/heating rate of VRF terminal unit (W)
  REAL(r64)    :: TerminalUnitLatentRate     = 0.0d0 ! latent dehumidificatino/humidification rate of VRF terminal unit (W)
  REAL(r64)    :: TotalCoolingRate           = 0.0d0 ! report variable for total cooling rate (W)
  REAL(r64)    :: TotalHeatingRate           = 0.0d0 ! report variable for total heating rate (W)
  REAL(r64)    :: SensibleCoolingRate        = 0.0d0 ! report variable for sensible cooling rate (W)
  REAL(r64)    :: SensibleHeatingRate        = 0.0d0 ! report variable for sensible heating rate (W)
  REAL(r64)    :: LatentCoolingRate          = 0.0d0 ! report variable for latent cooling rate (W)
  REAL(r64)    :: LatentHeatingRate          = 0.0d0 ! report variable for latent heating rate (W)

  REAL(r64)    :: TotalCoolingEnergy         = 0.0d0 ! report variable for total cooling energy (J)
  REAL(r64)    :: TotalHeatingEnergy         = 0.0d0 ! report variable for total heating energy (J)
  REAL(r64)    :: SensibleCoolingEnergy      = 0.0d0 ! report variable for sensible cooling energy (J)
  REAL(r64)    :: SensibleHeatingEnergy      = 0.0d0 ! report variable for sensible heating energy (J)
  REAL(r64)    :: LatentCoolingEnergy        = 0.0d0 ! report variable for latent cooling energy (J)
  REAL(r64)    :: LatentHeatingEnergy        = 0.0d0 ! report variable for latent heating energy (J)

  Logical      :: EMSOverridePartLoadFrac = .FALSE.  ! User defined EMS function
  REAL(r64)    :: EMSValueForPartLoadFrac = 0.0D0    ! user defined value for EMS function

END TYPE VRFTerminalUnitEquipment

  !MODULE VARIABLE DECLARATIONS:
  LOGICAL :: GetVRFInputFlag = .True.  ! Flag set to make sure you get input once
  LOGICAL, ALLOCATABLE, DIMENSION(:) :: CheckEquipName ! Flag set to check equipment connections once
  TYPE (VRFCondenserEquipment),    ALLOCATABLE, DIMENSION(:) :: VRF  ! AirConditioner:VariableRefrigerantFlow object
  TYPE (VRFTerminalUnitEquipment), ALLOCATABLE, DIMENSION(:) :: VRFTU ! ZoneHVAC:TerminalUnit:VariableRefrigerantFlow object
  TYPE (TerminalUnitListData),     ALLOCATABLE, DIMENSION(:) :: TerminalUnitList ! zoneTerminalUnitList object
  INTEGER :: NumVRFCond           = 0        ! total number of VRF condensers
  INTEGER :: NumVRFTU             = 0        ! total number of VRF terminal units
  INTEGER :: NumVRFTULists        = 0        ! The number of VRF TU lists
  REAL(r64) :: CompOnMassFlow     = 0.0      ! Supply air mass flow rate w/ compressor ON
  REAL(r64) :: OACompOnMassFlow   = 0.0      ! OA mass flow rate w/ compressor ON
  REAL(r64) :: CompOffMassFlow    = 0.0      ! Supply air mass flow rate w/ compressor OFF
  REAL(r64) :: OACompOffMassFlow  = 0.0      ! OA mass flow rate w/ compressor OFF
  REAL(r64) :: CompOnFlowRatio    = 0.0      ! fan flow ratio when coil on
  REAL(r64) :: CompOffFlowRatio   = 0.0      ! fan flow ratio when coil off
  REAL(r64) :: FanSpeedRatio      = 0.0      ! ratio of air flow ratio passed to fan object
  LOGICAL, ALLOCATABLE, DIMENSION(:) :: HeatingLoad     ! defines a heating load on VRFTerminalUnits
  LOGICAL, ALLOCATABLE, DIMENSION(:) :: CoolingLoad     ! defines a cooling load on VRFTerminalUnits
  LOGICAL, ALLOCATABLE, DIMENSION(:) :: LastModeHeating ! defines last mode was heating mode
  LOGICAL, ALLOCATABLE, DIMENSION(:) :: LastModeCooling ! defines last mode was cooling mode
  REAL(r64),ALLOCATABLE, DIMENSION(:) :: MaxCoolingCapacity   ! maximum capacity of any terminal unit
  REAL(r64),ALLOCATABLE, DIMENSION(:) :: MaxHeatingCapacity   ! maximum capacity of any terminal unit
  REAL(r64),ALLOCATABLE, DIMENSION(:) :: CoolCombinationRatio ! ratio of terminal unit capacity to VRF condenser capacity
  REAL(r64),ALLOCATABLE, DIMENSION(:) :: HeatCombinationRatio ! ratio of terminal unit capacity to VRF condenser capacity
  REAL(r64) :: LoopDXCoolCoilRTF = 0.d0 ! holds value of DX cooling coil RTF
  REAL(r64) :: LoopDXHeatCoilRTF = 0.d0 ! holds value of DX heating coil RTF

! Subroutine Specifications for the Module
          ! Driver/Manager Routines
Public  SimulateVRF

          ! Get Input routines for module
Private GetVRFInput

          ! Initialization routines for module
Private InitVRF
Private SizeVRF

          ! Algorithms for the module
Private SimVRF
Private CalcVRFCondenser

          ! Update routine to check convergence and update nodes
!Private UpdateVRF

          ! Reporting routines for module
Private ReportVRFTerminalUnit
Private ReportVRFCondenser

          ! Utility routines for module
          ! na

CONTAINS

! MODULE SUBROUTINES:
!*************************************************************************
SUBROUTINE SimulateVRF(CompName, ZoneNum, FirstHVACIteration, SysOutputProvided, LatOutputProvided, CompIndex)

          ! SUBROUTINE INFORMATION:
          ! AUTHOR         Richard Raustad, FSEC
          ! DATE WRITTEN   August 2010
          ! MODIFIED       na
          ! RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine manages VRF terminal unit simulation.

          ! METHODOLOGY EMPLOYED:
          ! Simulate all terminal units
          ! Once all terminal units have been simulated, simulate VRF condenser

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: FindIteminList
  USE General,        ONLY: TrimSigDigits
  USE DXCoils,        ONLY: DXCoilTotalCooling, DXCoilTotalHeating
  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT (IN)    :: CompName
  INTEGER,          INTENT (IN)    :: ZoneNum
  LOGICAL,          INTENT (IN)    :: FirstHVACIteration
  REAL(r64),        INTENT (OUT)   :: SysOutputProvided
  REAL(r64),        INTENT (OUT)   :: LatOutputProvided
  INTEGER,          INTENT (INOUT) :: CompIndex

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER :: Blank = ' '

          ! INTERFACE BLOCK SPECIFICATIONS

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER   :: VRFTUNum           ! current VRF system terminal unit index
  INTEGER   :: VRFCondenser       ! index to VRF AC system object - AirConditioner:VariableRefrigerantFlow
  INTEGER   :: TUListNum          ! index to VRF AC system terminal unit list
  INTEGER   :: NumTUInList        ! index to pointer in VRF AC system terminal unit list
  REAL(r64) :: OnOffAirFlowRatio  ! ratio of compressor ON airflow to average airflow over timestep
  INTEGER   :: DXCoolingCoilIndex ! index to this terminal units DX cooling coil
  INTEGER   :: DXHeatingCoilIndex ! index to this terminal units DX heating coil
  REAL(r64) :: QZnReq
          ! FLOW:

  ! Obtains and Allocates VRF system related parameters from input file
  IF (GetVRFInputFlag) THEN  !First time subroutine has been entered
    CALL GetVRFInput
    GetVRFInputFlag=.false.
  END IF

  ! CompIndex accounting
  IF (CompIndex == 0) THEN
    VRFTUNum = FindItemInList(CompName,VRFTU%Name,NumVRFTU)
    IF (VRFTUNum == 0) THEN
      CALL ShowFatalError('SimulateVRF: VRF Terminal Unit not found='//TRIM(CompName))
    ENDIF
    CompIndex=VRFTUNum

  ELSE
    VRFTUNum=CompIndex
    IF (VRFTUNum > NumVRFTU .or. VRFTUNum < 1) THEN
      CALL ShowFatalError('SimulateVRF: Invalid CompIndex passed='//  &
                          TRIM(TrimSigDigits(VRFTUNum))// &
                          ', Number of VRF Terminal Units = '//TRIM(TrimSigDigits(NumVRFTU))//  &
                          ', VRF Terminal Unit name = '//TRIM(CompName))
    ENDIF
    IF (CheckEquipName(VRFTUNum)) THEN
      IF (CompName /= Blank .AND. CompName /= VRFTU(VRFTUNum)%Name) THEN
        CALL ShowFatalError('SimulateVRF: Invalid CompIndex passed='//  &
                            TRIM(TrimSigDigits(VRFTUNum))// &
                            ', VRF Terminal Unit name='//TRIM(CompName)//', stored VRF TU Name for that index='//  &
                            TRIM(VRFTU(VRFTUNum)%Name))
      ENDIF
      CheckEquipName(VRFTUNum)=.TRUE.
    ENDIF
  ENDIF

  ! the VRF condenser index
  VRFCondenser = VRFTU(VRFTUNum)%VRFSysNum
  ! the terminal unit list object index
  TUListNum    = VRFTU(VRFTUNum)%TUListNum
  ! the entry number in the terminal unit list (which item in the terminal unit list, e.g. second in list)
  NumTUInList  = VRFTU(VRFTUNum)%NumTUInList
  ! index to cooling coil (coil is optional but at least one must be present)
  DXCoolingCoilIndex = VRFTU(VRFTUNum)%CoolCoilIndex
  ! index to heating coil (coil is optional but at least one must be present)
  DXHeatingCoilIndex = VRFTU(VRFTUNum)%HeatCoilIndex
  QZnReq = 0.d0

  ! Initialize terminal unit
  CALL InitVRF(VRFTuNum, FirstHVACIteration, OnOffAirFlowRatio, QZnReq)  ! Initialize all VRFTU related parameters

  ! Simulate terminal unit
  CALL SimVRF(VRFTUNum, FirstHVACIteration, OnOffAirFlowRatio, SysOutputProvided, LatOutputProvided, QZnReq)

  ! mark this terminal unit as simulated
  TerminalUnitList(TUListNum)%IsSimulated(NumTUInList) = .TRUE.

  ! keep track of individual coil loads
  If(DXCoolingCoilIndex .GT. 0)THEN
    TerminalUnitList(TUListNum)%TotalCoolLoad(NumTUInList) = DXCoilTotalCooling(DXCoolingCoilIndex)
  ELSE
    TerminalUnitList(TUListNum)%TotalCoolLoad(NumTUInList) = 0.d0
  END IF
  IF(DXHeatingCoilIndex .GT. 0)THEN
    TerminalUnitList(TUListNum)%TotalHeatLoad(NumTUInList) = DXCoilTotalHeating(DXHeatingCoilIndex)
  ELSE
    TerminalUnitList(TUListNum)%TotalHeatLoad(NumTUInList) = 0.d0
  END IF

  ! Update the current VRF terminal unit to the outlet nodes
!  CALL UpdateVRF(VRFTUNum)

  ! Report the current VRF terminal unit
  CALL ReportVRFTerminalunit(VRFTUNum)

! make sure all TU in a list are able to get simulated, otherwise condenser is never simulated **
! either fatal on GetInput, or keep track of unused TU's and set their respective flag to TRUE **
! after all VRF terminal units have been simulated, call the VRF condenser model
  IF(ALL(TerminalUnitList(TUListNum)%IsSimulated))THEN
    CALL CalcVRFCondenser(VRFCondenser)
    CALL ReportVRFCondenser(VRFCondenser)
  END IF

  RETURN

END SUBROUTINE SimulateVRF

SUBROUTINE CalcVRFCondenser(VRFCond)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         R. Raustad, FSEC
          !       DATE WRITTEN   September 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Model the interactions of VRF terminal units with a single variable-speed condenser.
          ! The terminal units are simulated first, and then the condenser is simulated.
          ! If terminal units require more capacity than can be delivered by condenser, a limit is set.

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE CurveManager,    ONLY: CurveValue
  Use DataEnvironment, ONLY: StdBaroPress, EnvironmentName, CurMnDy, OutDryBulbTemp, OutHumRat, OutBaroPress, OutWetBulbTemp
  USE DataGlobals,     ONLY: SecInHour
  USE DataHVACGlobals, ONLY: TimeStepSys
  USE DXCoils,         ONLY: DXCoilCoolInletAirWBTemp, DXCoilHeatInletAirDBTemp, DXCoilHeatInletAirWBTemp

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: VRFCond ! index to VRF condenser

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: TUListNum          ! index to TU List
  INTEGER :: NumTUInList        ! number of terminal units is list
  INTEGER :: NumTU              ! loop counter
  INTEGER :: TUIndex            ! Index to terminal unit
  INTEGER :: CoolCoilIndex      ! index to cooling coil in terminal unit
  INTEGER :: HeatCoilIndex      ! index to heating coil in terminal unit
  INTEGER :: NumTUInCoolingMode ! number of terminal units actually cooling
  INTEGER :: NumTUInHeatingMode ! number of terminal units actually heating
  INTEGER :: MinOutputIndex     ! index to TU with lowest load
  INTEGER :: TempTUIndex        ! temp variable used to find max terminal unit limit

  REAL(r64) :: TUCoolingLoad        ! DX cooling coil load to be met by condenser (W)
  REAL(r64) :: TUHeatingLoad        ! DX cooling coil load to be met by condenser (W)
  REAL(r64) :: TotCoolCapTempModFac ! cooling CAPFT curve output
  REAL(r64) :: TotHeatCapTempModFac ! heating CAPFT curve output
  REAL(r64) :: TotCoolEIRTempModFac ! cooling EIRFT curve output
  REAL(r64) :: TotHeatEIRTempModFac ! heating EIRFT curve output
  REAL(r64) :: InletAirWetbulbC     ! coil inlet air wet-bulb temperature (C)
  REAL(r64) :: InletAirDrybulbC     ! coil inlet air dry-bulb temperature (C)
  REAL(r64) :: CondInletTemp        ! condenser inlet air temperature (C)
  REAL(r64) :: CondInletHumrat      ! condenser inlet air humidity ratio (kg/kg)
  REAL(r64) :: OutdoorDryBulb       ! outdoor dry-bulb temperature (C)
  REAL(r64) :: OutdoorHumRat        ! outdoor humidity ratio (kg/kg)
  REAL(r64) :: OutdoorPressure      ! outdoor pressure (Pa)
  REAL(r64) :: OutdoorWetBulb       ! outdoor wet-bulb temperature (C)
  REAL(r64) :: SumCoolInletWB       ! sum of active TU's DX cooling coil inlet air wet-bulb temperature
  REAL(r64) :: SumHeatInletDB       ! sum of active TU's DX heating coil inlet air dry-bulb temperature
  REAL(r64) :: SumHeatInletWB       ! sum of active TU's DX heating coil inlet air wet-bulb temperature
  REAL(r64) :: CoolOABoundary       ! output of cooling boundary curve (outdoor temperature, C)
  REAL(r64) :: HeatOABoundary       ! output of heating boundary curve (outdoor temperature, C)
  REAL(r64) :: TotalTUCoolingCapacity   ! sum of TU's cooling capacity (W)
  REAL(r64) :: TotalTUHeatingCapacity   ! sum of TU's heating capacity (W)
  REAL(r64) :: TotalCondCoolingCapacity ! total available condenser cooling capacity (W)
  REAL(r64) :: TotalCondHeatingCapacity ! total available condenser heating capacity (W)
  REAL(r64) :: MinOutput                ! used when finding TU "max" capacity limit
  REAL(r64) :: RemainingCapacity        ! used when finding TU "max" capacity limit
  REAL(r64) :: CoolingPLR               ! condenser cooling PLR
  REAL(r64) :: HeatingPLR               ! condenser heating PLR
  INTEGER :: RemainingTUs               ! number of TUs left in list to meet remaining load
  REAL(r64), ALLOCATABLE :: Temp(:)     ! temporarary array for processing terminal units
  REAL(r64), ALLOCATABLE :: Temp2(:)    ! temporarary array for processing terminal units
  REAL(r64) :: CyclingRatio             ! cycling ratio of condenser's compressors
  REAL(r64) :: EIRFPLRModFac            ! EIRFPLR curve output
  INTEGER   :: Stage                    ! used for crankcase heater power calculation
  REAL(r64) :: UpperStageCompressorRatio ! used for crankcase heater power calculation
  REAL(r64) :: RhoAir                   ! Density of air [kg/m3]
  REAL(r64) :: RhoWater                 ! Density of water [kg/m3]
  REAL(r64) :: CondAirMassFlow          ! Condenser air mass flow rate [kg/s]
  REAL(r64) :: PartLoadFraction         ! Part load fraction from PLFFPLR curve
  REAL(r64) :: VRFRTF                   ! VRF runtime fraction when cycling below MINPLR
  REAL(r64) :: OutdoorCoilT             ! Outdoor coil temperature (C)
  REAL(r64) :: OutdoorCoildw            ! Outdoor coil delta w assuming coil temp of OutdoorCoilT (kg/kg)
  REAL(r64) :: FractionalDefrostTime    ! Fraction of time step system is in defrost
  REAL(r64) :: HeatingCapacityMultiplier ! Multiplier for heating capacity when system is in defrost
  REAL(r64) :: InputPowerMultiplier     ! Multiplier for power when system is in defrost
  REAL(r64) :: LoadDueToDefrost         ! Additional load due to defrost
  REAL(r64) :: DefrostEIRTempModFac     ! EIR modifier for defrost (function of entering drybulb, outside wetbulb)


          ! FLOW

  ! variable initializations
  TUListNum         = VRF(VRFCond)%ZoneTUListPtr
  NumTUInList       = TerminalUnitList(TUListNum)%NumTUInList
  TUCoolingLoad     = 0.0d0
  TUHeatingLoad     = 0.0d0
  CoolingPLR        = 0.0d0
  HeatingPLR        = 0.0d0
  CyclingRatio      = 1.0d0
  SumCoolInletWB    = 0.0d0
  SumHeatInletDB    = 0.0d0
  SumHeatInletWB    = 0.0d0
  TotalCondCoolingCapacity = 0.0d0
  TotalCondHeatingCapacity = 0.0d0
  NumTUInCoolingMode       = 0
  NumTUInHeatingMode       = 0
  VRF(VRFCond)%ElecCoolingPower      = 0.d0
  VRF(VRFCond)%ElecHeatingPower      = 0.d0
  VRF(VRFCond)%CrankCaseHeaterPower  = 0.d0
  VRF(VRFCond)%EvapCondPumpElecPower = 0.d0
  VRF(VRFCond)%EvapWaterConsumpRate  = 0.d0
  VRF(VRFCond)%DefrostPower          = 0.d0
  VRF(VRFCond)%OperatingCoolingCOP   = 0.D0
  VRF(VRFCond)%OperatingHeatingCOP   = 0.D0
  VRF(VRFCond)%BasinHeaterPower      = 0.0d0

  ! sum loads on TU coils
  DO NumTU = 1, NumTUInList
    TUCoolingLoad = TUCoolingLoad + TerminalUnitList(TUListNum)%TotalCoolLoad(NumTU)
    TUHeatingLoad = TUHeatingLoad + TerminalUnitList(TUListNum)%TotalHeatLoad(NumTU)
  END DO

  ! loop through TU's and calculate average inlet conditions for active coils
  DO NumTU = 1, NumTUInList
    TUIndex = TerminalUnitList(TUListNum)%ZoneTUPtr(NumTU)
    CoolCoilIndex = VRFTU(TUIndex)%CoolCoilIndex
    HeatCoilIndex = VRFTU(TUIndex)%HeatCoilIndex

    IF(TerminalUnitList(TUListNum)%TotalCoolLoad(NumTU) .GT. 0.0d0)THEN
      SumCoolInletWB = SumCoolInletWB + &
        DXCoilCoolInletAirWBTemp(CoolCoilIndex) * TerminalUnitList(TUListNum)%TotalCoolLoad(NumTU)/TUCoolingLoad
      NumTUInCoolingMode = NumTUInCoolingMode + 1
    END IF
    IF(TerminalUnitList(TUListNum)%TotalHeatLoad(NumTU) .GT. 0.0d0)THEN
      SumHeatInletDB = SumHeatInletDB + &
        DXCoilHeatInletAirDBTemp(HeatCoilIndex) * TerminalUnitList(TUListNum)%TotalHeatLoad(NumTU)/TUHeatingLoad
      SumHeatInletWB = SumHeatInletWB + &
        DXCoilHeatInletAirWBTemp(HeatCoilIndex) * TerminalUnitList(TUListNum)%TotalHeatLoad(NumTU)/TUHeatingLoad
      NumTUInHeatingMode = NumTUInHeatingMode + 1
    END IF
  END DO

  ! set condenser entering air conditions
  IF (VRF(VRFCond)%CondenserNodeNum /= 0) THEN
    OutdoorDryBulb  = Node(VRF(VRFCond)%CondenserNodeNum)%Temp
    OutdoorHumRat   = Node(VRF(VRFCond)%CondenserNodeNum)%HumRat
    OutdoorPressure = Node(VRF(VRFCond)%CondenserNodeNum)%Press
    OutdoorWetBulb  = Node(VRF(VRFCond)%CondenserNodeNum)%OutAirWetBulb
  ELSE
    OutdoorDryBulb  = OutDryBulbTemp
    OutdoorHumRat   = OutHumRat
    OutdoorPressure = OutBaroPress
    OutdoorWetBulb  = OutWetBulbTemp
  ENDIF

  IF (VRF(VRFCond)%CondenserType == AirCooled) THEN
    CondInletTemp   = OutdoorDryBulb ! Outdoor dry-bulb temp
  ELSEIF (VRF(VRFCond)%CondenserType == EvapCooled) THEN
    RhoAir          = PsyRhoAirFnPbTdbW(OutdoorPressure,OutdoorDryBulb,OutdoorHumRat)
    CondAirMassFlow =  RhoAir * VRF(VRFCond)%EvapCondAirVolFlowRate
    ! (Outdoor wet-bulb temp from DataEnvironment) + (1.0-EvapCondEffectiveness) * (drybulb - wetbulb)
    CondInletTemp   = OutdoorWetBulb + (OutdoorDryBulb-OutdoorWetBulb)*(1.0d0 - VRF(VRFCond)%EvapCondEffectiveness)
    CondInletHumrat = PsyWFnTdbTwbPb(CondInletTemp,OutdoorWetBulb,OutdoorPressure)
  END IF
  VRF(VRFCond)%CondenserInletTemp = CondInletTemp

  ! calculate capacities and energy use
  IF(CoolingLoad(VRFCond))THEN
    IF(NumTUInCoolingMode .GT. 0)THEN
      InletAirWetbulbC = SumCoolInletWB
    ELSE
      InletAirWetbulbC = 0.0d0 ! for now
    END IF

    TotCoolCapTempModFac = CurveValue(VRF(VRFCond)%CoolCapFT,InletAirWetbulbC,CondInletTemp)
    TotCoolEIRTempModFac = CurveValue(VRF(VRFCond)%CoolEIRFT,InletAirWetbulbC,CondInletTemp)

    ! recalculate cooling Cap and EIR curve output if using boundary curve along with dual Cap and EIR curves.
    IF(VRF(VRFCond)%CoolBoundaryCurvePtr .GT. 0)THEN
      CoolOABoundary = CurveValue(VRF(VRFCond)%CoolBoundaryCurvePtr,InletAirWetbulbC)
      IF(OutdoorDryBulb .GT. CoolOABoundary)THEN
        IF(VRF(VRFCond)%CoolCapFTHi .GT. 0) &
          TotCoolCapTempModFac = CurveValue(VRF(VRFCond)%CoolCapFTHi,InletAirWetbulbC,CondInletTemp)
        IF(VRF(VRFCond)%CoolEIRFTHi .GT. 0) &
          TotCoolEIRTempModFac = CurveValue(VRF(VRFCond)%CoolEIRFTHi,InletAirWetbulbC,CondInletTemp)
      END IF
    END IF

    TotalCondCoolingCapacity = VRF(VRFCond)%CoolingCapacity * CoolCombinationRatio(VRFCond) * TotCoolCapTempModFac
    TotalTUCoolingCapacity = TotalCondCoolingCapacity * VRF(VRFCond)%PipingCorrectionCooling

    IF(TotalCondCoolingCapacity .GT. 0.0D0)THEN
      CoolingPLR = (TUCoolingLoad/VRF(VRFCond)%PipingCorrectionCooling) / TotalCondCoolingCapacity
    ELSE
      CoolingPLR = 0.0D0
    END IF

  ELSE IF (HeatingLoad(VRFCond))THEN
    IF(NumTUInHeatingMode .GT. 0)THEN
      InletAirDrybulbC = SumHeatInletDB
      InletAirWetbulbC = SumHeatInletWB
    ELSE
      InletAirDrybulbC = 0.0d0 ! for now
      InletAirWetbulbC = 0.0d0 ! for now
    END IF
    SELECT CASE(VRF(VRFCond)%HeatingPerformanceOATType)
      CASE(DryBulbIndicator)
        TotHeatCapTempModFac = CurveValue(VRF(VRFCond)%HeatCapFT,InletAirDrybulbC,CondInletTemp)
        TotHeatEIRTempModFac = CurveValue(VRF(VRFCond)%HeatEIRFT,InletAirDrybulbC,CondInletTemp)
      CASE(WetBulbIndicator)
        TotHeatCapTempModFac = CurveValue(VRF(VRFCond)%HeatCapFT,InletAirDrybulbC,OutdoorWetBulb)
        TotHeatEIRTempModFac = CurveValue(VRF(VRFCond)%HeatEIRFT,InletAirDrybulbC,OutdoorWetBulb)
      CASE DEFAULT
        TotHeatCapTempModFac = 1.d0
        TotHeatEIRTempModFac = 1.d0
    END SELECT
    ! recalculate heating Cap and EIR curve output if using boundary curve along with dual Cap and EIR curves.
    IF(VRF(VRFCond)%HeatBoundaryCurvePtr .GT. 0)THEN
      HeatOABoundary = CurveValue(VRF(VRFCond)%HeatBoundaryCurvePtr,InletAirDrybulbC)
        SELECT CASE(VRF(VRFCond)%HeatingPerformanceOATType)
          CASE(DryBulbIndicator)
            IF(OutdoorDryBulb .GT. HeatOABoundary)THEN
              IF(VRF(VRFCond)%HeatCapFTHi .GT. 0) &
                TotHeatCapTempModFac = CurveValue(VRF(VRFCond)%HeatCapFTHi,InletAirDrybulbC,CondInletTemp)
            END IF
          CASE(WetBulbIndicator)
            IF(OutdoorWetBulb .GT. HeatOABoundary)THEN
              IF(VRF(VRFCond)%HeatCapFTHi .GT. 0) &
                TotHeatCapTempModFac = CurveValue(VRF(VRFCond)%HeatCapFTHi,InletAirDrybulbC,OutdoorWetBulb)
            END IF
          CASE DEFAULT
            TotHeatCapTempModFac = 1.d0
        END SELECT
    END IF
    IF(VRF(VRFCond)%EIRHeatBoundaryCurvePtr .GT. 0)THEN
      HeatOABoundary = CurveValue(VRF(VRFCond)%EIRHeatBoundaryCurvePtr,InletAirDrybulbC)
        SELECT CASE(VRF(VRFCond)%HeatingPerformanceOATType)
          CASE(DryBulbIndicator)
            IF(OutdoorDryBulb .GT. HeatOABoundary)THEN
              IF(VRF(VRFCond)%HeatEIRFTHi .GT. 0) &
                TotHeatEIRTempModFac = CurveValue(VRF(VRFCond)%HeatEIRFTHi,InletAirDrybulbC,CondInletTemp)
            END IF
          CASE(WetBulbIndicator)
            IF(OutdoorWetBulb .GT. HeatOABoundary)THEN
              IF(VRF(VRFCond)%HeatEIRFTHi .GT. 0) &
                TotHeatEIRTempModFac = CurveValue(VRF(VRFCond)%HeatEIRFTHi,InletAirDrybulbC,OutdoorWetBulb)
            END IF
          CASE DEFAULT
            TotHeatEIRTempModFac = 1.d0
        END SELECT
    END IF

    ! Calculating adjustment factors for defrost
    ! Calculate delta w through outdoor coil by assuming a coil temp of 0.82*DBT-9.7(F) per DOE2.1E
    OutdoorCoilT = 0.82d0 * OutdoorDryBulb - 8.589d0
    OutdoorCoildw = MAX(1.0d-6,(OutdoorHumRat - PsyWFnTdpPb(OutdoorCoilT,OutdoorPressure)))

    ! Initializing defrost adjustment factors
    LoadDueToDefrost = 0.0
    HeatingCapacityMultiplier = 1.0d0
    FractionalDefrostTime = 0.0
    InputPowerMultiplier = 1.0d0

    ! Check outdoor temperature to determine of defrost is active
    IF (OutdoorDryBulb .LE. VRF(VRFCond)%MaxOATDefrost) THEN
      ! Calculate defrost adjustment factors depending on defrost control type
      IF (VRF(VRFCond)%DefrostControl .EQ. Timed) THEN
        FractionalDefrostTime = VRF(VRFCond)%DefrostFraction
        HeatingCapacityMultiplier = 0.909d0 - 107.33d0 * OutdoorCoildw
        InputPowerMultiplier = 0.90d0 - 36.45d0*OutdoorCoildw
      ELSE !else defrost control is on-demand
        FractionalDefrostTime = 1.0d0 / (1.0d0 + 0.01446d0 / OutdoorCoildw)
        HeatingCapacityMultiplier = 0.875d0 * ( 1.0d0 - FractionalDefrostTime)
        InputPowerMultiplier = 0.954d0 * ( 1.0d0 - FractionalDefrostTime)
      END IF


      IF (FractionalDefrostTime .GT. 0.0) THEN
        ! Calculate defrost adjustment factors depending on defrost control strategy
        IF (VRF(VRFCond)%DefrostStrategy .EQ. ReverseCycle) THEN
          LoadDueToDefrost = (0.01d0 * FractionalDefrostTime) * &
                             (7.222d0 - OutdoorDryBulb) * &
                             (VRF(VRFCond)%HeatingCapacity/1.01667d0)
          DefrostEIRTempModFac = CurveValue(VRF(VRFCond)%DefrostEIRPtr,&
                                 MAX(15.555d0,InletAirWetbulbC),MAX(15.555d0,OutdoorDryBulb))
          VRF(VRFCond)%DefrostPower =  DefrostEIRTempModFac * &
                                            (VRF(VRFCond)%HeatingCapacity/1.01667d0) &
                                            * FractionalDefrostTime
        ELSE ! Defrost strategy is resistive
          VRF(VRFCond)%DefrostPower = VRF(VRFCond)%DefrostCapacity &
                                      * FractionalDefrostTime
        END IF
      ELSE ! Defrost is not active because FractionalDefrostTime = 0.0
        VRF(VRFCond)%DefrostPower = 0.d0
        HeatingCapacityMultiplier = 1.d0
        InputPowerMultiplier      = 1.d0
      END IF
    END IF

    TotalCondHeatingCapacity = VRF(VRFCond)%HeatingCapacity * HeatCombinationRatio(VRFCond) * TotHeatCapTempModFac &
                               * HeatingCapacityMultiplier
    TotalTUHeatingCapacity = TotalCondHeatingCapacity * VRF(VRFCond)%PipingCorrectionHeating
    IF(TotalCondHeatingCapacity .GT. 0.0D0)THEN
      HeatingPLR = (TUHeatingLoad/VRF(VRFCond)%PipingCorrectionHeating) / TotalCondHeatingCapacity
      HeatingPLR = HeatingPLR + LoadDueToDefrost / TotalCondHeatingCapacity
    ELSE
      HeatingPLR = 0.0D0
    END IF

  END IF

! limit the TU capacity when the condenser is maxed out on capacity
  IF(CoolingLoad(VRFCond) .AND. NumTUInCoolingMode .GT. 0)THEN

  ! IF TU capacity is greater than condenser capacity find maximum allowed TU capacity (i.e., conserve energy)
  !
  ! Calculate the maximum allowed terminal unit capacity. Total terminal unit capacity must not
  ! exceed the available condenser capacity. This variable (MaxCoolingCapacity or MaxHeatingCapacity)
  ! is used to limit the terminal units providing more capacity than allowed.
  ! Example: TU loads are 1-ton, 2-ton, 3-ton, and 4-ton connected to a condenser having only 9-tons available.
  ! This variable is will be set to 3-tons and the 4-ton terminal unit will be limited to 3-tons
  ! (see InitVRF where this variable is reset and CalcVRF where the call to the DX coils passes this argument).
    IF(TUCoolingLoad > TotalTUCoolingCapacity)THEN

      RemainingCapacity = TotalTUCoolingCapacity
      RemainingTUs = NumTUInList

      ALLOCATE(Temp(NumTUInList))
      ALLOCATE(Temp2(NumTUInList))
      Temp = TerminalUnitList(TUListNum)%TotalCoolLoad
      Temp2 = Temp

      ! sort TU capacity from lowest to highest
      DO TempTUIndex = 1, NumTUInList
        MinOutput = MaxCap
        DO NumTU = 1, NumTUInList
          IF(Temp2(NumTU) .LT. MinOutput)THEN
            MinOutput = Temp2(NumTU)
            Temp(TempTUIndex) = MinOutput
            MinOutputIndex = NumTU
          END IF
        END DO
        Temp2(MinOutputIndex) = MaxCap
      END DO

     ! find limit of "terminal unit" capacity so that sum of all TU's does not exceed condenser capacity
      DO TempTUIndex = 1, NumTUInList
        IF((Temp(TempTUIndex)*(NumTUInList-TempTUIndex+1)) .LT. RemainingCapacity)THEN
          RemainingCapacity = RemainingCapacity - Temp(TempTUIndex)
          CYCLE
        ELSE
          IF(NumTUInList-TempTUIndex .EQ. 0)THEN
            MaxCoolingCapacity(VRFCond) = RemainingCapacity
          ELSE
            MaxCoolingCapacity(VRFCond) = RemainingCapacity / (NumTUInList-TempTUIndex)
          END IF
        END IF
      END DO

      DEALLOCATE(Temp)
      DEALLOCATE(Temp2)
    END IF
  ELSE IF(HeatingLoad(VRFCond) .AND. NumTUInHeatingMode .GT. 0)THEN
!   IF TU capacity is greater than condenser capacity
    IF(TUHeatingLoad > TotalTUHeatingCapacity)THEN

      RemainingCapacity = TotalTUHeatingCapacity
      RemainingTUs = NumTUInList

      ALLOCATE(Temp(NumTUInList))
      ALLOCATE(Temp2(NumTUInList))
      Temp = TerminalUnitList(TUListNum)%TotalHeatLoad
      Temp2 = Temp

      ! sort TU capacity from lowest to highest
      DO TempTUIndex = 1, NumTUInList
        MinOutput = MaxCap
        DO NumTU = 1, NumTUInList
          IF(Temp2(NumTU) .LT. MinOutput)THEN
            MinOutput = Temp2(NumTU)
            Temp(TempTUIndex) = MinOutput
            MinOutputIndex = NumTU
          END IF
        END DO
        Temp2(MinOutputIndex) = MaxCap
      END DO

     ! find limit of "terminal unit" capacity so that sum of all TU's does not exceed condenser capacity
      DO TempTUIndex = 1, NumTUInList
        IF((Temp(TempTUIndex)*(NumTUInList-TempTUIndex+1)) .LT. RemainingCapacity)THEN
          RemainingCapacity = RemainingCapacity - Temp(TempTUIndex)
          CYCLE
        ELSE
          IF(NumTUInList-TempTUIndex .EQ. 0)THEN
            MaxHeatingCapacity(VRFCond) = RemainingCapacity
          ELSE
            MaxHeatingCapacity(VRFCond) = RemainingCapacity / (NumTUInList-TempTUIndex)
          END IF
        END IF
      END DO

      DEALLOCATE(Temp)
      DEALLOCATE(Temp2)
    END IF
  ELSE
  END IF

  VRF(VRFCond)%VRFCondPLR = MAX(CoolingPLR,HeatingPLR)

  VRF(VRFCond)%TotalCoolingCapacity = TotalCondCoolingCapacity * CoolingPLR
  VRF(VRFCond)%TotalHeatingCapacity = TotalCondHeatingCapacity * HeatingPLR

  IF(VRF(VRFCond)%MinPLR .GT. 0.0d0)THEN
    CyclingRatio = MIN(1.0d0,VRF(VRFCond)%VRFCondPLR/VRF(VRFCond)%MinPLR)
    IF(VRF(VRFCond)%VRFCondPLR .LT. VRF(VRFCond)%MinPLR)THEN
      VRF(VRFCond)%VRFCondPLR = VRF(VRFCond)%MinPLR
    END IF
  END IF
  VRF(VRFCond)%VRFCondCyclingRatio = CyclingRatio ! report variable for cycling rate

  VRF(VRFCond)%OperatingMode = 0 ! report variable for heating or cooling mode
  EIRFPLRModFac = 1.d0
  VRFRTF = 0.d0
  ! cooling and heating is optional (only one may exist), if so then performance curve for missing coil are not required
  IF(CoolingLoad(VRFCond) .AND. CoolingPLR > 0.d0)THEN
    VRF(VRFCond)%OperatingMode = 1
    IF(CoolingPLR .GT. 1.d0)THEN
      IF(VRF(VRFCond)%CoolEIRFPLR2.GT.0) EIRFPLRModFac=CurveValue(VRF(VRFCond)%CoolEIRFPLR2,MAX(VRF(VRFCond)%MinPLR,CoolingPLR))
    ELSE
      IF(VRF(VRFCond)%CoolEIRFPLR1.GT.0) EIRFPLRModFac=CurveValue(VRF(VRFCond)%CoolEIRFPLR1,MAX(VRF(VRFCond)%MinPLR,CoolingPLR))
    END IF
    ! find part load fraction to calculate RTF
    IF(VRF(VRFCond)%CoolPLFFPLR .GT. 0) THEN
      PartLoadFraction = MAX(0.7d0,CurveValue(VRF(VRFCond)%CoolPLFFPLR,CyclingRatio))
    ELSE
      PartLoadFraction = 1.0d0
    END IF
    VRFRTF = MIN(1.0d0,(CyclingRatio / PartLoadFraction))

    VRF(VRFCond)%ElecCoolingPower = (VRF(VRFCond)%RatedCoolingPower * TotCoolCapTempModFac) &
                                     * TotCoolEIRTempModFac * EIRFPLRModFac * VRFRTF
  END IF
  IF(HeatingLoad(VRFCond) .AND. HeatingPLR > 0.0d0)THEN
    VRF(VRFCond)%OperatingMode = 2
    IF(HeatingPLR .GT. 1.d0)THEN
      IF(VRF(VRFCond)%HeatEIRFPLR2.GT.0) EIRFPLRModFac=CurveValue(VRF(VRFCond)%HeatEIRFPLR2,MAX(VRF(VRFCond)%MinPLR,HeatingPLR))
    ELSE
      IF(VRF(VRFCond)%HeatEIRFPLR1.GT.0) EIRFPLRModFac=CurveValue(VRF(VRFCond)%HeatEIRFPLR1,MAX(VRF(VRFCond)%MinPLR,HeatingPLR))
    END IF
    ! find part load fraction to calculate RTF
    IF(VRF(VRFCond)%HeatPLFFPLR .GT. 0) THEN
      PartLoadFraction = MAX(0.7d0,CurveValue(VRF(VRFCond)%HeatPLFFPLR,CyclingRatio))
    ELSE
      PartLoadFraction = 1.0d0
    END IF
    VRFRTF = MIN(1.0d0,(CyclingRatio / PartLoadFraction))

    VRF(VRFCond)%ElecHeatingPower = (VRF(VRFCond)%RatedHeatingPower * TotHeatCapTempModFac) &
                                     * TotHeatEIRTempModFac * EIRFPLRModFac * VRFRTF &
                                     * InputPowerMultiplier
  END IF
  VRF(VRFCond)%VRFCondRTF = VRFRTF

  ! calculate crankcase heater power
  IF(VRF(VRFCond)%MaxOATCCHeater .GT. OutdoorDryBulb)THEN
    ! calculate crankcase heater power
    VRF(VRFCond)%CrankCaseHeaterPower = VRF(VRFCond)%CCHeaterPower * (1.d0 - VRFRTF)
    IF(VRF(VRFCond)%NumCompressors .GT. 1)THEN
      UpperStageCompressorRatio = (1.d0 - VRF(VRFCond)%CompressorSizeRatio) / (VRF(VRFCond)%NumCompressors - 1)
      DO Stage = 1, VRF(VRFCond)%NumCompressors - 2
        IF(VRF(VRFCond)%VRFCondPLR .LT. (VRF(VRFCond)%CompressorSizeRatio + Stage * UpperStageCompressorRatio)) THEN
          VRF(VRFCond)%CrankCaseHeaterPower = VRF(VRFCond)%CrankCaseHeaterPower + &
                 VRF(VRFCond)%CCHeaterPower
        END IF
      END DO
    END IF
  ELSE
    VRF(VRFCond)%CrankCaseHeaterPower = 0.d0
  END IF

  IF (VRF(VRFCond)%CondenserType == EvapCooled) THEN
     ! Calculate basin heater power
     CALL CalcBasinHeaterPower(VRF(VRFCond)%BasinHeaterPowerFTempDiff,&
                               VRF(VRFCond)%BasinHeaterSchedulePtr,&
                               VRF(VRFCond)%BasinHeaterSetPointTemp,VRF(VRFCond)%BasinHeaterPower)
     VRF(VRFCond)%BasinHeaterPower = VRF(VRFCond)%BasinHeaterPower * &
                                        (1.d0 - VRFRTF)

    ! calcualte evaporative condenser pump power and water consumption
    IF (CoolingLoad(VRFCond) .AND. CoolingPLR > 0.d0) THEN
      !******************
      ! WATER CONSUMPTION IN m3 OF WATER FOR DIRECT
      ! H2O [m3/sec] = Delta W[KgH2O/Kg air]*Mass Flow Air[Kg air]
      !                    /RhoWater [kg H2O/m3 H2O]
      !******************
      RhoWater = RhoH2O(OutdoorDryBulb)
      VRF(VRFCond)%EvapWaterConsumpRate =  &
             (CondInletHumrat - OutdoorHumRat) *  &
              CondAirMassFlow/RhoWater * VRF(VRFCond)%VRFCondPLR
      VRF(VRFCond)%EvapCondPumpElecPower = VRF(VRFCond)%EvapCondPumpPower * VRFRTF
    END IF

  END IF

  ! calculate operating COP
  IF(CoolingLoad(VRFCond) .AND. CoolingPLR > 0.d0)THEN
    IF(VRF(VRFCond)%ElecCoolingPower .NE. 0.d0)THEN
      VRF(VRFCond)%OperatingCoolingCOP = VRF(VRFCond)%TotalCoolingCapacity/ &
       (VRF(VRFCond)%ElecCoolingPower + VRF(VRFCond)%CrankCaseHeaterPower + &
        VRF(VRFCond)%EvapCondPumpElecPower + VRF(VRFCond)%DefrostPower)
    ELSE
      VRF(VRFCond)%OperatingCoolingCOP = 0.D0
    END IF
  END IF
  IF(HeatingLoad(VRFCond) .AND. HeatingPLR > 0.0d0)THEN
    IF(VRF(VRFCond)%ElecHeatingPower .NE. 0.d0)THEN
      VRF(VRFCond)%OperatingHeatingCOP = VRF(VRFCond)%TotalHeatingCapacity/ &
      (VRF(VRFCond)%ElecHeatingPower + VRF(VRFCond)%CrankCaseHeaterPower + &
        VRF(VRFCond)%EvapCondPumpElecPower + VRF(VRFCond)%DefrostPower)
    ELSE
      VRF(VRFCond)%OperatingHeatingCOP = 0.D0
    END IF
  END IF

  RETURN

END SUBROUTINE CalcVRFCondenser


! Get Input Section of the Module
!******************************************************************************
SUBROUTINE GetVRFInput

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Raustad, FSEC
          !       DATE WRITTEN   August 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Obtains input data for VRF systems and stores it in data structures

          ! METHODOLOGY EMPLOYED:
          ! Uses "Get" routines to read in data.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
    USE InputProcessor
    USE DataLoopNode
    USE General,               ONLY: TrimSigDigits
    USE ScheduleManager,       ONLY: GetScheduleIndex, CheckScheduleValueMinMax
    USE NodeInputManager,      ONLY: GetOnlySingleNode
    USE CurveManager,          ONLY: GetCurveIndex, GetCurveType
    USE BranchNodeConnections, ONLY: TestCompSet, SetUpCompSets
    USE Fans,                  ONLY: GetFanDesignVolumeFlowRate,GetFanInletNode,GetFanOutletNode,GetFanIndex, &
                                          GetFanAvailSchPtr, GetFanType
    USE MixedAir,              ONLY: GetOAMixerNodeNumbers
    USE DXCoils,               ONLY: GetDXCoolCoilIndex=>GetDXCoilIndex, &
                                     GetDXCoilInletNode=>GetCoilInletNode, GetDXCoilOutletNode=>GetCoilOutletNode, &
                                     GetCoilCondenserInletNode, GetCoilTypeNum, SetDXCoolingCoilData
    USE DataHeatBalance,       ONLY: Zone
    USE OutAirNodeManager,     ONLY: CheckOutAirNodeNumber
    USE WaterManager,          ONLY: SetupTankDemandComponent, SetupTankSupplyComponent
    USE DataZoneEquipment,     ONLY: ZoneEquipConfig
    USE DataSizing,            ONLY: AutoSize

!    USE DataIPShortCuts

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER    :: RoutineName='GetVRFInput: ' ! include trailing blank space

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    INTEGER :: NumVRFCTU    ! The number of VRF constant volume TUs (anticipating different types of TU's)
    INTEGER :: VRFTUNum     ! Loop index to the total number of VRF terminal units
    INTEGER :: VRFNum       ! Loop index to the total number of VRF terminal units
    INTEGER :: TUListNum    ! Loop index to the total number of VRF terminal unit lists
    INTEGER :: NumAlphas    ! Number of alpha arguments
    INTEGER :: NumNums      ! Number of real arguments
!    INTEGER :: checkNum
    INTEGER :: IOSTAT       ! Status
    LOGICAL :: ErrFlag                 ! error flag for mining functions
    LOGICAL :: ErrorsFound = .false.   ! If errors detected in input
    LOGICAL :: IsNotOK               ! Flag to verify name
    LOGICAL :: IsBlank               ! Flag for blank name
    CHARACTER(len=MaxNameLength+40),ALLOCATABLE, DIMENSION(:) :: cAlphaFieldNames
    CHARACTER(len=MaxNameLength+40),ALLOCATABLE, DIMENSION(:) :: cNumericFieldNames
    LOGICAL, ALLOCATABLE, DIMENSION(:) :: lNumericFieldBlanks
    LOGICAL, ALLOCATABLE, DIMENSION(:) :: lAlphaFieldBlanks
    CHARACTER(len=MaxNameLength),ALLOCATABLE, DIMENSION(:) :: cAlphaArgs
    REAL(r64),ALLOCATABLE, DIMENSION(:) :: rNumericArgs
    CHARACTER(len=MaxNameLength) :: cCurrentModuleObject
    INTEGER :: NumParams
    INTEGER :: MaxAlphas
    INTEGER :: MaxNumbers
    CHARACTER(len=MaxNameLength) :: FanType             ! Type of supply air fan
    CHARACTER(len=MaxNameLength) :: FanName             ! Supply air fan name
    CHARACTER(len=MaxNameLength) :: OAMixerType         ! Type of OA mixer
    CHARACTER(len=MaxNameLength) :: DXCoolingCoilType   ! Type of VRF DX cooling coil
    CHARACTER(len=MaxNameLength) :: DXHeatingCoilType   ! Type of VRF DX heating coil
    INTEGER                      :: FanType_Num         ! Used in mining function CALLS
    REAL(r64)                    :: FanVolFlowRate      ! Fan Max Flow Rate from Fan object (for comparisons to validity)
    INTEGER                      :: FanInletNodeNum     ! Used in TU configuration setup
    INTEGER                      :: FanOutletNodeNum    ! Used in TU configuration setup
    INTEGER, DIMENSION(4)        :: OANodeNums          ! Node numbers of OA mixer (OA, EA, RA, MA)
    INTEGER                      :: CCoilInletNodeNum   ! Used in TU configuration setup
    INTEGER                      :: CCoilOutletNodeNum  ! Used in TU configuration setup
    INTEGER                      :: HCoilInletNodeNum   ! Used in TU configuration setup
    INTEGER                      :: HCoilOutletNodeNum  ! Used in TU configuration setup
    INTEGER                      :: ZoneTerminalUnitListNum ! Used to find connection between VRFTU, TUList and VRF condenser
    INTEGER                      :: NumCond             ! loop counter
    INTEGER                      :: NumList             ! loop counter
    LOGICAL                      :: ZoneNodeNotFound    ! used in error checking
    INTEGER                      :: CtrlZone            ! index to loop counter
    INTEGER                      :: NodeNum             ! index to loop counter

          ! Flow
    MaxAlphas=0
    MaxNumbers=0
    NumVRFCTU = GetNumObjectsFound('ZoneHVAC:TerminalUnit:VariableRefrigerantFlow')
    IF (NumVRFCTU > 0) THEN
      CALL GetObjectDefMaxArgs('ZoneHVAC:TerminalUnit:VariableRefrigerantFlow',NumParams,NumAlphas,NumNums)
      MaxAlphas=MAX(MaxAlphas,NumAlphas)
      MaxNumbers=MAX(MaxNumbers,NumNums)
    ENDIF
    NumVRFCond = GetNumObjectsFound('AirConditioner:VariableRefrigerantFlow')
    IF (NumVRFCond > 0) THEN
      CALL GetObjectDefMaxArgs('AirConditioner:VariableRefrigerantFlow',NumParams,NumAlphas,NumNums)
      MaxAlphas=MAX(MaxAlphas,NumAlphas)
      MaxNumbers=MAX(MaxNumbers,NumNums)
    ENDIF
    NumVRFTULists = GetNumObjectsFound('ZoneTerminalUnitList')
    IF (NumVRFTULists > 0) THEN
      CALL GetObjectDefMaxArgs('ZoneTerminalUnitList',NumParams,NumAlphas,NumNums)
      MaxAlphas=MAX(MaxAlphas,NumAlphas)
      MaxNumbers=MAX(MaxNumbers,NumNums)
    ENDIF
    ALLOCATE(cAlphaArgs(MaxAlphas))
    cAlphaArgs=' '
    ALLOCATE(cAlphaFieldNames(MaxAlphas))
    cAlphaFieldNames=' '
    ALLOCATE(lAlphaFieldBlanks(MaxAlphas))
    lAlphaFieldBlanks=.false.
    ALLOCATE(cNumericFieldNames(MaxNumbers))
    cNumericFieldNames=' '
    ALLOCATE(lNumericFieldBlanks(MaxNumbers))
    lNumericFieldBlanks=.false.
    ALLOCATE(rNumericArgs(MaxNumbers))
    rNumericArgs=0.0

    NumVRFTU  = NumVRFCTU
    IF (NumVRFTU > 0) THEN
      ALLOCATE(VRFTU(NumVRFTU))
      ALLOCATE(CheckEquipName(NumVRFTU))
      CheckEquipName=.true.
    ENDIF

    IF (NumVRFCond > 0) THEN
      ALLOCATE(VRF(NumVRFCond))
      ALLOCATE(MaxCoolingCapacity(NumVRFCond))
      ALLOCATE(MaxHeatingCapacity(NumVRFCond))
      ALLOCATE(CoolCombinationRatio(NumVRFCond))
      ALLOCATE(HeatCombinationRatio(NumVRFCond))
      MaxCoolingCapacity = MaxCap
      MaxHeatingCapacity = MaxCap
      CoolCombinationRatio = 1.d0
      HeatCombinationRatio = 1.d0
    ENDIF

    IF (NumVRFTULists > 0) THEN
      ALLOCATE(TerminalUnitList(NumVRFTULists))
    ENDIF

    ! read all terminal unit list objects
    cCurrentModuleObject= 'ZoneTerminalUnitList'
    DO VRFNum = 1,  NumVRFTULists
      CALL GetObjectItem(TRIM(cCurrentModuleObject),VRFNum,cAlphaArgs,NumAlphas, &
                         rNumericArgs,NumNums,IOSTAT, &
                         NumBlank=lNumericFieldBlanks,AlphaBlank=lAlphaFieldBlanks, &
                         AlphaFieldNames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

      IsNotOK=.false.
      IsBlank=.false.
      CALL VerifyName(cAlphaArgs(1),TerminalUnitList%Name,VRFNum-1,IsNotOK,IsBlank,TRIM(cCurrentModuleObject)//' Name')
      IF (IsNotOK) THEN
        ErrorsFound=.true.
        IF (IsBlank) cAlphaArgs(1)='xxxxx'
      ENDIF
      TerminalUnitList(VRFNum)%Name  = cAlphaArgs(1)
      TerminalUnitList(VRFNum)%NumTUInList = NumAlphas - 1
      ALLOCATE(TerminalUnitList(VRFNum)%ZoneTUPtr(TerminalUnitList(VRFNum)%NumTUInList))
      ALLOCATE(TerminalUnitList(VRFNum)%ZoneTUName(TerminalUnitList(VRFNum)%NumTUInList))
      ALLOCATE(TerminalUnitList(VRFNum)%IsSimulated(TerminalUnitList(VRFNum)%NumTUInList))
      ALLOCATE(TerminalUnitList(VRFNum)%TotalCoolLoad(TerminalUnitList(VRFNum)%NumTUInList))
      ALLOCATE(TerminalUnitList(VRFNum)%TotalHeatLoad(TerminalUnitList(VRFNum)%NumTUInList))
      ALLOCATE(TerminalUnitList(VRFNum)%CoolingCoilPresent(TerminalUnitList(VRFNum)%NumTUInList))
      ALLOCATE(TerminalUnitList(VRFNum)%HeatingCoilPresent(TerminalUnitList(VRFNum)%NumTUInList))
      ALLOCATE(TerminalUnitList(VRFNum)%TerminalUnitNotSizedYet(TerminalUnitList(VRFNum)%NumTUInList))
      TerminalUnitList(VRFNum)%ZoneTUPtr     = 0
      TerminalUnitList(VRFNum)%IsSimulated   = .FALSE.
      TerminalUnitList(VRFNum)%TotalCoolLoad = 0.0d0
      TerminalUnitList(VRFNum)%TotalHeatLoad = 0.0d0
      TerminalUnitList(VRFNum)%CoolingCoilPresent = .TRUE.
      TerminalUnitList(VRFNum)%HeatingCoilPresent = .TRUE.
      TerminalUnitList(VRFNum)%TerminalUnitNotSizedYet = .TRUE.
      DO TUListNum = 1, TerminalUnitList(VRFNum)%NumTUInList
        TerminalUnitList(VRFNum)%ZoneTUName(TUListNum) = cAlphaArgs(TUListNum+1)
      END DO
    END DO

    ! read all VRF condenser objects
    cCurrentModuleObject= 'AirConditioner:VariableRefrigerantFlow'
    DO VRFNum = 1,  NumVRFCond
      CALL GetObjectItem(TRIM(cCurrentModuleObject),VRFNum,cAlphaArgs,NumAlphas, &
                         rNumericArgs,NumNums,IOSTAT, &
                         NumBlank=lNumericFieldBlanks,AlphaBlank=lAlphaFieldBlanks, &
                         AlphaFieldNames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
      IsNotOK=.false.
      IsBlank=.false.
      CALL VerifyName(cAlphaArgs(1),VRF%Name,VRFNum-1,IsNotOK,IsBlank,TRIM(cCurrentModuleObject)//' Name')
      IF (IsNotOK) THEN
        ErrorsFound=.true.
        IF (IsBlank) cAlphaArgs(1)='xxxxx'
      ENDIF
      VRF(VRFNum)%Name  = cAlphaArgs(1)
      VRF(VRFNum)%VRFSystemTypeNum = VRF_HeatPump
      IF (.NOT. lAlphaFieldBlanks(2)) VRF(VRFNum)%SchedPtr = GetScheduleIndex(cAlphaArgs(2))
 !     CALL TestCompSet(TRIM(cCurrentModuleObject),VRF(VRFTUNum)%Name,cAlphaArgs(3),cAlphaArgs(4),'Air Nodes')


      VRF(VRFNum)%CoolingCapacity = rNumericArgs(1)
      VRF(VRFNum)%CoolingCOP      = rNumericArgs(2)
      VRF(VRFNum)%MinOATCooling   = rNumericArgs(3)
      VRF(VRFNum)%MaxOATCooling   = rNumericArgs(4)

      VRF(VRFNum)%CoolCapFT = GetCurveIndex(cAlphaArgs(3))
      IF(VRF(VRFNum)%CoolCapFT .GT. 0)THEN
        ! Verify Curve Object, only legal type is biquadratic
        SELECT CASE(GetCurveType(VRF(VRFNum)%CoolCapFT))
        CASE('BIQUADRATIC')
        CASE DEFAULT
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//', "'//TRIM(VRF(VRFNum)%Name)// &
                     '" illegal '//TRIM(cAlphaFieldNames(3))//' type for this object = '// &
                              TRIM(GetCurveType(VRF(VRFNum)%CoolCapFT)))
          CALL ShowContinueError('... curve type must be BiQuadratic.')
          ErrorsFound=.TRUE.
        END SELECT
!  only show error if cooling coil is present, since TU's have not yet been read, do this later in GetInput
!      ELSE
!        CALL ShowSevereError(TRIM(cCurrentModuleObject)//', "'//TRIM(VRF(VRFNum)%Name)// &
!                     '" '//TRIM(cAlphaFieldNames(3))//' not found.')
!        ErrorsFound=.TRUE.
      END IF

      VRF(VRFNum)%CoolBoundaryCurvePtr = GetCurveIndex(cAlphaArgs(4))
      IF(VRF(VRFNum)%CoolBoundaryCurvePtr .GT. 0)THEN
        ! Verify Curve Object, only legal type is linear, quadratic, or cubic
        SELECT CASE(GetCurveType(VRF(VRFNum)%CoolBoundaryCurvePtr))
        CASE('LINEAR', 'QUADRATIC', 'CUBIC')
        CASE DEFAULT
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//', "'//TRIM(VRF(VRFNum)%Name)// &
                     '" illegal '//TRIM(cAlphaFieldNames(4))//' type for this object = '// &
                              TRIM(GetCurveType(VRF(VRFNum)%CoolBoundaryCurvePtr)))
          CALL ShowContinueError('... curve type must be Linear, Quadratic or Cubic.')
          ErrorsFound=.TRUE.
        END SELECT
      END IF

      VRF(VRFNum)%CoolCapFTHi = GetCurveIndex(cAlphaArgs(5))
      IF(VRF(VRFNum)%CoolCapFTHi .GT. 0)THEN
        ! Verify Curve Object, only legal type is biquadratic
        SELECT CASE(GetCurveType(VRF(VRFNum)%CoolCapFTHi))
        CASE('BIQUADRATIC')
        CASE DEFAULT
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//', "'//TRIM(VRF(VRFNum)%Name)// &
                     '" illegal '//TRIM(cAlphaFieldNames(5))//' type for this object = '// &
                              TRIM(GetCurveType(VRF(VRFNum)%CoolCapFTHi)))
          CALL ShowContinueError('... curve type must be BiQuadratic.')
          ErrorsFound=.TRUE.
        END SELECT
      END IF

      VRF(VRFNum)%CoolEIRFT = GetCurveIndex(cAlphaArgs(6))
      IF(VRF(VRFNum)%CoolEIRFT .GT. 0)THEN
        ! Verify Curve Object, only legal type is biquadratic
        SELECT CASE(GetCurveType(VRF(VRFNum)%CoolEIRFT))
        CASE('BIQUADRATIC')
        CASE DEFAULT
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//', "'//TRIM(VRF(VRFNum)%Name)// &
                     '" illegal '//TRIM(cAlphaFieldNames(6))//' type for this object = '// &
                              TRIM(GetCurveType(VRF(VRFNum)%CoolEIRFT)))
          CALL ShowContinueError('... curve type must be BiQuadratic.')
          ErrorsFound=.TRUE.
        END SELECT
!  only show error if cooling coil is present, since TU's have not yet been read, do this later in GetInput
!      ELSE
!        CALL ShowSevereError(TRIM(cCurrentModuleObject)//', "'//TRIM(VRF(VRFNum)%Name)// &
!                     '" '//TRIM(cAlphaFieldNames(6))//' not found.')
!        ErrorsFound=.TRUE.
      END IF

      VRF(VRFNum)%EIRCoolBoundaryCurvePtr = GetCurveIndex(cAlphaArgs(7))
      IF(VRF(VRFNum)%EIRCoolBoundaryCurvePtr .GT. 0)THEN
        ! Verify Curve Object, only legal type is linear, quadratic, or cubic
        SELECT CASE(GetCurveType(VRF(VRFNum)%EIRCoolBoundaryCurvePtr))
        CASE('LINEAR', 'QUADRATIC', 'CUBIC')
        CASE DEFAULT
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//', "'//TRIM(VRF(VRFNum)%Name)// &
                     '" illegal '//TRIM(cAlphaFieldNames(7))//' type for this object = '// &
                              TRIM(GetCurveType(VRF(VRFNum)%EIRCoolBoundaryCurvePtr)))
          CALL ShowContinueError('... curve type must be Linear, Quadratic or Cubic.')
          ErrorsFound=.TRUE.
        END SELECT
      END IF

      VRF(VRFNum)%CoolEIRFTHi = GetCurveIndex(cAlphaArgs(8))
      IF(VRF(VRFNum)%CoolEIRFTHi .GT. 0)THEN
        ! Verify Curve Object, only legal type is biquadratic
        SELECT CASE(GetCurveType(VRF(VRFNum)%CoolEIRFTHi))
        CASE('BIQUADRATIC')
        CASE DEFAULT
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//', "'//TRIM(VRF(VRFNum)%Name)// &
                     '" illegal '//TRIM(cAlphaFieldNames(8))//' type for this object = '// &
                              TRIM(GetCurveType(VRF(VRFNum)%CoolEIRFTHi)))
          CALL ShowContinueError('... curve type must be BiQuadratic.')
          ErrorsFound=.TRUE.
        END SELECT
      END IF

      VRF(VRFNum)%CoolEIRFPLR1 = GetCurveIndex(cAlphaArgs(9))
      IF(VRF(VRFNum)%CoolEIRFPLR1 .GT. 0)THEN
        ! Verify Curve Object, only legal type is linear, quadratic, or cubic
        SELECT CASE(GetCurveType(VRF(VRFNum)%CoolEIRFPLR1))
        CASE('LINEAR', 'QUADRATIC', 'CUBIC')
        CASE DEFAULT
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//', "'//TRIM(VRF(VRFNum)%Name)// &
                     '" illegal '//TRIM(cAlphaFieldNames(9))//' type for this object = '// &
                              TRIM(GetCurveType(VRF(VRFNum)%CoolEIRFPLR1)))
          CALL ShowContinueError('... curve type must be Linear, Quadratic or Cubic.')
          ErrorsFound=.TRUE.
        END SELECT
!  only show error if cooling coil is present, since TU's have not yet been read, do this later in GetInput
!      ELSE
!        CALL ShowSevereError(TRIM(cCurrentModuleObject)//', "'//TRIM(VRF(VRFNum)%Name)// &
!                     '" '//TRIM(cAlphaFieldNames(9))//' not found.')
!        ErrorsFound=.TRUE.
      END IF

      VRF(VRFNum)%CoolEIRFPLR2 = GetCurveIndex(cAlphaArgs(10))
      IF(VRF(VRFNum)%CoolEIRFPLR2 .GT. 0)THEN
        ! Verify Curve Object, only legal type is linear, quadratic, or cubic
        SELECT CASE(GetCurveType(VRF(VRFNum)%CoolEIRFPLR2))
        CASE('LINEAR', 'QUADRATIC', 'CUBIC')
        CASE DEFAULT
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//', "'//TRIM(VRF(VRFNum)%Name)// &
                     '" illegal '//TRIM(cAlphaFieldNames(10))//' type for this object = '// &
                              TRIM(GetCurveType(VRF(VRFNum)%CoolEIRFPLR2)))
          CALL ShowContinueError('... curve type must be Linear, Quadratic or Cubic.')
          ErrorsFound=.TRUE.
        END SELECT
      END IF

      VRF(VRFNum)%CoolCombRatioPTR = GetCurveIndex(cAlphaArgs(11))
      IF(VRF(VRFNum)%CoolCombRatioPTR .GT. 0)THEN
        ! Verify Curve Object, only legal type is linear, quadratic, or cubic
        SELECT CASE(GetCurveType(VRF(VRFNum)%CoolCombRatioPTR))
        CASE('LINEAR', 'QUADRATIC', 'CUBIC')
        CASE DEFAULT
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//', "'//TRIM(VRF(VRFNum)%Name)// &
                     '" illegal '//TRIM(cAlphaFieldNames(11))//' type for this object = '// &
                              TRIM(GetCurveType(VRF(VRFNum)%CoolCombRatioPTR)))
          CALL ShowContinueError('... curve type must be Linear, Quadratic or Cubic.')
          ErrorsFound=.TRUE.
        END SELECT
      END IF

      VRF(VRFNum)%CoolPLFFPLR = GetCurveIndex(cAlphaArgs(12))
      IF(VRF(VRFNum)%CoolPLFFPLR .GT. 0)THEN
        ! Verify Curve Object, only legal type is linear, quadratic, or cubic
        SELECT CASE(GetCurveType(VRF(VRFNum)%CoolPLFFPLR))
        CASE('LINEAR', 'QUADRATIC', 'CUBIC')
        CASE DEFAULT
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//', "'//TRIM(VRF(VRFNum)%Name)// &
                     '" illegal '//TRIM(cAlphaFieldNames(12))//' type for this object = '// &
                              TRIM(GetCurveType(VRF(VRFNum)%CoolPLFFPLR)))
          CALL ShowContinueError('... curve type must be Linear, Quadratic or Cubic.')
          ErrorsFound=.TRUE.
        END SELECT
      END IF

      VRF(VRFNum)%HeatingCapacity = rNumericArgs(5)
      VRF(VRFNum)%HeatingCOP      = rNumericArgs(6)
      VRF(VRFNum)%MinOATHeating   = rNumericArgs(7)
      VRF(VRFNum)%MaxOATHeating   = rNumericArgs(8)
      IF(VRF(VRFNum)%MinOATHeating .GE. VRF(VRFNum)%MaxOATHeating)THEN
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//', "'//TRIM(VRF(VRFNum)%Name)//'"')
        CALL ShowContinueError('... '//TRIM(cNumericFieldNames(7))//' ('//TRIM(TrimSigDigits(VRF(VRFNum)%MinOATHeating,3))// &
                               ') must be less than maximum ('//TRIM(TrimSigDigits(VRF(VRFNum)%MaxOATHeating,3))//').')
        ErrorsFound=.TRUE.
      END IF

      VRF(VRFNum)%HeatCapFT = GetCurveIndex(cAlphaArgs(13))
      IF(VRF(VRFNum)%HeatCapFT .GT. 0)THEN
        ! Verify Curve Object, only legal type is biquadratic
        SELECT CASE(GetCurveType(VRF(VRFNum)%HeatCapFT))
        CASE('BIQUADRATIC')
        CASE DEFAULT
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//', "'//TRIM(VRF(VRFNum)%Name)// &
                     '" illegal '//TRIM(cAlphaFieldNames(13))//' type for this object = '// &
                              TRIM(GetCurveType(VRF(VRFNum)%HeatCapFT)))
          CALL ShowContinueError('... curve type must be BiQuadratic.')
          ErrorsFound=.TRUE.
        END SELECT
!  only show error if heating coil is present, since TU's have not yet been read, do this later in GetInput
!      ELSE
!        CALL ShowSevereError(TRIM(cCurrentModuleObject)//', "'//TRIM(VRF(VRFNum)%Name)// &
!                     '" '//TRIM(cAlphaFieldNames(13))//' not found.')
!        ErrorsFound=.TRUE.
      END IF

      VRF(VRFNum)%HeatBoundaryCurvePtr = GetCurveIndex(cAlphaArgs(14))
      IF(VRF(VRFNum)%HeatBoundaryCurvePtr .GT. 0)THEN
        ! Verify Curve Object, only legal type is linear, quadratic, or cubic
        SELECT CASE(GetCurveType(VRF(VRFNum)%HeatBoundaryCurvePtr))
        CASE('LINEAR', 'QUADRATIC', 'CUBIC')
        CASE DEFAULT
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//', "'//TRIM(VRF(VRFNum)%Name)// &
                     '" illegal '//TRIM(cAlphaFieldNames(14))//' type for this object = '// &
                              TRIM(GetCurveType(VRF(VRFNum)%HeatBoundaryCurvePtr)))
          CALL ShowContinueError('... curve type must be Linear, Quadratic or Cubic.')
          ErrorsFound=.TRUE.
        END SELECT
      END IF

      VRF(VRFNum)%HeatCapFTHi = GetCurveIndex(cAlphaArgs(15))
      IF(VRF(VRFNum)%HeatCapFTHi .GT. 0)THEN
        ! Verify Curve Object, only legal type is biquadratic
        SELECT CASE(GetCurveType(VRF(VRFNum)%HeatCapFTHi))
        CASE('BIQUADRATIC')
        CASE DEFAULT
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//', "'//TRIM(VRF(VRFNum)%Name)// &
                     '" illegal '//TRIM(cAlphaFieldNames(15))//' type for this object = '// &
                              TRIM(GetCurveType(VRF(VRFNum)%HeatCapFTHi)))
          CALL ShowContinueError('... curve type must be BiQuadratic.')
          ErrorsFound=.TRUE.
        END SELECT
      END IF

      VRF(VRFNum)%HeatEIRFT = GetCurveIndex(cAlphaArgs(16))
      IF(VRF(VRFNum)%HeatEIRFT .GT. 0)THEN
        ! Verify Curve Object, only legal type is biquadratic
        SELECT CASE(GetCurveType(VRF(VRFNum)%HeatEIRFT))
        CASE('BIQUADRATIC')
        CASE DEFAULT
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//', "'//TRIM(VRF(VRFNum)%Name)// &
                     '" illegal '//TRIM(cAlphaFieldNames(16))//' type for this object = '// &
                              TRIM(GetCurveType(VRF(VRFNum)%HeatEIRFT)))
          CALL ShowContinueError('... curve type must be BiQuadratic.')
          ErrorsFound=.TRUE.
        END SELECT
!  only show error if heating coil is present, since TU's have not yet been read, do this later in GetInput
!      ELSE
!        CALL ShowSevereError(TRIM(cCurrentModuleObject)//', "'//TRIM(VRF(VRFNum)%Name)// &
!                     '" '//TRIM(cAlphaFieldNames(16))//' not found.')
!        ErrorsFound=.TRUE.
      END IF

      VRF(VRFNum)%EIRHeatBoundaryCurvePtr = GetCurveIndex(cAlphaArgs(17))
      IF(VRF(VRFNum)%EIRHeatBoundaryCurvePtr .GT. 0)THEN
        ! Verify Curve Object, only legal type is linear, quadratic, or cubic
        SELECT CASE(GetCurveType(VRF(VRFNum)%EIRHeatBoundaryCurvePtr))
        CASE('LINEAR', 'QUADRATIC', 'CUBIC')
        CASE DEFAULT
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//', "'//TRIM(VRF(VRFNum)%Name)// &
                     '" illegal '//TRIM(cAlphaFieldNames(17))//' type for this object = '// &
                              TRIM(GetCurveType(VRF(VRFNum)%EIRHeatBoundaryCurvePtr)))
          CALL ShowContinueError('... curve type must be Linear, Quadratic or Cubic.')
          ErrorsFound=.TRUE.
        END SELECT
      END IF

      VRF(VRFNum)%HeatEIRFTHi = GetCurveIndex(cAlphaArgs(18))
      IF(VRF(VRFNum)%HeatEIRFTHi .GT. 0)THEN
        ! Verify Curve Object, only legal type is biquadratic
        SELECT CASE(GetCurveType(VRF(VRFNum)%HeatEIRFTHi))
        CASE('BIQUADRATIC')
        CASE DEFAULT
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//', "'//TRIM(VRF(VRFNum)%Name)// &
                     '" illegal '//TRIM(cAlphaFieldNames(18))//' type for this object = '// &
                              TRIM(GetCurveType(VRF(VRFNum)%HeatEIRFTHi)))
          CALL ShowContinueError('... curve type must be BiQuadratic.')
          ErrorsFound=.TRUE.
        END SELECT
      END IF

      IF(SameString(cAlphaArgs(19),'WETBULBTEMPERATURE'))THEN
        VRF(VRFNum)%HeatingPerformanceOATType = WetBulbIndicator
      ELSE IF(SameString(cAlphaArgs(19),'DRYBULBTEMPERATURE'))THEN
        VRF(VRFNum)%HeatingPerformanceOATType = DryBulbIndicator
      ELSE
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//', "'//TRIM(VRF(VRFNum)%Name)// &
                     '" illegal '//TRIM(cAlphaFieldNames(19))//' input for this object = '// &
                              TRIM(cAlphaArgs(19)))
          CALL ShowContinueError('... input must be WETBULBTEMPERATURE or DRYBULBTEMPERATURE.')
          ErrorsFound=.TRUE.
      END IF

      VRF(VRFNum)%HeatEIRFPLR1 = GetCurveIndex(cAlphaArgs(20))
      IF(VRF(VRFNum)%HeatEIRFPLR1 .GT. 0)THEN
        ! Verify Curve Object, only legal type is linear, quadratic, or cubic
        SELECT CASE(GetCurveType(VRF(VRFNum)%HeatEIRFPLR1))
        CASE('LINEAR', 'QUADRATIC', 'CUBIC')
        CASE DEFAULT
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//', "'//TRIM(VRF(VRFNum)%Name)// &
                     '" illegal '//TRIM(cAlphaFieldNames(20))//' type for this object = '// &
                              TRIM(GetCurveType(VRF(VRFNum)%HeatEIRFPLR1)))
          CALL ShowContinueError('... curve type must be Linear, Quadratic or Cubic.')
          ErrorsFound=.TRUE.
        END SELECT
      END IF

      VRF(VRFNum)%HeatEIRFPLR2 = GetCurveIndex(cAlphaArgs(21))
      IF(VRF(VRFNum)%HeatEIRFPLR2 .GT. 0)THEN
        ! Verify Curve Object, only legal type is linear, quadratic, or cubic
        SELECT CASE(GetCurveType(VRF(VRFNum)%HeatEIRFPLR2))
        CASE('LINEAR', 'QUADRATIC', 'CUBIC')
        CASE DEFAULT
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//', "'//TRIM(VRF(VRFNum)%Name)// &
                     '" illegal '//TRIM(cAlphaFieldNames(21))//' type for this object = '// &
                              TRIM(GetCurveType(VRF(VRFNum)%HeatEIRFPLR2)))
          CALL ShowContinueError('... curve type must be Linear, Quadratic or Cubic.')
          ErrorsFound=.TRUE.
        END SELECT
      END IF

      VRF(VRFNum)%HeatCombRatioPTR = GetCurveIndex(cAlphaArgs(22))
      IF(VRF(VRFNum)%HeatCombRatioPTR .GT. 0)THEN
        ! Verify Curve Object, only legal type is linear, quadratic, or cubic
        SELECT CASE(GetCurveType(VRF(VRFNum)%HeatCombRatioPTR))
        CASE('LINEAR', 'QUADRATIC', 'CUBIC')
        CASE DEFAULT
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//', "'//TRIM(VRF(VRFNum)%Name)// &
                     '" illegal '//TRIM(cAlphaFieldNames(22))//' type for this object = '// &
                              TRIM(GetCurveType(VRF(VRFNum)%HeatCombRatioPTR)))
          CALL ShowContinueError('... curve type must be Linear, Quadratic or Cubic.')
          ErrorsFound=.TRUE.
        END SELECT
      END IF
      VRF(VRFNum)%HeatPLFFPLR = GetCurveIndex(cAlphaArgs(23))
      IF(VRF(VRFNum)%HeatPLFFPLR .GT. 0)THEN
        ! Verify Curve Object, only legal type is linear, quadratic, or cubic
        SELECT CASE(GetCurveType(VRF(VRFNum)%HeatPLFFPLR))
        CASE('LINEAR', 'QUADRATIC', 'CUBIC')
        CASE DEFAULT
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//', "'//TRIM(VRF(VRFNum)%Name)// &
                     '" illegal '//TRIM(cAlphaFieldNames(23))//' type for this object = '// &
                              TRIM(GetCurveType(VRF(VRFNum)%HeatPLFFPLR)))
          CALL ShowContinueError('... curve type must be Linear, Quadratic or Cubic.')
          ErrorsFound=.TRUE.
        END SELECT
      END IF

      VRF(VRFNum)%MinPLR = rNumericArgs(9)

      VRF(VRFNum)%MasterZonePtr = FindItemInList(cAlphaArgs(24),Zone%Name,NumOfZones)

      IF (SameString(cAlphaArgs(25),'LoadPriority') )THEN
        VRF(VRFNum)%ThermostatPriority = LoadPriority
      ELSE IF (SameString(cAlphaArgs(25),'ZonePriority') )THEN
        VRF(VRFNum)%ThermostatPriority = ZonePriority
      ELSE IF (SameString(cAlphaArgs(25),'ThermostatOffsetPriority') )THEN
        VRF(VRFNum)%ThermostatPriority = ThermostatOffsetPriority
      ELSE IF (SameString(cAlphaArgs(25),'Scheduled') )THEN
        VRF(VRFNum)%ThermostatPriority = ScheduledPriority
      ELSE IF (SameString(cAlphaArgs(25),'MasterThermostatPriority') )THEN
        VRF(VRFNum)%ThermostatPriority = MasterThermostatPriority
        IF(VRF(VRFNum)%MasterZonePtr == 0)THEN
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//' = "'//TRIM(VRF(VRFNum)%Name)//'"')
          CALL ShowContinueError(TRIM(cAlphaFieldNames(24))//' must be entered when '// &
          TRIM(cAlphaFieldNames(25))//' = '//TRIM(cAlphaArgs(25)))
          ErrorsFound = .TRUE.
        END IF
!      ELSE IF (SameString(cAlphaArgs(25),'FirstOnPriority') )THEN ! strategy not used
!        VRF(VRFNum)%ThermostatPriority = FirstOnPriority
      ELSE
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//' = '//TRIM(VRF(VRFNum)%Name))
        CALL ShowContinueError('Illegal '//TRIM(cAlphaFieldNames(25))//' = '//TRIM(cAlphaArgs(25)))
        ErrorsFound = .TRUE.
      END IF

      IF(VRF(VRFNum)%ThermostatPriority == ScheduledPriority)THEN
        VRF(VRFNum)%SchedPriorityPtr = GetScheduleIndex(cAlphaArgs(26))
        IF(VRF(VRFNum)%SchedPriorityPtr == 0)THEN
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//' = '//TRIM(VRF(VRFNum)%Name))
          CALL ShowContinueError('...'//TRIM(cAlphaFieldNames(26))//' = '//TRIM(cAlphaArgs(26))//' not found.')
          CALL ShowContinueError('A schedule name is required when '//TRIM(cAlphaFieldNames(25))//' = '//TRIM(cAlphaArgs(25)))
          ErrorsFound = .TRUE.
        END IF
      END IF

      VRF(VRFNum)%ZoneTUListPtr = FindItemInList(cAlphaArgs(27),TerminalUnitList%Name,NumVRFTULists)
      IF(VRF(VRFNum)%ZoneTUListPtr == 0)THEN
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//' = "'//TRIM(VRF(VRFNum)%Name)//'"')
        CALL ShowContinueError(TRIM(cAlphaFieldNames(27))//' = '//TRIM(cAlphaArgs(27))//' not found.')
        ErrorsFound = .TRUE.
      END IF

      VRF(VRFNum)%HeatRecoveryUsed = .FALSE.
      IF(.NOT. lAlphaFieldBlanks(28))THEN
        IF (SameString(cAlphaArgs(28),'No') )THEN
          VRF(VRFNum)%HeatRecoveryUsed = .FALSE.
!        ELSE IF (SameString(cAlphaArgs(28),'Yes') )THEN ! strategy not yet available
!          VRF(VRFNum)%HeatRecoveryUsed = .TRUE.
        ELSE
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//' = '//TRIM(VRF(VRFNum)%Name))
          CALL ShowContinueError('Illegal '//TRIM(cAlphaFieldNames(28))//' = '//TRIM(cAlphaArgs(28)))
          ErrorsFound = .TRUE.
        END IF
      END IF

      VRF(VRFNum)%EquivPipeLngthCool = rNumericArgs(10)
      VRF(VRFNum)%VertPipeLngth      = rNumericArgs(11)
      VRF(VRFNum)%PCFLengthCoolPtr   = GetCurveIndex(cAlphaArgs(29))
      IF(VRF(VRFNum)%PCFLengthCoolPtr .GT. 0)THEN
        ! Verify Curve Object, only legal type is linear, quadratic, cubic, or biquadratic
        SELECT CASE(GetCurveType(VRF(VRFNum)%PCFLengthCoolPtr))
        CASE('LINEAR', 'QUADRATIC', 'CUBIC')
        CASE('BIQUADRATIC')
          VRF(VRFNum)%PCFLengthCoolPtrType = BIQUADRATIC
        CASE DEFAULT
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//', "'//TRIM(VRF(VRFNum)%Name)// &
                     '" illegal '//TRIM(cAlphaFieldNames(29))//' type for this object = '// &
                              TRIM(GetCurveType(VRF(VRFNum)%PCFLengthCoolPtr)))
          CALL ShowContinueError('... curve type must be Linear, Quadratic, Cubic or BiQuadratic.')
          ErrorsFound=.TRUE.
        END SELECT
      END IF
      VRF(VRFNum)%PCFHeightCool      = rNumericArgs(12)

      VRF(VRFNum)%EquivPipeLngthHeat = rNumericArgs(13)
      VRF(VRFNum)%PCFLengthHeatPtr = GetCurveIndex(cAlphaArgs(30))
      IF(VRF(VRFNum)%PCFLengthHeatPtr .GT. 0)THEN
        ! Verify Curve Object, only legal type is linear, quadratic, cubic, or biquadratic
        SELECT CASE(GetCurveType(VRF(VRFNum)%PCFLengthHeatPtr))
        CASE('LINEAR', 'QUADRATIC', 'CUBIC')
        CASE('BIQUADRATIC')
          VRF(VRFNum)%PCFLengthHeatPtrType = BIQUADRATIC
        CASE DEFAULT
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//', "'//TRIM(VRF(VRFNum)%Name)// &
                     '" illegal '//TRIM(cAlphaFieldNames(30))//' type for this object = '// &
                              TRIM(GetCurveType(VRF(VRFNum)%PCFLengthHeatPtr)))
          CALL ShowContinueError('... curve type must be Linear, Quadratic, Cubic or BiQuadratic.')
          ErrorsFound=.TRUE.
        END SELECT
      END IF
      VRF(VRFNum)%PCFHeightHeat      = rNumericArgs(14)

      VRF(VRFNum)%CCHeaterPower      = rNumericArgs(15)
      VRF(VRFNum)%NumCompressors     = rNumericArgs(16)
      VRF(VRFNum)%CompressorSizeRatio = rNumericArgs(17)
      VRF(VRFNum)%MaxOATCCHeater     = rNumericArgs(18)


      IF(.NOT. lAlphaFieldBlanks(31))THEN
        IF (SameString(cAlphaArgs(31),'ReverseCycle'))  VRF(VRFNum)%DefrostStrategy = ReverseCycle
        IF (SameString(cAlphaArgs(31),'Resistive')) VRF(VRFNum)%DefrostStrategy = Resistive
        IF (VRF(VRFNum)%DefrostStrategy .EQ.0) THEN
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//', "'//TRIM(VRF(VRFNum)%Name)//&
                         '" '//TRIM(cAlphaFieldNames(31))//' not found: '//TRIM(cAlphaArgs(31)))
          ErrorsFound = .TRUE.
        END IF
      ELSE
        VRF(VRFNum)%DefrostStrategy = ReverseCycle
      END IF

      IF(.NOT. lAlphaFieldBlanks(32))THEN
        IF (SameString(cAlphaArgs(32),'Timed'))    VRF(VRFNum)%DefrostControl = Timed
        IF (SameString(cAlphaArgs(32),'OnDemand')) VRF(VRFNum)%DefrostControl = OnDemand
        IF (VRF(VRFNum)%DefrostControl .EQ.0) THEN
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//', "'//TRIM(VRF(VRFNum)%Name)//&
                               '" '//TRIM(cAlphaFieldNames(32))//' not found: '//TRIM(cAlphaArgs(32)))
          ErrorsFound = .TRUE.
        END IF
      ELSE
        VRF(VRFNum)%DefrostControl = Timed
      END IF

      IF (.NOT. lAlphaFieldBlanks(33)) THEN
        VRF(VRFNum)%DefrostEIRPtr = GetCurveIndex(cAlphaArgs(33))
        IF(VRF(VRFNum)%DefrostEIRPtr .GT. 0)THEN
          ! Verify Curve Object, only legal type is linear, quadratic, or cubic
          SELECT CASE(GetCurveType(VRF(VRFNum)%DefrostEIRPtr))
            CASE('BIQUADRATIC')
            CASE DEFAULT
              CALL ShowSevereError(TRIM(cCurrentModuleObject)//', "'//TRIM(VRF(VRFNum)%Name)// &
                     '" illegal '//TRIM(cAlphaFieldNames(33))//' type for this object = '// &
                              TRIM(GetCurveType(VRF(VRFNum)%DefrostEIRPtr)))
              CALL ShowContinueError('... curve type must be BiQuadratic.')
              ErrorsFound=.TRUE.
          END SELECT
        ELSE
          IF (SameString(cAlphaArgs(31),'ReverseCycle')) THEN
            CALL ShowSevereError(TRIM(cCurrentModuleObject)//', "'//TRIM(VRF(VRFNum)%Name)//&
                           '" '//TRIM(cAlphaFieldNames(33))//' not found:'//TRIM(cAlphaArgs(33)))
            ErrorsFound = .TRUE.
          END IF
        END IF
      ELSE
        IF (SameString(cAlphaArgs(31),'ReverseCycle')) THEN
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//', "'//TRIM(VRF(VRFNum)%Name)//&
                         '" '//TRIM(cAlphaFieldNames(33))//' not found:'//TRIM(cAlphaArgs(33)))
          ErrorsFound = .TRUE.
        END IF
      END IF

      VRF(VRFNum)%DefrostFraction = rNumericArgs(19)
      VRF(VRFNum)%DefrostCapacity = rNumericArgs(20)
      IF(VRF(VRFNum)%DefrostCapacity .EQ. 0.0d0 .AND. VRF(VRFNum)%DefrostStrategy .EQ. RESISTIVE) THEN
        CALL ShowWarningError(TRIM(cCurrentModuleObject)//', "'//TRIM(VRF(VRFNum)%Name)//&
                          '" '//TRIM(cNumericFieldNames(20))//' = 0.0 for defrost strategy = RESISTIVE.')
      END IF

      VRF(VRFNum)%MaxOATDefrost = rNumericArgs(21)

      IF (.NOT. lAlphaFieldBlanks(35)) THEN
        IF (SameString(cAlphaArgs(34),'AirCooled') ) VRF(VRFNum)%CondenserType = AirCooled
        IF (SameString(cAlphaArgs(34),'EvaporativelyCooled') ) VRF(VRFNum)%CondenserType = EvapCooled
        IF (VRF(VRFNum)%CondenserType .EQ. 0) THEN
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//' = '//TRIM(VRF(VRFNum)%Name))
          CALL ShowContinueError('Illegal '//TRIM(cAlphaFieldNames(34))//' = '//TRIM(cAlphaArgs(34)))
          ErrorsFound = .TRUE.
        END IF
      ELSE
        VRF(VRFNum)%CondenserType = AirCooled
      END IF

      ! outdoor condenser node
      IF (lAlphaFieldBlanks(35)) THEN
        VRF(VRFNum)%CondenserNodeNum = 0
      ELSE
        VRF(VRFNum)%CondenserNodeNum = &
           GetOnlySingleNode(cAlphaArgs(35),ErrorsFound,TRIM(cCurrentModuleObject),VRF(VRFNum)%Name, &
                         NodeType_Air,NodeConnectionType_OutsideAirReference,1,ObjectIsNotParent)
        IF (.not. CheckOutAirNodeNumber(VRF(VRFNum)%CondenserNodeNum)) THEN
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//', "'//TRIM(VRF(VRFNum)%Name)//&
                          '" '//TRIM(cAlphaFieldNames(35))//' not a valid Outdoor Air Node = '//TRIM(cAlphaArgs(35)))
          CALL ShowContinueError('...node name does not appear in an OutdoorAir:NodeList or as an OutdoorAir:Node.')
          ErrorsFound=.TRUE.
        END IF
      ENDIF

      VRF(VRFNum)%EvapCondEffectiveness = rNumericArgs(22)
      VRF(VRFNum)%EvapCondAirVolFlowRate = rNumericArgs(23)
      VRF(VRFNum)%EvapCondPumpPower = rNumericArgs(24)

      ! Get Water System tank connections
      ! A36, \field Supply Water Storage Tank Name
      VRF(VRFNum)%EvapWaterSupplyName = cAlphaArgs(36)
      IF (lAlphaFieldBlanks(36)) THEN
        VRF(VRFNum)%EvapWaterSupplyMode = WaterSupplyFromMains
      ELSE
        VRF(VRFNum)%EvapWaterSupplyMode = WaterSupplyFromTank
        CALL SetupTankDemandComponent(VRF(VRFNum)%Name,TRIM(cCurrentModuleObject), &
                 VRF(VRFNum)%EvapWaterSupplyName, ErrorsFound, VRF(VRFNum)%EvapWaterSupTankID, &
                 VRF(VRFNum)%EvapWaterTankDemandARRID )
      ENDIF

      !   Basin heater power as a function of temperature must be greater than or equal to 0
      VRF(VRFNum)%BasinHeaterPowerFTempDiff = rNumericArgs(25)
      IF(rNumericArgs(25) .LT. 0.0d0) THEN
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//' = "'//TRIM(VRF(VRFNum)%Name)//&
                   '", '//TRIM(cNumericFieldNames(25))//' must be >= 0')
        ErrorsFound = .TRUE.
      END IF

      VRF(VRFNum)%BasinHeaterSetPointTemp = rNumericArgs(26)
      IF(VRF(VRFNum)%BasinHeaterPowerFTempDiff .GT. 0.0d0) THEN
        IF(NumNums .LT. 26) THEN
          VRF(VRFNum)%BasinHeaterSetPointTemp = 2.0d0
        ENDIF
        IF(VRF(VRFNum)%BasinHeaterSetPointTemp < 2.0d0) THEN
          CALL ShowWarningError(TRIM(cCurrentModuleObject)//' = "'//TRIM(VRF(VRFNum)%Name)//&
           '", '//TRIM(cNumericFieldNames(26))//' is less than 2 deg C. Freezing could occur.')
        END IF
      END IF

      IF(.NOT. lAlphaFieldBlanks(37))THEN
        VRF(VRFNum)%BasinHeaterSchedulePtr   = GetScheduleIndex(cAlphaArgs(37))
        IF(VRF(VRFNum)%BasinHeaterSchedulePtr .EQ. 0)THEN
          CALL ShowWarningError(TRIM(cCurrentModuleObject)//' = "'//TRIM(VRF(VRFNum)%Name)//&
                     '", '// TRIM(cAlphaFieldNames(37))//' = "'//TRIM(cAlphaArgs(37))//&
                     '" was not found.')
          CALL ShowContinueError('Basin heater will be available to operate throughout the simulation.')
        END IF
      END IF

      VRF(VRFNum)%FuelType = FuelTypeElectric
      IF(.NOT. lAlphaFieldBlanks(38))THEN
        !A38; \field Fuel type
        IF (SameString(cAlphaArgs(38),"ELECTRIC")) THEN
          VRF(VRFNum)%FuelType = FuelTypeElectric
        ELSE IF (SameString(cAlphaArgs(38),"NATURALGAS")) THEN
          VRF(VRFNum)%FuelType = FuelTypeNaturalGas
        ELSE IF (SameString(cAlphaArgs(38),"PROPANEGAS")) THEN
          VRF(VRFNum)%FuelType = FuelTypePropaneGas
        ELSE IF (SameString(cAlphaArgs(38),"DIESEL")) THEN
          VRF(VRFNum)%FuelType = FuelTypeDiesel
        ELSE IF (SameString(cAlphaArgs(38),"GASOLINE")) THEN
          VRF(VRFNum)%FuelType = FuelTypeGasoline
        ELSE IF (SameString(cAlphaArgs(38),"FUELOIL#1")) THEN
          VRF(VRFNum)%FuelType = FuelTypeFuelOil1
        ELSE IF (SameString(cAlphaArgs(38),"FUELOIL#2")) THEN
          VRF(VRFNum)%FuelType = FuelTypeFuelOil2
        ELSE
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//', "'//TRIM(VRF(VRFNum)%Name)//&
               '", '//TRIM(cAlphaFieldNames(38))//' not found = '//TRIM(cAlphaArgs(38)))
          CALL ShowContinueError('Valid choices are Electric, NaturalGas, PropaneGas, Diesel, Gasoline, FuelOil#1 or FuelOil#2')
          ErrorsFound=.TRUE.
        END IF
      END IF
    END DO

    cCurrentModuleObject= 'ZoneHVAC:TerminalUnit:VariableRefrigerantFlow'
    DO VRFNum = 1,  NumVRFTU
      VRFTUNum = VRFNum

!     initialize local node number variables
      FanInletNodeNum    = 0
      FanOutletNodeNum   = 0
      CCoilInletNodeNum  = 0
      CCoilOutletNodeNum = 0
      HCoilInletNodeNum  = 0
      HCoilOutletNodeNum = 0

      CALL GetObjectItem(TRIM(cCurrentModuleObject),VRFTUNum,cAlphaArgs,NumAlphas, &
                         rNumericArgs,NumNums,IOSTAT, &
                         NumBlank=lNumericFieldBlanks,AlphaBlank=lAlphaFieldBlanks, &
                         AlphaFieldNames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
      IsNotOK=.false.
      IsBlank=.false.
      CALL VerifyName(cAlphaArgs(1),VRFTU%Name,VRFTUNum-1,IsNotOK,IsBlank,TRIM(cCurrentModuleObject)//' Name')
      IF (IsNotOK) THEN
        ErrorsFound=.true.
        IF (IsBlank) cAlphaArgs(1)='xxxxx'
      ENDIF
      VRFTU(VRFTUNum)%Name  = cAlphaArgs(1)
      ZoneTerminalUnitListNum = 0
      DO NumList = 1, NumVRFTULists
        ZoneTerminalUnitListNum = FindItemInList(VRFTU(VRFTUNum)%Name,TerminalUnitList(NumList)%ZoneTUName, &
                                                 TerminalUnitList(NumList)%NumTUInList)
        IF(ZoneTerminalUnitListNum .GT. 0)THEN
          VRFTU(VRFTUNum)%NumTUInList = ZoneTerminalUnitListNum
          TerminalUnitList(NumList)%ZoneTUPtr(ZoneTerminalUnitListNum) = VRFTUNum
          VRFTU(VRFTUNum)%TUListNum = NumList
          EXIT
        END IF
      END DO
      IF(VRFTU(VRFTUNum)%TUListNum .EQ. 0)THEN
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//' = '//TRIM(VRFTU(VRFTUNum)%Name))
        CALL ShowContinueError('Terminal unit not found on any ZoneTerminalUnitList.')
        ErrorsFound=.true.
      END IF

      DO NumCond = 1, NumVRFCond
        IF(VRF(NumCond)%ZoneTUListPtr /= VRFTU(VRFTUNum)%TUListNum)CYCLE
        VRFTU(VRFTUNum)%VRFSysNum = NumCond
        EXIT
      END DO
      VRFTU(VRFTUNum)%VRFTUType_Num = VRFTUType_ConstVolume
      IF (.NOT. lAlphaFieldBlanks(2)) VRFTU(VRFTUNum)%SchedPtr = GetScheduleIndex(cAlphaArgs(2))

      VRFTU(VRFTUNum)%VRFTUInletNodeNum = &
               GetOnlySingleNode(cAlphaArgs(3),ErrorsFound,TRIM(cCurrentModuleObject),VRFTU(VRFTUNum)%Name, &
                            NodeType_Air,NodeConnectionType_Inlet,1,ObjectIsParent)

      VRFTU(VRFTUNum)%VRFTUOutletNodeNum = &
               GetOnlySingleNode(cAlphaArgs(4),ErrorsFound,TRIM(cCurrentModuleObject),VRFTU(VRFTUNum)%Name, &
                            NodeType_Air,NodeConnectionType_Outlet,1,ObjectIsParent)

      VRFTU(VRFTUNum)%MaxCoolAirVolFlow       = rNumericArgs(1)
      VRFTU(VRFTUNum)%MaxNoCoolAirVolFlow     = rNumericArgs(2)
      VRFTU(VRFTUNum)%MaxHeatAirVolFlow       = rNumericArgs(3)
      VRFTU(VRFTUNum)%MaxNoHeatAirVolFlow     = rNumericArgs(4)
      VRFTU(VRFTUNum)%CoolOutAirVolFlow       = rNumericArgs(5)
      VRFTU(VRFTUNum)%HeatOutAirVolFlow       = rNumericArgs(6)
      VRFTU(VRFTUNum)%NoCoolHeatOutAirVolFlow = rNumericArgs(7)

      VRFTU(VRFTUNum)%FanOpModeSchedPtr = GetScheduleIndex(cAlphaArgs(5))
      ! default to constant fan operating mode
      IF(VRFTU(VRFTUNum)%FanOpModeSchedPtr == 0)VRFTU(VRFTUNum)%OpMode = ContFanCycCoil

      IF (SameString(cAlphaArgs(6),'BlowThrough') ) VRFTU(VRFTUNum)%FanPlace = BlowThru
      IF (SameString(cAlphaArgs(6),'DrawThrough') ) VRFTU(VRFTUNum)%FanPlace = DrawThru
      IF (VRFTU(VRFTUNum)%FanPlace .EQ.0) THEN
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//' = '//TRIM(VRFTU(VRFTUNum)%Name))
        CALL ShowContinueError('Illegal '//TRIM(cAlphaFieldNames(6))//' = '//TRIM(cAlphaArgs(6)))
        ErrorsFound = .TRUE.
      END IF

      !Get fan data
      FanType = cAlphaArgs(7)
      FanName = cAlphaArgs(8)

      ErrFlag=.FALSE.
      CALL GetFanType(TRIM(FanName), FanType_Num, ErrFlag, TRIM(cCurrentModuleObject))
      IF (ErrFlag) THEN
        CALL ShowContinueError('...occurs in '//TRIM(cCurrentModuleObject)//' = '//TRIM(VRFTU(VRFTUNum)%Name))
        ErrorsFound=.TRUE.
      END IF

      IF(.NOT. SameString(cFanTypes(FanType_Num),FanType))THEN
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//' = '//TRIM(VRFTU(VRFTUNum)%Name))
        CALL ShowContinueError('Fan type specified = '//TRIM(cAlphaArgs(7)))
        CALL ShowContinueError('Based on the fan name the type of fan actually used = '//TRIM(cFanTypes(FanType_Num)))
        ErrorsFound = .TRUE.
      END IF

      IF (FanType_Num == FanType_SimpleOnOff .OR. FanType_Num == FanType_SimpleConstVolume)THEN

        CALL ValidateComponent(cFanTypes(FanType_Num),FanName,IsNotOK,TRIM(cCurrentModuleObject))
        IF (IsNotOK) THEN
          CALL ShowContinueError('...occurs in '//TRIM(cCurrentModuleObject)//' = '//TRIM(VRFTU(VRFTUNum)%Name))
          ErrorsFound=.TRUE.

        ELSE ! mine data from fan object

          ! Get the fan index
          ErrFlag=.FALSE.
          CALL GetFanIndex(FanName, VRFTU(VRFTUNum)%FanIndex, ErrFlag)
          IF (ErrFlag) THEN
            CALL ShowContinueError('...occurs in '//TRIM(cCurrentModuleObject)//' = '//TRIM(VRFTU(VRFTUNum)%Name))
            ErrorsFound=.TRUE.
          ENDIF

          !Set the Design Fan Volume Flow Rate
          ErrFlag=.FALSE.
          FanVolFlowRate = GetFanDesignVolumeFlowRate(FanType,FanName,ErrFlag)
          VRFTU(VRFTUNum)%ActualFanVolFlowRate    = FanVolFlowRate

          IF (ErrFlag) THEN
            CALL ShowContinueError('...occurs in '//TRIM(cCurrentModuleObject)//' ='//TRIM(VRFTU(VRFTUNum)%Name))
            ErrorsFound=.TRUE.
          ENDIF

          ! Get the Fan Inlet Node
          ErrFlag=.FALSE.
          FanInletNodeNum = GetFanInletNode(FanType,FanName,ErrFlag)
          IF (ErrFlag) THEN
            CALL ShowContinueError('...occurs in '//TRIM(cCurrentModuleObject)//' = '//TRIM(VRFTU(VRFTUNum)%Name))
            ErrorsFound=.TRUE.
          ENDIF

          ! Get the Fan Outlet Node
          ErrFlag=.FALSE.
          FanOutletNodeNum = GetFanOutletNode(FanType,FanName,ErrFlag)
          IF (ErrFlag) THEN
            CALL ShowContinueError('...occurs in '//TRIM(cCurrentModuleObject)//' = '//TRIM(VRFTU(VRFTUNum)%Name))
            ErrorsFound=.TRUE.
          ENDIF

          ! Get the fan's availabitlity schedule
          ErrFlag=.FALSE.
          VRFTU(VRFTUNum)%FanAvailSchedPtr = GetFanAvailSchPtr(FanType,FanName,ErrFlag)
          IF (ErrFlag) THEN
            CALL ShowContinueError('...occurs in '//TRIM(cCurrentModuleObject)//' = '//TRIM(VRFTU(VRFTUNum)%Name))
            ErrorsFound=.TRUE.
          ENDIF

          ! Check fan's schedule for cycling fan operation if constant volume fan is used
          IF(VRFTU(VRFTUNum)%FanOpModeSchedPtr .GT. 0 .AND. FanType_Num == FanType_SimpleConstVolume)THEN
            IF (.NOT. CheckScheduleValueMinMax(VRFTU(VRFTUNum)%FanOpModeSchedPtr,'>',0.,'<=',1.)) THEN
              CALL ShowSevereError(TRIM(cCurrentModuleObject)//' = '//TRIM(VRFTU(VRFTUNum)%Name))
              CALL ShowContinueError('For fan type = '//TRIM(cFanTypes(FanType_SimpleConstVolume)))
              CALL ShowContinueError('Fan operating mode must be continuous (fan operating mode schedule values > 0).')
              CALL ShowContinueError('Error found in '//TRIM(cAlphaFieldNames(5))//' = '//TRIM(cAlphaArgs(5)))
              CALL ShowContinueError('...schedule values must be (>0., <=1.)')
              ErrorsFound=.TRUE.
            END IF
          END IF

        ENDIF ! IF (IsNotOK) THEN

      ELSE ! IF (FanType_Num == FanType_SimpleOnOff .OR. FanType_Num == FanType_SimpleConstVolume)THEN
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//' = '//TRIM(VRFTU(VRFTUNum)%Name))
        CALL ShowContinueError('Illegal '//TRIM(cAlphaFieldNames(7))//' = '//TRIM(cAlphaArgs(7)))
        ErrorsFound=.TRUE.
      END IF ! IF (FanType_Num == FanType_SimpleOnOff .OR. FanType_Num == FanType_SimpleConstVolume)THEN

      !Get OA mixer data
      OAMixerType = cAlphaArgs(9)

      IF(.NOT. lAlphaFieldBlanks(10))THEN
        VRFTU(VRFTUNum)%OAMixerName = cAlphaArgs(10)
        ErrFlag=.FALSE.
        OANodeNums = GetOAMixerNodeNumbers(VRFTU(VRFTUNum)%OAMixerName, ErrFlag)

!       OANodeNums(1) = OAMixer(OAMixerNum)%InletNode
!       OANodeNums(2) = OAMixer(OAMixerNum)%RelNode
!       OANodeNums(3) = OAMixer(OAMixerNum)%RetNode
!       OANodeNums(4) = OAMixer(OAMixerNum)%MixNode

        IF(ErrFlag)THEN
          CALL ShowContinueError('Occurs in '//TRIM(cCurrentModuleObject)//' = '//TRIM(VRFTU(VRFTUNum)%Name))
          ErrorsFound=.TRUE.
        ELSE
          VRFTU(VRFTUNum)%OAMixerUsed = .TRUE.
        END IF
        VRFTU(VRFTUNum)%VRFTUOAMixerOANodeNum      = OANodeNums(1)
        VRFTU(VRFTUNum)%VRFTUOAMixerRelNodeNum     = OANodeNums(2)
        VRFTU(VRFTUNum)%VRFTUOAMixerRetNodeNum     = OANodeNums(3)
      END IF

      !Get DX cooling coil data
      DXCoolingCoilType = cAlphaArgs(11)

       ErrFlag = .FALSE.
       VRFTU(VRFTUNum)%DXCoolCoilType_Num = GetCoilTypeNum(TRIM(DXCoolingCoilType),cAlphaArgs(12),ErrFlag,.FALSE.)
       IF(VRFTU(VRFTUNum)%DXCoolCoilType_Num == 0)THEN
         VRFTU(VRFTUNum)%CoolingCoilPresent = .FALSE.
         IF(VRFTU(VRFTUNum)%TUListNum .GT. 0 .AND. VRFTU(VRFTUNum)%NumTUInList .GT. 0)THEN
           TerminalUnitList(VRFTU(VRFTUNum)%TUListNum)%CoolingCoilPresent(VRFTU(VRFTUNum)%NumTUInList) = .FALSE.
         END IF
       ELSE
         IF (SameString(cAllCoilTypes(VRFTU(VRFTUNum)%DXCoolCoilType_Num),cAllCoilTypes(CoilVRF_Cooling))) THEN
           ErrFlag = .FALSE.
           CALL GetDXCoolCoilIndex(cAlphaArgs(12),VRFTU(VRFTUNum)%CoolCoilIndex, &
                                  ErrFlag, cAllCoilTypes(CoilVRF_Cooling))
           CCoilInletNodeNum = GetDXCoilInletNode(cAllCoilTypes(CoilVRF_Cooling),cAlphaArgs(12),ErrFlag)
           CCoilOutletNodeNum = GetDXCoilOutletNode(cAllCoilTypes(CoilVRF_Cooling),cAlphaArgs(12),ErrFlag)

           IF(ErrFlag)CALL ShowContinueError('...occurs in '//TRIM(cCurrentModuleObject)// &
                                           ' "'//TRIM(VRFTU(VRFTUNum)%Name)//'"')

           IF(VRFTU(VRFTUNum)%VRFSysNum .GT. 0)THEN
             CALL SetDXCoolingCoilData(VRFTU(VRFTUNum)%CoolCoilIndex,ErrorsFound, &
                    CondenserType=VRF(VRFTU(VRFTUNum)%VRFSysNum)%CondenserType)
             CALL SetDXCoolingCoilData(VRFTU(VRFTUNum)%CoolCoilIndex,ErrorsFound, &
                    CondenserInletNodeNum=VRF(VRFTU(VRFTUNum)%VRFSysNum)%CondenserNodeNum)
             CALL SetDXCoolingCoilData(VRFTU(VRFTUNum)%CoolCoilIndex,ErrorsFound, &
                    MaxOATCrankcaseHeater=VRF(VRFTU(VRFTUNum)%VRFSysNum)%MaxOATCCHeater)

             CALL SetDXCoolingCoilData(VRFTU(VRFTUNum)%CoolCoilIndex,ErrorsFound, &
                    MinOATCooling=VRF(VRFTU(VRFTUNum)%VRFSysNum)%MinOATCooling)
             CALL SetDXCoolingCoilData(VRFTU(VRFTUNum)%CoolCoilIndex,ErrorsFound, &
                    MaxOATCooling=VRF(VRFTU(VRFTUNum)%VRFSysNum)%MaxOATCooling)
          ELSE
            CALL ShowSevereError(TRIM(cCurrentModuleObject)//' "'//TRIM(VRFTU(VRFTUNum)%Name)//'"')
            CALL ShowContinueError('... when checking '//TRIM(cAllCoilTypes(VRFTU(VRFTUNum)%DXCoolCoilType_Num))// &
                                   ' "'//TRIM(cAlphaArgs(12))//'"')
            CALL ShowContinueError('... terminal unit not connected to condenser.')
            CALL ShowContinueError('... check that terminal unit is specified in a terminal unit list object.')
            CALL ShowContinueError('... also check that the terminal unit list name is specified in an '// &
                                   'AirConditioner:VariableRefrigerantFlow object.')
            ErrorsFound = .TRUE.
          END IF
        ELSE
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//' "'//TRIM(VRFTU(VRFTUNum)%Name)//'"')
          CALL ShowContinueError('... illegal '//TRIM(cAlphaFieldNames(12))//' = '//TRIM(cAlphaArgs(12)))
          ErrorsFound = .TRUE.
        END IF
      END IF

      !Get DX heating coil data
      DXHeatingCoilType = cAlphaArgs(13)

      ErrFlag = .FALSE.
      VRFTU(VRFTUNum)%DXHeatCoilType_Num = GetCoilTypeNum(TRIM(DXHeatingCoilType),cAlphaArgs(14),ErrFlag,.FALSE.)
      IF(VRFTU(VRFTUNum)%DXHeatCoilType_Num == 0)THEN
         VRFTU(VRFTUNum)%HeatingCoilPresent = .FALSE.
         IF(VRFTU(VRFTUNum)%TUListNum .GT. 0 .AND. VRFTU(VRFTUNum)%NumTUInList .GT. 0)THEN
           TerminalUnitList(VRFTU(VRFTUNum)%TUListNum)%HeatingCoilPresent(VRFTU(VRFTUNum)%NumTUInList) = .FALSE.
         END IF
      ELSE
        IF (SameString(cAllCoilTypes(VRFTU(VRFTUNum)%DXHeatCoilType_Num),cAllCoilTypes(CoilVRF_Heating))) THEN
          ErrFlag = .FALSE.
          CALL GetDXCoolCoilIndex(cAlphaArgs(14),VRFTU(VRFTUNum)%HeatCoilIndex, &
                                 ErrFlag, cAllCoilTypes(CoilVRF_Heating))
          HCoilInletNodeNum = GetDXCoilInletNode(cAllCoilTypes(CoilVRF_Heating),cAlphaArgs(14),ErrFlag)
          HCoilOutletNodeNum = GetDXCoilOutletNode(cAllCoilTypes(CoilVRF_Heating),cAlphaArgs(14),ErrFlag)

          IF(ErrFlag)CALL ShowContinueError('...occurs in '//TRIM(cCurrentModuleObject)// &
                                           ' "'//TRIM(VRFTU(VRFTUNum)%Name)//'"')

           IF(VRFTU(VRFTUNum)%VRFSysNum .GT. 0)THEN
             CALL SetDXCoolingCoilData(VRFTU(VRFTUNum)%HeatCoilIndex,ErrorsFound, &
                    CondenserType=VRF(VRFTU(VRFTUNum)%VRFSysNum)%CondenserType)
             CALL SetDXCoolingCoilData(VRFTU(VRFTUNum)%HeatCoilIndex,ErrorsFound, &
                    CondenserInletNodeNum=VRF(VRFTU(VRFTUNum)%VRFSysNum)%CondenserNodeNum)
             CALL SetDXCoolingCoilData(VRFTU(VRFTUNum)%HeatCoilIndex,ErrorsFound, &
                    MaxOATCrankcaseHeater=VRF(VRFTU(VRFTUNum)%VRFSysNum)%MaxOATCCHeater)

             CALL SetDXCoolingCoilData(VRFTU(VRFTUNum)%HeatCoilIndex,ErrorsFound, &
                    MinOATHeating=VRF(VRFTU(VRFTUNum)%VRFSysNum)%MinOATHeating)
             CALL SetDXCoolingCoilData(VRFTU(VRFTUNum)%HeatCoilIndex,ErrorsFound, &
                    MaxOATHeating=VRF(VRFTU(VRFTUNum)%VRFSysNum)%MaxOATHeating)
             CALL SetDXCoolingCoilData(VRFTU(VRFTUNum)%HeatCoilIndex,ErrorsFound, &
                    HeatingPerformanceOATType=VRF(VRFTU(VRFTUNum)%VRFSysNum)%HeatingPerformanceOATType)
             ! Set defrost controls in child object to trip child object defrost calculations
             CALL SetDXCoolingCoilData(VRFTU(VRFTUNum)%HeatCoilIndex,ErrorsFound, &
                    DefrostStrategy=VRF(VRFTU(VRFTUNum)%VRFSysNum)%DefrostStrategy)
             CALL SetDXCoolingCoilData(VRFTU(VRFTUNum)%HeatCoilIndex,ErrorsFound, &
                    DefrostControl=VRF(VRFTU(VRFTUNum)%VRFSysNum)%DefrostControl)
             CALL SetDXCoolingCoilData(VRFTU(VRFTUNum)%HeatCoilIndex,ErrorsFound, &
                    DefrostEIRPtr=VRF(VRFTU(VRFTUNum)%VRFSysNum)%DefrostEIRPtr)
             CALL SetDXCoolingCoilData(VRFTU(VRFTUNum)%HeatCoilIndex,ErrorsFound, &
                    DefrostFraction=VRF(VRFTU(VRFTUNum)%VRFSysNum)%DefrostFraction)
             CALL SetDXCoolingCoilData(VRFTU(VRFTUNum)%HeatCoilIndex,ErrorsFound, &
                    MaxOATDefrost=VRF(VRFTU(VRFTUNum)%VRFSysNum)%MaxOATDefrost)
! *** RAR, this needs correcting. If defrost is disabled in the VRF condenser, it must be disabled in the DX coil
             ! Defrost primarily handled in parent object, set defrost capacity to 1 to avoid autosizing.
             ! Defrost capacity is used for nothing more than setting defrost power/consumption report
             ! variables which are not reported. The coil's defrost algorythm IS used to derate the coil
             CALL SetDXCoolingCoilData(VRFTU(VRFTUNum)%HeatCoilIndex,ErrorsFound, &
                    DefrostCapacity=1.d0)
          ELSE
            CALL ShowSevereError(TRIM(cCurrentModuleObject)//' "'//TRIM(VRFTU(VRFTUNum)%Name)//'"')
            CALL ShowContinueError('... when checking '//TRIM(cAllCoilTypes(VRFTU(VRFTUNum)%DXHeatCoilType_Num))// &
                                   ' "'//TRIM(cAlphaArgs(14))//'"')
            CALL ShowContinueError('... terminal unit not connected to condenser.')
            CALL ShowContinueError('... check that terminal unit is specified in a terminal unit list object.')
            CALL ShowContinueError('... also check that the terminal unit list name is specified in an '// &
                                   'AirConditioner:VariableRefrigerantFlow object.')
            ErrorsFound = .TRUE.
          END IF
        ELSE
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//' "'//TRIM(VRFTU(VRFTUNum)%Name)//'"')
          CALL ShowContinueError('... illegal '//TRIM(cAlphaFieldNames(14))//' = '//TRIM(cAlphaArgs(14)))
          ErrorsFound = .TRUE.
        END IF
      END IF

      IF(.NOT. VRFTU(VRFTUNum)%CoolingCoilPresent .AND. .NOT. VRFTU(VRFTUNum)%HeatingCoilPresent)THEN
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//' "'//TRIM(VRFTU(VRFTUNum)%Name)//'"')
          CALL ShowContinueError('... no valid coils entered for this terminal unit. Simulation will not proceed.')
          ErrorsFound = .TRUE.
      END IF

      VRFTU(VRFTUNum)%ParasiticElec    = rNumericArgs(8)
      VRFTU(VRFTUNum)%ParasiticOffElec = rNumericArgs(9)

      ! Add TU to component sets array
      CALL SetUpCompSets(TRIM(cCurrentModuleObject), VRFTU(VRFTUNum)%Name, cFanTypes(FanType_Num), &
                       FanName,NodeID(FanInletNodeNum),NodeID(FanOutletNodeNum))

      ! Add cooling coil to component sets array
      IF(VRFTU(VRFTUNum)%CoolingCoilPresent)THEN
        CALL SetUpCompSets(TRIM(cCurrentModuleObject), VRFTU(VRFTUNum)%Name, cAllCoilTypes(VRFTU(VRFTUNum)%DXCoolCoilType_Num), &
                       cAlphaArgs(12),NodeID(CCoilInletNodeNum),NodeID(CCoilOutletNodeNum))
!     set heating coil present flag
        CALL SetDXCoolingCoilData(VRFTU(VRFTUNum)%CoolCoilIndex,ErrorsFound, &
                    HeatingCoilPresent=VRFTU(VRFTUNum)%HeatingCoilPresent)

! ***RAR move performance curve warnings here
!   check that curve types are present in VRF Condenser if cooling coil is present in terminal unit (can be blank)
!   all curves are checked for correct type if a curve name is entered in the VRF condenser object. Check that the
!   curve is present if the corresponding coil is entered in the terminal unit.
        IF(VRFTU(VRFTUNum)%VRFSysNum .GT. 0)THEN

          IF(VRF(VRFTU(VRFTUNum)%VRFSysNum)%CoolingCapacity .LE. 0 .AND. &
             VRF(VRFTU(VRFTUNum)%VRFSysNum)%CoolingCapacity .NE. Autosize)THEN
            CALL ShowSevereError(TRIM(cCurrentModuleObject)//' "'//TRIM(VRFTU(VRFTUNum)%Name)//'"')
            CALL ShowContinueError('...This terminal unit contains a cooling coil and rated cooling capacity is also '// &
                                 'required in the associated condenser object.')
            CALL ShowContinueError('...Rated Cooling Capacity must also be '// &
                                 'specified for condenser = '// &
                                  TRIM(cVRFTypes(VRF(VRFTU(VRFTUNum)%VRFSysNum)%VRFSystemTypeNum))//' "'// &
                                  TRIM(VRF(VRFTU(VRFTUNum)%VRFSysNum)%Name)//'".')
            ErrorsFound=.TRUE.
          END IF

          IF(VRF(VRFTU(VRFTUNum)%VRFSysNum)%CoolCapFT .EQ. 0)THEN
            CALL ShowSevereError(TRIM(cCurrentModuleObject)//' "'//TRIM(VRFTU(VRFTUNum)%Name)//'"')
            CALL ShowContinueError('...This terminal unit contains a cooling coil and cooling performance curves are also '// &
                                 'required in the associated condenser object.')
            CALL ShowContinueError('...Cooling Capacity Ratio Modifier Function of Low Temperature Curve must also be '// &
                                 'specified for condenser = '// &
                                  TRIM(cVRFTypes(VRF(VRFTU(VRFTUNum)%VRFSysNum)%VRFSystemTypeNum))//' "'// &
                                  TRIM(VRF(VRFTU(VRFTUNum)%VRFSysNum)%Name)//'".')
            ErrorsFound=.TRUE.
          END IF

          IF(VRF(VRFTU(VRFTUNum)%VRFSysNum)%CoolEIRFT .EQ. 0)THEN
            CALL ShowSevereError(TRIM(cCurrentModuleObject)//' "'//TRIM(VRFTU(VRFTUNum)%Name)//'"')
            CALL ShowContinueError('...This terminal unit contains a cooling coil and cooling performance curves are also '// &
                                 'required in the associated condenser object.')
            CALL ShowContinueError('...Cooling Energy Input Ratio Modifier Function of Low Temperature Curve must also be '// &
                                 'specified for condenser = '// &
                                  TRIM(cVRFTypes(VRF(VRFTU(VRFTUNum)%VRFSysNum)%VRFSystemTypeNum))//' "'// &
                                  TRIM(VRF(VRFTU(VRFTUNum)%VRFSysNum)%Name)//'".')
            ErrorsFound=.TRUE.
          END IF

          IF(VRF(VRFTU(VRFTUNum)%VRFSysNum)%CoolEIRFPLR1 .EQ. 0)THEN
            CALL ShowSevereError(TRIM(cCurrentModuleObject)//' "'//TRIM(VRFTU(VRFTUNum)%Name)//'"')
            CALL ShowContinueError('...This terminal unit contains a cooling coil and cooling performance curves are also '// &
                                 'required in the associated condenser object.')
            CALL ShowContinueError('...Cooling Energy Input Ratio Modifier Function of Low Part-Load Ratio Curve must also '// &
                                 'be specified for condenser = '// &
                                  TRIM(cVRFTypes(VRF(VRFTU(VRFTUNum)%VRFSysNum)%VRFSystemTypeNum))//' "'// &
                                  TRIM(VRF(VRFTU(VRFTUNum)%VRFSysNum)%Name)//'".')
            ErrorsFound=.TRUE.
          END IF

        END IF

      END IF

      ! Add heating coil to component sets array
      IF(VRFTU(VRFTUNum)%HeatingCoilPresent)THEN
        CALL SetUpCompSets(TRIM(cCurrentModuleObject), VRFTU(VRFTUNum)%Name, cAllCoilTypes(VRFTU(VRFTUNum)%DXHeatCoilType_Num), &
                       cAlphaArgs(14),NodeID(HCoilInletNodeNum),NodeID(HCoilOutletNodeNum))
!     set cooling coil present flag
        CALL SetDXCoolingCoilData(VRFTU(VRFTUNum)%HeatCoilIndex,ErrorsFound, &
                    CoolingCoilPresent=VRFTU(VRFTUNum)%CoolingCoilPresent)

        IF(VRFTU(VRFTUNum)%VRFSysNum .GT. 0)THEN

          IF(VRF(VRFTU(VRFTUNum)%VRFSysNum)%HeatingCapacity .LE. 0 .AND. &
             VRF(VRFTU(VRFTUNum)%VRFSysNum)%HeatingCapacity .NE. Autosize)THEN
            CALL ShowSevereError(TRIM(cCurrentModuleObject)//' "'//TRIM(VRFTU(VRFTUNum)%Name)//'"')
            CALL ShowContinueError('...This terminal unit contains a heating coil and rated heating capacity is also '// &
                                 'required in the associated condenser object.')
            CALL ShowContinueError('...Rated Heating Capacity must also be '// &
                                 'specified for condenser = '// &
                                  TRIM(cVRFTypes(VRF(VRFTU(VRFTUNum)%VRFSysNum)%VRFSystemTypeNum))//' "'// &
                                  TRIM(VRF(VRFTU(VRFTUNum)%VRFSysNum)%Name)//'".')
            ErrorsFound=.TRUE.
          END IF

          IF(VRF(VRFTU(VRFTUNum)%VRFSysNum)%HeatCapFT .EQ. 0)THEN
            CALL ShowSevereError(TRIM(cCurrentModuleObject)//' "'//TRIM(VRFTU(VRFTUNum)%Name)//'"')
            CALL ShowContinueError('...This terminal unit contains a heating coil and heating performance curves are also '// &
                                 'required in the associated condenser object.')
            CALL ShowContinueError('...Heating Capacity Ratio Modifier Function of Low Temperature Curve must also be '// &
                                 'specified for condenser = '// &
                                  TRIM(cVRFTypes(VRF(VRFTU(VRFTUNum)%VRFSysNum)%VRFSystemTypeNum))//' "'// &
                                  TRIM(VRF(VRFTU(VRFTUNum)%VRFSysNum)%Name)//'".')
            ErrorsFound=.TRUE.
          END IF

          IF(VRF(VRFTU(VRFTUNum)%VRFSysNum)%HeatEIRFT .EQ. 0)THEN
            CALL ShowSevereError(TRIM(cCurrentModuleObject)//' "'//TRIM(VRFTU(VRFTUNum)%Name)//'"')
            CALL ShowContinueError('...This terminal unit contains a heating coil and heating performance curves are also '// &
                                 'required in the associated condenser object.')
            CALL ShowContinueError('...Heating Energy Input Ratio Modifier Function of Low Temperature Curve must also be '// &
                                 'specified for condenser = '// &
                                  TRIM(cVRFTypes(VRF(VRFTU(VRFTUNum)%VRFSysNum)%VRFSystemTypeNum))//' "'// &
                                  TRIM(VRF(VRFTU(VRFTUNum)%VRFSysNum)%Name)//'".')
            ErrorsFound=.TRUE.
          END IF

          IF(VRF(VRFTU(VRFTUNum)%VRFSysNum)%HeatEIRFPLR1 .EQ. 0)THEN
            CALL ShowSevereError(TRIM(cCurrentModuleObject)//' "'//TRIM(VRFTU(VRFTUNum)%Name)//'"')
            CALL ShowContinueError('...This terminal unit contains a heating coil and heating performance curves are also '// &
                                 'required in the associated condenser object.')
            CALL ShowContinueError('...Heating Energy Input Ratio Modifier Function of Low Part-Load Ratio Curve must also '// &
                                 'be specified for condenser = '// &
                                  TRIM(cVRFTypes(VRF(VRFTU(VRFTUNum)%VRFSysNum)%VRFSystemTypeNum))//' "'// &
                                  TRIM(VRF(VRFTU(VRFTUNum)%VRFSysNum)%Name)//'".')
          END IF

        END IF

      END IF

      ! Set up component set for OA mixer - use OA node and Mixed air node
      IF(VRFTU(VRFTUNum)%OAMixerUsed)CALL SetUpCompSets(TRIM(cCurrentModuleObject), VRFTU(VRFTUNum)%Name, &
                       'UNDEFINED',VRFTU(VRFTUNum)%OAMixerName,NodeID(OANodeNums(1)),NodeID(OANodeNums(4)))

      ! TU inlet node must be the same as a zone exhaust node and the OA Mixer return node
      ! check that TU inlet node is a zone exhaust node.
      ZoneNodeNotFound = .TRUE.
      DO CtrlZone = 1,NumOfZones
        IF (.not. ZoneEquipConfig(CtrlZone)%IsControlled) CYCLE
        DO NodeNum = 1,ZoneEquipConfig(CtrlZone)%NumExhaustNodes
          IF (VRFTU(VRFTUNum)%VRFTUInletNodeNum .EQ. ZoneEquipConfig(CtrlZone)%ExhaustNode(NodeNum)) THEN
            ZoneNodeNotFound = .FALSE.
            EXIT
          END IF
        END DO
      END DO
      IF(ZoneNodeNotFound)THEN
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//' "'//TRIM(VRFTU(VRFTUNum)%Name)//&
                   '" Zone terminal unit air inlet node name must be the same as a zone exhaust node name.')
        CALL ShowContinueError('... Zone exhaust node name is specified in ZoneHVAC:EquipmentConnections object.')
        CALL ShowContinueError('... Zone terminal unit inlet node name = '//TRIM(NodeID(VRFTU(VRFTUNum)%VRFTUInletNodeNum)))
        ErrorsFound=.TRUE.
      END IF
      ! check OA Mixer return node
      IF(VRFTU(VRFTUNum)%OAMixerUsed)THEN
        IF(VRFTU(VRFTUNum)%VRFTUInletNodeNum /= OANodeNums(3))THEN
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//' "'//TRIM(VRFTU(VRFTUNum)%Name)//&
              '" Zone terminal unit air inlet node name must be the same as the OutdoorAir:Mixer return air node name.')
          CALL ShowContinueError('... Zone terminal unit air inlet node name = '//TRIM(NodeID(VRFTU(VRFTUNum)%VRFTUInletNodeNum)))
          CALL ShowContinueError('... OutdoorAir:Mixer return air node name = '//TRIM(NodeID(OANodeNums(3))))
          ErrorsFound=.TRUE.
        END IF
      END IF
      ! check that TU outlet node is a zone inlet node.
      ZoneNodeNotFound = .TRUE.
      DO CtrlZone = 1,NumOfZones
        IF (.not. ZoneEquipConfig(CtrlZone)%IsControlled) CYCLE
        DO NodeNum = 1,ZoneEquipConfig(CtrlZone)%NumInletNodes
          IF (VRFTU(VRFTUNum)%VRFTUOutletNodeNum .EQ. ZoneEquipConfig(CtrlZone)%InletNode(NodeNum)) THEN
            ZoneNodeNotFound = .FALSE.
            EXIT
          END IF
        END DO
      END DO
      IF(ZoneNodeNotFound)THEN
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//' "'//TRIM(VRFTU(VRFTUNum)%Name)//&
                           '" Zone terminal unit air outlet node name must be the same as a zone inlet node name.')
        CALL ShowContinueError('... Zone inlet node name is specified in ZoneHVAC:EquipmentConnections object.')
        CALL ShowContinueError('... Zone terminal unit outlet node name = '//TRIM(NodeID(VRFTU(VRFTUNum)%VRFTUOutletNodeNum)))
        ErrorsFound=.TRUE.
      END IF

      ! check fan inlet and outlet nodes
      IF(VRFTU(VRFTUNum)%FanPlace == BlowThru)THEN
        IF(VRFTU(VRFTUNum)%OAMixerUsed)THEN
          IF (FanInletNodeNum /= OANodeNums(4)) THEN
            CALL ShowSevereError(TRIM(cCurrentModuleObject)//' "'//TRIM(VRFTU(VRFTUNum)%Name)//&
                             '" Fan inlet node name must be the same')
            CALL ShowContinueError('as the outside air mixers mixed air node name when blow through '// &
                                'fan is specified and an outside air mixer is present.')
            CALL ShowContinueError('... Fan inlet node = '//TRIM(NodeID(FanInletNodeNum)))
            CALL ShowContinueError('... OA mixers mixed air node = '//TRIM(NodeID(OANodeNums(4))))
            ErrorsFound=.TRUE.
          END IF
        ELSE
          IF (FanInletNodeNum /= VRFTU(VRFTUNum)%VRFTUInletNodeNum) THEN
            CALL ShowSevereError(TRIM(cCurrentModuleObject)//' "'//TRIM(VRFTU(VRFTUNum)%Name)//&
                             '" Fan inlet node name must be the same')
            CALL ShowContinueError('as the terminal unit air inlet node name when blow through '// &
                                'fan is specified and an outside air mixer is not present.')
            CALL ShowContinueError('... Fan inlet node = '//TRIM(NodeID(FanInletNodeNum)))
            CALL ShowContinueError('... Terminal unit air inlet node = '//TRIM(NodeID(VRFTU(VRFTUNum)%VRFTUInletNodeNum)))
            ErrorsFound=.TRUE.
          END IF
        END IF
        IF(VRFTU(VRFTUNum)%CoolingCoilPresent)THEN
          IF (FanOutletNodeNum /= CCoilInletNodeNum) THEN
            CALL ShowSevereError(TRIM(cCurrentModuleObject)//' "'//TRIM(VRFTU(VRFTUNum)%Name)//&
                             '" Fan outlet node name must be the same')
            CALL ShowContinueError('as the DX cooling coil air inlet node name when blow through '// &
                                'fan is specified.')
            CALL ShowContinueError('... Fan outlet node = '//TRIM(NodeID(FanOutletNodeNum)))
            CALL ShowContinueError('... DX cooling coil air inlet node = '//TRIM(NodeID(CCoilInletNodeNum)))
            ErrorsFound=.TRUE.
          END IF
          IF(VRFTU(VRFTUNum)%HeatingCoilPresent)THEN
            IF(HCoilOutletNodeNum /= VRFTU(VRFTUNum)%VRFTUOutletNodeNum)THEN
              CALL ShowSevereError(TRIM(cCurrentModuleObject)//' "'//TRIM(VRFTU(VRFTUNum)%Name)//&
                             '" Heating coil outlet node name must be the same')
              CALL ShowContinueError('as the terminal unit air outlet node name when blow through '// &
                                'fan is specified.')
              CALL ShowContinueError('... Heating coil outlet node      = '//TRIM(NodeID(HCoilOutletNodeNum)))
              CALL ShowContinueError('... Terminal Unit air outlet node = '//TRIM(NodeID(VRFTU(VRFTUNum)%VRFTUOutletNodeNum)))
              ErrorsFound=.TRUE.
            END IF
          ELSE
            IF(CCoilOutletNodeNum /= VRFTU(VRFTUNum)%VRFTUOutletNodeNum)THEN
              CALL ShowSevereError(TRIM(cCurrentModuleObject)//' "'//TRIM(VRFTU(VRFTUNum)%Name)//&
                             '" Cooling coil outlet node name must be the same')
              CALL ShowContinueError('as the terminal unit air outlet node name when blow through '// &
                                'fan is specified and no DX heating coil is present.')
              CALL ShowContinueError('... Cooling coil outlet node      = '//TRIM(NodeID(CCoilOutletNodeNum)))
              CALL ShowContinueError('... Terminal Unit air outlet node = '//TRIM(NodeID(VRFTU(VRFTUNum)%VRFTUOutletNodeNum)))
              ErrorsFound=.TRUE.
            END IF
          END IF
        ELSEIF(VRFTU(VRFTUNum)%HeatingCoilPresent)THEN
          IF (FanOutletNodeNum /= HCoilInletNodeNum) THEN
            CALL ShowSevereError(TRIM(cCurrentModuleObject)//' "'//TRIM(VRFTU(VRFTUNum)%Name)//&
                             '" Fan outlet node name must be the same')
            CALL ShowContinueError('as the DX heating coil air inlet node name when blow through '// &
                                'fan is specified and a DX cooling coil is not present.')
            CALL ShowContinueError('... Fan outlet node = '//TRIM(NodeID(FanOutletNodeNum)))
            CALL ShowContinueError('... DX heating coil air inlet node = '//TRIM(NodeID(HCoilInletNodeNum)))
            ErrorsFound=.TRUE.
          END IF
          IF(HCoilOutletNodeNum /= VRFTU(VRFTUNum)%VRFTUOutletNodeNum)THEN
            CALL ShowSevereError(TRIM(cCurrentModuleObject)//' "'//TRIM(VRFTU(VRFTUNum)%Name)//&
                             '" Heating coil outlet node name must be the same')
            CALL ShowContinueError('as the terminal unit air outlet node name when blow through '// &
                                'fan is specified.')
            CALL ShowContinueError('... Heating coil outlet node      = '//TRIM(NodeID(HCoilOutletNodeNum)))
            CALL ShowContinueError('... Terminal Unit air outlet node = '//TRIM(NodeID(VRFTU(VRFTUNum)%VRFTUOutletNodeNum)))
            ErrorsFound=.TRUE.
          END IF
        END IF
      ELSEIF(VRFTU(VRFTUNum)%FanPlace == DrawThru)THEN
        IF(VRFTU(VRFTUNum)%CoolingCoilPresent)THEN
          IF(.NOT. VRFTU(VRFTUNum)%OAMixerUsed)THEN
            IF(VRFTU(VRFTUNum)%VRFTUInletNodeNum /= CCoilInletNodeNum)THEN
              CALL ShowSevereError(TRIM(cCurrentModuleObject)//' "'//TRIM(VRFTU(VRFTUNum)%Name)//&
                             '" Cooling coil inlet node name must be the same')
              CALL ShowContinueError('as the terminal unit air inlet node name when draw through '// &
                                'fan is specified.')
              CALL ShowContinueError('... Terminal unit air inlet node = '//TRIM(NodeID(VRFTU(VRFTUNum)%VRFTUInletNodeNum)))
              CALL ShowContinueError('... DX cooling coil air inlet node = '//TRIM(NodeID(CCoilInletNodeNum)))
              ErrorsFound=.TRUE.
            END IF
          END IF
          IF(VRFTU(VRFTUNum)%HeatingCoilPresent)THEN
            IF (FanInletNodeNum /= HCoilOutletNodeNum) THEN
              CALL ShowSevereError(TRIM(cCurrentModuleObject)//' "'//TRIM(VRFTU(VRFTUNum)%Name)//&
                             '" Fan inlet node name must be the same')
              CALL ShowContinueError('as the DX heating coil air outlet node name when draw through '// &
                                'fan is specifiedt.')
              CALL ShowContinueError('... Fan inlet node = '//TRIM(NodeID(FanInletNodeNum)))
              CALL ShowContinueError('... DX heating coil air outlet node = '//TRIM(NodeID(HCoilOutletNodeNum)))
              ErrorsFound=.TRUE.
            END IF
          ELSE
            IF (FanInletNodeNum /= CCoilOutletNodeNum) THEN
              CALL ShowSevereError(TRIM(cCurrentModuleObject)//' "'//TRIM(VRFTU(VRFTUNum)%Name)//&
                             '" Fan inlet node name must be the same')
              CALL ShowContinueError('as the DX cooling coil air outlet node name when draw through '// &
                                'fan is specified and a DX heating coil is not present.')
              CALL ShowContinueError('... Fan inlet node = '//TRIM(NodeID(FanInletNodeNum)))
              CALL ShowContinueError('... DX cooling coil air outlet node = '//TRIM(NodeID(CCoilOutletNodeNum)))
              ErrorsFound=.TRUE.
            END IF
          END IF
        ELSEIF(VRFTU(VRFTUNum)%HeatingCoilPresent)THEN
          IF (FanInletNodeNum /= HCoilOutletNodeNum) THEN
            CALL ShowSevereError(TRIM(cCurrentModuleObject)//' "'//TRIM(VRFTU(VRFTUNum)%Name)//&
                             '" Fan inlet node name must be the same')
            CALL ShowContinueError('as the DX heating coil air outlet node name when draw through '// &
                                'fan is specified.')
            CALL ShowContinueError('... Fan inlet node = '//TRIM(NodeID(FanInletNodeNum)))
            CALL ShowContinueError('... DX heating coil air outlet node = '//TRIM(NodeID(HCoilOutletNodeNum)))
            ErrorsFound=.TRUE.
          END IF
        END IF
        IF (FanOutletNodeNum /= VRFTU(VRFTUNum)%VRFTUOutletNodeNum) THEN
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//' "'//TRIM(VRFTU(VRFTUNum)%Name)//&
                             '" Fan outlet node name must be the same')
          CALL ShowContinueError('as the terminal unit air outlet node name when draw through '// &
                                'fan is specified.')
          CALL ShowContinueError('... Fan outlet node = '//TRIM(NodeID(FanOutletNodeNum)))
          CALL ShowContinueError('... Terminal unit air outlet node = '//TRIM(NodeID(VRFTU(VRFTUNum)%VRFTUOutletNodeNum)))
          ErrorsFound=.TRUE.
        END IF
      END IF
      IF(VRFTU(VRFTUNum)%CoolingCoilPresent .AND. VRFTU(VRFTUNum)%HeatingCoilPresent)THEN
        IF (CCoilOutletNodeNum /= HCoilInletNodeNum) THEN
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//' "'//TRIM(VRFTU(VRFTUNum)%Name)//&
                             '" DX cooling coil air outlet node name must be the same')
          CALL ShowContinueError(' as the DX heating coil air inlet node name.')
          CALL ShowContinueError('... DX cooling coil air outlet node = '//TRIM(NodeID(CCoilOutletNodeNum)))
          CALL ShowContinueError('... DX heating coil air inlet node  = '//TRIM(NodeID(HCoilInletNodeNum)))
          ErrorsFound=.TRUE.
        END IF
      END IF

    END DO   ! end Number of VRF Terminal Unit Loop

!   perform additional error checking
    DO NumList = 1, NumVRFTULists
      DO VRFNum = 1, TerminalUnitList(NumList)%NumTUInList
        IF(TerminalUnitList(NumList)%ZoneTUPtr(VRFNum) .GT. 0)CYCLE
          ! TU name in zone terminal unit list not found
          CALL ShowSevereError('ZoneTerminalUnitList "'//TRIM(TerminalUnitList(NumList)%Name)//'"')
          CALL ShowContinueError('...Zone Terminal Unit = '//TRIM(TerminalUnitList(NumList)%ZoneTUName(VRFNum))// &
                                 ' not found.')
          ErrorsFound = .TRUE.
      END DO
    END DO

    DO VRFNum = 1,  NumVRFTU
      IF(VRFTU(VRFNum)%CoolingCoilPresent)THEN
        CALL SetupOutputVariable('Zone Terminal Unit Cooling Electric Consumption Rate[W]', &
                                  VRFTU(VRFNum)%ParasiticCoolElecPower,'System','Average', VRFTU(VRFNum)%Name)
        CALL SetupOutputVariable('Zone Terminal Unit Cooling Electric Consumption[J]', &
                                  VRFTU(VRFNum)%ParasiticElecCoolConsumption, 'System','Sum', VRFTU(VRFNum)%Name, &
                                  ResourceTypeKey='Electric',EndUseKey='COOLING',GroupKey='System')
        CALL SetupOutputVariable('Zone Terminal Unit Total Cooling Rate[W]', &
                                  VRFTU(VRFNum)%TotalCoolingRate,'System','Average', VRFTU(VRFNum)%Name)
        CALL SetupOutputVariable('Zone Terminal Unit Sensible Cooling Rate[W]', &
                                  VRFTU(VRFNum)%SensibleCoolingRate,'System','Average', VRFTU(VRFNum)%Name)
        CALL SetupOutputVariable('Zone Terminal Unit Latent Cooling Rate[W]', &
                                  VRFTU(VRFNum)%LatentCoolingRate,'System','Average', VRFTU(VRFNum)%Name)
        CALL SetupOutputVariable('Zone Terminal Unit Total Cooling Energy[J]', &
                                  VRFTU(VRFNum)%TotalCoolingEnergy, 'System','Sum', VRFTU(VRFNum)%Name, &
                                 ResourceTypeKey='ENERGYTRANSFER',EndUseKey='COOLINGCOILS',GroupKey='System')
        CALL SetupOutputVariable('Zone Terminal Unit Sensible Cooling Energy[J]', &
                                  VRFTU(VRFNum)%SensibleCoolingEnergy, 'System','Sum', VRFTU(VRFNum)%Name)
        CALL SetupOutputVariable('Zone Terminal Unit Latent Cooling Energy[J]', &
                                  VRFTU(VRFNum)%LatentCoolingEnergy, 'System','Sum', VRFTU(VRFNum)%Name)
      END IF
      IF(VRFTU(VRFNum)%HeatingCoilPresent)THEN
        CALL SetupOutputVariable('Zone Terminal Unit Heating Electric Consumption Rate[W]', &
                                  VRFTU(VRFNum)%ParasiticHeatElecPower,'System','Average', VRFTU(VRFNum)%Name)
        CALL SetupOutputVariable('Zone Terminal Unit Heating Electric Consumption[J]', &
                                  VRFTU(VRFNum)%ParasiticElecHeatConsumption, 'System','Sum', VRFTU(VRFNum)%Name, &
                                  ResourceTypeKey='Electric',EndUseKey='HEATING',GroupKey='System')
        CALL SetupOutputVariable('Zone Terminal Unit Total Heating Rate[W]', &
                                  VRFTU(VRFNum)%TotalHeatingRate,'System','Average', VRFTU(VRFNum)%Name)
        CALL SetupOutputVariable('Zone Terminal Unit Sensible Heating Rate[W]', &
                                  VRFTU(VRFNum)%SensibleHeatingRate,'System','Average', VRFTU(VRFNum)%Name)
        CALL SetupOutputVariable('Zone Terminal Unit Latent Heating Rate[W]', &
                                  VRFTU(VRFNum)%LatentHeatingRate,'System','Average', VRFTU(VRFNum)%Name)
        CALL SetupOutputVariable('Zone Terminal Unit Total Heating Energy[J]', &
                                  VRFTU(VRFNum)%TotalHeatingEnergy, 'System','Sum', VRFTU(VRFNum)%Name, &
                                  ResourceTypeKey='ENERGYTRANSFER',EndUseKey='HEATINGCOILS',GroupKey='System')
        CALL SetupOutputVariable('Zone Terminal Unit Sensible Heating Energy[J]', &
                                  VRFTU(VRFNum)%SensibleHeatingEnergy, 'System','Sum', VRFTU(VRFNum)%Name)
        CALL SetupOutputVariable('Zone Terminal Unit Latent Heating Energy[J]', &
                                  VRFTU(VRFNum)%LatentHeatingEnergy, 'System','Sum', VRFTU(VRFNum)%Name)
      END IF
      IF (AnyEnergyManagementSystemInModel) THEN
        CALL SetupEMSActuator('Variable Refrigerant Flow Terminal Unit', VRFTU(VRFNum)%Name, 'Part Load Ratio' , '[fraction]', &
                           VRFTU(VRFNum)%EMSOverridePartLoadFrac, VRFTU(VRFNum)%EMSValueForPartLoadFrac )
      ENDIF
    END DO

    DO NumCond = 1, NumVRFCond
      CALL SetupOutputVariable('Variable Refrigerant Flow Heat Pump Total Cooling Capacity[W]', &
                                VRF(NumCond)%TotalCoolingCapacity, 'System','Average', VRF(NumCond)%Name)
      CALL SetupOutputVariable('Variable Refrigerant Flow Heat Pump Total Heating Capacity[W]', &
                                VRF(NumCond)%TotalHeatingCapacity, 'System','Average', VRF(NumCond)%Name)

      CALL SetupOutputVariable('Variable Refrigerant Flow Heat Pump Cooling '// &
                                TRIM(cValidFuelTypes(VRF(NumCond)%FuelType))//' Consumption Rate[W]', &
                                VRF(NumCond)%ElecCoolingPower, 'System','Average', VRF(NumCond)%Name)
      CALL SetupOutputVariable('Variable Refrigerant Flow Heat Pump Cooling '// &
                                TRIM(cValidFuelTypes(VRF(NumCond)%FuelType))//' Consumption[J]', &
                                VRF(NumCond)%CoolElecConsumption, 'System','Sum', VRF(NumCond)%Name, &
                                ResourceTypeKey=TRIM(cValidFuelTypes(VRF(NumCond)%FuelType)), &
                                EndUseKey='COOLING',GroupKey='System')
      CALL SetupOutputVariable('Variable Refrigerant Flow Heat Pump Heating '// &
                                TRIM(cValidFuelTypes(VRF(NumCond)%FuelType))//' Consumption Rate[W]', &
                                VRF(NumCond)%ElecHeatingPower, 'System','Average', VRF(NumCond)%Name)
      CALL SetupOutputVariable('Variable Refrigerant Flow Heat Pump Heating '// &
                                TRIM(cValidFuelTypes(VRF(NumCond)%FuelType))//' Consumption[J]', &
                                VRF(NumCond)%HeatElecConsumption, 'System','Sum', VRF(NumCond)%Name, &
                                ResourceTypeKey=TRIM(cValidFuelTypes(VRF(NumCond)%FuelType)), &
                                EndUseKey='HEATING',GroupKey='System')

      CALL SetupOutputVariable('Variable Refrigerant Flow Heat Pump Cooling COP[]', &
                                VRF(NumCond)%OperatingCoolingCOP,'System','Average', VRF(NumCond)%Name)
      CALL SetupOutputVariable('Variable Refrigerant Flow Heat Pump Heating COP[]', &
                                VRF(NumCond)%OperatingHeatingCOP,'System','Average', VRF(NumCond)%Name)

      IF(VRF(NumCond)%DefrostStrategy == Resistive)THEN
        CALL SetupOutputVariable('Variable Refrigerant Flow Heat Pump Electric Defrost Consumption Rate[W]', &
                                VRF(NumCond)%DefrostPower, 'System','Average', VRF(NumCond)%Name)
        CALL SetupOutputVariable('Variable Refrigerant Flow Heat Pump Electric Defrost Consumption[J]', &
                                VRF(NumCond)%DefrostConsumption, 'System','Sum',VRF(NumCond)%Name, &
                                ResourceTypeKey='Electric',EndUseKey='HEATING',GroupKey='System')
      ELSE ! defrost energy appied to fuel type
        CALL SetupOutputVariable('Variable Refrigerant Flow Heat Pump '// &
                                TRIM(cValidFuelTypes(VRF(NumCond)%FuelType))//' Defrost Consumption Rate[W]', &
                                VRF(NumCond)%ElecHeatingPower, 'System','Average', VRF(NumCond)%Name)
        CALL SetupOutputVariable('Variable Refrigerant Flow Heat Pump '// &
                                TRIM(cValidFuelTypes(VRF(NumCond)%FuelType))//' Defrost Consumption[J]', &
                                VRF(NumCond)%HeatElecConsumption, 'System','Sum', VRF(NumCond)%Name, &
                                ResourceTypeKey=TRIM(cValidFuelTypes(VRF(NumCond)%FuelType)), &
                                EndUseKey='HEATING',GroupKey='System')
      END IF

      CALL SetupOutputVariable('Variable Refrigerant Flow Heat Pump Part Load Ratio', &
                                VRF(NumCond)%VRFCondPLR, 'System','Average', VRF(NumCond)%Name)
      CALL SetupOutputVariable('Variable Refrigerant Flow Heat Pump Runtime Fraction', &
                                VRF(NumCond)%VRFCondRTF, 'System','Average', VRF(NumCond)%Name)
      CALL SetupOutputVariable('Variable Refrigerant Flow Heat Pump Cycling Ratio', &
                                VRF(NumCond)%VRFCondCyclingRatio, 'System','Average',VRF(NumCond)%Name)

      CALL SetupOutputVariable('Variable Refrigerant Flow Heat Pump Operating Mode', &
                                VRF(NumCond)%OperatingMode, 'System','Average', VRF(NumCond)%Name)
      CALL SetupOutputVariable('Variable Refrigerant Flow Heat Pump Inlet Temp[C]', &
                                VRF(NumCond)%CondenserInletTemp, 'System','Average', VRF(NumCond)%Name)
      CALL SetupOutputVariable('Variable Refrigerant Flow Heat Pump Maximum Terminal Unit Cooling Capacity[W]', &
                                MaxCoolingCapacity(NumCond), 'System','Average', VRF(NumCond)%Name)
      CALL SetupOutputVariable('Variable Refrigerant Flow Heat Pump Maximum Terminal Unit Heating Capacity[W]', &
                                MaxHeatingCapacity(NumCond), 'System','Average', VRF(NumCond)%Name)

      CALL SetupOutputVariable('Variable Refrigerant Flow Heat Pump CrankCase Heater Electric Power[W]', &
                                VRF(NumCond)%CrankCaseHeaterPower, 'System','Average', VRF(NumCond)%Name)
      CALL SetupOutputVariable('Variable Refrigerant Flow Heat Pump CrankCase Heater Electric Consumption[J]', &
                                VRF(NumCond)%CrankCaseHeaterElecConsumption, 'System','Sum', VRF(NumCond)%Name, &
                                ResourceTypeKey='Electric',EndUseKey='COOLING',GroupKey='System')
      IF(VRF(NumCond)%CondenserType .EQ. EvapCooled)THEN
        CALL SetupOutputVariable('Variable Refrigerant Flow Heat Pump Evap Condenser Water Consumption[m3]', &
                                  VRF(NumCond)%EvapWaterConsumpRate, 'System','Sum',VRF(NumCond)%Name, &
                                  ResourceTypeKey='Water',EndUseKey='Cooling',GroupKey='System')
        CALL SetupOutputVariable('Variable Refrigerant Flow Heat Pump Evap Condenser Pump Electric Power[W]', &
                                  VRF(NumCond)%EvapCondPumpElecPower, 'System','Average',VRF(NumCond)%Name)
        CALL SetupOutputVariable('Variable Refrigerant Flow Heat Pump Evap Condenser Pump Electric Consumption[J]', &
                                  VRF(NumCond)%EvapCondPumpElecConsumption,'System','Sum',VRF(NumCond)%Name, &
                                  ResourceTypeKey='Electric',EndUseKey='COOLING',GroupKey='System')

        IF(VRF(NumCond)%BasinHeaterPowerFTempDiff .GT. 0.0d0)THEN
          CALL SetupOutputVariable('Variable Refrigerant Flow Heat Pump Basin Heater Electric Power [W]', &
                                    VRF(NumCond)%BasinHeaterPower,'System','Average',VRF(NumCond)%Name)
          CALL SetupOutputVariable('Variable Refrigerant Flow Heat Pump Basin Heater Electric Consumption [J]', &
                                    VRF(NumCond)%BasinHeaterConsumption,'System','Sum',VRF(NumCond)%Name, &
                                    ResourceTypeKey='Electric',EndUseKey='COOLING',GroupKey='System')
        END IF

      END IF

      IF (AnyEnergyManagementSystemInModel) THEN
        CALL SetupEMSActuator('Variable Refrigerant Flow Heat Pump', VRF(NumCond)%Name, 'Operating Mode' , '[integer]', &
                           VRF(NumCond)%EMSOverrideHPOperatingMode, VRF(NumCond)%EMSValueForHPOperatingMode )
      ENDIF

    END DO

    IF (ErrorsFound) THEN
      CALL ShowFatalError(RoutineName//'Errors found in getting AirConditioner:VariableRefrigerantFlow system input. '//&
                      'Preceding condition(s) causes termination.')
    END IF

  RETURN

END SUBROUTINE GetVRFInput

! End of Get Input subroutines for the Module
!******************************************************************************



! Beginning Initialization Section of the Module
!******************************************************************************

SUBROUTINE InitVRF(VRFTUNum, FirstHVACIteration, OnOffAirFlowRatio, QZnReq)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Raustad, FSEC
          !       DATE WRITTEN   August 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is for initializations of the VRF Components.

          ! METHODOLOGY EMPLOYED:
          ! Uses the status flags to trigger initializations.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataZoneEquipment, ONLY: ZoneEquipInputsFilled,CheckZoneEquipmentList
  USE DataHeatBalFanSys, ONLY: TempControlType, ZT, ZoneThermostatSetPointHi, ZoneThermostatSetPointLo
  USE InputProcessor,    ONLY: SameString
  USE ScheduleManager,   ONLY: GetCurrentScheduleValue
  USE DataHVACGlobals,   ONLY: TimeStepSys, SysTimeElapsed
  USE DataEnvironment,   ONLY: StdBaroPress, StdRhoAir, OutDryBulbTemp, OutWetBulbTemp
  USE MixedAir,          ONLY: SimOAMixer, SimOAController
  USE DataZoneEquipment, ONLY: ZoneEquipList
  USE DataGlobals,       ONLY: BeginTimeStepFlag, CurrentTime
  USE DataSizing,        ONLY: AutoSize
  USE Fans,              ONLY: GetFanVolFlow
  USE General,           ONLY: TrimSigDigits, RoundSigDigits


  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  Integer, Intent(IN):: VRFTUNum
  LOGICAL, INTENT(IN):: FirstHVACIteration
  REAL(r64), INTENT(InOut) :: OnOffAirFlowRatio
  REAL(r64), INTENT(Out)   :: QZnReq

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER             :: InNode          ! TU inlet node
  INTEGER             :: OutNode         ! TU outlet node
  INTEGER             :: OutsideAirNode  ! TU mixer outside air inlet node
  LOGICAL, SAVE       :: MyOneTimeFlag = .true.                ! False after allocating and initializing subroutine variables
  LOGICAL, SAVE       :: ZoneEquipmentListNotChecked = .TRUE.  ! False after the Zone Equipment List has been checked for items
  LOGICAL, ALLOCATABLE,SAVE, DIMENSION(:) :: MyEnvrnFlag       ! Flag for initializing at beginning of each new environment
  LOGICAL, ALLOCATABLE,SAVE, DIMENSION(:) :: MySizeFlag        ! False after TU has been sized
  LOGICAL, ALLOCATABLE,SAVE, DIMENSION(:) :: MyBeginTimeStepFlag ! Flag to sense beginning of time step
  LOGICAL, ALLOCATABLE,SAVE, DIMENSION(:) :: MyVRFFlag         ! used for sizing VRF inputs one time
  INTEGER,   ALLOCATABLE,SAVE, DIMENSION(:) :: NumCoolingLoads ! number of TU's requesting cooling
  INTEGER,   ALLOCATABLE,SAVE, DIMENSION(:) :: NumHeatingLoads ! number of TU's requesting heating
  REAL(r64), ALLOCATABLE,SAVE, DIMENSION(:) :: MaxDeltaT       ! maximum zone temperature difference from setpoint
  REAL(r64), ALLOCATABLE,SAVE, DIMENSION(:) :: MinDeltaT       ! minimum zone temperature difference from setpoint
  REAL(r64), ALLOCATABLE,SAVE, DIMENSION(:) :: SumCoolingLoads ! sum of cooling loads
  REAL(r64), ALLOCATABLE,SAVE, DIMENSION(:) :: SumHeatingLoads ! sum of heating loads
  INTEGER   :: NumTULoop        ! loop counter, number of TU's in list
  INTEGER   :: ELLoop           ! loop counter, number of zone equipment lists
  INTEGER   :: ListLoop         ! loop counter, number of equipment is each list
  INTEGER   :: NumTU            ! loop counter, number of TU's in list
  INTEGER   :: VRFCond          ! index to VRF condenser
  INTEGER   :: TUIndex          ! index to TU
  INTEGER   :: TUListNum        ! index to VRF AC system terminal unit list
  INTEGER   :: ThisZoneNum      ! index to zone number where TU is located
  INTEGER   :: FanOpMode        ! TU fan operating mode
  REAL(r64) :: ZoneDeltaT       ! zone temperature difference from setpoint
  REAL(r64) :: RhoAir           ! air density at InNode
  REAL(r64) :: ZoneLoad         ! current zone load (W)
  REAL(r64) :: SPTempHi         ! thermostat setpoint high
  REAL(r64) :: SPTempLo         ! thermostat setpoint low
  REAL(r64), SAVE :: CurrentEndTime     ! end time of current time step
  REAL(r64), SAVE :: CurrentEndTimeLast ! end time of last time step
  REAL(r64), SAVE :: TimeStepSysLast    ! system time step on last time step
  REAL(r64) :: TempOutput       ! Sensible output of TU
  REAL(r64) :: LoadToCoolingSP  ! thermostat load to cooling setpoint (W)
  REAL(r64) :: LoadToHeatingSP  ! thermostat load to heating setpoint (W)


          ! FLOW:

  ! ALLOCATE and Initialize subroutine variables
  IF (MyOneTimeFlag) THEN

    ALLOCATE(MyEnvrnFlag(NumVRFTU))
    ALLOCATE(MySizeFlag(NumVRFTU))
    ALLOCATE(MyVRFFlag(NumVRFTU))
    ALLOCATE(MyBeginTimeStepFlag(NumVRFCond))
    ALLOCATE(MaxDeltaT(NumVRFCond))
    ALLOCATE(MinDeltaT(NumVRFCond))
    ALLOCATE(LastModeCooling(NumVRFCond))
    ALLOCATE(LastModeHeating(NumVRFCond))
    ALLOCATE(HeatingLoad(NumVRFCond))
    ALLOCATE(CoolingLoad(NumVRFCond))
    ALLOCATE(NumCoolingLoads(NumVRFCond))
    ALLOCATE(SumCoolingLoads(NumVRFCond))
    ALLOCATE(NumHeatingLoads(NumVRFCond))
    ALLOCATE(SumHeatingLoads(NumVRFCond))
    MyEnvrnFlag = .TRUE.
    MySizeFlag = .TRUE.
    MyVRFFlag = .TRUE.
    MyBeginTimeStepFlag = .TRUE.
    MaxDeltaT  = 0.d0
    MinDeltaT  = 0.d0
    LastModeCooling = .FALSE.
    LastModeHeating = .TRUE.
    NumCoolingLoads = 0
    SumCoolingLoads = 0.d0
    NumHeatingLoads = 0
    SumHeatingLoads = 0.d0

    MyOneTimeFlag = .false.

  END IF ! IF (MyOneTimeFlag) THEN

  ! identify VRF condenser connected to this TU
  VRFCond = VRFTU(VRFTUNum)%VRFSysNum
  InNode  = VRFTU(VRFTUNum)%VRFTUInletNodeNum
  OutNode = VRFTU(VRFTUNum)%VRFTUOutletNodeNum
  OutsideAirNode = VRFTU(VRFTUNum)%VRFTUOAMixerOANodeNum

  ! If all VRF Terminal Units on this VRF AC System have been simulated, reset the IsSimulated flag
  ! The condenser will be simulated after all terminal units have been simulated (see Sub SimulateVRF)
  IF(ALL(TerminalUnitList(VRFTU(VRFTUNum)%TUListNum)%IsSimulated))THEN
!   this should be the first time through on the next iteration. All TU's and condenser have been simulated.
!   reset simulation flag for each terminal unit
    TerminalUnitList(VRFTU(VRFTUNum)%TUListNum)%IsSimulated = .FALSE.
!     after all TU's have been simulated, reset operating mode flag if necessary
      IF(LastModeHeating(VRFCond) .AND. CoolingLoad(VRFCond))THEN
        LastModeCooling(VRFCond) = .TRUE.
        LastModeHeating(VRFCond) = .FALSE.
      END IF
      IF(LastModeCooling(VRFCond) .AND. HeatingLoad(VRFCond))THEN
        LastModeHeating(VRFCond) = .TRUE.
        LastModeCooling(VRFCond) = .FALSE.
      END IF
  END IF ! IF(ALL(TerminalUnitList(VRFTU(VRFTUNum)%TUListNum)%IsSimulated))THEN

  ! one-time check to see if VRF TU's are on Zone Equipment List or issue warning
  IF(ZoneEquipmentListNotChecked)THEN
    IF(ZoneEquipInputsFilled)THEN
      ZoneEquipmentListNotChecked=.FALSE.
      DO TUListNum = 1, NumVRFTULists
        DO NumTULoop=1,TerminalUnitList(TUListNum)%NumTUInList
          TUIndex = TerminalUnitList(TUListNum)%ZoneTUPtr(NumTULoop)
          EquipList: DO ELLoop=1,NumOfZones  ! NumofZoneEquipLists
            IF (ZoneEquipList(ELLoop)%Name == ' ') CYCLE    ! dimensioned by NumOfZones.  Only valid ones have names.
            DO ListLoop=1,ZoneEquipList(ELLoop)%NumOfEquipTypes
              IF (.NOT. SameString(ZoneEquipList(ELLoop)%EquipType(ListLoop),cVRFTUTypes(VRFTU(TUIndex)%VRFTUType_Num)))CYCLE
              IF (.NOT. SameString(ZoneEquipList(ELLoop)%EquipName(ListLoop), VRFTU(TUIndex)%Name)) CYCLE
              VRFTU(TUIndex)%ZoneNum = ELLoop
              IF(VRF(VRFTU(TUIndex)%VRFSysNum)%MasterZonePTR == ELLoop)THEN
                VRF(VRFTU(TUIndex)%VRFSysNum)%MasterZoneTUIndex = TUIndex
              END IF
              EXIT EquipList
            ENDDO
          ENDDO EquipList
        ENDDO

        IF (CheckZoneEquipmentList(cVRFTUTypes(VRFTU(TUIndex)%VRFTUType_Num),VRFTU(TUIndex)%Name)) CYCLE
        CALL ShowSevereError('InitVRF: VRF Terminal Unit = [' &
           //TRIM(cVRFTUTypes(VRFTU(TUIndex)%VRFTUType_Num))//','//TRIM(VRFTU(TUIndex)%Name)//  &
           '] is not on any ZoneHVAC:EquipmentList.  It will not be simulated.')
        CALL ShowContinueError('...The VRF AC System associated with this terminal unit may also not be simulated.')
    ENDDO
    END IF ! IF(ZoneEquipInputsFilled) THEN
  ENDIF ! IF(ZoneEquipmentListNotChecked)THEN

  ! Size TU
  IF (MySizeFlag(VRFTUNum)) THEN
    IF ( .NOT. SysSizingCalc) THEN
      CALL SizeVRF(VRFTUNum)
      TerminalUnitList(VRFCond)%TerminalUnitNotSizedYet(VRFTU(VRFTUNum)%NumTUInList) = .FALSE.
      MySizeFlag(VRFTUNum) = .FALSE.
    END IF ! IF ( .NOT. SysSizingCalc) THEN
  END IF ! IF (MySizeFlag(VRFTUNum)) THEN


! Do the Begin Environment initializations
  IF (BeginEnvrnFlag .and. MyEnvrnFlag(VRFTUNum)) THEN

    !Change the Volume Flow Rates to Mass Flow Rates

    RhoAir = StdRhoAir
    ! set the mass flow rates from the input volume flow rates
    VRFTU(VRFTUNum)%MaxCoolAirMassFlow = RhoAir*VRFTU(VRFTUNum)%MaxCoolAirVolFlow
    VRFTU(VRFTUNum)%CoolOutAirMassFlow = RhoAir*VRFTU(VRFTUNum)%CoolOutAirVolFlow
    VRFTU(VRFTUNum)%MaxHeatAirMassFlow = RhoAir*VRFTU(VRFTUNum)%MaxHeatAirVolFlow
    VRFTU(VRFTUNum)%HeatOutAirMassFlow = RhoAir*VRFTU(VRFTUNum)%HeatOutAirVolFlow
    VRFTU(VRFTUNum)%MaxNoCoolAirMassFlow = RhoAir*VRFTU(VRFTUNum)%MaxNoCoolAirVolFlow
    VRFTU(VRFTUNum)%MaxNoHeatAirMassFlow = RhoAir*VRFTU(VRFTUNum)%MaxNoHeatAirVolFlow
    VRFTU(VRFTUNum)%NoCoolHeatOutAirMassFlow = RhoAir*VRFTU(VRFTUNum)%NoCoolHeatOutAirVolFlow
    ! set the node max and min mass flow rates
    ! outside air mixer is optional, check that node num > 0
    IF(OutsideAirNode .GT. 0)THEN
      Node(OutsideAirNode)%MassFlowRateMax = MAX(VRFTU(VRFTUNum)%CoolOutAirMassFlow,VRFTU(VRFTUNum)%HeatOutAirMassFlow)
      Node(OutsideAirNode)%MassFlowRateMin = 0.0
      Node(OutsideAirNode)%MassFlowRateMinAvail = 0.0
    END IF
    Node(OutNode)%MassFlowRateMax = MAX(VRFTU(VRFTUNum)%MaxCoolAirMassFlow,VRFTU(VRFTUNum)%MaxHeatAirMassFlow)
    Node(OutNode)%MassFlowRateMin = 0.0
    Node(OutNode)%MassFlowRateMinAvail = 0.0
    Node(InNode)%MassFlowRateMax = MAX(VRFTU(VRFTUNum)%MaxCoolAirMassFlow,VRFTU(VRFTUNum)%MaxHeatAirMassFlow)
    Node(InNode)%MassFlowRateMin = 0.0
    Node(InNode)%MassFlowRateMinAvail = 0.0
    IF(VRFTU(VRFTUNum)%VRFTUOAMixerRelNodeNum .GT. 0)THEN
      Node(VRFTU(VRFTUNum)%VRFTUOAMixerRelNodeNum)%MassFlowRateMinAvail = 0.0
    END IF

    MyEnvrnFlag(VRFTUNum) = .FALSE.

  END IF ! IF (BeginEnvrnFlag .and. MyEnvrnFlag(VRFTUNum)) THEN

  ! reset environment flag for next environment
  IF (.not. BeginEnvrnFlag) THEN
    MyEnvrnFlag(VRFTUNum) = .TRUE.
  ENDIF

  ! one-time checks of flow rate vs fan flow rate
  IF(MyVRFFlag(VRFTUNum))THEN
    IF(.NOT. SysSizingCalc)THEN
      IF(VRFTU(VRFTUNum)%ActualFanVolFlowRate /= Autosize)THEN

        IF (VRFTU(VRFTUNum)%MaxCoolAirVolFlow > VRFTU(VRFTUNum)%ActualFanVolFlowRate) THEN
          CALL ShowWarningError('InitVRF: VRF Terminal Unit = [' &
             //TRIM(cVRFTUTypes(VRFTU(VRFTUNum)%VRFTUType_Num))//', "'//TRIM(VRFTU(VRFTUNum)%Name)//'"]')
          CALL ShowContinueError('... has Supply Air Flow Rate During Cooling Operation > Max Fan Volume Flow Rate, should be <=')
          CALL ShowContinueError('... Supply Air Flow Rate During Cooling Operation = '// &
                                 TRIM(RoundSigDigits(VRFTU(VRFTUNum)%MaxCoolAirVolFlow,4))//' m3/s')
          CALL ShowContinueError('... Max Fan Volume Flow Rate                      = '// &
                                 TRIM(RoundSigDigits(VRFTU(VRFTUNum)%ActualFanVolFlowRate,4))//' m3/s')
          CALL ShowContinueError('...the supply air flow rate during cooling operation will be reduced'// &
                                 ' to match and the simulation continues.')
          VRFTU(VRFTUNum)%MaxCoolAirVolFlow = VRFTU(VRFTUNum)%ActualFanVolFlowRate
        ENDIF

        IF (VRFTU(VRFTUNum)%MaxNoCoolAirVolFlow > VRFTU(VRFTUNum)%ActualFanVolFlowRate) THEN
          CALL ShowWarningError('InitVRF: VRF Terminal Unit = [' &
             //TRIM(cVRFTUTypes(VRFTU(VRFTUNum)%VRFTUType_Num))//', "'//TRIM(VRFTU(VRFTUNum)%Name)//'"]')
          CALL ShowContinueError('... has Supply Air Flow Rate When No Cooling is Needed > Max Fan Volume Flow Rate, should be <=')
          CALL ShowContinueError('... Supply Air Flow Rate When No Cooling is Needed = '// &
                                 TRIM(RoundSigDigits(VRFTU(VRFTUNum)%MaxNoCoolAirVolFlow,4))//' m3/s')
          CALL ShowContinueError('... Max Fan Volume Flow Rate                       = '// &
                                 TRIM(RoundSigDigits(VRFTU(VRFTUNum)%ActualFanVolFlowRate,4))//' m3/s')
          CALL ShowContinueError('...the supply air flow rate when no cooling is needed will be reduced'// &
                                 ' to match and the simulation continues.')
          VRFTU(VRFTUNum)%MaxNoCoolAirVolFlow = VRFTU(VRFTUNum)%ActualFanVolFlowRate
        ENDIF

        IF(VRFTU(VRFTUNum)%CoolOutAirVolFlow .GT. VRFTU(VRFTUNum)%MaxCoolAirVolFlow)THEN
          CALL ShowWarningError('InitVRF: VRF Terminal Unit = [' &
             //TRIM(cVRFTUTypes(VRFTU(VRFTUNum)%VRFTUType_Num))//', "'//TRIM(VRFTU(VRFTUNum)%Name)//'"]')
          CALL ShowContinueError('...The Outdoor Air Flow Rate During Cooling Operation exceeds the Supply Air'// &
                                 ' Flow Rate During Cooling Operation.')
          CALL ShowContinueError('...Outdoor Air Flow Rate During Cooling Operation = '// &
                                 TRIM(RoundSigDigits(VRFTU(VRFTUNum)%CoolOutAirVolFlow,4))//' m3/s')
          CALL ShowContinueError('... Supply Air Flow Rate During Cooling Operation = '// &
                                 TRIM(RoundSigDigits(VRFTU(VRFTUNum)%MaxCoolAirVolFlow,4))//' m3/s')
          CALL ShowContinueError('...the outdoor air flow rate will be reduced to match and the simulation continues.')
          VRFTU(VRFTUNum)%CoolOutAirVolFlow = VRFTU(VRFTUNum)%MaxCoolAirVolFlow
        END IF

        IF (VRFTU(VRFTUNum)%MaxHeatAirVolFlow > VRFTU(VRFTUNum)%ActualFanVolFlowRate) THEN
          CALL ShowWarningError('InitVRF: VRF Terminal Unit = [' &
             //TRIM(cVRFTUTypes(VRFTU(VRFTUNum)%VRFTUType_Num))//', "'//TRIM(VRFTU(VRFTUNum)%Name)//'"]')
          CALL ShowContinueError('... has Supply Air Flow Rate During Heating Operation > Max Fan Volume Flow Rate, should be <=')
          CALL ShowContinueError('... Supply Air Flow Rate During Heating Operation = '// &
                                 TRIM(RoundSigDigits(VRFTU(VRFTUNum)%MaxHeatAirVolFlow,4))//' m3/s')
          CALL ShowContinueError('... Max Fan Volume Flow Rate                      = '// &
                                 TRIM(RoundSigDigits(VRFTU(VRFTUNum)%ActualFanVolFlowRate,4))//' m3/s')
          CALL ShowContinueError('...the supply air flow rate during cooling operation will be reduced'// &
                                 ' to match and the simulation continues.')
          VRFTU(VRFTUNum)%MaxHeatAirVolFlow = VRFTU(VRFTUNum)%ActualFanVolFlowRate
        ENDIF

        IF (VRFTU(VRFTUNum)%MaxNoHeatAirVolFlow > VRFTU(VRFTUNum)%ActualFanVolFlowRate) THEN
          CALL ShowWarningError('InitVRF: VRF Terminal Unit = [' &
             //TRIM(cVRFTUTypes(VRFTU(VRFTUNum)%VRFTUType_Num))//', "'//TRIM(VRFTU(VRFTUNum)%Name)//'"]')
          CALL ShowContinueError('... has Supply Air Flow Rate When No Heating is Needed > Max Fan Volume Flow Rate, should be <=')
          CALL ShowContinueError('... Supply Air Flow Rate When No Heating is Needed = '// &
                                 TRIM(RoundSigDigits(VRFTU(VRFTUNum)%MaxNoHeatAirVolFlow,4))//' m3/s')
          CALL ShowContinueError('... Max Fan Volume Flow Rate                       = '// &
                                 TRIM(RoundSigDigits(VRFTU(VRFTUNum)%ActualFanVolFlowRate,4))//' m3/s')
          CALL ShowContinueError('...the supply air flow rate when no cooling is needed will be reduced'// &
                                 ' to match and the simulation continues.')
          VRFTU(VRFTUNum)%MaxNoHeatAirVolFlow = VRFTU(VRFTUNum)%ActualFanVolFlowRate
        ENDIF

        IF(VRFTU(VRFTUNum)%HeatOutAirVolFlow .GT. VRFTU(VRFTUNum)%MaxHeatAirVolFlow)THEN
          CALL ShowWarningError('InitVRF: VRF Terminal Unit = [' &
             //TRIM(cVRFTUTypes(VRFTU(VRFTUNum)%VRFTUType_Num))//', "'//TRIM(VRFTU(VRFTUNum)%Name)//'"]')
          CALL ShowContinueError('...The Outdoor Air Flow Rate During Heating Operation exceeds the Supply Air'// &
                                 ' Flow Rate During Heating Operation.')
          CALL ShowContinueError('...Outdoor Air Flow Rate During Heating Operation = '// &
                                 TRIM(RoundSigDigits(VRFTU(VRFTUNum)%HeatOutAirVolFlow,4))//' m3/s')
          CALL ShowContinueError('... Supply Air Flow Rate During Heating Operation = '// &
                                 TRIM(RoundSigDigits(VRFTU(VRFTUNum)%MaxHeatAirVolFlow,4))//' m3/s')
          CALL ShowContinueError('...the outdoor air flow rate will be reduced to match and the simulation continues.')
          VRFTU(VRFTUNum)%HeatOutAirVolFlow = VRFTU(VRFTUNum)%MaxHeatAirVolFlow
        END IF

        IF (VRFTU(VRFTUNum)%NoCoolHeatOutAirVolFlow > VRFTU(VRFTUNum)%ActualFanVolFlowRate) THEN
          CALL ShowWarningError('InitVRF: VRF Terminal Unit = [' &
             //TRIM(cVRFTUTypes(VRFTU(VRFTUNum)%VRFTUType_Num))//', "'//TRIM(VRFTU(VRFTUNum)%Name)//'"]')
          CALL ShowContinueError('... has a Outdoor Air Flow Rate When No Cooling or Heating is Needed > '// &
                                 'Max Fan Volume Flow Rate, should be <=')
          CALL ShowContinueError('... Outdoor Air Flow Rate When No Cooling or Heating is Needed = '// &
                                 TRIM(RoundSigDigits(VRFTU(VRFTUNum)%NoCoolHeatOutAirVolFlow,4))//' m3/s')
          CALL ShowContinueError('... Max Fan Volume Flow Rate                                   = '// &
                                 TRIM(RoundSigDigits(VRFTU(VRFTUNum)%ActualFanVolFlowRate,4))//' m3/s')
          CALL ShowContinueError('...the outdoor air flow rate when no cooling or heating is needed will be reduced'// &
                                 ' to match and the simulation continues.')
          VRFTU(VRFTUNum)%NoCoolHeatOutAirVolFlow = VRFTU(VRFTUNum)%ActualFanVolFlowRate
        ENDIF


        IF(VRFTU(VRFTUNum)%ActualFanVolFlowRate .GT. 0.0)THEN
          VRFTU(VRFTUNum)%HeatingSpeedRatio = VRFTU(VRFTUNum)%MaxHeatAirVolFlow/VRFTU(VRFTUNum)%ActualFanVolFlowRate
          VRFTU(VRFTUNum)%CoolingSpeedRatio = VRFTU(VRFTUNum)%MaxCoolAirVolFlow/VRFTU(VRFTUNum)%ActualFanVolFlowRate
        END IF

        MyVRFFlag(VRFTUNum) = .FALSE.
      ELSE
        CALL GetFanVolFlow(VRFTU(VRFTUNum)%FanIndex,VRFTU(VRFTUNum)%ActualFanVolFlowRate)
      END IF
    END IF
  END IF ! IF(MyVRFFlag(VRFTUNum))THEN

  ! calculate end time of current time step to determine if max capacity reset is required
  CurrentEndTime = CurrentTime + SysTimeElapsed

  ! Initialize the maximum allowed terminal unit capacity. Total terminal unit capacity must not
  ! exceed the available condenser capacity. This variable is used to limit the terminal units
  ! providing more capacity than allowed. Example: TU loads are 1-ton, 2-ton, 3-ton, and 4-ton connected
  ! to a condenser having only 9-tons available. This variable will be set to 3-tons and the 4-ton
  ! terminal unit will be limited to 3-tons (see SimVRFCondenser where this variable is calculated).
  IF(CurrentEndTime .GT. CurrentEndTimeLast .OR. TimeStepSysLast .GT. TimeStepSys .OR. &
     FirstHVACIteration .AND. MyBeginTimeStepFlag(VRFCond))THEN
    MaxCoolingCapacity(VRFCond) = MaxCap
    MaxHeatingCapacity(VRFCond) = MaxCap
    MyBeginTimeStepFlag(VRFCond) = .FALSE.
  END IF

  IF(.NOT. FirstHVACIteration)MyBeginTimeStepFlag(VRFCond) = .TRUE.

  ! Do the following initializations (every time step).

  TimeStepSysLast = TimeStepSys
  CurrentEndTimeLast = CurrentEndTime

  TUListNum = VRFTU(VRFTUNum)%TUListNum

  IF (VRFTU(VRFTUNum)%FanOpModeSchedPtr .GT. 0) THEN
    IF (GetCurrentScheduleValue(VRFTU(VRFTUNum)%FanOpModeSchedPtr) .EQ. 0.0) THEN
      VRFTU(VRFTUNum)%OpMode = CycFanCycCoil
    ELSE
      VRFTU(VRFTUNum)%OpMode = ContFanCycCoil
    END IF
  END IF

  ! if condenser is off, all terminal unit coils are off
  IF (GetCurrentScheduleValue(VRF(VRFCond)%SchedPtr) .EQ. 0.0) THEN
    HeatingLoad(VRFCond) = .FALSE.
    CoolingLoad(VRFCond) = .FALSE.
  ELSE

!*** Operating Mode Initialization done at beginning of each iteration ***!
!*** assumes all TU's and Condeser were simulated last iteration ***!
!*** this code is done ONCE each iteration when all TU's IsSimulated flag is FALSE ***!
    ! Determine operating mode prior to simulating any terminal units connected to a VRF condenser
    ! this should happen at the beginning of a time step where all TU's are polled to see what
    ! mode the heat pump condenser will operate in
    IF(.NOT. ANY(TerminalUnitList(TUListNum)%IsSimulated))THEN
      NumCoolingLoads(VRFCond) = 0
      NumHeatingLoads(VRFCond) = 0
      SumCoolingLoads(VRFCond) = 0.0d0
      SumHeatingLoads(VRFCond) = 0.0d0
      MaxDeltaT(VRFCond)     = 0.0d0
      MinDeltaT(VRFCond)     = 0.0d0
      ZoneDeltaT             = 0.0d0
      HeatingLoad(VRFCond) = .FALSE.
      CoolingLoad(VRFCond) = .FALSE.
      ! loop through all TU's to find operating mode. Be carefull not to mix loop counters with current TU/Cond index
      DO NumTU = 1, TerminalUnitList(TUListNum)%NumTUInList
        ! make sure TU's have been sized before looping through each one of them to determine operating mode
        ! (which would size all coils based on the zone that called this specific VRF terminal unit)
        IF(ANY(TerminalUnitList(TUListNum)%TerminalUnitNotSizedYet))EXIT
        TUIndex     = TerminalUnitList(TUListNum)%ZoneTUPtr(NumTU)
        ThisZoneNum = VRFTU(TUIndex)%ZoneNum

!     Constant fan systems are tested for ventilation load to determine if load to be met changes.
!     more logic may be needed here, what is the OA flow rate, was last mode heating or cooling, what control is used, etc...
        ZoneLoad = ZoneSysEnergyDemand(ThisZoneNum)%RemainingOutputRequired
        IF(VRF(VRFCond)%ThermostatPriority == ThermostatOffsetPriority)THEN
!         for TSTATPriority, just check difference between zone temp and thermostat setpoint
          IF(ThisZoneNum .GT. 0)THEN
            SPTempHi = ZoneThermostatSetPointHi(ThisZoneNum)
            SPTempLo = ZoneThermostatSetPointLo(ThisZoneNum)
            SELECT CASE (TempControlType(ThisZoneNum))
              CASE (0) ! Uncontrolled
              CASE (SingleHeatingSetPoint)
                ZoneDeltaT = MAX(0.0d0,SPTempLo - ZT(ThisZoneNum))
                MaxDeltaT(VRFCond) = MAX(MaxDeltaT(VRFCond),ZoneDeltaT)
              CASE (SingleCoolingSetPoint)
                ZoneDeltaT = MIN(0.0d0,SPTempHi - ZT(ThisZoneNum))
                MinDeltaT(VRFCond) = MIN(MinDeltaT(VRFCond),ZoneDeltaT)
              CASE (SingleHeatCoolSetPoint)
                ZoneDeltaT = ZT(ThisZoneNum) - SPTempHi !- SPTempHi and SPTempLo are same value
                IF(ZoneDeltaT .GT. 0.0d0)THEN
                  MaxDeltaT(VRFCond) = MAX(MaxDeltaT(VRFCond),ZoneDeltaT)
                ELSE
                  MinDeltaT(VRFCond) = MIN(MinDeltaT(VRFCond),ZoneDeltaT)
                END IF
              CASE (DualSetPointWithDeadBand)
                IF(ZT(ThisZoneNum) - SPTempHi .GT. 0.0d0)THEN
                  ZoneDeltaT = MAX(0.0d0,ZT(ThisZoneNum) - SPTempHi)
                  MaxDeltaT(VRFCond) = MAX(MaxDeltaT(VRFCond),ZoneDeltaT)
                ELSE IF(SPTempLo - ZT(ThisZoneNum) .GT. 0.0d0)THEN
                  ZoneDeltaT = MIN(0.0d0,ZT(ThisZoneNum) - SPTempLo)
                  MinDeltaT(VRFCond) = MIN(MinDeltaT(VRFCond),ZoneDeltaT)
                END IF
              CASE DEFAULT
            END SELECT
          END IF
        ELSE IF(VRF(VRFCond)%ThermostatPriority == LoadPriority .OR. VRF(VRFCond)%ThermostatPriority == ZonePriority)THEN
          IF(VRFTU(TUIndex)%OpMode == ContFanCycCoil)THEN
            CALL SetCompFlowRate(TUIndex, VRFCond)
            CALL CalcVRF(TUIndex,FirstHVACIteration,0.0d0,TempOutput,OnOffAirFlowRatio)
            LoadToCoolingSP = ZoneSysEnergyDemand(ThisZoneNum)%OutputRequiredToCoolingSP
            LoadToHeatingSP = ZoneSysEnergyDemand(ThisZoneNum)%OutputRequiredToHeatingSP
!           If the Terminal Unit has a net cooling capacity (NoCompOutput < 0) and
!           the zone temp is above the Tstat heating setpoint (QToHeatSetPt < 0)
            IF(TempOutput < 0.0d0 .AND. LoadToHeatingSP .LT. 0.0d0)THEN
!             If the net cooling capacity overshoots the heating setpoint count as heating load
              IF(TempOutput < LoadToHeatingSP)THEN
!               Don't count as heating load unless mode is allowed. Also check for floating zone.
                IF(TempControlType(ThisZoneNum) .NE. SingleCoolingSetPoint .AND. &
                   TempControlType(ThisZoneNum) .NE. 0)THEN
                  IF(.NOT. LastModeHeating(VRFCond))THEN
                    IF(VRFTU(TUIndex)%OAMixerUsed)THEN
                      Node(VRFTU(TUIndex)%VRFTUOAMixerRetNodeNum)%MassFlowRate = VRFTU(TUIndex)%MaxHeatAirMassFlow
                      Node(VRFTU(TUIndex)%VRFTUOAMixerOANodeNum)%MassFlowRate = VRFTU(TUIndex)%HeatOutAirMassFlow
                      CALL SimOAMixer(VRFTU(TUIndex)%OAMixerName,FirstHVACIteration,VRFTU(TUIndex)%OAMixerIndex)
                    ELSE
                      Node(VRFTU(TUIndex)%VRFTUInletNodeNum)%MassFlowRate = VRFTU(TUIndex)%MaxHeatAirMassFlow
                    END IF
                    CALL CalcVRF(TUIndex,FirstHVACIteration,0.0d0,TempOutput,OnOffAirFlowRatio)
                    IF(TempOutput < LoadToHeatingSP)THEN
                      NumHeatingLoads(VRFCond) = NumHeatingLoads(VRFCond) + 1
                      ! sum heating load on condenser, not total zone heating load
                      SumHeatingLoads(VRFCond) = SumHeatingLoads(VRFCond) + (LoadToHeatingSP-TempOutput)
                    END IF
                  ELSE
                    NumHeatingLoads(VRFCond) = NumHeatingLoads(VRFCond) + 1
                    ! sum heating load on condenser, not total zone heating load
                    SumHeatingLoads(VRFCond) = SumHeatingLoads(VRFCond) + (LoadToHeatingSP-TempOutput)
                  END IF
                END IF
              ELSE IF(TempOutput .LT. ZoneLoad)THEN
!             If the net cooling capacity meets the zone cooling load but does not overshoot heating setpoint, turn off coil
!             do nothing, the zone will float
              ELSE IF(ZoneLoad .LT. 0.0d0)THEN
!               still a cooling load
                NumCoolingLoads(VRFCond) = NumCoolingLoads(VRFCond) + 1
                ! sum cooling load on condenser, not total zone cooling load
                SumCoolingLoads(VRFCond) = SumCoolingLoads(VRFCond) + (LoadToCoolingSP-TempOutput)
              END IF

!           If the terminal unit has a net heating capacity and the zone temp is below the Tstat cooling setpoint
            ELSE IF(TempOutput .GT. 0.0d0 .AND. LoadToCoolingSP .GT. 0.0d0)THEN
!             If the net heating capacity overshoots the cooling setpoint count as cooling load
              IF(TempOutput > LoadToCoolingSP)THEN
!               Don't count as cooling load unless mode is allowed. Also check for floating zone.
                IF(TempControlType(ThisZoneNum) .NE. SingleHeatingSetPoint .AND. &
                  TempControlType(ThisZoneNum) .NE. 0)THEN
                  IF(.NOT. LastModeCooling(VRFCond))THEN
                    IF(VRFTU(TUIndex)%OAMixerUsed)THEN
                      Node(VRFTU(TUIndex)%VRFTUOAMixerRetNodeNum)%MassFlowRate = VRFTU(TUIndex)%MaxCoolAirMassFlow
                      Node(VRFTU(TUIndex)%VRFTUOAMixerOANodeNum)%MassFlowRate = VRFTU(TUIndex)%CoolOutAirMassFlow
                      CALL SimOAMixer(VRFTU(TUIndex)%OAMixerName,FirstHVACIteration,VRFTU(TUIndex)%OAMixerIndex)
                    ELSE
                      Node(VRFTU(TUIndex)%VRFTUInletNodeNum)%MassFlowRate = VRFTU(TUIndex)%MaxCoolAirMassFlow
                    END IF
                    CALL CalcVRF(TUIndex,FirstHVACIteration,0.0d0,TempOutput,OnOffAirFlowRatio)
                    IF(TempOutput > LoadToCoolingSP)THEN
                      NumCoolingLoads(VRFCond) = NumCoolingLoads(VRFCond) + 1
                      SumCoolingLoads(VRFCond) = SumCoolingLoads(VRFCond) + (LoadToCoolingSP-TempOutput)
                    END IF
                  ELSE
                    NumCoolingLoads(VRFCond) = NumCoolingLoads(VRFCond) + 1
                    SumCoolingLoads(VRFCond) = SumCoolingLoads(VRFCond) + (LoadToCoolingSP-TempOutput)
                  END IF
                END IF
              ELSE IF(TempOutput > ZoneLoad)THEN
              ! do nothing, zone will float
              ELSE IF(ZoneLoad > 0.0d0)THEN
                NumHeatingLoads(VRFCond) = NumHeatingLoads(VRFCond) + 1
                SumHeatingLoads(VRFCond) = SumHeatingLoads(VRFCond) + ZoneLoad
              END IF
!           ELSE there is no overshoot and the zone has a valid cooling load
            ELSE IF(ZoneLoad .LT. 0.0d0)THEN
              NumCoolingLoads(VRFCond) = NumCoolingLoads(VRFCond) + 1
              SumCoolingLoads(VRFCond) = SumCoolingLoads(VRFCond) + ZoneLoad
            ! ELSE there is no overshoot and the zone has a valid heating load
            ELSE IF(ZoneLoad .GT. 0.0d0)THEN
              NumHeatingLoads(VRFCond) = NumHeatingLoads(VRFCond) + 1
              SumHeatingLoads(VRFCond) = SumHeatingLoads(VRFCond) + ZoneLoad
            END IF
          ELSE ! is cycling fan
            IF(ZoneLoad .GT. 0.0D0)THEN
              NumHeatingLoads(VRFCond) = NumHeatingLoads(VRFCond) + 1
              SumHeatingLoads(VRFCond) = SumHeatingLoads(VRFCond) + ZoneLoad
            ELSE IF(ZoneLoad .LT. 0.0D0)THEN
              NumCoolingLoads(VRFCond) = NumCoolingLoads(VRFCond) + 1
              SumCoolingLoads(VRFCond) = SumCoolingLoads(VRFCond) + ZoneLoad
            END IF
          END IF ! IF(VRFTU(VRFTUNum)%OpMode == ContFanCycCoil)THEN
        END IF
      END DO

! Determine operating mode based on VRF type and thermostat control selection
      IF(VRF(VRFCond)%HeatRecoveryUsed)THEN
      ! na for now
      ELSE
        SELECT CASE(VRF(VRFCond)%ThermostatPriority)
          CASE(ThermostatOffsetPriority)
            TUIndex = VRF(VRFCond)%MasterZoneTUIndex
            IF (VRFTU(TUIndex)%FanOpModeSchedPtr .GT. 0) THEN
              IF (GetCurrentScheduleValue(VRFTU(TUIndex)%FanOpModeSchedPtr) .EQ. 0.0) THEN
                FanOpMode = CycFanCycCoil
              ELSE
                FanOpMode = ContFanCycCoil
              END IF
            END IF
            IF(MaxDeltaT(VRFCond) .GT. ABS(MinDeltaT(VRFCond)) .AND. MaxDeltaT(VRFCond) .GT. 0.0d0)THEN
              HeatingLoad(VRFCond) = .FALSE.
              CoolingLoad(VRFCond) = .TRUE.
            ELSE IF(MaxDeltaT(VRFCond) .LT. ABS(MinDeltaT(VRFCond)) .AND. MinDeltaT(VRFCond) .LT. 0.0d0)THEN
              HeatingLoad(VRFCond) = .TRUE.
              CoolingLoad(VRFCond) = .FALSE.
            ! assuming if constant fan mode then previous operating mode will be needed
            ! could actually run the master zone TU to see if overshoot occurs, but seems overkill
            ELSE IF(FanOpMode == ContFanCycCoil .AND. LastModeCooling(VRFCond))THEN
              HeatingLoad(VRFCond) = .FALSE.
              CoolingLoad(VRFCond) = .TRUE.
            ELSE IF(FanOpMode == ContFanCycCoil .AND. LastModeHeating(VRFCond))THEN
              HeatingLoad(VRFCond) = .TRUE.
              CoolingLoad(VRFCond) = .FALSE.
            ELSE
              HeatingLoad(VRFCond) = .FALSE.
              CoolingLoad(VRFCond) = .FALSE.
            END IF
          CASE(LoadPriority)
            IF(SumHeatingLoads(VRFCond) .GT. ABS(SumCoolingLoads(VRFCond)) .AND. SumHeatingLoads(VRFCond) .GT. 0.0D0)THEN
              HeatingLoad(VRFCond) = .TRUE.
              CoolingLoad(VRFCond) = .FALSE.
            ELSE IF(SumHeatingLoads(VRFCond) .LE. ABS(SumCoolingLoads(VRFCond)) .AND. SumCoolingLoads(VRFCond) .LT. 0.0D0)THEN
              HeatingLoad(VRFCond) = .FALSE.
              CoolingLoad(VRFCond) = .TRUE.
            ELSE
              HeatingLoad(VRFCond) = .FALSE.
              CoolingLoad(VRFCond) = .FALSE.
            END IF
          CASE(ZonePriority)
            IF(NumHeatingLoads(VRFCond) .GT. NumCoolingLoads(VRFCond) .AND. NumHeatingLoads(VRFCond) .GT. 0)THEN
              HeatingLoad(VRFCond) = .TRUE.
              CoolingLoad(VRFCond) = .FALSE.
            ELSE IF(NumHeatingLoads(VRFCond) .LE. NumCoolingLoads(VRFCond) .AND. NumCoolingLoads(VRFCond) .GT. 0)THEN
              HeatingLoad(VRFCond) = .FALSE.
              CoolingLoad(VRFCond) = .TRUE.
            ELSE
              HeatingLoad(VRFCond) = .FALSE.
              CoolingLoad(VRFCond) = .FALSE.
            END IF
          CASE(ScheduledPriority)
            IF(GetCurrentScheduleValue(VRF(VRFCond)%SchedPriorityPtr) == 0)THEN
              HeatingLoad(VRFCond) = .TRUE.
              CoolingLoad(VRFCond) = .FALSE.
            ELSEIF(GetCurrentScheduleValue(VRF(VRFCond)%SchedPriorityPtr) == 1)THEN
              HeatingLoad(VRFCond) = .FALSE.
              CoolingLoad(VRFCond) = .TRUE.
            ELSE
              HeatingLoad(VRFCond) = .FALSE.
              CoolingLoad(VRFCond) = .FALSE.
            END If
          CASE(MasterThermostatPriority)
            ZoneLoad = ZoneSysEnergyDemand(VRF(VRFCond)%MasterZonePtr)%RemainingOutputRequired
            IF(ZoneLoad .GT. 0.0D0)THEN
              HeatingLoad(VRFCond) = .TRUE.
              CoolingLoad(VRFCond) = .FALSE.
            ELSE IF(ZoneLoad .LT. 0.0D0)THEN
              HeatingLoad(VRFCond) = .FALSE.
              CoolingLoad(VRFCond) = .TRUE.
            ELSE IF(VRFTU(VRFTUNum)%OpMode == ContFanCycCoil .AND. LastModeCooling(VRFCond))THEN
              HeatingLoad(VRFCond) = .FALSE.
              CoolingLoad(VRFCond) = .TRUE.
            ELSE IF(VRFTU(VRFTUNum)%OpMode == ContFanCycCoil .AND. LastModeHeating(VRFCond))THEN
              HeatingLoad(VRFCond) = .TRUE.
              CoolingLoad(VRFCond) = .FALSE.
            ELSE
              HeatingLoad(VRFCond) = .FALSE.
              CoolingLoad(VRFCond) = .FALSE.
            END IF
          CASE(FirstOnPriority)
          CASE DEFAULT
        END SELECT
      END IF

    END IF  ! IF(.NOT. ANY(TerminalUnitList(TUListNum)%IsSimulated))THEN
!*** End of Operating Mode Initialization done at beginning of each iteration ***!

!    ! not sure these are actually needed (i.e., probably doesn't hurt to have a cooling load if there is no cooling coil)
!    ! if there is no cooling coil, then reset cooling load variable
!    IF(CoolingLoad(VRFCond) .AND. .NOT. TerminalUnitList(TUListNum)%CoolingCoilPresent(VRFTU(VRFTUNum)%NumTUInList))THEN
!      CoolingLoad(VRFCond) = .FALSE.
!    END IF
!    ! if there is no heating coil, then reset to heating load variable
!    IF(HeatingLoad(VRFCond) .AND. .NOT. TerminalUnitList(TUListNum)%HeatingCoilPresent(VRFTU(VRFTUNum)%NumTUInList))THEN
!      HeatingLoad(VRFCond) = .FALSE.
!    END IF

    ! disable VRF system when outside limits of operation are exceeded
    IF(CoolingLoad(VRFCond))THEN
      IF((OutDryBulbTemp .LT. VRF(VRFCond)%MinOATCooling .OR. OutDryBulbTemp .GT. VRF(VRFCond)%MaxOATCooling) .AND. &
          ANY(TerminalUnitList(TUListNum)%CoolingCoilPresent))THEN
        CoolingLoad(VRFCond) = .FALSE.
        IF(VRF(VRFCond)%CoolingMaxTempLimitIndex == 0)THEN
          CALL ShowWarningError(TRIM(cVRFTypes(VRF(VRFCond)%VRFSystemTypeNum))//' "'//TRIM(VRF(VRFCond)%Name)//'".')
          CALL ShowContinueError('...InitVRF: VRF Heat Pump Min/Max Outdoor Temperature in Cooling Mode Limits have '// &
                                 'been exceeded and VRF system is disabled.')
          CALL ShowContinueError('... Outdoor Dry-Bulb Temperature                 = '//TRIM(TrimSigDigits(OutDryBulbTemp,3)))
          CALL ShowContinueError('... Cooling Minimum Outdoor Dry-Bulb Temperature = '// &
                                 TRIM(TrimSigDigits(VRF(VRFCond)%MinOATCooling,3)))
          CALL ShowContinueError('... Cooling Maximum Outdoor Dry-Bulb Temperature = '//  &
                                 TRIM(TrimSigDigits(VRF(VRFCond)%MaxOATCooling,3)))
          CALL ShowContinueErrorTimeStamp('... Check VRF Heat Pump Min/Max Outdoor Temperature in Cooling Mode limits.')
        END IF
        CALL ShowRecurringWarningErrorAtEnd(TRIM(cVRFTypes(VRF(VRFCond)%VRFSystemTypeNum))//' "'//  &
                 TRIM(VRF(VRFCond)%Name)//'" -- Exceeded VRF Heat Pump min/max cooling temperature limit error continues...',  &
                 VRF(VRFCond)%CoolingMaxTempLimitIndex,OutDryBulbTemp,OutDryBulbTemp)
      END IF
    ELSEIF(HeatingLoad(VRFCond))THEN
      IF((OutDryBulbTemp .LT. VRF(VRFCond)%MinOATHeating .OR. OutDryBulbTemp .GT. VRF(VRFCond)%MaxOATHeating) .AND. &
          ANY(TerminalUnitList(TUListNum)%HeatingCoilPresent))THEN
        HeatingLoad(VRFCond) = .FALSE.
        IF(VRF(VRFCond)%HeatingMaxTempLimitIndex == 0)THEN
          CALL ShowWarningError(TRIM(cVRFTypes(VRF(VRFCond)%VRFSystemTypeNum))//' "'//TRIM(VRF(VRFCond)%Name)//'".')
          CALL ShowContinueError('...InitVRF: VRF Heat Pump Min/Max Outdoor Temperature in Heating Mode Limits '// &
                                 'have been exceeded and VRF system is disabled.')
          CALL ShowContinueError('... Outdoor Dry-Bulb Temperature                 = '//TRIM(TrimSigDigits(OutDryBulbTemp,3)))
          CALL ShowContinueError('... Heating Minimum Outdoor Dry-Bulb Temperature = '// &
                                 TRIM(TrimSigDigits(VRF(VRFCond)%MinOATHeating,3)))
          CALL ShowContinueError('... Heating Maximum Outdoor Dry-Bulb Temperature = '//  &
                                 TRIM(TrimSigDigits(VRF(VRFCond)%MaxOATHeating,3)))
          CALL ShowContinueErrorTimeStamp('... Check VRF Heat Pump Min/Max Outdoor Temperature in Heating Mode limits.')
        END IF
        CALL ShowRecurringWarningErrorAtEnd(TRIM(cVRFTypes(VRF(VRFCond)%VRFSystemTypeNum))//' "'//  &
                 TRIM(VRF(VRFCond)%Name)//'" -- Exceeded VRF Heat Pump min/max heating temperature limit error continues...',  &
                 VRF(VRFCond)%HeatingMaxTempLimitIndex,OutDryBulbTemp,OutDryBulbTemp)
      END IF
    END IF

  END IF ! IF (GetCurrentScheduleValue(VRF(VRFCond)%SchedPtr) .EQ. 0.0) THEN

  IF(HeatingLoad(VRFCond))THEN
    IF(VRFTU(VRFTUNum)%OAMixerUsed)THEN
      Node(VRFTU(VRFTUNum)%VRFTUOAMixerRetNodeNum)%MassFlowRate = VRFTU(VRFTUNum)%MaxHeatAirMassFlow
      Node(OutsideAirNode)%MassFlowRate = VRFTU(VRFTUNum)%HeatOutAirMassFlow
    ELSE
      Node(InNode)%MassFlowRate = VRFTU(VRFTUNum)%MaxHeatAirMassFlow
    END IF
  ELSE IF(CoolingLoad(VRFCond))THEN
    IF(VRFTU(VRFTUNum)%OAMixerUsed)THEN
      Node(VRFTU(VRFTUNum)%VRFTUOAMixerRetNodeNum)%MassFlowRate = VRFTU(VRFTUNum)%MaxCoolAirMassFlow
      Node(OutsideAirNode)%MassFlowRate = VRFTU(VRFTUNum)%CoolOutAirMassFlow
    ELSE
      Node(InNode)%MassFlowRate = VRFTU(VRFTUNum)%MaxCoolAirMassFlow
    END IF
  ELSE
    IF(LastModeCooling(VRFCond))THEN
      IF(VRFTU(VRFTUNum)%OAMixerUsed)THEN
        Node(VRFTU(VRFTUNum)%VRFTUOAMixerRetNodeNum)%MassFlowRate = VRFTU(VRFTUNum)%MaxNoCoolAirMassFlow
        Node(OutsideAirNode)%MassFlowRate  = VRFTU(VRFTUNum)%NoCoolHeatOutAirMassFlow
      ELSE
        Node(InNode)%MassFlowRate = VRFTU(VRFTUNum)%MaxNoCoolAirMassFlow
      END IF
    ELSEIF(LastModeHeating(VRFCond))THEN
      IF(VRFTU(VRFTUNum)%OAMixerUsed)THEN
        Node(VRFTU(VRFTUNum)%VRFTUOAMixerRetNodeNum)%MassFlowRate = VRFTU(VRFTUNum)%MaxNoHeatAirMassFlow
        Node(OutsideAirNode)%MassFlowRate  = VRFTU(VRFTUNum)%NoCoolHeatOutAirMassFlow
      ELSE
        Node(InNode)%MassFlowRate = VRFTU(VRFTUNum)%MaxNoHeatAirMassFlow
      END IF
    END IF
  END IF

  IF(VRFTU(VRFTUNum)%OAMixerUsed)CALL SimOAMixer(VRFTU(VRFTUNum)%OAMixerName,FirstHVACIteration,VRFTU(VRFTUNum)%OAMixerIndex)

  OnOffAirFlowRatio = 1.0d0


!  check zone load to see if OA will overshoot setpoint temperature
  QZnReq = ZoneSysEnergyDemand(VRFTU(VRFTUNum)%ZoneNum)%RemainingOutputRequired
  LoadToCoolingSP = ZoneSysEnergyDemand(VRFTU(VRFTUNum)%ZoneNum)%OutputRequiredToCoolingSP
  LoadToHeatingSP = ZoneSysEnergyDemand(VRFTU(VRFTUNum)%ZoneNum)%OutputRequiredToHeatingSP
  IF(VRFTU(VRFTUNum)%OpMode == ContFanCycCoil)THEN
    CALL SetCompFlowRate(VRFTUNum, VRFCond, .TRUE.)
    CALL CalcVRF(VRFTUNum,FirstHVACIteration,0.0d0,TempOutput,OnOffAirFlowRatio)
!   If the Terminal Unit has a net cooling capacity (NoCompOutput < 0) and
!   the zone temp is above the Tstat heating setpoint (QToHeatSetPt < 0)
    IF(TempOutput < 0.0d0 .AND. LoadToHeatingSP .LT. 0.0d0)THEN
!     If the net cooling capacity overshoots the heating setpoint count as heating load
      IF(TempOutput < LoadToHeatingSP)THEN
!       Don't count as heating load unless mode is allowed. Also check for floating zone.
        IF(TempControlType(VRFTU(VRFTUNum)%ZoneNum) .NE. SingleCoolingSetPoint .AND. &
           TempControlType(VRFTU(VRFTUNum)%ZoneNum) .NE. 0)THEN
          IF(.NOT. LastModeHeating(VRFCond))THEN
            ! system last operated in cooling mode, change air flows and repeat coil off capacity test
            IF(VRFTU(VRFTUNum)%OAMixerUsed)THEN
              Node(VRFTU(VRFTUNum)%VRFTUOAMixerRetNodeNum)%MassFlowRate = VRFTU(VRFTUNum)%MaxHeatAirMassFlow
              Node(VRFTU(VRFTUNum)%VRFTUOAMixerOANodeNum)%MassFlowRate = VRFTU(VRFTUNum)%HeatOutAirMassFlow
              CALL SimOAMixer(VRFTU(VRFTUNum)%OAMixerName,FirstHVACIteration,VRFTU(VRFTUNum)%OAMixerIndex)
            ELSE
              Node(InNode)%MassFlowRate = VRFTU(VRFTUNum)%MaxHeatAirMassFlow
            END IF
            CALL CalcVRF(VRFTUNum,FirstHVACIteration,0.0d0,TempOutput,OnOffAirFlowRatio)
            ! if zone temp will overshoot, pass the LoadToHeatingSP as the load to meet
            IF(TempOutput < LoadToHeatingSP)THEN
              QZnReq = LoadToHeatingSP
            END IF
          ELSE
            ! last mode was heating, zone temp will overshoot cooling setpoint, reset QznReq to LoadtoCoolingSP
!            IF(TempOutput > LoadToCoolingSP)THEN
!              QZnReq = LoadToCoolingSP
              QZnReq = LoadToHeatingSP
!            ELSE
!              QZnReq = 0.d0
!            END IF
          END IF
        END IF
      ELSE IF(TempOutput > LoadToCoolingSP .AND. LoadToCoolingSP < 0.d0)THEN
!     If the net cooling capacity meets the zone cooling load but does not overshoot heating setpoint
        QZnReq = LoadToCoolingSP
      END IF
!   If the terminal unit has a net heating capacity and the zone temp is below the Tstat cooling setpoint
    ELSE IF(TempOutput .GT. 0.0d0 .AND. LoadToCoolingSP .GT. 0.0d0)THEN
!     If the net heating capacity overshoots the cooling setpoint count as cooling load
      IF(TempOutput > LoadToCoolingSP)THEN
!       Don't count as cooling load unless mode is allowed. Also check for floating zone.
        IF(TempControlType(VRFTU(VRFTUNum)%ZoneNum) .NE. SingleHeatingSetPoint .AND. &
           TempControlType(VRFTU(VRFTUNum)%ZoneNum) .NE. 0)THEN
          IF(.NOT. LastModeCooling(VRFCond))THEN
            IF(VRFTU(VRFTUNum)%OAMixerUsed)THEN
              Node(VRFTU(VRFTUNum)%VRFTUOAMixerRetNodeNum)%MassFlowRate = VRFTU(VRFTUNum)%MaxCoolAirMassFlow
              Node(VRFTU(VRFTUNum)%VRFTUOAMixerOANodeNum)%MassFlowRate = VRFTU(VRFTUNum)%CoolOutAirMassFlow
              CALL SimOAMixer(VRFTU(VRFTUNum)%OAMixerName,FirstHVACIteration,VRFTU(VRFTUNum)%OAMixerIndex)
            ELSE
              Node(VRFTU(VRFTUNum)%VRFTUInletNodeNum)%MassFlowRate = VRFTU(VRFTUNum)%MaxCoolAirMassFlow
            END IF
            CALL CalcVRF(VRFTUNum,FirstHVACIteration,0.0d0,TempOutput,OnOffAirFlowRatio)
            ! if zone temp will overshoot, pass the LoadToCoolingSP as the load to meet
            IF(TempOutput > LoadToCoolingSP)THEN
              QZnReq = LoadToCoolingSP
            END IF
          ELSE
            QZnReq = LoadToCoolingSP
          END IF
        END IF
      ELSE IF(TempOutput > QZnReq)THEN
!       If the net heating capacity meets the zone heating load but does not overshoot cooling setpoint
        QZnReq = 0.d0
      END IF
    END IF
  END IF ! IF(VRFTU(VRFTUNum)%OpMode == ContFanCycCoil)THEN

  ! Override operating mode when using EMS
  IF (VRF(VRFCond)%EMSOverrideHPOperatingMode) THEN
    IF(VRF(VRFCond)%EMSValueForHPOperatingMode == 0.d0)THEN
      HeatingLoad(VRFCond) = .FALSE.
      CoolingLoad(VRFCond) = .FALSE.
    ELSE IF(VRF(VRFCond)%EMSValueForHPOperatingMode == 1.d0)THEN
      HeatingLoad(VRFCond) = .FALSE.
      CoolingLoad(VRFCond) = .TRUE.
      QZnReq = LoadToCoolingSP
    ELSE IF(VRF(VRFCond)%EMSValueForHPOperatingMode == 2.d0)THEN
      HeatingLoad(VRFCond) = .TRUE.
      CoolingLoad(VRFCond) = .FALSE.
      QZnReq = LoadToHeatingSP
    ELSE
! RAR  add recurring here
      IF(VRF(VRFCond)%HPOperatingModeErrorIndex == 0)THEN
        CALL ShowWarningError(TRIM(cVRFTypes(VRF(VRFCond)%VRFSystemTypeNum))//' "'//TRIM(VRF(VRFCond)%Name)//'".')
        CALL ShowContinueError('...InitVRF: Illegal HP operating mode = '// &
                               TRIM(TrimSigDigits(VRF(VRFCond)%EMSValueForHPOperatingMode,0)))
        CALL ShowContinueError('...InitVRF: VRF HP operating mode will not be controlled by EMS.')

      END IF
      CALL ShowRecurringWarningErrorAtEnd(TRIM(cVRFTypes(VRF(VRFCond)%VRFSystemTypeNum))//' "'//  &
           TRIM(VRF(VRFCond)%Name)//'" -- Illegal HP operating mode error continues...',  &
           VRF(VRFCond)%HPOperatingModeErrorIndex,VRF(VRFCond)%EMSValueForHPOperatingMode, &
            VRF(VRFCond)%EMSValueForHPOperatingMode)
    END IF
  ENDIF

  IF(CoolingLoad(VRFCond) .and. QZnReq /= 0.d0)THEN
    CompOnMassFlow = VRFTU(VRFTUNum)%MaxCoolAirMassFlow
    CompOffMassFlow = VRFTU(VRFTUNum)%MaxNoCoolAirMassFlow
    OACompOnMassFlow = VRFTU(VRFTUNum)%CoolOutAirMassFlow
    OACompOffMassFlow = VRFTU(VRFTUNum)%NoCoolHeatOutAirVolFlow
  ELSE IF(HeatingLoad(VRFCond) .and. QZnReq /= 0.d0)THEN
    CompOnMassFlow = VRFTU(VRFTUNum)%MaxHeatAirMassFlow
    CompOffMassFlow = VRFTU(VRFTUNum)%MaxNoHeatAirMassFlow
    OACompOnMassFlow = VRFTU(VRFTUNum)%HeatOutAirMassFlow
    OACompOffMassFlow = VRFTU(VRFTUNum)%NoCoolHeatOutAirVolFlow
  ELSE
    IF(LastModeCooling(VRFCond))CompOnMassFlow = VRFTU(VRFTUNum)%MaxNoCoolAirMassFlow
    IF(LastModeCooling(VRFCond))CompOffMassFlow = VRFTU(VRFTUNum)%MaxNoCoolAirMassFlow
    IF(LastModeHeating(VRFCond))CompOnMassFlow = VRFTU(VRFTUNum)%MaxNoHeatAirMassFlow
    IF(LastModeHeating(VRFCond))CompOffMassFlow = VRFTU(VRFTUNum)%MaxNoHeatAirMassFlow
    IF(LastModeCooling(VRFCond))OACompOnMassFlow = VRFTU(VRFTUNum)%CoolOutAirMassFlow
    IF(LastModeHeating(VRFCond))OACompOnMassFlow = VRFTU(VRFTUNum)%HeatOutAirMassFlow
    OACompOffMassFlow = VRFTU(VRFTUNum)%NoCoolHeatOutAirVolFlow
  END IF

  IF(VRFTU(VRFTUNum)%OpMode .EQ. CycFanCycCoil)THEN
    CompOffMassFlow = 0.d0
    OACompOffMassFlow = 0.d0
  END IF

  CALL SetAverageAirFlow(VRFTUNum, 0.d0, OnOffAirFlowRatio)

  RETURN

END SUBROUTINE InitVRF

SUBROUTINE SetCompFlowRate(VRFTUNum, VRFCond, UseCurrentMode)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Raustad, FSEC
          !       DATE WRITTEN   June 2011
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is for calling VRF terminal units during Init to initialize flow rate
          ! while looping through all terminal units connected to a specific condenser.
          ! This allows polling of capacities for all terminal units.
          ! Since the heat pump can only heat or cool, a single operating mode is chosen for each condenser.

          ! METHODOLOGY EMPLOYED:
          ! Initializes flow rates for a specific terminal unit.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  Integer, Intent(IN) :: VRFTUNum
  Integer, Intent(IN) :: VRFCond
  Logical, OPTIONAL, Intent(IN) :: UseCurrentMode
          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  LOGICAL :: CurrentMode    !- specifies whether current or previous operating mode is used

  CurrentMode = .FALSE.
  IF(Present(UseCurrentMode))CurrentMode = UseCurrentMode

  IF(CurrentMode)THEN ! uses current operating mode to set flow rate (after mode is set)
    IF(CoolingLoad(VRFCond))THEN
      CompOnMassFlow    = VRFTU(VRFTUNum)%MaxCoolAirMassFlow
      CompOffMassFlow   = VRFTU(VRFTUNum)%MaxNoCoolAirMassFlow
      OACompOnMassFlow  = VRFTU(VRFTUNum)%CoolOutAirMassFlow
      OACompOffMassFlow = VRFTU(VRFTUNum)%NoCoolHeatOutAirVolFlow
    ELSE IF(HeatingLoad(VRFCond))THEN
      CompOnMassFlow    = VRFTU(VRFTUNum)%MaxHeatAirMassFlow
      CompOffMassFlow   = VRFTU(VRFTUNum)%MaxNoHeatAirMassFlow
      OACompOnMassFlow  = VRFTU(VRFTUNum)%HeatOutAirMassFlow
      OACompOffMassFlow = VRFTU(VRFTUNum)%NoCoolHeatOutAirVolFlow
    ELSE IF(LastModeCooling(VRFCond))THEN ! if NOT cooling or heating then use last mode
      CompOnMassFlow    = VRFTU(VRFTUNum)%MaxCoolAirMassFlow
      CompOffMassFlow   = VRFTU(VRFTUNum)%MaxNoCoolAirMassFlow
      OACompOnMassFlow  = VRFTU(VRFTUNum)%CoolOutAirMassFlow
      OACompOffMassFlow = VRFTU(VRFTUNum)%NoCoolHeatOutAirVolFlow
    ELSE IF(LastModeHeating(VRFCond))THEN ! if NOT cooling or heating then use last mode
      CompOnMassFlow    = VRFTU(VRFTUNum)%MaxHeatAirMassFlow
      CompOffMassFlow   = VRFTU(VRFTUNum)%MaxNoHeatAirMassFlow
      OACompOnMassFlow  = VRFTU(VRFTUNum)%HeatOutAirMassFlow
      OACompOffMassFlow = VRFTU(VRFTUNum)%NoCoolHeatOutAirVolFlow
    ELSE ! should not happen so just set to cooling flow rate
      CompOnMassFlow    = VRFTU(VRFTUNum)%MaxCoolAirMassFlow
      CompOffMassFlow   = VRFTU(VRFTUNum)%MaxNoCoolAirMassFlow
      OACompOnMassFlow  = VRFTU(VRFTUNum)%CoolOutAirMassFlow
      OACompOffMassFlow = VRFTU(VRFTUNum)%NoCoolHeatOutAirVolFlow
    END IF
  ELSE ! uses previous operating mode to set flow rate (used for looping through each TU in Init before mode is set)
    IF(LastModeCooling(VRFCond))THEN
      CompOnMassFlow    = VRFTU(VRFTUNum)%MaxCoolAirMassFlow
      CompOffMassFlow   = VRFTU(VRFTUNum)%MaxNoCoolAirMassFlow
      OACompOnMassFlow  = VRFTU(VRFTUNum)%CoolOutAirMassFlow
      OACompOffMassFlow = VRFTU(VRFTUNum)%NoCoolHeatOutAirVolFlow
    ELSE IF(LastModeHeating(VRFCond))THEN
      CompOnMassFlow    = VRFTU(VRFTUNum)%MaxHeatAirMassFlow
      CompOffMassFlow   = VRFTU(VRFTUNum)%MaxNoHeatAirMassFlow
      OACompOnMassFlow  = VRFTU(VRFTUNum)%HeatOutAirMassFlow
      OACompOffMassFlow = VRFTU(VRFTUNum)%NoCoolHeatOutAirVolFlow
    ELSE ! should not happen so just set to cooling flow rate
      CompOnMassFlow    = VRFTU(VRFTUNum)%MaxCoolAirMassFlow
      CompOffMassFlow   = VRFTU(VRFTUNum)%MaxNoCoolAirMassFlow
      OACompOnMassFlow  = VRFTU(VRFTUNum)%CoolOutAirMassFlow
      OACompOffMassFlow = VRFTU(VRFTUNum)%NoCoolHeatOutAirVolFlow
    END IF
  END IF

  IF(VRFTU(VRFTUNum)%OpMode .EQ. CycFanCycCoil)THEN
    CompOffMassFlow = 0.d0
    OACompOffMassFlow = 0.d0
  END IF

  RETURN

END SUBROUTINE SetCompFlowRate

SUBROUTINE SizeVRF(VRFTUNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Raustad, FSEC
          !       DATE WRITTEN   August 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is for sizing VRF Components for which inputs have not been
          ! specified in the input.

          ! METHODOLOGY EMPLOYED:
          ! Obtains flow rates from the zone or system sizing arrays.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataSizing
  USE CurveManager, ONLY: CurveValue
  USE DXCoils,  ONLY: GetDXCoilCap=>GetCoilCapacityByIndexType
  USE ReportSizingManager, ONLY: ReportSizingOutput
  USE DataGlobals, ONLY : OutputFileInits
  USE General, ONLY: RoundSigDigits

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  Integer, Intent(IN) :: VRFTUNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  LOGICAL, ALLOCATABLE,SAVE, DIMENSION(:) :: CheckVRFCombinationRatio
  LOGICAL,SAVE :: MyOneTimeFlag = .TRUE.                  ! One time flag used to allocate MyEnvrnFlag and MySizeFlag
  LOGICAL       :: FoundAll       ! temporary variable used to check all terminal units
  LOGICAL       :: ErrFlag        ! temporary variable used for error checking
  REAL(r64)     :: TUCoolingCapacity  ! total terminal unit cooling capacity
  REAL(r64)     :: TUHeatingCapacity  ! total terminal unit heating capacity
  INTEGER       :: VRFCond        ! index to VRF condenser
  INTEGER       :: TUListNum      ! index to terminal unit list
  INTEGER       :: TUIndex        ! index to terminal unit
  INTEGER       :: NumTU          ! DO Loop index counter
  LOGICAL, SAVE :: MyOneTimeEIOFlag = .TRUE. ! eio header flag reporting
  REAL(r64)     :: OnOffAirFlowRat ! temporary variable used when sizing coils

  VRFCond = VRFTU(VRFTUNum)%VRFSysNum

  IF (MyOneTimeFlag) THEN
    ! initialize the environment and sizing flags
    ALLOCATE(CheckVRFCombinationRatio(NumVRFCond))
    CheckVRFCombinationRatio = .TRUE.
    MyOneTimeFlag = .FALSE.
  END IF

  IF (VRFTU(VRFTUNum)%MaxCoolAirVolFlow == AutoSize) THEN

    IF (CurZoneEqNum > 0) THEN

      CALL CheckZoneSizing(cVRFTUTypes(VRFTU(VRFTUNum)%VRFTUType_Num), VRFTU(VRFTUNum)%Name)
      VRFTU(VRFTUNum)%MaxCoolAirVolFlow = MAX(FinalZoneSizing(CurZoneEqNum)%DesCoolVolFlow, &
                                            FinalZoneSizing(CurZoneEqNum)%DesHeatVolFlow)
      IF (VRFTU(VRFTUNum)%MaxCoolAirVolFlow < SmallAirVolFlow) THEN
        VRFTU(VRFTUNum)%MaxCoolAirVolFlow = 0.0
      END IF
      CALL ReportSizingOutput(cVRFTUTypes(VRFTU(VRFTUNum)%VRFTUType_Num), VRFTU(VRFTUNum)%Name, &
                     'Supply Air Flow Rate During Cooling Operation [m3/s]', VRFTU(VRFTUNum)%MaxCoolAirVolFlow)

    END IF

  END IF

  IF (VRFTU(VRFTUNum)%MaxHeatAirVolFlow == AutoSize) THEN

    IF (CurZoneEqNum > 0) THEN

      CALL CheckZoneSizing(cVRFTUTypes(VRFTU(VRFTUNum)%VRFTUType_Num), VRFTU(VRFTUNum)%Name)
      VRFTU(VRFTUNum)%MaxHeatAirVolFlow = MAX(FinalZoneSizing(CurZoneEqNum)%DesCoolVolFlow, &
                                            FinalZoneSizing(CurZoneEqNum)%DesHeatVolFlow)
      IF (VRFTU(VRFTUNum)%MaxHeatAirVolFlow < SmallAirVolFlow) THEN
        VRFTU(VRFTUNum)%MaxHeatAirVolFlow = 0.0
      END IF

      CALL ReportSizingOutput(cVRFTUTypes(VRFTU(VRFTUNum)%VRFTUType_Num), VRFTU(VRFTUNum)%Name, &
                     'Supply Air Flow Rate During Heating Operation [m3/s]', VRFTU(VRFTUNum)%MaxHeatAirVolFlow)

    END IF

  END IF

  IF (VRFTU(VRFTUNum)%MaxNoCoolAirVolFlow == AutoSize) THEN

    IF (CurZoneEqNum > 0) THEN

      CALL CheckZoneSizing(cVRFTUTypes(VRFTU(VRFTUNum)%VRFTUType_Num), VRFTU(VRFTUNum)%Name)
      VRFTU(VRFTUNum)%MaxNoCoolAirVolFlow = VRFTU(VRFTUNum)%MaxCoolAirVolFlow
      IF (VRFTU(VRFTUNum)%MaxNoCoolAirVolFlow < SmallAirVolFlow) THEN
        VRFTU(VRFTUNum)%MaxNoCoolAirVolFlow = 0.0
      END IF

      CALL ReportSizingOutput(cVRFTUTypes(VRFTU(VRFTUNum)%VRFTUType_Num), VRFTU(VRFTUNum)%Name, &
                     'Supply Air Flow Rate When No Cooling is Needed [m3/s]', VRFTU(VRFTUNum)%MaxNoCoolAirVolFlow)

    END IF

  END IF

  IF (VRFTU(VRFTUNum)%MaxNoHeatAirVolFlow == AutoSize) THEN

    IF (CurZoneEqNum > 0) THEN

      CALL CheckZoneSizing(cVRFTUTypes(VRFTU(VRFTUNum)%VRFTUType_Num), VRFTU(VRFTUNum)%Name)
      VRFTU(VRFTUNum)%MaxNoHeatAirVolFlow = VRFTU(VRFTUNum)%MaxHeatAirVolFlow
      IF (VRFTU(VRFTUNum)%MaxNoHeatAirVolFlow < SmallAirVolFlow) THEN
        VRFTU(VRFTUNum)%MaxNoHeatAirVolFlow = 0.0
      END IF

      CALL ReportSizingOutput(cVRFTUTypes(VRFTU(VRFTUNum)%VRFTUType_Num), VRFTU(VRFTUNum)%Name, &
                     'Supply Air Flow Rate When No Heating is Needed [m3/s]', VRFTU(VRFTUNum)%MaxNoHeatAirVolFlow)

    END IF

  END IF

  IF (VRFTU(VRFTUNum)%CoolOutAirVolFlow == AutoSize) THEN

    IF (CurZoneEqNum > 0) THEN

      CALL CheckZoneSizing(cVRFTUTypes(VRFTU(VRFTUNum)%VRFTUType_Num), VRFTU(VRFTUNum)%Name)
      VRFTU(VRFTUNum)%CoolOutAirVolFlow = MIN(FinalZoneSizing(CurZoneEqNum)%MinOA,VRFTU(VRFTUNum)%MaxCoolAirVolFlow)
      IF (VRFTU(VRFTUNum)%CoolOutAirVolFlow < SmallAirVolFlow) THEN
        VRFTU(VRFTUNum)%CoolOutAirVolFlow = 0.0
      END IF
      CALL ReportSizingOutput(cVRFTUTypes(VRFTU(VRFTUNum)%VRFTUType_Num), VRFTU(VRFTUNum)%Name, &
                     'Outdoor Air Flow Rate During Cooling Operation [m3/s]',VRFTU(VRFTUNum)%CoolOutAirVolFlow)

    END IF

  END IF

  IF (VRFTU(VRFTUNum)%HeatOutAirVolFlow == AutoSize) THEN

    IF (CurZoneEqNum > 0) THEN

      CALL CheckZoneSizing(cVRFTUTypes(VRFTU(VRFTUNum)%VRFTUType_Num), VRFTU(VRFTUNum)%Name)
      VRFTU(VRFTUNum)%HeatOutAirVolFlow = MIN(FinalZoneSizing(CurZoneEqNum)%MinOA,VRFTU(VRFTUNum)%MaxHeatAirVolFlow)
      IF (VRFTU(VRFTUNum)%HeatOutAirVolFlow < SmallAirVolFlow) THEN
        VRFTU(VRFTUNum)%HeatOutAirVolFlow = 0.0
      END IF
      CALL ReportSizingOutput(cVRFTUTypes(VRFTU(VRFTUNum)%VRFTUType_Num), VRFTU(VRFTUNum)%Name, &
                     'Outdoor Air Flow Rate During Heating Operation [m3/s]',VRFTU(VRFTUNum)%CoolOutAirVolFlow)

    END IF

  END IF

  IF (VRFTU(VRFTUNum)%NoCoolHeatOutAirVolFlow == AutoSize) THEN

    IF (CurZoneEqNum > 0) THEN

      CALL CheckZoneSizing(cVRFTUTypes(VRFTU(VRFTUNum)%VRFTUType_Num), VRFTU(VRFTUNum)%Name)
      VRFTU(VRFTUNum)%NoCoolHeatOutAirVolFlow = MIN(FinalZoneSizing(CurZoneEqNum)%MinOA, &
                                                    VRFTU(VRFTUNum)%HeatOutAirVolFlow, &
                                                    VRFTU(VRFTUNum)%CoolOutAirVolFlow)
      IF (VRFTU(VRFTUNum)%NoCoolHeatOutAirVolFlow < SmallAirVolFlow) THEN
        VRFTU(VRFTUNum)%NoCoolHeatOutAirVolFlow = 0.0
      END IF
      CALL ReportSizingOutput(cVRFTUTypes(VRFTU(VRFTUNum)%VRFTUType_Num), VRFTU(VRFTUNum)%Name, &
              'Outdoor Air Flow Rate When No Cooling or Heating is Needed [m3/s]',VRFTU(VRFTUNum)%NoCoolHeatOutAirVolFlow)

    END IF

  END IF

  IF(CheckVRFCombinationRatio(VRFCond))THEN
    OnOffAirFlowRat = 1.d0
    ! simulate the TU to size the coils
    CALL CalcVRF(VRFTUNum,.TRUE.,0.0d0,TUCoolingCapacity,OnOffAirFlowRat)
    TUCoolingCapacity = 0.0d0
    TUHeatingCapacity = 0.0d0
    FoundAll      = .TRUE.
    TUListNum = VRFTU(VRFTUNum)%TUListNum
    DO NumTU = 1, TerminalUnitList(TUListNum)%NumTUInList
      TUIndex = TerminalUnitList(TUListNum)%ZoneTUPtr(NumTU)
      IF(VRFTU(TUIndex)%CoolCoilIndex .GT. 0)THEN
        TUCoolingCapacity = TUCoolingCapacity + &
                            GetDXCoilCap(VRFTU(TUIndex)%CoolCoilIndex,VRFTU(TUIndex)%DXCoolCoilType_Num,ErrFlag)
        IF(GetDXCoilCap(VRFTU(TUIndex)%CoolCoilIndex,VRFTU(TUIndex)%DXCoolCoilType_Num,ErrFlag) == AutoSize)THEN
          FoundAll = .FALSE.
          EXIT
        END IF
      END IF
      IF(VRFTU(TUIndex)%HeatCoilIndex .GT. 0)THEN
        TUHeatingCapacity = TUHeatingCapacity + &
                            GetDXCoilCap(VRFTU(TUIndex)%HeatCoilIndex,VRFTU(TUIndex)%DXHeatCoilType_Num,ErrFlag)
        IF(GetDXCoilCap(VRFTU(TUIndex)%HeatCoilIndex,VRFTU(TUIndex)%DXHeatCoilType_Num,ErrFlag) == AutoSize)THEN
          FoundAll = .FALSE.
          EXIT
        END IF
      END IF
    END DO

    IF(FoundAll)THEN
      IF(VRF(VRFCond)%CoolingCapacity == Autosize)THEN
        VRF(VRFCond)%CoolingCapacity = TUCoolingCapacity
        CALL ReportSizingOutput(cVRFTypes(VRF(VRFCond)%VRFSystemTypeNum), VRF(VRFCond)%Name, &
                                    'Rated Total Cooling Capacity (gross) [W]', VRF(VRFCond)%CoolingCapacity)
      END IF
      IF(VRF(VRFCond)%CoolingCapacity .GT. 0.0d0)THEN
        VRF(VRFCond)%CoolingCombinationRatio = TUCoolingCapacity / VRF(VRFCond)%CoolingCapacity
      END IF

      IF(VRF(VRFCond)%HeatingCapacity == Autosize)THEN
        VRF(VRFCond)%HeatingCapacity = TUHeatingCapacity
        CALL ReportSizingOutput(cVRFTypes(VRF(VRFCond)%VRFSystemTypeNum), VRF(VRFCond)%Name, &
               'Rated Total Heating Capacity [W]', VRF(VRFCond)%HeatingCapacity)
      END IF

      IF(VRF(VRFCond)%HeatingCapacity .GT. 0.0d0)THEN
        VRF(VRFCond)%HeatingCombinationRatio = TUHeatingCapacity / VRF(VRFCond)%HeatingCapacity
      END IF

      ! calculate the piping correction factors only once
      IF(VRF(VRFCond)%PCFLengthCoolPtr .GT. 0)THEN
        SELECT CASE(VRF(VRFCond)%PCFLengthCoolPtrType)
        CASE(BIQUADRATIC)
          VRF(VRFCond)%PipingCorrectionCooling = MIN(1.0d0,MAX(0.5d0,CurveValue(VRF(VRFCond)%PCFLengthCoolPtr, &
                                                 VRF(VRFCond)%EquivPipeLngthCool,VRF(VRFCond)%CoolingCombinationRatio) + &
                                                 VRF(VRFCond)%VertPipeLngth * VRF(VRFCond)%PCFHeightCool))
        CASE DEFAULT
          VRF(VRFCond)%PipingCorrectionCooling = MIN(1.0d0,MAX(0.5d0,CurveValue(VRF(VRFCond)%PCFLengthCoolPtr, &
                                                 VRF(VRFCond)%EquivPipeLngthCool) + &
                                                 VRF(VRFCond)%VertPipeLngth * VRF(VRFCond)%PCFHeightCool))
        END SELECT
      ELSE
        VRF(VRFCond)%PipingCorrectionCooling = MIN(1.0d0,MAX(0.5d0,(1.0d0 + VRF(VRFCond)%VertPipeLngth*VRF(VRFCond)%PCFHeightCool)))
      END IF

      IF(VRF(VRFCond)%PCFLengthHeatPtr .GT. 0)THEN
        SELECT CASE(VRF(VRFCond)%PCFLengthHeatPtrType)
        CASE(BIQUADRATIC)
          VRF(VRFCond)%PipingCorrectionHeating = MIN(1.0d0,MAX(0.5d0,CurveValue(VRF(VRFCond)%PCFLengthHeatPtr, &
                                                 VRF(VRFCond)%EquivPipeLngthHeat,VRF(VRFCond)%HeatingCombinationRatio) + &
                                                 VRF(VRFCond)%VertPipeLngth * VRF(VRFCond)%PCFHeightHeat))
        CASE DEFAULT
          VRF(VRFCond)%PipingCorrectionHeating = MIN(1.0d0,MAX(0.5d0,CurveValue(VRF(VRFCond)%PCFLengthHeatPtr, &
                                                 VRF(VRFCond)%EquivPipeLngthHeat) + &
                                                 VRF(VRFCond)%VertPipeLngth * VRF(VRFCond)%PCFHeightHeat))
        END SELECT
      ELSE
        VRF(VRFCond)%PipingCorrectionHeating = MIN(1.0d0,MAX(0.5d0,(1.0d0 + VRF(VRFCond)%VertPipeLngth*VRF(VRFCond)%PCFHeightHeat)))
      END IF

      VRF(VRFCond)%RatedCoolingPower = VRF(VRFCond)%CoolingCapacity/VRF(VRFCond)%CoolingCOP
      VRF(VRFCond)%RatedHeatingPower = VRF(VRFCond)%HeatingCapacity/VRF(VRFCond)%HeatingCOP

      IF(VRF(VRFCond)%CoolCombRatioPTR .GT. 0)THEN
        CoolCombinationRatio(VRFCond) = CurveValue(VRF(VRFCond)%CoolCombRatioPTR,VRF(VRFCond)%CoolingCombinationRatio)
      ELSE
        CoolCombinationRatio(VRFCond) = 1.0d0
      END IF

      IF(VRF(VRFCond)%HeatCombRatioPTR .GT. 0)THEN
        HeatCombinationRatio(VRFCond) = CurveValue(VRF(VRFCond)%HeatCombRatioPTR,VRF(VRFCond)%HeatingCombinationRatio)
      ELSE
        HeatCombinationRatio(VRFCond) = 1.0d0
      END IF

      IF (VRF(VRFCond)%DefrostCapacity == AutoSize) THEN

        IF (VRF(VRFCond)%DefrostStrategy == Resistive) THEN
          VRF(VRFCond)%DefrostCapacity = VRF(VRFCond)%CoolingCapacity
        ELSE
          VRF(VRFCond)%DefrostCapacity = 0.d0
        END IF
        CALL ReportSizingOutput(cVRFTypes(VRF(VRFCond)%VRFSystemTypeNum), VRF(VRFCond)%Name, &
                                'Resistive Defrost Heater Capacity', VRF(VRFCond)%DefrostCapacity)

      END IF

      IF (VRF(VRFCond)%EvapCondAirVolFlowRate == AutoSize) THEN

        ! Auto size condenser air flow to Total Capacity * 0.000114 m3/s/w (850 cfm/ton)
        VRF(VRFCond)%EvapCondAirVolFlowRate = VRF(VRFCond)%CoolingCapacity*0.000114d0

        CALL ReportSizingOutput(cVRFTypes(VRF(VRFCond)%VRFSystemTypeNum), VRF(VRFCond)%Name, &
              'Evaporative Condenser Air Flow Rate [m3/s]', VRF(VRFCond)%EvapCondAirVolFlowRate)

      END IF

      IF (VRF(VRFCond)%EvapCondPumpPower == AutoSize) THEN

        ! Auto size evap condenser pump power to Total Capacity * 0.004266 w/w (15 w/ton)
        VRF(VRFCond)%EvapCondPumpPower = VRF(VRFCond)%CoolingCapacity*0.004266d0

        CALL ReportSizingOutput(cVRFTypes(VRF(VRFCond)%VRFSystemTypeNum), VRF(VRFCond)%Name, &
                           'Evaporative Condenser Pump Rated Power Consumption [W]',   &
                            VRF(VRFCond)%EvapCondPumpPower)
      END IF

      ! Report to eio other information not related to autosizing
      IF(MyOneTimeEIOFlag)THEN
        WRITE(OutputFileInits, 990)
        MyOneTimeEIOFlag = .FALSE.
      END IF
      WRITE(OutputFileInits, 991) TRIM(cVRFTypes(VRF(VRFCond)%VRFSystemTypeNum)), TRIM(VRF(VRFCond)%Name), &
        TRIM(RoundSigDigits(VRF(VRFCond)%CoolingCombinationRatio,5)),  &
        TRIM(RoundSigDigits(VRF(VRFCond)%HeatingCombinationRatio,5)),  &
        TRIM(RoundSigDigits(VRF(VRFCond)%PipingCorrectionCooling,5)),  &
        TRIM(RoundSigDigits(VRF(VRFCond)%PipingCorrectionHeating,5))

      CheckVRFCombinationRatio(VRFCond) = .FALSE.
    END IF
  END IF

  990 FORMAT('! <VRF System Information>, VRF System Type, VRF System Name, ' &
                'VRF System Cooling Combination Ratio, VRF System Heating Combination Ratio, ',  &
        'VRF System Cooling Piping Correction Factor, VRF System Heating Piping Correction Factor')
  991 FORMAT(' VRF System Information',6(', ',A))

  RETURN

END SUBROUTINE SizeVRF

! End Initialization Section of the Module
!******************************************************************************


! Begin Algorithm Section of the Module
!******************************************************************************
SUBROUTINE SimVRF(VRFTUNum, FirstHVACIteration, OnOffAirFlowRatio, SysOutputProvided, LatOutputProvided, QZnReq)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Raustad, FSEC
          !       DATE WRITTEN   August 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine simulates the VRF TU's.

          ! METHODOLOGY EMPLOYED:
          ! Simulate terminal unit to meet zone load.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: VRFTUNum
  LOGICAL, INTENT(IN) :: FirstHVACIteration
  REAL(r64), INTENT(InOut) :: OnOffAirFlowRatio
  REAL(r64), INTENT(InOut) :: SysOutputProvided
  REAL(r64), INTENT(InOut) :: LatOutputProvided
  REAL(r64), INTENT(IN)    :: QZnReq

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
REAL(r64) :: PartLoadRatio
!REAL(r64) :: QZnReq        ! cooling or heating output needed by zone [W]
!REAL(r64) :: LoadToCoolingSP
!REAL(r64) :: LoadToHeatingSP

!  QZnReq = ZoneSysEnergyDemand(ZoneNum)%RemainingOutputRequired
!  LoadToCoolingSP = ZoneSysEnergyDemand(ZoneNum)%OutputRequiredToCoolingSP
!  LoadToHeatingSP = ZoneSysEnergyDemand(ZoneNum)%OutputRequiredToHeatingSP
!  IF(QZnReq == 0.0d0 .AND. HeatingLoad(VRFTU(VRFTUNum)%VRFSysNum))QZnReq = LoadToHeatingSP
!  IF(QZnReq == 0.0d0 .AND. CoolingLoad(VRFTU(VRFTUNum)%VRFSysNum))QZnReq = LoadToCoolingSP
  CALL ControlVRF(VRFTUNum, QZnReq, FirstHVACIteration,PartLoadRatio, OnOffAirFlowRatio)
  CALL CalcVRF(VRFTUNum, FirstHVACIteration, PartLoadRatio, SysOutputProvided, OnOffAirFlowRatio, LatOutputProvided)
  VRFTU(VRFTUNum)%TerminalUnitSensibleRate = SysOutputProvided
  VRFTU(VRFTUNum)%TerminalUnitLatentRate = LatOutputProvided

 RETURN
END SUBROUTINE SimVRF

SUBROUTINE ControlVRF(VRFTUNum,QZnReq,FirstHVACIteration,PartLoadRatio, OnOffAirFlowRatio)

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
  USE DataEnvironment,           ONLY: OutDryBulbTemp
  USE ScheduleManager,           ONLY: GetCurrentScheduleValue

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER,   INTENT (IN)    :: VRFTUNum           ! Index to VRF terminal unit
  REAL(r64), INTENT (IN)    :: QZnReq             ! Index to zone number
  LOGICAL,   INTENT (IN)    :: FirstHVACIteration ! flag for 1st HVAC iteration in the time step
  REAL(r64), INTENT (OUT)   :: PartLoadRatio      ! unit part load ratio
  REAL(r64), INTENT (INOUT) :: OnOffAirFlowRatio  ! ratio of compressor ON airflow to AVERAGE airflow over timestep

          ! SUBROUTINE PARAMETER DEFINITIONS:
          !
  INTEGER, PARAMETER   :: MaxIte   = 500          ! maximum number of iterations
  REAL(r64), PARAMETER :: MinPLF   = 0.0          ! minimum part load factor allowed

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64)          :: FullOutput    ! unit full output when compressor is operating [W]
  REAL(r64)          :: TempOutput    ! unit output when iteration limit exceeded [W]
  REAL(r64)          :: NoCompOutput  ! output when no active compressor [W]
  INTEGER            :: SolFla        ! Flag of RegulaFalsi solver
  REAL(r64), DIMENSION(6) :: Par      ! Parameters passed to RegulaFalsi
  CHARACTER(len=20)  :: IterNum       ! Max number of iterations for warning message
  REAL(r64)          :: OutsideDryBulbTemp ! Outside air temperature at external node height
  REAL(r64)          :: TempMinPLR    ! min PLR used in Regula Falsi call
  REAL(r64)          :: TempMaxPLR    ! max PLR used in Regula Falsi call
  LOGICAL            :: ContinueIter  ! used when convergence is an issue
  INTEGER            :: VRFCond       ! index to VRF condenser

  PartLoadRatio  = 0.d0
  LoopDXCoolCoilRTF = 0.d0
  LoopDXHeatCoilRTF = 0.d0
  VRFCond = VRFTU(VRFTUNum)%VRFSysNum

  IF(VRFTU(VRFTUNum)%VRFTUOAMixerOANodeNum .EQ. 0)THEN
    OutsideDryBulbTemp = OutDryBulbTemp
  ELSE
    OutsideDryBulbTemp = Node(VRFTU(VRFTUNum)%VRFTUOAMixerOANodeNum)%Temp
  END IF

  ! do nothing if TU is scheduled off
  IF (GetCurrentScheduleValue(VRFTU(VRFTUNum)%SchedPtr) .EQ. 0.0) RETURN

  ! Set EMS value for PLR and return
  IF (VRFTU(VRFTUNum)%EMSOverridePartLoadFrac) THEN
    PartLoadRatio = VRFTU(VRFTUNum)%EMSValueForPartLoadFrac
    RETURN
  ENDIF

  ! do nothing if TU has no load
  IF (QZnReq == 0.d0) RETURN

  ! Get result when DX coil is off
  CALL CalcVRF(VRFTUNum, FirstHVACIteration, 0.0d0, NoCompOutput, OnOffAirFlowRatio)

  IF(CoolingLoad(VRFTU(VRFTUNum)%VRFSysNum) .AND. NoCompOutput .LE. QZnReq)THEN
    PartLoadRatio = 0.0d0
    RETURN
  ELSE IF(HeatingLoad(VRFTU(VRFTUNum)%VRFSysNum) .AND. NoCompOutput .GE. QZnReq)THEN
    PartLoadRatio = 0.0d0
    RETURN
  ELSE
    ! Get full load result
    PartLoadRatio  = 1.0
    CALL CalcVRF(VRFTUNum, FirstHVACIteration, PartLoadRatio, FullOutput, OnOffAirFlowRatio)
    PartLoadRatio  = 0.0
  END IF

  IF (CoolingLoad(VRFCond)) THEN
  ! Since we are cooling, we expect FullOutput < NoCompOutput
  ! Check that this is the case; if not set PartLoadRatio = 0.0 (off) and return
 !   IF (FullOutput >= NoCompOutput) THEN
 !     PartLoadRatio = 0.0
 !     RETURN
 !   END IF
  ! If the QZnReq <= FullOutput the unit needs to run full out
    IF (QZnReq  <=  FullOutput .AND. QZnReq < 0.0d0) THEN
      PartLoadRatio = 1.0
      RETURN
    END IF
  ELSE
  ! Since we are heating, we expect FullOutput > NoCompOutput
  ! Check that this is the case; if not set PartLoadRatio = 0.0 (off)
    IF (QZnReq  >=  FullOutput .AND. QZnReq > 0.0d0) THEN
      PartLoadRatio = 1.0
      RETURN
    END IF
  END IF

  ! Calculate the part load fraction

  IF ((HeatingLoad(VRFCond) .AND. QZnReq < FullOutput) .OR. &
      (CoolingLoad(VRFCond) .AND. QZnReq > FullOutput)) THEN

    Par(1) = VRFTUNum
    Par(2)=0.0d0
    Par(4)=0.0d0
    IF (FirstHVACIteration) THEN
      Par(3) = 1.0
    ELSE
      Par(3) = 0.0
    END IF
!    Par(4) = OpMode
    Par(5) = QZnReq
    Par(6) = OnOffAirFlowRatio
    CALL SolveRegulaFalsi(0.001d0, MaxIte, SolFla, PartLoadRatio, PLRResidual,   &
                              0.0d0, 1.0d0, Par)
    IF (SolFla == -1) THEN
!     Very low loads may not converge quickly. Tighten PLR boundary and try again.
      TempMaxPLR = -0.1d0
      ContinueIter = .TRUE.
      DO WHILE(ContinueIter .AND. TempMaxPLR .LT. 1.0d0)
        TempMaxPLR = TempMaxPLR + 0.1d0
        CALL CalcVRF(VRFTUNum,FirstHVACIteration,TempMaxPLR,TempOutput,OnOffAirFlowRatio)
        IF(HeatingLoad(VRFCond) .AND. TempOutput .GT. QZnReq)ContinueIter = .FALSE.
        IF(CoolingLoad(VRFCond) .AND. TempOutput .LT. QZnReq)ContinueIter = .FALSE.
      END DO
      TempMinPLR = TempMaxPLR
      ContinueIter = .TRUE.
      DO WHILE(ContinueIter .AND. TempMinPLR .GT. 0.0d0)
        TempMaxPLR = TempMinPLR
        TempMinPLR = TempMinPLR - 0.01d0
        CALL CalcVRF(VRFTUNum,FirstHVACIteration,TempMinPLR,TempOutput,OnOffAirFlowRatio)
        IF(HeatingLoad(VRFCond) .AND. TempOutput .LT. QZnReq)ContinueIter = .FALSE.
        IF(CoolingLoad(VRFCond) .AND. TempOutput .GT. QZnReq)ContinueIter = .FALSE.
      END DO
      CALL SolveRegulaFalsi(0.001d0, MaxIte, SolFla, PartLoadRatio, PLRResidual,   &
                              TempMinPLR, TempMaxPLR, Par)
      IF (SolFla == -1) THEN
        IF (.NOT. FirstHVACIteration .AND. .NOT. WarmupFlag) THEN
          WRITE(IterNum,*) MaxIte
          IterNum=ADJUSTL(IterNum)
          Call ShowWarningError(TRIM(cVRFTUTypes(VRFTU(VRFTUNum)%VRFTUType_Num))//' "'//TRIM(VRFTU(VRFTUNum)%Name)//'"')
          CALL ShowContinueError(' Iteration limit exceeded calculating terminal unit part-load ratio, '// &
                                'maximum iterations = '//TRIM(IterNum))
          CALL ShowContinueErrorTimeStamp(' Part-load ratio returned = '//TRIM(RoundSigDigits(PartLoadRatio,3)))
          CALL CalcVRF(VRFTUNum,FirstHVACIteration,TempMinPLR,TempOutput,OnOffAirFlowRatio)
          CALL ShowContinueError(' Load requested = '//TRIM(TrimSigDigits(QZnReq,5))//', Load delivered = ' &
                                 //TRIM(TrimSigDigits(TempOutput,5)))
        END IF
      ELSE IF (SolFla == -2) THEN
        IF (.NOT. FirstHVACIteration) THEN
          Call ShowWarningError(TRIM(cVRFTUTypes(VRFTU(VRFTUNum)%VRFTUType_Num))//' "'//TRIM(VRFTU(VRFTUNum)%Name)//'"')
          CALL ShowContinueError('Terminal unit part-load ratio calculation failed: ' &
                           //'PLR limits of 0 to 1 exceeded')
          CALL ShowContinueError('Please fill out a bug report and forward to the EnergyPlus support group.')
          CALL ShowContinueErrorTimeStamp(' ')
          IF (WarmupFlag) CALL ShowContinueError ('Error occurred during warmup days.')
        END IF
        PartLoadRatio = MAX(MinPLF, ABS(QZnReq - NoCompOutput) / ABS(FullOutput - NoCompOutput))
      END IF
    ELSE IF (SolFla == -2) THEN
      IF (.NOT. FirstHVACIteration) THEN
        Call ShowWarningError(TRIM(cVRFTUTypes(VRFTU(VRFTUNum)%VRFTUType_Num))//' "'//TRIM(VRFTU(VRFTUNum)%Name)//'"')
        CALL ShowContinueError('Terminal unit part-load ratio calculation failed: ' &
                           //'PLR limits of 0 to 1 exceeded')
        CALL ShowContinueError('Please fill out a bug report and forward to the EnergyPlus support group.')
        CALL ShowContinueErrorTimeStamp(' ')
        IF (WarmupFlag) CALL ShowContinueError ('Error occurred during warmup days.')
      END IF
      PartLoadRatio = MAX(MinPLF, ABS(QZnReq - NoCompOutput) / ABS(FullOutput - NoCompOutput))
    END IF

  END IF

  RETURN
END SUBROUTINE ControlVRF

SUBROUTINE CalcVRF(VRFTUNum, FirstHVACIteration, PartLoadRatio, LoadMet, OnOffAirFlowRatio, LatOutputProvided)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Raustad
          !       DATE WRITTEN   July 2005
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Simulate the components making up the VRF terminal unit.

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
  USE DataEnvironment,           ONLY: OutDryBulbTemp
  USE DataSizing,                ONLY: Autosize
!  USE WatertoAirheatPumpSimple,  ONLY: SimWatertoAirHPSimple
  USE DataAirLoop,               ONLY: LoopDXCoilRTF

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER,   INTENT    (IN) :: VRFTUNum             ! Unit index in VRF terminal unit array
  LOGICAL,   INTENT    (IN) :: FirstHVACIteration   ! flag for 1st HVAC iteration in the time step
  REAL(r64), INTENT    (IN) :: PartLoadRatio        ! compressor part load fraction
  REAL(r64), INTENT   (OUT) :: LoadMet              ! load met by unit (W)
  REAL(r64), INTENT (INOUT) :: OnOffAirFlowRatio    ! ratio of ON air flow to average air flow
  REAL(r64), OPTIONAL, INTENT (INOUT) :: LatOutputProvided ! delivered latent capacity (W)

          ! SUBROUTINE PARAMETER DEFINITIONS:
  INTEGER, PARAMETER          :: MaxIte = 500       ! maximum number of iterations
  CHARACTER(len=*), PARAMETER :: Blank = ' '        ! subroutine argument when coil index is known

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na


          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER  :: VRFTUOutletNodeNum ! TU air outlet node
  INTEGER  :: VRFTUInletNodeNum  ! TU air inlet node
  REAL(r64):: AirMassFlow        ! total supply air mass flow [m3/s]
  REAL(r64):: MinHumRat          ! minimum humidity ratio for sensible capacity calculation (kg/kg)
  REAL(r64):: OutsideDryBulbTemp ! Outdoor air temperature at external node height
  INTEGER  :: OpMode             ! fan operating mode, CycFanCycCoil or ContFanCycCoil
  INTEGER  :: VRFCond            ! index to VRF condenser
  REAL(r64):: SpecHumOut         ! specific humidity ratio at outlet node
  REAL(r64):: SpecHumIn          ! specific humidity ratio at inlet node

          ! FLOW

  VRFCond = VRFTU(VRFTUNum)%VRFSysNum
  VRFTUOutletNodeNum = VRFTU(VRFTUNum)%VRFTUOutletNodeNum
  VRFTUInletNodeNum = VRFTU(VRFTUNum)%VRFTUInletNodeNum
  OpMode = VRFTU(VRFTUNum)%OpMode
  IF(VRFTU(VRFTUNum)%VRFTUOAMixerOANodeNum .EQ. 0)THEN
    OutsideDryBulbTemp = OutDryBulbTemp
  ELSE
    OutsideDryBulbTemp = Node(VRFTU(VRFTUNum)%VRFTUOAMixerOANodeNum)%Temp
  END IF

  ! Set inlet air mass flow rate based on PLR and compressor on/off air flow rates
  CALL SetAverageAirFlow(VRFTUNum, PartLoadRatio, OnOffAirFlowRatio)

  AirMassFlow = Node(VRFTUInletNodeNum)%MassFlowRate
  IF(VRFTU(VRFTUNum)%OAMixerUsed)CALL SimOAMixer(VRFTU(VRFTUNum)%OAMixerName,FirstHVACIteration,VRFTU(VRFTUNum)%OAMixerIndex)

  ! if blow through, simulate fan then coils
  IF (VRFTU(VRFTUNum)%FanPlace .EQ. BlowThru) THEN
    CALL SimulateFanComponents(' ',FirstHVACIteration,VRFTU(VRFTUNum)%FanIndex,FanSpeedRatio)
  END IF

  IF(VRFTU(VRFTUNum)%CoolingCoilPresent)THEN
    IF (CoolingLoad(VRFCond) .AND. OutsideDryBulbTemp .GE. VRF(VRFCond)%MinOATCooling .AND. &
        OutsideDryBulbTemp .LE. VRF(VRFCond)%MaxOATCooling)THEN
       CALL SimDXCoil(' ',On,FirstHVACIteration,PartLoadRatio,VRFTU(VRFTUNum)%CoolCoilIndex,  &
                    OpMode,OnOffAirFlowRatio, MaxCap=MaxCoolingCapacity(VRFCond), &
                    CompCyclingRatio = VRF(VRFTU(VRFTUNum)%VRFSysNum)%VRFCondCyclingRatio)
    ELSE ! cooling coil is off
       CALL SimDXCoil(' ',Off,FirstHVACIteration,0.0d0,  &
                    VRFTU(VRFTUNum)%CoolCoilIndex,OpMode,OnOffAirFlowRatio)
    END IF
    LoopDXCoolCoilRTF = LoopDXCoilRTF
  ELSE
    LoopDXCoolCoilRTF = 0.d0
  END IF

  IF(VRFTU(VRFTUNum)%HeatingCoilPresent)THEN
    IF (HeatingLoad(VRFCond) .AND. OutsideDryBulbTemp .GE. VRF(VRFCond)%MinOATHeating .AND. &
        OutsideDryBulbTemp .LE. VRF(VRFCond)%MaxOATHeating)THEN
       CALL SimDXCoil(' ',Off,FirstHVACIteration,PartLoadRatio,  &
                    VRFTU(VRFTUNum)%HeatCoilIndex,OpMode,OnOffAirFlowRatio, &
                    MaxCap=MaxHeatingCapacity(VRFCond))
    ELSE
       CALL SimDXCoil(' ',Off,FirstHVACIteration,0.0d0,  &
                    VRFTU(VRFTUNum)%HeatCoilIndex,OpMode,OnOffAirFlowRatio)
    END IF
    LoopDXHeatCoilRTF = LoopDXCoilRTF
  ELSE
    LoopDXHeatCoilRTF = 0.d0
  END IF

  ! if draw through, simulate coils then fan
  IF (VRFTU(VRFTUNum)%FanPlace .EQ. DrawThru) THEN
    CALL SimulateFanComponents(' ',FirstHVACIteration,VRFTU(VRFTUNum)%FanIndex,FanSpeedRatio)
  END IF

! calculate sensible load met using delta enthalpy at a constant (minimum) humidity ratio
  MinHumRat = MIN(Node(VRFTUInletNodeNum)%HumRat,Node(VRFTUOutletNodeNum)%HumRat)
  LoadMet   = AirMassFlow * (PsyHFnTdbW(Node(VRFTUOutletNodeNum)%Temp,MinHumRat) - &
                             PsyHFnTdbW(Node(VRFTUInletNodeNum)%Temp,MinHumRat))

  IF(PRESENT(LatOutputProvided))THEN
    SpecHumOut = Node(VRFTUOutletNodeNum)%HumRat / (1.0d0 + Node(VRFTUOutletNodeNum)%HumRat)
    SpecHumIn  = Node(VRFTUInletNodeNum)%HumRat / (1.0d0 + Node(VRFTUInletNodeNum)%HumRat)
    LatOutputProvided = AirMassFlow * (SpecHumOut - SpecHumIn) ! Latent rate, kg/s (dehumid = negative)
  END IF

RETURN
END SUBROUTINE CalcVRF

! End Algorithm Section of the Module
! *****************************************************************************

! Beginning of Update subroutines
! *****************************************************************************

!SUBROUTINE UpdateVRF()
!
!          ! SUBROUTINE INFORMATION:
!          !       AUTHOR         Richard Raustad, FSEC
!          !       DATE WRITTEN   August 2010
!          !       MODIFIED       na
!          !       RE-ENGINEERED  na
!
!          ! PURPOSE OF THIS SUBROUTINE:
!          ! This subroutine updates the fan outlet nodes.
!
!          ! METHODOLOGY EMPLOYED:
!          ! Data is moved from the fan data structure to the fan outlet nodes.
!
!          ! REFERENCES:
!          ! na
!
!          ! USE STATEMENTS:
!          ! na
!
!  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine
!
!          ! SUBROUTINE ARGUMENT DEFINITIONS:
!          ! na
!
!          ! SUBROUTINE PARAMETER DEFINITIONS:
!          ! na
!
!          ! INTERFACE BLOCK SPECIFICATIONS
!          ! na
!
!          ! DERIVED TYPE DEFINITIONS
!          ! na
!
!          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
!          ! na
!
!
!  RETURN
!END Subroutine UpdateVRF

!        End of Update subroutines for the Fan Module
! *****************************************************************************


! Beginning of Reporting subroutines
! *****************************************************************************

SUBROUTINE ReportVRFTerminalUnit(VRFTUNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Raustad, FSEC
          !       DATE WRITTEN   August 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine updates the report variables for the VRF Terminal Units.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DXCoils,        ONLY: DXCoilTotalCooling, DXCoilTotalHeating
  USE DataHVACGlobals, ONLY: TimeStepSys

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: VRFTUNum  ! index to VRF terminal unit

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER   :: DXCoolingCoilIndex   !- index to DX cooling coil
  INTEGER   :: DXHeatingCoilIndex   !- index to DX heating coil
  REAL(r64) :: TotalConditioning    !- sum of sensible and latent rates
  REAL(r64) :: SensibleConditioning !- sensible rate
  REAL(r64) :: LatentConditioning   !- latent rate
  REAL(r64) :: ReportingConstant    !- used to convert watts to joules
  INTEGER   :: VRFCond              !- index to VRF condenser

  DXCoolingCoilIndex = VRFTU(VRFTUNum)%CoolCoilIndex
  DXHeatingCoilIndex = VRFTU(VRFTUNum)%HeatCoilIndex
  VRFCond = VRFTU(VRFTUNum)%VRFSysNum
  ReportingConstant = TimeStepSys*SecInHour

  ! account for terminal unit parasitic On/Off power use
  IF(CoolingLoad(VRFCond) .OR. (.NOT. HeatingLoad(VRFCond) .AND. LastModeCooling(VRFTU(VRFTUNum)%VRFSysNum)))THEN
    IF(VRFTU(VRFTUNum)%CoolingCoilPresent)THEN
      VRFTU(VRFTUNum)%ParasiticCoolElecPower = VRFTU(VRFTUNum)%ParasiticElec * LoopDXCoolCoilRTF + &
                                               VRFTU(VRFTUNum)%ParasiticOffElec * (1.d0 - LoopDXCoolCoilRTF)
      VRFTU(VRFTUNum)%ParasiticElecCoolConsumption = VRFTU(VRFTUNum)%ParasiticCoolElecPower*ReportingConstant
      VRFTU(VRFTUNum)%ParasiticHeatElecPower       = 0.d0
      VRFTU(VRFTUNum)%ParasiticElecHeatConsumption = 0.d0
    ELSE
      ! cooling parasitic power report variable is not even available when there is no cooling coil, report for heating
      VRFTU(VRFTUNum)%ParasiticHeatElecPower = VRFTU(VRFTUNum)%ParasiticOffElec
      VRFTU(VRFTUNum)%ParasiticElecHeatConsumption = VRFTU(VRFTUNum)%ParasiticHeatElecPower*ReportingConstant
    END IF
  ELSE IF(HeatingLoad(VRFCond) .OR. (.NOT. CoolingLoad(VRFCond) .AND. LastModeHeating(VRFTU(VRFTUNum)%VRFSysNum)))THEN
    IF(VRFTU(VRFTUNum)%HeatingCoilPresent)THEN
      VRFTU(VRFTUNum)%ParasiticCoolElecPower       = 0.d0
      VRFTU(VRFTUNum)%ParasiticElecCoolConsumption = 0.d0
      VRFTU(VRFTUNum)%ParasiticHeatElecPower = VRFTU(VRFTUNum)%ParasiticElec * LoopDXHeatCoilRTF + &
                                               VRFTU(VRFTUNum)%ParasiticOffElec * (1.d0 - LoopDXHeatCoilRTF)
      VRFTU(VRFTUNum)%ParasiticElecHeatConsumption = VRFTU(VRFTUNum)%ParasiticHeatElecPower*ReportingConstant
    ELSE
      ! heating parasitic power report variable is not even available when there is no heating coil, report for cooling
      VRFTU(VRFTUNum)%ParasiticCoolElecPower = VRFTU(VRFTUNum)%ParasiticOffElec
      VRFTU(VRFTUNum)%ParasiticElecCoolConsumption = VRFTU(VRFTUNum)%ParasiticCoolElecPower*ReportingConstant
    END IF
  ELSE
    ! happens when there is no cooling or heating load
    IF(.NOT. VRFTU(VRFTUNum)%CoolingCoilPresent)THEN
     ! report all for heating
      VRFTU(VRFTUNum)%ParasiticHeatElecPower = VRFTU(VRFTUNum)%ParasiticOffElec
      VRFTU(VRFTUNum)%ParasiticElecHeatConsumption = VRFTU(VRFTUNum)%ParasiticHeatElecPower*ReportingConstant
    ELSE IF(.NOT. VRFTU(VRFTUNum)%HeatingCoilPresent)THEN
     ! report all for cooling
      VRFTU(VRFTUNum)%ParasiticCoolElecPower = VRFTU(VRFTUNum)%ParasiticOffElec
      VRFTU(VRFTUNum)%ParasiticElecCoolConsumption = VRFTU(VRFTUNum)%ParasiticCoolElecPower*ReportingConstant
    ELSE
      ! split parasitic between both reporting variables
      VRFTU(VRFTUNum)%ParasiticCoolElecPower = VRFTU(VRFTUNum)%ParasiticOffElec / 2.d0
      VRFTU(VRFTUNum)%ParasiticElecCoolConsumption = VRFTU(VRFTUNum)%ParasiticCoolElecPower*ReportingConstant
      VRFTU(VRFTUNum)%ParasiticHeatElecPower = VRFTU(VRFTUNum)%ParasiticOffElec / 2.d0
      VRFTU(VRFTUNum)%ParasiticElecHeatConsumption = VRFTU(VRFTUNum)%ParasiticHeatElecPower*ReportingConstant
    END IF
  END IF

  SensibleConditioning = VRFTU(VRFTUNum)%TerminalUnitSensibleRate
  LatentConditioning   = VRFTU(VRFTUNum)%TerminalUnitLatentRate
  TotalConditioning    = SensibleConditioning + LatentConditioning

  IF(TotalConditioning .LE. 0.d0)THEN
    VRFTU(VRFTUNum)%TotalCoolingRate = ABS(TotalConditioning)
    VRFTU(VRFTUNum)%TotalHeatingRate = 0.d0
  ELSE
    VRFTU(VRFTUNum)%TotalCoolingRate = 0.d0
    VRFTU(VRFTUNum)%TotalHeatingRate = TotalConditioning
  END IF
  IF(SensibleConditioning .LE. 0.d0)THEN
    VRFTU(VRFTUNum)%SensibleCoolingRate = ABS(SensibleConditioning)
    VRFTU(VRFTUNum)%SensibleHeatingRate = 0.d0
  ELSE
    VRFTU(VRFTUNum)%SensibleCoolingRate = 0.d0
    VRFTU(VRFTUNum)%SensibleHeatingRate = SensibleConditioning
  END IF
  IF(LatentConditioning .LE. 0.d0)THEN
    VRFTU(VRFTUNum)%LatentCoolingRate = ABS(LatentConditioning)
    VRFTU(VRFTUNum)%LatentHeatingRate = 0.d0
  ELSE
    VRFTU(VRFTUNum)%LatentCoolingRate = 0.d0
    VRFTU(VRFTUNum)%LatentHeatingRate = LatentConditioning
  END IF
  VRFTU(VRFTUNum)%TotalCoolingEnergy = VRFTU(VRFTUNum)%TotalCoolingRate * ReportingConstant
  VRFTU(VRFTUNum)%SensibleCoolingEnergy = VRFTU(VRFTUNum)%SensibleCoolingRate * ReportingConstant
  VRFTU(VRFTUNum)%LatentCoolingEnergy = VRFTU(VRFTUNum)%LatentCoolingRate * ReportingConstant
  VRFTU(VRFTUNum)%TotalHeatingEnergy = VRFTU(VRFTUNum)%TotalHeatingRate * ReportingConstant
  VRFTU(VRFTUNum)%SensibleHeatingEnergy = VRFTU(VRFTUNum)%SensibleHeatingRate * ReportingConstant
  VRFTU(VRFTUNum)%LatentHeatingEnergy = VRFTU(VRFTUNum)%LatentHeatingRate * ReportingConstant

  RETURN
END SUBROUTINE ReportVRFTerminalUnit

SUBROUTINE ReportVRFCondenser(VRFCond)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Raustad, FSEC
          !       DATE WRITTEN   August 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine updates the report variables for the VRF Condenser.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: VRFCond  ! index to VRF condensing unit

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    REAL(r64) :: ReportingConstant !- conversion constant for energy

    ReportingConstant = TimeStepSys * SecInHour

!   calculate VRF condenser power/energy use
    VRF(VRFCond)%CoolElecConsumption = VRF(VRFCond)%ElecCoolingPower * ReportingConstant
    VRF(VRFCond)%HeatElecConsumption = VRF(VRFCond)%ElecHeatingPower * ReportingConstant

    VRF(VRFCond)%DefrostConsumption     = VRF(VRFCond)%DefrostPower * ReportingConstant
    VRF(VRFCond)%BasinHeaterConsumption = VRF(VRFCond)%BasinHeaterPower * ReportingConstant

    VRF(VRFCond)%EvapCondPumpElecConsumption    = VRF(VRFCond)%EvapCondPumpElecPower * ReportingConstant
    VRF(VRFCond)%CrankCaseHeaterElecConsumption = VRF(VRFCond)%CrankCaseHeaterPower * ReportingConstant

  RETURN
END SUBROUTINE ReportVRFCondenser

!        End of Reporting subroutines for the Module
! *****************************************************************************

! Utility subroutines for the Module
REAL(r64) FUNCTION PLRResidual(PartLoadRatio,Par)
          ! FUNCTION INFORMATION:
          !       AUTHOR         Richard Raustad, FSEC
          !       DATE WRITTEN   August 2010
          !       MODIFIED
          !       RE-ENGINEERED

          ! PURPOSE OF THIS FUNCTION:
          !  Calculates residual function ((ActualOutput - QZnReq)/QZnReq)
          !  VRF TU output depends on the part load ratio which is being varied to zero the residual.

          ! METHODOLOGY EMPLOYED:
          !  Calls CalcVRF to get ActualOutput at the given part load ratio
          !  and calculates the residual as defined above

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  REAL(r64), INTENT(IN)     :: PartLoadRatio ! compressor cycling ratio (1.0 is continuous, 0.0 is off)
  REAL(r64), INTENT(IN), DIMENSION(:), OPTIONAL :: Par ! par(1) = VRFTUNum
                                                       ! par(2) = Not used
                                                       ! par(3) = FirstHVACIteration
                                                       ! par(4) = OpMode
                                                       ! par(5) = QZnReq
                                                       ! par(6) = OnOffAirFlowRatio

          ! FUNCTION PARAMETER DEFINITIONS:
          !  na

          ! INTERFACE BLOCK SPECIFICATIONS
          !  na

          ! DERIVED TYPE DEFINITIONS
          !  na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER   :: VRFTUNum            ! TU index
  LOGICAL   :: FirstHVACIteration  ! FirstHVACIteration flag
  INTEGER   :: OpMode              ! Compressor operating mode
  REAL(r64) :: QZnReq              ! zone load (W)
  REAL(r64) :: OnOffAirFlowRatio   ! ratio of compressor ON airflow to average airflow over timestep
  REAL(r64) :: ActualOutput        ! delivered capacity of VRF terminal unit

  VRFTUNum = INT(Par(1))
  ! FirstHVACIteration is a logical, Par is real, so make 1.0=TRUE and 0.0=FALSE
  IF(Par(3) .EQ. 1.d0)THEN
    FirstHVACIteration = .TRUE.
  ELSE
    FirstHVACIteration = .FALSE.
  END IF
  OpMode = INT(Par(4))
  QZnReq = Par(5)
  OnOffAirFlowRatio = Par(6)

  CALL CalcVRF(VRFTUNum,FirstHVACIteration,PartLoadRatio,ActualOutput,OnOffAirFlowRatio)
  PLRResidual = (ActualOutput - QZnReq)/QZnReq

  RETURN
END FUNCTION PLRResidual

SUBROUTINE SetAverageAirFlow(VRFTUNum,PartLoadRatio,OnOffAirFlowRatio)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Raustad
          !       DATE WRITTEN   August 2010
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
  USE ScheduleManager,   ONLY: GetCurrentScheduleValue

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER,   INTENT (IN)    :: VRFTUNum           ! Unit index
  REAL(r64), INTENT (IN)    :: PartLoadRatio      ! unit part load ratio
  REAL(r64), INTENT (INOUT) :: OnOffAirFlowRatio  ! ratio of compressor ON airflow to average airflow over timestep

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER             :: InletNode           ! inlet node number
  INTEGER             :: OutsideAirNode      ! outside air node number
  INTEGER             :: AirRelNode          ! relief air node number
  REAL(r64)           :: AverageUnitMassFlow ! average supply air mass flow rate over time step
  REAL(r64)           :: AverageOAMassFlow   ! average outdoor air mass flow rate over time step

  InletNode      = VRFTU(VRFTUNum)%VRFTUInletNodeNum
  OutsideAirNode = VRFTU(VRFTUNum)%VRFTUOAMixerOANodeNum
  AirRelNode     = VRFTU(VRFTUNum)%VRFTUOAMixerRelNodeNum

  IF(VRFTU(VRFTUNum)%OpMode .EQ. CycFanCycCoil)THEN
    AverageUnitMassFlow = (PartLoadRatio * CompOnMassFlow) + ((1-PartLoadRatio) * CompOffMassFlow)
    AverageOAMassFlow   = (PartLoadRatio * OACompOnMassFlow) + ((1-PartLoadRatio) * OACompOffMassFlow)
  ELSE
    AverageUnitMassFlow = CompOnMassFlow
    AverageOAMassFlow   = OACompOnMassFlow
  END IF
  IF(CompOffFlowRatio .GT. 0.d0)THEN
    FanSpeedRatio     = (PartLoadRatio * CompOnFlowRatio) + ((1-PartLoadRatio) * CompOffFlowRatio)
  ELSE
    FanSpeedRatio     = CompOnFlowRatio
  END IF

  ! if the terminal unit and fan are scheduled on then set flow rate
  IF ( GetCurrentScheduleValue(VRFTU(VRFTUNum)%SchedPtr) .GT. 0.d0 .AND. &
       GetCurrentScheduleValue(VRFTU(VRFTUNum)%FanAvailSchedPtr) .GT. 0.d0)THEN

    Node(InletNode)%MassFlowRate              = AverageUnitMassFlow
    Node(InletNode)%MassFlowRateMaxAvail      = AverageUnitMassFlow
    IF(OutsideAirNode .GT. 0)THEN
      Node(OutsideAirNode)%MassFlowRate         = AverageOAMassFlow
      Node(OutsideAirNode)%MassFlowRateMaxAvail = AverageOAMassFlow
      Node(AirRelNode)%MassFlowRate             = AverageOAMassFlow
      Node(AirRelNode)%MassFlowRateMaxAvail     = AverageOAMassFlow
    END IF
    IF (AverageUnitMassFlow .GT. 0.d0) THEN
      OnOffAirFlowRatio                       = CompOnMassFlow / AverageUnitMassFlow
    ELSE
      OnOffAirFlowRatio                       = 0.d0
    END IF

  ELSE ! terminal unit and/or fan is off

    Node(InletNode)%MassFlowRate              = 0.d0
    IF(OutsideAirNode .GT. 0)THEN
      Node(OutsideAirNode)%MassFlowRate       = 0.d0
      Node(AirRelNode)%MassFlowRate           = 0.d0
    END IF
    OnOffAirFlowRatio                         = 0.d0

  END IF

  RETURN
END SUBROUTINE SetAverageAirFlow

! End of Utility subroutines for the Module
! *****************************************************************************

!     NOTICE
!
!     Copyright © 1996-2012 The Board of Trustees of the University of Illinois
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

End Module HVACVariableRefrigerantFlow


