Module HVACVariableRefrigerantFlow
  ! Module containing the Variable Refrigerant Flow (VRF or VRV) simulation routines

  ! MODULE INFORMATION:
  !       AUTHOR         Richard Raustad, FSEC
  !       DATE WRITTEN   August 2010
  !       MODIFIED       FSEC - Raustad, Added Heat Recovery Operating Mode, April 2012
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
USE DataPlant

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

INTEGER, PARAMETER :: NumValidFuelTypes=9
CHARACTER(len=*), PARAMETER, DIMENSION(NumValidFuelTypes) :: cValidFuelTypes=    &
                 (/'Electric  ',  &
                   'NaturalGas',  &
                   'PropaneGas',  &
                   'Diesel    ',  &
                   'Gasoline  ',  &
                   'FuelOil#1 ',  &
                   'FuelOil#2 ',  &
                   'OtherFuel1', &
                   'OtherFuel2'/)

! Fuel Types
INTEGER, PARAMETER :: FuelTypeElectric    = 1     ! Fuel type for electricity
INTEGER, PARAMETER :: FuelTypeNaturalGas  = 2     ! Fuel type for natural gas
INTEGER, PARAMETER :: FuelTypePropaneGas  = 3     ! Fuel type for propane gas
INTEGER, PARAMETER :: FuelTypeDiesel      = 4     ! Fuel type for diesel
INTEGER, PARAMETER :: FuelTypeGasoline    = 5     ! Fuel type for gasoline
INTEGER, PARAMETER :: FuelTypeFuelOil1    = 6     ! Fuel type for fuel oil #1
INTEGER, PARAMETER :: FuelTypeFuelOil2    = 7     ! Fuel type for fuel oil #2
INTEGER, PARAMETER :: FuelTypeOtherFuel1  = 8     ! Fuel type for other fuel #1
INTEGER, PARAMETER :: FuelTypeOtherFuel2  = 9     ! Fuel type for other fuel #2

! curve type for equivalent piping losses (not necessarily the same value used in CurveManager)
INTEGER, PARAMETER :: BiQuadratic     = 4

  ! DERIVED TYPE DEFINITIONS
TYPE VRFCondenserEquipment
  CHARACTER(len=MaxNameLength) :: Name       =' ' ! Name of the VRF Terminal Unit
  INTEGER      :: VRFSystemTypeNum           =0   ! integer equivalent of system type
  INTEGER      :: VRFPlantTypeOfNum          =0   ! integer equivalent of index to DataPlant type
  INTEGER      :: SourceLoopNum              =0   ! plant data for water-coole only
  INTEGER      :: SourceLoopSideNum          =0   ! plant data for water-coole only
  INTEGER      :: SourceBranchNum            =0   ! plant data for water-coole only
  INTEGER      :: SourceCompNum              =0   ! plant data for water-coole only
  REAL(r64)    :: WaterCondenserDesignMassFlow =0.0d0 ! plant data for water-coole only
  REAL(r64)    :: WaterCondenserMassFlow     =0.0d0 ! Water condenser flow rate (kg/s)
  REAL(r64)    :: QCondenser                 =0.0d0 ! Water condenser heat rejection/absorption (W)
  REAL(r64)    :: QCondEnergy                =0.0d0 ! Water condenser heat rejection/aborption energy (J)
  REAL(r64)    :: CondenserSideOutletTemp    =0.0d0 ! Water condenser outlet temp (C)
  INTEGER      :: SchedPtr                   =-1  ! Pointer to the correct schedule
  REAL(r64)    :: CoolingCapacity            =0.0d0 ! Nominal VRF heat pump cooling capacity (W)
  REAL(r64)    :: TotalCoolingCapacity       =0.0d0 ! Nominal VRF heat pump cooling capacity (W)
  REAL(r64)    :: CoolingCombinationRatio    =1.d0 ! Ratio or terminal unit cooling capacity to VRF condenser capacity
  REAL(r64)    :: VRFCondPLR                 =0.0d0 ! Condenser part-load ratio wrt total capacity
  REAL(r64)    :: VRFCondRTF                 =0.0d0 ! Condenser runtime fraction
  REAL(r64)    :: VRFCondCyclingRatio        =0.0d0 ! Condenser cycling ratio below MinPLR
  REAL(r64)    :: CondenserInletTemp         =0.0d0 ! Condenser entering air temperature (C)
  REAL(r64)    :: CoolingCOP                 =0.0d0 ! Nominal VRF heat pump cooling COP (W/W)
  REAL(r64)    :: OperatingCoolingCOP        =0.0d0 ! Operating VRF heat pump cooling COP (W/W)
  REAL(r64)    :: RatedCoolingPower          =0.0d0 ! Rated cooling power = Rated Cooling Capacity / Rated COP (W)
  REAL(r64)    :: HeatingCapacity            =0.0d0 ! Nominal VRF heat pump heating capacity (W)
  REAL(r64)    :: HeatingCapacitySizeRatio   =1.d0 ! Ratio of heating to cooling when autosizing
  LOGICAL      :: LockHeatingCapacity        =.FALSE. ! used in sizing to size VRF heat cap to VRF cool cap
  REAL(r64)    :: TotalHeatingCapacity       =0.0d0 ! Nominal VRF heat pump heating capacity (W)
  REAL(r64)    :: HeatingCombinationRatio    =1.d0 ! Ratio or terminal unit heating capacity to VRF condenser capacity
  REAL(r64)    :: HeatingCOP                 =0.0d0 ! Nominal VRF heat pump heating COP
  REAL(r64)    :: OperatingHeatingCOP        =0.0d0 ! Operating VRF heat pump heating COP
  REAL(r64)    :: RatedHeatingPower          =0.0d0 ! Rated heating power = Rated Heating Capacity / Rated COP (W)
  REAL(r64)    :: MinOATCooling              =0.0d0 ! Minimum outdoor air dry-bulb temp in cooling mode (C)
  REAL(r64)    :: MaxOATCooling              =0.0d0 ! Maximum outdoor air dry-bulb temp in cooling mode (C)
  REAL(r64)    :: MinOATHeating              =0.0d0 ! Minimum outdoor air dry-bulb temp in heating mode (C)
  REAL(r64)    :: MaxOATHeating              =0.0d0 ! Maximum outdoor air dry-bulb temp in heating mode (C)
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
  REAL(r64)    :: MinPLR                     =0.0d0 ! minimum PLR before cycling occurs
  INTEGER      :: MasterZonePtr              =0   ! index to master thermostat zone
  INTEGER      :: MasterZoneTUIndex          =0   ! index to TU in master thermostat zone
  INTEGER      :: ThermostatPriority         =0   ! VRF priority control (1=LoadPriority, 2=ZonePriority, etc)
  INTEGER      :: SchedPriorityPtr           =0   ! VRF priority control schedule pointer
  INTEGER      :: ZoneTUListPtr              =0   ! index to zone terminal unit list
  LOGICAL      :: HeatRecoveryUsed           =.FALSE. ! .TRUE. = heat recovery used
  REAL(r64)    :: VertPipeLngth              =0.0d0 ! vertical piping length (m)
  INTEGER      :: PCFLengthCoolPtr           =0   ! piping correction factor for length in cooling mode curve index
  INTEGER      :: PCFLengthCoolPtrType       =0   ! PCF for length curve type
  REAL(r64)    :: PCFHeightCool              =0.0d0 ! piping correction factor for height in cooling mode
  REAL(r64)    :: EquivPipeLngthCool         =0.0d0 ! equivalent piping length for cooling
  REAL(r64)    :: PipingCorrectionCooling    =1.d0 ! piping correction factor for cooling
  INTEGER      :: PCFLengthHeatPtr           =0   ! piping correction factor for length in heating mode curve index
  INTEGER      :: PCFLengthHeatPtrType       =0   ! PCF for length curve type
  REAL(r64)    :: PCFHeightHeat              =0.0d0 ! piping correction factor for height in heating mode
  REAL(r64)    :: EquivPipeLngthHeat         =0.0d0 ! equivalent piping length for heating
  REAL(r64)    :: PipingCorrectionHeating    =1.d0 ! piping correction factor for heating
  REAL(r64)    :: CCHeaterPower              =0.0d0 ! crankcase heater power per compressor (W)
  REAL(r64)    :: CompressorSizeRatio        =0.0d0 ! ratio of min compressor size to total capacity
  INTEGER      :: NumCompressors             =0   ! number of compressors in VRF condenser
  REAL(r64)    :: MaxOATCCHeater             =0.0d0 ! maximum outdoor air dry-bulb temp for crankcase heater operation (C)
  INTEGER      :: DefrostEIRPtr              =0   ! index to defrost EIR curve
  REAL(r64)    :: DefrostFraction            =0.0d0 ! defrost time period fraction (hr)
  INTEGER      :: DefrostStrategy            =0   ! Type of defrost (reversecycle or resistive)
  INTEGER      :: DefrostControl             =0   ! type of defrost control (timed or ondemand)
  REAL(r64)    :: DefrostCapacity            =0.0d0 ! capacity of resistive defrost heating element (W)
  REAL(r64)    :: DefrostPower               =0.0d0 ! power used during defrost (W)
  REAL(r64)    :: DefrostConsumption         =0.0d0 ! energy used during defrost (J)
  REAL(r64)    :: MaxOATDefrost              =0.0d0 ! maximum outdoor air dry-bulb temp for defrost operation (C)
  INTEGER      :: CondenserType              =0   ! condenser type, evap- or air-cooled
  INTEGER      :: CondenserNodeNum           =0   ! condenser inlet node number
  LOGICAL      :: SkipCondenserNodeNumCheck  =.FALSE. ! used to check for duplicate node names
  INTEGER      :: CondenserOutletNodeNum     =0   ! condenser outlet node number
  REAL(r64)    :: WaterCondVolFlowRate       =0.0d0 ! water condenser volume flow rate (m3/s)
  REAL(r64)    :: EvapCondEffectiveness      =0.0d0 ! evaporative condenser effectiveness
  REAL(r64)    :: EvapCondAirVolFlowRate     =0.0d0 ! air volume flow rate through condenser (m3/s)
  REAL(r64)    :: EvapCondPumpPower          =0.0d0 ! evaporative condenser water pump power (W)
  INTEGER      :: CoolCombRatioPTR           = 0  ! index to cooling combination ratio curve pointer
  INTEGER      :: HeatCombRatioPTR           = 0  ! index to heating combination ratio curve pointer
  INTEGER      :: OperatingMode              = 0  ! VRF Condenser operating mode, 0=off, 1=cooling, 2=heating, 3=HR
  REAL(r64)    :: ElecPower                  =0.0d0 ! VRF Condenser power (W)
  REAL(r64)    :: ElecCoolingPower           =0.0d0 ! VRF Condenser power in cooling mode (W)
  REAL(r64)    :: ElecHeatingPower           =0.0d0 ! VRF Condenser power in heating mode (W)
  REAL(r64)    :: CoolElecConsumption        =0.0d0 ! VRF Condenser cooling energy (J)
  REAL(r64)    :: HeatElecConsumption        =0.0d0 ! VRF Condenser heating energy (J)
  REAL(r64)    :: CrankCaseHeaterPower       =0.0d0 ! VRF Condenser crankcase heater power (W)
  REAL(r64)    :: CrankCaseHeaterElecConsumption =0.0d0 ! VRF Condenser crankcase heater energy (J)
  REAL(r64)    :: EvapCondPumpElecPower      =0.0d0 ! VRF Condenser evaporatively cooled condenser pump power (W)
  REAL(r64)    :: EvapCondPumpElecConsumption=0.0d0 ! VRF Condenser evaporatively cooled condenser pump elec consumption (J)
  REAL(R64)    :: EvapWaterConsumpRate       =0.0d0 ! VRF Condenser evaporatively cooled condenser water consumption (m3/s)
  INTEGER      :: HRMaxTempLimitIndex        =0   ! Warning message recurring error index
  INTEGER      :: CoolingMaxTempLimitIndex   =0   ! Warning message recurring error index
  INTEGER      :: HeatingMaxTempLimitIndex   =0   ! Warning message recurring error index
  INTEGER      :: FuelType                   =0   ! Fuel type
  REAL(r64)    :: SUMultiplier               =0.0d0 ! exponential timer for mode changes
  REAL(r64)    :: TUCoolingLoad              =0.0d0 ! total TU cooling load for each VRF system
  REAL(r64)    :: TUHeatingLoad              =0.0d0 ! total TU heating load for each VRF system
  LOGICAL      :: SwitchedMode               = .FALSE. ! used to derate capacity/power when system changes operating mode

  ! begin variables used for heat recovery mode
  REAL(r64)    :: OperatingCOP               =0.0d0 ! Operating VRF heat pump COP (total TU capacity/total power)
  REAL(r64)    :: MinOATHeatRecovery         =0.0d0 ! Minimum outdoor air temperature for heat recovery operation (C)
  REAL(r64)    :: MaxOATHeatRecovery         =0.0d0 ! Maximum outdoor air temperature for heat recovery operation (C)
  INTEGER      :: HRCAPFTCool                =0   ! Index to cool capacity as a function of temperature curve for heat recovery
  REAL(r64)    :: HRCAPFTCoolConst           =0.9d0 ! constant used if curve is blank
  INTEGER      :: HRCAPFTCoolType            =0   ! Curve type for HRCAPFTCool
  REAL(r64)    :: HRInitialCoolCapFrac       =0.5d0 ! Fractional cooling degradation at the start of heat recovery from cooling mode
  REAL(r64)    :: HRCoolCapTC                =0.15d0 ! Time constant used to recover from intial degratation in cooling heat recovery
  INTEGER      :: HREIRFTCool                =0   ! Index to cool EIR as a function of temperature curve for heat recovery
  REAL(r64)    :: HREIRFTCoolConst           =1.1d0 ! constant used if curve is blank
  INTEGER      :: HREIRFTCoolType            =0   ! Curve type for HREIRFTCool
  REAL(r64)    :: HRInitialCoolEIRFrac       =1.d0 ! Fractional EIR degradation at the start of heat recovery from cooling mode
  REAL(r64)    :: HRCoolEIRTC                =0.d0 ! Time constant used to recover from intial degratation in cooling heat recovery
  INTEGER      :: HRCAPFTHeat                =0   ! Index to heat capacity as a function of temperature curve for heat recovery
  REAL(r64)    :: HRCAPFTHeatConst           =1.1d0 ! constant used if curve is blank
  INTEGER      :: HRCAPFTHeatType            =0   ! Curve type for HRCAPFTHeat
  REAL(r64)    :: HRInitialHeatCapFrac       =1.d0 ! Fractional heating degradation at the start of heat recovery from heating mode
  REAL(r64)    :: HRHeatCapTC                =0.d0 ! Time constant used to recover from intial degratation in heating heat recovery
  INTEGER      :: HREIRFTHeat                =0   ! Index to heat EIR as a function of temperature curve for heat recovery
  REAL(r64)    :: HREIRFTHeatConst           =1.1d0 ! constant used if curve is blank
  INTEGER      :: HREIRFTHeatType            =0   ! Curve type for HREIRFTHeat
  REAL(r64)    :: HRInitialHeatEIRFrac       =1.d0 ! Fractional EIR degradation at the start of heat recovery from heating mode
  REAL(r64)    :: HRHeatEIRTC                =0.d0 ! Time constant used to recover from intial degratation in heating heat recovery
  LOGICAL      :: HRCoolingActive            =.FALSE. ! heat recovery mode active in cooling mode
  LOGICAL      :: HRHeatingActive            =.FALSE. ! heat recovery mode active in heating mode
  LOGICAL      :: ModeChange                 =.FALSE. ! tracks changes in operating mode
  LOGICAL      :: HRModeChange               =.FALSE. ! tracks changes in heat recovery operating mode
  REAL(r64)    :: HRTimer                    =0.d0 ! timer used to model changes in system performance as mode changes
  REAL(r64)    :: HRTime                     =0.d0 ! length of time system has been in same mode (hr)
  INTEGER      :: EIRFTempCoolErrorIndex     =0    ! warning message index for recurring warnings
  INTEGER      :: EIRFTempHeatErrorIndex     =0    ! warning message index for recurring warnings
  INTEGER      :: DefrostHeatErrorIndex      =0    ! warning message index for recurring warnings
  ! end variables used for heat recovery mode

  ! begin variables for Water System interactions
  INTEGER ::EvapWaterSupplyMode              = WaterSupplyFromMains !  where does water come from
  CHARACTER(len=MaxNameLength) :: EvapWaterSupplyName = ' ' ! name of water source e.g. water storage tank
  INTEGER ::EvapWaterSupTankID               = 0 !
  INTEGER ::EvapWaterTankDemandARRID         = 0 !
  CHARACTER(len=MaxNameLength) :: CondensateCollectName = ' ' ! name of water source e.g. water storage tank
  INTEGER ::CondensateTankID                 = 0 !
  INTEGER ::CondensateTankSupplyARRID        = 0 !
  REAL(r64)   :: CondensateVdot = 0.0d0 ! rate of water condensation from air stream [m3/s]
  REAL(r64)   :: CondensateVol  = 0.0d0 ! amount of water condensed from air stream [m3]
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
  LOGICAL, ALLOCATABLE :: HRHeatRequest(:)        ! defines a heating load on VRFTerminalUnits when QZnReq < 0
  LOGICAL, ALLOCATABLE :: HRCoolRequest(:)        ! defines a cooling load on VRFTerminalUnits when QZnReq > 0
  LOGICAL, ALLOCATABLE :: CoolingCoilAvailable(:) ! cooling coil availability scheduled on
  LOGICAL, ALLOCATABLE :: HeatingCoilAvailable(:) ! cooling coil availability scheduled on
  INTEGER, ALLOCATABLE :: CoolingCoilAvailSchPtr(:) ! cooilng coil availability schedule index
  INTEGER, ALLOCATABLE :: HeatingCoilAvailSchPtr(:) ! heating coil availability schedule index
END TYPE TerminalUnitListData

TYPE VRFTerminalUnitEquipment
  CHARACTER(len=MaxNameLength) :: Name       =' ' ! Name of the VRF Terminal Unit
  INTEGER      :: VRFTUType_Num              =0   ! DataHVACGlobals VRF Terminal Unit type
  INTEGER      :: SchedPtr                   =-1  ! Pointer to the correct schedule
  INTEGER      :: VRFSysNum                  =0   ! index to VRF Condenser
  INTEGER      :: TUListIndex                =0   ! index to VRF Terminal Unit List
  INTEGER      :: IndexToTUInTUList          =0   ! index to TU in VRF Terminal Unit List
  INTEGER      :: ZoneNum                    =0   ! index to zone where VRF Terminal Unit resides
  INTEGER      :: VRFTUInletNodeNum          =0   ! VRF Terminal Unit inlet node number
  INTEGER      :: VRFTUOutletNodeNum         =0   ! VRF Terminal Unit outlet node number
  INTEGER      :: VRFTUOAMixerOANodeNum      =0   ! OA node number for this TU's OA mixer
  INTEGER      :: VRFTUOAMixerRelNodeNum     =0   ! Relief node number for this TU's OA mixer
  INTEGER      :: VRFTUOAMixerRetNodeNum     =0   ! Return node number for this TU's OA mixer
  REAL(r64)    :: MaxCoolAirVolFlow          =0.0d0 ! supply air volumetric flow rate during cooling operation [m3/s]
  REAL(r64)    :: MaxHeatAirVolFlow          =0.0d0 ! supply air volumetric flow rate during heating operation [m3/s]
  REAL(r64)    :: MaxNoCoolAirVolFlow        =0.0d0 ! supply air volumetric flow rate when no cooling [m3/s]
  REAL(r64)    :: MaxNoHeatAirVolFlow        =0.0d0 ! supply air volumetric flow rate when no heating [m3/s]
  REAL(r64)    :: MaxCoolAirMassFlow         =0.0d0 ! supply air mass flow rate during cooling operation [kg/s]
  REAL(r64)    :: MaxHeatAirMassFlow         =0.0d0 ! supply air mass flow rate during heating operation [kg/s]
  REAL(r64)    :: MaxNoCoolAirMassFlow       =0.0d0 ! supply air mass flow rate when no cooling [kg/s]
  REAL(r64)    :: MaxNoHeatAirMassFlow       =0.0d0 ! supply air mass flow rate when no heating [kg/s]
  REAL(r64)    :: CoolOutAirVolFlow          =0.0d0 ! OA volumetric flow rate during cooling operation [m3/s]
  REAL(r64)    :: HeatOutAirVolFlow          =0.0d0 ! OA volumetric flow rate during heating operation [m3/s]
  REAL(r64)    :: NoCoolHeatOutAirVolFlow    =0.0d0 ! OA volumetric flow rate when no cooling or heating [m3/s]
  REAL(r64)    :: CoolOutAirMassFlow         =0.0d0 ! OA mass flow rate during cooling operation [kg/s]
  REAL(r64)    :: HeatOutAirMassFlow         =0.0d0 ! OA mass flow rate during heating operation [kg/s]
  REAL(r64)    :: NoCoolHeatOutAirMassFlow   =0.0d0 ! OA mass flow rate when no cooling or heating [kg/s]
  INTEGER      :: FanOpModeSchedPtr          =0   ! Pointer to the correct fan operating mode schedule
  INTEGER      :: FanAvailSchedPtr           =0   ! Pointer to the correct fan availability schedule
  INTEGER      :: FanIndex                   =0   ! Index to fan object
  REAL(r64)    :: FanPower                   =0.d0 ! power reported by fan component
  INTEGER      :: OpMode                     =0   ! operation mode: 1 = cycling fan, cycling coil 2 = constant fan, cycling coil
  INTEGER      :: FanPlace                   =0   ! fan placement; 1=blow through, 2=draw through
  REAL(r64)    :: ActualFanVolFlowRate       =0.0d0 ! volumetric flow rate from fan object
  CHARACTER(len=MaxNameLength) :: OAMixerName=' ' ! name of outside air mixer
  INTEGER      :: OAMixerIndex               =0   ! index to outside air mixer
  LOGICAL      :: OAMixerUsed                =.FALSE. ! true if OA Mixer object is used
  INTEGER      :: CoolCoilIndex              =0   ! index to terminal unit cooling coil
  INTEGER      :: HeatCoilIndex              =0   ! index to terminal unit heating coil
  INTEGER      :: DXCoolCoilType_Num         =0   ! type of VRF cooling coil
  INTEGER      :: DXHeatCoilType_Num         =0   ! type of VRF cooling coil
  REAL(r64)    :: ParasiticElec              =0.0d0 ! parasitic electric for VRF terminal unit
  REAL(r64)    :: ParasiticOffElec           =0.0d0 ! parasitic electric for VRF terminal unit when off
  REAL(r64)    :: HeatingSpeedRatio          = 1.d0 ! Fan speed ratio in heating mode
  REAL(r64)    :: HeatingCapacitySizeRatio   =1.d0 ! Ratio of heating to cooling when autosizing
  REAL(r64)    :: CoolingSpeedRatio          = 1.d0 ! Fan speed ratio in cooling mode
  REAL(r64)    :: ParasiticCoolElecPower       = 0.d0 ! Terminal unit cooling parasitic electric power [W]
  REAL(r64)    :: ParasiticHeatElecPower       = 0.d0 ! Terminal unit heating parasitic electric power [W]
  REAL(r64)    :: ParasiticElecCoolConsumption = 0.d0 ! Terminal unit parasitic electric consumption in cooling [J]
  REAL(r64)    :: ParasiticElecHeatConsumption = 0.d0 ! Terminal unit parasitic electric consumption in heating [J]
  LOGICAL      :: CoolingCoilPresent         =.TRUE. ! FALSE if coil not present
  LOGICAL      :: HeatingCoilPresent         =.TRUE. ! FALSE if coil not present
  CHARACTER(len=MaxNameLength) :: AvailManagerListName = ' ' ! Name of an availability manager list object
  INTEGER                      :: AvailStatus          = 0

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

  INTEGER      :: IterLimitExceeded          = 0     ! index used for warning messages
  INTEGER      :: FirstIterfailed            = 0     ! index used for warning messages

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
  REAL(r64) :: CompOnMassFlow     = 0.0d0      ! Supply air mass flow rate w/ compressor ON
  REAL(r64) :: OACompOnMassFlow   = 0.0d0      ! OA mass flow rate w/ compressor ON
  REAL(r64) :: CompOffMassFlow    = 0.0d0      ! Supply air mass flow rate w/ compressor OFF
  REAL(r64) :: OACompOffMassFlow  = 0.0d0      ! OA mass flow rate w/ compressor OFF
  REAL(r64) :: CompOnFlowRatio    = 0.0d0      ! fan flow ratio when coil on
  REAL(r64) :: CompOffFlowRatio   = 0.0d0      ! fan flow ratio when coil off
  REAL(r64) :: FanSpeedRatio      = 0.0d0      ! ratio of air flow ratio passed to fan object
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
  REAL(r64) :: CondenserWaterMassFlowRate ! VRF water-cooled condenser mass flow rate (kg/s)
  INTEGER,   ALLOCATABLE,SAVE, DIMENSION(:) :: NumCoolingLoads ! number of TU's requesting cooling
  INTEGER,   ALLOCATABLE,SAVE, DIMENSION(:) :: NumHeatingLoads ! number of TU's requesting heating
  REAL(r64), ALLOCATABLE,SAVE, DIMENSION(:) :: MaxDeltaT       ! maximum zone temperature difference from setpoint
  REAL(r64), ALLOCATABLE,SAVE, DIMENSION(:) :: MinDeltaT       ! minimum zone temperature difference from setpoint
  REAL(r64), ALLOCATABLE,SAVE, DIMENSION(:) :: SumCoolingLoads ! sum of cooling loads
  REAL(r64), ALLOCATABLE,SAVE, DIMENSION(:) :: SumHeatingLoads ! sum of heating loads

! Subroutine Specifications for the Module
          ! Driver/Manager Routines
Public  SimulateVRF
Public  SimVRFCondenserPlant

          ! Get Input routines for module
Private GetVRFInput

          ! Initialization routines for module
Private InitVRF
Private SizeVRF
Private SizeVRFCondenser

          ! Algorithms for the module
Private SimVRF
Private CalcVRF
Private CalcVRFCondenser
Private InitializeOperatingMode
Private LimitTUCapacity
Private LimitCoilCapacity

          ! Update routine to check convergence and update nodes
!Private UpdateVRF

          ! Reporting routines for module
Private ReportVRFTerminalUnit
Private ReportVRFCondenser
Private UpdateVRFCondenser

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
  INTEGER   :: IndexToTUInTUList  ! index to pointer in VRF AC system terminal unit list
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
      CheckEquipName(VRFTUNum)=.FALSE.
    ENDIF
  ENDIF

  ! the VRF condenser index
  VRFCondenser = VRFTU(VRFTUNum)%VRFSysNum
  ! the terminal unit list object index
  TUListNum    = VRFTU(VRFTUNum)%TUListIndex
  ! the entry number in the terminal unit list (which item in the terminal unit list, e.g. second in list)
  IndexToTUInTUList  = VRFTU(VRFTUNum)%IndexToTUInTUList
  ! index to cooling coil (coil is optional but at least one must be present)
  DXCoolingCoilIndex = VRFTU(VRFTUNum)%CoolCoilIndex
  ! index to heating coil (coil is optional but at least one must be present)
  DXHeatingCoilIndex = VRFTU(VRFTUNum)%HeatCoilIndex
  QZnReq = 0.d0

  ! Initialize terminal unit
  CALL InitVRF(VRFTuNum, ZoneNum, FirstHVACIteration, OnOffAirFlowRatio, QZnReq)  ! Initialize all VRFTU related parameters

  ! Simulate terminal unit
  CALL SimVRF(VRFTUNum, FirstHVACIteration, OnOffAirFlowRatio, SysOutputProvided, LatOutputProvided, QZnReq)

  ! mark this terminal unit as simulated
  TerminalUnitList(TUListNum)%IsSimulated(IndexToTUInTUList) = .TRUE.

  ! keep track of individual coil loads
  If(DXCoolingCoilIndex .GT. 0)THEN
    TerminalUnitList(TUListNum)%TotalCoolLoad(IndexToTUInTUList) = DXCoilTotalCooling(DXCoolingCoilIndex)
  ELSE
    TerminalUnitList(TUListNum)%TotalCoolLoad(IndexToTUInTUList) = 0.d0
  END IF
  IF(DXHeatingCoilIndex .GT. 0)THEN
    TerminalUnitList(TUListNum)%TotalHeatLoad(IndexToTUInTUList) = DXCoilTotalHeating(DXHeatingCoilIndex)
  ELSE
    TerminalUnitList(TUListNum)%TotalHeatLoad(IndexToTUInTUList) = 0.d0
  END IF

  ! Update the current VRF terminal unit to the outlet nodes
!  CALL UpdateVRF(VRFTUNum)

  ! Report the current VRF terminal unit
  CALL ReportVRFTerminalunit(VRFTUNum)

! make sure all TU in a list are able to get simulated, otherwise condenser is never simulated **
! either fatal on GetInput, or keep track of unused TU's and set their respective flag to TRUE **
! after all VRF terminal units have been simulated, call the VRF condenser model
  IF(ALL(TerminalUnitList(TUListNum)%IsSimulated))THEN
    CALL CalcVRFCondenser(VRFCondenser, FirstHVACIteration)
    CALL ReportVRFCondenser(VRFCondenser)
    IF(VRF(VRFCondenser)%CondenserType == WaterCooled)CALL UpdateVRFCondenser(VRFCondenser)
  END IF

  RETURN

END SUBROUTINE SimulateVRF

SUBROUTINE SimVRFCondenserPlant(VRFType, VRFTypeNum, VRFName, VRFNum, FirstHVACIteration, &
                                    InitLoopEquip, MyLoad, MaxCap, MinCap, OptCap, LoopNum)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Raustad
          !       DATE WRITTEN   May 2012
          !       MODIFIED
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine manages water-source VRF condenser

          ! METHODOLOGY EMPLOYED:

          ! REFERENCES:

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: FindItemInList
  USE PlantUtilities, ONLY:UpdateChillerComponentCondenserSide
  USE DataEnvironment
  USE General, ONLY: TrimSigDigits
  USE DataPlant,  ONLY: TypeOf_HPWaterEFCooling, TypeOf_HPWaterEFHeating

  IMPLICIT NONE

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: VRFType            ! Type of VRF
  INTEGER         , INTENT(IN) :: VRFTypeNum         ! Type of VRF in Plant equipment
  INTEGER         , INTENT(IN) :: LoopNum            ! The calling loop number
  CHARACTER(len=*), INTENT(IN) :: VRFName            ! User Specified Name of VRF
  INTEGER, INTENT(INOUT)       :: VRFNum             ! Index of Equipment
  LOGICAL, INTENT(IN)          :: FirstHVACIteration ! Flag for first time through HVAC simulation
  LOGICAL, INTENT(INOUT)       :: InitLoopEquip      ! If not zero, calculate the max load for operating conditions
  REAL(r64), INTENT(IN)        :: MyLoad             ! Loop demand component will meet
  REAL(r64), INTENT(OUT)       :: MinCap             ! Minimum operating capacity of GSHP [W]
  REAL(r64), INTENT(OUT)       :: MaxCap             ! Maximum operating capacity of GSHP [W]
  REAL(r64), INTENT(OUT)       :: OptCap             ! Optimal operating capacity of GSHP [W]


          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          ! na

  !Get input from VRF
  IF (GetVRFInputFlag) THEN  !First time subroutine has been entered
    CALL GetVRFInput
    GetVRFInputFlag=.false.
  END IF

  IF (InitLoopEquip) THEN
     VRFNum = FindItemInList( VRFName, VRF%Name, NumVRFCond )
     IF (VRFNum /= 0) THEN  ! if 0, fall through to next
       SELECT CASE (VRFTypeNum)
       CASE (TypeOf_HeatPumpVRF)
            MinCap = 0.0d0
            MaxCap = VRF(VRFNum)%HeatingCapacity ! should be greater than cooling capacity
            OptCap = VRF(VRFNum)%HeatingCapacity ! connects to single loop, how to switch between cooling/heating capacity?
       CASE DEFAULT
            CALL ShowFatalError('SimVRFCondenserPlant: Module called with incorrect VRFType='//TRIM(VRFType))
       END SELECT
       CALL SizeVRFCondenser(VRFNum)
       Return
     ENDIF
  END IF

  ! Calculate Demand on heat pump
  TypeOfEquip: SELECT CASE (VRFTypeNum)
    CASE (TypeOf_HeatPumpVRF)
      IF (VRFNum /= 0) THEN
        IF (LoopNum == VRF(VRFNum)%SourceLoopNum) THEN ! condenser loop
          CALL UpdateChillerComponentCondenserSide(VRF(VRFNum)%SourceLoopNum, &
                                     VRF(VRFNum)%SourceLoopSideNum,           &
                                     TypeOf_HeatPumpVRF,                      &
                                     VRF(VRFNum)%CondenserNodeNum,            &
                                     VRF(VRFNum)%CondenserOutletNodeNum,      &
                                     VRF(VRFNum)%QCondenser,                  &
                                     VRF(VRFNum)%CondenserInletTemp,      &
                                     VRF(VRFNum)%CondenserSideOutletTemp,     &
                                     VRF(VRFNum)%WaterCondenserMassFlow,       &
                                     FirstHVACIteration)

        ELSE
          CALL ShowFatalError ('SimVRFCondenserPlant:: Invalid loop connection '//  &
             TRIM(cVRFTypes(VRF_HeatPump))//', Requested Unit='//TRIM(VRFName))
        ENDIF
      ELSE
        CALL ShowFatalError ('SimVRFCondenserPlant:: Invalid '//TRIM(cVRFTypes(VRF_HeatPump))//  &
           ', Requested Unit='//TRIM(VRFName))
      ENDIF

    CASE DEFAULT
      CALL ShowFatalError('SimVRFCondenserPlant: Module called with incorrect VRFType='//TRIM(VRFTYpe))
    END SELECT TypeOfEquip


RETURN
END SUBROUTINE SimVRFCondenserPlant

SUBROUTINE CalcVRFCondenser(VRFCond, FirstHVACIteration)

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
  USE General,         ONLY: TrimSigDigits
  USE Psychrometrics,  ONLY: RhoH2O
  Use DataEnvironment, ONLY: StdBaroPress, EnvironmentName, CurMnDy, OutDryBulbTemp, OutHumRat, OutBaroPress, OutWetBulbTemp
  USE DXCoils,         ONLY: DXCoilCoolInletAirWBTemp, DXCoilHeatInletAirDBTemp, DXCoilHeatInletAirWBTemp
  USE PlantUtilities,  ONLY: SetComponentFlowRate
  USE FluidProperties, ONLY: GetSpecificHeatGlycol

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: VRFCond ! index to VRF condenser
  LOGICAL, INTENT(IN) :: FirstHVACIteration ! flag for first time through HVAC system simulation

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

  REAL(r64) :: TUCoolingLoad        ! DX cooling coil load to be met by condenser (W)
  REAL(r64) :: TUHeatingLoad        ! DX heating coil load to be met by condenser (W)
  REAL(r64) :: TUParasiticPower     ! total terminal unit parasitic power (W)
  REAL(r64) :: TUFanPower           ! total terminal unit fan power (W)
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
  REAL(r64) :: CoolingPLR               ! condenser cooling PLR
  REAL(r64) :: HeatingPLR               ! condenser heating PLR
  REAL(r64) :: CyclingRatio             ! cycling ratio of condenser's compressors
  REAL(r64) :: EIRFPLRModFac            ! EIRFPLR curve output
  INTEGER   :: Stage                    ! used for crankcase heater power calculation
  REAL(r64) :: UpperStageCompressorRatio ! used for crankcase heater power calculation
  REAL(r64) :: RhoAir                   ! Density of air [kg/m3]
  REAL(r64) :: RhoWater                 ! Density of water [kg/m3]
  REAL(r64) :: CpCond                   ! Specific Heat of water [J/kg-k]
  REAL(r64) :: CondAirMassFlow          ! Condenser air mass flow rate [kg/s]
  REAL(r64) :: CondWaterMassFlow        ! Condenser water mass flow rate [kg/s]
  REAL(r64) :: PartLoadFraction         ! Part load fraction from PLFFPLR curve
  REAL(r64) :: VRFRTF                   ! VRF runtime fraction when cycling below MINPLR
  REAL(r64) :: OutdoorCoilT             ! Outdoor coil temperature (C)
  REAL(r64) :: OutdoorCoildw            ! Outdoor coil delta w assuming coil temp of OutdoorCoilT (kg/kg)
  REAL(r64) :: FractionalDefrostTime    ! Fraction of time step system is in defrost
  REAL(r64) :: HeatingCapacityMultiplier ! Multiplier for heating capacity when system is in defrost
  REAL(r64) :: InputPowerMultiplier     ! Multiplier for power when system is in defrost
  REAL(r64) :: LoadDueToDefrost         ! Additional load due to defrost
  REAL(r64) :: DefrostEIRTempModFac     ! EIR modifier for defrost (function of entering drybulb, outside wetbulb)
  INTEGER   :: HRCAPFT                  ! index to heat recovery CAPFTCool curve
  REAL(r64) :: HRCAPFTConst             ! stead-state capacity fraction
  REAL(r64) :: HRInitialCapFrac         ! Fractional cooling degradation at the start of heat recovery from cooling mode
  REAL(r64) :: HRCapTC                  ! Time constant used to recover from intial degratation in cooling heat recovery
  INTEGER   :: HREIRFT                  ! Index to cool EIR as a function of temperature curve for heat recovery
  REAL(r64) :: HREIRFTConst             ! stead-state EIR fraction
  REAL(r64) :: HRInitialEIRFrac         ! Fractional cooling degradation at the start of heat recovery from cooling mode
  REAL(r64) :: HREIRTC                  ! Time constant used to recover from intial degratation in cooling heat recovery
  REAL(r64), SAVE :: CurrentEndTime     ! end time of current time step
  REAL(r64), SAVE :: CurrentEndTimeLast ! end time of last time step
  REAL(r64), SAVE :: TimeStepSysLast    ! system time step on last time step
  REAL(r64) :: SUMultiplier             ! multiplier for simulating mode changes
  REAL(r64) :: CondPower                ! condenser power [W]
  REAL(r64) :: CondCapacity             ! condenser heat rejection [W]
  REAL(r64) :: CondOutletTemp           ! Outlet temperature from VRF condenser [C]
  REAL(r64) :: QCondTmp                 ! temporary variable for condenser heat rejection [W]
  REAL(r64) :: TotPower                 ! total condenser power use [W]
  LOGICAL   :: HRHeatRequestFlag        ! flag indicating VRF TU could operate in heating mode
  LOGICAL   :: HRCoolRequestFlag        ! flag indicating VRF TU could operate in cooling mode
          ! FLOW

  ! variable initializations
  TUListNum         = VRF(VRFCond)%ZoneTUListPtr
  NumTUInList       = TerminalUnitList(TUListNum)%NumTUInList
  TUCoolingLoad     = 0.0d0
  TUHeatingLoad     = 0.0d0
  TUParasiticPower  = 0.0d0
  TUFanPower        = 0.0d0
  CoolingPLR        = 0.0d0
  HeatingPLR        = 0.0d0
  CyclingRatio      = 1.0d0
  SumCoolInletWB    = 0.0d0
  SumHeatInletDB    = 0.0d0
  SumHeatInletWB    = 0.0d0
  TotalCondCoolingCapacity = 0.d0
  TotalCondHeatingCapacity = 0.d0
  TotalTUCoolingCapacity   = 0.d0
  TotalTUHeatingCapacity   = 0.d0
  NumTUInCoolingMode       = 0
  NumTUInHeatingMode       = 0
  VRF(VRFCond)%ElecCoolingPower      = 0.d0
  VRF(VRFCond)%ElecHeatingPower      = 0.d0
  VRF(VRFCond)%CrankCaseHeaterPower  = 0.d0
  VRF(VRFCond)%EvapCondPumpElecPower = 0.d0
  VRF(VRFCond)%EvapWaterConsumpRate  = 0.d0
  VRF(VRFCond)%DefrostPower          = 0.d0
  VRF(VRFCond)%OperatingCoolingCOP   = 0.d0
  VRF(VRFCond)%OperatingHeatingCOP   = 0.d0
  VRF(VRFCond)%OperatingCOP          = 0.d0
  VRF(VRFCond)%BasinHeaterPower      = 0.d0

  ! sum loads on TU coils
  DO NumTU = 1, NumTUInList
    TUCoolingLoad = TUCoolingLoad + TerminalUnitList(TUListNum)%TotalCoolLoad(NumTU)
    TUHeatingLoad = TUHeatingLoad + TerminalUnitList(TUListNum)%TotalHeatLoad(NumTU)
    TUParasiticPower = TUParasiticPower + &
                       VRFTU(TerminalUnitList(TUListNum)%ZoneTUPTR(NumTU))%ParasiticCoolElecPower + &
                       VRFTU(TerminalUnitList(TUListNum)%ZoneTUPTR(NumTU))%ParasiticHeatElecPower
    TUFanPower = TUFanPower + VRFTU(TerminalUnitList(TUListNum)%ZoneTUPTR(NumTU))%FanPower
  END DO
  VRF(VRFCond)%TUCoolingLoad = TUCoolingLoad
  VRF(VRFCond)%TUHeatingLoad = TUHeatingLoad

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
    IF(VRF(VRFCond)%CondenserType /= WaterCooled)THEN
      OutdoorHumRat   = Node(VRF(VRFCond)%CondenserNodeNum)%HumRat
      OutdoorPressure = Node(VRF(VRFCond)%CondenserNodeNum)%Press
      OutdoorWetBulb  = Node(VRF(VRFCond)%CondenserNodeNum)%OutAirWetBulb
    ELSE
      OutdoorHumRat   = OutHumRat
      OutdoorPressure = OutBaroPress
      OutdoorWetBulb  = OutWetBulbTemp
    END IF
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
  ELSEIF (VRF(VRFCond)%CondenserType == WaterCooled) THEN
    CondInletTemp   = OutdoorDryBulb ! node inlet temp from above
    CondWaterMassFlow =  VRF(VRFCond)%WaterCondenserDesignMassFlow
  END IF
  VRF(VRFCond)%CondenserInletTemp = CondInletTemp

  ! calculate capacities and energy use
  IF(CoolingLoad(VRFCond) .AND. TerminalUnitList(TUListNum)%CoolingCoilPresent(NumTUInList))THEN
    InletAirWetbulbC = SumCoolInletWB
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

!   Warn user if curve output goes negative
    IF(TotCoolEIRTempModFac .LT. 0.d0)THEN
      IF(.NOT. Warmupflag .AND. NumTUInCoolingMode .GT. 0)THEN
        IF(VRF(VRFCond)%EIRFTempCoolErrorIndex == 0)THEN
          CALL ShowSevereMessage(TRIM(cVRFTypes(VRF_HeatPump))//' "'//TRIM(VRF(VRFCond)%Name)//'":')
          CALL ShowContinueError(' Cooling Energy Input Ratio Modifier curve (function of temperature) output is negative (' &
                        //TRIM(TrimSigDigits(TotCoolEIRTempModFac,3))//').')
          CALL ShowContinueError(' Negative value occurs using an outdoor air temperature of ' &
                       //TRIM(TrimSigDigits(CondInletTemp,1))//' C'// &
                       ' and an average indoor air wet-bulb temperature of '//TRIM(TrimSigDigits(InletAirWetbulbC,1))//' C.')
          CALL ShowContinueErrorTimeStamp(' Resetting curve output to zero and continuing simulation.')
        END IF
        CALL ShowRecurringWarningErrorAtEnd(TRIM(ccSimPlantEquipTypes(TypeOf_HeatPumpVRF))//' "'//TRIM(VRF(VRFCond)%Name)//'":'//&
        ' Cooling Energy Input Ratio Modifier curve (function of temperature) output is negative warning continues...' &
        , VRF(VRFCond)%EIRFTempCoolErrorIndex, TotCoolEIRTempModFac, TotCoolEIRTempModFac)
        TotCoolEIRTempModFac = 0.d0
      END IF
    END IF

  ELSE IF (HeatingLoad(VRFCond) .AND. TerminalUnitList(TUListNum)%HeatingCoilPresent(NumTUInList))THEN
    InletAirDrybulbC = SumHeatInletDB
    InletAirWetbulbC = SumHeatInletWB
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

    ! Initializing defrost adjustment factors
    LoadDueToDefrost = 0.0d0
    HeatingCapacityMultiplier = 1.d0
    FractionalDefrostTime = 0.0d0
    InputPowerMultiplier = 1.d0

    ! Check outdoor temperature to determine of defrost is active
    IF (OutdoorDryBulb .LE. VRF(VRFCond)%MaxOATDefrost) THEN

      ! Calculating adjustment factors for defrost
      ! Calculate delta w through outdoor coil by assuming a coil temp of 0.82*DBT-9.7(F) per DOE2.1E
      OutdoorCoilT = 0.82d0 * OutdoorDryBulb - 8.589d0
      OutdoorCoildw = MAX(1.0d-6,(OutdoorHumRat - PsyWFnTdpPb(OutdoorCoilT,OutdoorPressure)))

      ! Calculate defrost adjustment factors depending on defrost control type
      IF (VRF(VRFCond)%DefrostControl .EQ. Timed) THEN
        FractionalDefrostTime = VRF(VRFCond)%DefrostFraction
        IF(FractionalDefrostTime .GT. 0.d0)THEN
          HeatingCapacityMultiplier = 0.909d0 - 107.33d0 * OutdoorCoildw
          InputPowerMultiplier = 0.90d0 - 36.45d0*OutdoorCoildw
        END IF
      ELSE !else defrost control is on-demand
        FractionalDefrostTime = 1.0d0 / (1.0d0 + 0.01446d0 / OutdoorCoildw)
        HeatingCapacityMultiplier = 0.875d0 * ( 1.0d0 - FractionalDefrostTime)
        InputPowerMultiplier = 0.954d0 * ( 1.0d0 - FractionalDefrostTime)
      END IF


      IF (FractionalDefrostTime .GT. 0.0d0) THEN
        ! Calculate defrost adjustment factors depending on defrost control strategy
        IF (VRF(VRFCond)%DefrostStrategy .EQ. ReverseCycle) THEN
          LoadDueToDefrost = (0.01d0 * FractionalDefrostTime) * &
                             (7.222d0 - OutdoorDryBulb) * &
                             (VRF(VRFCond)%HeatingCapacity/1.01667d0)
          DefrostEIRTempModFac = CurveValue(VRF(VRFCond)%DefrostEIRPtr,&
                                 MAX(15.555d0,InletAirWetbulbC),MAX(15.555d0,OutdoorDryBulb))

!         Warn user if curve output goes negative
          IF(DefrostEIRTempModFac .LT. 0.0d0)THEN
            IF(.NOT. Warmupflag)THEN
              IF(VRF(VRFCond)%DefrostHeatErrorIndex == 0)THEN
                CALL ShowSevereMessage(TRIM(cVRFTypes(VRF_HeatPump))//' "'//TRIM(VRF(VRFCond)%Name)//'":')
                CALL ShowContinueError(' Defrost Energy Input Ratio Modifier curve (function of temperature) output is'// &
                        ' negative ('//TRIM(TrimSigDigits(DefrostEIRTempModFac,3))//').')
                CALL ShowContinueError(' Negative value occurs using an outdoor air dry-bulb temperature of ' &
                       //TRIM(TrimSigDigits(OutdoorDryBulb,1))//' C'// &
                       ' and an average indoor air wet-bulb temperature of '//TRIM(TrimSigDigits(InletAirWetbulbC,1))//' C.')
                CALL ShowContinueErrorTimeStamp(' Resetting curve output to zero and continuing simulation.')
              END IF
              CALL ShowRecurringWarningErrorAtEnd(TRIM(ccSimPlantEquipTypes(TypeOf_HeatPumpVRF))//' "'//  &
                 TRIM(VRF(VRFCond)%Name)//'":'//&
                ' Defrost Energy Input Ratio Modifier curve (function of temperature) output is negative warning continues...' &
                , VRF(VRFCond)%DefrostHeatErrorIndex, DefrostEIRTempModFac, DefrostEIRTempModFac)
              DefrostEIRTempModFac = 0.0d0
            END IF
          END IF

          VRF(VRFCond)%DefrostPower =  DefrostEIRTempModFac * &
                                            (VRF(VRFCond)%HeatingCapacity/1.01667d0) &
                                            * FractionalDefrostTime

        ELSE ! Defrost strategy is resistive
          VRF(VRFCond)%DefrostPower = VRF(VRFCond)%DefrostCapacity &
                                      * FractionalDefrostTime
        END IF
      ELSE ! Defrost is not active because FractionalDefrostTime = 0.0
        VRF(VRFCond)%DefrostPower = 0.d0
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

!   Warn user if curve output goes negative
    IF(TotHeatEIRTempModFac .LT. 0.d0)THEN
      IF(.NOT. Warmupflag .AND. NumTUInHeatingMode .GT. 0)THEN
        IF(VRF(VRFCond)%EIRFTempHeatErrorIndex == 0)THEN
          CALL ShowSevereMessage(TRIM(cVRFTypes(VRF_HeatPump))//' "'//TRIM(VRF(VRFCond)%Name)//'":')
          CALL ShowContinueError(' Heating Energy Input Ratio Modifier curve (function of temperature) output is negative (' &
                        //TRIM(TrimSigDigits(TotHeatEIRTempModFac,3))//').')
          SELECT CASE(VRF(VRFCond)%HeatingPerformanceOATType)
            CASE(DryBulbIndicator)
              CALL ShowContinueError(' Negative value occurs using an outdoor air dry-bulb temperature of ' &
                       //TRIM(TrimSigDigits(CondInletTemp,1))//' C'// &
                       ' and an average indoor air dry-bulb temperature of '//TRIM(TrimSigDigits(InletAirDrybulbC,1))//' C.')
            CASE(WetBulbIndicator)
              CALL ShowContinueError(' Negative value occurs using an outdoor air wet-bulb temperature of ' &
                       //TRIM(TrimSigDigits(OutdoorWetBulb,1))//' C'// &
                       ' and an average indoor air wet-bulb temperature of '//TRIM(TrimSigDigits(InletAirWetbulbC,1))//' C.')
            CASE DEFAULT
          END SELECT
          CALL ShowContinueErrorTimeStamp(' Resetting curve output to zero and continuing simulation.')
        END IF
        CALL ShowRecurringWarningErrorAtEnd(TRIM(ccSimPlantEquipTypes(TypeOf_HeatPumpVRF))//' "'//TRIM(VRF(VRFCond)%Name)//'":'//&
        ' Heating Energy Input Ratio Modifier curve (function of temperature) output is negative warning continues...' &
        , VRF(VRFCond)%EIRFTempHeatErrorIndex, TotHeatEIRTempModFac, TotHeatEIRTempModFac)
        TotHeatEIRTempModFac = 0.d0
      END IF
    END IF

  END IF

  VRF(VRFCond)%VRFCondPLR = MAX(CoolingPLR,HeatingPLR)

  HRHeatRequestFlag = ANY(TerminalUnitList(TUListNum)%HRHeatRequest)
  HRCoolRequestFlag = ANY(TerminalUnitList(TUListNum)%HRCoolRequest)

  IF(.NOT. DoingSizing .AND. .NOT. Warmupflag)THEN
    IF(HRHeatRequestFlag .AND. HRCoolRequestFlag)THEN
      ! determine operating mode change
      IF(.NOT. VRF(VRFCond)%HRCoolingActive .AND. .NOT. VRF(VRFCond)%HRHeatingActive)THEN
        VRF(VRFCond)%ModeChange = .TRUE.
      END IF
      IF(CoolingLoad(VRFCond))THEN
        IF(VRF(VRFCond)%HRHeatingActive .AND. .NOT. VRF(VRFCond)%HRCoolingActive)THEN
          VRF(VRFCond)%HRModeChange = .TRUE.
        END IF
        VRF(VRFCond)%HRCoolingActive = .TRUE.
        VRF(VRFCond)%HRHeatingActive = .FALSE.
        HRCAPFT = VRF(VRFCond)%HRCAPFTCool   ! Index to cool capacity as a function of temperature\PLR curve for heat recovery
        IF(HRCAPFT .GT. 0)THEN
!         VRF(VRFCond)%HRCAPFTCoolConst = 0.9d0 ! initialized to 0.9
          IF(VRF(VRFCond)%HRCAPFTCoolType == BIQUADRATIC)THEN ! Curve type for HRCAPFTCool
            VRF(VRFCond)%HRCAPFTCoolConst = CurveValue(HRCAPFT,InletAirWetbulbC,CondInletTemp)
          ELSE
            VRF(VRFCond)%HRCAPFTCoolConst = CurveValue(HRCAPFT,VRF(VRFCond)%VRFCondPLR)
          END IF
        END IF
        HRCAPFTConst = VRF(VRFCond)%HRCAPFTCoolConst
        HRInitialCapFrac = VRF(VRFCond)%HRInitialCoolCapFrac ! Fractional cooling degradation at the start of heat recovery from cooling mode
        HRCapTC          = VRF(VRFCond)%HRCoolCapTC ! Time constant used to recover from intial degratation in cooling heat recovery

        HREIRFT = VRF(VRFCond)%HREIRFTCool   ! Index to cool EIR as a function of temperature curve for heat recovery
        IF(HREIRFT .GT. 0)THEN
!         VRF(VRFCond)%HREIRFTCoolConst = 1.1d0 ! initialized to 1.1
          IF(VRF(VRFCond)%HREIRFTCoolType == BIQUADRATIC)THEN ! Curve type for HRCAPFTCool
            VRF(VRFCond)%HREIRFTCoolConst = CurveValue(HREIRFT,InletAirWetbulbC,CondInletTemp)
          ELSE
            VRF(VRFCond)%HREIRFTCoolConst = CurveValue(HREIRFT,VRF(VRFCond)%VRFCondPLR)
          END IF
        END IF
        HREIRFTConst = VRF(VRFCond)%HREIRFTCoolConst
        HRInitialEIRFrac = VRF(VRFCond)%HRInitialCoolEIRFrac ! Fractional cooling degradation at the start of heat recovery from cooling mode
        HREIRTC          = VRF(VRFCond)%HRCoolEIRTC ! Time constant used to recover from intial degratation in cooling heat recovery
      ELSE IF(HeatingLoad(VRFCond))THEN
        IF(.NOT. VRF(VRFCond)%HRHeatingActive .AND. VRF(VRFCond)%HRCoolingActive)THEN
          VRF(VRFCond)%HRModeChange = .TRUE.
        END IF
        VRF(VRFCond)%HRCoolingActive = .FALSE.
        VRF(VRFCond)%HRHeatingActive = .TRUE.
        HRCAPFT = VRF(VRFCond)%HRCAPFTHeat   ! Index to heat capacity as a function of temperature\PLR curve for heat recovery
        IF(HRCAPFT .GT. 0)THEN
!         VRF(VRFCond)%HRCAPFTHeatConst = 1.1d0 ! initialized to 1.1
          IF(VRF(VRFCond)%HRCAPFTHeatType == BIQUADRATIC)THEN ! Curve type for HRCAPFTCool
            SELECT CASE(VRF(VRFCond)%HeatingPerformanceOATType)
              CASE(DryBulbIndicator)
                VRF(VRFCond)%HRCAPFTHeatConst = CurveValue(HRCAPFT,InletAirDrybulbC,CondInletTemp)
              CASE(WetBulbIndicator)
                VRF(VRFCond)%HRCAPFTHeatConst = CurveValue(HRCAPFT,InletAirDrybulbC,OutdoorWetBulb)
              CASE DEFAULT
                VRF(VRFCond)%HRCAPFTHeatConst = 1.d0
            END SELECT
          ELSE
            VRF(VRFCond)%HRCAPFTHeatConst = CurveValue(HRCAPFT,VRF(VRFCond)%VRFCondPLR)
          END IF
        END IF
        HRCAPFTConst = VRF(VRFCond)%HRCAPFTHeatConst
        HRInitialCapFrac = VRF(VRFCond)%HRInitialHeatCapFrac ! Fractional heating degradation at the start of heat recovery from cooling mode
        HRCapTC          = VRF(VRFCond)%HRHeatCapTC ! Time constant used to recover from intial degratation in heating heat recovery

        HREIRFT = VRF(VRFCond)%HREIRFTHeat   ! Index to cool EIR as a function of temperature curve for heat recovery
        IF(HREIRFT .GT. 0)THEN
!         VRF(VRFCond)%HREIRFTCoolConst = 1.1d0 ! initialized to 1.1
          IF(VRF(VRFCond)%HREIRFTHeatType == BIQUADRATIC)THEN ! Curve type for HRCAPFTHeat
            SELECT CASE(VRF(VRFCond)%HeatingPerformanceOATType)
              CASE(DryBulbIndicator)
                VRF(VRFCond)%HREIRFTHeatConst = CurveValue(HREIRFT,InletAirDrybulbC,CondInletTemp)
              CASE(WetBulbIndicator)
                VRF(VRFCond)%HREIRFTHeatConst = CurveValue(HREIRFT,InletAirDrybulbC,OutdoorWetBulb)
              CASE DEFAULT
                VRF(VRFCond)%HREIRFTHeatConst = 1.d0
            END SELECT
          ELSE
            VRF(VRFCond)%HREIRFTHeatConst = CurveValue(HREIRFT,VRF(VRFCond)%VRFCondPLR)
          END IF
        END IF
        HREIRFTConst = VRF(VRFCond)%HRCAPFTHeatConst
        HRInitialEIRFrac = VRF(VRFCond)%HRInitialHeatEIRFrac ! Fractional heating degradation at the start of heat recovery from heating mode
        HREIRTC          = VRF(VRFCond)%HRHeatEIRTC ! Time constant used to recover from intial degratation in heating heat recovery
      ELSE
    !   zone thermostats satisfied, condenser is off. Set values anyway
        HRCAPFTConst     = 1.d0
        HRInitialCapFrac = 1.d0
        HRCapTC          = 1.d0
        HREIRFTConst     = 1.d0
        HRInitialEIRFrac = 1.d0
        HREIRTC          = 1.d0
        IF(VRF(VRFCond)%HRHeatingActive .OR. VRF(VRFCond)%HRCoolingActive)THEN
          VRF(VRFCond)%HRModeChange = .TRUE.
        END IF
        VRF(VRFCond)%HRCoolingActive = .FALSE.
        VRF(VRFCond)%HRHeatingActive = .FALSE.
      END IF

    ELSE ! IF(HRHeatRequestFlag .AND. HRCoolRequestFlag)THEN -- Heat recovery turned off
      HRCAPFTConst     = 1.d0
      HRInitialCapFrac = 1.d0
      HRCapTC          = 0.d0
      HREIRFTConst     = 1.d0
      HRInitialEIRFrac = 1.d0
      HREIRTC          = 0.d0
      VRF(VRFCond)%HRModeChange = .FALSE.
      VRF(VRFCond)%HRCoolingActive = .FALSE.
      VRF(VRFCond)%HRHeatingActive = .FALSE.
    END IF

  ! calculate end time of current time step to determine if max capacity reset is required
    CurrentEndTime = REAL(((DayOfSim-1)*24),r64) + CurrentTime - TimeStepZone + SysTimeElapsed

    IF(VRF(VRFCond)%ModeChange .OR. VRF(VRFCond)%HRModeChange)THEN
      IF(VRF(VRFCond)%HRCoolingActive .AND. VRF(VRFCond)%HRTimer == 0.d0)THEN
        VRF(VRFCond)%HRTimer = CurrentEndTimeLast
      ELSE IF(VRF(VRFCond)%HRHeatingActive .AND. VRF(VRFCond)%HRTimer == 0.d0)THEN
        VRF(VRFCond)%HRTimer = CurrentEndTimeLast
      ELSE IF(.NOT. VRF(VRFCond)%HRCoolingActive .AND. .NOT. VRF(VRFCond)%HRHeatingActive)THEN
        VRF(VRFCond)%HRTimer = 0.d0
      END IF
    END IF

    VRF(VRFCond)%HRTime = MAX(0.d0,CurrentEndTime - VRF(VRFCond)%HRTimer)
    IF(VRF(VRFCond)%HRTime .LT. (HRCapTC * 5.d0))THEN
      IF(HRCAPTC .GT. 0.d0)THEN
        SUMultiplier = MIN(1.d0, 1.d0 - EXP(-VRF(VRFCond)%HRTime/HRCAPTC))
      ELSE
        SUMultiplier = 1.d0
      END IF
    ELSE
      SUMultiplier = 1.d0
      VRF(VRFCond)%ModeChange = .FALSE.
      VRF(VRFCond)%HRModeChange = .FALSE.
    END IF
    VRF(VRFCond)%SUMultiplier = SUMultiplier

    TimeStepSysLast = TimeStepSys
    CurrentEndTimeLast = CurrentEndTime

    IF(VRF(VRFCond)%HeatRecoveryUsed .AND. VRF(VRFCond)%HRCoolingActive)THEN
      TotalCondCoolingCapacity = HRCAPFTConst * TotalCondCoolingCapacity
      TotalCondCoolingCapacity = HRInitialCapFrac * TotalCondCoolingCapacity + &
                               (1.d0 - HRInitialCapFrac) * TotalCondCoolingCapacity * SUMultiplier
      TotalTUCoolingCapacity = TotalCondCoolingCapacity * VRF(VRFCond)%PipingCorrectionCooling
      IF(TotalCondCoolingCapacity .GT. 0.d0)THEN
        CoolingPLR = MIN(1.d0,(TUCoolingLoad/VRF(VRFCond)%PipingCorrectionCooling) / TotalCondCoolingCapacity)
      ELSE
        CoolingPLR = 0.d0
      END IF
    ELSE IF(VRF(VRFCond)%HeatRecoveryUsed .AND. VRF(VRFCond)%HRHeatingActive)THEN
      TotalCondHeatingCapacity = HRCAPFTConst * TotalCondHeatingCapacity
      TotalCondHeatingCapacity = HRInitialCapFrac * TotalCondHeatingCapacity + &
                               (1.d0 - HRInitialCapFrac) * TotalCondHeatingCapacity * SUMultiplier
      TotalTUHeatingCapacity = TotalCondHeatingCapacity * VRF(VRFCond)%PipingCorrectionHeating
      IF(TotalCondHeatingCapacity .GT. 0.d0)THEN
        HeatingPLR = MIN(1.d0,(TUHeatingLoad/VRF(VRFCond)%PipingCorrectionHeating) / TotalCondHeatingCapacity)
      ELSE
        HeatingPLR = 0.d0
      END IF
    END IF
    VRF(VRFCond)%VRFCondPLR = MAX(CoolingPLR,HeatingPLR)
  END IF

  VRF(VRFCond)%TotalCoolingCapacity = TotalCondCoolingCapacity * CoolingPLR
  VRF(VRFCond)%TotalHeatingCapacity = TotalCondHeatingCapacity * HeatingPLR

  IF(VRF(VRFCond)%MinPLR .GT. 0.0d0)THEN
    CyclingRatio = MIN(1.0d0,VRF(VRFCond)%VRFCondPLR/VRF(VRFCond)%MinPLR)
    IF(VRF(VRFCond)%VRFCondPLR .LT. VRF(VRFCond)%MinPLR .AND. VRF(VRFCond)%VRFCondPLR .GT. 0.0d0)THEN
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

  CondCapacity = MAX(VRF(VRFCond)%TotalCoolingCapacity,VRF(VRFCond)%TotalHeatingCapacity)*VRFRTF
  CondPower = MAX(VRF(VRFCond)%ElecCoolingPower,VRF(VRFCond)%ElecHeatingPower)
  IF(VRF(VRFCond)%ElecCoolingPower .GT. 0.d0)THEN
    VRF(VRFCond)%QCondenser           = CondCapacity + CondPower - &
                                        VRF(VRFCond)%TUHeatingLoad/VRF(VRFCond)%PipingCorrectionHeating
  ELSE IF(VRF(VRFCond)%ElecHeatingPower .GT. 0.d0)THEN
    VRF(VRFCond)%QCondenser           = -CondCapacity + CondPower + &
                                        VRF(VRFCond)%TUCoolingLoad/VRF(VRFCond)%PipingCorrectionCooling
  ELSE
    VRF(VRFCond)%QCondenser           = 0.d0
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
  ELSE IF(VRF(VRFCond)%CondenserType == WaterCooled) THEN

    IF(CondCapacity .GT. 0.0d0)THEN
      CondenserWaterMassFlowRate = CondWaterMassFlow
    ELSE
      CondenserWaterMassFlowRate = 0.d0
    END IF
    Call SetComponentFlowRate(CondenserWaterMassFlowRate, &
          VRF(VRFCond)%CondenserNodeNum, VRF(VRFCond)%CondenserOutletNodeNum, &
          VRF(VRFCond)%SourceLoopNum, VRF(VRFCond)%SourceLoopSideNum, &
          VRF(VRFCond)%SourceBranchNum, VRF(VRFCond)%SourceCompNum)

    VRF(VRFCond)%CondenserInletTemp = Node(VRF(VRFCond)%CondenserNodeNum)%Temp
    VRF(VRFCond)%WaterCondenserMassFlow = Node(VRF(VRFCond)%CondenserNodeNum)%MassFlowRate

    CpCond = GetSpecificHeatGlycol(PlantLoop(VRF(VRFCond)%SourceLoopNum)%FluidName,  &
                                   VRF(VRFCond)%CondenserInletTemp,                      &
                                   PlantLoop(VRF(VRFCond)%SourceLoopNum)%FluidIndex, &
                                   'VRFCondenser')
    IF(CondWaterMassFlow .GT. 0.0d0)THEN
      CondOutletTemp = VRF(VRFCond)%QCondenser/(CondWaterMassFlow*CpCond) + CondInletTemp
    ELSE
      CondOutletTemp = CondInletTemp
    END IF
    QCondTmp = CondWaterMassFlow*CpCond*(CondOutletTemp-CondInletTemp)
    VRF(VRFCond)%CondenserSideOutletTemp   = CondOutletTemp

  END IF

  ! calculate operating COP
  IF(CoolingLoad(VRFCond) .AND. CoolingPLR > 0.d0)THEN
    IF(VRF(VRFCond)%ElecCoolingPower .NE. 0.d0)THEN
      ! this calc should use delivered capacity, not condenser capacity, use VRF(VRFCond)%TUCoolingLoad
      VRF(VRFCond)%OperatingCoolingCOP = (VRF(VRFCond)%TotalCoolingCapacity)/ &
       (VRF(VRFCond)%ElecCoolingPower + VRF(VRFCond)%CrankCaseHeaterPower + &
        VRF(VRFCond)%EvapCondPumpElecPower + VRF(VRFCond)%DefrostPower)
    ELSE
      VRF(VRFCond)%OperatingCoolingCOP = 0.D0
    END IF
  END IF
  IF(HeatingLoad(VRFCond) .AND. HeatingPLR > 0.0d0)THEN
    IF(VRF(VRFCond)%ElecHeatingPower .NE. 0.d0)THEN
      ! this calc should use deleivered capacity, not condenser capacity, use VRF(VRFCond)%TUHeatingLoad
      VRF(VRFCond)%OperatingHeatingCOP = (VRF(VRFCond)%TotalHeatingCapacity)/ &
      (VRF(VRFCond)%ElecHeatingPower + VRF(VRFCond)%CrankCaseHeaterPower + &
        VRF(VRFCond)%EvapCondPumpElecPower + VRF(VRFCond)%DefrostPower)
    ELSE
      VRF(VRFCond)%OperatingHeatingCOP = 0.D0
    END IF
  END IF

  TotPower = TUParasiticPower + TUFanPower + VRF(VRFCond)%ElecHeatingPower + VRF(VRFCond)%ElecCoolingPower + &
             VRF(VRFCond)%CrankCaseHeaterPower + VRF(VRFCond)%EvapCondPumpElecPower + VRF(VRFCond)%DefrostPower
  IF(TotPower .GT. 0.d0) &
    VRF(VRFCond)%OperatingCOP = (VRF(VRFCond)%TUCoolingLoad + VRF(VRFCond)%TUHeatingLoad) / TotPower

! limit the TU capacity when the condenser is maxed out on capacity
! I think this next line will make the max cap report variable match the coil objects, will probably change the answer though
!  IF(CoolingLoad(VRFCond) .AND. NumTUInCoolingMode .GT. 0 .AND. MaxCoolingCapacity(VRFCond) == MaxCap)THEN
  IF(CoolingLoad(VRFCond) .AND. NumTUInCoolingMode .GT. 0)THEN

!   IF TU capacity is greater than condenser capacity find maximum allowed TU capacity (i.e., conserve energy)
    IF(TUCoolingLoad > TotalTUCoolingCapacity)THEN
      CALL LimitTUCapacity(VRFCond,NumTUInList, &
                TotalTUCoolingCapacity,TerminalUnitList(TUListNum)%TotalCoolLoad, MaxCoolingCapacity(VRFCond), &
                TotalTUHeatingCapacity,TerminalUnitList(TUListNum)%TotalHeatLoad, MaxHeatingCapacity(VRFCond))
    END IF
  ELSE IF(HeatingLoad(VRFCond) .AND. NumTUInHeatingMode .GT. 0)THEN
!   IF TU capacity is greater than condenser capacity
    IF(TUHeatingLoad > TotalTUHeatingCapacity)THEN
      CALL LimitTUCapacity(VRFCond,NumTUInList, &
                TotalTUHeatingCapacity,TerminalUnitList(TUListNum)%TotalHeatLoad, MaxHeatingCapacity(VRFCond), &
                TotalTUCoolingCapacity,TerminalUnitList(TUListNum)%TotalCoolLoad, MaxCoolingCapacity(VRFCond))
    END IF
  ELSE
  END IF

  RETURN

END SUBROUTINE CalcVRFCondenser


! Get Input Section of the Module
!******************************************************************************
SUBROUTINE GetVRFInput

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Raustad, FSEC
          !       DATE WRITTEN   August 2010
          !       MODIFIED       July 2012, Chandan Sharma - FSEC: Added zone sys avail managers
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
                                     GetCoilCondenserInletNode, GetCoilTypeNum, SetDXCoolingCoilData, GetDXCoilAvailSchPtr
    USE DataHeatBalance,       ONLY: Zone
    USE OutAirNodeManager,     ONLY: CheckOutAirNodeNumber
    USE WaterManager,          ONLY: SetupTankDemandComponent, SetupTankSupplyComponent
    USE DataZoneEquipment,     ONLY: ZoneEquipConfig, VRFTerminalUnit_Num
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
    rNumericArgs=0.0d0

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
      CALL GetObjectItem(cCurrentModuleObject,VRFNum,cAlphaArgs,NumAlphas, &
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
      ALLOCATE(TerminalUnitList(VRFNum)%HRHeatRequest(TerminalUnitList(VRFNum)%NumTUInList))
      ALLOCATE(TerminalUnitList(VRFNum)%HRCoolRequest(TerminalUnitList(VRFNum)%NumTUInList))
      ALLOCATE(TerminalUnitList(VRFNum)%CoolingCoilAvailable(TerminalUnitList(VRFNum)%NumTUInList))
      ALLOCATE(TerminalUnitList(VRFNum)%HeatingCoilAvailable(TerminalUnitList(VRFNum)%NumTUInList))
      ALLOCATE(TerminalUnitList(VRFNum)%CoolingCoilAvailSchPtr(TerminalUnitList(VRFNum)%NumTUInList))
      ALLOCATE(TerminalUnitList(VRFNum)%HeatingCoilAvailSchPtr(TerminalUnitList(VRFNum)%NumTUInList))
      TerminalUnitList(VRFNum)%ZoneTUPtr     = 0
      TerminalUnitList(VRFNum)%IsSimulated   = .FALSE.
      TerminalUnitList(VRFNum)%TotalCoolLoad = 0.0d0
      TerminalUnitList(VRFNum)%TotalHeatLoad = 0.0d0
      TerminalUnitList(VRFNum)%CoolingCoilPresent = .TRUE.
      TerminalUnitList(VRFNum)%HeatingCoilPresent = .TRUE.
      TerminalUnitList(VRFNum)%TerminalUnitNotSizedYet = .TRUE.
      TerminalUnitList(VRFNum)%HRHeatRequest = .FALSE.
      TerminalUnitList(VRFNum)%HRCoolRequest = .FALSE.
      TerminalUnitList(VRFNum)%CoolingCoilAvailable = .FALSE.
      TerminalUnitList(VRFNum)%HeatingCoilAvailable = .FALSE.
      TerminalUnitList(VRFNum)%CoolingCoilAvailSchPtr = -1
      TerminalUnitList(VRFNum)%HeatingCoilAvailSchPtr = -1
      DO TUListNum = 1, TerminalUnitList(VRFNum)%NumTUInList
        TerminalUnitList(VRFNum)%ZoneTUName(TUListNum) = cAlphaArgs(TUListNum+1)
      END DO
    END DO

    ! read all VRF condenser objects
    cCurrentModuleObject= 'AirConditioner:VariableRefrigerantFlow'
    DO VRFNum = 1,  NumVRFCond
      CALL GetObjectItem(cCurrentModuleObject,VRFNum,cAlphaArgs,NumAlphas, &
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
      IF (lAlphaFieldBlanks(2)) THEN
        VRF(VRFNum)%SchedPtr = ScheduleAlwaysOn
      ELSE
        VRF(VRFNum)%SchedPtr = GetScheduleIndex(cAlphaArgs(2))
        IF (VRF(VRFNum)%SchedPtr == 0) THEN
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(VRF(VRFNum)%Name)//'" invalid data')
          CALL ShowContinueError('Invalid-not found '//trim(cAlphaFieldNames(2))//'="'//    &
             trim(cAlphaArgs(2))//'".')
          ErrorsFound=.true.
        ENDIF
      ENDIF
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
      VRF(VRFNum)%HeatingCapacitySizeRatio = rNumericArgs(6)
      IF(.NOT. lNumericFieldBlanks(6) .AND. VRF(VRFNum)%HeatingCapacity .EQ. Autosize)THEN
        VRF(VRFNum)%LockHeatingCapacity = .TRUE.
      ENDIF
      VRF(VRFNum)%HeatingCOP      = rNumericArgs(7)
      VRF(VRFNum)%MinOATHeating   = rNumericArgs(8)
      VRF(VRFNum)%MaxOATHeating   = rNumericArgs(9)
      IF(VRF(VRFNum)%MinOATHeating .GE. VRF(VRFNum)%MaxOATHeating)THEN
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//', "'//TRIM(VRF(VRFNum)%Name)//'"')
        CALL ShowContinueError('... '//TRIM(cNumericFieldNames(8))//' ('//TRIM(TrimSigDigits(VRF(VRFNum)%MinOATHeating,3))// &
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

      VRF(VRFNum)%MinPLR = rNumericArgs(10)

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
        ELSE IF (SameString(cAlphaArgs(28),'Yes') )THEN
          VRF(VRFNum)%HeatRecoveryUsed = .TRUE.
        ELSE
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//' = '//TRIM(VRF(VRFNum)%Name))
          CALL ShowContinueError('Illegal '//TRIM(cAlphaFieldNames(28))//' = '//TRIM(cAlphaArgs(28)))
          ErrorsFound = .TRUE.
        END IF
      END IF

      VRF(VRFNum)%EquivPipeLngthCool = rNumericArgs(11)
      VRF(VRFNum)%VertPipeLngth      = rNumericArgs(12)
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
      VRF(VRFNum)%PCFHeightCool      = rNumericArgs(13)

      VRF(VRFNum)%EquivPipeLngthHeat = rNumericArgs(14)
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
      VRF(VRFNum)%PCFHeightHeat      = rNumericArgs(15)

      VRF(VRFNum)%CCHeaterPower      = rNumericArgs(16)
      VRF(VRFNum)%NumCompressors     = rNumericArgs(17)
      VRF(VRFNum)%CompressorSizeRatio = rNumericArgs(18)
      VRF(VRFNum)%MaxOATCCHeater     = rNumericArgs(19)


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

      VRF(VRFNum)%DefrostFraction = rNumericArgs(20)
      VRF(VRFNum)%DefrostCapacity = rNumericArgs(21)
      IF(VRF(VRFNum)%DefrostCapacity .EQ. 0.0d0 .AND. VRF(VRFNum)%DefrostStrategy .EQ. RESISTIVE) THEN
        CALL ShowWarningError(TRIM(cCurrentModuleObject)//', "'//TRIM(VRF(VRFNum)%Name)//&
                          '" '//TRIM(cNumericFieldNames(21))//' = 0.0 for defrost strategy = RESISTIVE.')
      END IF

      VRF(VRFNum)%MaxOATDefrost = rNumericArgs(22)

      IF (.NOT. lAlphaFieldBlanks(35)) THEN
        IF (SameString(cAlphaArgs(34),'AirCooled') ) VRF(VRFNum)%CondenserType = AirCooled
        IF (SameString(cAlphaArgs(34),'EvaporativelyCooled') ) VRF(VRFNum)%CondenserType = EvapCooled
        IF (SameString(cAlphaArgs(34),'WaterCooled') )THEN
          VRF(VRFNum)%CondenserType = WaterCooled
          VRF(VRFNum)%VRFPlantTypeOfNum = TypeOf_HeatPumpVRF
        END IF
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
        SELECT CASE(VRF(VRFNum)%CondenserType)
        CASE(AirCooled, EvapCooled)
          VRF(VRFNum)%CondenserNodeNum = &
           GetOnlySingleNode(cAlphaArgs(35),ErrorsFound,TRIM(cCurrentModuleObject),VRF(VRFNum)%Name, &
                         NodeType_Air,NodeConnectionType_OutsideAirReference,1,ObjectIsNotParent)
          IF (.not. CheckOutAirNodeNumber(VRF(VRFNum)%CondenserNodeNum)) THEN
            CALL ShowSevereError(TRIM(cCurrentModuleObject)//', "'//TRIM(VRF(VRFNum)%Name)//&
                          '" '//TRIM(cAlphaFieldNames(35))//' not a valid Outdoor Air Node = '//TRIM(cAlphaArgs(35)))
            CALL ShowContinueError('...node name does not appear in an OutdoorAir:NodeList or as an OutdoorAir:Node.')
            ErrorsFound=.TRUE.
          END IF
        CASE(WaterCooled)
          VRF(VRFNum)%CondenserNodeNum = &
             GetOnlySingleNode(cAlphaArgs(35),ErrorsFound,TRIM(cCurrentModuleObject),VRF(VRFNum)%Name, &
                         NodeType_Water,NodeConnectionType_Inlet,2,ObjectIsNotParent)
        CASE DEFAULT
        END SELECT
      ENDIF

      IF (.NOT. lAlphaFieldBlanks(36) .AND. VRF(VRFNum)%CondenserType == WaterCooled) THEN
        VRF(VRFNum)%CondenserOutletNodeNum = &
           GetOnlySingleNode(cAlphaArgs(36),ErrorsFound,TRIM(cCurrentModuleObject),VRF(VRFNum)%Name, &
                         NodeType_Water,NodeConnectionType_Outlet,2,ObjectIsNotParent)
          CALL TestCompSet(TRIM(cCurrentModuleObject),VRF(VRFNum)%Name,cAlphaArgs(35),cAlphaArgs(36),'Condenser Water Nodes')
      ELSE IF (lAlphaFieldBlanks(36) .AND. VRF(VRFNum)%CondenserType == WaterCooled) THEN
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//', "'//TRIM(VRF(VRFNum)%Name)//&
                          '" '//TRIM(cAlphaFieldNames(36))//' is blank.')
        CALL ShowContinueError('...node name must be entered when Condenser Type = WaterCooled.')
        ErrorsFound=.TRUE.
      END IF

      IF(lNumericFieldBlanks(23))THEN
        IF(VRF(VRFNum)%CondenserType == WaterCooled)THEN
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//', "'//TRIM(VRF(VRFNum)%Name)//&
                          '" '//TRIM(cNumericFieldNames(23))//' is blank.')
          CALL ShowContinueError('...input is required when '//TRIM(cAlphaFieldNames(34))//' = '//TRIM(cAlphaArgs(34)))
          ErrorsFound=.TRUE.
        END IF
      ELSE
        VRF(VRFNum)%WaterCondVolFlowRate = rNumericArgs(23)
      END IF
      VRF(VRFNum)%EvapCondEffectiveness = rNumericArgs(24)
      VRF(VRFNum)%EvapCondAirVolFlowRate = rNumericArgs(25)
      VRF(VRFNum)%EvapCondPumpPower = rNumericArgs(26)

      ! Get Water System tank connections
      ! A37, \field Supply Water Storage Tank Name
      VRF(VRFNum)%EvapWaterSupplyName = cAlphaArgs(37)
      IF (lAlphaFieldBlanks(37)) THEN
        VRF(VRFNum)%EvapWaterSupplyMode = WaterSupplyFromMains
      ELSE
        VRF(VRFNum)%EvapWaterSupplyMode = WaterSupplyFromTank
        CALL SetupTankDemandComponent(VRF(VRFNum)%Name,TRIM(cCurrentModuleObject), &
                 VRF(VRFNum)%EvapWaterSupplyName, ErrorsFound, VRF(VRFNum)%EvapWaterSupTankID, &
                 VRF(VRFNum)%EvapWaterTankDemandARRID )
      ENDIF

      !   Basin heater power as a function of temperature must be greater than or equal to 0
      VRF(VRFNum)%BasinHeaterPowerFTempDiff = rNumericArgs(27)
      IF(rNumericArgs(27) .LT. 0.0d0) THEN
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//' = "'//TRIM(VRF(VRFNum)%Name)//&
                   '", '//TRIM(cNumericFieldNames(27))//' must be >= 0')
        ErrorsFound = .TRUE.
      END IF

      VRF(VRFNum)%BasinHeaterSetPointTemp = rNumericArgs(28)
      IF(VRF(VRFNum)%BasinHeaterPowerFTempDiff .GT. 0.0d0) THEN
        IF(NumNums .LT. 27) THEN
          VRF(VRFNum)%BasinHeaterSetPointTemp = 2.0d0
        ENDIF
        IF(VRF(VRFNum)%BasinHeaterSetPointTemp < 2.0d0) THEN
          CALL ShowWarningError(TRIM(cCurrentModuleObject)//' = "'//TRIM(VRF(VRFNum)%Name)//&
           '", '//TRIM(cNumericFieldNames(28))//' is less than 2 deg C. Freezing could occur.')
        END IF
      END IF

      IF(.NOT. lAlphaFieldBlanks(38))THEN
        VRF(VRFNum)%BasinHeaterSchedulePtr   = GetScheduleIndex(cAlphaArgs(38))
        IF(VRF(VRFNum)%BasinHeaterSchedulePtr .EQ. 0)THEN
          CALL ShowWarningError(TRIM(cCurrentModuleObject)//' = "'//TRIM(VRF(VRFNum)%Name)//&
                     '", '// TRIM(cAlphaFieldNames(38))//' = "'//TRIM(cAlphaArgs(38))//&
                     '" was not found.')
          CALL ShowContinueError('Basin heater will be available to operate throughout the simulation.')
        END IF
      END IF

      VRF(VRFNum)%FuelType = FuelTypeElectric
      IF(.NOT. lAlphaFieldBlanks(39))THEN
        !A39; \field Fuel type
        IF (SameString(cAlphaArgs(39),"ELECTRICITY")) THEN
          VRF(VRFNum)%FuelType = FuelTypeElectric
        ELSE IF (SameString(cAlphaArgs(39),"ELECTRIC")) THEN
          VRF(VRFNum)%FuelType = FuelTypeElectric
        ELSE IF (SameString(cAlphaArgs(39),"NATURALGAS")) THEN
          VRF(VRFNum)%FuelType = FuelTypeNaturalGas
        ELSE IF (SameString(cAlphaArgs(39),"PROPANEGAS")) THEN
          VRF(VRFNum)%FuelType = FuelTypePropaneGas
        ELSE IF (SameString(cAlphaArgs(39),"DIESEL")) THEN
          VRF(VRFNum)%FuelType = FuelTypeDiesel
        ELSE IF (SameString(cAlphaArgs(39),"GASOLINE")) THEN
          VRF(VRFNum)%FuelType = FuelTypeGasoline
        ELSE IF (SameString(cAlphaArgs(39),"FUELOIL#1")) THEN
          VRF(VRFNum)%FuelType = FuelTypeFuelOil1
        ELSE IF (SameString(cAlphaArgs(39),"FUELOIL#2")) THEN
          VRF(VRFNum)%FuelType = FuelTypeFuelOil2
        ELSE IF (SameString(cAlphaArgs(39),'OtherFuel1')) THEN
          VRF(VRFNum)%FuelType = FuelTypeOtherFuel1
        ELSE IF (SameString(cAlphaArgs(39),'OtherFuel2')) THEN
          VRF(VRFNum)%FuelType = FuelTypeOtherFuel2
        ELSE
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//', "'//TRIM(VRF(VRFNum)%Name)//&
               '", '//TRIM(cAlphaFieldNames(39))//' not found = '//TRIM(cAlphaArgs(39)))
          CALL ShowContinueError('Valid choices are Electric, NaturalGas, PropaneGas, Diesel, Gasoline, FuelOil#1, '//  &
             'FuelOil#2, OtherFuel1 or OtherFuel2')
          ErrorsFound=.TRUE.
        END IF
      END IF

!  REAL(r64)    :: MinOATHeatRecovery         =0.0d0 ! Minimum outdoor air temperature for heat recovery operation (C)
!  REAL(r64)    :: MaxOATHeatRecovery         =0.0d0 ! Maximum outdoor air temperature for heat recovery operation (C)
      IF(VRF(VRFNum)%HeatRecoveryUsed)THEN
        IF(lNumericFieldBlanks(29))THEN
          VRF(VRFNum)%MinOATHeatRecovery = MAX(VRF(VRFNum)%MinOATCooling,VRF(VRFNum)%MinOATHeating)
        ELSE
          VRF(VRFNum)%MinOATHeatRecovery = rNumericArgs(29)
          IF(VRF(VRFNum)%MinOATHeatRecovery .LT. VRF(VRFNum)%MinOATCooling .OR. &
             VRF(VRFNum)%MinOATHeatRecovery .LT. VRF(VRFNum)%MinOATHeating)THEN
            CALL ShowWarningError(TRIM(cCurrentModuleObject)//' = "'//TRIM(VRF(VRFNum)%Name)//&
             '", '//TRIM(cNumericFieldNames(29))//' is less than the minimum temperature in heat pump mode.')
            CALL ShowContinueError('...'//TRIM(cNumericFieldNames(29))//' = '// &
                                   TRIM(TrimSigDigits(VRF(VRFNum)%MinOATHeatRecovery,2))//' C')
            CALL ShowContinueError('...Minimum Outdoor Temperature in Cooling Mode = '// &
                                   TRIM(TrimSigDigits(VRF(VRFNum)%MinOATCooling,2))//' C')
            CALL ShowContinueError('...Minimum Outdoor Temperature in Heating Mode = '// &
                                   TRIM(TrimSigDigits(VRF(VRFNum)%MinOATHeating,2))//' C')
            CALL ShowContinueError('...Minimum Outdoor Temperature in Heat Recovery Mode reset to greater'// &
                                   ' of cooling or heating minimum temperature and simulation continues.')
            VRF(VRFNum)%MinOATHeatRecovery = MAX(VRF(VRFNum)%MinOATCooling,VRF(VRFNum)%MinOATHeating)
            CALL ShowContinueError('... adjusted '//TRIM(cNumericFieldNames(29))//' = '// &
                                   TRIM(TrimSigDigits(VRF(VRFNum)%MinOATHeatRecovery,2))//' C')
          END IF
        END IF
        IF(lNumericFieldBlanks(30))THEN
          VRF(VRFNum)%MaxOATHeatRecovery = MIN(VRF(VRFNum)%MaxOATCooling,VRF(VRFNum)%MaxOATHeating)
        ELSE
          VRF(VRFNum)%MaxOATHeatRecovery = rNumericArgs(30)
          IF(VRF(VRFNum)%MaxOATHeatRecovery .GT. VRF(VRFNum)%MaxOATCooling .OR. &
             VRF(VRFNum)%MaxOATHeatRecovery .GT. VRF(VRFNum)%MaxOATHeating)THEN
            CALL ShowWarningError(TRIM(cCurrentModuleObject)//' = "'//TRIM(VRF(VRFNum)%Name)//&
             '", '//TRIM(cNumericFieldNames(30))//' is greater than the maximum temperature in heat pump mode.')
            CALL ShowContinueError('...'//TRIM(cNumericFieldNames(30))//' = '// &
                                   TRIM(TrimSigDigits(VRF(VRFNum)%MaxOATHeatRecovery,2))//' C')
            CALL ShowContinueError('...Maximum Outdoor Temperature in Cooling Mode = '// &
                                   TRIM(TrimSigDigits(VRF(VRFNum)%MaxOATCooling,2))//' C')
            CALL ShowContinueError('...Maximum Outdoor Temperature in Heating Mode = '// &
                                   TRIM(TrimSigDigits(VRF(VRFNum)%MaxOATHeating,2))//' C')
            CALL ShowContinueError('...Maximum Outdoor Temperature in Heat Recovery Mode reset to lesser'// &
                                   ' of cooling or heating minimum temperature and simulation continues.')
            VRF(VRFNum)%MaxOATHeatRecovery = MIN(VRF(VRFNum)%MaxOATCooling,VRF(VRFNum)%MaxOATHeating)
            CALL ShowContinueError('... adjusted '//TRIM(cNumericFieldNames(30))//' = '// &
                                   TRIM(TrimSigDigits(VRF(VRFNum)%MaxOATHeatRecovery,2))//' C')
          END IF
        END IF

!  INTEGER      :: HRCAPFTCool                =0   ! Index to cool capacity as a function of temperature curve for heat recovery
!  REAL(r64)    :: HRInitialCoolCapFrac       =0.0d0 ! Fractional cooling degradation at the start of heat recovery from cooling mode
!  REAL(r64)    :: HRCoolCapTC                =0.0d0 ! Time constant used to recover from intial degratation in cooling heat recovery
        VRF(VRFNum)%HRCAPFTCool = GetCurveIndex(cAlphaArgs(40))
        IF(VRF(VRFNum)%HRCAPFTCool .GT. 0)THEN
          ! Verify Curve Object, only legal type is bi-quadratic or linear, quadratic, or cubic
          SELECT CASE(GetCurveType(VRF(VRFNum)%HRCAPFTCool))
          CASE('LINEAR', 'QUADRATIC', 'CUBIC')
          CASE('BIQUADRATIC')
            VRF(VRFNum)%HRCAPFTCoolType = BIQUADRATIC
          CASE DEFAULT
            CALL ShowSevereError(TRIM(cCurrentModuleObject)//', "'//TRIM(VRF(VRFNum)%Name)// &
                     '" illegal '//TRIM(cAlphaFieldNames(40))//' type for this object = '// &
                              TRIM(GetCurveType(VRF(VRFNum)%HRCAPFTCool)))
            CALL ShowContinueError('... curve type must be Bi-Quadratic, Linear, Quadratic or Cubic.')
            ErrorsFound=.TRUE.
          END SELECT
        END IF
        IF(.NOT. lNumericFieldBlanks(31))THEN
          VRF(VRFNum)%HRInitialCoolCapFrac = rNumericArgs(31)
        END IF
        VRF(VRFNum)%HRCoolCapTC          = rNumericArgs(32)

!  INTEGER      :: HREIRFTCool                =0   ! Index to cool EIR as a function of temperature curve for heat recovery
!  REAL(r64)    :: HRInitialCoolEIRFrac       =0.0d0 ! Fractional EIR degradation at the start of heat recovery from cooling mode
!  REAL(r64)    :: HRCoolEIRTC                =0.0d0 ! Time constant used to recover from intial degratation in cooling heat recovery
        VRF(VRFNum)%HREIRFTCool = GetCurveIndex(cAlphaArgs(41))
        IF(VRF(VRFNum)%HREIRFTCool .GT. 0)THEN
          ! Verify Curve Object, only legal type is bi-quadratic or linear, quadratic, or cubic
          SELECT CASE(GetCurveType(VRF(VRFNum)%HREIRFTCool))
          CASE('LINEAR', 'QUADRATIC', 'CUBIC')
          CASE('BIQUADRATIC')
            VRF(VRFNum)%HREIRFTCoolType = BIQUADRATIC
          CASE DEFAULT
            CALL ShowSevereError(TRIM(cCurrentModuleObject)//', "'//TRIM(VRF(VRFNum)%Name)// &
                     '" illegal '//TRIM(cAlphaFieldNames(41))//' type for this object = '// &
                              TRIM(GetCurveType(VRF(VRFNum)%HREIRFTCool)))
            CALL ShowContinueError('... curve type must be Bi-Quadratic, Linear, Quadratic or Cubic.')
            ErrorsFound=.TRUE.
          END SELECT
        END IF
        VRF(VRFNum)%HRInitialCoolEIRFrac = rNumericArgs(33)
        VRF(VRFNum)%HRCoolEIRTC          = rNumericArgs(34)

!  INTEGER      :: HRCAPFTHeat                =0   ! Index to heat capacity as a function of temperature curve for heat recovery
!  REAL(r64)    :: HRInitialHeatCapFrac       =0.0d0 ! Fractional heating degradation at the start of heat recovery from heating mode
!  REAL(r64)    :: HRHeatCapTC                =0.0d0 ! Time constant used to recover from intial degratation in heating heat recovery
        VRF(VRFNum)%HRCAPFTHeat = GetCurveIndex(cAlphaArgs(42))
        IF(VRF(VRFNum)%HRCAPFTHeat .GT. 0)THEN
          ! Verify Curve Object, only legal type is bi-quadratic or linear, quadratic, or cubic
          SELECT CASE(GetCurveType(VRF(VRFNum)%HRCAPFTHeat))
          CASE('LINEAR', 'QUADRATIC', 'CUBIC')
          CASE('BIQUADRATIC')
            VRF(VRFNum)%HRCAPFTHeatType = BIQUADRATIC
          CASE DEFAULT
            CALL ShowSevereError(TRIM(cCurrentModuleObject)//', "'//TRIM(VRF(VRFNum)%Name)// &
                     '" illegal '//TRIM(cAlphaFieldNames(42))//' type for this object = '// &
                              TRIM(GetCurveType(VRF(VRFNum)%HRCAPFTHeat)))
            CALL ShowContinueError('... curve type must be Bi-Quadratic, Linear, Quadratic or Cubic.')
            ErrorsFound=.TRUE.
          END SELECT
        END IF
        VRF(VRFNum)%HRInitialHeatCapFrac = rNumericArgs(35)
        VRF(VRFNum)%HRHeatCapTC          = rNumericArgs(36)

!  INTEGER      :: HREIRFTHeat                =0   ! Index to heat EIR as a function of temperature curve for heat recovery
!  REAL(r64)    :: HRInitialHeatEIRFrac       =0.0d0 ! Fractional EIR degradation at the start of heat recovery from heating mode
!  REAL(r64)    :: HRHeatEIRTC                =0.0d0 ! Time constant used to recover from intial degratation in heating heat recovery
        VRF(VRFNum)%HREIRFTHeat = GetCurveIndex(cAlphaArgs(43))
        IF(VRF(VRFNum)%HREIRFTHeat .GT. 0)THEN
          ! Verify Curve Object, only legal type is bi-quadratic or linear, quadratic, or cubic
          SELECT CASE(GetCurveType(VRF(VRFNum)%HREIRFTHeat))
          CASE('LINEAR', 'QUADRATIC', 'CUBIC')
          CASE('BIQUADRATIC')
            VRF(VRFNum)%HREIRFTHeatType = BIQUADRATIC
          CASE DEFAULT
            CALL ShowSevereError(TRIM(cCurrentModuleObject)//', "'//TRIM(VRF(VRFNum)%Name)// &
                     '" illegal '//TRIM(cAlphaFieldNames(43))//' type for this object = '// &
                              TRIM(GetCurveType(VRF(VRFNum)%HREIRFTHeat)))
            CALL ShowContinueError('... curve type must be Bi-Quadratic, Linear, Quadratic or Cubic.')
            ErrorsFound=.TRUE.
          END SELECT
        END IF
        VRF(VRFNum)%HRInitialHeatEIRFrac = rNumericArgs(37)
        VRF(VRFNum)%HRHeatEIRTC          = rNumericArgs(38)

      ELSE
      END IF

    IF(VRF(VRFNum)%CondenserType == WaterCooled)THEN

    !scan for loop connection data
   errFlag=.false.
    CALL ScanPlantLoopsForObject(VRF(VRFNum)%Name, &
                                 VRF(VRFNum)%VRFPlantTypeOfNum, &
                                 VRF(VRFNum)%SourceLoopNum, &
                                 VRF(VRFNum)%SourceLoopSideNum, &
                                 VRF(VRFNum)%SourceBranchNum, &
                                 VRF(VRFNum)%SourceCompNum, &
                                 inletNodeNumber = VRF(VRFNum)%CondenserNodeNum,  &
                                 errflag=errFlag)

    IF (errFlag) THEN
      CALL ShowSevereError('GetVRFInput: Error scanning for plant loop data')
      ErrorsFound=.TRUE.
    ENDIF

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

      CALL GetObjectItem(cCurrentModuleObject,VRFTUNum,cAlphaArgs,NumAlphas, &
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
          VRFTU(VRFTUNum)%IndexToTUInTUList = ZoneTerminalUnitListNum
          TerminalUnitList(NumList)%ZoneTUPtr(ZoneTerminalUnitListNum) = VRFTUNum
          VRFTU(VRFTUNum)%TUListIndex = NumList
          EXIT
        END IF
      END DO
      IF(VRFTU(VRFTUNum)%TUListIndex .EQ. 0)THEN
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//' = '//TRIM(VRFTU(VRFTUNum)%Name))
        CALL ShowContinueError('Terminal unit not found on any ZoneTerminalUnitList.')
        ErrorsFound=.true.
      END IF

      DO NumCond = 1, NumVRFCond
        IF(VRF(NumCond)%ZoneTUListPtr /= VRFTU(VRFTUNum)%TUListIndex)CYCLE
        VRFTU(VRFTUNum)%VRFSysNum = NumCond
        EXIT
      END DO
      VRFTU(VRFTUNum)%VRFTUType_Num = VRFTUType_ConstVolume
      IF (lAlphaFieldBlanks(2)) THEN
        VRFTU(VRFTUNum)%SchedPtr = ScheduleAlwaysOn
      ELSE
        VRFTU(VRFTUNum)%SchedPtr = GetScheduleIndex(cAlphaArgs(2))
        IF (VRFTU(VRFTUNum)%SchedPtr == 0) THEN
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(VRFTU(VRFTUNum)%Name)//'" invalid data')
          CALL ShowContinueError('Invalid-not found '//trim(cAlphaFieldNames(2))//'="'//    &
             trim(cAlphaArgs(2))//'".')
          ErrorsFound=.true.
        ENDIF
      ENDIF

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
      IF(VRFTU(VRFTUNum)%FanOpModeSchedPtr == 0)THEN
        IF(.NOT. lAlphaFieldBlanks(5))THEN
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//' = '//TRIM(VRFTU(VRFTUNum)%Name))
          CALL ShowContinueError('...'//TRIM(cAlphaFieldNames(5))//' = '//TRIM(cAlphaArgs(5))//' not found.')
          CALL ShowContinueError('...Defaulting to constant fan operating mode and simulation continues.')
        END IF
        VRFTU(VRFTUNum)%OpMode = ContFanCycCoil
      END IF

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
            IF (.NOT. CheckScheduleValueMinMax(VRFTU(VRFTUNum)%FanOpModeSchedPtr,'>',0.0d0,'<=',1.0d0)) THEN
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
         IF(VRFTU(VRFTUNum)%TUListIndex .GT. 0 .AND. VRFTU(VRFTUNum)%IndexToTUInTUList .GT. 0)THEN
           TerminalUnitList(VRFTU(VRFTUNum)%TUListIndex)%CoolingCoilPresent(VRFTU(VRFTUNum)%IndexToTUInTUList) = .FALSE.
         END IF
       ELSE
         IF (SameString(cAllCoilTypes(VRFTU(VRFTUNum)%DXCoolCoilType_Num),cAllCoilTypes(CoilVRF_Cooling))) THEN
           ErrFlag = .FALSE.
           TerminalUnitList(VRFTU(VRFTUNum)%TUListIndex)%CoolingCoilAvailSchPtr(VRFTU(VRFTUNum)%IndexToTUInTUList) = &
               GetDXCoilAvailSchPtr(DXCoolingCoilType,cAlphaArgs(12),ErrFlag)
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

      ! Get the heating to cooling sizing ratio input before writing to DX heating coil data
      IF(.NOT. lNumericFieldBlanks(10))THEN
        VRFTU(VRFTUNum)%HeatingCapacitySizeRatio = rNumericArgs(10)
      END IF

      ErrFlag = .FALSE.
      VRFTU(VRFTUNum)%DXHeatCoilType_Num = GetCoilTypeNum(TRIM(DXHeatingCoilType),cAlphaArgs(14),ErrFlag,.FALSE.)
      IF(VRFTU(VRFTUNum)%DXHeatCoilType_Num == 0)THEN
         VRFTU(VRFTUNum)%HeatingCoilPresent = .FALSE.
         IF(VRFTU(VRFTUNum)%TUListIndex .GT. 0 .AND. VRFTU(VRFTUNum)%IndexToTUInTUList .GT. 0)THEN
           TerminalUnitList(VRFTU(VRFTUNum)%TUListIndex)%HeatingCoilPresent(VRFTU(VRFTUNum)%IndexToTUInTUList) = .FALSE.
         END IF
      ELSE
        IF (SameString(cAllCoilTypes(VRFTU(VRFTUNum)%DXHeatCoilType_Num),cAllCoilTypes(CoilVRF_Heating))) THEN
          ErrFlag = .FALSE.
          TerminalUnitList(VRFTU(VRFTUNum)%TUListIndex)%HeatingCoilAvailSchPtr(VRFTU(VRFTUNum)%IndexToTUInTUList) = &
               GetDXCoilAvailSchPtr(DXHeatingCoilType,cAlphaArgs(14),ErrFlag)
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
             ! If defrost is disabled in the VRF condenser, it must be disabled in the DX coil
             ! Defrost primarily handled in parent object, set defrost capacity to 1 to avoid autosizing.
             ! Defrost capacity is used for nothing more than setting defrost power/consumption report
             ! variables which are not reported. The coil's defrost algorythm IS used to derate the coil
             CALL SetDXCoolingCoilData(VRFTU(VRFTUNum)%HeatCoilIndex,ErrorsFound, &
                    DefrostCapacity=1.d0)
             ! Terminal unit heating to cooling sizing ratio has precedence over VRF system sizing ratio
             IF(VRFTU(VRFTUNum)%HeatingCapacitySizeRatio .GT. 1.d0)THEN
               CALL SetDXCoolingCoilData(VRFTU(VRFTUNum)%HeatCoilIndex,ErrorsFound, &
                    HeatSizeRatio=VRFTU(VRFTUNum)%HeatingCapacitySizeRatio)
             ELSE IF(VRF(VRFTU(VRFTUNum)%VRFSysNum)%HeatingCapacitySizeRatio .GT. 1.d0)THEN
               CALL SetDXCoolingCoilData(VRFTU(VRFTUNum)%HeatCoilIndex,ErrorsFound, &
                    HeatSizeRatio=VRF(VRFTU(VRFTUNum)%VRFSysNum)%HeatingCapacitySizeRatio)
             END IF
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

      IF (.NOT. lAlphaFieldBlanks(15)) THEN
        VRFTU(VRFTUNum)%AvailManagerListName = cAlphaArgs(15)
        ZoneComp(VRFTerminalUnit_Num)%ZoneCompAvailMgrs(VRFTUNum)%AvailManagerListName  = cAlphaArgs(15)
      ENDIF
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
                                 ' improperly connected to system.')
          CALL ShowContinueError('...either the ZoneHVAC:TerminalUnit:VariableRefrigerantFlow object does not exist,')
          CALL ShowContinueError('...or the ZoneTerminalUnitList object is not named in an '// &
                                 'AirConditioner:VariableRefrigerantFlow object.')
          ErrorsFound = .TRUE.
      END DO
    END DO

!   warn when number of ZoneTerminalUnitList different from number of AirConditioner:VariableRefrigerantFlow
    IF(NumVRFTULists /= NumVRFCond)THEN
      CALL ShowSevereError('The number of AirConditioner:VariableRefrigerantFlow objects ('//TRIM(TrimSigDigits(NumVRFCond,0))// &
                    ') does not match the number of ZoneTerminalUnitList objects ('//TRIM(TrimSigDigits(NumVRFTULists,0))//').')
      DO NumCond = 1, NumVRFCond
        CALL ShowContinueError('...AirConditioner:VariableRefrigerantFlow = '//TRIM(VRF(NumCond)%Name)// &
                       ' specifies Zone Terminal Unit List Name = '//TRIM(TerminalUnitList(VRF(NumCond)%ZoneTUListPtr)%Name))
      END DO
      CALL ShowContinueError('...listing ZoneTerminalUnitList objects.')
      DO NumList = 1,  NumVRFTULists
        CALL ShowContinueError('...ZoneTerminalUnitList = '//TRIM(TerminalUnitList(NumList)%Name))
      END DO
      ErrorsFound = .TRUE.
    END IF

    DO VRFNum = 1,  NumVRFTU
      IF(VRFTU(VRFNum)%CoolingCoilPresent)THEN
        CALL SetupOutputVariable('Zone VRF Air Terminal Cooling Electric Power [W]', &
                                  VRFTU(VRFNum)%ParasiticCoolElecPower,'System','Average', VRFTU(VRFNum)%Name)
        CALL SetupOutputVariable('Zone VRF Air Terminal Cooling Electric Energy [J]', &
                                  VRFTU(VRFNum)%ParasiticElecCoolConsumption, 'System','Sum', VRFTU(VRFNum)%Name, &
                                  ResourceTypeKey='Electric',EndUseKey='COOLING',GroupKey='System')
        CALL SetupOutputVariable('Zone VRF Air Terminal Total Cooling Rate [W]', &
                                  VRFTU(VRFNum)%TotalCoolingRate,'System','Average', VRFTU(VRFNum)%Name)
        CALL SetupOutputVariable('Zone VRF Air Terminal Sensible Cooling Rate [W]', &
                                  VRFTU(VRFNum)%SensibleCoolingRate,'System','Average', VRFTU(VRFNum)%Name)
        CALL SetupOutputVariable('Zone VRF Air Terminal Latent Cooling Rate [W]', &
                                  VRFTU(VRFNum)%LatentCoolingRate,'System','Average', VRFTU(VRFNum)%Name)
        CALL SetupOutputVariable('Zone VRF Air Terminal Total Cooling Energy [J]', &
                                  VRFTU(VRFNum)%TotalCoolingEnergy, 'System','Sum', VRFTU(VRFNum)%Name)
        CALL SetupOutputVariable('Zone VRF Air Terminal Sensible Cooling Energy [J]', &
                                  VRFTU(VRFNum)%SensibleCoolingEnergy, 'System','Sum', VRFTU(VRFNum)%Name)
        CALL SetupOutputVariable('Zone VRF Air Terminal Latent Cooling Energy [J]', &
                                  VRFTU(VRFNum)%LatentCoolingEnergy, 'System','Sum', VRFTU(VRFNum)%Name)
      END IF
      IF(VRFTU(VRFNum)%HeatingCoilPresent)THEN
        CALL SetupOutputVariable('Zone VRF Air Terminal Heating Electric Power [W]', &
                                  VRFTU(VRFNum)%ParasiticHeatElecPower,'System','Average', VRFTU(VRFNum)%Name)
        CALL SetupOutputVariable('Zone VRF Air Terminal Heating Electric Energy [J]', &
                                  VRFTU(VRFNum)%ParasiticElecHeatConsumption, 'System','Sum', VRFTU(VRFNum)%Name, &
                                  ResourceTypeKey='Electric',EndUseKey='HEATING',GroupKey='System')
        CALL SetupOutputVariable('Zone VRF Air Terminal Total Heating Rate [W]', &
                                  VRFTU(VRFNum)%TotalHeatingRate,'System','Average', VRFTU(VRFNum)%Name)
        CALL SetupOutputVariable('Zone VRF Air Terminal Sensible Heating Rate [W]', &
                                  VRFTU(VRFNum)%SensibleHeatingRate,'System','Average', VRFTU(VRFNum)%Name)
        CALL SetupOutputVariable('Zone VRF Air Terminal Latent Heating Rate [W]', &
                                  VRFTU(VRFNum)%LatentHeatingRate,'System','Average', VRFTU(VRFNum)%Name)
        CALL SetupOutputVariable('Zone VRF Air Terminal Total Heating Energy [J]', &
                                  VRFTU(VRFNum)%TotalHeatingEnergy, 'System','Sum', VRFTU(VRFNum)%Name)
        CALL SetupOutputVariable('Zone VRF Air Terminal Sensible Heating Energy [J]', &
                                  VRFTU(VRFNum)%SensibleHeatingEnergy, 'System','Sum', VRFTU(VRFNum)%Name)
        CALL SetupOutputVariable('Zone VRF Air Terminal Latent Heating Energy [J]', &
                                  VRFTU(VRFNum)%LatentHeatingEnergy, 'System','Sum', VRFTU(VRFNum)%Name)
      END IF
      CALL SetupOutputVariable('Zone VRF Air Terminal Fan Availability Status []',&
                                VRFTU(VRFNum)%AvailStatus,'System','Average',VRFTU(VRFNum)%Name)
      IF (AnyEnergyManagementSystemInModel) THEN
        CALL SetupEMSActuator('Variable Refrigerant Flow Terminal Unit', VRFTU(VRFNum)%Name, 'Part Load Ratio' , '[fraction]', &
                           VRFTU(VRFNum)%EMSOverridePartLoadFrac, VRFTU(VRFNum)%EMSValueForPartLoadFrac )
      ENDIF
    END DO

    DO NumCond = 1, NumVRFCond
      CALL SetupOutputVariable('VRF Heat Pump Total Cooling Rate [W]', &
                                VRF(NumCond)%TotalCoolingCapacity, 'System','Average', VRF(NumCond)%Name)
      CALL SetupOutputVariable('VRF Heat Pump Total Heating Rate [W]', &
                                VRF(NumCond)%TotalHeatingCapacity, 'System','Average', VRF(NumCond)%Name)
      IF (VRF(NumCond)%FuelType == FuelTypeElectric) THEN
        CALL SetupOutputVariable('VRF Heat Pump Cooling Electric Power [W]', &
                                  VRF(NumCond)%ElecCoolingPower, 'System','Average', VRF(NumCond)%Name)
        CALL SetupOutputVariable('VRF Heat Pump Cooling Electric Energy [J]', &
                                  VRF(NumCond)%CoolElecConsumption, 'System','Sum', VRF(NumCond)%Name, &
                                  ResourceTypeKey=TRIM(cValidFuelTypes(VRF(NumCond)%FuelType)), &
                                  EndUseKey='COOLING',GroupKey='System')
      ELSE
        CALL SetupOutputVariable('VRF Heat Pump Cooling '// &
                                  TRIM(cValidFuelTypes(VRF(NumCond)%FuelType))//' Rate [W]', &
                                  VRF(NumCond)%ElecCoolingPower, 'System','Average', VRF(NumCond)%Name)
        CALL SetupOutputVariable('VRF Heat Pump Cooling '// &
                                  TRIM(cValidFuelTypes(VRF(NumCond)%FuelType))//' Energy [J]', &
                                  VRF(NumCond)%CoolElecConsumption, 'System','Sum', VRF(NumCond)%Name, &
                                  ResourceTypeKey=TRIM(cValidFuelTypes(VRF(NumCond)%FuelType)), &
                                  EndUseKey='COOLING',GroupKey='System')
      ENDIF
      IF (VRF(NumCond)%FuelType == FuelTypeElectric) THEN
        CALL SetupOutputVariable('VRF Heat Pump Heating Electric Power [W]', &
                                  VRF(NumCond)%ElecHeatingPower, 'System','Average', VRF(NumCond)%Name)
        CALL SetupOutputVariable('VRF Heat Pump Heating Electric Energy [J]', &
                                  VRF(NumCond)%HeatElecConsumption, 'System','Sum', VRF(NumCond)%Name, &
                                  ResourceTypeKey=TRIM(cValidFuelTypes(VRF(NumCond)%FuelType)), &
                                  EndUseKey='HEATING',GroupKey='System')
      ELSE
        CALL SetupOutputVariable('VRF Heat Pump Heating '// &
                                  TRIM(cValidFuelTypes(VRF(NumCond)%FuelType))//' Rate [W]', &
                                  VRF(NumCond)%ElecHeatingPower, 'System','Average', VRF(NumCond)%Name)
        CALL SetupOutputVariable('VRF Heat Pump Heating '// &
                                  TRIM(cValidFuelTypes(VRF(NumCond)%FuelType))//' Energy [J]', &
                                  VRF(NumCond)%HeatElecConsumption, 'System','Sum', VRF(NumCond)%Name, &
                                  ResourceTypeKey=TRIM(cValidFuelTypes(VRF(NumCond)%FuelType)), &
                                  EndUseKey='HEATING',GroupKey='System')
      ENDIF

      CALL SetupOutputVariable('VRF Heat Pump Cooling COP []', &
                                VRF(NumCond)%OperatingCoolingCOP,'System','Average', VRF(NumCond)%Name)
      CALL SetupOutputVariable('VRF Heat Pump Heating COP []', &
                                VRF(NumCond)%OperatingHeatingCOP,'System','Average', VRF(NumCond)%Name)
      CALL SetupOutputVariable('VRF Heat Pump COP []', &
                                VRF(NumCond)%OperatingCOP,'System','Average', VRF(NumCond)%Name)

      IF(VRF(NumCond)%DefrostStrategy == Resistive .OR. &
        (VRF(NumCond)%DefrostStrategy == ReverseCycle .AND. VRF(NumCond)%FuelType == FuelTypeElectric))THEN
        CALL SetupOutputVariable('VRF Heat Pump Defrost Electric Power [W]', &
                                VRF(NumCond)%DefrostPower, 'System','Average', VRF(NumCond)%Name)
        CALL SetupOutputVariable('VRF Heat Pump Defrost Electric Energy [J]', &
                                VRF(NumCond)%DefrostConsumption, 'System','Sum',VRF(NumCond)%Name, &
                                ResourceTypeKey='Electric',EndUseKey='HEATING',GroupKey='System')
      ELSE ! defrost energy appied to fuel type
        CALL SetupOutputVariable('VRF Heat Pump Defrost '// &
                                TRIM(cValidFuelTypes(VRF(NumCond)%FuelType))//' Rate [W]', &
                                VRF(NumCond)%DefrostPower, 'System','Average', VRF(NumCond)%Name)
        CALL SetupOutputVariable('VRF Heat Pump Defrost '// &
                                TRIM(cValidFuelTypes(VRF(NumCond)%FuelType))//' Energy [J]', &
                                VRF(NumCond)%DefrostConsumption, 'System','Sum', VRF(NumCond)%Name, &
                                ResourceTypeKey=TRIM(cValidFuelTypes(VRF(NumCond)%FuelType)), &
                                EndUseKey='HEATING',GroupKey='System')
      END IF

      CALL SetupOutputVariable('VRF Heat Pump Part Load Ratio []', &
                                VRF(NumCond)%VRFCondPLR, 'System','Average', VRF(NumCond)%Name)
      CALL SetupOutputVariable('VRF Heat Pump Runtime Fraction []', &
                                VRF(NumCond)%VRFCondRTF, 'System','Average', VRF(NumCond)%Name)
      CALL SetupOutputVariable('VRF Heat Pump Cycling Ratio []', &
                                VRF(NumCond)%VRFCondCyclingRatio, 'System','Average',VRF(NumCond)%Name)

      CALL SetupOutputVariable('VRF Heat Pump Operating Mode []', &
                                VRF(NumCond)%OperatingMode, 'System','Average', VRF(NumCond)%Name)
      CALL SetupOutputVariable('VRF Heat Pump Condenser Inlet Temperature [C]', &
                                VRF(NumCond)%CondenserInletTemp, 'System','Average', VRF(NumCond)%Name)
      CALL SetupOutputVariable('VRF Heat Pump Maximum Capacity Cooling Rate [W]', &
                                MaxCoolingCapacity(NumCond), 'System','Average', VRF(NumCond)%Name)
      CALL SetupOutputVariable('VRF Heat Pump Maximum Capacity Heating Rate [W]', &
                                MaxHeatingCapacity(NumCond), 'System','Average', VRF(NumCond)%Name)

      CALL SetupOutputVariable('VRF Heat Pump Crankcase Heater Electric Power [W]', &
                                VRF(NumCond)%CrankCaseHeaterPower, 'System','Average', VRF(NumCond)%Name)
      CALL SetupOutputVariable('VRF Heat Pump Crankcase Heater Electric Energy [J]', &
                                VRF(NumCond)%CrankCaseHeaterElecConsumption, 'System','Sum', VRF(NumCond)%Name, &
                                ResourceTypeKey='Electric',EndUseKey='COOLING',GroupKey='System')
      CALL SetupOutputVariable('VRF Heat Pump Terminal Unit Cooling Load Rate [W]', &
                                VRF(NumCond)%TUCoolingLoad, 'System','Average', VRF(NumCond)%Name)
      CALL SetupOutputVariable('VRF Heat Pump Terminal Unit Heating Load Rate [W]', &
                                VRF(NumCond)%TUHeatingLoad, 'System','Average', VRF(NumCond)%Name)
      IF(VRF(NumCond)%HeatRecoveryUsed)THEN
        CALL SetupOutputVariable('VRF Heat Pump Heat Recovery Status Change Multiplier []', &
                                  VRF(NumCond)%SUMultiplier, 'System','Average',VRF(NumCond)%Name)
      END IF

      IF(VRF(NumCond)%CondenserType .EQ. EvapCooled)THEN
        CALL SetupOutputVariable('VRF Heat Pump Evaporative Condenser Water Use Volume [m3]', &
                                  VRF(NumCond)%EvapWaterConsumpRate, 'System','Sum',VRF(NumCond)%Name, &
                                  ResourceTypeKey='Water',EndUseKey='Cooling',GroupKey='System')
        CALL SetupOutputVariable('VRF Heat Pump Evaporative Condenser Pump Electric Power [W]', &
                                  VRF(NumCond)%EvapCondPumpElecPower, 'System','Average',VRF(NumCond)%Name)
        CALL SetupOutputVariable('VRF Heat Pump Evaporative Condenser Pump Electric Energy [J]', &
                                  VRF(NumCond)%EvapCondPumpElecConsumption,'System','Sum',VRF(NumCond)%Name, &
                                  ResourceTypeKey='Electric',EndUseKey='COOLING',GroupKey='System')

        IF(VRF(NumCond)%BasinHeaterPowerFTempDiff .GT. 0.0d0)THEN
          CALL SetupOutputVariable('VRF Heat Pump Basin Heater Electric Power [W]', &
                                    VRF(NumCond)%BasinHeaterPower,'System','Average',VRF(NumCond)%Name)
          CALL SetupOutputVariable('VRF Heat Pump Basin Heater Electric Energy [J]', &
                                    VRF(NumCond)%BasinHeaterConsumption,'System','Sum',VRF(NumCond)%Name, &
                                    ResourceTypeKey='Electric',EndUseKey='COOLING',GroupKey='System')
        END IF

      ELSE IF(VRF(NumCond)%CondenserType .EQ. WaterCooled)THEN
        CALL SetupOutputVariable('VRF Heat Pump Condenser Outlet Temperature [C]', &
                                  VRF(NumCond)%CondenserSideOutletTemp, 'System','Average',VRF(NumCond)%Name)
        CALL SetupOutputVariable('VRF Heat Pump Condenser Mass Flow Rate [kg/s]', &
                                  VRF(NumCond)%WaterCondenserMassFlow, 'System','Average',VRF(NumCond)%Name)
        CALL SetupOutputVariable('VRF Heat Pump Condenser Heat Transfer Rate [W]', &
                                  VRF(NumCond)%QCondenser, 'System','Average',VRF(NumCond)%Name)
        CALL SetupOutputVariable('VRF Heat Pump Condenser Heat Transfer Energy [J]', &
                                  VRF(NumCond)%QCondEnergy, 'System','Sum',VRF(NumCond)%Name)
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

SUBROUTINE InitVRF(VRFTUNum, ZoneNum, FirstHVACIteration, OnOffAirFlowRatio, QZnReq)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Raustad, FSEC
          !       DATE WRITTEN   August 2010
          !       MODIFIED       July 2012, Chandan Sharma - FSEC: Added zone sys avail managers
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is for initializations of the VRF Components.

          ! METHODOLOGY EMPLOYED:
          ! Uses the status flags to trigger initializations.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataZoneEquipment, ONLY: ZoneEquipInputsFilled,CheckZoneEquipmentList,VRFTerminalUnit_Num
  USE DataHeatBalFanSys, ONLY: TempControlType, ZT, ZoneThermostatSetPointHi, ZoneThermostatSetPointLo
  USE InputProcessor,    ONLY: SameString
  USE ScheduleManager,   ONLY: GetCurrentScheduleValue
  USE DataEnvironment,   ONLY: StdBaroPress, StdRhoAir, OutDryBulbTemp, OutWetBulbTemp
  USE MixedAir,          ONLY: SimOAMixer, SimOAController
  USE DataZoneEquipment, ONLY: ZoneEquipList
  USE DataSizing,        ONLY: AutoSize
  USE Fans,              ONLY: GetFanVolFlow
  USE General,           ONLY: TrimSigDigits, RoundSigDigits
  USE FluidProperties,   ONLY: GetDensityGlycol
  USE PlantUtilities,    ONLY: InitComponentNodes

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  Integer, Intent(IN)      :: VRFTUNum
  INTEGER, INTENT (IN)     :: ZoneNum
  LOGICAL, INTENT(IN)      :: FirstHVACIteration
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
  LOGICAL, ALLOCATABLE,SAVE, DIMENSION(:) :: MyVRFCondFlag     ! used to reset timer counter
  INTEGER   :: NumTULoop        ! loop counter, number of TU's in list
  INTEGER   :: ELLoop           ! loop counter, number of zone equipment lists
  INTEGER   :: ListLoop         ! loop counter, number of equipment is each list
  INTEGER   :: VRFCond          ! index to VRF condenser
  INTEGER   :: TUIndex          ! index to TU
  INTEGER   :: TUListNum        ! index to VRF AC system terminal unit list
  INTEGER   :: TUListIndex      ! pointer to TU list for this VRF system
  INTEGER   :: IndexToTUInTUList      ! index to TU in TerminalUnilList
  REAL(r64) :: RhoAir           ! air density at InNode
  REAL(r64), SAVE :: CurrentEndTime     ! end time of current time step
  REAL(r64), SAVE :: CurrentEndTimeLast ! end time of last time step
  REAL(r64), SAVE :: TimeStepSysLast    ! system time step on last time step
  REAL(r64) :: TempOutput       ! Sensible output of TU
  REAL(r64) :: LoadToCoolingSP  ! thermostat load to cooling setpoint (W)
  REAL(r64) :: LoadToHeatingSP  ! thermostat load to heating setpoint (W)
  LOGICAL   :: EnableSystem     ! use to turn on secondary operating mode if OA temp limits exceeded
  REAL(r64) :: rho              ! density of water (kg/m3)
  REAL(r64):: OutsideDryBulbTemp ! Outdoor air temperature at external node height


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
    ALLOCATE(MyVRFCondFlag(NumVRFCond))
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

    MyOneTimeFlag = .FALSE.
    MyVRFCondFlag = .TRUE.

  END IF ! IF (MyOneTimeFlag) THEN

  ! identify VRF condenser connected to this TU
  VRFCond = VRFTU(VRFTUNum)%VRFSysNum
  TUListIndex = VRF(VRFCond)%ZoneTUListPtr
  InNode  = VRFTU(VRFTUNum)%VRFTUInletNodeNum
  OutNode = VRFTU(VRFTUNum)%VRFTUOutletNodeNum
  OutsideAirNode = VRFTU(VRFTUNum)%VRFTUOAMixerOANodeNum
  IndexToTUInTUList = VRFTU(VRFTUNum)%IndexToTUInTUList

  ! set condenser inlet temp, used as surrogate for OAT (used to check limits of operation)
  IF(VRF(VRFCond)%CondenserType == WaterCooled) THEN
    OutsideDryBulbTemp = Node(VRF(VRFCond)%CondenserNodeNum)%Temp
  ELSE
    IF(OutsideAirNode .EQ. 0)THEN
      OutsideDryBulbTemp = OutDryBulbTemp
    ELSE
      OutsideDryBulbTemp = Node(OutsideAirNode)%Temp
    END IF
  END IF

  IF (ALLOCATED(ZoneComp)) THEN
    ZoneComp(VRFTerminalUnit_Num)%ZoneCompAvailMgrs(VRFTUNum)%ZoneNum = ZoneNum
    VRFTU(VRFTUNum)%AvailStatus = ZoneComp(VRFTerminalUnit_Num)%ZoneCompAvailMgrs(VRFTUNum)%AvailStatus
  ENDIF

  ! If all VRF Terminal Units on this VRF AC System have been simulated, reset the IsSimulated flag
  ! The condenser will be simulated after all terminal units have been simulated (see Sub SimulateVRF)
  IF(ALL(TerminalUnitList(TUListIndex)%IsSimulated))THEN
!   this should be the first time through on the next iteration. All TU's and condenser have been simulated.
!   reset simulation flag for each terminal unit
    TerminalUnitList(TUListIndex)%IsSimulated = .FALSE.
!     after all TU's have been simulated, reset operating mode flag if necessary
      IF(LastModeHeating(VRFCond) .AND. CoolingLoad(VRFCond))THEN
        LastModeCooling(VRFCond) = .TRUE.
        LastModeHeating(VRFCond) = .FALSE.
!        SwitchedMode(VRFCond)    = .TRUE.
      END IF
      IF(LastModeCooling(VRFCond) .AND. HeatingLoad(VRFCond))THEN
        LastModeHeating(VRFCond) = .TRUE.
        LastModeCooling(VRFCond) = .FALSE.
!        SwitchedMode(VRFCond)    = .TRUE.
      END IF
  END IF ! IF(ALL(TerminalUnitList(VRFTU(VRFTUNum)%TUListIndex)%IsSimulated))THEN

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
      TerminalUnitList(TUListIndex)%TerminalUnitNotSizedYet(IndexToTUInTUList) = .FALSE.
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
      Node(OutsideAirNode)%MassFlowRateMin = 0.0d0
      Node(OutsideAirNode)%MassFlowRateMinAvail = 0.0d0
    END IF
    Node(OutNode)%MassFlowRateMax = MAX(VRFTU(VRFTUNum)%MaxCoolAirMassFlow,VRFTU(VRFTUNum)%MaxHeatAirMassFlow)
    Node(OutNode)%MassFlowRateMin = 0.0d0
    Node(OutNode)%MassFlowRateMinAvail = 0.0d0
    Node(InNode)%MassFlowRateMax = MAX(VRFTU(VRFTUNum)%MaxCoolAirMassFlow,VRFTU(VRFTUNum)%MaxHeatAirMassFlow)
    Node(InNode)%MassFlowRateMin = 0.0d0
    Node(InNode)%MassFlowRateMinAvail = 0.0d0
    IF(VRFTU(VRFTUNum)%VRFTUOAMixerRelNodeNum .GT. 0)THEN
      Node(VRFTU(VRFTUNum)%VRFTUOAMixerRelNodeNum)%MassFlowRateMinAvail = 0.0d0
    END IF

    MyEnvrnFlag(VRFTUNum) = .FALSE.

    IF(VRF(VRFCond)%CondenserType == WaterCooled) THEN
      rho = GetDensityGlycol(PlantLoop(VRF(VRFCond)%SourceLoopNum)%FluidName, &
                         InitconvTemp, &
                         PlantLoop(VRF(VRFCond)%SourceLoopNum)%FluidIndex, &
                         'InitVRF')
      VRF(VRFCond)%WaterCondenserDesignMassFlow = VRF(VRFCond)%WaterCondVolFlowRate * rho

      CALL InitComponentNodes( 0.d0,VRF(VRFCond)%WaterCondenserDesignMassFlow, &
                                 VRF(VRFCond)%CondenserNodeNum, &
                                 VRF(VRFCond)%CondenserOutletNodeNum, &
                                 VRF(VRFCond)%SourceLoopNum, &
                                 VRF(VRFCond)%SourceLoopSideNum, &
                                 VRF(VRFCond)%SourceBranchNum, &
                                 VRF(VRFCond)%SourceCompNum)
    END IF
!    IF(MyVRFCondFlag(VRFCond))THEN
      VRF(VRFCond)%HRTimer      = 0.d0
      VRF(VRFCond)%ModeChange   = .FALSE.
      VRF(VRFCond)%HRModeChange = .FALSE.
      MyVRFCondFlag(VRFCond)    = .FALSE.
!    END IF
  END IF ! IF (BeginEnvrnFlag .and. MyEnvrnFlag(VRFTUNum)) THEN

  ! reset environment flag for next environment
  IF (.not. BeginEnvrnFlag) THEN
    MyEnvrnFlag(VRFTUNum) = .TRUE.
    MyVRFCondFlag(VRFCond) = .TRUE.
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


        IF(VRFTU(VRFTUNum)%ActualFanVolFlowRate .GT. 0.0d0)THEN
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

!  TUListNum = VRFTU(VRFTUNum)%TUListIndex

  IF (VRFTU(VRFTUNum)%FanOpModeSchedPtr .GT. 0) THEN
    IF (GetCurrentScheduleValue(VRFTU(VRFTUNum)%FanOpModeSchedPtr) .EQ. 0.0d0) THEN
      VRFTU(VRFTUNum)%OpMode = CycFanCycCoil
    ELSE
      VRFTU(VRFTUNum)%OpMode = ContFanCycCoil
    END IF
  END IF

  ! if condenser is off, all terminal unit coils are off
!!!LKL Discrepancy < 0
  IF (GetCurrentScheduleValue(VRF(VRFCond)%SchedPtr) .EQ. 0.0d0) THEN
    HeatingLoad(VRFCond) = .FALSE.
    CoolingLoad(VRFCond) = .FALSE.
  ELSE

!*** Operating Mode Initialization done at beginning of each iteration ***!
!*** assumes all TU's and Condeser were simulated last iteration ***!
!*** this code is done ONCE each iteration when all TU's IsSimulated flag is FALSE ***!
    ! Determine operating mode prior to simulating any terminal units connected to a VRF condenser
    ! this should happen at the beginning of a time step where all TU's are polled to see what
    ! mode the heat pump condenser will operate in
    IF(.NOT. ANY(TerminalUnitList(TUListIndex)%IsSimulated))THEN
      CALL InitializeOperatingMode(FirstHVACIteration,VRFCond,TUListIndex,OnOffAirFlowRatio)
    END IF  ! IF(.NOT. ANY(TerminalUnitList(TUListNum)%IsSimulated))THEN
!*** End of Operating Mode Initialization done at beginning of each iteration ***!

    ! disable VRF system when outside limits of operation based on OAT
    EnableSystem = .FALSE. ! flag used to switch operating modes when OAT is outside operating limits
    IF(CoolingLoad(VRFCond))THEN
      IF((OutsideDryBulbTemp .LT. VRF(VRFCond)%MinOATCooling .OR. OutsideDryBulbTemp .GT. VRF(VRFCond)%MaxOATCooling) .AND. &
          ANY(TerminalUnitList(TUListIndex)%CoolingCoilPresent))THEN
        CoolingLoad(VRFCond) = .FALSE.
        ! test if heating load exists, account for thermostat control type
        SELECT CASE(VRF(VRFCond)%ThermostatPriority)
          CASE(LoadPriority, ZonePriority)
            IF(SumHeatingLoads(VRFCond) .GT. 0.d0)EnableSystem = .TRUE.
          CASE(ThermostatOffsetPriority)
            IF(MinDeltaT(VRFCond) .LT. 0.d0)EnableSystem = .TRUE.
          CASE(ScheduledPriority, MasterThermostatPriority)
            ! can't switch modes if scheduled (i.e., would be switching to unscheduled mode)
            ! or master TSTAT used (i.e., master zone only has a specific load - can't switch)
          CASE DEFAULT
        END SELECT
        IF(EnableSystem)THEN
          IF((OutsideDryBulbTemp .GE. VRF(VRFCond)%MinOATHeating .AND. OutsideDryBulbTemp .LE. VRF(VRFCond)%MaxOATHeating) .AND. &
              ANY(TerminalUnitList(TUListIndex)%HeatingCoilPresent))THEN
            HeatingLoad(VRFCond) = .TRUE.
          ELSE
          IF(ANY(TerminalUnitList(TUListIndex)%CoolingCoilAvailable))THEN
            IF(VRF(VRFCond)%CoolingMaxTempLimitIndex == 0)THEN
              CALL ShowWarningMessage(TRIM(cVRFTypes(VRF(VRFCond)%VRFSystemTypeNum))//' "'//TRIM(VRF(VRFCond)%Name)//'".')
              CALL ShowContinueError('...InitVRF: VRF Heat Pump Min/Max Operating Temperature in Cooling Mode Limits have '// &
                                   'been exceeded and VRF system is disabled.')
              IF(VRF(VRFCond)%CondenserType == WaterCooled) THEN
                CALL ShowContinueError('... Outdoor Unit Inlet Water Temperature           = '// &
                                   TRIM(TrimSigDigits(OutsideDryBulbTemp,3)))
              ELSE
                CALL ShowContinueError('... Outdoor Unit Inlet Air Temperature                 = '// &
                                   TRIM(TrimSigDigits(OutsideDryBulbTemp,3)))
              END IF
              CALL ShowContinueError('... Cooling Minimum Outdoor Unit Inlet Temperature = '// &
                                   TRIM(TrimSigDigits(VRF(VRFCond)%MinOATCooling,3)))
              CALL ShowContinueError('... Cooling Maximum Outdoor Unit Inlet Temperature = '//  &
                                   TRIM(TrimSigDigits(VRF(VRFCond)%MaxOATCooling,3)))
              CALL ShowContinueErrorTimeStamp('... Check VRF Heat Pump Min/Max Outdoor Temperature in Cooling Mode limits.')
            END IF
            CALL ShowRecurringWarningErrorAtEnd(TRIM(cVRFTypes(VRF(VRFCond)%VRFSystemTypeNum))//' "'//  &
                 TRIM(VRF(VRFCond)%Name)//'" -- Exceeded VRF Heat Pump min/max cooling temperature limit error continues...',  &
                 VRF(VRFCond)%CoolingMaxTempLimitIndex,OutsideDryBulbTemp,OutsideDryBulbTemp)
          END IF
          END IF
        ELSE
          IF(ANY(TerminalUnitList(TUListIndex)%CoolingCoilAvailable))THEN
          IF(VRF(VRFCond)%CoolingMaxTempLimitIndex == 0)THEN
            CALL ShowWarningMessage(TRIM(cVRFTypes(VRF(VRFCond)%VRFSystemTypeNum))//' "'//TRIM(VRF(VRFCond)%Name)//'".')
            CALL ShowContinueError('...InitVRF: VRF Heat Pump Min/Max Operating Temperature in Cooling Mode Limits have '// &
                                   'been exceeded and VRF system is disabled.')
            IF(VRF(VRFCond)%CondenserType == WaterCooled) THEN
              CALL ShowContinueError('... Outdoor Unit Inlet Water Temperature           = '// &
                                   TRIM(TrimSigDigits(OutsideDryBulbTemp,3)))
            ELSE
              CALL ShowContinueError('... Outdoor Unit Inlet Air Temperature                 = '// &
                                   TRIM(TrimSigDigits(OutsideDryBulbTemp,3)))
            END IF
            CALL ShowContinueError('... Cooling Minimum Outdoor Unit Inlet Temperature = '// &
                                   TRIM(TrimSigDigits(VRF(VRFCond)%MinOATCooling,3)))
            CALL ShowContinueError('... Cooling Maximum Outdoor Unit Inlet Temperature = '//  &
                                   TRIM(TrimSigDigits(VRF(VRFCond)%MaxOATCooling,3)))
            CALL ShowContinueErrorTimeStamp('... Check VRF Heat Pump Min/Max Outdoor Temperature in Cooling Mode limits.')
          END IF
          CALL ShowRecurringWarningErrorAtEnd(TRIM(cVRFTypes(VRF(VRFCond)%VRFSystemTypeNum))//' "'//  &
                 TRIM(VRF(VRFCond)%Name)//'" -- Exceeded VRF Heat Pump min/max cooling temperature limit error continues...',  &
                 VRF(VRFCond)%CoolingMaxTempLimitIndex,OutsideDryBulbTemp,OutsideDryBulbTemp)
        END IF
        END IF
      END IF
    ELSEIF(HeatingLoad(VRFCond))THEN
      IF((OutsideDryBulbTemp .LT. VRF(VRFCond)%MinOATHeating .OR. OutsideDryBulbTemp .GT. VRF(VRFCond)%MaxOATHeating) .AND. &
          ANY(TerminalUnitList(TUListIndex)%HeatingCoilPresent))THEN
        HeatingLoad(VRFCond) = .FALSE.
        ! test if heating load exists, account for thermostat control type
        SELECT CASE(VRF(VRFCond)%ThermostatPriority)
          CASE(LoadPriority, ZonePriority)
            IF(SumCoolingLoads(VRFCond) .LT. 0.d0)EnableSystem = .TRUE.
          CASE(ThermostatOffsetPriority)
            IF(MaxDeltaT(VRFCond) .GT. 0.d0)EnableSystem = .TRUE.
          CASE(ScheduledPriority, MasterThermostatPriority)
          CASE DEFAULT
        END SELECT
        IF(EnableSystem)THEN
          IF((OutsideDryBulbTemp .GE. VRF(VRFCond)%MinOATCooling .AND. OutsideDryBulbTemp .LE. VRF(VRFCond)%MaxOATCooling) .AND. &
              ANY(TerminalUnitList(TUListIndex)%CoolingCoilPresent))THEN
            CoolingLoad(VRFCond) = .TRUE.
          ELSE
            IF(ANY(TerminalUnitList(TUListIndex)%HeatingCoilAvailable))THEN
            IF(VRF(VRFCond)%HeatingMaxTempLimitIndex == 0)THEN
              CALL ShowWarningMessage(TRIM(cVRFTypes(VRF(VRFCond)%VRFSystemTypeNum))//' "'//TRIM(VRF(VRFCond)%Name)//'".')
              CALL ShowContinueError('...InitVRF: VRF Heat Pump Min/Max Operating Temperature in Heating Mode Limits '// &
                                   'have been exceeded and VRF system is disabled.')
              IF(VRF(VRFCond)%CondenserType == WaterCooled) THEN
                CALL ShowContinueError('... Outdoor Unit Inlet Water Temperature           = '// &
                                   TRIM(TrimSigDigits(OutsideDryBulbTemp,3)))
              ELSE
                CALL ShowContinueError('... Outdoor Unit Inlet Air Temperature             = '// &
                                   TRIM(TrimSigDigits(OutsideDryBulbTemp,3)))
              ENDIF
              CALL ShowContinueError('... Heating Minimum Outdoor Unit Inlet Temperature = '// &
                                   TRIM(TrimSigDigits(VRF(VRFCond)%MinOATHeating,3)))
              CALL ShowContinueError('... Heating Maximum Outdoor Unit Inlet Temperature = '//  &
                                   TRIM(TrimSigDigits(VRF(VRFCond)%MaxOATHeating,3)))
              CALL ShowContinueErrorTimeStamp('... Check VRF Heat Pump Min/Max Outdoor Temperature in Heating Mode limits.')
            END IF
            CALL ShowRecurringWarningErrorAtEnd(TRIM(cVRFTypes(VRF(VRFCond)%VRFSystemTypeNum))//' "'//  &
                   TRIM(VRF(VRFCond)%Name)//'" -- Exceeded VRF Heat Pump min/max heating temperature limit error continues...',  &
                   VRF(VRFCond)%HeatingMaxTempLimitIndex,OutsideDryBulbTemp,OutsideDryBulbTemp)
          END IF
          END IF
        ELSE
          IF(ANY(TerminalUnitList(TUListIndex)%HeatingCoilAvailable))THEN
          IF(VRF(VRFCond)%HeatingMaxTempLimitIndex == 0)THEN
            CALL ShowWarningMessage(TRIM(cVRFTypes(VRF(VRFCond)%VRFSystemTypeNum))//' "'//TRIM(VRF(VRFCond)%Name)//'".')
            CALL ShowContinueError('...InitVRF: VRF Heat Pump Min/Max Operating Temperature in Heating Mode Limits '// &
                                   'have been exceeded and VRF system is disabled.')
            IF(VRF(VRFCond)%CondenserType == WaterCooled) THEN
              CALL ShowContinueError('... Outdoor Unit Inlet Water Temperature           = '// &
                                   TRIM(TrimSigDigits(OutsideDryBulbTemp,3)))
            ELSE
              CALL ShowContinueError('... Outdoor Unit Inlet Air Temperature             = '// &
                                   TRIM(TrimSigDigits(OutsideDryBulbTemp,3)))
            END IF
            CALL ShowContinueError('... Heating Minimum Outdoor Unit Inlet Temperature = '// &
                                   TRIM(TrimSigDigits(VRF(VRFCond)%MinOATHeating,3)))
            CALL ShowContinueError('... Heating Maximum Outdoor Unit Inlet Temperature = '//  &
                                   TRIM(TrimSigDigits(VRF(VRFCond)%MaxOATHeating,3)))
            CALL ShowContinueErrorTimeStamp('... Check VRF Heat Pump Min/Max Outdoor Temperature in Heating Mode limits.')
          END IF
          CALL ShowRecurringWarningErrorAtEnd(TRIM(cVRFTypes(VRF(VRFCond)%VRFSystemTypeNum))//' "'//  &
                   TRIM(VRF(VRFCond)%Name)//'" -- Exceeded VRF Heat Pump min/max heating temperature limit error continues...',  &
                   VRF(VRFCond)%HeatingMaxTempLimitIndex,OutsideDryBulbTemp,OutsideDryBulbTemp)
        END IF
        END IF
      END IF
    END IF

  END IF ! IF (GetCurrentScheduleValue(VRF(VRFCond)%SchedPtr) .EQ. 0.0) THEN

! initialize terminal unit flow rate
  IF(HeatingLoad(VRFCond) .OR. &
    (VRF(VRFCond)%HeatRecoveryUsed .AND. TerminalUnitList(TUListIndex)%HRHeatRequest(IndexToTUInTUList)))THEN
    IF(VRFTU(VRFTUNum)%OAMixerUsed)THEN
      Node(VRFTU(VRFTUNum)%VRFTUOAMixerRetNodeNum)%MassFlowRate = VRFTU(VRFTUNum)%MaxHeatAirMassFlow
      Node(OutsideAirNode)%MassFlowRate = VRFTU(VRFTUNum)%HeatOutAirMassFlow
    ELSE
      Node(InNode)%MassFlowRate = VRFTU(VRFTUNum)%MaxHeatAirMassFlow
    END IF
  ELSE IF(CoolingLoad(VRFCond) .OR. &
         (VRF(VRFCond)%HeatRecoveryUsed .AND. TerminalUnitList(TUListIndex)%HRCoolRequest(IndexToTUInTUList)))THEN
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

  ! these flags are used in Subroutine CalcVRF to turn on the correct coil (heating or cooling)
  ! valid operating modes
  ! Heat Pump (heat recovery flags are set to FALSE):
  ! CoolingLoad(VRFCond) - TU can only operate in this mode if heat recovery is not used and there is a cooling load
  ! HeatingLoad(VRFCond) - TU can only operate in this mode if heat recovery is not used and there is a heating load
  ! Heat Recovery (heat pump flags are set same as for Heat Pump operation):
  ! TerminalUnitList(TUListIndex)%HRCoolRequest(IndexToTUInTUList) - TU will operate in this mode if heat recovery is used
  ! TerminalUnitList(TUListIndex)%HRHeatRequest(IndexToTUInTUList) - TU will operate in this mode if heat recovery is used

    QZnReq = ZoneSysEnergyDemand(VRFTU(VRFTUNum)%ZoneNum)%RemainingOutputRequired
    IF(ABS(QZnReq) .LT. SmallLoad) QZnReq = 0.d0
    LoadToCoolingSP = ZoneSysEnergyDemand(VRFTU(VRFTUNum)%ZoneNum)%RemainingOutputReqToCoolSP
    ! set initial terminal unit operating mode for heat recovery
    ! operating mode for non-heat recovery set above using CoolingLoad(VRFCond) or HeatingLoad(VRFCond) variables
    ! first turn off terminal unit
    TerminalUnitList(TUListIndex)%HRCoolRequest(IndexToTUInTUList) = .FALSE.
    TerminalUnitList(TUListIndex)%HRHeatRequest(IndexToTUInTUList) = .FALSE.
    ! then set according to LoadToXXXXingSP variables
    IF(LoadToCoolingSP .LT. -1.d0*SmallLoad)THEN
      TerminalUnitList(TUListIndex)%HRCoolRequest(IndexToTUInTUList) = .TRUE.
      TerminalUnitList(TUListIndex)%HRHeatRequest(IndexToTUInTUList) = .FALSE.
    END IF
    LoadToHeatingSP = ZoneSysEnergyDemand(VRFTU(VRFTUNum)%ZoneNum)%RemainingOutputReqToHeatSP
    IF(LoadToHeatingSP .GT. SmallLoad)THEN
      TerminalUnitList(TUListIndex)%HRCoolRequest(IndexToTUInTUList) = .FALSE.
      TerminalUnitList(TUListIndex)%HRHeatRequest(IndexToTUInTUList) = .TRUE.
    END IF
    IF(LoadToCoolingSP > 0.d0 .AND. LoadToHeatingSP < 0.d0)QZnReq=0.d0

    ! next check for overshoot when constant fan mode is used
    ! check operating load to see if OA will overshoot setpoint temperature when constant fan mode is used
    IF(VRFTU(VRFTUNum)%OpMode == ContFanCycCoil)THEN
      CALL SetCompFlowRate(VRFTUNum, VRFCond, .TRUE.)
      CALL CalcVRF(VRFTUNum,FirstHVACIteration,0.0d0,TempOutput,OnOffAirFlowRatio)
      ! If the Terminal Unit has a net cooling capacity (TempOutput < 0) and
      ! the zone temp is above the Tstat heating setpoint (QToHeatSetPt < 0)
      ! see if the terminal unit operation will exceed the setpoint
      !
      ! 4 tests here to cover all possibilities:
      ! IF(TempOutput < 0.0d0 .AND. LoadToHeatingSP .LT. 0.0d0)THEN
      ! ELSE IF(TempOutput .GT. 0.0d0 .AND. LoadToCoolingSP .GT. 0.0d0)THEN
      ! ELSE IF(TempOutput .GT. 0.0d0 .AND. LoadToCoolingSP .LT. 0.0d0)THEN
      ! ELSE IF(TempOutput < 0.0d0 .AND. LoadToHeatingSP .GT. 0.0d0)THEN
      ! END IF
      ! could compress these to 2 complex IF's but logic inside each would get more complex
      !
      IF(TempOutput < 0.0d0 .AND. LoadToHeatingSP .LT. 0.0d0)THEN
        ! If the net cooling capacity overshoots the heating setpoint count as heating load
        IF(TempOutput < LoadToHeatingSP)THEN
          ! Don't count as heating load unless mode is allowed. Also check for floating zone.
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
                TerminalUnitList(TUListIndex)%HRHeatRequest(IndexToTUInTUList) = .TRUE.
                TerminalUnitList(TUListIndex)%HRCoolRequest(IndexToTUInTUList) = .FALSE.
              END IF
            ELSE
              ! last mode was heating, zone temp will overshoot heating setpoint, reset QznReq to LoadtoHeatingSP
              QZnReq = LoadToHeatingSP
              TerminalUnitList(TUListIndex)%HRHeatRequest(IndexToTUInTUList) = .TRUE.
              TerminalUnitList(TUListIndex)%HRCoolRequest(IndexToTUInTUList) = .FALSE.
            END IF
          END IF
        ELSE IF(TempOutput > LoadToCoolingSP .AND. LoadToCoolingSP < 0.d0)THEN
!       If the net cooling capacity does not meet the zone cooling load enable cooling
          QZnReq = LoadToCoolingSP
          TerminalUnitList(TUListIndex)%HRHeatRequest(IndexToTUInTUList) = .FALSE.
          TerminalUnitList(TUListIndex)%HRCoolRequest(IndexToTUInTUList) = .TRUE.
        ELSE IF(TempOutput < LoadToCoolingSP .AND. LoadToCoolingSP < 0.d0)THEN
!       If the net cooling capacity meets the zone cooling load but does not overshoot heating setpoint
          QZnReq = 0.d0
          TerminalUnitList(TUListIndex)%HRHeatRequest(IndexToTUInTUList) = .FALSE.
          TerminalUnitList(TUListIndex)%HRCoolRequest(IndexToTUInTUList) = .FALSE.
        END IF
!     If the terminal unit has a net heating capacity and the zone temp is below the Tstat cooling setpoint
!     see if the terminal unit operation will exceed the setpoint
      ELSE IF(TempOutput .GT. 0.0d0 .AND. LoadToCoolingSP .GT. 0.0d0)THEN
!       If the net heating capacity overshoots the cooling setpoint count as cooling load
        IF(TempOutput > LoadToCoolingSP)THEN
!         Don't count as cooling load unless mode is allowed. Also check for floating zone.
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
                TerminalUnitList(TUListIndex)%HRCoolRequest(IndexToTUInTUList) = .TRUE.
                TerminalUnitList(TUListIndex)%HRHeatRequest(IndexToTUInTUList) = .FALSE.
              END IF
            ELSE
              QZnReq = LoadToCoolingSP
              TerminalUnitList(TUListIndex)%HRCoolRequest(IndexToTUInTUList) = .TRUE.
              TerminalUnitList(TUListIndex)%HRHeatRequest(IndexToTUInTUList) = .FALSE.
            END IF
          END IF
        ELSE IF(TempOutput .LT. LoadToHeatingSP)THEN
!         Don't count as heating load unless mode is allowed. Also check for floating zone.
          IF(TempControlType(VRFTU(VRFTUNum)%ZoneNum) .NE. SingleCoolingSetPoint .AND. &
             TempControlType(VRFTU(VRFTUNum)%ZoneNum) .NE. 0)THEN
            IF(.NOT. LastModeHeating(VRFCond))THEN
              IF(VRFTU(VRFTUNum)%OAMixerUsed)THEN
                Node(VRFTU(VRFTUNum)%VRFTUOAMixerRetNodeNum)%MassFlowRate = VRFTU(VRFTUNum)%MaxHeatAirMassFlow
                Node(VRFTU(VRFTUNum)%VRFTUOAMixerOANodeNum)%MassFlowRate = VRFTU(VRFTUNum)%HeatOutAirMassFlow
                CALL SimOAMixer(VRFTU(VRFTUNum)%OAMixerName,FirstHVACIteration,VRFTU(VRFTUNum)%OAMixerIndex)
              ELSE
                Node(VRFTU(VRFTUNum)%VRFTUInletNodeNum)%MassFlowRate = VRFTU(VRFTUNum)%MaxHeatAirMassFlow
              END IF
              CALL CalcVRF(VRFTUNum,FirstHVACIteration,0.0d0,TempOutput,OnOffAirFlowRatio)
              ! if zone temp will overshoot, pass the LoadToHeatingSP as the load to meet
              IF(TempOutput < LoadToHeatingSP)THEN
                QZnReq = LoadToHeatingSP
                TerminalUnitList(TUListIndex)%HRHeatRequest(IndexToTUInTUList) = .TRUE.
                TerminalUnitList(TUListIndex)%HRCoolRequest(IndexToTUInTUList) = .FALSE.
              END IF
            ELSE
              QZnReq = LoadToHeatingSP
              TerminalUnitList(TUListIndex)%HRHeatRequest(IndexToTUInTUList) = .TRUE.
              TerminalUnitList(TUListIndex)%HRCoolRequest(IndexToTUInTUList) = .FALSE.
            END IF
          END IF
        ELSE IF(TempOutput > LoadToHeatingSP .AND. TempOutput < LoadToCoolingSP)THEN
!         If the net capacity does not overshoot either setpoint
          QZnReq = 0.d0
          TerminalUnitList(TUListIndex)%HRHeatRequest(IndexToTUInTUList) = .FALSE.
          TerminalUnitList(TUListIndex)%HRCoolRequest(IndexToTUInTUList) = .FALSE.
        ELSE
!         If the net heating capacity meets the zone heating load but does not overshoot cooling setpoint
          QZnReq = 0.d0
          TerminalUnitList(TUListIndex)%HRHeatRequest(IndexToTUInTUList) = .FALSE.
          TerminalUnitList(TUListIndex)%HRCoolRequest(IndexToTUInTUList) = .FALSE.
        END IF
!     If the terminal unit has a net heating capacity and the zone temp is above the Tstat cooling setpoint
!     see if the terminal unit operation will exceed the setpoint
      ELSE IF(TempOutput .GT. 0.0d0 .AND. LoadToCoolingSP .LT. 0.0d0)THEN
!       If the net heating capacity overshoots the cooling setpoint count as cooling load
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
              TerminalUnitList(TUListIndex)%HRCoolRequest(IndexToTUInTUList) = .TRUE.
              TerminalUnitList(TUListIndex)%HRHeatRequest(IndexToTUInTUList) = .FALSE.
            END IF
          ! last mode was cooling, zone temp will overshoot cooling setpoint, reset QznReq to LoadtoCoolingSP
          ELSE
            QZnReq = LoadToCoolingSP
            TerminalUnitList(TUListIndex)%HRCoolRequest(IndexToTUInTUList) = .TRUE.
            TerminalUnitList(TUListIndex)%HRHeatRequest(IndexToTUInTUList) = .FALSE.
          END IF
        END IF
      ! If the Terminal Unit has a net cooling capacity (TempOutput < 0) and
      ! the zone temp is below the Tstat heating setpoint (QToHeatSetPt > 0)
      ! see if the terminal unit operation will exceed the setpoint
      ELSE IF(TempOutput < 0.0d0 .AND. LoadToHeatingSP .GT. 0.0d0)THEN
        ! Don't count as heating load unless mode is allowed. Also check for floating zone.
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
              TerminalUnitList(TUListIndex)%HRHeatRequest(IndexToTUInTUList) = .TRUE.
              TerminalUnitList(TUListIndex)%HRCoolRequest(IndexToTUInTUList) = .FALSE.
            END IF
          ELSE
            ! last mode was heating, zone temp will overshoot heating setpoint, reset QznReq to LoadtoHeatingSP
            QZnReq = LoadToHeatingSP
            TerminalUnitList(TUListIndex)%HRHeatRequest(IndexToTUInTUList) = .TRUE.
            TerminalUnitList(TUListIndex)%HRCoolRequest(IndexToTUInTUList) = .FALSE.
          END IF
        END IF
      END IF
    END IF ! IF(VRFTU(VRFTUNum)%OpMode == ContFanCycCoil)THEN

  IF(VRF(VRFCond)%HeatRecoveryUsed)THEN
   IF(OutsideDryBulbTemp .LT. VRF(VRFCond)%MinOATHeatRecovery .OR. &
      OutsideDryBulbTemp .GT. VRF(VRFCond)%MaxOATHeatRecovery)THEN
      IF(ANY(TerminalUnitList(TUListIndex)%HRCoolRequest) .OR. &
        ANY(TerminalUnitList(TUListIndex)%HRHeatRequest))THEN
          IF(VRF(VRFCond)%HRMaxTempLimitIndex == 0)THEN
            CALL ShowWarningMessage(TRIM(cVRFTypes(VRF(VRFCond)%VRFSystemTypeNum))//' "'//TRIM(VRF(VRFCond)%Name)//'".')
            CALL ShowContinueError('...InitVRF: VRF Heat Pump Min/Max Outdoor Temperature in Heat Recovery Mode Limits '// &
                                   'have been exceeded and VRF heat recovery is disabled.')
            CALL ShowContinueError('... Outdoor Dry-Bulb Temperature                       = '// &
                                   TRIM(TrimSigDigits(OutsideDryBulbTemp,3)))
            CALL ShowContinueError('... Heat Recovery Minimum Outdoor Dry-Bulb Temperature = '// &
                                   TRIM(TrimSigDigits(VRF(VRFCond)%MinOATHeatRecovery,3)))
            CALL ShowContinueError('... Heat Recovery Maximum Outdoor Dry-Bulb Temperature = '//  &
                                   TRIM(TrimSigDigits(VRF(VRFCond)%MaxOATHeatRecovery,3)))
            CALL ShowContinueErrorTimeStamp('... Check VRF Heat Pump Min/Max Outdoor Temperature in Heat Recovery Mode limits.')
            CALL ShowContinueError('...the system will operate in heat pump mode when applicable.')
          END IF
          CALL ShowRecurringWarningErrorAtEnd(TRIM(cVRFTypes(VRF(VRFCond)%VRFSystemTypeNum))//' "'//  &
                 TRIM(VRF(VRFCond)%Name)//'" -- Exceeded VRF Heat Recovery min/max outdoor temperature limit error continues...', &
                 VRF(VRFCond)%HRMaxTempLimitIndex,OutsideDryBulbTemp,OutsideDryBulbTemp)
      END IF
      ! Allow heat pump mode to operate if within limits
      IF(OutsideDryBulbTemp .LT. VRF(VRFCond)%MinOATCooling .OR. OutsideDryBulbTemp .GT. VRF(VRFCond)%MaxOATCooling)THEN
        ! Disable cooling mode only, heating model will still be allowed
        TerminalUnitList(TUListIndex)%HRCoolRequest(IndexToTUInTUList) = .FALSE.
      END IF
      IF(OutsideDryBulbTemp .LT. VRF(VRFCond)%MinOATHeating .OR. OutsideDryBulbTemp .GT. VRF(VRFCond)%MaxOATHeating)THEN
        ! Disable heating mode only, cooling model will still be allowed
        TerminalUnitList(TUListIndex)%HRHeatRequest(IndexToTUInTUList) = .FALSE.
      END IF
    END IF
  ELSE
    TerminalUnitList(TUListIndex)%HRHeatRequest = .FALSE.
    TerminalUnitList(TUListIndex)%HRCoolRequest = .FALSE.
  END IF

  ! Override operating mode when using EMS
  ! this logic seems suspect, uses a "just run it on" mentality. Nee to test using EMS.
  IF (VRF(VRFCond)%EMSOverrideHPOperatingMode) THEN
    IF(VRF(VRFCond)%EMSValueForHPOperatingMode == 0.d0)THEN  ! Off
      HeatingLoad(VRFCond) = .FALSE.
      CoolingLoad(VRFCond) = .FALSE.
      TerminalUnitList(TUListIndex)%HRHeatRequest(IndexToTUInTUList) = .FALSE.
      TerminalUnitList(TUListIndex)%HRCoolRequest(IndexToTUInTUList) = .FALSE.
    ELSE IF(VRF(VRFCond)%EMSValueForHPOperatingMode == 1.d0)THEN ! Cooling
      HeatingLoad(VRFCond) = .FALSE.
      CoolingLoad(VRFCond) = .TRUE.
      QZnReq = LoadToCoolingSP
      IF(VRF(VRFCond)%HeatRecoveryUsed)THEN
        TerminalUnitList(TUListIndex)%HRHeatRequest(IndexToTUInTUList) = .FALSE.
        TerminalUnitList(TUListIndex)%HRCoolRequest(IndexToTUInTUList) = .TRUE.
      END IF
    ELSE IF(VRF(VRFCond)%EMSValueForHPOperatingMode == 2.d0)THEN ! Heating
      HeatingLoad(VRFCond) = .TRUE.
      CoolingLoad(VRFCond) = .FALSE.
      QZnReq = LoadToHeatingSP
      IF(VRF(VRFCond)%HeatRecoveryUsed)THEN
        TerminalUnitList(TUListIndex)%HRHeatRequest(IndexToTUInTUList) = .TRUE.
        TerminalUnitList(TUListIndex)%HRCoolRequest(IndexToTUInTUList) = .FALSE.
      END IF
    ELSE
      IF(VRF(VRFCond)%HPOperatingModeErrorIndex == 0)THEN
        CALL ShowWarningMessage(TRIM(cVRFTypes(VRF(VRFCond)%VRFSystemTypeNum))//' "'//TRIM(VRF(VRFCond)%Name)//'".')
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

  ! set the TU flow rate. Check for heat recovery operation first, these will be FALSE if HR is not used.
  IF(TerminalUnitList(TUListIndex)%HRCoolRequest(IndexToTUInTUList))THEN
    CompOnMassFlow = VRFTU(VRFTUNum)%MaxCoolAirMassFlow
    CompOffMassFlow = VRFTU(VRFTUNum)%MaxNoCoolAirMassFlow
    OACompOnMassFlow = VRFTU(VRFTUNum)%CoolOutAirMassFlow
    OACompOffMassFlow = VRFTU(VRFTUNum)%NoCoolHeatOutAirVolFlow
  ELSE IF(TerminalUnitList(TUListIndex)%HRHeatRequest(IndexToTUInTUList))THEN
    CompOnMassFlow = VRFTU(VRFTUNum)%MaxHeatAirMassFlow
    CompOffMassFlow = VRFTU(VRFTUNum)%MaxNoHeatAirMassFlow
    OACompOnMassFlow = VRFTU(VRFTUNum)%HeatOutAirMassFlow
    OACompOffMassFlow = VRFTU(VRFTUNum)%NoCoolHeatOutAirVolFlow
  ELSE IF(CoolingLoad(VRFCond) .and. QZnReq /= 0.d0)THEN
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
    IF(LastModeCooling(VRFCond))THEN
      CompOnMassFlow = VRFTU(VRFTUNum)%MaxNoCoolAirMassFlow
      CompOffMassFlow = VRFTU(VRFTUNum)%MaxNoCoolAirMassFlow
      OACompOnMassFlow = VRFTU(VRFTUNum)%CoolOutAirMassFlow
    END IF
    IF(LastModeHeating(VRFCond))THEN
      CompOnMassFlow = VRFTU(VRFTUNum)%MaxNoHeatAirMassFlow
      CompOffMassFlow = VRFTU(VRFTUNum)%MaxNoHeatAirMassFlow
      OACompOnMassFlow = VRFTU(VRFTUNum)%HeatOutAirMassFlow
    END IF
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
  INTEGER :: IndexToTUInTUList    !- index to TU in specific list for this VRF system
  INTEGER :: TUListIndex    ! index to TU list for this VRF system

  IndexToTUInTUList = VRFTU(VRFTUNum)%IndexToTUInTUList
  TUListIndex = VRFTU(VRFTUNum)%TUListIndex
  IF(Present(UseCurrentMode))THEN
    CurrentMode = UseCurrentMode
  ELSE
    CurrentMode = .FALSE.
  END IF

 ! uses current operating mode to set flow rate (after mode is set)
!  IF(VRF(VRFCond)%HeatRecoveryUsed .AND. TerminalUnitList(TUListIndex)%HRCoolRequest(IndexToTUInTUList))THEN
  IF(TerminalUnitList(TUListIndex)%HRCoolRequest(IndexToTUInTUList))THEN
    CompOnMassFlow    = VRFTU(VRFTUNum)%MaxCoolAirMassFlow
    CompOffMassFlow   = VRFTU(VRFTUNum)%MaxNoCoolAirMassFlow
    OACompOnMassFlow  = VRFTU(VRFTUNum)%CoolOutAirMassFlow
    OACompOffMassFlow = VRFTU(VRFTUNum)%NoCoolHeatOutAirVolFlow
 ! uses current operating mode to set flow rate (after mode is set)
!  ELSE IF(VRF(VRFCond)%HeatRecoveryUsed .AND. TerminalUnitList(TUListIndex)%HRHeatRequest(IndexToTUInTUList))THEN
  ELSE IF(TerminalUnitList(TUListIndex)%HRHeatRequest(IndexToTUInTUList))THEN
    CompOnMassFlow    = VRFTU(VRFTUNum)%MaxHeatAirMassFlow
    CompOffMassFlow   = VRFTU(VRFTUNum)%MaxNoHeatAirMassFlow
    OACompOnMassFlow  = VRFTU(VRFTUNum)%HeatOutAirMassFlow
    OACompOffMassFlow = VRFTU(VRFTUNum)%NoCoolHeatOutAirVolFlow
  ELSE IF(CurrentMode)THEN ! uses current operating mode to set flow rate (after mode is set)
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
          !       MODIFIED       August 2013 Daeho Kang, add component sizing table entries
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
  USE General, ONLY: RoundSigDigits
  USE PlantUtilities, ONLY: RegisterPlantCompDesignFlow

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
  REAL(r64)     :: DXCoilCap      ! capacity of DX cooling coil (W)
  LOGICAL       :: IsAutosize     ! Indicator to autosize
  REAL(r64)     :: MaxCoolAirVolFlowDes        ! Autosized supply air during cooling for reporting
  REAL(r64)     :: MaxCoolAirVolFlowUser       ! Hardsized supply air during cooling for reporting
  REAL(r64)     :: MaxHeatAirVolFlowDes        ! Autosized supply air during heating for reporting
  REAL(r64)     :: MaxHeatAirVolFlowUser       ! Hardsized supply air during heating for reporting
  REAL(r64)     :: MaxNoCoolAirVolFlowDes      ! Autosized supply air flow when no cooling is needed for reporting
  REAL(r64)     :: MaxNoCoolAirVolFlowUser     ! Hardsized supply air flow when no cooling is needed for reporting
  REAL(r64)     :: MaxNoHeatAirVolFlowDes      ! Autosized supply air flow when no heating is needed for reporting
  REAL(r64)     :: MaxNoHeatAirVolFlowUser     ! Hardsized supply air flow when no heating is needed for reporting
  REAL(r64)     :: CoolOutAirVolFlowDes        ! Autosized outdoor air flow during cooling for reporting
  REAL(r64)     :: CoolOutAirVolFlowUser       ! Hardsized outdoor air flow during cooling for reporting
  REAL(r64)     :: HeatOutAirVolFlowDes        ! Autosized outdoor air flow during heating for reporting
  REAL(r64)     :: HeatOutAirVolFlowUser       ! Hardsized outdoor air flow during heating for reporting
  REAL(r64)     :: NoCoolHeatOutAirVolFlowDes  ! Autosized outdoor air when unconditioned for reporting
  REAL(r64)     :: NoCoolHeatOutAirVolFlowUser ! Hardsized outdoor air when unconditioned for reporting
  REAL(r64)     :: CoolingCapacityDes          ! Autosized cooling capacity for reporting
  REAL(r64)     :: CoolingCapacityUser         ! Hardsized cooling capacity for reporting
  REAL(r64)     :: HeatingCapacityDes          ! Autosized heating capacity for reporting
  REAL(r64)     :: HeatingCapacityUser         ! Hardsized heating capacity for reporting
  REAL(r64)     :: DefrostCapacityDes          ! Autosized defrost heater capacity for reporting
  REAL(r64)     :: DefrostCapacityUser         ! Hardsized defrost heater capacity for reporting
  REAL(r64)     :: EvapCondAirVolFlowRateDes   ! Autosized evaporative condenser flow for reporting
  REAL(r64)     :: EvapCondAirVolFlowRateUser  ! Hardsized evaporative condenser flow for reporting
  REAL(r64)     :: EvapCondPumpPowerDes        ! Autosized evaporative condenser pump power for reporting
  REAL(r64)     :: EvapCondPumpPowerUser       ! Hardsized evaporative condenser pump power for reporting

  VRFCond = VRFTU(VRFTUNum)%VRFSysNum
  IsAutosize = .FALSE.
  MaxCoolAirVolFlowDes = 0.0d0
  MaxCoolAirVolFlowUser = 0.0d0
  MaxHeatAirVolFlowDes = 0.0d0
  MaxHeatAirVolFlowUser = 0.0d0
  MaxNoCoolAirVolFlowDes = 0.0d0
  MaxNoCoolAirVolFlowUser = 0.0d0
  MaxNoHeatAirVolFlowDes = 0.0d0
  MaxNoHeatAirVolFlowUser = 0.0d0
  CoolOutAirVolFlowDes = 0.0d0
  CoolOutAirVolFlowUser = 0.0d0
  HeatOutAirVolFlowDes = 0.0d0
  HeatOutAirVolFlowUser = 0.0d0
  NoCoolHeatOutAirVolFlowDes = 0.0d0
  NoCoolHeatOutAirVolFlowUser = 0.0d0
  CoolingCapacityDes = 0.0d0
  CoolingCapacityUser = 0.0d0
  HeatingCapacityDes = 0.0d0
  HeatingCapacityUser = 0.0d0
  DefrostCapacityDes = 0.0d0
  DefrostCapacityUser = 0.0d0
  EvapCondAirVolFlowRateDes = 0.0d0
  EvapCondAirVolFlowRateUser = 0.0d0
  EvapCondPumpPowerDes = 0.0d0
  EvapCondPumpPowerUser = 0.0d0

  IF (MyOneTimeFlag) THEN
    ! initialize the environment and sizing flags
    ALLOCATE(CheckVRFCombinationRatio(NumVRFCond))
    CheckVRFCombinationRatio = .TRUE.
    MyOneTimeFlag = .FALSE.
  END IF

  IF (VRFTU(VRFTUNum)%MaxCoolAirVolFlow == AutoSize) THEN
    IsAutosize = .TRUE.
  END IF
  IF (CurZoneEqNum > 0) THEN
    IF (.NOT. IsAutosize .AND. .NOT. ZoneSizingRunDone) THEN ! Simulation continue
      IF (VRFTU(VRFTUNum)%MaxCoolAirVolFlow > 0.0d0) THEN
        CALL ReportSizingOutput(cVRFTUTypes(VRFTU(VRFTUNum)%VRFTUType_Num), VRFTU(VRFTUNum)%Name, &
                        'User-Specified Supply Air Flow Rate During Cooling Operation [m3/s]', &
                        VRFTU(VRFTUNum)%MaxCoolAirVolFlow)
      END IF
    ELSE
      CALL CheckZoneSizing(cVRFTUTypes(VRFTU(VRFTUNum)%VRFTUType_Num), VRFTU(VRFTUNum)%Name)
      MaxCoolAirVolFlowDes = MAX(FinalZoneSizing(CurZoneEqNum)%DesCoolVolFlow, &
                                            FinalZoneSizing(CurZoneEqNum)%DesHeatVolFlow)
      IF (MaxCoolAirVolFlowDes < SmallAirVolFlow) THEN
        MaxCoolAirVolFlowDes = 0.0d0
      END IF

      IF (IsAutosize) THEN
        VRFTU(VRFTUNum)%MaxCoolAirVolFlow = MaxCoolAirVolFlowDes
        CALL ReportSizingOutput(cVRFTUTypes(VRFTU(VRFTUNum)%VRFTUType_Num), VRFTU(VRFTUNum)%Name, &
                     'Design Size Supply Air Flow Rate During Cooling Operation [m3/s]', MaxCoolAirVolFlowDes)
      ELSE
        IF (VRFTU(VRFTUNum)%MaxCoolAirVolFlow > 0.0d0 .AND. MaxCoolAirVolFlowDes > 0.0d0) THEN
          MaxCoolAirVolFlowUser = VRFTU(VRFTUNum)%MaxCoolAirVolFlow
          CALL ReportSizingOutput(cVRFTUTypes(VRFTU(VRFTUNum)%VRFTUType_Num), VRFTU(VRFTUNum)%Name, &
                     'Design Size Supply Air Flow Rate During Cooling Operation [m3/s]', MaxCoolAirVolFlowDes, &
                     'User-Specified Supply Air Flow Rate During Cooling Operation [m3/s]', MaxCoolAirVolFlowUser)
          IF (DisplayExtraWarnings) THEN
            IF ((ABS(MaxCoolAirVolFlowDes - MaxCoolAirVolFlowUser)/MaxCoolAirVolFlowUser) > AutoVsHardSizingThreshold) THEN
              CALL ShowMessage('SizeVRF: Potential issue with equipment sizing for ' &
                                   //TRIM(cVRFTUTypes(VRFTU(VRFTUNum)%VRFTUType_Num))//' '//TRIM(VRFTU(VRFTUNum)%Name))
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
  IF (VRFTU(VRFTUNum)%MaxHeatAirVolFlow == AutoSize) THEN
    IsAutosize = .TRUE.
  END IF
  IF (CurZoneEqNum > 0) THEN
    IF (.NOT. IsAutosize .AND. .NOT. ZoneSizingRunDone) THEN ! Simulation continue
      IF (VRFTU(VRFTUNum)%MaxHeatAirVolFlow > 0.0d0) THEN
        CALL ReportSizingOutput(cVRFTUTypes(VRFTU(VRFTUNum)%VRFTUType_Num), VRFTU(VRFTUNum)%Name, &
                        'User-Specified Supply Air Flow Rate During Heating Operation [m3/s]', &
                        VRFTU(VRFTUNum)%MaxHeatAirVolFlow)
      END IF
    ELSE
      CALL CheckZoneSizing(cVRFTUTypes(VRFTU(VRFTUNum)%VRFTUType_Num), VRFTU(VRFTUNum)%Name)
      MaxHeatAirVolFlowDes = MAX(FinalZoneSizing(CurZoneEqNum)%DesCoolVolFlow, &
                                            FinalZoneSizing(CurZoneEqNum)%DesHeatVolFlow)
      IF (MaxHeatAirVolFlowDes < SmallAirVolFlow) THEN
        MaxHeatAirVolFlowDes = 0.0d0
      END IF

      IF (IsAutosize) THEN
        VRFTU(VRFTUNum)%MaxHeatAirVolFlow = MaxHeatAirVolFlowDes
        CALL ReportSizingOutput(cVRFTUTypes(VRFTU(VRFTUNum)%VRFTUType_Num), VRFTU(VRFTUNum)%Name, &
                     'Design Size Supply Air Flow Rate During Heating Operation [m3/s]', MaxHeatAirVolFlowDes)
      ELSE
        IF (VRFTU(VRFTUNum)%MaxHeatAirVolFlow > 0.0d0 .AND. MaxHeatAirVolFlowDes > 0.0d0) THEN
          MaxHeatAirVolFlowUser = VRFTU(VRFTUNum)%MaxHeatAirVolFlow
          CALL ReportSizingOutput(cVRFTUTypes(VRFTU(VRFTUNum)%VRFTUType_Num), VRFTU(VRFTUNum)%Name, &
                     'Design Size Supply Air Flow Rate During Heating Operation [m3/s]', MaxHeatAirVolFlowDes, &
                     'User-Specified Supply Air Flow Rate During Heating Operation [m3/s]', MaxHeatAirVolFlowUser)
          IF (DisplayExtraWarnings) THEN
            IF ((ABS(MaxHeatAirVolFlowDes - MaxHeatAirVolFlowUser)/MaxHeatAirVolFlowUser) > AutoVsHardSizingThreshold) THEN
              CALL ShowMessage('SizeVRF: Potential issue with equipment sizing for' &
                                     //TRIM(cVRFTUTypes(VRFTU(VRFTUNum)%VRFTUType_Num))//' '//TRIM(VRFTU(VRFTUNum)%Name))
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
  IF (VRFTU(VRFTUNum)%MaxNoCoolAirVolFlow == AutoSize) THEN
    IsAutosize = .TRUE.
  END IF
  IF (CurZoneEqNum > 0) THEN
    IF (.NOT. IsAutosize .AND. .NOT. ZoneSizingRunDone) THEN ! Simulation continue
      IF (VRFTU(VRFTUNum)%MaxNoCoolAirVolFlow > 0.0d0) THEN
        CALL ReportSizingOutput(cVRFTUTypes(VRFTU(VRFTUNum)%VRFTUType_Num), VRFTU(VRFTUNum)%Name, &
                        'User-Specified Supply Air Flow Rate When No Cooling is Needed [m3/s]', &
                        VRFTU(VRFTUNum)%MaxNoCoolAirVolFlow)
      END IF
    ELSE
      CALL CheckZoneSizing(cVRFTUTypes(VRFTU(VRFTUNum)%VRFTUType_Num), VRFTU(VRFTUNum)%Name)
      MaxNoCoolAirVolFlowDes = VRFTU(VRFTUNum)%MaxCoolAirVolFlow
      IF (MaxNoCoolAirVolFlowDes < SmallAirVolFlow) THEN
        MaxNoCoolAirVolFlowDes = 0.0d0
      END IF

      IF (IsAutosize) THEN
        VRFTU(VRFTUNum)%MaxNoCoolAirVolFlow = MaxNoCoolAirVolFlowDes
        CALL ReportSizingOutput(cVRFTUTypes(VRFTU(VRFTUNum)%VRFTUType_Num), VRFTU(VRFTUNum)%Name, &
                     'Design Size Supply Air Flow Rate  When No Cooling is Needed [m3/s]', MaxNoCoolAirVolFlowDes)
      ELSE
        IF (VRFTU(VRFTUNum)%MaxNoCoolAirVolFlow > 0.0d0 .AND. MaxNoCoolAirVolFlowDes > 0.0d0) THEN
          MaxNoCoolAirVolFlowUser = VRFTU(VRFTUNum)%MaxNoCoolAirVolFlow
          CALL ReportSizingOutput(cVRFTUTypes(VRFTU(VRFTUNum)%VRFTUType_Num), VRFTU(VRFTUNum)%Name, &
                     'Design Size Supply Air Flow Rate  When No Cooling is Needed [m3/s]', MaxNoCoolAirVolFlowDes, &
                     'User-Specified Supply Air Flow Rate  When No Cooling is Needed [m3/s]', MaxNoCoolAirVolFlowUser)
          IF (DisplayExtraWarnings) THEN
            IF ((ABS(MaxNoCoolAirVolFlowDes - MaxNoCoolAirVolFlowUser)/MaxNoCoolAirVolFlowUser) > AutoVsHardSizingThreshold) THEN
              CALL ShowMessage('SizeVRF: Potential issue with equipment sizing for '&
                                   //TRIM(cVRFTUTypes(VRFTU(VRFTUNum)%VRFTUType_Num))//' '//TRIM(VRFTU(VRFTUNum)%Name))
              CALL ShowContinueError('User-Specified Supply Air Flow Rate  When No Cooling is Needed of '// &
                                      TRIM(RoundSigDigits(MaxNoCoolAirVolFlowUser,5))// ' [m3/s]')
              CALL ShowContinueError('differs from Design Size Supply Air Flow Rate  When No Cooling is Needed of ' // &
                                      TRIM(RoundSigDigits(MaxNoCoolAirVolFlowDes,5))// ' [m3/s]')
              CALL ShowContinueError('This may, or may not, indicate mismatched component sizes.')
              CALL ShowContinueError('Verify that the value entered is intended and is consistent with other components.')
            END IF
          ENDIF
        END IF
      END IF
    END IF
  END IF

  IsAutosize = .FALSE.
  IF (VRFTU(VRFTUNum)%MaxNoHeatAirVolFlow == AutoSize) THEN
    IsAutosize = .TRUE.
  END IF
  IF (CurZoneEqNum > 0) THEN
    IF (.NOT. IsAutosize .AND. .NOT. ZoneSizingRunDone) THEN ! Simulation continue
      IF (VRFTU(VRFTUNum)%MaxNoHeatAirVolFlow > 0.0d0) THEN
        CALL ReportSizingOutput(cVRFTUTypes(VRFTU(VRFTUNum)%VRFTUType_Num), VRFTU(VRFTUNum)%Name, &
                         'User-Specified Supply Air Flow Rate When No Heating is Needed [m3/s]', &
                        VRFTU(VRFTUNum)%MaxNoHeatAirVolFlow)
      END IF
    ELSE
      CALL CheckZoneSizing(cVRFTUTypes(VRFTU(VRFTUNum)%VRFTUType_Num), VRFTU(VRFTUNum)%Name)
      MaxNoHeatAirVolFlowDes = VRFTU(VRFTUNum)%MaxHeatAirVolFlow
      IF (MaxNoHeatAirVolFlowDes < SmallAirVolFlow) THEN
        MaxNoHeatAirVolFlowDes = 0.0d0
      END IF

      IF (IsAutosize) THEN
        VRFTU(VRFTUNum)%MaxNoHeatAirVolFlow = MaxNoHeatAirVolFlowDes
        CALL ReportSizingOutput(cVRFTUTypes(VRFTU(VRFTUNum)%VRFTUType_Num), VRFTU(VRFTUNum)%Name, &
                     'Design Size Supply Air Flow Rate  When No Heating is Needed [m3/s]', MaxNoHeatAirVolFlowDes)
      ELSE
        IF (VRFTU(VRFTUNum)%MaxNoHeatAirVolFlow > 0.0d0 .AND. MaxNoHeatAirVolFlowDes > 0.0d0) THEN
          MaxNoHeatAirVolFlowUser = VRFTU(VRFTUNum)%MaxNoHeatAirVolFlow
          CALL ReportSizingOutput(cVRFTUTypes(VRFTU(VRFTUNum)%VRFTUType_Num), VRFTU(VRFTUNum)%Name, &
                     'Design Size Supply Air Flow Rate  When No Heating is Needed [m3/s]', MaxNoHeatAirVolFlowDes, &
                     'User-Specified Supply Air Flow Rate  When No Heating is Needed [m3/s]', MaxNoHeatAirVolFlowUser)
          IF (DisplayExtraWarnings) THEN
            IF ((ABS(MaxNoHeatAirVolFlowDes - MaxNoHeatAirVolFlowUser)/MaxNoHeatAirVolFlowUser) > AutoVsHardSizingThreshold) THEN
              CALL ShowMessage('SizeVRF: Potential issue with equipment sizing for ' &
                                    //TRIM(cVRFTUTypes(VRFTU(VRFTUNum)%VRFTUType_Num))//' '//TRIM(VRFTU(VRFTUNum)%Name))
              CALL ShowContinueError('User-Specified Supply Air Flow Rate  When No Heating is Needed of '// &
                                      TRIM(RoundSigDigits(MaxNoHeatAirVolFlowUser,5))// ' [m3/s]')
              CALL ShowContinueError('differs from Design Size Supply Air Flow Rate  When No Heating is Needed of ' // &
                                      TRIM(RoundSigDigits(MaxNoHeatAirVolFlowDes,5))// ' [m3/s]')
              CALL ShowContinueError('This may, or may not, indicate mismatched component sizes.')
              CALL ShowContinueError('Verify that the value entered is intended and is consistent with other components.')
            END IF
          ENDIF
        END IF
      END IF
    END IF
  END IF

  IsAutosize = .FALSE.
  IF (VRFTU(VRFTUNum)%CoolOutAirVolFlow == AutoSize) THEN
    IsAutosize = .TRUE.
  END IF
  IF (CurZoneEqNum > 0) THEN
    IF (.NOT. IsAutosize .AND. .NOT. ZoneSizingRunDone) THEN ! Simulation continue
      IF (VRFTU(VRFTUNum)%CoolOutAirVolFlow > 0.0d0) THEN
        CALL ReportSizingOutput(cVRFTUTypes(VRFTU(VRFTUNum)%VRFTUType_Num), VRFTU(VRFTUNum)%Name, &
                        'User-Specified Outdoor Air Flow Rate During Cooling Operation [m3/s]', &
                        VRFTU(VRFTUNum)%CoolOutAirVolFlow)
      END IF
    ELSE
      CALL CheckZoneSizing(cVRFTUTypes(VRFTU(VRFTUNum)%VRFTUType_Num), VRFTU(VRFTUNum)%Name)
      CoolOutAirVolFlowDes = MIN(FinalZoneSizing(CurZoneEqNum)%MinOA,VRFTU(VRFTUNum)%MaxCoolAirVolFlow)
      IF (CoolOutAirVolFlowDes < SmallAirVolFlow) THEN
        CoolOutAirVolFlowDes = 0.0d0
      END IF

      IF (IsAutosize) THEN
        VRFTU(VRFTUNum)%CoolOutAirVolFlow = CoolOutAirVolFlowDes
        CALL ReportSizingOutput(cVRFTUTypes(VRFTU(VRFTUNum)%VRFTUType_Num), VRFTU(VRFTUNum)%Name, &
                     'Design Size Outdoor Air Flow Rate During Cooling Operation [m3/s]', CoolOutAirVolFlowDes)
      ELSE
        IF (VRFTU(VRFTUNum)%CoolOutAirVolFlow > 0.0d0 .AND. CoolOutAirVolFlowDes > 0.0d0) THEN
          CoolOutAirVolFlowUser = VRFTU(VRFTUNum)%CoolOutAirVolFlow
          CALL ReportSizingOutput(cVRFTUTypes(VRFTU(VRFTUNum)%VRFTUType_Num), VRFTU(VRFTUNum)%Name, &
                     'Design Size Outdoor Air Flow Rate During Cooling Operation [m3/s]', CoolOutAirVolFlowDes, &
                     'User-Specified Outdoor Air Flow Rate During Cooling Operation [m3/s]', CoolOutAirVolFlowUser)
          IF (DisplayExtraWarnings) THEN
            IF ((ABS(CoolOutAirVolFlowDes - CoolOutAirVolFlowUser)/CoolOutAirVolFlowUser) > AutoVsHardSizingThreshold) THEN
              CALL ShowMessage('SizeVRF: Potential issue with equipment sizing for ' &
                                       //TRIM(cVRFTUTypes(VRFTU(VRFTUNum)%VRFTUType_Num))//' '//TRIM(VRFTU(VRFTUNum)%Name))
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
  IF (VRFTU(VRFTUNum)%HeatOutAirVolFlow == AutoSize) THEN
    IsAutosize = .TRUE.
  END IF
  IF (CurZoneEqNum > 0) THEN
    IF (.NOT. IsAutosize .AND. .NOT. ZoneSizingRunDone) THEN ! Simulation continue
      IF (VRFTU(VRFTUNum)%CoolOutAirVolFlow > 0.0d0) THEN
        CALL ReportSizingOutput(cVRFTUTypes(VRFTU(VRFTUNum)%VRFTUType_Num), VRFTU(VRFTUNum)%Name, &
                     'Outdoor Air Flow Rate During Heating Operation [m3/s]',VRFTU(VRFTUNum)%CoolOutAirVolFlow)
      END IF
    ELSE
      CALL CheckZoneSizing(cVRFTUTypes(VRFTU(VRFTUNum)%VRFTUType_Num), VRFTU(VRFTUNum)%Name)
      HeatOutAirVolFlowDes = MIN(FinalZoneSizing(CurZoneEqNum)%MinOA,VRFTU(VRFTUNum)%MaxHeatAirVolFlow)
      IF (HeatOutAirVolFlowDes < SmallAirVolFlow) THEN
        HeatOutAirVolFlowDes = 0.0d0
      END IF

      IF (IsAutosize) THEN
        VRFTU(VRFTUNum)%HeatOutAirVolFlow = HeatOutAirVolFlowDes
        CALL ReportSizingOutput(cVRFTUTypes(VRFTU(VRFTUNum)%VRFTUType_Num), VRFTU(VRFTUNum)%Name, &
                     'Design Size Outdoor Air Flow Rate During Heating Operation [m3/s]', HeatOutAirVolFlowDes)
      ELSE
        IF (VRFTU(VRFTUNum)%HeatOutAirVolFlow > 0.0d0 .AND. HeatOutAirVolFlowDes > 0.0d0) THEN
          HeatOutAirVolFlowUser = VRFTU(VRFTUNum)%HeatOutAirVolFlow
          CALL ReportSizingOutput(cVRFTUTypes(VRFTU(VRFTUNum)%VRFTUType_Num), VRFTU(VRFTUNum)%Name, &
                     'Design Size Outdoor Air Flow Rate During Heating Operation [m3/s]', HeatOutAirVolFlowDes, &
                     'User-Specified Outdoor Air Flow Rate During Heating Operation [m3/s]', HeatOutAirVolFlowUser)
          IF (DisplayExtraWarnings) THEN
            IF ((ABS(HeatOutAirVolFlowDes - HeatOutAirVolFlowUser)/HeatOutAirVolFlowUser) > AutoVsHardSizingThreshold) THEN
              CALL ShowMessage('SizeVRF: Potential issue with equipment sizing for ' &
                                    //TRIM(cVRFTUTypes(VRFTU(VRFTUNum)%VRFTUType_Num))//' '//TRIM(VRFTU(VRFTUNum)%Name))
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
  IF (VRFTU(VRFTUNum)%NoCoolHeatOutAirVolFlow == AutoSize) THEN
    IsAutosize = .TRUE.
  END IF
  IF (CurZoneEqNum > 0) THEN
    IF (.NOT. IsAutosize .AND. .NOT. ZoneSizingRunDone) THEN ! Simulation continue
      IF (VRFTU(VRFTUNum)%NoCoolHeatOutAirVolFlow > 0.0d0) THEN
        CALL ReportSizingOutput(cVRFTUTypes(VRFTU(VRFTUNum)%VRFTUType_Num), VRFTU(VRFTUNum)%Name, &
                    'User-Specified Outdoor Air Flow Rate When No Cooling or Heating is Needed [m3/s]', &
                    VRFTU(VRFTUNum)%NoCoolHeatOutAirVolFlow)
      END IF
    ELSE
      CALL CheckZoneSizing(cVRFTUTypes(VRFTU(VRFTUNum)%VRFTUType_Num), VRFTU(VRFTUNum)%Name)
      NoCoolHeatOutAirVolFlowDes = MIN(FinalZoneSizing(CurZoneEqNum)%MinOA, &
                                                    VRFTU(VRFTUNum)%HeatOutAirVolFlow, &
                                                    VRFTU(VRFTUNum)%CoolOutAirVolFlow)
      IF (NoCoolHeatOutAirVolFlowDes < SmallAirVolFlow) THEN
        NoCoolHeatOutAirVolFlowDes = 0.0d0
      END IF

      IF (IsAutosize) THEN
        VRFTU(VRFTUNum)%NoCoolHeatOutAirVolFlow = NoCoolHeatOutAirVolFlowDes
        CALL ReportSizingOutput(cVRFTUTypes(VRFTU(VRFTUNum)%VRFTUType_Num), VRFTU(VRFTUNum)%Name, &
                    'Design Size Outdoor Air Flow Rate When No Cooling or Heating is Needed [m3/s]', &
                    NoCoolHeatOutAirVolFlowDes)
      ELSE
        IF (VRFTU(VRFTUNum)%NoCoolHeatOutAirVolFlow > 0.0d0 .AND. NoCoolHeatOutAirVolFlowDes > 0.0d0) THEN
          NoCoolHeatOutAirVolFlowUser = VRFTU(VRFTUNum)%NoCoolHeatOutAirVolFlow
          CALL ReportSizingOutput(cVRFTUTypes(VRFTU(VRFTUNum)%VRFTUType_Num), VRFTU(VRFTUNum)%Name, &
                    'Design Size Outdoor Air Flow Rate When No Cooling or Heating is Needed [m3/s]', &
                    NoCoolHeatOutAirVolFlowDes, &
                    'User-Specified Outdoor Air Flow Rate When No Cooling or Heating is Needed [m3/s]', &
                    NoCoolHeatOutAirVolFlowUser)
          IF (DisplayExtraWarnings) THEN
            IF ((ABS(NoCoolHeatOutAirVolFlowDes - NoCoolHeatOutAirVolFlowUser)/NoCoolHeatOutAirVolFlowUser) &
                          > AutoVsHardSizingThreshold) THEN
              CALL ShowMessage('SizeVRF: Potential issue with equipment sizing for ' &
                                     //TRIM(cVRFTUTypes(VRFTU(VRFTUNum)%VRFTUType_Num))//' '//TRIM(VRFTU(VRFTUNum)%Name))
              CALL ShowContinueError('User-Specified Outdoor Air Flow Rate When No Cooling or Heating is Needed of '// &
                                      TRIM(RoundSigDigits(NoCoolHeatOutAirVolFlowUser,5))// ' [m3/s]')
              CALL ShowContinueError('differs from Design Size Outdoor Air Flow Rate When No Cooling or Heating is Needed of ' &
                                     //  TRIM(RoundSigDigits(NoCoolHeatOutAirVolFlowDes,5))// ' [m3/s]')
              CALL ShowContinueError('This may, or may not, indicate mismatched component sizes.')
              CALL ShowContinueError('Verify that the value entered is intended and is consistent with other components.')
            END IF
          ENDIF
        END IF
      END IF
    END IF
  END IF

  IF(CheckVRFCombinationRatio(VRFCond))THEN
    OnOffAirFlowRat = 1.d0
    ! set up the outside air data for sizing the DX coils
    ZoneEqDXCoil = .TRUE.
    IF (CurZoneEqNum > 0) THEN
      IF (VRFTU(VRFTUNum)%CoolOutAirVolFlow > 0.0d0.OR. VRFTU(VRFTUNum)%HeatOutAirVolFlow > 0.0d0) THEN
        ZoneEqSizing(CurZoneEqNum)%OAVolFlow = MAX(VRFTU(VRFTUNum)%CoolOutAirVolFlow,VRFTU(VRFTUNum)%HeatOutAirVolFlow)
      ELSE
        ZoneEqSizing(CurZoneEqNum)%OAVolFlow = 0.0d0
      END IF
    ELSE
     ZoneEqSizing(CurZoneEqNum)%OAVolFlow = 0.0d0
    END IF
    ! simulate the TU to size the coils
    CALL CalcVRF(VRFTUNum,.TRUE.,0.0d0,TUCoolingCapacity,OnOffAirFlowRat)
!    ZoneEqDXCoil = .FALSE.
    TUCoolingCapacity = 0.0d0
    TUHeatingCapacity = 0.0d0
    FoundAll      = .TRUE.
    TUListNum = VRFTU(VRFTUNum)%TUListIndex
    DO NumTU = 1, TerminalUnitList(TUListNum)%NumTUInList
      TUIndex = TerminalUnitList(TUListNum)%ZoneTUPtr(NumTU)
      IF(VRFTU(TUIndex)%CoolCoilIndex .GT. 0)THEN
        DXCoilCap = GetDXCoilCap(VRFTU(TUIndex)%CoolCoilIndex,VRFTU(TUIndex)%DXCoolCoilType_Num,ErrFlag)
        TUCoolingCapacity = TUCoolingCapacity + DXCoilCap
        IF(DXCoilCap == AutoSize)THEN
          FoundAll = .FALSE.
          EXIT
        END IF
      END IF
      IF(VRFTU(TUIndex)%HeatCoilIndex .GT. 0)THEN
        DXCoilCap = GetDXCoilCap(VRFTU(TUIndex)%HeatCoilIndex,VRFTU(TUIndex)%DXHeatCoilType_Num,ErrFlag)
        TUHeatingCapacity = TUHeatingCapacity + DXCoilCap
        IF(DXCoilCap == AutoSize)THEN
          FoundAll = .FALSE.
          EXIT
        END IF
      END IF
    END DO

    IF(FoundAll)THEN
      IsAutosize = .FALSE.
      IF(VRF(VRFCond)%CoolingCapacity == Autosize)THEN
        IsAutosize = .TRUE.
      END IF
        CoolingCapacityDes = TUCoolingCapacity
      IF (IsAutosize) THEN
        VRF(VRFCond)%CoolingCapacity = CoolingCapacityDes
        CALL ReportSizingOutput(cVRFTypes(VRF(VRFCond)%VRFSystemTypeNum), VRF(VRFCond)%Name, &
                                    'Design Size Rated Total Cooling Capacity (gross) [W]', CoolingCapacityDes)
      ELSE
        IF (VRF(VRFCond)%CoolingCapacity > 0.0d0 .AND. CoolingCapacityDes > 0.0d0) THEN
          CoolingCapacityUser = VRF(VRFCond)%CoolingCapacity
          CALL ReportSizingOutput(cVRFTypes(VRF(VRFCond)%VRFSystemTypeNum), VRF(VRFCond)%Name, &
                                    'Design Size Rated Total Cooling Capacity (gross) [W]', CoolingCapacityDes, &
                                    'User-Specified Rated Total Cooling Capacity (gross) [W]', CoolingCapacityUser)
          IF (DisplayExtraWarnings) THEN
            IF ((ABS(CoolingCapacityDes - CoolingCapacityUser)/CoolingCapacityUser)> AutoVsHardSizingThreshold) THEN
              CALL ShowMessage('SizeVRF: Potential issue with equipment sizing for ' &
                                          //TRIM(cVRFTypes(VRF(VRFCond)%VRFSystemTypeNum))//' '//TRIM(VRFTU(VRFCond)%Name))
              CALL ShowContinueError('User-Specified Rated Total Cooling Capacity (gross) of '// &
                                      TRIM(RoundSigDigits(CoolingCapacityUser,2))// ' [W]')
              CALL ShowContinueError('differs from Design Size Rated Total Cooling Capacity (gross) of ' // &
                                      TRIM(RoundSigDigits(CoolingCapacityDes,2))// ' [W]')
              CALL ShowContinueError('This may, or may not, indicate mismatched component sizes.')
              CALL ShowContinueError('Verify that the value entered is intended and is consistent with other components.')
            END IF
          ENDIF
        END IF
      END IF

      IF(VRF(VRFCond)%CoolingCapacity .GT. 0.0d0)THEN
        VRF(VRFCond)%CoolingCombinationRatio = TUCoolingCapacity / VRF(VRFCond)%CoolingCapacity
      END IF

      IsAutosize = .FALSE.
      IF(VRF(VRFCond)%HeatingCapacity == Autosize)THEN
        IsAutosize = .TRUE.
      END IF
      IF(VRF(VRFCond)%LockHeatingCapacity)THEN
        HeatingCapacityDes = VRF(VRFCond)%CoolingCapacity * VRF(VRFCond)%HeatingCapacitySizeRatio
      ELSE
        HeatingCapacityDes = TUHeatingCapacity
      END IF
      IF (IsAutosize) THEN
        VRF(VRFCond)%HeatingCapacity = HeatingCapacityDes
        CALL ReportSizingOutput(cVRFTypes(VRF(VRFCond)%VRFSystemTypeNum), VRF(VRFCond)%Name, &
                                    'Design Size Rated Total Heating Capacity [W]', HeatingCapacityDes)
      ELSE
        IF (VRF(VRFCond)%HeatingCapacity > 0.0d0 .AND. HeatingCapacityDes > 0.0d0) THEN
          HeatingCapacityUser = VRF(VRFCond)%HeatingCapacity
          CALL ReportSizingOutput(cVRFTypes(VRF(VRFCond)%VRFSystemTypeNum), VRF(VRFCond)%Name, &
                                    'Design Size Rated Total Heating Capacity [W]', HeatingCapacityDes, &
                                    'User-Specified Rated Total Heating Capacity [W]', HeatingCapacityUser)
          IF (DisplayExtraWarnings) THEN
            IF ((ABS(HeatingCapacityDes - HeatingCapacityUser)/HeatingCapacityUser) > AutoVsHardSizingThreshold) THEN
              CALL ShowMessage('SizeVRF: Potential issue with equipment sizing for ' &
                                       //TRIM(cVRFTypes(VRF(VRFCond)%VRFSystemTypeNum))//' '//TRIM(VRFTU(VRFCond)%Name))
              CALL ShowContinueError('User-Specified Rated Total Heating Capacity of '// &
                                      TRIM(RoundSigDigits(HeatingCapacityUser,2))// ' [W]')
              CALL ShowContinueError('differs from Design Size Rated Total Heating Capacity of ' // &
                                      TRIM(RoundSigDigits(HeatingCapacityDes,2))// ' [W]')
              CALL ShowContinueError('This may, or may not, indicate mismatched component sizes.')
              CALL ShowContinueError('Verify that the value entered is intended and is consistent with other components.')
            END IF
          ENDIF
        END IF
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
        ! autosize resistive defrost heater capacity
      IsAutosize = .FALSE.
      IF (VRF(VRFCond)%DefrostCapacity == AutoSize) THEN
        IsAutosize = .TRUE.
      END IF
      IF (VRF(VRFCond)%DefrostStrategy == Resistive) THEN
          DefrostCapacityDes = VRF(VRFCond)%CoolingCapacity
      ELSE
          DefrostCapacityDes = 0.d0
      END IF
      IF (IsAutosize) THEN
        VRF(VRFCond)%DefrostCapacity = DefrostCapacityDes
        CALL ReportSizingOutput(cVRFTypes(VRF(VRFCond)%VRFSystemTypeNum), VRF(VRFCond)%Name, &
                                'Design Size Resistive Defrost Heater Capacity', DefrostCapacityDes)
      ELSE
        IF (VRF(VRFCond)%DefrostCapacity > 0.0d0 .AND. DefrostCapacityDes > 0.0d0) THEN
          DefrostCapacityUser = VRF(VRFCond)%DefrostCapacity
          CALL ReportSizingOutput(cVRFTypes(VRF(VRFCond)%VRFSystemTypeNum), VRF(VRFCond)%Name, &
                                'Design Size Resistive Defrost Heater Capacity', DefrostCapacityDes, &
                                'User-Specified Resistive Defrost Heater Capacity', DefrostCapacityUser)
          IF (DisplayExtraWarnings) THEN
            IF ((ABS(DefrostCapacityDes - DefrostCapacityUser)/DefrostCapacityUser) > AutoVsHardSizingThreshold) THEN
              CALL ShowMessage('SizeVRF: Potential issue with equipment sizing for ' &
                               //TRIM(cVRFTypes(VRF(VRFCond)%VRFSystemTypeNum))//' '//TRIM(VRFTU(VRFCond)%Name))
              CALL ShowContinueError('User-Specified Resistive Defrost Heater Capacity of '// &
                                      TRIM(RoundSigDigits(DefrostCapacityUser,2))// ' [W]')
              CALL ShowContinueError('differs from Design Size Resistive Defrost Heater Capacity of ' // &
                                      TRIM(RoundSigDigits(DefrostCapacityDes,2))// ' [W]')
              CALL ShowContinueError('This may, or may not, indicate mismatched component sizes.')
              CALL ShowContinueError('Verify that the value entered is intended and is consistent with other components.')
            END IF
          ENDIF
        END IF
      END IF

      IsAutosize = .FALSE.
      IF (VRF(VRFCond)%EvapCondAirVolFlowRate == AutoSize) THEN
        IsAutosize = .TRUE.
      END IF
        ! Auto size condenser air flow to Total Capacity * 0.000114 m3/s/w (850 cfm/ton)
        EvapCondAirVolFlowRateDes = VRF(VRFCond)%CoolingCapacity*0.000114d0
      IF (IsAutosize) THEN
        VRF(VRFCond)%EvapCondAirVolFlowRate = EvapCondAirVolFlowRateDes
        CALL ReportSizingOutput(cVRFTypes(VRF(VRFCond)%VRFSystemTypeNum), VRF(VRFCond)%Name, &
                    'Design Size Evaporative Condenser Air Flow Rate [m3/s]', EvapCondAirVolFlowRateDes)
      ELSE
        IF (VRF(VRFCond)%EvapCondAirVolFlowRate > 0.0d0 .AND. EvapCondAirVolFlowRateDes > 0.0d0) THEN
          EvapCondAirVolFlowRateUser = VRF(VRFCond)%EvapCondAirVolFlowRate
          CALL ReportSizingOutput(cVRFTypes(VRF(VRFCond)%VRFSystemTypeNum), VRF(VRFCond)%Name, &
                  'Design Size Evaporative Condenser Air Flow Rate [m3/s]', EvapCondAirVolFlowRateDes, &
                  'User-Specified Evaporative Condenser Air Flow Rate [m3/s]', EvapCondAirVolFlowRateUser)
          IF (DisplayExtraWarnings) THEN
            IF ((ABS(EvapCondAirVolFlowRateDes - EvapCondAirVolFlowRateUser)/EvapCondAirVolFlowRateUser) &
                            > AutoVsHardSizingThreshold) THEN
              CALL ShowMessage('SizeVRF: Potential issue with equipment sizing for ' &
                                    //TRIM(cVRFTypes(VRF(VRFCond)%VRFSystemTypeNum))//' '//TRIM(VRFTU(VRFCond)%Name))
              CALL ShowContinueError('User-Specified Evaporative Condenser Air Flow Rate of '// &
                                    TRIM(RoundSigDigits(EvapCondAirVolFlowRateUser,5))// ' [m3/s]')
              CALL ShowContinueError('differs from Design Size Evaporative Condenser Air Flow Rate of ' // &
                                    TRIM(RoundSigDigits(EvapCondAirVolFlowRateDes,5))// ' [m3/s]')
              CALL ShowContinueError('This may, or may not, indicate mismatched component sizes.')
              CALL ShowContinueError('Verify that the value entered is intended and is consistent with other components.')
            END IF
          ENDIF
        END IF
      END IF

      IsAutosize = .FALSE.
      IF (VRF(VRFCond)%EvapCondPumpPower == AutoSize) THEN
        IsAutosize = .TRUE.
      END IF
        ! Auto size evap condenser pump power to Total Capacity * 0.004266 w/w (15 w/ton)
      EvapCondPumpPowerDes = VRF(VRFCond)%CoolingCapacity*0.004266d0
      IF (IsAutosize) THEN
        VRF(VRFCond)%EvapCondPumpPower = EvapCondPumpPowerDes
        CALL ReportSizingOutput(cVRFTypes(VRF(VRFCond)%VRFSystemTypeNum), VRF(VRFCond)%Name, &
                           'Design Size Evaporative Condenser Pump Rated Power Consumption [W]', EvapCondPumpPowerDes)

      ELSE
        IF (VRF(VRFCond)%EvapCondPumpPower > 0.0d0 .AND. EvapCondPumpPowerDes > 0.0d0) THEN
          EvapCondPumpPowerUser = VRF(VRFCond)%EvapCondPumpPower
          CALL ReportSizingOutput(cVRFTypes(VRF(VRFCond)%VRFSystemTypeNum), VRF(VRFCond)%Name, &
                           'Design Size Evaporative Condenser Pump Rated Power Consumption [W]', EvapCondPumpPowerDes, &
                           'User-Specified Evaporative Condenser Pump Rated Power Consumption [W]', EvapCondPumpPowerUser)
          IF (DisplayExtraWarnings) THEN
            IF ((ABS(EvapCondPumpPowerDes - EvapCondPumpPowerUser)/EvapCondPumpPowerUser) > AutoVsHardSizingThreshold) THEN
              CALL ShowMessage('SizeVRF: Potential issue with equipment sizing for '  &
                                 //TRIM(cVRFTypes(VRF(VRFCond)%VRFSystemTypeNum))//' '//TRIM(VRFTU(VRFCond)%Name))
              CALL ShowContinueError('User-Specified Evaporative Condenser Pump Rated Power Consumption of '// &
                                      TRIM(RoundSigDigits(EvapCondPumpPowerUser,2))// ' [W]')
              CALL ShowContinueError('differs from Design Size Evaporative Condenser Pump Rated Power Consumption of ' // &
                                     TRIM(RoundSigDigits(EvapCondPumpPowerDes,2))// ' [W]')
              CALL ShowContinueError('This may, or may not, indicate mismatched component sizes.')
              CALL ShowContinueError('Verify that the value entered is intended and is consistent with other components.')
            END IF
          ENDIF
        END IF
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

  990 FORMAT('! <VRF System Information>, VRF System Type, VRF System Name, ', &
                'VRF System Cooling Combination Ratio, VRF System Heating Combination Ratio, ', &
        'VRF System Cooling Piping Correction Factor, VRF System Heating Piping Correction Factor')
  991 FORMAT(' VRF System Information',6(', ',A))

  RETURN

END SUBROUTINE SizeVRF

SUBROUTINE SizeVRFCondenser(VRFCond)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Raustad, FSEC
          !       DATE WRITTEN   August 2012
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is for sizing VRF Condenser.

          ! METHODOLOGY EMPLOYED:
          ! Obtains flow rates from the plant sizing arrays.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataSizing
  USE FluidProperties, ONLY: GetDensityGlycol, GetSpecificHeatGlycol
  USE ReportSizingManager, ONLY: ReportSizingOutput
  USE General, ONLY: RoundSigDigits
  USE PlantUtilities, ONLY: RegisterPlantCompDesignFlow

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  Integer, Intent(IN) :: VRFCond

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER   :: PltSizCondNum ! Plant Sizing index for condenser loop
  REAL(r64) :: rho ! local fluid density [kg/m3]
  REAL(r64) :: Cp  ! local fluid specific heat [J/kg-k]
  REAL(r64) :: tmpCondVolFlowRate ! local condenser design volume flow rate [m3/s]
  LOGICAL   :: ErrorsFound ! indicates problem with sizing

  ! save the design water flow rate for use by the water loop sizing algorithms
  IF(VRF(VRFCond)%CondenserType == WaterCooled)THEN

    ErrorsFound = .FALSE.
    PltSizCondNum = 0

    IF(VRF(VRFCond)%WaterCondVolFlowRate == Autosize)THEN
      IF(VRF(VRFCond)%SourceLoopNum .GT. 0) PltSizCondNum = PlantLoop(VRF(VRFCond)%SourceLoopNum)%PlantSizNum
      IF (PltSizCondNum > 0) THEN
        rho = GetDensityGlycol(PlantLoop(VRF(VRFCond)%SourceLoopNum)%FluidName,  &
                                  PlantSizData(PltSizCondNum)%ExitTemp, &
                                  PlantLoop(VRF(VRFCond)%SourceLoopNum)%FluidIndex,&
                                  'SizeVRFCondenser')

        Cp = GetSpecificHeatGlycol(PlantLoop(VRF(VRFCond)%SourceLoopNum)%FluidName,  &
                                 PlantSizData(PltSizCondNum)%ExitTemp,                      &
                                 PlantLoop(VRF(VRFCond)%SourceLoopNum)%FluidIndex, &
                                 'SizeVRFCondenser')
        tmpCondVolFlowRate = VRF(VRFCond)%HeatingCapacity / &
                                             ( PlantSizData(PltSizCondNum)%DeltaT * Cp * rho )
        IF(VRF(VRFCond)%HeatingCapacity /= Autosize)THEN
          VRF(VRFCond)%WaterCondVolFlowRate = tmpCondVolFlowRate
          CALL ReportSizingOutput('AirConditioner:VariableRefrigerantFlow', &
                                            VRF(VRFCond)%Name, &
                                            'Design Condenser Water Flow Rate [m3/s]', &
                                            VRF(VRFCond)%WaterCondVolFlowRate)
        END IF

        CALL RegisterPlantCompDesignFlow(VRF(VRFCond)%CondenserNodeNum,VRF(VRFCond)%WaterCondVolFlowRate)

      ELSE
        CALL ShowSevereError('Autosizing of condenser water flow rate requires a condenser loop Sizing:Plant object')
        CALL ShowContinueError('... occurs in AirConditioner:VariableRefrigerantFlow object='//TRIM(VRF(VRFCond)%Name))
        CALL ShowContinueError('... plant loop name must be referenced in Sizing:Plant object')
        ErrorsFound = .TRUE.
      END IF

    END IF

    IF (ErrorsFound) THEN
      CALL ShowFatalError('Preceding sizing errors cause program termination')
    END IF

  END IF

  RETURN

END SUBROUTINE SizeVRFCondenser

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
  REAL(r64), PARAMETER :: MinPLF   = 0.0d0          ! minimum part load factor allowed
  REAL(r64), PARAMETER :: ErrorTol = 0.001d0      ! tolerance for RegulaFalsi iterations

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
  REAL(r64)          :: TempMinPLR    ! min PLR used in Regula Falsi call
  REAL(r64)          :: TempMaxPLR    ! max PLR used in Regula Falsi call
  LOGICAL            :: ContinueIter  ! used when convergence is an issue
  INTEGER            :: VRFCond       ! index to VRF condenser
  INTEGER            :: IndexToTUInTUList   ! index to TU in specific list for the VRF system
  INTEGER            :: TUListIndex   ! index to TU list for this VRF system
  LOGICAL            :: VRFCoolingMode
  LOGICAL            :: VRFHeatingMode
  LOGICAL            :: HRCoolingMode
  LOGICAL            :: HRHeatingMode

  PartLoadRatio  = 0.d0
  LoopDXCoolCoilRTF = 0.d0
  LoopDXHeatCoilRTF = 0.d0
  VRFCond = VRFTU(VRFTUNum)%VRFSysNum
  IndexToTUInTUList = VRFTU(VRFTUNum)%IndexToTUInTUList
  TUListIndex = VRF(VRFCond)%ZoneTUListPtr
  VRFCoolingMode = CoolingLoad(VRFCond)
  VRFHeatingMode = HeatingLoad(VRFCond)
  HRCoolingMode  = TerminalUnitList(TUListIndex)%HRCoolRequest(IndexToTUInTUList)
  HRHeatingMode  = TerminalUnitList(TUListIndex)%HRHeatRequest(IndexToTUInTUList)

  ! The RETURNS here will jump back to SimVRF where the CalcVRF routine will simulate with lastest PLR

  ! do nothing else if TU is scheduled off
!!!LKL Discrepancy < 0
  IF (GetCurrentScheduleValue(VRFTU(VRFTUNum)%SchedPtr) .EQ. 0.0d0) RETURN

  ! do nothing if TU has no load (TU will be modeled using PLR=0)
  IF (QZnReq == 0.d0) RETURN

  ! Set EMS value for PLR and return
  IF (VRFTU(VRFTUNum)%EMSOverridePartLoadFrac) THEN
    PartLoadRatio = VRFTU(VRFTUNum)%EMSValueForPartLoadFrac
    RETURN
  ENDIF

  ! Get result when DX coil is off
  PartLoadRatio = 0.0d0
  CALL CalcVRF(VRFTUNum, FirstHVACIteration, 0.0d0, NoCompOutput, OnOffAirFlowRatio)

  IF(VRFCoolingMode .AND. HRHeatingMode)THEN
    ! IF the system is in cooling mode, but the terminal unit requests heating (heat recovery)
    IF(NoCompOutput .GE. QZnReq)RETURN
  ELSE IF(VRFHeatingMode .AND. HRCoolingMode)THEN
    ! IF the system is in heating mode, but the terminal unit requests cooling (heat recovery)
    IF(NoCompOutput .LE. QZnReq)RETURN
  ELSE IF(VRFCoolingMode .OR. HRCoolingMode)THEN
    ! IF the system is in cooling mode and/or the terminal unit requests cooling
    IF(NoCompOutput .LE. QZnReq)RETURN
  ELSE IF(VRFHeatingMode .OR. HRHeatingMode)THEN
    ! IF the system is in heating mode and/or the terminal unit requests heating
    IF(NoCompOutput .GE. QZnReq)RETURN
  END IF

  ! Otherwise the coil needs to turn on. Get full load result
  PartLoadRatio  = 1.0d0
  CALL CalcVRF(VRFTUNum, FirstHVACIteration, PartLoadRatio, FullOutput, OnOffAirFlowRatio)
  PartLoadRatio  = 0.0d0

  IF ((VRFCoolingMode .AND. .NOT. VRF(VRFCond)%HeatRecoveryUsed) .OR. &
      (VRF(VRFCond)%HeatRecoveryUsed .AND. HRCoolingMode)) THEN
    ! Since we are cooling, we expect FullOutput < NoCompOutput
    ! If the QZnReq <= FullOutput the unit needs to run full out
    IF (QZnReq  <=  FullOutput) THEN
      ! if no coil present in terminal unit, no need to reset PLR?
      IF(VRFTU(VRFTUNum)%CoolingCoilPresent)PartLoadRatio = 1.0d0
      RETURN
    END IF
  ELSE IF((VRFHeatingMode .AND. .NOT. VRF(VRFCond)%HeatRecoveryUsed) .OR. &
          (VRF(VRFCond)%HeatRecoveryUsed .AND. HRHeatingMode)) THEN
    ! Since we are heating, we expect FullOutput > NoCompOutput
    ! If the QZnReq >= FullOutput the unit needs to run full out
    IF (QZnReq  >=  FullOutput) THEN
      ! if no coil present in terminal unit, no need reset PLR?
      IF(VRFTU(VRFTUNum)%HeatingCoilPresent)PartLoadRatio = 1.0d0
      RETURN
    END IF
  ELSE
    ! VRF terminal unit is off, PLR already set to 0 above
    ! shouldn't actually get here
    RETURN
  END IF

  ! The coil will not operate at PLR=0 or PLR=1, calculate the operating part-load ratio

  IF ((VRFHeatingMode .OR. HRHeatingMode) .OR. (VRFCoolingMode .OR. HRCoolingMode)) THEN

    Par(1) = VRFTUNum
    Par(2)=0.0d0
    Par(4)=0.0d0
    IF (FirstHVACIteration) THEN
      Par(3) = 1.0d0
    ELSE
      Par(3) = 0.0d0
    END IF
!    Par(4) = OpMode
    Par(5) = QZnReq
    Par(6) = OnOffAirFlowRatio
      CALL SolveRegulaFalsi(ErrorTol, MaxIte, SolFla, PartLoadRatio, PLRResidual,   &
                              0.0d0, 1.0d0, Par)
    IF (SolFla == -1) THEN
!     Very low loads may not converge quickly. Tighten PLR boundary and try again.
      TempMaxPLR = -0.1d0
      ContinueIter = .TRUE.
      DO WHILE(ContinueIter .AND. TempMaxPLR .LT. 1.0d0)
        TempMaxPLR = TempMaxPLR + 0.1d0
        CALL CalcVRF(VRFTUNum,FirstHVACIteration,TempMaxPLR,TempOutput,OnOffAirFlowRatio)
        IF(VRFHeatingMode .AND. TempOutput .GT. QZnReq)ContinueIter = .FALSE.
        IF(VRFCoolingMode .AND. TempOutput .LT. QZnReq)ContinueIter = .FALSE.
      END DO
      TempMinPLR = TempMaxPLR
      ContinueIter = .TRUE.
      DO WHILE(ContinueIter .AND. TempMinPLR .GT. 0.0d0)
        TempMaxPLR = TempMinPLR
        TempMinPLR = TempMinPLR - 0.01d0
        CALL CalcVRF(VRFTUNum,FirstHVACIteration,TempMinPLR,TempOutput,OnOffAirFlowRatio)
        IF(VRFHeatingMode .AND. TempOutput .LT. QZnReq)ContinueIter = .FALSE.
        IF(VRFCoolingMode .AND. TempOutput .GT. QZnReq)ContinueIter = .FALSE.
      END DO
        CALL SolveRegulaFalsi(ErrorTol, MaxIte, SolFla, PartLoadRatio, PLRResidual,   &
                              TempMinPLR, TempMaxPLR, Par)
      IF (SolFla == -1) THEN
        IF (.NOT. FirstHVACIteration .AND. .NOT. WarmupFlag) THEN
          IF(VRFTU(VRFTUNum)%IterLimitExceeded == 0)THEN
            WRITE(IterNum,*) MaxIte
            IterNum=ADJUSTL(IterNum)
            Call ShowWarningMessage(TRIM(cVRFTUTypes(VRFTU(VRFTUNum)%VRFTUType_Num))//' "'//TRIM(VRFTU(VRFTUNum)%Name)//'"')
            CALL ShowContinueError(' Iteration limit exceeded calculating terminal unit part-load ratio, '// &
                                'maximum iterations = '//TRIM(IterNum))
            CALL ShowContinueErrorTimeStamp(' Part-load ratio returned = '//TRIM(RoundSigDigits(PartLoadRatio,3)))
            CALL CalcVRF(VRFTUNum,FirstHVACIteration,TempMinPLR,TempOutput,OnOffAirFlowRatio)
            CALL ShowContinueError(' Load requested = '//TRIM(TrimSigDigits(QZnReq,5))//', Load delivered = ' &
                                 //TRIM(TrimSigDigits(TempOutput,5)))
            CALL ShowRecurringWarningErrorAtEnd(TRIM(cVRFTUTypes(VRFTU(VRFTUNum)%VRFTUType_Num))//' "'//  &
                 TRIM(VRFTU(VRFTUNum)%Name)//'" -- Terminal unit Iteration limit exceeded error continues...',  &
                 VRFTU(VRFTUNum)%IterLimitExceeded)
          ELSE
            CALL ShowRecurringWarningErrorAtEnd(TRIM(cVRFTUTypes(VRFTU(VRFTUNum)%VRFTUType_Num))//' "'//  &
                 TRIM(VRFTU(VRFTUNum)%Name)//'" -- Terminal unit Iteration limit exceeded error continues...',  &
                 VRFTU(VRFTUNum)%IterLimitExceeded)
          END IF
        END IF
      ELSE IF (SolFla == -2) THEN
        IF (.NOT. FirstHVACIteration .AND. .NOT. WarmupFlag) THEN
          IF(VRFTU(VRFTUNum)%FirstIterfailed == 0)THEN
            Call ShowWarningMessage(TRIM(cVRFTUTypes(VRFTU(VRFTUNum)%VRFTUType_Num))//' "'//TRIM(VRFTU(VRFTUNum)%Name)//'"')
            CALL ShowContinueError('Terminal unit part-load ratio calculation failed: ' &
                           //'PLR limits of 0 to 1 exceeded')
            CALL ShowContinueError('Please fill out a bug report and forward to the EnergyPlus support group.')
            CALL ShowContinueErrorTimeStamp(' ')
            IF (WarmupFlag) CALL ShowContinueError ('Error occurred during warmup days.')
            CALL ShowRecurringWarningErrorAtEnd(TRIM(cVRFTUTypes(VRFTU(VRFTUNum)%VRFTUType_Num))//' "'//  &
                 TRIM(VRFTU(VRFTUNum)%Name)//'" -- Terminal unit part-load ratio limits of 0 to 1 exceeded error continues...',  &
                 VRFTU(VRFTUNum)%FirstIterfailed)
          ELSE
            CALL ShowRecurringWarningErrorAtEnd(TRIM(cVRFTUTypes(VRFTU(VRFTUNum)%VRFTUType_Num))//' "'//  &
                 TRIM(VRFTU(VRFTUNum)%Name)//'" -- Terminal unit part-load ratio limits of 0 to 1 exceeded error continues...',  &
                 VRFTU(VRFTUNum)%FirstIterfailed)
          END IF
        END IF
        PartLoadRatio = MAX(MinPLF, ABS(QZnReq - NoCompOutput) / ABS(FullOutput - NoCompOutput))
      END IF
    ELSE IF (SolFla == -2) THEN
      IF (.NOT. FirstHVACIteration .AND. .NOT. WarmupFlag) THEN
        IF(VRFTU(VRFTUNum)%FirstIterfailed == 0)THEN
          Call ShowWarningMessage(TRIM(cVRFTUTypes(VRFTU(VRFTUNum)%VRFTUType_Num))//' "'//TRIM(VRFTU(VRFTUNum)%Name)//'"')
          CALL ShowContinueError('Terminal unit part-load ratio calculation failed: ' &
                           //'PLR limits of 0 to 1 exceeded')
          CALL ShowContinueError('Please fill out a bug report and forward to the EnergyPlus support group.')
          CALL ShowContinueErrorTimeStamp(' ')
          IF (WarmupFlag) CALL ShowContinueError ('Error occurred during warmup days.')
          CALL ShowRecurringWarningErrorAtEnd(TRIM(cVRFTUTypes(VRFTU(VRFTUNum)%VRFTUType_Num))//' "'//  &
                 TRIM(VRFTU(VRFTUNum)%Name)//'" -- Terminal unit part-load ratio limits of 0 to 1 exceeded error continues...',  &
                 VRFTU(VRFTUNum)%FirstIterfailed)
        ELSE
          CALL ShowRecurringWarningErrorAtEnd(TRIM(cVRFTUTypes(VRFTU(VRFTUNum)%VRFTUType_Num))//' "'//  &
                 TRIM(VRFTU(VRFTUNum)%Name)//'" -- Terminal unit part-load ratio limits of 0 to 1 exceeded error continues...',  &
                 VRFTU(VRFTUNum)%FirstIterfailed)
        END IF
      END IF
      IF(FullOutput - NoCompOutput .EQ. 0.d0)THEN
        PartLoadRatio = 0.d0
      ELSE
        PartLoadRatio = MIN(1.d0,MAX(MinPLF, ABS(QZnReq - NoCompOutput) / ABS(FullOutput - NoCompOutput)))
      END IF
    END IF

  END IF

  RETURN
END SUBROUTINE ControlVRF

SUBROUTINE CalcVRF(VRFTUNum, FirstHVACIteration, PartLoadRatio, LoadMet, OnOffAirFlowRatio, LatOutputProvided)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Raustad
          !       DATE WRITTEN   July 2005
          !       MODIFIED       July 2012, Chandan Sharma - FSEC: Added zone sys avail managers
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
  INTEGER  :: OpMode             ! fan operating mode, CycFanCycCoil or ContFanCycCoil
  INTEGER  :: VRFCond            ! index to VRF condenser
  REAL(r64):: SpecHumOut         ! specific humidity ratio at outlet node
  REAL(r64):: SpecHumIn          ! specific humidity ratio at inlet node
  INTEGER  :: TUListIndex        ! index to TU list for this VRF system
  INTEGER  :: IndexToTUInTUList        ! index to TU in specific list for the VRF system

          ! FLOW

  VRFCond = VRFTU(VRFTUNum)%VRFSysNum
  TUListIndex = VRF(VRFCond)%ZoneTUListPtr
  IndexToTUInTUList = VRFTU(VRFTUNum)%IndexToTUInTUList
  VRFTUOutletNodeNum = VRFTU(VRFTUNum)%VRFTUOutletNodeNum
  VRFTUInletNodeNum = VRFTU(VRFTUNum)%VRFTUInletNodeNum
  OpMode = VRFTU(VRFTUNum)%OpMode

  ! Set inlet air mass flow rate based on PLR and compressor on/off air flow rates
  CALL SetAverageAirFlow(VRFTUNum, PartLoadRatio, OnOffAirFlowRatio)

  AirMassFlow = Node(VRFTUInletNodeNum)%MassFlowRate
  IF(VRFTU(VRFTUNum)%OAMixerUsed)CALL SimOAMixer(VRFTU(VRFTUNum)%OAMixerName,FirstHVACIteration,VRFTU(VRFTUNum)%OAMixerIndex)

  ! if blow through, simulate fan then coils
  IF (VRFTU(VRFTUNum)%FanPlace .EQ. BlowThru) THEN
    CALL SimulateFanComponents(' ',FirstHVACIteration,VRFTU(VRFTUNum)%FanIndex,FanSpeedRatio, &
                                  ZoneCompTurnFansOn, ZoneCompTurnFansOff)
  END IF

  IF(VRFTU(VRFTUNum)%CoolingCoilPresent)THEN
    IF ((.NOT. VRF(VRFCond)%HeatRecoveryUsed .AND. CoolingLoad(VRFCond)) .OR. &
        ! above condition for heat pump mode, below condition for heat recovery mode
        (VRF(VRFCond)%HeatRecoveryUsed .AND. TerminalUnitList(TUListIndex)%HRCoolRequest(IndexToTUInTUList)))THEN
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
    IF ((.NOT. VRF(VRFCond)%HeatRecoveryUsed .AND. HeatingLoad(VRFCond)) .OR. &
        ! above condition for heat pump mode, below condition for heat recovery mode
        (VRF(VRFCond)%HeatRecoveryUsed .AND. TerminalUnitList(TUListIndex)%HRHeatRequest(IndexToTUInTUList)))THEN
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
    CALL SimulateFanComponents(' ',FirstHVACIteration,VRFTU(VRFTUNum)%FanIndex,FanSpeedRatio, &
                                  ZoneCompTurnFansOn, ZoneCompTurnFansOff)
  END IF

  ! track fan power per terminal unit for calculating COP
  VRFTU(VRFTUNum)%FanPower = FanElecPower

! calculate sensible load met using delta enthalpy at a constant (minimum) humidity ratio
  MinHumRat = MIN(Node(VRFTUInletNodeNum)%HumRat,Node(VRFTUOutletNodeNum)%HumRat)
  LoadMet   = AirMassFlow * (PsyHFnTdbW(Node(VRFTUOutletNodeNum)%Temp,MinHumRat) - &
                             PsyHFnTdbW(Node(VRFTUInletNodeNum)%Temp,MinHumRat))

  IF(PRESENT(LatOutputProvided))THEN
!   CR9155 Remove specific humidity calculations
    SpecHumOut = Node(VRFTUOutletNodeNum)%HumRat
    SpecHumIn  = Node(VRFTUInletNodeNum)%HumRat
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
  REAL(r64) :: H2OHtOfVap           !- Heat of vaporization of air (J/kg)
  INTEGER   :: TUListIndex          !- index to terminal unit list
  INTEGER   :: IndexToTUInTUList    !- index to the TU in the list
  LOGICAL   :: HRHeatRequestFlag    !- indicates TU could be in heat mode
  LOGICAL   :: HRCoolRequestFlag    !- indicates TU could be in cool mode

  DXCoolingCoilIndex = VRFTU(VRFTUNum)%CoolCoilIndex
  DXHeatingCoilIndex = VRFTU(VRFTUNum)%HeatCoilIndex
  VRFCond = VRFTU(VRFTUNum)%VRFSysNum
  TUListIndex = VRF(VRFCond)%ZoneTUListPtr
  IndexToTUInTUList = VRFTU(VRFTUNum)%IndexToTUInTUList
  HRHeatRequestFlag = TerminalUnitList(TUListIndex)%HRHeatRequest(IndexToTUInTUList)
  HRCoolRequestFlag = TerminalUnitList(TUListIndex)%HRCoolRequest(IndexToTUInTUList)
  ReportingConstant = TimeStepSys*SecInHour

  ! account for terminal unit parasitic On/Off power use
  ! account for heat recovery first since these flags will be FALSE otherwise, each TU may have different operating mode

  IF(HRCoolRequestFlag)THEN
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
  ELSE IF(HRHeatRequestFlag)THEN
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
  ELSE IF(CoolingLoad(VRFCond) .OR. (.NOT. HeatingLoad(VRFCond) .AND. LastModeCooling(VRFTU(VRFTUNum)%VRFSysNum)))THEN
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
  ! convert latent in kg/s to watts
  H2OHtOfVap = PsyHfgAirFnWTdb(Node(VRFTU(VRFTUNum)%VRFTUOutletNodeNum)%HumRat,&
               Node(VRFTU(VRFTUNum)%VRFTUOutletNodeNum)%Temp,'ReportVRFTerminalUnit')
  TotalConditioning    = SensibleConditioning + (LatentConditioning*H2OHtOfVap)

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
    VRFTU(VRFTUNum)%LatentCoolingRate = ABS(LatentConditioning) * H2OHtOfVap
    VRFTU(VRFTUNum)%LatentHeatingRate = 0.d0
  ELSE
    VRFTU(VRFTUNum)%LatentCoolingRate = 0.d0
    VRFTU(VRFTUNum)%LatentHeatingRate = LatentConditioning * H2OHtOfVap
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

    VRF(VRFCond)%QCondEnergy = VRF(VRFCond)%QCondenser * ReportingConstant

  RETURN
END SUBROUTINE ReportVRFCondenser

SUBROUTINE UpdateVRFCondenser(VRFCond)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Raustad, FSEC
          !       DATE WRITTEN   May 2012
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine updates the node data for the VRF Condenser.

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
!    INTEGER :: CondenserInletNode  !- inlet node for VRF water-cooled condenser
    INTEGER :: CondenserOutletNode !- outlet node for VRF water-cooled condenser

!    CondenserInletNode = VRF(VRFCond)%CondenserNodeNum
    CondenserOutletNode = VRF(VRFCond)%CondenserOutletNodeNum

    Node(CondenserOutletNode)%Temp = VRF(VRFCond)%CondenserSideOutletTemp

!    Node(CondenserInletNode)%MassFlowRate = CondenserWaterMassFlowRate
    Node(CondenserOutletNode)%MassFlowRate = CondenserWaterMassFlowRate

    Node(CondenserOutletNode)%MassFlowRateMaxAvail = Node(CondenserOutletNode)%MassFlowRateMaxAvail
    Node(CondenserOutletNode)%MassFlowRateMinAvail = Node(CondenserOutletNode)%MassFlowRateMinAvail

  RETURN
END SUBROUTINE UpdateVRFCondenser

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
  REAL(r64) :: QZnReqTemp          ! denominator representing zone load (W)
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
  QZnReqTemp = QZnReq
  IF(ABS(QZnReq) .LT. 100.d0)QZnReqTemp=SIGN(100.d0,QZnReq)
  OnOffAirFlowRatio = Par(6)

  CALL CalcVRF(VRFTUNum,FirstHVACIteration,PartLoadRatio,ActualOutput,OnOffAirFlowRatio)
  PLRResidual = (ActualOutput - QZnReq)/QZnReqTemp

  RETURN
END FUNCTION PLRResidual

SUBROUTINE SetAverageAirFlow(VRFTUNum,PartLoadRatio,OnOffAirFlowRatio)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Raustad
          !       DATE WRITTEN   August 2010
          !       MODIFIED       July 2012, Chandan Sharma - FSEC: Added zone sys avail managers
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
  USE DataZoneEquipment, ONLY: VRFTerminalUnit_Num

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
       (GetCurrentScheduleValue(VRFTU(VRFTUNum)%FanAvailSchedPtr) .GT. 0.d0 .OR. &
       ZoneCompTurnFansOn) .AND. .NOT. ZoneCompTurnFansOff)THEN

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

SUBROUTINE InitializeOperatingMode(FirstHVACIteration,VRFCond,TUListNum,OnOffAirFlowRatio)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Raustad
          !       DATE WRITTEN   July 2012 (Moved from InitVRF)
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Scans each zone coil and determines the load based on control
          ! Moved from Init to clean up and localize code segments

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataHeatBalFanSys, ONLY: TempControlType, ZT, ZoneThermostatSetPointHi, ZoneThermostatSetPointLo
  USE ScheduleManager,   ONLY: GetCurrentScheduleValue
  USE MixedAir,          ONLY: SimOAMixer, SimOAController

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  LOGICAL,   INTENT (IN)    :: FirstHVACIteration ! flag for first time through HVAC systems
  INTEGER,   INTENT (IN)    :: VRFCond            ! Condenser Unit index
  INTEGER,   INTENT (IN)    :: TUListNum          ! Condenser Unit terminal unit list
  REAL(r64), INTENT (INOUT) :: OnOffAirFlowRatio  ! ratio of on to off flow rate
          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64) :: ZoneDeltaT       ! zone temperature difference from setpoint
  REAL(r64) :: SPTempHi         ! thermostat setpoint high
  REAL(r64) :: SPTempLo         ! thermostat setpoint low
  INTEGER   :: NumTU            ! loop counter, number of TU's in list
  INTEGER   :: TUIndex          ! index to TU
  INTEGER   :: ThisZoneNum      ! index to zone number where TU is located
  REAL(r64) :: ZoneLoad         ! current zone load (W)
  REAL(r64) :: LoadToCoolingSP  ! thermostat load to cooling setpoint (W)
  REAL(r64) :: LoadToHeatingSP  ! thermostat load to heating setpoint (W)
  REAL(r64) :: TempOutput       ! terminal unit output [W]
  INTEGER   :: FanOpMode        ! TU fan operating mode

    MaxDeltaT  = 0.d0
    MinDeltaT  = 0.d0
    NumCoolingLoads = 0
    SumCoolingLoads = 0.d0
    NumHeatingLoads = 0
    SumHeatingLoads = 0.d0


      NumCoolingLoads(VRFCond) = 0
      NumHeatingLoads(VRFCond) = 0
      SumCoolingLoads(VRFCond) = 0.0d0
      SumHeatingLoads(VRFCond) = 0.0d0
      MaxDeltaT(VRFCond)     = 0.0d0
      MinDeltaT(VRFCond)     = 0.0d0
      ZoneDeltaT             = 0.0d0
      HeatingLoad(VRFCond) = .FALSE.
      CoolingLoad(VRFCond) = .FALSE.
      TerminalUnitList(TUListNum)%CoolingCoilAvailable = .FALSE.
      TerminalUnitList(TUListNum)%HeatingCoilAvailable = .FALSE.
      ! loop through all TU's to find operating mode. Be carefull not to mix loop counters with current TU/Cond index
      DO NumTU = 1, TerminalUnitList(TUListNum)%NumTUInList
        ! make sure TU's have been sized before looping through each one of them to determine operating mode
        ! (which would size all coils based on the zone that called this specific VRF terminal unit)
        IF(ANY(TerminalUnitList(TUListNum)%TerminalUnitNotSizedYet))EXIT
        TUIndex     = TerminalUnitList(TUListNum)%ZoneTUPtr(NumTU)
        ThisZoneNum = VRFTU(TUIndex)%ZoneNum

!       check to see if coil is present
        IF(TerminalUnitList(VRFCond)%CoolingCoilPresent(NumTU))THEN
!         now check to see if coil is scheduled off
          IF(GetCurrentScheduleValue(TerminalUnitList(TUListNum)%CoolingCoilAvailSchPtr(NumTU)) .GT. 0.d0)THEN
            TerminalUnitList(TUListNum)%CoolingCoilAvailable(NumTU) = .TRUE.
          END IF
        END IF

!       check to see if coil is present
        IF(TerminalUnitList(VRFCond)%HeatingCoilPresent(NumTU))THEN
!         now check to see if coil is scheduled off
          IF(GetCurrentScheduleValue(TerminalUnitList(TUListNum)%HeatingCoilAvailSchPtr(NumTU)) .GT. 0.d0)THEN
            TerminalUnitList(TUListNum)%HeatingCoilAvailable(NumTU) = .TRUE.
          END IF
        END IF

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
                    ! if last mode was cooling, make sure heating flow rate is used
                    IF(VRFTU(TUIndex)%OAMixerUsed)THEN
                      Node(VRFTU(TUIndex)%VRFTUOAMixerRetNodeNum)%MassFlowRate = VRFTU(TUIndex)%MaxHeatAirMassFlow
                      Node(VRFTU(TUIndex)%VRFTUOAMixerOANodeNum)%MassFlowRate = VRFTU(TUIndex)%HeatOutAirMassFlow
                      CALL SimOAMixer(VRFTU(TUIndex)%OAMixerName,FirstHVACIteration,VRFTU(TUIndex)%OAMixerIndex)
                    ELSE
                      Node(VRFTU(TUIndex)%VRFTUInletNodeNum)%MassFlowRate = VRFTU(TUIndex)%MaxHeatAirMassFlow
                    END IF
                    ! recalculate using correct flow rate
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
          END IF ! IF(VRFTU(TUIndex)%OpMode == ContFanCycCoil)THEN
        END IF
      END DO

! Determine operating mode based on VRF type and thermostat control selection
      SELECT CASE(VRF(VRFCond)%ThermostatPriority)
        CASE(ThermostatOffsetPriority)
          TUIndex = VRF(VRFCond)%MasterZoneTUIndex
          IF (VRFTU(TUIndex)%FanOpModeSchedPtr .GT. 0) THEN
            IF (GetCurrentScheduleValue(VRFTU(TUIndex)%FanOpModeSchedPtr) .EQ. 0.0d0) THEN
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
          IF(VRFTU(VRF(VRFCond)%MasterZoneTUIndex)%OpMode == ContFanCycCoil)THEN
            CALL SetCompFlowRate(VRF(VRFCond)%MasterZoneTUIndex, VRFCond)
            CALL CalcVRF(VRF(VRFCond)%MasterZoneTUIndex,FirstHVACIteration,0.0d0,TempOutput,OnOffAirFlowRatio)
            LoadToCoolingSP = ZoneSysEnergyDemand(VRF(VRFCond)%MasterZonePtr)%OutputRequiredToCoolingSP
            LoadToHeatingSP = ZoneSysEnergyDemand(VRF(VRFCond)%MasterZonePtr)%OutputRequiredToHeatingSP
            IF(TempOutput .LT. LoadToHeatingSP)THEN
              CoolingLoad(VRFCond) = .FALSE.
              HeatingLoad(VRFCond) = .TRUE.
            ELSE IF(TempOutput .GT. LoadToCoolingSP)THEN
              CoolingLoad(VRFCond) = .TRUE.
              HeatingLoad(VRFCond) = .FALSE.
            ELSE
              CoolingLoad(VRFCond) = .FALSE.
              HeatingLoad(VRFCond) = .FALSE.
            END IF
          ELSE IF(ZoneLoad .GT. 0.0D0)THEN
            HeatingLoad(VRFCond) = .TRUE.
            CoolingLoad(VRFCond) = .FALSE.
          ELSE IF(ZoneLoad .LT. 0.0D0)THEN
            HeatingLoad(VRFCond) = .FALSE.
            CoolingLoad(VRFCond) = .TRUE.
          ELSE
            HeatingLoad(VRFCond) = .FALSE.
            CoolingLoad(VRFCond) = .FALSE.
          END IF
        CASE(FirstOnPriority)
        ! na
        CASE DEFAULT
      END SELECT

    ! limit to one possible mode
    IF(CoolingLoad(VRFCond) .AND. HeatingLoad(VRFCond))HeatingLoad(VRFCond)=.FALSE.

  RETURN
END SUBROUTINE InitializeOperatingMode

SUBROUTINE LimitTUCapacity(VRFCond,NumTUInList,StartingCapacity,CapArray,MaxLimit,AltCapacity,AltArray,AltLimit)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Raustad
          !       DATE WRITTEN   July 2012 (Moved from InitVRF)
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Calculate the maximum allowed terminal unit capacity. Total terminal unit capacity must not
          ! exceed the available condenser capacity. This variable, MaxCapacity (passed out to MaxCoolingCapacity
          ! or MaxHeatingCapacity), is used to limit the terminal units providing more capacity than allowed.
          ! Example: TU loads are 1-ton, 2-ton, 3-ton, and 4-ton connected to a condenser having only 9-tons available.
          ! This variable is will be set to 3-tons and the 4-ton terminal unit will be limited to 3-tons
          ! (see InitVRF where this variable is reset and CalcVRF where the call to the DX coils passes this argument).

          ! METHODOLOGY EMPLOYED:
          ! The coils are simulated and summed. This value is compared to the available capacity. If the summed
          ! TU capacity is greater than the available capacity, limit the TU's with the highest capacity so that
          ! the TU capacity equals the available capacity. The report variable Variable Refrigerant Flow Heat Pump
          ! Maximum Terminal Unit Cool/Heating Capacity holds the value for maximum TU capacity. This value may not
          ! match the maximum individual coil capacity exactly since the available capaity uses a load weighted
          ! average WB temperature to calculate available capacity. When the TU's are limited, this weighting changes.
          ! The extra iterations required for these values to converge is considered excessive.
          ! If the globabl flag SimZoneEquipment could be set for 1 additional iteration, these variables would
          ! converge more closely (setting this globabl flag is not yet implemented).

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER,   INTENT (IN)    :: VRFCond            ! Condenser Unit index
  INTEGER,   INTENT (IN)    :: NumTUInList        ! Number of terminal units in list
  REAL(r64), INTENT (IN)    :: StartingCapacity   ! temporary variable holding condenser capacity [W]
  REAL(r64),DIMENSION(:), INTENT (IN) :: CapArray ! Array of coil capacities in either cooling or heating mode [W]
  REAL(r64), INTENT (INOUT) :: MaxLimit           ! Maximum terminal unit capacity for coils in same operating mode [W]
  ! these variables hold information on coil in opposite operating mode (i.e., heat recovery)
  REAL(r64), INTENT (IN)    :: AltCapacity        ! temporary variable holding heat recovery capacity [W]
  REAL(r64),DIMENSION(:), INTENT (IN) :: AltArray ! Array of coil capacities of heat recovery [W]
  REAL(r64), INTENT (INOUT) :: AltLimit           ! Maximum terminal unit capacity of heat recovery coils [W]
          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64) :: RemainingCapacity     ! decrement capacity counter to find limiting TU capacity [W]

! limit TU coil capacity to be equal to the condenser capacity (piping losses already accounted for)
  CALL LimitCoilCapacity(NumTUInList,StartingCapacity,CapArray,MaxLimit)

! ** add in logic to limit coils operating opposite to mode when heat recovery is used
! ** this is a hard one since we are here because the system is overloaded. That means
! ** that we do not know at this point the actual operating capacity or compressor power.
  IF(VRF(VRFCond)%HeatRecoveryUsed)THEN
    IF(CoolingLoad(VRFCond))THEN
      RemainingCapacity = StartingCapacity*(1+1/VRF(VRFCond)%CoolingCOP)
      IF(AltCapacity .GT. RemainingCapacity)THEN
        CALL LimitCoilCapacity(NumTUInList,RemainingCapacity,AltArray,AltLimit)
      END IF
    END IF
    IF(HeatingLoad(VRFCond))THEN
      RemainingCapacity = StartingCapacity/(1+1/VRF(VRFCond)%HeatingCOP)
      IF(AltCapacity .GT. RemainingCapacity)THEN
        CALL LimitCoilCapacity(NumTUInList,RemainingCapacity,AltArray,AltLimit)
      END IF
    END IF
  END IF

  RETURN
END SUBROUTINE LimitTUCapacity

SUBROUTINE LimitCoilCapacity(NumTUInList,TotalCapacity,CapArray,MaxLimit)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Raustad
          !       DATE WRITTEN   July 2012 (Moved from InitVRF)
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Calculate the maximum allowed terminal unit capacity. Total terminal unit capacity must not
          ! exceed the available condenser capacity. This variable, MaxCapacity (passed out to MaxCoolingCapacity
          ! or MaxHeatingCapacity), is used to limit the terminal units providing more capacity than allowed.
          ! Example: TU loads are 1-ton, 2-ton, 3-ton, and 4-ton connected to a condenser having only 9-tons available.
          ! This variable is will be set to 3-tons and the 4-ton terminal unit will be limited to 3-tons
          ! (see InitVRF where this variable is reset and CalcVRF where the call to the DX coils passes this argument).

          ! METHODOLOGY EMPLOYED:
          ! The coils are simulated and summed. This value is compared to the available capacity. If the summed
          ! TU capacity is greater than the available capacity, limit the TU's with the highest capacity so that
          ! the TU capacity equals the available capacity. The report variable Variable Refrigerant Flow Heat Pump
          ! Maximum Terminal Unit Cool/Heating Capacity holds the value for maximum TU capacity. This value may not
          ! match the maximum individual coil capacity exactly since the available capaity uses a load weighted
          ! average WB temperature to calculate available capacity. When the TU's are limited, this weighting changes.
          ! The extra iterations required for these values to converge is considered excessive.
          ! If the globabl flag SimZoneEquipment could be set for 1 additional iteration, these variables would
          ! converge more closely (setting this globabl flag is not yet implemented).

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER,   INTENT (IN)    :: NumTUInList        ! Number of terminal units in list
  REAL(r64), INTENT (IN)    :: TotalCapacity      ! temporary variable holding condenser capacity [W]
  REAL(r64),DIMENSION(:), INTENT (IN) :: CapArray ! Array of coil capacities in either cooling or heating mode [W]
  REAL(r64), INTENT (INOUT) :: MaxLimit           ! Maximum terminal unit capacity for coils in same operating mode [W]
          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: NumTU                   ! loop counter
  INTEGER :: TempTUIndex             ! temp variable used to find max terminal unit limit
  INTEGER :: MinOutputIndex          ! index to TU with lowest load
  REAL(r64) :: MinOutput             ! used when finding TU "max" capacity limit
  REAL(r64) :: RemainingCapacity     ! decrement capacity counter to find limiting TU capacity [W]
  REAL(r64), ALLOCATABLE :: Temp(:)  ! temporarary array for processing terminal units
  REAL(r64), ALLOCATABLE :: Temp2(:) ! temporarary array for processing terminal units

  ALLOCATE(Temp(NumTUInList))
  ALLOCATE(Temp2(NumTUInList))
  Temp = CapArray
  Temp2 = Temp
  RemainingCapacity = TotalCapacity

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
  ! if the terminal unit capacity multiplied by number of remaining TU's does not exceed remaining available, subtract and cycle
  DO TempTUIndex = 1, NumTUInList
    IF((Temp(TempTUIndex)*(NumTUInList-TempTUIndex+1)) .LT. RemainingCapacity)THEN
      RemainingCapacity = RemainingCapacity - Temp(TempTUIndex)
      CYCLE
    ELSE
      ! if it does exceed, limit is found
      MaxLimit = RemainingCapacity / (NumTUInList-TempTUIndex+1)
      EXIT
    END IF
  END DO

  DEALLOCATE(Temp)
  DEALLOCATE(Temp2)

  RETURN
END SUBROUTINE LimitCoilCapacity

! End of Utility subroutines for the Module
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

End Module HVACVariableRefrigerantFlow


